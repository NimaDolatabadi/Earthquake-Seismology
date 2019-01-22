c
c  hyp_isc
c
c  Copy of hyp.for that calls iscloc_wrap instead of hypocent
c  A lot of functionality stripped out if it seemed unapplicable.
c
c  iscloc only locates using Jeffreys-Bullen so no model choice.
c
c   updates: 27-10-04 jh : add old location
c            10 01 13 jh : comment out seidim, in hypparm
c
      implicit none
      include 'hypparm.inc'
      include 'seisan.inc'
c     include 'seidim.inc'                 ! array dimentions

      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei open,                   ! File open handler.
     &         sei close,                  ! & closure.
     &         sei clen,                   ! String length.
     &         sei code                    ! Error condition handler.
      integer  sei clen                    ! & function.
      integer  code                        ! Condition.
      integer  text_c                      ! Text length.

      integer  iuin                        ! event file
      integer  iustat                      ! STATION0.HYP
      integer  iulst                       ! print.out
      integer  iuphs                       ! hyp.out

      parameter (text_c = 80)              ! & value.
      logical  b_flag                      ! Flag!!
      character chr_text   *(text_c)       ! Text string.

c---name of top directory
      character*60 top_directory
c---original header buffer
      character*80 datsave,data_old ! could they be the same ??
c---flag first says whether to locate and then whether successfuly located
      logical locate
c--- event type, indcator of explosion, indicator of fixing depth  
      character*1 typ,exp
c---indicator if output on screen  
      character*1 yter,yter1
      logical output
c---agency for magnitude  
      character*3 agency
c---directory separator character
      character*1 dchar
c---number of header lines, records, stations in nordic file, help variables
      integer nhead,nrecord,nstat,nfind,k,nphase,i
c---id line indicator
      integer id
C-- hypocenter help variable
      integer init
c-- help varibale
      character*80 text
c-- line counter and max number of line pr page
      integer nline,maxline
c-- save year, month ,day for old location
      integer year,month,day
c-- hour and difference in hours between solutions shifted one day
      integer hour,delhour
c-- abs times for above
      double precision abstim1,abstim2
c-- # phases      
      integer nphs
c-- indicator if program is used in connection with EEV in seisan
      logical use_eev
c-- indicator if seisan data base present
      logical seisan
c--  next for data base operation
      character*10 keys                      ! next choice key
      character*14 starttime,endtime         ! time interval for location
      character*80 basename                  ! data base or file name
      character*80 infile                    ! input file or base name
      character*80 eventfile                 ! single event file name
      integer status,eventno,newmonth,fstart ! 
      integer base                           ! 0: data base, 1: single file

c---minimum number of stations to locate
      integer minstat
c---minimum number of phases to locate  
      integer minphase
c---magnitude Ml coefficients
c      real a,b,c,d  
c---number of arguments and function
       integer nars
       character*80 args(10)
c---used in setting max number of lines per page for screen
      integer lunit
      logical terminal,isatty
      character*10 extension

      integer isindex

      call get_arguments(nars,args)

      call get_env_seisan_extension(extension)

C
C    initialise...
C    =============

      code   = e_ok$                            ! Local condition.
      b_f_debug$ = .false.                      ! Debug to screen/file?
      iuin   = 0                                ! file units.
      iustat = 0                                ! station/model file.
      iulst  = 0                                ! print.out.
      iuphs  = 0                                ! hyp.out.

c                                                           
c   get computer specifics
c
      call dir_char(dchar)         ! dirctory delimiter character

c
c   get seisan defaults
c
      call get_seisan_def

c set max number of lines pr page for screen print out
c modified to allow output to a pipe without expecting
c a reply from the terminal (bjb 2001/02/14)
c Function ISATTY may only be available on Solaris???
      lunit=6
      terminal=isatty(lunit)
      if(terminal) then
         maxline=20
      else
         maxline=1000
      endif

c 
c   get directory structure
c
      call topdir(top_directory)
c
c   check if SEISAN data base is present indicated by the presense of
c   the file station0.hyp in DAT directory
c
      chr_text = top_directory(:seiclen(top_directory)) //
     &           dchar // 'DAT' //dchar                 //
     &           'STATION0.HYP'
c
      call sei open( check$,            ! Check existance.
     &               ' ',               ! No prompt.
     &               chr_text, iuin,   ! File & unnit.
     &               seisan,            ! File exists?.
     &               code )             ! Local condition (n/a).
c
      if( seisan ) then                 ! Multiple files.
      base = 0                          ! General database.
      else                              ! Otherwise only a single file.
      base = 1                          ! Only a single file.
      end if                            !
c
c   check if input of a file as argument
c
      call get_env_event(eventfile)
      if(nars.gt.0) then
         eventfile=args(1)(1:80)
         maxline=1000
      endif
c
c  check if non interactive, then no output
c
      call get_seisan_message(text)

c   find if program will run from within EEV indicated by the presenses of file
c   name in an enviromental variable

      use_eev =  eventfile(1:1)        .ne. ' '    .and.
     &           ichar(eventfile(1:1)) .ne. 0  

C   open data base input single event file and read it
c   set flag for single event nrecord= -1

      if (use_eev) then
         nrecord = -1
         call sei open( old$,            ! Open old file (stop on error).
     &                  ' ',             ! No prompt.
     &                  eventfile, iuin,! File & unit.
     &                  b_flag,          ! File exists? (n/a).
     &                  code )           ! Local condition (n/a).
         call indata(iuin,nstat,nphase,nhead,nrecord,
     &               typ,exp,data,id)
         datsave=data(1)
         call sei close( close$, iuin, code ) ! Close file (stop on error).

         yter = 'Y'                            ! output on screen
         output=.true.

         if (text.eq.'hyp non interactive') then
            yter='N' 
            output = .false.
         endif
         goto 20                               ! jump remaining questions
      endif

c
c   input data base name and time interval
c
      if(seisan) then
         write(*,*) ' Arrival time data input - select one:'
         write(*,*)
         WRITE(*,*) 
     *  '    SEISAN data base or                             :',
     &  'RETURN '
         write(*,*) 
     *  '    Alternative data base, give 1-5 letter code or  :'  
         WRITE(*,*) 
     *  '    Local index file, name must start with index or :'
         WRITE(*,*) 
     *  '    Local data base, write ,, or                    :'
         WRITE(*,*) 
     *  '    Filename for one file, min. 6 chars or with a . : '
      else
         WRITE(*,*) 'Filename for file in NORDIC format     : '
      endif
      write(*,*)

      read(*,'(a)') infile
      basename=infile(1:80)
                       
      write(*,*)
C
c   check if this is a single multiple event file (base=1),
c   general data base (base=0) or local data base, only check if seisan
c   data base is present
C
      if (seisan) then
         starttime=' '
         endtime=' '
         if(basename(1:5).eq.'INDEX'.or.basename(1:5).eq.'index'.
     *   or.basename(1:2).eq.',,') then
            base=0
            goto 2929
         endif
         if(seiclen(basename).lt.6.and.index(basename,'.').eq.0) 
     *   then
            base=0
            goto 2929
         endif
         base = 1                                  ! Single event file
 2929    continue
      endif
C
      if( base.eq.0.and.
     *basename(1:2).ne.',,'.and.basename(1:5).
     *ne.'INDEX'.and.basename(1:5).ne.'index') then 
                 ! only ask time interval for true base
         write(*,'('' Start Time           (YYYYMMDDHHMMSS): '',$)')
         read(*,'(a14)') starttime
         write(*,'('' End Time, RETURN is to end of month:   '',$)')
         read(*,'(a14)') endtime
         write(*,*)
      endif
C
C   check if interactive operation, 
c
      if (yter.ne.'N') then
         write(*,'('' Interactive operation (N/Y=default)'',$)')
         read(*,'(a1)')  yter
      endif
      yter1=' '       
      write(*,*)
      call sei upc( yter )
      if(yter.eq.' ') yter='Y'
      if(yter.eq.'Y') output=.true.
  
c
c   enter here if used with eev
c
  20  continue

C
C   open  output files
C
      call sei open( unknown$,            ! Open print file.
     &               ' ',                 ! No prompt.
     &               'print.out', iulst, ! File & unit.
     &               b_flag,              ! File exists?.
     &               code )               ! Condition (n/a).
c
c   on  sun, old events seem to remain in print.out file, so delete and reopen
c
      call sei close( delete$, iulst, code ) ! Delete it (stop on error).
      call sei open( unknown$,                ! Open print file.
     &               ' ',                     ! No prompt.
     &               'print.out', iulst,     ! File & unit.
     &               b_flag,                  ! File exists?.
     &               code )                   ! Condition (n/a).

      call sei open( unknown$,                ! Open hyp file.
     &               ' ',                     ! No prompt.
     &               'hyp.out', iuphs,       ! File & unit.
     &               b_flag,                  ! File exists?.
     &               code )                   ! Condition (n/a).

c
c 4/94: write input filename to print output....
c
      write( iulst,                               ! Details to print file.
     &       '('' Input File: '',a40,/)',          ! On format.
     &       iostat=code ) basename                ! On condition, the basename.
      call sei code( fort$, code, iulst, b_flag ) ! Process the outcoe.

c
c   if used with eev, jump straight at location
c
      if(use_eev) goto 1000
C
C   open input event file if multiple event single file option has
C   has been selected 

      if(base.eq.1) then
C
         call sei open( old$,            ! Open old file (stop on error).
     &               ' ',             ! No prompt.
     &               infile, iuin,   ! File & unit.
     &               b_flag,          ! File exists? (n/a).
     &               code )           ! Local condition (n/a).
         eventno = 0
      endif

      keys(1:4)='NEXT'    ! start with next event

 50   continue
c
C----------------------------------------------------------------------
C   Point to same, next or another event 
C----------------------------------------------------------------------
C
      if (base.eq.0) then              ! data base event
         call findevin
     *   (basename,starttime,endtime,keys,0,eventno,
     *   eventfile,fstart,newmonth,status)

c
c   check if end of time interval or errors, then
c   close hyp.out and stop
C     
         if(status.gt.0) then
            write(6,*)' STOP WITH STATUS=',status
            GOTO 900   ! stop
         endif
C
C   open data base input single event file and read it
c   set flag for single event nrecord= -1
c
         nrecord=-1
         call sei open( old$,            ! Open old file (stop on error).
     &                  ' ',             ! No prompt.
     &                  eventfile, iuin,! File & unit.
     &                  b_flag,          ! File exists? (n/a).
     &                  code )           ! Local condition (n/a).
         call indata(iuin,nstat,nphase,nhead,nrecord,
     &               typ,exp,data,id)
         datsave=data(1)
         call sei close( close$, iuin, code ) ! Close file (stop on error).
c
      else                             ! single file  multiple event mode
c
         if(keys(1:4).eq.'NEXT') then
            call indata(iuin,nstat,nphase,nhead,nrecord,
     &               typ,exp,data,id)
            datsave=data(1)
            if(nrecord.eq.0) goto 900   ! check for end of file
            eventno=eventno+1
         endif
c
c  selection by number 
c
         if(keys(1:1).eq.'#') keys(1:4)=keys(2:5)
         if(keys(1:1).eq.'1'
     *   .or.keys(1:1).eq.'2'.or.keys(1:1).eq.'3'
     *   .or.keys(1:1).eq.'4'.or.keys(1:1).eq.'5'
     *   .or.keys(1:1).eq.'6'.or.keys(1:1).eq.'7'
     *   .or.keys(1:1).eq.'8'.or.keys(1:1).eq.'9') then
            read(keys(1:4),'(i4)') nfind
            if(keys(4:4).eq.' ') read(keys(1:3),'(i3)') nfind
            if(keys(3:3).eq.' ') read(keys(1:2),'(i2)') nfind
            if(keys(2:2).eq.' ') read(keys(1:1),'(i1)') nfind
            if(nfind.eq.0) goto 75    ! make choice
            if(nfind.eq.eventno) goto 75

            if (nfind.lt.eventno) then
               rewind( iuin, iostat=code )                ! Rewind unit.
               call sei code( fort$, code, iuin, b_flag ) ! Process outcome.
               eventno = 0
               k = nfind
            else
               k = nfind - eventno
            endif

            do i=1,k
               call indata(iuin,nstat,nphase,nhead,nrecord,
     *                     typ,exp,data,id)
               datsave=data(1)
               eventno=eventno+1
               if (nrecord.eq.0) then
                  write(6,*)' no such number ',nfind
                  rewind( iuin, iostat=code )                ! Rewind unit.
                  call sei code( fort$, code, iuin, b_flag ) ! Process outcome.
                  eventno=0
                  keys(1:4)='NEXT'
                  goto 50
               endif
            enddo
        endif
      endif
c
c--------------------------------------------------------------
c  decide what to do next if in interactive mode
c--------------------------------------------------------------
c
 75   continue
      if(yter.eq.'Y') then

         isindex=2
         do while (data(isindex)(80:80) .ne. '4' .and. 
     &             data(isindex)(80:80) .ne. ' ')
            isindex=isindex+1
         enddo
         isindex=isindex-1  
         nphs=nrecord-1-isindex                   
        
         write(6,289) eventno,data(1)(1:22),nphs
 289     format(1x,'#',i5,1x,a22,' NPHS=',i4,3x,'T Q L #XXX ', $)
         read(5,'(a)') keys
         if (keys(1:4) .eq. '    ') keys(1:4)='NEXT'
         if (keys(1:1) .eq. 'Q'.or. keys(1:1) .eq. 'q' ) goto 900
c
c   type event
c
         if (keys(1:1).eq.'t' .or. keys(1:1).eq.'T') then
            write(6,*)
            nline=0
            do i=1,nrecord
               write(6,'(a)')data(i)
               nline=nline+1
               if (nline.gt.maxline) then
                  nline=0
                  write(6,'('' Return to continue'',$)')
                  read(5,'(a)') text
               endif
            enddo
            keys(1:4)='SAME'      ! use same event
            goto 50               ! back for new choice
         endif
c
c   back one event
c
         if (keys(1:1).eq.'b' .or. keys(1:1).eq.'B') then
            keys(1:4)='BACK'
            goto 50
         endif
c
c  scroll in print.out file
c
         if (keys(1:1).eq.',' .or. keys(1:1).eq.'.') then
            call hyp_print(iulst,keys(1:1),maxline)
            keys(1:4)='SAME'
            goto 50 
         endif
C 
C   if location has been chosen go on, else restart in 50
C
         if (keys(1:1).ne.'l' .and. keys(1:1).ne.'L') goto 50
      endif
C
C------------------------------------------------------------------------
c   location start here
C--------------------------------------------------------------------------
C
 1000 continue

c
c   check if disable location has been set on for this event, in which case
c   event is passed on with no modification to hyp.out
c
      if(data(1)(45:45).eq.'*') then
         write(6,*)'Event flagged not to be located'
         keys(1:4)='NEXT'
         goto 31               ! go to outputs
      endif

c
c  check if event has enough phases and stations and if it
c  should be located, if not still try to calculate magnitudes
c      
      if (nphase .lt. minphase .or. nstat .lt. minstat) then  
         write(6,*) 'NPHASE=',nphase,'  NOT LOCATABLE' 
         if (nphase.lt.minphase) write(6,*)' Too few phases'
         if (nstat.lt.minstat) write(6,*) ' Too few stations'

         locate=.false.       ! was not located
         go to 30
      endif

c
c  if type is E or P, fix depth to 0.0 km
c
      if(exp.eq.'P'.or.exp.eq.'E') then
         data(1)(39:44)='  0.0F'
      endif

C
c    locate one event
C
      init=2
      if(yter.eq.'Y') write(6,*)

c
c   if interactive output, position print file at end
c
      if(yter.eq.'Y') call hyp_print(iulst,'e',maxline) 
c
c   clear old parameters
c
      call clear_old(data,nhead,nrecord,agency)

      do i=nhead+1,nrecord
         data(i)(61:70)='          '
c
c   keep distance if coda is there, so that magnitude can be calculated
c   in case not enough data for a location
c
         if (data(i)(30:33).eq.'    ') data(i)(71:75)='     '
         data(i)(76:79)='    '
      enddo
c
c   save year month and day before updating and calculate abs time
c
      read (data(1),'(1x,i4,1x,2i2)') year,month,day
      call  TIMSEC (year,month,day,0,0,0.0,abstim1)
c
c   save old header line
c
      data_old=data(1)
c
c   normal soulution
c
      call iscloc_wrap(iustat,iulst,data,nhead,nrecord,
     &                  agency,test,locate,use_eev)
 
c
c   check if located
c
      if (.not.locate) goto 30

c
c   check if day has changed, if so correct all arrival times by + or -24 hours
c
      read(data(1),'(1x,i4,1x,2i2)') year,month,day
      call  TIMSEC (year,month,day,0,0,0.0,abstim2)

      ! add 10 for posible round off error
      if (dabs(abstim2-abstim1) .gt. 10.0) then  
         delhour=(dabs(abstim1-abstim2)+10.0)/3600
         if (abstim2.gt.abstim1) delhour=-delhour
         write(6,*)' Day has changed, hours added:', delhour
         write(iulst,*,iostat=code)
     &         ' Day has changed, hours added:', delhour
         call sei code( fort$, code, iulst, b_flag ) ! Process the outcoe.

         do i=nhead+1,nrecord-1
            read(data(i)(19:20),'(i2)') hour
            hour=hour+delhour
            write(data(i)(19:20),'(i2)') hour
         enddo
      endif

c
c   print out event on screen 
c
         if (yter .eq. 'Y') then
            call hyp_print(iulst,'p',maxline)
            write(6,*)
         end if

c-----------------------------------------------------------------------
c   enter here if event could not be located to only calculate magnitude
c   or no locate is desired by test 106 set
c-----------------------------------------------------------------------

 30   continue

c
c   clear old magnitudes, except 3.
c
      data(1)(57:72)='                '
      if (data(1)(77:79) .eq. agency) data(1)(72:79)=' '
c
c   in case event not located, clear out old locations etc, however
c   keep 3. magnitude, unless same agency
c   if location is fixed, do not clean out header info
c   if test 106 set, do not clean location or location agency
c
      if (.not.locate) then
         if (data(1)(44:44).ne.'F')data(1)(39:43)   = '     '
         if (data(1)(45:45).ne.'F')data(1)(24:38)   = '               '
         if (data(1)(44:45).ne.'FF') data(1)(46:48) = '   '
         data(1)(49:72)='                        '
      endif

c
c   enter here if no location is set by using flag *
c
 31   continue

c
c  put in agency unless no location done
c
      if (data(1)(45:45) .ne. '*') data(1)(46:48)=agency

c
c   calculate  magnitudes, unless no location flag is set
c
      if(data(1)(45:45).ne.'*') then
         call update_mag(data,nhead,nrecord,agency,test,output)

         !  also update and average spectral information
         call update_spec(data,nrecord,nhead,agency,output)
      endif

c
c   output header line on screen, if not interactive, event number must be there
c
      if (yter .eq. 'Y') then
         write(6,'(a)') data(1)(1:79)
c
         ! write any second header line with additional mags
         do i=2,nhead
            if (data(1)(1:23) .eq. data(i)(1:23) .and. 
     &          data(1)(46:48).eq. data(i)(46:48))
     &          write(6,'(a)') data(i)(1:79)
         enddo
         write(6,'(a,a)') ' OLD:',data_old(6:79)
         write(6,*)
      else
         write(6,2838) eventno,data(1)(1:71)
2838     format(1x,'#',i5,1x,a71)
      endif

c
c   write out to hyp.out
c
      write(iuphs,'(a80)',iostat=code) (data(i),i=1,nrecord)
      call sei code(fort$,code,iuphs,b_flag)

c
c   stop here if using eev
c
      if(use_eev) goto 999 

      if (yter .ne. 'Y') keys='NEXT'
      if (yter .eq. 'Y') keys='SAME'

      data(1)=datsave
      go to 50  
                                      
  900 continue
      
      write(6,*)' print output in file print.out'
      write(6,*)' CAT-file in file hyp.out'

  999 continue

      call sei close( close$+all$, 0, code ) ! Close all files.

      stop
      end                

cccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine hyp_print(unit,action,maxline)
      implicit none
      include 'libsei.inc'
      external sei code
      integer   code          ! Local condition.
      logical   b_flag        ! Flag a situation?.

c
c   scroll forth and back in print.out file
c
      integer unit           ! unit for print.out file
      character*80 text      ! one line of text
      integer maxline        ! number of lines to print
      character*1 action     ! what to do
      integer i,nline

c
c   print solution from station line
c
      if (action .eq. 'p') then
         nline=0
1        continue
         backspace( unit,iostat=code)
         call sei code(fort$,code,unit,b_flag)

         read(unit,'(a)',iostat=code) text 
         call sei code(fort$,code,unit,b_flag)
         if (b_flag) goto 99

         if (text(4:7) .eq. 'date') goto 2
         backspace(unit,iostat=code)
         call sei code(fort$,code,unit,b_flag)
         goto 1
2        continue

c
c  epi line found, write out
c
         write(6,'(a)') text
         nline=nline+1
         read(unit,'(a)',iostat=code) text
         call sei code(fort$,code,unit,b_flag)
         if (b_flag) goto 99
         write(6,'(a)') text
         nline=nline+1

c
c   now go to station lines
c

3        continue
         read(unit,'(a)',iostat=code) text 
         call sei code(fort$,code,unit,b_flag)
         if (b_flag) goto 99
         if(text(2:4).eq.'stn') goto 4
         goto 3

4        continue
         write(6,'(a)') text
         nline=nline+1

5        continue
         read(unit,'(a)',iostat=code)text 
         call sei code(fort$,code,unit,b_flag)
         if (b_flag) goto 99

c
c   check for end of phases
c
         if(text(1:13).eq.'          ') go to 6
c
c   write station lines 
c
         write(6,'(a80)') text 
         nline=nline+1
         if(nline.gt.maxline) then
            nline=0
            write(6,'('' Return to continue, q to end listing '',$)')
            read(5,'(a)') text
            if (text(1:1) .eq. 'q' .or. text(1:1) .eq. 'Q') goto 6
         endif
         goto 5
 6       continue
         return
      endif

c
c   go back one screen
c

      if (action .eq. '.') then
         do i=1,maxline*2
            backspace (unit,iostat=code)
            call sei code(fort$,code,unit,b_flag)
         enddo
         do i=1,maxline
           read(unit,'(a)',iostat=code) text
           call sei code(fort$,code,unit,b_flag)
           if (b_flag) goto 99
           write(6,'(a)') text
         enddo
         return
      endif
c
c  go foreward one screen
c
      if (action .eq. ',') then
         do i=1,maxline
            read(unit,'(a)',iostat=code) text
            call sei code(fort$,code,unit,b_flag)
            if( b_flag ) goto 99
            write(6,'(a)') text
         enddo
         return
       endif
c
c   goto end of file
c
       if (action .eq. 'e') then
12        continue
          read(unit,'(a)',iostat=code) text
          call sei code(fort$,code,unit,b_flag)
          if( b_flag ) goto 99
          goto 12
       endif

 99    continue
       backspace( unit, iostat=code)
       call sei code(fort$,code,unit,b_flag)
 999   continue
       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine find_file_type(unit,type)
      save
      include 'libsei.inc'
      external sei code
c
      integer   code          ! Local condition.
      logical   b_flag        ! Flag a situation?.

c
c  find if an input file is Nordic (type=1) or index file (type 2) or
c  something else (type = 3)
c
      character*80 text
      integer i,unit,type 

      rewind( unit,iostat=code)
      call sei code(fort$,code,unit,b_flag)
      type=3
c
c   check if nordic file
c
      
c
c  one of the first 2 lines should be a header
c
      do i=1,2
         read(unit,'(a80)',iostat=code) text
         call sei code(fort$,code,unit,b_flag)
         if( b_flag ) goto 10                      ! End of file.
         if((text(22:22).ne.'L'.and.text(22:22).ne.'R'.
     *   and.text(22:22).ne.'D'.and.text(22:22).ne.' ').
     *   or.(text(11:11).ne.' '.or.text(16:16).eq.' '))
     *   type=3
      enddo

c
c   check for index file
c
      rewind( unit,iostat=code)
      call sei code(fort$,code,unit,b_flag)

      read(unit,'(a)',iostat=code) text
      call sei code(fort$,code,unit,b_flag)
      if( b_flag ) goto 10

      do i=1,80
         if((text(i:i+2).eq.'L.S'.or.text(i:i+3).eq.'D.S'.or.
     *      text(i:i+2).eq.'R.S').and.text(6:7).eq.'  ') then
            type=2
            return
         endif
      enddo 

10    continue
      return
      end


