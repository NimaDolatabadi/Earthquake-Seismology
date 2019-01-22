c
c  program to auto register waveform files
c
c  j. havskov, mar 93
c  Updates:
c  jul 93 by jh: version 3.0 
c  apr 94      : also possible to register in local data base
c  nov 24, 1994  : opening files changed
c  dec  15     : **********   version 5.0 ********************
c  jan 96    96: new seisinc
c  nov 97      : put line type 1 in header line
c  dec 8, 97   : change local to ,,,
c  dec 23      : error when asking for overwrite
c---------------------------------------------------------------------
c  oct 98      : ------------   version 7.0 check --------------------
c                make new id, 5 letter base, year 2000
c  jan 99, 5 lo: bug fix with reading base
c  mar 2 99  jh: REG to ARG
c  may2  00  jh: new wave structure
c  apr 29 01 lo: add check for blank line, if blank lines, add all names
c                up to next blank line to one event, this makes it possible
c                to register events with several waveform file names
c  jan 15 10 jh: optionally copy wavform files to WAV including wave databases
c 2010-01-21 pv: changed "copy" to "move" waveform files to WAV
c  mar 22 10 jh: now asks for WAV data base name
c  mar 25 25 lo: fix bug for filenr.lis with blank lines
c  mar 30 10 jh: put move back as an option
c  apr 07 10 lo: take time from filename if Seisan style date/time
c  may 19 11 jh: fix round of error bug resulting from above, pointed out
c                by Wayne
c 2015-11-11 pv: add flag -model to write model in header line
c 2017-01-01 jh: small info output on where s-files are copied
c
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'
       include 'waveform.inc'
       include 'seisan.inc'
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
       integer seiclen
C
C    ============= end of list ==========
C
c-- input file name
      character*80 infile,wave_file(990),xx
c-- filename of datafile including path                              
      character*80 filen
      character   long_text*200
c-- output file name 
      character*80 event_name
      integer nf    ! length of name  
c-- operator
      character*4 operator
c-- id line
      character*80 idline
c-- event type
      character*1 type,type_in
c-- base name
      character*5 base
c-- text string
      character*80 text
c-- main header date and time
      integer year,month,day,hour,min,isec
c-- processing time
      character*12 p_time
      character*14 proc_time
c-- file existence
      logical exist
      real sec
C-- s file data
      character*80 data(999)
c-- counters
      integer i,j,k,cnt
c logical for end of file
       logical          b_eof
       logical  b_flag                     ! Flag!!
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 .. #6
       integer          read01, read02
c write unit #1 .. #5
       integer          write01
c file name
       character*80     chr_file
       logical          nwrite_all,owrite_all ! for writing files
       logical          ignore_all            ! ignore all dublicates
       character*1      choise                ! choise from questions
       logical with_blanks  ! true if filenr.lis has blank lines to sep events
       character*1      copy_wav              ! if c or m, copy or move
c
c  for waveform copy
c
       character*4 cwavyear
       character*2 cwavmon
c-- path to seismo                             
       character*60 top_directory
c-- directory separator
      character*1 dchar
c-- computer type
      logical pc,sun,linux

c-- location model indicator
      character*1 model
      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments

c

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
c
c--------------------------------------------------------------
c   get default parameters
c--------------------------------------------------------------
c
c                                                                               
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)
c
      call dir_char(dchar)   ! directory separation character
c
c   get computertype
c    
      call computer_type(sun,pc,linux)
c
c        
c   get seisan defaults
c
      call get_seisan_def

c
c   get arguments
c
      call get_arguments(narg,arg)

c
c  read argument for location model indicator
c
      model=" "
      if( arg(1)(1:6) .eq. '-model' ) then
        model=arg(2)
      endif



      nwrite_all=.false.
      owrite_all=.false.
      ignore_all=.false.
      b_f_debug$=.false.                  ! file debugging
      b_eof=.false.
c
c   set event type to local
c
 1000 continue
      write(6,*)' Event type for all events: Local:    L (default)'
      write(6,*)'                            Regional: R'
      write(6,*)'                            Distant:  D'
      read(5,'(a)') type_in
      if(type_in.eq.' ') then
         type='L'
      else
         type=type_in
      endif
      if(type.eq.'l') type='L'
      if(type.eq.'r') type='R'
      if(type.eq.'d') type='D'
      if(type.ne.'L'.and.type.ne.'R'.and.type.ne.'D') then
         write(6,*)' Wrong type, enter again'
         goto 1000
      endif

      copy_wav=' '        ! defualt no copy or move
      write(6,*) ' Move (m) or copy (c) waveform files to WAV',
     *' (enter=n) ?'
      read(5,'(a)') copy_wav
c
      if(copy_wav.eq.'c'.and.copy_wav_dir.ne.' ') then
         write(6,'(a,a)') ' Files will be copied to default WAV base: ',
     *   copy_wav_dir
      endif
      if(copy_wav_dir.eq.'m') then
         write(6,'(a,a)') ' Files will be moved to default WAV base: ',
     *   copy_wav_dir
      endif
c
      if(copy_wav.eq.'c'.or.copy_wav.eq.'m')
     *then
         if(copy_wav_dir.ne.' ') then
           write(6,'(a,/,a)')
     *     ' Enter new WAV data base to copy or move to',
     *     ' enter for default base or m for main WAV directory '
         else
           write(6,'(a,a)')
     *     ' Enter  WAV data base to copy or move to',
     *     ' or enter for main WAV directory '
         endif

         write(6,'(a,a)')' If copy to a WAV  base, S-files will'
     *   ,' be copied to the same name REA directory' 

         read(5,'(a)') text
         if(text.ne.' ') then
            do i=2,5
              if(text(i:i).eq.' ') text(i:i)='_'
            enddo
            copy_wav_dir=text(1:5)
            if(copy_wav_dir(1:1).eq.'m') copy_wav_dir=' '
         endif 
      endif
c
c  get base name if not already given above for WAV, should be the same
c
      if((copy_wav.ne.'c'.and.copy_wav.ne.'m').or.
     *   ((copy_wav.eq.'c'.or.copy_wav.eq.'m').and.copy_wav_dir.eq.' ')) 
     *   then
         write(6,*)
     *   ' 1-5 letter base name, return for standard base, '
     *   ,',, for local base'
         read(5,'(a)') base

         if (base(1:1).ne.' ') then
           do i=2,5
             if (base(i:i).eq.' ') base(i:i)='_'
           enddo
          endif
       else
          base(1:5)=copy_wav_dir
       endif

c
c   get operator
c
 60   continue
      write(6,*) ' Operator, max 4 chars'
      read(5,'(a)') operator
      if(operator.eq.'    ') then
         write(6,*)' You must give operator id'
         goto 60
      endif
c
c   open file with file names
c	  	  
      call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     'filenr.lis',          ! File name
     &                     read01,                ! Read unit #1
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition
      if (code .ne. e_ok$) go to 10
      goto 11
 10   continue
        write(6,*)' No filenr.lis file, make a dirf first'
        stop
 11   continue

      i=0
      with_blanks=.false.

 12   continue

      read(read01,'(7x,a)',iostat=code) infile
      call sei code(stop$,code,read01,b_eof)
      if (b_eof) go to 14  

      if (seiclen(infile).le.3) then
        i=1
      else
        if (i.eq.1) then
          with_blanks=.true.
          goto 14
        endif
      endif

      goto 12
 14   continue
      call sei close (close$,read01,code)
      b_eof=.false.
      call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     'filenr.lis',          ! File name
     &                     read01,                ! Read unit #1
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.

      do i=1,990
        wave_file(i)=' '
      enddo
c 
c----------------------------------------------
c   enter here for new  event to register
c----------------------------------------------
c
 1    continue 
c      if (with_blanks) write(*,*) 
c     &   ' putting files seperated by blank lines into same s-file '

      read(read01,'(7x,a)',iostat=code) infile
      wave_file(1)=infile
      call sei code(stop$,code,read01,b_eof)
      if (b_eof) go to 999
      if (infile(1:1).eq.' ') goto 1
c
c read lines until next blank
c
      j=1   ! counter of waveform files for same event

      xx='x'
      if (with_blanks) then
        do while (.not.b_eof.and.
     &            seiclen(xx).gt.0)
          read(read01,'(7x,a)',iostat=code) xx
          call sei code(stop$,code,read01,b_eof)
          if (b_eof) go to 111
          if (seiclen(xx).gt.0) then
            j=j+1
            wave_file(j)=xx(1:seiclen(xx))
          else
            goto 111
          endif
        enddo
      endif
111   continue

      write(6,'(1x,a)') infile(1:60)
c-- check if end of group of filenr.lis	  
      if(infile(1:4).eq.'    ') goto 999
c      wav_filename(1)=infile
      wav_filename(1)=wave_file(1)
c
c read time from filename if Seisan style, otherwise from file
c
      if (wave_file(1)(5:5).eq.'-'.and.
     &    wave_file(1)(8:8).eq.'-'.and.
     &    wave_file(1)(11:11).eq.'-'.and.
     &    wave_file(1)(16:16).eq.'-') then

c010-01-01-0017-28
c        write(*,*) ' taking date from filename '
        read(wave_file(1)(1:18),'(i4,1x,i2,1x,i2,1x,i2,i2,1x,i2)') 
     &   year,month,day,hour,min,isec
         sec=isec  ! WCC added 10/2010, jh added 5-2011
      else
c
c read first header for one event
c	  
        call wav_init
        call read_wav_header(1)
        if(wav_error_message.ne.' ') then
           write(6,'(1x,a)') wav_error_message
           goto 1
        endif
c
c   make output file name
c
        year=wav_year(wav_first)
        month=wav_month(wav_first)
        day=wav_day(wav_first)
        hour=wav_hour(wav_first)
        min=wav_min(wav_first)
        sec=wav_sec(wav_first)
        isec=sec
      endif
      call sfilname
     *(year,month,day,hour,min,isec,base,type,event_name,nf)
      write(*,*) ' sfile: ',event_name

c
c   write header line in file
c
      data(1)=' '
c     write(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1,1x,a1)')     
c    *year,month,day,hour,min,sec,type
      write(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1,2a1)')     
     *year,month,day,hour,min,sec,model,type
      data(2)(1:1)=' '
      data(1)(80:80)='1'
c
c
c   next line is id line
c
      idline(1:40)= ' ACTION:                   OP:     STATU'
      idline(41:80)='S:               ID:                   I'
      WRITE(IDLINE(61:75),'(i4,5I2)')
     *YEAR,MONTH,DAY,HouR,MIN,ISEC
      DO I=61,74
         IF(IDLINE(I:I).EQ.' ') IDLINE(I:I)='0'
      ENDDO
      call systime(p_time,proc_time)
      WRITE(IDLINE(31:34),'(A)')OPERATOR
      WRITE(IDLINE(13:26),'(A)')PROC_TIME
      WRITE(IDLINE(9:11),'(A)')'ARG'
c
c  trace file name
c
      i=index(infile,'.')
      if(i.lt.0) 
     *write(6,*)' *** No . in trace file name ****'
c
c  write trace file name
c
      do i=1,80
         data(3)(i:i)=' '
      enddo
      i=index(infile,' ') -1
      if(i.lt.1) then
         data(3)(2:13)=infile(1:12)
      else
         data(3)(2:i+1)=infile(1:i)
      endif
      data(3)(80:80)='6'

c
c add more waveform files
c
      cnt=0
c add remaining files as '6' lines
      do k=2,j
        if (seiclen(wave_file(k)).gt.1) then 
          i=0
          data(k+2)='                                        '//
     &              '                                       6' 
          cnt=cnt+1
          i=index(wave_file(k),' ') -1 
          data(k+2)(2:i+1)=wave_file(k)(1:i)
        endif
      enddo
c
c   make help line
c
      data(4+cnt)(1:57)=
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO '
c      data(4+cnt)(58:80)='SNR AR TRES W  DIS CAZ7'
      data(4+cnt)(58:80)='AIN AR TRES W  DIS CAZ7'
c
c   blank last line
c
      do i=1,80
         data(5+cnt)(i:i)=' '
      enddo
c
c   check if file exists
c
 3    continue       ! get here from below to again check file existance
c
      inquire(file=event_name,exist=exist)
c
c  If file exists, check what to do
c
c   ignore
c
      if(exist.and.ignore_all) then ! was there but do not overwrite
         write(6,*)' New event ignored'
         goto 1       ! never overwrite
      endif
c
c   over write
c
      if(exist.and.owrite_all) then
         write(6,*)' New event overwriting old'
         go to 100    ! overwrite all
      endif
c
c   make new index
c
      if((exist.and.nwrite_all).or.(exist.and.(choise.eq.'N'.or.
     *    choise.eq.'n'))) then
          write(6,*)' Creating a new id for event'
          call inc_id(idline,event_name,nf)
          goto 3                            ! go and try to open again
      endif

 5    continue      ! here to make a new choise
c
c   make choises here
c
      if(exist) then
          write(6,*)
          write(6,'(a)')' File already registered, options are:'
          write(6,'(a)')' Ignore (leave old event):             Return'
          write(6,'(a)')' Ignore all                            I'
          write(6,'(a)')' Overwrite duplicate:                  O'
          write(6,'(a)')' Overwite all duplicates:              A'
          write(6,'(a)')' Create a new event, different ID:     N'
          write(6,'(a,$)')' Create new events for ALL duplicates: *'
          read(5,'(a)') choise
c
          write(6,*)
        
          if(choise.eq.' ') then
             goto 1     ! get next event
          endif
          if(choise.eq.'i'.or.choise.eq.'I') then
             ignore_all=.true.
             goto 1
          endif
          if(choise.eq.'a'.or.choise.eq.'A') then
             write(6,*)' Sure you want to overwrite ALL (y/n)'
             read(5,'(a)') choise
             if(choise.eq.'y'.or.choise.eq.'Y') then
                 owrite_all=.true.
                 goto 100
             endif
             goto 5 ! another choise  
          endif
          if(choise.eq.'o'.or.choise.eq.'O') then
             write(6,*)' Sure you want to overwrite(y/n)'
             read(5,'(a)') choise
             if(choise.eq.'y'.or.choise.eq.'Y') then
                goto 100
             else
                goto 5   
             endif
          endif
          if(choise.eq.'n'.or.choise.eq.'N'.or.choise.eq.'*') then
             call inc_id(idline,event_name,nf)
             if(choise.eq.'*') nwrite_all=.true.
             goto 3     ! test again
          endif
          goto 5   ! no valid choise
      endif
c
  100 continue 
      choise=' '    ! reset choise
c
      data(2)=idline
      write(6,'(1x,a)') event_name(1:seiclen(event_name))
      write(6,*)

c
c   open and write s-file 
c
      chr_file = event_name(1:80)
              call sei open(unknown$+warn$,       ! Open an unknown status file.
     &                      ' ',                  ! Prompt file name (n/a).
     &                      chr_file,             ! File name
     &                      write01,              ! Write unit #1
     &                      b_old,                ! Already exists? (n/a).
     &                      code)                 ! Returned condition
      if (code .ne. e_ok$) go to 50
      goto 51
 50   continue
        write(6,*)' Data base sub directory not made'
        stop
 51   continue
          write(write01,'(a80)',iostat=code) (data(i),i=1,5+cnt)
          call sei code(stop$,code,write01,b_eof)
          call sei close (close$,write01,code)
c
c-----------------------------------------------------------------                                                                               
c   copy trace file name to WAV if that is chosen
c-----------------------------------------------------------------
c
          if(copy_wav(1:1).eq.'m'.or.copy_wav(1:1).eq.'c' )then
            do i=1,j      ! loop over number of waveform files
c
c   read header to get time
c
              if (wave_file(i)(5:5).eq.'-'.and.
     &            wave_file(i)(8:8).eq.'-'.and.
     &            wave_file(i)(11:11).eq.'-'.and.
     &            wave_file(i)(16:16).eq.'-') then

c010-01-01-0017-28
c                write(*,*) ' taking date from filename '
         read(wave_file(i)(1:18),'(i4,1x,i2,1x,i2,1x,i2,i2,1x,i2)')
     &   year,month,day,hour,min,isec
                write(cwavyear,'(I4.4)') year
                write(cwavmon,'(I2.2)') month
              else
                wav_filename(1)=wave_file(i)
                call wav_init
                call read_wav_header(1)
                if(wav_error_message.ne.' ') then
                    write(6,'(1x,a)') wav_error_message
                endif
                write(cwavyear,'(I4.4)') wav_year(wav_first)
                write(cwavmon,'(I2.2)') wav_month(wav_first)
              endif

c              filen=wav_filename(1)    ! assume only one
              filen=wave_file(i)    
              text(1:5)='move '
              if(copy_wav.eq.'c') text(1:5)='copy '
              if( pc ) then
                 if(copy_wav_dir.eq.' ') then
                    long_text = text(1:5)// filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               filen(:seiclen(filen))
                 else
                    long_text = text(1:5)// filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               copy_wav_dir//dchar//cwavyear(1:4)//dchar //
     &               cwavmon(1:2)//dchar
                 endif

              else if( sun.or.linux ) then
                 text(1:5)='mv   '
                 if(copy_wav(1:1).eq.'c') text(1:5)='cp   '
                 if(copy_wav_dir.eq.' ') then
                    long_text = text(1:5)// filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               filen(:seiclen(filen))
                  else
                     long_text = text(1:5)// filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               copy_wav_dir//dchar//cwavyear(1:4)//dchar //
     &               cwavmon(1:2)//dchar

                  endif
              else
                  chr_err_msg$ = 
     &'**** ERROR: could not determine the computer type'
                  call sei code( stop$, e_init$, 0, b_flag ) ! Halt program
              end if                                     !
c
              write(6,'(1x,a)') long_text
c
c now copy
c
              call systemc( long_text,
     &                         seiclen(long_text) )
c
c  check that file got there
c
              call sei open( check$,             ! Check file exists.
     &                      ' ',                 ! No prompt.
     &        long_text(7+seiclen(filen):seiclen(long_text)),
     &                      0,                   ! Unit (n/a).
     &                      exist,               ! File exists?.
     &                      code )               ! Condition (n/a).
              if(exist) then
                  if(copy_wav_dir.eq.' ') then
                     write(6,*)' File transferred to WAV **********'
                  else
                     write(6,*)' File transferred to WAV base ',
     *               copy_wav_dir,' **********'
                  endif
              else
                  if(copy_wav_dir.eq.' ') then
                     write(6,*)' Failure to transfer to WAV base ',
     *               copy_wav_dir,' **********'
                  endif
                  write(6,*)' Return to continue'
c                  call flush (6)
c                  read(5,'(a4)') i
              endif
           enddo
          endif
  


c
c   back for next event
c	  
      goto 1
c
 999  continue
      call sei close (close$,read01,code) 
      stop
      end
