C*******************************************************************************
c      PROGRAM UPDATE
c
C   LATEST UPDATE :  4/94 BRL: added include file, eventno to hypocent window
c                    removed unused variables, minor changes to 
c                    interactive section
c   may 11 94 by jh : do double sorting
c   jul 25 94          : fix updates
c   sep 28 94        : do not update agency if hypocenter is fixed
c   nov 9            : do not clear magnides before location
c   dec 94           **********  version 5.0 **********  new hypo
c   jan 10, 94       : new find ev in
CJAB(BGS)May95       : Opening CAT file...peculiar problem on PC...crashing out
CJAB(BGS)May95         with memory problem. If the "sun" statement commented
CJAB(BGS)May95         out then works ok. Thus changed code to use if-then-else
CJAB(BGS)May95         construct.
c   jun 9   jh       : do not write loc. agency when no locate
c aug 16  jh        : do not modify event if no locate flag * is set
c mar 7 96          : subroutine update_spec to hyposub3
c mar 14            : put in clear old, cnages to read_...
c aug 27  96, jh    : enable use of local data base
c sep 19            : clear 3 magnitude if default agency
c oct 11            : fix sorting to use time so p comes before s
c dec 96            : change id's and file names according to origin time
C jan 97            : always hyp.out. cat.out for local base
c feb 19            : change how ordering is done
c mar  18, 97       : reverse arguments in sort_fi (sun)
c mar 20            : do not remove hyp agency if hypocenter fixed
c mar 24            : seems that above was wrong
c may 16            : sort also when no location
c nov 19            : put in option for locking id
c dec 8             : error in above for local data base
c jan 14  98        : ******** year 2000 ******************************
c feb 12 98         : clear residuals etc before location, not after
c jun 11 ,98        : remove routine increment_id and fix it
c aug 31 98         : do not change id line if event marked not to be located
C
c sep 98 jh         :  ------------    version 7.0 check -------------------
c                     year 2000, 5 char base, 5 char station, no pc-sun
c mar 19 lo         : name of sort_fi changed to sortfi
c apr 9 99 jh       : error with eisnenght of data base
c                     add print_ver
c august 20         : add variable use_eev to hypocenter and read_stat_model call
c sep 01 lo         : when changing date of file allow 24 hours
c sep 6  jh         : read_stat_model not changed in change of august 20
c sep 24 jh         : include seisan.inc and read values
c oct 18 jh         : agency_dir to 5 char, caused wrong base for def base
c oct 19            : probelm with weight 4 
c nov 2 99          : recompiled to include new findevin and indata
c feb 17 2000 ,jh   : new update_spec, no mw output
c march 10 2000     : move call clear_old up before location 
c march 29          : update id even if not located by '*' flag, ID might be
c                     wrong
c oct 25 2000       : read secs as f6.3 instead of f5.2 when sorting, compile
c                     for high accuracy 
c dec 6 2000 lo     : only change action to UPD if event had been registered
c may 2 2001 jh     : read in new model before header clear to avoid deleting
c                     type 1 lines not corresponding to active station file
c nov 15 2010 jh    : test for test(107) was missing
c jul 05 2011 lo    : bug when updating over month boundary, where basename seems
c                     to get corrupted at new month
c jan 10 2013 jh    : comment out seidim, now in hypparm
c feb 18 2013 jh    : agency was def base, was 3 , set to 5
c nov  9 2013 lo    : fixed issue when id line not found
c dec 23 2015 jh    : add test to update_spec
c
c
      implicit none
      include 'hypparm.inc'
c     include 'seidim.inc'
      include 'libsei.inc'
      include 'seisan.inc'
      include 'mbn.inc'
c
c---name of top directory
      character*60 top_directory
c---one event in nordic format, one line of data  
c---iaspei file name
      character*80 modnam
      character*1 answer
c---location indicator, can be L, R or D
      character*1 loc_indicator(3)
      logical locate
c--- event type, indcator of explosion, indicator of fixing depth  
      character*1 typ,exp,fixdepth
c---agency for magnitude  
      character*3 agency
c---directory separator character
      character*1 dchar
c---number of header lines, records, stations in nordic file, help variables
      integer nhead,nrecord,nstat,k,nphase,i,itp
c---id line indicator
      integer id
      character*1 id_lock          ! indicator for locked id
      character*1 id_to_lock       ! choise if to lock
c---model type or number
      character*1 model  
C-- hypocenter help variable
      integer init
      character*80 sfiles(max_index$)       ! save s-files names			
c-- save year, month ,day for old location
      integer year,month,day
c-- hour and difference in hours between solutions shifted one day
      integer hour,delhour
c-- abs times for above
      double precision abstim1,abstim2
c-- logical I/O units
      integer iusum,iuphs,iulst,iuin,iutab,iustat
c---logfile name when updating
      character*80 logfile
c---flag for indicatin if first month  when updating
      integer first_month
c---line in log file
      character*80 logrecord
c---number of event in one month in data base
      integer nout
c---time of making update
      character*12 p_time
      character*14 proc_time
c---operator id
      character*4 operator
c---data base code
      CHARACTER*5 TBASE
c---first and last event used when updating
      character*12 first_event,last_event
c-- covarriance matrix, origin time error
      real covar(3,3),oterr
c-- min and sec of phase
      integer min
      real sec
c-- for sorting
      character*5 old_stat
      logical use_eev     ! indicate if call from eev
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 
       integer          read02
      integer j,m,l
C
C  next for data base operation
C
      character*10 keys                      !next choice key
      character*14 starttime,endtime         ! time interval for location
      character*40 basename                  ! data base or file name
      character*80 eventfile                 ! single event file name
      character*80 old_eventfile             ! the one before
      character*5  agency_dir                ! agency used for def. base
      integer status,eventno,newmonth,fstart ! 
c---name of epi file in data base
      CHARACTER*80 EPIFILE

c
c---minimum number of stations to locate
      integer minstat
c---minimum number of phases to locate  
      integer minphase
c---code for current model  
      character*20 model_code  
c---magnitude Ml coefficients
c      real a,b,c,d  
      real y,z
c---number of arguments and function
c      integer nargs

c---added 4/94: sort arrays for sorting output by distanace
      integer ksort(narriv)     
      integer lsort(narriv)     
      character*5 old_dist
c messages, lo 30 June 2016
      integer nmessage
      character*80 message(50)
c mn 
      character*240 mbnfile
      integer read1,seiclen
      logical exists,b_flag

      
c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c 4/94: added logical unit# definitions      
      iuphs=12                                         
      iustat=11
      iusum=7
      iulst=9     
      iuin=21
      iutab=22                         
      use_eev=.false.    ! not from eev, print out station list etc
      
      first_month=0      
c                                                           
c   get computer specifics
c
      call dir_char(dchar)         ! dirctory delimiter character
      call get_def_base(agency_dir)
      call get_seisan_def

c 
c   get directory structure
c
      call topdir(top_directory)
      itp=index(top_directory,' ')-1
c read mbn parameters
      mbnfile='mbn.par'
      inquire(file=mbnfile,exist=exists)
      if(.not.exists) then
         mbnfile=top_directory(1:seiclen(top_directory))//dchar//
     &           'DAT'//dchar//'mbn.par'
        inquire(file=mbnfile,exist=exists)
      endif
      if (exists) then
        call sei open( old$,                 ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   mbnfile,          ! Filename.
     &                   read1,       ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
        call read_mbn(read1)
        call sei close( close$, read1, code ) ! Close file (stop on error).
      endif

c
c   input data base name and time interval
c
 333  continue   ! wrong entry of base name
      write(6,*)
     *'Give 1-5 letter data base name, ,, for local base, ',
     *'return for default'
      read(*,'(a40)') basename
      if(index(basename,' ').eq.2.and.basename(1:5).ne.'   ') goto 333
         tbase=basename(1:5)
         IF(TBASE.EQ.'     ') TBASE(1:5)= agency_dir
         WRITE(6,'(A,A)')
     * ' YOU ARE NOW GOING TO UPDATE THE SEISMIC DATA BASE ',TBASE
         WRITE(6,*)
     * 'ARE YOU SURE YOU KNOW WHAT YOU ARE DOING, IF SO, ENTER '
c
 6666    continue 
         WRITE(6,*)'OPERATOR ID (max 4 char):'
         READ(5,'(A4)') OPERATOR
c
         IF(OPERATOR.EQ.'    ') then
           write(6,*) ' You must give operator'
           goto 6666
         endif
c
         write(6,*)'LOCK ID, (N/Y=Return)'
         read(5,'(a)') answer
         id_to_lock='L'
         if(answer.eq.'N'.or.answer.eq.'n') id_to_lock=' '
         if(tbase(1:2).ne.',,') then   ! a local data base does not need time int.
            WRITE(6,'(a,$)')
     *      ' START TIME year-month, e.g. 199809  :'
            READ(5,'(A14)') STARTTIME
            IF(STARTTIME(7:7).NE.' ') THEN
               WRITE(*,*) ' YOU MUST START WITH BEGINNING OF MONTH'
               WRITE(*,*) ' THIS HAS NOW BEEN CORRECTED'
               STARTTIME(7:14)='        '
            ENDIF
            WRITE(6,'(a,$)')
     *      ' END TIME, RETURN IS TO END OF MONTH :'
            READ(5,'(A14)') ENDTIME
            IF(ENDTIME(7:7).NE.' ') THEN
               WRITE(*,*)' YOU MUST END WITH END OF MONTH'
               WRITE(*,*)' THIS HAS NOW BEEN CORRECTED'
               WRITE(*,*)' THIS HAS NOW BEEN CORRECTED'
               ENDTIME(7:14)='        '
            ENDIF
         else
            starttime='              '  ! start time very early
            endtime  ='              '  ! end of file
         endif
c
c   make shure base is 5 chars
c
         if(tbase(1:1).ne.',') then
            do i=2,5
              if(tbase(i:i).eq.' ') tbase(i:i)='_'
            enddo
         endif
c
c   start with standard model
c
      model=' '     
C
C   do not fix depth  
C
      fixdepth=' '
C
C   open  output files
C
      open(iulst,FILE='print.out',STATUS='unknown')      ! print file
c
c   on  sun, old events seem to remain in print.out file, so delete and reopen
c
      close(iulst,status='delete')
      open(iulst,FILE='print.out',STATUS='unknown')      ! print file

      open(iuphs,file='hyp.out',status='unknown')        ! nordic out
      open(iusum,file='hypsum.out',status='unknown')     ! hypo71 style summary 
C
C   open hypocenter file with model and station and read
c   file is first searched for in your local directory, then in SEISAN
c   DAT directory
c
      call read_stat_mod
     *(agency,model_code,model,minstat,minphase,
     *modnam,loc_indicator,iustat,iuphs,iulst,iusum,iutab,isort,
     &test,dlt,'N',1,use_eev)
c
      keys(1:4)='NEXT'    ! start with next event

c 3/94: added init call to zero residual summary
      call hypocent(4,iustat,iuphs,iulst,iusum,iutab,init,
     &'N',data,modnam,eventno,dlt,isort,test,'N',1,nstat,
     &locate,covar,oterr,nhead,nrecord,use_eev,nmessage,message)
C
c
c---------------------------------------------------------------------
c  event loop starts here, always come back here after locating one event
c---------------------------------------------------------------------
c
 50   continue
c
C----------------------------------------------------------------------
C    next event 
C----------------------------------------------------------------------
C
c
c   get system time so time of update can be time stamped
c
      call systime(p_time,proc_time)
C
C  GET FILENAME OF NEXT EVENT, save previous if any, also save last current
c  event, will be the last for the month when a new month is entered
C
      if(tbase(1:2).ne.',,'.and.first_month.ne.0) then
         last_event=eventfile(fstart:fstart+11) ! for log file
      endif
c
c  save name of first event in a month for log if a data base
c
      if(eventno.eq.1.and.tbase(1:2).ne.',,'.and.first_month.ne.0) then
         first_event=eventfile(FSTART:fstart+11)
      endif
c
	  old_eventfile=eventfile       ! save if next event is from next month
c
      call findevin
     *(basename,starttime,endtime,keys,0,eventno,
     *eventfile,fstart,newmonth,status)
c      write(*,*) ' debug ',eventfile,basename
c
c   if only one month and no data, stop here
c
      if(starttime(1:6).eq.endtime(1:6).
     *and.status.gt.0.and.eventno.eq.0) STOP
C
C  CHECK FOR OUTPUT IN CAT DATA BASE IF A NEW MONTH
c  or end of time period=end of month. Since the first month also
c  get the newmonth flag set, this is checked by first_month.
c  If a local data base, check for end of data.
C
      IF((NEWMONTH.EQ.1.or.status.eq.3).and.first_month.ne.0.or.
     *(tbase(1:2).eq.',,'.and.status.eq.2)) THEN
c
c
c   sort file names, might be different from before
c
      call sortfi(sfiles,nout)
c
c   read all s-files and make cat file in chrono order 
c
      write(6,*)
      write(6,*)' Put CAT file in chrono order'
      write(6,*)
c
      rewind 31
      do k=1,nout
c          write(6,'(1x,a)') sfiles(k)
          call sei open(old$+warn$,            ! Open 
     &                  ' ',                   ! Prompt file name (n/a).
     &                  sfiles(k),             ! File name
     &                  read02,                ! unit #
     &                  b_old,                 ! Already exists? (n/a).
     &                  code)                  ! Returned condition.

         call indata(read02,nstat,nphase,nhead,nrecord,typ,exp,data,id)
         write(31,'(a80)')(data(i),i=1,nrecord)
         call sei code(fort$,code,read02,b_eof)
         call sei close (close$,read02,code) ! close
      enddo
C
C  CLOSE EPI BASE FILE if not a local data base
C
         if(tbase(1:2).ne.',,') then
            CLOSE(31)

c   write in log file, latest information is on top
c   write when one month of locations is finished
c   not done if a local data base
c
c   first make log file name, assume that epifile (now cat file) name now known
c
            logfile(1:itp)=top_directory(1:itp)
            logfile(itp+1:itp+5)=dchar//'REA'//dchar
            logfile(itp+6:itp+10)=tbase
            logfile(itp+11:itp+15)=dchar//'LOG'//dchar
            logfile(itp+16:itp+28)='01-0000-00L.S'
            logfile(itp+29:itp+34)=epifile(itp+16:itp+21)
            write(6,*)
            write(6,'(1x,a)') logfile
            open(13,file=logfile,status='unknown')
            call indata(13,nstat,nphase,nhead,nrecord,typ,exp,data,id)
            rewind 13
            do i=1,80
              logrecord(i:i)=' '
            enddo
            logrecord(2:5)=epifile(itp+16:itp+19)
            logrecord(7:8)=epifile(itp+20:itp+21)
            logrecord(10:13)=operator
            logrecord(15:28)=proc_time
            logrecord(30:41)=first_event
            logrecord(43:54)=last_event
            write(logrecord(56:60),'(i5)') nout
            write(13,'(a80)') logrecord
            if(nrecord.eq.0) write(13,*)'        '
            write(13,'(a80)')(data(i),i=1,nrecord)
            close(13)
            nout=0
         endif
      endif
c
c   if not at end of time period, open next epi file, could be the first
c
	  if(tbase(1:2).eq.',,'.and.eventno.eq.1) 
     *open(31,file='hyp.cat',status='unknown')
c
      if(status.ne.3.and.newmonth.eq.1.and.tbase(1:2).ne.',,') then
c
C
C   open data base input single event file and read first line
c   needed for generating file name
c
         open(21,file=eventfile,status='old')
         read(21,'(a)') data(1)
         close(21)
C
C  YEAR-MONTH
C
         IF(DATA(1)(2:3).EQ.'  ') DATA(1)(2:3)='19'
         epifile(1:ITP)=TOP_DIRECTORY(1:ITP)
         epifile(itp+1:itp+1)=dchar
         epifile(ITP+2:ITP+4)='REA'
         epifile(itp+5:itp+5)=dchar
         epifile(ITP+6:ITP+10)=TBASE
         epifile(itp+11:itp+11)=dchar
         epifile(ITP+12:ITP+14)='CAT'
         epifile(itp+15:itp+15)=dchar
         EPIFILE(itp+16:itp+19)=data(1)(2:5)  ! year
         EPIFILE(itp+20:itp+21)=data(1)(7:8)  ! month
         if(epifile(itp+20:itp+20).eq.' ') epifile(itp+20:itp+20)='0'
         EPIFILE(itp+22:itp+25)= '.CAT'
C
C  OPEN NEW  cat file
C
C
         WRITE(6,'(1x,A)') epifile
         OPEN(UNIT=31,FILE=epifile,STATUS='unknown')
      endif
c
c   now that first event has been read, set first_month flag
c
      first_month=1
c
c   check if  errors or end of time interval, then stop
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
         open(21,file=eventfile,status='old')
         call indata(21,nstat,nphase,nhead,nrecord,typ,exp,data,id)
         if (id.gt.0)id_lock=data(id)(76:76)  ! save id locking parameter
C
C------------------------------------------------------------------------
c   location start here
C--------------------------------------------------------------------------
C
 1000 continue
c
c   assume this is after year 1900 and put in header
C
      if(data(1)(2:3).eq.'  ') data(1)(2:3)='19'
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
c   check if a new model has to be read in
c
      if(model.ne.data(1)(21:21)) then
         model=data(1)(21:21)
         call read_stat_mod
     *   (agency,model_code,model,minstat,minphase,
     *   modnam,loc_indicator,iustat,iuphs,
     &   iulst,iusum,iutab,isort,test,dlt,
     *   'N',1,use_eev)
         write(*,*)' Model changed, "'
     &    //model//'"'
      endif

c
c   clear old parameters
c
      call clear_old(data,nhead,nrecord,agency)
C
C check if type of event should be located 
C
      locate=.false.
      do i=1,3 
         if(typ.eq.loc_indicator(i)) locate=.true.
      enddo                                     
c
c  check if event has enough phases and stations and if it
c  should be located
c      
      if(nphase.ge.minphase.and.locate.
     *and.nstat.ge.minstat) then  
c
c   set fixdepth indicator
c
         if(fixdepth .ne. ' ')then
            data(1)(44:44)=fixdepth
         endif
c
c  if type is E or P, fix depth to 0.0 km
c
         if(exp.eq.'P'.or.exp.eq.'E') then
            data(1)(39:44)='  0.0F'
         endif
      else
         write(6,291)typ,nphase
 291     format
     *   (1x,'type',1x,a1,2x,'NPHASE=',
     *   i2,'  NOT LOCATABLE') 
         if(nphase.lt.minphase) write(6,*)' Too few phases'
         if(nstat.lt.minstat) write(6,*) ' Too few stations'
c
c  check if location indicator is set or correctly set
c
         if(typ.ne.'L'.and.typ.ne.'R'.and.typ.ne.'D')
     *   write(6,*)' Missing or wrong event type, must be L,R or D'
         if(.not.locate.and.(typ.eq.'L'.or.typ.eq.'R'.or.typ.eq.'D'))
     *   write(6,*)' Check your STATION0.HYP file under Locating ',
     *   'indicator'
         keys='NEXT'
         locate=.false.       ! was not located
C
C  No location, still try to calculate magnitudes  
C
         go to 30
      endif
C
c    locate one event
C
      init=2
      do i=nhead+1,nrecord
         data(i)(61:70)='          '
c
c   keep distance if coda is there, so that magnitude can be calculated
c   in case not enough data for a location
c
         if(data(i)(30:33).eq.'    ') data(i)(71:75)='     '
         data(i)(76:79)='    '
      enddo
c
c check for XNEAR and XFAR in sfile
c
      xxnear=-1.
      xxfar=-1.

      if (test(107).eq.1.) then
         do i=2,nhead
           if(data(i)(80:80).eq.'3'.and.data(i)(2:6).eq.'XNEAR')then 
c             read(data(i)(2:37),'(6x,f6.1,6x,f6.1,6x,f5.1)')
             read(data(i)(2:37),'(6x,f6.1,6x,f6.1)')
     &         xxnear,xxfar
              write(*,*) ' found xnear/xfar ',xxnear,xxfar
               if(iulst.gt.0) then
                 write(iulst,*)
                 write(iulst,'(a)')
     &          ' starting depth, xnear, xfar '//
     &          'from event file '
                 write(iulst,'(4(f6.1,1x))') ztr,xxnear,xxfar
               endif
            endif
         enddo
      endif

c
c   save year month and day before updating and calculate abs time
c
      read(data(1),'(1x,i4,1x,2i2)') year,month,day
      call  TIMSEC (year,month,day,0,0,0.0,abstim1)
      call hypocent(2,iustat,iuphs,iulst,iusum,iutab,
     &init,'N',data,modnam,eventno,
     &dlt,isort,test,'N',1,nstat,locate,covar,oterr,nhead,nrecord,
     *use_eev,nmessage,message)
c
c   check if located
c
      if(.not.locate) goto 30
c
c   check if day has changed, if so correct all arrival times by + or -24 hours
c
      read(data(1),'(1x,i4,1x,2i2)') year,month,day
      call  TIMSEC (year,month,day,0,0,0.0,abstim2)
      if(dabs(abstim2-abstim1).gt.10.0) then  ! add 10 for posible round off err
         delhour=(dabs(abstim1-abstim2)+10.0)/3600
         if(abstim2.gt.abstim1) delhour=-delhour
         write(6,*)' Day has changed, hours added:', delhour
c change lo
c        if(abs(delhour).gt.23) then
         if(abs(delhour).gt.24) then
             write(6,*)' Unrealistic change, will stop'
             stop
         endif
c         write(iulst,*)' Day has changed, hours added:', delhour
         do i=nhead+1,nrecord-1
            read(data(i)(19:20),'(i2)') hour
            hour=hour+delhour
            write(data(i)(19:20),'(i2)') hour
         enddo
      endif
c
c-----------------------------------------------------------------------
c   enter here if event could not be located to only calculate magnitude
c-----------------------------------------------------------------------
c
 30   continue


c
c
c   clear old magnitudes, except 3. unless the agency is the default
c
      data(1)(57:72)='                '
      if(data(1)(77:79).eq.agency) data(1)(72:79)=' '
c
c   in case event not located, clear out old locations etc, however 
c   keep 3. magnitude, unless the agency is the default
c   if location is fixed, do not clean out header info
c
      if(.not.locate) then
         if(data(1)(44:44).ne.'F')data(1)(39:43)='     '
         if(data(1)(45:45).ne.'F')data(1)(24:38)='               '
         if(data(1)(44:45).ne.'FF') data(1)(46:48)='   ' ! keep old aga if fixe
         data(1)(49:72)='                        '
         if(data(1)(77:79).eq.agency) data(1)(72:79)=' '
       endif
c
c   enter here if no location is made by using flag *
c
 31   continue
c
c  put in agency unless both epicenter and depth has been fixed
c  in which case it must be the existing agency resposible for hypocenter
c  or if no location done
c
      if(data(1)(44:45).ne.'FF'.and.data(1)(45:45).ne.'*') 
     *data(1)(46:48)=agency
C
c   calculate  magnitudes, if no location flag is not set
C
         if(data(1)(45:45).ne.'*') then
            call update_mag(data,nhead,nrecord,agency,test,.false.)
c
c  update and average spectral information
c
            call update_spec(data,nrecord,nhead,agency,test,.false.)
         endif

C
c   output header line on screen
C
            write(6,2838) eventno,data(1)(1:71)
 2838       format(1x,'#',i5,1x,a71)



C
C
C   UPDATE ID LINE
C
         IF(ID.NE.0) THEN
c
c   check that id line has not changed since first read
c   could happen when adding a new error line
c
            do i=1,nhead+1   
              if(data(i)(80:80).eq.'I') id=i
            enddo
            WRITE(DATA(ID)(31:34),'(A)')OPERATOR
            WRITE(data(id)(13:26),'(A)')PROC_TIME
c
c only if event registered or split, change action to update
c
           if(data(id)(9:10).eq.'RE'.or.data(id)(9:11).eq.'SPL')
     &        WRITE(data(id)(9:11),'(A)')'UPD'
           
         ENDIF
c
c   write number of stations in header
c
         write(data(1)(49:51),'(i3)') nstat
C
c  write output files
C


c
c remove empty xnear/xfar lines, lo
c
      k=0
      do i=1,nhead
        if (data(i)(2:6).eq.'XNEAR') then
          read(data(i)(2:37),'(6x,f6.1,6x,f6.1,6x,f5.1)')
     &        y,z
          if (y.eq.0..and.z.eq.0) k=i
        endif
      enddo
      if (k.gt.0) then
        write(*,*) ' removing line xnear '
        do i=k,nrecord-1
          data(i)=data(i+1)
        enddo
        nhead=nhead-1
        nrecord=nrecord-1
      endif
        


c 4/94: added sort by distance option

c     if(test(71).ne.0.0.and.locate)then
      if(test(71).ne.0.0)then
        do i=nhead+1,nrecord-1
         read(data(i)(71:75),'(f5.0)')dlt(i-nhead)
        end do

        call r4sort(nrecord-1-nhead,dlt,isort)
c
c   now sort so the distance sorted phase lines, for each group of
c   as in the original file
c
        old_dist=data(isort(1)+nhead)(71:75)
        old_stat=data(isort(1)+nhead)(2:6)
        k=0
        l=0
        do i=nhead+1,nrecord-1
           l=l+1
           read(data(isort(l)+nhead),'(18x,2i2,f6.2)') hour,min,sec
           if(data(isort(l)+nhead)(71:75).eq.old_dist.and.
     *     data(isort(l)+nhead)(2:6).eq.old_stat) then
              k=k+1
              lsort(k)=isort(l)
              dlt(k)=sec+min*60+hour*3600
              if(i.ne.nrecord-1) goto 73   ! at the end, always sort what is lef
              l=l+1                        ! since this is last value
           endif
c
c   if here, new distance or station or last group
c
           if(k.gt.1) then
               call r4sort(k,dlt,ksort)
               m=1
               do j=l-k,l-1
                  isort(j)=lsort(ksort(m))
                  m=m+1
               enddo
           endif
           if(i.eq.nrecord-1) goto 73
           old_dist=data(isort(l)+nhead)(71:75)
           old_stat=data(isort(l)+nhead)(2:6)
           k=1    ! this is the first of the next group
           dlt(k)=sec+min*60+hour*3600
           lsort(k)=isort(l)
 73        continue
         enddo
c
c   write out
c
        rewind 21
        do i=1,nhead
           write(21,'(a80)') data(i)
           write(31,'(a80)') data(i)
           write(iuphs,'(a80)') data(i)    ! hyp.out 
        enddo 
        do i=nhead+1,nrecord-1
            write(21,'(a80)') data(isort(i-nhead)+nhead)
            write(31,'(a80)')data(isort(i-nhead)+nhead)
            write(iuphs,'(a80)')data(isort(i-nhead)+nhead)
        end do          
        write(21,'(a80)') data(nrecord)
        write(31,'(a80)') data(nrecord)
        write(iuphs,'(a80)') data(nrecord)
      else                    
c
c   write unsorted
c
        rewind 21
        write(21,'(a80)') (data(i),i=1,nrecord)
C
C  WRITE IN MONTHLY FILE and hyp.out
C
            WRITE(31,'(A80)') (DATA(i),i=1,nrecord)
            WRITE(iuphs,'(A80)') (DATA(i),i=1,nrecord)
      endif    
      NOUT=NOUT+1
c      close(21)
c
c   Rewrite s-file with name according to origin time, at same time
c   check header line and correct id line
c
c   If ID is locked, just continue, since id and file name should remain
c   unless flag is set to unlock
c
      if(id_lock.ne.'L'.or.id_to_lock.eq.' ') then 
         call rewrite_sfile
     *   (21,sfiles(nout),tbase,proc_time,operator,data,id_to_lock)
      else
         sfiles(nout)=eventfile    ! it was not changed
         close(21)
      endif
c
      keys='NEXT'
c
c   get next event
c
      go to 50                                        

C
c   this was the end
C
  900 continue
      call hypocent(3,iustat,iuphs,iulst,iusum,iutab,
     &init,'N',data,modnam,eventno,
     &dlt,isort,test,'N',1,nstat,locate,covar,oterr,nhead,nrecord,
     *use_eev,nmessage,message)
 
c  close files (BRL 4/94)
      close(7)
      close(11)
      close(12)
      
      write(6,*)' Print output in file print.out'
      if(tbase(1:2).eq.',,') write(6,*)' CAT file hyp.cat'  
      write(6,*)' Summary file in hypsum.out'
      write(6,*)' Nordic in hyp.out'
      stop
      end                                                               


      subroutine rewrite_sfile
     *(read01,sfil,base,proc_time,operator,data,id_to_lock)
c
c  routine runs the split function on file on read01 in seisan data base base.
c  only one event is written out. The output file name is sfil.
c  proc_time and operator as in program update.
c  special for this version is that the file name and id is always taken
c  from the origin time since it is used in connection with update.
c  modified from split dec 96, see also program split. Routine assumes that
c  the input file is there and that the id line is present.
c
c  variable data just put in window to share memory space with main program
c
c  id_to_lock: lock on id put in at end
c
       implicit none

C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'seidim.inc'
       include 'libsei.inc'                ! Open file definitions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c-- OUTPUT FILE NAME                                   
      CHARACTER*80 OUTFL
      integer nf            ! length of file name
c-- EVENT TYPE, L,R OR D                              
      CHARACTER*1  EVTYPE			
c-- idline
      character*80 idline
      character*1 id_to_lock
c-- TOP DIR FOR REA               
      CHARACTER*40 TOP_DIRECTORY                
c-- dir separator
      character*1 dchar
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
      character*14 proc_time 
C-- OPERATOR ID
      CHARACTER*4 OPERATOR
c-- HELP FORMAT LINE 0: NO, 1: YES               
      INTEGER FORMAT_INDICATOR			
c-- RECORDS IN FILE                                
      CHARACTER*80 DATA(*)			
c-- DATES AND TIMES                             
      INTEGER century,YEAR,MONTH		        
     *,DAY,HR,MIN,ISEC                                                          
c-- SECONDS                                                    
      REAL SEC					
c-- EXPLOSION INDICATOR                                
      CHARACTER*1 EXP	  			
c-- INDICATOR FOR USE OF DATA BASE                      
      CHARACTER*5 BASE				
c-- NUMBER OF STATIONS                                     
      INTEGER NSTAT				
c-- NUMBER OF RECORDS FOR EVENT                          
      INTEGER NRECORD				
c-- NUMBER OF HEADERS FOR EVENT                            
      INTEGER NHEAD				
c-- NUMBER OF PHASES FOR EVENT	  
      INTEGER NPHASE
C-- ID LINE NUMBER
      INTEGER ID
c-- TIME IN SEC SINCE 1900 EVENT                    
      DOUBLE PRECISION TIME			
c-- MINIMUM TIME --------------   
      DOUBLE PRECISION MIN_TIME                 
c-- DAY OF YEAR, NOT USED                                    
      INTEGER DOY				
c-- COUNTER                                                  
      INTEGER I,K	
c   length of top_directory
      integer topdirlen
c  agency
      character*5 agency
      character*80 sfil    ! name of sfile returned
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 
       integer          read01
c write unit #1
       integer          write01
c file name
       character*80     chr_file
C                                                                               
C    SET DEFAULTS AND INITIAL PARAMETERS                                        
C
      call topdir(top_directory)
      call get_def_base(agency)
      topdirlen=index(top_directory,' ') -1
      call dir_char(dchar)
      b_f_debug$=.false.                  ! file debugging
c      nevent=0
c
C  rewind FILE                                                                   
C                         
      rewind read01              
C
C   START OF LOOP-------------------------------------------------
C                                       
                                                                                
 1    CONTINUE                                                                  
C                                                                               
C   READ  EVENT                                                                 
C          
         CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,
     *   EVTYPE,EXP,DATA,ID)
C                                                                               
C   CHECK FOR EOF                                                               
C                                                                               
      IF(NRECORD.EQ.0) GO TO 2
C
C                                                                               
C   CHECK IF FORMAT EXPLANATORY LINE IN FILE                        
C                                                                               
      FORMAT_INDICATOR=0                                                        
      DO K=1,NHEAD                                                              
         IF(DATA(K)(80:80).EQ.'7') FORMAT_INDICATOR=1                           
      ENDDO                                                                     
C                                                                               
C   GET ABS TIME FOR  EVENT, READ YEAR,MONTH AND DAY FROM HEADER,               
C   HR, MIN AND SEC FROM EARLIEST STATION IF NOT GIVEN IN HEADER
C                                                                               
      READ(DATA(1),'(1X,2I2,1X,2I2,1X,2I2,1X,F4.1)')                             
     *CENTURY,YEAR,MONTH,DAY,HR,MIN,SEC
C
C   ASSUME 1900 IF NO CENTURY GIVEN
C
      IF(CENTURY.EQ.0) CENTURY=19
      year=century*100+year                                                               
C                                                                               
C   CHECK IF HR AND MIN IN HEADER                                               
C                                                                               
c-- FIND EARLIEST TIME if phase records available                  
      IF(DATA(1)(12:15).EQ.'    '.and.(nrecord-1).gt.nhead) THEN		
         MIN_TIME=10E20                                                         
         DO I=NHEAD+1,NRECORD-1                                                 
            READ(DATA(I),'(18X,2I2,1X,F5.1)') HR,MIN,SEC                        
c-- ABS TIME              
            CALL TIMSEC(YEAR,MONTH,DAY,HR,MIN,SEC,TIME)	
            IF(TIME.LT.MIN_TIME) MIN_TIME=TIME                                  
         ENDDO                                                                  
         CALL SECTIM(MIN_TIME,YEAR,DOY,MONTH,DAY,HR,MIN,SEC)                    
c--  EARLIEST TIME  
         WRITE(DATA(1)(1:20),'(1X,I4,1X,2I2,1X,2I2,1X,F4.1)') 
     *   YEAR,MONTH,DAY,HR,MIN,SEC                   
         DATA(1)(80:80)='1'
      ENDIF                                                                     
c
C                                                                               
C  GENERATE OUTPUT FILE NAME                                                    
C     
      ISEC=SEC      
C
      IF(BASE.NE.'     '.and.base(1:2).ne.',,') THEN
         WRITE                                                                  
     *   (OUTFL,307)BASE,YEAR,MONTH,DAY,HR,MIN,ISEC,EVTYPE,YEAR,MONTH           
 307     FORMAT(
     *   '/',A5,'/',I4,'/',I2,'/',I2,'-',2i2,'-',i2,a1,'.S',i4,I2)               
C
         DO I=1,34                                                              
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                
         ENDDO   
         nf=34                                                               
      ELSE                                                                      
         WRITE(OUTFL,303)DAY,HR,MIN,ISEC,EVTYPE,'S',YEAR,MONTH                      
 303     FORMAT(I2,'-',2i2,'-',i2,a1,'.',a1,i4,I2)                                  
         DO I=1,19                                                                 
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                   
         ENDDO
         nf=19          
      ENDIF
c
c   make id line if not there, else update current id line
c
      if(id.ne.0) idline=data(id)
      if(id.eq.0) THEN
         idline(1:40)= ' ACTION:                   OP:     STATU'
         idline(41:80)='S:               ID:                   I'
         WRITE(IDLINE(31:34),'(A)')OPERATOR
         WRITE(IDLINE(13:26),'(A)')PROC_TIME
         WRITE(IDLINE(9:11),'(A)')'UPD' 
      endif
c
c  update id line
c
      WRITE(IDLINE(61:75),'(i4,5I2)')
     *YEAR,MONTH,DAY,HR,MIN,ISEC
      DO I=61,74
         IF(IDLINE(I:I).EQ.' ') IDLINE(I:I)='0'
      ENDDO
c
c   close and delete input file
c
      close (read01,status='delete') ! close
C                                                                               
C  OPEN AND WRITE OUTPUT FILE                                                   
C
 3    continue                 ! get here from below when file did exist
      IF(BASE(1:2).NE.',,') THEN                                                    
         chr_file = top_directory(1:topdirlen)//dchar//'REA'//outfl
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    chr_file,              ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            sfil=chr_file
      ELSE
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    outfl,                 ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            sfil=outfl
      ENDIF
c
c   make new index
c
      if(b_old) then
          write(6,*)' Creating a new id for event'
          call inc_id(idline,outfl,nf)
          call sei close (close$,write01,code) ! close
          goto 3                            ! go and try to open again
      endif
c
  10  continue 
C
C   WRITE FILE
C                                  

C                                                                     
c-- ORIGINAL HEADER LINES FIRST
C   IF NO ID LINE, PUT IN AS NUMBER 2
C
          write(write01,'(a80)',iostat=code) data(1)
          call sei code(fort$,code,write01,b_eof)
c
c   put in id update lock
c
      idline(76:76)=id_to_lock
c
c  if new id line, write as #2, else where it was before
c
      if(id.eq.0) then
          write(write01,'(a80)',iostat=code) idline
          call sei code(fort$,code,write01,b_eof)
      else
         data(id)=idline
      endif
c
c   write rest of header lines, skip format help lines
c
      if(nhead.ge.2) then
	     do i=2,nhead 
            if(data(i)(80:80).ne.'7') then
               write(write01,'(a80)',iostat=code) data(i)
            endif
	     enddo
      endif
      call sei code(fort$,code,write01,b_eof)
C
c-- write help line 
C
      write(write01,243,iostat=code)
      call sei code(fort$,code,write01,b_eof)
 243  FORMAT(
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO ',
c     *'SNR AR TRES W  DIS CAZ7')              
     *'AIN AR TRES W  DIS CAZ7')              
c-- WRITE REST OF EVENT         
       write(write01,'(a80)',iostat=code)
     * (data(i),i=nhead+1,nrecord)
      call sei code(fort$,code,write01,b_eof)
      call sei close (close$,write01,code)
c
 2    continue
      return                                                                      
      END                                                                       
                                                                                
