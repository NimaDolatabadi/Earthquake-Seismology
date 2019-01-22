C
C   program to update data base with existing solutions from s-files
c
c   updates:
c
c   april 14, 1999 jh : -------- version 7.0 check ---------------------
c                       many changes
c   aug   24, 1999 lo : init logfile and epifile
c   nov 2 1999   jh   : recompiled to include new findevin and indata
c   nov 25 2001  jh   : def base (agecy_dir) was still 3 chars, changed to 5
c   jan 4 2010   jh   : bug with initializing first_event
c
      implicit none
      include 'seidim.inc'
c---name of top directory
      character*60 top_directory
c---one event in nordic format, one line of data  
c--- event type, indcator of explosion, indicator of fixing depth  
      character*1 typ,exp
c---directory separator character
      character*1 dchar
c---number of header lines, records, stations in nordic file, help variables
      integer nhead,nrecord,nstat,nphase,i,itp
c---id line indicator
      integer id
c---logfile name when updating
      character*80 logfile
c---flag for indicatin if first month  when updating
      integer first_month
c---line in log file
      character*80 logrecord
      character*80 data(max_data)   ! s-file
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
C
C  next for data base operation
C
      character*10 keys                      !next choice key
      character*14 starttime,endtime         ! time interval for location
      character*40 basename                  ! data base or file name
      character*80 eventfile                 ! single event file name
      character*40 outname
      character*5  agency_dir                ! agency used for def. base
      integer status,eventno,newmonth,fstart ! 
c---name of epi file in data base
      CHARACTER*80 EPIFILE
c sei clen
      integer seiclen

c
c---number of arguments and function
c      integer nargs
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c                                                           
c   get computer specifics
c
      call dir_char(dchar)         ! dirctory delimiter character
      call get_def_base(agency_dir)

      epifile = ' '

c 
c   get directory structure
c
      call topdir(top_directory)
      itp=index(top_directory,' ')-1
c
c   input data base name and time interval
c
      write(6,*)'Give 1-5 letter data base name, return for default'
      read(*,'(a40)') basename                       
         tbase=basename(1:5)
         IF(TBASE.EQ.'     ') TBASE(1:5)= agency_dir
         WRITE(6,'(A,A)')
     * ' YOU ARE NOW GOING TO UPDATE THE SEISMIC DATA BASE ',TBASE
         WRITE(6,*)
     * 'ARE YOU SURE YOU KNOW WHAT YOU ARE DOING, IF SO, ENTER '
         WRITE(6,*)'OPERATOR ID (max 4 char):'
         READ(5,'(A4)') OPERATOR
         IF(OPERATOR.EQ.'    ') STOP
         WRITE(6,'(a,$)')
     *   ' START TIME(Year-Month)                          : '
         READ(5,'(A14)') STARTTIME
            IF(STARTTIME(7:7).NE.' ') THEN
            WRITE(*,*) ' YOU MUST START WITH BEGINNING OF MONTH'
            WRITE(*,*) ' THIS HAS NOW BEEN CORRECTED'
            STARTTIME(7:14)='        '
         ENDIF
         WRITE(6,'(a,$)')
     *   ' END TIME(Year-Month), RETURN IS TO END OF MONTH : '
         READ(5,'(A14)') ENDTIME
         IF(ENDTIME(7:7).NE.' ') THEN
            WRITE(*,*)' YOU MUST END WITH END OF MONTH'
            WRITE(*,*)' THIS HAS NOW BEEN CORRECTED'
            WRITE(*,*)' THIS HAS NOW BEEN CORRECTED'
            ENDTIME(7:14)='        '
         ENDIF
c
c   make shure base is 5 chars
c
         if(tbase(1:1).ne.',') then
            do i=2,5
              if(tbase(i:i).eq.' ') tbase(i:i)='_'
            enddo
         endif

C
C   open  output files
C
c
      keys(1:4)='NEXT'    ! start with next event
      nout=0              ! jh jan 10

c
c---------------------------------------------------------------------
c  event loop starts here, always come back here after locating one event
c  or to make a new choice in interactive mode
c---------------------------------------------------------------------
c
 50   continue
c
C----------------------------------------------------------------------
C    next event 
C----------------------------------------------------------------------
C
C
C  GET FILENAME OF NEXT EVENT, save previous if any
c  only save if at least one event has been read, jh jan 10
C
         if(nout.gt.0) last_event=eventfile(fstart:fstart+11) ! for log file
         call findevin
     *   (basename,starttime,endtime,keys,0,eventno,
     *   eventfile,fstart,newmonth,status)
         outname(1:17)=eventfile(fstart:fstart+16)
C
C  CHECK FOR OUTPUT IN CAT DATA BASE IF NEW MONTH
c  or end of time period=end of month
C
      IF((NEWMONTH.EQ.1.or.status.eq.3)) THEN
c
c   get system time
c
         call systime(p_time,proc_time)
c
c   write in log file, latest information is on top
c   write when one month of locations is finished
c
         if(first_month.ne.0) then
c
c   first make log file name, assume that epifile (now cat file) name now known
c
            logfile(1:40) = '                                        '
            logfile(41:80)= '                                        '
            logfile(1:itp)=top_directory(1:itp)
            logfile(itp+1:itp+5)=dchar//'REA'//dchar
            logfile(itp+6:itp+10)=tbase
            logfile(itp+11:itp+15)=dchar//'LOG'//dchar
            logfile(itp+16:itp+28)='01-0000-00L.S'
            logfile(itp+29:itp+34)=epifile(1:6)
            write(6,*)
            write(6,'(1x,a)') logfile(1:seiclen(logfile))
c changed lo 13-08-2002
            open(13,file=logfile,status='unknown',access='append')
c            call indata(13,nstat,nphase,nhead,nrecord,typ,exp,data,id)
c            rewind 13
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

c            if(nrecord.eq.0) write(13,*)'        '
c            write(13,'(a80)')(data(i),i=1,nrecord)
            close(13)
            nout=0
          endif
          first_month=1
          first_event=outname(1:12)
          nout=0
c
c   if not at end of time period, open next epi file
c
          if(status.ne.3) then
c
C
C  CLOSE PREVIOUS EPI BASE FILE IF ANY
C
            CLOSE(31,ERR=3333)
 3333       CONTINUE
C
C  YEAR-MONTH
C
            IF(DATA(1)(2:3).EQ.'  ') DATA(1)(2:3)='19'
            EPIFILE(1:4)=eventfile(fstart-8:fstart-5)
            EPIFILE(5:6)=EVENTFILE(FSTART-3:FSTART-2)
            EPIFILE(7:10)= '.CAT'
            WRITE(6,'(1x,a,a10)') 'CATFILE IS ',EPIFILE(1:10)
C
C  OPEN NEW  cat file
C
            OPEN(UNIT=31,FILE=top_directory(1:itp)//
     &      dchar//'REA'//dchar//tbase//dchar//
     &      'CAT'//dchar//EPIFILE(1:10),STATUS='unknown')
c         
         endif
      endif
c
c   check if  errors or end of time interval, then stop
C     
         if(status.gt.0) then
            write(6,*)' STOP WITH STATUS=',status
            stop   ! stop
         endif
C
C   open data base input single event file and read it
c   set flag for single event nrecord= -1
c
         nrecord=-1
         open(21,file=eventfile,status='old')
         call indata(21,nstat,nphase,nhead,nrecord,typ,exp,data,id)
         close(21)
C
c   event is passed on with no modification to data base
c

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
            WRITE(data(id)(9:11),'(A)')'UPD'
         ENDIF
C
c  write normal output file
C

c
c   write out
c
      WRITE(31,'(A80)') (DATA(i),i=1,nrecord)
      NOUT=NOUT+1

      keys='NEXT'

      go to 50                                        
C
c   this was the end
C
      
      stop
      end                                                               

