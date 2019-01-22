c$debug
c
c    PROGRAM TO CHECK S FILES
c
c
c   LATEST UPDATE
c   NOV 22, 88, J.H.: FIRST VERSION
c   JUN 28, 90  J.H.: ONLY USE HEADER LINES, ONLY NORDIC FORMAT
c   AUG 10  90      : MORE CHECKS
c       22          : ----------
c   Nov.    91  C.L.: Transfer to Unix with some minor changeS
c   apr 14  93 by jh: fix for new indata
c   jun 1   93      : output file
c   jul 7   93    jh: version 3.0 *********************************
c   aug 23          : filename to 80 
c   nov 24, 1994  : opening files changed
c   feb     1999  jh: -------------    version 7.0 check -----------
c                     year to 4 digits
c   apr 11  2014  jh: cut oput warning about coda missing
c


c Checkre is a small program that performs some elementary
c checking of the data in a Nordic file. If the files in some fields
c are inconsistent or invalid parameters are used a warning is 
c printed out. The program is good for checcking large data files
c that are going to be split and put into the database. The things checked
c are the following: 
c 1. If depth is greater than 30 km a warning is written
c 2. If a travel time residual is greater than 5 seconds or the rms value of

c  the residuals are greater than 2 seconds the user is warned.
c 3. Correct polarity.
c 4. Correct type of event.
c 5. May be most important: It is checked that the events are in chronological
c    order.
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! Dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c INPUT FILE                                 
      character file1*80
c FILE TYPES P,F, OR S                        
      integer type
c ERROR FLAG FOR OUTPUT 
      integer errflag
c NUMBER OF CODA READINGS                        
      integer ncoda
c EVENT TYPE, L,R OR D                     
      character evtype1*1
c RECORDS IN FILE 1                     
      character data1(max_data)*80
c TIME IN SEC SINCE 1900 EVENT 1         
      double precision time1
c OLD -------------     
      double precision time1_old
c DATES AND TIMES                   
      integer year1, month1, day1, hr1, min1
c SECONDS                                           
      real sec1
c OUTPUT INDICATOR                                 
      integer out
c EXPLOSION INDICATOR                        
      character exp1*1
c NUMBER OF STATIONS                            
      integer nstat1
c NUMBER OF RECORDS and phasesFOR EVENT                 
      integer nrecord1,nphase
c NUMBER OF HEADERS FOR EVENT                   
      integer nhead1
c NUMBER OF STATIONS FROM HEADER                  
      integer nsta
c LATITUDE AND LONGITUDE                          
      real lat, lon
c DEPTH AND RMS                                 
      real depth, rms
c MAGNITUDES                                    
      real mag1, mag2
c MAGNITUDE TYPES                    
      character magtyp1*1, magtyp2*1
c RESIDUAL                                           
      real res
c id line number
      integer id
c indicator for compact file
      logical compact
c EVENT COUNTERS          
      integer nl, nr, nd, ne, np, nu, nevent
c number of errors
      integer nerror
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
c
c
c    SET DEFAULTS AND INITIAL PARAMETERS
c
c COUNTER                                           
      integer i
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      nevent = 0
      nd = 0
      nl = 0
      nr = 0
      ne = 0
      np = 0
c
c  OPEN FILES
c
c PREVIOUS EVENT VERY OLD               
      time1_old = 0
      write(6, fmt=*) ' INPUT FILE  NAME'
      read(5, fmt='(A)') file1
      write(6, fmt=*) 
     &'OUTPUT OF ALL HEADER LINES(1) OR ERROR ONLY(2)'
      read(5, fmt=*) out
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     file1,                 ! File name
     &                     read01,                ! Read unit #1
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
      if (.not. b_old) go to 999
      call sei open(unknown$+warn$,        ! Open a unknown status file.
     &              ' ',                   ! Prompt file name (n/a).
     &              'checkre.out',         ! File name
     &              write01,               ! Write unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      errflag = 0
c
c   find if full file or compact file
c
      call nortype(read01,compact)
      type=2
      if(compact) type=1
c
c   READ  EVENT 
c
    1 continue
      nrecord1 = 1
      if (type .eq. 2) then
          call indata(read01, nstat1, nphase,
     *         nhead1, nrecord1, evtype1, exp1, data1,id)
      else
         read(read01,'(a80)',iostat=code) data1(1)
         call sei code(fort$,code,read01,b_eof)
         if (b_eof) go to 2
c
c   CHECK FOR EOF
c
      end if
c
c   GET ABS TIME FOR  EVENT 1, READ FROM HEADER
c
c EOF                                  
      if (nrecord1 .eq. 0) goto 2
      read(data1(1), fmt=
     &'(1X,I4,1X,2I2,1X,2I2,F5.1,1X,2A1,  F7.3,F8.3,F5.0
     &,5X,I3,F4.1,1X,F3.0,A1,4X,F3.0,A1)') year1, month1, day1, hr1, 
     &min1, sec1, evtype1, exp1, lat, lon, depth, nsta, rms, mag1, 
     &magtyp1, mag2, magtyp2
c
c   check format in file
c
         call check_s(data1,                     ! file
     &                nrecord1,                  ! number of records
     &                file1,                     ! file name
     &                nerror,                    ! number of errors
     &                2,                         ! output destination
     &                write01)                   ! unit number
c
c   CHECK MAGNITUDE
c
       IF(MAG1.NE.0.0.AND.MAG2.NE.0.0) THEN
          IF(ABS(MAG1-MAG2).GE.0.5) then 
           WRITE(6,*)
     *   ' LARGER THEN 0.5 MAGNITUDE DEVIATION'
           WRITE(write01,*)
         write(write01,*,iostat=code)
     *   ' LARGER THEN 0.5 MAGNITUDE DEVIATION'
         call sei code(fort$,code,write01,b_eof)
          endif
          ERRFLAG=1
       ENDIF
c
c  CHECK CODA MAGNITUDE
c
c ABS TIME     
      call timsec(year1, month1, day1, hr1, min1, sec1, time1)
C      if ((mag1 .eq. 0.0) .and. (magtyp1 .eq. 'C')) then
C      write(6, fmt=*) ' CODA READINGS MISSING'
C      errflag = 1
C      end if
c
c   CHECK DEPTH
c
      if (depth .gt. 30.0) then
      write(6, fmt=*) ' DEPTH  LARGER THEN 30'
         write(write01,*,iostat=code) ' DEPTH  LARGER THEN 30'
         call sei code(fort$,code,write01,b_eof)
      errflag = 1
c
c   CHECK RMS
c
      end if
      if (rms .gt. 2.0) then
      write(6, fmt=*) ' RMS LARGER THEN 2.0'
         write(write01,*,iostat=code) ' RMS LARGER THEN 2.0'
         call sei code(fort$,code,write01,b_eof)
      errflag = 1
c
c   CHECK RESIDUALS AND MORE
c
      end if
      if (type .eq. 2) then
c
c  residuals
c
      ncoda = 0
      do i = nhead1 + 1, nrecord1 - 1
      read(data1(i), fmt='(63X,F5.1)') res
      if (abs(res) .gt. 5.0) then
      write(6, fmt=*) ' RESIDUAL LARGER THEN 5'
         write(write01,*,iostat=code) ' RESIDUAL LARGER THEN 5'
         call sei code(fort$,code,write01,b_eof)
      errflag = 1
c
c   CODA READING CHECK, NOT FOR PURE NORSAR REPORTS
c
      end if
c
c CHECK FOR COMPRESSION-DILATATION INDICATOR
c
      if (data1(i)(30:33) .ne. '    ') ncoda = ncoda + 1
      if (((data1(i)(17:17) .ne. ' ') .and. (data1(i)(17:17) .ne. 'C'))
     & .and. (data1(i)(17:17) .ne. 'D')) then
      write(6, fmt=*) ' WRONG POLARITY'
         write(write01,*,iostat=code) ' WRONG POLARITY'
         call sei code(fort$,code,write01,b_eof)
      errflag = 1
      end if
      end do
c     if (((ncoda .eq. 0) .and. (data1(1)(46:48) .ne. 'NAO')) .and. ((
c    &evtype1 .eq. 'R') .or. (evtype1 .eq. 'L'))) then
c     write(6, fmt=*) ' NO CODA READINGS'
c        write(write01,*,iostat=code) ' NO CODA READINGS'
c        call sei code(fort$,code,write01,b_eof)
c     errflag = 1
c     end if
c
c   CHECK TIME
c
      end if
      if (time1 .lt. time1_old) then
      write(6, fmt=*) ' *** EVENT OUT OF ORDER ***'
         write(write01,*,iostat=code) ' *** EVENT OUT OF ORDER ***'
         call sei code(fort$,code,write01,b_eof)
      errflag = 1
      end if
c
c   CHECK TYPES
c
      time1_old = time1
      if (((evtype1 .ne. 'L') .and. (evtype1 .ne. 'D')) .and. (evtype1
     & .ne. 'R')) then
      write(6, fmt=*) ' *** EVENT HAS WRONG OR NO TYPE ***'
         write(write01,*,iostat=code)
     *           ' *** EVENT HAS WRONG OR NO TYPE ***'
         call sei code(fort$,code,write01,b_eof)
      errflag = 1
      end if
      if (((exp1 .ne. 'E') .and. (exp1 .ne. 'P')) .and. (exp1 .ne. ' ')
     &) then
      write(6, fmt=*) ' *** EXPLOSION TYPE WRONG ***'
      write(write01,*,iostat=code) ' *** EXPLOSION TYPE WRONG ***'
      call sei code(fort$,code,write01,b_eof)
      errflag = 1
c
c   OUTPUT
c
      end if
      if ((out .eq. 1) .or. (errflag .eq. 1)) 
     *write(6,'(a)' ) 
     &data1(1)
      if ((out .eq. 1) .or. (errflag .eq. 1)) then 
          write(write01,'(a)',iostat=code) data1(1)
          call sei code(fort$,code,write01,b_eof)
      end if
c
c   COUNT EVENTS
c
      errflag = 0
      nevent = nevent + 1
      if (evtype1 .eq. 'L') nl = nl + 1
      if (evtype1 .eq. 'R') nr = nr + 1
      if (evtype1 .eq. 'D') nd = nd + 1
      if (exp1 .eq. 'E') ne = ne + 1
c
c  BACK FOR NEXT EVENT
c
      if (exp1 .eq. 'P') np = np + 1
c
c   EOF, WRITE SUMMARY
c
      goto 1
    2 continue
      write(6, fmt=202) nl, nr, nd, ne, np, nevent
      write(write01,fmt=202,iostat=code) nl, nr, nd, ne, np, nevent
      call sei code(fort$,code,write01,b_eof)
  202 format(/,27h NUMBER OF LOCAL EVENTS:   ,i5,/,
     &27h NUMBER OF REGIONAL EVENTS:,i5,/,27h NUMBER OF DISTANT EVENTS: 
     &,i5,/,27h NUMBER OF EXPLOSIONS:     ,i5,/,
     &27h NUMBER OF PROB. EXPLOSIONS,i5,//,
     &27h TOTAL NUMBER OF EVENTS:   ,i5,/)
      nu = ((nevent - nl) - nr) - nd
      if (nu .gt. 0) write(6, fmt=*) 
     &' *** NUMBER OF UNIDINTIFIED EVENTS', ' ***', nu
      if (nu .gt. 0) write(write01, fmt=*) 
     &' *** NUMBER OF UNIDINTIFIED EVENTS', ' ***', nu
      write(6,*)
      write(6,*)
      write(6,*)'Output file name is checkre.out'
      write(6,*)
          call sei close (close$,read01,code)
999   continue
      stop 
      end
