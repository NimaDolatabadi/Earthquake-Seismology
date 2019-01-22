
C########################################################################### 
   
      SUBROUTINE INDATA
     *(FILE,NSTAT,NPHAS,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
C
C   ROUTINE READS ONE EVENT IN NORDIC FORMAT INTO TEXT
C   ARRAY DATA. FILE IS OPENED OUTSIDE ROUTINE  AND IT IS
C   ASSUMED THAT NEXT RECORD TO READ IS A HEADER.
C
c   FIRST VERSION FROM AROUND 1984
C
C
c   updates:
c   1990-01-30, R.N.ARVIDSSON TO FIT PC-DOS.
c   apr 4 91 by j.h. : GIVE OUT NUMBER OF PHASES AND STATIONS
C   NOV 23 91        : CHANGE TO NEW NORDIC FORMAT, SOME CLEAN UP
c   jan 16 92        : do not count phasses and stations weighted out
C   AUG 28 92        : MAKE SURE LAST LINE IS 80 CHARS BLANK
c   jul    93 by jh  : version 3.0
c   nov 12 93        : count number of stations taking into account that
c                      stations might not follow each other in file
C!JAB(BGS)Nov94      : install error handling.
c   feb 7     jh     : fix up blank line handling a bit, no message,
c                      check for ctl z in file on PC
c   dec 14, 95       : check for array dimension in data
c   dec 7   97 jh    : count phases and stations correctly in case of weight
c                      4 and 9
c   sep 98, jh       : ------ version 7.0 check: year 2000, 5 char station
c   october 29,99 jh : nhead not defined if only header lines and no blank 
c                      line at end
c   january 17 11 jh ; also stop if reading errror on first header line,
c                      with gfortran on pc, if already at end, a new read
c                      would create an error, not eof
c                      if eof while reading headers only, number of headers
c                      were wrongly counted
c
C   INPUT :   FILE: FILE NUMBER TO READ FROM
c             nrecord: if -1, always read to end of file
C
C   OUTPUT:   NSTAT:   NUMBER OF STATIONS
C             NPHAS:   NUMBER OF PHASES FOR EVENT
C             NHEAD:   NUMBER OF HEADER RECORDS
C             NRECORD: TOTAL NUMBER OF RECORDS FOR EVENT INCLUDING LAST
C                      BLANK, IF ZERO, END OF FILE IS HIT
C             TYPE:    EVENT TYPE (D,R OR L)
C             EXP:     E FOR EXPLOSION
C             DATA:    EVENT RECORDS
C             ID:      RECORD NUMBER FOR ID LINE, 0: NO ID LINE
C
C    NOTE*** nphas and nstat are not counted for weighted out phases
c
      IMPLICIT NONE
      include 'seidim.inc'
C---TYPE OF LINE
      CHARACTER*1 LINE_TYPE  
C---SEE ABOVE  
      CHARACTER*1 TYPE,EXP
C---STATION CODES 
      CHARACTER*5 OLDSTA,NEWSTA
C---DATA AND A BLANK LINE 
      CHARACTER*80 DATA(*),BLANK
C---LOGICAL FILE NUMBER FOR READ 
      INTEGER FILE
C---SEE ABOVE  
      INTEGER NPHAS,NRECORD,NHEAD,NSTAT,ID
c-- endof line read or not
      logical end_of_file,test
C---HELP VARIABLE
      INTEGER IERR,i       
C
C    Seisan library inserts and routines...
C    ======================================
C
       include  'libsei.inc'               ! Library definitions & data defns.
       external  sei code                  ! Error encoder.
       integer   code                      ! local condition
       logical   b_eof                     ! Flag end of file.
C
C    ============= end of list ==========
C
c
c   check if read should be done to end of file, this is
c   an option used if it is known that only one event is available
c   and can be used to avoid problems if an extra blank line is present
c
      end_of_file=.false.
      if(nrecord.eq.-1) end_of_file=.true.
c
      do i=1,80
        blank(i:i)=' '
      enddo
c
      NRECORD=1
      IERR=0
      ID=0
      NSTAT=0
      nphas=0
      OLDSTA='****'
C
C   READ FIRST HEADER, ALSO BACK HERE IF FIRST LINE IS BLANK
C
 150  CONTINUE
cold      READ(FILE,'(A80)',iostat=code) DATA(1)
c
c  read, now (jan 2011) als stop if reading error
c
      read(file,'(a80)',end=19,err=19)data(1)
      goto 20
 19   continue
      goto 9999
 20   continue

cold      call sei code( fort$,                      ! Stop on error.
cold     &               code,                       ! Condition.
cold     &               file,                       ! File with condition.
cold     &               b_eof )                     ! Flag end of file.
cold      if( b_eof ) goto 9999                      ! Return to caller.
C
      do i=1,80
        if(ichar(data(1)(i:i)).eq.26) goto 150  ! skip ctl z
      enddo
      IF(DATA(1).EQ.BLANK) THEN
         WRITE(6,*)'**********  BLANK HEADER LINE ***************'
         IERR=1
         GO TO 150
      ENDIF
      READ(DATA(1),'(21X,2A1,56X,A1)') TYPE,EXP,LINE_TYPE
      IF(IERR.EQ.1) THEN
C
C     WRITE DATE ETC BEFORE BLANK HEADER LINE
C
         WRITE(6,'(A80)') DATA(1)
         IERR=0
      ENDIF
C
C   LOOP FOR READING HEADERS
C
 1    CONTINUE
         NRECORD=NRECORD+1 
         if(nrecord.gt.max_data) then
            write(6,*) ' Data dimension max_data exeded, is now ',
     *      max_data
            write(6,*) ' Can be changed in seidim.inc'
            stop
         endif
 111     continue                                   ! from just below
         READ(FILE,'(A80)',IOSTAT=CODE) DATA(NRECORD)

         call sei code( fort$,                      ! Stop on error.
     &                  code,                       ! Condition.
     &                  file,                       ! File with condition.
     &                  b_eof )                     ! End of file?.
         if( b_eof ) then
             nhead=nrecord-1       !cxxxxx                   ! will add blank line
             
             goto 3                         
         endif
C
         do i=1,80
           if(ichar(data(nrecord)(i:i)).eq.26) goto 111  ! skip ctl z
         enddo
         READ(DATA(NRECORD),'(79X,A1)') LINE_TYPE
         
C
C   CHECK FOR ID LINE
C
         IF(DATA(NRECORD)(80:80).EQ.'I') ID=NRECORD
C
C   CHECK IF MORE HEADERS
C
         IF(LINE_TYPE.NE.' '.AND.LINE_TYPE.NE.'4') THEN
            GO TO 1
         ENDIF
         NHEAD=NRECORD-1
C
C   READ PHASE RECORDS UNTIL BLANK LINE OR END OF FILE
c   always to eof if flag set, used with single events to trap
c   blank lines
C
 2    CONTINUE
      IF(DATA(NRECORD)(1:40).EQ.BLANK(1:40)) then 
         if(end_of_file) then
c
c   check if last line, then ok
c
            READ(FILE,'(A80)',IOSTAT=CODE) DATA(NRECORD)
            call sei code( fort$,                   ! Stop on error.
     &                     code,                    ! Condition.
     &                     file,                    ! File with condition.
     &                     b_eof )                  ! End of file?.
            if( b_eof ) goto 3                      ! & skip
C
c            write(6,*) ' Blank line in file, check !!'
c            goto 10
             goto 3
         else
            goto 3
         endif
      endif
 10   continue
c
c   if phase is weighted out, phase and station is not counted
c
      if(data(nrecord)(15:15).ne.'4'.and.data(nrecord)(15:15).ne.'9') 
     *then  
c      if(data(nrecord)(15:15).ne.'4'.and.data(nrecord)(15:15).ne.'9' 
c     *   .and.data(nrecord)(11:11).ne.' ')    ! don't count E and I without phase
C
C   COUNT AZIMUTH AS A PHASE
C
         if(data(nrecord)(47:51).ne.'     ') nphas=nphas+1  
C
C  COUNT PHASES
C
         if(data(nrecord)(11:13).ne.'   ') nphas=nphas+1    
c
c   count stations, check all previous stations
c
         newsta=data(nrecord)(2:6)
         test=.false.
         if (data(nrecord)(11:13).ne.'   '.or.
     *       data(nrecord)(47:51).ne.'     ') then
          do i=nhead+1,nrecord-1
           if(newsta.eq.data(i)(2:6)
     *     .and.data(i)(15:15).ne.'4'
     *     .and.data(i)(15:15).ne.'9') then
             test=.true.
           endif
          enddo
          if(.not.test) nstat=nstat+1
         endif
      endif
c
c   position at next record
c
      nrecord=nrecord+1 
         if(nrecord.gt.max_data) then
            write(6,*) ' Data dimension max_data exeded, is now ',
     *      max_data
            write(6,*) ' Can be changed in seidim.inc'
            stop
         endif

 777  continue                                ! from just below
      READ(FILE,'(A80)',IOSTAT=CODE) DATA(NRECORD)
      call sei code( fort$,                   ! Stop on error.
     &               code,                    ! Condition.
     &               file,                    ! File with condition.
     &               b_eof )                  ! End of file?.
      if( b_eof ) then                        ! & skip.
         goto 3                                  ! 
      else                                    ! Otherwise.
         if(ichar(data(nrecord)(1:1)).eq.26) goto 777  ! skip ctl z
         GO TO 2                              ! Next to check.
      end if                                  !
C
C   END OF DATA FOR ONE EVENT
C
 3    CONTINUE
C
C   MAKE SURE LAST RECORD REALLY IS BLANK, COULD BE  ANYTHING IF
C   LAST LINE IF FILE WAS NOT 80 CHARS OR NOT BLANK
C
      DATA(NRECORD)=BLANK

      RETURN
C
C   END OF FILE
C
9999  NRECORD=0     ! indicates no data

      RETURN
      END
