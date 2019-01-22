c$debug
C
C    PROGRAM SPLIT                                                              
C                                                                               
c
c
C    SPLITS AN  S-FILE TO INDIVIDUAL FILES. FILENAME                        
C    IS DETERMINED BY  (IN ORDER: ID, TIME IN HEADER, OR IF NO TIME IN HEADER,                     
C    THEN A HEADER TIME IS MADE FROM EARLIEST ARRIVAL TIME IN FILE. IF NO ID LINE
C    IS GIVEN, IT WILL BE PUT IN, TYPE I.                                                                    
C
C                                                                               
C    MADE BY JENS HAVSKOV NOVEMBER 1988                                         
C                                                                               
C                                                                               
C    LATEST UPDATE    
C
c    nov 94          : ****************   version 5.0 *******************
c    nov 24, 1994    : opening files changed
c    dec 13          : create new index if same file name
c    jan 11          : new get def base call
c    jan 31  1995    : bug
c    jan 31 1996     : split of compact file
c    dec 23 96       : bug when 2 header lines and 2 header line not a help
c                      line, 2 header line was then lost.
c    jan 22 98 jh    : *********  year 2000 changes ***********************
c                    : remove to LIB routine inc_id
c    spe 98 by jh    : ---------- version 7.0 check ----------------
c                      5 char base names, only use long S-file names, include
c                      seidim to dimention DATA
c    oct 25, 00 jh:   high accuracy
C
       implicit none

C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c-- INPUT FILE                                         
      CHARACTER*80 FILE				
c-- OUTPUT FILE NAME                                   
      CHARACTER*80 OUTFL			
      integer nf            ! length of file name
c-- EVENT TYPE, L,R OR D                              
      CHARACTER*1  EVTYPE			
c-- idline
      character*80 idline
c-- TOP DIR FOR REA               
      CHARACTER*40 TOP_DIRECTORY                
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
      CHARACTER*12 p_time
      character*14 proc_time 
C-- OPERATOR ID
      CHARACTER*4 OPERATOR
c-- dir separator
      character*1 dchar
c-- RECORDS IN FILE                                
      CHARACTER*80 DATA(max_data)
c-- TIME IN SEC SINCE 1900 EVENT                    
      DOUBLE PRECISION TIME			
c-- OLD -------------             
      DOUBLE PRECISION TIME_OLD                 
c-- MINIMUM TIME --------------   
      DOUBLE PRECISION MIN_TIME                 
c-- DAY OF YEAR, NOT USED                                    
      INTEGER DOY				
c-- DATES AND TIMES                             
      INTEGER YEAR,MONTH		        
     *,DAY,HR,MIN,ISEC                                                          
c-- SECONDS                                                    
      REAL SEC					
c-- EXPLOSION INDICATOR                                
      CHARACTER*1 EXP	  			
c-- INDICATOR FOR USE OF DATA BASE                      
      CHARACTER*5 BASE				
c-- HELP FORMAT LINE 0: NO, 1: YES               
      INTEGER FORMAT_INDICATOR			
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
      character*1 answer    ! answer
      logical compact       ! compact input file or not
c-- EVENT COUNTERS                  
      INTEGER NL,NR,ND,NE,NP,NU,NEVENT       	
c-- TOTAL NUMBER OF RECORDS                            
      INTEGER REC_TOTAL				
c-- COUNTER                                                  
      INTEGER I,K				
c   length of top_directory
      integer topdirlen
c  def base
      character*5 def_base
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
       logical          nwrite_all,owrite_all  ! for witing files
       logical          ignore_all            ! ignore all dublicates
       character*1      choise                ! choise form questions

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

C                                                                               
C    SET DEFAULTS AND INITIAL PARAMETERS                                        
C
      call topdir(top_directory)
      call get_def_base(def_base)
      topdirlen=index(top_directory,' ') -1
      call dir_char(dchar)
      nwrite_all=.false.
      owrite_all=.false.
      ignore_all=.false.
      b_f_debug$=.false.                  ! file debugging
c
      idline(1:40)= ' ACTION:                   OP:     STATU'                                                  
      idline(41:80)='S:               ID:                   I'
      NEVENT=0                                                                  
      ND=0                                                                      
      NL=0                                                                      
      NR=0                                                                      
      NE=0                                                                      
      NP=0                                                                      
                                                                                
c-- PREVIOUS TIME VERY OLD                         
      TIME_OLD=0               
C                                                                               
C  OPEN FILES                                                                   
C                                       
 101  continue                                        
      WRITE(6,*)' INPUT FILE NAME'                                              
      READ(5,'(A)') FILE                                                      
c
c   open and check if file available
c
            call sei open(old$,                  ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    file,                  ! File name
     &                    read01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
c
      if(.not.b_old) goto 101     ! try again
c
c   check type
c
      call nortype(read01,compact)
      if(compact) then
         write(6,*)' Input file is compact, do you want to split (y/n)'
         read(5,'(a)') answer
         if(answer.ne.'y'.and.answer.ne.'Y') stop
         write(6,*)' Really sure ??????? (y/n)'
         read(5,'(a)') answer
         if(answer.ne.'y'.and.answer.ne.'Y') stop
      endif
      WRITE(6,*)' BASE NAME FOR OUTPUT FILES:'                                             
      WRITE(6,'(a,a,a)')'   ',def_base,'  FOR STANDARD DATA BASE: '                               
      WRITE(6,*)'   1-5 LETTER CODE FOR OTHER BASE'                           
      WRITE(6,*)'   RETURN FOR SPLIT UP IN LOCAL DIRECTORY'                     
      READ(5,'(A5)') BASE
      WRITE(6,*)' OPERATOR ID, MAX 4 CHARS'
 777  CONTINUE
      READ(5,'(A)') OPERATOR
      IF(OPERATOR.EQ.'        ') THEN
         WRITE(6,*)' YOU MUST GIVE OPERATOR ID '
         GOTO 777
      ENDIF           
c
c   fill out blanks in base name 
c
      if(base.ne.'     ') then
         do i=2,5
           if(base(i:i).eq.' ') base(i:i)='_'
         enddo
      endif                                            
C
C   GET SYSTEM TIME
C
      CALL SYSTIME(p_time,PROC_TIME)
C                                                                                
C
C   START OF LOOP-------------------------------------------------
C                                       
                                                                                
 1    CONTINUE                                                                  
C                                                                               
C   READ  EVENT                                                                 
C          
      if(compact) then
         read(read01,'(a)',end=2) data(1)
         data(2)=' '
         nrecord=2
         nhead=1
         nphase=0
         evtype=data(1)(22:22)
         exp=data(1)(23:23)
         id=0
      else                                                                     
         CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,
     *   EVTYPE,EXP,DATA,ID)
      endif
C                                                                               
C   CHECK FOR EOF                                                               
C                                                                               
      IF(NRECORD.EQ.0) GO TO 2
C
C   CHECK IF ID LINE PRESENT, IF NOT GET TIME FROM EVENT DATA
C
      IF(ID.EQ.0) THEN
C                                                                               
C   GET ABS TIME FOR  EVENT, READ YEAR,MONTH AND DAY FROM HEADER,               
C   HR, MIN AND SEC FROM EARLIEST STATION IF NOT GIVEN IN HEADER
C                                                                               
         READ(DATA(1),'(1X,i4,1X,2I2,1X,2I2,1X,F4.1)')                             
     *   YEAR,MONTH,DAY,HR,MIN,SEC
C
C   ASSUME 1900 IF NO CENTURY GIVENT
C
C                                                                               
C   CHECK IF HR AND MIN IN HEADER                                               
C                                                                               
c-- FIND EARLIEST TIME                   
         IF(DATA(1)(12:15).EQ.'    '.and.(.not.compact)) THEN		
            MIN_TIME=10E20                                                         
            DO I=NHEAD+1,NRECORD-1                                                 
               READ(DATA(I),'(18X,2I2,F6.1)') HR,MIN,SEC                        
c-- ABS TIME              
               CALL TIMSEC(YEAR,MONTH,DAY,HR,MIN,SEC,TIME)	
               IF(TIME.LT.MIN_TIME) MIN_TIME=TIME                                  
            ENDDO                                                                  
            CALL SECTIM(MIN_TIME,YEAR,DOY,MONTH,DAY,HR,MIN,SEC)                    
c--  EARLIEST TIME  
            WRITE(DATA(1)(1:20),'(1X,I4,1X,2I2,1X,2I2,1X,F4.1)') 
     *      YEAR,MONTH,DAY,HR,MIN,SEC                   
            DATA(1)(80:80)='1'
         ENDIF                                                                     
      ELSE
         READ(DATA(ID)(61:74),'(i4,5I2)')YEAR,MONTH,DAY,HR,MIN,ISEC
         SEC=ISEC
      ENDIF
      WRITE(6,201) YEAR,MONTH,DAY,HR,MIN,SEC,EVTYPE,EXP,                        
     *NRECORD                                                                   
c
c   temporary fixes 
c
c     data(1)(45:45)='*'
c     data(1)(72:79)=data(1)(56:63)
c     data(1)(56:63)=' '
c
c   temporary fixes end, make sure above is commented out
c
 201  FORMAT(1X,i4,2(I3),2X,I2,':',I2,F5.1,2X,A1,1X,A1,                            
     *'     RECORDS:',I4)                                                       
C                                                                               
C   CHECK TIME                                                                  
C                                                                               
      IF(TIME.LT.TIME_OLD) THEN                                                 
         WRITE(6,*)' *** EVENT OUT OF ORDER ***'                                
      ENDIF                                                                     
      TIME_OLD=TIME                                                             
C                                                                               
C   CHECK TYPES                                                                 
C                                                                               
      IF(EVTYPE.NE.'L'.AND.EVTYPE.NE.'D'.AND.EVTYPE.NE.'R') then
c         WRITE(6,*)' *** EVENT HAS WRONG OR NO DISTANCE INDICATOR ***'
c         write(6,*) 'Type will be set to L'
          if(evtype.eq.' ') evtype='D'
          data(1)(22:22)='D'
       endif
c      IF(EXP.NE.'E'.AND.EXP.NE.'P'.AND.EXP.NE.' ')                              
c     *   WRITE(6,*)' *** EXPLOSION TYPE WRONG ***'                              
C                                                                               
C   COUNT EVENTS                                                                
C                                                                               
      NEVENT=NEVENT+1                                                           
      IF(EVTYPE.EQ.'L') NL=NL+1                                                 
      IF(EVTYPE.EQ.'R') NR=NR+1                                                 
      IF(EVTYPE.EQ.'D') ND=ND+1                                                 
      IF(EXP.EQ.'E') NE=NE+1                                                    
      IF(EXP.EQ.'P') NP=NP+1                                                    
C                                                                               
C  GENERATE OUTPUT FILE NAME                                                    
C                                                                               
      ISEC=SEC                                                                  
C                                                                               
C   CHECK IF DATA BASE OR NOT                                                   
C
      IF(BASE.NE.'     ') THEN
         WRITE                                                                  
     *   (OUTFL,307) dchar,BASE,dchar,YEAR,dchar,MONTH,dchar,
     *   DAY,HR,MIN,ISEC,EVTYPE,YEAR,MONTH
 307     FORMAT(
     *   a1,A5,a1,I4,a1,I2,a1,I2,'-',2i2,'-',i2,a1,'.S',i4,I2)               
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
c   put in id if not there
c
      if(id.ne.0) idline=data(id)
      if(id.eq.0) THEN
         WRITE(IDLINE(61:75),'(i4,6I2)')
     *   YEAR,MONTH,DAY,HR,MIN,ISEC
         DO I=61,74
            IF(IDLINE(I:I).EQ.' ') IDLINE(I:I)='0'
         ENDDO
      ENDIF
      WRITE(IDLINE(31:34),'(A)')OPERATOR
      WRITE(IDLINE(13:26),'(A)')PROC_TIME
      WRITE(IDLINE(9:11),'(A)')'SPL' 
C                                                                               
C   CHECK IF FORMAT EXPLANATORY LINE IN FILE                        
C                                                                               
      FORMAT_INDICATOR=0                                                        
      DO K=1,NHEAD                                                              
         IF(DATA(K)(80:80).EQ.'7') FORMAT_INDICATOR=1                           
      ENDDO                                                                     
C                                                                               
C  OPEN AND WRITE OUTPUT FILE                                                   
C
      goto 4                                                                               
 3    continue                 ! get here from below when file did not exist
         call sei close (close$,write01,code) ! opened below
 4    continue
      IF(BASE.NE.'     ') THEN                                                    
         chr_file = top_directory(1:topdirlen)//dchar//'REA'//outfl
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    chr_file,              ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
      ELSE
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    outfl,                 ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
      ENDIF
c
c   check if file exists, if not continue. If file exists, check what to do
c
c   ignore
c
      if(b_old.and.ignore_all) then ! was there but do not overwrite
         call sei close (close$,write01,code) ! opened below
         write(6,*)' New event ignored'
         goto 1       ! never overwrite
      endif
c
c   over write
c
      if(b_old.and.owrite_all) then
         write(6,*)' New event overwriting old'
         go to 10     ! overwrite all
      endif
c
c   make new index
c
      if((b_old.and.nwrite_all).or.(b_old.and.(choise.eq.'N'.or.
     *    choise.eq.'n'))) then
          write(6,*)' Creating a new id for event'
          call inc_id(idline,outfl,nf)
          goto 3                            ! go and try to open again
      endif
 5    continue            ! only go here if no valid choise
c
c   make choises here
c
      if(b_old) then
          write(6,*)
          write(6,'(a)')' File already exists, options are:'
          write(6,'(a)')' Ignore (leave old event):             Return'
          write(6,'(a)')' Ignore all                            I'
          write(6,'(a)')' Overwrite duplicate:                  O'
          write(6,'(a)')' Overwite all duplicates               A'
          write(6,'(a)')' Create a new event, different ID:     N'
          write(6,'(a,$)')' Create new events for ALL duplicates: *'
          read(5,'(a)') choise
          write(6,*)
        
          if(choise.eq.' ') then
             call sei close (close$,write01,code) 
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
                 goto 10  ! overwrite
             endif
             goto 5 ! another choise  
          endif
          if(choise.eq.'o'.or.choise.eq.'O') then
             write(6,*)' Sure you want to overwrite(y/n)'
             read(5,'(a)') choise
             if(choise.eq.'y'.or.choise.eq.'Y') goto 10  ! overwrite
             goto 5   
          endif
          if(choise.eq.'n'.or.choise.eq.'N'.or.choise.eq.'*') then
             call inc_id(idline,outfl,nf)
             if(choise.eq.'*') nwrite_all=.true.
             goto 3     ! test again
          endif
          goto 5   ! no valid choise
      endif
c
  10  continue 
      choise=' '    ! reset choise
C
C   WRITE FILE
C                                                                     
c-- ORIGINAL HEADER LINES FIRST, MINUS FORMAT INDICATOR IF THERE,
C   IF NO ID LINE, PUT IN AS NUMBER 2
C
          write(write01,'(a80)',iostat=code) data(1)
          call sei code(fort$,code,write01,b_eof)
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
c   write rest of header lines except help line
c
      if(nhead.ge.2) then 
         do i=2,nhead
            if(data(i)(80:80).ne.'7')
     *      write(write01,'(a80)',iostat=code)data(i)
            call sei code(fort$,code,write01,b_eof)
         enddo
      endif
C
c-- write help line if not a compact file
C
      if(.not.compact) then
         write(write01,243,iostat=code)
         call sei code(fort$,code,write01,b_eof)
      endif
 243  FORMAT(
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO ',
c     *'SNR AR TRES W  DIS CAZ7')              
     *'AIN AR TRES W  DIS CAZ7')              
c-- WRITE REST OF EVENT         
       write(write01,'(a80)',iostat=code)
     * (data(i),i=nhead+1,nrecord)
      call sei code(fort$,code,write01,b_eof)
      call sei close (close$,write01,code)
C                                                                               
C   COUNT RECORDS                                                               
C                                                                               
      REC_TOTAL=REC_TOTAL+NRECORD                                               
c-- ONE WAS ADDED           
      IF(FORMAT_INDICATOR.EQ.0.and..not.compact) REC_TOTAL=REC_TOTAL+1 
C                                                                               
C  BACK FOR NEXT EVENT                                                          
C                                                                               
      GO TO 1                                                                   
C                                                                               
C   EOF, WRITE SUMMARY                                                          
C                                                                               
 2    CONTINUE                                                                  
      WRITE(6,202)NL,NR,ND,NE,NP,NEVENT,REC_TOTAL                               
 202  FORMAT(/,' NUMBER OF LOCAL EVENTS:   ',I5,                                
     *       /,' NUMBER OF REGIONAL EVENTS:',I5,                                
     *       /,' NUMBER OF DISTANT EVENTS: ',I5,                                
     *       /,' NUMBER OF EXPLOSIONS:     ',I5,                                
     *       /,' NUMBER OF PROB. EXPLOSIONS',I5,                                
     *       //,' TOTAL NUMBER OF EVENTS:   ',I5,                               
     *       //,' TOTAL NUMBER OF RECORDS:  ',I5/)                              
                                                                                
      NU=NEVENT-NL-NR-ND                                                        
      IF(NU.GT.0) WRITE(6,*) ' *** NUMBER OF UNIDINTIFIED EVENTS',              
     *' ***',NU
      call sei close (close$,read01,code)
      STOP                                                                      
      END                                                                       
                                                                                
