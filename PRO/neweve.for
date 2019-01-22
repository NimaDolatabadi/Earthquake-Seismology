CJAB(BGS)Mar28/1995 : changes to assist in data entry
CSTART**************************************************************************
c              PROGRAM TO CREATE SEISMIC DATA FILE
C              ===================================
C
C    WRITTEN BY EZRA AND REUBEN, MAY 1990
c
c
C    CHANGES AND UPDATES:
C    1991-04-04; CL : INITILIZATION OF TEXT STRINGS ADDED
C    1992  2  7  JH : NEW NORDIC, EXTRA HEADER LINE, do not ask for hr min
c    sep 10 92 ny jh: fix problems
c    sep 8  93      : check if output file there, fix bug
c    sep 18         : stop any where
CJAB(BGS)Mar95      : Make header easier to enter.
c---------------------------------------------------------------------
c    oct 98          --------------   version 7.0 check ---------------
c                    5 station chars, remove explanation line for header
c
c                     and sec in header
C
C                VARIABLE IDENTIFICATION
C                ------------------------
C           STAT: station name
C           SP: S=instrument type: S=short period
c                                  L=long period, I=intermediate
c               P=component(Z,N,E)
C           I=Quality indicator(I,E,( ) )
C           PHAS=phase identification(P,S,Pg e.t.c)
c           W= weighting indicator
c           D=first motion(U: up; D:down; or C: compression; D:dilatation)
c           CODA=duration(to noise) in seconds
c           AMPLIT=amplitude(zero-peak) in nanometers
c           PERI=period in seconds
c           HR= hours,MM=minutes,SECON=seconds
c      
                  integer      start              !JAB(BGS)Mar95. Text position.
                  CHARACTER*80 TEXT/' '/,OLDTEX/' '/
                  character    chr_text *(80)     !JAB(BGS)Mar95.
                  character answer
		  INTEGER ERROR


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c          
      WRITE(6,*) '  *************************************************'
	  WRITE(6,*) ' This program creates a file with new events'
	  WRITE(6,*) ' Output file: neweve.out'
	  WRITE(6,*) ' neweve.out is used as input to program: SPLIT'
      WRITE(6,*) '  *************************************************'
	  WRITE(6,*) '   '
	  WRITE(6,*) '        Note the following.....!!!!!'
	  WRITE(6,*) '   '
	  WRITE(6,*) ' 1. Text will be converted to UPPERCASE'   !JAB(BGS)Mar95.
	  WRITE(6,*) ' 2. Enter data only below MARKED fields'
	  WRITE(6,*) ' 3. RETURN (blank line) is new event.'
	  WRITE(6,*) ' 4. STOP  stops the program'
          write(6,*) ' 5. To correct a line, use _ for blank'
          write(6,*) ' 6. Auto repeat of STAT SP HHMM'
	  WRITE(6,*)
C
C   OPEN FILE
C
      OPEN(2,FILE='neweve.out',STATUS='new',err=888)
      goto 889
 888  continue
      write(6,*)' The NEWEVE.OUT file exists, do you want to',
     *          '  overwrite(y/n)'
      read(5,'(a)') answer
      if(answer.eq.'y'.or.answer.eq.'Y') then
          open(2,file='neweve.out',status='unknown')
      else
          stop
      endif
 889  continue           
C
C  ENTER HERE FOR NEW EVENT
C
   10 CONTINUE
C
      WRITE(6,*) 'New event: First enter header information....'
      WRITE(6,*)'YEAR MMDD LE' 
      READ(5,'(A)') chr_text                     !JAB(BGS)Mar95.
      call sei upc ( chr_text )                  !JAB(BGS)Mar95. Uppercase.
      if( chr_text(:4) .eq. 'STOP' ) stop        !JAB(BGS)Mar95.
c                                                !JAB(BGS)Mar95.
c    Find start position for text, so that       !JAB(BGS)Mar95.
c    different hardware situations can be        !JAB(BGS)Mar95.
c    accounted for                               !JAB(BGS)Mar95.
c                                                !JAB(BGS)Mar95.
      start = 0                                  !JAB(BGS)Mar95. Initialise.
1111  start = start + 1                          !JAB(BGS)Mar95. Increment.
      if( chr_text .eq. ' ' )then                !JAB(BGS)Mar95. Invalid.
      write(*,*)'Blank entry...try again...'     !JAB(BGS)Mar95.
      goto 10                                    !JAB(BGS)Mar95. Try again.
c                                                !JAB(BGS)Mar95.
      else if(chr_text(start:start) .eq. ' ')then!JAB(BGS)Mar95. Not there yet!.
      goto 1111                                  !JAB(BGS)Mar95. & try again.
c                                                !JAB(BGS)Mar95.
      else                                       !JAB(BGS)Mar95. Found.
      text(2:) = chr_text(start:)                !JAB(BGS)Mar95. & setup.
      end if                                     !JAB(BGS)Mar95.  
c
      TEXT(22:23)=TEXT(12:13)
      TEXT(12:13)='  '
      TEXT(1:1)=' '
C
C   CHECK HEADER
C
      IF(TEXT(22:22).NE.'L'.AND.TEXT(22:22).NE.'R'.AND.TEXT(22:22).
     *NE.'D') THEN
         WRITE(6,*)' YOU  MUST GIVE EVENT TYPE L, R OR D'
         GOTO 10
      ENDIF
      READ(TEXT(2:10),'(I4,1X,2I2)',ERR=27) IYEAR,IMONTH,IDAY
      goto 28
 27   CONTINUE
         WRITE(6,*)' SOMETHING WRONG WITH YEAR AND DATE INFO'
         goto 10
 28   continue
      IF(IYEAR.GT.2100.OR.IYEAR.LT.0.OR.IMONTH.GT.12.OR.IMONTH.
     *LT.1.OR.IDAY.GT.31.OR.IDAY.LT.1) THEN
         WRITE(6,*)' SOMETHING WRONG WITH YEAR AND DATE INFO'
         GOTO 10
      ENDIF
      WRITE(2,'(A)') TEXT
C
C Print headers in file and screen
C
c      WRITE(2,201)
c 201  FORMAT(' YYYY MMDD HHMM  SEC LE LT.XXX LON.XXX DP.XFF     N RMS',
c     *'                        3')	  
      WRITE(2,200)
  200 FORMAT(' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI',
     *' AZIMU VELO AIN AR TRES  W DIS CAZ7'  )
c     *' AZIMU VELO SNR AR TRES  W DIS CAZ7'  )
C
C   ***** NOTE: SOME DIFFERENCE BETWEEN COMPILERS MIGHT OCCUR HERE......
C
      WRITE(6,*)'STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI'
C
C Start of input data
      DO 20 I=1,2000
      read(5,'(A)') chr_text                    !JAB(BGS)Mar95. Get text.
      call sei upc( chr_text )                  !JAB(BGS)Mar95. Make uppercase.
      text(1:75) = ' ' // chr_text(start:)      !JAB(BGS)Mar95. positioned data.
C
C Check for end of data entry
C
	    IF(TEXT(2:5).EQ.'STOP') GOTO 99
C
C  Check for end of event
C
        IF(TEXT(1:30).EQ.'                             ') GOTO 30
C
C  Check if information from previous line should be used
C
C  STATION
C
        IF(TEXT(2:6).EQ.'     ')TEXT(2:6)=OLDTEX(2:6)  
        OLDTEX(2:6)=TEXT(2:6)
C Type of instrument
        IF(TEXT(7:7).EQ.' ') TEXT(7:7)=OLDTEX(7:7)
        OLDTEX(7:7)=TEXT(7:7)
c Component type
	   IF(TEXT(8:8).EQ.' ') TEXT(8:8)=OLDTEX(8:8)
	   OLDTEX(8:8)=TEXT(8:8)
c Quality indicator
C	  IF(TEXT(10:10).EQ.' ') TEXT(10:10)=OLDTEX(10:10)
C	  OLDTEX(10:10)=TEXT(10:10)
c Phase type
C	 IF(TEXT(11:11).EQ.' ') TEXT(11:11)=OLDTEX(11:11)
C	 OLDTEX(11:11)=TEXT(11:11)
c Hours 
        IF(TEXT(19:20).EQ.'  ')TEXT(19:20)=OLDTEX(19:20)
        OLDTEX(19:20)=TEXT(19:20)
c Minutes 
	  IF(TEXT(21:22).EQ.'  ') TEXT(21:22)=OLDTEX(21:22)
	  OLDTEX(21:22)=TEXT(21:22)
C Check input data
        CALL CHECK(start,TEXT,ERROR)      !JAB(BGS)Mar95.
        WRITE(2,'(A)')TEXT(1:75)
        oldtex=text
   20 CONTINUE   
C
   30 CONTINUE
c Leave blank between events
      WRITE(2,330)
  330 FORMAT(' ')
      GOTO 10
 99   CONTINUE
      WRITE(2,340)
 340  FORMAT(' ')
      STOP
      END

C            SUBROUTINE FOR VALIDATING INPUT DATA
C            ------------------------------------   
             SUBROUTINE CHECK(start,TEXT,ERROR)    !JAB(BGS)Mar95.
C
             CHARACTER*80 TEXT,NEWTEX
             INTEGER ERROR
             integer start                         !JAB(BGS)Mar95.
             character    chr_text *(80)           !JAB(BGS)Mar95.
C
C  Input:   text: input line to chech,if something is wrong
c                 a new line is asked
c  Output:  text:the corrected line
c                 ERRORS: NO ERROR=0
C                         ERRORS=1
C
          ERROR=0
C
C  Start of checking loop
   25 CONTINUE
c   
c Check onset phase
C
       IF(TEXT(10:10).NE.'I'.AND.TEXT(10:10).NE.'E'.AND.TEXT(10:10).
     *   NE.' ') THEN
         WRITE(6,120) 
  120    FORMAT(' ***ONSET NOT VALID ***')
         ERROR=1
       ENDIF
C Check phase
c change lot 03/2006
       IF((TEXT(11:11).NE.'P'.AND.TEXT(11:11).NE.'S'.AND.TEXT(11:11)
     *  .NE.' '.and.text(11:11).ne.'L'.and.text(11:11).ne.'R'.and.
     *  text(11:11).ne.'A')
     *  .OR.TEXT(10:11).EQ.'  ') THEN
        WRITE(6,150) TEXT(11:11)
  150   FORMAT(' ***PHASE INPUT  ' ,A1,' NOT LEGAL,CORRECT LINE')
        ERROR=1
      ENDIF
C
C Check whether time is   within the range
c
      READ(TEXT(19:20),'(I2)',ERR=123) IHOUR
      goto 1124
 123  CONTINUE
	  WRITE(6,260) TEXT(19:20)
      errror=1
 1124 continue

      IF(IHOUR.GT.24.OR.IHOUR.LT.0) THEN
	    WRITE(6,260) TEXT(19:20)
  260   FORMAT(' ***hours  ',A2,' out of range,correct time **')
        ERROR=1
      ENDIF
C
c Check for minutes
C
      READ(TEXT(21:22),'(I2)',ERR=124) MIN
      goto 1224
 124  continue
      write(6,270)
      error=1
 1224 continue
      IF(MIN.LT.0.OR.MIN.GT.60) THEN
	    WRITE(6,270)
  270   FORMAT(' ***time in minutes is out of range ***')
        ERROR=1
	  ENDIF	 			  		
C Check if any errors
      IF(ERROR.EQ.0) GOTO 40
C If errors are present
      WRITE(6,*)'Correct data---------'
      WRITE(6,*)'STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI'
      write(*,*)text(2:)                                  !JAB(BGS)Mar95.
C
C  Read corrected text line, compare with old text and replace
c  chars in old text with chars in new text if different,or blank
c  if underscore
c
      read(5,'(A)') chr_text                        !JAB(BGS)Mar95.
      call sei upc( chr_text )                      !JAB(BGS)Mar95.
      if( chr_text(:4) .eq. 'STOP' ) stop           !JAB(BGS)Mar95.
      newtex = ' ' // chr_text(start:)              !JAB(BGS)Mar95.
C                                                   !JAB(BGS)Mar95.
      DO 30 I=2,80
        IF(NEWTEX(I:I).EQ.' ') GOTO 30
        if(newtex(i:i).eq.'_') then
           text(i:i)=' '
           goto 30
        endif
        IF(NEWTEX(I:I).NE.TEXT(I:I)) TEXT(I:I)=NEWTEX(I:I)
   30 CONTINUE
      ERROR=0
      GOTO 25
C End of loop
  40  CONTINUE
      RETURN
      END                       
