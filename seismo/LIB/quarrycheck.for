      subroutine quarry_check(elat,elon,list,nqsel)
c
c routine to calculate distance between epicentre and quarry locations given in quarry.dat
c Lars Ottemoller, August 2011
c

      implicit none
      real elat,elon             ! event location
      real max                   ! max distance between epicentre and quarry to be listed
      integer qmax               ! max number of quarrier
      parameter (qmax=10000)
      real qlat(qmax),qlon(qmax) ! quarry coordinates
      character*60 qtext(qmax)   ! quarry names
      character*60 text          ! used to read
      real x,y                   ! used to read
      integer nq,nqsel           ! number of quarries
      character*80 seisan_top    ! seisan top directory
      character*255 filename     ! name of quarry file
      integer i,code             ! counter
      integer in                 ! input unit
      character*(*) list(*)      ! output list
      character*(100) temp(qmax)
      real degtorad              ! unit conversion
      parameter (degtorad=0.0174533)
      real dist(qmax),distdeg,az ! distance and azimuth
      integer ind(qmax)          ! index for sorting
      include 'libsei.inc' 
      include 'seidim.inc'
c
c read quarry file
c
      nq=0
      nqsel=0
      max=0.
      filename='quarry.dat'
      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   filename )       ! For this filename.

      if(code.ne.e_ok$) then
        write(*,*) ' definition file does not exist: ' // filename
        stop
      endif
      read(in,'(f10.0)') max

10    continue
      read(in,'(f7.3,1x,f8.3,a)',end=99) y,x,text 
      nq=nq+1
      qlat(nq)=y
      qlon(nq)=x
      qtext(nq)=text 

      goto 10
99    continue
      call sei close (close$,in,code)
      

c
c check for quarries wthin max distance and write to screen
c
      do i=1,nq
        call delaz(elat*degtorad,elon*degtorad,dist(i),distdeg,az,
     &     qlat(i)*degtorad,qlon(i)*degtorad)
c        write(*,*) dist,az,qlat(i),qlon(i),qtext(i)
        if (dist(i).le.max) then
          nqsel=nqsel+1
        write(temp(nqsel),'(f6.1,1x,f7.3,1x,f8.3,a60)') dist(i),qlat(i),
     &      qlon(i),qtext(i)
        endif
      enddo
c
c sort list
c
      call quarryqsort(ind,dist,nqsel)
      do i=1,nqsel
        list(i)=temp(ind(i))
      enddo

      return
      end

      subroutine quarryqsort(IND,DIS,NQ)
      implicit none
      integer IND(*),j,i,k,nq
      real DIS(*),rmax,rmin
      RMAX=-1
      DO 30 I=1,NQ
      RMIN=99999.9
      DO 20 J=1,NQ
      IF(DIS(J).LE.RMIN.AND.DIS(J).GE.RMAX) THEN
         DO 10 K=1,I-1
         IF(J.EQ.IND(K)) GOTO 20
   10    CONTINUE
         RMIN=DIS(J)
         IND(I)=J
      ENDIF
   20 CONTINUE
      RMAX=DIS(IND(I))
   30 CONTINUE
      RETURN
      END


c############################################################################ C
C---------------------------------------------------------------------------
C
      SUBROUTINE QUARRYCHECK(evfile)
C
C     This program has been adapted from the quarry blast 
C     discrimination program ** QUARRYCHECK ** written by 
C     R.M.W. Musson 5 Aug 1987, which ran on EMAS.
C     This revised version runs on the VAX and must be linked with
C     the NERCLIB library.
C
C     Amendments made by Sheila Alexander, September 1991
C
C     NOLONG NEED TO LINK WITH NERCLIB - NOV 1991 - BY ENRU LIU
C     MODIFIED FOR NEW PHASE DATA FORMAT - DEC 1991 - BY E LIU
C   
C     Modified for new database of quarries J Exton 30.10.98
C
C     Ported to UNIX with a few changes to file opening statements,
C     and modifications for the newer S file
C     format (line type '8' to '83' and compressions given as 'C' rather than
C     'U'
C         J Exton 5.8.2002




C     Declare variables
      CHARACTER *(*) evfile

      CHARACTER ANS*1, FMR*1, RTN*1, NLIND*2, QLIST*1, UPPER*1,
     +          PHASNAM*100, QUARRYFILE*80, QNAME(350)*50, QLOC(350)*50,
     +          QTYPE(350)*38, QOP(350)*50, QTEL(350)*20, 
     +          NOS(5)*5, MSAGES(5)*40, TREC*80, PDUM*3, PDOUT*1
C
      INTEGER KEYBRD, SCREEN, QDFIL, PHASFIL, EVFIL, PDFIL,
     +        YR, DAY, HRS, EVOUT, PERR, ESYM, QSYM
C
      REAL MAG1, KME, KMN
C
      DIMENSION QE(350),QN(350),DIS(350),IQFL(5),IND(350)
C
      DATA KEYBRD, SCREEN, QDFIL, PHASFIL, EVFIL, PDFIL 
     +/ 5, 6, 7, 9, 10, 11/
C
      DATA NOS/'  (i)',' (ii)','(iii)',' (iv)','  (v)'/

      DATA MSAGES/' Event is too deep to be a quarry blast',
     +' Event is too big to be a quarry blast',
     +' Wrong time of day for a quarry blast',
     +' There are no quarries near this event',
     +' There is at least one dilation'/
C
      PERR = 10
      ESYM = 134
      QSYM = 125
      EVOUT = 1
      

      WRITE (6,*) 'Working with event: ', evfile
C
C     Write program header to screen

      WRITE(SCREEN,10)
   10 FORMAT(26X,'***************'/26X,':             :'
     +/26X,': QUARRYCHECK :'/26X,':             :'/
     +26X,'***************'//' UK Quarry and mine database'
     +,' checking programme for dubious seismic events'/)
C
   20 CONTINUE
C
C     Open phase file and the quarry database file QUARRY.DAT,& skip 
C     headings
C     Newquarry.dat put in $ANALYST_BGS_DIR on UNIX  JE 7.8.02


c      CALL getenv2('ANALYST_BGS_DIR', QUARRYFILE)
      CALL getenv('ANALYST_BGS_DIR', QUARRYFILE) ! lot
      QUARRYFILE = QUARRYFILE(1:LNBLNK(QUARRYFILE))//'/newquarry.dat'


c      OPEN(UNIT=QDFIL,FILE=QUARRYFILE,READONLY,STATUS='OLD')
      OPEN(UNIT=QDFIL,FILE=QUARRYFILE,STATUS='OLD')
      read(qdfil,*)
      OPEN(UNIT=PHASFIL,FILE=evfile,STATUS='OLD',ERR=470)
C
C     Prompt for estimated radius of uncertainty in eq location
C
   60 WRITE(SCREEN,70)PERR
   70 FORMAT(//' Enter estimated radius of uncertainty in earthquake',
     +' location (',I3,') km > ',$)
      READ (KEYBRD,100)PDUM
      IF (PDUM(1:1).NE.' ') READ (PDUM,*,ERR=60)PERR
C
C     Initialise counters for first motion statistics
C
      NOCOMS=0
      NODILS=0
C
C     Begin reading records from seismic activity readings file
C     Read whole record in as a string then pick out the required bits
C
   90 READ  (PHASFIL,100,END=470)TREC
  100 FORMAT(A)
      IF (TREC(1:10).EQ.'          ') GOTO 90
      READ (TREC(2:20),110) YR, MON, DAY, HRS, MINS, SECS
  110 FORMAT(I4,2(1X,2I2),1X,F4.0)
      READ (TREC(39:43),120) DEPTH
  120 FORMAT(F5.0)
      READ(TREC(57:59),130) MAG1
  130 FORMAT(F3.0)
C
C     Check column 80 for next line type indicator
C     Read in Earthquake location and first motion identifier 
C
  140 CONTINUE
      READ (TREC(79:80),100)NLIND
      IF (NLIND.EQ.'83') THEN
        READ (TREC(2:17),*)KME,KMN
C	write(6,*) kme, kmn
        READ (PHASFIL,100,END=160)TREC
        GOTO 140
      ELSEIF (NLIND(2:2).EQ.'4' .OR. NLIND(2:2).EQ.' ') THEN
        READ (TREC(17:17),100)FMR
        IF (FMR.EQ.'C') NOCOMS=NOCOMS+1
        IF (FMR.EQ.'D') NODILS=NODILS+1
        READ (PHASFIL,100,END=160)TREC
        GOTO 140
      ELSE
        READ (PHASFIL,100,END=160)TREC
        GOTO 140
      ENDIF
      
C
C     IQFL is an array of indicators which are set to show which
C     earthquake diagnostics are present
C
C  160       write(6,*) KME, KMN, YR, MON, DAY, HRS, MINS, SECS

  
  160 IF(NODILS.GT.0) THEN 
         IQFL(5)=0
      ELSE
         IQFL(5)=1
      ENDIF
C
C     Now check out those events obviously not quarry blasts based on
C     depth, magnitude and time
C
      IF(DEPTH.GT.5) THEN
         IQFL(1)=0
      ELSE
         IQFL(1)=1
      ENDIF
C
      IF(MAG1.GT.2.7) THEN
         IQFL(2)=0
      ELSE
         IQFL(2)=1
      ENDIF
C
      IF(HRS.LT.07 .OR. HRS.GT.18) THEN
         IQFL(3)=0
      ELSE
         IQFL(3)=1
      ENDIF
C
C     Cycle through the quarry data file looking for close ones
C
      NQ=1
      RMINDI=999.0
      DO 180 I=1,5000
      READ(QDFIL,170,END=200) QNAME(NQ),QLOC(NQ),QE(NQ),QN(NQ), 
     + QTYPE(NQ), QOP(NQ), QTEL(NQ)        
  170 FORMAT(A40,A26,T80,F6.0,3x,f7.0,T98,A20,A41,A15)
      QE(NQ)=QE(NQ)/1000
      QN(NQ)=QN(NQ)/1000

      DIS(NQ)=QDIS(KME,KMN,QE(NQ),QN(NQ))
      IF(DIS(NQ).LT.RMINDI) RMINDI=DIS(NQ)
      IF(DIS(NQ).LE.PERR) NQ=NQ+1
  180 CONTINUE
C
  200 IF(NQ.EQ.1) THEN
         IQFL(4)=0
      ELSE
         IQFL(4)=1
      ENDIF
C
C     Calculate the number of signs that this may be an earthquake
C
      NOEVTS=0
      DO 210 I=1,5
        IF (IQFL(I).EQ.0) NOEVTS=NOEVTS+1
  210 CONTINUE
      NOSFL=0
C
C     Output results. If it is a quarry inform user then move to 
C     section to list quarries within the vicinity.
C     Otherwise, list indications of earthquake and polarities
C     for the event.
C
      IF (NOEVTS.EQ.0) THEN
        WRITE (SCREEN,220)
  220   FORMAT(/' All evidence is consistent with this being a',
     +  ' quarry blast.')
C        GOTO 255
      ELSE
        WRITE(SCREEN,230)NOEVTS
  230   FORMAT(/' The',I2,' indications that this may be an',
     +  ' earthquake are:')
        DO 250 I=1,5
          IF(IQFL(I).EQ.0) THEN
            NOSFL=NOSFL+1
            WRITE (SCREEN,240)NOS(NOSFL),MSAGES(I)
  240       FORMAT(1X,A5,1X,A40)
          ENDIF
  250   CONTINUE
      ENDIF

C
  255   WRITE (SCREEN,260) NOCOMS,NODILS,INT(RMINDI)
  260   FORMAT(/' The polarities for this event are:',
     +  /1X,I2,' Compressions'/1X,I2,' Dilations',
     +  //' The nearest quarry is',I4,' km away')
C
C     List the quarries near the event
C
      IF(NQ.GT.1) THEN
         NQ=NQ-1
  270    WRITE (SCREEN,280)NQ
  280    FORMAT(/' The number of quarries near this event is ',I3,
     +   //' Would you like a list of them? (Y/N) : ',$)
         READ (KEYBRD,100)QLIST
         QLIST = UPPER(QLIST)
         IF (QLIST.EQ.'N') GOTO 400
         IF (QLIST.NE.'Y') GOTO 270
C
C     Check whether output required to screen, file or both
C
  290    WRITE (SCREEN,300)
  300    FORMAT (//' Would you like results output to :- ',
     +   /' 1) Screen ',
     +   /' 2) File ',
     +   /' 3) Both ',
     +   /' > ',$)
         READ (KEYBRD,310)EVOUT
  310    FORMAT(I1)
         IF (EVOUT.LT.1 .OR. EVOUT.GT.3) GOTO 290
         IF (EVOUT.EQ.2 .OR. EVOUT.EQ.3) THEN
           OPEN(UNIT=EVFIL,FILE='EVENTS.DAT',STATUS='UNKNOWN')
         ENDIF
C
C     Check if user wishes to ouput a point data file for plotting
C     at a later stage. If yes, write out the earthquake location
C     and symbol number.
C
  315    WRITE (SCREEN,320)
  320    FORMAT(/' Do you wish to ouput file with point data for',
     +   ' plotting? (Y/N) :',$)
         READ (KEYBRD,100)PDOUT
         PDOUT = UPPER(PDOUT)
         IF (PDOUT.EQ.'N') GOTO 325
         IF (PDOUT.NE.'Y') GOTO 315
         OPEN(UNIT=PDFIL,FILE='EQPOINT.DAT',STATUS='UNKNOWN')
         WRITE(PDFIL,355)KME*1000,KMN*1000,ESYM
C
C     Sort out quarry data
C
  325    CALL QSORT(IND,DIS,NQ)
         IF (EVOUT.NE.2) WRITE (SCREEN,330)
         WRITE (EVFIL,330)
  330    FORMAT(/' They are as follows: ')
C
C     Write out quarry details. If ouput is to screen write 18 lines
C     then wait for a carriage return. If point data option was
C     selected write quarry location and symbol to point data file
C
         NOLINES = 0
         DO 370 I=1,NQ
           IF (EVOUT.NE.2) THEN
             WRITE (SCREEN,'(a,t41,a,F5.1,a)')
     +                                 ' Pit name: '//QNAME(IND(I)), 
     +                                 'Distance: ',DIS(ind(I)),'km'
             write (screen,'(a,2(1X,F7.2))')' Grid Ref: ',QE(IND(I)), 
     +                                                    QN(IND(I))
             WRITE(SCREEN,'(a,t41,a30)')' Location:  '//QLOC(IND(I)),
     +                                  'Product:   '//QTYPE(IND(I))
             WRITE (SCREEN,'(a,t41,a)')' Operator:  '//QOP(IND(I)),
     +                                  'Telephone: '//QTEL(IND(I))
             write(screen,*)

             NOLINES = NOLINES+6
           ENDIF
C
             WRITE (EVFIL,'(a,t41,a,F5.1,a)')
     +                                 ' Pit name: '//QNAME(IND(I)), 
     +                                 'Distance: ',DIS(ind(I)),'km'
             write (EVFIL,'(a,2(1X,F7.2))')' Grid Ref: ',QE(IND(I)), 
     +                                                    QN(IND(I))
             WRITE(EVFIL,'(a,t41,a30)')' Location:  '//QLOC(IND(I)),
     +                                  'Product:   '//QTYPE(IND(I))
             WRITE (EVFIL,'(a,t41,a)')' Operator:  '//QOP(IND(I)),
     +                                  'Telephone: '//QTEL(IND(I))
             write(EVFIL,*)
C           WRITE (EVFIL,350)QLOC(IND(I)),QTYPE(IND(I)),QOP(IND(I)),
C     +                         QTEL(IND(I))
C
           IF (PDOUT.EQ.'Y') THEN
             WRITE(PDFIL,355)QE(IND(I))*1000,QN(IND(I))*1000,QSYM
           ENDIF
C
  340      FORMAT(/1X,A27,' Grid ref.',2(1X,F7.2),' at a distance of ',
     +     F5.1,' km')
  350      FORMAT(A,A,A,A)
  355      FORMAT(2F10.1,3X,I3)
C
           IF (EVOUT.NE.2) THEN
C             IF (JMOD(NOLINES,18).EQ.0) THEN    ! changed lot 07/04/2003
             IF (INT(NOLINES/18.).EQ.FLOAT(NOLINES)/18.) THEN
               WRITE (SCREEN,360)
  360          FORMAT(/' Press <CR> for more ')
               READ (KEYBRD,100)RTN
             ENDIF
           ENDIF
  370    CONTINUE
      ENDIF
C
C     Close files and inform user of output files if required
C
  400 CONTINUE
      IF (EVOUT.EQ.1) THEN
        CLOSE (UNIT=EVFIL,STATUS='DELETE')
      ELSE
        CLOSE (UNIT=EVFIL)
        WRITE(SCREEN,420)
  420   FORMAT(/' List of quarries written to EVENTS.DAT ')
      ENDIF
C
      IF (PDOUT.EQ.'Y') THEN
        CLOSE(UNIT=PDFIL)
        WRITE(SCREEN,430)
  430   FORMAT(/' Point data written to EQPOINT.DAT ')
      ENDIF
      CLOSE (UNIT=PHASFIL)
      CLOSE (UNIT=QDFIL)
      GOTO 500
C
C     Cope with empty readings file
C
  470 WRITE (SCREEN,480)PHASNAM
  480 FORMAT(//' The readings file ',A,' is empty ',
     +/' Enter R to reenter or Q to quit > ',$)
      READ (KEYBRD,100)ANS
      ANS = UPPER(ANS)
      IF (ANS.EQ.'R') GOTO 20
      IF (ANS.EQ.'Q') GOTO 500
      GOTO 470
C
C     Inform user that program is finishing
C
  500 WRITE (SCREEN,510)
  510 FORMAT(/' QUARRYCHECK program finishing at user''s request ')
      END
C
C------------------------------------------------
C
      FUNCTION QDIS(A,B,C,D)
C
C     Pythagorean distance calculation
C
      QDIS=SQRT(((A-C)**2)+((B-D)**2))
      RETURN
      END
C
      SUBROUTINE QSORT(IND,DIS,NQ)
      DIMENSION IND(NQ),DIS(NQ)
      RMAX=-1
      DO 30 I=1,NQ
      RMIN=99999.9
      DO 20 J=1,NQ
      IF(DIS(J).LE.RMIN.AND.DIS(J).GE.RMAX) THEN
         DO 10 K=1,I-1
         IF(J.EQ.IND(K)) GOTO 20
   10    CONTINUE
         RMIN=DIS(J)
         IND(I)=J
      ENDIF
   20 CONTINUE
      RMAX=DIS(IND(I))
   30 CONTINUE
      RETURN
      END
C--------------------------------------------------------------------------
      FUNCTION UPPER (LINE)
      CHARACTER *(*) UPPER,LINE
      N=LEN(LINE)
      DO 100 I=1,N
      IC=ICHAR(LINE(I:I))
      IF (IC.GE.97.AND.IC.LE.122) IC=IC-32
 100  UPPER(I:I)=CHAR(IC)
      RETURN
      END
C--------------------------------------------------------------------------
C-------------------------------------------------------------------


C This subroutine is a wrapper for the non-standard SUN getenv function	
c lot, not sure if needed
	
	subroutine getenv2 (ename, evalue)
	
	character*(*) ename, evalue
	
	call getenv (ename, evalue)
	
	return
	end
	
