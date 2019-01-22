c  
c      program fk

C==========================================================================
C    Program fk reads data from file 'waveform.out'
C    asks values of parameters from keybord and feeds everyfing into
C    'slbbkfk' to calculate wide-band slowness estimate
C    Then subrouatine 'conte' is called and relative power is drawn in 
C    slowness space
C--------------------------------------------------------------------------
C    For input and output parameters see in text of 'slbbfk.for'
C--------------------------------------------------------------------------
C    writen by Andrius Pacesa
C    May , 1999
C    Bergen University
C    Institute of Solid Earth Physics
C-------------------------------------------------------------------------
C    the first chanel coordinates are reference coordinates
C     
c
c changes
c
c 20 mai 99   : Postscript output, include conte, pc test
c 28 mai 99   : Plotting of date, time and stations used added to graph window
c 28 sep 99 lo: check for waveform.out
c 05 oct    jh: stop if a missing station
c 02 may 00 lo: fixed for new waveform structure
c 11 sep 00 lo: use character to specify station file
c 19 oct 00 jh: only ask for station file char if station not found
c               more accuracy in output
c               more error messages
c 20 jun 05 jh: plt to eps
c 28 dec 10 jh: gfortran on pc, remove winplot, implicit none, unit check,
c               pc check, remove hctype, remove spcial stop for pc, remove
c               call to computer_type
c 28 jan 11 jh: more gfortran fixes. in several calls, the character  arrays
c               to the basic plotting function had wrong size.
c 22 feb 11 jh: window size from color.def
c  8 jun 11 pv: add flag -nowindow to bypass contour window, and -help flag
c  8 feb 13 jh: add call wav_mem_init
c 17 feb 14 jh: read seisan.def, fixed many dimensions 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      implicit none 
      include 'mulplt.inc'
      include 'seiplot.inc'     !     seisan graphics

c     variables for fk

      INTEGER       n        , i       , k
      INTEGER       cftpts(100), cftptr(100)
      INTEGER       NFFT
      INTEGER       YEAR,  MONTH, DAY,  HOUR
      INTEGER       MINU
      REAL          SECO, TMWIN
      REAL          pi       , ctrlat  , ctrlong 
      REAL          stlat(40), stlong(40), stelev(40)
      REAL          smprate
      REAL          array(99,99)     !  used for contouring
      REAL          xm, ym      ! requre subrautine xscursr pos of mouse
      REAL          xdim, ydim  ! scaling for subautine 'contour'
      CHARACTER     LEVNUM*1    ! indicator to plott level numbers
      CHARACTER     TEXT1*10    ! to read from key bord

c     variables for subroutine SLBBFK

      COMPLEX*8     CLXBUF(10000)
cxx
      REAL*4        CHNDAT(200000), DELTA(40), START(40)
      REAL*4        STOP(40)  , FRQLOW
      REAL*4        FRQHIG   , SLWMAX   , ALPHA
      REAL*4        BETA     , THETA
      REAL*4        WRKBUF(20000), APPVEL     , AZIMUT
      REAL*4        SXMAX    , SYMAX
      REAL*4        XCOORD(40), YCOORD(40), ZCOORD(40)
      REAL*4        OUTLOW   , OUTHIG
      REAL*4        POWER    , ABSPOW
      REAL*4        SLX(10000)      , SLY(10000)
cxx
      INTEGER*4     CHNPTR(40), CHNPTS(40), NCHAN
      INTEGER*4     GRDPTS   , WRKDIM
      INTEGER*4     CLXDIM   , IFKQ     , TYPCTL
cxx
      CHARACTER     IDENT(40)*10 , INSTR*8    , ERRMSG*70
      CHARACTER     TEXT*10
      character*1 modelc
      logical exist
      logical nowindow
      include 'version.inc'

      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments
      nowindow=.FALSE.


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call get_seisan_def
      pi=4.*ATAN(1.)

      TYPCTL=0          ! Verbosity level
      modelc='0'        ! use STATION0.HYP as default


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      call get_arguments(narg,arg)
      if(narg.ge.1) then
      do i=1,narg
      if( arg(i)(1:9) .eq. '-nowindow' ) nowindow=.TRUE.
      if( arg(i)(1:5).eq.'-help'.or.arg(i)(1:3).eq.'-h') then
        write(6,*)' '
        write(6,*)' The FK program determine azimuth and '
        write(6,*)' apparent velocity from array data.'
        write(6,*)
     +' The output is given in fk.out and fk.eps.'
        write(6,*)
     +' Data must be in a file named waveform.out'
        write(6,*)' '
        write(6,*)' Usage: fk [options]'
        write(6,*)'        lower frequency  '
        write(6,*)'        higher frequency  '
        write(6,*)'        max slowness '
        write(6,*)'        Number of grid points '
        write(6,*)'        plot level numbers, [Y]/N '
        write(6,*)' '
        write(6,*)' ## Options ##'
        write(6,'(a17,a15)')'    -help        '
     +,'Print this list'
        write(6,'(a17,a13)')'    -h           '
     +,'Same as -help'
        write(6,'(a17,a38)')'    -nowindow    '
     +,'Will close the window with the fk plot'
        write(6,'(a17,a15)')'    -version     '
     +,'Seisan version '
        write(6,*)' '
        write(6,*)' Examples:'
        write(6,*)'       1)  fk'
        write(6,*)'           enter values'
        write(6,666)
     +'        2)  echo ','1\\n5\\n0.25\\n99\\nN\\n',
     +' | fk -nowindow'
        write(6,*)' '
        goto 100
      endif

  666 FORMAT(a17,'"',a20,'"',a15,1x)

      enddo
      endif

c----
c      read data from 'waveform.out'
c----

111   continue
      wav_filename(1)='waveform.out'
c
c   check if file is there
c
      inquire(file='waveform.out',exist=exist)
      if(.not.exist) then
         write(6,*)' File waveform.out not present, file might not'
         write(6,*)' have been generated properly by mulplt'
         stop
      endif
c
c  read header 
c
      call wav_init
      call wav_mem_init
      call read_wav_header(1)
      if(wav_error_message.ne.' ') then
         write(6,'(1x,a)') wav_error_message
         stop  
      endif

      nchan=wav_nchan

c
c set date and time
c
      year=wav_year(wav_first)
      month=wav_month(wav_first)
      day=wav_day(wav_first)
      hour=wav_hour(wav_first)
      minu=wav_min(wav_first)
      seco=wav_sec(wav_first)

c---
c   read all channels
c---

       CHNPTR(1)=1           ! pointer of the first element of data array

 222   continue              ! from below, read again with different stat file

       DO n=1,nchan

        call wav_read_channel(n)

        if (n.eq.1) then
           tmwin=wav_duration(n)
           write(*,*) year,month,day,hour,minu,seco,tmwin
        endif
        IDENT(n) = wav_stat(n)(1:5) // wav_comp(n)(1:4)

        CHNPTS(n)=wav_nsamp(n)
        DELTA(n)=1/ANINT(wav_rate(n))
        START(n)=0
        STOP(n)=wav_duration(n)
        CHNPTR (n+1)=CHNPTR(n)+CHNPTS(n)

        IF (DELTA (1).NE.DELTA (n)) THEN    ! Checks if samlpe rate is the same
          WRITE (6,*) 'CHANEL ',IDENT(n),' HAS A DIFFERENT SAMPLE RATE'
        ENDIF

        DO i=CHNPTR(n), CHNPTR(n)+CHNPTS(n)-1
          CHNDAT(i)=signal1(i-CHNPTR(n)+1)
        ENDDO
c----
c    looking for station coordinates
c----
        call stat_loc(wav_stat(n),modelc,stlat(n),stlong(n),
     *  stelev(n))
        if(stlat(n).eq.0.0.and.stlong(n).eq.0.0) then
           write(6,'(1x,a,a)') 
     *   ' Station not found in STATION0.HYP file ',wav_stat(n)
         write(6,*)' Do you want to try another station file, if so'
         write(6,*) ' enter character in station file (STATION?.HYP) '
         write(6,*) ' else return to stop ********************'
         read(5,'(a1)') modelc
         if (modelc.eq.'') then
           stop
         else
           goto  222          ! read again from above
         endif
        endif
        ZCOORD (n)=stelev(n)

        XCOORD(n)=2*pi*6371*1000/360*COS(stlat(1)*2*pi/360)
     +  *(stlong(n)-stlong(1))
        YCOORD(n)=2*pi*6371*1000/360*(stlat(n)-stlat(1))

        IF (TYPCTL.GT.2) THEN
          WRITE(6,*)
          write(6,*) 'NUMBER OF CHANEL  ', n
c          WRITE(6,*) 'HEADER OF CHANEL  '
c          WRITE(6,*)  chead(1:75)
          WRITE(6,*) 'COORDINATES OF CHANEL (lat, long, elev.)'
          WRITE(6,*) stlat(n), stlong(n), stelev(n)
          WRITE(6,*) 'RELETIVE CHN. COORDINATES (m)',XCOORD(n),
     +                YCOORD(n)
          write(6,*) 'CHNPTR ', CHNPTR(n), ' CHNPTS ', CHNPTS(n)
          write(6,*) 'DELTA ', DELTA (n), ' START ', START (n),
     +               ' STOP ', STOP (n)
c         write(6,*) 'SIG', (signal(k),k=1,5)
c         write(6,*) 'DAT', (CHNDAT(k),k=CHNPTR(n),CHNPTR(n)+4)
       ENDIF

       ENDDO

       CHNPTR(n)=0       ! to remove pointer of unexist chanel

C----
C   Assigning coordinates of the first chanel as a reference coordinates
C----

      ctrlat = stlat(1)
      ctrlong = stlong(1)

      INSTR='VERTICAL'

c      close(22)

C----
C   Enter parameters from keybord
C----

      WRITE(6,*) 'Enter lower frequency,<ENTER> for default value 1 Hz'
      READ (5,'(a)') TEXT
      IF ( TEXT.EQ.' ') THEN
        FRQLOW=1.0
        GOTO 20
      ELSE
        READ ( TEXT,*,ERR=40) FRQLOW
        GOTO 20
      ENDIF    

40    WRITE (6,*) 'Bad entry, lower frequency=1 Hz'
      FRQLOW=1.0


20    WRITE(6,*)'Enter higher frequency,<ENTER> for default value 5 Hz'
      READ (5,'(a)') TEXT
      IF ( TEXT.EQ.' ') THEN
        FRQHIG=5.0
        GOTO 35
      ELSE
        READ ( TEXT,*,ERR=30) FRQHIG
        GOTO 35
      ENDIF

30    WRITE (6,*) 'Bad entry, higher frequency=5 Hz'
      FRQHIG=5.0

35    CONTINUE

      WRITE(6,*) 'Enter max slownes, <ENTER> for default value 0.4 s/km'
      READ (5,'(a)') TEXT
      IF ( TEXT.EQ.' ') THEN
        SLWMAX=0.4
        GOTO 60
      ELSE
        READ ( TEXT,*,ERR=65) SLWMAX
        GOTO 60
      ENDIF
65    WRITE (6,*) 'Bad entry, max slownes=0.4 s/km)'
      SLWMAX=0.4

60    CONTINUE

      WRITE(6,*) 'Enter No of grid points, <ENTER> for default value 51'
      READ (5,'(a)') TEXT
      IF ( TEXT.EQ.' ') THEN
        GRDPTS=51
        GOTO 70
      ELSE
        READ ( TEXT,*,ERR=75) GRDPTS
        GOTO 70
      ENDIF
75    WRITE (6,*) 'Bad entry, No of grid points=51'
      GRDPTS=51

70    CONTINUE

      WRITE(6,*) 'Do you want to plot level numbers, N/Y, <ENTER>=Y'
      READ (5,'(a)') TEXT
      IF ( TEXT.EQ.'n'.OR.TEXT.EQ.'N') THEN
        LEVNUM='N'
        GOTO 80
      ENDIF
      LEVNUM='Y'
     
80    CONTINUE

      THETA=90.0

      ALPHA=10

      BETA=10

C     WRITE(6,*) FRQLOW, FRQHIG, SLWMAX, GRDPTS

c     Calculation of CLXDIM
c
c     NFFT comes from SLFOUT 
c     NFFT have to be larger than INTPTS(I), number of points 
c     of physical sequence for each chanel, and NFFT have to
c     be pover of 2.
c     As INTPTS(I) .LE. CHNPTS(I), therefore INTPTS(I)=CHNPTS(I)

      NFFT =  8

      DO i=1, NCHAN
 
2     CONTINUE
         IF ( NFFT .GE. CHNPTS(i) ) THEN
            GO TO    3
         ELSE
            NFFT = NFFT * 2 
            GO TO  2
         ENDIF
3     CONTINUE


         IF ( i .EQ. 1) THEN
            CFTPTR (i) = 1
         ELSE
           cftptr(i) = cftptr( i-1 ) + cftpts( i - 1 )
         ENDIF
         CFTPTS (i) = NFFT / 2 + 1
         CLXDIM=cftptr(i) + cftpts(i)
      END DO

c     Calculation of WRKDIM
c
c     Normaly WRKDIM has to be =GRDPTS**2+GRDPTS/2
c     but in case peases of seismograms are long
c     WRKDIM has to be =NFFT*2, requirement from SLFOUT

      WRKDIM=GRDPTS*GRDPTS+GRDPTS/2

      IF (NFFT*2.GT.(GRDPTS*GRDPTS+GRDPTS/2)) THEN
       WRKDIM=NFFT*2
      END IF


      WRITE(6,*) 'It will take some time ...'

      call SLBBFK ( CHNDAT, CHNPTR, CHNPTS, DELTA , START ,
     +              STOP  , IDENT , INSTR , FRQLOW, FRQHIG,
     +              XCOORD, YCOORD, ZCOORD, SLWMAX, NCHAN ,
     +              ALPHA , BETA  , THETA , GRDPTS, WRKBUF,
     +              WRKDIM, CLXBUF, CLXDIM, APPVEL, AZIMUT,
     +              SXMAX , SYMAX , OUTLOW, OUTHIG, POWER ,
     +              ABSPOW, IFKQ  , ERRMSG, TYPCTL, SLX   ,
     +              SLY)

C   OUTPUT

      WRITE (6, 210) APPVEL, INT(AZIMUT)
210   FORMAT (' APPARENT VELOCITY  ',F5.2,
     +        '   AZIMUTH          ',I3)
      WRITE (6, 220) POWER, ABSPOW
220   FORMAT (' NORM. POWER MAX    ',F5.2, 
     +        '   POWER MAX IN dB  ',F5.2)  
      WRITE (6, 230)  SXMAX,  SYMAX
230   FORMAT (' MAX X SLOWNESS     ',F5.2,
     +        '   MAX Y SLOWNESS   ',F5.2)
      WRITE (6, 240) OUTLOW, OUTHIG
240   FORMAT (' LOW FREQUENCY      ',F5.2,
     +        '   HIGH FREQUENCY   ',F5.2)
      WRITE (6, *) 'QUALITY (1=best, 4=worst)', IFKQ
      IF (ERRMSG.NE.' ') THEN
        WRITE (6, *) 'ERROR MESSAGE ', ERRMSG
      ENDIF
c      WRITE (6, *) 'IRC ',    IRC


C----
CAP   Draw contours
C----
      xdim=1.3     ! scaling for 'contour'
      ydim=1.3     ! see in 'contour'

C                  Transfer data from WRKBUF to ARRAY used by Contor
      n=1
      DO i=1, GRDPTS
        DO k=1, GRDPTS
         ARRAY(i,k)= WRKBUF(n)
         n=n+1 
        ENDDO
      ENDDO


c----
c  set defaults for output on screen and one hardcopy file
c----
             open(65,file='fk.eps',status='unknown')
             plotunit=65
             plotoption=1
             wsize=60

             call get_window_size

             if(size_fk.gt.0) wsize=size_fk ! from color.def


           call open_display

c----
c set some postscipt scalings
c----
           write(65,*) ' 1.0 0.55 scale'

      

      call CONTOR (GRDPTS , GRDPTS , ARRAY  , SLWMAX  ,
     +             SXMAX  , SYMAX  , AZIMUT , APPVEL ,
     +             POWER, LEVNUM, OUTLOW, OUTHIG,
     +             YEAR, MONTH, DAY, HOUR, MINU, SECO,
     +             NCHAN, IDENT, xdim, ydim, TMWIN)


c----
c   close postscript
c----

           call close_post

c----
c   close output plot file
c----
           close(65)

C---
C     STARTS WRITING OUTPUT FILE
C---
      open(2, file='fk.out',status='unknown')

      WRITE(2,250) year,month,day,hour,minu,seco,tmwin
250   FORMAT(1X,'DATE, TIME AND WINDOW LENGTH',4X,
     *i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3,1x,f8.3)

      WRITE (2, 210) APPVEL, INT(AZIMUT)
      WRITE (2, 220) POWER, ABSPOW
      WRITE (2, 230)  SXMAX,  SYMAX
      WRITE (2, 240) OUTLOW, OUTHIG
      WRITE (2, *) 'QUALITY (1=best, 4=worst)', IFKQ
      IF (ERRMSG.NE.' ') THEN
        WRITE (2, *) 'ERROR MESSAGE ', ERRMSG
      ENDIF


      if(nowindow) then
        i=113
      else
112   call xscursr(i,xm,ym)
      endif

c pv      if(nowindow) goto 100            

      IF (i.eq.113.or.i.eq.81) THEN       ! Checck if Q
        GOTO 50
      ENDIF

      if (char(i).eq.'#') goto 112

      IF(i.eq.114.or.i.eq.82) THEN       ! check if R
        call clear_to_alpha
        CLOSE(2)
        GOTO 111
      ENDIF

      IF (i.eq.109.or.i.eq.77.or.i.eq.32) THEN   ! check if M
         SXMAX=((xm/520)-0.5)/xdim
         SYMAX=((ym/390)-0.5)/ydim

         IF (SXMAX.lt.0.0.or.SXMAX.gt.1.0) THEN
            GOTO 112
         ENDIF

         IF (SYMAX.lt.0.0.or.SYMAX.gt.1.0) THEN
            GOTO 112
         ENDIF

         SXMAX=(SXMAX-0.5)*2*SLWMAX
         SYMAX=(SYMAX-0.5)*2*SLWMAX

         AZIMUT=90-ATAN(SYMAX/SXMAX)/(2*pi)*360
         IF (SXMAX.lt.0.0) THEN
           AZIMUT=AZIMUT+180
         ENDIF
         APPVEL=1/SQRT(SXMAX*SXMAX+SYMAX*SYMAX)

         WRITE (TEXT(1:5),'(I3)') INT(AZIMUT)
         call contxmess ('MAN',1,3,0.4,-0.22)
         call contxmess (TEXT,1,5,0.5,-0.22)
         WRITE (TEXT(1:5),'(F5.2)') APPVEL
         call contxmess ('MAN',1,3,0.4,-0.27)
         call contxmess (TEXT,1,5,0.5,-0.27)
         GOTO 112
      ENDIF

      IF (i.eq.115.or.i.eq.83) THEN       ! check if S
        WRITE(2,'(A15)') ' VALUES TO SAVE:    ' 
        WRITE(2, 210) APPVEL, INT(AZIMUT)
        GOTO 280
      ENDIF


50    call clear_to_alpha

      write(6,*)' Plot file is fk.eps'

      WRITE(2,*)'VALUES TO SAVE:    '
                    
280   WRITE(2,*) 'Station         Long       Lat   Elev'//
     +'    Xcoord    Ycoord Zcoord'

      DO n=1, NCHAN

        WRITE(2,260) IDENT(n), stlong(n), stlat(n), 
     +  INT(stelev(n)),INT(XCOORD(n)), INT(YCOORD(n)),
     +  INT(ZCOORD(n))
260     FORMAT(1X, A10, F10.5, 1X, F9.5, 1X, I6, 1X,
     +          I9, 1X, I9, 1X, I6)
         
      END DO

      WRITE(2,'(A )') ' XSLOW   YSLOW  POWER'

      DO i=1, WRKDIM
      IF (SLX(i).EQ.0 .AND. SLY(i).EQ.0 .AND. 
     +    WRKBUF(i).EQ.0) THEN
        GO TO 100
      ELSE
        WRITE (2,'(1X, F5.2,3X, F5.2,2X, F5.3)')
     +    SLX(i), SLY(i), WRKBUF(i)
      END IF
      END DO

100   CONTINUE


      CLOSE(2)

      stop
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c-----
c fk analysis subroutines 
c-----

      SUBROUTINE SLBBFK ( CHNDAT, CHNPTR, CHNPTS, DELTA , START ,
     +                    STOP  , IDENT , INSTR , FRQLOW, FRQHIG,
     +                    XCOORD, YCOORD, ZCOORD, SLWMAX, NCHAN ,
     +                    ALPHA , BETA  , THETA , GRDPTS, WRKBUF,
     +                    WRKDIM, CLXBUF, CLXDIM, APPVEL, AZIMUT,
     +                    SXMAX , SYMAX , OUTLOW, OUTHIG, POWER ,
     +                    ABSPOW, IFKQ  , ERRMSG, TYPCTL, SLX   ,
     +                    SLY)

      COMPLEX*8     CLXBUF(*)
      REAL*4        CHNDAT(*), DELTA(*) , START(*) , STOP(*)  , FRQLOW
      REAL*4        FRQHIG   , SLWMAX   , ALPHA    , BETA     , THETA
      REAL*4        WRKBUF(*), APPVEL   , AZIMUT   , SXMAX    , SYMAX
      REAL*4        XCOORD(*), YCOORD(*), ZCOORD(*), OUTLOW   , OUTHIG
      REAL*4        POWER    , ABSPOW
      REAL*4        SLX(*)   , SLY(*)
      INTEGER*4     CHNPTR(*), CHNPTS(*), NCHAN    , GRDPTS   , WRKDIM
      INTEGER*4     CLXDIM   , IFKQ     , TYPCTL, XDB
      CHARACTER*(*) IDENT(*) , INSTR    , ERRMSG
C.======================================================================
C.    PURPOSE
C     slbbfk                                                        SS<<
C     To calculate wide-band slowness estimates
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C     Parameters:
C
C..   CHNDAT - Array of data
C
C..   CHNPTR - Pointer of start of each channel in CHNDAT
C
C..   CHNPTS - points for each channel
C
C..   DELTA  - Sampling interval for each channel
C
C..   START  - Start of analysis interval (seconds), if analysis
C              window .eq. the full data window => START = 0.0
C
C..   STOP   - Stop of analysis interval (seconds), if analysis
C              window .eq. the full data window => STOP = CHNPTS*DELTA
C
C..   IDENT  - Channel identification (A0Z, A0N, A0E, C1Z, C2Z,...)
C
C..   INSTR  - If 'VERTICAL', then only vertical sensors processed,
C
C              if '3COMP', three-comp. processing of P-waves.
C
C..   FRQLOW - Lower frequency limit
C
C..   FRQHIG - Higher frequency limit
C
C..   XCOORD - Relative X-coordinate (EW) of channel in meters
C
C..   YCOORD - Relative Y-coordinate (NS) of channel in meters
C
C..   ZCOORD - Elevation of channel in meters
C
C..   SLWMAX - Maximum slowness in search
C
C..   NCHAN  - channels to process
C
C..   THETA  - Angle of incidence for elevation correction, if THETA
C
C              is set to 90.0 ==> no elevation correction with THETA
C
C..   ALPHA  - local P-wave velocity, if known and THETA not set,
C              
c              used for P-phase elevation corrections. Unknow mean any 
c
c              value larger than 10 km/s.
C
C..   BETA   - local S-wave velocity, if known and THETA not set,
C              
c              used for S-phase elevation corrections. Unknow mean any 
c
c              value larger than 10 km/s.
C
C..   GRDPTS - points in one slowness search row.
C
C..   WRKDIM - dimension of WRKBUF, should be .GE. GRDPTS**2 + GRDPTS/2
C
C..   CLXBUF - complex work array for storing fourier transforms
C
C..   CLXDIM - dimension of CLXBUF, should be .GT. (NFFT/2 + 1)*NCHAN
C
C..   TYPCTL - Verbosity level,i.e. 0=no printout(terminal),1,2,3
C              more details.
C
C
C.    OUTPUT
C..   WRKBUF - array of power estimates in slowness space, stored in
C
C              standard lexicographical order
C
C..   APPVEL - Apparent velocity of power maximum, interpolated.
C
C..   AZIMUT - Azimuth of power maximum, interpolated
C
C..   SXMAX  - Horizontal slowness Sx of power maximum, interpolated
C
C..   SYMAX  - Horizontal slowness Sy of power maximum, interpolated
C
C..   OUTLOW - Corrected lower frequency
C
C..   OUTHIG - Corrected higher frequency
C
C..   POWER  - Normalized power maximum
C
C..   ABSPOW - Power maximum in dB, comparable to narrow-band F-K
C
C..   IFKQ   - F-K quality measure (see FKQUAL FORTRAN)
C
C..   ERRMSG - Error message
C
C..   IRC    - Return code
C            = 0  No error
C            = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Tormod Kvaerna
C.    CREATION_DATE December 1, 1986
C.    MADE_AT       NTNF/NORSAR
C                   Pb. 51
C                   N-2007 Kjeller
C
C.    MODIFICATION  June 6, 1988
C                   June 6, 1988: Implementation of quality measure
C                   Nov   , 1988: Trying to increase performace. Jan Fyen
C
C                   Oct 26, 1994: Changes for elevation corrections. JS
C.    CORRECTION    Nov 18, 1994
C                   Nov 18, 1994: ABSPOWER in 3comp-case corrected. JS
C.======================================================================
C
C     COMMENT:
C
C=======================================================================
 
C----
C   Internal declarations
C----
 
      PARAMETER ( NDIM = 100 )
 
      COMPLEX*8 CSPEC        , CTEMP       , CARR(NDIM), QDIR(3)
      REAL*8    DOTPOW       , DTEMP       , DTEMP1
      REAL*8    DBLTMP
      REAL*4    XX(NDIM)     , YY(NDIM)    , ZZ(NDIM), XREF, YREF, ZREF
      REAL*4    FARR (NDIM)  , WC(2)
      REAL*4    GSCAL, GSCAL1, PI2, PI, VEL0, TEMP0, TEMP4
      INTEGER*4 INTPTR (NDIM), INTPTS(NDIM), CFTPTR(NDIM) , INDCH(NDIM)
      INTEGER*4 CFTPTS (NDIM), IERR        , LIMIT        , CENTER
      LOGICAL   INHOM
      CHARACTER*1 COMP
 
C---
C JF set default output values for errors in fk-results.
C---
      SYMAX  = 0.0
      SXMAX  = 0.0
      AZIMUT = 0.0
      APPVEL = 999.8
      POWER  = 0.01
      ABSPOW = 0.01
      OUTLOW = FRQLOW
      OUTHIG = FRQHIG
      IFKQ   = 5
C---
      ERRMSG = ' '
      WC(1)  = 0.0
      WC(2)  = 0.0
C j.s. PI     = 3.14159265
      PI     = 4.* atan(1.)
      TWOPI  = 2.0*PI
      PI2    = PI / 2.
      TEMP0  = 0.
      GSCAL1 = 0.
      LIMIT  = GRDPTS / 2
      CENTER = LIMIT + 1
c---
c JF 95/06/01 : I tried to figure out why deltas is negative and
c               found after some trials that Sx, Sy does not follow
c               the slowness coordinates for FK plot. I.e. SX is NOT
c               positive towards east. It actually follow the direction
c               of the wave, just like NORSAR UX,UY slowness coordinates.
c               I.e.  a slowness vector with positive XSX and positive YSY
c               components, points towards south-west.
c---
      DELTAS = -SLWMAX / FLOAT (LIMIT)
      THETEN = THETA * PI / 180.0
CJF
      COSTHE = COS(THETEN)
      SINTHE = SIN(THETEN)
      IF( SINTHE .LE. 0.0  ) THEN
          WRITE(ERRMSG,'(2X,''***> SLBBFK Sin(THETA)  '',2F10.3)')
     +                        SINTHE,THETA
          RETURN
      END IF
      TANTHE = COSTHE/SINTHE
CJF
 
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''---> SLBBFK Clear WRKBUF'',I6)') WRKDIM
          WRITE(6,'(7X,''Points,center,deltas'',2I5,E14.6)')
     +              GRDPTS,CENTER,DELTAS
      END IF
      IF( FRQLOW.LE.0.0.OR.
     +    FRQHIG.LE.0.0.OR.
     +    FRQHIG.LE.FRQLOW ) THEN
          WRITE(ERRMSG,'(2X,''***> SLBBFK Wrong freq  '',2F10.3)')
     +                        FRQLOW,FRQHIG
          RETURN
      END IF
 
      CALL CHZERO ( WRKBUF(1), WRKDIM )
 
C----
C   Prepare input to fourier transform calculation
C----

      CALL SLSUBI ( NCHAN , CHNPTR, NCHAN , CHNPTS, NCHAN , START ,
     +              NCHAN , STOP  , NCHAN , NDIM  , INTPTR, INTPTS,
     +              DELTA , ERRMSG, IERR  , TYPCTL )

      IF ( ERRMSG .NE. ' ' ) THEN
 
         RETURN
 
      ENDIF
 
C----
C   Fourier transform calculation
C----

      CALL SLFOUT ( CHNDAT, NCHAN , INTPTR, NDIM  , INTPTS, NDIM  ,
     +              WRKBUF, WRKDIM, CLXBUF, CLXDIM, CFTPTR, NDIM  ,
     +              CFTPTS, NDIM  , ERRMSG, IERR  , TYPCTL )
 
      IF ( ERRMSG .NE. ' ' ) THEN

         RETURN
 
      ENDIF
 
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''---> SLBBFK Clear WRKBUF'',I6)') WRKDIM
      END IF
      CALL CHZERO ( WRKBUF(1), WRKDIM )
 
C----
C   Calculate centroid of channels
C----
 
      CALL SLCENT ( XCOORD, YCOORD, ZCOORD, XREF, YREF, ZREF, NCHAN )
 
C----
C   Store corrected coordinates in tables
C----
 
      DO 1000 ICHAN = 1, NCHAN
 
         XX(ICHAN) = ( XCOORD(ICHAN) - XREF ) / 1000.0
         YY(ICHAN) = ( YCOORD(ICHAN) - YREF ) / 1000.0
         ZZ(ICHAN) = ( ZCOORD(ICHAN) - ZREF ) / 1000.0
 
	 IF ( INSTR .EQ. '3COMP' ) THEN
C---
C Choose what component from last character of name being 'z','n','e'.
C---

C
C AP Next line is changed, old: NC    = IC0LEN(IDENT(ICHAN))
C

            NC    = LEN(IDENT(ICHAN))
            COMP  = IDENT(ICHAN)(NC:NC)

            IF( COMP.EQ.'z' .OR. COMP.EQ.'Z' ) THEN
	        INDCH(ICHAN) = 1

            ELSE IF( COMP.EQ.'n' .OR. COMP.EQ.'N' ) THEN
	        INDCH(ICHAN) = 2

            ELSE IF( COMP.EQ.'e' .OR. COMP.EQ.'E' ) THEN
	        INDCH(ICHAN) = 3

            ELSE 
 
                ERRMSG = '***> SLBBFK (3-comp): Illegal channel '//
     +                           'id '
                ERRMSG(43:50) = IDENT(ICHAN)(1:8)
 
                RETURN
 
            ENDIF

         ENDIF
 
 1000 CONTINUE
 
C----
C   Calculate start and stop points of FFT from low and high
C   frequency bounds, assuming all channels to have same sampling rate
C----
 
      XNFFT   = 2.0*FLOAT(CFTPTS(1) - 1)
      K1      = IFIX ( (DELTA(1) * FRQLOW * XNFFT ) + 1.001)
      K2      = IFIX ( (DELTA(1) * FRQHIG * XNFFT ) + 1.001)
 
      THLP    = TWOPI / ( ( XNFFT ) * DELTA(1) )
 
      IF ( K1 .LT. 1 ) THEN
 
         K1 = 1
 
      ENDIF
 
      IF ( K2 .GT. CFTPTS(1) ) THEN
 
         K2 = CFTPTS(1)
 
      ENDIF
 
      OUTLOW  = FLOAT (K1 - 1) * THLP / TWOPI
 
      OUTHIG  = FLOAT (K2 - 1) * THLP / TWOPI
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''...> SLBBFK K1,K2'',2I5,F7.1,F9.3)')
     +                        K1,K2,XNFFT,THLP
          WRITE(6,'(2X,''...> SLBBFK Outlow,high'',2F9.3)')
     +                        OUTLOW,OUTHIG
      END IF
 
      DOTPOW = 0.0D0
 
      DO 1200 IFRQ = K1, K2
 
         IFRQ1 = IFRQ - 1
         DO 1100 ICHAN = 1, NCHAN
 
            IND    = CFTPTR(ICHAN) + IFRQ1
            DBLTMP = CABS( CLXBUF(IND) )
            DOTPOW = DOTPOW + DBLTMP*DBLTMP

 1100    CONTINUE
 
 1200 CONTINUE
 
C----
C   Scaling factor
C----
 
      DTEMP1 = ( DFLOAT (NCHAN) * DOTPOW )
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''...> SLBBFK Scale DTEMP1,DOTPOW,N'',
     +              2E14.6,I4)')
     +              DTEMP1,DOTPOW,NCHAN
      END IF
C---
C  JF : for bad data, DOTPOW and DTEMP1 may turn zero.
C---
      IF( DTEMP1.LE.0.0.OR.DOTPOW.LE.0.0 ) THEN
          ERRMSG = '***> SLBBFK zero dot power (bad data)'
 
          RETURN
 
      END IF
C----
C   Now, do the calculation for vertical sensors
C----
 
      IF ( INSTR (1:8) .EQ. 'VERTICAL' ) THEN
 
C----
C   Loop over Sy
C----
 
         DO 3000 I = -LIMIT, LIMIT
 
            YSY = FLOAT(I) * DELTAS
            YS2 = YSY*YSY
C----
C   Loop over Sx
C----
 
            DO 2900 J = -LIMIT, LIMIT
 
               XSX = FLOAT(J) * DELTAS
 
C----
C   Time element of phase lag (with or without elevation corrections)
C----
 
               VINV = SQRT ( YS2 + (XSX*XSX) )
 
CJF            TEMP = VINV*COS(THETEN) / SIN(THETEN)
C----
CJS            TEMP = VINV*TANTHE
C
C   Johannes Schweitzer 26. Oct. 1994
C   Elevation phase correction included for all cases.
C
C   Assuming that ALPHA and BETA give local velocity 
C   under the array then THETA and the corresponding 
C   elevation correction can be calculated.
C
C   If incidenc angle THETA (i.e. THETA.ne.90 deg) is given, 
C   this value is used anyway.
C----

               IF ( THETA.GE.90. .AND. (ALPHA.LT. 10. .OR. 
     +	            BETA.LT. 10.)) THEN

		  VEL0 = 999. 

                  IF(ALPHA.LT. 10.)    VEL0 = ALPHA

	          IF(VINV.GT.1./VEL0 .AND. BETA.LT. 10.) 
     +                                 VEL0 = BETA

	          IF(VINV.GE.1./VEL0)  THEN
		     TEMP = 0.
		  ELSE
	             THETEN = ASIN (VEL0 * VINV)
                     TEMP = COS ( THETEN ) / VEL0
		  ENDIF

               ELSE

		  TEMP = VINV*TANTHE

               ENDIF
 
C----
C   Store coordinate phase contribution in tables
C----
 
               DO 2100 JK = 1, NCHAN
 
                  FARR (JK) = ( (XSX * XX (JK) ) + (YSY * YY (JK) ) ) +
     +                        TEMP * ZZ(JK)
 
 2100          CONTINUE
 
C----
C   Loop over angular frequency
C----
 
               DTEMP = 0.0D0
 
               DO 2300 K = K1, K2
 
                  OMEGA = THLP * FLOAT (K - 1)
 
C----
C   Loop over channels
C----
 
                  CTEMP = CMPLX (0.0, 0.0)
 
                  DO 2200 L = 1, NCHAN
 
                     ARG   = OMEGA * FARR(L)
 
                     CSPEC = CMPLX ( COS(ARG), SIN(ARG) )
 
                     IND   = CFTPTR(L) + K - 1
 
                     CTEMP = CTEMP + CLXBUF(IND) * CSPEC
 
2200              CONTINUE
 
                  DBLTMP = CABS (CTEMP)
                  DTEMP  = DTEMP + DBLTMP*DBLTMP
 
2300           CONTINUE
 
C----
C   And now, store the final value
C----
 
               I1 = J + CENTER + ( I + CENTER - 1 ) * GRDPTS
 
               WRKBUF (I1) = SNGL ( DTEMP / DTEMP1 )

C
C AP To store slowness values in arrays SLX, SLY 
C

               SLX (I1) = XSX

               SLY (I1) = YSY
 
               IF( TYPCTL.GT.6 .OR.
     +            (TYPCTL.GT.2 .AND.IABS(I).EQ.IABS(J))) THEN
                   VKM  = VINV
                   VANG = 0.0
                   IF( YSY.NE.0.0.AND.XSX.NE.0.0 ) VANG=ATAN2(XSX,YSY)
                   VANG = VANG*180.0/3.14159257
                   IF( VINV.NE.0.0 ) VKM = 1.0/VINV
                   WRITE(6,'(2X, 3I5,2F8.4,F7.3,F8.2,E14.6)')
     +                       I,J,I1,YSY,XSX,VKM ,VANG,DTEMP
               END IF
 
 
2900        CONTINUE
 
3000     CONTINUE
 
C----
C   Three-component method comes here
C----
 
      ELSE IF ( INSTR(1:5) .EQ. '3COMP') THEN

C----
C   Loop over Sy
C----
 
         DO 5000 I = -LIMIT, LIMIT
 
            YSY = FLOAT(I) * DELTAS
 
C----
C   Loop over Sx
C----
 
            DO 4900 J = -LIMIT, LIMIT
 
               XSX = FLOAT(J) * DELTAS
 
C----
C   Time element of phase lag (with or without elevation corrections)
C----
 
               VINV = SQRT ( (YSY*YSY) + (XSX*XSX) )
 
CJF            TEMP = VINV*COS(THETEN) / SIN(THETEN)
C----
CJS            TEMP = VINV*TANTHE
C
C   Johannes Schweitzer 26. Oct. 1994
C   Elevation phase correction included for all cases 
C
C   Assuming that ALPHA and BETA give local velocity 
C   under the array then THETA and the corresponding 
C   elevation correction can be calculated.
C
C   If incidenc angle THETA (i.e. THETA ne 90 deg) is given, 
C   this value is used anyway.
c----

               IF ( THETA.GE.90. .AND. (ALPHA.LT. 10. .OR. 
     +	            BETA.LT. 10.)) THEN
		
		  VEL0 = 999.

                  IF(ALPHA.LT. 10.)    VEL0 = ALPHA

	          IF(VINV.GT.1./VEL0 .AND. BETA.LT. 10.) 
     +                                 VEL0 = BETA

	          IF(VINV.GE.1./VEL0)  THEN
		     THETEN = PI2
		     TEMP = 0.
		  ELSE
	             THETEN = ASIN (VEL0 * VINV)
                     TEMP = COS ( THETEN ) / VEL0
		  ENDIF

               ELSE

		  TEMP = VINV*TANTHE
C----
C    Use ALPHA and BETA default values from NORESS if set 
C    (ever needed for 3comp fk!).
C----
CJF              IF (ALPHA.GE.999.) ALPHA = 6.0
CJF              IF (BETA .GE.999.) BETA  = 3.46

               ENDIF
 
C----
C   Call P-phase direction vector estimate for this slowness
C----
               INHOM = .TRUE.
	       TEMP4 = 0.
 
	       IF ( VINV .LE. 1./ALPHA ) THEN
                  CALL SLDIR3 ( XSX, YSY, ALPHA, BETA, QDIR, INHOM )
               ENDIF
 
               IF ( INHOM ) GO TO 4700

               GSCAL = 0.0

               DO 4400 JK = 1, NCHAN
 
                 IF ( INDCH(JK).EQ.1 ) THEN
 
                    CARR(JK) = QDIR(1)
                    GSCAL    = GSCAL + CABS ( (QDIR(1) )**2 )

                 ELSE IF ( INDCH(JK).EQ.2 ) THEN
 
                    CARR(JK) = QDIR(2)
                    GSCAL    = GSCAL + CABS ( (QDIR(2) )**2 )
 
                 ELSE IF ( INDCH(JK).EQ.3 ) THEN
   
                    CARR(JK) = QDIR(3)
                    GSCAL    = GSCAL + CABS ( (QDIR(3) )**2 )
 
                 ENDIF
 
C----
C   Store coordinate phase contribution in tables
C----
 
                 FARR (JK) = ( (XSX * XX (JK) ) + (YSY * YY (JK) ) ) +
     +                       TEMP * ZZ(JK)
 
4400           CONTINUE
 
C----
C   Loop over angular frequency
C----
 
               DTEMP = 0.0D0
 
               DO 4600 K = K1, K2
 
                  OMEGA = THLP * FLOAT (K - 1)
 
C----
C   Loop over channels
C----
 
                  CTEMP = CMPLX (0.0, 0.0)
 
                  DO 4500 L = 1, NCHAN

                     ARG   = OMEGA * FARR(L)
 
                     CSPEC = CMPLX ( COS(ARG), SIN(ARG) )
 
                     IND   = CFTPTR(L) + K - 1
 
                     CTEMP = CTEMP + CLXBUF(IND) * CSPEC*
     +                          CONJG ( CARR(L) )
 
4500              CONTINUE
 
                  DTEMP = DTEMP + DBLE(CABS(CTEMP))**2.0D0
 
4600           CONTINUE
 
               TEMP4 = SNGL (DTEMP / DOTPOW) / GSCAL

C----
C   To save the GSCAL of the MAXIMUM FK-result
C   (To get the right ABSPOW-value)
C   Johannes Schweitzer
C----
	       IF(TEMP4.GT.TEMP0) THEN

	         GSCAL1 = GSCAL
	         TEMP0  = TEMP4

	       ENDIF

C----
C   And now, store the final value
C----

4700           CONTINUE

               I1 = J + CENTER + ( I + CENTER - 1 ) * GRDPTS

               WRKBUF (I1) = TEMP4
 
4900        CONTINUE
 
5000     CONTINUE
 
      ENDIF
 
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''...> SLBBFK That took some time ...   '')')
      END IF
 
C----
C   Measure maximum of slowness matrix
C----
 
      CALL SLFKMS ( WRKBUF, GRDPTS, SLWMAX, WC, 1.0, SYMAX, SXMAX,
     +              AZIMUT, APPVEL, POWER )
 
C----
C   Calculate absolute power, analogue to narrow-band F-K
C----
 
      IF ( INSTR (1:8) .EQ. 'VERTICAL' ) THEN
         ABSPOW = POWER * SNGL(DTEMP1) / FLOAT(K2-K1+1)
 
      ELSE IF ( INSTR (1:5) .EQ. '3COMP' ) THEN
         ABSPOW = POWER * SNGL(DOTPOW)*GSCAL1 / FLOAT(K2-K1+1)
 
      ENDIF

      ABSPOW = ABSPOW / ( FLOAT (NCHAN*NCHAN) )
      ABSPOW = ABSPOW * DELTA(1) / ( FLOAT (INTPTS(1) ) )
 
      IF ( ABSPOW .GT. 0.0 ) THEN
         ABSPOW = 10.0 * ALOG10 (ABSPOW)
      ELSE
         ABSPOW = 0.0
      ENDIF
 
C----
C   Calculate F-K quality measure
C----
 
      IPOINT = (GRDPTS*GRDPTS) + 1
 
      XDB = 7.0
 
      CALL SLFKQA (WRKBUF, GRDPTS, POWER, XDB, WRKBUF(IPOINT), IFKQ )
 
C----
C   That's it|
C----
 
      RETURN
 
      END
      SUBROUTINE SLCENT (X, Y, Z, XREF, YREF, ZREF, NCHAN)
      REAL*4    XREF, YREF, ZREF
      REAL*4    X(*), Y(*), Z(*)
      INTEGER*4 NCHAN

C.======================================================================
C.    PURPOSE
C     Slcent                                                        SS<< 
C     This function returns the centroid of NCHAN given stations
C
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C..   X, Y, Z      - Coordinates of stations in meters
C
C..   NCHAN        - Number of channels
C
C
C
C.    OUTPUT
C..   XREF, YREF, ZREF - Coordinates of reference point in meters
C
C
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Tormod Kvaerna
C.    CREATION_DATE November 19, 1986
C.    MADE_AT       NTNF/NORSAR
C                   Pb. 51
C                   N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
C.@PROCESS OPT(3)
C 
C.OPT(3)
C=======================================================================
C
C     COMMENT:
C
C=======================================================================
 
      XREF = 0.0
      YREF = 0.0
      ZREF = 0.0
 
      DO 100 I = 1, NCHAN
 
         XREF = XREF + X(I)
         YREF = YREF + Y(I)
         ZREF = ZREF + Z(I)
 
100   CONTINUE
 
      ZREF = ZREF / FLOAT (NCHAN)
      YREF = YREF / FLOAT (NCHAN)
      XREF = XREF / FLOAT (NCHAN)
 
      RETURN
 
      END
      SUBROUTINE SLDIR3 ( SX, SY, ALPHA, BETA, QDIR, INHOM )
      REAL*4      SX, SY, ALPHA, BETA
      COMPLEX     QDIR (3)
      LOGICAL     INHOM
C.======================================================================
C.    PURPOSE
C     sldir3                                                        SS<<
C     Subroutine to calculate direction vector for P- waves on a
C     3C - station. The direction vector is scaled to unity.
C
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C..   SX     - Horizontal wavenumber in EW-direction
C
C..   SY     - Horizontal wavenumber in NS-direction
C
C..   ALPHA  - Surface P-wave velocity
C
C..   BETA   - Surface S-wave velocity
C
C
C.    OUTPUT
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C
C..   QDIR   - Direction vectors for displacement
C
C..   QDIR(1)- Direction vector for Z-comp
C
C..   QDIR(2)- Direction vector for NS-comp
C
C..   QDIR(3)- Direction vector for EW-comp
C
C.----------------------------------------------------------------------
C.    PROGRAMMER    Tormod Kvaerna   
C.    CREATION_DATE 
C.    MADE_AT       NTNF/NORSAR
C                   Pb. 51
C                   N-2007 Kjeller
C
C.    MODIFICATION  
C.                  Restricted to P-wave slowness space.
C.                  Johannes Schweitzer, Novmeber 1994
C.    CORRECTION
C.======================================================================
C.@PROCESS OPT(3)
C.OPT(3)
C-----------------------------------------------------------------------
C
C
C
C
C
C----
 
 
 
 
 
C----
C   Declaration of internal variables
C----
 
 
      INHOM = .FALSE.
 
      IF ( (SX .LT. 0.0001) .AND. (SY .LT. 0.0001)   .AND.
     %     (SX .GT. -0.0001) .AND. (SY .GT. -0.0001) ) THEN
 
         QDIR (1) = CMPLX (1.0, 0.0)
         QDIR (2) = CMPLX (0.0, 0.0)
         QDIR (3) = CMPLX (0.0, 0.0)
 
      ELSE
 
C----
C   Calculate some terms
C----
 
         SXSY2    = SX**2 + SY**2
 
         P        = SQRT ( SXSY2 )
CJS      PLIMIT   = SQRT ( 1.0 / (2.0 * BETA * BETA) )
         PLIMIT   = 1.0 / P
 
C----
C   Ray parameter must be less than PLIMIT
C----
 
         IF ( P .LE. PLIMIT ) THEN
 
            PBETA2   = (P**2) * (BETA**2)
 
            TERM1    = (1.0 - 2.0*PBETA2)**2
 
            TERM2    = 4.0 * PBETA2 * (1.0 - PBETA2)
 
            TERM3    = 1.0 / ( SXSY2 * ( 1.0 + ( TERM1 / TERM2 ) ) )
 
            TERM4    = 1.0 / ( 1.0 + ( TERM2 / TERM1 ) )
 
            A        = SQRT (TERM3)
 
            TK1      = SQRT (TERM4)
 
            QDIR(1)  = CMPLX (TK1, 0.0)
 
            TK1      =  A * SY
 
            QDIR(2)  = CMPLX (TK1, 0.0)
 
            TK1      =  A * SX
 
            QDIR(3)  = CMPLX (TK1, 0.0)
 
         ELSE
 
            QDIR(1) = CMPLX (0.0, 0.0)
 
            QDIR(2) = CMPLX (0.0, 0.0)
 
            QDIR(3) = CMPLX (0.0, 0.0)
 
            INHOM = .TRUE.
 
         ENDIF
 
      ENDIF
 
C----
C   The correct vector is now hopefully calculated
C----
 
      RETURN
 
      END
      SUBROUTINE SLFFT(XREAL, XIMAG, N, IDIR)
      DIMENSION XREAL(*), XIMAG(*), TCOS(2049), TSIN(2049)
      DOUBLE PRECISION FUND, COSINE, SINE
C.======================================================================
C.    PURPOSE
C     Slfft                                                         SS<<
C     Program to compute the FFT of a sequence.
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C
C
C.    OUTPUT
C..   XREAL(*)        -   The real part of the sequence is stored in
C                         this array.
C..   XIMAG(*)        -   The imaginary part is stored here.
C..   TCOS(*),TSIN(*) -   Cosine and sine tables.
C..   N               -   The length of the sequence.
C..   LOGN            -   The base-two logarithm of N.
C..   IDIR            -   Variable to indicate the direction of the transform.
C                         For a forward transform IDIR=-1 and for an inverse
C                         transform IDIR=1.  Normalization is performed
C                         for IDIR=1.
C     The transform result is returned in XREAL and XIMAG.
C     The initial sequence is destroyed.
C
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Dave Harris
C.    CREATION_DATE 
C.    MADE_AT       Livermore, CA, USA 
C
C.    MODIFICATION  November 14, 1977
C.    CORRECTION
C.======================================================================
C.@PROCESS OPT(3)
C.OPT(3)
C---------------------------------------------------------------------
C
C - FFT
C
C----
 
 
      NBY2 = N/2
      LOGN = 1
    3 CONTINUE
        IF (  2**LOGN  .EQ.  N ) THEN
          GO TO    4
        ELSE
          LOGN = LOGN+1
        END IF
      GO TO    3
    4 CONTINUE
C----
C     Table generator
C----
      FUND = 3.141592653589793D0*2.0D0
      FUND = FUND/FLOAT(N)
      COSINE = DCOS(FUND)
      SINE = DSIN(FUND)
      IF (  IDIR .EQ. -1 ) THEN
        SINE = -SINE
      END IF
      TCOS(1) = 1.
      TSIN(1) = 0.
      DO    5 I = 2, NBY2
        TCOS(I) = COSINE*TCOS(I-1) - SINE*TSIN(I-1)
        TSIN(I) = COSINE*TSIN(I-1) + SINE*TCOS(I-1)
    5 CONTINUE
C----
C     Bit reverse code
C----
        NM1 = N-1
        J = 1
        DO    6 I = 1, NM1
        IF (  I .LT. J ) THEN
          TEMP = XREAL(I)
          XREAL(I) = XREAL(J)
          XREAL(J) = TEMP
          TEMP = XIMAG(I)
          XIMAG(I) = XIMAG(J)
          XIMAG(J) = TEMP
        END IF
        K = NBY2
    7   CONTINUE
        IF (.NOT.( K .LT. J )) GO TO    8
          J = J-K
          K = K/2
        GO TO    7
    8   CONTINUE
        J = J+K
    6   CONTINUE
C----
C     Indexing code
C----
        NBLOCK = N
        IREL = 1
        DO    9 NSTAGE = 1, LOGN
          IF (  NSTAGE .GT. 1 ) THEN
            IREL = IREL*2
          END IF
          I = -IREL
          NBLOCK = NBLOCK/2
          DO   10 IBLOCK = 1, NBLOCK
            I = I+IREL
            ITI = 1-NBLOCK
            DO   11 ICOUNT = 1, IREL
              I = I+1
              ITI = ITI+NBLOCK
              I1 = I+IREL
C----
C             Butterfly code
C----
              IF (  NSTAGE  .GT.  1 ) THEN
                SINE = TSIN(ITI)
                COSINE = TCOS(ITI)
                TEMP = XREAL(I1)*COSINE-XIMAG(I1)*SINE
                XIMAG(I1) = XREAL(I1)*SINE+XIMAG(I1)*COSINE
                XREAL(I1) = TEMP
              END IF
              TEMP = XREAL(I)+XREAL(I1)
              XREAL(I1) = XREAL(I)-XREAL(I1)
              XREAL(I) = TEMP
              TEMP = XIMAG(I)+XIMAG(I1)
              XIMAG(I1) = XIMAG(I)-XIMAG(I1)
              XIMAG(I) = TEMP
C
   11       CONTINUE
   10     CONTINUE
    9   CONTINUE
C----
C  If reverse transform, divide through by N
C----
        IF (  IDIR  .EQ.  1 ) THEN
          SCALE = 1./FLOAT(N)
          DO   12 I = 1, N
            XREAL(I) = XREAL(I)*SCALE
            XIMAG(I) = XIMAG(I)*SCALE
   12     CONTINUE
        END IF
C
C  BYE
C
      RETURN
      END
      SUBROUTINE SLFIT2(H, X, Y)
      DIMENSION H(-1:1, -1:1), PHI(0:2, 0:2)
C.======================================================================
C.    PURPOSE
C     Slfit2                                                     SS<<
C     Calculates parabolic refinement of estimated peak location from
C     grid values.
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C..   H  -   Input grid
C
C.    OUTPUT
C..   X  -   Peak location x coordinate
C..   Y  -   Peak location y coordinate
C
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Dave Harris.
C.    CREATION_DATE 
C.    MADE_AT       Livermore, CA, USA
C
C.    MODIFICATION  February 15, 1985 (NORSAR)
C                   November   , 1988 (Jan Fyen,performance)
C
C.    CORRECTION
C.======================================================================
C
C----
C.@PROCESS OPT(3)

CJF   INTEGER*4 SLBAS(9)
CJF   DATA SLBAS/1,-1,1,1,0,0,1,1,1/
C----
C  Construct correlations
C----

        DO   23 I = 0, 2
          I4       = I+4
          DO   24 J = 0, 2
            J4       = J+4
            PHI(I,J) = 0.
            DO   25 M = -1, 1
              MM = M*3
              DO   26 N = -1, 1
                PHI(I,J) = PHI(I,J) + H(M,N)*SLBAS(M,I)*SLBAS(N,J)
CJF             JFI      = MM+I4
CJF             JFJ      = 3*N+J4
CJF             PHI(I,J) = PHI(I,J) + H(M,N)*SLBAS(JFI)*SLBAS(JFJ)
C.Modified for performance
CJ.Fyen       M  I   SLBAS    N  J   SLBAS         (1+M)*3+I+1=  3M+I+4
C            -1**0   1       -1**0   1        1    1
C            -1**1  -1       -1**1  -1        2    2
C            -1**2   1       -1**2   1        3    3
C             0**0   1        0**0   1        4    4
C             0**1   0        0**1   0        5    5
C             0**2   0        0**2   0        6    6
C             1**0   1        1**0   1        7    7
C             1**1   1        1**1   1        8    8
C             1**2   1        1**2   1        9    9

   26         CONTINUE

   25       CONTINUE

   24     CONTINUE

   23   CONTINUE

        A = PHI(2,0)/2. - PHI(0,0)/3.
        B = PHI(1,1)/4.
        C = PHI(0,2)/2. - PHI(0,0)/3.
        D = PHI(1,0)/6.
        E = PHI(0,1)/6.

        DENOM = B**2 - 4.*A*C
        X = (2.*C*D - B*E)/DENOM
        Y = (2.*A*E - B*D)/DENOM

      RETURN
      END
C-----------------------------------------------------------------------
C
C----
CCC
      REAL FUNCTION SLBAS(N,I)


        IF (  I .EQ. 0 ) THEN

          SLBAS = 1.

        ELSE

          SLBAS = N**I

        END IF

      RETURN

      END

      SUBROUTINE SLFKMS(FKS, ISMPSZ, WAVNUM, WC,
     &                  FREQ, RKNMAX, RKEMAX, AZIM,
     &                  VAP, SMAX)
C.======================================================================
C.    PURPOSE
C     Slfkms                                                        SS<<
C     SLFKMS -- Code to find spectral FK maximum, then calculate
C               apparent velocity and bearing from its location.
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C..   FKS           -   REAL*4 array containing estimated FK spectrum
C
C..   SAMPLE_SIZE   -   INTEGER variable containing ^[ samples along
C                       each axis of FK spectrum.
C
C..   WAVENUMBER    -   REAL*4 variable containing maximum wavenumber
C                       along each axis, in cycles/kilometer.
C
C
C..   WINDOW_CENTER -   REAL*4 array containing the center of the FK
C                       window that is passed.  XY coordinates in
C                       wavenumber.
C
C..   FREQUENCY     -   REAL*4 variable containing analysis frequency.
C
C
C
C.    OUTPUT
C..   KN_MAX        -   REAL*4 variable containing north wavenumber
C                       of spectral maximum.
C
C..   KE_MAX        -   REAL*4 variable containing east wavenumber
C                       of spectral maximum.
C
C..   AZIMUTH       -   REAL*4 variable containing azimuth of event
C
C..   V_APPARENT    -   REAL*4 variable containing apparent velocity
C
C..   SPECTRAL_MAX  -   REAL*4 variable containing spectral maximum
C
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Dave Harris
C.    CREATION_DATE 
C.    MADE_AT       Livermore, CA, USA
C
C.    MODIFICATION  February 15, 1985 (NORSAR)
C                   November   , 1988 (Jan Fyen,performance)
C.    CORRECTION
C.======================================================================
C.@PROCESS OPT(3)
C.OPT(3)
C-----------------------------------------------------------------------
C
C
C  Replacements
C----
 
 
 
C----
C  Declarations
C----
        DIMENSION  FKS(*), H(-1:1, -1:1)
        INTEGER CENTER
        REAL*4 WC(2)
 
C----
C  Initializations
C----
 
        PI = 3.14159265
        LIMIT = ISMPSZ/2
        CENTER = LIMIT + 1
 
C----
C  Search for spectral maximum
C
C
C    Find wavenumber of spectral maximum
C----
 
          SMAX = 0.
          IMAX=0
          JMAX=0
 
          DO   19 I= -LIMIT, LIMIT
 
            KKC = CENTER + (I + CENTER - 1) * ISMPSZ
            DO   20 J= -LIMIT, LIMIT
 
              K = J + KKC
              VALUE = FKS(K)
 
              IF (  VALUE .GE. SMAX ) THEN
 
                IMAX=I
                JMAX=J
                SMAX = VALUE
 
              END IF
 
 
   20       CONTINUE
 
 
   19     CONTINUE
 
C----
C    Refine estimate if it is in the interior
C----
 
          DELTAK = WAVNUM/FLOAT(LIMIT)
 
          IF (  IABS(IMAX) .LT. LIMIT .AND. IABS(JMAX) .LT. LIMIT ) THEN
 
            DO   21 I = -1, 1
 
              KKJ = JMAX+CENTER + (IMAX+I+CENTER-1)*ISMPSZ
 
              DO   22 J = -1, 1
 
                K = J+KKJ
                H(I,J) = FKS(K)
 
   22         CONTINUE
 
   21       CONTINUE
 
            CALL SLFIT2(H, YINC, XINC)
 
            RKEMAX = (FLOAT(JMAX) + XINC)*DELTAK + WC(1)
            RKNMAX = (FLOAT(IMAX) + YINC)*DELTAK + WC(2)
 
          ELSE
 
            RKEMAX = DELTAK*FLOAT(JMAX) + WC(1)
            RKNMAX = DELTAK*FLOAT(IMAX) + WC(2)
 
          END IF
 
C----
C    Calculate azimuth and apparent velocity of event
C----
          AZIM = ATAN2(RKEMAX, RKNMAX)/PI*180.
          IF (  AZIM .LT. 0. ) THEN
            AZIM = AZIM + 360.
          END IF
          RADIUS = SQRT(RKNMAX*RKNMAX + RKEMAX*RKEMAX)
          VAP = FREQ/RADIUS
C----
C  Bye
C----
      RETURN
      END
C
 
      SUBROUTINE SLFKQA ( WRKBUF, GRDPTS, POWER , XDB, Z, IFKQ )
      REAL*4        WRKBUF(*), Z(*), POWER
      INTEGER*4     GRDPTS   , XDB, IFKQ
C.======================================================================
C.    PURPOSE
C     Slfkqa                                                        SS<<
C     To calculate F-K quality measure
C     This routine searches for peaks in WRKBUF matrix.
C     It scans the lexicographically stored matrix for for local
C     maxima within XDB dB of the largest peak.
C     The program assigns a quality measure IFKQ as a function of
C     the amplitude AMP of the second highest peak,
C     according to the following criteria.
C     6.0 \= AMP \<       :  IFKQ \= 1
C     4.0 \= AMP \< 6.0  :  IFKQ \= 2
C     1.0 \= AMP \< 4.0  :  IFKQ \= 3
C
C            AMP  1.0  :  IFKQ \= 3 if double peak (only one secondary
C                                             peak within 1.0 dB).
C            A double peak is characterized by the fact that the second
C            highest peak is less than 1 dB down from the maximum, and
C            the third highest peak is more than 1 dB down from the
C            maximum.  I.e., if more than two peaks within 1 dB,
C            the solution is bad.
C
C                       :  IFKQ \= 4 if bad solution
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C
C..   WRKBUF - Array of linear power estimates
C
C..   GRDPTS - points in one slowness search row
C
C..   POWER  - Linear power of maximum peak
C
C..   XDB    - Searching criterion for secondary peaks
C
C..   Z      - Work array (length at least 1600 elements)
C
C
C
C.    OUTPUT
C..   IFKQ   - F-K quality measure
C
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Anne Henson
C.    CREATION_DATE August 1986
C.    MADE_AT       SAIC
C                   10210 Campus Point Drive
C                   San Diego, CA 92121
C
C
C
C.    MODIFICATION Tormod Kvaerna, June 6, 1988, NORSAR
C.    CORRECTION
C.======================================================================
C.@PROCESS OPT(2)
C
C     COMMENT:
C
C=======================================================================
 
      ISIZ1 = GRDPTS - 1
      IFKQ  = 1
 
C----
C   Search for local maxima and store them
C----
 
      K = 0
 
      DO 1100 I = 2, ISIZ1
         DO 1000 J = 2, ISIZ1
 
            ICENT = J + (I - 1)*GRDPTS
            IF (WRKBUF(ICENT) .LE. 0.0) GO TO 1000
 
            Y = 10.0*ALOG10(POWER / WRKBUF(ICENT))
            IF (Y .GT. XDB) GO TO 1000
 
            I1    = J - 1 + (I - 2)*GRDPTS
            I2    = J +     (I - 2)*GRDPTS
            I3    = J + 1 + (I - 2)*GRDPTS
            I4    = J - 1 + (I - 1)*GRDPTS
            I5    = J + 1 + (I - 1)*GRDPTS
            I6    = J - 1 +       I*GRDPTS
            I7    = J +           I*GRDPTS
            I8    = J + 1 +       I*GRDPTS
 
            X  = WRKBUF(ICENT)
            X1 = WRKBUF(I1)
            X2 = WRKBUF(I2)
            X3 = WRKBUF(I3)
            X4 = WRKBUF(I4)
            X5 = WRKBUF(I5)
            X6 = WRKBUF(I6)
            X7 = WRKBUF(I7)
            X8 = WRKBUF(I8)
 
            IF ( (X1.LT.X).AND.(X2.LT.X).AND.(X3.LT.X).AND.
     +           (X4.LT.X).AND.(X5.LT.X).AND.(X6.LT.X).AND.
     +           (X7.LT.X).AND.(X8.LT.X) )                     THEN
 
 
               K = K + 1
               Z(K) = Y
 
            ENDIF
 
 1000    CONTINUE
 1100 CONTINUE
 
      NK = K
 
C----
C   Distinguish between single, double, and bad solutions
C----
 
      I1 = 0
 
      DO 1200 K = 1, NK
         IF ( Z(K) .LT. 1.0) I1 = I1 + 1
 1200 CONTINUE
 
      IF (I1 .EQ. 1) THEN
         ISOL = 1
      ELSE IF (I1 .EQ. 2 ) THEN
         ISOL = 2
      ELSE
         ISOL = 3
      ENDIF
 
C----
C   Assign F-K quality measure
C----
 
      IF (ISOL .EQ. 1) THEN
 
         DO 1300 K = 1, NK
 
            Y = Z(K)
 
            IF (Y .LT. 1.0) THEN
               GO TO 1300
            ELSE IF ( (Y .GE. 4.0) .AND. (Y .LT. 6.0) ) THEN
               IFKQ = MAX0(IFKQ,2)
            ELSE IF ( (Y .GE. 1.0) .AND. (Y .LT. 4.0) ) THEN
               IFKQ = 3
            ENDIF
 
 1300    CONTINUE
 
      ELSE IF (ISOL .EQ. 2) THEN
         IFKQ = 3
      ELSE
         IFKQ = 4
      ENDIF
 
      RETURN
 
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SLFOUT ( CHNDAT , NSEQU  , INTPTR ,
     +                    PTRDIM , INTPTS , PTSDIM , WRKBUF ,
     +                    BUFDIM , CFFT   , CFTDIM , CFTPTR ,
     +                    CPRDIM , CFTPTS , CPSDIM , ERRMSG , IERR ,
     +                    TYPCTL)

      INTEGER*4  TYPCTL
      REAL*4     CHNDAT(*)
      INTEGER*4  NSEQU
      INTEGER*4  INTPTR(*)
      INTEGER*4  PTRDIM
      INTEGER*4  INTPTS(*)
      INTEGER*4  PTSDIM
      REAL*4     WRKBUF(*)
      INTEGER*4  BUFDIM
      COMPLEX*8  CFFT(*)
      INTEGER*4  CFTDIM
      INTEGER*4     CFTPTR(*)
      INTEGER*4     CPRDIM
      INTEGER*4     CFTPTS(*)
      INTEGER*4     CPSDIM
      CHARACTER*(*) ERRMSG
      INTEGER*4     IERR
C.======================================================================
C.    PURPOSE
C     Slfout                                                        SS<<
C     Subroutine to compute fast FOUrier TRAnsforms
C     of any number of sequences. ( Only the positive part is output ).
C     Number of sequences are given by NSEQU.
C     Start of (sub)-sequence N is given by INTPTR(N),
C     dimension of INTPTR is PTRDIM.
C     points of (sub)-sequence N is given by INTPTS (N),
C     dimension of INTPTS is PTSDIM.
C     As working space we have array WRKBUF with dimension BUFDIM,
C     BUFDIM have to be twice the length of the fourier transform
C     of the longest sequence.
C     The resulting positive FFT's are stored in the
C     one dimensional complex array CFFT with dimension CFTDIM. The
C     positive FFT's are the NFFT/2 + 1 first samples of
C     the total FFT.
C     The first sample of the positive FFT of sequence N is given
C     by CFTPTR (N). Dimension of CFTPTR is CPRDIM.
C     Number of samples of the positive FFT of sequence N is given
C     by CFTPTS (N). Dimension of CFTPTS is CPSDIM.
C     Any reported mistakes is returned through ERRMSG and
C     type of error is given by IERR
C
C
C   Applications so far: Wide-Band Slowness Analysis.
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C
C..   CHNDAT(*)    - Array containing input sequences.
C..   NSEQU        - Number of sequences. ( Channels ).
C..   INTPTR(*)    - Start of (sub)-sequence N in CHNDAT
C..   PTRDIM       - Dimension of array INTPTR.
C..   INTPTS(*)      (sub)-sequence.
C..   PTSDIM       - Dimension of array INTPTS.
C..   WRKBUF(*)    - Working array
C..   BUFDIM       - Dimension of working array WRKBUF.
C
C
C.    OUTPUT
C..   CFFT(*)      - Output array containing positive
C                                  FFT's of all sequences.
C..   CFTDIM       - Dimension of array CFFT
C..   CFTPTR(*)    - Pointer array to start of positive
C                                  FFT's in CFFT.
C..   CPRDIM       - Dimension of array CFTPTR.
C..   CFTPTS(*)    - Number of points for each positive
C                                  FFT.
C..   CPSDIM       - Dimension of array CFTPTS.
C..   ERRMSG       - Error message
C..   IERR         - IERR =  0 : Everything O.K.
C.                 - IERR =  1 : Warning, calculation
C                                continues.
C.                 - IERR = -1 : Severe error,
C                                calculation stopped and
C                                return from subroutine.
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Tormod Kvaerna
C.    CREATION_DATE 
C.    MADE_AT       NTNF/NORSAR
C                   Pb. 51
C                   N-2007 Kjeller
C
C.    MODIFICATION  August 15, 1986
C.    CORRECTION
C.======================================================================
C.@PROCESS OPT(3)
C.OPT(3)
C----
 
C----
 
      INTEGER*4     PXI,PXR
 
C----
C   Initiate quality variables
C----
 
      IERR   = 0
 
C----
C   Some checks on array dimensions to avoid ABEND CODE
C----
 
C----
C   Check on array CHNPTR
C----
 
      IF (  NSEQU .GT. PTRDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array INTPTR is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array CHNPNT
C----
 
      IF (  NSEQU .GT. PTSDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array INTPTS is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array CFTPTR
C----
 
      IF (  NSEQU .GT. CPRDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array CFTPTR is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array CFTPTS
C----
 
      IF (  NSEQU .GT. CPSDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array CFTPTS is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Channel loop
C----
 
      DO 1 I = 1, NSEQU
 
C----
C   FFT size must be equal to or larger than the length of the
C   sequences
C----
 
         NFFT =  8
 
    2    CONTINUE
 
         IF ( NFFT .GE. INTPTS(I) ) GO TO    3
 
            NFFT = NFFT * 2
 
         GO TO    2
 
    3    CONTINUE

C----
C   Check on array WRKBUF
C----
 
         IF (  NFFT .GT. BUFDIM/2 ) THEN
 
           ERRMSG = '***>SLFOUT: Length of array WRKBUF is exceeded'
c          WRITE(ERRMSG(64:70),'(I6)') NFFT
 
            IERR = -1
 
            RETURN
 
         END IF
 
C----
C    Store fourier transforms in buffer
C----
 
         PXR =  1
         PXI =  PXR + BUFDIM/2
 
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT Zero WRKBUF'',
     +                                  2I6)') PXR,NFFT
         CALL CHZERO ( WRKBUF(PXR), NFFT )
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT Zero WRKBUF'',
     +                                  2I6)') PXI,NFFT
         CALL CHZERO ( WRKBUF(PXI), NFFT )
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT Move from DAT'',
     +                              I6,'' to WRKBUF'',2I6)')
     +                              INTPTR(I),PXR,INTPTS(I)
         CALL CHMOVE ( WRKBUF(PXR), CHNDAT( INTPTR(I) ), INTPTS(I) )
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT FFT WRKBUF'',
     +                              I6,'' R,I '',2I6)')
     +                              PXR,PXI,NFFT
         CALL SLFFT  ( WRKBUF(PXR), WRKBUF(PXI), NFFT, -1 )

C----
C   Create pointer arrays to CFFT
C----
 
         IF ( I .EQ. 1) THEN
 
            CFTPTR (I) = 1
 
         ELSE
 
            CFTPTR (I) = CFTPTR ( I - 1) + CFTPTS ( I - 1 )
 
         ENDIF
 
         CFTPTS (I) = NFFT / 2 + 1

C----
C   Check on array CFFT
C----
 
         IF (  CFTPTR(I)+CFTPTS(I) .GT. CFTDIM ) THEN
 
            ERRMSG = '***>SLFOUT: Length of array CFFT is exceeded'
 
            IERR = -1
 
            RETURN
 
         END IF
C---
C   Store positive part of FFT in CFFT
C---
 
         IF( TYPCTL.GT.1 ) WRITE(6,'(2X,''...> SLFOUT Store WRKBUF'',
     +                              2I6,'' to CFFT'',2I6)')
     +                              PXR,PXI,CFTPTR(I),CFTPTS(I)
 
         DO    4 J = 1, CFTPTS(I)
 
            CFFT ( CFTPTR (I) + J - 1 ) = CMPLX( WRKBUF (PXR + J - 1),
     %                                           WRKBUF (PXI + J - 1) )
 
    4    CONTINUE
 
    1 CONTINUE
 
C----
C   That's it|
C----
 
      RETURN
 
      END
      SUBROUTINE SLSUBI ( NSEQU  , CHNPTR , PTRDIM , CHNPTS , PTSDIM ,
     +                    START  , STRDIM , STOP   , STPDIM , INTDIM ,
     +                    INTPTR , INTPTS , DELTA  , ERRMSG , IERR   ,
     +                    TYPCTL )
      INTEGER*4     NSEQU
      INTEGER*4     CHNPTR(*)
      INTEGER*4     PTRDIM
      INTEGER*4     CHNPTS(*)
      REAL*4        START (*)
      INTEGER*4     PTSDIM
      INTEGER*4     STRDIM
      REAL*4        STOP  (*)
      REAL*4        DELTA  (*)
      INTEGER*4     STPDIM
      INTEGER*4     INTDIM
      INTEGER*4     INTPTR(*)
      INTEGER*4     INTPTS(*)
      CHARACTER*(*) ERRMSG
      INTEGER*4     IERR  , TYPCTL
C.======================================================================
C.    PURPOSE
C     Slsubi                                                          SS<<
C     Subroutine to find first physical element of a sequence
C     sub-interval given the start and stop of each sequence.
C     Number of sequences is NSEQU
C     The first sample of sequence N is given by CHNPTR (N),
C     dimension of CHNPTR is PTRDIM.
C     Number of sampels of sequence N is given by CHNPTS (N),
C     dimension of CHNPTS is PTSDIM.
C     The first sample of sub-sequence N in sequence N is given by
C     START(N) with dimension STRDIM.
C     The last sample of sub-sequence N in sequence N is given by
C     STOP(N) with dimension STPDIM.
C     The output of this subroutine is the physical pointers to
C     each sub-sequence given by INTPTR(N).  The dimension of INTPTR
C     is INTDIM.
C     points of each sub-sequence is INTPTS(N).  The dimension
C     if INTPTS is also INTDIM
C     Sampling interval is given by DELTA
C     By physical pointers we mean pointers to the one-dimensional
C     array where the sequences are physically stored.
C     Any reported mistakes is returned through ERRMSG and
C     type of error is given by IERR
C
C
C     Applications so far: Preparation of input to subroutine FOUTRA
C
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C..   NSEQU      - Number of sequences
C..   CHNPTR(*)  - Array containing pointer to start
C                     of sequence N.
C..   PTRDIM     - Dimension of array CHNPTR.
C..   CHNPTS(*)  - Array containing number of samples
C                     of sequence N.
C..   PTSDIM     - Dimension of array CHNPTS.
C..   START (*)  - Pointer array to start of each
C                     sub-sequence, referenced to start
C                     of each sequence.
C..   STRDIM     - Dimension of array START
C..   STOP  (*)  - Pointer array to stop  of each
C                     sub-sequence, referenced to start
C                     of each sequence.
C
C..   DELTA  (*) - Sampling interval for channel N
C..   STPDIM     - Dimension of array START
C..   INTDIM     - Dimension of array INTPTR.
C
C
C
C
C.    OUTPUT
C..   INTPTR(*)  - Pointer array containing physical
C                     pointers to each sub-sequence
C                     referenced to start of the one-
C                     dimensional data array.
C
C..   INTPTS(*)  - Array containing points
C                     of each sub-sequence.
C
C..   ERRMSG     - Error message
C
C..   IERR  , TYPCTL            - IERR =  0 : Everything O.K.
C.                               - IERR =  1 : Warning, calculation
C                                                continues.
C.                               - IERR = -1 : Severe error,
C                                              calculation stopped and
C                                              return from subroutine.
C..   IRC     - Return code
C             = 0  No error
C             = ?  Fatal error
C.----------------------------------------------------------------------
C.    PROGRAMMER    Tormod Kvaerna
C.    CREATION_DATE December 2, 1986
C.    MADE_AT       NTNF/NORSAR
C                   Pb. 51
C                   N-2007 Kjeller
C
C.    MODIFICATION  October  1988 Jan Fyen
C.    CORRECTION
C.======================================================================
C.@PROCESS OPT(3)
C.OPT(3)
 
C----
C   Initiate quality variables
C----
 
      IERR   = 0
 
C----
C   Some checks on array dimensions to avoid ABEND CODE
C----
 
C----
C   Check on array CHNPTR
C----
 
      IF (  NSEQU .GT. PTRDIM ) THEN
 
         ERRMSG = '***>SLSUBI: Length of array CHNPTR is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array CHNPTS
C----
 
      IF (  NSEQU .GT. PTSDIM ) THEN
 
         ERRMSG = '***>SLSUBI: Length of array CHNPTS is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array START
C----
 
      IF (  NSEQU .GT. STRDIM ) THEN
 
         ERRMSG = '***>SLSUBI: Length of array START is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array STOP
C----
 
      IF (  NSEQU .GT. STPDIM ) THEN
 
         ERRMSG = '***>SLSUBI: Length of array STOP is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array INTPTR
C----
 
      IF (  NSEQU .GT. INTDIM ) THEN
 
         ERRMSG = '***>SLSUBI: Length of array INTPTR is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Channel loop
C----
 
      DO 1 I = 1, NSEQU
 
         ISAMP      = 0
         SMPFRQ     = 1.0 / DELTA(I)
         ISAMP      = NINT(START(I)*SMPFRQ)
         INTPTR (I) = CHNPTR(I) + ISAMP
         ISAMP      = 0
         ISAMP      = NINT(STOP(I)*SMPFRQ)
         NRIGHT     = CHNPTR(I) + ISAMP
         INTPTS (I) = NRIGHT - INTPTR(I)
 
         IF( TYPCTL.GT.2 ) THEN
             WRITE(6,'(2X,''...> SLSUBI'', I3,3F9.3,2I6)')
     +                 I,START(I),STOP(I),SMPFRQ,INTPTR(I),INTPTS(I)
         END IF
C----
C   Check that start of sub-sequence is within sequence, return
C   with warning.
C----
 
         IF ( INTPTR(I) .GT. CHNPTR(I) + CHNPTS(I) - 1 ) THEN
 
            ERRMSG = '...>SLSUBI: Start sub-sequence exceeeds sequence'
 
            IERR = 1
 
         ENDIF
 
    1 CONTINUE
 
C----
C   That's it|
C----
 
      RETURN
 
      END
C.@PROCESS OPT(3)
      SUBROUTINE CHMOVE (A, B, N)
      INTEGER*4  N
      REAL*4     A(*),B(*)
C.======================================================================
C.    PURPOSE
C     Make copy of an array. Move from  B to A.                     CH<<
C.----------------------------------------------------------------------
C.    KEYWORDS  CHNDAT
C.----------------------------------------------------------------------
C.    PACKAGE  CHNxxx
C.    VISIBLE
C.    STANDARD_FORTRAN_77
C.    USE_ONLY
C.----------------------------------------------------------------------
C.    INPUT
C..   N  - Number of elements to move from B to A.
C..   B  - Array to copy.
C.    OUTPUT
C..   A  - Resulting copy of array B.
C.----------------------------------------------------------------------
C.    PROGRAMMER    Jan Fyen
C.    CREATION_DATE 21 Apr 1988
C.    MADE_AT  NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
 
C.OPT(3)
 
      DO  100 I = 1, N
 
         A(I) = B(I)
 
100   CONTINUE
 
      RETURN
 
      END
      SUBROUTINE CHZERO (A, N)
      INTEGER*4  N
      REAL*4     A(*)
C.======================================================================
C.    PURPOSE
C     Zero one array.                                               CH<<
C.----------------------------------------------------------------------
C.    KEYWORDS  CHNDAT
C.----------------------------------------------------------------------
C.    PACKAGE  CHNxxx
C.    VISIBLE
C.    STANDARD_FORTRAN_77
C.    USE_ONLY
C.----------------------------------------------------------------------
C.    INPUT
C..   N  - Number of elements to zero in A.
C.    OUTPUT
C.----------------------------------------------------------------------
C.    PROGRAMMER    Jan Fyen
C.    CREATION_DATE 21 Apr 1988
C.    MADE_AT  NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
C.OPT(3)
 
 
      IF( N.LE.0 ) RETURN
 
      DO  100 I = 1, N
 
         A(I) = 0.0
 
100   CONTINUE
 
      RETURN
 
      END

c-----------------------------------------------------------------
c
c
c
      SUBROUTINE CONTOR (IIYMX , IIXMX , ARRAY  , SLWMAX ,
     +                   SXMAX , SYMAX , AZIMUT , APPVEL,
     +                   POWER, LEVNUM, OUTLOW, OUTHIG,
     +                   YEAR, MONTH, DAY, HOUR, MINU, SECO,
     +                   NCHAN, IDENT, xdim, ydim, TMWIN)


C     *****************************                                                
C     ** CONTOUR PLOT SUBROUTINE **                                                
C     *****************************                                                
C                                                                               
C     THIS SUBROUTINE PLOTS 'NCLEV' CONTOURS EACH TIME IT IS CALLED             
c
c     routine uses whole screen in range -0.5 to 1.5
c     for contour plott routine uses span 0 to 1 with center 0.5
C                                                                               
C     ARRAY(IR,IC) = DATA INPUT ARRAY                                           
C     IR  CORRESPONDS TO ROWS FROM IYMN TO IYMX                                 
C     IC  CORRESPONDS TO COLUMNS FROM IXMN TO IXMX                              
C     CLEV(I) = THE CONTOUR LEVELS TO BE PLOTTED                                
C     PLEV(I) = THE NUMBERS WHICH ARE PRINTED ON THE PLOT. PLEV(I)              
C     CORRESPONDS WITH CLEV(I)                                                  
C     NCLEV = THE NUMBER OF LEVELS TO PLOT                                      
C     IIXMX = number of x elements                                     
C     IIYMY = number of y elements                                             
C     THE ROWS EXTEND IN THE HORIZONTAL DIRECTION, THE COLUMNS IN THE           
C     VERTICAL DIRECTION.                                                       
C
c      implicit none
C
C     Variables comming from upper programm
C
      INTEGER      IIYMAX, IIXMAX
      REAL*4        ARRAY(99,99)
      REAL         SXMAX, SYMAX       ! max slownes at max power 
      REAL*4       AZIMUT             ! azimut of max power
      REAL*4       APPVEL             ! app. vel of max power
      REAL*4       POWER              ! normalized max powere
      REAL         OUTLOW, OUTHIG     ! used frequency band
      CHARACTER    LEVNUM*1           ! indicator to plott level numbers
      INTEGER       YEAR,  MONTH, DAY,  HOUR
      INTEGER       MINU
      REAL          SECO, TMVIN
      CHARACTER    IDENT(40)*10
      INTEGER*4    NCHAN
      REAL         xdim, ydim

C----
C     Internal variables
C----

      INTEGER      idim
      integer ncon         ! unit to read contours from
c     parameter (idim=51)
      DIMENSION    ENDPT(10000), STPT(10000),                 
     *             XGRID(99)   , YGRID(99)  ,
     *             CLEV(40)    , PLEV(40) 
      INTEGER      STPT , ENDPT
      CHARACTER*12 ANOT             ! to write anotation and text on graph dis
      CHARACTER*60 TXTOUT           ! to write text on graph displ
      CHARACTER*10 xlev(40)         ! contour levels
    
      COMMON/AA/ IXMN,IXMX,IYMN,IYMX,NLX,NRX,NTY,NBY
C
C
      ixmx=iixmx  ! transfer since in common block
      iymx=iiymx 

c
c   fix contour levels to 0 to 1
c
       nclev=11
       do i=1,11
          clev(i)=(i-1)*0.1
          plev(i)=clev(i)
          xlev(i)=' '
          write(xlev(i)(1:3),'(f3.1)') clev(i)
       enddo
C
	  iymn=1
	  ixmn=1
c
c   set old versatec dimensions to somehting reasonble so it is easy to
c   scale afterwards, cannot be too big, scaling goes funny, scaling up
c   to seisan is done in routines plot and number.
c
c      xdim=1.3
c      ydim=1.3 
C                                                                               
C   INITIALIZE ARRAYS                                                           
C                                                                               
      IDIM=99                                                                 
C                                                                               
      KMAX=IDIM*IDIM                                                            
 80   DO 90 I=1,KMAX                                                            
      STPT(I)=0                                                                 
 90   ENDPT(I)=0                                                                
C                                                                               
C   COMPUTE QUADRANT SIZE AND GRID DISPLACEMENTS                                
C                                                                               
      XSCAL = XDIM/(IXMX-IXMN)                                                  
      YSCAL = YDIM/(IYMX-IYMN)                                                  
      DO 110 I=1,IDIM                                                           
      XGRID(I) = (I-1)* XSCAL                                                   
 110  YGRID(I) = (I-1)* YSCAL                                                   
      NLX = XGRID(IXMN) *100. + .5                                              
      NRX = XGRID(IXMX) *100. + .5                                              
      NTY = YGRID(IYMN) *100. + .5                                              
      NBY = YGRID(IYMX) *100. + .5                                              
      IXMN1 = IXMN + 1                                                          
      IYMN1 = IYMN + 1                                                          

C----
CAP   Plott box and zerrow axes 
C----
      call plot (0.0*xdim, 0.0*ydim, 3)
      call plot (1.0*xdim, 0.0*ydim, 2)
      call plot (1.0*xdim, 1.0*ydim, 2)
      call plot (0.0*xdim, 1.0*ydim, 2)
      call plot (0.0*xdim, 0.0*ydim, 2)
C
      call plot (0.5*xdim, 0.0*ydim, 3)
      call plot (0.5*xdim, 1.0*ydim, 2)
C
      call plot (0.0*xdim, 0.5*ydim, 3)
      call plot (1.0*xdim, 0.5*ydim, 2)
C----
CAP   Loop to draw ticks
C----
      DO i=(-1)*FLOAT(INT(SLWMAX*10)), FLOAT(INT(SLWMAX*10))
         call plot ((0.5/SLWMAX*i/10+0.5)*xdim, 0.0*ydim, 3)
         call plot ((0.5/SLWMAX*i/10+0.5)*xdim, 0.01*ydim, 2)
         call plot ((0.5/SLWMAX*i/10+0.5)*xdim, 1.0*ydim, 3)
         call plot ((0.5/SLWMAX*i/10+0.5)*xdim, 0.99*ydim, 2)
         call plot (0.0*xdim, (0.5/SLWMAX*i/10+0.5)*ydim, 3)
         call plot (0.005*xdim, (0.5/SLWMAX*i/10+0.5)*ydim, 2)
         call plot (1.0*xdim, (0.5/SLWMAX*i/10+0.5)*ydim, 3)
         call plot (0.995*xdim, (0.5/SLWMAX*i/10+0.5)*ydim, 2)
      ENDDO
C----
CAP   Write slownes values
C----
      IF ((SLWMAX-FLOAT(INT(SLWMAX*10))/10).eq.0.0) THEN
        WRITE (ANOT(1:4),'(f3.1)') SLWMAX        ! one decimal case
      ELSE
        WRITE (ANOT(1:4),'(f4.2)') SLWMAX        ! two decimal case
      ENDIF
C----
CAP   Plott slowness anotation
C----
c                                                 plott max slownes number
      call number (ANOT, 0.98*xdim, -0.06*ydim)
c      call number (ANOT, 0.98*xdim, 1.03*ydim)
      call number (ANOT, -0.08*xdim, 0.99*ydim)
c      call number (ANOT, 1.03*xdim, 0.99*ydim)
c
      ANOT='-'//ANOT                            ! plot min sloness number
      call number (ANOT, -0.08*xdim, -0.01*ydim)
c      call number (ANOT, 1.03*xdim, -0.01*ydim)
      call number (ANOT, -0.02*xdim, -0.06*ydim)
c      call number (ANOT, -0.02*xdim, 1.03*ydim)
c
      ANOT='0.0       '                         ! plot zerow slowness number
      call number (ANOT, 0.48*xdim, -0.06*ydim)
c      call number (ANOT, 0.48*xdim, 1.03*ydim)
      call number (ANOT, -0.08*xdim, 0.49*ydim)
c      call number (ANOT, 1.03*xdim, 0.49*ydim)
c                                                                               
c
      DO 700 J=1,NCLEV                                                          
C   LOOP ONCE FOR EACH DB CONTOUR  ***************M4                            
      K=0                                                                       
C                                                                               
C   TEST ALL SQUARES FOR DB LEVEL CROSSING                                  
C                                                                               
      DO 690 N=IYMN1,IYMX                                                       
      DO 690 I=IXMN1,IXMX                                                       
      PT1 = RATIO(ARRAY(N-1,I-1),ARRAY(N-1,I),XGRID(I-1),XGRID(I),              
     1CLEV(J))                                                                  
      PT2 = RATIO(ARRAY(N-1,I-1),ARRAY(N,I-1),YGRID(N-1),YGRID(N),              
     1CLEV(J))                                                                  
      PT3 = RATIO(ARRAY(N,I-1),ARRAY(N,I),XGRID(I-1),XGRID(I),CLEV(J))          
      PT4 = RATIO(ARRAY(N-1,I),ARRAY(N,I),YGRID(N-1),YGRID(N),CLEV(J))          
C                                                                               
C   TEST IF ANY SIDE HAS AN INTERSECTION                                        
C                                                                               
      IF (PT1+PT2+PT3+PT4) 450,690,450                                          
C                                                                               
C     TEST IF ALL SIDES HAVE INTERSECTIONS                                      
C                                                                               
 450  IF (PT1*PT2*PT3*PT4) 660,460,660                                          
C                                                                               
C     THIS IS THE TWO SIDE CASE - DETERMINE WHICH TWO                           
C                                                                               
 460  IF (PT1) 480,470,480                                                      
 470  IF (PT2) 500,560,500                                                      
 490  IF (PT3) 520,530,520                                                      
 480  IF (PT2) 510,490,510                                                      
 500  IF (PT3) 540,550,540                                                      
 510  LDIR=1                                                                    
      GO TO 570                                                                 
 520  LDIR=2                                                                    
      GO TO 570                                                                 
 530  LDIR=3                                                                    
      GO TO 570                                                                 
 540  LDIR=4                                                                    
      GO TO 570                                                                 
 550  LDIR=5                                                                    
      GO TO 570                                                                 
 560  LDIR=6                                                                    
C                                                                               
C                                                                               
C   ASSIGN LINE START COORDINATES BY LINE DIRECTION                             
 570  GO TO (580,580,580,590,590,600),LDIR                                      
 580  STX = PT1                                                                 
      STY = YGRID(N-1)                                                          
      GO TO 610                                                                 
 590  STX = XGRID(I-1)                                                          
      STY = PT2                                                                 
      GO TO 610                                                                 
 600  STX = PT3                                                                 
      STY = YGRID(N)                                                            
C                                                                               
C   ASSIGN LINE END COORDINATES BY LINE DIRECTION                               
C                                                                               
 610  GO TO (620,630,640,630,640,640),LDIR                                      
 620  ENDX = XGRID(I-1)                                                         
      ENDY = PT2                                                                
      GO TO 650                                                                 
 630  ENDX = PT3                                                                
      ENDY = YGRID(N)                                                           
      GO TO 650                                                                 
 640  ENDX = XGRID(I)                                                           
      ENDY = PT4                                                                
C                                                                               
C   OUTPUT LINE SEGMENT                                                         
C  
                                                                        
 650  CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K) = MSTRT                                                           
      ENDPT(K) = MEND                                                           
      IF (K-KMAX ) 690,790,790                                                  
C                                                                               
C   THIS IS THE FOUR SIDE CASE - DETERMINE MOST PROBABLE PAIR OF LINES          
C   USING DIAGONALS                                                             
C                                                                               
 660  IF (ABS(CLEV(J)-(ARRAY(N-1,I)+ARRAY(N,I-1))/2.)-ABS(CLEV(J)-              
     1(ARRAY(N-1,I-1)+ARRAY(N,I))/2.)) 680,670,670                              
C                                                                               
C   LINES DIRECTION PAIR 1 AND 6                                                
C                                                                               
 670  STX = PT1                                                                 
      STY = YGRID(N-1)                                                          
      ENDX = XGRID(I-1)                                                         
      ENDY = PT2                                                                
C                                                                               
C   OUTPUT LINE SEGMENT                                                         
C                                                                               
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K)= MSTRT                                                            
      ENDPT(K)= MEND                                                            
      IF (K-KMAX ) 675,790,790                                                  
 675  STX = PT3                                                                 
      STY = YGRID(N)                                                            
      ENDX = XGRID(I)                                                           
      ENDY = PT4                                                                
C                                                                               
C   OUTPUT LINE SEGMENT                                                         
C                                                                               
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K)=MSTRT                                                             
      ENDPT(K) = MEND                                                           
      IF (K-KMAX ) 690,790,790                                                  
C                                                                               
C   LINE DIRECTION PAIR 4 AND 3     
                                           
C                                                                               
 680  STX = PT1                                                                 
      STY = YGRID(N-1)                                                          
      ENDX = XGRID(I)                                                           
      ENDY = PT4                                                                
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K) = MSTRT                                                           
      ENDPT(K) = MEND                                                           
      IF (K-KMAX ) 685,790,790                                                  
 685  STX = XGRID(I-1)                                                          
      STY = PT2                                                                 
      ENDX = PT3                                                                
      ENDY = YGRID(N)                                                           
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K)=MSTRT                                                             
      ENDPT(K)=MEND                                                             
      IF (K-KMAX ) 690,790,790                                                  
 690  CONTINUE                                                                  
C                                                                               
C   ANY DATA THIS LEVEL                                                         
C                                                                               
C     IF (K) 810,810,9100                                               DBUG    
      IF (K) 810,810,910                                                        
 810  continue
c      WRITE(6,140) PLEV(J)                                                      
c 140  FORMAT(' NO DATA FOR DB LEVEL ',F8.4)                                     
      GO TO 700                                                                 
C                                                                               
C   CONNECT POINTS AND PLOT                                                     
C                                                                               
C9100 WRITE(3,9101) J                                                   DBUG    
C9101 FORMAT('1DATA FOR LEVEL',I5/)                                     DBUG    
C     WRITE(3,9102) (STPT(IKJ),ENDPT(IKJ),IKJ=1,K)                      DBUG    
C9102 FORMAT(1X,5(2I11,3X))                                             DBUG    
 910  NN=1                                                                      
C                                                                               
C   DOES START OR END POINT START ON THE EDGE                                   
C                                                                               
 915  IST = 0                                                                   
      MX = MAX0(NN-1,K-NN)                                                      
      DO 925 LL=1,MX                                                            
      L=NN + LL                                                                 
      IF (L-K) 916,916,920                                                      
 916  IF (STPT(L)) 930,920,917                                                  
 917  IF (ENDPT(L)) 950,918,918                                                 
 918  IF (IST) 920,919,920                                                      
 919  IST=L                                                                     
 920  L = NN - LL                                                               
      IF (L-1) 925,921,921                                                      
 921  IF (STPT(L)) 930,925,922                                                  
 922  IF (ENDPT(L)) 950,923,923                                                 
 923  IF (IST) 925,924,925                                                      
 924  IST = L                                                                   
 925  CONTINUE                                                                  
C                                                                               
C   ANY MORE DATA VALUES LEFT                                                   
C                                                                               
      IF (IST) 926,700,926                                                      
 926  L = IST                                                                   
      NIS = STPT(L)                                                             
C                                                                               
C   BEGIN ON START EDGE                                                         
C                                                                               
 930  STPT(L) = IABS(STPT(L))                                                   
      CALL PT(STPT(L),XS,YS)                                                    
C     WRITE(3,9103) L,XS,YS                                             DBUG    
C9103 FORMAT(' START ',I5,2(2X,F8.2))                                   DBUG    
C  JF  CHANGES MADE FOR THE SUBMATRIX CASE                                      
 2133 IF(IXMN-1) 790, 2001, 2000                                                
 2000 XS=XS-(IXMN-1)*XSCAL                                                      
 2001 IF(IYMN-1) 790, 2003, 2002                                                
 2002 YS=YS+(IYMN-1)*YSCAL                                                      
 2003 CALL PLOT(XS,YS,3)                                                        
      XMX = XS                                                                  
      YMY = YS                                                                  
C                                                                               
C                                                                               
C   CONTINUE ON START VALUES                                                    
C                                                                               
 940  LAST = ENDPT(L)                                                           
      GO TO 970                                                                 
C                                                                               
C   BEGIN ON END EDGE                                                           
C                                                                               
 950  ENDPT(L) = IABS(ENDPT(L))                                                 
      CALL PT(ENDPT(L),XS,YS)                                                   
C     WRITE(3,9103) L,XS,YS                                             DBUG    
 2134 IF(IXMN-1) 790, 2005, 2004                                                
 2004 XS=XS-(IXMN-1)*XSCAL                                                      
 2005 IF(IYMN-1) 790, 2007, 2006                                                
 2006 YS=YS+(IYMN-1)*YSCAL                                                      
 2007 CALL PLOT(XS,YS,3)                                                        
      XMX = XS                                                                  
      YMY = YS                                                                  
C                                                                               
C   CONTINUE ON END VALUES                                                      
C                                                                               
 960  LAST = STPT(L)                                                            
 970  STPT(L) = 0                                                               
      ENDPT(L) = 0                                                              
C                                                                               
C   IS THIS THE LAST VALUE                                                      
C                                                                               
      IF (LAST) 975,975,980                                                     
 975  LAST = IABS(LAST)                                                         
      CALL PT(LAST,XS,YS)                                                       
C     WRITE(3,9104) L,XS,YS                                             DBUG    
C9104 FORMAT('   END ',I5,2(2X,F8.2))                                   DBUG    
 2135 IF(IXMN-1) 790, 2009, 2008                                                
 2008 XS=XS-(IXMN-1)*XSCAL                                                      
 2009 IF(IYMN-1) 790, 2011, 2010                                                
 2010 YS=YS+(IYMN-1)*YSCAL                                                      
 2011 CALL PLOT(XS,YS,2)                                                        
      NN = L                                                                    
      IF (YMY - YS) 976,1010,1010                                               
 976  XMX = XS                                                                  
      YMY = YS                                                                  
C   START ON NEW POINT AFTER LABEL ROUTINE                                      
C                                                                               
C   START ON NEW POINT AFTER LABEL ROUTINE                                      
C                                                                               
      GO TO 1010                                                                
C                                                                               
C   CONTINUE IN ROUTINE                                                         
 980  CALL PT(LAST,XS,YS)                                                       
C     WRITE(3,9105) L,XS,YS                                             DBUG    
C9105 FORMAT('       ',I5,2(2X,F8.2))                                   DBUG    
 2136 IF(IXMN-1) 790, 2013, 2012                                                
 2012 XS=XS-(IXMN-1)*XSCAL                                                      
 2013 IF(IYMN-1) 790, 2015, 2014                                                
 2014 YS=YS+(IYMN-1)*YSCAL                                                      
 2015 CALL PLOT(XS,YS,2)                                                        
      IF (YMY-YS) 981,985,985                                                   
 981  XMX = XS                                                                  
      YMY = YS                                                                  
 985  MX = MAX0(L-1,K-L)                                                        
      NN = L                                                                    
      DO 1000 LL=1,MX                                                           
      L = NN + LL                                                               
      IF (L-K)  986,986,990                                                     
 986  IF (STPT(L)) 987,990,987                                                  
 987  IF (STPT(L)-LAST) 988,940,988                                             
 988  IF (ENDPT(L)-LAST) 990,960,990                                            
 990  L = NN - LL                                                               
      IF (L-1) 1000,991,991                                                     
 991  IF (STPT(L)) 992,1000,992                                                 
 992  IF (STPT(L)-LAST) 993,940,993                                             
 993  IF (ENDPT(L)-LAST) 1000,960,1000                                          
 1000 CONTINUE                                                                  
      CALL PT(NIS,XS,YS)                                                        
C     WRITE(3,9104) L,XS,YS                                             DBUG    
 2137 IF(IXMN-1) 790, 2017, 2016                                                
 2016 XS=XS-(IXMN-1)*XSCAL                                                      
 2017 IF(IYMN-1) 790, 2019, 2018                                                
 2018 YS=YS+(IYMN-1)*YSCAL                                                      
 2019 CALL PLOT(XS,YS,2)                                                        
C     CALL PLOT (XS,YS,2)                                                       
C                                                                               
C   LABEL HIGHEST POINT ON CONTOUR                                              
C                                                                               
 1010 CRT = PLEV(J)                                                             
c
c   here is some number displacemnts which is assumed to be in inches
c   since whole plot is now from 0 to 1 in both directions, must be changed
c   somehow. for the time being is just disabled
c
c     ADD=0.02
      IF(PLEV(J)-10.0) 4000,4001,4001                                           
 4000 ADD=0.1                                                                   
      GO TO 4002                                                                
 4001 ADD=0.15                                                                  
 4002 YMY=YMY+0.02                                                              
c     XMX=XMX-ADD                                                               
c      CALL NUMBER (XMX,YMY,.1,CRT,0.,-1)

      IF (LEVNUM.EQ.'y'.or.LEVNUM.EQ.'Y') THEN
        call number(xlev(j),xmx,ymy)  
        GOTO 915
      ENDIF
      GO TO 915                                                                 
C
 790  WRITE ( 3,8100)
 8100 FORMAT(' TOO MANY POINTS, THIS LEVEL IGNORED')
c
 700  CONTINUE                                                                  

C----
CAP   Marks maximum power
C----
      call plot ((0.5/SLWMAX*SXMAX-0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX-0.007+0.5)*ydim, 3)
      call plot ((0.5/SLWMAX*SXMAX+0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX+0.007+0.5)*ydim, 2)
      call plot ((0.5/SLWMAX*SXMAX-0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX+0.007+0.5)*ydim, 3)
      call plot ((0.5/SLWMAX*SXMAX+0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX-0.007+0.5)*ydim, 2)
      call plot ((0.5/SLWMAX*SXMAX+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX+0.007+0.5)*ydim, 3)
      call plot ((0.5/SLWMAX*SXMAX+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX-0.007+0.5)*ydim, 2)

      call plot ((0.5/SLWMAX*SXMAX+0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX+0.007+0.5)*ydim, 3)
      call plot ((0.5/SLWMAX*SXMAX-0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX+0.007+0.5)*ydim, 2)
      call plot ((0.5/SLWMAX*SXMAX-0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX-0.007+0.5)*ydim, 2)
      call plot ((0.5/SLWMAX*SXMAX+0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX-0.007+0.5)*ydim, 2)
      call plot ((0.5/SLWMAX*SXMAX+0.007+0.5)*xdim,
     + (0.5/SLWMAX*SYMAX+0.007+0.5)*ydim, 2)

C----
CAP   DRAW line from origin to power max in case LEVNUM='N'
C----
      IF (LEVNUM.EQ.'N') THEN
        call plot (0.5*xdim, 0.5*ydim, 3)
        call plot ((0.5/SLWMAX*SXMAX+0.5)*xdim,
     +    (0.5/SLWMAX*SYMAX+0.5)*ydim, 2)
      ENDIF

C----
CAP  Write date and time and time window
C----
      ANOT='          '
      WRITE (ANOT(1:4), '(A4)') 'Date'

      call contxchar ('DATE',4, -0.45, 1.4)
      ANOT='          '
      WRITE (ANOT(1:4),'(I4)') YEAR
      WRITE (ANOT(5:5),'(a1)') '.'
      WRITE (ANOT(6:7),'(I2)') MONTH
      WRITE (ANOT(8:8),'(a1)') '.'
      WRITE (ANOT(9:10),'(I2)') DAY
      call contxchar (ANOT,10, -0.30, 1.4)

      call contxchar ('TIME',4, 0.0, 1.4)
      ANOT='          '
      WRITE (ANOT(1:2),'(I2)') HOUR
      WRITE (ANOT(3:3),'(a1)') ':'
      WRITE (ANOT(4:5),'(I2)') MINU
      WRITE (ANOT(6:6),'(a1)') ':'
      WRITE (ANOT(7:12),'(F6.3)') SECO
      call contxchar (ANOT,12, 0.15, 1.4)

      call contxchar ('WINDOW LENGTH, SEC',18, 0.5, 1.4)
      ANOT='          '
      WRITE (ANOT(1:8),'(F8.3)') TMWIN
      call contxchar (ANOT, 8, 0.9, 1.4)

C---
CAP  Write id of used chanels
C----

      call number ('CHAN. USED', -0.45, 1.30)

      DO n=1, NCHAN
        ANOT='          '
        WRITE (ANOT(1:10),'(a10)') IDENT(n)
        call number (ANOT, -0.45, 1.25-n*0.05)
      ENDDO

C----
CAP*  Write output parameters of F-K analysis
C----

      call contxchar ('AZIMUTH',7, -0.25, -0.2)
      ANOT='          '
      WRITE (ANOT(1:3),'(I3)') INT(AZIMUT)
      call contxchar (ANOT,3, 0.25, -0.2)
      call contxchar('APPARENT VELOCITY',17,-0.25,-0.25)
      ANOT='          '
      WRITE (ANOT(1:4),'(F4.1)') APPVEL
      call contxchar (ANOT,4, 0.25, -0.25)
      call contxchar('NORM. POWER MAX',15,-0.25, -0.3)
      ANOT='          '
      WRITE (ANOT(1:4),'(F4.2)') POWER
      call contxchar (ANOT,4,  0.25, -0.3)
                                            
      call contxchar('LOW FREQUENCY',13,0.65, -0.2)
      ANOT='          '
      WRITE (ANOT(1:5),'(F5.2)') OUTLOW
      call contxchar (ANOT,5, 1.10, -0.2)
      call contxchar('HIGH FREQUENCY',14, 0.65, -0.25)
      ANOT='          '
      WRITE (ANOT(1:5),'(F5.2)') OUTHIG
      call contxchar (ANOT,5, 1.10, -0.25)
      call contxchar('NO OF GRID POINTS',17, 0.65, -0.3)
      ANOT='          '
      WRITE (ANOT(1:2),'(I2)') IIXMX
      call contxchar (ANOT,2, 1.10, -0.3)

c----
cAP  Write on screen only meanings of keys
c----
      call contxmess ('R-Redo',1,6, -0.35,-0.45)
      call contxmess ('M-Mouse',1,7,0.10,-0.45)
      call contxmess ('S-Save and quit',1,15, 0.50,-0.45)
      call contxmess ('Q-Quit',1, 6, 1.0, -0.45)
                          
c
c    Return to Caller...
c    ===================
c

 9999  continue

      return
      END                                                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine number(xnumber,x,y)
c
c  plot a number xnumber at position x,y
c
      implicit none
      character*12 xnumber      ! the text string to plot with the number
      character*90 text
      real xx,yy          ! to projection routine
      real x,y
      text=' '
      text(1:12)=xnumber

      xx=(x+0.5)*520.0                                                           
      yy=(y+0.5)*390.0
   	  call xchars(text,10,xx,yy)
      return
      end

c-----------------------------------------

      subroutine contxchar (outtxt,n,x,y)

c---
cAP   plot some text of i lines and n characters at coordinates x y  
c     x and y ranges from -0.5 to 1.5 according to subrautine contor
c---  

      implicit none
      character*60 outtxt    ! the text string to plot
      character*90 text
      real xx,yy             ! to projection routine
      real x,y
      integer n
      text=' '
      text(1:60)=outtxt

      xx=(x+0.5)*520.0                                                           
      yy=(y+0.5)*390.0
          call xchars (text,n,xx,yy)
      return
      end
c-----------------------------------------

      subroutine contxmess (outtxt,i,n,x,y)

c---
cAP   plot some text of i lines and n characters at coordinates x y on
c     screen only
c     x and y ranges from -0.5 to 1.5 according to subrautine contor
c---  

      implicit none
      character*60 outtxt    ! the text string to plot
      character*80 text
      real xx,yy             ! to projection routine
      real x,y
      integer i, n
      text=' '
      text(1:60)=outtxt

      xx=(x+0.5)*520.0                                                           
      yy=(y+0.5)*390.0
          call xmessage (text,i,n,xx,yy)
      return
      end


c-----------------------------------------

      subroutine plot(x,y,n)
c
c  x and y  : input values to plot
c  n        : 2: pen down  3: pen up
c
c  used with contor routine to use verstec calls and do the scaling and
c  and map projection
c
      real xx,yy          ! to projection routine
      real x,y
      integer n,nold,nn         ! pen up or down
c
       nn=n
          if(nold.eq.3) then
            nn=3
            nold=2  ! draw to next time
          endif

c       else
c          nold=3    ! move to for next value inside
c          return    ! point is outside, do not plot
c       endif

        xx=(x+0.5)*520.0                                                           
        yy=(y+0.5)*390.0

        if(nn.eq.3) call xmovabs(xx,yy)
        if(nn.eq.2) call xdrwabs(xx,yy)
        return
        end

c--------------------------------------------------

      SUBROUTINE PUT (STX,STY,ENDX,ENDY,MSTRT,MEND)                             
C                                                                               
      COMMON/AA/ IXMN,IXMX,IYMN,IYMX,NLX,NRX,NTY,NBY                            
      MSTRTX = STX*100. + .5                                                    
      MSTRTY = STY*100. + .5                                                    
      MENDX = ENDX*100. + .5                                                    
      MENDY = ENDY*100. + .5                                                    
      MSTRT = MSTRTX*100000 + MSTRTY                                            
      MEND = MENDX*100000 + MENDY                                               
      IF (MSTRT-MEND) 5,200,5                                                   
 5    IF(MSTRTX-NLX) 10,40,10                                                   
 10   IF(MSTRTY-NTY) 20,40,20                                                   
 20   IF(MSTRTY-NBY) 30,40,30                                                   
 30   IF(MSTRTX-NRX) 50,40,50                                                   
 40   MSTRT = - MSTRT                                                           
 50   IF(MENDX-NRX) 60,90,60                                                    
 60   IF(MENDX-NLX) 70,90,70                                                    
 70   IF(MENDY-NBY) 80,90,80                                                    
 80   IF(MENDY-NTY) 100,90,100                                                  
 90   MEND = - MEND                                                             
 100  RETURN                                                                    
 200  MEND=0                                                                    
      MSTRT=0                                                                   
      RETURN                                                                    
      END                                                                       
C                                                                               
      FUNCTION RATIO(A,B,C,D,VAL)                                               
C                                                                               
      IF(A-VAL) 30,20,20                                                        
 20   IF(B-VAL) 50,60,60                                                        
 30   IF(B-VAL) 60,50,50                                                        
 50   RATIO = ((VAL-A)/(B-A))*(D-C) + C                                         
      RETURN                                                                    
 60   RATIO = 0                                                                 
      RETURN                                                                    
C                                                                               
C                                                                               
C  ORIGINAL 'RATIO'                                                             
C. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .          
C     ABIAS=0                                                                   
C     BBIAS=0                                                                   
C     IF (A-VAL) 30,10,20                                                       
C10   ABIAS =  .00001                                                           
C20   IF (B-VAL) 50,60,60                                                       
C40   BBIAS = .00001                                                            
C30   IF (B-VAL) 60,40,50                                                       
C50   RATIO = ((VAL-A-ABIAS)/(BBIAS+B-A-ABIAS))*(D-C)+C                         
C     GO TO 70                                                                  
C60   RATIO = 0                                                                 
C70   RETURN                                                                    
      END                                                                       
C
      SUBROUTINE PT(IUV,X,Y)                                                    
C                                                                               
      X   = IUV/100000 * .01                                                   
      Y    = MOD(IUV,100000) * .01                                              
cjh       Y = - Y                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                               
