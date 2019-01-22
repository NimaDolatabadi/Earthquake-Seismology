CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     PROGRAM EXFILTER
C
C     PROGRAM TO FLAG PROBABLE EXPLOSIONS IN A STANDARD
C     COMPRESSED FORMAT FILE, WRITTEN BY CONRAD D. LINDHOLM. JAN. 1991
C     IF A PROBABLE EXPLOSION IS FOUND, A 'P'
C     IS WRITTEN IN COLUMN 2. KNOWN EXPLOSIONS ARE NOT TOUCHED.
C
C     CRITICAL VALUES:
C     LHOUR,HHOUR:  HOUR-LIMITS
C     LMAG       :  LOWER MAGNITUDE FOR REAL EARTHQUAKES
C     LLAT,HLAT  :  LATITUDE-LIMITS
C     LLON,HLON  :  LONGITUDE-LIMITS
C     EXP        :  EXPLOSION INDICATOR
C     MAXDEPTH   :  MAXIMUM DEPTH POSSIBLE FOR AN EXPLOSION
C     NAREAS     :  NUMBER OF AREAS TO FILTER
C     NEXEPTIONS :  NUMBER OF AREAS EXCEPTED FROM THE FILTER
C
C     "E" IS PRECEEDING PARAMETERS FOR THE EXCEPTED REGIONS.
C     PRESENTLY THE EXCEPTION REGIONS ARE ONLY TESTED FOR
C     LATITUDE LONGITUDE AND YEAR AND MONTH OF VALIDITY.
C
C     8 HOURS ARE SUBTRACTED TO GO FROM GMT TO LOCAL TIME
C
C     PARAMETER INPUT FILE:     "~/seismo/DAT/EXFILTER.AREA"
c     Last update:
c
C 24.03.93 by K.Atakan :   new indata routine added
c 07 7 93  jh          :   verison 3.0 (new indata)
c sept. 95 jh          :   new program polygons by Lars Ottemuller
c aug 96   jh          :   use sei get reading routine
c                      :   put in sei get routine for file open
c sep 96   lo          :   use nortype to get file type 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c mar 99 lo : -----------version 7.0 ----------------------
c mar 18 99 lo dont filter events with fixed depth or star events
c oct 23 00 lo ------ events marked as quakes (q in col 23)
c jan 31 17 jh increase dimensions
c
      include 'seidim.inc'        ! dimensions
      include 'libsei.inc'
c
      character*1 ctype,cexp
      CHARACTER*80 DATA(10000)
      CHARACTER CARD*80
      CHARACTER PARAMFILE*80,filename*80
      LOGICAL EXP
      integer read1, code     ! unit to read parameter file from and er. code
      INTEGER LHOUR(500),HHOUR(500),TIME1(500),TIME2(500),NAREAS
      REAL LMAG(500),LLAT(500),LLON(500),HLAT(500),HLON(500)
      REAL MAXDEPTH(500)
      integer nprob    ! number of probable explosions found
      REAL ELMAG(500),ELLAT(500),ELLON(500),EHLAT(500),EHLON(500)
      INTEGER ELHOUR(500),EHHOUR(500),ETIME1(500),ETIME2(500)
      integer nexeptions
      real ypol(500,500,2)    ! array defining polygon
      integer npoints(500)
      REAL EMAXDEPTH(500)
      COMMON/A3/READ1
      COMMON/A2/ELMAG,ELLAT,ELLON,ELHOUR,EHLAT,EHLON,
     *          EHHOUR,ENAREAS,ETIME1,ETIME2,EMAXDEPTH,
     *          NEXEPTIONS
      COMMON/A1/LMAG,LLAT,LLON,LHOUR,HLAT,HLON,
     *          HHOUR,NAREAS,TIME1,TIME2,MAXDEPTH
      common/a4/npoints,ypol
c format compact or Nordic
      LOGICAL compact



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   get parameter file

        call sei get file( open$,             ! Open parameter  file.
     &                     read1,             ! On unit.
     &                     code,              ! Returned condition.
     &                     'DAT',             ! Alternative search directory.
     &                     'EXFILTER.PAR' )   ! For this filename.

      nprob=0
C
C---- Read the filter parameters from parameter file
C
      call get_areas
c
c     write(6,*) (npoints(i),i=1,nareas)
c     write(6,*) ypol
      WRITE(6,*)'NUMBER OF AREAS:',NAREAS
      WRITE(6,*)'     '
c     WRITE(6,*)' Two possible input formats:'
c     WRITE(6,*)' 1. Compressed Nordic format'
c     WRITE(6,*)' 2. Standard Nordic format'
c     WRITE(6,*)' What format has your file? (1/2)'
c     READ(5,*)NFORMAT
      WRITE(6,*)' FILENAME... ?'
      READ(5,'(A)')FILENAME
      OPEN(1,FILE=FILENAME,STATUS='OLD')
      call nortype(1,compact)
      if (compact) THEN
        NFORMAT=1
      ELSE 
        NFORMAT=2
      ENDIF
      CLOSE(1)
      OPEN(1,FILE=FILENAME,STATUS='OLD')
      OPEN(2,FILE='exfilter.out',STATUS='unknown')
C
C----- Do the filtering
C
10    IF(NFORMAT.EQ.2)THEN
        call indata(1,nstat,nphase,nhead,nrecord,ctype,cexp,data,k)
        IF(NRECORD.EQ.0) GO TO 20
        CALL FILTER(DATA(1),EXP)
        IF (EXP.AND.DATA(1)(23:23).NE.'E'.AND.DATA(1)(23:23).NE.'Q') 
     &      then 
           nprob=nprob+1
           DATA(1)(23:23)='P'
        endif
        DO I = 1,NRECORD
          WRITE(2,'(A80)')DATA(I)
        ENDDO
      ELSEIF(NFORMAT.EQ.1)THEN
        READ(1,'(A80)',END=20)CARD
        CALL FILTER(CARD,EXP)
        IF (EXP.AND.CARD(23:23).NE.'E') then 
          CARD(23:23)='P'
          nprob=nprob+1
        endif
        WRITE(2,'(A80)')CARD
      ENDIF

      GO TO 10


20    CONTINUE

      write(*,*)' ************************************'
      write(*,*)' Number of probable explosions found:', nprob
      write(*,*)' Output written on file: exfilter.out'
      write(*,*)' ************************************'

      END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE FILTER(CARD,EXP)
C
C     EXP   : EXPLOSION INDICATOR
C
      include 'seidim.inc'
      REAL LAT,LON,DEPTH,MAG,amag,bmag,cmag
      INTEGER HOUR
      LOGICAL EXP
      CHARACTER CARD*80
      CHARACTER*1 NS,EW
      INTEGER LATDEG,LONDEG,LATMIN,LONMIN
      INTEGER LHOUR(500),HHOUR(500),TIME1(500),TIME2(500),NAREAS
      INTEGER YEAR,MONTH,TIME
      REAL LMAG(500),LLAT(500),LLON(500),HLAT(500),HLON(500)
      REAL MAXDEPTH(500)
      REAL ELMAG(500),ELLAT(500),ELLON(500),EHLAT(500),EHLON(500)
      INTEGER ELHOUR(500),EHHOUR(500),ETIME1(500),ETIME2(500)
      integer nexeptions
      REAL EMAXDEPTH(500)
      real ypol(500,500,2)    ! array defining polygon
      real yypol(max_polyn$,2)      ! -------------one-----
      integer npoints(500)   
      logical insidepol         ! true if point inside polygon
      COMMON/A2/ELMAG,ELLAT,ELLON,ELHOUR,EHLAT,EHLON,
     *          EHHOUR,ENAREAS,ETIME1,ETIME2,EMAXDEPTH,
     *          NEXEPTIONS
      COMMON/A1/LMAG,LLAT,LLON,LHOUR,HLAT,HLON,
     *          HHOUR,NAREAS,TIME1,TIME2,MAXDEPTH
      common/a4/npoints,ypol

C
c------ Read hypocenter parameters
c
      READ(CARD,5)YEAR,MONTH,HOUR,LAT,LON,DEPTH,AMAG,BMAG,CMAG
5     FORMAT(1X,I4,1X,I2,3X,I2,10X,F7.0,
     *         F8.0,f5.0,13X,F3.0,5X,F3.0,5X,F3.0)
      IF(AMAG.NE.0.)THEN
        MAG=AMAG
      ELSEIF(BMAG.NE.0.)THEN
        MAG=BMAG
      ELSEIF(CMAG.NE.0.)THEN
        MAG=CMAG
      ENDIF
c
c---- GMT time used in Norway ; 8 hours subtracted in Washington, USA
c
c      hour = hour - 8
      if(hour .le. 0) hour = hour + 24
c------------------------------------------------------

      TIME=YEAR*100+MONTH
      IF (HOUR.EQ.0)HOUR=24
C
      EXP=.FALSE.
C
c
c  skip if depth fixed
c
      if (card(44:44).eq.'F') goto 15
c
c  skip if star event
c
      if (card(23:23).eq.'*') goto 15

      DO 8 I=1,NAREAS
c     write(6,*)time,time1(i),time2(i)
      IF(TIME.LT.TIME1(I).OR.TIME.GT.TIME2(I)) GO TO 9
c     IF(LAT.LE.LLAT(I).OR.LAT.GE.HLAT(I)) GO TO 9
c     IF(LON.LE.LLON(I).OR.LON.GE.HLON(I)) GO TO 9
c
c   search inside polygon, first transfer polygon points
c
      do k=1,npoints(i)
        yypol(k,1)=ypol(i,k,1)
        yypol(k,2)=ypol(i,k,2)
      enddo

c     write(6,*)lat,lon
c     write(6,*) npoints(i)
c     write(6,*) (yypol(k,1),k=1,npoints(i))
c     write(6,*) (yypol(k,2),k=1,npoints(i))
      call polos(lat,lon,yypol,npoints(i),insidepol)
c     write(6,*)' sel',insidepol
      if(.not.insidepol) goto 9

      IF(DEPTH .GT. MAXDEPTH(I) )          GO TO 9
      IF(MAG.GT.LMAG(I)) GO TO 9
      IF(HOUR.GE.LHOUR(I).AND.HOUR.LE.HHOUR(I))THEN
c
c------- Explosion seem to bee true, but first check for exeptions------
c
         do 19 k=1,nexeptions
            IF(    TIME.GE.ETIME1(k)   .AND.
     *             TIME.LE.ETIME2(k)   .AND.
     *             LAT.GE.ELLAT(k)     .AND.
     *             LAT.LE.EHLAT(k)     .AND.
     *             LON.GE.ELLON(k)     .AND.
     *             LON.LE.EHLON(k)            )then 
                write(6,*)' exception' 
                GO TO 9
             endif
19       continue

c------- Explosion is true -----------------
         EXP=.TRUE.
         GO TO 15
      ENDIF
9     CONTINUE
8     CONTINUE
C
15    CONTINUE
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_areas
      include 'seidim.inc'        ! dimensions
      include 'libsei.inc'
      character*80 incard
      integer  read1              ! unit for reading
      INTEGER LHOUR(500),HHOUR(500),TIME1(500),TIME2(500),NAREAS
      REAL LMAG(500),LLAT(500),LLON(500),HLAT(500),HLON(500)
      REAL MAXDEPTH(500)
      REAL ELMAG(500),ELLAT(500),ELLON(500),EHLAT(500),EHLON(500)
      INTEGER ELHOUR(500),EHHOUR(500),ETIME1(500),ETIME2(500)
      integer npoints(500),code
      integer nexeptions
      REAL EMAXDEPTH(500)
      real ypol(500,500,2)    ! array defining polygon
      COMMON/A3/READ1         ! unit for reading
      COMMON/A1/LMAG,LLAT,LLON,LHOUR,HLAT,HLON,
     *          HHOUR,NAREAS,TIME1,TIME2,MAXDEPTH
      COMMON/A2/ELMAG,ELLAT,ELLON,ELHOUR,EHLAT,EHLON,
     *          EHHOUR,ENAREAS,ETIME1,ETIME2,EMAXDEPTH,
     *          NEXEPTIONS
      common/a4/npoints,ypol

c
c------- Filter paramters are always found on the file: exp_filter.areas
c
      nareas = 0
      nexeptions = 0

1     read(read1,'(a)',end=999)incard
c     write(6,'(a)') incard
      if(incard(1:1) .eq. '#')then  ! first line for a new area
        nareas = nareas + 1
        read(incard,'(16x,2f8.3,5i8)')maxdepth(nareas),lmag(nareas),
     *        lhour(nareas),hhour(nareas),time1(nareas),time2(nareas),
     *        npoints(nareas)
c     write(6,*)' np',npoints(nareas)
c
c   read points of polygon
c
             read(read1,*) 
     *       (ypol(nareas,l,2),ypol(nareas,l,1),l=1,npoints(nareas))
      endif

      if(incard(1:1) .eq. '*')then
        nexeptions = nexeptions + 1
        call sei get values (11,incard(2:80),code)
        i=array$(1)
        emaxdepth(nexeptions)=array$(2)
        elmag(nexeptions)=array$(3)
        ellat(nexeptions)=array$(4)
        ehlat(nexeptions)=array$(5)
        ellon(nexeptions)=array$(6)
        ehlon(nexeptions)=array$(7)
        elhour(nexeptions)=array$(8)
        ehhour(nexeptions)=array$(9)
        etime1(nexeptions)=array$(10)
        etime2(nexeptions)=array$(11)
      endif

      go to 1

999   return
      end
