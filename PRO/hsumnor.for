c
c   converts hypo71 summary cards to nordic format
c
c   jh june 1993
c
c   oct  29 94: add rms
c   nov 94   rem one space when reading
c   nov 30   put event type to L
c   feb 99 jh : ---------------   version 7.0 check ------------------
c
       IMPLICIT NONE
c  ARRAY WITH ORIGINAL READINGS
      CHARACTER*80 DATA(1)
      character*80 infile
      INTEGER YEAR,MONTH,DAY,HR,MIN
      REAL SEC
C  LATITUDE AND LONGITUDE IN DEGREES
      INTEGER LAT,LON
C  MINUTES OF ------------------
      REAL MLAT,MLON
C  DECIMAL LATITUDE AND LONGITUDE
      REAL DLAT,DLON
C  DEPTH, MAGNITUDE, RMS OF RESIDUALS
      REAL DEPTH,MAG,RMS
      INTEGER NPHASE
      character*3 agency
C  NUMBER OF DIFFERENT PHASE FOR EVENT
      character*2 cen         ! century
      INTEGER I


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

 
      agency=' '

      write(6,*)' Input file'
      read(5,'(a)') infile
 2    continue
      write(6,*)' Century(19 or 20) ?'
      read(5,'(a2)') cen
      if(cen.ne.'19'.and.cen.ne.'20') goto 2
      open(1,file=infile,status='old')
      open(2,file='hsumnor.out',status='unknown')
c
 50   continue 
      READ(1,100,end=99)YEAR,MONTH,DAY,HR,MIN,SEC,LAT,MLAT,LON,
     *    MLON,DEPTH,MAG,NPHASE,rms
 100  FORMAT(3I2,1X,2I2,F4.2,I3,F4.2,I3,
     *F4.2,F6.2,F4.2,I3,10x,f4.2)
C
      DLAT=LAT+MLAT/60.0
      DLON=-LON-MLON/60.0
C
         do i=1,80
           data(1)(i:i)=' '
         enddo
c        rms=0.0
         WRITE(DATA(1)(2:20),'(a2,I2,1X,2I2,1X,2I2,1X,F4.1)')
     *   cen,year,MONTH,DAY,HR,MIN,SEC
         WRITE(DATA(1)(24:43),'(F7.3,F8.3,F5.1)') DLAT,DLON,DEPTH
         WRITE(DATA(1)(46:71),202) AGENCY,Nphase,RMS
         write(data(1)(57:59),'(f3.1)') mag
         data(1)(60:60)='C'
         data(1)(22:22)='L'
 202     FORMAT(A3,I3,F4.1)
      write(2,'(a)') data(1)
      goto 50
99    continue
      stop
      end 
