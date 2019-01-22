C   RESPONSE PROGRAM
C
C   J. HAVSKOV    FEB 90
C
C   CALCULATES RESPONSE OF A SEISMIC SYSTEM INCLUDING
C   SENSORS, AMPLIFIER, FILTERS ETC. CAN BE COMBINED WITH
C   POLES AND ZEROS SPECIFICATIONS 
c
C
C   ALL UNITS ARE IN METERS AND SECONDS EXCEPT ACCELERATION
C   WHICH IS SUPPOSED TO BE IN G.
C
C   LAST UPDATE:
C   mar 13, 90 by j.h. : last changes 
C   dec 90             : seisan  response file
C   jan  4 91          : minor cleanup
C   JAN 29             :----
C   feb 4              : error in output format
c   nov 12 91 by jh    : adapt seisan  response filename to pc
C   NOV 22 92          : decrease lower frequency to 0.01 hz, make
c   dec 17             : array bound bug
c                        calculation with accelerometer correct if
c                        a displacemrnt or velocity response is wanted,
c                        from now on units also m and s for accellerometer
c                        in output, g in input as before.
C   DEC 1 92           : BUG WHEN CALCULATING LOWER FREQUENCY STEPS
C   dec 93             : fix pc file names, seisan  to seisan
c   mar 30 94          : do not allow seisan response file if not displacement
c   aug 26 94          : RESPONSE.OUT to resp.out
c   dec 94             : ********* version 5.0 ****************************
c   aug 2  96  jh      : changed phase response , seem that there was
c                        an error, term with 2 * i * damp changed sign
c   feb 97             : poles and zeros
C   feb 97 lo          : moved subrourine inter_p to ../LIB/sys_resp.for
c   mar 14 jh          : bugs with new implemention of T and C indicators
c                        chaekc if A is used for accelerometer
c   mar 21             : error message if missing tabulated file
c   oct 7, 1998        : ------------   verison 7.0 check------------------
c                        5 letter station, year 2000
c   nov 5 98 jh        : remove computer dependence, system-c to systemc
c   sep 7,99 lo        : fix output of filename on Linux
c   oct 1, 99 lo       : add a write statement
c   mar 19, 00 lo      : add paz output for seismometer and filters
c   may 00  jh         : get right order of lat an dlon, thanks mario
c   sep 18 lo          : fix linux bug, init variables
c   oct 04 lo          : set gsesei.def comp code fore GSE
c   nov  1 lo          : change format of GSE output from 2.1 to 2.0
c   nov  2 lo          : some smaller changes in GSE out part
c   nov  3 lo          : changed GSE response
c   jan 3  2001 jh     : check component name, redo option, check equation
c                        again, some comments.
c   jan 24             : fix so seisan output works if combined response of
c                        paz an dparameters, fix seisan output of paz combined
c                        with input file of paz, only input file paz was written
c                        out, sligh change in parameter output format to 
c                        accomodate negative gain constants
c   july 13            : add gse description, and changed calib in CAL2 line
c   jun 8 2004 jh      : small cahnge in format for recalculation
c   may 24 2005 lo     : some changes, added gse fir
c   jan 11 2005 jh     : in seisan format, was writing out double number 
c                        of poles and zeros !!!
c   jan 08 2009 lo     : accept enter when entering number of fir, rec med gai,
c                        amp gain, and input sampling rate, 
c                        and fixed bug with paz input and gse
c   dec  8 2010 lo     : changed scaling for gse and no sensor type
c   ma 22  2011 jh     : make it possibel to use any band and instument codes
c   may 11 2011 lo     : fixed bug in fir normalization

C
      IMPLICIT NONE
      include 'libsei.inc'
C
C   VARIABLES
C
C   COMPLEX RESPONSE OF SYSTEM FOR NF VALUES
      COMPLEX RESPON(61)
      INTEGER NF
C   FIRST AND LAST FREQUENCY TO USE
      REAL F1,F2
C   REAL AMPLITUDE AND PHASE VALUES, GAIN AT 1HZ, LOG OF AMP
      REAL AMP(61),PHASE(61), AMP1HZ, LAMP(61)
C   MANUAL ENTERED FREQUENCIES, AMPLITUDE AND PHASE VALUES 
C   FOR MNF VALUES
      REAL MF(61),MAMP(61),MPHASE(61)
      INTEGER MNF
C   FILE NAME OF MANUAL ENTERED VALUES
      CHARACTER*20 MANFIL 
C   Seisan FILE NAME
      CHARACTER*40 FILE
      CHARACTER*80 TEXT  ! GENERAL TEXT
      CHARACTER*96 GSETEXT
      INTEGER CODE       ! ERROR RETURN
C   DATE AND TIME
      INTEGER IYR,IDOY,IMON,IDY,IHR,IMIN,ISEC
      REAL SEC
C   LATITUDE,LONGITUDE AND ELEVATION
      REAL ALAT,ALON
      INTEGER IELEV
C   STATION AND COMPONENT
      CHARACTER STA*5,COMP*4
C   COMMENT FOR SEISAN RESPONSE FILE
      CHARACTER*80 COMMENT
C   FREQUENCIES FOR CALCULATIONS, LOG OF F
      REAL F(61),LF(61)
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      REAL FFILT(10),FC
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   SEISMOMETER NATURAL ANG FREQUENCY
      real omega0
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
C   FLAG FOR TYPE OF SENSOR 0: NONE 1: SEISMOMETER 2: ACCELEROMETER
      INTEGER SENTYP
C   SEISAN HEADER
      CHARACTER*1040 HEADER
c   equivalent to header
      character*80 chead(13)
C   FLAG FOR TYPE OF RESPONSE 1: DISPLACEMENT, 2: VELOCITY, 3: ACCELER.
      INTEGER RESTYP
C OUTPUT TYPE
      INTEGER OUTTYP
C   TEXT FOR RESPONSE AND SENSOR TYPE
      CHARACTER*15 RESTXT(3),SENTXT(3)
C   RESPONSE CURVE CORRECTION EXPONENT
      INTEGER RESCOR
C   PLOT TEXT
      CHARACTER*8 YTEXT
      CHARACTER*6 XTEXT
C   CONSTANT
      REAL PI
C   HELP VARIABLES
      COMPLEX CX,FACTOR
      REAL X,Y
      DOUBLE PRECISION XL
      CHARACTER*1 ANSWER
      INTEGER I,J,K,I1,I2,I3,I4,I5,I6,J1,J2,LINE
c   FLAG IF PAZ CHOSEN
      logical paz
c   flag true if gse
      logical gse
      logical recal                     ! true if recalculation
      character*80 deffile              ! text
      character*29 mainhead_text
      character*5 net_code              ! network code
      logical no_net                    ! flag if net_code set
c
c gse response
c
      real gse_dig2_samprat     ! digitizer sample rate
      real gse_rate             ! combined sample rate
      real gse_dig2_sensitivity ! digitizer sensitivity (counts/input)
      real gse_paz2_sfactor     ! paz scale factor
      real gse_cal2_calib       ! total calibration of all stages
      real gsepazscale          ! used to scale PAZ part for GSE format
      character*25 gse_dig2_description  ! digitizer model
      character*6 gse_cal2_instype       ! seismometer type
      integer gse_stage
c
c  for tabulated input
c
      integer nresp        ! number of tabulated input values
      real freq_val(300),gain_val(300),phas_val(300) ! freq, amp and phase
      real normtab         ! normalization constant for tabulated input
c
c   for poles and zeros
c
      real ppr(1000),ppi(1000)   ! poles,  real and im.
      real pzr(1000),pzi(1000)   ! zeros, real and im.
      complex pol(500),zero(500) ! complex poles and zeros
      integer npol,nzero        ! number of poles and zeros
      integer nfir(20)          ! number of fir
      integer fir_deci(20)      ! decimation in fir stage
      character*1 fir_sym(20)   ! symmetry  flag
      real fir_rate(20)         ! decimation in fir stage
      real normalize_fir        ! return normalisation constant
      real delay_fir
      character*1 sym_fir       ! return symmetry flag
      integer nfirstage         ! number of fir filter stages
      double precision fir(20,10000) ! fir filter coeffs 
      double precision rfir(10000)
      double precision fir_sum(20)
      real w                    ! weight for filter poles
      complex cw                ! ---------- complex
      real norm                 ! normalization constant for poles and zeros
      complex presp             ! complex response from poles and zeros
      character*80 filepole     ! file with poles and zeros 
      character*80 filetab      ! file with poles and zeros 
      character*80 filefir      ! file with fir coeff 
      integer seiclen           ! routine to get length of string
c
      equivalence (header,chead)
      common /resp/npol,nzero,ppr,ppi,pzr,pzi,pol,zero,norm
      DATA PI/3.141592654/


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   initialize
c
      sta=' '
      iyr=0


C
C   TEXT FOR OPTIONS
C
      SENTXT(1)='NO SENSOR      '
      SENTXT(2)='SEISMOMETER    '
      SENTXT(3)='ACCELEROMETER  '
      RESTXT(1)='DISPLACEMENT   '
      RESTXT(2)='VELOCITY       '
      RESTXT(3)='ACCELERATION   '      
      npol=0
      nzero=0
      norm=1.
      period=0.
      dampin=0.
      gencon=0.
      gain=0.
      regain=0.
      gsepazscale=1.
      gse_dig2_samprat=0.
      gse_dig2_sensitivity=1.
      amp1hz=0.
      do i=1,7
        ffilt(i)=0.
        pole(i)=0.
      enddo
C
C   FREQUENCIES FOR CALCULATION, ROUND OFF VALUES
C
      NF=60
      F1=0.005
      F2=100.0
      DO 5 I=1,NF
         X=ALOG10(F1)+(I-1)*(ALOG10(F2)-ALOG10(F1))/(NF-1)
         F(I)=10**X
         if(f(i).le.0.01.and.f(i).gt.0.001) k=10000
         IF(F(I).LE.0.1.AND.F(I).GT.0.01) K=1000
         IF(F(I).LE.1.0.AND.F(I).GT.0.1) K=100
         IF(F(I).LE.10.0.AND.F(I).GT.1.0) K=10
         IF(F(I).GE.10.0) K=1
         J=F(I)*K+0.5
         F(I)=J/FLOAT(K)          
 5    CONTINUE
C
C   PUT 1 HZ RESPONSE IN NUMBER 61
C
      F(61)=1.0
      NF=61
C
C   ENTER OUTPUT FORMAT
C
      paz=.false.
      gse=.false.
      write(6,*) ' RESP - PROGRAM TO CREATE RESPONSE FILES IN SEISAN'
      write(6,*) '        OR GSE FORMAT. THE RESPONSE CAN BE CREATED'
      write(6,*) '        AS POLES AND ZEROS (PAZ) OR FREQUENCY'
      write(6,*) '        AMPLITUDE AND PHASE (FAP). THE SAME' 
      write(6,*) '        TRANSFER FUNCTION AND FILTERS ARE USED'
      write(6,*) '        IN BOTH CASES.'
      write(6,*)
      write(6,*) ' CHOSE OUTPUT FORMAT: 0: NO OUTPUT FILE '
      write(6,*) '                      1: SEISAN FAP '
      write(6,*) '                      2: SEISAN PAZ '
      write(6,*) '                      3: GSE2 FAP '
      write(6,*) '                      4: GSE2 PAZ '
      read(5,*) outtyp
      if (outtyp.eq.3.or.outtyp.eq.4) gse=.true.
      if (outtyp.eq.2.or.outtyp.eq.4) paz=.true.

C
C   ENTER INPUT PARAMETERS FROM KEYBOARD
C
      WRITE(6,*)' TYPE OF SENSOR:       1: NONE '
      WRITE(6,*)'                       2: SEISMOMETER '
      WRITE(6,*)'                       3: ACCELEROMETER '
      WRITE(6,*)
     *'                       4: MECHANICAL DISPLACEMENT SEISMOMETER '   
      READ(5,*) SENTYP
C
C   SENSOR PARAMETERS
C
      GENCON=1.
      IF(SENTYP.EQ.2.OR.SENTYP.EQ.4) THEN
         WRITE(6,*)' SEISMOMETER NATURAL PERIOD ? '
         READ(5,*) PERIOD
         WRITE(6,*)' SEISMOMETER DAMPING RATIO ? '
         READ(5,*) DAMPIN
      ENDIF
      IF(SENTYP.EQ.2.OR.SENTYP.EQ.3) THEN
         WRITE(6,*) 
     *   ' SENSOR LOADED GENERATOR CONSTANT (V/M/S OR V/G) ? '
         READ(5,*) GENCON
      ENDIF
C
C INSTRUMENT TYPE IF GSE ONLY
C
      gse_cal2_instype=' '
      if (gse.and.SENTYP.ne.1) then
        write(*,*) '  INSTRUMENT TYPE FROM LIST BELOW (<6 CHARS)'
        write(*,*)
        write(*,*) 
     &  '  e.g., Akashi, 23900, BB-13V, CMG-3, CMG-3N, CMG-3T, CMG-3E, '
        write(*,*) 
     &  '  FBA-23, GS-13, GS-21, KS3600, KS360i, KS5400, MK II, '
        write(*,*) 
     &  '  Oki, Parus2, S-13, S-500, STS-1, STS-2, TSJ-1e '
        write(*,*) ' CHOICE '
        read(5,'(a)') gse_cal2_instype
      endif

C
C   RECORDING MEDIA GAIN
C
      WRITE(6,*)
     *' RECORDING MEDIA GAIN (COUNT/V, M/V OR TIMES), enter for 1.0 ? '
      READ(5,'(a)') TEXT
      IF(TEXT(1:2).EQ.'  ') THEN
         regain=1.
      ELSE
         CALL SEI GET VALUES(1,TEXT,CODE)
         regain=ARRAY$(1)
      ENDIF
c      READ(5,*) REGAIN
      if (gse) then
        write(*,*) ' DIGITIZER SAMPLE RATE '
     &           //'(BEFORE POSSIBLE FIR FILTER)'
        READ(5,'(a)') TEXT
        IF(TEXT(1:2).EQ.'  ') THEN
           gse_dig2_samprat=0.
        ELSE
           CALL SEI GET VALUES(1,TEXT,CODE)
           gse_dig2_samprat=ARRAY$(1)
        ENDIF
c        read(5,*) gse_dig2_samprat
        gse_rate=gse_dig2_samprat
        write(*,*) ' DIGITIZER MODEL '
        read(5,'(a)') gse_dig2_description
      endif
C
C   AMPLIFIER PARAMETERS
C
      WRITE(6,*)' AMPLIFIER GAIN (DB), ENTER FOR 0 DB (GAIN 1.0) ? '
      READ(5,'(a)') TEXT
      IF(TEXT(1:2).EQ.'  ') THEN
         gain=0
      ELSE
         CALL SEI GET VALUES(1,TEXT,CODE)
         gain=ARRAY$(1)
      ENDIF
c      READ(5,*) GAIN
      WRITE(6,*) ' NUMBER OF FILTERS (0-10), RETURN FOR NONE ? '
      READ(5,'(a)') TEXT
      IF(TEXT(1:2).EQ.'  ') THEN
         NFILT=0
      ELSE
         CALL SEI GET VALUES(1,TEXT,CODE)
         NFILT=ARRAY$(1)
      ENDIF
      IF(NFILT.GT.0) THEN
         WRITE(6,*)' FREQUENCY AND NUMBER OF POLES FOR EACH FILTER,'
         WRITE(6,*)' POLES NEGATIVE FOR HIGH PASS '
         DO 10, I=1,NFILT
            READ(5,*) FFILT(I),POLE(I)
 10      CONTINUE
      ENDIF
C
C   TYPE OF RESPONSE IF SENSOR IS USED
C
      RESTYP=1
      RESCOR=0
      IF(SENTYP.GT.1) THEN
         if (.not.paz) then
           WRITE(6,*)' TYPE OF RESPONSE: 1: DISPLACEMENT'
           WRITE(6,*)'                   2: VELOCITY'
           WRITE(6,*)'                   3: ACCELERATION '
           WRITE(6,*)' NOTE: CHOOSE DISPLACEMENT IF YOU ARE '//
     &               'CREATING SEISAN RESPONSE FILE'
           READ(5,*) RESTYP
C
C   CALCULATE EXPONENT FOR RESPONSE TYPE CORRECTION
C        
           IF(SENTYP.EQ.2) RESCOR=RESTYP-1
           IF(SENTYP.EQ.3) RESCOR=RESTYP-3
           IF(SENTYP.EQ.4) RESCOR=RESTYP                   
         endif
      ENDIF
c
c   file name of file with poles and zeros or tabulated values,
c   can multiply with
c   existing curve or use alone
c
         nresp=0
         goto 544
 444     CONTINUE
         write(6,*)' No such file'
 544     continue
         WRITE(6,*) ' FILE NAME FOR FILE WITH POLES AND ZEROS ',
     *   'FOR SEISMOMETER, RETURN FOR NO FILE'
         READ(5,'(A)') FILEPOLE
         IF(FILEPOLE(1:2).NE.'    ') THEN
            OPEN(20,FILE=FILEPOLE,STATUS='OLD',ERR=444)
            CALL READ_POLES                 ! read poles and zeros
            write(6,*)' ',npol+nzero,' poles and zeros'
            close(20)
         endif
         if (gse.and.sentyp.lt.2) then
           if (npol+nzero.eq.0) then
             write(*,*) ' for GSE output: instrument or paz '//
     &        'have to be given, forced EXIT '
             stop
           endif
         endif
c
c   tabulated values
c
         goto 545
 445     CONTINUE
         write(6,*) 'No such file'
 545     continue
         FILETAB = '  '
         if (.not.paz) then
           WRITE(6,*) ' FILE NAME FOR TABULATED VALUES,',
     *       ' RETURN FOR NO FILE'
           READ(5,'(A)') FILETAB
         endif
         IF(FILETAB(1:2).NE.'    ') THEN
            OPEN(20,FILE=FILETAB,STATUS='OLD',ERR=445)
            read(20,*) nresp,normtab
            write(6,*) ' ',nresp,' tabulated values'
            do i=1,nresp                    ! read tabulated values
               read(20,*) freq_val(i),gain_val(i),phas_val(i)
            enddo
            close(20)
         endif
c
c FIR FILTERS IF FORMAT IS GSE
c
      nfirstage=0
      if (gse) then
        write(*,*) ' NUMBER OF FIR FILTER STAGES '
        READ(5,'(a)') TEXT
        IF(TEXT(1:2).EQ.'  ') THEN
          nfirstage=0
        ELSE
          CALL SEI GET VALUES(1,TEXT,CODE)
          nfirstage=ARRAY$(1)
        ENDIF
c        read(5,*) nfirstage
        do i=1,nfirstage
          goto 546
 446      CONTINUE
          write(6,*) 'No such file'
 546      continue
          FILEFIR = '  '
          WRITE(6,*) ' FILE NAME FOR FIR COEFFICIENTS,',
     *        ' RETURN FOR NO FILE'
          READ(5,'(A)') FILEFIR
          write(*,*) ' DECIMATION, FIR STAGE ',i
          read(5,*) fir_deci(i)
          write(*,*) ' FIR FILTER SYMMETRY (A=asymetric, ' //
     &               'B=symetric (odd) '//
     &               'C=symetric (even) '
          read(5,*) fir_sym(i)
          IF(FILEFIR(1:2).NE.'    ') THEN
             OPEN(20,FILE=FILEFIR,STATUS='OLD',ERR=446)
               nfir(i)=0
               fir_sum(i)=0.
 547           CONTINUE
               read(20,*,end=548) xl
               nfir(i)=nfir(i)+1
               fir(i,nfir(i))=xl
               fir_sum(i)=fir_sum(i)+xl
c               write(55,*) nfir(i),xl
               GOTO 547
 548           CONTINUE
             close(20)
             if (fir_sym(i).eq.'B') then
               do k=1,nfir(i)-1
                 fir_sum(i)=fir_sum(i)+fir(i,k)
               enddo
             elseif (fir_sym(i).eq.'C') then
               do k=1,nfir(i)
                 fir_sum(i)=fir_sum(i)+fir(i,k)
               enddo
             endif
c
c normalise
c
             do k=1,nfir(i)
               fir(i,k)=fir(i,k)/fir_sum(i)
             enddo
             write(*,*) ' stage ',i,
     &           ' number of fir coefficients ',nfir(i)
          endif
c          write(*,*) ' SAMPLE RATE AT INPUT TO FIR FILTER, '
c     &     //'FIR STAGE ',i
c          read(5,*) fir_rate(i)
          if (i.eq.1) then
            fir_rate(i)=gse_dig2_samprat
          else
            fir_rate(i)=fir_rate(i-1)/fir_deci(i-1)
          endif
          write(*,*) ' input sample rate ',fir_rate(i)
          write(*,*) ' output sample rate ',fir_rate(i)/fir_deci(i)
          if (i.eq.nfirstage) gse_rate=fir_rate(i)/fir_deci(i)
        enddo
      endif
c
c-------------------------------------------------------
c   enter here for recalculation from end of program
c--------------------------------------------------------
c
 2000 continue

      nf=61  ! to get 1 hz again if recal
C
C   NOW READY FOR CALCULATIONS, FIRST CALCULATE CONSTANT FACTORS  
C
      FACTOR=1.0
C
C  AMP GAIN AND RECORDING MEDIA GAIN
C
      FACTOR=FACTOR*10**(GAIN/20.0)*REGAIN
C
C  SEISMOMETER IF ANY
C
      if (.NOT.PAZ) then
        IF(SENTYP.EQ.2) THEN
           FACTOR=FACTOR*GENCON
c          FACTOR=FACTOR*PI*(0.0,2.0)*GENCON*(-1.0)*PERIOD**2 ! before mar 2000
        ENDIF
C
C  ACCELEROMETER IF ANY, DO CONVERSION OF G TO M/SS
C
        IF(SENTYP.EQ.3) FACTOR=FACTOR*GENCON/9.81
      endif
C
C response in PAZ
C
      if (paz) then
        omega0=2.*PI/period
c
c norm in Counts/m
c
c        norm=norm*10**(GAIN/20.0)*REGAIN   ! norm=1.
        gsepazscale=norm*1E-09   ! norm may be set if paz file m to nm
        norm=norm*10**(GAIN/20.0)*REGAIN
c        write(6,*)'norm,gain,regain:',norm,gain,regain
        gse_dig2_sensitivity=10**(GAIN/20.0)*REGAIN
        if (sentyp.eq.2.or.sentyp.eq.4) then
          norm=norm*GENCON    ! Seisan
          gse_paz2_sfactor= GENCON * gsepazscale     ! c/m -> c/nm
          ppr(npol+1)=-dampin*omega0
          ppi(npol+1)=sqrt(1.-dampin**2)*omega0
          pol(npol+1)=cmplx(ppr(npol+1),ppi(npol+1))
          ppr(npol+2)=-dampin*omega0
          ppi(npol+2)=-sqrt(1.-dampin**2)*omega0
          pol(npol+2)=cmplx(ppr(npol+2),ppi(npol+2))
          npol=npol+2
          zero(nzero+1)=cmplx(0.,0.)
          zero(nzero+2)=cmplx(0.,0.)
          if(sentyp.eq.2) then
             zero(nzero+3)=cmplx(0.,0.)
             nzero=nzero+3
          else                    ! mechnical displacemnt seismometer
             nzero=nzero+2
          endif
        elseif (sentyp.eq.3) then
          norm=norm*GENCON/9.81   ! Seisan
          gse_paz2_sfactor=GENCON/9.81 * gsepazscale     ! c/m -> c/nm
          zero(nzero+1)=cmplx(0.,0.)
          zero(nzero+2)=cmplx(0.,0.)
          nzero=nzero+2
        elseif (sentyp.eq.1) then
          gse_paz2_sfactor=gsepazscale
c          gse_paz2_sfactor= norm / 1E9     ! c/m -> c/nm    ---- only if paz added later ... lo 8/10/2010
        endif
c
c get poles and zeros for Butterworth filters, same as routine 
c bworth, but gives paz instead of the transfer function
c
c    the formulas used -- 
c
c    lowpass:      h(s)=1/(s-s1)(s-s2)...(s-sk)
c    where poles   sk=omega0*exp(i*pi*(1/2+((2*k-1)/(2*np))))
c                                   k=1,2, ... number of poles
c                  s = i omega
c                  norm = norm * omega0
c    highpass:     h(s)=z**k/(s-s1)(s-s2) ....
c    where poles   sk=omega0/exp ...
c

        if (nfilt.gt.0) then
          do i=1,nfilt
            fc=ffilt(i)*2*PI 
            k=iabs(pole(i))
            do j=1,abs(pole(i))
              w=PI*(0.5+((2.*float(j)-1.)/(2.*float(k))))
              cw=cmplx(0.,w)
c
c lowpass
c
              if (pole(i).gt.0) then
                norm=norm*fc
                gse_paz2_sfactor = gse_paz2_sfactor*fc
                pol(npol+1)=fc*cexp(cw)
                npol=npol+1 
c
c highpass
c
              else
                 pol(npol+1)=fc/cexp(cw)
                 npol=npol+1 
                 zero(nzero+1)=cmplx(0.,0.)
                 nzero=nzero+1
               endif
            enddo         
          enddo
        endif
      endif
C
C  ENTER LOOP OF NF PREDEFINED FREQUENCES TO CALCULTE REPONSE
C  INITIALLY DISPLACEMENT FOR SEISMOMETER AND ACCELERATION FOR
C  ACCELEROMETER
C   
      DO 100 I=1,NF
C
         if (.not.paz) then
C
C   SEISMOMETER
c
c                                  REGAIN[V/m/s] * GENCON * s^3
c T(s)= preamplification *  ----------------------------------------------
c                                 s^2 + 2*h*omega0*s + omega0^2
c
c   T(j omega) = T(s) for s -> j omega
c


C           RESPON(I)=FACTOR         ! before mar 00
 
           IF(SENTYP.EQ.1) THEN
             RESPON(I)=FACTOR
           ELSEIF(SENTYP.EQ.2.or.sentyp.eq.4) THEN
C
C CLEANED UP IN MARCH 2000 lo
C
              X=PERIOD*F(I)
              RESPON(I)=FACTOR*(0.,2.)*PI*PERIOD**2*(-1.)*F(I)**3
     &                      /(1.+(0.,2.)*DAMPIN*X-X*X)
c
c  this is the same as:
c
c                          - i * (om)**3
c         RESPON(om) = ---------------------------
c                       om0**2 - om**2 +2*h*om*om0
c
c  where om=2*pi*f, om0 = 2*pi*f0,  f=/T, f0=1/T0
c
c  the minus sign in the nominator comes from the generator constant
c  or, if polarity has put so, is there to make sure a positive pilse gives
c  a positive first motion
c

C RESPON(I)=RESPON(I)*F(I)**3/(1.+(0.,2.)*DAMPIN*X-X*X) ! before aug96
c RESPON(I)=RESPON(I)*F(I)**3/(1.-(0.,2.)*DAMPIN*X-X*X) ! until mar 00

           ELSEIF (SENTYP.EQ.3) THEN
             RESPON(I)=FACTOR
           ENDIF
C
C   FILTERS
C
           IF(NFILT.GT.0) THEN
              DO 20, J=1,NFILT
                 CALL BWORTH(F(I),FFILT(J),POLE(J),CX)
                 RESPON(I)=RESPON(I)*CX
 20           CONTINUE
           ENDIF
         endif
C
C   MULTIPLY WITH ANOTHER RESPONSE CURVE WHICH CAN BE POLES AND ZERO'S
C   if available, if only paz used, that is all which is done, if paz
c   are additional to fap, multiply with earlier fap values
C

        if(npol.ne.0.or.nzero.ne.0) then
           call pazresp (f(i), norm, nzero, zero,
     $     npol, pol,presp)
           if (.not.paz) then     ! fap
              respon(i)=respon(i)*presp
           else
              respon(i)=presp     ! paz
           endif
        else
           if (paz) then
             respon(i)=norm
           endif
        endif
c
c   interpolate in tabulated values
c
        if(nresp.gt.2) then
           call inter_p(nresp,normtab,
     *     freq_val,gain_val,phas_val,f(i),presp)                                       
           respon(i)=respon(i)*presp
        endif

C
C   COMPLEX RESPONSE FINISHED, CALCULATE REAL RESPONSE
C   TAKING INTO CONSIDERATION THE TYPE OF RESPONSE WANTED
C        
         AMP(I)=CABS(RESPON(I))/((2*PI*F(I))**RESCOR)
         X=AIMAG(RESPON(I))
         Y=REAL(RESPON(I))
         IF(Y.EQ.0.0) THEN
            PHASE(I)=0.0
         ELSE
            PHASE(I)=(ATAN2(X,Y)-(PI/2.0)*RESCOR)*(180./PI)
         ENDIF
 100  CONTINUE
C
C  FINISHED RESPONSE CALCULATION. NORMALIZE TO 1.0 AT 1 HZ
C  AND SAVE 1HZ VALUE
C
      AMP1HZ=AMP(61)
C
C   FINISHED WITH 1 HZ, NF BACK TO 60
C
      NF=60
      DO 110 I=1,NF
         AMP(I)=AMP(I)/AMP1HZ
 110  CONTINUE
C
C   CHECK IF MANUAL POINTS SHOULD BE USED FOR COMPARISON
C
      if(.not.recal) then
         WRITE(6,*)
     *   ' FILE NAME FOR MEASURED VALUES, RETURN FOR NO FILE '
         READ(5,'(A20)') MANFIL
         MNF=0
         IF(MANFIL.NE.'                    ') THEN
            OPEN(3,FILE=MANFIL,STATUS='OLD')
            I=1
 30         CONTINUE
            READ(3,*,END=40)MF(I),MAMP(I),MPHASE(I)
            I=I+1
            GOTO 30
 40         CONTINUE
            MNF=I-1
            WRITE(6,*) MNF,' VALUES READ IN'
         ENDIF
      endif
C
C   OUTPUT IN FILE
C
      OPEN(2,FILE='resp.out',STATUS='UNKNOWN')
      WRITE(2,220)SENTXT(SENTYP),RESTXT(RESTYP)
 220  FORMAT('  SENSOR TYPE: ',A15,3X,'RESPONSE: ',A15) 
      WRITE(2,*)' SEISMOMETER PERIOD=', PERIOD
      WRITE(2,*)' GENERATOR CONSTANT=', GENCON
      WRITE(2,*)' DAMPING RATIO     =', DAMPIN
      WRITE(2,*)' AMPLIFIER GAIN(DB)=', GAIN
      WRITE(2,*)' RECORDING GAIN=    ', REGAIN
      IF(FILEPOLE(1:3).NE.'   ') 
     *write(2,'(a,a)') 
     *'  FILE WITH POLES AND ZEROS: ',FILEPOLE
      IF(FILETAB(1:3).NE.'   ') 
     *write(2,'(a,a)') 
     *'  FILE WITH TABULATED VALUES: ',FILETAB
      IF(NFILT.GE.1) THEN
         WRITE(2,*)' FILTER CONSTANTS' 
         WRITE(2,200) (FFILT(I),POLE(I),I=1,NFILT)
 200     FORMAT(1X,' F=',F6.2,3X,'POLES=',I3)
      ENDIF
      WRITE(2,*)' GAIN AT 1 HZ=      ', AMP1HZ
      WRITE(2,*) 
      WRITE(2,201)(F(I),1.0/F(I),AMP(I),
     *20*ALOG10(AMP(I)),PHASE(I),I=1,NF)
 201  FORMAT
     *(1X,'F=',F8.4,3X,'T=',F8.2,3X,' AMP=',F14.6,3X,
     *' AMPDB=',F6.1,3X,' PHAS=',F8.1)
C
C   FINISHED, TAKE LOG BEFORE PLOTTING
C
       DO 130 I=1,NF
         LAMP(I)=ALOG10(AMP(I))
         LF(I)=ALOG10(F(I))
 130  CONTINUE
      DO 135 I=1,MNF
         MAMP(I)=ALOG10(MAMP(I))
         MF(I)=ALOG10(MF(I))
 135  CONTINUE
C
C   PLOT AMPLITUDE RESPONSE
C
      WRITE(6,*)     
      XTEXT='FREQ  '
      YTEXT='AMPL    '  
      WRITE(6,221) SENTXT(SENTYP),RESTXT(RESTYP)
 221  FORMAT(12X,' AMPLITUDE RESPONSE',3X,A15,3X,A15) 
      CALL PLOTW(MNF,NF,MF,MAMP,LF,LAMP,XTEXT,YTEXT,1,1)
      WRITE(6,'(a,G10.3,2x,a,$)')' GAIN FACTOR AT 1 HZ:',AMP1HZ,
     *'   RETURN FOR PHASE RESPONSE'
      READ(5,'(A)') X 
C
C   PLOT PHASE RESPONSE                   
C
      YTEXT='PHAS DEG'
      WRITE(6,222) SENTXT(SENTYP),RESTXT(RESTYP)
 222  FORMAT(12X,' PHASE RESPONSE',3X,A15,3X,A15) 

      CALL PLOTW(MNF,NF,MF,MPHASE,LF,PHASE,XTEXT,YTEXT,1,0)
c
C----------------------------------------------------------------
C  OPTION FOR SEISAN RESPONSE  FILE
C-----------------------------------------------------------------
c
      if (outtyp.eq.0) then
        WRITE(6,*) ' RESPONSE CURVE IS IN FILE resp.out'
        write(6,*) ' NO CALIBRATION FILE CREATED '
        stop
      endif
      if (outtyp.eq.1.or.outtyp.eq.2) then
        WRITE(6,'(a,$)')' SEISAN RESPONSE FILE (Y/N=default)?'
      else
        WRITE(6,'(a,$)')' GSE RESPONSE FILE (Y/N=default)?'
      endif
        
      READ(5,'(a1)') ANSWER
      IF(ANSWER.NE.'Y'.and.ANSWER.NE.'y') GOTO 999
C
C   MUST BE DISPLACEMENT, CHECK
C
      IF(RESTYP.NE.1) THEN
         WRITE(6,*)' RESPONSE TYPE MUST BE DISPLACMENT'
         GOTO 999
      ENDIF

c
c  questions, skip if recal unless parameter not given previously
c
      if(recal.and.sta.ne.' '.and.iyr.ne.0) goto 3000
c
      print*,'Enter station code. e.g. BERGE, max 5 chars '
      read(5,'(a)')sta
 666  continue
      print*,' Enter component (4 chars) e.g. SH Z'
      print*,
     *' First character is type, should be one of the following: '
      print*,' E, S, H, B, M, L, U '
      Print*,
     *' Second character is instrument code,',
     *' should be one of the following: '
      print*,' H, L, G, M, N'
      print*,' Third character is not used, leave blank'
      print*,' Last character is orientation, must be Z,N or E'
c      print*,'Character 2 and 3 can be anything'
      read(5,'(a4)')comp
      if(comp(4:4).ne.'N'.and.comp(4:4).ne.'E'.
     *and.comp(4:4).ne.'Z') then
        write(6,*)' Wrong orientation, redo'
        goto 666
      endif
c
c      if(sentyp.eq.3.and.comp(1:1).ne.'A') then
c         comp(1:1)='A'
c         write(6,*)
c     *' Sensor is an accelerometer, first letter of component must be A'
c         write(6,'(a,a)') ' Component changed to ',comp
c      endif
      write(6,'(1x,a,a,$)')
     *'Enter date as YYYYMMDDHHMMSS, at least up to the day ',
     *' (e.g. 19880123):'
      read(5,'(i4,5i2)')iyr,imon,idy,ihr,imin,isec
      sec=float(isec)
c
c   get doy
c
      call date_doy(idoy,idy,imon,iyr)
      ielev=-999
      alat=999
      alon=999
c
c get some input, SEISAN
c
      if (outtyp.eq.1.or.outtyp.eq.2) then
        write(6,'(1x,a,a,$)')'Latitude (Up to 4 ',
     *'decimal places and - for south), return for none:'
        read(5,'(a)') text
        if(text(1:2).ne.'  ') then
           call sei get values(1,text,code)
           alat=array$(1)
        endif
        write(6,'(1x,a,a,$)')'Longitude (Up to 4 ',
     *'decimal places and - for west), return for none:'
        read(5,'(a)') text
        if(text(1:2).ne.'  ') then
           call sei get values(1,text,code)
           alon=array$(1)
        endif
        write(6,'(1x,a,a,$)')'Enter elevation in meters (integer), ',
     *'return for none:'
        read(5,'(a)') text
        if(text(1:2).ne.'  ') then
           call sei get values(1,text,code)
           ielev=array$(1)
        endif
        write(6,*)'Enter comments, one line. e.g. amp type, sensor type',
     *' return for none'
        read(5,'(a80)') comment
      endif
c
c   enter here if question skipped for recal, just above
c
 3000 continue
c
c
c   make file name
c
      file=' '
      file(1:5)=sta(1:5)
      file(6:9)=comp(1:4)
      do i=1,9
        if(file(i:i).eq.' '.or.file(i:i).eq.char(0)) file(i:i)='_'
      enddo
      file(10:10)='.'
      write(file(11:25),150)iyr,imon,idy,ihr,imin
 150  format(i4,'-',i2,'-',i2,'-',2i2)

      if (outtyp.eq.1.or.outtyp.eq.2) then
        file(26:29)='_SEI'
      elseif (gse) then
        file(26:29)='_GSE'
      endif
      do i=11,25
        if(file(i:i).eq.' ') file(i:i)='0'
      enddo
c
      open(4,file=file(1:seiclen(file)),status='unknown')


      if (outtyp.eq.1.or.outtyp.eq.2) then
c
C--------------------------------------------------------------
C   SEISAN RESPONSE FILE
C--------------------------------------------------------------
c
      do 64 i=1,1040
         header(i:i)=' '
 64   continue

C  
C   CONSTANTS
C
c   indicator TC for force use of tabulated values if:
c   1. combined response of poles and zeros and parameters, fab wanted
c   2. combined rersponse with tabulated values
c   3. number of poles and zero larger than 37 since not enough room in format
c

      if(((npol+nzero).gt.0.and.(gencon.gt.0.0.or.nfilt.ne.0.
     *or.regain.ne.1.0.or.gain.ne.0).and..not.paz).
     *or.nresp.gt.0.or.(npol+nzero).gt.37) header(78:79)='TC'

c
      WRITE(HEADER(161:240),'(f8.3,f8.4,f8.1,f8.1,
     *2G8.3,2(g8.3,f8.3))')
     *PERIOD,DAMPIN,GENCON,GAIN,REGAIN,AMP1HZ,
     *FFILT(1),FLOAT(POLE(1)),FFILT(2),FLOAT(POLE(2))
      WRITE(HEADER(241:320),'(5(G8.3,f8.2))')(FFILT(I),
     *FLOAT(POLE(I)),I=3,7)   
C
C  RESPONSE VALUES
C
        do 60 i=1,3
          j1=(i-1)*20 + 1
          j2=j1+19
          i1=320+(i-1)*240+1
          i2=i1+79
          i3=400+(i-1)*240+1
          i4=i3+79
          i5=480+(i-1)*240+1
          i6=i5+79
          write(header(i1:i2),'(10g8.3)')(f(j),j=j1,j2,2)
          write(header(i3:i4),'(10g8.3)')(amp(j),j=j1,j2,2)
          write(header(i5:i6),'(10f8.3)')(phase(j),j=j1,j2,2)
 60     continue
c
c   if only poles and zeros are given and less than 38 together, 
c   use that instead of table values, indicate with P. If combined
c   response has been calculated, do not use poles and zeros.
c
        if(header(79:79).eq.' '.and.paz.and.(npol+nzero).lt.38) then
         header(78:78)='P'   ! indicate poles and zeros
         do i=161,1040
            header(i:i)=' '
         enddo
         write(header(161:182),'(1x,2i5,g11.4)') npol,nzero,norm
         k=23
         line=3
c        do i=1,npol*2
         do i=1,npol          !jh jan 06
c           write(chead(line)(k:k+10),'(g11.4)') ppr(i)
            write(chead(line)(k:k+10),'(g11.4)') real(pol(i))
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
c           write(chead(line)(k:k+10),'(g11.4)') ppi(i)
            write(chead(line)(k:k+10),'(g11.4)') aimag(pol(i))
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
         enddo
c        do i=1,nzero*2
         do i=1,nzero     ! jh jan 06
c           write(chead(line)(k:k+10),'(g11.4)') pzr(i)
            write(chead(line)(k:k+10),'(g11.4)') real(zero(i)) 
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
c           write(chead(line)(k:k+10),'(g11.4)') pzi(i)
            write(chead(line)(k:k+10),'(g11.4)') aimag(zero(i))
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
         enddo
      endif

c
c  put in header, first blank
c
      header(1:5)=sta(1:5)
      header(6:9)=comp(1:4)
      if(alat.ne.999.) write(header(52:59),'(f8.4)')alat
      if(alon.ne.999.) write(header(61:69),'(f9.4,i5)')alon
      if(ielev.ne.-999)write(header(71:75),'(i5)')ielev
      k=iyr-1900 ! century info in 3 digits
      write(header(10:35),'(i3,1x,i3,4(1x,i2),1x,f6.3)')k,idoy,
     *imon,idy,ihr,imin,sec
      write(header(81:160),'(a80)')comment(1:80)
c
c   write out
c     
      do 70, i=1,13
        j=(i-1)*80+1
        write(4,'(a80)')header(j:j+79)   
 70   continue      

c
c write GSE2 output
c

c
c PAZ
c    
      elseif (gse) then

c
c   get def file for station codes, give file name
c
        deffile='gsesei.def'
        no_net = .FALSE.
        net_code=' '
        call read_def_chan(deffile,mainhead_text,net_code)
        gse_stage=1

c
c header line, CAL2 
c 
        gsetext(1:96)='                                        '
     &// '                                                        '
        gsetext(1:4)='CAL2'
        gsetext(6:10)=sta(1:5)

c
c convert component to GSE
c
        call set_def_chan(1,sta,comp)
        if (seiclen(comp).gt.3) then
          gsetext(12:13)=comp(1:2)
          gsetext(14:14)=comp(4:4)
        else
          gsetext(12:14)=comp(1:3)
        endif
        gsetext(21:26)=gse_cal2_instype

        if (outtyp.eq.3) then
c         write(gsetext(28:42),'(e10.2)') 1e9/amp1hz        ! GSE2.1
          write(gsetext(28:37),'(e10.2)') 1e9/amp1hz        
        else
c         write(gsetext(28:42),'(e10.2)') 1.                ! GSE2.1
c
c product of all stage scaling factors, in nm/count
c
          write(gsetext(28:37),'(e10.2)') 1e9/amp1hz        
c changed lo, july 13, 2001
c          write(gsetext(28:37),'(e10.2)') 
c     &           1/(gse_paz2_sfactor*gse_dig2_sensitivity)
        endif
        write(gsetext(44:45),'(a2)') '1.'
c sample rate, added lo July 13, 2001
        write(gsetext(47:56),'(f10.5)') gse_rate

        write(gsetext(58:67),'(i4.4,a1,i2.2,a1,i2.2)') 
     &       iyr,'/',imon,'/',idy
        write(gsetext(69:73),'(i2.2,a1,i2.2)') ihr,':',imin

        write(4,'(a96)') gsetext
c
c paz line, stage 1
c

        gsetext(1:96)='                                        '
     &// '                                                        '
        if (outtyp.eq.3) then
          gsetext(1:6)='FAP2  '
          gsetext(26:27)='60'
ccc changed lo 7/12/2010
        elseif (outtyp.eq.4) then
          gsetext(1:6)='PAZ2  '
          write(gsetext(11:25),'(e15.8)') gse_paz2_sfactor
          write(gsetext(41:43),'(i3)') npol
          write(gsetext(45:47),'(i3)') nzero
          write(gsetext(49:65),'(a17)') 'Laplace transform'
        endif
        write(gsetext(7:7),'(i1)') gse_stage
        gsetext(9:9)='V'             ! Volts
        write(4,'(a96)') gsetext
        if (outtyp.eq.3) then
          do i=1,60
            write(4,'(1x,f10.5,1x,e15.8,1x,i4)') f(i),amp(i),
     &            int(phase(i))
          enddo 
        else
          do i=1,npol
            write(4,'(1x,e15.8,1x,e15.8)') real(pol(i)),
     &      aimag(pol(i))
          enddo
          do i=1,nzero
            write(4,'(1x,e15.8,1x,e15.8)') real(zero(i)),
     &      aimag(zero(i))
          enddo
        endif
c
c digitizer DIG2, stage 2
c
        gse_stage=gse_stage+1
        if (outtyp.eq.4) then
          gsetext(1:96)='                                        '
     &// '                                                        '
          gsetext(1:6)='DIG2  '
          write(gsetext(7:7),'(i1)') gse_stage
          write(gsetext(9:23),'(e15.8)') gse_dig2_sensitivity
          write(gsetext(25:35),'(f11.5)') gse_dig2_samprat
          gsetext(37:61)=gse_dig2_description

          write(4,'(a96)') gsetext
        endif
      endif
c
c fir filters
c
      if (nfirstage.gt.0) then
        do i=1,nfirstage
          gse_stage=gse_stage+1
          do k=1,nfir(i)
            rfir(k)=fir(i,k)
          enddo
          if(abs(1.-normalize_fir(nfir(i),rfir,fir_rate(i))).gt..01)then
            write(*,*) ' Warning: fir filters not normalized '
          endif
          if (nfir(i).gt.0) then
            gsetext(1:80) = 
     &          '                                        ' //
     &          '                                        '
            gsetext(1:4)='FIR2'
            write(gsetext(6:7),'(i2)') gse_stage
            write(gsetext(9:18),'(e10.2)') 1. ! changed lot12/2/2008
c     &   normalize_fir(nfir(i),rfir,fir_rate(i))
            write(gsetext(20:23),'(i4)') fir_deci(i)
            write(gsetext(25:32),'(f8.3)')delay_fir(nfir(i),fir_rate(i))
c            gsetext(34:34) = sym_fir(nfir(i),rfir)
            gsetext(34:34) = fir_sym(i)
            write(gsetext(36:39),'(i4)') nfir(i)
            write(4,'(a80)') gsetext(1:80)
            do j=1,nfir(i),5
              gsetext(1:80) = 
     &          '                                        ' //
     &          '                                        '
              write(gsetext(1:80),'(5(1x,e15.8))')
     &           fir(i,j),fir(i,j+1),fir(i,j+2),fir(i,j+3),fir(i,j+4)
c
c remove 0s from last line
c
              if (j.eq.nfir(i).and.mod(nfir(i),5).ne.0) then
                do k=mod(nfir(i),5),4
                  write(gsetext(16*k+1:16*k+16),'(a16)')
     &              '                '
                enddo
              endif
              write(4,'(a80)') gsetext(1:80)
            enddo
          endif
        enddo
      endif
       
      close(4)
c
c   plot
c
      call systemc('presp '//file(1:seiclen(file)),seiclen(file)+6)
c
C------------------------------------------------------------------------
C parameters for recalculation   
C-------------------------------------------------------------------------
c
cxx 999  CONTINUE
      recal=.false.
 990  continue
      write(6,*)' Constants used:'
      write(6,*)
      WRITE(6,'(a,f13.2)')' 1: SEISMOMETER PERIOD=', PERIOD
      WRITE(6,'(a,f13.2)')' 2: GENERATOR CONSTANT=', GENCON
      WRITE(6,'(a,f13.2)')' 3: DAMPING RATIO     =', DAMPIN
      WRITE(6,'(a,f13.2)')' 4: AMPLIFIER GAIN(DB)=', GAIN
      WRITE(6,'(a,f13.0)')' 5: RECORDING GAIN=    ', REGAIN
      write(6,'(a,a,a)')  ' 6: STATION AND COMP=  ', sta,comp
      write(6,'(a,i4,5i2)')  ' 7: DATE AND TIME=     ',
     *                         iyr,imon,idy,ihr,imin,isec
      IF(NFILT.GE.1) THEN
         WRITE(6,'(a)')   ' 8: FILTER CONSTANTS' 
         WRITE(6,200) (FFILT(I),POLE(I),I=1,NFILT)
      ENDIF
      write(6,*)
      write(6,*)
     *' Run again: Enter number to change value, enter to end'
      write(6,*)' End: Enter' 
c
      read(5,'(a)') text
      if(text(1:1).ne.' ') then
         read(text(1:1),'(i1)') k
         if(k.gt.8) then
            write(6,*)' Not a valid number'
            goto 990
         endif
         recal=.true.
         if(k.eq.1) then
            write(6,*)' SEISMOMETER NATURAL PERIOD'
            read(5,*) PERIOD
            goto 990
         endif
         if(k.eq.2) then
            write(6,*)' SENSOR LOADED GENERATOR CONSTANT'
            read(5,*) GENCON
            goto 990
         endif
         if(k.eq.3) then
            write(6,*)' SEISMOMETER DAMPING RATIO'
            read(5,*) DAMPIN
            goto 990
         endif
         if(k.eq.4) then
            write(6,*)' AMPLIFIER GAIN(DB)'
            read(5,*) GAIN
            goto 990
         endif
         if(k.eq.5) then
            write(6,*)' RECORDING MEDIA GAIN'
            read(5,*) REGAIN
            goto 990
         endif
         if(k.eq.6) then
            print*,'Enter station code. e.g. BERGE, max 5 chars '
            read(5,'(a)')sta
 667        continue
            print*,'Enter component (4 chars) e.g. SL Z'
            print*,
     *      '  First character is type, must be one of the following: '
            print*,'  S: Short period, L: Long period'
            print*,'  B: Broad band,   A: Accelerometer'
            print*,'Last character must be Z,N or E'
            print*,'Character 2 and 3 can be anything'
            read(5,'(a4)')comp
            if( comp(1:1).ne.'S'.and.comp(1:1).ne.'L'.
     *      and.comp(1:1).ne.'B'.
     *      and.comp(1:1).ne.'A'.and.comp(4:4).ne.'N'.
     *      and.comp(4:4).ne.'E'.
     *      and.comp(4:4).ne.'Z') then
               write(6,*)' Wrong component, redo'
               goto 667
            endif
            if(sentyp.eq.3.and.comp(1:1).ne.'A') then
              comp(1:1)='A'
              write(6,*)
     *        ' Sensor is an accelerometer, first letter',
     *        ' of component must be A'
               write(6,'(a,a)') ' Component changed to ',comp
             endif
             goto 990
         endif
         if(k.eq.8) then
            WRITE(6,*) ' NUMBER OF FILTERS (0-10), RETURN FOR NONE ? '
            READ(5,'(a)') TEXT
            IF(TEXT(1:2).EQ.'  ') THEN
              NFILT=0
            ELSE
              CALL SEI GET VALUES(1,TEXT,CODE)
              NFILT=ARRAY$(1)
            ENDIF
            IF(NFILT.GT.0) THEN
               WRITE(6,*)
     *         ' FREQUENCY AND NUMBER OF POLES FOR EACH FILTER,'
               WRITE(6,*)' POLES NEGATIVE FOR HIGH PASS '
               DO  I=1,NFILT
                  READ(5,*) FFILT(I),POLE(I)
               enddo
            ENDIF
            goto 990
         endif
         if(k.eq.7) then
            write(6,'(1x,a,a,$)')
     *      'Enter date as YYYYMMDDHHMMSS, at least up to the day ',
     *      ' (e.g. 19880123):'
             read(5,'(i4,5i2)')iyr,imon,idy,ihr,imin,isec
             sec=float(isec)
c
c   get doy
c
             call date_doy(idoy,idy,imon,iyr)
             goto 990
         endif
      else
         if(recal) then
            goto 2000   ! recalculate
         endif
      endif
 999  continue

      write(6,*)' '
      write(6,'(a,a)')' Response file name is: ',file(1:seiclen(file))
      WRITE(6,*)' RESPONSE CURVE IS IN FILE resp.out'
      STOP
      END


c
      SUBROUTINE PLOTW(N1,N2,X1,Y1,X2,Y2,XTEXT,YTEXT,XLOG,YLOG)           
C
C   J. HAVSKOV SOMETIMES IN 1978 IN MEXICO WHEN WE HAD NO PLOTTER
C   HAS BEEN CHANGED A BIT FOR CURRENT VERSION (1990) 
C
C                                                                       
C   ROUTINE TO PLOT ON A LINE PRINTER 2 SETS OF DATA                    
C   Y AS A FUNCTION X ON ONE PAGE, DIMENSION NXDIM*NYDIM                     
C   THERE ARE N1 AND N2 POINTS IN THE TWO DATA SETS X1,Y1 AND           
C   X2,Y2 RESPECTIVELY. THE OUTLINE OF THE PLOT IS                      
C   DETERMINED BY THE MAX AND MIN VALUES IN THE DATA SET 2              
C   AND VALUES X1,Y1 OUTSIDE THE GRID WILL NOT BE USED.                 
C   IF N1 = 0 ,DATA SET X1-Y1 IS NOT USED                               
C   THE X AND Y VALUES CAN COME IN ANY ORDER, THEY                      
C   WILL AUTOMATICALLY BE SCALED.                                       
C   IF XLOG OR YLOG=1, LOG SCALE FOR AXIS NUMBERS, ELSE LINEAR
C                                                                       
C   number of digits to be printed must
c   be fixed in the the format statement. Likewise if numbers
c   printed out should be log or antilog
C                                                                       
      DIMENSION X1(*),Y1(*),X2(*),Y2(*)             
      DIMENSION YSC(41),XSC(11)
      CHARACTER*1 A(101,41)
      CHARACTER*8 YTEXT
      CHARACTER*6 XTEXT                                         
      INTEGER XLOG,YLOG
      XMAX=-1*10E10                                                     
      XMIN=-XMAX                                                        
      YMIN=XMIN
      YMAX=XMAX
C
C   DIMENSION OF PRINTING SURFACE FOR PLOT, ALSO CHANGE SOME
C   FORMAT STATEMENTS
C                                                         
      NXDIM=61
      NYDIM=19
C                                                                       
C   FIND EXTREMAS                                                       
C                                                                       
      DO 1 I=1,N2                                                       
      IF(X2(I).GT.XMAX) XMAX=X2(I)                                      
      IF(Y2(I).GT.YMAX) YMAX=Y2(I)                                      
      IF(X2(I).LT.XMIN) XMIN=X2(I)                                      
      IF(Y2(I).LT.YMIN) YMIN=Y2(I)                                      
c
c   blank array
c
 1    CONTINUE                                                          
      DO 29 I=1,NXDIM
      DO 29 K=1,NYDIM
      A(I,K)=' '
 29   CONTINUE                                                          
C                                                                       
C   PUT IN GRID                                                         
C                                                                       
       DO 50 I=nydim-4,1,-5                                                   
       DO 50 K=1,nxdim,2                                                   
       A(K,I)='.'                                                        
  50   CONTINUE                                                          
       DO 51 K=1,nxdim,10                                                  
       DO 51 I=1,nydim,2                                                    
       A(K,I)='.'                                                        
  51   CONTINUE                                                          
C                                                                       
C   SCALING                                                             
C                                                                       
C                                                                       
C   ENSURE THAT VERTICAL AND HORIZONTAL LINES CAN BE PLOTTED            
C                                                                       
      XMM=XMAX-XMIN                                                     
      YMM=YMAX-YMIN                                                     
      IF(XMM.EQ.0.0) XMIN=XMIN-1.0                                      
      IF(YMM.EQ.0.0)YMIN=YMIN-1.0                                       
      XR=(NXDIM-1)/(XMAX-XMIN)                                              
      YR=(NYDIM-1)/(YMAX-YMIN)                                               
      IF(XMM.EQ.0.0) XR=XR/2.0                                          
      IF(YMM.EQ.0.0) YR=YR/2.0                                          
C                                                                       
C   A-VALUES FOR X2-Y2 VARIABLES                                        
C                                                                       
      DO 12 I=1,N2                                                      
      IX=(X2(I)-XMIN)*XR+1.5                                            
      IY=(Y2(I)-YMIN)*YR+1.5                                            
      IY=NYDIM+1-IY                                                                                                     
      A(IX,IY)='+'
 12   CONTINUE                                                          
C                                                                       
C   X1 VARIABLES, REMOVE THE ONES OUTSIDE THE GRID                      
C                                                                       
      IF(N1.EQ.0) GO TO 5                                               
      DO 2 I=1,N1                                                       
         NX=(X1(I)-XMIN)*XR+1.5                                            
         IF(NX.GT.NXDIM.OR.NX.LT.1) GO TO 11                                 
         NY=(Y1(I)-YMIN)*YR+1.5                                            
         NY=NYDIM+1-NY                                                          
         IF(NY.GT.NYDIM.OR.NY.LT.1) GO TO 11                                  
         A(NX,NY)='O'
 11      CONTINUE                                                          
 2    CONTINUE 
C                                                         
 5    CONTINUE                                                          

C                                                                       
C   VALUES AT AXIS DIVISIONS                                            
C                                                                       
      DO 10 I=1,NYDIM
      YSC(NYDIM+1-I)=YMIN+(I-1)/YR                                           
 10   CONTINUE                                                          
      DO 15 I=1,NXDIM/10+1                                                      
      XSC(I)=XMIN+(I-1)*10.0/XR                                         
 15   CONTINUE                                                          
C                                                                       
C   PRINTING                                                            
C                                                                       
      WRITE(6,100) YTEXT                                                      
 100  FORMAT(2X,A8,2X,63('-'),5X)                               
 101  FORMAT(12X,63('-'),5X)                               
      DO 20 IL=1,NYDIM                                                 
      IF(YLOG.EQ.1) THEN
         WRITE(6,201)10**YSC(IL),(A(I,IL),I=1,NXDIM)                     
      ELSE
         WRITE(6,201)YSC(IL),(A(I,IL),I=1,NXDIM)                     
      ENDIF
 201  FORMAT(1X,G10.3,1X,'I',61A1,'I')                            
 20   CONTINUE                                                          
      WRITE(6,101)
      IF(XLOG.EQ.1) THEN                                                      
         WRITE(6,202) XTEXT,(10**XSC(I),I=1,NXDIM/10+1)
      ELSE
         WRITE(6,202) XTEXT,(XSC(I),I=1,NXDIM/10+1)    
      ENDIF
 202  FORMAT(2X,A6,11(F8.2,2X))                                           
      RETURN                                                            
      END
c
c
      subroutine read_poles
c
c   read poles and zeros
c
      implicit none
c
      real ppr(1000),ppi(1000)   ! poles,  real and im.
      real pzr(1000),pzi(1000)   ! zeros, real and im.
      complex pol(500),zero(500)  ! complex poles and zeros
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
      INTEGER I
c
      common /resp/npol,nzero,ppr,ppi,pzr,pzi,pol,zero,norm
c
c   read response values
c         
         read(20,*) npol,nzero,norm
         if(npol.gt.0) then
             do i=1,npol
               read(20,*) ppr(i),ppi(i)
             enddo
         endif
         if(nzero.gt.0) then
             do i=1,nzero
                read(20,*) pzr(i),pzi(i)
             enddo
         endif
c         write(6,*)npol,nzero,norm
c
c   convert to complex
c
         do i=1,npol
           pol(i)=cmplx(ppr(i),ppi(i))
         enddo
         do i=1,nzero
           zero(i)=cmplx(pzr(i),pzi(i))
         enddo
         return
         end

      character*1 function sym_fir(nfir,fir)
      implicit none
      double precision fir(*)
      integer nfir,i
      logical even
      logical sym

      if (mod(nfir,2).eq.0) then
        even=.true.
      else
        even=.false.
      endif
      sym=.true.
      do i=1,int(nfir/2)
        if (abs(fir(i)-fir(nfir-i+1)).gt.abs(0.01*fir(i))) then
          sym=.false.
        endif
      enddo
      if (.not.sym) then
        sym_fir='A'
      elseif (even) then
        sym_fir='C'
      else
        sym_fir='B'
      endif

c
c for the case of symmetric filters, only half the coeffs are stored
c
      if (sym_fir.eq.'B') nfir=(nfir+1)/2
      if (sym_fir.eq.'C') nfir=nfir/2

      return
      end

      real function delay_fir(nfir,rate)
c
c compute fir filter delay
c
      implicit none
      integer nfir
      real rate,delay
      
      if (mod(nfir,2).eq.0) then
        delay=float(nfir)/2.*1./rate
      else
        delay=float(nfir-1)/2.*1./rate
      endif
      delay_fir=delay
      return
      end

      real function normalize_fir(nfir,fir,rate)
      implicit none
      double precision fir(*)
      real rate,pi,f0,x,y,dt
      integer nfir
      integer a
      parameter (pi=3.141592654)
      complex hc(10000),resp,i

      f0=1.
      dt=1./rate   
cc      write(*,*) ' sample interval ',dt,nfir
      i=(0,1)
c
c normalize FIR filter
c
      resp=(0,0)
      do a=1,nfir
        resp=resp+fir(a)*exp(2.*pi*i*f0*dt) 
      enddo
      normalize_fir=1./cabs(resp)
c      write(*,*) cabs(resp)
c      do a=1,nfir
c        x=fir(a)*cos(2.*pi*f0*dt*float(a))
c        y=-1.*fir(a)*sin(2.*pi*f0*dt*float(a))
cc        x=fir(a)*cos(2.*pi*f0*dt)
cc        y=-1.*fir(a)*sin(2.*pi*f0*dt)
c        hc(a)=x+y*i
c      enddo
c      resp=(0,0)
c      do a=1,nfir
c        resp=resp+hc(a)
c      enddo
c      write(*,*) cabs(resp)
      return
      end


