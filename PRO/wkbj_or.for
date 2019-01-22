c
C WKBJ.PRG - program WKBJ
C
c  
c   updates
c   aug 94 by jh : fix input and output units, do not use 5 and 6
c   april 97 jh  : ---------   version 7.0 check, no change ------------
C
C-----------------------------------------------------------------------
C Program to compute WKBJ seismograms.
C-----------------------------------------------------------------------
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation numbers refer to that paper.  See also:
C
C         Chapman, C.H., 1978.  Geophys. J. R. astr. Soc., 54, 481-518.
C         A new method for computing synthetic seismograms.
C
C         Dey-Sarkar, S.K. and Chapman, C.H., 1978.  Bull. Seism.
C         Soc. Amer., 68, 1577-1593.  A simple method for the
C         computation of body-wave seismograms.
C
C         Chapman, C.H. and Orcutt, J.A., 1985. Rev. Geophys., 23,
C         105-163.  The computation of body wave synthetic seismograms
C         in laterally homogeneous media.
C
C
C
C         Note this is a very simple program intended only to illustrate
C         the use of the routines PXINT, COMPON, COEFF, THETAC and
C         CCSQRT.  The user will obviously need to add plotting
C         routines and many other features to make the program user
C         friendly and suitable for his/her application and computer
C         system.  Suggested modifications are:
C
C         (a) make input of model, ray definitions, seismogram ranges,
C             etc. more versatile and interactive.  Use format free
C             input.
C         (b) write travel-time tables to file to allow repeated use for
C             seismograms in a standard Earth model.
C         (c) write seismograms to file for repeated use with different
C             source functions, etc.
C         (d) plot model, travel-time tables and seismograms.
C         (e) include source/receiver convolutions in this program and
C             avoid using CCSQRT if its smoothing is too severe.
C         (f) interpolate slowly varying coefficients, e.g. transmission
C             coefficients, to make program more efficient (transmission
C             coefficients can already be set to unity by omitting from
C             ray definition).
C         (g) compute coefficients for all dynamic groups together to
C             avoid repeating calculations in COEFF.
C         (h) smooth ends of amplitude table to reduce end-point errors
C             (this is a cosmetic improvement and may not be real).
C         (i) include SH rays as option.
C         (j) give source and receiver in geographic coordinates and
C             include coordinate transformation, i.e. radiation pattern.
C         (k) include attenuation in model and apply as an approximate
C             convolution operator.
C
C         NOTE THIS PROGRAM DOES NOTHING USEFUL WITH THE SEISMOGRAMS.
C         AT THE VERY LEAST, THE USER WILL WANT TO PLOT OR WRITE THE
C         SEISMOGRAMS.
C
C Calls: RAYIN, PXINT, COMPON, COEFF, coefh, compoh, THETAC, CCSQRT
C
C Errors: None.  Note it is assumed that the input parameters are all
C         defined sensibly and are compatible.  The user should include
C         appropriate checks on the input parameters to make the program
C         user friendly.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: C.H.Chapman
C
C Date: revised version, 28 July 1986
C
C Local variables:
C
      parameter (nnrays=20,nnptst=40000,nndept=200,nnpp=200,nnstat=10)
      parameter (nnint=20,nncomp=29)
      INTEGER ITRAY(nnrays),IL(nnint),NPS(2,nnint),ID(10,nnint),
     & JMIN(2),JMID(2),
     1 JMAX(2),INM,INR,IMTH,NMOD,I,J,N,ILAY,II,IRAY,NN,MULT,ISS,IRR,
     2 IT,NUM,ICNR,ICNS,JMIN1,JMIN2,JMID1,JMID2,JMAX1,JMAX2,NUM1,K,IRTR,
     3 I1,I2,KK,NX,NT,NT5,IR,M
      REAL VEL(6,nndept),SEIS(nnptst),PT(nncomp,nnpp,nnrays),
     & FREE(3),ONE,PI2,RSUR,XCONV,
     1 Y,YI,X,XI,A,B,Z,ZI,T,PMIN,PMID,PMAX,DP1,DP2,RSOR,RREC,PP,XSTRT,
     2 DX,TSTRT(nnstat),DT,XST(nnstat),TST,RANGE,SYSCL,SMAXX
c    2 DX,TSTRT(nnstat),DT,UAPPR,XST(nnstat),TST,RANGE,SYSCL,SMAXX
      COMPLEX CSEIS(nnptst+5),COMP(13),COEFF,coefh,AMP,CAMP
      LOGICAL LSPHR,LDEPTH,LPM,LRAY,lsh(nnrays),lsh0
      EQUIVALENCE (JMIN(1),JMIN1),(JMIN(2),JMIN2),(JMID(1),JMID1),
     1 (JMID(2),JMID2),(JMAX(1),JMAX1),(JMAX(2),JMAX2)
      DATA ONE/1.0000001/,PI2/1.570796/,FREE/0.,0.,0./
c
c     output unit
      iout=4
      open(iout,file='wkbj.out',status='unknown')
C
C Read in model parameters
C
C***********************************************************************
C
C (1) FORMAT (312,I4,2L2,F10.2)
C
C INM    , I2    , data set number for model input (see input (2))
C INR    , I2    , data set number for ray input (see input (3))
C
C IMTH   , I2    , controls interpolation of velocity model
C                  IMTH = 1 , v(z)=1./sqrt(a-2*b*z) (fastest)
C                  IMTH = 2 , v(z)=a-b*z
C                  IMTH = 3 , v(z)=a*exp(-b*z) equivalent to v(r)=a*r**b
C                  (see equation (13))
C
C NMOD   , I4    , number of points in model (see input (2) - storage
C                  must be at least NMOD+1)
C LSPHR  , L2    , = T for spherical model
C                  = F for flat model
C LDEPTH , L2    , = T to convert input data of depths to radii if
C                  LSPHR=T, or convert input data of heights to
C                  depths if LSPHR=F , using RSUR.
C                  = F for no conversions.
C RSUR   , F10.2 , radius of Earth (used if LDEPTH=T) and/or reference
C                  radius in Earth Flattening Transformation (used if
C                  LSPHR=T).
C
C***********************************************************************
C
c
c   hardwire input and output units and open
c
      inm=2
      inr=2
      open(2,file='wkbj.inp',status='unknown')
      open(3,file='wkbj.tab',status='unknown')
      READ (inm,1000) Ibid,MIbid,IMTH,NMOD,LSPHR,LDEPTH,RSUR
      write(3,'(2l2)') lsphr,ldepth
C
C read in model
C
C***********************************************************************
C
C (2) FORMAT (4F10.5) - NMOD records on data set number INM
C                       (see input (1))
C
C VEL(1,I) , F10.5 , radius , depth or height ) (see input (1) and
C                                             )  LSPHR, LDEPTH)
C VEL(2,I) , F10.5 , P wave velocity          )
C VEL(3,I) , F10.5 , S wave velocity          ) I=1,NMOD
C VEL(4,I) , F10.5 , density                  )
C
C The model should be ordered from the free-surface downwards.
C
C***********************************************************************
C
      READ (INM,1001) ((VEL(J,I),J=1,4),I=1,NMOD)
C
C Convert model to internal format - see PXINT
C
C Change coordinate direction
C
      IF (.NOT.LDEPTH) GO TO 1
      DO 2 I=1,NMOD
2     VEL(1,I)=RSUR-VEL(1,I)
C
C Conversion factor for horizontal coordinate.
C
1     XCONV=1.
      IF (LSPHR) XCONV=RSUR/57.29578
C
C Remove r=0 in spherical model and add dummy interface at bottom
C
      IF (LSPHR.AND.VEL(1,NMOD).EQ.0.) NMOD=NMOD-1
      N=NMOD+1
      DO 3 J=1,4
3     VEL(J,N)=VEL(J,NMOD)
C
C Loop to do Earth flattening transformation, convert to slownesses,
C find interfaces, compute interpolation coefficients, and print model.
C
      ILAY=0
      WRITE (3,1004)
      YI=VEL(1,1)
      DO 4 I=1,N
          Y=YI
          YI=VEL(1,I)
          A=VEL(2,I)
          B=VEL(3,I)
          IF (.NOT.LSPHR) GO TO 5
          X=RSUR/YI
          VEL(1,I)=RSUR*ALOG(X)
          VEL(2,I)=X*A
          VEL(3,I)=X*B
5         VEL(2,I)=1./VEL(2,I)
          IF (VEL(3,I).NE.0.) VEL(3,I)=1./VEL(3,I)
          IF (Y.NE.YI) GO TO 6
          ILAY=ILAY+1
          IL(ILAY)=I
          WRITE (3,1005) ILAY
          ZI=VEL(1,I)
          GO TO 4
C
C Interpolation coefficients
C
6         II=I-1
          Z=ZI
          ZI=VEL(1,I)
          DO 10 J=2,3
               X=VEL(J,II)
               XI=VEL(J,I)
C
C This test is introduced for a fluid layer (X=XI=0) but is also
C good for any method with a homogeneous interval (X=XI)
C
              IF (X.EQ.XI) GO TO 11
              GO TO (11,12,13),IMTH
11            VEL(J+3,II)=.5*(X-XI)*(X+XI)/(Z-ZI)
              GO TO 10
12            VEL(J+3,II)=(XI-X)/(Z-ZI)/X/XI
              GO TO 10
13            VEL(J+3,II)=ALOG(XI/X)/(Z-ZI)
C
10        CONTINUE
C
4     WRITE (3,1010) I,YI,ZI,A,VEL(2,I),B,VEL(3,I),VEL(4,I)
C
C At this point the user may want to plot the model.
C
C
C Travel time section.
C
      IRAY=0
C
C read ray definition
C
C***********************************************************************
C
C (3) Read ray definition on data set number INR (see input (1))
C
C See subroutine RAYIN for FORMAT of record(s).
C
C***********************************************************************
C
100   CALL RAYIN (NPS,ID,NN,MULT,ISS,IRR,LRAY,lsh0,INR)
      IF (.NOT.LRAY) GO TO 130
C
C New kinematic group.
C
      IF (IRAY.EQ.0) GO TO 102
C
C Print previous travel-time table.
C
C Note we can only print this now as we have been summing the amplitude
C functions of dynamic groups.
C
C We print the first component of the amplitude function C(p) to
C allow simple check on gradients and discontinuities.
C
      IT=ITRAY(IRAY)
      IF (IT.EQ.0) GO TO 103
      WRITE (3,1103)
      DO 110 I=1,IT
          T=PT(2,I,IRAY)+PT(1,I,IRAY)*PT(3,I,IRAY)
          PT(3,I,IRAY)=PT(3,I,IRAY)/XCONV
          WRITE (3,1110) PT(1,I,IRAY),T,PT(3,I,IRAY),PT(2,I,IRAY),
     1    PT(4,I,IRAY),PT(5,I,IRAY)
110   CONTINUE
C
102   IRAY=IRAY+1
      lsh(iray)=lsh0
103   IF (NN.EQ.0) GO TO 200
C
C read ray parameters
C
C***********************************************************************
C
C (4) FORMAT (3I4,3(I2,I4),3F10.5)
C
C NUM     , I4    , number of ray parameters in travel-time table
C ICNR    , I4    , receiver depth model index
C ICNS    , I4    , source depth model index
C                   These are the indices of the source and receiver
C                   positions in the model i.e. for the free surface
C                   we would have ICNR = ICNS = 1.  They correspond to
C                   the index I in input (2).
C
C The next 6 variables specify the range of ray parameters using a
C minimum, intermediate (see below) and maximum value.  They must be
C in increasing order.
C
C 2 methods can be used:
C
C    (a) specify the ray parameter using the model indices
C    (b) specify the ray parameter in s/km
C
C Either method can be used for the minimum, intermediate and
C maximum ray parameters.
C For method (a), the variables JMIN(2), JMID(2) and/or JMAX(2) are used,
C and for method (b), PMIN, PMID and/or PMAX are specified (and JMIN,
C JMID and/or JMAX are set zero).
C Using method (a), the ray parameter is set equal to a slowness in the
C model.  This may be used to specify the exact ray parameter for a
C grazing turning ray or a head wave, etc. without needing to enter the
C exact numerical value.  It is necessary to specify the slowness type
C and model index (corresponding to the index I in input (2)) to give
C the ray parameter. Thus:
C
C JMIN(1) , I2    , slowness type for minimum ray parameter
C                   (1=P ray , 2=S ray , 0 if method (b) is used)
C JMIN(2) , I4    , model index for minimum ray parameter
C                   (if method (a) is used)
C                   Then PMIN=1/VEL(JMIN(1)+1,JMIN(2)) where VEL is
C                   the input array from input (2).
C JMID(1) , I2    , as JMIN(1) for intermediate ray parameter
C JMID(2) , I4    , as JMIN(2) for intermediate ray parameter
C JMAX(1) , I2    , as JMIN(1) for maximum ray parameter
C JMAX(2) , I4    , as JMIN(2) for maximum ray parameter
C
C PMIN    , F10.5 , minimum ray parameter in s/km (if (b) is used)
C PMID    , F10.5 , intermediate ray parameter in s/km (if (b) used)
C PMAX    , F10.5 , maximum ray parameter in s/km (if (b) is used)
C
C The following is a description of the algorithm used to distribute
C the NUM ray parameters and the reasons behind the choice.
C The range of ray parameters, PMIN to PMAX, is sub-divided to give
C NUM points.
C In general the density of points is increased (as an inverse
C square root) near the intermediate and maximum ray parameters, PMID
C and PMAX, to sample the singularities in the reflection coefficient
C and/or range function better. If the intermediate ray
C parameter is a critical value, this allows the square root
C discontinuities of the reflection coefficient to be sampled
C better (on both sides of the critical value), and if the
C maximum ray parameter is a grazing value, this allows the
C singularity of X'(p) to be sampled better. The NUM points are
C divided between the 2 intervals so that near the intermediate
C value, points are approximately symmetric. The algorithm remains
C valid if the intermediate and maximum ray parameters are equal, when
C the density is increased near the maximum value.  If the minimum and
C intermediate ray parameters are equal, then a different algorithm
C is used and the ray parameters are distributed uniformly from
C PMIN=PMID to PMAX.
C
C Note the ray parameters will determine the ray type (see subroutine
C RAYIN). For p = 0. to the grazing value we will have a reflection,
C for p = grazing value to the maximum slowness in layer we will have a
C turning ray, and if p is greater than the maximum layer
C slowness, the ray will not exist and will be omitted from table.
C It is important that the user chooses the range of ray parameters
C sensibly for the ray under consideration as no corrections are made
C by the program.
C
C
C Repeat inputs (3) and (4) as required for different rays.
C Omit input (4) if LRAY=F on input (3), i.e. a new dynamic group is
C defined which is kinematically equivalent to previous ray. The same
C ray parameters will be used for all dynamic groups in a kinematic
C group and the amplitudes are summed.
C
C
C (5) FORMAT (L1,I3) - to end travel-time calculations
C
C LRAY , L1 , must be T (.TRUE.)
C NN   , I3 , must be 0
C
C***********************************************************************
C
      READ (inm,1111) NUM,ICNR,ICNS,JMIN,JMID,JMAX,PMIN,PMID,PMAX
C
C p range - use ONE to avoid rounding errors putting limiting rays
C out of range.
C
      IF (JMIN1.NE.0) PMIN=VEL(JMIN1+1,JMIN2)*ONE
      IF (JMID1.NE.0) PMID=VEL(JMID1+1,JMID2)
      IF (JMAX1.NE.0) PMAX=VEL(JMAX1+1,JMAX2)/ONE
C
C Interpolation of p range.
C
      LPM=PMIN.GE.PMID
      IF (LPM) GO TO 111
C
C Cosine interpolation from PMIN to PMID , and PMID to PMAX (LPM=F)
C
      NUM1=IFIX(ONE*FLOAT(NUM-1)/(1.+SQRT(2.*AMAX1(0.,PMAX-PMID)/
     1 (PMID-PMIN))))+1
      NUM1=MIN0(NUM,MAX0(2,NUM1))
      DP1=PI2/FLOAT(NUM1-1)
      IF (NUM1.LT.NUM) DP2=PI2/FLOAT(NUM-NUM1)
      GO TO 120
C
C Linear interpolation from PMIN=PMID to PMAX (LPM=T)
C
111   DP1=(PMAX-PMIN)/FLOAT(NUM-1)
      NUM1=NUM
C
C Set up travel-time table - zero amplitude
C
120   ITRAY(IRAY)=NUM
      DO 121 I=1,nnpp
      DO 121 J=4,nncomp
          PT(J,I,IRAY)=0.
121   CONTINUE
      RSOR=VEL(1,ICNS)
      RREC=VEL(1,ICNR)
      IF (.NOT.LSPHR) GO TO 122
      RSOR=RSUR*EXP(-RSOR/RSUR)
      RREC=RSUR*EXP(-RREC/RSUR)
122   WRITE (3,1122) RSOR,ISS,RREC,IRR
C
C New kinematic or dynamic group set up - compute table
C
130   NUM=ITRAY(IRAY)
      IT=1
      WRITE (3,1130) LRAY,MULT,(K,NPS(1,K),NPS(2,K),(ID(J,K),J=1,10),
     1 K=1,NN)
      IF (NUM.EQ.0) GO TO 100
      DO 131 I=1,NUM
          IF (.NOT.LRAY) GO TO 140
C
C Compute p value
C
          IF (I.EQ.NUM) GO TO 132
          IF (LPM) GO TO 133
          IF (I.LT.NUM1) GO TO 134
          PP=PMID+(PMAX-PMID)*COS(FLOAT(NUM-I)*DP2)**2
          IF (I.EQ.NUM1) PP=PMID
          GO TO 135
134       PP=PMIN+(PMID-PMIN)*COS(FLOAT(NUM1-I)*DP1)
          IF (I.EQ.1) PP=PMIN
          GO TO 135
132       PP=PMAX
          GO TO 135
133       PP=PMIN+FLOAT(I-1)*DP1
C
C Compute tau(p) and X(p)
C
135       CALL PXINT (PP,VEL,6,IL,NPS,NN,IMTH,
     1                PT(2,IT,IRAY),PT(3,IT,IRAY),IRTR)
          PT(1,IT,IRAY)=PP
C
C Test return - note IT used to count rather than I
C
          GO TO (140,131),IRTR
C
C Now do dynamic properties
C
140       PP=PT(1,IT,IRAY)
c
c ...................P-SV waves
C
C Free surface coefficients and multiplicity
C
          AMP=COEFF(PP,FREE,VEL(2,1),ID)
          AMP=CMPLX(FLOAT(MULT)*REAL(AMP),FLOAT(MULT)*AIMAG(AMP))
C
C Loop over interface coefficients
C
          DO 141 J=2,NN
              I1=IL(J)
              I2=I1-1
              AMP=AMP*COEFF(PP,VEL(2,I2),VEL(2,I1),ID(1,J))
141       CONTINUE
C
C Source and receiver components.
C
          CALL COMPON (PP,VEL(2,ICNS),ISS,VEL(2,ICNR),IRR,COMP)
          KK=3
C
C Add to previous amplitude
C
          DO 142 K=1,10
              CAMP=AMP*COMP(K)
              KK=KK+1
              PT(KK,IT,IRAY)=PT(KK,IT,IRAY)+REAL(CAMP)
              KK=KK+1
              PT(KK,IT,IRAY)=PT(KK,IT,IRAY)+AIMAG(CAMP)
142       CONTINUE
c
c ...................  SH waves
          if (lsh(iray))  then
C
C Multiplicity (Free surface reflection=1.; no need to calculate)
C
          AMP=cmplx(1.,0.)
          AMP=CMPLX(FLOAT(MULT)*REAL(AMP),FLOAT(MULT)*AIMAG(AMP))
C
C Loop over interface coefficients
C
          DO 151 J=2,NN
              I1=IL(J)
              I2=I1-1
              AMP=AMP*COEFH(PP,VEL(2,I2),VEL(2,I1),ID(1,J))
151       CONTINUE
C
C Source and receiver components.
C
          CALL COMPOH (PP,VEL(2,ICNS),ISS,VEL(2,ICNR),IRR,COMP(11))
C
C Add to previous amplitude
C
          DO 152 K=11,13
              CAMP=AMP*COMP(K)
              KK=KK+1
              PT(KK,IT,IRAY)=PT(KK,IT,IRAY)+REAL(CAMP)
              KK=KK+1
              PT(KK,IT,IRAY)=PT(KK,IT,IRAY)+AIMAG(CAMP)
152       CONTINUE
c
          endif
C
C Increment table count
C
          IT=IT+1
C
131   CONTINUE
C
      ITRAY(IRAY)=IT-1
      GO TO 100
C
C Seismogram section.
C
C At this point the user may want to plot the travel-time tables or
C to write them to a file.
C
200   CONTINUE
C
C Input seismogram parameters
C
C**********************************************************************
C
C (6) FORMAT (2(2F8.2,I4),F10.5)
C
C The units of the variables are degrees if LSPHR=T and km if LSPHR=F
C
C XSTRT , F8.2  , initial range in degrees or km
C DX    , F8.2  , range step in degrees or km (should be non-zero
C                 even if NX=1)
C NX    , I4    , number of ranges
C TSTRT , F8.2  , initial time of seismogram at XSTRT in sec
C DT    , F8.2  , time step of discrete seismogram in sec
C NT    , I4    , number of points in time series
C UAPPR , F10.5 , reducing slowness in s/deg or s/km used to shift
C                 initial time point of successive seismograms by
C                 DX*UAPPR
C
C***********************************************************************
C
c     READ (5,1200) XSTRT,DX,NX,TSTRT,DT,NT,UAPPR
c  modif VM. to accomodate more varied source-station distances 
      READ (inm,1201) NX,(XST(I),I=1,NX)
      READ (inm,1202) (TSTRT(I),I=1,NX)
      READ (inm,1203) DT,NT
      NT5=NT+5
C
C Loop over ranges
C
      DO 201 I=1,NX
c         XST=XSTRT+FLOAT(I-1)*DX
c         TST=TSTRT+(XST-XSTRT)*UAPPR
          TST=TSTRT(I)
          RANGE=XST(I)*XCONV
C
C Loop over 13 components
C
          DO 210 K=1,13
              KK=K+K+2
C
C Zero seismogram
C
              DO 211 J=1,NT5
                  CSEIS(J)=CMPLX(0.,0.)
211           CONTINUE
C
C Loop over kinemtic groups
C
c  modif by V. Maupin, May 1994: changed IRAY into IRAY-1 in loop 212
              DO 212 IR=1,IRAY-1
                  CALL THETAC (PT(1,1,IR),PT(2,1,IR),PT(KK,1,IR),
     1                         ITRAY(IR),nncomp,RANGE,TST,DT,NT,CSEIS)
212           CONTINUE
C
C Hilbert transformation in spherical model - see
C Chapman, C.H., 1979. On impulsive wave propagation in a spherically
C symmetric model, Geophys. J. R. astr. Soc., 58, 229-234.
C
              IF (.NOT.LSPHR) GO TO 230
              M=MOD(IFIX(XST(I)/180.),4)
              IF (M.EQ.0) GO TO 230
              IF (M.EQ.2) GO TO 220
C
C Multiply by (0.,-1.) if M=1 or 3 .
C
              DO 221 J=1,NT5
                  CSEIS(J)=CMPLX(-AIMAG(CSEIS(J)),REAL(CSEIS(J)))
221           CONTINUE
              IF (M.EQ.1) GO TO 230
C
C Multiply by -1. if M=2 or 3 .
C
220           DO 222 J=1,NT5
                  CSEIS(J)=-CSEIS(J)
222           CONTINUE
C
C Do convolution with d/dt Im( 1/sqrt(t) *
C
230           CALL CCSQRT (CSEIS,NT,SEIS)
C
C Range, thetac, ccsqrt and constant factors.
C Normalize by 1/12/sqrt(DT*X) = DT/(6*SQRT(DT))/(2*DT)/SQRT(X).
C DT is to make pulse amplitude independent of DT (true amplitude
C inversely proportional to DT as pulse width proportional to DT).
C Factor 1/(6*SQRT(DT)) from subroutine CCSQRT.
C Factor 1/(2*DT) from subroutine THETAC.
C
              IF (.NOT.LSPHR) SYSCL=12.*SQRT(RANGE*DT)
C
C Additional normalization in spherical model:
C R**2                  from Legendre transform,
C 1/RSOR                from WKBJ at source,
C 1/R                   from WKBJ at receiver,
C 1/SQRT(R*SIN(THETA))  from Legendre function,
C (RSUR/RSOR)**3/2      from EFT of slowness at source,
C (RSUR/R)**1/2         from EFT of slowness at receiver,
C (RSUR/R)**3/2         from EFT of horizontal slowness in integral,
C SQRT(RSUR**7/RSOR**5/R**3/SQRT(THETA)) total.
C
C Note the force source only has:
C (RSUR/RSOR)**1/2      from EFT of slowness at source,
C SQRT(RSUR**5/RSOR/R**3/SQRT(THETA)) total.
C
C See Chapman, C.H. and Orcutt, J.A., 1985.  The computation of body
C wave synthetic seismograms in laterally homogeneous media, Rev.
C Geophys., 23, 105-163.
C
              IF (LSPHR) SYSCL=12.*(RSOR/RSUR)**2*RREC*
     1        SQRT(ABS(SIN(RANGE/RSUR))*RSOR*RREC*DT/RSUR)/RSUR
C
C Scale seismogram and find maximum
C
              SMAXX=0.
              DO 231 J=1,NT
                  SEIS(J)=SEIS(J)/SYSCL
                  SMAXX=AMAX1(SMAXX,ABS(SEIS(J)))
231           CONTINUE
              WRITE (3,1231) XST(I),K,TST,SMAXX
C
C At this point the user will want to plot the seismogram, write to
C a file, or combine various source components
C
              write (iout,1240) i,xst(i),k,tst,dt,nt,smaxx
              write (iout,1241) (seis(j),j=1,nt)
C
C End of loop over components
C
210       CONTINUE
C
C End of loop over ranges
C
201   CONTINUE
C
      STOP
C
1000  FORMAT (3I2,I4,2L2,F10.2)
1001  FORMAT (4F10.5)
1004  FORMAT ('1EARTH MODEL'/' INPUT DEPTH  INTERNAL  P VELOCITY P SLOWN
     1ESS S VELOCITY S SLOWNESS DENSITY'/' OR RADIUS KM DEPTH KM',8X,
     2 'KM/S',7X,'S/KM',7X,'KM/S',7X,'S/KM MG/M**3')
1005  FORMAT (' INTERFACE',I3)
1010  FORMAT (I4,F8.2,F10.2,F8.3,G14.6,F8.3,G14.6,F8.3)
1103  FORMAT ('0P S/KM',11X,'T(P) S',9X,'X(P) KM OR DEG TAU(P) S',
     1 7X,'AMPLITUDE')
1110  FORMAT (G13.5,3F15.4,2G11.3)
1111  FORMAT (3I4,3(I2,I4),3F10.5)
1122  FORMAT ('1RAY DEFINITION'/' SOURCE DEPTH/RADIUS',F10.2,' TYPE',
     1 I3,' RECEIVER DEPTH/RADIUS',F10.2,' TYPE',I3)
1130  FORMAT (' KINEMATIC GROUP',L2,' MULTIPLICITY',I3/15X,
     1 'NP NS P1P1 P1P2 P2P2 S1S1 S1S2 S2S2 P1S1 P1S2 P2S1 P2S2'/
     2 26X,'P2P1',11X,'S2S1',6X,'S1P1 S2P1 S1P2 S2P2'/
     3 (' INTERFACE',I4,2I3,10I5))
1200  FORMAT (2(2F8.2,I4),F10.5)
1201  FORMAT (i2,10f7.1)
1202  FORMAT (10f7.2)
1203  FORMAT (F8.2,I4)
1231  FORMAT (' SEIS RANGE',F8.2,' COMP',I3,' INIT TIME',F15.4,
     .' SEC, MAX AMP',G13.5)
1240  format (i3,f8.2,i3,2f15.4,i5,g13.5)
1241  format (5g13.5)
C
      END                                                               WKBJ



      SUBROUTINE RAYIN (NPS,ID,NN,MULT,IS,IR,LRAY,lsh0,IN)               RAYIN
C
C-----------------------------------------------------------------------
C Routine to read ray definition.
C-----------------------------------------------------------------------
C
C Input: IN         = input data set number
C
C Output: NPS(2,*)  = number of rays segments
C                     NPS(1,l) = number of P ray segments in l-th layer
C                     NPS(2,l) = number of SV ray segments in l-th layer
C                     i.e. n_Ìalpha lË and n_Ìbeta lË
C         ID(10,*)  = multiplicity of coefficients
C                     ID(k,l) = multiplicity of k-th coefficient at
C                               l-th interface, i.e. nu_ÌlkË
C         NN        = number of layers and interfaces to define ray
C         MULT      = multiplicity of ray in dynamic group
C         IS        = source type index
C         IR        = receiver type index
C         LRAY      = kinematic/dynamic group flag
C                     LRAY=.true. for new kinematic group
C                     LRAY=.false. for new dynamic group within old
C                     kinematic group.
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equations and figures refer to that paper.
C
C Calls: none
C
C Errors: It is assumed that the input data is correct.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: C.H.Chapman
C
C Date: revised version, 28 July 1986
C
C Arguments:
C
      INTEGER NPS(2,*),ID(10,*),NN,MULT,IS,IR,IN
      LOGICAL LRAY,lsh0
C
C Local variables:
C
      INTEGER NPX(2,5),IDX(10,5),N(5),I,J,K,L,KK,NI,NK
C
C Read ray in integer format
C
C***********************************************************************
C
C (3) FORMAT (L1,I3,3I2,5(I2,12I1))
C
C This record(s) defines the kinematic and dynamic properties of a
C dynamic group of rays.
C
C The record(s) has ray type identification in cols 1-10 and then each
C layer/interface is described by its index plus 12 integers (14
C columns per layer/interface).  The layer/interface index runs
C from 1 to NN (but see notes below about repeating or omitting
C a layer and continuing records).
C
C Note the l-th interface is at the top of the l-th layer.
C
C See figure 2.
C
C
C COL 1    (L1) LRAY=T new kinematic group
C                    F dynamic group within preceding kinematic group
C
C COL 2-4  (I3) NN=number of layers to define ray including 'half-space'
C               Note the ray will either turn in the (NN-1)-th layer
C               or be reflected by the NN-TH interface. Both the turning
C               ray and the reflection have the same specification, i.e.
c               both include the reflection coefficients at the NN-th
C               interface. The type of ray is controlled by the ray
C               parameter.
C
C COL 5-6  (I2) MULT=multiplicity of ray in dynamic group
C
C COL 7-8  (I2) IS=source index - 1 is P down, 2 S down, 5 P up, 6 S up
C COL 9-10 (I2) IR=receiver index - 1 is P up, 2 is S up, 5 is P down,
C                                   6 is S down (in material), 9 is P up
C                                   and 10 S up (at free surface)
C
C See subroutine COMPON and figure 4.
C
C
C The following 14 columns are repeated for l=1,NN (but see below).
C See subroutines PXINT and COEFF and figures 2 and 3.
C
C COL 11-12 (I2)   l layer/interface index
C
C COL 13    (I1)   NPS(1,l) number of P segments in l-th layer
C COL 14    (I1)   NPS(2,l) number of S segments in l-th layer
C                  See figure 2 - NPS(1,l) is n_Ìalpha lË and
C                                 NPS(2,l) is n_Ìbeta lË.
C
C COL 15-24 (10I1) (ID(k,l),k=1,10) multiplicity of coefficients at
C                  l-th interface.  First index, k, refers to coefficient
C                  type:
C                                    PP           PP      PP           PP
C                         k = 1 for R  , = 2 for T   and T  , = 3 for R
C                                    D            D       U            U
C
C                                    VV           VV      VV           VV
C                         k = 4 for R  , = 5 for T   and T  , = 6 for R
C                                    D            D       U            U
C
C                                    PV      VP           PV      VP
C                         k = 7 for R   and R  , = 8 for T   and T
C                                    D       D            D       U
C
C                                    PV      VP            PV      VP
C                         k = 9 for T   and T  , = 10 for R   and R
C                                    U       D             U       U
C
C                  See figures 2 and 3 - ID(k,l) is nu_ÌlkË.
C
C COLS 25-38, 39-52, 53-66, 67-80 repeat 11-24 with increasing
C values of l, the layer/interface index.
C
C Note identical layers can be omitted after the first one - the
C final layer will always be different as NP(NN)=NS(NN)=0 and
C all the coefficients are reflections.
C
C If the I1 FORMAT is insufficient a layer/interface can be
C repeated as necessary.  All numbers are added.
C
C Records can be continued as required (COLS 1-10 being identical
C and the layer/interface descriptions continuing in
C COLS 11-24, 25-38, 39-52, 53-66 and 67-80).
C Records are read until the final layer is encountered.  Therefore the
C final layer should not be repeated on a continuation record - this can
C be avoided by repeating another layer (with zero entries).
C
C Example:
C
C Consider the ray SKPP4KP in a model with 5 layers - the crust,
C a 3-layer mantle, and core.  This ray has a multiplicity of 4 -
C the dynamic analogues are S2KPP3KP, S3KPP2KP and S4KPPKP.
C We assume a receiver at the free surface.  The record would be
C (with column 1 T rather than C):
C
C  6 4 2 9 1310010000000 2310300100000 5800330000010 5200000000000 6005000000000
C
C This is kinematically equivalent to SCPP5KP and S5KPPCP which
C would be specified by (with column 1 F rather than C):
C
C  6 1 2 9 1310010000000 2310300100000 5800240001000 5200000000000 6005000000000
C  6 1 2 9 1310010000000 2310300100000 5801140000010 5200000000000 6005000000000
C
C***********************************************************************
C
      K=0
C
C Read record
C
1     READ (IN,1001) LRAY,NN,MULT,IS,IR,(N(I),(NPX(J,I),J=1,2),
     1 (IDX(J,I),J=1,10),I=1,5)
      IF (NN.EQ.0) RETURN
C
C Loop over 5 fields on record
C
      DO 2 I=1,5
          NI=N(I)
          IF (NI.EQ.0) goto 12
          IF (NI.EQ.K) GO TO 4
          NK=NI-1
C
C Test whether identical layers omitted. K is previous layer.
C
          IF (NK.EQ.K) GO TO 5
          KK=K+1
C
C Include identical layer(s).
C
          DO 6 L=KK,NK
              NPS(1,L)=NPS(1,K)
              NPS(2,L)=NPS(2,K)
          DO 6 J=1,10
              ID(J,L)=ID(J,K)
6         CONTINUE
C
C New layer.
C
5         K=NI
          NPS(1,K)=NPX(1,I)
          NPS(2,K)=NPX(2,I)
          DO 7 J=1,10
              ID(J,K)=IDX(J,I)
7         CONTINUE
C
      GO TO 2
C
C Sum parameters in repeat layer.
C
4          NPS(1,K)=NPS(1,K)+NPX(1,I)
           NPS(2,K)=NPS(2,K)+NPX(2,I)
           DO 8 J=1,10
               ID(J,K)=ID(J,K)+IDX(J,I)
8          CONTINUE
C
C End of loop for field on record
C
2     CONTINUE
C
      IF (K.NE.NN) GO TO 1
c
c Test if SH waves exist for this ray
c
 12   continue
      lsh0=.true.
      do i=1,nn
        if(nps(1,i).ne.0)   lsh0=.false.
      enddo
c
      RETURN
C
1001  FORMAT (L1,I3,3I2,5(I2,12I1))
C
      END                                                               RAYIN
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C WKBJ.FOR - subroutines THETAC and CCSQRT
C
      SUBROUTINE THETAC (P,TAU,C,J,ICON,X,T0,DT,M,W)                    THETAC
C
C-----------------------------------------------------------------------
C Routine to compute WKBJ seismogram, i.e.
C w(m*DT+T0) = w(m*DT+T0) + sum( int PA to PB (C(p) dp))
C where the range of the integral (PA,PB) is defined by
C theta = (m-1)*DT+T0 to m*DT+T0 and the sum is over all such intervals
C with theta(p,X) = P(j)*X + TAU(j), see equation (16c).
C-----------------------------------------------------------------------
C
C Input: P(ICON,*)  = horizontal slownesses
C                     P(1,j) = pj (j=1 to J) in monotonically increasing
C                     order.
C        TAU(ICON,*)= delay times
C                     TAU(1,j) = tau(pj) (necessarily in decreasing
C                     order).
C        C(ICON,*)  = (C(1,j),C(2,j)) = Cj (complex integrand)
C        J          = number of tabulated points
C        ICON       = multiple array dimension to allow P, TAU and C to
C                     be stored in one multidimensional array in calling
C                     program.
C        X          = x (range)
C        T0         = t0 (initial time)
C        DT         = time step
C        M          = number of time points in final time series.
C                     Note output of this routine has (M+5) points -
C                     see CCSQRT.
C
C Input and output: W(*) = W(m+3) = w(m*DT+T0,X) (complex seismogram)
C                          Note m=-2 to M+2 to allow 2 point shift and
C                          1 point decrement in CCSQRT.
C                          Note result is added into W and W is not
C                          zeroed by this routine.  This allows many
C                          rays to be accumulated in the same buffer.
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation and figure numbers refer to that paper.
C
C Calls: none
C
C Errors: None.  It is assumed that the input parameters are all defined
C         sensibly and are compatible.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: C.H.Chapman
C
C Date: revised version, 28 July 1986
C
C Arguments:
C
      INTEGER J,ICON,M
      REAL P(ICON,*),TAU(ICON,*),C(ICON,*),X,T0,DT
      COMPLEX W(*)
C
C Local variables:
C
      COMPLEX PAMP,QAMP,BOT,TOP,Z,DIF
      REAL PJ,PB,P1,P2,P3,T,T1,T2,T3,TDIR,DP2,DP3,DT1,DT2,DT3,
     1 DTDP1,DTDP2,DTDP3,DQ
      INTEGER M5,JJ,J1,J2,J3,MM,MDIR,M0
C
C Total number of points in seismogram W.
C
C We have 2 extra points at each end to allow for 2 point shift in
C convolution operator in CCSQRT - equation (23).  The 3rd extra
C point at the top allows for decrement of the series - equation (19) -
C as this routine only computes the lower half of the boxcar -
C equation (16c).
C
      M5=M+5
C
C Find initial point
C
C T1+DT to T3 is window of seismogram (just in this loop).
C Note T1=T0-3*DT as T band is from T-DT to T.
C T1 < theta = P(1,JJ)*X+TAU(1,JJ) < T3 at end of loop - statement 10.
C
      T1=T0-3.*DT
      T3=T0+FLOAT(M+2)*DT
      DO 1 JJ=1,J
          T2=P(1,JJ)*X+TAU(1,JJ)
          IF (T2.GT.T1.AND.T2.LT.T3) GO TO 10
1     CONTINUE
C
C Table completely outside seismogram window - exit.
C
      RETURN
C
C Initialize values - see comments at statement 21 where we jump into
C main part of code for meaning of variables.  Effectively at this
C point before we jump into main code for a new interval, the current
C interval is P1=P(JJ-2) to P2=P(JJ-1) which is entirely outside
C seismogram window, and PB is P2 which will become PA (note we refer
C to PA in the interval PA to PB in the comments but it is not used
C explicitly as a variable).
C
10    J2=MAX0(1,JJ-1)
      J3=J2+1
      J1=MAX0(J2-1,1)
C
C PJ is end of table
C
      PJ=P(1,J)
      PB=P(1,J2)
      P2=PB
      P3=P(1,J3)
      P1=P(1,J1)
C
      T1=P1*X+TAU(1,J1)
      T2=P2*X+TAU(1,J2)
      T3=P3*X+TAU(1,J3)
C
C Index for first time point from table
C
      MM=INT((T2-T0)/DT)+3
      T=T0+FLOAT(MM-2)*DT
C
C Search direction indicators.
C
C These are set assuming current interval has a positive gradient.
C They will be reset at statement 23 - if the new interval, i.e. the
C first significant interval P2 to P3, has a negative gradient DTDP3.
C
      TDIR=DT
      MDIR=1
C
C Initializes integrals to zero.
C
C PAMP will be integral to P1 - figure (7b).
C QAMP will be integral to PB - figure (7a).
C
      PAMP=CMPLX(0.,0.)
      QAMP=CMPLX(0.,0.)
C
C Next interval, i.e. first significant interval
C
      DP3=P3-P2
      DT3=T3-T2
      DTDP3=DT3/DP3
C
C Current interval - if it is before table, i.e. JJ=J1=J2=1, then use
C same values as next interval.
C
      DT2=DT3
      DTDP2=DTDP3
      IF (J1.EQ.J2) GO TO 21
      DT2=T2-T1
      DTDP2=DT2/(P2-P1)
      GO TO 21
C
C End of introductory code.
C
C***********************************************************************
C
C The following code is the main part of the algorithm.
C
C The tightest loop is from statement 25 to the end of the code for new
C DT bands within the same p-interval, jumping out to statement 20
C whenever the DT band moves into a new p interval.
C
C***********************************************************************
C
C New p-interval section.
C
C Add in integral for interval P1 to P2 minus P1 to PA if PA within
C interval (QAMP will be zero from previous pass if PA outside interval)
C (note DP2=2*(P2-P1)) - figure (7b).
C
20    PAMP=.25*DP2*(BOT+TOP)-QAMP+PAMP
      QAMP=CMPLX(0.,0.)
C
C Get a new point from the P-TAU table
C
C Update interval from table - see figure (6).
C
C Previous interval: J1 end index
C                    P1 end p value
C                    T1 end theta value
C                    DT1 theta increment
C                    DTDP1 gradient
C
C Current interval: J1 and J2 indices
C                   P1 and P2 p values
C                   T1 and T2 theta values
C                   DT2 theta increment
C                   DP2 2 times p increment
C                   DTDP2 gradient
C
C Next interval: J2 and J3 indices
C                P2 and P3 p values
C                T2 and T3 theta values
C                DT3 theta increment
C                DP3 p increment
C                DTDP3 gradient
C
21    IF (J2.EQ.J) RETURN
      J1=J2
      J2=J3
      J3=MIN0(J3+1,J)
C
      P1=P2
      P2=P3
      P3=P(1,J3)
C
      T1=T2
      T2=T3
      T3=P3*X+TAU(1,J3)
C
      DT1=DT2
      DT2=DT3
      DT3=T3-T2
C
      DTDP1=DTDP2
      DTDP2=DTDP3
      DP2=DP3+DP3
      DP3=P3-P2
      IF (J2.NE.J) DTDP3=DT3/DP3
C
C Integrand at ends of current interval
C
      BOT=CMPLX(C(1,J1),C(2,J1))
      TOP=CMPLX(C(1,J2),C(2,J2))
C
C Modify integrand to make result continuous - equation (18)
C
      IF (ABS(DT2).LE.DT) GO TO 22
      IF (ABS(DT1).GT.DT) BOT=2.*ABS(DTDP2)/(ABS(DTDP1)+ABS(DTDP2))*BOT
      IF (ABS(DT3).GT.DT) TOP=2.*ABS(DTDP2)/(ABS(DTDP2)+ABS(DTDP3))*TOP
22    DIF=TOP-BOT
C
C Test whether direction in new current interval is still the same
C
      IF (DTDP2*TDIR) 23,20,24
C
C Gradient (DTDP2) and direction (TDIR) opposite sign so gradient has
C changed direction and we must change search direction.
C
23    MM=MM+MDIR
      TDIR=-TDIR
      MDIR=-MDIR
C
C End of new P-interval section.
C
C***********************************************************************
C
C New DT band section.
C
C Test old PB to exit if end
C
25    IF (PB.EQ.PJ) RETURN
C
C Increment T for new DT band to define new PB - figure (5b).
C
      T=T+TDIR
C
C Test whether new T is with current interval T1 to T2
C
C Note if the current interval is the last interval of the table (J=J2),
C then we end by putting PB=P2=PJ.
C
24    IF (((T-T2)*TDIR.GT.0.).AND.(J2.LT.J)) GO TO 20
C
C Solve for PB by linear interpolation in (P1,P2) interval between
C T1 and T2, i.e. theta(p,x) = T1 + DTDP2*(p-P1) - figure (6).
C
      PB=AMIN1(P1+(T-T1)/DTDP2,PJ)
C
C PAMP is integral from PA to P1 if P1>PA - statement 20 and
C         figure (7b),
C      is zero if P1<PA from previous pass here.
C QAMP is integral from P1 to PA if P1<PA from previous pass here when
C         PA was PB - figure (7a),
C      is zero if P1>PA - statement 20 and figure (7b).
C Z is integral from PA to P1 in either circumstance.
C
      Z=PAMP-QAMP
      PAMP=CMPLX(0.,0.)
C
C New QAMP is integral from P1 to PB by trapezoidal rule assuming
C linear interpolation of integrand between BOT and TOP at P1 and P2.
C
      DQ=PB-P1
      QAMP=DQ*(DQ/DP2*DIF+BOT)
C
C Increment time point index and find maximum of old and new index
C for top of DT band.
C
      M0=MM
      MM=MM+MDIR
      M0=MAX0(MM,M0)
C
C Sum integral from PA to PB into time point corresponding to top of
C interval - equation (16c).  Z is integral from PA to P1 and QAMP is
C integral from P1 to PB.
C
      IF (M0.GE.1.AND.M0.LE.M5) W(M0)=W(M0)+Z+QAMP
C
C End of DT band section.
C
C***********************************************************************
C
C Continue with next DT band.
C
      GO TO 25
C
      END                                                               THETAC





      SUBROUTINE CCSQRT (W,M,U)                                         CCSQRT
C
C-----------------------------------------------------------------------
C Routine to compute d/dt Im (1/sqrt(t) * w(t)) using a rational approx.
C The operator d/dt H(t)/sqrt(t) is smoothed by a boxcar 2*dt long, 3
C times.  The rational approximation for d/dt H(t)/sqrt(t) is given in
C the z-domain by
C
C          1    1+n1*z+n2*z**2+n3*z**3+n4*z**4+n5*z**5
C       ------  --------------------------------------
C       6*z**2    1+d1*z+d2*z**2+d3*z**3+d4*z**4
C
C e.g. equation (23).  The rational approximation is then applied as
C
C Y(i) = X(i)+N1*X(i-1)+N2*X(i-2)+N3*X(i-3)+N4*X(i-4)+N5*X(i-5)
C            -D1*Y(i-1)-D2*Y(i-2)-D3*Y(i-3)-D4*Y(i-4)
C
C where X(i) is the input series and Y(i) the output series.
C The operator is applied in the forward direction on the imaginary
C part, and backwards on the real part, see equation (21).
C The factor 1/6 is omitted and 1/z**2 shifts 2 points (in direction
C opposite to operator).
C The input data are decremented and added, i.e. v(m)=w(m)+w(m+1),
C equation (19) with 1/2*DT omitted, to compensate for the half-boxcar
C from t-dt to t used in THETAC.
C The input data are M+5 points from TSTRT-2*DT to TSTRT+(M+2)*DT, i.e.
C output of THETAC.  The 2 extra points allow the operator to run to end
C point of output time series with 2 point shift, z**2.  The 3rd extra
C point at the top is for the decrementing the input series.
C-----------------------------------------------------------------------
C
C Input: W(2,*)     = complex time series
C                     (W(1,m+3),W(2,m+3)) = w(m*dt+t0,x) (m=-2 to M+2)
C        M          = number of time points in output series
C
C Output: U(*)      = real time series - u(t)=d/dt Im (1/sqrt(t)*w(t))
C                     U(m) = u(m*dt-dt+t0,x) (m=1 to M)
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation numbers refer to that paper.
C
C Calls: none
C
C Errors: Note it is assumed that the input parameters are all defined
C         sensibly and are compatible.  It is assumed that M>2.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: C.H.Chapman
C
C Date: revised version, 25 July 1986
C
C
C Arguments:
C
      REAL W(2,*),U(*)
      INTEGER M
C
C Local variables:
C
      INTEGER M4,K,L,J,N,K1
      REAL X0,X1,X2,X3,X4,X5,Y0,Y1,Y2,Y3,Y4,N1,N2,N3,N4,N5,D1,D2,D3,D4
      DATA N1,N2,N3,N4,N5/1.265298,-1.508272,-2.047727,
     1 0.5205257,0.8033268/
      DATA D1,D2,D3,D4/-1.563129,0.7167736,-0.1569175,0.3977094E-1/
C
      M4=M+4
C
C Indices for doing operation on real part backwards,
C i.e. d/dt H(-t)/sqrt(-t).
C K counts through input series using K1 as increment.
C
      K=M+1
      K1=-1
C
C Test for non-zero real element and zero output series
C
      DO 1 N=5,M4
          K=K-1
          IF (W(1,K).NE.0.) GO TO 10
1     U(K)=0.
      N=M4
C
C Loop for backward (J=1) and forward (J=2) operations
C
10    DO 20 J=1,2
C
C Initialize X and Y's - see loop 30.
C
C Normally these will be zero unless series start immediately with
C non-zero values and loop 1 or 21 is left early.
C X4=0 as no attempt is made to extrapolate known series.
C Note decrement of input data.
C
          X0=W(J,K-K1)+W(J,K-K1+1)
          X1=W(J,K-2*K1)+W(J,K-2*K1+1)
          X2=W(J,K-3*K1)+W(J,K-3*K1+1)
          X3=W(J,K-4*K1)+W(J,K-4*K1+1)
          X4=0.
C
          Y3=X3
          Y2=X2+N1*X3-D1*Y3
          Y1=X1+N1*X2+N2*X3-D1*Y2-D2*Y3
          Y0=X0+N1*X1+N2*X2+N3*X3-D1*Y1-D2*Y2-D3*Y3
C
C Apply rational approximation, see equation (23).
C
C Note scalars Xn and Yn are used within loop for the input and output
C series to reduce index operations.
C
          DO 30 L=N,M4
C
C Update X and Y's.
C
              X5=X4
              X4=X3
              X3=X2
              X2=X1
              X1=X0
C
C Input series decremented and added, see equation (19).
C
              X0=W(J,K)+W(J,K+1)
C
              Y4=Y3
              Y3=Y2
              Y2=Y1
              Y1=Y0
C
              Y0=X0+N1*X1+N2*X2+N3*X3+N4*X4+N5*X5
     1             -D1*Y1-D2*Y2-D3*Y3-D4*Y4
C
C Branch for backward and forward operations.
C
              GO TO (31,32),J
C
C Backward operation - store result.
C
C Note 2 point shift and 2 extra points in input series cancel.
C d/dt is not backwards so negate result.
C
31            U(K)=-Y0
              GO TO 30
C
C Forward operation - add to previous result,
C
C Note 2 point shift and 2 extra points combine to give 4 point shift.
C
32            U(K-4)=U(K-4)+Y0
C
C End of loop running through time series.
C
30        K=K+K1
C
C If doing forward operation then this is the end.
C
          IF (J.EQ.2) RETURN
C
C Set parameters for forward operation and find starting point of
C imaginary part.
C
          K1=1
          DO 21 K=6,M4
              IF (W(2,K).NE.0.) GO TO 22
21        CONTINUE
          K=M4
C
C Reduce K as we decrement input series.
C
22        K=K-1
          N=K
C
C End of loop for backward operation.
C
20    CONTINUE
C
C No route through here.
C
      STOP
C
      END                                                               CCSQRT
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C RAY1D.FOR - subroutines PXINT, COEFF and COMPON
C
      SUBROUTINE PXINT (P,VEL,IVEL,IL,NPS,L,IMTH,TAU,X,IRTR)            PXINT
C
C-----------------------------------------------------------------------
C Routine to compute TAU(p) and X(p), see equation (15).
C-----------------------------------------------------------------------
C
C Input: P           = horizontal slowness
C        VEL(IVEL,*) = model
C                      VEL(1,*) = depths in increasing order
C                      VEL(2,*) = P slownesses
C                      VEL(3,*) = S slownesses
C                                 For a fluid, this is irrelevant and
C                                 should be set zero for routine COEFF
C                      VEL(4,*) = densities (irrelevant)
C                      VEL(5,*) = b for interpolation of P velocity
C                      VEL(6,*) = b for interpolation of S velocity
C                                 (see equations (13) and (14))
C        IVEL        = multiple array dimension for VEL ( >= 6)
C        IL(*)       = interface indices i(l),
C                      i.e. VEL(1,IL(LL)-1) = VEL(1,IL(LL)) with IL(1)=1
C        NPS(2,*)    = numbers of ray segments
C                      NPS(1,*) = number of P ray segments
C                      NPS(2,*) = number of SV ray segments
C        L           = maximum layer with ray segments
C                      This can be larger, i.e. total number of layers,
C                      provided appropriate NPS(*,*) are zero.
C        IMTH        = interpolation method
C                      IMTH = 1,  v(z) = 1/sqrt(a - 2*b*z)
C                           = 2,  v(z) = a - b*z
C                           = 3,  v(z) = a*exp(-b*z)
C                                 (see equations (13) and (14))
C
C Output: TAU        = delay function tau(p)
C         X          = range function X(p)
C         IRTR       = return code
C                      IRTR = 1, normal return
C                           = 2, error return - TAU would be complex
C                                for ray definition
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation numbers refer to that paper.
C
C Calls: none
C
C Errors: see IRTR.
C         Note it is assumed that the input parameters are all defined
C         sensibly and are compatible.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: C.H.Chapman
C
C Date: revised version, 23 July 1986
C
C Arguments:
C
      INTEGER IVEL,IL(*),L,NPS(2,*),IMTH,IRTR
      REAL P,VEL(IVEL,*),TAU,X
C
C Local variables:
C
      REAL DTAU,DX,U,Y,Q,QS,QR,B,ETAU,EX,Z
      INTEGER LL,I1,I2,K,MULT,KU,KV,I,II
C
C Zero integrals
C
      TAU=0.
      X=0.
C
C Loop over layers - LL is l in equation (8)
C
      DO 100 LL=1,L
C
C Range of model indices in layer - IL(l) = i(l) in definition II.
C Layer is model points I1 to I2+1.
C
          I1=IL(LL)
          I2=IL(LL+1)-2
C
C Loop over ray types - K corresponds to alpha and beta in equation (8)
C
          DO 100 K=1,2
C
C Multiplicity of ray segments
C
              MULT=NPS(K,LL)
              IF (MULT.EQ.0) GO TO 100
C
C Zero contribution from layer.
C Note layer contributions to integral accumulated separately (DTAU and
C DX) so multiplicity can be used once at end of loop.
C
              DTAU=0.
              DX=0.
C
C KU = slowness index, KV = b index
C
              KU=K+1
              KV=K+4
C
C Initialize variables Q, QS and QR at top of layer
C
C Slowness and test whether vertical slowness real
C
              U=VEL(KU,I1)
              Y=U-P
              IF (Y) 900,100,110
C
C Vertical slowness
C
110           Q=Y*(U+P)
              QS=SQRT(Q)
C
C Special function needed for integral at top of layer.
C See loop 130 for details.
C
              GO TO (120,112,113),IMTH
C
112           Y=U+QS
              IF (P.NE.0.) Y=Y/P
              QR=ALOG(Y)
              GO TO 120
C
113           QR=ATAN2(QS,P)
C
C Loop over model points in layer
C
120           CONTINUE
              DO 130 I=I1,I2
                  II=I+1
C
C Interpolation parameter - b=0 is a constant velocity layer
C
                  B=VEL(KV,I)
                  IF (B.EQ.0.) GO TO 150
C
C Integral at upper limit - model point I.
C
C Factor 1/b omitted until end of loop (at 160).
C
C Note contributions between model points I and II evaluated separately
C (ETAU and EX) to reduce rounding errors (if ETAU is small compared
C with DTAU, etc.).
C
                  GO TO (131,132,133),IMTH
C
C IMTH=1, v(z) = 1/sqrt(a-2*b*z) - equation (13a)
C         tau(p) = -q**3/3/b - equation (15a)
C         x(p) = -p*q/b
C
131               ETAU=-Q*QS/3.
                  EX=-QS*P
                  GO TO 140
C
C IMTH=2, v(z) = a-b*z - equation (13b)
C         tau(p) = (log((1+v*q)/v/p)-v*q)/b - equation (15b)
C         x(p) = v*q/p
C
132               EX=QS/U
                  ETAU=QR-EX
                  IF (P.NE.0.) EX=EX/P
                  GO TO 140
C
C IMTH=3, v(z) = a*exp(-b*z) - equation (13c)
C         tau(p) = (q-p*atan(q/p))/b - equation (15c)
C         x(p) = atan(q/p)/b
C
133               ETAU=QS-P*QR
                  EX=QR
C
C Variables at lower limit - model point II
C
C Slowness and test whether vertical slowness real.  If vertical
C slowness is not real we have a turning point.  Integral at lower
C limit is then zero.
C
140               U=VEL(KU,II)
                  IF (U.LE.P) GO TO 160
                  Q=(U-P)*(U+P)
                  QS=SQRT(Q)
C
C Integral at lower limit
C
                  GO TO (141,142,143),IMTH
C
141               ETAU=ETAU+Q*QS/3.
                  EX=EX+QS*P
                  GO TO 160
C
142               Y=U+QS
                  Z=QS/U
                  ETAU=ETAU+Z
                  IF (P.EQ.0.) GO TO 144
                  Y=Y/P
                  Z=Z/P
144               QR=ALOG(Y)
                  ETAU=ETAU-QR
                  EX=EX-Z
                  GO TO 160
C
143               QR=ATAN2(QS,P)
                  ETAU=ETAU-QS+P*QR
                  EX=EX-QR
                  GO TO 160
C
C Special results if b=0 - homogeneous, 1/B = thickness
C
150               B=1./(VEL(1,II)-VEL(1,I))
                  ETAU=QS
                  EX=P/QS
C
C Contribution to layer sums from model points I to II
C
160               DTAU=DTAU+ETAU/B
                  DX=DX+EX/B
C
C Turning point condition
C
                  IF (U.LE.P) GO TO 170
C
C End of loop through model points in layer
C
130           CONTINUE
C
C Contribution from layer to total integral
C
170           TAU=TAU+FLOAT(MULT)*DTAU
              X=X+FLOAT(MULT)*DX
C
C End of loops though ray types and layers
C
100   CONTINUE
C
C Normal return condition
C
      IRTR=1
      RETURN
C
C Error return condition - tau(p) complex
C
900   IRTR=2
      RETURN
C
      END                                                               PXINT




      COMPLEX FUNCTION COEFF (P,VEL1,VEL2,ID)                           COEFF
C
C-----------------------------------------------------------------------
C Routine to compute product of reflection/transmission coefficients at
C interface, equation (7).
C-----------------------------------------------------------------------
C
C Input: P          = horizontal slowness
C        VEL1(*)    = parameters above interface
C                     VEL1(1) = P slowness
C                     VEL1(2) = S slowness (zero for a fluid)
C                     VEL1(3) = density (zero for a free surface)
C        VEL2(*)    = parameters below interface
C                     VEL2(1) = P slowness
C                     VEL2(2) = S slowness (zero for a fluid)
C                     VEL2(3) = density
C        ID(*)      = multiplicity of coefficients. ID(k) is nu_ÌlkË
C                     where
C                                PP           PP      PP           PP
C                     k = 1 for R  , = 2 for T   and T  , = 3 for R
C                                D            D       U            U
C
C                                VV           VV      VV           VV
C                     k = 4 for R  , = 5 for T   and T  , = 6 for R
C                                D            D       U            U
C
C                                PV      VP           PV      VP
C                     k = 7 for R   and R  , = 8 for T   and T
C                                D       D            D       U
C
C                                PV      VP            PV      VP
C                     k = 9 for T   and T  , = 10 for R   and R
C                                U       D             U       U
C
C                     See figures 2 and 3.
C
C Output: COEFF     = product of reflection/transmission coefficients,
C                     e.g. inner product in equation (6) with
C                     equation (7).
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation numbers refer to that paper.
C
C Calls: none
C
C Errors: Note it is assumed that the input parameters are all defined
C         sensibly and are compatible.  If for P1P1 or S1S1, the
C         incident ray is evanescent, a turning ray is assumed.  For
C         other coefficients, if an incident or generated ray is
C         evanescent, then it probably indicates an error in the input
C         parameters.  The coefficient may be correct or may be zero
C         as the slownesses of the incident and generated rays are
C         assumed real in some places.
C         Non-existent coefficients in fluid media are not set zero -
C         in fact they implicitly are unity.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: C.H.Chapman
C
C Date: revised version, 24 July 1986
C
C Arguments:
C
      REAL P,VEL1(*),VEL2(*)
      INTEGER ID(*)
C
C Local variables:
C
C The code has been written to reduce the amount of unnecessary complex
C arithmetic.  This may not be necessary with smart FORTRAN compilers.
C DUM is used to simplify zeroing many variables.
C
      REAL DUM(22),P1,P2,S1,S2,D1,D2,MU1,MU2,PP,PP4,QA1R,QA1I,QA2R,QA2I,
     1 QB1R,QB1I,QB2R,QB2I,D2QA1R,D2QA1I,D1QA2R,D1QA2I,D2QB1R,
     2 D2QB1I,D1QB2R,D1QB2I,QA1B1R,QA1B1I,QA2B2R,QA2B2I,B1,
     3 D,E1,E2,B1P2,X2,X1,X,Y
      COMPLEX DELTA,AAP,AAM,ABP,ABM,C1P,C1M,C2P,C2M
      INTEGER K
C
      EQUIVALENCE (DUM(1),QA1R),(DUM(2),QA1I),(DUM(3),QA2R),
     1 (DUM(4),QA2I),(DUM(5),QB1R),(DUM(6),QB1I),(DUM(7),QB2R),
     2 (DUM(8),QB2I),(DUM(9),D2QA1R),(DUM(10),D2QA1I),(DUM(11),D1QA2R),
     3 (DUM(12),D1QA2I),(DUM(13),D2QB1R),(DUM(14),D2QB1I),
     4 (DUM(15),D1QB2R),(DUM(16),D1QB2I),(DUM(17),QA1B1R),
     5 (DUM(18),QA1B1I),(DUM(19),QA2B2R),(DUM(20),QA2B2I),
     6 (DUM(21),MU1),(DUM(22),MU2)
C
C Initialize COEFF=1
C
      COEFF=CMPLX(1.,0.)
C
C Test for non-zero coefficients
C
      DO 1 K=1,10
          IF (ID(K).NE.0) GO TO 2
1     CONTINUE
C
C Return if no coefficients - note COEFF=1
C
      RETURN
C
C Initialize variables
C
2     PP=P*P
      PP4=4.*PP
      DO 3 K=1,22
          DUM(K)=0.
3     CONTINUE
      P1=VEL1(1)
      S1=VEL1(2)
      D1=VEL1(3)
      P2=VEL2(1)
      S2=VEL2(2)
      D2=VEL2(3)
C
C Set up real or imaginary parts of vertical slownesses.
C Note Reimann sheet is defined by Im(q) >= 0.
C
C qa2
C
      X2=(P2-P)*(P2+P)
      IF (X2) 11,20,12
11    QA2I=SQRT(-X2)
      D1QA2I=D1*QA2I
      GO TO 20
12    QA2R=SQRT(X2)
      D1QA2R=D1*QA2R
C
C qa1
C
20    IF (D1.EQ.0.) GO TO 500
      X1=(P1-P)*(P1+P)
      IF (X1) 21,30,22
21    QA1I=SQRT(-X1)
      D2QA1I=D2*QA1I
      GO TO 30
22    QA1R=SQRT(X1)
      D2QA1R=D2*QA1R
C
C Aa+ and Aa- - equation (7l)
C
30    AAP=CMPLX(D2QA1R+D1QA2R,D2QA1I+D1QA2I)
      AAM=CMPLX(D2QA1R-D1QA2R,D2QA1I-D1QA2I)
C
C Test for fluids
C
      IF (S1.EQ.0..AND.S2.EQ.0.) GO TO 400
      IF (S1.EQ.0.) GO TO 40
C
C qb1 and qa1*qb1
C
      MU1=D1/S1**2
      X=(S1-P)*(S1+P)
      IF (X) 31,40,32
31    QB1I=SQRT(-X)
      D2QB1I=D2*QB1I
      IF (X1) 33,40,34
33    QA1B1R=-QA1I*QB1I
      GO TO 40
34    QA1B1I=QA1R*QB1I
      GO TO 40
32    QB1R=SQRT(X)
      D2QB1R=D2*QB1R
      IF (X1) 35,40,36
35    QA1B1I=QB1R*QA1I
      GO TO 40
36    QA1B1R=QB1R*QA1R
C
C B1 - equation (7m)
C C1+ - equation (7n) (C1P = 2*p*C1+)
C C1- - equation (7n) (C1M = -2*p*C1-)
C
40    IF (S2.NE.0.) MU2=D2/S2**2
      B1=MU1-MU2
      X=PP4*B1*QA1B1I
      C1P=CMPLX(PP4*(B1*(QA1B1R+PP)-D1),X)
      C1M=CMPLX(PP4*(B1*(QA1B1R-PP)+D1),X)
C
C qb2 and qa2*qb2
C
      IF (S2.EQ.0.) GO TO 300
      X=(S2-P)*(S2+P)
      IF (X) 41,50,42
41    QB2I=SQRT(-X)
      D1QB2I=D1*QB2I
      IF (X2) 43,50,44
43    QA2B2R=-QA2I*QB2I
      GO TO 50
44    QA2B2I=QA2R*QB2I
      GO TO 50
42    QB2R=SQRT(X)
      D1QB2R=D1*QB2R
      IF (X2) 45,50,46
45    QA2B2I=QB2R*QA2I
      GO TO 50
46    QA2B2R=QB2R*QA2R
C
C C2+ - equation (7n) (C2P = -C2+/2/p)
C C2- - equation (7n) (C2M = C2-/2/p)
C D - equation (7o)
C E1 and E2 - equation (7p)
C Ab+ and Ab- - equation (7l)
C
50    X=B1*QA2B2I
      C2P=CMPLX(B1*(QA2B2R+PP)+D2,X)
      C2M=CMPLX(B1*(QA2B2R-PP)-D2,X)
      IF (S1.EQ.0.) GO TO 200
      D=PP*(D1+D2)**2
      B1P2=2.*PP*B1
      E1=D1-B1P2
      E2=D2+B1P2
      ABP=CMPLX(D2QB1R+D1QB2R,D2QB1I+D1QB2I)
      ABM=CMPLX(D2QB1R-D1QB2R,D2QB1I-D1QB2I)
C
C DELTA - equation (7k)
C
      DELTA=AAP*ABP+C1P*C2P+CMPLX(D,0.)
C
C P-SV reflection/transmission coefficients at solid-solid interface
C
100   CONTINUE
C
C P1P1 - equation (7a)
C
      K=ID(1)
      IF (K.EQ.0) GO TO 101
      IF (P.GE.P1) GO TO 110
      COEFF=COEFF*((AAM*ABP+C1M*C2P-CMPLX(D,0.))/DELTA)**K
C
C P1P2 and P2P1 - equation (7e)
C
101   K=ID(2)
      IF (K.EQ.0) GO TO 102
C
C Fa1*Fa2 - equation (7q)
C
      X=2.*SQRT(D2QA1R*D1QA2R)
      COEFF=COEFF*(CMPLX(X*(E2*QB1R+E1*QB2R),X*(E2*QB1I+E1*QB2I))/
     1 DELTA)**K
C
C P2P2 - equation (7c)
C
102   K=ID(3)
      IF (K.EQ.0) GO TO 103
      COEFF=COEFF*((C1P*C2M-AAM*ABP-CMPLX(D,0.))/DELTA)**K
C
C S1S1 - equation (7b)
C
103   K=ID(4)
      IF (K.EQ.0) GO TO 104
      IF (P.GE.S1) GO TO 111
      COEFF=COEFF*((CMPLX(D,0.)-AAP*ABM-C1M*C2P)/DELTA)**K
C
C S1S2 and S2S1 - equation (7f)
C
104   K=ID(5)
      IF (K.EQ.0) GO TO 105
C
C Fb1 * Fb2 - equation (7q)
C
      X=2.*SQRT(D2QB1R*D1QB2R)
      COEFF=COEFF*(CMPLX(X*(E2*QA1R+E1*QA2R),X*(E2*QA1I+E1*QA2I))/
     1 DELTA)**K
C
C S2S2 - equation (7d)
C
105   K=ID(6)
      IF (K.EQ.0) GO TO 106
      COEFF=COEFF*((AAP*ABM-C1P*C2M+CMPLX(D,0.))/DELTA)**K
C
C P1S1 and S1P1 - equation (7g)
C
106   K=ID(7)
      IF (K.EQ.0) GO TO 107
C
C -2*E1*B2 and p*Fa1*Fb1/rho1 - equations (7m,p,q)
C
      Y=2.*B1*E1
      X=SQRT(PP4*QA1B1R)
      COEFF=COEFF*(CMPLX(X*(Y*QA2B2R+(E1-D2)*E2),X*Y*QA2B2I)/
     1 DELTA)**K
C
C P1S2 and S2P1 - equation (7i)
C
107   K=ID(8)
      IF (K.EQ.0) GO TO 108
C
C -2*B2 and p*Fa1*Fb2 - equations (7m,q)
C
      Y=B1+B1
      X=SQRT(PP4*D2QA1R*D1QB2R)
      COEFF=COEFF*(CMPLX(X*(Y*(QB1R*QA2R-QB1I*QA2I)+E2-D1),
     1 X*Y*(QB1I*QA2R+QB1R*QA2I))/DELTA)**K
C
C P2S1 and S1P2 - equation (7j)
C
108   K=ID(9)
      IF (K.EQ.0) GO TO 109
C
C 2*B1 and -p*Fa2*Fb1 - equations (7m,q)
C
      Y=B1+B1
      X=-SQRT(PP4*D1QA2R*D2QB1R)
      COEFF=COEFF*(CMPLX(X*(Y*(QA1R*QB2R-QA1I*QB2I)+E2-D1),
     1 X*Y*(QA1R*QB2I+QA1I*QB2R))/DELTA)**K
C
C P2S2 and S2P2 - equation (7h)
C
109   K=ID(10)
      IF (K.EQ.0) GO TO 900
C
C 2*E2*B1 and -p*Fa2*Fb2/rho2 - equations (7m,p,q)
C
      Y=2.*E2*B1
      X=-SQRT(PP4*QA2B2R)
      COEFF=COEFF*(CMPLX(X*(Y*QA1B1R+(E1-D2)*E1),X*Y*QA1B1I)/DELTA)**K
      GO TO 900
C
C P1P1 turning point
C
110   COEFF=COEFF*CMPLX(0.,-1.)**K
      GO TO 101
C
C S1S1 turning point
C
111   COEFF=COEFF*CMPLX(0.,1.)**K
      GO TO 104
C
C P-SV reflection/transmission coefficients for fluid/solid
C
200   X1=4.*P/S2**2
      X=P*X1
      C1P=-CMPLX(X*QA1R,X*QA1I)
      DELTA=AAP+C1P*C2P
C
C P1P1
C
      K=ID(1)
      IF (K.EQ.0) GO TO 201
      IF (P.GE.P1) GO TO 210
      COEFF=COEFF*((DELTA-CMPLX(D1QA2R+D1QA2R,D1QA2I+D1QA2I))/DELTA)**K
C
C P1P2 and P2P1
C
201   K=ID(2)
      IF (K.EQ.0) GO TO 202
      COEFF=COEFF*(CMPLX((2.-X)*SQRT(D2QA1R*D1QA2R),0.)/DELTA)**K
C
C P2P2
C
202   K=ID(3)
      IF (K.EQ.0) GO TO 203
      COEFF=COEFF*((C1P*C2M-AAM)/DELTA)**K
C
C S2S2
C
203   K=ID(6)
      IF (K.EQ.0) GO TO 206
      COEFF=COEFF*((AAP-C1P*C2M)/DELTA)**K
C
C P1S2 and S2P1
C
206   K=ID(8)
      IF (K.EQ.0) GO TO 208
      Y=-X1*SQRT(D1*D2QA1R*QB2R)
      COEFF=COEFF*(CMPLX(Y*QA2R,Y*QA2I)/DELTA)**K
C
C P2S2 and S2P2
C
208   K=ID(10)
      IF (K.EQ.0) GO TO 900
      Y=X1*(D2-2.*PP*MU2)*SQRT(QA2B2R)
      COEFF=COEFF*(CMPLX(Y*QA1R,Y*QA1I)/DELTA)**K
      GO TO 900
C
C P1P1 turning point
C
210   COEFF=COEFF*CMPLX(0.,-1.)**K
      GO TO 201
C
C P-SV reflection/transmission coefficients for solid/fluid interface
C
300   X1=MU1/D1
      C2P=CMPLX(X1*QA2R,X1*QA2I)
      X=PP4*X1
      DELTA=AAP+C1P*C2P
C
C P1P1
C
      K=ID(1)
      IF (K.EQ.0) GO TO 301
      IF (P.GE.P1) GO TO 310
      COEFF=COEFF*((AAM+C2P*C1M)/DELTA)**K
C
C P1P2 and P2P1
C
301   K=ID(2)
      IF (K.EQ.0) GO TO 302
      COEFF=COEFF*(CMPLX((2.-X)*SQRT(D2QA1R*D1QA2R),0.)/DELTA)**K
C
C P2P2
C
302   K=ID(3)
      IF (K.EQ.0) GO TO 303
      COEFF=COEFF*((DELTA-CMPLX(D2QA1R+D2QA1R,D2QA1I+D2QA1I))/DELTA)**K
C
C S1S1
C
303   K=ID(4)
      IF (K.EQ.0) GO TO 304
      IF (P.GE.S1) GO TO 311
      COEFF=COEFF*((AAP-C2P*C1M)/DELTA)**K
C
C P1S1 and S1P1
C
304   K=ID(7)
      IF (K.EQ.0) GO TO 307
      Y=4.*P*X1*(D1-2.*PP*MU1)*SQRT(QA1B1R)
      COEFF=COEFF*(CMPLX(Y*QA2R,Y*QA2I)/DELTA)**K
C
C P2S1 and S1P2
C
307   K=ID(9)
      IF (K.EQ.0) GO TO 900
      Y=-4.*P*X1*SQRT(D2*D1QA2R*QB1R)
      COEFF=COEFF*(CMPLX(Y*QA1R,Y*QA1I)/DELTA)**K
      GO TO 900
C
C P1P1 turning point
C
310   COEFF=COEFF*CMPLX(0.,-1.)**K
      GO TO 301
C
C S1S1 turning point
C
311   COEFF=COEFF*CMPLX(0.,1.)**K
      GO TO 304
C
C P reflection/transmission coefficients for fluid/fluid interface
C
400   K=ID(1)
      IF (K.EQ.0) GO TO 401
      IF (P.GE.P1) GO TO 410
      COEFF=COEFF*(AAM/AAP)**K
C
C P1P2 and P2P1
C
401   K=ID(2)
      IF (K.EQ.0) GO TO 402
      COEFF=COEFF*(CMPLX(2.*SQRT(D2QA1R*D1QA2R),0.)/AAP)**K
C
C P2P2
C
402   K=ID(3)
      IF (K.EQ.0) GO TO 900
      COEFF=COEFF*(-AAM/AAP)**K
      GO TO 900
C
C P1P1 turning point
C
410   COEFF=COEFF*CMPLX(0.,-1.)**K
      GO TO 401
C
C P-SV reflection coefficients from a free solid surface
C
500   IF (S2.EQ.0.) GO TO 600
      X=(S2-P)*(S2+P)
      Y=X-PP
      X2=Y**2
      X=PP4*SQRT(X)
      QA2B2R=X*QA2R
      QA2B2I=X*QA2I
      DELTA=CMPLX(X2+QA2B2R,QA2B2I)
C
C P2S2 and S2P2
C
      K=ID(10)
      IF (K.EQ.0) GO TO 510
      COEFF=COEFF*(CMPLX(2.*Y*SQRT(QA2B2R),0.)/DELTA)**K
C
C -P2P2 and S2S2
C
510   K=ID(3)+ID(6)
      IF (K.EQ.0) GO TO 900
      COEFF=COEFF*(CMPLX(X2-QA2B2R,-QA2B2I)/DELTA)**K
C
C P2P2 from fluid free surface and correct P2P2 for solid surface
C
600   IF (MOD(ID(3),2).EQ.1) COEFF=-COEFF
C
900   RETURN
C
      END                                                               COEFF




      SUBROUTINE COMPON (P,VELS,ISS,VELR,IRR,COMP)                      COMPON
C
C-----------------------------------------------------------------------
C Routine to compute source and receiver component functions,
C equations (4) and (5) (see also equation (9)).
C-----------------------------------------------------------------------
C
C Input: P          = horizontal slowness
C        VELS(*)    = source parameters
C                     VELS(1) = P slowness at source
C                     VELS(2) = S slowness at source
C                                For a fluid, this is irrelevant and
C                                should be set zero for routine COEFF
C                     VELS(3) = density at source
C        ISS        = source type
C                     ISS = 1 for downgoing P source ray
C                         = 2 for downgoing SV source ray
C                         = 5 for upgoing P source ray
C                         = 6 for upgoing SV source ray
C        VELR(*)    = receiver parameters
C                     VELR(1) = P slowness at receiver
C                     VELR(2) = S slowness at receiver
C                                For a fluid, this is irrelevant and
C                                should be set zero for routine COEFF
C                     VELR(3) = density at receiver
C        IRR        = receiver type
C                     IRR = 1 for upgoing P receiver ray in medium
C                         = 2 for upgoing SV receiver ray in medium
C                         = 5 for downgoing P receiver ray in medium
C                         = 6 for downgoing SV receiver ray in medium
C                         = 9 for P receiver ray at free surface
C                         = 10 for SV receiver ray at free surface
C
C Output: COMP(*)   = components of M U and F U
C                     COMP(1) = Mzz Ux
C                     COMP(2) = Mxx Ux
C                     COMP(3) = (Mxz + Mzx) Ux
C                     COMP(4) = Mzz Uz
C                     COMP(5) = Mxx Uz
C                     COMP(6) = (Mxz + Mzx) Uz
C                     COMP(7) = Fz Ux
C                     COMP(8) = Fx Ux
C                     COMP(9) = Fz Uz
C                     COMP(10) = Fx Uz
C                     Note a factor of -sqrt(p/8)/pi**2 is included.
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation numbers refer to that paper.
C
C Calls: none
C
C Errors: Note it is assumed that the input parameters are all defined
C         sensibly and are compatible.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: C.H.Chapman
C
C Date: revised version, 24 July 1986
C
C Arguments:
C
      REAL P,VELS(*),VELR(*)
      INTEGER ISS,IRR
      COMPLEX COMP(*)
C
C Local variables:
C
      COMPLEX UX,UZ,QA,CC
      REAL QS(2),QR(2),C,FZ,FX,MZZ,MXX,MXZ,OMEGA
      INTEGER ISV,IRV,I
C
C Source and receiver types - 1 is P, 2 is SV
C
      ISV=MOD(ISS,4)
      IRV=MOD(IRR,4)
C
C Source and receiver vertical slownesses.
C
C Qalpha may be imaginary so include ABS.
C
      DO 100 I=1,2
          QS(I)=SQRT(ABS(VELS(I)-P)*(VELS(I)+P))
          QR(I)=SQRT(ABS(VELR(I)-P)*(VELR(I)+P))
100   CONTINUE
C
C Factor of -sqrt(p/8)/pi**2 * 1/sqrt(2*rho0*q0) * 1/sqrt(2*rho*q)
C from equations (9), (4) and (5).
C
      C=-1.791122E-2*SQRT(P/VELS(3)/VELR(3)/QS(ISV)/QR(IRV))
C
C Source components - equation (4)
C
      IF (ISV.EQ.2) GO TO 200
C
C P source.
C
C Body force source - equation (4b)
C
      FZ=QS(1)
      FX=P
C
C Moment tensor source - equation (4a)
C
      MZZ=FZ*FZ
      MXX=P*P
      MXZ=2.*FZ*P
      GO TO 210
C
C SV source.
C
C Body force source - equation (4d)
C
200   FZ=-P
      FX=QS(2)
C
C Moment tensor source - equation (4c)
C
      MZZ=-P*FX
      MXX=-MZZ
      MXZ=(FX-P)*(FX+P)
C
C Change source direction - equation (4)
C
210   IF (ISS.GE.5) GO TO 300
      FZ=-FZ
      MXZ=-MXZ
C
C Receiver components - equation (5)
C
300   IF (IRR.GE.9) GO TO 310
      IF (IRV.EQ.2) GO TO 320
C
C P receiver in medium - equation (5a)
C
      UX=CMPLX(C*P,0.)
      UZ=CMPLX(C*QR(1),0.)
      GO TO 330
C
C SV receiver in  medium - equation (5b)
C
320   UX=CMPLX(C*QR(2),0.)
      UZ=CMPLX(-C*P,0.)
C
C Change receiver ray direction - equation (5)
C
330   IF (IRR.GE.5) UZ=-UZ
      GO TO 400
C
C Receiver at free surface
C
C Test for fluid
C
310   IF (VELR(2).EQ.0.) GO TO 340
C
      OMEGA=(QR(2)-P)*(QR(2)+P)
      IF (IRR.EQ.10) GO TO 350
C
C P receiver at free surface - equation (5c)
C
      C=2.*C*VELR(2)**2*QR(1)/(4.*QR(1)*QR(2)*P**2+OMEGA**2)
      UX=CMPLX(2.*C*QR(2)*P,0.)
      UZ=CMPLX(C*OMEGA,0.)
      GO TO 400
C
C Fluid free surface
C
340   UX=CMPLX(0.,0.)
      UZ=CMPLX(2.*C*QR(1),0.)
      GO TO 400
C
C SV receiver at free surface - equation (5d)
C
C Note Qalpha can be imaginary
C
350   QA=CMPLX(QR(1),0.)
      IF (VELR(1).LT.P) QA=CMPLX(0.,QR(1))
      CC=2.*C*VELR(2)**2*QR(2)/(4.*QR(2)*QA*P**2+OMEGA**2)
      UX=CC*OMEGA
      UZ=-2.*P*CC*QA
C
C Combine source and receiver components
C
400   COMP(1)=MZZ*UX
      COMP(2)=MXX*UX
      COMP(3)=MXZ*UX
      COMP(4)=MZZ*UZ
      COMP(5)=MXX*UZ
      COMP(6)=MXZ*UZ
      COMP(7)=FZ*UX
      COMP(8)=FX*UX
      COMP(9)=FZ*UZ
      COMP(10)=FX*UZ
C
      RETURN
      END                                                               COMPON
c****************************************************************************
      COMPLEX FUNCTION COEFH (P,VEL1,VEL2,ID)                           COEFH
c        the same as function COEFF, but for SH waves
C
C-----------------------------------------------------------------------
C Routine to compute product of reflection/transmission coefficients at
C interface, equation (7).
C-----------------------------------------------------------------------
C
C Input: P          = horizontal slowness
C        VEL1(*)    = parameters above interface
C                     VEL1(1) = P slowness
C                     VEL1(2) = S slowness (zero for a fluid)
C                     VEL1(3) = density (zero for a free surface)
C        VEL2(*)    = parameters below interface
C                     VEL2(1) = P slowness
C                     VEL2(2) = S slowness (zero for a fluid)
C                     VEL2(3) = density
C        ID(*)      = multiplicity of coefficients. ID(k) is nu_ÌlkË
C                     where
C
C                                HH           HH      HH           HH
C                     k = 4 for R  , = 5 for T   and T  , = 6 for R
C                                D            D       U            U
C
C                     See figures 2 and 3.
C
C Output: COEFH     = product of reflection/transmission coefficients,
C                     e.g. inner product in equation (6) with
C                     equation (7).
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation numbers refer to that paper.
C
C Calls: none
C
C Errors: Note it is assumed that the input parameters are all defined
C         sensibly and are compatible.  If for S1S1, the
C         incident ray is evanescent, a turning ray is assumed.  For
C         other coefficients, if an incident or generated ray is
C         evanescent, then it probably indicates an error in the input
C         parameters.  The coefficient may be correct or may be zero
C         as the slownesses of the incident and generated rays are
C         assumed real in some places.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: V. Maupin, after subroutine COEFF of Chapman
C
C Date: July 1994
C
C Arguments:
C
      REAL P,VEL1(*),VEL2(*)
      INTEGER ID(*)
C
C Local variables:
C
C The code has been written to reduce the amount of unnecessary complex
C arithmetic.  This may not be necessary with smart FORTRAN compilers.
C DUM is used to simplify zeroing many variables.
C
      REAL DUM(12),P1,P2,S1,S2,D1,D2,eMU1,eMU2,PP,PP4,QA1R,QA1I,QA2R,QA2I,
     1 QB1R,QB1I,QB2R,QB2I,D2QA1R,D2QA1I,D1QA2R,D1QA2I
      COMPLEX AAP,AAM
      INTEGER K
C
      EQUIVALENCE (DUM(1),QA1R),(DUM(2),QA1I),(DUM(3),QA2R),
     1 (DUM(4),QA2I),(DUM(5),QB1R),(DUM(6),QB1I),(DUM(7),QB2R),
     2 (DUM(8),QB2I),(DUM(9),D1QA1R),(DUM(10),D1QA1I),(DUM(11),D2QA2R),
     3 (DUM(12),D2QA2I)
C
C Initialize COEFH=1
C
      COEFH=CMPLX(1.,0.)
C
C Test for non-zero coefficients
C
      DO 1 K=4,6
          IF (ID(K).NE.0) GO TO 2
1     CONTINUE
C
C Return if no coefficients - note COEFH=1
C
      RETURN
C
C Initialize variables
C
2     PP=P*P
      PP4=4.*PP
      DO 3 K=1,12
          DUM(K)=0.
3     CONTINUE
      S1=VEL1(2)
      D1=VEL1(3)
      emu1=d1/s1/s1
      S2=VEL2(2)
      D2=VEL2(3)
      emu2=d2/s2/s2
C
C Set up real or imaginary parts of vertical slownesses.
C Note Reimann sheet is defined by Im(q) >= 0.
C
C qa2
C
      X2=(S2-P)*(S2+P)
      IF (X2) 11,20,12
11    QA2I=SQRT(-X2)
      D2QA2I=emu2*QA2I
      GO TO 20
12    QA2R=SQRT(X2)
      D2QA2R=emu2*QA2R
C
C qa1
C
 20   continue
      X1=(S1-P)*(S1+P)
      IF (X1) 21,30,22
21    QA1I=SQRT(-X1)
      D1QA1I=emu1*QA1I
      GO TO 30
22    QA1R=SQRT(X1)
      D1QA1R=emu1*QA1R
C
C Aa+ and Aa- - equation (7l)
C
30    AAP=CMPLX(D1QA1R+D2QA2R,D1QA1I+D2QA2I)
      AAM=CMPLX(D1QA1R-D2QA2R,D1QA1I-D2QA2I)
C
C  reflection/transmission coefficients 
C
      K=ID(4)
      IF (K.EQ.0) GO TO 401
      IF (P.GE.S1) GO TO 410
      COEFH=COEFH*(AAM/AAP)**K
C
C H1H2 and H2H1
C
401   K=ID(5)
      IF (K.EQ.0) GO TO 402
      COEFH=COEFH*(CMPLX(2.*D2QA2R,2.*D2QA2I)/AAP)**K
C
C H2H2
C
402   K=ID(6)
      IF (K.EQ.0) GO TO 900
      COEFH=COEFH*(-AAM/AAP)**K
      GO TO 900
C
C H1H1 turning point
C
410   COEFH=COEFH*CMPLX(0.,-1.)**K
      GO TO 401
   
C
900   RETURN
C
      END                                                               COEFH

c*************************************************************************
      SUBROUTINE COMPOH (P,VELS,ISS,VELR,IRR,COMP)                      COMPON
C
C-----------------------------------------------------------------------
C Routine to compute source and receiver component functions,
C equations (4) and (5) (see also equation (9)).
C-----------------------------------------------------------------------
C
C Input: P          = horizontal slowness
C        VELS(*)    = source parameters
C                     VELS(1) = P slowness at source
C                     VELS(2) = S slowness at source
C                                For a fluid, this is irrelevant and
C                                should be set zero for routine COEFH
C                     VELS(3) = density at source
C        ISS        = source type
C                     ISS = 1 for downgoing P source ray
C                         = 2 for downgoing SV source ray
C                         = 5 for upgoing P source ray
C                         = 6 for upgoing SV source ray
C        VELR(*)    = receiver parameters
C                     VELR(1) = P slowness at receiver
C                     VELR(2) = S slowness at receiver
C                                For a fluid, this is irrelevant and
C                                should be set zero for routine COEFH
C                     VELR(3) = density at receiver
C        IRR        = receiver type
C                     IRR = 1 for upgoing P receiver ray in medium
C                         = 2 for upgoing SV receiver ray in medium
C                         = 5 for downgoing P receiver ray in medium
C                         = 6 for downgoing SV receiver ray in medium
C                         = 9 for P receiver ray at free surface
C                         = 10 for SV receiver ray at free surface
C
C Output: COMP(*)   = components of M U and F U
C                     COMP(1) = Mxy Uy
C                     COMP(2) = Mzy Uy
C                     COMP(3) = Fy Uy
C                     Note a factor of -sqrt(p/8)/pi**2 is included.
C
C Theory: See 'The WKBJ Seismogram Algorithm' by C.H.Chapman,
C         Chu Jen-yi and D.G.Lyness, in 'Seismological Algorithms',
C         Ed. D.J.Doornbos
C         Equation numbers refer to that paper.
C
C Calls: none
C
C Errors: Note it is assumed that the input parameters are all defined
C         sensibly and are compatible.
C
C Language: FORTRAN-77 (and other FORTRANs)
C
C Machine: VAX 11/750 (and many others)
C
C Author: V. Maupin, after subroutine COMPON of Chapman
C
C Date: July 1994
C
C Arguments:
C
      REAL P,VELS(*),VELR(*)
      INTEGER ISS,IRR
      COMPLEX COMP(*)
C
C Local variables:
C
      COMPLEX UY
      REAL QS,QR,C,FY,MXY,MZY
      INTEGER ISV,IRV,I
C
C Source and receiver vertical slownesses.
C
C Qbeta  may be imaginary so include ABS.
C
      DO 100 I=1,2
          QS=SQRT(ABS(VELS(2)-P)*(VELS(2)+P))
          QR=SQRT(ABS(VELR(2)-P)*(VELR(2)+P))
100   CONTINUE
C
C Factor of -sqrt(p/8)/pi**2 * 1/sqrt(2*rho0*q0) * 1/sqrt(2*rho*q)
C from equations (9), (4) and (5).
C
      C=-1.791122E-2*SQRT(P/VELS(3)/VELR(3)/QS/QR)
C
C Source components - equation (4)
C
C Body force source - equation (4b)
C
      FY=VELS(2)
C
C Moment tensor source - equation (4a)
C
      MXY=P*VELS(2)
      MZY=QS*VELS(2)
C
C Change source direction - equation (4)
C
      IF (ISS.GE.5) MZY=-MZY
C
C Receiver components - equation (5)
C
C receiver in medium - equation (5a)
C
      UY=C*CMPLX(1.,0.)*VELR(2)
C
C receiver at free surface - equation (5d)
C
      IF (IRR.EQ.10) UY=2.*UY 
C
C Combine source and receiver components
C
400   COMP(1)=MXY*UY
      COMP(2)=MZY*UY
      COMP(3)=FY *UY
C
      RETURN
      END                                                               COMPON
