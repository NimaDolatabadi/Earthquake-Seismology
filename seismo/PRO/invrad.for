c$debug
      PROGRAM INVRAD
c   march 31 99 by jh   ----verson 7.0 check--------- no changes
C
C  Program to invert amplitudes of different phases for an
C    earthquake source, using the Langston, BSSA, 1981, p. 1 moment
C    tensor formulation.  Written by John E. Ebel, Summer, 1987
C    at the Geophysical Institute, University of Karlsruhe, W. Germany.
C    Paper on method being published in GEOPHYSICAL JOURNAL.
C    Please report all errors, improvements, etc. to John E. Ebel at Weston
C    Observatory of Boston College (EBEL@BCVMS.BITNET).
C
C  A bug in the SH radiation pattern part of the program was fixed 8/5/92
C
      DIMENSION VTOPP(50), VBOTP(50), GRP(50), TH(50), DEPTOP(50)
      DIMENSION VTOPS(50), VBOTS(50), GRS(50)
      DIMENSION R(50),AZ(50),AMP(50),D(50),P(50),HDC(3),DM0(50)
      DIMENSION HGREEN(6)
      CHARACTER*4 SN(50),PS(50)*2,COMP(50)*1,HEAD*40
      DIMENSION LEGDUPP(50), LEGDUPS(50)
      CHARACTER*40 FNAME, TYPE*10, ANS*1
      REAL MTENS(3,3),M0,N
      DIMENSION G(50,5),RESM(5,5),RESD(50,50),DM(6),VARM(5)
      DIMENSION S(3),N(3),B(3),AMPCOR(50),SDRMOM(6)



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver


      NMAX=50  !  Maximum number of stations allowed at present
      RHO=2.7  !  Assumed density in g/cm**3 in source region
      FPIRHO=4.*3.14159265*RHO
      TAREA=.01    ! Assumed area of time function in sec

C
C  Open output file
C
      OPEN(7,FILE='invrad.out')
C
C***Get input from the screen
C
1000  continue
cjh      WRITE(6,*)' Give file name for CRUSTAL MODEL (blank for STANDARD M
cjh     *ODEL):'
cjh      READ(5,1) FNAME
      fname='invrad.mod'   ! cjh
1     FORMAT(A)
      IF(FNAME(1:1).EQ.' ') FNAME='MODEL1.DAT'
C
cjh      WRITE(6,*)' Give SOURCE DEPTH in km:'
cjh      READ(5,*) SRCEDEP
c
c   simple fix to get depth from fixed input file
c
      open(1,file='invrad.inp',status='old')
      read(1,'(a1)') i        ! comment line
      read(1,'(i5,2f5.1)') i,srcedep,datsd
      close(1)
      
C
      if(srcedep.eq.0.0) then
         write(6,*)' Source depth cannot be zero'
         WRITE(6,*)' Give SOURCE DEPTH in km:'
         READ(5,*) SRCEDEP
      endif
c
      WRITE(7,144) SRCEDEP
144   FORMAT(' Source depth: ',F5.2,' km')
C
      if(datsd.eq.0.0) then
        write(6,*)' Readings must have errors'
        WRITE(6,*)' Give ERROR in data readings:'
         READ(5,*) DATSD
      endif
C
cjh      WRITE(6,*)' Read STATION DATA from SCREEN (Y or N)?'
cjh      READ(5,1) ANS
      ans='n'
      IUNIT=9   !  Unit number for input station data
      IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
         CALL SCRDAT(NMAX,HEAD,NSTAT,SN,COMP,PS,R,AZ,AMP,IUNIT)
        ELSE
         CALL FILDAT(NMAX,HEAD,NSTAT,SN,COMP,PS,R,AZ,AMP,-1,IUNIT)
      END IF
C
C***Read in model and set up crustal arrays
      CALL ZERO(50,VTOPP)
      CALL ZERO(50,VTOPS)
      CALL ZERO(50,VBOTP)
      CALL ZERO(50,VBOTS)
      CALL ZERO(50,TH)
      CALL ZERO(50,GRP)
      CALL ZERO(50,GRS)
      CALL ZERO(50,DEPTOP)
      XTEST=.01
      CALL GETMODEL(FNAME,VTOPP,VBOTP,TH,GRP,DEPTOP,NLAYER)
      CALL SRCELAYR(VTOPP,VBOTP,TH,GRP,DEPTOP,NLAYER,SRCEDEP,LAYBELSR)
C
      WRITE(6,*)' '
      WRITE(6,*)'               *****CRUSTAL MODEL*****'
      WRITE(6,*) '     VTOPP     VTOPS     GRP       TH       DEPTOP'
      WRITE(7,*)' '
      WRITE(7,*)'               *****CRUSTAL MODEL*****'
      WRITE(7,*) '     VTOPP     VTOPS     GRP       TH       DEPTOP'
      DO 101 J=1, NLAYER
      VTOPS(J)=VTOPP(J)/SQRT(3.)
      VBOTS(J)=VBOTP(J)/SQRT(3.)
      WRITE(7,1001) VTOPP(J), VTOPS(J), GRP(J), TH(J), DEPTOP(J)
101   WRITE(6,1001) VTOPP(J), VTOPS(J), GRP(J), TH(J), DEPTOP(J)
1001  FORMAT(2X,F8.2,2X,F8.2,2X,F8.5,2X,F8.2,2X,F8.2)
      WRITE(6,*)' '
C
      DO 500 J=1,NSTAT
      CALL ZEROI(50,LEGDUPP)
      CALL ZEROI(50,LEGDUPS)
      TYPE='P'
      IF(PS(J).EQ.' P'.OR.PS(J).EQ.'P '.OR.PS(J).EQ.' p'.OR.PS(J).EQ.
     *'p ') THEN
            CALL PATHDEF(TYPE,NLAYER,LAYBELSR,VTOPP,MANTOP,LEGDUPP)
        ELSE
            CALL PATHDEF(TYPE,NLAYER,LAYBELSR,VTOPP,MANTOP,LEGDUPS)
      END IF
C***Iterate in on the correct ray parameter and travel time using the
C    Newton Divided Differences method
      RR=R(J)   !  Needed for HP EMA statement
      CALL NEWTTME(RR,XTEST,LEGDUPP,LEGDUPS,VTOPP,VBOTP,GRP,VTOPS,
     *VBOTS,GRS,TH,NLAYER,LAYBELSR,MANTOP,XMAX,T,PP,NITER)
      P(J)=PP  !  Needed for the HP EMA statement
      IF(PS(J).EQ.' P'.OR.PS(J).EQ.'P '.OR.PS(J).EQ.' p'.OR.PS(J).EQ.
     *'p ') THEN
          PP=P(J)  !  Needed for the HP EMA statement
          CALL PTHLEN(LAYBELSR-1,TH,VTOPP,PP,RTOT)
        ELSE
          PP=P(J)  !  Needed for the HP EMA statement
          CALL PTHLEN(LAYBELSR-1,TH,VTOPS,PP,RTOT)
      END IF
      INDR=0
      IF(PS(J).EQ.'SH'.OR.PS(J).EQ.'sh') THEN
          INDR=4
          V=VTOPS(LAYBELSR-1)
        ELSE IF (PS(J).EQ.'SV'.OR.PS(J).EQ.'sv') THEN
          INDR=3
          V=VTOPS(LAYBELSR-1)
        ELSE IF(PS(J).EQ.' P'.OR.PS(J).EQ.'P '.OR.PS(J).EQ.' p'.OR.
     *PS(J).EQ.'p ') THEN
          INDR=5
          V=VTOPP(LAYBELSR-1)
      END IF
      PP=P(J)  !  Needed for the HP EMA statement
      CALL VRAD(INDR,PP,V,-1.,HDC)
      AAZZ=AZ(J)  !  Needed for the HP EMA statement
      CALL MTGREEN(INDR,AAZZ,HDC,HGREEN)
      NTYPE=0
      IF(COMP(J).EQ.'Z'.OR.COMP(J).EQ.'z') THEN
          NTYPE=1
        ELSE IF(COMP(J).EQ.'R'.OR.COMP(J).EQ.'r') THEN
          NTYPE=2
        ELSE IF(COMP(J).EQ.'T'.OR.COMP(J).EQ.'t') THEN
          NTYPE=3
      END IF
      PP=P(J)  !  Needed for the HP EMA statement
      VTP=VTOPP(1)  !  Needed for the HP EMA statement
      VTS=VTOPS(1)  !  Needed for the HP EMA statement
      CALL RECEVH(INDR,PP,NTYPE,VTP,VTS,RCV)
C***Check if ray past critical anlge at free surface
      IF(RCV.EQ.-99999.) THEN
         WRITE(7,231) SN(J),COMP(J),PS(J)
         write(6,231) SN(J),COMP(J),PS(J)
231      FORMAT('  WARNING!!!  ',A,1X,A,1X,A,'  past critical angle
     * at free surface!')
      END IF
C***Scale Green's functions by receiver function times geometric factor
C     and divide them by 4*PI*RHO times the area of the time function
C     (units of seconds).
      AMPCOR(J)=RCV/(RTOT*TAREA*FPIRHO)
      AA=AMPCOR(J)  !  Needed for the HP EMA statement
      CALL RESCALE(AA,5,HGREEN)
      DO 600 I=1,5
600   G(J,I)=HGREEN(I)
500   CONTINUE
C
C
C***Invert the data
      CALL CLSPCK(50,5,G,NSTAT,5)
      CALL GENINV(NSTAT,5,G,AMP,DM,RESM,RESD,VARM)
C***Get the strike, dip, rake and moment from the moment tensor
      DM(6)=-DM(1)-DM(2)   !  Making the trace of the moment tensor zero
      CALL GLTOM(MTENS,DM)
      CALL FGMRTI(S,N,B,M0,MTENS,CLVDM)
      CALL FGI(ST,DP,RK,S,N,B)
      CALL CNFG(S,N,B)   !  Other alternative representation of source
      CALL FGI(ST1,DP1,RK1,S,N,B)
C***Calculate the displacements expected from the best model
      CALL SDTOM(ST,DP,RK,SDRMOM)
      CALL OPNPCK(50,5,G,NSTAT,5)
      DO 520 J=1,NSTAT
      DO 521 II=1,5
521   HGREEN(II)=G(J,II)
      CALL DISPS(HGREEN,SDRMOM,DD) ! Calculate amps from double-couple mom tens
      CALL DISPS(HGREEN,DM,DM0(J)) ! Calculate amps from original mom tens
520   D(J)=DD  !  Needed for HP EMA statement
      CALL RESCALE(M0,NSTAT,D)
C***Find the RMS error of the expected compared to the observed amplitudes
      ERRRMS=0.
      DO 505 J=1,NSTAT
505   ERRRMS=ERRRMS+(D(J)-AMP(J))*(D(J)-AMP(J))
      ERRRMS=SQRT(ERRRMS/NSTAT)
C***Unpack the data resolution matrix
      CALL OPNPCK(50,50,RESD,NSTAT,NSTAT)
C***Find the standard deviation of the moment tensor and use to find the
C     standard deviation of the strike, dip, rake and seismic moment
      DO 506 J=1,5
506   IF(VARM(J).NE.0.) VARM(J)=DATSD*SQRT(VARM(J))
      CALL SDRM0SD(VARM,M0,ST,DP,RK,SDM0,SDST,SDDP,SDRK)
C***Write it all out
      WRITE(6,1100)
      WRITE(7,1100)
      CALL OUTINV(M0,SDM0,HEAD,NSTAT,SN,COMP,PS,R,AZ,AMP,D,DM0,
     * ST,DP,RK,ST1,DP1,RK1,SDST,SDDP,SDRK,ERRRMS,CLVDM,7)
1100  FORMAT(' ')
      WRITE(7,1101)
1101  FORMAT(' MODEL RESOLUTION MATRIX')
      DO 701 J=1,5
701   WRITE(7,702)(RESM(J,I),I=1,5)
702   FORMAT(18(F6.3,1X),F6.3)
      WRITE(7,1100)
      WRITE(7,1102)
1102  FORMAT(' DATA RESOLUTION MATRIX')
      DO 704 J=1,NSTAT
704   WRITE(7,702)(RESD(J,I),I=1,NSTAT)
      WRITE(7,1100)
      WRITE(7,1100)
      WRITE(6,1100)
999   continue
cjh     WRITE(6,*)' Do another run (Y/N)?'
cjh      READ(5,1)ANS
      ans='n'
      IF (ANS.EQ.'Y'.OR.ANS.EQ.'y') GO TO 1000
      CLOSE(7)
      END
C
C
C
      SUBROUTINE PTHLEN(NLAYERS,TH,V,P,R)
C
C  Subroutine to compute the total length of the travel path for a direct P or
C    S wave.  The result can be used for geometric spreading corrections.
C
C  Inputs:  NLAYERS - # of layers traversed by ray
C           TH - array of layer thickness values
C           V - array of layer velocities.  V(I) corresponds to TH(I).
C           P - ray parameter of direct ray
C
C  Output:  R - total travel length of ray
C
      DIMENSION TH(NLAYERS),V(NLAYERS)
      R=0.
      IF(NLAYERS.LE.0) RETURN
      DO 1 J = 1, NLAYERS
1     R = R + TH(J) / SQRT(1.-P*P*V(J)*V(J))
      RETURN
      END
C
C
C
      SUBROUTINE HRAD (IND,AZ,ST,DP,RK,A)
C
C  Subroutine to calculate the Ai horizontal, farfield radiation pattern
C   terms, after Langston and Helmberger, GJRAS, v. 42, pp 117-130, 1975
C   equations (2).
C
C  Inputs:  IND - =3 for SV wave; =4 for SH wave; or =5 for P wave, as i
C                 takes off from the source
C           AZ - station azimuth from North in degrees
C           ST - fault strike from North in degrees
C           DP - fault dip in degrees
C           RK - fault rake in degrees
C
C  Outputs: A(I) - Ai terms, A1, A2, and A3, for the corresponding phase
C                  indicated by the value of IND
C
      DIMENSION A(3)
      PIBY180 = 3.14159265/180.
      THETA = (AZ-ST)*PIBY180
      DPRAD = DP * PIBY180
      RKRAD = RK * PIBY180
      IF (IND.LT.3.OR.IND.GT.5) THEN
          A(1) = 0.
          A(2) = 0.
          A(3) = 0.
          RETURN
        ELSE IF (IND.EQ.4) THEN
C***SH wave
          A(1) = COS(2.*THETA)*COS(RKRAD)*SIN(DPRAD) - .5*SIN(2.*THETA)
     &*SIN(RKRAD)*SIN(2.*DPRAD)
          A(2) = -SIN(THETA)*COS(RKRAD)*COS(DPRAD) - COS(THETA)*
     &SIN(RKRAD)*COS(2.*DPRAD)
          A(3) = 0.
        ELSE
C***P or SV wave
          A(1) = SIN(2.*THETA)*COS(RKRAD)*SIN(DPRAD) + COS(2.*THETA)
     &*SIN(RKRAD)*SIN(2.*DPRAD)/2.
          A(2) = COS(THETA)*COS(RKRAD)*COS(DPRAD) - SIN(THETA)*
     &SIN(RKRAD)*COS(2.*DPRAD)
          A(3) = SIN(RKRAD)*SIN(2.*DPRAD)/2.
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE MTGREEN (IND,AZ,HDC,HMT)
C
C  Subroutine to calculate moment tensor formulation Green's functions
C    from the far-field, first-motion approximation, double-couple Greens
C    functions; after Langston, BSSA, v. 71, pp 1-24, 1981, equations (6)
C    are an example.
C    The SH Green's functions have been multiplied by -1 to make the Langston
C    formulation of the problem have the same moment tensor and receiver
C    coordinate system as Aki&Richards (equations 4.87, 4.91, and Box 9.10)
C    where the receiver coordinate system is: Positive Up, Positive radially
C    Toward source, and Positive to Right facing away from source.  This
C    makes the formulation directly compatible with the Aki&Richards moment
C    tensor (Box 4.4) which is -1 times the Langston moment tensor.  See
C    Aki&Richards Figure 4.20 for the complete coordinate system.
C
C  Inputs:   IND - =3 for SV wave; =4 for SH wave; =5 for P wave, as ray
C                  leaves from the source
C            AZ - station azimuth from North in degrees
C            HDC(J) - =Cj terms for P waves (j=1,2,3)
C                     =SVj terms for SV waves (j=1,2,3)
C                     =SHj terms for SH waves (j=1,2)
C                     These are the vertical radiation pattern terms after
C            Langston & Helmberger, GJRAS, v.42, pp. 117-130, 1975, equations
C            (7).
C
C  Output:  HMT(1) = Green's function for M11
C           HMT(2) = Green's function for M22
C           HMT(3) = Green's function for M12
C           HMT(4) = Green's function for M13
C           HMT(5) = Green's function for M23
C           HMT(6) = Green's function for M33
C
      DIMENSION HDC(3),HMT(6)
      AZRAD = AZ * 3.14159265 / 180.
      IF (IND.LT.3.OR.IND.GT.5) THEN
          DO 1 J=1,6
1         HMT(J) = 0.
          RETURN
        ELSE IF (IND.EQ.4) THEN
C***SH wave
          HMT(1) = -1.*(HDC(1) * SIN(2.*AZRAD) / 2.)
          HMT(2) = -1.*(-HDC(1) * SIN(2.*AZRAD) / 2.)
          HMT(3) = -1.*(-HDC(1) * COS (2.*AZRAD))
          HMT(4) = -1.*(-HDC(2) * SIN(AZRAD))
          HMT(5) = -1.*(HDC(2) * COS(AZRAD))
          HMT(6) = 0.
        ELSE
C***P or SV wave
          HMT(1) = -HDC(1) * COS(2.*AZRAD) / 2. + HDC(3) / 2.
          HMT(2) = HDC(1) * COS(2.*AZRAD) / 2. + HDC(3) / 2.
          HMT(3) = -HDC(1) * SIN(2.*AZRAD)
          HMT(4) = HDC(2) * COS(AZRAD)
          HMT(5) = HDC(2) * SIN(AZRAD)
          HMT(6) = 0.
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE VRAD (IND,P,V,E,H)
C
C  Subroutine to compute the vertical, far-field, radiation patterns from
C    a double-couple source, equations (7) from Langston and Helmberger,
C    GJRAS, v. 42, pp. 117-130, 1975.
C
C  Inputs:  IND - =3 for SVj terms (SV waves from source)
C                 =4 for SHj terms (SH waves from source)
C                 =5 for Cj terms (P waves from source)
C           P - ray parameter for ray from source
C           V - velocity of particular wave (P or S) at source
C           E - =+1 for downgoing ray from source; =-1 for upgoing ray from
C               source
C
C  Output: H(I) - =Ci for P waves; SVi for SV waves; or SHi for SH waves
C                 depending upon value of IND
C
      DIMENSION H(3)
      IF (IND.LT.3.OR.IND.GT.5) THEN
          H(1) = 0.
          H(2) = 0.
          H(3)=0.
          RETURN
        ELSE IF (IND.EQ.5) THEN
C***P wave
          ETA2 = 1./(V*V) - P*P
          IF (ETA2.LT.0.) THEN
            H(1) = 0.
            H(2) = 0.
            H(3) = 0.
            RETURN
          END IF
          ETA = SQRT(ETA2)
          H(1) = -P*P
          H(2) = 2.*E*P*ETA
          H(3) = P*P - 2.*ETA2
        ELSE
          ETB2 = 1./(V*V) - P*P
          IF (ETB2.LE.0.) THEN
            H(1) = 0.
            H(2) = 0.
            H(3) = 0.
            RETURN
          END IF
          ETB = SQRT(ETB2)
          IF (IND.EQ.3) THEN
C***SV wave
            H(1) = -E*P*ETB
            H(2) = ETB2 - P*P
            H(3) = 3.*E*P*ETB
           ELSE
C***SH wave
            H(1) = 1./(V*V)
            H(2) = -E*ETB / (V*V*P)
            H(3) = 0.
          END IF
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE SDTOM(ST,DP,RK,M)
C
C  Subroutine to convert strike, dip and rake to moment tensor components.
C    After Langston, BSSA, v. 71, pp 1-24, 1981, equations (4), but multiplied
C    all components by -1 to be compatible with Aki&Richards formulation,
C    Box 4.4.
C
C  Inputs:  ST - fault strike from North in degrees
C           DP - fault dip in degrees
C           RK - fault rake in degrees
C
C  Output:  M - moment tensor components with:
C               M(1)=M11
C               M(2)=M22
C               M(3)=M12
C               M(4)=M13
C               M(5)=M23
C               M(6)=M33
C
      DIMENSION M(6)
      REAL M
      PIBY180 = 3.14159265 / 180.
      STRAD = ST * PIBY180
      DPRAD = DP * PIBY180
      RKRAD = RK * PIBY180
      M(1) = SIN(STRAD)*SIN(STRAD)*SIN(RKRAD)*SIN(2.*DPRAD) +
     &SIN(2.*STRAD)*COS(RKRAD)*SIN(DPRAD)
      M(2) = COS(STRAD)*COS(STRAD)*SIN(RKRAD)*SIN(2.*DPRAD) -
     &SIN(2.*STRAD)*COS(RKRAD)*SIN(DPRAD)
      M(3) = -.5*SIN(2.*STRAD)*SIN(RKRAD)*SIN(2.*DPRAD) -
     &COS(2.*STRAD)*COS(RKRAD)*SIN(DPRAD)
      M(4) = COS(STRAD)*COS(RKRAD)*COS(DPRAD) + SIN(STRAD)
     &*SIN(RKRAD)*COS(2.*DPRAD)
      M(5) = -COS(STRAD)*SIN(RKRAD)*COS(2.*DPRAD) + SIN(STRAD)
     &*COS(RKRAD)*COS(DPRAD)
      M(6) = -M(1) - M(2)
C***Multiply all components by -1 to be compatible with Aki&Richards
      DO 1 J=1,6
1     M(J) = -1.*M(J)
      RETURN
      END
C
C
C
      SUBROUTINE FGMRTI(S,N,B,M0,M,MCLVD)
C
C  Subroutine to compute the the fault geometry unit vectors slip (S),
C    normal (N), and null (B), as well as moment M0, from the moment tensor
C    M.  This version is
C    for X, Y, Z - (North, East, Down) coordinates.  From Karl Koch,
C    by John Ebel, 3/18/87.
C
C  Revised to calculate the smallest CLVD of the moment tensor.  J. Ebel,
C     Apr. 8, 1987.
C
C  ###Calls CROSSV, FGPAF, FGPAI, and JACOBI
C
C  Inputs:  M(I,J) - Moment tensor component Mij
C
C  Outputs: S(I) - Slip vector components in X, Y, Z coordinate system
C           N(I) - Normal vector components in X, Y, Z coordinate system
C           B(I) - Null vector components in X, Y, Z coordinate system
C           M0 - Seismic moment
C           MCLVD - Moment of smallest compensated linear vector dipole
C                    component
C
      REAL S(3),N(3),B(3),M0,M(3,3),P(3),T(3),EV(3),VEC(3,3),MCLVD
C ??? CALL FGPAF(S,N,P,T)  ! Statement as received from K. Koch
C***Decompose the matrix into eigenvalues and eigenvectors
      CALL JACOBI(3,M,EV,VEC,NROT)
C***Find the maximum and minimum eigenvalues
      EMAX=EV(1)
      EMIN=EMAX
      NP=1
      NT=1
      DO 2 K=2,3
      IF (EV(K).GE.EMIN) GO TO 3
      EMIN=EV(K)
      NP=K
3     IF(EV(K).LE.EMAX) GO TO 2
      EMAX=EV(K)
      NT=K
2     CONTINUE
C***Compute S and N
      SP=SIGN(1.,VEC(3,NP))
      ST=SIGN(1.,VEC(3,NT))
      DO 4 K=1,3
      P(K)=-SP*VEC(K,NP)
4     T(K)=-ST*VEC(K,NT)
      CALL CROSSV(P,T,B)
C***Compute the moment of the double couple source
      M0=.5*(EV(NT)-EV(NP))
C***Compute the moment of the smallest compensated linear vector dipole
C     component
      MCLVD=.5*(EV(NT)+EV(NP))
      CALL FGPAI(S,N,P,T)
      RETURN
      END
C
C
C
      SUBROUTINE CROSSV(A,B,C)
C
C  Subroutine to compute the vector product C = A X B, where A, B, and C
C    are all three vectors.  Programmed on 30 Oct., 1978 by R. Buland.
C
      DIMENSION A(3),B(3),C(3)
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-B(1)*A(2)
      RETURN
      END
C
C
C
      SUBROUTINE FGPAI(S,N,P,T)
C
C  Subroutine to convert unit vectors defining the principal axis coordinate
C    system P, T, and B to the unit vectors S, N, and B defining the fault
C    geometry coordinate system (see subroutine FGFXY for a definition of
C    the fault geometry coordinate system).  The P, T, and B unit vectors are
C    colinear with the P, T, and B axes such that P(3) <= 0 and T = B X P
C    (where B = N X S).  B is not needed as it is common to both systems.
C    Programmed on 30 Oct. 1978 by R. Buland.
C
      REAL S(3),N(3),P(3),T(3)
      DATA CON/.70710678/
      CN=CON
      IF(T(3)+P(3).GT.0.) CN=-CN
      DO 2 K=1,3
      N(K)=CN*(T(K)+P(K))
2     S(K)=CN*(T(K)-P(K))
      RETURN
      END
C
C
C
      SUBROUTINE FGI(SIG,DEL,GAM,S,N,B)
C
C  Subroutine to compute the strike (SIG), dip (DEL), and slip (GAM) from
C    the fault geometry unit vectors slip (S), normal (N), and null (B).
C    The strike is measured clockwise from North in the (X,Y) plane such
C    that if one faces in the strike direction the fault plane dips to the
C    right at an acute angle.  The strike is defined to fall in the range
C    (0<=SIG<360).  The dip is the acute angle between the (X,Y) plane
C    and the fault plane measured in the plane normal to the strike with
C    (0<=DIP<=90).  Slip is the direction the hanging wall moves relative to
C    the foot wall measured counterclockwise from the strike in the fault
C    plane, with (0<=GAM<360).  S is the unit vector in the fault plane in
C    the direction of slip.  N is the unit vector normal to the fault plane
C    such that N(3)<=0.  B = N X S.
C    Vectors are in Cartesian coordinates: X is North, Y is East, and Z is
C    positive down.  All angles are given in degrees.  Programmed by K. Koch
C    on 1 Jun., 1985.
C
      REAL S(3),N(3),B(3)
      DATA CNI/57.29578/
      DEL=ACOS(-N(3))
      SIG=ATAN2(-N(1),N(2))
      GAM=CNI*ATAN2(-S(3)/SIN(DEL),S(2)*SIN(SIG)+S(1)*COS(SIG))
      DEL=CNI*DEL
      SIG=CNI*SIG
      IF (SIG.LT.0.) SIG=SIG+360.
      IF (GAM.LT.0.) GAM=GAM+360.
      RETURN
      END
C
C
C
      SUBROUTINE FGF(SIG,DEL,GAM,S,N,B)
C
C  Subroutine to convert from a despription of fault geometry in terms of
C    the angles strike (SIG), dip (DEL), and slip (GAM) to fault geometry
C    unit vectors slip (S), normal (N), and null (B).
C    The strike is measured clockwise from North in the (X,Y) plane such
C    that if one faces in the strike direction the fault plane dips to the
C    right at an acute angle.  The strike is defined to fall in the range
C    (0<=SIG<360).  The dip is the acute angle between the (X,Y) plane
C    and the fault plane measured in the plane normal to the strike with
C    (0<=DIP<=90).  Slip is the direction the hanging wall moves relative to
C    the foot wall measured counterclockwise from the strike in the fault
C    plane, with (0<=GAM<360).  S is the unit vector in the fault plane in
C    the direction of slip.  N is the unit vector normal to the fault plane
C    such that N(3)<=0.  B = N X S.
C    Vectors are in Cartesian coordinates: X is North, Y is East, and Z is
C    positive down.  All angles are given in degrees.  Programmed by K.
C    Koch on 1 Jun., 1985.
C
      REAL S(3),N(3),B(3)
      DATA CON/.017453293/
      SS=SIN(SIG*CON)
      CS=COS(SIG*CON)
      SD=SIN(DEL*CON)
      CD=COS(DEL*CON)
      SG=SIN(GAM*CON)
      CG=COS(GAM*CON)
      S(1)=CS*CG+SS*CD*SG
      S(2)=SS*CG-CS*CD*SG
      S(3)=-SD*SG
      N(1)=-SS*SD
      N(2)=CS*SD
      N(3)=-CD
      CALL CROSSV(N,S,B)
      RETURN
      END
C
C
C
      SUBROUTINE CNFG(S,N,B)
C
C  Subroutine which uses S, N, and B (see subroutine FGF) defining a
C    unique fault geometry to compute S, N, and B for the conjugate
C    fault geometry.  Programmed on 30 Oct 1978 by R. Buland.
C
      REAL P(3),T(3),B(3),S(3),N(3)
      CALL FGPAF(S,N,P,T)
      DO 1 K=1,3
      T(K)=-T(K)
1     B(K)=-B(K)
      CALL FGPAI(S,N,P,T)
      RETURN
      END
C
C
C
      SUBROUTINE FGMRTF(S,N,B,M0,M)
C
C  Subroutine to convert the unit vectors in the fault geometry of slip
C    (S) and normal (N) and the moment M0 to a moment tensor M.  See
C    subroutine FGF for details about the vectors and coordinate system.
C    Programmed by R. Buland on 30 Oct. 1978.
C
      REAL S(3),N(3),B(3),M0,M(3,3)
      DO 1 K=1,3
      DO 1 J=1,3
1     M(J,K)=M0*(S(K)*N(J)+S(J)*N(K))
      RETURN
      END
C
C
C
      SUBROUTINE FGPAF(S,N,P,T)
C
C  Subroutine to convert the unit vectors defining the fault geometry
C    coordinate system slip (S) and normal (N) to the unit vectors defining
C    the principal axis coordinate system: P, T, and B.  These unit vectors
C    are colinear with the P, T, and B axes such that P(3) <= 0 and T = B X P
C    (where B = N X S).  B is not needed as it is common to both systems
C    See subroutine FGF for details on the coordinate system used.
C    Programmed on 30 Oct. 1978 by R. Buland.
C    Updated on 11 Sep 1986 by K. Koch.
C
      REAL S(3),N(3),P(3),T(3)
      DATA CON/.70710678/
      CN=CON
      IF(N(3)-S(3).GT.0.) CN=-CN
      DO 1 K=1,3
      P(K)=CN*(N(K)-S(K))
1     T(K)=CN*(N(K)+S(K))
      RETURN
      END
C
C
C
      SUBROUTINE MTOGL(M,G)
C
C  Subroutine to convert a moment tensor M into a six-vector representation
C    G where G is compatible with the formulation of Langston, BSSA, v. 71,
C    pp. 1-24, 1981.  G(1)=M11; G(2)=M22; G(3)=M12; G(4)=M13; G(5)=M23;
C    G(6)=M33.
C    Programmed by J. Ebel, 3/18/1987.
C
      REAL M(3,3),G(6)
      G(1)=M(1,1)
      G(2)=M(2,2)
      G(3)=M(1,2)
      G(4)=M(1,3)
      G(5)=M(2,3)
      G(6)=M(3,3)
      RETURN
      END
C
C
C
      SUBROUTINE GLTOM(M,G)
C
C  Subroutine to convert the six-vector G into moment tensor M, where G is
C    compatible with the formulation of Langston, BSSA, v. 71, pp. 1-24,
C    1981.  Programmed by J. Ebel on 3/18/87.
C
      REAL M(3,3),G(6)
      M(1,1)=G(1)
      M(2,2)=G(2)
      M(1,2)=G(3)
      M(2,1)=M(1,2)
      M(1,3)=G(4)
      M(3,1)=M(1,3)
      M(2,3)=G(5)
      M(3,2)=M(2,3)
      M(3,3)=G(6)
      RETURN
      END
C
C
C
      SUBROUTINE PAF(ZETA,ETA,S,N,B)
C
C  Subroutine which converts an angular description (in degrees) of a set
C    of principal axes into the fault geometry coordinate system defined by
C    unit vectors S, N, and B (see subroutine FGF for a description of the
C    coordinate system).  The portion of each axis below the (X,Y) plane is
C    described by an azimuth, ZETA, measured clockwise from North in the
C    (X,Y) plane (0<=ZETA<360) and a plunge, ETA, the acute angle between the
C    (X,Y) plane and the axis measured in the plane of the axis and the
C    Z-axis (0<=ETA<=90).  ZETA(I) and ETA(I) describe the compression or
C    P axis (I=1), the tension or T axis (I=2), and the null or B axis (I=3).
C    Since the geometry is uniquely defined by the P and T axes, the B axis
C    (ZETA(3) and ETA(3)) is computed by PAF.  The unit vectors S, N, and B
C    define on the the two conjugate fault
C    planes consistent with the principal axes (subroutine CNFG will calculate
C    S, N, and B for the other).  This routine is defined in a (X, Y, Z)
C    coordinate system with orientation (North, East, Down), see Aki and
C    Richards, p 119ff.  Programmed by K. Koch, June 1, 1985.
C
C   Calls CROSSV and FGPAI
C
      REAL ZETA(3),ETA(3),S(3),N(3),B(3),P(3),T(3)
      DATA CON,CNI,TOL,TOL2/.017453293,57.29578,1E-5,2E-5/
      CE=COS(ETA(1)*CON)
      P(1)=COS(ZETA(1)*CON)*CE
      P(2)=SIN(ZETA(1)*CON)*CE
      P(3)=SIN(ETA(1)*CON)
      CE=COS(ETA(2)*CON)
      T(1)=COS(ZETA(2)*CON)*CE
      T(2)=SIN(ZETA(2)*CON)*CE
      T(3)=SIN(ETA(2)*CON)
      CALL CROSSV(P,T,B)
      CALL FGPAI(S,N,P,T)
C***Calculate ZETA and ETA for the B axis.
      CE=SIGN(1.,B(3))
      ZETA(3)=0.
      IF(ABS(B(1))+ABS(B(2)).GT.TOL2) ZETA(3)=CNI*ATAN2(CE*B(2),
     1   CE*B(1))
      ETA(3)=CNI*ASIN(CE*B(3))
      IF(ZETA(3).LT.0.) ZETA(3)=ZETA(3)+360.
      IF((ABS(B(3)).LE.TOL).AND.(ZETA(3).GT.180.)) ZETA(3)=ZETA(3)-180.
      RETURN
      END
C
C
C
      SUBROUTINE PAI(ZETA,ETA,S,N,B)
C
C  Subroutine to convert fault geometry coordinate system unit vectors slip
C    (S), normal (N), and null (B) into an angular description (in degrees)
C    of a set of principal axes.  This is the inverse of subroutine PAF.
C    See subroutine PAF for a description of the coordinate system, and
C    Aki and Richards p. 119ff.  Programmed by K. Koch on June 1, 1985.
C
      REAL ZETA(3),ETA(3),S(3),N(3),B(3),P(3),T(3)
      DATA CNI,TOL,TOL2/57.29578,1E-5,2E-5/
      CALL FGPAF(S,N,P,T)
      DO 2 K=1,3
2     ZETA(K)=0.
      CE=SIGN(1.,P(3))
      IF(ABS(P(1))+ABS(P(2)).GT.TOL2) ZETA(1)=CNI*ATAN2(CE*P(2),
     1  CE*P(1))
      ETA(1)=CNI*ASIN(CE*P(3))
      CE=SIGN(1.,T(3))
      IF(ABS(T(1))+ABS(T(2)).GT.TOL2) ZETA(2)=CNI*ATAN2(CE*T(2),
     1  CE*T(1))
      ETA(2)=CNI*ASIN(CE*T(3))
      CE=SIGN(1.,B(3))
      IF(ABS(B(1))+ABS(B(2)).GT.TOL2) ZETA(3)=CNI*ATAN2(CE*B(2),
     1  CE*B(1))
      ETA(3)=CNI*ASIN(CE*B(3))
      DO 1 K=1,3
      IF(ZETA(K).LT.0.) ZETA(K)=ZETA(K)+360.
1     CONTINUE
C***Put zeta into canonical form if ETA is nearly zero.
      IF((ABS(P(3)).LE.TOL).AND.(ZETA(1).GT.180.)) ZETA(1)=ZETA(1)-180.
      IF((ABS(T(3)).LE.TOL).AND.(ZETA(2).GT.180.)) ZETA(2)=ZETA(2)-180.
      IF((ABS(B(3)).LE.TOL).AND.(ZETA(3).GT.180.)) ZETA(3)=ZETA(3)-180.
      RETURN
      END
C
C
C
      SUBROUTINE PATHDEF(TYPE,NLAYER,NLAYSR,VTOP,MANTOP,LEGDUP)
C
C   Subroutine to calculate the number of P wave traverses through each
C    layer of a crustal model.  The crustal model may have vertical gradients.
C
C   Inputs:  TYPE - Name of ray path, i.e. pPmPmP
C            NLAYSR - Number of layer immediately below source
C            VTOP(J) - Velocity at the top of layer J
C            NLAYER - Number of layers for velocity model
C
C   Outputs: MANTOP - Index of first mantle layer (i.e. layer below Moho)
C            LEGDUP(J) - Number of traverses through layer J
C
      CHARACTER*(*) TYPE
      DIMENSION VTOP(*), LEGDUP(*)
C***Find first mantle layer
      DO 1 J=1, NLAYER+1
      IF (VTOP(J).GE.8.0) GO TO 2
1     CONTINUE
2     MANTOP = J
C***Define number of times ray traverses model above and below source
      IF (TYPE.EQ.'P') THEN
           DUPOVSR=1      ! Number of traverses above source
           DUPBLSR=0      ! Number of traverses below source
        ELSE IF (TYPE.EQ.'PmP') THEN
           DUPOVSR=1
           DUPBLSR=2
        ELSE IF (TYPE.EQ.'pPmP') THEN
           DUPOVSR=3
           DUPBLSR=2
        ELSE IF (TYPE.EQ.'PmPmP') THEN
           DUPOVSR=3
           DUPBLSR=4
      ENDIF
C***Now set up only for case of source above Moho
      IF (NLAYSR.EQ.1) GO TO 101
      DO 10 J=1, NLAYSR-1
10    LEGDUP(J) = DUPOVSR
101   IF (NLAYSR.GE.(MANTOP-1)) GO TO 102
      DO 11 J=NLAYSR, MANTOP-1
11    LEGDUP(J) = DUPBLSR
102   RETURN
      END
C
C
      SUBROUTINE GETMODEL (FNAME,VTOP,VBOT,TH,GR,DEPTOP,NLAYER)
C
C   Subroutine to set up the crustal model.  The model is read in as a
C    velocity vs depth function assuming a constant gradient for the velocity
C    between the depths specified.  A discontinuity in velocity is entered as
C    two velocities at the same depth, with the first being above the inter-
C    face and the second being below the interface.
C
C   The output of the subroutine is the crustal model parameters for each
C    layer.
C
C   Input:   FNAME - Name of file with velocity-depth function (must be
C                    specified from the surface downward).
C
C   Outputs: VTOP(J) - Velocity at top of layer J
C            VBOT(J) - Velocity at bottom of layer J
C            TH(J) - Thickness of layer J
C            GR(J) - Gradient in layer J
C            DEPTOP(J) - Depth to top of layer J
C            NLAYER - Number of layers in model.  Note: halfspace is NLAYER
C
      CHARACTER*(*) FNAME
      DIMENSION VTOP(*), VBOT(*), TH(*), GR(*), DEPTOP(*)
      DIMENSION H(100), V(100)
      LMAX = 100
C***LMAX is the maximum number of vel-depth pairs that can be read in.  If
C     more than 100 values are desired, change the values in the preceding
C     two lines and recompile.
C
C***Open and read the file with the velocity-depth function (2F10.4 format).
C
      OPEN(UNIT=19,FILE=FNAME,STATUS='OLD',ERR=999)
      READ(19,1,END=10)(H(I),V(I),I=1,LMAX)
1     FORMAT(2F10.4)
10    CLOSE(19)
      NVEL=I-1
C***Get the thickness and gradient of each layer, and set up the output
C     parameters
      J = 0
      DO 11 I=1, NVEL-1
      IF (H(I).EQ.H(I+1)) GO TO 11
         J = J + 1
         TH(J) = H(I+1) - H(I)
         VTOP(J) = V(I)
         VBOT(J) = V(I+1)
         GR(J) = (VBOT(J) - VTOP(J)) / TH(J)
         DEPTOP(J) = H(I)
11    CONTINUE
      NLAYER = J
      VTOP(J+1) = V(NVEL)
      RETURN
999   WRITE(6,998) FNAME
998   FORMAT(' FILE ',A,' NOT FOUND, PROGRAM WILL STOP!')
      STOP
      END
C
C
C
      SUBROUTINE SRCELAYR(VTOP,VBOT,TH,GR,DEPTOP,NLAYERS,SRCEDEP,
     *LAYBELSR)
C
C   Subroutine to put a source into a crustal model.  An interface is
C    constructed at the source depth, and the layer in which the source
C    occurs is divided into two separate layers.
C
C   Inputs:  VTOP(J) - Velocity at the top of layer J
C            VBOT(J) - Velocity at the bottom of layer J
C            TH(J) - Thickness of layer J
C            GR(J) - Velocity gradient in layer J
C            DEPTOP(J) - Depth to the top of layer J
C            NLAYERS - Number of layers in the crustal model
C            SRCEDEP - Source Depth
C
C   Outputs: LAYBELSR - Index of layer number immediately below source
C            Recomputed arrays VTOP, VBOT, TH, GR, DEPTOP, and
C            recomputed number of layers NLAYERS
      DIMENSION VTOP(*), VBOT(*), TH(*), GR(*), DEPTOP(*)
C***Find layer is which source lies
      DO 1 J=1, NLAYERS
1     IF (SRCEDEP.LT.DEPTOP(J)) GO TO 2
2     NLAYSR = J - 1
C***Increase all layer indices by 1, thus creating space for the new layer
      VTOP(NLAYERS+2) = VTOP(NLAYERS+1)
      DO 3 J=NLAYERS, NLAYSR+1, -1
          VTOP(J+1) = VTOP(J)
          VBOT(J+1) = VBOT(J)
          TH(J+1) = TH(J)
          GR(J+1) = GR(J)
          DEPTOP(J+1) = DEPTOP(J)
3     CONTINUE
C***Divide up layer with source into 2 layers by first putting in the new
C    interface and constructing the layer above the source, and then con-
C    structing the layer below the source.
      TH(NLAYSR) = SRCEDEP - DEPTOP(NLAYSR)
      VBOT(NLAYSR+1) = VBOT(NLAYSR)
      VBOT(NLAYSR) = VTOP(NLAYSR) + GR(NLAYSR) * TH(NLAYSR)
C***The layer above the source has been constructed, now create the one
C    below the source
      TH(NLAYSR+1) = DEPTOP(NLAYSR+1) - SRCEDEP
      VTOP(NLAYSR+1) = VBOT(NLAYSR)
      GR(NLAYSR+1) = GR(NLAYSR)
      DEPTOP(NLAYSR+1) = SRCEDEP
      NLAYERS = NLAYERS + 1
      LAYBELSR = NLAYSR + 1
      RETURN
      END
C
C
C
      SUBROUTINE XANDT (P,LEGDUPP,LEGDUPS,VTOPP,VBOTP,GRP,VTOPS,VBOTS,
     *GRS,TH,NLAYERS,X,T)
C
C   Subroutine to compute the travel time and epicentral distance of a ray
C    given a crustal model and ray parameter.  The earth model is with planar
C    layers.
C
C   Inputs:  P - Ray parameter for ray
C            VTOPP(J) - P velocity at the top of layer J
C            VTOPS(J) - S velocity at the top of layer J
C            VBOTP(J) - P velocity at the bottom of layer J
C            VBOTS(J) - S velocity at the bottom of layer J
C            GRP(J) - P velocity gradient in layer J
C            GRS(J) - S velocity gradient in layer J
C            TH(J) - Thickness of layer J
C            LEGDUPP(J) - Number of P ray path traverses in layer J
C            LEGDUPS(J) - Number of S ray path traverses in layer J
C            NLAYERS - Number of layers in crustal model
C
C   OUTPUTS: X - Epicentral distance for ray
C            T - Travel time for ray
C
      DIMENSION VTOPP(*), VTOPS(*), VBOTP(*), VBOTS(*), GRP(*), GRS(*)
      DIMENSION LEGDUPP(*), LEGDUPS(*), TH(*)
C***Find X and T
      X=0.
      T=0.
      DO 1 J=1, NLAYERS
C
C***P wave legs
C
      IF (LEGDUPP(J).EQ.0) THEN
             XP = 0.
             TP = 0.
             GO TO 10
      ENDIF
C***No gradient section
      IF (GRP(J).EQ.0.) THEN
            XPDEN = 1./(P*P) - VTOPP(J) * VTOPP(J)
            XP = LEGDUPP(J) * VTOPP(J) * TH(J) / SQRT(XPDEN)
            TP = LEGDUPP(J) * TH(J) / (P * VTOPP(J) * SQRT(XPDEN))
C***Gradient section
        ELSE IF (P.LE.(1./VBOTP(J))) THEN  ! Ray turns below this layer
            XP2 = 1./(P*P) - VBOTP(J) * VBOTP(J)
            IF (XP2.EQ.0.) XP2 = 1.E-20  ! Ray turns at layer bottom
            XP1 = 1./(P*P) - VTOPP(J) * VTOPP(J)
            XP2 = SQRT(XP2)
            XP1 = SQRT(XP1)
            XP = LEGDUPP(J) * (XP1 - XP2) / GRP(J)
            XP2 = (1./P + XP2) / VBOTP(J)
            XP1 = (1./P + XP1) / VTOPP(J)
            XP2 = ALOG(XP2)
            XP1 = ALOG(XP1)
            TP = LEGDUPP(J) * (XP1 - XP2) / GRP(J)
         ELSE IF (P.LE.(1./VTOPP(J))) THEN ! Ray turns in layer
            XP1 = 1./(P*P) - VTOPP(J) * VTOPP(J)
            IF (XP1.EQ.0.) XP1 = 1.E-20
            XP1 = SQRT(XP1)
            XP = LEGDUPP(J) * XP1 / GRP(J)
            XP1 = (1./P + XP1) / VTOPP(J)
            XP1 = ALOG(XP1)
            TP = LEGDUPP(J) * XP1 / GRP(J)
         ELSE   ! Ray turns above this layer
            XP = 0.
            TP = 0.
      ENDIF
C
C***S wave legs
C
10    IF (LEGDUPS(J).EQ.0) THEN
            XS = 0.
            TS = 0.
            GO TO 11
      ENDIF
C***No gradient case
      IF (GRS(J).EQ.0.) THEN
            XSDEN = 1./(P*P) - VTOPS(J) * VTOPS(J)
            XS = LEGDUPS(J) * VTOPS(J) * TH(J) / SQRT(XSDEN)
            TS = LEGDUPS(J) * TH(J) / (P * VTOPS(J) * SQRT(XSDEN))
C***Gradient case
        ELSE IF (P.LE.(1./VBOTS(J))) THEN ! Ray turns below layer
            XS1 = 1./(P*P) - VTOPS(J) * VTOPS(J)
            XS2 = 1./(P*P) - VBOTS(J) * VBOTS(J)
            IF (XS2.EQ.0.) XS2 = 1.E-20  ! Ray turns at bottom of layer
            XS1 = SQRT(XS1)
            XS2 = SQRT(XS2)
            XS = LEGDUPS(J) * (XS1 - XS2) / GRS(J)
            XS2 = (1./P + XS2) / VBOTS(J)
            XS1 = (1./P + XS1) / VTOPS(J)
            XS2 = ALOG(XS2)
            XS1 = ALOG(XS1)
            TS = LEGDUPS(J) * (XS1 - XS2) / GRS(J)
         ELSE IF (P.LE.(1./VTOPS(J))) THEN ! Ray turns in layer
            XS1 = 1./(P*P) - VTOPS(J) * VTOPS(J)
            IF (XS1.EQ.0.) XS1 = 1.E-20
            XS1 = SQRT(XS1)
            XS = LEGDUPS(J) * XS1 / GRS(J)
            XS1 = (1./P + XS1) / VTOPS(J)
            XS1 = ALOG(XS1)
            TS = LEGDUPS(J) * XS1 / GRS(J)
         ELSE   ! Ray turns above this layer
            XS = 0.
            TS = 0.
      ENDIF
11    X = X + XP + XS
      T = T + TP + TS
1     CONTINUE
      RETURN
      END
C
C
      SUBROUTINE INSRT(X,NX,XIN,INDEX,IORDER)
C
C  Subroutine to insert a new value into its proper place in an array
C   of monotonically increasing or decreasing values.
C
C  Inputs:  X - Array of values
C           NX - Number of values of array X
C           XIN - New value to be inserted into array X
C           IORDER - Any positive integer indicates array X contains
C                    progressively increasing values, while any negative
C                    integer indicates progressively decreasing values.
C                    If IORDER=0, XIN is not inserted and X and NX are
C                    returned unchanged.
C
C  Outputs: X - New array of values with XIN inserted in proper place
C           NX - New number of array values of X
C           INDEX - Index of array element where XIN was inserted.  This
C                   is set to 0 if IORDER=0.
C
      DIMENSION X(*)
C
      IF (IORDER.EQ.0) THEN
         INDEX=0
         RETURN
      ENDIF
C
      IF (IORDER.GT.0) THEN
C***Array has increasing values
          DO 1 INDEX=1, NX-1
            IF (XIN.EQ.X(INDEX)) RETURN  ! XIN is already in list
1         IF (XIN.LT.X(INDEX)) GO TO 2
C***Section to insert XIN at end of X array
      IF (XIN.EQ.X(NX)) THEN
          INDEX = NX
          RETURN
      END IF
          IF (XIN.LT.X(NX)) THEN
             X(NX+1) = X(NX)
             X(NX) = XIN
             INDEX = NX
             NX = NX + 1
             RETURN
          ELSE
             X(NX+1) = XIN
             NX = NX + 1
             INDEX = NX
             RETURN
          ENDIF
C***Section to insert XIN in the middle of X array
2         DO 3 J=NX, INDEX, -1
3         X(J+1) = X(J)
          X(INDEX) = XIN
          NX = NX + 1
          RETURN
      ELSE
C***Section for decreasing values in X
          DO 11 INDEX=1, NX-1
            IF (XIN.EQ.X(INDEX)) RETURN  ! XIN is already in list
11        IF (XIN.GT.X(INDEX)) GO TO 12
C***Section to insert XIN at end of X array
      IF (XIN.EQ.X(NX)) THEN
         INDEX = NX
         RETURN
      END IF
          IF (XIN.GT.X(NX)) THEN
             X(NX+1) = X(NX)
             X(NX) = XIN
             INDEX = NX
             NX = NX + 1
             RETURN
           ELSE
             X(NX+1) = XIN
             NX = NX + 1
             INDEX = NX
           RETURN
          ENDIF
C***Section to insert XIN in the middle of X array
12        DO 13 J=NX, INDEX, -1
13        X(J+1) = X(J)
          X(INDEX) = XIN
          NX = NX + 1
          RETURN
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE NEWTTME(X,XTEST,LEGDUPP,LEGDUPS,VTOPP,VBOTP,GRP,
     *VTOPS,VBOTS,GRS,TH,NLAYER,NLAYSR,MANTOP,XMAX,T,P,NITER)
C
C  Subroutine to iterate into the proper ray parameter and travel time
C   for a ray given a known epicentral distance and crustal model. The
C   Newton Divided Difference interpolation scheme is used in the itera-
C   tion.  If ray does not exist, T and P are set to -99.
C
C   Subroutine is set up only for direct or Moho reflected phases.
C
C
C  Inputs:  X - Value of epicentral distance for which the ray parameter
C               and travel time are being sought
C           XTEST - When the subroutine finds a ray parameter and travel
C                   time for a ray in the range of X-XTEST and X+XTEST,
C                   or when it has performed 20 iterations, the search
C                   islterminated and the values for T, P and NITER are
C                   returned.
C           VTOPP(J) and VTOPS(J) - P and S velocities respectively at the
C                                   top of layer J
C           VBOTP(J) and VBOTS(J) - P and S velocities respectively at the
C                                   bottom of layer J
C           GRP(J) and GRS(J) - P and S wave velocity gradients respectively
C                               in layer J
C           TH(J) - Thickness of layer J
C           LEGDUPP(J) and LEGDUPS(J) - Number of traverses of P and S waves
C                                       in layer J for ray
C           NLAYER - Number of layers in crustal model
C           NLAYSR - Index of layer immediately below source
C           MANTOP - Index of first mantle layer
C
C  Outputs: T - Travel time for ray
C           P - Ray parameter for ray
C           XMAX - Maximum distance at which ray can be seen
C           NITER - Number of iterations needed to find T and P
C
      DIMENSION VTOPP(*), VTOPS(*), VBOTP(*), VBOTS(*), GRP(*), GRS(*)
      DIMENSION LEGDUPP(*), LEGDUPS(*), TH(*)
      DIMENSION XTEMP(12), PTEMP(12)
      MAXTRMN = 10
C***Set up for a maximum 10th order polynomial to be used in the interpolation
C    at present.  If other values are desired, increment the preceding two
C    lines accordingly and recompile.  Remember to check NEWTDIF to be sure
C    it can handle the order of the polynomial being specified here.
C
C***Find maximum distance ray can be seen
      XMAX = 99999.
      PMAX = 99999.
      IF ((LEGDUPP(NLAYSR).NE.0).OR.(LEGDUPS(NLAYSR).NE.0)) THEN
C***Downgoing ray case
         IF (GRP(MANTOP-1).GT.0.) THEN  ! Ray can turn in bottom layer
            PMAX = 1. / VBOTP(MANTOP-1)  ! Maximum ray parameter
            CALL XANDT(PMAX,LEGDUPP,LEGDUPS,VTOPP,VBOTP,GRP,VTOPS,VBOTS,
     *      GRS,TH,NLAYER,XMAX,TMAX)
            IF (X.GT.XMAX) THEN   ! Ray does not exist
               NITER = 0
               T = -99.
               P = -99.
               RETURN
            ENDIF
            IF((ABS(XMAX-X)).LT.XTEST) THEN ! Stumbled on correct value
               P = PMAX
               T = TMAX
               NITER = 0
               RETURN
            ENDIF
         ELSE
C***Find smallest P velocity along path
             VMAXP = 0.0
             DO 5 JJ=1, MANTOP-1
             IF (VBOTP(JJ).GT.VMAXP) VMAXP = VBOTP(JJ)
5            IF (VTOPP(JJ).GT.VMAXP) VMAXP = VTOPP(JJ)
             PMAX = .99 / VMAXP  ! Largest ray parameter allowed
         ENDIF
      ELSE
C***Direct ray case
        IF(LEGDUPP(NLAYSR-1).NE.0) THEN
         PMAX = .999999 / VBOTP(NLAYSR-1) ! PMAX for direct ray
        ELSE
         PMAX = .999999 / VBOTS(NLAYSR-1) ! PMAX for direct ray
        END IF
      ENDIF
C
C  Starting values for the iteration.  Can be reset if desired.
C
C***First starting value
      PTEMP(1) = .0001  ! Smallest ray parameter expected
      PP=PTEMP(1)    !  Needed for HP EMA statement
      CALL XANDT(PP,LEGDUPP,LEGDUPS, VTOPP,VBOTP,GRP,VTOPS,
     *VBOTS,GRS,TH,NLAYER,XX,T)
      XTEMP(1)=XX   !  Needed for HP EMA statement
      IF ((ABS(X-XTEMP(1))).LT.XTEST) THEN
         P = PTEMP(1)
         NITER = 0
         RETURN
      ENDIF
C***Second starting value
      IF((LEGDUPP(NLAYSR).GT.0).OR.(LEGDUPS(NLAYSR).GT.0)) THEN
            PTEMP(2) = 1. / VTOPP(MANTOP)  ! Critical ray parameter
        ELSE
           IF(LEGDUPP(NLAYSR-1).NE.0) THEN
            PTEMP(2) = .995 / VBOTP(NLAYSR-1)  ! Starting value for direct ray
           ELSE
            PTEMP(2) = .995 / VBOTS(NLAYSR-1)  ! Starting value for direct ray
           END IF
      ENDIF
      PP=PTEMP(2)    !  Needed for HP EMA statement
      CALL XANDT(PP,LEGDUPP,LEGDUPS,VTOPP,VBOTP,GRP,VTOPS,
     *VBOTS,GRS,TH,NLAYER,XX,T)
      XTEMP(2)=XX   !  Needed for HP EMA statement
      IF((ABS(X-XTEMP(2))).LT.XTEST) THEN
         P = PTEMP(2)
         NITER = 0
         RETURN
      ENDIF
      NTERMS = 2
C***If ray is past critical angle at the Moho or if a direct ray
C    add a third starting value
      IF ((LEGDUPP(NLAYSR).GT.0).OR.(LEGDUPS(NLAYSR).GT.0)) THEN
            IF ((X.GT.XTEMP(2)).AND.(GRP(MANTOP-1).GT.0.)) THEN
               PTEMP(3) = PMAX
               XTEMP(3) = XMAX
               NTERMS = 3
            ENDIF
         ELSE
           PTEMP(3) = .999 / VBOTP(NLAYSR-1)
           PP=PTEMP(3)   !  Needed for HP EMA statement
           CALL XANDT(PP,LEGDUPP,LEGDUPS,VTOPP,VBOTP,GRP,
     *     VTOPS,VBOTS,GRS,TH,NLAYER,XX,T)
           XTEMP(3)=XX   !  Needed for HP EMA statement
           IF ((ABS(X-XTEMP(3))).LT.XTEST) THEN
              P = PTEMP(3)
              NITER = 0
              RETURN
           ENDIF
           NTERMS = 3
      ENDIF
C***
C  I'M NOT SURE IF THE NEXT SECTION OF CODE IS NEEDED OR NOT
C
C      IF(PTEMP(3).LT.PTEMP(2)) THEN
C         STOREP=PTEMP(3)
C         STOREX=XTEMP(3)
C         PTEMP(3)=PTEMP(2)
C         XTEMP(3)=XTEMP(2)
C         PTEMP(2)=STOREP
C         XTEMP(2)=STOREX
C      END IF
C***
C***Perform the iterations
      IORDER = 1  ! XTEMP contains increasing values of distance
      DO 1 JJ=1, 20
C
      NITER = JJ
      CALL NEWTDIF(NTERMS, XTEMP, PTEMP, X, P)
C***If ray parameter falls outside range of possible values, then bump
C     back into the appropriate range.  This is to keep the program from
C     wandering onto the wrong branch of the travel time curve or into unreal
C     values of the ray parameter.
C
C
      IF (P.LT.0.) P = -P
      IF (P.GT.PMAX) THEN
         DO 21 J=2, NTERMS
         IF (X.LT.XTEMP(J)) THEN
             P = (PTEMP(J) + PTEMP(J-1)) / 2. ! Average appropriate values
             GO TO 22
         ENDIF
21       CONTINUE
         P = (PMAX + PTEMP(NTERMS)) /2.
22       CONTINUE
      ENDIF
C
C
      CALL XANDT(P,LEGDUPP,LEGDUPS,VTOPP,VBOTP,GRP,VTOPS,VBOTS,
     *GRS,TH,NLAYER,XOUT,T)
C***Test the result
      IF ((ABS(X-XOUT)).LT.XTEST) GO TO 99
C***Result not good enough, update the arrays of known values and try again
      CALL INSRT(XTEMP,NTERMS,XOUT,INDEX,IORDER)
      IF (INDEX.EQ.NTERMS) THEN
          PTEMP(NTERMS) = P
        ELSE
          DO 10 J=NTERMS, INDEX, -1
10        PTEMP(J+1) = PTEMP(J)
          PTEMP(INDEX) = P
      ENDIF
C***Test end points of XTEMP and throw out one most different from X
      IF (NTERMS.GT.MAXTRMN) THEN
         IF((ABS(XTEMP(1)-X)).GT.(ABS(XTEMP(NTERMS)-X))) THEN
            IF (INDEX.EQ.NTERMS) THEN   !Scheme not converging
              NITER = 99
              GO TO 99
            END IF
            DO 11 J=1, NTERMS-1
            XTEMP(J) = XTEMP(J+1)
11          PTEMP(J) = PTEMP(J+1)
         ENDIF
      NTERMS = MAXTRMN
      ENDIF
1     CONTINUE
99    RETURN
      END
C
C
C
      SUBROUTINE MINSN(LX,X,XMIN,INDEX)
      DIMENSION X(LX)
      INDEX=1
      DO 1 I=1,LX
      IF(X(INDEX).GT.X(I))INDEX=I
1     CONTINUE
      XMIN=X(INDEX)
      RETURN
      END
C
C
C
      SUBROUTINE MAXSN(LX,X,XMAX,INDEX)
      DIMENSION X(LX)
      INDEX=1
      DO 1 I=1,LX
      IF(X(INDEX).LT.X(I)) INDEX=I
1     CONTINUE
      XMAX=X(INDEX)
      RETURN
      END
C
C
C
      SUBROUTINE MAXABS(LX,X,XABS,INDEX)
      DIMENSION X(LX)
      INDEX=1
      DO 1 I=1,LX
      IF(ABS(X(I)).GT.ABS(X(INDEX))) INDEX=I
1     CONTINUE
      XABS=ABS(X(INDEX))
      RETURN
      END
C
C
C
      SUBROUTINE MOVE(LX,X,Y)
      DIMENSION X(LX),Y(LX)
      DO 1 I=1,LX
1     Y(I)=X(I)
      RETURN
      END
C
C
C
      SUBROUTINE RECEVH(MR,P,NTYPE,VP,VS,RCV)
C
C  Subroutine to compute the receiver functions (real values) for a receiver
C    on a half space.
C
C  Inputs:  MR - =3 for incident SV wave; =4 for incident SH wave; =5 for
C                   incident P wave
C           P - Ray parameter for ray
C           NTYPE - =1 for vertical component; =2 for radial component; =3
C                   for tangential component
C           VP - P velocity for surface layer
C           VS - S velocity for surface layer
C
C  Output:  RCV - Value of receiver function
C
      RCV=0.
      IF (MR.EQ.4) GO TO 50
      EA = CR(P,VP)
      EB = CR(P,VS)
C***Check to see if past a critical angle
      IF(EA.EQ.0.0.OR.EB.EQ.0.0) THEN
        IF(P.NE.0.0) THEN
C***Yes, we are past a critical angle.  Pass a dummy value back through RCV
           RCV=-99999.
           RETURN
        END IF
      END IF
      RP = (EB*EB - P*P)**2 + 4.*P*P*EB*EA
      RPB = RP*VS*VS
      IF (MR.EQ.3) GO TO 10
C***Compute receiver function for P waves
      IF (NTYPE.EQ.1) THEN
        SR = 2.*EA*(EB*EB - P*P)
       ELSE IF (NTYPE.EQ.2) THEN
        SR = -4.*EA*EB*P
       ELSE IF (NTYPE.EQ.3) THEN
        SR = 4.*EA*EB
       ELSE
        SR = 0.
      END IF
      RCV = SR / RPB
      RETURN
10    CONTINUE
C***Compute receiver function for SV waves
      IF (NTYPE.EQ.1) THEN
        SR = 4.*EA*EB*P
       ELSE IF (NTYPE.EQ.2) THEN
        SR = 2.*EB*(EB*EB - P*P)
       ELSE IF (NTYPE.EQ.3) THEN
        SR = -2.*EB*(EB*EB - P*P)/P
       ELSE
        SR = 0.
      END IF
      RCV = SR / RPB
      RETURN
50    CONTINUE
C***Compute receiver function for SH waves
      IF (NTYPE.EQ.2) THEN
        RCV = 2.
       ELSE IF (NTYPE.EQ.3) THEN
        RCV = 2.*P
       ELSE
        RCV = 0.
      END IF
      RETURN
      END
C
C
C
      FUNCTION CR(P,V)
C
C  Function to calculate ETA function (vertical slowness) of Helmberger
C    notation.
C
C  Inputs:  P - Ray parameter of ray
C           V - Velocity of ray
C
C  Output:  CR - SQRT(1/V**2 - P**2)
C
C  Note: this is a real valued version
C
      CR2 = 1./(V*V) - P*P
      IF (CR2.LE.0.) THEN
        CR = 0.
        RETURN
      END IF
      CR = SQRT(CR2)
      RETURN
      END
C
C
C
      SUBROUTINE DISPS(HMT,M,D)
C
C  Subroutine to compute a particular component of displacement from Greens
C    functions and moment tensor.
C
C  Inputs:  HMT(I) - Six-vector of Green's functions
C           M(I) - Six-vector of moment tensor elements, where M(I)
C                corresponds to HMT(I)
C
C  Output: D - Displacement from sum of Green's functions times moment tensor
C               elements
C
      REAL M(6),HMT(6),D
      D = 0.
      DO 1 J=1,6
1     D = D + HMT(J)*M(J)
      RETURN
      END
C
C
C
      SUBROUTINE FILDAT(NMAX,HEAD,NSTAT,SN,COMP,PS,R,AZ,AMP,IND,IUNIT)
C
C  Subroutine to read in and write out data for phase amplitude inversion
C    for moment tensor components.
C
C  Inputs:  NMAX - Maximum number of ampliutde readings allowed
C           HEAD - Header line describing event
C           NSTAT - Number of amplitude readings
C           SN(I) - Name of station for observation I
C           COMP(I) - Z, R, or T component of motion for observation I
C           PS(I) - Phase type (P, SV, or SH) for observation I
C           R(I) - Epicentral distance to station SN(I)
C           AZ(I) - Azimuth from north (in degrees) for station SN(I)
C           AMP(I) - Amplitude of ground motion in microns for station SN(I)
C           IND - is NEGATIVE if data is to be READ from the file
C                 is POSITIVE if data is to be WRITTEN to the file
C           IUNIT - Unit number to be assigned to file being opened for
C                     reading or writing
C
C  Output:  If IND is negative, NSTAT, SN, COMP, PS, R, AZ and AMP are all
C             read from the file and output from the subroutine.
C           If IND is positive, there is no output.
C
      CHARACTER*(*) SN(NMAX),PS(NMAX),COMP(NMAX),HEAD
      CHARACTER*40 FNAME
      DIMENSION R(NMAX),AZ(NMAX),AMP(NMAX)
1     FORMAT(A)
      IF (IND.GT.0) THEN
          IF (NSTAT.GT.NMAX) THEN
            WRITE(6,*)' Dimension of NMAX is too small, exit FILDAT'
            RETURN
          END IF
          WRITE(6,*)' Give name of FILE for WRITING DATA:'
          READ(5,1)FNAME
          OPEN(IUNIT,FILE=FNAME,STATUS='NEW',ERR=999)
          WRITE(IUNIT,1) HEAD
          WRITE(IUNIT,101) NSTAT
101   FORMAT(I5)
          DO 10 J=1,NSTAT
10        WRITE(IUNIT,102) SN(J),COMP(J),PS(J),R(J),AZ(J),AMP(J)
102   FORMAT(1X,A,1X,A,1X,A,1X,F6.2,4X,F7.2,3X,E12.4)
          CLOSE(IUNIT)
          RETURN
        ELSE
cjh          WRITE(6,*)' Give name of FILE for READING DATA:'
cjh          READ(5,1)FNAME
          fname='invrad.inp'
          OPEN(IUNIT,FILE=FNAME,STATUS='OLD',ERR=999)
          READ(IUNIT,1) HEAD
          READ(IUNIT,101) NSTAT
          IF (NSTAT.GT.NMAX) THEN
            CLOSE(IUNIT)
            NSTAT=0
            WRITE(6,*)' Dimension of NMAX is too small, exit FILDAT'
            RETURN
          END IF
          DO 20 J=1,NSTAT
20        READ(IUNIT,102)SN(J),COMP(J),PS(J),R(J),AZ(J),AMP(J)
          CLOSE(IUNIT)
          RETURN
      END IF
999   WRITE(6,902)
902   FORMAT('Error opening file in FILDAT, exit subroutine')
      RETURN
      END
C
C
C
      SUBROUTINE SCRDAT(NMAX,HEAD,NSTAT,SN,COMP,PS,R,AZ,AMP,IUNIT)
C
C  Subroutine to get amplitude data from screen and write to file for moment
C    tensor inversion program.
C
C  Inputs: NMAX - Maximum number of observations allowed
C          IUNIT - Unit number of file to be opened for storing data
C
C Outputs: HEAD - Header line describing event
C          NSTAT - Number of observations
C          SN(J) - Name of station for observation J
C          COMP(J) - Z, R, or T component of motion for observation J
C          PS(J) - P, SV or SH type of wave for observation J
C          R(J) - Epicentral distance to station SN(J)
C          AZ(J) - Azimuth to station SN(J)
C          AMP(J) - Amplitude observation in microns for station SN(J)
C
      DIMENSION R(NMAX),AZ(NMAX),AMP(NMAX)
      CHARACTER*(*) SN(NMAX),PS(NMAX),COMP(NMAX),HEAD
      CHARACTER*1 ANS
1     FORMAT(A)
      WRITE(6,*)' Give EVENT HEADER:'
      READ(5,1) HEAD
      DO 10 J=1,NMAX
      WRITE(6,*)' Give STA NAME, COMP(Z, R, or T), and WAVE TYPE (P, SV,
     * or SH)'
      WRITE(6,*)' Enter blanks to quit'
      WRITE(6,*)' Reenter STA NAME, COMP and WAVE TYPE to replace old or
     *incorrect data'
      WRITE(6,*)' Separate each entry by a space (STA NAME is 4 chars)'
      READ(5,2) SN(J),COMP(J),PS(J)
2     FORMAT(A,1X,A,1X,A)
      IF (SN(J).EQ.'    ') GO TO 900
      WRITE(6,*)' Give DIST, AZ, and AMP'
      READ(5,*) R(J),AZ(J),AMP(J)
      IF (J.GT.1) THEN
        DO 11 I=1,J-1
        IF (SN(J).EQ.SN(I).AND.COMP(J).EQ.COMP(I).AND.PS(J).EQ.PS(I))
     *   THEN
            R(I)=R(J)
            AZ(I)=AZ(J)
            AMP(I)=AMP(J)
c           J=J-1 ******masked 
            GO TO 10
        END IF
11      CONTINUE
      END IF
10    CONTINUE
900   IF(J.NE.NMAX+1) NSTAT=J-1
899   WRITE(6,*)' # STA C PS  DIST     AZ      AMP'
      DO 21 J=1,NSTAT
21    WRITE(6,201)J,SN(J),COMP(J),PS(J),R(J),AZ(J),AMP(J)
201   FORMAT(I2,1X,A,1X,A,1X,A,1X,F6.2,1X,F7.2,1X,E12.5)
      WRITE(6,*)' DELETE any data (Y or N)?'
      READ(5,1)ANS
      IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
        WRITE(6,*)' Give # of data to be deleted:'
        READ(5,*) ND
        IF(ND.EQ.NSTAT) GO TO 23
        DO 22 J=ND,NSTAT-1
        SN(J)=SN(J+1)
        COMP(J)=COMP(J+1)
        PS(J)=PS(J+1)
        R(J)=R(J+1)
        AZ(J)=AZ(J+1)
22      AMP(J)=AMP(J+1)
23      NSTAT=NSTAT-1
        GO TO 899
      END IF
      CALL FILDAT(NMAX,HEAD,NSTAT,SN,COMP,PS,R,AZ,AMP,+1,IUNIT)
      RETURN
      END
C
C
C
      SUBROUTINE OUTALL(EMOM,SDEV,HEAD,NST,SN,COMP,PS,R,AZ,AMP,D,SMOM,
     * ST,DP,RK,IUNIT)
C
C  Subroutine to write out observed and calculated data from moment tensor
C    programs.
C
C  Inputs:  EMOM - Seismic moment of event, in units of 1 E16 dyne-cm
C           SDEV - Standard deviation of moment
C           HEAD - Header line describing event
C           NST - Number of amplitude readings
C           SN(I) - Name of station for observation I
C           COMP(I) - Z, R, or T component of motion for observation I
C           PS(I) - Phase type (P, SV, or SH) for observation I
C           R(I) - Epicentral distance to station SN(I)
C           AZ(I) - Azimuth from north (in degrees) for station SN(I)
C           AMP(I) - Amplitude of ground motion in microns for station SN(I)
C           D(I) - Calculated amplitude of ground motion in microns for SN(I)
C           SMOM(I) - Calculated moment in dyne-cm x E16 at station SN(I)
C           ST - Event strike
C           DP - Event dip
C           RK - Event rake
C           IUNIT - Unit number for output file
C
      CHARACTER*(*) SN(NST),COMP(NST),PS(NST),HEAD
      DIMENSION R(NST),AZ(NST),AMP(NST),D(NST),SMOM(NST)
      WRITE(IUNIT,1) HEAD
1     FORMAT(A)
      WRITE(6,1) HEAD
      WRITE(6,900)
      WRITE(IUNIT,900)
      WRITE(IUNIT,910) ST,DP,RK
      WRITE(6,910) ST,DP,RK
910   FORMAT('*** FOR STRIKE=',F7.2,'  DIP=',F6.2,'  RAKE=',F7.2,' ***')
      WRITE(6,901) EMOM,SDEV
      WRITE(IUNIT,901) EMOM,SDEV
901   FORMAT(' EVENT MOMENT=',F10.2,' +-',F10.2,' x E16 DYNE-CM')
      WRITE(6,900)
      WRITE(IUNIT,900)
900   FORMAT(' ')
      WRITE(6,902)
      WRITE(IUNIT,902)
902   FORMAT(' STA  C PS  DIST    AZ      AMP OBS      AMP CALC    MOM
     *x E16')
      DO 4 J=1,NST
      WRITE(6,903) SN(J),COMP(J),PS(J),R(J),AZ(J),AMP(J),D(J),SMOM(J)
      WRITE(IUNIT,903) SN(J),COMP(J),PS(J),R(J),AZ(J),AMP(J),D(J),
     *  SMOM(J)
903   FORMAT(1X,A,1X,A,1X,A,F6.2,1X,F7.2,1X,E12.5,1X,E12.5,1X,F10.2)
4     CONTINUE
      WRITE(IUNIT,900)
      WRITE(IUNIT,900)
      RETURN
      END
C
C
C
      SUBROUTINE OUTINV(EMOM,SDEV,HEAD,NST,SN,COMP,PS,R,AZ,AMP,D,DM0,
     * ST,DP,RK,ST1,DP1,RK1,SDST,SDDP,SDRK,ERRRMS,CLVDM,IUNIT)
C
C  Subroutine to write out observed and calculated data from moment tensor
C    inversion program.
C
C  Inputs:  EMOM - Seismic moment of event, in units of 1 E16 dyne-cm
C           SDEV - Standard deviation of moment
C           HEAD - Header line describing event
C           NST - Number of amplitude readings
C           SN(I) - Name of station for observation I
C           COMP(I) - Z, R, or T component of motion for observation I
C           PS(I) - Phase type (P, SV, or SH) for observation I
C           R(I) - Epicentral distance to station SN(I)
C           AZ(I) - Azimuth from north (in degrees) for station SN(I)
C           AMP(I) - Amplitude of ground motion in microns for station SN(I)
C           D(I) - Calculated amplitude of ground motion in microns for SN(I)
C           DM0(I) - Calc. amp. of ground motion in microns from original moment
C                    tensor solution
C           ST - Event strike
C           DP - Event dip
C           RK - Event rake
C           ST1 - Event strike (second possible plane)
C           DP1 - Event dip (second possible plane)
C           RK1 - Event rake (second possible plane)
C           SDST - Standard deviation of event strike
C           SDDP - Standard deviation of event dip
C           SDRK - Standard deviation of event rake
C           ERRRMS - RMS error of difference between calculated and observed
C                    amplitudes
C           CLVDM - Moment of compensated linear vector dipole component of
C                     moment tensor
C           IUNIT - Unit number for output file
C
      CHARACTER*(*) SN(NST),COMP(NST),PS(NST),HEAD
      DIMENSION R(NST),AZ(NST),AMP(NST),D(NST),DM0(NST)
      WRITE(IUNIT,1100)
1100  FORMAT(' Inversion results for:')
      WRITE(IUNIT,1) HEAD
1     FORMAT(A)
      WRITE(6,1) HEAD
      WRITE(6,900)
      WRITE(IUNIT,900)
      WRITE(IUNIT,910) ST,SDST,DP,SDDP,RK,SDRK
      WRITE(IUNIT,910) ST1,SDST,DP1,SDDP,RK1,SDRK
      WRITE(6,910) ST,SDST,DP,SDDP,RK,SDRK
      WRITE(6,910) ST1,SDST,DP1,SDDP,RK1,SDRK
910   FORMAT('STRIKE=',F7.2,' +-',F7.2,'  DIP=',F6.2,' +-',F6.2,'  RAKE=
     *',F7.2,' +-',F7.2)
      WRITE(6,901) EMOM,SDEV
      WRITE(IUNIT,901) EMOM,SDEV
901   FORMAT(' EVENT MOMENT=',F10.2,' +-',F10.2,' x E16 DYNE-CM')
      WRITE(IUNIT,921) CLVDM
      WRITE(6,921) CLVDM
921   FORMAT('    CLVD MOMENT FROM MOMENT TENSOR=',F10.2,' x E16 DYNE-CM
     *')
      WRITE(6,900)
      WRITE(IUNIT,900)
900   FORMAT(' ')
      WRITE(6,902)
      WRITE(IUNIT,902)
902   FORMAT(' STA  C PS  DIST    AZ      AMP OBS    MT AMP CALC
     *  DC AMP CALC')
      DO 4 J=1,NST
      WRITE(IUNIT,903) SN(J),COMP(J),PS(J),R(J),AZ(J),AMP(J),
     *DM0(J),D(J)
      WRITE(6,903) SN(J),COMP(J),PS(J),R(J),AZ(J),AMP(J),DM0(J),D(J)
903   FORMAT(1X,A,1X,A,1X,A,F6.2,1X,F7.2,1X,E12.5,1X,E12.5,1X,E12.5)
4     CONTINUE
C***Find the RMS error of the expected compared to the observed amplitudes
C   for the moment tensor result.
      RMSMT=0.
      DO 505 J=1,NST
505   RMSMT=RMSMT+(DM0(J)-AMP(J))*(DM0(J)-AMP(J))
      RMSMT=SQRT(RMSMT/NST)
C      WRITE(IUNIT,900)
      WRITE(6,900)
      WRITE(6,1102)RMSMT
      WRITE(IUNIT,1102)RMSMT
1102  FORMAT('  MT RMS error=',E12.5,' microns')
      WRITE(6,900)
      WRITE(6,1101)ERRRMS
      WRITE(IUNIT,1101) ERRRMS
1101  FORMAT('  DC RMS error=',E12.5,' microns')
C      WRITE(IUNIT,900)
      RETURN
      END
C
C
C
      SUBROUTINE SDRM0SD(SDDM,M0,ST,DP,RK,SDM0,SDST,SDDP,SDRK)
C  Subroutine to calculate the standard deviations of the strike, dip, rake
C    and seismic moment given the standard deviation of the moment tensor
C    elements.  This is all calculated from Langston, BSSA, 1981, p.1,
C    formulation.
C
C  Inputs:  SDDM - Vector array of moment tensor errors, as ordered in
C                  the formulation of Langston, 1981.
C           M0 - Calculated seismic moment of event
C           ST - Calculated strike of event in degrees
C           DP - Calculated dip of event in degrees
C           RK - Calculated rake of event in degrees
C
C  Outputs: SDM0 - Standard deviation of seismic moment
C           SDST - Standard deviation in degrees of fault strike
C           SDDP - Standard deviation in degrees of fault dip
C           SDRK - Standard deviation in degrees of fault rake
C
      DIMENSION SDDM(5),VECERR(4),G(5,4),GT(4,5),GTG(4,4)
      DIMENSION GINVLS(4,5),GINV(4,4)
      REAL M0
      PIBY180=3.14159265/180.
C***Form the tensor relating the errors in the strike, dip, rake and seismic
C     moment to the errors in the moment tensor elements.
      STSIN=SIN(ST*PIBY180)
      STCOS=COS(ST*PIBY180)
      ST2SIN=SIN(2.*ST*PIBY180)
      ST2COS=COS(2.*ST*PIBY180)
      DPSIN=SIN(DP*PIBY180)
      DPCOS=COS(DP*PIBY180)
      DP2SIN=SIN(2.*DP*PIBY180)
      DP2COS=COS(2.*DP*PIBY180)
      RKSIN=SIN(RK*PIBY180)
      RKCOS=COS(RK*PIBY180)
      G(1,1)=(2.*STSIN*STCOS*RKSIN*DP2SIN+2.*ST2COS*RKCOS*DPSIN)*M0
      G(1,2)=(STSIN*STSIN*RKCOS*DP2SIN-ST2SIN*RKSIN*DPSIN)*M0
      G(1,3)=(2.*STSIN*STSIN*RKSIN*DP2COS+ST2SIN*RKCOS*DPCOS)*M0
      G(1,4)=STSIN*STSIN*RKSIN*DP2SIN+ST2SIN*RKCOS*DPSIN
      G(2,1)=(-2.*STSIN*STCOS*RKSIN*DP2SIN-2.*ST2COS*RKCOS*DPSIN)*M0
      G(2,2)=(STCOS*STCOS*RKCOS*DP2SIN+ST2SIN*RKSIN*DPSIN)*M0
      G(2,3)=(2.*STCOS*STCOS*RKSIN*DP2COS-ST2SIN*RKCOS*DPCOS)*M0
      G(2,4)=STCOS*STCOS*RKSIN*DP2SIN-ST2SIN*RKCOS*DPSIN
      G(3,1)=(-ST2COS*RKSIN*DP2COS+2.*ST2SIN*RKCOS*DPSIN)*M0
      G(3,2)=(-.5*ST2SIN*RKCOS*DP2SIN+ST2COS*RKSIN*DPSIN)*M0
      G(3,3)=(-ST2SIN*RKSIN*DP2COS-ST2COS*RKCOS*DPCOS)*M0
      G(3,4)=-.5*ST2SIN*RKSIN*DP2SIN-ST2COS*RKCOS*DPSIN
      G(4,1)=(-STSIN*RKCOS*DPCOS+STCOS*RKSIN*DP2COS)*M0
      G(4,2)=(-STCOS*RKSIN*DPCOS+STSIN*RKCOS*DP2COS)*M0
      G(4,3)=(-STCOS*RKCOS*DPSIN-2.*STSIN*RKSIN*DP2SIN)*M0
      G(4,4)=STCOS*RKCOS*DPCOS+STSIN*RKSIN*DP2COS
      G(5,1)=(STSIN*RKSIN*DP2COS+STCOS*RKCOS*DPCOS)*M0
      G(5,2)=(-STCOS*RKCOS*DP2COS-STSIN*RKSIN*DPCOS)*M0
      G(5,3)=(2.*STCOS*RKSIN*DP2SIN-STSIN*RKCOS*DPSIN)*M0
      G(5,4)=-STCOS*RKSIN*DP2COS+STSIN*RKCOS*DPCOS
C***Multiply G matrix by -1 to change from Langston to Aki&Richards formula-
C     tion
      DO 22 JJ=1,4
      DO 22 II=1,5
22    G(II,JJ)=-G(II,JJ)
C***Form G transpose and multiply into G
      CALL TRANSP(5,4,G,GT)
      CALL MATMULT(4,5,GT,4,G,GTG)
C***Form the inverse of G transpose times G and then multiply into G transpose
      CALL MAINE(4,GTG,GINV)
      CALL MATMULT(4,4,GINV,5,GT,GINVLS)
C***Get the solution vector
      CALL MATMULT(4,5,GINVLS,1,SDDM,VECERR)
C***Sort out the components and exit subroutine
      SDM0=ABS(VECERR(4))
      SDST=ABS(VECERR(1))/PIBY180
      SDDP=ABS(VECERR(3))/PIBY180
      SDRK=ABS(VECERR(2))/PIBY180
      RETURN
      END
C
C
C
      SUBROUTINE ATA(N,M,A,ATAM)
C  Subroutine to calculate the matrix product A transpose times A where
C     A is a real, second order matrix
C
C  Inputs:  A - Real matrix with N rows and M columns
C
C  Output:  ATAM - Output matrix product A transpose times A
C
      DIMENSION A(N,M),ATAM(M,M)
      CALL ZERO(M*M,ATAM)
      DO 1 J=1,M
      DO 1 I=J,M
      DO 2 K=1,N
2     ATAM(J,I)=ATAM(J,I)+A(K,J)*A(K,I)
1     ATAM(I,J)=ATAM(J,I)
      RETURN
      END
C
C
C
      SUBROUTINE CLSPCK(NMAX,MMAX,A,N,M)
C  Subroutine to close pack a two dimension array.  The purpose of it
C    is to eliminate the zeros between non-zero elements due to the use
C    of an array which is smaller than the maximum dimensions specified
C    in the dimension statement.
C
C  Inputs:  A - Array with maximum dimensions of NMAX rows and MMAX columns
C                 to be close packed in storage as an array with N rows
C                 and M columns.
C
      DIMENSION A(NMAX,MMAX)
      L=1
      K=N
      DO 1 J=2,M
      DO 1 I=1,N
      K=K+1
      IF(K.GT.NMAX) THEN
         L=L+1
         K=1
      END IF
1     A(K,L)=A(I,J)
      NLEFT=NMAX*MMAX-N*M
      CALL ZERO(NLEFT,A(K+1,L))
      RETURN
      END
C
C
C
      SUBROUTINE OPNPCK(NMAX,MMAX,A,N,M)
C  Subroutine to open pack a two dimension array.  The purpose of it
C    is to put in the zeros between non-zero elements due to the use
C    of an array which is smaller than the maximum dimensions specified
C    in the dimension statement.
C
C  Inputs:  A - Array with dimensions of N rows and M columns
C                 to be open packed in storage as an array with NMAX row
C                 and MMAX columns.
C
      DIMENSION A(NMAX,MMAX)
      L=M*N/NMAX+1
      K=MOD(N*M,NMAX)+1
      DO 1 J=M,2,-1
        DO 2 II=NMAX,N+1,-1
2       A(II,J)=0.
      DO 1 I=N,1,-1
      K=K-1
      IF(K.EQ.0) THEN
         L=L-1
         K=NMAX
      END IF
1     A(I,J)=A(K,L)
      DO 3 II=NMAX,N+1,-1
3     A(II,1)=0.
      RETURN
      END
C
C
C
      SUBROUTINE TRANSP(N,M,A,AT)
C  Subroutine to calculate the transpose of a second order real matrix
C
C  Inputs:  A - Real matrix of N rows and M columns to be transposed
C
C  Ouptuts: AT - Transpose of A
C
      DIMENSION A(N,M),AT(M,N)
      DO 1 J=1,N
      DO 1 I=1,M
1     AT(I,J)=A(J,I)
      RETURN
      END
C
C
C
      SUBROUTINE MATMULT(NA,MA,A,MB,B,C)
C  Subroutine to form the matrix product of A times B to get matrix C,
C    where A, B, and C are real matrices.
C
C  Inputs:  A - Left-hand matrix in the multiplication with NA rows
C                 and MA columns
C           B - Right-hand matrix in the multiplication with MA rows and MB
C                columns
C
C  Outputs: C - Product of A times B with NA rows and MB columns
C
      DIMENSION A(NA,MA),B(MA,MB),C(NA,MB)
      CALL ZERO(NA*MB,C)
      DO 1 J=1,NA
      DO 1 I=1,MB
      DO 2 K=1,MA
2     C(J,I)=C(J,I) + A(J,K)*B(K,I)
1     CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE GENINV(N,M,G,DAT,DM,RESM,RESD,VARM)
C  Subroutine to use a generalized inverse to calculate the solution M of
C    the vector equation D = G times M where D is a data vector, G is
C    usually a derivative matrix, and M is the model vector.
C
C  Inputs:  G - Matrix with N rows and M columns which relates D to M.
C                 G is often the derivative matrix dDi/dMj.
C           DAT - Column vector of length N of data points (observations)
C
C  Outputs: DM - Column vector of model parameters of length M found using
C                 the generalized inverse
C           RESM - Resolution matrix of M rows and M columns which shows the
C                   independence of the model parameters found in the
C                   inversion
C           RESD - Resolution matrix of N rows and N columns which shows the
C                   independence of the data parameters used in the inversion
C           VARM - Column vector of length M which contains the variance
C                  values of the model parameters.  VARM(I) corresponds to
C                  DM(I).  The subroutine assumes that the variances of all
C                  the input data are independent and equal as in Aki&
C                  Richards eq 12.107 ff.  The values of VARM are normalized
C                  to unit data variance, and only the diagonal elements of
C                  the variance matrix are found (i.e. the cross terms are
C                  not calculated).
C
C  Based on Aki&Richards section 12.3.
C
      DIMENSION G(N,M),DAT(N),DM(M),RESM(M,M),RESD(N,N),VARM(M)
      DIMENSION GTEMP(400),D(20),V(400),GINV(400),GINVP(400)
      DIMENSION U(400),UPT(400)
C***The subroutine is set up presently for maximum dimensions of 20 for N
C     and 20 for M.  If these must be changed, adjust the next two statements
C     and the corresponding dimensions in the DIMENSION statements above
C     The values in the DIMENSION statements above are either MM or MM*MM
C     where MM is the larger of NMAX or MMAX.
C     Also please change the values in this comment statement.
      NMAX=20
      MMAX=20
      IF(N.GT.NMAX.OR.M.GT.MMAX) THEN
         WRITE(6,*)' Array dimensions in GENINV too small, exit subrouti
     *ne!'
         RETURN
      END IF
C
C  Start finding the solution for the generalized inverse
C
C***Form the matrix G transpose times G
      CALL ATA(N,M,G,GTEMP)
C***Find the eigenvalues for G transpose times G.
      CALL JACOBI(M,GTEMP,D,V,NROT)
C***Look for zero eigenvalues.  If none, use Least-squares inverse.
      DO 10 J=1,M
10    IF(D(J).EQ.0.0) GO TO 500
C***All eigenvalues are non-zero, use Least-squares inverse (no damping at
C     present).
      CALL MAINE(M,GTEMP,GINV)
      CALL TRANSP(N,M,G,GTEMP)
      CALL MATMULT(M,M,GINV,N,GTEMP,GINVP)
      GO TO 900
C***The problem is underdetermined, use the generalized inverse
500   CONTINUE
      K=0
      DO 501 J=1,M
      IF(D(J).EQ.0.) GO TO 501
      K=K+1   !  Count the number of non-zero eigenvalues
      EIGEN=1./SQRT(D(J))
C***Form the Vp times LAMBDA inverse matrix
      DO 502 I=1,M
502   GTEMP((K-1)*M+I)=V((J-1)*M+I)*EIGEN
C***Find a row of the Up transpose matrix
      CALL MATMULT(N,M,G,1,V((J-1)*M+1),U((K-1)*M+1))
      CALL RESCALE(EIGEN,N,U((K-1)*M+1))
501   CONTINUE
C***Form Up transpose
      CALL TRANSP(N,K,U,UPT)
C***Form the generalized inverse matrix
      CALL MATMULT(M,K,GTEMP,N,UPT,GINVP)
900   CONTINUE
C***Get the solution vector
      CALL MATMULT(M,N,GINVP,1,DAT,DM)
C***Get the model resolution matrix
      CALL MATMULT(M,N,GINVP,M,G,RESM)
C***Get the data resolution matrix
      CALL MATMULT(N,M,G,N,GINVP,RESD)
C***Get the normalized variance of the model parameters.  These must be
C     multiplied by the variance of the observations to get the true
C     variance of the model parameters.
C     Note:  This only gets the diagonal elements of the variance matrix
C     and does not compute the cross terms.  See A&R eq. 12.107.
      DO 901 J=1,M
      VARM(J)=0.
      DO 901 I=1,N
901   VARM(J)=VARM(J)+GINVP((I-1)*M+J)*GINVP((I-1)*M+J)
      RETURN
      END
C
C
C
      SUBROUTINE NORMEN(LX,X)
      DIMENSION X(LX)
      E=0.
      DO 10 I=1,LX
10    E=E+X(I)*X(I)
      E=SQRT(E)
      DO 20 I=1,LX
20    X(I)=X(I)/E
      RETURN
      END
C
C
C
      SUBROUTINE RESCALE(S,LX,X)
      DIMENSION X(LX)
      DO 1 I=1,LX
1     X(I)=S*X(I)
      RETURN
      END
C
C
C
      SUBROUTINE JACOBI(N,A,D,V,NROT)
C
C  Subroutine to find all of the eigenvalues and eigenvectors of the
C    N by N real, symmetric matrix A by Jacobi's method.  N must be less
C    than or equal to 50 (determined by the dimensions of B and Z below)
C    The unordered eigenvalues are returned in real N vector D and the
C    corresponding eigenvectors are in the respective columns of N by N
C    real matrix V.  The number of Jacobi rotations needed are returned
C    in NROT.  Taken from LINEAR ALGEBRA, volume II, by Wilkinson and
C    Reinsch, 1971, Springer-Verlag.  The algorithm is published (in
C    ALGOL) by H. Rutishauser on pages 202-211.  Converted to FORTRAN by
C    R. Buland, 30 Oct., 1978.
C
      INTEGER P,Q,P1,P2,Q1,Q2
      DIMENSION A(N,N),D(N),V(N,N),B(50),Z(50)
      N1=N-1
      NN=N*N
C***Initialize the storage array
      DO 1 P=1,N
      DO 2 Q=1,N
2     V(P,Q)=0.
      V(P,P)=1.
      B(P)=A(P,P)
      D(P)=B(P)
1     Z(P)=0.
      NROT=0
C***Make up to 50 passes rotating each off-diagonal element.
      DO 3 I=1,50
      SM=0.
      DO 4 P=1,N1
      P1=P+1
      DO 4 Q=P1,N
4     SM=SM+ABS(A(P,Q))
C***Exit if all off-diagonal elements have underflowed.
      IF (SM.EQ.0.) GO TO 13
      TRESH=0.
      IF (I.LT.4) TRESH=.2*SM/NN
C***Loop over each off-diagonal element
      DO 5 P=1,N1
      P1=P+1
      P2=P-1
      DO 5 Q=P1,N
      Q1=Q+1
      Q2=Q-1
      G=100.*ABS(A(P,Q))
C***Skip this element if it has already underflowed.
      IF((I.LE.4).OR.(ABS(D(P))+G.NE.ABS(D(P))).OR.
     1  (ABS(D(Q))+G.NE.ABS(D(Q)))) GO TO 6
      A(P,Q)=0.
      GO TO 5
6     IF(ABS(A(P,Q)).LE.TRESH) GO TO 5
C***Compute the rotation.
      H=D(Q)-D(P)
      IF(ABS(H)+G.EQ.ABS(H)) GO TO 7
      THETA=.5*H/A(P,Q)
      T=1./(ABS(THETA)+SQRT(1.+THETA*THETA))
      IF (THETA.LT.0.) T=-T
      GO TO 14
7     T=A(P,Q)/H
14    C=1./SQRT(1.+T*T)
      S=T*C
      TAU=S/(1.+C)
C***Rotate the diagonal.
      H=T*A(P,Q)
      Z(P)=Z(P)-H
      Z(Q)=Z(Q)+H
      D(P)=D(P)-H
      D(Q)=D(Q)+H
      A(P,Q)=0.
C***Rotate the off diagonal.  Note that only the upper-diagonal
C      elements are touched.  This allows the recovery of matrix A later.
      IF(P2.LT.1) GO TO 15
      DO 8 J=1,P2
      G=A(J,P)
      H=A(J,Q)
      A(J,P)=G-S*(H+G*TAU)
8     A(J,Q)=H+S*(G-H*TAU)
15    IF(Q2.LT.P1) GO TO 16
      DO 9 J=P1,Q2
      G=A(P,J)
      H=A(J,Q)
      A(P,J)=G-S*(H+G*TAU)
9     A(J,Q)=H+S*(G-H*TAU)
16    IF(N.LT.Q1) GO TO 17
      DO 10 J=Q1,N
      G=A(P,J)
      H=A(Q,J)
      A(P,J)=G-S*(H+G*TAU)
10    A(Q,J)=H+S*(G-H*TAU)
C***Rotate the eigenvector matrix.
17    DO 11 J=1,N
      G=V(J,P)
      H=V(J,Q)
      V(J,P)=G-S*(H+G*TAU)
11    V(J,Q)=H+S*(G-H*TAU)
      NROT=NROT+1
5     CONTINUE
C***Reset the temporary storage for the next rotation pass.
      DO 3 P=1,N
      D(P)=B(P)+Z(P)
      B(P)=D(P)
3     Z(P)=0.
C***All finished.  Prepare for exiting by reconstructing the upper
C     triangle of A from the untouched lower triangle.
13    DO 12 P=1,N1
      P1=P+1
      DO 12 Q=P1,N
12    A(P,Q)=A(Q,P)
      RETURN
      END
C
C
C
      SUBROUTINE ZERO(LX,X)
      DIMENSION X(LX)
      IF(LX.LE.0) RETURN
      DO 1 J=1,LX
1     X(J)=0.
      RETURN
      END
C
C
C
      SUBROUTINE MAINE(N,A,B)
      DIMENSION A(N,N),B(N,N)
      DO 5 I=1,N
      DO 5 J=1,N
5     B(I,J)=0.0
      B(1,1)=1./A(1,1)
      IF(N.EQ.1) RETURN
      DO 40 M=2,N
      K=M-1
      EK=A(M,M)
      DO 10 I=1,K
      DO 10 J=1,K
10    EK=EK-A(M,I)*B(I,J)*A(J,M)
      B(M,M)=1./EK
      DO 30 I=1,K
      DO 20 J=1,K
20    B(I,M)=B(I,M)-B(I,J)*A(J,M)/EK
30    B(M,I)=B(I,M)
      DO 40 I=1,K
      DO 40 J=1,K
40    B(I,J)=B(I,J)+B(I,M)*B(M,J)*EK
      RETURN
      END
C
C
C
      SUBROUTINE ZEROI(LX,IX)
      DIMENSION IX(LX)
      IF(LX.LE.0) RETURN
      DO 1 J=1,LX
1     IX(J)=0
      RETURN
      END
C
C
C
      SUBROUTINE DCDERIV(ST,DP,RK,M0,NSTAT,G,GDER)
C  Subroutine to calculate the derivative matrix for a double-couple source
C    given the starting strike, dip, rake and seismic moment.  All of this
C    uses the moment tensor formulation of Barker & Langston, GJRAS,
C    1982, p 777 and from Langston, BSSA, 1981, p 1.  All moment tensor
C    derivative components have been multiplied by -1 to be compatible
C    with the A&R coordinate system (see subroutine SDTOM).
C
C  Inputs:  G - Tensor array of Greens functions for each station.  Column
C            number i of G(i,j) has the 5 Greens functions of Langston, BSSA,
C            1981, p 1 for station j.  The dimensions of G are 5 by NSTAT.
C           M0 - Calculated seismic moment of event
C           ST - Calculated strike of event in degrees
C           DP - Calculated dip of event in degrees
C           RK - Calculated rake of event in degrees
C           NSTAT - Number of station observations in data set
C
C  Output: GDER - Matrix of NSTAT rows by 4 columns where the first column
C           contains the derivatives with respect to the fault strike,
C           the second column contains the derivatives with respect to the
C           fault dip, the third row contains the derivatives with respect
C           to the fault rake, and the fourth row contains the
C           derivatives with respect to the moment.  Row i correpsonds tO
C           station i as ordered in the G matrix.
C
      DIMENSION G(5,NSTAT),GDER(NSTAT,4)
      REAL M0
      PIBY180=3.14159265/180.
C***Form the tensor relating the errors in the strike, dip, rake and seismic
C     moment to the errors in the moment tensor elements.
      STSIN=SIN(ST*PIBY180)
      STCOS=COS(ST*PIBY180)
      ST2SIN=SIN(2.*ST*PIBY180)
      ST2COS=COS(2.*ST*PIBY180)
      DPSIN=SIN(DP*PIBY180)
      DPCOS=COS(DP*PIBY180)
      DP2SIN=SIN(2.*DP*PIBY180)
      DP2COS=COS(2.*DP*PIBY180)
      RKSIN=SIN(RK*PIBY180)
      RKCOS=COS(RK*PIBY180)
      DO 100 J=1,NSTAT
      DST1=(2.*STSIN*STCOS*RKSIN*DP2SIN+2.*ST2COS*RKCOS*DPSIN)*M0
      DRK1=(STSIN*STSIN*RKCOS*DP2SIN-ST2SIN*RKSIN*DPSIN)*M0
      DDP1=(2.*STSIN*STSIN*RKSIN*DP2COS+ST2SIN*RKCOS*DPCOS)*M0
      DM01=STSIN*STSIN*RKSIN*DP2SIN+ST2SIN*RKCOS*DPSIN
      DST2=(-2.*STSIN*STCOS*RKSIN*DP2SIN-2.*ST2COS*RKCOS*DPSIN)*M0
      DRK2=(STCOS*STCOS*RKCOS*DP2SIN+ST2SIN*RKSIN*DPSIN)*M0
      DDP2=(2.*STCOS*STCOS*RKSIN*DP2COS-ST2SIN*RKCOS*DPCOS)*M0
      DM02=STCOS*STCOS*RKSIN*DP2SIN-ST2SIN*RKCOS*DPSIN
      DST3=(-ST2COS*RKSIN*DP2COS+2.*ST2SIN*RKCOS*DPSIN)*M0
      DRK3=(-.5*ST2SIN*RKCOS*DP2SIN+ST2COS*RKSIN*DPSIN)*M0
      DDP3=(-ST2SIN*RKSIN*DP2COS-ST2COS*RKCOS*DPCOS)*M0
      DM03=-.5*ST2SIN*RKSIN*DP2SIN-ST2COS*RKCOS*DPSIN
      DST4=(-STSIN*RKCOS*DPCOS+STCOS*RKSIN*DP2COS)*M0
      DRK4=(-STCOS*RKSIN*DPCOS+STSIN*RKCOS*DP2COS)*M0
      DDP4=(-STCOS*RKCOS*DPSIN-2.*STSIN*RKSIN*DP2SIN)*M0
      DM04=STCOS*RKCOS*DPCOS+STSIN*RKSIN*DP2COS
      DST5=(STSIN*RKSIN*DP2COS+STCOS*RKCOS*DPCOS)*M0
      DRK5=(-STCOS*RKCOS*DP2COS-STSIN*RKSIN*DPCOS)*M0
      DDP5=(2.*STCOS*RKSIN*DP2SIN-STSIN*RKCOS*DPSIN)*M0
      DM05=-STCOS*RKSIN*DP2COS+STSIN*RKCOS*DPCOS
      GDER(J,1)=G(1,J)*DST1+G(2,J)*DST2+G(3,J)*DST3+G(4,J)*DST4+G(5,J)
     &*DST5
      GDER(J,1)=-GDER(J,1)
      GDER(J,2)=G(1,J)*DDP1+G(2,J)*DDP2+G(3,J)*DDP3+G(4,J)*DDP4+G(5,J)
     &*DDP5
      GDER(J,2)=-GDER(J,2)
      GDER(J,3)=G(1,J)*DRK1+G(2,J)*DRK2+G(3,J)*DRK3+G(4,J)*DRK4+G(5,J)
     &*DRK5
      GDER(J,3)=-GDER(J,3)
      GDER(J,4)=G(1,J)*DM01+G(2,J)*DM02+G(3,J)*DM03+G(4,J)*DM04+G(5,J)
     &*DM05
      GDER(J,4)=-GDER(J,4)
100   CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE NEWTDIF(N,XI,YI,X,Y)
C
C  Subroutine to perform Newton Divided Difference interpolation of unequally
C   spaced data.
C
C  Inputs:  N - Number of input pairs XI and YI along curve to be inter-
C               polated
C           XI - Array of independent coordinate values along curve to
C                be interpolated (must be in ascending order)
C           YI - Array of dependent coordinate values along curve to be
C                interpolated.  Each YI(J) corresponds to XI(J)
C           X - Independent coordinate at which function is to be evaluated
C
C  Output: Y - Interpolated value of X
C
      DIMENSION XI(*), YI(*)
      DIMENSION DELTAR(10)
      NTEMP=10
C***A maximum of 10 values can be used at the present time.  To increase
C    the number of points being used in the interpolation, change the
C    number 10 in the previous two lines and in this comment and recompile.
      NN=N
      IF (NN.GT.NTEMP) THEN
         WRITE(6,1) NTEMP
1        FORMAT(' WARNING, ONLY ',I2,' TERMS WILL BE USED FOR INTERPOL
     *ATION!')
         NN = NTEMP
      ENDIF
C***Form the divided differences
      DO 10 J=1, NN
10    DELTAR(J) = YI(J)
      XDIF = 1.
      Y = YI(1)
      DO 20 J=1, NN-1
         DO 30 I=1, NN-J
30       DELTAR(I) = (DELTAR(I+1) - DELTAR(I)) / (XI(I+J) - XI(I))
      XDIF = (X - XI(J)) * XDIF
20    Y = Y + XDIF * DELTAR(1)
      RETURN
      END
