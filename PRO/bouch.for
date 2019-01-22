
c
c   changes
c
c   may 1999 by jh : -----------  version 7.0 check -------------------
c                    changes to subroutine make_bou
c   jan 18 2009 jh : increase dimensions 
c
c org      parameter  (nfq=513,nsta=32,njm=1200,nz=10)

      parameter  (nfq=2049,nsta=32,njm=8000,nz=20)
	DIMENSION TH(nz),ALPHA(nz),BETA(nz),DENS(nz),CTH(nz),EMU(nz),
     & AL0(nz),BE0(nz),QP(nz),QS(nz),AP(nz),AS(nz),
     & EMU2(nz),CMU(nz),WA2(nz),WB2(nz),R1(nz),R3(nz),R5(nz),R6(nz),
     & R7(nz),R8(nz),R9(nz),R10(nz),R11(nz),R12(nz),R14(nz),R15(nz),
     & R(nsta),S2T(nsta),C2T(nsta),U1(nsta),U2(nsta)
     & ,U3(nsta),DJ1(nsta),DJ2(nsta),DJ3(nsta),CT(nsta),ST(nsta),
     & COMP1(nsta),COMP2(nsta),CP1(nsta),CP2(nsta),SP1(nsta),SP2(nsta),
     & TETA(nsta), CJ1(5),CJ2(5),CJ3(5),K00(nfq),
     & AK20(njm),AJ1(njm,nsta,5),AJ2(njm,nsta,5),AJ3(njm,nsta,5),
     & AJ4(njm,nsta,5),AJ5(njm,nsta,5),
     & B(2,4),BB(6),BBB(6),G(4,4),P(4,4),A(4,4),O(36),OO(6),OOO(6)
     & ,SH(2),TSH(2),PSH(2),GSH(2,2)
	COMPLEX*16 AI,AIPI,CZ,CTH,CMU,OMEGA,WA2,WB2,R0,B1,B2,B3,R1,R3,
     & R5,R6,R7,R8,R9,R10,R11,W1,W2,WZA,WZB,WZ1,WZ2,C0,C0B,B,BB,BBB,
     & CE1,CE2,CE3,CE4,CA,CB,SA,SB,G,D4,D5,D6,D7,D8,P,A,O,OO,DET,OOO,
     & C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C14,C16,C17,C19,
     & C20,C21,C22,C23,C24,C25,C26,C27,C28,C29,C30,C31,C32,C33,C34,
     & CX,C02,C35,C36,R12,R14,R15,D9,D10,D11,AIK,AIKI,D12
     & ,TSH,PSH,GSH,SH,CSH,CSH1,CLIQ
     & ,ALPHA,BETA,AP,AS,EMU,EMU2,XLNF
	COMPLEX*16 Q1,Q2,Q3,U1,U2,U3,AJ2,AJ4,AJ5
	REAL*8 TH,AL0,BE0,QP,QS,DENS,Z0,TL,Q,R,XL,PI,PI2,PIL,A2,
     & DFREQ,AW,RW,FREQ,AK20,AK2,AK
     & ,ST,CT,S2T,C2T,XZZ,ZOM,PHI
        REAL*8 AAA,VY,A1,BJ0,BJ1,xzz1,xzz2
	LOGICAL LIQ
	COMPLEX*16 DUC,DUS,DW,DTC,DTS,DS,DSC2,DSS2,DVC,DVS,DXC2,DXS2,
     & CJ1,CJ2,CJ3,DJ1,DJ2,DJ3
	REAL*8 DFXL,A3,A4,A5,A6,A7,F1N2,F1N3,F2N2,F2N3,F3N2,F3N3,
     & CN2,CN3
       character*40 fic
C-----------------------------------------------------------------------
C         INPUT PARAMETERS:
C    - NL:    NUMBER OF LAYERS
C    - TH:    LAYER THICKNESS
C    - AL0:   P- WAVE VELOCITY
C    - BE0:   S- WAVE VELOCITY
C    - DENS:  DENSITY
C    - QP:    P-WAVE QUALITY FACTOR
C    - QS:    S- WAVE QUALITY FACTOR
C    - L0:    SOURCE LAYER (L0=1 CORRESPONDS TO THE CASE WHERE THE SOUR CE IS
C             IN THE UPPER LAYER).  THE SOURCE CANNOT BE IN THE HALF-SPACE .
C    - Z0:    SOURCE DEPTH IN LAYER L0 (MEASURED FROM THE TOP OF LAYER L0)
C    - STRIKE:FAULT STRIKE MEASURED CLOCKWISE FROM NORTH
C    - DIP:   DIP ANGLE
C    - ASLIP: SLIP ANGLE.  IMPORTANT TO NOTE THAT IN AN X,Y,Z COORDINATE
C	      SYSTEM, THE X AXIS POINTS ALONG THE STRIKE, Z IS DOWN INTO
C	      THE EARTH, AND Y IS THE CROSS PRODUCT DIRECTION FOR A RIGHT-
C             HANDED SYSTEM.  NOW, THE SLIP ANGLE GIVES THE DIRECTION OF
C             MOTION OF THE -Y BLOCK RELATIVE TO THE +Y BLOCK.  FOR EXAMPLE,
C	      SUPPOSE THE DIRECTION OF THE STRIKE IS DUE NORTH.  THEN THE
C	      +Y DIRECTION IS DUE EAST.  A SLIP ANGLE OF -180 MEANS THAT
C	      THE WEST BLOCK MOVES TO THE SOUTH AND THE EAST BLOCK MOVES
C	      TO THE NORTH, I.E., A LEFT-LATERAL FAULT.
C    - TL:    LENGTH OF THE DESIRED TIME WINDOW
C    - Q:     DETERMINES THE IMAGINARY PART OF THE FREQUENCY (LATER REMOVED
C             FROM THE TIME DOMAIN SOLUTION):  IMAGINARY PART OF CIRCULAR
C             FREQUENCY = 3.14159.../ Q .  SUGGESTED VALUE:  Q=TL .
C    - NT:    NUMBER OF POINTS OF THE SEISMOGRAMS
C    - NR:    NUMBER OF RECEIVERS
C    - R:     EPICENTRAL DISTANCE
C    - TETA: STATION AZIMUTH MEASURED CLOCKWISE FROM NORTH
C    - COMP1: AZIMUTH OF HORIZONTAL COMPONENT OF MOTION MEASURED CLOCKWISE
C             FROM NORTH.
C    - COMP2: SAME AS ABOVE FOR THE OTHER COMPONENT
C    - IR0:   DETERMINES ON WHICH RECEIVER THE CONVERGENCE TEST IS TO BE
C	      PERFORMED.
C    - XL:    DISCRETIZATION LENGTH (RADIAL INTERVAL BETWEEN SOURCES)
C    - M:     MAXIMUM NUMBER OF HORIZONTAL WAVENUMBERS CONSIDERED
C    - EPS:   DETERMINES WHEN TO TRUNCATE THE SERIES OVER THE HORIZONTAL
C             WAVENUMBER: TRUNCATION TAKES PLACE WHEN THE TERM OF THE SERIES
C             BECOMES SMALLER (OR EQUAL TO) EPS*(SUM OF THE SERIES).
C    - LIQ:   - TRUE IF UPPER LAYER IS A LIQUID LAYER.  IN THIS CASE THE
C             SEISMOGRAMS ARE COMPUTED AT THE BOTTOM OF THE LIQUID LAYER.
C	      - FALSE OTHERWISE
C----------------------------------------------------------------------------
c   ============================
c   ============================
c	 READ(5,*)nl
c	 read(5,*) th(i),al0(i),be0(i),dens(i),qp(i),qs(i)
c	 read(5,*)nr
c	 read(5,*) r(i),teta(i),comp1(i),comp2(i)
c	 read(5,*) l0,z0,strike,dip,aslip
c	 read(5,*) tl,q,nt,ir0
c	 read(5,*) xl,m,eps,liq
c   ===========================
c   ===========================
c
c   make syntetic parameter file synt.inp from hyp.out station file
c
c      call makesyn
c
c
c   make bouchon input file from synt.inp and hyp.out file
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call makebou()
c
	AI=(0.d0,1.d0)
	PI=3.1415926535d0
	PI2=PI+PI
cjh	write(*,*)' fichier donnees'
cjh	read(*,'(a)') fic
	fic='bouch.inp'
	OPEN(5,FILE=fic,status='unknown')
cjh152	continue
cjh    write(*,*)' fichier sortie'
cjh	read(*,'(a)') fic
cjh	inquire(file=fic,exist=liq)
        fic='bouch.out'
cjh	if(liq) then
cjh	write(*,*)' fichier existe deja- on ecrit dessus(1)'
cjh	read(*,*) nl
cjh	if(nl.ne.1) go to 152
cjh	endif
        OPEN(10,FILE=fic,status='unknown')
2500	write(*,*)' nombre de couches- fin=0'
	READ(5,*,end=80)nl
	write(*,*) nl
	if(nl.eq.0) go to 80
	if(nl.gt.nz) go to 2500
	do 750 i=1,nl
	read(5,*) th(i),al0(i),be0(i),dens(i),qp(i),qs(i)
750	continue
        DO 919 I=1,NL
	if(be0(i).eq.0.) BE0(I)=AL0(I)/SQRT(3.)
	if(qp(i).lt.1e-6) qp(i)=9999.
	if(qs(i).lt.1e-6) qs(i)=9999.
919	continue
5551	FORMAT(4F6.3,2F7.1)
2520	write(*,*) ' nombre de recepteurs'
	read(5,*)nr
	if(nr.gt.nsta) go to 2520
	do 751 i=1,nr
 	read(5,*) r(i),teta(i),comp1(i),comp2(i)
751	continue
	write(*,*)' enter l0,z0,strike,dip,slip'
	read(5,*) l0,z0,strike,dip,aslip
2540	write(*,*)' enter tl,q,nt,ir0,'
	write(*,*)' nt must be equal to a power of 2'
	read(5,*) tl,q,nt,ir0
	if(nt.ge.nfq*2) then
	    write(*,*)' nt too big for dimentions'
	    go to 2540
	endif
2530	write(*,*)' enter xl,m,eps'
	read(5,*) xl,m,eps
	if(m.gt.njm) go to 2530
	write(*,*)' liq (t or f)'
	read(5,*) liq
	write(*,*) ' xl,m,eps,liq',xl,m,eps,liq
	write(10,*) nt,tl,q,nr
	WRITE(*,6663)
	WRITE(*,6661) (L,TH(L),AL0(L),BE0(L),DENS(L),QP(L),QS(L),L=1,NL)
6663	FORMAT('  L   TH     AL0   BE0   DENS   QP   QS')
6661    FORMAT(' ',I2,2x,4F7.3,2F7.1)
5552	FORMAT(5F8.2)
	WRITE(*,6664)
	WRITE(*,6662) (IR,R(IR),TETA(IR),COMP1(IR),COMP2(IR),
     &	IR=1,NR)
6662	FORMAT(' ',I4,4F8.2)
6664	FORMAT('   IR    R       TETA   COMP1  COMP2')
	PIL=PI2/XL
	AIPI=AI*PIL
	DFREQ=1.d0/TL
	NFREQ=NT/2+1
	AW=-PI/Q
	DFXL=DFREQ/XL
	XDIP=DIP*PI/180
	XSLIP=ASLIP*PI/180
	CN2=SIN(XDIP)
	CN3=-COS(XDIP)
	CF1=COS(XSLIP)
	CF2=SIN(XSLIP)*CN3
	CF3=-SIN(XSLIP)*CN2
	F1N2=CF1*CN2
	F1N3=CF1*CN3
	F2N2=CF2*CN2
	F2N3=CF2*CN3
	F3N2=CF3*CN2
	F3N3=CF3*CN3
	CZ=AI*(TH(L0)-Z0)
	NL1=NL-1
	DO 9 L=1,NL1
9	CTH(L)=AI*TH(L)
	NL0=1
	IF(LIQ) NL0=2
	DO 14 IR=1,NR
	AZI=(COMP1(IR)-strike)*PI/180.
	CP1(IR)=COS(AZI)
	SP1(IR)=SIN(AZI)
	AZI=(COMP2(IR)-strike)*PI/180.
	CP2(IR)=COS(AZI)
	SP2(IR)=SIN(AZI)
	AZI=(TETA(IR)-STRIKE)*PI/180.
	ct(IR)=COS(AZI)
	st(IR)=SIN(AZI)
	S2T(IR)=ST(IR)*(CT(IR)+CT(IR))
14	C2T(IR)=CT(IR)**2-ST(IR)**2
	write(10,*)(r(ir),ir=1,nr)
	DO 1 L=1,NL
	AP(L)=(1.+AI/(QP(L)+QP(L)))/(1.+.25/QP(L)**2)*AL0(L)
	IF(LIQ.AND.L.EQ.1) GO TO 1
	AS(L)=(1.+AI/(QS(L)+QS(L)))/(1.+.25/QS(L)**2)*BE0(L)
1	CONTINUE
	DSC2=-AI*F2N2*DFXL
	DSS2=AI*F1N2*DFXL
	DXC2=F1N2*DFXL
	DXS2=F2N2*DFXL
	AK=0.
	DO 10 K=1,M
	AK=AK+PIL
	AK20(K)=AK**2
	DO 10 IR=1,NR
	A1=AK*R(IR)
C-----------------
C    COMPUTES BESSEL FUNCTIONS J0 AND J1
	call ff01ad(AAA,VY,A1,0)
        BJ0=AAA
	call ff02ad(AAA,VY,A1,0)
	BJ1=AAA
C-----------------
	A2=AK*BJ0
	A3=AK*BJ1
	A4=BJ1/R(IR)
	A5=A2-A4
	C1=AI*A4
	C2=AI*A5
	C3=-AI*A3
	A6=(BJ0-(A4+A4)/AK)/R(IR)
	A6=A6+A6
	A7=A6+A3
	C4=AI*A6
	C5=AI*A7
	C6=AI*(A2-A4-A4)
	AJ1(K,IR,1)=-A3
	AJ2(K,IR,1)=0.
	AJ3(K,IR,1)=0.
	AJ4(K,IR,1)=0.
	AJ5(K,IR,1)=-AI*A2
	AJ1(K,IR,2)=A5*CT(IR)
	AJ2(K,IR,2)=-C1*CT(IR)
	AJ3(K,IR,2)=-A4*ST(IR)
	AJ4(K,IR,2)=C2*ST(IR)
	AJ5(K,IR,2)=C3*CT(IR)
	AJ1(K,IR,3)=A5*ST(IR)
	AJ2(K,IR,3)=C1*ST(IR)
	AJ3(K,IR,3)=A4*CT(IR)
	AJ4(K,IR,3)=C2*CT(IR)
	AJ5(K,IR,3)=C3*ST(IR)
	AJ1(K,IR,4)=A7*C2T(IR)
	AJ2(K,IR,4)=C4*C2T(IR)
	AJ3(K,IR,4)=A6*S2T(IR)
	AJ4(K,IR,4)=C5*S2T(IR)
	AJ5(K,IR,4)=C6*C2T(IR)
	AJ1(K,IR,5)=A7*S2T(IR)
	AJ2(K,IR,5)=-C4*S2T(IR)
	AJ3(K,IR,5)=-A6*C2T(IR)
	AJ4(K,IR,5)=C5*C2T(IR)
10	AJ5(K,IR,5)=C6*S2T(IR)
	DO 765 J=1,5
cjh	WRITE(*,*) AJ1(3,1,J),AJ2(3,1,J),AJ3(3,1,J),AJ4(3,1,J),
cjh     &	AJ5(3,1,J)
765	CONTINUE
	FREQ=0.
	DO 100 IF=1,NFREQ
	RW=PI2*FREQ
	OMEGA=CMPLX(RW,AW)
C--------------------------
	ZOM=SQRT(RW**2+AW**2)/PI2
	IF(IF.EQ.1) PHI=-PI/2.
	IF(IF.NE.1) PHI=ATAN(AW/RW)
	XLNF=(AI*PHI+DLOG(ZOM))/PI
	DO 2 L=1,NL
	ALPHA(L)=AP(L)/(1.-XLNF/QP(L))
	WA2(L)=(OMEGA/ALPHA(L))**2
	IF(LIQ.AND.L.EQ.1) GO TO 2
	BETA(L)=AS(L)/(1.-XLNF/QS(L))
	EMU(L)=BETA(L)**2*DENS(L)
	EMU2(L)=EMU(L)+EMU(L)
	CMU(L)=AI*EMU(L)
	WB2(L)=(OMEGA/BETA(L))**2
2	CONTINUE
	C1=DFXL/EMU(L0)
	C2=2.*(BETA(L0)/ALPHA(L0))**2
	DUC=F1N3*C1
	DUS=(F2N3+F3N2)*C1
	DW=AI*(F3N3+F3N3)*DFXL/(DENS(L0)*ALPHA(L0)**2)
	DTC=F1N3*DFXL*(1.-C2)
	DTS=DFXL*(F2N3+F3N2)*(1.-C2)
	DS=-AI*F3N3*DFXL*(C2+C2-3.)
	DVC=-AI*(F2N3+F3N2)*C1
	DVS=AI*F1N3*C1
	B1=-(BETA(NL)/ALPHA(NL))**2
	B2=.5/(DENS(NL)*ALPHA(NL)**2)
	B3=-.5/EMU2(NL)
	IF(LIQ) CLIQ=AI*DENS(1)*OMEGA**2
	R0=1./WB2(NL)
	DO 48 L=NL0,NL1
	R1(L)=1./WB2(L)
	R3(L)=CMU(L)*R1(L)
	R5(L)=R3(L)+R3(L)
	R6(L)=R1(L)**2
	R7(L)=CMU(L)*R6(L)
	R8(L)=-R3(L)**2
	R9(L)=R1(L)/CMU(L)
	R10(L)=-R9(L)*R1(L)
	R12(L)=-R5(L)*AI
	R14(L)=EMU(L)*R6(L)
	R15(L)=AI*R10(L)
48	R11(L)=-R9(L)**2
	DO 40 IR=1,NR
	U1(IR)=0.
	U2(IR)=0.
40	U3(IR)=0.
	AK=0.
	DO 50 K=1,M
	AK=AK+PIL
	AK2=AK20(K)
	AIK=AI*AK
	AIKI=-AI/AK
	C0=AK2+AK2
	DO 3 LL=1,NL
	L=NL+1-LL
	W1=WA2(L)-AK2
         wza=cdsqrt(w1)
	Q1=WZA
	IF(DIMAG(Q1).GT.0.) WZA=-WZA
	WZ1=1./WZA
	IF(LIQ.AND.L.EQ.1) GO TO 16
	W2=WB2(L)-AK2
         wzb=cdsqrt(w2)
	Q2=WZB
	IF(DIMAG(Q2).GT.0.) WZB=-WZB
	WZ2=1./WZB
	C0B=C0-WB2(L)
	IF(L.NE.NL) GO TO 4
	C2=C0B/(AK+AK+AK+AK)
	B(1,1)=B1
	B(1,2)=C2*WZ1*(B1+B1)
	B(1,3)=B2
	B(1,4)=-AK*WZ1*B2
	B(2,1)=C2*WZ2
	B(2,2)=-.5
	B(2,3)=AK*WZ2*B3
	B(2,4)=B3
	IJ=0
	DO 144 I=1,3
	I1=I+1
	DO 144 J=I1,4
	IJ=IJ+1
144	BB(IJ)=B(1,I)*B(2,J)-B(1,J)*B(2,I)
	TSH(1)=-AI/(AK+AK)
	TSH(2)=-AI/((EMU(L)+EMU(L))*WZB)
	GO TO 3
16	IW=1
         CE1=CDEXP(CTH(1)*WZA)
	CE2=1./CE1
	C1=CLIQ*WZ1*(CE1-CE2)/(CE1+CE2)*AIKI
	G(1,1)=1.
	G(1,2)=0.
	G(2,1)=0.
	G(2,2)=1.
	G(3,1)=0.
	G(3,2)=C1
	G(4,1)=0.
	G(4,2)=0.
	O(1)=1.
	O(2)=C1
	O(3)=0.
	O(4)=0.
	O(5)=0.
	O(6)=0.
	NJ=2
	GO TO 990
4	IF(L.NE.L0) GO TO 1003
	DO 1004 I=1,6
1004	BBB(I)=BB(I)
	PSH(1)=TSH(1)
	PSH(2)=TSH(2)
1003	NJ=4
	IF(L.EQ.1) NJ=2
	CX=CTH(L)
	C02=C0+C0
	C3=C0**2
	C4=C02*C0B
	C5=C0B**2
	C6=WZA*WZB
	C7=C0*(C6+C6)
	C12=C5/C6
	C14=AK2*C12+C7
	C16=WZB*C02
	C17=C5*WZ1
	C19=C0B*C12+C7+C7
	C20=C5*WZ2
	C21=WZA*C02
	C22=C4*(C0B+C0B)
	C23=C5*C12+C7*C02
	D9=R3(L)*AIKI
	D11=-R8(L)/AK2
	CSH=EMU(L)*WZB/AK
	IW=0
	IF(L.EQ.1.AND.L0.NE.1) GO TO 1000
	C24=AK2*WZ2
	C25=-AK*R1(L)
	C26=C0B*WZ2
	C27=-WZA*WZ2
	C28=R15(L)*AK2
	C30=C0B*WZ1
	C29=C6+C6+C24*C30
	C31=AK2*(C6+C6+C6+C6+C12)
	C32=AK2*WZ1
	C33=-WZB*WZ1
	C34=C6+C24*C32
	D10=R9(L)*AIK
	D12=-AK2*R11(L)
1000    continue	
         CE1=CDEXP(CX*WZA)
         CE2=CDEXP(CX*WZB)
	IW=IW+1
	C1=CE1*.5
	C2=.25/C1
	CA=C1+C2
	SA=C1-C2
	C1=CE2*.5
	C2=.25/C1
	CB=C1+C2
	SB=C1-C2
	GSH(1,1)=CB
	GSH(2,2)=CB
	GSH(1,2)=SB/CSH
	GSH(2,1)=SB*CSH
	IF(IW.EQ.2) GO TO 2002
	CSH1=TSH(1)*GSH(1,1)+TSH(2)*GSH(2,1)
	TSH(2)=TSH(1)*GSH(1,2)+TSH(2)*GSH(2,2)
	TSH(1)=CSH1
	GO TO 2003
2002	CSH1=PSH(1)*GSH(1,1)+PSH(2)*GSH(2,1)
	PSH(2)=PSH(1)*GSH(1,2)+PSH(2)*GSH(2,2)
	PSH(1)=CSH1
2003 	CONTINUE
	C8=CA*CB
	C9=SA*SB
	C10=CA*SB
	C11=CB*SA
	O(1)=R6(L)*(C8*(C3+C5)-C4+C9*C14)
	O(2)=D9*(C16*C10+C17*C11)
	O(3)=R14(L)*((C4+C5+C5)*(1.-C8)-C9*C19)
	O(4)=O(3)
	O(5)=-D9*(C20*C10+C21*C11)
	O(6)=D11*(C22*(C8-1.)+C9*C23)
	NIJ=6
	IF(L.EQ.1.AND.IW.EQ.1) GO TO 1001
	C35=WZA*C11
	C36=WZB*C10
	O(7)=D10*(C24*C10+C35)
	O(8)=C8
	O(9)=C25*(C26*C10+C35+C35)
	O(10)=O(9)
	O(11)=C27*C9
	O(12)=O(5)
	O(13)=C28*((C0+C0B)*(1.-C8)-C29*C9)
	O(14)=-C25*(C36+C36+C30*C11)
	O(15)=R6(L)*(C3+C5-C4*C8-C31*C9)
	O(16)=O(15)-1.
	O(17)=O(9)
	O(18)=O(3)
	O(19)=O(13)
	O(20)=O(14)
	O(21)=O(16)
	O(22)=O(15)
	O(23)=O(9)
	O(24)=O(3)
	O(25)=-D10*(C36+C32*C11)
	O(26)=C33*C9
	O(27)=O(14)
	O(28)=O(14)
	O(29)=O(8)
	O(30)=O(2)
	O(31)=D12*(C0*(C8-1.)+C34*C9)
	O(32)=O(25)
	O(33)=O(13)
	O(34)=O(13)
	O(35)=O(7)
	O(36)=O(1)
	NIJ=36
1001	IF(L.LE.L0) GO TO 9393
	XZZ1=0.
	DO 9090 IJ=1,NIJ
	Q1=O(IJ)
	XZZ2=DABS(DREAL(Q1))+DABS(DIMAG(Q1))
	IF(XZZ2.GT.XZZ1) XZZ1=XZZ2
9090	CONTINUE
	XZZ=1./XZZ1
	DO 9191 IJ=1,NIJ
9191	O(IJ)=O(IJ)*XZZ
9393	IF(L.GE.L0.AND.IW.EQ.1) GO TO 999
	IF(L. NE.L0) GO TO 950
	C1=CE3/(CE1+CE1)
	C2=.25/C1
	CA=C1+C2
	SA=C1-C2
	C1=CE4/(CE2+CE2)
	C2=.25/C1
	CB=C1+C2
	SB=C1-C2
950	G(1,1)=R1(L)*(C0*CA-C0B*CB)
	C8=SA*WZ1
	C9=SB*WZ2
	C10=SA*WZA
	C11=SB*WZB
	D4=C0B*C8
	D5=C11+C11
	D6=AK*R1(L)
	G(1,2)=D6*(D4+D5)
	D7=C10+C10
	D8=C0B*C9
	G(2,1)=-D6*(D7+D8)
	G(2,2)=R1(L)*(C0*CB-C0B*CA)
	G(3,1)=R12(L)*C0B*(CA-CB)
	G(3,2)=D9*(C0B*D4+D5*C0)
	G(4,1)=D9*(C0*D7+D8*C0B)
	G(4,2)=G(3,1)
	IF(L.EQ.1) GO TO 997
	G(1,3)=-D10*AK*(CA-CB)
	G(1,4)=D10*(AK2*C8+C11)
	G(2,3)=D10*(C10+AK2*C9)
	G(2,4)=G(1,3)
	G(3,3)=G(2,2)
	G(3,4)=G(1,2)
	G(4,3)=G(2,1)
	G(4,4)=G(1,1)
997	IF(L.NE.L0) GO TO 990
	DO 991 I=1,4
	DO 991 J=1,NJ
991	P(I,J)=G(I,J)
	GO TO 999
990	DO 992 I=1,4
	DO 992 J=1,NJ
	A(I,J)=0.
	DO 992 IJ=1,4
992	A(I,J)=A(I,J)+P(I,IJ)*G(IJ,J)
	DO 993 I=1,4
	DO 993 J=1,NJ
993	P(I,J)=A(I,J)
999	IF(L.EQ.1.AND.IW.EQ.1) GO TO 1011
	IF(IW.EQ.2) GO TO 1020
	IJ=0
	DO 1005 I=1,6
	OO(I)=0.
	DO 1005 J=1,6
	IJ=IJ+1
1005	OO(I)=OO(I)+BB(J)*O(IJ)
	DO 1006 I=1,6
1006	BB(I)=OO(I)
	GO TO 1010
1011	DET=0.
	DO 1007 I=1,6
1007	DET=DET+BB(I)*O(I)
	DET=1./DET
1010	IF(L.NE.L0) GO TO 3
	CE3=CE1
	CE4=CE2
	CX=CZ
	GO TO 1000
1020	IJ=0
	DO 1025 I=1,6
	OOO(I)=0.
	DO 1025 J=1,6
	IJ=IJ+1
1025	OOO(I)=OOO(I)+BBB(J)*O(IJ)
3	CONTINUE
	A(1,4)=P(1,2)*OOO(3)+P(2,2)*OOO(5)+P(3,2)*OOO(6)
	A(2,4)=-P(1,1)*OOO(3)-P(2,1)*OOO(5)-P(3,1)*OOO(6)
	SH(2)=-PSH(2)/TSH(1)
	A(1,1)=-P(2,2)*OOO(1)-P(3,2)*OOO(2)-P(4,2)*OOO(3)
	A(1,2)=P(1,2)*OOO(1)-P(3,2)*OOO(4)-P(4,2)*OOO(5)
	A(2,1)=P(2,1)*OOO(1)+P(3,1)*OOO(2)+P(4,1)*OOO(3)
	A(2,2)=-P(1,1)*OOO(1)+P(3,1)*OOO(4)+P(4,1)*OOO(5)
	A(1,3)=P(1,2)*OOO(2)+P(2,2)*OOO(4)-P(4,2)*OOO(6)
	A(2,3)=-P(1,1)*OOO(2)-P(2,1)*OOO(4)+P(4,1)*OOO(6)
	SH(1)=-PSH(1)/TSH(1)
	DO 180 I=1,2
	DO 180 J=1,4
180	A(I,J)=A(I,J)*DET
	CJ1(1)=DW*A(1,2)+DS*A(1,4)
	CJ2(1)=0.
	CJ3(1)=DW*A(2,2)+DS*A(2,4)
	CJ1(2)=DUC*A(1,1)+DTC*A(1,3)
	CJ2(2)=DVS*SH(1)
	CJ3(2)=DUC*A(2,1)+DTC*A(2,3)
	CJ1(3)=DUS*A(1,1)+DTS*A(1,3)
	CJ2(3)=DVC*SH(1)
	CJ3(3)=DUS*A(2,1)+DTS*A(2,3)
	CJ1(4)=DSC2*A(1,4)
	CJ2(4)=DXS2*SH(2)
	CJ3(4)=DSC2*A(2,4)
	CJ1(5)=DSS2*A(1,4)
	CJ2(5)=DXC2*SH(2)
	CJ3(5)=DSS2*A(2,4)
	DO 181 IR=1,NR
	DJ1(IR)=0.
	DJ2(IR)=0.
	DJ3(IR)=0.
	DO 181 J=1,5
	DJ1(IR)=DJ1(IR)+AJ1(K,IR,J)*CJ1(J)+AJ2(K,IR,J)*CJ2(J)
	DJ2(IR)=DJ2(IR)+AJ3(K,IR,J)*CJ1(J)+AJ4(K,IR,J)*CJ2(J)
181	DJ3(IR)=DJ3(IR)+AJ5(K,IR,J)*CJ3(J)
	DO 21 IR=1,NR
	U1(IR)=U1(IR)+DJ1(IR)
	U2(IR)=U2(IR)+DJ2(IR)
	U3(IR)=U3(IR)+DJ3(IR)
	IF(IR.NE.IR0) GO TO 21
C-----------------------
C  CONVERGENCE TEST
	Q1=DJ1(IR)
	Q2=DJ2(IR)
	Q3=DJ3(IR)
	A10=ABS(REAL(Q1))+ABS(DIMAG(Q1))
	A11=ABS(REAL(Q2))+ABS(DIMAG(Q2))
	A15=ABS(REAL(Q3))+ABS(DIMAG(Q3))
	A12=EPS*(ABS(REAL(U1(IR)))+ABS(DIMAG(U1(IR))))
	A14=EPS*(ABS(REAL(U2(IR)))+ABS(DIMAG(U2(IR))))
	A16=EPS*(ABS(REAL(U3(IR)))+ABS(DIMAG(U3(IR))))
21	CONTINUE
	IF((A10.LE.A12.AND.A11.LE.A14).AND.A15.LE.A16) GO TO 20
C-----------------------
50	CONTINUE
20	K00(IF)=K
	DO 30 IR=1,NR
C----------------------
C  COMPONENTS OF GROUND DISPLACEMENT (IN THE FREQUENCY DOMAIN) FOR AN IMPULSE
C  DISLOCATION AT THE SOURCE. 
C
	C1=U1(IR)*CT(IR)-U2(IR)*ST(IR)
	C2=U1(IR)*ST(IR)+U2(IR)*CT(IR)
	U1(IR)=C1*CP1(IR)+C2*SP1(IR)
	U2(IR)=C1*CP2(IR)+C2*SP2(IR)
C----------   RADIAL
C      U1(IR)
C----------   TANGENTIAL
C      U2(IR)
C----------   VERTICAL
C      U3(IR)
C----------
30	CONTINUE
	WRITE(10,73)(U1(IR),u2(ir),u3(ir),IR=1,NR)
73	FORMAT(10E12.5)
C---------------------
C K=K00(IF)  IS THE NUMBER OF HORIZONTAL WAVENUMBERS CONSIDERED FOR EACH
C  FREQUENCY
	WRITE(10,61) K
	write(*,*)'if,k:',if,k00(if)
C---------------------
100	FREQ=FREQ+DFREQ
c	 WRITE(*,61) (K00(IF),IF=1,NFREQ)
61	FORMAT(' ',16I5)
6161	FORMAT(1X,10E12.5)
	GO TO 2500
80	STOP
	END
C/     ADD NAME=FF01AD          HSL     F77     DOUBLE
C######DATE   01 J
c  AN 1984     COPYRIGHT UKAEA, HARWELL.
C######ALIAS FF01AD
c---------------------------------------------------------------------
c   make a bouchon input file from hyp.out
c
c
c  updates:
c  apr 5 93 by jh: bug when reading all from station0.hyp
c  apr 7 93      : bugs in makesynt
c  jul   93      : use hyp.out instead of synt.inp
c  apr   95      : set damping equal to time
c  may   99      : ----------- version 7.0 ------------------
c                  dimensions, 5 char stat
c
      subroutine makebou()
c
      implicit none
      include 'seidim.inc'
c--- the hyp file
      character*80 data(max_data)
c--- nymber fo lines in hyp file
      integer ndata
c--- model parameters
      real beta0,depth(30)
c--- fault plane solution
      real strike,dip,rake,azimuth(max_trace)
c-- code for component orientation
      logical radial
c--- component orientation
      real backazi(max_trace),comp1(max_trace),comp2(max_trace)
c--- focal depth
      real sdepth
c--- stations to model
      character*5 stat(max_trace)
c--- counters and pointers and help variablws
      integer i,kl,il,nstat,k,istat,npoint,kfirst
      real x,y,z,window,t0,time,damping,distance(32)
      character*1 liquid
c
c   open and read synt.inp file
c
      ndata=1
c     open(1,file='synt.inp',status='old')
      open(1,file='hyp.out',status='old')
 10   continue
        read(1,'(a)',end=20)data(ndata)
        ndata=ndata+1
        goto 10
  20  continue
      ndata=ndata-1
      close (1)
c
c   open bouchon input file
c
      open(3,file='bouch.inp',status='unknown')
c
c  get model, first count number of layers, assume first line header
c
         kl=0
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: MODEL--:') then
               kl=kl+1
               if(kl.eq.1) kfirst=i
            endif
         enddo
c
c   check if the top layer is liquid or solid
         read(data(kfirst+1)(36:45),'(f10.3)') beta0
         liquid='f'
         if (beta0.eq.0.)  liquid='t'
c
c   write down the model
         write(3,*) kl-1
         il=1
         do i=kfirst+1,kl+kfirst-1
            write(3,'(a)') data(i)(16:79)
            read(data(i)(16:25),'(f10.1)') depth(il)
            il=il+1
         enddo
c
c   read if Radial-Transverse or North-East components
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: COMPON-:') then
            radial=.false.
            if  (data(i)(20:25).eq.'RADIAL') radial=.true.
         endif
      enddo
c
c   find number of stations and stations to model
c
      nstat=0
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: STATION:') 
     *   then
            do k=1,nstat
              if(stat(k).eq.data(i)(17:21)) goto 80
            enddo
c
c   station was not counted before
c
            nstat=nstat+1
            stat(nstat)=data(i)(17:21)
 80         continue
         endif
      enddo
      write(6,*)'Number of stations to model', nstat
      write(3,*) nstat
c
c   get station data
c
       do istat=1,nstat
c
c   find distance for stations, either given by SYNT, or by hyp
c
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3') 
     *      then
               read(data(i)(36:45),'(f10.1)') distance(istat)
            endif
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'AZIMUTH:'.and.data(i)(80:80).eq.'3') 
     *      then
               read(data(i)(36:45),'(f10.1)') azimuth(istat)
               read(data(i)(56:65),'(f10.1)') backazi(istat)
            endif
         enddo
         if (radial)   then
           comp1(istat)= backazi(istat)+180.
           if (comp1(istat).lt.0.  ) comp1(istat)=comp1(istat)+360.
           if (comp1(istat).ge.360.) comp1(istat)=comp1(istat)-360.
           comp2(istat)= comp1(istat)+90.
           if (comp2(istat).lt.0.  ) comp2(istat)=comp2(istat)+360.
           if (comp2(istat).ge.360.) comp2(istat)=comp2(istat)-360.
         else
           comp1(istat)= 0.
           comp2(istat)= 90.
         endif
      enddo
         do i=1, nstat
            write(3,*) distance(i),azimuth(i),comp1(i),comp2(i)
         enddo
c
c   find depth and place source in layer
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: DEPTH--:') then
               read(data(i)(16:25),'(f10.1)') sdepth
            endif
         enddo
c
c   find which layer
c
         y=0.0
         do i=1,kl-1
           y=y+depth(i)
           if(y.ge.sdepth) then
              il=i
c VM          sdepth=y-sdepth
              sdepth=sdepth-(y-depth(i))
              goto 180
           endif
         enddo
c
c   if here, depth must be in lowest layer, not allowed !!!
c
         write(6,*)' Source depth in halfspce, not allowed'
         stop
 180     continue
c
c   get strike slip and dip         
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: ST-D-RK:') then
               read(data(i),'(15x,3(f10.1))') strike,dip,rake
            endif
         enddo
c
c   convert aki rake to bouchon rake
c
cold         rake=180.0-rake
         rake=rake+180.0
         if(rake.gt.180.0)  rake=rake-360.0
         if(rake.lt.-180.0) rake=rake+360.0
         write(3,*)il,sdepth,strike,dip,rake
c
c   duration, damping, number of points and receiver to convert on
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: NPOINTS:') then
               read(data(i),'(15x,i10)') npoint
            endif
c           if(data(i)(2:15).eq.'SYNT: DAMPING:') then
c              read(data(i),'(15x,f10.1)') damping
c           endif
            if(data(i)(2:15).eq.'SYNT: TIMES--:') then
               read(data(i),'(15x,3(10x,f10.1))') window,t0,time
            endif
          enddo
          k=1
c
c   now automatically put damping equal to time window
c
          damping=time
          write(3,*) time,damping,npoint,k
c
c   boupar
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: BOUPAR-:') then
               read(data(i),'(15x,f10.1,i10,f10.1)') x,k,z
            endif
c           if(data(i)(2:15).eq.'SYNT: LIQUID-:') then
c              read(data(i),'(24x,a1)') liquid
c           endif
          
          enddo
          write(3,*) x,k,z
          write(3,'(a1)') liquid
          write(3,'(a1)')'0'
          write(3,'(a3)')'fin'
          close (3)
      return
      end
c----------------------------------

      SUBROUTINE FF01AD(VJ0,VY0,XD,N)
C  STANDARD FORTRAN 66(A VERIFIED PFORT SUBROUTINE)
      DOUBLE PRECISION VJ0,VY0,X,Y,Z,Q1,Q2,Q3,FX,X1,X2,X3,
     1                 X4,XD,XLG,A,B,C,D,E
      DIMENSION A(73),B(18),C(19),D(18),E(18)
      EQUIVALENCE (A(1),B(1)),(A(19),C(1)),(A(38),D(1)),(A(56),E(1))
      DATA XLG /1.0D+70/
      DATA B(1),B(2),B(3),B(4),B(5),B(6),B(7),B(8),B(9),B(10),B(11),
     1     B(12),B(13),B(14),B(15),B(16),B(17),B(18)    /
     1	 -.17D-18		   , .1222D-16		   ,
     2   -.75885D-15               , .4125321D-13          ,
     3   -.194383469D-11           , .7848696314D-10       ,
     4   -.267925353056D-8         , .7608163592419D-7     ,
     5   -.176194690776215D-5      , .3246032882100508D-4  ,
     6   -.46062616620627505D-3    , .48191800694676045D-2 ,
     7   -.34893769411408885D-1    , .15806710233209726D0  ,
     8   -.37009499387264978D0     , .26517861320333681D0  ,
     9	 -.87234423528522213D-2    , .31545594294978024D0  /
      DATA C(1),C(2),C(3),C(4),C(5),C(6),C(7),C(8),C(9),C(10),C(11),
     1     C(12),C(13),C(14),C(15),C(16),C(17),C(18),C(19)    /
     A	 -.1D-19		   , .39D-18		   ,
     B	 -.2698D-16		   , .164349D-14	   ,
     C   -.8747341D-13             , .402633082D-11        ,
     D	 -.15837552542D-9	   , .524879478733D-8	   ,
     E   -.14407233274019D-6       , .32065325376548D-5    ,
     F   -.5632079141056987D-4     , .75311359325777423D-3 ,
     G	 -.72879624795520792D-2    , .47196689595763387D-1 ,
     H   -.17730201278114358D0     , .26156734625504664D0  ,
     I    .17903431407718266D0     ,-.27447430552974527D0  ,
     J   -.66292226406569883D-1     /
      DATA D(1),D(2),D(3),D(4),D(5),D(6),D(7),D(8),D(9),D(10),D(11),
     1     D(12),D(13),D(14),D(15),D(16),D(17),D(18)    /
     K   -.1D-19                   , .2D-19                ,
     L	 -.11D-18		   , .55D-18		   ,
     M	 -.288D-17		   , .1631D-16		   ,
     N   -.10012D-15               , .67481D-15            ,
     O   -.506903D-14              , .4326596D-13          ,
     O   -.43045789D-12            , .516826239D-11        ,
     P   -.7864091377D-10          , .163064646352D-8      ,
     Q   -.5170594537606D-7        , .307518478751947D-5   ,
     R   -.53652204681321174D-3    , .19989206986950373D1 /
      DATA E(1),E(2),E(3),E(4),E(5),E(6),E(7),E(8),E(9),E(10),E(11),
     1     E(12),E(13),E(14),E(15),E(16),E(17),E(18)   /
     S	  .1D-19		   ,-.3D-19		   ,
     T    .13D-18                  ,-.62D-18               ,
     U	  .311D-17		   ,-.1669D-16		   ,
     V	  .9662D-16		   ,-.60999D-15 	   ,
     W	  .425523D-14		   ,-.3336328D-13	   ,
     X    .30061451D-12            ,-.320674742D-11        ,
     Y	  .4220121905D-10	   ,-.72719159369D-9	   ,
     Z    .1797245724797D-7        ,-.74144984110606D-6    ,
     1    .683851994261165D-4      ,-.31111709210674018D-1 /
      X=XD
      Y=DABS(X)
      Z=Y*.125D0
      IF(Z.LE.1.0D0)GO TO 10
      Z=1.0D0/Z
      X2=4.0D0*Z*Z-2.0D0
      N1=38
      N2=55
	 GO TO 70
   10 IF(Z .EQ. 0.0D0)GO TO  78
      X2=4.0D0*Z*Z-2.0D0
      N1=1
      N2=18
   70 DO 80 J=1,2
      Q3=0.0D0
      Q2=0.0D0
      DO 40  I=N1,N2
      Q1=Q2
      Q2=Q3
   40 Q3=X2*Q2-Q1+A(I)
      FX=(Q3-Q1)*.5D0
      IF(N1-19)50,51,52
   50 VJ0=FX
      IF(N .LE. 0)GO TO 75
      N1=19
      N2=37
      GO TO 80
   52 IF(N1.EQ.56)GO TO 53
      X1=FX
      N1=56
      N2=73
   80 CONTINUE
   78 VJ0=1.0D0
      VY0=-XLG
      GO TO 75
   51 VY0=.6366197723675813D0*DLOG(Y)*VJ0+FX
      GO TO 75
   53 X2=DCOS(Y-.7853981633974483D0)
      X3=DSIN(Y-.7853981633974483D0)
      X4=.7978845608028654D0/DSQRT(Y)
      FX=FX*Z
      VJ0=X4*(X1*X2-FX*X3)
      VY0=X4*(FX*X2+X1*X3)
   75 RETURN
      END
C/ ADD NAME=FF02AD          HSL     F77     DOUBLE
C######DATE   01 JAN 1984 COPYRIGHT UKAEA, HARWELL.
C######ALIAS FF02AD
      SUBROUTINE FF02AD(VJ1,VY1,XD,N)
C  STANDARD FORTRAN 66(A VERIFIED PFORT SUBROUTINE)
      DOUBLE PRECISION VJ1,VY1,X,Y,Z,Q1,Q2,Q3,FX,X1,X2,X3,X4,
     1                 XD,XLG,A,B,C,D,E
      DIMENSION A(72),B(18),C(18),D(18),E(18)
      EQUIVALENCE (A(1),B(1)),(A(19),C(1)),(A(37),D(1)),(A(55),E(1))
      DATA XLG/1.0D+70 /
      DATA B(1),B(2),B(3),B(4),B(5),B(6),B(7),B(8),B(9),B(10),B(11),
     1     B(12),B(13),B(14),B(15),B(16),B(17),B(18)   /
     1	 -.4D-19		   , .295D-17		   ,
     2   -.19554D-15               , .1138572D-13          ,
     3	 -. 57774042D-12	   , .2528123664D-10	   ,
     4	 -.94242129816D-9	   , .2949707007278D-7	   ,
     5   -.76175878054003D-6       , .1588701923993213D-4  ,
     6   -.26044438934858068D-3    , .32402701826838575D-2 ,
     7	 -.29175524806154208D-1    , .17770911723972828D0  ,
     8   -.66144393413454325D0     , .12879940988576776D1  ,
     9   -.11918011605412169D1     , .12967175412105298D1  /
      DATA C(1),C(2),C(3),C(4),C(5),C(6),C(7),C(8),C(9),C(10),C(11),
     1     C(12),C(13),C(14),C(15),C(16),C(17),C(18)   /
     A    .9D-19 ,-.658D-17              ,
     B    .42773D-15               ,-.2440949D-13          ,
     C	  .121143321D-11	   ,-.5172121473D-10	   ,
     D    .187547032473D-8         ,-.5688440039919D-7     ,
     E    .141662436449235D-5      ,-.283046401495148D-4   ,
     F	  .44047862986709951D-3    ,-.51316411610610848D-2 ,
     G    .42319180353336904D-1    ,-.22662499155675492D0  ,
     H    .67561578077218767D0     ,-.76729636288664594D0  ,
     I   -.12869738438135000D0     , .40608211771868508D-1 /
      DATA D(1),D(2),D(3),D(4),D(5),D(6),D(7),D(8),D(9),D(10),D(11),
     1     D(12),D(13),D(14),D(15),D(16),D(17),D(18)   /
     J    .1D-19                   ,-.2D-19                ,
     K    .12D-18                  ,-.58D-18               ,
     L	  .305D-17		   ,-.1731D-16		   ,
     M    .10668D-15               ,-.72212D-15            ,
     N	  .545267D-14		   ,-.4684224D-13	   ,
     O    .46991955D-12            ,-.570486364D-11        ,
     P    .881689866D-10           ,-.187189074911D-8      ,
     Q	  .6177633960644D-7	   , -.398728430048891D-5  ,
     R    .89898983308594085D-3    , .20018060817200274D1  /
      DATA E(1),E(2),E(3),E(4),E(5),E(6),E(7),E(8),E(9),E(10),E(11),
     1	   E(12),E(13),E(14),E(15),E(16),E(17),E(18)   /
     S   -.1D-19, .3D-19                ,
     T	 -.14D-18		   , .65D-18		   ,
     U   -.328D-17                 , .1768D-16             ,
     V   -.10269D-15               , .65083D-15            ,
     W	 -.456125D-14		   , .3596777D-13	   ,
     X   -.32643157D-12            , .351521879D-11        ,
     Y   -.4686363688D-10          , .82291933277D-9       ,
     Z   -.2095978138408D-7        , .91386152579555D-6    ,
     1   -.9627723549157079D-4     , .93555574139070650D-1 /
      X=XD
      Y=DABS(X)
      Z=Y*.125D0
      IF(Z.LE.1.0D0)GO TO 10
      Z=1.0D0/Z
      X2=4.0D0*Z*Z-2.0D0
      N1=37
      N2=54
      GO TO 70
   10 IF(Z .LE. 0.0D0)GO TO 78
      X2=4.0D0*Z*Z-2.0D0
      N1=1
      N2=18
   70 DO 80 J=1,2
      Q3=0.0D0
      Q2=0.0D0
      DO 40 I=N1,N2
      Q1=Q2
      Q2=Q3
      Q3=X2*Q2-Q1+A(I)
   40 CONTINUE
      FX=(Q3-Q1)*.5D0
      IF(N1-19)50,51,52
   50 VJ1=FX*Z
      IF(N.LE.0)GO TO 75
      N1=19
      N2=36
      GO TO 80
   52 IF(N1.EQ.55)GO TO 53
      X1=FX 
      N1=55
      N2=72
   80 CONTINUE
   78 VJ1=0.0D0
      VY1=-XLG
      GO TO 75
   51 VY1=.6366197723675813D0*(DLOG(Y)*VJ1-1.0D0/Y)+FX*Z
      GO TO 75
   53 X2=DCOS(Y-2.356194490192345D0)
      X3=DSIN(Y-2.356194490192345D0)
      X4=.7978845608028654D0/DSQRT(Y)
      FX=FX*Z
      VJ1=X4*(X1*X2-FX*X3)
      VY1=X4*(FX*X2+X1*X3)
   75 RETURN
      END
