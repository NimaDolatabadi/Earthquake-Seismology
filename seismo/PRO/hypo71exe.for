C------------------ EVP6.FOR -------------------------------------------
C This version of HYPO71 is as unadulterated as possible except for
C turning it into a subroutine. Opening and closing of channels :
C	5       input .station list, velocity structure, phase data
C       6       full output
C RHN  18/7/90
C somebody's mod between lines 819 & 820 was altered by T Turbitt
C 1 Oct 1991s to remove bug whereby all downward going rays
C were listed in output file with AIN of 180 deg. Corrected (hopefully!)
C by checking for sines .lt. -1.0 rather than .lt. 0.0 before resetting
C to -.99999 rather than 0.000001
C
C jan 1992 tt trapped a zero divide at line 534 by setting sqt = .0001
C
C
C
C-------------------------------------------------------------------
      PROGRAM HYPO71
C------- PROGRAM* HYPO71 (DEC. 21, 1971* REVISED NOV. 25, 1973) --------   1.
      INTEGER*2 SYM                                                        2.
      REAL*8 TIME1,TIME2                                                   3.
      REAL LAT,LON,LAT2,LON2,LATEP,LONEP,MAG,LATR,LONR                     4.
      COMMON /A3/ NRES(2,151),NXM(151),NFM(151),SR(2,151),SRSQ(2,151),     5.
     1       SRWT(2,151),SXM(151),SXMSQ(151),SFM(151),SFMSQ(151),QNO(4)    6.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40)    7.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101)   8.
      COMMON /A7/ KP,KZ,KOUT,WT(101),Y(4),SE(4),XMEAN(4),CP(180),SP(180)   9.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,      10.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)       11.
      COMMON /A12/ MSTA(101),PRMK(101),W(101),JMIN(101),P(101),           12.
     1       RMK(101),WRK(101),TP(101),DT(101),COSL(701)                  13.
      COMMON /A14/ MBK,MDOL,BLANK,MSTAR,DOT,STAR4,QUES,CRMK,MCENT,ISTAR   14.
      COMMON /A15/ M,L,J,ORG,JAV,PMIN,AZRES(101),NEAR,IDXS,LATEP,LONEP    15.
      COMMON /A17/ TIME1,TIME2,LATR,LONR,KTEST,KAZ,KSORT,KSEL,XFN         16.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)     17.
      COMMON /A21/ KSMP(151),FMO,ONF,B(4),IPH,KF,AVRPS,IEXIT              18.
      COMMON /A23/ AIN(101),RMS,ADJ,SYM(101)                              19.
      COMMON /A25/ INS(151),IEW(151),JPH                                  20.
C-----------------------------------------------------------------------  21.
C     INITIALISE SOME VARIABLES
      DO 1 I=1,101
      CAL(I)=0
    1 CONTINUE
C------- SET UP SINE + COSINE TABLES FOR CALCULATING DISTANCES ---------  28.
      DO 10 I=1,180                                                       29.
      PI=I*.0349066                                                       30.
      CP(I)=COS(PI)                                                       31.
   10 SP(I)=SIN(PI)                                                       32.
      DO 20 I=1,701                                                       33.
   20 COSL(I)=COS((I-1)*.0017453)                                         34.
   30 M=0                                                                 35.
C------- INPUT STATION LIST, CRUSTAL MODEL, + CONTROL CARD -------------  36.
   40 CALL INPUT1                                                         37.
      IF(IPUN .EQ. 0) GO TO 44                                            38.
      WRITE(6,41) INS(1),IEW(1)                                           39.
   41 FORMAT(' DATE    ORIGIN    LAT ',A1,'    LONG ',A1,'    DEPTH    M  40.
     1AG NO GAP DMIN  RMS  ERH  ERZ QM')                                  41.
C-------- INITIALIZE SUMMARY OF RESIDUALS ------------------------------  42.
   44 DO 48 L=1,NS                                                        43.
      NRES(1,L)=0                                                         44.
      NRES(2,L)=0                                                         45.
      NXM(L)=0                                                            46.
      NFM(L)=0                                                            47.
      SR(1,L)=0.                                                          48.
      SR(2,L)=0.                                                          49.
      SRSQ(1,L)=0.                                                        50.
      SRSQ(2,L)=0.                                                        51.
      SRWT(1,L)=0.                                                        52.
      SRWT(2,L)=0.                                                        53.
      SXM(L)=0.                                                           54.
      SXMSQ(L)=0.                                                         55.
      SFM(L)=0.                                                           56.
      SFMSQ(L)=0.                                                         57.
   48 CONTINUE                                                            58.
      DO 49 I=1,4                                                         59.
   49 QNO(I)=0.                                                           60.
      XFN=XFAR-XNEAR+0.000001                                             61.
      TIME1=0.D+00                                                        62.
   50 CALL INPUT2                                                         63.
C------- TO PROCESS ONE EARTHQUAKE -------------------------------------  64.
      IF (M .EQ. 1) GO TO 900                                             65.
      IF (NR .GE. 1) GO TO 100                                            66.
      WRITE(6,55)                                                         67.
   55 FORMAT( ///,' ***** EXTRA BLANK CARD ENCOUNTERED *****')            68.
      GO TO 50                                                            69.
  100 CALL SINGLE                                                         70.
      IF (IEXIT .EQ. 1) GO TO 50                                          71.
C------- COMPUTE SUMMARY OF MAGNITUDE RESIDUALS -----------------------   72.
  110 IF (JAV .GT. IQ) GO TO 50                                           73.
      DO 150 I=1,NRP                                                      74.
      IF  (XMAG(I) .EQ. BLANK) GO TO 120                                  75.
      JI=KDX(I)                                                           76.
      DXMAG=XMAG(I)-AVXM                                                  77.
      NXM(JI)=NXM(JI)+1                                                   78.
      SXM(JI)=SXM(JI)+DXMAG                                               79.
      SXMSQ(JI)=SXMSQ(JI)+DXMAG**2                                        80.
  120 IF  (FMAG(I) .EQ. BLANK) GO TO 150                                  81.
      JI=KDX(I)                                                           82.
      DFMAG=FMAG(I)-AVFM                                                  83.
      NFM(JI)=NFM(JI)+1                                                   84.
      SFM(JI)=SFM(JI)+DFMAG                                               85.
      SFMSQ(JI)=SFMSQ(JI)+DFMAG**2                                        86.
  150 CONTINUE                                                            87.
      GO TO 50                                                            88.
  900 CONTINUE                                                            89.
C------- END OF ONE DATA SET* PRINT SUMMARY OF RESIDUALS + RETURN ------  90.
      CALL SUMOUT                                                         91.
      IF (MSTA(NR+1) .EQ. MSTAR) GO TO 30                                 92.
      M=1                                                                 93.
      IF (MSTA(NR+1) .EQ. MDOL) GO TO 40                                  94.
      M=2                                                                 95.
      IF (MSTA(NR+1) .EQ. MCENT) GO TO 40                                 96.

      STOP                                                                97.
      END                                                                 98.


      SUBROUTINE ANSWER(A,S,XMEAN,SIGMA,IDX,PHI,L,M,MM,PF,NDX,ADX)        99.
C------- PRINT INTERMEDIATE RESULTS OF REGRESSION ANALYSIS (SWMREG) ---  100.
      
      REAL*8 ADX                                                         101.
      DIMENSION A(7,7),S(4,4)                                            102.
      DIMENSION XMEAN(1),SIGMA(1),IDX(1),B(4),BSE(4),PF(1)               103.
C----------------------------------------------------------------------- 104.
      DO 410 I=1,MM                                                      105.
      WRITE(6,400) (A(I,J),J=1,MM)                                       106.
  400 FORMAT(7E18.8)                                                     107.
  410 CONTINUE                                                           108.
      FVE=1.-A(M,M)                                                      109.
      B0=XMEAN(M)                                                        110.
  450 YSE=77.7                                                           111.
      IF (PHI .GE. 1) YSE=SIGMA(M)*SQRT(ABS(A(M,M)/PHI))                 112.
      DO  5 I=1,L                                                        113.
      IF (IDX(I).EQ.0) GO TO  5                                          114.
      B(I)=A(I,M)* SQRT(ABS(S(M,M)/S(I,I)))                              115.
      BSE(I)=YSE* SQRT(ABS(A(I+M,I+M)/S(I,I)))                           116.
      B0=B0-B(I)*XMEAN(I)                                                117.
    5 CONTINUE                                                           118.
      WRITE(6,10) ADX,NDX,FVE,YSE,B0                                     119.
   10 FORMAT(/,' VARIABLE ', A8, '................',I5                   120.
     2,      /,' FRACTION OF VARIATION EXPLAINED..',E18.8                121.
     3,      /,' STANDARD ERROR OF Y..............',E18.8                122.
     4,      /,' CONSTANT IN REGRESSION EQUATION..',E18.8)               123.
      WRITE(6,20)                                                        124.
   20 FORMAT(/,' VARIABLE     COEFFICIENT      STANDARD ERROR'           125.
     1,'     PARTIAL F-VALUE')                                           126.
      DO 40 I=1,L                                                        127.
      IF (IDX(I).EQ.0) GO TO 40                                          128.
      WRITE(6,30) I,B(I),BSE(I),PF(I)                                    129.
   30 FORMAT(I5,3E20.6)                                                  130.
   40 CONTINUE                                                           131.
      RETURN                                                             132.
      END                                                                133.
      SUBROUTINE AZWTOS                                                  134.
C------- AZIMUTHAL WEIGHTING OF STATIONS BY QUADRANTS ------------------ 135.
      
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)    136.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101) 137.
      COMMON /A7/ KP,KZ,KOUT,WT(101),Y(4),SE(4),XMEAN(4),CP(180),SP(180) 138.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,     139.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)      140.
      COMMON /A10/ ANIN(101),AZ(101),TEMP(101),CA(71),CB(71)             141.
      COMMON /A13/ JDX(151),LDX(101),KEY(101),CLASS(4)                   142.
      COMMON /A25/ INS(151),IEW(151),JPH                                 143.
      DIMENSION TX(4),TXN(4),KTX(4),KEMP(101)                            144.
      DATA KS1,KW1/'S','W'/                                              145.
C----------------------------------------------------------------------- 146.
      J=0                                                                147.
      DO 10 I=1,NR                                                       148.
      IF (WT(I) .EQ. 0.) GO TO 10                                        149.
      DXI=DX(I)                                                          150.
      DYI=DY(I)                                                          151.
      IF ((DXI.EQ.0.).AND.(DYI.EQ.0.)) GO TO 6                           152.
      JI=KDX(I)                                                          153.
      IF (INS(JI) .EQ. KS1) DYI=-DYI                                     154.
      IF (IEW(JI) .EQ. KW1) DXI=-DXI                                     155.
      AZ(I)=AMOD(ATAN2(DXI,DYI)*57.29578 + 360., 360.)                   156.
      GO TO 7                                                            157.
    6 AZ(I)=999.                                                         158.
    7 J=J+1                                                              159.
      TEMP(J)=AZ(I)                                                      160.
   10 CONTINUE                                                           161.
      CALL SORT(TEMP,KEY,J)                                              162.
      GAP=TEMP(1)+360.-TEMP(J)                                           163.
      IG=1                                                               164.
      DO 20 I=2,J                                                        165.
      DTEMP=TEMP(I)-TEMP(I-1)                                            166.
      IF (DTEMP .LE. GAP) GO TO 20                                       167.
      GAP=DTEMP                                                          168.
      IG=I                                                               169.
   20 CONTINUE                                                           170.
      TX(1)=TEMP(IG)-0.5*GAP                                             171.
      TX(2)=TX(1)+90.                                                    172.
      TX(3)=TX(1)+180.                                                   173.
      TX(4)=TX(1)+270.                                                   174.
      DO 124 I=1,4                                                       175.
      TXN(I)=0.                                                          176.
      IF (TX(I) .LT. 0.) TX(I)=TX(I)+360.                                177.
      IF (TX(I).GT.360.) TX(I)=TX(I)-360.                                178.
  124 CONTINUE                                                           179.
      CALL SORT(TX,KTX,4)                                                180.
      DO 130 I=1,NR                                                      181.
      IF (WT(I) .EQ. 0.) GO TO 130                                       182.
      IF (AZ(I) .GT. TX(1)) GO TO 126                                    183.
  125 TXN(1)=TXN(1)+1.                                                   184.
      KEMP(I)=1                                                          185.
      GO TO 130                                                          186.
  126 IF (AZ(I) .GT. TX(2)) GO TO 127                                    187.
      TXN(2)=TXN(2)+1.                                                   188.
      KEMP(I)=2                                                          189.
      GO TO 130                                                          190.
  127 IF (AZ(I) .GT. TX(3)) GO TO 128                                    191.
      TXN(3)=TXN(3)+1.                                                   192.
      KEMP(I)=3                                                          193.
      GO TO 130                                                          194.
  128 IF (AZ(I) .GT. TX(4)) GO TO 125                                    195.
      TXN(4)=TXN(4)+1.                                                   196.
      KEMP(I)=4                                                          197.
  130 CONTINUE                                                           198.
      XN=4                                                               199.
      IF (TXN(1).EQ.0.) XN=XN-1                                          200.
      IF (TXN(2).EQ.0.) XN=XN-1                                          201.
      IF (TXN(3).EQ.0.) XN=XN-1                                          202.
      IF (TXN(4).EQ.0.) XN=XN-1                                          203.
      FJ=J/XN                                                            204.
      DO 150 I=1,NR                                                      205.
      IF (WT(I) .EQ. 0.) GO TO 150                                       206.
      KI=KEMP(I)                                                         207.
      WT(I)=WT(I)*FJ/TXN(KI)                                             208.
  150 CONTINUE                                                           209.
      RETURN                                                             210.
      END                                                                211.
      BLOCK DATA                                                         212.
C------- INITIALIZE CONSTANTS IN COMMON STATEMENTS --------------------- 213.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101) 214.
      COMMON /A10/ ANIN(101),AZ(101),TEMP(101),CA(71),CB(71)             215.
      COMMON /A13/ JDX(151),LDX(101),KEY(101),CLASS(4)                   216.
      COMMON /A14/ MBK,MDOL,BLANK,MSTAR,DOT,STAR4,QUES,CRMK,MCENT,ISTAR  217.
      COMMON /A22/ F(21,21),G(4,21),H(21),DEPTH(21),IONE                 218.
      COMMON /A24/ FLTEP,IPRO,ISTTT,ISKP(4),AHEAD(12),FLIM,AF(3),NDEC    219.
      DATA CA/  1.855365,1.855369,1.855374,1.855383,1.855396,1.855414,   220.
     1 1.855434,1.855458,1.855487,1.855520,1.855555,1.855595,1.855638,   221.
     2 1.855683,1.855733,1.855786,1.855842,1.855902,1.855966,1.856031,   222.
     3 1.856100,1.856173,1.856248,1.856325,1.856404,1.856488,1.856573,   223.
     4 1.856661,1.856750,1.856843,1.856937,1.857033,1.857132,1.857231,   224.
     5 1.857331,1.857435,1.857538,1.857643,1.857750,1.857858,1.857964,   225.
     6 1.858074,1.858184,1.858294,1.858403,1.858512,1.858623,1.858734,   226.
     7 1.858842,1.858951,1.859061,1.859170,1.859276,1.859384,1.859488,   227.
     8 1.859592,1.859695,1.859798,1.859896,1.859995,1.860094,1.860187,   228.
     9 1.860279,1.860369,1.860459,1.860544,1.860627,1.860709,1.860787,   229.
     A 1.860861,1.860934/                                                230.
      DATA CB/  1.842808,1.842813,1.842830,1.842858,1.842898,1.842950,   231.
     1 1.843011,1.843085,1.843170,1.843265,1.843372,1.843488,1.843617,   232.
     2 1.843755,1.843903,1.844062,1.844230,1.844408,1.844595,1.844792,   233.
     3 1.844998,1.845213,1.845437,1.845668,1.845907,1.846153,1.846408,   234.
     4 1.846670,1.846938,1.847213,1.847495,1.847781,1.848073,1.848372,   235.
     5 1.848673,1.848980,1.849290,1.849605,1.849922,1.850242,1.850565,   236.
     6 1.850890,1.851217,1.851543,1.851873,1.852202,1.852531,1.852860,   237.
     7 1.853188,1.853515,1.853842,1.854165,1.854487,1.854805,1.855122,   238.
     8 1.855433,1.855742,1.856045,1.856345,1.856640,1.856928,1.857212,   239.
     9 1.857490,1.857762,1.858025,1.858283,1.858533,1.858775,1.859008,   240.
     A 1.859235,1.859452/                                                241.
      DATA MBK,DOT,MSTAR,MDOL,MCENT/'    ',' .  ',' ***',' $$$',' ***'/  242.
      DATA ISTTT/' ** '/                                                 243.
      DATA BLANK,STAR4,CLASS/'    ','****','A','B','C','D'/,QUES/'*'/    244.
      DATA LMAX,MMAX,NMAX/21,101,151/,CRMK,ISTAR,IONE/'CAL','*','1   '/  245.
      DATA AHEAD/'    ','    ','    ','    ','    ','    ','    ',       246.
     1 '    ','    ','    ','    ','    '/                               247.
      END                                                                248.
      SUBROUTINE FMPLOT                                                  249.
C------- PLOT FIRST-MOTION DIRECTIONS OF THE LOWER FOCAL HEMISPHERE      250.
C------- IN EQUAL AREA PROJECTION, WHERE C DENOTES COMPRESSION AND       251.
C------- D DENOTES DILATATION ------------------------------------------ 252.
      
      INTEGER*2 GRAPH(95,59),SYM,TEMP                                    253.
      INTEGER*2 BORD,BLANK,PL,CR,DOT,SI,A,B,C,D,E,F,CD,SN,UP             254.
      REAL LAT2,LON2,MAG                                                 255.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40)  256.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,G(4,101),ZSQ,NRP,DF(101) 257.
      COMMON /A7/ KP,KZ,KOUT,WT(101),O(4),SE(4),XMEAN(4),CP(180),SP(180) 258.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,     259.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)      260.
      COMMON /A10/ ANIN(101),AZ(101),OOOO(101),CA(71),CB(71)             261.
      COMMON /A11/ KDATE,KHR,KMIN,SEC,LAT1,LAT2,LON1,LON2,RMK1,RMK2,     262.
     1       IGAP,DMIN,RMSSQ,ERH,Q,QS,QD,ADJSQ,INST,AVR,AAR,NI,KNST,JHR  263.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)    264.
      COMMON /A23/ AIN(101),RMS,ADJ,SYM(101)                             265.
      COMMON /A25/ INS(151),IEW(151),JPH                                 266.
      DATA BORD,BLANK,PL,CR,DOT,SI/'*',' ','+','-','.','I'/              267.
      DATA A,B,C,D,E,F,CD,SN,UP/'A','B','C','D','E','F','X','N','U'/     268.
      DATA NOX,NOY,IX,IY,NOY1,NOX2,NOY2/95,59,39,24,57,48,30/            269.
      DATA RMAX,XSCALE,YSCALE,ADD/3.937008,0.101064,0.169643,4.75/       270.
C----------------------------------------------------------------------- 271.
      NFMR=0                                                             272.
      NO=FNO                                                             273.
      DO 1 I=1,NRP                                                       274.
      IF (SYM(I) .EQ. SN) SYM(I)=BLANK                                   275.
      IF (SYM(I) .EQ. BLANK) GO TO 1                                     276.
      IF (SYM(I) .EQ. UP) SYM(I)=C                                       277.
      NFMR=NFMR+1                                                        278.
    1 CONTINUE                                                           279.
      IF (NFMR .LT. KFM) RETURN                                          280.
      WRITE(6,2) INS(1),IEW(1)                                           281.
    2 FORMAT(1H1,'  DATE    ORIGIN    LAT ',A1,'    LONG ',A1,'    DEPTH 282.
     1    MAG NO GAP DMIN  RMS  ERH  ERZ Q M')                           283.
      WRITE(6,5) KDATE,KHR,KMIN,SEC,LAT1,LAT2,LON1,LON2,RMK1,Z,RMK2      284.
     1,MAG,NO,IGAP,DMIN,RMS,ERH,SE(3),Q,KNO                              285.
    5 FORMAT(2X,I6,1X,2I2,F6.2,I3,'-',F5.2,I4,'-',F5.2,A1,F6.2,A1        286.
     1,F6.2,I3,I4,F5.1,F5.2,2F5.1,1X,A1,1X,I1)                           287.
      DO 10 I=1,NOX                                                      288.
      DO 10 J=1,NOY                                                      289.
   10 GRAPH(I,J)=BLANK                                                   290.
      DO 20 I=1,180                                                      291.
      X=RMAX*CP(I)+ADD                                                   292.
      Y=RMAX*SP(I)+ADD                                                   293.
      JX=X/XSCALE+1.5                                                    294.
      JY=Y/YSCALE+.5                                                     295.
      JY=NOY-JY-1                                                        296.
   20 GRAPH(JX,JY)=BORD                                                  297.
      IT=NOX2-IX-1                                                       298.
      GRAPH(IT,NOY2)=CR                                                  299.
      IT=NOX2+IX+1                                                       300.
      GRAPH(IT,NOY2)=CR                                                  301.
      IT=NOY2-IY-1                                                       302.
      GRAPH(NOX2,IT)=SI                                                  303.
      IT=NOY2+IY+1                                                       304.
      GRAPH(NOX2,IT)=SI                                                  305.
      DO 50 I=1,NRP                                                      306.
      IF (SYM(I) .EQ. BLANK) GO TO 50                                    307.
      IF (AIN(I) .GT. 90.) GO TO 31                                      308.
      ANN=AIN(I)                                                         309.
      AZZ=AZ(I)*.0174533                                                 310.
      GO TO 32                                                           311.
   31 ANN=180.-AIN(I)                                                    312.
      AZZ=(180.+AZ(I))*.0174533                                          313.
   32 R=RMAX*1.414214*SIN(ANN*.0087266)                                  314.
      X=R*SIN(AZZ)+ADD                                                   315.
      Y=R*COS(AZZ)+ADD                                                   316.
      JX=X/XSCALE+1.5                                                    317.
      JY=Y/YSCALE+.5                                                     318.
      JY=NOY-JY-1                                                        319.
      TEMP=GRAPH(JX,JY)                                                  320.
C-----OVER-WRITE TEMP IF IT IS EQUAL TO BLANK,DOT,*,+,OR -               321.
      IF ((TEMP.EQ.BLANK).OR.(TEMP.EQ.BORD).OR.(TEMP.EQ.PL)              322.
     1.OR.(TEMP.EQ.CR).OR.(TEMP.EQ.DOT)) GO TO 47                        323.
C-----TEMP IS OCCUPIED SO IF SYS(I)=+ OR - SKIP THIS STATION             324.
      IF ((SYM(I).EQ.PL).OR.(SYM(I).EQ.CR)) GO TO 50                     325.
      IF (SYM(I) .EQ. C) GO TO 40                                        326.
      IF (GRAPH(JX,JY) .NE. D) GO TO 35                                  327.
      GRAPH(JX,JY)=E                                                     328.
      GO TO 50                                                           329.
   35 IF (GRAPH(JX,JY) .NE. E) GO TO 37                                  330.
      GRAPH(JX,JY)=F                                                     331.
      GO TO 50                                                           332.
   37 IF (GRAPH(JX,JY) .EQ. F) GO TO 50                                  333.
      GRAPH(JX,JY)=CD                                                    334.
      GO TO 50                                                           335.
   40 IF (GRAPH(JX,JY) .NE. C) GO TO 43                                  336.
      GRAPH(JX,JY)=B                                                     337.
      GO TO 50                                                           338.
   43 IF (GRAPH(JX,JY) .NE. B) GO TO 45                                  339.
      GRAPH(JX,JY)=A                                                     340.
      GO TO 50                                                           341.
   45 IF (GRAPH(JX,JY) .EQ. A) GO TO 50                                  342.
      GRAPH(JX,JY)=CD                                                    343.
      GO TO 50                                                           344.
   47 GRAPH(JX,JY)=SYM(I)                                                345.
   50 CONTINUE                                                           346.
      GRAPH(NOX2,NOY2)=BORD                                              347.
      WRITE(6,61)                                                        348.
   61 FORMAT(1H0,67X,'0')                                                349.
      DO 80 I=3,NOY1                                                     350.
      IF (I .EQ .NOY2) GO TO 70                                          351.
      WRITE(6,65) (GRAPH(J,I),J=1,NOX)                                   352.
   65 FORMAT(1H ,20X,95A1)                                               353.
      GO TO 80                                                           354.
   70 WRITE(6,75) (GRAPH(J,I),J=1,NOX)                                   355.
   75 FORMAT(1H ,16X,'270 ',95A1,' 90')                                  356.
   80 CONTINUE                                                           357.
      WRITE(6,85)                                                        358.
   85 FORMAT(67X,'180')                                                  359.
      RETURN                                                             360.
      END                                                                361.
      SUBROUTINE INPUT1                                                  362.
C------- INPUT STATION LIST,CRUSTAL MODEL,AND CONTROL CARD ------------- 363.
      
      INTEGER HEAD/'HEAD'/                                               364.
      REAL*8 TIME1,TIME2                                                 365.
      REAL LAT,LON,LAT2,LON2,LATR,LONR                                   366.
      COMMON /A1/ NSTA(151),DLY(2,151),FMGC(151),XMGC(151),KLAS(151),    367.
     1       PRR(151),CALR(151),ICAL(151),IS(151),NDATE(151),NHRMN(151)  368.
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)    369.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40)  370.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101) 371.
      COMMON /A14/ MBK,MDOL,BLANK,MSTAR,DOT,STAR4,QUES,CRMK,MCENT,ISTAR  372.
      COMMON /A15/ M,L,J,ORG,JAV,PMIN,AZRES(101),NEAR,IDXS,LATEP,LONEP   373.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW    374.
      COMMON /A17/ TIME1,TIME2,LATR,LONR,KTEST,KAZ,KSORT,KSEL,XFN        375.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)    376.
      COMMON /A20/ V(21),D(21),VSQ(21),THK(21),TID(21,21),DID(21,21)     377.
      COMMON /A22/ F(21,21),G(4,21),H(21),DEPTH(21),IONE                 378.
      COMMON /A24/ FLTEP,IPRO,ISTTT,ISKP(4),AHEAD(12),FLIM,AF(3),NDEC    379.
      COMMON /A25/ INS(151),IEW(151),JPH                                 380.
      DIMENSION BHEAD(12), ATEST(15)                                     381.
      DATA IB1,KN1,KW1/' ','N','W'/                                      382.
C----------------------------------------------------------------------- 383.
      DO 350 I=1,15                                                      384.
      ATEST(I) = 1.23456                                                 385.
  350 CONTINUE                                                           386.
      WRITE(6,300)                                                       387.
  300 FORMAT(1H1)                                                        388.
      IF (M-1) 1,100,200                                                 389.
C------- INITIALIZE TEST VARIABLES ------------------------------------- 390.
    1 TEST(1)=0.10                                                       391.
      TEST(2)=10.                                                        392.
      TEST(3)=2.                                                         393.
      TEST(4)=0.05                                                       394.
      TEST(5)=5.                                                         395.
      TEST(6)= 4.                                                        396.
      TEST(7)=-0.87                                                      397.
      TEST(8)=+2.00                                                      398.
      TEST(9)=+0.0035                                                    399.
      TEST(10)=100.                                                      400.
      TEST(11)=8.0                                                       401.
      TEST(12)=0.5                                                       402.
      TEST(13)= 1.                                                       403.
      IFLAG=0                                                            404.
C------- INPUT RESET TEST-VARIABLE CARDS AND SELECTION CARD ------------ 405.
      DO 5 I=1,16                                                        406.
      READ(5,4) ISW,J,  TESTJ,BHEAD                                      407.
    4 FORMAT(A4,T12, I2,T16,F9.4,12A4)                                   408.
   11 IF ((ISW.EQ.MBK).OR.(ISW.EQ.IONE)) GO TO 6                         409.
      IF(ISW .NE. HEAD) GO TO 12                                         410.
      DO 13 II=1,12                                                      411.
      AHEAD(II)= BHEAD(II)                                               412.
   13 CONTINUE                                                           413.
      GO TO 5                                                            414.
   12 IFLAG=1                                                            415.
      ATEST(J)=TESTJ                                                     416.
    5 CONTINUE                                                           417.
    6 WRITE(6,14) AHEAD                                                  418.
   14 FORMAT(40X,12A4)                                                   419.
      WRITE(6,2)                                                         420.
    2 FORMAT(///,' ********** PROGRAM* HYPO71 REVISED (11/25/73) ******* 421.
     1',    ///,13X,'TEST(1)  TEST(2)  TEST(3)  TEST(4)  TEST(5)  TEST(6 422.
     2)  TEST(7)  TEST(8)  TEST(9) TEST(10) TEST(11) TEST(12) TEST(13)') 423.
      WRITE(6,3) (TEST(I),I=1,13)                                        424.
    3 FORMAT(' STANDARD ',13F9.4)                                        425.
      IF (IFLAG .EQ. 0) GO TO 8                                          426.
      DO 16 I = 1,15                                                     427.
      IF(ATEST(I) .NE. 1.23456) TEST(I)=ATEST(I)                         428.
   16 CONTINUE                                                           429.
      WRITE(6,7) (TEST(I),I=1,13)                                        430.
    7 FORMAT(' RESET TO ',13F9.4)                                        431.
C------- SQUARE SOME TEST-VARIABLES FOR LATER USE ---------------------- 432.
    8 TEST(1)=TEST(1)**2                                                 433.
      TEST(2)=TEST(2)**2                                                 434.
      TEST(4)=TEST(4)**2                                                 435.
C------- INPUT STATION LIST -------------------------------------------- 436.
      IF (ISW .EQ. IONE) GO TO 10                                        437.
      KNO=1                                                              438.
      WRITE(6,9)                                                         439.
    9 FORMAT(/,4X,'L     STN     LAT     LONG  ','  ELV DELAY',5X        440.
     1,'FMGC  XMGC KL  PRR  CALR IC      DATE HRMN')                     441.
      GO TO 20                                                           442.
   10 WRITE(6,15)                                                        443.
   15 FORMAT(/,4X,'L   STN    LAT      LONG      ELV     M  DLY1  DLY2', 444.
     1'  XMGC FMGC KL CALR IC   DATE HRMN')                              445.
   20 DO 50 L=1,NMAX                                                     446.
      IF (ISW .EQ. IONE) GO TO 30                                        447.
      READ(5,25)IW(L),NSTA(L),LAT1,LAT2,INS(L),LON1,LON2,IEW(L),IELV(L)  448.
     1,DLY(1,L),FMGC(L),XMGC(L),KLAS(L),PRR(L),CALR(L),ICAL(L),NDATE(L)  449.
     2,NHRMN(L)                                                          450.
   25 FORMAT(1X,A1,A4,I2,F5.2,A1,I3,F5.2,A1,I4,F6.2,4X,F5.2,2X,F5.2,1X   451.
     1,I1,F5.2,F7.2,1X,I1,5X,I6,I4)                                      452.
      IF (NSTA(L) .EQ. MBK) GO TO 60                                     453.
      IF (INS(L) .EQ. IB1) INS(L)=KN1                                    454.
      IF (IEW(L) .EQ. IB1) IEW(L)=KW1                                    455.
      WRITE(6,26) L,IW(L),NSTA(L),LAT1,LAT2,INS(L),LON1,LON2,IEW(L)      456.
     1,IELV(L),DLY(1,L),FMGC(L),XMGC(L),KLAS(L),PRR(L),CALR(L),ICAL(L)   457.
     2,NDATE(L),NHRMN(L)                                                 458.
   26 FORMAT(I5,3X,A1,A4,I3,F5.2,A1,I4,F5.2,A1,I5,F6.2,4X,F5.2,2X,F5.2   459.
     1,1X,I1,F5.2,F7.2,1X,I1,5X,I6,I4)                                   460.
      GO TO 40                                                           461.
   30 READ(5,35)NSTA(L),IW(L),LAT1,LAT2,INS(L),LON1,LON2,IEW(L),IELV(L)  462.
     1,MNO(L),DLY(1,L),DLY(2,L),XMGC(L),FMGC(L),KLAS(L),CALR(L),ICAL(L)  463.
     2,NDATE(L),NHRMN(L)                                                 464.
   35 FORMAT(A4,A1,I2,1X,F5.2,A1,I3,1X,F5.2,A1,I4,5X,I1                  465.
     1,4F6.2,1X,I1,F6.2,1X,I1,2X,I6,I4)                                  466.
      IF (NSTA(L) .EQ. MBK) GO TO 60                                     467.
      IF (INS(L) .EQ. IB1) INS(L)=KN1                                    468.
      IF (IEW(L) .EQ. IB1) IEW(L)=KW1                                    469.
      WRITE(6,36) L,NSTA(L),IW(L),LAT1,LAT2,INS(L),LON1,LON2,IEW(L)      470.
     1,IELV(L),MNO(L),DLY(1,L),DLY(2,L),XMGC(L),FMGC(L),KLAS(L),CALR(L)  471.
     2,ICAL(L),NDATE(L),NHRMN(L)                                         472.
   36 FORMAT(I5,2X,A4,A1,I2,1X,F5.2,A1,I4,1X,F5.2,A1,I5,5X,I1            473.
     1,4F6.2,1X,I1,F6.2,1X,I1,2X,I6,I4)                                  474.
      PRR(L)=0.                                                          475.
   40 LAT(L)=60.*LAT1+LAT2                                               476.
      LON(L)=60.*LON1+LON2                                               477.
      MDATE(L)=NDATE(L)                                                  478.
      MHRMN(L)=NHRMN(L)                                                  479.
      KLSS(L)=KLAS(L)                                                    480.
      CALS(L)=CALR(L)                                                    481.
   50 CONTINUE                                                           482.
      WRITE(6,55)                                                        483.
   55 FORMAT(///,' ***** ERROR* STATION LIST EXCEEDS ARRAY DIMENSION')   484.
      RETURN                                                             485.
   60 NS=L-1                                                             486.
C------- INPUT CRUSTAL MODEL ------------------------------------------- 487.
  100 WRITE(6,105)                                                       488.
  105 FORMAT(///,7X,'CRUSTAL MODEL 1',/,5X,'VELOCITY     DEPTH')         489.
      DO 130 L=1,LMAX                                                    490.
      READ(5,115) V(L),D(L)                                              491.
  115 FORMAT(2F7.3)                                                      492.
      IF (V(L) .LT. 0.01) GO TO 140                                      493.
      WRITE(6,125) V(L),D(L)                                             494.
  125 FORMAT(3X,2F10.3)                                                  495.
      DEPTH(L)=D(L)                                                      496.
      VSQ(L)=V(L)**2                                                     497.
  130 CONTINUE                                                           498.
      WRITE(6,135)                                                       499.
  135 FORMAT(///,' ***** ERROR* CRUSTAL MODEL EXCEEDS ARRAY DIMENSION')  500.
      RETURN                                                             501.
  140 NL=L-1                                                             502.
      N1=NL-1                                                            503.
C-----LAYER THICKNESS THK,F + G TERMS                                    504.
      DO 145 L=1,N1                                                      505.
      THK(L)=D(L+1)-D(L)                                                 506.
  145 H(L)=THK(L)                                                        507.
C---- COMPUTE TID AND DID                                                508.
      DO 150 J=1,NL                                                      509.
      G(1,J)=SQRT(ABS(VSQ(J)-VSQ(1)))/(V(1)*V(J))                        510.
      G(2,J)=SQRT(ABS(VSQ(J)-VSQ(2)))/(V(2)*V(J))                        511.
      G(3,J)=V(1)/SQRT(ABS(VSQ(J)-VSQ(1))+0.000001)                      512.
      G(4,J)=V(2)/SQRT(ABS(VSQ(J)-VSQ(2))+0.000001)                      513.
      IF (J .LE. 1) G(1,J)=0.                                            514.
      IF (J .LE. 2) G(2,J)=0.                                            515.
      IF (J .LE. 1) G(3,J)=0.                                            516.
      IF (J .LE. 2) G(4,J)=0.                                            517.
      DO 150 L=1,NL                                                      518.
      F(L,J)=1.                                                          519.
      IF (L .GE. J) F(L,J)=2.                                            520.
  150 CONTINUE                                                           521.
      DO 165 J=1,NL                                                      522.
      DO 165 M=1,NL                                                      523.
      TID(J,M)=0.                                                        524.
  165 DID(J,M)=0.                                                        525.
      DO 170 J=1,NL                                                      526.
      DO 170 M=J,NL                                                      527.
      IF (M .EQ. 1) GO TO 170                                            528.
      M1=M-1                                                             529.
      DO 160 L=1,M1                                                      530.
      SQT=SQRT(  abs (VSQ(M)-VSQ(L)) )                                   531.
      if(sqt .eq. 0) sqt = .0001      ! tt jan 1992
      TIM=THK(L)*SQT/(V(L)*V(M))                                         532.
      DIM=THK(L)*V(L)/SQT                                                533.
      TID(J,M)=TID(J,M)+F(L,J)*TIM                                       534.
  160 DID(J,M)=DID(J,M)+F(L,J)*DIM                                       535.
  170 CONTINUE                                                           536.
      IF (ISW .NE. IONE) GO TO 200                                       537.
C---- VARIABLE FIRST LAYER                                               538.
      VC=V(1)*V(2)/SQRT(VSQ(2)-VSQ(1))                                   539.
      DO 180 I=1,NS                                                      540.
      FLT(1,I)=DLY(1,I)*VC+D(2)                                          541.
  180 FLT(2,I)=DLY(2,I)*VC+D(2)                                          542.
C------- INPUT CONTROL CARD -------------------------------------------- 543.
  200 WRITE(6,205)                                                       544.
  205 FORMAT(///,' ZTR XNEAR XFAR  POS   IQ  KMS  KFM IPUN IMAG   IR'    545.
     1,' IPRN CODE   LATR      LONR')                                    546.
      READ(5,215) ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,IPRN        547.
     1,KTEST,KAZ,KSORT,KSEL,LAT1,LAT2,LON1,LON2                          548.
  215 FORMAT(3F5.0,F5.2,7I5,1X,4I1,2(I4,F6.2))                           549.
      WRITE(6,215) ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,IPRN       550.
     1,KTEST,KAZ,KSORT,KSEL,LAT1,LAT2,LON1,LON2                          551.
      LATR=60.*LAT1+LAT2                                                 552.
      LONR=60.*LON1+LON2                                                 553.
      IF (IR .EQ. 0) RETURN                                              554.
      DO 240 I=1,IR                                                      555.
      READ(5,225) (QSPA(I,J),J=1,40)                                     556.
  225 FORMAT(20F4.2)                                                     557.
      WRITE(6,235) I,(QSPA(I,J),J=1,40)                                  558.
  235 FORMAT(/,' QSPA(',I1,')* ',20F5.2,/,10X,20F5.2)                    559.
  240 CONTINUE                                                           560.
      RETURN                                                             561.
      END                                                                562.
      SUBROUTINE INPUT2                                                  563.
C------- INPUT PHASE LIST ---------------------------------------------- 564.
      
      INTEGER*2 SYM                                                      565.
      REAL*8 TIME1,TIME2                                                 566.
      REAL LAT2,LON2,LATEP,LONEP,MAG                                     567.
      COMMON /A1/ NSTA(151),DLY(2,151),FMGC(151),XMGC(151),KLAS(151),    568.
     1       PRR(151),CALR(151),ICAL(151),IS(151),NDATE(151),NHRMN(151)  569.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101) 570.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,     571.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)      572.
      COMMON /A10/ ANIN(101),AZ(101),TEMP(101),CA(71),CB(71)             573.
      COMMON /A11/ KDATE,KHR,KMIN,SEC,LAT1,LAT2,LON1,LON2,RMK1,RMK2,     574.
     1       IGAP,DMIN,RMSSQ,ERH,Q,QS,QD,ADJSQ,INST,AVR,AAR,NI,KNST,JHR  575.
      COMMON /A12/ MSTA(101),PRMK(101),W(101),JMIN(101),P(101),          576.
     1       RMK(101),WRK(101),TP(101),DT(101),COSL(701)                 577.
      COMMON /A13/ JDX(151),LDX(101),KEY(101),CLASS(4)                   578.
      COMMON /A14/ MBK,MDOL,BLANK,MSTAR,DOT,STAR4,QUES,CRMK,MCENT,ISTAR  579.
      COMMON /A15/ M,L,J,ORG,JAV,PMIN,AZRES(101),NEAR,IDXS,LATEP,LONEP   580.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW    581.
      COMMON /A17/ TIME1,TIME2,LATR,LONR,KTEST,KAZ,KSORT,KSEL,XFN        582.
      COMMON /A18/ S(101),SRMK(101),WS(101),TS(101),NOS,QRMK(101)        583.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)    584.
      COMMON /A21/ KSMP(151),FMO,ONF,B(4),IPH,KF,AVRPS,IEXIT             585.
      COMMON /A23/ AIN(101),RMS,ADJ,SYM(101)                             586.
      COMMON /A24/ FLTEP,IPRO,ISTTT,ISKP(4),AHEAD(12),FLIM,AF(3),NDEC    587.
      DIMENSION ICARD(20)                                                588.
C----------------------------------------------------------------------- 589.
   10 PMIN=9999.                                                         590.
      IDXS=0                                                             591.
      DO 20 I=1,NS                                                       592.
      KSMP(I)=0                                                          593.
   20 JDX(I)=0                                                           594.
   25 L=1                                                                595.
   30 READ(5,35,END=300) MSTA(L),PRMK(L),W(L),JTIME,JMIN(L),P(L),S(L)    596.
     1,SRMK(L),WS(L),AMX(L),PRX(L),CALP,CALX(L),RMK(L),DT(L),FMP(L)      597.
     2,AZRES(L),SYM(L),AS,ICARD,QRMK(L),IPRO                             598.
   35 FORMAT(2A4,T8,F1.0,T10,I8,I2,F5.2,T31,F6.2,A4,T40,F1.0,T44,F4.0    599.
     1,F3.2,F4.1,T59,F4.1,A3,F5.2,F5.0,T21,A4,T7,A1,T32,A4,T1,20A4       600.
     2,T63,A1,T5,A4)                                                     601.
      IF ((MSTA(L).EQ.MSTAR).OR.(MSTA(L).EQ.MDOL).OR.(MSTA(L).EQ.MCENT)) 602.
     1GO TO 300                                                          603.
      IF (MSTA(L).EQ.MBK) GO TO 350                                      604.
      IF (CALX(L) .LT. 0.01) CALX(L)=CALP                                605.
      DO 40 I=1,NS                                                       606.
      IF (MSTA(L) .EQ. NSTA(I)) GO TO 50                                 607.
   40 CONTINUE                                                           608.
      WRITE(6,45) ICARD,MSTA(L)                                          609.
   45 FORMAT(///,' ***** ',20A4,' ***** DELETED* ',A4,' NOT ON STATION L 610.
     1IST')                                                              611.
      GO TO 30                                                           612.
   50 KDX(L)=I                                                           613.
      LDX(L)=0                                                           614.
      JDX(I)=1                                                           615.
      IF (FMP(L) .LE. 0.) FMP(L)=BLANK                                   616.
      IF (L .GT. 1) GO TO 60                                             617.
      KTIME=JTIME                                                        618.
      KDATE=KTIME/100                                                    619.
      KHR=KTIME-KDATE*100                                                620.
   60 IF (JTIME .EQ. KTIME) GO TO 70                                     621.
      WRITE(6,65) ICARD                                                  622.
   65 FORMAT(///,' ***** ',20A4,' ***** DELETED* WRONG TIME')            623.
      GO TO 30                                                           624.
   70 IF (RMK(L) .EQ. CRMK) GO TO 200                                    625.
   80 W(L)=(4.-W(L))/4.                                                  626.
      IF (IW(I) .EQ. ISTAR) W(L)=0.                                      627.
      TP(L)=60.*JMIN(L)+P(L)+DT(L)                                       628.
      WRK(L)=BLANK                                                       629.
      IF (W(L) .EQ. 0.) GO TO 90                                         630.
      IF (W(L) .GT. 0.) GO TO 89                                         631.
C------- SMP DATA* RESET WEIGHT ---------------------------------------- 632.
      W(L)=(4.-WS(L))/4.                                                 633.
      KSMP(L)=1                                                          634.
      IF(TP(L).GE.PMIN) GO TO 95                                         635.
      PMIN=TP(L)                                                         636.
      NEAR=L                                                             637.
      GO TO 95                                                           638.
   89 IF (TP(L) .GE. PMIN) GO TO 90                                      639.
      PMIN=TP(L)                                                         640.
      NEAR=L                                                             641.
   90 IF (AS .EQ. BLANK) GO TO 100                                       642.
C------- S DATA -------------------------------------------------------- 643.
      IDXS=1                                                             644.
      LDX(L)=1                                                           645.
      WS(L)=(4.-WS(L))/4.                                                646.
      IF (IW(I) .EQ. ISTAR) WS(L)=0.                                     647.
   95 TS(L)=60.*JMIN(L)+S(L)+DT(L)                                       648.
  100 L=L+1                                                              649.
      IF (L .LT. MMAX) GO TO 30                                          650.
      WRITE(6,105)                                                       651.
  105 FORMAT(///,' ***** ERROR* PHASE LIST EXCEEDS ARRAY DIMENSION* EXTR 652.
     1A DATA TREATED AS NEXT EARTHQUAKE')                                653.
      GO TO 350                                                          654.
C------- CALIBRATION CHANGE IN STATION LIST ---------------------------- 655.
  200 IF (P(L) .NE. 0.) KLAS(I)=P(L)                                     656.
      CALR(I)=CALX(L)                                                    657.
      TIME2=1.D+06*KDATE+1.D+04*KHR+1.D+02*JMIN(L)                       658.
      IF (TIME2 .GE. TIME1) GO TO 250                                    659.
      WRITE(6,205)                                                       660.
  205 FORMAT(///,' ********** THE FOLLOWING EVENT IS OUT OF CHRONOLOGICA 661.
     1L ORDER **********')                                               662.
  250 WRITE(6,255) KDATE,KHR,JMIN(L),MSTA(L),KLAS(I),CALR(I)             663.
  255 FORMAT(///,' ***** ',I6,1X,2I2,' ***** CALIBRATION CHANGE FOR ',A4 664.
     1,'* KLAS = ',I1,', CALR = ',F4.1)                                  665.
      MDATE(I)=KDATE                                                     666.
      MHRMN(I)=100*KHR+JMIN(L)                                           667.
      TIME1=TIME2                                                        668.
      GO TO 10                                                           669.
  300 M=1                                                                670.
      NR=L-1                                                             671.
      RETURN                                                             672.
  350 M=0                                                                673.
  400 NR=L-1                                                             674.
      RETURN                                                             675.
      END                                                                676.
      SUBROUTINE MISING                                                  677.
C------- CHECK MISSING STATIONS ---------------------------------------- 678.
      
      REAL*8 TIME1,TIME2                                                 679.
      REAL LAT,LON,LAT2,LON2,LATEP,LONEP,MAG                             680.
      COMMON /A1/ NSTA(151),DLY(2,151),FMGC(151),XMGC(151),KLAS(151),    681.
     1       PRR(151),CALR(151),ICAL(151),IS(151),NDATE(151),NHRMN(151)  682.
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)    683.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40)  684.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101) 685.
      COMMON /A7/ KP,KZ,KOUT,WT(101),Y(4),SE(4),XMEAN(4),CP(180),SP(180) 686.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,     687.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)      688.
      COMMON /A10/ ANIN(101),AZ(101),TEMP(101),CA(71),CB(71)             689.
      COMMON /A11/ KDATE,KHR,KMIN,SEC,LAT1,LAT2,LON1,LON2,RMK1,RMK2,     690.
     1       IGAP,DMIN,RMSSQ,ERH,Q,QS,QD,ADJSQ,INST,AVR,AAR,NI,KNST,JHR  691.
      COMMON /A12/ MSTA(101),PRMK(101),W(101),JMIN(101),P(101),          692.
     1       RMK(101),WRK(101),TP(101),DT(101),COSL(701)                 693.
      COMMON /A13/ JDX(151),LDX(101),KEY(101),CLASS(4)                   694.
      COMMON /A14/ MBK,MDOL,BLANK,MSTAR,DOT,STAR4,QUES,CRMK,MCENT,ISTAR  695.
      COMMON /A15/ M,L,J,ORG,JAV,PMIN,AZRES(101),NEAR,IDXS,LATEP,LONEP   696.
      COMMON /A25/ INS(151),IEW(151),JPH                                 697.
      DATA KS1,KW1/'S','W'/                                              698.
C----------------------------------------------------------------------- 699.
      IHD=0                                                              700.
      NJ=J+1                                                             701.
      TEMP(NJ)=TEMP(1)+360.                                              702.
      TDEL=25.*MAG**2                                                    703.
      IF (MAG .EQ. BLANK) TDEL=100.                                      704.
      DO 30 I=1,NS                                                       705.
      IF (JDX(I) .EQ. 1) GO TO 30                                        706.
      AVL=(LAT(I)+LATEP)/120.                                            707.
      M1=AVL+1.5                                                         708.
      IF(M1.GT.71)M1=71
      M2=AVL*10.+1.5                                                     709.
      IF(M2.GT.701)M2=701
      DXI=(LON(I)-LONEP)*CA(M1)*COSL(M2)                                 710.
      DYI=(LAT(I)-LATEP)*CB(M1)                                          711.
      DELI=SQRT(DXI**2+DYI**2)+0.000001                                  712.
      IF (DELI .GT. TDEL) GO TO 30                                       713.
C     CHECK LATITUDE AND LONGITUDE                                       714.
      IF (INS(I)  .EQ. KS1) DYI=-DYI                                     715.
      IF (IEW(I)  .EQ. KW1) DXI=-DXI                                     716.
      AZI=AMOD(ATAN2(DXI,DYI)*57.29578 + 360., 360.)                     717.
      IF (AZI .LE. TEMP(1)) AZI=AZI+360.                                 718.
      DO 10 J=2,NJ                                                       719.
      IF (AZI .LT. TEMP(J)) GO TO 20                                     720.
   10 CONTINUE                                                           721.
      J=NJ                                                               722.
   20 EXGAP=TEMP(J)-TEMP(J-1)                                            723.
      RDGAP=TEMP(J)-AZI                                                  724.
      TGAP=AZI-TEMP(J-1)                                                 725.
      IF (TGAP .LT. RDGAP) RDGAP=TGAP                                    726.
      IF ((DELI.GT.DMIN).AND.(RDGAP.LT.30.)) GO TO 30                    727.
      IF (AZI .GE. 360.) AZI=AZI-360.                                    728.
      IF (IHD .EQ. 1) GO TO 22                                           729.
      WRITE(6,5)                                                         730.
    5 FORMAT(/,10X,'MISSING STATION  DELTA   AZIM  EX-GAP  RD-GAP')      731.
      IHD=1                                                              732.
   22 WRITE(6,25) NSTA(I),DELI,AZI,EXGAP,RDGAP                           733.
   25 FORMAT(21X,A4,2F7.1,2F8.1)                                         734.
   30 CONTINUE                                                           735.
      RETURN                                                             736.
      END                                                                737.
      SUBROUTINE OUTPUT                                                  738.
C------- OUTPUT HYPOCENTER --------------------------------------------- 739.
      
      INTEGER*2 SYM                                                      740.
      REAL*8 TIME1,TIME2                                                 741.
      REAL LAT,LON,LAT2,LON2,LATEP,LONEP,MAG                             742.
      COMMON /A1/ NSTA(151),DLY(2,151),FMGC(151),XMGC(151),KLAS(151),    743.
     1       PRR(151),CALR(151),ICAL(151),IS(151),NDATE(151),NHRMN(151)  744.
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)    745.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40)  746.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101) 747.
      COMMON /A7/ KP,KZ,KOUT,WT(101),Y(4),SE(4),XMEAN(4),CP(180),SP(180) 748.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,     749.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)      750.
      COMMON /A10/ ANIN(101),AZ(101),TEMP(101),CA(71),CB(71)             751.
      COMMON /A11/ KDATE,KHR,KMIN,SEC,LAT1,LAT2,LON1,LON2,RMK1,RMK2,     752.
     1       IGAP,DMIN,RMSSQ,ERH,Q,QS,QD,ADJSQ,INST,AVR,AAR,NI,KNST,JHR  753.
      COMMON /A12/ MSTA(101),PRMK(101),W(101),JMIN(101),P(101),          754.
     1       RMK(101),WRK(101),TP(101),DT(101),COSL(701)                 755.
      COMMON /A13/ JDX(151),LDX(101),KEY(101),CLASS(4)                   756.
      COMMON /A14/ MBK,MDOL,BLANK,MSTAR,DOT,STAR4,QUES,CRMK,MCENT,ISTAR  757.
      COMMON /A15/ M,L,J,ORG,JAV,PMIN,AZRES(101),NEAR,IDXS,LATEP,LONEP   758.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW    759.
      COMMON /A17/ TIME1,TIME2,LATR,LONR,KTEST,KAZ,KSORT,KSEL,XFN        760.
      COMMON /A18/ S(101),SRMK(101),WS(101),TS(101),NOS,QRMK(101)        761.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)    762.
      COMMON /A21/ KSMP(151),FMO,ONF,B(4),IPH,KF,AVRPS,IEXIT             763.
      COMMON /A22/ F(21,21),G(4,21),H(21),DEPTH(21),IONE                 764.
      COMMON /A23/ AIN(101),RMS,ADJ,SYM(101)                             765.
      COMMON /A24/ FLTEP,IPRO,ISTTT,ISKP(4),AHEAD(12),FLIM,AF(3),NDEC    766.
      COMMON /A25/ INS(151),IEW(151),JPH                                 767.
c pv > f90 -g  -I../INC -o hypo71exe hypo71exe.for
c        WRITE(6,FMT1,ERR=1234)KDATE,RMKO,KHR,KMIN,SEC,LAT1,SYM1,LAT2,LON1,
c                ^
c "hypo71exe.for", Line = 960, Column = 15: ERROR: The FMT= control list 
citem must be a label, a character expression or an ASSIGN integer variable.
      CHARACTER*4 FMT1(32)
      CHARACTER*4 FMT2(24)
      CHARACTER*4 FMT3(32)
      CHARACTER*4 FMT4(16)
c     DIMENSION FMT1(32),FMT2(24),FMT3(32),FMT4(16)
      DIMENSION DEMP(101),SYMBOL(5)
      DATA FMT1/'(1X,','I6,A','1,2I','2,F6','.2,I','3,A1',',F5.','2,I4', 769.
     1          ',A1,','F5.2',',A1,','F6.2',',A1,','F6.2',',2I3',',I4,', 770.
     2          'I2,F','5.2,','F5.1',',   ','F5.1',',2(1','X,A1','),2A', 771.
     3          '1,F5','.2,2','I3,2','F5.2',',2(I','3,2F','5.1)',',I2)'/ 772.
      DATA FMT2/'(I6,','1X,2','I2,F','6.2,','I3,A','1,F5','.2,I','4,A1', 773.
     1          ',F5.','2,A1',',F6.','2,A1',',   ','F6.2',',I3,','I4,F', 774.
     2          '5.1,','F5.2',',   ','F5.1',',   ','F5.1',',3A1',')   '/ 775.
      DATA FMT3/'(1X,','A4,F','6.1,','2I4,','1X,A','4,1X',',2I2',',4F6', 776.
     1          '.2, ','F6.2',',A2,','F4.2',',I4,','I3,F','6.2,','I2, ', 777.
     2          'F4.1',',A1,','1X,A','3,  ','I4, ','F4.1',',A1,','1X,A', 778.
     3          '4, 3','F6.2',',A2,','F4.2',',   ','F6.2',',T6,','A1) '/ 779.
      DATA FMT4/'(A4,','3F6.','1,1X',',A4,','2F6.','2,F5','.1, ','F6.2', 780.
     1          ',1X,','A3, ','F6.2',',I7,','   2','I2,2','I4,A','1)  '/ 781.
      CHARACTER*4 G1
      CHARACTER*4 G2
      CHARACTER*4 G3
      CHARACTER*4 G4
      CHARACTER*4 F1
      CHARACTER*4 F2
      CHARACTER*4 F4
      CHARACTER*4 F5
      CHARACTER*4 F6
      DATA SYM1,SYM2,F1,F2,G1,G2/'-','*','F6.2','F5.1','A6  ','  A5'/    782.
      DATA F4,F5,F6,G3,G4/'F4.1','I4, ','F4.2','A4  ','A4, '/            783.
      DATA SYMBOL/' ','1','2','Q','*'/,ZDOT/'0.  '/,KS1,KW1/'S','W'/     784.
C----------------------------------------------------------------------- 785.
      IF((IPRN.GE.2).OR.(KP.EQ.1))CALL XFMAGS
      LAT1=LATEP/60.                                                     787.
      LAT2=LATEP-60.*LAT1                                                788.
      LON1=LONEP/60.                                                     789.
      LON2=LONEP-60.*LON1                                                790.
      ADJ=SQRT(ADJSQ)                                                    791.
      RMS=SQRT(RMSSQ)                                                    792.
      JHR=KHR                                                            793.
      OSAVE = ORG                                                        794.
      IF (ORG .GE. 0.) GO TO 5                                           795.
      ORG=ORG+3600.                                                      796.
      KHR=KHR-1                                                          797.
    5 KMIN=ORG/60.0                                                      798.
      SEC=ORG-60.0*KMIN                                                  799.
      ERH=SQRT(SE(1)**2+SE(2)**2)                                        800.
      NO=FNO                                                             801.
      RMK1=BLANK                                                         802.
      RMK2=BLANK                                                         803.
      RMKO=BLANK                                                         804.
C---- KZ=1 FOR FIXED DEPTH* ONF=0 FOR ORIGIN TIME BASED ON SMP'S         805.
      IF (ONF .EQ. 0.) RMKO=STAR4                                        806.
      IF (KZ .EQ. 1) RMK2=STAR4                                          807.
      J=0                                                                808.
      DO 10 I=1,NRP                                                      809.
      DXI=DX(I)                                                          810.
      DYI=DY(I)                                                          811.
      IF ((DXI.EQ.0.).AND.(DYI.EQ.0.)) GO TO 6                           812.
      JI=KDX(I)                                                          813.
      IF (INS(JI) .EQ. KS1) DYI=-DYI                                     814.
      IF (IEW(JI) .EQ. KW1) DXI=-DXI                                     815.
      AZ(I)=AMOD(ATAN2(DXI,DYI)*57.29578 + 360., 360.)                   816.
      GO TO 7                                                            817.
    6 AZ(I)= 999.                                                        818.
    7 CONTINUE                                                           819.
	if (anin (i) .gt. 1.0) anin (i) =  0.999999
	if (anin (i) .lt.-1.0) anin (i) = -0.999999
      AIN(I)=ASIN(ANIN(I))*57.29578                                      820.
      IF (AIN(I) .LT. 0.) AIN(I)=180.+AIN(I)                             821.
      AIN(I)=180.-AIN(I)                                                 822.
      SWT=0.                                                             823.
      IF (LDX(I) .EQ. 0.) GO TO 8                                        824.
      KK=LDX(I)                                                          825.
      SWT=WT(KK)                                                         826.
    8 IF ((WT(I).EQ.0.).AND.(SWT.EQ.0.)) GO TO 10                        827.
      J=J+1                                                              828.
      TEMP(J)=AZ(I)                                                      829.
   10 CONTINUE                                                           830.
      CALL SORT(TEMP,KEY,J)                                              831.
      GAP=TEMP(1)+360.-TEMP(J)                                           832.
      DO 20 I=2,J                                                        833.
      DTEMP=TEMP(I)-TEMP(I-1)                                            834.
      IF (DTEMP .GT. GAP) GAP=DTEMP                                      835.
   20 CONTINUE                                                           836.
      IGAP=GAP+0.5                                                       837.
      DO 25 I=1,NRP                                                      838.
   25 DEMP(I)=DELTA(I)                                                   839.
      CALL SORT(DEMP,KEY,NRP)                                            840.
      DO 27 I=1,NRP                                                      841.
      K=KEY(I)                                                           842.
      SWT=0.                                                             843.
      IF (LDX(K) .EQ. 0.) GO TO 26                                       844.
      KK=LDX(K)                                                          845.
      SWT=WT(KK)                                                         846.
   26 IF ((WT(K).GT.0.).OR.(SWT.GT.0.)) GO TO 28                         847.
   27 CONTINUE                                                           848.
   28 DMIN=DEMP(I)                                                       849.
      IDMIN=DMIN+0.5                                                     850.
      OFD=Z                                                              851.
      TFD=2.*Z                                                           852.
      IF (OFD .LT. 5.) OFD=5.                                            853.
      IF (TFD .LT. 10.) TFD=10.                                          854.
      JS=4                                                               855.
      IF ((RMS.LT.0.50).AND.(ERH.LE.5.0)) JS=3                           856.
      IF ((RMS.LT.0.30).AND.(ERH.LE.2.5).AND.(SE(3).LE.5.0)) JS=2        857.
      IF ((RMS.LT.0.15).AND.(ERH.LE.1.0).AND.(SE(3).LE.2.0)) JS=1        858.
      JD=4                                                               859.
      IF (NO .LT. 6) GO TO 30                                            860.
      IF ((GAP.LE.180.).AND.(DMIN.LE.50.)) JD=3                          861.
      IF ((GAP.LE.135.).AND.(DMIN.LE.TFD)) JD=2                          862.
      IF ((GAP.LE. 90.).AND.(DMIN.LE.OFD)) JD=1                          863.
   30 JAV=(JS+JD+1)/2                                                    864.
      Q=CLASS(JAV)                                                       865.
      QS=CLASS(JS)                                                       866.
      QD=CLASS(JD)                                                       867.
   50 TIME2=SEC+1.D+02*KMIN+1.D+04*KHR+1.D+06*KDATE                      868.
      IF(IPRN .EQ. 0) GO TO 52                                           869.
      IF(NI .NE. 1) GO TO 60                                             870.
      IF(NDEC .GE. 1) GO TO 60                                           871.
      IF (JPH .EQ. 1) GO TO 60                                           872.
   52 KKYR=KDATE/10000                                                   873.
      KKMO=(KDATE-KKYR*10000)/100                                        874.
      KKDAY=(KDATE-KKYR*10000-KKMO*100)                                  875.
      JPH=1                                                              876.
      IF(KSEL) 501,501,505                                               877.
  501 WRITE(6,502)                                                       878.
  502 FORMAT(///)                                                        879.
      GO TO 535                                                          880.
  505 WRITE(6,506)                                                       881.
  506 FORMAT(1H1)                                                        882.
   51 WRITE(6,53) AHEAD,KKYR,KKMO,KKDAY,KHR,KMIN                         883.
   53 FORMAT(/,30X,12A4,T112,I2,'/',I2,'/',I2,4X,I2,'*',I2)              884.
  535 IF( TIME2 - TIME1 .GT. -20.)GO TO 60                               885.
      WRITE(6,54)                                                        886.
   54 FORMAT(' ***** FOLLOWING EVENT IS OUT OF ORDER *****')             887.
   60 IF ((KP.EQ.1) .AND. (IPRN.EQ.0)) GO TO 67                          888.
      IF (IPH .EQ. 1) GO TO 62                                           889.
      WRITE(6,61) INS(1),IEW(1)                                          890.
   61 FORMAT(/,59X,'  ADJUSTMENTS (KM)  PARTIAL F-VALUES  STANDARD ERROR 891.
     1S  ADJUSTMENTS TAKEN',/,'  I  ORIG  LAT ',A1                       892.
     2,'    LONG ',A1,           '   DEPTH  DM  RMS AVRPS SKD   CF   DLA 893.
     3T  DLON    DZ  DLAT  DLON    DZ  DLAT  DLON    DZ  DLAT  DLON    D 894.
     4Z')                                                                895.
      IF (IPRN .EQ. 1) IPH=1                                             896.
C Very minor alteration of adding ERR=1111 to statement below
   62 WRITE(6,63,ERR=1111) NI,SEC,LAT1,LAT2,LON1,LON2,Z,RMK2,IDMIN,RMS,
     1 AVRPS,QS,KF,QD,FLIM,B(2),B(1),B(3),AF(2),AF(1),AF(3),SE(2),SE(1),
     2 SE(3),Y(2),Y(1),Y(3)                                             
   63 FORMAT(I3,F6.2,I3,'-',F5.2,I4,'-',F5.2,F6.2,A1,I3,F5.2,F6.2,       900.
     1 1X,A1,I1,A1,13F6.2)                                               901.
1111  IF (KP .EQ. 0) GO TO 100                                           902.
   67 JNST=KNST*10+INST                                                  903.
      IF (NM .EQ. 0) AVXM=0.                                             904.
      IF (NF .EQ. 0) AVFM=0.                                             905.
      FMT1(14)=F1                                                        906.
      FMT1(19)=F2                                                        907.
      FMT1(21)=F2                                                        908.
      FMT2(14)=F1                                                        909.
      FMT2(20)=F2                                                        910.
      FMT2(22)=F2                                                        911.
      IF (MAG .NE. BLANK) GO TO 68                                       912.
      FMT1(14)=G1                                                        913.
      FMT2(14)=G1                                                        914.
   68 IF (SE(3) .NE. 0.) GO TO 70                                        915.
      SE(3)=BLANK                                                        916.
      FMT1(21)=G2                                                        917.
      FMT2(22)=G2                                                        918.
   70 IF (ERH .NE. 0.) GO TO 72                                          919.
      ERH=BLANK                                                          920.
      FMT1(19)=G2                                                        921.
      FMT2(20)=G2                                                        922.
   72 WRITE(6,75) INS(1),IEW(1)                                          923.
   75 FORMAT(//, '  DATE    ORIGIN    LAT ',A1,'    LONG ',A1,'    DEPTH 924.
     1    MAG NO DM GAP M  RMS  ERH  ERZ Q SQD  ADJ IN NR  AVR  AAR NM A 925.
     2VXM SDXM NF AVFM SDFM I')                                          926.
  909 CONTINUE
      WRITE(6,FMT1,ERR=1234)KDATE,RMKO,KHR,KMIN,SEC,LAT1,SYM1,LAT2,LON1,
     1SYM1,LON2,RMK1,Z,RMK2,MAG,NO,IDMIN,IGAP,KNO,RMS,ERH,SE(3),Q,QS,
     2SYM2,QD,ADJ,JNST,NR,AVR,AAR,NM,AVXM,SDXM,NF,AVFM,SDFM,NI   
1234  IF (IPUN .EQ. 0) GO TO 100                                         930.
      IF ((QRMK(1).NE.SYMBOL(4)).AND.(QRMK(1).NE.SYMBOL(5)))             931.
     1QRMK(1)=SYMBOL(1)                                                  932.
      SYM3=SYMBOL(KNO+1)                                                 933.
      WRITE(6,FMT2) KDATE,KHR,KMIN,SEC,LAT1,SYM1,LAT2,LON1,SYM1,LON2     934.
     1,RMK1,Z,RMK2,MAG,NO,IGAP,DMIN,RMS,ERH,SE(3),QRMK(1),Q,SYM3         935.
  100 IF (KP .EQ. 1) GO TO 105                                           936.
      IF(IPRN .LE. 1) GO TO 300                                          937.
  105 WRITE(6,110)                                                       938.
  110 FORMAT(/,'  STN  DIST AZM AIN PRMK HRMN P-SEC TPOBS TPCAL DLY/H1 P 939.
     1-RES P-WT AMX PRX CALX K XMAG RMK FMP FMAG SRMK S-SEC TSOBS S-RES  940.
     2 S-WT    DT')                                                      941.
      DO 200 I=1,NRP                                                     942.
      K=I                                                                943.
      IF (KSORT .EQ. 1) K=KEY(I)                                         944.
      KJI=KDX(K)                                                         945.
      TPK=TP(K)-ORG                                                      946.
      IF (TPK .LT. 0.) TPK=TPK+3600.                                     947.
      FMT3(10)=F1                                                        948.
      IF ((AZRES(K).NE.DOT).AND.(AZRES(K).NE.BLANK).AND.                 949.
     1(AZRES(K).NE.ZDOT)) GO TO 114                                      950.
      X(4,K)=BLANK                                                       951.
      FMT3(10)=G1                                                        952.
  114 RMK3=BLANK                                                         953.
      IF (XMAG(K) .EQ. BLANK) GO TO 115                                  954.
      IF (ABS(XMAG(K)-AVXM) .GE. 0.5) RMK3=STAR4                         955.
  115 RMK4=BLANK                                                         956.
      IF (FMAG(K) .EQ. BLANK) GO TO 130                                  957.
      IF (ABS(FMAG(K)-AVFM) .GE. 0.5) RMK4=STAR4                         958.
  130 FMT3(17)=F4                                                        959.
      FMT3(21)=F5                                                        960.
      FMT3(22)=F4                                                        961.
      FMT4(8)=F1                                                         962.
      FMT4(11)=F1                                                        963.
      IF (XMAG(K) .NE. BLANK) GO TO 160                                  964.
      FMT3(17)=G3                                                        965.
      FMT4(8)=G1                                                         966.
  160 IF (FMAG(K) .NE. BLANK) GO TO 162                                  967.
      FMT3(21)=G4                                                        968.
      FMT3(22)=G3                                                        969.
      FMT4(11)=G1                                                        970.
  162 FMT3(26)=F1                                                        971.
      FMT3(28)=F6                                                        972.
      IAZ=AZ(K)+0.5                                                      973.
      IAIN=AIN(K)+0.5                                                    974.
      IAMX=AMX(K)                                                        975.
      IPRX=100.*PRX(K)+0.5                                               976.
      IFMP=FMP(K)                                                        977.
      IF (LDX(K) .NE. 0) GO TO 163                                       978.
C-----CHECK FOR SMP DATA                                                 979.
      IF (KSMP(K) .EQ. 0) GO TO 165                                      980.
      SRES=X(4,K)                                                        981.
      RMK5=BLANK                                                         982.
      SWT=11111.                                                         983.
      TSK=S(K)-P(K)                                                      984.
      GO TO 168                                                          985.
  163 KK=LDX(K)                                                          986.
      SRES=X(4,KK)                                                       987.
      RMK5=WRK(KK)                                                       988.
      SWT=WT(KK)                                                         989.
  164 TSK=TS(K)-ORG                                                      990.
      GO TO 168                                                          991.
  165 S(K)=BLANK                                                         992.
      TSK=BLANK                                                          993.
      SRES=BLANK                                                         994.
      RMK5=BLANK                                                         995.
      SWT=BLANK                                                          996.
      FMT3(26)=G1                                                        997.
      FMT3(28)=G3                                                        998.
  168 FMT3(30)=F1                                                        999.
      DLYK=DLY(KNO,KJI)                                                 1000.
      IF (ISW .EQ. IONE) DLYK=FLT(KNO,KJI)                              1001.
      DTK=DT(K)                                                         1002.
      IF (DTK .NE. 0.) GO TO 170                                        1003.
      DTK=BLANK                                                         1004.
      FMT3(30)=G1                                                       1005.
  170 WRITE(6,FMT3,ERR=1235) MSTA(K),DELTA(K),IAZ,IAIN,PRMK(K),JHR,
     1JMIN(K),P(K),TPK,T(K),DLYK,X(4,K),WRK(K),WT(K),IAMX,IPRX,CAL(K)
     2,KLAS(KJI),XMAG(K),RMK3,RMK(K),IFMP,FMAG(K),RMK4,SRMK(K),S(K)
     3,TSK,SRES,RMK5,SWT,DTK,IW(KJI)                               
1235  IF (IPUN .NE. 2) GO TO 200                                        1010.
      ISEC = 100.*SEC                                                   1011.
      WRITE(6,FMT4) MSTA(K),DELTA(K),AZ(K),AIN(K),PRMK(K),TPK,X(4,K)    1012.
     1,WT(K),XMAG(K),RMK(K),FMAG(K),KDATE,KHR,KMIN,ISEC,KJI,SYM3        1013.
  200 CONTINUE                                                          1014.
      IF (IPUN .NE. 2) GO TO 300                                        1015.
      WRITE(6,205)                                                      1016.
  205 FORMAT(' $$$')                                                    1017.
  300 KHR = JHR                                                         1018.
      ORG = OSAVE                                                       1019.
      RETURN                                                            1020.
      END                                                               1021.
      SUBROUTINE SINGLE                                                 1022.
C-------- SOLUTION FOR A SINGLE EARTHQUAKE ---------------------------- 1023.
      
      INTEGER*2 SYM                                                     1024.
      REAL*8 TIME1,TIME2                                                1025.
      REAL LATRT, LONRT, LATSV, LONSV                                   1026.
      REAL LAT,LON,LAT2,LON2,LATEP,LONEP,MAG,LATR,LONR                  1027.
      COMMON /A1/ NSTA(151),DLY(2,151),FMGC(151),XMGC(151),KLAS(151),   1028.
     1       PRR(151),CALR(151),ICAL(151),IS(151),NDATE(151),NHRMN(151) 1029.
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)   1030.
      COMMON /A3/ NRES(2,151),NXM(151),NFM(151),SR(2,151),SRSQ(2,151),  1031.
     1       SRWT(2,151),SXM(151),SXMSQ(151),SFM(151),SFMSQ(151),QNO(4) 1032.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40) 1033.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101)1034.
      COMMON /A7/ KP,KZ,KOUT,WT(101),Y(4),SE(4),XMEAN(4),CP(180),SP(180)1035.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,    1036.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)     1037.
      COMMON /A10/ ANIN(101),AZ(101),TEMP(101),CA(71),CB(71)            1038.
      COMMON /A11/ KDATE,KHR,KMIN,SEC,LAT1,LAT2,LON1,LON2,RMK1,RMK2,    1039.
     1       IGAP,DMIN,RMSSQ,ERH,Q,QS,QD,ADJSQ,INST,AVR,AAR,NI,KNST,JHR 1040.
      COMMON /A12/ MSTA(101),PRMK(101),W(101),JMIN(101),P(101),         1041.
     1       RMK(101),WRK(101),TP(101),DT(101),COSL(701)                1042.
      COMMON /A13/ JDX(151),LDX(101),KEY(101),CLASS(4)                  1043.
      COMMON /A14/ MBK,MDOL,BLANK,MSTAR,DOT,STAR4,QUES,CRMK,MCENT,ISTAR 1044.
      COMMON /A15/ M,L,J,ORG,JAV,PMIN,AZRES(101),NEAR,IDXS,LATEP,LONEP  1045.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW   1046.
      COMMON /A17/ TIME1,TIME2,LATR,LONR,KTEST,KAZ,KSORT,KSEL,XFN       1047.
      COMMON /A18/ S(101),SRMK(101),WS(101),TS(101),NOS,QRMK(101)       1048.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)   1049.
      COMMON /A20/ V(21),D(21),VSQ(21),THK(21),TID(21,21),DID(21,21)    1050.
      COMMON /A21/ KSMP(151),FMO,ONF,B(4),IPH,KF,AVRPS,IEXIT            1051.
      COMMON /A22/ F(21,21),G(4,21),H(21),DEPTH(21),IONE                1052.
      COMMON /A23/ AIN(101),RMS,ADJ,SYM(101)                            1053.
      COMMON /A24/ FLTEP,IPRO,ISTTT,ISKP(4),AHEAD(12),FLIM,AF(3),NDEC   1054.
      COMMON /A25/ INS(151),IEW(151),JPH                                1055.
      DIMENSION SUM(5),YSAVE(4),WF(41),ALZ(10),LA(10),LO(10)            1056.
      DATA WF/.95,0.95,0.95,0.95,0.95,0.95,0.94,0.94,0.94,0.93,         1057.
     1       0.92,0.92,0.91,0.90,0.88,0.87,0.85,0.83,0.80,0.77,         1058.
     2       0.73,0.69,0.64,0.59,0.53,0.47,0.41,0.34,0.28,0.23,         1059.
     3       0.18,0.14,0.11,0.08,0.06,0.04,0.03,0.02,0.01,0.01,0./      1060.
      DATA LA/1,1,1,1,0,0,-1,-1,-1,-1/,                                 1061.
     1     LO/+1,-1,+1,-1,0,0,+1,-1,+1,-1/,                             1062.
     2     ALZ/-1.0,-1.0,+1.0,+1.0,-1.732,+1.732,-1.0,-1.0,+1.0,+1.0/   1063.
C-----------------------------------------------------------------------1064.
      AVRPS = 0.0                                                       1065.
      IEXIT=0                                                           1066.
      LATRT=0.                                                          1067.
      ZRES=P(NR+1)                                                      1068.
      KNST=JMIN(NR+1)/10                                                1069.
      INST=JMIN(NR+1)-KNST*10                                           1070.
      NRP=NR                                                            1071.
   30 IF (IDXS .EQ. 0) GO TO 80                                         1072.
C------- TREAT S DATA BY AUGMENTING P DATA -----------------------------1073.
      NOS=0                                                             1074.
      DO 65 I=1,NRP                                                     1075.
      IF (LDX(I) .EQ. 0) GO TO 65                                       1076.
      NOS=NOS+1                                                         1077.
      NRS=NRP+NOS                                                       1078.
      TP(NRS)=TS(I)                                                     1079.
      W(NRS)=WS(I)                                                      1080.
      KSMP(NRS)=0                                                       1081.
      IF ((KNST.NE.1).AND.(KNST.NE.6)) W(NRS)=0.                        1082.
      KDX(NRS)=KDX(I)                                                   1083.
      LDX(I)=NRS                                                        1084.
      WRK(NRS)=BLANK                                                    1085.
   65 CONTINUE                                                          1086.
      NR=NRP+NOS                                                        1087.
C------- INITIALIZE TRIAL HYPOCENTER -----------------------------------1088.
   80 K=KDX(NEAR)                                                       1089.
      SVY1 = 0.0                                                        1090.
      SVY2 = 0.0                                                        1091.
      SVY3 = 0.0                                                        1092.
      ERLMT = 0.                                                        1093.
      DO 25 I = 1,3                                                     1094.
      ISKP(I)=0                                                         1095.
   25 CONTINUE                                                          1096.
      IF (INST .NE. 9) GO TO 90                                         1097.
      READ(5,85) ORG1,ORG2,LAT1,LAT2,LON1,LON2,Z                        1098.
   85 FORMAT(F5.0,F5.2,I5,F5.2,I5,2F5.2)                                1099.
      ORG=60.*ORG1+ORG2                                                 1100.
      LATEP=60.*LAT1+LAT2                                               1101.
      LONEP=60.*LON1+LON2                                               1102.
      GO TO 105                                                         1103.
   90 IF (NR .GE. 3) GO TO 100                                          1104.
   96 WRITE(6,97)                                                       1105.
   97 FORMAT(///,' ***** INSUFFICIENT DATA FOR LOCATING THIS QUAKE*')   1106.
      IF( NRP .EQ. 0 ) NRP = 1                                          1107.
      DO 98 L=1,NRP                                                     1108.
   98 WRITE(6,99) MSTA(L),PRMK(L),KDATE,KHR,JMIN(L),P(L),S(L)           1109.
   99 FORMAT(5X,2A4,1X,I6,2I2,F5.2,7X,F5.2)                             1110.
      IEXIT=1                                                           1111.
      IF (NRP .EQ. 1) RETURN                                            1112.
      GO TO 575                                                         1113.
  100 Z=ZTR                                                             1114.
      IF (AZRES(NRP+1).NE. BLANK) Z=ZRES                                1115.
      ORG=PMIN-Z/5.-1.                                                  1116.
      IF(LATRT.EQ.0.) GO TO 102                                         1117.
      LATEP=LATRT                                                       1118.
      LONEP=LONRT                                                       1119.
      GO TO 105                                                         1120.
  102 IF (LATR .EQ. 0.) GO TO 104                                       1121.
      LATEP=LATR                                                        1122.
      LONEP=LONR                                                        1123.
      GO TO 105                                                         1124.
  104 LATEP=LAT(K)+0.1                                                  1125.
      LONEP=LON(K)+0.1                                                  1126.
  105 ADJSQ=0.                                                          1127.
      IPH=0                                                             1128.
      NDEC=0                                                            1129.
      PRMSSQ=100000.                                                    1130.
      IF (ISW .EQ. IONE) KNO=MNO(K)                                     1131.
      IF(ISW .EQ. IONE) FLTEP=FLT(KNO,K)                                1132.
      NIMAX=TEST(11)+.0001                                              1133.
C------- GEIGER'S ITERATION TO FIND HYPOCENTRAL ADJUSTMENTS ------------1134.
  109 NI = 1                                                            1135.
      IF (INST .EQ. 9) NI=NIMAX                                         1136.
  111 IF(ERLMT .EQ. 0.) GO TO 110                                       1137.
      LATEP = LATSV + LA(NA)*DELAT                                      1138.
      LONEP = LONSV + LO(NA)*DELON                                      1139.
      Z = ZSV + ALZ(NA)*DEZ                                             1140.
      IF(Z .LT. 0.) Z=0.                                                1141.
  110 FMO=0.                                                            1142.
      FNO=0.                                                            1143.
      DO 112 I=1,5                                                      1144.
  112 SUM(I)=0.                                                         1145.
C------- CALCULATE EPICENTRAL DISTANCE BY RICHTER'S METHOD -------------1146.
      DO 120 I=1,NR                                                     1147.
      JI=KDX(I)                                                         1148.
      AVL=(LAT(JI)+LATEP)/120.                                          1149.
      M1=AVL+1.5                                                        1150.
      M2=AVL*10.+1.5                                                    1151.
      DX(I)=(LON(JI)-LONEP)*CA(M1)*COSL(M2)                             1152.
      DY(I)=(LAT(JI)-LATEP)*CB(M1)                                      1153.
      DELTA(I)=SQRT(DX(I)**2+DY(I)**2)+0.000001                         1154.
      WT(I)=W(I)                                                        1155.
      IF (NI .LE. 1) GO TO 115                                          1156.
C------- DISTANCE WEIGHTING --------------------------------------------1157.
      IF (DELTA(I) .LE. XNEAR) GO TO 115                                1158.
      WT(I)=W(I)*(XFAR-DELTA(I))/XFN                                    1159.
      IF (WT(I) .LT. 0.005) WT(I)=0.                                    1160.
  115 IF (WT(I) .EQ. 0.) GO TO 120                                      1161.
      IF (KSMP(I) .EQ. 1) FMO=FMO+1.                                    1162.
      FNO=FNO+1.                                                        1163.
      SUM(4)=SUM(4)+WT(I)                                               1164.
  120 CONTINUE                                                          1165.
      IF (FNO .LT. 3.) GO TO 96                                         1166.
      AVWT=SUM(4)/FNO                                                   1167.
C------- NORMALIZE DISTANCE WEIGHTS ------------------------------------1168.
      SUM(4)=0.0                                                        1169.
      DO 122 I=1,NR                                                     1170.
  122 WT(I)=WT(I)/AVWT                                                  1171.
      IF ((NI.LE.2).OR.(KAZ.EQ.0)) GO TO 130                            1172.
C------- AZIMUTHAL WEIGHTING -------------------------------------------1173.
      CALL AZWTOS                                                       1174.
C------- COMPUTE TRAVEL TIMES + DERIVATIVES ----------------------------1175.
  130 ZSQ=Z**2                                                          1176.
      CALL TRVDRV                                                       1177.
      FDLY=1.                                                           1178.
      IF (ISW .EQ. IONE) FDLY=0.                                        1179.
C------- CALCULATE TRAVEL TIME RESIDUALS X(4,I) + MODIFY THE DERIV'S ---1180.
  140 DO 150 I=1,NR                                                     1181.
      JI=KDX(I)                                                         1182.
      IF (I .LE. NRP) GO TO 145                                         1183.
C------- S PHASE DATA --------------------------------------------------1184.
      T(I)=POS*T(I)                                                     1185.
      X(1,I)=POS*X(1,I)                                                 1186.
      X(2,I)=POS*X(2,I)                                                 1187.
      X(3,I)=POS*X(3,I)                                                 1188.
      X(4,I)=TP(I)-T(I)-ORG-POS*DLY(KNO,JI)*FDLY                        1189.
      GO TO 150                                                         1190.
  145 IF (KSMP(I) .EQ. 0) GO TO 146                                     1191.
C------- S-P DATA ------------------------------------------------------1192.
      X(1,I)=(POS-1.)*X(1,I)                                            1193.
      X(2,I)=(POS-1.)*X(2,I)                                            1194.
      X(3,I)=(POS-1.)*X(3,I)                                            1195.
      X(4,I)=TS(I)-TP(I)-(POS-1.)*(DLY(KNO,JI)*FDLY+T(I))               1196.
      GO TO 150                                                         1197.
C------- P TRAVEL TIME RESIDUAL ----------------------------------------1198.
  146 X(4,I)=TP(I)-T(I)-ORG-DLY(KNO,JI)*FDLY                            1199.
  150 CONTINUE                                                          1200.
C------- COMPUTE AVR, AAR, RMSSQ, + SDR --------------------------------1201.
      ONF=0.0                                                           1202.
      DO 152 I=1,NR                                                     1203.
      ONF = ONF + WT(I)*(1-KSMP(I))                                     1204.
      XWT = X(4,I)*WT(I)                                                1205.
      SUM(1)=SUM(1)+XWT                                                 1206.
      SUM(2)=SUM(2)+ABS(XWT)                                            1207.
      SUM(3)=SUM(3)+X(4,I)*XWT                                          1208.
      SUM(5)=SUM(5)+XWT*(1-KSMP(I))                                     1209.
  152 CONTINUE                                                          1210.
      IF(FNO .GT. FMO) AVRPS=SUM(5)/(ONF)                               1211.
      AVR=SUM(1)/FNO                                                    1212.
      AAR=SUM(2)/FNO                                                    1213.
      RMSSQ=SUM(3)/FNO                                                  1214.
      SDR=SQRT(ABS(RMSSQ-AVR**2))                                       1215.
      DO 153 I=1,5                                                      1216.
      SUM(I)= 0.0                                                       1217.
  153 CONTINUE                                                          1218.
      IF (RMSSQ .GE. TEST(1)) GO TO 154                                 1219.
      IF(ERLMT .EQ. 1.) GO TO 167                                       1220.
      IF(INST.EQ.9) GO TO 501                                           1221.
      IF(NI .GE. 2) GO TO 167                                           1222.
      GO TO 165                                                         1223.
C------- JEFFREYS' WEIGHTING -------------------------------------------1224.
  154 FMO=0.                                                            1225.
      FNO=0.                                                            1226.
      DO 160 I=1,NR                                                     1227.
      WRK(I)=BLANK                                                      1228.
      IF (WT(I) .EQ. 0.) GO TO 160                                      1229.
      K=10.*ABS(X(4,I)-AVR)/SDR+1.5                                     1230.
      IF (K .GT. 41) K=41                                               1231.
      WT(I)=WT(I)*WF(K)                                                 1232.
      IF (K .GT. 30) WRK(I)=STAR4                                       1233.
      IF (WT(I) .LT. 0.005) WT(I)=0.                                    1234.
      IF (WT(I) .EQ. 0.) GO TO 160                                      1235.
      IF (KSMP(I) .EQ. 1) FMO=FMO+1.                                    1236.
      FNO=FNO+1.                                                        1237.
      SUM(4)=SUM(4)+WT(I)                                               1238.
  160 CONTINUE                                                          1239.
      IF (FNO .LT. 3.) GO TO 96                                         1240.
      AVWT=SUM(4)/FNO                                                   1241.
      SUM(4)=0.0                                                        1242.
      ONF=0.0                                                           1243.
      DO 164 I=1,NR                                                     1244.
      WT(I)=WT(I)/AVWT                                                  1245.
      ONF = ONF + WT(I)*(1-KSMP(I))                                     1246.
      XWT=X(4,I)*WT(I)                                                  1247.
      SUM(5)=SUM(5)+XWT*(1-KSMP(I))                                     1248.
  164 CONTINUE                                                          1249.
C------- RECALCULATE AVRPS ---------------------------------------------1250.
      IF(ERLMT .EQ. 1.) GO TO 163                                       1251.
      IF(INST .NE. 9) GO TO 163                                         1252.
      AVRPS = 0.0                                                       1253.
      IF(FNO .NE. FMO) AVRPS = SUM(5)/ONF                               1254.
      GO TO 501                                                         1255.
  163 IF(FNO.EQ.FMO) AVRPS=0.0                                          1256.
      IF(FNO.EQ.FMO) GO TO 167                                          1257.
      AVRPS=SUM(5)/(ONF)                                                1258.
      SUM(5)=0.0                                                        1259.
      IF(ERLMT .EQ. 1.) GO TO 167                                       1260.
C------- RESET FIRST ORIGIN TIME ---------------------------------------1261.
      IF(NI.GE. 2) GO TO 167                                            1262.
  165 ORG=ORG+AVRPS                                                     1263.
      DO 166 I=1,NR                                                     1264.
      IF(KSMP(I) .EQ. 0) X(4,I)=X(4,I)-AVRPS                            1265.
      XWT=WT(I)*X(4,I)                                                  1266.
      SUM(5)=SUM(5)+XWT*(1 - KSMP(I))                                   1267.
      SUM(2)=SUM(2)+ABS(XWT)                                            1268.
      SUM(3)=SUM(3)+X(4,I)*XWT                                          1269.
  166 CONTINUE                                                          1270.
      IF(FNO .GT. FMO) AVRPS=SUM(5)/(ONF)                               1271.
      AAR=SUM(2)/FNO                                                    1272.
      RMSSQ = SUM(3)/FNO                                                1273.
      GO TO 169                                                         1274.
C------- FOR NI*1, COMPUTE AAR, + RMSSQ AS IF AVRPS=0. -----------------1275.
  167 DO 168 I=1,NR                                                     1276.
      XWT=WT(I)*(X(4,I)-AVRPS*(1-KSMP(I)))                              1277.
      SUM(2)=SUM(2)+ABS(XWT)                                            1278.
      SUM(3)=SUM(3)+(X(4,I)-AVRPS*(1-KSMP(I)))*XWT                      1279.
  168 CONTINUE                                                          1280.
      AAR=SUM(2)/FNO                                                    1281.
      RMSSQ=SUM(3)/FNO                                                  1282.
      IF(ERLMT .EQ. 0.) GO TO 169                                       1283.
C------- OUTPUT RMS ERROR OF AUXILIARY POINTS --------------------------1284.
      L = LATEP/60.                                                     1285.
      ALA = LATEP - 60.*L                                               1286.
      L = LONEP/60.                                                     1287.
      ALO = LONEP - 60.*L                                               1288.
      RMSX= SQRT(RMSSQ)                                                 1289.
      DRMS = RMSX - RMSSV                                               1290.
      GO TO (1,2,3,4,5,6,1,2,3,4), NA                                   1291.
    1 WRITE(6,801) ALA,ALO,Z,AVRPS,RMSX,DRMS                            1292.
  801 FORMAT(5F10.2,10X,F6.2)                                           1293.
      GO TO 174                                                         1294.
    2 WRITE(6,802) ALA,ALO,Z,AVRPS,RMSX,DRMS                            1295.
  802 FORMAT(5F10.2,28X,F6.2)                                           1296.
      GO TO 174                                                         1297.
    3 WRITE(6,803) ALA,ALO,Z,AVRPS,RMSX,DRMS                            1298.
  803 FORMAT(5F10.2,13X,'(',F6.2,')')                                   1299.
      GO TO 174                                                         1300.
    4 WRITE(6,804) ALA,ALO,Z,AVRPS,RMSX,DRMS                            1301.
  804 FORMAT(5F10.2,31X,'(',F6.2,')')                                   1302.
      IF(NA .EQ. 10) GO TO 550                                          1303.
      GO TO 174                                                         1304.
    5 WRITE(6,805) ALA,ALO,Z,AVRPS,RMSX,DRMS                            1305.
  805 FORMAT(/5F10.2,19X,F6.2)                                          1306.
      WRITE(6,807) RMSSV                                                1307.
  807 FORMAT(40X,F10.2,23X,'0.00')                                      1308.
      GO TO 174                                                         1309.
    6 WRITE(6,806) ALA,ALO,Z,AVRPS,RMSX,DRMS                            1310.
  806 FORMAT(5F10.2,22X,'(',F6.2,')'/)                                  1311.
  174 NA = NA + 1                                                       1312.
      GO TO 111                                                         1313.
C------- CHECK IF SOLUTION IS BETTER THAN PREVIOUS ONE -----------------1314.
  169 IF((NI .EQ. 1) .AND. (NDEC .EQ. 0)) GO TO 170                     1315.
      IF(PRMSSQ.GE.RMSSQ) GO TO 170                                     1316.
      NDEC = NDEC +1                                                    1317.
      IF(NDEC .GT. 1) GO TO 175                                         1318.
      DO 177 I= 1,3                                                     1319.
      B(I) = 0.0                                                        1320.
      AF(I)=-1.0                                                        1321.
      SE(I) = 0.0                                                       1322.
  177 CONTINUE                                                          1323.
      NI = NI -1                                                        1324.
      BM1=Y(1)                                                          1325.
      BM2=Y(2)                                                          1326.
      BM3=Y(3)                                                          1327.
      BMAX = ABS(Y(1))                                                  1328.
      IIMAX = 1                                                         1329.
      DO 176 I = 2,3                                                    1330.
      IF(ABS(Y(I)).LE.BMAX) GO TO 176                                   1331.
      BMAX = ABS(Y(I))                                                  1332.
      IIMAX = I                                                         1333.
  176 CONTINUE                                                          1334.
      ISKP(IIMAX)=1                                                     1335.
      Y(1)=-BM1/5.                                                      1336.
      Y(2)=-BM2/5.                                                      1337.
      Y(3)=-BM3/5.                                                      1338.
      Y(4)=-Y(1)*XMEAN(1)-Y(2)*XMEAN(2)-Y(3)*XMEAN(3)                   1339.
      XADJSQ=Y(1)**2+Y(2)**2+Y(3)**2                                    1340.
      KP=0                                                              1341.
      IF(XADJSQ .LT. 4.*TEST(4)/25.) GO TO 170                          1342.
  175 IF(NDEC .EQ. 5) GO TO 170                                         1343.
      GO TO 325                                                         1344.
C------- STEPWISE MULTIPLE REGRESSION ANALYSIS OF TRAVEL TIME RESIDUALS-1345.
  170 IF(NDEC .GE. 1) NI = NI + 1                                       1346.
      IF (INST.EQ.1) GO TO 250                                          1347.
      IF(ISKP(3) .EQ. 1) GO TO 250                                      1348.
      IF (INST .EQ. 9) GO TO 501                                        1349.
      IF ((FNO.EQ.3) .AND. (FMO.LT.3)) GO TO 250                        1350.
C---- FREE SOLUTION                                                     1351.
  200 KZ=0                                                              1352.
      KF=0                                                              1353.
      CALL SWMREG                                                       1354.
C------- AVOID CORRECTING DEPTH IF HORIZONTAL CHANGE IS LARGE ----------1355.
      IF (Y(1)**2+Y(2)**2 .LT. TEST(2)) GO TO 300                       1356.
C---- FIXED DEPTH SOLUTION                                              1357.
  250 KZ=1                                                              1358.
      KF=0                                                              1359.
      CALL SWMREG                                                       1360.
C------- LIMIT FOCAL DEPTH CHANGE + AVOID HYPOCENTER IN THE AIR --------1361.
  300 DO 275 I= 1,3                                                     1362.
      ISKP(I)=0                                                         1363.
  275 CONTINUE                                                          1364.
      OLDY1=Y(1)                                                        1365.
      OLDY2=Y(2)                                                        1366.
      OLDY3=Y(3)                                                        1367.
      ABSY1=ABS(Y(1))                                                   1368.
      ABSY2=ABS(Y(2))                                                   1369.
      ABSY3=ABS(Y(3))                                                   1370.
      IF(ABSY1.GT.ABSY2) GO TO 305                                      1371.
      ABSGR=ABSY2                                                       1372.
      GO TO 308                                                         1373.
  305 ABSGR=ABSY1                                                       1374.
  308 IF(ABSY3.LE.TEST(5)) GO TO 310                                    1375.
      I=ABSY3/TEST(5)                                                   1376.
      Y(3)=Y(3)/(I+1)                                                   1377.
  310 IF((Z+Y(3)).GT. 0.0) GO TO 315                                    1378.
      Y(3)=-Z*TEST(12)+.000001                                          1379.
      ISKP(3) = 1                                                       1380.
C------- LIMIT HORIZONTAL ADJUSTMENT OF EPICENTER ----------------------1381.
  315 IF(ABSGR.LE.TEST(10)) GO TO 320                                   1382.
      I=ABSGR/TEST(10)                                                  1383.
      Y(1)=Y(1)/(I+1)                                                   1384.
      Y(2)=Y(2)/(I+1)                                                   1385.
  320 Y(4)=Y(4)-(Y(3)-OLDY3)*XMEAN(3)-(Y(1)-OLDY1)*XMEAN(1)             1386.
     1 -(Y(2)-OLDY2)*XMEAN(2)                                           1387.
      XADJSQ=Y(1)**2+Y(2)**2+Y(3)**2                                    1388.
      KP=0                                                              1389.
      NDEC=0                                                            1390.
      JPH=0                                                             1391.
  325 IF (IPRN .GE. 1) CALL OUTPUT                                      1392.
      IF(NDEC .GE. 1) GO TO 330                                         1393.
C------- TERMINATE ITERATION IF HYPOCENTER ADJUSTMENT ) TEST(4) --------1394.
      IF (XADJSQ .LT. TEST(4)) GO TO 500                                1395.
  330 IF(NI .EQ. NIMAX) GO TO 500                                       1396.
C------- ADJUST HYPOCENTER ---------------------------------------------1397.
  350 AVL=LATEP/60.                                                     1398.
  375 M1=AVL+1.5                                                        1399.
      M2=AVL*10.+1.5                                                    1400.
      DY1 =Y(1)/(CA(M1)*COSL(M2))                                       1401.
      DY2 =Y(2)/CB(M1)                                                  1402.
      LATEP=LATEP+DY2                                                   1403.
      LONEP=LONEP+DY1                                                   1404.
      Z=Z+Y(3)                                                          1405.
      ORG=ORG+Y(4)                                                      1406.
      SVY1 = Y(1)                                                       1407.
      SVY2 = Y(2)                                                       1408.
      SVY3 = Y(3)                                                       1409.
      ADJSQ=XADJSQ                                                      1410.
      IF(NDEC .EQ. 0) PRMSSQ=RMSSQ                                      1411.
      IF(NDEC.GE.1) GO TO 110                                           1412.
  400 NI = NI + 1                                                       1413.
      IF(NI .LE. NIMAX) GO TO 111                                       1414.
C------- RESET ORIGIN TIME ---------------------------------------------1415.
  500 ORG=ORG+XMEAN(4)                                                  1416.
      GO TO 502                                                         1417.
  501 XMEAN(4)=0.0                                                      1418.
  502 DO 505 I=1,5                                                      1419.
  505 SUM(I)=0.0                                                        1420.
      SUMM = 0.0                                                        1421.
      DO 510 I=1,NR                                                     1422.
      IF (KSMP(I) .EQ. 0) X(4,I)=X(4,I)-XMEAN(4)                        1423.
      IF (WT(I) .EQ. 0.) GO TO 510                                      1424.
      IF(INST .NE. 9) GO TO 509                                         1425.
      XWTS=WT(I)*(X(4,I)**2)                                            1426.
      IF(KSMP(I) .EQ. 0) XWTS=WT(I)*((X(4,I)-AVRPS)**2)                 1427.
      SUMM = SUMM + XWTS                                                1428.
  509 XWT=X(4,I)*WT(I)                                                  1429.
      SUM(1)=SUM(1)+XWT                                                 1430.
      SUM(2)=SUM(2)+ABS(XWT)                                            1431.
      SUM(3)=SUM(3)+X(4,I)*XWT                                          1432.
      SUM(5)=SUM(5)+XWT*(1-KSMP(I))                                     1433.
  510 CONTINUE                                                          1434.
      RM9SV = SUMM/FNO                                                  1435.
      AVR=SUM(1)/FNO                                                    1436.
      AVRPS = 0.0                                                       1437.
      IF(FNO .GT. FMO) AVRPS=SUM(5)/ONF                                 1438.
      AAR=SUM(2)/FNO                                                    1439.
      RMSSQ=SUM(3)/FNO                                                  1440.
C------- COMPUTE ERROR ESTIMATES BY SOLVING FULL NORMAL EQUATION -------1441.
  520 KF=2                                                              1442.
      KP=1                                                              1443.
      KZ=0                                                              1444.
      CALL SWMREG                                                       1445.
      DO 521 I =1,3                                                     1446.
  521 Y(I)=0.0                                                          1447.
      IF(INST.EQ.1) KZ = 1                                              1448.
      CALL OUTPUT                                                       1449.
      IF (KMS .EQ. 1) CALL MISING                                       1450.
      IF ((KNST.GE.5) .OR. (KFM.GE.1)) CALL FMPLOT                      1451.
      QNO(JAV)=QNO(JAV)+1.                                              1452.
      IF (JAV .GT. IQ) GO TO 523                                        1453.
C------- COMPUTE SUMMARY OF TRAVEL TIME RESIDUALS ----------------------1454.
      DO 522 I=1,NRP                                                    1455.
      IF ((WT(I).EQ.0.) .OR. (KSMP(I).EQ.1))  GO TO 522                 1456.
      JI=KDX(I)                                                         1457.
      NRES(KNO,JI)=NRES(KNO,JI)+1                                       1458.
      SR(KNO,JI)=SR(KNO,JI)+X(4,I)*WT(I)                                1459.
      SRSQ(KNO,JI)=SRSQ(KNO,JI)+X(4,I)**2*WT(I)                         1460.
      SRWT(KNO,JI)=SRWT(KNO,JI)+WT(I)                                   1461.
  522 CONTINUE                                                          1462.
  523 IF (KTEST .NE. 1) GO TO 550                                       1463.
C------- COMPUTE RMS AT AUXILIARY POINTS -------------------------------1464.
      RMSSV = SQRT(RMSSQ)                                               1465.
      IF(INST.EQ.9) RMSSV = SQRT(RM9SV)                                 1466.
      ERLMT = 1.                                                        1467.
      LATSV = LATEP                                                     1468.
      LONSV = LONEP                                                     1469.
      ZSV = Z                                                           1470.
      AVL = LATEP/60.                                                   1471.
      M1 = AVL + 1.5                                                    1472.
      M2 = AVL*10. + 1.5                                                1473.
      DELAT = TEST(13)/CB(M1)                                           1474.
      DELON = TEST(13)/(CA(M1)*COSL(M2))                                1475.
      DEZ = TEST(13)                                                    1476.
      WRITE(6,525)                                                      1477.
  525 FORMAT (/'       LAT       LON         Z     AVRPS       RMS      1478.
     1                DRMS'/)                                           1479.
      NA=1                                                              1480.
      GO TO 111                                                         1481.
  550 TIME1=TIME2                                                       1482.
  575 CONTINUE                                                          1483.
C------- CHECK FOR MULTIPLE SOLUTIONS OF THE SAME EARTHQUAKE -----------1484.
      IF(IPRO.NE.ISTTT) RETURN                                          1485.
      NR=NRP                                                            1486.
      NRP1=NR +1                                                        1487.
      READ(5,600)  CHECK,IPRO,KNST,INST,ZRES,LAT1,LAT2,LON1,LON2,       1488.
     1 AZRES(NRP1)                                                      1489.
      WRITE(6,601) CHECK,IPRO,KNST,INST,ZRES,LAT1,LAT2,LON1,LON2        1490.
  601 FORMAT(//2A4,9X,2I1,F5.2,1X,2(I4,F6.2),'--- RUN AGAIN ---')       1491.
  600 FORMAT(2A4,9X,2I1,F5.2,1X,2(I4,F6.2),T21,A4)                      1492.
      LATRT=60.*LAT1+LAT2                                               1493.
      LONRT=60.*LON1+LON2                                               1494.
      IF(CHECK.EQ.BLANK) GO TO 30                                       1495.
      WRITE(6,610) CHECK                                                1496.
  610 FORMAT(/' ERROR ',A4,' SKIPPED.   INST. CARD DID NOT FOLLOW ***') 1497.
      RETURN                                                            1498.
      END                                                               1499.
      SUBROUTINE SORT(X,KEY,NO)                                         1500.
      DIMENSION X(NO),KEY(NO)                                           1501.
C-----------------------------------------------------------------------1502.
      DO 1 I=1,NO                                                       1503.
 1    KEY(I)=I                                                          1504.
      MO=NO                                                             1505.
 2    IF (MO-15) 21,21,23                                               1506.
 21   IF (MO-1) 29,29,22                                                1507.
 22   MO=2*(MO/4)+1                                                     1508.
      GO TO 24                                                          1509.
 23   MO=2*(MO/8)+1                                                     1510.
 24   KO=NO-MO                                                          1511.
      JO=1                                                              1512.
 25   I=JO                                                              1513.
 26   IF (X(I)-X(I+MO)) 28,28,27                                        1514.
 27   TEMP=X(I)                                                         1515.
      X(I)=X(I+MO)                                                      1516.
      X(I+MO)=TEMP                                                      1517.
      KEMP=KEY(I)                                                       1518.
      KEY(I)=KEY(I+MO)                                                  1519.
      KEY(I+MO)=KEMP                                                    1520.
      I=I-MO                                                            1521.
      IF (I-1) 28,26,26                                                 1522.
 28   JO=JO+1                                                           1523.
      IF (JO-KO) 25,25,2                                                1524.
 29   RETURN                                                            1525.
      END                                                               1526.
      SUBROUTINE SUMOUT                                                 1527.
C------- OUTPUT SUMMARY OF TIME AND MAGNITUDE RESIDUALS ----------------1528.
      
      REAL LAT,LON,LAT2,LON2                                            1529.
      COMMON /A1/ NSTA(151),DLY(2,151),FMGC(151),XMGC(151),KLAS(151),   1530.
     1       PRR(151),CALR(151),ICAL(151),IS(151),NDATE(151),NHRMN(151) 1531.
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)   1532.
      COMMON /A3/ NRES(2,151),NXM(151),NFM(151),SR(2,151),SRSQ(2,151),  1533.
     1       SRWT(2,151),SXM(151),SXMSQ(151),SFM(151),SFMSQ(151),QNO(4) 1534.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40) 1535.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101)1536.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW   1537.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)   1538.
      COMMON /A22/ F(21,21),G(4,21),H(21),DEPTH(21),IONE                1539.
      COMMON /A25/ INS(151),IEW(151),JPH                                1540.
      DIMENSION AVRES(4,151),SDRES(4,151)                               1541.
      DATA KS1,KW1/'S','W'/                                             1542.
C-----------------------------------------------------------------------1543.
      QSUM=QNO(1)+QNO(2)+QNO(3)+QNO(4)                                  1544.
      IF (QSUM .EQ. 0.) GO TO 72                                        1545.
      WRITE(6,5) (QNO(I),I=1,4),QSUM                                    1546.
    5 FORMAT(1H1,' ***** CLASS*     A     B     C     D TOTAL *****'    1547.
     1,//,7X,'NUMBER*',5F6.1)                                           1548.
      DO 10 I=1,4                                                       1549.
   10 QNO(I)=100.*QNO(I)/QSUM                                           1550.
      WRITE(6,15)(QNO(I),I=1,4)                                         1551.
   15 FORMAT(/,12X,'(*',4F6.1)                                          1552.
      WRITE(6,20)                                                       1553.
   20 FORMAT(///,10X,'TRAVELTIME RESIDUALS (MODEL=1)',5X                1554.
     1,'TRAVELTIME RESIDUALS (MODEL=2)',5X,'X-MAGNITUDE RESIDUALS'      1555.
     2,6X,'F-MAGNITUDE RESIDUALS',/,' STATION   NRES    SRWT   AVRES   S1556.
     3DRES       NRES    SRWT   AVRES   SDRES        NXM    AVXM    SDXM1557.
     4        NFM    AVFM    SDFM')                                     1558.
      DO 70 I=1,NS                                                      1559.
      DO 30 J=1,4                                                       1560.
      AVRES(J,I)=0.                                                     1561.
   30 SDRES(J,I)=0.                                                     1562.
      IF (NRES(1,I) .EQ. 0) GO TO 35                                    1563.
      AVRES(1,I)=SR(1,I)/SRWT(1,I)                                      1564.
      GHJ=SRSQ(1,I)/SRWT(1,I)-AVRES(1,I)**2+0.000001
      IF(GHJ.LT.0.0)GHJ=0.0
      SDRES(1,I)=SQRT(GHJ)
C      SDRES(1,I)=SQRT(SRSQ(1,I)/SRWT(1,I)-AVRES(1,I)**2+0.000001)       1565.
   35 IF (NRES(2,I) .EQ. 0) GO TO 40                                    1566.
      AVRES(2,I)=SR(2,I)/SRWT(2,I)                                      1567.
      SDRES(2,I)=SQRT(SRSQ(2,I)/SRWT(2,I)-AVRES(2,I)**2+0.000001)       1568.
   40 IF (NXM(I) .EQ. 0) GO TO 50                                       1569.
      AVRES(3,I)=SXM(I)/NXM(I)                                          1570.
      SDRES(3,I)=SQRT(SXMSQ(I)/NXM(I)-AVRES(3,I)**2+0.000001)           1571.
   50 IF (NFM(I) .EQ. 0) GO TO 60                                       1572.
      AVRES(4,I)=SFM(I)/NFM(I)                                          1573.
      SDRES(4,I)=SQRT(SFMSQ(I)/NFM(I)-AVRES(4,I)**2+0.000001)           1574.
   60 WRITE(6,65) NSTA(I),NRES(1,I),SRWT(1,I),AVRES(1,I),SDRES(1,I)     1575.
     1,NRES(2,I),SRWT(2,I),AVRES(2,I),SDRES(2,I),NXM(I),AVRES(3,I)      1576.
     2,SDRES(3,I),NFM(I),AVRES(4,I),SDRES(4,I)                          1577.
   65 FORMAT(4X,A4,2X,I5,3F8.2,6X,I5,3F8.2,2(6X,I5,2F8.2))              1578.
   70 CONTINUE                                                          1579.
   72 IF (IPUN .NE. 3) GO TO 200                                        1580.
C------- PUNCH STATION LIST WITH REVISED DELAYS,XMGC,AND FMGC ----------1581.
      IF (ISW .EQ. IONE) GO TO 80                                       1582.
      WRITE(6,75) INS(1),IEW(1)                                         1583.
   75 FORMAT(1H1,' ***** NEW STATION LIST *****'                        1584.
     1,///,    4X,'I   STN  LAT ',A1,'    LONG ',A1,'   ELV DELAY',5X   1585.
     2,'FMGC  XMGC KL  PRR  CALR IC IS   DATE HRMN')                    1586.
      GO TO 90                                                          1587.
   80 WRITE(6,85) INS(1),IEW(1)                                         1588.
   85 FORMAT(1H1,' ***** NEW STATION LIST *****'                        1589.
     1,///,4X,'I   STN   LAT ',A1,'     LONG ',A1,'    ELV     M  DLY1  1590.
     2DLY2  XMGC  FMGC K  CALR IC  DATE HRMN')                          1591.
   90 DO 120 I=1,NS                                                     1592.
      DLY(1,I)=DLY(1,I)+AVRES(1,I)                                      1593.
      IF (ISW.EQ.IONE) DLY(2,I)=DLY(2,I)+AVRES(2,I)                     1594.
      XMGC(I)=XMGC(I)+AVRES(3,I)                                        1595.
      FMGC(I)=FMGC(I)+AVRES(4,I)                                        1596.
      LAT1=LAT(I)/60.                                                   1597.
      LAT2=LAT(I)-60.*LAT1                                              1598.
      LON1=LON(I)/60.                                                   1599.
      LON2=LON(I)-60.*LON1                                              1600.
      IF (ISW .EQ. IONE) GO TO 115                                      1601.
      WRITE(6,105)I,NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)   1602.
     1,DLY(1,I),FMGC(I),XMGC(I),KLSS(I),PRR(I),CALS(I),ICAL(I),NDATE(I) 1603.
     2,NHRMN(I)                                                         1604.
  105 FORMAT(I5,2X,A4,I2,F5.2,A1,I4,F5.2,A1,I5,F6.2,4X,F5.2,2X,F5.2,I2  1605.
     1,1X,F4.2,1X,F6.2,I2,5X,I6,I4)                                     1606.
      WRITE(6,110) NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)    1607.
     1,DLY(1,I),FMGC(I),XMGC(I),KLSS(I),PRR(I),CALS(I),ICAL(I),NDATE(I) 1608.
     2,NHRMN(I)                                                         1609.
  110 FORMAT(2X,A4,I2,F5.2,A1,I3,F5.2,A1,I4,F6.2,T38,F5.2,T45,F5.2      1610.
     1,I2,1X,F4.2,1X,F6.2,I2,T71,I6,I4)                                 1611.
      GO TO 120                                                         1612.
  115 WRITE(6,116)I,NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)   1613.
     1,MNO(I),DLY(1,I),DLY(2,I),XMGC(I),FMGC(I),KLSS(I),CALS(I),ICAL(I) 1614.
     2,NDATE(I),NHRMN(I)                                                1615.
  116 FORMAT(I5,2X,A4,I3,'-',F5.2,A1,I4,'-',F5.2,A1,I5,I6,2F6.2         1616.
     1,2F6.2,I2,F6.2,I2,2X,I6,I4)                                       1617.
      WRITE(6,117) NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)    1618.
     1,MNO(I),DLY(1,I),DLY(2,I),XMGC(I),FMGC(I),KLSS(I),CALS(I),ICAL(I) 1619.
     2,NDATE(I),NHRMN(I)                                                1620.
  117 FORMAT(A4,I3,'-',F5.2,A1,I3,'-',F5.2,A1,I4,I6,2F6.2               1621.
     1,2F6.2,I2,F6.2,I2,2X,I6,I4)                                       1622.
  120 CONTINUE                                                          1623.
      RETURN                                                            1624.
C------- PUNCH STATION LIST WITH REVISED CALIBRATIONS ------------------1625.
  200 IF (IPUN .NE. 4) RETURN                                           1626.
      IF (ISW .EQ. IONE) GO TO 205                                      1627.
      WRITE(6,75) INS(1),IEW(1)                                         1628.
      GO TO 206                                                         1629.
  205 WRITE(6,85) INS(1),IEW(1)                                         1630.
  206 DO 220 I=1,NS                                                     1631.
      LAT1=LAT(I)/60.                                                   1632.
      LAT2=LAT(I)-60.*LAT1                                              1633.
      LON1=LON(I)/60.                                                   1634.
      LON2=LON(I)-60.*LON1                                              1635.
      IF (ISW .EQ. IONE) GO TO 210                                      1636.
      WRITE(6,105)I,NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)   1637.
     1,DLY(1,I),FMGC(I),XMGC(I),KLAS(I),PRR(I),CALR(I),ICAL(I),MDATE(I) 1638.
     2,MHRMN(I)                                                         1639.
      WRITE(6,110) NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)    1640.
     1,DLY(1,I),FMGC(I),XMGC(I),KLAS(I),PRR(I),CALR(I),ICAL(I),MDATE(I) 1641.
     2,MHRMN(I)                                                         1642.
      GO TO 220                                                         1643.
  210 WRITE(6,116)I,NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)   1644.
     1,MNO(I),DLY(1,I),DLY(2,I),XMGC(I),FMGC(I),KLAS(I),CALR(I),ICAL(I) 1645.
     2,MDATE(I),MHRMN(I)                                                1646.
      WRITE(6,117) NSTA(I),LAT1,LAT2,INS(I),LON1,LON2,IEW(I),IELV(I)    1647.
     1,MNO(I),DLY(1,I),DLY(2,I),XMGC(I),FMGC(I),KLAS(I),CALR(I),ICAL(I) 1648.
     2,MDATE(I),MHRMN(I)                                                1649.
  220 CONTINUE                                                          1650.
      RETURN                                                            1651.
      END                                                               1652.
      SUBROUTINE SWMREG                                                 1653.
C------- COMPUTE GEIGER ADJUSTMENTS BY STEP-WISE MULTIPLE REGRESSION OF 1654.
C        TRAVEL TIME RESIDUALS -----------------------------------------1655.
      
      REAL*8 ENT,ELM,FMT                                                1656.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101)1657.
      COMMON /A7/ KP,KZ,KOUT,W(101),Y(4),BSE(4),XMEAN(4),CP(180),SP(180)1658.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW   1659.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)   1660.
      COMMON /A21/ KSMP(151),FMO,ONF,B(4),IPH,KF,AVRPS,IEXIT            1661.
      COMMON /A24/ FLTEP,IPRO,ISTTT,ISKP(4),AHEAD(12),FLIM,AF(3),NDEC   1662.
      DIMENSION XSUM(4),SIGMA(4),IDX(4),V(3),PF(3),A(7,7),T(7,7),S(4,4) 1663.
      DATA L,M,MM,M1/3,4,7,5/,ENT,ELM/'ENTERING','LEAVING.'/            1664.
C-----------------------------------------------------------------------1665.
      KFLAG=0                                                           1666.
      SVTEST = TEST(3)                                                  1667.
      ONF=0.0                                                           1668.
      FLIM = TEST(3)                                                    1669.
      DO 2 I=1,3                                                        1670.
      AF(I)=-1.00                                                       1671.
    2 CONTINUE                                                          1672.
      DO 5 I=1,NR                                                       1673.
      ONF=ONF + W(I)*(1-KSMP(I))                                        1674.
    5 CONTINUE                                                          1675.
      DO 10 I=1,MM                                                      1676.
      DO 10 J=1,MM                                                      1677.
   10 A(I,J)=0.                                                         1678.
C-----COMPUTE MEANS,STANDARD DEVIATIONS,AND CORRECTED SUMS OF SQUARE    1679.
      DO 40 I=1,M                                                       1680.
      XSUM(I)=0.                                                        1681.
      XMEAN(I)=0.                                                       1682.
      DO 40 J=1,M                                                       1683.
   40 S(I,J)=0.                                                         1684.
      DO 50 K=1,NR                                                      1685.
      DO 50 I=1,M                                                       1686.
      TEMP=X(I,K)*W(K)                                                  1687.
      ETMP=TEMP*(1-KSMP(K))                                             1688.
      XSUM(I)=XSUM(I)+ETMP                                              1689.
      DO 50 J=I,M                                                       1690.
   50 S(I,J)=S(I,J)+TEMP*X(J,K)                                         1691.
      DO 70 I=1,M                                                       1692.
      IF (ONF .EQ. 0.) GO TO 65                                         1693.
      XMEAN(I)=XSUM(I)/ONF                                              1694.
      DO 60 J=I,M                                                       1695.
   60 S(I,J)=S(I,J)-XSUM(I)*XSUM(J)/ONF                                 1696.
   65 A(I,I)=1.                                                         1697.
      IF (S(I,I) .LT. 0.000001) S(I,I)=0.000001                         1698.
      SIGMA(I)=SQRT(S(I,I))                                             1699.
   70 CONTINUE                                                          1700.
C-----COMPUTE AND AUGMENT CORRELATION MATRIX A                          1701.
      DO 80 I=1,L                                                       1702.
      I1=I+1                                                            1703.
      DO 80 J=I1,M                                                      1704.
      A(I,J)=S(I,J)/(SIGMA(I)*SIGMA(J))                                 1705.
   80 A(J,I)=A(I,J)                                                     1706.
      PHI=FNO-1.                                                        1707.
      DO 120 I=M1,MM                                                    1708.
      A(I-M,I)=1.                                                       1709.
  120 A(I,I-M)=-1.                                                      1710.
  130 DO 140 I=1,M                                                      1711.
      B(I)=0.                                                           1712.
      Y(I)=0.                                                           1713.
      BSE(I)=0.                                                         1714.
  140 IDX(I)=0                                                          1715.
      IF (IPRN .LT. 3) GO TO 150                                        1716.
      WRITE(6,45)                                                       1717.
   45 FORMAT(///, '***** DATA *****',//,4X,'K',8X,'W'                   1718.
     1,14X,'X1',14X,'X2',14X,'X3',14X,'X4',/)                           1719.
      DO 47 K=1,NR                                                      1720.
      WRITE(6,46) K,W(K),(X(I,K),I=1,M)                                 1721.
   46 FORMAT(I5,8E16.8)                                                 1722.
   47 CONTINUE                                                          1723.
      WRITE(6,75) (XMEAN(I),I=1,M)                                      1724.
   75 FORMAT(/,' MEAN',16X,8E16.8)                                      1725.
      WRITE(6,76) (SIGMA(I),I=1,M)                                      1726.
   76 FORMAT(/,' SIGMA',15X,7E16.8)                                     1727.
      WRITE(6,77)                                                       1728.
   77 FORMAT(///,' ***** CORRECTED SUMS OF SQUARES MATRIX *****',/)     1729.
      DO 78 I=1,M                                                       1730.
   78 WRITE(6,95) (S(I,J),J=1,M)                                        1731.
      WRITE(6,85)                                                       1732.
   85 FORMAT(///,' ***** CORRELATION MATRIX R *****',/)                 1733.
      DO 90 I=1,M                                                       1734.
   90 WRITE(6,95) (A(I,J),J=1,M)                                        1735.
   95 FORMAT(7E18.8)                                                    1736.
C-----STEPWISE MULTIPLE REGRESSION                                      1737.
      WRITE(6,125) NR,L,TEST(3)                                         1738.
  125 FORMAT(///, '********** STEPWISE MULTIPLE REGRESSION ANALYSIS'    1739.
     1,' **********',//' NUMBER OF DATA....................',I5         1740.
     2,              /,' NUMBER OF INDEPENDENT VARIABLES...',I5         1741.
     3,              /,' CRITICAL F-VALUE..................',F8.2)      1742.
  150 DO 300 NSTEP=1,L                                                  1743.
      NU=0                                                              1744.
      MU=0                                                              1745.
      IF (IPRN .LT. 3) GO TO 155                                        1746.
      WRITE(6,154) NSTEP,KZ,KF                                          1747.
  154 FORMAT(//,' ***** STEP NO.',I2,' *****',5X,'KZ =',I2,5X,'KF =',I2)1748.
C-----FIND VARIABLE TO ENTER REGRESSION                                 1749.
  155 VMAX=0.                                                           1750.
      MAX=NSTEP                                                         1751.
      DO 160 I=1,L                                                      1752.
      IF(ISKP(I).EQ.1) GO TO 160                                        1753.
      IF (IDX(I) .EQ. 1) GO TO 160                                      1754.
      IF ((I.EQ.3).AND.(KZ.EQ.1)) GO TO 160                             1755.
	if (a(i,i) .eq. 0.0 ) a(i,i) = 0.00001
      V(I)=A(I,M)*A(M,I)/A(I,I)                                         1756.
      IF (V(I) .LE. VMAX) GO TO 160                                     1757.
      VMAX=V(I)                                                         1758.
      MAX=I                                                             1759.
  160 CONTINUE                                                          1760.
      F=0.0                                                             1761.
      IF(VMAX.EQ.0.0) GO TO 163                                         1762.
      F=(PHI-1.)*VMAX/(A(M,M)-VMAX)                                     1763.
      IF(F .GE. 1000.) F=999.99                                         1764.
  163 AF(MAX)=F                                                         1765.
      IF(KF .GE. 2) GO TO 165                                           1766.
      IF (F .LT. TEST(3)) GO TO 400                                     1767.
  165 IF ((MAX.EQ.3).AND.(KZ.EQ.1)) GO TO 300                           1768.
  166 NU=MAX                                                            1769.
      IDX(NU)=1                                                         1770.
      PHI=PHI-1.                                                        1771.
C-----COMPUTE MATRIX T FOR THE ENTRANCE OF VARIABLE X(NU)               1772.
      DO 170 J=1,MM                                                     1773.
  170 T(NU,J)=A(NU,J)/A(NU,NU)                                          1774.
      DO 180 I=1,MM                                                     1775.
      IF (I .EQ. NU) GO TO 180                                          1776.
      DO 175 J=1,MM                                                     1777.
  175 T(I,J)=A(I,J)-A(I,NU)*A(NU,J)/A(NU,NU)                            1778.
  180 CONTINUE                                                          1779.
      DO 190 I=1,MM                                                     1780.
      DO 190 J=1,MM                                                     1781.
  190 A(I,J)=T(I,J)                                                     1782.
      DO 200 I=1,L                                                      1783.
      IF (IDX(I) .EQ. 0) GO TO 200                                      1784.
      IF (ABS(A(M,M)*A(I+M,I+M)) .LT. .000001 ) GO TO 195               1785.
      PF(I)=PHI*A(I,M)**2/(A(M,M)*A(I+M,I+M))                           1786.
      IF(PF(I) .GE. 1000.0) PF(I)=999.99                                1787.
      AF(I) = PF(I)                                                     1788.
      GO TO 200                                                         1789.
  195 PF(I) = 999.99                                                    1790.
  200 CONTINUE                                                          1791.
      IF (IPRN .LT. 3) GO TO 210                                        1792.
      CALL ANSWER(A,S,XMEAN,SIGMA,IDX,PHI,L,M,MM,PF,NU,ENT)             1793.
  210 IF (KF .EQ. 2) GO TO 300                                          1794.
      IF(KF .GE. 3) GO TO 450                                           1795.
C-----FIND VARIABLE TO LEAVE REGRESSION                                 1796.
      DO 250 K=1,L                                                      1797.
      IF (IDX(K) .EQ. 0) GO TO 250                                      1798.
      IF (PF(K) .GE. TEST(3)) GO TO 250                                 1799.
      MU=K                                                              1800.
      F=PF(MU)                                                          1801.
      IDX(MU)=0                                                         1802.
      PHI=PHI+1.                                                        1803.
      DO 220 J=1,MM                                                     1804.
  220 T(MU,J)=A(MU,J)/A(MU+M,MU+M)                                      1805.
      DO 230 I=1,MM                                                     1806.
      IF (I .EQ. MU) GO TO 230                                          1807.
      DO 225 J=1,MM                                                     1808.
      IF (J .EQ. MU) GO TO 225                                          1809.
      T(I,J)=A(I,J)-A(I,MU+M)*A(MU+M,J)/A(MU+M,MU+M)                    1810.
  225 CONTINUE                                                          1811.
  230 CONTINUE                                                          1812.
      DO 240 I=1,MM                                                     1813.
      IF (I .EQ. MU) GO TO 240                                          1814.
      T(I,MU)=A(I,MU)-A(I,MU+M)/A(MU+M,MU+M)                            1815.
  240 CONTINUE                                                          1816.
      DO 245 I=1,MM                                                     1817.
      DO 245 J=1,MM                                                     1818.
  245 A(I,J)=T(I,J)                                                     1819.
      IF (IPRN .LT. 3) GO TO 250                                        1820.
      CALL ANSWER(A,S,XMEAN,SIGMA,IDX,PHI,L,M,MM,PF,MU,ELM)             1821.
  250 CONTINUE                                                          1822.
  300 CONTINUE                                                          1823.
C-----CHECK TERMINATION CONDITION                                       1824.
  400 KOUT=0                                                            1825.
      DO 410 I=1,L                                                      1826.
  410 KOUT=KOUT+IDX(I)                                                  1827.
      B(4)=XMEAN(M)                                                     1828.
      IF (KOUT .NE. 0) GO TO 450                                        1829.
      IF(KF .NE. 1) GO TO 420                                           1830.
      KF = 3                                                            1831.
      GO TO 150                                                         1832.
  420 TEST(3)= TEST(3)/TEST(6)                                          1833.
      FLIM=TEST(3)                                                      1834.
      KF=1                                                              1835.
      KFLAG = 0                                                         1836.
      IF(TEST(6) .GT. 1.) GO TO 150                                     1837.
      KFLAG = 1                                                         1838.
      KF = 4                                                            1839.
      GO TO 150                                                         1840.
C-----COMPUTE REGRESSION CONSTANT,COEFFICIENTS,AND STANDARD ERRORS      1841.
  450 YSE=77.7                                                          1842.
      IF (PHI .GE. 1) YSE=SIGMA(M)*SQRT(ABS(A(M,M)/PHI))                1843.
      DO 500 I=1,L                                                      1844.
      IF (IDX(I) .EQ. 0) GO TO 500                                      1845.
      B(I)=A(I,M)*SQRT(S(M,M)/S(I,I))                                   1846.
      BSE(I)=YSE*SQRT(ABS(A(I+M,I+M)/S(I,I)))                           1847.
      IF(KF .NE. 3) Y(I)=B(I)                                           1848.
      IF(KFLAG .EQ. 0) GO TO 480                                        1849.
      IF(ABS(B(I)) .LE. TEST(6)*BSE(I)) Y(I)=0.                         1850.
  480 IF(PHI .LT. 1.) BSE(I) = 0.                                       1851.
      B(4)=B(4)-Y(I)*XMEAN(I)                                           1852.
  500 CONTINUE                                                          1853.
      IF(KF .NE. 3) Y(4)=B(4)                                           1854.
      TEST(3)=SVTEST                                                    1855.
      RETURN                                                            1856.
      END                                                               1857.
      SUBROUTINE TRVDRV                                                 1858.
C------- COMPUTE TRAVEL TIME AND DERIVATIVES FROM CRUSTAL MODEL --------1859.
      
      REAL*8 TIME1,TIME2                                                1860.
      REAL LAT,LON                                                      1861.
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)   1862.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101)1863.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,    1864.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)     1865.
      COMMON /A10/ ANIN(101),AZ(101),TEMP(101),CA(71),CB(71)            1866.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW   1867.
      COMMON /A17/ TIME1,TIME2,LATR,LONR,KTEST,KAZ,KSORT,KSEL,XFN       1868.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)   1869.
      COMMON /A20/ V(21),D(21),VSQ(21),THK(21),TID(21,21),DID(21,21)    1870.
      COMMON /A22/ F(21,21),G(4,21),H(21),DEPTH(21),IONE                1871.
      COMMON /A24/ FLTEP,IPRO,ISTTT,ISKP(4),AHEAD(12),FLIM,AF(3),NDEC   1872.
      DIMENSION TINJ(21),DIDJ(21),TR(21)                                1873.
C-----------------------------------------------------------------------1874.
      IF (ISW .EQ. IONE) GO TO 5                                        1875.
C-----INITIALIZATION FOR FIXED LAYER MODEL -----------------------------1876.
      DO  1 L=1,NL                                                      1877.
      IF (D(L) .GT. Z) GO TO  2                                         1878.
    1 CONTINUE                                                          1879.
      JL=NL                                                             1880.
      GO TO  3                                                          1881.
    2 JJ=L                                                              1882.
      JL=L-1                                                            1883.
    3 TKJ=Z-D(JL)                                                       1884.
      TKJSQ=TKJ**2+0.000001                                             1885.
      IF (JL .EQ. NL) GO TO  5                                          1886.
      DO  4 L=JJ,NL                                                     1887.
      SQT=SQRT( abs (VSQ(L)-VSQ(JL)))                                   1888.
      TINJ(L)=TID(JL,L)-TKJ*SQT/(V(L)*V(JL))                            1889.
    4 DIDJ(L)=DID(JL,L)-TKJ*V(JL)/SQT                                   1890.
      XOVMAX=V(JJ)*V(JL)*(TINJ(JJ)-TID(JL,JL))/(V(JJ)-V(JL))            1891.
    5 DO 300 I=1,NR                                                     1892.
      IF (ISW .NE. IONE) GO TO 45                                       1893.
C-----INITIALIZATION FOR VARIABLE LAYER MODEL --------------------------1894.
      JI=KDX(I)                                                         1895.
      DEPTH(2)=FLT(KNO,JI)                                              1896.
      IF (Z .LT. FLTEP) DEPTH(2)=0.5*(FLT(KNO,JI)+FLTEP)                1897.
      THK(1)=DEPTH(2)                                                   1898.
      THK(2)=D(3)-DEPTH(2)                                              1899.
      DH1=THK(1)-H(1)                                                   1900.
      DH2=THK(2)-H(2)                                                   1901.
      DO 10 L=1,NL                                                      1902.
      IF (DEPTH(L) .GT. Z) GO TO 20                                     1903.
   10 CONTINUE                                                          1904.
      JL=NL                                                             1905.
      GO TO 30                                                          1906.
   20 JJ=L                                                              1907.
      JL=L-1                                                            1908.
   30 TKJ=Z-DEPTH(JL)                                                   1909.
      TKJSQ=TKJ**2+0.000001                                             1910.
      IF (JL .EQ. NL) GO TO 100                                         1911.
C-----CALCULATION FOR REFRACTED WAVES ----------------------------------1912.
      DO 40 L=JJ,NL                                                     1913.
      SQT=SQRT(VSQ(L)-VSQ(JL))                                          1914.
      TIX=F(1,JL)*DH1*G(1,L)+F(2,JL)*DH2*G(2,L)+TID(JL,L)               1915.
      DIX=F(1,JL)*DH1*G(3,L)+F(2,JL)*DH2*G(4,L)+DID(JL,L)               1916.
      TINJ(L)=TIX-TKJ*SQT/(V(L)*V(JL))                                  1917.
   40 DIDJ(L)=DIX-TKJ*V(JL)/SQT                                         1918.
      TIX=F(1,JL)*DH1*G(1,JL)+F(2,JL)*DH2*G(2,JL)+TID(JL,JL)            1919.
      XOVMAX=V(JJ)*V(JL)*(TINJ(JJ)-TIX)/(V(JJ)-V(JL))                   1920.
      GO TO 50                                                          1921.
   45 IF (JL .EQ. NL) GO TO 100                                         1922.
   50 DO 60 M=JJ,NL                                                     1923.
   60 TR(M)=TINJ(M)+DELTA(I)/V(M)                                       1924.
      TMIN=999.99                                                       1925.
      DO 70 M=JJ,NL                                                     1926.
      IF (TR(M) .GT. TMIN) GO TO 70                                     1927.
      IF (DIDJ(M) .GT. DELTA(I)) GO TO 70                               1928.
      K=M                                                               1929.
      TMIN=TR(M)                                                        1930.
   70 CONTINUE                                                          1931.
      IF (DELTA(I) .LT. XOVMAX) GO TO 90                                1932.
C-----TRAVEL TIME + DERIVATIVES FOR REFRACTED WAVE                      1933.
   80 T(I)=TR(K)                                                        1934.
      if (v(k) .eq. 0.0) v(k) = 0.00001
      DTDD=1.0/V(K)                                                     1935.
      DTDH=-SQRT   ( abs (VSQ(K)-VSQ(JL)) ) / ( V(K) * V(JL) )          1936.
      ANIN(I)=-V(JL)/V(K)                                               1937.
      GO TO 260                                                         1938.
C-----CALCULATION FOR DIRECT WAVE --------------------------------------1939.
   90 IF (JL .NE. 1) GO TO 100                                          1940.
      SQT=SQRT(ZSQ+DELTA(I)**2)                                         1941.
      TDJ1=SQT/V(1)                                                     1942.
      IF (TDJ1 .GE. TMIN) GO TO 80                                      1943.
C-----TRAVEL TIME + DERIVATIVES FOR DIRECT WAVE IN FIRST LAYER          1944.
      T(I)=TDJ1                                                         1945.
      DTDD=DELTA(I)/(V(1)*SQT)                                          1946.
      DTDH=Z/(V(1)*SQT)                                                 1947.
      ANIN(I)=DELTA(I)/SQT                                              1948.
      GO TO 260                                                         1949.
C-----FIND A DIRECT WAVE THAT WILL EMERGE AT THE STATION                1950.
  100 XBIG=DELTA(I)                                                     1951.
      XLIT=DELTA(I)*TKJ/Z                                               1952.
      UB=XBIG/SQRT(XBIG**2+TKJSQ)                                       1953.
      UL=XLIT/SQRT(XLIT**2+TKJSQ)                                       1954.
      UBSQ=UB**2                                                        1955.
      ULSQ=UL**2                                                        1956.
      DELBIG=TKJ*UB/SQRT(1.000001-UBSQ)                                 1957.
      DELLIT=TKJ*UL/SQRT(1.000001-ULSQ)                                 1958.
      J1=JL-1                                                           1959.
      DO 110 L=1,J1                                                     1960.
      DELBIG=DELBIG+(THK(L)*UB)/SQRT(abs (VSQ(JL)/VSQ(L)-UBSQ))         1961.
  110 DELLIT=DELLIT+(THK(L)*UL)/SQRT(abs (VSQ(JL)/VSQ(L)-ULSQ))         1962.
      DO 170 LL=1,25                                                    1963.
      IF (DELBIG-DELLIT .LT. 0.02) GO TO 180                            1964.
      XTR=XLIT+(DELTA(I)-DELLIT)*(XBIG-XLIT)/(DELBIG-DELLIT)            1965.
      U=XTR/SQRT(XTR**2+TKJSQ)                                          1966.
      USQ=U**2                                                          1967.
      DELXTR=TKJ*U/SQRT(1.000001-USQ)                                   1968.
      DO 120 L=1,J1                                                     1969.
  120 DELXTR=DELXTR+(THK(L)*U)/SQRT(abs (VSQ(JL)/VSQ(L)-USQ))           1970.
      XTEST=DELTA(I)-DELXTR                                             1971.
      IF (ABS(XTEST) .LE. 0.02) GO TO 190                               1972.
      IF (XTEST) 140,190,150                                            1973.
  140 XBIG=XTR                                                          1974.
      DELBIG=DELXTR                                                     1975.
      GO TO 160                                                         1976.
  150 XLIT=XTR                                                          1977.
      DELLIT=DELXTR                                                     1978.
  160 IF (LL .LT. 10) GO TO 170                                         1979.
      IF (1.0-U .LT. 0.0002) GO TO 190                                  1980.
  170 CONTINUE                                                          1981.
  180 XTR=0.5*(XBIG+XLIT)                                               1982.
      U=XTR/SQRT(XTR**2+TKJSQ)                                          1983.
      USQ=U**2                                                          1984.
  190 IF (1.0-U .GT. 0.0002) GO TO 220                                  1985.
C-----IF U IS TOO NEAR 1, COMPUTE TDIR AS WAVE ALONG THE TOP OF LAYER JL1986.
      IF (ISW .EQ. IONE) GO TO 195                                      1987.
      TDC=TID(JL,JL)+DELTA(I)/V(JL)                                     1988.
      GO TO 200                                                         1989.
  195 TIX=F(1,JL)*DH1*G(1,JL)+F(2,JL)*DH2*G(2,JL)+TID(JL,JL)            1990.
      TDC=TIX+DELTA(I)/V(JL)                                            1991.
  200 IF (JL .EQ. NL) GO TO 210                                         1992.
      IF (TDC .GE. TMIN) GO TO 80                                       1993.
  210 T(I)=TDC                                                          1994.
      DTDD=1.0/V(JL)                                                    1995.
      DTDH=0.0                                                          1996.
      ANIN(I)=0.9999999                                                 1997.
      GO TO 260                                                         1998.
C-----TRAVEL TIME + DERIVATIVES FOR DIRECT WAVE BELOW FIRST LAYER       1999.
  220 TDIR=TKJ/(V(JL)*SQRT(abs (1.0-USQ)))                                    2000.
      DO 240 L=1,J1                                                     2001.
  240 TDIR=TDIR+(THK(L)*V(JL))/(VSQ(L)*SQRT(abs (VSQ(JL)/VSQ(L)-USQ)))  2002.
      IF (JL .EQ. NL) GO TO 245                                         2003.
      IF (TDIR .GE. TMIN) GO TO 80                                      2004.
  245 T(I)=TDIR                                                         2005.
      SRR=SQRT(abs (1.-USQ))                                                  2006.
      SRT=SRR**3                                                        2007.
      ALFA=TKJ/SRT                                                      2008.
      BETA=TKJ*U/(V(JL)*SRT)                                            2009.
      DO 250 L=1,J1                                                     2010.
      STK=(SQRT(abs (VSQ(JL)/VSQ(L)-USQ)))**3                           2011.
      VTK=THK(L)/(VSQ(L)*STK)                                           2012.
      ALFA=ALFA+VTK*VSQ(JL)                                             2013.
  250 BETA=BETA+VTK*V(JL)*U                                             2014.
      DTDD=BETA/ALFA                                                    2015.
      DTDH=(1.0-V(JL)*U*DTDD)/(V(JL)*SRR)                               2016.
      ANIN(I)=U                                                         2017.
C-----SET UP PARTIAL DERIVATIVES FOR REGRESSION ANALYSIS ---------------2018.
  260 X(1,I)=-DTDD*DX(I)/DELTA(I)                                       2019.
      X(2,I)=-DTDD*DY(I)/DELTA(I)                                       2020.
      X(3,I)=DTDH                                                       2021.
  300 CONTINUE                                                          2022.
      RETURN                                                            2023.
      END                                                               2024.
      SUBROUTINE XFMAGS                                                 2025.
C------- COMPUTE X-MAGNITUDE AND F-MAGNITUDE ---------------------------2026.
      
      REAL LAT,LON,MAG                                                  2027.
      COMMON /A1/ NSTA(151),DLY(2,151),FMGC(151),XMGC(151),KLAS(151),   2028.
     1       PRR(151),CALR(151),ICAL(151),IS(151),NDATE(151),NHRMN(151) 2029.
      COMMON /A2/ LAT(151),LON(151),DELTA(101),DX(101),DY(101),T(101)   2030.
      COMMON /A5/ ZTR,XNEAR,XFAR,POS,IQ,KMS,KFM,IPUN,IMAG,IR,QSPA(9,40) 2031.
      COMMON /A6/ NMAX,LMAX,NS,NL,MMAX,NR,FNO,Z,X(4,101),ZSQ,NRP,DF(101)2032.
      COMMON /A8/ CAL(101),XMAG(101),FMAG(101),NM,AVXM,SDXM,NF,AVFM,    2033.
     1       SDFM,MAG,KDX(101),AMX(101),PRX(101),CALX(101),FMP(101)     2034.
      COMMON /A16/ KLSS(151),CALS(151),MDATE(151),MHRMN(151),IPRN,ISW   2035.
      COMMON /A19/ KNO,IELV(151),TEST(15),FLT(2,151),MNO(151),IW(151)   2036.
      DIMENSION RSPA(8,20)                                              2037.
      DATA ZMC1,ZMC2,PWC1,PWC2/0.15,3.38,0.80,1.50/,BLANK/'    '/       2038.
      DATA RSPA/-0.02, 1.05,-0.15,-0.13, 0.66, 0.55, 0.17, 0.42,        2039.
     2           0.14, 1.18,-0.01, 0.01, 0.79, 0.66, 0.27, 0.64,        2040.
     3           0.30, 1.29, 0.12, 0.14, 0.90, 0.76, 0.35, 0.84,        2041.
     4           0.43, 1.40, 0.25, 0.27, 1.00, 0.86, 0.43, 0.95,        2042.
     5           0.55, 1.49, 0.38, 0.41, 1.08, 0.93, 0.49, 1.04,        2043.
     6           0.65, 1.57, 0.53, 0.57, 1.16, 1.00, 0.55, 1.13,        2044.
     7           0.74, 1.63, 0.71, 0.75, 1.23, 1.07, 0.63, 1.24,        2045.
     8           0.83, 1.70, 0.90, 0.95, 1.30, 1.15, 0.72, 1.40,        2046.
     9           0.92, 1.77, 1.07, 1.14, 1.38, 1.25, 0.83, 1.50,        2047.
     A           1.01, 1.86, 1.23, 1.28, 1.47, 1.35, 0.95, 1.62,        2048.
     B           1.11, 1.96, 1.35, 1.40, 1.57, 1.46, 1.08, 1.73,        2049.
     C           1.20, 2.05, 1.45, 1.49, 1.67, 1.56, 1.19, 1.84,        2050.
     D           1.30, 2.14, 1.55, 1.58, 1.77, 1.66, 1.30, 1.94,        2051.
     E           1.39, 2.24, 1.65, 1.67, 1.86, 1.76, 1.40, 2.04,        2052.
     F           1.47, 2.33, 1.74, 1.76, 1.95, 1.85, 1.50, 2.14,        2053.
     G           1.53, 2.41, 1.81, 1.83, 2.03, 1.93, 1.58, 2.24,        2054.
     H           1.56, 2.45, 1.85, 1.87, 2.07, 1.97, 1.62, 2.31,        2055.
     I           1.53, 2.44, 1.84, 1.86, 2.06, 1.96, 1.61, 2.31,        2056.
     J           1.43, 2.36, 1.76, 1.78, 1.98, 1.88, 1.53, 1.92,        2057.
     K           1.25, 2.18, 1.59, 1.61, 1.82, 1.72, 1.37, 1.49/        2058.
C-----------------------------------------------------------------------2059.
      NM=0                                                              2060.
      AVXM=0.                                                           2061.
      SDXM=0.                                                           2062.
      NF=0                                                              2063.
      AVFM=0.                                                           2064.
      SDFM=0.                                                           2065.
      DO 40 I=1,NRP                                                     2066.
      XMAG(I)=BLANK                                                     2067.
      RAD2=DELTA(I)**2+ZSQ                                              2068.
      IF ((RAD2.LT.1.).OR.(RAD2.GT.600000.)) GO TO 30                   2069.
      JI=KDX(I)                                                         2070.
      K=KLAS(JI)                                                        2071.
      AMXI=ABS(AMX(I))                                                  2072.
      CAL(I)=CALX(I)                                                    2073.
      IF ((CAL(I).LT.0.01).OR.(ICAL(JI).EQ.1)) CAL(I)=CALR(JI)          2074.
      IF ((AMXI.LT.0.01).OR.(CAL(I).LT.0.01)) GO TO 30                  2075.
      IF ((K.LT.0).OR.(K.GT.8)) GO TO 30                                2076.
      XLMR=0.                                                           2077.
      IF (K .EQ. 0) GO TO 20                                            2078.
      PRXI=PRX(I)                                                       2079.
      IF (PRXI .LT. 0.01) PRXI=PRR(JI)                                  2080.
      IF (IR .EQ. 0) GO TO 10                                           2081.
      IF ((PRXI.GT.20.).OR.(PRXI.LT.0.033)) GO TO 30                    2082.
      FQ=10.*ALOG10(1./PRXI)+20.                                        2083.
      IFQ=FQ                                                            2084.
      XLMR=QSPA(K,IFQ)+(FQ-IFQ)*(QSPA(K,IFQ+1)-QSPA(K,IFQ))             2085.
      GO TO 20                                                          2086.
   10 IF ((PRXI.GT.3.).OR.(PRXI.LT.0.05)) GO TO 30                      2087.
      FQ=10.*ALOG10(1./PRXI)+6.                                         2088.
      IFQ=FQ                                                            2089.
      XLMR=RSPA(K,IFQ)+(FQ-IFQ)*(RSPA(K,IFQ+1)-RSPA(K,IFQ))             2090.
   20 BLAC=ALOG10(AMXI/(2.*CAL(I)))-XLMR                                2091.
      RLD2=ALOG10(RAD2)                                                 2092.
      BLNT=ZMC1-PWC1*RLD2                                               2093.
      IF (RAD2 .GE. 40000.) BLNT=ZMC2-PWC2*RLD2                         2094.
      XMAG(I)=BLAC-BLNT+XMGC(JI)                                        2095.
      NM=NM+1                                                           2096.
      AVXM=AVXM+XMAG(I)                                                 2097.
      SDXM=SDXM+XMAG(I)**2                                              2098.
   30 FMAG(I)=BLANK                                                     2099.
      IF (FMP(I) .EQ. BLANK) GO TO 40                                   2100.
      FMAG(I)=TEST(7)+TEST(8)*ALOG10(FMP(I))+TEST(9)*DELTA(I)+FMGC(JI)  2101.
      NF=NF+1                                                           2102.
      AVFM=AVFM+FMAG(I)                                                 2103.
      SDFM=SDFM+FMAG(I)**2                                              2104.
   40 CONTINUE                                                          2105.
      IF (NM .EQ. 0) GO TO 50                                           2106.
      AVXM=AVXM/NM                                                      2107.
      SDXM=SQRT(abs (SDXM/NM-AVXM**2))                                        2108.
   50 IF (NF .EQ. 0) GO TO 60                                           2109.
      AVFM=AVFM/NF                                                      2110.
      SDFM=SQRT(abs (SDFM/NF-AVFM**2))
   60 IF (NM .EQ. 0) AVXM=BLANK                                         2112.
      IF (NF .EQ. 0) AVFM=BLANK                                         2113.
      IF (IMAG-1) 70,80,90                                              2114.
   70 MAG=AVXM                                                          2115.
      RETURN                                                            2116.
   80 MAG=AVFM                                                          2117.
      RETURN                                                            2118.
   90 MAG=0.5*(AVXM+AVFM)                                               2119.
      IF (AVXM .EQ. BLANK) GO TO 80                                     2120.
      IF (AVFM .EQ. BLANK) GO TO 70                                     2121.
      RETURN                                                            2122.
      END                                                               2123.
