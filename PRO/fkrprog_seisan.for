C     F-K.PROG
C---------------------------------------
C  For changes for SEISAN, look for cjh
C---------------------------------------
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                    FREQUENCY WAVENUMBER-INTEGRATION PROGRAM.
C
C                               REFRENCES :
C
C                   1.  DUNKIN J. W.      (1965) BSSA 335-358
C                   2.  HASKELL N. A.     (1964) BSSA 337-393
C                   3.  WANG AND HERRMANN (1980) BSSA 1015-1036
C                   4.  WATSON T. H.      (1970) BSSA 161-166
C
C                            WRITTEN BY CHANDAN K. SAIKIA
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
changes:
c
c   dec 22 2011 jh: always overwrite output file
c                   add reading of station codes and s-file name






      PARAMETER(LER=0,LIN=5,LOT=6,NL=70,IB=100)
      COMMON/FILT/WVCM,WVC1,WVC2,WVCN
      COMMON/SAIK/JBDRY,VBMIN,ALPHA,XLENG,XFAC,DEPTH
      COMMON/MODEL/D(NL),A(NL),B(NL),RHO(NL),MMAX,QA(NL),QB(NL)
      COMMON/CNTR/CMAX,C1,C2,CMIN,VRED(100),T0(100)
      COMMON/BLK12/FACT(IB)
      COMMON/LGG1/GG(100,10)
      DIMENSION RANGE(100),ISRC(10)
cjh added stat and s-file
      character*5 stat(100)
      character*80 sfile
      COMPLEX*16 GG
      REAL*8 FACT
      CHARACTER*50 NAME2*50,ISTAT3*3
      LOGICAL PRNTA,IXST3
   59 FORMAT(' ',A)
   60 FORMAT(A)
      PRNTA=.FALSE.
cjh  added sfile
      READ(LIN,'(L3,1x,a)') PRNTA,sfile
      READ(LIN,21) M1,M2
      READ(LIN,18)NAME2
      IFREQ=0
   18 FORMAT(A)
   19 FORMAT(2I5)
C     READ(LIN,20) DECAY,DEPTH,N1,N2,N,DT,MMAX
C  20 FORMAT(2F10.4,3I5,F10.4,I5)
      READ(LIN,20) DECAY,DEPTH,N1,N2,N,DT,MMAX,NSKIP
   20 FORMAT(2F10.4,3I5,F10.4,I5,I5)
      READ(LIN,21) ISRC , JBDRY
   21 FORMAT(11I5)
      READ(LIN,22)(D(I),A(I),B(I),RHO(I),QA(I),QB(I),I=1,MMAX)

cjh  format below from e to g

   22 FORMAT(4G11.4,2F10.2)
      DO 1 I=1,MMAX
      QA(I)=1./QA(I)
    1 QB(I)=1./QB(I)
      READ(LIN,25) LMAX
      READ(LIN,24) XLENG, XFAC
   24 FORMAT(2E15.7)
      READ(LIN,25) NRANGE,CMAX,C1,C2,CMIN
   25 FORMAT(I5,4F10.4)
      DO 2 I=1,NRANGE
cjh
    2 READ(LIN,26) stat(i),RANGE(I), T0(I), VRED(I)
   26 FORMAT(A5,3F10.4)
C
      IF(CMAX.NE.0.0) CMAX=1./CMAX
      C1=1./C1
      C2=1./C2
      CMIN=1./CMIN
      IF(CMAX.LT.0.0) THEN
          DO 3 I=1,IB
    3     FACT(I)=1.0
      ENDIF
C
C     CHECK IF DEPTH DOES CORRESPOND TO DEPTH INTERFACE +++++++++++++++>
C
      DPH=0.0
      DO 4 I=1,LMAX-1
    4 DPH=DPH+D(I)
      IF(ABS(DPH-DEPTH).LE.1.0E-01) DEPTH=DPH
      IF(ABS(DPH-DEPTH).GT.1.0E-01) GO TO 9999
C
C     CALCULATE ALPHA FROM DECAY +++++++++++++++++++++++++++++++>
C
      TLEN=N*DT
      DECAY=1./DECAY
      ALPHA=-LOG(DECAY)
      ALPHA=ALPHA/TLEN
C
C     CHECK BOUCHON'S CRITERIA  ++++++++++++++++++++++++++++++++>
C
      RMAX=-1.0
      VAMAX=0.0
      VBMAX=0.0
      VAMIN=100.0
      VBMIN=100.0
      DO 5 I=1,MMAX
      VAMIN=AMIN1(VAMIN,A(I))
      VAMAX=AMAX1(VAMAX,A(I))
      VBMIN=AMIN1(VBMIN,B(I))
    5 VBMAX=AMAX1(VBMAX,B(I))
      DO 6 I=1,NRANGE
    6 RMAX=AMAX1(RMAX,RANGE(I))
      YMAX=2.*RMAX
      IF(XLENG.LT.YMAX) XLENG=YMAX
      RMAX=RMAX+SQRT(VAMAX*VAMAX*TLEN*TLEN-DEPTH*DEPTH)
      IF(XLENG.LT.RMAX) THEN
            XLENG=RMAX
            MM=XLENG/100.
            XLL=FLOAT(MM)*100.
            IF(XLL.NE.XLENG) XLENG=XLL+100.
      ENDIF
C
      DF = 1./(N*DT)
      NYQ = N/2+1
      FL=0.0
      FU=(NYQ-1)*DF
  100 FORMAT(1H ,4HFL =,F10.5,5X,4HFU =,F10.5,5X,4HDF =,F10.5,/
     $       5X,4HN1 =,I4,5X,4HN2 =,I4,5X,7HDEPTH =,F10.2,4H N =,I5,
     $       5X,7HNSKIP =,I5)
  102 FORMAT(1H ,39HFREQUENCIES FOR WHICH RESPONSE COMPUTED     )
  101 FORMAT(1H ,7HALPHA =,F10.5,5X,4HDT =,F10.3,'  XLENG= ',E12.4
     $, '  XFAC = ',f6.2)
      WRITE(LOT,100)  FL,FU,DF,N1,N2,DEPTH,N,NSKIP
      WRITE(LOT,101)  ALPHA,DT,XLENG,XFAC
      WRITE(LOT,27)(D(I),A(I),B(I),RHO(I),QA(I),QB(I),I=1,MMAX)
   27 FORMAT(6E11.4)
      WRITE(LOT,102)
C
c      INQUIRE(FILE=NAME2,EXIST=IXST3)
c      IF(IXST3)THEN
c            ISTAT3 = 'OLD'
c      ELSE
c            ISTAT3 = 'NEW'
c      ENDIF
c
c   always overwrite
cjh


      OPEN(UNIT=12,FILE=NAME2,STATUS='unknown',ACCESS='SEQUENTIAL',
cjh      OPEN(UNIT=12,FILE=NAME2,STATUS=ISTAT3,ACCESS='SEQUENTIAL',
     $          FORM='UNFORMATTED')
cjh     IF(.NOT.IXST3) THEN
          write(12) sfile   ! jh added sfile
          WRITE(12) ALPHA,DEPTH,FL,FU,DT,N1,N2,DF,NYQ,NRANGE,NSKIP
          WRITE(12) ISRC
          WRITE(12) D,A,B,RHO,MMAX,QA,QB
cjh  added stat
          WRITE(12) stat,RANGE,VRED,T0
c      ELSE
c          CALL RESET3(IFREQ,ISRC,NRANGE,N1,N2)
c      ENDIF
      IF(IFREQ.GT.N1) N1=IFREQ+1
C
      DO 1000 I = N1,N2,NSKIP
      FREQ=(I-1)*DF
      IF(FREQ.LT.DF) FREQ = 0.01*DF
      OMEGA=2.*ACOS(-1.0)*FREQ
C
      WVCM=OMEGA*CMAX
      WVC1=OMEGA*C1
      WVC2=OMEGA*C2
      WVCN=OMEGA*CMIN
C
      CALL EXCIT(OMEGA,ISRC,LMAX,RANGE,NRANGE,NBLK,IBLK)
      WRITE(LOT,111) I,FREQ,NBLK,IBLK
      CALL FLUSH(LOT)
      IF(I.GE.M1.AND.I.LE.M2) THEN
       IF(PRNTA) THEN
            DO 1001 J=1,NRANGE
            WRITE(LOT,*) ' DIST =>   ',J
            DO 1002 K=1,10
            WRITE(LOT,8889) GG(J,K)
 8889       FORMAT(10X,E16.9,4X,E16.9)
 1002       CONTINUE
 1001       CONTINUE
       ENDIF
      ENDIF
C
 1000 CONTINUE
  111 FORMAT(10X,I5,'  F =  ',E12.4,' NBLOCK =  ',I5,' IBLOCK= ',I3)
      XX=-999.0
      T0X=0.0
      WRITE(12) XX,T0X
      CLOSE(12)
      GO TO 9888
 9999 WRITE(LIN,*) ' CHECK TWO PARAMETERS <+> LMAX AND DEPTH'
 9888 CONTINUE
      STOP
      END
C
      SUBROUTINE RESET3(IFREQ,ISRC,NDIST,N1,N2)
C
C     THE FILE IS READ UNTIL AN ERROR IS FOUND, WHICH INDICATES TOTAL
C     NUMBER OF CORRECT FREQUENCIES ON THE OUTPUT FILE.  THE FILE WILL BE
C     REWOND AND CORRECT FREQUENCIES WILL BE READ TO REPOSITION
C
      PARAMETER(NL=70)
      DIMENSION RANGE(100),ISRC(10)
      COMMON/MODEL/D(NL),A(NL),B(NL),RHO(NL),MMAX,QA(NL),QB(NL)
      COMMON/CNTR/CMAX,C1,C2,CMIN,VRED(100),T0(100)
      IFREQ = 0
      REWIND 12
      READ(12) ALPHA,DEPTH,FL,FU,DT,N1,N2,DF,NYQ,NDIST
      READ(12) ISRC
      READ(12) D,A,B,RHO,MMAX,QA,QB
      READ(12) RANGE,VRED,T0
      DO 1 I = N1, N2
            READ(12) OMEGA,NK
            DO 2 JD=1,NDIST
                  DO 3 JJ=1,10
                  IF(ISRC(JJ).EQ.1)READ(12,ERR=9998,END=9999)XX,YY
    3             CONTINUE
    2       CONTINUE
            IFREQ = IFREQ + 1
    1 CONTINUE
 9998 CONTINUE
 9999 CONTINUE
C
C     POSITION THE COUNTER OF FREQUNECIES
C
      REWIND 12
      READ(12) ALPHA,DEPTH,FL,FU,DT,N1,N2,DF,NYQ,NDIST
      READ(12) ISRC
      READ(12) D,A,B,RHO,MMAX,QA,QB
      READ(12) RANGE,VRED,T0
      DO 4 I = 1,IFREQ
            READ(12) OMEGA,NK
            DO 5 JD=1,NDIST
                  DO 6 JJ=1,10
                  IF(ISRC(JJ).EQ.1)READ(12,ERR=9998,END=9999)XX,YY
    6             CONTINUE
    5       CONTINUE
    4 CONTINUE
      IFREQ = IFREQ -1 + N1
      RETURN
      END
C
      SUBROUTINE EXCIT(OMEGA,ISRC,LMAX,RANGE,ND,NBLOCK,IBLOCK)
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     VECTORIZED FREQUENCY-WAVENUMBER CODE +++++ THIS SUBROUTINE CALCULATES
C     THE KERNEL FOR BOTH P-SV AND SH SYSTEM I.E., EIGHT BASIC FAULTS AND
C     FOR THE EXPLOSIVE SOURCE.  SOURCE SHOULD LOCATE AT AN INTERFACE. I HAVE
C     USED HASKELL'S 4X4 MATRIX AND WANG AND HERRMANN'S 6X6 COMPOUND MATRICES.
C     MATRIX MULTIPLICATION IS REDUCED SIGNIFICANTLY BY USING WATSON (1970).
C
C                   OMEGA    = ANGULAR FREQUENCY
C                   ISRC(I)  = I VARIES BETWEEN 1 TO 10
C                   LMAX     = INTERFACE WHERE THE SOURCE IS BUIRED
C                   RANGE    = DISTANCES
C                   ND       = NUMBER OF TOTAL DISTANCES
C                   NBLOCK   = TOTAL NUMBER OF BLOCKS FOR THIS FREQUENCY
C                   IBLOCK   = COUNT IN NBLOCK TH BLOCK
C
C     WRITTEN  BY CHANDAN K. SAIKIA
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PARAMETER(NL=70,IB=100,NR=100)
C
      DIMENSION RANGE(NR),ICNT(NR),ISRC(10),FACT(IB),FACX(IB)
      COMPLEX*16 AA11,AA12,AA13,AA14,AA21,AA22,AA23,AA24,AA31,AA32,AA33,
     $           AA34,AA41,AA42,AA43,AA44,CA11,CA12,CA13,CA14,CA15,CA21,
     $           CA22,CA23,CA24,CA25,CA31,CA32,CA33,CA34,CA35,CA41,CA42,
     $           CA43,CA44,CA45,CA51,CA52,CA53,CA54,CA55,D11,D12,D13,D14
     $           ,D21,D22,D23,D24,D31,D32,D33,D34,D41,D42,D43,D44,YLI,
     $           ZLI,Y11,Y12,Y21,Y22,Y31,Y32,Y41,Y42,R1,R2,R3,R4,R5,R6,
     $           R7,X1,X2,X3,X4,X5,X6,X7,G,G1,G2,G3,G4,G5,G6,G7,G8,G9,
     $           G10,WVNO,WVNO2,WVNO3,AT,ATNA,ATNB,RA,RB,XKA,XKB,XKA2,
     $           XKB2,GAM,GAM2,GAM3,GAM4,GAMX1,GAMX2,GAMM1,GAMM2,GAMM3,
     $           GM,ONE,ZERO,ER1,ER2,ER3,ER4,ER5,S12,S21,S32,S14,S23,
     $           S34,S32E,S34E,ZX,COSQ,Z,COSP,YL,ZL,COSQL,P,Q,W,X,Y,
     $           OM,FOURPI,EPP,EPM,EQP,EQM,CPCQ,CPY,CPZ,CQW,CQX,XY,
     $           XZ,WY,WZ,XZ2,WY2,A0C,TEMP,SINP,SINQ,FRHO,XKK,SMM,GG
     $           ,HH
      COMPLEX*16 QAL,PAL,TWO
      REAL*8 BA
      REAL*8 TEST,TESTT,FAC,PR,QR,PI,QI,EXA,EXB,EXE,EXEL,EXL,EXLL,FACT0,
     $       A0,QMP,ELJ,FACT,FACX,EX,XNORM
      COMMON/LGG1/GG(100,10)
      COMMON/LGK/IASYMP
      COMMON/CNTR/CMAX,C1,C2,CMIN,VRED(100),T0(100)
      COMMON/FILT/WVCM,WVC1,WVC2,WVCN
      COMMON/MODEL/D(NL),A(NL),B(NL),RHO(NL),MMAX,QA(NL),QB(NL)
      COMMON/SAIK/JBDRY,VBMIN,ALPHA,XLENG,XFAC,DEPTH
      COMMON/BLK1/S12(IB),S21(IB),S32(IB),S14(IB),S23(IB),S34(IB)
     $                            ,S32E(IB),S34E(IB)
      COMMON/BLK2/ZX(IB,4,4)
      COMMON/BLK3/R1(IB),R2(IB),R3(IB),R4(IB),R5(IB),R6(IB),R7(IB),
     $            X1(IB),X2(IB),X3(IB),X4(IB),X5(IB),X6(IB),X7(IB)
      COMMON/BLK4/Y11(IB),Y12(IB),Y21(IB),Y22(IB),Y31(IB),Y32(IB),
     $            Y41(IB),Y42(IB)
      COMMON/BLK5/WVNO(IB),WVNO2(IB),WVNO3(IB),ATNA(NL),ATNB(NL),
     $            XKA(NL),XKB(NL)
      COMMON/BLK6/GAM(NL,IB),GAM2(NL,IB),GAM3(NL,IB),GAM4(NL,IB),
     $            GAMX1(NL,IB),GAMX2(NL,IB),GAMM1(NL,IB),GAMM2(NL,IB),
     $            GAMM3(NL,IB),RA(NL,IB),RB(NL,IB)
      COMMON/BLK7/G1(IB),G2(IB),G3(IB),G4(IB),G5(IB),G6(IB),G7(IB),
     $            G8(IB),G9(IB),G10(IB)
      COMMON/BLK8/COSQL(NL,IB),YL(NL,IB),ZL(NL,IB)
      COMMON/BLK9/EXE(IB),EXEL(IB),EXL(IB),EXLL(IB)
      COMMON/BLK11/SMM(NR,10)
      LOGICAL IASYMP
C
      IASYMP=.TRUE.
      ID=0
      ISYST=0
      DO 100 I=1,8
  100 IF(ISRC(I).EQ.1) ID=1
      IF(ID.EQ.1) ISYST=1
      ID=0
      IF(ISRC(9).EQ.1) ID=1
      IF(ISRC(10).EQ.1) ID=1
      IF(ID.EQ.0.AND.ISYST.EQ.0) RETURN
      IF(ID.EQ.1.AND.ISYST.EQ.0) ISYST=2
      IF(ID.EQ.1.AND.ISYST.EQ.1) ISYST=3
C
      DK=2.*ACOS(-1.0)/XLENG
c     IF(CMAX.GE.0.0) THEN
c             MM=(WVC2-WVC1)/DK
c             IF(MM.LT.5) DK=(WVC2-WVC1)/50.
c     ENDIF
C
      WVBM=OMEGA/VBMIN
      WVMM=(5./DEPTH)+XFAC*WVBM
      NK=WVMM/DK
      NK=NK+2
C
      V=6.0/DEPTH
      WVNO(2)=DCMPLX(DBLE(V),0.0D+00)
      WVNO2(2)=WVNO(2)*WVNO(2)
      WVNO3(2)=WVNO2(2)*WVNO(2)
      AKL=(NK-3)*DK+0.218*DK
      IF(AKL.GT.V) IASYMP=.FALSE.
      V=2.5/DEPTH
      WVNO(1)=DCMPLX(DBLE(V),0.0D+00)
      WVNO2(1)=WVNO(1)*WVNO(1)
C
      IF(CMAX.GE.0.0) MM=(WVCN/DK)+2
      IF(NK.GT.MM) NK=MM
      WRITE(12) OMEGA,NK
C
C     DETERMINE INDEX FOR INTEGRATION  I.E., WHEN KR IS GT. 3.0
C
      DO 152 I=1,ND
      DO 153 J=1,50000
      WVV=(J-1)*DK+0.218*DK
      AKR=WVV*RANGE(I)
      IF(AKR.LE.3.0) ICNT(I)=J+2
  153 IF(AKR.GT.3.0) GO TO 152
  152 CONTINUE
C
      OM=DCMPLX(DBLE(OMEGA),-DBLE(ALPHA))
      FOURPI=DBLE(12.5663706)*OM*OM
      ONE=DCMPLX(1.0,0.0)
      ZERO=DCMPLX(0.0,0.0)
      TWO=DCMPLX(DBLE(2.),0.0D+00)
C
      PI=3.141592654
      OM1=6.283185307
      OML=0.06283185307
C
C     COMPUTING THE ATTENUATION OF THE MDEIUM USING QA, QB
C
      AT=ZERO
      IF(ZABS(OM).GT.OML) AT=ZLOG(OM/DBLE(OM1))/DBLE(PI) 
      IF(ZABS(OM).GT.OML) GO TO 1
      FA=SQRT(OML*OML + ALPHA*ALPHA)/OML
      FAC=DBLE(FA)
      AT=ZLOG(DCMPLX(DBLE(OML),-DBLE(ALPHA))/DBLE(OM1*FAC))
     $                           /DBLE(PI)
    1 CONTINUE
      DO 2 I=1,MMAX
      ATNA(I)=ONE
      ATNB(I)=ONE
      IF(QA(I).GT.0.0) THEN
                    PAL=DCMPLX(DBLE(QA(I)),0.0D+00)*AT
                    BA=DBLE(0.5*QA(I))
                    QAL=DCMPLX(0.0D+00,BA)
                    ATNA(I)=ONE+PAL+QAL 
      ENDIF
      IF(QB(I).GT.0.0) THEN
                    PAL=DCMPLX(DBLE(QB(I)),0.0D+00)*AT
                    BA=DBLE(0.5*QB(I))
                    QAL=DCMPLX(0.0D+00,BA)
                    ATNB(I)=ONE+PAL+QAL
      ENDIF
      XKA(I)=OM/(DBLE(A(I))*ATNA(I))
      XKB(I)=OM/(DBLE(B(I))*ATNB(I))
    2 CONTINUE
      FRHO=DCMPLX(DBLE(RHO(LMAX)),0.0D+00)*FOURPI
      XKA2=XKA(LMAX)*XKA(LMAX)
      XKB2=XKB(LMAX)*XKB(LMAX)
      XKK=ATNB(LMAX)*ATNB(LMAX)
      FACXX=1./(12.5663706*B(LMAX)*B(LMAX))
C
      IF(NK.LE.IB) THEN
           IBLOCK=NK
           NBLOCK=1
           LEFT=0
      ELSE
           IBLOCK=IB
           NBLOCK=NK/IB
           LEFT=NK-NBLOCK*IB
           IF(LEFT.NE.0) NBLOCK=NBLOCK+1
      ENDIF
      NBLK=1
      LR=0
C
C     LOOP INCLUDES HALF-SPACE ALSO +++++++++++++++++++++>
      WVNO3(1)=WVNO2(1)*WVNO(1)
      N1=3
  500 CONTINUE
      IF(NBLK.GT.NBLOCK) GO TO 600
      IF(NBLK.EQ.NBLOCK.AND.LEFT.NE.0) THEN
          IBLOCK=LEFT
      ENDIF
      IF(NBLK.GT.1) N1=1
      DO 3 J=N1,IBLOCK
      LR=LR+1
      WV=(LR-1)*DK+0.218*DK
      WVNO(J)=DCMPLX(DBLE(WV),0.0D+00)
      WVNO2(J)=WVNO(J)*WVNO(J)
      WVNO3(J)=WVNO2(J)*WVNO(J)
    3 CONTINUE
      DO 999 J=1,IBLOCK
      R1(J)=ZERO
      R2(J)=ZERO
      R3(J)=ZERO
      R4(J)=ZERO
      R5(J)=ZERO
      R6(J)=ZERO
      R7(J)=ZERO
      X1(J)=ZERO
      X2(J)=ZERO
      X3(J)=ZERO
      X4(J)=ZERO
      X5(J)=ZERO
      X6(J)=ZERO
      X7(J)=ZERO
  999 CONTINUE
C
      DO 4 I=1,MMAX
      DO 4 J=1,IBLOCK
      RA(I,J)=ZSQRT(WVNO2(J)-XKA(I)*XKA(I))
      RB(I,J)=ZSQRT(WVNO2(J)-XKB(I)*XKB(I))
      GM=DBLE(B(I))*WVNO(J)/OM
      GAM(I,J)=GM*ATNB(I)
      GAM(I,J)=2.0D+00*GAM(I,J)*GAM(I,J)
      GAM2(I,J)=GAM(I,J)*GAM(I,J)
      GAM3(I,J)=GAM2(I,J)*GAM(I,J)
      GAM4(I,J)=GAM3(I,J)*GAM(I,J)
      GAMM1(I,J)=GAM(I,J)-ONE
      GAMM2(I,J)=GAMM1(I,J)*GAMM1(I,J)
      GAMM3(I,J)=GAMM1(I,J)*GAMM2(I,J)
      GAMX1(I,J)=GAM(I,J)*GAMM1(I,J)
      GAMX2(I,J)=GAM2(I,J)*GAMM2(I,J)
    4 CONTINUE
C
      EX=0.0D+00
      DO 5 J=1,IBLOCK
      EXE(J)=0.0
      EXEL(J)=0.0
      EXL(J)=0.0
    5 EXLL(J)=0.0
C
      IF(ISYST.EQ.2) GO TO 300
C     COMPUTATION FOR P-SV SYSTEM STARTS +++++++++++++>
C
      DO 6 J=1,IBLOCK
         S14(J)=-TWO*WVNO(J)/FOURPI
         S21(J)=TWO*XKB2/FRHO
         S32(J)=WVNO(J)*4.0D+00*XKA2/FRHO
         TT=(2.*B(LMAX)/A(LMAX))**2-3.
         S34(J)=TWO*WVNO(J)*DCMPLX(DBLE(TT),0.0D+00)/FOURPI 
    6 CONTINUE
C
C     SET UP HALFSPACE BOUNDARY CONDITIONS
C     JBDRY=-1 RIGID;  0= ELASTIC;  +1= FREE SURFACE
C
      M=MMAX
      IF(JBDRY.EQ.0)THEN
        DO 7 J=1,IBLOCK
            RH=RHO(M)*RHO(M)
            R1(J)=DBLE(RH)*(-GAM2(M,J)*RA(M,J)*RB(M,J)+
     $                       WVNO2(J)*GAMM2(M,J))
            R2(J)=-DCMPLX(DBLE(RHO(M)),0.0D+00)*WVNO2(J)*RA(M,J)
            R3(J)=(-GAM(M,J)*RA(M,J)*RB(M,J)+WVNO2(J)*GAMM1(M,J))
            R3(J)=DCMPLX(DBLE(RHO(M)),0.0D+00)*R3(J)
            R4(J)=DCMPLX(DBLE(RHO(M)),0.0D+00)*WVNO2(J)*RB(M,J)
            R5(J)=WVNO2(J)*(WVNO2(J)-RA(M,J)*RB(M,J))
    7   CONTINUE
       ELSEIF(JBDRY.LT.0)THEN
        DO 8 J=1,IBLOCK
            R1(J) = ONE
            R2(J) = ZERO
            R3(J) = ZERO
            R4(J) = ZERO
            R5(J) = ZERO
    8   CONTINUE
       ELSEIF(JBDRY.GT.0)THEN
         DO 9 J=1,IBLOCK
            R1(J) = ZERO
            R2(J) = ZERO
            R3(J) = ZERO
            R4(J) = ZERO
            R5(J) = ONE
    9   CONTINUE
       ENDIF
C
C     MATRIX-MULTIPLICATION - LAYERWISE ++++> FROM BOTTOM UPWARD TO LMAX
C
      DO 10 I=LMAX,MMAX-1
C     MATRIX MULTIPLICATION FROM BOTTOM LAYER UPWARD
      M=MMAX-1+LMAX-I
      DPTH=D(M)
      DO 11 J=1,IBLOCK
      P=RA(M,J)*DCMPLX(DBLE(DPTH),0.0D+00)
      Q=RB(M,J)*DCMPLX(DBLE(DPTH),0.0D+00)
C
      A0=0.0D+00
      PR=REAL(P)
      PI=DIMAG(P)
      QR=REAL(Q)
      QI=DIMAG(Q)
      EPP=DCMPLX(DCOS(PI),DSIN(PI))/DBLE(2.)
      EPM=DCONJG(EPP)
      EQP=DCMPLX(DCOS(QI),DSIN(QI))/DBLE(2.)
      EQM=DCONJG(EQP)
      EXB=QR
      FAC=0.0D+00
      IF(PR.LT.15.0D+00) FAC=DEXP(-DBLE(2.)*PR)
      COSP=EPP + FAC*EPM
      SINP=EPP - FAC*EPM
      W=SINP/RA(M,J)
      X=RA(M,J)*SINP
      FAC=0.0D+00
      IF(QR.LT.15.0D+00) FAC=DEXP(-DBLE(2.)*QR)
      COSQL(M,J)=EQP + FAC*EQM
      SINQ=EQP - FAC*EQM
      YL(M,J)=SINQ/RB(M,J)
      ZL(M,J)=RB(M,J)*SINQ
      EXA=PR + QR
      CPCQ=COSP*COSQL(M,J)
      CPY=COSP*YL(M,J)
      CPZ=COSP*ZL(M,J)
      CQW=COSQL(M,J)*W
      CQX=COSQL(M,J)*X
      XY=X*YL(M,J)
      XZ=X*ZL(M,J)
      WY=W*YL(M,J)
      WZ=W*ZL(M,J)
      FAC=0.0D+00
      QMP=QR-PR
      IF(QMP.GT.-40.0D+00) FAC=DEXP(QMP)
      COSQ=COSQL(M,J)*FAC
      Y=FAC*YL(M,J)
      Z=FAC*ZL(M,J)
      FAC=0.0D+00
      IF(EXA.LT.60.0D+00) A0=DEXP(-EXA)
      RHO2  = RHO(M)*RHO(M)
      A0C  = DBLE(2.)*(A0-CPCQ)
      XZ2  = XZ/WVNO2(J)
      WY2  = WY*WVNO2(J)
      TEMP = A0C*WVNO2(J)+XZ+WY2*WVNO2(J)
      CA15 = -TEMP/DBLE(RHO2)
      TEMP=DBLE(0.5)*A0C*(GAM(M,J)+GAMM1(M,J))+GAM(M,J)*XZ2+
     $                       GAMM1(M,J)*WY2
      CA13= -TEMP/DBLE(RHO(M))
      TEMP = A0C*GAMX1(M,J)+GAM2(M,J)*XZ2+GAMM2(M,J)*WY2
      CA33 = A0+TEMP+TEMP
      CA11 = CPCQ-TEMP
      TEMP = DBLE(0.5)*A0C*GAMX1(M,J)*(GAM(M,J)+GAMM1(M,J))+
     $      GAM3(M,J)*XZ2+GAMM3(M,J)*WY2
      CA31 = DBLE(2.)*TEMP*DBLE(RHO(M))
      TEMP = A0C*GAMX2(M,J)+GAM4(M,J)*XZ2+GAMM3(M,J)*GAMM1(M,J)*WY2
      CA51 = -DBLE(RHO2)*TEMP/WVNO2(J)
      CA14 = (-WVNO2(J)*CQW+CPZ)/DBLE(RHO(M))
      CA21 = (-GAMM2(M,J)*CQW+GAM2(M,J)*CPZ/WVNO2(J))*DBLE(RHO(M))
      CA23 = -(WVNO2(J)*GAMM1(M,J)*CQW-GAM(M,J)*CPZ)/WVNO2(J)
      CA12 = (-CQX+WVNO2(J)*CPY)/DBLE(RHO(M))
      CA32 = WVNO2(J)*(GAM(M,J)*CQX/WVNO2(J)-GAMM1(M,J)*CPY)*DBLE(2.)
      CA41 = (-GAM2(M,J)*CQX/WVNO2(J)+GAMM2(M,J)*CPY)*DBLE(RHO(M))
      CA22 = CPCQ
      CA24 = -WZ
      CA42 = -XY
      CA25=CA14
      CA55=CA11
      CA54=CA21
      CA53=-CA31/(DBLE(2.)*WVNO2(J))
      CA52=CA41
      CA43= -CA32/(DBLE(2.)*WVNO2(J))
      CA45=CA12
      CA44=CA22
      CA34=-DBLE(2.)*WVNO2(J)*CA23
      CA35=-DBLE(2.)*WVNO2(J)*CA13
C
      EXE(J)=EXE(J)+EXA
C
      ER1=R1(J)*CA11+R2(J)*CA21+R3(J)*CA31+R4(J)*CA41+R5(J)*CA51
      ER2=R1(J)*CA12+R2(J)*CA22+R3(J)*CA32+R4(J)*CA42+R5(J)*CA52
      ER3=R1(J)*CA13+R2(J)*CA23+R3(J)*CA33+R4(J)*CA43+R5(J)*CA53
      ER4=R1(J)*CA14+R2(J)*CA24+R3(J)*CA34+R4(J)*CA44+R5(J)*CA54
      ER5=R1(J)*CA15+R2(J)*CA25+R3(J)*CA35+R4(J)*CA45+R5(J)*CA55
C
      TESTT = 0.0D+00
      IF(ABS(REAL(ER1)).GT.TESTT) TESTT=ABS(REAL(ER1))
      IF(ABS(REAL(ER2)).GT.TESTT) TESTT=ABS(REAL(ER2))
      IF(ABS(REAL(ER3)).GT.TESTT) TESTT=ABS(REAL(ER3))
      IF(ABS(REAL(ER4)).GT.TESTT) TESTT=ABS(REAL(ER4))
      IF(ABS(REAL(ER5)).GT.TESTT) TESTT=ABS(REAL(ER5))
      IF(DABS(DIMAG(ER1)).GT.TESTT) TESTT=DABS(DIMAG(ER1))
      IF(DABS(DIMAG(ER2)).GT.TESTT) TESTT=DABS(DIMAG(ER2))
      IF(DABS(DIMAG(ER3)).GT.TESTT) TESTT=DABS(DIMAG(ER3))
      IF(DABS(DIMAG(ER4)).GT.TESTT) TESTT=DABS(DIMAG(ER4))
      IF(DABS(DIMAG(ER5)).GT.TESTT) TESTT=DABS(DIMAG(ER5))
      IF(TESTT.LT.1.0E-30) TESTT=1.0D+00
C
      TESTT=TESTT
      TEST = 0.0D+00
      FAC=ZABS(ER1)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER2)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER3)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER4)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER5)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      TEST = TEST*TESTT
      IF(TEST.LT.1.0E-30) TEST=1.0
      XNORM = 1./TEST
      EXA =-DLOG(XNORM)
      R1(J)=ER1*XNORM
      R2(J)=ER2*XNORM
      R3(J)=ER3*XNORM
      R4(J)=ER4*XNORM
      R5(J)=ER5*XNORM
C
      EXE(J)=EXE(J) + EXA
      EXEL(J) = EXEL(J) + EXB
C
   11 CONTINUE
   10 CONTINUE
C
C     STORE PREVIOUS MATRIX INTO A SEPARATE BUFFER +++++++++++>
C
      DO 12 J=1,IBLOCK
      X1(J)=R1(J)
      X2(J)=R2(J)
      X3(J)=R3(J)
      X4(J)=R4(J)
      X5(J)=R5(J)
      EXL(J)=EXE(J)
      EXLL(J)=EXEL(J)
      DO 13 K=1,4
      DO 13 L=1,4
      ZX(J,K,L)=(0.0,0.0)
   13 IF(K.EQ.L) ZX(J,K,L)=(1.0,0.0)
   12 CONTINUE
C
      DO 14 I=1,LMAX-1
C     MATRIX MULTIPLICATION FROM THE SOURCE INTERFACE UPWARD TO THE SURFACE
      M=LMAX-I
      DPTH=D(M)
      DO 15 J=1,IBLOCK
      P=RA(M,J)*DBLE(DPTH)
      Q=RB(M,J)*DBLE(DPTH)
C
      A0=0.0D+00
      PR=REAL(P)
      PI=DIMAG(P)
      QR=REAL(Q)
      QI=DIMAG(Q)
      EPP=DCMPLX(DCOS(PI),DSIN(PI))/DBLE(2.)
      EPM=DCONJG(EPP)
      EQP=DCMPLX(DCOS(QI),DSIN(QI))/DBLE(2.)
      EQM=DCONJG(EQP)
      EX=PR
      EXB=QR
      FAC=0.0D+00
      IF(PR.LT.15.0D+00) FAC=DEXP(-DBLE(2.)*PR)
      COSP=EPP + FAC*EPM
      SINP=EPP - FAC*EPM
      W=SINP/RA(M,J)
      X=RA(M,J)*SINP
      FAC=0.0D+00
      IF(QR.LT.15.0D+00) FAC=DEXP(-DBLE(2.)*QR)
      COSQL(M,J)=EQP + FAC*EQM
      SINQ=EQP - FAC*EQM
      YL(M,J)=SINQ/RB(M,J)
      ZL(M,J)=RB(M,J)*SINQ
      EXA=PR + QR
      CPCQ=COSP*COSQL(M,J)
      CPY=COSP*YL(M,J)
      CPZ=COSP*ZL(M,J)
      CQW=COSQL(M,J)*W
      CQX=COSQL(M,J)*X
      XY=X*YL(M,J)
      XZ=X*ZL(M,J)
      WY=W*YL(M,J)
      WZ=W*ZL(M,J)
      FAC=0.0D+00
      QMP=QR-PR
      IF(QMP.GT.-40.0D+00) FAC=DEXP(QMP)
      COSQ=COSQL(M,J)*FAC
      Y=FAC*YL(M,J)
      Z=FAC*ZL(M,J)
      FAC=0.0D+00
      IF(EXA.LT.60.0D+00) A0=DEXP(-EXA)
      RHO2  = RHO(M)*RHO(M)
      A0C  = DBLE(2.)*(A0-CPCQ)
      XZ2  = XZ/WVNO2(J)
      WY2  = WY*WVNO2(J)
      TEMP = A0C*WVNO2(J)+XZ+WY2*WVNO2(J)
      CA15 = -TEMP/DBLE(RHO2)
      TEMP=DBLE(0.5)*A0C*(GAM(M,J)+GAMM1(M,J))+GAM(M,J)*XZ2+
     $                       GAMM1(M,J)*WY2
      CA13= -TEMP/DBLE(RHO(M))
      TEMP = A0C*GAMX1(M,J)+GAM2(M,J)*XZ2+GAMM2(M,J)*WY2
      CA33 = A0+TEMP+TEMP
      CA11 = CPCQ-TEMP
      TEMP = DBLE(0.5)*A0C*GAMX1(M,J)*(GAM(M,J)+GAMM1(M,J))+
     $      GAM3(M,J)*XZ2+GAMM3(M,J)*WY2
      CA31 = DBLE(2.)*TEMP*DBLE(RHO(M))
      TEMP = A0C*GAMX2(M,J)+GAM4(M,J)*XZ2+GAMM3(M,J)*GAMM1(M,J)*WY2
      CA51 = -DBLE(RHO2)*TEMP/WVNO2(J)
      CA14 = (-WVNO2(J)*CQW+CPZ)/DBLE(RHO(M))
      CA21 = (-GAMM2(M,J)*CQW+GAM2(M,J)*CPZ/WVNO2(J))*DBLE(RHO(M))
      CA23 = -(WVNO2(J)*GAMM1(M,J)*CQW-GAM(M,J)*CPZ)/WVNO2(J)
      CA12 = (-CQX+WVNO2(J)*CPY)/DBLE(RHO(M))
      CA32 = WVNO2(J)*(GAM(M,J)*CQX/WVNO2(J)-GAMM1(M,J)*CPY)*DBLE(2.)
      CA41 = (-GAM2(M,J)*CQX/WVNO2(J)+GAMM2(M,J)*CPY)*DBLE(RHO(M))
      CA22 = CPCQ
      CA24 = -WZ
      CA42 = -XY
      CA25=CA14
      CA55=CA11
      CA54=CA21
      CA53=-CA31/(DBLE(2.)*WVNO2(J))
      CA52=CA41
      CA43= -CA32/(DBLE(2.)*WVNO2(J))
      CA45=CA12
      CA44=CA22
      CA34=-DBLE(2.)*WVNO2(J)*CA23
      CA35=-DBLE(2.)*WVNO2(J)*CA13
C
      EXE(J)=EXE(J)+EXA
      ER1=R1(J)*CA11+R2(J)*CA21+R3(J)*CA31+R4(J)*CA41+R5(J)*CA51
      ER2=R1(J)*CA12+R2(J)*CA22+R3(J)*CA32+R4(J)*CA42+R5(J)*CA52
      ER3=R1(J)*CA13+R2(J)*CA23+R3(J)*CA33+R4(J)*CA43+R5(J)*CA53
      ER4=R1(J)*CA14+R2(J)*CA24+R3(J)*CA34+R4(J)*CA44+R5(J)*CA54
      ER5=R1(J)*CA15+R2(J)*CA25+R3(J)*CA35+R4(J)*CA45+R5(J)*CA55
C
      TESTT = 0.0D+00
      IF(ABS(REAL(ER1)).GT.TESTT) TESTT=ABS(REAL(ER1))
      IF(ABS(REAL(ER2)).GT.TESTT) TESTT=ABS(REAL(ER2))
      IF(ABS(REAL(ER3)).GT.TESTT) TESTT=ABS(REAL(ER3))
      IF(ABS(REAL(ER4)).GT.TESTT) TESTT=ABS(REAL(ER4))
      IF(ABS(REAL(ER5)).GT.TESTT) TESTT=ABS(REAL(ER5))
      IF(DABS(DIMAG(ER1)).GT.TESTT) TESTT=DABS(DIMAG(ER1))
      IF(DABS(DIMAG(ER2)).GT.TESTT) TESTT=DABS(DIMAG(ER2))
      IF(DABS(DIMAG(ER3)).GT.TESTT) TESTT=DABS(DIMAG(ER3))
      IF(DABS(DIMAG(ER4)).GT.TESTT) TESTT=DABS(DIMAG(ER4))
      IF(DABS(DIMAG(ER5)).GT.TESTT) TESTT=DABS(DIMAG(ER5))
      IF(TESTT.LT.1.0D-30)TESTT=1.0D+00
C
      TESTT=TESTT
      TEST = 0.0D+00
      FAC=ZABS(ER1)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER2)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER3)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER4)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      FAC=ZABS(ER5)/TESTT
      IF(TEST.LT.FAC) TEST = FAC
      TEST = TEST*TESTT
      IF(TEST.LT.1.0D-30) TEST=1.0
      XNORM = 1./TEST
      EXA =-DLOG(XNORM)
C
      R1(J)=ER1*XNORM
      R2(J)=ER2*XNORM
      R3(J)=ER3*XNORM
      R4(J)=ER4*XNORM
      R5(J)=ER5*XNORM
C
      EXE(J)=EXE(J) + EXA
      EXEL(J) = EXEL(J) + EXB
C
C     PROPAGATION OF 4X4 HASKEL MATRIX UPWARD ++++++++++++>
      EXL(J)=EXL(J)+EX
      AA11=GAM(M,J)*COSP - GAMM1(M,J)*COSQ
      AA12=-GAMM1(M,J)*W+GAM(M,J)*Z/WVNO2(J)
      AA13=-(COSP-COSQ)/DBLE(RHO(M))
      AA14=(WVNO2(J)*W-Z)/DBLE(RHO(M))
      AA21= GAM(M,J)*X - WVNO2(J)*GAMM1(M,J)*Y
      AA22= -GAMM1(M,J)*COSP+GAM(M,J)*COSQ
      AA23=(-X+WVNO2(J)*Y)/DBLE(RHO(M))
      AA24= - WVNO2(J)*AA13
      AA31= DBLE(RHO(M))*GAM(M,J)*GAMM1(M,J)*(COSP-COSQ)
      AA32=DBLE(RHO(M))*(-GAMM2(M,J)*W+GAM2(M,J)*Z/WVNO2(J))
      AA34=-WVNO2(J)*AA12
      AA33=AA22
      AA41=DBLE(RHO(M))*(GAM2(M,J)*X/WVNO2(J) - GAMM2(M,J)*Y)
      AA42=-AA31/WVNO2(J)
      AA43=-AA21/WVNO2(J)
      AA44=AA11
C
      D11=ZX(J,1,1)*AA11+ZX(J,1,2)*AA21+ZX(J,1,3)*AA31+ZX(J,1,4)*AA41
      D21=ZX(J,2,1)*AA11+ZX(J,2,2)*AA21+ZX(J,2,3)*AA31+ZX(J,2,4)*AA41
      D31=ZX(J,3,1)*AA11+ZX(J,3,2)*AA21+ZX(J,3,3)*AA31+ZX(J,3,4)*AA41
      D41=ZX(J,4,1)*AA11+ZX(J,4,2)*AA21+ZX(J,4,3)*AA31+ZX(J,4,4)*AA41
C
      D12=ZX(J,1,1)*AA12+ZX(J,1,2)*AA22+ZX(J,1,3)*AA32+ZX(J,1,4)*AA42
      D22=ZX(J,2,1)*AA12+ZX(J,2,2)*AA22+ZX(J,2,3)*AA32+ZX(J,2,4)*AA42
      D32=ZX(J,3,1)*AA12+ZX(J,3,2)*AA22+ZX(J,3,3)*AA32+ZX(J,3,4)*AA42
      D42=ZX(J,4,1)*AA12+ZX(J,4,2)*AA22+ZX(J,4,3)*AA32+ZX(J,4,4)*AA42
C
      D13=ZX(J,1,1)*AA13+ZX(J,1,2)*AA23+ZX(J,1,3)*AA33+ZX(J,1,4)*AA43
      D23=ZX(J,2,1)*AA13+ZX(J,2,2)*AA23+ZX(J,2,3)*AA33+ZX(J,2,4)*AA43
      D33=ZX(J,3,1)*AA13+ZX(J,3,2)*AA23+ZX(J,3,3)*AA33+ZX(J,3,4)*AA43
      D43=ZX(J,4,1)*AA13+ZX(J,4,2)*AA23+ZX(J,4,3)*AA33+ZX(J,4,4)*AA43
C
      D14=ZX(J,1,1)*AA14+ZX(J,1,2)*AA24+ZX(J,1,3)*AA34+ZX(J,1,4)*AA44
      D24=ZX(J,2,1)*AA14+ZX(J,2,2)*AA24+ZX(J,2,3)*AA34+ZX(J,2,4)*AA44
      D34=ZX(J,3,1)*AA14+ZX(J,3,2)*AA24+ZX(J,3,3)*AA34+ZX(J,3,4)*AA44
      D44=ZX(J,4,1)*AA14+ZX(J,4,2)*AA24+ZX(J,4,3)*AA34+ZX(J,4,4)*AA44
C
      ZX(J,1,1)=D11
      ZX(J,1,2)=D12
      ZX(J,1,3)=D13
      ZX(J,1,4)=D14
      ZX(J,2,1)=D21
      ZX(J,2,2)=D22
      ZX(J,2,3)=D23
      ZX(J,2,4)=D24
      ZX(J,3,1)=D31
      ZX(J,3,2)=D32
      ZX(J,3,3)=D33
      ZX(J,3,4)=D34
      ZX(J,4,1)=D41
      ZX(J,4,2)=D42
      ZX(J,4,3)=D43
      ZX(J,4,4)=D44
C
   15 CONTINUE
   14 CONTINUE
C
C
      IF(ISRC(3).EQ.1.OR.ISRC(4).EQ.1) THEN
        DO 16 K=1,IBLOCK
        Y11(K)=X1(K)*ZX(K,2,1)+X2(K)*ZX(K,3,1)-WVNO2(K)*X3(K)*ZX(K,4,1)
        Y12(K)=X1(K)*ZX(K,2,2)+X2(K)*ZX(K,3,2)-WVNO2(K)*X3(K)*ZX(K,4,2)
        Y31(K)=-X2(K)*ZX(K,1,1)-X3(K)*ZX(K,2,1)+X5(K)*ZX(K,4,1)
        Y32(K)=-X2(K)*ZX(K,1,2)-X3(K)*ZX(K,2,2)+X5(K)*ZX(K,4,2)
   16   CONTINUE
      ENDIF
      IF(ISRC(1).EQ.1.OR.ISRC(2).EQ.1.OR.ISRC(5).EQ.1.OR.ISRC(6)
     $     .EQ.1.OR.ISRC(7).EQ.1.OR.ISRC(8).EQ.1) THEN
        DO 17 K=1,IBLOCK
        Y21(K)=-X1(K)*ZX(K,1,1)+X3(K)*ZX(K,3,1)+X4(K)*ZX(K,4,1)
        Y22(K)=-X1(K)*ZX(K,1,2)+X3(K)*ZX(K,3,2)+X4(K)*ZX(K,4,2)
        Y41(K)=WVNO2(K)*X3(K)*ZX(K,1,1)-X4(K)*ZX(K,2,1)-X5(K)*ZX(K,3,1)
        Y42(K)=WVNO2(K)*X3(K)*ZX(K,1,2)-X4(K)*ZX(K,2,2)-X5(K)*ZX(K,3,2)
   17   CONTINUE
      ENDIF
  300 IF(ISYST.EQ.1) GO TO 301
C     SH-MOTION STARTS HERE +++++>
      DO 18 J=1,IBLOCK
         S32E(J)=DBLE(2.)*XKA2*WVNO(J)/FRHO
         S34E(J)=DBLE(4.)*WVNO(J)*XKA2/(FOURPI*XKB2)
   18 CONTINUE
       IF(JBDRY.EQ.0)THEN
        DO 19 J=1,IBLOCK
            R6(J)= DBLE(RHO(MMAX))*RB(MMAX,J)
            R7(J)= ONE/(DBLE(B(MMAX))*ATNB(MMAX))**2
   19   CONTINUE
      ELSEIF(JBDRY.LT.0)THEN
        DO 20 J=1,IBLOCK
            R6(J) = ONE
            R7(J) = ZERO
   20   CONTINUE
      ELSEIF(JBDRY.GT.0)THEN
         DO 21 J=1,IBLOCK
            R6(J) = ZERO
            R7(J) = ONE
   21   CONTINUE
      ENDIF
C
      IF(ISYST.EQ.3) GO TO 302
C
      DO 22 M=1,MMAX
      DO 22 J=1,IBLOCK
      P=RA(M,J)*DBLE(D(M))
      Q=RB(M,J)*DBLE(D(M))
      A0=0.0D+00
      PR=REAL(P)
      PI=DIMAG(P)
      QR=REAL(Q)
      QI=DIMAG(Q)
      EPP=DCMPLX(DCOS(PI),DSIN(PI))/DBLE(2.)
      EPM=DCONJG(EPP)
      EQP=DCMPLX(DCOS(QI),DSIN(QI))/DBLE(2.)
      EQM=DCONJG(EQP)
      EXEL(J)=EXEL(J)+QR
      IF(M.GE.LMAX) EXLL(J)=EXLL(J)+QR
      FAC=0.0D+00
      IF(PR.LT.15.) FAC=DEXP(-2.*PR)
      COSP=EPP + FAC*EPM
      SINP=EPP - FAC*EPM
      FAC=0.0D+00
      IF(SNGL(QR).LT.15.) FAC=DEXP(-2.0D+00*QR)
      COSQL(M,J)=EQP + FAC*EQM
      SINQ=EQP - FAC*EQM
      YL(M,J)=SINQ/RB(M,J)
      ZL(M,J)=RB(M,J)*SINQ
   22 CONTINUE
C
  302 CONTINUE
      DO 23 I=LMAX,MMAX-1
      M=MMAX+LMAX-1-I
      HH=DBLE(RHO(M))*(DBLE(B(M))*ATNB(M))**2
      DO 24 J=1,IBLOCK
      YLI=YL(M,J)/HH
      ZLI=ZL(M,J)*HH
      D11=R6(J)
      D12=R7(J)
      R6(J)=D11*COSQL(M,J)+D12*ZLI
      R7(J)=D11*YLI+D12*COSQL(M,J)
   24 CONTINUE
   23 CONTINUE
      DO 25 J=1,IBLOCK
      X6(J)=R6(J)
   25 X7(J)=R7(J)
      DO 26 I=1,LMAX-1
      M=LMAX-I
      HH=DBLE(RHO(M))*(DBLE(B(M))*ATNB(M))**2
      DO 27 J=1,IBLOCK
      YLI=YL(M,J)/HH
      ZLI=ZL(M,J)*HH
      D11=R6(J)
      D12=R7(J)
      R6(J)=D11*COSQL(M,J)+D12*ZLI
      R7(J)=D11*YLI+D12*COSQL(M,J)
   27 CONTINUE
   26 CONTINUE
C
  301 CONTINUE
      DO 28 J=1,IBLOCK
      FACT(J)=0.0
      ELJ=EXE(J)-EXL(J)
      IF(ELJ.LT.55.0D+00) FACT(J)=DEXP(-ELJ)
      FACT0 = 0.0D+00
      ELJ=EXLL(J)-EXEL(J)
      IF(SNGL(ELJ).GT.-40.0D+00) FACT0 =DEXP(ELJ)
   28 FACX(J)=FACT0*DBLE(FACXX)
      IF(ISRC(1).EQ.1) THEN
        DO 29 J=1,IBLOCK
        G=S32(J)*Y21(J)+S34(J)*Y41(J)
        G1(J)=-G*FACT(J)/R1(J)
   29   CONTINUE
      ENDIF
      IF(ISRC(2).EQ.1) THEN
        DO 30 J=1,IBLOCK
        G=S32(J)*Y22(J)+S34(J)*Y42(J)
        G2(J)=-G*FACT(J)/R1(J)
   30   CONTINUE
      ENDIF
      IF(ISRC(3).EQ.1) THEN
        DO 31 J=1,IBLOCK
        G=S21(J)*Y11(J)
        G3(J)=-G*FACT(J)/R1(J)
   31   CONTINUE
      ENDIF
      IF(ISRC(4).EQ.1) THEN
        DO 32 J=1,IBLOCK
        G=S21(J)*Y12(J)
        G4(J)=-G*FACT(J)/R1(J)
   32   CONTINUE
      ENDIF
      IF(ISRC(5).EQ.1) THEN
        DO 33 J=1,IBLOCK
        G=S14(J)*Y41(J)
        G5(J)=-G*FACT(J)/R1(J)
   33   CONTINUE
      ENDIF
      IF(ISRC(6).EQ.1) THEN
        DO 34 J=1,IBLOCK
        G=S14(J)*Y42(J)
        G6(J)=-G*FACT(J)/R1(J)
   34   CONTINUE
      ENDIF
      IF(ISRC(7).EQ.1) THEN
        DO 35 J=1,IBLOCK
        G=S32E(J)*Y21(J)+S34E(J)*Y41(J)
        G7(J)=-G*FACT(J)/R1(J)
   35   CONTINUE
      ENDIF
      IF(ISRC(8).EQ.1) THEN
        DO 36 J=1,IBLOCK
        G=S32E(J)*Y22(J)+S34E(J)*Y42(J)
        G8(J)=-G*FACT(J)/R1(J)
   36   CONTINUE
      ENDIF
      IF(ISRC(9).EQ.1) THEN
        DO 37 J=1,IBLOCK
        G = DBLE(2.)*X6(J)/DBLE(RHO(LMAX))
        G9(J)=G*FACX(J)/(R6(J)*XKK)
   37   CONTINUE
      ENDIF
      IF(ISRC(10).EQ.1) THEN
        DO 38 J=1,IBLOCK
        G=-DBLE(2.)*WVNO(J)*X7(J)*(DBLE(B(LMAX))*ATNB(LMAX))**2
        G10(J)=G*FACX(J)/(R6(J)*XKK)
   38   CONTINUE
      ENDIF
C
      N11=(NBLK-1)*IB+1
      IS=NBLOCK
      N22=N11+IBLOCK-1
C
      DO 51 I=1,ND
      R=RANGE(I)
      ICN=ICNT(I)
      CALL WVINT(WVNO,R,NBLK,IS,IBLOCK,ICN,ISRC,N11,N22,I)
   51 CONTINUE
      NBLK=NBLK+1
      GO TO 500
  600 CONTINUE
C
      DO 601 I=1,ND
      IF(VRED(I).LE.0.0) VRED(I)=10.0
      T0X=T0(I)+RANGE(I)/VRED(I)
      FAT=OMEGA*T0X
      XR=COS(FAT)
      XI=SIN(FAT)
      DKK=DK*RANGE(I)
      SMM(I,2)=-SMM(I,2)
      SMM(I,10)=-SMM(I,10)
      DO 602 J=1,10
      SMM(I,J)=SMM(I,J)*DBLE(DKK)
      SMM(I,J)=SMM(I,J)/DBLE(RANGE(I))
  602 CONTINUE
      DO 603 J=1,10
      IF(ISRC(J).NE.1) GO TO 603
      GG(I,J)=SMM(I,J)*DCMPLX(DBLE(XR),DBLE(XI))
      WRITE(12) DBLE(GG(I,J)),DIMAG(GG(I,J))
  603 CONTINUE
  601 CONTINUE
      RETURN
      END
C
      SUBROUTINE WVINT(WVNO,R,NBL,ISK,IBL,ICNT,ISRC,N11,N22,II)
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     R     = DISTANCES IN KM FOR WAVENUMBER INTEGRATION
C     NBL   = INDEX AT WHICH [KR] BECOMES LESS TAN AND EQUAL TO 3.0
C     IBL   = NO OF WAVENUMBERS TO BE INTEGRETAED. LESS THN IB
C     ISRC  = CONTROL FOR REQUIRED COMPONENT
C     N11   = INDEX FOR FIRST WAVENUMBER IN ARRAY
C     N22   = INDEX FOR LAST  WAVENUMBER IN ARRAY
C     II    = INDEX FOR DISTANCES
C
C     PROGRAMMED BY CHANDAN K. SAIKIA
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PARAMETER (IB=100,NR=100)
      COMMON/LGK/IASYMP
      REAL*8 J0,J1,J2,FACT,W1,W2,EX,AK
      REAL J0K0,J0K1,J0K2,J0K3,J1K0,J1K1,J1K2,J1K3,J2K0,J2K1,J2K2,
     $                 J2K3
      COMPLEX*16 G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,ZERO,WVNO
      COMPLEX*16 S1,S2,S3,S4,S5,S6,S7,S8,S9,S10
      COMPLEX*16 SUMC,SUMD,SUME,SMM,AA,BB,CC,ASM
      COMMON/ASYM/J0K0,J0K1,J0K2,J0K3,J1K0,J1K1,J1K2,J1K3,
     $              J2K0,J2K1,J2K2,J2K3
      COMMON/BL77/G1(IB),G2(IB),G3(IB),G4(IB),G5(IB),G6(IB),G7(IB),
     $            G8(IB),G9(IB),G10(IB)
      COMMON/BLK7/S1(IB),S2(IB),S3(IB),S4(IB),S5(IB),S6(IB),S7(IB),
     $            S8(IB),S9(IB),S10(IB)
      COMMON/FILT/WVCM,WVC1,WVC2,WVCN
      COMMON/CNTR/CMAX,C1,C2,CMIN,VRED(100),P0(100)
      COMMON/SAIK/JBDRY,VBMIN,ALPHA,XLENG,XFAC,H
      COMMON/BLK11/SMM(NR,10)
      COMMON/BLK12/FACT(IB)
      COMMON/BLK13/AA(10),BB(10),CC(10)
      DIMENSION ISRC(10),J0(IB),J1(IB),J2(IB),SUME(IB),SUMC(IB),
     $        WVNO(IB)
      LOGICAL IASYMP
      ZERO=(0.0D+00,0.0D+00)
      PI=ACOS(-1.0)
      N1=1
      DO 100 J=1,IB
      SUME(J)=ZERO
  100 SUMC(J)=ZERO
C
C     FIND THE ASYMPTOTIC COEFFICIENTS ++++ NECESSARY ONLY WHEN IASYMP
C     IS .TRUE.                        +++++++++++++>
C
      IF(NBL.EQ.1) THEN
         N1=3
          IF(IASYMP) THEN
           CALL SETUP(R)
           IF(II.EQ.1) THEN
              W1=REAL(WVNO(1))
              W2=REAL(WVNO(2))
              IF(ISRC(1).EQ.1) CALL SOLU(S1(1),S1(2),W1,W2,1)
              IF(ISRC(2).EQ.1) CALL SOLU(S2(1),S2(2),W1,W2,2)
              IF(ISRC(3).EQ.1) CALL SOLU(S3(1),S3(2),W1,W2,3)
              IF(ISRC(4).EQ.1) CALL SOLU(S4(1),S4(2),W1,W2,4)
              IF(ISRC(5).EQ.1) CALL SOLU(S5(1),S5(2),W1,W2,5)
              IF(ISRC(6).EQ.1) CALL SOLU(S6(1),S6(2),W1,W2,6)
              IF(ISRC(7).EQ.1) CALL SOLU(S7(1),S7(2),W1,W2,7)
              IF(ISRC(8).EQ.1) CALL SOLU(S8(1),S8(2),W1,W2,8)
              IF(ISRC(9).EQ.1) CALL SOLU(S9(1),S9(2),W1,W2,9)
              IF(ISRC(10).EQ.1)CALL SOLU(S10(1),S10(2),W1,W2,10)
              DO 101 J=1,10
              IF(ISRC(J).NE.1) AA(J)=ZERO
              IF(ISRC(J).NE.1) BB(J)=ZERO
              IF(ISRC(J).NE.1) CC(J)=ZERO
  101         CONTINUE
           ENDIF
C
C          INITIALIZE THE INTEGRALS +++  NECESSARY FOR IASYMP.EQ.TRUE
C
            SMM(II,1)=AA(1)*DBLE(J0K0) + BB(1)*DBLE(J0K1) +
     $                                       CC(1)*DBLE(J0K2)
            SMM(II,2)=AA(2)*DBLE(J1K1)+BB(2)*DBLE(J1K2)+
     $                                         CC(2)*DBLE(J1K3)
            SMM(II,3)=AA(3)*DBLE(J1K0)+BB(3)*DBLE(J1K1)+
     $                                          CC(3)*DBLE(J1K2) 
            IF(ISRC(9).EQ.1.AND.ISRC(4).EQ.1)THEN
              SUMD=(AA(4)+AA(9))*DBLE(J1K0)+(BB(4)+BB(9))*DBLE(J1K1) +
     $                (CC(4)+CC(9))*DBLE(J1K2)
            ELSE
              SUMD=DCMPLX(0.0,0.0)
            ENDIF
            SUMD= -SUMD/DBLE(R)
            SMM(II,4)=SUMD+AA(4)*DBLE(J0K1)+BB(4)*DBLE(J0K2)+
     $                                          CC(4)*DBLE(J0K3)
            SMM(II,5)=SUMD+AA(9)*DBLE(J0K1)+BB(9)*DBLE(J0K2)+
     $                                        CC(9)*DBLE(J0K3)
            SMM(II,6)=AA(5)*DBLE(J2K0)+BB(5)*DBLE(J2K1)+
     $                                   CC(5)*DBLE(J2K2)
            IF(ISRC(6).EQ.1.AND.ISRC(10).EQ.1)THEN
                SUMD=(AA(6)+AA(10))*DBLE(J2K0)+(BB(6)+BB(10))*
     $                     DBLE(J2K1)+(CC(6)+CC(10))*DBLE(J2K2)
            ELSE
                SUMD = DCMPLX(0.0,0.0)
            ENDIF
            SUMD= -DBLE(2.)*SUMD/DBLE(R)
            SMM(II,7)=SUMD+AA(6)*DBLE(J1K1)+BB(6)*DBLE(J1K2)+
     $                                   CC(6)*DBLE(J1K3)
            SMM(II,8)=SUMD+AA(10)*DBLE(J1K1)+ BB(10)*DBLE(J1K2)+
     $                                    CC(10)*DBLE(J1K3)
            SMM(II,9)=AA(7)*DBLE(J0K0)+BB(7)*DBLE(J0K1)+
     $                                    CC(7)*DBLE(J0K2)
            SMM(II,10)=AA(8)*DBLE(J1K1)+BB(8)*DBLE(J1K2)+
     $                                    CC(8)*DBLE(J1K3)
       ELSE
            DO 102 I=1,10
  102       SMM(II,I)=DCMPLX(0.0,0.0)
       ENDIF
      ENDIF
C
C     GET THE BESSEL'S FUNCTIONS +++++++++++++++++++++++++++++++>
C
      IF(N22.LE.ICNT) THEN
         DO 2 J=N1,IBL
         Z=REAL(WVNO(J))*R
         X = (Z/3.)*(Z/3.)
         AJ0 = 1.-X*(2.2499997-X*(1.2656208-X*(.3163866-X*(
     $           .0444479-X*(.0039444-X*(.0002100))))))
         A1Z = 0.5-X*(.56249985-X*(.21093573-X*(.03954289-X*(
     $           .00443319-X*(.00031761-X*(.00001109))))))
         AJ1 = Z * A1Z
         J2(J)=DBLE(2.*AJ1/Z)-DBLE(AJ0)
         J1(J)=DBLE(AJ1)
         J0(J)=DBLE(AJ0)
    2    CONTINUE
      ENDIF
C
      IF(N11.GT.ICNT) THEN
         DO 3 J=N1,IBL
         Z=REAL(WVNO(J))*R
         X = 3./Z
         FAC = 1./SQRT(Z)
         F0 = .79788456+X*(-.00000077 + X*(-.00552740 + X*(
     $     -.00009512+X*(.00137237+X*(-.00072805+X*(.00014476))))
     $      ))
         T0 = Z - .78539816+X*(-.04166397+X*(-.00003954+X*(
     $     .00262573+X*(-.00054125+X*(-.00029333+X*(.00013558))))
     $     ))
         F1 = .79788456+X*(.00000156+X*(.01659667+X*(.00017105+
     $   X*(-.00249511+X*(.00113653+X*(-.00020033))))))
         T1 = Z-2.35619449+X*(.12499612+X*(.00005650+X*(
     $      -.00637879+X*(.00074348+X*(.00079824+X*(-.00029166)))
     $     )))
         AJ0 = FAC * F0 * COS(T0)
         AJ1 = FAC * F1 * COS(T1)
         J2(J)=DBLE(2.*AJ1/Z)-DBLE(AJ0)
         J1(J)=DBLE(AJ1)
         J0(J)=DBLE(AJ0)
    3    CONTINUE
      ENDIF
      IF(N22.GT.ICNT.AND.N11.LE.ICNT) THEN
         IM=ICNT-N11+1
         DO 5 J=N1,IM
         Z=REAL(WVNO(J))*R
         X = (Z/3.)*(Z/3.)
         AJ0 = 1.-X*(2.2499997-X*(1.2656208-X*(.3163866-X*(
     $           .0444479-X*(.0039444-X*(.0002100))))))
         A1Z = 0.5-X*(.56249985-X*(.21093573-X*(.03954289-X*(
     $           .00443319-X*(.00031761-X*(.00001109))))))
         AJ1 = Z * A1Z
         J2(J)=DBLE(2.*AJ1/Z)-DBLE(AJ0)
         J1(J)=DBLE(AJ1)
         J0(J)=DBLE(AJ0)
    5    CONTINUE
         DO 6 J=IM+1,IBL
         Z=REAL(WVNO(J))*R
         X = 3./Z
         FAC = 1./SQRT(Z)
         F0 = .79788456+X*(-.00000077 + X*(-.00552740 + X*(
     $     -.00009512+X*(.00137237+X*(-.00072805+X*(.00014476))))
     $      ))
         T0 = Z - .78539816+X*(-.04166397+X*(-.00003954+X*(
     $     .00262573+X*(-.00054125+X*(-.00029333+X*(.00013558))))
     $     ))
         F1 = .79788456+X*(.00000156+X*(.01659667+X*(.00017105+
     $     X*(-.00249511+X*(.00113653+X*(-.00020033))))))
         T1 = Z-2.35619449+X*(.12499612+X*(.00005650+X*(
     $      -.00637879+X*(.00074348+X*(.00079824+X*(-.00029166)))
     $        )))
         AJ0 = FAC * F0 * COS(T0)
         AJ1 = FAC * F1 * COS(T1)
         J2(J)=DBLE(2.*AJ1/Z)-DBLE(AJ0)
         J1(J)=DBLE(AJ1)
         J0(J)=DBLE(AJ0)
    6    CONTINUE
      ENDIF
C
C     GET THE PHASE VELOCITY FILTER  +++++++++++++++++++++++++>
C
      IF(CMAX.LT.0.0) GO TO 8
      DO 7 J=N1,IBL
      FACT(J)=0.0D+00
      WVN=SNGL(REAL(WVNO(J)))
      IF(WVN.GE.WVC1.AND.WVN.LE.WVC2) FACT(J)=1.0D+00
      IF(WVN.GE.WVCM.AND.WVN.LT.WVC1)
     $      FACT(J)=DBLE((1.-COS(PI*(WVN-WVCM)/ (WVC1-WVCM)))/2.)
      IF(WVN.GT.WVC2.AND.WVN.LE.WVCN)
     $      FACT(J)=DBLE((1.-COS(PI*(WVN-WVCN)/ (WVC2-WVCN)))/2.)
    7 CONTINUE
    8 CONTINUE
C
C     REMOVAL OF ASYMPTOTIC TREND FROM THE INTEGRAND
C
      IF(IASYMP) THEN
         DO 9 K=1,10
         IF(ISRC(K).NE.1) GO TO 9
         DO 10 J=N1,IBL
         AK=REAL(WVNO(J))
         EX=DEXP(-AK*DBLE(H))
         ASM=EX*(AA(K)+AK*(BB(K)+AK*CC(K)))
         IF(K.EQ.1) G1(J)=S1(J)-ASM
         IF(K.EQ.2) G2(J)=S2(J)-ASM
         IF(K.EQ.3) G3(J)=S3(J)-ASM
         IF(K.EQ.4) G4(J)=S4(J)-ASM
         IF(K.EQ.5) G5(J)=S5(J)-ASM
         IF(K.EQ.6) G6(J)=S6(J)-ASM
         IF(K.EQ.7) G7(J)=S7(J)-ASM
         IF(K.EQ.8) G8(J)=S8(J)-ASM
         IF(K.EQ.9) G9(J)=S9(J)-ASM
         IF(K.EQ.10) G10(J)=S10(J)-ASM
   10    CONTINUE
    9    CONTINUE
      ENDIF
      IF(NBL.EQ.ISK) THEN
      IF(IASYMP) THEN
         DO 666 K=1,10
         IF(ISRC(K).NE.1) GO TO 666
         AK=REAL(WVNO(IBL))
         EX=DEXP(-AK*DBLE(H))
         ASM=EX*(AA(K)+AK*(BB(K)+AK*CC(K)))
         IF(K.EQ.1) G1(IBL)=G1(IBL)-ASM
         IF(K.EQ.2) G2(IBL)=G2(IBL)-ASM
         IF(K.EQ.3) G3(IBL)=G3(IBL)-ASM
         IF(K.EQ.4) G4(IBL)=G4(IBL)-ASM
         IF(K.EQ.5) G5(IBL)=G5(IBL)-ASM
         IF(K.EQ.6) G6(IBL)=G6(IBL)-ASM
         IF(K.EQ.7) G7(IBL)=G7(IBL)-ASM
         IF(K.EQ.8) G8(IBL)=G8(IBL)-ASM
         IF(K.EQ.9) G9(IBL)=G9(IBL)-ASM
         IF(K.EQ.10) G10(IBL)=G10(IBL)-ASM
  666    CONTINUE
      ENDIF
      ENDIF
      IF(.NOT.IASYMP) THEN
         DO 776 K=1,10
         IF(ISRC(K).NE.1) GO TO 776
         DO 777 J=N1,IBL
         IF(K.EQ.1) G1(J)=S1(J)
         IF(K.EQ.2) G2(J)=S2(J)
         IF(K.EQ.3) G3(J)=S3(J)
         IF(K.EQ.4) G4(J)=S4(J)
         IF(K.EQ.5) G5(J)=S5(J)
         IF(K.EQ.6) G6(J)=S6(J)
         IF(K.EQ.7) G7(J)=S7(J)
         IF(K.EQ.8) G8(J)=S8(J)
         IF(K.EQ.9) G9(J)=S9(J)
         IF(K.EQ.10) G10(J)=S10(J)
  777    CONTINUE
  776    CONTINUE
      ENDIF
C
      IF(ISRC(1).EQ.1)THEN
         DO 13 J=IBL,N1,-1
         SUMD=FACT(J)*G1(J)*J0(J)
         SMM(II,1) = SMM(II,1) + SUMD
   13    CONTINUE
      ENDIF
      IF(ISRC(2).EQ.1)THEN
         DO 14 J=N1,IBL
         AK=REAL(WVNO(J))
         SUMD=FACT(J)*G2(J)*AK*J1(J)
         SMM(II,2) = SMM(II,2) + SUMD
   14    CONTINUE
      ENDIF
      IF(ISRC(3).EQ.1)THEN
         DO 15 J=N1,IBL
         SUMD=FACT(J)*G3(J)*J1(J)
         SMM(II,3) = SMM(II,3) + SUMD
   15    CONTINUE
      ENDIF
C
C     INCLUDE NEAR FIELD TERM IF BOTH SH AND P-SV ARE COMPUTED
C
      IF(ISRC(4).EQ.1.AND.ISRC(9).EQ.1)THEN
        DO 16 J=N1,IBL
        SUMC(J)=(G4(J)+G9(J))*J1(J)
   16   CONTINUE
      ENDIF
      IF(ISRC(4).EQ.1)THEN
        DO 17 J=N1,IBL
        AK=REAL(WVNO(J))
        SUMD=G4(J)*AK*J0(J)
        SMM(II,4) = SMM(II,4) + (SUMD - SUMC(J)/DBLE(R))*FACT(J)
   17   CONTINUE
      ENDIF
      IF(ISRC(9).EQ.1)THEN
        DO 18 J=N1,IBL
        AK=REAL(WVNO(J))
        SUMD=G9(J)*AK*J0(J)
        SMM(II,5) = SMM(II,5) + (SUMD - SUMC(J)/DBLE(R))*FACT(J)
   18   CONTINUE
      ENDIF
      IF(ISRC(5).EQ.1)THEN
        DO 19 J=N1,IBL
        SUMD=FACT(J)*G5(J)*J2(J)
        SMM(II,6) = SMM(II,6) + SUMD
   19   CONTINUE
      ENDIF
C
C     NEAR FIELD TERM IF BOTH SH AND P-SV TERMS ARE COMPUTED
C
      IF(ISRC(6).EQ.1.AND.ISRC(10).EQ.1)THEN
        DO 20 J=N1,IBL
        SUME(J)=(G6(J)+G10(J))*J2(J)
   20   CONTINUE
      ENDIF
      IF(ISRC(6).EQ.1)THEN
        DO 21 J=N1,IBL
        AK=REAL(WVNO(J))
        SUMD=G6(J)*AK*J1(J)
        SMM(II,7) = SMM(II,7)+(SUMD-2.0D+00*SUME(J)/DBLE(R))*FACT(J)
   21   CONTINUE
      ENDIF
      IF(ISRC(10).EQ.1)THEN
        DO 22 J=IBL,N1,-1
        AK=REAL(WVNO(J))
        SUMD=G10(J)*AK*J1(J)
        SMM(II,8) = SMM(II,8)+(SUMD-2.0D+00*SUME(J)/DBLE(R))*FACT(J)
   22   CONTINUE
      ENDIF
      IF(ISRC(7).EQ.1)THEN
        DO 23 J=N1,IBL
        SUMD=FACT(J)*G7(J)*J0(J)
        SMM(II,9) = SMM(II,9) + SUMD
   23   CONTINUE
      ENDIF
      IF(ISRC(8).EQ.1)THEN
        DO 24 J=N1,IBL
        AK=REAL(WVNO(J))
        SUMD=FACT(J)*G8(J)*AK*J1(J)
        SMM(II,10) = SMM(II,10) + SUMD
   24   CONTINUE
      ENDIF
    1 CONTINUE
      RETURN
      END
      SUBROUTINE SETUP(R)
C----------------------------------------------------------
C       JNKM = R INTEGRAL EXP(-KH) KSUP M J SUB N (KR) DK
C       THE R IN FRONT TAKES INTO ACCOUNT THE 1/R IN THE
C       DO 300 OF SUBROUTINE WVINT
C----------------------------------------------------------
      COMMON/SAIK/JBDRY,VBMIN,ALPHA,XLENG,XFAC,Z
      COMMON/ASYM/J0K0,J0K1,J0K2,J0K3,J1K0,J1K1,J1K2,J1K3,
     $       J2K0,J2K1,J2K2,J2K3
      REAL J0K0,J0K1,J0K2,J0K3,J1K0,J1K1,J1K2,J1K3,J2K0,J2K1,
     $       J2K2,J2K3
      DIST=SQRT(R*R + Z*Z)
      DIST3=DIST**3
      DIST5=DIST**5
      DIST7=DIST**7
      RZ=R*Z
      Z2=Z*Z
      R2=R*R
      R3=R*R2
      Z3=Z*Z2
      RZ2=R*Z2
      RZ3=R*Z3
      ZOR = Z/DIST
      ZOR2= ZOR*ZOR
      ZOR3= ZOR*ZOR2
      J0K0=R/DIST
      J0K1=RZ/DIST3
      J0K2=(2.*RZ2 - R3)/DIST5
      J0K3=(6.*RZ3 - 9.*Z*R3)/DIST7
      J1K0=1. -Z/DIST
      J1K1=R2/DIST3
      J1K2=3.*Z*R2/DIST5
      J1K3=3.*R2*(4.*Z2 - R2)/DIST7
      J2K0 = ( 1. - 2.*ZOR + ZOR2)*(DIST/R)
      J2K1 = (2. - 3.*ZOR + ZOR3)/R
      J2K2=3.*R3/DIST5
      J2K3=15.*Z*R3/DIST7
      RETURN
      END

      SUBROUTINE SOLU(Y1,Y2,X1,X2,J)
      COMPLEX*16 Y1,Y2,A,B,C
      REAL*8 X1,X2,DET,U1,U2 
      COMMON/SAIK/JBDRY,VBMIN,ALPHA,XLENG,XFAC,H
      COMMON/BLK13/A(10),B(10),C(10)
      C(J)=DCMPLX(0.0,0.0)
      GO TO (300,200,300,200,300,200,300,200,200,200),J
  200 CONTINUE
C     [ A + B K  ]EXP(-KH)
      U1=X1*DBLE(H)
      U2=X2*DBLE(H)
      DET=X2-X1
      A(J)=X2*Y1*DEXP(U1)-X1*Y2*DEXP(U2)
      A(J)=A(J)/DET
      B(J)=Y2*DEXP(U2)-Y1*DEXP(U1)
      B(J)=B(J)/DET
      RETURN
  300 CONTINUE
C     [ A + B K  ]  K EXP(-KH)
      U1=X1*DBLE(H)
      U2=X2*DBLE(H)
      DET=X2-X1
      A(J) = DCMPLX(0.0,0.0)
      B(J)=X2*Y1*DEXP(U1)/X1-X1*Y2*DEXP(U2)/X2
      B(J)=B(J)/DET
      C(J)=Y2*DEXP(U2)/X2-Y1*EXP(U1)/X1
      C(J)=C(J)/DET
      RETURN
      END
