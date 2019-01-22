CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C                                                                               
      SUBROUTINE BNDPAS(F1,F2,DELT,D,G)                                         
C                                                                       00387000
C     SUBROUTINE BY DAVE GANLEY ON MARCH 5, 1977.                       00387100
C                                                                       00387200
changes:
c        mar 96 by jh: fix to filter only one way as an option
c        sep 98 by jh: ------------- verison 7.0 check ---------
c                      no change
c
C RECURSIVE BUTTERWORTH BAND PASS FILTER (KANASEWICH, TIME SERIES       00387300
C ANALYSIS IN GEOPHYSICS, UNIVERSITY OF ALBERTA PRESS, 1975; SHANKS,    00387400
C JOHN L, RECURSION FILTERS FOR DIGITAL PROCESSING, GEOPHYSICS, V32,    00387500
C FILTER.  THE FILTER WILL HAVE 8 POLES IN THE S PLANE AND IS           00387600
C APPLIED IN FORWARD AND REVERSE DIRECTIONS SO AS TO HAVE ZERO          00387700
C PHASE SHIFT.  THE GAIN AT THE TWO FREQUENCIES SPECIFIED AS            00387800
C CUTOFF FREQUENCIES WILL BE -6DB AND THE ROLLOFF WILL BE ABOUT         00387900
C THE FILTER TO PREVENT ALIASING PROBLEMS.                              00388000
C                                                                       00388100
      COMPLEX P(4),S(8),Z1,Z2                                           00388200
      DIMENSION D(8),X(*),XC(3),XD(3),XE(3)                             00388300
      DATA ISW/0/,TWOPI/6.2831853/                                      00388400
C                                                                       00388500
C THIS SECTION CALCULATES THE FILTER AND MUST BE CALLED BEFORE          00388600
C FILTER IS CALLED                                                      00388700
C                                                                       00388800
C    F2 = HIGH FREQUENCY CUTOFF (6 DB DOWN)                             00388900
C    DELT = SAMPLE INTERVAL IN MILLISECONDS                             00389000
C    D = WILL CONTAIN 8 Z DOMAIN COEFICIENTS OF RECURSIVE FILTER        00389100
C    G = WILL CONTAIN THE GAIN OF THE FILTER,                           00389200
C                                                                       00389300
      DT=DELT/1000.0                                                    00389400
      TDT=2.0/DT                                                        00389500
      FDT=4.0/DT                                                        00389600
      ISW=1                                                             00389700
      P(1)=CMPLX(-.3826834,.9238795)                                    00389800
      P(2)=CMPLX(-.3826834,-.9238795)                                   00389900
      P(3)=CMPLX(-.9238795,.3826834)                                    00390000
      P(4)=CMPLX(-.9238795,-.3826834)                                   00390100
      W1=TWOPI*F1                                                       00390200
      W2=TWOPI*F2                                                       00390300
      W1=TDT*TAN(W1/TDT)                                                00390400
      W2=TDT*TAN(W2/TDT)                                                00390500
      HWID=(W2-W1)/2.0                                                  00390600
      WW=W1*W2                                                          00390700
      DO 19 I=1,4                                                       00390800
      Z1=P(I)*HWID                                                      00390900
      Z2=Z1*Z1-WW                                                       00391000
      Z2=CSQRT(Z2)                                                      00391100
      S(I)=Z1+Z2                                                        00391200
   19 S(I+4)=Z1-Z2                                                      00391300
      G=.5/HWID                                                         00391400
      G=G*G                                                             00391500
      G=G*G                                                             00391600
      DO 29 I=1,7,2                                                     00391700
      B=-2.0*REAL(S(I))                                                 00391800
      Z1=S(I)*S(I+1)                                                    00391900
      C=REAL(Z1)                                                        00392000
      A=TDT+B+C/TDT                                                     00392100
      G=G*A                                                             00392200
      D(I)=(C*DT-FDT)/A                                                 00392300
   29 D(I+1)=(A-2.0*B)/A                                                00392400
      G=G*G                                                             00392500
    5 FORMAT ('-FILTER GAIN IS ',E12.6)                                 00392600
      RETURN                                                            00392700
      ENTRY FILTER(X,N,D,G,IG)                                          00392800
C                                                                       00392900
C     X = DATA VECTOR OF LENGTH N CONTAINING DATA TO BE FILTERED        00393000
C     D = FILTER COEFFICIENTS CALCULATED BY BNDPAS                      00393100
C     G = FILTER GAIN                                                   00393200
C     IG = 1  one pass
c     ig = 2  two passes
C                                                                       00393500
      IF (ISW.EQ.1) GO TO 31                                            00393600
      WRITE (6,6)                                                       00393700
    6 FORMAT ('1BNDPAS MUST BE CALLED BEFORE FILTER')                   00393800
      return                                                            00393900
C                                                                       00394000
C     APPLY FILTER IN FORWARD DIRECTION                                 00394100
C                                                                       00394200
   31 XM2=X(1)                                                          00394300
      XM1=X(2)                                                          00394400
      XM=X(3)                                                           00394500
      XC(1)=XM2                                                         00394600
      XC(2)=XM1-D(1)*XC(1)                                              00394700
      XC(3)=XM-XM2-D(1)*XC(2)-D(2)*XC(1)                                00394800
      XD(1)=XC(1)                                                       00394900
      XD(2)=XC(2)-D(3)*XD(1)                                            00395000
      XD(3)=XC(3)-XC(1)-D(3)*XD(2)-D(4)*XD(1)                           00395100
      XE(1)=XD(1)                                                       00395200
      XE(2)=XD(2)-D(5)*XE(1)                                            00395300
      XE(3)=XD(3)-XD(1)-D(5)*XE(2)-D(6)*XE(1)                           00395400
      X(1)=XE(1)                                                        00395500
      X(2)=XE(2)-D(7)*X(1)                                              00395600
      X(3)=XE(3)-XE(1)-D(7)*X(2)-D(8)*X(1)                              00395700
      DO 39 I=4,N                                                       00395800
      XM2=XM1                                                           00395900
      XM1=XM                                                            00396000
      XM=X(I)                                                           00396100
      K=I-((I-1)/3)*3                                                   00396200
      GO TO (34,35,36),K                                                00396300
   34 M=1                                                               00396400
      M1=3                                                              00396500
      M2=2                                                              00396600
      GO TO 37                                                          00396700
   35 M=2                                                               00396800
      M1=1                                                              00396900
      M2=3                                                              00397000
      GO TO 37                                                          00397100
   36 M=3                                                               00397200
      M1=2                                                              00397300
      M2=1                                                              00397400
   37 XC(M)=XM-XM2-D(1)*XC(M1)-D(2)*XC(M2)                              00397500
      XD(M)=XC(M)-XC(M2)-D(3)*XD(M1)-D(4)*XD(M2)                        00397600
      XE(M)=XD(M)-XD(M2)-D(5)*XE(M1)-D(6)*XE(M2)                        00397700
   39 X(I)=XE(M)-XE(M2)-D(7)*X(I-1)-D(8)*X(I-2)                         00397800
C                                                                       00397900
C
      if(ig.eq.1) goto 3333                                             00398000
      XM2=X(N)                                                          00398100
      XM1=X(N-1)                                                        00398200
      XM=X(N-2)                                                         00398300
      XC(1)=XM2                                                         00398400
      XC(2)=XM1-D(1)*XC(1)                                              00398500
      XC(3)=XM-XM2-D(1)*XC(2)-D(2)*XC(1)                                00398600
      XD(1)=XC(1)                                                       00398700
      XD(2)=XC(2)-D(3)*XD(1)                                            00398800
      XD(3)=XC(3)-XC(1)-D(3)*XD(2)-D(4)*XD(1)                           00398900
      XE(1)=XD(1)                                                       00399000
      XE(2)=XD(2)-D(5)*XE(1)                                            00399100
      XE(3)=XD(3)-XD(1)-D(5)*XE(2)-D(6)*XE(1)                           00399200
      X(N)=XE(1)                                                        00399300
      X(N-1)=XE(2)-D(7)*X(1)                                            00399400
      X(N-2)=XE(3)-XE(1)-D(7)*X(2)-D(8)*X(1)                            00399500
      DO 49 I=4,N                                                       00399600
      XM2=XM1                                                           00399700
      XM1=XM                                                            00399800
      J=N-I+1                                                           00399900
      XM=X(J)                                                           00400000
      K=I-((I-1)/3)*3                                                   00400100
      GO TO (44,45,46),K                                                00400200
   44 M=1                                                               00400300
      M1=3                                                              00400400
      M2=2                                                              00400500
      GO TO 47                                                          00400600
   45 M=2                                                               00400700
      M1=1                                                              00400800
      M2=3                                                              00400900
      GO TO 47                                                          00401000
   46 M=3                                                               00401100
      M1=2                                                              00401200
      M2=1                                                              00401300
   47 XC(M)=XM-XM2-D(1)*XC(M1)-D(2)*XC(M2)                              00401400
      XD(M)=XC(M)-XC(M2)-D(3)*XD(M1)-D(4)*XD(M2)                        00401500
      XE(M)=XD(M)-XD(M2)-D(5)*XE(M1)-D(6)*XE(M2)                        00401600
   49 X(J)=XE(M)-XE(M2)-D(7)*X(J+1)-D(8)*X(J+2)                         00401700
 3333 continue
      if (ig.eq.1) then
        gg=sqrt(g)   ! if only pass once, modify gain
      else
        gg=g
      endif
c
      DO 59 I=1,N                                                       00401900
   59 X(I)=X(I)/gg                                                      00402000
      RETURN                                                                    
      END                                                               00402100
