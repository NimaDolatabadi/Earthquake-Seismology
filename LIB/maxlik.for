
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE MAXLIK(FMX,FMY,NP,FLA,FKA,SLA,SKA)
C
C     REF.: U. ERICSON: MAXIMUM LIKELIHOOD LINEAR FITTING WHEN BOTH
C                       VARIABLES HAVE NORMAL AND CORRELATED ERRORS,
C                       FOA 4 REPORT C 4474-A1, SEPTEMBER 1971.
C     H. BUNGUM
C
c     mar, 08 99 : simplified for use in seisan
c

C
C     INPUT:
C
C     FMX(I),I=1,NP  MAGNITUDES IN X DIRECTIONS
C     FMY(I),I=1,NP  MAGNITUDES IN NG Y-AXIS
C     NP             NO OF POINTS
C
C     RO (=0)        CORRELATION BETWEEN Y AND X
C     GE (=1.0)      RATIO BETWEEN VARIANCES IN Y AND X
C
C     OUTPUT, not all put output:
C
C     XBAR,YBAR      AVERAGES IN X AND Y DIRECTIONS
C     AX,BX          REGRESSION COEFFICIENTS IN Y=AX*X+BX
C     AY,BY          REGRESSION COEFFICIENTS IN X=AY*Y+BY
C     SBX,SBY        STANDARD DEVIATIONS IN BX AND BY
C     FLA,FKA        MAXIMUM-LIKELIHOOD COEFFICIENTS
C     SLA,SKA        STANDARD DEVIATIONS IN FLA AND FKA
C     SS             SUM OF SQUARES AROUND MAX-LIK REGRESSION
C     SX,SY          SIGMA VALUES IN X AND Y FOR MAX-LIK SOLUTION
C
C
C
      real*4 FMX(*),FMY(*)
C
      GE=1.0
      RO=0.0
C
      JB=0
      XBAR=0.0
      YBAR=0.0
      DO 500  I=1,NP
      JB=JB+1
      XBAR=XBAR+FMX(I)
      YBAR=YBAR+FMY(I)
  500 CONTINUE
      XBAR=XBAR/JB
      YBAR=YBAR/JB
C
      SU2=0.0
      SV2=0.0
      SUV=0.0
      DO 501  I=1,NP
      SU2=SU2+(FMX(I)-XBAR)**2
      SV2=SV2+(FMY(I)-YBAR)**2
      SUV=SUV+(FMX(I)-XBAR)*(FMY(I)-YBAR)
  501 CONTINUE
      BX=SUV/SU2
      AX=YBAR-BX*XBAR
      BY=SUV/SV2
      AY=XBAR-BY*YBAR
      ARG1=(1./(BX*BY)-1.)/(JB-2.)
      IF(ARG1.GE.0.0)  then
         SBX=BX*SQRT(ARG1)
         SBY=BY*SQRT(ARG1)
      else
         sbx=0.
         sby=0. 
      endif
      S0=-GE*GE+RO*GE/BY
      S1=GE*GE/BX-1./BY
      S2=1.-RO*GE/BX
      ARG2=S1*S1-4.*S0*S2
      IF(ARG2.GE.0.0)  then
         FKA=(-S1+SQRT(ARG2))/(2.*S2)
      else
         fka=0.
      endif
      FLA=YBAR-FKA*XBAR
C
      SS=0.0
      DO 502  I=1,NP
      SS=SS+(FMY(I)-FLA-FKA*FMX(I))**2
  502 CONTINUE
      ARG3=SS/((GE*GE+FKA*FKA-2.*FKA*GE)*(JB-2.))
      IF(ARG3.GE.0.0)  then
         SX=SQRT(ARG3)
      else
         sx=0.
      endif  
      SY=GE*SX
      ARG4=(GE*GE+BX/BY)*(1./(BX*BY)-1.)/((JB-2.)*(GE*GE+FKA*FKA))
      IF(ARG4.GE.0.0)  then
         SKA=SQRT(ARG4)
      else
         ska=0.
      endif
      ARG5=SV2/(JB*(JB-2.))+XBAR*XBAR*SKA*SKA
      IF(ARG5.GE.0.0)  then
         SLA=SQRT(ARG5)
      else
         sla=0.
      endif
C
c     WRITE(2,503)  JB
c 503 FORMAT(//' LEAST-SQUARES AND MAXIMUM LIKELIHOOD LINEAR FITTING OF 
c    *MAGNITUDE DISTRIBUTIONS'//' NUMBER OF EVENTS',I7)
c     write(2,'(a,i7)') ' NUMBER OF EVENTS',jb
c     WRITE(2,504) AX,BX,SBX,AY,BY,SBY,FLA,FKA
c 504 FORMAT(' Y AS A FUNCTION OF X',2F10.3/,5X,'STD',23X,F10.3/
c    *        ' X AS A FUNCTION OF Y',2F10.3/,5X,'STD',23X,F10.3/
c    *        ' MAXIMUM-LIKELIHOOD  ',2F10.3)
C
c     IF(RO.EQ.0.0)  WRITE(2,16) SLA,SKA
c  16 FORMAT(5X,'STD',13X,2F10.3)
c     WRITE(2,505) XBAR,YBAR,SS,SX,SY
c 505 FORMAT(' XBAR, YBAR',2F8.3,5X,
c    *' SS',F8.3,5X,'SX, SY',2F8.3)
C
      RETURN
      END
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

