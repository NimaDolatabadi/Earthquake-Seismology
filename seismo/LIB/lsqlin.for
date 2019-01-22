C***********************************************************************
C                                                                    
c  nov 9 98  jh -----------verison7.0 check, no change ------------------------
c
      SUBROUTINE LSQLIN(N,X,Y,A,B,CORR,RMS)                               
C                                                                       
C  REGRESSION LINE WITH PARAMETERS A AND B
C  Y=A+BX                              
C                                                                       
      DIMENSION X(*),Y(*)
      INTRINSIC SQRT                                         
      REAL NEV
      SX=0.0                                                            
      SY=0.0                                                            
      SXY=0.0                                                           
      SXS=0.0                                                       
      DO 1 I=1,N                                                       
      SX=SX+X(I)                                                        
      SY=SY+Y(I)                                                        
      SXY=SXY+X(I)*Y(I)                                                 
      SXS=SXS+X(I)*X(I)                                                 
 1    CONTINUE                                                          
      D1=REAL(N*SXS-SX*SX)                                                   
      IF (ABS(D1).LT.10E-6) GOTO 3     
      A=REAL(SY*SXS-SX*SXY)/(D1)                                            
      B=REAL(SXY*N-SX*SY)/(D1)
      RMS=0.0               
      TEL=0.0
      WVX=0.0
      WVY=0.0
      RX=SX/REAL(N)
      RY=SY/REAL(N)                              
      DO 2 I=3,N
      TEL=TEL+(X(I)-RX)*(Y(I)-RY)
      WVX=WVX+(X(I)-RX)**2
      WVY=WVY+(Y(I)-RY)**2
      RMS=RMS+(Y(I)-A-B*X(I))**2
 2    CONTINUE
      ARG=WVX*WVY
      IF (ARG.LT.10E-6) GOTO 4
      NEV=SQRT(ARG)
      CORR=TEL/NEV
      RMS=SQRT(RMS/N)
      RETURN                       
 3    A=0.0                                      
      B=0.0  
 4    RMS=0.0                                                           
      CORR=0.0
      RETURN                                                            
      END                                                               
C
C############################################################################
C
      SUBROUTINE SDV(N,X,AV,SD)
C
C   CALCULATES STANDARD DEVIATION SD AND AVERAGE OF
C   N DATA SAMPLES X. IF N LT 10, WEIGHT IS (N-1)
C
      DIMENSION X(*)
C
      IF(N.EQ.0) RETURN
      SD=0.0
      AV=0.0
      DO 1 I=1,N
      AV=AV+X(I)
 1    CONTINUE
      AV=AV/N
      IF(N.EQ.1) RETURN
      DO 2 I=1,N
      SD=SD+(AV-X(I))*(AV-X(I))
 2    CONTINUE
      IF(N.GE.10) SD=SQRT(SD/N)
      IF(N.LT.10) SD=SQRT(SD/(N-1))
      RETURN
      END
