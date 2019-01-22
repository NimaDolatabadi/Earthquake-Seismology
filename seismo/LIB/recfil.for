C---------------------------------------------------------------------- 
C                                                                       
C RECFIL -- SUBROUTINE:   IIR FILTER DESIGN AND IMPLEMENTATION          
C                                                                       
C  AUTHOR:  DAVE HARRIS                                                 
C                                                                       
C  LAST MODIFIED:  FEBRUARY 13, 1981                                    
c                  dec 5 , 1995 by jh: enable continous filtering by not
c                                      not clearing buffer between calls
c                  sep 98 by jh      : --------- verison 7.0 check ----------
c                                      no change
C 
c    oct 7  98  bmt        :linux changed (*) // (1)                            c                                       
C  ARGUMENTS:                                                           
C  ----------                                                           
C                                                                       
C    DATA           REAL ARRAY CONTAINING SEQUENCE TO BE FILTERED       
C                                                                       
C    NDATA          NUMBER OF SAMPLES IN DATA AND FDATA                 
C                                                                       
C    FDATA          REAL ARRAY CONTAINING FILTERED SEQUENCE             
C                   MAY BE THE SAME ARRAY AS DATA IN CALLING SEQUENCE   
C                                                                       
C    PROTO          CHARACTER*8 VARIABLE, CONTAINS TYPE OF ANALOG       
C                     PROTOTYPE FILTER                                  
C                     '(BU)TTER  ' -- BUTTERWORTH FILTER                
C                     '(BE)SSEL  ' -- BESSEL FILTER                     
C                     'C1        ' -- CHEBYSHEV TYPE I                  
C                     'C2        ' -- CHEBYSHEV TYPE II                 
C                                                                       
C    TRBNDW         TRANSITION BANDWIDTH AS FRACTION OF LOWPASS         
C                   PROTOTYPE FILTER CUTOFF FREQUENCY.  USED            
C                   ONLY BY CHEBYSHEV FILTERS.                          
C                                                                       
C    ATTEN          ATTENUATION FACTOR.  EQUALS RECIPROCAL AMPLITUDE    
C                   REACHED AT STOPBAND EDGE.  USED ONLY BY             
C                   CHEBYSHEV FILTERS.                                  
C                                                                       
C    IORD           ORDER (#POLES) OF ANALOG PROTOTYPE                  
C                   NOT TO EXCEED 10 IN THIS CONFIGURATION.  4 - 5      
C                   SHOULD BE AMPLE.                                    
C                                                                       
C    TYPE           CHARACTER*8 VARIABLE CONTAINING FILTER TYPE         
C                     'LP' -- LOW PASS                                  
C                     'HP' -- HIGH PASS                                 
C                     'BP' -- BAND PASS                                 
C                     'BR' -- BAND REJECT                               
C                                                                       
C    FLO            LOW FREQUENCY CUTOFF OF FILTER (HERTZ)              
C                   IGNORED IF TYPE = 'LP'                              
C                                                                       
C    FHI            HIGH FREQUENCY CUTOFF OF FILTER (HERTZ)             
C                   IGNORED IF TYPE = 'HP'                              
C                                                                       
C    TS             SAMPLING INTERVAL (SECONDS)                         
C                                                                       
C    PASSES           INTEGER VARIABLE CONTAINING THE NUMBER OF PASSES  
C                   1 -- FORWARD FILTERING ONLY                         
C                   2 -- FORWARD AND REVERSE (I.E. ZERO PHASE) FILTERING
c                   If negative 1 or 2, do not clear buffers use with cont. data
C                                                                       
C                                                                       
C  SUBPROGRAMS REFERENCED:  BILIN, BUTTER, WARP, CUTOFF, LPTHP, LPTBP,  
C    BESSEL,CTPOLY,MPOLYS,CHEBI,CHEBII                                  
C                                                                       
      SUBROUTINE RECFIL(DATA, NDATA, FDATA, PROTO, TRBNDW, ATTEN, IORD, 
     +TYPE, FLO, FHI, TS, PASSES)                                       
C                                                                       
        DIMENSION DATA(*), FDATA(*)                                     
        DOUBLE PRECISION A(21), B(21), P(21), Q(21)                     
        DOUBLE PRECISION OUT, DBUF(21), FBUF(21)                        
        CHARACTER*8 TYPE                                                
        CHARACTER*8 PROTO                                               
        INTEGER POINT, ORDER, NDATA, PASSES                             
        REAL*4 WARP                                                     
        logical clear_buf     ! if clear buffer, for cont. data

        if(passes.lt.0) then
          clear_buf=.false.
          passes=-passes
        else
          clear_buf=.true.
        endif
C                                                                       
C  SCALE SAMPLING RATE AND CUTOFF FREQUENCIES                           
C                                                                       
        SCALE=2./TS                                                     
        T=2.                                                            
        FL=FLO/SCALE                                                    
        FH=FHI/SCALE                                                    
C                                                                       
C  GENERATE PROTOTYPE ANALOG FILTER                                     
C                                                                       
        IF (  PROTO(1:2) .EQ. 'BU' ) THEN                               
          CALL BUTTER(P,Q,M,N,IORD)                                     
        ELSE IF ( PROTO(1:2) .EQ. 'BE' ) THEN                           
          CALL BESSEL(P,Q,M,N,IORD)                                     
        ELSE IF ( PROTO(1:2) .EQ. 'C1' ) THEN                           
          OMEGAR=1.+TRBNDW                                              
          TEMP=OMEGAR+SQRT(OMEGAR*OMEGAR-1.)                            
          ALPHA=1.                                                      
          DO    4 I=1, IORD                                             
            ALPHA=ALPHA*TEMP                                            
    4     CONTINUE                                                      
          G=(ALPHA*ALPHA+1.)/(2.*ALPHA)                                 
          EPS=SQRT(ATTEN*ATTEN-1.)/G                                    
          CALL CHEBI(P,Q,M,N,IORD,EPS)                                  
        ELSE IF ( PROTO(1:2) .EQ. 'C2' ) THEN                           
          OMEGAR=1.+TRBNDW                                              
          CALL CHEBII(P,Q,M,N,IORD,ATTEN,OMEGAR)                        
        ELSE                                                            
          WRITE(6,*) '**** RECFIL - PROTO FILTER UNKNOWN ****'          
          RETURN                                                        
        END IF                                                          
C                                                                       
C  TRANSFORM ANALOG PROTOTYPE TO DESIRED ANALOG TYPE                    
C                                                                       
        IF (  TYPE(1:2) .EQ. 'LP' ) THEN                                
          F=WARP(FH,T)                                                  
          CALL CUTOFF(P,Q,M,N,F)                                        
        ELSE IF ( TYPE(1:2) .EQ. 'HP' ) THEN                            
          F=WARP(FL,T)                                                  
          CALL LPTHP(P,Q,M,N)                                           
          CALL CUTOFF(P,Q,M,N,F)                                        
        ELSE IF ( TYPE(1:2) .EQ. 'BP' ) THEN                            
          F1=WARP(FL,T)                                                 
          F2=WARP(FH,T)                                                 
          CALL LPTBP(P,Q,M,N,F1,F2)                                     
        ELSE IF ( TYPE(1:2) .EQ. 'BR' ) THEN                            
          F1=WARP(FL,T)                                                 
          F2=WARP(FH,T)                                                 
          CALL LPTHP(P,Q,M,N)                                           
          CALL LPTBP(P,Q,M,N,F1,F2)                                     
        ELSE                                                            
          WRITE(6,*) '**** RECFIL - FILTER TYPE UNDEFINED ****'         
          RETURN                                                        
        END IF                                                          
C                                                                       
C  BILINEAR TRANSFORMATION TO CONVERT ANALOG FILTER TO DIGITAL FILTER   
C                                                                       
        CALL BILIN(P,Q,M,N,A,B,T)                                       
C                                                                       
C  FILTER DATA - FORWARD DIRECTION                                      
C                                                                       
        ORDER=MAX0(M,N)+1                                               
C                                                                       
C    INITIALIZE BUFFER ARRAYS if not a continous filtering
C            
        if(clear_buf) then                                                           
           DO    5 I=1, 21                                                 
             DBUF(I)=0.                                                    
             FBUF(I)=0.                                                    
    5      CONTINUE          
        else 
c          write(*,*) ' keeping buffer ',fbuf(1),dbuf(1)
        endif                                              
C                                                                       
C    INITIALIZE POINTER                                                 
C                                                                       
        POINT=1                                                         
C                                                                       
C    LOOP                                                               
C                                                                       
    6   CONTINUE                                                        
        IF ( POINT .GT. NDATA ) GO TO    7                              
C                                                                       
C      FETCH NEW INPUT DATUM                                            
C                                                                       
          DBUF(1)=DATA(POINT)                                           
C                                                                       
C      CALCULATE NEW OUTPUT POINT                                       
C                                                                       
          OUT=A(1)*DBUF(1)                                              
          DO    8 I=2, ORDER                                            
            OUT=OUT+A(I)*DBUF(I)-B(I)*FBUF(I)                           
    8     CONTINUE                                                      
          FBUF(1)=OUT/B(1)                                              
          FDATA(POINT)=FBUF(1)                                          
C                                                                       
C      SHIFT BUFFERS                                                    
C                                                                       
          DO    9 I=2, ORDER                                            
            K=ORDER+2-I                                                 
            FBUF(K)=FBUF(K-1)                                           
            DBUF(K)=DBUF(K-1)                                           
    9     CONTINUE                                                      
C                                                                       
C      UPDATE POINTER                                                   
C                                                                       
          POINT=POINT+1                                                 
        GO TO    6                                                      
    7   CONTINUE                                                        
C                                                                       
C  FILTER DATA - REVERSE DIRECTION                                      
C                                                                       
        IF (  PASSES .GT. 1 ) THEN                                      
C                                                                       
C      INITIALIZE BUFFER ARRAYS                                         
C                                                                       
          DO   10 I=1, 21                                               
            DBUF(I)=0.                                                  
            FBUF(I)=0.                                                  
   10     CONTINUE                                                      
C                                                                       
C      INITIALIZE POINTER                                               
C                                                                       
          POINT=NDATA                                                   
C                                                                       
C      LOOP                                                             
C                                                                       
   11     CONTINUE                                                      
          IF ( POINT .LT. 1 ) GO TO   12                                
C                                                                       
C        FETCH NEW INPUT DATUM                                          
C                                                                       
            DBUF(1)=FDATA(POINT)                                        
C                                                                       
C        CALCULATE NEW OUTPUT POINT                                     
C                                                                       
            OUT=A(1)*DBUF(1)                                            
            DO   13 I=2, ORDER                                          
              OUT=OUT+A(I)*DBUF(I)-B(I)*FBUF(I)                         
   13       CONTINUE                                                    
            FBUF(1)=OUT/B(1)                                            
            FDATA(POINT)=FBUF(1)                                        
C                                                                       
C        SHIFT BUFFERS                                                  
C                                                                       
            DO   14 I=2, ORDER                                          
              K=ORDER+2-I                                               
              FBUF(K)=FBUF(K-1)                                         
              DBUF(K)=DBUF(K-1)                                         
   14       CONTINUE                                                    
C                                                                       
C        UPDATE POINTER                                                 
C                                                                       
            POINT=POINT-1                                               
          GO TO   11                                                    
   12     CONTINUE                                                      
        END IF                                                          
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C WARP -- FUNCTION, APPLIES TANGENT FREQUENCY WARPING TO COMPENSATE     
C         FOR BILINEAR ANALOG -> DIGITAL TRANSFORMATION                 
C                                                                       
C ARGUMENTS:                                                            
C ----------                                                            
C                                                                       
C      F      ORIGINAL DESIGN FREQUENCY SPECIFICATION (HERTZ)           
C      T      SAMPLING INTERVAL                                         
C                                                                       
C  LAST MODIFIED:  JULY 7, 1980                                         
C                                                                       
      REAL FUNCTION WARP(F,T)                                           
C                                                                       
        TWOPI = 6.2831853                                               
        ANGLE = TWOPI*F*T/2.                                            
        WARP = 2.*TAN(ANGLE)/T                                          
        WARP = WARP/TWOPI                                               
C                                                                       
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  CHEBII -- SUBROUTINE TO COMPUTE CHEBYSHEV TYPE II FILTER             
C            POLYNOMIALS                                                
C                                                                       
C                                                                       
C  AUTHOR:  DAVID HARRIS                                                
C                                                                       
C  LAST MODIFIED:  DECEMBER 22, 1980                                    
C                                                                       
C                                                                       
C  ARGUMENTS:                                                           
C  ----------                                                           
C                                                                       
C      P              ARRAY OF NUMERATOR POLYNOMIAL COEFFICIENTS        
C                                                                       
C      Q              ARRAY OF DENOMINATOR POLYNOMIAL COEFFICIENTS      
C                                                                       
C      M              ORDER OF NUMERATOR POLYNOMIAL                     
C                                                                       
C      N              ORDER OF DENOMINATOR POLYNOMIAL                   
C                                                                       
C      IORD           ORDER OF FILTER (NUMBER OF POLES)                 
C                                                                       
C      A              STOPBAND ATTENUATION FACTOR                       
C                                                                       
C      OMEGAR         CUTOFF FREQUENCY OF STOPBAND                      
C                     PASSBAND CUTOFF IS AT 1.0 HERTZ                   
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND M=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE CHEBII(P,Q,M,N,IORD,A,OMEGAR)                          
C                                                                       
        INTEGER PTYPE(10),HALF                                          
        DOUBLE PRECISION P(*),Q(*)                                      
        COMPLEX ROOTS(10)                                               
        PI=3.14159265                                                   
        HALF=IORD/2                                                     
C                                                                       
C  INTERMEDIATE DESIGN PARAMETERS                                       
C                                                                       
        GAMMA=(A+SQRT(A*A-1.))                                          
        GAMMA=ALOG(GAMMA)/FLOAT(IORD)                                   
        GAMMA=EXP(GAMMA)                                                
        S=.5*(GAMMA-1./GAMMA)                                           
        C=.5*(GAMMA+1./GAMMA)                                           
C                                                                       
C  CALCULATE POLES                                                      
C                                                                       
        NROOTS=0                                                        
        DO   15 I=1, HALF                                               
          PTYPE(I)=0                                                    
          ANGLE=FLOAT(2*I-1)*PI/FLOAT(2*IORD)                           
          ALPHA=-S*SIN(ANGLE)                                           
          BETA=C*COS(ANGLE)                                             
          DENOM=ALPHA*ALPHA+BETA*BETA                                   
          SIGMA=OMEGAR*ALPHA/DENOM                                      
          OMEGA=-OMEGAR*BETA/DENOM                                      
          ROOTS(I)=CMPLX(SIGMA,OMEGA)                                   
          NROOTS=NROOTS+1                                               
   15   CONTINUE                                                        
        IF (  2*HALF .LT. IORD ) THEN                                   
          PTYPE(HALF+1)=1                                               
          ROOTS(HALF+1)=CMPLX(-OMEGAR/S,0.0)                            
          NROOTS=NROOTS+1                                               
        END IF                                                          
C                                                                       
C  DENOMINATOR POLYNOMIAL CONVERSION                                    
C                                                                       
        CALL CTPOLY(NROOTS,ROOTS,PTYPE,Q,N)                             
C                                                                       
C  CALCULATE ZEROS                                                      
C                                                                       
        NROOTS=0                                                        
        DO   16 I=1, HALF                                               
          PTYPE(I)=0                                                    
          ANGLE=FLOAT(2*I-1)*PI/FLOAT(2*IORD)                           
          OMEGA=OMEGAR/COS(ANGLE)                                       
          ROOTS(I)=CMPLX(0.0,OMEGA)                                     
          NROOTS=NROOTS+1                                               
   16   CONTINUE                                                        
C                                                                       
C  NUMERATOR POLYNOMIAL CONVERSION                                      
C                                                                       
        CALL CTPOLY(NROOTS,ROOTS,PTYPE,P,M)                             
C                                                                       
C  NORMALIZE NUMERATOR POLYNOMIAL SO THAT P(0)/Q(0) = 1.                
C                                                                       
        SCALE=Q(1)/P(1)                                                 
        DO   17 I=1, M+1                                                
          P(I)=P(I)*SCALE                                               
   17   CONTINUE                                                        
C                                                                       
C  DONE                                                                 
C                                                                       
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  CHEBI -- SUBROUTINE TO COMPUTE CHEBYSHEV TYPE I FILTER               
C            POLYNOMIALS                                                
C                                                                       
C                                                                       
C  AUTHOR:  DAVID HARRIS                                                
C                                                                       
C  LAST MODIFIED:  DECEMBER 22, 1980                                    
C                                                                       
C                                                                       
C  ARGUMENTS:                                                           
C  ----------                                                           
C                                                                       
C      P              ARRAY OF NUMERATOR POLYNOMIAL COEFFICIENTS        
C                                                                       
C      Q              ARRAY OF DENOMINATOR POLYNOMIAL COEFFICIENTS      
C                                                                       
C      M              ORDER OF NUMERATOR POLYNOMIAL                     
C                                                                       
C      N              ORDER OF DENOMINATOR POLYNOMIAL                   
C                                                                       
C      IORD           ORDER OF FILTER (NUMBER OF POLES)                 
C                                                                       
C      EPS            CHEBYSHEV PARAMETER RELATED TO PASSBAND RIPPLE    
C                                                                       
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND M=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE CHEBI(P,Q,M,N,IORD,EPS)                                
C                                                                       
        INTEGER PTYPE(10),HALF                                          
        DOUBLE PRECISION P(1), Q(1)                                     
        COMPLEX POLE(10)                                                
        PI=3.14159265                                                   
        HALF=IORD/2                                                     
C                                                                       
C  NUMERATOR POLYNOMIAL                                                 
C                                                                       
        P(1)=1.D0                                                       
        DO   18 I=2, IORD+1                                             
          P(I)=0.D0                                                     
   18   CONTINUE                                                        
        M=0                                                             
C                                                                       
C  INTERMEDIATE DESIGN PARAMETERS                                       
C                                                                       
        GAMMA=(1.+SQRT(1.+EPS*EPS))/EPS                                 
        GAMMA=ALOG(GAMMA)/FLOAT(IORD)                                   
        GAMMA=EXP(GAMMA)                                                
        S=.5*(GAMMA-1./GAMMA)                                           
        C=.5*(GAMMA+1./GAMMA)                                           
C                                                                       
C  CALCULATE POLES                                                      
C                                                                       
        NPOLES=0                                                        
        DO   19 I=1, HALF                                               
          PTYPE(I)=0                                                    
          ANGLE=FLOAT(2*I-1)*PI/FLOAT(2*IORD)                           
          SIGMA=-S*SIN(ANGLE)                                           
          OMEGA=C*COS(ANGLE)                                            
          POLE(I)=CMPLX(SIGMA,OMEGA)                                    
          NPOLES=NPOLES+1                                               
   19   CONTINUE                                                        
        IF (  2*HALF .LT. IORD ) THEN                                   
          PTYPE(HALF+1)=1                                               
          POLE(HALF+1)=CMPLX(-S,0.0)                                    
          NPOLES=NPOLES+1                                               
        END IF                                                          
C                                                                       
C  DENOMINATOR POLYNOMIAL CONVERSION                                    
C                                                                       
        CALL CTPOLY(NPOLES,POLE,PTYPE,Q,N)                              
C                                                                       
C  NORMALIZE NUMERATOR POLYNOMIAL SO THAT P(0)/Q(0) = 1.                
C                                                                       
        P(1)=Q(1)                                                       
C                                                                       
C  DONE                                                                 
C                                                                       
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  MPOLYS -- SUBROUTINE TO MULTIPLY TWO POLYNOMIALS                     
C                                                                       
C                                                                       
C  AUTHOR:  DAVE HARRIS                                                 
C                                                                       
C  LAST MODIFIED:  DECEMBER 11, 1980                                    
C                                                                       
C                                                                       
C  ARGUMENTS:                                                           
C  ----------                                                           
C                                                                       
C      P1                ARRAY CONTAINING FIRST POLYNOMIAL              
C                                                                       
C      N1                ORDER OF FIRST POLYNOMIAL                      
C                                                                       
C      P2                ARRAY CONTAINING SECOND POLYNOMIAL             
C                                                                       
C      N2                ORDER OF SECOND POLYNOMIAL                     
C                                                                       
C      Q                 ARRAY CONTAINING PRODUCT POLYNOMIAL            
C                                                                       
C      M                 ORDER OF PRODUCT POLYNOMIAL                    
C                                                                       
C                                                                       
C  NOTE:  POLYNOMIAL COEFFICIENT ARRAYS HAVE LOW ORDER POLYNOMIAL       
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND N=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE MPOLYS(P1,N1,P2,N2,Q,M)                                
        DOUBLE PRECISION P1(*),P2(*),Q(*)                               
C                                                                       
C  COMPUTE ORDER OF NEW POLYNOMIAL                                      
C                                                                       
        M=N1+N2                                                         
C                                                                       
C  NEW POLYNOMIAL FOUND BY CONVOLUTION OF P1 AND P2                     
C                                                                       
        DO   20 I=0, M                                                  
          IP1=I+1                                                       
          Q(IP1)=0.                                                     
          LIM1=MAX0(0,I-N2)                                             
          LIM2=MIN0(I,N1)                                               
          DO   21 J=LIM1, LIM2                                          
            Q(IP1)=Q(IP1)+P1(J+1)*P2(I-J+1)                             
   21     CONTINUE                                                      
   20   CONTINUE                                                        
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  CTPOLY -- SUBROUTINE TO CONVERT ROOT DESCRIPTION TO POLYNOMIAL       
C            DESCRIPTION                                                
C                                                                       
C                                                                       
C  AUTHOR:  DAVID HARRIS                                                
C                                                                       
C  LAST MODIFIED:  DECEMBER 12, 1980                                    
C                                                                       
C                                                                       
C  ARGUMENTS:                                                           
C  ----------                                                           
C                                                                       
C   NROOTS                 NUMBER OF ROOTS IN DESCRIPTION               
C                                                                       
C   ROOTS                   COMPLEX ARRAY CONTAINING ROOTS OF POLYNOMIAL
C                                                                       
C   RTYPE                  INTEGER ARRAY CONTAINING CODE FOR TYPE OF ROO
C                          0    -- COMPLEX CONJUGATE PAIR               
C                          ]] 0  -- REAL ROOTS REPEATED RTYPE TIMES     
C                                                                       
C   P                      POLYNOMIAL COEFFICIENT ARRAY                 
C                                                                       
C   N                      ORDER OF POLYNOMIAL IN P                     
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND N=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE CTPOLY(NROOTS,ROOTS,RTYPE,P,N)                         
        COMPLEX ROOTS(1)                                                
        INTEGER RTYPE(1)                                                
        DOUBLE PRECISION P(*),C(3),TEMP(21)                             
C                                                                       
C  INITIALIZE POLYNOMIAL                                                
C                                                                       
        N=0                                                             
        DO   22 I=1, NROOTS                                             
          IF (  RTYPE(I) .EQ. 0 ) THEN                                  
            N=N+2                                                       
          ELSE                                                          
            N=N+RTYPE(I)                                                
          END IF                                                        
   22   CONTINUE                                                        
        DO   23 I=1, N+1                                                
          P(I)=0.D0                                                     
   23   CONTINUE                                                        
        P(1)=1.D0                                                       
        N=0                                                             
C                                                                       
C  MULTIPLY OUT SECOND ORDER SECTIONS                                   
C                                                                       
        DO   24 I=1, NROOTS                                             
C                              CONJUGATE PAIR OF ROOTS                  
          IF (  RTYPE(I) .EQ. 0 ) THEN                                  
            C(3)=1.D0                                                   
            C(2)=-2.D0*REAL(ROOTS(I))                                   
            C(1)=REAL(ROOTS(I)*CONJG(ROOTS(I)))                         
            CALL MPOLYS(P,N,C,2,TEMP,NT)                                
            N=NT                                                        
            DO   25 J=1, N+1                                            
              P(J)=TEMP(J)                                              
   25       CONTINUE                                                    
C                              REAL ROOTS                               
          ELSE                                                          
            DO   26 J=1, RTYPE(I)                                       
              C(2)=1.D0                                                 
              C(1)=-REAL(ROOTS(I))                                      
              CALL MPOLYS(P,N,C,1,TEMP,NT)                              
              N=NT                                                      
              DO   27 K=1, N+1                                          
                P(K)=TEMP(K)                                            
   27         CONTINUE                                                  
   26       CONTINUE                                                    
          END IF                                                        
   24   CONTINUE                                                        
C                                                                       
C  BYE                                                                  
C                                                                       
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  BESSEL - SUBROUTINE TO GENERATE BESSEL SYSTEM FUNCTION               
C                                                                       
C  AUTHOR: DAVE HARRIS                                                  
C                                                                       
C  LAST MODIFIED:  DECEMBER 11, 1980                                    
C                                                                       
C  ARGUMENTS:                                                           
C  ----------                                                           
C                                                                       
C      P          NUMERATOR POLYNOMIAL COEFFICIENT ARRAY                
C                                                                       
C      Q          DENOMINATOR ARRAY                                     
C                                                                       
C      M          ORDER OF NUMERATOR POLYNOMIAL                         
C                                                                       
C      N          ORDER OF DENOMINATOR                                  
C                                                                       
C      IORD       DESIRED NUMBER OF POLES                               
C                                                                       
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND M=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE BESSEL(P,Q,M,N,IORD)                                   
        DOUBLE PRECISION P(*),Q(*),TABLE(2,4,8),C1,C2,TEMP(11),C(11)    
C                                                                       
C  FILL TABLE OF BESSEL POLES                                           
C                                                                       
        DO   28 I=1, 8                                                  
          DO   29 J=1, 4                                                
            DO   30 K=1, 2                                              
              TABLE(K,J,I)=0.D0                                         
   30       CONTINUE                                                    
   29     CONTINUE                                                      
   28   CONTINUE                                                        
C                                                                       
C                                                                       
C                                                                       
C                                     FIRST ORDER POLES                 
        TABLE(1,1,1)=-1.D0                                              
C                                                                       
C                                     SECOND ORDER POLES                
        TABLE(1,1,2)=-1.1016013                                         
        TABLE(2,1,2)= 0.6360098                                         
C                                                                       
C                                     THIRD ORDER POLES                 
        TABLE(1,1,3)=-1.0474091                                         
        TABLE(2,1,3)= 0.9992645                                         
C                                                                       
        TABLE(1,2,3)=-1.3226758                                         
C                                                                       
C                                                                       
C                                     FOURTH ORDER POLES                
        TABLE(1,1,4)=-0.9952088                                         
        TABLE(2,1,4)= 1.2571058                                         
C                                                                       
        TABLE(1,2,4)=-1.3700679                                         
        TABLE(2,2,4)= 0.4102497                                         
C                                                                       
C                                                                       
C                                     FIFTH ORDER POLES                 
        TABLE(1,1,5)=-0.9576766                                         
        TABLE(2,1,5)= 1.4711244                                         
C                                                                       
        TABLE(1,2,5)=-1.3808774                                         
        TABLE(2,2,5)= 0.7179096                                         
C                                                                       
        TABLE(1,3,5)=-1.5023160                                         
C                                                                       
C                                                                       
C                                     SIXTH ORDER POLES                 
        TABLE(1,1,6)=-0.9306565                                         
        TABLE(2,1,6)= 1.6618633                                         
C                                                                       
        TABLE(1,2,6)=-1.3818581                                         
        TABLE(2,2,6)= 0.9714719                                         
C                                                                       
        TABLE(1,3,6)=-1.5714904                                         
        TABLE(2,3,6)= 0.3208964                                         
C                                                                       
C                                                                       
C                                     SEVENTH ORDER POLES               
        TABLE(1,1,7)=-0.9098678                                         
        TABLE(2,1,7)= 1.8364514                                         
C                                                                       
        TABLE(1,2,7)=-1.3789032                                         
        TABLE(2,2,7)= 1.1915667                                         
C                                                                       
        TABLE(1,3,7)=-1.6120388                                         
        TABLE(2,3,7)= 0.5892445                                         
C                                                                       
        TABLE(1,4,7)=-1.6843682                                         
C                                                                       
C                                                                       
C                                     EIGHTH ORDER POLES                
        TABLE(1,1,8)=-0.8928710                                         
        TABLE(2,1,8)= 1.9983286                                         
C                                                                       
        TABLE(1,2,8)=-1.3738431                                         
        TABLE(2,2,8)= 1.3883585                                         
C                                                                       
        TABLE(1,3,8)=-1.6369417                                         
        TABLE(2,3,8)= 0.8227968                                         
C                                                                       
        TABLE(1,4,8)=-1.7574108                                         
        TABLE(2,4,8)= 0.2728679                                         
C                                                                       
C  INITIALIZE POLYNOMIALS                                               
C                                                                       
        IHALF=IORD/2                                                    
        IOP=IORD+1                                                      
        DO   31 I=1, IOP                                                
          P(I)=0.D0                                                     
          Q(I)=0.D0                                                     
   31   CONTINUE                                                        
        P(1)=1.D0                                                       
        Q(1)=1.D0                                                       
        M=0                                                             
        N=0                                                             
C                                                                       
C  GENERATE DENOMINATOR POLYNOMIAL BY MULTIPLYING SECOND ORDER SECTIONS 
C                                                                       
        DO   32 I=1, IHALF                                              
          C1=TABLE(1,I,IORD)                                            
          C2=TABLE(2,I,IORD)                                            
          C(3)=1.D0                                                     
          C(2)=-2.D0*C1                                                 
          C(1)=C1*C1+C2*C2                                              
          CALL MPOLYS(Q,N,C,2,TEMP,NT)                                  
          DO   33 J=1, NT+1                                             
            Q(J)=TEMP(J)                                                
   33     CONTINUE                                                      
          N=NT                                                          
   32   CONTINUE                                                        
C                                                                       
C  TEST FOR ODD ORDER FILTER                                            
C                                                                       
        IF (  IHALF*2 .LT. IORD ) THEN                                  
          C1=TABLE(1,IHALF+1,IORD)                                      
          C(2)=1.D0                                                     
          C(1)=-C1                                                      
          CALL MPOLYS(Q,N,C,1,TEMP,NT)                                  
          DO   34 J=1, NT+1                                             
            Q(J)=TEMP(J)                                                
   34     CONTINUE                                                      
          N=NT                                                          
        END IF                                                          
C                                                                       
C  NORMALIZE NUMERATOR POLYNOMIAL SO THAT P(0)/Q(0) = 1.                
C                                                                       
        P(1)=Q(1)                                                       
C                                                                       
C  DONE                                                                 
C                                                                       
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C LPTBP -- SUBROUTINE TO TRANSFORM ANALOG LOWPASS FILTER TO             
C          ANALOG BANDPASS FILTER                                       
C                                                                       
C  AUTHOR:  DAVE HARRIS                                                 
C                                                                       
C  LAST MODIFIED:  DECEMBER 11, 1980                                    
C                                                                       
C                                                                       
C ARGUMENTS:                                                            
C ----------                                                            
C                                                                       
C     P       ARRAY CONTAINING NUMERATOR POLYNOMIAL COEFFICIENTS        
C     Q       ARRAY CONTAINING DENOMINATOR POLYNOMIAL COEFFICIENTS      
C     M       ORDER OF NUMERATOR POLYNOMIAL                             
C     N       ORDER OF DENOMINATOR POLYNOMIAL                           
C     F1      LOW FREQUENCY CUTOFF (HERTZ)                              
C     F2      HIGH FREQUENCY CUTOFF (HERTZ)                             
C                                                                       
C  NOTE:  PROTOTYPE ANALOG FILTER SUPPLIED AS INPUT IN P AND Q          
C         POLYNOMIAL ARRAYS IS ASSUMED TO BE DESIGNED TO BE A LOWPASS   
C         FILTER WITH CUTOFF AT ONE HERTZ.  P AND Q CONTAIN THE         
C         MODIFIED FILTER AS OUTPUT.                                    
C                                                                       
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND M=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE LPTBP(P,Q,M,N,F1,F2)                                   
        DOUBLE PRECISION P(*),Q(*),A(21),B(21)                          
        DOUBLE PRECISION TEMPA(21),TEMPB(21),ALPHA,BETA,SCALE           
        PI=3.14159265                                                   
        TWOPI=2.*PI                                                     
        DO   35 I=1, 21                                                 
          A(I)=0.                                                       
          B(I)=0.                                                       
   35   CONTINUE                                                        
        NMAX=MAX0(M,N)                                                  
        ALPHA=TWOPI*TWOPI*F1*F2                                         
        BETA=TWOPI*(F2-F1)                                              
C                                                                       
C INITIALIZE RECURSION                                                  
C                                                                       
        A(1)=P(NMAX+1)                                                  
        B(1)=Q(NMAX+1)                                                  
        SCALE=1.                                                        
C                                                                       
C RECURSION                                                             
C                                                                       
        DO   36 I=1, NMAX                                               
          SCALE=SCALE*BETA                                              
          IP1=I+1                                                       
          DO   37 I1=1, 21                                              
            TEMPA(I1)=0.                                                
            TEMPB(I1)=0.                                                
   37     CONTINUE                                                      
          DO   38 J=1, 2*I-1                                            
            TEMPA(J)=ALPHA*A(J)+TEMPA(J)                                
            TEMPA(J+2)=TEMPA(J+2)+A(J)                                  
            TEMPB(J)=TEMPB(J)+ALPHA*B(J)                                
            TEMPB(J+2)=TEMPB(J+2)+B(J)                                  
   38     CONTINUE                                                      
          TEMPA(IP1)=TEMPA(IP1)+SCALE*P(NMAX+1-I)                       
          TEMPB(IP1)=TEMPB(IP1)+SCALE*Q(NMAX+1-I)                       
          DO   39 J=1, 2*I+1                                            
            A(J)=TEMPA(J)                                               
            B(J)=TEMPB(J)                                               
   39     CONTINUE                                                      
   36   CONTINUE                                                        
C                                                                       
        DO   40 I=1, 21                                                 
          P(I)=A(I)                                                     
          Q(I)=B(I)                                                     
   40   CONTINUE                                                        
        M=NMAX+M                                                        
        N=NMAX+N                                                        
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C LPTHP -- SUBROUTINE TO PERFORM LOWPASS TO HIGHPASS                    
C          TRANSFORMATION ON ANALOG FILTERS                             
C                                                                       
C  AUTHOR:  DAVE HARRIS                                                 
C                                                                       
C  LAST MODIFIED:  DECEMBER 11, 1980                                    
C                                                                       
C                                                                       
C ARGUMENTS:                                                            
C ----------                                                            
C                                                                       
C     P         ARRAY CONTAINING NUMERATOR POLYNOMIAL COEFFICIENTS      
C     Q         ARRAY CONTAINING DENOMINATOR POLYNOMIAL COEFFICIENTS    
C     M         ORDER OF NUMERATOR POLYNOMIAL                           
C     N         ORDER OF DENOMINATOR POLYNOMIAL                         
C                                                                       
C  NOTE:  PROTOTYPE ANALOG FILTER SUPPLIED AS INPUT IN P AND Q          
C         POLYNOMIAL ARRAYS IS ASSUMED TO BE DESIGNED TO BE A LOWPASS   
C         FILTER WITH CUTOFF AT ONE HERTZ.  P AND Q CONTAIN THE         
C         MODIFIED FILTER AS OUTPUT.                                    
C                                                                       
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND M=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
C LAST MODIFIED:  JULY 7, 1980                                          
C                                                                       
C AUTHOR:  DAVID HARRIS                                                 
C                                                                       
      SUBROUTINE LPTHP(P,Q,M,N)                                         
        DOUBLE PRECISION P(*),Q(*),TEMP(21)                             
        NMAX=MAX0(M,N)                                                  
C                                                                       
        DO   41 I=1, NMAX+1                                             
          K=NMAX+2-I                                                    
          TEMP(I)=P(K)                                                  
   41   CONTINUE                                                        
        DO   42 I=1, NMAX+1                                             
          K=NMAX+2-I                                                    
          P(I)=TEMP(I)                                                  
          TEMP(I)=Q(K)                                                  
   42   CONTINUE                                                        
        DO   43 I=1, NMAX+1                                             
          Q(I)=TEMP(I)                                                  
   43   CONTINUE                                                        
        M=NMAX                                                          
        N=NMAX                                                          
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C CUTOFF -- SUBROUTINE TO CHANGE CUTOFF OF ANALOG FILTER                
C                                                                       
C ARGUMENTS:                                                            
C ----------                                                            
C                                                                       
C     P         ARRAY CONTAINING NUMERATOR POLYNOMIAL COEFFICIENTS      
C     Q         ARRAY CONTAINING DENOMINATOR POLYNOMIAL COEFFICIENTS    
C     M         ORDER OF NUMERATOR POLYNOMIAL                           
C     N         ORDER OF DENOMINATOR POLYNOMIAL                         
C     F         NEW CUTOFF FREQUENCY IN HERTZ                           
C                                                                       
C  NOTE:  PROTOTYPE ANALOG FILTER SUPPLIED AS INPUT IN P AND Q          
C         POLYNOMIAL ARRAYS IS ASSUMED TO BE DESIGNED TO BE A LOWPASS   
C         FILTER WITH CUTOFF AT ONE HERTZ.  P AND Q CONTAIN THE         
C         MODIFIED FILTER AS OUTPUT.                                    
C                                                                       
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND M=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE CUTOFF(P,Q,M,N,F)                                      
        DOUBLE PRECISION P(*),Q(*)                                      
        PI=3.14159265                                                   
        TWOPI=2.*PI                                                     
        ANGF=TWOPI*F                                                    
        SCALE=1.                                                        
        NMAX=MAX0(M,N)                                                  
        DO   44 I=2, NMAX+1                                             
          SCALE=SCALE/(ANGF)                                            
          P(I)=P(I)*SCALE                                               
          Q(I)=Q(I)*SCALE                                               
   44   CONTINUE                                                        
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C BUTTER -- SUBROUTINE TO COMPUTE BUTTERWORTH POLYNOMIAL                
C                                                                       
C LAST MODIFIED:  JULY 7, 1980                                          
C                                                                       
C  ARGUMENTS:                                                           
C  ----------                                                           
C      P              NUMERATOR POLYNOMIAL                              
C                                                                       
C      Q              DENOMINATOR POLYNOMIAL                            
C                                                                       
C      M              ORDER OF NUMERATOR POLYNOMIAL                     
C                                                                       
C      N              ORDER OF DENOMINATOR POLYNOMIAL                   
C                                                                       
C      IORD           DESIRED NUMBER OF POLES                           
C                                                                       
C                                                                       
C  NOTE:  NUMERATOR AND DENOMINATOR ARRAYS HAVE LOW ORDER POLYNOMIAL    
C         COEFFICIENTS STORED IN FIRST ELEMENT.  THE NUMBER OF          
C         COEFFICIENTS IS ONE GREATER THAN THE ORDER.  THUS, THE        
C         POLYNOMIAL:                                                   
C                        A(S) = S**2 + 2.*S + 3.                        
C         WILL BE STORED AS:                                            
C                        P(1) = 3.                                      
C                        P(2) = 2.                                      
C                        P(3) = 1.                                      
C         AND M=2.  POLYNOMIAL COEFFICIENT ARRAYS ARE DOUBLE PRECISION. 
C                                                                       
      SUBROUTINE BUTTER(P, Q, M, N, IORD)                               
C                                                                       
        DOUBLE PRECISION P(*), Q(*), TEMP(21), PI, ANGLE                
        INTEGER HALF                                                    
C                                                                       
        PI=3.14159265359                                                
        DO   45 I=1, 21                                                 
          P(I)=0.                                                       
          Q(I)=0.                                                       
   45   CONTINUE                                                        
        P(1)=1.                                                         
        Q(1)=1.                                                         
        M=0                                                             
        N=0                                                             
        HALF=IORD/2                                                     
C                                                                       
C TEST FOR ODD ORDER, AND ADD POLE AT -1                                
C                                                                       
        IF (  2*HALF .LT. IORD ) THEN                                   
          Q(2)=1.                                                       
          N=1                                                           
        END IF                                                          
C                                                                       
        DENOM=2.*FLOAT(IORD)                                            
        DO   46 K=1, HALF                                               
          ANGLE=PI*(.5+FLOAT(2*K-1)/DENOM)                              
          SCALE=-2.*DCOS(ANGLE)                                         
          DO   47 K1=1, 21                                              
            TEMP(K1)=0.                                                 
   47     CONTINUE                                                      
          DO   48 J=1, N+1                                              
            TEMP(J)=TEMP(J)+Q(J)                                        
            TEMP(J+1)=TEMP(J+1)+SCALE*Q(J)                              
            TEMP(J+2)=TEMP(J+2)+Q(J)                                    
   48     CONTINUE                                                      
          N=N+2                                                         
          DO   49 J=1, N+1                                              
            Q(J)=TEMP(J)                                                
   49     CONTINUE                                                      
   46   CONTINUE                                                        
C                                                                       
C  NORMALIZE NUMERATOR POLYNOMIAL SO THAT P(0)/Q(0) = 1.                
C                                                                       
        P(1)=Q(1)                                                       
C                                                                       
      RETURN                                                            
      END                                                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C SUBROUTINE BILIN -- BILINEAR TRANSFORMATION FOR GENERATING DIGITAL    
C                     FILTERS FROM ANALOG FILTERS                       
C                                                                       
C ARGUMENTS:                                                            
C ----------                                                            
C                                                                       
C P        ARRAY CONTAINING COEFFICIENTS OF ANALOG FILTER NUMERATOR     
C          POLYNOMIAL                                                   
C Q        ARRAY CONTAINING COEFFICIENTS OF ANALOG FILTER DENOMINATOR   
C          POLYNOMIAL                                                   
C M        ORDER OF ANALOG FILTER NUMERATOR POLYNOMIAL                  
C N        ORDER OF ANALOG FILTER DENOMINATOR POLYNOMIAL                
C A        NUMERATOR POLYNOMIAL OF DIGITAL FILTER                       
C B        DENOMINATOR POLYNOMIAL OF DIGITAL FILTER                     
C T        SAMPLING INTERVAL IN SECONDS                                 
C                                                                       
C COMMENT:  IN ALL ARRAYS, THE CONSTANT COEFFICIENTS OF THE POLYNOMIALS 
C           ARE IN THE FIRST ARRAY ELEMENTS, AND THE REST OCCUR IN      
C           ORDER OF INCREASING POWER.  RECALL THAT A POLYNOMIAL OF     
C           DEGREE N HAS N+1 COEFFICIENTS                               
C                THE RESULTING DIGITAL FILTER POLYNOMIALS               
C           SHOULD BE ASSUMED TO HAVE DEGREE MAX(M,N), ALTHOUGH         
C           SOME COEFFICIENTS MAY BE ZERO.  THIS PROGRAM IS             
C           LIMITED TO FILTERS OF ORDER 20, THOUGH THIS LIMIT           
C           MAY BE RELAXED BY REDIMENSIONING ARRAYS BIN AND             
C           TEMP.                                                       
C                A MODIFIED HORNER SCHEME IS USED TO                    
C           EXPAND THE DIGITAL FILTER POLYNOMIALS.                      
C                                                                       
C AUTHOR:  DAVID HARRIS                                                 
C                                                                       
C LAST MODIFIED:  JULY 2, 1980                                          
C                                                                       
      SUBROUTINE BILIN(P,Q,M,N,A,B,T)                                   
        DOUBLE PRECISION P(*),Q(*),A(*),B(*)                            
        DOUBLE PRECISION BIN(21),TEMP(21),TOP,BOT                       
C                                                                       
        NMAX=MAX0(M,N)                                                  
        DO   50 I=1, NMAX+1                                             
          A(I)=0.                                                       
          B(I)=0.                                                       
   50   CONTINUE                                                        
        DO   51 I=1, 21                                                 
          BIN(I)=0.                                                     
   51   CONTINUE                                                        
C                                                                       
C  INITIALIZE RECURSION FOR BILINEAR TRANSFORM                          
C                                                                       
        A(1)=P(NMAX+1)                                                  
        B(1)=Q(NMAX+1)                                                  
        SCALE=2./T                                                      
        BIN(1)=1.                                                       
C                                                                       
C MAIN RECURSION                                                        
C                                                                       
        DO   52 I=1, NMAX                                               
          IP1=I+1                                                       
C                                                                       
C   COMPUTE BINOMIAL COEFFICIENTS AS PART OF RECURSION                  
C                                                                       
          DO   53 I1=1, 21                                              
            TEMP(I1)=0.                                                 
   53     CONTINUE                                                      
          DO   54 J=2, IP1                                              
            TEMP(J)=BIN(J)+BIN(J-1)                                     
   54     CONTINUE                                                      
          DO   55 J=2, IP1                                              
            BIN(J)=TEMP(J)                                              
   55     CONTINUE                                                      
C                                                                       
C   ADD NEW INCREMENTS TO DIGITAL FILTER POLYNOMIALS                    
C                                                                       
          TOP=P(NMAX+1-I)                                               
          BOT=Q(NMAX+1-I)                                               
          DO   56 J=1, I                                                
            A(J)=SCALE*A(J)                                             
            B(J)=SCALE*B(J)                                             
   56     CONTINUE                                                      
          TEMP(1)=A(1)                                                  
          DO   57 J= 2, IP1                                             
            TEMP(J)=A(J)-A(J-1)                                         
   57     CONTINUE                                                      
          DO   58 J=1, IP1                                              
            A(J)=TEMP(J)+TOP*BIN(J)                                     
   58     CONTINUE                                                      
          TEMP(1)=B(1)                                                  
          DO   59 J=2, IP1                                              
            TEMP(J)=B(J)-B(J-1)                                         
   59     CONTINUE                                                      
          DO   60 J=1, IP1                                              
            B(J)=TEMP(J)+BOT*BIN(J)                                     
   60     CONTINUE                                                      
   52   CONTINUE                                                        
        M=NMAX                                                          
        N=NMAX                                                          
      RETURN                                                            
      END                                                               
