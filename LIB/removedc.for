CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       
C                                                                               
      SUBROUTINE REMOVE_DC(DATABUFF,NDC,DC,NSAMP)                               
C                                                                               
C     ROUTINE TO REMOVE DC                                                      
c
c  sep 98 by jh :      --------------   version 7.0 check ---------------
c                      no change
C                                                                               
C                                                                               
c--DATA FROM WHICH TO REMOVE DC              
      REAL        DATABUFF(*)        
c--NUMBER OF SAMPLES TO USE FOR              
      INTEGER     NDC                
c--DC CALCULATION,                           
                                     
c--NDC=0: NDC=NSAMP                          
                                     
c--DC LEVEL FOUND                            
      REAL        DC                 
c--TOTAL NUMBER OF SAMPLES IN DATABUFF       
      INTEGER     NSAMP              
C                                                                               
      IF(NDC.EQ.0)THEN                                                          
          NDC=NSAMP                                                             
      ENDIF                                                                     
      DC=0.0                                                                    
      DO 10 I=1,NDC                                                             
         DC=DC+DATABUFF(I)/NDC                                                  
 10   CONTINUE                                                                  
      DO 100 I=1,NSAMP                                                          
          DATABUFF(I)=DATABUFF(I)-DC                                            
 100  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
