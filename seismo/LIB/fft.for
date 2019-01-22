c
c   new routine for fft for SEISAN, put in by jh, jan 2002
c   the old routines has problem with numerical overflow
c   on solaris
c   
c   the old routine is listed below
c
c   this new routine was origianlly for double precision, has been
c   modified to single precision since it is so in all SEISAN calls
c   modifications are indicated by cx. modification also include
c   changing normalization
c
c
c 16 12 2012 jh: change (1) to (*)
c
cx      Subroutine FFT(A,m,INV)
      Subroutine FFT(m,INV,A)
*------------------------------------------------------------------*
*                                                                  *
* A First Course in Computational Physics                          *
*                                                                  *
* Paul L. DeVries, Department of Physics, Miami University         *
*                                                                  *
* This subroutine performs the Fast Fourier Transform by           *
* the method of Cooley and Tukey --- the FORTRAN code was          *
* adapted from                                                     *
*                                                                  *
*   Cooley, Lewis, and Welch, IEEE Transactions E-12               *
*       (March 1965).                                              *
*                                                                  *
* The array A contains the complex data to be transformed,         *
* `m' is log2(N), and INV is an index = 1 if the inverse           *
* transform is to be computed. (The forward transform is           *
* evaluated if INV is not = 1.)                                    *
*                                                                  *
*                            start: 1965                           *
*                    last modified: 1993                           *
*                                                                  *
*------------------------------------------------------------------*
*
cx       Complex*16 A(1), u, w, t 
       Complex A(*), u, w, t
       Double precision ang, pi
       real inv  !cx
       Integer N, Nd2, i, j, k, l, le, le1, ip
       Parameter (pi = 3.141592653589793d0)
*
*  This routine computes the Fast Fourier Transform of the 
*  input data and returns it in the same array. Note that 
*  the k's and x's are related in the following way:
*
*    IF    K = range of k's      and     X = range of x's
*
*    THEN  delta-k = 2 pi / X    and   delta-x = 2 pi / K
*        
*  When the transform is evaluated, it is assumed that the 
*  input data is periodic. The output is therefore periodic 
*  (you have no choice in this). Thus, the transform is 
*  periodic in k-space, with the first N/2 points being 
*  'most significant'. The second N/2 points are the same 
*  as the Fourier transform at negative k!!! That is,
*
*              FFT(N+1-i) = FFT(-i)  ,i = 1,2,....,N/2
*
       N   = 2**m
       Nd2 = N/2
       j   = 1
       DO i = 1, N-1
          IF( i .lt. j ) THEN
             t    = A(j)
             A(j) = A(i)
             A(i) = t
          ENDIF
          k = Nd2
100       IF( k .lt. j ) THEN
             j = j-k
             k = k/2
             goto 100
          ENDIF
          j  = j+k
       END DO
       le = 1       
       DO l = 1, m
          le1 = le
          le  = le + le

          u = ( 1.D0, 0.D0 )
          ang = pi / dble(le1)
          W = Dcmplx( cos(ang), -sin(ang) )
cx          IF(inv .eq. 1) W = Dconjg(W)
          IF(inv .gt. 0.0) W = conjg(W)

          DO j = 1, le1
             DO i = j, N, le
                ip   = i+le1
                t    = A(ip)*u
                A(ip)= A(i)-t
                A(i) = A(i)+t
             END DO
             u=u*w
          END DO

       END DO

cx       IF(inv .ne. 1) THEN
cx          DO i = 1, N
cx             A(i) = A(i) / dble(N)
cx          END DO
cx       ENDIF
       return
       end



c    old routine
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   
C                                                                               
c     SUBROUTINE FFT(NR,SIGN,DAT)                                               
C                                                                               
C   FAST FOURIER TRANSFORM OF 2**NR COMPLEX POINTS DAT.                         
C   SIGN=-1 : FROM TIME TO FREQUENCY                                            
C   SIGN=+1 : FROM FREQUENCY TO TIME                                            
C                                                                               
c     COMPLEX COMP,DAT,EIFI                                                     
c     DIMENSION DAT(1)                                                          
c     DIMENSION IPW2(14)                                                        
c     NTOTAL=2**NR                                                              
c     ANG=SIGN*6.283185307E+00/FLOAT(NTOTAL)                                    
c     JF=NTOTAL                                                                 
c     DO 10 II=1,NR                                                             
c     JF=JF/2                                                                   
c  10 IPW2(II)=JF                                                               
c     I2P1=1                                                                    
c     I2RP=NTOTAL                                                               
c     DO 40 IP=1,NR                                                             
c     I2RP1=I2RP                                                                
c     I2RP=I2RP/2                                                               
c     MINV=0                                                                    
c     DO 41 MDIR=1,I2P1                                                         
c     QR=ANG*FLOAT(MINV)                                                        
c     EIFI=CMPLX(COS(QR),SIN(QR))                                               
c     ICONST=I2RP1*(MDIR-1)                                                     
c     DO 20 II=1,I2RP                                                           
c     JF=ICONST+II                                                              
c     JL=JF+I2RP                                                                
c     COMP=DAT(JL)*EIFI                                                         
c     DAT(JL)=DAT(JF)-COMP                                                      
c  20 DAT(JF)=DAT(JF)+COMP                                                      
c     DO 30 II=2,NR                                                             
c     JF=IPW2(II)                                                               
c     JL=MINV/JF                                                                
c     ICONST=JL/2                                                               
c     IF(JL-ICONST-ICONST) 41,41,30                                             
c  30 MINV=MINV-JF                                                              
c  41 MINV=MINV+JF                                                              
c  40 I2P1=I2P1+I2P1                                                            
c     I2N=NTOTAL-1                                                              
c     MINV=IPW2(1)                                                              
c     DO 60 IJ=2,I2N                                                            
c     IF(MINV-IJ)48,47,47                                                       
c  47 COMP=DAT(IJ)                                                              
c     JF=MINV+1                                                                 
c     DAT(IJ)=DAT(JF)                                                           
c     DAT(JF)=COMP                                                              
c  48 DO 49 II=1,NR                                                             
c     JF=IPW2(II)                                                               
c     JL=MINV/JF                                                                
c     ICONST=JL/2                                                               
c     IF(JL-ICONST-ICONST)60,60,49                                              
c  49 MINV=MINV-JF                                                              
c  60 MINV = MINV + JF                                                          
c                                                                               
C      IF(SIGN.EQ. -1.0) DAT=(0.0,0.0)  ! this line was always commented out                                        
C                                                                               
c     RETURN                                                                    
c     END                                                                       
