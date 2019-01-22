c
c   average q relations, jh oct 2010   
c   
c changes
c
c  17 03 2014 jh: change to also read format out of automag grid
c  30 08 2014 jh: change to also output av stress drop, use stress drop limit
c                 add plot odf curves
c  05 01 2015 jh: stress drop limits now in automag, add seisan.def values
c                 will not plot without in linux, add option for min number of
c                 good fit to use
c

      implicit none   
      include 'gmt_xy_par.inc'            ! include block for gmt_xy routines
      include 'seiplot.inc'               ! seisan graphics
      include 'version.inc'               ! version
      include 'seisan.inc'
                    

      real x(10),y(10)                    ! xy values


      integer n,i,k,ntotal,kstart,naverage
      character*80 text
      integer format
      character*80 fname 
      real a,b,corr,rms,aa,bb             ! for lsq relation
      integer nf                          ! number of frequencies
      real q0(10000),alpha(10000)         ! q-relations
      integer nq(10000)                   ! number of points used for each relation
      real xx(10000),yy(10000)            ! for plotting
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      character*1 cha(10)                 ! dummy for xy_plot
      real x0
c  for automag
      real mag(10000),avmag               ! magnitudes
      real res(10000),avres               ! residuals
      real f0 (10000),avf0                ! corner f
      real st (10000), avst               ! stress drop
      real st_low,st_high                 ! stress drop range to use 
      integer min_fit                     ! min number fo fits to use in average

      st_low=1.0
      st_high=100.0

      call get_seisan_def

c
c   get input file name
c

      write(6,*)'File name, enter for automag_grid.out'
      read(5,'(a)') fname
      if(fname.eq.' ') fname='automag_grid.out'
      open(1,file=fname,err = 5,status='old') 
      goto 6
 5    continue
      write(6,*) ' Errror with file or file not there'
      write(6,*) ' Return to stop'
      read(5,'(a)') i
      stop
 6    continue
c
c   read file, first check which format
c
      
      read(1,'(a)') text     
      rewind 1
      format=0
      if(text(1:3).eq.'q0=') then
        format=1
c        write(6,*) 'Stress drop range, def 1-100=enter'
c        read(5,'(a)') text
c        if(text.ne.' ') read(text,*) st_low,st_high
      endif
      write(6,*) 'Min number of fits to use in average, enter for 1'
      read(5,'(a)') text
      min_fit=1
      if(text.ne.' ') read(text,*) min_fit

      n=1
      

 10   continue

      if(format.eq.1) then
         read(1,'(a)',end=20) text
         read(text,'
     *   (3x,f6.1,4x,f6.3,14x,i6,4x,f6.3,4x,f6.2,4x,f6.2,4x,f6.1)
     *   ',err=20) 
     *   q0(n),alpha(n),nq(n),res(n),mag(n),f0(n),st(n)
         write(6,'(a)') text(1:79)
      else
         read(1,*,end=20,err=20) q0(n),alpha(n),nq(n)
         write(6,*)'Q0,alpha,n',q0(n),alpha(n),nq(n)
      endif

c      write(6,*)'Q0,alpha,n',q0(n),alpha(n),nq(n)
      n=n+1
c
c  check for stress drop
c
c      if(st(n-1).lt.st_low.or.st(n-1).gt.st_high) n=n-1
c
c  check for min fits
c
       if(nq(n-1).lt.min_fit) n=n-1

      goto 10

 20   continue
      n=n-1
      write(6,*)' Number of curves to average: ',n

      if(n.lt.2) then
         write(6,*) ' Not enough points, return to stop'
         read(5,'(a)') n
         stop
      endif
c
c  calculate 1/q values to average, use fixed frequencies of 1,2,4,8,16
c
      x(1)=1.0
      x(2)=2.0
      x(3)=4.0
      x(4)=8.0
      x(5)=16.0

      nf=5

c---------------------------------------------------------
c   loop for running average
c---------------------------------------------------------

      write(6,*)
     *'Running average over how many, enter for average of all?'
      read(5,'(a)')text
      if(text.ne.' ') then
         read(text,*) naverage
      else
         naverage=n
      endif

      kstart=1

c
c   loop for running average starts here
c
 50   continue

      x(1)=1.0
      x(2)=2.0
      x(3)=4.0
      x(4)=8.0
      x(5)=16.0

      if(kstart+naverage-1.gt.n) goto 500

      ntotal=0

      do i=1,nf
         y(i)=0
         avmag=0.0
         avres=0.0
         avf0=0.0
         avst=0
         do k=kstart,kstart+naverage-1
            y(i)=nq(k)/(q0(k)*x(i)**alpha(k))+y(i)
            ntotal=ntotal+nq(k)
            avmag=avmag+mag(k)
            avf0=avf0+f0(k)
            avres=avres+res(k)
            avst=avst+st(k)
         enddo
      enddo
      avmag=avmag/naverage
      avres=avres/naverage
      avf0=avf0/naverage
      avst=avst/naverage
      ntotal=ntotal/nf
c      write(6,*) ' Total number of observations',ntotal

      do i=1,nf
c        write(6,*) x(i),y(i)
        y(i)=y(i)/ntotal
        y(i)=alog10(1.0/y(i))
        x(i)=alog10(x(i))
      enddo
     


c      close(1)


c
c   find least squares relation
c
c      write(6,*)(x(i),y(i),i=1,nf)

      call lsqlin(nf,x,y,a,b,corr,rms)

c
c  save first average for plot
c 
      if(kstart.eq.1) then
         aa=a
         bb=b
      endif

      a=10.0**a

     
      if(format.eq.1) then
         write(6,'(a,f6.1,a,f6.2,a,f6.3,a,f6.2,a,f6.2,a,f6.1)')
     *'Q0= ',a,' qalpha=',b,' res=',avres,' mw=',avmag,' f0=',avf0,
     *' avst=',avst
      else
         write(6,*) 'Q0,alpha,corr', a,b,corr
      endif
      kstart=kstart+1
      goto 50

 500  continue


c
c   plot first runing average of first group and the average
c
c
          write(6,*) 'Start plot'
c
c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='avq.eps',status='unknown')  ! open postscript output file
          plotunit=65                               ! use unit 65 for output
          plotoption=1                              ! plot ps
          wsize=70                                  ! use 60 % of screen
c          call get_window_size
c          if(size_lsq.gt.0) wsize=size_lsq ! from color.def
c

c
c set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'

c   open plotter
c
          call open_display()
c
c   plot coordinate system with seisan routine, next for scaling
c
             xx(1)=0.0
             xx(2)=1.5
             yy(1)=1.0
             yy(2)=4.0
             title=' '
             title(1:41)='Q vs frequency, red is average relation '
             title(42:45)='Q0= '
             write(title(46:51),'(f6.1)')10.0**aa
             title(52:61)='  Qalpha='
             write(title(62:67),'(f6.2)') bb
c            write(6,*) title
             xtext='Frequency '
             ytext='Log Q '
             k=2
             x0=-100.0
             call xy_plot
     *       (1,k,xx,yy,title,xtext,ytext,
     *       600.0,600.0,x0,100.0,1,1,0.01,
     *       0,cha,i,xx,yy)
c
c   plot lines with seisan routine given equation for line and size of coordinate system
c
          
          do i=1,naverage
             a=alog10(q0(i))
             b=alpha(i)
             call xy_plot_line(b,a,100.0,100.0)
          enddo
c
c   plot average
c
             call xset_color(xred)
     
             a=alog10(q0(i))
             b=alpha(i)
             call xy_plot_line(bb,aa,100.0,100.0)
            
             call xset_color(xblack)      
             text='Push any key to stop'
             call  tchars(text,20,750.0,700.0)   ! put command on plot
c
c   call up cursxor so plots remains and wait for input from keyboard
c	 
495          continue
             call xscursr(i,xx(1),yy(1))
             if (char(i).eq.'#') goto 495

c
c   close postscript output
c
             call close_post
c
c   close output plot file
c 
             close(65)
c
c   close display and back to alpha screen
c
           call clear_to_alpha
           write(6,*) 'Output of plot in avq.eps'
      stop
      end

    
		

C***********************************************************************
C                                                                    
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
c-----------------------------------------------------------------------------
