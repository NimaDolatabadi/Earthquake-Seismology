c
c   simple program to make a least squres calculation between two data sets 
c   in a two column file and plot the relation
c
c   nov 98 by jh: ----------verison 7.09 check, no change -----------------
c   
c   mar 22 99 bmt: include winplot.inc 
c   jun 24 02 jh : include gmtxy
c   jun 20 05 jh : plt to eps
c   dec 13 05 jh : put in max likelihood
c   dec 28 10 jh : gfortran on pc. remove winplot, put in implicit none,
c                  unit check(16 to 26), computer check, remove hctype
c   feb 22 11 jh : read size from color.def
c   jan 02 14 jh : add plotting of a curve
c   feb 17 14 jh : read seisan.def
c   sep 04 14 jh : add column plot, no question if plot

      implicit none                      
      include 'gmt_xy_par.inc'            ! include block for gmt_xy routines

      include 'seiplot.inc'               ! seisan graphics
      include 'version.inc'               ! version

      real x(90000),y(90000)              ! xy values
      real xx(9000),yy(90000)             ! help variables
      real cx(10),cy(10)                  ! can be used for colors, also return
      real xm,ym                          ! help variables
      real x1,x2,y1,y2                    ! help varibales
      real x0,delta
      character*1 cha(10)                 ! dummy for xy_plot
      character*80 text                   ! general text
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      character*80 fname                  ! input file name 
      integer n,i,k 
      real a,b,corr,rms                   ! for lsq relation
      real sigmaa,sigmab                  ! errors
      integer reg                         ! which regression
      integer option                      ! different options for plotting
      character*1 answer 
      integer draw                        ! how to draw

c
c   print version
c
      out_version_date='June 24, 2003'
      if (version_new) out_version_date=version_date
      call print_ver

      call get_seisan_def
c
c   get input file name


      write(6,*)'File name'
      read(5,'(a)') fname
      open(1,file=fname,err = 5,status='old') 
      goto 6
 5    continue
      write(6,*) ' Errror with file or file not there'
      write(6,*) ' Return to stop'
      read(5,'(a)') i
      stop
 6    continue

      n=1
 10   continue
c
c   read file
c
      read(1,*,end=20,err=20) x(n),y(n)
      n=n+1
      goto 10
 20   continue
      n=n-1
      write(6,*)'Number of points: ',n
      write(6,*)
      if(n.lt.2) then
         write(6,*) ' Not enough points, return to stop'
         read(5,'(a)') n
         stop
      endif
      close(1)
c
c   get options
c
      write(6,*)'Option  1: line fit'
      write(6,*)'        2: make a curve'
      write(6,*)'        3: make a histogram'
      read(5,*) option

c
c   line fit
c
      if(option.eq.1) then
         write(6,*) 'Least squares (1) or maximum likelihood (2) ?'
         read(5,*) reg

         if(reg.eq.1) then
c
c   find least squares relation
c
            call lsqlin(n,x,y,a,b,corr,rms)
             write(6,'(a,f9.4,a,f9.4)')
     *      'Least squares relation: y = ',b,' * x + ',a
         else
c
c   find max likelihood
c
            call maxlik(x,y,n,a,b,sigmaa,sigmab)
             write(6,'(a,f9.4,a,f9.4)')
     *      'Maximum liklihood     : y = ',b,' * x + ',a
         endif
      endif
	 
		
c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='lsq.eps',status='unknown')  ! open postscript output file
          plotunit=65                               ! use unit 65 for output
          plotoption=1                              ! plot ps
          wsize=60                                  ! use 60 % of screen
          call get_window_size
          if(size_lsq.gt.0) wsize=size_lsq ! from color.def
c

c
c set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'
c
c   make titles
c
          if(reg.eq.1) then
             write(title,'(a,f7.3,a,f9.3)')
     *      'Least squares relation: y = ',b,' * x + ',a
          else
             write(title,'(a,f7.3,a,f9.3)')
     *      'Maximum liklihood     : y = ',b,' * x + ',a
          endif
          xtext='                   '                ! no title on x-axis
          ytext='                   '                ! no title on y-axis
c
c-----------------------------------------
c   linear fit
c-----------------------------------------
c
          if(option.eq.1) then 

c
c   open plotter
c
             call open_display
c
c   plot coordinate system and points with seisan routine
c
             call xy_plot
     *       (1,n,x,y,title,xtext,ytext,
     *       600.0,600.0,100.0,100.0,1,1,20.0,
     *       0,cha,i,cx,cy)
c
c   plot line with seisan routine given equation for line and size of coordinate system
c
             call xy_plot_line(b,a,100.0,100.0)
             text='Push any key to stop'
             call  tchars(text,20,750.0,700.0)   ! put command on plot
          endif
c
c--------------------------------------------
c   plot points or curve
c--------------------------------------------
c
          if(option.eq.2) then
             ytext='linear y'
             xtext='linear x'
             title='Standard x-y plot'
             x0=100.0

             write(6,*)'x-axis logaritmic(y/n=default)'
             read(5,'(a)')answer
             if(answer.eq.'y') then
                do i=1,n
                  if(x(i).le.0.0) then
                     write(6,*)'Zero or negative x'
                     stop
                  endif
                  x(i)=alog10(x(i))
                  xtext='log x'
                enddo
c                x0=-100.0  ! antilog for x-axis numbers
                 x0=-100.0  ! log on x-axis
             endif

             write(6,*)'y-axis logaritmic(y/n=default)'
             read(5,'(a)')answer
             if(answer.eq.'y') then
                do i=1,n
                  if(y(i).le.0.0) then
                     write(6,*)'Zero or negative y'
                     stop
                  endif
                  y(i)=alog10(y(i))
                  ytext='log y'
                enddo
             endif

 22          continue
             write(6,*)'Draw: 1: symbols only'
             write(6,*)'      2: lines only'
             write(6,*)'      3: symbols and connecting lines'
             read(5,*) draw

             if(draw.lt.1.or.draw.gt.3) goto 22
c
c   open plotter
c
             call open_display

c
c   plot coordinate system and points with seisan routine
c
             call xy_plot
     *       (1,n,x,y,title,xtext,ytext,
     *       600.0,600.0,x0,100.0,draw,1,20.0,
     *       0,cha,i,cx,cy)

         endif
c
c------------------------------------
c   histogram
c------------------------------------
c
         if(option.eq.3) then
c
c   generate points of histogram, assume input x is equidistant
c
             k=1
c
c   distance between columns
c
             delta=x(2)-x(1)
             do i=1,n-1
                xx(k)=x(i)
                yy(k)=0.0
                k=k+1
                xx(k)=x(i)
                yy(k)=y(i)
                k=k+1
                xx(k)=x(i+1)
                yy(k)=y(i)
                k=k+1
                xx(k)=x(i+1)
                yy(k)=0.0
                k=k+1
             enddo
             xx(k)=x(n)
             yy(k)=y(n)
             k=k+1
             xx(k)=x(n)+delta
             yy(k)=y(n)
             k=k+1
             xx(k)=xx(k-1)
             yy(k)=0.0
             k=k+1
             xx(k)=xx(1)
             yy(k)=yy(1)
c
c   shift all columns
c
             do i=1,k
               xx(i)=xx(i)-delta/2.0
             enddo

             write(6,*)'Give x-axis text'
             read(5,'(a)') xtext

c
c   open plotter
c
             call open_display

c
c   plot coordinate system and points with seisan routine
c
             title=' '
             ytext='Number '
             draw=2
             x0=100.0
             call xy_plot
     *       (1,k,xx,yy,title,xtext,ytext,
     *       600.0,600.0,x0,100.0,draw,1,20.0,
     *       0,cha,i,cx,cy)

             text='Push any key to stop'
             call  tchars(text,20,750.0,700.0)   ! put command on plot
          endif
c
c   call up cursxor so plots remains and wait for input from keyboard
c	 
495       continue
          call xscursr(i,xm,ym)
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
c
c----------------------------------------------------------
c   make gmt_xy file if option 1
c----------------------------------------------------------
c
      if(option.eq.1) then
         call gmt_xy_init                 ! set default gmt parameters
c
c   gmt main header settings
c
         gmt_xtitle='X-axis'
         gmt_ytitle='Y-axis'
         gmt_maintitle='Main title'       ! title
         gmt_xsize=12.0                   ! y-size in cm
         gmt_xlog=0                       ! linear
         gmt_ysize=10.0                   ! x-size in cm
         gmt_ylog=0                       ! linear
         gmt_ntraces=2                    ! two data sets, points and line

c
c  open gmt file and write out main parameters
c
         open(26,file='lsq_gmt.out',
     *   status='unknown')

         call gmt_xy_par(26,1,1,x,y)
c
c    set some parameters for first data set, the points
c
         gmt_linestyle=' '                ! do not join points
         gmt_tracesymbol='d'              ! diamond as a symbol
         gmt_scaling=1.1                  ! increase size of plot by 10 %
c
c   write out xy data for first data set
c
         call gmt_xy_par(26,0,n,x,y)
c
c   calculate two endpoints of line so within plot
c

         x1=gmt_minx
         y1=b*x1+a
         if(y1.gt.gmt_maxy) then
             y1=gmt_maxy
             x1=(y1-a)/b
         endif
         if(y1.lt.gmt_miny) then
             y1=gmt_miny
             x1=(y1-a)/b
         endif
c
         x2=gmt_maxx
         y2=b*x2+a
         if(y2.gt.gmt_maxy) then
             y2=gmt_maxy
             x2=(y2-a)/b
         endif
         if(y2.lt.gmt_miny) then
             y2=gmt_miny
             x2=(y2-a)/b
         endif
         x(1)=x1
         x(2)=x2
         y(1)=y1
         y(2)=y2
c
c    set some parameters for second data set1
c
         gmt_linestyle='solid'            ! join points
         gmt_tracesymbol=' '              ! no symbol for end points of line
         gmt_scaling=1.0                  ! normal size now              
         gmt_tracetitle='lsq line'        ! title for 2. trace
c
c   write out xy data, in this case just the line end points
c
         n=2
         call gmt_xy_par(26,0,n,x,y)
c
c   close gmtxy file
c
         close(26)
         write(6,*)' gmt_xy plot file is lsq_gmt.out'
      endif

      write(6,*)' Plot file is lsq.eps'
        
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
