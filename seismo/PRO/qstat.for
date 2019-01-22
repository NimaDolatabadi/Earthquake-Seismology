c--------------------------------------------------------------------------
c  plot a histigram of Q-values from codaq as well as Q vs distance
C  use codaq.are file as input
c  simple plots for initial inspection
c  also make an output file with code and coordinates of all stations used
c--------------------------------------------------------------------------
c
c  jh july 2015
c
c  updates
c  
c  30 11 2017 jh: new format for codaq.area with sd of Q
c
      implicit none                       ! force delcaration of all variables

      include 'seiplot.inc'               ! seisan graphics
      include 'version.inc'
      include 'seisan.inc'
 

      character*1 cha(10)                 ! dummy for xy_plot
      character*80 text                   ! general text

      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      real xc(10),yc(10)
 

      character*5 stat(90000)             ! station
      real slat(90000),slon(15000),elev(15000) ! station coordinates and elevation
      character*1 file_indicator          ! stationx.hyp indicator
      character*80 infile                 ! input file
      character*10 stacom                 ! station component
      real f(90000),q(15000)              ! frequency and q
      real depth(90000),edist(15000),hdist(15000) ! depth, epi dist, hyp dist  
      integer nf                          ! number of frequencies
      real ff(300)                        ! frequencies
      real qf(90000)                      ! q-values at a given frequency
      real qdist(90000)                   ! distances for this frequency
      real qhist(30)                      ! histogram boundaries
      integer nqhist(30)                  ! number in each
      integer kqhist                      ! number of boundaries
      real qdelta(30)                     ! q-increment for histogram
      real x(30),y(30),xx(120),yy(120)    ! help arrays
      real x0                             ! origin point
      real xsize                          ! size of each plot
 
      integer kstat                       ! number of stations

      integer code                        ! error return code

      real dist(90000)                    ! distance
     
      integer i,ii,l,k,n,m,kk,kkk(1)      ! counter


      call get_seisan_def

      n=0
      do i=1,300
         ff(i)=0.0
      enddo
      file_indicator=' '  ! assume STATION0.HYP
c
c   steps in histogram for different frequencies
c   should be changed for different data sets
c
      qdelta(1)=20.0
      qdelta(2)=50.0
      qdelta(3)=90.0
      qdelta(4)=125.0
      qdelta(5)=220.0
      qdelta(6)=250.0
   

      kqhist=10   ! number of intervals for histogram   

      kstat=0
c
c   get input file
c
      write(6,*) 'Input file, default is codaq.area'
      read(5,'(a)') infile
      if(infile.eq.' ') infile='codaq.area'

      open(1,file=infile,status='old',err=10)
      goto 11     

 10   continue
      write(6,*)' No such input file'
      stop

 11   continue

      open(3,file='qstat.stat',status='unknown')

c
c   read file and store parameters, also make a list of stations
c
 12   continue
      n=n+1
      read(1,'(15x,a10,14x,2f7.0,7x,3f7.0)',end=15) 
     *stacom,f(n),q(n),depth(n),edist(n),hdist(n)

      do i=1,kstat
        if(stat(i).eq.stacom(1:5)) goto 12
      enddo
      kstat=kstat+1
      stat(kstat)=stacom(1:5)           
      goto 11

 15   continue


      n=n-1
      write(6,*)'Number of Q-values ',n

      write(6,*)'Number of stations ',kstat
c
c   get coordiantes and write out
c
 16   continue
      k=0
      call  stat_loc_many(stat,kstat,file_indicator,slat,slon,elev,k)   
c
c   check if station file was there
c
      if(k.eq.1) then         
         write(6,*)'Maybe file indicator is not 0, give a new one'
         write(6,*)'Skip getting station coordintes, enter'
         read(5,'(a)') file_indicator
         if(file_indicator.eq.' ') goto 17
         goto 16
      endif



 17   continue

c
c   find which frequencies in data set
c
      nf=0
      do i=1,n
         k=f(i)*10  ! assume frequencies only to one decimal point
         ff(k)=-1.0
      enddo

      do i=1,300
         if(ff(i).ne.0.0) then
            nf=nf+1
            ff(nf)=i/10.0
         endif
      enddo   

      write(6,'(a,10f5.1)')' Frequencies in data set: ',(ff(i),i=1,nf)


      if(nf.gt.6) then
         write(6,*)'This program will only analyze for 6 frequencies',
     *   ', dat set reduced to 6'
         nf=6
      endif

c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='qstat.eps',status='unknown')
          plotunit=65
          plotoption=1
          wsize=60
          call get_window_size

c
c   open plotter
c
c
c   set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'
c
c   open display
c
	call open_display 

c
c--------------------------------------------------------------
c  find all q-values for each frequency, big loop for plotting
c--------------------------------------------------------------
c

      do kk=1,nf
c
c   zero array for histogram
c
         do i=1,kqhist
           nqhist(i)=0
         enddo
c
c  intervals for this frequency
c
         do i=1,kqhist
            qhist(i)=qdelta(kk)*i 
         enddo
         l=0
c
c   save values for this frequency 
c
         do m=1,n
            if(f(m).eq.ff(kk)) then
              l=l+1
              qf(l)=q(m)
              qdist(l)=edist(m)
            endif
         enddo
c
c   find values for histogram
c
        do i=1,l
           do ii=1,kqhist             
              if(qf(i).ge.(ii*qdelta(kk)-qdelta(kk)/2.0).and.
     *           qf(i).lt.(ii*qdelta(kk)+qdelta(kk)/2.0)) then
                 nqhist(ii)=nqhist(ii)+1
              endif
           enddo
        enddo

c        do i=1,kqhist
c           write(6,*) qhist(i),nqhist(i)
c        enddo        

c
c  transfer data to x and y
c
           do i=1,kqhist
              x(i)=qhist(i)
              y(i)=nqhist(i)
           enddo
c
c   generate points of histogram
c
             k=1
             xsize=155.0
c
c   distance between columns
c

             do i=1,kqhist-1
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
             xx(k)=x(kqhist)
             yy(k)=y(kqhist)
             k=k+1
             xx(k)=x(kqhist)+qdelta(kk)
             yy(k)=y(kqhist)
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
               xx(i)=xx(i)-qdelta(kk)/2.0
             enddo

c   plot coordinate system and points with seisan routine
c
             write(title,'(f4.1)')ff(kk)
             ytext=''
             xtext=''
             cha='2'  ! no number
 
              
             x0=xsize*(kk-1)+20
             kkk(1)=k
             call xy_plot_qstat
     *       (1,kkk,xx,yy,title,xtext,ytext,
     *       xsize,xsize,x0,100.0,2,1,5.0,
     *       0,cha,i,xc,yc)

c
c   now plot q vs distance
c          
          
             ytext=''
             xtext=''
             cha='2'  ! no number
              
             x0=xsize*(kk-1)+20
             kkk(1)=l
             call xy_plot
     *       (1,kkk,qdist,qf,title,xtext,ytext,
     *       xsize,xsize,x0,400.0,1,1,5.0,
     *       0,cha,i,xc,yc)

             text='Coda Q vs distance'
             call  tchars(text,18,50.0,650.0) 

             text='Coda Q statistics'
             call  tchars(text,17,50.0,330.0) 
 
 
 30       continue


        enddo

        text='Push any key to stop'
        call  tchars(text,20,750.0,700.0)   ! put command on plot

        call xscursr(i,xx(1),yy(1))
c
c   write out station file
c
        do i=1,kstat
           write(3,'(a5,3f9.3)') stat(i),slat(i),slon(i),elev(i)
           if(slat(i).eq.0.0.and.slon(i).eq.0.0) write(6,*) 
     *     'Station not found ',stat(i)        
        enddo

        write(6,*) 'Plot file in qstat.eps'
        write(6,*) 'Station in qstat.stat'


      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine xy_plot_qstat
     *(np,n,x,y,title,xtext,ytext,xlength,ylength,x0,y0,draw,
     *symbol,symbol_size,
     *xy_cursor,cha,nc,cx,cy)

c
c  fix some numbers for qstat, look for qstat
c
c   makes an x-y plot, optionally calls up cursor and returns x and
c   y coordinaes and key pushed. A q, Q, f, F, R, r, s or S
c   terminates cursor operation. 
c   Coordinates returned are in same units as input.
c
c   
c
c   j havskov, jun 93 
c   feb 95: put axis text
c   jan 95: small adjsutment x-axis numbering space
c   jan 23, 96: bugs when  using option same scale for next plot
c   jan 31    : add exit on s
c   feb  7    : small change
c   feb 13    : increase number field from 5 to 7
c   jul 24    : bug with number of digits
c   feb 24 97 : more patching with xlin=0 for linear numbers
c   mar 14 97 : plotting of horizontal and vertical lines
c   jun 10 98 : bug in format write(text(1:5),'(i7)') int(xnumber), mario v.
c   sep 98 jh : ----------------   version 7.0 chaeck -----------------
c               no changes
c   mar 1 99  : move all numbers on x-axis 20 to the right
c   may 3 00jh: fix so extra data sets only plots points inside if scaled
c               by main data set
c   apr 03 lo : dont quit when replot given
c   apr 8 10 jh: add comment about negative draw
c   dec 27 10 jh: remove call to tsend
c   oct 7 13 jh: color for different plots, option for no numbers on axes
c   2014-02-21 pv: fixed Warning: Deleted feature: End expression in Do loop must be integer
c   2014-09-03 jh: plot 80 in title instead of 60
c
c     input:      np: number of data sets
c                 n: number of points in each data set
c                 x,y: x and y points in all data sets in one vector
c                 title: plot title
c                 xtext,ytext : axis text, 30 chars
c                 xlength,ylength: axis lengths in tek units
c                                  if xlength zero, only plot data, old scale
c                                  however, data outside plot are cut down to
c                                  fit plot, WILL BE MODIFIED ON RETURN
c                 x0,y0: positon of lower left hand corner, tek units, if
c                        x0 is negative, antilog is taken of x-values
c                 draw: 1: symbols only
c                       2: lines only
c                       3: symbols and connecting lines
c                       if draw is negative, smae scale on x and y
c                 symbol: symbol code, 1,2 etc, see routine for symbols
c                 symbol_size: size in tek units of symbol
c                 xy_cursor: 0: do not call up cursor
c                         1: call cursor
c                         2: ----------- and mark positions with symbol
c                         3: --------------- connect with lines
c                         4: ------------------symbol and connecting lines
c                 cha: 1: no number x, 2: no number y, 3: no number both axes
c                 cx(1-np): colors of plots, real numbers
c
c     output:      cha:   characters input
c                  nc :   number of positions
c                  cx,cy; x-y positions
c
c
       implicit none
       real x(*),y(*)
       character*1 cha(*)
       real cx(*),cy(*),symbol_size
       integer symbol,xy_cursor
       integer n(*),np,draw,nc
       character*80 title
       character*30 xtext,ytext
c--length of axis in tek units
       real xlength,ylength
c--scaling factors
       real xscale,yscale
c--max and min values
       real xmax,ymax,xmin,ymin
       real xmax1,ymax1,xmin1,ymin1
       real xin,yin
c--position of 0,0 of frame
       real x0,y0
c--x and y axix divisions
       real xdiv,ydiv
       integer xlin
c--exponent of xdiv and ydiv
       integer ixdiv,iydiv
c--permitted numbers on axix
       real anumber(10)
c--values at first axis tics
       real xfirst,yfirst
c--help variables
       character*80 text
       logical last_point_in,move_to      ! for plotting inside and outside
       real xd,yd,xnumber,ynumber,x1,y1,xold,yold
       integer i,j,ichar,nread,nn,k
c
c   put some scaling values in a common block so other routines can use
c   them
c
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax
       data anumber/0.01,0.02,0.05,0.1,0.2,0.5,1.0,2.0,5.0,10.0/
c
c   log lin
c
       xlin=1   ! as is
       if(x0.lt.0) then
         x0=-x0
         xlin=0
       endif
c
c   check if only plotting data with old scale and position
c
       if(xlength.eq.0.0) goto 10
c
       xmax=-10e20
       ymax=-10e20
       xmin=10e20
       ymin=10e20
       nc=0
c
c   find max and min values, use all data sets, nn is total number of points
c
       nn=0
       do i=1,np
         nn=nn+n(i)
       enddo
c
       do i=1,nn
         if(x(i).gt.xmax) xmax=x(i)
         if(y(i).gt.ymax) ymax=y(i)
         if(x(i).lt.xmin) xmin=x(i)
         if(y(i).lt.ymin) ymin=y(i)
       enddo
c
c if draw <0 use same max scaling for both x and y, lot 14/02/2008
c
       if (draw.le.0) then
         draw=-draw
         if (xmax-xmin.gt.ymax-ymin) then
           ymax=xmax
           ymin=xmin
         else
           xmax=ymax
           xmin=ymin
         endif
       endif
       
c
c   ensure plotting of horizontal and vertical lines
c
       if((xmax-xmin).eq.0) then
         xmax=xmax*1.1
         if(xmax.eq.0.0) xmax=0.1
       endif
       if((ymax-ymin).eq.0) then
         ymax=ymax*1.1
         if(ymax.eq.0.0) ymax=0.1
       endif
       
c
c   add 5% to make plot look nicer
c
       xmax1=xmax+(xmax-xmin)*.05
       xmin1=xmin-(xmax-xmin)*.05
       ymax1=ymax+(ymax-ymin)*.05
       ymin1=ymin-(ymax-ymin)*.05
       xmax=xmax1
       xmin=xmin1
       ymax=ymax1
       ymin=ymin1
c
c   slighly larger limits for prescaled data sets
c
       xmax1=xmax+(xmax-xmin)*.02
       xmin1=xmin-(xmax-xmin)*.02
       ymax1=ymax+(ymax-ymin)*.02
       ymin1=ymin-(ymax-ymin)*.02
c
c   qstat fix
c
       xmin=0.0
       ymin=0.0

c
c   find axis divisions, assume max 10 tics with number
c

c     qstat fix 4

       xdiv=(xmax-xmin)/4
       ydiv=(ymax-ymin)/4
c
c  could be that xdiv is too small
c
       if(xdiv*xscale.lt.80.0)
     * xdiv=(xmax-xmin)/(xlength/60.0)
       
c
c   find exponent of div for scaling
c
       ixdiv=alog10(xdiv)
       if(ixdiv.ge.0) ixdiv=ixdiv+1
       if(ixdiv.lt.0) ixdiv=ixdiv-1
       iydiv=alog10(ydiv)
       if(iydiv.ge.0) iydiv=iydiv+1
       if(iydiv.lt.0) iydiv=iydiv-1
       do i=1,9
          if(xdiv/10.0**ixdiv.gt.anumber(i)) 
     *       xd=anumber(i+1)*10.0**ixdiv
          if(ydiv/10.0**iydiv.gt.anumber(i)) 
     *       yd=anumber(i+1)*10.0**iydiv
       enddo
       xdiv=xd
       ydiv=yd
c
c   find axis number at lower left hand corner
c
       xfirst=(int(xmin/xdiv))*xdiv
       if(xmin.lt.0) xfirst=xfirst-xdiv
       yfirst=(int(ymin/ydiv))*ydiv
       if(ymin.lt.0) yfirst=yfirst-ydiv
c
c   scaling factors
c
       xscale=xlength/(xmax-xfirst)
       yscale=ylength/(ymax-yfirst)
c
c   plot frame
c
       call xmovabs(x0,y0)
       call xdrwabs(x0,y0+ylength)
       call xdrwabs(x0+xlength,y0+ylength)
       call xdrwabs(x0+xlength,y0)
       call xdrwabs(x0,y0)
c
c   plot x-axis text
c
       call tchars(xtext,30,x0+50.0,y0-60.0)
       call pchars(xtext,30,x0+50.0,y0-40.0)
c
c   plot axis tics with number, first x axis
c
c pv:       do i=1,(xmax-xfirst)/xdiv
       do i=1,int((xmax-xfirst)/xdiv)
         xd=x0+i*xdiv*xscale-50.0
         yd=y0-40.0
         xnumber=xdiv*i+xfirst
c
c   if number too large, print only number without exponent
c
         if(abs(ixdiv).gt.6) then 
            xnumber=xnumber/10.0**ixdiv
            write(text(1:7),'(i7)') int(xnumber)
         else
            if(xlin.eq.0) xnumber=10**xnumber    ! patch to write log numbers
            if(xdiv.ge.1.0) write(text(1:7),'(i7)') int(xnumber)
            if(xdiv.lt.1.0) write(text(1:7),'(f7.1)') xnumber
            if(xdiv.lt.0.1) write(text(1:7),'(f7.2)') xnumber
            if(xdiv.lt.1.0.and.xlin.eq.0) 
     *      write(text(1:7),'(f7.1)') xnumber
            if(xnumber.lt.1.0.and.xnumber.ge.0.001.and.xlin.eq.0) 
     *      write(text(1:7),'(f7.3)') xnumber
            if(xnumber.lt.0.1.and.xnumber.ge.0.0001.and.xlin.eq.0) 
     *      write(text(1:7),'(f7.4)') xnumber
	        
         endif

c
c   do not plot numbers
c
         if(cha(1).eq.'1'.or.cha(1).eq.'3') text=' '

         call tchars(text,7,xd,yd)
         call pchars(text,7,xd+25,yd+20)
         call xmovabs(x0+i*xdiv*xscale,y0)
         call xdrwabs(x0+i*xdiv*xscale,y0+10)
       enddo
       if(abs(ixdiv).gt.6) then
          write(text(1:4),'(a,i3)')'E',ixdiv
          call xchars(text,4,x0+xlength,y0-40.0)
       endif
c
c   plot y-axis text
c
       yd=y0+ylength+13.0
       do i=1,30
         yd=yd-13.0
         call xchars(ytext(i:i),1,x0-90.0,yd)
       enddo
c
c   plot y axis
c
c pv:  do i=1,(ymax-yfirst)/ydiv
       do i=1,int((ymax-yfirst)/ydiv)
         xd=x0-95.0
         yd=y0+i*ydiv*yscale-5.0
         ynumber=ydiv*i+yfirst

         if(abs(iydiv).gt.6) then 
            ynumber=ynumber/10.0**iydiv
            write(text(1:7),'(i7)') int(ynumber)
         else
            if(ydiv.ge.1.0) write(text(1:7),'(i7)') int(ynumber)
            if(ydiv.lt.1.0) write(text(1:7),'(f7.1)') ynumber
            if(ydiv.lt.0.1) write(text(1:7),'(f7.2)') ynumber
         endif

c
c   do not plot numbers
c
         if(cha(1).eq.'2'.or.cha(1).eq.'3') text=' '

         call xchars(text,7,xd,yd)
         call xmovabs(x0,y0+i*ydiv*yscale)
         call xdrwabs(x0+10.0,y0+i*ydiv*yscale)
       enddo
       if(abs(iydiv).gt.6) then
          write(text(1:4),'(a,i3)')'E',iydiv

c
c   do not plot numbers
c
         if(cha(1).eq.'2'.or.cha(1).eq.'3') text=' '

          call xchars(text,4,x0-80.0,y0+ylength+10)
       endif
c
c   plot title
c
       call xchars(title,80,x0,y0+ylength+23)
c
c   plot data points, enter here if only draw a new data set with
c   previous scaling factors
c
 10    continue
       i=0      ! counter for total number of points plotted
       do k=1,np   ! loop over data sets
cjh
c
c   set color
c
          nn=cx(k)
          if(nn.eq.1.or.nn.eq.2.or.nn.eq.3.or.nn.
     *    eq.4) then
             call xset_color(nn)
          endif

          last_point_in=.false.                ! to force move to first point
c
c  if another data set with no scaling check, only plot numbers inside
c

          do j=1,n(k)  ! loop over each set
            i=i+1
            if(xlength.eq.0.0) then           ! case of no scaling
               if(x(i).ge.xmin1.and.x(i).le.xmax1.and.  ! check if inside
     *         y(i).ge.ymin1.and.y(i).le.ymax1) then 
                  if(last_point_in) then
                     move_to=.false.  ! point inside, prev. point inside, draw
                  else
                     move_to=.true.   ! point inside, prev. outside, move to
cx                     last_point_in=.true.
                  endif
                  last_point_in=.true.
               else
                  last_point_in=.false.
                  move_to=.true.
               endif
            else                               ! case of point inside always 
               move_to=.false.                 ! always draw, except first point
               if(j.eq.1) move_to=.true.       ! always move to first point
            endif

            xin=x(i)
            yin=y(i)

            xd=(x(i)-xfirst)*xscale+x0
            yd=(y(i)-yfirst)*yscale+y0
            if(xd.gt.1023.0) xd=1023.0
            if(xd.lt.0.0) xd=1.0
            if(yd.gt.780.0) yd=780.0
            if(yd.lt.0.0) yd=1.0
            if(move_to) then             ! only move to point
               call xmovabs(xd,yd)
               if( xlength.ne.0.0) then  ! make symbol if regular data set
                  if(draw.eq.1.or.draw.eq.3) 
     *            call xsymbol(symbol,symbol_size,xd,yd)
               endif
            else                         ! draw to point
               if(draw.eq.2.or.draw.eq.3) call xdrwabs(xd,yd)                  
               if(draw.eq.1.or.draw.eq.3) 
     *         call xsymbol(symbol,symbol_size,xd,yd)
            endif
         enddo

c
c   back to black
c
         call xset_color(6)

      enddo
c
c   calls up cursor
c
      if(xy_cursor.gt.0) then
         nread=0
 50      continue
            call xscursr(ichar,x1,y1)
            nread=nread+1
            cha(nread)=char(ichar)
            cx(nread)=(x1-x0)/xscale+xfirst
            cy(nread)=(y1-y0)/yscale+yfirst
            if(nread.eq.1) then
               xold=x1
               yold=y1
            endif
c
c   do not draw if terminating character
c
            if(cha(nread).eq.'q'.or.cha(nread).eq.'Q'.or.
     *      cha(nread).eq.'f'.or.cha(nread).eq.'F'.or.
     *      cha(nread).eq.'r'.or.cha(nread).eq.'R'.or.
     *      cha(nread).eq.'S'.or.cha(nread).eq.'s') goto 60
c
c   possible drawing
c           
            if(xy_cursor.eq.3.or.xy_cursor.eq.4) 
     *        call xmovabs(xold,yold)
            if(xy_cursor.eq.3.or.xy_cursor.eq.4) 
     *        call xdrwabs(x1,y1)
			xold=x1
            yold=y1
            if(xy_cursor.eq.2.or.xy_cursor.eq.4) 
     *         call xsymbol(symbol,symbol_size,x1,y1)
            goto 50   ! next cursor input
      endif
 60   continue
      nc=nread
c
c   back to black
c
      call xset_color(6)
      return
      end                  
