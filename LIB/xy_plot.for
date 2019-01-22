      subroutine xy_plot
     *(np,n,x,y,title,xtext,ytext,xlength,ylength,x0,y0,draw,
     *symbol,symbol_size,
     *xy_cursor,cha,nc,cx,cy)
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
c   find axis divisions, assume max 10 tics with number
c
       xdiv=(xmax-xmin)/10.0
       ydiv=(ymax-ymin)/10.0
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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine xy_plot_line(a,b,x0,y0)
c
c  plots the line with constants y = ax + b
c  all scaling values from routine xy_plot via common
c  x0,y0 are lower left hand corner of plot
c
       implicit none
       real x1,x2,y1,y2   ! line endpoints
       real a,b
       real xfirst,yfirst,x0,y0,xscale,yscale,ymin,ymax,xmin,xmax   ! see xy_plot
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax

c      open(27,file='xy.tst',status='unknown')
c	  write(27,*) a,b
c
c   calculate two endpoints
c
      x1=xfirst
      y1=a*x1+b
      if(y1.gt.ymax) then
          y1=ymax
          x1=(y1-b)/a
      endif
      if(y1.lt.ymin) then
          y1=ymin
          x1=(y1-b)/a
      endif
c
      x2=xmax
      y2=a*x2+b
      if(y2.gt.ymax) then
          y2=ymax
          x2=(y2-b)/a
      endif
      if(y2.lt.ymin) then
          y2=ymin
          x2=(y2-b)/a
      endif
c	  write(27,*)'x,y',x1,y1,x2,y2
c
c   plot line
c
      call xmovabs((x1-xfirst)*xscale+x0,(y1-yfirst)*yscale+y0)
      call xdrwabs((x2-xfirst)*xscale+x0,(y2-yfirst)*yscale+y0)
c
      return
      end
