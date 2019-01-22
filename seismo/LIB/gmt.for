cccccccccccccccccccccccccccccccccccccc
c
c   changes
c
c  june 24 2003 jh : put in gmt_scaling
c


      subroutine gmt_xy_par(unit,set,npoint,x,y)
c
c   write out parameters and data to unit for gmt xy routine
c   if set is 1, main parameters, if set is differnet from 1,
c   trace parameters. the number of points is npoint and
c   the values are stored in x and y
c
c
      implicit none
      include 'gmt_xy_par.inc'
      integer unit,set
      integer npoint
      real x(*), y(*)
      character*80 text   ! output string
      integer i           ! counter
      real x1             ! help variable
c
c   find max and min of x-y values
c
      gmt_maxx=-1.0e30
      gmt_maxy=gmt_maxx
      gmt_minx=1.0e30
      gmt_miny=gmt_minx
c
      do i=1,npoint
         if(y(i).gt.gmt_maxy) gmt_maxy=y(i)
         if(y(i).lt.gmt_miny) gmt_miny=y(i)
         if(x(i).gt.gmt_maxx) gmt_maxx=x(i)
         if(x(i).lt.gmt_minx) gmt_minx=x(i)
      enddo
c
c   if scaling to fit inside borders, do it here
c
      if(gmt_scaling.ne.1.0) then
         x1=(gmt_scaling-1.0)*(gmt_maxx-gmt_minx)
         gmt_maxx=gmt_maxx+x1
         gmt_minx=gmt_minx-x1
         x1=(gmt_scaling-1.0)*(gmt_maxy-gmt_miny)
         gmt_maxy=gmt_maxy+x1
         gmt_miny=gmt_miny-x1
      endif
c
      text=' '
      text(1:1)='#'
c
c   main headers
c
      if(set.eq.1) then

         text(2:80)=' '
         text(3:11)='OFILENAME'
         text(15:80)=gmt_ofilename(1:66)
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='MAINTITLE'
         text(15:80)=gmt_maintitle(1:66)
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:16)='MAIN_FONT_SIZE'
         write(text(25:34),'(i10)') gmt_main_font_size
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:16)='MAIN_FONT_TYPE'
         text(18:80)=gmt_main_font_type(1:63)
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='XTITLE   '
         text(15:80)=gmt_xtitle(1:66)
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='XSIZE    '
         write(text(25:34),'(f10.2)') gmt_xsize
         text(35:80)=' '
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='XLOG     '
         write(text(25:34),'(i10)') gmt_xlog
         text(35:80)=' '         
         write(unit,'(a)') text
     
         text(2:80)=' '
         text(3:11)='YTITLE   '
         text(15:80)=gmt_Ytitle(1:66)
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='YSIZE    '
         write(text(25:34),'(f10.2)') gmt_ysize
         text(35:80)=' '
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='YLOG     '
         write(text(25:34),'(i10)') gmt_ylog
         text(35:80)=' '         
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='GRID_X   '
         write(text(25:34),'(i10)') gmt_grid_x   
         text(35:80)=' '         

         write(unit,'(a)') text
         text(2:80)=' '
         text(3:11)='GRID_Y   '
         write(text(25:34),'(i10)') gmt_grid_y   
         text(35:80)=' '         
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='NTRACES  '
         write(text(25:34),'(i10)') gmt_ntraces
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:12)='LEGEND    '
         write(text(23:34),'(i12)') gmt_legend         
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:12)='BAR_WIDTH '
         write(text(23:34),'(f12.5)') gmt_bar_width      
         write(unit,'(a)') text
      else
c
c   trace
c
         write(unit,*)

         text(2:80)=' '
         text(3:12)='TRACETITLE'
         text(15:80)=gmt_tracetitle(1:66)
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:12)='TRACECOLOR'
         text(15:80)=gmt_tracecolor(1:66)
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:13)='TRACESYMBOL'
         text(15:15)=gmt_tracesymbol       
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:13)='SYMBOL_SIZE'
         write(text(25:34),'(f10.2)') gmt_symbol_size         
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:12)='LINE_WIDTH'
         write(text(23:34),'(i12)') gmt_line_width         
         write(unit,'(a)') text


         text(2:80)=' '
         text(3:13)='LINESTYLE  '
         text(15:20)=gmt_linestyle      
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:13)='MINX      '
         write(text(15:34),'(g20.8)') gmt_minx
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='MAXX    '
         write(text(15:34),'(g20.8)') gmt_maxx
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='MINY    '
         write(text(15:34),'(g20.8)') gmt_miny
         write(unit,'(a)') text

         text(2:80)=' '
         text(3:11)='MAXY    '
         write(text(15:34),'(g20.8)') gmt_maxy
         write(unit,'(a)') text
 
         do i=1,npoint
            write(unit,*) x(i),y(i)
         enddo
      endif
      return
      end

  
      subroutine gmt_xy_init

      implicit none
      include 'gmt_xy_par.inc'
c
c   set default values for gmt_xy_par routine
c

c   set some defaults main block
c
      data gmt_ofilename /'gmtxy.out'/
      data gmt_maintitle /' '/
      data gmt_main_font_type /'Times-Roman'/
      data gmt_main_font_size /18/
      data gmt_xtitle /' '/
      data gmt_xsize /12.0/
      data gmt_xlog  /0/
      data gmt_ytitle /' '/
      data gmt_ysize /12.0/
      data gmt_ylog /0/
      data gmt_legend /1/
      data gmt_ntraces /1/
      data gmt_grid_x /1/
      data gmt_grid_y /1/
      data gmt_bar_width /1.0/

c
c   set defaults for trace block
c
      data gmt_tracetitle /' '/
      data gmt_tracecolor/'black'/
      data gmt_tracesymbol /' '/
      data gmt_symbol_size /0.6/
      data gmt_ncolor /8/
      data gmt_linestyle /'solid'/
      data gmt_line_width /5/
      data gmt_color(1)	/'black    '/
      data gmt_color(2)	/'blue     '/      
      data gmt_color(3)	/'purple   '/      
      data gmt_color(4)	/'red      '/
      data gmt_color(5)	/'green    '/
      data gmt_color(6)	/'orange   '/      
      data gmt_color(7)	/'yellow   '/      
      data gmt_color(8)	/'pink     '/
      data gmt_scaling  /1.0/

      return
      end
