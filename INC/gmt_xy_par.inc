c
c   include file for gmt parameters for xy-plot
c
c 
c   changes
c
c    may 6 2002 jh : add grid_x, grid_y, bar_width,  remove gridlines
c
c   main body of output for all traces
c
      character*80      gmt_ofilename   ! output file name
      character*80      gmt_maintitle   ! main title
      character*80	gmt_main_font_type ! font type for main title
      integer		gmt_main_font_size ! font size for main title
      character*80      gmt_xtitle      ! x-axis title
      real              gmt_xsize       ! size of x-axis in cm
      integer           gmt_xlog        ! log or lin (1 or 0)
      character*80      gmt_ytitle      ! y-axis title
      real              gmt_ysize       ! size of y-axis in cm
      integer           gmt_ylog        ! log or lin (1 or 0)
      integer 		gmt_legend	! 1: trace title, 0: no trace title
      integer           gmt_ntraces     ! number of traces
      integer		gmt_grid_x      ! x: direction 0; no lines, 1: lines
      integer		gmt_grid_y      ! y: direction 0; no lines, 1: lines
      real 		gmt_bar_width	! width of bars

c
c   for each trace
c
      character*80      gmt_tracetitle  ! title for each trace
      character*80      gmt_tracecolor  ! color of trace
      character*1       gmt_tracesymbol ! c(ircle),a(square),t(triangel),d(iamond)
      real		gmt_symbol_size ! symbol size in cm      
      character*6       gmt_linestyle   ! dashed, dotted, solid
      integer 		gmt_line_width  ! width of line
      real              gmt_maxx        ! maximum x
      real              gmt_minx        ! minimum x
      real              gmt_maxy        ! maximum y
      real              gmt_miny        ! minimum y
      character*10	gmt_color(50)   ! colors
      integer 		gmt_ncolor      ! number of colors currently pre defined
      real              gmt_scaling     ! if 1.0, true size, if larger, plot inside
c                                         intended to make all symbols fit inside frame
c                                         use e.g. a value of 1.1
c
c   common block
c
      common /gmt_xy1/  gmt_tracesymbol
      common /gmt_xy4/  gmt_xsize,gmt_ysize,gmt_xlog,gmt_ylog,
     *                  gmt_ntraces, gmt_minx,gmt_maxx,gmt_miny,
     *                  gmt_maxy,gmt_main_font_size,gmt_legend,
     *                  gmt_symbol_size,gmt_line_width,
     *                  gmt_ncolor,gmt_grid_x,gmt_grid_y,
     *                  gmt_bar_width,gmt_scaling
      common /gmt_xy6/  gmt_linestyle
      common /gmt_xy10/ gmt_color
      common /gmt_xy80/ gmt_ofilename,gmt_maintitle,gmt_xtitle,
     *                  gmt_ytitle,gmt_tracetitle,gmt_tracecolor,
     *                  gmt_main_font_type

