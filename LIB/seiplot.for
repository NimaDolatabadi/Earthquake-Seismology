c                                                                 
c   updates
c   apr 2 91 by j.h. : bugs
c   oct              : adoption to sun , scursr form tektronics has been
c                      replaced by cursor, a c routine, in order to read
c                      one char form x-windows keyboard, a bug in the tek
c                      imulation that it cannot be set up to read without
c                      a CR. In order to make it work, system calls 
c                      stty cbreak must be issued, stty -cbreak to get
c                      keyboard back to normal. 
c    nov 13          : cursor is disabled again, gave some buffer 
c                      problems. Problem solved by using a new c-routine
c                      readn in inout.f, so to read one char from keyboard.
c    aug 29 92       : add xt and vt routines
c    jul 93          : add xy_plot
c    aug 93          : new symbol
c    aug 29          : pc adoption
c    sep 6 93        : put a tsend in xcursr to fix cursor problem on pc
c    nov  25 93 by jh: check for ( and ) in xchars, move title a bit in xy_plot
c    jul 94          : add X capability
CJAB(BGS)Jan95       : Add JAB's symbol generator.
CJAB(BGS)Jan95       : Install use of parameters in SEIPLOT.INC
c mar 29, 95 by jh   : none option in input_box
c apr 11 95 by jh    : put in %! in start of poscript file, where did it go ???
c may 95             : new routines pmovabs and pdrwabs, change to xdrwvec
c                      change initial postscript statement
C!JAB(BGS)Jun95      : Problem when PostScript element formats are filled, so
C!JAB(BGS)Jun95        removing white spaces so that 2 elements required to
C!JAB(BGS)Jun95        operated on, merge into 1, so confusing PostScript.
C!JAB(BGS)Jun95        ...change all formats to ensure a white space between
C!JAB(BGS)Jun95        all fields.
c aug 22, 95 jh      : fix colors in  postscript, black was not set
c nov 20  95         : return in drwvec if less than 2 points
c aug 8   96         : set linewidth to 0.1 everywhere, also in init_post
c nov 6   96         : add color_syn
c feb     97         : do not link to tektronics anymore
c may 22             : change write statement for ps initiation so no blank
c oct                : do not open display in input_box
c sep 98 by jh       : ---------------   version 7.0 check -----------------
c                      no change
c nov 4              : linux logic
c nov 5              : remove '_' form c routine calls because of linux
c mar 22 99 bmt      : add oneline subroutine
c apr 14 99 bmt      : online modified
c apr 20    bmt      : oneline modified 
c may 6     bmt      : oneline modified 
c sep 20    bmt      : oneline modified 
c dec 12    lo       : inbox routine, dont return after 'all'
c mar 16 01 jh       : set def color station
c apr 24 03 jh       : put option to select all z-channels in input_box
c may  7 03 lo       : add myxfont to xopen call
c june 22   jh       : increase title in input_box
c may 26    lo       : deselect all non-Z channels when selecting All Z
c apr 22 09 jh       : Wayne Crawford's suggestions for improved graphics
c                      by making argument to plot position real
c 2010-05-04 pv  : changed some tab-formatted source line that gave warnings in f90
c 2010-05-11 wcc     : Add shortcut characters at right side of box
c 2010 12 18 jh      : gfortran on pc, many changes
c 2010 dec 31 jh     : remove most computer dependendence except in oneline
c 2011 jan 10 jh     : do not use color_type, can it be removed ?
c 2011 feb 22 jh     : put in plot size reading
c 2011 jun 20 jh     : make all input variables dynamic
c 2011 nov 2  jh     : more of the above
c 2011 nov 3  jh     : ----------------
c 2012 dec 9  jh     : increase dimension of ch from 200 to 1000
c 2013 jan 8  jh     : increase xchar string length from 90 to 200
c                      plot text as one string in box call,
c                      do not write numbers in All Z and Picked
c 2013 jan 10 jh     : call clearwindow and call DrawBox did not work in
c                      some circumsrtances so replace by simpler calls
c 2013 may 21 jh     :  dimension problem
c 2013 jun 21 jh     : patch up problem with long time windows in ps
c 2014 feb 06 jh     :  remove call get_seisan_def, creates probelms by
c                      redinfinig paremters. make a fix to keep original 
c                      value of myxfont, was changed be call and no longer
c                      redefined by get_seisan_def 
c 2014 02 24 jh       :in get_colr, increase dim of colors from 60 to 100
c 2014 09 05 jh       : also in get_window_size

c
c   Subroutines for making hard copy files when using screen         
c   plot. The calls will plot on either an X-screen, on PC using 
c   DISLIN graphics and can at the same time make PostScript files.               
c   The screen coordinate system is tectronics (1024*780) since the first
c   version was made for Tektronics. The numbers are reals so higher 
c   resolution can be used.           
c 
c   When making postscript files and using many xdrwabs and xmovabs             
c   might give too many postscript commands without using a STROKE              
c   (postscript for making the bitmap). There is currently no check             
c   of how many lines are drawn except in the routine for drawing               
c   traces of data (XDRWVEC), so be careful.
c
c       
c                                                          
c                                                                               
c   All routines have common block tekout with the variables:                   
c                                                                               
c   plotoption: 0: only screen plot                                         
c               1: screen + hc file                                             
c               2: only hc file                                                 
c
c
c   plotunit:   logical file unit for hc file                                   
c                                                
c                                                                               
c   The parameters in common block must be set in main program.                 
c   Most routines make both a screen plot and               
c   a plot file (routine name start with x). For plotting characters,           
c   scaling might be different since postscript has more font sizes             
c   and separate routines are provided: pchars: for post script                 
c   tchars: for screen
c
c   *** if used with DISLIN on Linux, routine oneline must be modified ****
c                                                      
c--------------------------------------------------------------------------     
c 


      subroutine open_display
c
c  open the grapichs display and put attention there
c
      implicit none                                                             

      integer color_type        ! 1: a color x screen, 0; bw
      character*80 myxfont_org  ! original font for x
      include 'seiplot.inc'
      include 'libsei.inc'      !JAB(BGS)Jul95 Library definitions.
      include 'seisan.inc'      

c
       myxfont_org=myxfont
c           myxfont=
c    &  '-adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1'
c
c   read and set object colors
c
       xblue=blue$
       xgreen=green$
       xred=red$
       xyellow=yellow$
       xwhite=white$
       xblack=black$
      call set_color_object

c
c   Get platform details...
c   -----------------------
c

c                                                                               
c   check if screen plot, if only hardcopy, jump next section                     
c                                                                               
      if(plotoption.lt.2) then               
	 if(disp_open.eq.0) then ! display not already open
            call xopen(wsize,color_type,cursortype,myxfont_org)   
cx            if(color_type.eq.0) color_screen=0 ! if not c. screen, turn off col.
cx   next two for x, fix for dislin, now dummy in dislinplot
	    call xgetabsposition()     ! get position of initial text window
            call xmovetographics()     ! move to graphics window
cx   next dummy in dislin
            call set_background(color_back) ! set background color
	    disp_open=1          ! indicate display has been opened
	 else                    ! display already open, just clear
	    call clear_display
c
c   display might have been changed since last call, reajust, dummy for dislin
c
cx	    if(display_type.eq.1) call updatewindowsize()
	 endif
      endif                                                                     
c
c                                                                               
c   postscript, assume always initialize when calling open display   
c   
                                                                          
      if(plotoption.gt.0) then                                  
	 call init_post
      endif                                                                     
c
c   set def color
c
      call xset_color(xblack)
                                                                         
      return                                                                    
      end                                                                       
c-----------------------------------------------------------------------------  

      subroutine init_post                                                   
c
c   just for initializing a post script file
c
      implicit none                                                             
      include 'seiplot.inc'
c
      character  chr_text *(80)                        ! Local text string.
c
  
      if(plotoption.gt.0) then                              
c
c-- open plotter for postscript                   
         write(plotunit,'(a)')'%!PS-Adobe-2.0 EPSF-2.0'    ! start of postscript
	 write(plotunit,'(a)')'%%BoundingBox: 0 0 595 841' ! A4 page size.
	 write(plotunit,'(a)')'newpath'
c                                                                               
c  move plot a bit so it fits                                                   
c                                                                               
	 write(chr_text(1:10),'(2F5.1)') PLOT_TRANSX$,
     &                                   PLOT_TRANSY$
	 chr_text = chr_text(1:10) // ' translate'
	 write(plotunit,'(a)') chr_text
c                                                                               
c   scale to fit tektronics coordinate system                                   
c                                                                               
         call get_env_psscale(PLOT_SCALEX$,PLOT_SCALEY$) ! lo
	 write(chr_text(1:9),'(F5.2,F4.1)') PLOT_SCALEX$,
     &                                      PLOT_SCALEY$
	 chr_text = chr_text(1:9) // ' scale'
	 write(plotunit,'(a)') chr_text
c                                                                               
c        initialize one font                                                    
c                                                                               
	 write(plotunit,*)'/Times-Roman findfont'
c                                                                               
c   scale font so that they are not deformed                                    
c   from scale                                                                  
c                                                                               
	 write(plotunit,*)'[20 0 0 15 0 0] makefont'
	 write(plotunit,*)'setfont'
c
c   set def color
c
c        call xset_color(color_def)
c
c   set line size
c
	 write(plotunit,*)'0.1 setlinewidth'
      endif                                                                     
c                                                                               
      return                                                                    
      end                                                                       
c-----------------------------------------------------------------------------  
      subroutine fontsize(font,size)                                            
c                                                                               
c   set postscript font and size                   
c                                                                               
c      if font is zero, use standard font                                       
c      if size is zero, use standard size                                       

      implicit none                                                             
c-- font multiplication factor                                 
      real scale                        
      integer font                                                              
      real size                                                                 
      include 'seiplot.inc'
cx                                                                               
c   check if screen plot                                                           
c                                                                               
cc      if(hctype.eq.0.or.(hctype.gt.0.and.plotoption.lt.2)) then                 
cc      endif                                                                     
c                                                                               
c   postscript                                                                  
c                                                                               

      if(plotoption.gt.0) then                                 
c                                                                               
c   set size in mm (hopefully)                                                  
c                                                                               
	 if(size.eq.0.0) then
	    scale=1.0
	 else
	    scale=size/3.5
	 endif
c                                                                               
c        initialize font                                                        
c                                                                               
	 if(font.eq.0) write(plotunit,*)'/Times-Roman findfont'
	 write(plotunit,*)'[20 0 0 15 0 0] makefont'
c                                                                               
	 write(plotunit,201)scale
 201     format(f8.2,1x,' scalefont')
	 write(plotunit,*)'setfont'
      endif                                                                     
c                                                                               
      return                                                                    
      end                                                                       




      subroutine close_post                                                   
c
c   put closing lines in postscript file
c
      implicit none
      character*1 ctld                                                          
      include 'seiplot.inc'
c                                                                               
c   postscript                                                                  
c                                                                                 
      if(plotoption.gt.0) then                              
c        write(plotunit,*)'0.2 setlinewidth'                                    
	 write(plotunit,*)'0.1 setlinewidth'
	 write(plotunit,*)'stroke'
	 write(plotunit,*)'showpage'
	 ctld=char(4)
	 write(plotunit,'(a)') ctld
      endif                                                                     
c                                                                               
      return                                                                    
      end                                                                       
c------------------------------------------------------------------------------ 
      subroutine clear_display                                                         
c                                                                               
c   clear display                                                           
c               
      implicit none                                                                
      include 'seiplot.inc'
c                                                                               
c   check if screen plot                                                           
c                                                                               
      if(plotoption.lt.2) then              
	    call xclear  
      endif                                                                     
                                                                              
      return                                                                    
      end                                                                       
c------------------------------------------------------------------
                                                                              
      subroutine xmovabs(x,y)                                                   
c                                                                               
c   emulate tek movabs                                                          
c                                                                               
      implicit none                                                             
      real x,y                                                                  
c WCC 4/2009: Mod to real for smoother plots
      integer ix,iy
      real xold,yold
      include 'seiplot.inc'
      common /movdrw/xold,yold
c                                                                               
c   check if screen plot                                                           
c                                                                               
      if(plotoption.lt.2) then
	 ix=x
	 iy=y
         call xmoveto(x,y)
	 xold=x
	 yold=y
      endif                                                                     
c                                                                               
c   postscript                                                                  
c                                                                                
      if(plotoption.gt.0) then                               
	 write(plotunit,200) x,y
 200     format(2(f7.1,1x),'moveto')              !JAB(BGS)Jun95.
      endif                                                                     
      return                                                                    
      end                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine pmovabs(x,y)                                                   
c                                                                               
c   same as xmovabs without postscript                                                          
c                                                                               
      implicit none                                                             
      real x,y                                                                  
c
c WCC 4/2009: Mod to real for smoother plots
      integer ix,iy
      real xold,yold
      include 'seiplot.inc'
      common /movdrw/xold,yold
c                                                                                                                                                           
      if(plotoption.lt.2) then
	    ix=x
	    iy=y
	    call xmoveto(x,y)
	    xold=x
	    yold=y
      endif                                                                     
      return
      end
c                                                                               
cc---------------------------------------------------------------------         
      subroutine xout(x,y)                                                      
c                                                                               
c   makes a stroke and move pen to x,y, post script only                        
c                                                                               
      implicit none                                                             
      real x,y                                                                  
      include 'seiplot.inc'
                                                             
c                                                                               
c   postscript                                                                  
c                                                                               
      if(plotoption.gt.0) then                                
	 write(plotunit,*)'0.1 setlinewidth'
	 write(plotunit,*)'stroke'
	 write(plotunit,200) x,y
 200     format(2(f7.1,1x),'moveto')            !JAB(BGS)Jun95.
      endif                                                                     
      return                                                                    
      end  
                                                                    
c----------------------------------------------------------------------         
      subroutine xdrwabs(x,y)                                                   
c                                                                               
c   emulate tek drwabs                                                          
c               
      implicit none                                                                
      real x,y                                                                  

c WCC 4/2009: change to real for smoother plotting
      integer ix,iy
      real xold,yold
      include 'seiplot.inc'
      common /movdrw/ xold,yold
c
c                                                                               
c   check if screen plot                                                           
c                                                                               

      if(plotoption.lt.2) then
	 ix=x
	 iy=y
	 call xlineto(xold,yold,x,y)
	 xold=x
	 yold=y
      endif                                                                     
c                                                                               
c   postscript                                                                  
c                                                                               
  
      if(plotoption.gt.0) then                                 
	 write(plotunit,200) x,y
 200     format(2(f7.1,1x),'lineto')              !JAB(BGS)Jun95.
      endif                                                                     
      return                                                                    
      end                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine pdrwabs(x,y)                                                   
c                                                                               
c   emulate tek drwabs, same as xdrwabs without postscript                                                          
c               
      implicit none                                                                
      real x,y                                                                  
c WCC 4/2009: change to real for smoother plotting
      integer ix,iy
      real xold,yold
      include 'seiplot.inc'
      common /movdrw/ xold,yold
c
c                                                                               
c   check if screen plot                                                           
c                                                                               
      if(plotoption.lt.2) then                
	 ix=x
	 iy=y
	 call xlineto(xold,yold,x,y)
	 xold=x
	 yold=y    
      endif                                                                     
      return
      end
c                                                                               
c----------------------------------------------------------------------         
      subroutine xdrwvec(n,x0,y,step)
c 
c   draw a vector of n points, special for equally spaced points
c
c  sep 25 by jh : modified for pc to use less memory, step in call
c  may 95       : use pmovabs and pdrwabs for screen plot to avoid
c                 double plotting in plotoption 1
c  nov 20 95    : retuen if less than 2 points
c
      implicit none
      integer n                         ! number of points to plot
	  real x0                                               ! first x-value
      real x                            ! x-value to plot
	  real y(*)                                                     ! y-values to plot
      integer i,i1,i2                           ! counters
      real step                                                 ! distance between points, x-axis
      include 'seiplot.inc'
c
c   do not plot if less than 2 points
c
      if(n.lt.2) return
c
c   check if screen plot 
c

      if(plotoption.lt.2) then
	 x=x0
	 call pmovabs(x0,y(1))
	 do 1 i=2,n
	    x=x+step
	    if(x.gt.screen_sizex$) x=screen_sizex$
	    call pdrwabs(x,y(i))
1        continue
      endif
c
c   postscript, print out 200 points at a time, put y values in stack
c   and then plot in a loop using lineto
c   remember that points must be put in stack in reverse order
c

      if(plotoption.gt.0) then
	 i1=1
	 i2=200
	 if(i2.gt.n) i2=n
	 if(n.eq.1) n=2
	 write(plotunit,200) x0,y(1)            
c
 10      continue
	 write(plotunit,'(16i5)') (int(10*y(i)),i=i2,i1,-1)     
c
c   multiply upper loop number with 1.00001 to ensure that exactly
c   200 numbers are read from the stack, real number inaccuracy
c   did somtimes result in only 199 numbers were read
c
c   jun 20134 jh: change accuracy of sept wit 2 decimals and the factor
c                 1.0001 to 1.00001 to make long plots possible
c
	 write(plotunit,201) 
     *   x0+(i1-1)*step,step,x0+(i2-1)*step*1.000001
 201     format(f12.7,1x,f12.9,1x,f12.7,1x,'{exch 0.1 mul lineto} for') !JAB(BGS)Jun95.
	 write(plotunit,*)'0.1 setlinewidth'
	 write(plotunit,*)'stroke'
	 i1=i1+200
	 i2=i2+200
	 if(i2.gt.n) i2=n
	 if(i1.gt.n) return                             
	 write(plotunit,200) x0+(i1-2)*step,y(i1-1) 
 200     format(2(f7.1,1x),'moveto')                 !JAB(BGS)Jun95.
	 goto 10         
      endif
      return
      end
c----------------------------------------------------------------------
c                                                                     
c----------------------------------------------------------------------         
      subroutine xdrwlin(n,x,y)                                                 
c                                                                               
c   draw a vector of n points                                                   
c                                                                               
      implicit none                                                             
c-- number of points to plot              
      integer n                         
c-- data to plot                          
      real x(*),y(*)                    
c-- counters                                         
      integer ix,iy,i,i1,i2             
      include 'seiplot.inc'
c                                                                               

c   check if screen plot                                                           
c                                                                               

      if(plotoption.lt.2) then              
	 call xmovabs(x(1),y(1))
	 do 1 i=2,n
	    ix=x(i)
	    iy=y(i)
	    call xdrwabs(x(i),y(i))
1        continue
      endif
c                                                                               
c   postscript, print out 100 points at a time, put y values in stack           
c   and then plot in a loop using lineto                                        
c   remeber that points must be put in stack in reverse order                   
c                                                                                
      if(plotoption.gt.0) then                               
	 i1=1
	 i2=100
	 if(i2.gt.n) i2=n
	 if(n.eq.1) n=2
c-- move to first point of trace          
	 write(plotunit,200) x(1),y(1)          
c                                                                               
 10      continue
	 do i=i1,i2
	    write(plotunit,201) x(i),y(i)
 201        format(2(f7.1,1x),'lineto')         !JAB(BGS)Jun95.
	 enddo
	 write(plotunit,*)'0.1 setlinewidth'
	 write(plotunit,*)'stroke'
	 i1=i1+100
	 i2=i2+100
	 if(i2.gt.n) i2=n
c-- check if more poits
	 if(i1.gt.n) return                             
c-- move to last point              
	 write(plotunit,200) x(i1-1),y(i1-1)            
 200     format(2(f7.1,1x),'moveto')             !JAB(BGS)Jun95.                                 
	 goto 10
      endif
      return                                                                    
      end                                                                       
c#############################################################################  

      SUBROUTINE XCHARS(STRING,NSTR,X,Y)                                        
C                                                                               
C   PLOTS NSTR CHARACTERS FROM STRING STRING ON                                 
C   POSITION X,Y ON A TECTRONICS SCREEN AND A POSTSCRIPT PRINTER                
C                                                                               
c   J. Havskov, sometimes in 1985                                               
c   1991 by j.h. : adopted to postscript and tektronics                         
c   nov  25 93 by jh: check for ( and )
CJAB(BGS)Feb95   : Put double spacing if non-proportional font (eg. T.Roman)
CJAB(BGS)Feb95     and required by calling program
c                                                                               
      implicit none
      CHARACTER(*) STRING                                                       
      CHARACTER*1 SINGLE(200)                                                    
      REAL X,Y                                                                  
      INTEGER CHAR(200)                                                          
      INTEGER IX,IY,i,nstr                                                             
c
      external     sei clen                     ! String length.
      integer      sei clen,                    ! Function.
     &             copy_n,                      ! # chars in string copy.
     &             string_n                     ! # chars in string.
      character    chr_string *(200),            ! Working string.
     &             chr_copy   *(200),            ! & copy.
     &             chr_temp   *(200)             ! & working copy.
      equivalence (chr_string,single)           ! Byte by byte.
c
      include 'seiplot.inc'
c
c   Prepare the string...
c   ======================
c
      string_n   = nstr                         ! Input length of string.
      chr_string = string                       ! Copy string.
c
c   Remove parenthesis...
c   ---------------------
c
1000  ix = index(chr_string,'(')                !
      if( ix .gt. 0 ) then                      !
         chr_string(ix:ix) = ' '                   !
         goto 1000                                 !
      end if                                    !
c
1100  ix = index(chr_string,')')                !
         if( ix .gt. 0 ) then                      !
         chr_string(ix:ix) = ' '                   !
         goto 1100                                 !
      end if                                    !
c
c    Force double spacing when forced and non-proportional font...
c    -------------------------------------------------------------
c    Do this because, space is about half-space of proportional font...
c
      copy_n     = nstr                         ! & copy.
      chr_copy = chr_string                         ! Make a copy.
c
      if( b_force_space$ ) then                     ! Look at spacing.
      b_force_space$ = .false.                      ! Re-set.
c
      ix = 0                                        ! Pointer.
100   ix = ix + 1                                   ! & increment.
c
         if( ix .eq. string_n ) then                ! Finished.
         continue                                   !
c
         else if( chr_string(ix:ix) .eq. ' ' ) then ! Found a space.
         ix = ix + 1                                ! Increment pointer.
         string_n = string_n + 1                    ! & length.
         chr_temp = ' ' // chr_string(ix:)          ! Padded remainder.
         chr_string(ix:) = chr_temp                 ! & add insert.
         goto 100                                   ! Next character.
c
         else                                       ! No space.
         goto 100                                   ! Next character.
         end if                                     !
      end if                                        !
c
c  PostScript...
c  =============
c

      if(plotoption.gt.0) then
         write(plotunit,200) x,y
 200     format(2(f7.1,1x),'moveto')               !JAB(BGS)Jun95.
c
            if( b_prop_psfont$ ) then
            write(plotunit,201) chr_copy(1:copy_n)        
            else
            write(plotunit,201) chr_string(1:string_n)
            end if
c
 201     format('(',a,') show')
      endif
c
c   check if screen plot...
c   ====================
c                                                                                    
      if(plotoption.lt.2) then              
	 ix=x
	 iy=y
         if( b_prop_font$ ) then
            call xtext(chr_copy,copy_n,ix,iy)
         else
            call xtext(chr_string,string_n,ix,iy)
         end if
      endif                                                                     
c                                                                               
      RETURN                                                                    
      END                                                                       
c#############################################################################  

      SUBROUTINE XCHARSWIN(STRING,NSTR,X,Y)                                        
c
c XCHARSWIN is the same as XCHARS, but calls xtextwin instead of xwin
c
c lo 18.03.2002                                  
c
      implicit none
      CHARACTER*90 STRING                                                       
      CHARACTER*1 SINGLE(90)                                                    
      REAL X,Y                                                                  
      INTEGER CHAR(90)                                                          
      INTEGER IX,IY,i,nstr                                                             
c
      external     sei clen                     ! String length.
      integer      sei clen,                    ! Function.
     &             copy_n,                      ! # chars in string copy.
     &             string_n                     ! # chars in string.
      character    chr_string *(90),            ! Working string.
     &             chr_copy   *(90),            ! & copy.
     &             chr_temp   *(90)             ! & working copy.
      equivalence (chr_string,single)           ! Byte by byte.
c
      include 'seiplot.inc'
c
c   Prepare the string...
c   ======================
c
      string_n   = nstr                         ! Input length of string.
      chr_string = string                       ! Copy string.
c
c   Remove parenthesis...
c   ---------------------
c
1000  ix = index(chr_string,'(')                !
      if( ix .gt. 0 ) then                      !
      chr_string(ix:ix) = ' '                   !
      goto 1000                                 !
      end if                                    !
c
1100  ix = index(chr_string,')')                !
      if( ix .gt. 0 ) then                      !
      chr_string(ix:ix) = ' '                   !
      goto 1100                                 !
      end if                                    !
c
c    Force double spacing when forced and non-proportional font...
c    -------------------------------------------------------------
c    Do this because, space is about half-space of proportional font...
c
      copy_n     = nstr                         ! & copy.
      chr_copy = chr_string                         ! Make a copy.
c
      if( b_force_space$ ) then                     ! Look at spacing.
      b_force_space$ = .false.                      ! Re-set.
c
      ix = 0                                        ! Pointer.
100   ix = ix + 1                                   ! & increment.
c
         if( ix .eq. string_n ) then                ! Finished.
         continue                                   !
c
         else if( chr_string(ix:ix) .eq. ' ' ) then ! Found a space.
         ix = ix + 1                                ! Increment pointer.
         string_n = string_n + 1                    ! & length.
         chr_temp = ' ' // chr_string(ix:)          ! Padded remainder.
         chr_string(ix:) = chr_temp                 ! & add insert.
         goto 100                                   ! Next character.
c
         else                                       ! No space.
         goto 100                                   ! Next character.
         end if                                     !
      end if                                        !
c
c  PostScript...
c  =============
c

      if(plotoption.gt.0) then
         write(plotunit,200) x,y
 200     format(2(f7.1,1x),'moveto')               !JAB(BGS)Jun95.
c
            if( b_prop_psfont$ ) then
            write(plotunit,201) chr_copy(1:copy_n)        
            else
            write(plotunit,201) chr_string(1:string_n)
            end if
c
 201     format('(',a,') show')
      endif
c
c   check if screen plot...
c   ====================
c                                                                                 
      if(plotoption.lt.2) then                 
	 ix=x
	 iy=y
         if( b_prop_font$ ) then
	    call xtextwin(chr_copy,copy_n,ix,iy)
         else
            call xtextwin(chr_string,string_n,ix,iy)
         end if                                                                
c  
      endif                                                                             
      RETURN                                                                    
      END                                                                       
c#############################################################################  

      SUBROUTINE PCHARS(STRING,NSTR,X,Y)                                        
C                                                                               
C   PLOTS NSTR CHARACTERS FROM STRING STRING ON                                 
C   POSITION X,Y ON A POSTSCRIPT LASER                                          
C                                                                               
c   J. Havskov, sometimes in 1985                                               
c   1991 by j.h. : adopted to postscript                                        
      implicit none
      CHARACTER(*) STRING                                                       
      REAL X,Y                                                                  
      INTEGER nstr,i                                                          
C                                                                               
      include 'seiplot.inc'
c
c   check for parenthesis
c
      do i=1,nstr
	if(string(i:i).eq.'(') string(i:i)=' '
	if(string(i:i).eq.')') string(i:i)=' '
      enddo
c                                                                               
c  postscript                                                                   
c
      if(plotoption.gt.0) then                                                                                                                 
	 write(plotunit,200) x,y
 200     format(2(f7.1,1x),'moveto')            !JAB(BGS)Jun95.
	 write(plotunit,201) string(1:nstr)
 201     format('(',a,') show')                                                 
      endif                                                                     
c                                                                               
      RETURN                                                                    
      END                                                                       
c###############################################################

      SUBROUTINE TCHARS(STRING,NSTR,X,Y)
cx needed anymore, replace ???????
                                        
C                                                                               
C   PLOTS NSTR CHARACTERS FROM STRING STRING ON                                 
C   POSITION X,Y ON A TECTRONICS SCREEN                                         
C                                                                               
c   J. Havskov, sometimes in 1985                                               
c   1991 by j.h. : adopted to tektronics with real numbers                      c                                                                               
      implicit none
      CHARACTER(*) STRING                                                       
      CHARACTER*1 SINGLE(80)                                                    
      REAL X,Y                                                                  
      INTEGER CHAR(80)                                                          
      INTEGER IX,IY,nstr,i                                                             
C                                                                               
      include 'seiplot.inc'
c
c   check for parenthesis
c
      do i=1,nstr
	if(string(i:i).eq.'(') string(i:i)=' '
	if(string(i:i).eq.')') string(i:i)=' '
      enddo
c     
      do i=1,nstr                                                               
         READ(STRING(i:i),'(A1)') SINGLE(i)                              
      enddo
c                                                                               
c   check if screen plot                                                           
c   
                                                                          
      if(plotoption.lt.2) then                 
	 ix=x
	 iy=y
         call xtext(string,nstr,ix,iy)
      endif                                                                     
c                                                                               
      RETURN                                                                    
      END                                                                       

c------------------------------------------------------------------------------ 
      subroutine xnewpag                                                        
      implicit none
      include 'seiplot.inc'
c
      character  chr_text *(80)                        ! Local text string.
c
c   check if screen plot                                                           
c                                                                               

      if(plotoption.lt.2) then               
	 call clear_display
      endif                                                                     
c                                                                               
c   postscript                                                                  
c                                                                               

      if(plotoption.gt.0) then                                 
	 write(plotunit,*)'0.1 setlinewidth'
c        write(plotunit,*)'0.2 setlinewidth'
	 write(plotunit,*)'stroke'
	 write(plotunit,*)'showpage'
c                                                                               
c  move plot a bit so it fits                                                   
c                                                                               
	 write(chr_text(1:10),'(2F5.1)') PLOT_TRANSX$,
     &                                   PLOT_TRANSY$
	 chr_text = chr_text(1:10) // ' translate'
	 write(plotunit,'(a)') chr_text
c                                                                               
c   scale to fit tektronics coordinate system                                   
c                                                                               
         call get_env_psscale(PLOT_SCALEX$,PLOT_SCALEY$) ! lo
	 write(chr_text(1:9),'(F5.2,F4.1)') PLOT_SCALEX$,
     &                                      PLOT_SCALEY$
	 chr_text = chr_text(1:9) // ' scale'
	 write(plotunit,'(a)') chr_text
c                                                                               
c        initialize one font                                                    
c                                                                               
	 write(plotunit,*)'/Times-Roman findfont'
c                                                                               
c   scale font so that they are not deformed                                    
c   from scale                                                                  
c                                                                               
	 write(plotunit,*)'[20 0 0 15 0 0] makefont'
	 write(plotunit,*)'setfont'
c                                                                                
      endif                                                                     
c                                                                               
      return                                                                    
      end                                                                       
c----------------------------------------------------------------------------   
          
      subroutine xscursr(ich,x,y)                                               
      implicit none
c                                                                               
c   simulates tek routine scursr                                                          
c
      real x,y                                                                               
      integer ix,iy                                                             
      integer ich                                                              
      character*4 key
      include 'seiplot.inc'

c                                                                               
c   check if screen  plot                                                           
c                                                                               
      if(plotoption.lt.2) then             
	    call xcursr(key,ix,iy)
	    ich=ichar(key(1:1))
	 x=ix
	 y=iy
      endif                                                                     
c                                                                               
      return                                                                    
      end                                                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine xsymbol(symbol,size,x,y)
c
c
c   plot a symbol centered
c
c     symbol: symbol number
c     size:   size across in tek units
c     x,y:    position in tek units
c
c  aug 93 by jh : add triangel
cJAB(BGS)Jan95  : Convert to using symbol generator...
c
      implicit none
      external   draw_symbol
      integer    draw_symbol
c
      real x,y,size
      integer symbol     
c
      integer    code                       ! Dummy.
c
c    Use symbol generator...
c    -----------------------
c
      code = draw_symbol( symbol, x, y, size ) ! Draw the symbol.
c
c    Return to Caller...
c    ===================
c
       return
       end
c


      subroutine clear_to_alpha
c
c  clears from graphics and returns to the alpha screen
c
      include 'seiplot.inc'
      include 'libsei.inc'                 !JAB(BGS)Jul95 Library definitions.

c   Messages can be written...
c   --------------------------
c
      B_F_GRAPH$   = .false.                    !JAB(BGS)Jul95 Graphics closed!
      B_F_MESSAGE$ = .true.                     !JAB(BGS)Jul95 Can write!!.
c
c   if display is not open, just return
c
      if(disp_open.eq.0) return

      call xmovetoalpha()        ! move cursor to alpha window
      call xclose                ! close x window

c
c  indicate graphics window closed
c
      disp_open=0
c
c   Return to caller...
c   -------------------
c
      return
      end
c
c
      subroutine xset_color(color)
c
c   sets the color to color for following operations
c
      implicit none
      integer color    ! different colors
      integer color_to_set ! the actual color set
      include 'seiplot.inc'
      color_to_set=color   ! transfer variable since it might be changed
c
c  if color not in valid range, return
c
      if( color .lt. 1            .or.        ! Invalid.
     &    color .gt. xcolour_n$)  then
c          write(6,*) 'invalid color ',color
      return
      endif
c
c   if no colors are set, make sure the drawing and background are different
c   if no color background is chosen, make it white. If call comes from
c   set_box, black and white is already set, so jump.
c
      if(color_screen.eq.0.and.from_input_box.ne.1) then
	 if(color_back.eq.xblack) then 
	    color_to_set=xwhite
	 else
	    color_to_set=xblack   
	    color_back=xwhite
	 endif
      endif
c
      if(from_input_box.eq.1) color_to_set=color

c
c                                                                               
c   check if screen plot                                                          
c
      if(plotoption.lt.2) then  ! check if only hc plot                 
         call setcolorx(color_to_set)
      endif
c                                                                               
c   postscript, if not hc colors return since background here is
c   always white                                                                  
c               
      if(color_hard_copy.ne.1) return                                                                
      color_to_set=color
      if(plotoption.gt.0) then
	 write(plotunit,*)'stroke'
	 write(plotunit,*)'newpath'
         if(color_to_set.eq.xblue)
     *   write(plotunit,*) '0. 0. 1. setrgbcolor'
         if(color_to_set.eq.xred)
     *   write(plotunit,*) '1. 0. 0. setrgbcolor'
         if(color_to_set.eq.xgreen)
     *   write(plotunit,*) '0. 1. 0. setrgbcolor'
         if(color_to_set.eq.xyellow)
     *   write(plotunit,*) '0. 1. 1. setrgbcolor'
         if(color_to_set.eq.xwhite)
     *   write(plotunit,*) '1. 1. 1. setrgbcolor'
         if(color_to_set.eq.xblack)
     *   write(plotunit,*) '0. 0. 0. setrgbcolor'
	 write(plotunit,*)'stroke'
      endif
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine set_color_object
c
c   sets the color for different objects
c
      implicit none
      include 'seiplot.inc'
c                                                                               
c   set default colors                                                         
c
c
c   default is no colors
c
       color_screen=0
       color_hard_copy=0
c
c   no call from set_input_box yet
c
       from_input_box=0
c
       color_trace=xyellow
       color_station=xblue
       color_pic=xred
       color_syn=xblue
       color_def=xwhite
       color_frame=xgreen
       color_title=xyellow
       color_spec=xred
       color_axis_not=xyellow
       color_epi=xred
       color_map_contour=xyellow
       color_map_grid=xgreen
       color_symbol_key=xred
       color_prompt=xblack
       color_label_grid=xgreen
       color_section=xgreen
       color_bval_np=xgreen
       color_bval_ac=xred
       color_bval_line=xblue
       color_back=xwhite
       color_box=xyellow
       color_box_letter=xblack
       color_zoom=xred
       color_foc_dilat=xred
       color_foc_comp=xgreen
       color_foc_p=xred
       color_foc_t=xblue
       color_foc_plane=xblue
c
c  check if color definition file and possibly reset
c
       call get_colors      
c
c   if black and white, make sure toggling with set_box is correct
c
       if(color_screen.eq.0) then
	  if((color_box.ne.xblack.and.color_box.ne.xwhite).or.
     *    (color_box_letter.ne.xwhite.and.color_box_letter.ne.xblack).
     *    or.color_box_letter.eq.color_box) then
	     color_box=xwhite
	     color_box_letter=xblack
	  endif
       endif

       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_colors
c
c   reads colors from color.def file
c
      implicit none
      include 'seiplot.inc'
      include 'libsei.inc'
      character*80 colors(100)  ! max number of color objects
      integer in                ! input file unit
      integer n                 ! number of lines in def file
      integer c                 ! color number
      integer i,code

c
c   get def file
c
	   call sei get file( open$+ignore$,  ! Check for file.
     &                        in,              ! Unit 
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                        'COLOR.DEF' )    ! For this filename.
c
c  if it does not exist, return
c
      if( code .ne. e_ok$ ) return
c
c   read file
c
      i=1
  1   continue
      read(in,'(a)',end=2) colors(i)
      i=i+1
      goto 1
c
c   end of file, find and set object colors
c
  2   continue
      n=i
c
      do i=1,n
	 read(colors(i)(24:25),'(i2)',err=5) c
  5      continue
	 if(colors(i)(1:20).eq.'color_screen        ') color_screen=c
	 if(colors(i)(1:20).eq.'color_hard_copy     ') color_hard_copy=c
	 if(colors(i)(1:20).eq.'color_trace         ') color_trace=c
	 if(colors(i)(1:20).eq.'color_pic           ') color_pic=c
	 if(colors(i)(1:20).eq.'color_syn           ') color_syn=c
	 if(colors(i)(1:20).eq.'color_def           ') color_def=c
	 if(colors(i)(1:20).eq.'color_frame         ') color_frame=c
	 if(colors(i)(1:20).eq.'color_title         ') color_title=c
	 if(colors(i)(1:20).eq.'color_spec          ') color_spec=c
	 if(colors(i)(1:20).eq.'color_axis_not      ') color_axis_not=c
	 if(colors(i)(1:20).eq.'color_epi           ') color_epi=c
	 if(colors(i)(1:20).eq.'color_station       ') color_station=c
	 if(colors(i)(1:20).eq.'color_map_contour   ') 
     *      color_map_contour=c
	 if(colors(i)(1:20).eq.'color_map_grid      ')
     *      color_map_grid=c
	 if(colors(i)(1:20).eq.'color_label_grid    ')
     *      color_label_grid=c
	 if(colors(i)(1:20).eq.'color_symbol_key    ')
     *      color_symbol_key=c
	 if(colors(i)(1:20).eq.'color_prompt        ')
     *      color_prompt=c
	 if(colors(i)(1:20).eq.'color_section       ') color_section=c
	 if(colors(i)(1:20).eq.'color_bval_np       ') color_bval_np=c
	 if(colors(i)(1:20).eq.'color_bval_ac       ') color_bval_ac=c
	 if(colors(i)(1:20).eq.'color_bval_line     ') color_bval_line=c
	 if(colors(i)(1:20).eq.'color_box           ') color_box=c
	 if(colors(i)(1:20).eq.'color_box_letter    ') color_box_letter=c
	 if(colors(i)(1:20).eq.'color_back          ') color_back=c
	 if(colors(i)(1:20).eq.'color_zoom          ') color_zoom=c
	 if(colors(i)(1:20).eq.'color_foc_dilat     ') color_foc_dilat=c
	 if(colors(i)(1:20).eq.'color_foc_comp      ') color_foc_comp=c
	 if(colors(i)(1:20).eq.'color_foc_p         ') color_foc_p=c
	 if(colors(i)(1:20).eq.'color_foc_t         ') color_foc_t=c
	 if(colors(i)(1:20).eq.'color_foc_plane     ') color_foc_plane=c
      enddo
c
c   close file
c
      call sei close( close$, in, code ) ! Close (default stop on error).
      return
      end


      subroutine get_window_size
c
c   reads plot size from color.def file
c
      implicit none
      include 'seiplot.inc'
      include 'libsei.inc'
      character*80 colors(100)   ! max number of color objects
      integer in                ! input file unit
      integer n                 ! number of lines in def file
      integer c                 ! color number
      integer i,code
c
c   initilize size variables
c
          c=0
	  size_bvalue=c
	  size_catstat=c
	  size_codaq=c
	  size_corr=c
	  size_epimap=c
	  size_fk=c
	  size_focmec=c
	  size_lsq=c
	  size_mag=c
	  size_presp=c
	  size_rmsdep=c
	  size_sample_graphics=c
	  size_spec=c
	  size_ttplot=c
	  size_wad_plot=c
c
c   get def file
c
	   call sei get file( open$+ignore$,  ! Check for file.
     &                        in,              ! Unit 
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                        'COLOR.DEF' )    ! For this filename.
c
c  if it does not exist, return
c
      if( code .ne. e_ok$ ) return
c
c   read file
c
      i=1
  1   continue
      read(in,'(a)',end=2) colors(i)
      i=i+1
      goto 1
c
c   end of file, find and set sizes
c
  2   continue
      n=i
c
      do i=1,n
	 read(colors(i)(24:25),'(i2)',err=5) c
  5      continue
	 
	 if(colors(i)(1:20).eq.'size_bvalue         ') size_bvalue=c
	 if(colors(i)(1:20).eq.'size_catstat        ') size_catstat=c
	 if(colors(i)(1:20).eq.'size_codaq          ') size_codaq=c
	 if(colors(i)(1:20).eq.'size_corr           ') size_corr=c
	 if(colors(i)(1:20).eq.'size_epimap         ') size_epimap=c
	 if(colors(i)(1:20).eq.'size_fk             ') size_fk=c
	 if(colors(i)(1:20).eq.'size_focmec         ') size_focmec=c
	 if(colors(i)(1:20).eq.'size_lsq            ') size_lsq=c
	 if(colors(i)(1:20).eq.'size_mag            ') size_mag=c
	 if(colors(i)(1:20).eq.'size_presp          ') size_presp=c
	 if(colors(i)(1:20).eq.'size_rmsdep         ') size_rmsdep=c
	 if(colors(i)(1:20).eq.'size_sample_graphics') 
     *      size_sample_graphics=c
	 if(colors(i)(1:20).eq.'size_spec           ') size_spec=c
	 if(colors(i)(1:20).eq.'size_ttplot         ') size_ttplot=c
	 if(colors(i)(1:20).eq.'size_wad_plot       ') size_wad_plot=c


      enddo
c
c   close file
c
      call sei close( close$, in, code ) ! Close (default stop on error).
      return
      end


c
c   
      subroutine set_background(color)
      include 'seiplot.inc'
c
c  set the background color to color
c
      integer color,color_to_set

      color_to_set=color
c
c   if no colors are used, make sure background is not a color
c   and set to white if not black or white has been specified.
c
      if(color_screen.eq.0.and.
     *(color_back.ne.xblack.or.color_back.ne.xwhite))
     *color_to_set=xwhite
cx  next for x, fix
         call setbackx(color_to_set)  !
cx fix on dislin
	 call xclear
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine input_box(nbox,on,nx,xsize,ysize,text,ntext,
     *                     x0,y0,title,last_char,opendisp,
     *                     option)
c
c   displays a series of boxes, by clicking with the mouse
c   the value can be set or unset shown by inverse video. 
c
CJAB(BGS)Jan95   : Treat "ALL" box correctly; allocate an input colour
C                  when not set and allocate character before returning to
C                  caller (not after) and note that in limit the "on" array
C                  is "nbox" long and cannot accomodate the "ALL" and "OK"
C                  situations. Change code to reflect this..
c mar 29, 95 by jh: add option NONE
C nov 6   96      : add more dimentions to number of boxes
c 
C
      implicit none
      include 'seiplot.inc'
      include 'seidim.inc'
      integer nbox,cbox  ! number of boxes
      integer on(*)      ! 0: box not selected, 1:box selected
      integer nx         ! --------------- in x and y directions
      real xsize,ysize   ! box size
      character(*) text(*)   ! text in box
      character*1  ch(1000)	 ! shortcut character (WCC added)
      integer  ntext         ! number of characters to plot
      real x0,y0             ! upper left hand cornor
      character(*) title     ! title on top of plot
      character*1  last_char ! last character clicked
      integer opendisp       ! 0: do not open display, 1:open display
      real x1(max_trace+5),y1(max_trace+5),
     *x2(max_trace+5),y2(max_trace+5) ! cornors in squares of boxes
      real x,y,del
      integer ix,iy,i,k,ib,ich,l
      character*1 last_choise ! last_choise for channel selection
      integer color_in       ! color when routine called
      integer color_fr       ! color of frame
      integer hc_old         ! hard copy type
      integer plotoption_old ! save plot option
      integer option         ! 0: default with NONE, ALL and OK
                             ! 1: only single choice, not NONE, ALL and OK
      integer itxtmax	 ! don't go beyond here in text array
c
      logical  b_all,        !JAB(BGS)Jan95 All set on?.
     &         b_none        !JAB(BGS)Mar95 All set off?.
      common/box_input/last_choise 
      
c
c    Initialise...
c    =============
c
      itxtmax=80			 ! Must correpsond to definition of character * text
      color_in=color_current ! save current color
c
c   save hc setting, since hc must be turned off in this routine
c
cc      hc_old=hctype
      plotoption_old=plotoption
cc      hctype=0
      plotoption=0
      if (option.gt.1.or.option.lt.0) option=0
c
c   open display
c
      if(opendisp.eq.1) call open_display
      x=x0
      y=y0
      del=13      ! distance between boxes
c
c   title
c 
      call xset_color(color_title)
      call xchars(title(1:120),120,x0,y0+20+ysize)
c     call xchars(title(1:80),80,x0,y0+20+ysize)
c     call xchars(title(81:120),40,x0+700.0,y0+20+ysize)
c
c   text in last boxes
c
      text(nbox+3) = 'NONE'
      text(nbox+2) = 'OK'      
      text(nbox+1) = 'ALL'
c WCC
      ch(nbox+3) = 'n'
      ch(nbox+2) = 'o'      
      ch(nbox+1) = 'a'
      if(text(nbox)(1:5).eq.'All Z') then
      	ch(nbox)='z'
      endif
      if(nbox.gt.1) then   ! jh may 2013 
      if (text(nbox-1)(1:5).eq.'All Z') then
      	ch(nbox-1)='z'
      endif
      endif
      if(text(nbox)(1:6).eq.'Picked') then
      	ch(nbox)='p'
c end WCC
      endif
      	
c
c   Determine if all or no boxes are turned on...
c
      b_all = .true.                       ! Assume that all are on.
      b_none= .true.                       ! And off.
      do i = 1, nbox                       ! Loop boxes.
      b_all = b_all .and.                  ! Still all on?.
     &        on(i) .eq.  1                !
      b_none= b_none.and.                  ! Still all off?.
     &        on(i) .eq.  0                !
      end do                               !
c
c   indicate that color changes come from this routine, since
c   black and white must be togled when changing selection
c
	 from_input_box=1
c
c   draw, the frame must always contrast the background, so it is either
c   black or white
c
	 color_fr=xblack
	 if(color_screen.eq.0) then    ! no color screen
	    if(color_back.eq.xblack) color_fr=xwhite
	    if(color_back.eq.xwhite) color_fr=xblack
	 endif
c
c
 1    continue             ! from selection of z-channels
c
c   draw boxes...
c
      x=x0
      y=y0
      ix=1
c
c if option 1 dont plot boxes for ALL, NONE and OK
c
      cbox=nbox+3
      if (option.eq.1) cbox=nbox
      do ib=1,cbox
      
c   Add numbers to first 9 text boxes
      if(text(ib)(1:5).eq.'All Z') goto 666  ! jump out  
      if(text(ib)(1:6).eq.'Picked') goto 666 


      if (ib.le.9.and.ib.le.nbox) then
      	ch(ib)=char(ib+ichar('0'))
      endif
 666  continue
c
c   corners in box
c
	 x1(ib)=x
	 y1(ib)=y
	 x2(ib)=x+xsize
	 y2(ib)=y+ysize
c
	 call xset_color(color_fr)
	 call xmovabs(x1(ib)-3,y1(ib)-3)
	 call xdrwabs(x2(ib)+3,y1(ib)-3)
	 call xdrwabs(x2(ib)+3,y2(ib)+3)
	 call xdrwabs(x1(ib)-3,y2(ib)+3)
	 call xdrwabs(x1(ib)-3,y1(ib)-3)
c
c   -------------------------------
c   Allocate and set the colours...
c   -------------------------------
c   Display boxes on or off...
c   --------------------------
c   On...
c
      if( ib .le. nbox ) then                       ! A valid box.
	 if( on(ib) .eq. 1 ) then                   ! Is on.
	 call xset_color(color_box_letter)          ! Interior fill colour.
	 call fillbox(x1(ib),y1(ib),x2(ib),y2(ib)) ! & fill.
	 call xset_color(color_box)                 ! Text colour.
	 call xchars(text(ib),ntext,x1(ib)+5.0,     ! Write text.
     &                              y2(ib)-15.0)    !
c Add shortcut character at right side of box
	 call xset_color(color_box)                 ! Text colour.
	 call xchars(ch(ib),1,x2(ib)-8.0,     ! Write shortcut text.
     &                              y2(ib)-15.0)    !
c
c   Off...
c
	 else                                       ! Otherwise off.
	 call xset_color(color_box)                 ! Interior fill colour.
	 call fillbox(x1(ib),y1(ib),x2(ib),y2(ib)) ! & fill.
	 call xset_color(color_box_letter)          ! Text colour.
	 call xchars(text(ib),ntext,x1(ib)+5.0,     ! Write text.
     &                              y2(ib)-15.0)    !
c Add shortcut character at right side of box
	 call xset_color(color_box_letter)           ! Text colour.
	 call xchars(ch(ib),1,x2(ib)-8.0,     ! Write shortcut text.
     &                              y2(ib)-15.0)    !
	 end if                                     !
c
c   The "OK" box is off...
c   ----------------------
c
      else if( ib .eq. nbox+2 ) then                     ! Ok box is off.      
      call xset_color(color_box)                         ! Interior fill colour.
      call fillbox(x1(ib),y1(ib),x2(ib),y2(ib))         ! & fill.
      call xset_color(color_box_letter)                  ! Text colour.
      call xchars(text(ib),ntext,x1(ib)+5.0,y2(ib)-15.0) ! & write text.
c Add shortcut character at right side of box
	 call xset_color(color_box_letter)           ! Text colour.
	 call xchars(ch(ib),1,x2(ib)-8.0,     ! Write shortcut text.
     &                              y2(ib)-15.0)    !
c
c   The "None" box is on...
c   -----------------------
c
      else if( ib .eq. nbox+3 .and. b_none ) then        ! None box on.
      call xset_color(color_box_letter)                  ! Interior fill colour.
      call fillbox(x1(ib),y1(ib),x2(ib),y2(ib))         ! & fill.
      call xset_color(color_box)                         ! Text colour.
      call xchars(text(ib),ntext,x1(ib)+5.0,y2(ib)-15.0) ! & write text.
c Add shortcut character at right side of box
	 call xset_color(color_box)           ! Text colour.
	 call xchars(ch(ib),1,x2(ib)-8.0,     ! Write shortcut text.
     &                              y2(ib)-15.0)    !
c
c   The "None" box is off...
c   ------------------------
c
      else if( ib .eq. nbox+3 .and. (.not.b_none) ) then ! None box off.
      call xset_color(color_box)                         ! Interior fill colour.
      call fillbox(x1(ib),y1(ib),x2(ib),y2(ib))         ! & fill.
      call xset_color(color_box_letter)                  ! Text colour.
      call xchars(text(ib),ntext,x1(ib)+5.0,y2(ib)-15.0) ! & write text.
c Add shortcut character at right side of box
	 call xset_color(color_box_letter)           ! Text colour.
	 call xchars(ch(ib),1,x2(ib)-8.0,     ! Write shortcut text.
     &                              y2(ib)-15.0)    !
c
c   The "ALL" box is on...
c   ----------------------
c
      else if( b_all ) then                              ! All are on.
      call xset_color(color_box_letter)                  ! Interior fill colour.
      call fillbox(x1(ib),y1(ib),x2(ib),y2(ib))         ! & fill.
      call xset_color(color_box)                         ! Text colour.
      call xchars(text(ib),ntext,x1(ib)+5.0,y2(ib)-15.0) ! & write text.
c Add shortcut character at right side of box
	 call xset_color(color_box)           ! Text colour.
	 call xchars(ch(ib),1,x2(ib)-8.0,     ! Write shortcut text.
     &                              y2(ib)-15.0)    !
c
c   The "ALL" box is off...
c   -----------------------
c
      else                                               ! Otherwise off.
      call xset_color(color_box)                         ! Interior fill colour.
      call fillbox(x1(ib),y1(ib),x2(ib),y2(ib))         ! & fill.
      call xset_color(color_box_letter)                  ! Text colour.
      call xchars(text(ib),ntext,x1(ib)+5.0,y2(ib)-15.0) ! & write text.
c Add shortcut character at right side of box
	 call xset_color(color_box_letter)           ! Text colour.
	 call xchars(ch(ib),1,x2(ib)-8.0,     ! Write shortcut text.
     &                              y2(ib)-15.0)    !
      end if                                             !
c
c---------------------------------
c
c   check if box on other line
c
	 ix=ix+1
	 x=x+xsize+del
	 if(ix.gt.nx) then
	    ix=1
	    x=x0
	    y=y-ysize-del
	 endif
c pv put ALL Z, ALL, OK, NONE in new line
 	 if(new_line_all) then
 	 if(ix.eq.nbox) then
 	    ix=1
 	    x=x0
 	    y=y-ysize-del
 	 endif
 	 endif
      enddo
c
c   get up cursor...
c   ================
c
 10   continue
      call xscursr(ich,x,y)
      last_char=char(ich)
      if(last_char.eq.'z'.or.last_char.eq.'Z') last_choise='Z'
      if(last_char.eq.'p'.or.last_char.eq.'P') last_choise='P'
c
c   Return...
c   ---------
c
      if( last_char.eq.'q'  .or.
     &    last_char.eq.'Q'  .or.
     &    last_char.eq.'f'  .or.
     &    last_char.eq.'F') goto 99  ! return

c	Make chars '1'-'9' map to button click
      if( last_char.ge.'1' .and. last_char.le.'9') then
      	i=ichar(last_char)-ichar('0')
      	if (i.le.nbox) then
      		x=x1(i)+1
      		y=y1(i)+1
      	endif
      endif
c	Make chars 'o' and 'O' map to 'ok'
      if( last_char.eq.'o'.or.last_char.eq.'O'.and.cbox.eq.nbox+3) then
      	x=x1(nbox+2)+1
      	y=y1(nbox+2)+1
       endif
c	Make chars 'a' and 'A' map to 'All'
      if( last_char.eq.'a'.or.last_char.eq.'A'.and.cbox.eq.nbox+3) then
      	x=x1(nbox+1)+1
      	y=y1(nbox+1)+1
       endif
c	Make chars 'n' and 'N' map to 'None'
      if( last_char.eq.'n'.or.last_char.eq.'N'.and.cbox.eq.nbox+3) then
      	x=x1(nbox+3)+1
      	y=y1(nbox+3)+1
       endif
c	Make char 'z' and 'Z' map to 'All Z'
      if( last_char.eq.'z'.or.last_char.eq.'Z') then
		if ( text(nbox)(1:5).eq.'All Z') then
      		x=x1(nbox)+1
      		y=y1(nbox)+1
      	elseif( text(nbox-1)(1:5).eq.'All Z') then
      		x=x1(nbox-1)+1
      		y=y1(nbox-1)+1
      	endif
       endif
c	Make char 'p' and 'P' map to 'Picked'
      if( last_char.eq.'p'.or.last_char.eq.'P'.and.
     *  text(nbox)(1:6).eq.'Picked') then
      	x=x1(nbox)+1
      	y=y1(nbox)+1
       endif

c
c   check if a click in a box
c
c      do i=1,nbox+3
      do i=1,cbox
	 if(x.gt.x1(i).and.x.lt.x2(i).and.y.gt.y1(i).and.y.lt.y2(i))
     *   then                    
c
c   -----------------------------
c   Proceed on the box located...
c   -----------------------------
c   "OK" box selected...Exit...
c   ---------------------------
c
	    if( i .eq. nbox+2 ) then          ! Exit.
	    goto 99                           ! Return to caller.
c
c   "ALL" box selected...
c   ---------------------
c
	    else if( i .eq. nbox+1 ) then     ! select all
	    do k=1,nbox                       ! Loop boxes.
	      on(k)=1                         ! & turn on.
	      call xset_color(color_box_letter)
	      call fillbox(x1(k),y1(k),
     *                    x2(k),y2(k))
	      call xset_color(color_box)
	      call xchars(text(k),ntext,x1(k)+5.0,y2(k)-15.0)
c Add shortcut character at right side of box
	 	call xset_color(color_box)           ! Text colour.
	 	call xchars(ch(k),1,x2(k)-8.0,     ! Write shortcut text.
     &                              y2(k)-15.0)    !
	    enddo                          !
c           goto 99                        ! return to caller., comment out lo

c
c   "NONE" box selected...
c   ---------------------
c
            else if( i .eq. nbox+3 ) then     ! deselect all
            do k=1,nbox                       ! Loop boxes.
            on(k)=0                           ! & turn off.
            call xset_color(color_box)
            call fillbox(x1(k),y1(k),
     *                    x2(k),y2(k))
            call xset_color(color_box_letter)
            call xchars(text(k),ntext,x1(k)+5.0,y2(k)-15.0)
c Add shortcut character at right side of box
	 		call xset_color(color_box_letter)           ! Text colour.
	 		call xchars(ch(k),1,x2(k)-8.0,     ! Write shortcut text.
     &                              y2(k)-15.0)    !
            enddo
c
c   check for last input box to be all Z, used in mulplt
c
c           elseif( i.eq.nbox) then
c
c   select all z-cahnnels (if used in mulplt)
c
c              if(text(i)(1:5).eq.'All Z') then
c                 do l=1,nbox
c                   if(text(l)(9:9).eq.'Z') on(l)=1
c                 enddo
c                 goto 1
c              endif 
c
c   Turn an "off" box "on"...
c   -------------------------
c
	    else if( on(i) .eq. 0 ) then      ! Turn on.
	    on(i)=1
	    call xset_color(color_box_letter)
	    call fillbox(x1(i),y1(i),
     *                    x2(i),y2(i))
	    call xset_color(color_box)
	    call xchars(text(i),ntext,x1(i)+5.0,y2(i)-15.0)
c Add shortcut character at right side of box
	 	call xset_color(color_box)           ! Text colour.
	 	call xchars(ch(i),1,x2(i)-8.0,     ! Write shortcut text.
     &                              y2(i)-15.0)    !

c 
c return if option is 1
c
            if (option.eq.1) goto 99
c
c    Turn an "on" box "off"...
c    -------------------------
c
	    else                              !
	    on(i)=0
	    call xset_color(color_box)
	    call fillbox(x1(i),y1(i),
     *                    x2(i),y2(i))
	    call xset_color(color_box_letter)
	    call xchars(text(i),ntext,x1(i)+5.0,y2(i)-15.0)
c WCC added shortcut character at right side of box
	 	call xset_color(color_box_letter)           ! Text colour.
	 	call xchars(ch(i),1,x2(i)-8.0,     ! Write shortcut text.
     &                              y2(i)-15.0)    !
	    endif


c
c   check for last input box to be all Z, used in mulplt
c
           if( i.eq.nbox.or.i.eq.nbox-1) then
c
c   select all z-cahnnels (if used in mulplt)
c
              if(text(i)(1:5).eq.'All Z') then
                 do l=1,nbox
c                   if(text(l)(9:9).eq.'Z') on(l)=1
c lot 26/5/2005
                   if(text(l)(9:9).eq.'Z') then
                     on(l)=1
                   else
                     on(l)=0
                   endif  
                 enddo
                 goto 1
              endif 
            endif
c   check for last input box to be all Picks, used in mulplt
c
           if( i.eq.nbox) then
c
c   select all picked channels (if used in mulplt)
c
              if(text(i)(1:6).eq.'Picked') then
                 do l=1,nbox
                   if(text(l)(10:10).eq.'*') then
                     on(l)=1
                   else
                     on(l)=0
                   endif  
                 enddo
                 goto 1
              endif 
            endif
c
c    Check if all are on...
c    ----------------------
c
	 b_all = .true.                    ! Assume that all are on.
         b_none= .true.                    ! And all are off.
	 do iy = 1, nbox                   ! Loop boxes.
	 b_all = b_all .and.               ! Still all on?.
     &           on(iy) .eq.  1            !
         b_none= b_none.and.               ! Still all off?.
     &           on(iy) .eq.  0            !
	 end do                            !
c
c   The "ALL" box is on...
c   ----------------------
c
	 iy = nbox + 1                                 ! Point to "ALL" box.
	    if( b_all ) then                           ! All are on.
	    call xset_color(color_box_letter)          ! Interior fill colour.
	    call fillbox(x1(iy),y1(iy),x2(iy),y2(iy)) ! & fill.
	    call xset_color(color_box)                 ! Text colour.
	    call xchars(text(iy),ntext,x1(iy)+5.0,y2(iy)-15.0) ! & write text.
c Add shortcut character at right side of box
	 	call xset_color(color_box)           ! Text colour.
	 	call xchars(ch(iy),1,x2(iy)-8.0,     ! Write shortcut text.
     &                              y2(iy)-15.0)    !
c
c   The "ALL" box is off...
c   -----------------------
c
	    else                                       ! Otherwise off.
	    call xset_color(color_box)                 ! Interior fill colour.
	    call fillbox(x1(iy),y1(iy),x2(iy),y2(iy)) ! & fill.
	    call xset_color(color_box_letter)          ! Text colour.
	    call xchars(text(iy),ntext,x1(iy)+5.0,y2(iy)-15.0) ! & write text.
c Add shortcut character at right side of box
	 	call xset_color(color_box_letter)           ! Text colour.
	 	call xchars(ch(iy),1,x2(iy)-8.0,     ! Write shortcut text.
     &                              y2(iy)-15.0)    !
	    end if                                             !
c
c   The "NONE" box is on...
c   -----------------------
c
         iy = nbox + 3                                 ! Point to "NONE" box.
            if( b_none ) then                          ! All are off.
            call xset_color(color_box_letter)          ! Interior fill colour.
            call fillbox(x1(iy),y1(iy),x2(iy),y2(iy)) ! & fill.
            call xset_color(color_box)                 ! Text colour.
            call xchars(text(iy),ntext,x1(iy)+5.0,y2(iy)-15.0) ! & write text.
c Add shortcut character at right side of box
	 		call xset_color(color_box)           ! Text colour.
	 		call xchars(ch(iy),1,x2(iy)-8.0,     ! Write shortcut text.
     &                              y2(iy)-15.0)    !
c
c   The "NONE" box is off...
c   -----------------------
c
            else                                       ! Otherwise off.
            call xset_color(color_box)                 ! Interior fill colour.
            call fillbox(x1(iy),y1(iy),x2(iy),y2(iy)) ! & fill.
            call xset_color(color_box_letter)          ! Text colour.
            call xchars(text(iy),ntext,x1(iy)+5.0,y2(iy)-15.0) ! & write text.
c Add shortcut character at right side of box
	 		call xset_color(color_box_letter)           ! Text colour.
	 		call xchars(ch(iy),1,x2(iy)-8.0,     ! Write shortcut text.
     &                              y2(iy)-15.0)    !
            end if                                             !
	 endif                             !
      enddo
c
c   back for next click
c
      goto 10
c
99    continue
c
      from_input_box=0
c
c   reset color and hc type
c
      if( color_in .eq. 0 ) then                ! No colour installed.
      call xset_color(color_def)                ! Put in default.
      else                                      ! Otherwise.
      call xset_color(color_in)                 ! Put back colour.
      end if                                    !
c
cc      hctype=hc_old
      plotoption=plotoption_old
c
c   close display if only hc plotting 
c
      if(plotoption.eq.3) call clear_to_alpha
      return
      end

      subroutine xmessage(message,nlines,nchar,x,y)
c
c  put nlines messages message on the screen, all nchar long,
c  starting in lower left hand corner x,y
c
      implicit none
      include 'seiplot.inc'
      character(*) message(*)
      real x,y
      integer nlines,nchar
      real yy,ydel         ! y coordinate for line and distance between lines
      real xdel            ! distance between chars
      real xlength,ylength ! size of box
      integer i
c
c   calculate are to blank out
c
c
c   indicate that set color must follow instruction irrespective of
c   background
c
      from_input_box=1
      xdel=13.0
      ydel=20.0
c
      xlength=nchar*xdel
      ylength=nlines*ydel
c
c   blank out area, set background to white
c
c      xwhite=5
c      write(*,*) ' debug xwhite ',xwhite
      call xset_color(xwhite)
      call fillbox(x,y,x+xlength,y+ylength)
c
c   write text in black
c
      yy=y+ylength-12.0
      call xset_color(xblack)
      do i=1,nlines
	 call tchars(message(i),nchar,x+5.0,yy)
	 yy=yy-ydel
      enddo
      from_input_box=0
c
c   set color back to what it was
c
c     call xset_color(color_current)
      return
      end
c
      INTEGER FUNCTION DRAW_SYMBOL( SYM, X, Y, SIZE )
CSTART*************************************************************************
C                                                                             *
C                         BGS/GSRG Programming Unit                           *
C                         =========================                           *
C                                                                             *
C    System           : SEISAN                                                *
C    Module Name      : DRAW_SYMBOL                                           *
C    Purpose          : Primitive to draw a symbol                            *
C    Arguments -input : SYM        (I) Drawing instruction function index     *
C                                      add FILL$ to infill open symbols       *
C                     : SIZE       (R) Size of symbol in Tektronix units      *
C                     : X, Y       (R) Location of centre of symbol box in    *
C                                      Tektronix units.                       *
C             -output : function value is dummy condition                     *
C                                                                             *
C    Note             : Based on a routine of the same name for a development *
C                       GKS version of EQANAL (BGS Edinburgh - same author)   *
C    Author           : J. A. Bolton                                          *
C    Date             : 26 January 1995                                       *
C    Version          : V01                                                   *
C                                                                             *
CEND***************************************************************************
C
C    Arguments....
C    =============
C
      INTEGER     SYM                            ! Function to perform.
      REAL        SIZE,                          ! Size of symbol.
     &            X, Y                           ! & location of box centre. 
C
C    Local variables...
C    ==================
C
      INTEGER     CODE,                          ! local condition.
     &            IY,                            ! Very local.
     &            RADII,                         ! # radii.
     &            VERTEX,                        ! # vertices.
     &            RFRACT,                        ! Internal angle fraction
     &            VFRACT                         ! & for vertices.
     &           ,SYMBOL                         ! Primitive symbol.
      REAL        SCALE,                         ! Scaling of chord set.
     &            ANGLE,                         ! internal chord angle.
     &            DISPLACE,                      ! Initial angle displacme
     &            THETA                          ! Working angle.
      INTEGER     SYMBOL_C                       ! Length of symbol instruction.
      PARAMETER  (SYMBOL_C = 20)                 ! Larger than required!!
      CHARACTER   CHR_SYMBOL *(SYMBOL_C)         ! Symbol length
      LOGICAL     B_RADII,                       ! Draw a radius?.
     &            B_CHORD,                       ! or chord.?.
     &            B_FILL                         ! & fill it?.
C
C    Closed Polylines used for BASIC symbols...
C    ------------------------------------------
C
      INTEGER     POLY_N                         ! # points.
      PARAMETER  (POLY_N = 1000)                 ! & value.
      REAL        XX(POLY_N), YY(POLY_N)         ! & arrays.
C
C    Angular conversion...
C    =====================
C
      REAL       PI$,                           ! Radians in semi-circle.
     &           DEG_TO_RAD                     ! Degrees to radians.
      PARAMETER (PI$        = 3.141592654)      ! & values.
      PARAMETER (DEG_TO_RAD = PI$ / 180.0)      ! Degrees to radians.
C
C    System variables...(here since "symbol.inc" contains data)...
C    =============================================================
C
      INCLUDE     'libsei.inc'
      INCLUDE     'seiplot.inc'
      INCLUDE     'symbol.inc'
C
C    =============
C    Initialise...
C    =============
C
      CODE = E_OK$                             ! Initial condition.
      SYMBOL = MOD(SYM,FILL$)                  ! Primitive.
      B_FILL = SYM .GE. FILL$                  ! Infilled?.
C
C    Decode the instructions...
C    ==========================
C
      CHR_SYMBOL = CHR_SYMBOL$( SYMBOL )       ! Get symbol.
      B_RADII = CHR_SYMBOL(1:1) .EQ. 'T'       ! Radii required?.
      B_CHORD = CHR_SYMBOL(7:7) .EQ. 'T'       ! Draw chords?.
      B_FILL  = B_CHORD .AND. B_FILL           ! & area fill?.
C
      READ(CHR_SYMBOL(2:2),  '(I1)') RADII     ! # radii.
      READ(CHR_SYMBOL(3:4),  '(I2)') RFRACT    ! % displacement.
      READ(CHR_SYMBOL(5:6),  '(I2)') IY        ! Change in scale of radii.
      SCALE = 1.0 / (1.0 + FLOAT(IY)*.01)      ! Scale chord so radii fits box.
      READ(CHR_SYMBOL(8:10), '(I3)') VERTEX    ! # vertices.
      READ(CHR_SYMBOL(11:13),'(I3)') VFRACT    ! Vertex % displacment.
C
C    =============
C    Draw radii...
C    =============
C
      IF( B_RADII ) THEN                         ! Radii required!
      ANGLE = (360.0/RADII) * DEG_TO_RAD         ! Internal angle.
      DISPLACE = ANGLE * RFRACT*0.01             ! Displacement angle.
C
      THETA = DISPLACE                           ! initialise angle.
      DO 1000 IY = 1, RADII                      ! Loop radii.
      THETA = THETA + ANGLE                      ! vertex angle.
      XX(2) = X + SIZE*0.5*COS(THETA)            ! Other end.
      YY(2) = Y + SIZE*0.5*SIN(THETA)            !
C
      CALL XMOVABS(X,    Y)                      ! Move to 1st point.
      CALL XDRWABS(XX(2),YY(2))                  ! & draw to next.
1000  CONTINUE                                   !
      END IF                                     !
C
C    ==============
C    Draw chords...
C    ==============
C    Set up PostScript filling...
C    ----------------------------
C 
      IF( B_CHORD ) THEN                         ! Chords required.!
         if( b_fill             .and.
     &       plotoption .gt. 0)
cc     &       .and. hctype     .eq. 1 )
     &   write(plotunit,*)' stroke newpath'
c
      ANGLE = (360.0/VERTEX) * DEG_TO_RAD        ! Internal angle.
      DISPLACE = ANGLE * VFRACT*0.01             ! Displacement angle.
      VERTEX = VERTEX + 1                        ! Ensure polygon closure.
C
      THETA = DISPLACE                           ! initialise angle.
      DO 1200 IY = 1, VERTEX                     ! Loop vertices.
      THETA = THETA + ANGLE                      ! vertex angle.
      XX(IY) = X                                 ! X location is offset.
     &       + SIZE*0.5*SCALE*COS(THETA)         ! + scaled radius vector.
      YY(IY) = Y                                 ! Ditto y location.
     &       + SIZE*0.5*SCALE*SIN(THETA)         !
C
C    Draw outline...
C
	 IF( IY .EQ. 1 ) THEN                    ! Move to 1st point.
	 CALL XMOVABS(XX(1),YY(1))               !
	 ELSE                                    ! Otherwise displace.
	 CALL XDRWABS(XX(IY),YY(IY))             !
	 END IF                                  !
1200  CONTINUE                                   !
      CALL XDRWABS(XX(1),YY(1))                  ! & draw to 1st point.
C
C    Fill the polygon...
C    -------------------
C
	 IF( B_FILL ) THEN                       ! Area fill.
         if( plotoption .gt. 0)                  ! PostScript fill completion.
cc     &  .and.     hctype     .eq. 1 )                 ! Ditto.
     &   write(plotunit,*)' closepath fill'      ! Et ditto.
c
	 CONTINUE                                ! X fill not installed.
	 END IF                                  !
      END IF                                     !
C
C    ===================
C    Return to caller...
C    ===================
C
9999  CALL XMOVABS( X, Y )                  ! Move back to centre of symbol.
      DRAW_SYMBOL = CODE
      RETURN
       END

	  
c**********************************************************
        subroutine oneline(question,nq,answer,na,x,y)

c  general version

c   Puts the question with nq characters at position x,y on the screen,
c   reads the answer answer with a maximum of na characters. The whole
c   string has a frame. The answer is terminated by a CR.
c
      implicit none
      character(*) question           ! question
      integer nq
      character(*) answer
      integer na
      real  x,y
      character*80 text
      logical pc,linux,sun
      call computer_type(sun,pc,linux)
c
c   use only poneline if dislin on linux
C

c
c   the folloing is the normal setup, X on linux and dislin on pc
c   comment out 5 lines to use on both platforms
c
      if(pc) then   ! dislin version
         call poneline(question,nq,answer,na,x,y)
      else          ! xlib version
         call xoneline(question,nq,answer,na,x,y)
      endif
c
c   the following is to use dislin on both platforms, remove comment
c
c     call poneline(question,nq,answer,na,x,y)

      return
      end


        subroutine poneline(question,nq,answer,na,x,y)

c  DISLIN version

c   Puts the question with nq characters at position x,y on the screen,
c   reads the answer answer with a maximum of na characters. The whole
c   string has a frame. The answer is terminated by a CR.
c
      implicit none
      character(*) question           ! question
      integer nq
      character(*) answer
      integer na
      real  x,y
      character*80 text
      CALL SWGWTH (60)
      answer=' '
      call dwgtxt(question,answer)

      return
      end

c**********************************************************


	subroutine xoneline(question,nq,answer,na,x,y)

c

c   Puts the question with nq characters at position x,y on the screen,

c   reads the answer answer with a maximum of na characters. The whole

c   string has a frame. The answer is terminated by a CR. 

c 

c      implicit none

      

      character(*) question           ! question

      integer nq

      character(*) answer

      integer na

      real  x,y               

      include 'seiplot.inc'           ! seisan parameters

      integer ix,iy                 ! for plotting

      character*1  key                ! character form one key press

      integer j,nchar

      integer ia

      real ix1,iy1,ix2,iy2



 

c      type (xycoord) xy

	

	integer flag,kchar,flagkey,flagMouse

        real xmouse,ymouse

 	common /mousekey/flag,xmouse,ymouse,kchar,flagkey,flagMouse

	real xfirst,yfirst,xscale,yscale,ymin,ymax,xmin,xmax

	common /xy_plot_common/xfirst,yfirst,xscale, 

     * yscale,ymin,ymax,xmin,xmax

	character*80 text

c

      ix=int(x) 

      iy=int(y) 

   	nchar = na

c

c   blank variables

c

      key=' '

      do i=1,na

         text(i:i)=' '

      enddo

c

c   make rectangel

c

      ix1=ix

      iy1=iy-3

      ix2=ix+(nq+na)*11+42

      iy2=iy+25
c
c   blank out area, set background to white
c
c      xwhite=5
c      write(*,*) ' debug xwhite ',xwhite
      call xset_color(xwhite)
      call fillbox(ix1,iy1,ix2,iy2)
      call xset_color(xblack)
      call xmovabs(ix1,iy1)
      call xdrwabs(ix2,iy1)
      call xdrwabs(ix2,iy2)
      call xdrwabs(ix1,iy2)
      call xdrwabs(ix1,iy1)
c
c  two calls below do not work in some circumstances so replaced
c  by above  jh jan 2013
c
c     call clearwindow(int(ix1),int(iy1),int(ix2),int(iy2))

c     call DrawBox(int(ix1),int(iy1),int(ix2),int(iy2))

c

      call xset_color(6)



c

	call xchars(question,nq,real(ix+5),real(iy+7))



c   move to end of question

c

      ix=ix+nq*11+30

	ia=0



c    clear answer



c      j=1 

c	do while ( (answer(j:j) .ne. ' ') .and. (answer(j:j) .ne. char(0)) 

c     * .and. (j .le. na))

c	 j=j+1

c	enddo

	do i=1,na

	   answer(i:i) = ' '

	enddo

c	call xchars(text,na,real(ix+5),real(iy+7)) 

cc	do j=1,na

cc	 text(1:1) = answer(j:j)

cc	 if (text(1:1) .ne. ' ') then

cc	 call xchars(text,1,real(ix),real(iy+7)) 

cc	 ix=ix+10

cc	 ia = ia + 1

cc	 endif

cc	enddo 

  

c

c   go in a loop until a CR is hit 

c

        flagMouse = 0

	do while((ichar(key).ne.13) )

c	     dummy =setcolor(2)   ! background for text, blank

cc	     dummy =setcolor(color_box)   ! background for text, blank

c		 dummy=setcolor(4)

cc         dummy=setcolor(color_box_letter)

 50      continue

	   
 	   call DrawBox(ix,iy+4,ix+6,iy+21) ! draw cursor

	   
c

c	

         call xscursr(j,xmouse,ymouse)

         kchar = j

         if (j .eq.32)  flagMouse = 1 !global flag defined in winplot.for

         if (j .eq. 13) flagkey = 1   !activate key or mouse event
	   
         key = char(j)

cc		 call moveto(ix,iy,xy)

c

c   check for BS or DEL in which case cursor is moved back one char and the

c   character already put out is blanked out

c

c         write(1,'(1X,I5,1x,A20)') IA,ANSWER(1:20)

		 if((ichar(key).eq.8) .or. (ichar(key).eq.127)) then     

	      if(ia.eq.0) goto 50

		    ix=ix-11

c			dummy=setcolor(2)    ! set color for blanking out

cc			dummy=setcolor(color_box)    ! set color for blanking out

                    call clearwindow(ix,iy,ix+21,iy+22)

cc		    dummy=setcolor(color_box_letter)    ! color back for characters

cc			call moveto(ix,iy,xy)

            if(ia.gt.0) answer(ia:ia)=' '   ! blank out deleted char

               ia=ia-1

cc               na=ia

			goto 50

		 endif

c

c   plot out the character entered

c

	if((ia.lt.nchar) .and. (ichar(key).ne.13)) then

	text(1:1) = key

 	call clearwindow(ix,iy,ix+11,iy+22) 

	call xchars(text,1,real(ix),real(iy+7))

         ix=ix+11

cc		 call outgtext(key)

	  ia=ia+1

         answer(ia:ia)=key

cc        na=ia

	endif

	  enddo

c

 99   continue

c

c   reset color

c

c	  dummy=setcolor(2)

c

	ix = int(x)

	iy = int(y)

        call clearwindow(ix,iy-6,ix+(na+nq)*11+45,iy+28)

	flag = 0

cc        na = ia

      return

      end

  
     

