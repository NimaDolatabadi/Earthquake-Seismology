c
c   sample program to make graphics
c
c   changes
c
c   june 20 2005 jh : plt to eps
c   dec  28 2010 jh : gfortran on pc: remove winplot, put in implicit none
c                     remove hctype, do not stop at end
c   feb 21  2011 jh : adopt better to dislin
c   feb 22  2011 jh : size from color.def
c   dec 07  2016 jh : add call to get_seisan_def


      implicit none              ! cannot be used on pc due to windows include
      include 'seiplot.inc'      ! seisan graphics
      real x,y,xold,yold         ! x-y posiiton
      character*80 text          ! general text
      character*80 answer        ! answer
      integer ich                ! mouse return character
c
c   get seisan defaults
c
      call get_seisan_def
c
c   open an output file
c
      open(1,file='sample_graphics.out',status='unknown') 
c
c   set default size in % of sreeen size of plot
c
            
      wsize=60         ! common block variable
      call get_window_size
      if(size_sample_graphics.gt.0) wsize=size_sample_graphics ! from color.def
c
c   assign file unit for postscriptt file
c
      plotunit=65      ! common block variable

c
      plotoption=1     ! common block variable
c
c   open postscript output file
c
      open(65,file='sample_graphics.eps',status='unknown')
c
c   open plotter (display and initial output in postscript file)
c
      call open_display
c
c   put a title at 750,700, bottom left corenr is (0,0), max x is 1024,
c   max y is 780.0
c
      text='Sample_graphics program'
c
      call  tchars(text,23,750.0,700.0)  ! this only comes on screen
      call  pchars(text,23,750.0,700.0)  ! this only comes in plot file
c
c  move cursor to (10.0,500.0)
c
      call xmovabs(10.0,500.0)
c
c   draw a line to 300.0, 550.0
c
      call xdrwabs(300.0,550.0)
c
c   draw a text string with a question and get an answer, position 50.0,200.0,
c   the text string has 42 characters and the answer maximum 30
c
      text='Enter a text string, terminate with return'
      call oneline(text,42,answer,30,50.0,300.0)
      write(1,'(a)')' Answer from keyboard:'
      write(1,'(1x,a)') answer      ! write answer to file
      write(1,*)                 ! blank line
      write(1,'(a)')' Character  ASCii value       x      y'
c
c   call up cursor to be moved with the the mouse, when cursor is
c   pushed, the key ascii code is stored in ich and the posiiton in
c   x and y. Enter a loop with mouse where a line is drawn to
c   each mouse click or character press, terminates with character q.
c   mouse click characters and position is output in file.
c
c   first give info text
c
      text='Press mouse or character to mark position, q to terminate'
      call  tchars(text,80,20.0,20.0)  ! this only comes on screenc
c
c   put cursor in midle of screen
c
      call xmovabs(512.0,390.0)
c
c   save old posiiton since we draw from here
c
      xold=512
      yold=390
c
c   start of loop
c
 10   continue
c
c   call up cursor
c
      call xscursr(ich,x,y)
c
c  in dislin, a call to cursor position puts the drawing cursor
c  at the locaiton it has at the call. So drawing to the cursor will just
c  give a point. In X, the drawing cursor remains where is was in previous
c  call. So to get a line from previous click to current click, the drawing
c  cursor must first be moved back to the previous positions, here xold and yold
c
      call xmovabs(xold,yold)
c
c   save for next call
c
      xold=x
      yold=y
c
c   write values in file
c
      write(1,'(9x,a1,7x,i6,1x,2f7.1)') char(ich),ich,X,Y
c
c   check if finish
c
      if(char(ich).eq.'q') goto 20
c
c   draw a line to cursor
c
      call xdrwabs(x,y)
c
c   back to call up cursor again
c
      goto 10
c
c   finished
c
  20  continue

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
c
      call clear_to_alpha
      close(1)           ! close output file
      write(6,*) 'Output file in sample_graphics.out'
      write(6,*) 'Putput graphics file in sample_graphics.eps'
      stop
      end
