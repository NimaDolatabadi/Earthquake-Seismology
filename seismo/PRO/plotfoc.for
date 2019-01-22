c--------------------------------------------------------------------------
c  plot focal mechanisms for one event, no polarities plotted
c  file name is first argument or input is asked for if no argument
c  jh january 2017
c--------------------------------------------------------------------------
c 
c
c  For detail on parameters and variables names, see rea.inc
c

c
      implicit none               
      include 'seidim.inc'       ! dimensions for rea block
      include 'rea.inc'          ! parameter common block
      integer nars               ! number of arguments
      character*80 args(5)       ! arguments
c
      include 'seiplot.inc'      ! seisan graphics
      real x,y                   ! x-y posiiton
      character*100 text         ! general text
      integer ich                ! mouse return character
c
      character*80 data(10000)   ! s-file with data in text array
      character*80 infile        ! input file
      real x0,y0,r               ! for plotting circles

      logical all                ! true: read all data, false: headers
      integer code               ! error return code

      integer i,j,k,kpos,lpos    ! counter
      integer color_plane        ! color of fault plane
      character*5 quality        ! fps quality
      integer npol               ! number of polarities
      real gap                   ! gap in fps angles

      real ANGS(3)               ! dip, strike and rake of principal fault plane
    
      real pi,degrad
 
      call get_seisan_def

      PI = 4.0*ATAN(1.0)
      degrad=180.0/pi
c
c   get arguments
c
      call get_arguments(nars,args)
c
c   size and position of fps
c      
      r=300.0
      x0=600.0
      y0=350.0   
c
c   set size in % of sreeen size of plot, only for unix
c
      wsize=80         ! common block variable
c
c   assign file unit for postscriptt file
c
      plotunit=65      ! common block variable

c
c  set defaults for output: 0: screem, 1: screen + file, 2: only file
c
      plotoption=1     ! common block variable
c
c   open postscript output file
c
      open(65,file='plotfoc.eps',status='unknown')

c
c   get input file name, check if exist
c
      if(nars.eq.0) then
 9       continue
         write(6,*) 'Give input file'
         read(5,'(a)') infile
         open(1,file=infile,status='old',err=10)
         goto 11
 10      continue
         write(6,*)' No such input file'
         goto 9
 11      continue
      else
         infile=args(1)
         open(1,file=infile,status='old',err=12)
         goto 13
 12      continue
         write(6,*)' No such file'
         stop
 13      continue
      endif


c   open plotter (display and initial output in postscript file)
c
      call open_display 
c
c set some postscipt scalings
c
      write(65,*) ' 1.0 0.55 scale'
c
      all=.true.                  ! read all parameters
      kpos=0
      lpos=0

 20   continue
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so end
c
      if(code.eq.1) goto 1000
c

c----------------------------------------------------
c  check if fault plane solutions 
c----------------------------------------------------
c
      if(rea_nfault.gt.0) then
c
c   get gap and number of polarities if possible
c
             call fps_gap(npol,gap)
c
c   plot header, number of polarities and gap
c
         lpos=lpos+1
         text=' '
         text(1:78)=data(1)(2:79)
         text(80:84)='npol='
         write(text(85:88),'(i4)') npol
         text(90:93)='gap='
         i=gap+0.5
         write(text(94:96),'(i3)') i

         call  xchars(text,96,200.0,770.0-lpos*15.0) 
c
c   read solutions 
c
         do k=1,rea_nfault
            read(rea_fault(k),'(3f10.1)') angs(2),angs(1),angs(3)        
c
c   plot solution,  position plot at x0, y0
c

c
c  set color
c
             if(k.lt.4) then
                color_plane=k
             else
                color_plane=6    ! black
             endif
             call plot_foc(angs(2),angs(1),angs(3),x0,y0,r,color_plane)

c          
c
c   plot info
c
             call xset_color(color_plane)
             text=' '             
             write(text(1:12),'(3i4)')nint(angs(2)), ! fps
     *       nint(angs(1)),nint(angs(3))
             text(14:26)=rea_fault(k)(67:79)
             kpos=kpos+1
             call  xchars(text,26,5.0,760.0-kpos*20.0) 
           enddo     
      
           text=' '     
           if(nars.le.1) then
           text=
     *     'q to quit, f to next event or s to next event superimposed'
           else
              text='q to quit'
           endif
           call  tchars(text,80,20.0,20.0)  ! this only comes on screen

           call xscursr(ich,x,y)
           if(char(ich).eq.'f'.or.char(ich).eq.'s') then   ! next event
              if(char(ich).eq.'f') then
                 call clear_display
c
c    clear couters for position of texts
c
                 kpos=0
                 lpos=0
              endif
              goto 20
           endif

           if(char(ich).eq.'q') then
           call clear_to_alpha
           call close_post
         endif   
      else
          write(6,*)'No fault plane solution in file'
          goto 20
      endif

c-------------------------------------------------------------
c     end of file or data
c-------------------------------------------------------------
c

 1000 continue

      stop
      end