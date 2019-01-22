c--------------------------------------------------------------------------
c program for plotting moment vs arrival time for data in an s-file
c--------------------------------------------------------------------------c
c
c  For detail on parameters and variables naames, see rea.inc
c
c  To compile this program, use command make sample_read_write_s on sun
c  and nmake sample_read_write_s on pc
c
c  updates
c  feb 17 2014 jh: read seisan.def
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock
      include 'seiplot.inc'               ! seisan graphics

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile(2)              ! input file

      character*1 cha(10)                 ! dummy for xy_plot
      character*80 text                   ! general text
      character*80 txt(100)
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      real xc,yc
      real tor,slope,corr,rms             ! for lsq analysis 
      real xmin                           ! min time
      character*5 stat(5000)              ! station
      real av_f0                          ! average corner frequency
      real q                              ! Q

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer nmom			  ! number of stats with moment
      real x(5000),y(5000)                ! values to plot, time and moment
      real mw(5000)                       ! mw
      real xx,yy                          ! help variabels
      integer nars                        ! number of arguments
      integer i,l,k                       ! counter

c
c   check if input via argument
c
      call get_arguments(nars,infile)
      if(nars.lt.1) then
         write(6,*)'File name'
         read(5,'(a)') infile(1)
      endif
      
      call get_seisan_def    
c
c   get input file name, check if exist
c

      open(1,file=infile(1),status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file', infile(1)
      stop
 11   continue
c
      all=.true.                  ! read all parameters
      xmin=9999999.

c
c   read all parameters for one event form file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file 
c
      if(code.eq.1) stop

c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)

c
c---------------------------------------------------------
c   select all traces with s-wave spectral values, print
c   on screen station, time and log moment
c----------------------------------------------------------
c
      nmom=0
      write(6,'(a)')' STAT  COMP HHMM   SEC MOMENT  MW'
      do i=1,rea_nphase
          if(rea_phase(i)(1:5).eq.'SPECS') then
              if(rea_moment(i).lt.-10.0) rea_moment(i)=0.0
              if(rea_mw(i).lt.-10.0) rea_mw(i)=0.0
              write(6,'(1x,a4,1x,a5,1x,2i2,f6.1,1x,f6.2,f4.1)')
     *        rea_stat(i), rea_comp(i), rea_hour(i),
     *        rea_min(i),rea_sec(i),
     *        rea_moment(i),rea_mw(i)
              if(rea_moment(i).gt.0.0) then
                 nmom=nmom+1
                 x(nmom)=rea_hour(i)*3600.0+rea_min(i)*60.0+rea_sec(i)  ! time of window
                 if(x(nmom).lt.xmin) xmin=x(nmom)
                 y(nmom)=rea_moment(i)
                 stat(nmom)=rea_stat(i)
                 mw(nmom)=rea_mw(i)
                 av_f0=av_f0+rea_cornerf(i)
              endif
          endif
      enddo
c
c  reduce times 
c
      k=xmin/3600
      do i=1,nmom
        x(i)=x(i)-k*3600.0
      enddo
     
      if(nmom.lt.2) then
         write(6,*)' Too little data'
         stop
      endif
      av_f0=av_f0/nmom

      write(6,'(a,f6.1)')'Average corner frequency ',av_f0

c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='moment.eps',status='unknown')
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
c   make least squares analysis
c
         call lsqlin(nmom,x,y,tor,slope,corr,rms)
c

c   calculate Q
c
         q=-3.1416*log10(2.71)*(av_f0/3)/slope
         write(6,'(a,f7.0)')'qcorr=',q

         title(1:20)='Mo versus time      '
         write(title(21:80),'(a,f7.0,a,f6.1)')
     *   'Qcorr=',q,' at average f0/3 ',av_f0/3
c
c   plot points
c

          xtext='Time(sec)'
          ytext='Log moment'
          call xy_plot
     *    (1,nmom,x,y,title,xtext,ytext,
     *    600.0,600.0,100.0,100.0,1,1,20.0,
     *    0,cha,i,x,y)	   

         
             
c  plot line
c
          call xy_plot_line(slope,tor,100.0,100.0)
          txt(1)='Select station by'
          txt(2)='clicking near symbol'
          txt(3)='q to quit'
          call xmessage(txt,3,25,720.0,700.0)

c
c   plot all stations
c
          i=1
          l=2
          txt(1)='STAT MOMENT  MW    STAT MOMENT  MW  '
 554      continue
          txt(l)=' '
          do k=1,2
             if(k.eq.1) then
                write(txt(l)(1:17),'(a5,1x,f5.2,1x,f3.1)')
     *          stat(i),y(i),mw(i)
                i=i+1
                if(i.gt.nmom) goto 555
             endif
             if(k.eq.2) then
                write(txt(l)(20:36),'(a5,1x,f5.2,1x,f3.1)')
     *          stat(i),y(i),mw(i)
                i=i+1
                l=l+1
                if(i.gt.nmom) goto 555
                goto 554
             endif
          enddo
 555      continue
          if(k.eq.2) l=l-1

          call xmessage(txt,l,36,710.0,10.0)

c
c  call up cursxor so plots remains
c
 
 30       continue

          call xy_plot_input(100.0,100.0,cha(1),xx,yy,xc,yc)
c
c   find corresponding station
c
          txt(1)=' '
          txt(1)='Too far from symbol,'
          txt(2)='try again'
          txt(3)=' '
          do i=1,nmom
              if(abs(xx-x(i))*xc.lt.10.0.and.   ! distance in pixels
     *           abs(yy-y(i))*yc.lt.8.0) then
                 txt(1)(1:25)=stat(i)
                 txt(2)(1:25)=' '
                 txt(3)=' '
              endif
          enddo
          call xmessage(txt,3,25,720.0,700.0)
          if(cha(1).ne.'q') goto 30   ! next input
c
      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine xy_plot_input(x0,y0,c,x,y,xc,yc)
c
c  reads positions from the screen much like the main routine
c  all scaling values from routine xy_plot via common
c  x0,y0 are lower left hand corner of plot
c
c  x0,yo : same input as main routine
c  x,y   : output in scaled units
c  c     : character pressed
c  xc    : xscale
c
       implicit none
       character*1 c
       integer ichar
       reaL xc,yc   ! same as x and yscale
       real x1,x,y1,y   ! help variables
       real xfirst,yfirst,x0,y0,xscale,yscale,ymin,ymax,xmin,xmax   ! see xy_plot
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax
c
c   call up cursor
c

1           continue
            call xscursr(ichar,x1,y1)
            if (char(ichar).eq.'#') goto 1
            c=char(ichar)
            x=(x1-x0)/xscale+xfirst
            y=(y1-y0)/yscale+yfirst
            xc=xscale
            yc=yscale
c
      return
      end

