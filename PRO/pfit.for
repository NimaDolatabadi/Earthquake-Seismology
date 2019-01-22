c--------------------------------------------------------------------------
c  calculates the best fit of a plane wave to a set of stations so the
c  stations are assuemd to act as a seismic array. the program uses one
c  s-file
c--------------------------------------------------------------------------c
c
c   updates
c
c aug 20 2008 jh: add q to quit to plot
c dec 28 2010 jh: gfortran on pc: remove winplot, implicit none, remove hctype,
c                 unit check, computer type check
c feb 17 2014 jh: read seisan.def   
c 2015.06.03 pv : moved line 9 continue, due to compiler warning on GOTO
c 2016 10 25  jh: fixed color of points plotted, fixed so no crash if
c                 reference station appears more than one time.
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block
      include 'version.inc'
      include 'libsei.inc'

      include 'seiplot.inc'               ! seisan graphics
      character*1 cha(10)                 ! dummy for xy_plot

      character*1 make_plot

      character*80 text(1000)             ! general text
      character*80 txt(100)
      character*80 title                  ! title for plot
      character*30 xtext,ytext            ! axis title
      character*80 data(5000)             ! s-file with data in text array
      character*80 infile                 ! input file
      character*5  stat_p(100)            ! stations with p
      real*8       time_p(100)            ! abs arrival time
      real*8       time1_p                ! temporary of above
      real stat_x(100),stat_y(100)        ! cartesian station coordinates
      real x(100),y(100)                  ! work array
      real stat_dist(100)                 ! distance from reference station
      character*5 ref_stat                ! reference station
      integer k_ref                       ! index of reference station
      real max_dist                       ! maximum distance from ref stat.
      real px,py                          ! slownes components
      real vapp, baz                      ! apparent velocity and back azimuth
      real corr, rms                      ! correlation coefficient and rms
      character*1  modelc                 ! model flag
      real stlat(100),stlong(100)         ! lat lon of selected stations
      real stlat1,stlong1                 ! temporary of above
      real stelev(100)                    ! elevation ------------------
      real xm,ym                          ! cursor input
      real xc,yc                          ! scaling factors

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer il                          ! line counter
      integer i,k,n,m                     ! counters

      call get_seisan_def
c
c   start writing from line 1
c
c   check if input via argumants
c
      call get_arguments(n,infile)
c
 9    continue
      if(n.lt.1) then
c
c   get input file name, check if exist
c
         write(6,*) 'Give input file'
         read(5,'(a)') infile
      endif

      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      write(6,*) 
     *' Give reference station, enter for using first station in list'
      read(5,'(a)') ref_stat
      write(6,*) 'Maximum distance from reference station',
     *', default is 1000 km'
      read(5,'(a)') txt(1)
      if(txt(1).eq.' ') then
         max_dist=1000.0
      else
         call sei get values(1 , txt(1), code )
         max_dist=array$(1)
      endif
c
      all=.true.                  ! read all parameters
c
c   open output file
c
      open(2,file='array.out',status='unknown')  
c
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so terminate program
c
      if(code.eq.1) goto 1000
c
c   write the whole first header line
c
      il=2
      write(6,'(a)') data(1)(1:79)
      write(text(il),'(a)') data(1)(1:79)
      il=il+1
c
c count P-phases and find coordinates, only first arrival are used
c

      k=1
      modelc=' '
      do i=1,rea_nphase
        if(rea_phase(i)(1:2).eq.'P '.or.
     *     rea_phase(i)(1:2).eq.'Pg'.or. 
     *     rea_phase(i)(1:2).eq.'PG'.or. 
     *     rea_phase(i)(1:2).eq.'Pn'.or. 
     *     rea_phase(i)(1:2).eq.'PN'.or. 
     *     rea_phase(i)(1:3).eq.'PKP'.or. 
     *     rea_phase(i)(1:2).eq.'Pb'.or. 
     *     rea_phase(i)(1:2).eq.'PB') 
     *   then
           time_p(k)=rea_abs_time(i)
           stat_p(k)=rea_stat(i)
           call stat_loc(stat_p(k),modelc,stlat(k),stlong(k),
     *     stelev(k))
           if(stlat(k).eq.0.0.and.stlong(k).eq.0.0) then
              write(6,'(1x,a,a)') 
     *        ' Station not found in STATION0.HYP file ',stat_p(k)
           else
              k=k+1
           endif   
        endif
      enddo
      k=k-1
c
c   find reference station
c
      k_ref=0
      do i=1,k
        if(ref_stat.eq.stat_p(i)) then
          k_ref=i
        endif
      enddo
      if(k_ref.eq.0) then
        write(6,*) 
     *  'Reference station not found, will use first station in list'
        k_ref=1
        ref_stat=stat_p(1)
      endif
c
        
c
c   put reference station in index 1
c
      time1_p=time_p(1)  ! save what was in 1
      stlat1=stlat(1)
      stlong1=stlong(1)
c 
      time_p(1)=time_p(k_ref)
      stlat(1)=stlat(k_ref)
      stlong(1)=stlong(k_ref)
      time_p(k_ref)=time1_p
      stlat(k_ref)=stlat1
      stlong(k_ref)=stlong1
      stat_p(k_ref)=stat_p(1)
      stat_p(1)=ref_stat
c
c  make sure reference station only appears once
c
      m=1
      do i=2,k
        if(stat_p(i).ne.stat_p(1)) then
          m=m+1
          stat_p(m)=stat_p(i)
          stlong(m)=stlong(i)
          stlat(m)=stlat(i)
          time_p(m)=time_p(i)
        endif
      enddo
      k=m
c
c   reference to first station with respect to time
c
      do i=2,k
        time_p(i)=time_p(i)-time_p(1)
      enddo 
      time_p(1)=0.0
c
c  calculate xy coordinates with respect to first station
c
      do i=2,k
        stat_x(i)=111.195*cos(stlat(1)/57.296)*
     *  (stlong(i)-stlong(1))
        stat_y(i)=111.195*(stlat(i)-stlat(1))
        stat_dist(i)=sqrt(stat_x(i)*stat_x(i)+stat_y(i)*stat_y(i))
      enddo
      stat_x(1)=0.0
      stat_y(1)=0.0
c
c   select stations within max-dist radius
c
      n=2
      do i=2,k
        if(stat_dist(i).le.max_dist) then
          stat_x(n)=stat_x(i)
          stat_y(n)=stat_y(i)
          time_p(n)=time_p(i)
          stat_p(n)=stat_p(i)
          stlong(n)=stlong(i)
          stlat(n)=stlat(i)
          n=n+1
        endif
      enddo
      n=n-1
      write(6,*)
      write(6,'(a,i3,a,i3)')
     *' Stations available:',k,'  Stations used:',n
      write(text(il),'(a,i3,a,i3)')
     *' Stations available:',k,'  Stations used:',n
 
      il=il+1

      k=n

      write(6,'(2a)') ' Stat      Delta t  Latitude',
     *' Longitude         x         y'
      write(text(il),'(2a)') ' Stat      Delta t  Latitude',
     *' Longitude         x         y'
      il=il+1
      do i=1,k
         write(6,'(1x,a5,2x,5f10.3)') 
     *   stat_p(i),time_p(i),stlat(i),stlong(i),
     *              stat_x(i),stat_y(i)
         write(text(il),'(1x,a5,2x,5f10.3)') 
     *   stat_p(i),time_p(i),stlat(i),stlong(i),
     *              stat_x(i),stat_y(i)
         il=il+1
      enddo
   
      if(k.lt.3) then
        write(6,*)
        write(6,*) ' Too few stations available, will stop'
        write(6,*) ' Enter to continue'
        read(5,'(a)')  txt(1)
        write(2,*)' No data'
        close(2)
        stop
      endif
c
c   put into x and y variables for least squares
c
      do i=2,k
        x(i-1)=stat_y(i)/stat_x(i)
        y(i-1)=time_p(i)/stat_x(i)
      enddo

      k=k-1

      call lsqlin(k,x,y,py,px,corr,rms)
c
c   calcualte azimuth and apparent velocity
c
      vapp=1.0/sqrt(px*px+py*py)
      baz=atan2(py,px)
      baz=baz*57.2 -180.0
      if(baz.lt.0.0) baz=baz+360.0
c
      write(6,*)
      write(6,'(a,a)') ' Reference station is: ',ref_stat
      write(text(il),'(a,a)') ' Reference station is: ',ref_stat
      il=il+1
      write(6,*)
      write(6,'(a,f5.1,a,f5.2,a,f5.2,a,f6.2)')
     *' Back azimuth =',baz,'  Apparent velocity =',vapp,
     *'  corr =',corr,'  rms = ',rms
      write(text(il),'(a,f5.1,a,f5.2,a,f5.2,a,f6.2)')
     *' Back azimuth =',baz,'  Apparent velocity =',vapp,
     *'  corr =',corr,'  rms = ',rms
      il=il+1
c
c   put baz and app in top of file
c
      write(text(1),'(a,a5,1x,2f10.2)')
     *' Back azi. and apparent velocity  '
     *,ref_stat,baz,vapp
c
c   wrtiew in file
c
      do i=1,il-1
        write(2,'(a)') text(i)
      enddo
      write(6,*) ' Plot relation (y=default/n)'
      read(5,'(a)') make_plot
      if((make_plot.eq.'y') .or. (make_plot.eq.' ')) then
c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='array.eps',status='unknown')
          plotunit=65
          plotoption=1
          wsize=60
c
c   open plotter
c
c
c   set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'

          write(title,'(a,f5.1,a,f6.1,a,f6.3)')
     *   'App. vel=',vapp,'   Back Azimuth= ',baz,
     *    '   corr=',corr
          xtext=' (x-coordinate)/(y-coordinate)'
          ytext='(P-time) / (Stat x-coordinate )     '

c
c   open display
c
        call open_display
c
c   plot points
c
          x(1)=3     ! points in red
          call xy_plot
     *    (1,k,x,y,title,xtext,ytext,
     *    600.0,600.0,100.0,100.0,1,1,20.0,
     *    0,cha,i,x,y)
c
c  plot line
c
          call xy_plot_line(px,py,100.0,100.0)
          txt(1)='Select station by'
          txt(2)='clicking near symbol'
          txt(3)='q to quit'
          call xmessage(txt,3,25,720.0,650.0)
c
c   plot all stations
c
c         i=1
c         l=2
c         txt(1)='STAT PS  S-P  STAT PS  S-P'
c554      continue
c         txt(l)=' '
c         do k=1,2
c            if(k.eq.1) then
c               write(txt(l)(1:13),'(a5,a1,a1,f5.2)')
c    *          stat(i),phase_p(i)(11:11),phase_s(i)(11:11),tsp(i)
c               i=i+1
c               if(i.gt.nwad) goto 555
c            endif
c            if(k.eq.2) then
c               write(txt(l)(16:28),'(a5,a1,a1,f5.2)')
c    *          stat(i),phase_p(i)(11:11),phase_s(i)(11:11),tsp(i)
c               i=i+1
c               l=l+1
c               if(i.gt.nwad) goto 555
c               goto 554
c            endif
c         enddo
c555      continue
c         if(k.eq.2) l=l-1
c         call xmessage(txt,l,28,710.0,10.0)
c

c
c  call up cursxor so plots remains
c
 55       continue
          call xy_plot_input(100.0,100.0,cha(1),xm,ym,xc,yc)
 
c   find corresponding station
 
          txt(1)=' '
          txt(1)='Too far from symbol,'
          txt(2)='try again'
          txt(3)=' '
c          write(27,*) x,y
          do i=1,k
              if(abs(xm-x(i))*xc.lt.10.0.and.   ! distance in pixels
     *           abs(ym-y(i))*yc.lt.8.0) then
                 txt(1)(1:25)='STAT      Relative P '
                 write(txt(2)(1:25),
     *           '(a,5x,f10.3)')stat_p(i+1),time_p(i+1)
                 txt(3)=' '
c                 write(27,*) x,y,stat(i),phase_p(i),phase_s(i)
c                 write(27,*) txt(1),txt(2),txt(3)
              endif
          enddo

         call xmessage(txt,3,25,720.0,655.0)
          if(cha(1).ne.'q') goto 55   ! next input

c          call xscursr(i,xm,ym)

 
c   close postscript
c
          call close_post
          close(2)
c
c   close output plot file
c
          close(65)
c
c   close display and back to alpha screen
c
         write(6,*)
         write(6,*)' Plot file is array.eps'
      endif
 30   call clear_to_alpha

c
c     end of file
c
 1000 continue
c
      write(6,*)            ! blank line
      write(6,*) ' Output file is array.out'


      stop
      end


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
       real xfirst,yfirst,x0,y0,xscale,yscale,ymin,ymax,xmin,xmax ! see xy_plot
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax
c
c   call up cursor
c

   1        continue
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


