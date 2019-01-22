c--------------------------------------------------------------------------
c  select focal parameter from an s-file and calculate t and p
c  and more:
c
c  - different outputs like sorting polarities
c  - plot all p amnd t
c  - plot all mechnisms
c  - plot rose diagram of P and T
c--------------------------------------------------------------------------c
c 
c
c  For detail on parameters and variables naames, see rea.inc
c
c  dec 17 2010 jh: gfortran on pc: remove winplot, put in implitict none,
c                  remvoe hctype, unit check, pc check
c  jan 11 2011 jh: error in system call to slick
c  jan 12 2011 jh: add strike, dip and rake to plot
c  feb 25 2011 jh: fix PostScript scaling
c  mar    2011 jh: add polar plot
c  apr 21 2011 jh: add polar plot for x
c  may 29 2013 jh: stop if no fps
c  feb 17 2014 jh: read seisan.def
c  feb 24 2014 jh: fix dim in making bins for rose diagram, 
c                  plot up to 3 digit numbers 
c  mar 3  2014 jh: fix rose plot for linux, completely wrong
c  2015.06.03 pv : small change due to compiler warning on GOTO
c  2017.01.06 jh : new argument to plot_foc
c
      implicit none              ! 
      include 'seidim.inc'       ! dimensions for rea block
      include 'rea.inc'          ! parameter common bliock
c

      include 'seiplot.inc'      ! seisan graphics
      real x,y                   ! x-y posiiton
      character*80 text          ! general text
      character*80 answer        ! answer
      integer ich                ! mouse return character
c
      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file
      integer kpol_stat_c(max_data/2)     ! count compressions
      integer kpol_stat_d(max_data/2)     ! count dilatatation
      integer kpol                        ! polarity station index
      integer kpol_stat                   ! number of stations with polarity
      character*5 pol_stat(max_data/2)    ! stations with polarity
      real x0,y0,r                        ! for plotting circles

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer pol_max,pol_min             ! count max vs min polarities
      integer nevent                      ! number of events in file
      integer i,j,k                       ! counter
      character*5 quality                 ! fps quality
      logical ok                          ! ok if event selected
      real s1(6),s2(6),s3(6),s(3,6)       ! stress directions for slick
      real smin,smax                      ! for --------------
      integer imin,imax,izero             ! for sorting ------
c
c   for xy plot
c
      character*1 cha(10)
      real cx(10),cy(10)
      integer npoint(10)
      character*30 xtext,ytext
      character*80 title
c
      real PTTP(4)                        ! strike and dip of P and T
      integer ipttp(4)
      real ANGS(3)                        ! dip, strike and rake of principal fault plane
      real ANGS2(3)                       ! dip, strike and rake of auilary ------------
      real ANBTP(6) 
      real momten(6)
      real sldata(5)                      ! data in slick file
      real fit_ang(5000)                  ! fit angle
      real xx(5000)                       ! x-axis
      integer nfit                        ! numberr of data for fit angle
      logical cum                         ! if true, plot cumulative misfit
      real pi,degrad
      integer nfoc                        ! number of fault plane solutions converted
      real t(2,5000)                      ! t-axis
      real p(2,5000)                      ! p-axis
      real azimuth(360)                   ! for rose digrams
      real t_rose(360),p_rose(360)        ! for rose diagram
      integer nbin                        ! bin size (deg) for rose diagram
      integer nplot                       ! coubt number of fps plots for page change
      character*1 plot_all                ! if y, plot all fps selected
      logical pc,sun,linux

      call computer_type(sun,pc,linux)
      call get_seisan_def
      PI = 4.0*ATAN(1.0)
      degrad=180.0/pi
c
      do i=1,max_data/2
         kpol_stat_c(i)=0
         kpol_stat_d(i)=0
      enddo
      kpol_stat=0

      x0=95.0
      y0=690.0
      nplot=0
c
c
c   open output file

       open(2,file='foc.out',status='unknown')
       open(3,file='foc_events.out',status='unknown')
       open(4,file='foc_pol.out',status='unknown')
       open(7,file='foc.zmap',status='unknown')
       open(8,file='foc.slick',status='unknown')
 

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
      open(65,file='foc.eps',status='unknown')

c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
c   give quality
c
      write(6,*) 'Quality, ABC.., up to 5 chars, enter for all'
      read(5,'(a)') quality
      write(6,*)'Cumulative(c) or individual misfit(def)'
      read(5,'(a)') text
      if(text(1:1).eq.'c') then
         cum=.true.
      else
         cum=.false.
      endif
c
c   bin size
c
      write(6,*) 'Bin size for rose diagram (def 10 deg)'
      read(5,'(a)') text
      if(text.eq.' ') then
        nbin=10
      else
        read(text,*) nbin
      endif
      write(6,*) 'Plot all solutions selected (Y=enter/n) '
      read(5,'(a)') plot_all
      if(plot_all.eq.' ') plot_all='y'
c

c
c
c   open plotter (display and initial output in postscript file)
c
      call open_display 
c
c set some postscipt scalings
c
      write(65,*) ' 1.0 0.55 scale'
c
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter
      nfoc=0
      write(2,'(a)')'  Str  P  Dip  P   St  T  Dip  T'
      write(8,'(a)')'Strik dp     Dip    Rake'
c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1      
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)
c
c
c----------------------------------------------------
c  check if fault plane solution 
c----------------------------------------------------
c
      degrad=57.3

c
      if(rea_nfault.gt.0) then
c
c   check quality
c         
          ok=.false.    
          if(quality.ne.' ') then
             do i=1,5
                if(rea_fault(1)(78:78).ne.' '.and.
     *          rea_fault(1)(78:78).eq.quality(i:i)) ok=.true.
             enddo
          else
             ok=.true.
          endif
          if(.not.ok) goto 50    ! skip solution
c
c   read first solution found and accepted, is strike dip and rake
c
          read(rea_fault(1),'(3f10.1)') angs(2),angs(1),angs(3)
          

          if(plot_all.eq.'y'.and.char(ich).ne.'q') then
c
c   plot solution, use number in input file, position plot at x0, y0
c
             r=75.0
          
             call plot_foc(angs(2),angs(1),angs(3),x0,y0,r,0)
             nplot=nplot+1
c
c   plot info
c
             text=' '
             text(1:1)=rea_fault(1)(78:78)  ! type of program
             write(text(3:4),'(i2)') nevent ! event number
             write(text(5:16),'(3i4)')nint(angs(2)), ! fps
     *       nint(angs(1)),nint(angs(3))
             call  xchars(text,17,x0-r-5.0,y0-r-19.0)  
c
c   plot quality
c
c             text=' '
c             text(1:1)=rea_fault(1)(78:78)
c             call  xchars(text,1,x0-60.0,y0-r-19.0)

             x0=x0+2*r+18.0

             if(x0.gt.1000.0-r/2.0) then
                x0=95.0
                y0=y0-2.0*r-40.0
             endif
c
c   plot strike, dip and rake
c
             
c
c   call up cursor to get next page,  first give info text
c
             text='f to continue, q to quit fps plot'
             if(nplot.eq.24) then
 102            continue
                call  tchars(text,80,20.0,20.0)  ! this only comes on screen
                call xscursr(ich,x,y)
                if(char(ich).eq.'f'.or.char(ich).eq.'q') then
                   x0=95.0
                   y0=690.0
                   call clear_to_alpha
                   call close_post

                   call open_display
c
c set some postscipt scalings
c
                   write(65,*) ' 1.0 0.55 scale'

                   nplot=0
                else
                  goto 102
                endif
             endif
          endif

c
c   convert to P and T
c
          do i=1,3
            angs(i)=angs(i)/degrad
          enddo

c
       	  call DSRIN (ANGS,ANBTP,ANGS2,PTTP,momten,PI)
          do i=1,3
            angs2(i)=angs2(i)*degrad
          enddo
          do i=1,4
            ipttp(i)=pttp(i)*degrad+0.5
          enddo
c
c   write p and T
c
          write(2,'(4i8)')ipttp

c                    
c  count selected events
c
          nfoc=nfoc+1
c
c   save T and P
c
          p(1,nfoc)=ipttp(1)
          p(2,nfoc)=ipttp(2)
          t(1,nfoc)=ipttp(3)
          t(2,nfoc)=ipttp(4)
c
c   sum polarities for each station
c
          do i=1,rea_nphase
             if(rea_polarity(i).eq.'D'.or.rea_polarity(i).eq.'C') then
c
c   find station index, make new if not saved
c
                 if(kpol_stat.eq.0) then        ! first station
                    kpol_stat=1
                    pol_stat(1)=rea_stat(i)
                 endif
c
                 kpol=0
                 do j=1,kpol_stat
                    if(pol_stat(j).eq.rea_stat(i)) then
                       kpol=j
                    endif
                 enddo
                 if(kpol.eq.0) then         ! found new station
                    kpol_stat=kpol_stat+1
                     kpol=kpol_stat
                    pol_stat(kpol)=rea_stat(i)
                 endif
c
c  count polarity for that station
c
                  if(rea_polarity(i).eq.'C') kpol_stat_c(kpol)=
     *            kpol_stat_c(kpol)+1 
                  if(rea_polarity(i).eq.'D') kpol_stat_d(kpol)=
     *            kpol_stat_d(kpol)+1 
              endif
           enddo

c    
c   write selected event
c
c
       call rea_event_out(3,all,data,code)
c
c   write data for zmag, lon,lat y m d mag depth hr min st dip rak
c
        x=angs(2)*degrad+90.0  ! uses direction of dip
        if(x.gt.360.0) x=x-360.0
        write(7,'(2f8.3,1x,i4,1x,2i3,f5.1,f6.1,2i3,3f8.1)')

     *  hyp_lon(1),hyp_lat(1),hyp_year(1),hyp_month(1),
     *  hyp_day(1),hyp_mag(1,1),
     *  hyp_depth(1),hyp_hour(1),hyp_min(1),x,
     *  angs(1)*degrad,angs(3)*degrad
c
c   write for slick
c
        write(8,'(3f8.1)') x,angs(1)*degrad,angs(3)*degrad
      endif
c
c   get next event
c
      goto 50

c-------------------------------------------------------------
c     end of file
c-------------------------------------------------------------
c

 1000 continue
c
c   write polarity data
c
      pol_max=0
      pol_min=0

      write(4,'(a)')'Stat     C    D'
      do i=1,kpol_stat
         write(4,'(a5,2i5)') pol_stat(i),kpol_stat_c(i),kpol_stat_d(i)
c
c   group in min and max
c
         if(kpol_stat_c(i).ge.kpol_stat_d(i)) then
            pol_max=kpol_stat_c(i)+pol_max
            pol_min=kpol_stat_d(i)+pol_min
         else
            pol_max=pol_max+kpol_stat_d(i)
            pol_min=pol_min+kpol_stat_c(i)
         endif
      enddo
      write(4,*) 'Sum of maximum number of polarities', pol_max
      write(4,*) 'Sum of minimum number of polarities', pol_min

      if(nfoc.eq.0) then
         write(6,*)' No fault plane solutions'
         stop
      endif
c
c          finished plotting mechanisams, to go to p and t
c
c
c   call up cursor,  first give info text
c
      text='f to continue, q to stop'

      if(plot_all.eq.'y'.and.char(ich).ne.'q') then
 101     continue
         call  tchars(text,80,20.0,20.0)  ! this only comes on screen
         call xscursr(ich,x,y)
         if(char(ich).eq.'q') goto 200
         if(char(ich).eq.'f') then
            continue
         else
            goto 101
         endif   
         call clear_to_alpha
         call close_post

         call open_display
c
c set some postscipt scalings
c
         write(65,*) ' 1.0 0.55 scale'

      endif
   

c
c   plot T
c
c  make circle
c
      x0=250
      y0=290
      r=240
      call draw_circle(x0,y0,r)

      do i=1,nfoc
         y=r*cos(t(1,i)/degrad)*cos(t(2,i)/degrad)+y0
         x=r*cos((90.0-t(1,i))/degrad)*cos(t(2,i)/degrad)
     *     +x0
c        call draw_triangle(x,y,10.0)
         write(text,'(i3)') i
         call  tchars(text,3,x-10.0,y)
          
      enddo
      text(1:1)='T'
c
      call  tchars(text,1,x0,10.0)  

c
c  PLOT p
c
      x0=740
      call draw_circle(x0,y0,r)

      do i=1,nfoc
         y=r*cos(p(1,i)/degrad)*cos(P(2,i)/degrad)+y0
         x=r*cos((90.0-p(1,i))/degrad)*cos(p(2,i)/degrad)
     *     +x0
c         call draw_triangle(x,y,10.0)
         write(text,'(i3)') i
         call  tchars(text,3,x-10.0,y)
      enddo

      text(1:1)='P'
      call  tchars(text,1,x0,10.0)
c
c
c   run slick
c
      close(8)
      call systemc('slick foc.slick',15)
c
c  read  s1 s2 and s2
c
      open(8,file='foc.slick.oput',status='old')
      
 51   continue
      read(8,'(a)',end=55) text
      if(text(1:10).eq.'eigenvalue') then
c
c  index 1 is eigen value, 2-4 is eigen vector and 5-6 strike and dip of vector
c
         read(8,*,end=55) (s(1,i),i=1,6)    
         read(8,*,end=55) (s(2,i),i=1,6)    
         read(8,*,end=55) (s(3,i),i=1,6)    
         goto 60
      endif
      goto 51
 55   continue
      write(6,*)' No stess data'
      goto 65
 60   continue
c
c  find larges and smallest
c
      smin=100000.0
      smax=-smin
      do i=1,3
         if(s(i,1).lt.smin) then
            smin=s(i,1)
            imin=i
         endif
         if(s(i,1).gt.smax) then
             smax=s(i,1)
             imax=i
         endif
      enddo
      do i=1,3
        if(i.ne.imin.and.i.ne.imax) izero=i 
      enddo
c      write(6,*) 'max,min,zero',imax,imin,izero
      
c
c  put in order
c
      do i=1,6
         s1(i)=s(imin,i)     ! P
         s2(i)=s(izero,i)    ! B
         s3(i)=s(imax,i)     ! T
      enddo     
c
c read fit angle
c
      read(8,'(a)',end=63) text
      if(text(1:13).eq.'dip direction') then
         nfit=1
 62      continue
         read(8,*,err=63)sldata
         fit_ang(nfit)= sldata(4)
         xx(nfit)=nfit
         nfit=nfit+1
         goto 62
c63      continue
c        nfit=nfit-1      
      else
         goto 60
      endif    
 63   continue
      nfit=nfit-1      
c
c   plot s1, s2, s3
c
      x0=750

c 
c   plot s1, P
c

      y=r*cos(s1(5)/degrad)*cos(s1(6)/degrad)+y0
      x=r*cos((90.0-s1(5))/degrad)*cos(s1(6)/degrad)+x0
      call draw_triangle(x,y,20.0)


c
c 
c   plot s2, B
c
      y=r*cos(s2(5)/degrad)*cos(s2(6)/degrad)+y0
      x=r*cos((90.0-s2(5))/degrad)*cos(s2(6)/degrad)+x0
      call draw_circle(x,y,20.0)

      x0=260

c 
c   plot s3, T
c

      y=r*cos(s3(5)/degrad)*cos(s3(6)/degrad)+y0
      x=r*cos((90.0-s3(5))/degrad)*cos(s3(6)/degrad)+x0
      call draw_triangle(x,y,20.0)

c
c 
c   plot s2, B
c
      y=r*cos(s2(5)/degrad)*cos(s2(6)/degrad)+y0
      x=r*cos((90.0-s2(5))/degrad)*cos(s2(6)/degrad)+x0
      call draw_circle(x,y,20.0)

 65   continue
      
c
c   plot misfit
c
      npoint(1)=nfit
      xtext='Number'
      ytext='degrees'
      title='slick misfit'
c
c  do cumulative if desired
c
      if(cum) then
      do i=2,nfit
         fit_ang(i)=fit_ang(i)+fit_ang(i-1)
         title='cumulative slick misfit'
      enddo      
      endif   
      call xy_plot
     *(1,npoint,xx,fit_ang,title,xtext,ytext,800.0,140.0,110.0,600.0,2,
     *1,10.0,
     *0,cha,i,cx,cy)
c
 100  continue


c
c   call up cursor,  first give info text
c
      text='q: quit, f: rose digrams'
      call  tchars(text,80,20.0,20.0)  ! this only comes on screen
      call xscursr(ich,x,y)

 
c   check if finish
c
      if(char(ich).eq.'q') goto 200
      if(char(ich).eq.'f') goto 190
      goto 100
c
 190  continue
c
c  plot rose diagrams, first count in nbin deg bins
c
      do i=1,360
        p_rose(i)=0
        t_rose(i)=0
        azimuth(i)=(i-1)*nbin+nbin/2
      enddo
      do i=1,nfoc
         k=(p(1,i)+1.0)/nbin+1
         if(k.eq.361) k=360
         p_rose(k)=p_rose(k)+1
         k=(t(1,i)+1.0)/nbin+1
         if(k.eq.361) k=360
         t_rose(k)=t_rose(k)+1
      enddo
      call clear_to_alpha
      call close_post
      call open_display
c      call xopen_window(2)
      if(pc) then
         call polar_plot_dis(360/nbin,p_rose,t_rose,azimuth,nbin) ! in dislinplot
      else
         call polar_plot_x(360/nbin,p_rose,t_rose,azimuth,nbin)
      endif
c
c
c   call up cursor,  first give info text
c
 150  continue
      text='q to terminate'
      call  tchars(text,80,20.0,20.0)  ! this only comes on screen
      call xscursr(ich,x,y)

c 
c   check if finish
c
      if(char(ich).eq.'q') goto 200      
      goto 150

 200  continue
c
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

      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Number of fault plane solutions converted',nfoc
      write(6,*) 'Number of stations with polarity',kpol_stat
      write(6,*) 'Output file name with P and T is foc.out'
      write(6,*) 'Output file name with polareties is foc_pol.out'
      write(6,*) 'Output filename with events is foc_events.out'
      write(6,*) 'Output filename for zmap is foc.zmap'

      stop
      end

      subroutine polar_plot_x(n,pnumber,tnumber,azimuth,nbin) 
c
c   makes a polar plot
c
c      n: number of bins
c      pnumber: number of p values in each bin
c      tnumber: ---------t------------------
c      azimuth: azimuth values for each bin
c      nbin: bin size in degrees      

      IMPLICIT NONE
      integer i,n,max,k
      real az
      character*80 text
      real pnumber(*),tnumber(*),azimuth(*)
      real x,y   ! work variables
      integer nbin
      real xbin, x0,y0,r

      x0=300.0
      y0=450.0
      r=150.0
c
c   T
c
      call draw_circle(x0,y0,r)
c
c   find max number in a bin
c
      max=0
      do i=1,n
        if(tnumber(i).gt.max) max=tnumber(i)
      enddo
c
c  draw a line for each bin, proportinal to number
c
      do i=1,n
        do k=-nbin*4,nbin*4
           az=azimuth(i)+float(k)/8.0
           x=x0+r*(tnumber(i)/max)*sin(az/57.3)
           y=y0+r*(tnumber(i)/max)*cos(az/57.3)
           call xmovabs(x0,y0)
           call xdrwabs(x,y)
        enddo
      enddo
      text='T'
      call  tchars(text,1,x0-5.0,y0+r+20.0) 

c
c  P
c
      x0=700.0
      call draw_circle(x0,y0,r)
c
c   find max number in a bin
c
      max=0
      do i=1,n
        if(pnumber(i).gt.max) max=pnumber(i)
      enddo
c
c  draw a line for each bin, proportinal to number
c
      do i=1,n
        do k=-nbin*4,nbin*4
           az=azimuth(i)+float(k)/8.0
           x=x0+r*(pnumber(i)/max)*sin(az/57.3)
           y=y0+r*(pnumber(i)/max)*cos(az/57.3)
           call xmovabs(x0,y0)
           call xdrwabs(x,y)
        enddo
      enddo

      text='P'
      call  tchars(text,1,x0-5.0,y0+r+20.0)
      return
      end

