
c
c program to: - invert for regional Q Lg 
c             - tomography inversion for QLg
c             - caclulate spectral ratios e.g. Lg/P 
c             - produe input for psxy distance plots
c
c
c by Lars Ottemoeller, Mexico City, July/August 2000
c
c email: lars.ottemoller@.geo.uib.no
c
c

c
c changes:
c
c    Jul  7 2000 lo: work with horizontal components
c    Jul 11 2000 lo: Q tomography started
c    Jul 17 2000 lo: most bugs fixed
c    Jul 25 2000 lo: some more output
c    Jul 27      lo: add apriori values from regional inversion
c    Jul 31      lo: remove selected site terms from G matrix
c    Dec         lo: check if signal clipped
c    Dec 28 2000 lo: only plot traces if data selected for inversion
c    Jan 19 2001 lo: add checkerboard test, which sets synthetic data
c    Jan 01 2001 lo: routines spec_value and check_clipped moved out
c                    use r_m instead of m_2t, since same dim, no overlap,
c                    use h_m instead of m_2s, use h_m instead of rd_m
c    Mar 06 2001 lo: pass number of smooth into spec_values
c -------------------------------------- June 01,2001 moved to s2000
c    Jun 02 2001 lo: tested and fixed under Linux
c                    changed check for min time window
c    Jun 10 2001 lo: fix the damping constants
c    jul 3       jh: take out spec_values_old, crash on pc, wrong arguments
c                    change write01 to write1, did not work on pc
c    may 26 2005 lo: increased dimensions for grid and station number
c    jan 26 2006 lo: add file with lat,lon,slat,slon,ratio
c    feb 03 2007 lo: add fix_site and fix_source
c    mar 06 2007 lo: exclude data if either phase or waveform data have 
c                    uncertain time
c    jan 29 2008 lo: added correction for kappa to spectra
c    feb  8 2008 lo: changed term add to data for perturbation of source
c    dec 30 2009 jh: add paz to call spec_value, not used
c    may 06 2010 lo: change of variable type for f90
c

      program qlg

      implicit none

      include 'libsei.inc'        ! Open file definitions
      include 'mulplt.inc'
      include 'qlg.inc'

      integer seiclen
      character*80 infile         ! waveform input file name
      integer a,i,j,ind,b,k       ! counters
      real sec_max,d_max       ! maximum seconds
c      real dist_min,dist_max         ! distance range
      real x,yy,x1,x2,y1,y2       ! x and y value
      real sum                    ! normalization factor
      real signal_lg(max_sample),signal_p(max_sample),
     &     signal_lg_e(max_sample),signal_lg_n(max_sample),
     &           signal_n(max_sample)  ! signal windows
      complex y_com(max_sample)   ! complex spectrum
      integer nsamp_lg,nsamp_lg_e,nsamp_lg_n,nsamp_p,nsamp_n
c      real max                    ! max amplitude
      real slat,slon,selev        ! station coords
      real lat,lon,dep            ! event coords
      real slat_rad,slon_rad,lat_rad,lon_rad
      real y_scale                ! scale for signal
      logical flag(max_trace)     ! flag
      logical good_signal         ! flag for signal
      character*50 epiline        ! epi info
      character*80 line           ! text
      real gv                     ! group velocity
      character*5 stat(max_trace) ! array with station names
      character*5 total_stat(max_trace) ! array with station names
      integer total_nstat         ! total number of stations
      integer swap_with           ! index
      integer n_lgmax(max_trace)  ! number of group velocitis for stat
      real gv_lgmax(max_trace)    ! group velocity of LG maximum
      real lg_max,lg_gv           ! max of lg amplitude
      integer nstat               ! number of stations
      logical new                 ! flag for new station
      logical clipped             ! true if signal clipped
c      real cbdelta                ! delta +/- apriror value
c      parameter (cbdelta=0.0004)

      real      cof(8)                  ! filter coefficients
      real      gain                    ! filter gain
c      integer npasses                   ! number of passes, 1 forward,

      integer year,month,day,hour,min ! time
      real sec
      double precision msec,rsec
      real pi,rearth,dist(max_trace),dedeg,az0 ! distance
      parameter (pi=3.141593)
c      real lg_low,lg_high,p_low,p_high
      real lg_p_ratio(max_trace)
      integer lg_nsamp,p_nsamp
      real lg_mean,p_mean
      integer path_unit
      integer npath(500)
      integer ipow
c
c   paz for spectrum, not used
c
      complex ppole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros


c      integer nfreq
c      parameter (nfreq=16)              ! number of frequencies
c      real freq_array(nfreq)            ! array of freq.
c      data freq_array /.2,.3,.4,.5,.65,.80,1.,1.25,1.60,2.,2.5,3.15,
c     &      4.,5.,6.3,8./
c      real alpha_array (nfreq)          ! array for Q-1
c
c CA
c
c      data alpha_array /4.56597E-02,1.43133E-02,7.68871E-03,5.18730E-03,
c     & 6.19911E-03,5.64579E-03,5.23922E-03,4.52463E-03,
c     & 4.12399E-03,3.53125E-03,
c     & 2.90813E-03,2.25682E-03,1.70294E-03,1.12286E-03,
c     & 6.17665E-04,2.59072E-04/

c
c Mexico
c
c      data alpha_array /2.78998E-03,2.64024E-03,2.63714E-03,2.91308E-03,
c     & 2.76488E-03,2.63947E-03,2.70083E-03,2.79560E-03,2.79123E-03,
c     & 2.60281E-03,2.38572E-03,2.15875E-03,1.71854E-03,1.33095E-03,
c     & 1.00949E-03,7.05749E-04/

      real alpha_apriori
      real level_lg(max_nfreq),level_p(max_nfreq),level_n(max_nfreq)
      real level_lg_n(max_nfreq),level_lg_e(max_nfreq)
      integer f_choice                  ! frequency choice
      character*1 option,ch    
      character*80 outfile
      integer write1

      integer station_counter

c-- number of stations
c      integer           nstat
c-- number of phases
      integer nphase
c-- number of header lines
c      integer           nhead
c-- number of records for event
c      integer           nrecord
c-- event type
      character*1      type
c-- exp indicator
      character*1       expl 
c-- id line number
      integer id
c event counter
      integer event_id
c response
      INTEGER NFILT,POLE(10)
      REAL FFILT(10)
      common /response/nfilt,pole,ffilt

c
c vectors and matrices for inversion d = G * m
c
c      integer min_stat        ! minimum number of stations
c      parameter (min_stat=4)  ! 
c      integer max_stat        ! max ----------------------
c      parameter (max_stat=200) ! used as start of source paramete in m
c      parameter (max_stat=200) ! used as start of source paramete in m
c      integer max_event       ! maximum number of events
c      parameter (max_event=150)
c      parameter (max_event=150)
c      integer xdim,ydim
c      parameter (xdim=1200)
c      parameter (xdim=1200)
c      parameter (ydim=2000)
c      parameter (ydim=2000)
      real d(ydim)
      integer d_index
      real m(xdim)
      real g(ydim,xdim)
      real f_m(xdim,xdim),h_m(xdim,xdim),c_m(ydim,ydim),q_m(xdim,xdim)
      real m_1(ydim,xdim),m_3(xdim,ydim),r_m(xdim,xdim)
c ,rd_m(xdim,xdim)
c      real m_2s(xdim,xdim),m_2t(xdim,xdim) ! lo
      real m_2(xdim,xdim),y_m(xdim,xdim)
      integer indx(xdim),dd
      character*80 event_line(ydim)
c      real lg_vel             ! lg max velocity in m/s ?
c      parameter(lg_vel=3350.) 
      real svel               ! s velocity
      parameter(svel=3500.) 
      real dens               ! density
      parameter (dens=2850.)
      real tol
      parameter (tol=1.E-5)
      real v(xdim,xdim),w(xdim)
      real cvm(xdim,xdim)
      real thresh,wmax
      integer mp,np
c      real moment
      real mmag
      real pol(max_polyn$,2)
      integer npol
      logical inst,inev
      character*1 polflag
      real geomsp                          ! geometrical spreading constant
c      real f_m_damping                     ! alpha
c      parameter (f_m_damping=500.)         ! Mexico
c      parameter (f_m_damping=0.)         ! 0=no smoothness 
c      parameter (f_m_damping=500.) 
c      real k0,sigma_k                      ! factors used to compute F
c      parameter (k0=1.)
c      parameter (sigma_k=100.)       ! Mexico 1 deg, 100
c      parameter (sigma_k=100.)         ! CA
      character*5 sel_stat_list(max_stat,2)! list of selected stations
      integer sel_stat_n
      logical sel_stat_flag,tflag
      logical event_id_fix(max_event)
      real magnitude
      integer statnobs(max_trace)  ! number of obs per station
      integer nstatobs
      character*5 statcode(max_trace)
      
c
c the grid
c
c      character*1 grid_flag                ! d if q for grid, else r
      integer max_grid                     ! maximum number of grid points
c      parameter (max_grid=25)
      parameter (max_grid=50)              ! lot 26/5/2005
c      integer xgrid_npts,ygrid_npts        ! number of grid points
c      real xgrid_delta,ygrid_delta         ! grid spacing
c      real xgrid_start,ygrid_start         ! start value for grid
      real xgrid(max_grid),ygrid(max_grid) ! the grid values
      real seg_length(max_grid**2)         ! length of segments
      real grid_xy(max_grid**2,2)          ! x and y of grid center
      real grid_len(max_grid,max_grid)     ! total path length in grid cell
      integer igrid                        ! flag
      integer ngrid                        ! number of cells in grid
      integer traces_per_event             ! counter per event
      real fin
      integer idum
      real gasdev
      real source1,source2                 ! source terms used in cb test

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      nzero=0
      npole=0
c
c read definition file
c
      call get_qlg_par

c
c initialize vectors and array
c
      do i=1,ydim
        d(i)=0.
        do j=1,xdim
          g(i,j)=0.
          if (i.eq.1) m(j)=0.
        enddo
      enddo
      d_index = 0
      do i=1,max_event
        event_id_fix(i)=.false.
      enddo

c
c set some consts
c
c      nsmooth=5   ! changed May 24, 2001, since smoothing should not be needed
      nsmooth=0
c      checkerboard = .true.
c      checkerboard = .false.
      geomsp=.5              ! .5 for Lg, 1.for Sn and Pn
      rearth=6371.
      total_nstat=0
      nstatobs=0
      do i=1,max_trace
        n_lgmax(i)=0
        gv_lgmax(i)=0.
        statnobs(i)=0
        statcode(i)=' '
      enddo
      event_id=0

      do j=1,100
        npath(j)=0
      enddo
      d_max=10.
      sec_max=10.
      write1=0
c      verbose=.false.
c      verbose=.true.

      do i=1,max_grid
        do j=1,max_grid
          grid_len(i,j)=0.
        enddo
      enddo
      do i=1,max_grid**2
        grid_xy(i,1)=0.
        grid_xy(i,2)=0.
      enddo
        
c
c set polygon, which can be used to use travel paths only inside or outside
c
      npol=0
c      npol=4
c      pol(1,2)=21.
c      pol(1,1)=-106.
c      pol(2,2)=14.
c      pol(2,1)=-90.
c      pol(3,2)=10.
c      pol(3,1)=-90.
c      pol(4,2)=16.5
c      pol(4,1)=-106.

c
c read list of selected stations
c
      do j=1,max_stat
        sel_stat_list(j,1)='     '
        sel_stat_list(j,2)='     '
      enddo
      sel_stat_flag=.false.
      inquire(file='qlg.stat',exist=sel_stat_flag)
      if (sel_stat_flag) then
        open(2,file='qlg.stat',status='old')
        sel_stat_n=0
5       continue
        read(2,'(a)',end=6) line
c not sure where this format comes from
        if (line(1:1).ne.'#'.and.line(16:16).eq.'1'.and.     
     &      line.ne.'') then
          sel_stat_n=sel_stat_n+1 
          sel_stat_list(sel_stat_n,1)=line(1:5)
          sel_stat_list(sel_stat_n,2)=line(11:14)
        endif
        goto 5
6       continue

        close(2)
      endif


c
c open output files
c
      open(2,file='psxy.inp',status='unknown')
      open(50,file='past.inp',status='unknown')
      open(51,file='paths_1.inp',status='unknown')
      open(52,file='paths_2.inp',status='unknown')
      open(53,file='paths_3.inp',status='unknown')
      open(54,file='paths_4.inp',status='unknown')
      open(55,file='paths_5.inp',status='unknown')
      open(56,file='paths_6.inp',status='unknown')
      open(57,file='paths_7.inp',status='unknown')
      open(58,file='paths_8.inp',status='unknown')
      open(48,file='paths.inp',status='unknown')
      open(67,file='stat_xy.inp',status='unknown')
      open(61,file='stat_1.inp',status='unknown')
      open(62,file='stat_2.inp',status='unknown')
      open(60,file='epi.inp',status='unknown')
      open(73,file='epi_xy.inp',status='unknown')
      open(74,file='res.out',status='unknown')
      open(75,file='dres.out',status='unknown')
      open(76,file='resp.out',status='unknown')
      open(77,file='cb_sm.out',status='unknown')
      open(59,file='lgmax.inp',status='unknown')
      open(15,file='spec.p.inp',status='unknown')
      open(16,file='spec.lg.inp',status='unknown')
      open(17,file='qlg.out',status='unknown')
      open(21,file='dens.out',status='unknown')
      open(22,file='adistr.out',status='unknown')
      open(23,file='qtomo.out',status='unknown')
      open(91,file='distances.out',status='unknown')
      open(92,file='recordings.out',status='unknown')
      open(93,file='path_eff.out',status='unknown')
      open(94,file='site.out',status='unknown')
      open(90,file='statnobs.out',status='unknown')

c
c open input file
c
      write(*,*) ' Nordic Input File '
      read(5,'(a)') s_file 
      open(1,file=s_file,status='old',err=22)
c
c input
c
      write(*,*) '   (i)nversion '
      write(*,*) '   (d)istance plots '
      write(*,*) '                        Choice: '
      read(5,'(a1)') option

      if (option.ne.'i'.and.option.ne.'d') then
        write(*,*) ' wrong choice '
        stop
      endif
      if (option.eq.'i') then
        f_low=0.001
        f_high=100.
      endif

c      write(*,*) ' Filter '
c      read(5,*) f_low,f_high

c      write(*,*) ' Minimum Distance '
c      read(5,*) dist_min

c      write(*,*) ' Maximum Distance '
c      read(5,*) dist_max

c      write(*,*) ' LG velocities '
c      read(5,*) lg_low,lg_high

c      write(*,*) ' PG velocities '
c      read(5,*) p_low,p_high

c      do j=1,nfreq
c        write(*,*) j,' - ',freq_array(j)
c      enddo
      write(*,*) ' Frequency choice '
      read(5,*) fin 
      f_choice=0
      do i=1,nfreq
        if (fin.eq.freq_array(i)) f_choice=i
      enddo
       
c read(5,*) f_choice 

c      write(*,*) ' regional Q (r) or Q for grid (g) ? '
c      read(5,*) grid_flag

c      write(*,*) ' Vertical (v) or horizontal (h) component ? '
c      read(5,*) orientation

c      write(*,*) ' Polygon in, out, both (i,o,b) '
c      read(5,*) polflag    
      polflag='b'

c      write(*,*) ' Use stations with phase picks only (y/n) '
c      read(5,'(a)') ch
c      phase_only_flag=.false.
c      if (ch.eq.'y'.or.ch.eq.'Y') phase_only_flag=.true.

      open(19,file='freq.inp',status='unknown')
      write(19,'(f5.2)') freq_array(f_choice)
      close(19)

c
c apriori alpha, from array
c
      if (tomo_flag) then
        if (nfreq.eq.0) then
          write(*,*) ' frequency not defined in qlg.par '
          stop
        endif
        alpha_apriori = q_1_array(f_choice) 
        if(verbose) write(*,*) ' alpha apriori = ',alpha_apriori
      endif

c
c set grid
c
      if (tomo_flag) then
        do i=1,xgrid_npts
          xgrid(i)= xgrid_start + (i-1) * xgrid_delta
        enddo
        do i=1,ygrid_npts
          ygrid(i)= ygrid_start + (i-1) * ygrid_delta
        enddo
        ngrid=(xgrid_npts-1)*(ygrid_npts-1)

c
c set center of grid cells
c
        b=0
        do i=1,ygrid_npts-1
          do j=1,xgrid_npts-1
            b=b+1
            grid_xy(b,1)=xgrid_start+(j-1)*xgrid_delta+xgrid_delta/2.
            grid_xy(b,2)=ygrid_start+(i-1)*ygrid_delta+ygrid_delta/2.
          enddo
        enddo

c
c compute F matrix
c
        do i=1,xdim
          do j=1,xdim
            f_m(i,j)=0.
          enddo
        enddo
          
        do i=1,ngrid
          sum=0.
          do j=1,ngrid
c
c first compute Vij (stored in f_m) and pj (stored in sum)
            if (i.eq.j) then
              f_m(i,j)=damp_alpha*1.
            else
              x1=grid_xy(i,1)
              y1=grid_xy(i,2)
              x2=grid_xy(j,1)
              y2=grid_xy(j,2)
              call delaz(y1*pi/180.,x1*pi/180.,yy,dedeg,az0,
     &             y2*pi/180.,x2*pi/180.)
c              f_m(i,j)=-damp_alpha*exp(-(yy**2)/(2.*(damp_sigma**2)))
c compute Vij
              f_m(i,j)=exp(-(yy**2)/(2.*(damp_sigma**2)))
c compute pj
              sum=sum+f_m(i,j)
            endif
          enddo
c
c now compute f_m
          do j=1,ngrid
            if (i.ne.j) then
              if (sum.ne.0.) then
c                 f_m(i,j)=100.*f_m(i,j)/abs(sum)
                 f_m(i,j)=-damp_alpha*f_m(i,j)/abs(sum)
              else
c
c check if this should be '0'
c
                 f_m(i,j)=0.
              endif
            endif
          enddo
        enddo  

c
c init matrices
c
        do i=1,xdim
          do j=1,xdim
            q_m(i,j)=0.
          enddo
        enddo
        do i=1,ydim
          do j=1,ydim
            c_m(i,j)=0.
          enddo
          c_m(i,i)=1.
        enddo
        do i=1,ydim
          do j=1,xdim
            m_1(i,j)=0.
          enddo
        enddo
        do i=1,xdim
          do j=1,xdim
            h_m(i,j)=0.
            m_2(i,j)=0.
c            m_2t(i,j)=0.
c            m_2s(i,j)=0.
            cvm(i,j)=0.
            r_m(i,j)=0.
c            rd_m(i,j)=0.
          enddo
        enddo
        do i=1,xdim
          do j=1,ydim
            m_3(i,j)=0.
          enddo
        enddo

      else
        ngrid=1
      endif

c
c write out input parameters
c
      write(17,*) 
      write(17,'(a)') ' LG1 RESULT OUTPUT '
      write(17,*) 
      if (option.eq.'d') write(17,'(a)') ' NO INVERSION !!! '
      if (tomo_flag) then
        write(17,'(a)') ' method: Q tomography '
      else
        write(17,'(a)') ' method: SVD '
      endif
      write(17,*) 
      write(17,*) ' filters ',f_low,f_high
      write(17,*) ' distances ',dist_min,dist_max
      write(17,*) ' lg vel ',lg_low,lg_high
      write(17,*) ' p vel ',p_low,p_high
      write(17,*) ' comp ',orientation
      write(17,*) ' picked stations only ',ch
      if (checkerboard) 
     &      write(17,*) ' checker-board test '
      write(17,*) 
      write(17,*) ' reading sfiles... '
      write(17,*) 
  
      goto 23

22    continue

      write(*,*) ' file not found'
      stop

23    continue

      nstat=0
      station_counter = 0      
c
c read Seisan Nordic parameter file
c
      call get_seisan_def
      call indata(1,nstat,nphase,nhead,nrecord,type,expl,data,id)
      nphase=nrecord-1-nhead

      traces_per_event = 0

c
c go out if end of file or empty file
c
      if (nrecord.eq.0.and.option.eq.'i') goto 900
      if (nrecord.eq.0.and.option.eq.'d') goto 999

      event_id=event_id+1
c      write(17,'(a,i4,1x,a)') ' sfile: ',event_id,
c     &      infile(1:seiclen(infile))

      write(99,*) ' event_id ',event_id
      if(verbose) write(*,'(i3,a60)') event_id,data(1)(1:60)

c
c open output file
c
      if (option.eq.'d') then
        outfile = ' '
        write(outfile,'(a8,i3,1x,f6.3)') 
     &  'lg_dist.',event_id,freq_array(f_choice)
        do i=1,16
           if (outfile(i:i).eq.' ') outfile(i:i)='_'
        enddo
        open(18,file=outfile,status='unknown')
      endif

      if (event_id.gt.max_event) then
        write(*,*) ' Too many events '
        stop
      endif
      event_line(event_id)=data(1)

c
c read parameter data
c
      epiline = ' '
   
      read(data(1),'(1x,i4,1x,i2,i2,1x,i2,i2,1x,f4.1)')
     &  year,month,day,hour,min,sec

      read(data(1)(24:43),'(f7.3,f8.3,f5.1)') lat,lon,dep

      write(epiline,'(a)') 'sfile: '//s_file(1:seiclen(s_file))
      write(60,'(f7.3,1x,f8.3,1x,a8,1x,a80)')
     &     80.,dist_min+80.,'8 0 11 5 ',epiline
      write(epiline,'("origin time: ",i4,"/",i2,"/",i2,
     &                1x,i4,":",i2)') year,month,day,
     &                hour,min
      write(60,'(f7.3,1x,f8.3,1x,a8,1x,a80)')
     &     80.,dist_min+60.,'8 0 11 5 ',epiline

      write(epiline,'("latitude/longitude: ",f7.3,"/",f8.3)')
     *     lat,lon
      write(60,'(f7.3,1x,f8.3,1x,a8,1x,a80)')
     &     80.,dist_min+40.,'8 0 11 5 ',epiline

      write(epiline,'("depth/magnitude: ",f5.1,"/",a3)')
     *     dep,data(1)(57:59)
      if (data(1)(57:59).eq.'   '.and.fix_source) then
        write(*,*) ' you need magnitude if source fixed '
        stop
      endif
      read(data(1)(57:59),'(f3.1)') magnitude
      write(60,'(f7.3,1x,f8.3,1x,a8,1x,a80)')
     &     80.,dist_min+20.,'8 0 11 5 ',epiline

      call polos(lat,lon,pol,npol,inev) ! check if event in polygon
      lon_rad =lon*pi/180.
      lat_rad =lat*pi/180.
      call timsec(year,month,day,hour,min,sec,msec)
c
c init wav_* variables
c
      call wav_init
      b=0
c
c read all headers
c
      do a=1,nhead
        if (data(a)(80:80).eq.'6') then

          b=b+1
          infile = ' '
          infile=data(a)(2:79)
          if (verbose)
     &      write(*,*) ' Waveform file: ',infile(1:seiclen(infile))

          call get_full_wav_name(infile(1:seiclen(infile)),
     &           wav_filename(b))
          
c
c   now read first header for one event
c
          call read_wav_header(b)
c          write(17,*) '       waveform file: ' // wav_filename(b)
c     &          (1:seiclen(wav_filename(b)))
          if(wav_error_message.ne.' ') then
             write(6,'(1x,a)') wav_error_message
c             goto 25
          endif
        endif
      enddo

  
c
c first loop over all traces
c
      do i=1,wav_nchan
c
c find distance
c
            flag(i)=.true.
            call stat_loc(wav_stat(i),' ',slat,slon,selev)
            if (slat.eq.0..and.slon.eq.0.) flag(i)=.false. 
c
c vertical
c
            if (wav_comp(i)(1:4).ne.'HH Z'.and.
     &          wav_comp(i)(1:4).ne.'S  Z'.and.
     &          wav_comp(i)(4:4).ne.'Z'.and.
     &          wav_comp(i)(1:4).ne.'B  Z'.and.
     &          wav_comp(i)(1:4).ne.'BH Z') flag(i)=.false.   


            call polos(slat,slon,pol,npol,inst)
c
c check that not uncertain time
c
            if (wav_time_error(i).ne.' ') flag(i)=.false.

c
c check if station wanted
c
            if (sel_stat_flag) then
                tflag=.false.
                do j=1,sel_stat_n
                  if (sel_stat_list(j,1).eq.wav_stat(i).and.
     &                (sel_stat_list(j,2).eq.wav_comp(i).or.
     &                 seiclen(sel_stat_list(j,2)).le.0)) then
                     tflag=.true.
c                    write(*,*) ' found on list: ',wav_stat(i),
c     &                   wav_comp(i)
                  endif
                enddo
                if (.not.tflag) flag(i)=.false.
            endif
c
c if phases given, check that station is selected
c
            if (phase_only_flag.and.flag(i).eqv..true.) then
                if (nphase.ne.0) then
                  tflag=.false.
                  do k=nhead+1,nhead+nphase
c                      if (data(k)(2:5).eq.wav_stat(i)) 
                      if (data(k)(2:5).eq.wav_stat(i).and.
     &                    data(k)(15:15).ne.'9') ! check for uncertain time
     &                   tflag=.true.
                  enddo
                  if (.not.tflag) flag(i)=.false.
                endif
            endif

c
c check if response
c
            if (flag(i)) then
              wav_resp_file = ' '
              wav_current_chan(1)=i
              call read_resp
              if(wav_resp_status(1:1).eq.'9') then
                if (verbose)
     &            write(*,*)' no response: ',wav_stat(i),' ',wav_comp(i)
                write(76,*)' No response file for: ',
     &              wav_stat(i),' ',wav_comp(i)
                flag(i)=.false.
              endif
              if(wav_resp_status(1:1).eq.'8') then
                if (verbose)
     &           write(*,*)' response from header: ',
     &             wav_stat(i),' ',wav_comp(i)
                write(76,*)' response from header: ',
     &              wav_stat(i),' ',wav_comp(i)
              endif
c
c get rid of channels with low low-pass
c
              do j=1,nfilt
                if (ffilt(j).lt.10.and.pole(j).gt.1) then
c                  flag(i)=.false.
                  write(751,*) wav_stat(i),ffilt(j),pole(j)
                endif
              enddo
            endif
              
c
c take out stations
c

           if (wav_stat(i)(1:4).eq.'TEIG') flag(i)=.false.

c
c check if path within polygon
c
            if (polflag.eq.'i'.and.flag(i)) then
              if (.not.inev.or..not.inst) then
                flag(i)=.false.
                write(*,*) ' path outside ',wav_stat(i)
              endif
            elseif (polflag.eq.'o'.and.flag(i)) then
              if (inev.and.inst) then
                flag(i)=.false.
                write(*,*) ' path outside ',wav_stat(i)
              endif
            endif

c
c check if station already done
c
            new=.true.
            do j=1,nstat
              if (wav_stat(i).eq.stat(j)) new=.false.
            enddo
            if (new.and.flag(i)) then
              nstat=nstat+1
              stat(nstat)=wav_stat(i)
            else
              flag(i)=.false.
            endif

            if (flag(i)) then
              slat_rad=slat*pi/180.
              slon_rad=slon*pi/180.
              call delaz(slat_rad,slon_rad,dist(i),dedeg,az0,
     &            lat_rad,lon_rad)
    
              if (dist(i).lt.dist_min) then
                 flag(i)=.false.
                 if (verbose)
     &            write(*,*) ' distance too short, ',wav_stat(i),dist(i)
              endif
              if (dist(i).gt.dist_max) then
                 if (verbose)
     &            write(*,*) ' distance too large, ',wav_stat(i),dist(i)
                 flag(i)=.false.
              endif
c
c check if trace goes beyond 2.9 km/s group velocity
c
              rsec=wav_abs_time(i)-msec    ! msec is OT
              gv=dist(i)/(rsec+wav_nsamp(i)/wav_rate(i))
c              if (gv.gt.2.9) then
              if (gv.gt.lg_low) then
                flag(i)=.false.
                if (verbose)
     &            write(*,*) wav_stat(i),' time window too short ! '
              endif

c
c check if path within grid
c
              if (ngrid.ne.1) then
                if (lon.ge.xgrid(1).and.
     &              lon.le.xgrid(xgrid_npts).and.
     &              lat.ge.ygrid(1).and.
     &              lat.le.ygrid(ygrid_npts).and.
     &              slon.ge.xgrid(1).and.
     &              slon.le.xgrid(xgrid_npts).and.
     &              slat.ge.ygrid(1).and.
     &              slat.le.ygrid(ygrid_npts)) then

                else
                  flag(i)=.false. 
                  if (verbose)
     &            write(*,*) ' Path outside grid: ',wav_stat(i)
                endif
              endif

c
c get max distance
c
              if (dist(i).gt.d_max.and.flag(i)) then
                d_max = dist(i)
              endif
c
c write out station code for use with pstext
c
              if (flag(i)) then
                write(62,'(f7.3,1x,f8.3,1x,i3,1x,a5,1x,a10)') 
     &              10.,dist(i),6,'0 8 5',wav_stat(i)// ' '//
     &              wav_comp(i)
              endif
            endif
      enddo  ! end of first loop over all traces

c
c check how many stations are flaged
c
      nstat=0
      do i=1,wav_nchan
          if (flag(i)) nstat=nstat+1
      enddo

c
c check if enough stations
c
      if (nstat.lt.min_stat.and.option.eq.'i') then
            if (verbose) write(*,*) ' Too few stations '    
            event_id = event_id -1
            if (option.eq.'d') then
              close(18)
            endif
            goto 23
      endif

c
c sety_scale, signal=signal/max*y_scale
c
      y_scale=d_max/wav_nchan
c      y_scale=d_max/15.   !  xxx remember to change back

c
c second loop over traces
c
      do i=1,wav_nchan
        if (flag(i)) then

c
c add new stations to array of all stations, and set ind
c
              new=.true.
              ind=0
              do j=1,total_nstat
                if (wav_stat(i).eq.total_stat(j)) then
                  new=.false.
                  ind=j
                endif
              enddo
              if (new) then
                total_nstat=total_nstat+1
                nstatobs=nstatobs+1
                total_stat(total_nstat)=wav_stat(i)
                ind=total_nstat
c
c write out new stations
c
                call stat_loc(wav_stat(i),' ',slat,slon,selev)
                write(67,'(f8.3,1x,f8.3)') slat,slon
                write(61,'(f7.3,1x,f8.3,1x,a,1x,a)') 
     &                  slat,slon,'10 0 1 7',wav_stat(i)
              endif
              if(total_nstat.gt.max_stat) then
                if (verbose) write(*,*) ' Too many stations '
                stop
              endif
        endif
      enddo ! end of second loop over all traces


c
c third loop over traces
c
      do i=1,wav_nchan
        if (flag(i)) then
c
c set station index
c
              do j=1,total_nstat
                if (wav_stat(i).eq.total_stat(j)) then
                  ind=j
                endif
              enddo

c
c get time since event origin
c
              rsec=wav_abs_time(i)-msec
              if (orientation.eq.'h') then
c
c read 3 components
c
                call wav_read_3channel(i)
              else
c
c read vertical component only
c
                if (verbose) write(*,*) wav_stat(i),wav_comp(i)
                call wav_read_channel(i)
              endif
c
c check if signal clipped
c
              call check_clipped(signal1,wav_nsamp(i),clipped)
              if (clipped) then
                 flag(i)=.false.
                 if (verbose)
     &           write(*,'(a)') ' channel clipped: ' // 
     &              wav_stat(i) // ' ' // wav_comp(i)
                 write(17,'(a)') ' channel clipped: ' // 
     &              wav_stat(i) // ' ' // wav_comp(i)
                 goto 24
              endif

c
c read response, will be removed when spectrum is calculated
c
              wav_resp_file = ' '
              wav_current_chan(1)=i
              call read_resp

c
c get time since event origin
c
              rsec=wav_abs_time(i)-msec

              if (orientation.eq.'h') then
c
c read 3 components
c
                call wav_read_3channel(i)

              else
c
c read vertical component only
c
                call wav_read_channel(i)
 
              endif

c
c read response, will be removed when spectrum is calculated
c
              wav_resp_file = ' '
              wav_current_chan(1)=i
              call read_resp

              if(wav_resp_status(1:1).eq.'9') then
                 flag(i)=.false.
              endif


c
c remove mean
c
              call rmean(signal1,wav_nsamp(i))


c
c extract lg, p and noise windows
c
              nsamp_lg=0
              nsamp_p=0
              nsamp_n=0

              do j=1,wav_nsamp(i)
                x=(rsec+j/wav_rate(i))
                if (dist(i)/x.le.lg_high.and.dist(i)/x.ge.lg_low) then
                  nsamp_lg=nsamp_lg+1
                  signal_lg(nsamp_lg)=signal1(j)
                endif
                if (dist(i)/x.le.p_high.and.dist(i)/x.ge.p_low) then
                  nsamp_p=nsamp_p+1
                  signal_p(nsamp_p)=signal1(j)
                endif
                if (dist(i)/x.le.10.0.and.dist(i)/x.gt.9.) then
                  nsamp_n=nsamp_n+1
                  signal_n(nsamp_n)=signal1(j)
                endif
              enddo

c
c extract horizontal compents if wanted, 2=ns, 3=ew
c
              nsamp_lg_e=0
              nsamp_lg_n=0
              if (orientation.eq.'h')  then
                do j=1,wav_nsamp(wav_current_chan(2))
                  x=(rsec+j/wav_rate(wav_current_chan(2)))
                  if (dist(i)/x.le.lg_high.and.dist(i)/x.ge.lg_low) then
                    nsamp_lg_e=nsamp_lg_e+1
                    signal_lg_e(nsamp_lg_e)=signal2(j)
                  endif
                enddo
                do j=1,wav_nsamp(wav_current_chan(3))
                  x=(rsec+j/wav_rate(wav_current_chan(3)))
                  if (dist(i)/x.le.lg_high.and.dist(i)/x.ge.lg_low) then
                    nsamp_lg_n=nsamp_lg_n+1
                    signal_lg_n(nsamp_lg_n)=signal3(j)
                  endif
                enddo
              endif

c
c write out lg samples to ascii files
c
c              outfile = ' '
c              write(outfile,'(a5,1x,a4,a3)')
c     &          wav_stat(i),wav_comp(i),'.lg'
c              do j=1,13
c                 if (outfile(j:j).eq.' ') outfile(j:j)='_'
c              enddo
c
c              open(101,file=outfile,status='unknown')
c              do j=1,nsamp_lg
c                write(101,'(f15.1)') signal_lg(j)
c              enddo
c              close(101)


c
c get lg spectral levels, it is assumed that response is the
c same as for vertical component
c
              write1=0
              if (option.eq.'d'.and.f_choice.eq.1)
     &        write1=16
              call spec_value(i,signal_lg,nsamp_lg,nsmooth,f_low,
     &        f_high,1,freq_array,nfreq,write1,level_lg,y_com,ipow,
     *        zero,ppole,nzero,npole,norm)
c        write(*,*) ' xxx lg ',level_lg(f_choice)

              if (orientation.eq.'h')  then
                call spec_value(i,signal_lg_n,nsamp_lg_n,nsmooth,f_low,
     &          f_high,1,freq_array,nfreq,0,level_lg_n,y_com,ipow,
     *          zero,ppole,nzero,npole,norm)
                call spec_value(i,signal_lg_e,nsamp_lg_e,nsmooth,f_low,
     &          f_high,1,freq_array,nfreq,0,level_lg_e,y_com,ipow,
     *          zero,ppole,nzero,npole,norm)
c
c get sqrt of square levels
c
                do j=1,nfreq
                  if (level_lg_e(f_choice).gt.0..and.
     &         log10(level_lg_e(f_choice)).lt.30.and.
     &         level_lg_e(f_choice).gt.2.*level_n(f_choice).and.
     &         nsamp_lg_e/wav_rate(wav_current_chan(3)).gt.
     &         2./freq_array(f_choice).and.
     &         level_lg_n(f_choice).gt.0..and.
     &         log10(level_lg_n(f_choice)).lt.30.and.
     &         level_lg_n(f_choice).gt.2.*level_n(f_choice).and.
     &         nsamp_lg_n/wav_rate(wav_current_chan(2)).gt.
     &         2./freq_array(f_choice))
     &            then
                    level_lg(j) = sqrt(level_lg_e(j)**2 +
     &                               level_lg_n(j)**2 ) 
                  else
                    level_lg(j)=0.
                  endif
                enddo
              endif
c
c correct for kappa, this will not affect resulting Q-1, but will be
c seen in change to source term
c
              do j=1,nfreq
c                write(*,*)  ' before ',j,level_lg(j),freq_array(j),
c     &   level_lg(j)*exp(pi*qlgkappa*freq_array(j))/level_lg(j)
                level_lg(j)=level_lg(j)*exp(pi*qlgkappa*freq_array(j))
c                write(*,*)  ' after ',j,level_lg(j)
              enddo
c
c get noise spectral levels
c
              call spec_value(i,signal_n,nsamp_n,nsmooth,f_low,
     &        f_high,1,freq_array,nfreq,0,level_n,y_com,ipow,
     *        zero,ppole,nzero,npole,norm)
c        write(*,*) ' xxx n ',level_n(f_choice)

c
c get p spectral levels
c
              if (option.eq.'d'.and.f_choice.eq.1) write1=15
                call spec_value(i,signal_p,nsamp_p,nsmooth,f_low,
     &          f_high,1,freq_array,nfreq,write1,level_p,y_com,ipow,
     *          zero,ppole,nzero,npole,norm)
c
c correct for kappa
c
              do j=1,nfreq
                level_p(j)=level_p(j)*exp(pi*qlgkappa*freq_array(j))
              enddo

c        write(*,*) ' xxx p ',level_p(f_choice)

c
c write out Moment corrected for G but not for Q
c
              moment = 
     &          4.*pi*dens*(svel**3)*1.E-9*level_lg(f_choice) /
     &          (.55*2.*(1./sqrt(2.))) *
     &          sqrt(100.*1.E6*dist(i))
              if (option.eq.'d') then 
                if (moment.gt.0.) 
     &          write(18,'(f6.1,1x,f6.3)') dist(i),log10(moment)
              endif

c
c check if signal is ok
c
              good_signal = .false.
              if (level_lg(f_choice).gt.0..and.
     &          log10(level_lg(f_choice)).lt.30.and.
     &          level_lg(f_choice).gt.2.*level_n(f_choice).and.
     &          nsamp_lg/wav_rate(i).gt.2./freq_array(f_choice)) 
     &        then
                good_signal = .true.
              else
                if(verbose) write(*,*) ' bad signal !!! '
              endif

c
c set data vector and kernel matrix
c
              if (good_signal) then
c
c count observations per station
c
              statnobs(ind)=statnobs(ind)+1
              if (statcode(ind).eq.' ') statcode(ind)=wav_stat(i)

c
c get station coords
c
                call stat_loc(wav_stat(i),' ',slat,slon,selev)
                if(verbose) write(*,*)
                if (verbose)
     &          write(*,*) i,' ',wav_stat(i),' ',wav_comp(i),dist(i),
     &              slat,slon,lat,lon

c
c stop if too many paths
c
                if (d_index.ge.ydim) then
                  write(*,*) ' too many paths: ',d_index
                  stop
                endif
                d_index=d_index+1

c
c write out some statistics
c
                traces_per_event=traces_per_event+1
                write(91,'(f10.2)') dist(i)
 
                station_counter = station_counter + 1
 
                if (ngrid.eq.1) then
c                if (ngrid.eq.-1) then
c
c assume geometrical spreading as 1/R  for r < R0
c                       and       1/sqrt(100.*R) for r> R0
c
                  if (dist(i).le.100.) then
                    d(d_index)=log10(1.E-9*level_lg(f_choice))+
     &                      1.*log10(1000.*dist(i))
                  else
                    d(d_index)=log10(1.E-9*level_lg(f_choice))+
     &                      geomsp*log10(100.*1.E6*dist(i))
                  endif

c
c compute moment from spectral level, corrected for G and Q
c
          moment=4.*pi*dens*(svel**3)*1.E-9*level_lg(f_choice) /
     &          (.55*2.*(1./sqrt(2.))) *
     &          sqrt(100.*1.E6*dist(i)) *
     &          exp(pi*1000.*dist(i)*freq_array(f_choice)
     &          /(lg_vel*276.*freq_array(f_choice)**.66))

           if(verbose) write(*,*) ' Moment: ',moment

              if(verbose) write(*,*) event_id,dist(i),
     &               log10(level_lg(f_choice)),
     &               ' d(',d_index,') = ',d(d_index)


c
c set G for event (event_id) and station (ind)
c
                  g(d_index,1)=-pi*1000.*dist(i)*freq_array(f_choice)
     &                *log10(exp(1.))/lg_vel

                else
c
c get length of segments in grid cells
c
c                  call grid_seg(lat,lon,slat,slon,xgrid,ygrid,
c     &              max_grid,xgrid_npts,ygrid_npts,seg_length,
c     &              grid_len,dist_min,igrid)
                  call grid_seg(lat,lon,slat,slon,xgrid,ygrid,
     &              seg_length,grid_len,dist_min,igrid)

      
                  x = 0.
                  do j=1,ngrid
                    x=x+seg_length(j)
        if ((j.eq.78.or.j.eq.79).and.seg_length(j).gt.1.) 
     &      write(700,*)  ! xxx 
     &       wav_year(i),
     &             wav_month(i),wav_day(i),wav_hour(i),wav_min(i)
                  enddo
 
                  if (abs(x-dist(i)).gt.0.05*dist(i)) then
                    if (verbose)
     &                write(*,*) ' something wrong with path ... ',
     &                    x,dist(i)
                  endif
   
c
c add term from q apriori to data vector, invert for delta Q-1
c
                  if (dist(i).le.100.) then
                    d(d_index)=log10(1.E-9*level_lg(f_choice))+
     &                      1.*log10(1000.*dist(i)) +
     &                      pi*1000.*dist(i)*log10(exp(1.))*
     &                      freq_array(f_choice)*alpha_apriori/
     &                      lg_vel
                  else
                    d(d_index)=log10(1.E-9*level_lg(f_choice))+
     &                      geomsp*log10(100.*1.E6*dist(i)) +
     &                      pi*1000.*dist(i)*log10(exp(1.))*
     &                      freq_array(f_choice)*alpha_apriori/
     &                      lg_vel
                  endif

c
c set checkerboard values in case of cb test
c
                  j=0
                  if (checkerboard) then
                    d(d_index)=0.
                    do a=1,ygrid_npts-1
                      do b=1,xgrid_npts-1
                        j=j+1
                        if (float(int((a+b)/2.)).eq.(a+b)/2.) then
            write(101,*) a,' ',b,' + '
                          d(d_index)= d(d_index) -
     &                       pi*freq_array(f_choice)*log10(exp(1.))
     &                      *cbdelta*1000.*seg_length(j)/lg_vel
                        else
            write(101,*) a,' ',b,' - '
                           d(d_index)= d(d_index) +
     &                       pi*freq_array(f_choice)*log10(exp(1.))
     &                      *cbdelta*1000.*seg_length(j)/lg_vel
                        endif
                      enddo
                    enddo
                    if (source_perturb(event_id,1).eq.1.) then
c
c add source perturbation, see BSSA 96, 1131-1139, Menke(2006)
c
                      write(33,*) ' # ',event_id
                      write(33,*) ' before perturbation ',d(d_index)
c
c add differene to data due to magnitude perturbation:
c log moment ~ 3/2 MW, d() is in log units so change to magnitude
c is added times 3/2
c
              d(d_index)= d(d_index)+3./2.*source_perturb(event_id,2)

                      write(33,*) ' after perturbation  ',d(d_index)
                    endif
                  endif

c
c add gaussian noise, gasdev gives randum number between -1 and +1,
c gaussian noise gives noise to add in units of moment magnitude,
c log moment ~ 3/2 MW 
c
                  if (gaussian_noise.ne.0.) then
           write(88,*) ' event, station ',event_id,' ',wav_stat(i),' ',
     &        magnitude
           write(88,*) ' before noise ',d(d_index)
                      d(d_index)=d(d_index)+
     &                    gasdev(idum)*3./2.*gaussian_noise
           write(88,*) ' after noise ',d(d_index)
                  endif

                  do j=1,ngrid
                    g(d_index,j)=-pi*1000.*seg_length(j)
     &                   *freq_array(f_choice)
     &                   *log10(exp(1.))/lg_vel
                  enddo
                endif
c
c set source and site in G matrix for both regional and tomographic inversion
c
                if (ngrid+max_stat+event_id.gt.xdim) then
                  write(*,*) ' Too many sites/stations/cells !!! '
                  stop
                endif
                if (.not.checkerboard) then
                  if (.not.fix_site) then
                    g(d_index,ngrid+ind)=1.                  ! site term
                  endif
                  if (.not.fix_source) then
                    g(d_index,ngrid+max_stat+event_id)=1.    ! source term
                  else
c
c in checkerboard test do not invert for site term
c add equation to fix source, only once
c
                    if (.not.event_id_fix(event_id)) then
                      d_index=d_index+1
                      g(d_index,ngrid+max_stat+event_id)=1.    ! source term
c
c set data to expected model parameter
c
c           mmag=2./3.*log10(moment)-6.06
c           moment=4.*pi*dens*(svel**3)*(10**m(ngrid+total_nstat+i))
c     &        / (.55*2.*(1./sqrt(2.)))

                      d(d_index)=(magnitude+6.06)*3./2.
     &       -log10(4.*pi*dens*(svel**3))+log10(.55*2.*(1./sqrt(2.)))

                      event_id_fix(event_id)=.true. 
        write(*,*) ' fixed source, magnitude=',magnitude,d(d_index)
                    endif
                  endif
                endif
              endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c finished setting d and G, now prepare output for plots
c

c
c make sure that p level is above the noise, and p level gt 0.
c
              if (good_signal)
     &         lg_p_ratio(i)=level_lg(f_choice)/level_p(f_choice)

              if (good_signal.and.lg_p_ratio(i).lt.9999999.) then 

c
c set output unit
c

c
c ratio is for lg/pn
c
c                path_unit=int(lg_p_ratio(i)/2.)+51
                path_unit=int(lg_p_ratio(i)/3.)+51


                npath(int(lg_p_ratio(i))+1)=
     &              npath(int(lg_p_ratio(i))+1)+1
              else
                if (level_p(f_choice).lt.2.*level_n(f_choice)) then
                  if (verbose)
     &             write(*,*) ' p below noise level: ',
     &             level_p(f_choice),level_n(f_choice),wav_year(i),
     &             wav_month(i),wav_day(i),wav_hour(i),wav_min(i)
                endif
                path_unit=47
                lg_p_ratio(i)=0.
              endif
c              if (path_unit.gt.58) path_unit=58
              if (path_unit.gt.53) path_unit=53
              if (path_unit.le.0) path_unit=47

              if (lg_p_ratio(i).gt.0..and.good_signal) then
                if (verbose) 
     &          write(*,*) ' lg-p ratio: ',lg_p_ratio(i)
              endif

c
c write out paths
c
c               write(*,*) path_unit
                write(48,'(f7.3,1x,f8.3)') 
     *           lat,lon
                write(48,'(f7.3,1x,f8.3)') 
     *           slat,slon
                write(48,'(a1)') '>'

                write(path_unit,'(f7.3,1x,f8.3)') 
     *           lat,lon
                write(path_unit,'(f7.3,1x,f8.3)') 
     *           slat,slon
                write(path_unit,'(a1)') '>'
c
c write out path and efficency
c
                if (lg_p_ratio(i).gt.0..and.good_signal) then
                  write(93,'(f7.3,1x,f8.3,1x,f7.3,1x,f8.3,1x,f10.2)') 
     *              lat,lon,slat,slon,lg_p_ratio(i)
                endif
c
c seismogram distance plots
c
                if (option.eq.'d') then
c
c filter data
c

c-- calc. filt. cof.
                  call bndpas(f_low,f_high,1000.0/wav_rate(i),
     *              cof,gain)
c-- and filter
                  call filter(signal1,wav_nsamp(i),cof,gain,npasses)

c
c find max
c
                  max=0.
                  do j=1,wav_nsamp(i)
                    if (abs(signal1(j)).gt.max) max=abs(signal1(j))
                  enddo

                  lg_max=0.
                  lg_mean=0.
                  p_mean=0.
                  lg_nsamp=0
                  p_nsamp=0
                  lg_gv=0.

                  do j=1,wav_nsamp(i)

                    x=(rsec+j/wav_rate(i))
                    yy=signal1(j)/max*y_scale+dist(i)

c
c check for LG max
c
                    if (dist(i)/x.le.3.8.and.dist(i)/x.ge.2.5) then
                      if (abs(signal1(j)).gt.lg_max) then
                        lg_max=abs(signal1(j))
                        lg_gv=dist(i)/x
                      endif
                    endif

c
c write data, only if group velocity more than 2.5
c
                    if (dist(i)/x.gt.2.5) then
c                    if (x.lt.500.) then   ! xxx remember to change back
                      if (x.gt.sec_max) sec_max=x
                        if (yy.lt.1E20) then
                          write(2,'(f10.4,1x,f15.4)') x,yy
                        endif
                    endif
                  enddo

                  if (lg_gv.gt.0.) then
                    gv_lgmax(ind)=gv_lgmax(ind)+lg_gv
                    n_lgmax(ind)=n_lgmax(ind)+1
                  endif
c
c next trace
c
                  write(2,'(a1)') '>'
                endif
        endif

24      continue

      enddo   ! end of 3rd loop over all traces

      write(92,*) traces_per_event

c
c write out ratios
c
      if (option.eq.'d') then
            do i=1,wav_nchan
              if (flag(i)) then
                if (lg_p_ratio(i).gt.0.) then
                  write(62,'(f7.3,1x,f8.3,1x,i3,1x,a5,1x,f7.1)') 
     &              sec_max-25,dist(i),6,'0 1 5',lg_p_ratio(i)
                endif
              endif
            enddo
      endif
25    continue

c      enddo      ! end of reading waveform files

c
c make sure that event really has more than min_stat useful recordings
c
          
      if (station_counter.lt.min_stat) then
         event_id=event_id - 1       ! dont use event
         d_index=d_index - station_counter
         do b=d_index+1,d_index+station_counter
           d(b)=0.
           do j=1,xdim
             g(b,j)=0.
           enddo
         enddo
      endif
c
c needed by qlg.exp, to get the max distance
c

      write(*,*) ' maxdist ',d_max,' max_sec ',sec_max,' #'

      if (option.eq.'d') close(18)
      write(73,*) lat,lon
      goto 23

900   continue
c
c take out site terms if sites fixed
c
      if (fix_site) total_nstat=0

c
c add constraint that sum over log(site) = 1
c
      if (.not.checkerboard) then
        d_index=d_index+1
        d(d_index)=0.
        do i=1,total_nstat
          g(d_index,i+ngrid)=1.
        enddo
      endif

c
c for certain stations remove site term
c
      swap_with = total_nstat
      do b=1,total_nstat
         if (total_stat(b).eq.'TEIG') then
c     &       total_stat(b).eq.'LPIG'.or.
c     &       total_stat(b).eq.'ZAIG') then    

           write(*,*) ' removing site term for station ',total_stat(b) 
           do i=1,d_index
             g(i,ngrid+b)=g(i,ngrid+swap_with)
           enddo 
           total_stat(b)=total_stat(swap_with)
           swap_with=swap_with-1
         endif
      enddo
      total_nstat=swap_with
     
c
c remove empty columns for not defined sites
c
      do i=1,d_index
        do j=ngrid+total_nstat+1,ngrid+total_nstat+event_id
          g(i,j)=g(i,j+max_stat-total_nstat)
        enddo
      enddo
        
c
c now invert the data d = G * m
c
       if (ngrid.eq.1) then               ! regional Q
c
c call SVD
c
         mp=d_index
         if (.not.fix_site) then
           np=ngrid+total_nstat+event_id
         else
           np=ngrid+event_id
         endif

         write(*,*) ' calling svdcmp ... ',mp,np
         call svdcmpx(g,mp,np,ydim,xdim,w,v)

c
c edit w, remove small values
c
         wmax=0.
         do j=1,xdim
           if(w(j).gt.wmax)wmax=w(j)
         enddo
         thresh=tol*wmax
         do j=1,xdim
           if (w(j).lt.thresh)w(j)=0.
         enddo

c
c call svbksb
c
         write(*,*) ' calling svbksb ... '
         call svbksbx(g,w,v,mp,np,ydim,xdim,d,m)

c
c call svdvar to get variance
c
         write(*,*) ' calling svdvar ... '
         call svdvar(v,np,xdim,w,cvm,xdim)


c
c Q tomography
c
       else

         write(*,*) ' starting Q tomography ... '

c
c get H matrix
c
         call get_h(grid_len,xgrid_npts-1,ygrid_npts-1,
     &             max_grid,h_m,xdim)

c
c write out H matrix and checkerboard synth model
c
         b=0
         do i=1,ygrid_npts-1
           do j=1,xgrid_npts-1
             b=b+1
c             write(21,*) grid_xy(b,1),grid_xy(b,2),h_m(b,b)
             if (checkerboard) then
               if (float(int((i+j)/2.)).eq.(i+j)/2.) then
                 write(77,*) grid_xy(b,1),grid_xy(b,2),
     &             q_1_array(f_choice) + cbdelta
               else
                 write(77,*) grid_xy(b,1),grid_xy(b,2),
     &             q_1_array(f_choice) - cbdelta
               endif
             endif
           enddo
         enddo

c
c compute Q matrix, F-T*F + H-T * H
c
         do i=1,ngrid
           q_m(i,i)=h_m(i,i)**2
         enddo
         do i=1,ngrid
           do j=1,ngrid
             sum=0.
             do b=1,ngrid 
               sum=sum+f_m(b,i)*f_m(b,j)
             enddo
             q_m(i,j)=q_m(i,j)+sum
           enddo
         enddo
       
         mp=d_index
         if (.not.checkerboard) then
           np=ngrid+total_nstat+event_id
         else
           np=ngrid
         endif

c
c C-1 * G, which is G, since we assume C as I
c
         do i=1,mp
           do j=1,np
             m_1(i,j)=0.
             do b=1,mp
               m_1(i,j)=m_1(i,j) + c_m(i,b) * g(b,j)
             enddo
           enddo
         enddo

c
c G-T * m_1
c
         do i=1,np
           do j=1,np
             m_2(i,j)=0.
             do b=1,mp
               m_2(i,j)=m_2(i,j) + g(b,i) * m_1(b,j)
             enddo
           enddo
         enddo
c
c now add Q
c
         do i=1,np
           do j=1,np
             m_2(i,j)=m_2(i,j)+q_m(i,j)
c             m_2s(i,j)=m_2(i,j)    ! just to save m_2
             h_m(i,j)=m_2(i,j)    ! just to save m_2
           enddo
         enddo

c
c invert m_2, y_m is the inverted matrix
c
         do i=1,np
           do j=1,np
             y_m(i,j)=0.
           enddo
           y_m(i,i)=1. 
         enddo
         call ludcmpx(m_2,np,xdim,indx,dd)
         do j=1,np
           call lubksbx(m_2,np,xdim,indx,y_m(1,j))
         enddo
c
c check if m_2 * y_m is I
c
         x=0.
         do i=1,np
           do j=1,np
c             m_2t(i,j)=0.
             r_m(i,j)=0.
             do b=1,np
c               m_2t(i,j)=m_2t(i,j) + y_m(i,b) * m_2s(b,j)
               r_m(i,j)=r_m(i,j) + y_m(i,b) * h_m(b,j)
             enddo
             x=x+r_m(i,j)
           enddo
         enddo
         write(*,*) ' trend of GTG+Q * (GTG+Q)-1 = ',x/np
         write(17,*) ' trend of GTG+Q * (GTG+Q)-1 = ',x/np

         do i=1,xdim
           do j=1,xdim
             r_m(i,j)=0.
           enddo
         enddo
   
c
c y_m * G-T
c
         do i=1,np
           do j=1,mp
             m_3(i,j)=0.
             do b=1,mp
               m_3(i,j)=m_3(i,j) + y_m(i,b) * g(j,b)
             enddo
           enddo
         enddo

c
c covariance matrix cvm = m_3 * m_3-T
c
         do i=1,np
           do j=1,np
             cvm(i,j)=0.
             do b=1,mp
               cvm(i,j)=cvm(i,j) + m_3(i,b)*m_3(j,b)
             enddo
           enddo
         enddo

c
c resolution matrix r_m = m_3 * G
c
         do i=1,np
           do j=1,np
             r_m(i,j)=0.
             do b=1,mp
               r_m(i,j)=r_m(i,j) + m_3(i,b) * g(b,j)
             enddo
           enddo
         enddo
         b=0 

c
c compute rd_m matrix
c
        do i=1,xdim
          do j=1,xdim
            h_m(i,j)=0.
          enddo
        enddo

        do i=1,ngrid
          sum=0.
c          rd_m(i,i)=0.
          h_m(i,i)=0.   ! resolution matrix
          do j=1,ngrid
              x1=grid_xy(i,1)
              y1=grid_xy(i,2)
              x2=grid_xy(j,1)
              y2=grid_xy(j,2)
              call delaz(y1*pi/180.,x1*pi/180.,yy,dedeg,az0,
     &             y2*pi/180.,x2*pi/180.)
c              rd_m(i,i)=rd_m(i,i)+abs(r_m(i,j))*yy
              h_m(i,i)=h_m(i,i)+abs(r_m(i,j))*yy
              sum=sum+abs(r_m(i,j))
c              write(113,*) sum
          enddo
c          rd_m(i,i)=rd_m(i,i)/sum
          h_m(i,i)=h_m(i,i)/sum
        enddo

c
c write out resolution matrices
c
         do i=1,ygrid_npts-1
           do j=1,xgrid_npts-1
             b=b+1  
             write(74,*) grid_xy(b,1),grid_xy(b,2),r_m(b,b)
c             write(75,*) grid_xy(b,1),grid_xy(b,2),rd_m(b,b)
             write(75,*) grid_xy(b,1),grid_xy(b,2),h_m(b,b)
           enddo
         enddo
             

c
c m = m_3 * d
c
         do i=1,np
           m(i)=0.
           do j=1,mp
             m(i)=m(i)+m_3(i,j) * d(j)
           enddo
         enddo


         b=0
         do i=1,ygrid_npts-1
           do j=1,xgrid_npts-1
             b=b+1
             write(22,*) grid_xy(b,1),grid_xy(b,2)
           enddo
         enddo
              
       endif
999   continue

c
c output
c

c
c some statistics
c
        write(17,*) ' Total number of events: ',event_id
        write(17,*) ' Total number of paths : ',mp

         write(*,*) ' result ',freq_array(f_choice),m(1),
     &        sqrt(cvm(1,1)),' #'

         write(17,'(a,f5.2)')
     &     ' freq= ',freq_array(f_choice)

c
c write out the grid coordinates
c
         if (ngrid.ne.1) then
           write(17,*) ' The grid: '
           b=0
           do i=1,ygrid_npts-1
             do j=1,xgrid_npts-1
               b=b+1
               write(17,*) b,grid_xy(b,1),grid_xy(b,2)
             enddo
           enddo
         endif

c
c write out Q
c
         if (ngrid.ne.1) then
           write(17,'(a,g10.2)') ' Q-1 apriori: ',alpha_apriori
         endif
         do i=1,ngrid
           if (ngrid.ne.1) then
             m(i)=m(i)+alpha_apriori
           endif
           write(17,'(i5,a,g10.2,a,g10.2)')
     &       i,' Q-1 = ',m(i),' +/- ',sqrt(cvm(i,i))
           write(23,'(f10.3,1x,f10.3,1x,f8.6)') 
     &       grid_xy(i,1),grid_xy(i,2),m(i)
         enddo

         if (.not.fix_site) then
           write(17,'(a)') ' Site term '
           do i=1,total_nstat
             write(*,'(i3,1x,a5,1x,f6.2)') i,total_stat(i),m(i+ngrid)
             write(17,'(i3,1x,a5,1x,f6.2,1x,f6.2)')
     &           i,total_stat(i),m(ngrid+i),sqrt(cvm(i+ngrid,i+ngrid))
             call stat_loc(total_stat(i),' ',slat,slon,selev)
             write(94,'(f10.3,1x,f10.3,f10.3)') slat,slon,m(ngrid+i)
           enddo
         else
           write(17,'(a)') ' Site term fixed'
         endif
         write(17,'(a)') ' Source term '
         do i=1,event_id
c
c calculate moment in Nm
c
           moment=4.*pi*dens*(svel**3)*(10**m(ngrid+total_nstat+i))
     &        / (.55*2.*(1./sqrt(2.)))

c
c dyne cm
c           moment=4.*pi*dens*svel**3.*100*10.**m(ngrid+total_nstat+i) /
c     &            (.55*2.*(1./sqrt(2.)))

           mmag=2./3.*log10(moment)-6.06
           write(*,'(i3,1x,a60,f6.2)')
     &          i,event_line(i)(1:60),m(ngrid+total_nstat+i)
           write(17,'(i3,1x,a60,f6.2,1x,f6.2,1x,g8.2,1x,f5.2)')
     &          i,event_line(i)(1:60),m(ngrid+total_nstat+i),
     &          sqrt(cvm(ngrid+total_nstat+i,ngrid+total_nstat+i)),
     &          moment,mmag
         enddo


      

c
c write out lg group velocities
c
      lg_max=0.
      do i=1,total_nstat
        write(59,'(a5,1x,f4.2)') total_stat(i),
     &       gv_lgmax(i)/n_lgmax(i)
        lg_max=lg_max+gv_lgmax(i)/n_lgmax(i)
      enddo
      write(59,'(a7,f4.2)') 'total: ',lg_max/total_nstat

      do i=1,100
        write(50,'(i5)') npath(i)
      enddo
c
c write out observations per station
c
c      write(*,*) ' xxx nstatobs ',nstatobs
      do i=1,nstatobs
        write(90,*) statcode(i),statnobs(i)
c        write(*,*) statcode(i),statnobs(i)
      enddo
        
c
c close files
c
      close(1)
      close(2)
      close(15)
      close(16)
      close(17)
      close(20)
      close(21)
      close(22)
      close(23)
      close(91)
      close(92)
      close(50)
      close(51)
      close(52)
      close(53)
      close(54)
      close(55)
      close(56)
      close(48)
      close(61)
      close(62)
      close(67)
      close(73)
      close(74)
      close(75)
      close(76)
      close(77)
      close(60)
      close(59)
      close(93)
      close(94)
      close(90)

      stop
      end



      subroutine rmean(idata,nlen)
c
c simple routine to remove the mean
c
c input/output: idata - real array with nlen samples
c

      implicit none
C    Seisan library inserts and routines...
C    ======================================
C
      include 'libsei.inc'                ! Open file definitions
      include 'seidim.inc'

      real idata(max_sample)              ! data
      integer i                           ! counters
      integer nlen                        ! number of samples
      real dc                             ! average or DC level

c
c get dc
c
      dc=0.
      do i=1,nlen
        dc=dc+idata(i)
      enddo
      dc=dc/nlen

c
c remove dc
c
      do i=1,nlen
        idata(i)=idata(i)-int(dc)
      enddo

      return
      end



      subroutine grid_seg(lat,lon,slat,slon,xgrid,ygrid,
     &             seg_length,grid_len,min_dist,flag)
c
c routine to compute path length in grid cells
c
      implicit none
      include 'seidim.inc'
      include 'qlg.inc'
      integer i,j
      real lat,lon,slat,slon
      real ygrid(*),xgrid(*)
      integer max
c      parameter (max=25)
      parameter (max=50)
c      integer max,xmax,ymax
      real xinter(max,2),yinter(max,2)
      real seg_length(*)
      real grid_len(max,max)
      integer xint,yint
      real d1,d2,m,x,y,x1,x2,y1,y2,dist,dedeg,az0
      integer np,b
      integer flag,ind
      real pi
      parameter (pi=3.141593)
      logical done
      real points(2*max+2,3)
      integer nseg
      real seg(max**2,7)
      real min
      real grid(max,max)
      logical inside
      real min_dist,mind

      flag=1
      xint=0
      yint=0

c
c min_dist in degree, just approximately
c
      mind = min_dist/111.

c
c initialize
c
      do i=1,max**2
        seg_length(i) = 0.
      enddo
      do i=1,2*max+2
        points(i,1)=0.
        points(i,2)=0.
        points(i,3)=0.
      enddo

c 
c get linear equation, y = mx+b
c
      m = (slat-lat)/(slon-lon)
      b = slat - m * slon
c
c get distance between the endpoints 
c
      x1=slon*pi/180.
      x2=lon*pi/180.
      y1=slat*pi/180.
      y2=lat*pi/180.
      call delaz(y1,x1,dist,dedeg,az0,y2,x2)

c
c get x intersections
c
      do i=1,xgrid_npts

        x=xgrid(i)
        y=m*xgrid(i)+b
        call delaz(y*pi/180.,x*pi/180.,d1,dedeg,az0,y1,x1)
        call delaz(y*pi/180.,x*pi/180.,d2,dedeg,az0,y2,x2)
c
c check if distance to both end points smaller than distance between the
c endpoints 
c
        if (d1.lt.dist.and.d2.lt.dist) then
c
c check that not start or endpoint
c
          if (.not.(x.eq.lon.and.y.eq.lat).and.
     &        .not.(x.eq.slon.and.y.eq.slat)) then
            xint=xint+1
            xinter(xint,1)=x
            xinter(xint,2)=y
          endif
        endif
      enddo
c
c get y intersections
c
      do i=1,ygrid_npts

        y=ygrid(i)
        x=(y-b)/m
        call delaz(y*pi/180.,x*pi/180.,d1,dedeg,az0,y1,x1)
        call delaz(y*pi/180.,x*pi/180.,d2,dedeg,az0,y2,x2)
c
c check that not already an x intersect
c
        done=.false.
        do j=1,xint
          if (x.eq.xinter(j,1).and.y.eq.xinter(j,2)) done=.true.
        enddo
c
c check if distance to both end points smaller than distance between the
c endpoints
c
        if (d1.lt.dist.and.d2.lt.dist.and..not.done) then
          if (.not.(x.eq.lon.and.y.eq.lat).and.
     &        .not.(x.eq.slon.and.y.eq.slat)) then
            yint=yint+1
            yinter(yint,1)=x
            yinter(yint,2)=y
          endif
        endif
      enddo

c
c put all points into one array
c
      np=2+xint+yint
      points(1,1)=lon
      points(1,2)=lat
      points(np,1)=slon
      points(np,2)=slat
      do i=1,xint
        points(i+1,1)=xinter(i,1)
        points(i+1,2)=xinter(i,2)
      enddo
      do i=1,yint
        points(i+1+xint,1)=yinter(i,1)
        points(i+1+xint,2)=yinter(i,2)
      enddo
     
c      do i=1,np
c        write(*,*) i,points(i,1),points(i,2)
c      enddo

c
c find segments starting mind from epicenter
c
      nseg=0
      x1=points(1,1)
      y1=points(1,2)
c      call delaz(y1*pi/180.,x1*pi/180.,d1,dedeg,az0,
c     &             points(1,2)*pi/180.,points(1,1)*pi/180.)
c      write(*,*) points(1,1),points(1,2),x,y,x1,y1,d1
      points(1,3)=1.
      d2=0.

150   continue
      min=99999.
c
c search for minimum path length between points
c
      do i=1,np
        x2=points(i,1)
        y2=points(i,2)
        call delaz(y1*pi/180.,x1*pi/180.,d1,dedeg,az0,
     &             y2*pi/180.,x2*pi/180.)
        if (d1.lt.min.and.points(i,3).ne.1..and.d1.ne.0.) then
          ind=i
          min=d1
        endif
      enddo
      if (min.eq.99999.) goto 160
  
c
c set segment
c
      nseg=nseg+1
      seg(nseg,1)=x1
      seg(nseg,2)=y1
      seg(nseg,3)=points(ind,1)
      seg(nseg,4)=points(ind,2)
      seg(nseg,5)=(x1+points(ind,1))/2.
      seg(nseg,6)=m*seg(nseg,5)+b
      seg(nseg,7)=min
      d2=d2+min

      points(ind,3)=1.
      x1=points(ind,1)
      y1=points(ind,2)

      goto 150

160   continue

c      do i=1,nseg
c        write(*,*) i,seg(i,1),seg(i,2),seg(i,3),seg(i,4),
c     &               seg(i,5),seg(i,6),seg(i,7)
c      enddo

c
c check in which grid box center of segment lies, and set length of segment
c
      do i=1,ygrid_npts
        do j=1,xgrid_npts
           grid(i,j)=0.
        enddo
      enddo
      do i=1,ygrid_npts-1
        do j=1,xgrid_npts-1
c          pol(1,1)=xgrid(j)
c          pol(1,2)=ygrid(i)
c          pol(2,1)=xgrid(j)
c          pol(2,2)=ygrid(i+1)
c          pol(3,1)=xgrid(j+1)
c          pol(3,2)=ygrid(i+1)
c          pol(4,1)=xgrid(j+1)
c          pol(4,2)=ygrid(i)
c
          do b=1,nseg
c            call polos(seg(b,6),seg(b,5),pol,4,inside) ! check if in polygon

c
c check if in grid cell, seg(,5) is lon, linear
c
            inside = .false.
            if (seg(b,5).ge.xgrid(j).and.
     &          seg(b,5).lt.xgrid(j+1).and.
     &          seg(b,6).ge.ygrid(i).and.
     &          seg(b,6).lt.ygrid(i+1)) then
                   inside = .true.
c                  write(*,*) b,i,j,seg(b,5),seg(b,6),xgrid(j),
c     &                      xgrid(j+1), ygrid(i),ygrid(i+1)

            endif

            if (inside) then
              grid(i,j)=seg(b,7)
c              write(*,*) ' found ',i,j,seg(b,7),seg(b,6),seg(b,5),
c     &              seg(b,1),seg(b,2),seg(b,3),seg(b,4)
            endif
          enddo
        enddo
      enddo
      b=0
      do i=1,ygrid_npts-1
        do j=1,xgrid_npts-1
          b=b+1
          seg_length(b)=grid(i,j)
c          write(*,*) i,j,b,seg_length(b)
          grid_len(i,j)=grid_len(i,j)+grid(i,j)
        enddo
      enddo

      return
      end


      subroutine get_h(len,xgrid,ygrid,maxg,h,hdim)
c
c compute weigthing matrix
c
      implicit none
      include 'qlg.inc'
      integer i,j,hdim,b,maxg,xgrid,ygrid
      real len(maxg,maxg)           ! seg_dim is physical dim
      real h(hdim,hdim)
c      real lambda
c      parameter (lambda=.001) ! Mexico
c      parameter (lambda=.001)
c      real damping
c      parameter (damping=250.) ! Mexico
c      parameter (damping=1000.) ! Mexico, changed
c      parameter (damping=500.) ! CA
      real x
      real min

      min=damp_beta * exp(-50.)
      b=0
      do i=1,ygrid
        do j=1,xgrid
          b=b+1
c      write(*,*) i,j,len(i,j)
          if (len(i,j).gt.0) then
            x=len(i,j)
          else
            x=0.
          endif
          h(b,b)= damp_beta * exp(-damp_lambda*x)
         write(666,*) b,' ',h(b,b),' ',x
c          if (h(b,b).lt.min) h(b,b)=0.
        enddo
      enddo
          
      return
      end


      subroutine check_clipped_old(x,nsamp,clipped)
c
c routine to check if signal is clipped
c
      implicit none
      include 'libsei.inc'                ! Open file definitions
      include 'seidim.inc'

      real x(max_sample)                  ! data
      integer nsamp,i,max_cnt
      real max
      logical clipped

      max=0.
      max_cnt=0.
      clipped=.false.
      do i=1,nsamp
        if (abs(x(i)).gt.max) then
          max=abs(x(i)) 
        elseif (abs(x(i)).eq.max) then
          max_cnt=max_cnt+1
        endif
      enddo
c
c if the max amplitude appears several times, the signal is considered
c to be clipped
c
      if (max_cnt.ge.25) then
        clipped=.true.
        write(*,*) ' signal clipped, max_cnt = ',max_cnt
      endif

      return
      end

      subroutine get_qlg_par
      implicit none

      include 'libsei.inc'                ! Seisan definitions
      include 'seidim.inc'                ! ------------------
      include 'seisan.inc'                ! ------------------
      include 'qlg.inc'                   ! ------------------
      external sei open,                  ! Open file routine.
     &         sei close,                 ! Close file routine.
     &         sei get file               ! Find files in diff dirs
      integer seiclen

      integer read1                       ! input unit
      integer code 
      character*80 line
      real var,mag
      integer i

      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   read1,           ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   'qlg.par')       ! For this filename.

      if(code.ne.e_ok$) then
        write(*,*) ' definition file does not exist: qlg.par'
        stop
      endif

c
c init values
c
      f_low=0.
      f_high=0.
      dist_min=0.
      dist_max=5000.
      lg_low=3.
      lg_high=3.7 
      p_low=6.5
      p_high=8.0
      tomo_flag=.false.
      phase_only_flag=.false.
      orientation='v'
      nsmooth=0
      nfreq=0
      min_stat=4
      lg_vel=3350.
      damp_alpha=500.
      damp_beta=1000.
      damp_lambda=.001
      damp_sigma=100.
      checkerboard=.false.
      verbose=.false.
      xgrid_npts=0
      ygrid_npts=0
      xgrid_delta=0.
      ygrid_delta=0.
      xgrid_start=0.
      ygrid_start=0.
      fix_source=.false.
      fix_site=.false.
      gaussian_noise=0.
      qlgkappa=0.
      cbdelta=0.0
      do i=1,max_event
        source_perturb(i,1)=0.
        source_perturb(i,2)=0.
      enddo

10    continue
      read(read1,'(a80)',end=999) line
c
c read parameters
c
      if (line(1:10).eq.'INPUT FILE') then       
        s_file=line(41:seiclen(line))
      elseif (line(1:6).eq.'FILTER') then
        read(line(41:50),*) var
        if (var.ne.0.) f_low=var
        read(line(51:60),*) var
        if (var.ne.0.) f_high=var
      elseif (line(1:9).eq.'DISTANCES') then
        read(line(41:50),*) var
        if (var.ne.0.) dist_min=var
        read(line(51:60),*) var
        if (var.ne.0.) dist_max=var
      elseif (line(1:12).eq.'GROUP VEL LG') then
        read(line(41:50),*) var
        if (var.ne.0.) lg_low=var
        read(line(51:60),*) var
        if (var.ne.0.) lg_high=var
      elseif (line(1:11).eq.'GROUP VEL P') then
        read(line(41:50),*) var
        if (var.ne.0.) p_low=var
        read(line(51:60),*) var
        if (var.ne.0.) p_high=var
      elseif (line(1:14).eq.'INVERSION TYPE') then
        read(line(41:50),*) var
        if (var.ne.0.) tomo_flag=.true.
      elseif (line(1:11).eq.'ORIENTATION') then
        read(line(41:50),*) var
        if (var.ne.0.) orientation='h'
      elseif (line(1:10).eq.'PHASE ONLY') then
        read(line(41:50),*) var
        if (var.ne.0.) phase_only_flag=.true.
      elseif (line(1:10).eq.'FIX SOURCE') then
        read(line(41:50),*) var
        if (var.ne.0.) fix_source=.true.
      elseif (line(1:8).eq.'FIX SITE') then
        read(line(41:50),*) var
        if (var.ne.0.) fix_site=.true.
      elseif (line(1:5).eq.'KAPPA') then
        read(line(41:50),*) var
        if (var.gt.0.) qlgkappa=var
      elseif (line(1:10).eq.'FREQUENCY') then
        if (nfreq+1.le.max_nfreq) then
          read(line(41:50),*) var
          if (var.ne.0.) then
            nfreq=nfreq+1
            freq_array(nfreq)=var
          endif
          read(line(51:60),*) var
          q_1_array(nfreq)=var
        endif
      elseif (line(1:11).eq.'STATION MIN') then
        read(line(41:50),*) var
        if (var.ne.0.) min_stat=int(var)
      elseif (line(1:11).eq.'VELOCITY LG') then
        read(line(41:50),*) var
        if (var.ne.0.) lg_vel=var
      elseif (line(1:13).eq.'DAMPING ALPHA') then
        read(line(41:50),*) var
        if (var.ne.0.) damp_alpha=var
      elseif (line(1:12).eq.'DAMPING BETA') then
        read(line(41:50),*) var
        if (var.ne.0.) damp_beta=var
      elseif (line(1:13).eq.'DAMPING LAMBDA') then
        read(line(41:50),*) var
        if (var.ne.0.) damp_lambda=var
      elseif (line(1:12).eq.'DAMPING SIGMA') then
        read(line(41:50),*) var
        if (var.ne.0.) damp_sigma=var
      elseif (line(1:7).eq.'NSMOOTH') then
        read(line(41:50),*) var
        if (var.ne.0.) nsmooth=int(var)
      elseif (line(1:18).eq.'CHECKERBOARD DELTA') then
        read(line(41:50),*) var
        if (var.ne.0.) cbdelta=var
      elseif (line(1:14).eq.'CHECKERBOARD  ') then
        read(line(41:50),*) var
        if (var.ne.0.) checkerboard=.true.
c      elseif (line(1:16).eq.'CHECKERBOARD MAG') then
c        read(line(41:50),*) var
c        if (var.ne.0.) cbmag=var
      elseif (line(1:19).eq.'SOURCE PERTURBATION') then
        read(line(41:50),*) var
        source_perturb(int(var),1)=1. ! flag
        read(line(51:60),*) mag
        source_perturb(int(var),2)=mag ! magnitude
      elseif (line(1:14).eq.'GAUSSIAN NOISE') then
        read(line(41:50),*) var
        if (var.ne.0.) gaussian_noise=var
      elseif (line(1:7).eq.'VERBOSE') then
        read(line(41:50),*) var
        if (var.ne.0.) verbose=.true.
      elseif (line(1:7).eq.'X START') then
        read(line(41:50),*) var
        if (var.ne.0.) xgrid_start=var
      elseif (line(1:7).eq.'Y START') then
        read(line(41:50),*) var
        if (var.ne.0.) ygrid_start=var
      elseif (line(1:7).eq.'X DELTA') then
        read(line(41:50),*) var
        if (var.ne.0.) xgrid_delta=var
      elseif (line(1:7).eq.'Y DELTA') then
        read(line(41:50),*) var
        if (var.ne.0.) ygrid_delta=var
      elseif (line(1:9).eq.'X NPOINTS') then
        read(line(41:50),*) var
        if (var.ne.0.) xgrid_npts=int(var)
      elseif (line(1:9).eq.'Y NPOINTS') then
        read(line(41:50),*) var
        if (var.ne.0.) ygrid_npts=int(var)
      
      endif

      goto 10

999   continue
      call sei close( close$, read1, code)

      return
      end

      FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *Ko!63+16.

      FUNCTION gasdev(idum)
      INTEGER idum
      REAL gasdev
CU    USES ran1
      INTEGER iset
      REAL fac,gset,rsq,v1,v2,ran1
      SAVE iset,gset
      DATA iset/0/
      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
      else
        gasdev=gset
        iset=0
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *Ko!63+16.

