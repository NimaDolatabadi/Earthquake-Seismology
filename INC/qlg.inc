c
c dimensions
c
c
c changes 
c
c 03/02/07 lot added fix_source and fix_site
c 07/03/97 lot added source_perturb
c
      integer max_nfreq 
      parameter (max_nfreq=50)         ! max number of frequencies
      integer xdim,ydim                ! Kernel matrix dimension
      parameter (xdim=2000)
      parameter (ydim=4500)
      integer max_event                ! maximum number of events
      parameter (max_event=800)
      integer max_stat                 ! max ----------------------
      parameter (max_stat=250)         ! used as start of source paramete in m


      character*80 s_file              ! Nordic input file name
      real f_low,f_high                ! filter
      real dist_min,dist_max           ! distance range
      real lg_low,lg_high,p_low,p_high ! group velocities
      logical tomo_flag                ! true for tomography
      character*1 orientation          ! sensor orientation, v or h
      logical phase_only_flag          ! true if phase pick required
      integer nsmooth                  ! number of times to smooth
      integer nfreq                    ! number of frequencies
      real freq_array(max_nfreq)       ! array of freq.
      real q_1_array(max_nfreq)        ! 1/Q a priori values
      real source_perturb(max_event,2) ! event number and magnitude to 
                                       ! perturb source
      integer min_stat                 ! minimum number of stations
      real lg_vel                      ! lg max velocity in m/s
      real cbdelta                     ! checkerboard q-1 delta
      real cbmag                       ! reference magnitude for cb test
      real damp_alpha,damp_beta,       ! damping constants
     &     damp_lambda,damp_sigma
      real gaussian_noise              ! percent gaussian noise to be added
      logical checkerboard             ! true for checker test
      logical verbose                  ! verbose or quite mode
      logical fix_source               ! flag to fix source term
      logical fix_site                 ! flag to fix site term
      real qlgkappa                    ! near surface attenuation

      integer xgrid_npts,ygrid_npts        ! number of grid points
      real xgrid_delta,ygrid_delta         ! grid spacing
      real xgrid_start,ygrid_start         ! start value for grid

      common /qlg_1/ 
     &   freq_array,q_1_array,source_perturb,
     &   f_low,f_high,dist_min,dist_max,qlgkappa,
     &   lg_low,lg_high,p_low,p_high,lg_vel,damp_alpha,damp_beta,
     &   damp_lambda,damp_sigma,gaussian_noise,cbdelta,cbmag,
     &   xgrid_delta,ygrid_delta,xgrid_start,ygrid_start,
     &   nsmooth,nfreq,min_stat,xgrid_npts,ygrid_npts,
     &   tomo_flag,phase_only_flag,checkerboard,verbose,
     &   fix_source,fix_site,
     &   s_file,orientation

