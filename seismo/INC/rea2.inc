c
c    include file and common block for parameter data for one event
c
c    updates
c
c  may 26 2001 jh : add hyp_mag_all,hyp_mag_agency_all,hyp_type_all
c  dec 5  2011 jh : rea_sn changed to rea_ain (angle of incidense, sn: signal
c                   to noise ratio no longer used
c

c
c  general parameters
c
      integer      rea2_nstat                  ! number of stations
      integer      rea2_nphase                 ! number of phases lines
      integer      rea2_nhead                  ! number of header lines
      integer      rea2_nrecord                ! number of records
      integer      rea2_nspec                  ! number of spectra 
      integer      rea2_nhyp                   ! number of hypocenters
      integer      rea2_nmag                   ! number of magnitudes
      integer      rea2_nmacro                 ! number macroseismic lines
      integer      rea2_nwav                   ! number of waveform files
      integer      rea2_ncomment               ! number of comment lines
      integer      rea2_nfault                 ! number of fault plane solutions
      character*80 rea2_macro(100)             ! macroseismic solutions
      character*80 rea2_wav(100)               ! waveform file names lines
      character*80 rea2_fault(100)             ! fault plane solutions lines
      character*80 rea2_comment(100)           ! comment lines
      character*80 rea2_id_line                ! event id line
c
c   hypocentral parameters, index 1 is the prime solution in first
c   line
c
      integer     hyp2_year(100)              ! hypocenter year
      integer     hyp2_month(100)
      integer     hyp2_day(100)
      integer     hyp2_hour(100)
      integer     hyp2_min(100)
      real        hyp2_sec(100)
      character*1 hyp2_model(100)             ! model indicator
      character*1 hyp2_dist_id(100)           ! distance indicator
      character*1 hyp2_type(100)              ! event type like E
      character*1 hyp2_fix_org(100)           ! fix origin time flag
      real        hyp2_lat(100)               ! latitude
      real        hyp2_lon(100)               ! longitude
      real        hyp2_depth(100)             ! depth
      character*1 hyp2_depth_flag(100)        ! depth flag
      character*1 hyp2_epi_flag(100)          ! epicenter flag
      character*5 hyp2_agency(100)            ! hypocenter agency, use 3 only
      integer     hyp2_nstat(100)             ! number of station
      real        hyp2_rms(100)               ! rms of hypocenter solution
      real        hyp2_mag(6,100)             ! magnitudes
      real        hyp2_mag_all(200)           ! all magnitudes, no hyp. assco.
      character*1 hyp2_mag_type(6,100)        ! magnitude types
      character*1 hyp2_mag_type_all(200)      ! all -----------
      character*5 hyp2_mag_agency(6,100)      ! magnitude agencies
      character*5 hyp2_mag_agency_all(200)    ! all
      logical     hyp2_high_accuracy(100)     ! high accurcy flag
      logical     hyp2_error(100)             ! true if hypocenter has an error
      character*20 hyp2_auto(100)             ! name of auto process for parameter
c
c   hypocenter errors
c
      real        hyp2_gap(100)               ! gap, degrees
      real        hyp2_sec_err(100)           ! oriign time error (sec)
      real        hyp2_lat_err(100)           ! latitude error (km)
      real        hyp2_lon_err(100)           ! longitude error (km)
      real        hyp2_depth_err(100)         ! depth error (km)
      real        hyp2_cov(3,100)             ! covariance, xy,xz,yz (kmXkm)
c
c  phase line parameters and associated parameters, all start with rea
c  and there can be nphase different parameters of each
c
      character*5 rea2_stat(max_data/2)       ! station codes
      character*4 rea2_comp(max_data/2)       ! componenets
      character*2 rea2_co(max_data/2)         ! 2 letter componenets
      character*8 rea2_phase(max_data/2)      ! phase name
      character*1 rea2_onset(max_data/2)      ! onset I or E or blank
      character*1 rea2_weight_in(max_data/2)  ! input weight
      character*2 rea2_weight_out(max_data/2) ! weight out
      character*1 rea2_polarity(max_data/2)   ! polarity, D or C
      integer     rea2_year(max_data/2)
      integer     rea2_month(max_data/2)
      integer     rea2_day(max_data/2)
      integer     rea2_hour(max_data/2)
      integer     rea2_min(max_data/2)
      real        rea2_sec(max_data/2)
      real*8      rea2_abs_time(max_data/2)   ! abs time of phase time
      real        rea2_coda(max_data/2)       ! coda length in s
      real        rea2_amp(max_data/2)        ! amplitude in nm
      real        rea2_per(max_data/2)        ! period of amplitude
      real        rea2_baz_obs(max_data/2)    ! observed back azimuth
      real        rea2_baz_cal(max_data/2)    ! calculated back azimuth
      real        rea2_vel(max_data/2)        ! observed apparent velocity
      real        rea2_ain(max_data/2)        ! calculated ain
      real        rea2_baz_res(max_data/2)    ! back azimuth residual
      real        rea2_res(max_data/2)        ! travel time residual
      real        rea2_dist(max_data/2)       ! epicentral distance
      real        rea2_az(max_data/2)         ! azimuth
c
c   spectral parameters, time is give in parameter above, spectra
c   have phase name SPECP or SPECS and are thus considered phases
c   although not written as such in s-file. the avrge spectral values is
c   not considered a phase
c
      real        rea2_moment(max_data/2)     ! log moment, Nm
      real        rea2_sdrop(max_data/2)      ! stress drop, bar
      real        rea2_omega0(max_data/2)     ! log spectral flat level, ns
      real        rea2_cornerf(max_data/2)    ! corner f
      real        rea2_radius(max_data/2)     ! source radius
      real        rea2_swin(max_data/2)       ! window lenght used
      real        rea2_vs(max_data/2)         ! s-velocity at source, km/s
      real        rea2_vp(max_data/2)         ! p-velocity at source, km/s
      real        rea2_q0(max_data/2)         ! q0
      real        rea2_qalpha(max_data/2)     ! q alpha
      real        rea2_kappa(max_data/2)      ! kappa
      real        rea2_density(max_data/2)    ! density g/cm**3
      real        rea2_slope(max_data/2)      ! - measured slope of spectrum
      real        rea2_geo_dist(max_data/2)   ! geo distance
c
c   same as above, the but averages, only one value
c
      real        rea2_av_moment              ! log moment, Nm
      real        rea2_av_sdrop               ! stress drop, bar
      real        rea2_av_omega0              ! log spectral flat level, ns
      real        rea2_av_cornerf             ! corner f
      real        rea2_av_radius              ! source radius
      real        rea2_av_swin                ! window lenght used
      real        rea2_av_mw                  ! moment mag
      real        rea2_av_slope               ! slope
c
      character*20 rea2_auto(max_data/2)      ! name of auto process making par

c   magnitudes
c
      real        rea2_mc(max_data/2)         ! coda
      real        rea2_ml(max_data/2)         ! local
      real        rea2_mb(max_data/2)         ! mb
      real        rea2_ms(max_data/2)         ! ms
      real        rea2_mw(max_data/2)         ! mw

c
c   fixed parameters for spectral analysis and others parameters
c
      integer     par2_nstat      ! number of stations with paremeters
      real        par2_vs         ! s-velocity at source, km/s
      real        par2_vp         ! p-velocity at source, km/s
      real        par2_q0         ! q0, general for p and s if not given below
      real        par2_qalpha     ! q alpha ----------------------------------
      real        par2_kappa      ! kappa   ----------------------------------
      real        par2_density    ! density at source g/cm**3
c
      real        par2_eventype   ! determine event type (L or D) (0 or 1)
      real        par2_coda       ! determine coda length if a P-phase (0 or 1)
      real        par2_flow       ! filt low for spec, general
      real        par2_flow_pole  ! number of poles 
      real        par2_fhigh      ! filt high-----------------------------
      real        par2_fhigh_pole ! number of poles ----------------------
c
c   p-phase processing
c
      real        par2_pick_p       ! pick p-phases (0: no, 1 yes)
      real        par2_mb_amp       ! determine mb amp if a P-phase (0 or 1)
      real        par2_mb_flow      ! low filter for mb-amp
      real        par2_mb_flow_pole ! number of poles for  mb low filter
      real        par2_mb_fhigh     ! high filter for mb
      real        par2_mb_fhigh_pole! number of poles for mb high filter
      real        par2_mb_amp_sn    ! minimum s/n for mb amp
      real        par2_pspec        ! do p-spectrum if P-phase (0 or 1)
      real        par2_pspec_sn     ! minimum s/n for p-spec
      real        par2_p_q0         ! Q0 for P, overrides par_q0 if set
      real        par2_p_qalpha     ! qalpha -------------par_qalpha ---
      real        par2_p_kappa      ! kappa --------------par_kappa----
      real        par2_p_flow       ! filt low for spec., overrides par_flow
      real        par2_p_flow_pole  ! number of poles ----------------------
      real        par2_p_fhigh      ! filt high-----------------------------
      real        par2_p_fhigh_pole ! number of poles ----------------------
c
c   s-phase processing
c
      real        par2_pick_s       ! pick s or not (0,1)
      real        par2_ml_amp       ! determine ml amp if a P-phase, or if
                                   ! origin time and P-phase (0 or 1)
      real        par2_ml_flow      ! low filter for ml-amp
      real        par2_ml_flow_pole ! number of poles for  ml low filter
      real        par2_ml_fhigh     ! high filter for ml
      real        par2_ml_fhigh_pole! number of poles for ml high filter
      real        par2_ml_amp_sn    ! minimum s/n for ml amp
      real        par2_sspec        ! do s-spectrum if S-phase, or P-phase
                                   ! and origin time (0 or 1)
      real        par2_sspec_sn     ! minimum s/n for S-spec
      real        par2_s_q0         ! Q0 for S, overrides par_q0 if set
      real        par2_s_qalpha     ! qalpha -------------par_qalpha ---
      real        par2_s_kappa      ! kappa --------------par_kappa----
      real        par2_s_flow       ! filt low for spec., overrides par_flow
      real        par2_s_flow_pole  ! number of poles ----------------------
      real        par2_s_fhigh      ! filt high-----------------------------
      real        par2_s_fhigh_pole ! number of poles ----------------------
c
c   station dependent parameters
c
      character*5 par2_stat(100)    ! station to select
      character*4 par2_comp(100)    ! component toselect
      real        par2_sta(100)     ! short term average
      real        par2_lta(100)     ! long term average
      real        par2_ratio(100)   ! trigger ratio
      real        par2_mincoda(100) ! minimum coda to select trace
      real        par2_dtrle(100)   ! 
      real        par2_fill(100)    ! filter low
      real        par2_filh(100)    ! filter high
c
c
c 
      common /hyp2_1/hyp2_model,hyp2_dist_id,hyp2_type,hyp2_fix_org,
     *             hyp2_depth_flag,hyp2_epi_flag,hyp2_mag_type,
     *             hyp2_high_accuracy,hyp2_error,hyp2_mag_type_all
      common /hyp2_4/hyp2_year,hyp2_month,hyp2_day,hyp2_hour,
     *             hyp2_min,hyp2_sec,hyp2_lat,hyp2_lon,hyp2_depth,
     *             hyp2_nstat,hyp2_rms,hyp2_mag,hyp2_mag_all,
     *             hyp2_gap,hyp2_sec_err,hyp2_lat_err,hyp2_lon_err,
     *             hyp2_depth_err,hyp2_cov,hyp2_auto
      common /hyp2_5/hyp2_agency,hyp2_mag_agency,hyp2_mag_agency_all
c
c 
      common /rea2_1/rea2_weight_in,rea2_onset,rea2_polarity
      common /rea2_2/rea2_co,rea2_weight_out
      common /rea2_4/rea2_comp,rea2_hour,rea2_min,rea2_sec,rea2_coda,
     *             rea2_amp,rea2_per,rea2_baz_obs,rea2_baz_cal,
     *             rea2_vel,rea2_ain,rea2_baz_res,rea2_dist,rea2_az,
     *             rea2_nstat,rea2_nphase,rea2_res,rea2_year,rea2_month,
     *             rea2_day,rea2_moment,rea2_nmag,
     *             rea2_sdrop,rea2_omega0,rea2_cornerf,rea2_radius,
     *             rea2_swin,rea2_vs,rea2_vp,
     *     rea2_q0,rea2_qalpha,rea2_kappa,rea2_density,rea2_slope,
     *          rea2_mc,rea2_ml,rea2_mb,rea2_ms,rea2_mw,rea2_geo_dist,
     *             rea2_nhead,rea2_nrecord,rea2_nspec,rea2_nhyp,
     *             rea2_id_line,rea2_nmacro,rea2_nwav,rea2_nfault,
     *             rea2_ncomment,
     *             rea2_macro,rea2_wav,rea2_fault,rea2_comment,
     *             rea2_av_moment,rea2_av_sdrop,rea2_av_omega0,
     *             rea2_av_cornerf,rea2_av_radius,rea2_av_swin,
     *             rea2_av_mw,rea2_av_slope,rea2_auto 

c
c
      common /rea2_5/rea2_stat
      common /rea2_8/rea2_phase,rea2_abs_time
      common /par2_4/par2_nstat,par2_vp,par2_vs,par2_q0,par2_qalpha,
     *  par2_kappa,par2_density,par2_comp,par2_sta,par2_lta,par2_ratio,
     *             par2_mincoda,par2_dtrle,par2_fill,par2_filh,
     *           par2_pick_p,par2_mb_amp,par2_mb_flow,par2_mb_flow_pole,
     *             par2_mb_fhigh,par2_mb_fhigh_pole,par2_mb_amp_sn,
     *             par2_pspec,par2_pspec_sn,par2_p_q0,par2_p_qalpha,
     *           par2_p_kappa,par2_p_flow,par2_p_flow_pole,par2_p_fhigh,
     *             par2_p_fhigh_pole,
     *         par2_ml_amp,par2_ml_flow,par2_ml_flow_pole,par2_ml_fhigh,
     *             par2_ml_fhigh_pole,par2_ml_amp_sn,par2_sspec,
     *             par2_sspec_sn,par2_s_q0,par2_s_qalpha,par2_s_kappa,
     *             par2_s_flow,par2_s_flow_pole,par2_s_fhigh,
     *             par2_s_fhigh_pole,par2_pick_s,
     *             par2_eventype,par2_coda,par2_flow,par2_flow_pole,
     *             par2_fhigh,par2_fhigh_pole
     
      
      common /par2_5/par2_stat

