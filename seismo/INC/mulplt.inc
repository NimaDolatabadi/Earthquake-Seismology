c
c                                                                               
c  common block for pic routines                                                
c                                                                               
c  j. havskov and c. lindholm 1988                                           
c                                                                               
c  The common blocks are for graphics routines communication and                
c  main program communications. For the main program, you only                  
c  need the common block PIC, although all can be included. This                
c  might however give problems if you do not change the names                   
c  since many common names have been used.                                      c  
c                                                                               
c  last update:                                                                 
c                                                                               
c  jan 91: major addition to include plot options                               
c  jun 94: use an include file to set dimentions, bug
c  oct 28: add chan_delay
c  dec  2, 94 : add colors
c  dec 29     : add unit for wavform file
c  feb 95     : add max_count
c  mar 21, 95 by jh: add resolu_x and resolu_hc
c  apr 21      : parameters for rotation
c  jul 25          : add dc_chan
c  oct 20 95       : sorting by distance
c  nov 1           : add nsort_distance_save
c  nov 17          : add ffmin,ffmax
c  jan 23 96       : add noise_spectrum
c  jan 31 96       : add nhead,nhrecord
c  aug 28 96       : add fhour
c  feb 10, 97      : add do_ms
c  feb 20          : add disp_vel
c  mar 19          : add auto_process, auto_process_name, auto_locate, 
c                    auto_update
c  oct 3           : 
c  nov 28          : add wave_out
c  feb 18, 98      : add pvelocity,spec_velocity, spec_phase
c  mar 18          : add yscale_mul
c  april 7         : add ypos_max,event_no
c  june 5          : default filters
c  june 24  lo     : include automatic coda parameters
c  oct 6   bmt     : linux changed  (common block order)  
c  nov 26 98  jh   : add variable kappa
c  may 19 99       : add fk
c  ep 20 99        : add edistance, edepth
c  sep 21 99       : add show_menu
c  sep 23 99       : add geo_distance  ! 1/geo_distance is  equivalent spreading
c  nov 28 99       : add channelname
c  dec 10          : add rspec
c  april 2000      : new wav structure
c  jan 2001        : add alt_weight
c  jan 23          : add vaiable ms,mb filters
c  feb 19, 2001    : filter_type added
c  mar 5, 2001, jh : sdistance changed from integer to real
c  nov 18          : add filter_true
c  nov 24, 02   jh : add channelname_org, nchan_org
c  apr 10  03      : add n_chan_screen, save filter values
c  may 2           : add chan_sort, n_screen,total_n_screen
c  dec 10  04   jh : add cont_window
c  jan12    05     : add onbox
c  feb 13  08   lo : add do_pmp
c  oct 2   09   jh : add do_mbb, do_mss
c  sep 28  10   jh : add spec_nphase, spec_phase,spec_tim
c  dec 29  11   jh : add mt_out
c  jan 22  12   jh : add recfil_pole,filt_old_low_pole,filt_old_high_pole and a lot more
c  mar 22  12   jh : add n_chan_screen_org
c  apr  4  12   jh : add mulplt_lat,mulplt_lon,mulplt_radius,mulplt_area, mulplt_area_save
c  jan  7  13   jh : add mulplt_stat
c  feb 11  13   jh : add mulplt_wav_out_format
c  feb 13  13   lo : add ymag
c  apr 9   13   jh : add spectral_model
c  mar 24  14   jh : add spectral_model_used
c  feb 23  16   jh : add current_mode
c  feb 25  16   jh : add filt_single
c  jan 04  17   lo : added port,myport
c  dec 18  17   jh : add spectrogram variables
c  mar 5   18   jh : add plot_distance
c
c       implicit none
c                                                                               
c   input to routine PIC                                                        
c
c-- get seisan dimensions
       include 'seidim.inc'
c-- get waveform variables
       include 'waveform.inc'                   
cx       real	signal(max_sample)
c-- number of samples to use                             
       integer	numb_samp	
c-- sample rate                                                 
       real	rate		
c-- no of keys defined for phases, if 0, def. only     
       integer   numb_keys	
c-- no of keys for weights, mac 10
       integer numb_weights
c-- user specified keys for phases,                  
       character*1 ascip(max_phase)	
c-- user specified phase names corr. to ASCIP          
       character*9 phs(max_phase)	
c-- asci value for mouse left click                       
       integer   mouse		
       integer show_menu   ! save if menu shown
       integer nchan_org   ! original selected number of channels
       integer nchan_selected ! number of channels selected by clickin on traces
       integer n_chan_screen ! max number of channels per screen
       integer n_chan_screen_org ! original max number of channels per screen
       integer total_n_screen  ! total number of screens to plot selected channels
       integer n_screen      ! current screen number
       logical chan_sort     ! to sort channel alphabetical or not
       logical forward,backward
       real plot_distance    ! 0: no, 1: epicentral, 2: hypocentral

       logical mt_out        ! output data for mt

       integer recfil_pole   ! number of poles used by recfil routine
                                                                                
c                                                                               
c   input and output to routine PIC                                             
c                                                                               
c-- number of phases in or out of PIC routine, one channel               
       integer	nphas		
c-- number of phases in or out of PIC routine, all channels, one station               
       integer	spec_nphas	
c-- names of phases inc. i and e, one channel                    
       character*9 phase(max_phase)
c-- names of phases inc. i and e, all channels, one station                    
       character*9 spec_phs(max_phase)
c-- weight if not part of phase name, for long phase names
       character*1 alt_weight(max_phase)
c-- times(sec) of pick rel. first sample in SIGNAL,one channel         
       real	pictim(max_phase)	
c-- times(sec) of pick rel. first sample in SIGNAL, all channels, one station         
       real	spec_tim(max_phase)	
c-- amplitude,  zero to peak                        
       real     amplitude(max_phase)	
c-- period (sec)                                           
       real	period(max_phase)	
c-- azimuth to event, not changed, can be deleted         
       real	azimuth(max_phase)	
c-- apparent velocity, not changed, can be deleted       
       real	velocity(max_phase)	
c-- rest of phase line to be saved
       character*23 data_end(max_phase)
c-- plot header for top plot                      
       character*80 head
c-- y-scaling factor each trace in multi trace mode        
       real yscale_mul(max_trace)
c-- magnification of plot in y direction used for phase picking
       real ymag
c-- list of channels used
       integer channelname(max_trace)
       integer channelname_org(max_trace)
c-- if true, filter permanent
       logical filter_perm
       integer filt_single  ! if 1, use the default filter in single
                                                                                
c                                                                               
c   communication between pic routines                                          
c
c-- operating mode 0: data base, 1: single waveform files
       integer opmode
c-- readings
       character*80 data(max_data)                                                                               
c-- number of records and number of headers in data
       integer nrecord,nhead
c-- local data vector after filter, selection etc.        
       real 	y(max_sample)
c-- for spectral analysis
       complex  com(max_sample/2) 
c-- max of y after removal of dc, one window                 
       real     max		
c-- correcsponding dc
       real dc_chan
c-- max of whole trace, used if more pages plotted       
       real     max_all		
c-- max count fore scaling if fixed scale
       integer max_count
c-- factor to scale plot in y after auto scale             
       real	over_scale	
c-- # of tectronics tics pr second of current plt.        
       real     xscale		
c-- -------------------- amp unit --------------          
       real     yscale		
c-- number of points to plot horizontally, current,x and hc plot          
       integer  resolu,resolu_x,resolu_hc		
c-- first and last time selected when zooming            
       real time1,time2		
c-- time (second or min) of start of plot window                   
       real     fsec,fmin,fhour		
c-- first and last point of data vector to use          
       integer	first,last	
c-- x,y-position of lower left corner of plot box           
       real	xpos,ypos
c-- maximum ypos in multi trace mode
       real max_ypos	
c-- height of plot box                                        
       real	height		
c-- keys for weights                            
       character*1 key_weight(10) 
c-- actual asci chars for weight, 1,2,3......     
       character*1 i_weight(10)
c-- choice of next action                               
       character*4 choice	
c-- filter indicator, 0: not use, 1,2,3 : see prog           
       integer	filt
c-- recursive time domain filter type, 0 for bndpas, 1 for recfil
       real	filter_type
c-- default filters for option 0
       real flow_def,fhigh_def
c-- filter passes, 1 foreward, 2 both ways+ the saved value
       integer npasses,npasses_old		
c-- lower cutoff frequency of fixed filters and poles                  
       real	flow(9)
       integer  flow_pole(9)
c-- higher cutoff frequency of fixed filters and poles                
       real	fhigh(9)
       integer  fhigh_pole(9)
c-- (0/1) 1 ==> system response removed           
       integer remove_response
       integer disp_vel   ! type of response 1: disp., 2: velocity, 3: acceler. 
c-- do spectrum, 3com analysis and wood anderson response, etc
       logical do_spectrum,do_3comp,do_wa,do_mb,do_ms,do_pmp,do_mbb,
     * do_mss
c-- velocity used for 3 component analysis
       real three_comp_velocity
c-- length in secs of one window plotted                    
       real	page_time	
c-- delay in seconds before starting plot, current channel                 
       real	trace_delay
c-- delay in seconds of current channesl with respect to main header
       real chan_delay	
c-- start time of page relative to main header
       real page_start
c-- main header
       character*80 mainhead(max_trace)
c-- channel header
       character*1040 chead
c-- last picked azimuth and velocity
       real azim,veli
c-- last values from phase pick, character and position
       integer last_ix,last_iy,last_ich
c-- current channel active in traceplot
       integer current_chan
c-- current sequential channel active in traceplot
       integer current_seq_chan
c-- sorting by distance
       integer nsort_distance,nsort_distance_save
c-- make noise spectrum or not: 0: no, 1: yes, 2: indicate signal is made, now
c   ready for noise spectrum
       integer noise_spectrum
       integer onbox(50)     ! for menu
c port to talk to SE
       integer port,myport
 
c
c   spectral values
c
       real q0            ! q0 for Q
       real kappa         ! near field attenuation constant
       real qalpha        ! frequency dependence of q, q = q0**qalpha
       double precision travel_time   ! travel time used in Q correction
       double precision ffsec         ! waveform file start time
       real stime         ! start of spectrum measured from wavform start time
       real omega0        ! spectral level in displacement spectrum
       real moment        ! log seismic moment
       real mw            ! moment magnitude
       real sdrop         ! sress drop in bars
       real cornerf       ! corner frequency in hz
       real sslope        ! decay of spectrum
       real svelocity     ! s-velocity 
       real pvelocity     ! p-velocity
       real spec_velocity ! used for spectrum
       character*1 spec_phase ! neares phase to spectrum
       real density       ! density
       real radius        ! source radius in km
       real sdistance     ! hypocentral distance
       real edepth        ! depth
       real edistance     ! epicenter distance
       real geo_distance  ! equivalnt geometrical sperading distance
       real swindow       ! window length in secs for spectrum
       real ffmin,ffmax   ! frequeny band for spectrum plotted
       real spectral_model !if 1.o use model instead of individual parameters
       integer spectral_model_used ! the actual model used
       character*80 spectrogram_command ! command for spectrogram
       real spectrogram_window      ! window lenght for  ---
       real spectrogram_filter      ! filter for -----------
       real spectrogram_plot_trace  ! plot trace -----------
c
c   others
c
       integer unit_wave
       real    baz(max_trace) ! backazimuth angle of current trace
       logical rotate      ! if true, do component rotation if possible
       logical flip_rotate ! true if rotate flipped
       real    auto_locate ! locate after register 0:no, 1: yes, 2:yes/prompt
       real    auto_update ! update after register 0:no, 1: yes, 2:yes/prompt
       real    auto_process! run process after register 0: no, 1: yes, 2:y/prompt
       character*14 eev_start_time  ! start time from eev
       character*14 eev_end_time  ! end time from eev
       character*5  seisan_base ! seisan 5 letter data base
       character*10 auto_process_name   ! auto process name
       real spec_out       ! switch for spectral output
       real ms_low,ms_high  ! ms filter
       real mb_low,mb_high  ! mb filter
       real ml_high,ml_low               ! wa filter
       integer ms_low_pole,ms_high_pole ! ms poles
       integer mb_low_pole,mb_high_pole ! mb poles
       integer ml_high_pole,ml_low_pole                   ! wa or ml pole
       logical wave_out    ! if true, write waveform file out
       integer event_no    ! eev event number
       REAL    mulplt_area ! if > 0, use area selection
       real mulplt_lat,mulplt_lon ! center for area selection
       real mulplt_radius         ! radius for area selection
       integer mulplt_area_save   ! if >0, values have been saved
       character*5 mulplt_stat    ! distance from this station
       character*80 current_mode  ! info if all comp mode etc

       real coda_h,coda_l  ! high and low filter for automatic coda
       real coda_sta       ! length of short term window
       real coda_ratio     ! ratio of s/n to find coda end
       real auto_coda_time ! coda time relative to trace start
       logical do_auto_coda! is coda done automatically
       logical fk          ! true if fk analysis
       logical rspec       ! true if response spectrum analysis
       logical cont_window ! true if cont window changed
       character*80 sfile  ! s-filename
       character*10 mulplt_wav_out_format ! ouptut format of extract
c
c   next to save filter values
c       
       integer  filt_old
       real     filt_old_low,filt_old_low_pole
       real     filt_old_high,filt_old_high_pole
       integer  disp_vel_old
       integer  remove_response_old
       logical  do_wa_old
       logical  do_mb_old
       logical  do_mbb_old
       logical  do_ms_old
       logical  do_mss_old  
       real     ml_low_old
       real     ml_high_old   
       real     mulplt_multi_label

       logical fromse     ! call from se
c
c spetrogram section
c
       integer spectro_ntimes ! number of time slices
       integer spectro_nf     ! number of f slices
       integer spectro_ncol   ! number of colors
       real spectro_rate      ! inverse of time spacing
       double precision spectro_abstime ! absolute start time
       real spectro_amp(200,50)! spectrogram amplitudes
       character*20 spectro_col(200,50)! spectrogram colour
       integer spectro_colind(200,50)! spectrogram colour
       common /spectrogramcb/spectro_col,spectro_abstime,
     &         spectro_rate,spectro_ntimes,spectro_nf,
     &         spectro_colind

c  
c
c                                                                               
c  plot options                                                                 
c                                                                               
c-- 1: plot frame, 0: no frame, 2: sides only              
       integer	pframe		
c-- 3: top and sides                              
                                
c-- 1: plot axis tics down, 2: plot axix tics top          
       integer  paxis		
c-- 3: tics both up and down 0: no axis tics      
                                
c-- 1: plot axis numbers, 0: no axix numbers            
       integer  paxisnumb	
c
       integer  ptitle		
c-- 1: 80 char title on top one line above frame          
c   2: 80 char title on frame                     
c   3: 3 * 8 char at beginning of plot            
c   0: no title                                   
c
       integer	ppick		
c-- 0: plot old pics and go in picking mode
c   1: plot old pics, do not go in picking mode
c-- 2: do not plot old picks and go in picking mode, 0: no picks                          
c
       integer  pmax            
c-- 1: plot max below (plotw), 0: no max          
c   2: plot left end (traceplot)                  

c-- 1: plot help texts, 0: no help texts          
       integer  phelp  
c                                                                               
c   common block for main routine PIC                                           
c                                                                               
       common/pic_main4/numb_samp,rate,                      
     *            numb_keys,numb_weights,mouse,                    
     *            nphas,spec_nphas,pictim,spec_tim,amplitude,                 
     *            period,azimuth,velocity,head,
     *            nrecord,nhead,yscale_mul,channelname,
     *            channelname_org,nchan_org,n_chan_screen,n_screen,
     *            total_n_screen,n_chan_screen_org                                  
       common/pic_main1/ascip,filter_perm            
       common/pic_main9/phs,phase,spec_phs
       common/pic_main23/data_end                                                        
c                                                                               
c   common blocks for pic routines communication                                 
c                                                                               
       common/pic_com1/y,max,max_all,xscale,yscale,ymag,
     *            resolu,time1,time2,
     *            first,last,xpos,ypos,max_ypos,height, 
     *            over_scale,disp_vel,
     *	          filter_type,filt_single,                               
     *            choice,filt,npasses,flow,fhigh,remove_response,
     *            page_time,trace_delay,fsec,fmin,fhour,page_start,
     *            chead,mainhead,opmode,azim,veli,
     *            last_ix,last_iy,last_ich,current_chan,
     *            chan_delay,max_count,resolu_x,resolu_hc,
     *            current_seq_chan,dc_chan,nsort_distance,
     *            nsort_distance_save,show_menu,flow_pole,fhigh_pole,
     *            npasses_old
       common/pic_com1a/key_weight,i_weight,
     *            do_spectrum,do_3comp,do_wa,do_mb,do_ms,do_pmp,do_mbb,
     *            do_mss
      common/pic_com2/com,unit_wave,flow_def,fhigh_def,onbox
      common/pic_com3/data                       
c                                                                               
c   common block for plot options                                               
c                                                                               
       common/picoption/pframe,paxis,paxisnumb,ptitle,ppick,pmax,phelp          
c
c   common block of spectral values and 3 component analysis
c
       common/spectral/q0,qalpha,omega0,moment,mw,sdrop,
     *                  cornerf,sslope,svelocity,density,sdistance,
     *                  edepth,edistance, geo_distance,
     *                  swindow,stime,radius,three_comp_velocity,
     *                  ffmin,ffmax,noise_spectrum,pvelocity,
     *                  spec_velocity,kappa, spectral_model,
     *                  spectral_model_used
       common/spectral8/travel_time,ffsec
       common/spectral1/spec_phase
c
c   common block for other parameters
c
       common/otherpar/auto_locate,auto_process,
     *                 auto_update,spec_out,auto_coda_time,
     *                 coda_sta,coda_h,coda_l,coda_ratio,event_no,baz,
     *                 ms_low_pole,ms_high_pole,
     *                 mb_low_pole,mb_high_pole,
     *                 ml_high_pole,ml_low_pole,
     *                 ms_low,ms_high,  
     *                 mb_low,mb_high,  
     *                 ml_high,ml_low,sfile,
     *                 filt_old,filt_old_low,
     *                 filt_old_high,disp_vel_old,
     *                 filt_old_low_pole,filt_old_high_pole,
     *                 remove_response_old,
     *                 recfil_pole,nchan_selected,
     *                 ml_low_old,ml_high_old,mulplt_area,mulplt_lat,
     *                 mulplt_lon,mulplt_radius, current_mode,
     *                 plot_distance
c  
       common/otherpar10/auto_process_name,mulplt_wav_out_format,
     *          spectrogram_command,spectrogram_window,
     *          spectrogram_filter,spectrogram_plot_trace

       common/otherpar1/wave_out,do_auto_coda,fk,rspec,rotate,
     *                  flip_rotate,alt_weight,chan_sort,
     *                  forward,backward,
     *                  cont_window,mt_out,do_wa_old,do_mb_old,
     *                  do_mbb_old,do_ms_old,do_mss_old,
     *                  mulplt_area_save  
       common/otherpar14/eev_start_time,eev_end_time
       common/otherpar5/mulplt_multi_label,seisan_base,mulplt_stat
       common/udp/port,myport
       common/se/fromse

