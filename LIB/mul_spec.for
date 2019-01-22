c
c   spectral calculations in mulplt
c
c   problems: seems like corner frequnecy-stress drop is no calculated correctly
c                   for spectral modelling , jh ocy 07


c   updates
c   
c   jun 6 by jh : add mb filter
c   jul 25      : do not calculate moment etc if no distance
c   dec 28      : ******** version 5.0 ********************* (no change)
c                 put in new xy_plot routine
c   fef 95      : -------------------------- with axis text
c   mar 21      : bug in get_3_chan
CJAB(BGS)Apr95  : Give prompt text the prompt text colour in remove_resp, so 
CJAB(BGS)Apr95.   that it always shows.
c   apr 21 95 jh : ratation in get_3_chan
c   april 28  jh : az bug
c   jun 7        : only ask once for disp vel in multitrace mode
c   nov 17       : ffmin and ffmax as parameters from MULPLT.DEF
c   dec 95       : plot response
c   jan 23   96  : noise spectrum
c   jan 31       : spectral fitting
c   feb 14       : enter moment interactively
c   mar 20       : message if response from header
c   jul 25       : fix picking error if 3 comp
c   aug 2        : do not calculate hbypocentral distance, now in convert
c                  use seiget for spectral modeling
c   sep 24       : power spectrum
c   dec 96       : remove all reference to depth in calc moment
c   feb 97   97  : ms response
c   feb 19       : new message in resp and spec
c   feb 20       : remove disp_vel from call and spectrum input, put in
c                  mulplt.inc
c   sep 22       : limit wa response to 20 hz
c   oct          : spectral output
c   oct 29       : check if no zero lenght spectrum 
c   nov 30       : do not ask question if waveform out
c   jan 23 98    : do not save chead since now done in sys_resp
c   feb 17       : small changes to selection
c   feb 18       : fix spectral bug with p spectra, auto selection of velocity
c-----------------------------------------------------------------------------
c   sep 98 jh    : ----------- year 2000 check -----------------------------
c                  year 2000, 5 character station code
c   nov 4  jh    : linux logig
c   nov 5        : change c-routines having '_'
c   nov 26 jh    : change use of kappa, also kappa in spectrum
c   dec 18       : bug turning off permanenly response removel if one trace
c                  did not have a response file
c   march 17, 99 : bug in 3 comp
c
c   mar 24 bmt   : clear input in oneline
c   may 27 jh    : hardwire  petersen noise curves
c   may 2        : fix noise spectrum level                
c   jul 14       : put correct petersen curves
c   aug 23       : do not correct for q and kappa when making noise spectra
c   sep 19       : add routien spec_dist, change geometrical spreading calcula
c   sep 23       : add geo_distance
c   sep 27       : put kappa correction back in, where did it go ????
c   nov 9 99 jh  : do not calculate moment if not displacment spec
c                  clean up display a bit
c   nov 26       : add date to response plot, shift a bit dec 6
c   dec 08 lo    : in get_3_chan read more then 30 stations from header
c   mar 30 00 jh : fix text for plotting response file
c   oct 26       : frequency limits in spectral plotting is determined by
c                  filter used, overdides defualts
c   dec  14      : scale power noise spectra after peterson curves
c   dec  17      : choice of lin x-axis in plot, chose fmin, fmax, t*
c   jan 2001     : remove mulplt.inc from prepare, spectrum, rem_resp
c                  remove prpare, remove_resp, pectrum precoh1,cross
c                  and rotate_comp form this file
c   feb 2001 bgs : bgs added choise of axis types and what else ?
c                  t* disabled again
c   mar 28     lo: seesm that lars corrected the power spectrum
c   may 27     jh: make log-log the defualt spectrum again
c   june  10   jh: some correction to above
c   june 22    lo: set dimension of nnspec in amp_spec to 3
c   jul 10     jh: add output of modeled spectrum in spectral_fit.out
c   sep 10       : add selection of type of spectrum
c   sep 21       : more spectrum selection fixing
c   jan 26 04  jh: if not 3 channels, do not refesh readings, were lost 
c   nov 22 04  jh: plot whole response type instead of one letter
c   sep 3  07  jh: check for p or s velocity when spectrun do not have
c                  a phase read
c   oct 12 07  jh: fix new way of calculating source radius
c   feb 13 08  lo: add particle motion
c   oct 29 08  jh: bettere message if spectral limit too small
c   dec 29 09  jh: add paz to call spectrum, not used
c   apr 09 10  jh: zero pole_low and pole_high after call to spectrum
c   oct 07 10  jh: make sure spectral parmater are not read on power spectrum
c   dec 27 10  jh: gfortran on pc, change unit 15 to 25
c   apr 01 11  jh: small letters for spectral choise
c   nov 20 12  jh: spectral choice c did not work, add density input
c   mar 22 13  jh: array overflow
c   apr 02 13  jh: auto spectral fit
c   apr 22 13  Jh: mode fix in above
c   mar 20 14  jh: add 1_below_1hz to spec fit
c   mar 24 14  Jh: move aroud reading of spectral model and fix some 
c                  parameter display
c   may 31 14  jh: more above, now only aske for undefined phase
c                  in mul_spec to not ask when making noise spectrum
c   jun 12 14  jh: update phase list after call convert after
c                  spectral analysis to avoind doubling of phases
c   jan 22 18  jh: multichannels spectrogram
c   feb 5  18  jh: fix white lines in spectrogtam on windows
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc             
      subroutine amp_spec                                                 
c                                                                               
c     routine to make a real spectrum                
c     The tapering of  the data is fixed to 10%                                 
c                                                                               
c     First written by C. Lindholm Jan. -90  Jh june 93                                         
c
      implicit none                                                                               
c                                                                                
c-- common block     
c                                
      include 'mulplt.inc'			
      include 'libsei.inc'
      include 'seiplot.inc'
      include 'seisan.inc'


c
c-- percentage of tapering (fixed)                       
      real taper/10./				
c-- length of input vector                                 
      integer nsamp				
c-- length after padding                                    
      integer ipow				
c-- vector padded with npad zeros                           
      integer npad				
c-- indicator for response removal or not (1 or 0).                        
      integer rem_resp			
      real ff ! frequency
c-- number of points in spectrum
      integer nspec
      real f_low,f_high
      integer pole_low,pole_high
      real fflow,ffhigh 
      double precision phase_time  ! abs time of start of phase window
c
c  next is not used here
c
      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros
c
c  for auto spec
c
      logical auto_spec            ! true if auto sepctral fit
      real res_m_a(2)              ! for automatic spec par
      real ka                      ! computed delta kappa, auto spec, not used
      real dkappa                  ! for auto spec
      real omi,f0i                 ! -------
c      real ffixl,ffixh             ! fixed f band
      real fsn1,fsn2               ! frequency range of good sn
      integer smodel               ! type of spectral model 1: spec model, 
c                                  2: mulplt.def. 3: def

c-- if several data sets, number of points in each

      integer nnspec(3)   
      integer xy_cursor             ! for call to xy_plot
      real xlength                  ! x-axis length for spectrum
      real save_q0,save_kappa       ! temporary storage
c-- counters and help variables                                               
      integer i,j
      real aa,bb,corr,rms           ! output from least squares
      real tstar                    ! tstar atteneuation, same as kk
      character*80 text, text1
      character*30 xtext,ytext      ! axis text
      character*1 ch                ! retuern from spectral type select
      character*1 ch1               ! return from axes type select (bjb 2001/02/22)
      real xx,yy                    ! help variables
      real amp_average              ! average log amplitude spectra
      real norm_factor              ! normalization factor for source fit
      real qq,qa,kk,f0              ! source test parameters:Q0, qalpha,diminu
                                    ! -ation parameter, cornor frequency
      integer code                  ! return status code
c--returns from spectral plot
      character*1 c_spec(20)
      character*80 message(15)      ! text to screen
      character*80 blank(15)        ! blank text strings
      real average_moment           ! moment read from data
      real ssdrop                   ! sress drop calculated in spectral fitting
      real sei real num             ! function
      real x0                       ! left corner of spectrum
c-- origin time
      integer oyear,omonth,oday,ohour,omin
      real osec
      double precision otime
      real cx(20),cy(20)
      integer nc,k
      real geo_factor     ! geometrical spreading factor
      real x_work(max_sample)
      logical power    ! if true, make power spectrum
      logical linx     ! if true, linear x-axis in spec
c-- axis type variable for spectra
c-- 1=Log-Linear
c-- 2=Log-Log
c-- 3=Linear-Linear
      integer iaxes_type
      real spec_velocity_new   ! new velocity in case entered interactively
      real spec_density_new    ! ---  density ----------------------------
      character*80 answer
      logical sun,pc,linux
      real low_f(22),high_f(14),low_l(22),high_l(14) ! petersen noise
      equivalence(x_work,com)
c
c   petersen noise curves, log f and db
c
      data high_f/   1.000000,    6.575773E-01,    4.948500E-01,
     *           9.691001E-02,   -5.797836E-01,   -6.627579E-01,
     *           -7.993407E-01,  -8.976271E-01,   -1.187521,
     *           -1.301030,       -2.549984,       -4.000000,
     *            -4.477121, -5.0/
      data high_l/ -91.5,-97.4,-110.5,-120.0,-97.9,-96.5,-101.0,
     *             -113.5,-120.0,-138.5,-126.0,-80.1,-65.0,-48.5/ 

      data low_f/    1.000000,    7.695511E-01,    3.979400E-01,
     *             9.691001E-02, -9.342168E-02,   -3.802112E-01,
     *            -6.334685E-01,  -6.989700E-01,  -7.781512E-01,
     *             -1.000000,       -1.079181,       -1.193125,
     *              -1.340444, -1.50, -1.65, -1.89, -2.0,
     *              -2.19,-2.51,-2.78,-4.0,-5.0/
      data low_l/   -168.0,-166.7,-166.7,-169.2,-163.7,-148.6,
     *              -141.1,-141.1,-149.0,-163.8,-166.2,-162.1,
     *              -177.5,-185.0,-187.5,-187.5,-185.0,-184.9,
     *              -187.5,-184.4,-151.9,-103.1/ 
c
      do i=1,15
        blank(i)=' '
      enddo
      linx=.false.      ! normally log x axis
      call computer_type(sun,pc,linux)
      spec_velocity_new=-1.0    ! no new velocity yet
      spec_density_new=-1.0    ! no new density yet
c
c   check if enough points
c
      if((last-first).lt.20) then
         text=' Window too short for transformation, nothing done'
         call xmessage(text,1,80,10.0,60.0)
         return
      endif
                 
      average_moment=0.0
c-- indicator for type of spectrum fixed to 1 for displacement                 
      disp_vel=1			

      iaxes_type=2  ! defualt log-log axis
c
c   display general parameters
c
 400  continue


c
c   find if p or s spectrum
c
      if(spec_phase.eq.'P') spec_velocity=pvelocity
      if(spec_phase.eq.'S'.or.spec_phase.eq.'N') spec_velocity=svelocity
      if(spec_phase.eq.'N') spec_phase='?'  ! unknown spectrum
c
c   check if change of velocity or density
c
      if(spec_velocity_new.gt.0.0) spec_velocity=spec_velocity_new
      if(spec_density_new.gt.0.0) density=spec_density_new

c      write(6,*) 's used', spectral_model_used
      message(1)='General parameters, '//spec_phase//'-spectrum'
      write(message(2),'(a,f5.1,a,f5.2,a,i5,a,i4)')  'Vel ',
     *  spec_velocity,
     *' Dens',density, ' Dist',int(edistance),' Depth',int(edepth)
      if(spectral_model.eq.0.0) 
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Mulp model'
      if(spectral_model.eq.1.0.and.spectral_model_used.eq.1.0)
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Spec model'
      if(spectral_model.eq.1.0.and.spectral_model_used.eq.3.0)
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Def model'
      if(spectral_model.eq.1.0.and.spectral_model_used.eq.2.0)
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Mulp model'
       write(message(4),'(a,2f7.3)')'Spectral frequency band ',
     * ffmin,ffmax
  
       call xmessage(message,4,45,620.0,50.0)   

c
c   first check if response is to be removed and if available
c
      rem_resp=0
      message(1)='Displacement:    d'
      message(2)='Velocity:        v'
      message(3)='Acceleration:    a'
      message(4)='Raw spectrum:    r'
      message(5)='Change velocity: c'
      message(6)='Change density:  e'
      message(7)='Change moment:   m'
      message(8)='Noise Pow. spec: n'
      message(9)='Lin axis:        l'
      message(10)='New spec f-lim.  f'
      message(11)='Type of spectr.  t'
      message(12)='Autofit spectrum s'

      call xmessage(message,12,40,620.0,145.0)
c
c   get answer
c
      call xscursr(i,xx,yy)
      ch=char(i)
      if(ch.eq.'c'.or.ch.eq.'C') then
         call xmessage(blank,12,40,620.0,145.0)
	   text = '                    '
         call oneline('New velocity',12,text,20,620.0,300.0)
         if(text.ne.' ')read(text,*) spec_velocity_new
c        write(6,*) spec_velocity_new
c
c  below does not seem to be needed, jh nov 20, 2012
c
c        if(sun.or.linux) call xscursr(i,xx,yy)  ! on sun buffer not cleared, why ?
         goto 400   ! start again
      endif
c
c   density
c
      if(ch.eq.'e'.or.ch.eq.'E') then
         call xmessage(blank,12,40,620.0,145.0)
           text = '                    '
         call oneline('New density',11,text,20,620.0,300.0)
         if(text.ne.' ') read(text,*) spec_density_new
         goto 400   ! start again
      endif


      if(ch.eq.'f'.or.ch.eq.'F') then
         call xmessage(blank,12,40,620.0,145.0)
	   text = '                    '
         call oneline('Spec fmin, fmax',15,text,20,620.0,300.0)
         call sei get values(2,text,code)
         ffmin=array$(1)
         ffmax=array$(2)
c         if(sun.or.linux) call xscursr(i,xx,yy)  ! on sun buffer not cleared, why ?
         goto 400   ! start again
      endif

       if(ch.eq.'l') then
c
c Ask for axes type log-log or log-linear or linear-linear (bjb 01/03/2000)
c
           message(1)='Log(x)-Linear(y)    1'
           message(2)='Log(x)-Log(y)       2'
           message(3)='Linear(x)-Linear(y) 3'
           message(4)='Linear(x)-Log(y)    4'
     
           call xmessage(message,4,30,800.0,275.0)
c
c   get answer
c
           call xscursr(i,xx,yy)
           ch1=char(i)
           call xmessage(blank,3,30,800.0,275.0)

           iaxes_type=1
           if(ch1.eq.'1') iaxes_type=1 
           if(ch1.eq.'2') iaxes_type=2
           if(ch1.eq.'3') iaxes_type=3
           if(ch1.eq.'4') iaxes_type=4
           goto 400
      endif
c
c   moment
c
      if(ch.eq.'m'.or.ch.eq.'M') then
         call xmessage(blank,12,40,620.0,145.0)
	   text = '                    '
         call oneline('New moment',12,text,20,620.0,300.0)
         read(text,'(f8.2)') average_moment
         goto 400   ! start again
      endif
c
      if(ch.eq.'t'.or.ch.eq.'T') then
 7654    continue
         call xmessage(blank,12,40,620.0,145.0)
	   text = '                    '
         call oneline('Enter p or s for type',21,text,5,620.0,300.0)
         spec_phase=text(1:1)
         if(spec_phase.eq.'p') spec_phase='P'
         if(spec_phase.eq.'s') spec_phase='S'
         if(spec_phase.ne.'P'.and.spec_phase.ne.'S') goto 7654
         goto 400   ! start again
      endif
c
c   final clear before going on
c
      call xmessage(blank,12,40,620.0,145.0)

      auto_spec=.false.
      if(ch.eq.'s') then
         auto_spec=.true.  ! auto spectral fit
         ch='d'
      endif
         
      disp_vel=0
      power=.false.

      if(ch.eq.'A'.or.ch.eq.'a') disp_vel=3
      if(ch.eq.'V'.or.ch.eq.'v') disp_vel=2
      if(ch.eq.'D'.or.ch.eq.'d') disp_vel=1
      if(ch.eq.'D'.or.ch.eq.'V'.or.ch.eq.'A') power=.true.
      if(ch.eq.'n'.or.ch.eq.'N') then
        power=.true.
        disp_vel=3
        ch='N'
      endif


      if(disp_vel.gt.0) rem_resp=1
      if(rem_resp.eq.1) then
         wav_resp_file = ' '
         call read_resp
         if(wav_resp_status(1:1).eq.'9') then
            write(text(1:20),'(a)')'No response info ***'
            call tchars(text,20,620.0,225.0)
            rem_resp=0
         endif
         if(wav_resp_status.eq.'8') then
            write(text(1:33),'(a)')'Response from waweform header ***'
            call tchars(text,33,620.0,320.0)
         endif
         if(rem_resp.eq.0) then
            write(text(1:25),'(a)')'Press any key to continue'
            call tchars(text,25,620.0,195.0)
            call xscursr(i,xx,yy)
            choice='REPL'
            return
          endif           
           
      endif
c
c   check if phase given for displacment spectrum
c
      if(disp_vel.eq.1.and.spec_phase.eq.'?') then
 9765        continue
             text='Spectrum not defined, enter p or s'
             call oneline(text,35,answer,2,500.0,500.0)
             spec_phase=answer(1:1)
             if(spec_phase.eq.'p') spec_phase='P'
             if(spec_phase.eq.'s') spec_phase='S'
             if(spec_phase.ne.'S'.and.spec_phase.ne.'P') goto 9765
c
c   parmeter must be defined again since phase was not defined
c

c
c   if spectral model set, all parameters come from spec model and overides values
c   from MULPLT.DEF. q0, qalpha and kappa are returned for p or s repectively.
c   if no spectral model, then use mulplt.def values
c
             spectral_model_used=2   ! def from mulplt.def
             if(spectral_model.eq.1.0) 
     *       call get_att_vel(edepth,spec_phase,q0,qalpha,kappa,
     *       pvelocity,svelocity,density,spectral_model_used)     

c
c   check for velocity, added by jh sep 3 2007
c
             if(spec_phase.eq.'P') spec_velocity=pvelocity
             if(spec_phase.eq.'S'.or.spec_phase.eq.'N')
     *       spec_velocity=svelocity

c   check if change of velocity 
c
             if(spec_velocity_new.gt.0.0) spec_velocity=
     *       spec_velocity_new
      endif

c                                                                               
c--------put part of the signal into local datavector, can be noise or signal
c
 1    continue                  ! back here if a noise spectrum made
c                                                                               
      if(noise_spectrum.eq.2) then   ! use start of signal for noise spectrum
         last=last-first
         first=1
         do i = 1,last                                                         
            y(i)=signal1(i)                                                              
         enddo
      else                      ! use selected time window
         j =  0                                                                    
         do i = first,last                                                         
            j = j + 1                                                               
            y(j) = y(i)                                                            
         enddo                                                                     
      endif
                                                                                
      nsamp = last-first+1                                                                 
c                                                                               
c------- prepare data.     Pad with zeros and taper 10%.                        
c                                                                               
      call prepare(nsamp,y,taper,ipow,npad,com)                                
c
c   check for max number of points
c
      if(ipow.ge.max_sample/2) then
           call clear_to_alpha
           write(6,*)' Too many points in spectrum'
           write(6,*)' Max number is ', max_sample/2
           stop
      endif                                

c
c   calculate travel time for q correction, first get origin time
c   but only if running from data base (opmode = 0)
c
      if(opmode.eq.0) read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')                                     
     *oyear,omonth,oday,ohour,omin,osec                                         
c                                                                               
c   get absolute origin time in secs                                            
c                                                                               
      if(opmode.eq.0) 
     *call timsec(oyear,omonth,oday,ohour,omin,osec,otime)                      
c
c   calculate travel time. ffsec is waveform file abs start time from
c   routine convert in mulplt, stime is time from start of wave form file
c   to first sample in spectral window
c
      if(opmode.eq.0) then
         travel_time=ffsec-otime+stime
         phase_time=otime+travel_time
      endif
c
c   put in some defaults if opmode not 0
c
      if(opmode.ne.0) then
         travel_time=1.0
      endif

c                                                                               
c-------- Calculate the spectrum, and remove the system response
c         if desired. If noise spectrum, do not use q and kappa                
c              
      if(ch.eq.'N') then
         save_q0=q0
         save_kappa=kappa
         kappa=0
         q0=0          ! do not use q and kappa
         spec_phase='N'
      endif
c
c   rewrite message since it might have changed
c

      message(1)='General parameters, '//spec_phase//'-spectrum'
      write(message(2),'(a,f5.1,a,f5.2,a,i5,a,i4)')  'Vel ',
     *  spec_velocity,
     *' Dens',density, ' Dist',int(edistance),' Depth',int(edepth)
      if(spectral_model.eq.0.0) 
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Mulp model'
      if(spectral_model.eq.1.0.and.spectral_model_used.eq.1.0)
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Spec model'
      if(spectral_model.eq.1.0.and.spectral_model_used.eq.3.0)
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Def model'
      if(spectral_model.eq.1.0.and.spectral_model_used.eq.2.0)
     * write(message(3),'(a,i4,a,f4.1,a,f4.2,a,f4.1,a)')
     *'Q0 ',int(q0),' Qalp',
     *   qalpha,' k ',kappa,' q<1Hz',q_below_1hz,' Mulp model'
       write(message(4),'(a,2f7.3)')'Spectral frequency band ',
     * ffmin,ffmax 
       call xmessage(message,4,45,620.0,50.0)   



      pole_low=0
      pole_high=0

      if(filt.gt.0) then
         f_low=flow(filt)
         f_high=fhigh(filt)
         pole_low=8
         pole_high=8
      endif
      call spectrum(com,ipow,disp_vel,rem_resp,rate,
     +     f_low,f_high,pole_low,pole_high,q0,qalpha,
     *     kappa,travel_time,zero,pole,nzero,npole,norm)
c
c   zero filter   jh apr 9 2010
c
       pole_low=0
       pole_high=0


c
      if(ch.eq.'N') then          ! put values back in
         q0=save_q0                            
         kappa=save_kappa
         iaxes_type=2             ! only allow log-log
      endif

c
c   write out spectrum if not a noise spectrum and flag set
c
      if(noise_spectrum.lt.2.and.abs(spec_out).eq.1) then
          open(25,file='com_spec.out',status='unknown')
          write(25,*) ipow,rate,swindow
          write(25,'(4E15.8)')(com(i),i=1,ipow/2+1)
          close(25)
          spec_out=-1   ! indicate this was first spectrum
      endif
c
c   calculate real spectrum, limit to frequencies ffmin and ffmax
c
      j = 0

      if(filt.gt.0) then
         fflow=flow(filt)
         ffhigh=fhigh(filt)
      else
         fflow=ffmin
         ffhigh=ffmax
      endif 
      do i = 2,((ipow/2) + 1)
       ff = (i-1)*rate/ipow

       if(ff.ge.fflow.and.ff.le.ffhigh) then

cold       if((filt.ne.0.and.ff .ge. flow(filt) .and. ff .le. fhigh(filt)).
cold     * or.(filt.eq.0.and.ff .ge. ffmin      .and. ff .le. ffmax)) then

         j = j + 1
         if(power) then
c            y(j) = ((com(i)*conjg(com(i)))/nsamp)    ! stationary signal
c changed March 28, 2001, lo, psd as defined in the numerical recip.
c
            y(j) =2*com(i)*conjg(com(i))/(rate*nsamp)    ! stationary signal
         else
            y(j) = ((1/rate)**2)*(com(i)*conjg(com(i)))    ! transient signal
            y(j) = sqrt(y(j))              ! j.h. change
         endif

c         y(j) = log10( y(j) )                        ! take the logarithm
c
c Set the axes type (bjb 22/02/2001) 
c
         if(iaxes_type.eq.1) then
           x_work(j) = log10(ff)
           ytext='    Amplitude                '
           x0=-110.0                  ! indicate antilog on x-axis notation
         endif
         if(iaxes_type.eq.2) then
           y(j) = log10( y(j) )       ! take the logarithm
           x_work(j) = log10(ff)      
           ytext='Log amplitude                '
           x0=-110.0                  ! indicate antilog on x-axis notation
         endif
         if(iaxes_type.eq.3) then
           x_work(j) = ff
           ytext='    Amplitude                '
           x0=110.0                   
         endif
         if(iaxes_type.eq.4) then
           y(j) = log10( y(j) )       ! take the logarithm
           x_work(j) = ff             
           ytext='Log amplitude                '
           x0=110.0                   
         endif

         if(ch.eq.'N') y(j)=(y(j)-18.0)*10.0  ! db (m/s**2)**2

c         if(linx) then
c            x_work(j) = (ff)
c         else
c            x_work(j) = log10(ff)
c         endif
c
  500     continue
       endif
      enddo
c
c   text for kind of spectrum
c
      if(disp_vel.eq.0) write(text,'(a)')'Uncorrected  '
      if(disp_vel.eq.3) write(text,'(a)')'Acceleration '
      if(disp_vel.eq.2) write(text,'(a)')'Velocity     '
      if(disp_vel.eq.1) write(text,'(a)')'Displacement '
      if(power.and.ch.ne.'N') text=text(1:12)//', power'
      if(ch.eq.'N') text=text(1:12)//', power, db(m/s**2)**2/Hz'

c
c  possibly clculate slope and t*
c
      if(linx.and.noise_spectrum.lt.2) then
         call lsqlin(j,x_work,y,aa,bb,corr,rms)
         tstar=bb*0.879    ! 0.879 = 1/(pi * log10(e))
         message(1)(1:12)='Lin fit: t*='
         write(message(1)(13:20),'(f8.3)') tstar
         message(1)(21:22)=' c '
         write(message(1)(23:30),'(f8.3)') corr
         call xmessage(message,1,30,620.0,20.0)
      endif
c
c   possibly write out amplitude spectrum
c

      if(noise_spectrum.lt.2.and.abs(spec_out).eq.1) then
          open(25,file='amp_spec.out',status='unknown')
          write(25,'(a)') text
          if(linx) then
             do i=1,j
               write(25,*) x_work(i),y(i)
             enddo
          else
             do i=1,j
               write(25,*) 10.0**x_work(i),y(i)
             enddo
          endif
          spec_out=-1           ! indicate a spectrum written out
          close(25)
      endif
c
c   check that filter limits for spectral output give at least 3 points
c
      if(j.lt.3) then
         call clear_to_alpha
         write(6,*)' Too few points in spectrum or limits wrong'
         write(6,*)' see mulplt.def'
         write(6,*)' Enter to stop'
         read(5,'(a)') i
         stop
      endif
c
c   find some kind of average to use with spectral fitting when
c   the signal spectrum is in y
c
      if(noise_spectrum.lt.2) then
c        amp_average=(y(j/2) +y(j/2-5)+y(j/2+10))/3.0
          do i=3,j/4
             amp_average=amp_average+y(i)
          enddo
          amp_average=amp_average/(j/4-3+1)
      endif
      nspec=j
c
c   clear top part of screen and display new choises
c
      call xset_color(color_back)
      call fillbox(0.0,690.0,1024.0,780.0)
      
c
c   plot spectrum
c
      xtext='          Frequency (Hz)     '
 
      nnspec(1)=nspec
      if(linx) then
         x0=110.0
      else

      endif
      xlength=500.0    ! length of x-axis
      if(noise_spectrum.eq.0.or.noise_spectrum.eq.2) then
         xy_cursor=4   ! stop with cursor, signal or noise after sign spectrum
         if(noise_spectrum.eq.2) xlength=0.0  ! same scale as signal spectrum
      else
         xy_cursor=0   ! do not call up cursor
      endif
c
c   display new choises
c
      if(auto_spec) then

      message(1)=
     *'Valid input is now:'
	  message(2)=
     *'                     r: Replot   f: Foreward or next trace   ' 

      message(3)=
     *'                     q: Quit     R: Delete auto spec. parameters'
      else
      message(1)=
     *'Valid input is now:  Mouse click to select 3 points in spectrum'
	  message(2)=
     *'                     r: Replot     f: Foreward or next trace   ' 

      message(3)=
     *'                     q: Quit       s: Make spectral modeling   '
      endif

      call xmessage(message,3,64,10.0,700.0)

c
c   add noise limits if power noise spectrum, only log-log
c
      if(ch.eq.'N') then
        k=0
        do i=1,22
          ff = 10.0**low_f(i)
          if(ff .ge. ffmin .and. ff .le. ffmax) then
             j=j+1
             k=k+1
                x_work(j)=low_f(i)
                y(j)=low_l(i)
           endif
        enddo
        nnspec(2)=k
        k=0
        do i=1,14
          ff = 10.0**high_f(i)
          if(ff .ge. ffmin .and. ff .le. ffmax) then
             j=j+1
             k=k+1
               x_work(j)=high_f(i)
               y(j)=high_l(i)
          endif
        enddo
        nnspec(3)=k
        xy_cursor=4   ! stop after plot
       endif
c
c   do not stop after plot if auto_spec
c
        if(auto_spec) xy_cursor=0

       k=1
       if(ch.eq.'N') k=3

      call xy_plot
     *(k,nnspec,x_work,y,text,xtext,ytext,xlength,270.0,
     *x0,70.0,2,1,30.0
     *,xy_cursor,c_spec,nc,cx,cy)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   if noise spectrum (not power noise), go back to the beginning, 
c   but first reset
c   noise spectrum indicator
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if(noise_spectrum.eq.1.and.ch.ne.'N') then
         noise_spectrum=2     ! indicate next spectrum to be a noise spectrum
         goto 1
      else
         noise_spectrum=0     ! reset
      endif

      if(.not.auto_spec)then ! do not stop if auto_spec
         if(c_spec(nc).eq.'f'.or.c_spec(nc).eq.'F') choice='FINI'
         if(c_spec(nc).eq.'r'.or.c_spec(nc).eq.'R') choice='REPL'
         if(c_spec(nc).eq.'q'.or.c_spec(nc).eq.'Q') choice='QUIT'
         if(c_spec(nc).eq.'s') choice='FIT '
      endif
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  automatic spectral fit
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      if(auto_spec) then
c
            
c           write(6,*)wav_current_chan(1),
c     *     wav_rate(wav_current_chan(1)),phase_time,0.0,
c     *      (last-first)/wav_rate(wav_current_chan(1)),otime
c            write(6,'(7f7.0)')(signal1(i),i=1,20)
c            write(6,*) omega0,cornerf,ka,omi,f0i
c            write(6,*) 'filt',ffmin,ffmax

            res_m_a(2)=0.0

            call get_om_f0(
     *      signal1,                                     ! data vector
     *      wav_current_chan(1),                         ! index of current waveform channel
     *      ffmin,ffmax,                                 ! lower and higher frequency limits from MULPLT.DEF
     &      phase_time,                                  ! abs phase time of phase used or start of window   
     *      0.0        ,                                 ! start before phase_time
     &      (last-first)/wav_rate(wav_current_chan(1)),  ! duration
     *      omega0,cornerf,ka,                           ! output omega0, corner f and delta kappa
     *      omi,f0i,                                     ! input of --------------
     &      otime,                                       ! abs origin time
     *      q0,qalpha,                                   ! q-relation
     *      q_below_1hz,                                 ! alternative q parameter
     *      kappa,                                       ! kappa
     &      dkappa,                                      ! used in grid search, must be a variable since return value
     *      50,                                          ! size of population in genetic algorithm
     *      250,                                         ! number of generations to be done
     &      100,100,5,                                   ! grid parameters
     &      res_m_a,                                     ! some residual
     *      2.0,                                         ! 1=GA, 2=GRID
     *      1.0,                                         ! used to calculate norm fit 
     *      0.0,0.0,                                     ! fixed f band , since 0, use s/n
     *      fsn1,fsn2)                                   ! output good sn frequency range

c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  plot automatically fitted spectrum
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c

            moment=0.0
            sdrop=0.0
c
c   only calculate if autofit was ok
c
c
c            write(6,*) cornerf,omega0
            if(fsn1.gt.0.0.and.fsn2.gt.0.0.and.cornerf.gt.0.01) then
              write(6,*)'Spectral residual ',res_m_a(2)
              radius=0.0
              mw=0.0
              call calcmoment
            else
              omega0=0.0
              radius=0.0
              mw=0.0
            endif
c
c  output on screen
c
            message(1)= 'Amplitude spectral parameters'
            write(message(2),
     *      '(a,f6.2,a,f6.1,a,f6.3)') 'Mo  ',moment,
     *      ' ST ',sdrop,' OM ',omega0
            write(message(3),'(a,f7.3,a,f5.1,a,f5.1)')
     *      'f0 ',cornerf,' R   ',radius,' MW  ',mw
            message(4)= ' '
            message(5)=' '
            if(fsn1.eq.0.0.and.fsn2.eq.0.0.and.cornerf.le.0.01) then
               message(6)='f-range too small, bad S/N or low f0'
            else
               message(6)=' '
            endif

            write(message(7),'(a,2f6.2)')'Auto f-band used',fsn1,fsn2
            write(message(8),'(a,f7.3)')'Spectral residual of fit ',
     *      res_m_a(2)
            call xmessage(message,8,40,620.0,200.0)
c
c   must call up curor here since next section might be skipped
c
            if(fsn1.eq.0.0.and.fsn2.eq.0.0.and.cornerf.eq.0.0) then
               call xscursr(i,xx,yy)		
               if(char(i).eq.'r'.or.char(i).eq.'R') choice='REPL'
               if(char(i).eq.'s') choice='FIT '
               if(char(i).eq.'q'.or.char(i).eq.'R') choice='QUIT'
               if(char(i).eq.'f') choice='FINI '
            endif
c
c   calculate theroretical spectrum if parameters calcualted
c
 
        if(cornerf.gt.0.0) then   

         f0=cornerf
         j=0
         do i = 2,((ipow/2) + 1)
           ff = (i-1)*rate/ipow
           if(ff .ge. ffmin
     *     .and. ff .le. ffmax) then
              j = j + 1
c
c   source spectrum only. there is no q-corrections since spectrum already
c   has been corrected for q 
c
              y(j) = (10**omega0/(1+ff*ff/(f0*f0)))  ! source
              y(j) = log10( y(j) )     ! take the logarithm
              x_work(j) = log10(ff)     
            endif
         enddo

         xlength=0.0     ! use same scale as before
c
c   check type of axis
c

        do j=1,nspec
           if(iaxes_type.eq.3.or.iaxes_type.eq.4)
     *     then
             x_work(j)=10.0**x_work(j)
           endif
           if(iaxes_type.eq.3.or.iaxes_type.eq.1) then
              y(j)=10.0**y(j)
           endif 
        enddo


         xy_cursor=1    
         call xy_plot
     *   (1,nnspec,x_work,y,text,xtext,ytext,xlength,270.0,
     *   x0,70.0,2,1,30.0
     *   ,xy_cursor,c_spec,nc,cx,cy)

         if(c_spec(nc).eq.'f'.or.c_spec(nc).eq.'F') choice='FINI'
         if(c_spec(nc).eq.'r') choice='REPL'
         if(c_spec(nc).eq.'q'.or.c_spec(nc).eq.'Q') choice='QUIT'
         if(c_spec(nc).eq.'s'.or.c_spec(nc).eq.'S') choice='FIT '
c
c   delete values
c
         if(c_spec(nc).eq.'R') then
            omega0=0.0
            cornerf=0.0
            choice='REPL'        
         endif
c
c  convert readings  back to nordic format, must be done for each spectrum
c  since only the last sepctrum is in memory so if two form same trace, only last 
c  saved

              call convert(wav_abs_time(wav_first),
     *        wav_stat(wav_current_chan(1)),                     
     +        wav_comp(wav_current_chan(1)),                               
     +        1)              
c
c   update phase list
c
              call convert(wav_abs_time(wav_first),
     *        wav_stat(wav_current_chan(1)),                     
     +        wav_comp(wav_current_chan(1)),                               
     +        0)              
        endif          
      endif               ! end of auto spec*******************************

c-----------------------------------------------------------------------------
c   calculate and write spectral values if at least 3 points sampled,
c   last point is exit, use 3 values before that. only if not auto spec
c--------------------------------------------------------------------------------   
c


      if(nc.gt.3.and.(.not.auto_spec)) then
         omega0=0.0
         cornerf=0.0
         if((ch.eq.'D'.or.ch.eq.'d').and..not.power) then   ! only calculate parameters for
            omega0=(cy(nc-3)+cy(nc-2))/2.0    ! displacement spectrum
            cornerf=10.0**cx(nc-2)
            if((cx(nc-2)-cx(nc-1)).ne.0.0)
     *      sslope=-(cy(nc-2)-cy(nc-1))/(cx(nc-2)-cx(nc-1))

            moment=0.0
            sdrop=0.0

            call calcmoment
c
c  output on screen
c
            message(1)= 'Amplitude spectral parameters'
            write(message(2),
     *      '(a,f6.2,a,f6.1,a,f5.1)') 'Mo  ',moment,
     *      ' ST ',sdrop,' OM ',omega0
            write(message(3),'(a,f7.3,a,f5.1,a,f5.3)')
     *      'f0 ',cornerf,' R   ',radius,' MW  ',mw
            message(4)= ' '
            message(5)='Push any key to continue, R to replot '
            call xmessage(message,5,40,620.0,275.0)
c
c  convert readings  back to nordic format, must be done for each spectrum
c  since only the last sepctrum is in memory so if two form same trace, only last 
c  saved

              call convert(wav_abs_time(wav_first),
     *        wav_stat(wav_current_chan(1)),                     
     +        wav_comp(wav_current_chan(1)),                               
     +        1)              

c
c   update phase list
c
              call convert(wav_abs_time(wav_first),
     *        wav_stat(wav_current_chan(1)),                     
     +        wav_comp(wav_current_chan(1)),                               
     +        0)              
         else
            moment=0.0
            sdrop=0.0
            message(1)='No spectral parameters calculated'
            message(2)='Not a displacement spectrum'
            message(3)='Push any key to continue, R to replot '
            call xmessage(message,3,40,620.0,275.0)
         endif
c
c   if choice is forward, stop so display remain
c
         call xscursr(i,xx,yy)	
		
         if(char(i).eq.'r'.or.char(i).eq.'R') choice='REPL'
         if(char(i).eq.'s') choice='FIT '

      endif

c
c------------------------------------------------
c   calculate spectral fitting parameters
c------------------------------------------------
c
      if(choice.eq.'FIT') then
c
c   first check if a moment already calculated recently for this station
c   then use that
c
         if(moment.gt.0.0) average_moment=moment      
c
c   get moment if available and if not already entered manually in which case
c   average moment different from 0
c
         if(average_moment.eq.0.0) then
            do i=2,nhead
              if(data(i)(2:13).eq.'SPEC AVERAGE') then
                 read(data(i)(18:22),'(f5.2)',err=3846) average_moment
                 goto 3847
              endif
 3846         continue
              average_moment=0.0    ! is log moment
            enddo
 3847       continue              ! moment found
         endif

c
c   if no moment, do not model
c
         if(average_moment.eq.0.0) then
            message(1)='No moment, cannot model'
            message(2)='Push any key to continue, R to replot '
            call xmessage(message,2,40,620.0,250.0)
            call xscursr(i,xx,yy)		
            if(char(i).eq.'r'.or.char(i).eq.'R') choice='REPL'
            return
         endif

c
c   if no distance, do not model
c
         if(sdistance.eq.0.0) then
            message(1)='No distance, cannot model'
            message(2)='Push any key to continue, R to replot '
            call xmessage(message,2,40,620.0,250.0)
            call xscursr(i,xx,yy)		
            if(char(i).eq.'r'.or.char(i).eq.'R') choice='REPL'
            return
         endif
            

 7464    continue
c
c   read parameters
c

         write(text1,'(a3,f5.1,a17)')'f0=',f0,' enter new/return'
	   text = '                    '
         call oneline(text1,25,text,8,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            f0=array$(1)
         endif
         write(text1,'(a3,f5.3,a17)')'k =',kk,' enter new/return'
	   text = '                    '
         call oneline(text1,25,text,8,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            kk=array$(1)
         endif
         write(text1,'(a3,i5,a17)')'Q0=',int(qq),' enter new/return'
         text = '                    '
	   call oneline(text1,25,text,8,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            qq=array$(1)
         endif
         write(text1,'(a3,f5.3,a17)')'qa=',qa,' enter new/return'
         text = '                    '
	   call oneline(text1,25,text,10,630.0,250.0)
         if(text(1:4).ne.'    ') then
            call sei get values (1,text,code)
            qa=array$(1)
         endif
c
c   check q
c
         if(qq.eq.0.0) qq=99999.0   ! a large q is like no q
c
c  calculate f0 or stress drop dependent on what was entered
c
c
c   if corner frequency not used, enter stress drop
c
         if(f0.eq.0.0) then
	      text = '                    '
            call oneline('Stress drop',11,text,20,630.0,250.0)
            ssdrop=sei real num(text,i)  ! i is error code
c
c   calculate f0 from stress drop
c
            if(average_moment.gt.0.0.and.svelocity.gt.0.0) then 
              f0=ssdrop*1e14*svelocity**3/(8.5*10**average_moment)
              f0=f0**0.333333
            else
              f0=1.0
            endif
         else
c
c  calculate stress drop from f0
c
            if(average_moment.gt.0.0.and.svelocity.gt.0.0) then
              ssdrop=(1e-14)*8.5*10**average_moment*f0**3/svelocity**3
            else
              ssdrop=-1.0
            endif
         endif
c
c   calculate theoretical spectrum
c
         j=0
         do i = 2,((ipow/2) + 1)
           ff = (i-1)*rate/ipow
cccc there is probabaly not addtional filters anymore
c           if((filt.ne.0.and.ff .ge. flow(filt) .
c     *     and. ff .le. fhigh(filt)).
c     *     or.(filt.eq.0.and.
            if(ff .ge. ffmin
     *     .and. ff .le. ffmax) then
              j = j + 1
              y(j) = ((2*3.14*ff)**(disp_vel-1)/(1+ff*ff/(f0*f0)))*  ! source
cjh     *               exp(-3.14*sdistance*ff/(spec_velocity*qq*(ff**qa))) ! Q
     *               exp(-3.14*travel_time*ff/(qq*(ff**qa))) ! Q  from apr 2013
     *               *exp(-3.14*kk*ff)      ! kappa correction
              y(j) = log10( y(j) )      ! take the logarithm
c              if(linx) then
c                x_work(j)=ff
c              else
                x_work(j) = log10(ff)     ! this after use of com, equivalent
c              endif
            endif
         enddo

c
c   normalize values, use middle frequency value for normalizing, 
c
c
c   calculate spectral level from moment

         norm_factor=average_moment     ! moment, here it is logaritmic
         norm_factor=norm_factor+log10(1.2)   ! rad. pat, free surface
         norm_factor=norm_factor+log10(1.0/   ! the rest, 1000 to convert metr.
     *               (4*3.14*(density*1000)*((1000*spec_velocity)**3)))
c
c   geometrical spreading
c
         call spec_dist
     *   (spec_phase,edistance,edepth,geo_factor)
         norm_factor=norm_factor-log10(1000.0/geo_factor)
         geo_distance=1.0/geo_factor
         norm_factor=norm_factor+9             ! convert to nanometers
c
c   add factor
c
         do i=1,nspec
            y(i)=y(i)+norm_factor
         enddo
        
c
c   factor to normalize plot
c
c          norm_factor=0.0
c          do i=3,j/4
c             norm_factor=norm_factor+y(i)
c          enddo
c          norm_factor=norm_factor/(j/4-3+1)
c         norm_factor=amp_average - norm_factor
c
c        norm_factor=amp_average - 
c    *   (y(nspec/2)+y(nspec/2-5)+y(nspec/2+10))/3.0
c
c   output
c
c
         message(1)= 'Spectral modelling parameters'
c         write(message(2),'(a,f6.2)') 'Obs - calc level', norm_factor
         write(message(2),'(a,f6.2,a,i5)')      'Moment ',
     *   average_moment, ' Geo_dist ',int(geo_distance)
         write(message(3),'(a,f9.2,a,f5.1)') 
     *   'Stress drop', ssdrop,' f0 ',f0
         write(message(4),'(a,f5.3,a,i5,a,f5.2)')
     *   'k   ',kk,' q   ',int(qq),' qa ',qa 
         call xmessage(message,4,30,620.0,180.0)

c         do i=1,nspec
c           y(i)=y(i)+norm_factor
c         enddo

         xlength=0.0     ! use same scale as before
c
c   check type of axis
c

        do j=1,nspec
           if(iaxes_type.eq.3.or.iaxes_type.eq.4)
     *     then
             x_work(j)=10.0**x_work(j)
           endif
           if(iaxes_type.eq.3.or.iaxes_type.eq.1) then
              y(j)=10.0**y(j)
           endif
        enddo
c
c   put in a file
c
        open(25,file='spectral_fit.out',status='unknown')
c        write(6,*) 'spectral fit f, amp'
        do i=1,nspec
          write(25,*) x_work(i),y(i)
        enddo
        close(25)


         call xy_plot
     *   (1,nnspec,x_work,y,text,xtext,ytext,xlength,270.0,
     *   x0,70.0,2,1,30.0
     *   ,xy_cursor,c_spec,nc,cx,cy)
         if(c_spec(nc).eq.'f'.or.c_spec(nc).eq.'F') choice='FINI'
         if(c_spec(nc).eq.'r'.or.c_spec(nc).eq.'R') choice='REPL'
         if(c_spec(nc).eq.'q'.or.c_spec(nc).eq.'Q') choice='QUIT'
         if(c_spec(nc).eq.'s'.or.c_spec(nc).eq.'S') choice='FIT '
c
c   try again
c
         if(choice.eq.'FIT ') goto 7464
 7465    continue
      endif

      return                                                                    
      end                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc            
      subroutine calcmoment
c
c     Subroutine to calculate log10(seismic moment)
c     and stressdrop.
c     Author: C. Lindholm, July, 1993
c
c     Returned parameters:
c       log10(seismic moment)
c       stress drop
c
c       output is newton-meter for moment and km for radius
c       input is nanometer-sec for spectral level and else in 
c       km/sec, g/cm*cm and km

      implicit none
      include 'mulplt.inc'      
      include 'seisan.inc'
      real pi
      real geo_factor  ! geometrical spreading factor
      real surface,radpat  !  surface effect and radiation pattern effect 
      real factor
c
      pi=3.14159265
      surface=2.0
      radpat=0.6
c      radius=0.35*svelocity/cornerf ! 
      radius=0.37*spec_velocity/cornerf ! changed nov 07 jh
c
      if(sdistance.eq.0) then
         moment=0.0
         sdrop=0.0
         mw=0.0
         return
      endif
c
c   geometrical spreading
c
      call spec_dist
     *(spec_phase,edistance,edepth,geo_factor)
      geo_distance=1.0/geo_factor
      factor= 4*pi*(density*1000.0)*((spec_velocity*1000.0)**3)
      moment = factor*(1000.0/geo_factor)*
     *(10.0**omega0)/(radpat*surface*1.0e9)
      sdrop = (0.44*moment)/(1.0e14*radius**3)
      moment = log10(moment)
      if(moment.gt.0.0) mw=moment*0.667-6.06

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_3_chan
c                
c  does 3 component analysis, azimut calculation and component
c  rotation
c                                                               
      include 'mulplt.inc'
      include 'seiplot.inc'                                                             
c                                                                               
c-- No. of channels in file                     
      integer nchan		
c-- number of samples
      integer nsamp
c-- counters,etc                                         
      integer i,j,k,nsamp1			
c-- general text string
      character*80 text
c-- Station and component info    
      character*5 station(max_trace),stat_c
      character*4 comp(max_trace),comp_c
c-- time of main header and channel header
      integer myear,mmonth,mday,mhour,mmin,cyear,
     *cmonth,cday,chour,cmin,doy
      real msec,csec
c-- index for channels this station
      integer chan_z,chan_e, chan_n
c-- 3 component parmeters, coherence and rms
       real COHER,RMSAMP
c-- azimuth for rotating the horizontal seismograms
      real backazi,caz,saz
c-- channel delay
      real delay
      double precision mtime,ctime
c-- stat time and window relative main header
      real start,window
      integer first1,last1   ! local counters
c-- save xscale
      real x_old_scale
c-- 1: error, 0: no error                                  
      integer error
      integer code    ! error code
c-- save rotate flag
      logical save_rotate
      integer first_org			
      integer hl,c_chan
      integer current_chan_3com  ! channel number on entry of routine
c
      backazi=10000    ! always ask for backazimuth, this parameter gives
c
c   save current channel from which call was made
c
      current_chan_3com=wav_current_chan(1)
c
c   get and check rotation paramters
c
      if(rotate) then
        if(backazi.gt.999) then    ! no azimuth given, enter manually
            text = '                    '
            call oneline(' Give back-azimuth ',19,text,10,500.0,35.0)
            backazi=sei real num(text,code)
        endif
      endif
c
c   save old xscale
c
      x_old_scale=xscale
      error=0
      j = 1                                                                     
c
c   read 3 channels
c
      current_chan_3com=wav_current_chan(1)    ! save active channel
      first_org=first
      rate=wav_rate(wav_current_chan(1))
c
      call wav_read_3channel(current_chan_3com)
c
c   check if both horizontal channels there
c                  
      if(wav_current_chan(2).eq.0.or.wav_current_chan(3).eq.0) then
         error=1                                                               
         write(text,'(a)')' Not both horizontal channels, replot'
         call xchars(text,37,100.0,350.0)
         goto 500
      endif
c
c   calculate start time for all 3 channels, check if data is there
c
cfix
      wav_out_nchan=3
      do i= 1,3
        k=wav_current_chan(i)
        wav_out_chan(i)=k  ! index of 3 channels
        wav_out_start(k)=wav_delay(current_chan_3com)+(first-1)/rate !same start
        wav_out_duration(k)=(last-first+1)/rate
      enddo
c
      call wav_get_interval          ! find where and if data available
c
c   write out if a problem
c
      if(wav_out_status(wav_current_chan(1)).ne.4.or.wav_out_status
     *(wav_current_chan(2)).ne.4.or.wav_out_status
     *(wav_current_chan(3)).ne.4) then
         error=1                         
         write(text,'(a)')' Data not available same interval, replot'
         call xchars(text,36,100.0,350.0)
         goto 500
      endif
c
c   select, filter, remove response, save in work array
c
      nsamp=wav_out_duration(wav_current_chan(1))*rate ! assume same for3 chan.
      do i=1,3
         numb_samp=wav_nsamp(wav_current_chan(i))
         call trans_filt(i)
         first1=wav_out_first_sample(wav_current_chan(i))
         last1=first1+nsamp-1   ! cfix
c
c   put window in beginning of array
c		 
         do k=first1,last1
            if(i.eq.1)
     *      wav_y1(k-first1+1)=wav_y1(k)
            if(i.eq.2)
     *      wav_y2(k-first1+1)=wav_y2(k)
            if(i.eq.3)
     *      wav_y3(k-first1+1)=wav_y3(k)
         enddo
      enddo
      numb_samp=nsamp
c                                                                               
c   calculate start second for lower plot, will be the same for all 3 channels 
c 
cfix                                                                           
         fsec=wav_sec(wav_current_chan(1))+(first-1)/rate                      
         fmin=wav_min(wav_current_chan(1))+(first-1)/(rate*60.0)
                       
c                                                                               
c  set plotting parameters etc
c
      ypos=270                                                                  
      height=90        
      pframe=3     ! plot only top and sides
      paxis=0      ! no axis tics
      paxisnumb=0  ! no axis numbers                                           
      pmax=0       ! plot no max below trace
      ppick=0      ! no picking mode
      page_time=time2-time1                                                     
c                                                                               
c    plot z channel                                                         
c                 
         first=1
         last=nsamp
         do i=1,nsamp
            y(i)=wav_y1(i)
         enddo                                                              
         call plotw	                                                        
         i=max+0.5                                                        
         write(text,'(i9)') i                                             
         call xchars(text,9,900.0,ypos+height-22)	
         text(1:1)='Z'
         call xchars(text,1,20.0,ypos+height-22)	
         if(rotate) then
            i=backazi+0.5
            write(text,'(a,i4)')'baz=',i
            call xchars(text,8,50.0,ypos+height-22)
         endif
c
c   plot picks
c
         page_start=0.0
         k=wav_current_chan(1)
c
c   pics are referenced in time to channel where zoom was made,
c   therefore use trace start time from current_chan_3com
c
         wav_rot_comp(k)=' '
c
c   save picks from original channel displayed on top
c
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(current_chan_3com),wav_comp(current_chan_3com),1)
c
c   load picks from z-channel
c
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(k),wav_comp(k),-1)
         first=first_org   ! original zoom start

         call tecpic       ! plot picks

         first=1           ! now data put into array starting with zoom start
         
c
c  set rotation parameters
c
      caz = -cos(backazi*3.14159/180.)
      saz = -sin(backazi*3.14159/180.)
c
c  plot nw channel
c
      pframe=2    ! plot side of frame only
      ypos=180
c
      do i=1,nsamp
        if(rotate) then
           y(i) = caz*wav_y2(i) + saz*wav_y3(i)  ! make radial component
        else
           y(i)=wav_y2(i)
        endif
      enddo
c
c   load picks from n-channel
c
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(wav_current_chan(2)),
     *   wav_comp(wav_current_chan(2)),-1)
         first=first_org   ! original zoom start
         call tecpic
         first=1
c                                                                               
c   plot                                                              
c                                                                               
      call plotw	                                                        
      i=max+0.5                                                        
      write(text,'(i9)') i                                             
      call xchars(text,9,900.0,ypos+height-22)	
      text(1:1)='N'
      if(rotate) text(1:1)='R'
      call xchars(text,1,20.0,ypos+height-22)	
c
c  plot ew channel, transverse channel if rotate
c
      ypos=90                                                                  
      paxis=1      ! plot axis again
      paxisnumb=1                                    
c
c   put signal in plotting array
c
      do i=1,nsamp
        if(rotate) then
           y(i) = -saz*wav_y2(i) + caz*wav_y3(i)  ! transvers component
        else
           y(i)=wav_y3(i)
        endif
      enddo
c                                                                               
c   plot ew                                         
c                                                                               
c
c   load picks from ew-channel
c
      call convert(wav_abs_time(current_chan_3com),
     *wav_stat(wav_current_chan(3)),
     *wav_comp(wav_current_chan(3)),-1)
      first=first_org   ! original zoom start
      call tecpic
      first=1
      call plotw	                
      i=max+0.5                                                        
      write(text,'(i9)') i                                             
      call xchars(text,9,900.0,ypos+height-22)	
      text(1:1)='E'
      if(rotate) text(1:1)='T'
      call xchars(text,1,20.0,ypos+height-22)	
c
c   do 3 component analysis
c
      do i=1,nsamp
         wav_y3comp(i,1)=wav_y1(i)
         wav_y3comp(i,2)=wav_y2(i)
         wav_y3comp(i,3)=wav_y3(i)
      enddo
      call PRECOH1(wav_y3comp,max_sample,1,nsamp,three_comp_velocity,
     *COHER,AZIM,VELI,RMSAMP)
      i=azim+0.5
      write(text,333) i,veli,coher
 333  format('Az ',i4,1x,'Vel ',f7.1,1x,'Co ',f6.1)
      call xchars(text,31,1.0,3.0)
c
 500  continue

c
c   load in original channel, disable rotate
c
      rotate=.false.
      call wav_read_channel(current_chan_3com)
c
c   put in original picks unlsess an errror occured (jh, jan 04)
c
      if(error.ne.1) then
         call convert(wav_abs_time(current_chan_3com),
     *   wav_stat(current_chan_3com),wav_comp(current_chan_3com),0)
      endif

      first=1
      numb_samp=wav_nsamp(wav_current_chan(1))
c
c   rest plot parameters
c
      pframe=1                                                                  
      paxis=1                                                                   
      paxisnumb=1                                                               
      phelp=1                                                                   
      pmax=1                                                                    
      ppick=1                                                                   
      ptitle=1                                                                  
      filt=0
      ypos=450.0
      height=200.0
      xscale=x_old_scale
c     write(39,*) 'first,last',first,last
c     write(39,*)'p st,last_ix',page_start,last_ix
c     write(39,*)'pictim',(pictim(i),i=1,5)
c     write(39,*)'xscale,rate,xpos',xscale,rate,xpos
c
      return                                                                    
      end                                                                       




      subroutine plot_resp                                                 
c                                                                               
c     routine to plot response function                
c                                                                                
c-- common block     
c                                
      include 'mulplt.inc'	
      include 'seiplot.inc'
c
      include 'seisan.inc'		
c
c-- indicator for response removal or not (1 or 0).                        
      integer rem_resp			
      real ff ! frequency
c-- number of points in spectrum
      integer nspec
c-- if several data sets, number of points in each
      integer nnspec(2)
c-- counters and help variables                                               
      integer i,j				
c-- response value
      complex respons
      character*80 text
      character*80 message(10)
      character*30 xtext,ytext      ! axis text
c--returns from spectral plot
      character*1 c_spec(20)
      real x0                       ! left corner of spectrum
      real cx(20),cy(20)
      real ix,iy                    ! mouse position
      integer nc
      real x_work(max_sample)
c-- counter bounds for spectrum                    
      equivalence(x_work,com)                                                     
      
c
c   first check if response is available
c
      rem_resp=0
         wav_resp_file = ' '
         call read_resp
         if(wav_resp_status.eq.'9') then
            write(text(1:20),'(a)')'No response info ***'
            call tchars(text,20,600.0,225.0)
            write(text(1:24),'(a)')'Push any key to continue'
            call tchars(text,23,600.0,190.0)
c
c-- call up cursor                               
c
            call xscursr(i,ix,iy)			
            choice='REPL'
            return
         endif
         if(wav_resp_status(1:1).eq.'8') then
            write(text(1:33),'(a)')'Response from waweform header ***'
            call tchars(text,33,600.0,320.0)
         endif
c
c  the amplitude response
c
c
       call calc_resp(10.0,respons)
c       write(6,*) cabs(respons)
      j=0
      do i = -260,190,1    ! log frequencies
         j=j+1
         ff=10**(i/100.0)
         call calc_resp(ff,respons)
         y(j)=log10(cabs(respons))
         x_work(j) = log10(ff)          
 1243    continue
      enddo
      nspec=j
c
c   clear top part of screen and display new choises
c
      call xset_color(color_back)
      call fillbox(0.0,700.0,1024.0,780.0)
      message(1)='Valid input is now:  R: Replot          '
      message(2)='                     Q: Quit            '
      message(3)='                     F: Foreward or next'
      call xmessage(message,3,40,10.0,700.0)
      
c
c   plot spectrum
c
      xtext='      Log frequency (Hz)     '
      ytext='Log amplitude                '
      write(text,'(a)')'Amplitude Response, count/nm '
      text(30:80)=' '
      if(wav_resp_status.eq.' ') then
         write(text(34:42),'(i4,1x,2i2)') wav_resp_year,wav_resp_month,
     *                                 wav_resp_day
      endif
      if(wav_resp_status.eq.'8') then
         text(34:41)='W header'
      endif
      text(44:49)=wav_resp_type(1:6)
      nnspec(1)=nspec
      x0=100.0                
      call xy_plot
     *(1,nnspec,x_work,y,text,xtext,ytext,380.0,270.0,
     *x0,70.0,2,1,30.0
     *,0,c_spec,nc,cx,cy)
c
c  the phase response
c
      j=0
      do i = -260,190,1    ! log frequencies
         j=j+1
         ff=10**(i/100.0)
         call calc_resp(ff,respons)
         y(j)=180.0*atan2(imag(respons),real(respons))/3.141592654
         x_work(j) = log10(ff)          
      enddo
      nspec=j
c
c   unwrap phases
c
c      call unwrap_phase(y,nspec,y)
c
c   plot spectrum
c
      ytext='Phase                '
      write(text,'(a)')'Phase response '
      nnspec(1)=nspec
      x0=620.0                
      call xy_plot
     *(1,nnspec,x_work,y,text,xtext,ytext,380.0,270.0,
     *x0,70.0,2,1,30.0
     *,1,c_spec,nc,cx,cy)
      if(c_spec(nc).eq.'f'.or.c_spec(nc).eq.'F') choice='FINI'
      if(c_spec(nc).eq.'r'.or.c_spec(nc).eq.'R') choice='REPL'
      if(c_spec(nc).eq.'q'.or.c_spec(nc).eq.'Q') choice='QUIT'
      return
      end                                                                       





      subroutine plot_pm(start_time,stop_time)
c
c     routine to plot particle motion
c
c-- common block
c
      implicit none
      include 'mulplt.inc'
      include 'seiplot.inc'
c      include 'waveform.inc'
 
      integer i,c,ind,j
      character*80 text
      character*80 message(10)
      character*30 xtext,ytext      ! axis text
c--returns from spectral plot
      character*1 c_spec(20)
      real cx(20),cy(20)
      real ix,iy                    ! mouse position
      integer nc
      character*1 cha(20)
      integer cind(3)               ! channel index Z,N,E or Z,R,T
      real dx(max_sample),dy(max_sample),dz(max_sample)
      real dr(max_sample),dt(max_sample)
      integer np
      real xlength
      real x0,y0
      integer draw,xyc
      real start_time,stop_time,time
      real dc

c settings
      text=' '
      xlength=200
      y0=70
      xyc=0

      do i=1,3
        cind(i)=0
      enddo
      c=0  ! counter for selected channels
      do i=1,wav_nchan
        if (channelname(i).gt.0) then
          c=c+1
          ind=channelname(i)
          if (wav_comp(ind)(4:4).eq.'Z') cind(1)=ind
          if (wav_comp(ind)(4:4).eq.'N') cind(2)=ind
          if (wav_comp(ind)(4:4).eq.'E') cind(3)=ind
        endif
      enddo
c      write(*,*) ' nchan = ',c
c
c check
c
      if (c.ne.3) then
        write(*,*) ' Three channels required for particle motion plot ',
     &    wav_nchan
        return
      endif
      do i=2,c
c        write(*,*) wav_stat(cind(i)),wav_comp(cind(i))
        if (wav_stat(cind(1)).ne.wav_stat(cind(i))) then
          write(*,*) ' not all channels from same station '
        endif
      enddo
      do i=1,3
        if (cind(i).lt.1) then
          write(*,*) ' 3-comp data required '
        endif
      enddo

c
c read data
c
      call wav_read_3channel(cind(1))
c
c filter
c
      if (filt.ne.0) then
        do i=1,3
          call trans_filt(i)
c copy data from y to signal
          do j=1,wav_nsamp(cind(i))
            if (i.eq.1) signal1(j)=y(j)
            if (i.eq.2) signal2(j)=y(j)
            if (i.eq.3) signal3(j)=y(j)
          enddo
        enddo
      else
c remove dc if not filtered
        do i=1,3
          if (i.eq.1) call remove_dc
     &     (signal1,wav_nsamp(cind(i)),dc,wav_nsamp(cind(i)))
          if (i.eq.2) call remove_dc
     &     (signal2,wav_nsamp(cind(i)),dc,wav_nsamp(cind(i)))
          if (i.eq.3) call remove_dc
     &     (signal3,wav_nsamp(cind(i)),dc,wav_nsamp(cind(i)))
        enddo
      endif
        
      np=0
      do i=1,wav_nsamp(cind(1))
        time=wav_delay(cind(1))+i/wav_rate(cind(1))   ! relative to first channel in file
        if (time.ge.start_time.and.time.le.stop_time) then
          if (i.le.wav_nsamp(cind(2)).and.
     &        i.le.wav_nsamp(cind(3))) then
            np=np+1
            dz(np)=signal1(i)
            dy(np)=signal2(i)
            dx(np)=signal3(i)
          endif
        endif 
      enddo
c
c rotate
c
      if (rotate) then
        call rotate_comp(np,1,1,
     *     'R',baz(cind(1)),dr,dy,dx)
        call rotate_comp(np,1,1,
     *     'T',baz(cind(1)),dt,dy,dx)
      endif
c
c plot 
c
      if (rotate) then
      x0=125.
      draw=-2
      xtext=wav_comp(cind(1))(1:2)//wav_comp(cind(1))(4:4)
      ytext=wav_comp(cind(1))(1:2)//'R'
      call xy_plot(1,np,dz,dr,text,xtext,ytext,xlength,xlength,x0,y0,
     &    draw,1,0.,xyc,cha,nc,cx,cy)
      draw=1
      call xy_plot(1,2,dz,dr,text,xtext,ytext,0.,0.,x0,y0,
     &    draw,1,20.,xyc,cha,nc,cx,cy)
      x0=425.
      draw=-2
      xtext=wav_comp(cind(1))(1:2)//wav_comp(cind(1))(4:4)
      ytext=wav_comp(cind(1))(1:2)//'T'
      call xy_plot(1,np,dz,dt,text,xtext,ytext,xlength,xlength,x0,y0,
     &    draw,1,0.,xyc,cha,nc,cx,cy)
      draw=1
      call xy_plot(1,2,dz,dt,text,xtext,ytext,0.,0.,x0,y0,
     &    draw,1,20.,xyc,cha,nc,cx,cy)
      x0=725.
      draw=-2
      xtext=wav_comp(cind(1))(1:2)//'R'
      ytext=wav_comp(cind(1))(1:2)//'T'
      call xy_plot(1,np,dr,dt,text,xtext,ytext,xlength,xlength,x0,y0,
     &    draw,1,0.,xyc,cha,nc,cx,cy)
      draw=1
      call xy_plot(1,2,dr,dt,text,xtext,ytext,0.,0.,x0,y0,
     &    draw,1,20.,xyc,cha,nc,cx,cy)

      else

      x0=125.
      draw=-2
      xtext=wav_comp(cind(1))(1:2)//wav_comp(cind(1))(4:4)
      ytext=wav_comp(cind(2))(1:2)//wav_comp(cind(2))(4:4)
      call xy_plot(1,np,dz,dy,text,xtext,ytext,xlength,xlength,x0,y0,
     &    draw,1,0.,xyc,cha,nc,cx,cy)
      draw=1
      call xy_plot(1,2,dz,dy,text,xtext,ytext,0.,0.,x0,y0,
     &    draw,1,20.,xyc,cha,nc,cx,cy)
      x0=425.
      draw=-2
      xtext=wav_comp(cind(1))(1:2)//wav_comp(cind(1))(4:4)
      ytext=wav_comp(cind(3))(1:2)//wav_comp(cind(3))(4:4)
      call xy_plot(1,np,dz,dx,text,xtext,ytext,xlength,xlength,x0,y0,
     &    draw,1,0.,xyc,cha,nc,cx,cy)
      draw=1
      call xy_plot(1,2,dz,dx,text,xtext,ytext,0.,0.,x0,y0,
     &    draw,1,20.,xyc,cha,nc,cx,cy)
      x0=725.
      draw=-2
      xtext=wav_comp(cind(3))(1:2)//wav_comp(cind(3))(4:4)
      ytext=wav_comp(cind(2))(1:2)//wav_comp(cind(2))(4:4)
      call xy_plot(1,np,dx,dy,text,xtext,ytext,xlength,xlength,x0,y0,
     &    draw,1,0.,xyc,cha,nc,cx,cy)
      draw=1
      call xy_plot(1,2,dx,dy,text,xtext,ytext,0.,0.,x0,y0,
     &    draw,1,20.,xyc,cha,nc,cx,cy)
      endif
 
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c subroutine for spectrogram calculation used with mulplt
c
      subroutine spectrogram(code)
c
c    made by lo jan 2018, adopted for mulplt by jh
c
      implicit none
      include 'mulplt.inc'
      include 'seiplot.inc'

      integer index  ! index in wav structure
      integer code   ! return 0 if ok, otherwise 1
      integer nt,nf  ! number of time and frequenct slices used
      integer ntt,nff! number of time slices in window
      integer smp    ! number of samples in one time slice
      integer i,j,a,b,k,ind,m
      real taper
      real df
      integer n(500)
      real avamp(500)
      real x(max_sample) ! temp data
      real yy(max_sample)
      integer ipow,npad
      complex y_com(max_sample)
      integer spstart,spstop
      real f,xx
      real maxamp,amp
      real dcol
      real deltax,deltay        ! size of boxes in x and y
      
      integer col
      integer nsamp      ! number of samples
      integer ip
      real yy1,yy2,yy3,yy4
      real x1a,x1b,x2a,x2b
      real intp(4000,2000)
      real t,u

      taper=10.
      ntt=200            ! number of time slices in window 
      nff=50 
      spectro_ncol=256
      maxamp=-1.
c
c   transfer data to start of array from traceplot
c
      k=0
      do i=first,last
         k=k+1
         yy(k)=y(i)
      enddo
c      write(*,*) ' debug sgrm ',first,last,yy(1)
      nsamp=k

      ip=-1
      j=0
      m=0
      nf=nff
      nt=ntt
 10   continue
      if (ip.gt.0..and.ip/2+1.lt.nf) then   ! find time window to give enough frequencies
        if (float(nt)/float(nf).ge.4.) then
          nt=int(nt/2.)
        else
          nf=int(nf/2)
        endif
      endif

c   find length of time slice in number of samples taking
c   trace_delay into account
c
c      write(6,*)'trace_delay,pt,nsamp',trace_delay, page_time,nsamp
      smp=int(page_time*rate/float(nt-1))
      
c        write(*,*) ' debug smp = ',smp,page_time

c
c   number of time slices in data
c
c        nt=nsamp/float(smp)   
c      nt=ntt   ! lo
c      write(*,*) ' debug nt,ntt ',nt,ntt
c
c find ipow
c
      k=2
      ip=1
      do while (k.lt.smp*2)  ! take data from before and after time interval
        k=k*2
      enddo
      ip=k
c      write(*,*) ' debug num f ',ip/2+1,nt,nf
      if (nt.eq.1.or.nf.eq.1) then
        write(*,*) ' sepctrogram: not enough resolution'
        return
      endif
      if (ip/2+1.ge.nf) goto 20   ! find time window to give enough frequencies
      goto 10
 20   continue

c      write(6,*) ' debug nt,nf ',nt,nf 

      df=rate/2./float(nf) 
      i=0 
      do while(i.le.nt-1)
        i=i+1 
        a=(i-1)*smp+1
        b=a+smp-1
c        if (b.gt.nsamp) b=nsamp
        j=0
c
c   transfer samples for fft 
c
c        write(*,*) ' debug smp ',smp,a,b
        do k=a-int(.5*smp),b+int(.5*smp)   ! 50 % overlap at start and end
          j=j+1
          if (a.gt.0.and.b.le.nsamp) then
            x(j)=yy(k)
          else
            x(j)=0.   ! before and after segment
          endif
        enddo
c        call prepare(smp,x,taper,ipow,npad,y_com)
        call prepare(j,x,taper,ipow,npad,y_com)
c        write(*,*) ' debug call prepare j ',i,j,ipow,y_com(1)

c
c------- Do FFT -------
c
        j = ipow
        do k = 1,20
          j = j/2
            if (j .eq. 1)then
              j = k
              go to 222
            endif
        enddo
222     continue
        call fft(j,-1.,y_com)
c
c average for frequency bins
c
        do k=1,nf   ! init
          avamp(k)=0.
          n(k)=0
        enddo
        spstart = 2
        spstop = (ipow/2) + 1
        xx = ipow*1./rate
        do k = spstart,spstop
          f = (k-1)/xx
          ind=int(f*.999/df)+1
c          write(*,*) ' debug f,ind ',f,ind,df
          n(ind)=n(ind)+1
          amp=sqrt((1/rate**2)*y_com(k)*conjg(y_com(k)))
          avamp(ind)=avamp(ind)+amp
c          write(*,*) k,ind,avamp(ind)
        enddo

        do k=1,nf
         if (n(k).gt.0) then
            avamp(k)=avamp(k)/float(n(k))
            if (avamp(k).gt.maxamp) maxamp=avamp(k)
            spectro_amp(i,k)=avamp(k)
          endif
        enddo
      enddo

11    continue
c      write(*,*) ' debug spectrogram ',maxamp

c
c assign colors
c

      dcol=maxamp/spectro_ncol
      do i=1,nt
        do j=1,nf
          col=int(spectro_amp(i,j)/dcol)+1
          if (col.lt.1) col=1
          if (col.gt.spectro_ncol) col=spectro_ncol
          spectro_colind(i,j)=col
c          write(*,*) ' debug ',i,j,col
        enddo
      enddo

c
c   make box sizes so they fill plot
c

c      deltax=(1023-xpos)/float(ntt)
      deltax=(1023-xpos)/float(nt)
      deltay=height/float(nf)
cxx
c      write(6,*) 'deltax,deltay,nf,nsamp',deltax,deltay,nf,nsamp
      call spectro_plot(nt,nf,xpos,ypos,deltax,deltay)
c
c interpolation, num rec p 116
c
c      do i=1,1023-xpos
c        do j=1,height
cc find box that point is in
c          k=int(i/deltax)+1   ! x ind
c          m=int(j/deltay)+1   ! y ind
c          if (i.lt.(k+.5)*deltax) then
c          y1=spectro_amp(k,m)
c          y2=spectro_amp(k+1,m)
c          y3=spectro_amp(k+1,m+1)
c          y4=spectro_amp(k,m+1)
c          x1a=
c          t=
c
c        enddo
c      enddo

c
c draw a white line at bottom
c
      call xset_color(5)
c      call xmoveto(xpos+trace_delay*xscale,ypos+1)
c      call xlineto(xpos+trace_delay*xscale,ypos+1,1023
c     &   +trace_delay*xscale,ypos+1)
      call xmoveto(xpos,ypos+1)
      call xlineto(xpos,ypos+1,1023.0,ypos+1.0)
      
      code=0    ! all fine
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine spectro_plot(nx,ny,x0,y0,deltax,deltay)
c
      implicit none
      include 'seiplot.inc'
      include 'mulplt.inc'
      integer nx,ny         	! number of boxes in x and y
      real deltax,deltay        ! spacing of boxes in x and y
      real x0,y0                ! lower left corner
      real xx,yy                ! help variable
      integer i,j
c
c   plot boxes
c
      do i=1,nx
         do j=1,ny
            xx=x0+(i-1)*deltax+trace_delay*xscale
            yy=y0+(j-1)*deltay       
c            write(*,*) ' debug ',spectro_colind(i,j)
	    call fillbox1(xx,yy,xx+deltax,yy+deltay,spectro_colind(i,j))
          enddo
      enddo

      return
      end

