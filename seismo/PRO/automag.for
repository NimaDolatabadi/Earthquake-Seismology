c--------------------------------------------------------------------------
c   program to make read WA amplitudes and make spectral fitting for channels
c   in one or several s-files. This is a simplified version of AUTOSIG and
c   the program does not requre any parameter file. The program will only
c   process stations with a P or S reading. P or S spectra can be made from
c   any component. Default is S-spectra on Z component.
c   The program can also make a grid search for attenuation parameters. 
c
c  updates
c
c  2013 05 03 jh: allow overwrite
c  2013 03 13 jh: add component selection from prompt
c  2013 10 07 jh: outut file automag.list, set fit limit for event to be used,
c                 epicentral distance was wrong for mw calculation
c  2013 11 19 jh: option for giving frequecy range when in interactive mode
c  2014 03 07 jh: now used any component in auto station selection mode, no
c                 requirement of phase reading on z, also make p-spectrum
c  2014 03 13 jh: adjust for p-spectrum from eev, used Lg to select 
c                 s-spectrum, read wa filter from mulplt.def,possible to
c                 have variable q below 1 Hz 
c  2014 04 10 jh: the filter was not used since it was frequecy domian, so 
c                 change to the same filterieng in time domain as in mulplt
c  2014 04 17 jh: stop or go to next events if error reading s-file
c  2014 04 23 jh: missing output to automag.list for p-spectra
c  2014 05 31 jh: new parameter for get_att_vel to indicate read of MULPLT.DEF
c                 if not spec mode. fix bug opening MULPLT.DEF
c                 add correction for additional wa filter, fix so p spectrum does 
c                 overwrite s-spectrum etc
c  2014 08 30 jh: add average stress drop, do not use events outside a fixed stress drop
c                 range for grid search, hardwired range 1-150, q_below_1hz 
c                 was not written out in s-file
c  2014 12 21 jh: fix output of dist to automag.list if no dist
c  2015 12 22 jh: calculate median magnitude and list
c  2016 02 05 jh: bug with component code

c


c
c   
c
c   Interactive input: Give s-file name
c                      Give spec window and wa window, if zero operation
c                         not done
c                      Give channel names, if station is called ALL
c                         all stations with readings (P or S) are used
c   Input from prompt: S-file name, ALL options is then assumed
c                      s_file name, s spec window ,w wa window, if any of the 
c                         windows are not seelected or zero, operation is not
c                         done. E.g. automag sfile s 10, only spectra in 10 s 
c                         window, give component
c
c   Channels to process: If interactive option, each channel must be given
c                        Non interactive: Stations with P or S readings are 
c                                            selected
c                                         First given channesl for those stations,
c                                            which have a reading on any channle selected
c
c
c  Read S-file with readings and location
c  Get wave form files from S-file
c  Enter or find station and component for instrument correction
c  Find S-time or P-time from readings
c  Select out a time window from waveform channel around  S-time,
c      currently 50 s is used starting 2 s before the s
c  Find channel number in waveform file(s) corrstatonding to desired channel
c  Read the S-time window from waveform channel
c  Read response file
c  Prepare response removal, different filters and poles and zeros used for
c     Wood-Anderson response
c  Correct for instrument response
c  Automatically pick maximum amplitude and corresponding period
c  Write out results in interanal s-file, overwite old results
c  Do the automatic spectral fitting using a time window of 20 s starting
c    2 s before the S
c  Write results in internal s-file, overwrite old results
c
c  hardwired parameters
c
c  WA window: 50 s
c  WA window also filtered accoring to MULPLT.DEF, no correction for this filter
c  S-phase only
c  Frequency range for spectral analysis: 0.05 to (samp_rate)/2.5
c  WA period must be less than 5 s
c  Corner frequency must be larger than 0.05 for data to be used
c  Q, density and velocity come from q-model in SEISAN.DEF or MULPLT.DEF

c
c--------------------------------------------------------------------------c
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! seisan parameters
      include 'waveform.inc'              ! waveform data
      include 'rea.inc'                   ! parameter common bliock
      include 'libsei.inc'                ! for all seisan subroutines

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file
      integer nars                        ! number of arguments
      character*80 arg(100)               ! arguments
      integer npresent                    ! number of wav files present
      character*80 text                   ! general text string
      real flow,fhigh                     ! filters
      integer pole_low,pole_high          ! poles of filters
      integer pl,ph                       ! dummy for rem resp call
      integer recfil_pole                 ! poles for recfil filter
      character*8 filt_name               ! recfil filter type
      complex pole(max_resp_value),zero(max_resp_value)  ! complex PAZ

      logical auto_select                 ! if true , autoselect stations
      integer smodel                      ! which spectral model
      
      integer npole,nzero                 ! number of poles and zeros
      real norm                           ! normalization constant
      complex y_com(max_sample)           ! spectrum
      integer seiclen                     ! function
      character*1 spectrum_type           ! type of spectrum, p or s
      real minsp                          ! smallest s-p in data set
      integer nminsp                      ! number of s-p smaller than window
      real lg_velocity                    ! lg_velocity

c  for grid search, number of steps, start and step
       
      integer nq0                         
      real start_q0
      real step_q0
      integer nqalpha
      real start_qalpha
      real step_qalpha
      integer nkappa
      real start_kappa
      real step_kappa
      real min_res                        ! minimum of fit
      integer min_index                   ! index of----
      real av_f0                          ! average f0
      real av_f0_st                       ! ---------- with ok stress drop
      real av_st                          ! average stress drop
      logical grid                        ! if true, grid search
      character*80 result(50000)          ! store results
      real ress(50000)                    ! residuals
      integer iptr(50000)                 ! pointer for sorting
      integer nresult                     ! number of grid seach with results
      integer ngrid                       ! counter of iterations
      

      integer nsamp                       ! number of samples

      logical all                         ! true: read all data, false: select
      integer code                        ! error return code
      integer year,month,day,hour,min,doy
      real sec
      double precision s_time,hyp_abs_time(10),p_time,start_time
      character*5 stat(1000)
      character*4 comp(1000)
      character*2 co(1000)
      character*1 comp_select          ! component to use
      real amp,per                     ! amplitude and period
      integer rea_nphase_org           ! number of phases in input file
      integer kphase                   ! phase index for current phase
      double precision abstime         ! help variable
      real time_before_s               ! time before s to start analysis
      real duration_wa                 ! duration wa_window
      real travel_time                 ! traveltime to beginning of window
      logical wa                       ! if true do wa
      logical overwrite                ! true if overwrite input file
      integer nevent                   ! number of events processed
c
c   for spectral analysis, most are in rea.inc (parameters)
c
      real om,f0,omi,f0i               ! flat level and corner frequency
      real ka                          ! computed delta kappa
      real kappa,q0,qalpha             ! attenuation parameters for phase used
      real dkappa                      ! can be used in grid search
      real pvelocity,svelocity,density ! velocity and density at souce
      real duration_spec               ! duration of spectral window
      character*1 phase                ! P or S
      real stress_low,stress_high      ! acceptabel range of stress drop doing grid search
      real res_m_a(2)                  ! residuals of fit
      real av_res                      ! average residuals of all runs
      real av_res_st                   ! average residual of runs with ok stress drop
      real av_mw                       ! average mw of all runs
      real med_mw                      ! median mw
      real av_mw_st                    ! average mw of all runs with ok stress drop
      integer nmw                      ! number of calculated mw
      real mw(1000)                    ! individual mw
      real av_ml                       ! average Ml of all runs
      real med_ml                      ! median ml
      integer nnml                     ! number of ml
      integer nfit                     ! number of fits made
      integer nfit_st                  ! number of fits made with ok stress drop
      integer nml                      ! number of wa readings

      real av                          ! average
      real fcmin,fcmax,yfirst,ylast    ! frequency range for amplitude and first and last amplitude picked
      real fsmin,fsmax                 ! input spectral frequency range
      real max_residual                ! max acceptable result of spectral fit 
      integer ifirst,ilast             ! sample number corrosponding to
                                       ! yfirst and ylast
      integer maxcros                  ! maximum number of zero crossings
      integer nstat_process            ! number of channels to process
      real fsn1,fsn2                   ! ususeful frequency range
      complex hs                       ! gain of extra wa filter
      real gain                        ! -----------------------

      logical spec                     ! if true do spectral analysis
      integer ntest                    ! number of test values to get, for ml
      integer itest(200)               ! test value number
      real test(200)                   ! returned test value
      real am,bm,cm,dm                 ! ml scale parameters
      real ml(1000)                    ! ml
      real hdist                       ! hypocentral distance
      real maxdist                     ! max epi dist for grid search
c
      integer i,k,l,m,n,j,kq0,kqalpha,kkappa  ! counters

      open(66,file='automag.par',status='unknown')
c
c   set defaults
c


      spec=.false.                     !  spectrum
      grid=.false.
      wa=.false.
      auto_select=.false.
      overwrite=.false.
      nevent=0
      spectrum_type='S'               ! default make s-spectrum
      comp_select='Z'                 ! default use z-component
      minsp=99999.
      nminsp=0

c
c   hard wired parameters
c     
      duration_spec=20.0              ! duration spec window
      duration_wa=50.0                ! duration wa window
      time_before_s=2.0               ! start window 2 s before S
      fsmin=0.05                      ! lowest frequency for spectral fit
      max_residual=0.3                ! max residual for acceptable fit
      lg_velocity=3.5                 ! lg velocity
      stress_low=1.0                  ! acceptable stress range for grid search
      stress_high=150.0  
      q_below_1Hz=1.0                 ! default constant q below 1 Hz    


      nfit=0
      nfit_st=0
      nmw=0
      nnml=0
      av_ml=0.0
      av_res=0.0
      av_res_st=0.0   
      av_mw=0.0
      av_mw_st=0.0
      av_f0=0.0
      av_f0_st=0.0
      av_st=0.0
      nresult=0
      min_res=999.0
      nq0=1
      nqalpha=1
      nkappa=1
      nml=0
      am=1
      bm=1.11
      cm=0.00189
      dm=-2.09

c
c   get arguments
c
      call get_arguments(nars,arg)
c
c   if first argument is grid, then only gridsearch
c
      if(arg(1).eq.'grid') then
        grid=.true.
        nars=0      ! force manual input
        spec=.true.
        wa=.false.
        goto 7
      endif
c
c  if overwite is set, set overwrite flag and
c  remove overwrite from argument list
c
      k=0
      do i=1,nars
        if(arg(i)(1:9).eq.'overwrite') then
           overwrite=.true.
           k=i
        endif
      enddo
      l=0
      if(k.gt.0) then
        do i=1,nars
           if(i.ne.k) then
              l=l+1
              arg(l)=arg(i)
           endif
        enddo
        nars=nars-1
      endif
c
c   no arg, both spectrum and wa
c
      if(nars.eq.0) then
         spec=.true.
         wa=.true.
      endif

      if(nars.gt.0) then
        infile=arg(1)
        write(6,'(a,a)')' Input S-file  ',arg(1)
        open(1,file=infile,status='old',err=5)
        goto 6
 5      continue
        write(6,*)'File does not exist'
        stop
 6      continue
c
c   if only one arg, it is input file, both spectrum and wa, auto select stations
c
        if(nars.eq.1) then
           spec=.true.
           wa=.true.
           auto_select=.true.
        endif
c
c   find if other arguments
c 
        if(nars.gt.1) then
        auto_select=.true.

        do i=2,nars
          if(arg(i)(1:1).eq.'s') then
            spec=.true.
            read(arg(i+1),*) duration_spec
            if(duration_spec.eq.0.0) spec=.false.
          endif
          if(arg(i)(1:1).eq.'p'.or.arg(i)(1:1).eq.'P') then
            spectrum_type='P'
            spec=.true.
            wa=.true.
            if(duration_wa.eq.0.0) wa=.false.
          endif
          if(arg(i)(1:1).eq.'w') then
            wa=.true.
            read(arg(i+1),*) duration_wa
            if(duration_wa.eq.0.0) wa=.false.
          endif
          if(arg(i)(1:1).eq.'n') then
            comp_select='N'
          endif
          if(arg(i)(1:1).eq.'e') then
            comp_select='E'
          endif
        enddo
        endif
      endif


c
c   get reset test values for ml, only take values for STATION0.HYP
c
      ntest=5     
      itest(1)=75
      itest(2)=76
      itest(3)=77
      itest(4)=78

      call loc_reset_test(ntest,itest,test,' ')
c
c   check values, if none, use default hutton and boore
c
      if(test(1).eq.-9999.9) test(1)=1.0
      if(test(2).eq.-9999.9) test(2)=1.11
      if(test(3).eq.-9999.9) test(3)=0.000189
      if(test(4).eq.-9999.9) test(4)=-2.09

      
      write(6,'(a,4f10.6)')         ' Ml constants:        ',
     *(test(i),i=1,4)
      write(66,'(a,4f10.6)')         ' Ml constants:        ',
     *(test(i),i=1,4)
c
c   get ml filter
c
      call get wa_filt(flow,pole_low,fhigh,pole_high)
c
c  only use 4 pole filters
c
      if(pole_low.gt.0.and.pole_low.ne.4) then
         write(6,*)'Only 4 pole filter for wa, changed'
         pole_low=4
      endif
      if(pole_high.gt.0.and.pole_high.ne.4) then
         write(6,*)'Only 4 pole filter for wa, changed'
         pole_high=4
      endif
      
      write(6,'(a,f6.2,i3,f6.2,i3)')' WA filter constants: ',
     *flow,pole_low,fhigh,pole_high

      write(66,'(a,f6.2,i3,f6.2,i3)')' WA filter constants: ',
     *flow,pole_low,fhigh,pole_high




ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      

 7    continue   ! here from above if grid search



c
c   open output file s-file
c
      open(2,file='automag.out',status='unknown')
c
c   open list of all components processed etc, use with plotting program
c
      open(55,file='automag.list',status='unknown')
c
c   get seisan parameters, here attenuation parameters
c
      call get_seisan_def           
c
c   get input sfile name, check if exist
c
      if(nars.eq.0) then
 9       continue
         write(6,*) 'Give input S-file '
         read(5,'(a)') infile
         open(1,file=infile,status='old',err=10)

         if(grid) then  
            write(6,*)'Give spec window, enter for def (20)'
            read(5,'(a)') text
            if(text.ne.' ') read(text,*)duration_spec
            write(6,*)'Give spectral range, enter for 0.05-srate/2.5'
            read(5,'(a)') text
            fsmax=0.0
            if(text.ne.' ') read(text,*) fsmin,fsmax
            if(fsmin.lt.0.05) then
              write(6,*)'min f too low, reset to 0.05'
              fsmin=0.05
            endif
            write(6,*)'give max distance, enter for all'
            read(5,'(a)') text
            if(text.eq.' ') then
               maxdist=10000.0
            else
               read(text,*) maxdist
            endif
         else
            write(6,*)
     *      'Spec and wa windows, enter for def (20,50),',
     *      ' 0 means operation not selected'
            read(5,'(a)') text
            if(text.ne.' ') read(text,*)duration_spec,duration_wa
            if(duration_spec.eq.0.0) spec=.false.
            if(duration_wa.eq.0.0) wa=.false.
            if(spec) then
               write(6,*)'Give spectral range, enter for 0.05-srate/2.5'
               read(5,'(a)') text
               fsmax=0.0
               if(text.ne.' ') read(text,*) fsmin,fsmax
               if(fsmin.lt.0.05) then
                 write(6,*)'min f too low, reset to 0.05'
                 fsmin=0.05
               endif
             endif
         endif



         if(.not.spec.and.(.not.wa)) then
             write(6,*)' No spectral or time window selected, will stop'
             stop
         endif
c
c   get grid variables
c
         if(grid) then
            wa=.false.   ! do not do wa in grid mode
            write(6,*)'Start q0, step Q0 and number of steps'
            read(5,*) start_q0,step_q0,nq0
            write(6,*)'Start qalpha, step qalpha and number of steps'
            read(5,*) start_qalpha,step_qalpha,nqalpha
            write(6,*)'Start kappa, step kappa and number of steps'
            read(5,*) start_kappa,step_kappa,nkappa
            open(3,file='automag_grid.out',status='unknown')
            n=nq0*nqalpha*nkappa
            write(6,'(a,i8)')' Number of tests is: ',n
            if(n.gt.50000) then
               write(6,*)'Too many, max is 50000'
               stop
            endif
            write(6,'(a)')' Continue(n/y=enter)'
            read(5,'(a)') text
            if(text(1:1).ne.'y'.and.text(1:1).ne.' ') then
               stop
            endif
         endif
         goto 11
 10      continue
         write(6,*)' No such input file'
         goto 9
 11      continue
      endif
c
      all=.true.        ! read all parameters from s-file

c
c   give stations and components to use if interactive input
c
      if(nars.eq.0) then
         k=0
         write(6,*) 
     *   'Station code and component (e.g. BER  BHZ), end with blank'
         write(6,*)
     *   'Giving station code ALL   : Use all Z channels with readings'
         write(6,*)
     *   'Giving station code ALL C : Use all C channels with readings'

 40      continue
         read(5,'(a)') text
         if(text(1:3).eq.'ALL') then
            auto_select=.true.
            if(text(5:5).ne.' ') comp_select=text(5:5)
            goto 45
         endif
         if(text.ne.' ') then
            k=k+1
            stat(k)=text(1:5)
            comp(k)(1:2)=text(6:7)
            comp(k)(3:3)=' '
            comp(k)(4:4)=text(8:8)
            goto 40
         endif
         nstat_process=k         ! number of stations to process 
 45      continue

         write(6,*)'P or S-spectrum=default (enter)'
         read(5,'(a)') text
         if(text(1:1).eq.'p'.or.text(1:1).eq.'P') spectrum_type='P'
      endif
c
c   write out parameters
c                           '                      '

      write(66,'(a,a)')' Input file:               ',
     *infile(1:seiclen(infile))
      write(66,'(a,a)')' Component used:           ',comp_select
      write(66,'(a,a)')
     *' Spectrum type:            ',
     *spectrum_type
      write(66,'(a,2f8.1)')
     *' Spec dur. wa. window:     ',
     *duration_spec,duration_wa
      write(66,'(a,2f8.2)')    
     *' Filter:                   ',fsmin,fsmax
  
      if(grid) then
         write(66,'(a,f8.1)') 
     *' Max distance:             ', maxdist
         write(66,'(a,2f8.1,i8)')
     *' Start Q0, delQ,  nQ       ',
     *   start_q0,step_q0,nq0
         write(66,'(a,2f8.2,i8)')
     *' Start Qa, delQa, nQa      ',
     *   start_qalpha,step_qalpha,nqalpha
         write(66,'(a,2f8.3,i8)') 
     *' Start ka, delka, nka      ',
     *   start_kappa,step_kappa,nkappa
      endif
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   grid loops
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do kq0=1,nq0
      do kqalpha=1,nqalpha
      do kkappa=1,nkappa
      ngrid=ngrid+1
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc       loop of events
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      nevent=0
 20   continue   ! here from end for next event
c
c  initialize wav reading
c
      call wav_init
      call wav_mem_init
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump to stop
c
      if(code.eq.1) goto 1000
c
c  if code is 3, a serious error whole event might not have been
c  read so go to next event if several events, else to end
c
      if(code.eq.3) then 
         if(.not.overwrite) goto 20
         goto 1000
      endif


      nevent=nevent+1

      rea_nphase_org=rea_nphase
c
c   get waveform file names, could be several
c
      call auto_tr(data,rea_nhead,rea_nrecord,wav_nfiles,wav_filename)
      
c   find how many waveform files are present,
c   replace the origial names without path, with the present files
c   full path like
c
       npresent=0                      ! no files initially
       do i=1,wav_nfiles
          call  get_full_wav_name(wav_filename(i),text)            
          if(text.ne.' ') then
             npresent=npresent+1
             wav_filename(npresent)=text
          endif
       enddo
c
c   print how many files were found
c
      wav_nfiles=npresent
      write(6,*)' Number of wav-files present', wav_nfiles

c
c   terminate if no waveform files
c
      if(wav_nfiles.eq.0) then
         write(6,*)'No waveform files this event, got to next event'
         goto 20
      endif
c
c   loop to read all headers of all wav files
c
      do i=1,wav_nfiles
         write(6,'(1x,a)') wav_filename(i)
c
c   read all headers of file i
c
         call read_wav_header(i)
c
c   output possible errors
c
         if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *   wav_error_message
      enddo

      write(6,*)' Total number of channels available:',wav_nchan
c
c   terminate if no channels
c
      if(wav_nchan.eq.0) then
        write(6,*)'No channels found, go to next event'
        goto 20
      endif
c
c   write some input info for each channnel on screen
c
      write(6,*)
      write(6,'(a,a)')
     *'CHA STAT  COMP  YEAR MO DA HR MI    SEC   NSAMP ',
     *'    RATE    DELAY DURATION'
      do i=1,wav_nchan
         write(6,'(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i8,3f9.3)')
     *   i, wav_stat(i),wav_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i)
      enddo

c 
c  abs origin time
c
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),hyp_abs_time(1))
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)


c
ccccccccccccccccccccccccccc autoselect stations cccccccccccccccccccccccc
c
c
c   find stations in file with P or S readings, 
c    "Spectral phases" are not checked.
c

      if(auto_select) then
         k=0
         do i=1,rea_nphase
            if((rea_phase(i)(1:1).eq.'P'.or.rea_phase(i)(1:1).eq.'S').
c     *   and.rea_co(i)(2:2).eq.'Z'.and.
c     *      and.rea_co(i)(2:2).eq.comp_select.and.
     *      and.rea_phase(i)(1:4).ne.'SPEC') then
               k=k+1
               stat(k)=rea_stat(i)
               co(k)=rea_co(i)
            endif
         enddo

         write(6,'(7(a5,1x,a2,1x))')(stat(i),co(i),i=1,k)
c
c   remove duplicates
c
         do i=1,k
            do j=i+1,k
              if(stat(i).eq.stat(j)) stat(j)=' '
            enddo
         enddo
         m=0
         do i=1,k
            if(stat(i).ne.' ') then
               m=m+1
               stat(m)=stat(i)
               co(m)=co(i)
            endif
         enddo
c      write(6,*)

c      write(6,'(7(a5,1x,a2,1x))')(stat(i),co(i),i=1,m)
c
c  find which of these stations have comp_select in wav file and 
c  select only those, make sure same type of channel (e.g. S, B) 
c  as used to read phase
c
         k=0 
         do i=1,m
           do j=1,wav_nchan
              if(stat(i).eq.wav_stat(j).and.wav_comp(j)(4:4).
     *        eq.comp_select.and.
     *        (wav_comp(j)(1:1).eq.co(i)(1:1).or.
     *        wav_comp(j)(1:1).eq.'H')) then   !afad fix
                 k=k+1
                 stat(k)=stat(i)
                 comp(k)=wav_comp(j)
                 goto 42
              endif
           enddo
 42        continue
         enddo
         nstat_process=k
      endif    ! end of auto select section


      write(6,*)
      write(6,*) 'Components auto selected to use'
      write(6,'(7(a5,1x,a4,1x))')(stat(i),comp(i),i=1,k)
                    
c-------------------------------------------------------------------
c   loop over channels selected for analysis
c-------------------------------------------------------------------

      do m=1,nstat_process
         write(6,*)
         write(6,'(a)')'**********************************************'
         write(6,'(a,a5,1x,a4,1x,a)')'***** process ',stat(m),comp(m),
     *   ' ********************'
         if(grid) write(6,'(a,i6,a)')
     *  '****** ITERATION NUMBER **********************', ngrid,
     *  ' *********************'
         write(6,*)

c
c
c   find where the S-phase window is for station stat, use first s found,
c   which could be on another channel than used for analysis
c   also find P-time in case a P-spectrum is made
c
c   look for P-phase
c
      p_time=0.0
      do i=1,rea_nphase
        if(rea_phase(i)(1:1).eq.'P'.and.rea_stat(i).eq.stat(m).and.
     *  rea_phase(i)(1:4).ne.'SPEC') then
           p_time=rea_abs_time(i)  ! abs p-time
c
c   save phase index to know distance
c
           kphase=i
           write(6,'(a,a)')' P time on component ',rea_co(i)
           call sectim(p_time, year,doy,month,day,hour,min,sec)

           write(6,'(a,i4,1x,2i2,1x,2i2,f6.1)') ' Time of P ',
     *     year,month,day,hour,min,sec
           goto 222   ! jump to S-phase
        endif
      enddo
      write(6,'(a,a)')' No P-phase for station ',stat(m)
 222  continue
c
c  look for s-phase
c
      do i=1,rea_nphase
        if(rea_phase(i)(1:1).eq.'S'.and.rea_stat(i).eq.stat(m).and.
     *  rea_phase(i)(1:4).ne.'SPEC') then
           s_time=rea_abs_time(i)  ! abs s-time
c
c   save phase index
c
           kphase=i
           write(6,'(a,a)')' S time from S on component ',rea_co(i)
           goto 100   ! found so jump out
        endif
      enddo
c
c  S from P since no S
c
      do i=1,rea_nphase
        if(rea_phase(i)(1:1).eq.'P'.and.rea_stat(i).eq.stat(m)) then
           kphase=i
           if(rea_dist(i).eq.0.0) then
              write(6,*)'No distance, skip station'
              goto 500
           endif
c
c  direct phase or deep event
c
           if(hyp_depth(1).ge.50.0.or.rea_dist(i).le.200.0) then
              s_time=(rea_abs_time(i)-hyp_abs_time(1))*1.78+
     *        hyp_abs_time(1)
              write(6,'(a,a)') ' S time from P on component ',rea_co(i)
           endif
c
c
c  lg phase
c
           if(hyp_depth(1).lt.50.0.and.rea_dist(i).gt.200.0) then
              s_time=hyp_abs_time(1)+rea_dist(i)/lg_velocity
              write(6,'(a,a)') ' Lg time from distance on component '
     *        ,rea_co(i)
           endif
        
           write(6,'(a,f6.1)') ' Epicentral distance ',rea_dist(i) 

           goto 100   ! found so jump out
        endif
      enddo
c
c   no phase found
c
      write(6,'(a,a)')' No phase found for station, no processing ',
     *stat(m)

      goto 500    ! next channel
      
c---------------------
c   a phase found
c---------------------

 100  continue
c
c   check distance if grid
c
       if(rea_dist(kphase).gt.maxdist.and.grid) then
           write(6,*)'Station too far for grid search, skip'
           goto 500
       endif

c
c  abs time of S, convert to year month ...
c
      call sectim(s_time, year,doy,month,day,hour,min,sec)

      write(6,'(a,i4,1x,2i2,1x,2i2,f6.1)') ' Time of S/Lg ',
     *year,month,day,hour,min,sec
c
c  find channel
c
      call wav_find_chan(stat(m),comp(m),k)
      if(wav_error_message.ne.' ')write(6,*) wav_error_message
 
      if(k.eq.0) then
        write(6,'(a,a5,1x,a4)') 'Channel not found: ',stat(m),comp(m)
        goto 500   ! try next
      endif
c
c   read whole channel
c
      call wav_read_channel(k)
c
c   select out time window, only for one channel
c
       wav_out_nchan=1        ! one channel
       wav_out_chan(1)=k      ! channel number
c
c  start time relative to start of particular channel
c
       wav_out_start(k)=s_time-wav_abs_time(wav_first)-time_before_s
c
c   look for max amp in a duration_wa window
c
       wav_out_duration(k)=duration_wa 

c
c  specify channel for response 
c
       wav_resp_stat=stat(m)
       wav_resp_comp=comp(m)
       wav_resp_year=wav_out_year(k)
       wav_resp_month=wav_out_month(k)
       wav_resp_day=wav_out_day(k)
c
c   read response file
c
      call read_resp

      if(wav_resp_status(1:1).ne.' ') then
         write(6,'(a,a,a)') 'No response for ', 
     *   wav_resp_stat,wav_resp_comp
         write(6,*) 'Try next channel'
         goto 500
      endif



c
c   find if data is available
c
       call wav_get_interval

c       write(6,*) 'Select status, 4 is ok',wav_out_status(k)
c       write(6,*) 'Start sample',wav_out_first_sample(k)
c       write(6,*) 'Duration available', wav_out_duration(k)
c
c   if here, processing possible, put in list file
c
c
c   write in list file even if it might not be used
c         
        write(55,'(a,a,a)') data(1)(1:79),
     *  '   Type of spectrum is ',spectrum_type
        i=rea_dist(kphase)+0.5
c
c  if no dist, negtive, make it zero
c
        if(i.lt.0) i=0

        write(55,'(a,a,i5)') stat(m),comp(m),i
c
c-------------------------------------------------------------------------
c   skip wa if not selected, must be after respoense since used for spec
c-------------------------------------------------------------------------
c
       if(.not.wa)  then
         write(55,*)' '     ! signal no wa
         goto 400
       endif
       amp=0.0
       per=0.0 
c
c   skip if not enough data
c
        if(wav_out_status(k).ne.4) then
           write(6,*)' Not enough data for WA, skip channel for WA'
           write(55,*)  ! signal no wa
           goto 400
        endif
         
c
c   move data interval to beginning of array
c
       nsamp=wav_out_nsamp(k)

       l=1
       do i=wav_out_first_sample(k),
     * wav_out_first_sample(k)+ wav_out_nsamp(k)-1
         wav_y1(l)=signal1(i)
         l=l+1
       enddo 
c
c  subtract dc
c
      av=0.0
      do i=1,wav_out_nsamp(k)
        av=av+wav_y1(i)
      enddo
c
      av=av/wav_out_nsamp(k)

      do i=1,wav_out_nsamp(k)
        wav_y1(i)=wav_y1(i)-av
      enddo

c   
c  wood-anderson poles and zeros to be multipled by corrected signal,
c  set npole and nzero to zero if only response removal
c
      npole=2
      nzero=2
      norm=1.0
      pole(1)=(-6.283,-4.712)
      pole(2)=(-6.283, 4.712)
      zero(1)=(0.0,0.0)
      zero(2)=(0.0,0.0)      
c
c   apply a taper
c  
      call applytaper(wav_y1,nsamp,10.)   ! apply taper, width 10% of half the samples
c
c   do filtering in time domain if required
c
      if(flow.ne.0.or.fhigh.ne.0) then
         recfil_pole=4        ! the default
         filt_name='BP      ' ! default a bp filter
c
c  if a bp,  both poles must be the same
c
         if(flow.ne.0.0.and.fhigh.ne.0.0) then
            if(pole_low.ne.0.and.pole_high.ne.0)then
               recfil_pole=pole_low
            endif
         endif
c
c  lp filter
c
         if(flow.eq.0.0) then
            filt_name='LP      '
            if(pole_high.ne.0) recfil_pole=pole_high ! it could be different from 4
         endif
c
c  hp filter
c
         if(fhigh.eq.0.0) then
            filt_name='HP      '
            if(pole_low.ne.0) recfil_pole=pole_low ! it could be different from 4
         endif     
c
c   use recfil to filter
c
          call recfil(wav_y1,nsamp,wav_y1,'BU      ',
     *    0.0,0.0,recfil_pole,
     *    filt_name,flow,fhigh,1.0/wav_out_rate(k),1)  
        endif               
c
c   correct for instument, no filtering
c
      call remove_resp(wav_y1,y_com,nsamp,wav_out_rate(k),1,
     *0.0,0.0,pl,ph,
     *zero,pole,nzero,npole,norm) 
c
c  set auto amp parameters
c
      fcmin=0.01
      fcmax=20.0
      maxcros=2
c
c   autopick amplitude
c
      call auto_amp
     *(wav_y1,wav_out_nsamp(k),wav_out_rate(k),fcmin,fcmax,maxcros,
     *yfirst,ylast,ifirst,ilast)
c
c   check if valid reading
c
      if(ifirst.eq.0.or.abs(yfirst-ylast).gt.90000000) then
         write(6,*)'No valid amplitude reading'   
         write(55,*)' '   ! signal no ml
c
c   calculate amplitude and period
c
      else
          amp=abs(yfirst-ylast)/2.0   ! first and last sample number for 2 extreemes
          per=2.0*abs(ilast-ifirst)/wav_out_rate(k)
c
c  if perieod more then 5 sec, consider unreliable
c
          if(per.lt.5.0) then
c
c  correct for filter if used
c
            
             if(pole_low.gt.0.or.pole_high.gt.0) then
               gain=1.0
               if(flow.gt.0.0.and.pole_low.gt.0) then
                  call bworth(1.0/per,
     *            flow,-pole_low,hs) 
                  gain=gain*cabs(hs)
               endif
               if(fhigh.gt.0.and.pole_high.gt.0) then
                  call bworth(1.0/per,
     *            fhigh,pole_high,hs) 
                  gain=gain*cabs(hs)
               endif
               amp=amp/gain
               write(6,*) 'wa: gain,period',gain, per
             endif
        
c

c
c   save trace in file
c
             text=stat(m)(1:seiclen(stat(m)))//'.'//comp(m)//'.wa'
             do i=1,10
               if(text(i:i).eq.' ') text(i:i)='_'
             enddo
             open(33,file=text(1:13),status='unknown')
             write(33,*)wav_out_nsamp(k),wav_out_rate(k),ifirst,ilast
             do i=1,wav_out_nsamp(k)
               write(33,*) wav_y1(i)
             enddo
             close (33)
c
c   calculate time of pick
c
             abstime=s_time+ifirst/wav_out_rate(k)-time_before_s
             call sectim(abstime,
     *       year,doy,month,day,hour,min,sec)
             write(6,'(a,1x,i4,1x,2i2,1x,2i2,f6.1)')
     *       ' Time of amplitude ',year,month,day,hour,min,sec
             write(6,'(a, 2f11.2)')  ' Amplitude(nm) and period(sec)',
     *       amp,per
             nml=nml+1
c             write(6,*)
c
c   save in data array
c
             rea_nphase=rea_nphase+1
             call rea_phase_clear(rea_nphase)
             rea_stat(rea_nphase)=stat(m)
             rea_comp(rea_nphase)=comp(m)
             call component(comp(m),rea_co(rea_nphase))
             rea_phase(rea_nphase)='IAML     '      
             rea_year(rea_nphase)=year
             rea_month(rea_nphase)=month
             rea_day(rea_nphase)=day
             rea_hour(rea_nphase)=hour
             rea_min(rea_nphase)=min
             rea_sec(rea_nphase)=sec     
             rea_amp(rea_nphase)=amp
             rea_per(rea_nphase)=per      
c
c   check if phase was there from before, then delete
c
             do l=1,rea_nphase_org
               if(rea_stat(l).eq.stat(m).and.rea_co(l).
     *         eq.rea_co(rea_nphase)
     *         .and.rea_phase(l)(1:4).eq.'IAML') then
                  rea_phase(l)(1:6)='DELETE'
               endif
             enddo
c
c   calculate Ml
c
             ml(nml+1)=0.0

c             write(6,*)'dist',rea_dist(kphase)
             if(rea_dist(kphase).gt.0.0) then
                nnml=nnml+1
                hdist=sqrt(rea_dist(kphase)*rea_dist(kphase)+
     *          hyp_depth(1)*hyp_depth(1))
c                write(6,*)'hdist',hdist
                ml(nml)=am*alog10(amp)+bm*alog10(hdist)+cm*hdist+dm
                av_ml=av_ml+ml(nml)
                write(6,'(a,f4.1)') ' Ml=',ml(nml)
             else
                write(6,'(a)')' No distance, cannot calculate Ml'
             endif
             i=amp+0.5
             write(55,'(a,f3.1,a,i6,a,f4.2)')'Ml=',ml(nml),
     *       ' A=',i,' T=',per    
           else
             write(55,*)  ! signal no wa
             write(6,'(a,f6.1)')'Period too large: ',per
           endif   ! for period less than 5 s
       endif       ! for valid amp reading
c
c------------------------------------------------------------------------------
c   spectral analysis
c------------------------------------------------------------------------------
c
 400     continue   ! here if a channel was skipped due to too litle data for wa
                    ! or wa not selected

         if(spec) then
            write(6,*)
            write(6,'(a,a,a)')' *** Now making spectrum for ',
     *      spectrum_type, '***'
c
c  if a P-spectrum, check if a P-reading, else skip. the check for 
c  S was done above since S is also needed for wa
c
            if(spectrum_type.eq.'P'. and. p_time.eq.0) then
               write(6,*)' No P-reading so no P-spectrum'
               write(55,*)' '  ! blank line to indicae no spectrum
               write(55,*)' '
               goto 500
            endif
c
c  if a P-spectrum, also check that the window does no go into the S
c  cannot be more than the s-time minus 2 s 
c
            if(spectrum_type.eq.'P'. and. (s_time-p_time-2.0). lt.
     *      duration_spec) then
               write(6,*)'Too long window for P-spectrum, no spectrum'
               write(6,'(a,f7.1)') ' S-P time is ',s_time-p_time
               nminsp=nminsp+1
               if(minsp.gt.(s_time-p_time)) then
                  minsp=s_time-p_time
               endif
               write(55,*)' '  ! blank line to indicae no spectrum
               write(55,*)' '
               goto 500
            endif
            

c
c   get attenuation parameters for particular depth and type of phase from
c   SEISAN.DEF. If not there, get from MULPLT.DEF
c
          
            smodel=0          ! indicate that MULPLT.DEF not already read 
            call get_att_vel(hyp_depth(1),spectrum_type,
     *      q0,qalpha,kappa,pvelocity,svelocity,density,smodel)
            if(smodel.eq.1) write(6,*)'Using spec model  '
            if(smodel.eq.2) write(6,*)'Using mulplt model'

           if(grid) then
            q0=start_q0+(kq0-1)*step_q0
            qalpha=start_qalpha+(kqalpha-1)*step_qalpha
            kappa=start_kappa+(kkappa-1)*step_kappa
           endif

            write(6,'(a,f6.1,6f5.2)')
     *     ' q0,qalpha,q1Hz,kappa,pvelocity,svelocity,density ', 
     *      q0,qalpha,q_below_1hz,kappa,pvelocity,svelocity,density
            write(6,*)

            
            phase=spectrum_type
            dkappa=0.0
            res_m_a(2)=0.0
c
c   check and set high frequency limit
c
            if(fsmax.gt.wav_out_rate(k)/2.5) then
               write(6,*)' max frequecy too high, reset to ',
     *         wav_out_rate(k)/2.5
               fsmax=wav_out_rate(k)/2.5
            endif
            if(fsmax.eq.0.0) fsmax=wav_out_rate(k)/2.5

c            travel_time=s_time-hyp_abs_time(1)
c
c            write(6,*) fsmin,fsmax

            if(spectrum_type.eq.'P') then
               start_time=p_time
            else
               start_time=s_time
            endif
           
         
            call get_om_f0(
     *      signal1,                                     ! data vector
     *      k,                                           ! index of current waveform channel
     *      fsmin,fsmax,                                 ! lower and higher input fixed frequency limits
     &      start_time,                                  ! abs phase time of phase used
     *      time_before_s,                               ! start before s-time, same as for wa
     &      duration_spec,                               ! duration
     *      om,f0,ka,                                    ! output omega0, corner f and delta kappa
     *      omi,f0i,                                     ! input of --------------
     &      hyp_abs_time(1),                             ! abs origin time, not in common block
     *      q0,qalpha,                                   ! q-relation used for particular phase
     *      q_below_1Hz,                                 ! q parameter for q below 1 Hz
     *      kappa,                                       ! kappa
     &      dkappa,                                      ! used in grid search, return value also
     *      50,                                          ! size of population in genetic algorithm
     *      250,                                         ! number of generations to be done
     &      100,100,5,
     &      res_m_a,
     *      2.0,                                         ! 1=GA, 2=GRID
     *      1.0,                                         ! normalization constant
     *      0.0,0.0,                                     ! fixed frequency range, not used
     *      fsn1,fsn2)                                   ! frequency range with good s/n
            
            write(6,*)
            write(6,'(a,f6.2,a,f6.2,a,f6.3)')' Omega0=',om,
     *      '  Corner frequency=',f0,' Fit residual= ',res_m_a(2)
            
c
c write results to common block if ok
c
            if(om.ne.0..and.f0.gt.fsmin.and.res_m_a(2).lt.max_residual)
     *      then


c
c   first write to list file
c
              write(55,'(a,f5.2,a,f5.2)') 'f=',fsn1,'-',fsn2

c
c   average residual and f0, used when using grid search
c
              av_res=av_res+res_m_a(2)
              av_f0=av_f0+f0
c
c   count number of ok fits

              nfit=nfit+1             

              rea_nphase=rea_nphase+1
              k=rea_nphase

c
c   calculate time of start of window
c
              if(spectrum_type.eq.'S') then
                 abstime=s_time-time_before_s
              else
                 abstime=p_time-time_before_s 
              endif
               
              call sectim(abstime, 
     *        year,doy,month,day,hour,min,sec)
              rea_hour(k)=hour
              rea_min(k)=min
              rea_sec(k)=sec    


              rea_stat(k)=stat(m)
              rea_comp(k)=comp(m)
              rea_phase(k)= '     '
              rea_phase(k)(1:4)='SPEC'
              rea_phase(k)(5:5)=phase
              rea_omega0(k)=om
              rea_cornerf(k)=f0
              rea_vp(k) = pvelocity
              rea_vs(k) = svelocity
              rea_q0(k)=q0
              rea_qalpha(k)=qalpha
              rea_q_below_1hz(k)=q_below_1hz
              rea_kappa(k)=kappa 
              rea_density(k)=density
              rea_swin(k)=duration_spec
              rea_dist(k)=rea_dist(kphase)
c
c source dimension
c
              if(phase.eq.'P') then
                 rea_radius(k)=.37*pvelocity/f0
              else
                 rea_radius(k)=.37*svelocity/f0
              endif
c
c   geo distance
c
              if(rea_dist(kphase).gt.0.0) then
                 nmw=nmw+1
                 call spec_dist(phase,rea_dist(kphase),hyp_depth(1),
     *           rea_geo_dist(k))
                 rea_geo_dist(k)=1./rea_geo_dist(k)

c
c log moment
c

                 rea_moment(k)=4.*3.1416*
     &           rea_density(k)*10.**rea_omega0(k)
     &           * .833   ! kk=1/(2.0*0.6)
     &           * rea_geo_dist(k)
     &           * (1000.**5)   ! to convert units of density and distance
     &           / (1.e9)       ! to convert to Nm
                 if (phase.eq.'P') then
                    rea_moment(k)=rea_moment(k)*
     &              rea_vp(k)**3
                 else
                    rea_moment(k)=rea_moment(k)*
     &              rea_vs(k)**3
                 endif
                 rea_moment(k)=log10(rea_moment(k))
                 rea_mw(k)=2./3.*rea_moment(k)-6.06

                 write(6,'(a,f5.1)')' Mw=',rea_mw(k)
                 write(55,'(a,f3.1)')'Mw=',rea_mw(k)
                 av_mw=av_mw+rea_mw(k)
                 mw(nmw)=rea_mw(k)
c                 write(17,*) travel_time,rea_moment(k)
c
c   stress drop
c
                 rea_sdrop(k)=(.44*10.**rea_moment(k))
     &           / (1.e14*rea_radius(k)**3)
                 
c-----------
c  for grid search, only use fits within a given stress drop range
c-----------
                 if(rea_sdrop(k).ge.stress_low.and.rea_sdrop(k).le.
     *           stress_high) then
                    nfit_st=nfit_st+1
                    av_res_st=av_res_st+res_m_a(2)
                    av_f0_st=av_f0_st+f0
                    av_st=av_st+rea_sdrop(k)
                    av_mw_st=av_mw_st+rea_mw(k)
                 endif                  
             else
                 write(6,*)' No distance, no Mw calculation'
                 write(55,'(a)')'Mw='
                 rea_moment(k)=0.0
                 rea_mw(k)=0.0
                 rea_sdrop(k)=0.0
             endif
c
c   check if SPEC phase was there from before, then delete
c
             do l=1,rea_nphase_org

                if(rea_stat(l).eq.rea_stat(k).and.rea_comp(l).
     *          eq.rea_comp(k)
     *          .and.rea_phase(l)(1:5).eq.'SPEC'//spectrum_type) then
                   rea_phase(l)(1:6)='DELETE'
                 endif
             enddo
            else
             write(6,*)'Bad spectral parameters, spectrum not used'           
c
c   write two blank lines in list file to indicate no good spectrum
c
             write(55,*)
             write(55,*)
          endif
         else
            write(55,*)   ! indicate that spectrum not selected at all
            write(55,*)
         endif
c
c  end of channel loop
c 
 500     continue   ! get here if a channel was skipped due to missing info
       enddo

      write(6,*) 
c
c     end of input for one event
c
c
c   write out s-file to automag.out
c
       call rea_event_out(2,all,data,code)
c
c   overwrite input file if flag set, only for first event
c
       if(overwrite.and.nevent.eq.1) then
          rewind 1
          call rea_event_out(1,all,data,code)
       endif
c
c   go to next event in input file if not overwrite, then assume only one event
c
       if(.not.overwrite) goto 20
c
c   end of input data, can be several events
c
        
 1000 continue

      write(6,*)            ! blank line
      write(6,'(a)') ' Results for one run of all input data'
      write(6,'(a,i7)')     ' Total number of spectral fits  ',nfit
      if(nfit.gt.0) then
         write(6,'(a,f7.3)')' Average residuals of fits      ', 
     *   av_res/nfit
      endif
      write(6,'(a,i7)')     ' Total number of calculated mw  ',nmw
      if(nmw.gt.0) then
         call mdian(mw,nmw,med_mw)
         write(6,'(a,2f7.2)')' Average and median mw          ',
     *   av_mw/nmw,med_mw
      endif
      write(6,'(a,i7)')     ' Total number of readings for ml',nml
      write(6,'(a,i7)')     ' Total number of calculated ml  ',nnml
      if(nnml.gt.0) then
         call mdian(ml,nml,med_ml)
         write(6,'(a,2f7.2)')' Average and median ml          ',
     *   av_ml/nnml,med_ml
      endif

      rewind(1)      
c
c   calculate average for grid
c
      if(nfit_st.gt.0) then
         nresult=nresult+1
         av_res_st=av_res_st/nfit_st
         av_mw_st=av_mw_st/nfit_st
         av_st=av_st/nfit_st
         av_f0_st=av_f0_st/nfit_st

         write(result(nresult)
     *   ,'(a,f6.1, a,f6.3,a,f6.3,a,i6,a,f6.3,a,f6.2,a,f6.2,a,f6.1)')
     *   'q0=',q0,' qa=',qalpha,' ka=',kappa,' nf=',
     *    nfit_st,' re=',av_res_st,' mw=',av_mw_st,' f0=',av_f0_st,
     *   ' st=',av_st
          ress(nresult)=av_res_st      

c         write(result(nresult)
c     *   ,'(a,f6.1, a,f6.3,a,f6.3,a,i6,a,f6.3,a,f6.2,a,f6.2,a,f6.1)')
c     *   'q0=',q0,' qa=',qalpha,' ka=',kappa,' nf=',
c     *    nfit,' re=',av_res,' mw=',av_mw,' f0=',av_f0,' st=',av_st
c          ress(nresult)=av_res
       endif 
       

       nfit=0
       nfit_st=0       
       nmw=0
       av_f0=0.0
       av_f0_st=0.0
       av_mw=0.0
       av_mw_st=0.0
       av_res=0.0
       av_res_st=0.0
       av_st=0.0
       

        nml=0
        nnml=0
c
c   end of grid loops    
c
      enddo
      enddo
      enddo

      if(grid) then
c
c   sort the results
c
         call r4sort(nresult,ress,iptr)
         write(6,*) 'Total number of grid searches',nresult

         write(3,'(a)') (result(iptr(i)),i=1,nresult)
      endif

      close(1)

      write(6,*)       
      if(nminsp.gt.0) then
      write(6,'(a,i5,f6.1)')
     *' Number of traces with too small S-P and min S-P for P-spectrum'
     * ,nminsp,minsp
      endif
      write(6,'(a,i5)')' Number of events processed ',nevent
      write(6,'(a)')     ' Output nordic file is automag.out'
      write(6,'(a)')     ' Output of parameters used is automag.par'
      if(grid) write(6,'(a)') 
     *' Output file from grid seaerch is automag_grid.out'

      stop
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine loc_reset_test(ntest,itest,test,file_ind)
c
c     ntest: number of test values to find
c     itest: the number of the test value
c     test:  the test value                            
c                                                                               
c     Routine to return reset test values from station file                       
c
c  jh october 2013
c
c    updates:
c   
      implicit none 
      include 'libsei.inc'	
      integer ntest
      integer itest(*)
      real test(*)			
c-- station file indicator x in STATIONx.HYP
      character*1 file_ind
c-- string with data read from stationfile             
      character*80 string		
c-- station file name
      character*80 stat_file            
c-- unit and error code
      integer unit,code, i,k
			
c
c   reset values
c
      do i=1,ntest
         test(i)=-9999.0
      enddo                                 

      stat_file(1:12)='STATION0.HYP'
      if(file_ind.ne.' ') stat_file(8:8)=file_ind ! check if alternative file
c
c  Search  file in current, then in DAT 
c
           call sei get file( open$+ignore$,   ! Check for  file.
     &                        unit,            ! Unit (n/a).
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                   stat_file )           ! For this filename.

                                                                                
      if (unit.le.0) then
        write(6,'(a,a)') 
     * ' Station file not found:  ' ,stat_file(1:12)
        return
      endif
c
c   reset value
c
      do i=1,ntest
         test(i)=-9999.0
      enddo
c                                                                               
c---- read all test values and compare                                            
c                                                                               

 1    continue        
      read(unit,'(a)',end=999) string
      if(string(1:10).ne.'RESET TEST') goto 999
c
c   check if one of desired tests
c
      read(string(12:13),'(i2)') k
      if(string(15:15).eq.')') read(string(12:14),'(i3)') k
      do i=1,ntest
         if(k.eq.itest(i)) read(string(16:26),'(f10.3)') test(i)
      enddo
      goto 1

 999  continue                                                               

      call sei close(close$,unit,code)
      return
      end                                                                       

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine get wa_filt(flow,polelow,fhigh,polehigh)
c
c   get wa filter from mulplt.def
c
c
      implicit none
      include 'libsei.inc'
      integer polelow,polehigh    ! number of poles in filter
      real flow,fhigh             ! filter frequencies
      real xin                    ! help variable
      character*80 data
      integer unit,code

c
c  Search  file in current, then in DAT 
c
           call sei get file( open$+ignore$,   ! Check for  file.
     &                        unit,            ! Unit (n/a).
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                   'MULPLT.DEF' )        ! For this filename.

      polelow=0
      polehigh=0
      flow=0.0
      fhigh=0.0
                                                                                
      if (unit.le.0) then
        write(6,'(a,a)') 
     * ' MULPLT.DEF file not found, no wa filter used'
        return
      endif
c
c   find if filter
c
 1       continue

         read(unit,'(a)',end=99) data      

         if(data(1:21).eq.'ML HIGH CUT AND POLES'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:60),'(2f10.1)',err=99) fhigh,xin
            polehigh=xin
         endif

         if(data(1:20).eq.'ML LOW CUT AND POLES'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:60),'(2f10.1)',err=99) flow,xin
            polelow=xin
         endif
         
         goto 1
 99      continue
         call sei close(close$,unit,code)
         return
         end
