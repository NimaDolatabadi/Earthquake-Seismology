c--------------------------------------------------------------------------
c   program to make obtain amplitudes from specta and signals used
c   to calculate fault plane solutions. based on program automag
c
c   the program can use P on Z, SV on Z and SH on T. it can be used 
c   with both three component and one component data. SV can be turned off
c   with variable s_on_z
c
c   for a station to be processed, it must have both p and s reading
c
c   lars 2015
c
c  updates
c    21 oct 2015 lo: program started as conversion from automag
c    11 jul 2016 jh: phaseout was 9 chars, should be 8, no delete of amplitudes if
c                    same was repicked, now also delete of all amplitudes if new are
c                    picked, showed defaults in input, read also s on vertical, no
c                    write up if no results, can use 1 comp data, some clean up.
c    18 nov 2016 jm: added question whether to read S on Z (def=n)
c    26 dec 2016 jh: input can come as arguments, output file with parameters ueds
c    18 jan 2017 jh: sample mismatch allowed to be 1, fix bug if only 2 components
c    25 jan 2016 jh: weight out amplitudes if the time and spectral amps too different
c    

c
c    Arguments:
c    First argument: Must be file name
c    f:            : Filter, default is 2-4 Hz
c    d:            : Max distance epicentral, default 100 km
c    t:            : Time window, default 2 s
c    g:            : Ground motion, 0: none, 1:displacement, 2: velocity
c    z:            : If z, read S on Z, default no 
c    s:            : 0: Ask if save 1: never save, 2: always save


c

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
      integer recfil_pole                 ! poles for recfil filter
      character*8 filt_name               ! recfil filter type
      complex pole(max_resp_value),zero(max_resp_value)  ! complex PAZ

      logical auto_select                 ! if true , autoselect stations
      integer smodel                      ! which spectral model
      
      integer npole,nzero                 ! number of poles and zeros
      integer pl,ph                       ! dummy for rem resp call
      real norm                           ! normalization constant
      complex y_com(max_sample)           ! spectrum
      real wav_x(max_sample)              ! temp storage of data
      integer seiclen                     ! function
      real minsp                          ! smallest s-p in data set
      integer nminsp                      ! number of s-p smaller than window
      logical sh_on_trans                 ! use transverse component for SH
      logical s_on_z                      ! use S on Z if true
      integer save_to_s                   ! choises for saving s-file


      

      integer nsamp                       ! number of samples

      logical all                         ! true: read all data, false: select
      integer code                        ! error return code
      integer year,month,day,hour,min,doy
      real sec
      double precision s_time,hyp_abs_time(10),p_time,start_time,time
      character*5 stat(1000)
      character*4 comp(1000)
      real dist(1000)
      character*2 co(1000)
      character*1 comp_select          ! component to use
      real amp,per                     ! amplitude and period
      integer rea_nphase_org           ! number of phases in input file
      integer kphase                   ! phase index for current phase
      double precision abstime         ! help variable
      real time_before                 ! time before phase to start analysis
      real duration                    ! duration analysis window
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
      character*1 phase                ! P or S, for autoratio
      character*1 phase_comp           ! z or T, --------------

c

      real av                          ! average
      real fcmin,fcmax,yfirst,ylast    ! frequency range for amplitude and first and last amplitude picked
      real fsmin,fsmax                 ! input spectral frequency range
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
      real ml                          ! ml
      real hdist                       ! hypocentral distance
      real maxdist                     ! max epi dist for calculating amplitudes
      integer c,i,k,l,m,n,j,kq0,kqalpha,kkappa  ! counters
      character*8 phaseout
      integer ipow
      real ampspec(max_sample),ff(max_sample)
      character*9 phx(1000)
      real ga,gb,gc
      real gauss
      real ampsp,ampss,amptp,ampts    ! amplitudes for P and S in 
c                                       time and spectral domain
      real amperr                     ! spectral to time amp-ratio must be beteen 1+- amperr
      integer ground ! 1 for dis, 2 for vel, 3 for acc
      real baz(max_trace)   ! back azimuth
      character*1 choice
      integer nratio        ! number of ratios calcualted
   
     

c
c   set defaults
c

      s_on_z=.false.                  !  normaly do NOT calculate SV on Z
      spec=.true.                     !  spectrum
      wa=.false.
      auto_select=.true.
      overwrite=.false.
      nevent=0
      comp_select='Z'                 ! default use z-component
      maxdist=100.0
      flow=2.0
      fhigh=4.0
      duration=2.0
      ground=0
      minsp=99999.
      nminsp=0
      sh_on_trans=.true.
      save_to_s=0
      nratio=0
      amperr=0.5   ! ca 50 %

c
c   hard wired parameters
c     
c     
      time_before=duration*.2     ! start window 20% before onset (JM)


c
c   get seisan parameters, here attenuation parameters
c
      call get_seisan_def           
c
c   get arguments
c
      call get_arguments(nars,arg)
c
c   if only one argument, it is the file name, must be first
c
      if(nars.gt.0) then
        infile=arg(1)
        write(6,'(a,a)')' Input S-file  ',arg(1)
        open(1,file=infile,status='old',err=5)
        goto 6
 5      continue
        write(6,*)'File does not exist'
        stop
 6      continue
      endiF
c
c   check if more arguments
c
      if (nars.gt.1) then
         save_to_s=2    ! assume write out if more arguments
         do i=2,nars
c   filter
           if(arg(i)(1:1).eq.'f') then
              read(arg(i+1),*) flow
              read(arg(i+2),*) fhigh
           endif
c    time window
           if(arg(i)(1:1).eq.'t') then
              read(arg(i+1),*) duration
           endif 
c    max distance
           if(arg(i)(1:1).eq.'d') then
              read(arg(i+1),*) maxdist
           endif 
c    ground motion
           if(arg(i)(1:1).eq.'g') then
              read(arg(i+1),*) ground
           endif  
c    component
           if(arg(i)(1:1).eq.'z') then
              s_on_z=.true.
           endif 
c    save to s-file
           if(arg(i)(1:1).eq.'s') then
              read(arg(i+1),*) save_to_s
           endif 
        enddo
      endif

c
c   if more then 1 argument, it is assumed that all input from argument
c   and/or defaults used


      if (nars.gt.1) goto 7

c
c   set filter, hardwired to 2-4 Hz and 4 pole for now
c
      write(*,*) ' filter frequencies (f1,f2), def=(2,4) '
      read(5,'(a)') text
      if (seiclen(text).gt.0) then
        read(text,*) flow,fhigh
      else
        flow=2.
        fhigh=4.
      endif

      pole_low=4
      pole_high=4


      write(*,*) ' time window for analysis, enter for default 2 s'
      read(5,'(a)') text
      if(text.ne.' ') then
         read(text,*) duration
         time_before=duration*.2     ! start window 20% before onset (JM) 
      endif


      write(*,*) ' max distance in km (def=99) '
      read(5,'(a)') text
      if (seiclen(text).gt.0) then
        read(text,*) maxdist
      else
        maxdist=99.
      endif
      fsmin=flow*.5    ! lowest frequency for spectral fit
      fsmax=fhigh*2.   ! lowest frequency for spectral fit
      ground=0
      write(*,*) 
     *' ground motion (0=none (enter), 1=displacement, 2=velocity)'
      read(5,'(a)') text
      if (seiclen(text).gt.0) then
        read(text,'(i1)') ground
      else
        ground=0
      endif

      write(6,*)' Max deviation in time and spec amp ratios, def 50%'
      read(5,'(a)') text
      if(text.ne.' ') then
          read(text,*) amperr
          amperr=amperr/100.0
      endif

      write(*,*) ' read S amplitudes on Z component (y/n, def=n)?'
      read(5,'(a)') text
      if(text.eq.'y') then
         s_on_z=.true.
      endif      

 7    continue     ! get here if all def which is more then 1 arguments
c
c   open output file s-file
c
      open(3,file='autoratio.out',status='unknown')
      write(3,'(a)') ' STAT      ATPG     ATSG     ASPG     ASSG'//
     &               '   StoPt   StoPs'
      write(3,'(a)') ' ----------------------------------------------'//
     &               '------------'
c
c   open list of all components processed etc, use with plotting program
c
      open(55,file='plotratio.inp',status='unknown')
c
c   get input sfile name, check if exist
c
      if(nars.eq.0) then
 9       continue
         write(6,*) 'Give input S-file '
         read(5,'(a)') infile
         if (seiclen(infile).le.0) stop
         open(1,file=infile,status='old',err=10)

         goto 11
 10      continue
         write(6,*)' No such input file'
         goto 9
 11      continue
      endif
c
c    write out parameters used
c
      open(27,file='autoratio.par',status='unknown')
      write(27,*) 'File used ', infile
      write(27,*) 'Filter',flow,fhigh
      write(27,*) 'Time window', duration
      write(27,*) 'Max distance',maxdist
      if(ground.eq.0) write(27,*) 'Uncorrected'
      if(ground.eq.1) write(27,*) 'Displacement'
      if(ground.eq.2) write(27,*) 'Velocity'

      if(s_on_z) then
c          write(27,*) 'Use S on Z'
      else
c          write(27,*) 'Do not use S on Z'
      endif



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc       loop of events
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      nevent=0
 20   continue   ! here from end for next event
      call wav_mem_init
c
c   read all parameters for one event from file unit 1
c
      all=.true.        ! read all parameters from s-file
      call rea_event_in(1,all,data,code)
      close(1)
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
c  initialize wav reading
c
      call wav_init
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
c   terminate if no waveform files
c
      if(wav_nfiles.eq.0) then
         write(6,*)'No waveform files this event, got to next event'
         goto 20
      endif
c
c
c   print how many files were found
c
      wav_nfiles=npresent
      write(6,*)' Number of wav-files present', wav_nfiles

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

c
c   terminate if no channels
c
      if(wav_nchan.eq.0) then
        write(6,*)'No channels found, go to next event'
        goto 20
      endif
      write(6,*)' Total number of channels available:',wav_nchan
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
      call get_baz
     *    (wav_nchan,wav_stat,data,rea_nhead,rea_nrecord,baz)
      if(auto_select) then
         k=0
         do i=1,rea_nphase
          if((rea_phase(i)(1:1).eq.'P'.or.rea_phase(i)(1:1).eq.'S').and.
     &       rea_phase(i)(1:4).ne.'SPEC')then
               k=k+1
               stat(k)=rea_stat(i)
               co(k)=rea_co(i)
               dist(k)=rea_dist(i)
c           write(*,*) ' debug ',rea_dist(i)
               phx(k)=rea_phase(i)
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
               dist(m)=dist(i)
               phx(m)=phx(i)
            endif
         enddo

c
c  find which of these stations have comp_select in wav file and 
c  select only those, make sure same type of channel (e.g. S, B) 
c  as used to read phase
c
         k=0 
         do i=1,m
           do j=1,wav_nchan
              if(stat(i).eq.wav_stat(j).and.
     &        wav_comp(j)(4:4).eq.comp_select.and.
     *        wav_comp(j)(1:1).eq.co(i)(1:1)) then
c         write(*,*) ' debug ',
c     &  wav_stat(j),wav_comp(j),dist(i),phx(i)
                if(dist(i).gt.0..and.dist(i).le.maxdist) then
                  k=k+1
                  stat(k)=stat(i)
                  comp(k)=wav_comp(j)
                  dist(k)=dist(i)
                  phx(k)=phx(i)
                else
                  write(6,*)'Station too far, skip',stat(i),dist(i)
                endif
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

       phase='P'      ! start with P, later loop over P and S
       phase_comp='Z' ! start with Z
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
         if(rea_phase(i)(1:1).eq.'P'.and.rea_stat(i).eq.stat(m)) then
           p_time=rea_abs_time(i)  ! abs p-time
c
c   save phase index to know distance
c
           kphase=i
           write(6,'(a,a)')' P time on component ',rea_co(i)
           call sectim(p_time, year,doy,month,day,hour,min,sec)
c           write(6,'(a,i4,1x,2i2,1x,2i2,f6.1)') ' Time of P ',
c     *     year,month,day,hour,min,sec
           goto 222   ! jump to S-phase
         endif
       enddo
       write(6,'(a,a)')' No P-phase for station ',stat(m)
 222   continue
c
c  look for s-phase
c
       s_time=0.
       do i=1,rea_nphase
        if(rea_phase(i)(1:1).eq.'S'.and.rea_stat(i).eq.stat(m).and.
     &     rea_phase(i)(1:4).ne.'SPEC') then
           s_time=rea_abs_time(i)  ! abs s-time
c
c   save phase index
c
           kphase=i
           write(6,'(a,a)')' S time on component ',rea_co(i)
           goto 223   ! jump to S-phase
        endif
       enddo
 223   continue
c
c   check distance 
c
       if (p_time.eq.0..or.s_time.eq.0.) then
         write(6,'(a,a)')' No P or S-phase found for station, skip ',
     * stat(m)
         goto 500   ! require P and S
       endif

c---------------------
c   a phase found
c---------------------

c
c  find channel
c
       call wav_find_chan(stat(m),comp(m),k)
       if(wav_error_message.ne.' ')write(6,*) wav_error_message
 
       if(k.eq.0) then
         write(6,'(a,a5,1x,a4)') 'Channel not found: ',stat(m),comp(m)
         goto 500   ! try next
       else
         write(*,*) ' Channel found: ',k,stat(m),comp(m)
       endif

c
c   find if one or 3 component
c
        wav_out_nchan=3
        call wav_read_3channel(k)
        if(wav_error_message(1:30).eq.
     *  'All 3 components not available') then
           write(6,*) 'One or two component data, use Z only, if there'
           wav_out_nchan=1
        endif

c
c   read whole channel
c

       if(wav_out_nchan.eq.1) then
          call wav_read_channel(k)
       else
c
c read 3 channels
c
        call wav_read_3channel(k)
       endif
c
c   select out time window, only for one channel
c

c
        wav_out_rate(k)=wav_rate(k)
c
c   look for max amp in a duration window
c

c
c loop over  channels, filter and correct response before rotation
c could be just z-channel
c
c
        do i=1,wav_out_nchan   ! Z,N,E
          if (i.eq.1) then
            comp(m)(4:4)='Z'
            wav_out_chan(1)=wav_current_chan(1)
          elseif (i.eq.2) then
            comp(m)(4:4)='N'
            wav_out_chan(2)=wav_current_chan(2)
          elseif (i.eq.3) then
            comp(m)(4:4)='E'
            wav_out_chan(3)=wav_current_chan(3)
          endif
c          write(*,*) wav_out_chan(i)
c
c  specify channel for response 
c
          wav_resp_stat=stat(m)
          wav_resp_comp=comp(m)
          wav_resp_year=wav_out_year(k)
          wav_resp_month=wav_out_month(k)
          wav_resp_day=wav_out_day(k)
          nsamp=wav_nsamp(k)
c
c   read response file
c
          call read_resp

          if(ground.ne.0.and.wav_resp_status(1:1).ne.' ') then
            write(6,'(a,a,a)') 'No response for ', 
     *      wav_resp_stat,wav_resp_comp
            write(6,*) 'Try next station'
            goto 500
          endif

c set wav_x to Z,N,E

          if (i.eq.1) then
            do j=1,nsamp
               wav_x(j)=signal1(j)
            enddo
          elseif (i.eq.2) then
            do j=1,nsamp
               wav_x(j)=signal2(j)
            enddo
          elseif (i.eq.3) then
            do j=1,nsamp
               wav_x(j)=signal3(j)
            enddo
          endif
c
c   apply a taper
c  
c remove dc

          call applytaper(wav_x,nsamp,10.)   ! apply taper, width 10% of half the samples

c
c   do filtering
c
          if(flow.ne.0.and.fhigh.ne.0) then
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
c   use recfil to filter
c
c            write(*,*) ' debug ',flow,fhigh,wav_out_rate(k)
            call recfil(wav_x,nsamp,wav_x,'BU      ',
     *     0.0,0.0,recfil_pole,
     *     filt_name,flow,fhigh,1.0/wav_out_rate(k),1)  
         endif               
c
c   correct for instument, no filtering, ground=1 for displ and
c   ground=2 for vel
c
         nzero=0
         npole=0
         if (ground.ge.1.and.ground.le.2)
     &   call remove_resp(wav_x,y_com,nsamp,wav_out_rate(k),ground,
     *     0.,0.,pl,ph,
     *     zero,pole,nzero,npole,norm) 
         if (i.eq.1) then
           do j=1,nsamp
              signal1(j)=wav_x(j)
c              write(85,*) j,signal1(j)
           enddo
         elseif (i.eq.2) then
           do j=1,nsamp
              signal2(j)=wav_x(j)
c              write(86,*) j,signal2(j)
           enddo
         elseif (i.eq.3) then
           do j=1,nsamp
              signal3(j)=wav_x(j)
c              write(87,*) j,signal3(j)
           enddo
         endif
       enddo    ! channel(s) corrected

 100   continue
c
c   find if data is available
c
c        write(*,*) ' debug ',wav_out_nchan
c        do i=1,3
c          write(*,*) ' debug chan ',i,wav_out_chan(i)
c        enddo


        do i=1,wav_out_nchan
          k=wav_out_chan(i)
c
c set time first to P then to S
c
          if (phase.eq.'P') then 
            time=p_time
          else
            time=s_time
          endif
c
c  start time relative to start of particular channel
c
           wav_out_start(k)=
     &       time-wav_abs_time(wav_first)-time_before

           wav_out_duration(k)=duration + time_before   !*2.
c check that P does not include S
          if (phase.eq.'P') then 
           if (wav_abs_time(wav_first)+wav_out_start(k)+
     &       wav_out_duration(k).ge.s_time) then
            wav_out_duration(k)=s_time-
     &        (wav_abs_time(wav_first)+wav_out_start(k))   ! limit to start of S
              write(*,*) '    limited P window to start of S '
           endif
          endif
c          write(*,*) ' debug ',i,time,wav_out_start(k)
       enddo

        call wav_get_interval
c
c   skip if not enough data
c
         do i=1,wav_out_nchan
          k=wav_out_chan(i)
          if(wav_out_status(k).ne.4) then
            write(6,*)' Not enough data, skip channel'
            write(*,*) wav_abs_time(wav_first),wav_out_start(k)
            write(*,*) wav_abs_time(k),wav_duration(k)
            write(*,*) k,wav_out_status(k)
            goto 400
          endif
        enddo
c
c   if here, processing possible, put in list file
c
c
c   write in list file even if it might not be used
c         
        write(55,'(a,a,a)') data(1)(1:79),
     *  '   Phase ',phase

        amp=0.0
        per=0.0 
         
c
c   move data interval to beginning of arrays, wav y1 used for processing
c

c---------------------------------------------
c   case of P on Z-channel
c
        if (phase.eq.'P') then 
          k=wav_out_chan(1)
          l=1
          do i=wav_out_first_sample(k),
     *      wav_out_first_sample(k)+wav_out_nsamp(k)-1
            wav_y1(l)=signal1(i)
            l=l+1
          enddo 
        endif

c---------------------------------------------
c   case of S on Z channel
c
        if (phase.eq.'S'.and.phase_comp.eq.'Z') then 
          k=wav_out_chan(1)
          l=1
          do i=wav_out_first_sample(k),
     *      wav_out_first_sample(k)+wav_out_nsamp(k)-1
            wav_y1(l)=signal1(i)
            l=l+1
          enddo 
         endif

c-----------------------------------------------------
c   case of S on T-channel

       if (phase.eq.'S'.and.phase_comp.eq.'T'.and.wav_out_nchan.eq.3) 
     * then  
c
c cut horizontal data before rotation
c
          k=wav_out_chan(2)
          nsamp=wav_out_nsamp(k)
          l=1
          do i=wav_out_first_sample(k),
     *      wav_out_first_sample(k)+wav_out_nsamp(k)-1
            wav_y2(l)=signal2(i)
c            write(88,*) l,wav_y2(l)
            l=l+1
          enddo 
          k=wav_out_chan(3)
c          if (wav_out_nsamp(k).ne.nsamp) then
           if (abs(wav_out_nsamp(k)-nsamp).gt.1) then
            write(*,*) ' !! mismatch horizontal sample number, skip !!'
            write(*,*) '    w_out=',wav_out_nsamp(k),' , samp=',nsamp
            write(*,*) '    Try different time window.'
              amptp=0.
              ampsp=0.
              ampts=0.
              ampss=0.  
            goto 400
          endif
          l=1
          do i=wav_out_first_sample(k),
     *      wav_out_first_sample(k)+wav_out_nsamp(k)-1
            wav_y3(l)=signal3(i)
c            write(89,*) l,wav_y3(l)
            l=l+1
          enddo
c
c rotate data if 3 channels
c
          if(wav_out_nchan.eq.3) then
             write(*,*) ' rotating, baz= ',baz(k)

             call rotate_comp(nsamp,1,1,'T',baz(k),wav_y1,wav_y2,wav_y3)
          endif
c          do j=1,nsamp
c            write(90,*) j,wav_y1(j)
c          enddo
        endif

c
c  set auto amp parameters
c
       fcmin=0.5
       fcmax=10.0
       maxcros=1
c
c   autopick amplitude
c
       call auto_amp2
     *(wav_y1,wav_out_nsamp(k),wav_out_rate(k),fcmin,fcmax,maxcros,
     *yfirst,ylast,ifirst,ilast)
c
c   set component to use
c
             if (phase.eq.'P') then
               comp(m)(4:4)='Z'
             endif

             if (phase.eq.'S'.and.phase_comp.eq.'Z') then             
               comp(m)(4:4)='Z'
             endif


            if (phase.eq.'S'.and.phase_comp.eq.'T') then             
               comp(m)(4:4)='T'
             endif


             text=stat(m)(1:seiclen(stat(m)))//
     &           '.'//comp(m)//'.td.'//phase
             do i=1,seiclen(text)
               if(text(i:i).eq.' ') text(i:i)='_'
             enddo
             open(33,file=text(1:seiclen(text)),status='unknown')
       if (phase.eq.'P') then 
         amptp=0.
         ampsp=0.
       endif
       if (phase.eq.'S') then 
         ampts=0.
         ampss=0.
       endif
       write(55,'(a,a,i5)') stat(m),comp(m),int(dist(m)+.5)
       if(ifirst.eq.0.or.abs(yfirst-ylast).gt.90000000) then
         write(6,*)'No valid amplitude reading'   
         write(55,*)' '   ! signal no ml
         write(33,*)
c
c   calculate amplitude and period
c
       else
          amp=abs(yfirst-ylast)/2.0   ! first and last sample number for 2 extreemes
          if (phase.eq.'P') amptp=amp
          if (phase.eq.'S') ampts=amp
          per=2.0*abs(ilast-ifirst)/wav_out_rate(k)
c
c   save trace in file
c
             write(33,*)wav_out_nsamp(k),wav_out_rate(k),ifirst,ilast
             do i=1,wav_out_nsamp(k)
               write(33,*) wav_y1(i)
             enddo
c
c   calculate time of pick
c
             abstime=time+ifirst/wav_out_rate(k)-time_before
             call sectim(abstime,
     *       year,doy,month,day,hour,min,sec)
             write(6,'(a,1x,i4,1x,2i2,1x,2i2,f6.1)')
     *       ' Time of amplitude ',year,month,day,hour,min,sec
             write(6,'(a, 2f11.2)') 
     *       ' Amplitude(nm or count) and period(sec)',
     *       amp,per
c
c   save in data array
c
             rea_nphase=rea_nphase+1
             call rea_phase_clear(rea_nphase)
             rea_stat(rea_nphase)=stat(m)
             rea_comp(rea_nphase)=comp(m)

             if (phase.eq.'P') then
               phaseout='ATPG    '      
               comp(m)(4:4)='Z'
             endif

             if (phase.eq.'S'.and.phase_comp.eq.'Z') then
               phaseout='ATSG    '      
               comp(m)(4:4)='Z'
             endif

             if (phase.eq.'S'.and.phase_comp.eq.'T') then
               phaseout='ATSG    '      
               comp(m)(4:4)='T'
             endif


             call component(comp(m),rea_co(rea_nphase))
             rea_phase(rea_nphase)=phaseout
             rea_year(rea_nphase)=year
             rea_month(rea_nphase)=month
             rea_day(rea_nphase)=day
             rea_hour(rea_nphase)=hour
             rea_min(rea_nphase)=min
             rea_sec(rea_nphase)=sec     
             rea_amp(rea_nphase)=amp
             rea_per(rea_nphase)=per      
             write(55,'(a,a1,a,f10.1,a,f4.2)')
     &           'AMP=',phase,' A=',amp,' T=',per
c
c   check if phase was there from before, then delete
c
             do l=1,rea_nphase_org
               if(rea_stat(l).eq.stat(m).and.rea_co(l).
     *         eq.rea_co(rea_nphase)
     *         .and.rea_phase(l)(1:4).eq.phaseout(1:4)) then
                  rea_phase(l)(1:6)='DELETE'
               endif
             enddo

c
c   delete all old amp phases, new jul 11, 2016, jh
c
             do l=1,rea_nphase_org
                if(rea_phase(l)(1:2).eq.'AT'.or.
     *             rea_phase(l)(1:2).eq.'AS') 
     *             rea_phase(l)(1:8)='DELETE  '
             enddo


       endif       ! for valid amp reading
       close (33)
c
c------------------------------------------------------------------------------
c   spectral analysis
c------------------------------------------------------------------------------
c
 400     continue   ! here if a channel was skipped due to too litle data for 
c                     in time domain

         if(spec) then
            write(6,*)
            write(6,'(a,a,a)')' *** computing spectrum for ',
     *      phase, '***'

            
c            phase=spectrum_type


c
c  chose data 
c
c

c
c      P on z
c
            if (phase.eq.'P') then 
              k=wav_out_chan(1)
            endif
c
c   s on z
c

            if(phase.eq.'S'.and.phase_comp.eq.'Z') then
              k=wav_out_chan(1)
            endif
c
c   s on T
c
            if(phase.eq.'S'.and.phase_comp.eq.'T') then
              k=wav_out_chan(2)
            endif
 
c
c   check and set high frequency limit
c
            if(fsmax.gt.wav_out_rate(k)/2.5) then
               write(6,*)' max frequecy too high, reset to ',
     *         wav_out_rate(k)/2.5
               fsmax=wav_out_rate(k)/2.5
            endif
            if(fsmax.eq.0.0) fsmax=wav_out_rate(k)/2.5



c 
c compute spectrum for signal stored in y1 with wav_out_nsamp(k) samples
c
             ipow = 1
             do l = 1,20
                 ipow = ipow*2
              if(ipow .ge. wav_out_nsamp(k)) go to 350
             enddo
350          continue              
c             write(*,*) ' debug ',ipow
             do l=1,wav_out_nsamp(k)
               y_com(l)=wav_y1(l)*(1,0.)
             enddo
             do l=wav_out_nsamp(k),ipow
               y_com(l)=(0.,0.)
             enddo
            
             call spectrum(y_com,ipow,1,0,wav_out_rate(k),
     &         0.,0.,0,0,0.,0.,
     &         0.,0.,pole,zero,0,0)
c
c get amplitude spectrum and frequencies
c
             c = 0
             do i = 2,((ipow/2) + 1)
               c = c + 1
               ff(c) = (i-1)*wav_out_rate(k)/ipow
               ampspec(c) = (1/wav_out_rate(k)**2)*
     &            y_com(i)*conjg(y_com(i))
               ampspec(c) = sqrt(ampspec(c))
             enddo
             call fitspec(ampspec,ff,c,ga,gb,gc)
             write(*,*) ' spectral fit ',ga,gb,gc
             if (phase.eq.'P') ampsp=ga
             if (phase.eq.'S') ampss=ga
c
c   save spectrum in file
c
             text=stat(m)(1:seiclen(stat(m)))//
     &           '.'//comp(m)//'.sp.'//phase
             do i=1,seiclen(text)
               if(text(i:i).eq.' ') text(i:i)='_'
             enddo
             open(33,file=text(1:seiclen(text)),status='unknown')
c             write(33,*)wav_out_nsamp(k),wav_out_rate(k),ifirst,ilast
             do i=1,c
               if (ff(i).ge.fsmin.and.ff(i).le.fsmax) then
                 write(33,*) ff(i),ampspec(i)
               endif
             enddo
             close (33)
c
c save gaussian fit
c
             text=stat(m)(1:seiclen(stat(m)))//
     &           '.'//comp(m)//'.ga.'//phase
             do i=1,seiclen(text)
               if(text(i:i).eq.' ') text(i:i)='_'
             enddo
             open(33,file=text(1:seiclen(text)),status='unknown')
             do i=1,c
               if (ff(i).ge.fsmin.and.ff(i).le.fsmax) then
                 write(33,*) ff(i),gauss(ff(i),ga,gb,gc)
               endif
             enddo
             close (33)


c
c   first write to list file
c
             write(55,'(a,f5.2,a,f5.2)') 'f=',flow,'-',fhigh
             write(55,'(a,f9.1,a,f5.2)') 'spec=',ga,' f= ',gb

c
c   count number of ok fits
c
              rea_nphase=rea_nphase+1
              call rea_phase_clear(rea_nphase)
              k=rea_nphase

c
c   calculate time of start of window
c
              if(phase.eq.'S') then
                 abstime=s_time-time_before
                 phaseout='ASSG    '
                 if (amptp.gt.0..and.ampts.gt.0..and.
     &               ampsp.gt.0..and.ampss.gt.0.) then
                 write(55,'(a,f5.2,a,f5.2)') 
     &             'td_StoP=',ampts/amptp,' sd_StoP=',ampss/ampsp
                 else
                   write(55,'(a)') 'ratios=???'
                 endif
                    comp(m)(4:4)=phase_comp
              else
                 abstime=p_time-time_before
                 phaseout='ASPG    '
                 comp(m)(4:4)='Z'
                 write(55,'(a)') ' '
              endif

              if (phase.eq.'S') then
                 write(3,'(1x,a5,a1,f8.1,1x,f8.1,1x,
     &           f8.1,1x,f8.1,1x,f7.1,1x,f7.1)')
     &           stat(m),phase_comp,amptp,ampts,ampsp,
     *           ampss,ampts/amptp,ampss/ampsp
c
c   count writeout
c
                 nratio=nratio+1
              endif

              call component(comp(m),rea_co(k))
              call sectim(abstime, 
     *        year,doy,month,day,hour,min,sec)
              rea_hour(k)=hour
              rea_min(k)=min
              rea_sec(k)=sec    

              rea_stat(k)=stat(m)
              
              rea_phase(k)=' '
              rea_phase(k)=phaseout
              rea_amp(k)=ga
              rea_per(k)=1./gb

c
c   weight out amplitudes if too much difference between
c   spectral and time amplitudes
c
              if(((ampts/amptp)/(ampss/ampsp)).gt.1.0+amperr.or.
     *        ((ampts/amptp)/(ampss/ampsp)).lt.1.0-amperr)  then
              do l=1,k
                 if(rea_stat(l).eq.rea_stat(k)
     *           .and.(rea_phase(l)(1:4).eq.'ATPG'.or.
     *           rea_phase(l)(1:4).eq.'ATSG'.or.
     *           rea_phase(l)(1:4).eq.'ASPG'.or.
     *           rea_phase(l)(1:4).eq.'ASSG')) then
                    rea_weight_in(l)='4'
                 endif
             enddo
             endif
c
c   check if SPEC phase was there from before, then delete
c
             do l=1,rea_nphase_org
                if(rea_stat(l).eq.rea_stat(k).and.rea_comp(l).
     *          eq.rea_comp(k)
     *          .and.rea_phase(l)(1:5).eq.phaseout(1:5)) then
                   rea_phase(l)(1:6)='DELETE'
                 endif
             enddo
c
c   delete all old amp phases, new jul 11, 2016, jh
c
             do l=1,rea_nphase_org
                if(rea_phase(l)(1:2).eq.'AT'.or.
     *             rea_phase(l)(1:2).eq.'AS') 
     *             rea_phase(l)(1:8)='DELETE  '
             enddo
         endif
c
c jump to do S
c

c   S on T

         if(wav_out_nchan.eq.1) goto 500  ! one channel only, no T
c
c     S on Z if not skip
c
         if (phase.eq.'P') then
           phase='S'
           if(.not.s_on_z) phase_comp='T'    ! skip s on z if  not flag
           goto 100
         endif

c   S on T

         if(phase.eq.'S'.and. phase_comp.eq.'Z') then
            phase_comp='T'
            goto 100
         endif

c         else 
c           goto 600   ! xxx leave after first channel for testing
c         endif
 
c
c  end of channel loop
c 
 500     continue   ! get here if a channel was skipped due to missing info
       enddo
 600   continue

c
c plot results to screen
c
      write(6,*) 
      close(3)
      open(3,file='autoratio.out',status='old')
 610  continue
      read(3,'(a)',end=620) text
      write(*,*) text(1:seiclen(text))
      goto 610
 620  continue

c
c     end of input for one event
c
c
c   write out s-file
c
      if(nratio.eq.0) then
         write(6,*) ' No results'
      else
         if(save_to_s.eq.0) then   ! ask
            write(6,'(a)') ' Save results to sfile (y/n=enter)?'
            read(5,'(a)') choice
         endif
         if(save_to_s.eq.1) choice='n'   ! do not save in any case
         if(save_to_s.eq.2) choice='y'   ! always save
         if (choice.eq.'y') then


            open(2,file=infile,status='unknown')
            call rea_event_out(2,all,data,code)
            close(2)
         endif
      endif


c
c   end of input data, can be several events
c
        
 1000 continue

c      rewind(1)      
c
c   calculate average for grid
c

      write(6,*)       
      if(nminsp.gt.0) then
      write(6,'(a,i5,f6.1)')
     *' Number of traces with too small S-P and min S-P for P-spectrum'
     * ,nminsp,minsp
      endif
      write(6,'(a,i5)')' Number of events processed ',nevent
      write(6,'(a)')     ' Output nordic file is autoratio.nor'
      write(6,'(a)')     ' Output file with ratios is autoratio.out'
      write(6,'(a)')     ' Input for plotratio is plotratio.inp.'
      write(6,'(a)')     ' Parameters used in autoratio.par'
      stop
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    subroutines
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine fitspec(x,ff,n,ga,gb,gc)
      implicit none
      real x(*),ff(*)   ! spectral amplitude and frequency
      integer n         ! number of data points
      real ga,gb,gc     ! parameters in Gaussian curve
      integer na,nb,nc
      real xa,xb,xc
      real fit,min
      real gauss
      integer i,j,k,l   ! counters for loop over three parameters
      real maxa,maxb,maxc

      na=10
      nb=10
      nc=10
      
      maxb=10.
      maxc=10.
      maxa=0.
      do i=1,n
        if (x(i).gt.maxa) maxa=x(i)
      enddo
      xa=0.
            min=-1.
      do i=1,na
        xa=xa+maxa/na
        xb=0.
        do j=1,nb
          xb=xb+maxb/nb
          xc=0.
          do k=1,nb
            xc=xc+maxc/nc
c
c check fit
c 
      fit=0.
            do l=1,n
              fit=fit+(x(l)-gauss(ff(l),xa,xb,xc))**2
            enddo
c jh            write(12,*) xa,xb,xc,fit
            if (fit.lt.min.or.min.lt.0.) then
                min=fit
                ga=xa
                gb=xb
                gc=xc
            endif
          enddo
        enddo
      enddo

      return
      end

      real function gauss(x,a,b,c)
      implicit none
      real y,x,a,b,c
      y=a*exp(-(x-b)*(x-b)/2/c/c)
      gauss=y
      return
      end
      
