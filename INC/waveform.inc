c
c  include for handling waveform files
c
c  updates
c
c  april 7 2000 jh.: add several
c  may  11 2000 lo : add wav_resp_file
c  feb  19 2001 lo : common block signal renamed to signalx, since signal 
c                    used in Linux system routines, Redhat 7.0
c  mar  21 2001 lo : add wav_out_stat, wav_out_comp
c  july 13      lo : add wav_resp_filename
c  oct 5        jh : add wav_out_cbyte,wav_out_abs_time,cwav block
c  nov 11          : add wav_sav...
c  may 7 2003   lo : add wav_interactive
c  oct 27 2004  jh : add signal_int
c  nov 28          : add seed variables
c  oct 28 2005  jh : add seed_last_file
c  nov 25 2005  jh : increase number of waveform files from 200 to 1000
c  mar 23 2007  jh : add wav_location, wav_network
c  jun 14 2006  jh : add wav_seed_network and wav_seed_location
c  sep 07 2007  jh : keep data in memeory, MEM variables
c  oct 19 2007  jh : clean out soem mem variables, remove wav_seed location
c                    network etc
c  dec 20 2007  jh:  put cseed in common block
c  jun    2011  jh:  chad seed read, more in wav_seed block
c  oct 23 2011  jh:  add cwav_file_position_start, cwav_file_position_end, 
c                    needed for chad
c  oct 27 2011  jh:  add wav_overlay
c  nov 10 2011  jh:  correct dim for ----
c  jan 15 2012  jh:  wav_chan_position
c  jan 29 2013  jh:  add wav_sav_filename, wav_sav_chan_nr-file,wav_sav_file_nr_chan
c  jun 26 2016  lo:  add wav_dist to be used in mulplt for dist sorting

c
 
      character*80	wav_error_message	   ! error message of operation     
      integer 		wav_nfiles                 ! number of waveform files
      character*80      wav_header_text(max_trace) ! info text in wav file
      character*80      wav_filename(max_trace)    ! waveform file name
      character*10	wav_file_format(max_trace) ! format of wav file
      integer 		wav_year(max_trace)        ! year
      integer           wav_month(max_trace)       ! month
      integer           wav_day(max_trace)         ! day
      integer           wav_hour(max_trace)        ! hour
      integer           wav_min(max_trace)         ! minute
      real              wav_sec(max_trace)         ! second
      double precision  wav_abs_time(max_trace)    ! abs time since 1900
      character*5       wav_stat(max_trace)        ! station
      character*4       wav_comp(max_trace)        ! component
      character*2       wav_location(max_trace)    ! seed location code
      character*2       wav_network(max_trace)     ! seed network code
      integer           wav_nsamp(max_trace)       ! number of samples
      real              wav_rate(max_trace)        ! sample rate
      real              wav_dist(max_trace)        ! distance
      integer           wav_nchan                  ! total number of channels
      real              wav_duration(max_trace)    ! channel duration in secs
      integer           wav_chan_nr_file(max_trace)! channel number in file 
      integer           wav_file_nr_chan(max_trace)! file number for channel
      character*1       wav_cbyte(max_trace)       ! 2 or 4 byte integer
      real		wav_total_time 		   ! total time all channels
      integer           wav_first		   ! index of ealiest trace
      integer           wav_last                   ! --------- last ending --
      logical           wav_overlay  (max_trace)   ! true if a overlay channel
      real              wav_chan_position(max_trace) ! plot position of channel
      integer           wav_current_chan(3)        ! last channel read 1:z, 2:n 3:e
      real 		wav_delay(max_trace)       ! delay each ch. rel. main
      character*80	wav_time_error(max_trace)  ! timing error,char 1 E
                                                   ! rest not defined
      integer           wav_file_position_start(max_trace) ! start position in seed  file of channel
      integer           wav_file_position_end(max_trace) ! end position in seed file of channel
c
c
      real 		signal1(max_sample)        ! all samples from z   trace
      real 		signal2(max_sample)        ! all samples from ns  trace
      real 		signal3(max_sample)        ! all samples from ew  trace
      integer           signal_int(max_sample)     ! one trace in integers
c
      real  		wav_y1(max_sample)         ! work arrays
      real		wav_y2(max_sample)
      real 		wav_y3(max_sample)
cfix   next should be equivalenced to something
c     real		wav_y3comp(max_sample/3,3)
      real		wav_y3comp(max_sample,3)  ! changed lo
c
c   data in memory variables
c
      real 		wav_mem_signal(max_mem_sample) ! all data in one variable
      integer		wav_mem_next_position          ! pointer to next sample to write
      integer           wav_mem_free_header            ! header index to write into
      integer           wav_mem_counter                ! count number of segments
      integer           wav_mem_position(max_trace)    ! position of start of segment
      integer           wav_mem_nsamp(max_trace)       ! number of samples
c      integer           wav_mem_max
c      parameter         (wav_mem_max=1000)
      character*80      wav_mem_filename(max_trace)    ! waveform file name
      integer           wav_mem_chan_number(max_trace) ! channel number

      
c
c   for output selection
c
      integer		wav_out_nchan		   ! number of chan in subset
      real		wav_out_total_time         ! total time all out channels
      character*5       wav_out_stat(max_trace)    ! station
      character*4       wav_out_comp(max_trace)    ! component
      integer		wav_out_chan(max_trace)	   ! select channel numbers
      real		wav_out_start(max_trace)   ! start of trace rel. first
      real		wav_out_duration(max_trace)! duration of selection
      integer		wav_out_nsamp(max_trace)   ! number of samples out
      real		wav_out_rate(max_trace)    ! sample rate out
      integer		wav_out_year(max_trace)    ! year of start
      integer		wav_out_month(max_trace)   ! month of start
      integer		wav_out_day(max_trace)     ! day of start
      integer		wav_out_hour(max_trace)    ! hour of start
      integer		wav_out_min(max_trace)     ! min of start
      real 		wav_out_sec(max_trace)     ! sec of start
      double precision  wav_out_abs_time(max_trace)! abs time
      integer		wav_out_status(max_trace)  ! status of selection
      integer		wav_out_first_sample(max_trace) ! first sample nr sel.
      character*1       wav_out_cbyte(max_trace)   ! 2 or 4 bytes
      character*80      wav_out_header_text(200)   ! info text in wav file
      real              wav_out_delay(max_trace)   ! delay rel main header
      integer           wav_out_first              ! index of first trace
      integer           wav_out_last               ! index of last trace

      integer           wav_nsamp_req(max_trace)   ! number of samples in required time interval

c
c   for saving parameters
c
      integer           wav_sav_nchan              ! number of chan in subset
      real              wav_sav_total_time         ! total time all out channels
      character*5       wav_sav_stat(max_trace)    ! station
      character*4       wav_sav_comp(max_trace)    ! component
      character*2       wav_sav_location(max_trace)! location
      character*2       wav_sav_network(max_trace) ! network code
      integer           wav_sav_chan(max_trace)    ! select channel numbers
      real              wav_sav_start(max_trace)   ! start of trace rel. first
      real              wav_sav_duration(max_trace)! duration of selection
      integer           wav_sav_nsamp(max_trace)   ! number of samples out
      real              wav_sav_rate(max_trace)    ! sample rate out
      integer           wav_sav_year(max_trace)    ! year of start
      integer           wav_sav_month(max_trace)   ! month of start
      integer           wav_sav_day(max_trace)     ! day of start
      integer           wav_sav_hour(max_trace)    ! hour of start
      integer           wav_sav_min(max_trace)     ! min of start
      real              wav_sav_sec(max_trace)     ! sec of start
      double precision  wav_sav_abs_time(max_trace)! abs time
      integer           wav_sav_status(max_trace)  ! status of selection
      integer           wav_sav_first_sample(max_trace) ! first sample nr sel.
      character*1       wav_sav_cbyte(max_trace)   ! 2 or 4 bytes
      character*80      wav_sav_header_text(200)   ! info text in wav file
      real              wav_sav_delay(max_trace)   ! delay rel main header
      integer           wav_sav_first              ! index of first trace
      integer           wav_sav_last               ! index of last trace
      integer           wav_sav_chan_nr_file(max_trace)
      integer           wav_sav_file_nr_chan(max_trace)
      character*80      wav_sav_filename(max_trace)


c
c  response related
c
      character*5       wav_resp_stat              ! station code
      character*4       wav_resp_comp              ! component code
      character*80	wav_resp_status            ! char 1, seisan status 
      character*80      wav_resp_file              ! response file name
      character*80      wav_resp_filename          ! response file name
      character*80      wav_resp_type              ! GSE or SEISAN for type
      character*80      wav_resp_action            ! char 1, seisan action
      character*1040    wav_resp_seisan_chead      ! seisan channel head
      integer		wav_resp_year              ! year of response file
      integer 		wav_resp_month             ! month of response file
      integer		wav_resp_day               ! day of repsponse file
      
c
      character*1	wav_rot_comp(max_trace)    ! indicate if rotated,T or R
      real		wav_rot_delay(max_trace)   ! delay due to rotation
c
c   seed
c
      integer           wav_seed_nchan                  ! # chan in seed file
      integer 		wav_seed_year(max_trace)        ! year
      integer           wav_seed_month(max_trace)       ! month
      integer           wav_seed_day(max_trace)         ! day
      integer           wav_seed_hour(max_trace)        ! hour
      integer           wav_seed_min(max_trace)         ! minute
      real              wav_seed_sec(max_trace)         ! second
      character*5       wav_seed_stat(max_trace)        ! station
      character*3       wav_seed_comp(max_trace)        ! component
      character*2       wav_seed_location(max_trace)    ! seed location code
      character*2       wav_seed_network(max_trace)     ! seed network code
      integer           wav_seed_nsamp(max_trace)       ! number of samples
      real              wav_seed_rate(max_trace)        ! sample rate
      character*80      wav_seed_time_error(max_trace)  ! timing indicator
      integer           wav_seed_file_position_start(max_trace) ! start pos in file
      integer           wav_seed_file_position_end(max_trace)  ! end pos in file

c
c  cwav block
c

      logical           cwav                       ! true if continuous data
      logical           cseed                      ! if true, part of large seed file
      logical           arc                        ! if arc archive


      integer           max_ctrace                 ! max number of traces
c      parameter         (max_ctrace=200) ! increased from 150, lo 04/03/10
      parameter         (max_ctrace=300) ! increased from 150, lo 04/03/10
      integer           max_cseg                   ! maximum number of segments for each trace
      parameter         (max_cseg=1000)
      integer           n_cont_trace               ! number of cont traces

c
      character*80      cwav_filename(max_ctrace,max_cseg)    ! waveform file name
      character*10      cwav_file_format(max_ctrace,max_cseg) ! format of wav file
      double precision  cwav_abs_time(max_ctrace,max_cseg)    ! abs time since 1900
      integer           cwav_nsamp(max_ctrace,max_cseg)       ! number of samples
      integer           cwav_year(max_ctrace,max_cseg)        ! year
      integer           cwav_month(max_ctrace,max_cseg)       ! month
      integer           cwav_day(max_ctrace,max_cseg)         ! day
      integer           cwav_hour(max_ctrace,max_cseg)        ! hour
      integer           cwav_min(max_ctrace,max_cseg)         ! min
      integer           cwav_file_position_start(max_ctrace,max_cseg)  ! start position in seed file of channel
      integer           cwav_file_position_end(max_ctrace,max_cseg)    ! end position in seed file of channel
      real              cwav_sec(max_ctrace,max_cseg)         ! sec
      real              cwav_rate(max_ctrace,max_cseg)        ! sample rate
      real              cwav_duration(max_ctrace,max_cseg)    ! channel duration in secs
      integer           cwav_chan_nr_file(max_ctrace,max_cseg)! channel number in file 
      character*80      cwav_time_error(max_ctrace,max_cseg)  ! timing error
      character*5       cwav_stat(max_ctrace)                 ! station
      character*2       cwav_network(max_ctrace)              ! network code
      character*1       cwav_cbyte(max_ctrace)                ! number of bytes
      character*4       cwav_comp(max_ctrace)                 ! component
      character*2       cwav_location(max_ctrace)             ! location code
      integer           cwav_nseg(max_ctrace)                 ! number of segments
c changed the next three from 14 to 18 to allow f6.3 for seconds
      character*18      cwav_start_time                       ! start time for data set yyyymmddhhmmss
      character*18      cwav_data_start_time                  ! start to find fist s-file
      character*18      cwav_end_time                         ! end time for data set, yyyymmddhhmmss
      double precision  cwav_abs_start_time                   ! abs time cwav_start_time
      double precision  cwav_abs_end_time                     ! abs time for cwav_end_time

      logical wav_interactive                                 ! interactive mode

      common /waveform4/ wav_nfiles,wav_filename,
     *                  wav_year,wav_month,wav_day,wav_hour,wav_min,
     *                  wav_sec,wav_stat,wav_out_stat,wav_sav_stat,
     *                  wav_comp,wav_out_comp,wav_nsamp,wav_rate,
     *                  wav_dist,
     *                  wav_nsamp_req,wav_sav_comp,
     *                  wav_file_nr_chan,wav_nchan,wav_duration,
     *                  wav_chan_nr_file,wav_cbyte,wav_first, 
     *                  wav_last,wav_total_time,wav_out_total_time,
     *                  wav_delay,wav_rot_delay,wav_sav_total_time,
     *                  wav_error_message,wav_header_text,
     *                  wav_current_chan,
     *                  wav_out_nchan,wav_out_chan,wav_out_start,
     *                  wav_out_duration,wav_out_year,wav_out_month,
     *                  wav_out_day,wav_out_hour,wav_out_min,
     *                  wav_out_sec,wav_out_status,
     *                  wav_out_first_sample,wav_out_nsamp,
     *                  wav_out_rate,wav_out_header_text,
     *                  wav_sav_nchan,wav_sav_chan,wav_sav_start,
     *                  wav_sav_duration,wav_sav_year,wav_sav_month,
     *                  wav_sav_day,wav_sav_hour,wav_sav_min,
     *                  wav_sav_sec,wav_sav_status,
     *                  wav_sav_first_sample,wav_sav_nsamp,
     *                  wav_sav_rate,wav_sav_header_text,
     *                  wav_y1,wav_y2,wav_y3,wav_y3comp,wav_time_error,
     *                  wav_resp_comp,wav_resp_file,wav_resp_filename,
     *                  wav_resp_status,wav_resp_action,wav_resp_type,
     *                  wav_resp_year,wav_resp_month,wav_resp_day,
     *                  wav_resp_seisan_chead,wav_out_first,
     *                  wav_sav_first,
     *                  wav_out_last,wav_out_delay,
     *                  wav_sav_last,wav_sav_delay,
     *                  wav_interactive,wav_seed_file_position_start,
     *                  cwav_file_position_start,cwav_file_position_end,
     *                  wav_seed_file_position_end,wav_seed_nchan,
     *                  wav_seed_year,wav_seed_month,wav_seed_day,
     *                  wav_seed_hour,wav_seed_min,
     *                  wav_seed_sec,
     *                  wav_seed_nsamp,wav_seed_rate,
     *                  wav_file_position_start,wav_file_position_end,
     *                  wav_seed_time_error,wav_chan_position,
     *                  wav_sav_filename,wav_sav_chan_nr_file,
     *                  wav_sav_file_nr_chan                  
c
      common /waveform10/ wav_file_format
      common /waveform8/ wav_abs_time, wav_out_abs_time, 
     *                   wav_sav_abs_time
      common /waveform5/ wav_resp_stat,wav_seed_stat
      common /waveform3/ wav_seed_comp
      common /waveform2/ wav_location,wav_network, wav_sav_location,
     *                   wav_sav_network,wav_seed_location,
     *                   wav_seed_network
c
      common /waveform1/ wav_rot_comp,wav_out_cbyte,wav_sav_cbyte,
     *                   wav_overlay
c
      common /signalx/signal1,signal_int
      common /signal1/signal2,signal3
c
c  cwav BLOCK
c

      common /cwav1/    cwav,cseed,cwav_cbyte,arc
      common /cwav4/    cwav_abs_time,
     *                  cwav_rate,
     *                  cwav_duration,
     *                  n_cont_trace,
     *                  cwav_nseg,
     *                  cwav_chan_nr_file,cwav_nsamp,
     *                  cwav_time_error,
     *                  cwav_filename,
     *                  cwav_comp,
     *                  cwav_year,cwav_month,cwav_day,
     *                  cwav_hour,cwav_min,cwav_sec

      common /cwav2/    cwav_location,cwav_network 
      common /cwav5/    cwav_stat
      common /cwav8/    cwav_abs_start_time,cwav_abs_end_time
      common /cwav10/   cwav_file_format

      common /cwav14/   cwav_start_time,cwav_end_time,
     *                  cwav_data_start_time

     
      common /mem4/	wav_mem_signal,wav_mem_next_position,
     *                  wav_mem_counter,wav_mem_position,
     *                  wav_mem_free_header,
     *                  wav_mem_nsamp,wav_mem_filename,
     *                  wav_mem_chan_number
      
