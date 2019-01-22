c
c  include for handling continuous data
c
c  updates
c  oct 2001     jh: several
c

c     integer max_ctrace             ! max number of traces
c     parameter (max_ctrace=50)
c     integer max_cseg               ! maximum number of segments for each trace
c     parameter (max_cseg=300)

      integer n_cont_trace                                    ! number of cont traces
 
c
c header info
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
      real              cwav_sec(max_ctrace,max_cseg)         ! sec
      real              cwav_rate(max_ctrace,max_cseg)        ! sample rate
      real              cwav_duration(max_ctrace,max_cseg)    ! channel duration in secs
      integer           cwav_chan_nr_file(max_ctrace,max_cseg)! channel number in file 
      character*80      cwav_time_error(max_ctrace,max_cseg)  ! timing error
      character*5       cwav_stat(max_ctrace)                 ! station
      character*1       cwav_cbyte(max_ctrace)                ! number of bytes
      character*4       cwav_comp(max_ctrace)                 ! component
      integer           cwav_nseg(max_ctrace)                 ! number of segments
c
c canvas size
c
      character*14 cwav_start_time     ! start time for data set yyyymmddhhmmss
      character*14 cwav_data_start_time! start to find fist s-file
      character*14 cwav_end_time       ! end time for data set, yyyymmddhhmmss
      double precision cwav_abs_start_time   ! abs time cwav_start_time
      double precision cwav_abs_end_time     ! abs time for cwav_end_time


      common /cont1/
     *                 cwav_cbyte
      common /cont4/    
     *                  cwav_abs_time,
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

      common /cont5/    cwav_stat
      common /cont8/    cwav_abs_start_time,cwav_abs_end_time
      common /cont10/   cwav_file_format

      common /cont14/   cwav_start_time,cwav_end_time,
     *                  cwav_data_start_time


