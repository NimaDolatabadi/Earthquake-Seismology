ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c seed.inc.f
c Summary of seed file.
c
c Author: Rodrigo Canabrava
c E-mail: rlpcfr@yahoo.com.br
c Universitetet i Bergen - Department of Earth Sciences
c
c   changes
c
c  nov 9, 2005 jh : add variabel seed_blk_read
c  oct 6  2006 jh : add seed_network
c  oct 21 2007 jh : add chn_network
c  nov 9  2007 jh : add seed_chan_time
c  sep 25 2007 jh : increase max_n_block from 2000 to 9000
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER*4 MAX_N_CHN       ! Maximum number of channels
      INTEGER*4 MAX_N_BLK       ! Maximum number of blocks read at a time
      PARAMETER (MAX_N_CHN = 1000)
      PARAMETER (MAX_N_BLK = 9000)

      INTEGER*4 STDIN           ! Standard input number
      INTEGER*4 STDOUT          ! Standard output number

      PARAMETER (STDIN = 95)
      PARAMETER (STDOUT = 6)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c File Summary
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer*4   nb_channels   ! number of channels in file
      character*3 chn_name(MAX_N_CHN) ! names of channels
      character*5 stn_name(MAX_N_CHN) ! names of stations of each channel
      character*2 chn_location(MAX_N_CHN) ! location id
      character*2 chn_network(MAX_N_CHN) ! network
      integer*4   chn_begin(MAX_N_CHN) ! first block of each channel
      integer*4   chn_end(MAX_N_CHN) ! last block of each channel
      integer*4   chn_samples(MAX_N_CHN) ! number of samples of each channel
      real*4      chn_sample_rate(MAX_N_CHN) ! sample rate of each channel
      integer*2   chn_year(MAX_N_CHN) ! start year of each channel
      integer*2   chn_day(MAX_N_CHN) ! start day
      integer*2   chn_hour(MAX_N_CHN) ! start hour
      integer*2   chn_minute(MAX_N_CHN) ! start minute
      integer*2   chn_second(MAX_N_CHN) ! start second
      integer*2   chn_fracsec(MAX_N_CHN) ! start fraction of seconds
      logical*1   chn_questionable_time_tag(MAX_N_CHN) !flag if time tag is bad

      common / file_summary / stn_name, chn_name, chn_begin, chn_end, 
     & chn_samples, chn_year, chn_day, chn_hour, chn_minute, 
     & chn_second, chn_fracsec, chn_sample_rate,
     & chn_questionable_time_tag, chn_location,
     & nb_channels, chn_network


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Section summary.
c This common block is updated by seed_read_chn, so the user can
c have the information of start time of the section read by the mentioned 
c function.
c Each is an array, which holds the specified information for all the
c channels.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer*4   seed_year(MAX_N_CHN) ! year
      integer*4   seed_month(MAX_N_CHN) ! month
      integer*4   seed_day(MAX_N_CHN) ! day
      integer*4   seed_hour(MAX_N_CHN) ! hour
      integer*4   seed_minute(MAX_N_CHN) ! minute
      real*4      seed_second(MAX_N_CHN) ! second
      real*4      seed_rate(MAX_N_CHN) ! sample rate
      logical*4   seed_bad_time_tag(MAX_N_CHN) ! flag saying if time tag is bad
      integer*4   seed_nsamp(MAX_N_CHN) ! number of samples
      integer*4   seed_begin(MAX_N_CHN) ! beginning block
      integer*4   seed_end(MAX_N_CHN) ! end block
      character*3 seed_comp(MAX_N_CHN) ! component (channel)
      character*5 seed_station(MAX_N_CHN) ! station name
      character*2 seed_location(MAX_N_CHN) ! location id
      character*2 seed_network(MAX_N_CHN)  ! network code

      real*8      seed_blk_time(MAX_N_BLK) ! start time of the block
      real*8      seed_chan_time(max_n_blk) ! abs time current block+1
      integer*4   seed_blk_index(MAX_N_BLK) ! number of the sample
      integer*4   seed_blk_read             ! number of blocks read

      common / section_summary / seed_blk_time, seed_blk_index, 
     & seed_year,
     & seed_month, seed_day, seed_hour, seed_minute, seed_second,
     & seed_rate, seed_nsamp, seed_begin, seed_end, seed_bad_time_tag,
     & seed_station, seed_comp, seed_location, seed_blk_read,
     & seed_network,seed_chan_time


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Error message
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      character*80 error_msg  ! error message

      common / error_handle / error_msg
