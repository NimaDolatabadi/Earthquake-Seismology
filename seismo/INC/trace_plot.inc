
      real f_low,f_high                   ! filter limits
      real min_dist,max_dist              ! distance range in km
      real min_time,max_time              ! time range in seconds from OT

      integer gmt_set_max                 ! max of GMT def settings
      integer gmt_set_cnt                 ! counter for GMT def settings
      parameter (gmt_set_max=50)
      character*80 set_gmt_def(gmt_set_max)

      real amp_scale                      ! amplitude scale
      logical sfileonly_flag              ! 1. for stations in sfile only

      character*4 comp_list(50)
      integer comp_list_cnt

      common /traceplot1/f_low,f_high,min_dist,max_dist,amp_scale,
     &        min_time,max_time,gmt_set_cnt,comp_list_cnt,
     &        set_gmt_def,comp_list,
     &        sfileonly_flag

