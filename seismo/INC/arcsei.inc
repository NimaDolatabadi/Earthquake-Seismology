      
      character*12 def_arcfetch_channel(200) ! unit,stream,channel
      integer n_def_arcfetch_channel    ! number of
      character*80 archive              ! path to archive
      character*80 outpath              ! path to put converted data
      character*5 arcsei_code           ! network code
      logical merge_flag                ! true if merge Seisan files

      common /arcsei1/n_def_arcfetch_channel,
     &       merge_flag,
     &       outpath,
     &       archive,def_arcfetch_channel,arcsei_code

