
c     file codeco_common.f
c          ===============
c
c     version 3, 21-Aug-98
c
c     variables of header lines of GSE files
c     K. Stammler, 19-Aug-98

c     -- constants
      integer   c_sigsize          ! dimension of sample array
      integer   c_bufsize          ! dimension of char buffer for compressed data
      parameter(
     &   c_sigsize = 3000000,
     &   c_bufsize = 6*c_sigsize
     &)

c     -- header variables of common block
      integer   hdr_debug          ! debug level
      integer   hdr_year           ! data start time; year
      integer   hdr_month          ! data start time; month
      integer   hdr_day            ! data start time; day of month
      integer   hdr_jday           ! data start time; day of year
      integer   hdr_hour           ! data start time; hour
      integer   hdr_min            ! data start time; minute
      integer   hdr_sec            ! data start time; second
      integer   hdr_msec           ! data start time; millisecond
      integer   hdr_nsamp          ! number of samples
      integer   hdr_ifmtnum        ! input format number
      integer   hdr_ofmtnum        ! output format number
      integer   hdr_idiff          ! number of input differences
      integer   hdr_odiff          ! number of output differences
      integer   hdr_calunit        ! unit of calibration (0=displ, 1=vel, 2=acc)
      real      hdr_smprate        ! sample rate in Hz
      real      hdr_calfac         ! calibration factor
      real      hdr_calper         ! calibration period
      real      hdr_stalat         ! station latitude
      real      hdr_stalon         ! station longitude
      real      hdr_staelev        ! station elevation
      real      hdr_stadepth       ! emplacement depth
      real      hdr_beamaz         ! beam azimuth
      real      hdr_beamslo        ! beam slowness
      real      hdr_hang           ! horizontal orientation
      real      hdr_vang           ! vertical orientation
      character hdr_station*10     ! station name
      character hdr_stadescr*10    ! station/chan description
      character hdr_chan*3         ! channel name
      character hdr_instr*6        ! instrument type
      character hdr_network*9      ! network name
      character hdr_coosys*12      ! name of coordinate system
      character hdr_ifmtname*10    ! name of input format
      character hdr_ofmtname*10    ! name of output format

      common /gsehdr/
     &   hdr_debug, hdr_year, hdr_month, hdr_day, hdr_jday,
     &   hdr_hour, hdr_min, hdr_sec, hdr_msec, hdr_nsamp,
     &   hdr_ifmtnum, hdr_ofmtnum, hdr_idiff, hdr_odiff,
     &   hdr_calunit, hdr_smprate, hdr_calfac, hdr_calper,
     &   hdr_stalat, hdr_stalon, hdr_staelev, hdr_stadepth,
     &   hdr_beamaz, hdr_beamslo, hdr_hang, hdr_vang,
     &   hdr_station, hdr_stadescr, hdr_chan, hdr_instr,
     &   hdr_network, hdr_coosys, hdr_ifmtname, hdr_ofmtname
