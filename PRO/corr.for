c
c Program: corr
c
c correlate signals to either group events based on waveform similarity
c or determine phase readings through cross-correlation with master event
c
c Lars Ottemoller, BGS, Edinburgh, May 2005
c
ccccccccccccccccccccccccccccccccccccccccc 
c
c changes
c
c  11/10/2004 lot added conversion to single bit data
c                 store event data in y2_save for write out
c                 option to write out correlation function as 0/1 dep on min
c  03/11/2004 lot added cumulative energy and changed time format
c  30/05/2005 lot fixed bug with doubled sample rate
c  18/08/2005 lot added synthetic phase times
c                 identification of groups based on several channels
c                 changed xchar to tchar
c  22/08/2005 lot take out scaling from main part
c  31/01/2006 lot change on data selection for esel_crit.eq.0.
c  02/03/2006 lot check for maximum correlation in continuous mode
c     04/2006 lot write out file cc_pairs.out
c  02/05/2006 lot give index file in corr.inp, keyword 'SFILE INDEXFILE' 
c                 changed array structure, removed event grouping, which
c                 is now done in xclust
c  12/08/2008 lot for first signal, filter first, then select time window
c  13/03/2009 lot some cleanup, stop using maxcor above 1, still to check
c                 why this happens
c  28/12/2010 jh  gfortran pc: winplot remove, imlicit none, unit check
c  25.01.2012 jh  replce data2 with data3, there is now a data2 in hypparm.inc
c  25 10 2010 jh  change dim from 200 2000 in def channels
c  04 01 2013 jh nrecord to auto_tr
c  10 01 2013 jh  comment out seidim, in hypparm
c  03 03 2016 lo  some changes: write list of sfile trace file to find sfile name
c                 for event number, added check for max dt 
c  05 06 2018 ad  write out magnitude

      program corr

      implicit none                       ! force delcaration of all variables
      include 'hypparm.inc'
c     include 'seidim.inc'                ! SEISAN definitions
      include 'libsei.inc'                ! ------------------------
      include 'rea.inc'                   ! parameter common bliock
      include 'waveform.inc'              ! waveform data
      include 'seiplot.inc'
      integer seiclen
      external sei open,
     &         sei close,computer_type

      character*80 data1(5000),data3(5000)! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file
      character*80 text

      integer maxch,nch                   ! max number of channels and actual number
      parameter (maxch=9999)
      integer maxe                        ! max number of events
      parameter (maxe=50000)
      integer maxph
      parameter (maxph=500)     ! also see dimension of temp in read_corr_inp
      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer i,j,c,k,l,m                 ! counter
      integer maxm
      logical b_flag                      ! flag for file exist
      integer out_unit1,out_unit2         ! output units
      integer out_unit3,out_unit4         ! output units
      integer out_unit5
c out_unit6
      integer out_unit,out_unit7,out_unit8    ! output units
      integer in_unit                     ! input unit
      integer tu                          ! trace unit
      integer read1,write1                ! unit names
      character*80 sfile1(maxe),sfile2(maxe)! s-file names
      integer nmaster,nevent,cevent,cmaster ! number of events and counter
      character*5 station(maxch)          ! station name
      character*4 component(maxch)        ! component name
      character*5 ustat(maxch)            ! unknown station
      character*4 ucomp(maxch)            ! unknown component
      integer unch                        ! number of unknonwn channels
      character*5 stat(maxch)             ! station name
      character*4 comp(maxch)             ! component name
      real filter_low(maxch),filter_high(maxch)
      logical phase_flag(maxch)
      integer nstations
      real y1(max_sample+1),y2(max_sample),y2_save(max_sample) ! data vectors
      real wav_out_rate_save
      real y(max_sample),y3(max_sample),energy(max_sample+1)
      real cumeng(maxch)
      real amplitude(max_sample+1)
      integer iy(max_sample)
      integer nsamp_sel
      integer nfreq
      integer sel_crit(maxch),esel_crit,selc ! 0 for full trace 
                                             ! 1 for P
                                             ! 2 for S
                                             ! 3 for synth P
                                             ! 4 for synth S
      integer nsamp
      real start,t0,tstart
      integer ierr
      integer start_sample,stop_sample
      real pre_time,time_window(maxch),eselcrit,time_shift
      real pretime_ratio
      integer nset,ndouble_rate
      real ccmwin
      real xreal,rdc
      real x_work(max_sample)
      real y_work(max_sample*3)
      integer npoint_set(3)
      real x_size,y_size,xx0,yy0,aaa,
     &    xx,yy,dc,max
      real thl1_save       
      real maxcm(maxch)
      real difftime(maxch)
      logical chan_flag(maxch)
      integer nc,ndc
      real cx(1),cy(1)
      character*1 c_spec,key
      character*30 x_text,y_text
      character*80 title,wavefile
      real pi/3.1415972/
      integer nsmooth/10/
      real fmax
      character*80 fheader(max_trace)   ! file headers
      character*1040 theader            ! trace header
      character*80 outf(maxch)
      character*29 headtext
      double precision abs_phase_time(maxph)
      character*5 phase_stat(maxph)
      character*4 phase_comp(maxph)
      character*8 cphase(maxph)
      integer interactive
      logical sun,pc,linux
      real      cof(8)                  ! filter coefficients
      real      gain                    ! filter gain
      integer npasses                   ! number of passes, 1 forward,
                                        ! 2 both ways
      real mincorr
      real max1,max2
      integer mode                      ! 1 normal, 2 correlation matrix
      character*80 event_text(2)
      integer ind
      double precision msec1,msec2,msec3,msec4(maxch)
      double precision msecot1,msecot2
      double precision msecph1,msecph2
      integer year,month,day,hour,min,doy,isec
      real sec
      logical gflag,flag,flg
      character*80 index_file
      real start_lat,start_lon,fix_depth
      real lat1,lat2,lon1,lon2,degtorad
      real dist,distdeg,az,maxevdist,maxdifftime,maxstdist
      integer mincorrchan
      real slat,slon,sheight,stdist,dist1,dist2
      real depth1,depth2
      integer nmoho,nconrad
      real tmin
      parameter (degtorad=0.0174533)
      real wave_name_option,wave_corr_option
      real running_scale,single_bit,continuous_mode
      real continuous_ext
      integer ncphase,ncphase_new,ext_nchan
      character*160 system_call
      integer nrecord1,nrecord2,nhead1,nhead2
      logical filterflag,tuf,plimit_flag
      real av

      real mag1,mag2                       ! store event magnitude
c
c   chan def block
c
      character*5 def_in_stat(2000)  ! input station code in def file
      character*4 def_in_comp(2000)  ! input component code in def file
      character*5 def_out_stat(2000) ! output station code in def file
      character*4 def_out_comp(2000) ! output component code in def file
      integer   def_chan_number(2000)! channel number of current unit
      integer ndef_chan             ! number of channels defined
      character*29 dummy1
      character*5 dummy2
      character*12 time1,time2
      character*14 timelong
      common /def_chan/def_in_stat,def_in_comp,def_out_stat,
     *                 def_out_comp,def_chan_number,ndef_chan
      include 'version.inc'
c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)
      call systime(time1,timelong)
c
c remove index.??? files
c
      if (sun.or.linux) then
        system_call='rm corrind.???'
      elseif (pc) then
        system_call='del corrind.???'
      endif
      call systemc(system_call,seiclen(system_call))

      call get_seisan_def
      call get_model(nmoho,nconrad)
      thl1_save=parm(nl+1)
c
c   get def file for station codes, give file name
c
      text='wavfix.def'
      call read_def_chan(text,dummy1,dummy2)

c
c   open parameter file
c
      call sei open( old$,                 ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'corr.inp',       ! Filename.
     &                   in_unit,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
      call read_corr_inp(in_unit,maxch,maxe,
     &    sfile1,sfile2,nmaster,nevent,interactive,station,
     &    sel_crit,component,time_window,filter_low,filter_high,nch,
     &    mincorr,mode,pretime_ratio,esel_crit,ndouble_rate,ccmwin,
     &    start_lat,start_lon,fix_depth,wave_name_option,
     &    wave_corr_option,running_scale,single_bit,
     &    continuous_mode,continuous_ext,maxevdist,
     &    maxstdist,mincorrchan,maxdifftime,
     &    filterflag,tuf,plimit_flag)

c      write(*,*) ' debug nevent = ',nevent,mode
     
      call sei close(close$,in_unit,code)
      unch=0
      do i=1,maxch
        ustat(i)=' '
        ucomp(i)=' '
      enddo
c
c   open trace file
c
      if (tuf)
     &  call sei open( unknown$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'corr.trace',     ! Filename.
     &                   tu,               ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
c
c open output file
c
      if (mode.eq.1) then
        call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'corr.out',       ! Filename.
     &                   out_unit2,      ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
      endif

c
c read event info
c
      if (mode.eq.1) then
       do j=1,nevent
        if (tuf) write(tu,'(a1,i5,1x,a)') '#',j,sfile2(j)
        call sei open( old$,               ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   sfile2(j),        ! Filename.
     &                   read1,            ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
        call rea_event_in(read1,all,data1,code)
        event_text(1)=data1(1)(2:79)
        call sei close(close$,read1,code)
       enddo
      else
       do j=1,nevent
        if (tuf) write(tu,'(a1,i5,1x,a)') '#',j,sfile1(j)
        call sei open( old$,               ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   sfile1(j),        ! Filename.
     &                   read1,            ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
        call rea_event_in(read1,all,data1,code)
        event_text(1)=data1(1)(2:79)
        call sei close(close$,read1,code)
       enddo
      endif

      npasses=1
      ndc=0
c
c open output file
c
      if (continuous_mode.eq.1.) then
        call sei open( unknown$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                 'corr_cont.out',! Filename.
     &                   out_unit3,        ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
      endif
      if (mode.eq.2) then
         call sei open( unknown$,          ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'dt.cc',          ! Filename.
     &                   out_unit5,        ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
        call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'cc_pairs.out',  ! Filename.
     &                   out_unit7,        ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
        call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                  'cc_pairs_mag.out',  ! Filename.
     &                   out_unit8,        ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
      endif


c
c loop if corrleation matrix
c

      cmaster=1
5     continue
      if (cmaster.gt.nevent) goto 2000
      write(*,*) ' master # ',cmaster,
     &    ' '//sfile1(cmaster)(1:seiclen(sfile1(cmaster)))
      if (tuf) write(tu,*) ' master # ',cmaster,
     &    ' '//sfile1(cmaster)(1:seiclen(sfile1(cmaster)))
 
c
c loop over all events
c
      if (mode.eq.2) then 
        cevent=cmaster+1
      else
        cevent=1
      endif
      do j=1,maxch
        msec4(j)=0.
        cumeng(j)=0.
      enddo
10    continue
      all=.true.                  ! read all parameters
      ncphase=0
      c=1
      if (cevent.gt.nevent) goto 1500
 
      write(*,*) ' event # ',cevent,
     &    ' '//sfile2(cevent)(1:seiclen(sfile2(cevent)))
      if (tuf) write(tu,*) ' event # ',cevent,
     &    ' '//sfile2(cevent)(1:seiclen(sfile2(cevent)))
c
c loop over channels defined in corr.inp
c
20    continue

      if (c.gt.nch) goto 1000
      maxcm(c)=0.
      phase_flag(c)=.false.
      if(tuf)write(tu,*) ' station ',station(c)
      if (c.gt.1.and..not.chan_flag(c)) then
        if (tuf) write(tu,*) ' no data master/event/channel ',
     &      cmaster,cevent,c
        goto 990   ! skip channel if no data
      endif
      max=0.
      write(*,*) c,' station: '//station(c)//' component: '//
     &   component(c)//' selection: ',sel_crit(c)
      if (tuf) write(tu,*) ' station: '//station(c)//' component: '//
     &   component(c)//' selection: ',sel_crit(c)

c
c   read parameters for first event
c
      if (seiclen(sfile1(cmaster)).le.0) stop
      call sei open( old$,                 ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   sfile1(cmaster),  ! Filename.
     &                   read1,            ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
      call rea_event_in(read1,all,data1,code)
      nhead1=rea_nhead
      nrecord1=rea_nrecord
      event_text(1)=data1(1)(2:79)
      call sei close(close$,read1,code)
      if (hyp_lat(1).gt.-900) lat1=hyp_lat(1)*degtorad
      if (hyp_lon(1).gt.-900) lon1=hyp_lon(1)*degtorad
      if (tuf) write(tu,*) ' event 1 ',hyp_lat(1),hyp_lon(1)
      depth1=hyp_depth(1)
      mag1=hyp_mag_all(1)
c origin time of master event
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &  hyp_hour(1),hyp_min(1),hyp_sec(1),msecot1)
c
c read second event
c
      if (seiclen(sfile2(cevent)).le.0) stop
      call sei open( old$,                 ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   sfile2(cevent),   ! Filename.
     &                   read1,            ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
      call rea_event_in(read1,all,data3,code)
      nhead2=rea_nhead
      nrecord2=rea_nrecord
      event_text(2)=data3(1)(2:79)
      call sei close(close$,read1,code)
      if (hyp_lat(1).gt.-900) lat2=hyp_lat(1)*degtorad
      if (hyp_lon(1).gt.-900) lon2=hyp_lon(1)*degtorad
      if (tuf) write(tu,*) ' event 2 ',hyp_lat(1),hyp_lon(1)
      depth2=hyp_depth(1)
      mag2=hyp_mag_all(1)
c calculate origin time in seconds for event
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &  hyp_hour(1),hyp_min(1),hyp_sec(1),msecot2)

c
c check distance, and skip pair if distance too large
c
      call delaz(lat1,lon1,dist,distdeg,az,lat2,lon2)
      if (tuf) write(tu,*) ' inter event distance(km) = ',dist
      if (tuf) write(tu,*) ' lat1,lon1,lat2,lon2 ',
     &    lat1,lon1,lat2,lon2

      if (maxevdist.gt.0..and.dist.gt.maxevdist) then
        if (tuf) write(tu,*) ' skipping event pair, '//
     &     'distance(km) too large: ',dist
        write(*,*) ' skipping event pair, '//
     &     'distance(km) too large: ',dist
        cevent=cevent+1
        goto 10    ! event loop
      endif

c
c check for available channels in first event, if channel loop first channel
c
      if (c.eq.1) then
        do i=1,nch
          chan_flag(i)=.false.
        enddo
        write(*,*) ' checking for station, master ',cmaster
        call wav_init
        call wav_mem_init
c
c   get trace data file names
c
        call auto_tr(data1,nhead1,nrecord1,wav_nfiles,wav_filename)
c
c read waveform headers
c
        do i=1,wav_nfiles
          call get_full_wav_name(wav_filename(i),wavefile)
          wav_filename(i)=wavefile
          if (wavefile.ne.' ') then
            write(*,*) ' read header ',wav_filename(i)
            call read_wav_header(i)
          endif
        enddo
        write(*,*) ' done '
c
c check for stations on list in waveform data
c
        do i=1,nch
          do j=1,wav_nchan
            if (station(i).eq.wav_stat(j)) then
              chan_flag(i)=.true.
            endif
          enddo
        enddo
c
c check for stations not listed in parameter file
c
        do j=1,wav_nchan
          flg=.false.
          do i=1,nch
            if (station(i).eq.wav_stat(j)) then
              flg=.true.
            endif
          enddo
          write(*,*) j,tu
          if (.not.flg) then
            if (tuf) write(tu,*) 'notinpar',wav_stat(j),wav_comp(j)
            ind=0
            do i=1,unch
              if (ustat(i).eq.wav_stat(j).and.
     &            ucomp(i).eq.wav_comp(j)) ind=i
            enddo
            if (ind.eq.0.and.wav_comp(j)(4:4).eq.'Z') then
                if (unch.eq.maxch) then
                  write(*,*) ' too many unknown channels '
                endif
                unch=unch+1
                ustat(i)=wav_stat(j)
                ucomp(i)=wav_comp(j)
            endif
          endif
        enddo

c
c check for available channels in event, at first channel
c
        call wav_init
        call wav_mem_init
        write(*,*) ' checking for station, event ',cevent
c
c   get trace data file names
c
        call auto_tr(data3,nhead2,nrecord2,wav_nfiles,wav_filename)
        do i=1,wav_nfiles
          call get_full_wav_name(wav_filename(i),wavefile)
          wav_filename(i)=wavefile
          write(*,*) ' read header ',wav_filename(i)
          if (wavefile.ne.' ') then
            call read_wav_header(i)
          endif
        enddo
        write(*,*) ' done '
        do i=1,nch
          if (chan_flag(i)) then
            chan_flag(i)=.false.
            do j=1,wav_nchan
              if (station(i).eq.wav_stat(j)) then
                chan_flag(i)=.true.
              endif
            enddo
          endif
        enddo
        if (.not.chan_flag(c)) then
          if (tuf) write(tu,*) ' no data master/event/channel',
     &       cmaster,cevent,c
          goto 990   ! next channel
        endif
      endif
c
c restore thickness of first layer, as station elevation will be added
c
      parm(nl+1)=thl1_save   
c
c get station location
c
      stdist=0.
      call stat_loc(station(c),data1(1)(21:21),slat,slon,sheight)
      if (tuf) write(tu,*) ' station lat,lon,elevation ',
     &     slat,slon,sheight
      slat=slat*degtorad
      slon=slon*degtorad
      if (slat.eq.0..and.slon.eq.0..and.sheight.eq.0.) then
         if (tuf) write(tu,*) ' location unknown for station: ' //
     &                station(c)
         dist1=-1.
         dist2=-1.
      else
        call delaz(slat,slon,dist1,distdeg,az,lat1,lon1)
        call delaz(slat,slon,dist2,distdeg,az,lat2,lon2)
      endif
      if (maxstdist.gt.0..and.dist1.gt.maxstdist) then
        if (tuf) write(tu,*) 
     &    ' skipping station, station/master dist(km) = ',
     &    dist1
        goto 990
      endif
      if (maxstdist.gt.0..and.dist2.gt.maxstdist) then
        if (tuf) write(tu,*) 
     &    ' skipping station, station/event dist(km) = ',dist2
        goto 990
      endif
c
c add station elevation to first layer, this requires the elevation
c to be added to event depths as this is relative to 0 depth
c 
      parm(nl+1)=parm(nl+1)+sheight/1000.
      depth1=depth1+sheight/1000.
      depth2=depth2+sheight/1000.

c
c set pre_time, lot 19/09/2005
c
      pre_time=time_window(c)*pretime_ratio
      if (tuf) write(tu,*) ' PRE TIME = ',pre_time

c
c extract signal
c
      if (sel_crit(c).eq.1.or.sel_crit(c).eq.2) then   ! change 14102004
        start=1.
        selc=sel_crit(c)
      elseif (sel_crit(c).eq.3.or.sel_crit(c).eq.4) then
        if (stdist.eq.-1.) goto 990
        if (sel_crit(c).eq.3) then
          call corr_ttmin(nmoho,nconrad,dist1,depth1,v,tmin)
        else
          call corr_ttmin(nmoho,nconrad,dist1,depth1,vs,tmin)
        endif
        selc=4
        start=0.
c       if (tuf) write(tu,'(a,f20.3)') ' ot phase time hyp ',msecot2+tmin
        if(tuf)write(tu,*) ' master-station dist = ',dist1
c        if(tuf)write(tu,*) ' depth 1 = ',depth1
        if(tuf)write(tu,*) ' travel time master = ',tmin
c
c compute start
c
      else
        selc=4
        start=0.   ! absolute time
      endif
c      pre_time=0.
      err_text=' '
      call spec_select(data1,nhead1,nrecord1,.false.,
     *  station(c),component(c),
     *  selc,start,tstart,t0,nsamp,ierr,err_text)
c
c try to convert component code if channel not found
c
      if (seiclen(err_text).gt.0) then
        err_text=' '
        stat(c)=station(c)
        comp(c)=component(c)
        call set_def_chan(c,stat(c),comp(c))
c        if(tuf)write(tu,*) ' trying ',stat(c),comp(c)
        call spec_select(data1,nhead1,nrecord1,.false.,
     *    stat(c),comp(c),
     *    selc,start,tstart,t0,nsamp,ierr,err_text)
      endif

      if (seiclen(err_text).gt.0) then
        write(*,*) ' skipping '//station(c)//' '//
     &    err_text(1:seiclen(err_text))
        if(tuf)write(tu,*) ' skipping '//station(c)//' '//
     &    err_text(1:seiclen(err_text))
        goto 990
      endif

      wav_out_rate(3)=wav_rate(wav_current_chan(1))
c
c set tstart if synthetic phases
c
      if (sel_crit(c).eq.3.or.sel_crit(c).eq.4) then
        tstart=t0+tmin
      endif

c      if (sel_crit(c).eq.1.or.sel_crit(c).eq.2) then
      if (sel_crit(c).eq.0) then
        start_sample=1
        stop_sample=nsamp
      else
c        start_sample=int((tstart-pre_time)*wav_out_rate(3))
        start_sample=int((tstart-pre_time)*wav_out_rate(3)+.5)+1
        stop_sample=int((tstart+time_window(c))*
     &       wav_out_rate(3))
      endif
      if (start_sample.lt.0) then
         start_sample=1
c        write(*,*) ' wrong time interval '
c        stop
      endif
      if (stop_sample.gt.nsamp) then
        stop_sample=nsamp
c        write(*,*) ' wrong time interval '
c        stop
      endif
c
c check if p signal stops before s start, limit to p
c
      if (plimit_flag.and.sel_crit(c).eq.3) then
        if (tmin*pos-tmin.lt.time_window(c)) then 
          stop_sample=int((tstart+tmin*pos-tmin)*
     &       wav_out_rate(3))
          if (tuf) write(tu,*) 
     &   ' WARNING: P window ends after S arrival, signal cut'
        endif
      endif

c      msecph1=msecot1+tmin
c
c phase time for master signal from waveform data
c
      call timsec(wav_year(wav_current_chan(1)),
     &  wav_month(wav_current_chan(1)),wav_day(wav_current_chan(1)),
     &  wav_hour(wav_current_chan(1)),wav_min(wav_current_chan(1)),
     &  wav_sec(wav_current_chan(1)),msec1)
c      msecph1=msec1+(start_sample-1)/wav_rate(1)+pre_time
      msecph1=msec1+(start_sample-1)/wav_rate(wav_current_chan(1))
     &        +pre_time

      wav_out_stat(3)=wav_stat(wav_current_chan(1))
      wav_out_comp(3)=wav_comp(wav_current_chan(1))
      wav_out_year(3)=wav_year(wav_current_chan(1))
      wav_out_month(3)=wav_month(wav_current_chan(1))
      wav_out_day(3)=wav_day(wav_current_chan(1))
      wav_out_hour(3) = wav_hour(wav_current_chan(1))
      wav_out_min(3) = wav_min(wav_current_chan(1))
      wav_out_sec(3)= wav_sec(wav_current_chan(1))

      call remove_dc(y3,ndc,rdc,nsamp)

      if (filter_low(c).ne.0..and.filter_high(c).ne.0.) then
c        write(*,*) ' filter ',station(c),filter_low(c),filter_high(c)
        if(tuf)write(tu,*) 
     &    ' filter ',station(c),filter_low(c),filter_high(c)
c-- calc. filt. cof.
        call bndpas(filter_low(c),filter_high(c),1000.0/
     *        wav_out_rate(3),cof,gain)
c-- and filter
        call filter(signal1,nsamp,cof,gain,npasses)
c        call filter(y3,nsamp_sel,cof,gain,npasses)
      endif
      j=0
      do i=start_sample,stop_sample
        j=j+1
        y3(j)=signal1(i)
      enddo
      nsamp_sel=j
      wav_out_nsamp(3)=nsamp_sel
c
c moved here 07/2/2006, lot
c
c normalize to 1bit
c
      if (single_bit.eq.1.) then
        do i=1,nsamp_sel
          if (y3(i).ge.0.) then
            y3(i)=1.        
          else
            y3(i)=-1.
          endif
        enddo
      endif
        
c
c if second event is full trace, use the full master trace to find max,
c otherwise just master siganl
c
      max1=1.

      if (esel_crit.eq.0) then
        selc=4
        start=0.
      else
        if (sel_crit(c).eq.1.or.sel_crit(c).eq.2) then
          selc=sel_crit(c)
          start=1.
        elseif (sel_crit(c).eq.3.or.sel_crit(c).eq.4) then
          if (sel_crit(c).eq.3) then
            call corr_ttmin(nmoho,nconrad,dist2,depth2,v,tmin)
          else
            call corr_ttmin(nmoho,nconrad,dist2,depth2,vs,tmin)
          endif
          if (tuf) write(tu,*) ' event-station dist = ',dist2
c          if (tuf) write(tu,*) ' depth 2 = ',depth2
          if (tuf) write(tu,*) ' travel time event = ',tmin
c          selc=4
          selc=sel_crit(c)
          start=0.
        endif
      endif
      call spec_select(data3,nhead2,nrecord2,.false.,
     *  station(c),component(c),
     *  selc,start,tstart,t0,nsamp,ierr,err_text)
c
c try to convert component code if channel not found
c
      if (seiclen(err_text).gt.0) then
        err_text=' '
        stat(c)=station(c)
        comp(c)=component(c)
        call set_def_chan(c,stat(c),comp(c))
        call spec_select(data3,nhead2,nrecord2,.false.,
     *    stat(c),comp(c),
     *    selc,start,tstart,t0,nsamp,ierr,err_text)
      endif

      if (seiclen(err_text).gt.0) then
        write(*,*) ' skipping '//station(c)//' '//
     &    err_text(1:seiclen(err_text))
        if (tuf) write(tu,*) ' skipping '//station(c)//' '//
     &    err_text(1:seiclen(err_text))
        goto 990
      endif
      wav_out_rate(2)=wav_rate(wav_current_chan(1))
c
c set tstart if synthetic phases
c
c 31/1/2006
c      if (sel_crit(c).eq.3.or.sel_crit(c).eq.4) then
      if (esel_crit.eq.1..and.
     &   (sel_crit(c).eq.3.or.sel_crit(c).eq.4)) then
        tstart=t0+tmin
      endif

c      if (selc.eq.1.or.selc.eq.2) then
c      if (sel_crit(c).ne.0) then
c 31/01/2006 lot
      if (esel_crit.eq.1..and.sel_crit(c).ne.0) then
        if (mode.eq.1) then
c          start_sample=int((tstart-2.*pre_time)*wav_out_rate(2))
          start_sample=int((tstart-2.*pre_time)*wav_out_rate(2)+.5)+1
          time_shift=pre_time
          stop_sample=int((tstart+time_window(c))*
     &       wav_out_rate(2))
        elseif (mode.eq.2) then
c changed lot 08/08/05
c          start_sample=int((tstart-2.*pre_time)*wav_out_rate(2))
          start_sample=int((tstart-2.*pre_time)*wav_out_rate(2)+.5)+1
          time_shift=pre_time   
c          if (ccmwin.ne.999..and.ccmwin.gt.0.) then
          if (ccmwin.gt.0.) then
c            start_sample=int((tstart-pre_time-ccmwin)*wav_out_rate(2))
c            time_shift=ccmwin
            stop_sample=int((tstart+ccmwin)*
     &        wav_out_rate(2))
          else
            stop_sample=int((tstart+time_window(c))*
     &        wav_out_rate(2))
c          time_shift=2.*pre_time
          endif
        endif
      else
        time_shift=pre_time ! added 16/10/2004
        start_sample=1
        stop_sample=nsamp
      endif
      if (start_sample.lt.0) then
        start_sample=1
c        write(*,*) ' wrong time interval '
c        stop
      endif
      if (stop_sample.gt.nsamp) then
        stop_sample=nsamp
c        write(*,*) ' wrong time interval '
c        stop
      endif
c
c check if p signal stops before s start, limit to p
c
      if (plimit_flag.and.sel_crit(c).eq.3) then
        if (tmin*pos-tmin.lt.time_window(c)) then 
          stop_sample=int((tstart+tmin*pos-tmin)*
     &       wav_out_rate(3))
c          if (tuf) write(tu,*) 
c     &   ' WARNING: P window ends after S arrival, signal cut'
        endif
      endif

      if (seiclen(err_text).gt.0) then
        write(*,*) ' skipping '//station(c)//' '//
     &    err_text(1:seiclen(err_text))
        if (tuf) write(tu,*) ' skipping '//station(c)//' '//
     &    err_text(1:seiclen(err_text))
      endif
 
      call timsec(wav_year(wav_current_chan(1)),
     &  wav_month(wav_current_chan(1)),wav_day(wav_current_chan(1)),
     &  wav_hour(wav_current_chan(1)),wav_min(wav_current_chan(1)),
     &  wav_sec(wav_current_chan(1)),msec1)
c
c get absolute time of first channel
c
      call timsec(wav_year(wav_first),
     &  wav_month(wav_first),wav_day(wav_first),
     &  wav_hour(wav_first),wav_min(wav_first),
     &  wav_sec(wav_first),wav_abs_time(wav_first))

c
c time of start sample of extracted event signal
c
      msec1=msec1+(start_sample-1)/wav_out_rate(2)
      msec2=msec1+time_shift
      call sectim(msec1,wav_out_year(2),doy,
     &   wav_out_month(2),wav_out_day(2),wav_out_hour(2),
     &   wav_out_min(2),wav_out_sec(2))


      wav_out_stat(2)=wav_stat(wav_current_chan(1))
      wav_out_comp(2)=wav_comp(wav_current_chan(1))
      wav_out_nsamp(2)=wav_nsamp(wav_current_chan(1))
      wav_out_cbyte(2)='4'

      call remove_dc(signal1,ndc,rdc,wav_out_nsamp(2))
      if (filter_low(c).ne.0..and.filter_high(c).ne.0.) then
c-- calc. filt. cof.
        call bndpas(filter_low(c),filter_high(c),1000.0/wav_out_rate(2),
     *              cof,gain)
c-- and filter
        call filter(signal1,wav_out_nsamp(2),cof,gain,npasses)
      endif
      max2=1.
      j=0
c
c copy signal to y2 array
c
      do i=start_sample,stop_sample
        j=j+1
        y2_save(j)=signal1(i)
        y2(j)=signal1(i)
      enddo
c
c normalize to 1bit
c
      if (single_bit.eq.1.) then
        do i=1,j
          if (y2(i).ge.0.) then
            y2(i)=1.        
            y2_save(i)=1.        
          else
            y2(i)=-1.        
            y2_save(i)=-1.
          endif
        enddo
      endif
        
      wav_out_nsamp(2)=j
      wav_out_rate_save=wav_out_rate(2)
c      write(tu,*) ' scale master/event ',max1/max2

c
c resample signals
c
      if (single_bit.eq.0.) then
        do i=1,ndouble_rate
          if (tuf) write(tu,*) ' doubling sample rate ',wav_out_stat(2),
     &      ' ',wav_out_comp(2),' ',wav_out_rate(2),'->',
     &      wav_out_rate(2)*2.
          call resample_linear(y3,wav_out_nsamp(3),max_sample,
     &     wav_out_rate(3))
          call resample_linear(y2,wav_out_nsamp(2),max_sample,
     &     wav_out_rate(2))
          j=wav_out_nsamp(2)
          call resample_linear(y2_save,j,max_sample,
     &     wav_out_rate_save)
        enddo
      endif

c
c compute correlation, y3 is master signal, y2 is event signal,
c y1 is correlation function
c
      call correlation(y3,wav_out_nsamp(3),y2,wav_out_nsamp(2),
     &     y1,energy,amplitude,wav_out_nsamp(1))

      call sectim(msec2,wav_out_year(1),doy,
     &   wav_out_month(1),wav_out_day(1),wav_out_hour(1),
     &   wav_out_min(1),wav_out_sec(1))

      wav_out_stat(1)=wav_out_stat(2)
      wav_out_comp(1)=wav_out_comp(2)
      wav_out_comp(1)(3:3)='C'
      wav_out_rate(1)=wav_out_rate(2)

      wav_out_cbyte(1)='4'

      if (continuous_mode.eq.0.) then
c
c event based data
c

c
c work out time of maximum
c
c changed lot 18/08/2005
          max=0.  ! added 19/08/2005
c j is index of max correlation
          j=0
          do i=1,int(wav_out_rate(1)*
     &      (stop_sample-start_sample))
            if (y1(i).gt.max.and.
     &          y1(i).le.1.) then ! 13/03/2009 lot
              max=y1(i)
              j=i
            endif
          enddo

        if (sel_crit(c).eq.1.or.sel_crit(c).eq.3) then 
          cphase(c)='P'
        elseif (sel_crit(c).eq.2.or.sel_crit(c).eq.4) then 
          cphase(c)='S'
        endif
        maxcm(c)=max
        phase_stat(c)=wav_out_stat(1)
        phase_comp(c)=wav_out_comp(1)(1:1)//wav_out_comp(1)(4:4)

        abs_phase_time(c)=msec1+
     &      time_shift+
     &      (j-1)/wav_out_rate(1)

        msecph2=abs_phase_time(c)

        if (tuf) write(tu,'(a,f20.3)') ' ot phase time wave ',
     &    msecph2
c
c store differential travel time between events
c
        difftime(c)=
     &    (msecph1-msecot1)-(msecph2-msecot2)
c
c added check for dt to avoid cycle skipping, lo 03/03/2016
c
        if (max.ge.mincorr) then
          if (tuf) write(tu,*) station(c)//' phase found '//cphase(c)
          if (tuf) write(tu,*) ' max corr = ',max
          phase_flag(c)=.true.
        else
          if (tuf) write(tu,*) ' correlation low, max = ',
     &       max,' or dt too large ',difftime(c)
          phase_flag(c)=.false.
        endif
      else
c
c continuous mode: search for all times where correlation
c                  is above threshold
        ncphase_new=ncphase+1
        do i=1,wav_out_nsamp(1)
          if (y1(i).gt.mincorr) then
            msec3=msec1+time_shift+(i-1)/wav_out_rate(1)
c require that time difference abs_phase_time(c) at least 1/2. of time window
            if (msec3.gt.msec4(c)+time_window(c)/2.) then
c
c search for maxiumum within 1/2 window of exceeding threshold
c added 26/2/2006, lot
c
              max=y1(i)
c              write(*,*) ' xxx ',msec3
              k=i
              do j=i,i+int(time_window(c)/2.*wav_out_rate(1))
                if (y1(j).gt.max) then
                  max=y1(j)
                  k=j
                endif
              enddo
              msec3=msec1+time_shift+(k-1)/wav_out_rate(1)
              ncphase=ncphase+1
              if (ncphase.ge.maxph) then
                write(*,*) ' too many detections, stop '
                stop
              endif
              if (sel_crit(c).eq.1.or.sel_crit(c).eq.3) then 
                cphase(ncphase)='P'
              elseif (sel_crit(c).eq.2.or.sel_crit(c).eq.4) then 
                cphase(ncphase)='S'
              endif
              phase_stat(ncphase)=wav_out_stat(1)
              phase_comp(ncphase)=wav_out_comp(1)(1:1)//
     &          wav_out_comp(1)(4:4)
              abs_phase_time(ncphase)=msec3
              call sectim(msec3,year,doy,month,day,hour,min,sec)
              cumeng(c)=cumeng(c)+energy(i)
              if (msec4(c).ne.0.) then
                write(out_unit3,'(a5,1x,i4.4,"-",i2.2,"-",i2.2,"T",
     &               i2.2,":",i2.2,":",i2.2,".",i2.2,1x,f15.3,
     &               1x,f12.3,1x,f15.1,1x,f20.1,1x,f10.1,1x,f5.2)') 
     &          phase_stat(ncphase),
     &          year,month,day,hour,min,int(sec),
     &          int((sec-int(sec))*100.),msec3,msec3-msec4(c),
c     &          energy(i),cumeng(c),amplitude(i),y1(i)
     &          energy(k),cumeng(c),amplitude(k),y1(k)
              endif
              msec4(c)=msec3
            endif
          endif
        enddo
c
c write out event data for times where correlation is above threshold
c
        if (continuous_ext.ge.1.) then
          do i=ncphase_new,ncphase
c
c open extract input file
c
            call sei open( unknown$,         ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'extr.inp',       ! Filename.
     &                   out_unit4,        ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
 
            if (continuous_ext.eq.1.) then
              ext_nchan=1
            elseif (continuous_ext.eq.2.) then
              ext_nchan=wav_nchan
            endif
            write(out_unit4,'(i2)') ext_nchan
            do k=1,ext_nchan
              if (continuous_ext.eq.1.) then
                l=wav_current_chan(1)
              else
                l=k
              endif
              write(out_unit4,'(i3,1x,2f10.3)') 
     &          l,
c     &          abs_phase_time(i)-wav_abs_time(wav_first)-
c     &          pre_time,time_window(c)+2.*pre_time
     &          abs_phase_time(i)-wav_abs_time(wav_first)-
     &          5.,time_window(c)+2.*5.
            enddo

            call sei close(close$,out_unit4,code)

            write(*,*) cevent
            system_call='wavetool -sfile '// sfile2(cevent)
     &        (1:seiclen(sfile2(cevent))) //
     &       ' -chansel extr.inp -wav_out_file SEISAN'
            write(*,*) system_call(1:seiclen(system_call))
            call systemc(system_call,seiclen(system_call))
          enddo
        endif
      endif

c
c make headers
c
      if (wave_corr_option.ne.0.) then
        wav_out_nchan=2
        headtext='correlated signal'
        call sheads(wav_out_year,wav_out_month,
     *      wav_out_day,wav_out_hour,
     *      wav_out_min,wav_out_sec,wav_out_nchan,1,
     *      wav_out_stat(1),headtext,wav_out_stat,wav_out_comp,
     *      wav_out_nsamp,wav_out_rate,wav_out_cbyte,
     *      outf(c),fheader,theader)

        theader(76:76)='G'
        theader(156:159)='100.'
       
        if (sel_crit(c).eq.1.or.sel_crit(c).eq.3) then
          outf(c)=outf(c)(1:seiclen(outf(c)))//'_P'
        elseif (sel_crit(c).eq.2.or.sel_crit(c).eq.4) then
          outf(c)=outf(c)(1:seiclen(outf(c)))//'_S'
        endif
      write(*,*) ' output file '//outf(c)(1:seiclen(outf(c)))
c
c   open output file
c
        chr_f_form$ = 'unformatted'
        call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   outf(c),          ! Filename.
     &                   out_unit1,         ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
c
c write out correlated signal
c
        do i=1,12
          write(out_unit1) fheader(i)
        enddo
        do j=1,2
          call sheads(wav_out_year,wav_out_month,
     *      wav_out_day,wav_out_hour,
     *      wav_out_min,wav_out_sec,wav_out_nchan,j,
     *      wav_out_stat(j),headtext,wav_out_stat,wav_out_comp,
     *      wav_out_nsamp,wav_out_rate,wav_out_cbyte,
     *      text,fheader,theader)
          write(out_unit1) theader
          nsamp=wav_out_nsamp(j) 
          do k=1,wav_out_nsamp(j)
            if (j.eq.1) then
              if (wave_corr_option.eq.1.) then
                iy(k)=int(y1(k)*100.)
              else
                if (y1(k).ge.mincorr) then
                  iy(k)=1
                else
                  iy(k)=0
                endif
              endif
            endif
            if (j.eq.2) iy(k)=int(y2_save(k))
          enddo
          write(out_unit1) (iy(i),i=1,nsamp)
        enddo
        call sei close(close$,out_unit1,code)
      endif

c
c plot 
c
500   continue
c
c start graphics
c
      if (interactive.eq.1) then
        call open_display

        text=event_text(1)
        i=seiclen(text)
        call tchars(text,i,200.,760.)
        text=event_text(2)
        i=seiclen(text)
        call tchars(text,i,200.,750.)

        text='station: '// station(c)//' '//' component: '//component(c)
        i=seiclen(text)
        call tchars(text,i,200.,740.)
        text='Press f to move forward '
        i=seiclen(text)
        call tchars(text,i,770.,200.)
        text='   or q to quit '
        i=seiclen(text)
        call tchars(text,i,770.,190.)
        nset=1
        npoint_set(1)=wav_out_nsamp(3)
          
        x_size=600.
        y_size=150.
        xx0=120.
        yy0=580.
        aaa=-xx0
        x_text='time (s)'
        y_text='amp'
        title=' '

c
c plot signals
c

c
c first signal
c
        do i=1,wav_out_nsamp(3)
          x_work(i)=i/wav_out_rate(3)
          y_work(i)=y3(i)
        enddo

        call xy_plot(nset,npoint_set,x_work,y_work,
     *     title,x_text,y_text,x_size,
     *     y_size,xx0,yy0,2,0,30.0,0,c_spec,nc,cx,cy)

c
c second signal
c
        npoint_set(1)=wav_out_nsamp(2)
        do i=1,wav_out_nsamp(2)
          x_work(i)=i/wav_out_rate(2)
          y_work(i)=y2_save(i)
        enddo

        yy0=330
        call xy_plot(nset,npoint_set,x_work,y_work,
     *     title,x_text,y_text,x_size,
     *     y_size,xx0,yy0,2,0,30.0,0,c_spec,nc,cx,cy)

c
c third signal
c
        npoint_set(1)=wav_out_nsamp(2)
        do i=1,wav_out_nsamp(2)
          x_work(i)=(i-1)/wav_out_rate(2)
          y_work(i)=y1(i)
        enddo

        yy0=80
        call xy_plot(nset,npoint_set,x_work,y_work,
     *     title,x_text,y_text,x_size,
     *     y_size,xx0,yy0,2,0,30.0,interactive,c_spec,nc,cx,cy)

        if (c_spec.eq.'q'.or.c_spec.eq.'Q') goto 2000
        if (c_spec.eq.'r'.or.c_spec.eq.'R') goto 500
        call clear_to_alpha
      endif

 990  continue
      c=c+1
      goto 20

 1000 continue
c
c end of channel loop
c

c
c check if enough channels above minimum correlation
c lot 21/04/2006
c
      xx=0.   ! average correlation
      m=0     ! number of channels above correlation threshold
      do k=1,nch
        if (maxcm(k).ge.mincorr) then
          m=m+1
c          write(*,*) m,maxcm(k),mincorr
          xx=xx+maxcm(k)
        endif
      enddo   
c
c average correlation
c
      if (m.gt.0.and.m.ge.mincorrchan) then
        xx=xx/float(m)
        if (xx.gt.999.) then
          write(*,*)  cmaster,
     &     sfile1(cmaster)(1:seiclen(sfile1(cmaster))),
     &     cevent,sfile1(cevent)(1:seiclen(sfile1(cevent))),m,xx
          stop
        endif
        if (mode.eq.2) then
c
c write out pair
c
          write(out_unit7,'(i4,1x,a,1x,i4,1x,a,1x,i3,1x,f5.3)') 
     &     cmaster,sfile1(cmaster)(1:seiclen(sfile1(cmaster))),
     &     cevent,sfile1(cevent)(1:seiclen(sfile1(cevent))),m,xx

          write(out_unit8,'(i4,1x,a,1x,f4.1,1x,i4,1x,a,1x,i3,1x,
     &     f5.3,1x,f4.1)') 
     &     cmaster,sfile1(cmaster)(1:seiclen(sfile1(cmaster))),mag1,
     &     cevent,sfile1(cevent)(1:seiclen(sfile1(cevent))),m,xx,mag2
     
c
c write out differential times for hypodd
c
          av=0.
          do k=1,nch
            av=av+difftime(k)
          enddo
          av=av/float(nch)
          flag=.false. ! lot 28/2/2007
          do k=1,nch
            if (maxcm(k).ge.mincorr.and.
c     &        abs(difftime(k)).le.maxdifftime) then
     &        abs(difftime(k)-av).le.maxdifftime) then
                flag=.true.
            endif
          enddo
          if (flag) then
                    write(out_unit5,'(a1,1x,i4,1x,i4,1x,f3.1)') 
     &                '#',cmaster,cevent,0.
            do k=1,nch
                  if (maxcm(k).ge.mincorr.and.
c     &              abs(difftime(k)).le.maxdifftime) then
     &              abs(difftime(k)-av).le.maxdifftime) then
                    write(out_unit5,'(a5,1x,f6.3,1x,f5.3,1x,a1)')
     &              station(k),
     &              difftime(k),
     &              maxcm(k), 
     &              cphase(k)
                  endif
            enddo
          endif
        endif
      endif

      if (mode.eq.1) then
c
c write out s-file
c
        call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   sfile2(cevent),   ! Filename.
     &                   in_unit,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
        all=.true.
        call rea_event_in(in_unit,all,data1,code) 
        call sei close(close$,in_unit,code)

c
c save amplitudes
c
        j=0
        do i=1,rea_nphase
          if (rea_phase(i)(1:2).eq.'AM') then
            j=j+1
            if (i.ne.j) then
              call rea_phase_clear(j)
              rea_phase(j)=rea_phase(i)
              rea_stat(j)=rea_stat(i)
              rea_co(j)=rea_co(i)
              rea_abs_time(j)=rea_abs_time(i)
              rea_year(j)=rea_year(i)
              rea_month(j)=rea_month(i)
              rea_day(j)=rea_day(i)
              rea_hour(j)=rea_hour(i)
              rea_min(j)=rea_min(i)
              rea_sec(j)=rea_sec(i)
              rea_amp(j)=rea_amp(i)
              rea_per(j)=rea_per(i)
            endif
          endif
        enddo
        rea_nphase=j
        if (wave_name_option.eq.1.) rea_nwav=0
        do i=1,nch
c lot 16/08/2005, always include name
          if (phase_flag(i)) then 
c waveform file names
            if (wave_name_option.eq.1.) then
              rea_nwav=rea_nwav+1
       rea_wav(rea_nwav)='                                        ' 
     &        //'                                       6' 
              write(rea_wav(rea_nwav)(2:1+seiclen(outf(i))),'(a)') 
     &               outf(i)(1:seiclen(outf(i))) 
            endif
c phases
            rea_nphase=rea_nphase+1
            call rea_phase_clear(rea_nphase)
            rea_abs_time(rea_nphase)=abs_phase_time(i)
            rea_stat(rea_nphase)=phase_stat(i)
            rea_co(rea_nphase)=phase_comp(i)
            rea_phase(rea_nphase)=cphase(i)  
          endif
        enddo
c continuous phases
        do i=1,ncphase
          if (i.eq.1) then
c waveform file names
            if (wave_name_option.eq.1.) then
              rea_nwav=rea_nwav+1
       rea_wav(rea_nwav)='                                        ' 
     &        //'                                       6' 
              write(rea_wav(rea_nwav)(2:1+seiclen(outf(i))),'(a)') 
     &               outf(i)(1:seiclen(outf(i))) 
            endif
          endif
          rea_nphase=rea_nphase+1
          call rea_phase_clear(rea_nphase)
          rea_abs_time(rea_nphase)=abs_phase_time(i)
          rea_stat(rea_nphase)=phase_stat(i)
          rea_co(rea_nphase)=phase_comp(i)
          rea_phase(rea_nphase)=cphase(i)  
        enddo
        
        all=.true.
        if (start_lat.ne.999.) hyp_lat(1)=start_lat
        if (start_lon.ne.999.) hyp_lon(1)=start_lon
        if (start_lat.ne.999..or.start_lon.ne.999.)
     &      hyp_epi_flag(1)='S'
        if (fix_depth.ne.999.) then
          hyp_depth(1)=fix_depth
          hyp_depth_flag(1)='F'
        endif
        call rea_event_out(out_unit2,all,data1,code)
c      elseif (mode.eq.2) then
c        do i=1,nch
c          write(out_unit2,'(i3,1x,i3,1x,a5,1x,a4,1x,f6.3)') 
c     &       cmaster,cevent,phase_stat(i),phase_comp(i),
c     &       maxcm(cmaster,cevent,i)
c        enddo
      endif
      cevent=cevent+1
      goto 10    ! event loop
1500  continue
      cmaster=cmaster+1
      goto 5     ! master event loop

2000  continue


      if (mode.eq.1) then
        call sei close(close$,out_unit2,code)
      endif
      if (mode.eq.2) then
        call sei close(close$,out_unit5,code)
        call sei close(close$,out_unit7,code)
        call sei close(close$,out_unit8,code)

      endif
      if (continuous_mode.eq.1.)
     &    call sei close(close$,out_unit3,code)
      if (tuf) call sei close(close$,tu,code)
      
c
c write out unknown channels
c
      call sei open( unknown$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'unknown_channel.out',     ! Filename.
     &                   tu,               ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
      do i=1,unch
        write(tu,'(a10,a5,5x,a4)') 'STATION   ',ustat(i),ucomp(i)
      enddo
      call sei close(close$,tu,code)

      write(*,*)
      if (mode.eq.1) then
        write(*,*) ' nordic output file: corr.out '
      endif
      if (tuf) then
        write(*,*) ' trace output file:  corr.trace '
      endif
      if (mode.eq.2) then
        write(*,*) ' correlated event pairs: cc_pairs.out '
        write(*,*) ' input for hypo_DD: dt.cc '
      endif

      read(time1,'(6i2)') year,month,day,hour,min,isec
      year=year+2000
      sec=float(isec)
      call timsec(year,month,day,hour,min,sec,msec1)

      call systime(time2,timelong)
      read(time2,'(6i2)') year,month,day,hour,min,isec
      year=year+2000
      sec=float(isec)
      call timsec(year,month,day,hour,min,sec,msec2)
c      write(*,*) time1,time2,msec2-msec1
      write(*,*)
      write(*,*) ' run time in sec: ',msec2-msec1

      stop
      end


      subroutine read_corr_inp(in_unit,maxch,maxe,
     &    sfile1,sfile2,
     &    nmaster,nevent,interactive,
     &    station,sel_crit,component,time_window,filter_low,
     &    filter_high,nch,mincorr,mode,pretime_ratio,esel_crit,
     &    ndouble_rate,ccmwin,start_lat,start_lon,fix_depth,
     &    wave_name_option,wave_corr_option,running_scale,
     &    single_bit,continuous_mode,continuous_ext,
     &    maxevdist,maxstdist,mincorrchan,maxdifftime,
     &    filterflag,traceout_flag,plimit_flag)
      implicit none
      character*(*) sfile1(*),sfile2(*)
      character*80 temp(50000) ! same as maxe in main program
      character*5 station(*)
      character*4 component(*)
      character*80 indexfile
      real time_window(*),xreal
      integer sel_crit(*),nch,esel_crit,ndouble_rate
      real filter_low(*),filter_high(*),mincorr,pretime_ratio,ccmwin
      character*80 line
      integer seiclen,in_unit,i,interactive,maxch,maxe,
     &    nmaster,nevent,mode,mincorrchan
      logical filterflag,traceout_flag
      real start_lat,start_lon,fix_depth
      real wave_name_option,wave_corr_option
      real running_scale,single_bit,continuous_mode
      real continuous_ext,maxevdist,maxstdist,maxdifftime
      logical plimit_flag
      integer inu,code
      logical b_flag                      ! flag for file exist
      include 'seidim.inc'                ! SEISAN definitions
      include 'libsei.inc'                ! ------------------------

c
c init
c
      sfile1(1)=' '
      xreal=1.
      nch=0
      interactive=0.
      nevent=0
      nmaster=0
      mincorr=.5
      filterflag=.true.
      mode=1
      pretime_ratio=0.1
      plimit_flag=.true.
      esel_crit=0.
      ndouble_rate=0
      ccmwin=999.
      start_lat=999.
      start_lon=999.
      fix_depth=999.
      wave_name_option=0.
      wave_corr_option=0.
      running_scale=0.
      single_bit=0.
      continuous_mode=0.
      continuous_ext=0.
      maxevdist=0.
      maxstdist=0.
      maxdifftime=9.99
      mincorrchan=0
      traceout_flag=.true. 
      indexfile=' '
c
c read input
c
10    continue

      read(in_unit,'(a80)',end=20) line

c fill missing characters
      do i=seiclen(line)+1,80
        line(i:i)=' '
      enddo

      if (line(1:12).eq.'SFILE MASTER') then
        nmaster=nmaster+1
        sfile1(nmaster)=line(41:seiclen(line)) 
        if (seiclen(sfile1(nmaster)).le.0) nmaster=nmaster-1
      elseif (line(1:11).eq.'SFILE EVENT') then
        nevent=nevent+1
        if (nevent.ge.maxe) then
          write(*,*) ' too many stations in corr.inp ' 
          stop
        endif
        sfile2(nevent)=line(41:seiclen(line)) 
        if (seiclen(sfile2(nevent)).le.0) nevent=nevent-1
      elseif (line(1:15).eq.'SFILE INDEXFILE') then
        indexfile=line(41:seiclen(line))
      elseif (line(1:11).eq.'INTERACTIVE') then
        read(line(41:),*) xreal
        interactive=int(xreal)
      elseif (line(1:6).eq.'FILTER') then
        read(line(41:),*) xreal
        if (xreal.ne.1.) filterflag=.false.
      elseif (line(1:12).eq.'TRACE OUTPUT') then
        read(line(41:),*) xreal
        if (xreal.ne.1.) traceout_flag=.false.
      elseif (line(1:14).eq.'START LATITUDE') then
        read(line(41:),*) xreal
        if (xreal.ne.999.) start_lat=xreal
      elseif (line(1:15).eq.'START LONGITUDE') then
        read(line(41:),*) xreal
        if (xreal.ne.999.) start_lon=xreal
      elseif (line(1:9).eq.'FIX DEPTH') then
        read(line(41:),*) xreal
        if (xreal.ne.999.) fix_depth=xreal
      elseif (line(1:16).eq.'CC MATRIX WINDOW') then
        read(line(41:),*) ccmwin
      elseif (line(1:14).eq.'N DOUBLE SRATE') then
        read(line(41:),*) xreal
        ndouble_rate=int(xreal)
      elseif (line(1:10).eq.'MIN CORR  ') then
        read(line(41:),*) mincorr
      elseif (line(1:13).eq.'MIN CORR CHAN') then
        read(line(41:),*) xreal
        mincorrchan=int(xreal)
      elseif (line(1:18).eq.'MAX EVENT DISTANCE') then
        read(line(41:),*) xreal
        maxevdist=xreal
      elseif (line(1:17).eq.'MAX STAT DISTANCE') then
        read(line(41:),*) xreal
        maxstdist=xreal
      elseif (line(1:13).eq.'MAX DIFF TIME') then
        read(line(41:),*) xreal
        maxdifftime=xreal
      elseif (line(1:10).eq.'SINGLE BIT') then
        read(line(41:),*) xreal
        single_bit=int(xreal)
        if (single_bit.ne.0..and.
     &      single_bit.ne.1.) single_bit=0.
      elseif (line(1:15).eq.'CONTINUOUS MODE') then
        read(line(41:),*) xreal
        continuous_mode=int(xreal)
        if (continuous_mode.ne.0..and.
     &      continuous_mode.ne.1.) continuous_mode=0.
      elseif (line(1:18).eq.'CONTINUOUS EXTRACT') then
        read(line(41:),*) xreal
        continuous_ext=int(xreal)
        if (continuous_ext.ne.0..and.
     &      continuous_ext.ne.1..and.
     &      continuous_ext.ne.2.) continuous_ext=0.
      elseif (line(1:12).eq.'WAVENAME OUT') then
        read(line(41:),*) xreal
        wave_name_option=int(xreal)
        if (wave_name_option.ne.0..and.
     &      wave_name_option.ne.1.) wave_name_option=0.
      elseif (line(1:13).eq.'WAVE CORR OUT') then
        read(line(41:),*) xreal
        wave_corr_option=int(xreal)
        if (wave_corr_option.ne.0..and.
     &      wave_corr_option.ne.1..and.
     &      wave_corr_option.ne.2.) wave_corr_option=0.
      elseif (line(1:13).eq.'RUNNING SCALE') then
        read(line(41:),*) xreal
        running_scale=int(xreal)
        if (running_scale.ne.0..and.
     &      running_scale.ne.1.) running_scale=0.
      elseif (line(1:10).eq.'PRE SIGNAL') then
        read(line(41:),*) pretime_ratio
      elseif (line(1:7).eq.'P LIMIT') then
        read(line(41:),*) xreal 
        if (xreal.eq.1.) then
          plimit_flag=.true.
        else
          plimit_flag=.false.
        endif
      elseif (line(1:13).eq.'EVENT SELCRIT') then
        read(line(41:),*) xreal
        esel_crit=int(xreal)
        if (esel_crit.ne.0.and.esel_crit.ne.1) then
          write(*,*) ' wrong phase selection, has to be 0 or 1 '
          stop
        endif
      elseif (line(1:7).eq.'STATION') then
        nch=nch+1
        if (nch.ge.maxch) then
          write(*,*) ' too many stations in corr.inp ' 
          stop
        endif
        filter_low(nch)=0.
        filter_high(nch)=0.
        sel_crit(nch)=1.
        time_window(nch)=0.
        station(nch)=line(11:15) 
        component(nch)=line(21:24) 
        read(line(31:40),*) xreal
        sel_crit(nch)=int(xreal)
        if (sel_crit(nch).ne.1.and.sel_crit(nch).ne.2.and.
     &      sel_crit(nch).ne.0.and.
     &      sel_crit(nch).ne.3.and.sel_crit(nch).ne.4) then
          write(*,*) ' wrong phase selection, has to be 0-4 '
          stop
        endif
        if (seiclen(line).gt.41)
     &    read(line(41:50),*) time_window(nch)
        if (filterflag) then
          if (seiclen(line).gt.51)
     &      read(line(51:60),*) filter_low(nch)
          if (seiclen(line).gt.61)
     &      read(line(61:70),*) filter_high(nch)
        endif
      endif
15    continue
      goto 10

20    continue

c 
c add sfile names from index file
c
      if (seiclen(indexfile).gt.0) then
        call sei open( old$,               ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   indexfile,        ! Filename.
     &                   inu,              ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )
25      continue
        read(inu,'(a80)',end=30) line
        if (seiclen(line).le.1) goto 30   ! moved, buf fix 3/3/2016
        nevent=nevent+1
        if (nevent.ge.maxe) then
          write(*,*) ' too many stations in corr.inp ' 
          stop
        endif
        read(line(8:seiclen(line)),'(a80)') sfile2(nevent)
        goto 25
30      continue
        call sei close(close$,inu,code)
      endif
c
c sort event array, sfile2(nevent)
c
      do i=1,nevent
        temp(i)=sfile2(i)(14:19)//sfile2(i)(1:13)
      enddo
      call sortfi(temp,nevent)
      do i=1,nevent
        sfile2(i)=temp(i)(7:19)//temp(i)(1:6)
      enddo

c
c if no master event, assume correlation between all events
c
      if (seiclen(sfile1(1)).le.0.and.nevent.gt.0) then
        mode=2
        do i=1,nevent
          sfile1(i)=sfile2(i)
        enddo
      endif
      if (mode.eq.1) maxevdist=0.
      if (mode.eq.1) maxstdist=0.
      if (nmaster.gt.1.) then
        write(*,*) ' too many master events, stop '
        stop
      endif

c
c keep original waveforms if no output files
c
      if (wave_corr_option.eq.0.) wave_name_option=0.
 
      return
      end


      subroutine correlation(y1,nsamp1,y2,nsamp2,yout,
     &    energy,amplitude,nsampout)
c
c compute cross-correlation function
c
      implicit none
      include 'seidim.inc'
      real y1(*),y2(*),yout(*),energy(*),amplitude(*)
      real y2work(max_sample)
      real max1,max2,option
      double precision scale,a1,a2
      logical gap,flag
      integer nsamp1,nsamp2,nsampout,i,j,k,l,n,ndc,rdc 

      scale=0.
      max1=1.
      ndc=0
      a1=0.
      do j=1,nsamp1
        a1=a1+y1(j)**2
      enddo
      write(*,*) nsamp2
      do i=1,nsamp2
        max2=1.
        energy(i)=0.
        amplitude(i)=0.
          a2=0.
          n=0
          do k=i,i-1+nsamp1
            n=n+1
            if (k.le.nsamp2) then
              y2work(n)=y2(k)
            else
              y2work(n)=0.
            endif
          enddo
c remove dc
          call remove_dc(y2work,ndc,rdc,nsamp1)
          do k=1,nsamp1
              a2=a2+y2work(k)**2
              if (abs(y2work(k)).gt.max2) max2=abs(y2work(k))
c compute signal energy, assuming signal has no dc offset
              energy(i)=energy(i)+y2work(k)**2
          enddo
        amplitude(i)=max2
        yout(i)=0.
        do j=1,nsamp1
          if (i-1+j.le.nsamp2) then
            yout(i)=yout(i)+y2work(j)*y1(j)
          endif
        enddo
c normalise so that maximum of auto-correlation is 1
        yout(i)=yout(i)/sqrt(a1*a2)
        if (abs(yout(i)).gt.1e7) yout(i)=0.
c
c check for gap! by checking if next 10 samples constant
c        gap=.false.
c        if (i.lt.nsamp2-10) then
c          flag=.true.
c          do j=i+1,i+10
c            write(26,*) j,y2(j)
c            if (flag.and.(y2(j).eq.y2(j-1))) then
cc same value
c            else 
c              flag=.false.
c            endif
c          enddo
c          if (flag) gap=.true.
c            write(*,*) j,y2work(j)
c        endif
c        if (gap) then 
c          write(26,*) ' gap found '
c          yout(i)=0.
c          stop
c        else 
c          write(26,*) ' NO gap '
c        endif
      enddo
      nsampout=nsamp2
      return
      end


      subroutine resample_linear(y,nsamp,max,rate)
      implicit none
      real y(*),rate
      integer nsamp,n,i,c,max

      n=2
      rate=rate*2.

      if (n.eq.1) return
      if (nsamp*n.ge.max) then
        write(*,*) ' too many samples for resample '
        stop
      endif
c
c first spread samples over array
c
      c=nsamp
      do i=(nsamp-1)*n+1,n+1,-n
        y(i)=y(c)
        y(c)=0.
        c=c-1
      enddo
      nsamp=(nsamp-1)*n+1
c      do i=1,nsamp
c        write(*,*) y(i)
c      enddo

      do i=n,nsamp-1,n
        y(i)=(y(i-1)+y(i+1))/2.
      enddo

      return
      end

      subroutine get_model(nmoho,nconrad)
c
c read test parameters and model from STATION0.HYP
c
      implicit none
      include 'hypparm.inc'
      character*80 infile
      character*80 indat,testdat
      character*1 reff,ucase
      character*4 stat
      logical exist
      character*80 top_directory
      character*1 dchar
      real testj
      integer seiclen,j,iustat,i
      integer nmoho,nconrad
c
c get directory structure
c
      call topdir(top_directory)
      call dir_char(dchar)         ! dirctory delimiter character

      infile='STATION0.HYP'
      inquire(file=infile, exist=exist)

      if (.not.exist) then
         infile = top_directory(1:seiclen(top_directory)) // dchar // 
     &         'DAT' // dchar //
     &         infile(1:seiclen(infile))
         inquire(file=infile, exist=exist)
         if (.not.exist) then
           write(*,*) 'station file does not exist !'
           stop
         endif
      endif
      open(9,file=infile)
      iustat=9         
c    set test parameter defaults
      call settest(test)

c    read test parameter changes and station data
        stat='xxxx'
        j=-1
        
      do while (stat.ne. '    '.and.j.ne.0)
         read(iustat,'(a80)')testdat
         if(testdat(14:14).eq.')')then
          read(testdat,'(a4,t12,i2,t16,f9.4)')stat,j,testj
         elseif(testdat(13:13).eq.')')then
          read(testdat,'(a4,t12,i1,t15,f9.4)')stat,j,testj
         else
          read(testdat,'(a4)')stat
          j=-1
         endif
         if(j.gt.0)then
          test(j)=testj
         endif
      end do

      indat='XXXXXXXX'
      do while (indat(1:8).ne.'        ')
       read(9,'(a80)')indat
      end do

c read the velocity model
      i=1
      iustat=9
        
c    reff is used to specify the moho layer for PN calculation
5     read(iustat,101,end=99)v(i),d(i),vs(i),reff
101   format(3f7.3,a1)
      if(ucase(reff).eq.'N')nmoho=i

c 4/94: added nconrad variable
      if(ucase(reff).eq.'B')nconrad=i
        
        if(v(i).eq.0.0)go to 6
        i=i+1
        go to 5

c    nl is the number of layers in the velocity model
6       nl=i-1
      
c    read in trial depth and vp/vs ratio
        read(iustat,'(3f5.0,f5.2)',end=99)ztr,xxnear,xxfar,pos

c    if vs(1)=0 then set vs(i)=v(i)/pos
        if(vs(1).eq.0.0)then
          do i=1,nl
            vs(i)=v(i)/pos
          end do
        endif

c  store thicknesses in parm
        do i=1,nl-1
          parm(nl+i)=d(i+1)-d(i)

c    change 10/93: add maximum elevation to upper layer thickness
c    if test(40)=0.0 Need this because dtdx2 origin is always at maxelv
c          if(i.eq.1.and.test(40).eq.0.0)then
c            parm(nl+i)=d(i+1)-d(i)
c          endif
        end do
        do i=1,nl
          parm(i)=v(i)
        end do
        
99    continue
      close(9)
      close(10)
      end
      

      subroutine corr_ttmin(nmoho,nconrad,dist,depth,veloc,tmin)
      include 'hypparm.inc'
      character*80 indat,testdat
      character*1 reff,ucase,prmd,prm2
      character*8 phsid
      real veloc(*)
      real rearth,degtorad,dist,depth,stel
      integer seiclen,nmoho,nconrad

c settings as in hypocent
      degtorad=0.0174533
      rearth=6371.
      minflag=int(test(63))
        
c    xs(1), xs(2) are the station long. and lat.
      x0(1)=0.0
      x0(2)=0.0
      x0(3)=0.0
      
      prmd=' '                        
      prm2=' '

      xh(1)=dist/rearth
      xh(2)=0.
      xh(3)=depth

      do i=1,nl
        parm(i)=veloc(i)
      enddo

      call dtdx2(xh,x0,prmd,nmoho,nconrad,iulst,tmin,
     &  dx,delta,ann,iflag,phsid)
      if(iflag.eq.0)tmin=0.0
      return 
      end

      
