      program autosig  

c
c input files
c
c   1 - one waveform file
c   2 - s-file and 1 waveform file
c   3 - s-file, multiple waveform files
c

c 
c input data 
c
c   1 - no location, no phase readings  -> find phase arrival
c   2 - location, no phase readings     -> compute phase arrivals, or force 1
c   3 - phase readings                  -> use phase arrivals, or force 1/2 
c

c
c input_mode
c
c   1 - input file is Nordic file, name given through environmental variable
c   2 - S-file given through environmental variable, used by seisnet
c   3 - input is Nordic file
c   4 - continuous data

c
c
c functions
c
c   1 - pick phase based on sta/lta
c   2 - check if spectral level of signal is above noise at its maximum
c   3 - determine type (L,R,D,N) based on spectral levels at different 
c       frequencies
c   4 - determine spectral flat level and corner frequency based on 
c       spectrum of P or S, if distance known to correct for geom spr and Q,
c       using genetic algorithm
c ( 5 - read amplitude for Ml and Mb )
c ( 6 - location )
c

c
c by Lars Ottemoeller, Dec 1999
c
c changes:
c   08.02.2000 lo: use file_ready
c   09.02.2000 lo: if no detrigger use last sample for coda
c   10.02.2000 lo: put event id 'R'
c   23.05.2000 lo: use new wav reading routines
c   23.05.2000 lo: compute spectrum, new routine auto_moment
c   14.02.2001 lo: change in sta_lta_nrec, at BGS
c   15.02.2001 lo: add call to get_seisan_def, at BGS
c   21.02.2001 lo: fix call to spectrum 
c                  taken out mulplt.inc
c   05.03.2001 lo: add routine to determine type of event from spectrum
c   08.03.2001 lo: fixes in sta_lta_nrec and find_coda
c   14.03.2001 lo: add genetic algortihm to determine om and f0 from spectrum
c -----------------------------------------
c   20.03.2001 lo: name of program changed to autosig
c   21.03.2001 lo: changed variable odata to raw_signal
c   23.03.2001 lo: add correction for near surface attenuation using kappa
c   30.03.2001 lo: add get_amp_per, auto amplitude routine
c   20.06.2001 lo: added function to use const Q for long periods
c   03.07.2001 lo: removed call to dtime
c   23.09.2002 lo: add all phase output
c   11.11.2002 lo: fix bug with no output for waveform file input
c   11.11.2002 lo: add arguments
c   13.11.2002 lo: bug fix on input_mode
c   16.06.2003 lo: add reading of continuous data, not yet finished
c   24.05.2005 lo: put in recent changes 
c      07.2008 lo: some changes, compute spec average
c   22.02.2009 lo: search for delta kappa as well
c                  change to finding low frequency
c   12.03.2010 jh: add poles and zeros to subroutine get_amp_per,
c                  currently not used, also added to get_om_f0,
c                  freq_event_type
c   22 03 2013 jh: add wav_mem_init, remeove msdif, proc_time and other unnused pars in
c                  call to get_om_f0, comment out run of expect script
c   22.05.2013 lo: added plotunit, but no plotting yet
c   23 12 2015 jh: add test to call update_spec, only test(116) is 
c                  used and set locally since normally read form
c                    STATION0.HYP
c

c on Linux routines auto_moment and invert_spectrum cause problems
c

      implicit none

C    Seisan library inserts and routines...
C    ======================================
C
      include 'libsei.inc'                ! Seisan definitions
      include 'seidim.inc'                ! ------------------
      include 'seisan.inc'                ! ------------------
      include 'waveform.inc'              ! ------------------
      include 'autosig.inc'               ! autosig include file
      include 'rea.inc'                   ! rea include file

      external computer_type              ! Get platform type.
      external sei open,                  ! Open file routine.
     &         sei close,                 ! Close file routine.
     &         sei get file               ! Find files in diff dirs
      integer sei clen                    ! Length of string
      integer r_index
      real test(200)                       ! reset test form hyp
      character*1 dchar
      integer  write01,write02,err_unit,! File units,    
     &         read01,read02,code       ! Local condition.
      logical  b_flag                   ! Flag end of file
c --------------------------------------------------------------

      character*80 infile,outfile,autosig_out, ! file names
     &                outf,xfile,autosig_err
      integer*4 idata(max_sample)       ! integer data
      real raw_signal(max_sample),fil_signal(max_sample) 
                                        ! original and filtered data
      integer it                        ! trace counter
      character*80 fheader(max_trace)   ! file headers
      character*1040 theader            ! trace header
      integer c,i,x,z,j,y,ind           ! counters 
      integer coda(max_trace)           ! coda duration
      double precision drate            ! rate as double prec.
      character*80 line,text1,text2     ! some text line
      character*80 sfile                ! name of sfile
      character*80 question             ! text
      character*1 cbyte(max_trace)      ! 2 or 4 byte
      character*5 net_code              ! network code
      character*29 mainhead_text        ! mainheader text
      double precision phase_time(max_phase) ! phase times
      double precision msec,msec2
      character*80 aphases(max_phase)   ! phase array
      real sta,lta,ratio,mincoda,dtrle,fill,filh ! variables from parameter file
                                        ! for individual stations
      integer in                        ! 1 if filenr.lis
      integer filec                     ! file counter
      real      cof(8)                  ! filter coefficients
      real      gain                    ! filter gain
      integer npasses                   ! number of passes, 1 forward, 
                                        ! 2 both ways
      real fsn1,fsn2                    ! spectral band with good s/n
      character*80      data(max_data)  ! event arrays
      integer           nstat           ! no of stations for event
c      integer nphase                    ! no of phases for one event
c      integer           nrecord         ! no of records for event
c      integer           nhead           ! no of headers for event
      character*1       exp,evid        ! event ids--------------
      integer id                        ! event id line number
      character*2       fr_type         ! event type based on spectrum
      integer dist_cnt,local_cnt,reg_cnt! counter for type of event
      real om,f0,omi,f0i                ! flat level and corner frequency
      real ka                           ! computed delta kappa
      integer nspec                     ! number of spectra
c      real spfmin,spfmax                ! frequnecy range in which signal above noise
      logical clipped                   ! true if signal clipped
      logical lfreeze                   ! true if to freeze lta at trigger
      integer nfiles                    ! number of files
      real duration                     ! signal duration
      integer phase_index               ! index to phase in rea structure
      integer spec_index                ! index to spec phase
      integer jday                      ! julian day
      logical hyp_flag                  ! true if hypocenter known
      real slat,slon,sheight,sdist      ! station coords and distance
      real degtorad                     ! constant to convert degree to radians
      real distdeg,az0,xx(4),yy
      double precision abs_time
      character*1 phase
      character*1 which_phase
      real geo_corr
      integer trace_unit,event_unit,    ! file units
     &   mom_unit,f0_unit,mag_unit,res_unit,plotunit
      real res_sum

      real proc_count,time
      
      real amp,per
      real mag_sum,mom_sum,mag_save(100)
      character*80 data_save(max_data)
      integer nrecord_save
      logical event_done  
      integer mag_n
      real res_m_a(2)
      double precision time_amp
      real pi
      real max_dist,min_dist
      integer mag_ml_index
      parameter (pi=3.141592654)
      character*12 time1                      ! time strings
      character*14 time2
      integer narg                            ! number of arguments
      character*80 arg(10)                    ! arguments
      logical onlyonce                        ! true if input file given on prompt or
                                              ! from env variable
      real cmin,rsec
      integer year,month,day,doy,hour,min,sec
      character*14 end_time
      logical flag
      character*80 file_name
      character*3 agency
     
c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      test(116)=0.0      ! average mags, set 1.0 for median
c
c   get arguments
c
      call get_arguments(narg,arg)


      call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'autosig.trace',  ! Filename.
     &                   trace_unit,       ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      write(trace_unit,*) ' AUTOSIG TRACE FILE '

      call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'autosig.event',  ! Filename.
     &                   event_unit,       ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'moment.comp',    ! Filename.
     &                   mom_unit,         ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'f0.comp',        ! Filename.
     &                   f0_unit,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'mag.comp',       ! Filename.
     &                   mag_unit,         ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'res.comp',       ! Filename.
     &                   res_unit,         ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      call sei open( unknown$,             ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'autosig.eps',    ! Filename.
     &                   plotunit,        ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

c
c get parameters from parameter file
c
      call get_autosig_par(trace_unit)
      call get_seisan_def
      call dir_char(dchar)
      call get_agency(agency)

c
c init variables
c
      infile = ' '
      outfile='autosig.wav.001'
      autosig_out ='autosig.out'
      autosig_err ='autosig.err'
      filec = 0
      npasses = 1
      clipped=.FALSE.
      input_mode = 1             ! default waveform data

      degtorad=0.0174533
      proc_count=0.
      res_sum=0.
      max_dist=-1.
      min_dist=99999.
      in=0
      onlyonce=.false.
      cwav=.false.

c
c overwrite settings if set as arguments
c
      do i=1,narg
        if (arg(i)(1:5).eq.'-spec') then
          if (arg(i+1)(1:2).eq.'on') then
            lautospec=.true. 
          else 
            lautospec=.false.
          endif
        elseif (arg(i)(1:6).eq.'-clear') then
          if (arg(i+1)(1:2).eq.'on') then
            lclear=.true.
          else 
            lclear=.false.
          endif
        elseif (arg(i)(1:6).eq.'-phase') then
          if (arg(i+1)(1:2).eq.'on') then
            lautophase=.true.
          else 
            lautophase=.false.
          endif
        elseif (arg(i)(1:7).eq.'-infile') then
          infile=arg(i+1)(1:seiclen(arg(i+1)))
        elseif (arg(i)(1:5).eq.'-help') then
          write(*,*) 'autosig [-infile <filename> -spec on/off '//
     &               '-phase on/off -clear on/off' 
        endif
      enddo

c
c open error output file
c
        call sei open( unknown$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   autosig_err,      ! Filename.
     &                   err_unit,         ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.


      line=' '   
c
c check if started from other program
c
      call get_message('autosig',line)

40    continue 

c
c in case autosig is started from other program
c
      if (line(1:7).eq.'autosig'.and.line(9:9).ne.' ') then

          call get_env_event(sfile)
c
c read sfile
c
c          call file_ready(sfile)
          call sei open( old$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   sfile,            ! Filename.
     &                   write02,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
          call rea_event_in(write02,.true.,data,code)
          if (code.ne.0) then
            write(*,*) ' error reading file '
            call sei close( close$, write02, code)
            stop
          endif
          call sei close( close$, write02, code)
          read(line(9:79),'(a73)') infile
          input_mode = 2
          onlyonce=.true.
          lwavout= .FALSE.
      elseif (infile.ne.' '.and.in.ne.1) then
        onlyonce=.true.
      else
c
c if autosig not started from other program read input file name
c
        question=' Filename or number, filenr.lis for all, or cont'
        call filename(question,infile)
      endif

      if (infile(1:3).eq.'EOF') STOP
      if(infile(1:10).eq.'filenr.lis') then
c        call file_ready('filenr.lis')
        call sei open( old$,               ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'filenr.lis',     ! Filename.
     &                   read02,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
        in=1
      elseif (infile(1:4).eq.'cont') then
        if (n_cont_base.eq.0) then
           write(*,*) ' no continuous database defined in SEISAN.DEF '
           stop
        endif
c
c write out the database names
c
        write(*,*)'Databases are:'
        do i=1,n_cont_base
           write(*,*)'    ',cont_base(i)
        enddo
        write(*,*) ' Start time (yyyymmddhhmmss) '
        read(5,'(a)')  cwav_start_time
        write(*,*) ' End time (yyyymmddhhmmss) '
        read(5,'(a)')  end_time
        write(*,*) ' Time interval in minutes '
        read(5,*) cmin
c
c set interval length
c
        cont_interval=cmin*60.
c
c set end time for continuous data
c
        read(cwav_start_time,'(i4,4i2,f2.0)') year,month,day,
     &    hour,min,rsec
        call timsec(year,month,day,hour,min,rsec,msec)
        msec=msec+cont_interval
        call sectim(msec,year,doy,month,day,hour,min,rsec)
        write(cwav_end_time,'(i4.0,5i2.0)') 
     &    year,month,day,hour,min,int(rsec)
        lwavout=.false.
        lautoamp=.false.
        lautospec=.false.
        lall_phases=.true.
        onlyonce=.true.
        input_mode=4
        cwav=.true.
        call cwav_time_limits(0)
      endif

c
c open ascii output
c
      if (input_mode.ne.2) then
        call sei open( unknown$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   autosig_out,      ! Filename.
     &                   write02,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
       elseif (input_mode.eq.2) then
         call sei open( old$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   sfile,            ! Filename.
     &                   write02,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
      endif

50    continue

c
c if filenr.lis
c
      if(in.eq.1) then
        read(read02,'(7x,a)',end=40) infile
        if(infile(1:4).eq.'    ') stop
        filec=filec+1
        write(outfile(13:15),'(i3)') filec
        do i=13,15
          if (outfile(i:i).eq.' ') outfile(i:i)='0'
        enddo 
      endif
c
c open input file
c
      if (input_mode.ne.4) then
        call wav_init
        call wav_mem_init
        wav_filename(1)=' '
        wav_filename(1)=infile
c
c   now read first header for one event
c
        call get_full_wav_name(infile, wav_filename(1))
        write(*,*) ' INPUT FILE : '//
     &      wav_filename(1)(1:seiclen(wav_filename(1)))
        if (lwavout) then
          write(*,*) ' OUTPUT FILE: '//outfile(1:seiclen(outfile))
        endif
        call read_wav_header(1)
      else 
        wav_error_message=' '
      endif
60    continue

      if (cwav) then
        rea_nwav=0
        n_cont_trace=0
      endif

      if (input_mode.eq.3) wav_error_message='MODE 3'
      write(*,*) ' wav_error_message= '//wav_error_message

      if(wav_error_message.ne.' ') then
c
c file could be nordic
c 
        if (input_mode.ne.3) then
c
c open when checking 1st time
c
          call sei open( old$,           ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   infile,           ! Filename.
     &                   read01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
        endif

        nspec=0
        mag_n=0
        mag_sum=0.
        mom_sum=0.
        mag_ml_index=0

        call rea_event_in(read01,.true.,data,code)
        write(*,*) data(1)
c
c save data vector
c
        do i=1,rea_nrecord
          data_save(i)=data(i)
        enddo
        nrecord_save=rea_nrecord

        if ((rea_nrecord.eq.0.or.code.ne.0).and.input_mode.eq.3) then
          call sei close( close$, read01, code)
          goto 9999
        endif
        onlyonce=.true.
        if (rea_nrecord.eq.0.and.in.ne.1) then
          write(6,'(1x,a)') wav_error_message
          goto 9999
        else
          input_mode=3
          write(*,*) ' Nordic input file ... '
c
c read hyp parameters
c

          hyp_flag=.false.
          if (hyp_lat(1).ne.0..or.
     &        hyp_lon(1).ne.0..or.
     &        hyp_depth(1).ne.0.) hyp_flag=.true.

          call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &       hyp_hour(1),hyp_min(1),hyp_sec(1),abs_time)
c     write(*,*) ' lat/lon/dep ',hyp_lat(1),hyp_lon(1),hyp_depth(1)
          j=0

c
c find index to ml value
c
          do i=1,6
            if (hyp_mag_type(i,1).eq.'L') then
              mag_ml_index=i
            endif
          enddo

c
c get waveform filenames
c
          call wav_init
          nfiles=0
          wav_nfiles=0
          do i=1,rea_nwav 
            xfile=' '
            xfile=rea_wav(i)(2:79)
            xfile=xfile(1:seiclen(xfile))
            call get_full_wav_name(xfile,wav_filename(i))
          enddo
c
c read headers
c
          do i=1,rea_nwav
            call read_wav_header(i)
            if(wav_error_message.ne.' ') then
               write(6,'(1x,a)') wav_error_message
            endif
          enddo
        endif

        if (input_mode.eq.1) then
          if (in.eq.1) goto 50
          goto 9999
        endif

c
c file is waveform file, set content for output file
c
      elseif (input_mode.eq.1) then
        input_mode=1
        hyp_flag=.false.
c        hyp_fix_org(1)=' '
c        hyp_depth_flag(1)=' '
        call rea_hyp_clear(1)
        hyp_lat(1)=0.
        hyp_lon(1)=0.
        hyp_depth(1)=0.
        hyp_year(1)=wav_year(1)
        hyp_month(1)=wav_month(1)
        hyp_day(1)=wav_day(1)
        hyp_hour(1)=wav_hour(1)
        hyp_min(1)=wav_min(1)
        hyp_sec(1)=wav_sec(1)
        rea_nphase=0
        rea_nhyp=1

        rea_nwav=1
        rea_wav(1)='                                        '//
     &       '                                        '
        write(rea_wav(1)(2:),'(a)') infile(1:seiclen(infile))
        rea_wav(1)(80:80)='6'
        call systime(time1,time2) 
        rea_id_line=' ACTION:ASI '//time2//' OP:     '//
     &       'STATUS:               ID:20021001025954     I'
cACTION:REE 02-10-01 10:02 OP:gdf  STATUS:               ID:20021001025954
        write(rea_id_line(61:74),'(i4.4,5i2.2)')
     &     wav_year(1),wav_month(1),wav_day(1),wav_hour(1),wav_min(1),
     &     int(wav_sec(1)) 

      elseif (input_mode.eq.4) then
        call cwav_time_limits(0)
        call cwav_read_bases
        wav_nchan=n_cont_trace

        hyp_flag=.false.
        call rea_hyp_clear(1)
        hyp_lat(1)=0.
        hyp_lon(1)=0.
        hyp_depth(1)=0.
        read(cwav_start_time,'(i4,4i2,f2.0)') hyp_year(1),hyp_month(1),
     &    hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1)
        rea_nphase=0
        rea_nhyp=1

c        rea_nwav=1
c        rea_wav(1)='                                        '//
c     &       '                                        '
c        write(rea_wav(1)(2:),'(a)') infile(1:seiclen(infile))
c        rea_wav(1)(80:80)='6'
        call systime(time1,time2)
        rea_id_line=' ACTION:ASI '//time2//' OP:     '//
     &       'STATUS:               ID:20021001025954     I'
        write(rea_id_line(61:74),'(i4.4,5i2.2)')
     &     wav_year(1),wav_month(1),wav_day(1),wav_hour(1),wav_min(1),
     &     int(wav_sec(1))
      endif

c
c clear out phases if set
c
      if (lclear) then
        rea_nphase=0
      endif

c
c open output files
c 
      if (wav_nchan.ge.100) then
        lwavout= .FALSE.
        write(*,*) ' too many traces to create output file '
      endif

c waveform output
      if (lwavout) then
            chr_f_form$ = 'unformatted'
c            call file_ready(outfile)
            call sei open( unknown$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
      endif


c
c number of output traces is twice the input traces, since trigger trace is added
c
      wav_out_nchan = wav_nchan * 2
      write(*,*) ' number of traces: ',wav_nchan

c
c read trace header(s) to make new file header
c
      dist_cnt=0
      local_cnt=0
      reg_cnt=0
      if (input_mode.eq.1) nspec = 0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c start loop over all traces
c
      do i=1,wav_nchan

c
c original trace is c
c
c
c sta/lta ratio signal is index x
c
        if (lwavout) then      ! double traces if cretae output file
          c=i*2 -1
          x=c+1
        else
          c=i
          x=i
        endif
        it=c
c
c set output variables
c
        wav_out_stat(c)=wav_stat(i)
        wav_out_stat(x)=wav_stat(i)
        wav_out_comp(c)=wav_comp(i)
        wav_out_comp(x)=wav_comp(i)

        wav_out_year(c)=wav_year(i)
        wav_out_year(x)=wav_year(i)
        wav_out_month(c)=wav_month(i)
        wav_out_month(x)=wav_month(i)
        wav_out_day(c)=wav_day(i)
        wav_out_day(x)=wav_day(i)
        wav_out_hour(c) = wav_hour(i)
        wav_out_hour(x) = wav_hour(i)
        wav_out_min(c) = wav_min(i)
        wav_out_min(x) = wav_min(i)
        wav_out_sec(c)= wav_sec(i)
        wav_out_sec(x)= wav_sec(i)
        wav_out_nsamp(c)= wav_nsamp(i)
        wav_out_nsamp(x)= wav_nsamp(i)
        wav_out_rate(c)=wav_rate(i)
        wav_out_rate(x)=wav_rate(i)

        cbyte(x)='4'
        cbyte(c)='4'

c
c set waveform names if continuous data
c
        if (cwav) then
          do j=1,cwav_nseg(i)
            ind=0
            do z=seiclen(cwav_filename(i,j)),1,-1
              if (cwav_filename(i,j)(z:z).eq.dchar.and.
     &            ind.eq.0) ind=z
            enddo
            file_name=cwav_filename(i,j)
     &          (ind+1:seiclen(cwav_filename(i,j)))
            flag=.true.
            do c=1,rea_nwav
              if (rea_wav(c)(2:seiclen(file_name)+1).eq.file_name) 
     &            flag=.false.
            enddo  
            if (flag) then
              rea_nwav=rea_nwav+1
              rea_wav(rea_nwav)=
     &          '                                        '//
     &          '                                       6'
              write(rea_wav(rea_nwav)(2:seiclen(file_name)+1),'(a)') 
     &           file_name(1:seiclen(file_name))
            endif
          enddo
        endif
      enddo
  
c
c   make seisan headers
c
      if (lwavout) then
        call sheads(wav_out_year,wav_out_month,wav_out_day,
     *    wav_out_hour,wav_out_min,wav_out_sec,wav_out_nchan,c,
     *    net_code,mainhead_text,wav_out_stat,wav_out_comp,
     *    wav_out_nsamp,wav_out_rate,cbyte,
     *    outf,fheader,theader)
      endif
c
c write header
c
      if (lwavout) then
        x=int(((2*wav_nchan)-28)/3)
        if (x.lt.0) x=0
        do z=1,12+x
             write(write01) fheader(z)
        enddo
      endif

c
c delete files
c
      call systemc('rm -f *.obs',11)
      event_done=.false.

      do i=1,wav_nchan
c
c read response into common block
c
        wav_current_chan(1)=i

        if (lwavout) then      ! double traces if cretae output file
          c=i*2 -1
          x=c+1
        else
          c=i
          x=i
        endif

        write(*,*) 'trace: ',i,' ' // wav_stat(i) // wav_comp(i)
        it=c
c
c get parameters for station c
c
        call get_station(wav_out_stat(c),wav_out_comp(c),sta,lta,ratio,
     &      mincoda,dtrle,fill,filh,code)
 
        if (code.eq.1) then
c
c set flag that some station in parameter file
c
          event_done=.true.
          
c
c check if P or S
c
          phase=which_phase(phase_mode)
c
c use P for Q waves if set and P waves used
c
          if (phase.eq.'P') then
             par_q0=par_p_q0
             par_qalpha=par_p_qalpha
          else
             par_q0=par_s_q0
             par_qalpha=par_s_qalpha
          endif
c
c check for phase time
c
          phase_index=0
          if (phase_mode.eq.3..or.phase_mode.eq.4..or.
     &      phase_mode.eq.5.) then
            do j=1,rea_nphase
              if (rea_stat(j).eq.wav_stat(i).and.
     &            rea_phase(j)(1:4).ne.'SPEC') then
                if (rea_phase(j)(1:1).eq.phase(1:1).or.
c     &            rea_phase(j)(1:1).eq.phase(1:1).or.
     &            phase_mode.eq.5.) then
                  phase_index=j
                  phase=rea_phase(j)(1:1)
                  call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &             rea_hour(j),rea_min(j),rea_sec(j),rea_abs_time(j))
                endif
              endif
            enddo
c
c check that phase there if required
c
          elseif ((phase_mode.eq.1..or.phase_mode.eq.2.).and.
     &      phase_infile.eq.1.) then
            do j=1,rea_nphase
              if (rea_stat(j).eq.wav_stat(i).and.
     &            rea_phase(j)(1:4).ne.'SPEC') then
                  phase_index=j
              endif
            enddo
            if (phase_index.eq.0.) code=0
          endif

c
c check if spec already done and keep index
c
          spec_index=0
          do j=1,rea_nphase
            if (rea_stat(j).eq.wav_stat(i).and.
     &          rea_phase(j)(1:4).eq.'SPEC') then
              if (spec_index.eq.0) spec_index=j
            endif
          enddo

c
c read trace, signal stored in signal1
c
          call wav_read_channel(i)
          call read_resp
          if (wav_nsamp(i).gt.max_sample) code=0
        endif

        if (code.eq.1) then
          do z=1,wav_nsamp(i)
            idata(z)=int(signal1(z))
c
c save raw signal
c
            raw_signal(z)=signal1(z)
          enddo
        endif
c
c   make trace header
c
        if (lwavout) then
          call sheads(wav_out_year,wav_out_month,
     *      wav_out_day,wav_out_hour,
     *      wav_out_min,wav_out_sec,wav_out_nchan,c,
     *      net_code,mainhead_text,wav_out_stat,wav_out_comp,
     *      wav_out_nsamp,wav_out_rate,cbyte,
     *      outf,fheader,theader)

          write(write01) theader
          write(write01) (idata(z),z=1,wav_out_nsamp(c))
          call sheads(wav_out_year,wav_out_month,
     *      wav_out_day,wav_out_hour,
     *      wav_out_min,wav_out_sec,wav_out_nchan,x,
     *      net_code,mainhead_text,wav_out_stat,wav_out_comp,
     *      wav_out_nsamp,wav_out_rate,cbyte,
     *      outf,fheader,theader)

           write(write01) theader
        endif


        if (code.eq.1) then     ! station defined in parameter file

c
c compute distance if hypocenter given
c
          if (hyp_flag) then
            call stat_loc(wav_stat(i),data(1)(21:21),slat,slon,sheight)
            if (slat.eq.0..and.slon.eq.0..and.sheight.eq.0.) then
              write(*,*) ' location unknown for station: ' //
     &           wav_stat(i)
              sdist=0.
            else
              if (hyp_lat(1).ne.-999.) then
                xx(1)=slat*degtorad
                xx(2)=slon*degtorad
                xx(3)=hyp_lat(1)*degtorad
                xx(4)=hyp_lon(1)*degtorad
                call delaz(xx(1),xx(2),
     &             sdist,distdeg,az0,xx(3),xx(4)) 
                write(*,*) ' dist for station ' // wav_stat(i)
     &             // ' in km: ',  sdist
              else
                sdist=0.1
              endif
            endif
          endif

c
c calculations depending on which processes are activated in parameter file
c
          drate=wav_out_rate(c)        ! store rate as double precision

c
c remove linear trend or mean
c
          if (lrtr) then
            write(*,*) ' removing linear trend'
            call rtr(signal1,wav_out_nsamp(c),drate)
          else
            if (lrmean) then
              call rmean(signal1,wav_out_nsamp(c))
            endif  
          endif

c
c check if signal is clipped
c
          call check_clipped(signal1,wav_out_nsamp(c),clipped)
          if (clipped) write(*,*) ' *** signal clipped *** '

c
c  filter if not lower filter is -1.
c
          if (par_fill(c).ne.-1.) then

            write(*,*) 'filtering: ',fill,filh

c-- calc. filt. cof.
            call bndpas(fill,filh,1000.0/wav_out_rate(c),
     *              cof,gain)
c-- and filter
            call filter(signal1,wav_out_nsamp(c),cof,gain,npasses)
          endif

          do z=1,wav_out_nsamp(c)
            fil_signal(z)=signal1(z)
          enddo
c
c normalize signal to max of 1000
c
          call normalize_signal(signal1,wav_out_nsamp(c),1000.)

c
c compute characteristic function, y**2 + k * y'**2
          if (lchfu) then
             call char_fu(signal1,wav_out_nsamp(c),kchfu)
          endif
c
c compute sta/lta, ratio signal in signal1
c
 
          if (.not.lrec) then
            lfreeze=.true.
            call sta_lta_nrec(signal1,wav_out_nsamp(c),
     *        drate,sta,lta,lstasq,ratio,lfreeze)
          else
            call sta_lta_rec(signal1,wav_out_nsamp(c),
     *        drate,sta,lta,lstasq)
          endif
c
c find phases, ratio given in signal1, spectral analysis to check if signal done
c on raw_signal
c
          duration=0.
          if (lautophase.or.
     &        phase_mode.eq.0..or.      
     &     (phase_mode.ge.3..and.phase_index.eq.0.and.lautophase)) then 

           lautoamp=.false.

            call find_phases(i,signal1,raw_signal,wav_out_nsamp(c),
     &           drate,sta,ratio,dtrle,wav_abs_time(i),
     &           aphases,phase_time,dist_f_sel,fr_type,
     &           spike_test)

c
c loop over all phases
c
            y=1
            if (lall_phases) y=max_phase
            do z=1,y
c
c find coda on filtered signal
c
              call find_coda(fil_signal,wav_out_nsamp(c),drate,
     &           wav_abs_time(i),sta,
     &           dtrle,phase_time(z),coda(z),lautocodasq)
c
c write phases to array, signal has to either above noise spectrum or longer
c than minimum coda duration
c
c            if (coda(1).gt.mincoda.or.fr_type(1:1).ne.'N') then
       
              if (coda(z).ge.mincoda.and.phase_time(z).ne.0.) then
                if (aphases(z)(22:22).ne.' ') then
c
c new phase
c
                  lautoamp=.true.
                  rea_nphase=rea_nphase+1
                  phase_index=rea_nphase

                  call rea_phase_clear(phase_index)
                  rea_auto(phase_index)='autosig'
                  rea_stat(phase_index)=wav_out_stat(c)
                  rea_comp(phase_index)=wav_out_comp(c)
                  rea_co(phase_index)(1:2)=wav_out_comp(c)(1:1)//
     &               wav_out_comp(c)(4:4) 
                  rea_phase(phase_index)='P'
                  phase(1:1)='P'
                  rea_onset(phase_index)='E'
                  rea_weight_out(phase_index)='  '
                  rea_weight_in(phase_index)=' '
                  rea_polarity(phase_index)=' '
                  call sectim(phase_time(z),rea_year(phase_index),
     &     jday,rea_month(phase_index),rea_day(phase_index),
     &     rea_hour(phase_index),rea_min(phase_index),
     &     rea_sec(phase_index))
                  if (coda(z).gt.0..and.coda(z).lt.10000) then
                    rea_coda(phase_index)=coda(z)
                  endif
                endif
              endif
            enddo
c
c set duration either for P or S, as given in parameter file
c
            if (sdist.ne.0..and.spec_dur_choice.eq.1.) then
              call gvel_to_time(sdist,phase_time(1),
     &          msec,duration)
            else
              duration=spectrum_length_p
            endif
          elseif (phase_mode.ge.3..and.
     &      phase_index.ne.0) then
c
c if phase to be taken from file
c
            phase_time(1) = rea_abs_time(phase_index)
            if (spec_dur_choice.eq.1.) then
              call gvel_to_time(sdist,rea_abs_time(phase_index),
     &          msec,duration)
            else
              if (which_phase(phase_mode).eq.'P') then 
                phase='P'
                duration=spectrum_length_p
              elseif (which_phase(phase_mode).eq.'S') then 
                phase='S'
                duration=spectrum_length_s
              endif
            endif
          elseif ((phase_mode.eq.1..or.phase_mode.eq.2.)
c     &       .and.phase_time(1).ne.0.) then   ! changed lot 20/03/2003
     &       ) then
c 
c set group velocity window
c
            rea_nphase=rea_nphase+1
            phase_index=rea_nphase
            phase=which_phase(phase_mode)
            call rea_phase_clear(phase_index)
            call gvel_to_time(sdist,abs_time,
     &          phase_time(1),duration)
                  rea_auto(phase_index)='autosig'
                  rea_stat(phase_index)=wav_out_stat(c)
                  rea_comp(phase_index)=wav_out_comp(c)
                  rea_co(phase_index)(1:2)=wav_out_comp(c)(1:1)//
     &               wav_out_comp(c)(4:4)
                  rea_phase(phase_index)='        '
                  rea_phase(phase_index)(1:1)=which_phase(phase_mode)
                  rea_onset(phase_index)='E'
                  rea_weight_out(phase_index)='  '
                  rea_weight_in(phase_index)=' '
c xxx
                  rea_coda(phase_index)=int(duration)
                  rea_dist(phase_index)=sdist
                  rea_polarity(phase_index)=' '
                  call sectim(phase_time(1),rea_year(phase_index),
     &     jday,rea_month(phase_index),rea_day(phase_index),
     &     rea_hour(phase_index),rea_min(phase_index),
     &     rea_sec(phase_index))
          endif
c       write(*,*) ' phase_mode, phase_time ',phase_mode,phase_time(1)

c
c check event type from frequency content
c          
c          write(*,*) ' xxx duration: ',duration 
          if (.not.clipped.and.duration.ne.0.) then
            call freq_event_type(raw_signal,i,dist_f_sel,
     &        phase_time(1),
     &        spectrum_pre_length,duration,fr_type)
            if (fr_type(1:1).eq.'D') then
              dist_cnt= dist_cnt+1
            elseif (fr_type(1:1).eq.'R'.or.fr_type(1:1).eq.'N') then
              reg_cnt = reg_cnt + 1
            else
              local_cnt= local_cnt+1
            endif
          endif

          if (.not.clipped.and.duration.ne.0.) then
c
c determine auto amplitude
c
            if (lautoamp.and.phase(1:1).eq.'S') then

              call get_amp_per(i,'WA',raw_signal,phase_time(1),
     &          duration,amp,per,time_amp)
c              write(*,*) ' xxx ',amp,per,time_amp
            endif
c
c add amp and per to common block, add to old phase or make new phase
c
            if (amp.ne.0..and.per.ne.0.) then
              if ((phase_index.eq.0.or.
     &          phase_mode.eq.1..or.phase_mode.eq.2.).and.
     &          time_amp.ne.0.) then  ! if new phase
                rea_nphase=rea_nphase+1
                phase_index=rea_nphase
                call rea_phase_clear(phase_index)
                rea_stat(phase_index)=wav_stat(i)
                rea_comp(phase_index)=wav_comp(i)
                rea_onset(phase_index)='E'
                call sectim(time_amp,rea_year(phase_index),
     &     jday,rea_month(phase_index),rea_day(phase_index),
     &     rea_hour(phase_index),rea_min(phase_index),
     &     rea_sec(phase_index))
              endif
              rea_auto(phase_index)='autosig'
              rea_amp(phase_index)=amp
              rea_per(phase_index)=per
            endif

c          write(*,*) ' xxx autospec ',lautospec
c
c determine spectral parameters, flat level and corner frequency
c
            if (lautospec) then
c
c set values from s-file to initial values, used to compare residual
c
              if (spec_index.gt.0) then
                omi=rea_omega0(spec_index)
                f0i=rea_cornerf(spec_index)
              else
                omi=0. 
                f0i=0.
              endif
              write(*,*) ' trying to match spectrum ... '
              call get_om_f0(raw_signal,i,spfmin,wav_rate(i)/2.5,
     &          phase_time(1),spectrum_pre_length,
     &          duration,om,f0,ka,omi,f0i,
     &          abs_time,par_q0,par_qalpha,qcorner,par_kappa,
     &          dkappa,
     &          ga_pop_size,ga_n_generations,
     &          ngrid_f,ngrid_om,grid_nloop,
     &          res_m_a,search_method,norm_fit,ffixl,ffixh,fsn1,fsn2)
            endif

            res_sum=res_sum+res_m_a(2)

            proc_count=proc_count+1.
         write(res_unit,*) proc_count,res_m_a(2)

c
c write results to common block
c
            if (om.ne.0..and.f0.ne.0.) then
              rea_nphase=rea_nphase+1
              phase_index=rea_nphase
              rea_stat(phase_index)=wav_stat(i)
              rea_comp(phase_index)=wav_comp(i)
              rea_auto(phase_index)='autosig'
              rea_phase(phase_index)= '     '
              rea_phase(phase_index)(1:4)='SPEC'
              rea_phase(phase_index)(5:5)=phase
c              rea_phase(phase_index)(5:5)=rea_phase(phase_index)(1:1)
c              rea_coda(phase_index)=int(duration)
              rea_omega0(phase_index)=om
              rea_cornerf(phase_index)=f0
              rea_vp(phase_index) = par_vp
              rea_vs(phase_index) = par_vs
              rea_q0(phase_index)=par_q0
              rea_qalpha(phase_index)=par_qalpha
              rea_kappa(phase_index)=par_kappa+ka ! changed lot 22/02/2009
              rea_density(phase_index)=par_density
              rea_swin(phase_index)=duration
c
c source dimension
c
              rea_radius(phase_index)=.37*par_vs/f0 ! changed to 0.36 apr 2013 jh
              call spec_dist(phase,sdist,hyp_depth(1),geo_corr)
              rea_geo_dist(phase_index)=1./geo_corr
              if (sdist.gt.max_dist) max_dist=sdist
              if (sdist.lt.min_dist) min_dist=sdist
c
c log moment
c
              rea_moment(phase_index)=4.*pi*
     &          rea_density(phase_index)*10.**rea_omega0(phase_index)
     &          * .833   ! kk
     &          * rea_geo_dist(phase_index)
     &          * (1000.**5)   ! to convert units of density and distance
     &          / (1.e9)       ! to convert to Nm
              if (rea_phase(phase_index)(5:5).eq.'P') then
                rea_moment(phase_index)=rea_moment(phase_index)*
     &            rea_vp(phase_index)**3
              else
                rea_moment(phase_index)=rea_moment(phase_index)*
     &            rea_vs(phase_index)**3
              endif
              rea_moment(phase_index)=log10(rea_moment(phase_index))
              rea_mw(phase_index)=2./3.*rea_moment(phase_index)
     &          -6.06
c
c xxx remove if loop later
c
       if (spec_index.ne.0) then
       if (rea_moment(spec_index).ne.0.) then
              mag_n=mag_n+1
              mag_sum=mag_sum+rea_mw(phase_index)
              mag_save(mag_n)=rea_mw(phase_index)
              mom_sum=mom_sum+rea_moment(phase_index)
c        write(*,*) mag_n,mag_sum,rea_mw(phase_index)
       endif
       endif
c
c stress drop
c
              rea_sdrop(phase_index)=(.44*10.**rea_moment(phase_index))
     &           / (1.e14*rea_radius(phase_index)**3)

c
c write out comparison
c
       if (rea_moment(spec_index).ne.0.) then
         write(mom_unit,*) rea_moment(spec_index),
     &      rea_moment(phase_index)
         write(f0_unit,*) rea_cornerf(spec_index),
     &      rea_cornerf(phase_index)
       endif

            endif
          endif

        else
          write(err_unit,*) ' station/comp ',wav_out_stat(c),' ',
     &        wav_out_comp(c), 
     &          ' not defined in autosig.par'
        endif


c
c write out data
c
        if (lwavout) then
          do z=1,wav_out_nsamp(c)
            idata(z)=int(signal1(z))
          enddo
          write(write01) (idata(z),z=1,wav_out_nsamp(c))
        endif

      enddo    ! end of loop over all traces
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if (mag_n.gt.0) then
c
c get variance
c
        do i=1,mag_n
          yy=yy+(mag_save(i)-mag_sum/float(mag_n))**2
        enddo
        yy=(1./float(mag_n-1)*yy)**.5

        write(mag_unit,*) rea_av_mw,mag_sum/float(mag_n),yy
        write(65,*) mom_sum/float(mag_n)
        if (mag_ml_index.ne.0) then
           write(66,*) hyp_mag(mag_ml_index,1),mag_sum/float(mag_n)
        endif
      endif

      if (dist_cnt.gt.local_cnt.and.dist_cnt.gt.reg_cnt) then
         fr_type='D ' 
         write(*,*) ' *************** Distant *********** '
      elseif (local_cnt.gt.dist_cnt.and.local_cnt.gt.reg_cnt) then
         fr_type='L ' 
         write(*,*) ' *************** Local ************* '
      else
         fr_type='R '
         write(*,*) ' *************** Regional ********** '
      endif
      if (input_mode.ne.2) then
c        data(1)(22:22)=fr_type(1:1)
         hyp_dist_id(1)=fr_type(1:1)
      endif

c
c compute averages and standard deviation and store in data
c
      call rea_event_out(0,.true.,data,code)  ! update array
      do i=1,rea_nhead
        write(*,*) ' deb autosig ',data(i)
      enddo
      call update_spec(data,rea_nrecord,rea_nhead,agency,test,.false.)

c
c write out nordic file
c
       do i=1,rea_nrecord
         write(write02,'(a)') data(i)
       enddo
c
c write out event if at least 1 channel processed
c
      if (event_done) then
        do i=1,nrecord_save
          write(event_unit,'(a80)') data_save(i)
        enddo
      endif
c
c call gmt script
c
c change 
      if (input_mode.ne.2.and.input_mode.ne.4) then
        call systemc('spec.exp',8)
      endif

      if (lwavout) then
        call sei close( close$, write01, code)
      endif
      write(*,*) ' min distance = ',min_dist
      write(*,*) ' max distance = ',max_dist
      if (input_mode.eq.3) then
        goto 60
      endif
      if (input_mode.eq.4) then
c
c calculate new start time
c
        read(cwav_start_time,'(i4,4i2,f2.0)') year,month,day,hour,
     &       min,rsec
        call timsec(year,month,day,hour,min,rsec,msec) 
        msec=msec+(cont_interval*.9)
        call sectim(msec,year,jday,month,day,hour,min,rsec)
        write(cwav_start_time,'(i4.4,4i2.2,i2.2)') 
     &     year,month,day,hour,min,int(sec)
        write(*,*) ' new start time: ',cwav_start_time

        read(end_time,'(i4,4i2,f2.0)') year,month,day,hour,
     &       min,sec
        call timsec(year,month,day,hour,min,rsec,msec2) 
        if (msec.ge.msec2) goto 9999
        goto 60
      endif
      if (in.eq.1) then
        goto 50
      endif

9999  continue
      write(*,*) ' residual ',
     &    res_sum/proc_count
      if (.not.onlyonce) then
        input_mode=1
        goto 40
      endif

c
c close files
c
      if (in.eq.1) call sei close( close$, read02, code)
      call sei close( close$, err_unit, code)
      call sei close( close$, trace_unit, code)
      call sei close( close$, event_unit, code)
      call sei close( close$, mag_unit, code)
      call sei close( close$, f0_unit, code)
      call sei close( close$, mom_unit, code)
      call sei close( close$, res_unit, code)
      call sei close( close$, write02, code)
      call close_post
      call sei close( close$, plotunit, code)
   


      stop
      end

