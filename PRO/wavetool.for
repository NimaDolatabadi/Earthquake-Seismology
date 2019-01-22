
c   - program to get waveform data from all waveform files in one s-file,
c     or a list ow waveform file in a file of filenr.lis format
c     the data  can be selected in verious ways like channels, times etc
c   - the information is written out to a file in the local directory 
c   - in seisweb mode, data is  written to a file specified
c     and at end a seisweb message written to screen
c   - output format is SEISAN, GSE and SAC
c   - program can also only give file info
c   - optinally points can be skipped so resampled files can also be made,
c     it is up to the user to use correct antialias filters
c   - filter option is a 4 poles butterworth running one way, there is currently
c     no check if the output after filtering is larger than one, except in SEISAN
c     format so funny results might occur in the write out
c
c     Filer name convention for output files:
c
c     In interactive mode, the output file name is SEISAN by default since
c                                there is no question about file name.
c
c     No output file name given: wavetool.out for SEISAN output file
c                                wavetool.out_SEED for SEED output file
c                                wavetool.out_MSEED for MSEED output file
c                                wavetool.out_GSE for GSE output file
c
c     Output file name given as SEISAN: 
c                                SEISAN name for SEISAN output file
c                                SEISAN_name_SEED for SEED output file
c                                SEISAN_name_MSEED for MSEED output file
c                                SEISAN_name_GSE for GSE output file
c
c     No output file name given and output format is SAC:
c                                SEISAN_name_component_SAC
c
c     Output name given:         That name is used exactly as specified. There
c                                if no check if file exists and there is no
c                                ending indicating format
c
c     Ouput of one channel files with SEISAN names: The channel code is added 
c                                to avoid overwrite of different channels 
c                                from the same station 
c
c     Output format is SEISAN and ouput file exists: _SEI is added to file name
c                                
c                                 
c
c   arguments:
c              -seisweb: use with seisweb, seisweb screen message
C
c              -chan_out_file: file name  of file with channel description
c                              if this option is given, program terminates
c                              after writing out the file
c              -wav_out_file: complete filename including path to wav output,
c                        if not set, output to extract.out 
c                        If set to SEISAN, a seisan standard output file name
c                        will be used, net code from SEISAN.DEF
c                        If set to MSEED, filename similar to SEISAN standard is created
c              -sfile  : s-filename with complete path
c              -eventdata <database> <station>: search for sfiles that contain data, then
c                        use sfile option to extract given station
c              -wav_files: list of waveform files, filenr.lis format
c                          overrides input from -sfile
c              -format : output format, SEISAN, GSECM6, GSEINT, MSEED, SEED 
c                        default is SEISAN
c              -maxpoints : skip points to get approximately maxpoints
c              -filter : bandpass filter limits 
c              -ground : compute displacement, velocity or acceleration (1,2,3)
c              -chansel: file with selection parameters, if -chansel not used,
c                        all channels selected with whole window.
c                        Format: First line gives number of channels, free form
c                                Each following line give channel #, start and
c                                duration of selected channels. if start is
c                                zero use first sample, if interval is zero,
c                                use whole interval, free format
c              -start  : (1) Start time relative earliest channnel,
c                        (2) Abs start time string yy...s.sss for all channels
c                            IF ABS time used, string MUST contain a '.'
c                        (3) Abs start time string yyyymmddhhmmss (integer),
c                            used to define cont start time
c              -duration:    length of all channels to select
c                             start and duration not used if chansel file given 
c                             -duration will not have an effect if -start
c                             is not used.
c
c              -ichan : selection of one channel
c              -npole n: number of poles used for filter, same for both, not used
c              -stat_out: write out station location file
c              -resp_out: write out list of response files
c              -cwav    : read from cont data base
c              -cseed   : interval from large seed volume
c              -arc     : bud or scp archive
c              -seed_location: seed location code NOT USED 
c              -wav_in_file: input waveform file
c              -interactive : enable interactive mode
c              -rsam: comput 1 min rsam data
c              -cbase: name of file with selected continuous databases, 
c                     all is default
c              -chandef file: give name of file with defintion of change of
c                     station and/or component code following seisan default
c
c   examples:
c  
c   wavetool -sfile sfile     
c           all channels selected from all files
c   wavetool -sfile sfile -start 100
c           start 100 secs after earliest channel, take all channels and all
c           remaining data
c   wavetool -sfile sfile -start 19950101202211.1 
c           same as above except now start time is in absolute time
c   wavetool -wav_in_file lsa.iris.big.seed -start 19990509000000 -duration 500 -cseed
c           extract from a seed volume
c
c
c changes:
c
c april 19, 00 lo fix gse output
c april 20, 00 lo add some arg options and fix gse output
c april 25     jh many changes
c may    3     lo routine signal_to_visual
c may    8  00 lo some fixes with the chansel input file
c may   10     jh set network code to MERGE if no code defined
c may   15     lo some smaller changes, bug net code(jh)
c may   25     bmt add ichan option (select 1 channel)
c oct   20     jh  merge_wav changed to 5 chars
c feb   01 01  lo ground motion output
c mar   05 01  lo use i3 in channel list
c mar   06 01  lo keep time of max and min in signal_to_visual
c may   01 01  lo started to extract from waveform files directly
c july  13 01  lo extract now also wave conversion, any input, any output
c nov   11 01  jh extract from cont base
c dec   03 01  bm clean chan_out_file when nothing is found
c              bm added new option "-command_file" for reading prompt arguments 
c                 from a file, one line per argument
c mar 14   02  jh comment out write(17
c april 30 02  lo seisan output always 4 byte
c may   8  02  bm multiply by 10 when output is GSE format (only seisweb)
c aug  22  02  lo added option -wav_in_file to give waveform file nam
c apr 9    03  jh put in gain in seisan output if filter or ground correction to
c                  make sure values are more than 1 count, make it possibele to filter
c                  also when no response removal, check if filtering is possible
c may 14   03  jh  add text to header when corrected output written
c jun 30   03  lo add rsam output
c sep 10   03  jh check value of sample in GSE format, if response, was sometimes too big
c apr      04  lo: some format fixing
c dec 2        jh: large seed volume
c mar 10   05  jh: add component to output file name if only one channel
c jun 20   05  jh: seed location code input
c feb 17   06  jh: change references to extract. NOTE: seisweb uses old 
c                  extract !!!!!!!
c mar 28   06  jh: bug with overwrite seisan file
c                  bugs in defining waveform file name etc pointed out
c                  by wayne crawford
c mar 30   06  lo: option to select continuous database, used from mulplt
c jan 17   07  jh: fix bug when reading seed files with time gap, if converting
c                  to seisan file, main header will be wrong, but channel
c                  header ok, to be fixed
c mar 14   07  lo: added seed output using gse2seed
c mar 21   07  lo: added option for dataless seed
c may 30   07  lo: use file to read cbase
c oct 24   07  jh: put in writing of location and networ in seisan and mseed
c                  remove old INPUT OF LOCATION CODE, should now come from
c                  input file
c dec 19   07  jh  fix probelm of wrong interval using selection out of a 
c                  seed file, cseed in common block
c aug 01   08  lo  set wav_interactive to true if filenr.lis
c aug 27   08  jh  put in more comments
c oct 23   08  jh  fix output for seisweb from chan_out_text
c april 17 09  jh  remove option s-file from option cseed, should not have
c                  been there and made cseed option not work
c jan 14   09  lo  use rea routine to read s-file for sac output
c dec 22   09  jh  partly put in option s-file for cseed, needed for mulplt 
c                  extract, not sure where it did not work before.
c mar 12   10  jh  add poles and zeros remove_resp call, not used
c mar 22   10  jh  alway check size of number when writing out seisan format
c apr 9    10  jh: small text change in question on skipping
c apr 15   10  jh: changed scaling in seisan. only scale seisan output files 
c                  if any one of the samples are smaller than 1 and larger 
c                  than 0
c oct      10  jh: bud reading
c nov            : arc reading
c feb 9    11  jh: call to remove_resp crashed since call was with a number
c                  for butterworth filters so values could not be returned
c                  made to be variables
c june 2011    jh: chad writing routines
c dec 13 2011  jh: wrong argument in above for writing
c jan 23 2012  jh: fix bug response removal. simplify filter and response removel
c                  now only bp filter allowerd, fixed 4 poles
c feb 29 2012  jh: taper and dc rem was not used if no filter
c dec 21 2012  jh: improve s-file detection
c jan 04 2013  jh: nrecord to auto_tr, remove reference to scp and bud, only arc
c feb 08 2013  jh: problem with file names if a different output format than seisan 
c                  was chosen from the prompt and a few more similar problems
c feb 22 2013  jh: more file name fixing
c dec 28 2013  jh: update of arc extract which did not work correctly in case of
c                  a subselexction of channels
c feb 6 2014   jh: change dim of c_base_select to 1000
c feb 182014   jh: make sure _SEI is not added to seisan file name if it
c                  exists if name has been given specifically
c mar 28 2014  lo: have option '-chandef file' to convert station and or channel names
c                  have SEISAN style filename when writing MSEED files and not using filenr.lis
c jun  6 2014  lo: bug fix with writing of station/channel names
c sep 18 2014  lo: bug fix with mseed output filename
c jun  1 2015  lo: set nodata flag if continuous and data not there
c oct 19 2015  lo: added functionality to use extract on event data
c                  similar to continuous data, database and station have 
c                  to be given, and wavetool looks at all possible files
c                  to select which individual waveform files gives 
c                  most data for the given request
c oct 27 2015  jh  add arc_start and arc_stop to cbase_select
c jan 13 2016  jh  delete message file  extract.mes on start up
c jan 25 2017  lo  call wav_mem_init between files when using filenr.lis

      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'waveform.inc'
      include 'rea.inc'
      include 'seisan.inc'
      integer seiclen
      integer nstat,nphase,nhead,nrecord,id   ! for reading s-file
      character*1 type,exp                    ! -----------------
      character*80 sfilname                   ! s-file name
      character*80 chan_out_file              ! file name for channel info
      character*80 data(max_data)             ! one s-file
      character*80 chansel                    ! file with selection
      integer npresent                        ! number of wav files present
      character*80 text                       ! general text string
      character*80 wav_out_file               ! full name of output file
      character*80 wav_files                  ! input files with wav f. names
      character*80 file_out                   ! output file name
      character*80 seed_file_out              ! output file name
      character*5  net_code                   ! network code, for output file name
      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments
      logical seisweb                         ! true if call from seisweb
      logical exist                           ! file existance
      logical small                           ! true if small samples, <1
      logical station_out_flag                ! true if write out station file
      logical resp_out_flag                   ! true if write out list of 
c     logical cseed now in common block       ! large seed file
      logical is_s_file                       ! true if s-file
                                              ! resp files 
c     character*2 seed_location_code          ! seed location code
      character*80 stat_list(max_trace)       ! station list
      logical nodata(max_trace)
      integer stat_cnt                        ! station counter
      character*80 format_out                 ! output format
      integer max_points                      ! maximum number of points out
      integer nskip(max_trace)                ! number of points to skip
      integer syear,smonth,sday,shour,smin    ! common start
      real ssec                               ! common start
      integer iyear,idoy,imonth,iday,ihour,imin,isec! date variables
      real fsec                                ! real seconds
      integer code                            ! return error code
      real flow,fhigh                         ! filter band pass f
      integer npole                           ! number of poles
      real      cof(8)                        ! filter coefficients
      real      gain                          ! filter gain
      logical ground                          ! true if ground motion
      integer ground_type                     ! dis=1, vel=2 or acc=3
      complex y_com(max_sample)               ! complex spectrum
      integer file_cnt                        ! file counter
      real slat,slon,sheight                  ! station location
      character*1 resp_flag                   ! Y if response there
      character*1 statfile_char               ! character in station file
      character*120 chan_out_text(1000)       ! output text array
      real dc,ndc                             ! used for remove_dc
      character*80 question                   ! question for filenames
      logical sfile_flag                      ! true if sfile
      logical eventdata_flag                  ! true if search from sfile
      logical rsam_flag                       ! true for rsam output
      character*80 cbase_select(1000)         ! sel. seisan databases or arcchan.
      integer n_cbase_select                  ! number of the above
      character*80 cbase_list                 ! file with cont databases

c
c  following for seisan format output files
c
      integer year(max_trace),month(max_trace),day(max_trace),
     *        hour(max_trace),min(max_trace),nsamp(max_trace)
      real rate(max_trace),sec(max_trace)
      character*80 mainhead(max_trace)   ! seisan main header
      character*1040 chahead             ! seisan channel header
      character*29 h_text                ! seisan header
      integer signal4b(max_sample)       ! 4 byte seisan data
      integer*2 signal2b(max_sample)     ! 2 byte seisan data
      character*1 cbyte(max_trace)       ! indicator of 2 or 4 bytes
      real ymax                          ! maximum output value
      real factor                        ! gain factor if used
c
c  following parameters for selected output
c
      double precision start,duration    ! start and duration one channel
      double precision msec,msece
      character*18 pstart,pstop          ! absolute start from prompt,ymdhms
      double precision abs_start_time    ! pstart in seconds
      double precision abs_stop_time     ! pstop in seconds
      real pduration                     ! duratiopn from prompt
      character*5 stat(max_trace)
      character*4 comp(max_trace)
      character*2 location(max_trace)    ! seed location
      character*2 network(max_trace)     ! seed network
      character*3 seed_comp              ! seed component
      integer appendFlag                 ! append flag for mseed write
      integer compFlag                   ! compression flag, 0: steim1, 1 steim2
      integer recSize                    ! record size to write mseed
      integer i,k,j,l,ichan,in
      character*80 file
      real evla,evlo,evel                ! event loctaion
      integer nerr                       ! error code
      logical open_file,op_file          ! for opening mseed file
      integer mseed_block                ! block counter for wring miniseed files
      character*160 system_call
      logical sun,linux,pc
      integer npole_low,npole_high       ! number of poles is a butterworth
                                         ! filter is used
      character*160 chan_def_file        ! file with channel name change defintion
      character*29 mainhead_text   ! text to be put into main header
      character*5 base_name
c-- start and end time of select      
      character*14    start_time,end_time
c-- select key                                       
      character*10     key
      integer event_no,fstart,new_month,status   ! used by findevin
c-- event file name                                   
      character*80     evfile
c-- station name  and component
      character*5 eventstat
      double precision x,y
c-- selecetd event waveform
      character*80 eventwavefile
c-- selected channel ids
      integer eventchan(99),neventchan
c-- computed duration of required signal
      double precision eventseg
c-- duration of selected channel
      double precision eventseg_selected


      
c
c print version
c
      include 'version.inc'
c
c  poles and zeros, not used
c
      complex pole(100),zero(100)               ! complex PAZ
      integer npole1,nzero                      ! number of poles and zeros
      real norm                                 ! normalization constant for poles and zeros
      npole1=0                                  
      nzero=0    
      call wav_mem_init                               
c
c   make sure no message file left from before
c

      open(1,file='extract.mes',status='unknown')
      close(1,status='delete')


      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call computer_type(sun,pc,linux)

      chan_out_file=' '
      sfilname=' '
      wav_files=' '
      wav_out_file=' '
      seisweb=.false.
c     seed_location_code='  '
      pstart='0'
      pstop='0'
      flow=0.0
      fhigh=0.0
      npole=0             ! no npole given
      npole_low=4         ! default of no npole is given       
      npole_high=4        ! -------------------------
      pduration=0.0
      n_cbase_select=0
      chansel='_'
      format_out='SEISAN'     ! default format to write is seisan
      evla=0.
      evlo=0.
      evel=0.
      ichan=-1
      ground=.false.
      ground_type=0
      in=0
      statfile_char=' '
      stat_cnt=0
      sfile_flag=.false.
      eventdata_flag=.false.
      station_out_flag=.false.
      resp_out_flag=.false.
      max_points=0
      wav_interactive=.false.
      rsam_flag=.false.
      cseed=.false.
      cwav=.false.
      mseed_block=1             ! start with first block
      cbase_list=' '
      chan_def_file=' '
      eventstat=' '
      eventseg_selected=0.
      neventchan=0
      
c
c   get seisan defaults 
c
      call get_seisan_def
c
c   get arguments
c
      call get_arguments(narg,arg)

 10   continue
      if(narg.lt.2) then
c
c  since no question about output name, use SEISAN as default
c
         wav_out_file(1:6)='SEISAN'
c
c   get s-file name or waveform file name
c
         question=' Filename of s-file or waveform file, ' //
     &          'number or filenr.lis '
         call filename(question,sfilname)
         if (seiclen(sfilname).le.0.or.sfilname.eq.'EOF') stop


c
c filenr.lis, interactive input
c
         if (sfilname(1:10).eq.'filenr.lis') then 
           open(2,file='filenr.lis',status='old', err=999)
           in=1
           wav_interactive=.true.  ! lot 1/8/08
           file_cnt=0
  11       continue
           read(2,'(7x,a)',end=12) sfilname
           if(sfilname(1:1).eq.' ') go to 12
           goto 13
  12       continue
           close(1)
           goto 10
         endif
  13     continue
         file_cnt=file_cnt+1  
c
c reset wav
c
         call wav_init

c
c   check if wav file exist
c
         if(sfilname.ne.' ') then
            inquire(file=sfilname,exist=exist)
            if(.not.exist) then
               write(6,*) ' No such file'
               goto 10
            endif
         endif
  
c
c   interactive input of parameters
c
         text=' '
         if (in.eq.0) then
           write(6,*) 
     *     'Maximum number of points in output trace, '
           write(6,*)'Points are skipped to make it fit, return for all'
            read(5,'(a)') text
         endif
         if(text.eq.' ') then 
            max_points=0
         else
            read(text,'(i8)') max_points
         endif
         text=' '

         if (file_cnt.eq.1) then
           write(6,*) ' Ground motion output (dis = 1, '
     &         // 'vel = 2, acc = 3, none = return)'
           read(5,'(a)') text
           if(text.eq.' ') then 
              ground_type=0
           else
              read(text,'(i1)') ground_type
              if (ground_type.eq.1.or.
     &                ground_type.eq.2.or.
     &                ground_type.eq.3) then
                 ground = .true.
              endif
c
c read filter
c
c             write(*,*) ' filter low and high '
c             read(5,*) flow,fhigh
           endif
c
c read filter
c
           write(*,*) ' Filter low and high, return for no filter '
           read(5,'(a)') text
           if(text.eq.' ') then
             flow=0.0
             fhigh=0.0
           else
             call sei get values(2,text, code ) 
             flow=array$(1) 
             fhigh=array$(2)
           endif              
         endif
      else
c
c  read argument from a file
c
         if( arg(1)(1:13) .eq. '-command_file' ) then
           call get_argument_from_file(arg,narg)
         endif

c
c   read arguments to fill in choices
c
         do i=1,narg
            if(arg(i)(1:13).eq.'-wav_out_file') then
               wav_out_file=arg(i+1)
            endif
            if(arg(i)(1:6).eq.'-sfile') then
               sfilname=arg(i+1)
               sfile_flag=.true.
            endif
            if(arg(i)(1:10).eq.'-eventdata') then
               base_name=arg(i+1)
               eventstat=arg(i+2)
               eventdata_flag=.true.
            endif
            if(arg(i)(1:6).eq.'-cbase') then
c               n_cbase_select=n_cbase_select+1
c               cbase_select(n_cbase_select)=arg(i+1)
                cbase_list=arg(i+1)
            endif
            if(arg(i)(1:7).eq.'-filter') then
                call sei get values(1,arg(i+1), code )  
                flow=array$(1) 
                call sei get values(1,arg(i+2), code )
                fhigh=array$(1) 
            endif
            if(arg(i)(1:5).eq.'-rsam') then
               rsam_flag=.true.
            endif
            if(arg(i)(1:6).eq.'-npole') then
                call sei get values(1,arg(i+1), code )
                npole=int(array$(1))
            endif
            if(arg(i)(1:7).eq.'-ground') then
                call sei get values(1,arg(i+1), code ) 
                ground_type = int(array$(1))
                if (ground_type.eq.1.or.
     &              ground_type.eq.2.or.
     &              ground_type.eq.3) then
                  ground = .true.
                endif
            endif
            if(arg(i)(1:8).eq.'-seisweb') then
                seisweb=.true.
            endif
            if(arg(i)(1:9).eq.'-stat_out') then
                station_out_flag=.true.
            endif
            if(arg(i)(1:9).eq.'-resp_out') then
                resp_out_flag=.true.
            endif
            if(arg(i)(1:10).eq.'-wav_files') then
                wav_files=arg(i+1)
            endif
            if(arg(i)(1:7).eq.'-format') then
                format_out=arg(i+1)
            endif
            if(arg(i)(1:8).eq.'-chansel') then
                chansel=arg(i+1)
            endif
            if(arg(i)(1:14).eq.'-chan_out_file') then
                chan_out_file=arg(i+1)
	          open(1,file=chan_out_file,status='unknown')
	          endfile 1
                rewind 1
	          close(1)
            endif
            if(arg(i)(1:6).eq.'-start') then
                pstart=arg(i+1)
            endif
            if(arg(i)(1:5).eq.'-stop') then
                pstop=arg(i+1)
            endif
            if(arg(i)(1:9).eq.'-duration') then
                call sei get values(1,arg(i+1), code )   ! Extract 1 value
                pduration=array$(1) 
            endif
            if(arg(i)(1:10).eq.'-maxpoints') then
                call sei get values(1,arg(i+1), code )   ! Extract 1 value
                max_points=array$(1) 
            endif
            if(arg(i)(1:6).eq.'-ichan') then
                call sei get values(1,arg(i+1), code )   ! Extract 1 value
                ichan=array$(1)
            endif
            if(arg(i)(1:5).eq.'-cwav') then
                cwav=.true.                              ! cont base
            endif
            if(arg(i)(1:4).eq.'-arc') then
                cwav=.true.                              ! cont base
                arc=.true.
            endif
            if(arg(i)(1:6).eq.'-cseed') then
                cseed=.true.                             !  large seed
            endif
c           if(arg(i)(1:6).eq.'-seed_location') then
c               seed_location_code=arg(i+1)(1:2)         !  seed location code
c           endif
            if(arg(i)(1:12).eq.'-interactive') then
                wav_interactive=.true.
            endif
            if(arg(i)(1:12).eq.'-wav_in_file') then
                sfilname=arg(i+1)
            endif
            if(arg(i)(1:8).eq.'-chandef') then
                chan_def_file=arg(i+1)
            endif
         enddo
      endif
c
c--------------------------------------------------------
c---------------------------------------------------------
c   get all info if cont data base or a large seed file or arc
c---------------------------------------------------------
c---------------------------------------------------------
c
      if(cwav.or.cseed.or.arc) then
         if(cseed) write(6,*)' Large SEED'
c
c check if seisan cont databases
c
        if(cwav) then
         if (n_cont_base.eq.0.and..not.arc) then
           write(6,*) ' No continuous database defined in SEISAN.DEF '
           stop
         endif
c
c check if arc archive available 
c
        if(arc) then
         if (arc_nchan.eq.0) then
           write(6,*) ' No archive defined in SEISAN.DEF' 
           stop
         endif
        endif

c
c read file with cont databases, either seisan or arc. this is needed
c if not all channels defined in SEISAN.DEF are used as potential input
c and the extract.inp file numbers refer to this subset. this is 
c currently used with an extract started from mulplt
c
         if (cbase_list.ne.' ') then
           open(1,file=cbase_list,status='old')
18         continue
           n_cbase_select=n_cbase_select+1
c
c          write(*,*) n_cbase_select
           read(1,'(a)',end=19) cbase_select(n_cbase_select)
c          write(*,*) ' database selected: '//
c    &          cbase_select(n_cbase_select)
           goto 18
19         continue
           close(1)
           if(cwav) n_cbase_select=n_cbase_select-1
           if(arc) arc_nchan=n_cbase_select
         endif
c
c   check if databases or arc channels selected in a subset for definition in 
c   SEISAN.DEF
c
         if (n_cbase_select.gt.0) then
           do i=1,n_cbase_select
              if(cwav.and..not.arc)cont_base(i)=cbase_select(i)(1:5)
              if(arc) then
                 arc_stat(i)=cbase_select(i)(1:5)
                 arc_comp(i)=cbase_select(i)(6:8)
                 arc_net(i)=cbase_select(i)(9:10)
                 arc_loc(i)=cbase_select(i)(11:12)
                 read(cbase_select(i)(13:42),'(2f15.0)') 
     *           arc_start(i),arc_stop(i)
c set to time given by -start or -stop
                 if (arc_start(i).eq.0..and.pstart.ne.'0') then
          read(pstart,'(i4,5i2)') iyear,imonth,iday,ihour,imin,isec
          fsec=float(isec)
          call timsec(iyear,imonth,iday,ihour,imin,fsec,msec)
                   arc_start(i)=msec
                 endif
                 if (arc_stop(i).eq.0..and.pstop.ne.'0') then
          read(pstop,'(i4,5i2)') iyear,imonth,iday,ihour,imin,isec
          fsec=float(isec)
          call timsec(iyear,imonth,iday,ihour,imin,fsec,msec)
                   arc_stop(i)=msec
                   wav_total_time=arc_stop(i)-arc_start(i)
c                   write(*,*) ' debug lo ',wav_total_time
                 endif
           write(6,*) arc_stat(i),arc_loc(i),arc_comp(i),
     *           arc_net(i),arc_start(i),arc_stop(i)
              endif
           enddo
           if(cwav.and..not.arc)n_cont_base=n_cbase_select
         endif

         if(arc) write(6,*)'Number of archive channels defined',
     *   arc_nchan
c        do i=1,arc_nchan
c                write(6,*) arc_stat(i),arc_loc(i),arc_comp(i),
c    *           arc_net(i),arc_start(i),arc_stop(i)
c        enddo
c
c write out the cont databases
c
         if(cwav.and..not.arc) then
         write(*,*)'Cont databases are:'
         do i=1,n_cont_base
            write(*,*)'    ',cont_base(i)
         enddo
         endif
        endif
c
c work out duration from stop time if that is given
c
        if(pduration.eq.0.0.and.pstop.ne.'0'.and.
     &     pstart.ne.'0') then
          read(pstart,'(i4,5i2)') iyear,imonth,iday,ihour,imin,isec
          fsec=float(isec)
          call timsec(iyear,imonth,iday,ihour,imin,fsec,msec)
          read(pstop,'(i4,5i2)') iyear,imonth,iday,ihour,imin,isec
          fsec=float(isec)
          call timsec(iyear,imonth,iday,ihour,imin,fsec,msece)
          pduration=int(msece-msec)
          write(*,*) ' duration = ',pduration
        endif
c
c   get start time and time interval
c
         if(pduration.eq.0.0.or.pstart.eq.' ') then
            write(6,*)
     *     'Abs start time and interval must be given for'//
     *      ' cwav or cseed option'
            stop
         endif

         cwav_start_time=pstart     ! also used for seed
         pstart='0'     ! indicate that whole window used, no further
                        ! selection in time
         cont_interval=pduration

c
c   calculate end time and extended start time
c
         call cwav_time_limits(0)
c
c  read the header information for all files in all bases in time
c  interval, assume info available in common block, not large seed file
c
         if(cwav.and..not.arc) call cwav_read_bases
         write(*,*) ' debug ',wav_total_time
         if(arc) call wav_read_arc_headers
         write(*,*) ' debug ',wav_total_time

c
c  read data for section of seed file
c
         if(cseed) then
c
c   case where waveform file name comes from s-file, can only be the case of an exatract
c   from mulplt
c
            if(sfile_flag) then
               open(1,file=sfilname,status='old', err=999)
c
c  read s-file
c
               call rea_event_in(1,.true.,data,code)
               close(1)
c
c   get waveform file names, should be only one
c
               call auto_tr(data,rea_nhead,rea_nrecord
     *         ,wav_nfiles,wav_filename)
            else
c
c  case where name comes directly from prompt or interctive input
c
               wav_filename(1)=' '
               wav_filename(1)=sfilname
               wav_nfiles=1         ! only one file can be used
            endif

            write(*,*) ' reading waveform headers ... '
            write(6,*) wav_filename(1)
            call wav_init
            call wav_mem_init
            call read_wav_header(1)
            write(6,*) 'headers read'
            call wav_seed_read_header_interval ! find position in file of window
            write(6,*) '  large seed read'
         endif
 
c
c for continuous/seed data, if number of samples not set, do it here
c
         do k=1,wav_nchan
           nodata(k)=.false.
           if (wav_nsamp(k).le.0) then
             write(6,*)'set nsamp'
             wav_nsamp(k)=int(pduration*wav_rate(k))
             wav_duration(k)=pduration
             nodata(k)=.true.
           endif
         enddo

c
c   terminate if no waveform files
c
         if(wav_nchan.eq.0.and..not.arc) then
           write(*,*) ' no waveform files (1) ' 
           goto 999   
         endif
         goto 700      ! jump all normal reading, straight to output
      endif
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccccccccccccccc  end of section for large seed or cont data cccccccccccc
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c


c-----------------------------------------------------------------------
c check if waveform file by reading the headers of all channels in waveform file
c this is done because sfilname can be a waveform file name
c and a sfile name, should be changed, whole waveform file should not be read
c for thsi test !
c
c dec 21 2012 jh: now check if s-file by reading first line of file, old code 
c                 commented out

c at this point if an s-file, the waveform file name is transferred from 
c sfilname to wav_file_name
c-----------------------------------------------------------------------

      if (.not.sfile_flag.and.sfilname.ne.' ') then
         is_s_file=.false.
         open(110,file=sfilname,status='old')
         read(110,'(a)') text
         close(110)
         if((text(22:22).eq.'L'.or.text(22:22).eq.'R'.or.
     *   text(22:22).eq.'D').and.text(80:80).eq.'1'.and.
     *   (text(2:2).eq.'1'.or.text(2:2).eq.'2')) is_s_file=.true.
c
c        wav_error_message=' '        
c        call wav_init
c
c  was a waveform file, transfer name and number
c
         if(.not.is_s_file) then
            wav_nfiles=1
            wav_filename(1)=' '
            wav_filename(1)=sfilname
            call get_full_wav_name(sfilname, wav_filename(1))
         endif
c        write(*,*) ' reading waveform headers to check if sfile ... '
c        call read_wav_header(1)
      endif
c
c case of station data extract from event files, steps are
c    - search for sfiles
c    - read headers
c    - check for most complete trace from various files
c      for given time interval
c    - store waveform filename
c    - write channel extract file
c
       if (eventdata_flag) then
c         write(*,*) ' debug eventdata_flag '
c set start time
         start_time=pstart(1:14)
c set stop time
         read(start_time,'(i4,5i2)') iyear,imonth,iday,ihour,imin,isec
         fsec=float(isec)
         call timsec(iyear,imonth,iday,ihour,imin,fsec,msec)
         msece=msec
c subtract 2 hours from start
         msec=msec-7200   ! start 2 hours before
         call sectim(msec,iyear,idoy,imonth,iday,ihour,imin,fsec)
         write(start_time,'(i4,5i2.2)')iyear,imonth,iday,ihour,imin,isec
         msec=msec+7200+pduration+3600 ! continue one hour after
         call sectim(msec,iyear,idoy,imonth,iday,ihour,imin,fsec)
         isec=int(fsec)
         write(end_time,'(i4,5i2.2)') iyear,imonth,iday,ihour,imin,isec
         write(*,*) ' start: ',start_time
         write(*,*) ' end: ',end_time
222      continue
         key='          ' 
         call findevin
     *     (base_name,start_time,end_time,key,0,
     *     event_no,evfile,fstart,new_month,status)
c         write(*,*) ' debug ',evfile,status
c         if(status.eq.0.or.          ! event found
c     &      status.eq.3) then        ! end of time period
          if(status.eq.0) then
c
c get the waveforms and read headers
c
           write(*,*) ' reading headers ',evfile
           open(1,file=evfile,status='old', err=999)
           call rea_event_in(1,.true.,data,code)
           close(1)
           call auto_tr(data,rea_nhead,rea_nrecord,
     *       wav_nfiles,wav_filename)
           do i=1,wav_nfiles
             write(*,*) ' reading headers: ',wav_filename(i)
             call wav_init
             call wav_mem_init
             call get_full_wav_name(wav_filename(i),text)
             if(text.ne.' ') then
               wav_filename(i)=text
               call read_wav_header(i)
               if(wav_error_message.ne.' ') write(6,'(1x,a)')
     *            wav_error_message
               write(6,'(1x,a40,2x,a)') wav_filename(i)(1:40),
     *            wav_file_format(i)
c
c search the header for station and time 
c
               do j=1,wav_nchan
                 if (wav_stat(j).eq.eventstat.and.
     *             wav_comp(j)(4:4).eq.'Z') then
                   write(*,*) wav_stat(j),wav_comp(j)
c work out time in seconds of overlap for this channel                 
                   if (msece.ge.wav_abs_time(j)+wav_duration(j).or.
     &                 msece+pduration.le.wav_abs_time(j)) then
                     eventseg=0.
c          write(*,*) ' debug eventseg 0 ',msece-wav_abs_time(j)
                   else
c          write(*,*) ' debug computing eventseg '
                     x=msece
                     if (wav_abs_time(j).gt.x) x=wav_abs_time(j)
                     y=msece+pduration
                     if (wav_abs_time(j)+wav_duration(j).lt.y) 
     &                 y=wav_abs_time(j)+wav_duration(j)
                     write(*,*) y,x
                     if (y.gt.x) eventseg=y-x
                   endif
                   write(*,*) j,eventseg,wav_duration(j),pduration
c
c check if longest required segment
                   if (eventseg.gt.eventseg_selected) then
                     eventseg_selected=eventseg
                     eventwavefile=wav_filename(i)
                   endif
                 endif
               enddo
c store all channels ids belonging to wanted station
               if (eventwavefile.eq.wav_filename(i)) then
                 neventchan=0
                 do j=1,wav_nchan
                   if (wav_stat(j).eq.eventstat) then
                     neventchan=neventchan+1
                     eventchan(neventchan)=j
                     write(*,*) ' channel to save: ',j
                   endif
                 enddo
               endif
             else 
               write(*,*) ' missing waveform: ',wav_filename(i)
             endif
           enddo
         else
c           write(*,*) ' something wrong finding sfiles '
           goto 223
         endif
         goto 222
223      continue

c
c write out channel selection list
c
         if (neventchan.eq.0) then
           write(*,*) ' station not found '
           stop
         endif
         chansel='chansel.inp'
         wav_nfiles=1
         wav_filename(1)=eventwavefile
         open(1,file=chansel,status='unknown')
         write(1,*) neventchan
         do j=1,neventchan
           write(1,*) eventchan(j),pstart,pduration
         enddo
         close(1)
       endif


c
c---------------------------------------------------------
c  case of waveform files from sfile
c---------------------------------------------------------
c
c check if sfile, if not waveform
c
c      if(wav_error_message.ne.' ') then
       if(is_s_file.or.sfile_flag) then
       if(sfilname.ne.' ') then    ! assume s-file
         open(1,file=sfilname,status='old', err=999)
c
c  read s-file
c
         call rea_event_in(1,.true.,data,code)
c
c   get waveform file names
c
         call auto_tr(data,rea_nhead,rea_nrecord,
     *   wav_nfiles,wav_filename)
       endif
      endif
c
c----------------------------------------------------------------
c   case of input from a list of waveform files
c---------------------------------------------------------------
c
c   check for waveform files in a filenr.lis format input file
c   wav_files is from argument list
c
      if(wav_files.ne.' ') then
         write(6,*) wav_files
         open(1,file=wav_files,status='old', err=999)
         j=0
 20      continue
         j=j+1
         read(1,'(7x,a)',end=21) wav_filename(j)
         if(wav_filename(j).ne.' ') go to 20
 21      continue
         wav_nfiles=j-1
      endif
c
c----------------------------
c
      if(.not.seisweb) then              
        write(6,*)' Number of wav-files',wav_nfiles
      endif
c
c   find how many waveform files are present, replace the origial
c   names with the present files full path
c
      npresent=0                ! no files initially
      do i=1,wav_nfiles
          call get_full_wav_name(wav_filename(i),text)
      if(.not.seisweb) then              
c        write(*,*) i,wav_filename(i),text
      endif
          if(text.ne.' ') then
             npresent=npresent+1
             wav_filename(npresent)=text
          endif
      enddo
      wav_nfiles=npresent
      if(.not.seisweb) then              
        write(6,*)' Number of wav-files present', wav_nfiles
      endif
c
c read channel defintion file
c
      if (seiclen(chan_def_file).gt.0)
     &call read_def_chan(chan_def_file,mainhead_text,net_code)

c
c   terminate if no waveform files
c
      if(wav_nfiles.eq.0) then
        write(*,*) ' no waveform files (2) '
        goto 999   
      endif
c
      wav_nchan=0
c
c   loop to read all headers of all files
c
      do i=1,wav_nfiles
c        write(6,'(1x,a)') wav_filename(i)
         call read_wav_header(i)
         if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *   wav_error_message
         write(6,'(1x,a40,2x,a)') wav_filename(i)(1:40),
     *   wav_file_format(i)
      enddo
c
      if(.not.seisweb) then              
        write(6,*)' Total number of channels available:',wav_nchan
      endif
c
c   terminate if no channels
c
      if(wav_nchan.eq.0) then
        write(*,*) ' no channels '
        goto 999
      endif

c---------------------------------------------------------------
c   section for output, if cont data base enter here directly
c   since all headers read above
c---------------------------------------------------------------
c
 700  continue      ! from cont reading above
c
c   write input out, optionally also in a file
c
      if(.not.seisweb) then              
        write(6,*)' Total duration:',wav_total_time
        open(3,file='respfile_list.out',status='unknown')
        do i=1,wav_nchan
c
c check if response available for channel and get station location
c
c   j was not set in 2. "if(station_out..)", pointed out by  
c   wayne crawford, mar 06, so in following line j=1 so j is set 
c   when station_out is false
c
          j=1
c
          if (station_out_flag) then
          call stat_loc(wav_out_stat(i),statfile_char,slat,slon,sheight)
c 
c sort into station location array
c
            j=1
            do while(j.le.stat_cnt.and.
     &           wav_out_stat(i).ne.stat_list(j)(1:5))
               j=j+1
            enddo
          endif
          if (station_out_flag.and.
     &       wav_out_stat(i).ne.stat_list(j)(1:5)) then
             stat_cnt=stat_cnt+1
             stat_list(stat_cnt)=' '
             write(stat_list(stat_cnt),'(a5,1x,f8.3,1x,f8.3)')
     &             wav_out_stat(i),slat,slon
          endif

          resp_flag='N'
          if (ground.or.resp_out_flag) then
            wav_current_chan(1) = i
            wav_resp_file = ' '
c xxx
            call read_resp
            if (wav_resp_filename.ne.' ') then
              resp_flag='Y'
              write(3,'(a)')
     &    wav_resp_filename(1:seiclen(wav_resp_filename))
c              write(3,'(a)') 'cp /net/seismo/seismo/CAL/'//
c     &         wav_resp_filename(1:seiclen(wav_resp_filename)) //' .'
            else
c            write(555,'(a)') 'cp /net/seismo/seismo/CAL/'//
c     &         wav_resp_file(1:seiclen(wav_resp_file)) //' .'
            endif
            wav_resp_file = ' '
          endif

c         write(6,'(1x,i2,1x,a,1x,a,1x,i5,4i3,f7.3,i6,3f9.3)')
c 05 march 2001, lo
c 08 June 2001 lo, added location and resp
c
          chan_out_text(i)=' '
         write(chan_out_text(i),
     *        '(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i6,3f9.3,
     *        2(1x,f8.3),1x,f5.0,1x,a1)')
     *   i, wav_out_stat(i),wav_out_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i),
     *   slat,slon,sheight,resp_flag

cx         write(*,'(a)') chan_out_text(i)(1:seiclen(chan_out_text(i)))

        enddo
        close(3) ! close response list file
        if (station_out_flag) then
          open(3,file='station_list.out',status='unknown')
          open(4,file='station_list.err',status='unknown')
          do j=1,stat_cnt
            read(stat_list(j),'(6x,f8.3,1x,f8.3)') slat,slon
            if (slat.ne.0..and.slon.ne.0.) then
              write(3,'(a)') stat_list(j)(1:seiclen(stat_list(j)))
            else
              write(4,'(a)') stat_list(j)(1:seiclen(stat_list(j)))
            endif
          enddo
          close(3)
          close(4)
        endif
      endif

      if(seisweb) then
         do i=1,wav_nchan
         chan_out_text(i)=' '
         write(chan_out_text(i),
     *        '(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i6,3f9.3,
     *        2(1x,f8.3),1x,f5.0,1x,a1)')
     *   i, wav_out_stat(i),wav_out_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i),
     *   slat,slon,sheight,resp_flag
         enddo
      endif

c
c   read channel selection file
c
      if(chan_out_file.ne.' ') then
         open(1,file=chan_out_file,status='unknown')
         do i=1,wav_nchan
c            write(1,'(1x,i2,1x,a,1x,a,1x,i5,4i3,f7.3,i6,3f9.3)')
c 05 march 2001, lo
           write(1,'(a)') chan_out_text(i)(1:seiclen(chan_out_text(i)))

c            write(1,'(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i6,3f9.3)')
c         write(1,'(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i6,3f9.3,
c     *        2(1x,f8.3),1x,f5.1,1x,a1)')
c     *      i, wav_stat(i),wav_comp(i),
c     *      wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
c     *      wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
c     *      wav_duration(i),
c     *      slat,slon,sheight,resp_flag
         enddo
         close(1)
         if (wav_out_file.eq.' ' ) then
           if(seisweb) then
             write(6,*)'extract channel info terminated: ok ', wav_nchan
           endif
           stop
         endif
      endif

c
c-------------------------------------------------------------------------
c  select and possibly process data for output file, can be done
c  from keyboard if no input on  prompt line
c  following is what is selected using keyboard input
c-------------------------------------------------------------------------
c
c   select channels to read, by default all data are selected
c
      call wav_index_total
      do i=1,wav_nchan
        wav_out_chan(i)=i
        wav_out_start(i)=wav_abs_time(i)-wav_abs_time(wav_first)
        wav_out_duration(i)=wav_duration(i)
c        write(6,*) 'org wav duration', wav_duration(i)
      enddo
      wav_out_nchan=wav_nchan
c
c-----------------------------------------------------------------------
c  keyboard input
c-----------------------------------------------------------------------
c     
      if(narg.lt.2) then
       if(in.ne.1) then
         write(6,*)' Select all data, y=return,n'
         read(5,'(a)') text
         if(text.ne.' '.and.text.ne.'y'.and.text.ne.'Y') then 
            write(6,*)' Number of channels to read'
            read(5,*) wav_out_nchan
            write(6,*)' Channel number, start time and window'
            do i=1,wav_out_nchan
               read(5,*,err=555) wav_out_chan(i),start,duration
c
c   find out if a small or a large number, which indicate if abs start
c
               if(start.gt.1.0e10) then
                  write(pstart,'(f18.3)',err=555) start
                  read(pstart,'(i4,4i2,f6.3)',err=555)
     *            syear,smonth,sday,shour,smin,ssec
                  goto 556
 555              continue
                  write(6,*)' Error in input'
                  stop
 556              continue
                  call timsec(syear,smonth,sday,shour,smin,ssec,
     *            abs_start_time)
                  start=abs_start_time-wav_abs_time(wav_first) ! cal. rel start
               endif
               wav_out_start(wav_out_chan(i))=start  ! put in correct channel
               if(duration.ne.0.0) then
                  wav_out_duration(wav_out_chan(i))=duration
               endif
            enddo
         endif
        endif
        if ((in.eq.1.and.file_cnt.eq.1).or.(in.eq.0)) then
          if (sun.or.linux) then
            write(*,*) 
     *' Output formats '//
     *'(SEISAN, GSE (def=CM6), GSEINT, SAC, MSEED, SEED, SEEDDL) '
          else
            write(*,*) 
     *' Output formats '//
     *'(SEISAN, GSE (def=CM6), GSEINT, SAC, MSEED) '
          endif
          write(*,*) ' Default is SEISAN=return'
          read(*,'(a)') format_out
          call casefold(format_out)
          if(format_out.eq.' ') format_out='SEISAN'
          if(format_out.eq.'SEED'.and.pc) then
            write(*,*) ' SEED output format only works on Unix '
            stop
          endif
c         if(format_out.eq.'MSEED'.and.seed_location_code.eq.'  ') then
c             write(6,*)'SEED location code, 2 characters'
c             read(5,'(a2)') seed_location_code
c         endif
c          if(format_out(1:5).eq.'MSEED'.and.in.eq.1) then
c            join_all=.false.
c            write(6,*)
c     *     'Do you want to join all output files to one (y/n=default) ?'
c            read(5,'(a)') answer
c            if(answer.eq.'y') join_all=.true.
c           endif
        endif
       else
c
c------------------------------------------------------------------------------
c   selection is from prompt, if no selection file, all data selected initially
c   there could also be a start and duration, see below
c------------------------------------------------------------------------------
c

c-------------------------
c   option selection file
c-------------------------
c
         if(chansel.ne.'_') then         ! from a file
c            write(*,*) ' debug reading chanel selection input '
            open(1,file=chansel,status='old',err=901)
            goto 902
 901        write(6,*)' Channel selection input file missing'
            goto 999
 902        continue
c
c   first get number of channels
c
            read(1,*,err=903,end=903) wav_out_nchan
c
c   read info if abs time used
c
            do i=1,wav_out_nchan
               read(1,*,err=903) wav_out_chan(i),start,duration 
c
c   find out if a small or a large number, which indicate if abs start
c   start is used to select the start of the window in abs time
c
               if(start.gt.1.0e10) then    ! start is abs start
                  write(pstart,'(f18.3)') start
                  read(pstart,'(i4,4i2,f6.3)',err=903)
     *            syear,smonth,sday,shour,smin,ssec
                  call timsec(syear,smonth,sday,shour,smin,ssec,
     *            abs_start_time)
                  start=abs_start_time-wav_abs_time(wav_first) ! cal. rel start
                  wav_out_start(wav_out_chan(i))=start
               else
                 if (start.eq.0.) then    ! use first sample
                   wav_out_start(wav_out_chan(i))=
     *  -wav_abs_time(wav_first)+wav_abs_time(wav_out_chan(i)) ! relative first channel
                 else
                   wav_out_start(wav_out_chan(i))=start
                 endif
               endif
               if(duration.ne.0.0) then
                  wav_out_duration(wav_out_chan(i))=duration
               endif
            enddo
            close(1)
            goto 904
 903        continue
            write(6,'(a,a)')' Error in channel select file'
            goto 999
 904        continue
        else

c
c----------------------------------------------------------------
c   option same start and window all channels as given on prompt
c-----------------------------------------------------------------
c
            if(pstart.ne.'0') then    
              read(pstart,'(f20.3)')start 
              if(start.gt.1.0e10) then   ! abs start times
                 write(pstart,'(f18.3)') start
                 read(pstart,'(i4,4i2,f6.3)',err=950)
     *           syear,smonth,sday,shour,smin,ssec  
                 call timsec(syear,smonth,sday,shour,smin,ssec,
     *           abs_start_time)
                 if(abs_start_time.lt.wav_abs_time(wav_first)) then
                    write(6,*)' Start is before first data point, ',
     *              ' will use first point'
                    abs_start_time=wav_abs_time(wav_first)
                 endif
                 start=abs_start_time-wav_abs_time(wav_first) ! cal. rel start
              else
                call sei get values(1,pstart, code )   ! Extract 1 value
                start=array$(1) 
              endif
              goto 951
 950          continue
              write(6,*)' Error in time string'
              goto 999
 951          continue
              do i=1,wav_out_nchan
                 wav_out_start(i)=start
                 if(pduration.ne.0.0) wav_out_duration(i)=pduration
              enddo
            endif
c
c----------------------------------------------------------------
c   option for select 1 channel
c-----------------------------------------------------------------
c
       if(ichan.ne.-1) then
            wav_out_nchan=1
            wav_out_chan(1)=ichan
       endif
      endif
      if(pstart.ne.' '.and.pduration.ne.0.0) then
      endif 
      endif

c
c-------------------------------------------------------------------------
c   calculate new start times and intervals, check if data is available
c   the call to wav_get_interval also calculates new number of samples
c   in variable wav_out_nsamp. wav_out_start is input and gives start time relative
c   to first sample in file, wav_out duration gives the lenght of the window.
c   both set above
c-------------------------------------------------------------------------
c
c      write(6,*) 'wav-out-start,duration',wav_out_start(1),
c     *wav_out_duration(1)      

      call wav_get_interval

c
c   get rid of empty channels
c
      k=0
      do i=1,wav_out_nchan
        if(wav_out_status(wav_out_chan(i)).gt.0) then
           k=k+1
           wav_out_chan(k)=wav_out_chan(i)
        endif
      enddo
      wav_out_nchan=k
c
c copy chan to out_chan
c
c      do i=1,wav_out_nchan ! CHANGED LO 6/6/2014

      do i=1,wav_nchan
        wav_out_stat(i)=wav_stat(i)
        wav_out_comp(i)=wav_comp(i)
c        write(*,*) i,wav_stat(i),wav_comp(i)
c        write(*,*) i,wav_out_stat(i),wav_out_comp(i)
      enddo
c
c convert channel names if needed, lo 28 March 2014
c
      if (chan_def_file.ne.' ') then
c       write(*,*) ' debug using chan def file '
        do i=1,wav_out_nchan
          call set_def_chan(i,wav_out_stat(i),wav_out_comp(i))
c          write(*,*) ' debug name changed: ',wav_comp(i),' ',
c     &          wav_out_comp(i)
        enddo
      endif
c
c   check if any data left
c
cc     write(6,*)' Number of channels after time window selection',
cc    *wav_out_nchaN
      if(wav_out_nchan.eq.0) then
         write(6,*)' No data left after window selection'
         goto 999
      endif          
c      if(.not.seisweb) then              
c      do i=1,wav_out_nchan
c         k=wav_out_chan(i)
c         nskip(k)=1           ! not less than every point
c         write(6,'(i5,1x,a5,a4,i4)')k,wav_stat(k),wav_comp(k),
c     *   wav_out_status(k)
c      enddo  
c      endif
c
c   find if skipping, if so correct number of points and sample rate
c xxx
c      if(max_points.gt.0) then
c        do i=1,wav_out_nchan
c          k=wav_out_chan(i)
c          nskip(k)=int(wav_out_nsamp(k)/max_points)
c          if(nskip(k).lt.1) nskip(k)=1
c        enddo
c      endif          

      if(max_points.gt.0) then
        do i=1,wav_out_nchan
          call signal_to_visual(wav_out_chan(i),max_points,1)
        enddo
      endif
c
c set headers for rsam data
c
      if (rsam_flag) then
        do i=1,wav_out_nchan
          call signal_to_rsam(wav_out_chan(i),1)
        enddo
      endif
       
c
c-------------------------------------------------------------------------
c   section to write out, different formats possible
c-------------------------------------------------------------------------
c
c   Make main header if needed  and open output file, decide name of
c   output file
c
c --------------------------------------------------------------------------
c   decide on file name
c---------------------------------------------------------------------------
c
c  if an output file name is given and the standard SEISAN name not used,
c  the use the given name 
c
      if(wav_out_file.ne.' '.and.wav_out_file.ne.'SEISAN') then !lo 2004
          file_out=wav_out_file

c
c  a standard SEISAN name is asked for
c
      elseif (wav_out_file.eq.'SEISAN') then
          file_out='SEISAN'
c
c   case of intercative input
c
cxx      elseif (format_out.ne.' '.and.narg.lt.2) then   
cxx          file_out=format_out
c
c  no name given, use default
c
      else
          file_out='wavetool.out'
      endif

c
c fix sample rate and number of samples according to skipping
c xxx
c      do i=1,wav_out_nchan
c        k=wav_out_chan(i)
c        wav_out_rate(k)=wav_out_rate(k)/nskip(k)
c        wav_out_nsamp(k)=wav_out_nsamp(k)/nskip(k)
c      enddo

      net_code='MERGE'
c
c  take net code from SEISAN.DEF
c
      if(merge_wav.ne.' ') then
         net_code=merge_wav
      endif
c
c   if all channels have the same station name, use that for net code
c
c      text(1:5)=wav_stat(wav_out_chan(1))
      text(1:5)=wav_out_stat(wav_out_chan(1))
      do i=1,wav_out_nchan
        k=wav_out_chan(i)
c        if(text(1:5).ne.wav_stat(k)) goto 888
        if(text(1:5).ne.wav_out_stat(k)) goto 888
      enddo
      net_code=text(1:5)   ! use station code for net code
 888  continue
c 
c
c
c   make main header, the out variabels are used since the file might have been
c   cut so no longer the read start time
c
c
         do i=1,wav_out_nchan
            k=wav_out_chan(i)
            year(i)=wav_out_year(k)
            month(i)=wav_out_month(k)
            day(i)=wav_out_day(k)
            hour(i)=wav_out_hour(k)
            min(i)=wav_out_min(k)
            sec(i)=wav_out_sec(k)
c            stat(i)=wav_stat(k)
c            comp(i)=wav_comp(k)
            stat(i)=wav_out_stat(k)
            comp(i)=wav_out_comp(k)
            location(i)=wav_location(k)
            network(i)=wav_network(k)
c xxx        rate(i)=wav_out_rate(k)/nskip(k)
c            nsamp(i)=wav_out_nsamp(k)/nskip(k)
            rate(i)=wav_out_rate(k)
            nsamp(i)=wav_out_nsamp(k)
c            cbyte(i)=wav_cbyte(k)
            cbyte(i)='4'  ! lot 30 April 2002
         enddo

c
c   make seisan main header, also used to make file name, if not given
c
         call sheads(year,month,day,hour,min,sec,wav_out_nchan,1,
     *                 net_code,h_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 file,mainhead,chahead)
 
c
c   if only one chan, add component so no overwrite
c
         if(wav_out_nchan.eq.1) then
           text=file
           file=text(1:seiclen(text))//comp(1)
           do i=1,seiclen(file)
             if(file(i:i).eq.' ') file(i:i)='_'
           enddo
         endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c  use seisan name if file_out is SEISAN
c
         if(file_out.eq.'SEISAN') file_out=file
         if(file_out.eq.'MSEED') file_out=file

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c----------------------
c   SEISAN format
c----------------------
c


      if(format_out.eq.'SEISAN') then
         h_text='Extracted waveform file '   ! title
       
c        write(6,'(1x,a)') file

c
c   check if file exist, if so add SEI to name, jh mar 06
c   but only if no specific output file name has been given
c

         if(wav_out_file.eq.'SEISAN') then
         inquire(file=file_out,exist=exist)
         if(exist) then
            file_out=file_out(1:seiclen(file_out))//'_SEI'
         endif
         endif
         do i=seiclen(file_out)-3,seiclen(file_out)
           if (file_out(i:i).eq.' ') file_out(i:i)='_'
         enddo
c
c   if a correction is made, write in main header
c
           
          if(ground.or.flow.ne.0.0.or.fhigh.ne.0.0) then
                                  mainhead(1)(2:13)='Filtered raw'
             if(ground_type.eq.1) mainhead(1)(2:13)='Displacement'
             if(ground_type.eq.2) mainhead(1)(2:13)='Velocity    '
             if(ground_type.eq.3) mainhead(1)(2:13)='Acceleration'
             if(flow.ne.0.0.and.fhigh.ne.0.0) then
                if(flow.lt.10.0) write(mainhead(1)(15:19),'(f5.3)')flow
                if(flow.ge.10.0) write(mainhead(1)(15:19),'(f5.1)')flow
                if(fhigh.lt.10.0.and.fhigh.ge.0.0) 
     *                       write(mainhead(1)(21:25),'(f5.3)') fhigh
                if(fhigh.ge.10.0) write(mainhead(1)(21:25),'(f5.1)')
     *          fhigh
                if(flow.ne.0.0.or.fhigh.ne.0.0) then
                   mainhead(1)(27:28)='hz'
                   mainhead(1)(20:20)='-'
                endif
             endif
             

         endif
  
         do i=1,8
            write(6,'(1x,a)') mainhead(i)(1:78)
         enddo
         open(96,file=file_out,status='unknown',form='unformatted')
         do k=1,12
            write(96)mainhead(k)
         enddo
         if(wav_out_nchan.gt.30) then
            k=(wav_out_nchan-31)/3+1
            do i=13,k+12
               write(96)mainhead(i)
            enddo       
         endif                       
      endif
c
c----------------------------------------------------------
c  GSE format has no main header, gse is also used as a temporary
c  format to make SEED
c----------------------------------------------------------
c         
c
c   make file name
c
      if(format_out(1:3).eq.'GSE'.or.
     &   format_out(1:4).eq.'SEED') then

        if (format_out(1:4).eq.'SEED') then
          seed_file_out=' '
          seed_file_out=file_out(1:seiclen(file_out))
c
c   add SEED if standard name
c
          if(wav_out_file.eq.' '.or.wav_out_file(1:6).eq.'SEISAN')
     *    seed_file_out=file_out(1:seiclen(file_out))//'_SEED'
        endif

        if (format_out(1:3).eq.'GSE'.or.
     &      format_out(1:4).eq.'SEED') then
c
c   add GSE if standard name
c
          if(wav_out_file.eq.' '.or.wav_out_file(1:6).eq.'SEISAN')
     *        file_out=file_out(1:seiclen(file_out))//'_GSE'
        endif
c
c   delete file, why ?
c
        open(96,file=file_out,status='unknown')
        close(96)

        open(96,file=file_out,status='unknown')
      endif

c
c----------------------------------------------------------
c  SAC nothing done about header
c----------------------------------------------------------
c

c
c-----------------------------------------------------------
c   MSEED, just check file name, open and write later
c-----------------------------------------------------------
c
      if(format_out(1:5).eq.'MSEED') then
cjh 11-2-13        if (file_out(1:5).eq.'MSEED') then
cjh          file_out=file(1:seiclen(file))//'_MSEED'
c
c  add MSEED to name if not a given name
c
          if(wav_out_file.eq.' ') then
             file_out=file_out(1:seiclen(file_out))//'_MSEED'
c2014-01-01-0800-00S
c          elseif(wav_out_file.eq.'MSEED') then
          elseif(wav_out_file.eq.'SEISAN') then
            file_out(19:19)='M'
          endif
cjh        endif
      endif          
c
c----------------------------------------------------------
c  Channel processing and write out loop
c-----------------------------------------------------------
c
      do i=1,wav_out_nchan
         k=wav_out_chan(i)
c
c   read whole channel
c
         if (nodata(k)) then
           write(*,*) ' no data found, set to 0 '
           do l=1,wav_out_nsamp(k)
             signal1(l)=0.
           enddo
         else
           call wav_read_channel(k)
         endif
         
c
c   correct number of samples, reading process might have found a gap
c   commnet out dec 19 2007 jh: this was wrong since the the number 
c   of samples in whole interval was  was used, not the number in
c   selected interval, seemed to only affect seed since this number was 
c   reset after reading
c
c        nsamp(k)=wav_out_nsamp(k)
cjh
c         write(6,*) 
c     *  'nsamp,out,wav',nsamp(k),wav_out_nsamp(k),wav_nsamp(k)
c        write(6,*)' Samples after correction',nsamp(k)

         if((flow.ne.0.0.and.fhigh.ne.0.0).or.ground) then
c
c remove DC
c
            ndc=0           
            call remove_dc(signal1,ndc,dc,wav_nsamp(k))
c
c  taper signal
c
c            call applytaper(signal1,wav_nsamp,10.)   ! taper, width 10% of half the samples
            call applytaper(signal1,wav_nsamp(k),10.)   ! taper, width 10% of half the samples
         endif

c
c  
c   filter is now always (2012) in time domain and one way
c
c
         if((flow.ne.0.0.and.fhigh.ne.0.0)) then ! must be bndpas
c
c   check sample rate before filtering
c
        if(fhigh.ge.wav_rate(k)/2.0) then
            write(6,'(a,f8.2)')' Sample rate is: ',wav_rate(k)
            write(6,'(a,f8.2)') ' Filter is:      ',fhigh
            write(6,'(a)')' Cannot filter **********'
         else
c

c           write(6,*) ' filter ',flow,'-',fhigh
            call bndpas(flow,fhigh,1000.0/wav_rate(k),
     *              cof,gain)
            call filter(signal1,wav_nsamp(k),cof,gain,1)   ! 4 pole one way
         endif
        endif
c
c compute ground motion
c
        if (ground) then
          wav_current_chan(1) = k
          wav_resp_file = ' '
c
c   There currently no use of poles since only 4 pole time domain 
c   filters are used. However, poles are still in the call to rem_resp but not used.
c

          call read_resp
          if (wav_resp_status.ne.'9') then 
c           write(6,*) ground_type,flow,fhigh,npole_low,npole_high
c
            call remove_resp(signal1,y_com,wav_nsamp(k),
     &      wav_rate(k),ground_type,
     &      flow,fhigh,npole_low,npole_high,zero,pole,
     *      nzero,npole1,norm)   
          endif
        endif
c
c compute rsam data
c
       if (rsam_flag) then
         call remove_dc(signal1,ndc,dc,wav_nsamp(k))
         call signal_to_rsam(k,2)
       endif

c 
c reduce signal to what is seen
c
      if(max_points.gt.0) then
        call signal_to_visual(k,max_points,2)
      endif
c
c   cut out data and put data in beginning of array
c
         j=0
         do l=wav_out_first_sample(k),wav_out_nsamp(k)+
     *        wav_out_first_sample(k)-1
            j=j+1
            signal1(j)=signal1(l) 
          enddo
c
c   skip points
c xxx
c          j=0
c          if(nskip(k).gt.1) then
c             do l=1,wav_out_nsamp(k),nskip(k)
c                j=j+1
c                signal1(j)=signal1(l)
c             enddo
c
c fix sample rate and number of samples according to skipping
c
c             wav_out_rate(k)=wav_out_rate(k)/nskip(k)
c             wav_out_nsamp(k)=wav_out_nsamp(k)/nskip(k)
c          endif

c
c   write out, different formats
c
c
c-----------------
c  SEISAN format
c-----------------
c
         if(format_out.eq.'SEISAN') then
            call sheads(year,month,day,hour,min,sec,wav_out_nchan,i,
     *                 'NEWAV',h_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 file,mainhead,chahead)
c
c   put in location and network
c
            chahead(8:8)=location(i)(1:1)
            chahead(13:13)=location(i)(2:2)
            chahead(17:17)=network(i)(1:1)
            chahead(20:20)=network(i)(2:2)
	    write(6,'(1x,a)') chahead(1:79)
c
c   if filtered or correction for ground response, values might be very small.
c   SAC data also can have small valaues since reals.
c   if any number in file are in the range 0 < 1, then a gain factor is used.
c
c
c   numbers are multiplied by a gain factor and the gain factor is put in header
c
           
c
c   find largest value to be sure data can be scaled up or down if needed
c   and at the same time check if any small values
c
           ymax=0.0
           small=.false.
           do l=1,nsamp(i)
              if(abs(signal1(l)).gt.ymax) ymax=abs(signal1(l))
              if(abs(signal1(l)).gt.0.0.and.abs(signal1(l)).lt.1.0)
     *        small=.true.
           enddo
c
c  scale if small
c
           if(small) then
c
c  find factor
c
             factor=ymax/1.0e9

             do l=1,nsamp(i)
                 signal1(l)=signal1(l)/factor
             enddo
             chahead(76:76)='G'
             write(chahead(148:159),'(g12.7)') factor  ! save factor in header
          endif

          write(96) chahead
c
cjh apr 2003            if(cbyte(i).eq.' '.or.cbyte(k).eq.'2') then
cjh              do l=1,nsamp(i)
cjh                 signal2b(l)=signal1(l) 
cjh              enddo
cjh              write(96)(signal2b(l),l=1,nsamp(i))
cjh            else
           do l=1,nsamp(i)
              signal4b(l)=signal1(l)
           enddo
           write(96)(signal4b(l),l=1,nsamp(i))
cjh            endif
         endif
c
c---------------------------------------
c   GSE format
c---------------------------------------
c
        if(format_out(1:3).eq.'GSE'.or.
     &     format_out(1:4).eq.'SEED') then
c
c   make sure number is not too big
c
           do l=1,nsamp(i)
              if(signal1(l).gt.  100000000.0) signal1(l)= 100000000.0
              if(signal1(l).lt. -100000000.0) signal1(l)=-100000000.0
           enddo
           if (seisweb) then
              do l=1,nsamp(i)
                 signal1(l)=signal1(l)*10. 
              enddo
           endif
 
           if(format_out(1:5).eq.'SEED ') then
             call write_seisan_to_gse(96,k,.true.,'GSE2SEED')
           elseif(format_out(1:6).eq.'SEEDDL') then
             call write_seisan_to_gse(96,k,.true.,'GSE2SEEDDL')
           else
             call write_seisan_to_gse(96,k,.true.,format_out)
           endif

c
c---------------------------------------
c   SAC format
c---------------------------------------
c
        elseif(format_out(1:3).eq.'SAC') then
          call write_seisan_to_sacbin(k,.false.,
     *     .true.,nerr)
c     *     .true.,evla,evlo,evel,nerr)
c
c-----------------------------------------
c   MINISEED format
c------------------------------------------
c
         elseif(format_out(1:5).eq.'MSEED') then
c
c  Append flag        0 - Replace    1 - Append
           if(i.eq.1) then
              appendFlag = 0
           else
              appendFlag = 1
           endif
c
c  Compression flag   0 - STEIM1     1 - STEIM2
c 
           compFlag = 0
c
c  Record Size
c
           recSize = 4096
           seed_comp(1:2)=comp(i)(1:2)
           seed_comp(3:3)=comp(i)(4:4)
c
c   transefer data to integer array
c
           do l=1,nsamp(i)
              signal4b(l)=signal1(l)
           enddo     
c
           call seed_channel_write(file_out,
     *     stat(i), seed_comp, network(i), location(i),
     *     year(i),month(i),day(i),hour(i),min(i),sec(i),rate(i),
     *     nsamp(i),signal4b,
     *     appendFlag,compFlag,recSize,wav_time_error(i))
       
        endif
      enddo
c
c   close output waveform file
c
      if (format_out(1:6).eq.'SEISAN'.or.format_out(1:3).eq.'GSE'.
     *or.format_out(1:4).eq.'SEED')
     *close(96)

      if (format_out(1:4).eq.'SEED') then
        system_call = ' '
        system_call = 'gse2seed -i '//file_out(1:seiclen(file_out))
     &    //' -o '//seed_file_out(1:seiclen(seed_file_out))
        write(*,*) system_call(1:seiclen(system_call))
        call systemc(system_call,seiclen(system_call))
      endif

c-----------------------------------------------------
      goto 500
c
 999  continue
      write(6,*)' Wavetool terminated: error'
      if(seisweb) then
        write(6,*)'Wavetool terminated: error' 
      endif
      goto 600
c
 500  continue

      if(seisweb) then
         write(6,*)' Waveform extraction terminated: ok'
      endif

      text=' '
      text(1:2)='OK'
      text(3:62)=file_out(1:60)
      open(1,file='extract.mes',status='unknown')
      write(1,'(a)') text
      close(1)
c      call put_seisan_message(text)
      if (format_out(1:4).eq.'SEED') then
        write(6,'(1x,a,a)')' Output waveform file name is ',
     &       seed_file_out
      elseif(format_out(1:3).ne.'SAC')then  ! do not give name if SAC, single files
        write(6,'(1x,a,a)')' Output waveform file name is ',file_out
      endif

      if(in.eq.1) goto 11
 600  continue

      stop
      end


      subroutine signal_to_visual(ichan,max_points,option)
c
c reduce number of samples in single trace, so that total windows of all
c traces will have max_points 
c 
c  input: ichan      - channel index for wav_out
c'         max_points - number of points for total window
c         option     - 1: header only, 2: header and data
c
      implicit none
      save  
      include 'seidim.inc'
      include 'waveform.inc'

      integer ichan                   ! channel index
      integer max_points              ! number of points in total time window
      integer option                  ! option
      real data(max_sample)           ! data array
      real min,max                    ! min and max in interval delta
      double precision delta          ! size of time window to take data
      double precision stop,now
      integer nsamp_save(max_trace)
      real rate_save(max_trace)
      integer i,k
      integer ind(2)                  ! index to sample of min and max

c
c option 1, only set wav_out_nsamp and wav_out_rate
c
      if (option.eq.1) then
c
c get length of total output window
c
        call wav_out_index_total
        delta = wav_out_total_time / max_points
c       write(*,*) ichan,wav_out_rate(ichan),
c    *         wav_out_nsamp(ichan),wav_out_total_time
        nsamp_save(ichan)=wav_out_nsamp(ichan)
        rate_save(ichan)=wav_out_rate(ichan)
        wav_out_rate(ichan)=1./delta
        wav_out_nsamp(ichan)=wav_out_duration(ichan)/delta
c       write(*,*) ichan,wav_out_rate(ichan),
c    *         wav_out_nsamp(ichan)
        return 
 
      elseif (option.eq.2) then
c
c reset samples and rate
c
        wav_out_rate(ichan)=rate_save(ichan)
        wav_out_nsamp(ichan)=nsamp_save(ichan)
c
c get length of total output window
c
        delta = wav_out_total_time / max_points
c
c go out if max_points larger than number of samples
c
        if (max_points.ge.wav_out_nsamp(ichan)) return

c
c get time interval for new sampling
c
        stop  = wav_abs_time(wav_first)+wav_out_start(ichan)+delta

        k=1
        min=signal1(wav_out_first_sample(ichan))
        ind(1)=wav_out_first_sample(ichan)
        max=signal1(wav_out_first_sample(ichan))
        ind(2)=wav_out_first_sample(ichan)
 
c        write(17,*) 'max,min ',max,min

c
c loop through all samples
c
        do i=wav_out_first_sample(ichan),wav_out_first_sample(ichan)+
     *       wav_out_nsamp(ichan)-1
c
c time of sample
c
          now=wav_abs_time(wav_first)+wav_out_start(ichan)+
     *     (i-wav_out_first_sample(ichan))/wav_out_rate(ichan) 

c         write(17,*) now,stop,ichan,wav_out_first_sample(ichan),
c    *           signal1(i)
          if (now.le.stop) then                  ! check if sample in interval
            if (signal1(i).ge.max) then
              max=signal1(i)
              ind(2)=i
            endif
            if (signal1(i).le.min) then
              min=signal1(i)
              ind(1)=i
            endif
c
c sort with time
c
            if (ind(2).le.ind(1)) then
              data(k)=max
              data(k+1)=min
            else
              data(k+1)=max
              data(k)=min
            endif
          else                                   ! new interval
c            write(17,*) k,data(k),data(k+1)
            k=k+2
            stop=stop+2*delta
            max=signal1(i)
            min=signal1(i)
            data(k)=max
            data(k+1)=min
          endif
        enddo

c
c set number of samples and first sample in wav_out
c
        wav_out_rate(ichan)=1./delta
        wav_out_nsamp(ichan)=wav_out_duration(ichan)/delta
        wav_out_first_sample(ichan)=1
c
c write data to signal1
c
        do i=1,wav_out_nsamp(ichan)
          signal1(i)=data(i)
        enddo

c
c change number of samples and sample rate
c
      endif

      return
      end


      subroutine get_argument_from_file(arguments,nargs)
	implicit none
	include 'libsei.inc'
	  character*80 arguments(*)
	  integer nargs
	  integer read1 
	  integer seiclen
	  character*80 strline
	  integer code
	  logical b_flag
	  integer i

        call sei open( old$+warn$,       ! Open file & warn of erro.
     &                   ' ',              ! Prompt (n/a).
     &                   arguments(2),     ! Filename.
     &                   read1,            ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
          if( code .ne. e_ok$ ) then       ! An error or does not exist.
                write(6,'(a,a)') ' File does not exist: ',arguments(2)
                stop                          ! stop if input from choise file                           !
           endif
         do i=1,9999
          read(read1,'(a)',end=9711) strline
	    arguments(i) = strline(1:seiclen(strline)) 
         enddo
 9711	   nargs = i - 1
	   close(read1)
        return
	 end

      subroutine signal_to_rsam(ichan,option)
c
c compute 1 s p minute rsam data
c
c  input: ichan      - channel index for wav_out

      implicit none
      save
      include 'seidim.inc'
      include 'waveform.inc'

      integer ichan                   ! channel index
      real data(max_sample)           ! data array
      double precision delta          ! size of time window to take data
      double precision msec1,msec2,start
      integer i,k
      integer year,month,day,hour,min,doy
      real sec,secprev
      real  rsam
      integer nrsam
      integer option

      if (option.eq.1) then

        msec1=wav_abs_time(ichan)
        msec2=wav_abs_time(ichan)+wav_nsamp(ichan)/wav_rate(ichan)

        k=int((msec2-msec1)/60.)
        call sectim(msec1,year,doy,month,day,hour,min,sec)
        if (sec.ne.0.) k=k+1
        call sectim(msec2,year,doy,month,day,hour,min,sec)
        if (sec.ne.0.) k=k+1

        wav_out_rate(ichan)=1./60.
        wav_out_nsamp(ichan)=k
        wav_out_first_sample(ichan)=1
      else
c
c compute rsam data
c
        secprev=-1
        rsam=0.
        k=0
        do i=1,wav_nsamp(ichan)
c
c get time of sample
c
          msec1=wav_abs_time(ichan)+(i-1)/wav_rate(ichan)
          call sectim(msec1,year,doy,month,day,hour,min,sec)
          if (i.eq.1) then
            wav_out_sec(ichan)=0.
          endif
          if (sec.lt.secprev) then
            k=k+1
            if (nrsam.ne.0) then
              data(k)=rsam/float(nrsam)
            else
              data(k)=0
            endif
            nrsam=1
            rsam=abs(signal1(i))
            secprev=-1
          else
            secprev=sec
            nrsam=nrsam+1
            rsam=rsam+abs(signal1(i))
          endif
        enddo
        k=k+1
        data(k)=rsam/float(nrsam)
c
c write data to signal1
c
        do i=1,wav_out_nsamp(ichan)
          signal1(i)=data(i)
        enddo

      endif
      return
      end
