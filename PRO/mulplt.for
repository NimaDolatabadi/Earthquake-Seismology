c**************************************************************************
c                                                                               
c     Program to pick phase arrival times and write to SEISAN data base.        
c                                                                               
c     Written by Jens Havskov                               
c
ccccccccccccc   mulplt scematically ccccccccccccccccccccccccccccccccccccccccc
c
c  mulplt has 3 important loops where goto is used. below is seen the
c  loop labels (1000, 1100 and 2000) to the right and to the left is shown
c  all instances of goto to these labels.
c
c
c     program start
c
c     initialize mulplt
c        - get parameters from mulplt.def
c        - get iasp synthetic readings
c        - find if input from eev (opmode 0) or filenr.lis (opmode 1)
c          opmode 1 can also be cont data base
c
c---------------------------------------------------------------------------
c
c 1000 continue   ! loop back point for next event, filenr.lis or eev
c
c      if (input from eev, opmode = 0) then
c         if (plot finished) then
c            stop
c            return to eev
c         else
c            read waveform file names from s-file
c            select waveform files to use
c         endif
c      endif
c
c      if(input from filenr.lis, opmode = 1) then     
c         get file name of first or next event
c         if(filename(1:4) = 'cont') then
c             continuous data base input, cwaw=.true.
c             read start time and interval
c             goto 1050
c         endif
c         if(filename = 'filenr.lis') then
c            opmode=2              ! many hard copy plots
c            enter input of multi event plot parameters
c         endif
c      endif
c
c      if(opmode = 2)  then
c         read next waveform file name
c         if(no more files) stop                              --- goto 999
c      endif
c
c      check if waveform file exist and where it is
c
c      if(not exist)   get next event or back to eev          --- goto 1000
c
c      read waveform file(s) headers and initilize variables
c
c      if(opmode = 2) then
c         plot one event, hard copy only
c         back for next event                                ---  goto 1000
c      endif
c
c 1050 continue   
c      if(cwav)
c         read cont event headers
c      endif
c
c      if(opmode.eq.1) make an s-file
c-----------------------------------------------------------------------------
c
c      input of general options 0-6
c
c      if(option 4-6) then
c         continuous plot
c         stop
c      endif
c
c-----------------------------------------------------------------------
c  multi trace mode
c-----------------------------------------------------------------------
c
c      if(option 1-3) then
c
c         additional questions for multi trace option
c
c 1100    continue  ! loop back point for multitrace choices
c
c            -multi trace plot
c            -fk                                         
c            -pick                                      
c            -registation
c            -etc, for all above return to 1100          --- goto 1100
c
c            - go to single channel mode                 --- goto 2000
c
c            - delete s-file                            
c            - merge s-files                            
c            - delete wav-file(s)    
c            - register, input from eev                 
c            - other waveform file       
c            - next event
c            - for all above, return to 1000             --- goto 1000
c      endif
c
c--------------------------------------------------------------------------
c  single trace mode
c--------------------------------------------------------------------------
c
c     if(option 0) then
c
c        additional questions for single trace mode
c
c 2000   continue  ! loop back point when finished one channel
c          - single channel options                      ---- goto 2000
c          - jump to multi trace mode                    ---- goto 1100
c          - end of channels                             ---- goto 99
c        goto 2000
c     endif
c
c 99   continue
c
c      If input from filenr.lis, goto to next event      ---- goto 1000
c
c 999  continue
c
c      possibly send plot to plotter
c
c      close files
c
c      stop
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Latest update:                                                            
c     nov 20         : major clean up for 5.0
C!JAB(BGS)Dec94      : Install file & error handling (libsei)
c     jh jan 95      : file open problem, not properly closed
c     feb 20 95      : add question for hard copy plot
C!JAB(BGS)Mar95      : Accomodate large filenames in commands.
C!JAB(BGS)Mar95      : Accomodate the large suffix filenames.
C!JAB(BGS)Mar95      : If filename given, do not crash out if no filenr.lis
C!JAB(BGS)Mar95        exists!.
c     March 20 by jh : Resolution in mulplt.def file
c     March 21       : Separate resolution in screen and hc
c                      correct bug if readings from next day realtive to
c                      nordic header
C!JAB(BGS)Mar95      : Prevent core crashing for invalid user entry.
c april 10 94 jh     : Bugs with saving pics when not using data base
c april 21 94 jh     : component rotation
c april 26 94 jh     : extrac option for hc
c may 11             : enter both distance id and event type for registration
C!JAB(BGS)Jun95      : Use "sei get file" rather than "sei open" to open wave
C!JAB(BGS)Jun95        files in all cases, except where its pathname has already
C!JAB(BGS)Jun95        been found.
c jun 8       jh     : indicate in boxes if phases read
c jun 9              : fix problem with option OTHER
c sep 21             : disable rotate after traceplot
c oct 95             : new seisinc, add ffmin and ffmax to MULPLT.DEF,
c                      allow omega0 to be less than zero
c dec 95             : continous plotting
c dec 21             : check amplitude
c jan 8 96           : do not use synthetic data if not form the same date
c                      as the readings
c jan 16             : bug in above
c jan 31             : include nhead and nrecord in mulplt.inc
c feb 6              : bug cont. plot
c feb 13             : get distance irrespective of channel
c 5 3 96             : bug iasp in multitrace mode
c 12 3 96            : put routine convert into picsub
c aug 1 96           : fix problems wiht shifts in time  multi trace - single tr
c     8              : one extra return cleared in hc input
c    12              : wrong header in output when not using data base
c    28              : add hourr to commom block
c dec 27             : some modifications to phases: mouse is now blan on sun
c                      as on pc, lg put on = or \, sn has no i or e, L and l
c                      is locate
c feb 12, 97         : bug with hardcopy in single trace mode
c feb 20, 97,        : add write when locate
c bjb(BGS)Mar97	     : allow sub-classes for LV events to be entered in S-file
c                      and phase picks to be saved to s-file
c apr 16, 97 lo      : minor changes on volcanic part to solve Solaris problem
c apr 23, 97 lo      : plot iasp.out phases, when header date in S-file 0709
c aug 2      jh      : convert a sun wav name to pc format if on pc
c oct 97             : muilti file, spectral output
c nov 18             : revision
c dec 3              : output of file
c feb 11 98          : enable seach in several wav data bases
c feb 18             : both p and s velocity for spectral analysis
c feb 25             : fix eev connection
c mar 2              : more -------------
c mar 9
c mar 22             : fix rotation and amplitude
c jul 9  lo          : fix sfile gets lost when merge on solaris
c --------------------------------------------------------------------
c sep 98 jh          : ----------   version 6.0 check ----------------
c                      year 2000, 5 char station names, 5 char base
c oct 7              : bugs
c nov 4  jh          : put logical for linux
c nov 5              : systemc to systemc
c nov 26 98 jh       : add kappa
c jan 04 98 lo       : bug, read 5 characters when register
c jan 7     jh       : bug on merge, enlarge fileds for wav file name
c feb 3              : mores space for box_text
c feb 28             : fix fixed scaling, weigh 8 will not work on sun
c mar 19             : use save in main program
c mar 22 bmt         : include winplot.inc
c may 18 lo          : save fstart and ev_number when starting mulplt
c                    : save sfile when permanent merging
c may 28 jh          : put in FK
c jun 10             : add phase AMP
c jun 13 jh          : main_time to time, wrong addressing
c aug 16             : day was written wrongly in mulplt.out
c aug 26 lo          : fix some text output on Linux
c sep  8 jh          : put waveform file name in mulplt.out
c sep  9 lo          : fix bug in new_s_file
c sep  9 lo          : when register and no phase read, del synth phases
c sep 21 jh          : save menu show to eev so it can get back to mulplt
c sep 22             : add fk reading
c sep 30             : bug in above
c oct  1 lo          : read CODA AUTO as 0/1
c oct  4             : bug in above
c oct 22 jh          : new picsub, revise channel selection
c nov 5              : add variable long_text to copy files to wav, text too
c                      short
c nov 28             : variable channelname put into mulplt.inc in order to fix
c                      amplitude picking in multi trace mode
c dec 10             : put in response spectrum, channels name in mulplt
c                      mide, fixed in picsub
c feb 16 2000 jh     : option for plotting output ffrom modeling directly
c feb 29             : add copy_wav_dir to copy directly to data base
c april 2000         : new waveform structure
c may 25 2000 lo     : fix call for extract in fk
c may 30      jh     : error with hard copy plotting
c may 31      jh     : terminate PostScript file also on quit
c jun 5              : option o in single trace fixed in case of defaults
c jun 18      jh     : error when jumping intoi single trace mode from
c                      channel other then one
c october 20         : do not use upper character e and i for phases, I used
c                      for iasp option
c Nov 5, 2000        : ******** MAJOR CLEAN UP ************************
c Jan 14 2001        : fix bug so it is possible to pick more than one iasp
c                      phase for one staiton in multi trace mode
c jan 24             : more filters choises for ml, mb and ms
c feb 14             : pathname stripped from wavefilename for registration
c                    : user selected filters defined in MULPLT.DEF
c                    : FILTER_TYPE defined to allow use of bndpas or recfil
c                    : Year and month for wave database registration changed
c                    : to use header variables rather than filename syntax
c feb 20          lo : Changed length of xhead
c mar 9           jh : bug in routine write_s_file, no error check on write
c apr  23         jh : remove last check for hard copy, caused crash at end
c june 14         jh : change a filter from 0.8 haz to 1.25 hz
c sep 11             : def wa-filter set to 8 poles
c nov 11             : extract from cont data base
c nov 18             : back option in multi trace mode
c feb  1, 2002    lo : write out message when register s-file,used by seisantool
c feb 7           jh : do not allow filter lower than 0.1 for cont mode
c april  2003     jh : multiple screens for one event, extract to wavetool,
c                      also extract filtered and response corrected traces,
c                      stations alphabetical
c june 25         jh : 1000 channels and files, multiple selection screens
c july 10         lo : ask for stat/comp for continuous plotting
c july 21         jh : G and N to g and n for crustal phases
c sep 4           jh : make it possibel to quit after reg. in cont. mode
c oct 3           lo : set time_scale to 0 at 1100 to fix linux problem
c dec 2004        jh : seed 
c jan 12 2005     jh : fix some menu problems
c mar 11 2005     jh : initilize n_wav_file and n_wav_del to 0, not doing so
c                      created problem for wav deletiopn on linux
c jun 20 2005     jh:  plt to eps
c nov 11 2005     jh:  initialize cwav and cseed
c mar 06 2006     lo : added selection of continuous databases
c mar 24 2006     lo : init cseed at start
c jan    2006     rl : some changes, set n_wav_file=0 after 1000
c jun 1  2006     jh : Above, nsynt also set to zero, so iasp option from
c                      mulplt did not work, nsynt=0 commented out
c mar 15 2007     jh : do not use nsort_distance_save, dist can only be 
c                      turned on
c jul 30 2007     lo : write file to give cbase to wavetool
c oct    2007     lo:  read list of continuous databases from file
c oct 16 2007     jh : add init of mem function
c feb 13 2008     lo : add particle motion
c apr 25 2008     jh : change ms and mb filters, see manual version 8.2
c sep 12 2008     jh : small change in writing out waveform file name
c oct 23 2008     jh:  error in format writing out in cbase.inp, made soemtimes
c                      a blank in form of base on Linux
c nov 12 2008     jh: Indicate when plotting continuous data
c dec 27 2009     jh: - add variabel wav_nchan_cseed to fix cseed plotting
c                     - rem def filters for ml, ms and mb, wood anderson high cut
c                     - removed, if filter poles is 0, do no longer put in 8 poles
c                     - commented out option to use filtes for mb and Ms
c mar  5 2010     lo  - took out chansel for continuous data extract
c may 5  2010     jh: rm test output to 17
c may 10 2010     wcc: add new features
c may 17 2010     jh: change position of header lines a bit os better fit to pc
c oct 5  2010     jh : there was no check if both horizontal channels present
c                       for rotation so if a singel horizontal was there, it would
c                       be rotated with something in memory !!!!
c oct 21 2010     jh: s-file is always sorted before asking for distance order,
c                     no onger possibel to plot in s-file order

c oct    2010     Jh: read bud archive
c nov 17 2010     jh: seiscomp
c dec 21 2010     jh: fix so boudary zero month and zero day works with arc
c dec 27 2010     Jh: gfortran on pc, remove winplot, use imlicit none, 
c                     unit 17 to unit 27, new window for single trace plot, 
c                     size of window
c jan 10 2011     jh: text change new window
c jan 12 2011     Jh: char 80 to 90 in make_res_file
c jan 16 2011     jh: add all component mode
c feb 02 2011     jh: chage y to Y for more sysnthetic phases
c feb 22 2011     jh: change call to xwindow_size
c feb 25 2011     Jh: fix distance sorting problem, was not working if iasp phases
c may    2011     pv: new online options
c oct 3  2011     jh: fix bug in above
c oct 27 2011     jh: file name was blank after registration
c                     add eev option for model, automatically change case for
c                     p,e,q (like when registering from eev)
c nov 28 2011     jh: add option to write out all channels if in cont or 
c                     arc data base
c dec 29 2011     jh: mt, add variable, sorting for R and T
c jan 30 2012     jh: increase stat_all to 1200
c mar 22 2012     jh: add option to toggle number of chans per screen
c apr 04 2012     jh: add option to select by area
c jun 06 2012     jh: fix dimension bug in above
c oct 11 2012     jh: add a parameter FIX FILTER yo MULPLT.DEF to fix filter (=1.0)
c nov 23 2012     jh: go back to multitrace mode at end of single trace mode
c dec 19 2012     jh: increase dimension of station cord. in mulplt_dist
c                     from 13000 to 20000
c jan 4  2013     jh: add agument to auto_tr, remove ref to BUD and SCP, only arc,
c                     add area select by distance to a station
c jan 19 2013     jh: wav_mem_init before new event
c jan 21 2013     jh: change to channels can be selected also if only
c                     the orientation code matches.
c feb 11 2013     jh: dimension fix
c feb 19 2013     lo: add magnification
c feb 25 2013     lo: allow def for sorted database selection
c feb 28 2013     lo: implement forward and backward in multi trace mode
c apr 9  2013     jh: add parameter spectral_model
c may 21 2013     jh: fix multiple delete for s-file
c sep 1 2013      jh: stop if not data in mulplt cont
c sep 24 2013     Jh: above was in wrong positionh sp arc stopped
c nov 15 2013     jh: option to select achive channels before they are
c                     read using list in seisan.def. stations not within given
c                     radius not shown, if option radius used. stations not
c                     active at the time not shown.
c dec 12 2013     jh: errror message
c dec 28 2013     jh: revision and update of the data extract sections which did not work
c                     in some cases of subselection of datbases or selection of 
c                     channesl for arc
c feb 3 2014      jh: bug when registering events with large seed
c feb 9 2014      jh:  more fixing in change of dec 28
c fen 25 2014     jh: put back * for weigh 8 on linux
c 2014.03.14      pv : cleanup warnings - GOTO...
c may 32 2014     jh: hardwired ml filter poles to 4
c octg 20 2014    jh: bug with station as midpoint
c jul 28 2015     jh: add argument to stat_loc
c oct 29 2015     jh: make it possible to register with ARC lines without
c                     extracting waveform file
c jan 06 2016     jh: add time limits to cbase.inp for arc, fix  so
c                     single channel extracted file name come out ok
c jan 30 2016     jh: foreward in all channel mode
c mar 12 2016     lo: added selection of virtaul channels for arc
c maj 26 2016     pv: added possibility to update sfile if the event is located
c aug 23 2016     Jh; fixed array out of bounds with cahnnelname_allc
c sep 27 2016     pv: added option to turn off logging
c sep 29 2016     pv: fixed bug in distance sorting for stations not in STATION.HYP
c nov  3 2016     pv: if IASP is deselected, iasp.out is now deleted, to avoid 
c                     conflict with the following events.
c jan  4 2017     lo: added udp communication with se, in case of reload
c                     of s-file MULPLT no longer closes graphics window
c jan 26 2017     lo: bug fix se, jh: fix placement of text
c 2017-12-01      pv: bug fix due to long path in hyp call, changed string from text to long_text
c dec 18 2017     jh: add more on plotting spectrogram
c jan 18 2018     jh: fix so def filter works with se
c jan 22 2018     jh: multichannel spectrogram
c feb  7 2018     jh: now send message to locate in se when using locate in mulplt   
c mar  5 2018     jh: plot epi or hypocentral distance, put in mulplt.def
c jun  4 2018     jh: clear phase keys not used

        implicit none
        include 'mulplt.inc'    ! mulplt includes
        include 'seiplot.inc'
        include 'seisan.inc'

c          
c-- operator
       character*4 operator
c-- header for plot                                   
       character*160 xhead			
       character*80 question                                                    
c-- path to seismo                             
       character*60 top_directory		
c-- answer                                            
       character*1 answer			
c-- directory for s files                             
       character*40 s_dir			
c--  file number when reading many from filenr.lis
       integer filenumber
       integer n_wav_file                       ! # wav files in s-file
       integer n_wav_out                        ! # wav files selected
       integer n_wav_del                        ! # ------ for deletion
       integer n_wav_mer                        ! # ------ for merging
       integer wav_nchan_cseed                  ! number of channels in cseed file
       character*80 wav_file_text               ! info on wav files(s)
       character*40 extra_wav                   ! extra wav file from S-file
c                                                 used with synthetics
c-- general text                                       
       character*80  text,text1 
       character*80  file_out                   ! complete wav name
       character*200 long_text
c-- time scal ein cm/sec                                
       real time_scale				
c-- multi trace start time                              
       real start_time				
c-- multi trace stop time                                
       real stop_time				
c-- stop_time - start_time                                  
       real window				
       character*1   model       ! model indicator when registrate
       logical  one_plot         ! true if one plot mode
       logical all_from_single   ! true if going to all from single
       logical allcomp           ! true if in all component mode
       logical allcomp_many      ! true if in ------------------ for several stations
       integer nchan_all_current ! pointer to current channel in all channel mode
       integer nchan_all_station ! number of channels for one station -----------
       character*5 stat_current  ! current station used in ----------------------
       logical single_open       ! true if singel channel is open
       logical def_single        ! tru if def mode is to single
       logical no_arc_extract    ! true if no extract from arc when reg.
      character*10      keys                    

c-- single channel windows, not used presently       
       real wt1(max_trace),wt2(max_trace)                     
c-- save filter for option 0
c       real flow_def,fhigh_def ! now in common block 6-98
       integer filt_def
c-- operation mode 0: with SEISAN                        
c--       integer opmode	in common block			
c         0: single files, from eev 
c         1: mult. fil., 2: many file auto hard copy only plot
c                                                                               
c-- counters for above
      integer nsynt			
      character*10 synt_head  ! date of synthetic records
      character*80 data1(max_data)
c-- unit for default file
      integer def_unit
c-- Length in sec. of datafile                           
      real total_time				
c-- plot option, see below                              
      integer p_option				
      integer old_plotoption         ! save plot option
      integer npick                  ! number of time picking routine is called form traceplot
c-- Vector with selected channels                 
      integer old_chan(max_trace)			
c-- Number of selected channels                            
      integer nchan,old_nchan				
c-- See routine: 'indata'                      
      integer nstat,nphase		
c-- counters                                    
      integer i,j,k,l,ichan,x,ich,kk,m,n
c-- See routine: 'indata'                            
      character*1 type,exp			
c-- type and kind
      character*2 type_kind
c-- Indicator if default parameters are chosen                   
      logical default,default_sort               
c-- filename of datafile including path                              
      character*80 filen			
c-- filename of datafile excluding path (bjb)                              
      character*80 trace_file			
c-- --------------------          
c-- name of current S file                             
      character*80 xsfile
c-- ---- new--- after registration                 
      character*80 new_sfile	
      character*23 start_time_duration ! for ARC line
      integer doy                      ! for ARC
      double precision msecs           ! for ARC		
      character*5 stat_def(max_trace)
      character*4 comp_def(max_trace)   ! default channels
      integer nchan_def                       ! number of -----
      integer channelname_win(max_trace) ! initial selection of channels for multi windows
      integer channelname_all(max_trace) ! expanded selection for all comp plotting
      integer channelname_all_old(50)    ! save expanded selection from one station -----
      integer channelname_allc (max_trace) ! -------------------------------------
      integer nchan_all                  ! number of channels -----------
      integer channelname_single(max_trace) ! in selection single mode
      integer nchan_single               ! number of channels --------------------
c-- dummy                                              
      character*6 dummy				
c-- See above                                            
      integer iswitch				
c-- id line number 
      integer id
c--- for extract of cont data
      double precision window_abs_time
      integer year,month,day,hour,min
      real sec
c-- .true. if a file exists                          
      logical exist
      			
c-- defualt for input parameters
      logical default_org
c-- save number of channels before interactive multi channel selection
      integer nchan_initial
c-- indicate if baz has been calculated
      logical no_baz
c-- directory separator
      character*1 dchar
c-- computer type
      logical pc,sun,linux
      character*5 station_code
      character*4 component_code
      character*(5) cbase_sel(max_cont) 
      character*5 stat_all(1200)   ! for sorting in all component mode
      character*4 comp_all(1200)
c  temporary arc values
      character*5       arc_stat_temp(3000)               ! staiton name
      character*3       arc_comp_temp(3000)               ! component
      character*2       arc_net_temp(3000)                ! network
      character*2       arc_loc_temp(3000)                ! location
      logical           arc_flag_temp(3000)
      double precision  arc_start_temp(3000)              ! start valid date
      double precision  arc_stop_temp(3000)               ! stop valid date
      double precision  arc_time                          ! abs time of start of request
      integer           arc_nchan_temp                    ! selected arc channels
c-- check if IASP/I is on
      logical iaspon
      

C--Variables for volcanic sub-classification
c bjb 18/3/97 
c
C-- comment line in VOLCANO.DEF
      CHARACTER*80 	comment
c-- User entered sub-class flag 
      CHARACTER*6	sub_class_flag
c-- sub-class descriptor
c      CHARACTER*40  	sub_class_descriptor
c-- sub class code
      CHARACTER*6       s_class(60)
c-- sub class number 
      INTEGER           n_class(60)
c-- user entered number 
      INTEGER           n_select
c-- sub-class description array
      CHARACTER*40  	description
c-- true if sub_class is in s_class()
      LOGICAL  		valid_class
c-- full pathname of VOLCANO.DEF
      CHARACTER*80	vol_file
c-- bjb variables for last dir character
      integer r_index,lastsl
      character*4 cwavyear
      character*2 cwavmon

      logical sfile_flag    ! true if : mulplt -sfile <sfile>

      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments
      integer hctype                          ! hardcopy type
      integer ichan_single
      integer display_type
      character*512 udptext ! udp message text for se
      integer msglen
      logical regflag   ! true if register sent to se


C
C    SEISAN library inclusions...
c    ----------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei get file,               ! Find & open file.
     &         sei get values,             ! Extract from text string.
     &         sei open,                   ! File open handler.
     &         sei close,                  ! & closure.
     &         sei real num,               ! Extract real number from string.
     &         sei integer,                ! Extract integer from string.
     &         sei clen,                   ! Length of string.
     &         sei code                    ! Error condition handler.
      integer  sei clen,                   ! & function.
     &         sei integer                 ! Ditto.
      real     sei real num                ! Ditto.
      integer  code,                       ! Condition.
     &         ix,                         ! Very local.
     &         read1,                      ! Read unit1.
     &         read2,                      ! Ditto 2.
     &         read4,                      ! & 4.
     &         write1,                     ! Write unit1.
     &         write2,                     ! Ditto 2.
     &         write44,                    ! 
     *         readfk,
     *         any_unit
      logical  b_flag,                     ! Flag!!
     &         b_filenr                    ! Using filenr.lis?.
               character*6 arc_text        ! for extraction from arc archive

C    ------- End of details -------
      include 'version.inc'
c
c                                                                               

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c this first section is only done at startup of program
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c print version
c
      out_version_date='July 23, 2001'
      arc_type=0
      if (version_new) out_version_date=version_date
      call print_ver

c---------------------------------------------------------------------------
c  get default parameters from MULPLT.DEF.  
c---------------------------------------------------------------------------
c
       call get_mulplt_def(def_unit,stat_def,comp_def,
     + nchan_def)     
c        
c   get seisan defaults
c
      call get_seisan_def

cccccc    inizialize     ccccccccccccccc                                        
c     b_f_debug$ = .true.
      b_filenr$  = .false.      !JAB(BGS)Mar95. Not using filenr.lis yet!
      one_plot=.false. ! no plot made yet
      wav_interactive=.true.
      sfile_flag=.false.

c-- postscript hard copy                                          
      hctype=1		
      rotate=.false.      ! cfix
c	  
c-- x-window size in % max
c
      wsize=90.0
c
c  initilize memory handling
c
      call wav_mem_init
c
c  position top frame in multi trace mode
c
c      max_ypos=678.0   ! before wayne
       max_ypos=670.0   ! jh new
c
c  graphic display not yet open
c
      disp_open=0
c
c  by default, no default are chosen
c
      default=.false.
      default_org=.false.

      port=0    ! where mulplt sends to
      myport=0  ! where mulplt listens
      fromse=.false.
c                                                                               
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)                                                
      call dir_char(dchar)   ! directory separation character
c
c   get computertype
c    
      call computer_type(sun,pc,linux)
c
c  no operator
c
      operator='    '
c                                                                               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   get arguments
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      call get_arguments(narg,arg)
      if(narg.ge.1) then
      do i=1,narg
            if(arg(i)(1:6).eq.'-sfile') then
               sfile=arg(i+1)
               sfile_flag=.true.
            endif
      if( arg(i)(1:13) .eq. '-plotdefault' ) then
        default=.true.
        default_org=.true.
      endif
      if( arg(i)(1:4) .eq. '-qdp' ) then
        default=.true.
        default_org=.true.
      endif
      if( arg(i)(1:3) .eq. '-se' ) then
        fromse=.true.
      endif
      if( arg(i)(1:5) .eq. '-port' ) then
        read(arg(i+1),'(i5)') port
      elseif( arg(i)(1:3) .eq. '-po' ) then
        default=.true.
        default_org=.true.
      endif
      if( arg(i)(1:9) .eq. '-operator' ) then
         operator=arg(i+1)(1:4)
      endif
      if( arg(i)(1:9) .eq. '-base' ) then
         s_dir(1:5)=arg(i+1)(1:5)
         do k=2,5
           if(s_dir(k:k).eq.' ') s_dir(k:k)='_'
         enddo
      endif
      if( arg(i)(1:5).eq.'-help'.or.arg(i)(1:3).eq.'-h') then
        write(6,*)' '
        write(6,*)' The MULPLT program - for plotting earthquake data '
        write(6,*)' '
        write(6,*)' Usage: mulplt [options]'
        write(6,*)'        enter input'
        write(6,*)' '
        write(6,*)' ## Options ##'
        write(6,'(a17,a15)')'    -help        '
     +,'Print this list'
        write(6,'(a17,a13)')'    -h           '
     +,'Same as -help'
        write(6,'(a17,a45)')'    -plotdefault '
     +,'Will plot data directly with default values   '
        write(6,'(a14,a17)')'    -operator '
     +,'Operator code    '
        write(6,'(a17,a20)')'    -qdp         '
     +,'Same as -plotdefault'
        write(6,'(a17,a20)')'    -po          '
     +,'Same as -plotdefault'
        write(6,'(a17,a15)')'    -version     '
     +,'Seisan version '
        write(6,*)' '
        write(6,*)' Examples:'
        write(6,*)'       1)  mulplt'
        write(6,*)'           enter values'
        write(6,777)
     +'        2)  mulplt -plotdefault -sfile ',
     +'/home/seismo/REA/TEST_/1996/06/03-1955-40D.S199606'
        write(6,*)' '
        goto 999
      endif

  777 FORMAT(a39,a50,1x)

      enddo
      endif

c
c open network port for incoming communication
c
      if (port.gt.0) then
        call init_network(myport)
c        write(*,*) ' debug myport is ',myport
c
c tell se what myport is
c
        write(udptext,'(a4,1x,i6)')'PORT',myport
        call send_udp_msg(port,udptext)
      endif

11    continue      ! jump here to load new sfile
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c this second section is resetting parameters 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      max_count  = 0   ! for fixed scaling
      no_baz=.true.    ! initially, no back azimuth angles have been calculated
c lot 24/03/2006, problem on RH Enterprise Linux
      cwav=.false.
      cseed=.false.
      ymag=1.
      forward=.false.
      backward=.false.
c-- check if IASP/I is on, default is off
      iaspon=.false.
      current_seq_chan=0     ! no active channel in traceplot
      npasses=0              ! filter
      cont_window=.false.    ! no change to window for cont plotting
      n_wav_file=0           ! initially 0
      n_wav_del=0            ! initially 0
      allcomp=.false.        ! initially not all component mode
      single_open=.false.
      mulplt_area_save=0     ! no area selected
      choice=' '
c      filt=0    ! prevented use of default filter, dec 2017
       filt_def=0
      arc=.false.
      extra_wav=' '
      do i=1,max_trace
        wav_filename(i)=' '
      enddo
c                                                                               
c   agency directory by default
c                                                                               
      s_dir='                                        '                          
      do i  = 1,16                                                              
        phase(i) = ' '                                                          
      enddo                                                                     
      do i = 1,max_trace                                                       
        channelname(i) = 0                                                      
      enddo                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c---------------------------------------------------------------------                                                                               
c   get synthetic readings if file iasp.out exists    cccccc                          
c---------------------------------------------------------------------
c
       call get_synt(nsynt,synt_head)

c---------------------------------------------------------------
c   look for s-file reference to check if from eev                             
c---------------------------------------------------------------
c
      if(sfile_flag) then
        write(6,*)'Reading sfile'
        write(6,*)sfile
      else
        call get_env_event(sfile)
      endif
      call put_env_event(sfile)    ! put back in so iasp can use it
      opmode=0                     ! default from eev

      if(sfile(1:3).eq.'   '.or.ichar(sfile(1:1)).eq.0) then
         opmode=1
         nrecord=0
         nhead=0
         sfile='mulplt.out'   ! make an initial s-file
         open(1,file=sfile,status='unknown')
         write(1,*)
         close(1)
      elseif(.not.fromse) then
         extra_wav=' '  ! initially no extra wav file
         call get_env_string(text)  ! get options from eev
c        write(6,*)'mulplt eev mes',text
         if (seiclen(text).gt.0) then   ! if (lo 6 March 2017)

           def_single=.false.   ! no default for single
           if(text(1:1).eq.'o'.or.text(1:1).eq.'O') then
             default=.true.
             default_org=.true.
             filt_def=filt
             if(text(2:2).eq.'o') def_single=.true. ! default from single
           endif

           if(text(2:2).eq.'A') arc_by_default=1

           read(text(3:8),'(i6)') event_no    ! get event number
           read(text(12:12),'(i1)') show_menu  ! get menu show option
           extra_wav(1:40)=text(40:79)
           seisan_base=text(16:20)
           eev_start_time=text(21:34)
         endif
      endif
c
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c   enter here if: 
c   1: one or several waveform files deleted from s-file
c   2: merge  
c   3: next from eev
c   4: next or previous file from filenr.lis
c   5: select other waveform files
c------------------------------------------------------------------------
c
 1000 continue                                          
c
c   initialize waveform file info
c
      call wav_init
c     nsynt = 0                 ! RL 1/2006
      n_wav_file = 0            ! RL 1/2006

c
c-----------------------------------------------------                                                                     
c-- data base, next file or other actions, from eev                                                 
c-----------------------------------------------------
c
       if(opmode.eq.0) then
c
c   directly from data base, select eev action if one plot already done
c   and return to eev
c
         if(one_plot) then
             text= ' '
             text(1:10)=keys
             write(text(12:12),'(i1)') show_menu  ! save menu option
             call put_seisan_message(text(1:seiclen(text)))
c
c return udp message to se
c
             if (fromse.and.port.gt.0) then
               regflag=.true.
c delete sfile
               if (text(1:4).eq.'D   ') then
c                 udptext='DELETE '//sfile(1:seiclen(sfile))
                 udptext='DELETE'
c next event
               elseif (text(1:4).eq.'NEXT') then
                 udptext='NEXT '
c previous event
               elseif (text(1:4).eq.'BACK') then
                 udptext='PREV'
c register event
               elseif (text(1:3).eq.'REG') then
c                 udptext='REGISTER '//sfile(1:seiclen(sfile))
                 udptext='REGISTER'
                 regflag=.true.
               endif
               write(*,*) ' sendig udp message '//
     &          udptext(1:seiclen(udptext))
               call send_udp_msg(port,udptext)

c
c check what comes back from se
c
               i=0  ! counter
               udptext=' '
               write(*,*) ' now waiting for return msg from se '
               call clear_display ! clear display
c               text='Loading ...'
c               call xset_color( xblack )  
c               call xchars(text,11,100,300)
               do while (udptext.ne.'QUIT'.and.i.le.200)
                 call get_udp_msg(udptext,msglen)
                 if (msglen.gt.0) then
c                   write(*,*) ' received ',udptext(1:msglen)
c quit from SE
                   if (udptext.eq.'QUIT') then
                     write(*,*) ' received QUIT - program closing '
                     stop
c load new or same sfile from se
                   elseif (udptext(1:4).eq.'LOAD') then
c                     write(*,*) ' debug ',udptext
                     sfile=udptext(6:seiclen(udptext))
                     sfile_flag=.true.
                     write(*,*) ' jump to sfile '//
     &                sfile(1:seiclen(sfile))
c clear display and write text
                     keys=' '
                     one_plot=.false.
                     goto 11
                   endif
                 endif
                 call ms_sleep(50)
                 if (regflag) then
                   i=1   ! keep in endless loop if register
                 else
                   i=i+1
                 endif
               enddo
c
c close down network communication
c
               udptext='EXIT'
               call send_udp_msg(port,udptext)
               call close_network()
             endif                        
             stop   ! stop here and go back to eev
         endif
         one_plot=.true.   ! if passing here, there must come a plot
c
c   read s-file so old  picks and other info can be obtained
c         
         call read_s_file

c
c   find if any waveform files
c
c
c
c         write(6,*)'arc def',arc_by_default
         if(extra_wav.eq.' ') then            ! no special wav file from EEV
      
            call auto_tr(data,nhead,nrecord,n_wav_file,wav_filename)

            if(n_wav_file.eq.0) then   ! if no waveform, stop, if single file
c
c mulplt started from se, send exit and stop
c
               if (fromse.and.port.gt.0) then
                 udptext='SBMESSAGE No waveform data'
                 call send_udp_msg(port,udptext)
                 udptext='EXIT'
                 call send_udp_msg(port,udptext)
                 call close_network()
                 stop
               endif
               write(6,*)' No waveform file name in S-file'
               write(6,*)' Return to continue'
               call flush (6)
               read(5,'(a)') i
               keys(1:4)='    '          ! go  back to same event
               goto 1000
            endif
c
c   select file(s) to use if more than one if all defaults not chosen
c
            if(n_wav_file.gt.1.and.(.not.default)) then
               plotoption=0           ! only display
               call open_display      ! 
               call wav_select(n_wav_file,n_wav_out)                            
               call clear_to_alpha
c
c   stop if no files selected
c
               if(n_wav_out.eq.0) then
                  write(6,*) ' Waveform files not selected'
                  write(6,*) ' Return to stop'
                  read(5,'(a)') i
                  stop
               endif
            else
                n_wav_out=n_wav_file  ! use all if default or one if not def.  
            endif
            default=default_org       ! default could have been disabled, if
                                      ! optin other wav file used
         else            ! for separate wav
            wav_filename(1)(1:40)=extra_wav
c
c  if mt synthetics, make sure sorting is on
c
            if(extra_wav(1:13).eq.'synt_seis.out') chan_sort=.true.
            n_wav_out=1
         endif
      endif
c                                                                               
c-------------------------------------------------------------------            
c   next file from filenr.lis                                          
c-------------------------------------------------------------------            
c                                                                               
c                                                                               
c   get waveform data file name if single files are used, if question is
c   #Next_file#, then the next number is chosen from filenr.lis AND
c   no question is asked. if question is #Back_file#, previous file
c   is used and no question asked.
c                                                                               
      if(opmode.eq.1) then
         call wav_init              ! new file
         call wav_mem_init
         n_wav_out=1                ! only one file at a time  
         sfile='mulplt.out'                            !mar19
         if( b_filenr$      .and.                      !JAB(BGS)Mar95. Next!!.
     &       (question(1:11) .eq. '#Next_file#'.or.
     &        question(1:11) .eq. '#Back_file#' )) then 
             continue                                  !JAB(BGS)Mar95.
         else                                          !JAB(BGS)Mar95.
            write(6,*)' Filename, number, filenr.lis (all)'
            write(6,*)' Continuous SEISAN data base: cont '
            write(6,*)' Large SEED volume: conts'   
            write(6,*)' Archive: arc'

            question= '  Make a choice'                                                                                        
         endif
         call filename(question,wav_filename(1))
c
c  if mt synthetics, make sure sorting is on
c
         if(wav_filename(1)(1:13).eq.'synt_seis.out') chan_sort=.true.
c
c   check if continuous data bases, can also be bud or seiscomp archives
c
         if(wav_filename(1)(1:4).eq.'cont'.or.
     *   wav_filename(1)(1:3).eq.'arc')
     *   then
            wav_file_text='Continuous data base'   ! jh nov 12 08
            cseed=.false.
            if(wav_filename(1)(1:3).eq.'arc') then
               arc=.true.
               wav_file_text='Archive'   
            endif

            cwav=.true.
            if(wav_filename(1)(5:5).eq.'s') cseed=.true. ! large seed volume
            if(.not.cseed) then
                write(6,*) 'Give start time, yyyymmddhhmmss'
                read(5,'(a)') cwav_start_time
c
c  make sure month and day not zero
c
                if(cwav_start_time(5:6).eq.'  ') 
     *          cwav_start_time(5:6)='01'
                if(cwav_start_time(7:8).eq.'  ') 
     *          cwav_start_time(7:8)='01'
c               write(6,*) cwav_start_time
                write(6,*) 'Interval in min'
                read(5,*) cont_interval
                cont_interval=cont_interval*60.0
c
c select waveform databases, seisan cont
c
                if(.not.arc) then
                   plotoption=0           ! only display
                   call open_display      ! 
                   call cbase_select
     *             (cont_base,cont_base_def,n_cont_base,i)
                   n_cont_base=i
                   n_cont_base_sel=i
c
c save selection, this is overwritten by calls to get_seisan_def
c
                   if (n_cont_base_sel.eq.0) then
                      write(*,*) ' no database selected '
                      stop
                    endif
                    call clear_to_alpha
                endif
             endif
c
c-------------------------------------------------------------
c  large seed
c--------------------------------------------------------------
c
             if(cseed) then
                wav_file_text='Large seed file'  
               
                cwav=.false.
 500            continue
                question=' Seed file name or number'                              
                call filename(question,wav_filename(1))

	        inquire(file=wav_filename(1),exist=exist)
	        if(.not.exist) then
	           write(6,*) ' No such file, try again'
	           goto 500
	        endif 
ccc                call wav_init
                wav_nfiles=1              ! jh jun 2011
                wav_nchan=0
                call read_wav_header(1)
                write(6,*)
                write(6,*) ' Available data in SEED file'
                write(6,'(a,a)')
     *          ' STAT  COMP LO YEAR MO DA HR MN    SEC',
     *          ' DUR: DA HR MN  SEC'
c
c   same number of channels in seed file, needed later when finding actual data
c   since not all channels might be available in same time interval
c
                wav_nchan_cseed=wav_nchan
c
                do i=1,wav_nchan
                   min=wav_duration(i)/60.0
                   sec=wav_duration(i)-min*60.0
                   hour=min/60.0
                   min=min-hour*60.0
                   day=hour/24.0
                   hour=hour-day*24.0
                   write(6,'(1x,a5,1x,a4,1x,a,1x,i4,4i3,f7.3,
     *             5x,i3,1x,i2,1x,i2,f5.1)') 
     *             wav_stat(i),wav_comp(i),wav_location(i),
     *             wav_year(i),
     *             wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *             wav_sec(i),day,hour,min,sec
                enddo
                WRITE(6,*)
                write(6,*) 'Give start time, yyyymmddhhmmss,',
     *          ' enter for start time in first channel'
                read(5,'(a)') cwav_start_time
                IF(cwav_start_time.eq.' ') then
                  write(cwav_start_time,'(i4,5i2)') 
     *            wav_year(1),
     *            wav_month(1),wav_day(1),wav_hour(1),wav_min(1),
     *            int(wav_sec(1)+0.1) ! make sure start in first block
                endif
                write(6,*) 'Interval in min'
                read(5,*) cont_interval
                cont_interval=cont_interval*60.0
            endif
            call cwav_time_limits(0)  ! set times
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c

           if(arc) then
             default=.true.   ! lo March 2016
c
c   display all channels and select so not to read headers of all,
c   first deselect channels not active at this time
c
             read(cwav_start_time,'(i4,5i2)') year,month,day,hour,min,
     *       k

             j=0
             do i=1,arc_nchan ! components in seisan way
                sec=k
                call TIMSEC (YEAR,month,DAY,hour,1,0.0,arc_time)
c
c   check if archive channel active for this time, if not
c   do not selct for display
c
                if(arc_time.lt.arc_start(i).or.arc_time.gt.arc_stop(i)) 
     *          goto 2221
                j=j+1       ! count channels with data in time interval
c
c  save possible channels in start of array
c
                arc_stat(j)=arc_stat(i)
                wav_comp(j)=arc_comp(i)(1:2)//' '//arc_comp(i)(3:3)
                arc_comp(j)=arc_comp(i)
                arc_net(j)=arc_net(i)
                arc_loc(j)=arc_loc(i)
                arc_def(j)=arc_def(i)
                arc_start(j)=arc_start(i)
                arc_stop(j)=arc_stop(i)
 2221           continue                
             enddo

c
c set cont_base to station names and use that for selection
c
             k=0    ! number of stations
             do l=1,j
               x=0
               do m=1,k
                 if (arc_stat(l).eq.cont_base(k)) then
                   x=k
                 endif
               enddo
c new station
               if (x.eq.0) then
                 k=k+1
                 cont_base(k)=arc_stat(l)
                 cont_base_def(k)=arc_def(l)
               endif
             enddo  
c
c add virtual networks
c            
             do n=1,arc_vnet
               cont_base(k+n)=arc_vnet_name(n)
             enddo

             n_cont_base=k+arc_vnet
             call cbase_select
     *         (cont_base,cont_base_def,n_cont_base,m)

c             call channelselect(j,arc_stat,wav_comp,                 
c     *           k,nchan_def,stat_def,comp_def,
c     *           .false.,2)

c             arc_nchan=k ! number of channels selected
c             arc_nchan_temp=k

             if(m.eq.0)  then
               write(6,*) 'No channels selected'
               stop
             endif

c
c  make copy of channels before selection
c

             do i=1,j
               arc_stat_temp(i)=arc_stat(i)
               arc_comp_temp(i)=arc_comp(i)
               arc_net_temp(i)= arc_net(i)
               arc_loc_temp(i)= arc_loc(i)
               arc_start_temp(i)=arc_start(i)
               arc_stop_temp(i)=arc_stop(i)
             enddo
             arc_nchan=0
c
c keep channels selected by station name stored in cont_base
c
             do i=1,j    ! all arc c channels
               arc_flag_temp(i)=.false.
               x=0
               do l=1,m  ! m has number of selections
                 if (arc_stat(i).eq.cont_base(l)) then
                   x=l
c assume that virtual networks at the end of array
c                 elseif (x.eq.0.and.cont_base(l)(1:1).eq.'_') then
c                   x=-l 
                 endif
               enddo
               if (x.gt.0.and..not.arc_flag_temp(i)) then
                 arc_nchan=arc_nchan+1
                 arc_flag_temp(i)=.true.
                 arc_stat(arc_nchan)=arc_stat_temp(i)
                 arc_comp(arc_nchan)=arc_comp_temp(i)
                 arc_net(arc_nchan)= arc_net_temp(i)
                 arc_loc(arc_nchan)= arc_loc_temp(i)
                 arc_start(arc_nchan)=arc_start_temp(i)
                 arc_stop(arc_nchan)=arc_stop_temp(i)
c virtual network
               elseif (x.eq.0) then
c                 x=-x
c go through all virtual networks
                 do l=1,arc_vnet
                   x=0
                   do n=1,m
                     if (cont_base(n).eq.arc_vnet_name(l)) x=n
                   enddo
                   if (x.ne.0.and..not.arc_flag_temp(i)) then
c and all channels
                     do n=1,arc_vnet_nchan(l)
                       if (arc_stat_temp(i).eq.arc_vnet_stat(l,n).and.
     &                arc_comp_temp(i).eq.arc_vnet_comp(l,n).and.
     &                arc_net_temp(i).eq.arc_vnet_net(l,n).and.
     &                arc_loc_temp(i).eq.arc_vnet_loc(l,n).and.
     &                .not.arc_flag_temp(i)) then

                       arc_nchan=arc_nchan+1
                       arc_flag_temp(i)=.true.
                       arc_stat(arc_nchan)=arc_stat_temp(i)
                       arc_comp(arc_nchan)=arc_comp_temp(i)
                       arc_net(arc_nchan)= arc_net_temp(i)
                       arc_loc(arc_nchan)= arc_loc_temp(i)
                       arc_start(arc_nchan)=arc_start_temp(i)
                       arc_stop(arc_nchan)=arc_stop_temp(i)
                       endif
                     enddo
                   endif
                 enddo
               endif
             enddo
            endif
            goto 1050    ! jump down to more cont stuff
         endif
c
c   this was end of cont, conts or archive
c
         b_filenr = wav_filename(1)(1:10).eq.'filenr.lis' .or.   
     *              wav_filename(1)(1:10).eq.'FILENR.LIS'        
c
c   clear question
c
         question(1:11)='           '                                          
c-- no more files                    
         if(wav_filename(1)(1:3).eq.'EOF') goto 999          
c
         if( b_filenr ) then                           !JAB(BGS)Mar95.
            opmode=2                                                            
            display_type=0    ! make sure nothing is sent to x
c
            call sei open( old$,              ! Open old file.
     &                     ' ',               ! No prompt.
     &      'filenr.lis',      ! This filename.
     &                     read2,             ! On this unit.
     &                     b_flag,            ! Existance?.
     &                     code )             ! Returned consition (n/a).
c                                                                               
c   get scaling parameters                                                      
c                                                                               
            write(6,*)' Resolution in cm/sec, ',                                
     *              '0: plot all on one page (default)'                         
            call flush (6)
            read(5,'(a)') text                                                  
            if(text(1:5).eq.'     ') then                                       
               time_scale=0.0                                                   
            else
            time_scale = sei real num( text, code ) ! Get time.
            code = e_ok$                            ! Ignore conversion errors.
            endif                                                               
            write(6,*)
     *      ' Low and high cut for filter, return for no filter'
             call flush (6)
             read(5,'(a)') text
             if(text(1:4).eq.'    ') then
               filt=0
             else
c-- use index 9 for variable filter
                call sei get values( 2, text, code )   ! Extract 2 values.
                code = e_ok$                           ! re-store.
                flow(9) = array$(1)                    ! Retrieve.
                fhigh(9)= array$(2)                    ! Ditto.
                filt=9
             endif
         endif                                                                  
      endif
c
c----------------------------------------------------------------------                                                                               
c   many files to plot sequentially, names in filenr.lis                                     
c-----------------------------------------------------------------------
c
      if(opmode.eq.2) then                                                      
         call wav_init
         n_wav_out=1
         read(read2,'(2x,i3,2x,a)',iostat=code) filenumber,
     *   wav_filename(1)
         call sei code( fort$, code, read2, b_flag )             ! Process e.
         if( b_flag ) goto 999                                   ! end of file.
c-- no more files                        
         if(wav_filename(1)(1:5).eq.'     ') goto 999	
      endif                                                                     
c
c-------------------------------------------------------------------------------
c   check if file(s) exist, read waveform file(s) headers
c-------------------------------------------------------------------------------
c

c
c   file name text
c
       wav_file_text=' '
       if(n_wav_out.gt.1) then
          wav_file_text='Many waveform files'
       else
          wav_file_text=wav_filename(1)(1:seiclen(wav_filename(1)))
       endif
c
c   check if files exist and where, also check if data corresponding
c   to archive reference is there
c
       k=0
       do i=1,n_wav_out
         call get_full_wav_name(wav_filename(i),file_out)
         if(file_out.eq.' ') then
              write(6,'(1x,a,a)') ' No such file: ',
     *        wav_filename(i)(1:seiclen(wav_filename(i)))
         else
            k=k+1
            wav_filename(k)=file_out    ! only change name if not archive
         endif
       enddo                       
c
c   action if no files available, assume that if opmode 2, file must exist
c
       if(k.eq.0) then            !
         if (fromse.and.port.gt.0) then
           udptext='SBMESSAGE No waveform data'
           call send_udp_msg(port,udptext)
           udptext='EXIT'
           call send_udp_msg(port,udptext)
           call close_network()
           stop
         endif
         write(6,*)' No waveform files found, return to continue'
         read(5,'(a)') i
         keys(1:4)='    '    
         goto 1000
       endif                                
c                                                                               
c   read all header information from all files selected or available           
c                              
       wav_nfiles=k
       do i=1,wav_nfiles
          call read_wav_header(i)
          if(wav_nchan.gt.max_trace) then
              write(6,*)' Too many channels'
              write(6,*)' Return to stop'
              read(5,'(a)') k
              stop
          endif   
       enddo
       write(6,'(a,a)')' Read headers from files:'
       do i=1,wav_nfiles
          write(6,'(1x,a)') wav_filename(i)(1:80)
       enddo                          

c                                                                               
c  set default start time and window                                            
c                                                                               
       start_time=0.0       
       total_time=wav_total_time                                                    
       window=wav_total_time                                                        
       stop_time=start_time+window                                              
c                                                                               
c--------------------------------------------------------------------------     
c   if in multi event only hard copy plotting mode, do it here                       
c--------------------------------------------------------------------------     
c                                                                               
       if(opmode.eq.2) then                                                     
c-- only hard copy plot                                   
          p_option=3				
c-- resolution                                           
          resolu=resolu_hc
c-- use all channels                         
          nchan=wav_nchan 		
          do i=1,wav_nchan                                             
             channelname(i)=i                                                   
          enddo                                                                 
c                                                                               
c   plot header for traceplot                                                   
c                                                                               
          do i=1,160
            xhead(i:i)=' '
          enddo
          xhead(1:28)=wav_header_text(1)(1:28)
          xhead(30:30)=' '
          xhead(31:)  =wav_file_text(1:seiclen(wav_file_text))
c
c   check for null chars
c
          do i=1,160
            if(ichar(xhead(i:i)).eq.0) xhead(i:i)=' '
          enddo
c                                                                               
c   open plot file                                                              
c                                                                               
          call sei open( unknown$,      ! Open file.
     &                   ' ',           ! No prompt.
     &                   'mulplt.eps',  ! This file.
     &                   plotunit,      ! On this unit.
     &                   b_flag,        ! Existance.
     &                   code )         ! Condition not applicable.
c--                             
          plotoption=p_option-1			
c
c  sort after distance or time
c
        call channel_order(wav_stat,wav_delay,nchan,nsynt)
c                                                                               
c   plot                                                                        
c                                                                               
         call open_display      ! display here is only postscript file

         call traceplot
     *   (nchan,time_scale,start_time,stop_time,wav_delay,xhead)
c
c   close plot in post script file
c
         if(hctype.eq.1)  call close_post                                
c                                                                               
c   send hard copy plot to laser                                             
c                                                                               
         call sei close( close$, plotunit, code ) ! Close (stop on error).
         call send_plot("mulplt.eps",10)
         write(6,*)' ...Plotfile sent'
c
c   go back for next event                                                      
c                                                                               
         goto 1000                                                              
      endif                                                                     
c                                                                               
c-------end of multiplot hard copy only--------------
c
c
c-----------------------------------------------------------------
c   cont multiple data bases, large seed or archive
c-----------------------------------------------------------------
c

 1050 continue   ! enter here if cont, conts or archive mode

      if(cwav.or.cseed) then
c
c   large seed
c         
         if(cseed) then
            wav_nchan=wav_nchan_cseed          ! make sure all channels are searched for
            call wav_seed_read_header_interval ! find position in file of window
         endif
c
c   cont seisan
c
         if(cwav.and..not.arc) then
            call cwav_read_bases   ! read all base info in interval
         endif
c
c  archive
c
         if(arc) then

            call wav_read_arc_headers
c
c   stop if no data
c
            if(wav_nchan.eq.0) then
               write(6,*) ' No archive data, will stop'
               stop
            else
               write(6,*) ' Number of archive channels with data:',
     *         wav_nchan
            endif
         endif
c
c   stop if no channels
c
         if (wav_nchan.eq.0) then
            write(6,*) ' No channels found, MULPLT stops'
            stop
         endif


c                                                                              
c  set default start time and window                                            
c                                                                               
         start_time=0.0       
cx         total_time_window=cwav_abs_end_time-cwav_abs_start_time
         window=wav_total_time
cx         window=total_time_window
         stop_time=start_time+window
      endif

c--------------------------------------------------------------------------
c
c  if this is a run without data base, put in time in header from
c  main header currently used, no check on change of day !!!!!!!!
c
      if(opmode.eq.1) then
         do i=1,80
            data(1)(i:i)=' '
            data(2)(i:i)=' '
            data(3)(i:i)=' '
            data(4)(i:i)=' '
         enddo
         write(data(1)(2:5),'(i4)') wav_year(wav_first)
         write(data(1)(7:20),'(2i2,1x,2i2,f5.1)') wav_month(wav_first),
     *   wav_day(wav_first),wav_hour(wav_first),wav_min(wav_first),
     *   wav_sec(wav_first)
         data(1)(22:22)='L'
         data(1)(80:80)='1'
c
c   put in waveform file name, if cont base, not correct
c
         data(2)(2:seiclen(wav_filename(1))+1)=
     *   wav_filename(1)(1:seiclen(wav_filename(1)))
         data(2)(80:80)='6'
         data(3)(1:57)=
     *   ' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO '           
c         data(3)(58:80)='SNR AR TRES W  DIS CAZ7'
         data(3)(58:80)='AIN AR TRES W  DIS CAZ7'
         nhead=3
         nrecord=4
c
c   write s-file
c
         call write_s_file
      endif

c          
c----------------------------------------------------------------------         
ccccc     General options entered here         
c----------------------------------------------------------------------
c

c
c   set the default plotting option
c
      p_option=1          
c
c   check if in default multichannel mode
c
         
      if(.not.default) then   ! get input
c
c   get plotting output device and type of plot, make sure 
c   not in graphics mode                                 
c                         
         call clear_to_alpha
c                                                      
2222     write(6,*)'                                                   '          
         write(6,*)' Plot options: Interactive picking          Return '         
         write(6,*)'               Multi trace plot on screen, def (0) '  
         write(6,*)'               Multi trace plot on screen      (1) '           
         write(6,*)'               Multi trace plot on screen+laser(2) '           
         write(6,*)'               Multi trace plot on laser       (3) '
         write(6,*)'               Continuoues on screen           (4) '
         write(6,*)'               Continuoues on screen + laser   (5) '
         write(6,*)'               Continuoues on laser            (6) '
         write(6,*)                                                          
     *             '               Stop                            (q) '
         call flush (6)
         read(5,'(a)') dummy                                                       
         if(dummy(1:1).eq.'q'.or.dummy(1:1).eq.'Q') goto 999
         if(dummy(1:1).ne.' ') then
            ix = sei integer( dummy, code )        
            if( code .ne. e_ok$ ) then             
               write(*,*)'****       ... try again...'
               goto 2222                           
            else if(ix.lt.0.or.ix.gt.6) then
               write(*,*)                             
               write(*,*)                             
     &         '** WARN: option is out of range ...try again      **'
               goto 2222                              
            end if                                 
 2639       continue
            write(6,*)                                                          
     *      ' Low and high cut for filter, return for no filter'                  
            call flush (6)
            read(5,'(a)') text                                                  
            if(text(1:4).eq.'    ') then                                        
               filt=0                                                           
            else                                                                
c-- use index 9 for variable filter   
               call sei get values( 2, text, code )   ! Extract 2 values.
               code = e_ok$                           ! re-store.
               flow(9) = array$(1)                    ! Retrieve.
               fhigh(9)= array$(2)                    ! Ditto.
               filt=9
               flow_def=flow(9)
               fhigh_def=fhigh(9)                                                  
c
c   check filter lower bound in cont mode
c
               if(ix.gt.3.and.flow(9).lt.0.1.and.flow(9).gt.0.0) 
     *         then
                  write(6,*)
     *            ' In this mode, lowest filter can only be 0.1'
                  write(6,*)
     *           ' Use low pass instead by gving 0 as low cut'
                  goto 2639
               endif
       
            endif ! end of dummy > 0               
            filt_def=filt                                               
         endif
c
c   check if all defaults
c
         default=.false.
         if(dummy(1:1).eq.'0') then
           default=.true.
           default_org=default
           dummy(1:1)='1'
         endif

         read(dummy,'(i1)') p_option                                               
c-- default output on screen                                
         plotoption=0
c--------------------------------------------------------------------------------------
      endif         ! end of block where input is asked
c
c------------------------------------------------------------------------------------
c
c check if default is for single channel plot
c

      if(default.and.def_single) then
         p_option=0
         plotoption=0
      endif
c
c   if from se, set default filter
c
      if(fromse)then
         filt=9
         filt_def=9
      endif
c
c   put in default filter
c

      if(default) then
         flow(9)=flow_def
         fhigh(9)=fhigh_def
         filt=filt_def
      endif
      if(p_option.gt.1) then                              
c-- for graphics device                              
          plotoption=p_option-1			
      endif
c
c----------------------------------------------------------------------
c CONTINOUES PLOTTING, ONE CHANNEL
c----------------------------------------------------------------------
c
      if(p_option.ge.4.and.p_option.le.6) then
        plotoption=p_option-4
         write(6,*)' Seconds pr line'
         call flush (6)
         read(5,*) page_time
         if(opmode.eq.0) then
            write(6,*)' End time (ymd..), return is to end of month'
            call flush (6)
            read(5,'(a)') eev_end_time
         endif
         write(6,*)' Max count'
         call flush (6)
         read(5,*) max_count
         write(6,*)' Number of lines pr page'
         call flush (6)
         read(5,*) k
         if(k.lt.1) k=1
c         write(6,*)' Channel number'
c         call flush (6)
c         read(5,*) current_chan
          write(6,*) ' Station code, 5 chars max '
          call flush (6)
          read(5,'(a5)') station_code
          write(6,*) ' Component code, 4 chars max '
          call flush (6)
          read(5,'(a4)') component_code
          
c
c  open plot file if needed
c
         if(plotoption.ge.1) then                              
             call sei open( unknown$,      ! Open file.
     &                      ' ',           ! No prompt.
     &                      'mulplt.eps',  ! This file.
     &                      plotunit,      ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
         endif                                                                 
c 
c open display, could be just postscript file
c

         call open_display
c
c   plot continoues, put in original name without path in order to
c   be able to find event in filenr.lis, could be read form TMP if
c   decompressed
c
c         call plot_cont(k,wav_file_text)
         call plot_cont(k,wav_file_text,station_code,
     &    component_code)
c
         call clear_to_alpha                                                 
         stop
      endif
                                                                              
c                                                                               
c----------------------------------------------------------------------         
c MULTI CHANNEL OPTIONS                                                          
c----------------------------------------------------------------------         
c                                                                                
      if(p_option.gt.0) then                                                    
c
c   check if plotting parameter should be entered
c
        if(.not.default.and.p_option.eq.3) then
c                                                                               
c   get scaling parameters                                                      
c                                                                               
            write(6,*)' Resolution in cm/sec, ',                                
     *                 '0: plot all on one page (default)'                      
            call flush (6)
            read(5,'(a)') text                                                  
            if(text(1:5).eq.'     ') then                                       
               time_scale=0.0                                                   
            else                                                                
               time_scale = sei realnum(text,code) ! Extract real number.
               code       = e_ok$                  ! Restore.
            endif                                                               
c                                                                            
c   get window                                                                  
c                                                                               
            window=stop_time-start_time                                         
            write(6,277) start_time,window                                      
 277        format(1x,'Selected start time and window is:',2f8.1,              
     *      ' Ok, else enter new value?')                                    
            call flush (6)
            read(5,'(a)' )text                                                 
            if(text(1:4).ne.'    ') then                                  
               call sei get values( 2, text, code ) ! Extract 2 variables.
               code = e_ok$                         ! Restore.
               start_time = array$(1)               ! Retrieve.
               window     = array$(2)               !
               if(start_time.eq.0.0.and.window.eq.0.0) window=total_time        
               if(window.eq.0.and.start_time.ne.0.0)                            
     *            window=total_time-start_time                                  
               stop_time=start_time+window                                      
            endif                                                              
c
c   now enter defaults
c
        else
           time_scale=0.0 
           window=stop_time-start_time
        endif
c                                                                               
c   show (if not default) which channels selected and possibly select others                     
c
        call channelselect(wav_nchan,wav_stat,wav_comp,                       
     *  nchan,nchan_def,stat_def,comp_def,default,1)
c
c   save all selected channel numbers, could be more than used for one screen
c
c        write(27,*)' save org'
        do i=1,nchan
           channelname_org(i)=channelname(i)   ! the new total selection
c           channelname_win(i)=channelname(i)   ! the origial total selection used for the first windows
        enddo
c
c   save original number of channels, used if several screens are plotted
c
        nchan_org=nchan
c
c   set screen number
c
        n_screen=1
c
c   clear to alpha if no screen plot
c
        if(p_option.eq.3) call clear_to_alpha                                                 
c
c   stop if zero number of channels, can be caused by a q in selection
c
        if(nchan.eq.0) goto 999
c
c   reset default, could have been temporaryly disabled
c

        default=default_org
c
c---------------------------------------------------------------------------
c---------------------------------------------------------------------------
c   Main loop start point in multitrace mode, come back here after most
c   choises
c---------------------------------------------------------------------------
c---------------------------------------------------------------------------
c
 1099   continue
c
c   select channels for screen number n_screen
c   channelname_org: channels selected after initial channel selection screen
c                    or after selection when all windows has been plotted
c   channelname_win: the original selection for each window
c
c
c  when starting a new set of windows, save the channel selection available
c  then. this selection might be different from the very first selection due
c  to selection in the multi trace window so channelname_org might have been
c  changed in picsub
c
        if(n_screen.eq.1) then     ! new set of windows
           do i=1,nchan_org
              channelname_win(i)=channelname_org(i)
           enddo
           do i=nchan_org+1,max_trace
              channelname_win(i)=0
           enddo
c
c   for first screen, calculate total number of windows to show 
c
        total_n_screen=(nchan_org-1)/n_chan_screen +1
c        write(27,*) total_n_screen
c
c        write(6,*)'n_chan_screen',n_chan_screen
       endif
c
c   now select channels for window
c
        l=1
        nchan=0
c        write(27,*) 'nscreen before',n_screen
        k=(n_screen-1)*n_chan_screen+1
        do i=k,k+n_chan_screen-1
c           write(27,*) 'i,chan', i, channelname_org(i)
           if(channelname_win(i).gt.0) then
               channelname(l)=channelname_win(i)
               nchan=nchan+1
               l=l+1
           else  ! no more channels to plot, return to first screen next time
c               n_screen=0
           endif
        enddo
c        write(27,*) 'nchna,nscreen',nchan,n_screen

        if(nchan.eq.0) then       ! could be that last screen has zero channels, 
c                                   then go to first
          n_screen=1
          goto 1099
        endif
        nchan_initial=nchan_org
c
c------------------------------------------------------------------
c   enter here at 1100 for replot, multi trace mode and all component mode
c------------------------------------------------------------------
c


 1100   continue
 

        time_scale=0.  ! lo 03/10/2003
c
c------------------------------------------------------------------------------
c   if all component plot, select channels
c------------------------------------------------------------------------------
c

c
c  following section is for going forth and back for many stations in all
c  component mode
c

       if(allcomp.and.(choice.eq.'FINI'.or.choice.eq.'BACK')) then
c
c    go back one station
c
           if(choice.eq.'BACK') then
               nchan_all_current=nchan_all_current-nchan_all_station-1  ! end of last station section

c
c   go back to first channel of previous station channel section
c
               stat_current=wav_stat
     *         (channelname_allc(nchan_all_current))
               nchan_all_current=nchan_all_current-1

 1103          continue
               if(stat_current.eq.
     *         wav_stat(channelname_allc(nchan_all_current))) then
                  nchan_all_current=nchan_all_current-1

                  goto 1103
               endif
               nchan_all_current=nchan_all_current+1
c
               if(nchan_all_current.lt.1) nchan_all_current=1
           endif

c
c   what to do whene reaching last channel
c
c   start again with first channel
c
c           if(nchan_all_current-1.eq.nchan_all) nchan_all_current=1   ! start again
c
c   go back to multi mode
c
           if(nchan_all_current-1.eq.nchan_all) then
              choice='ALLC'  ! go out of allcomp mode
              goto 1106
           endif

           nchan_all_station=1        ! number of channels for one station, start counting
c
c   find channels for next station, or previous, nchan_all_curent points to it, 
c   there is always one
c
           stat_current=wav_stat(channelname_allc(nchan_all_current))
           channelname(nchan_all_station)=
     *     channelname_allc(nchan_all_current) 
c
c   check if next channel is from the same station
c          
 1105      continue                                           ! loop from just below
           nchan_all_current=nchan_all_current+1              ! look at next channel
               
c lo: problem here when switching to 3 comp (y)  if only one channel in multi trace mode
c jh: this is caused by array channelname_allc not taking into account
c     that there is more channels to check. so fixed by checking that. 

           if(channelname_allc(nchan_all_current).gt.0) then
              if(stat_current.eq.
     *        wav_stat(channelname_allc(nchan_all_current))) then
                 nchan_all_station=nchan_all_station+1           ! one more channel
                 channelname(nchan_all_station)=
     *           channelname_allc(nchan_all_current)
                 goto 1105                                       ! check for next
              endif
           endif
c
c   now no more for that station so plot
c
           nchan=nchan_all_station 
           if (fromse.and.port.gt.0.and.nchan.eq.0) then ! exit if fromi
                                                         ! se and no channels
             udptext='SBMESSAGE No waveform data'
             call send_udp_msg(port,udptext)
             udptext='EXIT'
             call send_udp_msg(port,udptext)
             call close_network()
             stop
           endif
c
c   save all channels for particular station, used when going to single trace
c   mode from one station in all trace mode

           do i=1,nchan
              channelname_all_old(i)=channelname(i)
           enddo

        endif   ! end of forth and back of all comp station display
c
c   if all component mode, open a new smaller window, else multi trace window
c
        if(allcomp) then
           k=wsize*0.8
           call xwindow_size(k,60,70)
           call xopen_window(2)
           text=' '
           text(1:25)='MULPLT all component mode'
           call xwindow_title(text)
        endif            
c
c  check if change of window in cont, arch or cseed plotting
c
        if(cont_window.and.(cwav.or.cseed)) then
           cont_window=.false.
           call cwav_time_limits(1)             ! calculate new lim         
	     if(cseed) then
              wav_nchan=wav_nchan_cseed          ! make sure all channels are searched for
              call wav_seed_read_header_interval ! find position in file of window
           endif

           if(cwav) then
              call cwav_read_bases   ! read all base info in interval
           endif

           if(arc) then
              call  wav_read_arc_headers
           endif

           call wav_index_total  ! testing this


c                                                                              
c  set default start time and window                                            
c                                                                               
           start_time=0.0       
           window=wav_total_time
           stop_time=start_time+window
        endif
c        write(6,*)' wav_total_time',  wav_total_time
c 
c                                                                               
c   plot header for traceplot                                                   
c                                                                               
        do i=1,160
           xhead(i:i)=' '
        enddo
        i=index(filen,' ')                                               
        xhead(1:28)=wav_header_text(1)(1:28)
        xhead(30:30)=' '
        xhead(31:)  = wav_file_text            
c
c   check for null chars
c
        do i=1,160
           if(ichar(xhead(i:i)).eq.0) xhead(i:i)=' '
        enddo
c                                                                               
c   Get old picks                                                               
c                                                                               
        call read_s_file
c
c   add synthetic readings if any
c
        call add_synt_readings(nsynt,synt_head)
c                                                                               
c  plot, set resolution to 1000 if on screen, else 3000 (defaults)
c                                                                               
        resolu=resolu_x
        if(p_option.gt.1) resolu=resolu_hc
c
c  sort after distance or time
c
        call channel_order(wav_stat,wav_delay,nchan,nsynt)                               
c
c   check if baz angles should be calculated
c
        if(rotate.and.no_baz) then
            call get_baz
     *      (wav_nchan,wav_stat,data,nhead,nrecord,baz)
            no_baz=.false.      ! indicate that baz has been calculated
        endif
c
c   close plot file if open to make sure that another plot is not put on top
c   this means that if a replot is made, only the last plot can be sent for
c   hardcopy
c
        if(plotoption.ge.1.and.plotunit.gt.10)  then 
           call sei close( close$, plotunit, code ) ! Close (stop on error).
        endif
c
c
c  open plot file if needed
c
        if(plotoption.ge.1) then                              
             call sei open( unknown$,      ! Open file.
     &                      ' ',           ! No prompt.
     &                      'mulplt.eps',  ! This file.
     &                      plotunit,      ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
        endif                                                                 
c 
c open display, could be just postscript file
c
        call open_display
c        text='Loading ...'
c        call xset_color( xblack )  
c        call xchars(text,11,200,500)

c
c   clear menu choises
c
         do i=1,13
           onbox(i)=0
         enddo
         do i=15,42
           onbox(i)=0
         enddo
c
c   multi trace plot
c
        call traceplot                                                         
     *  (nchan,time_scale,start_time,stop_time,wav_delay,xhead)
c
c   add text of current mode
c
        text=' '
        if(allcomp.and.all_from_single) 
     *    text(1:34)='3 comp from single, back with y   '
        if(allcomp.and..not.all_from_single) 
     *    text(1:34)='3 comp from multi, back with y    '
        if(allcomp.and.allcomp_many)
     *    text(1:34)='3 comp all from multi, back with y'
        call xset_color( xred )  
        call tchars(text,34,xpos+400.0,max_ypos+4.0)
        call xset_color( xblack )  

c
c--------------------------------------------------------------------------
c particle motion plot
c--------------------------------------------------------------------------
c
        if(do_pmp) then
          call plot_pm(start_time,stop_time)
        endif

c
c   reset temporary settings due to hc plot and submit plot
c
        if(choice.eq.'HCAL') then
           do l=1,old_nchan
              channelname(l)=old_chan(l)
           enddo
           call close_post                               
           call sei close( close$, plotunit, code ) ! Close (stop on error).
           nchan=old_nchan
           plotoption=old_plotoption
           call send_plot("mulplt.eps",10)
           resolu=resolu_x
        endif
c
c   count number of calls to pic routine
c
        npick=0
c                                                                               
c   activate channel select if not only hardcopy is made, also enter here
c   for more choices
c   if only hard copy done, choice is assumed to be the next event
c
        if(p_option.eq.3) choice='FINI'
c
c--------------------------------------------------------------------------
c   block where interactive graphics is used
c--------------------------------------------------------------------------
c
        if(p_option.le.2) then     ! remain in graphics mode                                                  
c
c   save old channel selection if only one selected, might want to
c   got back to old selection, used e.g. for one channel to single trace
c   also used when resetting for temporary hc plot
c
           if(.not.allcomp) then ! when in all component mode, not org. chan
              do i=1,nchan
                 old_chan(i)=channelname(i)
              enddo
              old_nchan=nchan
           endif
c
c---------------------------------------------------------------------------
c   get up cursor for choises
c---------------------------------------------------------------------------
c
 1110      continue
           call chasel(nchan,wt1,wt2,start_time,stop_time)
c
c   skip invalid choices in connection with all channel mode
c

c   if all comp mode from single, only allow to go back to single
c   using ALLC or FINI (next channel)
c

           if(choice.eq.'BACK'.and.
     *     all_from_single) goto 1110


           if(choice.eq.'TOGL'.and.all_from_single) goto 1110
           
c
c--------------------------------------------------------------
c   Multiple window handling
c--------------------------------------------------------------
c

           if(choice.eq.'NEXW') then
c
c   only go to next screen if all channels of current screen are selected
c
c               if(nchan_org.eq.nchan_initial) n_screen=n_screen+1
               n_screen=n_screen+1
               if(n_screen.gt.total_n_screen) n_screen=1
c               write(27,*) 'nscreen new', n_screen
               choice=' '
               goto 1099
           endif

c
c--------------------------------------------------------------------------
c   check if pick, remain in picking loop as long as picks are chosen
c--------------------------------------------------------------------------
c
           do while(choice.eq.'PICP') 
              npick=npick+1
              call add_synt_readings(nsynt,synt_head)
              iswitch = 0  ! convert readings before calling picking routine
              call convert(wav_abs_time(wav_first),
     *        wav_stat(current_chan),                     
     +        wav_comp(current_chan),                            
     +        iswitch)
              chan_delay=start_time 
              first=1
              call tecpic
              chan_delay=0.0  ! make sure no delay if going into s. chan. mode
              iswitch = 1  ! convert readings  back to nordic format
              call convert(wav_abs_time(wav_first),
     *        wav_stat(current_chan),                     
     +        wav_comp(current_chan),                               
     +        iswitch)                                         
c                          
c   save any new picks
c          
              if(npick.gt.0) then
                npick=0
                call write_s_file
              endif
c
c   get cursor up, if a pick, remain in picking loop, else continue. 
c   this is a routine specially for multi trace plot  handeling
c   cursor input
c
              call chasel(nchan,wt1,wt2,start_time,stop_time)       
           enddo
c
c----------------------------------------------------------------------------
c  hard copy of all
c---------------------------------------------------------------------------
c
           if(choice.eq.'HCAL') then
              do i=1,wav_nchan
                 channelname(i)=i
              enddo
              nchan=wav_nchan
c
c   sort again, make sure no menu comes up, use all channels so number
c   of default channels has been set to 0
c
              default_sort=.true.                

              call channelselect(wav_nchan,wav_stat,wav_comp, 
     *        nchan,0,stat_def,comp_def,default_sort,1)

              old_plotoption=plotoption
              plotoption=1                  ! temporarely reset plot option
              resolu=resolu_hc
              goto 1100                     ! hc plot
           endif
c
c-----------------------------------------------------------------------------------------
c     FINI and all component mode, go to next channel if not from single channel
c-----------------------------------------------------------------------------------------
c
       if(choice.eq.'FINI'.and.allcomp.and..not.all_from_single) then
           goto 1100
       endif
c
c------------------------------------------------------------------------
c   check if back one station in all component mode but not if from single channel
c------------------------------------------------------------------------

        if(choice.eq.'BACK'.and.allcomp.and..not.all_from_single) then
          goto 1100
        endif

c
c--------------------------------------------------------------------------
c  all component mode, in or out
c--------------------------------------------------------------------------
c
c  come here from above if back to multi mode form 3 comp when reaching last channel
c
 1106      continue
c

           if(choice.eq.'ALLC'.or.
     *     (choice.eq.'FINI'.and.allcomp.and.all_from_single).or.
     *     choice.eq.'ALCC') then
c
c   if already in all component mode, turn it off and close window
c

              if(allcomp) then
                 allcomp_many=.false.
                 allcomp=.false.
c                 write(6,*) 'xclose'
                 call xclose_window(2)

c
c   go back to old multi channel selection if all component originate in
c   multi channel mode, else go back to channel selection used for
c   single channel mode 
c

                 if(all_from_single) then
                    do l=1,nchan_single
                       channelname(l)=channelname_single(l)
                    enddo
                    nchan=nchan_single
                    i=ichan_single-1     ! start with old channel if togle with y

                    if(choice.eq.'FINI') i=ichan_single ! start with next channesl
                    goto 2000
                 else
                    do l=1,old_nchan
                       channelname(l)=old_chan(l)
                    enddo
                    nchan=old_nchan
                    goto 1100            
                 endif
                 choice=' '              
c
c   start all channel mode
c
              else
                 allcomp=.true.
                 if(choice.eq.'ALCC') allcomp_many=.true.
c
c   select all channels for selected station(s)
c
                 nchan_all = 0                ! count expanded number of channels
c
c  if none selected, select first what should be included
c  here it is selected that all stations in display should be searched
c  for, if only the first (like toggle mode) set nchan_selected=1
c
                 if(nchan_selected.eq.0) nchan_selected=nchan

                   

                 do k=1,nchan_selected
                    ich=channelname(k)      ! currently selected channel, more than one
                    do i=1,wav_nchan        ! check if more channels with same station code
                       if(wav_stat(ich).eq.wav_stat(i)) then
                          nchan_all=nchan_all+1
c
c  save for sorting
c
                          stat_all(nchan_all)=wav_stat(i)
                          comp_all(nchan_all)=wav_comp(i)
                          channelname_all(nchan_all)=i    ! channel number in origial order
                       endif
                    enddo
                enddo
c
c   sort again, make sure no menu comes up, use all channels so number
c   of default channels has been set to 0
c
                 default_sort=.true.                

                 call channelselect(nchan_all,stat_all,comp_all, 
     *           nchan,0,stat_def,comp_def,default_sort,1)
c
c   update channel array used for plotting
c   first get original numbers into channelname_org
c
                  do i=1,nchan_all
                    channelname_org(i)=channelname_all(channelname(i))
                  enddo
c
c  put orignal numbers back to channelname used for plotting
c  at the same time check that a channel is not repeated
c
                  channelname(1)=channelname_org(1)
                  k=1
                  kk=k                   
                  do i=1,nchan_all
                     m=0
                     do j=1,kk  ! check if already included
                       if(channelname_org(i).eq.channelname(j)) m=1
                     enddo
                     if(m.eq.0) then ! found a new one
                        k=k+1
                        channelname(k)=channelname_org(i)
                     endif
                     kk=k
                  enddo
c
c   force using all channesl for one station if choice ='FiNI'
c
                  if(.not.choice.eq.'ALCC') choice='FINI' ! force selection               
                  nchan_all=k                ! save how many channels at end
                  nchan_all_current=1        ! current channel used in all channel mode
c
c   if multi stations all chan, use all channels
c
                  nchan=k         
c
c  save complete selection
c
                  do i=1,nchan
                     channelname_allc(i)=channelname(i)
                  enddo
              endif

              goto 1100    ! multi channel plot
           endif
c
c---------------------------------------------------------------------------
c check if help
c---------------------------------------------------------------------------
c
           if(choice.eq.'HELP') then
              call mulplt_help            ! print help file
              goto 1100                   ! replot		   
           endif
c

c
c---------------------------------------------------------------------------
c check if multitrace spectrogram
c---------------------------------------------------------------------------
c

           if(choice.eq.'SGM') then
              goto 1100                   ! replot		   
           endif
c--------------------------------------------------------------------------
c iasp
c--------------------------------------------------------------------------
c
           if(choice.eq.'IASP') then
             if(iaspon) then
               nsynt=-1
c remove iasp.inp
               call sei open( unknown$,    ! Open file.
     &                      ' ',           ! No prompt.
     &                      'iasp.out',    ! This file.
     &                      any_unit    ,  ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
               call sei close(DELETE$, any_unit, code)
               iaspon=.FALSE.
             else
               iaspon=.TRUE.
               call sei open( unknown$,     ! Open file.
     &                       ' ',           ! No prompt.
     &                       'iasp.inp',    ! This file.
     &                       any_unit    ,  ! On this unit.
     &                       b_flag,        ! Existance.
     &                       code )         ! Condition not applicable.
               do i=1,wav_nchan
                 b_flag=.false.
                 do j=1,i-1
                   if (wav_stat(i).eq.wav_stat(j)) b_flag=.true. 
                 enddo
                 if (.not.b_flag) write(any_unit,'(a5)') wav_stat(i)
               enddo
               call sei close( close$, any_unit, code )
               if (data(1)(22:22).eq.'D') then
                 write(*,*) ' synthetic phases distant '
                 call systemc('iasp',4)        ! calculate synthetic phases
               else
                 write(*,*) ' synthetic phases local '
                 call systemc('ttlocal',7)     ! calculate synthetic phases
               endif
               call get_synt(nsynt,synt_head)   ! read synthetic phases
c
c remove iasp.inp
c
               call sei open( unknown$,    ! Open file.
     &                      ' ',           ! No prompt.
     &                      'iasp.inp',    ! This file.
     &                      any_unit    ,  ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
               call sei close(DELETE$, any_unit, code)
             endif
             goto 1100
           endif
c
c---------------------------------------------------------------------------
c check if write out of waveform file, either isolated or to use with FK
c or to register event
c---------------------------------------------------------------------------
c
           if(choice.eq.'WOUT'.or.choice.eq.'FK  '
     &        .or.choice.eq.'SG') then

              long_text=' '
c
c   open and write in file for choises to wavetool program
c
              call sei open( unknown$,     ! Open file.
     &                      ' ',           ! No prompt.
     &                      'extract.inp', ! This file.
     &                      any_unit    ,  ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
c
              window=stop_time-start_time
c
c   if spectrogram, only one channel, extract 300 s in case it is needed
c
              if(choice.eq.'SG') then
                 write(any_unit,*)'1'
                 write(any_unit,*) current_chan,trace_delay,300.0
                 trace_delay=0.0     ! reset
              else
              write(any_unit,*) nchan
                 do i=1,nchan
c also arc ?
                    if(cwav.or.cseed) then ! cont use first sample and whole window
                      write(any_unit,'(i3,a)') channelname(i),' 0.0 0.0'
                    else
                      write(any_unit,'(i3,1x,2f10.3)')
     *                channelname(i),start_time,window
                    endif
                 enddo
              endif

              call sei close( close$, any_unit, code )
              wave_out=.false.
c
c   filter resp text
c

              text=' '
              if(filt_old.gt.0) then
c
c   first check if possible to filter in wavetool
c
                  if(filt_old_low.eq.0.0.or.filt_old_high.eq.0.or.
     *            recfil_pole.ne.4) then
                      write(6,*)'*************************************'
                      write(6,*)'*************************************'
                      write(6,*)
     *          'Filter must be bp and 4 poles, no filter in write out'
                      write(6,*)'Filter and poles are:',
     *                filt_old_low,filt_old_high,recfil_pole
                      write(6,*)'*************************************'
                      write(6,*)'*************************************'
                 else 
                    write(text,'(a,2f10.4)') 
     *               ' -filter ',filt_old_low,filt_old_high
                 endif
              endif
              if(remove_response_old.eq.1) then
                   write(text(31:43),'(a,i3)') ' -ground ',
     *             disp_vel_old
              endif
c              write(27,*) filt_old,disp_vel_old,
c     *                    remove_response_old
c
c----------------------------------------------------------------------------
c  set up time window to get data in cont base, large seed or arc 
c----------------------------------------------------------------------------
c
              if(cwav.or.cseed) then
	         if(cseed) then
	              window_abs_time=wav_abs_time(wav_first)
     *              +start_time
	         else
                    window_abs_time=cwav_abs_start_time+start_time
	         endif

                 call sectim
     *           (window_abs_time,year,i,month,day,hour,min,sec)
                 i=sec+0.5
                 write(text,'(i4,5i2)')year,month,day,hour,min,i
                 do i=1,14
                    if(text(i:i).eq.' ') text(i:i)='0'
                 enddo
                 write(text1(1:10),'(f10.1)') window
c
c-------------------------------
c   large seed extract
c------------------------------
c
                 if(cseed) then
                    if (choice.eq.'FK  ') then
                      long_text='wavetool -start '//text(1:14)//
     *                ' -cwav -duration '//text1(1:10)
     *                //
     *                ' -chansel extract.inp -wav_out_file waveform.out'
                    else
                      long_text='wavetool -start '//text(1:14)//
     *                ' -cseed -duration '//text1(1:10)
     *                 //' -sfile '//sfile(1:seiclen(sfile))
     *                //' -chansel extract.inp -wav_out_file SEISAN'
     *                //' -format '//mulplt_wav_out_format
                    endif
                 endif     
c
c---------------------------------------------------------------------------
c  if a subset of channels have been selected before data bases are opened 
c  with the standard set of channesl in SEISAN.DEF (cont or arc), this 
c  subset must be written in a file so wavetool uses only the subset and not 
c  the  channels in SEISAN.DEF. the same file is used for cont and arc.
c---------------------------------------------------------------------------    

c
c  for cont
c

                if(cwav.and..not.arc.and.n_cont_base_sel.gt.0) then 
                    call sei open( unknown$,     ! Open file.
     &              ' ',           ! No prompt.
     &              'cbase.inp', ! This file.
     &              any_unit    ,  ! On this unit.
     &              b_flag,        ! Existance.
     &              code )         ! Condition not applicable.
                    do i=1,n_cont_base_sel
                       write(any_unit,'(a)')
     *                 cont_base(i)(1:seiclen(cont_base(i)))//' ' 
                    enddo
                    call sei close( close$, any_unit, code )
                endif
c
c   for arc, always use cbase.inp since channels could have been sorted 
c   so they are not same order as in SEISAN.DEF
c
               if(arc.and.arc_nchan.gt.0) then 
                    call sei open( unknown$,     ! Open file.
     &              ' ',           ! No prompt.
     &              'cbase.inp',   ! This file.
     &              any_unit    ,  ! On this unit.
     &              b_flag,        ! Existance.
     &              code )         ! Condition not applicable.
                    do i=1,arc_nchan
                       write(any_unit,'(a5,a3,a2,a2,1x,2f15.3)')
     *                 arc_stat(i),arc_comp(i),arc_net(i),arc_loc(i),
     *                 arc_start(i),arc_stop(i)
                    enddo
                    call sei close( close$, any_unit, code )
                endif
c
c----------------------
c  seisan cont extract
c----------------------
c
                if(cwav.and..not.arc) then
                   if (choice.eq.'FK  ') then
                      long_text='wavetool -start '//text(1:14)//
     *                ' -cwav -duration '//text1(1:10)
     *                //
     *                ' -chansel extract.inp -wav_out_file waveform.out'
                   else
                      long_text='wavetool -start '//text(1:14)//
     *                ' -cwav -duration '//text1(1:10)
     *                //' -wav_out_file SEISAN'
     *                //' -chansel extract.inp'
     *                //' -format '//mulplt_wav_out_format
                   endif
c
c only selected databases for seisan type cont data if not all data bases used
c
                   if (n_cont_base_sel.gt.0) then
                      long_text=long_text(1:seiclen(long_text))//
     *                ' -cbase cbase.inp '
                   endif
                endif             
c
c----------------------
c  arc extract
c---------------------
c               
                if(arc) then 
                   arc_text=' -arc '   ! set archive 
                   if (choice.eq.'FK  ') then
                      long_text='wavetool -start '//text(1:14)//
     *                arc_text//' -duration '//text1(1:10)
     *                //
     *                ' -chansel extract.inp -wav_out_file waveform.out'
                   else
                      long_text='wavetool -start '//text(1:14)//
     *                arc_text//' -duration '//text1(1:10)
     *                //' -wav_out_file SEISAN'
     *                //' -chansel extract.inp'
     *                //' -format '
     *                //mulplt_wav_out_format(1:
     *                seiclen(mulplt_wav_out_format))
                   endif
c
c always use cbase.inp for arc 
c
                   long_text=long_text(1:seiclen(long_text))//
     *                ' -cbase cbase.inp '
                endif
c
c-----------------------------
c normal file extract
c----------------------------
c
              else     ! a normal file opposed to arc cseed or cont
                 if (choice.eq.'FK  '.or.choice.eq.'SG ') then
                   long_text='wavetool -sfile '//sfile(1:seiclen(sfile))
     *             //' -chansel extract.inp -wav_out_file waveform.out'
                 else
                   long_text='wavetool -sfile '//sfile(1:seiclen(sfile))
     *             //' -chansel extract.inp -wav_out_file SEISAN'
     *             //' -format '//mulplt_wav_out_format
     *             //text(1:42)
                 endif
              endif

              write(6,*) long_text

  
c             write(27,'(a,a,i5)') long_text(1:seiclen(long_text)),'  ',
c     *        seiclen(long_text)

              call systemc(long_text(1:seiclen(long_text)),
     *        seiclen(long_text))

              if(choice.ne.'FK  '.and.
     &           choice.ne.'SG') goto 1100    ! replot
           endif             ! end of waveform extract
c
c   make spectrogram
c
           if(.not.wave_out.and.choice.eq.'SG') then
             i=seiclen(spectrogram_command)
             long_text=' '
             long_text(1:i+15)=spectrogram_command (1:i)
     *       //' waveform.out '
             write(long_text(i+16:i+37),'(3f7.2)')spectrogram_window,
     *       spectrogram_filter,spectrogram_plot_trace
             write(*,*) long_text(1:seiclen(long_text))
             call systemc
     &(long_text(1:seiclen(long_text)),seiclen(long_text))
             goto 1110     ! back to choises
           endif
c
c--------------------------------------------------------------
c    FK analysis if waveform file has been written out
c--------------------------------------------------------------
c

           if(.not.wave_out.and.choice.eq.'FK  ') then
              call systemc('fk',2)
              fk=.false.
c
c     some problem to make sei open work
c
c   open file with results
c
c             call sei open( old$+ignore$,            
c    &               ' ',                     ! No prompt.
c    &               'fk.out',                ! This file.
c    &               readfk,                  ! On unit.
c    &               exist,                   ! File exists?.
c    &               code )                   ! Condition.
c
c    File exists and is open!!..
c
             open(29,file='fk.out',status='old',err=9362)
             goto 9361
 9362        continue
             write(6,*)' No fk.out file'
             goto  9365
 9361        continue
             readfk=29
c            if(exist) then
                do i=1,8
                   read(readfk,'(a)',err=9364) text
c                  write(6,'(a)',err=9364) text
                enddo
c
c   check if results
c
                if(text(2:9).eq.'APPARENT') then
                   read(text(21:25),'(f5.1)',err=9364) veli
                   read(text(45:48),'(i4)',err=9364) k
                   azim=k
                   goto 9365
                else
                   write(6,*)' No FK values were selected'
                   veli=0
                   azim=0
                   goto 9365
                endif
 9364           continue
                write(6,*)' Something wrong with fk.out'
 9365           continue
c               call sei close( close$, readfk, code ) ! Close 
                close(29)
              choice='    '      ! clear FK choice
              goto 1100          ! replot
           endif

c
c----------------------------------------------------------
c check if locate event
c----------------------------------------------------------
c
           if(choice.eq.'LOCA') then
             if(fromse) then    ! locate in se
               udptext='LOCATE'
               call send_udp_msg(port,udptext)
c               write(6,*)'Now sent locate message to port ',port
c
c   wait for locate finished
c
               do i=1,100
                  call ms_sleep(100)  ! wait 100 ms  
                  call get_udp_msg(udptext,msglen)
                  if (msglen.gt.0) goto 5566
               enddo
 5566          continue
c
c   make an iasp.out file with theoretical arrival times
c
              call make_res_file(nrecord,data,nsynt) ! calulate residual
              synt_head=data(1)(1:10)
              call add_synt_readings(nsynt,synt_head)
               goto 1100
             else
               call clear_to_alpha
c
c   if no data base, put name mulplt.out in as an s-file for location
c
              if(opmode.eq.1) call put_env_event('mulplt.out')
              call systemc('hyp',3)  ! locate and put screen output
                                      ! in hyp.tmp
              write(6,*)' Return to continue'
c			  call flush (6)
c			  read(5,'(a)') text      ! pause for input
c
              write(6,*)' U to update       '
              call flush (6)
              read(5,'(a)') text      ! pause for input
c UPDATE
              if(text(1:1).EQ.'u'.OR.text(1:1).EQ.'U')then
c              read and check if event should be updated
               call read_s_file_e(sfile)
               if(data1(1)(45:45).eq.'*') then
                write(6,*)' Event marked for no location'
                write(6,*)' Return to continue'
                call flush (6)
                read(5,'(a)') text      ! pause for input
               else
c               text='hyp '//sfile(1:seiclen(sfile))//
c    *          ' -update -operator '//operator(1:4)
c               call systemc(text,seiclen(text))
                long_text='hyp '//sfile(1:seiclen(sfile))//
     *          ' -update -operator '//operator(1:4)
                call systemc(long_text,seiclen(long_text))
c               Log the update:
                if ( seisan_logging.GE.1 ) 
     +          call message_to_log_file(sfile,operator,1)
c               write(6,*)' Return to continue'
c               call flush (6)
c               read(5,'(a)') text      ! pause for input
               endif
              endif
c
c   make an iasp.out file with theoretical arrival times
c
              call make_res_file(nrecord,data,nsynt) ! calulate residual
              synt_head=data(1)(1:10)
              call add_synt_readings(nsynt,synt_head)
              goto 1100               ! replot   
             endif          
           endif
c
c-------------------------------------------------------------------
c   check if quit
c------------------------------------------------------------------
c
           if(choice.eq.'QUIT') then
              goto 999                                            
           endif
c
c----------------------------------------------------------------------
c   replot traces with new choice of parameters, if any picks were made
c   go to reread picks
c-----------------------------------------------------------------------
c
           if(choice(1:4).eq.'REPL') then
c             if (choice.eq.'REPLSGM') choice='SGM'
             goto 1100
           endif

c
c----------------------------------------------------------------------
c   replot traces with new choice of number of channels per screen
c-----------------------------------------------------------------------
c
           if(choice.eq.'NCSC') then
              goto 1099
           endif
c
c-------------------------------------------------------------------                        
c   check if going into pick mode, toggle mode
c-------------------------------------------------------------------
c
           if(choice.eq.'TOGL') then
             if(plotoption.ge.1)
     *       call sei close( close$, plotunit, code ) ! Close (stop on error).
             i=0   ! start with first channel
c
c   if from all comp mode, close window
c
             if(allcomp) then
                call xclose_window(2)
             endif
             goto 2000      ! this is far below in single trace plotting
           endif
c
c---------------------------------------------------------------------
c   check if merge of multiple files
c-------------------------------------------------------------------
c
           if(choice.eq.'MERG') then
              call wav_select_mer(wav_nfiles,n_wav_mer)
           endif
c
c----------------------------------------------------------------
c   check if multi file delete, then display delete box
c----------------------------------------------------------------
c
           if(choice(1:4).eq.'DEL '.and.n_wav_file.gt.1) then
              call wav_select_del(wav_nfiles,n_wav_del)            
c
c   replot traces, if no files deleted or at least 2 selected for merge.
c   go to reread picks
c
              if((choice(1:4).eq.'DEL '.or.choice(1:4).eq.'MERG').and.
     *        n_wav_del.eq.0.and.n_wav_mer.lt.2.and.opmode.eq.0) then
                 goto 1100
              endif
           endif

        endif           ! this endif is end of options in graphics mode


c
c---------------------------------------------------------------------
c   for the remaining options, screen must be cleared and cursor moved
c   to text window 
c---------------------------------------------------------------------
c
c        call clear_to_alpha
c
c this part is moved to individual parts below to not close
c grapics when running from se (port.ne.0), lo 3/1/2017
c


c
c-------------------------------------------------------
c   Delete S-file
c------------------------------------------------------
c
        if(choice.eq.'DELS'.and.opmode.eq.0) then
          write(*,*) ' deleting s-file ...'  ! lot 22-04-2002
           keys(1:4)='D   '
           if (port.eq.0) call clear_to_alpha
           goto 1000
         endif

c 
c---------------
c   merge files
c---------------
c
        if(choice.eq.'MERG'.and.n_wav_mer.gt.1) then
c
c   write s-file so merge can be done
c
              call clear_to_alpha
              xsfile = sfile
              call write_s_file

c
c   make a list of files to merge for extract
c
              call sei open( unknown$,          ! Open file.
     &                      ' ',           ! No prompt.
     &                      'extract.inp', ! This file.
     &                      any_unit    ,  ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
c
              do i=1,n_wav_mer
                  write(any_unit,'(7x,a)') wav_filename(i)
              enddo
              call sei close( close$, any_unit, code )
c
c   merge files
c
              long_text='wavetool -wav_files extract.inp'// 
     *        '  -wav_out_file SEISAN'
              write(6,*) long_text
              call systemc(long_text,seiclen(long_text))
c
c   find if merge was ok
c
              call get_message('extract',text)
              if(text(1:2).ne.'OK') then
                 write(6,*)' Merged failed ************'
                 write(6,*)' Return to continue        '
                 call flush (6)
                 read(5,'(a)')text 
              else
                 filen(1:60)=text(3:62)   ! new file name
c
c   read s-file with new name of waveform file
c
                 sfile = xsfile
                 call read_s_file
c
c   remove merged files from s-file if more than one
c
                 if(n_wav_mer.gt.1) then
                    k=2
                    do i=2,nrecord
                      if(data(i)(80:80).eq.'6'.and.data(i)(1:1).eq.'*')
     *                then
                         continue    ! just skip line
                      else
                         data(k)=data(i)
                         k=k+1
                      endif
                    enddo
c
c   put in new name of merged file in line 2
c
                    do i=nrecord,2,-1
                       data(i+1)=data(i)
                    enddo
                    data(2)=' '
                    data(2)(2:61)=filen(1:60)
                    data(2)(80:80)='6'
                    nrecord=nrecord-n_wav_mer+1
                    nhead=nhead-n_wav_mer+1
                    data(nrecord)=' '
                 endif 
c
c   write s file again
c
                 call write_s_file
c
c  go and select again from very start
c
cx                 keys(1:4)='    '   ! same event in eev
                 one_plot=.false.   ! do not return to eev
                 goto 1000
              endif
        endif

c
C---------------------------------------------------------------------                                                    
C   DELETE FILES ?                                                         
C-------------------------------------------------------------------
c          
        if(choice.eq.'DEL ') then                                            
           call clear_to_alpha
           if(choice.eq.'DEL ') 
     *     WRITE(6,'(a,$)')
     *  '  SURE YOU WANT TO DELETE WAVEFORM FILE (Y/N) ??????? '        
           call flush (6)
           READ(5,'(A1)') answer                                             
           IF(answer.EQ.'Y'.or.answer.eq.'y') then                      
c
c-- CLOSE and DELETE FILE(S)
c
c   check if one or several of many files to delete 
c
c suggested change RL 1/20006, kept at 0 lot
c               if(n_wav_del.gt.1) then   ! many files, must be from s-file
c               if(n_wav_del.gt.0) then   ! many files, must be from s-file
                if (opmode.eq.0) then    ! lot 17/05/2006
                  do i=1,n_wav_del       ! delete
                     if(sun.or.linux) then
                         write(text,'(a,a)')'rm ',wav_filename(i)(1:60)
                      else
                         write(text,'(a,a)')'del ',wav_filename(i)(1:60)
                      endif
                      write(6,'(a,a)') 'Deleting: ',
     *                wav_filename(i)(1:60)
c no delete from s-file                       call systemc( text,seiclen(text) )
                  enddo
c
c   remove deleted files from s-file
c
                  k=2
                  do i=2,nrecord
                     if(data(i)(80:80).eq.'6'.and.data(i)(1:1).eq.'*')
     *               then
                        continue    ! just skip line
                     else
                        data(k)=data(i)
                        k=k+1
                     endif
                  enddo
                  nrecord=nrecord-n_wav_del
                  nhead=nhead-n_wav_del
                  data(nrecord)=' '
c
c   convert possible readings and write the corrected s-file
c   seems no need here to convert, make dim problems jh 2-2013
c
c                  iswitch=0
c
c                  write(6,*)'current chan,stat,comp',current_chan,
c     *            wav_stat(current_chan),                     
c     +            wav_comp(current_chan)
c                  call convert(wav_abs_time(wav_first),
c     *            wav_stat(current_chan),                     
c     +            wav_comp(current_chan),                            
c     +            iswitch)
c
c   write s-file
c
                  call write_s_file
c
c  go and select again from very start
c
                  keys(1:4)='    '   ! same event in eev
                  one_plot=.false.
                  goto 1000
               else
c
c   one input file
c                     
                  if(sun.or.linux) then
                     write(text,'(a,a)')'rm ',wav_filename(1)(1:60)
                  else
                     write(text,'(a,a)')'del ',wav_filename(1)(1:60)
                  endif
                  call systemc( text,seiclen(text) )
 
                  if(opmode.eq.1) then
                     if(default) then
                        question(1:11)='#Next_file#'
                     else
                        question(1:11)='           '
                     endif
                  endif
c
c-- new event                                       
c
                  keys(1:4)='NEXT'   ! in case from eev
                  goto 1000   
               endif   ! endif of delete many files
           endif       ! endif of confirm delite
c
c   if here, assumed wrong choice,  close plot file (if any)
c   and replot
c
           if(plotoption.ge.1) call sei close( close$,   ! Close plot.
     &                                      plotunit, ! Unit.
     &                                      code )    ! Condition (n/a).
           goto 1100                                                             
        endif            ! endif of delete section                                                    
c
c---------------------------------------------------------------------
c   register  (put) event using eev                                                              
c---------------------------------------------------------------------
c
        if(choice.eq.'PUT '.and.opmode.eq.0) then
           if (port.eq.0) call clear_to_alpha
           write(*,*) ' register existing s-file ... '
           keys(1:3)='REG'
           goto 1000
        endif          
c
c---------------------------------------------------------------------
c   register  (put) event  using filenr.lis or cont base or archive  
c---------------------------------------------------------------------
c          
        if(choice.eq.'PUT ') then                            
           call clear_to_alpha
           write(6,*)                                                         
           WRITE(6,'(a)')
     &  ' ENTER EVENT TYPE L,R OR D '
           write(6,'(a,a)')
     *   ' Second. optional character for event ID (e.g. E)'
           write(6,'(a,$)')
     *   ' Third optional character for model ID (e.g. J) '
c          call flush (6)
           model=' '
           READ(5,'(A2,a1)')TYPE_kind,model                                                 
c
c   make sure offical ID are upper case
c
           if(type_kind(2:2).eq.'p') type_kind(2:2)='P'
           if(type_kind(2:2).eq.'q') type_kind(2:2)='Q'
           if(type_kind(2:2).eq.'e') type_kind(2:2)='E'
 846       CONTINUE                                                           
           IF(TYPE_kind(1:1).EQ.'l') type_kind(1:1)='L'                                           
           IF(TYPE_kind(1:1).EQ.'d') type_kind(1:1)='D'                                           
           IF(TYPE_kind(1:1).EQ.'r') type_kind(1:1)='R'                                           
           IF(TYPE_kind(1:1).NE.'L'.AND.TYPE_kind(1:1).NE.'D'
     *        .AND.TYPE_kind(1:1).NE.'R') THEN               
              WRITE(6,*)' WRONG TYPE, TRY AGAIN'                              
              call flush (6)
              READ(5,'(A2)') TYPE_kind                                             
              GOTO 846                                                        
           ENDIF                                                              

C bjb 18/3/97 
C If a local volcanic event then we want register a sub class defined in
C the file VOLCANO.DEF located in the DAT directory.
C The format of this file will be one line of text (80A) followed by
C i2,1x,6A,1X,40A for number, code, space, description
c
c next lines moved down to be run only if 'V' LO, 04.97
c
c      valid_class=.FALSE.
c      sub_class_flag='  '
c
           IF(TYPE_kind(2:2).eq.'v') TYPE_kind(2:2)='V'
              IF(TYPE_kind(1:1).eq.'L'.and.TYPE_kind(2:2).eq.'V') THEN

c
c call routine to select subclass, lo May 27, 2002
c
              call select_volcano_subclass(sub_class_flag)
 
c              valid_class=.FALSE.
c              sub_class_flag='  '
cC Create path name for VOLCANO.DEF on sun or PC
c              vol_file=top_directory(1:60)
c              vol_file(seiclen(top_directory)+1:
c     *        seiclen(top_directory)+1)=dchar
c              vol_file(seiclen(vol_file)+1:seiclen(vol_file)+3)='DAT'
c              vol_file(seiclen(vol_file)+1:seiclen(vol_file)+1)=dchar
c              vol_file(seiclen(vol_file)+1:)='VOLCANO.DEF'
c
c              CALL sei open(check$,
c     &                  ' ',
c     &                  vol_file,
c     &                  0,
c     &                  exist,
c     &                  code)
c              IF(.not.exist) THEN 
c                 WRITE(6,*)
c     *           'Warning: Cannot find VOLCANO.DEF file in DAT dir'
c                 GOTO 3413
c              ENDIF
cc
cC If we are here then we want to open up VOLCANO.DEF file and print to
cC the screen and let the user select which code he wishes to enter
cC in line 1 col 56
cc
c              CALL sei open(old$,
c     &                ' ',
c     &                 vol_file,
c     &                read2,
c     &                exist,
c     &                code)
c 3922         READ(read2,'(a80)') comment
c              WRITE(6,*)' '
c              WRITE(6,*) comment
c
c              WRITE(6,*)'---------------------------------------'
c              Write(6,*)'Number Sub-Class Description'
c              WRITE(6,*)'---------------------------------------'
c              DO i=1,60
c                 READ(read2,'(i2,1x,a6,1x,a40)') n_class(i),s_class(i),
c     &                                    description
c                 WRITE(6,'(4x,i2,1x,a6,4x,40a)') n_class(i),s_class(i),
c     &                                    description
c
c                 IF(description.eq.'QUIT') GOTO 3923
c              ENDDO
c 3923         CONTINUE
c              WRITE(6,*)'---------------------------------------'
c              WRITE(6,*) 'Please Enter number for sub-class : '
c              call flush (6)
c              READ(5,'(i2)') n_select 
cC Test for valid sub_class code number
c              DO i=1,60
c                 IF(n_select.eq.n_class(i)) then
c                    valid_class=.TRUE.
c                    sub_class_flag=s_class(i)
c                    WRITE(6,*)'Sub-class ', sub_class_flag, ' selected.'
c                 endif
c              ENDDO
c              IF(.not.valid_class) THEN
c                 WRITE(6,*)'Invalid Sub-class code. Try again'
c                 REWIND(read2)
c                 GOTO 3922
c              ENDIF
c              CALL sei close(close$,read2,code)
           ENDIF        ! end of volcanic section

c
c   get operator code
c
 3413     continue    ! enter here if no operator code
          if(operator.eq.'    ') then
             write(6,*)' Give operator code (max 4 char)'
             call flush (6)
             read(5,'(a4)') operator
             write(6,*)
     *     ' Give 2-5 letter data base, ,, for local dir,',
     *     ' return for default base'
             call flush (6)
             read(5,'(a5)')s_dir(1:5)
             if (s_dir(1:1).ne.' ') then
                do x=2,5
                   if (s_dir(x:x).eq.' ') s_dir(x:x)='_'
                enddo
             endif
             if(operator.eq.'    ') goto 3413
          endif
c     
c take out the synthetic phases for register , lo 09/99
c
          do k=1,nrecord
             data1(k)=data(k)
          enddo
          do k=1,max_data
                data(k) = ' '
          enddo
          i=0
          do k=1,nrecord
               if (data1(k)(10:10).ne.'Y') then
                 i=i+1
                 data(i) = data1(k)
               endif
           enddo
           nrecord = i
c
c   generate extract file if cont data base, arc  or large seed volume 
c
           if(cwav.or.cseed.or.arc) then

c
c   open and write in file for choises to extract program
c
              call sei open( unknown$,     ! Open file.
     &                      ' ',           ! No prompt.
     &                      'extract.inp', ! This file.
     &                      any_unit    ,  ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
c
              window=stop_time-start_time
c
c  there is now several choises
c
 3737         continue           ! from just below
              write(6,*)'Output channels on screen: s '
              if(arc)then
                 write(6,*)'No output, just register:  n'
                 write(6,*)'No output, just ARC line:  a'
              endif
              write(6,*)'Output all channels:   enter'
              write(6,*)
              question=' '
              read(5,'(a1)') question

              if(question.ne.'s'.and.question.ne.' '.and.
     *        question.ne.'n'.and.
     *        question.ne.'a') then
                 write(6,*)'Wrong answer'
                 goto 3737
              endif

              no_arc_extract=.false.
              if(question.eq.'n'.or.question.eq.'a') 
     *        no_arc_extract=.true.

c
c  make a requst file with channels desired
c
              if(question.ne.' '.and.question.ne.'a'.and.
     *        question.ne.'n') then
                 write(any_unit,*) nchan
                 do i=1,nchan
                    write(any_unit,'(i3,a)') channelname(i),' 0.0 0.0'
                 enddo
              else
                 write(any_unit,*) wav_nchan
                 do i=1,wav_nchan
                    write(any_unit,'(i3,a)') i,' 0.0 0.0'
                 enddo
              endif

              call sei close( close$, any_unit, code )
              wave_out=.false.
c
c-------------------------------------------------------------------
c   extract file and put in S-file, copy to WAV
c   if ARC, choise might be only to register
c-------------------------------------------------------------------
c


c
c---------------------------------------------------------------------------
c  if a subset of channels have been selected before data bases are opened 
c  with the standard set of channesl in SEISAN.DEF (cont or arc), this 
c  subset must be written in a file so wavetool uses only the subset and not 
c  the  channels in SEISAN.DEF. the same file is used.
c---------------------------------------------------------------------------    

c
c  for cont
c

                if(cwav.and..not.arc.and.n_cont_base_sel.gt.0) then 
                    call sei open( unknown$,     ! Open file.
     &              ' ',           ! No prompt.
     &              'cbase.inp', ! This file.
     &              any_unit    ,  ! On this unit.
     &              b_flag,        ! Existance.
     &              code )         ! Condition not applicable.
                    do i=1,n_cont_base_sel
                       write(any_unit,'(a)')
     *                 cont_base(i)(1:seiclen(cont_base(i)))//' ' 
                    enddo
                    call sei close( close$, any_unit, code )
                endif
c
c   for arc, always write out since station are sorted in mulplt
c   and could have a different order from channels in SEISAN.DEF
c
                if(arc.and.arc_nchan.gt.0) then 
                    call sei open( unknown$,     ! Open file.
     &              ' ',           ! No prompt.
     &              'cbase.inp',   ! This file.
     &              any_unit    ,  ! On this unit.
     &              b_flag,        ! Existance.
     &              code )         ! Condition not applicable.
                    do i=1,arc_nchan
                       write(any_unit,'(a5,a3,a2,a2,1x,2f15.3)')
     *                 arc_stat(i),arc_comp(i),arc_net(i),arc_loc(i),
     *                 arc_start(i),arc_stop(i)
                    enddo
                    call sei close( close$, any_unit, code )
                endif
c

c------------
c  large seed
c------------
              if(cseed) then
                 window_abs_time=wav_abs_time(wav_first)+
     *           start_time
	        else
                  window_abs_time=cwav_abs_start_time+start_time
              endif
              call sectim
     *        (window_abs_time,year,i,month,day,hour,min,sec)
              i=sec+0.5
              write(text,'(i4,5i2)')year,month,day,hour,min,i
              do i=1,14
                 if(text(i:i).eq.' ') text(i:i)='0'
              enddo

c--------------
c  seisan cont
c--------------

              if(cwav.and..not.arc) then
              write(text1(1:10),'(f10.1)') window
                long_text='wavetool -start '//text(1:14)//
     *          ' -cwav -duration '//text1(1:10)
     *          //' -chansel extract.inp -wav_out_file SEISAN'

c
c only selected databases for seisan type cont data if not all data bases used
c
                if (n_cont_base_sel.gt.0) then
                   long_text=long_text(1:seiclen(long_text))//
     *             ' -cbase cbase.inp '
                endif
              endif
c----------
c   ceseed
c----------

              if(cseed) then
                write(text1(1:10),'(f10.1)') window
                long_text='wavetool -start '//text(1:14)//
     *          ' -cseed -duration '//text1(1:10)
     *           //' -sfile mulplt.out'                       ! always mulplt.out
     *          //' -chansel extract.inp -wav_out_file SEISAN'
              endif
c
c--------------------------------
c  arc extract, could be optional
c---------------------------------
c
              if(no_arc_extract) goto 4445  ! skip extract
                if(arc) then
                   write(text1(1:10),'(f10.1)') window
                   arc_text=' -arc '   ! set archive
                   long_text='wavetool -start '//text(1:14)//
     *             arc_text//' -duration '//text1(1:10)
     *             //' -wav_out_file SEISAN'
     *             //' -chansel extract.inp'
c
c always use cbase.inp for arc 
c
                   long_text=long_text(1:seiclen(long_text))//
     *             ' -cbase cbase.inp '
                endif
c
c-------------------------------------------------
c  now extract file
c-------------------------------------------------
c

              write(6,*) long_text
              call systemc(long_text,seiclen(long_text))
c
c   get file name
c
              call get_message('extract',long_text)
c
c   check if ok
c
   
             if(long_text(1:2).ne.'OK') then
                  write(6,*)
     *            ' Something wrong with wavetool, return to continue'
                  read(5,'(a)') i
                  goto 1100
              else
                  trace_file=' '
                  trace_file(1:seiclen(long_text)-2)=
     *            long_text(3:seiclen(long_text))
              endif

 4445         continue  ! from just above, arc extract skipped

c-----------------------------------------------------------------
c  if ARC line and no extract, generate ARC line in waveform line
c-----------------------------------------------------------------
           
             if(question.eq.'a') then
c
c  find if 6 line from before, then delete, assume only one
c
                k=1
                do i=2,nrecord
                   if(data(i)(80:80).ne.'6') then
                      k=k+1
                      data(k)=data(i)
                   else
                      continue
                   endif
                 enddo

                 if(k.lt.nrecord) then
                    data(nrecord)=' '
                    nrecord=nrecord-1
                    nhead=nhead-1
                 endif

                 text=' '
                 text(80:80)='6'
                 text(2:4)='ARC'
                 text(6:6)='*'

                 start_time_duration=' '
c
c   set start time, used plotted window start time as start time
c

                 call sectim
     *          (window_abs_time,year,i,month,day,hour,min,sec)         
                 write(start_time_duration(1:17),
     *          '(i4,1x,2i2,1x,2i2,1x,i2)')
     *           YEAR,MONTH,DAY,HOUR,MIN,int(SEC)
c
c   set duration, 600s if not defined in seisan.def
c
                 if(arc_duration.eq.0.0) arc_duration=600.0
                 k=arc_duration
                 write(start_time_duration(18:23),'(i6)') k
                 text(22:44)=start_time_duration
                 trace_file(1:50)=text(2:51)  ! shift since it is now a file name
              endif          
c
c  if only register and arc, no waveform file name
c
              if(question.eq.'n') trace_file=' '

              CALL new_s_file(year,month,
     *            day,hour,min,sec,type_kind, 
     *            s_dir,trace_file,new_sfile,data,operator,
     *            sub_class_flag,nhead,nrecord)
                  data(1)(21:21)=model
           else
c
c    case of normal file
c    bjb 2001/02/13 Remove path from filename if normal waveform file
c
c
c   trim filename for leading directory structure
c
              lastsl=r_index(wav_filename(1),dchar)
              trace_file=wav_filename(1)(lastsl+1:)

              CALL new_s_file(wav_year(wav_first),wav_month(wav_first),
     *            wav_day(wav_first),wav_hour(wav_first),
     *            wav_min(wav_first),
     *            wav_sec(wav_first),type_kind, 
     *            s_dir,trace_file,new_sfile,data,operator,
     *            sub_class_flag,nhead,nrecord)
           endif
c
c   add model
c
           data(1)(21:21)=model
c  
c-- CHECK IF S-FILE ALREADY EXIST, this section the same for single files
c   and cont data base
c
           call sei open( check$,              ! Check file exists.
     &                      ' ',                 ! No prompt.
     &                      new_sfile,           ! This filename.
     &                      0,                   ! Unit (n/a).
     &                      exist,               ! File exists?.
     &                      code )               ! Condition (n/a).
           write(6,*)                                                         
           write(6,'(a,2x,a)')' S-file name: ',new_sfile(1:
     &              seiclen(new_sfile))
           if(exist) THEN                                                     
                write(6,*)' S-FILE ALREADY EXISTS, OPTIONS ARE:       '
                write(6,*)' Ignore (leave old event)            Return'
                write(6,*)' Overwrite duplicate                 o     '
                write(6,*)' Create new event, different ID:     n     '
                call flush (6)
                READ(5,'(A1)') answer
c
c   overwrite
c
                IF(answer.EQ.'o'.OR.answer.EQ.'O') THEN                         
                   WRITE(6,*)' REALLY, REALLY SURE ?'
                   call flush (6)
                   READ(5,'(A1)') answer                                        
                ENDIF
c
c   ignore
c 
                if(answer.eq.' ') then
                   write(6,*)' Event not registered, return to continue'
                   call flush (6)
                   read(5,'(a)') answer
                   answer=' '   ! make sure a y do not come here
                endif
c
c   new id
c
                if(answer.eq.'n'.or.answer.eq.'N') then
                   i=seiclen(new_sfile)
                   k=1
 5857              continue
                   write(6,*) ' Creating a new id for event'
                   call inc_id(data(2),new_sfile,i) ! id line is assumed to be in line # 2
                   inquire(file=new_sfile,exist=exist)
                   k=k+1
c
c   check if not too many duplicates
c
                   if(k.gt.59) then
                      write(6,*)
     *                ' Cannot make a duplicate,',
     *                ' more than 60 events with same second'
                      write(6,*)' Return to stop'
                      call flush (6)
                      read(5,'(a)') answer
                      stop
                   endif
                   if(exist) then
                      goto 5857    ! try again
                   else
                      answer='y'   ! accept new event
                   endif
                endif
           ELSE                  ! file do not exist
                WRITE(6,739)                                           
 739            FORMAT(' GO AHEAD (Y/N)')              
c                call flush (6)
                READ(5,'(A1)') answer                                           
           ENDIF                                                              
C                                                                               
C  OPEN AND WRITE S-FILE                                                        
C                                                                               
           if(answer.EQ.'Y'.OR.answer.EQ.'y') then                            
              call sei open( unknown$,       ! Open file.
     &                      ' ',             ! No prompt.
     &                      new_sfile,       ! This file.
     &                      write44,         ! On this unit.
     &                      b_flag,          ! exist
     &                      code )           ! Condition (n/a).

              WRITE(write44,'(A80)',iostat=code)
     &                 (DATA(I),I=1,nrecord) ! Write record.
c             WRITE(6,'(A80)',iostat=code)
c    &                 (DATA(I),I=1,nrecord) ! Write record.
              call sei code( fort$, code, write44, b_flag )     ! Process outcome.
              call sei close( close$, write44, code )           ! Close down.
c
c   now write following picks to data base, but only if not cont or large seed and arc 
c
              if(.not.cwav.and..not.cseed) then
                 sfile=new_sfile                 ! mar 19
                 call put_env_event(sfile)       ! mar 19, make sfile available
             
c
c   optinally start a process
c
              if(auto_process.gt.0) then
                 if(auto_process.eq.2) then
                    write(6,'(a,a)') ' Run process(y/return=n)? '
     *              ,auto_process_name
                     call flush (6)
                     read(5,'(a1)') answer
                     if(answer.eq.'y'.or.answer.eq.'Y') then
                        call systemc(auto_process_name,10)
                     endif
                  else
                     call systemc(auto_process_name,10)
                  endif
              endif  ! end of start a process
                  
c
c run hypocenter
c
              if(auto_locate.gt.0) then
                 if(auto_locate.eq.2) then
                    write(6,'(a,a)') 
     &              ' Locate event (y/return=n))? '
                    call flush (6)
                    read(5,'(a1)') answer
                    if(answer.eq.'y'.or.answer.eq.'Y') then
                       call systemc('hyp',3)
                    endif
                 else
                    call systemc('hyp',3)
                 endif
c
c  save location in s-file if desired 
c
                 if(auto_update.gt.0) then
                    if(auto_update.eq.2) then
                        write(6,*)
     &                  ' Do you want to save location, (y/return=n) ?'
                         call flush (6)
                         read(5,'(a1)') answer
                         if(answer.eq.'y'.or.answer.eq.'Y') then
                            if(sun.or.linux)call 
     &                      systemc('cp hyp.out '//sfile,91) 
                            if(pc) call 
     &                      systemc('copy hyp.out '//sfile,80)
                         endif
                      else
                         if(sun.or.linux)call
     &                   systemc('cp hyp.out '//sfile,91)
                         if(pc) call
     &                   systemc('copy hyp.out '//sfile,80)   ! bug ??
                      endif 
                   endif
c
c end of auto-location section
c
              endif
              endif   ! of not cont
c
c now copy if not ARC without making wav file
c
              if(no_arc_extract) goto 3389

c                                                                               
c   copy trace file name to WAV
c
              write(cwavyear,'(I4.4)') wav_year(1)
              write(cwavmon,'(I2.2)') wav_month(1)

              if(cwav.or.cseed) then
                 filen=trace_file
              else
                 filen=wav_filename(1)    ! assume only one
              endif
              if( pc ) then
                 if(copy_wav_dir.eq.' ') then
                    long_text = 'copy ' // filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               filen(:seiclen(filen))
                 else
                    long_text = 'copy ' // filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               copy_wav_dir//dchar//cwavyear(1:4)//dchar //
     &               cwavmon(1:2)//dchar
c     &               copy_wav_dir//dchar//filen(1:4)//dchar //
c     &               filen(6:7)//dchar                     
c     &               filen(:seiclen(filen))
                 endif
c
c bjb modified wavefile name to remove path when writing into S-file
c
              else if( sun.or.linux ) then
                 if(copy_wav_dir.eq.' ') then
                    long_text = 'cp   ' // filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               trace_file(:seiclen(trace_file))
                  else
                     long_text = 'cp   ' // filen(:seiclen(filen)) //
     &                 ' '                                    //
     &               top_directory(:seiclen(top_directory)) //
     &               dchar//'WAV'//dchar                    //
     &               copy_wav_dir//dchar//cwavyear(1:4)//dchar //
     &               cwavmon(1:2)//dchar
c     &               copy_wav_dir//dchar//trace_file(1:4)//dchar //
c     &               trace_file(6:7)//dchar                      //
c     &               trace_file(:seiclen(trace_file))
                  endif
              else
                  chr_err_msg$ = 
     &'**** ERROR: could not determine the computer type'
                  call sei code( stop$, e_init$, 0, b_flag ) ! Halt program
              end if                                     !
c
              write(6,'(1x,a)') long_text                                          

   
              call systemc( long_text,
     &                         seiclen(long_text) )
c
c  check that file got there
c
              call sei open( check$,              ! Check file exists.
     &                      ' ',                 ! No prompt.
     &        long_text(7+seiclen(filen):seiclen(long_text)),
     &                      0,                   ! Unit (n/a).
     &                      exist,               ! File exists?.
     &                      code )               ! Condition (n/a).
              if(exist) then
                  write(6,*)' File transferred to WAV **********'               
              else
                  write(6,*)' Failure to transfer to WAV ****'
                  write(6,*)' Return to continue'
                  call flush (6)
                  read(5,'(a4)') i
              endif

 3389         continue  ! get here from just above if skipping wav copy

              write(6,*)                                                      
c
c   event was registered, go to next or another according to opmode,
c   however if in cont mode, replot
c
              if(cwav.or.cseed) then
                 choice='REPL'
                 write(6,*)' Continue plot(y/n=default)'
                 read(5,'(a)') text
                 if(text(1:1).eq.'y') then
                    goto 1100
                 else
                   goto 999 
                 endif
              else
                 choice='FINI'
              endif
           else
               write(6,*)' You did not register event'
c
c   if here, assume  pressed by mistake, replot
c   clean out data fil by resetting line counters
c
               nrecord=0
               nhead=0
c
               goto 1100                                                      
           endif                                                              
        endif               ! end of register (PUT) section
c
c-----------------------------------------------------------------
c   for remaining options, there might be a hard copy file which
c   could be sent to the plotter, check !                     
c-----------------------------------------------------------------
c
c   make hard copy plot                                                         
c                                                                               
        if(p_option.gt.1) then                                               
              call close_post
              call sei close( close$, plotunit, code ) ! Close (stop on error).
              write(6,'(a,$)')' Submit plot (y=return/n) '                      
              call flush (6)
              read(5,'(a)') dummy                                               
              if(dummy.eq.' '.or.dummy.eq.'Y'.or.dummy.eq.'y') 
     *        call send_plot("mulplt.eps",10)
        endif
c
c------------------------------------------------------------------------
c   check if other channels 
c------------------------------------------------------------------------
c
        if(choice.eq.'OTHE') then
              call clear_to_alpha
              if(default) then         ! unset default in order to set ch.
                 default_org=default
                 default=.false.
              endif
c
c   show menu of other channels
c
              call channelselect(wav_nchan,wav_stat,wav_comp, 
     *        nchan,nchan_def,stat_def,comp_def,default,1)
c
c   stop if zero number of channels, can be caused by a q in selection
c
              if(nchan.eq.0) goto 999
        do i=1,nchan
           channelname_org(i)=channelname(i)
        enddo
c
c   save original number of channels, used if several screens are plotted
c
        nchan_org=nchan
c
c   set screen number
c
        n_screen=1
c
c
c   reset default
c
              default=default_org
              goto 1099                ! other channels
        endif
c
c-----------------------------------------------------------------------
c    check if other waveform file when called from eev
c-----------------------------------------------------------------------
c
        if(choice.eq.'OTWA' ) then
             call clear_to_alpha
             if(opmode.eq.0) then
                one_plot=.false.   ! allow one more plot
                default=.false.    ! only if no default will file menu be shown
                goto 1000
             endif
        endif
c
c------------------------------------------------------------------------
c   check if next or another event from filenr.lis or next event
c   from eev is wanted, also next cont interval
c------------------------------------------------------------------------
c          

        if(choice.eq.'FINI') then
c             write(6,*)'w,s,a',cwav,cseed,arc
              if(cwav.or.cseed.or.arc) then            ! cont base
                  call clear_to_alpha
                  cwav_abs_start_time=cwav_abs_start_time+  ! advance start
     *            cont_interval*0.8
                  call cwav_time_limits(1)             ! calculate new lim
c                 write(6,*)'cwav,int',cwav_abs_start_time,cont_interval
c                 write(6,*)'cwav mulplt',cwav_start_time
                  goto 1050                            ! next window
              endif
              if(opmode.eq.1) then     ! from filenr.lis
                 call clear_to_alpha
                 if(default ) then    
                     question(1:11)='#Next_file#' ! next event chosen, no quesitons
                 else
                     question(1:11)='           '
                 endif
                 goto 1000            ! get next file from filenr.lis
              else
                 if(opmode.eq.0) then
                   if (port.eq.0) call clear_to_alpha
                   keys='NEXT'          ! - from data base, next event
                   goto 1000
                 else
                   call clear_to_alpha
                   goto 999             ! stop if called from eev
                 endif
              endif
        endif


c
c------------------------------------------------------------------------
c   check if back an event from filenr.lis or back one event
c   from eev is wanted, also back in cont interval
c------------------------------------------------------------------------
c
        if(choice.eq.'BACK') then
              if(cwav.or.cseed) then            ! cont base
                  call clear_to_alpha
                  cwav_abs_start_time=cwav_abs_start_time-  ! advance start
     *            cont_interval*0.8
                  call cwav_time_limits(1)             ! calculate new lim
                  goto 1050                            ! next window
              endif
              if(opmode.eq.1) then     ! from filenr.lis
                 call clear_to_alpha
                 if(default ) then    
                     question(1:11)='#Back_file#' ! next event chosen, no quesitons
                 else
                     question(1:11)='           '
                 endif
                 goto 1000            ! get next file from filenr.lis
              else
                if(opmode.eq.0) then
                   if (port.eq.0) call clear_to_alpha
                   keys='BACK'          ! - from data base, next event
                   goto 1000
                 else
                   call clear_to_alpha
                   goto 999             ! stop if called from eev
                 endif
              endif
        endif

c
c   end of multitrace option, very far back up 
c
      endif



         
c----------------------------------------------------------------------------------                                                                               
c-----------------------------------------------------------------------        
cccccc      start of section where channel data are picked   cccccccc
c           single trace option 0, only option left so no if
c-----------------------------------------------------------------------
c----------------------------------------------------------------------------------
c
c


c
c   select channels
c


       call open_display
       call channelselect(wav_nchan,wav_stat,wav_comp,                   
     * nchan,nchan_def,stat_def,comp_def,default,1)
       call clear_to_alpha
c
c  sort after distance or time
c
       call channel_order(wav_stat,wav_delay,nchan,nsynt)                               
c
c  save original selection
c
       do i=1,nchan
          old_chan(i)=channelname(i)
       enddo
       old_nchan=nchan
c
c   stop if zero number of channels, can be cause by a q in selection
c
       if(nchan.eq.0) goto 999
c
c-------------------------------------------------------------------------
c   single trace mode loop starts here, also get here if jump out
c   of multi trace mode above.
c   if back from a jump to all channel mode, start with same channel
c   as before
c-------------------------------------------------------------------------
c
       i=0           ! start with channel one

 2000 continue       ! single channel loop
c
c   handle defualt filter for single for first plot
c
      filt_single=0
      if(def_single) then
         flow(9)=flow_def
         fhigh(9)=fhigh_def
         filt_single=1
         filt=filt_def
      endif
c
c   open in a new window for pic mode, only for dislin on windows, else same window
c 
      if(.not.single_open) then
         k=wsize*0.8
         call xwindow_size(k,60,70)
         call xopen_window(3)
         text=' '
         text(1:25)='MULPLT single trace mode'
         call xwindow_title(text)
         single_open=.true.
      endif            

      i=i+1
      if(i.le.0) i=1
c
c   in all component mode roll around when reaching end of channel list
c
      if(i.gt.nchan.and.allcomp) then
         i=1
      endif
c
c   at end of loop, go to multitrace mode
c
        if(i.gt.nchan) then
c
c  close single trace window
c
           call xclose_window(3)
           single_open=.false.
c
           if(nchan.eq.1.or.nchan_single.eq.1) then
              do l=1,old_nchan
                 channelname(l)=old_chan(l)
              enddo
              nchan=old_nchan
           endif
           goto 1100
        endif

c                                                                               
c  read one channel data                                                        
c                                                                               
         ichan=channelname(i)   
c        do i=1,wav_nchan
c          write(6,*) i,channelname(i)
c        enddo
c        write(6,*) 'mulplt i, ichan',i, ichan
c
c   check if baz angles should be calculated
c
         if(rotate.and.no_baz) then
            call get_baz
     *      (wav_nchan,wav_stat,data,nhead,nrecord,baz)
            no_baz=.false.      ! indicate that baz has been calculated
         endif
         resolu=resolu_x
c
c   read one channel, might have to be rotated in which case both horizontal
c   must be read
c

         wav_rot_comp(ichan)=' '
         wav_rot_delay(ichan)=0.0
          if(rotate.and.baz(ichan).lt.400.0.
     *    and.wav_comp(ichan)(4:4).ne.'Z') then
             call wav_read_2channel(ichan)

c
c   check of both channles there
c
             if(wav_error_message.ne.' ') then
                 write(6,*)wav_error_message
                 call wav_read_channel(ichan)
                 numb_samp=wav_nsamp(ichan)
                 goto 3030                   ! jump to normal reading
             endif


             numb_samp=wav_out_duration(1)*wav_rate(ichan)+1
             if(wav_comp(ichan)(4:4).eq.'N') wav_rot_comp(ichan)='R'
             if(wav_comp(ichan)(4:4).eq.'E') wav_rot_comp(ichan)='T'
             call rotate_comp(numb_samp,
     *       wav_out_first_sample(wav_current_chan(2)),
     *       wav_out_first_sample(wav_current_chan(3)),
     *       wav_rot_comp(ichan),
     *       baz(ichan),signal1,signal2,signal3)
c
c   replace parmeters for current channel due to rotation, time
c   start and interval might be different if 2 horizontal channels
c   have different start times
c
             wav_rot_delay(ichan)=wav_out_start(1)
     *       -wav_delay(ichan) ! normally zero
          else
c            write(6,*)'ichan before call to wav read',ichan
             call wav_read_channel(ichan)
             numb_samp=wav_nsamp(ichan)
          endif
 3030     continue

         rate=wav_rate(ichan) 
c                                                                               
c   make plot header, rotated times could be different                         
c                          
         head=' '
         head(1:40)=wav_file_text(1:40)
         head(41:45)=wav_stat(ichan)
         head(47:50)=wav_comp(ichan)
         if(wav_rot_comp(ichan).ne.' ') 
     *   head(50:50)=wav_rot_comp(ichan)
         if(rotate) then
            write(head(54:80),'(i4,4i2,f6.3)')wav_out_year(1),
     *      wav_out_month(1),wav_out_day(1),
     *      wav_out_hour(1),
     *      wav_out_min(1),wav_out_sec(1)                 
         else
            write(head(54:80),'(i4,4i2,f6.3)')wav_year(ichan),
     *      wav_month(ichan),wav_day(ichan),wav_hour(ichan),
     *      wav_min(ichan),wav_sec(ichan)                 
         endif


c         
c check for null chars
c
         do k=1,80
           if(ichar(head(k:k)).eq.0) head(k:k)=' '
         enddo
c                                                                               
cccccc   Get old picks       ccccccccccccccc                                    
c                                                                               
         call read_s_file
         if(nrecord.ge.max_data) then
              write(6,*)
     *      ' Too many lines in readings file'
              write(6,*)' Maximum is ',max_data
              goto 999 
          endif
c
c   add synthetic readings if any                                         
c
          call add_synt_readings(nsynt,synt_head)
c
         iswitch = 0
         call convert(wav_abs_time(ichan),wav_stat(ichan),                        
     +   wav_comp(ichan),iswitch)
c                                                                               
ccccc   plot the data and pick phases   ccccccccccccccccccc                                   
c
c  open plot file if needed
c
         if(plotoption.ge.1) then                              
             call sei open( unknown$,      ! Open file.
     &                      ' ',           ! No prompt.
     &                      'mulplt.eps',  ! This file.
     &                      plotunit,      ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
         endif                                                                 
c                 
         call open_display
c
 2049    continue    ! from just below
c
c   add text of current mode to plot in single mode
c
        current_mode=' '
        if(allcomp)
     *  current_mode(1:34)='Single from 3 comp, back with t   '        
 
         call picc   ! pick phases, make spec etc, one channel, most done here

c
c   if in single mode from all comp mode, do not accept ALLC, only TOGL
c
         if(choice.eq.'ALLC'.and.allcomp) goto 2049                                                               
c
c   close plot file, note everything done in picking mode is on one page !
c
        if(plotoption.ge.1)  then 
           call close_post                               
           call sei close( close$, plotunit, code ) ! Close (stop on error).
        endif
c                                                                               
c   make hard copy plot                                                         
c                                                                               
        if(p_option.gt.1) then                                               
           call clear_to_alpha
           write(6,'(a,$)')' Submit plot (y=return/n) '                      
           call flush (6)
           read(5,'(a)') dummy                                               
           if(dummy.eq.' '.or.dummy.eq.'Y'.or.dummy.eq.'y') 
     *     call send_plot("mulplt.eps",10)
        endif
c
c-----------------------------------------------------------------
c   if replot, assume this or previous choice was rotate, 
c   go and read other horizontal channel and plot again
c-----------------------------------------------------------------
c
        if(choice.eq.'REPL') then
           i=i-1    ! use same channel
           goto 2000
        endif
c
c------------------------------------------------------------------                                                                               
c   Always save readings, also when quitting         
c------------------------------------------------------------------

         iswitch = 1                                                            
                                                                                
         call convert(wav_abs_time(ichan),wav_stat(ichan),                     
     +   wav_comp(ichan),iswitch)
c
         call write_s_file
c
c                                                                               
c   decide what to do next                                                      
c                                                                               
         if(choice.eq.'QUIT') then       ! save also if quit                   
             call clear_to_alpha
             goto 999
         endif
c
c    going back from single component to all component for same station
c
       if(choice.eq.'ALLC'.and.allcomp) goto 1100
c
c------------------------------------------------------
c  check if going to all component plot, assume here that
c  call did not come from all component mode
c-------------------------------------------------------
c
        all_from_single=.false.
        ichan_single=0

        if(choice.eq.'ALLC'.and..not.allcomp) then
           allcomp=.true.
c
c  close single trace window
c
           call xclose_window(3)
           single_open=.false.
c
c   save that request for all channel mode comes from single
c   channel mode. also save current channel number so it is possible
c   to come back to single mode to the same channel
c
           all_from_single=.true.
           ichan_single=i
c
c  save channel selection for single channel mode
c
           do l=1,nchan
              channelname_single(l)=channelname(l)
           enddo
           nchan_single=nchan
c
c   select all channels for selected station
c
                    nchan_all = 0       ! count expanded number of channels

                    ich=channelname(ichan_single)   ! currently selected channel
                    do l=1,wav_nchan  ! check if more channels with same station code
                       if(wav_stat(ich).eq.wav_stat(l)) then
                          nchan_all=nchan_all+1
c
c  save for sorting
c
                          stat_all(nchan_all)=wav_stat(l)
                          comp_all(nchan_all)=wav_comp(l)
                          channelname_all(nchan_all)=l    ! channel number in origial order
                       endif
                    enddo
c
c   sort again, make sure no menu comes up, use all channels so number
c   of defualt channels has been set to 0
c
                 default_sort=.true.                

                 call channelselect(nchan_all,stat_all,comp_all, 
     *           nchan,0,stat_def,comp_def,default_sort,1)
c
c   update channel array used for plotting
c   first get original numbers into channelname_org
c
                  do l=1,nchan_all
                    channelname_org(l)=channelname_all(channelname(l))
                  enddo
c
c  put orignal numbers back to channelname used for plotting
c
                  do l=1,nchan_all
                    channelname(l)=channelname_org(l)
                  enddo
                  nchan=nchan_all

              choice=' '       ! make sure to stay in all component mode
              call selwin(2)   ! focus on all component window
              goto 1100         ! go to multi channel plotting
         endif

c
c-------------------------------------------------------------------------------
c  check if back to initial multi or all component plot from single channel plot
c-------------------------------------------------------------------------------
c
        
        if(choice.eq.'TOGL') then
c
c  close single trace window
c
        call xclose_window(3)
        single_open=.false.
c
c   if back to all componenets, restore all components channels
c

           if(allcomp) then
c
c   back to single station allcomp
c
              if(.not.allcomp_many) then
              do l=1,nchan_all_station
                 channelname(l)=channelname_all_old(l)
              enddo
              nchan=nchan_all_station
              endif
c
c   back to multi station all comp
c
              if(allcomp_many) then
                 do l=1,nchan_all
                   channelname(l)=channelname_allc(l)
                 enddo
                 nchan=nchan_all
              endif
           endif
c
c   if only one channel was selected for single trace mode,
c   assume you want to go
c   back to old multi channel selection
c

          if(.not.allcomp_many) then
           if(nchan.eq.1.or.nchan_single.eq.1) then
              do l=1,old_nchan
                 channelname(l)=old_chan(l)
              enddo
              nchan=old_nchan
           endif
          endif
c
c   back to multi channels or all channel mode
c

           goto 1100
        endif
c
c----------------------------------
c-- chose other channels                        
c----------------------------------
c
        if(choice.eq.'OTHE') then
           if(default) then         ! unset default in order to set ch.
              default_org=default
              default=.false.
           endif
           call channelselect(wav_nchan,wav_stat,wav_comp,
     *     nchan,nchan_def,stat_def,comp_def,default,1)
           default=default_org
c
c   stop if zero number of channels, can be caused by a q in selection
c
           if(nchan.eq.0) goto 999
c
c  sort after distance or time
c
           call channel_order(wav_stat,wav_delay,nchan,nsynt)                  
           i=0
c
           goto 2000              ! start again
        endif
c-----------------------------------
c check if going back one channel
c-----------------------------------
        if(choice.eq.'BACK') i=i-2 
        goto 2000
c 
c---------------------------------------------------------------------------
c   End of channel picking loop in single channel mode
c---------------------------------------------------------------------------
c---------------------------------------------------------------------------                                                                  

 99    continue    

c
c-------------------------------------------------------------------------- 
c
      if(opmode.eq.1) then ! if in multiple event mode
         call clear_to_alpha
         goto 1000         ! another event
      endif                                                      
c                                                                               
 999  continue                                                                  
      call clear_to_alpha                                                     
c
c   there might be a hard copy file which
c   could be sent to the plotter, check !   ! 4-2001 jh: there never seesm to
c                                                        to be a plot, caused
c                                                        crash
c
c   make hard copy plot
c
c          if(p_option.gt.10) then     
c             call close_post
c             call sei close( close$, plotunit, code ) ! Close (stop on error).
c             write(6,'(a,$)')' Submit plot (y=return/n) '
c             call flush (6)
c             read(5,'(a)') dummy
c             if(dummy.eq.' '.or.dummy.eq.'Y'.or.dummy.eq.'y')
c    *        call send_plot("mulplt.eps",10)
c          endif

c                                                                               
c    Close down all files...
c
      call sei close( close$+all$,               ! Close all files.
     &                0,                         ! Unit (n/a).
     &                code )                     ! Condition (n/a).
      
c udp to se
      udptext='EXIT'
      if (port.gt.0) call send_udp_msg(port,udptext) 
      stop                                                                      
      end                                                                       
                                                                                
c------------------------------------------------------------------------------ 
       subroutine channelselect(number_of_channels,station,comp,                 
     *           nchan,nchan_def,stat_def,comp_def,
     *           default,itext)                               
c                                                                               
c                                                                               
c     Routine to show default channels and select channels to plot.             
c                                                                               
c     written by  j. havskov                                     
c                                                                               
c     Input:   number_of_channels   : in input file                             
c              station, comp        : station and component in input file       
c              stat_def,comp_def    : default channels
c              nchan_def            : number of default channels
c              default              : if default or not for selection, if true, no
c                                     channel selection box comes up
c              itext                : type for text on first line, see program                                
c     Input/Output:  channelname    : channel numbers selected, now in common                  
c                    nchan          : number of channels selected               
c
      implicit none
      include 'mulplt.inc'
c
c------------------------------------------------------
      include 'libsei.inc'              ! Library definitions & data defns.
      external   sei integer            ! Extract integer from string.
      integer    sei integer            ! & function.
      integer    code                   ! Condition code.
c---------------- end of sei lib changes --------------
c-- channels selected                             
c     integer channelname(max_channel)			
c-- number of channels in file                  
      integer number_of_channels
      integer nchan_def		
c-- station and components                
      character*5 station(*)
      character*4 comp(*)		
      character*5 stat_def(*)
      character*4 comp_def(*)
      character*2 answer   
      integer     itext  
      character*1 last_choise  ! in box input
c--- next 3 for sorting
      character*80 cha_sort_in(max_trace)
      character*80 cha_sort_out(max_trace)
      integer order_out(max_trace)                                                   
c-- selected array                               
      character*1 selected(max_trace)			
      integer on(max_trace)
      integer on_temp(max_trace)
c-- output text                                     
      character*75 text(12)
      character*80 box_text(max_trace)
      character*120 title
      character*80 box_text_temp(max_trace)
      character*1  ph(max_trace)  ! mark if a phase present
c-- number of ch. selected                                 
      integer nchan	
      integer npicked		! WCC
c-- last character pushed in input_box
      character*1 last_char
c-- number of lines print.                             
      integer lines_out				
      character*2 comp_short   ! short version of component notation
      integer nchan_in_box     ! number of channels used for selkection if more than 250
      logical default
c-- Counters                                               
      integer i,j,k,box,l
      common/box_input/last_choise    
      box=1

c
c   initialize channel names
c
      do i=1,max_trace
         channelname(i)=0
      enddo	
	  
      do i=1,number_of_channels
         ph(i)=' '
      enddo
c
c   sort channels 
c
      do i=1,number_of_channels
          cha_sort_in(i)(1:10)=station(i)//'A '//comp(i)
c
c   get components in right order
c
          if(comp(i)(4:4).eq.'Z') cha_sort_in(i)(7:7)='1'
          if(comp(i)(1:1).eq.'A') cha_sort_in(i)(6:6)='B'     ! acceleration channels at end
          if(comp(i)(4:4).eq.'N') cha_sort_in(i)(7:7)='2'
          if(comp(i)(4:4).eq.'E') cha_sort_in(i)(7:7)='3'
          if(comp(i)(4:4).eq.'1') cha_sort_in(i)(7:7)='4'
          if(comp(i)(4:4).eq.'2') cha_sort_in(i)(7:7)='5'
          if(comp(i)(4:4).eq.'3') cha_sort_in(i)(7:7)='6'
          if(comp(i)(4:4).eq.'R') cha_sort_in(i)(7:7)='7'
          if(comp(i)(4:4).eq.'T') cha_sort_in(i)(7:7)='8'
      enddo
      call  text_sort(cha_sort_in,cha_sort_out,order_out,
     *                number_of_channels)        

c                                                                               
c   find which default channels are present, initially selected if the
c   default option is used                                     
c                                                                               
c      do i=1,number_of_channels
c         k=order_out(i)
c         write(27,*) 
c     *   i,station(i),comp(i),order_out(i),station(k),comp(k)
c      enddo

      k=0
      npicked=0
      do i=1,number_of_channels  
                                                      
         do j=1,nchan_def                                                       
c            write(27,*) station(i),stat_def(j),comp(i),comp_def(j)
            l=i
            if(chan_sort) l=order_out(i)
            if(station(l).eq.stat_def(j).and.comp(l).eq.
     *         comp_def(j)) then                
c               write(27,*) k, selected,order_out(i)
               k=k+1                                                            
               channelname(k)=l
            endif                                                               
         enddo                                                                 


c
c   find if any reading this channel
c
         if(nrecord.gt.0) then
          do j=nhead+1,nrecord-1
            call component(comp(i),comp_short) ! get short version of component
            if(station(i).eq.data(j)(2:6)
     *      .and.comp_short.eq.data(j)(7:8))  then
            	ph(i)='*'
            	npicked=npicked+1	! WCC
            elseif(station(i).eq.data(j)(2:6)
     *      .and.comp(i)(4:4).eq.data(j)(8:8))  then
            	ph(i)='*'
            	npicked=npicked+1	! WCC
            endif
          enddo
         endif
      enddo
                                                                     
      nchan = k
                                                                 
c
c      do i=1,nchan
c         write(6,*) i,channelname(i)
C     enddo
c
c   check if not too many channels
c
      if(nchan.gt.max_trace) then
         write(6,*)' Too many channels, will define ',max_trace
         nchan=max_trace ! look like it does not work, is it needed ?
       endif
c
c   if no default channels, select all
c
      if(nchan.eq.0) then
         do i=1,number_of_channels
            if(chan_sort) then
               channelname(i)=order_out(i)
            else
               channelname(i)=i
            endif
         enddo
         nchan=number_of_channels
      endif
c
c   optionally deselect all channels too far
c
c      call mulplt_dist(nchan,station)      	  
	  
c
c   return if default
c
      if(default) return

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                               
c  mark channels already selected                                               
c                                                                               
      do i=1,number_of_channels
        on(i)=0
      enddo
      do i=1,number_of_channels                                                 
        on(i)=0                                                     
        do k=1,nchan                                                            
           if(chan_sort.and.channelname(k).eq.order_out(i))                            
     *       on(i)=1
           if(.not.chan_sort.and.channelname(k).eq.i)
     *       on(i)=1
        enddo                                                                   
      enddo
c
c
c  put in text for boxes
c
      do k=1,number_of_channels
            box_text(k)=' '
            if(chan_sort) then
                i=order_out(k)
            else
                i=k
            endif
            box_text(k)=station(i)//comp(i)//ph(i)//'  '
            channelname(i)=0
      enddo
c
c   display channels
c
         title=' '
         if(itext.eq.1) title(1:64)=
     *   'FILLED OUT BOXES INDICATE SELECTION,'//
     *   ' A * INDICATES A PHASE READ '
         if(itext.eq.2) title(1:57)=
     *   'DEFINED ARC CHANNELS, '//
     *   'FILLED OUT BOXES INDICATE SELECTION'

c
c   less than 250 channels
c
      if(number_of_channels.le.250) then
         box_text(number_of_channels+1)='All Z'
c
c  jh an 2013: always show picked box. also when no phases
c

cjh         if (npicked.ge.1) then							!WCC
         	box_text(number_of_channels+2)='Picked'	!WCC
                call input_box
     *   (number_of_channels+2,on,10,90.0,15.0,box_text,10,
     *                     3.0,685.0,title,last_char,1,0)
cjh        else											!WCC
cjh     	call input_box
cjh  *   (number_of_channels+1,on,10,90.0,15.0,box_text,10,
cjh  *                     3.0,685.0,title,last_char,1,0)
cjh  	 endif											!WCC
       endif

c
c   Quit...
c
      if(last_char.eq.'Q'.or.last_char.eq.'q') then
         nchan=0
         return
      end if   


c
c   250 to 500 channels
c
      if(number_of_channels.gt.250) then

         do i=1,number_of_channels
           on_temp(i)=on(i)
           box_text_temp(i)=box_text(i)
         enddo
c
c  show 1-250
c
         title(73:120)=
     *   'MORE CHANS: OK OR f, all chans in all windows: F'

         nchan_in_box=250
         box_text(nchan_in_box+1)='All Z'
         do i=1,nchan_in_box
            on_temp(i)=on(i)
         enddo
         k=0
cjh         if (npicked.ge.1) then
             box_text(nchan_in_box+2)='Picked' !WCC
             k=1
cjh         endif

         call input_box(nchan_in_box+1+k,on_temp,10,90.0,15.0,
     *   box_text,10,
     *                     3.0,710.0,title,last_char,1,0)

c   Quit...
c
         if(last_char.eq.'Q'.or.last_char.eq.'q') then
            nchan=0
            return
         end if   
c
c   save choises
c
         do i=1,250
             on(i)=on_temp(i)
         enddo
c
         if(last_char.eq.'F') then
c
c   select Z for rest of channesl
c
            if(last_choise.eq.'Z') then
               do k=251,number_of_channels
                  on(k)=0
                  if(chan_sort) then  ! might be sorted
                     i=order_out(k)
                  else
                     i=k
                  endif
                  if(comp(i)(4:4).eq.'Z') on(k)=1
               enddo
            endif
c
c   select phases read for rest of channels
c
            if(last_choise.eq.'P') then
               do k=251,number_of_channels
                  on(k)=0
                  if(chan_sort) then  ! might be sorted
                     i=order_out(k)
                  else
                     i=k
                  endif
                  if(ph(i).eq.'*') on(k)=1
               enddo
            endif
            goto 1   ! skip next windows
         endif

      endif
c
c   show 251-500
c
      if(number_of_channels.gt.250) then 
         if(number_of_channels.gt.500) then
             title(85:111)='MORE CHANNELS, PUSH OK OR F'
             nchan_in_box=250
         else
             nchan_in_box=number_of_channels-250
             title(85:111)=' '
         endif  
         do i=1,nchan_in_box
            on_temp(i)=on_temp(i+250)
            box_text(i)=box_text_temp(i+250)   ! use original text
         enddo

         box_text(nchan_in_box+1)='All Z'
         k=0
cjh         if (npicked.ge.1) then                                                 
             box_text(nchan_in_box+2)='Picked' !WCC
            k=1
cjh         endif

         call input_box
     *   (nchan_in_box+1+k,on_temp,10,90.0,15.0,box_text,10,
     *                     3.0,710.0,title,last_char,1,0)
c   Quit...
c
         if(last_char.eq.'Q'.or.last_char.eq.'q') then
            nchan=0
            return
         endif

c
c   save choises
c
         do i=1,nchan_in_box
             on(i+250)=on_temp(i)
         enddo         
      endif
c
c   show 501-750
c

      if(number_of_channels.gt.500) then
         if(number_of_channels.gt.750) then
             title(85:111)='MORE CHANNELS, PUSH OK OR F'
             nchan_in_box=250
         else
             nchan_in_box=number_of_channels-500
             title(85:111)=' '
         endif
         do i=1,nchan_in_box
            on_temp(i)=on_temp(i+500)
            box_text(i)=box_text_temp(i+500)   ! use original text
         enddo
         box_text(nchan_in_box+1)='All Z'
         k=0
cjh         if (npicked.ge.1) then                                                 
             box_text(nchan_in_box+2)='Picked' !WCC
             k=1
cjh         endif

         call input_box
     *   (nchan_in_box+1+1,on_temp,10,90.0,15.0,box_text,10,
     *                     3.0,710.0,title,last_char,1,0)
c   Quit...
c
         if(last_char.eq.'Q'.or.last_char.eq.'q') then
            nchan=0
            return
         endif

c
c   save choises
c
         do i=1,nchan_in_box
            on(i+500)=on_temp(i)
         enddo
      endif
c
c   show 750-1000
c
      if(number_of_channels.gt.750) then
         if(number_of_channels.gt.1000) then
             write(6,*)' Number of channels too large'
             stop
         else
             nchan_in_box=number_of_channels-750
             title(85:111)=' '
         endif
         do i=1,nchan_in_box
            on_temp(i)=on_temp(i+750)
            box_text(i)=box_text_temp(i+750)   ! use original text
         enddo

         box_text(nchan_in_box+1)='All Z'
         k=0
cjh         if (npicked.ge.1) then                                                 
             box_text(nchan_in_box+2)='Picked' !WCC
             k=1
cjh         endif

         call input_box
     *   (nchan_in_box+1+k,on_temp,10,90.0,15.0,box_text,10,
     *                     3.0,710.0,title,last_char,1,0)
c   Quit...
c
         if(last_char.eq.'Q'.or.last_char.eq.'q') then
            nchan=0
            return
         endif

c
c   save choises
c
         do i=1,nchan_in_box
             on(i+750)=on_temp(i)
         enddo
      endif
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
 1    continue
c
c   take out selected channels
c
      k=1
      do i=1,number_of_channels
         if(on(i).eq.1) then
            if(chan_sort) then
               channelname(k)=order_out(i)   
            else
               channelname(k)=i
            endif
            k=k+1
         endif
      enddo

      nchan=k-1                                                         
                                                                 
      return                                                                    
      end                                                                       
******************************************************************              

      subroutine channel_order(station,                 
     *           delay,nchan,nsynt)                               
c                                                                               
c                                                                               
c     Routine to order channels in aproximate distance              
c                                                                               
c     written j. havskov, oct 95                                     
c                                                                               
c     Input:                                
c              station              : station       
c              delay                : delay each trace relative to main header
c              nsynt                : number of synthetic readings
c
c     Input/Output:  channelname    : channel numbers reordered,now in common                  
c                    nchan          : number of channels selected               
c     
c
      Implicit none
      include 'mulplt.inc'
c
c-- channels selected                             
c     integer channelname(*)			
      real delay(*)                      ! channel delay
      integer new_channelname(max_trace) ! for reordering channels
      logical not_used(max_trace)        ! if channel reordered or not
      integer min             ! channel number of channel with minumum delay
      real mindel             ! minimum delay
      integer nsynt
c-- station                 
      character*5 station(*)
c-- number of ch. selected                                 
      integer nchan,nrecord2,nhead2,nphase2
c-- Counters                                               
      integer i,j,k,l
      character*80 data2(max_trace)
      real elat,elon,slat,slon,sz
      real degtorad,dist,distdeg,az
      parameter (degtorad=0.0174533)
      logical ldum                    ! logical dummy
      integer yr1,mo1,da1,hr1,mi1     ! time and date
      real se1
      double precision msec1          ! for time conversion
      double precision firstpick
      character*5 firststat

      firstpick=99999999999.9
      ldum=.FALSE.
	  
c
c  check if stations are to be plotted in distance order, nsort_distance must
c  be positive
c
 
c     write(6,*)nsort_distance,nrecord,nhead
      if(nsort_distance.eq.0) return
      do i=1, nchan
        not_used(i)=.true.          ! no channels are ordered initially
      enddo

      nhead2=nhead
      do i=1,nhead2
        data2(i)=data(i)
      enddo
      nphase2=0
      read(data2(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *yr1,mo1,da1,hr1,mi1,se1
      call TIMSEC (yr1,mo1,da1,hr1,mi1,se1,msec1)
c check if event is located:
      if(data(1)(24:38).NE."               ")then
        read(data(1)(24:30),'(f7.3)') elat
        read(data(1)(31:38),'(f8.3)') elon
      else
c use the first phase pick here :
c       write(*,*) ' pv1: nrecord:',nrecord
        do i=1,nrecord
c         write(*,*) ' pv1: ',i,data(i)
          if(ldum.AND.data(i)(2:16).EQ."               ") ldum=.FALSE.
          if(ldum)then
            read(data(i)(19:20),'(i2)') hr1
            read(data(i)(21:22),'(i2)') mi1
            read(data(i)(23:28),'(f6.3)') se1
            call TIMSEC (yr1,mo1,da1,hr1,mi1,se1,msec1)
            if(msec1.lt.firstpick) then
              firstpick=msec1
              read(data(i)(2:6),'(a5)') firststat
            endif
          endif
          if(data2(i)(2:16).EQ."STAT SP IPHASW ") ldum=.TRUE.
        enddo
c       write(*,*) ' pv1: firststat:',firststat
        call stat_loc(firststat,
     &  data(1)(21:21),slat,slon,sz)
c       write(*,*) ' pv1: slat:',slat,', slon:',slon
        elat=slat
        elon=slon
      endif
      elat=elat*degtorad
      elon=elon*degtorad
c else exit
      if(firstpick.gt.99999999999.0)then
        write(*,*) ' no event location or phase picks '
        return
      endif

         do i=1,nchan
c
c get station and event coordinates
c          
           call stat_loc(station(channelname(i)),
     &       data(1)(21:21),slat,slon,sz)
           if (slat.ne.0..and.slon.ne.0.) then
c compute distances
             slat=slat*degtorad
             slon=slon*degtorad
             call delaz(elat,elon,dist,distdeg,az,slat,slon)

             write(data2(i+nhead2),'(1x,a5,74x)') 
     &          station(channelname(i))
             if (dist.lt.10.) then
               write(data2(i+nhead2)(71:75),'(f5.2)') dist
             elseif (dist.lt.1000.) then
               write(data2(i+nhead2)(71:75),'(f5.1)') dist
             else
               write(data2(i+nhead2)(71:75),'(i5)') int(dist)
             endif
c             write(*,*) data2(i+nhead2)
             nphase2=nphase2+1
           else
             write(6,'(a,a)') ' Station not in STATION.HYP file: ',
     &       station(channelname(i))
             write(data2(i+nhead2),'(1x,a5,74x)') 
     &       station(channelname(i))
             dist=20037.5
              write(data2(i+nhead2)(71:75),'(i5)') int(dist)
             nphase2=nphase2+1
           endif
         enddo
         nrecord2=nhead2+nphase2+1
         data2(nrecord2)=' '

c
c   sort data in data array from s-file, jh oct 2010, thus no longer possible
c   to plot in actual order of s-file
c

c         call sort_event(data,nhead,nrecord)
         call sort_event(data2,nhead2,nrecord2)
c         do i=nhead2+1,nrecord2-1
c           write(*,*) data2(i)(1:80)
c         enddo

c
c  plot in order as given in data array, if a station is not in s-file
c  or distance is blank, put at end
c
c         write(6,*) nrecord2, nhead2,nsynt,nsort_distance
c         if((nrecord2-nhead2-nsynt).gt.nsort_distance) then ! only do if enough readings
         if((nphase2).gt.1) then ! lo

c         write(6,*)'phas'
            k=0                         ! count channels reordered
            do j=nhead2+1,nrecord2-1      ! put in same order as in s-file
               do i=1,nchan             ! check every prev. selected channel
c                  if(station(channelname(i)).eq.data(j)(2:6).
c     *            and.not_used(i).and.data(j)(72:75).ne.'    ') then
                  if(station(channelname(i)).eq.data2(j)(2:6).
     *            and.not_used(i).and.data2(j)(72:75).ne.'    ') then
                     k=k+1
                     not_used(i)=.false.     ! channel now ordered
                     new_channelname(k)=channelname(i)
                  endif
               enddo
            enddo
            do i=1,nchan                 ! put channels not ordered at the end
               if(not_used(i)) then
                  k=k+1
                  new_channelname(k)=channelname(i)
               endif
            enddo
            do i=1,nchan                 ! put channel order back in org. array
               channelname(i)=new_channelname(i)
            enddo
         else                           
c
c   use waveform file header time order
c
c            write(6,*)'time order'
             i=1
 10             continue
                mindel=9999999.0      ! initialize minumum delay
                   do l=1,nchan          ! find minimum delay of remaining stats.
c                      if(delay(channelname(l)).lt.mindel.and.
                      if(delay(channelname(l)).lt.mindel.and.
     *                not_used(l))  then 
                         mindel=delay(channelname(l))
                         min=l
                      endif
                   enddo
                   if (min.eq.0) then
                     write(*,*) ' something wrong in sorting ' ! lo
                     stop
                   endif
                   not_used(min)=.false.     ! channel now ordered
                   new_channelname(i)=channelname(min)
                   k=0                      ! number of channels, same station
                   do l=1,nchan             ! make sure remaining channels
                      if(station(channelname(l)).eq.   ! - of same station are
     *                station(channelname(min)).and.not_used(l)) then 
                         not_used(l)=.false.      ! - selected together
                         i=i+1
                         new_channelname(i)=channelname(l)
                      endif                   
                   enddo
               i=i+1
               if(i.le.nchan) goto 10
            do i=1,nchan                 ! put channel order back in org. array
               channelname(i)=new_channelname(i)
            enddo
         endif
                         
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine get_mulplt_def(def_unit,stat_def
     *,comp_def,nchan_def)               
c                                                                               
C     get mulplt defaults                                            
c                                                                               
c     written by  J. Havskov                                     
c                                                                               
c     Input:   def_unit  : unit to read from (changed to output..JAB(BGS)Dec94)
c              data      : work array                                               
c     Output:  defaults, through window, default channels to plot,
c                        the rest through common block                                                      
c                  
      implicit none
      external sei get file,                 ! Search directories & open file.
     &         sei close,                    ! Close file open.
     &         sei code                      ! Error processor.
      include 'libsei.inc'                   ! Library definitions & data defns
      integer  code                          ! Condition.
      logical  b_eof                         ! End of file?.
      integer seiclen
c
      include 'mulplt.inc'
      include 'seiplot.inc'
      character*5 stat_def(*)
      character*4 comp_def(*)			
c-- unit for file
      integer def_unit
c-- directory separator char
      character*1 dchar
c-- path to seismo                             
      character*60 top_directory			
c-- number of default channels                                
      integer nchan_def
c-- Counters and help variables
      character*1 amouse            
      real xmouse,x_screen_size,xres,xsort_distance,ca,xin
      real fix_filter   ! indicate if a fixed filter
                                        
      integer i,k,nline				
      logical sun,pc,linux
c
      call dir_char(dchar)
      call topdir(top_directory)
      call computer_type(sun,pc,linux)
c
c   set default parameters in case there is no mulplt.def
c
c-- mouse is blank                                               
c
      if(sun.or.linux) mouse=ichar(' ')
      if(pc) mouse=ichar(' ')
      resolu_x=1000
      resolu_hc=3000
      nchan_def=0
      numb_keys=0
      numb_weights=0
      q0=0.0
      kappa=0
      qalpha=1.0
      travel_time=1.0
      pvelocity=6.0
      svelocity=3.5
      density=3.0
      three_comp_velocity=5.0
      nsort_distance=0
      ffmin=0.05
      ffmax=50.0
      auto_locate=0.0
      auto_update=0.0
      auto_process=0.0
      auto_process_name=' '
      spec_out=0.0
      filt=0
      coda_h=10.0
      coda_l=5.0
      coda_sta=5.0
      coda_ratio=1.5
      do_auto_coda=.FALSE.
      new_line_all=.FALSE.
      ms_low_pole=0
      ms_high_pole=0
      mb_low_pole=0
      mb_high_pole=0
      ml_high_pole=0
      ml_low_pole=0
      spectral_model=0.0
      mulplt_multi_label=1.0
      plot_distance=0.0
c
c   others
c
      n_chan_screen=99 ! max 99 channels per screen
      chan_sort=.true. ! sort alphabetically
      mulplt_area=0    ! defualt no area selection
      mulplt_lat=0.0
      mulplt_lon=0.0
      mulplt_radius=180.0
      mulplt_stat=' '
      mulplt_wav_out_format='SEISAN    '
      spectrogram_command='spectrogram '
      spectrogram_window=80.0
      spectrogram_filter=2.0
      spectrogram_plot_trace=1.0

c
c Set time domain recursive filter type to 0 (use function bndpas bjb 2001/02/14)
c
      filter_type=0
      fix_filter=0
c                                                                               
c   Initially set mulplt filters (bjb 2001/02/13)
c                                                                               
      flow(1)=0.01                                                             
      fhigh(1)=.1
      flow(2)=.1                                                              
      fhigh(2)=1.0
      flow(3)=1.0                                                               
      fhigh(3)=5.0                                                             
      flow(4)=5.0                                                              
      fhigh(4)=10.0                                                             
      flow(5)=10.0                                                              
      fhigh(5)=15.0                                                             
      flow(6)=15.0                                                              
      fhigh(6)=23.0                                                             
      flow(7)=2.0
      fhigh(7)=4.0
c                                                                               
c   get path to seismo                                                          
c                                                                               
      k=index(top_directory,' ')-1                                              
c                                                                               
c   open and read default file with stations and components to use              
c   --------------------------------------------------------------
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   def_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'MULPLT.DEF' )    ! For this file.
c                                                                               
c   read file if there...
c   ------------
c                                                                               
      if(code.ne.e_ok$)  goto 666

      i=1                                                                       
10    read(def_unit,'(a)',iostat=code) data(i) ! Read from file.
      call sei code( fort$,                    ! Process fortran i/o condition.
     &               code,                     ! Condition.
     &               def_unit,                 ! On unit.
     &               b_eof )                   ! End of file?.
c
      if( b_eof ) then                         ! End of file.
      nline=i                                  ! Store records in file.
      call sei close( close$, def_unit, code ) ! Close (Default stop on error).
c
      else                                     ! Otherwise.
      i=i+1                                    ! Increment record number.
      goto 10                                  ! Read another record.
      end if                                   !
c
c   find default chanels
c
      do i=1,nline
         if(data(i)(1:15).eq.'DEFAULT CHANNEL'.and.
     *      data(i)(41:50).ne.'          ') then
            nchan_def=nchan_def+1
            stat_def(nchan_def)=data(i)(41:45)
            comp_def(nchan_def)=data(i)(51:54)
         endif
      enddo		 
c
c   find keys assigned to phases
c
      do i=1,nline
         if(data(i)(1:14).eq.'PHASE NAME KEY'.and.
     *      data(i)(41:50).ne.'          ') then
            numb_keys=numb_keys+1
            ascip(numb_keys)=data(i)(41:41)
            phs(numb_keys)=data(i)(51:59)
         endif
c
c  weight keys assigned
c
         if(data(i)(1:16).eq.'PHASE WEIGHT KEY'.and.
     *      data(i)(41:50).ne.'          ') then
            numb_weights=numb_weights+1
            if(numb_weights.gt.10) then
               write(6,*)' You have defined too many weights'
               write(6,*)' Reduce to 10 or less in MULPLT.DEF'
               stop
            endif
c           if(ICHAR(data(i)(41:43)).GE.48.AND.ICHAR(data(i)(41:43)).LE.57) then
c             key_weight(numb_weights)=CHAR(data(41:43))
c             write(key_weight(numb_weights),'(i3)') data(41:43)
            if(data(i)(41:43).EQ."164") then
              key_weight(numb_weights)=CHAR(164)
            else
              key_weight(numb_weights)=data(i)(41:41)
            endif
            i_weight(numb_weights)=data(i)(51:51)
         endif
      enddo
c
c   find if a mouse definition, can be a character or ascii code
c
      do i=1,nline
         if(data(i)(1:15).eq.'PHASE MOUSE KEY'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)', err=273 ) xmouse
            mouse=xmouse
            goto 274
 273        continue
c
c   was not a number, try a character
c
            amouse=data(i)(41:41)
            mouse=ichar(amouse)
 274        continue
         endif
      enddo
c
c   find and set spectral and other parameters
c
      do i=1,nline
         if(data(i)(1:11).eq.'SPECTRAL Q0'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) q0
         endif
         if(data(i)(1:15).eq.'SPECTRAL QALPHA'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) qalpha
         endif
         if(data(i)(1:14).eq.'SPECTRAL KAPPA'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99)kappa
         endif
         if(data(i)(1:19).eq.'SPECTRAL S-VELOCITY'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) svelocity
         endif
         if(data(i)(1:19).eq.'SPECTRAL P-VELOCITY'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) pvelocity
         endif
         if(data(i)(1:16).eq.'SPECTRAL DENSITY'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) density
         endif
         if(data(i)(1:14).eq.'SPECTRAL MODEL'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) spectral_model
         endif
         if(data(i)(1:15).eq.'SPECTRAL F-BAND'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:60),'(2f10.1)',err=99) ffmin,ffmax
         endif
         if(data(i)(1:15).eq.'BANDPASS FILTER'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:60),'(2f10.1)',err=99) flow_def,fhigh_def
            filt=9
         endif
         if(data(i)(1:10).eq.'FIX FILTER'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:60),'(f10.1)',err=99) fix_filter
            filt=9
         endif
         if(data(i)(1:15).eq.'SPECTRAL OUTPUT'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:60),'(f10.1)',err=99) spec_out
         endif


         if(data(i)(1:21).eq.'ML HIGH CUT AND POLES'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:60),'(2f10.1)',err=99) ml_high,xin
            ml_high_pole=xin
c  haredwired for now, may 31 14
            ml_high_pole=4
c            if(ml_high_pole.eq.0) ml_high_pole=8
         endif

c         if(data(i)(1:21).eq.'MS HIGH CUT AND POLES'.and.
c     *      data(i)(41:50).ne.'          ') then
c            read(data(i)(41:60),'(2f10.1)',err=99) ms_high,xin
c            ms_high_pole=xin
c            if(ms_high_pole.eq.0) ms_high_pole=8
c         endif

         if(data(i)(1:20).eq.'ML LOW CUT AND POLES'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:60),'(2f10.1)',err=99) ml_low,xin
            ml_low_pole=xin
c  hardwirded for now, may 14
            ml_low_pole=4
c            if(ml_low_pole.eq.0) ml_low_pole=8
         endif


c         if(data(i)(1:20).eq.'MS LOW CUT AND POLES'.and.
c     *      data(i)(41:50).ne.'          ') then
c            read(data(i)(41:60),'(2f10.1)',err=99) ms_low,xin
c            ms_low_pole=xin
c            if(ms_low_pole.eq.0) ms_low_pole=8
c         endif

c         if(data(i)(1:21).eq.'MB HIGH CUT AND POLES'.and.
c     *      data(i)(41:50).ne.'          ') then
c            read(data(i)(41:60),'(2f10.1)',err=99) mb_high,xin
c            mb_high_pole=xin
c            if(mb_high_pole.eq.0) mb_high_pole=8
c         endif

c         if(data(i)(1:20).eq.'MB LOW CUT AND POLES'.and.
c     *      data(i)(41:50).ne.'          ') then
c            read(data(i)(41:60),'(2f10.1)',err=99) mb_low,xin
c            mb_low_pole=xin
c            if(mb_low_pole.eq.0) mb_low_pole=8
c         endif
c
c   velocity for 3c analysis
c
         if(data(i)(1:14).eq.'3COMP VELOCITY'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) three_comp_velocity
         endif
c
c   autoprocessing and location
c
         if(data(i)(1:11).eq.'AUTO_LOCATE'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99)auto_locate 
            read(data(i)(51:60),'(f10.1)',err=99)auto_update 
         endif


         if(data(i)(1:12).eq.'AUTO_PROCESS'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) auto_process
            auto_process_name(1:10)=data(i)(51:60)
         endif

c
c Read filter type parameter from MULPLT.DEF (bjb 2001/02/14)
c
         if(data(i)(1:11).eq.'FILTER TYPE'.and.
     *      data(i)(41:50).ne.'          ') then

            read(data(i)(41:50),'(f10.1)') filter_type
         endif

c
c plot of distances, 0 no, 1 epicentral, 2 hypocentral
c
         if(data(i)(1:13).eq.'PLOT DISTANCE'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') plot_distance
         endif


c
c Read filter parameters for up to 7 filters from MULPLT.DEF (bjb 2001/02/13)
c
         if(data(i)(1:8).eq.'FILTER 1'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') flow(1)
            read(data(i)(51:60),'(f10.1)') fhigh(1)
         endif

         if(data(i)(1:8).eq.'FILTER 2'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') flow(2)
            read(data(i)(51:60),'(f10.1)') fhigh(2)
         endif

         if(data(i)(1:8).eq.'FILTER 3'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') flow(3)
            read(data(i)(51:60),'(f10.1)') fhigh(3)
         endif

         if(data(i)(1:8).eq.'FILTER 4'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') flow(4)
            read(data(i)(51:60),'(f10.1)') fhigh(4)
         endif

         if(data(i)(1:8).eq.'FILTER 5'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') flow(5)
            read(data(i)(51:60),'(f10.1)') fhigh(5)
         endif

         if(data(i)(1:8).eq.'FILTER 6'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') flow(6)
            read(data(i)(51:60),'(f10.1)') fhigh(6)
         endif

         if(data(i)(1:8).eq.'FILTER 7'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') flow(7)
            read(data(i)(51:60),'(f10.1)') fhigh(7)
         endif

         if(data(i)(1:11).eq.'MULPLT AREA'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') mulplt_area
         endif

         if(data(i)(1:14).eq.'MULPLT LAT LON'.and.
     *      data(i)(41:50).ne.'          '
     *     .and.data(i)(51:60).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') mulplt_lat
            read(data(i)(51:60),'(f10.1)') mulplt_lon
         endif

         if(data(i)(1:13).eq.'MULPLT RADIUS'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') mulplt_radius
         endif

         if(data(i)(1:11).eq.'MULPLT STAT'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(a5)') mulplt_stat
         endif

         if(data(i)(1:21).eq.'MULPLT WAV OUT FORMAT'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(a10)') mulplt_wav_out_format
         endif

         if(data(i)(1:19).eq.'SPECTROGRAM COMMAND'.and.
     *      data(i)(41:50).ne.'          ') then
            spectrogram_command=data(i)(41:seiclen(data(i)))
         endif

         if(data(i)(1:18).eq.'SPECTROGRAM WINDOW'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),*) spectrogram_window
         endif

         if(data(i)(1:18).eq.'SPECTROGRAM FILTER'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),*) spectrogram_filter
         endif

         if(data(i)(1:22).eq.'SPECTROGRAM PLOT TRACE'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),*) spectrogram_plot_trace
         endif

         if(data(i)(1:19).eq.'SPECTROGRAM PLOT TRACE'.and.
     *      data(i)(41:50).ne.'          ') then
            spectrogram_command=data(i)(41:seiclen(data(i)))
         endif

c
c   show OP and last action label in MULPLT Multi trace
c
         if(data(i)(1:18).eq.'MULPLT_MULTI_LABEL'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)') mulplt_multi_label
         endif
c
c   number of phases to do sorting, or no sortingó
c
         if(data(i)(1:14).eq.'NSORT_DISTANCE'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99)xsort_distance 
            nsort_distance=xsort_distance
         endif
c
c   x screen size
c
         if(data(i)(1:13).eq.'X_SCREEN_SIZE'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99) x_screen_size
            wsize=x_screen_size
         endif                      
c
c  resolution
c
         if(data(i)(1:11).eq.'RESOLUTIONX'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99)xres
            resolu_x=xres
         endif
         if(data(i)(1:12).eq.'RESOLUTIONHC'.and.
     *      data(i)(41:50).ne.'          ') then
            read(data(i)(41:50),'(f10.1)',err=99)xres
            resolu_hc=xres
         endif
c
c parameters for automatic coda
c
         if(data(i)(1:15).eq.'AUTOCODA FILTER') then
           if (data(i)(41:50).ne.'          ') then
              read(data(i)(41:50),'(f10.1)',err=99) coda_l
           endif
           if (data(i)(51:60).ne.'          ') then
              read(data(i)(51:60),'(f10.1)',err=99) coda_h
           endif
         endif

         if(data(i)(1:12).eq.'AUTOCODA STA'.and.
     *       data(i)(41:50).ne.'          ') then
             read(data(i)(41:50),'(f10.1)',err=99) coda_sta
         endif

         if(data(i)(1:14).eq.'AUTOCODA RATIO'.and.
     *       data(i)(41:50).ne.'          ') then
             read(data(i)(41:50),'(f10.1)',err=99) coda_ratio
         endif

         if(data(i)(1:9).eq.'CODA AUTO') then
             read(data(i)(41:50),'(f10.1)',err=99)ca 
               k = int(ca)
             if (k.eq.0) do_auto_coda=.FALSE.
             if (k.eq.1) do_auto_coda=.TRUE.
         endif
c
c  number of traces to one screen
c
         if(data(i)(1:16).eq.'NCHAN PER SCREEN') then
             read(data(i)(41:50),'(f10.1)',err=99)ca 
               k = int(ca)
             if (k.gt.0) n_chan_screen=k
         endif
         n_chan_screen_org=n_chan_screen

        if(data(i)(1:15).eq.'CHANNEL SORTING') then
             read(data(i)(41:50),'(f10.1)',err=99)ca 
               k = int(ca)
             if (k.gt.0) chan_sort=.true.
             if (k.eq.0) chan_sort=.false.
         endif

c pv: put the All Z, ALL, OK and NONE in a new line
         if(data(i)(1:12).eq.'NEW LINE ALL') then
             read(data(i)(41:50),'(f10.1)',err=99)ca 
               k = int(ca)
             if (k.eq.0) new_line_all=.FALSE.
             if (k.eq.1) new_line_all=.TRUE.
         endif
c


      enddo
c
c   clear phase keys not used
c
      if (numb_keys.lt.12) then
        do i=numb_keys+1,12
          ascip(i)=' '
          phs(i)=' '
        enddo
      endif
c
c 
  666   continue
c
c  set fixed filter of a filter is also set 
c
        if(filt.eq.9.and.fix_filter.eq.1.0) filter_perm=.true.
c
c   if no phases were defined, do it here
c
       if(numb_keys.eq.0) then	
c-- IP	! - default                                         

c-- IP	! - default                                         
          ascip(1)='1'	
          phs(1)='IP       '                                                       
c-- EP                                                     
          ascip(2)='2'	
          phs(2)='EP       '                                                       
c-- EPG                                                    
          ascip(3)='3'	
          phs(3)='IPg      '                                                       
c-- EPG                                                    
          ascip(4)='4'	
          phs(4)='EPg      '                                                       
c-- IPN                                                    
          ascip(5)='5'	
          phs(5)='IPn      '                                                       
c-- EPN                                                    
          ascip(6)='6'	
          phs(6)='EPn      '	                                                      
c-- IS                                                     
          ascip(7)='7'	
          phs(7)='IS       '                                                       
c-- ES                                                     
          ascip(8)='8'	
          phs(8)='ES       '                                                       
c-- ISG                                                    
          ascip(9)='9'	
          phs(9)='ISg      '                                                       
c-- ESG                                                   
          ascip(10)='0'	
          phs(10)='ESg      '                                                      
c--  SN                                                   
          if(pc.or.linux)ascip(11)='+'	
          if(sun)ascip(11)='-'	
          phs(11)=' Sn      '                                                      
c-- ESN                                                   
c         ascip(13)='}'	
c         if(pc) ascip(13)=dchar
c         phs(13)='ESn      '                                                      
c-- LG                                                    
           if(sun) ascip(12)='='	
           if(pc.or.linux) ascip(12)=dchar
           phs(12)=' Lg      '                                                      
c         ascip(14)='l'	
c         phs(14)='Lg       '                                                      
c-- coda                                                  
          ascip(15)='c'	
          phs(15)='CODA     '                                                      
c-- coda                                                  
c
          ascip(16)='C'	
          phs(16)='CODA     '                                                      
c-- i     
          ascip(17)='i'
          phs(17)='I        '
c          ascip(18)='I'
c          phs(18)='I        '                                                      
c-- e     
          ascip(19)='e'
          phs(19)='E        '
c          ascip(20)='E'
c          phs(20)='E        '                                                      
c-- A
          ascip(21)='A'
          phs(21)=' AMP'
          numb_keys=21    

      endif
c                                                                               
c   define keys for weight if not done above                                  
c                                           
      if(numb_weights.eq.0) then                                 
c--	 upper case 1                                          
      key_weight(1)='!'
c-- upper case 2                                          
      key_weight(2)='@'
      if(pc) key_weight(2)='"'	
c-- upper case 3                                          
      key_weight(3)='#'	
c-- upper case 4                                          
      key_weight(4)='$'
c-- upper case 5                                          
      key_weight(5)='%'	
c-- upper case 6                                          
      key_weight(6)='^'	
      if(pc)key_weight(6)='&'
c-- upper case 7                                          
      key_weight(7)='&'	
      if(pc) key_weight(7)='/'
c-- upper case 8                                          
      key_weight(8)='*'   ! temporary fix on sun	
      if(pc) key_weight(8)='('
c-- upper case 9                                          
      key_weight(9)='('	
      if(pc) key_weight(9)=')'
c-- upper case 0                                          
      key_weight(10)=')'
      if(pc) key_weight(10)='='
c                                                                               
c  assign weights asci values                                                   
c                                                                               
      i_weight(1)='1'                                                           
      i_weight(2)='2'                                                           
      i_weight(3)='3'                                                           
      i_weight(4)='4'                                                           
      i_weight(5)='5'                                                           
      i_weight(6)='6'                                                           
      i_weight(7)='7'                                                           
      i_weight(8)='8'                                                           
      i_weight(9)='9'                                                           
      i_weight(10)=' '          
      endif                      

c
c  save value of nsort_distance, might be temporarely turned off
c
c3-07-jh      nsort_distance_save=nsort_distance                          
c
c    Return to Caller...
c    ===================
c
c   errror
c
      goto 9999
99    continue
      write(6,*)' Error in MULPLT.DEF'
      write(6,*) data(i)(1:79)
      write(6,*)' Enter to to continue'
      call flush (6)
      read(5,'(a)') i
      stop
c
9999  return
      end
c
c******************************************************************             
                                                                                
      subroutine new_s_file(year,month,day,hr,min,sec,type,                     
     *            sdir,trace_file,event_name,data,operator,
     *            sub_class_flag,nhead,nrecord)                    
c                                                                               
c     Makes an initial S-file name from trace data file data                    
c                                                                               
c      J. Havskov, December 1988                                                
c                                                                               
c  last update:                                                                 
c  jan 16, 89 by j.h. : change comment line from type 3 to 7                    
c  sep 15  89    j.h. : new directory structure adaption                        
c  mar 22  89         : remove [wave] from file name output                     
c  aug 14  90         : pos+17 changed to pos+18 in blank string                
c  oct 01  91    j.h. : some unix clean up
c  nov 26  91         : new nordic format
c  jan 31  92    c.l. : Always write type on first header line
c  mar 18  97    bjb  : subclass information and number of headers
c                       and records are passed to subroutine
c                       a type 3 line is created if volcanic
c                       subclass info is desired.
c                       Phase readings are saved in the array DATA
c                       so these can be written out when event is 
c                       registered
c                                                                               
c     Input: Year,month,day,hr,min,sec: time of event                           
c            type: event type: 1. char L,R or D, 2. char can be different
c                       things like V, E or P                                   
c	     sub_class: for local volcanic events only. See 
C 			VOLCANO.DEF in DAT dir                                        
c            sdir: dirctory for S-files, deafault (blank) is agency:...            
c            trace_file: trace file name                                        
c     Output:event_name: S - file name                                         
c            data: standard data readings array with one help line              
c                                                                               
c                                    
      implicit none
      integer year,month,day,hr,min                               
c--input-output                                
      character*80      event_name	
c--------------                                
      character*80      trace_file	
c--------------                                     
      character*40      sdir		
c--------------                                           
      character*2	type		
c--------------                                           
      character*6	sub_class_flag
c--------------                                     
      character*80  	data(*)	
      character*80      reading(500) ! for saving
      integer           nreading     ! number fo readings	
c-- seconds                                                      
      real		sec		
c-- seconds                                        
      integer		isec            
c-- disk and directory name lengths                       
      integer		l1	
c-- number of header lines in temp. s-file
      integer nhead
c-- number of additional header lines to be added
      integer nh
c-- number of record lines in temp. s-file including headers
      integer nrecord
c-- system time
      character*12 p_time
      character*14 proc_time
c-- id line
      character*80 idline
      integer iline           ! line counter
c-- operator
      character*4 operator
      character*5 agency
      integer 	i,k	
c-- volcano line
      character*80 volc_line
c-- counter
      integer seiclen
      integer x


      call get_def_base(agency)

c
c   check on default data base
c
      if(sdir(1:5).eq.'     ') sdir(1:5)=agency

      do x=2,5
        if (sdir(x:x).eq.' ') sdir(x:x)='_'  
      enddO
c                                                                               
c   make output file name                                                       
c                                                                               
      isec=sec
      call sfilname
     *(year,month,day,hr,min,isec,sdir(1:5),type(1:1),event_name,l1)
c
c  make sure nhead and nrecord have reasonable values
c
      if(nrecord.lt.2) then
         nrecord=3
         nhead=2
      endif
c
c   save readings
c
      k=0
      do i=nhead+1,nrecord
         k=k+1
         reading(k)=data(i)
      enddo
      nreading=k
c
c  shift possible readings
c
c     nh=2   ! changed lo
c      nh=1
c      if(type(2:2).eq.'V') nh=nh+1  ! one extra if new volcano line
c      if(trace_file.eq.' ') nh=nh-1 ! one less if no waveform line
c      do i=nrecord,nhead+1,-1
c        data(i+nh)=data(i)
c      enddo
c      nhead=nhead+nh
c      nrecord=nrecord+nh
c                                                                               
c   write header line in file always with type '1' at the end        
c                                                                               
      write(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1,1x,a2)')                      
     *year,month,day,hr,min,sec,type                                            
      write(data(1)(80:80),'(a1)')'1'
c
c   next line is id line
c
      idline(1:40)= ' ACTION:                   OP:     STATU'
      idline(41:80)='S:               ID:                   I'
      WRITE(IDLINE(61:75),'(i4,5I2)')
     *YEAR,MONTH,DAY,HR,MIN,ISEC
      DO I=61,74
         IF(IDLINE(I:I).EQ.' ') IDLINE(I:I)='0'
      ENDDO
      call systime(p_time,proc_time)
      WRITE(IDLINE(31:34),'(A)')OPERATOR
      WRITE(IDLINE(13:26),'(A)')PROC_TIME
      WRITE(IDLINE(9:11),'(A)')'REG'
      data(2)=idline
      iline=2
c
c-- next line is trace data file name                   
c                                                                               
c  trace file name if not blank                                                 
c
      if(trace_file(1:10).ne.'          ') then
         iline=iline+1
c                                                                               
c-- blank whole string                        
         do i=1,80
           data(iline)(i:i)=' '
         enddo
         i=seiclen(trace_file)         ! jh oct 27 2011
         data(iline)(2:i+1)=trace_file(1:i)                                            
         data(iline)(80:80)='6'
      endif

c
c construct the volcano line if required
c
      if(type(2:2).eq.'V') then
        iline=iline+1 
        do i=1,80
          volc_line(i:i)=' '
        enddo
        volc_line(2:10)='VOLC MAIN'
        volc_line(12:17)=sub_class_flag
        volc_line(80:80)='3'
        data(iline)=volc_line
      endif
      
      iline=iline+1
      data(iline)(1:57)=
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO '               
      data(iline)(58:80)='AIN AR TRES W  DIS CAZ7'
c
c   add readings
c
       do i=1,nreading
         iline=iline+1
         data(iline)=reading(i)
       enddo
       nrecord=iline
c
c-- blank last line                                    
c
c      do i=1,80                                                                 
c         data(nrecord)(i:i)=' '		
c      enddo                                                                     
      return                                                                    
      end                  
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                     
        subroutine make_res_file(nrecord,data,nsynt)
c
c   update array data so residuals have been added to the travel times
c   and put at end of array as synthetics for residual plotting
c
        implicit none
        include 'seidim.inc'
        include 'libsei.inc'             !  Library definitions & data defns.
        character*80 data(*), data_out(max_data)
        character*90 text                ! from 80 to 90 for gfortran pc
        integer nrecord
        integer hour,min
		integer unit           ! file unit
        logical exist          ! true if file exist
        integer code           ! code for open
        integer nsynt          ! number of calculated phases
        real sec
        real res               ! residual
        real time              ! time in seconds
        integer i,k
c
c   read results from print.out file
c
      call sei open( old$+ignore$,            ! Try to open & warn.
     &               ' ',                     ! No prompt.
     &               'print.out',             ! This file.
     &               unit,                    ! On unit.
     &               exist,                   ! File exists?.
     &               code )                   ! Condition.
c
c    File exists and is open!!..
c
c   find start of event
c
  10    continue
        read(unit,'(a)',end=50) text
        if(text(2:4).eq.'stn') goto 20
        goto 10
  20    continue
c
c   read results
c
        k=1
  30    continue
        read(unit,'(a)') text
        if(text(2:6).eq.'    ') goto 40   ! end of stations
        data_out(k)=' '
        data_out(k)(2:6)=text(2:6)
        data_out(k)(11:18)=text(34:41)    ! output phase
        read(text(42:50),'(2i2,f5.2)') hour,min,sec
        read(text(65:71),'(f7.2)') res
        time=hour*3600+min*60.0+sec-res
        if(time.ge.0.0) then
           hour= time/3600
           min=(time-hour*3600)/60
           sec=time-hour*3600-min*60
           write(data_out(k)(19:28),'(2i2,1x,f5.2)') hour,min,sec
        else
           write(data_out(k)(19:28),'(2i2,1x,f5.1)') hour,min,time
        endif
        data_out(k)(10:10)='Y'      ! indicate calculated time
        k=k+1
        goto 30
c
c   finish station list
c
  40    continue
c
c   put back in data array, first clear other synthetic readings
c
          do i=1,nsynt
             data(max_data-i+1)=' '
          enddo
c
          nsynt=k-1
          do i=1,nsynt
             data(max_data-i+1)=data_out(i)
          enddo
            if((nrecord+nsynt).ge.max_data) then
              call clear_to_alpha
              write(6,*)
     *      ' Too many real and synthetic phases'
              stop
            endif
c
c   no data
c
 50      continue
c
c   close print.out file
c
         call sei close( close$, unit, code ) ! Close (default stop on error).
         return
         end                 
c
                                                                                             


c----------------------------------------------------------------------------   
      subroutine cbase_select(cbase,cbase_def,n_cbase_in,n_cbase_out)         
c                                                                               
c                                                                               
c     Routine toselect continuous databases.
c                                                                               
c     Input:   cbase                : list of continuous databases              
c              cbase_def            : index list of default databases (SEISAN.DEF)
c              n_cbase_in           : number of databases in
c                                                                               
c     Output   n_cbase_out          : number of databases in
c
      implicit none
      include 'mulplt.inc'
c
c------------------------------------------------------
      include 'libsei.inc'              ! Library definitions & data defns.
      external   sei integer            ! Extract integer from string.
      integer    sei integer            ! & function.
c---------------- end of sei lib changes --------------
c-- number of file names in file                  
      integer n_cbase_in
c-- number of files selected
      integer n_cbase_out
      character*(*) cbase(*)
      integer cbase_def(*)
c-- selected array                               
      integer on(1000)
      integer order_out(1000)      ! order after sorting
      character*80 box_text(1000),box_text_local(1000)
      character*120 title
c-- last character pushed in input_box
      character*1 last_char
      integer nfile_page           ! number of files, one page
      integer npage                ! number of pages
c-- Counters                                               
      integer i,k,ipage		

c
c   sort file names
c
      if (chan_sort) then
        call  text_sort(cbase,box_text,order_out,
     *                n_cbase_in)        
      endif

c
c  put in text for boxes if no sorting, else done above
c
      if(.not.chan_sort) then
         do i=1,n_cbase_in
           box_text(i)=' '
           box_text(i)=cbase(i)
           on(i)=cbase_def(i)
         enddo
      else
         do i=1,n_cbase_in
           on(i)=cbase_def(order_out(i))
         enddo
      endif
c
c   display boxes
c
      title=
     *'SELECT DATABASES, FILLED OUT BOXES INDICATE SELECTION'

c
c   loop to show many pages of file names
c
c      npage=(n_cbase_in-1)/75+1           ! max 75 names per page
      npage=(n_cbase_in-1)/200+1           ! max 200 names per page
      k=1                                 ! counter for selected files

       
      do ipage=1,npage
c
c   check if last page
c
          if(ipage.eq.npage) then
              title(81:120)=' '
c              nfile_page=n_cbase_in-(ipage-1)*75
              nfile_page=n_cbase_in-(ipage-1)*200
          else
              title(81:120)=' More databases, push OK or F'
c              nfile_page=75
              nfile_page=200
          endif
c
c   transfer data to local array
c
          do i=1,nfile_page
c             box_text_local(i)=box_text(i+(ipage-1)*75)
             box_text_local(i)=box_text(i+(ipage-1)*200)
          enddo
c
c   display one page
c     
c          call input_box(nfile_page,on,3,330.0,15.0,
          call input_box(nfile_page,on,10,89.0,20.0,
     *                     box_text_local,35,
     *                     3.0,max_ypos+30.0,title,last_char,1,0)
c
c   Quit...
c
         if(last_char.eq.'Q'.or.last_char.eq.'q') then
            n_cbase_out=0
            return
         end if                           !
c
c   put selected files at top of list
c
         do i=1,nfile_page
            if(on(i).eq.1) then
c               cbase(k)=box_text(i+(ipage-1)*75)
               cbase(k)=box_text(i+(ipage-1)*200)
               k=k+1
               on(i)=0       ! clear for next page
            endif
         enddo
      enddo

      n_cbase_out=k-1
                                                            
      return                                                                    
      end                                                                       


       
c----------------------------------------------------------------------------   
      subroutine wav_select(n_wav_file,n_wav_out)                               
c                                                                               
c                                                                               
c     Routine to show wav file names and select files to plot.             
c                                                                               
c     written by  j. havskov                                     
c                                                                               
c     Input:   n_wav_file           : in input file                             
c              wav_filename         : all waveform file names       
c                                                                               
c     Output   n_wav_out            : number of wav files selected               
c              wav_filename         : selected files
c
      implicit none
      include 'mulplt.inc'
c
c------------------------------------------------------
      include 'libsei.inc'              ! Library definitions & data defns.
      external   sei integer            ! Extract integer from string.
      integer    sei integer            ! & function.
c---------------- end of sei lib changes --------------
c-- channels selected                             
c-- number of file names in file                  
      integer n_wav_file
c-- number of files selected
      integer n_wav_out
c-- selected array                               
      integer on(80)
      integer order_out(1000)      ! order after sorting
      character*80 box_text(1000),box_text_local(80)
      character*120 title
c-- last character pushed in input_box
      character*1 last_char
      integer nfile_page           ! number of files, one page
      integer npage                ! number of pages
c-- Counters                                               
      integer i,k,ipage		

c
c   sort file names
c

      call  text_sort(wav_filename,box_text,order_out,
     *                n_wav_file)        

  
c
c  put in text for boxes if no sorting, else done above
c
      if(.not.chan_sort) then
         do i=1,n_wav_file
           box_text(i)(1:35)=wav_filename(i)(1:35)
         enddo
      endif
c
c   display boxes
c
      title=
     *'SELECT FILENAMES, FILLED OUT BOXES INDICATE SELECTION'

c
c   loop to show many pages of file names
c
      npage=(n_wav_file-1)/75+1           ! max 75 names per page
      k=1                                 ! counter for selected files

       
      do ipage=1,npage
c
c   check if last page
c
          if(ipage.eq.npage) then
              title(81:120)=' '
              nfile_page=n_wav_file-(ipage-1)*75
          else
              title(87:120)=' More files: OK or f, All files: F'
              nfile_page=75
          endif
c
c   transfer data to local array
c
          do i=1,nfile_page
             box_text_local(i)=box_text(i+(ipage-1)*75)
          enddo
c
c   display one page
c     
          call input_box(nfile_page,on,3,330.0,15.0,
     *                     box_text_local,35,
     *                     3.0,max_ypos+30.0,title,last_char,1,0)
c
c   Quit...
c
         if(last_char.eq.'Q'.or.last_char.eq.'q') then
            n_wav_out=0
            return
         end if                           !
c
c   if last char is F (not f), selectin all and show no more windows
c
        if(last_char.eq.'F') then
           n_wav_out=n_wav_file
           return
        endif
c
c   put selected files at top of list
c
         do i=1,nfile_page
            if(on(i).eq.1) then
               wav_filename(k)=box_text(i+(ipage-1)*75)
               k=k+1
               on(i)=0       ! clear for next page
            endif
         enddo
      enddo

      n_wav_out=k-1
c
c   mark in s-file if more than one
c
cfix  is this really needed, cannot see it, delete ???
c      k=2
c      if(n_wav_out.gt.1) then
c        do i=1,n_wav_file
c 200       continue
c           if(k.gt.nhead) then
c              write(6,*)' Something wrong in file selection'
c              stop
c           endif
c           if(data(k)(80:80).eq.'6') then
c              if(on(i).eq.1) data(k)(1:1)='*'
c              k=k+1
c           else
c              k=k+1
c              goto 200
c           endif
c        enddo
c      endif
                     
                                                            
      return                                                                    
      end                                                                       
c----------------------------------------------------------------------------   
      subroutine wav_select_del(n_wav_file,n_wav_out)                               
c                                                                               
c                                                                               
c     Routine to show wav file names and select files to delete.             
c                                                                               
c     written by  j. havskov                                     
c                                                                               
c     Input:   n_wav_file           : input file                             
c              wav_filename         : all waveform file names       
c
c     Output:  n_wav_out            : number of wav files selected     
c                                   : selected files
c
      implicit none
      include 'mulplt.inc'
c
c------------------------------------------------------
      include 'libsei.inc'              ! Library definitions & data defns.
      external   sei integer            ! Extract integer from string.
      integer    sei integer            ! & function.
c---------------- end of sei lib changes --------------
c-- channels selected                             
      integer n_wav_file
c-- number of files selected
      integer n_wav_out
      integer seiclen
c-- selected array                               
      integer on(100)
      character*80 box_text(100)
      character*200 title
c-- last character pushed in input_box
      character*1 last_char
c-- Counters                                               
      integer i,k,knew,l,m
      character*1 dchar
c
      call dir_char(dchar)
	  
c
c  put in text for boxes
c
      k=0
      do i=1,n_wav_file
        if(wav_filename(i)(1:1).ne.dchar) then
           k=k+1
           box_text(k)(1:35)=wav_filename(i)(1:35)
        endif
      enddo
c
c   check if any files in working directory
c
      if(k.eq.0) then
         write(6,*) 'No waveform files in working directory'
         write(6,*) 'Cannot delete names from S-file'
         return
      endif
c
      knew=k
c
c   display boxes
c
      title=
     *'SELECT FILES FOR DELETION'
cx      if(n_wav_file.gt.22) then
      if(knew.gt.22) then
cx         call input_box(n_wav_file,on,2,300.0,20.0,box_text,35,
         call input_box(knew,on,2,300.0,20.0,box_text,35,
     *                    400.0,max_ypos-50.0,title,last_char,0,0)
      else
         call input_box(knew,on,1,300.0,20.0,box_text,35,
cx         call input_box(n_wav_file,on,1,300.0,20.0,box_text,35,
     *                    700.0,max_ypos-50.0,title,last_char,0,0)
      endif
c
c   Quit...
c
      if(last_char.eq.'Q'.or.last_char.eq.'q') then
         n_wav_out=0
         return
      end if                           !
c
c   put  selected files at top of list
c
      k=1
cx      do i=1,n_wav_file
      do i=1,knew
         if(on(i).eq.1) then
cx            wav_filename(k)=wav_filename(i)
            wav_filename(k)=' '
            wav_filename(k)(1:35)=box_text(i)(1:35)
cx            write(6,*) k,wav_filename(k)
            k=k+1
         endif
      enddo
      n_wav_out=k-1
c
c   mark in s-file files to be deleted if one or more
c
      k=2
      if(n_wav_out.ge.1) then
cx        do i=1,n_wav_file
        do i=1,knew
 200       continue
           if(k.gt.nhead) then
              write(6,*)' Something wrong in file selection'
              stop
           endif
           if(data(k)(80:80).eq.'6') then
              do l=1,n_wav_out
                 m=seiclen(wav_filename(l)(1:35))
cx                 write(6,*) 
cx     *           wav_filename(l)(1:m),' ',data(k)(2:m+1)
                 if(wav_filename(l)(1:m).
     *           eq.data(k)(2:m+1)) 
     *           data(k)(1:1)='*'
              enddo
cx              if(on(i).eq.1) data(k)(1:1)='*'
              k=k+1
           else
              k=k+1
              goto 200
           endif
        enddo
      endif
                     
                                                            
      return                                                                    
      end                                                                       
c----------------------------------------------------------------------------   
      subroutine wav_select_mer(n_wav_file,n_wav_out)                               
c                                                                               
c                                                                               
c     Routine to show wav file names and select files to merge.             
c                                                                               
c     written by  j. havskov                                     
c                                                                               
c     Input:   n_wav_file           : input file                             
c              wav_filename         : all waveform file names       
c
c     Output:  n_wav_out            : number of wav files selected     
c
      implicit none
      include 'mulplt.inc'
c
c------------------------------------------------------
      include 'libsei.inc'              ! Library definitions & data defns.
      external   sei integer            ! Extract integer from string.
      integer    sei integer            ! & function.
c---------------- end of sei lib changes --------------
c-- number of file names in file                  
      integer n_wav_file
c-- number of files selected
      integer n_wav_out
c-- selected array                               
      integer on(100)
      character*80 box_text(100)
      character*120 title
c-- last character pushed in input_box
      character*1 last_char
c-- Counters                                               
      integer i,k,l,knew		
      character*1 dchar
     
      call dir_char(dchar)
      n_wav_out=0    ! initially nothing selected	  
c
c  put in text for boxes, only select files in working directory
c
      k=0
      do i=1,n_wav_file
        if(wav_filename(i)(1:1).ne.dchar) then
           k=k+1
           box_text(k)(1:35)=wav_filename(i)(1:35)
        endif
      enddo
      knew=k

c
c   display boxes
c
      title=
     *'Select files for merging in current directory'
      if(knew.gt.22) then
         call input_box(knew,on,2,300.0,20.0,box_text,35,
     *                    400.0,max_ypos-50.0,title,last_char,0,0)
      else
         call input_box(knew,on,1,300.0,20.0,box_text,35,
     *                    700.0,max_ypos-50.0,title,last_char,0,0)
      endif

c
c   Quit...
c
      if(last_char.eq.'Q'.or.last_char.eq.'q') then
         n_wav_out=0
         return
      end if                           !
c
c   put  selected files at top of list
c
      k=1
      do i=1,knew
         if(on(i).eq.1) then
            wav_filename(k)=' '
            wav_filename(k)(1:35)=box_text(i)(1:35)
            k=k+1
         endif
      enddo
c
      n_wav_out=k-1
c
c   mark in s-file if two or more
c
      k=2
      if(n_wav_out.ge.1) then
        do i=1,knew
 200       continue
           if(k.gt.nhead) then
              write(6,*)' Something wrong in file selection'
              stop
           endif
           if(data(k)(80:80).eq.'6') then
              do l=1,n_wav_out
                 if(wav_filename(l)(1:35).eq.data(k)(2:36))
     *           data(k)(1:1)='*'
              enddo
              k=k+1
           else
              k=k+1
              goto 200
           endif
        enddo
      endif


                                                            
      return                                                                    
      end                                                                       
******************************************************************              
      subroutine read_s_file

c
c   open and read s-file, close
c
c   see main program for variables
c
      implicit none
      include 'mulplt.inc'
      include 'libsei.inc'
      integer read_unit
      logical b_flag
      integer code,nstat,nphase,id
      character*1 exp,type
      
              call sei open( old$,                 ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  sfile,                ! This file.
     &                  read_unit,            ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
             call indata
     *       (read_unit,nstat,nphase,nhead,nrecord,type,exp,data,id)
             call sei close(close$,read_unit,code)    ! Close (stop on error).
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine write_s_file

c
c   open and write s-file, close 
c   do not write synthetic phases
c
c   see main program for variables
c
      implicit none
      include 'mulplt.inc'
      include 'libsei.inc'
      integer write_unit
      logical b_flag
      integer code,i,nrecord_old
      integer seiclen
c      integer port
      character*240 udptext

              call sei open( old$,            ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  sfile,                ! This file.
     &                  write_unit,           ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
              if( code .ne. e_ok$ ) then                     !
                 code = e_ok$                                ! Restore.
                 write(6,*)' Error opening S-file'
                 write(6,*)
     *           ' You might not be allowed to change data base'
c                write(6,*)' Return to continue'
                 call flush (6)
c                read(5,'(a)') i
                 goto 1                                        ! & skip out.
              endif

              do i=1,nrecord
                 if(.not.(data(i)(10:10).eq.'Y'.and.i.gt.nhead))
     *           write(write_unit,'(a80)',err=10)data(i)
              enddo
              goto 1
 10           continue
c
c   only write message first time if no change
c
                 if(nrecord_old.ne.nrecord) then
                    write(6,*)' Error writing to S-file,',
     *              ' you might not be allowed to change data base'
c                   write(6,*)' Return to continue'
                    call flush (6)
c                   read(5,'(a)') i
                    nrecord_old=nrecord
                 endif
 1           continue
             call sei close(close$,write_unit,code)    ! Close (stop on error).
c             udptext='REFRESH '//sfile(1:seiclen(sfile))//char(0)
             udptext='RELOAD'
             if (port.gt.0) call send_udp_msg(port,udptext)
       return
       end

       subroutine get_synt(nsynt,synt_head)
c
c   put possibele syntethic readings into data array
c
c   variable names as defined in mulplt main
c
       implicit none
       include 'mulplt.inc'
       include 'libsei.inc'
       character*80 text
       integer read1,code,nsynt,k
       character*10 synt_head
       logical exist

      call sei open( old$+ignore$,            ! Try to open & warn.
     &               ' ',                     ! No prompt.
     &               'iasp.out',              ! This file.
     &               read1,                   ! On unit.
     &               exist,                   ! File exists?.
     &               code )                   ! Condition.
c
c    File exists and is open!!..
c
      if (exist) then                                                           
      code = e_ok$                            ! Re-set condition.
c
c  delete all from data and down
c
      do k=nrecord+1,max_data
         data(k)=' '
      enddo
c
c  read
c
          k=0
          read(read1,'(a)',end=99) text       ! header line
 1        continue
          read(read1,'(a)',end=99) data(max_data-k) ! read into end of array
          if(k.ge.max_data-300) then  ! leave room for at least 300 phase lines
              write(6,*)
     *      ' Too many synthetic phases, check iasp.out'
              nsynt=k
              return
          endif
          if(data(max_data-k).eq.' ') goto 99
          k=k+1
          goto 1
c
 99       continue
c
c  all data read
c
          nsynt=k
          synt_head=text(1:10)   ! save date of synthetic data
          call sei close( close$, read1, code ) ! Close (default stop on error).
      endif

      return
      end

      subroutine add_synt_readings(nsynt,synt_head)
c
c   add synthetic readings if any, however only if the data is from
c   the same day
c
c   for explanation of variables, see main program
c
      implicit none
      include 'mulplt.inc'
      integer nsynt,k
      character*10 synt_head  ! date of synthetic records
c
c if character 7 or 9 of data(1)=0, make blank   LO , apr 97
c
      if (data(1)(7:7).eq.'0') data(1)(7:7)=' '
      if (data(1)(9:9).eq.'0') data(1)(9:9)=' '
         if(nsynt.gt.0.and.synt_head(4:10).eq.data(1)(4:10)) then      
            if((nrecord+nsynt).ge.max_data) then
               write(6,*)
     *         ' Too many real and synthetic phases'
                write(6,*)' Return to continue'
                call flush (6)
                read(5,'(a)') k
                stop
             endif
          data(nrecord+nsynt)=data(nrecord)
          do k=1,nsynt
             data(nrecord-1+k)=data(max_data-k+1)
          enddo
          nrecord=nrecord+nsynt
       endif
       return
       end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine mulplt_dist(nchan,station)
c
c   calculates which stations should be selected according to distance
c   and priorities set up in MULPLT.DEF for parmater mulplt_area
c
c   nchan:   number of channels in
c   station:    complete station list, all stations in wav-files
c   channelname: channel number in and out, in common

c
c   mulplt_area:   0: do not select for area
c                  1: midpoint from s-file, radius from MULPLT.DEF
c                  2: --------------------, ------- interactive
c                  3: midpoint and radius from MULPLT.DEF
c                  4: ------------------, radius interactive
c                  5: midpoint and radius interactive
c                  6: mipoindt from station in mulplt.def, radius mulplt.def
c                  7: midpoint interactive stat, distance from mulplt.def
c                  8: both interactive
c
      implicit none       
      include 'mulplt.inc'
      integer nchan

      character*5 station(*)

      character*5 stat(max_nstation) ! stations in selection
      real slat(max_nstation),slon(max_nstation),elev(max_nstation) ! lat,lon and elevation of above
      real dekm, dedeg, az0 ! dist in km, deg and azimuth
      real epilat,epilon     ! midpoint, often the epicenter
      real epilat_old,epilon_old
      real radius_area       ! radius from midpoint
      real radius_area_old
      character*80 text,prompt
      character*5 stat_area
      integer i,k

      if(mulplt_area.le.0.or.mulplt_area.gt.8) return

      if(mulplt_area_save.gt.0) then   ! get old values
         epilat=epilat_old
         epilon=epilon_old
         radius_area=radius_area_old
         if(mulplt_area_save.eq.2) radius_area=mulplt_radius  ! new interactive radius
         goto 1  ! do not ask this time
      endif
c
c   select out stations used
c
      do i=1,nchan
        k=channelname(i)
        stat(i)=station(k)
      enddo
c
c   get locations of stations used from STATION0.HYP
c
      call stat_loc_many(stat,nchan,' ',slat,slon,elev,i)
      if(i.eq.1) write(6,*) 'No station file'   

c---------------------------------------------------------------
c   first find which midpoint to use
c
c  midpoint from s-file, radius from mulplt.def
c
      if(mulplt_area.eq.1.or.mulplt_area.eq.2) then       
c
c   read epicenter
c
        if(data(1)(24:38).ne.' ') then
            read(data(1)(24:38),'(f7.3,f8.3)',err=3355) epilat,epilon
            radius_area=mulplt_radius
            goto 3356
 3355       continue
            write(6,*)'Somehing wrong with Lat-Lon in S-file'
            write(6,*)'when using mulplt_area option '
            epilat=0.0
            epilon=0.0
            radius_area=0.0
 3356       continue
         else 
            epilat=0.0
            epilon=0.0
            radius_area=0.0
         endif
c
c   radius interactive
c
         if(mulplt_area.eq.2.and.epilat.ne.0.0.and.epilon.ne.0.0) then
            write(6,*)'Enter radius in degres'
            read(5,*) radius_area
         endif
      endif
c
c   midpoint from MULPLT.DEF, radius interactive
c
      if(mulplt_area.eq.3.or.mulplt_area.eq.4) then
         radius_area=mulplt_radius
         epilat=mulplt_lat
         epilon=mulplt_lon
         if(mulplt_area.eq.4.and.epilat.ne.0.0.and.epilon.ne.0.0) then
            write(6,*)'Enter radius in degres'
            read(5,*) radius_area
         endif  
       endif
c
c  midpoint and radius interactive
c
      if(mulplt_area.eq.5) then
         write(6,*)'Enter midpoint lat, lon and radius in degrees'
         read(5,*) epilat,epilon,radius_area
      endif

c----------------------------------------------------------------
c   case of midpoint from given station
c----------------------------------------------------------------

      if(mulplt_area.eq.6.or.mulplt_area.eq.7.or.mulplt_area.eq.8)then
 2    continue    
c
c   case of both station and radius from mulplt.def
c
         
         if(mulplt_area.eq.6) then
           stat_area=mulplt_stat
           radius_area=mulplt_radius
         endif
c
c   case of radius from mulplt.def and stat interactive
c
         if(mulplt_area.eq.7.or.mulplt_area.eq.8) then         
            write(6,*)'Enter midpoint station'
            read(5,'(a)') stat_area
         endif

         if(mulplt_area.eq.7) radius_area=mulplt_radius
c
c   find if station is in list
c
 22      continue
         do i=1,nchan
            if(stat_area.eq.stat(i)) goto 5
         enddo
         write(6,*)'No such station for midpoint, try again: ',stat_area

          prompt=
     *  ' Enter station for area selection'//char(0)
          call oneline(prompt,33,text,30,20.0,500.0)
c 
          if(text(1:5).ne.'     ') then
             stat_area=text(1:5)
          endif
         goto 22
 5       continue
c
c   station found
c
         epilat=slat(i)
         epilon=slon(i)
c
c   case of station and radius interactive
c
         if(mulplt_area.eq.8) then
            write(6,*)' Enter radius in degrees'
            read(5,*) radius_area
         endif
c
      endif     
c
c   get here from beginning if no questions to be asked
c
 1    continue
                                                      
c
c   find which stations are within radius radius
c   if station at (0,0), no location for this 
c   station so include
c
      write(6,'(a,2f9.3,f7.1)')'Selecting for midpoint and radius:',
     *epilat,epilon,radius_area
      k=0
      do i=1,nchan
         if((slat(i).eq.0.0.and.slon(i).eq.0.0).or.
     *   (epilat.eq.0.0.and.epilon.eq.0.0)) then
            k=k+1
            channelname(k)=channelname(i)  ! use this station
         else      
            call delaz(slat(i)/57.3, slon(i)/57.3, dekm, dedeg, az0, 
     *      epilat/57.3, epilon/57.3)
            if(dedeg.lt.radius_area) then
               k=k+1
               channelname(k)=channelname(i)
            endif
         endif
      enddo
c
c   new number of channels
c
      nchan=k
c
c   save area and set flag that this was first time
c
      epilat_old=epilat
      epilon_old=epilon
      radius_area_old=radius_area
      mulplt_area_save=1
      return
      end
