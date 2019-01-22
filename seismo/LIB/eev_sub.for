c
c    eev subroutines moved to LIB oct 1 99
c
c    changes
c
c
c  oct 01 99 by jh: Check for id before checking for evfile name
c  oct 4          : replace read_s_file and write_s_file by 
c                           read_s_file_e and write_s_file_e to avoid
c                   conflict with nearly same files in mulplt
c  nov 3          : check for null chars after edit
c  nov 28         : stop if wrong time interval, check better Eyyymm option
c                   fix map option for PC
c  nov 30         : new options comment and U
c  jan 11 00      : variable index_file gets changed when calling mulplt for
c                   unknown reason, to patch it up, its value was saved in
c                   old_index_file before call to mulplt and restored
c                   after the call
c  feb 16 00      : remove screen output for hyp when used with focmec etc
c                   make commands pw,ph,pb for plotting synthetics
c  feb 23 00      : use parameter if to delete auto phases
c  mar 10 00      : add command print
c  may 16 00      : check if id and file name ok for several operations like
c                   R,REG and C
c  may 30 00  jh  : do not delete SEISNET request lines if reg
c  aug 18 00 jh   : add grid search, wadat
c  oct 19         : fix so if a bad seisan.mes is present, eev will not crash
c  feb 2001       : BGS changes, hypo71
c  feb 23     jh  : add local data base for copy function
c  mar 2          : accept distnace in f-format
c  feb 1, 02  jh  : do not delete s-file with a request line 
c  april 11   lot : changed seisnet request check
c  april 11   lot : register in any database, copy wave files into waveform database setup in SEISAN.DEF
c  may 7 02   lot : keep seisnet request, if event registerd into another
c                   database
c  may 27 02  lot : added selection for volcanic sub classes
c  jun 04 02  lot : add register of events from local database
c  aug 23 02  je  : added quarrycheck option
c  oct 17 02  lo  : fixed problem with event type when register
c  nov 25 02  lo  : add id if not exist when registering
c  feb 02 03  jh  : fix bug in invrad.out
c  may 14 03  jh  : explosion input
c  jul 21 03  jh  : mak esure invrad can use Pg and Sg
c  oct 27 03  jh  : add isc location program
c  feb 20 04  jh  : do not allow registration if a seisnet request present
c  jun 22 04  jh  : ---------------------------------------  CRQ ---------
c  aug 10 04  jh  : above did not work, fixed
c  jan 27 05  jh  : small format change in exp
c  feb 4  05  jh  : command macromap
c  may 25 05  lo  : change to registration
c  may 30 05  lo  : added option inputx to enter xnear,xfar
c                   added option to list models, given in MODEL.DEF
c  june 1 05  jh  : fix invrad for new amplitudes
c  oct 12 05  jh  : when inputtinf explsion, only copy until hour from header
c  feb 15 06  jh  : put epicenter into macromap
c                   fix option NEW, sec field in file name was **
c  mar 29 06  jh  : message that isc loc not on pc
c  mar    06  lo  : use get_operator in all places
c  may 24 06  jh  : operator, change to 4 chars
c  oct 15 07  pv  : add gmap
c  jan 9  08  jh  : synt program could not handle station names with first
c                   chars blank
c  mar 19 08  jh  : array option
c  apr 21 08  pv  : add TERRAIN to gmap
c  apr 24 08  jh  : bug in array option
c  may    08  jh  : change array tp pfit
c  May    08  pv  : add seisan_timeshiftdata
c  jan 30  09 jh  : add open gmap in firefox
c  jan 12 09  jh  : change bouchon default parameters, was 300, 600 and 0.001,
c                   is now 800, 2000 and 0.01, also icrease npoints from 256
c                   to 512, REDVELO set to 0, was 8.
c  2010-04-22 pv  : removed - open gmap in firefox
c  2010-04-28 jh  : implement pinv fp solution 
c  2010-08-25 jh  : implement fpfit, add option for for plot saved solution
c                   without questions
c  2010 10 01 jh  : option dd to duplicate header line
c  2010 10 28 jh  : option cm to copy out many files
c  2010 10 29 jh  : bud reading from s-file
c  2010 11 04 jh  : run hyp before hash, foc prepare is run in hash
c                   over samer type fault plane solution
c  2010 12 22 jh  : gfortran pc, all input options like inputone etc, shift output ot 'T '
c  2011 01 18 jh  : hash.out to hash_foc.out, new format for hash_foc.out
c  2011 01 19 jh  : gfortran pc: output 79 instead of 80 chars for help, for grid option
c                   add blanks to blank line in epimap.inp, associat to associa in
c                   printed output 
c  2011 01 26 jh  : do not write out hash errors
c  2011 01 31 jh  : clean up fps out with new f-line formt etc
c  2011 02 09 lo  : fixed one call to get_agency_hyp
c  2011 02 23 jh  : hash_foc to hash_seisan
c  2011 03 06 jh  : add option inputfps and ifp position wrong pol in pinv fixed
c  2011 03 22 jh  : fix ifp, put in keep_old_id_line
c  2011 03 29 jh  : put new commands to enter comments, coml and comf
c  2011 04 01 jh  : u for update, up for update event list
c  2011 04 05 jh  : fix gmap choises and output
c  2011 04 06 jh  : add option m for inputting model indicator
c  2011 10 27 jh  : make sure q for confirmed quake is upper case
c  2011 12 16 jh  : fix problem with keep_old_id_line 
c  2011 12 29 jh  : add mt inversion, look for mt
c  2012 02 21 jh  : check for zero depth and zero distance for mt and synt
c  2012 02 27 jh  : put in depth search for dreger
c  2012 02 28 jh  : put in comp and coml for argentina
c  2012 03 21 jh  : command fq for ordring fps
c  2012 04 03 jh  : small change in dreger
c  2012 04 23 jh  : support for se, bug with mt update and sample rate check
c  2012 04 26 pv  : change dreger mt values to Nm
c  2012 04 27 pv  : change mt depth to real
c  2012 04 27 pv  : change mt depth to real
c  2012 04 27 pv  : add code in update_mt_dreger to write psmeca.in file
c  2012 04 30 pv  : changed update_mt_dreger from
c                      mt_val(1,i)=mzz mt_val(2,i)=mxx mt_val(3,i)=myy 
c                      mt_val(4,i)=mxz mt_val(5,i)=mxy mt_val(6,i)=myz
c                   to: 
c                      mt_val(1,i)=mxx mt_val(2,i)=mxy mt_val(3,i)=mxz 
c                      mt_val(4,i)=myy mt_val(5,i)=myz mt_val(6,i)=mzz
c  2012 05 13 jh : put in subroutine register_event for se
c  2012 05 14 jh : change in merge_f, prepare REG for se
c  2012 06 14 jh : fix fpfit if no solution
c  2012 08  7 lo : stay on same event when append
c  2012 11 06 pv : added change of event id under 'r' command
c  2012 11 15 pv : fix 'reg' so that BUD/SCP type 6 lines are not being copied
c  2012 12 31 jh : able to check expanded arc references with w
c  2013 01 16 jh : new arc option, do not go to next event if no wav
c  2013 03 06 jh : add pic option
c  2013 04 12 jh : add automag
c  2013 04 22 jh : a plotmoment
c  2013 04 24 jh : update header line with Dreger Mw, add plotml
c  2013 05 03 jh : move update from eev to hyp, like it is done with se. this
c                  avoid usig a wrong hyp.out to be copied back in data
c                  base if 2 persons locate in same directory
c  2013 05 14 jh: bug with next event from mulplt and inf. loop
c  2013 05 15 jh: add command dels, rm put_env_op for linux, does not seem
c                 to be used, call get_operator no longer has linux in call
c  2013 10 13 jh: add command  to plot spectra and wa,pspec
c  2013 10 30 jh: add command em to edit macroseismic file, pp to plot a
c                 picture file 
c  2014 02 18 jh: add command mte to edit mt station parameters
c  2014 03 12 jh: bug with dels, new option amp for p-spectrum
c  2014 03 14 pv: cleanup warnings
c  2014 04 17 jh: fix probelm with overwrite using automag, p-option automag
c                 had disappered!
c  2014 04 29 pv: fixed write statement, could not be reached
c  2014 05 31 jh: add q to print help
c  2014 06 17 jh: more problem with overwrite for am
c  2014 10 11 jh: new hypoinverse
c  2015 01 13 jh: update file mt_inv_redi.out with nordic file header line
c  2015 03 19 jh: add option of rejecting phases
c  2015.06.04 pv: add command log to show log file (and create if no log file)
c  2015.06.22 pv: Logging has been added if comment is inserted (COM command)
c  2015.06.22 pv: Logging has been added if event is registered  (REG or PUT command)
c  2015.06.22 pv: Logging has been added if event is updated  (UPDATE command)
c  2015.06.22 pv: Logging has been added if event is duplicated  (DUP command)
c  2015.06.22 pv: Logging has been added if event is moved or copied to another 
c                 database  (C command)
c  2015.06.22 pv: Logging has been added if event type is changed  (R command)
c  2015.06.22 pv: Logging has been added if sfile has been edited  (E command)
c  2015.06.22 pv: Logging has been added if event type is deleted  (D command)
c  2015.10.29 pv: fixed logging bug when deleting event in local database
c  2015.10.30 jh: add command ARX to extarct wav file from arc
c  2016.10.04 jh: fix command for autopic with lomax
c  2016.01.14 jh: fix so autophase does not start if wavatool crash
c  2016 01 20 lo: put in ratio
c  2016 01 27 jh: changeer ratio to ampratio
c  2016 02 10 jh: put in fix depth command fix
c  2016 02 24 jh: put in option poo to enter by default in single trace mode
c  2016 03 16 jh: remove comand MT, not used
c  2016 07 18 Jh: do not ask for plotting of autoratio if no results
c  2016 08 18 lo: alternatives for input commands
c  2016 09 03 jh: fixed diemnsion of sfile in read and write sfile
c                 routines
c  2016 10 26 jh: close unit 80 after reading array.out
c  2016 12 02 jh: remove seimeca, hash_seisan to hash, fpfit_seisan to fpfit
c  2016 12 20 jh: fix eof for hash
c  2017 01 06 jh: add command foo using plotfoc    
c  2017 01 20 jh: add command fd to compare P and T of two solutions 
c  2017 01 25 jh: add ste and std to put in start location flags, ep to edit print.out
c  2017 02 10 jh: mapf to plot epimap with fps
c  2017 05 13 jh: fix so Lxx works again
c  2017 06 02 jh: put in command fm to plot moment tensor solution with mopad   
c  2017 11 26 jh: implement old focmec program, allow UR command from SE
c  2018 01 30 jh: put in command FIXO to fix and unfix origin time, fixe for epicenter
c  2018 02 09 jh: start google earth or map automatically when locating and start 
c                 google map or earth
c                 immediately after command. new command turns on or off gearth
c                 or map after location with command ghyp. input files are always
c                 written in current directory. the gmap command also starts the plot immediately
c  2018 03 19 jh: bug with finding current directory, redo gmtmap commend without expect script
c  2018 03 23 jh: bug when delting file using index file of local data base
c  2018 05 21 jh: new address for gmap, gmtmap to mapg, new command mapgf to also plot fps

c  
      subroutine eev_action(base_name,start_time,end_time,keys,
     *command,event_no,evfile,fstart,index_file,
     *from_eev,from_mulplt,operator,se)
c
c   routine for the action of eev
c
      implicit none                                                 
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimensions
       include 'seisan.inc'
       include 'waveform.inc'
       include 'rea.inc'
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C

c-- top directory name
      character*60 top_directory
c-- time interval chosen          
      CHARACTER*14      START_TIME,END_TIME     
c-- data base name                
      CHARACTER*40      BASE_NAME               
      character*80  wave_file   ! waveform file name
      character*80 id_old
c-- input fron transfer file      
c      character*40      base_or_time            
c-- one line                                            
      character*80	text
      character*160     pitsa_text,sac_text,long_text 
      character*400 txt			
c-- answer
      character*1 choise
      character*80 eev_edit   ! editor name
      integer sei clen            ! function to get string length
      integer from_eev     ! indicate to routine findevin that call is from eev
      character*3 agency   ! station file agency code
c-- unix command
      character*80 command
c-- event file name               
      character*80      evfile,oldfile          
c-- waveform file name
      character*80      wavefile,wavefile1,directory
      character*3       new_type   ! new type of event
      character*1       old_type   ! old type of event
c-- old and new----               
      character*80      newfile   !,master 
c-- ev. selection key             
      character*10      keys                    
c-- file name printed out         
      character*80      out_name                
c-- base name to move file to                   
      character*5       new_base
c-- answer                        
      character*1       answer                  
c-- event id line number
      integer id
c-- operator code
      character*4 operator
      character*5 reg_base         ! database to register in
      character*4 cwavyear
      character*2 cwavmon
      character*80 from_mulplt     ! command from mulplt
      logical se                   ! eev started from se if true
      character*1    mulplt_def    ! if O, all default for mulplt plotting
      character*1 auto_p           ! blank, P or S for automag

c-- log file name
      character*120      logfile

c---time of making update
      character*12 p_time
      character*14 proc_time
c-- event number                  
      integer           event_no,oldno,current_event_number,old_event_no
c-- new month indicator           
      integer           new_month               
c-- status of event search        
      integer           status                  
c-- see base                      
      integer           fstart,old_fstart
c-- event arrays                  
      character*80      data1(max_data),data2(max_data)   
c-- no of stations for event                        
      integer		nstat1,nstat2		
c-- no of phases for one event
      integer nphase1
c-- no of records for event                  
      integer     	nrecord1,nrecord2	
c-- no of headers for event                         
      integer		nhead1,nhead2		
c-- event ids--------------               
      character*1	exp1,exp2,evid1,evid2	
      integer show_menu            ! indicator if menu shown in mulplt
      real dist,azim,amp           ! distance and azimuth and amplitude

      real strike,dip,rake
      real del_strike,del_dip,del_rake ! errors in strike, dip and rake
      real fit                     ! fit of fps solution
      real amperr                  ! error amp ratio
      character*1 fps_quality      ! fps quality
      character*3 fps_agency
      character*7 fps_program
      real stdr                    ! station distribution ration in fps
      real hdepth                  ! hypocenter depth
      real amp_error               ! error in amplitudes for mom tensor
      real depth(50),vp(50)        ! model

      real minlat,maxlat,minlon,maxlon,dlat,dlon   ! for gridsearch
c
c-- number of errors in s-file
      integer nerr
c-- number of arc entries in s-file
      integer karc
c-- counter                                                    
      integer		i,j,k,kk,ilen,l,m,n,n1,n2
      logical       loc_append    ! true if appended events only to be located
      logical       delete_event  ! delete event without asking
      logical b_flag,exist  ! for input output
c--- logicals for fault plane solution 
      logical oldfault,newfault
      logical keep_old_id_line    ! if true keep old lines
c
c  mt parameters
c
      real azimuth(300)            ! azimuth for several stations
      real distance(300)           ! distance -------------------

      character*19 mt_start_time   ! start time of data selection window
      character*19 mt_start_time_par ! -------------- in par file--------
      character*80 mt_data_file    ! mt data file with date-time
      real mt_window               ! window of data file secs
      logical mt_comp_use(3,300)   ! if true, use component, order T, R, Z
      integer mt_offset(300)       ! offset for mt
      integer mt_offset_all(300)   ! ------- all in s-file
      integer mt_offset_used(300)  ! offset from inversion
      integer mt_ch(300)           ! channel number in file mulplt.wav with first 
                                   ! channel for station i 
      integer mt_used(300)         ! 1 if used, 0 if not used
      integer skip(300)            ! skip in data files
      integer ndepth               ! number of depths for greens function
      integer kdepth               ! counter ------------
      integer kdepth_first,kdepth_last ! first and last to use
      real del_depth               ! increment in depth ----------------
      real vr(300)                 ! varience reduction	
      real vr_max                  ! max vr
      real max                     ! dummy
      real mt_strike(300),mt_dip(300),mt_rake(300) ! used in depth loop
      integer vr_max_depth         ! depth number corresponding to vr_max
      logical first_run            ! if true, first rund through mt depth loop
	
      logical mt_par              ! if true make synt parameters from mt
      logical mt_stat_found
      logical ghyp                ! true if plot with google earth/map after location
      logical from_hyp            ! true if plot of gmap after a location
      logical googleearth         ! use google earth
      logical googlemap           ! use google map
      integer mt_nstat            ! number of stations used for mt
      integer mt_nstat_all        ! all in s-file
      integer mt_npoints          ! number of points for inversion
      character*5 mt_stat(300)    ! mt-stations
      character*5 mt_stat_all(300)! ----------- in sfile
      character*5 mt_stat_full(300) ! mt stations with __
      integer mt_stat_nsamp(300)    !sample of each station
      real mt_stat_rate(300)        ! sample rate each station
      character*80 mt_sfile       ! sfile used to generate file with data traces for mt
      real mt_flow,mt_fhigh       ! mt filters
      integer mt_pole             ! mt poles
      integer mt_npasses           ! mt  filter passes, 1 or 2
      real mt_data_flow,mt_data_fhigh ! mt filter as given in data file
      integer mt_data_pole,mt_data_npasses ! more filter stuff
      character*80 mt_data_sfile  ! sfile name as given in data file
      character*12 mt_dispvel     ! mt displacement or velocity
      character*12 mt_data_dispvel! -------------  in data file

      real strike1,dip1,rake1     ! fps event1
      real strike2,dip2,rake2     ! ---------2
      real delp,delt              ! difference in P and T

      real xskip                  ! --------------
      real mt_rate,mt_data_rate   ! sample rate for mt
      real mt_redvel              ! reduction velocity
      real mt_dist(300)           ! epicentral distances
      integer mt_shift(300)       ! time shift in data in org. samples

      character*70 hlp_text(500)  ! help text
      integer hlp_start(50)       ! start number of each section
      integer hlp_number          ! counting sections
      integer hlp_total           ! toal number of lines in hlp file
 


c-- dirctory separation character
      character*1 dchar
c-- indicator of computer used
          logical sun,pc,linux
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c logical for inquire
       logical file_exist
c returned code
       integer          code
       real             baz,appvel   ! back azi., app. velocity for array option
       character*5      ref_station  ! reference station when using array option
c read unit #1 .. #6
       integer          read01, read02, read03 
c write unit #1 .. #5
       integer          write01,  write03, write04
       integer          write06
c file name
       character*80     chr_file
c indicator of the use of an index file
       logical index_file, old_index_file
       real sec          ! seconds
       integer isec      ! seconds
       character*3 month(12)
       real range        ! help variable
       integer year,mon,day,hour,min
c length of the command line to call vi editor, LAA 09.97
       integer leng
c variables for epimap
       real map_lat_c,map_lon_c
c number of wave files for conversion to pitsa
       integer npitsa,nsac
c xnear and xfar values
       real xnear,xfar,sdep
c list of wave files
       character*80 file_list(99)
       real duration_spec,duration_wa   ! durations for autmag
c command line to run gmtmap and macromap
       character*160 gmtmap
c line to run user command
       character*160 usercom
c volcanic sub class
       character*6 sub_class
       character*23 start_time_duration ! for ARC line
       integer doy                      ! for ARC
       double precision msecs           ! for ARC
c flag
       logical request_flag
c quarry
       integer nquarry
       character*(100) quarry(100)
c-- filter coefficients  
      real cof(8)
c-- filter gain
      real gain
       real elat,elon
       character*1 choice
c SEISAN extension
       character*10 extension
       common/eev/data1,nstat1,nphase1,nrecord1,nhead1,id,exp1,evid1
       data month /'Jan','Feb','Mar','Apr','May',
     * 'Jun','Jul','Aug','Sep','Oct','Nov','Dec'/ 
c

      call topdir(top_directory)
      call dir_char(dchar)
      call computer_type(sun,pc,linux)
      call get_seisan_def
      call get_env_seisan_extension(extension)

c      fps_agency=' '
c      fps_quality=' '
c      fps_program=' '
       keep_old_id_line=.true.
       mt_par=.false.
       mt_start_time=' '
       mt_start_time_par=' '
       mt_dispvel=' '
       duration_spec=0.0
       duration_wa=0.0
       auto_p=' '
cxx       ghyp=.false.
c
c   jump data base positioning
c
      goto 15
c
c   back here for data base action before return
c

 4724 continue
      keys(1:4)='SAME'

 10   continue
        
C                                                                               
C  data base action, find next event                                                   
C                                                                               
c
c     write(6,*)' pv: eev_sub :  255: end time : ',end_time
      call findevin
     *(base_name,start_time,end_time,keys,from_eev,event_no, 
     *evfile,fstart,new_month,status)
      call put_env_event(evfile)   ! put event file name in memory
c
c  check if previous command was from mulplt, if so, reset and return to
c  mulplt
c
      if(from_mulplt(1:4).ne.'    ') then
         from_mulplt(1:10)='P         '
      endif

c
c   if a jump to another month is made, reset jump indicator
c
      if(from_eev.eq.0) from_eev=1
c
c                                                                               
C   CHECK IF END OF TIME INTERVAL OR OTHER ERRORS                               
C                                                                               
      IF(STATUS.GT.0) THEN                                                      
         if(status.eq.1) write(6,*)' ***** event not found'                     
         if(status.eq.5) write(6,*)' ***** wrong time interval'                 
c
c   if an empty index file, stop
c
         if(event_no.eq.0.and.status.eq.3) stop
         if(status.eq.9.or.status.eq.5) stop   ! something wrong
c
cccc the next section is probably not active anymore since variable
c    from_eev prevent status of being 2 or 3
cccc
c
c   if here, assume end of month, status=3 start again
c
c-- begin with first event        
         keys(1:10)='#1        '
         goto 10                                
      ENDIF  
      return                                                                   
c
c-- continue here at entry to subroutine                      
c
15    continue
c
c---------------------------------------------------------------------
c   check here if eev should go over several months indicated by eyymm
c   e.g. e9202
c---------------------------------------------------------------------
c
      if((keys(1:1).eq.'e'.or.keys(1:1).eq.'E').and.
     *   keys(2:3).ne.'  ') then
         read(keys(2:7),'(i6)',err=7733) i   ! just check if a valid number
c         goto 7734
c 7733    continue
c         keys(1:4)='SAME'
c         goto 10
c 7734    continue
         end_time(1:6)=keys(2:7)
         if (end_time(5:6).eq.'  '.or.end_time(3:4).eq.'  '.or.
     *       end_time(2:2).eq.' ') then
           write(*,'(a)') ' TRY Eyyyymm ********************'
           keys(1:4)='SAME'
           goto 10
         endif
         keys=' '
         from_eev=2
         goto 10
      endif

 7733 continue          
c
c-----------------------------------------------------------------------
c  check if eev jump to another month and base, j command
c-----------------------------------------------------------------------
c
      if((keys(1:1).eq.'j'.or.keys(1:1).eq.'J').and.
     *   keys(2:5).ne.'     ') then
         if(se) then
            write(6,*)' Select another month or base in SE'
            keys(1:4)='SAME'
            goto 10
         endif
         read(keys(2:7),'(i6)',err=8733) i   ! just check if a valid number
         goto 8734
 8733    continue
         keys(1:4)='SAME'
         goto 10
 8734    continue
         start_time=keys(2:7)
         end_time=' '
         from_eev=0
         if(keys(9:10).ne.' ') then
           base_name(1:2)=keys(9:10) ! new base
           base_name(3:3)=command(1:1)
         endif
         keys=' '
         goto 10
      endif
         
                                                                                
c-----------------------------------------------------                                                                               
c   check for lower and upper case and abbreviations                                              
c                                                                               
      if(keys(1:5).eq.'pfit') keys(1:5)='PFIT'
      if(keys(1:2).eq.'pf') keys(1:2)='PF'
      if(keys(1:2).eq.'fp') keys(1:2)='FP'
      if(keys(1:2).eq.'fd') keys(1:2)='FD'
      if(keys(1:4).eq.'fdif') keys(1:2)='FP'
      if(keys(1:3).eq.'foo') keys(1:3)='FOO'
      if(keys(1:5).eq.'f_old') keys(1:5)='F_OLD'
      if(keys(1:2).eq.'fo') keys(1:2)='FO'
      if(keys(1:2).eq.'fm') keys(1:2)='FM'
      if(keys(1:5).eq.'fpfit') keys(1:5)='FPFIT'
      if(keys(1:7).eq.'autosig') keys(1:7)='AUTOSIG'
      if(keys(1:7).eq.'automag') keys(1:7)='AUTOMAG'
      if(keys(1:3).eq.'am ') keys(1:7)='AUTOMAG'
      if(keys(1:3).eq.'ami') then
         keys(1:7)='AUTOMAG'
         write(6,*)
     *  'Give spectral and wa windows, a zero means not processing'
         read(5,*) duration_spec,duration_wa
         write(6,*)'Give p if P-spectrum, else enter for S-spectrum'
         read(5,'(a)') text
         if(text(1:1).eq.'p') auto_p='P'
      endif
      if(keys(1:3).eq.'arx')keys(1:3)='ARX'
      if(keys(1:4).eq.'arc ') keys(1:4)='ARC '
      if(keys(1:6).eq.'arcdel') keys(1:6)='ARCDEL'
      if(keys(1:8).eq.'ampratio') keys(1:8)='AMPRATIO'
      if(keys(1:3).eq.'ar ') keys(1:8)='AMPRATIO'
      if(keys(1:4).eq.'iasp') keys(1:4)='IASP'
      if(keys(1:3).eq.'ic ') keys(1:3)='IC '
      if(keys(1:6).eq.'email ') keys(1:6)='EMAIL '
      if(keys(1:8).eq.'herrmann') keys(1:8)='HERRMANN'
      if(keys(1:6).eq.'hersei') keys(1:6)='HERSEI'
      if(keys(1:5).eq.'bouch') keys(1:5)='BOUCH'
      if(keys(1:6).eq.'bousei') keys(1:6)='BOUSEI'
      if(keys(1:6).eq.'rmsdep') keys(1:6)='RMSDEP'
      if(keys(1:4).eq.'wkbj') keys(1:4)='WKBJ'
      if(keys(1:3).eq.'tsd') keys(1:3)='TSD'  ! Not implemented, alpha version
      if(keys(1:3).eq.'wad') keys(1:3)='WAD'
      if(keys(1:3).eq.'put')  keys(1:3)='PUT'
      if(keys(1:3).eq.'reg')  keys(1:3)='REG'
      if(keys(1:3).eq.'std') keys(1:3)='STD'
      if(keys(1:3).eq.'ste') keys(1:3)='STE'
      if(keys(1:5).eq.'dels')  keys(1:5)='DELS'
      if(keys(1:3).eq.'sac') keys(1:3)='SAC'
      if(keys(1:5).eq.'pitsa') keys(1:5)='PITSA'
      if(keys(1:3).eq.'pmm') keys(1:3)='PMM'
      if(keys(1:3).eq.'pml') keys(1:3)='PML'
      if(keys(1:4).eq.'mapf') keys(1:4)='MAPF'
      if(keys(1:5).eq.'mapgf') keys(1:5)='MAPGF'
      if(keys(1:4).eq.'mapg') keys(1:4)='MAPG'
      if(keys(1:3).eq.'map') keys(1:4)='MAP '
      if(keys(1:2).eq.'m ') keys(1:7)='MODELIN'
      if(keys(1:6).eq.'models') keys(1:6)='MODELS'
      if(keys(1:4).eq.'grid') keys(1:4)='GRID'
      if(keys(1:4).eq.'gmap') keys(1:4)='GMAP'
      if(keys(1:4).eq.'ghyp') keys(1:4)='GHYP'
      if(keys(1:8).eq.'macromap') keys(1:8)='MACROMAP'
      if(keys(1:2).eq.'ss') keys(1:2)='SS'
      if(keys(1:2).eq.'em') keys(1:2)='EM'
      if(keys(1:2).eq.'s ') keys(1:2)='S '                                      
      if(keys(1:1).eq.'s') keys(1:1)='S'                                      
      if(keys(1:2).eq.'t ') keys(1:2)='T '
      if(keys(1:3).eq.'tt ') keys(1:3)='TT '
      if(keys(1:6).eq.'ttplot') keys(1:6)='TTPLOT'
      if(keys(1:2).eq.'e ') keys(1:1)='E '                                        
      if(keys(1:3).eq.'dup') keys(1:3)='DUP'
      if(keys(1:2).eq.'dd') keys(1:2)='DD'
      if(keys(1:1).eq.'d') keys(1:1)='D' 
      if(keys(1:2).eq.'ep') keys(1:2)='EP'                                       
      if(keys(1:2).eq.'p ') keys(1:2)='P ' 
      if(keys(1:3).eq.'poo') keys(1:3)='POO'                                   
      if(keys(1:3).eq.'po ') keys(1:3)='PO '
      if(keys(1:3).eq.'pol') keys(1:3)='POL'
      if(keys(1:2).eq.'pa') keys(1:2)='PA'
      if(keys(1:2).eq.'pw') keys(1:2)='PW'
      if(keys(1:2).eq.'ph') keys(1:2)='PH'
      if(keys(1:2).eq.'pb') keys(1:2)='PB'
      if(keys(1:2).eq.'pm') keys(1:2)='PM'
      if(keys(1:2).eq.'pg') keys(1:2)='PG'
      if(keys(1:2).eq.'pd') keys(1:2)='PD'
      if(keys(1:2).eq.'pp') keys(1:2)='PP'
      if(keys(1:2).eq.'ps') keys(1:5)='PSPEC'
      if(keys(1:2).eq.'pspec') keys(1:5)='PSPEC'

      if(keys(1:2).eq.'ap') keys(1:2)='AP'
      if(keys(1:9).eq.'autophase') keys(1:2)='AP'
      if(keys(1:2).eq.'il') keys(1:2)='IL'
      if(keys(1:1).eq.'a') keys(1:1)='A'                                         
      if(keys(1:2).eq.'lr') keys(1:2)='LR' 
      if(keys(1:2).eq.'ll') keys(1:2)='LL' 
      if(keys(1:2).eq.'l ') keys(1:2)='L '                                     
      if(keys(1:3).eq.'log') keys(1:3)='LOG'
      if(keys(1:6).eq.'hypo71') keys(1:6)='HYPO71'                             
      if(keys(1:1).eq.'h') keys(1:1)='H'                                        
      if(keys(1:6).eq.'invrad') keys(1:6)='INVRAD'                             
      if(keys(1:7).eq.'comment') keys(1:7)='COMMENT'
      if(keys(1:4).eq.'com ') keys(1:7)='COMMENT'
      if(keys(1:4).eq.'comf') keys(1:4)='COMF'
      if(keys(1:4).eq.'coml') keys(1:4)='COML'
      if(keys(1:4).eq.'comt') keys(1:4)='COMT'
      if(keys(1:4).eq.'comp') keys(1:4)='COMP'
      if(keys(1:2).eq.'cm') keys(1:2)='CM' 
      if(keys(1:3).eq.'mti') keys(1:3)='MTI'
      if(keys(1:3).eq.'mtp') keys(1:3)='MTP'
      if(keys(1:3).eq.'mtg') keys(1:3)='MTG'
      if(keys(1:3).eq.'mtd') keys(1:3)='MTD'
      if(keys(1:3).eq.'mte') keys(1:3)='MTE'
      if(keys(1:4).eq.'fixo') keys(1:4)='FIXO'
      if(keys(1:4).eq.'fixe') keys(1:4)='FIXE'
      If(keys(1:4).eq.'fix ') keys(1:4)='FIX '

 
      if(keys(1:1).eq.'c') keys(1:1)='C' 
      if(keys(1:1).eq.'m') keys(1:1)='M'
      if(keys(1:2).eq.'fh') keys(1:2)='FH'
      if(keys(1:2).eq.'fq') keys(1:2)='FQ'                                        
      if(keys(1:1).eq.'w') keys(1:1)='W'
      if(keys(1:2).eq.'r ') keys(1:2)='R '
      if(keys(1:1).eq.'z') keys(1:1)='Z'
      if(keys(1:2).eq.'fc') keys(1:2)='FC'
      if(keys(1:2).eq.'fi') keys(1:2)='FI'
      if(keys(1:1).eq.'f') keys(1:1)='F'
      if(keys(1:1).eq.'o') keys(1:1)='O'
      if(keys(1:4).eq.'Synt') keys(1:4)='SYNT'
      if(keys(1:4).eq.'synt') keys(1:4)='SYNT'
      if(keys(1:5).eq.'print') keys(1:5)='PRINT'
      if(keys(1:3).eq.'Mac') keys(1:3)='MAC'
      if(keys(1:4).eq.'pmac') keys(1:4)='PMAC'
      if(keys(1:3).eq.'new') keys(1:3)='NEW'
      if(keys(1:8).eq.'inputepi') keys(1:8)='INPUTEPI' 
      if(keys(1:3).eq.'ie ') keys(1:8)='INPUTEPI' 
      if(keys(1:8).eq.'inputfps') keys(1:8)='INPUTFP '
      if(keys(1:3).eq.'ifp') keys(1:8)='INPUTFPS' 
      if(keys(1:8).eq.'inputone') keys(1:8)='INPUTONE' 
      if(keys(1:3).eq.'i1 ') keys(1:8)='INPUTONE' 
      if(keys(1:6).eq.'inputx') keys(1:6)='INPUTX'
      if(keys(1:3).eq.'ix ') keys(1:6)='INPUTX'
      if(keys(1:2).eq.'u ') keys(1:6)='UPDATE' 
      if(keys(1:2).eq.'ur') keys(1:6)='UPDREJ' 
      if(keys(1:6).eq.'update')    keys(1:6)='UPDATE'
      if(keys(1:7).eq.'usercom') keys(1:7)='USERCOM'
      if(keys(1:2).eq.'up') keys(1:1)='U'
      if(keys(1:6).eq.'quarry') keys(1:6)='QUARRY'  !JE Aug2002 Quarrycheck
      if(keys(1:3).eq.'exp') keys(1:3)='EXP'
      if(keys(1:1).eq.'l') keys(1:1)='L'            ! for locating two events together

c
c----------------------------------------------------------
c   help
c----------------------------------------------------------
c
      if(keys(1:4).eq.'help') keys(1:1)='?'
      if(keys(1:4).eq.'HELP') keys(1:1)='?'
      if(keys(1:1).eq.'?') then
         i=index(top_directory,' ')-1

         chr_file = top_directory(1:i)//'/DAT/EEV.HLP'
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     chr_file,              ! File name
     &                     read01,                ! Read unit #1
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
         if(code.ne.e_ok$) then
            write(6,*)' Help file missing'
            keys(1:4)='SAME'
            goto 10   
         endif
         i=1

         hlp_number=0
         do k=1,1000
            read(read01,'(a)', end=1017) hlp_text(k)
            if(hlp_text(k)(1:3).eq.'***') then
              hlp_number=hlp_number+1
              hlp_start(hlp_number)=k
            endif
         enddo
 1017    continue
         hlp_total=k
c
c   show topics
c
 1018    continue
         do i=1,3
           write(6,'(a)') hlp_text(i)
         enddo

         do i=1, hlp_number-1
           write(6,'(i2,2x,a)') i,hlp_text(hlp_start(i))
         enddo
         write(6,*)
         write(6,*)
     *  'Give a number for topic, 0 for whole list, enter to terminate'
         read(5,'(a)') text
         write(6,*)
         if(text.eq.' ') goto 1001
         read(text,*) i
         if(i.lt.0.or.i.gt.hlp_number-1) goto 1018
         if(i.eq.0) goto 1019
         k=hlp_start(i)
         write(6,'(a)') hlp_text(k)
         do i=k+1,1000
           if(hlp_text(i)(1:3).eq.'***') then
              write(6,'(a)')
     *        'Get list (enter), q to terminate'
              read(5,'(a)') text
              if(text.eq.' ') then
                goto 1018
              else
                goto 1001
              endif  
           endif           
           write(6,'(a)') hlp_text(i)
         enddo

 1019    continue
         rewind(read01)
         do k=1,1000
            i=i+1
            if(i.eq.20)  then
               write(6,*)' Return for next page, q to quit'
               read(5,'(a)') text
               if(text(1:1).eq.'q') goto 1001
               i=1
            endif
            read(read01,'(a)',iostat=code) text
            call sei code(fort$,code,read01,b_eof)
            if (b_eof) go to 1001
            write(6,'(a)') text(1:79)
         enddo
 1001    continue
         call sei close (close$,read01,code)
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------
c MULTILETTER COMMANDS
c----------------------------
c
c-----------------------------------------------------------------------
c put in epicenter and origin time
c
      if(keys(1:8).eq.'INPUTEPI') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 1329
         goto 1330                                                         
 1329    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 1330   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   update line
c
         write(6,'(a)') ' Input data for header line'
         write(6,'(a,a)') ' Type below some or all data below fields,',
     *   ' field starts with - or upper case chr'
         write(6,'(a,a,a)') ' Original information remain ',
     *              'unless new is put in,',' text is deleted with _'
         write(6,*)' Original data is shown below text line'

 
         write(6,'(a,a)') 
     *              ' YEAR MODA HRMI SECC LE-Latitu-Longitu-DeptFF',
     *              'AGA-NS-RMS MAGLXXX MAGLXXX MAGlXXX'
         write(6,'(a)') data1(1)(1:79)
cc         if(pc)read(5,'(a)')text(2:79)
cc         if(sun.or.linux)
         read(5,'(a)')text(1:79)
         text(1:1)=' '
         text(80:80)='1'
c
c   only non blank fields are put in
c
         do i=1,80
          if(text(i:i).ne.' ') data1(1)(i:i)=text(i:i)
          if(data1(1)(i:i).eq.'_') data1(1)(i:i)=' '
         enddo
c-- write in file                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
        do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif

c------------------------------------------------------------------------
c  delete ARC lines in s-file 
c------------------------------------------------------------------------
      if(keys(1:6).eq.'ARCDEL') then
         txt='arc_del '//evfile(1:fstart+20)//' overwrite '
     *   //operator
         call systemc(txt,seiclen(txt))
         keys(1:4)='SAME'
         goto 10
      endif
c
c-----------------------------------------------------------------------
c put in new arc line for virtual net
c-----------------------------------------------------------------------

      if(keys(1:5).eq.'ARC _') then
c
c   read s-file
         call read_s_file_e(evfile)
c
        text=' '
        text(80:80)='6'
        text(2:4)='ARC'
        text(6:6)='_'
        do i=1,5
         if(keys(5+i:5+i).NE." ")text(6+i:6+i)=keys(5+i:5+i)
        enddo
c
c   if there are default values, put them in
c
         start_time_duration=' '
c
c   set start time, take origin time
c
         read(data1(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *   year,mon,day,hour,min,sec
         call TIMSEC (YEAR,MON,DAY,HOUR,MIN,SEC,MSECS)
c
c   start 30 s before origin time of arc_start_time not defined
c
         if(arc_start_time.eq.0.0) arc_start_time=30.0
         msecs=msecs-arc_start_time   ! start arc_start_time before origin time
         call SECTIM (MSECS,YEAR,DOY,MON,DAY,HOUR,MIN,SEC)
         write(start_time_duration(1:17),
     *   '(i4,1x,2i2,1x,2i2,1x,i2)')
     *   YEAR,MON,DAY,HOUR,MIN,int(SEC)
c
c   set duration, 600s if not defined in seisan.def
c
         if(arc_duration.eq.0.0) arc_duration=300.0
         k=arc_duration
         write(start_time_duration(18:23),'(i6)') k
         text(22:44)=start_time_duration
c
c   reshuffle lines
c
         do i=nrecord1,2,-1 
            data1(i+1)=data1(i)
         enddo
         data1(2)=text
         nrecord1=nrecord1+1
         nhead1=nhead1+1
         write(6,*) '**********  new Virtual NET ARC line made *******'
        call write_s_file_e(evfile)
        keys(1:4)='SAME'
        goto 10
      endif
c
c
c-----------------------------------------------------------------------
c put in or edit arc line
c-----------------------------------------------------------------------

      if(keys(1:5).eq.'ARC  ') then
c
c   read s-file
c
         call read_s_file_e(evfile)

c
c  find if arc line
c
         k=0
         do i=2,nhead1
           if(data1(i)(2:4).eq.'ARC'.and.data1(i)(80:80).eq.'6') then  ! only find first arc line
              k=i
              goto 4746
           endif
         enddo

c
c  if here, no arc line, generate a default line
c
         text=' '
         text(80:80)='6'
         text(2:4)='ARC'
         text(6:6)='*'
c
c   if there are default values, put them in
c

         start_time_duration=' '
c
c   set start time, take origin time
c
         read(data1(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *   year,mon,day,hour,min,sec
         call TIMSEC (YEAR,MON,DAY,HOUR,MIN,SEC,MSECS)
c
c   start 30 s before origin time of arc_start_time not defined
c
         if(arc_start_time.eq.0.0) arc_start_time=30.0
         msecs=msecs-arc_start_time   ! start arc_start_time before origin time
         call SECTIM (MSECS,YEAR,DOY,MON,DAY,HOUR,MIN,SEC)
         write(start_time_duration(1:17),
     *   '(i4,1x,2i2,1x,2i2,1x,i2)')
     *   YEAR,MON,DAY,HOUR,MIN,int(SEC)
c
c   set duration, 600s if not defined in seisan.def
c
         if(arc_duration.eq.0.0) arc_duration=300.0
         k=arc_duration
         write(start_time_duration(18:23),'(i6)') k
         text(22:44)=start_time_duration
c
c   reshuffle lines
c
         do i=nrecord1,2,-1 
            data1(i+1)=data1(i)
         enddo
         data1(2)=text
         nrecord1=nrecord1+1
         nhead1=nhead1+1
         write(6,*) '**********  new ARC line made *******************'

         goto 4747
c
c  edit existing arc line
c
 4746    continue                                             
c
c   update line
c
         write(6,'(a)') ' Input data for ARC line'
         write(6,'(a,a)') ' Type below some or all data below fields,',
     *   ' field starts with - or upper case chr'
         write(6,'(a,a,a)') ' Original information remain ',
     *              'unless new is put in,',' text is deleted with _'
         write(6,*)'Original data is shown below text line'

 
         write(6,'(a)') 
     *   ' ARC STAT  CMP NT LO YEAR MODA HRMM SS   DUR'
         write(6,'(a)') data1(k)(1:60)
         read(5,'(a)')text(1:79)
         text(1:1)=' '
         text(80:80)='6'
c
c   only non blank fields are put in
c
         do i=1,80
           if(text(i:i).ne.' ') data1(k)(i:i)=text(i:i)
         enddo
         do i=7,80
           if(data1(k)(i:i).eq.'_') data1(k)(i:i)=' '
         enddo
c
c-- write in file
c                            
 4747   continue
        call write_s_file_e(evfile)
c
        keys(1:4)='SAME'
        goto 10
      endif
c
c------------------------------------------------------------------------
c extract a waveform file from ARC and register in file
c
      if(keys(1:3).eq.'ARX') then
          call get_operator(operator)
          call systemc('get_arc '//evfile(1:fstart+20)//' '//operator,
     *    8+fstart+20+5)
          keys='SAME'
          goto 10
      endif       


c
c-----------------------------------------------------------------------
c fix or unfix depth
c------------------------------------------------------------------------
c
      if(keys(1:4).eq.'FIX ') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 7729
         goto 7730                                                         
 7729    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 7730   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   flip fix flag
c
         if(data1(1)(44:44).eq.' '.or.data1(1)(44:44).eq.'S') then
            data1(1)(44:44)='F'
            write(6,*)
     *      ' Give depth to fix to, enter to keep depth in file'
            read(5,'(a)') text
            if(text.ne.' ') then
               read(text,*) depth(1)
               write(data1(1)(39:43),'(f5.1)') depth(1)
               write(6,'(a,f5.1)') 'Depth fixed to ',depth(1)
            else
               write(6,*)' Depth fixed'
            endif
         else
            data1(1)(44:44)=' '
            write(6,*)' Depth unfixed'
         endif
c  
c-- write s file
c                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif

c
c-----------------------------------------------------------------------
c fix or unfix origin time
c------------------------------------------------------------------------
c
      if(keys(1:4).eq.'FIXO') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 7429
         goto 7430                                                         
 7429    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 7430   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   flip fix flag
c
         if(data1(1)(11:11).eq.' ') then
            data1(1)(11:11)='F'
            write(6,*)' Origin time fixed'
         else
            data1(1)(11:11)=' '
            write(6,*)' Origin time unfixed'
         endif
c  
c-- write s file
c                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif

c
c-----------------------------------------------------------------------
c fix or unfix epicenter
c------------------------------------------------------------------------
c
      if(keys(1:4).eq.'FIXE') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 7479
         goto 7470                                                         
 7479    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 7470   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   flip fix flag
c
         if(data1(1)(45:45).eq.' ') then
            data1(1)(45:45)='F'
            write(6,*)' Epicenter fixed'
         else
            data1(1)(45:45)=' '
            write(6,*)' Epicenter unfixed'
         endif
c  
c-- write s file
c                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif

c
c-----------------------------------------------------------------------
c set start location flag for depth
c------------------------------------------------------------------------
c
      if(keys(1:3).eq.'STD') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 7929
         goto 7930                                                         
 7929    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 7930   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   flip start flag
c
         if(data1(1)(44:44).eq.' ') then
            data1(1)(44:44)='S'
            write(6,*)
     *      ' Give depth to start with, enter to keep depth in file'
            read(5,'(a)') text
            if(text.ne.' ') then
               read(text,*) depth(1)
               write(data1(1)(39:43),'(f5.1)') depth(1)
               write(6,'(a,f5.1)') ' Start depth set to ',depth(1)
            else
               write(6,*)' Start depth flag set'
            endif
         else
            data1(1)(44:44)=' '
            write(6,*)' Start depth flag removed'
         endif
c  
c-- write s file
c                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif


c-----------------------------------------------------------------------
c set start location flag for epicenter
c------------------------------------------------------------------------
c
      if(keys(1:3).eq.'STE') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 7829
         goto 7830                                                         
 7829    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 7830   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   flip start flag
c
         if(data1(1)(45:45).eq.' ') then
            data1(1)(45:45)='S'    
            write(6,'(a)') ' Start epicenter flag set'
         else
            data1(1)(45:45)=' '
            write(6,*)' Start epicenter flag removed'
         endif
c  
c-- write s file
c                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif


c
c-----------------------------------------------------------------------
c put in model indicator
c-----------------------------------------------------------------------
c
      if(keys(1:7).eq.'MODELIN') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 7329
         goto 7330                                                         
 7329    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
        goto 10                                                                 
c                                                                               
 7330   continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   give model indicator
c
         write(6,'(a,$)') ' Input model indicator, one character: '
         read(5,'(a)')text
         data1(1)(21:21)=text(1:1)
c  
c-- write in file
c                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif

c
c-----------------------------------------------------------------------
c put in xnear and xfar
c
      if(keys(1:6).eq.'INPUTX') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) then
           write(6,*) 'No such event'          
           keys(1:4)='SAME'              
           goto 10
         endif

        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   update line
c
         write(6,'(a)') ' Enter xnear,xfar,start depth '
         read(5,*) xnear,xfar,sdep
c
c Check for existing 'XNEAR' line type
c
          k=1
          do while (data1(k)(2:6).ne.'XNEAR'.and.k.le.nhead1)
             k=k+1
          enddo
c no location parameter record: move data array below header down 1
c only write if one of the values set
          if (xnear.ne.0..or.xfar.ne.0..or.sdep.ne.0.) then ! lo 06/05/2010
c 
           if(data1(k)(80:80).ne.'3'.and.data1(k)(2:6).ne.'XNEAR')then
            do i=nrecord1,nhead1,-1
               data1(i+1)=data1(i)
            enddo
            k=nhead1
            nrecord1=nrecord1+1
            nhead1=nhead1+1

c write xnear, xfar to new line
            data1(k)(1:80)=
     &  ' XNEAR        XFAR         SDEP         '//
     &  '                                       3'
            write(data1(k)(8:13),'(f6.1)') xnear
            write(data1(k)(20:25),'(f6.1)') xfar
            write(data1(k)(33:37),'(f5.1)') sdep
           else                   ! overwrite existing line
            write(data1(k)(8:13),'(f6.1)') xnear
            write(data1(k)(20:25),'(f6.1)') xfar
            write(data1(k)(33:37),'(f5.1)') sdep
           endif
          endif

c-- write in file                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
        do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif
c
c
c-----------------------------------------------------------------------
c duplicate header line
c---------------------------------------------------
c
      if(keys(1:2).eq.'DD') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) then
           write(6,*) 'No such event'          
           keys(1:4)='SAME'              
           goto 10
         endif

        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      

c-- write 
                            
        rewind (write01,iostat=code)
        call sei code(fort$,code,write01,b_eof)
        write(write01,'(a)') data1(1)
        do i=1,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        write(6,*)' ******* header line duplicated *******'
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif
c
c----------------------------------------------------------------------
c  print S-file 
c---------------------------------------------------------------------
c
      if(keys(1:5).eq.'PRINT') then
        if(text_print.eq.' ') then
           write(6,*) 'Printer command not defined in SEISAN.DEF'
           keys(1:4)='SAME'
           goto 10
        endif
        call systemc(text_print(1:seiclen(text_print))//' '//
     *  evfile(1:fstart+20),seiclen(text_print)+fstart+21)
         write(6,*)
     *   ' S-file being printed'
        keys(1:4)='SAME'
        goto 10
      endif

c
c----------------------------------------------------------------------
c   input comments
c---------------------------------------------------------------------
c
      if(keys(1:7).eq.'COMMENT') then
         call get_operator(operator)
c        make log file if it does not exist:
         if ( seisan_logging.GE.1 )
     +   call create_log_file(evfile,operator)
         write(6,*)
     *   ' Input comments, line by line, finish by blank line'
         i=1
 6342    continue
         read(5,'(a)') data2(i)            ! read comments
         if(data2(i).eq.' ') then
           i=i-1
           if(i.gt.0) then
              call read_s_file_e(evfile)
              do k=nrecord1, 2, -1          ! shift records to make room
                 data1(k+i)=data1(k)
              enddo
              do k=1,i
                 data1(k+1)(2:79)=data2(k)(1:78)
                 data1(k+1)(80:80)='3'
              enddo
              nrecord1=nrecord1+i
              nhead1=nhead1+1
              call write_s_file_e(evfile)
           endif
         else
           i=i+1
           goto 6342
         endif
c Logging
         if ( seisan_logging.GE.1 )
     +   call message_to_log_file(evfile,operator,6)
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------------------------------------------------
c   input felt comments
c---------------------------------------------------------------------
c
      if(keys(1:4).eq.'COMF') then
         write(6,*)
     * ' Input felt event comments, line by line, finish by blank line'
         write(6,'(a,a)')
     * 'Write under this line until end ....',
     * '..............................|'
         i=1
 7342    continue
         data2(i)=' '
         read(5,'(a)') data2(i)(12:79)            ! read comments
         if(data2(i).eq.' ') then
           i=i-1
           if(i.gt.0) then
              call read_s_file_e(evfile)
              do k=nrecord1, 2, -1          ! shift records to make room
                 data1(k+i)=data1(k)
              enddo
              do k=1,i
                 data1(k+1)=data2(k)
                 data1(k+1)(80:80)='3'
                 data1(k+1)(2:11)='FELTINFO: '
              enddo
              nrecord1=nrecord1+i
              nhead1=nhead1+1
              call write_s_file_e(evfile)
           endif
         else
           i=i+1
           goto 7342
         endif
         keys(1:4)='SAME'
         goto 10
      endif
c----------------------------------------------------------------------
c   input intensity comments
c---------------------------------------------------------------------
c
      if(keys(1:4).eq.'COMT') then
         write(6,*)
     * ' Ingrese INTENSIDAD, line by line, finish by blank line'
         write(6,'(a,a)')
     * 'Write under this line until end ....',
     * '..............................|'
         i=1
 9342    continue
         data2(i)=' '
         read(5,'(a)') data2(i)(12:79)            ! read comments
         if(data2(i).eq.' ') then
           i=i-1
           if(i.gt.0) then
              call read_s_file_e(evfile)
              do k=nrecord1, 2, -1          ! shift records to make room
                 data1(k+i)=data1(k)
              enddo
              do k=1,i
                 data1(k+1)=data2(k)
                 data1(k+1)(80:80)='3'
                 data1(k+1)(2:11)='FELTINTE: '
              enddo
              nrecord1=nrecord1+i
              nhead1=nhead1+1
              call write_s_file_e(evfile)
           endif
         else
           i=i+1
           goto 9342
         endif
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------------------------------------------------
c   input province comments
c---------------------------------------------------------------------
c
      if(keys(1:4).eq.'COMP') then
         write(6,*)
     * ' Input province'
         write(6,'(a,a)')
     * 'Write under this line until end ....',
     * '..............................|'
         data2(1)=' '
         read(5,'(a)') data2(1)(12:79)            ! read comments
         if(data2(1).ne.' ') then
              call read_s_file_e(evfile)
              do k=nrecord1, 2, -1          ! shift records to make room
                 data1(k+1)=data1(k)
              enddo
              data1(k+1)=data2(k)
              data1(k+1)(80:80)='3'
              data1(k+1)(2:11)='FELTPROV: '
             nrecord1=nrecord1+1
             nhead1=nhead1+1
             call write_s_file_e(evfile)
           endif
         keys(1:4)='SAME'
         goto 10
      endif


c----------------------------------------------------------------------
c   input locality line
c---------------------------------------------------------------------
c
      if(keys(1:4).eq.'COML') then
         write(6,*)
     * ' Input loality, line by line, finish with blank line'
         write(6,'(a,a)')
     * 'Write under this line until end ....',
     * '..............................|'
         i=1
 8342    continue
         data2(i)=' '
         read(5,'(a)') data2(i)(12:79)            ! read comments
         if(data2(i).eq.' ') then
           i=i-1
           if(i.gt.0) then
              call read_s_file_e(evfile)
              do k=nrecord1, 2, -1          ! shift records to make room
                 data1(k+i)=data1(k)
              enddo
              do k=1,i
                 data1(k+1)=data2(k)
                 data1(k+1)(80:80)='3'
                 data1(k+1)(2:11)='LOCALITY: '
              enddo
              nrecord1=nrecord1+i
              nhead1=nhead1+1
              call write_s_file_e(evfile)
           endif
         else
           i=i+1
           goto 8342
         endif
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------------------------------------------------
c   input explosion data 
c---------------------------------------------------------------------
c
      if(keys(1:3).eq.'EXP') then
         write(6,'(a,a)')
     *   ' Inputting data for a known explosion,',
     *   ' event will be classified as an explosion'
c
c   read s-file
c
         call read_s_file_e(evfile) 
c
c   shift records 3 lines to make room
c
         do k=nrecord1, 2, -1          
            data1(k+3)=data1(k)
         enddo
c
c   classify main event entry as explosion
c
         data1(1)(23:23)='E'
c
c   enter agency, to be put in in 1 and second explosion line
c
         write(6,'(a,a)')
     *   ' Enter agency for explosion, 3 characters,',
     *   ' return for none and OTH will be used'
         read(5,'(a)') text
         data1(2)=' '                             ! first exp line
         data1(2)(23:23)='E'
         data1(2)(22:22)=data1(1)(22:22)          ! same type as in first line
         data1(2)(80:80)='1'
         data1(2)(41:43)='0.0'
         if(text(1:3).eq.' ') text(1:3)='OTH'     ! unknown agency
         data1(2)(46:48)=text(1:3)
c
c   enter info, could be ok from header line so start with that
c
         data1(2)(1:13)=data1(1)(1:13)
         write(6,'(a)') ' Input info for explosion line'
         write(6,'(a,a)') ' Type below some or all data below fields,',
     *   ' field starts with - or upper case chr'
         write(6,'(a,a,a)') ' Original information remain ',
     *              'unless new is put in,',' text is deleted with _'
         write(6,'(a)')' Original data is shown below text line'
         write(6,'(a,a)') 
     *              ' YEAR MODA HRMI SECC LE-Latitu-Longitu-Dept ',
     *              ' AGA'

         
         write(6,'(a)') data1(2)(1:48)
cc         if(pc)read(5,'(a)')text(2:79)
cc         if(sun.or.linux)
           read(5,'(a)')text(1:79)
c
c   only non blank fields are put in
c

         do i=1,48
             if(text(i:i).ne.' ') data1(2)(i:i)=text(i:i)
             if(text(i:i).eq.'_') data1(2)(i:i)=' '
         enddo
                        




c         write(6,*)
c    *   ' Enter latitude and longitude, return for none'
c         read(5,'(a)') text
c
c         if(text.ne.' ') then
c            call sei get values( 2, text, code )   ! Extract 2 values.        
c            code = e_ok$                           ! re-store.
c            dlat = array$(1)                       ! Retrieve.
c            dlon = array$(2)                       ! Ditto.
c            write(data1(2)(24:38),'(f7.3,f8.3)')   
c         endif
c
         data1(3)=data1(2)
         data1(3)(78:80)='E13'               ! special explosion line
c
c   last explosion line
c
         data1(4)=' '
         data1(4)(78:80)='EC3'
         data1(4)(2:11)='CHARGE(T):'
         write(6,*)' Enter charge in ton, return for unknown'
         read(5,'(a)') text
c
c   get charge
c
         if(text.ne.' ') then                 
            call sei get values( 1, text, code )   ! Extract 2 values.        
            code = e_ok$                           ! re-store.
            dlat = array$(1)                       ! Retrieve.
            write(data1(4)(12:20),'(f9.3)') dlat
         endif
c
c  comments
c
         write(6,*)
     *   ' Enter comments, max 56 characters......................'
         read(5,'(a)') text
         data1(4)(22:77)=text(1:56)  
c
c   write event out again
c
         nrecord1=nrecord1+3
         nhead1=nhead1+3
         call write_s_file_e(evfile)
c
         keys(1:4)='SAME'
         goto 10
      endif

c
c------------------------------------------------------------------------
c Input a whole header type line
c------------------------------------------------------------------------
c
      if(keys(1:8).eq.'INPUTONE') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 1529
         goto 1530                                                         
 1529    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
         goto 10                                                                
c
 1530   continue                                                                
         write(6,'(a)') ' Input type 1 line'
         write(6,'(a,a)') 
     *' Type below some or all info below fields,',
     *   ' field starts with - or capital letter'
         write(6,'(a,a)') 
     *' YEAR MODA HRMI SECC LE-Latitu-Longitu-DeptFF',
     *              'AGA-NS-RMS MAGLXXX MAGLXXX MAGlXXX'
cc         if(pc)read(5,'(a)')text(2:79)
cc         if(sun.or.linux)
           read(5,'(a)')text(1:79)
c        read(5,'(a)')text(2:79)
         text(1:1)=' '
         text(80:80)='1'
c                                                                               
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c-- write in file                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
          write(write01,'(a)',iostat=code) data1(1)
          call sei code(fort$,code,write01,b_eof)
          write(write01,'(a)',iostat=code) text
          call sei code(fort$,code,write01,b_eof)
        do i=2,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
          keys(1:4)='SAME'
        goto 10
      endif
c
c
c----------------------------------------------------------------------
c  set fps prime location and quality
c---------------------------------------------------------------------
c
      if(keys(1:2).eq.'FQ') then

         call read_s_file_e(evfile)
c
c   display fps solutions
c
         i=0
         write(6,*)' Fault plane solutions for this event'
         do k=2,nhead1
           if(data1(k)(80:80).eq.'F') then
              i=i+1
              write(6,'(i1,1x,a)') i,data1(k)(2:79)
              data2(i)=data1(k) ! save for reorganization
           endif
         enddo
         if(i.eq.0) then
            write(6,*)' No fps solutions'
            keys(1:4)='SAME'
            goto 10
         endif
c
         write(6,*)
     *   ' Give fps number to be prime solution, enter for no change'
         read(5,'(a)') text
         if(text.eq.' ') then
            keys(1:4)='SAME'
            goto 10
         else
            read(text,*) n
            write(6,*) 
     *     ' Give quality of prime solution, any one char, e.g. A, B..'
            read(5,'(a)') text
c
c   reorgnize data so fps at end of headers but before type 7 line if there
c
            l=1
            do k=2,nhead1-1
               if(data1(k)(80:80).ne.'F') then  ! skip fps
                 l=l+1
                 data1(l)=data1(k)
               endif
            enddo
c
c  put in fps in right order, write prime first
c
            l=l+1
            data1(l)=data2(n)
            data1(l)(78:78)=text(1:1)   ! quality
            if(i.gt.1) then             ! put in rest of solutions
c
               do k=1,i
                  if(k.ne.n) then       ! now skip prime
                     l=l+1
                     data1(l)=data2(k)
                  endif
               enddo
            endif
c
c   write out data again
c  
            call write_s_file_e(evfile)
         endif
c
         keys(1:4)='SAME'
         goto 10
      endif        
              
c

c
c
c----------------------------------------------------------------------
c    find difference between two fps solutions
c---------------------------------------------------------------------
c
      if(keys(1:2).eq.'FD') then

         call read_s_file_e(evfile)
c
c   display fps solutions
c
         i=0
         write(6,*)
         write(6,*)' Fault plane solutions for this event'
         do k=2,nhead1
           if(data1(k)(80:80).eq.'F') then
              i=i+1
              write(6,'(i1,1x,a)') i,data1(k)(2:79)
              data2(i)=data1(k) ! save for reading mechanisms
           endif
         enddo
         n=i

         if(i.lt.2) then
            write(6,*)' One or no fps solutions'
            keys(1:4)='SAME'
            goto 10
         endif
c
         write(6,*)
     *   ' Give two fps numbers to compare P and T'
         read(5,'(a)') text
         if(text.eq.' ') then
            keys(1:4)='SAME'
            goto 10
         else
            read(text,*) n1,n2
            if(n1.gt.n.or.n2.gt.n.or.n1.lt.1.or.n2.lt.1) then
               write(6,*) ' Wrong number(s)'
               keys(1:4)='SAME'
               goto 10
            endif
         endif

c
c   read 2 fps solutions
c
         read(data2(n1)(1:30),'(3f10.1)') strike1,dip1,rake1
         read(data2(n2)(1:30),'(3f10.1)') strike2,dip2,rake2
c
c   calculate difference
c
         call p_t_gap
     *   (strike1,dip1,rake1,strike2,dip2,rake2,delp,delt)
         write(6,*)
         write(6,'(a,2f6.1)')' Difference in P and T, respectively ',
     *   delp,delt
         write(6,*)
         keys(1:4)='SAME'
         goto 10
      endif      
c
c------------------------------------------------------------------------
c Input fps type line
c------------------------------------------------------------------------
c
      if(keys(1:7).eq.'INPUTFP') then
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
         if (code .ne. e_ok$) go to 1577
         goto 1578                                                         
 1577    continue                                                              
         write(6,*) 'No such event'                                            
         keys(1:4)='SAME'                                                       
         goto 10                                                                
c
 1578   continue
         if(keys(8:8).eq.'S') then
            write(6,*) 'strike,dip,rake'
            read(5,*) strike,dip,rake
c
c  agency
c
c            if(fps_agency.eq.' ') then
c 4444          continue

               write(6,*) 'Give agency, max 3 chars'
               read(5,'(a)') fps_agency

c            else
c               write(6,'(a,a,a)') 'Previous agency was ',fps_agency,
c     *         ' enter to use, else give new agency'
c               read(5,'(a)') text
c               if(text.ne.' ') then
c                  fps_agency=text(1:3)
c               else
c                  goto 4444
c               endif
c            endif
c
c   program
c           
c            if(fps_program.eq.' ') then
               write(6,*) 'Give fps program, max 7 chars'
               read(5,'(a)') fps_program
c            else
c               write(6,'(a,a,a)') 'Previous fps program was ',
c     *         fps_program,
c     *         ' enter to use, else give new fps program'
c               read(5,'(a)') text
c               if(text.ne.' ') fps_program=text(1:7)
c            endif
c
c   quality
c
c            if(fps_quality.eq.' ') then
               write(6,*) 'Give quality, max 1 char'
               read(5,'(a)') fps_quality
c            else
c               write(6,'(a,a,a)') 'Previous quality was ',fps_quality,
c     *         ' enter to use, else give new agency'
c               read(5,'(a)') text
c               if(text.ne.' ') fps_quality=text(1:1)
c            endif
            text=' '
            write(text,'(3f10.1)') strike,dip,rake
            text(67:69)=fps_agency
            text(71:77)=fps_program
            text(78:78)=fps_quality
            text(80:80)='F'
            goto 2837    ! write out
         endif          
                                                                
         write(6,'(a)') ' Input fps line'
         write(6,'(a)') 
     *' Type below some or all info below fields, fields end with name'
         write(6,'(a)')
     *' All numbers until N are real and must contain a decimal point'
         write(6,'(a)')
     *' In addition to strike, dip and rake, most important are:'
         write(6,'(a)') 
     *' ERRS-ERRD: Errors in strike dip and rake, N: # bad polarities'
         write(6,'(a)')    
     *' AGA: Agency code, Q: Quality (A-F), others, see manual'

         write(6,'(a,a)')
     *  '    STRIKE       DIP      RAKE ERRS ERRD ERRR ERRF DRAT',
     *  ' AFIT N NA AGA PROG.. Q'
     
cc         if(pc)read(5,'(a)')text(2:79)
cc         if(sun.or.linux)
           read(5,'(a)')text(1:79)
c        read(5,'(a)')text(2:79)
         text(1:1)=' '
         text(80:80)='F'
c    
c  get here from fast input
c
 2837   continue                                                                           
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c-- write in file                            
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
          write(write01,'(a)',iostat=code) data1(1)
          call sei code(fort$,code,write01,b_eof)
          write(write01,'(a)',iostat=code) text
          call sei code(fort$,code,write01,b_eof)
        do i=2,nrecord1
          write(write01,'(a)',iostat=code) data1(i)
          call sei code(fort$,code,write01,b_eof)
        enddo
        call sei close (close$,write01,code)
        keys(1:4)='SAME'
        goto 10
      endif
c
c------------------------------------------------------------------------
c iaspei phase calculation
c------------------------------------------------------------------------
      if(keys(1:4).eq.'IASP') then
         call systemc("iasp",4)
         keys(1:4)='SAME'
         goto 10
      endif

c
c------------------------------------------------------------------------
c  delete some lines in s-file
c------------------------------------------------------------------------
      if(keys(1:4).eq.'DELS') then
         txt='dels '//evfile(1:fstart+20)//' overwrite '
     *   //operator
         call systemc(txt,seiclen(txt))
         keys(1:4)='SAME'
         goto 10
      endif


c------------------------------------------------------------------------
c wadati diagram
c------------------------------------------------------------------------
c
      if(keys(1:3).eq.'WAD') then
         call systemc('wad_plot '//evfile(1:fstart+20),9+20+fstart)
         keys(1:4)='SAME'
         goto 10
      endif
c------------------------------------------------------------------------
c travel time plot
c------------------------------------------------------------------------
c
      if(keys(1:6).eq.'TTPLOT') then
         call systemc('ttplot '//evfile(1:fstart+20),7+20+fstart)
         keys(1:4)='SAME'
         goto 10
      endif


c------------------------------------------------------------------------
c array analysis with P arrivals
c------------------------------------------------------------------------
c
      if(keys(1:2).eq.'PF') then
         call systemc('pfit '//evfile(1:fstart+20),5+20+fstart)
c
c   put result into S-file
c   put on first P for reference station
c
         call read_s_file_e(evfile)
c
c   open and read result from array analysis
c
         open(80,file='array.out',err=9678)
         goto 9679
 9678    continue
         write(6,*)' No array.out file, no result'
         goto 9680
 9679    continue
           read(80,'(a34,a5,1x,2f10.3)',err=9680) 
     *     text(1:34),ref_station,baz,appvel
           if(text(3:9).eq.'No data') goto 9680
           write(6,*)' Update event with new values(y/n) ?'
           read(5,'(a)') answer
           if(answer.eq.'y') then
              do i=nhead1+1,nrecord1-1
c               write(6,*) ref_station,data1(i)(2:6),
c    *          data1(i)(11:11)
                if(data1(i)(2:6).eq.ref_station.and.
     *             data1(i)(11:11).eq.'P') then
                   write(6,*)' Found reference station, updating'
                   write(data1(i)(47:56),'(f5.0,1x,f4.1)') 
     *             baz,appvel
                   call write_s_file_e(evfile)
                   goto 9681
                endif
              enddo
           endif
 9681      continue
 9680    continue
         close(80)
         keys(1:4)='SAME'
         goto 10
      endif

c------------------------------------------------------------------------
c isoseismal calculation and effect input
c------------------------------------------------------------------------
      if(keys(1:4).eq.'PMAC') then
        if(.not.pc) then
          write(6,*)' Program only works on PC'
        else
          text='promac ' // evfile(1:fstart+20)
          call systemc(text,fstart+27)
        endif
        keys(1:4)='SAME'
        goto 10
      endif

c
c-----------------------------------------------------------------------
c copy many events
c-----------------------------------------------------------------------
c
      if(keys(1:2).eq.'CM') then

         current_event_number=event_no 
         write(6,*)
     * ' Copy events to eev.out, how many starting from current ?'
         read(5,*) l
c
c   open eev.out
c
           chr_f_access$ = 'append'
              call sei open(unknown$,              ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'eev.out',             ! File name
     &                      write03,               ! Write unit #3
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.

c
c  copy
c

         do i=1,l
            if(i.eq.1) then
               keys(1:4)='SAME'   ! start with current event
            else
               keys(1:4)='NEXT'
            endif

            call findevin
     *      (base_name,start_time,end_time,keys,from_eev,event_no,
     *      evfile,fstart,new_month,status)
c
c   check if not end of list
c
            if(i.gt.1.and.event_no.eq.1) then
               write(6,*)' Reached end of list, stop copy at # ',i
               goto 3746   
            endif
            call read_s_file_e(evfile)
            write(write03,'(a)') (data1(k),k=1,nrecord1)
         enddo 
 3746    continue

         call sei code(fort$,code,write03,b_eof)
         call sei close (close$,write03,code)         

c
c   go to start event
c
         keys(1:1)='#'
         write(keys(2:5),'(i4)') current_event_number 
         call findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   evfile,fstart,new_month,status)
         keys(1:4)='SAME'
         goto 10
      endif

c
c-----------------------------------------------------------------------
c  find next new event
c-----------------------------------------------------------------------
c
      if(keys(1:2).eq.'SS') then
         if(se) then
            write(6,*)' Select new event in SE'
            keys(1:4)='SAME'
            goto 10
         endif
         current_event_number=event_no       ! save so search only done once
         keys(1:4)='NEXT'
 1010    continue
c     write(6,*)' pv: eev_sub : 909 : end time : ',end_time
         call findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   evfile,fstart,new_month,status)
c
c   check if process has read whole month or base
c
         if(event_no.eq.current_event_number) then
            write(6,*)' No new event found *********************'
            keys(1:4)='SAME'
            goto 10
         endif
c  
         call read_s_file_e(evfile) 
c
c   check if new event
c

         if((data1(id)(9:11).eq.'NEW'.or.data1(id)(9:11).eq.'SPL'
     *       .or.data1(id)(9:11).eq.'HYP'.or.
     *       data1(id)(9:11).eq.
     *       'ARG').and.
     *       data1(1)(46:48).ne.'NAO') then
            write(6,*)' New event ------------------------------'
            keys(1:4)='SAME'
            goto 10
         else
            keys(1:4)='NEXT'
            goto 1010
         endif
      endif
            
c
            
c
c------------------------------------------------------------------------
c register or put event
c------------------------------------------------------------------------
c

      if(keys(1:3).eq.'PUT'.or.keys(1:3).eq.'REG') then
c        if(se) then
c           write(6,*)' Event cannot be registered since call from SE'
c           keys(1:4)='SAME'
c           goto 10
c        endif
         
         newfile=' '   ! must be initialized since return to se
c
c   read file
c
         call read_s_file_e(evfile)
c
         call get_operator(operator)
         if ( seisan_logging.GE.1 )
     +   call create_log_file(evfile,operator)
c
c  check if any request lines, if so registration not allowed
c
         request_flag=.false.
         do i=2,nrecord1
           if (data1(i)(2:4).eq.'NET'.and.
     *       (data1(i)(76:78).eq.'REQ'.or.data1(i)(76:78).
     *       eq.'CRQ')) then
             request_flag=.true.
           endif
         enddo
c
c        if(request_flag) then
c lot 24/5/2005
         if(request_flag.and.reg_base(1:1).eq.'_') then
            write(6,*) 
     *      'You are not allowed to register, pending requests'
            write(6,*)' Enter to continue'
            read(5,'(a)') i
            request_flag=.false.
            goto 10
         endif
c


c
c   check that file not already has been registered
c
         if(id.gt.0) then
            if(data1(id)(9:11).eq.'REG'.or.data1(id)(9:11).eq.'REE'.
     *      or.data1(id)(9:10).eq.'UP' .or.data1(id)(9:11).eq.'REE'.
     *      or.data1(id)(9:11).eq.'UPD') 
     *      then
c added confirmation level, lot 25-7-2002
              if (confirmation_level.eq.1.) then
                write(6,*)' Event already registered, continue(y/n)'
                read(5,'(a)') answer
                if(answer.ne.'y'.and.answer.ne.'Y') then
                  keys(1:4)='SAME'
                  goto  10
                endif
              endif
            endif
         else
c
c add id line
c
            write(6,*)' No ID line, fixing it !!!'
            do k=nhead1+1,nrecord1,-1
              data1(k)=data1(k-1)
            enddo
            data1(nhead1+1)=data1(nhead1)
            nrecord1=nrecord1+1
            nhead1=nhead1+1
            id=nhead1-1
            read(evfile(seiclen(evfile)-18:seiclen(evfile)),
     &         '(i2,1x,i2,i2,1x,i2,3x,i4,i2)')
     &         day,hour,min,isec,year,mon
            data1(id)=
     &    ' ACTION:REE 02-11-22 00:01 OP:aut  STATUS:'//
     &    '               ID:20021122050017     I'
            call systime(p_time,proc_time)
            WRITE(data1(id)(13:26),'(A)')PROC_TIME
            write(data1(id)(61:74),'(i4.4,5i2.2)')
     &        year,mon,day,hour,min,isec

            write(*,'(a)') ' new ID line : '//data1(id)
c            write(6,*)' No ID line, fix it !!!'
         endif
c
c   check if id and file name ok
c
         if(id.ne.0) then 
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif

         write(6,*)' You are now about to register the current ',
     *   'event '
c in the data base.'
         write(6,*)
     *   ' File will be cleaned up and waveform files copied to WAV'
c only ask if confirmation wanted, 17-04-2002
         if (confirmation_level.eq.1.) then
           write(6,*)' Sure you want to register, (y/n) ?'
           read(5,'(a)') answer
           if(answer.ne.'Y'.and.answer.ne.'y') then
              keys(1:4)='SAME'
              goto 10
           endif
         endif

         write(6,*)                                                             
 1724    continue                                                               
c
c set database for registration
c
         if (seiclen(reg_base).eq.0) then
           write(6,*) ' Give 2-5 letter data base, ' //
     *       ' return for same base'
           read(5,'(a)') reg_base
           do k=1,5
             if (reg_base(k:k).eq.' ') reg_base(k:k)='_'
           enddo
         endif

         write(6,*)
     *   ' Change event type to L,R or D'
         write(6,*)
     *   ' Second character for event ID (e.g. E) and/or '//
     *   ' third for model (e.g. J) '
         write(6,*)' Return for no change ?'
         read(5,'(a3)') new_type                                                
         if(new_type(1:1).eq.'l') new_type(1:1)='L'                             
         if(new_type(1:1).eq.'r') new_type(1:1)='R'                             
         if(new_type(1:1).eq.'d') new_type(1:1)='D'                             
         if(new_type(2:2).eq.'e') new_type(2:2)='E'
         if(new_type(2:2).eq.'p') new_type(2:2)='P'
         if(new_type(2:2).eq.'v') new_type(2:2)='V'
         if(new_type(2:2).eq.'q') new_type(2:2)='Q'
         if(new_type(1:1).ne.'L'.and.new_type(1:1).ne.                         
     *   'R'.and.new_type(1:1).ne.'D'.and.new_type(1:1).ne.' ') then        
            write(6,*)' Wrong type ******'                                      
            goto 1724                                                  
         endif
         old_type=data1(1)(22:23)
         if (seiclen(new_type).le.0) new_type=old_type
c                                                                               
c   check if type is different, if so reset
c                                                  
         if(data1(1)(22:23).eq.new_type) then 
            write(6,*)' New type same as old, nothing changed'                  
         else
            if(new_type(1:1).ne.' ') data1(1)(22:23)=new_type ! put in new type
         endif
c
c put in model, lo 8/2/2011
c
         if (seiclen(new_type).eq.3) data1(1)(21:21)=new_type(3:3)

c
c if volcanic event, select subclass
c
         if (new_type(1:2).eq.'LV') then
           call select_volcano_subclass(sub_class)
c
c add line
c          
           data1(nhead1+1)=data1(nhead1)
           data1(nhead1)=' VOLC MAIN                              '
     &                //'                                       3'
           write(data1(nhead1)(12:17),'(a6)') sub_class
           nhead1=nhead1+1
         endif
c
c set database for registration
c
         if (seiclen(reg_base).eq.0) then
           write(6,*) ' Give 2-5 letter data base, ' //
     *       ' return for same base'
           read(5,'(a)') reg_base
           do k=1,5
             if (reg_base(k:k).eq.' ') reg_base(k:k)='_'
           enddo
         endif

         if((new_type(1:1).ne.' '.and.old_type.ne.
     *   new_type(1:1)).or.reg_base(1:1).ne.'_') then                                                                  
c
c   make new file name
c                                            

c
c set event file name, newfile
c


            if (seiclen(evfile).gt.19) then   ! lot 04-06-2002
c event not in local database
              newfile=evfile                   
              if (reg_base(1:1).ne.'_') then
                k=seiclen(newfile)-32
                newfile(k:k+4)=reg_base(1:5)
              endif
              newfile(fstart+10:fstart+10)=new_type(1:1) 
            else 
c event in local database
              newfile=top_directory(1:seiclen(top_directory))
     &                //dchar//'REA'//dchar//reg_base(1:5)//dchar//
     &                evfile(14:17)//dchar//evfile(18:19)//
     &                dchar//evfile(1:seiclen(evfile))
              fstart=seiclen(newfile) - 18
            endif
c
c   check if file can be opened or if it already is there
c
 7331       continue
            call sei open(unknown$+warn$,          ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      newfile,               ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
             if (code .ne. e_ok$) then                ! check 
                write(6,'(1x,a,a)')' Cannot open file: ',newfile
                keys='SAME'
                goto 10
             endif
c
c   check for duplicate event in base
c
             if(b_old) then                               ! check if file there
 7333           continue	  
                write(6,*)
                write(6,'(a)')
     *          ' File already exists in base, options are:'
                write(6,'(a)')
     *          ' Do not change type:                        Return'
                write(6,'(a)')
     *          ' Overwrite existing event:                       O'
                if(id.ne.0) write(6,'(a)')
     *          ' Create a new event in base, different ID:       N'
                read(5,'(a)') choise
                if(choise.eq.' ') then
                   write(6,*)' File type will not be changed'
                   keys(1:4)='SAME'
                   goto 10
                endif
                if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                   call inc_id(data1(id),newfile,fstart+18)
                   call sei close (close$,write01,code) ! close, a new will be tested
                   goto 7331                            ! test again
                endif
                if(choise.eq.'o'.or.choise.eq.'O') then
                   write(6,*)' Overwriting old event'
                   goto 8155   ! go to write out
                endif
                goto 7333                                ! no valid choise
             endif
c
c    write out
c
 8155    continue             ! from just above
               write(write01,'(a80)',iostat=code)
     *         (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               call sei close (close$,write01,code)
c
c check for request
c
              request_flag=.false.
              do i=2,nrecord1
                if (data1(i)(2:4).eq.'NET'.and.
     *            (data1(i)(76:78).eq.'REQ'.or.
     *             data1(i)(76:78).eq.'CRQ')) then
                  request_flag=.true.
                endif
              enddo

              write(6,'(a,a)')' New file       ', newfile(1:fstart+20)
              if (reg_base(1:1).ne.'_'.and.      ! lot 07-05-2002
     &            request_flag) then
c
c keep old file with Seisnet request lines only
c
                chr_file = evfile(1:fstart+20)
                call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
                do i=1,nrecord1
                  if ((data1(i)(2:4).eq.'NET'.and.
     &                (data1(i)(76:78).eq.'REQ'.or.
     &                 data1(i)(76:78).eq.'CRQ')).or.
     &                 data1(i)(80:80).eq.'1'.or.
     &                 data1(i)(80:80).eq.'I'.or.
     &                 data1(i)(80:80).eq.'7') then
                    if (data1(i)(80:80).eq.'1')then
                       data1(i)(22:22)=chr_file
     &       (seiclen(chr_file)-8:seiclen(chr_file)-8)
                    endif
                  write(write01,'(a80)') data1(i)

                  endif
                enddo
                write(6,'(a,a)')' Keeping file:  ', evfile(1:fstart+20)                 
                call sei close (close$,write01,code)

              else
                                                                                
c                                                                               
c   delete old event if no Seisnet request lines
c
                chr_file = evfile(1:fstart+20)
                call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
                call sei close (delete$,write01,code)
                write(6,'(a,a)')' Deleted file:  ', evfile(1:fstart+20)                 
              endif
c                                                                               
c   update event list                                                           
c                                                                               
              keys(1:3)='REN'                                                        
              CALL findevin
     *        (base_name,start_time,end_time,keys,from_eev,event_no,
     *        evfile,fstart,new_month,status)                                       
  
              evfile=newfile   ! new file name
c
         endif
c
c  copy waveform files to wav
c
         do k=2,nhead1
            if(data1(k)(80:80).eq.'6'.AND.data1(k)(2:4).NE.'BUD'.AND
     &      .data1(k)(2:4).NE.'BUD') then
                wave_file=' '
                txt=' '
                read(data1(k)(2:79),'(a)') wave_file(1:78)
             
c
c get waveform header info
c
                call wav_init
                call get_full_wav_name(wave_file,wav_filename(1))
              if(wav_filename(1).eq.' ') then
                  write(*,*) ' File not found: '//
     &              wav_filename(1)(1:seiclen(wav_filename(1)))

              else
                call read_wav_header(1)
                write(cwavyear,'(i4)') wav_year(1)
                write(cwavmon,'(i2)') wav_month(1)
                do i=1,2
                  if (cwavmon(i:i).eq.' ') cwavmon(i:i)='0' 
                enddo
  
                if( pc ) then
                  txt  = 'copy X' 

c                txt  = 'copy ' // wave_file(:seiclen(wave_file)) 
c     &                 // ' '
c     &                 // top_directory(:seiclen(top_directory)) 
c     &                 // dchar//'WAV'//dchar                    
c     &                 // wave_file(:seiclen(wave_file))
c
                else if( sun.or.linux ) then
                  txt  = 'cp   X' 
                else
                  chr_err_msg$ = 
     &'**** ERROR: could not determine the computer type'
                  call sei code( stop$, e_init$, 0, b_flag ) ! Halt program
                end if                                     !

                if(copy_wav_dir.eq.' ') then     ! allow to copy to database, same as in mulplt, lot 11-4-2002

                  txt = txt(1:seiclen(txt)-1) // 
     &                wav_filename(1)(:seiclen(wav_filename(1))) //
     &                ' '                                    //
     &                top_directory(:seiclen(top_directory)) //
     &                dchar//'WAV'//dchar                    //
     &                wave_file(:seiclen(wave_file))
                else

                  txt = txt(1:seiclen(txt)-1) //
     &                wav_filename(1)(:seiclen(wav_filename(1))) //
     &                ' '                                    //
     &                top_directory(:seiclen(top_directory)) //
     &                dchar//'WAV'//dchar                    //
     &                copy_wav_dir//dchar//cwavyear(1:4)//dchar //
     &                cwavmon(1:2)//dchar //
     &                wave_file(:seiclen(wave_file))
                endif


                write(6,'(1x,a)') txt                      ! show file name
c
c  check if file already there
c
                call sei open( check$,              ! Check file exists.
     &                    ' ',                 ! No prompt.
     &                    txt(7+seiclen(wav_filename(1)):seiclen(txt)),
     &                    0,                   ! Unit (n/a).
     &                    exist,               ! File exists?.
     &                    code )               ! Condition (n/a).
                if(exist) then
                   write(6,*)' File already transferred to WAV *****'
                endif
c
c now copy if not already there
c
                if(.not.exist) then
                   call systemc( txt,
     &                         seiclen(txt) )
c
c  check that file got there
c
                   call sei open( check$,        ! Check file exists.
     &                    ' ',                 ! No prompt.
     &                    txt(7+seiclen(wav_filename(1)):seiclen(txt)),
     &                    0,                   ! Unit (n/a).
     &                    exist,               ! File exists?.
     &                    code )               ! Condition (n/a).
                   if(exist) then
                      write(6,*)' File transferred to WAV **********'
                   else
                      write(6,*)' Failure to transfer to WAV ****'
                      write(6,*)' Return to continue'
                      read(5,'(a4)') i
                   endif
                   write(6,*)
                endif                          
              endif
            endif
         enddo 
c
c   check if current operator id given
c
         call get_operator(operator)
c 4723       continue
c            if(operator.eq.'    ') then
c               write(6,*) 'Give operator code, max 4 characters'
c               read(5,'(a)') operator
c               goto 4723
c            endif
c
c   get system time
c
            call systime(p_time,proc_time)
c
c   update id line
c
            if(id.gt.0) then
               WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
               WRITE(data1(id)(13:26),'(A)')PROC_TIME
               WRITE(data1(id)(9:11),'(A)')'REE'
            else
               write(6,*)' No ID line !!!!!!!'
            endif
c
c   clean up junk lines from seisnet, automatic picks and old header lines
c
            k=2
            data2(1)=data1(1)
            if(new_type(1:1).ne.' ') data2(1)(22:23)=new_type ! put in new type, lot 17-10-2002
            data2(2)=data1(id)
            if (data2(1)(45:45).eq.'*') data2(1)(45:45)=' '
            do i=2,nrecord1

              if( (data1(i)(2:4).eq.'NET'.and.data1(i)(80:80).eq.'3'.
     *             and.data1(i)(76:78).ne.'REQ'.
     *             and.data1(i)(76:78).ne.'CRQ') 
     *        .or. data1(i)(80:80).eq.'I'.or. 
     *            (data1(i)(80:80).eq.'1'.and.data1(i)(46:48).ne.'PDE')
     *        .or.(data1(i)(80:80).eq.' '.and.data1(i)(16:16).eq.'A'
     *               .and.keep_auto.eq.0.)
     *        .or.(data1(i)(2:6).eq.'SPECA')
     *        .or.(data1(i)(80:80).eq.'3'.and.data1(i)(2:7).
     *         eq.'ACTION'))
     *        then
                 continue    ! skip line
              else
                 k=k+1
                 data2(k)=data1(i)
                 if(data1(i)(76:78).eq.'REQ'.
     *           or.data1(i)(76:78).eq.'CRQ') then
                    write(6,*) ' This event contains a SEISNET',
     *              ' request line, is NOT deleted'
                 endif
              endif
            enddo
            data2(k+1)=' '
            nrecord1=k+1
            do i=1,nrecord1
              data1(i)=data2(i)
            enddo
            call write_s_file_e(evfile)

c
c   signal a new file to se
c
           call put_seisan_message(newfile)
c
c   optinally start a process
c
            if(reg_autoprocess_flag.eq.2) then
               write(6,'(a,a)') ' Run process(y/return=n)? '
     *            ,reg_autoprocess_name
               call flush (6)
               read(5,'(a1)') answer
               if(answer.eq.'y'.or.answer.eq.'Y') then
                   call systemc(reg_autoprocess_name,10)
               endif
            elseif (reg_autoprocess_flag.eq.1) then
               write(*,*) ' running '//reg_autoprocess_name
               call systemc(reg_autoprocess_name,10)
            endif

c Logging
         if ( seisan_logging.GE.1 )
     +   call message_to_log_file(evfile,operator,2)

         keys(1:4)='SAME'
         goto 10
      endif


c------------------------------------------------------------------------
c polarity plot
c------------------------------------------------------------------------
      if(keys(1:3).eq.'POL') then
        call systemc('plotpolarity '//evfile(1:seiclen(evfile))//' all',
     &     13+seiclen(evfile)+4)
         keys(1:4)='SAME'
         goto 10
      endif
c------------------------------------------------------------------------
c rmsdep calculation
c------------------------------------------------------------------------
      if(keys(1:6).eq.'RMSDEP') then
         call systemc("rmsdep",6)
         keys(1:4)='SAME'
         goto 10
      endif

c----------------------------------------------------------------
c
c  quarry check command  (JE Aug2002)
c-----------------------------------------------------------------
c
c      if(keys(1:6).eq.'QUARRY'.and.extension.eq.'BGS') then
      if(keys(1:6).eq.'QUARRY') then
         write(6,*) 'Checking '//evfile(1:seiclen(evfile))//
     &      ' against quarry.dat ' 
         chr_file = evfile(1:seiclen(evfile))
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       read01,                ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.

         call indata
     *      (read01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
         call sei close (close$,read01,code)
c         call quarrycheck(evfile)
c         write(*,*) data1(1)(24:38)
         read(data1(1)(24:38),'(f7.3,f8.3)') elat,elon
         call quarry_check(elat,elon,quarry,nquarry)
         do k=1,nquarry
           if (k.eq.1) write(*,*) ' dist,lat,lon,text '
           write(*,*) quarry(k)(1:seiclen(quarry(k)))
         enddo
         goto 10
      endif
c
c------------------------------------------------------------------------
c Google earth or map turned on or off when locating
c------------------------------------------------------------------------
c
       if(keys(1:4).eq.'GHYP') then
          if(ghyp) then
             ghyp=.false.
             if(googleearth) then
               write(6,*) 'Google Earth turned off when locating'
             else
               write(6,*) 'Google Map turned off when locating'
             endif
          else
             ghyp=.true.
             write(6,*)'Use google Earth (e) or Google Map (enter)'
             googleearth=.false.
             googlemap=.false.
             read(5,'(a)') text
             if(text.eq.' ') then
                googlemap=.true.
             else
                googleearth=.true.
             endif
             if(googleearth) then
                write(6,*) 'Google Earth turned on when locating'
             else
                write(6,*) 'Google Map turned on when locating'
             endif
          endif
         keys(1:4)='SAME'
         goto 10
       endif
c
c------------------------------------------------------------------------
c gmap
c------------------------------------------------------------------------
c

 4488  continue                        ! from location
       if(keys(1:4).eq.'GMAP') then
c
c check for location in S-file first. it could be an existing s-file
c in data base or the hyp.out from a location
c
         if(from_hyp) then
           evfile='hyp.out'
         else 
           call put_env_event(evfile)
         endif
         from_hyp=.false.

         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) go to 4724

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)

         call sei close (close$,write01,code)

         read(data1(1)(24:30),'(f7.3)') map_lat_c
         read(data1(1)(31:38),'(f8.3)') map_lon_c
c        write(*,*)"Location is generated for maps.google.com"
         write(*,'(a47,1x)')
     *   "Location map is generated for maps.google.com"
c        write(*,*)"          open gmap.html with you browser"

         if (map_lat_c.ne.0.0.and.map_lon_c.ne.0.0) then

               call sei open(unknown$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       'gmap.html',             ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition

           write(write01,'(a6)')"<HTML>"
           write(write01,'(a6)')"<HEAD>"
           txt(1:40)='<meta HTTP-EQUIV="REFRESH" content="0; '
           txt(41:78)='url=http://www.google.com/maps?q='
           i=79
  
           do k=24,30
           if(data1(1)(k:k).ne." ")then
             txt(i:i)=data1(1)(k:k)
             i=i+1
           endif
           enddo

           txt(i:i)="+"
           i=i+1

           do k=31,38
           if(data1(1)(k:k).ne." ")then
             txt(i:i)=data1(1)(k:k)
             i=i+1
           endif
           enddo

           txt(i:i)="("
           i=i+1

           do k=2,80
             if(data1(1)(k:k).ne." ")then
               txt(i:i)=data1(1)(k:k)
               i=i+1
             else
               txt(i:i+2)="%20"
               i=i+3
             endif
           enddo
           txt(i:i+4)=")&ll="
           i=i+5

           do k=24,30
           if(data1(1)(k:k).ne." ")then
             txt(i:i)=data1(1)(k:k)
             i=i+1
           endif
           enddo

           txt(i:i)=","
           i=i+1

           do k=31,38
           if(data1(1)(k:k).ne." ")then
             txt(i:i)=data1(1)(k:k)
             i=i+1
           endif
           enddo

c jens GMAP_SIZE is added here WIDHT and MAP HEIGTH is hard code to 2,2 (see spn=2,2):
           txt(i:i+13)='&spn=2,2&t=h">'
           if(gmap_type(1:4).eq.'MAP') txt(i:i+13)='&spn=2,2&t=m">'
           if(gmap_type(1:9).eq.'SATELLITE') 
     *          txt(i:i+13)='&spn=2,2&t=k">'
           if(gmap_type(1:6).eq.'HYBRID') txt(i:i+13)='&spn=2,2&t=h">'
           if(gmap_type(1:7).eq.'TERRAIN') txt(i:i+13)='&spn=2,2&t=p">'
           i=i+1

           write(write01,'(a400)') txt
           write(write01,'(a7)')"</HEAD>"
           write(write01,'(a7)')"</HTML>"
c          write(*,*)i

           call sei close (close$,write01,code)
c          call systemc("firefox gmap.html",17)
c
c   GMAP_DIR is added here if a directory is given
cxx
c           if(gmap_dir.ne.' ') then
c           k=sei clen(gmap_dir)
c           if(.not.pc)
c     *     call systemc("cp gmap.html "//gmap_dir(1:k),13+k)
c           if(pc)
c     *     call systemc("copy gmap.html "//gmap_dir(1:k),15+k)
c           endif

c           write(*,*)' Open gmap.html with your favorite browser'
c           if(gmap_dir.eq.' ') then
c             write(6,*) ' you will find it in working directory'
c           else
c             write(*,*)' you will find it here : ',gmap_dir(1:k)
c           endif

           if(pc) then 
              call systemc('start gmap.html',15)
           elseif(linux) then
              call systemc('xdg-open gmap.html &',20)
           endif

            keys(1:4)='SAME'
           
            goto 10

         else
           write(*,*) ' sorry but you need to have a location !'
           goto 4724
         endif
       endif
c
c-----------------------------------------------------------------
c    email
c-----------------------------------------------------------------
c
       if(keys(1:6).eq.'EMAIL ') then
         write(*,*) ' Sending email to the following address: '
         do k=1,n_alertemail
           write(*,'(2x,i2,1x,a)') k,alertemail(k) 
         enddo
         write(*,*) ' Really send email (y/n) ?'
         read(5,'(a1)') answer
         if (answer.eq.'y') then
           do k=1,n_alertemail
             call systemc(
     &            mailx(1:seiclen(mailx))//" "//
     &            alertemail(k)(1:seiclen(alertemail(k)))//"<"//
     &            evfile(1:seiclen(evfile)),
     &       seiclen(mailx)+seiclen(alertemail(k))+seiclen(evfile)+2)
           enddo
c          make log file if it does not exist:
           if ( seisan_logging.GE.1 )
     +     call create_log_file(evfile,operator)
c          Log the action:
           if ( seisan_logging.GE.1 )
     +     call message_to_log_file(evfile,operator,7)
         endif
         goto 10
       endif

       if(keys(1:3).eq.'IC ') then
         if (n_eev_comments.gt.0) then
           write(*,'(a)') 'Predefined comments are: '
           write(*,'(a)') '------------------------ '
           do k=1,n_eev_comments
             write(*,'(i2,1x,a)') k,eev_comments(k)
           enddo
           write(*,'(a)') '------------------------ '
           write(*,'(a)') 'Enter number, 0 to abort '
           read(5,*) k
           if (k.gt.0.and.k.le.n_eev_comments) then
             chr_file = evfile(1:seiclen(evfile))
             call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists?  (n/a).
     &                      code)                  ! Returned condition.
c
c   read all parameters for one event from file unit 1
c
             call rea_event_in(write01,.true.,data1,code)
             call sei close (close$,write01,code) 

c add comments
             rea_ncomment=rea_ncomment+1
             rea_comment(rea_ncomment)=' '
             do i=1,80
               rea_comment(rea_ncomment)(i:i)=' '
             enddo
             write(rea_comment(rea_ncomment)(2:),'(a)')
     &  eev_comments(k)(1:seiclen(eev_comments(k)))
             rea_comment(rea_ncomment)(80:80)='3'

             chr_file = evfile(1:seiclen(evfile))
             call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists?  (n/a).
     &                      code)                  ! Returned condition.
c
c   read all parameters for one event from file unit 1
c
             call rea_event_out(write01,.true.,data1,code)
             call sei close (close$,write01,code) 

           endif
         else
           write(*,*) ' no comments defined, add to DAT/SEISAN.DEF '
         endif
         goto 10
       endif


c
c------------------------------------------------------------------------
C
c------------------------------------------------------------------------
c epimap old
c------------------------------------------------------------------------
 
       if(keys(1:4).eq.'MAP '.or.keys(1:4).eq.'MAPF') then
 
         call put_env_event(evfile)
c
c check for location in S-file first
c
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) go to 4724

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)

         call sei close (close$,write01,code)

         read(data1(1)(24:30),'(f7.3)') map_lat_c
         read(data1(1)(31:38),'(f8.3)') map_lon_c

         if (map_lat_c.ne.0.0.and.map_lon_c.ne.0.0) then

               call sei open(unknown$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       'hyp.out',             ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition

           do k=1,nrecord1
             write(write01,'(a80)') data1(k) 
           enddo

           call sei close (close$,write01,code)
           goto 4726

         else
           write(*,*) ' sorry but you need to have a location !'
           goto 4724
         endif
c 
 
            goto 10
 


 4726       continue
 
c            if (map_lat_c.eq.0.0.and.map_lon_c.eq.0.0) then
c              write(*,*) 'no location !!!'
c              goto 10
c            endif

c
c enter map_lat and map_lon if one of them is 0.
c
            if (map_lat.eq.0.or.map_lon.eq.0) then
              write(*,*) ' distance of map border from '
     *           // 'event location in degrees (e.g. 10.) '
              write(*,*) '     latitude: '
              read(*,'(f5.1)') map_lat
              write(*,*) '     longitude: '
              read(*,'(f5.1)') map_lon
            endif
 
            write(*,*) 'starting epimap ... '
            chr_file = 'epimap.inp'
 
            call sei open(unknown$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
 
 
            write(write01,'(i2)') map_proj
            write(write01,'(f6.1,1x,f6.1)') map_lat_c-map_lat,
     &             map_lat_c+ map_lat
            write(write01,'(f7.1,1x,f7.1)') map_lon_c-map_lon,
     &             map_lon_c+map_lon
            write(write01,*)
            write(write01,'(a3)') '1 1'
            write(write01,'(a3)') '1 1'
            write(write01,*)
            txt  = top_directory(:seiclen(top_directory))
     &           // dchar // "DAT" // dchar //
     &           map_file(:seiclen(map_file)) // ".MAP"
            write(write01,'(a80)') txt
            write(write01,'(a31)') '$$$$$$end of contour list$$$$$$'
            write(write01,*)
            if(keys(1:4).eq.'MAPF') then
               write(write01,'(a)')'f'          ! fault plane solution
            else
               write(write01,*)
            endif
            write(write01,*)
            write(write01,'(a1)') map_stations
            write(write01,'(a7)') 'hyp.out'
            write(write01,'(a38)') '$$$$$$end of epicentre file list$$$$$
     *$'
            write(write01,*)
            write(*,*) 'writing epimap.inp'
            call sei close (close$,write01,code)
 
            call systemc("epimap epimap.inp",17)
 
 
           goto 10
 
         endif


c
c------------------------------------------------------------------------
c gmtmap, mapg and mapgf command
c------------------------------------------------------------------------
c

       if(keys(1:4).eq.'MAPG') then


c check for location in S-file first
c
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) then
           keys(1:4)='SAME'
           goto 10
         endif
            

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)

         call sei close (close$,write01,code)

         read(data1(1)(24:30),'(f7.3)') map_lat_c
         read(data1(1)(31:38),'(f8.3)') map_lon_c

         if (map_lat_c.eq.0.and.map_lon_c.eq.0) then
           write(*,*) ' sorry but you need to have a location !'
           keys(1:4)='SAME'
           goto 10
         endif
 
         if(keys(5:5).eq.'F') then ! plot fps
            call systemc('mapg '//evfile(1:seiclen(evfile))//' f',
     *      7+seiclen(evfile))
         else
            call systemc('mapg '//evfile(1:seiclen(evfile)),
     *      5+seiclen(evfile))
         endif
         keys(1:4)='SAME'
         goto 10

       endif

c------------------------------------------------------------------------
c macromap
c------------------------------------------------------------------------

       if(keys(1:8).eq.'MACROMAP') then
         if (pc) then
           keys(1:4)='SAME'
           write(6,*) ' Does not work on PC'
           goto 10
         endif

c
c check for location in S-file first
c
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) then
           keys(1:4)='SAME'
           goto 10
         endif
            

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)

         call sei close (close$,write01,code)
c
c   get file name of macroseismic file
c
         do i=1,nhead1
           if(data1(i)(75:80).eq.'MACRO3') then
              text=' '
              text(1:73)=data1(i)(2:74)
              go to 4956
           endif
         enddo
         write(6,*)' No macroseismic file given in S-file'
         keys(1:4)='SAME'
         goto 10
c
 4956    continue
c
c   open file in ISO
c
         call sei get file( open$,            ! Get file.
     &                          i,           ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'ISO',            ! Alternative directory to search.
     &                      text)             ! For this station file.
         if(code.ne.e_ok$) then
            write(6,*)' Macroseismic file missing'
            keys(1:4)='SAME'
            goto 10
         endif
c
         call sei close (close$,i,code)

         write(*,*) 'Starting macromap ... '
         write(gmtmap,'(a,a,a,a,a,a,a)')
     *    'macromap -macroinput ',text,
     *   ' -placename place_names.macro',' -lat ',data1(1)(24:30),
     *   ' -lon ',data1(1)(31:38)
         write(*,*) gmtmap

            call systemc(gmtmap,seiclen(gmtmap))
c           call systemc("gs gmt.ps",9)

           goto 10

       endif

c
c------------------------------------------------------------------------
c grid search
c------------------------------------------------------------------------
 
       if(keys(1:4).eq.'GRID') then
         call put_env_event(evfile)
         call systemc('hyp -gridsearch',15)
c
c   check if plotting the grid
c
         write(6,*)' Plot rms grid (y=default/n)'
         read(5,'(a)') answer
         if(answer.eq.'n') then
            keys(1:4)='SAME'
            goto 10
         endif

               chr_file = 'gridsearch.out'
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 4924
            goto 4925
 
 4924       continue
            write(6,*) 'No gridsearch.out file, hyp failure'
 
            keys(1:4)='SAME'
            goto 10
 

 4925       continue

            read(write01,'(a)') i
            read(write01,'(40x,2f10.3)') minlat,maxlat
            read(write01,'(40x,2f10.3)') minlon,maxlon
            call sei close (close$,write01,code)
c
c   get grid search solution for header
c
            call read_s_file_e('hyp.out')
 
            write(*,*) 'starting epimap ... '
            chr_file = 'epimap.inp'
 
            call sei open(unknown$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
 
 
            write(write01,'(i2)') map_proj
            write(write01,'(f6.1,1x,f6.1)') minlat,maxlat
            write(write01,'(f7.1,1x,f7.1)') minlon,maxlon
            write(write01,*) '         '
            range=maxlat-minlat
            if(range.ge.100.0)                    dlat=20.0
            if(range.lt.100.0.and.range.ge. 50.0) dlat=10.0
            if(range.lt.50.0. and.range.ge. 10.0) dlat= 5.0
            if(range.lt.10.0. and.range.ge.  5.0) dlat= 1.0
            if(range.lt.5.0.  and.range.ge.  1.0) dlat= 0.5
            if(range.lt.1.0.  and.range.ge.  0.5) dlat= 0.1
            if(range.lt.0.5)                      dlat=0.05
            range=maxlon-minlon
            if(range.ge.200.0)                    dlon=40.0
            if(range.lt.200.0.and.range.ge.100.0) dlon=20.0
            if(range.lt.100.0.and.range.ge. 50.0) dlon=10.0
            if(range.lt.50.0. and.range.ge. 10.0) dlon= 5.0
            if(range.lt.10.0. and.range.ge.  5.0) dlon= 1.0
            if(range.lt.5.0.  and.range.ge.  1.0) dlon= 0.5
            if(range.lt.1.0.  and.range.ge.  0.5) dlon= 0.1
            if(range.lt.0.5)                      dlon=0.05
            write(write01,'(2f7.2)') dlat,dlon
            write(write01,'(2f7.2)') dlat,dlon
            write(write01,*)'   '
            txt  = top_directory(:seiclen(top_directory))
     &           // dchar // "DAT" // dchar //
     &           map_file(:seiclen(map_file)) // ".MAP"
            write(write01,'(a80)') txt
            write(write01,'(a31)') '$$$$$$end of contour list$$$$$$'
            write(write01,'(a)') data1(1)(2:79)
            write(write01,*)'    '
            write(write01,'(a)') 'gridsearch.out'
            write(write01,*)'    '
            write(write01,'(a7)') 'hyp.out'
            write(write01,'(a38)') '$$$$$$end of epicentre file list$$$$$
     *$'
            write(write01,*) '   '
            write(*,*) 'writing epimap.inp'
            call sei close (close$,write01,code)
 
            call systemc("epimap epimap.inp",17)
 
 
           goto 10
 
         endif



c
c------------------------------------------------------------------------
c userdefined command
c------------------------------------------------------------------------
c
      if (keys(1:7).eq.'USERCOM') then
         usercom = ' '
         usercom = 'usercom -sfile ' // evfile(1:seiclen(evfile))
         write(*,*) ' ',usercom(1:seiclen(usercom))
         call systemc(usercom,seiclen(usercom))
         keys(1:4)='SAME'
         goto 10
      endif
        
         
c
c------------------------------------------------------------------------
c macroseismic input
c------------------------------------------------------------------------
c
      if(keys(1:3).eq.'MAC') then
         call systemc("macroin",7)
         keys(1:4)='SAME'
         goto 10
      endif

c
c------------------------------------------------------------------------
c list models 
c------------------------------------------------------------------------
c
      if(keys(1:6).eq.'MODELS') then
        text='MODEL.DEF' 
        call sei get file( open$,          ! Get file.
     &                      read03,         ! Opened on this unit.
     &                      code,           ! Returned condition.
     &                      'DAT',          ! Alternative directory to search
     &                      text )          ! For this station file.
        if(code.ne.e_ok$) then
          write(6,*)' No such file: MODEL.DEF'
          keys(1:4)='SAME'
          goto 10
        endif
        write(6,'(a)') ' '
        write(6,'(a)') ' List of model files: '
        write(6,'(a)') ' '
 1920   continue
        read(read03,'(a)',iostat=code) text
        call sei code(fort$,code,read03,b_eof)
        if (b_eof) goto 1925
        write(6,'(a)') text
        goto 1920
 1925   continue
        call sei close (close$,read03,code)
        keys(1:4)='SAME'
        goto 10
      endif

c
c-----------------------------------------------------------------------        
c  update event                                                          
c-----------------------------------------------------------------------        
c
      if(keys(1:6).eq.'UPDATE') then
         if(se) then
            write(6,*)
     *     ' You cannot update event in EEV, do it in SE with L-command'

            keys(1:4)='SAME'
            goto 10
         endif                                           
c
c   read and check if event should be updated
c        
         call read_s_file_e(evfile)

         if(data1(1)(45:45).eq.'*') then
            write(6,*)' Event marked for no location'
            keys(1:4)='SAME'
            goto  10
         endif
c
c   check if current operator id given
c
         call get_operator(operator)
c        write(6,*) operator

c        make log file if it does not exist:
         if ( seisan_logging.GE.1 )
     +   call create_log_file(evfile,operator)

c
         txt='hyp '//evfile(1:seiclen(evfile))//
     *   ' -update -operator '//operator(1:4)
c         write(6,*) txt

            call systemc(txt,seiclen(txt))

c        Log the update:
         if ( seisan_logging.GE.1 )
     +   call message_to_log_file(evfile,operator,1)

            keys(1:4)='SAME'
            goto  10
        endif

                                                                  
c

c-----------------------------------------------------------------------        
c  update event eleiminating rejected phases                                                         
c-----------------------------------------------------------------------        
c
      if(keys(1:6).eq.'UPDREJ') then
c        if(se) then
c           write(6,*)
c    *     ' You cannot update event in EEV, do it in SE with L-command'

c           keys(1:4)='SAME'
c           goto 10
c        endif                                           
c
c   read and check if event should be updated
c        
         call read_s_file_e(evfile)

         if(data1(1)(45:45).eq.'*') then
            write(6,*)' Event marked for no location'
            keys(1:4)='SAME'
            goto  10
         endif
c
c   check if current operator id given
c
         call get_operator(operator)
c        write(6,*) operator
c
         txt='hyp '//evfile(1:seiclen(evfile))//
     *   ' -update -operator '//operator(1:4)//' -reject'
c         write(6,*) txt

            call systemc(txt,seiclen(txt))

            keys(1:4)='SAME'
            goto  10
        endif

                                                                  
c

c
c-----------------------------------------------------------------------        
c  moment tensor with invrad                                                   
c-----------------------------------------------------------------------        
c
      if(keys(1:6).eq.'INVRAD') then                                                

               chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 3929
            goto 3930                                                         
 3929       continue                                                              
            write(6,*) 'No such event'                                            
            keys(1:4)='SAME'                                                       
            goto 10                                                                
c
 3930       continue                                                                
            call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            call sei close (close$,write01,code)
c
c   find relevant data
c
            read(data1(1)(39:43),'(f5.1)') hdepth
            k=0
            do i=nhead1+1,nrecord1
              if((data1(i)(17:17).eq.'D'.or.data1(i)(17:17).eq.'C').
     *        and.(data1(i)(11:12).eq.'PG'.or.data1(i)(11:12).eq.'SG'.
     *        or.data1(i)(11:12).eq.'Pg'.or.data1(i)(11:12).eq.'Sg').
     *        and.data1(i)(71:75).ne.'     ') then
                read(data1(i)(71:75),'(f5.0)') dist 
c jh 0203                dist=l
                read(data1(i)(77:79),'(i3)') l
                azim=l
                text(1:2)=data1(i)(11:12)
                if(text(2:2).eq.'g') text(2:2)='G'
c
c   check if amplitude on separate line
c
                amp=0.0
                do m=nhead1+1,nrecord1
                   if(data1(i)(2:8).eq.data1(m)(2:8).and.
     *                text(1:2).eq.data1(m)(13:14)) then 
                        k=k+1
                        read(data1(m)(34:40),'(g7.1)') amp
                        if(data1(i)(17:17).eq.'D') amp=-amp
                        amp=amp/1000.0      ! convert to microns
                   endif
                enddo
                if(amp.eq.0.0) goto 4856  ! no amp found
                write(data2(k),'(1x,a4,1x,a1,1x,a1,f8.2,f11.2,5x,g9.3)') 
     *          data1(i)(2:5),data1(i)(8:8),data1(i)(11:11),dist,azim,
     *          amp
                if(data1(i)(11:11).eq.'S'.and.(data1(i)(8:8).eq.'Z'.or.
     *          data1(i)(8:8).eq.'R')) data2(k)(10:10)='V'    ! assume SV
                if(data1(i)(11:11).eq.'S'.and.data1(i)(8:8).eq.'T')
     *          data2(k)(10:10)='H'    ! assume SH
              endif
 4856         continue
            enddo
            if(k.gt.4) then
               write(6,*)k,' data for inversion'
               chr_file = 'invrad.inp'
               call sei open(unknown$+warn$,            ! Open file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
               write(write01,*) ' Data for moment inversion'
               amp_error=0.1
               write(write01,'(i5,f5.1,f5.3)') k,hdepth,amp_error
               do i=1,k
                  write(write01,'(a80)') data2(i)
               enddo
               call sei close (close$,write01,code)
            else
               write(6,*)k,' data for inversion, not enough'
               keys(1:4)='SAME'
               goto 10
            endif
c
c   make model file
c
c
c  first look for station file in current directory, then in DAT
c
c
c  make file name for both file in current and dat directory
c  find if alternative model is to be used
c
         text=' '
         text(1:12)='STATION0.HYP'        ! Default.
         if(data1(1)(21:21).ne.' ') text(8:8)=data1(1)(21:21) ! specific model
 
          call sei get file( open$,           ! Get file.
     &                      read03,           ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'DAT',            ! Alternative directory to search.
     &                      text )            ! For this station file.
 
c   read down to model, past 2 blank lines
c        
        k=0   ! counting blank lines
 3330   continue
        read(read03,'(a)',iostat=code) text
        call sei code(fort$,code,read03,b_eof)
            if(text(1:3).eq.'   ') k=k+1
            if(k.eq.2) goto 3340
            goto 3330
 3340    continue
c
c  read model
c
        k=0
 3350   continue
        read(read03,'(a)',iostat=code) text
        call sei code(fort$,code,read03,b_eof)
            if(text(1:3).eq.'   ') goto 3360
            k=k+1
            read(text,'(2f8.2)') vp(k),depth(k)
            goto 3350
 3360    continue
c
c   close file
c
         call sei close (close$,read03,code)
c
c   generate and write model
c
               call sei open(unknown$+warn$,            ! Open file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       'invrad.mod',          ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
c
c   add infinite layer
c
         depth(k+1)=700.0
         do i=1,k
            write(write01,'(2f8.2)') depth(i),vp(i)
            write(write01,'(2f8.2)') depth(i+1),vp(i)
         enddo  
         call sei close (close$,write01,code)
c
c   run inversion program
c
         call systemc('invrad',6)

c
c  update data base with fault plane solution, open file with solution
c
         call sei open(old$+warn$,                 ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'invrad.out',          ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
         if( code .eq. e_ok$ ) then        ! Updating database.
           write(*,*)
     &     '.... updating database with  fault plane solution'
c
         else                              ! Can't open file.
            code = e_ok$                      ! Re-set condition.
            keys(1:4)='SAME'                  ! Back to loop.
            goto 10                           !
         end if                            !
c
c   read fault plane solutio
c
 1234    continue
         read(write01,'(a80)') text
         if(text(1:6).eq.'STRIKE') then
             read(text(8:14) ,'(f7.1)') strike
             read(text(31:36),'(f6.1)') dip
             read(text(53:59),'(f7.1)') rake
             newfault=.true.
             goto 1235
         endif
         goto 1234
 1235    continue
c
         call sei close (close$,write01,code)
c
c   write out in data base if there was a solution
c
         text=' '
         if(newfault) then
            write(text,'(3f10.1)') strike,dip,rake
            text(79:80)=' F'
            text(71:78)='INVRAD  '
            chr_file = evfile(1:fstart+20)
            call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            call indata(write01,nstat1,nphase1,nhead1,
     *      nrecord1,evid1,exp1,data1,id)        
            oldfault=.false.
            do i=2,nhead1
              if(data1(i)(71:80).eq.'INVRAD   F') then
                 data1(i)=text                     ! overwrite old solution
                 oldfault=.true.
               endif
            enddo
c
            rewind (write01,iostat=code)
            call sei code(fort$,code,write01,b_eof)
c
            if(oldfault) then
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2539
c
            else
               write(write01,'(a80)',iostat=code) data1(1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2539
c
               write(write01,'(a80)',iostat=code) text
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2539
c
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=2,nrecord1)
               call sei code(fort$,code,write01,b_eof)
            endif
c
c    Errors...
c
2539        if( code .ne. e_ok$ ) then
            chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
            call sei code(fort$+warn$,code,write01,b_eof)
            end if
c
c    Close down file & back to menu...
c
            call sei close (close$,write01,code)
         endif
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c



c
c-----------------------------------------------------------------------        
c  Duplicate existing event                                                    
c-----------------------------------------------------------------------        
c
      if(keys(1:3).eq.'DUP') then
         if(se) then
            write(6,*)' Event cannot be duplicated since call from SE'
            keys(1:4)='SAME'
            goto 10
         endif
c
c   check if index file
c
      do i=1,40
        if(base_name(i:i).eq.'.') then
           write(6,*)' Cannot duplicate when using index file'
           keys(1:4)='SAME'
           goto 10
        endif
      enddo

      call get_operator(operator)
c     make log file if it does not exist:
      if ( seisan_logging.GE.1 )
     +call create_log_file(evfile,operator)

c
c   copy existing event with a new id
c
c   open exising file
c
               chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit 
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 2929
            goto 2930                                                         
 2929       continue                                                           
            write(6,*) 'No such event'                                        
            keys(1:4)='SAME'                                                   
            goto 10              
c
c   read event
c
 2930       continue                                                                
            call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
            call sei code(fort$,code,write01,b_eof)
            call sei close (close$,write01,code)
c
c   check if id and file name ok
c
         if(id.ne.0) then
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif

c
c   check if current operator id given
c
         call get_operator(operator)
c 4793       continue
c            if(operator.eq.'    ') then
c               write(6,*) 'Give operator code, max 4 characters'
c               read(5,'(a)') operator
c               goto 4793
c            endif

c
c   get system time
c
            call systime(p_time,proc_time)
c
c   make new id and file name
c
           newfile=evfile                                                    
c           newfile(fstart-10:fstart-8)=new_base                              
c
c   make new id by adding one second
c
                call inc_id(data1(id),newfile,fstart+18)
c
c  get here to test if duplicate file already is there
c
 7531      continue
           chr_file = newfile(1:fstart+20)
              call sei open(unknown$+warn$,       ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 9154
           goto 9155                                                          
 9154      continue                                                          
           write(6,*)' Something wrong with data base'                                    
           keys(1:4)='SAME'                                                  
           goto 10                                                           
c
c   check for duplicate event in base
c
 9155     continue                                                          
          if(b_old) then                               ! check if file there
 7533        continue	  
             write(6,*)
             write(6,'(a)')
     *       ' File already exists in base, options are       :'
             write(6,'(a)')
     *       ' Do not make new file:                     Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event,         different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' No new file made'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                call inc_id(data1(id),newfile,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 7531                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 7255   ! go to write out
             endif
             goto 7533                                ! no valid choise
          endif
c
c    write out
c
 7255     continue
c
c   update id line
c
           WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
           WRITE(data1(id)(13:26),'(A)')PROC_TIME
           WRITE(data1(id)(9:11),'(A)')'DUP'
c
c  write
c
           write(write01,'(a80)',iostat=code) 
     *          (data1(i),i=1,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a,1x,a)')' New event file: ',newfile(1:fstart+20)

c Logging
      if ( seisan_logging.GE.1 ) then
         call message_to_log_file(evfile,operator,3)
         call create_log_file(newfile,operator)
         call message_to_log_file(newfile,operator,4)
      endif

c                                                                               
c   update event list                                                           
c                                                                               
           keys(1:3)='REN'                                                        
           CALL findevin
     *     (base_name,start_time,end_time,keys,from_eev,event_no,                          
     *     evfile,fstart,new_month,status)                                       
           keys(1:4)='SAME'                                                  
           goto 10                                                           
        endif                                                                   

c
c-----------------------------------------------------------------------        
c  NEW event                                                          
c-----------------------------------------------------------------------        
c
      if(keys(1:3).eq.'NEW') then
         if(se) then
            write(6,*)' New event cannot be made since call from SE'
            keys(1:4)='SAME'
            goto 10
         endif                
c
c   check if index file
c
      do i=1,40
        if(base_name(i:i).eq.'.') then
           write(6,*)' Cannot make new event when using index file'
           keys(1:4)='SAME'
           goto 10
        endif
      enddo

c
c
c   check if current operator id given
c
         call get_operator(operator)
c 8723       continue
c            if(operator.eq.'    ') then
c               write(6,*) 'Give operator code, max 4 characters'
c               read(5,'(a)') operator
c               goto 8723
c            endif

c
c   get system time
c
            call systime(p_time,proc_time)
c
c   get data for new event
c
         write(6,'(a,a)') ' Header line for new event, only info up ',
     *   'and including L is needed'
         write(6,'(a,a)')' Same base is assumed ***********'
 7473    continue            ! enter here if wrong data entry'
         write(6,'(a,a)') 
     *' Type below some or all info below fields,',
     *   ' field starts with - or capital letter'
         write(6,'(1x,a,a,a)') 
     *'CCYY MO',
     *'DA HRMI SECC LE-Latitu-Longitu-DeptFF',
     *              'AGA-NS-RMS MAGLXXX MAGLXXX MAGlXXX'
cc         if(pc)read(5,'(a)')text(2:79)
cc         if(sun.or.linux)
         read(5,'(a)')text(1:79)
         text(1:1)=' '
         text(80:80)='1'
c
         data1(1)=text
c
         read(text,'(1x,i4,1x,2i2,1x,2i2,1x,f4.1)',err=7474) 
     *   year,mon,day,hour,min,sec
c
c   error check
c
         goto 7475
 7474    continue
         write(6,*)' Something wrong with data entry, try again'
         goto 7473
 7475    continue
         if(text(22:22).ne.'L'.and.text(22:22).ne.'R'.and.text(22:22).
     *   ne.'D') then
            write(6,*)' Wrong event type (L)'
            goto 7473
         endif
         if(day.gt.31.or.hour.gt.24.or.min.gt.60.or.sec.gt.60.or
     *   .day.lt.1.or.hour.lt.0.or.min.lt.0.or.sec.lt.0) then
            write(6,*) ' Wrong day or time'
            goto 7473
         endif
c
c  make ID line
c
         isec=sec
         data1(2)=' '
         data1(2)(1:40)= ' ACTION:                   OP:     STATU'
         data1(2)(41:80)='S:               ID:                   I'
         WRITE(data1(2)(61:75),'(i4,6I2)')
     *   YEAR,MON,DAY,HOUR,MIN,ISEC
         DO I=61,74
            IF(data1(2)(I:I).EQ.' ') data1(2)(I:I)='0'
         ENDDO
         WRITE(data1(2)(31:34),'(A)')OPERATOR
         WRITE(data1(2)(13:26),'(A)')PROC_TIME
         WRITE(data1(2)(9:11),'(A)')'NEW' 
         id=2
c
c   help line
c
         data1(3)( 1:40)=' STAT SP IPHASW D HRMM SECON CODA AMPLIT'
c         data1(3)(41:80)=' PERI AZIMU VELO SNR AR TRES W  DIS CAZ7'
         data1(3)(41:80)=' PERI AZIMU VELO AIN AR TRES W  DIS CAZ7'
         data1(4)=' '
         nrecord1=4

c
c   make new file name, assume same data base, year and month
c
           call sfilname
     *     (year,mon,day,hour,min,isec,base_name(1:5),
     *     data1(1)(22:22),newfile,i)

c
c  get here to test if duplicate file already is there
c
 8531      continue
           chr_file = newfile(1:fstart+20)
              call sei open(unknown$+warn$,       ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 8154
           goto 8455                                                          
 8154      continue                                                          
           write(6,*)' Something wrong with data base'                                    
           keys(1:4)='SAME'                                                  
           goto 10                                                           
c
c   check for duplicate event in base
c
 8455     continue                                                          
          if(b_old) then                               ! check if file there
 8533        continue	  
             write(6,*)
             write(6,'(a,a,a)')
     *       ' File already exists in base ',new_base,' options are:'
             write(6,'(a)')
     *       ' Do not make new file:                     Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event,         different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' No new file made'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                call inc_id(data1(id),newfile,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 8531                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 8255   ! go to write out
             endif
             goto 8533                                ! no valid choise
          endif
c
c    write out
c
 8255     continue
c
c   update id line
c
           WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
           WRITE(data1(id)(13:26),'(A)')PROC_TIME
           WRITE(data1(id)(9:11),'(A)')'NEW'
c
c  write
c
           write(write01,'(a80)',iostat=code) 
     *          (data1(i),i=1,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a,1x,a)')' New event file: ',newfile(1:fstart+20)
c                                                                               
c   update event list                                                           
c                                                                               
           keys(1:3)='REN'                                                        
           CALL findevin
     *     (base_name,start_time,end_time,keys,from_eev,event_no,                          
     *     evfile,fstart,new_month,status)                                       
           keys(1:4)='SAME'                                                  
           goto 10                                                           
        endif                                                                   
c
c------------------------------------------------------------------------
c modelling, for most commands, a relocation must be done first
c------------------------------------------------------------------------
c
      if(keys(1:8).eq.'HERRMANN'.or.keys(1:5).eq.'BOUCH'.or.
     *keys(1:6).eq.'BOUSEI'.or.keys(1:6).eq.'HERSEI'.or.
     *keys(1:4).eq.'WKBJ') then

c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif

         if(keys(1:6).eq.'HERSEI') call systemc('hersei',6)
         if(keys(1:5).eq.'BOUCH') call systemc('bouch',5)
         if(keys(1:6).eq.'BOUSEI') call systemc('bousei',6)
         if(keys(1:8).eq.'HERRMANN') call systemc('herrman',7)
         if(keys(1:4).eq.'WKBJ') call systemc('wkbj',4)
c
c   delete event name in memory  so it is possible to use plot or other
c   external programs using event nam ein memory 
c
         call put_env_event('      ')
         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------------------------------------------
c
c  operating system command
c-----------------------------------------------------------------
c
      if(keys(1:1).eq.'O') then
c
c peter voss :
c   open file with s-file name, index file type
c
              call sei open( unknown$+warn$,       ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'eev.cur.sfile',       ! File name
     &                      write06,               ! Write unit #6
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
         write(write06,'(7x,a70)',iostat=code) evfile
         call sei code(fort$,code,write06,b_eof)
         call sei close (close$,write06,code)
c
c   delete event name in memory so it is possible to use plot or other
c   external programs 
c
         call put_env_event('      ')
         text(1:9)=keys(2:10)
         text(10:80)=command(1:70)
         write(6,'(1x,a)') text
         call systemc(text,79)
         keys(1:4)='SAME'
         goto 10
      endif
c----------------------------------------------------------------         
c  moving single events out or between data bases                               
c---------------------------------------------------------------        
      if(keys(1:1).eq.'C') then                                                 
        call get_operator(operator)
        if ( seisan_logging.GE.1 )
     +  call create_log_file(evfile,operator)
        write(6,*)                                                              
        write(6,*)                                                              
     *' Copy event: Other data base, give 1-5 letter name'                        
        write(6,*)                                                              
     *'             Local data base, type ,,             '                        
        write(6,*)                                                              
     *'             Working directory in file eev.out: return'                  
        read(5,'(a5)') new_base
        if(new_base(1:1).ne.' ') then     ! if blank, use eev.out
           do i=2,5
             if(new_base(i:i).eq.' ') new_base(i:i)='_'
           enddo                        
        endif                         
         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
        if (code .ne. e_ok$) go to 129              
        goto 130                                                                
 129    continue                                                                
        write(6,*) 'No such event'                                              
        keys(1:4)='SAME'                                                        
        goto 10                                                                 
c                                                                               
 130    continue                                                                
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
        call sei close (close$,write01,code)

c
c   check if id and file name ok
c
         if(id.ne.0) then
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif


c-- write in file                            
        if(new_base.eq.'     ') then 		
           chr_f_access$ = 'append'
              call sei open(unknown$,              ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'eev.out',             ! File name
     &                      write03,               ! Write unit #3
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
         write(write03,'(a80)',iostat=code) (data1(i),i=1,nrecord1)
         call sei code(fort$,code,write03,b_eof)
         call sei close (close$,write03,code)
c
c   open file with file names, index file
c
              chr_f_access$ = 'append'
              call sei open(unknown$,              ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'indexeev.out',        ! File name
     &                      write04,               ! Write unit #4
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
         write(write04,'(7x,a70)',iostat=code) evfile 
         call sei code(fort$,code,write04,b_eof)
         call sei close (close$,write04,code)
c
           keys(1:4)='SAME'                                                     
           goto 10                                                              
        else            
           if(base_name(1:2).eq.',,') then
              write(6,*)
     *   ' Not possible to copy to a data base from a local data base'
              keys(1:4)='SAME'
              goto 10
           endif                                                        
c                                                                               
c  copy event to other data base                   
c
           newfile=evfile                                                    
           newfile(fstart-14:fstart-10)=new_base                              
c
c  copy to local base
c
           if(new_base(1:2).eq.',,') then
              newfile=' '
              newfile=evfile(fstart:fstart+18)
           endif
c
c  get here to test
c
 1531      continue
           chr_file = newfile(1:fstart+20)
              call sei open(unknown$+warn$,       ! Open a unknown status file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 154
           goto 155                                                          
 154       continue                                                          
           write(6,*)' No such data base'                                    
           keys(1:4)='SAME'                                                  
           goto 10                                                           
c
c   check for duplicate event in base
c
 155      continue                                                          
          if(b_old) then                               ! check if file there
 1533        continue	  
             write(6,*)
             write(6,'(a,a,a)')
     *       ' File already exists in base ',new_base,' options are:'
             write(6,'(a)')
     *       ' Do not copy type:                          Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event in base, different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' File type will not be changed'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                if(new_base(1:2).eq.',,') then
                   call inc_id(data1(id),newfile,19)
                else
                   call inc_id(data1(id),newfile,fstart+18)
                endif
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 1531                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 2255   ! go to write out
             endif
             goto 1533                                ! no valid choise
          endif
c
c    write out
c
 2255     continue
           write(write01,'(a80)',iostat=code) 
     *          (data1(i),i=1,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a)')' File copied: ',newfile(1:fstart+20)
           keys(1:4)='SAME'                                                  
           goto 10                                                           
        endif                                                                   
      endif   
c
c-----------------------------------------------------------------
c  plot fault plane solutions with plotfoc                                                                  
c-----------------------------------------------------------------
c
      if(keys(1:3).eq.'FOO') then
         call systemc
     *   ('plotfoc '//evfile(1:fstart+20)//' eev',12+fstart+20)
         keys(1:4)='SAME'                                                  
         goto 10
      endif

c                                
c----------------------------------------------------------------
c  plot moment tensor solution with mopad
c----------------------------------------------------------------
c
      if(keys(1:2).eq.'FM') then

c
c   read s-file to get moment tensor if 'FM'
c

c
c   open s-file
c
            chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
c   read all parameters for one event from file unit 1
c
              call rea_event_in(write01,.true.,data1,code)
              call sei close (close$,write01,code)
c              write(6,*)mt_nmt
c             write(6,*)(mt_val(i,1),i=1,6)
c
c   mt plot if a solution, use first instance
c
              if(mt_nmt.gt.0) then
                  text=' '
                  text(1:14)='mopad.py plot '

                  write(text(15:22),'(f8.2)') mt_val(2,1)
                  text(23:23)=','
                  write(text(24:31),'(f8.2)') mt_val(3,1)
                  text(32:32)=','
                  write(text(33:40),'(f8.2)') mt_val(1,1)
                  text(41:41)=','
                  write(text(42:49),'(f8.2)') mt_val(5,1)
                  text(50:50)=','
                  write(text(51:58),'(f8.2)') mt_val(4,1)
                  text(59:59)=','
                  write(text(60:67),'(f8.2)') mt_val(6,1)
c
c   remove blanks
c
                  j=15
                  do i=15,67
                     if(text(i:i).ne.' ') then
                        text(j:j)=text(i:i)
                        j=j+1
                     endif
                  enddo
                  j=j-1
c
c   add absolute path to python script
c
                  long_text=' '
                  long_text=
     *            'python '//top_directory(1:seiclen(top_directory))
     *            //dchar//'PRO'//dchar//text(1:j)
                  j=seiclen(long_text)                       
                  write(6,*) long_text(1:j)
                  call systemc(long_text,j)
              else
                 write(6,*)' No moment tensor solution'
              endif
        keys(1:4)='SAME'                                                  
        goto 10
      endif





c                                
c----------------------------------------------------------------
c  fault plane solution with focmec or plot solutions with focmec
c----------------------------------------------------------------
c
      if(keys(1:2).eq.'F '.or.keys(1:2).eq.'FO'.or.keys(1:2).eq.'F_') 
     *then
c
c   first locate event, do not write out
c
         if(keys(1:2).eq.'FO') answer='o'  ! no question, plot saved solution
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
c
c   read s-file  if 'FO'
c
         if(keys(1:2).eq.'FO') then
c
c   open s-file
c
            chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
c   read all parameters for one event from file unit 1
c
              call rea_event_in(write01,.true.,data1,code)
              call sei close (close$,write01,code)
           endif

c
c  start focmec program
c
         if(keys(2:2).eq.' ') call systemc('focmec',6)
         if(keys(1:5).eq.'F_OLD') call systemc('focmec_old',10)
         if(keys(2:2).eq.'O') call systemc('focmec o',8) ! def plot saved solution
         write(*,*)'================================'
c
c  update data base with fault plane solution if no def plotting
c
         if(keys(2:2).eq.'O') then   ! def plotting
            keys(1:4)='SAME'
            goto 10
          endif
c
c   open file with solution made by focmec
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'focmec.inp',          ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
            if( code .eq. e_ok$ ) then        ! Updating database.
              write(*,*)
     &  '.... updating database with any saved fault plane solution'
c
            else                              ! Can't open file.
               code = e_ok$                      ! Re-set condition.
               keys(1:4)='SAME'                  ! Back to loop.
               goto 10                           !
            end if                            !
c
         call indata
     *   (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)        
         if(data1(1)(72:72).eq.'.') then
             write(6,*)' No focmec save, no solution made'
             call sei close (close$,write01,code)
             keys(1:4)='SAME'
             goto 10
         endif
         newfault=.false.
         do i=2,nhead1
           if(data1(i)(71:80).eq.'FOCMEC   F') then
              text=data1(i)              ! save new solution in text
              newfault=.true.            ! there was a new solution
           endif
         enddo
         call sei close (close$,write01,code)

         if(newfault) then
c
c   open s-file
c
            chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            call indata(write01,nstat1,nphase1,nhead1,
     *      nrecord1,evid1,exp1,data1,id)
c
c   get agency from station file and put in solution, make sure 
c   correct station file is used
c
          call get_agency_hyp(data1(1)(21:21),agency)
          text(67:69)=agency
c
c   check if already an fps in s-file
c        
            oldfault=.false.
            do i=2,nhead1
              if(data1(i)(71:80).eq.'FOCMEC   F') then
                 data1(i)=text
                 oldfault=.true.
                 goto 3745          ! only first valid solution updated
               endif
            enddo
 3745       continue
c
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
c
c   there was an old fps, so overwrite it
c
            if(oldfault) then
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2534
c
            else
               write(write01,'(a80)',iostat=code) data1(1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2534
c
               write(write01,'(a80)',iostat=code) text
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 2534
c
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=2,nrecord1)
               call sei code(fort$,code,write01,b_eof)
            endif
c
c    Errors...
c
2534        if( code .ne. e_ok$ ) then
            chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
            call sei code(fort$+warn$,code,write01,b_eof)
            end if
c
c    Close down file & back to menu...
c
         call sei close (close$,write01,code)
         endif
c
      keys(1:4)='SAME'
      goto 10
      endif


c
c----------------------------------------------------------
c  fault plane solution by inversion
c----------------------------------------------------------
c
      if(keys(1:2).eq.'FI') then
c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
c
c  start inversion program
c
         call systemc('pinv',4)
         write(*,*)'================================'
c
c  get new fault plane solution
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'pinv.out',            ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
            if( code .eq. e_ok$ ) then        ! Updating database.
              continue
            else                              ! Can't open file.
             code = e_ok$                      ! Re-set condition.
             keys(1:4)='SAME'                  ! Back to loop.
             goto 10                           !
            end if                            !
c
c  read number of polarities
c
            read(write01,'(35x,i5)') i
            if(i.lt.5) then
               write(6,*)' Too few polarities, do not update S-file'
               call sei close (close$,write01,code)
               keys(1:4)='SAME'
               goto 10
            endif
            write(*,*)
     &'.... updating database with PINV fault plane solution'
c

            text=' '
c
c   read solution
c
            read(write01,'(20x,a)')text(1:30) 
            text(71:80)='PINV     F'
c
c   read number of wrong polarities
c
            read(write01,'(a)') txt(1:1)
            read(write01,'(23x,a2)') text(61:62)
c            call get_agency_hyp(agency)
            call get_agency_hyp(data1(1)(21:21),agency) ! lo 9/2/2011
            text(67:69)=agency

            call sei close (close$,write01,code)
c
c  put into s-file
c
            chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            call indata(write01,nstat1,nphase1,nhead1,
     *      nrecord1,evid1,exp1,data1,id)  
     
            call get_agency_hyp(data1(1)(21:21),agency)
            text(67:69)=agency

            oldfault=.false.
            do i=2,nhead1
              if(data1(i)(71:80).eq.'PINV     F') then
                 data1(i)=text
                 oldfault=.true.
                 goto 3785          ! only first valid solution updated
               endif
            enddo
 3785       continue
c
            rewind (write01,iostat=code)
            call sei code(fort$,code,write01,b_eof)
c
            if(oldfault) then
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7534
c
            else
               write(write01,'(a80)',iostat=code) data1(1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7534
c
               write(write01,'(a80)',iostat=code) text
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7534
c
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=2,nrecord1)
               call sei code(fort$,code,write01,b_eof)
            endif
c
c    Errors...
c
7534        if( code .ne. e_ok$ ) then
             chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
             call sei code(fort$+warn$,code,write01,b_eof)
            end if
c
c    Close down file & back to menu...
c
             call sei close (close$,write01,code)
c         endif
c
        keys(1:4)='SAME'
        goto 10
      endif

c----------------------------------------------------------
c  fault plane solution by fpfit
c----------------------------------------------------------
c
      if(keys(1:2).eq.'FP') then
c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
         open(97,file='fpfit.fps',status='unknown')
         close(97,status='delete')
c
c  start seisan fpfit driver program
c
         call systemc('fpfit',5)
         write(*,*)'================================'
c
c  get new fault plane solution
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'fpfit.fps',           ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
            if( code .eq. e_ok$ ) then        ! Updating database.
              continue
            else                              ! Can't open file.
c
c  if no output file, there is no solution
c
             write(6,*)' No solution'
             code = e_ok$                      ! Re-set condition.
             keys(1:4)='SAME'                  ! Back to loop.
             goto 10                           !
            end if                            !
c
c  get parameters
c

            text=' '
c
c   read solution
c
            read(write01,
     *'(83x,f3.0,1x,f2.0,f4.0,2x,f4.2,5x,f5.1,12x,f2.0,1x,f2.0,1x,f2.0)'
     *      ,err=2222,end=2222)
     *      strike,dip,rake,fit,stdr,del_strike,del_dip,del_rake
            goto 2223
 2222       continue
            write(6,*)' No solution'
            keys(1:4)='SAME'                  ! Back to loop.
            goto 10
 2223       continue
            write(6,*)
            write(6,'(a,1x,f6.3)')' Fit',fit
            write(6,'(a,1x,3f6.1)')' Errors in strike, dip and rake',
     *      del_strike,del_dip,del_rake
            write(6,*)  
               

           write(*,*)
     &'.... updating database with FPFIT fault plane solution'

c
c   in fpfit, strike is downdip azimuth so correct
c
            strike=strike-90.0
c   
c   write values
c
            text=' '
            write(text,'(3f10.1,3f5.1)')
     *      strike,dip,rake,del_strike,del_dip,del_rake
            write(text(46:50),'(f5.1)') fit     ! fit
            write(text(51:55),'(f5.1)') stdr    ! station distribution ratio
            text(71:80)='FPFIT    F'


            call sei close (close$,write01,code)
c
c  put into s-file
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            call indata(write01,nstat1,nphase1,nhead1,
     *      nrecord1,evid1,exp1,data1,id)
c
c  get agency
c
            call get_agency_hyp(data1(1)(21:21),agency)
            text(67:69)=agency        

            oldfault=.false.
            do i=2,nhead1
              if(data1(i)(71:80).eq.'FPFIT    F') then
                 data1(i)=text
                 oldfault=.true.
                 goto 3795          ! only first valid solution updated
               endif
            enddo
 3795       continue
c
            rewind (write01,iostat=code)
            call sei code(fort$,code,write01,b_eof)
c
            if(oldfault) then
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7544
c
            else
               write(write01,'(a80)',iostat=code) data1(1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7544
c
               write(write01,'(a80)',iostat=code) text
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7544
c
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=2,nrecord1)
               call sei code(fort$,code,write01,b_eof)
            endif
c
c    Errors...
c
7544        if( code .ne. e_ok$ ) then
             chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
             call sei code(fort$+warn$,code,write01,b_eof)
            end if
c
c    Close down file & back to menu...
c
             call sei close (close$,write01,code)
c         endif
c
        keys(1:4)='SAME'
        goto 10
      endif

c----------------------------------------------------------
c  fault plane solution by hash
c----------------------------------------------------------
c
      if(keys(1:2).eq.'FH') then
c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
c
c  run hash
c
         call systemc('hash',4)

         write(*,*)'================================'
c
c  get new fault plane solution
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'hash.out',            ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
c
            if( code .eq. e_ok$ ) then        ! Updating database.
              continue
            else                              ! Can't open file.
c
c  if no output file, there is no solution
c
             write(6,*)' No solution'
             code = e_ok$                      ! Re-set condition.
             keys(1:4)='SAME'                  ! Back to loop.
             goto 10                           !
            end if                            !
c
c  get parameters
c
            text=' '
c
c   read solution
c
            read(write01,'(27x,3f8.1)',end=3646)
     *      strike,dip,rake
            goto 3647
 3646       continue

            write(6,*)'No fps solution'
            keys(1:4)='SAME'                  ! Back to loop.
            goto 10       
 3647       continue
            read(write01,'(27x,2f8.1)') del_dip,del_strike   
            read(write01,'(32x,f5.2)') fit
            read(write01,'(32x,f5.2)') amperr
            read(write01,'(32x,f5.2)') stdr


            goto 2723
 2722       continue
            write(6,*)' No solution'
            keys(1:4)='SAME'                  ! Back to loop.
            goto 10
 2723       continue
            write(6,*)  
               

           write(*,*)
     &'.... updating database with first HASH fault plane solution'

c   
c   write values
c
            text=' '
            write(text,'(3f10.1,2f5.1)')
     *      strike,dip,rake,del_strike,del_dip
            write(text(46:50),'(f5.2)') fit     ! fit
            write(text(51:55),'(f5.2)') stdr    ! station distribution ratio
            write(text(56:60),'(f5.2)')amperr   ! Ratio error
            text(71:80)='HASH     F'


            call sei close (close$,write01,code)
c
c  put into s-file
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            call indata(write01,nstat1,nphase1,nhead1,
     *      nrecord1,evid1,exp1,data1,id)        
c
c  get agency
c
            call get_agency_hyp(data1(1)(21:21),agency)
            text(67:69)=agency        

            oldfault=.false.
            do i=2,nhead1
              if(data1(i)(71:80).eq.'HASH     F') then
                 data1(i)=text
                 oldfault=.true.
                 goto 3995          ! only first valid solution updated
               endif
            enddo
 3995       continue
c
            rewind (write01,iostat=code)
            call sei code(fort$,code,write01,b_eof)
c
            if(oldfault) then
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7744
c
            else
               write(write01,'(a80)',iostat=code) data1(1)
               call sei code(fort$,code,write01,b_eof)
               if (code .ne. e_ok$) go to 7744
c
               write(write01,'(a80)',iostat=code) text
               call sei code(fort$,code,write01,b_eof)
c              if (code .ne. e_ok$) go to 7544
               if( code .ne. e_ok$ ) then
                 chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
                 call sei code(fort$+warn$,code,write01,b_eof)
c
c    Close down file & back to menu...
c
                 call sei close (close$,write01,code)
c
                 keys(1:4)='SAME'
                 goto 10
               endif
c
               write(write01,'(a80)',iostat=code) 
     *              (data1(i),i=2,nrecord1)
               call sei code(fort$,code,write01,b_eof)
            endif
c
c    Errors...
c
7744        if( code .ne. e_ok$ ) then
             chr_err_msg$ =                      ! Write error message.
     &'**** WARN: no update with fault plane solution ****'
             call sei code(fort$+warn$,code,write01,b_eof)
            end if
c
c    Close down file & back to menu...
c
             call sei close (close$,write01,code)
c         endif
c
        keys(1:4)='SAME'
        goto 10
      endif

c
c----------------------------------------------------------
c  composite fault plane solution
c----------------------------------------------------------
c
      if(keys(1:2).eq.'FC') then
c
c   first locate event
c
         call systemc('hyp',3)
c
c   save focmec input data
c
         call composite_foc
         keys(1:4)='SAME'
         goto 10
      endif
c
c------------------------------------------------------------------------
c   plot moment as a function of arrival time
c------------------------------------------------------------------------
c

      if(keys(1:3).eq.'PMM') then
c
         call systemc('plotmoment '//evfile(1:fstart+20),11+fstart+20)
         keys(1:4)='SAME'
         goto 10
      endif
c
c
c------------------------------------------------------------------------
c   plot ml as a function of distrance time
c------------------------------------------------------------------------
c

      if(keys(1:3).eq.'PML') then
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
c
         call systemc('plotml',6)
         keys(1:4)='SAME'
         goto 10
      endif
c
c------------------------------------------------------------------------       
c   change event type                                                           
c------------------------------------------------------------------------       
c
      if(keys(1:2).eq.'R ') then
         if(se) then
            write(6,*)' Change event type in SE'
            keys(1:4)='SAME'
            goto 10
         endif                               
         if(index_file) then
            write(6,*)
     *    ' Event type cannot be changed using an index file'
            keys(1:4)='SAME'
            goto 10
          endif
         write(6,*)
 724     continue
         call get_operator(operator)
         if ( seisan_logging.GE.1 )
     +   call create_log_file(evfile,operator)
         write(6,*)' Change event type to L,R or D ?'
         write(6,*)
     *   ' Second character for event ID (e.g. E or P)'
         write(6,*)' Return for no change ?'
         read(5,'(a2)') new_type
         if(new_type(1:1).eq.' ') then
           keys(1:4)='SAME'
           goto 10
         endif
         if(new_type(1:1).eq.'l') new_type(1:1)='L'
         if(new_type(1:1).eq.'r') new_type(1:1)='R'
         if(new_type(1:1).eq.'d') new_type(1:1)='D'
         if(new_type(1:1).ne.'L'.and.new_type(1:1).ne.
     *   'R'.and.new_type(1:1).ne.'D') then
            write(6,*)' Wrong type ******'
            goto 724
         endif
         if(new_type(2:2).eq.'e') new_type(2:2)='E'
         if(new_type(2:2).eq.'p') new_type(2:2)='P'
         if(new_type(2:2).eq.'v') new_type(2:2)='V'
         if(new_type(2:2).eq.'q') new_type(2:2)='Q'
         if(new_type(2:2).eq.'b') new_type(2:2)='B'
c
c   open and read file
c
            chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            if (code .ne. e_ok$) go to 1129
         goto 1130
 1129    continue
         write(6,*) 'No such event'
         keys(1:4)='SAME'
         goto 10
c                                                                               
c   read file, change type and rewrite                                          
c                                                                               
 1130    continue
         call indata
     *   (read01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
         call sei close (close$,read01,code)
c
c   check if id and file name ok
c
         if(id.ne.0) then
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
               goto 10
            endif
         endif

c                                                                               
c   check if type is different                                                  
c                                                                               
         if(data1(1)(22:22).eq.new_type(1:1)) then
           if(new_type(2:2).ne.' ') then
             if(new_type(2:2).eq.'B') then
               data1(1)(23:23)=' '
             else
               data1(1)(23:23)=new_type(2:2)
             endif
           call sei open(unknown$+warn$,           ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) then               ! check
             write(6,'(1x,a,a)')' Cannot open file: ',chr_file
             keys='SAME'
             goto 10
           endif
           write(write01,'(a80)',iostat=code)
     *       (data1(i),i=1,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a,a)')' Updated file       ', newfile(1:fstart+20)
           else
             write(6,*)' New type same as old, nothing changed'
           endif
           keys(1:4)='SAME'
           goto 10
         endif
c
c   make new file name
c                                            

         newfile=evfile                   
         newfile(fstart+10:fstart+10)=new_type

         if (newfile(fstart+10:fstart+10).eq.' ') 
     &          newfile(fstart+10:fstart+10)=data1(1)(22:22)
c
c   check if file can be opened or if it already is there
c
 1331    continue
           call sei open(unknown$+warn$,           ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      newfile,               ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
          if (code .ne. e_ok$) then               ! check 
             write(6,'(1x,a,a)')' Cannot open file: ',newfile
             keys='SAME'
             goto 10
          endif
c
c   check for id line
c
c           if(id.eq.0) then
c              write(6,*)' No id line, cannot check for duplication'
c     *                  ' of file id'
c              goto 1155
c          endif
c
c   check for duplicate event in base
c
          if(b_old) then                               ! check if file there
 1333        continue
             write(6,*)
             write(6,'(a)')
     *       ' File already exists in base, options are:'
             write(6,'(a)')
     *       ' Do not change type:                        Return'
             write(6,'(a)')
     *       ' Overwrite existing event:                       O'
             if(id.ne.0) write(6,'(a)')
     *       ' Create a new event in base, different ID:       N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' File type will not be changed'
                keys(1:4)='SAME'
                goto 10
             endif
             if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                call inc_id(data1(id),newfile,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 1331                            ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old event'
                goto 2155   ! go to write out
             endif
             goto 1333                                ! no valid choise
          endif
c
c    write out
c
 2155    continue             ! from just above
               data1(1)(22:22)=new_type(1:1)
               if(new_type(2:2).ne.' ') then
                 if(new_type(2:2).eq.'B') then
                   data1(1)(23:23)=' '
                 else
                   data1(1)(23:23)=new_type(2:2)
                 endif
               endif
               write(write01,'(a80)',iostat=code)
     *              (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               call sei close (close$,write01,code)
c                                                                               
c   delete old event                                                            
c                                                                               
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
               call sei close (delete$,write01,code)
          write(6,'(a,a)')' New file       ', newfile(1:fstart+20)
          write(6,'(a,a)')' Deleted file:  ', evfile(1:fstart+20)
c                                                                               
c   update event list
c                                                                               
         keys(1:3)='REN'
         CALL findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   newfile,fstart,new_month,status)
c
c logging rename logfile
c
         if ( seisan_logging.GE.1 )
     +   call move_log_file(newfile,evfile,operator,1)
c
          keys(1:4)='SAME'
          goto 10
      endif
c------------------------------------------------------------------------       
c  editor                                                                       
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'E ') then

         call get_operator(operator)
c        make log file if it does not exist:
         if ( seisan_logging.GE.1 )
     +   call create_log_file(evfile,operator)

c
c   get editor type
c
         call get_editor(eev_edit)
c        write(6,*) eev_edit
         text = eev_edit(1:sei clen(eev_edit)) // ' ' //
     *     evfile(1:fstart+20)     ! otherwise crash on Solaris lo
 358     continue
         leng = sei clen(text) ! added by LAA, 09.97
         call systemc(text,leng)                             
c
c   read file again to check errors
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)
         call sei close (close$,write01,code)
c
c   check for null chars
c
         do k=1,nrecord1-1
            do i=1,80
               if(data1(k)(i:i).eq.char(0)) then
                   write(6,*) 'NULL char in line ',k,' row ',i
               endif
            enddo
         enddo
c
c   check if any id
c
         if(id.eq.0) then
            write(6,*)' No valid ID line, check!!'
            write(6,*)' Return to continue, i to ignore'
            read(5,'(a)')answer 
            if(answer(1:1).eq.'i'.or.answer(1:1).eq.'I') goto 3761
            goto 358
         endif
 3761    continue
c
c   check if id and filename are ok but only if id line is there
c
         if(id.eq.0) goto 3758
         call check_id(evfile,evid1,data1(id),nerr)
         if(nerr.ne.0) then
            write(6,*)
     *      ' Filename and ID different or event type (L,R,D)'
            write(6,*)' not the same as in file name, fix it !!!'
            write(6,*)
            write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *      ' ',evid1
            l=seiclen(evfile)
            write(6,'(a,a)')   ' S-file name ',evfile(l-18:l) 
            write(6,*)' Return to continue, i to ignore'
            read(5,'(a)')answer
            if(answer(1:1).eq.'i'.or.answer(1:1).eq.'I') goto 3758
            goto 358
         endif
 3758    continue
c
c   check all fields in s-file
c
c   if output destination eq file (1 or 2), unit number must be changed
         call check_s(data1,                     ! file
     &                nrecord1,                  ! number of records
     &                evfile,                    ! file name
     &                nerr,                      ! number of errors
     &                0,                         ! output destination
     &                0)                         ! unit number
         if(nerr.gt.0) then
            write(6,*)' '
            write(6,*)'  Error in file, return to continue, i to ignore'
            read(5,'(a)')answer
            if(answer(1:1).eq.'i'.or.answer(1:1).eq.'I') goto 3759
            goto 358
         end if
 3759    continue

c        Log:
         if ( seisan_logging.GE.1 )
     +   call message_to_log_file(evfile,operator,5)

         keys(1:4)='SAME'
         goto 10
      endif      
c
c------------------------------------------------------------------------       
c  editor macroseismic file, ISO file                                                                      
c-----------------------------------------------------------------------        
c
      if(keys(1:2).eq.'EM') then
c
c   get editor type
c
         call get_editor(eev_edit)
c        write(6,*) eev_edit
 
c
c check for location of iso file in S-file first
c
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) then
           keys(1:4)='SAME'
           goto 10
         endif
            

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)

         call sei close (close$,write01,code)
c
c   get file name of macroseismic file
c
         do i=1,nhead1
           if(data1(i)(75:80).eq.'MACRO3') then
              text=' '
              text(1:73)=data1(i)(2:74)
              go to 7956
           endif
         enddo
         write(6,*)' No macroseismic file given in S-file'
         keys(1:4)='SAME'
         goto 10
c
 7956    continue
c
c   check file in ISO
c
         call sei get file( check$,            ! Get file.
     &                          i,            ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'ISO',            ! Alternative directory to search.
     &                      text)             ! For this station file.
         if(code.ne.e_ok$) then
            write(6,*)' Macroseismic file missing locally and in ISO'
            keys(1:4)='SAME'
            goto 10
         endif
         write(6,'(a,a)') 'tt ',text
c     
         txt = eev_edit(1:sei clen(eev_edit)) // ' ' //
     *     text(1:seiclen(text))
 
         leng = sei clen(txt) 
         call systemc(txt,leng)                       

         keys(1:4)='SAME'
         goto 10
      endif  


c------------------------------------------------------------------------       
c  editor print.out file                                                                       
c-----------------------------------------------------------------------        
c
      if(keys(1:2).eq.'EP') then
c
c   get editor type
c
         call get_editor(eev_edit)
c        write(6,*) eev_edit
     
         txt = eev_edit(1:sei clen(eev_edit)) // ' print.out '

         leng = sei clen(txt) 
         call systemc(txt,leng)                       

         keys(1:4)='SAME'
         goto 10
      endif                                                                     
                                                               
c                                                                   
                                                               
c
c------------------------------------------------------------------------       
c  plot picture file                                                                      
c-----------------------------------------------------------------------        
c
      if(keys(1:2).eq.'PP') then
 
c
c check for location of picture file  file in S-file first
c
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) then
           keys(1:4)='SAME'
           goto 10
         endif
c
c   read all parameters for one event 
c
         call rea_event_in(write01,.true.,data1,code)            

         call sei close (close$,write01,code)
c
c   get file name of picture file
c

         if(rea_npicture.eq.0) then
            write(6,*)' No picture file given in S-file'
            keys(1:4)='SAME'
            goto 10
         endif
c
c   check if more than one file, then select
c
         k=1
         if(rea_npicture.gt.1) then
           write(6,*)
           write(6,*)'Several picture files available, give number'
           do i=1,rea_npicture
             write(6,'(i3,2x,a)')i, rea_picture(i)(2:70)
           enddo
           read(5,*) k
         endif
c
c   check file in PIC
c
         call sei get file( check$,            ! Get file.
     &                          i,            ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'PIC',            ! Alternative directory to search.
     &                      rea_picture(k)(2:79))   ! For this file.
         if(code.ne.e_ok$) then
            write(6,*)' Picture file missing locally and in PIC'
            keys(1:4)='SAME'
            goto 10
         endif
c
c   find which type of file
c
         txt(1:3)=rea_picture(k)
     *   (seiclen(rea_picture(k)(2:79))-2+1:
     *   seiclen(rea_picture(k)(2:79))+1)
         if(txt(1:3).eq.'pdf'.or.txt(1:3).eq.'PDF') then
           txt = plot_pdf_command(1:sei clen(plot_pdf_command))
     *      // ' ' //
     *     rea_picture(k)(2:seiclen(rea_picture(k)(2:79))+1)
         else
           txt = plot_picture_command(1:sei clen(plot_picture_command))
     *      // ' ' //
     *     rea_picture(k)(2:seiclen(rea_picture(k)(2:79))+1)
         endif
 
         leng = sei clen(txt)
         write(6,*)'Picture plotting command:' 
         write(6,*) txt(1:leng)

         call systemc(txt,leng)                       

         keys(1:4)='SAME'
         goto 10
      endif                                                                     
   
c
c----------------------------------------------------------------------
c   delete all syntetic lines and mt lines
c---------------------------------------------------------------------
c
      if(keys(1:3).eq.'MTD') then
         write(6,*)
     *   ' This deletes all SYNT and MT lines, continue (y/n=enter)'
         read(5,'(a)') text
         if(text(1:1).eq.'y') then
            call read_s_file_e(evfile)
            m=0
            do k=1,nrecord1          ! shift records
               if(data1(k)(2:6).eq.'SYNT:'.and.data1(k)(80:80).eq.'3')
     *         then
                 continue
               else
                 m=m+1
                 data1(m)=data1(k)
               endif
             enddo
            nhead1=nhead1-(nrecord1-m)
            nrecord1=m
            call write_s_file_e(evfile)
         endif
         keys(1:4)='SAME'
         goto 10
      endif
c
c--------------------------------------------------------
c    Make parameters for mt, set mt_par and continue with synt
c--------------------------------------------------------
c
      if(keys(1:3).eq.'MTP') then
         mt_par=.true.
         keys(1:4)='SYNT'
      endif  

c
c----------------------------------------------------------------------
c  edit station lines for MT
c---------------------------------------------------------------------
c
cxx
      if(keys(1:3).eq.'MTE') then
         write(6,*)
     *   'Edit MT station lines'
     
            call read_s_file_e(evfile)
        
c   get parameters from s-file
c

        call mt_par_inv_all
     *   (data1,nhead1,hdepth,ndepth,del_depth,
     *   mt_npoints,mt_nstat_all,mt_stat_all,
     *   distance,azimuth,mt_offset_all,mt_redvel,
     *   mt_rate,
     *   mt_flow,mt_fhigh,mt_pole,mt_npasses,
     *   mt_dispvel,mt_comp_use,mt_start_time_par,mt_window,mt_used)
c
c   get the zcor used
c
        do i=1,300
          mt_offset_used(i)=0
        enddo

        call read_mt_dreger_output(mt_nstat,mt_stat,mt_offset)
c
c   associate used zcor with values in s-file
c
        do i=1,mt_nstat_all
           do k=1,mt_nstat
              if(mt_stat_all(i).eq.mt_stat(k)) 
     *        mt_offset_used(i)=mt_offset(k)
           enddo
        enddo
c
c  write out
c 
 2243    continue
         write(6,'(a)') 
     *   'NUM STAT ACTIVE COMP ZCOR-SFIL ZCOR-AUTO DIST  AZ'
         do i=1,mt_nstat_all
            text=' '
            if(mt_comp_use(1,i)) text(1:1)='T'
            if(mt_comp_use(2,i)) text(2:2)='R'
            if(mt_comp_use(3,i)) text(3:3)='Z'
            write(6,'(i3,1x,a5,i6,2x,a3,1x,i9,i10,i5,i4)')
     *      i,mt_stat_all(i),mt_used(i),text(1:3), 
     *      mt_offset_all(i),mt_offset_used(i),
     *      int(distance(i)),int(azimuth(i))
         enddo

         write(6,*)
     *   'Enter station number to flip if station is used or not'
         write(6,*) 
     *   'Enter station number and components to use, e.g. 4 TZ'
         write(6,*) 'Enter station number and new zcor, e.g. 3 33'
         write(6,*) 'Enter to terminate'
c
c   now start editing
c
         read(5,'(a)') text
         if(text.eq.' ') goto 2245

c-----------------------
c   components to use
c
         k=0
         text(51:53)=' '
         do i=1,20
           if(text(i:i).eq.'T') then
             k=k+1
             text(50+k:50+k)='T'
           endif
           if(text(i:i).eq.'R') then
             k=k+1
             text(50+k:50+k)='R'
           endif
           if(text(i:i).eq.'Z') then
             k=k+1
             text(50+k:50+k)='Z'
           endif
         enddo
          if(k.gt.0) then
c
c  first find station number
c
            i=index(text,text(51:51))
            read(text(1:i-1),*,err=2243, end=2243) k
            mt_comp_use(1,k)=.false.
            mt_comp_use(2,k)=.false.
            mt_comp_use(3,k)=.false.

            if(text(51:51).eq.'T') mt_comp_use(1,k)=.true.
            if(text(52:52).eq.'T') mt_comp_use(1,k)=.true.
            if(text(53:53).eq.'T') mt_comp_use(1,k)=.true.

            if(text(51:51).eq.'R') mt_comp_use(2,k)=.true.
            if(text(52:52).eq.'R') mt_comp_use(2,k)=.true.
            if(text(53:53).eq.'R') mt_comp_use(2,k)=.true.

            if(text(51:51).eq.'Z') mt_comp_use(3,k)=.true.
            if(text(52:52).eq.'Z') mt_comp_use(3,k)=.true.
            if(text(53:53).eq.'Z') mt_comp_use(3,k)=.true.

            mt_used(k)=1  ! if components selected, assume station is used
            goto 2243
         endif
c----------------------
c   new zcor
c
         read(text,*,err=2244,end=2244) k,i
         mt_offset_all(k)=i
         mt_used(k)=1  ! if zcor selected, assume station is used
         goto 2243
         
 2244    continue
c----------------------
c   only  flip station
c 
         read(text,*,err=2243,end=2243) k
         if(mt_used(k).eq.0) then
            mt_used(k)=1
         else
            mt_used(k)=0
         endif
         goto 2243 
 2245    continue
c--------------------------------------
c  update data array
c

         k=0
         do i=1,nhead1
            if(data1(i)(80:80).eq.'3'.and.data1(i)(2:6).eq.'SYNT:'.
     *       and.data1(i)(8:15).eq.'STATION:'
     *       .and.data1(i)(47:55).eq.'MTOFFSET:') then
               k=k+1
               data1(i)(7:7)=' '
               if(mt_used(k).eq.0) data1(i)(7:7)='x'
               write(data1(i)(61:65),'(i5)') mt_offset_all(k)
               text=' '
               if(mt_comp_use(1,k)) text(1:1)='T'
               if(mt_comp_use(2,k)) text(2:2)='R'
               if(mt_comp_use(3,k)) text(3:3)='Z'
               data1(i)(76:78)=text(1:3)
            endif
         enddo 


c--------------------------------------
c   save in s-file
c   
         write(6,*)'Update S-file(y/n=default)'
         read(5,'(a)')text
         if(text(1:1).eq.'y') call write_s_file_e(evfile)
         keys(1:4)='SAME'
         goto 10
      endif
c

c
c---------------------------------------------------------
c generate input values for synthetic modelling and mt
c---------------------------------------------------------
c
c
      if(keys(1:4).eq.'SYNT') then
         if(mt_par) then   
c
c   mark channels selected for mt, first check if mt waveform file
c   is there
c
             open(77,file='mulplt.wav',status='old', err=7272)
             goto 7273
 7272        continue
             write(6,*)' No mulplt.wav file, select with mulplt'
             keys(1:4)='SAME'
             goto 10
 7273        continue
c
c   check if correct event, must be same s-file
c
             read(77,'(9x,2f7.3,2i2,1x,a)') 
     *       mt_flow,mt_fhigh,mt_pole,mt_npasses,mt_sfile
c
c  check for filter
c
             if(mt_flow.eq.0.0.and.mt_fhigh.eq.0.0) then
                write(6,*)' No filters used, not possible'
                write(6,*)' Remake data files with filters'
                keys(1:4)='SAME'
                close(77)
                goto 10
             endif
             if(mt_pole.ne.4) then
                write(6,*)'Must be a 4 pole filter, is ',mt_pole
                keys(1:4)='SAME'
                close(77)
                goto 10
             endif
             if(mt_npasses.le.0.or.mt_npasses.gt.2) then
                write(6,*)'Filter passes is ',mt_npasses
                write(6,*)'Must be 1 or 2'
                keys(1:4)='SAME'
                close(77)
                goto 10
             endif

 
c            write(6,'(1x,a,1x,a)')
c     *      mt_sfile(1:seiclen(mt_sfile)),evfile(1:fstart+20)
             if(mt_sfile(1:seiclen(mt_sfile)).ne.evfile(1:fstart+20))
     *       then
                write(6,'(a)')
     *       ' mulplt.wav does not correspond to current event'  
                keys(1:4)='SAME'
                close(77)
                goto 10
             endif
c
c  find if displacement or velocity
c
             read(77,'(9x,a12)') mt_dispvel
             if(mt_dispvel.ne.'displacement'.
     *       and.mt_dispvel.ne.'velocity    ') then
                write(6,*)'Data not in displacment or velocity'
                keys(1:4)='SAME'
                close(77)
                goto 10
             endif    
             close(77)
c
c   now find stations used
c
             wav_filename(1)='mulplt.wav'
c             wav_file_nr_chan(1)=1
             call wav_init
             call read_wav_header(1)
             if(wav_nchan.eq.0) then
                 write(6,*) ' No data in mulplt.wav'
                 keys(1:4)='SAME'
                goto 10
             endif
c
c   save start time and duration of data
c
             write(mt_start_time,'(i4,a1,2i2,a1,2i2,a1,f4.1)') 
     *       wav_year(1),'-',wav_month(1),wav_day(1),'-',wav_hour(1),
     *       wav_min(1),'-',wav_sec(1)
             do i=1,19
                if(mt_start_time(i:i).eq.' ') mt_start_time(i:i)='0'
             enddo
             mt_window=wav_duration(1) ! assume all channels same length
c
c   make a copy of mulplt.wav with correct date
c
             mt_data_file(1:28)=mt_start_time(1:17)//'.mulplt.wav'
             write(6,*)
             write(6,*) ' Copy mulplt.wav to ',mt_data_file(1:28)
             write(6,*)

        if(pc) then
           call systemc('copy mulplt.wav '//mt_data_file(1:28),44)
        else
           call systemc('cp mulplt.wav '//mt_data_file(1:28),42)      
        endif                          
c
c   make list of stations to model, station names are usually repeated 3 times
c   for different channels so make sure not counted more than once
c
             mt_nstat=1
             mt_stat(1)=wav_stat(1)
             do i=1,wav_nchan
                mt_stat_found=.false.
                do k=1,mt_nstat
                   if(mt_stat(k).eq.wav_stat(i)) then
                      mt_stat_found=.true.
                   endif
                enddo
                if(.not.mt_stat_found) then
                   mt_nstat=mt_nstat+1
                   mt_stat(mt_nstat)=wav_stat(i)
                endif
             enddo    
             write(6,'(a)')' Stations to use for mt inversion'
             write(6,'(13(1x,a5))')(mt_stat(i),i=1,mt_nstat)                             
          endif

c
c   first locate event, do not write out
c
         call hyp_loc(answer)
         if(answer.eq.'N') then
            keys(1:4)='SAME'
            goto 10
         endif
c
c   mark stations in hyp.out file if mt parameter generation
c
         if(mt_par) then
            mt_stat_found=.true.     ! assume stations are there
            call read_s_file_e('hyp.out')
            do k=1,mt_nstat
               do i=nhead1+1,nrecord1
                  if(data1(i)(2:6).eq.mt_stat(k)) then
                     data1(i)(1:1)='s'
                     goto 7774        ! jump out of loop
                  endif
               enddo
c
c  if here, station not found
c
               write(6,'(a,a)')
     *         ' Station not found in S-file ',mt_stat(k)        
               mt_stat_found=.false.
 7774          continue
             enddo
             if(.not.mt_stat_found) then
                write(6,'(a)')' No mt parameters made'      
                keys(1:4)='SAME'
                goto 10
             endif
             call write_s_file_e('hyp.out')
          endif               
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)      
c
c   delete old references to synthetics
c
         do i=1,max_data
            do k=1,80
               data2(i)(k:k)=' '
            enddo
         enddo
         k=0
         do i=1, nrecord1
            if(data1(i)(2:6).ne.'SYNT:') then
               k=k+1
c
c  remove the station markers for synthetics
c
               if(data1(i)(1:1).eq.'s') data1(i)(1:1)=' '
               data2(k)=data1(i)
            endif
         enddo
c
c   write and read again to reinitialize counters nhead1 and nrecord1
c
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
               write(write01,'(a)',iostat=code) (data2(i),i=1,k)
               call sei code(fort$,code,write01,b_eof)
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)      

c
c   generate parameter file in s-file for synthetic programs and mt
c
         call makesyn(mt_flow,mt_fhigh,mt_dispvel,mt_pole,mt_npasses,
     *   mt_start_time,mt_window)
c
c   put in new syntetic data from synt.inp
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      'synt.inp',            ! File name
     &                      read03,                ! Read unit 
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)
         write(write01,'(a)',iostat=code) (data1(i),i=1,nhead1-1)
         call sei code(fort$,code,write01,b_eof)
 373     continue
         read(read03,'(a)',iostat=code) text
         call sei code(fort$,code,read03,b_eof)
         if (b_eof) go to 374
         write(write01,'(a)',iostat=code) text
         call sei code(fort$,code,write01,b_eof)
         goto 373
 374     continue
         write(write01,'(a)',iostat=code) 
     *        (data1(i),i=nhead1,nrecord1)
         call sei code(fort$,code,write01,b_eof)
         call sei close (close$,read03,code)
         call sei close (close$,write01,code)
c
c   turn off mt par mode
c
         mt_par=.false.
         keys(1:4)='SAME'
         goto 10
      endif
c
c-----------------------------------------------------------------
c  make  green parameter file and run greens function, there can be
c  more than one depth
c---------------------------------------------------------------
c
      if(keys(1:3).eq.'MTG') then
c
c   delete all old greens functions
c
        write(6,*)'Deleting Greens parameter fiels and functions'
        if(pc) then
           call systemc('del *.green*',12)
           call systemc('del green.*',11)
        else
           call systemc('rm *.green*',11)
           call systemc('rm green.*',10)       
        endif         
c
c   make parameter input file(s) from s-file, one per depth,
c
         call green_par(evfile,hdepth,ndepth,del_depth)
c
c   run green function, output file is green.out for each model depth
c
         do i=1,ndepth
            text(1:29)='fkrprog_seisan < green.par   '
            write(text(27:29),'(i3)')int(hdepth+(i-1)*del_depth)
            if(text(27:27).eq.' ') text(27:27)='0'
            if(text(28:28).eq.' ') text(28:28)='0'
            write(6,'(1x,a)') text(1:29)
            call systemc(text,29)
c
c   convert to time domain
c
            call systemc('wvint_seisan',12)
c
c   copy file to a file  with depth in name 
c

            if(pc) then
              call systemc('copy all.green all.green'//text(27:29)
     *        ,27)
            else
              call systemc('cp all.green all.green'//text(27:29),25)
            endif
         enddo 

         keys(1:4)='SAME'
         goto 10
      endif
c
c----------------------------------------------------------------------
c mt inversion
c----------------------------------------------------------------------
c
      if(keys(1:3).eq.'MTI') then
        write(6,*)'Deleting inversion data and parameterfiles'
        if(pc) then
           call systemc('del *.data',10)
           call systemc('del mt_inv.in',13)
        else
           call systemc('rm *.data',9)
           call systemc('rm mt_inv.in',12)       
        endif     

c
c   read event s-file
c

         call read_s_file_e(evfile(1:fstart+20))
c
c  get list of stations to use for inversion and some paramters from
c  synt lines
c
         call mt_par_inv
     *   (data1,nhead1,hdepth,ndepth,del_depth,
     *   mt_npoints,mt_nstat,mt_stat,
     *   distance,azimuth,mt_offset,mt_redvel,mt_rate,
     *   mt_flow,mt_fhigh,mt_pole,mt_npasses,
     *   mt_dispvel,mt_comp_use,mt_start_time_par,mt_window)
c
         write(6,*)    
         write(6,'(a,a)') '***Event to invert***: ',
     *   evfile(1:seiclen(evfile))
c
c  write and check parameters
c
         write(6,*)
         write(6,'(a,a)')   'Inversion in:             ',mt_dispvel
         write(6,'(a,i8)')  'Number of stations:       ',mt_nstat
         write(6,'(a,i8)')  'Number of points to use:  ',mt_npoints
         write(6,'(a,f8.3)')'Depth [km]:               ',hdepth
         write(6,'(a,f8.3)')'Sample rate:              ',mt_rate
         write(6,'(a,f8.1)')'Reduction velocity:       ',mt_redvel
         write(6,'(a,2f8.3,2x,2i3)')'Filter:                   ',
     *   mt_flow,mt_fhigh,mt_pole,mt_npasses
         write(6,*)

         if(mt_nstat.eq.0.or.mt_npoints.eq.0.or.hdepth.eq.0.0.or.
     *   mt_rate.eq.0.0) then
            write(6,*)' Some of above parameters are wrong'
            keys(1:4)='SAME'
            goto 10
         endif
c
c   check if correct data file and that filter has not been changed
c   also check displacemnt or velocity
c
c
c    check if mulplt.wav present, possibly use a backup file
c

         open(75,file='mulplt.wav',status='old',err=8245)
         goto 8246
 8245    continue
         write(6,*)' No mulplt.wav file'
         inquire(file=mt_start_time_par(1:17)//'.mulplt.wav',
     *   exist=exist)
         if(exist) then
            write(6,'(a,a)')'Backup file exists ',
     *      mt_start_time_par(1:17)//'.mulplt.wav'
            write(6,*)' Use backup file (n/y=enter)?'
            read(5,'(a)') text
            if(text.eq.' '.or.text(1:1).eq.'y') then
               if(pc) then
                  text(1:44)='copy '//mt_start_time_par(1:17)//
     *            '.mulplt.wav mulplt.wav'
                  call systemc(text(1:44),44)
               else
                  text(1:42)='cp '//mt_start_time_par(1:17)//
     *            '.mulplt.wav mulplt.wav'
                  call systemc(text(1:42),42)
               endif
               open(75,file='mulplt.wav',status='old')
            endif
         else        
           write(6,*)' Select with mulplt' 
           keys(1:4)='SAME'
           goto 10
         endif
 8246    continue

         read(75,'(9x,2f7.3,2i2,1x,a)')
     *   mt_data_flow,mt_data_fhigh,
     *   mt_data_pole,mt_data_npasses,mt_data_sfile
         read(75,'(9x,a12)') mt_data_dispvel
         close(75)

c   sfile check

         if(mt_data_sfile(1:seiclen(mt_data_sfile)).ne.
     *   evfile(1:fstart+20)) then
            write(6,*)
            write(6,'(a,a)')'Current event:   ',evfile(1:fstart+20)
            write(6,'(a,a)')'Data file event: ',
     *      mt_data_sfile(1:seiclen(mt_data_sfile))
            write(6,'(a)')
     *      'Current event does not correspond to data file'
c
c   check if backup file
c
            inquire(file=mt_start_time_par(1:17)//'.mulplt.wav',
     *      exist=exist)
            if(exist) then
               write(6,'(a,a)')'Backup file exists ',
     *         mt_start_time_par(1:17)//'.mulplt.wav'
               write(6,*)' Use backup file (n/y=enter)?'
               read(5,'(a)') text
               if(text.eq.' '.or.text(1:1).eq.'y') then
                  if(pc) then
                     text(1:44)='copy '//mt_start_time_par(1:17)//
     *               '.mulplt.wav mulplt.wav'
                     call systemc(text(1:44),44)
                  else
                     text(1:42)='cp '//mt_start_time_par(1:17)//
     *               '.mulplt.wav mulplt.wav'
                     call systemc(text(1:42),42)
                  endif
c
c   parameters must be read again since before read
c   from a wrong file
c
                  open(75,file='mulplt.wav',status='old')
                  read(75,'(9x,2f7.3,2i2,1x,a)')
     *            mt_data_flow,mt_data_fhigh,
     *            mt_data_pole,mt_data_npasses,mt_data_sfile
                  read(75,'(9x,a12)') mt_data_dispvel
                  close(75)
               endif
            else     
              write(6,'(a)')'Make data file for current event'   
              keys(1:4)='SAME'
              goto 10
            endif
        endif
c
c   filter check
c
        if(mt_flow.ne.mt_data_flow.or.mt_fhigh.ne.mt_data_fhigh) then
            write(6,*)
            write(6,'(a,2f8.3)') 'Paramter file filters:',
     *      mt_flow,mt_fhigh
            write(6,'(a,2f8.3)') 'Data file filters    :',
     *      mt_data_flow,mt_data_fhigh 
            write(6,'(a)') 'Filters cannot be different'
            write(6,'(a,a)')
     *      'Change filters in s-file to filters in data',
     *      ' file or make a new data file'
            write(6,*)
            keys(1:4)='SAME'
            goto 10
        endif      
 
       if(mt_pole.ne.mt_data_pole.or.mt_npasses.ne.mt_data_npasses) 
     * then
            write(6,*)
            write(6,'(a,2i3)') 'Paramter file poles and passes:',
     *      mt_pole,mt_npasses
            write(6,'(a,2i3)') 'Data file poles and passes    :',
     *      mt_data_pole,mt_data_npasses 
            write(6,'(a)') 'They cannot be different'
            write(6,'(a,a)')
     *      'Change in s-file to values in in data',
     *      ' file or make a new data file'
            write(6,*)
            keys(1:4)='SAME'
            goto 10
        endif     
c
c  disp vel check
c
       if(mt_dispvel.ne.mt_data_dispvel) then
           write(6,*)
           write(6,'(a,a)') 'Paramter file: ',mt_dispvel
           write(6,'(a,a)') 'Data file    : ',mt_data_dispvel
           write(6,'(a)')   'Disagreenment in disp/vel, fix'
           write(6,*)
           keys(1:4)='SAME'
           goto 10
        endif
           

c
c  open and make inversion parameter file
c
         open(66,file='mt_inv.in',status='unknown')
         i=hdepth

c   for now dist weight flag is fixed to 1 and no plot indicated by 0 (not used)
c   the depth i can only be integer 

c        write(66,'(2i4,a)') mt_nstat,i,' 1 0 '
c pv change to write true depth:
         write(66,'(i4,1x,f7.3,1x,a)') mt_nstat,hdepth,' 1 0 '
c 
c--------------------------------------------
c  extract out data files from mulplt.wav
c--------------------------------------------
c

c
c   read contents of data file mulplt.wav containing all data
c
         call wav_init
         wav_filename(1)='mulplt.wav'
         call read_wav_header(1)
c
c   check start time
c
            write(mt_start_time,'(i4,a1,2i2,a1,2i2,a1,f4.1)')
     *      wav_year(1),'-',wav_month(1),wav_day(1),'-',wav_hour(1),
     *      wav_min(1),'-',wav_sec(1)
            do i=1,19
               if(mt_start_time(i:i).eq.' ') mt_start_time(i:i)='0'
            enddo
            if(mt_start_time.ne.mt_start_time_par) then
               write(6,'(a,a)')'Start time in s-file is   : ',
     *         mt_start_time_par
               write(6,'(a,a)')'Start time in data file is: ',
     *         mt_start_time
               write(6,'(a)')'For you record, they should be the same'
               write(6,'(a)')'Continue(y/n=enter) ?'
               read(5,'(a)')text
               if(text.eq.' ') then
                  keys(1:4)='SAME'
                  goto 10
               endif
            endif
c
c  check if data in file
c
            if(wav_nchan.eq.0) then
               write(6,*) ' No data in mulplt.wav'
               keys(1:4)='SAME'
               goto 10
            endif
c
c  check how much sample reduction, assume 3 componets of same station
c  has the same sample rate and number of samples
c  so only look for station (component blank, return first station found)
c
            do i=1,mt_nstat
               call wav_find_chan(mt_stat(i),'    ',k) ! k is channel number
c
c   check if station there
c
               if(wav_error_message.ne.' ') then
                  write(6,'(a)') wav_error_message
                  write(6,'(a)')' Data not there'
                  write(6,'(a)')' Correct parameter file'
                  keys(1:4)='SAME'
                  goto 10
               endif
c
c   save chanel number, parameters can be used later to write out
c   missing channels (with zeroes)
c
               mt_ch(i)=k
c 
               xskip=wav_rate(k)/mt_rate  
c
c   save rate, nsamp and skip per station
c    
               mt_stat_rate(i)=wav_rate(k)
               mt_stat_nsamp(i)=wav_nsamp(k)
               skip(i)=xskip+0.01    ! correct for roundoff

               if(abs(xskip-skip(i)).gt.0.05) then
                  write(6,'(a,a)')'Cannot down sample data for ',
     *            wav_stat(k)
                  write(6,'(a,f7.3)')'Sample rate of data is: ',
     *            wav_rate(k)
                  keys(1:4)='SAME'
                  goto 10
               endif
               write(6,'(a,a5,1x,i3)')
     *         'Skip for down sampling for ',
     *         wav_stat(k),skip(i)
            enddo
            write(6,*)

c
c   calculate shift from reduction velocity, use nearest 
c   station as zero reference. use org. sample rate so shif in org. data set
c   
c 
         k=10000000
         do i=1, mt_nstat
           mt_shift(i)=(distance(i)/mt_redvel)*mt_stat_rate(i)
           if(mt_shift(i).lt.k) k=mt_shift(i)
         enddo
         do i=1,mt_nstat
           mt_shift(i)=mt_shift(i)-k+1
           write(6,'(a5,1x,a,f7.1,1x,a,f7.1,1x,a,i4)') mt_stat(i),
     *     'Distance(km)= ',distance(i), 'ShiftRedVel(s)= ',
     *     mt_shift(i)/mt_stat_rate(i),
     *     ' User zcor(sample)= ',mt_offset(i)
         enddo
         write(6,*)

c
c   check if enough data in input file for desired number of points
c

           text=' '
           do k=1,mt_nstat
              if(mt_npoints.gt.(mt_stat_nsamp(k)-mt_shift(k)-1)/skip(k)) 
     *        then
                 text='too few'
                 write(6,*)
                 write(6,'(a,a,a)')
     *           ' Too many samples selected for inversion',
     *           ' for station ',mt_stat(k)
                 write(6,'(a,f8.3)')
     *           ' The sample rate of resampled data is ',mt_rate
                 write(6,'(a,i8)')
     *           ' Samples in input data file after skip: ',
     *           (mt_stat_nsamp(k)-mt_shift(k))/skip(k)
                 write(6,'(a,i8)')
     *           ' Number of samples to shift:',mt_shift(k)/skip(k)
                 write(6,*)
              endif
           enddo            
           if(text(1:7).eq.'too few') then
              keys(1:4)='SAME'
              goto 10
           endif
c
c  read and write one channel at a time, in first call number of channels is
c  3 so header is written, later call it is -1 to indicate only channel written
c  if the channel is not there or it has been deselected, put in zereos, there 
c  must always be data in output file for each channel
c
         
         do i=1, mt_nstat
            text=mt_stat(i)//'.data'
            
c
c   output file name
c
            do l=2,5
               if(text(l:l).eq.' ') text(l:l)='_'
            enddo

            write(6,'(a,a)') 'Make data file for ',mt_stat(i)

            open(67,file=text(1:10),status='unknown')
c
c  T channel
c
            if(mt_comp_use(1,i)) then  ! check if channel should be used
               call wav_find_chan(mt_stat(i),'RR T',k) ! k is channel number
               if(wav_error_message.ne.' ') then
                  write(6,'(a)') wav_error_message
                  write(6,'(a)')' Data not there'
                  mt_comp_use(1,i)=.false.
c
c   since no data, generate header values from from other channels
c   for same station, put in #300
c
                  k=300
                  wav_stat(300)=mt_stat(i)
                  wav_comp(300)='RR T'
                  wav_month(300)=wav_month(mt_ch(i))
                  wav_day(300)=wav_day(mt_ch(i))
                  wav_hour(300)=wav_hour(mt_ch(i))
                  wav_min(300)=wav_min(mt_ch(i))
                  wav_sec(300)=wav_sec(mt_ch(i))
                  wav_year(300)=wav_year(mt_ch(i))
               endif
            endif
c
c   put zeros if not used
c
            if(mt_comp_use(1,i).eqv..false.) then
               do n=1,2*mt_npoints
                  signal1(n)=0.0
               enddo
               goto 9901  ! goto to write out
            endif

            call wav_read_channel(k)
c        
c  anti alias LP filter
c
               call recfil
     *        (signal1,wav_nsamp(k),signal1,'BU      ',0.0,0.0,4,
     *         'LP      ',0.0,wav_rate/5.0,1.0/wav_rate(k),1)                       
c
c  skip and shift, put data into wav block where it was before
c
            m=mt_shift(i)
c
c            write(6,*)'real skip',i, skip(i)
            do l=1,mt_npoints 
              signal1(l)=signal1(m)
              m=m+skip(i)
            enddo

 9901       continue

            wav_rate(k)=mt_rate
            call write_chan_helm(k,3,67,      ! k is channel in wav block
     *      mt_npoints ,evfile(1:fstart+20))

c-----------------
c   R channel
c-----------------
            if(mt_comp_use(2,i)) then  ! only check if channel should be used
               call wav_find_chan(mt_stat(i),'RR R',k) ! k is channel number
               if(wav_error_message.ne.' ') then
                  write(6,'(a)') wav_error_message
                  write(6,'(a)')' Data might not be rotated'
                  mt_comp_use(2,i)=.false.
c
c   since no data, generate header values from from other channels
c   for same station, put in #100
c
                  k=300
                  wav_stat(300)=mt_stat(i)
                  wav_comp(300)='RR T'
                  wav_month(300)=wav_month(mt_ch(i))
                  wav_day(300)=wav_day(mt_ch(i))
                  wav_hour(300)=wav_hour(mt_ch(i))
                  wav_min(300)=wav_min(mt_ch(i))
                  wav_sec(300)=wav_sec(mt_ch(i))
                  wav_year(300)=wav_year(mt_ch(i))
               endif
            endif
            if(mt_comp_use(2,i).eqv..false.) then  ! write zeros
               do n=1,2*mt_npoints
                  signal1(n)=0.0
               enddo
               goto 9902
            endif

            call wav_read_channel(k)

c        
c  anti alias LP filter
c
               call recfil
     *         (signal1,wav_nsamp(k),signal1,'BU      ',0.0,0.0,4,
     *         'LP      ',0.0,wav_rate/5.0,1.0/wav_rate(k),1)                       
c
c  skip and shift
c
            m=mt_shift(i)
            do l=1,mt_npoints 
              signal1(l)=signal1(m)
              m=m+skip(i)
            enddo
 9902       continue
            wav_rate(k)=mt_rate
            call write_chan_helm(k,-1,67,
     *      mt_npoints ,evfile(1:fstart+20))
c------------
c   Z channel
c------------
            if(mt_comp_use(3,i)) then  ! only check if channel should be used
               call wav_find_chan(mt_stat(i),'RR Z',k) ! k is channel number
               if(wav_error_message.ne.' ') then
                  write(6,'(a)') wav_error_message
                  write(6,'(a)')' Data might not be rotated'
                  mt_comp_use(3,i)=.false.
c
c   since no data, generate header values from from other channels
c   for same station, put in #300
c
                  k=300
                  wav_stat(300)=mt_stat(i)
                  wav_comp(300)='RR T'
                  wav_month(300)=wav_month(mt_ch(i))
                  wav_day(300)=wav_day(mt_ch(i))
                  wav_hour(300)=wav_hour(mt_ch(i))
                  wav_min(300)=wav_min(mt_ch(i))
                  wav_sec(300)=wav_sec(mt_ch(i))
                  wav_year(300)=wav_year(mt_ch(i))
               endif
            endif
c
c   put zeros if not used
c
            if(mt_comp_use(3,i).eqv..false.) then
               do n=1,2*mt_npoints
                  signal1(n)=0.0
               enddo
               goto 9903  ! goto to write out
            endif
            
            call wav_read_channel(k)

c        
c  anti alias LP filter
c
               call recfil
     *     (signal1,wav_nsamp(k),signal1,'BU      ',0.0,0.0,4,
     *         'LP      ',0.0,wav_rate/5.0,1.0/wav_rate(k),1)                       
c
c  skip and shift
c
            m=mt_shift(i)
            do l=1,mt_npoints 
              signal1(l)=signal1(m)
              m=m+skip(i)
            enddo
 9903       continue
            wav_rate(k)=mt_rate
            call write_chan_helm(k,-1,67,
     *      mt_npoints ,evfile(1:fstart+20)) 
            close (67)
c
c   file name and parameters in parameter file
c
            write(66,'(a,1x,2f6.1,2i6)') 
     *      text(1:10),distance(i),azimuth(i),mt_offset(i),mt_npoints
            mt_stat_full(i)=text(1:5)  ! save to make green stat names
         enddo

c------------------------------------------------------
c   cut green functions files and 
c   write greens function parameter lines
c------------------------------------------------------------
c
c   check if correct green data file and that filter has not been changed
c   also check displacemnt or velocity. use last generated greens file
c   if files for many depths
c 
         open(75,file='all.green',status='old')
         read(75,'(9x,2f7.3,2i2,1x,a)')
     *   mt_data_flow,mt_data_fhigh,mt_data_pole,
     *   mt_data_npasses,mt_data_sfile
         read(75,'(9x,a12)') mt_data_dispvel
         read(75,'(a)') text
         read(75,'(8x,f10.3)') mt_data_rate ! assume all chans same rate
         mt_data_rate=1.0/mt_data_rate
         close(75)

c   sfile check

         if(mt_data_sfile(1:seiclen(mt_data_sfile)).ne.
     *   evfile(1:fstart+20)) then
            write(6,*)
            write(6,'(a,a)')'Current event    :   ',evfile(1:fstart+20)
            write(6,'(a,a)')'Greens file event: ',
     *      mt_data_sfile(1:seiclen(mt_data_sfile))
            write(6,'(a)')
     *      'Current event does not correspond to Greens file'
            write(6,'(a)')'Make Greens file for current event'
            keys(1:4)='SAME'
            goto 10
         endif

c
c  disp vel check
c
       if(mt_dispvel.ne.mt_data_dispvel) then
           write(6,*)
           write(6,'(a,a)') 'Paramter file  : ',mt_dispvel
           write(6,'(a,a)') 'Greens file    : ',mt_data_dispvel
           write(6,'(a)')   'Disagreenment in disp/vel, fix'
           write(6,*)
           keys(1:4)='SAME'
           goto 10
        endif

c
c  sample rate check
c
       if(abs(mt_rate-mt_data_rate).gt.0.01) then ! allow for some slack
           write(6,*)
           write(6,'(a,f6.3)') 'Paramter file s. rate: ',mt_rate
           write(6,'(a,f6.3)') 'Greens file s. rate  : ',mt_data_rate
           write(6,'(a)')   'Disagreenment in sample rate'
           write(6,*)
           keys(1:4)='SAME'
           goto 10
        endif
c
c--------------------------------------------
c
        vr_max=0.0
        kdepth_first=1     ! start with first depth
        kdepth_last=ndepth ! end with last depth
        first_run=.true.   ! first time so write to parameter file
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    loop over depths
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
 3388  continue           ! go here for final calculation at best depth
       do kdepth=kdepth_first,kdepth_last
          call wav_init
c
c   open file all.greenxxx with all greens functions, one per depth
c

          wav_filename(1)='all.green'
          write(wav_filename(1)(10:12),'(i3)') 
     *    int(hdepth+(kdepth-1)*del_depth)
c         write(6,*)'hd,int',hdepth,int(hdepth+(kdepth-1)*del_depth)
          if(wav_filename(1)(10:10).eq.' ') 
     *    wav_filename(1)(10:10)='0'
          if(wav_filename(1)(11:11).eq.' ') 
     *    wav_filename(1)(11:11)='0'
   
          call read_wav_header(1)  ! read all headers
          if(wav_nchan.eq.0) then
             write(6,'(a,a)') 
     *      ' No data in file or no file ',wav_filename(1)(1:12)
              keys(1:4)='SAME'
              goto 10
          endif


          if(mt_flow.eq.0.0.and.mt_fhigh.eq.0.0) then
             write(6,*)
     *       ' No filter given in S-file, will stop'
             stop
          endif      
c
c   select and filter channels
c        
         do i=1,mt_nstat
c
c   find first channel in sequence
c
            call wav_find_chan(mt_stat(i),'TS S',k) ! k is channel number
c
c  assume the following 8 channels belong to this station and are in
c  correcrt order  
c
            open(67,file=mt_stat_full(i)//'.green',status='unknown')
            write(6,'(a,a)') 'Writing ',mt_stat_full(i)//'.green'
            do m=k,k+7             
               call wav_read_channel(m)                ! **** must check if same sfile
c
c   filter 
c
c-- calc. filt. cof.    
               call bndpas(mt_flow,mt_fhigh,1000.0/wav_rate(m),  
     *         cof,gain)                                                   
c
c-- apply filter to all data
c
c              write(6,*)'fil',mt_flow,mt_fhigh,mt_npasses
               call filter(signal1,wav_nsamp(m),cof,gain,mt_npasses)  ! 
c        
c  anti alias LP filter
c
               call recfil
     *         (signal1,wav_nsamp(m),signal1,'BU      ',0.0,0.0,4,
     *         'LP      ',0.0,wav_rate/5.0,1.0/wav_rate(m),1)                       
               if(k.eq.m) then   ! write file header   
                  call write_chan_helm(m,8,67,mt_npoints,
     *            evfile(1:fstart+20))
               else              ! do not write header
                  call write_chan_helm(m,-1,67,mt_npoints,
     *            evfile(1:fstart+20))
               endif
            enddo
            close(67)
            if(kdepth.eq.1.and.first_run)   ! only write in par file for first depth 
     *      write(66,'(a,a,i6)') mt_stat_full(i)//'.green',
     *      ' 0 ',mt_npoints
          enddo
          if(kdepth.eq.1.and.first_run) close(66)  ! only close for first depth
c
c  depth in parameter file must be changed to current depth, the rest is
c  the same. depth is not used in inversion but output and used to update
c  s-file
c
         open(66,file='mt_inv.in',status='old')
         kk=0
         do i=1,1000
           read(66,'(a)',end=9977) data2(i)
           kk=kk+1
         enddo
 9977    continue
         rewind(66)
         i=hdepth+(kdepth-1)*del_depth
c        write(66,'(2i4,a)') mt_nstat,i,' 1 0 '
         write(66,'(i4,1x,f7.3,1x,a)') 
     *mt_nstat,hdepth+(kdepth-1)*del_depth,' 1 0 '
         do i=2,kk
           write(66,'(a)')data2(i)
         enddo
         close(66)
        
         write(6,*)
         
c        
c   call the inversion program
c
         write(6,'(a)')'Output from tdmt_invc_seisan'
         write(6,*)

         call systemc('tdmt_invc_seisan',16)
c
c   save varience reduction and strike, dip and rake
c
         text=' '
         if(ndepth.gt.1) call update_mt_dreger(text,vr(kdepth),
     *   mt_strike(kdepth),mt_dip(kdepth),mt_rake(kdepth))
c
c   find max vr
c
         if(vr(kdepth).gt.vr_max.and.ndepth.gt.1) then
           vr_max=vr(kdepth)
           vr_max_depth=kdepth
         endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      enddo         ! end of loop over depths, kdepth
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   rerun with best results if several depths
c
          if(kdepth_last.gt.kdepth_first) then    ! there was more than one run           
             kdepth_first=vr_max_depth   ! start with this depth index
             kdepth_last=kdepth_first    ! only invert for one depth in final run
             first_run=.false.           ! not first time through depth loop
             goto 3388        ! invert again for best depth 
          endif

c
c   give summary of varience reduction and fps solution
c
          if(vr_max.gt.0.0) then
             write(6,*)
             write(6,'(a)')' depth      vr  strike     dip    rake'
             do i=1,ndepth
                write(6,'(f6.1,4f8.2)')hdepth+(i-1)*del_depth,vr(i),
     *          mt_strike(i),mt_dip(i),mt_rake(i)
             enddo
             write(6,*)
           endif
          
c
c   make file with observed and synthetics
c
         call mt_write_traces
     *   (mt_nstat,mt_stat,mt_rate,mt_npoints,evfile)       
c
c   possibly update s-file
c
         write(6,*)
         write(6,'(a)')'Update event with new mt solution(n=enter/y) ?'
         read(5,'(a)') text
         if(text(1:1).eq.'y') then
            call update_mt_dreger(evfile,vr(1),
     *      mt_strike(1),mt_dip(1),mt_rake(1))
         endif            
c
c   add event id to output file
c
         open(77,file='mt_inv_redi.out',status='unknown')
         do i=1,10000
            read(77,'(a)',end=7788) data2(i)
         enddo
 7788    continue
         rewind(77)
         if(i.eq.0) i=1
c
c   write s-file header line
c
         write(77,'(a)') data1(1)(2:79)
c
c   write back mt output file
c
         do j=1,i-1
            write(77,'(a)') data2(j)
         enddo
         close(77)
c
         keys(1:4)='SAME'
         goto 10
      endif
c

c
c------------------------------------------------------------------------
c   find event by time association, can be a default 180 sec set in findevin
c   or a number input below
c------------------------------------------------------------------------
c
      if(keys(1:1).eq.'S'.and.(keys(2:2).eq.'0'.or.keys(2:2).eq.'1'.
     *  or.keys(2:2).eq.'3'.or.keys(2:2).eq.'4'.or.keys(2:2).eq.'5'.
     *  or.keys(2:2).eq.'6'.or.keys(2:2).eq.'7'.or.keys(2:2).eq.'8'.
     *  or.keys(2:2).eq.'9'.or.keys(2:2).eq.' '.or.keys(2:2).eq.'2'
     *  )) then
c
c   search for event
c
         CALL findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   evfile,fstart,new_month,status)
         out_name=evfile(fstart:60)

c
c   if event found, display this and previous event
c
         if(status.eq.0) then
           open(1,file=evfile,status='old')
              read(1,'(a)') text
           close(1)
            write(6,*)
c
c   write nicely
c
      read(text(17:20),'(f4.1)') sec
      isec=sec
      read(text(7:8),'(i2)') i
c
c   check for magnitudes, if first not there, see if others
c
      if(text(57:64).eq.'        ') then
         if(text(65:71).ne.'       ') then
            text(57:64)=text(65:71)
         elseif(text(73:79).ne.'       ') then
            text(57:64)=text(73:79)
         endif
      endif
      write(6,289) event_no,
     *text(9:10),             ! day
     *month(i),               ! month
     *text(2:5),              ! year
     *text(12:13),            ! hour
     *text(14:15),            ! min
     *isec,                   ! sec
     *text(22:23),            ! indicators
     *text(24:30),text(31:38),text(39:45), ! hypocenter
     *text(57:64),                         ! first magnitude
     *text(49:51)                          ! number of stations
 289  format('#',i5,1x,a2,1x,a3,1x,a4,1x,a2,':',a2,1x,i2,2x,
     *a2,
     *a7,a8,a7,6x,a8,1x,a3,' Associa')   
c
c   back up one event
c
            keys(1:4)='BACK'

            CALL findevin
     *      (base_name,start_time,end_time,keys,from_eev,event_no,
     *      evfile,fstart,new_month,status)
            out_name=evfile(fstart:60)
c
c   go and ask what to do
c
             return
          endif
          keys(1:4)='SAME'
          goto 10
      endif

c
c-----------------------------------------------------------------------        
c  find existance of a waveform file or reference to archive                                                       
c-----------------------------------------------------------------------        
c
c
      if(keys(1:1).eq.'W'.and.keys(2:2).eq.' ') then                                                
c
c   read s-file
c
              chr_file = evfile(1:fstart+20)
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata(write01,nstat1,nphase1,nhead1,
     *   nrecord1,evid1,exp1,data1,id)
         call sei close (close$,write01,code)

         call auto_tr(data1,nhead1,nrecord1,k,wav_filename)
c
c  check for existance
c

         do i=1,k
              call  get_full_wav_name(wav_filename(i),text)
              if(text.eq.' ') then
                 write(6,'(a,a)') 
     *           ' File does not exist: ',
     *           wav_filename(i)(:seiclen(wav_filename))
              else
                 write(6,'(a,a)') 
     *           ' Full path name    :  ', text(:seiclen(text))
              endif
         enddo
         if(k.eq.0) 
     *   write(6,'(a)')' No waveform file names for this event'
         keys(1:4)='SAME'
         goto 10
      endif                                                                     

c
c
c-----------------------------------------------------------------------        
c  hypocenter program                                                          
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'L ') then                                                
         call systemc('hyp',3)
c         text=data1(1)
c         text(2:5)='New '
c         write(6,'(a)') text
c
c   plot with google map if flag set
c
cxx         write(6,*) pc,ghyp,googleearth,googlemap
         if(ghyp) then
           if(googlemap) then
              keys(1:4)='GMAP'
              from_hyp=.true. ! signal that hyp.out should be used instead of sfile
              goto 4488       ! goto gmap
           endif
c
c   if here assume googleearth, file is in working directory, find it
c
           k = getcwd(text)
           if ( k .ne. 0 ) then
             write(6,*) 'error finding current directory'
             keys(1:4)='SAME'
             goto 10
           endif
c
           if(pc) then
             long_text=' '
             long_text='start googleearth '
     *       //text(1:seiclen(text))//dchar//
     *       'gmap.cur.kml'
             call systemc(long_text(1:seiclen(long_text)),
     *       seiclen(long_text))
           elseif(linux) then
             long_text=' '
             long_text='google-earth '
     *       //text(1:seiclen(text))//dchar//
     *       'gmap.cur.kml &'
             call systemc(long_text(1:seiclen(long_text)),
     *       seiclen(long_text))
           endif
         endif
         keys(1:4)='SAME'
         goto 10
      endif          

c
c-----------------------------------------------------------------------        
c  hypocenter program with reject phases                                                       
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'LR') then                                                
         call systemc('hyp -reject',11)
c         text=data1(1)
c         text(2:5)='New '
c         write(6,'(a)') text
         keys(1:4)='SAME'
         goto 10
      endif                                                                
c
c-----------------------------------------------------------------------        
c  isc location program                                                        
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'IL') then                                                
         if(.not.pc) then
             call systemc('hyp_isc',7)
         else
             write(6,*)' ISC loction not available on PC'
         endif
c         text=data1(1)
c         text(2:5)='New '
c         write(6,'(a)') text
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c
c
c-----------------------------------------------------------------------        
c  hypo71 program
c-----------------------------------------------------------------------        
      if(keys(1:6).eq.'HYPO71') then   
         if (sun.or.linux) then
           call systemc('hypo71',6)
         elseif (pc) then
           call systemc('hypo71.exe',10)
         endif
         if (pc) then
            call systemc('more < hypo71.brief',19)
         else
            call systemc('more hypo71.brief',17)
         endif
c         text=data1(1)
c         text(2:5)='New '
c         write(6,'(a)') text
         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c
c-----------------------------------------------------------------------        
c  hypo inverse program                                                         
c-----------------------------------------------------------------------        
      if(keys(1:2).eq.'H ') then
c
c   it must be a local earthquake
c
         if(evfile(fstart+10:fstart+10).ne.'L') then
            write(6,*)
            write(6,*) 'Event must be local for hypoinverse' 
            keys(1:4)='SAME'
            write(6,*)
            goto 10
         endif
      
 4656    continue                                                
         if(operator.eq.' ') then
            write(6,*)'Give operator'
            read(5,'(a)') operator
            if(operator.eq.' ') goto 4656
         endif
c
         call systemc('hypinv_seisan '//evfile(1:fstart+20)//' '//
     *   operator,fstart+20+14+5)             ! locate
c
c   print out truncated file
c
         open (111,file='print.out',status='old')
         i=1
         k=1
 6564    continue
         read(111,'(a)',end=6565,err=6565) txt
         i=i+1
         k=k+1
         if(i.gt.18) then
           i=1
           write(6,*)'Enter to continue, q to quit listing'
           read(5,'(a)') text
           if(text(1:1).eq.'q') goto 6565
         endif
         write(6,'(a)') txt(2:80)
         goto 6564
 6565    continue
         close(111)
c
c   no location if print.out empty
c
         if(k.lt.2) then
            keys(1:4)='SAME'
            goto 10
         endif
c
c   no location if hypinv_seisan.out empty
c
         open(111,file='hypinv_seisan.out',status='old',err=4535)
         goto 4536
 4535    continue
         keys(1:4)='SAME'
         goto 10
 4536    continue
         close(111)

c
c   get header of existing event and new location
c
         text=' '
         text='hypinv_seisan.out'
         call read_s_file_e(text)
         write(6,*)
         write(6,'(a)')data1(1)(1:79)
         call read_s_file_e(evfile(1:fstart+20))
         write(6,'(a,a)')' OLD:',data1(1)(6:79)
         write(6,*)
c
c   possibly update
c
         
c   copy file back to data base
c
         write(6,*)' Overwite the current ',
     *   'event in the data base'
         write(6,*)' with the hypoinverse solution just shown'
         write(6,*)' Some phase information might be lost,(y/n=enter)?'
         read(5,'(a)') answer
         if(answer.eq.'Y'.or.answer.eq.'y') then
            if(sun.or.linux)
     *      call systemc(
     *      'cp hypinv_seisan.out '//evfile(1:fstart+20),fstart+20+21) 
            if(pc) 
     *      call systemc(
     *     'copy hypinv_seisan.out '//evfile(1:fstart+20),fstart+20+23)
         endif

         keys(1:4)='SAME'
         goto 10
      endif                                                                     
c
c-----------------------------------------------------------------------
c  autopick
c-----------------------------------------------------------------------
c
      if(keys(1:2).eq.'ZN') then               !JAB(BGS)Aug95.
         call systemc('neuropic',8)           !JAB(BGS)Aug95.
         keys(1:4)='SAME'                      !JAB(BGS)Aug95.
         goto 10                               !JAB(BGS)Aug95.
      else if(keys(1:2).eq.'ZT') then          !JAB(BGS)Aug95.
         call systemc('neurotrn',8)           !JAB(BGS)Aug95.
         keys(1:4)='SAME'                      !JAB(BGS)Aug95.
         goto 10                               !JAB(BGS)Aug95.
      else if(keys(1:1).eq.'Z') then           !JAB(BGS)Aug95.
         call systemc('autopic',7)
c        call systemc('dbx autopic',11)
         keys(1:4)='SAME'
         goto 10
      endif
c
c--------------------------------------------------------------------------
c  lomax pic
c--------------------------------------------------------------------------
c
      if (keys(1:2).eq.'AP') then
c
c  make one miniseed file from all input files
c
         txt=' '
         txt='wavetool -sfile '//evfile(1:seiclen(evfile))//
     *   ' -format MSEED -wav_out_file seed.out'
         write(6,'(a)') txt
c
c  run wavetool 
c
         call systemc(txt,seiclen(txt))
c
c  check if conversion ok, if not return to same event
c
         keys(1:4)='SAME'
         open(111,file='extract.mes',status='old',err=10)
c
c   run autophase
c
         txt=' '
         txt='autophase -sfile '//evfile(1:seiclen(evfile))//
     *   ' -wavefile seed.out'
         write(6,'(a)') txt

         call systemc(txt,seiclen(txt))


         keys(1:4)='SAME'
         goto 10
      endif
c------------------------------------------------------------------------
c autoratio and plotratio 
c------------------------------------------------------------------------
      if(keys(1:8).eq.'AMPRATIO') then
        call systemc('autoratio '//evfile(1:seiclen(evfile)),
     &     10+seiclen(evfile))
c
c   check if results
c
         open(113,file='autoratio.out',status='old')
         do i=1,2000
            read(113,'(a)',end=6353)text
         enddo
 6353    continue
         close(113)
         if(i.gt.3) then
             write(*,*) ' Plot autoratio reslults (y/n=enter) ? '
             read(5,'(a)') choice
             if (choice.eq.'y') then
                call systemc('plotratio',9)
             endif
         endif
         keys(1:4)='SAME'
         goto 10
      endif

c
c--------------------------------------------------------------------------
c automag
c--------------------------------------------------------------------------
c

      if (keys(1:7).eq.'AUTOMAG') then
c
c   run automag,different choices
c
        
         text=' '
         if(duration_spec.ne.0.0.or.duration_wa.ne.0.0) then
            write(text(1:25),'(a,f8.1,a,f8.1,1x,a)') ' s ',
     *      duration_spec,' w ',duration_wa,auto_p
         else
            text=' '//auto_p
         endif
c        text=text//' overwrite'
         text(27:35)='overwrite'
         
c
c        write(6,*) text(1:43)
         call systemc('automag '//evfile(1:seiclen(evfile))//text(1:43),
     *   43+seiclen(evfile))
         
         keys(1:4)='SAME'
         goto 10
      endif

c
c--------------------------------------------------------------------------
c autosig
c--------------------------------------------------------------------------
c
      if (keys(1:7).eq.'AUTOSIG') then

         call put_env_event(evfile)
         call sei open(old$+warn$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition
         if (code .ne. e_ok$) go to 4724

         call indata
     *      (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)

         rewind (write01,iostat=code)
         call sei code(fort$,code,write01,b_eof)

         call sei close (close$,write01,code)
c
c run autosig for all wave files
c
         do i=1,nhead1

           if (data1(i)(80:80).eq.'6') then
             text = data1(i)(2:79)
             text = 'autosig ' // text(1:seiclen(text))
             write(*,'(a)') ' starting autosig'
             write(*,'(1x,a)') text
             call put_message('autosig',text)
             call systemc('autosig',7)

           endif
         enddo
         keys(1:4)='SAME'
         goto 10
      endif
c
c--------------------------------------------------------------------------
c plotspec, plot resutls from automag
c--------------------------------------------------------------------------
c
      if (keys(1:5).eq.'PSPEC') then
         call systemc('plotspec '//evfile,9+seiclen(evfile))
         keys(1:4)='SAME'
         goto 10
      endif

c
c--------------------------------------------------------------------------
c  plot traces with mulplt 
c--------------------------------------------------------------------------
c

      if(keys(1:1).eq.'P'.and.
     *(keys(2:2).eq.' '.or.keys(2:2).eq.'O'.or.keys(2:2).eq.'o'.or.
     * keys(2:2).eq.'W'.or.keys(2:2).eq.'H'.or.keys(2:2).eq.'B'.or.
     * keys(2:2).eq.'M'.or.keys(2:2).eq.'G'.or.keys(2:2).eq.'D'.
     * or.keys(2:2).eq.'A')) then

      call get_operator(operator)
c     make log file if it does not exist:
      if ( seisan_logging.GE.1 )
     +call create_log_file(evfile,operator)

c
c check if any waveform files
c
c
c   read s-file
c
           chr_file = evfile(1:fstart+20)
           call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
           call indata(write01,nstat1,nphase1,nhead1,
     *             nrecord1,evid1,exp1,data1,id)
           call sei close (close$,write01,code)

           k=0
           karc=0             ! jh oct 2010

           if(keys(2:2).eq.'A') arc_by_default=1 

           do i=2,nhead1
             if(data1(i)(80:80).eq.'6'.and.data1(i)(2:2).ne.' ')
     *       then
                wave_file=' '
                wave_file(1:78)=data1(i)(2:79)
                if(wave_file(1:3).ne.'ARC') 
     *          then
                   call get_full_wav_name(wave_file,text)
               else
                   karc=karc+1
               endif
                if(text.ne.' ') then
                  k=k+1
                endif
             endif
           enddo

c           write(6,*)'karc',karc,arc_by_default
c
c   if arc is set by default, an arc lines will be made later so karc > 0
c
           if(arc_by_default.gt.1.and.karc.eq.0) karc=1
c
c           write(6,*)'karc2',karc

c          if(arc_vnet.gt.0)then
c           write(6,*)"arc_vnet",arc_vnet
c          else
c           write(6,*)"no arc_vnet"
c          endif


           if(keys(2:2).ne.'O'.and.keys(2:2).ne.'o') then
             if(k.eq.0.and.karc.eq.0) then
               write(6,'(a)')' No waveform file names for this event'
cjh jan 2012               from_mulplt='NEXT       0'
c               return
                from_mulplt=' '        ! signal not to go back to mulplt
                keys(1:4)='SAME'
                goto 10
             endif
           endif

         if(keys(2:2).eq.'O'.or.keys(2:2).eq.'o') then
           mulplt_def='O'! save def value
         endif



         if(mulplt_def.ne.'O') mulplt_def=' '
         oldfile=evfile
         old_event_no = event_no
         old_fstart = fstart
         old_index_file=index_file
         text=' ' 
         if(keys(2:2).eq.'A') text(2:2)='A'
         text(1:1)=mulplt_def   ! see if all options
         if(keys(3:3).eq.'O') text(2:2)='o'
         write(text(3:8),'(i6)') event_no
         write(text(12:12),'(i1)') show_menu ! signal to mulplt if menu shown
         text(16:20)=base_name(1:5)  ! used in cont plot
         text(21:34)=start_time       ! used in cont. plot
c
c   name of synthetics if plotted
c
         if(keys(2:2).eq.'W') text(40:50)='wkbjsei.out'
         if(keys(2:2).eq.'B') text(40:49)='bousei.out'
         if(keys(2:2).eq.'H') text(40:49)='hersei.out'
c
c   mt synthetics
c
 
         if(keys(2:2).eq.'M') then
             text(40:56)='synt_seis.out'
             text(1:1)='O'   ! all def
         endif
c
c   mt data
c
 
         if(keys(2:2).eq.'D') then
             text(40:49)='mulplt.wav'
             text(1:1)='O'   ! all def
         endif
c
c
c  mt green
c
         if(keys(2:2).eq.'G') then
             text(40:49)='all.green'
             text(1:1)='O'   ! all def
         endif
c
c        write(6,*) 'message ',text,' ',seiclen(text)
c
c   put message to mulplt about options
c
         call put_env_string(text)
         text=' '
c     write(6,*)' pv: eev_sub: end time : ',end_time
c        call systemc('mulplt',6)
         text='mulplt -operator     '
         write(text(18:21),'(a4)') operator(1:4)
c     write(6,*)' pv: eev_sub: text : ',text
         call systemc(text,21)
c
c  get message back from mulplt if any command to pass to eev
c
         call get_seisan_message(from_mulplt)
c     write(6,*)' pv: eev_sub: from mulplt: ', from_mulplt
c     write(6,*)' pv: eev_sub: l3947: keys ',keys
c         write(6,*) 'from mulplt: ', from_mulplt
c
c lo 29/04/2010 fixd in get_seisan_message, should not be needed anymore
c            evfile=oldfile          ! where did it get deleted ?
c            event_no = old_event_no
c            fstart   = old_fstart
c            index_file=old_index_file ! where did it get changed ??

         if(from_mulplt(1:4).ne.'    ') then ! a command to eev
             read(from_mulplt(12:12),'(i1)',err=4757) show_menu
             goto 4758
 4757        continue               ! error, should not happen
               mulplt_def=' '
               keys(1:4)='SAME'
               from_mulplt=' '
               goto 10
 4758        continue

c            evfile=oldfile          ! where did it get deleted ?
c            event_no = old_event_no
c            fstart   = old_fstart
c            write(*,*) evfile
            return                  ! if command, go to command in main
         else
            mulplt_def=' '
            keys(1:4)='SAME'
            goto 10
         endif
      endif

c
c--------------------------------------------------------------------------
c  plot in routine mode from data base 
c--------------------------------------------------------------------------
c
cx     if(keys(1:2).eq.'PP') then
c
c   send message to mulplt about current data base and event number
c
cx         write(text,'(i5,a40,2a14)') event_no,base_name,
cx     *   start_time,end_time
cx         text(70:70)=keys(3:3)   ! see if all options
cx         do i=1,80
cx           if(ichar(text(i:i)).eq.0) text(i:i)=' ' ! remove null chars
cx         enddo
cx         call put_env_string(text)
cx         text=' '
cx         call systemc('mulplt',6)
cx         keys(1:4)='SAME'
cx         goto 10
cx      endif

c
c--------------------------------------------------------------------------
c  SAC   
c--------------------------------------------------------------------------
c
      if(keys(1:3).eq.'SAC') then
         if(pc) then
            write(6,*)' SAC not available on PC'
            keys(1:4)='SAME'
            goto 10
         endif

c         sac_text = 'extract -sfile ' //
         sac_text = 'wavetool -sfile ' //
     *      evfile(1:seiclen(evfile)) //
     *      ' -format SAC ' 
  
         call systemc(sac_text,seiclen(sac_text))
       
c
c start sac
c
         call systemc('sac',3)
         keys(1:4)='SAME'
         goto 10
      endif





c
c--------------------------------------------------------------------------
c  PITSA
c--------------------------------------------------------------------------
c
      if(keys(1:5).eq.'pitsa' .or. keys(1:5).eq.'PITSA') then
         if(pc) then
            write(6,*)' Pitsa not available on PC'
            keys(1:4)='SAME'
            goto 10
         endif

c         pitsa_text = 'extract -sfile ' //
         pitsa_text = 'wavetool -sfile ' //
     *      evfile(1:seiclen(evfile)) //
     *      ' -format GSE -wav_out_file pitsa.gse' 

         write(6,*) ' GSE input file: pitsa.gse '

         call systemc(pitsa_text,seiclen(pitsa_text))
  
c
c start pitsa
c
         call systemc('pitsa',5)
         keys(1:4)='SAME'
         goto 10
      endif

c
c-------------------------------------------------------------------------      
c   delete event and save the deleted event in base DELET                      
c------------------------------------------------------------------------       

      delete_event=.FALSE.            ! default is ask
 3749 continue                   ! get to here from append delete_event=.TRUE.

      if(keys(1:3).eq.'d  '.or.keys(1:3).eq.'D  '.OR.delete_event) then
         if(se) then
            write(6,*)' Delete event in SE'
            keys(1:4)='SAME'
            goto 10
         endif                         
c only ask if confirmation wanted lot, 17-04-2002
         if(from_mulplt(1:4).ne.'    ') then
           if (confirmation_level.eq.1.) then
             write(6,*)
     *       ' Sure you want to delete S-file ?'          
             read(5,'(a)') answer            
           else
             answer='y'
           endif
         else
           write(6,*)' Sure you want to delete event (y/n) ?'                 
           read(5,'(a)') answer            
         endif
         if(answer.eq.'y'.or.answer.eq.'Y'.OR.delete_event) then
           call get_operator(operator)
           if ( seisan_logging.GE.1 )
     +     call create_log_file(evfile,operator)
c                                                                               
c  copy event to DELET base before deleting if not in DELET already                   
c
c
c   open file to delete
c
           chr_file = evfile(1:fstart+20)
           call sei open(old$+warn$,              ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
          if (code .ne. e_ok$) go to 2001
c
c   if in DELET data base, event to be deleted without copy !!!!!
c   if local data base, --------------------------------------
c   if an index file of a local data base, assume indicated by fstart=1
c          if(evfile(fstart-10:fstart-8).eq.'DEL'.or.
          if(base_name.eq.'DELET'.or.
     *    base_name(1:2).eq.',,'.or.fstart.eq.1) then
            write(6,*)
     *'Local data base, index file of local data base'//'
     *  or DELET data base'
            write(6,*)' File deleted without backup copy'
            goto 335
          endif
c
c  read file
c
          call indata
     *    (read01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)      
c
c   only alow delete if no SEISNET request line
c
c start change lot, 24/5/2005
          request_flag=.false.
          do i=2,nrecord1
            if(data1(i)(2:4).eq.'NET'.and.data1(i)(80:80).eq.'3'.
     *          and.(data1(i)(76:78).eq.'REQ'.or.
     *               data1(i)(76:78).eq.'CRQ')) then
c                write(6,*) 
c     *          'Event not deleted since it contains a request'
              request_flag=.true.
            endif
          enddo
          if (request_flag) then
            write(6,*)' File contains waveform request,'//
     &       ' really sure you want to delete event (y/n) ?'
            read(5,'(a)') answer
            if (answer.ne.'y'.and.answer.ne.'Y') then

c
c keep old file with Seisnet request lines only
c
                call sei close (close$,read01,code) 
                call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
                do k=1,nrecord1
                  if ((data1(k)(2:4).eq.'NET'.and.
     &                (data1(k)(76:78).eq.'REQ'.or.
     &                 data1(k)(76:78).eq.'CRQ')).or.
     &                 data1(k)(80:80).eq.'1'.or.
     &                 data1(k)(80:80).eq.'I'.or.
     &                 data1(k)(80:80).eq.'7') then
                    if (data1(k)(80:80).eq.'1')then
                       data1(k)(22:22)=chr_file
     &       (seiclen(chr_file)-8:seiclen(chr_file)-8)
                    endif
                  write(write01,'(a80)') data1(k)

                  endif
                enddo
                write(write01,'(a80)')
     &            '                                        '//
     &            '                                        '
                call sei close (close$,write01,code)
                write(6,'(a,a)')' Keeping file:  '
     &             ,evfile(1:fstart+20)
                keys='SAME      '                                             
                goto 10    
             endif
          endif                                                    
c end change lot, 24/5/2005
      
           newfile=evfile                                                    
           newfile(fstart-14:fstart-10)='DELET'                              
           chr_file = newfile(1:fstart+20)
 331       continue                                ! get here to test again
c
c   check if file can be opened in DEL and if it already is there
c
           call sei open(unknown$+warn$,           ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
           if (code .ne. e_ok$) go to 1154         ! check if DELET base there
c
c   check for id line
c
           if(id.eq.0) then
              write(6,*)' No id line, cannot check for duplication'
              write(6,*)' Event will be saved in DELET base with same',
     *       ' name'
              goto 1155
          endif
c
c   check for duplicate event in DELET base
c
          if(b_old) then                               ! check if file there
 333         continue	  
             write(6,*)
             write(6,'(a)')
     *       ' File already exists DELET base, options are:'
             write(6,'(a)')
     *       ' Do not save deleted event in DELET base:        Return'
             write(6,'(a)')
     *       ' Overwrite saved event in DELET base:            O'
             write(6,'(a)')
     *       ' Create a new event in DELET base, different ID: N'
             read(5,'(a)') choise
             if(choise.eq.' ') then
                write(6,*)' File will not be saved in DELET data base'
                goto 335  ! go to delete only
             endif
             if(choise.eq.'n'.or.choise.eq.'N') then
                call inc_id(data1(id),chr_file,fstart+18)
                call sei close (close$,write01,code) ! close, a new will be tested
                goto 331                             ! test again
             endif
             if(choise.eq.'o'.or.choise.eq.'O') then
                write(6,*)' Overwriting old saved event in DELET base'
                goto 1155   ! go to copy and delete
             endif
             goto 333                                ! no valid choise
          endif
c
c   error output
c
          goto 1155                                                          
 1154     continue                                                          
          write(6,*)' No such data base'                                    
          keys(1:4)='SAME'                                                  
          goto 10                                                           
c
c   copy and delete
c
 1155     continue
c
c   if here, file is read and written out in DEL data base
c                                                          
c
c  write file in DELET, header line first
c
           write(write01,'(a80)',iostat=code) 
     *     data1(1)
c
c   write who deleted event and when
c
c
c   check if current operator id given
c
         call get_operator(operator)
c 5723       continue
c            if(operator.eq.'    ') then
c               write(6,*) 'Give operator code, max 4 characters'
c               read(5,'(a)') operator
c               goto 5723
c            endif

c
c   get system time
c
            call systime(p_time,proc_time)

           text=' '
           text(1:29)=' File deleted from data base '
           text(30:34)=base_name(1:5)
           text(36:37)='by'
           text(39:42)=operator
           text(44:45)='at'
           text(47:60)=proc_time
           text(80:80)='3'
           
           write(write01,'(a80)',iostat=code) 
     *     text 
           
           write(write01,'(a80)',iostat=code) 
     *     (data1(i),i=2,nrecord1)
           call sei code(fort$,code,write01,b_eof)
           call sei close (close$,write01,code)
           write(6,'(a,a)')
     *    ' Backup copy saved as: ', chr_file(1:fstart+20)                          
c
c  close and delete
c
 335        continue                                ! to here if delete
            call sei close (delete$,read01,code)
            write(6,'(a,a)')
     *      ' Deleted file         ', evfile(1:fstart+20)                              
c
          if(base_name.eq.'DELET'.or.
     *    base_name(1:2).eq.',,') then
            write(6,*)' Log file is not deleted'
          else
c  move log file to DELET
            if ( seisan_logging.GE.1 )
     +      call move_log_file(newfile,evfile,operator,2)
          endif
c
            goto 2002                                                           
 2001       continue                                                            
            write(6,*)' No such file: ',evfile                                  
            keys='SAME'                                                         
c     write(6,*)' pv: eev_sub : 4286: end time : ',end_time
            goto 10                                                             
 2002       continue
                                                            
c                                                                               
c   update list of files                                                       
c                                                                               
            keys(1:3)='DEL'                                                     
c     write(6,*)' pv: eev_sub : 4288: end time : ',end_time
            CALL findevin
     *      (base_name,start_time,end_time,keys,from_eev,event_no,             
     *      evfile,fstart,new_month,status)                                     
            keys='SAME      '                                                   
c
c   if delete initiated from append, go to same event as appended to
c
c            write(*,*) ' debug oldno= ',oldno
            if(oldno.gt.1) then
               keys(1:1)='#'
c               write(keys(2:5),'(i4)') oldno-1
               write(keys(2:5),'(i4)') oldno
               oldno=0
            endif
            goto 10                                                             
         endif                                                                  
         keys='SAME      '                                                      
         goto 10                                                                
      endif                                                                     
c
c----------------------------------------------------------------
c   update list of files   
c-----------------------------------------------------------------
c
      if(keys(1:1).eq.'U') then
         if(se) then
            write(6,*)' Update event list in SE'
            keys(1:4)='SAME'
            goto 10
         endif
            keys(1:3)='REN'
c     write(6,*)' pv: eev_sub : 4314: end time : ',end_time
            CALL findevin
     *      (base_name,start_time,end_time,keys,from_eev,event_no,  
     *      evfile,fstart,new_month,status)
            keys='SAME      '
            goto 10
      endif

c                                                                               
c-------------------------------------------------------------------------                                                                               
c   another event                                                               
c-------------------------------------------------------------------------
c
      if(keys(1:1).eq.' '.or.keys(1:1).eq.'#'.or.
     *   keys(1:1).eq.'d'.or.keys(1:1).eq.'D'.or.
     *   keys(1:1).eq.'1'.or.keys(1:1).eq.'2'.or.keys(1:1).eq.'3'.or.
     *   keys(1:1).eq.'4'.or.keys(1:1).eq.'5'.or.keys(1:1).eq.'6'.or.
     *   keys(1:1).eq.'7'.or.keys(1:1).eq.'8'.or.keys(1:1).eq.'9'.or.
     *   keys(1:1).eq.'0'.or.keys(1:4).eq.'NEXT'.or.
     *   keys(1:4).eq.'next'.or.keys(1:4).eq.'SAME'.or.
     *   keys(1:4).eq.'same')then
         if(se.and.(keys(1:4).ne.'SAME'.or.keys(1:4).ne.'same')) then
           write(6,*) ' Select another event in SE'
           keys(1:4)='same'
           go to 10
         else
           go to 10
         endif
       endif

c -----------------------------------------------------------------------                                                                              
c   back space one event                                                        
c------------------------------------------------------------------------
c                                                                               
      if(keys(1:1).eq.'B'.or.keys(1:1).eq.'b') then                             
         keys(1:4)='BACK'                                                       
         goto 10                                                                
      endif
                                                                       
c
c----------------------------------------------------------------
c   show log file
c----------------------------------------------------------------
c

      if(keys(1:3).eq.'LOG') then
         call get_log_file_name(evfile,logfile)
         if ( seisan_logging.GE.1 ) then
         write(6,'(1x,a)') ''
         write(6,'(1x,a,a)') 'Log file name: ',logfile
         inquire(file=logfile, exist=file_exist)
         if (.NOT.file_exist) then
           write(6,'(1x,a)') 'Log file empty, creating a new log file'
           write(6,'(1x,a)') ''
           call get_operator(operator)
           call create_log_file(evfile,operator)
         endif
         call show_log_file(logfile)
         else
           inquire(file=logfile, exist=file_exist)
           if (file_exist) call show_log_file(logfile)
           write(6,'(1x,a)') ''
           write(6,'(1x,a)') 'Logging is disabled in SEISAN.DEF'
           write(6,'(1x,a)') ''
         endif
         keys='SAME      '
         goto 10
      endif

c
c----------------------------------------------------------------
c   type event                                                                  
c----------------------------------------------------------------          
c
                                                                                
      if(keys(1:2).eq.'T '.or.keys(1:3).eq.'TT ') then
	     i=1
         write(6,*)
         write(6,'(1x,a,a)') 'File name: ',evfile                                               
c        write(6,*)
         chr_file = evfile(1:fstart+20)
         call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
              if (code .ne. e_ok$) go to 2023
 6       continue                                                               
         read(read01,'(a)',iostat=code) text
         call sei code(fort$,code,read01,b_eof)
         if (b_eof) go to 7
cc         write(6,'(1x,a79)') text(2:80)
         write(6,'(a79)') text(2:80)      ! new for gfortran pc                                         
         i=i+1
         if(i.gt.20) then
            write(6,*)' Return to continue, q to return to EEV'
            read(5,'(a)') text
            if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') goto 7
            i=1
         endif
         if(keys(2:2).eq.'T') goto 7   ! only print header line 
         goto 6                                                                 
 7       continue                                                               
         call sei close (close$,read01,code)
         goto 2024                                                              
 2023    continue                                                               
c        write(6,*)' No such file: ',evfile                                     
 2024    continue             
         write(6,*)                                                  
         keys='SAME      '                                                      
         goto 10                                                                
      endif                                                              

c
c----------------------------------------------------------------
c   time shift data
c----------------------------------------------------------------
c
c THIS PART IS UNDER ALPHA TESTING !!!!
c

      if(keys(1:3).eq.'tsd'.or.keys(1:3).eq.'TSD') then
c           write(6,*)' Time shift data'


         chr_file = evfile(1:fstart+20)
               call sei open(old$+warn$,            ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       chr_file,              ! File name
     &                       write01,               ! Write unit
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
        if (code .ne. e_ok$) go to 929
        goto 930
 929    continue
        write(6,*) 'No such event'
        keys(1:4)='SAME'
        goto 10
c   
 930    continue
        call indata
     *  (write01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
        call sei close (close$,write01,code)

c-- write in file
      call sei open( check$,               ! check if file exists
     &                   ' ',              ! Prompt (n/a).
     &                   'tsd.tmp',          ! Filename.
     &                   write03,          ! Unit opened on.
     &                   exist,            ! Flag.
     &                   code )            ! Returned condition.

      if (exist) then
          call sei open( old$,           ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'tsd.tmp',          ! Filename.
     &                   write03,          ! Unit opened on.
     &                   b_flag,            ! Flag.
     &                   code )            ! Returned condition.

      else
        call sei open( new$,           ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'tsd.tmp',          ! Filename.
     &                   write03,          ! Unit opened on.
     &                   b_flag,            ! Flag.
     &                   code )            ! Returned condition.


      endif

         write(write03,'(a80)',iostat=code) (data1(i),i=1,nrecord1)
         call sei code(fort$,code,write03,b_eof)
         call sei close (close$,write03,code)
c
      call systemc('seisan_timeshiftdata',20)

         keys='SAME      '
         goto 10
      endif

c   
c------------------------------------------------------------------------
c   append an event, also in case of temporary append for location
c-------------------------------------------------------------------------
c
      loc_append=.false.            ! default is not to locate aappended events
      if(keys(1:1).eq.'L'.and.keys(2:2).ne.' ') loc_append=.true.
      if(keys(1:2).eq.'LL') loc_append=.true.   ! jh mar 15
      if((keys(1:1).eq.'A'.or.loc_append).and.
     &   keys(1:2).ne.'AU') then
c
c   check if from se
c
c        if(se) then
c           write(6,*)' Do append in SE'
c           keys(1:4)='same'
c           goto 10
c        endif

         call get_operator(operator)
c        make log file if it does not exist:
         if ( seisan_logging.GE.1 )
     +   call create_log_file(evfile,operator)

c
c   check  if a number was given
c
         if(keys(2:2).eq.' ') then
            write(6,*)' You must give a number to append from'
            keys(1:4)='SAME'
            goto 10
         endif
         oldno=event_no
         oldfile=evfile
         chr_file = evfile(1:fstart+20)
         call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         call indata
     *   (read01,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id)
         keys(1:1)='#'
         call sei close (close$,read01,code)
c
c   use next event if so specified without a number
c
         if(keys(1:2).eq.'#L'.or.keys(1:2).eq.'#A') keys(1:5)='NEXT '
         if(keys(1:2).eq.'#l'.or.keys(1:2).eq.'#a') keys(1:5)='NEXT '
c
c   find event to be appended
c
c     write(6,*)' pv: eev_sub l4488: end time : ',end_time
         CALL findevin
     *   (base_name,start_time,end_time,keys,from_eev,event_no,
     *   evfile,fstart,new_month,status)
c
c   check that you do not append to same event
c
         if(event_no.eq.oldno) then
            write(6,*)' Cannot append to same event'
            keys(1:4)='SAME'
            oldno=0
            goto 10
         endif
c
         if(status.eq.9) then
            write(6,*)' File to append not there'
            keys(1:4)='SAME'
            oldno=0
            goto 10
         endif
c
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      evfile,                ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
          call indata
     *(read01,nstat2,nphase1,nhead2,nrecord2,evid2,exp2,data2,id)
          call sei close (close$,read01,code)
c
c   append events and write out, in temporary file if for location
c
      call merge_f(data1,data2,nhead1,nhead2,nrecord1,nrecord2,status)

      if(status.eq.1) then
         write(6,*)  
     *   ' You cannot merge 2 S-files more than 24 hours apart'
         write(6,*) ' Nothing done'
         keys(1:4)='SAME'
         goto 10
      endif
      if(status.eq.2) then
         write(6,*)  
     *  ' S-files on different days, you must',
     *      ' start with earliest file'
         write(6,*) ' Nothing done'
         keys(1:4)='SAME'
         goto 10
      endif

      if(loc_append) then
        oldfile='append.out'
        evfile='append.out'
        keys(1:10)='L         '
      endif
              call sei open(unknown$,             ! Open an unknown status file.
     &                      ' ',                  ! Prompt file name (n/a).
     &                      oldfile,              ! File name
     &                      write01,              ! Write unit #1
     &                      b_old,                ! Already exists? (n/a).
     &                      code)                 ! Returned condition
         write(write01,'(a)',iostat=code)
     *        (data1(i),i=1,nrecord1)
         call sei code(fort$,code,write01,b_eof)
         call sei close (close$,write01,code)
c
c   locate if temporary append
c
      if(loc_append) then
         call put_env_event(evfile)
         write(6,422) event_no,oldno
 422     format(' Event # ',i4,
     *   ' temporarely appended to event # ',i4)
         call systemc('hyp',3)
         keys(1:4)='SAME'
         oldno=0
         goto 10
      endif
c
      write(6,222) event_no,oldno
 222  format(' Event # ',i4,' appended to event # ',i4,
     *   '   Appended event still present')
      write(6,*)' Do you want to delete appended event(y/n=return)'
      read(5,'(a)') choise
      delete_event=.FALSE.             ! delete event without asking
      if(choise.eq.'y'.or.choise.eq.'Y') then
c        keys(1:1)='#'
c        write(keys(2:5),'(i4)') event_no-1
        if (oldno.lt.event_no) oldno=oldno
        if (oldno.gt.event_no) oldno=oldno-1
        if (oldno.eq.0) oldno=1
        delete_event=.TRUE.            ! delete event without asking
        goto 3749   ! goto delete
      endif
c
c   position at old event
c
c moved lo
      keys(1:1)='#'
      write(keys(2:5),'(i4)') oldno
      oldno=0
      goto 10
      endif
 

      write(6,*)' WRONG CHOICE, TRY AGAIN ********************'                 
c      go to 14                                                                  
                                                                                
c                                                                               
c   stop                                                                        
c                                                                               
      return
      end                                                                       
                                                                                

c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c   Subroutines for synthetic modelling
c
c   latest update:
c
c  apr 93 by ra:  add herrmann parameters to maksyn
c  aug 94 by jh:  add wkbj
c
      subroutine makesyn(mt_flow,mt_fhigh,mt_dispvel,mt_pole,
     *mt_npasses,mt_start_time,mt_window)
c
c   made a synthetic input modeling file from station.hyp and hyp.out
c
c
c     mt_flow,mt_fhigh are filters used for mt
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimentions
C
       external sei open,                  ! Open file routine.
     &          sei get file,              ! Find & open file in ?directory?.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
       integer  code                       ! Returned contition.
C
C    ============= end of list ==========
C
c--- name of station-model file in current directory
      character*60 cur_file
c--- mt filters
      real mt_flow,mt_fhigh
      integer mt_pole,mt_npasses
c--- mt displacement or velocity
      character*12 mt_dispvel
c--- mt data start time and window
      character*19 mt_start_time
      real mt_window 
c--- the hyp file
      character*80 data(max_data)
c--- nymber of lines in hyp file
      integer ndata
c--- model indicator
      character*1 model
c--- model parameters
      real vp(50),vs(50),qp(50),qs(50),dens(50),depth(50),thicknes,vpvs
c--- fault plane solution
      real strike,dip,rake
c--- focal depth
      real fdepth
c--- a flag
      logical flag
c--- a control
c      logical check
c--- one line of text
      character*80 text
c--- stations to model
      character*5 stat(300)
c--- distance and different parameters
      integer ipar
      real distance
      character*1 moho(50)   ! indicates moho if 'N'
c--- counters and pointers and help variablws
      integer i,il,l,iblank,nstat,k,istat
c-- flag for changing parameters due to changing depth
      logical change_depth
      real total_time,start_time,window_time
      character*1 dchar    ! directory separation character
c-- coordinates of epicenter and stations
      real elat,elon,slat,slon
c-- distance azimuth etc
      real delta, azim,bazim,height
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c read unit #1 .. #6
       integer          read03, read06
c write unit #1 .. #5
       integer          write05
c
c   set computer type
c
      call dir_char(dchar)
c
c   open and read hyp.out file
c
      ndata=1
      open(95,file='hyp.out',status='old',err=5)
      goto 6
 5    continue
      write(6,*)' No hyp.out file, location probably failed'
      write(6,*)' No parameters made'
      return
 6    continue
 

 10   continue
        read(95,'(a)',end=20) data(ndata)
        ndata=ndata+1
        goto 10
  20  continue
      ndata=ndata-1
      close(95)
c
c   open model output file
c
      call sei open(unknown$,              ! Open a unknown status file.
     &              ' ',                   ! Prompt file name (n/a).
     &              'synt.inp',            ! File name
     &              write05,               ! Write unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
c
c   generate crustal model, first look in hyp.out, if no model there
c   use model from stationx.hyp, x can be any character
c
      flag=.false.
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: MODEL--:') 
     *   then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
             flag=.true.
         endif
      enddo
c
c  if no model in hyp.out, read from station file, first look for
c  file in current directory, then in DAT
c
      if(.not.flag) then
c
c  make file name for both file in current and dat directory
c  find if alternative model is to be used
c
         cur_file(1:12)='STATION0.HYP'        ! Default.
         read(data(1),'(20x,a1)') model       ! Alternative.
         if(model.ne.' ') cur_file(8:8)=model ! Install it.
 
          call sei get file( open$,            ! Get file.
     &                      read06,           ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'DAT',            ! Alternative directory to search.
     &                      cur_file )        ! For this station file.
 
c   read down to model, past 2 blank lines or model format line
c        
         iblank=0
 30      continue
        read(read06,'(a)',iostat=code) text
        call sei code(fort$,code,read06,b_eof)
            if(text(1:6).eq.'      ') iblank=iblank+1
            if(text(1:30).eq.'     VP  DEPTH     VSm    DENS')
     *      iblank=iblank+1
            if(iblank.eq.2) goto 40
            goto 30
 40      continue

c
c  read model
c
         il=0
 50      continue
        read(read06,'(a)',iostat=code) text
        call sei code(fort$,code,read06,b_eof)
            if(text(1:7).eq.'       ') goto 60
            il=il+1
            read(text,'(3f7.2,a1,1x,3f7.2)') vp(il),depth(il),vs(il), ! correct format jh dec 11
     *      moho(il),dens(il),qp(il),qs(il)
            goto 50
 60      continue
         write(6,'(a,i6)') ' Number of layers in model',il
c
c   give a thickness to last layer
c
         depth(il+1)=50.0+depth(il)
c
c   read vp/vs ratio in case it is needed
c
        read(read06,'(16x,f4.2)',iostat=code) vpvs
        call sei code(fort$,code,read06,b_eof)
c
c   close file
c
      call sei close (close$,read06,code)
c
c   generate model
c
         write(write05,200,iostat=code)
         call sei code(fort$,code,write05,b_eof)
 200   format(1x,'SYNT: MODEL--:     THICK        VP        VS',
     * '      DENS        QP        QS    3')
         do i=1,il
            if(vs(i).eq.0.0) vs(i)=vp(i)/vpvs
            if(dens(i).eq.0.0) dens(i)=2.4+i*0.2
            if(qp(i).eq.0.0) qp(i)= 600.0
            if(qs(i).eq.0.0) qs(i) =300.0
            thicknes=depth(i+1)-depth(i)
         write(write05,201,iostat=code) thicknes,vp(i),vs(i),
     *      dens(i),qp(i),qs(i),moho(i)
         call sei code(fort$,code,write05,b_eof)
 201        format(' SYNT: MODEL--:',6f10.3,1x,a1,2x,'3')
         enddo
      endif
c
c  get fault plane solution, first look if one specified for synthetic
c  modelling, else use Nordic format type F line or else set one arbitrarily
c
      flag=.false.
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: ST-D-RK:') 
     *   then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
            flag=.true.
         endif
      enddo
c
c   look for F-line, only the first is used
c
      if(.not.flag) then
         do i=1,ndata
            if(data(i)(80:80).eq.'F') then
               read(data(i),'(3f10.2)') strike,dip,rake
               goto 70
            endif
         enddo
c
c   there were no values given, make some
c
         strike=0.0
         dip=45.0
         rake=90.0
 70      continue
         write(write05,203,iostat=code) strike,dip,rake
         call sei code(fort$,code,write05,b_eof)
 203     format(' SYNT: ST-D-RK:',3f10.1,34x,'3')
      endif
c
c   get focal depth
c
      change_depth=.false.
      flag=.false.
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: DEPTH--:') 
     *   then
c
c   check if update due to changing depth
c
         if(data(i)(35:35).eq.'F') then
            change_depth=.true.
            read(data(1)(39:43),'(f5.0)') fdepth
            write(data(i)(16:25),'(f10.1)') fdepth
         endif
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
            flag=.true.
         endif
      enddo
c
c   use header line depth if not given above
c
      if(.not.flag) then
          read(data(1)(39:43),'(f5.0)') fdepth
         if(fdepth.eq.0.0) then
            write(6,*)
     *      ' Depth cannot be zero, now use 1.0'
            fdepth=1.0
            write(6,*)'Enter to continue'
            read(5,'(a)') text
         endif
         write(write05,204,iostat=code) fdepth,'         1       1.0'
         call sei code(fort$,code,write05,b_eof)
 204     format(' SYNT: DEPTH--:',f10.1,a,34x,'3')
      endif
c
c   number of points to model and number of points for mt inversion
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: NPOINTS:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1152
            endif
         enddo
         ipar=512
c         write(6,'(a,i5)')
c     *   ' Number of points to model not given, will use:',ipar
         write(write05,1208,iostat=code) ipar,(ipar/2+1)
         call sei code(fort$,code,write05,b_eof)
 1208    format(' SYNT: NPOINTS:',i10,' MT-NP-USE',i10,34x,'3')
 1152    continue
c
c   time window
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: TIMES--:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
c pc           read(data(i)(16:25),'(15x,3(10x,f10.1))') 
               read(data(i)(1:75),'(15x,3(10x,f10.1))') 
     *         total_time,start_time,
     *         window_time
c
c   make sure not zero
c
               if(total_time.eq.0.0) total_time=60.0      
               if(start_time.eq.0.0) start_time=0.0      
               if(window_time.eq.0.0) window_time=60.0      
               goto 1154
            endif
         enddo
c
c   no time was given, set to 60 secs
c
         total_time=60.0
         start_time=0.0
         window_time=60.0
c         write(6,'(a,f6.1)')
c     *   ' No time windows given, will use:',total_time
         write(write05,1212,iostat=code) 
     *         total_time,start_time,window_time
         call sei code(fort$,code,write05,b_eof)
 1212    format(' SYNT: TIMES--:','     TOTAL',f10.3
     *   ,'   INITIAL',f10.3,'  SY-TRACE',f10.3,4x,'3')
 1154    continue
c
c   damping, bouchon
c
c         do i=1,ndata
c            if(data(i)(2:15).eq.'SYNT: DAMPING:') then
c         write(write05,'(a)',iostat=code) data(i)
c         call sei code(fort$,code,write05,b_eof)
c               goto 1155
c            endif
c         enddo
c
c   use duration as damping
c
c         write(write05,1214,iostat=code) total_time
c         call sei code(fort$,code,write05,b_eof)
c 1214    format(' SYNT: DAMPING:',f10.1,54x,'3')
c 1155    continue

c
c    bouchon parameters xl, m and eps
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: BOUPAR-:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1156
            endif
         enddo
c
c   use defaults for BOUPAR
c
         write(write05,1216,iostat=code) 
         call sei code(fort$,code,write05,b_eof)
 1216    format(' SYNT: BOUPAR-:',
     *   '     800.0      2000     0.010',34x,'3')
 1156    continue
c
c    herrmann parameters xleng and xfac
c
c         do i=1,ndata
c            if(data(i)(2:15).eq.'SYNT: HERPAR-:') then
c         write(write05,'(a)',iostat=code) data(i)
c         call sei code(fort$,code,write05,b_eof)
c               check = .true.
c            endif
c         enddo
c
c   use defaults for BOUPAR
c
c         if (.not. check) then
c         write(write05,1217,iostat=code) 
c         call sei code(fort$,code,write05,b_eof)
c 1217             format(' SYNT: HERPAR-:',
c     *   '    1000.0         4     0.001',34x,'3')
c         endif
c
c   liquid layer or not
c
c         do i=1,ndata
c            if(data(i)(2:15).eq.'SYNT: LIQUID-:') then
c         write(write05,'(a)',iostat=code) data(i)
c         call sei code(fort$,code,write05,b_eof)
c               goto 1157
c            endif
c         enddo
c
c   use no liquid layer as default
c
c         write(write05,1218,iostat=code) 
c         call sei code(fort$,code,write05,b_eof)
c 1218    format(' SYNT: LIQUID-:',
c     *   '         f',54x,'3')
c 1157    continue
c
c   wkbj parameters
c
c   phases
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: PHASES-:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1256    ! do no change
            endif
         enddo
c
         write(write05,'(a,a,14x,a)',iostat=code) 
     *   ' SYNT: PHASES-:        Pg        Sg       PmP       SmS',
     *   '       SmP','3' 
         call sei code(fort$,code,write05,b_eof)
 1256    continue
c
c  sampling rate and source duration
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: DT-Tsou:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1257    ! do not change
            endif
         enddo
c   new def values
         write(write05,'(a,a,24x,a)',iostat=code)
     *   ' SYNT: DT-Tsou:     0.050      .100',
     *   '   MT-RATE       1.0','3'
         call sei code(fort$,code,write05,b_eof)
 1257    continue
c
c   mt start time
c
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: MTSTART:') then
               write(write05,'(a)',iostat=code) data(i)
               call sei code(fort$,code,write05,b_eof)
               goto 1367    ! do not change
            endif
         enddo
c   new value
         write(write05,'(a,a,a,f10.1,24x,a1)',iostat=code)
     *   ' SYNT: MTSTART: ',mt_start_time,' MT-WINDOW',
     *   mt_window,'3'
         call sei code(fort$,code,write05,b_eof)
 1367    continue

c
c
c   displacement or velocity for mt
c

         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: MT-D-V-:') then
               write(write05,'(a)',iostat=code) data(i)
               call sei code(fort$,code,write05,b_eof)
               goto 1357    ! do not change
            endif
         enddo
c   new values
         write(write05,'(a,a,51x,a1)',iostat=code)
     *   ' SYNT: MT-D-V-: ',mt_dispvel,'3'
         call sei code(fort$,code,write05,b_eof)
 1357    continue
c
c   reduction velocity
c
         do i=1,ndata
            if(data(i)(2:15).eq.' SYNT: REDVELO:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1258    ! do not change
            endif
         enddo

         write(write05,'(a,34x,a)',iostat=code) 
     *          ' SYNT: REDVELO:    0.0000 MT-REDVL:       8.0','3'
         call sei code(fort$,code,write05,b_eof)
 1258    continue
c
c  component 
c
         do i=1,ndata
            if(data(i)(2:15).eq.' SYNT: COMPON-:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1259    ! do not change
            endif
         enddo

         write(write05,'(a,54x,a)',iostat=code) 
     *          ' SYNT: COMPON-:    RADIAL','3'
         call sei code(fort$,code,write05,b_eof)
 1259    continue
c
c  free surface 
c
         do i=1,ndata
            if(data(i)(2:15).eq.' SYNT: STAT-AT:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 1260    ! do not change
            endif
         enddo

         write(write05,'(a,47x,a)',iostat=code) 
     *          ' SYNT: STAT-AT:  no             ','3'
         call sei code(fort$,code,write05,b_eof)
 1260    continue
c
c  mt filters 
c
         do i=1,ndata
            if(data(i)(2:15).eq.' SYNT: MT-FILT:') then
         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 12601    ! do not change
            endif
         enddo

         write(write05,'(a,2f10.3,2i10,24x,a)',iostat=code) 
     *   ' SYNT: MT-FILT:',mt_flow,mt_fhigh,
     *   mt_pole,mt_npasses,'3'
         call sei code(fort$,code,write05,b_eof)
12601    continue

c
c   find number of stations and stations to model
c
      nstat=0
      do i=1,ndata
         if((data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.
     *        'SYNT: STATION:').or.data(i)(1:1).eq.'s') 
     *   then
            do k=1,nstat
              if(stat(k).eq.data(i)(17:21)) goto 80
              if(stat(k).eq.data(i)(2:6)) goto 80
            enddo
c
c   station was not counted before
c   check added for zero distance stations jh 2 /12
c
cx            nstat=nstat+1
            if(data(i)(1:1).eq.'s'.and.data(i)(73:79).ne.' ') then
               nstat=nstat+1
               stat(nstat)=data(i)(2:6)
            else
               stat(nstat)=data(i)(17:21)
            endif 
 80         continue
         endif
      enddo
      write(6,*)'Number of stations to model', nstat
         write(write05,205,iostat=code) nstat
         call sei code(fort$,code,write05,b_eof)
 205  format(' SYNT: NSTAT--:',i10,54('-'),'3')
c
c----------------------------------------------------------------------------
c   enter loop over stations
c----------------------------------------------------------------------------
c
      do istat=1,nstat
         write(write05,99,iostat=code) 
         call sei code(fort$,code,write05,b_eof)
 99      format(' SYNT: NEW STAT:',63('-'),'3')
c
c   find distance for stations, either given by SYNT, or by hyp
c
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3') 
     *      then
c
c   check for an update due to changing depth
c
               if(change_depth) then
                  do l=1,ndata
                     if(stat(istat)(1:5).eq.data(l)(2:6)) then
                        read(data(l)(71:75),'(f5.0)') distance
                        write(data(i)(36:45),'(f10.1)') distance
                        goto 163
                     endif
                  enddo
                endif
 163         continue
             write(write05,'(a)',iostat=code) data(i)
             call sei code(fort$,code,write05,b_eof)
             goto 150
            endif
         enddo
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(2:6)) then
               read(data(i)(71:75),'(f5.0)') distance
         write(write05,206,iostat=code) 
     *         stat(istat),distance
         call sei code(fort$,code,write05,b_eof)
 206           format
     *         (' SYNT: STATION: ',a5,'S  Z'
     *         ,'  DISTANC:',f10.1,' MTOFFSET:         0',
     *         ' MT-COMP: TRZ 3')
               goto 150
            endif
         enddo
         write(6,'(a,a)')' No distance for station',stat(istat)
 150     continue
c  
c   get azimuth
c
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'AZIMUTH:'.and.data(i)(80:80).eq.'3') 
     *      then
c
c   check for an update due to changing depth
c
               if(change_depth) then
                  do l=1,ndata
                     if(stat(istat)(1:5).eq.data(l)(2:6)) then
                        read(data(l)(77:79),'(i3)') ipar
c
c   calculate backaz
c
                        read(data(1)(24:38),'(f7.3,f8.3)') elat,elon  ! epicente
                        call stat_loc(stat(istat),data(1)(21:21),
     *                  slat,slon,height)    ! station lat and lon
                        if(slat.eq.0.0.and.slon.eq.0.0.and.
     *                  height.eq.0.0) then
                           write(6,'(a,a,a)')' Station ',stat(istat),
     *                     ' not in station file'
                           write(6,*)
     *                     ' Will use azimut+180 for back azimuth'
                           bazim=ipar+180
                           if(bazim.gt.360) bazim=bazim-360.0
                           else
                           call azibazi(elat,elon,slat,slon,
     *                     delta,azim,bazim) ! calculate baz
                        endif
                        write(data(i)(36:45),'(f10.2)') float(ipar)
                        write(data(i)(56:65),'(f10.2)') bazim
                        goto 164
                     endif
                  enddo
                endif
 164     continue

         write(write05,'(a)',iostat=code) data(i)
         call sei code(fort$,code,write05,b_eof)
               goto 151
            endif
         enddo
         do i=1,ndata
            if(stat(istat)(1:5).eq.data(i)(2:6)) then
               read(data(i)(77:79),'(i3)') ipar
c
c   calculate backaz
c
         read(data(1)(24:38),'(f7.3,f8.3)') elat,elon  ! epicenter lat and lon
         call stat_loc(stat(istat),data(1)(21:21),slat,slon,height)    ! station lat and lon
c         call stat_loc(stat(istat),slat,slon,height)    ! station lat and lon
         if(slat.eq.0.0.and.slon.eq.0.0.and.height.eq.0.0) then
            write(6,'(a,a,a)')' Station ',stat(istat),
     *      ' not in station file'
            write(6,*)' Will use azimut+180 for back azimuth'
            bazim=ipar+180
            if(bazim.gt.360) bazim=bazim-360.0
         else
            call azibazi(elat,elon,slat,slon,delta,azim,bazim) ! calculate baz
         endif
         write(write05,207,iostat=code) stat(istat),float(ipar),
     *   bazim 
         call sei code(fort$,code,write05,b_eof)
 207           format
     *         (' SYNT: STATION: ',a5,4x,'  AZIMUTH:',
     *          f10.1,' BAZIMUTH:',f10.1,14x,'3')
               goto 151
            endif
         enddo
         write(6,'(a,a)')' No azimuth for station',stat(istat)
 151     continue



      enddo
c     call sei close (close$,read03,code)
      call sei close (close$,write05,code)
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine composite_foc
c
c  save data for a composite fault plane solution in one file
c  focmec.inp
C
c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! File operations.
       include 'seidim.inc'                ! dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c--data array for one event
      CHARACTER*80 DATA(max_data)		
C  NUMBER OF DIFFERENT STATIONS IN DATA
      INTEGER NSTAT			
C  NUMBER OF HEADERS AND RECORDS IN DATA
      INTEGER NHEAD,NRECORD		
C  ANGLE OF INCIDENCE
      REAL ANGINC			
      integer iang
C  NUMBER OF DIFFERENT PHASE FOR EVENT
      INTEGER NPHASE			
      INTEGER I,id			
c---event type etc
      character*1 type,exp
c-- one line in print file
      character*80 oneline         	
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 .. #6
       integer          read04, read05
c write unit #1 .. #5
       integer          write04
c
c   open print file
c
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     'print.out',           ! File name
     &                     read05,                ! Read unit #5
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
c
c   open and read hyp.out file
c
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     'hyp.out',             ! File name
     &                     read04,                ! Read unit #4
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
      call INDATA(read04,nstat,nphase,NHEAD,NRECORD,TYPE,EXP,DATA,id)
      call sei close (close$,read04,code)
c
c  read forward to station lines in print file
c
 22   continue
            read(read05,'(a80)',iostat=code) oneline
            call sei code(fort$,code,read05,b_eof)
            if (b_eof) go to 99
      if(oneline(2:4).eq.'stn') go to 3
      go to 22     
c
c  station line found
c
 3    continue
c
c  updating starts at first phase line
c
      i=nhead			
 4    continue
            read(read05,'(a80)',iostat=code) oneline
            call sei code(fort$,code,read05,b_eof)
c
c   check for end of phases
c	  
      if(oneline(1:13).eq.'          ') go to 99 
c	  
c   check if a station line
c
         if(oneline(1:4).ne.'    ') then  
c
c   check if normal phase line or az line
c
            if(oneline(26:27).ne.'AZ') then
               i=i+1
               READ(oneline,101) anginc
101            FORMAT(17x,f6.1)              ! was 18x, feb 97
               data(i)(58:60)='   '	
               iang=anginc+0.5
               WRITE(DATA(I)(58:60),'(I3)')iang
            endif 
         endif
c
c   back for next phase
c
      goto 4
c
c
c   end of file
c
 99   continue
      call sei close (close$,read05,code)
c
c   open and write out focmec file
c
           chr_f_access$ = 'append'
              call sei open(old$+ignore$,      ! Open a existing file.
     &                      ' ',               ! Prompt file name (n/a).
     &                      'focmec.inp',      ! File name
     &                      write04,           ! Write unit #4
     &                      b_old,             ! Already exists? (n/a).
     &                      code)              ! Returned condition.
              if (code .ne. e_ok$) go to 200

      write(6,*)' You are appending to an existing focmec.inp file'
      goto 210
 200  continue  
c
c  must be a new file, open as new and write whole event
c  only write out P-phases
c
              call sei open(new$+warn$,        ! Open a new file.
     &                      ' ',               ! Prompt file name (n/a).
     &                      'focmec.inp',      ! File name
     &                      write04,           ! Write unit #4
     &                      b_old,             ! Already exists? (n/a).
     &                      code)              ! Returned condition.
         write(write04,'(a80)',iostat=code) 
     *        (data(i),i=1,nhead)
         do i=nhead+1,nrecord-1
            if(data(i)(11:11).eq.'p'.or.data(i)(11:11).eq.'P')
     *      write(write04,'(a80)',iostat=code) 
     *      data(i)
         enddo
         call sei code(fort$,code,write04,b_eof)
         call sei close (close$,write04,code)
      return                                   ! return added  feb 97
c
c   already some data, only write phase lines
c
 210  continue
         do i=nhead+1,nrecord-1
            if(data(i)(11:11).eq.'p'.or.data(i)(11:11).eq.'P')
     *      write(write04,'(a80)',iostat=code) 
     *      data(i)
         enddo
         call sei code(fort$,code,write04,b_eof)
         call sei close (close$,write04,code)
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine check_s_file(s_file,data)
c
c   checks the s-file for errors, if err=0, no errors
c   file name is s_file, and data is in data, only there to not redefine
c   so space is saved
c
c   variables as in eev
c
      implicit none
      include 'libsei.inc'
      integer unit,code,nstat,nphase,nhead,nrecord,id,err
      character*80 data(*)
      character*80 s_file
      logical b_old
      character*1 evid,exp

              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      s_file  ,              ! File name
     &                      unit   ,               ! Write unit 
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition

         call indata(unit,nstat,nphase,nhead,
     *   nrecord,evid,exp,data,id)
         call sei close (close$,unit,code)
c
c   check all fields in s-file
c
c   if output destination eq file (1 or 2), unit number must be changed
         call check_s(data,                      ! file
     &                nrecord,                   ! number of records
     &                s_file,                    ! file name
     &                err,                       ! number of errors
     &                0,                         ! output destination
     &                0)                         ! unit number
c
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine read_s_file_e(sfile)

c
c   open and read s-file, close, for eev
c
c   see main program for variables
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      character*(*) sfile       ! fix jh sep 2016
      integer read_unit
      logical b_flag
      integer code,nstat,nphase,id,nrecord,nhead
      character*80 data(max_data)
      character*1 exp,type
      common /eev/ data,nstat,nphase,nrecord,nhead,id,exp,type
      
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
      subroutine write_s_file_e(sfile)

c
c   open and write s-file, close 
c
c   see main program for variables
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      character*(*) sfile
      integer write_unit
      logical b_flag
      integer code,i,nrecord,nhead,nphase,nstat,id
      character*80 data(max_data)
      character*1 exp,type
      common /eev/ data,nstat,nphase,nrecord,nhead,id,exp,type
      
              call sei open( old$,                 ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  sfile,                ! This file.
     &                  write_unit,           ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
              write(write_unit,'(a80)')(data(i),i=1,nrecord)
             call sei close(close$,write_unit,code)    ! Close (stop on error).
       return
       end

       subroutine id_status(unit,ids)
c
c   find id line in file opened at unit unit and return status in ids
c
       implicit none
       integer unit,i
       character*3 ids
       character*80 text
c
       ids='   '
       do i=1,10000
         read(unit,'(a)',end=1) text
         if(text(80:80).eq.'I') then
            ids=text(9:11)
            goto 1
         endif
       enddo
c
 1     continue
       return
       end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       subroutine hyp_loc(status)
c
c   sends message to hyp for no write out on screen. locates event
c   and ask for a return to continue
c   if status is N, operation might be stopped
c
c  added jh aug 2010: if status is y on entry, no question
c
      implicit none
      character*200 text
      character*1 status
c
c   set text for no write out for hyp for some operations
c
      text='hyp non interactive'
c
c   make sure no old file
c
      open(95,file='hyp.out',status='unknown')
      close(95,status='delete')
c
      write(6,*)
      write(6,*)' **** now locating with hyp as a preparation ***'
      write(6,*)

      call put_seisan_message(text)
      call systemc('hyp',3)            ! locate
c
      if(status.ne.'o') then
         write(6,*)' If location not ok, result might be unpredictable'
         write(6,*)' Return to continue (y=return/N)'
         read(5,'(a1)') status
         if(status.eq.'n') status='N'
      else
         status='y'
      endif
      return
      end

      subroutine get_operator(operator)
      implicit none
      character*4 operator
c     logical linux
cxx jh
      if(operator.ne.' ') return  ! operartor already given 
c     if (linux) 
      call get_env_op(operator)
1     continue
      if(operator.eq.'    ') then
         write(6,*) 'Give operator code, max 4 characters'
         read(5,'(a)') operator
         goto 1
      endif
c     if (linux) 
      call put_env_op(operator)

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine write_idline(year,month,day,hr,min,isec,
     &   operator,regcode,idline)
c
c create id line
c
      implicit none
      integer YEAR,MONTH,DAY,HR,MIN,ISEC,i
      character*80 idline
      character*12 p_time
      character*14 proc_time
      character*3 regcode
      character*2 operator

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
      WRITE(IDLINE(9:11),'(A)')REGCODE

      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c


      Subroutine green_par(sfile,hdepth,ndepth,del_depth)
c
c
c   makes a model file green.par for greger greens function program using the  
c   seisan s-file with synt lines
c
c     input:  sfile		sfile
c             hdepth            depth
c             ndepth            number of depths starting with hdepth
c             del_depth         depth increment

      implicit none

c--- stations to model
      character*5 stat(300)  ! station codes
      real distance(300)     ! epicentral distance
      real delay(300)        ! delay, not used
      integer ndepth         ! number of depth to make files for
      real del_depth         ! depth interval between models

c
      character*80 sfile,data(5000)
      character*1 TYPE,EXP
      character*55 text
      real dph,dep         ! model depths
c  model
      real thick(50),vp(50),vs(50),dens(50),qp(50),qs(50)
      real thick1(50),vp1(50),vs1(50),dens1(50),qp1(50),qs1(50)
      integer npoints      ! number of points
      integer NSTAT,NPHAS,NHEAD,NRECORD,ID
      real sample_int      ! sample interval
      real vred            ! reduction velocity
      integer nlayer       ! number of layers
      real window,t0,time  ! times
      real depth           ! hypocenter depth in model
      real hdepth          ! hypocenter depth
      integer i,k,lmax,istat,kdepth
c
      
      vred=0.0
      npoints=0
      sample_int=0.0
      k = 0

      open(77,file=sfile,status='old')
      call indata(77,NSTAT,NPHAS,NHEAD,NRECORD,TYPE,EXP,DATA,ID)

c
c  read parameters from s-file hyp.out, change ?
c

      do i = 1, nhead
        if(data(i)(80:80).eq.'3'.and.data(i)(2:6).eq.'SYNT:') then
          if(data(i)(8:15) .eq. 'MODEL--:' )then 
            if(data(i)(16:25) .ne. '     THICK')then
              k = k + 1
              read(data(i)(16:79),'(6f10.0)')thick(k),vp(k),vs(k),
     +                          dens(k),qp(k),qs(k)
              if(qp(k) .eq. 0. )qp(k) = 600.
              if(qs(k) .eq. 0. )qs(k) = 300.
            endif
          endif
          if(data(i)(8:15) .eq. 'DEPTH--:')then
            read(data(i)(16:79),'(f10.0,i10,f10.0)')
     *      hdepth,ndepth,del_depth
          endif
          if(data(i)(8:15) .eq. 'NPOINTS:')then
            read(data(i)(16:79),'(i10)')npoints
          endif
          if(data(i)(8:15) .eq. 'TIMES--:')then
            read(data(i)(16:79),'(3(10x,f10.1))')window,t0,time
          endif
          if(data(i)(8:15) .eq. 'DT-Tsou:')then
            read(data(i)(46:55),'(f10.1)')sample_int
            if(sample_int.eq.0.0)then
               write(6,*) 'Sample rate not defined'
               stop
            endif
            sample_int=1.0/sample_int
          endif
          if(data(i)(8:15) .eq. 'REDVELO:')then
            read(data(i)(16:79),'(20x,f10.3)')vred 
          endif
        endif
      enddo
      nlayer=k
      if(npoints.eq.0) then
         write(6,*) 'NPOINTS not defined'
         stop
      endif

      if(vred.eq.0.0) then
         write(6,*)'Reduction velocity not defined'
         stop
      endif
c
c   find number of stations and stations to model
c
      nstat=0
      do i=1,nhead
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: STATION:')
     *   then
            do istat=1,nstat
              if(stat(istat).eq.data(i)(17:21)) goto 80
            enddo
c
c   station was not counted before
c
            nstat=nstat+1
            stat(nstat)=data(i)(17:21)
 80         continue
         endif
      enddo
c
c   get station data
c
       do istat=1,nstat
         do i=1,nhead
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3')
     *      then
               read(data(i)(36:45),'(f10.1)') distance(istat)
            endif
         enddo
      enddo
      depth=hdepth
      close(77)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  depth loop
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do kdepth=1,ndepth
      depth=hdepth + del_depth*(kdepth-1)      
c
      dep = 0.
      lmax = 0

c
c   check if hypocenter is at a layer interface or below model
c
      do i = 1,nlayer
        dep = dep + thick(i)
        if(depth .lt. dep .and. lmax .eq. 0)then
          lmax = i            ! layer of hypocenter
          dph = dep - depth   ! depth below interface below
        endif
        if(depth .eq. dep)then
          write(6,*)' Depth is:',dep
          write(*,*)' Focal depth on layer interface'
          write(*,*)' Change parameter file'
          stop
        endif
      enddo 

c
c   depth below model
c

      if(lmax .eq. 0) then
         write(6,*)'  Focal depth below model, change parameter file'
         stop
      endif
c
c   make atificial boundary where hypocenter is located
c
      k=1
      do i=1,nlayer
         thick1(k)=thick(i)
         vp1(k)=vp(i)
         vs1(k)=vs(i)
         dens1(k)=dens(i)
         qp1(k)=qp(i)
         qs1(k)=qs(i)

         if(i.eq.lmax) then  ! in hypocenter layer
            thick1(k)=thick(i)-dph
            k=k+1
            thick1(k)=dph
            vp1(k)=vp(i)
            vs1(k)=vs(i)
            dens1(k)=dens(i)
            qp1(k)=qp(i)
            qs1(k)=qs(i)
         endif
         k=k+1
      enddo
      nlayer=nlayer+1
                      
     
c
c---- Now write parameter file for greens function generation
c

      write(*,*)' Writing parameter file green.par'

      write(text(1:3),'(i3)') int(depth)
      if(text(2:2).eq.' ') text(2:2)='0'
      if(text(1:1).eq.' ') text(1:1)='0' 
      open(88,file='green.par'//text(1:3))   

      write(88,'(a,1x,a)')'.F.',sfile ! debug flag and sfile name
      write(88,'(a)')'    0   64'     ! frequency range for debug, fixed
      write(88,'(a)')'green.out'      ! output file
      text='    6.0      8.00       1  512 1024    0.500     5    1'
      write(text(11:17),'(f7.2)') depth  
      write(text(26:35),'(2i5)') npoints/2,npoints
c      sample_int=0.5
      write(text(39:44),'(f6.3)') sample_int
      write(text(47:50),'(i4)') nlayer
      write(88,'(a)') text
      write(88,'(a)')   ! mostly undocumented
     *'    1    1    1    1    1    1    1    1    1    1    0'
c
c   write model
c
      do i=1,nlayer    
        write(88,'(6f11.2)')
     *  thick1(i),vp1(i),vs1(i),dens1(i),qp1(i),qs1(i)
      enddo
      write(88,'(i5)') lmax+1   ! layer number below source
      write(88,'(a)') '  0.4000000E+03  1.500000E+00         0' ! undocumented
c
c   number of stations to model and phase velocities
c
      write(88,'(i5,a)') nstat,'  10000.0     30.0      1.2       1.0'  
c
c   station code, distance, delay time (not used) and reduction velocity
c
      if(vred.eq.0.0) vred=8.0
      do i=1,nstat
         delay(i)=0.0
         write(88,'(a5,3f10.2)') stat(i),distance(i),delay(i),vred
      enddo

      close(88)
      nlayer=nlayer-1  ! put back to read number of layers in org model
      enddo
      end


C########################################################################### 
  
c
       subroutine mt_par_inv
     *(data,nhead,depth,ndepth,del_depth,npoints,
     *nstat,stat,distance,azimuth,
     * mt_offset,mt_redvel,mt_rate,mt_flow,mt_fhigh,
     * mt_pole,mt_npasses,mt_dispvel,
     * mt_comp_use,mt_start_time_par,mt_window)
c
c   get a list of stations to be used for mt and other parameters needed
c   for the inversion parameter file. parameter in synt line in s-file
c  
c   input is seisan s-file  with synt lines

      implicit none

c--- stations to model
      character*5 stat(*)  ! station codes
      real distance(*)     ! epicentral distance
      real azimuth(*)      ! azimuth
      integer mt_offset(*) ! offset for observed traces
      logical mt_comp_use(3,*)  ! components to use, normally all
      character*19 mt_start_time_par ! start time of data in par file
      real mt_window       ! mt window
      integer nhead        ! number of headers
      real mt_redvel       ! reduction veleocity
      real mt_rate         ! sample rate
      real mt_flow,mt_fhigh ! filter
      integer mt_pole,mt_npasses
      character*12 mt_dispvel ! dispacement or velocity

      character*80 data(*)

      character*55 text

      integer npoints      ! number of points to invert for
      integer NSTAT        ! number of stations

      real window,t0,time  ! times
      real depth           ! hypocenter depth
      integer ndepth       ! number of depths
      real del_depth       ! depth increment
      integer i,k,istat
c
      
c
c  read parameters from s-file
c
      mt_redvel=0.0
      mt_rate=0.0
      depth=0.0
      npoints=0
      mt_rate=0.0
      mt_flow=0.0
      mt_fhigh=0.0
      mt_pole=0
      mt_npasses=0

      do i = 1, nhead
        if(data(i)(80:80).eq.'3'.and.data(i)(2:6).eq.'SYNT:') then
          if(data(i)(8:15) .eq. 'DEPTH--:')then
            read(data(i)(16:79),'(f10.0,i10,f10.0)')
     *      depth,ndepth,del_depth
            if(ndepth.le.0) ndepth=1
          endif
c
c   the number of points to use in the inversion
c
          if(data(i)(8:15) .eq. 'NPOINTS:')then
            read(data(i)(36:45),'(i10)')npoints   
          endif
          if(data(i)(2:15).eq.'SYNT: DT-Tsou:') then
            read(data(i)(46:55),'(f10.3)')mt_rate  
            if(mt_rate.eq.0.0) then          
               write(6,*)' Sample rate is zero in S-file'
               stop
            endif 
          endif

          if(data(i)(8:15) .eq. 'MT-D-V-:')then
            read(data(i)(17:28),'(a12)')mt_dispvel
          endif

          if(data(i)(8:15) .eq. 'TIMES--:')then
            read(data(i)(16:79),'(3(10x,f10.1))')window,t0,time
          endif

          if(data(i)(8:15) .eq. 'REDVELO:')then
            read(data(i)(36:79),'(f10.3)')mt_redvel
          endif
          if(data(i)(8:15) .eq. 'MT-FILT:')then
            read(data(i)(16:55),'(2f10.3,2i10)')
     *      mt_flow,mt_fhigh,mt_pole,mt_npasses
          endif
          if(data(i)(8:15) .eq. 'MTSTART:')then
            mt_start_time_par=data(i)(17:35)
            read(data(i)(46:55),'(f10.3)') mt_window
          endif
        endif
      enddo

c
c   find number of stations and stations to invert for, only count
c   stations with mt information
c
      nstat=0
      do i=1,nhead
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: STATION:'
     *    .and.data(i)(47:55).eq.'MTOFFSET:')
     *   then
            nstat=nstat+1
            stat(nstat)=data(i)(17:21)
         endif
      enddo
c      write(6,*) 'nstat',nstat

c
c   get station data
c
       do istat=1,nstat
         do i=1,nhead
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3'
     *     .and.data(i)(47:55).eq.'MTOFFSET:')  ! mt line
     *      then
               read(data(i)(36:65),'(f10.1,10x,i10)') distance(istat),
     *         mt_offset(istat)
c
c  components to use, can come in any order
c
               mt_comp_use(1,istat)=.false.
               mt_comp_use(2,istat)=.false.
               mt_comp_use(3,istat)=.false.
               if(data(i)(76:78).eq.'   ') then
                  write(6,'(a)')'At least one component must be used'
                  write(6,'(a,a)')'Station: ',stat(istat)
                  stop
               endif
               do k=76,78
                  if(data(i)(k:k).eq.'T') mt_comp_use(1,istat)=.true.
                  if(data(i)(k:k).eq.'R') mt_comp_use(2,istat)=.true.
                  if(data(i)(k:k).eq.'Z') mt_comp_use(3,istat)=.true.
               enddo
c
c   azimuth should be in the next line, but check
c
               if(data(i+1)(17:21).ne.stat(istat).and.
     *            data(i+1)(28:34).ne.'AZIMUTH') then
                  write(6,*)' Azimuth line for station missing: ',
     *            stat(istat)
                  stop
               else
                  read(data(i+1)(36:45),'(f10.1)') azimuth(istat)
               endif
            endif
         enddo
c
c   intially 0
c
      enddo  

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
       subroutine mt_par_inv_all
     *(data,nhead,depth,ndepth,del_depth,npoints,
     *nstat,stat,distance,azimuth,
     * mt_offset,mt_redvel,mt_rate,mt_flow,mt_fhigh,
     * mt_pole,mt_npasses,mt_dispvel,
     * mt_comp_use,mt_start_time_par,mt_window,mt_used)
c
c   get a list of stations listed in file for mt and other parameters needed
c   for the inversion parameter file. parameter in synt line in s-file
c   also use weighted out station are read indicated by mt_used
c 
c   input is seisan s-file  with synt lines

      implicit none

c--- stations to model
      character*5 stat(*)  ! station codes
      real distance(*)     ! epicentral distance
      real azimuth(*)      ! azimuth
      integer mt_offset(*) ! offset for observed traces in s-file
      integer mt_used(*)   ! 0 not used, 1  used
      logical mt_comp_use(3,*)  ! components to use, normally all
      character*19 mt_start_time_par ! start time of data in par file
      real mt_window       ! mt window
      integer nhead        ! number of headers
      real mt_redvel       ! reduction veleocity
      real mt_rate         ! sample rate
      real mt_flow,mt_fhigh ! filter
      integer mt_pole,mt_npasses
      character*12 mt_dispvel ! dispacement or velocity

      character*80 data(*)

      character*55 text

      integer npoints      ! number of points to invert for
      integer NSTAT        ! number of stations

      real window,t0,time  ! times
      real depth           ! hypocenter depth
      integer ndepth       ! number of depths
      real del_depth       ! depth increment
      integer i,k,istat
c
      
c
c  read parameters from s-file
c
      mt_redvel=0.0
      mt_rate=0.0
      depth=0.0
      npoints=0
      mt_rate=0.0
      mt_flow=0.0
      mt_fhigh=0.0
      mt_pole=0
      mt_npasses=0

      do i = 1, nhead
        if(data(i)(80:80).eq.'3'.and.data(i)(2:6).eq.'SYNT:') then
          if(data(i)(8:15) .eq. 'DEPTH--:')then
            read(data(i)(16:79),'(f10.0,i10,f10.0)')
     *      depth,ndepth,del_depth
            if(ndepth.le.0) ndepth=1
          endif
c
c   the number of points to use in the inversion
c
          if(data(i)(8:15) .eq. 'NPOINTS:')then
            read(data(i)(36:45),'(i10)')npoints   
          endif
          if(data(i)(2:15).eq.'SYNT: DT-Tsou:') then
            read(data(i)(46:55),'(f10.3)')mt_rate  
            if(mt_rate.eq.0.0) then          
               write(6,*)' Sample rate is zero in S-file'
               stop
            endif 
          endif

          if(data(i)(8:15) .eq. 'MT-D-V-:')then
            read(data(i)(17:28),'(a12)')mt_dispvel
          endif

          if(data(i)(8:15) .eq. 'TIMES--:')then
            read(data(i)(16:79),'(3(10x,f10.1))')window,t0,time
          endif

          if(data(i)(8:15) .eq. 'REDVELO:')then
            read(data(i)(36:79),'(f10.3)')mt_redvel
          endif
          if(data(i)(8:15) .eq. 'MT-FILT:')then
            read(data(i)(16:55),'(2f10.3,2i10)')
     *      mt_flow,mt_fhigh,mt_pole,mt_npasses
          endif
          if(data(i)(8:15) .eq. 'MTSTART:')then
            mt_start_time_par=data(i)(17:35)
            read(data(i)(46:55),'(f10.3)') mt_window
          endif
        endif
      enddo

c
c   find number of stations and stations to invert for, only count
c   stations with mt information
c
      nstat=0
      do i=1,nhead
         if(data(i)(80:80).eq.'3'.and.data(i)(2:6).eq.'SYNT:'.
     *    and.data(i)(8:15).eq.'STATION:'
     *    .and.data(i)(47:55).eq.'MTOFFSET:')
     *   then
            nstat=nstat+1
            stat(nstat)=data(i)(17:21)
            mt_used(nstat)=1
            if(data(i)(7:7).ne.' ') mt_used(nstat)=0
         endif
      enddo
c      write(6,*) 'nstat',nstat

c
c   get station data
c
       do istat=1,nstat
         do i=1,nhead
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3'
     *     .and.data(i)(47:55).eq.'MTOFFSET:')  ! mt line
     *      then
               read(data(i)(36:65),'(f10.1,10x,i10)') distance(istat),
     *         mt_offset(istat)
c
c  components to use, can come in any order
c
               mt_comp_use(1,istat)=.false.
               mt_comp_use(2,istat)=.false.
               mt_comp_use(3,istat)=.false.
               if(data(i)(76:78).eq.'   ') then
                  write(6,'(a)')'At least one component must be used'
                  write(6,'(a,a)')'Station: ',stat(istat)
c                  stop
               endif
               do k=76,78
                  if(data(i)(k:k).eq.'T') mt_comp_use(1,istat)=.true.
                  if(data(i)(k:k).eq.'R') mt_comp_use(2,istat)=.true.
                  if(data(i)(k:k).eq.'Z') mt_comp_use(3,istat)=.true.
               enddo
c
c   azimuth should be in the next line, but check
c
               if(data(i+1)(17:21).ne.stat(istat).and.
     *            data(i+1)(28:34).ne.'AZIMUTH') then
                  write(6,*)' Azimuth line for station missing: ',
     *            stat(istat)
                  stop
               else
                  read(data(i+1)(36:45),'(f10.1)') azimuth(istat)
               endif
            endif
         enddo
c
c   intially 0
c
      enddo  

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine update_mt_dreger(sfile,vr,strike,dip,rake)
c
c  update sfile from a dreger solution, call also be used to only
c  read paramters if sfile is blank
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      character*80 data(5000)             ! s-file with data in text array
      character*80 sfile                  ! input file
      character*80 text
      character*3  agency
      integer nstat
      real depth,mag,m0


      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      real vr                             ! varience reduction
      real max                            ! dummy
      integer i,k                         ! counter
      real strike,dip,rake                ! fps
      real mxx,mxy,mxz,myy,myz,mzz        ! mt


      nstat=0
c
c   read parameters from dreger output file
c
      open(77,file='mt_inv_redi.out',status='old',err=1)
      goto 2
 1    continue
      write(6,*)' No mt_inv_redi.out file'
      return
 2    continue
      
      read(77,'(a)',end=10) text
      if(text(1:6).eq.'Strike')then
        do i=1,15
          if(text(i:i).eq.';') text(i:i)=' '
        enddo
        read (text(8:11),*) strike
      endif
      if(text(1:3).eq.'Dip')then
        do i=1,15
          if(text(i:i).eq.';') text(i:i)=' '
        enddo
        read (text(5:8),*) dip
      endif
      if(text(1:4).eq.'Rake')then
        do i=1,15
          if(text(i:i).eq.';') text(i:i)=' '
        enddo
        read (text(6:9),*) rake
      endif
      if(text(1:6).eq.'VarRed') read(text(8:18),*) vr
      if(text(1:2).eq.'Mw')read (text(4:8),*) mag
c     if(text(1:5).eq.'Depth')read (text(7:9),*) depth
      if(text(1:5).eq.'Depth')read (text(7:13),*) depth
      if(text(1:3).eq.'Mxx')read (text(5:16),*) mxx
      if(text(1:3).eq.'Mxy')read (text(5:16),*) mxy
      if(text(1:3).eq.'Mxz')read (text(5:16),*) mxz
      if(text(1:3).eq.'Myy')read (text(5:16),*) myy
      if(text(1:3).eq.'Myz')read (text(5:16),*) myz
      if(text(1:3).eq.'Mzz')read (text(5:16),*) mzz   
      if(text(1:2).eq.'Mo') read (text(4:17),*) m0
c      if(text(1:4).eq.'Stat') mt_nstat=mt_nstat+1

      goto 2
c
c   file now read
c
 10   continue
      close(77)
c
c   convert to Newton meter, note Dreger use 10**20 Dyne-cm
      mxx=mxx*10.0**27    ! dreger scale and Dyne-cm to Nm
      mxy=mxy*10.0**27    ! dreger scale and Dyne-cm to Nm
      mxz=mxz*10.0**27    ! dreger scale and Dyne-cm to Nm
      myy=myy*10.0**27    ! dreger scale and Dyne-cm to Nm
      myz=myz*10.0**27    ! dreger scale and Dyne-cm to Nm
      mzz=mzz*10.0**27    ! dreger scale and Dyne-cm to Nm
      m0=m0*10.0**7       ! Dyne-cm to Nm
c
c   write mt in psmeca.in file
c
      open(66,file='psmeca.in',status='unknown',POSITION='APPEND')
      max=0.0
      if(abs(mxx).gt.max) max=abs(mxx)
      if(abs(mxy).gt.max) max=abs(mxy)
      if(abs(mxz).gt.max) max=abs(mxz)
      if(abs(myy).gt.max) max=abs(myy)
      if(abs(myz).gt.max) max=abs(myz)
      if(abs(mzz).gt.max) max=abs(mzz)
      k=log10(max)
      max=10.0**k
      write(66,'(f8.2,1x,f7.3,a3,6(f6.3,1x),i2,1x,f8.2,1x,f7.3,a7)')
     *vr,depth," 0 ",
     *mzz/max,mxx/max,myy/max,mxz/max,-1.*myz/max,-1.*mxy/max,
     *k-7,vr,depth," SEISAN"           !  -1.*m?? for spherical coordinates
      close(66)                        !  k-7 is Dyne-cm for psmeca -Sm
c
c   return of only readng parmeters
c
      if(sfile.eq.' ') return

c
c   update fault plane solution line, should be in rea block
c
      open(75,file=sfile,status='old')

      all=.true.                  ! read all parameters
c
c   read all parameters for one event 
c
      call rea_event_in(75,all,data,code)
      if(code.ne.0) then
         write(6,*) ' Error reading event s-file'
         return
      endif
      call get_agency_hyp(data(1)(21:21),agency)
      write(6,'(a,a)') 'Agency is ',agency
       text=' '
       write(text,'(3f10.1)')strike,dip,rake
       text(67:69)=agency
       text(71:80)='DREGER   F'

       do i=1,rea_nfault
          if(rea_fault(i)(71:80).eq.'DREGER   F') then
             rea_fault(i)=text     ! old solution written over        
             goto 50               ! only first valid solution updated
          endif
        enddo
c
c   no solution, add fps
c
      rea_nfault=rea_nfault+1
      rea_fault(rea_nfault)=text  
 50   continue
c
c   save moment tensor solution, check if overwrite of same method and agency
c
      do i=1, mt_nmt
        if(mt_method(i).eq.'DREGER '.and.mt_agency(i)(1:3).eq.agency) 
     *  then ! overwrite
           mt_year(i)=hyp_year(1)
           mt_month(i)=hyp_month(1)
           mt_day(i)=hyp_day(1)
           mt_hour(i)=hyp_hour(1)
           mt_min(i)=hyp_min(1)
           mt_sec(i)=hyp_sec(1)
           mt_val(1,i)=mzz
           mt_val(2,i)=mxx
           mt_val(3,i)=myy
           mt_val(4,i)=mxz
           mt_val(5,i)=mxy
           mt_val(6,i)=myz
           mt_lat(i)=hyp_lat(1)
           mt_lon(i)=hyp_lon(1)
           mt_depth(i)=depth
           mt_agency(i)=agency//'  '
           mt_method(i)='DREGER '
c           mt_nstat(i)=nstat        ! was not implmented
           mt_mag(i)=mag
           mt_mag_type(i)='W'
           mt_moment(i)=m0
           mt_coor(i)='C'
           goto 52        
        endif
      enddo
c
c   a new solution, add      
c
      mt_nmt=mt_nmt+1
      i=mt_nmt
           mt_year(i)=hyp_year(1)
           mt_month(i)=hyp_month(1)
           mt_day(i)=hyp_day(1)
           mt_hour(i)=hyp_hour(1)
           mt_min(i)=hyp_min(1)
           mt_sec(i)=hyp_sec(1)
           mt_moment(i)=m0
           mt_val(1,i)=mxx
           mt_val(2,i)=mxy
           mt_val(3,i)=mxz
           mt_val(4,i)=myy
           mt_val(5,i)=myz
           mt_val(6,i)=mzz
           mt_lat(i)=hyp_lat(1)
           mt_lon(i)=hyp_lon(1)
           mt_depth(i)=depth
           mt_agency(i)=agency//'  '
           mt_method(i)='DREGER '
c           mt_nstat(i)=nstat
           mt_mag(i)=mag
           mt_mag_type(i)='W'
           mt_moment(i)=m0
           mt_coor(i)='C'

 52   continue
c
c   add magnitude to prime event
c
      do i=1,rea_nmag
         if(hyp_mag_type(i,1).eq.'W'.and.hyp_mag_agency(i,1).
     *   eq.'DRE') then
             hyp_mag(i,1)=mag
             goto 61
         endif
      enddo
c
c   new magnitude
c
      rea_nmag=rea_nmag+1
      if(rea_nmag.gt.6) then
        rea_nmag=rea_nmag-1
        write(6,*)'Not room for Dreger magntude, only 6 magnitudes'
        goto 61
      endif
      hyp_mag_type(rea_nmag,1)='W'
      hyp_mag(rea_nmag,1)=mag
      hyp_mag_agency(rea_nmag,1)='DRE' 
 61   continue
c
c   write out
c

      rewind(75)
      call rea_event_out(75,all,data,code)    
      close(75)

      return
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_mt_dreger_output(nstat,stat,zcor)
c
c  read zcor from dreger output file
c
      implicit none

      character*80 text

      integer nstat
      integer zcor(*) 
      character*5 stat(*)
      integer i,k                         ! counter  

      nstat=0
c
c   read parameters from dreger output file
c
      open(77,file='mt_inv_redi.out',status='old',err=1)
      goto 2
 1    continue
      write(6,*)' No mt_inv_redi.out file, so no calculated zcor'
      return
 2    continue
      
     

      read(77,'(a)',end=3) text
      if(text(1:8).eq.'Station(') then
         nstat=nstat+1
         stat(nstat)=text(13:17)
         do i=1,5
           if(stat(nstat)(i:i).eq.'_') stat(nstat)(i:i)=' '
         enddo
c
c   fish out Zcor string
c
          k=index(text,'Zcor')
          read(text(k+5:k+9),*) zcor(nstat)
       endif
      goto 2
 3    continue
      close(77)

      return
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mt_write_traces
     *(mt_nstat,mt_stat,mt_rate,mt_nsamp,sfile)
c
c   read the file synt.out from greger inversion and makes a
c   helmberger out file with observed and synthetics
c   jh jan 2012
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i,j,k,m,n
      character*80 sfile
      integer mt_nstat         ! number of stations  
      character*5 mt_stat(*)   ! stations modelled 
      character*4 mt_comp(6)   ! components
      real mt_rate             ! sample rate
      integer mt_nsamp         ! number of samples 


      open(77,file='synt.out',status='old',err=50)
      goto 51
 50   continue
      write(6,*)
      write(6,'(a)')
     *'No synt.out file from inversion, it probably crashed'
      write(6,'(a)')
     *'Try running it by command tdmt_invc_seisan to get errror message'
      stop
 51   continue

      mt_comp(1)='RR T'
      mt_comp(2)='RR R'
      mt_comp(3)='RR Z'
      mt_comp(4)='SY T'
      mt_comp(5)='SY R'
      mt_comp(6)='SY Z'
c
c  check if file has correct number of samples
c
      read(77,*) i,k
      if(k.ne.mt_nsamp) then
         write(6,*) 'Seems a  wrong synt.out file'
         return
      endif
      rewind(77)
      wav_header_text(1)=' '                              
      open(78,file='synt_seis.out',status='unknown')
      call wav_init
      wav_header_text(1)(1:26)='Dreger, data left, -Zcor  '
      do k=1,mt_nstat
         read(77,*) m,n  ! channel number and number of samples, not used
         do j=1,6          ! 6 channels to write, 3 data and 3 synt        
            read(77,*) (signal1(i),i=1,mt_nsamp)
            m=-1
            if(k.eq.1.and.j.eq.1) m=6*mt_nstat  ! first time write number of channels
            wav_stat(1)=mt_stat(k)
            wav_comp(1)=mt_comp(j)
            wav_nsamp(1)=mt_nsamp
            wav_rate(1)=mt_rate        
            call write_chan_helm(1,m,78,mt_nsamp,sfile)
         enddo
      enddo
      close(77)
      close(78)
      return
      end
         


C########################################################################### 
   
C########################################################################### 
   
     
      subroutine register_event(evfile,reg_base,operator,keys)
c
c   register an event. the subroutine is intened for se, nearly identical to code found 
c   earlier in file.
c
c     evfile: sfile to be registered, full path name. the return value is 
c             the file name registred which can be different both in type
c             and data base location
c     reg_base: base to register event in. must be initilized in start of calling 
c               program to length 0, value is entred in subroutine
c     operator: operator, if not defined, question will be asked
c     keys: if blank, registration done, if SAME, no registration
c
  
      implicit none

       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimensions
       include 'seisan.inc'
       include 'waveform.inc'

      character*80 evfile

c-- event arrays                  
      character*80      data1(max_data),data2(max_data)   
c-- no of stations for event                        
      integer		nstat1,nstat2		
c-- no of phases for one event
      integer nphase1
c-- no of records for event                  
      integer     	nrecord1,nrecord2	
c-- no of headers for event                         
      integer		nhead1,nhead2		
c-- event ids--------------               
      character*1	exp1,exp2,evid1,evid2	
c-- operator code
      character*4 operator
      character*5 reg_base         ! database to register in
      character*4 cwavyear
      character*2 cwavmon
      character*80 from_mulplt     ! command from mulplt
c---time of making update
      character*12 p_time
      character*14 proc_time
c-- number of errors in s-file
      integer nerr
      logical b_flag,exist  ! for input output
c logical for existing file or not
       logical          b_old
       integer          write01,  write03, write04
c returned code
       integer          code
c-- ev. selection key             
      character*10      keys 
      character*80  wave_file   ! waveform file name
      character*400 txt	
c volcanic sub class
       character*6 sub_class
      character*3       new_type   ! new type of event
      character*1       old_type   ! old type of event
c-- answer
      character*1 choise
c-- see base                      
      integer           fstart
c flag
       logical request_flag
c-- old and new----               
      character*80      newfile   !,master 
c-- top directory name
      character*60 top_directory
c-- data base name                
      CHARACTER*40      BASE_NAME  
c-- event number                  
      integer           event_no,oldno,current_event_number,old_event_no
c-- dirctory separation character
      character*1 dchar
c-- indicator of computer used
          logical sun,pc,linux
c-- time interval chosen          
      CHARACTER*14      START_TIME,END_TIME 
c-- status of event search        
      integer           status  
c logical for end of file
       logical          b_eof
c-- new month indicator           
      integer           new_month  
c-- event id line number
      integer id
      integer sei clen            ! function to get string length
c file name
       character*80     chr_file
      integer from_eev     ! indicate to routine findevin that call is from eev


c-- answer                        
      character*1       answer  
       integer isec      ! seconds
       integer year,mon,day,hour,min   
      integer i,j,l,k
      common/eev/data1,nstat1,nphase1,nrecord1,nhead1,id,exp1,evid1
c
c  get defaults
c
         call dir_char(dchar)
  
         call topdir(top_directory)
      
         call computer_type(sun,pc,linux)

c
c   get start of event id
c
         call get_fstart(evfile,fstart)
c
c   read file
c
         call read_s_file_e(evfile)
c
c  check if any request lines, if so registration not allowed
c
         request_flag=.false.
         do i=2,nrecord1
           if (data1(i)(2:4).eq.'NET'.and.
     *       (data1(i)(76:78).eq.'REQ'.or.data1(i)(76:78).
     *       eq.'CRQ')) then
             request_flag=.true.
           endif
         enddo
c
c        if(request_flag) then
c lot 24/5/2005
         if(request_flag.and.reg_base(1:1).eq.'_') then
            write(6,*) 
     *      'You are not allowed to register, pending requests'
            write(6,*)' Enter to continue'
            read(5,'(a)') i
            request_flag=.false.
c            goto 10
            return
         endif
c
c
c   check that file not already has been registered
c
         if(id.gt.0) then
            if(data1(id)(9:11).eq.'REG'.or.data1(id)(9:11).eq.'REE'.
     *      or.data1(id)(9:10).eq.'UP' .or.data1(id)(9:11).eq.'REE'.
     *      or.data1(id)(9:11).eq.'UPD') 
     *      then
c added confirmation level, lot 25-7-2002
              if (confirmation_level.eq.1.) then
                write(6,*)' Event already registered, continue(y/n)'
                read(5,'(a)') answer
                if(answer.ne.'y'.and.answer.ne.'Y') then
                  keys(1:4)='SAME'
cx                  goto  10
                  return
                endif
              endif
            endif
         else
c
c add id line
c
            write(6,*)' No ID line, fixing it !!!'
            do k=nhead1+1,nrecord1,-1
              data1(k)=data1(k-1)
            enddo
            data1(nhead1+1)=data1(nhead1)
            nrecord1=nrecord1+1
            nhead1=nhead1+1
            id=nhead1-1
            read(evfile(seiclen(evfile)-18:seiclen(evfile)),
     &         '(i2,1x,i2,i2,1x,i2,3x,i4,i2)')
     &         day,hour,min,isec,year,mon
            data1(id)=
     &    ' ACTION:REE 02-11-22 00:01 OP:aut  STATUS:'//
     &    '               ID:20021122050017     I'
            call systime(p_time,proc_time)
            WRITE(data1(id)(13:26),'(A)')PROC_TIME
            write(data1(id)(61:74),'(i4.4,5i2.2)')
     &        year,mon,day,hour,min,isec

            write(*,'(a)') ' new ID line : '//data1(id)
c            write(6,*)' No ID line, fix it !!!'
         endif

c-------------------------------------------------------
c   check if id and file name ok
c-------------------------------------------------------

         if(id.ne.0) then 
            call check_id(evfile,evid1,data1(id),nerr)
            if(nerr.ne.0) then
               write(6,*)
     *         ' Filename and ID different or event type (L,R,D)'
               write(6,*)' not the same as in file name, fix it !!!'
               write(6,*)
               write(6,'(a,a,a,a)') ' ID and type ', data1(id)(61:74),
     *         ' ',evid1
               l=seiclen(evfile)
               write(6,'(a,a)')   ' S-file name ',evfile(l-18:l)
               write(6,*)' Return to continue'
               read(5,'(a)')answer
               keys(1:4)='SAME'
cxx               goto 10
               return
            endif
         endif

         write(6,*)' You are now about to register the current ',
     *   'event '
c in the data base.'
         write(6,*)
     *   ' File will be cleaned up and waveform files copied to WAV'
c only ask if confirmation wanted, 17-04-2002
         if (confirmation_level.eq.1.) then
           write(6,*)' Sure you want to register, (y/n) ?'
           read(5,'(a)') answer
           if(answer.ne.'Y'.and.answer.ne.'y') then
              keys(1:4)='SAME'
cxx              goto 10
              return
           endif
         endif

         write(6,*)                                                             
 1724    continue                                                               

c-------------------------------
c set database for registration
c-------------------------------

         if (seiclen(reg_base).eq.0) then
           write(6,*) ' Give 2-5 letter data base, ' //
     *       ' return for same base'
           read(5,'(a)') reg_base
           do k=1,5
             if (reg_base(k:k).eq.' ') reg_base(k:k)='_'
           enddo
         endif

         write(6,*)
     *   ' Change event type to L,R or D'
         write(6,*)
     *   ' Second character for event ID (e.g. E) and/or '//
     *   ' third for model (e.g. J) '
         write(6,*)' Return for no change ?'
         read(5,'(a3)') new_type                                                
         if(new_type(1:1).eq.'l') new_type(1:1)='L'                             
         if(new_type(1:1).eq.'r') new_type(1:1)='R'                             
         if(new_type(1:1).eq.'d') new_type(1:1)='D'                             
         if(new_type(2:2).eq.'e') new_type(2:2)='E'
         if(new_type(2:2).eq.'p') new_type(2:2)='P'
         if(new_type(2:2).eq.'v') new_type(2:2)='V'
         if(new_type(2:2).eq.'q') new_type(2:2)='Q'
         if(new_type(1:1).ne.'L'.and.new_type(1:1).ne.                         
     *   'R'.and.new_type(1:1).ne.'D'.and.new_type(1:1).ne.' ') then        
            write(6,*)' Wrong type ******'                                      
            goto 1724                                                  
         endif
         old_type=data1(1)(22:23)
         if (seiclen(new_type).le.0) new_type=old_type
c                                                                               
c   check if type is different, if so reset
c                                                  
         if(data1(1)(22:23).eq.new_type) then 
            write(6,*)' New type same as old, nothing changed'                  
         else
            if(new_type(1:1).ne.' ') data1(1)(22:23)=new_type ! put in new type
         endif
c
c put in model, lo 8/2/2011
c
         if (seiclen(new_type).eq.3) data1(1)(21:21)=new_type(3:3)

c
c if volcanic event, select subclass
c
         if (new_type(1:2).eq.'LV') then
           call select_volcano_subclass(sub_class)
c
c add line
c          
           data1(nhead1+1)=data1(nhead1)
           data1(nhead1)=' VOLC MAIN                              '
     &                //'                                       3'
           write(data1(nhead1)(12:17),'(a6)') sub_class
           nhead1=nhead1+1
         endif

c------------------------------
c set database for registration
c------------------------------

         if (seiclen(reg_base).eq.0) then
           write(6,*) ' Give 2-5 letter data base, ' //
     *       ' return for same base'
           read(5,'(a)') reg_base
           do k=1,5
             if (reg_base(k:k).eq.' ') reg_base(k:k)='_'
           enddo
         endif      
cxx
         write(6,*) new_type,old_type,reg_base

c--------------------------------
c   make new file name if needed
c--------------------------------

         if((new_type(1:1).ne.' '.and.old_type.ne.
     *   new_type(1:1)).or.reg_base(1:1).ne.'_') then                                                                  
c
c   make new file name
c                                            
c
c            write(6,*) 'make new name ',reg_base

c
c set event file name, newfile
c


            if (seiclen(evfile).gt.19) then   ! lot 04-06-2002
c event not in local database
              newfile=evfile                   
              if (reg_base(1:1).ne.'_') then
                k=seiclen(newfile)-32
                newfile(k:k+4)=reg_base(1:5)
              endif
              newfile(fstart+10:fstart+10)=new_type(1:1) 
            else 
c event in local database
              newfile=top_directory(1:seiclen(top_directory))
     &                //dchar//'REA'//dchar//reg_base(1:5)//dchar//
     &                evfile(14:17)//dchar//evfile(18:19)//
     &                dchar//evfile(1:seiclen(evfile))
              fstart=seiclen(newfile) - 18
            endif
c
c   check if file can be opened or if it already is there

c
 7331       continue
            call sei open(unknown$+warn$,          ! Open file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      newfile,               ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
            if (code .ne. e_ok$) then                ! check 
                write(6,'(1x,a,a)')' Cannot open file: ',newfile
                keys='SAME'

cx                goto 10
                return
            endif
c
c   check for duplicate event in base
c
             if(b_old) then                               ! check if file there
 7333           continue	  
                write(6,*)
                write(6,'(a)')
     *          ' File already exists in base, options are:'
                write(6,'(a)')
     *          ' Do not change type:                        Return'
                write(6,'(a)')
     *          ' Overwrite existing event:                       O'
                if(id.ne.0) write(6,'(a)')
     *          ' Create a new event in base, different ID:       N'
                read(5,'(a)') choise
                if(choise.eq.' ') then
                   write(6,*)' File type will not be changed'
                   keys(1:4)='SAME'
cx                   goto 10
                   return
                endif
                if((choise.eq.'n'.or.choise.eq.'N').and.id.ne.0) then
                   call inc_id(data1(id),newfile,fstart+18)
                   call sei close (close$,write01,code) ! close, a new will be tested
                   goto 7331                            ! test again
                endif
                if(choise.eq.'o'.or.choise.eq.'O') then
                   write(6,*)' Overwriting old event'
                   goto 8155   ! go to write out
                endif
                goto 7333                                ! no valid choise
             endif
c
c    write out
c
 8155    continue             ! from just above
               write(write01,'(a80)',iostat=code)
     *         (data1(i),i=1,nrecord1)
               call sei code(fort$,code,write01,b_eof)
               call sei close (close$,write01,code)
c
c check for request
c
              request_flag=.false.
              do i=2,nrecord1
                if (data1(i)(2:4).eq.'NET'.and.
     *            (data1(i)(76:78).eq.'REQ'.or.
     *             data1(i)(76:78).eq.'CRQ')) then
                  request_flag=.true.
                endif
              enddo

              write(6,'(a,a)')' New file       ', newfile(1:fstart+20)
              if (reg_base(1:1).ne.'_'.and.      ! lot 07-05-2002
     &            request_flag) then
c
c keep old file with Seisnet request lines only
c
                chr_file = evfile(1:fstart+20)
                call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
                do i=1,nrecord1
                  if ((data1(i)(2:4).eq.'NET'.and.
     &                (data1(i)(76:78).eq.'REQ'.or.
     &                 data1(i)(76:78).eq.'CRQ')).or.
     &                 data1(i)(80:80).eq.'1'.or.
     &                 data1(i)(80:80).eq.'I'.or.
     &                 data1(i)(80:80).eq.'7') then
                          if (data1(i)(80:80).eq.'1')then
                             data1(i)(22:22)=chr_file(
     &                       seiclen(chr_file)-8:seiclen(chr_file)-8)
                          endif
                          write(write01,'(a80)') data1(i)
                    endif
                enddo
                write(6,'(a,a)')' Keeping file:  ', evfile(1:fstart+20)                 
                call sei close (close$,write01,code)

              else
                                                                                
c                                                                               
c   delete old event if no Seisnet request lines
c
                chr_file = evfile(1:fstart+20)
                call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      chr_file,              ! File name
     &                      write01,               ! Write unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition.
                call sei close (delete$,write01,code)
                write(6,'(a,a)')' Deleted file:  ', evfile(1:fstart+20)                 
              endif
c                                                                               
c   update event list, not done for se                                                          
c                                                                               
cx              keys(1:3)='REN'                                                        
cx              CALL findevin
cx     *        (base_name,start_time,end_time,keys,from_eev,event_no,
cx     *        evfile,fstart,new_month,status)                                       
  
              evfile=newfile   ! new file name
c
         endif

c--------------------------------------
c  copy waveform files to wav
c--------------------------------------

         do k=2,nhead1
            if(data1(k)(80:80).eq.'6') then
                wave_file=' '
                txt=' '
                read(data1(k)(2:79),'(a)') wave_file(1:78)
             
c
c get waveform header info
c
                call wav_init
                call get_full_wav_name(wave_file,wav_filename(1))
              if(wav_filename(1).eq.' ') then
                  write(*,*) ' File not found: '//
     &              wav_filename(1)(1:seiclen(wav_filename(1)))

              else
                call read_wav_header(1)
                write(cwavyear,'(i4)') wav_year(1)
                write(cwavmon,'(i2)') wav_month(1)
                do i=1,2
                  if (cwavmon(i:i).eq.' ') cwavmon(i:i)='0' 
                enddo
  
                if( pc ) then
                  txt  = 'copy X' 

c                txt  = 'copy ' // wave_file(:seiclen(wave_file)) 
c     &                 // ' '
c     &                 // top_directory(:seiclen(top_directory)) 
c     &                 // dchar//'WAV'//dchar                    
c     &                 // wave_file(:seiclen(wave_file))
c
                else if( sun.or.linux ) then
                  txt  = 'cp   X' 
                else
                  chr_err_msg$ = 
     &'**** ERROR: could not determine the computer type'
                  call sei code( stop$, e_init$, 0, b_flag ) ! Halt program
                end if                                     !

                if(copy_wav_dir.eq.' ') then     ! allow to copy to database, same as in mulplt, lot 11-4-2002

                  txt = txt(1:seiclen(txt)-1) // 
     &                wav_filename(1)(:seiclen(wav_filename(1))) //
     &                ' '                                    //
     &                top_directory(:seiclen(top_directory)) //
     &                dchar//'WAV'//dchar                    //
     &                wave_file(:seiclen(wave_file))
                else

                  txt = txt(1:seiclen(txt)-1) //
     &                wav_filename(1)(:seiclen(wav_filename(1))) //
     &                ' '                                    //
     &                top_directory(:seiclen(top_directory)) //
     &                dchar//'WAV'//dchar                    //
     &                copy_wav_dir//dchar//cwavyear(1:4)//dchar //
     &                cwavmon(1:2)//dchar //
     &                wave_file(:seiclen(wave_file))
                endif


                write(6,'(1x,a)') txt                      ! show file name
c
c  check if file already there
c
                call sei open( check$,              ! Check file exists.
     &                    ' ',                 ! No prompt.
     &                    txt(7+seiclen(wav_filename(1)):seiclen(txt)),
     &                    0,                   ! Unit (n/a).
     &                    exist,               ! File exists?.
     &                    code )               ! Condition (n/a).
                if(exist) then
                   write(6,*)' File already transferred to WAV *****'
                endif
c
c now copy if not already there
c
                if(.not.exist) then
                   call systemc( txt,
     &                         seiclen(txt) )
c
c  check that file got there
c
                   call sei open( check$,        ! Check file exists.
     &                    ' ',                 ! No prompt.
     &                    txt(7+seiclen(wav_filename(1)):seiclen(txt)),
     &                    0,                   ! Unit (n/a).
     &                    exist,               ! File exists?.
     &                    code )               ! Condition (n/a).
                   if(exist) then
                      write(6,*)' File transferred to WAV **********'
                   else
                      write(6,*)' Failure to transfer to WAV ****'
                      write(6,*)' Return to continue'
                      read(5,'(a4)') i
                   endif
                   write(6,*)
                endif                          
              endif
            endif
         enddo 
c
c   check if current operator id given
c
         call get_operator(operator)
c 4723       continue
c            if(operator.eq.'    ') then
c               write(6,*) 'Give operator code, max 4 characters'
c               read(5,'(a)') operator
c               goto 4723
c            endif
c
c   get system time
c
            call systime(p_time,proc_time)
c
c   update id line
c
            if(id.gt.0) then
               WRITE(DATA1(ID)(31:34),'(A)')OPERATOR
               WRITE(data1(id)(13:26),'(A)')PROC_TIME
               WRITE(data1(id)(9:11),'(A)')'REE'
            else
               write(6,*)' No ID line !!!!!!!'
            endif
c
c   clean up junk lines from seisnet, automatic picks and old header lines
c
            k=2
            data2(1)=data1(1)
            if(new_type(1:1).ne.' ') data2(1)(22:23)=new_type ! put in new type, lot 17-10-2002
            data2(2)=data1(id)
            if (data2(1)(45:45).eq.'*') data2(1)(45:45)=' '
            do i=2,nrecord1

              if( (data1(i)(2:4).eq.'NET'.and.data1(i)(80:80).eq.'3'.
     *             and.data1(i)(76:78).ne.'REQ'.
     *             and.data1(i)(76:78).ne.'CRQ') 
     *        .or. data1(i)(80:80).eq.'I'.or. 
     *            (data1(i)(80:80).eq.'1'.and.data1(i)(46:48).ne.'PDE')
     *        .or.(data1(i)(80:80).eq.' '.and.data1(i)(16:16).eq.'A'
     *               .and.keep_auto.eq.0.)
     *        .or.(data1(i)(2:6).eq.'SPECA')
     *        .or.(data1(i)(80:80).eq.'3'.and.data1(i)(2:7).
     *         eq.'ACTION'))
     *        then
                 continue    ! skip line
              else
                 k=k+1
                 data2(k)=data1(i)
                 if(data1(i)(76:78).eq.'REQ'.
     *           or.data1(i)(76:78).eq.'CRQ') then
                    write(6,*) ' This event contains a SEISNET',
     *              ' request line, is NOT deleted'
                 endif
              endif
            enddo
            data2(k+1)=' '
            nrecord1=k+1
            do i=1,nrecord1
              data1(i)=data2(i)
            enddo
            call write_s_file_e(evfile)

c
c   optinally start a process
c
            if(reg_autoprocess_flag.eq.2) then
               write(6,'(a,a)') ' Run process(y/return=n)? '
     *            ,reg_autoprocess_name
               call flush (6)
               read(5,'(a1)') answer
               if(answer.eq.'y'.or.answer.eq.'Y') then
                   call systemc(reg_autoprocess_name,10)
               endif
            elseif (reg_autoprocess_flag.eq.1) then
               write(*,*) ' running '//reg_autoprocess_name
               call systemc(reg_autoprocess_name,10)
            endif


      return
      end     

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine p_t_gap
     *(strike1,dip1,rake1,strike2,dip2,rake2,delp,delt)
c
c  calculate the difference in P and T axis (delp delt) direction for two
c  fault plane solutions 1 and 2. the different is measures along a
c  great cirle between the points of P and T on the focal sphere
c
c  jh jan 2017
c


      implicit none
      real strike1,dip1,rake1   ! fps event1
      real strike2,dip2,rake2   ! ---------2
      real delp,delt            ! different in P and T
      real PTTP1(4),pttp2(4)    ! strike and dip of P and T
      real momten(6)            ! help variable, not used                                                                                 
      real pi,degrad 
      real ANGS1(3),angs2(3)    ! dip, strike and rake of fault plane solutions
      real ANGSa(3)             ! dip, strike and rake of auilary ------------
      real ANBTP(6)             ! help variable , not used
      real bazim,azim           ! azimuths, not used
      integer i

      PI = 4.0*ATAN(1.0)
      degrad=180.0/pi

c
c   calcualte P and T
c
      angs1(2)=strike1
      angs1(1)=dip1
      angs1(3)=rake1

      do i=1,3
         angs1(i)=angs1(i)/degrad
      enddo
c
c   calcualte T and P for fault 1
c
      call DSRIN (ANGS1,ANBTP,ANGSa,PTTP1,momten,PI)

      do i=1,4
         pttp1(i)=pttp1(i)*degrad
      enddo
c
c   write p and T
c
c          write(6,'(4f8.1)')pttp1


      angs2(2)=strike2
      angs2(1)=dip2
      angs2(3)=rake2

      do i=1,3
         angs2(i)=angs2(i)/degrad
      enddo

c
c   calcualte P and T for fault 2
c
      call DSRIN (ANGS2,ANBTP,ANGSa,PTTP2,momten,PI)

      do i=1,4
         pttp2(i)=pttp2(i)*degrad
      enddo

c
c   write p and T
c
c     write(6,'(4f8.1)')pttp2
c
c   calculate the difference in P
c
      call azibazi
     *(pttp1(2),pttp1(1),pttp2(2),pttp2(1),delp,azim,bazim)
c
c   calculate the difference in T
c
      call azibazi
     *(pttp1(4),pttp1(3),pttp2(4),pttp2(3),delt,azim,bazim)

c
c   assume that if P and T on opposite side, it is equivalent with
c   t or p +180

      if(180.0-delt.lt.delt) delt=180.0-delt
      if(180.0-delp.lt.delp) delp=180.0-delp

      return
      end
