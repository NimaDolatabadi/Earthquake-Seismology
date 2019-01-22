c                                                                               
c   initial version april 21 1989                                               
c   latest updates:                                                             
c   jan 31 96     : operation over several months
c   feb 6         : -----------------------------, also in initial call
c   feb 14        : bug with del, put quit with type in again
c   jun 6, 96     : editor as a variable
c   aug 12        : possible to ignore no id line
C   nov-dec 96    : split program up in subroutine eev_act and main program
c                   eev in preparation for w95 version. Display more info
c                   at prompt line
c   feb 13  97    : small changes in output, UPD to UP, bug in FC
c   feb 26        : make output file indexeev.out, type file name
c   mar 25        : change format of inputone
c   sep 5  LAA    : length of string in edit
c   sep 7  jh     : add from_eev to eev_action routine
c   sep 18        : add options new and dub
c   oct           : option PP
c   nov 12        : option reg or put
c   nov 19        : option ppo (plot many,def),po(plot def) ,ss(search new)
c   dec  17       : bug with option 'NEW'
c   dec  30       : no chrash if empty file
c   feb           : option 'w'
c   feb 22        : fix probelme with PP, remove null char
c   feb 23        : moment tensor
c   feb 25        : redo PP option
c   following changes implement after first version 7.0 check, might not work
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   mar 1         : several faultplane solutions
c   april 1       : add processing status in output
c   april 7       : more clean up when register, check wav file status
c   may 20        : do not find NAO with SS, delte old id lines using reg
c   jun 5 98      : in option REG, also enter event id
c   jun 11        : remove routine increment_id and fix bug, also include
c                   status UPD as events considered registered
c   july  10 lo   : bug when starting editor after mulplt on Solaris
c   july  20 lo   : start epimap from eev
cccccccccccccccccccccccccccccccccccccccccccccc
c   sep 98 jh     : -----------version 7.0 check----------------------
c                   year 2000, 5 char base name, long file names
c   sep 17        : local base from ,,,,, to ,,
c   oct 16        : bug
c   oct 28        : bug with REG and option LP, inc_id
c   nov 4 bmt     : linux ok, add comma in FORMAT sentences        
c   nov 5 jh      : linux logiac, replace system_c with systemc
c   jan 5 99 jh   : bug with event type in option REG
c   jan 8         : bug with NEXT option
c   feb 15   lo   : change call for pitsa 
c   feb 23   lo   : interface to sac
c   mar 2    jh   : define event as new if autoreg ARG
c                   indicate by who and when file deleted
c   mar 30   lo   : start pitsa with command pitsa not i
c   april 13 jh   : do not use capital i in above
c   april 14 bmt  : add PMAC command (PROMAC)
c   june 16  jh   : do not append to same event
c   aug 13        : fix synt
c   aug 24  lo    : fix Eyyyymm option
c                   in option 'MAP' take location from S-file, if there
c   sep 5    jh   : change message if s-file deleted from mulplt
c                   bug in time association with chosen window due to option ss
c   sep 9         : put in CR whne from_mulplt true
c   sep 15        : fix problem with infinite loop if all events in index
c                   file non existent
c   sep 21        : eev over severalmonths was never implemented !!!
c                   add shwow_menu
c   sep 22        : bugs with updating of fault plane solution
c                   change mom to invrad
c   sep 29        : make shure event not updated if marked for no location
c   sep 30        : put in check for same id an ds-file name under edit
c   oct 1   lo    : change dub to dup
c   oct           : put subroutines to LIB
c   nov 2         : recompile to include new base structure
c   nov 28        : recompile to include fix of MAP option on PC, new
c                   options u and comment
c   jan 30 09 jh : initilize base name
c   nov 3  09 jh : remove argument in call print_ver, crashed
c   june 1 11 jh : do not crash on an empty s-file
c   apr 23 12 jh : add support for se
c   may 15 12 jh : all possible to give command from se
c   2016-03-09 pv: ask for operator id when eev is started
c   2016-05-19 pv: fix that eev was not showing negative magnitudes correct
c   2016-09-29 pv: add option to turn logging off

      implicit none                                                 
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'seisan.inc'                ! Open file definitions
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C

c-- base
      character*5 ba
c-- time interval chosen          
      CHARACTER*14      START_TIME,END_TIME     
c-- data base name                
      CHARACTER*40      BASE_NAME               
c-- input fron transfer file      
c      character*40      base_or_time            
c-- one line                                            
      character*80	text			
      integer from_eev     ! indicate to routine findevin that call is from eev
c-- unix command
      character*80 command
c-- event file name               
      character*80      evfile          
c-- ev. selection key             
      character*10      keys                    
c-- event number                  
      integer           event_no,oldno          
c-- status of event search        
      integer           status                  
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c-- new month indicator           
      integer           new_month               
c-- see base                      
      integer           fstart                  
c-- no of phases for one event
c      integer		mstatus			
c-- counter                                                    
      integer		i
c   arguments to eev
      character*80 arg(5)
c   number of arguments
      integer nargs
c-- operator code
      character*4 operator
c-- dirctory separation character
      character*1 dchar
      real sec      ! seconds
      character*80 from_mulplt   ! message from mulplt
      character*4  from_se       ! command from se
      character *3 ids           ! id status
      character *1 proc_status   ! processing status
      logical se                 ! true if call from se
	  integer isec
c read unit #1 .. #6
       integer          read01 
c indicator of the use of an index file
       logical index_file
       character*3 month(12)
       data month /'Jan','Feb','Mar','Apr','May',
     * 'Jun','Jul','Aug','Sep','Oct','Nov','Dec'/ 

c
      call print_ver
c
c   event number must be zero before anything has been defined
c
      event_no=0
      evfile=' ' ! lo 10/09/09
      from_mulplt=' '
      oldno=0
      index_file=.false.
      operator='    '      ! no operator given initially
      from_eev=1
      se=.false.
c
c   no debugging on file io
c
      b_f_debug$=.false.
      call dir_char(dchar)
      call get_def_base( ba )                 !JAB(BGS)Feb95. Get database.
c     write(6,*)' Def base :', ba      
      write(*,*)                              !JAB(BGS)Feb95.
c
c   get arguments
c
      call get_arguments(nargs,arg)
c
c   check if call from se
c
c      if(nargs.eq.3) then
         if(arg(3)(1:10).eq.'-seisanexp') then
            nargs=2     ! only use first 2 arg later
            se=.true.   ! signal call from se
            from_se=arg(4)(1:4) ! get command from se
            operator=arg(5)(1:4)
            if(from_se.ne.'SAME') write(6,*)
     *      ' EEV started from SE, not all commands can be used' 
         endif
c      endif
c
c  case of local directory
c
c      write(6,'(1x,a)') arg(1)
c      write(6,'(1x,a)') arg(2)
      if (nargs.eq.0) then
        write(6,*)' Local directory'
        base_name=',,   '     ! signal a local directory
c                                                                               
c-- start is very early
c              
        start_time='            '
      endif
c
c   data base or index file
c
      if (nargs.eq.1) then
        read(arg(1)(1:4),'(i4)',err=6353) i   ! if a number, must be data base
c
c  assume default data base
c
          base_name=' '                        ! jh jan 08
          base_name(1:5)=ba                    !JAB(BGS)Feb95..default dbase.
          start_time=arg(1)
          goto 6354
c
c  assume index file since one argument and not a number
c
 6353   continue
          base_name=' '                        ! jh jan 08
          i=index(arg(1),' ') -1
          base_name(1:i)=arg(1)(1:i)
          start_time='              '
          index_file=.true.
      endif
 6354 continue
c
c   end time is always to end of month if end time not given or index file
c
      end_time='            '
c
c   must be a base different from standard or interval
c
      if (nargs.eq.2) then
        start_time=arg(1)
c
c   check if arg2 is a number
c
        read(arg(2)(1:4),'(i4)',err=2736) i
        end_time=arg(2)                     ! assume it was a number        
        base_name(1:5)=ba                   !..default dbase.
        from_eev=2
        goto 2737
 2736   continue
        base_name=arg(2)                    ! assume 2 argument a data base
 2737   continue
      endif
c
c   must be a base different from standard with an interval
c
      if (nargs.eq.3) then
        start_time=arg(1)
        end_time=arg(2)
        base_name=arg(3)                    ! assume 3 argument a data base
        from_eev=2
      endif
c
c   delete eev.out and indexeev.out if there
c
            call sei open(unknown$      ,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    'eev.out',             ! File name
     &                    read01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            call sei close (delete$,read01,code)
c            call sei open(unknown$      ,        ! Open a unknown status file.
c     &                    ' ',                   ! Prompt file name (n/a).
c     &                    'indexeev.out',        ! File name
c     &                    read01,                ! Read unit #1
c     &                    b_old,                 ! Already exists? (n/a).
c     &                    code)                  ! Returned condition.
c            call sei close (delete$,read01,code)
c
c   get seisan defaults
c
      call get_seisan_def
c                                                                               
c   get operator id if logging is on
      if ( seisan_logging.GE.1 ) then
        call get_operator(operator)
      endif
c
C                                                                               
C  initial position                                                   
C                                                                               
      call findevin
     *(base_name,start_time,end_time,keys,from_eev,event_no,
     *evfile,fstart,new_month,status)
      call put_env_event(evfile)
c                                                                               
C   CHECK IF END OF TIME INTERVAL OR OTHER ERRORS                               
C                                                                               
      IF(STATUS.GT.0) THEN
         if(status.eq.1) write(6,*)' ***** event not found'
         if(status.eq.5) then
            write(6,*)' ***** wrong time interval'                 
            stop
         endif
c
c   if an empty index file, stop
c
         if(event_no.eq.0.and.status.eq.3) stop
         if(status.eq.9) stop   ! something wrong
      ENDIF                                                                     
c
c
c-------------------------------------------------------                  
c   start of interactive loop                                                   
c-------------------------------------------------------
c
 10   continue
c
c   open file to get main info
c
      open(1,file=evfile,status='old',err=1818)
      read(1,'(a)',end=1919) text
      goto 1819
c
c   errror
c
 
 1818 continue
      write(6,'(a,i5)')'   ****   Event No ',event_no
      write(6,*)' Event not present in S-file data base, file name is: '
      write(6,'(1x,a)') evfile
      write(6,*) 'Return to continue, q to quit'
      read(5,'(a)') text
      if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') stop
 1820 continue
      keys(1:10)='          '   ! go to next event
      close(1)
      goto 1000
c
 1919 continue
      write(6,'(a,i5)')'   ****   Event No ',event_no
      write(6,'(1x,a)') evfile
      write(6,*)' EMPTY event file'
      text=' '
c
 1819 continue
c
c   find id status
c
      if(text.ne.' ')   ! do not read from an empty file
     *call id_status(1,ids)
c
c   set flag for processing status
c
      proc_status=' '
      if(ids.eq.'NEW'.or.ids.eq.'SPL'.or.ids.eq.'HYP') proc_status='N'
      if(ids.eq.'   ') proc_status='?'   ! unknown status
c
      close(1)
c
C   display info and ask what to do next                                                          
C                                                                               
      read(text(17:20),'(f4.1)',err=1100) sec   ! second
      goto 1101
 1100 continue
      write(6,*)' Something wrong with second in header, set to 00'
      sec=0.0
c
 1101 continue
      isec=sec
      read(text(7:8),'(i2)',err=1300) i   ! month
      if(i.gt.12.or.i.lt.1) then
         write(6,*)' Month wrong in file, fix it !!!'
         write(6,*)' For this event set to month in start time'
         read(start_time(5:6),'(i2)') i
      endif
      goto 1301
 1300 continue
      write(6,*)' Something wrong with month in header, set to 01'
      i=1
 1301 continue

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
c
c   check what should be in magnitude field
c
      if(text(57:63).eq.'       ') then
         if(text(65:71).ne.'       ') then
           text(57:63)=text(65:71)     ! try 2. magnitude
         else
           text(57:63)=text(73:79)     ! try 3. magnitude
         endif
      endif
c
c   if nothing in magnitude field, put in location agency
c
      if(text(57:63).eq.'       ') then
        text(61:63)=text(46:48)
      endif     

      write(6,289) event_no,
     *text(9:10),             ! day
     *month(i),               ! month
     *text(2:5),              ! year
     *text(12:13),            ! hour
     *text(14:15),            ! min
     *isec,                   ! sec
     *text(21:23),            ! indicators
     *text(24:30),text(31:38),text(39:45), ! hypocenter
     *proc_status,
     *text(52:55),            ! rms
     *text(56:63),            ! first magnitude 2016-05-19 pv: fixed for neg mag
     *text(49:51)             ! number of stations
 289  format('#',i5,1x,a2,1x,a3,1x,a4,1x,a2,':',a2,1x,i2,1x,
     *a3,a7,a8,a7,a1,a4,1x,a8,1x,a3,'  ? ',$)

c

c
c   get next action unless given from mulplt or se with command
c
      if((from_mulplt(1:4).ne.'    '.or.se).and.from_se.ne.'SAME') then
         if(.not.se) then
            keys(1:10)=from_mulplt(1:10)
         else
            keys=' '
            keys(1:4)=from_se
            from_se='Q   '                 ! go back to se next time
         endif
         write(6,*)                        ! nothing was read so no CR, put it in
      else
         READ(5,'(A10,a)') KEYS,command                          
      endif

c
c   get here from above if file not there
c   
 1000 continue
c       
c  check if quit                                                                
c       
      if(keys(1:2).eq.'Q '.or.keys(1:2).eq.'q '.or.keys(1:4).
     *eq.'quit'.or.keys(1:4).eq.'QUIT') stop                                             
c
c  find what action to do
c
      call eev_action(base_name,start_time,end_time,keys,
     *command,event_no,evfile,fstart,
     *index_file,from_eev,from_mulplt,operator,se)
c
c   back to ask for next action
c
      goto 10
c
c    end
c
      stop
      end



 
