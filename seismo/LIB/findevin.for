      subroutine findevin                                                  
     *(base_name,start_time,end_time,key,from_eev,event_no,                    
     *evfile,fstart,new_month,status)                                           
c                                                                               
c   routine finds an event in event index file given input parameters           
c                                                                               
c   jens havskov dec 88                                                         
c                                                                               
c   latest updates:                                                                
c   jul 94 jh      : enable continous search with empty months
c   nov 24, 1994   : opening files changed
c   jan 95         : reorganizing completely so index files are not used
c   jan 18, 95     : bug
c   jan 31 96      : enable eev to go over months
c   mar 25 97      : stop for NEXT if end of list for local directory
c                    see comment 25-3
c   sep 5  97      : bug when deleting from index file, return with first
c                    event to eev without reading files again
C   sep 11         : continue into next month also if last part of month
c                    has no data
c
c   feb 98         : year 2000
c   sep            : ------------  version 7.0 check------------------------
c                    5 char base name
c   sep 17         : bugs with alt base, base ,,,,, to ,,
c   jan 4 99       : bug when going from one year to next
c   jan 6            one more of above
c   march 17       : data base logging
c   may 24  bmt    : bug fixed (Work Directory)
c   jun 10  jh     : modify so there can be a '-' in directory name, fstart cal.
c   jul 24         : improve date for index file, clean up
c   oct 29  jh     : change base_update: convert base to upper case, do not
c                    write if nevents=0
c   march29 00 jh  : allow to use year 1900
c   feb 17 02  jh  : give message if not allowed to update REA.LOG
c   mar 23         : replace seiopen with read-write 77 in log subroutines
c                    to avoid error message 'too many files open'
c   jun 16 05  jh  : check for error in s-file name
c   2010-05-04 pv  : changed some tab-formatted source line that gave warnings in f90
c   2011-05-01 jh  ; added option start to key to reset s-file list
c   2011-05-09 jh  : search for a normal data base directory structure under any given
c                    directory structure.
c   2011-06 03 jh  : fix out of range in i1
c   2012-04 27 jh  : add common block for se
c   2012 11 27 jh  : time back to year 0000
c   2013 01 28 jh  : also search for minute in d-function
c   2013 02 08 jh  : fix dimension problem
c   2014 03 14 pv  : cleanup warning
c   2017 10 01 jh  : put output to screen in array findevin_message in addition
c   2018 06 04 jh  : fix for bug when sfil returns a '.' for file name,
c                    bug to be fixed
c
c   input: base_name : Blank : local data base, also indicated by ,,       
c                      Data base name: Up to 5 letter name, this is the standard
c                         seisan data base name located under REA                                              
c                      A file name: an index file name:  
c                         an index file is identified to have a '.' or        
c                         be longer than 5 chars and not terminated by a /
c                      A directory name under which a normal structure is found: the
c                         directory name must be terminated by a /                          
c                     
c          start_time: time to find first event:c-y-m-d-hr-min-sec (7a2)          
c          end_time  : time of last event,-----------------------, if           
c                      blank default to end of month. If a local index file     
c                      is used, both start and end time can be blank,           
c                      indicating that the whole index file is used.            
c          key       : key to select event or index file update                 
c                                           Dxxhh  : go to day xx and hour hh 
c                                                    in current month or in. file                        
c                                           #xxx : go to event #xxx -------     
c                                                  also work without #
c                                           sxxx : find next event within xxx secs
c                                           next : go to next event             
c                                           blank: ----------------- 
c                                           back : back one event           
c                                           same : same event (return)
c                                           del  : delete event
c                                           ren  : read in event list     
c                                           start: reset s-file list to make sure files read 
c                                                  correspond to start date     
c          from_eev   : 0 normal operation, several months
c                       1 if call comes from eev, else 0
c                       2 call from eev, over several months and output                          
c                                                                               
c   output: evfile   : file name of event file found                            
c           fstart   : character # start of event id                            
c           event_no : event number                                             
c           new_month: indicate if a new month has been entered                 
c                      0: same month, 1: new month                              
c           status   : 0: event found, key is next, # or date                   
c                      1: event not found, -----------------                    
c                      2: end of index file, not data base                      
c                      3: end of time period                                    
c                      4: next index file missing, key: next                    
c                      5: wrong time interval                                   
c                      6: start time is blank when using a database, e.g. REA   
c                      9: something fatally wrong
c
C
       implicit none
c
C    Seisan library inserts and routines...
C    ======================================
C
       save
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! Seisan dimensions    
       integer seiclen
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c
c--- total number of event names 
      integer total_no_events
c--- events without directory info
      character*19 sfiles(max_index$)
c--- year and month infor used with index files
      character*4 syear(max_index$)
      character*2 smonth(max_index$)
c-- directory where event files are, s-files
      character*80 sdir
c-- lenght of sdir
      integer slength
c-- call from eev indicator
      integer from_eev
c-- see above                                       
      character*14      start_time      
c-- start_time at return from call         
      character*14      old_start_time  
c-- same as start_time in time since 1900 
      double precision  time1           
c-- see above                                        
      character*14      end_time        
c-- end_time at return form call             
      character*14      old_end_time    
c-- same as end_time                      
      double precision  time2           
c-- current time                          
      double precision  time,tim1,tim2
c-- maximum time difference for associating 2 events            
      integer time_dif,old_time_dif
c-- see above                                        
      character*40      base_name       
c agency
      character*5 def_base
c-- base_name at return from call           
      character*40      old_base_name   
      character*80      text
c-- top directory of data base, def REA         
      character*5       base_dir        
c-- dirctory separation character
      character*1 dchar
c-- see above                                        
      character*10      key             
c-- see above                                     
      character*80      evfile          
c-- saved version of evfile               
      character*80      old_evfile      
c-- top directory of system                 
      character*60      top_directory   
c-- see above                                            
      integer           new_month       
c-- see above                             
      integer           status          
c-- date                                             
      integer year,month,day    
c-- 1: index file/list is open, 0: not open                  
      integer           index_open      
c-- counters                                           
      integer           i,j,i1,i2,q      
c-- start of ev id in full name string                    
      integer           fstart          
c-- index numbers                                         
      integer           old_no          
c-- event numbers                             
      integer event_no,ev_no_wanted     
c-- day number in month  and hour and min         
      integer ev_day_wanted,current_day,hour_wanted,current_hour,
     *min_wanted,current_min, abs_min_wanted, abs_min_current 
c logical for existing file or not
       logical          b_old
c logical for first closing file
       logical          first
c standard or non standard data base
       logical standard
c logical dummy
       logical ldummy
c returned code
       integer          code
c read unit #1 
       integer          read01

       integer          se_out                  ! if 1, call from se
       character*80     findevin_message(1000)  ! screen output from findevin
       character*80 message
       integer findevin_n_message                        ! number of messages
       common  /seisan_explorer/se_out,findevin_message,
     *          findevin_n_message


ctest  write(6,'(a6,1x,a5,1x,a5,1x)') ' base',base_name,old_base_name               
c        write(6,'(1x,a,1x,3i3)')                                             
c     * 'ichar base',(ichar(base_name(i:i)),i=1,3)                          
c      write(6,'(1x,a,1x,a,1x,a)')'st,ost',start_time,old_start_time           
c      write(6,'(1x,a,1x,a,1x,a)')'et,oet',end_time,old_end_time               
c      write(6,*)'status',status                                               
c      write(6,*)' fstart', fstart                                          
ctest         write(6,'(1x,a)') key
c                                                                               
c   set some defaults                                                            
c
c  check if from se
c
      if(se_out.ne.1) se_out=0
      findevin_n_message=0      
      first = .true.
      call get_def_base(def_base)
      call dir_char(dchar)
c
c   start from beginning 
c
      if(key(1:5).eq.'start') then
         index_open=0
         key=' '
      endif
c                                                                               
c-- clear new month flag                                      
c
      new_month=0                       
c                                                                               
c  top directory                                                  
c
	call topdir(top_directory)                             
c                                                                               
c  find which kind of data base: if 5 letters must be a normal              
c  type data base, if containing a . or larger than 5 letters
c  where last letter is noty a / must              
c  be an index file, which is indicated by a blank base_dir.
c  If ,, only s-files in local directory.               
c                                                                               
c-- default is no standard data base
c                
      base_dir=base_name(1:5)   
      if(base_name(1:5).eq.'     ') base_dir=def_base    
c
c   cases where an index file is assumed indicated by base_dir = '     '
c   or a file longer than 5 chars, or a . in name
c
      if(start_time.eq.'              ') base_dir='     '                                                         
      i=index(base_name,' ')
c 
c  check if index file, name not terminated by a '/' which is direct acces to 
c  a directory with structure
c
      if(i.gt.1) then  ! make sure no test if i=1, out of bounds
        if(i.gt.6.and.base_name(i-1:i-1).ne.'/') base_dir='     '
      endif   
      do i=1,40                                                                 
         if(base_name(i:i).eq.'.') base_dir='     '                                
      enddo             
c
c  make shure a real base name has 5 non blank characters
c
      if(base_dir(1:1).ne.' '.and.base_dir(1:2).ne.',,') then
        do i=2,5
          if(base_dir(i:i).eq.' ') base_dir(i:i)='_'
        enddo
      endif          
c
c   case of local data base
c
      if(base_name(1:2).eq.',,') base_dir=',,   '        ! no base, local dir                                          
c
c   save base_dir
c
      call put_env_base(base_dir)
c                                                                               
c   if start time has no month, put in january                                  
c                                                                               
      if(start_time(5:6).eq.'  ') start_time(5:6)='01'                          
      do i=7,14                                                                 
         if(start_time(i:i).eq.' ') start_time(i:i)='0'                          
      enddo                                                                     
c                                                                               
c   If start time year is blank, should be first event in an index file,   
c   or local data base (events in local directory),
c   then start from year 0000. If end time in that case is also blank, go to      
c   end of file indicated by year 2099                                               
c                                                                               
c     write(6,*)' pv: findevin : l244: end time : ' ,end_time
      if(start_time(1:2).eq.'  ') then                                          
	     if(base_dir.ne.'     '.and.base_dir(1:2).ne.',,') then
	       status=6
	       return
	    else
	       start_time(1:4)='0000'
	       if(end_time(1:2).eq.'  ')  end_time(1:4)='2099'
	     endif
      endif
c            
c   if end time year is blank, must be to end of month, put in                  
c   same year-month as start time                                               
c                                                                               
c-- end of month   
      if(end_time(1:2).eq.'  ')  end_time(1:6)=start_time(1:6) 
c                                                         
c   make sure end time is to end of month, day etc if left blank                
c                                                                               
      if(end_time(5:6).eq.'  ') end_time(5:6)='12'                              
      if(end_time(7:8).eq.'  ') then                                            
c      write(6,*)' pv: findevin : end time : ' ,end_time
      read(end_time(1:6),'(i4,i2)',err=7354) year,month
	  goto 7355
 7354 continue
	  write(6,*)' Something wrong with end time'
          message=' Something wrong with end time'
          call add_message(message)  
	  status=9
	  return
 7355 continue
      call month_day(year,month,day)                                         
      write(end_time(7:8),'(i2)') day                                        
      if(end_time(7:7).eq.' ') end_time(7:7)='0'                             
      endif                                                                     
      if(end_time(9:10).eq.'  ') end_time(9:10)='23'                              
      if(end_time(11:12).eq.'  ' ) end_time(11:12)='59'                           
      if(end_time(13:14).eq.'  ') end_time(13:14)='59'                          
ctest      write(6,*) end_time
c                                                                               
c-------------------------------------------------------------------------      
c   check if time interval or data base has changed since last call             
c   when the index list was loaded                                                  
c-------------------------------------------------------------------------      
c
      if(old_start_time.ne.start_time.or.old_end_time.ne.end_time.              
     *or.old_base_name.ne.base_name) then                                       
         if (.not. first) then
             continue
         else
             first = .false.
         end if
      index_open=0                                                           
      endif                                                                     
c-- save base characteristecs                     
      if(index_open.eq.0) then          
      old_start_time=start_time                                              
      old_end_time=end_time                                                  
      old_base_name=base_name  
      endif                                                                     
c      write(6,'(1x,a,1x,a)')' base_dir',base_dir
c      write(6,'(1x,a,1x,a,1x,a)')' stt  end t', start_time,end_time
c
c-------------------------------------------------------------------------      
c   check for file list update if there is index is open                   
c-------------------------------------------------------------------------      
c
      if((key(1:3).eq.'DEL'.or.key(1:3).eq.'REN')                              
     *    .and.index_open.eq.1) then               
          if(base_dir.ne.'     ') then   ! a data base   
             write(6,'(1x,i4,1x,i2,1x,$)') year,month ! this is not an error message
             call sfil(sdir,slength,sfiles,max_index$,
     *                 total_no_events, se_out)
c
c   fix if first file is wrong, jh 6-18
c
             if(sfiles(1)(1:1).eq.'.') then
                do i=1,total_no_events
                   sfiles(i)=sfiles(i+1)
                enddo
                total_no_events=total_no_events-1
             endif
c
c   update log file
c
             call base_update(total_no_events,base_dir,year,month)
c
             if(total_no_events.eq.0) then
                write(6,*)' No more events'
                message=' No more events'
                call add_message(message)  
                status=9
                index_open=0
                return
             endif
          else                         ! an index file
             if(key(1:3).eq.'DEL') then
                total_no_events=total_no_events-1
                if(total_no_events.eq.0) then   ! no more
                   write(6,*)' No more events'
                   message=' No more events'
                   call add_message(message)  
                   status=9
                   index_open=0
                   return
                else
                   do i=event_no,total_no_events
                      sfiles(i)=sfiles(i+1)
                      smonth(i)=smonth(i+1)
                      syear(i)=syear(i+1)
                   enddo
c
c   rewrite index file
c
                   call sei open(old$+warn$,            ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             base_name,             ! File name
     &             read01,                ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition
                   if (code .ne. e_ok$) then
                       write(6,*)' No such index file'
                       message=' No such index file'
                       call add_message(message)  
                       status=9
                       return
                   endif

                   do i=1,total_no_events
                      sdir(slength-7:slength-4)=syear(i)
                      sdir(slength-2:slength-1)=smonth(i)
                      write(read01,'(7x,a)')
     *                sdir(1:slength)//sfiles(i)
                   enddo
                   write(read01,'(a)') '                            '
                   call sei close (close$,read01,code)
                endif
              endif
          endif
c
c   position event at event after deleted event or at first
c   event if that was the last
c
          if(event_no.gt.total_no_events) event_no=1   ! at end of list
             if(slength.gt.0) then
                if(base_dir.eq.'     ') then    ! index file, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
                evfile=sdir(1:slength)//sfiles(event_no)
             else
                evfile=sfiles(event_no)
             endif
          old_evfile=evfile 
          status=0
          return
      endif                                                                     
c----------------------------------------------------------------------         
c   get list of files if not there, first check if base or special file            
c----------------------------------------------------------------------         
c                           
      if(index_open.eq.0) then                  
c                                                                               
c   first check if time interval is valid, get abs time from 0000               
c                                                                               
	    call abstim(start_time,0,time1)
	    call abstim(end_time,0,time2)
	    if(time1.ge.time2) then 
	       write(6,*)' ********* wrong time interval *************'
               message=' Wrong time interval'
               call add_message(message)  
	       status=5
	       return
	    endif
c                                                                               
c-- standard type s-file data base (standard true) or a given directory
c   (standard false)             
c
c      write(6,*)' base_dir,base_name ',base_dir,base_name

       ldummy=.FALSE.
           
 7365  continue     ! to here from section of positioning (goto 1)

	 if(base_dir.ne.'     '.and.base_dir(1:2).ne.',,'.OR.ldummy) then
             if(ldummy) goto 7366
c             i1=index(base_name,' ')-1
             i1=seiclen(base_name)
             if(i1.le.0) i1=1    ! make sure not zero
             if(base_name(i1:i1).eq.'/') then
                standard=.false.
                sdir(1:i1)=base_name(1:i1)
                i2=i1-11
             else
	        i2=index(top_directory,' ')-1
                standard=.true.       
                sdir(1:i2)=top_directory(1:i2)
                sdir(i2+1:i2+1)=dchar
                sdir(i2+2:i2+4)='REA'
                sdir(i2+5:i2+5)=dchar
                sdir(i2+6:i2+10)=base_dir    ! data base
                sdir(i2+11:i2+11)=dchar
             endif
	     sdir(i2+12:i2+15)=start_time(1:4)    ! year
             read(start_time(1:4),'(i4)',err=2736) year
             if(year.ge.2099.or.year.lt.0000) then
                write(6,*)'Wrong year'
                message=' Wrong year'
                call add_message(message)  
                stop
             endif
             sdir(i2+16:i2+16)=dchar
             sdir(i2+17:i2+18)=start_time(5:6)   ! month
             read(start_time(5:6),'(i2)',err=2736) month
             if(month.gt.12.or.month.le.0) then
                write(6,*)'Wrong month'
                message=' Wrong month'
                call add_message(message)  
                stop
             endif
             sdir(i2+19:i2+19)=dchar
             slength=i2+19
             goto  2737
 2736        continue
             write(6,*)' Something wrong with year or month'
             message=' Something wrong with year or month'
             call add_message(message)  
             stop
 2737        continue
c
c   get list of s-files
c
c            write(6,*) 'call sfil'
             write(6,'(1x,i4,1x,i2,1x,$)') year,month ! not an error message
             call sfil(sdir,slength,sfiles,max_index$,
     *                 total_no_events,se_out)
c
c   fix if first file is wrong, jh 6-18
c
             if(sfiles(1)(1:1).eq.'.') then
                do i=1,total_no_events
                   sfiles(i)=sfiles(i+1)
                enddo
                total_no_events=total_no_events-1
             endif
c
c   update log file
c
             call base_update(total_no_events,base_dir,year,month)
c
c
c-- flag index file is open                            
	     index_open=1                        
c-- must be a new month                                
             new_month=1                         
             event_no=0
c
c----------------------------------------------------------------------
c   check if no files, if so go to next month if
c   within time interval
c--------------------------------------------------------------------
c
 7363        continue     ! get here from below in this section to check
             if(total_no_events.eq.0) goto 7364
	     goto 1       ! there was data
 7364        continue
	     write(6,'(a,i4,1x,i2)')' No data for year and month: ',
     *       year,month  ! not error message
 7366        continue     ! to here from section of positioning (goto 1)
             ldummy=.FALSE.
c
c-- time of next month                             
c
	     month=month+1
	     if(month.eq.13) then
	        month=1
	        year=year+1
	     endif
c                
c   get abs time from 0000
c                        
	     call timsec(year,month,1,0,0,0.0,time)
c-- not yet end of desired time interval
	     if(time.lt.time2) then
c-- make new sdir
                write(sdir(i2+12:i2+15),'(i4)') year
                write(sdir(i2+17:i2+18),'(i2)') month    
                if(sdir(i2+14:i2+14).eq.' ') sdir(i2+14:i2+14)='0'
                if(sdir(i2+17:i2+17).eq.' ') sdir(i2+17:i2+17)='0'
                write(6,'(1x,i4,1x,i2,1x,$)') year,month  ! not error message
                call sfil(sdir,slength,sfiles,max_index$,
     *          total_no_events,se_out)
c
c   fix if first file is wrong, jh 6-18
c
             if(sfiles(1)(1:1).eq.'.') then
                do i=1,total_no_events
                   sfiles(i)=sfiles(i+1)
                enddo
                total_no_events=total_no_events-1
             endif
c
c   update log file
c
                call base_update(total_no_events,base_dir,year,month)
c

	        goto 7363    ! back to check again for empty file
c-- end of time interval                                           
	     else
	        status=3
	        return
	     endif
      endif               ! end section s data base
c
c-----------------------------------------------------------------------        
c   Files in local directory
c-----------------------------------------------------------------------
c
      if(base_dir(1:2).eq.',,') then   ! local directory only
         do q=1,79
          sdir(q:q)=' '
	   enddo 
         slength=0
         call sfil(sdir,slength,sfiles,max_index$,
     *             total_no_events,se_out)
c
c   fix if first file is wrong, jh 6-18
c
             if(sfiles(1)(1:1).eq.'.') then
                do i=1,total_no_events
                   sfiles(i)=sfiles(i+1)
                enddo
                total_no_events=total_no_events-1
             endif
c
c   update log file
c
          call base_update(total_no_events,base_dir,year,month)
c

          if(total_no_events.eq.0) then
             write(6,*)' No events in local directory'
             message=' No events in local directory'
             call add_message(message)  
             status=9
             return
          endif
          status=0
          index_open=1
          new_month=1
          event_no=0
       endif
c
c---------------------------------------------------------------------
c   Index file
c---------------------------------------------------------------------
c                                                                   
      if(base_dir.eq.'     ') then
              call sei open(old$+warn$,            ! Open a existing file.
     &                      ' ',                   ! Prompt file name (n/a).
     &                      base_name,             ! File name
     &                      read01,                ! Read unit #1
     &                      b_old,                 ! Already exists? (n/a).
     &                      code)                  ! Returned condition
         if (code .ne. e_ok$) then
            write(6,*)' No such index file'

c write(*,*) ' debug findevein ',seiclen(base_dir)
               message=' No such index file'
                call add_message(message)  
            status=9
            return
         endif
c
c-- flag index file is open                            
	     index_open=1                        
c-- must be a new month                                
	     new_month=1                         
             event_no=0
c
c   read file
c
         i=0
 77      continue
         i=i+1
c
c   check for max number of indexes
c
         if(i.gt.max_index$) then
            write(6,*) 'Too many lines in index file:',i
            message=' Too many lines in index file'
            call add_message(message)  
            stop
         endif
            
         read(read01,'(a)',end=78) text
         if(text(8:12).eq.'    ') goto 78
c        j=index(text,'-')-2
         call get_fstart(text,j)
         sfiles(i)=text(j:j+18)    ! save file name without path
         if(j.gt.8) then           ! only if 5 letter data base
            smonth(i)=text(j-3:j-2)   ! save month and year since it might
            syear(i)= text(j-8:j-5)   ! - change in index file
         endif
c
         if(i.eq.1) then
            if(j.gt.8) then             ! must have a path
               sdir(1:j-8)=text(8:j-1)  ! save path  test2000
c			   write(6,'(1x,a)') sdir
               slength=j-8
            else
               slength=0                ! local index file with no path. but
            endif                       ! is it used anymore ???, yes, 1-98 jh
            fstart=slength+1
         endif
         goto 77
 78      continue
c        write(6,*)'fs,sle',fstart,slength
         call sei close (close$,read01,code)
         i=i-1
         total_no_events=i
         if(i.eq.0.or.(i.eq.1.and.sfiles(1)(1:3).eq.'   ')) then
            write(6,*)' No events in index file'
            message=' No events in index file'
            call add_message(message)  
            index_open=0
            total_no_events=0
            status=9
            return
         else
            write(6,*)' Number of events in index file: ', ! not error message
     *      total_no_events
         endif
      endif
c
c-----------------------------------------------------------------------        
c  Loop to start time if a data base or first event if a local base 
c  or index file                                             
c-----------------------------------------------------------------------        
c
 1       continue          
         event_no=0
 111     continue
c         write(6,*)' loop to time'                                                     
             event_no=event_no+1
             if(event_no.gt.total_no_events) goto 2   ! at end of list, below
             if(slength.gt.0) then
                if(base_dir.eq.'     ') then    ! index file, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
                evfile=sdir(1:slength)//sfiles(event_no)
             else
                evfile=sfiles(event_no)
             endif
	         old_evfile=evfile 
c			 write(6,'(1x,a)') evfile                                                     
c                                                                               
c  check time of this event against start time.                                 
c                                                                               
c       fstart=index(evfile,'-')-2
        call get_fstart(evfile,fstart)
c        write(6,*)'fstart',fstart
       call abstim(evfile,fstart,time) 
c
c-- position not found or passed              
c
	 if(time.lt.time1) go to 111     ! loop above to try next event         
c
c-- position found to nearest sec                    
c
	 if(time.le.time2) then
	    status=0
c-- save index number
	    old_no=event_no
	    return
	 else
c-- first event found was after end time
	    status=3
	    index_open = 0
	    return
	 endif

c                                                                               
c-- end of list of files for month or in index file
c   if a data base, there might be more months to check since there
c   could be data in the next month
c
 2       continue                               
         if(base_dir.ne.'     ') then  ! not an index file
            ldummy=.TRUE.
            goto 7365                  ! check if next month has data in the
         endif                         ! - the interval
c
c-- position not found                                       
	 status=1
c-- at beginning of file, no is 1
	 old_no=1
	 return
      endif

c-----------------------------------------------------------------------------  
c  SAME EVENT # SELECTED                                                          
c-----------------------------------------------------------------------------  
c                                                                               
c  same event by number, can be a different event
c
      if(key(1:4).eq.'same'.or.key(1:4).eq.'SAME')then                          
         event_no=old_no
         if(event_no.eq.0) then
            write(6,*)' No more events'
            message=' No more events'
            call add_message(message)  
            status=9
            return
         endif
         if(event_no.gt.total_no_events) then
            event_no=1
         endif                                                                             
        if(slength.gt.0) then
                if(base_dir.eq.'     ') then    ! index file, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
           evfile=sdir(1:slength)//sfiles(event_no)
        else
           evfile=sfiles(event_no)
        endif
        old_evfile=evfile                                                   
c- save current event number                
	    old_no=event_no                     
        status=0
	  return                 
      endif                                                                     
c                                                                               
c---------------------------------------------------------------------------    
c  PREVIOUS EVENT SELECTED                                                      
c---------------------------------------------------------------------------    

      if(key(1:4).eq.'BACK'.or.key(1:4).eq.'back') then                         
c-- cannot backspace from beginning of list         
	     if(old_no.ne.1) then   
           event_no=event_no-1
           if(slength.gt.0) then
                if(base_dir.eq.'     ') then    ! index file, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
               evfile=sdir(1:slength)//sfiles(event_no)
           else
              evfile=sfiles(event_no)
           endif
	       old_evfile=evfile
c-- save current event number
	       old_no=event_no
	       status=0
	    endif
	    return
      endif

c---------------------------------------------------------------------------    
c  FIND EVENT BY NUMBER                                                         
c---------------------------------------------------------------------------    

c                                                                               
c   find event number in current index file                                     
c                                                                               
      if(key(1:1).eq.'#'.or.key(1:1).eq.'1'.or.key(1:1).eq.'2'.
     *   or.key(1:1).eq.'3'.or.key(1:1).eq.'4'.or.key(1:1).eq.'5'.
     *   or.key(1:1).eq.'6'.or.key(1:1).eq.'7'.or.key(1:1).eq.'8'.
     *   or.key(1:1).eq.'9'.or.key(1:1).eq.'0') then                                                  
	 if(key(1:1).eq.'#') then
	 if(key(2:2).ne.' ') read(key(2:2),'(i1)',err=374) ev_no_wanted
	 if(key(3:3).ne.' ') read(key(2:3),'(i2)',err=374) ev_no_wanted
	 if(key(4:4).ne.' ') read(key(2:4),'(i3)',err=374) ev_no_wanted
	 if(key(5:5).ne.' ') read(key(2:5),'(i4)',err=374) ev_no_wanted
	 if(key(6:6).ne.' ') read(key(2:6),'(i5)',err=374) ev_no_wanted
      else
	 if(key(1:1).ne.' ') read(key(1:1),'(i1)',err=374) ev_no_wanted
	 if(key(2:2).ne.' ') read(key(1:2),'(i2)',err=374) ev_no_wanted
	 if(key(3:3).ne.' ') read(key(1:3),'(i3)',err=374) ev_no_wanted
	 if(key(4:4).ne.' ') read(key(1:4),'(i4)',err=374) ev_no_wanted
	 if(key(5:5).ne.' ') read(key(1:5),'(i5)',err=374) ev_no_wanted
	 endif
	 if(ev_no_wanted.eq.0) ev_no_wanted=1
	 goto 375
 374     continue
	 write(6,*)' Number not valid'
         message=' Number not valid'
         call add_message(message)  
	 status=9
	 return
 375     continue
c                                                                               
c   start of search loop, one file only                                         
c                                                                               
             if(ev_no_wanted.gt.total_no_events) then
               if(total_no_events.eq.0) then
               write(6,*)' No more events'
               message=' No more events'
               call add_message(message)  
                  status=9
                  return
               endif
               write(6,*)' Number too large'
               message=' Number too large'
               call add_message(message)  
               status=1
               return
             endif
c            old_evfile=evfile   !!!????? should it be here ?????             
             event_no=ev_no_wanted
             if(slength.gt.0) then
                if(base_dir.eq.'     ') then    ! index file, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
                evfile=sdir(1:slength)//sfiles(event_no)
             else
                evfile=sfiles(event_no)
             endif
	    status=0
c-- save current index number                       
        old_evfile=evfile                
	    old_no=event_no                     
	    return
c-- end of number select block                                    
      endif                                     
c                                                                               
c-----------------------------------------------------------------------------  
c   FIND EVENT FROM DATE OF CURRENT MONTH                                       
c-----------------------------------------------------------------------------  
c                                                                               
c   go to nearest date in current month, local index file or local data base                     
c                                                                               

      if(key(1:1).eq.'d'.or.key(1:1).eq.'D') then
      hour_wanted=0
      min_wanted=0                               
      if(key(2:2).ne.' ') then
	    read(key(2:2),'(i1)',err=628) ev_day_wanted
	  endif
	  if(key(3:3).ne.' ') then 
	     read(key(2:3),'(i2)',err=628) ev_day_wanted                
	  endif
	  read(key(4:5),'(i2)',err=628) hour_wanted
          read(key(6:7),'(i2)',err=628) min_wanted
          abs_min_wanted=ev_day_wanted*24*60+hour_wanted*60+min_wanted
	  goto 629
 628     continue
	  write(6,*)' Invalid date'
          message=' Invalid date'
          call add_message(message)  
      return
 629     continue
c
c-- start searching from beginning if one month in regular data base,
c   if index file or local data base, find next event with EXACT match of day
c
         if(base_dir.ne.'     '.and.base_dir(1:2).ne.',,') event_no=0
 8       continue
            event_no=event_no+1            
            if(base_dir.eq.'     '.or.base_dir(1:2).eq.',,') then
               if(event_no.gt.total_no_events) event_no=1 ! wrap around                                                   
               if(event_no.eq.old_no) goto 9              ! gone round once, stop
            else
               if(event_no.gt.total_no_events) goto 9
            endif

             if(slength.gt.0) then
c
c   if an index file with a complete path, the data base year and  month
c   might change throughout the file. Since sdir only hase one name for the
c   first file, it must be corrected.
c
                if(base_dir.eq.'     ') then    ! index file, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
                evfile=sdir(1:slength)//sfiles(event_no)
             else
                evfile=sfiles(event_no)
             endif
        call get_fstart(evfile,fstart)

      read(evfile(fstart:fstart+6),'(i2,1x,2i2)',err=3847) 
     *current_day,current_hour,current_min
      abs_min_current=current_day*24*60+current_hour*60+current_min
      goto 3848
 3847 continue
      write(6,'(a,a)')' Error in s-file name ',evfile
      message=' Error in sfile name'
      call add_message(message)  
      stop
 3848 continue
c
c   check date and time
c
      if(base_dir.eq.'     '.or.base_dir(1:2).eq.',,') then ! local base or in.
          if(abs_min_current.ge.abs_min_wanted) then       
c         if(current_day.eq.ev_day_wanted.and.current_hour.ge.
c     *      hour_wanted) then   
	        status=0
	        old_no=event_no
	        return
	      endif
      else
c             write(6,*) 'cur, want', abs_min_current,abs_min_wanted
c	     if(current_day.ge.ev_day_wanted.and.current_hour.ge.
c     *      hour_wanted.and.current_min.ge.min_wanted) then
             if(abs_min_current.ge.abs_min_wanted) then                                  
	        status=0
	        old_no=event_no
	        return
	     endif
      endif                                                                  
c-- try next event                                            
	  go to 8                                
c-- no date larger then current                              
 9       continue                               
    	 evfile=old_evfile                                                      
        event_no=old_no
	  status=1
      return
c-- end of day select block                                       
      endif                                     

c                                                                               
c-----------------------------------------------------------------------------  
c   Find events associated in time
c-----------------------------------------------------------------------------  
c                                                                               
c   go to nearest date in current month or local index file                     
c                                                                               
										
      if(key(1:1).eq.'s'.or.key(1:1).eq.'S') then                               
c
c   get abstime  of current event
c
	 call abstim(old_evfile,fstart,tim1) 
c
c   get max time differnece, first set default if not set before
c
	 if(old_time_dif.le.0.or.old_time_dif.gt.9999) old_time_dif=180
	     if(key(2:2).eq.' ') time_dif=old_time_dif
	 if(key(2:2).ne.' ') 
     *      read(key(2:2),'(i1)',err=1628) time_dif
	 if(key(3:3).ne.' ') 
     *      read(key(2:3),'(i2)',err=1628) time_dif                
	 if(key(4:4).ne.' ') 
     *      read(key(2:4),'(i3)',err=1628) time_dif                
	 if(key(5:5).ne.' ') 
     *      read(key(2:5),'(i4)',err=1628) time_dif                
	 if(key(6:6).ne.' ') 
     *      read(key(2:6),'(i5)',err=1628) time_dif                
	 if(key(7:7).ne.' ') 
     *      read(key(2:7),'(i6)',err=1628) time_dif                
	 goto 1629
 1628    continue
	 write(6,*)' Invalid time difference'
         message=' Invalid time difference'
         call add_message(message)  
	 return
 1629    continue
c-- start searching from current position                           
 1608    continue
            event_no=event_no+1
            if(event_no.gt.total_no_events) goto 1609
             if(slength.gt.0) then
                if(base_dir.eq.'     ') then    ! index file, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
                evfile=sdir(1:slength)//sfiles(event_no)
             else
                evfile=sfiles(event_no)
             endif
	 old_evfile=evfile
        call get_fstart(evfile,fstart)
c
c   get abs time current event
c
	 call abstim(evfile,fstart,tim2)
c
c   check if within difference
c
	 if(abs(tim1-tim2).lt.time_dif) then                                  
	    status=0
	    old_no=event_no                                     
	    old_time_dif=time_dif                
	    return
	 endif
c-- try next event                       
	 tim1=tim2                     
	 go to 1608                             
c-- no more associations                              
 1609    continue                               
         
	 status=1
	 return
c-- end of time association block                                       
      endif                                     
c----------------------------------------------------------------------------   
c   GO TO NEXT EVENT                                                            
c----------------------------------------------------------------------------   
c                                                                               
c  go to next event                                                             
c                                                                               
      if(key(1:4).eq.'next'.or.key(1:4).eq.'NEXT'                               
     *.or.key(1:4).eq.'    ') then                                              
 5        continue                                                               
          event_no=event_no+1
          if(event_no.gt.total_no_events) then
             if(from_eev.eq.0.or.from_eev.eq.2) then ! eev over months
c                write(6,*)' goto 6'
                goto 6                  ! next month
             else
                event_no=1              ! start again
c
c   indicate wrap around with a blank line
c
             write(6,*) ! not error message
c                goto 5                  ! true january ??
             endif
          endif
          if(slength.gt.0) then
                if(base_dir.eq.'     ') then  ! 3 letter data base, fix year and month
                   sdir(slength-7:slength-4)=syear(event_no)
                   sdir(slength-2:slength-1)=smonth(event_no)
                endif
             evfile=sdir(1:slength)//sfiles(event_no)
          else
             evfile=sfiles(event_no)
          endif
          old_evfile=evfile                                                      
c-- save current event number                   
          old_no=event_no                        
c
c   if call from eev, return since time check not needed
c
          if(from_eev.eq.1) then
             status=0
             return
          endif
c                                                                               
c  check if inside time window

          call get_fstart(evfile,fstart)
          call abstim(evfile,fstart,time)                                        
c-- still within time window                    
	      if(time.le.time2) then                 
	         status=0
	         return
c-- end of time window                                  
          else                                           
	         status=3
	         index_open = 0
	         return
	      endif
c                                                                               
c-- end of month, go to next month                          
c
 6        continue                                   
	      index_open=0             
c          write(6,*)'base dir',base_dir              
         
c                                                                               
c  only continue if standard data base type                                     
c                                                                               
c             if(base_dir.eq.'   ') then  ! until 25-3-97
	      if(base_dir.eq.'     '.or.base_dir(1:2).eq.',,') then
c-- end of single index file                              
	         status=2
	         return
	      endif
c                                                                               
c-- year and month, time of next index file                             
c
          month=month+1                          
	      if(month.eq.13) then
	         month=1
	         year=year+1
	      endif
c                                                                               
c   get abs time from 0000                                                      
c                                                                               
	      call timsec(year,month,1,0,0,0.0,time)
c          write(6,*) year,month
c          write(6,*)'t,t2',time,time2
c-- not yet end of desired time interval       
	      if(time.lt.time2) then         
c-- make new sdir
             write(sdir(i2+12:i2+15),'(i4)') year
             write(sdir(i2+17:i2+18),'(i2)') month    
             if(sdir(i2+14:i2+14).eq.' ') sdir(i2+14:i2+14)='0'
             if(sdir(i2+17:i2+17).eq.' ') sdir(i2+17:i2+17)='0'
c             write(6,*)'new index',sdir,slength
              write(6,'(1x,i4,1x,i2,1x,$)') year,month ! not error message

             call sfil(sdir,slength,sfiles,max_index$,
     *                 total_no_events,se_out)
c
c   fix if first file is wrong, jh 6-18
c
             if(sfiles(1)(1:1).eq.'.') then
                do i=1,total_no_events
                   sfiles(i)=sfiles(i+1)
                enddo
                total_no_events=total_no_events-1
             endif
c
c   update log file
c
             call base_update(total_no_events,base_dir,year,month)
c

c
c   check if no events for new month, if so  write a message
c
             if (total_no_events.eq.0) go to 9364
c-- new month                                         
             new_month=1                                 
             event_no=0
	         index_open=1
             goto 5
 9364        continue
	         write(6,'(a,i4,1x,i2)')' No data for year and month: ',
     *       year,month
             write(message,'(a,i4,1x,i2)')
     *       ' No data for year and month: ',
     *       year,month
             call add_message(message)  
	         goto 5
c-- end of time interval                                           
	     else           
	        status=3
	        return
	     endif
      endif                                                                     
c
      return                                                                    
      end                                                                       
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                               

      subroutine base_address(year,month,base_no,pointer)
c
c   calulate address of data base info in data base log file
c   address is in terms of 4 byte pointers
c
      implicit none
      integer base_no      ! base no
      integer year, month
      integer pointer      ! address in log file
c
c pv  if(year.lt.1900.or.year.gt.2049) then
      if(year.lt.1900.or.year.gt.2099) then
         write(6,*)' Wrong year:',year
         stop
      endif
      pointer=((year-1900)*12+month-1)*100+200+base_no
c
      return 
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine base_get_bases(nbase,base)
c
c     get a list of all data bases (base) in data base, nbase
c     is the number of bases
c     if nbase is  -1, some error has occurred
c     if the central data base file does not exist, create it
c
      implicit none
      include 'libsei.inc'
      character*60 top_directory ! seisan top directory name
      character*800 bases     ! all bases in one string
      character*8   bas(100)  ! -----------
      character*5 base(100)   ! individual bases
      integer nbase           ! number of bases
      character*80 base_file  ! data base log file
      integer ini(100)        ! to initialize
      integer code            ! error code
      integer log_unit        ! unit to read and write from
      logical b_old           ! logical if file is old
      integer i
c
      equivalence (bases,bas)
c
c
c   get location of log file
c
      call topdir(top_directory)
      i=index(top_directory,' ')-1

      base_file = top_directory(1:i)//'/REA/REA.LOG'
      nbase=-1
c

c
c   open data base log file
c
      chr_f_access$='direct'
      f_recl$=800

      call sei open(old$+warn$,           ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             base_file,             ! File name
     &             log_unit,              ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition
      if (code .ne. e_ok$) then
        write(6,*)' No data base log file, now make it'
c
        chr_f_access$='direct'
        f_recl$=400
        call sei open(new$+warn$,         ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             base_file,             ! File name
     &             log_unit,              ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition

        if (code .ne. e_ok$) then
           nbase=-1
           return
        endif
c
        bases=' '
c
c   put blank for base names
c
        write(log_unit,rec=1) bases(1:400)
        write(log_unit,rec=2) bases(1:400)
c
c   put -1 for number of events pr month
c
        do i=1,100
          ini(i)=0
        enddo
c
c   initialize for 150 years, 150*12
c
        do i=3,1802
          write(log_unit,rec=i) ini
        enddo
        call sei close(close$,log_unit,code)                       
        nbase=0
        return
      endif
c
c-------------------------------------
c   a data base log file was present
c-------------------------------------
c
c
c   read data base names and find how many
c
      read(log_unit,rec=1) bases
      do i=1,100
        base(i)=bas(i)(1:5)
        if(base(i)(1:1).eq.' ') then
          nbase=i-1
c          write(6,*)' found',i
          call sei close(close$,log_unit,code)                       
         return
        endif
      enddo
c
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine base_update(nevents,base,year,month)
c
c     updata data base log file with new number of events, nevents 
c     for a particular year and month for a given base
c
      implicit none
      include 'libsei.inc'
      integer nevents,year,month
      character*5 base
      character*60 top_directory ! seisan top directory name
      character*5 bases(100)  ! max 100 data bases
      character*800 b800      ! -----------------
      character*8   bas(100)  ! -----------
      integer nbases          ! number of bases
      character*80 base_file  ! data base log file
      integer code            ! error code
      integer log_unit        ! unit to read and write from
      logical b_old           ! logical if file is old
      integer pointer         ! address of element with data base info
      integer ibase           ! number of the data base
      character*80 message
      character*1 ucase
      integer i
c
      equivalence (b800,bas)
      log_unit=77
c
c   if no events, return
c
      if(nevents.eq.0) return
c
c   if before 1900 retuen
c
      if(year.lt.1900) return
c
c   do not update a local data base or a local index file
c
      if(base(1:2).eq.',,'.or.base.eq.'     ') return
c
c   make sure only upper case is used
c
      do i=1,5
        base(i:i)= ucase(base(i:i))
      enddo

c
c   get location of log file
c
      call topdir(top_directory)
      i=index(top_directory,' ')-1

      base_file = top_directory(1:i)//'/REA/REA.LOG'
c
c  get number of data bases
c
      call base_get_bases(nbases,bases)
      if(nbases.eq.-1) goto 99
c     write(6,*)' Number of bases ',nbases
c
c  find the number of the data base to use
c
      ibase=0
      do i=1,nbases
        if(base.eq.bases(i)) then
           ibase=i
           goto 1
        endif
      enddo
 1    continue
c
c   check if data base was registered in data base log file, if not, do it
c
      if(ibase.eq.0) then
         ibase=nbases+1                     ! number of new data base
         nbases=nbases+1                    ! number of bases after adding one
c
c   open data base log file
c
c        chr_f_access$='direct'
c        f_recl$=800
c
c        call sei open(old$+warn$,        ! Open a existing file.
c    &             ' ',                   ! Prompt file name (n/a).
c    &             base_file,             ! File name
c    &             log_unit,              ! Read unit #1
c    &             b_old,                 ! Already exists? (n/a).
c    &             code)                  ! Returned condition
c                  if(code.ne.e_ok$) goto 99
         open(77,file=base_file,access='direct',recl=800)
c
c   put name in string and write it
c
         bases(ibase)=base
         b800=' '
         do i=1,nbases
           bas(i)(1:5)=bases(i)
           bas(i)(6:8)=' '
         enddo
         write(log_unit,rec=1,err=99) b800
c        call sei close(close$,log_unit,code)
         close(77)
         write(6,*)' Data base log file updated with a new data base'
      endif                       
c
c   open data base file 
c
c      chr_f_access$='direct'
c      f_recl$=4
c      call sei open(old$+warn$,          ! Open a existing file.
c    &             ' ',                   ! Prompt file name (n/a).
c    &             base_file,             ! File name
c    &             log_unit,              ! Read unit #1
c    &             b_old,                 ! Already exists? (n/a).
c    &             code)                  ! Returned condition
c                  if(code.ne.e_ok$) goto 99
c
       open(77,file=base_file,access='direct',recl=4)
c   calculate address to put info
c
      call base_address(year,month,ibase,pointer)
c
c  write info
c
c     write(6,*) 'nev,point',nevents,pointer
      write(log_unit,rec=pointer,err=99) nevents
c     call sei close(close$,log_unit,code)
      close(77)
c
      return
c
 99   continue
      write(6,*)' Error with data base log file, REA.LOG'
      write(6,*)' or you do not have write permission'
      message=
     *' Error with base log file, REA.LOG, maybe no write permission'
      call add_message(message)  

      return
      end

      subroutine get_fstart(filename,fstart)
c
c     find start of s-file name and check name
c
      implicit none
      integer fstart,sei clen,i1,i2,i3,i4
      character*80 filename,message
c
c 
      fstart=sei clen(filename)-18

      read(filename(fstart:fstart+18),'(i2,1x,i4,1x,i2,3x,i6)',  ! check name
     *err=1)i1,i2,i3,i4
      if(filename(fstart+2:fstart+2).ne.'-'.and.
     *   filename(fstart+7:fstart+7).ne.'-') then
         goto 1
      endif
      goto 2
c
c    errror
c
 1    continue
         write(6,*)' Corrupted S-file name, name is:'
         write(6,'(1x,a)') filename
         message=' Corrupted S-file name'//filename(fstart:fstart+18)
         call add_message(message)  
         write(6,*)' Return to stop'
         read(5,'(a)') filename
         stop

 2    continue

      return
      end

      subroutine add_message(message)
c
c  add message
c
      implicit none
      character*80 message
      integer          se_out                  ! if 1, call from se
      character*80     findevin_message(1000)  ! screen output from findevin
      integer findevin_n_message                        ! number of messages
      common  /seisan_explorer/se_out,findevin_message,
     *         findevin_n_message

      if(findevin_n_message.gt.1000) return
      findevin_n_message=findevin_n_message+1
      findevin_message(findevin_n_message)=message

      return
      end
