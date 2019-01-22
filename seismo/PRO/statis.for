c   program to  make event statistics                  
c                                                                      
c   jens havskov, apr 93                                             
c                                                                  
c   uppdates                                                      
c   may 21, 93 by jh: count all phases
c   july 8  93      : version 3.0 ***********
c   nov 93          : upgrade
c   nov 14          : count more, more things
c   dec 1   94      : total phases
c   dec 1           : seidim.inc
C  dec 8        : Installed file & error handling (libsei)
c                   **********  version 5.0 ***********************
c   jan 10  95      : new fin ev in
c   jan 20          : bugs
c   sep 02, 96    lo: setting of default stations, 
c                     new output file (statab.out),
c                     calculation of LOCAL S corrected 
c   sep 07  98      : stations added (tron,rund,arcess,noress,svaess,norsar
c   march 99   jh   : -----------------   version 7.0  check -------------
c   march 23 99 bmt : add one argument to computer_type call (linux)                                                                  
c   oct 5       jh  : times were not 14 but 12 chars
c   may 10 2000 lo  : init file_input, was linux problem
c   apr 21 2004 jh  : fix so 5 char stations can be used
c   may 23        jh  : add STOK  and remove NRA0
c   sep 29 2010  jh : read from def file in DAT, no hardwired stations 
c   aug 09 2017  jh : fix bug when reading from a file, output date instead 
c                     of s-file name
c
       implicit 	none
                                                
       include 'seidim.inc'        ! dimentions
c-- event data                                     
       character*80	data(max_data)	
c-- exp indicator, answer                                         
       character*1	exp,answer		
c-- event type                                      
       character*1      type		
c-- number of recors for event                           
       integer		nrecord		
c-- number of header lines                                 
       integer		nhead		
c-- number of stations                                     
       integer		nstat		
c-- number of phases
       integer nphase
c-- start and end time of select      
       character*14    start_time,end_time 
c-- data base name if not rea (blank)               
       character*40	base_name	
c-- event file name                                   
       character*80	evfile		
c-- select key                                       
       character*10     key
       integer seiclen   ! length of text string		
c-- see subroutine find...       
       integer		status,new_month,fstart,event_no 
c-- switch for file or data base
       logical file_input
c-- counters for events, records etc.      
       integer nr,nd,nl,records,nevent	
c-- counters,etc                                                   
       integer 		i,k,irec,iwaw_more,istat,iwaw
c-- event type
       integer evtype
c-- true if file exists              
       logical          exist           
c-- stations to use in statistics
       character*5 station(500)
       character*80 stat_text
c-- wave form codes
       character*3 waw_code(100)
       integer nwaw_code(100)
       integer nwcode
c-- total number of phases local and tele
       integer nphases1,nphases2
c-- length of wave form file code
       integer wlen
c-- one station code
       character*5 stat,stat_single
c-- number of stations to use in statistics
       integer kstat
c-- counter of single station detections
       integer nsingle(2,500)
c-- counter for number of detections at each station
       integer nstation(2,500)
c-- counts number of detection for one event
       integer nevent_station
c-- conts number of event with one ore more stations
       integer nevent_sel
c-- detecting stations for one event
       character*5 event_station(500)
c-- old station
       logical  oldstat
c-- count all phases
       integer count_all
c-- number of events with no phases
       integer nophase
c-- number of events with one or several waveform files
       integer nwaw
c-- total number of wave form files
       integer nwaw_total
c-- number of events with only waveform files
       integer only_waw
c--number of events with 2 or more waveform files
       integer waw_2
c-- number of selected events which had more stations than used to select with
       integer nstat_more
       character*80 chr_text      ! Text string.
c-- Machine type
       logical pc,sun,linux
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code,                  ! Error encoder.
     &          sei real num               ! Get real number
C
       integer  write01,                   ! Output unit 1.
     &          write02,                   ! Output unit 2.
     &          write03,                   ! Output unit 3.
     &          read01,                    ! Input unit1.
     &          read02,                    ! Ditto 2.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       real sei real num                   ! Real number


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c
C
      call computer_type(sun,pc,linux)
      file_input=.false.

c
c  clear arrays
c
      do i=1,500
        station(i)='     '
        do k=1,2
           nstation(k,i)=0
           nsingle(k,i)=0
        enddo
      enddo
      do i=1,100
        waw_code(i)='   '
        nwaw_code(i)=0
      enddo
c
c  get stations
c
      write(6,*)' Give file name with stations to use, must have a .'
      write(6,*)'                    or'
      write(6,*)' Give stations, one pr line, enter to finish,'
     *,' enter for def file statis.def'
      read(5,'(a)') stat_text
      if(stat_text(1:5).eq.'    ') then 
        goto 59                   ! def file
      endif
c
c   check if a file
c
      k=index(stat_text,'.')
c
c----------------------------------------------------
c   read from a file
c
      if(k.gt.1) then
         i=1
          call sei open( old$+warn$,       ! Open file & warn of erro.
     &                   ' ',              ! Prompt (n/a).
     &                   stat_text,        ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
  55     continue
         read(read01,'(a5)',end=60) station(i)
         i=i+1
         goto 55
  60     continue
         kstat=i-1
         goto 80
      endif         
c
c--------------------------------------------------------
c  Read stations from keyboard
c
      i=1
      station(1)=stat_text(1:5)
  56  continue
      i=i+1
      read(5,'(a)') stat_text
      if (stat_text(1:5).eq.'    ') then
        kstat=i-1
        goto 80
      else
        station(i)=stat_text(1:5) 
        goto 56
      endif
c
c--------------------------------------------------------
c  read stations from def file, in local dir or DAT
c
 59   continue
      chr_text = 'statis.def'               ! Station filename.
      call sei get file( open$+ignore$,     ! Find & open file.
     &                      read02,             ! On unit.
     &                      code,              ! Code (n/a).
     &                      'DAT',             ! Alternative dir to search.
     &                      chr_text)          ! For station file.
c
      if( code .ne. e_ok$ ) then         ! Does not exist.
         chr_err_msg$ = chr_text(:seiclen(chr_text)) //
     &                     ' does not exist'
         call sei code( stop$,              ! Halt the program with user-.
     &                     e_misf$,            ! Message; dummy use of code.
     &                     0,                  ! Unit (n/a).
     &                     b_flag )            ! Flag (n/a).
       end if                             !

c
c  now read def file
c
         i=1
  70     continue
         read(read02,'(a5)',end=71) station(i)
         i=i+1
         goto 70
  71     continue
         kstat=i-1                                

 80    continue
       write(6,*) ' Number of stations ',kstat
       write(6,*)
c                                             
c   input base name and time interval                                      
c                                                                               
       write(6,*)' Base name or file name, return for default'                 
       read(5,'(a40)') base_name                                                
       i=index(base_name,'.')
       if(i.gt.1.and.
     * base_name(1:5).ne.'index'.and.base_name(1:5).ne.'INDEX') then
          file_input=.true.
       else
          write(6,*)' Start time'                                              
          read(5,'(a14)') start_time                                           
          write(6,*)' End time, return for end of month'                       
          read(5,'(a14)') end_time                                              
       endif
       count_all=0
       write(6,*)' Count all phases(y/n)'
       read(5,'(a1)') answer
       if(answer.eq.'y'.or.answer.eq.'Y') count_all=1
	                                                                    
c                                                                               
c  reset counters                                                               
c                                                                               
50     nl=0                                                                     
       nr=0                                                                     
       nd=0                                                                     
       nophase=0
       nevent=0                                                                 
       records=0                                                                
       nevent_sel=0
       nwaw=0
       nwaw_total=0
       only_waw=0
       waw_2=0
       nwcode=0
       nstat_more=0
c                                                                               
c   open output files                                                            
c                                                                               
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'statis.out',     ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'sta2waw.out',    ! Filename.
     &                   write02,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'statab.out',     ! Filename.
     &                   write03,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
c   open input file if  file input
c
       if(file_input) then 
          write(*,*) 'Input from file'
          call sei open( old$+warn$,       ! Open file & warn of erro.
     &                   ' ',              ! Prompt (n/a).
     &                   base_name,        ! Filename.
     &                   read02,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
       endif
                                           
c                                                                               
c  start read and write loop                                                    
c                                                                               
 5     continue                                                                 
       if(file_input) goto 8
c-- always use next event                                
       key='          '		
       CALL findevin                                                       
     * (base_name,start_time,end_time,key,0,                                    
     * event_no,evfile,fstart,new_month,status)                                 
c
c   check for end of data set or errors
c
       if(status.ne.0) goto 999                                                
c
       inquire(file=evfile,exist=exist)                         
       if (.not.exist) then                                          
          write(6,'(1x,a60,a)') evfile,' doesn''t exist.'         
          goto 5
       endif
c
c--------------------------------------------------------------------------
c now make statistics
c--------------------------------------------------------------------------
c
          call sei open( old$+warn$,       ! Open file & warn of erro.
     &                   ' ',              ! Prompt (n/a).
     &                   evfile,           ! Filename.
     &                   read02,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
 8     continue
       call indata
     * (read02,nstat,nphase,nhead,nrecord,type,exp,data,i)              
       if(nrecord.eq.0) goto 999
c      write(6,'(a)') data(1)(1:60)
c
c   find if any waveform files, and how many
c
       iwaw=0
       iwaw_more=0
       wlen=3
c 9-8-17       if(pc) wlen=1                         ! only one letter on pc
       do irec=2,nhead
          if(data(irec)(80:80).eq.'6') then
c
c   find network codes
c
             k=index(data(irec),'.')+1
             do i=1,nwcode                   ! record and count wave form codes
                if(data(irec)(k:k+wlen-1).eq.waw_code(i)(1:1+wlen-1)) 
     *          then
                  nwaw_code(i)=nwaw_code(i)+1
                  goto 45
                endif
             enddo
c
c   if here, code was not in register, add
c
             nwcode=nwcode+1
             waw_code(nwcode)(1:1+wlen-1)=data(irec)(k:k+wlen-1)
             nwaw_code(nwcode)=1             ! count that this was the first
 45          continue
c
             iwaw_more=iwaw_more+1
             nwaw_total=nwaw_total+1
             if(iwaw.eq.0) nwaw=nwaw+1
             iwaw=1                          ! do not count anymore this event
          endif
       enddo
c
c   count events with 2 or more wave form files and write out
c
       if(iwaw_more.gt.1) then
          waw_2=waw_2+1
          write(write02,'(a)')(data(i),i=1,nrecord)
       endif
c
c   count if event only has waveforms
c
       if(iwaw.eq.1.and.nstat.eq.0) only_waw=only_waw+1
c
       evtype=1
       if(type.eq.'D') evtype=2
c
c   find which stations are used for this events
c
       nevent_station=0
       do irec=nhead+1,nrecord - 1
          read(data(irec),'(1x,a5)') stat
c
c  make sure stations is only used once if that is the choice
c
          oldstat=.false.
          do i=1,nevent_station
             if(event_station(i).eq.stat) oldstat = .true.
          enddo
c
c   count all occurences of stations if count_all is 1
c
          if(.not.oldstat.or.count_all.eq.1) then
             do istat=1,kstat
                if(stat.eq.station(istat)) then
                   nstation(evtype,istat)=
     *             nstation(evtype,istat)+1
                   nevent_station=nevent_station+1
                   stat_single=stat
c
c   save stations found for this event for later write out and check for 
c   several occurences of station
c
                   event_station(nevent_station)=stat
                endif
             enddo
          endif
       enddo
c
c   check and record if a single station detection
c
c       if(nevent_station.eq.1) then    ! single station among selected
       if(nstat.eq.1) then             ! single station among any
          stat=data(nhead+1)(2:6)
          do i=1, kstat
             if(stat.eq.station(i)) 
     *       nsingle(evtype,i)=nsingle(evtype,i)+1
          enddo
       endif
c
c   count events selected with one or several of the stations asked for
c
       if(nevent_station.ge.1) nevent_sel=nevent_sel+1            
c
c   count events which had more stations than asked for
c
       if(nstat.gt.nevent_station.and.nevent_station.ge.1) 
     * nstat_more=nstat_more+1
c                                                                               
c   count total events etc                                                            
c                                                                               
       if(type.eq.'L') nl=nl+1                                              
       if(type.eq.'R') nr=nr+1                                              
       if(type.eq.'D') nd=nd+1
c
c   count stations with no phases
c                                  
       if(nstat.eq.0) nophase=nophase+1
       records=records+nrecord                                              
       nevent=nevent+1                                                      
c                                                                               
c  check event                            
c                                                                               
c                                                                               
c  print event sfile name if from base                                                             
c  
                                                                                   
       if(.not.file_input) write(6,'(1x,a60)') evfile  
                                         
cold       write(write01,'(a,1x,30a5)') evfile(fstart:fstart+16),
       write(write01,'(a,1x,30a5)') data(1)(1:20),
     *(event_station(i),
     * i=1,nevent_station)
      if(.not.file_input) call sei close(close$,read02,code)

c-- back for next event                                         
      goto 5
c
c  end
c
 999  continue
c                                                                               
c  print out statistics                                                         
c
c  statab.out:  table with station statistics only
c
      write(6,*)
      write(6,'(1x,5a11)')'Station    ',' Local Ev. ',
     *' Local S. ',' Distant E ', ' Distant S '
      write(write01,*)
      write(write01,'(1x,5a11)')'Station    ',' Local Ev. ',
     *' Local S. ',' Distant E ', ' Distant S '
      write(write03,'(a19)') 'Station,LM,LS,D,DS '

c
c   total number of event-stations, can be number of phases
c
      nphases1=0
      nphases2=0
      do i=1,kstat
         write(6,'(4x,a5,4i11)')station(i),nstation(1,i),
     *   nsingle(1,i),nstation(2,i),nsingle(2,i)
         nphases1=nphases1+nstation(1,i)
         nphases2=nphases2+nstation(2,i)
      enddo
      write(6,'(4x,a,i10,11x,i11)')'Total ',nphases1,nphases2
      write(6,*)
      do i=1,kstat
         write(write01,'(4x,a5,4i11)')station(i),nstation(1,i),
     *   nsingle(1,i),nstation(2,i),nsingle(2,i)
         write(write03,'(a5,a1,i6,a1,i6,a1,i6,a1,i6)')
     *     station(i),',',nstation(1,i)-nsingle(1,i),',',
     *     nsingle(1,i),',',nstation(2,i),',',nsingle(2,i)
      enddo
      write(write01,'(4x,a,i10,11x,i11)')'Total ',nphases1,nphases2
      write(write01,*)
c
c   wave for file statistics
c
       write(6,*)
       write(6,'(1x,a3,1x,a5)')'NET',' NWAW'
       do i=1,nwcode
          write(6,'(1x,a3,1x,i5)') waw_code(i),nwaw_code(i)
       enddo
       write(write01,*)
       write(write01,'(1x,a3,1x,a5)')'NET',' NWAW'
       do i=1,nwcode
          write(write01,'(1x,a3,1x,i5)') waw_code(i),nwaw_code(i)
       enddo
c
c   summary statistics
c
       write(6,*)
       write(6,201) nevent_sel,nstat_more,nophase,nwaw,only_waw,
     * waw_2,nwaw_total,nl,nr,nd,nevent,records                                      
       write(write01,*)
       write(write01,201) nevent_sel,nstat_more,nophase,nwaw,only_waw,
     * waw_2,nwaw_total,nl,nr,nd,nevent,records                                      
 201  format(
     *       ' Number of events selected with given stations ',i7,                            
     *     /,' Number of events selected with more than ---  ',i7,                            
     *     /,' Number of events with no phases               ',i7,
     *     /,' Number of events with waveforms               ',i7,
     *     /,' Number of events with only waveforms          ',i7,
     *     /,' Number of events with 2 or more waveforms     ',i7,
     *     /,' Total number of waveform files                ',i7,
     *     /,' Total number of local events                  ',i7                             
     *     /,' Total number of regional events               ',i7                             
     *     /,' Total number of distant events                ',i7                             
     *     /,' Total number of events                        ',i7                             
     *     /,' Total number of records                       ',i7)                            
      write(6,*)                                                                
      write(6,*)' Output files are: statis.out'                                   
      write(6,*)'                   statab.out ',
     *          '(station statistics only)'
      call sei close(close$+all$,read01, code)       ! Close all open files.
      stop                                                                      
      end                                                                       
                                                                                
                                                                               
