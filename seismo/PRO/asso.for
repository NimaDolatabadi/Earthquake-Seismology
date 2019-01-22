c--------------------------------------------------------------------------
c
c   program to associate and merge events in a catalog, 
c   must be a seisan data base
c
c   parameters are given in asso.def
c
c   The magnitude used are selected among all prime magnitudes in file,
c   which is the magnitudes on main header line (can be 2 lines if more
c   than 3 magnitudes).
c
c   The magnitudes can be given an order of priority in SEISAN.DEF:
c
c   KEYWORD............Comments.............Par 1.....Par 2
c   MAGNITUDE_ORDER                         LBER
c   MAGNITUDE_ORDER                         LNAO
c
c   In this exmaple, LBER is first chosen, if not there, LNAO and if not
c   there either, the first magnitude found in file. If no order is given
c   in SEISAN.DEF, the magnitrude used will be the first found for the 
c   event, irrespetive of type or agency. It is possible to leave either 
c   magnitude or agency blank in which case the blank represents a wildcard.
c
c   changes
c
c   dec 19 2013 jh: allow first event to be after second event but do not 
c                   copy phases from second event, do not stop in this case
c   mar 8  2015 jh: if blank depth, set depth to 20 km
c
c   
c--------------------------------------------------------------------------c
c
c  For more detail on parameters and variables naames, see rea.inc
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(10000)             ! s-file with data in text array
      character*80 data1(20000)
      character*80 err_text               ! error text
      character*80 infile                 ! input file
      integer id                          ! for s-file
      character*1 type,exp                ! for s-file
      integer debug                       ! 1: debug output, o: no
c
       character*14     start_time,end_time  ! time interval
       character*40     base_name
c data base name if not rea (blank)
       character*80     evfile
       integer nstat1,nphase1,nhead1,nrecord1
       integer nstat,nphase,nhead,nrecord
c event file name
       character*10     key
c select key
       integer          status,new_month,fstart,event_no

c
c  for asso
c
      integer max_event                   ! maximum number of events
      parameter(max_event=500000)
      real lat(Max_event),lon(max_event)  ! latitude and longitude
      real depth(max_event)               ! depth
      double precision time(max_event)    ! abs origin time
      real mag(max_event)                 ! magnitude
      character*1 mag_type(max_event)     ! magnitude type
      character*3 mag_agency(max_event)   ! magnitude agency
      integer select(max_event)           ! 1: event slected, 0: not selected
      character*80 sfile(max_event)       ! store s-file names
      integer n_de_sel                    ! number of deselected events
      real azi,baz                        ! azimuths, not used
      real dist1                          ! max distance
      real dist                           ! current distance
      real time1                          ! max time interval
      real mag1                           ! magnitude upper limit for aftershock
      real mag_input(2,100)               ! mag fixed input values
      real mag_range(2,100)               ! mag upper limit as a function of mag
      real time_range(2,100)              ! time window as a function of mag
      real dist_range(2,100)              ! distance range as a function of mag
      integer nrange_after                ! number of values in range, aftershocks
      integer nrange_before               ! number of values in range, forschocs
      logical depth_flag                  ! if true, use hypocentral distance
      real depth_diff                     ! max difference in depth
      integer year,month,day,hour,min,doy
      real sec


      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer mday,mday1,idist,idist1     ! help variables
      integer i,k,m,l                     ! counters

      n_de_sel=0
      depth_flag=.false.
      debug=1
c
c   open output file
c
      open(2,file='asso.out',status='unknown')
c
c   read parameter file
c
      call mag_range_par_read_asso
     *(mag_input,mag_range,time_range,dist_range,nrange_after,
     *nrange_before,depth_flag,
     *depth_diff,debug)
c
c  optionally open debug file
c
      if(debug.eq.2) then
        open(7,file='asso_debug.out', status='unknown')
      endif
c
c  write out pars
c
       write(6,*)
       write(6,'(a)')' Parameters'
       if(depth_flag) then
          write(6,'(a)') ' Use hypocentral distance'
       else
          write(6,'(a)')' Use epicentral distance'
       endif
       write(6,'(a,f7.1)')' Max depth difference ',depth_diff
       write(6,*)
       write(6,'(a)')' Input Mag  Mag diff Dist diff(km) Time diff(sec)'
       do i=1,nrange_after
         write(6,'(2f10.1,f14.1,f15.1)') 
     *   mag_input(1,i),mag_range(1,i),dist_range(1,i),time_range(1,i)
       enddo  

c
c   input base name and time interval
c
      write(6,*)' Base name, return for default'
      read(5,'(a40)') base_name
      write(6,*)' Start time, blank is 0000'
      read(5,'(a14)') start_time
      if(start_time.eq.' ') start_time='0000'
      write(6,*)' End time, blank is 3000'
      read(5,'(a14)') end_time
      if(end_time.eq.' ') end_time='3000'

      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter
c
c always use next event
c
       key='          '


c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event from file unit 1
c
c
c   read  event 
c
       CALL findevin
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile,fstart,new_month,status)

c      if(first.eq.1) then
c  check if first time around
c         new_month=0
c lower new month flag
c         first=0
c  lower first flag
c      endif

c  return if eof or other erorrs

       if(status.ne.0) go to 1000 

      open(1,file=evfile,status='old',err=1000)

      call rea_event_in(1,all,data,code)
      close(1)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1               ! count events
      if(nevent.gt.max_event) then
          write(6,*)'Too many events, max is ', max_event
          stop
      endif
c
c   save s-file name
c
      sfile(nevent)=evfile

c
c   save epicenter, origin time and magnitude, use prime solution
c   for magnitude, use the firs tin array hyp_mag_all wheich optionallly
c   can be sorted by parameters in seisan.def
c
      lat(nevent)=hyp_lat(1)
      lon(nevent)=hyp_lon(1)
      depth(nevent)=hyp_depth(1)
c
c   check if depth is undefined, then replace with a standard value
c
      if(depth(nevent).lt.-100) depth(nevent)=20.0

      mag(nevent)=hyp_mag_all(1)
      mag_type(nevent)=hyp_mag_type_all(1)
      mag_agency(nevent)=hyp_mag_agency_all(1)
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),time(nevent))
c
c   event selected by default
c
      select(nevent)=1
c
c   get next event
c
      goto 50
c
c     end of file
c
 1000 continue

      write(6,*)
      write(6,'(a)') 'Finished reading input file, start association'
      write(6,*)
c
c   now start association
c

      k=0
      m=0
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  main event loop for checking for association
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do i=1,nevent

c
c   read event, others might have to be merged with this one
c
         open(1,file=sfile(i),status='old')

         call indata
     *   (1,nstat,nphase,nhead,nrecord,
     *   type,exp,data,id)

         if(nrecord.eq.0) then
            stop
         endif
         close (1)
c
c  check if event already has been merged
c
         if(select(i).eq.0) goto 200
c
c   check that event has location and magnitude, else skip for checking
c   but write it out
c
         if(lat(i).lt.-900.0.or.lon(i).lt.-900.or.
     *   mag(i).lt.-900.0) goto 150
c
         if(debug.gt.0) then
             call sectim(time(i),
     *       year,doy,month,day,hour,min,sec)
             write(debug+5,'
     *       (a,i4,1x,2i2,1x,2i2,f5.1,1x,f5.1,f6.1,f6.1,f4.1,1x,a1,a3)')
     *       'Main : ', year,month,day,hour,min,sec,lat(i),lon(i),
     *       depth(i),mag(i),mag_type(i),mag_agency(i)
         endif

c
c
c-----------------------------------
c  Check for associated following events 
c-----------------------------------
c
c   calculate magnitude dependent parameters using current event index i
c   as "main shock"
c
         call mag_range_par
     *   (1,mag(i),mag_input,mag_range,time_range,
     *   dist_range,nrange_after,
     *   nrange_before,mag1,time1,dist1)
c
c   go foreward in time, time1 and check events with distance less then dist1
c   and magnitude less than mag1
c
 60      continue    ! arrive if checking for next event relative to
                     ! first main event
         k=k+1
c
c  check that not after end of events
c
         if((k+i).gt.nevent) goto 90
c
c   check that event has location and magnitude, else skip
c
         if(lat(i+k).lt.-900.0.or.lon(i+k).lt.-900.or.
     *   mag(i+k).lt.-900.0) goto 150
c
c   calculate distance to this event from reference event
c
         call distaz(lat(i),lon(i),lat(i+k),lon(i+k),dist,azi,baz)
         dist=dist*111.2   ! deg to km
c
c   convert to "hypocentral" distance if required
c
         if(depth_flag) then
            dist=
     *      sqrt(dist*dist+(depth(i+k)-depth(i))*(depth(i+k)-depth(i)))
         endif

c
c   check time, distance, depth and magnitude
c
         if(time(i+k).lt.time(i)+time1) then   ! within time window
            if(dist.lt.dist1) then             ! within distance window
                if(abs(depth(i)-depth(i+k)).lt.depth_diff) then ! depth wind
c                  if(mag(i+k).ge.mag1) then       ! within mag window
                  if(abs(mag(i+k)- mag(i)).lt.mag1) then       ! within mag window


                     if(select(i+k).ne.0) then    ! do not associate twice
                        select(i+k)=0             ! event associated 
                        n_de_sel=n_de_sel+1
                        if(debug.gt.0) then
                           call sectim(time(i+k),
     *                     year,doy,month,day,hour,min,sec)
                           mday=time1
                           idist=dist1
                           idist1=dist
                           mday1=(time(i+k)-time(i))
                           write(debug+5,'(a,i4,1x,2i2,1x,2i2,f5.1,1x,
     *                     f5.1,f6.1,f6.1,f4.1,1x,a1,a3,1x,
     *                     a,f3.1,a,2i4,a,2i4)')'Asso : ',
     *                     year,month,day,hour,min,sec,lat(i+k),lon(i+k)
     *                     ,depth(i+k),mag(i+k),mag_type(i+k),
     *                     mag_agency(i+k),' M',mag1,' T',
     *                     mday,mday1,' D',idist,idist1 
                        endif

c
c   merge event, first read it
c

                       open(1,file=sfile(i+k),status='old')

                       call indata
     *                 (1,nstat1,nphase1,nhead1,nrecord1,
     *                 type,exp,data1,id)
                       if(nrecord1.eq.0) then
                          stop
                       endif
                       close (1)


c
c   merge events, make sure largest first
c
                       status=-1                  ! allow first event after second
                       if(mag(i).gt.mag(i+k)) then
                          call merge_f(data,data1,nhead,nhead1,
     *                    nrecord,nrecord1,status)
                       else                       
                          call merge_f(data1,data,nhead1,nhead,
     *                    nrecord1,nrecord,status)
c
c   since data1 is main storage, must be copied back
c
                          do l=1,nrecord1
                             data(l)=data1(l)
                          enddo
                          nrecord=nrecord1
                          nhead1=nhead1
                       endif

                       if(status.eq.1) then
                          write(6,*)' Two events more than 24 h apart,',
     *                     ' no merge'
                          if(debug.eq.2)                       
     *                    write(7,*)' Two events more than 24 h apart,',
     *                     ' no merge'
c                           stop
                       endif

                       if(status.eq.3) then
                          write(6,*)' First event after second event',
     *                     ' and on different days,'
                        write(6,*)' phases from second event not merged'
                        if(debug.eq.2)
     *                  write(7,*)' First event after second event',
     *                  ' and on different days,'
                        write(7,*)' phases from second event not merged'
c                           stop
                       endif
c
c   save content if more merge
c
c                       nhead=nhead1
c                       nrecord=nrecord1
                     endif   ! check if not alread associated
                  endif      ! mag check                 
               endif         ! depth check
            endif            ! dist check
            goto 60          ! try next event since not end of time window 
          endif              ! test of time range

 90       continue
 
c
c  if here, end of time window for one event  
c  use next event as main event if not alread associated and merged
c       
c
c  write out associated events or event not associted or with not  lat,lon, mag
c
 150     continue   ! here to write out event which has not been checked

         write(2,'(a)')(data(l),l=1,nrecord) 
         k=0   ! start a new dependent search for next event
c
c   go to here from event loop above if event already is merged 
c
 200     continue  
         k=0
      enddo        ! end of event loop




      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file          ', nevent
      write(6,*) 'Number of events asssociated and merged ', n_de_sel
      write(6,*) 'File with merged events: asso.out'
      if(debug.eq.2) write(6,*)'Debug file: asso_debug.out'

      stop
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mag_range_par
     *(after_before,mag_event,mag_input,mag_range,time_range,
     *dist_range,nrange_after,nrange_before,mag,time,dist) 
c
c  given the table of mag dependent parameters, current mag mag-event,
c  give mag,time and dist corresponding to mag-event 
 
      implicit none
      real mag_event                    ! input magnitude
      real time                         ! output time window
      real mag                          ! ouput mag limit
      real dist                         ! output distance
      integer after_before              ! 1: aftershocks, 2: foreschocsk
      real mag_input(2,*)               ! input mag function
      real mag_range(2,*)               ! mag upper limit as a function of mag
      real time_range(2,*)              ! time window as a function of mag
      real dist_range(2,*)              ! distance range as a function of mag
      integer nrange_after              ! number of values in range of aftershocks
      integer nrange_before             ! number of values in range of forshocks
      integer i,k,m

      if(after_before.eq.1) then
         k=nrange_after
      else
         k=nrange_before
      endif
     
      do i=1,k-1
        if(mag_event.ge.mag_input(after_before,i).and.
     *     mag_event.lt.mag_input(after_before,i+1)) then
c           mag=mag_range(after_before,i)
c           time=time_range(after_before,i)
c           dist=dist_range(after_before,i)
           m=i   ! save index
           goto 10
        endif
      enddo

 10   continue

c
c   if before start, do not use as main shock
      if(mag_event.lt.mag_input(after_before,1)) then
         mag=-10.0
         time=0.0
         dist=0.0  
         goto 20
      endif    
c
c   if end hit, use last value, do no interpolate
c
      if(mag_event.ge.mag_input(after_before,k)) then
         mag=mag_range(after_before,k)
         time=time_range(after_before,k)
         dist=dist_range(after_before,k)  
         goto 20
      endif    
c
c   do interpolation
c
      call lin_interp(mag_event,mag_input(after_before,m),
     *mag_input(after_before,m+1),mag_range(after_before,m),
     *mag_range(after_before,m+1),mag)

      call lin_interp(mag_event,mag_input(after_before,m),
     *mag_input(after_before,m+1),time_range(after_before,m),
     *time_range(after_before,m+1),time)

      call lin_interp(mag_event,mag_input(after_before,m),
     *mag_input(after_before,m+1),dist_range(after_before,m),
     *dist_range(after_before,m+1),dist)

 20   continue
      

      return
      end


      subroutine mag_range_par_read_asso
     *(mag_input,mag_range,time_range,dist_range,nrange_after,
     *nrange_before,depth_flag,
     *depth_diff,debug) 
c
c  read the table of mag dependent parameters from file asso.def
c  also used for asso.def although not all parameters used
c
      implicit none    
      include 'libsei.inc'  
      character*80 text                 ! general text
      logical depth_flag                ! if true use hypcentral distance
      real x                            ! help variable

      real mag_input(2,*)               ! input mag valuers of which others is a function
      real mag_range(2,*)               ! mag upper limit as a function of mag
      real time_range(2,*)              ! time window as a function of mag
      real dist_range(2,*)              ! distance range as a function of mag
      real depth_diff                   ! max difference in depth
      integer debug                     ! 1:debug out, else not
      integer code                      ! error code
      integer nrange_after              ! number of values in range, aftershocks
      integer nrange_before             ! ...........................foreschocks
      integer i,k,un
c
c     open file
c
c                                                                               
c   open and read default file               
c   ---------------------------
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   un,               ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'asso.def' )      ! For this file.
c                                                                               
c   read file if there...
c   ---------------------
c                                                                               
      if(code.ne.e_ok$)  then
         write(6,*) 'Failed to open asso.def'
         stop
      endif

      i=0
      depth_diff=1000.0    ! use all events
      debug=0

 50   continue
      read(un,'(a)',end=10) text
c      if(text(1:20).eq.'MAGS AFTER DIST TIME'.and.text(41:50).ne.' ')
      if(text(1:20).eq.'MAGS MDIF DIST TIME '.and.text(41:50).ne.' ')
     *then
         i=i+1
         read(text(41:80),'(4f10.2)',err=100)
     *   mag_input(1,i),mag_range(1,i),dist_range(1,i),time_range(1,i)
      endif

c      if(text(1:21).eq.'MAGS BEFORE DIST TIME'.and.text(41:50).ne.' ')
c     *then
c         k=k+1
c         read(text(41:80),'(4f10.2)',err=100)
c     *   mag_input(2,k),mag_range(2,k),dist_range(2,k),time_range(2,k)
c      endif

      if(text(1:16).eq.'HYPOCENTRAL DIST'.and.text(41:50).ne.' ') then
         read(text(41:80),'(f10.2)',err=100) x
         if(x.eq.1.0) depth_flag=.true.
      endif

      if(text(1:14).eq.'MAX DEPTH DIFF'.and.text(41:50).ne.' ') then
         read(text(41:80),'(f10.2)',err=100) depth_diff
      endif

      if(text(1:9).eq.'DEBUG OUT'.and.text(41:50).ne.' ') then
         read(text(41:80),'(f10.2)',err=100) x
         debug=x
      endif

      goto 50
c
 10   continue
      call sei close( close$, un, code ) ! Close (Default stop on error).

      nrange_after=i
      nrange_before=k
      
      goto 200
 100  continue
      write(6,'(a)')'Error in parameter file in following line'
      write(6,'(a)')text
      stop

 200  continue      
      return
      end
    

