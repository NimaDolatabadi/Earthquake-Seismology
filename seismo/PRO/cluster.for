c--------------------------------------------------------------------------
c
c   program to decluster a catalog
c
c
c   changes
c
c may 10 2013 jh: overflowe in output
c
c   parameters are given in cluster.def
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
      character*80 err_text               ! error text
      character*80 text                   ! general text
      character*80 infile                 ! input file
      integer id                          ! for s-file
      character*1 type,exp                ! for s-file
      integer debug                       ! 1: debug output, o: no
      logical compact                    ! true if a compact file
c
c  for cluster
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
      integer i,k,m                       ! counters

      n_de_sel=0
      depth_flag=.false.
      debug=1
c
c   open output file
c
      open(2,file='cluster_use.out',status='unknown')
      open(3,file='cluster_reject.out',status='unknown')   
c
c   read parameter file
c
      call mag_range_par_read
     *(mag_input,mag_range,time_range,dist_range,nrange_after,
     *nrange_before,depth_flag,
     *depth_diff,debug)
c
c  optionally open debug file
c
      if(debug.eq.2) then
        open(7,file='cluster_debug.out', status='unknown')
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
       write(6,'(a)') ' Aftershocks'
       write(6,'(a)')' Input Mag Mag limit      Dist      Time'
       do i=1,nrange_after
         write(6,'(4f10.1)') 
     *   mag_input(1,i),mag_range(1,i),dist_range(1,i),time_range(1,i)
         time_range(1,i)=time_range(1,i)*24*3600.0  ! convert to seconds
       enddo  
       if(nrange_before.gt.0) then
          write(6,*)
          write(6,'(a)') ' Foreschocs'
          write(6,'(a)')' Input Mag Mag limit      Dist      Time'
          do i=1,nrange_before
             write(6,'(4f10.1)') 
     *       mag_input(2,i),mag_range(2,i),dist_range(2,i),
     *       time_range(2,i)
             time_range(2,i)=time_range(2,i)*24*3600.0  ! convert to seconds
          enddo 
       endif 
c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input file'
      read(5,'(a)') infile
      if(infile.eq.' ') stop
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter
c
c   check if compact
c
      call nortype(1,compact)
      if(compact) then
         write(6,*) 'File is compact, cannot be used'
         write(6,*) 'Convert to CAT file with e.g. SPLIT and COLLECT'
         stop
      endif

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
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
c   save epicenter, origin time and magnitude, use prime solution
c   for magnitude, use the firs tin array hyp_mag_all wheich optionallly
c   can be sorted by parameters in seisan.def
c
      lat(nevent)=hyp_lat(1)
      lon(nevent)=hyp_lon(1)
      depth(nevent)=hyp_depth(1)
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
      write(6,'(a)') 'Finished reading input file, start decluster'
      write(6,*)
c
c   now start declustering
c

      k=0
      m=0
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  main event loop for checking for dependency
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do i=1,nevent
c
c  check if event already has been deselected
c
         if(select(i).eq.0) goto 200
c
c  if no location or magnitude, do not check
c
         if(lat(i).lt.-900.0.or.lon(i).lt.-900.0.or.
     *   mag(i).lt.-900.0) goto 200
c
         if(debug.gt.0) then
             call sectim(time(i),
     *       year,doy,month,day,hour,min,sec)
             write(text,'
     *       (a,i4,1x,2i2,1x,2i2,f5.1,1x,f5.1,f6.1,f6.1,f4.1,1x,a1,a3)')
     *       'Main : ', year,month,day,hour,min,sec,lat(i),lon(i),
     *       depth(i),mag(i),mag_type(i),mag_agency(i)
c
c   check for no values of lat,lon or mag
c

             if(lat(i).lt.-900) text(28:46)=' '
             if(mag(i).lt.-900) text(45:48)=' '
             write(5+debug,'(a)') text(1:79)
         endif
c
c-----------------------------------
c   Aftershocks
c-----------------------------------
c
c   calculate magnitude dependent parameters using current event index i
c   as "main shock", first for aftershocks
c
      call mag_range_par
     *(1,mag(i),mag_input,mag_range,time_range,dist_range,nrange_after,
     * nrange_before,mag1,time1,dist1)
c
c   go foreward in time, time1 and check events with distance less then dist1
c   and magnitude less than mag1
c
 60      continue    ! arrive if checking for next event relative to
                     ! first main event
         k=k+1
c
c  check that not after end of events, then go to check for aftershocks
c
         if((k+i).gt.nevent) goto 70
c
c   calculate distance to this event
c
         call distaz(lat(i),lon(i),lat(i+k),lon(i+k),dist,azi,baz)
         dist=dist*111.2   ! deg to km
c
c   convert to hypocentral distance if required
c
         if(depth_flag) then
            dist=sqrt(dist*dist+
     *      (depth(i)-depth(i+k))*(depth(i)-depth(i+k)))
         endif

c
c   check time, distance, depth and magnitude
c
         if(time(i+k).lt.time(i)+time1) then   ! within time window
            if(dist.lt.dist1) then             ! within distance window
                if(abs(depth(i)-depth(i+k)).lt.depth_diff) then ! depth wind
                  if(mag(i+k).le.mag1) then       ! within mag window
                     if(debug.gt.0.and.select(i+k).ne.0) then  ! do not write out twice
                        call sectim(time(i+k),
     *                  year,doy,month,day,hour,min,sec)
                        mday=time1/86400
                        idist=dist1
                        idist1=dist
                        mday1=(time(i+k)-time(i))/86400
                        write(debug+5,'(a,i4,1x,2i2,1x,2i2,f5.1,1x,
     *                  f5.1,f6.1,f6.1,f4.1,1x,a1,a3,1x,
     *                  a,f3.1,a,2i3,a,2i5)')
     *                  'After: ',
     *                  year,month,day,hour,min,sec,lat(i+k),lon(i+k),
     *                  depth(i+k),mag(i+k),mag_type(i+k),
     *                  mag_agency(i+k),' M',mag1,' T',
     *                  mday,mday1,' D',idist,idist1 
     *                  
                     endif

                     if(select(i+k).ne.0) then    ! do not deselect twice
                        select(i+k)=0                ! event deselected
                        n_de_sel=n_de_sel+1
                     endif
                  endif                       
               endif       
            endif
            goto 60           ! try next event since not end of time window 
          endif
c
c   if here, end of time window, check forschocks
c
 
c
c-----------------------------------
c   Foreshocks
c-----------------------------------
c
 70   continue  ! jump here if aftershock check at end of events
      
      if(nrange_before.eq.0) go to 90  ! no check for foreschocks
c
c   calculate magnitude dependent parameters using current event index i
c   as "main shock"
c
      call mag_range_par
     *(2,mag(i),mag_input,mag_range,time_range,dist_range,nrange_after,
     * nrange_before,mag1,time1,dist1)
c
c   go backwards in time, time1 and check events with distance less then dist1
c   and magnitude less than mag1
c
 80      continue    ! arrive here if checking for next event relative to
                     ! first main event

         m=m+1       ! back one event
c
c  check that not before start of events
c
         if((i-m).lt.1) goto 90
c
c   calculate distance to this event
c
         call distaz(lat(i),lon(i),lat(i-m),lon(i-m),dist,azi,baz)
         dist=dist*111.2   ! deg to km
c
c   convert to hypocentral distance if required
c
         if(depth_flag) then
            dist=sqrt(dist*dist+depth(i+k)*depth(i+k))
         endif

c
c   check time, distance, depth and magnitude
c
         if(time(i-m).ge.time(i)-time1) then   ! within time window
            if(dist.lt.dist1) then             ! within distance window
                if(abs(depth(i)-depth(i-m)).lt.depth_diff) then ! depth wind
                  if(mag(i-m).le.mag1) then       ! within mag window
                     if(debug.gt.0.and.select(i-m).ne.0) then ! do not write twice
                        call sectim(time(i-m),
     *                  year,doy,month,day,hour,min,sec)
                        mday=time1/86400
                        idist=dist1
                        idist1=dist
                        mday1=(time(i)-time(i-m))/86400
                        write(debug+5,'(a,i4,1x,2i2,1x,2i2,f5.1,1x,
     *                  f5.1,f6.1,f6.1,f4.1,1x,a1,a3,1x,
     *                  a,f3.1,a,2i3,a,2i5)')
     *                  'Fore : ',
     *                  year,month,day,hour,min,sec,lat(i-m),lon(i-m),
     *                  depth(i-m),mag(i-m),mag_type(i-m),
     *                  mag_agency(i-m),' M',mag1,' T',
     *                  mday,mday1,' D',idist,idist1
     *                  
                     endif
                     if(select(i-m).ne.0) then     ! do not deselect twice
                        select(i-m)=0              ! event deselected
                        n_de_sel=n_de_sel+1
                     endif
                  endif                       
               endif       
            endif
            goto 80           ! try next event since not end of time window 
          endif

 90       continue  ! jump here if before first event or no aftershcock
                    ! checking

c
c  if here, end of time window for both after and foreschcks, 
c  use next event as main event
c       
          k=0   ! start a new dependent search for next event
          m=0
c
c   go to here from event loop above if event already is deselected 
c
 200     continue  
      enddo        ! end of event loop


c
c   write out results, file must be read again
c
      rewind(1)

      do i=1,nevent
c
c   read all parameters for one event from file unit 1
c
         call indata
     *   (1,rea_nstat,rea_nphase,rea_nhead,rea_nrecord,
     *   type,exp,data,id)
         if(rea_nrecord.eq.0) then
            write(6,*)' end of file'
            stop
         endif
c
         if(select(i).eq.1) then
             write(2,'(a)')(data(k),k=1,rea_nrecord)
         else
             write(3,'(a)')(data(k),k=1,rea_nrecord)
         endif
      enddo

      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Number of events rejected', n_de_sel
      write(6,*) 'File with included events: cluster_use.out'
      write(6,*) 'File with rejected events: cluster_reject.out'
      if(debug.eq.2) write(6,*)'Debug file: cluster_debug.out'

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


      subroutine mag_range_par_read
     *(mag_input,mag_range,time_range,dist_range,nrange_after,
     *nrange_before,depth_flag,
     *depth_diff,debug) 
c
c  read the table of mag dependent parameters from file cluster.def
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
      integer nrange_after              ! number of values in range, aftershocks
      integer nrange_before             ! ...........................foreschocks
      integer code                      ! error code
      integer i,k,un
c
c     open file
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   un,               ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'cluster.def' )   ! For this file.
c                                                                               
c   read file if there...
c   ---------------------
c                                                                               
      if(code.ne.e_ok$)  then
         write(6,*) 'Failed to open cluster.def'
         stop
      endif

      i=0
      depth_diff=1000.0    ! use all events
      debug=0

 50   continue
      read(un,'(a)',end=10) text
      if(text(1:20).eq.'MAGS AFTER DIST TIME'.and.text(41:50).ne.' ')
     *then
         i=i+1
         read(text(41:80),'(4f10.2)',err=100)
     *   mag_input(1,i),mag_range(1,i),dist_range(1,i),time_range(1,i)
      endif

      if(text(1:21).eq.'MAGS BEFORE DIST TIME'.and.text(41:50).ne.' ')
     *then
         k=k+1
         read(text(41:80),'(4f10.2)',err=100)
     *   mag_input(2,k),mag_range(2,k),dist_range(2,k),time_range(2,k)
      endif

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
    


