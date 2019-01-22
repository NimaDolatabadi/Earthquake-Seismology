c--------------------------------------------------------------------------
c  Program for reading one s-file at a time in a loop, 
c  extract the corresponding  waveform file from an archive
c  and register the waveform file name in the S-file.
c
c  the program can get the input file name and operator as argument
c  one and two. this option is intened for eev, works only with
c  one event and will overwrite the input file. the normal get_arc.out
c  will also be written.
c
c  j. havskov, october 2015
c
c  changes:
c
c  04 10 2016 jh: forgotten to check for s
c  
c--------------------------------------------------------------------------c
c
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions 
      include 'seisan.inc'                ! seisan variables
      include 'rea.inc'                   ! parameter common block
      include 'libsei.inc'                ! seisan variables
      include 'version.inc'

      character*80 data(max_data)         ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file
      character*80 extract_file           ! file with station codes to extract
      character*80 text,text1             ! general text
      character*200 long_text

      integer nargs                       ! number of arguments
      character*80 args(5)                ! arguments

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      real distance                       ! distance form station to epicenter
      real pre_time,post_time             ! time before and after origin time
      double precision abs_origin         ! abs origin time
      integer year,month,day,hour,min
      real sec
      integer i,k,m                       ! counters
      character*1 answer                  ! choise of channels to extract
      character*5 stat(max_trace)         ! station codes to extract
      integer chan(max_trace)             ! channel numbers in archive selected
      real slat(5000),slon(5000),elev(5000) ! lat,lon and elevation of stations
      real dekm, dedeg, az0               ! dist in km, deg and azimuth
      real epilat,epilon                  ! epicenter
      real plat,plon                      ! location of a point
      integer nchan                       ! number of channels to extract
      integer nstat                       ! number of stations to extract
      integer seiclen                     ! function
      character*12 p_time                 ! system time
      character*14 proc_time              ! system timer
      character*4 operator                ! operator
      integer nskip                       ! number of events with no extraction

      call print_ver                      ! give version number
      
      call get_arguments(nargs,args)

      nskip=0
c
c   get defaults
c
      call get_seisan_def

c
c   skip two first questions if by arguments
c
      if(nargs.eq.2) then
         operator=args(2)(1:4)
         infile=args(1)
         goto 9 
      endif

 5    continue
      write(6,*) 'Give operator'
      read(5,'(a)') operator
      if(operator.eq.' ') goto 5   
c
c   get input file name, check if exist
c
 9    continue
      if(nargs.ne.2) then
         write(6,*) 'Give input file'
         read(5,'(a)') infile
      endif
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      goto 9
 11   continue

c
c   open output file
c
      open(2,file='get_arc.out',status='unknown')
    

      write(6,*)
     *'Interval in number of seconds before and after origin time'
      write(6,*)'Default (enter) is 30 and 300'
      read(5,'(a)') text
      pre_time=30.0
      post_time=300.0
      if(text.ne.' ') read(text,*) pre_time,post_time

c
c   get info for which channels to extract
c
 112  continue
      write(6,*)'Extract stations with readings:                  enter'
      write(6,*)'       all channels in archive:                      a'
      write(6,*)'       for given stations interactively:             s'
      write(6,*)'       for stations in a file:                       f'
      write(6,*)'       for stations at given distance to epicenter:  d'
      write(6,*)'       for stations to given distance to given point:p'
      read(5,'(a)') answer
      if(answer.ne.' '.and.answer.ne.'a'.and.answer.ne.'f'.and.
     'answer.ne.'d'.and.answer.ne.'p'.and.answer.ne.'s') then
         write(6,*)'Wrong answer'
         goto 112
      endif

      if(answer.eq.'s') then
         write(6,*)'Give station codes, one per line, end with enter'
         nstat=0
 12      continue
         nstat=nstat+1
         read(5,'(a)') stat(nstat)
         if(stat(nstat).eq.' ') goto 13
         goto 12
 13      continue
         nstat=nstat-1
      endif
c
c   get info from a file
c
      if(answer.eq.'f') then
         write(6,*)'Give file name for stations to extract'
         read(5,'(a)') extract_file
         open(3,file=extract_file,status='old')
         nstat=1
  14     continue
         read(3,'(a)',end=15) stat(nstat)
         nstat=nstat+1
         if(stat(nstat).eq.' ') goto 15
         goto 14
 15      continue
         nstat=nstat-1
         close(3)
      endif
      if(nstat.gt.0) 
     *write(6,'(a,i3)')'Number of stations to extract:',nstat
      write(6,*)
c
c  distance to epicenter
c
      if(answer.eq.'d') then
         write(6,*)'Give distance(km) to epicenter'
         write(6,*)'If no epicenter, use stations in file'
         write(6,*)
     *  'If no stations in file, use all channels in archive'
         read(5,*) distance
c
c   get station coordinates from STATION0.HYP
c
     
         call stat_loc_many(arc_stat,arc_nchan,' ',slat,slon,elev,i)
         if(i.eq.1) then
            write(6,*) 'No station file'
            stop
         endif 
      endif
c
c   distance to a point
c
      if(answer.eq.'p') then
         write(6,*)'Give lat,lon of point'
         read(5,*) plat,plon       
         write(6,*)'Give distance(km) to epicenter'
         read(5,*) distance
c
c   get station coordinates from STATION0.HYP
c   
         call stat_loc_many(arc_stat,arc_nchan,' ',slat,slon,elev,i)
         if(i.eq.1) then
            write(6,*) 'No station file'
            stop
         endif  
      endif 

         
      
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events and extract starts here
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
c
c   write part of first header line for info
c
      write(6,'(a,i4,2x,a)') '********* Event ',nevent,data(1)(1:43)

c-------------------------------------------
c   prepare file listing channels to extract
c-------------------------------------------

c--------------------------------------------------------
c   case of distance to stations
c--------------------------------------------------------

      if(answer.eq.'d') then
c
c   read epicenter
c
        if(data(1)(24:38).ne.' ') then
            read(data(1)(24:38),'(f7.3,f8.3)',err=25) epilat,epilon
            goto 26
 25         continue
            epilat=0.0
            epilon=0.0
 26         continue
         else 
            write(6,*)'No Lat-Lon in S-file'
            epilat=0.0
            epilon=0.0
         endif

         if(epilat.eq.0.0.and.epilon.eq.0.0.and.rea_nstat.gt.0) then
            write(6,*)
     *     'Epicenter problem, will use stations in s-file'
            goto 30
         endif

         if(epilat.eq.0.0.and.epilon.eq.0.0.and.rea_nstat.eq.0) then
            write(6,*)
     * 'Epicenter problem, no stations in s-file, will use all channels'
         endif
c
c   find channels within radius
c
         k=0
   
         do i=1,arc_nchan
            if(epilat.eq.0.0.and.epilon.eq.0.0) then  ! use all
               k=k+1
               chan(k)=i  ! use this station
            else      
               call delaz(slat(i)/57.3, slon(i)/57.3, dekm, dedeg, az0, 
     *         epilat/57.3, epilon/57.3)
               write(6,*) dekm,distance
               if(dekm.lt.distance) then
                  k=k+1
                  chan(k)=i
               endif
            endif
         enddo
         nchan=k
         nchan=k
         if(k.eq.0) then
            write(6,*)
     *      'No channels to extract using distance option, skip event'
            nskip=nskip+1
            goto 20
         else  
            write(6,*)'Channel to extract w?th rad?ous opt?on ',nchan
         endif
      endif

c
c   find channels within radius to given point
c
      if(answer.eq.'p') then
         k=0  
         do i=1,arc_nchan
            call delaz(slat(i)/57.3, slon(i)/57.3, dekm, dedeg, az0, 
     *      plat/57.3, plon/57.3)
            if(dekm.lt.distance) then
               k=k+1
               chan(k)=i
            endif
         enddo
         nchan=k   
         if(k.eq.0) then
            write(6,*)
     *      'No channels to extract using dist-point option, skip event'
            nskip=nskip+1
            goto 20
         else  
            write(6,*)'Channel to extract with dist-point option ',nchan
         endif
      endif
 
 
c--------------------------------------------------------
c   case of all archive channels
c--------------------------------------------------------

      if(answer.eq.'a') then
         nchan=arc_nchan  
         do i=1,arc_nchan
           chan(i)=i                     
         enddo
      endif

c-------------------------------------------------------------------
c  case of stations from s-file
c-------------------------------------------------------------------
c
c   first get stations from s-file, there will be several repeats but
c   but they are later sorted out
c
      if(answer.eq.' ') then
c
c   get here from d option if no epicenter
c
 30      continue
         nstat=rea_nphase
         do i=1,rea_nphase
            stat(i)=rea_stat(i)
         enddo
         write(6,*)'Number of station observations in S-file', nstat
c
c skip if no stations
c
         if(nstat.eq.0) then
            write(6,*)'No stations, skip event'
            nskip=nskip+1
            goto 20
         endif
      endif

c---------------------------------------------------------------- 
c   case of stations from a file, s-file or interactively entered 
c----------------------------------------------------------------

      if(answer.eq.' '.or.answer.eq.'f'.or.answer.eq.'s') then
         if(nstat.eq.0) then
             write(6,*)'No stations, skip event'
             nskip=nskip+1
             goto 20
         endif
c
c   find how many different stations available so a station is not used
c   more times to select arc channels
c
         do i=1,nstat
           do k=i+1,nstat
              if(stat(i).eq.stat(k)) stat(k)=' '
           enddo
         enddo  
         k=0
         do i=1,nstat
           if(stat(i).ne.' ') then
             k=k+1
             stat(k)=stat(i)
           endif
         enddo
         nstat=k
         write(6,*)'Diffent stations for this event: ',nstat      
c
c   find channels in archive, all channels corresponding to station are selected
c
         m=0   ! count channels in archive corresonding to
               ! stations required
         do k=1,arc_nchan
            do i=1,nstat
               if(stat(i).eq.arc_stat(k)) then
                  m=m+1
                  chan(m)=k
               endif
             enddo
          enddo
          nchan=m
          if(nchan.eq.0) then
             write(6,*)'No corresponding stations in archive'
             write(6,*)'Skip event'
             nskip=nskip+1
             goto 20               ! write out of event
          endif
      endif
c
c----------------------------------------------------------
c  make list of channels in terms of stat, comp etc, make sure there
c  is not an old one
c----------------------------------------------------------
c

      open(3,file='cbase.inp',status='unknown')
      close(3,status='delete')
      open(3,file='cbase.inp',status='unknown')

      do i=1,nchan
         k=chan(i)
         write(3,'(a5,a3,a2,a2,2f15.0)') arc_stat(k),arc_comp(k),
     *   arc_net(k),arc_loc(k),arc_start(k),arc_stop(k)
      enddo
      close(3)
c
c-----------------------------------------------------------
c  start extraction process
c-----------------------------------------------------------

c
c  get abs origin time
c
      call timsec (hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),abs_origin)
c
c  calculate abs extract start time
c
      abs_origin=abs_origin-pre_time

      call sectim
     *(abs_origin,year,i,month,day,hour,min,sec)
      i=sec+0.5
      write(text,'(i4,5i2)')year,month,day,hour,min,i
      do i=1,14
         if(text(i:i).eq.' ') text(i:i)='0'
      enddo
c
c   prepare extract command for wavetool
c
      write(text1(1:10),'(f10.1)') pre_time+post_time
c     arc_text=' -arc '   ! set archive
      long_text='wavetool -start '//text(1:14)//
     *' -arc '//' -duration '//text1(1:10)
     *//' -wav_out_file SEISAN'     
     *//' -format MSEED'
     *//' -cbase cbase.inp'
            
c
c    extract waveform file
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
     *' Something wrong with wavetool, return to continue'
        write(6,*)'Check if archive and channel definition ok'
c       read(5,'(a)') i
      else
        long_text(1:198)=long_text(3:200)
        k=seiclen(long_text)
        write(6,*)'Extracted file ',long_text(1:k)
      endif

c----------------------------------------------------
c  put waveform file name into s-file
c----------------------------------------------------
c
      

c
c  check if the file name is there from before, if so skip
c
      do i=1,rea_nwav
         if(rea_wav(i)(2:k+1).eq.long_text(1:k)) then
             write(6,*)'Wav file already in S-file, skip ***********'
             goto 20
         endif
      enddo
c
      rea_nwav=rea_nwav+1
      rea_wav(rea_nwav)=' '
      rea_wav(rea_nwav)(2:k+1)=long_text(1:k)
      rea_wav(rea_nwav)(80:80)='6'
c
c   update ID line
c
      call systime(p_time,proc_time) 
      if(rea_id_line.eq.' ') then
c
c   make new id line
c
         rea_id_line(1:40)= ' ACTION:                   OP:     STATU'
         rea_id_line(41:80)='S:               ID:                   I'
         i=hyp_sec(1)
         write(rea_id_line(61:75),'(i4,5I2)')
     *   hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),hyp_min(1),i
         do i=61,74
            if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
         enddo
         write(rea_id_line(31:34),'(a)')operator
         write(rea_id_line(13:26),'(a)')proc_time
         rea_id_line(9:11)='ARX'  ! for arc extract
      else
c
c   update existing id line, first copy old line to a comment line
c
         rea_ncomment=rea_ncomment+1
         rea_comment(rea_ncomment)=rea_id_line(1:79)//'3'
c
c  update
c
         write(rea_id_line(31:34),'(a)')operator
         write(rea_id_line(13:26),'(a)')proc_time
         rea_id_line(9:11)='ARX'  ! for arc extract
      endif
               
c
 20   continue         
c
c   write out modified event
c
       call rea_event_out(2,all,data,code)
c
c   stop if arguments
c
      if(nargs.eq.2) goto 1000
c
c   get next event
c
      goto 50
c
c     end of file
c
 1000 continue

c
c   overwrite input if form eev
c
      if(nargs.eq.2) then
         rewind(1)
         call rea_event_out(1,all,data,code)   
      else
         write(6,*)            ! blank line
         close(2)              ! close output file
         write(6,*) 'Number of events in input file', nevent
         write(6,*) 'Number of events skipped      ', nskip
         write(6,*) 'Output file name is get_arc.out'
      endif
      close(1)

      stop
      end
