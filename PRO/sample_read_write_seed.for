c
c  program to test seed/miniseed reading routines and miniseed writing 
c  routines made by ruben luis.  
c  the routines are based  on the chad trabant libmseed library.
c  the routines are used in seisan to read seed/miniseed and write
c  miniseed from fortran.
c
c  j havskov, june 2011
c
c  this test program does not require any seisan include files or
c  subroutines
c
c  changes
c
c  mar 18 2015 jh: also read real, test output also real

      implicit none
      character*80 in_file            ! input file
      character*80 out_file           ! output file
      character*5 stat(10000)	      ! stations
      character*3 comp(10000)         ! components
      character*2 net(10000)          ! network code
      character*2 loc(10000)          ! location code
      integer year(10000)            
      integer month(10000)
      integer day(10000)
      integer hour(10000)
      integer min(10000)
      real sec(10000)
      real samprate(10000)            ! sample rate
      integer nsamp(10000)            ! number of samples in each channel
      character*80 text(10000)        ! general text string
      character*80 data_quality(10000)! timing err, gaps
      integer filestart(10000)        ! adress of start of channel
      integer fileend(10000)          ! ---------- end ----------
      integer i,m,k
      real samples(1000000)           ! the data
      real samples_int(1000000)       ! the data
      real tinterval                  ! time interval to select, all channels
      integer nsamples                ! number of samples returned, data read
      integer nchan                   ! number of channels in req. time int. 
      integer nchan_org               ! number of channels in file
      integer maxchan                 ! array size of channels
      integer maxsamples              ! array size of samples array
      integer appendFlag, compFlag, recSize ! for write out, see below
c
c   set dimensions
c
      maxsamples=1000000
      maxchan=10000
c
c  get file names
c
      write(6,*) ' Enter input file name'
      read(5,'(a)') in_file
      write(6,*) ' Enter output file name, enter for default'
      read(5,'(a)') out_file
      if(out_file.eq.' ') out_file(1:26)='sample_read_write_seed.out'

c
c  read contents of whole file to find channels available and 
c  content of each channel. The whole file is checked 
c  by setting year(1)=0. if not zero, a time interval is needed
c
      year(1)=0
c
      call seed_contents(in_file,maxchan,nchan,
     *stat,comp,net,loc,year,
     *month,day,hour,min,sec,samprate,nsamp,data_quality,
     *filestart,fileend)
c
c  display summary of file content
c
      write(6,'(a,a)')
     *' STAT  COM NT LO YEAR MO DA HR MN    SEC   NSAMP  RATE QUA',
     *'   START-A    STOP-A' 
      do i=1, nchan
         write(6,'(1x,a5,1x,a3,1x,a2,1x,a2,1x,i4,4i3,f7.3,i8,f6.1,
     *   1x,a3,2i10)')
     *   stat(i),comp(i),net(i),loc(i),year(i),month(i),day(i),hour(i),
     *   min(i),sec(i), nsamp(i),samprate(i),data_quality(i)(1:3),
     *   filestart(i),fileend(i)
      enddo
c
c   save total number of channels in file for later comparison with
c   number of channels available in a particular time interval
c
      nchan_org=nchan

c
c  make a summary of the file starting at a user given start and end time
c  if desired
c
      write(6,*)
     *' Enter start time to make a summary yyyy,mm,dd,hh,mm,ss.s'
      write(6,*)' enter to continue without selecting a time interval'
      read(5,'(a)') text(1)

      tinterval=0.0          ! default selection of whole file
      if(text(1).ne.' ') then
         read(text(1),*) year(1),month(1),day(1),hour(1),min(1),sec(1)
         write(6,*) ' Enter end time'
         read(5,*) year(2),month(2),day(2),hour(2),min(2),sec(2)
         tinterval=10.0
      else
         goto 1    ! no interval selection
      endif
c
c   find content for a time interval
c
      write(6,*)
      write(6,*) ' Content for the selected time interval'
      call seed_contents(in_file,maxchan,nchan,
     *stat,comp,net,loc,year,
     *month,day,hour,min,sec,samprate,nsamp,data_quality,
     *filestart,fileend)
c
c  check if any channels, number of channels must be positive
c
      if(nchan.eq.0) then
        write(6,*) ' No data in requested time interval'
        stop
      endif
c
c  check if some  some channels
c
      if(nchan.gt.0.and.nchan.lt.nchan_org) then
        write(6,'(a,i4,a,i4)') 
     *  ' Data available for ',nchan, ' channels out of',
     *  nchan_org
      endif
c
c  display summary of file content in the  requested  time interval
c  if part of requested time interval is  not available, the interval 
c  present will be shown.
c
      write(6,'(a,a)')
     *' STAT  COM NT LO YEAR MO DA HR MN    SEC   NSAMP  RATE QUA',
     *'   START-A    STOP-A' 
      do i=1, nchan
         write(6,'(1x,a5,1x,a3,1x,a2,1x,a2,1x,i4,4i3,f7.3,i8,f6.1,
     *   1x,a3,2i10)')
     *   stat(i),comp(i),net(i),loc(i),year(i),month(i),day(i),hour(i),
     *   min(i),sec(i), nsamp(i),samprate(i),data_quality(i)(1:3),
     *   filestart(i),fileend(i)
      enddo

 1    continue

c
c  read data for each channel for either whole file or
c  for a time interval of 10 secs with start time given above
c       
      write(6,*)
      if(tinterval.eq.0.0) then
         write(6,*) 'Read whole file'
      else
         write(6,*) 'Read 10 secs of file at start time given above'
      endif

      write(6,*) 
      write(6,*)' Returned values from data reading routine'

      write(6,'(a,a)')
     *' STAT  COM NT LO YEAR MO DA HR MN    SEC   NSAMP  RATE QUA',
     *'   START-A    STOP-A' 

      do i=1,nchan
         call seed_channel_read(in_file,
     *   stat(i),comp(i),net(i),loc(i),
     *   year(i),month(i),day(i),
     *   hour(i),min(i),sec(i),
     *   tinterval,
     *   maxsamples,nsamples, samples, data_quality(i),
     *   filestart(i),fileend(i))  
c
c   write the returned values
c
         write(6,'(1x,a5,1x,a3,1x,a2,1x,a2,1x,i4,4i3,f7.3,i8,f6.1,
     *   1x,a3,2i10)')
     *   stat(i),comp(i),net(i),loc(i),year(i),month(i),day(i),hour(i),
     *   min(i),sec(i), nsamp(i),samprate(i),data_quality(i)(1:3),
     *   filestart(i),fileend(i)
c
c   write the 5 first samples  in text array to be printed out later
c
          write(text(i),'(1x,i5,1x,5e14.6)') i,(samples(m),m=1,5)

c   
c   write in output file
c
c
c  Append flag        0 - Replace    1 - Append
c
c
           if(i.eq.1) then
              appendFlag = 0     ! if first write, start a new file
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
c
c   write one channel, now it must be integers
c
           do k=1,nsamp(i)
              samples_int(k)=samples(k)
           enddo

           call seed_channel_write(out_file,
     *     stat(i), comp(i), net(i), loc(i),
     *     year(i),month(i),day(i),hour(i),min(i),sec(i),samprate(i),
     *     nsamp(i),samples_int,
     *     appendFlag,compFlag,recSize,data_quality)
      enddo
c
c   write samples on screen
c
      write(6,*) ' '
      write(6,*)' First 5 samples from each channel'
      write(6,*)
      do i=1, nchan
         write(6,'(a)') text(i)
      enddo
c
      write(6,*)
      write(6,'(a,a)') ' Output file name is ',out_file
          
      stop
      end
