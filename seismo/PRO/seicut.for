c
c  simple program to extract out a section of a seisan file (any seisan
c  primary format). a similar job can be done with wavetool, 
c  however seicut can do the same cut on many files
c  
c  jh, april 2004
c
c  changes
c
c  2005 oct 21  jh: Fix so it also works with files with more than 30 channels
c  2007 apr 20  jh: Make it work with filenr.lis, and use relative time from
c                   start of file 
c  2008 may 15  jh: modify to also use seed output format, 
c                   do not stop if whole data interval not there
c  2008 jun 18  jh> third argument is output format
c  2012 jan 25  jh: new seed writing, initilize wav_mem
c
c  mode 1 with one file
c
c  syntax is: seicut filename yyyymmdd..  inteval (sec)
c  
c  If no input on prompt, user can enter values manually.
c  the first sample to use is the first sample found before the start time
c  the time interval out will be time from first to last sample, so if e.g.
c  one second of data is asked for at a sample rate of 100 Hz, the time interval
c  in header will be 0.99 sec and the number of samples output will be 100.
c
c  if interval not available in any of the channels, the program will stop.
c
c
c  mode 2
c
c  syntax is : seicut
c
c  When entereing a file name, filenr.lis can be used. The program will
c  then use a list of files. Start time is now relative to start of file, 
c  interval is same as before. So all files are cut the same amount. if
c  file is too short, there is an option to use end of file as end time.
c
c
c  
c  the output file name will use a network code reflecting station code of
c  first channel in input file and '_CUT' is added to the end of the file name

      implicit none
      include 'seidim.inc'                    ! for giving dimensions
      include 'libsei.inc'                    ! for all seisan subroutines
      include 'waveform.inc'                  ! waveform data
c      include 'seed.inc.f'                    ! seed reading
c      include 'write_mseed.inc.f'             ! seed writing
      include 'seisan.inc'                    ! seisan general parameters
      character*80 arg(5)    	   	      ! arguments to cfile
      integer nargs                           ! number of arguments
      integer doy                             ! day of year
      character*5 net_code                    ! network code
      character*80 outfile                    ! output file name
      character*80 mainhead(max_trace)        ! waveform header
      character*1040 chead                    ! waveform header
      integer syear,smonth,sday,shour,smin    ! start time
      real*8 abs_start                        ! abs time of start time
      integer last_samp(max_trace)            ! sample number for last sample
      real ssec                               ! start sec
      real interval                           ! interval to extract (sec)
      real start_tt                           ! start time relative to file start, many files
      character*80  start_t                   ! abs start time
      integer seiclen                         ! function
      integer accept                          ! if 1, accept many files too short
c--- output format
      character*6 out_format
      character*3 seed_comp              ! seed component
      integer appendFlag                 ! append flag for mseed write
      integer compFlag                   ! compression flag, 0: steim1, 1 steim2
      integer recSize                    ! record size to write mseed
c      integer mseed_block                 ! count mseed blocks whrn writing out
c      logical op_file,open_file           ! for opening mseed output  file


      real*8 del_time                           ! time difference between first sample and
                                              ! sample to use
      integer first_samp(max_trace)           ! first sample to use, first sample in trace
                                              ! is number 1
      integer i,l,k,kk,in                        ! counters

c     accept=0
      accept=1
c
c 
c   defult output format is miniseed
c
      out_format(1:5)='mseed'
c
c   get arguments, first is start time in yyyymmdd... and second is interval in seconds
c   this argument function is SEISAN general, has the same form on all computer platforms
c
      call get_arguments(nargs,arg)
C
c   check arguments
c
      
      if (nargs.lt.3) then
         write(6,*)' Not enough arguments'
         write(*,*)
     *  'filename start time (yymmdd...) and time',
     *  ' interval (sec) can be given'
         write(6,*)
         write(6,*) 'Will use interactive input'
c
c   just enter one wav name
c
         write(6,*)
     *   'Waveform file name, can be filenr.lis for many files'
         read(5,'(a)') wav_filename(1)
         in=0
         if(wav_filename(1)(1:10).eq.'filenr.lis') then
            open(8,file='filenr.lis',status='old',err=20)
            goto 21
 20         continue
            write(6,*)' No filenr.lis'
             stop
 21          continue
             in=1
          endif          

 4646    continue   ! from just below
         write(6,*)' Output format, seisan or mseed ?'
         read(5,'(a)') out_format
         if(out_format(1:6).ne.'seisan'.
     *   and.out_format(1:5).ne.'mseed') then
            write(6,*)' Wrong format, try again'
            goto 4646
         endif
c
c   one file
c 
         if(in.eq.0) then      
            write(6,*)
     *      'start time (yyyymmddhhmmss.sss)'
            read(5,'(a)') start_t
            read(start_t,'(i4,4i2,f6.0)')
     *      syear,smonth,sday,shour,smin,ssec
         endif
c
c   many files
c
         if(in.eq.1) then
            write(6,*) 'Start time (sec) relative to file start time'
            read(5,*) start_tt
         endif

         write(6,*)'Interval in seconds'
         read(5,*) interval
      else
         wav_filename(1)=arg(1)
         read(arg(2),'(i4,4i2,f6.0)')
     *   syear,smonth,sday,shour,smin,ssec
         read(arg(3),'(f9.0)') interval
c
c  take format if there
c
         if(nargs.eq.4) then
           out_format=arg(3)
         endif
      endif

      wav_nfiles=1                   ! there is only one file in this case
      wav_nchan=0

c
c  file loop if many files
c

 1000 continue

      if(in.eq.1) then
         read(8,'(7x,a)') wav_filename(1)
         if(wav_filename(1)(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') wav_filename(1)
      call wav_init
      call wav_mem_init
c
c   read all headers of waveform file 
c
      call read_wav_header(1)
c
c   output possible errors
c
      if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *wav_error_message
c
c   write out the format
c
      write(6,'(1x,a,a)') 'Format ',wav_file_format(1)
c
      write(6,*)' Total number of channels available:',wav_nchan
c
c   write some input info for each channnel on screen
c
      write(6,'(a,a)')
     *' CHA STAT  COMP  YEAR MO DA HR MI    SEC  NSAMP ',
     *                '    RATE    DELAY DURATION'
      do i=1,wav_nchan
         write(6,'(1x,i3,1x,a,1x,a,1x,i5,4i3,f7.3,1x,i6,3f9.3)')
     *   i, wav_stat(i),wav_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i)
      enddo
      write(6,*)
c
c   find first sample to use for each channel
c
      do i=1,wav_nchan
         if(in.eq.0) then
            call timsec(syear,smonth,sday,shour,smin,ssec,abs_start)
            del_time=abs_start-wav_abs_time(i)
         else
            del_time=start_tt
         endif
c        write(6,*)'del_time',del_time
c
c  check if start time ok
c
c        write(6,'(2f15.3)') 
c    *   'abs_start, wav_abs_time',abs_start,wav_abs_time(i)
         if(del_time.lt.0) then
            del_time=0.0
            write(6,*)'channel ',i
            write(6,*)'start time is before channel start time'
            write(6,*) 'will start with start of file'
c           stop
         endif
c
c  fix round off errors
c
         del_time=del_time+0.0001
         interval=interval+0.0001
c
         first_samp(i)=del_time*wav_rate(i)+1
c        write(6,*) 'del time, first samp', del_time,first_samp(i)
c
c   check if end time ok
c
         last_samp(i)=first_samp(i)+wav_rate(i)*interval-1 
c        write(6,*) 'last_samp, wav_nsamp',last_samp(i),wav_nsamp(i)
         if(last_samp(i).gt.wav_nsamp(i)) then
             write(6,*) 'Channel ',i
             write(6,*) 'End time after channel end'
             write(6,*) 'Will use end of file'
             last_samp(i)=wav_nsamp(i)
         endif
c
c   case of many files
c
c            if(accept.eq.1.and.in.eq.1) then
c            endif
c            if(in.eq.1.and.accept.eq.0) then
c              write(6,*) 
c    *        'Write available samples for this file only (1)'
c              write(6,*)
c    *        'Write available samples for all files      (2)'
c              write(6,*)
c    *        'Stop                                       (3)'
c              read(5,*) k
c              if(k.eq.1.or.k.eq.2) then
c                 last_samp(i)=wav_nsamp(i)
c              endif
c              if(k.eq.2) accept=1                  
c              if(k.eq.3) stop
c            endif
c
c
c  always stop if one file only
c
c            if(in.eq.0) then
c               stop
c            endif
c         endif
c        write(6,*) 'last samp', last_samp(i)
c
c  find exact time of first sample and put in ouput common block
c
          abs_start=wav_abs_time(i)+(first_samp(i)-1)/wav_rate(i)          
c
c  fix round off
c
          abs_start=abs_start+0.00001

          call sectim(abs_start,wav_year(i),doy,wav_month(i),wav_day(i),
     *    wav_hour(i),wav_min(i),wav_sec(i))
c          wav_nsamp(i)=wav_rate(i)*interval
          wav_nsamp(i)=last_samp(i) - first_samp(i) +1
       enddo
c
c   make and write seisan output header, also made for mseed to get name
c
      net_code='     '
      call wav_sheads(1,net_code,outfile,mainhead,chead)   ! make headers
c
c  put in M if miniseed
c
      if(out_format(1:5).eq.'mseed') outfile(19:19)='M'
c
      outfile=outfile(1:seiclen(outfile))//'_CUT'
c      write(6,*) outfile(1:seiclen(outfile))//'_CUT'
      write(6,*) ' Output filename: ',
     &   outfile
c
c   write output header information
c

      write(6,*)' Output information'
      write(6,'(a,a)')
     *' CHA STAT  COMP  YEAR MO DA HR MI    SEC  NSAMP '
      do i=1,wav_nchan
         write(6,'(1x,i3,1x,a,1x,a,1x,i5,4i3,f7.3,1x,i6)')
     *   i, wav_stat(i),wav_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i)
      enddo
      write(6,*)

c
c   seisan format
c
      if(out_format(1:6).eq.'seisan') then
         open(2,file=outfile,
     *   status='unknown',form='unformatted')
c
         do i=1,12
            write(2) mainhead(i)
         enddo
c
c   write remaining main header lines if more than 30
c
         if(wav_nchan.gt.30) then
            kk=(wav_nchan-31)/3+1
            do i=13,kk+12
               write(2)mainhead(i)
            enddo
         endif

         write(6,*) mainhead(1)(1:79)
c 
      endif
c
c------------------------------------------------
c   read write loop   
c------------------------------------------------
c
      do i=1,wav_nchan
          call wav_read_channel(i)   ! read one trace
c
c   if reading mininseed, wav_nsamp is reset to input file value so reset
c   here to get correct output value. wav_out variables should have
c   been use, to be fixed
c
          wav_nsamp(i)=last_samp(i) - first_samp(i) +1

          if(out_format(1:6).eq.'seisan') then
             call wav_sheads(i,net_code,outfile,mainhead,chead)  
             write(2) chead          ! write channel header
             write(6,*) chead(1:79)
             write(2)(int(signal1(l)),
     *       l=first_samp(i),last_samp(i)) ! write trace, converted to integer
          endif
c
c   mseed
c
          if(out_format(1:5).eq.'mseed') then
c
c  Compression flag   0 - STEIM1     1 - STEIM2
c 
             compFlag = 0
c 
c  Record Size
c
             recSize = 4096

             seed_comp(1:2)=wav_comp(i)(1:2)
             seed_comp(3:3)=wav_comp(i)(4:4)
c
c   transefer data to integer array
c
             k=1
             do l=first_samp(i),last_samp(i)
                signal_int(k)=signal1(l)         ! copy window
                k=k+1
             enddo
c              
c
c  Append flag        0 - Replace    1 - Append
c
             if(i.eq.1) then
                appendFlag = 0
             else
                appendFlag = 1
             endif
c
c             write(6,*) 'wrinting mseed', wav_nsamp(i)
             call seed_channel_write(outfile,
     *       wav_stat(i), seed_comp, wav_network(i), wav_location(i),
     *       wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *       wav_sec(i),wav_rate(i),
     *       wav_nsamp(i),signal_int,
     *       appendFlag,compFlag,recSize,wav_time_error(i))
          endif
      enddo
      if(out_format(1:6).eq.'seisan')close(2)
c
c   if many files go back for next
c
      if(in.eq.1) goto 1000
      stop
      end

