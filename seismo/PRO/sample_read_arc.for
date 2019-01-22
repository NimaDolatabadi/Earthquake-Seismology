c***************************************************************************
c
c     Program: sample_read_arc
c
c
c     program is a simple example of reading from an archive, bud or seiscomp 
c
c     how it works: 
c     start program with command 
c           sample_read_arc yyyymmddhhmmss timewindow 
c           this will extract interval timewindow in minutes starting at time
c           yyymmddhhmmss using all archive channels as defined in SEISAN.DEF,
c           program WAVETOOL can be used to extract  specific channels. 
c           max 30 channels for this test
c
c
c     nov 2010  jh   
c     jan 4 , 2013 jh : remove ref to scp and bud
c     oct 29  2014 jh : more removal o fabove     
c
c***************************************************************************

      implicit none 

c
c read SEISAN include files
c
      include 'seidim.inc'   ! dimensions
      include 'seisan.inc'   ! general seisan parameters including arc 
                             ! channel names
      include 'waveform.inc' ! for waveform handling

      character*80 outfile   ! name of output seisan  waveform file
      character*1040 chead   ! seisan channel header
      character*80 mainhead(max_trace) ! seisan waveform main header
      character*5 net_code   ! code of network
      character*80 arg(5)    ! arguments to  program
      integer nargs          ! number of arguments
      real min               ! minute
      real x                 ! help variable

      integer i,l            ! counter

c
c  signal that reading is from an arc data base
c
      arc=.true.
c
c   get seisan defaults including names of arc channels
c
      call get_seisan_def 
c
c check if channels defined, arc_nchan gives the number of channels defined
c
      if (arc_nchan.eq.0) then
         write(*,*) ' No archive channels defined in SEISAN.DEF'
         stop
      endif
c
c write out the database names
c
      write(*,*)'Archive channels are:'
      do i=1,arc_nchan
 	 write(*,*)'    ',arc_stat(i),arc_comp(i),arc_net(i),arc_loc(i)
      enddo
      write(6,*)
      write(6,*)'Archive is: ',arc_archive
c
c get arguments, first is start time in yyyymmdd... and second 
c is interval in min
c this argument function is SEISAN general, has the same form on all 
c computer platforms
c
      call get_arguments(nargs,arg)
c
c check if any arguments
c
      if (nargs.eq.0) then
         write(*,*)
     *  'Start time (yymmdd...) and time',
     *  ' interval (min) must be given' 
         stop
      endif
c
c   store the start time
c
      cwav_start_time = arg(1)
c
c read the time interval and convert to seconds
c

      read(arg(2),'(f7.0)') min 
      cont_interval=min*60.

c
c  interval, assume info available in common block
c
      call wav_read_arc_headers    
c
c 
      write(6,*)' Number of channels with data',wav_nchan
c
c     stop if no data
c
      if(wav_nchan.eq.0) then
          write(6,*) ' No data'
          stop
      endif
c
c     write header info
c
      do i=1,wav_nchan
        write(6,'(1x,a5,1x,a3,1x,a2,1x,a2,i4,4i2,f6.1,f7.1,i8)') 
     *  wav_stat(i),wav_comp(i),wav_network(i),
     *  wav_location(i),wav_year(i),wav_month(i),wav_day(i),
     *  wav_hour(i),wav_min(i),wav_sec(i),
     *  wav_rate(i),wav_nsamp(i)
      enddo
c
c   make and write seisan output header
c
      net_code='TEST '
      call wav_sheads(1,net_code,outfile,mainhead,chead)   ! make headers
      write(6,*) outfile
      open(66,file=outfile,status='unknown',form='unformatted')
c
c   write headers, assumem maximum of 30 channels, since only 12 headers are written out
c
      do i=1,12
          write(66) mainhead(i)
      enddo
c
c  read the waveform data, one trace at a time
c
      do i=1,wav_nchan
          call wav_read_channel(i)                            ! read one trace, read as integer, converted to real
          call wav_sheads(i,net_code,outfile,mainhead,chead)  ! make channel header
          write(66) chead                                     ! write channel header
          write(66)(int(signal1(l)),l=1,wav_nsamp(i))         ! write trace, converted to integer
      enddo

      stop
      end


