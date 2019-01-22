c***************************************************************************
c
c     Program: sample_read_cont
c
c
c     program is a simple example of reading from continuous waveform data base 
c
c     how it works: start program with command sample_read_cont yyyymmddhhmmss timewindow
c                this will extract interval timewindow in minutes starting at time
c                yyymmddhhmmss using all continous data bases as defined in SIESAN.DEF,
c                program WAVETOOL can be used to extract also specific channels
c
c
c     May 2003  jh        
c
c***************************************************************************

      implicit none 

c
c read SEISAN include files
c
      include 'seidim.inc'   ! dimensions
      include 'seisan.inc'   ! general seisan parameters including contiuous data base names
      include 'waveform.inc' ! for waveform handling

      character*80 outfile   ! name of output seisan  waveform file
      character*1040 chead   ! seisan channel header
      character*80 mainhead(max_trace) ! seisan waveform main header
      character*5 net_code   ! code of network
      character*80 arg(5)    ! arguments to cfile
      integer nargs          ! number of arguments
      real min               ! minute

      integer i,l            ! counter

c
c  signal that reading is from a continous data base
c
      cwav=.true.
c
c   get seisan defaults including names of continuous waveform data bases
c
      call get_seisan_def 
c
c check if cont databases defined, n_cont_base gives the number of data bases
c
      if (n_cont_base.eq.0) then
         write(*,*) ' No continuous database defined in SEISAN.DEF '
         stop
      endif
c
c write out the database names
c
      write(*,*)'Databases are:'
      do i=1,n_cont_base
 	 write(*,*)'    ',cont_base(i)
      enddo
c
c get arguments, first is start time in yyyymmdd... and second is interval in min
c this argument function is SEISAN general, has the same form on all computer platforms
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

      read(arg(2),'(f5.0)') min
      cont_interval=min*60.

c
c   calculate end time and extended start time, needed in order to
c   make sure enough files read in
c
      call cwav_time_limits(0)
c
c  read the header information for all files in allo bases in time
c  interval, assume info available in common block
c
      call cwav_read_bases     
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
      do i=1,n_cont_trace
          call wav_read_channel(i)                            ! read one trace, read as integer, converted to real
          call wav_sheads(i,net_code,outfile,mainhead,chead)  ! make channel header
          write(66) chead                                     ! write channel header
          write(66)(int(signal1(l)),l=1,wav_nsamp(i))         ! write trace, converted to integer
      enddo

      stop
      end


