c--------------------------------------------------------------------------
c  sample program to read a channel from s-file and make instrument correction

c  This program will mainly demonstrate how to make instrument correction, but is also demonstrates several other standard subroutine calls in SEISAN. The operations are:

c  Read S-file with readings and location
c  Get wave form files from S-file
c  Enter station and component for instrument correction
c  Find S-time from readings
c  Select out a time window for waveform channel around  S-time
c  Find channel number in waveform file(s) corresponding to desired channel
c  Read the S-time window from waveform channel
c  Read response file
c  Prepare response removal, different filters and poles and zeros possible, 
c  the example is Wood-Anderson response
c
c  Correct for instrument response
c  Write out corrected data in an ASCII file in Helberger format
c  Automatically pick maximum amplitude and corresponding period
c  Write out results


c
c--------------------------------------------------------------------------c
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'waveform.inc'              ! waveform data
      include 'rea.inc'                   ! parameter common bliock
      include 'libsei.inc'                ! for all seisan subroutines

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file

      integer npresent                    ! number of wav files present
      character*80 text                   ! general text string
      real flow,fhigh                     ! filters
      integer pole_low,pole_high          ! poles of filters
      complex pole(max_resp_value),zero(max_resp_value)  ! complex PAZ
      
      integer npole,nzero                  ! number of poles and zeros
      real norm                           ! normalization constant
      real start_before_s                 ! window start before s
      complex y_com(max_sample)           ! spectrum

      integer nsamp                       ! number of samples

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer year,month,day,hour,min,doy
      real sec
      double precision s_time,hyp_abs_time(10)
      character*5 stat
      character*4 comp
      real amp,per                     ! amplitude and period
 
      real av                          ! average
      real fcmin,fcmax,yfirst,ylast    ! frequency range and first and last
                                       ! amplitude picked
      integer ifirst,ilast             ! sample number corrosponding to
                                       ! yfirst and ylast
      integer maxcros                  ! maximum number of zero crossings
c
      integer i,k,l                    ! counters

c
c   get input sfile name, check if exist
c

 9    continue
      write(6,*) 'Give input S-file '
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
c
c  initialize wav reading
c
      call wav_init
      call wav_mem_init
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump stop
c
      if(code.eq.1) goto 1000
c
c   get waveform file names, could be several
c
      call auto_tr(data,rea_nhead,rea_nrecord,wav_nfiles,wav_filename)
      
c   find how many waveform files are present,
c   replace the origial names without path, with the present files
c   full path like
c
       npresent=0                      ! no files initially
       do i=1,wav_nfiles
          call  get_full_wav_name(wav_filename(i),text)            
          if(text.ne.' ') then
             npresent=npresent+1
             wav_filename(npresent)=text
          endif
      enddo
c
c   print how many files were found
c
      wav_nfiles=npresent
      write(6,*)' Number of wav-files present', wav_nfiles

c
c   terminate if no waveform files
c
      if(wav_nfiles.eq.0) stop

c 
c  abs origin time
c
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),hyp_abs_time(1))
c     
c   write on screen a bit info on event, number of headers and
c   total number of records
c
      write(6,*)' Number of headers and number of records',
     *rea_nhead,rea_nrecord

c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)

c----------------------------------------------------
c
c   enter station and component to analyze
c
 50   continue   ! enter here if station not found

      write(6,*) 'Give station code, e.g. EGD'
      read(5,'(a)') stat
      write(6,'(a,a)')
     *'Give component code, e.g. S  Z, blank is first channel for ',
     * stat
      read(5,'(a)') comp
c
c
c   find where the S-phase window is for station stat
c
      do i=1,rea_nphase
        if(rea_phase(i)(1:1).eq.'S'.and.rea_stat(i).eq.stat) then
           s_time=rea_abs_time(i)
           goto 100   ! found so jump out
        endif
      enddo
c
c  S from P
c
      do i=1,rea_nphase
        if(rea_phase(i)(1:1).eq.'P'.and.rea_stat(i).eq.stat) then
           s_time=(rea_abs_time(i)-hyp_abs_time(1))*1.78+
     *     hyp_abs_time(1)
           goto 100   ! found so jump out
        endif
      enddo
c
c   no phase found
c
      write(6,'(a,a)')'No phase found for station ',stat
      s_time=-1
      goto 50    ! try again
      
c
c   a phase found
c
 100  continue

c
c  abs time of S
c
      call sectim(s_time, year,doy,month,day,hour,min,sec)

      write(6,'(a,i4,1x,2i2,1x,2i2,f6.1)') ' Time of S ',
     *year,month,day,hour,min,sec
c
c   loop to read all headers of all wav files
c
      do i=1,wav_nfiles
         write(6,'(1x,a)') wav_filename(i)
c
c   read all headers of file i
c
         call read_wav_header(i)
c
c   output possible errors
c
         if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *   wav_error_message
c
c   write out the format
c
         write(6,'(1x,a,a)') 'Format ',wav_file_format(i)
      enddo

      write(6,*)' Total number of channels available:',wav_nchan
c
c   terminate if no channels
c
      if(wav_nchan.eq.0) stop
c
c   write some input info for each channnel on screen
c
      write(6,'(a,a)')
     *'CHA STAT  COMP  YEAR MO DA HR MI    SEC   NSAMP ',
     *'    RATE    DELAY DURATION'
      do i=1,wav_nchan
         write(6,'(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i8,3f9.3)')
     *   i, wav_stat(i),wav_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i)
      enddo
c
c  find channel
c
      call wav_find_chan(stat,comp,k)
      write(6,*) wav_error_message
c
c   info on which channel it is
c
      write(6,'(a,a,a,i3)')'Channel number for ',stat,comp,k
 
      if(k.eq.0) then
        write(6,*) 'Channel not found'
        goto 50   ! try another
      endif
c
c   read whole channel
c
      call wav_read_channel(k)
c
c   select out time window, only for one channel
c
       start_before_s=10.0    ! start 10 s before S
       wav_out_nchan=1        ! one channel
       wav_out_chan(1)=k      ! channel number
c
c  start time relative to start of particular channel
c
       wav_out_start(k)=s_time-wav_abs_time(wav_first)-start_before_s
       wav_out_duration(k)=100.0 ! 100 s window
c
c   find if data is available
c
       call wav_get_interval

       write(6,*) 'Select status, 4 is ok',wav_out_status(k)
       write(6,*) 'Start sample',wav_out_first_sample(k)
       write(6,*) 'Duration available', wav_out_duration(k)
c
c   move data interval to beginning of array
c
       nsamp=wav_out_nsamp(k)

       l=1
       do i=wav_out_first_sample(k),
     * wav_out_first_sample(k)+ wav_out_nsamp(k)-1
         signal1(l)=signal1(i)
         l=l+1
       enddo 
c
c  subtract dc
c
      av=0.0
      do i=1,wav_out_nsamp(k)
        av=av+signal1(i)
      enddo
c
      av=av/wav_out_nsamp(k)

      do i=1,wav_out_nsamp(k)
        signal1(i)=signal1(i)-av
      enddo
c
c  specify channel for response 
c
      wav_resp_stat=wav_out_stat(k)
      wav_resp_comp=wav_out_comp(k)
      wav_resp_year=wav_out_year(k)
      wav_resp_month=wav_out_month(k)
      wav_resp_day=wav_out_day(k)

c
c   read response file
c
      call read_resp

      if(wav_resp_status(1:1).ne.' ') then
         write(6,'(a,a,a)') 'No response for ', 
     *   wav_resp_stat,wav_resp_comp
         stop
      endif
     
c
c  give filter used before response, in this case an addtional 
c  frequency domain filter to stabilize the instrument correction
c
      pole_low=4
      pole_high=4
      flow=1.25
      fhigh=20.0
c
c  wood-anderson poles and zeros to be multipled by corrected signal
c  set npole and nzero to zero if only response removal
c
      npole=2
      nzero=2
      norm=1.0
      pole(1)=(-6.283,-4.712)
      pole(2)=(-6.283, 4.712)
      zero(1)=(0.0,0.0)
      zero(2)=(0.0,0.0)      
c
c   apply a taper
c  
      call applytaper(signal1,nsamp,10.)   ! apply taper, width 10% of half the samples
c
c   correct for instument
c
      call remove_resp(signal1,y_com,nsamp,wav_out_rate(k),1,
     *  flow,fhigh,pole_low,pole_high,zero,pole,nzero,npole,
     * norm) 

c
c   write channel, in this example in ascii helmberger format
c
      open(10,file='test.wav',status='unknown')

      call write_chan_helm_out(k,1,10,nsamp,'')
      close (10)

c
c  set auto amp parameters
c
      fcmin=0.01
      fcmax=20.0
      maxcros=2
c
c   autopick amplitude
c
      call auto_amp
     *(signal1,wav_out_nsamp(k),wav_out_rate(k),fcmin,fcmax,maxcros,
     *yfirst,ylast,ifirst,ilast)
c
c   check if valid reading
c
      if(ifirst.eq.0.or.abs(yfirst-ylast).gt.90000000) then
         write(6,*)'No valid reading'   
c
c   calculate amplitude and period
c
      else
          amp=abs(yfirst-ylast)/2.0   ! first and last sampel number fo 2 extreemes
          per=2.0*abs(ilast-ifirst)/wav_out_rate(k)
c
c   calculate time of pick
c
          ifirst=ifirst
          call sectim(s_time+ifirst/wav_out_rate(k)-start_before_s, 
     *    year,doy,month,day,hour,min,sec)

          write(6,'(a,1x,i4,1x,2i2,1x,2i2,f6.1)')
     *'   Time of amplitude ',year,month,day,hour,min,sec
          write(6,'(a, 2f11.2)')  ' Amplitude(nm) and period(sec)',
     *    amp,per
       endif
      write(6,*) 
c
c     end of file
c
 1000 continue
c
      write(6,*)            ! blank line
      write(6,*) 'Output waveform file in test.wav'
      stop
      end
