c
c   subroutiens for reading guralp gcf files
c   some of the routiens were before in gursei
c   used in wave and gursei
c
c   jh nov 2011
c
c   changes
c
c dec  03 2012 jh: remove some test output
c dec   8 2012 jh: change reading unit, intferes with output unit in
c                  wavetool, change from 96 to 99, but in an index
c feb  7 2013  jh: change B  Z to BH Z etc for default channel
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine check_if_guralp(file,format)
c
c check if this is a guralp gcf file 
c
      implicit none
      character*80 file
      character*10 format



c--channel header date and times	  
      integer year,month,day,
     *hour,min
      real sec
      real rate	  
      integer nrecord         ! records in following data section
      integer comp_code       ! guralp compression code
      character*6 system_id,stream_id  ! guralp data identifiers
      integer x4(256)         ! 4 byte integers in a block
      character*1 block(1024) ! corresponding string
      integer*2 x2(512)       ! 2 byte integers in a block
      integer*1 x1(1024)      ! one byte intger, for linux or pc
      integer seiclen,i
cfor sun or pc     logical*1 x1(1024)      ! one byte intger
      equivalence (x4,block)
c
c   first check if file tyep is gcf
c
        i=seiclen(file)
        if(file(i-2:i).eq.'gcf') then
           format(1:6)='GURALP'
           return
        endif
c
c   now make a few primitive checks inside file
c
c
c  open file 
c
       open(99,file=file,status='old',
     * access='direct',recl=1024)
c
c   read header block to get start time etc
c
c
       format=' '
       read(99,rec=1,err=99) block
       close(99)
c
c   get file start time etc
c
       call read_guralp_head(block,year,month,day,hour,
     *    min,sec,rate,nrecord,comp_code)

c      write(6,*) 'ymdhms',year,month,day,hour,min,sec
c      write(6,*) 'rate,nr,comp_code',rate,nrecord,comp_code
c
c   get system id and stream id
c
       call get_36_string(x4(1),system_id)
       call get_36_string(x4(2),stream_id)
c      write(6,*) 'System and stream id ',system_id,'  ',stream_id
c
c  check year
c
       if(year.gt.2020.or.year.lt.1970) return
c
c  check component code
c       
       if(comp_code.le.0.or.comp_code.gt.16) return
c
c   check stream id
c
       if(stream_id(5:5).ne.'Z'.and.stream_id(5:5).ne.'N'.
     * and.stream_id(5:5).ne.'E') return
c
c   check system id
c
       if(system_id(1:1).ne.'0'.and.system_id(1:1).ne.' ') return
c
c   check sample rate, assume sample rates are 10, 20 etc
c
       if(rate.gt.1.and.rate.lt.10) return
       if(rate.gt.10.and.rate.lt.20) return
       if(rate.gt.20.and.rate.lt.30) return
       if(rate.gt.30.and.rate.lt.40) return
       if(rate.gt.40.and.rate.lt.50) return
       if(rate.gt.50.and.rate.lt.60) return
       if(rate.gt.60.and.rate.lt.70) return
       if(rate.gt.70.and.rate.lt.80) return
       if(rate.gt.80.and.rate.lt.90) return
       if(rate.gt.90.and.rate.lt.100) return
       if(rate.gt.500) return
c
c   if here it should be a guralp format
c
       format(1:6)='GURALP'
c

 99   continue
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine get_guralp_header(ifile)
c
c   get gurlap header from one file and put data into wav common block
c
      implicit none
      include 'seidim.inc' ! dimensions
      include 'waveform.inc'


c
c-----------------------------------------------------------------------
c   start block of definitions for most conversion programs
c

      character*29 mainhead_text   ! text to be put into main header
      character*80 text
c-- input file number 
      integer ifile	  

c-- network code
      character*5 net_code	  

c--channel header date and times	  
      integer year,month,day,
     *hour,min
      real sec

c-- channel samples and sample rate
      integer nsamp
      real rate	  
c
c--end block of definitions most conversion programs
c---------------------------------------------------------------------

      integer i,j,k
c-- computer type
      logical pc,sun,linux

      integer nrecord         ! records in following data section
      integer def_read        ! if 999, def file already read
      integer comp_code       ! guralp compression code
      character*6 system_id,stream_id  ! guralp data identifiers
      integer x4(256)         ! 4 byte integers in a block
      character*1 block(1024) ! corresponding string
      integer*2 x2(512)       ! 2 byte integers in a block
      integer*1 x1(1024)      ! one byte intger, for linux or pc
cfor sun or pc     logical*1 x1(1024)      ! one byte intger
      equivalence (x4,block)

      call computer_type(sun,pc,linux)
c
c   get def file for station codes unless already read
c   indicated by def_read=999
c
       if(def_read.ne.999) then
          text='gursei.def'
          call read_def_chan(text,mainhead_text,net_code)
          def_read=999
       endif
c
c  open file 
c
       open(99,file=wav_filename(ifile),status='old',
     * access='direct',recl=1024)
c
c   read header block to get start time etc
c
c
       read(99,rec=1,err=99) block
c
c   get file start time etc
c
       call read_guralp_head(block,year,month,day,hour,
     *    min,sec,rate,nrecord,comp_code)

       close(99)

c       write(6,*) 'ymdhms',year,month,day,hour,min,sec
c       write(6,*) 'rate,nr,for',rate,nrecord,comp_code
c
c   get system id and stream id
c
       call get_36_string(x4(1),system_id)
       call get_36_string(x4(2),stream_id)
c       write(6,*) 'System and stream id ',system_id,'  ',stream_id
c
c  put into wav common block
c
c
c   add one channel
c
       wav_nchan=wav_nchan+1
       k=wav_nchan
       wav_year(k)=year
       wav_month(k)=month
       wav_day(k)=day 
       wav_stat(k)=' '
c
c   initially use stream for station name
c
       wav_stat(k)=stream_id(1:4)//' '

c   default station and channel names
c
       wav_comp(k)='BH Z'
       wav_comp(k)(4:4)=stream_id(5:5)
c
c change station or component names according to def file
c
      call set_def_chan(1,wav_stat(k),wav_comp(k))

       wav_location(k)=' '
       wav_network(k)=' '
       wav_time_error(k)=' '
       wav_hour(k)=hour
       wav_min(k)=min
       wav_sec(k)=sec
       wav_file_nr_chan(k)=ifile
       wav_chan_nr_file(k)=1    ! one channel file only      
       wav_rate(k)=rate
       wav_cbyte(k)='4'

 99   continue
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_guralp_data(ichan)
c
c   read one guralp file. it is assumed that header is already known
c   but not number of samples. use wav common blocks
c   ichna is global from wav blocks

      implicit none
      include 'seidim.inc' ! dimensions
      include 'waveform.inc'

      integer yr,mo,da,hr,mn    ! date and time any block
      real se,ra                ! second and sample rate, any block
      character*80 text         ! general text
c-- times since epoch for checking that blocks are contiguous (RL 03/2006)
      double precision start_time
      double precision prev_end_time

      integer ifile  ! file number in wav block to read
      integer ichan  ! channel number in wav block	  
	  
c--Counters etc
      integer i,j,k
c-- computer type
      logical pc,sun,linux
      integer nsmp            ! sample counter
      integer nrecord         ! records in following data section
      integer comp_code       ! guralp compression code
      character*6 system_id,stream_id  ! guralp data identifiers
      integer x4(256)         ! 4 byte integers in a block
      character*1 block(1024) ! corresponding string
      integer*2 x2(512)       ! 2 byte integers in a block
      integer*1 x1(1024)      ! one byte intger, for linux or pc
cfor sun or pc     logical*1 x1(1024)      ! one byte intger
c      logical*1 x1(1024)      ! one byte intger
      integer*4 xlast         ! last value in block, used for control
      integer max24           ! largest 24 bit value
      integer nsamp_block     ! number of samples in one block
      integer sample          ! one sample
      character*4 csample     ! same as string
      equivalence (x4,block)
      equivalence (x2,block)
      equivalence (x1,block)
      equivalence (sample,csample)
c
c  find which file corresponds to channel number
c
      ifile=wav_file_nr_chan(ichan)
c
      max24=2**23
c
      call computer_type(sun,pc,linux)
c
c  open file 
c
       open(99,file=wav_filename(ifile),status='old',
     * access='direct',recl=1024)
c
c---------------------------------------------------------------
c   read file and decode numbers, one block at a time
c---------------------------------------------------------------
c
      i=1
      nsmp=1
c
c   back here after reading and decoding one block
c
 30   continue
c
      read(99,rec=i,err=50) x4    ! error is also the end
c
c  read header, does not open or close file
c
      call read_guralp_head(block,yr,mo,da,hr,
     *    mn,se,ra,nrecord,comp_code)

c
c   this could be a status block indicated by sample rate zero, if so skip it
c
      if(ra.eq.0.0) then
          i=i+1
          goto 30
      endif

c   Check for missing data between previous block and this one.
c   If there is a gap then fill space with 0s                          (RL 03/2006)
c      start_time = (((hr*60)+mn)*60)+se
      call timsec(yr,mo,da,hr,mn,se,start_time)
      if (nsmp .ne. 1 .and. start_time .ne. prev_end_time) then
          nsamp_block = (start_time - prev_end_time) * ra
          do j=1,nsamp_block
              signal_int(nsmp)=0
              nsmp=nsmp+1
          enddo	  
      endif
	 
c
c   samples in block
c
      nsamp_block=comp_code*nrecord
c
c   first sample in block, possibly swap
c
      if(pc.or.linux) call swap4_one(x4(5))
c
c   save last sample in block, possibly swap
c
      xlast=x4(nrecord+6)
      if(pc.or.linux) call swap4_one(xlast)
c
c   put into output data vector
c
      signal_int(nsmp)=x4(5)
      nsmp=nsmp+1
c
c   get remaining compressed values and  find differences
c   depending on compression
c
c
c   one byte data
c
      if(comp_code.eq.4) then
         do j=2,nsamp_block
            signal_int(nsmp)=x1(j+20)+signal_int(nsmp-1)
            nsmp=nsmp+1
         enddo
      endif
c
c   two byte data
c
      if(comp_code.eq.2) then
         if(pc.or.linux) call swap2(512,x2)
         do j=2,nsamp_block
           signal_int(nsmp)=x2(j+10)+signal_int(nsmp-1)
           nsmp=nsmp+1
         enddo
      endif
      
c
c   4 byte data
c
      if(comp_code.eq.1) then
         if(pc.or.linux) call swap4(256,x4)
         do j=2,nsamp_block
           if(x4(j+5)+signal_int(nsmp-1).gt.max24) then  ! check 24 bit overflow
              sample=x4(j+5)+signal_int(nsmp-1)         ! save sample
              if(sun) then
                 csample(1:1)=char(255)            ! add sign
              else
                 csample(4:4)=char(255)
              endif
              signal_int(nsmp)=sample
           else
              signal_int(nsmp)=signal_int(nsmp-1)+x4(j+5)
           endif
           nsmp=nsmp+1
         enddo
      endif
c
c   check that last value is correct
c
      if(signal_int(nsmp-1).ne.xlast) then
        write(6,*)' Something wrong with decompression'
        write(6,*)' Block #', i
        write(6,*) ' Last decompressed ',signal_int(nsmp-1)
        write(6,*) ' Last correct value ',xlast
        stop
      endif

c
c   Remember time of end of block so can check for missing data
c
      prev_end_time = start_time + nsamp_block/ra

c
c   go back to read next block
c
      i=i+1   

      goto 30
c
c------------------------------------------------------
c  all data decoded
c------------------------------------------------------
c
 50   continue
      close(99)   
c     write(6,*) 'Number of samples', nsmp-1
c     write(6,*) 'chan in read',ichan
      wav_nsamp(ichan)=nsmp-1
c
c   only after reading all samples is it known how many samples there are
c
      k=ichan
      wav_duration(k)=(wav_nsamp(k))/wav_rate(k)  ! jh aug 2008
      call timsec(wav_year(k),wav_month(k),wav_day(k),
     * wav_hour(k),wav_min(k),wav_sec(k),wav_abs_time(k))

      wav_nsamp(ichan)=nsmp-1

      do i=1,wav_nsamp(ichan)
        signal1(i)=signal_int(i)
      enddo 	  	  	  		 	     	  
      return
      end	   	  	  	         	  	  
								  
c
       subroutine get_36_string(val,string6)
c
c   decode guralp base 36 number
c
c      val: input integer
c      string6: 6 char output string
c
       implicit none
       character*6 string6
       integer val
       character*1 st(36)
       logical sun,pc,linux    ! computer type
       integer i,j,k
       data st/'0','1','2','3','4','5','6','7','8','9',
     *         'A','B','C','D','E','F','G','H','I','J',
     *         'K','L','M','N','O','P','Q','R','S','T',
     *         'U','V','W','X','Y','Z'/

        string6=' '      ! initialize with blanks
c
        call computer_type(sun,pc,linux)

c
c   swap if on pc
c
       if(pc.or.linux) call swap4_one(val)
c
       do i=1,6
          j=val/36
          k=val-j*36
          if(k.ge.0) string6(7-i:7-i)=st(k+1)
          val=j
       enddo

       return
       end

      
      subroutine read_guralp_head(block,year,month,day,hour,min,sec,
     *                            rate,nrecord,comp_code)

c
c  read part of one 16 byte guralp gcf header
c
      implicit none
      character*1 block(1024)   ! guralp data block
      integer year,month,day,doy,hour,min
      real sec
      real rate           ! sample rate
      integer nrecord     ! number of records in data block
      integer comp_code   ! compression code, see guralp format description
      integer i,k
      integer seconds_day ! seconds since midnight
      integer day_89      ! days since 17 nov 1989
      double precision  time89  ! for time conversion
c
c  date and time
c
      day_89=ichar(block(9))*256+ichar(block(10))
      k=day_89          ! save
      day_89=day_89/2   ! lowest bit is part of 17 bit second number
      seconds_day    =ichar(block(11))*256+ichar(block(12))
c
c  check if bit 17 is used
c
      i=k-day_89*2
      if(i.eq.1) seconds_day=seconds_day+65536 ! 17 bit is used
      call timsec(1989,11,17,0,0,0.0,time89)   ! time from 1900 in secs
      time89=time89+day_89*24*3600.0+seconds_day     ! time till now
      call sectim(time89,year,doy,month,day,hour,min,sec) ! data now
c
c  sample rate
c
      rate=ichar(block(14))
c
c  compression code
c
      comp_code=ichar(block(15))
c
c   number of records
c
      nrecord=ichar(block(16))

      return
      end


