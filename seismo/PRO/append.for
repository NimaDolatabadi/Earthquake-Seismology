c
c   Program to append seisan waveform files jh, sep 99
c
c   The program uses a filenr.lis input file. All files are read,
c   and  then written out as one new file.
c   The maximum number of channels handles is max_chan_out which
c   is set as a parameter. Only the first max_chan_out channels
c   are used or less if fewer channels in file.
c   A blank line followed by a new group of file will make a new 
c   output file.
c
c   It is assued that all channels have the same start and end times
c   in the same file
c   and that the sample rate is the same.
c
c   The sample rate for the appended file, if more than one input file, is 
c   calculated from the header times of the first and last files. 
c
c
c   changes:
c
c   may 2000 lo : new waveform structure
c
      implicit none
      include 'seidim.inc'                     ! dimensions
      include 'waveform.inc'
C
C    Seisan library details...
C    -------------------------
C
      INCLUDE   'libsei.inc'                   ! Definitions and data defns.
      external  sei open,                      ! Open file handler.
     &          sei close,                     ! Close file handler.
     &          sei code,                      ! Error handler.
     &          sei clen                       ! String length
      integer   sei clen                       ! Function.
c
      integer   code,                          ! Local error condition.
     &          read1                          ! Read unit 1.
      logical   b_flag                         ! Flag existance.?
C
C    -------- end ------------
c                                                                               
c                                                                               
c   local variables                                                             
c                                                          
      integer max_chan_out                ! max number of channels out
      integer max_chan_out_dim            !---------------------------
      parameter (max_chan_out_dim=7)   
      character*80 mainhead(max_trace)    ! main header
      character*80 mainhead_out(max_trace)! output ----
      character*1040 chead                ! channel header                     
      character*1040 chead_out(max_chan_out_dim)         ! output channel header
      integer isamp(max_chan_out_dim)         ! output sample counter                                                                                
      character*5 net_code                ! code for resampled file
      character*80 text
      integer ifile                       ! file counter
      integer nfile                       ! number of files in one sequence
      integer maxfile                     ! number of files to merge
      integer ichan                       ! channel counter
      integer numb_samp                   ! number of samples, one trace
      integer numb_samp_total             ! -------------------------, all
      integer unit_wave                   ! unit to read waveform file from
      integer write01                     ! unit for writing
                                          ! between two files
      real rate                           ! sample rate from header
      real rate_out                       ! sample rate calculated
      logical more_files                  ! there could be more files
c-- plotting string                                      
      character*80 files(200)             ! waveform file name
      character*80 out_file,outfile       ! resampled file name
c-- dc value                                                      
c      integer nchan                          ! number of channels in file
c      integer nchan_out                      ! number of channels out
      double precision   file_start_time     ! abs file start time
      double precision   file_end_time       ! abs file end time
      double precision   first_file_time     ! abs file time, first file
                                             ! decimated output
      real total_time
      integer year1,month1,day1,hour1,min1    ! time etc
      real sec1
c-- counters and help variables
      integer	i		
      integer data_out(max_chan_out_dim,max_sample)  !output 

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      max_chan_out=max_chan_out_dim
c
      write(6,*)
     *' Enter new network code, default is channel one station'
      read(5,'(a5)') net_code

c      write(6,*) 'Number of channels out, max is ',max_chan_out
c      write(6,*) 'The count starts from the first channel'
c      read(5,*) max_chan_out
      write(6,*) 'Number of files to merge, enter for all'
      read(5,'(a)') text
      maxfile=100000
      if(text.ne.' ') read(text,*) maxfile
c      write(6,*) maxfile 
c
c    open file containing list of files numbered                                
c    -------------------------------------------
c                                                                               
      call sei open( old$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               'filenr.lis',         ! Filename.
     &               read1,                ! Open on unit.
     &               b_filenr$,            ! Flag existance & usage?.
     &               code )                ! Condition.
      chr_err_msg$ = 
     &'**** FATAL: problem opening "filenr.lis"'
      call sei code( stop$, code, 0, b_flag ) ! Force an abort on error.
c
c   get  here for another set of files to convert
c
 500  continue
c
c   set some defaults
c
      ifile=1
      do i=1,max_chan_out
         isamp(i)=0                               ! reset output sample counter
      enddo
      numb_samp_total=0
c
c  file read loop
c
 1000 continue
c
c   get next file name
c
      read(read1,'(7x,a)',iostat=code) files(ifile)           ! Read a record.
      call sei code( fort$, code, read1, b_flag )             ! Process outcome.
      if( b_flag.and.ifile.eq.1 ) goto 1999                   ! On end of file.
c-- no more files                        
      if(files(ifile)(1:5).eq.'     '.or.b_flag) then !  new group or eof
         more_files=.true.                ! there could be more files
         nfile=ifile-1                    ! the last was blank
         if(nfile.eq.0) goto 1000         ! could be more blanks
         goto 1200
      endif
      ifile=ifile+1
      if(ifile.gt.maxfile) then
         nfile=ifile-1
         goto 1200
      endif
      goto 1000                           ! read next file
c
c   read filenames of one group, now process
c
 1200 continue
c
c   open and read first file to get specifics
c
      write(6,*)'*********************************************'
      write(6,*) ' Number of files to append:', nfile
      write(6,*)
c      chr_f_access$ = 'direct'            ! Type of file.
c      f_recl$=2048                        ! Record length
c      call sei get file( open$+ignore$,   ! Open waveform file.
c     &                   unit_wave,       ! On unit.
c     &                   code,            ! Returned condition.
c     &                   'WAV',           ! Alternative search directory.
c     &                   files(1))        ! For this filename.
c 
c         if( code .ne. e_ok$ ) then       ! File does not exist
c            write(*,*)
c            write(*,*)
c     &'**** WARN: ',files(ifile)(:seiclen(files(ifile)))
c     &,' does not exist ****'
c            write(*,*)
c            call sei code(stop$,e_misf$,0,b_flag) ! & stop.
c         end if                                   !
c
c   read main header 
c

      wav_filename(1)=files(1)
c
c init wave variables
c
      call wav_init
c
c read header
c
      call read_wav_header(1)
      if(wav_error_message.ne.' ') then
         write(6,'(1x,a)') wav_error_message
         stop   
      endif
c
c generate main header
c
      call wav_sheads(1,net_code,outfile,mainhead,chead)

c      call seisinc                                                           
c     *(unit_wave,0,nchan,0,
c     *mainhead,chead,0.0,0.0)
c      call sei close( close$, unit_wave, code )       ! Close down file

c
c   check for error
c
c      if(nchan.lt.0) then
c         write(6,*)' Something wrong with first file in a sequence'
c         stop
c      endif

   
c
c   save header and file name
c
      do i=1,12
          mainhead_out(i)=mainhead(i)
      enddo
c
c   if more than max_chan_out channels, cut down to max_chan_out 
c
      if(wav_nchan.gt.max_chan_out) then
         write(6,*)' Input file has ',wav_nchan,' channels'
         write(6,*)' The output will only use ',max_chan_out
         wav_out_nchan=max_chan_out
      else
         wav_out_nchan=wav_nchan
      endif
      write(6,*)' Number of channels used:',wav_out_nchan
c
c-------------------------------------------
c   loop for channels
c-------------------------------------------
c                           
      do 1600 ichan=1,wav_out_nchan
c
c   loop for files
c
      do 1500 ifile=1,nfile
         write(6,'(1x,a)') files(ifile)
c        write(27,'(1x,a)') files(ifile)
c         chr_f_access$ = 'direct'            ! Type of file.
c         f_recl$=2048                        ! Record length
c         call sei get file( open$+ignore$,   ! Open waveform file.
c     &                      unit_wave,       ! On unit.
c     &                      code,            ! Returned condition.
c     &                      'WAV',           ! Alternative search directory.
c     &                      files(ifile))    ! For this filename.
c 
c         if( code .ne. e_ok$ ) then       ! File does not exist
c            write(*,*)
c            write(*,*)
c     &      '**** WARN: ',files(ifile)
c     &      (:seiclen(files(ifile))),' does not exist ****'
c            write(*,*)
c            call sei code(stop$,e_misf$,0,b_flag) ! & stop.
c         end if                                   !
c         call seisinc 
c     *   (unit_wave,ichan,nchan,2,
c     *   MAINHEAD,chead,0.0,0.0)

         wav_filename(1)=files(ifile)
c
c init wave variables
c
         call wav_init
c
c read header
c
         call read_wav_header(1)
         if(wav_error_message.ne.' ') then
            write(6,'(1x,a)') wav_error_message
            stop   
         endif
c
c generate headers 
c
         call wav_sheads(1,net_code,outfile,mainhead,chead)
         write(6,'(1x,a)')mainhead(1)(1:79)            
         write(27,'(1x,a)')mainhead(1)(1:79)            
         write(6,'(1x,a)') chead(1:79) 

c
c read channel
c
         call wav_read_channel(ichan)

c
c   check for error
c
c         if(nchan.lt.0) then
c            write(6,*)' Something wrong with input file'
c            stop
c         endif

c
c   if first file, save channel header
c
         if(ifile.eq.1) then
           chead_out(ichan)=chead
         endif           
c                                                    
c   get srate and number of samples                                               
c                                                                               
c        read(chead(37:50),'(f7.2,1x,i6)') rate,numb_samp
         rate=wav_rate(ichan)
         numb_samp=wav_nsamp(ichan) 
         if(ichan.eq.1) numb_samp_total=numb_samp_total+numb_samp
c
c   get times
c
         year1=wav_year(ichan)
         month1=wav_month(ichan)
         day1=wav_day(ichan)
         hour1=wav_hour(ichan)
         min1=wav_min(ichan)
         sec1=wav_sec(ichan)
         call timsec(year1,month1,day1,hour1,min1,sec1,file_start_time)
c
c   save start time and make output file name if first file
c
         if(ifile.eq.1) then
           first_file_time=file_start_time
           
           if (net_code.eq.'     ') then
             net_code = wav_stat(1)
           endif
           call wav_sheads(1,net_code,out_file,mainhead,chead)
           out_file(19:19)='A'
         endif

c
c  put in  output buffer
c
         do i=1,numb_samp
           isamp(ichan)=isamp(ichan)+1
           data_out(ichan,isamp(ichan))=signal1(i)
         enddo
c
c         call sei close( close$, unit_wave, code )       ! Close down file
         file_end_time=file_start_time+(numb_samp)/rate ! start of next file
c
c  end of file loop
c
 1500 continue
c
c   end of channel loop
c
 1600 continue
c
c-----------------------------------------------------------------------
c   enter here when all files read for one sequence
c-----------------------------------------------------------------------
c
c
 1700 continue
c
c   calculate observed sample rate if more than one file read, assume
c   file follow each other
c
c     if(nfile.gt.1) then
c        rate_out=
c    *   (file_start_time-first_file_time)/(numb_samp_total-numb_samp)
c        rate_out=1.0/rate_out
c        write(6,*) 'Sample rate from header times:', rate_out
c        rate=rate_out
c     endif
c
c  calculate new sample rate
c
c     rate=rate/float(skip)
c
c   finish making main header and write it out, assume max 30 channels
c
      total_time=float(isamp(1))/rate       ! assume all channels same isamp
      write(mainhead_out(1)(61:69),'(f9.2)') total_time
      write(mainhead_out(1)(31:33),'(i3)') wav_out_nchan
      do i=3,(wav_out_nchan-1)/3+3
         if(total_time.gt.99999.0) then
            write(mainhead_out(i)(19:26),'(f8.1)') total_time
            write(mainhead_out(i)(45:52),'(f8.1)') total_time
            write(mainhead_out(i)(71:78),'(f8.1)') total_time
         else
            write(mainhead_out(i)(19:26),'(f8.2)') total_time
            write(mainhead_out(i)(45:52),'(f8.2)') total_time
            write(mainhead_out(i)(71:78),'(f8.2)') total_time
         endif
      enddo
      write(6,'(a,1x,a)') ' Output file name is: ',out_file(1:40)
c
c   open file and write main header
c
      chr_f_form$ = 'unformatted'
      call sei open( unknown$,             ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   out_file,         ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
      do i=1,12
         write(write01)mainhead_out(i)
         if(i.lt.4) write(6,'(a79)') mainhead_out(i)(1:79)		 
      enddo
c
c   write channels, 99999945 to round off
c
      do ichan=1,wav_out_nchan
         chead_out(ichan)(1:5)=wav_stat(ichan)
         chead_out(ichan)(6:9)=wav_comp(ichan)
         write(chead_out(ichan)(37:50),'(f7.3,i7)')rate,isamp(ichan)
         if(rate.lt.99.999945)
     *   write(chead_out(ichan)(37:50),'(f7.4,i7)')rate,isamp(ichan)
         if(rate.lt.9.9999945)
     *   write(chead_out(ichan)(37:50),'(f7.5,i7)')rate,isamp(ichan)
         if(rate.lt.0.99999945)
     *   write(chead_out(ichan)(37:50),'(f7.6,i7)')rate,isamp(ichan)
         chead_out(ichan)(77:77)='4'    ! always store as 4 byte
         write(write01) chead_out(ichan)
         write(6,'(1x,a)') chead_out(ichan)(1:79)
         write(write01) (data_out(ichan,i),i=1,isamp(ichan))
c         write(6,*)'Re-calculated sample rate',rate
      enddo
      call sei close( close$, write01, code )       ! Close down file
c
c  check if more files to convert
c
      goto 500
c
c
 1999 continue
      stop
      end

