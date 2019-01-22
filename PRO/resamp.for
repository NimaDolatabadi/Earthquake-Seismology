c
c   Program to resample seisan waveform files jh, dec 95
c
c   The program uses an filenr.lis input file. All files are read,
c   filtered and resampled. Then written out as one new file.
c   The maximum number of channels handles is max_chan_out which
c   is set as a parameter. Only the first max_chan_out channels
c   are used or less if fewer channels in file.
c   A blank line followed by a new group of file will make a new 
c   output file.
c
c   It is assued that all channels have the same start and end times
c   and that the sample rate is the same. The decimation MUST be a number
c   which can be divided up in rate. E.g. for rate 75, it can be 75,25,15,
c   5 or 3.
c
c   The files are checked for time consistency. If the the time calculated
c   from the last header time of a file and the last sample does not agree 
c   the header time of the following file within an error set in the program
c   (variable err_samp) two cases are considered:
c
c   The next file starts later: Could be becuse there has been a stop and 
c                               zeros are filled in
c   The next file starts earlier: There must be a serious timing error in the
c                                data. The user gets the choice to stop.
c
c
c   The sample rate for the resmapled file, if more than one input file, is 
c   calculated from the header times of the first and last files. Zeros put
c   in will be then into account
c   updates
c
c   The DC value must be removed to avoid problem when doing antialias
c   filtering. In order to avoid steps in the data, the dc level from
c   the first file is used throughtout the whole resample process.
c
c   resamp.out gives info on each file
c
c
c   oct 15 97 by jh: bug when missing files
c   dec 11         : remove dc before filtering
c   jun 8, 98      : fix problem of format error in sample rate write
c   april 7 99  jh :  --------------  version 7.0 upgrade------------
c   march 13 00 jh : err_samp increased from 70 to 300
c   oct 3    00 jh : possibel to write a time larger than 100 000
c   may 5 05    jh: remove stuff signalx block
c    2015-06-02 pv   : add signal_int to signalx common block due to
c                      compiler warning
c
      implicit none
      include 'seidim.inc'                     ! dimensions
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
      real      sei real num   
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
      parameter (max_chan_out=7)   
      character*80 mainhead(12)           ! main header
      character*80 mainhead_out(12)       ! output ----
      character*1040 chead                ! channel header                     
      character*1040 chead_out(max_chan_out)         ! output channel header
      integer isamp(max_chan_out)         ! output sample counter                                                                                
      character*5 net_code                ! code for resampled file
      integer skip                        ! skipping for resampling
      integer ifile                       ! file counter
      integer nfile                       ! number of files in one sequence
      integer ichan                       ! channel counter
      integer passes                      ! number of passes through filter
      integer npoles                      ! number of poles
      integer numb_samp                   ! number of samples, one trace
      integer numb_samp_total             ! -------------------------, all
      integer unit_wave                   ! unit to read waveform file from
      integer write01                     ! unit for writing
      integer nmiss                       ! number of files missing in a series
      real err_samp                       ! number of samples tolerated as error
      real time_shift                     ! accumulated time shift, one output f
                                          ! between two files
      real rate                           ! sample rate from header
      real rate_out                       ! sample rate calculated
      logical more_files                  ! there could be more files
c-- plotting string                                      
      character*80 files(200)             ! waveform file name
      character*80 out_file               ! resampled file name
c-- dc value                                                      
      real dc
      integer nchan                          ! number of channels in file
      integer nchan_out                      ! number of channels out
      double precision   file_start_time     ! abs file start time
      double precision   file_end_time       ! abs file end time
      double precision   first_file_time     ! abs file time, first file
      character*80       text                ! help text
      character*2        new_comp            ! first 2 letters of comp code in
                                             ! decimated output
      real total_time
      integer year1,month1,day1,hour1,min1    ! time etc
      real sec1
c-- counters and help varibales
      integer i
      real x
      integer data_out(max_chan_out,max_sample)  ! resampled data
      real filt                  ! anti alias filter frequency
c
c  for seisin common block
c
      real data(max_sample)
      integer signal_int(max_sample)
c     real baz(max_trace)
c     real        del_rot       ! delay rotated trace
c     character*1 rot_comp(max_trace)  ! T or R
c     logical rotate

c     common /signalx/data !for seisinc
      common /signalx/data,signal_int


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c
c
c   assume for the time filter and skip, used with 75 hz data to 1 hz data
c
      npoles=6
      filt=0.1 
      skip=75
      err_samp=300    ! max number of samples error between two files
      new_comp='L '  ! assuming decimaiton to LP
      write(6,'(a,3x,i7)')      ' Decimation is:', skip
      write(6,'(a,3x,f7.1)')    ' Filter is    :',filt
      write(6,'(a,8x,a)')       ' New component:', new_comp
      write(6,*)
     *' If ok, return, else enter new value for decimation'
      read(5,'(a)') text
      if(text(1:3).ne.'   ') then
         x=seirealnum(text,code)
         skip=x
         write(6,*)' Enter filter frequency'
         read(5,*) filt
         write(6,*)' Enter new component code, max 2 chars'
         read(5,'(a)') new_comp
      endif
      write(6,*)
     *' Enter new network code, default is channel one station'
      read(5,'(a5)') net_code
c
c  open logging file
c
      open(27,file='resamp.out',status='unknown')
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
      time_shift=0.0
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
      if( b_flag ) goto 1999                                  ! On end of file.
c-- no more files                        
      if(files(ifile)(1:5).eq.'     ') then ! if blank, ind. new group or eof
         more_files=.true.                ! there could be more files
         nfile=ifile-1                    ! the last was blank
         if(nfile.eq.0) goto 1000         ! could be more blanks
         goto 1200
      endif
      ifile=ifile+1
      goto 1000                           ! read next file
c
c   read filenames of one group, now process
c
 1200 continue
c
c   open and read first file to get specifics
c
      write(6,*)'*********************************************'
      write(6,*) ' Number of files to resample:', nfile
      write(6,*)
      chr_f_access$ = 'direct'            ! Type of file.
      f_recl$=2048                        ! Record length
      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   unit_wave,       ! On unit.
     &                   code,            ! Returned condition.
     &                   'WAV',           ! Alternative search directory.
     &                   files(1))        ! For this filename.
 
         if( code .ne. e_ok$ ) then       ! File does not exist
            write(*,*)
            write(*,*)
     &'**** WARN: ',files(ifile)(:seiclen(files(ifile)))
     &,' does not exist ****'
            write(*,*)
            call sei code(stop$,e_misf$,0,b_flag) ! & stop.
         end if                                   !
c
c   read main header 
c
      call seisinc                                                           
     *(unit_wave,0,nchan,0,
     *mainhead,chead,0.0,0.0)
      call sei close( close$, unit_wave, code )       ! Close down file
c
c   check for error
c
      if(nchan.lt.0) then
         write(6,*)' Something wrong with first file in a sequence'
         stop
      endif
c
c   save header and file name
c
         do i=1,12
            mainhead_out(i)=mainhead(i)
         enddo
c
c   if more than max_chan_out channels, cut down to max_chan_out 
c
      if(nchan.gt.max_chan_out) then
         write(6,*)' Input file has ',nchan,' channels'
         write(6,*)' The output will only use ',max_chan_out
         nchan_out=max_chan_out
      else
         nchan_out=nchan
      endif
      write(6,*)' Number of channels to resample:',nchan_out
c
c-------------------------------------------
c   loop for channels
c-------------------------------------------
c                           
      do 1600 ichan=1,nchan_out
c
c   loop for files
c
      do 1500 ifile=1,nfile
         write(6,'(1x,a)') files(ifile)
         write(27,'(1x,a)') files(ifile)
         chr_f_access$ = 'direct'            ! Type of file.
         f_recl$=2048                        ! Record length
         call sei get file( open$+ignore$,   ! Open waveform file.
     &                      unit_wave,       ! On unit.
     &                      code,            ! Returned condition.
     &                      'WAV',           ! Alternative search directory.
     &                      files(ifile))    ! For this filename.
 
         if( code .ne. e_ok$ ) then       ! File does not exist
            write(*,*)
            write(*,*)
     &      '**** WARN: ',files(ifile)
     &      (:seiclen(files(ifile))),' does not exist ****'
            write(*,*)
            call sei code(stop$,e_misf$,0,b_flag) ! & stop.
         end if                                   !
         call seisinc                                                           
     *   (unit_wave,ichan,nchan,2,
     *   MAINHEAD,chead,0.0,0.0)
         write(6,'(1x,a)')mainhead(1)(1:79)            
         write(27,'(1x,a)')mainhead(1)(1:79)            
         write(6,'(1x,a)') chead(1:79) 
c
c   check for error
c
         if(nchan.lt.0) then
            write(6,*)' Something wrong with input file'
            stop
         endif
c
c   if first file, save channel header
c
         if(ifile.eq.1) then
           chead_out(ichan)=chead
         endif           
c                                                    
c   get srate and number of samples                                               
c                                                                               
         read(chead(37:50),'(f7.2,1x,i6)') rate,numb_samp                       
         if(ichan.eq.1) numb_samp_total=numb_samp_total+numb_samp
c
c   get times
c
         read(chead(10:12),'(i3)') year1
         year1=year1+1900
         read(chead(18:35),'(4(i2,1x),f6.3)')month1,day1,hour1,min1,sec1
         call timsec(year1,month1,day1,hour1,min1,sec1,file_start_time)
c
c   save start time an dmake output file name if first file
c
         if(ifile.eq.1) then
           first_file_time=file_start_time
c-- year
           write(out_file(1:4),'(i4)') year1
           out_file(5:5)='-'
c-- month
           write(out_file(6:7),'(i2)') month1
           out_file(8:8)='-'
c-- day
           write(out_file(9:10),'(i2)') day1
           out_file(11:11)='-'
c-- hr
           write(out_file(12:13),'(i2)') hour1
c-- min
           write(out_file(14:15),'(i2)') min1
           out_file(16:16)='-'
c-- sec
           write(out_file(17:18),'(i2)') int(sec1)
           out_file(19:20)='R.'
c-- network or station code
           if(net_code.eq.' ') then
              out_file(21:25)=chead(1:5)
           else
              out_file(21:25)=net_code
           endif
           out_file(26:26)='_'
c-- number of channels
           write(out_file(27:29),'(i3)') nchan
c-- check for blanks
           do i=1,20
              if(out_file(i:i).eq.' ') out_file(i:i)='0'
           enddo
           do i=21,29
              if(out_file(i:i).eq.' '.or.ichar(out_file(i:i)).eq.0) 
     *        out_file(i:i)='_'
           enddo

         endif
c
c   check if start time is what to be expected, allow for a few samples 
c   error, only check if not the first file
c
         if(ifile.gt.1) then
c
c   check for next file starting before end of previous file
c   for the time being, let the error be 3 times larger than positive
c   errors
c
            if(file_start_time+3*err_samp/rate.lt.file_end_time) 
     *      then
               write(27,*)' Next file starts before end of previous',
     *                   ' file with the amount:', (file_end_time-
     *                   file_start_time)
                         write(27,'(a)') files(ifile)
                         write(27,*)
     *                   ' Next file starts before end of previous',
     *                   ' file with the amount:', (file_end_time-
     *                   file_start_time)
               write(27,*)
               write(6,*)' Next file starts before end of previous',
     *                   ' file with the amount:', (file_end_time-
     *                   file_start_time)
                         write(27,'(a)') files(ifile)
                         write(27,*)
     *                   ' Next file starts before end of previous',
     *                   ' file with the amount:', (file_end_time-
     *                   file_start_time)
               write(27,*)
               write(6,*)'Continue (y/n)'
c              read(5,'(a)') answer
c              if(answer.ne.'y'.and.answer.ne.'Y') stop 
            endif
c
c   check for next file starting after previous file, if only a few samples
c   assume problems with sample rate and continue since this can be fixed later
c
            write(27,*)'file start time',file_start_time
            write(27,*)'file end   time',file_end_time
            write(27,'(a,f20.3)') ' time difference start - end  ',
     *      file_start_time-file_end_time
            time_shift=time_shift+ (file_start_time-file_end_time)
            write(27,*)
            if(file_start_time.gt.file_end_time+err_samp/rate) then
               write(6,*)' Data missing for time interval: ',
     *         file_start_time-file_end_time
               write(6,*)' Will be filled out with zeroes'
               write(27,*)' Data missing for time interval: ',
     *         file_start_time-file_end_time
               write(27,*)' Will be filled out with zeroes'
c
c   find out how many samples are missing, assume the same 
c   sample rate as last file, round off to nearest sample
c
               nmiss=(file_start_time-file_end_time+0.5/rate)*rate
               write(6,*)' Number of samples missing: ',nmiss
               write(27,*) ' Sample rate',rate
               write(27,*)' Number of samples missing: ',nmiss
c
c   add to total number of samples
c
               if(ichan.eq.1) numb_samp_total=numb_samp_total+nmiss
c
c   put in zeroes
c
                  do i=1,nmiss,skip
                     isamp(ichan)=isamp(ichan)+1
                     data_out(ichan,isamp(ichan))=0
                  enddo
            endif
         endif
c
c   remove dc and filter
c
        passes=-1                    ! overlap filter
        if(ifile.eq.1)then 
           dc=0.0
           do i=1,numb_samp
              dc=dc+data(i)
           enddo
           dc=dc/numb_samp
           passes=1      ! first time, reset filter buffers
        endif
c
c   same dc for all files
c
        do i=1,numb_samp
           data(i)=data(i)-dc
        enddo
c
        call recfil(data,numb_samp,data,'BU      ',0.0,0.0,npoles,
     *  'LP      ',0.0,filt,1.0/rate,passes)

c
c  resample
c
         do i=1,numb_samp,skip
           isamp(ichan)=isamp(ichan)+1
           data_out(ichan,isamp(ichan))=data(i)
         enddo
c
         call sei close( close$, unit_wave, code )       ! Close down file
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
      if(nfile.gt.1) then
         rate_out=
     *   (file_start_time-first_file_time)/(numb_samp_total-numb_samp)
         rate_out=1.0/rate_out
         write(6,*) 'Sample rate from header times:', rate_out
         write(27,*) 'Sample rate from header times:', rate_out
         write(6,*)  'Accumulated time shift',time_shift
         rate=rate_out
      endif
c
c  calculate new sample rate
c
      rate=rate/float(skip)
c
c   finish making main header and write it out, assume max 30 channels
c
      total_time=float(isamp(1))/rate       ! assume all channels same isamp
      write(mainhead_out(1)(61:69),'(f9.2)') total_time
      write(mainhead_out(1)(31:33),'(i3)') nchan_out
      do i=3,(nchan_out-1)/3+3
         if(total_time.lt.100000.0) then
            write(mainhead_out(i)(19:26),'(f8.2)') total_time
            write(mainhead_out(i)(45:52),'(f8.2)') total_time
            write(mainhead_out(i)(71:78),'(f8.2)') total_time
         endif
         if(total_time.ge.100000.0) then
            write(mainhead_out(i)(19:26),'(f8.1)') total_time
            write(mainhead_out(i)(45:52),'(f8.1)') total_time
            write(mainhead_out(i)(71:78),'(f8.1)') total_time
         endif
         mainhead_out(i)(6:7)=new_comp
         mainhead_out(i)(32:33)=new_comp
         mainhead_out(i)(58:59)=new_comp
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
      do ichan=1,nchan_out
         chead_out(ichan)(6:7)=new_comp
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

