c
c    
c
c        checks existance of waveform files form nmes in a cat-file
c        chack for existance of calibration files
c        make script file for copying waveform file to current directory
c
c
c   LATEST UPDATE
c
c   march 31 99 by jh : -----------  version 7.0 check --------------
c                       add linux logic
c   jan 2001       jh : more output
c   sep 2008       jh : check for cal files
c   oct 2 2008     jh : change file type to bat for pc
c   jan 25 2011    jh : gfortran: use seiclen to write out cal file name
c   feb 12 2016    jh : optional write and output destination
c   maj 13 2016    pv : added net and loc to find_resp_file
c   jan 25 2017    jh : output of s-fiel with events with missing wav files

c
      implicit none
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! Dimentions
       include 'waveform.inc'              ! waveform handling routines
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c INPUT FILE                                 
      character file1*80
      character*80 destination      ! place to copy to, defult is .
c EVENT TYPE, L,R OR D                     
      character evtype1*1
c RECORDS IN FILE 1                     
      character data1(max_data)*80
      integer errflag
c EXPLOSION INDICATOR                        
      character exp1*1
c NUMBER OF STATIONS                            
      integer nstat1
c NUMBER OF RECORDS and phasesFOR EVENT                 
      integer nrecord1,nphase
c NUMBER OF HEADERS FOR EVENT                   
      integer nhead
      character*80 wave_file
      character*80 text
      integer seiclen
      integer k
c id line number
      integer id
c indicator for compact file
      logical compact
c EVENT COUNTERS          
      integer  nevent
c calibration file name with path
       character*80 cal_file
c  alll cal files
       character*80 cal_file_all(5000)
c  counter for cal files
       integer ncal,ncal_mis
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1
       integer          read01
       integer year,month,day,hour,min
       real sec
c write unit #1
       logical sun,pc,linux            ! computer types 
       integer n_wav_total
       integer n_wav_present
       integer n_wav_none,istart
       integer i,l,m


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c    SET DEFAULTS AND INITIAL PARAMETERS
c
c
      call computer_type(sun,pc,linux)
      nevent = 0
      n_wav_total = 0
      n_wav_present=0
      n_wav_none=0
      call get_seisan_def
      ncal=0
c
c  OPEN FILE
c
      write(6, fmt=*) ' INPUT FILE  NAME'
      read(5, fmt='(A)') file1
c
             call sei open(old$+warn$,            ! Open a existing file.
     &                     ' ',                   ! Prompt file name (n/a).
     &                     file1,                 ! File name
     &                     read01,                ! Read unit #1
     &                     b_old,                 ! Already exists? (n/a).
     &                     code)                  ! Returned condition.
      if (.not. b_old) go to 999
      errflag = 0

      write(6,*)
     *'Where to copy files to, default . on Linux and blank on Windows'
      read(5,'(a)') destination

      if(destination.eq.' ') then
         if(linux.or.sun) destination(1:1)='.'
      endif
 
      open(1,file='get_wav.out',status='unknown')
      if(.not.pc) then
         open(2,file='copy_wav.out',status='unknown')
         open(4,file='copy_cal.out',status='unknown')
      else
         open(2,file='copy_wav.bat',status='unknown')
         open(4,file='copy_cal.bat',status='unknown')
      endif

      open(3,file='copy_wav_missing.out',status='unknown')
      open(7,file='copy_cal_missing.out',status='unknown')
      open(8,file='copy_wav_missing_sfile.out',status='unknown')
c
c   find if full file or compact file
c
      call nortype(read01,compact)
      if(compact) then
        write(6,*)' Input file is compact, no waveform info'
        stop
      endif
c
c   READ  EVENT 
c
    1 continue
      nrecord1 = 1
      call indata(read01, nstat1, nphase,
     *     nhead, nrecord1, evtype1, exp1, data1,id)
c
c   CHECK FOR EOF
c
      if (nrecord1 .eq. 0) goto 2
c
c   check waveform file names
c
      k=0
      do i = 2, nhead
         if(data1(i)(80:80).eq.'6'.and.data1(i)(2:2).ne.' '
     &      .and.data1(i)(2:4).ne.'arc') then
            n_wav_total=n_wav_total+1
            wave_file=' '
            wave_file(1:78)=data1(i)(2:79)
            call  get_full_wav_name(wave_file,text)
            if(text.eq.' ') then
c commented in
               write(6,'(a,a)') 
     *           ' File does not exist: ',wave_file(:seiclen(wave_file))

                 write(8,'(a)') (data1(l),l=1,nrecord1)

                 if(pc) write(3,'(a,a,a)') 'copy ',
     *           wave_file(:seiclen(wave_file)),' xx' 
                 if(sun.or.linux) write(3,'(a,a,a)') 'cp ',
     *           wave_file(:seiclen(wave_file)),' xx'
            else
               n_wav_present=n_wav_present+1
               write(6,'(a,a)') 
     *           ' Full path name    :  ', text(:seiclen(text))
                 if(pc) write(2,'(a,a,1x,a)') 'copy ',
     *              text(:seiclen(text)),
     *              destination(1:seiclen(destination))
                 if(sun.or.linux) write(2,'(a,a,1x,a)') 'cp ',
     *           text(:seiclen(text)),
     *           destination(1:seiclen(destination))
c
c   check for response files
c
                 wav_filename(1)=text(:seiclen(text))
                 call wav_init
                 call read_wav_header(1)  ! get headers
c                 write(6,*)' Number of channels', wav_nchan
                 do l=1,wav_nchan
c                   call find_resp_file(wav_stat(l),
c    *              wav_comp(l),wav_abs_time,cal_file,istart)
                    call find_resp_file(wav_network(l),wav_stat(l),
     *              wav_location(l),wav_comp(l),wav_abs_time,cal_file,
     *              istart)
                    if(cal_file.eq.' ') then
                       call sectim(wav_abs_time(l),year,m,month,
     *                 day,hour,min,sec)
c                      write(7,'(a,2x,a5,1x,a4,2x,i4,4i3,f5.1)')
                      write(7,'(a,2x,a5,1x,a4,2(1x,a2),2x,i4,4i3,f5.1)')
     *                 ' No calibration file for '
     *                 ,wav_stat(l),wav_comp(l)
     *                 ,wav_location(l),wav_network(l),
     *                 year,month,day,hour,min,sec
                       ncal_mis=ncal_mis+1
                    else
                       if(ncal.eq.0) then
                          ncal=1                       ! first file found
                          cal_file_all(ncal)=cal_file  ! save
                       else                            ! check if file saved already
                          do m=1,ncal
                             if(cal_file.eq.cal_file_all(m))
     *                       goto 200                  ! skip, file already there
                          enddo
                          ncal=ncal+1
                          cal_file_all(ncal)=cal_file  ! save
 200                      continue
                       endif 
                    endif                       
c                   write(6,*) cal_file
                 enddo               
               k=k+1
            endif
        endif
      enddo
c
      if(k.eq.0) then
         n_wav_none=n_wav_none+1 
c         write(6,'(a)')' No waveform file names for this event'
      else
          write(1,'(a)') (data1(l),l=1,nrecord1)
      endif
c
c
c   COUNT EVENTS
c
      nevent = nevent + 1
c
c  BACK FOR NEXT EVENT
c
      goto 1

c
c   write summary
c

    2 continue
c
c
c   print out cal files found
c
      do i=1,ncal
        if(pc) write(4,'(a,a)') 'copy ',cal_file_all(i)
     *  (1:seiclen(cal_file_all(i)))                
        if(sun.or.linux) write(4,'(a,a,a)') 'cp ',cal_file_all(i)
     *(1:seiclen(cal_file_all(i))),' .'
      enddo  
c
c   info
c
  
      write(6,*)
      write(6,*)' Total number of events',nevent
      write(6,*)' Number of events without waveform files',n_wav_none
      write(6,*)' Number of waveform files',n_wav_total
      write(6,*)' Number of waveform files present', n_wav_present
      write(6,*)' Number of waveform files missing', 
     *n_wav_total-n_wav_present
      write(6,*)' Number of cal files found', ncal
      write(6,*)' Maximum number of cal files missing ', ncal_mis      
      write(6,*)' Output file with events is get_wav.out'
      if(.not.pc) then
         write(6,*)
     *   ' Output file with waveform file names is copy_wav.out'
         write(6,*)
     *   ' Output file with cal files is copy_cal.out'
      else
         write(6,*)
     *   ' Output file with waveform file names is copy_wav.bat'
         write(6,*)' Output file with cal files is copy_cal.bat'
      endif
      write(6,*)' Output file with waveform file names missing',
     *          ' is copy_wav_missing.out'
      write(6,*)' Output s-file with waveform file names missing',
     *          ' is copy_wav_missing_sfile.out'

      write(6,*)' Output file with missing calibration channels is',
     *          ' copy_cal_missing.out' 
      write(6,*)
      call sei close (close$,read01,code)
999   continue
      stop 
      end
