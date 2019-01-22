c
c  program to make proper waveform file names from header info
c  all valid seisan format can be used as input
c  output is always mseed format unles only filename is changed
c          in which case original format is keps
c  program can make a time change to all headers
c  program to make  changes to individual channels, only 
c          channels where station and component is given in input
c          definition in wavfix.def, will be used
c          if file contnet is not changed, only rename. but then output file
c             format is the same as input file format
c
c  j. havskov, december 1995
c
c  Updates:
c  aug 2 96 by jh : 2 byte data were decleared as 4 bytes !!!
c--------------------------------------------------------------------
c  oct 98 by jh   : --------------    version 7.0 check ------------
c                   remove pc names, year 2000, 5 char agency
c  nov 5          : linux logic, system_c to systemc
c  feb 5          : include channel name change
c  june 11   lo   : use also Nordic file as input and wavfix.def
c  sep 9     jh   : make shure net code is not blank in output name
c  jan 10,2003 lo : apply individual channel time correction given in wavfix.tim
c  oct 20      jh : make it possible ot hardwire sample rate changes, look 
c                   for new_rate
c  feb 10 2004 lo : fix bug with net_code
c  may 12 2004 jh : possibel to change polarity by changing gain constat
c  mar 30 2006 lo : use converted station/component codes in output filenames
c  mar  5 2007 lo : add option to mark uncertain time, and command line options
c  jul  7 2011 lo : fix for gfortran on reading time correction
c  oct 25 2012 jh : fix dimension in def channels, 200 to 2000
c  may  5 2014 jh : take variabels out of signax block
c  2015-06-02  pv : add signal_int to signalx common block due to
c                   compiler warning
c  mar 22 2016 jh : change to work with all input formats, output is only mseed
c                   polarity change did not seem to work in either options, fixed
c                   will only work with one waveform file in s-file as before, could
c                   be changed, some cleanup
c
c           options tested when changing wavfix to use wav structure
c
c           input seisan  ok
c           input mseed   ok
c           pol change one chan: ok
c           pol change all chan:  ok
c           stat comp change:  ok
c           time change one chan, input mseed or seisan: ok 
c           time change and file name change, same for all channels: ok
c           time change and always usw first arriving channel for file name: ok
c           time interval for time change: not tested
c           rename only: ok
c           overwrite: ok
c           input nordic file: ok
c           input from command line: not tested
c           output nordic file and correct wav file name: ok
c           one component file name, add component to file name: ok
c           overwrite same file by temporary rename: ok
c           change sample rate: not tested
c           many files with filenr.lis: ok
c           uncertain time: not tested since apparently it does not work with mseed
c
      implicit none
c
c   general dimensions
c
      include 'seidim.inc'                    ! for giving dimensions
      include 'libsei.inc'                    ! for all seisan subroutines
      include 'waveform.inc'                  ! waveform data
      include 'seisan.inc'                    ! seisan general parameters


c-- input file name
      character*80 infile
c-- output file name 
      character*80 outfile,outfile1
c -- for channel redefination
      character*5 stat_in
      character*4 comp_in
      logical change
      logical chg_pol(max_trace)   ! true if channel pol changed
      real new_rate(max_trace)     ! new sample rate
      double precision ind_time_cor(max_trace) ! individual time correction
      character*80 text      ! general text string
      character*1 answer
      character*1 polarity_change    ! if yes, change polarity
      character*1 uncertain_time_change ! if yes mark time as uncertain
      logical exist          ! for file open
      logical same_file      ! if same name of in and output file
c-- merged file network code
      character*5 net_code,net_code_out
c-- counters, etc
      integer i,k,l,j
      real sec, sec1
      double precision time_cor           ! time correction in seconds
      double precision msec               ! time correction in seconds
      character*1 replace_filename
      real gain                           ! for polarity change
c-- computer type
      logical sun,pc,linux
c
c   chan def block
c
      character*5 def_in_stat(2000)  ! input station code in def file
      character*4 def_in_comp(2000)  ! input component code in def file
      character*5 def_out_stat(2000) ! output station code in def file
      character*4 def_out_comp(2000) ! output component code in def file
      integer   def_chan_number(2000)! channel number of current unit
      integer ndef_chan              ! number of channels defined
      integer appendFlag                 ! append flag for mseed write
      integer compFlag                   ! compression flag, 0: steim1, 1 steim2
      integer recSize                    ! record size to write mseed
      character*3 seed_comp              ! seed component


      common /def_chan/def_in_stat,def_in_comp,def_out_stat,
     *                 def_out_comp,def_chan_number,ndef_chan
c
c	  
C    Seisan library inserts and routines...
C    ======================================
C

       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code,                  ! Error encoder.
     &          sei real num               ! Get real number
       integer  seiclen
C
       integer  
     &          read01,                    ! Input unit1.
     &          read02,                    ! Ditto 2.
     &          read03,
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       character*80 chr_file               ! File name
       real sei real num                   ! Real number
C
C    ============= end of list ==========

c-- name of sfile
      character*40 nordic_file
      character*29 mainhead_text
c output file unit 
      integer write01
      character*1 choice
      character*80 line
c question
      character*80 question
c 1 if filenr.lis
      integer in
c number of components defined in wavfix.tim
      integer ntime
      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
    
      replace_filename='N'
      net_code=' '
      polarity_change='n'
      uncertain_time_change='n'
      ntime=0
      time_cor=0.
      in=0
      call computer_type(sun,pc,linux)

c
c read command line options
c

c
c   get arguments
c
      call get_arguments(narg,arg)
      if (narg.ge.1) then
        do i=1,narg
          if (arg(i)(1:15).eq.'-uncertain_time') then
            uncertain_time_change='y'
          elseif (arg(i)(1:9).eq.'-polarity') then
            polarity_change='y'
          elseif (arg(i)(1:7).eq.'-infile') then
            infile=arg(i+1)
          elseif (arg(i)(1:10).eq.'-overwrite') then
            replace_filename='A'
          elseif (arg(i)(1:16).eq.'-time_correction') then
            call sei get values(1,arg(i+1), code )
            time_cor=array$(1)
          elseif (arg(i)(1:5).eq.'-help') then
            write(6,'(a)') 'wavfix' 
            write(6,'(a)') 'usage: wavfix -infile <infile> ' 
     &        //'[-uncertain_time -polarity] '
     &        //'[-time_correction <x in seconds> ]'
            write(6,'(a)') '-uncertain_time   add uncertain time flag '
     &                   //'to all channel headers'
            write(6,'(a)') '-polarity         change polarity'    
            write(6,'(a)') '-time_correction  apply time correction '
     &        //'of x seconds'
            stop
          endif
        enddo
        if (seiclen(infile).le.0) then
          write(*,*) 'give input file' 
          stop
        endif
        goto 30
      endif
        

c
c   hardwire possible sample rate changes, normally disabled by being negative
c
      do i=1,max_trace
         new_rate(i)=-1.0
      enddo


c
c   get def file for station codes, give file name
c
      text='wavfix.def'

      call read_def_chan(text,mainhead_text,net_code)

      text='wavfix.tim'                                      

      call read_def_time(text,ntime)                         

      write(6,*)' This program can change header times in all headers'
      write(6,*)' with the same amount. The waveform file name will be'
      write(6,*)' changed at the same time and adjusted to the standard'
      write(6,*)' name.'
      write(6,*)' If no time correction is given, only the waveform'
      write(6,*)' names are adjusted.'
      write(6,*)' Optionally polarty can be changed for channels',
     *          ' listed in wavfix.def'
      write(6,*)' In addition, channels names can be changed if a'
      write(6,*)' wavfix.def file is available'
c
c   no individual time corrections so possible to change all with the same amount
c
      if (ntime.eq.0) then
        write(6,*)
     &    ' Time correction in seconds, u for uncertain time, '
     &    //' return for no correction'

        read(5,'(a)') text
        if (text(1:1).eq.'u'.or.text(1:1).eq.'U') then
          uncertain_time_change='y'
          time_cor=0.
        else
          if (seiclen(text).eq.0) text='0.'  ! lo 07/07/2011
          time_cor=sei real num(text,code)
        endif
      else
        write(6,*) 
     &    ' Time correction applied from file wavfix.tim '
      endif

      write(6,*)' Polarity change(y/n=default)'
      polarity_change='n'
      read(5,'(a)') polarity_change
      if(polarity_change.eq.'y') then
          write(6,*)' Sure you want to change polarity (y/n) ?'
          read(5,'(a)') answer
          if(answer.ne.'y') polarity_change='n'
      endif
      write(6,*)

220   continue

      write(*,*) ' Input options: (1) filenr.lis or wavfeform file name'
      write(*,*) '                (2) Nordic file'
      read(5,'(a1)') choice
      if (choice.ne.'1'.and.choice.ne.'2')goto 220

      if (choice.eq.'2') then
        write(6,*) ' Name of Nordic file'
        read(5,'(a40)') nordic_file
        if (nordic_file.eq.'nordic.fix') then
           write(*,*) ' Please rename your input file.'
           stop
        endif
      endif

225   continue

      if (choice.eq.'1') then
        question=' Filename or number, filenr.lis for all'
        call filename(question,infile)

        if (infile(1:3).eq.'EOF') stop
      endif

c
c   open file with file names
c	  	  
      if (choice.eq.'1') then
        if(infile(1:10).eq.'filenr.lis') then
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'filenr.lis',     ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
          in=1
        endif
        if (code .ne. e_ok$) then      
          write(6,*)' No filenr.lis file, make a dirf first'
          stop
        endif

      elseif (choice.eq.'2') then
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   nordic_file,      ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
        if (code .ne. e_ok$) then         
            write(6,*) 'file does not exist'
            stop
        endif
c
c open output nordic file
c
        call sei open( unknown$+warn$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'nordic.fix',     ! Filename.
     &                   write01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )
      endif


11     continue
c 
c----------------------------------------------
c   enter here for new event to name or fix
c----------------------------------------------
c
 1    continue 

      if (in.eq.1) then
        read(read01,'(7x,a)', end=999) infile
c-- check if end of group of events to name	  
        if(infile(1:4).eq.'    ') goto 999

c
c  read from Nordic file
c
      elseif (choice.eq.'2') then
        read(read01,'(a80)',end=999) line

        if(line(80:80).eq.'6') then
          infile=' '
          read(line(2:79),'(a78)') infile

c
c  check if file exists
c
          inquire(file=infile,exist=exist)
          if (.not.exist) then
               write(write01,'(a80)') line
               write(*,*) 'file does not exist: ',infile
               if (in.eq.1.or.choice.eq.'2') goto 1
               if (choice.eq.'1') goto 225
          endif
        else
          write(write01,'(a80)') line
          if (in.eq.1.or.choice.eq.'2') goto 1
          if (choice.eq.'1') goto 225
        endif
      endif

c
c   now read header for one event
c	  
 30   continue

c
c  initialize 
c
      call wav_init                   
      call wav_mem_init               

      net_code_out=net_code

      do i=1,5
        if (net_code_out(i:i).eq.' ') net_code_out(i:i)='_'
      enddo

      do i=1,max_trace
        ind_time_cor(i)=0.
      enddo

      wav_filename(1)=infile           ! assume only one file for now
c
c   read all headers of file 1
c
      call read_wav_header(1)           
c
c   output possible errors
c
      if(wav_error_message.ne.' ') then
         write(6,'(1x,a)') wav_error_message
c
c   next event
c    
         if (in.eq.1.or.choice.eq.'2') goto 1
         if (choice.eq.'1') goto 225
      endif
c
c   save stat-comp etc, could be changed
c
      do i=1,wav_nchan
          call wav_copy_sav(i)
      enddo
                              
c
c   write out the format of input file
c
      write(6,'(1x,a,a)') 'Format ',wav_file_format(1)        
c
c   individual channel time corrections indicated by ntime not zero
c
      if (ntime.ne.0) then
c
c   find correction for each channel and check time interval for 
c   correction
c
           do i=1,wav_nchan
             if (seiclen(wav_stat(i)).gt.0) then
               call set_time_chan(
     &         wav_stat(i),wav_comp(i),
     *         wav_abs_time(i),ind_time_cor(i))
               write(*,*) 
     *         ' time correction for '//wav_stat(i)//wav_comp(i)//
     &         ': ',ind_time_cor(i)
             endif
           enddo
      else
c
c   same corrections for all
c        
            do i=1,wav_nchan
               ind_time_cor(i)=time_cor
            enddo
      endif

c
c  do the time corection, save in SAV array to leave original unchanged,
c  needed for reading seed data
c
      do i=1,wav_nchan
            call timadd(wav_year(i),wav_month(i),wav_day(i),             
     *      wav_hour(i),wav_min(i),wav_sec(i),ind_time_cor(i),           
     *      wav_sav_year(i),wav_sav_month(i),wav_sav_day(i),
     *      wav_sav_hour(i),wav_sav_min(i),wav_sav_sec(i))

            wav_sav_abs_time(i)=wav_abs_time(i)+ind_time_cor(i)              
      enddo
c
c   find new first channel
c
      call wav_sav_index_total
c                                                                               
c   make output file name, use first arriving channel for name 
c

c
c   case of time correction and how to handle file name
c             
      if ((ntime.ne.0.or.time_cor.ne.0.).and.
     &    (replace_filename.ne.'A'.and.replace_filename.ne.'E')
     &    .and.outfile(21:25).ne.net_code_out) then
           write(*,*) ' Keep input file name as output file name '//
     &     ', (y)es,(n)o, (a)lways or n(e)ver ? '
           replace_filename=' '
           read(5,'(a)') replace_filename
           if (replace_filename.eq.'y') replace_filename='Y'
           if (replace_filename.eq.'n') replace_filename='N'
           if (replace_filename.eq.'e') replace_filename='E'
           if (replace_filename.eq.'a') replace_filename='A'
      endif

      outfile=' '

      if (replace_filename.ne.'N'.and.replace_filename.ne.'E') then
        outfile=infile
      else
         
c-- year                          
         write(outfile(1:4),'(i4)') wav_sav_year(wav_sav_first)            
         outfile(5:5)='-'               
c-- month                                    
         write(outfile(6:7),'(i2)') wav_sav_month(wav_sav_first)           
         if(outfile(6:6).eq.' ') outfile(6:6)='0'                  		
         outfile(8:8)='-'
c-- day                                      
         write(outfile(9:10),'(i2)') wav_sav_day(wav_sav_first)            
         if(outfile(9:9).eq.' ') outfile(9:9)='0'                  		
         outfile(11:11)='-'
c-- hr                                       
         write(outfile(12:13),'(i2)') wav_sav_hour(wav_sav_first)          
         if(outfile(12:12).eq.' ') outfile(12:12)='0'              		
c-- min                                    
         write(outfile(14:15),'(i2)') wav_sav_min(wav_sav_first)           
         if(outfile(14:14).eq.' ') outfile(14:14)='0'              		
         outfile(16:16)='-'
c-- sec                                    
         i=wav_sav_sec(wav_sav_first)                                      
         write(outfile(17:18),'(i2)') i                            
         if(outfile(17:17).eq.' ') outfile(17:17)='0'              		
         outfile(19:20)='M.'                                       
c
c  convert station and channel for first channel and use for filename
c  if not taken from def file
c  lot 07/03/2006
c
         stat_in = wav_stat(1)         
         comp_in = wav_comp(1)         

         call set_def_chan(1,wav_stat(1),wav_comp(1))         
c
c-- network or station code, if not defined used from input file name, else 
c   first station
c                  
          if(seiclen(net_code_out).le.0.or.net_code_out.eq.'_____') then
            if(infile(19:20).eq.'M.'.and.wav_nchan.gt.1) then         
              net_code_out=infile(21:25)
            else
              net_code_out=wav_stat(1)         
            endif
         endif	
         outfile(21:25)=net_code_out
         outfile(26:26)='_'
c-- number of channels
         write(outfile(27:29),'(i3)') wav_nchan         
c-- check for blanks                                        
         do i=1,18				
            if(outfile(i:i).eq.' ') outfile(i:i)='0'                            
         enddo                                                                  
         do i=19,26
            if(outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         do i=27,29
            if(outfile(i:i).eq.' ') outfile(i:i)='0'
         enddo
c-- add component code if only one channel
         if (wav_nchan.eq.1) then         
           outfile(31:34)=wav_comp(1)        
           do i=30,seiclen(outfile)
              if(outfile(i:i).eq.' ') outfile(i:i)='_'
           enddo
         endif
       endif
c
c restore station and component codes
c
       wav_stat(1)= stat_in         
       wav_comp(1)= comp_in         

         write(6,200) outfile(1:seiclen(outfile))                                             
 200     format(' Output file name ',a)                                
         write(6,*)

c 
c check if input name same as output name
c
      same_file=.false.
c      write(*,*) infile
c      write(*,*) outfile

      if (outfile(1:seiclen(outfile)).eq.
     &     infile(1:seiclen(outfile))) same_file=.true.
c
c   check that outfile name is not the same as an input
c   file names in which case the event should not be renamed if
c   there is no internal changes
c
      call sei open( check$,                  ! Try to open & warn.
     &               ' ',                     ! No prompt.
     &             outfile,                   ! This file.
     &               read03,                  ! On unit.
     &               exist,                   ! File exists?.
     &               code )                   ! Condition.

      if (exist.and..not.same_file) then
            write(6,*)' Name already exists, overwrite (y/n)'
            read(5,'(a)') answer
            if(answer.ne.'y'.and.answer.ne.'Y') then
               call write_out(write01,choice,line,outfile)  ! nordic file write
               if (in.eq.1.or.choice.eq.'2') goto 1
               if (choice.eq.'1') goto 225
            endif        
c
c  if a time correction, or channel name change
c  the output file must have a different name
c  temporarely, to be able to read the old file with the same name
c

         if(time_cor.ne.0.0) then
           same_file=.true.
         endif
      endif

c
c  change station and component code, put new codes in saved array since
c  original array is needed if seed read
c
      change = .false.

      do k=1,wav_nchan          
        chg_pol(k)=.false.
c
c   save original codes
c
        stat_in = wav_stat(k)         
        comp_in = wav_comp(k)         
c
c   polarity should be changed if input and def stat and comp input and
c   output are is the same
c
        do i=1,ndef_chan
           if(stat_in.eq.def_in_stat(i).
     *     and.comp_in.eq.def_in_comp(i).and.
     *     stat_in.eq.def_out_stat(i).and.comp_in.eq.def_out_comp(i))
     *     then 
             chg_pol(k)=.true.
           endif
        enddo
c
c   here change codes in saved array
c
        call set_def_chan(k,wav_sav_stat(k),wav_sav_comp(k))         

        if (.not.change) then
          if (wav_sav_comp(k).ne.comp_in) change=.true.          
          if (wav_sav_stat(k).ne.stat_in) change=.true.          
        endif
c
c   check for time change
c
        if (ind_time_cor(k).ne.0.) change=.true.
      enddo

      if (uncertain_time_change.eq.'y') change=.true.

c
c   rename only since if no change in file content
c
      if(time_cor.eq.0..and.(.not.change).and.
     *   polarity_change.ne.'y'.and.
     *   uncertain_time_change.ne.'y') then
         k=seiclen(infile)+seiclen(outfile)
 
         write(6,*)'Rename only' 

         if(sun.or.linux) 
     *   call systemc('mv '//infile(1:seiclen(infile))//
     *   ' '//outfile(1:seiclen(outfile)),k+4)
         if(pc) call systemc('rename '//infile(1:seiclen(infile))//
     *   ' '//outfile(1:seiclen(outfile)),k+8)

      else
c
c   correct all headers and rewrite file
c
         outfile1=outfile     ! save outfile name in case of temporary file, new
         if(same_file) then   ! make a temporary file
            write(*,*) ' New file same as old, creating wavfix.tmp '
            outfile='wavfix.tmp'                  
         endif  
c
c   read each channel, fix  polarity and rewrite
c

c
c-----------------------------------------------------------
c  mseed format
c-----------------------------------------------------------
c
c
c  Compression flag   0 - STEIM1     1 - STEIM2
c 
           compFlag = 0
c
c  Record Size
c
           recSize = 4096

c
c   read and write each channel
c
          do i=1,wav_nchan
c
c   read one channel
c
c            write(6,*) 'read chan',i,' ',wav_filename(1)

            call wav_read_channel(i)

c
c   possibly correct sample rate, hardwired in program
c
            if(new_rate(i).gt.0.0) wav_rate(i)=new_rate(i) 
              
c
c   put in possibly new channel names, could be the same if polarity fix
c

c
c   use SAV array with corrected values for write out
c           
           call wav_copy_wav(i)
         
           seed_comp(1:2)=wav_comp(i)(1:2)                               
           seed_comp(3:3)=wav_comp(i)(4:4)                               
c
c   transefer data to integer array, optionally change polarity 
c
            gain=1.0
            if(polarity_change.eq.'y'.or.chg_pol(i)) gain=-1.0            

            do l=1,wav_nsamp(i)                                           
              signal_int(l)=gain*signal1(l)                               
            enddo                                                         
c
c  Append flag        0 - Replace    1 - Append
c
           if(i.eq.1) then
              appendFlag = 0
           else
              appendFlag = 1
           endif

           call seed_channel_write(outfile,
     *     wav_stat(i), seed_comp, wav_network(i), wav_location(i),
     *     wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *     wav_sec(i),wav_rate(i),
     *     wav_nsamp(i),signal_int,
     *     appendFlag,compFlag,recSize,wav_time_error(i))
         enddo  
c
c   rename temporary file if used
c
         if(same_file) then
c
            write(6,*) 'rename back'
            k=seiclen('wavfix.tmp')+seiclen(outfile1)             
            if(sun.or.linux) call systemc('mv '//'wavfix.tmp'//        
     *      ' '//outfile1(1:seiclen(outfile1)),k+4)           
            if(pc) then
               call systemc('del '//outfile1(1:seiclen(outfile1)),        
     *         seiclen(outfile1)+4)                
               call systemc('rename '//'wavfix.tmp'//
     *         ' '//outfile1(1:seiclen(outfile1)),k+8)        
            endif
         endif
      endif      ! end of of changes for one file      
c
c   get next file
c
      call write_out(write01,choice,line,outfile)
      if (in.eq.1.or.choice.eq.'2') goto 1
      if (choice.eq.'1') goto 225
c
 999  continue	  

      if (choice.eq.'2') then
         write(*,*) 'Nordic output file: nordic.fix'
         call sei close(close$,write01,code)
      endif
      stop
      end	  	  	 

      subroutine write_out(write01,choice,line,outfile)
c 
c write to nordic file
c
      implicit none
      integer write01,seiclen
      character*80 line,outfile
      character*1 choice

      if (choice.eq.'2') then
        line(1:40)='                                        '
        line(41:80)='                                       6'
        line(2:1+seiclen(outfile))=outfile(1:seiclen(outfile))
        write(write01,'(a80)') line
      endif

      return
      end

      
