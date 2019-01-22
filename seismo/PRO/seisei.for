c  
c  program to merge and split seisan waveform files. All valid seisan formats can be used
c  as input, but output will be seisan or mseed only
c
c  j. havskov, december 1991
c  Updates:
c  4.2 -92  c.l.: Fix small bug at OVSICORI
c  28-4-92  j.h.: Automatic event merging within certain time window
c  16-10-92     : Increase array dim to 100000
c  21-10-92     : check if more than 30 chan, if split, also use component
c                 in file name
c  18 8 93  jh  : max duration to 9000 secs, variable max-ev-dif
c  23 8 93      : filename and question to 80
c  jun 94       : 999 channels,bug with last channel, bug
c  aug 2        : in split, use 4 chars stat for file name instead of 3
c  sep 5        : only duplicates of also header time the same
c  sep 23       : bug in split when more than 30 channels
c  oct 5        : bug in merge when 30, 33, 36 etc channels
c  dec 7        : new seidim
C  dec 8        : Installed file & error handling (libsei)
c  dec 13       : ********* version 5.0 *****************
c  dec 28       : bug with backspace
c  jan 4 95     : close files, too many opened
c  jan 30       : bug
c  mar 31, 95,jh: not closing file in split mode, too many open
c  oct 95       : using seisinc routine to read
c  mar 26  96   : if interval > 9999, write with f7.1
c  mar 26  97   : fix probelm of wrong merge, caused by seisinc
c                 not reading header
c  sep 28  97 jh: fix splitting problem of wrong file names
c                 add input from s-file for info on merge
c  mar 9   98   : send message if no merge
c  jun  5 98    : not require blank line at end of filenr.lis
c  jun 18 98  lo: net_code is read from SEISAN.DEF
c-------------------------------------------------------------------------
c  oct 98 jh    : ----------------   version 7.0 check -----------------
c                 year 2000, station names to 5 char, base or agency to 5 chars
c  nov 5 98     : remove computer type
c  nov 26  jh   : enable merge from eev for event in wav base
c  jan 05 99 lo : bug with date and hour in filename
c  jAN 8   JH   : fix compare file names
c  nov 2 99     : some check for null chars
c  dec 6        : close read02 when splitting
c  march 23 00  : add number to file name when splitting for easy sorting
c  june 17 01   : taken out return confirmation, for use with Seisan
c  nov 24 03 jh : changed totoal time window from 99000 to 130000
c  oct 2006  jh : new waveform reading routines, remove option of getting
c                 input from eev, does not seem to be used, can now usee mseed
c  oct 21 2007 : remeove variabels wav_mseed_location and replace 
c                by wav_location, same for network
c                bug fixing
c  aug 1 08 lo : fixed bug, channel number was wrong
c  june 5 09 jh : could not properly merge multi channel files
c  dec 05 11 jh: chad writing, still to be debugged
c  jan 8 12  Jh: fix so no crash if last line in filenr.lis is not blank
c  jan 18 13 jh: put in wav_mem_init
c  jan 29 13 jh: change warning about dup channels a bit
c  nov  5 14 lo: fix to header reading which stopped if large gaps between files
c
      implicit none
c
      include 'seidim.inc'                ! dimensions
      include 'seisan.inc'                ! seisan defintions
      include 'waveform.inc'              ! for common waveform structure

c-- main header
      character*80 mainhead(max_trace)
c-- help variable
      character*80 text
c-- channel header
      character*1040 chahead	  
c-- input file names
      character*80 infile(999) ! increased from 64, lo June 27, 2001
c-- output file name 
      character*80 outfile	  
c-- number of files, one event
      integer ifile	  
c-- input indicator
      integer in 
c-- input question
      character*80 question	  	  
c-- station counter
      integer istat,nstat	  
c-- number of stations and samples
      integer nsamp,nchan	  
c-- merged file network code
      character*5 net_code
c-- number of channels in last read file
      integer nchan_current_file
c-- channel number of first channel in previous read file
      integer k_old
c-- total number of channels up to previous file
      integer nchan_previous_file	  	  
c-- function
      integer sei clen
c-- event start time
      double precision ev_time(max_trace)
c-- max time between event in order to merge the events
      real max_ev_dif
c-- channel duplication indicator
      integer dup(max_trace)
c-- number of duplicate channels
      integer ndup	  	  
c-- maximum and minumum of above and their difference
      double precision first_time,last_time, total_window	  	  
c-- counters
      integer i,j,k,l,ich,khead
c-- selcector for two parts of program
      integer select
c--- output format
      character*6 out_format
c     integer mseed_block                 ! count mseed blocks whrn writing out
c     logical op_file,open_file           ! for opening mseed output  file
      character*3 seed_comp              ! seed component
      integer appendFlag                 ! append flag for mseed write
      integer compFlag                   ! compression flag, 0: steim1, 1 steim2
      integer recSize                    ! record size to write mseed


C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code,                  ! Error encoder.
     &          sei real num               ! Get real number
C
       integer  write01,                   ! Output unit 1.
     &          read01,                    ! Input unit1.
     &          read02,                    ! Ditto 2.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       character*80 chr_file               ! File name
       real sei real num                   ! Real number
C
C    ============= end of list ==========
c


c
c print version
c
      include 'version.inc'
      out_version_date='October 2006'
      if (version_new) out_version_date=version_date
      call print_ver

c	  	  
      merge_wav='MERGE'
      call get_seisan_def
      net_code=merge_wav
      
      max_ev_dif = 180.0	  	  
c
c   question if merge or split
c
      write(6,*)' Merge (1) or split (2) files:'
      read(5,*) select
      if(select.ne.1.and.select.ne.2) then
          write(6,*)' Wrong choice, stop'
          stop
       endif	
c
c   output format
c
 4646 continue   ! from just below
      write(6,*)' Output format, seisan or mseed ?'
      read(5,'(a)') out_format
      if(out_format(1:6).ne.'seisan'.
     *and.out_format(1:5).ne.'mseed') then
         write(6,*)' Wrong format, try again'
         goto 4646
      endif
c
c   go to split if selected
c
      if(select.eq.2) goto 500
c	 		 	  	  	  	  	  	  
      write(6,*)
     *   ' Give 1-5 letter network code for merged file(s),'
     *,' ',merge_wav,' is default'
      read(5,'(a5)') net_code
      if(net_code.eq.'     ') net_code=merge_wav 
  678 continue
      write(6,*) ' Maximum difference (sec) of events to merge,',
     *' return for default (180 secs)'
      read(5,'(a)') text
      if(text(1:4).eq.'    ') then
         max_ev_dif=180.0
      else
         max_ev_dif = sei real num(text,code)
         if(code .ne. e_ok$) then
            write (6,*) ' wrong value'
            go to 678
         end if
      endif

c
c   open file with file names
c	  	  
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'filenr.lis',     ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
         if (code .ne. e_ok$) go to 10
         goto 11
 10      continue
         write(6,*)' No filenr.lis file, make a dirf first'
         stop
 11      continue
 
c 
c----------------------------------------------
c   enter here for new group of events to merge
c----------------------------------------------
c
 1    continue 
c
c   read headers of group of files to merge, first initialize wav arrays
c
      call wav_init
      call wav_mem_init
      wav_out_nchan=0
      nchan_current_file=0
      k=1
      nchan_previous_file=0
c
      ifile=1
 20   continue 		
      read(read01,'(7x,a)', end=50,err=560) infile(ifile)
      goto 561
 560  continue
      stop
 561  continue
c
c-- check if end of group of events to merge	  
c
      if(infile(ifile)(1:4).eq.'    ') goto 50 	  	   	  	  
c
c   now read headers for one event
c	  
 30   continue

      wav_filename(ifile)=infile(ifile)
c
c   read headers for all channels in file
c
c lo 5/11/2014, changed read_wav_header not to stop on large gaps between 
c headers
      call read_wav_header(ifile)
c
c  store number of channels in current file
c
      nchan_current_file=wav_nchan-nchan_previous_file
c      write(6,*) wav_nchan,nchan_previous_file,nchan_current_file
      nchan_previous_file=wav_nchan

c
c  get event abs time and check if within current event merge window
c  the input from filenr.lis must be backspaced to begin next time
c  with correct event
c


c
c  save first channel of previous file
c
          k_old=k
c
c  first channel of current file is k
c
          k=wav_nchan-nchan_current_file+1 
         

c
c   find difference between this and previous file, time from first channel
c
c      write(6,*) ' debug ', k,k_old,wav_abs_time(k),wav_abs_time(k_old),
c     &   wav_abs_time(k_old)-wav_abs_time(k)    
      if(abs(wav_abs_time(k)-wav_abs_time(k_old)).gt.max_ev_dif)
     *  then
        backspace read01
        goto 50
      endif
      wav_out_nchan=wav_nchan ! avoid late channels to be added
c
c-- go to get next event file
c
      ifile=ifile+1
      if (ifile.gt.999) then
        write(*,*)  ' Too many files '
        stop
      endif
      goto 20
c
c------------------------------------------------------------
c   all files headers of files to merge read for one event
c------------------------------------------------------------
c	  	  	 
 50   continue 	 
      write(6,*)
      write(6,*)
      ifile=ifile-1  
      wav_nchan=wav_out_nchan
      if(ifile.eq.0) goto 999
      write(6,*)' Number of files to merge', ifile
c
c   find earliest trace, total time window and delay relative to earliest time
c
      call wav_index_total 	  
c
c   check if total time window is reasonable
c

      if(wav_total_time.gt.130000.0) then
         write(6,*)' Total time window is:',total_window,' secs'
         write(6,*)' This is unrealsistic, event is skipped'
c         write(6,*)' Return to continue		 '
c         write(6,*)
c         read(5,'(a1)') i		 		 
         goto 1
      endif		 		 		 	  	   		 		 	  
c
c   mark identical channels to be removed on merging 
c
      do i=1,wav_nchan
         dup(i)=0
      enddo		 	   
      ndup=0

c      goto 2233

      do i=1,wav_nchan
         do j=i,wav_nchan
c
c  send a message if station and components are the same while header times
c  are different, the two channels might belong to different events
c
          if(wav_stat(i).eq.wav_stat(j).and.wav_comp(i).eq.wav_comp(j).
     *    and.wav_location(i).eq.wav_location(j).
     *    and.i.ne.j.and.wav_abs_time(i).ne.wav_abs_time(j)) then   ! check if duplicate
              write(6,'(1x,a,1x,a4,1x,a4)')
     *        ' Same station, component and location, different time',
     *        wav_stat(i),wav_comp(i)
c              write(6,*)' Return to continue'
c              read(5,'(a)') text
           endif
          if(wav_stat(i).eq.wav_stat(j).and.wav_comp(i).eq.wav_comp(j).
     *    and.wav_location(i).eq.wav_location(j).
     *    and.i.ne.j.and.wav_abs_time(i).eq.wav_abs_time(j)) then   ! check if duplicate
c
c   check that channel has not already been counted as duplicated
c
              if(dup(j).ne.1) then
                 write(6,'(1x,a,1x,a4,1x,a4)')
     *           ' Same station, component, location and time, removed',
     *           wav_stat(i),wav_comp(i)
                 dup(j)=1
                 ndup=ndup+1			  
              endif
           endif
        enddo
      enddo
      write(6,*)wav_nchan, ' Number of input channels'
      if(ndup.gt.0) 
     *write(6,*) ndup,' duplicate channels will not be merged'

c  
c   check if not too many channels, max 999
c
      if(wav_nchan-ndup.gt.999) then
         write(6,*)' Number of output channels is:',wav_nchan-ndup
         write(6,*)' Max allowed is 999, will stop'
         stop
      endif
c
c   remove duplicate channels if any
c
      if(ndup.gt.0) then
         k=0
         do i=1,wav_nchan
           if(dup(i).ne.1) then
              k=k+1
c
c   copy valid channel
c
              call wav_select_sav(k,i)             
           endif
         enddo
c
c  put headers back
c
         wav_nchan=wav_nchan-ndup
         do i=1,wav_nchan
             call wav_copy_wav(i)
         enddo         
c
c  update header info
c
        
         call wav_index_total

      endif
c
c   section for writing out, seisan or mseed format
c
 
c
c   make new main seisan header, clear first, done for both formats to
c   get file name
C
      khead=(wav_nchan-1)/3+3
      if(khead.lt.12) khead=12    ! never less than 12 main headers
      do i=1,khead
         do j=1,80
            mainhead(i)(j:j)=' '
         enddo
      enddo
      call wav_sheads(1,net_code,outfile,mainhead,chahead) 
      if(out_format(1:5).eq.'mseed') outfile(19:19)='M'                                                              
      write(6,200) outfile(1:29)                                             
 200  format(' Output file name is: ',a29)                                
        
c
c   check that outfile name is not the same as one of the input
c   file names in which case the event should not be written out
c
         do i=1,ifile
            if(infile(i)(1:35).eq.outfile(1:35)) then
               write(6,*)'**************************************'
               write(6,*)'Output file already exists, skip merge'
               write(6,*)'**************************************'
               write(6,*)' Enter to continue'
               read(5,'(a)') k
               goto 1      ! try next group
            endif
         enddo
c
c---------------------------
c   seisan format
c---------------------------
c
      if(out_format(1:6).eq.'seisan') then
c      
c   open output file and write main header
c
          chr_f_form$ = 'unformatted'
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
         do i=1,khead
            write(write01)mainhead(i)
            write(6,'(a80)') mainhead(i)		 
         enddo
c
c   channel loop
              
c
c   write output file                       
c
         do j=1,wav_nchan
            call wav_read_channel(j)                            ! read one trace, read as integer, converted to real
            call wav_sheads(j,net_code,outfile,mainhead,chahead)  ! make channel header
            write(write01) chahead                                     ! write channel header
            write(write01)(int(signal1(l)),l=1,wav_nsamp(j))         ! write trace, converted to integer
         enddo
cx   2 byte integers not taken care of	 		 

c
c close
c
         call sei close(close$,write01, code)
      else
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

          do i=1,wav_nchan
c
c   read one channel
c
c
c            write(6,*) 'read chan',i
            call wav_read_channel(i)

           seed_comp(1:2)=wav_comp(i)(1:2)
           seed_comp(3:3)=wav_comp(i)(4:4)
c
c   transefer data to integer array
c
           do l=1,wav_nsamp(i)
             signal_int(l)=signal1(l)
           enddo     
c
c  Append flag        0 - Replace    1 - Append
           if(i.eq.1) then
              appendFlag = 0
           else
              appendFlag = 1
           endif

c
c 
c           write(6,*) 'write chan'
           call seed_channel_write(outfile,
     *     wav_stat(i), seed_comp, wav_network(i), wav_location(i),
     *     wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *     wav_sec(i),wav_rate(i),
     *     wav_nsamp(i),signal_int,
     *     appendFlag,compFlag,recSize,wav_time_error(i))
         enddo
      endif
c
 
       
c
c   back to check if more groups of files to merge 
c	  
      goto 1  		 						   			   
   

c---------------------------------------------------------------------------
c---------------------------------------------------------------------------
c
c Section for splitting a file	  
c
c---------------------------------------------------------------------------
c----------------------------------------------------------------------------
c
 500  continue
c
c   open file 
c
      in=0
      question=' Filename, # or filenr.lis for all'
      call filename(question,infile(1))
      if(infile(1)(1:3).eq.'EOF') stop
      if(infile(1)(1:10).eq.'filenr.lis') then	  	  	  	  
c        write(6,*) 'open'
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'filenr.lis',     ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
      if (code .ne. e_ok$) go to 110
         in=1		 
         goto 111
 110     continue
         write(6,*)' No filenr.lis file, make a dirf first'
         stop
 111     continue
      endif 
c
c   here starts a new event to split if many using filenr.lis
c
c      write(6,*) 'start'
 100  continue 
      if(in.eq.1) then  
         read(read01,'(7x,a)', end=999) infile(1)
         if(infile(1)(1:3).eq.'   ') goto 999
      endif		 
      write(6,'(1x,a)') infile(1)
c
c   now read headers for one event
c
       call wav_init	  
       wav_filename(1)=infile(1)
       call read_wav_header(1)

c
c   loop for writing out single channels
c
      do j=1,wav_nchan

c
c   make new main seisan header, clear first, done for both formats to
c   get file name
c
         do i=1,12
           do k=1,80
              mainhead(i)(k:k)=' '
           enddo
         enddo	
c
c  select channel to output array, index 1
c
         call wav_select_sav(1,j)
c
c   only one channel
c
         wav_sav_nchan=1
c
c   make header
c
         call wav_sav_sheads(1,wav_stat(j),outfile,
     *   mainhead,chahead) 
         if(out_format(1:5).eq.'mseed') outfile(19:19)='M' 	  
c
c
c   add componenets to make filenames different for different channels
c
         outfile(30:30)='_'
c
c   add number to make component sorting easier
c
         outfile(31:33)=wav_comp(j)(1:3)
         outfile(34:34)='0'
         if(wav_comp(j)(4:4).eq.'Z') outfile(34:34)='1'
         if(wav_comp(j)(4:4).eq.'N') outfile(34:34)='2'
         if(wav_comp(j)(4:4).eq.'E') outfile(34:34)='3'
         outfile(35:35)=wav_comp(j)(4:4)
         do i=31,35
            if(outfile(i:i).eq.' '.or.outfile(i:i).eq.char(0)) 
     *      outfile(i:i)='_'
         enddo
c-- check for blanks                                        
         do i=1,29				
            if(outfile(i:i).eq.' '.or.outfile.eq.char(0)) 
     *      outfile(i:i)='0'                            
         enddo                                                                  
         write(6,'(1x,a)') outfile(1:35)
         write(6,'(a80)') mainhead(1)	                                             
         write(6,*)


c---------------------------
c   seisan format
c---------------------------
c
         if(out_format(1:6).eq.'seisan') then
c      
c   open output file and write main header
c
            chr_f_form$ = 'unformatted'
            call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
            do i=1,12
               write(write01)mainhead(i) 
            enddo

c
c   write output file                       
c
            call wav_read_channel(j)                         ! read one trace, read as integer, converted to real
            write(write01) chahead                           ! write channel header
            write(write01)(int(signal1(l)),l=1,wav_nsamp(j)) ! write trace, converted to integer
         
cx   2 byte integers not taken care of	 		 

c
c close
c
            call sei close(close$,write01, code)
         else
c
c-----------------------------------------------------------
c  mseed format
c-----------------------------------------------------------
c

c
c   read one channel
c
            call wav_read_channel(j)
cxx
         write(6,*) 'read channel',j
c  Append flag        0 - Replace    1 - Append
              appendFlag = 0
c
c  Compression flag   0 - STEIM1     1 - STEIM2
c
           compFlag = 0
c
c  Record Size
c
           recSize = 4096

           seed_comp(1:2)=wav_comp(j)(1:2)
           seed_comp(3:3)=wav_comp(j)(4:4)
c
c   transefer data to integer array
c
           do l=1,wav_nsamp(j)
             signal_int(l)=signal1(l)
           enddo
c
c
cxx
           write(6,*)'write channnel'
           call seed_channel_write(outfile,
     *     wav_stat(j), seed_comp, wav_network(j), wav_location(j),
     *     wav_year(j),wav_month(j),wav_day(j),wav_hour(j),wav_min(j),
     *     wav_sec(j),wav_rate(j),
     *     wav_nsamp(j),signal_int,
     *     appendFlag,compFlag,recSize,wav_time_error(j))
        endif
      enddo

c
c   mulitievent mode
c
      if(in.eq.1) goto 100
c
c   single event mode
c	  
      goto 500	  	  	   
c
 999  continue	  
      call sei close(close$+all$,read01, code)       ! Close all open files.
      stop
      end	  	  	 
