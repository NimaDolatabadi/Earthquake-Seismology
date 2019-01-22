ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   subroutines for handling waveform data
c  
c   some principles:
c   - all channels from all files are indexed sequentially
c   - the index is used in all operations in all variables, also
c     for selected variables (wav_out ...)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   look for word problem for petentially unsolved problems
c
c   changes:
c   
c   may 24 2000 lo: bug fix in wav_read_channel
c   oct  3 2000 jh: allow a time window of 300 000 secs, from 90 000
c   oct  9 2000 lo: check for channel in file for GSE , wav_read_channel
c   oct  5 2001 jh: add routine wav_out_sheads, change wav_read_channel
c                   to wav_read_channel_one, put in new wav_read_cahnnel
c                   to be used with both one and many files (cont data)
c   nov  11     jh: bugs in cont part when many waveform files in cont base
c                   initilize wav_header_text
c   nov 21 2001 lo: in gse read, also check for aux station code
c   dec 7  2001 bjb add if statement to allow progs to read BGS VAX wave files
c                   that do not support the century 
c   jun 30 2003 lo: read 3 comp seisan comp codes as 4 chars
c   sep 29 2003 ct: add "form='unformatted'" to 3 open calls for SeisAn data
c   oct-nov2004 jh: seed reading
c   april 8, check for timegap in seed reading, fill with zero
c            remove temporary xx array in seed reading
c   june 7   a few check if too many samples
c   july 5 2005 lo: check if sac binary before seed, example of sac file that
c                   was thought to be miniseed
c   oct 19, 2005 jh:fix so many seed files can be read form one s-file, has to
c                   read contents of each file again before reading channel
c   nov 09 2005 jh :fix time gap problems
c   nov 23 2005 jh :message about cont_before
c   jan 06 2005 jh : bug in time gap and seed
c   oct 3  2005 jh : check of which seed file to index, was not set up
c                     correctly
c   jan 8  2006 lo : implemented sacsubf rading routines
c                    check if seed file before sac
c   jan 17 2007 jh : fix bug with time gaps in seed file. output information
c                    was not updated if a gap was found. now channel infor is
c                    ok, but initial reading of headers without reading
c                    traaces is still wrong
c   june   2007 jh : add wav_select_sav, add wav_seed_location and 
c                    wav_seed_network
c   sep 6  2007 lo : check wrecord is not what it would be if mseed data
c                    before check if data is sac binary
c   oct    2007 Jh : memeory handling
c   oct  19        : start fixin location codes etc
c   nov 5          : fixed bug in memory handling when time gaps in mseed
c   nov 10         : more bugs with cont
c   dec 19 2007 jh : comment out update of wav_out_nsamp and duration,
c                    seems to be a msitake
c       20         : do not use memory handling with large seed files
c   april 15 07 jh : add 0.4 sample time to output duration when calculating
c                    number of samples out to to avoid round off error, 
c                    variabel wav_nsmap_out
c   may 5 07    jh: probelm with reading cont data when mixing types of
c                   files (seisan+seed), cause by a wrong memery store when
c                   reading seed, and missing update of wav_nsamp
c   august 6    jh: seems an old call to wav_copy_sav had been uncommented
c                   in seed reading section
c                   wav_duration set to nsamp/rate, was (nsamp-1)/rate
c                   output from cont set start at or after start time and
c                        number of samples = interval * rate, 
c                        was interval * rate + 1 
c   august 12   jh: Ensure correct format when reading cont data
c   aug 27      JH: more fixing of duration problem, more comments
c   sep 17 08   jh: more output if a gap
c   sep 25 08   JH: comments
c   oct 7  08   jh: avoid crash if file empty
c   oct 23 08   jh: do not write out 'seed file indexed'
c   nov 12 08   jh: make sure all channels with same station and component
c                   but different network or location codes are counted 
c                   when reading cont data
c   feb 03 09   lo: small change in check for miniseed file
c   aug 20 09   lo: dont read location if files from Earthworm with 
c                   component code 1-3 in seisan file, rather than 1-2,4
c   2009-12-18  pv: orientation code 1 and 2 are read as N and E, respectively.
c   2010 03 22  lo: changed call to sac bin reading
c   2010 03 30  jh: check for sac bin before seed
c   2010 04 08  jh: again check sacbin after seed, some seed files look like
c                   sacbin. now check seed for sequence number
c   2010 04 08  jh: again check sacbin after seed, soem seed files look loke
c                   sacbin. now check seed for sequence number
c   2010 10 14  jh: bud reading
c   2010 11 16  jh: arc instead of bud in call to archive, add arc_type to
c                   to indicate bud or seiscomp
c   2010 12 21  jh: bug when going to next window in archive
c                   bug of starting on minute
c   2011 01 10  jh: better detection of miniseed files, now check if each
C                   of the 6 first bytes can be read as integer
c   2011 01 12  lo: use new function to detect sac binary file
c   2011 04 05  jh: arch_type was wrong in one place
c   2011 06     Jh: new chad reading routines
c   2011 06 17  pv: nasty f95 fix for sun, for reading EOF in last record
c                   in Seisan wav files
c   2011 10 20  jh: fixed seed reading in cont
c   2011 11 03  jh: removed multiple entry of identical waveform files 
c                   in s-file for cont reading
c   2011 12 13  jh: guralp reading, one channel only
c   2011 12 19  jh: helmberger format, turn off error message when checking
c                   for sacac
c   2012 12 03  jh: identify guralp before sacbin, soem guralp files
c                   were identified as sacbin.
c                   make sure current cont base is searched for first,
c                   change in cwav_read_header.
c                   put '_' in base name in cwav_read_bases.
c   2013 01 04   jh: nrecord to auto_tr, remove reference to SCP and BUD, only ARC
c   2013 01 15   jh: check for valid arc channel in time
c   2013 01 29   JH: in the wav_sav etc routines, filename and references 
c                    were missing
c   2013 02 05   jh: get rid of  a warning
c   2013 03 04   jh: miniseed can now also be identified by character 'M',
c                    seen in a file from IRIS
c   2013 03 06   jh: add routine write_chan_helm_out
c   2013  5 14   lo: check that wav_mem_next_position is not 0
c   2014 11  5   lo: in header reading do not stop if big gap between headers
c                    needed by seisei with big time gap between files
c   2015 03 18   pv: reading of mseed reals signal_int replaced by signal1, 
c                    see seed_channel_read. 
c   2015 03 24   jh: in wav_find_chan, also find if input only is e.g. 'S  Z'
c                    and '   Z' 
c   2015 06 04   lo: in read_2channel take first possible channel,
c                    makes difference when same data there twice
c   2015 10 23   pv: fix bug while reading arc data, gaps was filled with
c                    data from previous channel
c   2016 03 23   jh: add subroutine wav_sav_index_total
c   2016 05 08   jh: add fix to read seed on w10 systems, just close and open 
c                    file before indexing
c   2016 05 23   pv: made the w10 fix above, a pc only check. It made sun crash.
c   2016 08 23   jh: fix so 1 and 2 become N and E ONLY whne three
c                    component data is used, look for 'jh component fix'.
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   definition of interval or duration
c
c   there has been some confusion in SEISAN about this definition which
c   has lead one too few or one too many samples. From August, 2008, we try to
c   follow the following principle in all operations:
c
c   duration  = number of samples * sample rate
c
c   this means that the duration is defined as the time from the first sample to
c   the last sample plus one sample interval. this might seem wrong for single files
c   where the time difference between the first and last sample is (number of samples-1) 
c   times the sample rate. however if two following segments are put together, there
c   is the extra samle interval of time between them so the data in one segment
c   really reprent the time inval as now defined in SEISAN.so using this definition  is
c   particular impoertant when dealing with continuouis data.





c
c   Continuous data
c
c   Continuous data is a data set of many traces and several segments
c   of traces. This means that an event (collection of files) from
c   the continuous data base is like a normal event with one dimension
c   added for the file structure. Reading of continuous data will, for
c   the user, appear almost as reading a 'normal event'. The only
c   added input information needed is the list of data bases used
c   and the time window. From there on, the same calls, as for one
c   dimensionsl events' is used and the data ends up in the same
c   variables.
c
c   assumptions: It is assumed that there is only one waveform file
c   in S-file. The system could be programmed to use several waveform
c   files even of different format, but there has so far been no need.
c
c   If 2 files in a continuous stream are less than 2 s apart (last sample in
c   first and first sample in next), it is assumed that the data is continuous
c   and no samples will be put in for the time gap. larger time gaps are replaced
c   by the DC value.
c
c   Continuous event consisting of 4 traces with repectively 5, 8, 5
c   and 9 files (segments) each:
c
c   _______ ________ _______ _______ _______
c     ____ ____ ____ ____ ____ ____ ____ ____
c    ________ ________ ________ ________ ________
c       ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
c   
c   
c   
c   One dimensional file (normal) with 4 traces:
c   
c   _______________
c    ________________
c       _________________
c     ____________
c   

c   Sequence to read continuous data:
c
c   - Get data bases to use, if not specified by users, read from
c     SEISAN.DEF
c   - Get time interval, must be shorter than corresponding to
c     usual dimension of SEISAN
c   - Read all header info for whole data set to cwav and place
c     info, for combined data set in normal wav-common block
c   - Read any one channel of data with normal reading routines
c
c   EXAMPLE
c
c   tell all routnes that this is reading from cont data base
c
c      cwav=.true.
c
c   set interval in secs
c
c      cont_interval=1800.0  ! common block variable
c
c   set start time
c
c      cwav_start_time = '200101101000'  ! common block variable
c
c   calculate end time
c
c      call cwav_time_limits(0)
c
c  read the header information for all files in all bases in time
c  interval, assume info available in common block
c
c      call cwav_read_bases
c
c  the normal SEISAN main head in not available since all data is now in
c  the standard waveform common block. if needed (e.g. for output) it
c  can be generated by following command
c
c      call wav_sheads(1,net_code,outfile,mainhead,chead)
c
c
c  read the waveform data, one trace at a time, note normal call is used
c
c      do i=1,n_cont_trace         ! n_cont_traces are available traces
c          call wav_read_channel(i)  - variable from common block
c          call user routine
c      enddo
c
c--------------------------------------------------------------------------
c   large seed file
c   variable cseed must bet set to true, however in these routines it is only
c   used to avoid memeoryt buffereing for large seed files. in calling program,
c   the start and stop block to read must be called before reading seed dara here.
c
c--------------------------------------------------------------------------
c
c   read archive data as an event from eev
c
c   the referenc ein s-file is treated as if it was a file. it is read as a
c   file using the same variabels as the files. when opening and the keyword
c   for archive ARC is found, the read is directed to the archive
c   instead of to the file. for memory store there is no change since the
c   archive request is treated as a file name (archive reference) e.g.
c   ARC STAT  COM NT LO YYYY MMDD HHMM SS   DUR
c   ARC ROSA  BHZ PM    2010 1011 0100 00 14400
c   thus the segment in archive with given start time and duration is  
c   considred a file. if later plots require less data than the segment
c   referenced, the whole segment is still read, like reading the whole
c   trace in a fil in archive with given start time and duration is  
c   considred a file. if later plots require less data than the segment
c   referenced, the whole segment is still read, like reading the whole
c   trace in a file.
c
c   memory buffering
c   
c   when a channel is read, the waveform data is stored in a buffer, a
c   one dimensional array, wav_mem_signal. the first channel read is
c   stored in the beginning, the next one followin etc. when the buffer is 
c   full, it starts from the beginning. signals are not broken up at end
c   of buffer so first sample in a trace  is also a first sample
c   in memory buffer. for each trace stored, the correspondign filename,
c   wav,mem_filename, number of samples, wav_mem_nsamp and channel number
c   in original file, wav_mem_chan_number are stored in arrays. The index
c   in these arrays increase until memory buffer is full. after that there
c   is a check if an index is no longer used (number of samples negative)
c   and if not used it is resused. the data traces are 'remembered' for as 
c   long as program is active so e.g. in mulplt cont mode, the data read in
c   in previous windows will be remembered, if there is space in the 
c   buffer.
c   when the waveform reading routine starts, it will first chek if data
c   already is in memory by cheking if the desired filename and channel 
c   number, corresponds to a stored filename and channel number.
c   all the header informaton is not stored so if mem routines are used for
c   cont mode, it all has to be read in again, something to fix. For a single
c   event it is remembered (also seed, check ??)
c   For achives the, the archive reference ('file name') is used as a 
c   reference. For continuous archive read, a reference is created for
c   each segment read.
c
c   memory buffereing is not used in large seed files, since a whole file
c   is not used
c
c   if a program opens many files sequentially like codaq, data will continue
c   to go in memory and the memory pointers overflow, currently no check.
c   however, there is no need for memeory storage between events, so 
c   simply initlize between each event with wav_mem_init.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   location and network codes and seisan format
c
c   the network code is now and put into channel header position 17 
c   and 20. location code is stored in 8 and 13. position 8 is 3. position
c   of seisan component code and this means that old seisan data files
c   will not looose the 3. position information if given. in the future
c   it is assume that 3. posion component code is reserved for 1. location
c   code. when plotting, seed standard is followed.
c   the variabel wav_comp is keep unchanged since many programs use it
c   but changes will have to be made so the 3. character is not used
c
c   List of routines in this file
c
c
c   wav_read_channel(ichan)           ! reads one channel, ichan, one or cont
c
c   read_wav_header(ifile)            ! reads all headers in file nr ifile
c
c   wav_read_channel_one(ichan)       ! reads one channel,ichan, of one file
c
c   wav_read_3channel(ichan)          ! reads 3 channels of data
c
c   wav_index_total                   ! find total time window of all traces
c                                       and delay of each traces  relative
c                                       to earliest data point
c
c   wav_out_index_total               ! find total time window of all output
c                                       traces and delay of each trace relative
c                                       to earliest data point
c
c   wav_sav_index_total               ! for SAV data --------------------------
c
c   wav_get_interval                  ! check availibility of data for
c                                       different channels
c
c   wav_read_2channel(ichan)          ! reads 2 horizontal channels of data
c
c
c   wav_get_max_interval              ! finds largest common time window where
c                                       data from all selected channels
c                                       available
c
c   put_chead(chanhead)               ! reads one seisan channel header and put
c                                       it into index 1 which also becomes
c                                       current channel, used for seisan
c
c   wav_init                          ! initialize variables from waveform.inc
c
c   wav_find_chan(station,component,channel) ! finds channel number correspond.
c                                       to a given station and component
c
c   wav_sav_sheads(ichan,net_code,outfile,mainhead,chead) ! calls sheads with
c                                       sav_out_block as input
c   wav_copy_sav(i)                   ! copy some of content of wav array to
c                                       sav array for index i, only for cont.
c   wav_select_sav(i,j)               ! copy from one place in waw to another in
c                                       sav

c
c   wav_copy_wav(i)                   ! copy some of content of sav array to
c                                       wav array for index i, only for cont.
c   cwav_read_bases                   ! read all headers for all files in
c                                       cont data set
c
c   cwav_time_limits(start)           ! calculate abs start and end times and 
c                                       extended start time,
c                                       cwav_data_start_time
c
c   cwav_read_header(cbase)           ! read header information from cont
c                                       data bases and append to continuous
c                                       common block
c   cwav_read_channel_one(ichan)      ! read signal from one channel ichan of
c                                       continuous database
c   wav_mem_free_index                  find location of next channel to store 
c                                       in memory
c   wav_mem_in_memeory                  check if channel ichan already is in 
c                                       memory 
c   wav_mem_init                        initilizes counters for memory buffer, 
c                                       done only when program starts
c   update_mem                          after writing samples in memory, some 
c                                       channels might have been overwritten.
c                                       check and update
c   wav_read_arc_headers                read all headers in archive as defined 
c                                       in SEISAN.DEF
c   wav_read_arc_one_header(ichan,filename):  read one header in a in archive, 
c                                       info in filename 
c   3 routines for helmber format
c

c



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine wav_read_channel(ichan)

c
c   read one channel of data, either from one file or a sereis of files
c   following each other in time. gaps are filled with dc levels if
c   cont. data
c   jh oct 2001
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer ichan   ! overall channel number
c
c  check if continous data or one file only
c
      if(cwav.and..not.arc) then
         call cwav_read_channel_one(ichan)
      else
         call wav_read_channel_one(ichan)
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_wav_header(ifile)
c
c  read all headers from waveform file ifile in list of waveform files
c
      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'waveform.inc'
      include 'codeco_common.f'
      include 'libsei.inc'

      integer seiclen
      integer nchan                      ! number of channels, one file
      logical sun,linux,pc
      character*80 mainhead(max_trace)   ! seisan main header
      character*1040 chanhead            ! seisan channel header
      character*20   wrecord             ! first 20 bytes of waveform file
      integer i,j,k,ifile
      character*1 cbuf(c_bufsize)        ! gse ascii data
      integer*4 iy(c_sigsize)            ! gse integer data
      integer*4 ichecksum                ! gse checksum
      integer*4 ierr                     ! gse error
      logical check_sacbin


      call computer_type(sun,pc,linux)
c
      wav_file_format(ifile)=' '
      wav_error_message=' '
c
c   first check for archive since not really a file, here it is a reference
c   in an s-file, or a direct reference given as a waveform file name
c
      if(wav_filename(ifile)(1:3).eq.'ARC') then
         k=wav_nchan      ! index of previous channel
         call wav_read_arc_one_header(k,wav_filename(ifile))
         wav_file_format(ifile)='arc'
         wav_file_nr_chan(wav_nchan)=ifile
         wav_chan_nr_file(wav_nchan)=1   ! always one channel in archive file
         goto 999                        ! go to end
      endif

c
c first check if file compressed and uncompress if so
c
      call uncompress_file(wav_filename(ifile))
c
c-------------------------------------------------------------------
c   then find type of file
c------------------------------------------------------------------

c
c check if Seisan
c 

       open(95,file=wav_filename(ifile),access='direct',
     * recl=20,status='old',err=100,form='unformatted')
       goto 101
 100   continue
       wav_error_message(1:14)='File missing: '
       wav_error_message(15:80)=
     * wav_filename(ifile)(1:66)
       return
 101   continue

       read(95,rec=1,err=1010) wrecord
       close(95)
       if(wrecord(1:2).eq.'KP'.or.wrecord(1:1).eq.'P'.or.
     * wrecord(4:4).eq.'P') then
           wav_file_format(ifile)(1:6)='SEISAN'
           if(sun) call sun_fix_file(wav_filename(ifile))
       endif
c
       goto 1011

 1010  continue
       write(6,*)' Error with file, probably file is empty '
     * ,wav_filename(ifile)
       wav_error_message(1:17)='Error with read  '
       wav_error_message(18:80)=wav_filename(ifile)(1:63)
       return
 1011  continue


c
c check if seed  or miniseed file
c
       if (wav_file_format(ifile).eq.' ') then
c
c   check that sequence number can be read, each digit must be
c   checked since a CR wil terminate read and next char can then be wrong
c
         do i=1,6
           read(wrecord(i:i),'(i1)',err=777) k
         enddo

c         read(wrecord(1:6),'(i6)',err=777) i
c        write(6,*) 'i ',i
c        if(wrecord(1:1).eq.'À') goto 777
         if(wrecord(7:7).eq.'R'.or.wrecord(7:7).eq.'Q'.or.
     *     wrecord(7:7).eq.'D'.or.wrecord(7:7).eq.'M') then
           wav_file_format(ifile)(1:8)='MINISEED'
         elseif(wrecord(7:7).eq.'V') then
           wav_file_format(ifile)(1:4)='SEED'
         endif
       endif
 777   continue

c       if (wav_file_format(ifile).eq.' ') then
cc added lot 6 Sep 2007
c         if(wrecord(7:7).ne.'R'.and.wrecord(7:7).ne.'Q'.and.
c     *     wrecord(7:7).ne.'D') then
c         call read_sacbin_to_seisan(wav_filename(ifile),1,ierr)
c         if (ierr.eq.0) wav_file_format(ifile)(1:6)='SACBIN'
c        endif
c       endif

C
c   check if guralp
c
        if(wav_file_format(ifile).eq.' ')
     *  call check_if_guralp(wav_filename(ifile),wav_file_format(ifile))

c
c check if SACBIN file
c
 
       if (wav_file_format(ifile).eq.' ') then
c         call read_sacbin_to_seisan(wav_filename(ifile),1,0,ierr)
         if (check_sacbin(95,wav_filename(ifile),max_sample)) 
     &        wav_file_format(ifile)(1:6)='SACBIN'    ! changed lo 12 Jan 2011
       endif
c       write(*,*) ' debug ',wav_file_format(ifile)

c
c check if GSE file
c

       if (wav_file_format(ifile).eq.' ') then
         open(95,file=wav_filename(ifile),status='unknown')
         open(94,file='gsetemp.out',status='unknown')

c
c try to read first channel
c
         call gsein( 95, 94, cbuf, iy, ichecksum, ierr )
         if (ierr.eq.0) wav_file_format(ifile)(1:3)='GSE'

         close(95)
         close(94)
       endif

c
c check if SACASC file
c
 
       B_F_MESSAGE$=.false. ! avoid error message if not a sacasc file
       if (wav_file_format(ifile).eq.' ') then
         call read_sacasc_to_seisan(wav_filename(ifile),1,ierr)
         B_F_MESSAGE$=.true.
         if (ierr.eq.0) wav_file_format(ifile)(1:6)='SACASC'
       endif
c
c changed and moved lot nov 07
c
c check if SACBIN file
c
c       if (.not.(pc).and.wav_file_format(ifile).eq.' '.and.
c     &   sacaux.ne.' ') then
c       if (wav_file_format(ifile).eq.' ') then
c         call read_sacbin_to_seisan(wav_filename(ifile),1,ierr)
c         if (ierr.eq.0) wav_file_format(ifile)(1:6)='SACBIN'
c       endif
       
c       write(*,'(2a)') ' input file is ',wav_file_format(ifile)
C
C
c   check if guralp, moved up before sacbin dec 2012
c 
c       if(wav_file_format(ifile).eq.' ')
c    *  call check_if_guralp(wav_filename(ifile),wav_file_format(ifile))
        
c
c   check if helmberger
c

        if(wav_file_format(ifile).eq.' ')
     *  call detect_helm(wav_filename(ifile),
     *  wav_file_format(ifile))
  
c
c  return if no valid format and set a message
c
c      write(6,*)'ifile',ifile,wav_file_format(ifile)
       if(wav_file_format(ifile).eq.' ') then
          wav_error_message='Unknown waveform file format'
          return		! added oct 04
       endif
c      write(6,*)'ifile',ifile,wav_file_format(ifile)
c
c----------------------------------------------------------------------
c   section for reading all headers
c----------------------------------------------------------------------
c
c
c   SEISAN format
c
      if(wav_file_format(ifile)(1:6).eq.'SEISAN') then
         open(95,file=wav_filename(ifile),access='direct',
     *   recl=2048,status='old',form='unformatted')
c
c   read main header to get number of channels, nchan
c
         call seisinc 
     *   (95,1,nchan,0,mainhead,chanhead,0.0,0.0)
         wav_header_text(ifile)(1:28)=mainhead(1)(2:29)
c
c   loop trough all channel headers to get channel info
c
          k=wav_nchan     ! set total channel counter
          do i=1,nchan
             k=k+1
             call seisinc 
     *       (95,i,nchan,3,mainhead,chanhead,0.0,0.0)
             read(chanhead(10:12),'(i3)') j      ! year - 1900
             wav_year(k)=j+1900
             wav_stat(k)=chanhead(1:5)
c lot 28/02/2008
c             wav_comp(k)=chanhead(6:9)
             wav_comp(k)='    '
             wav_comp(k)(1:2)=chanhead(6:7)
             if (chanhead(9:9).ne.' ') then
               wav_comp(k)(4:4)=chanhead(9:9)
             elseif (chanhead(8:8).ne.' ') then
c files written by earthworm use first 3 chars
               wav_comp(k)(4:4)=chanhead(8:8)
             endif
c lot 30/06/2003
c             if (seiclen(wav_comp(k)).eq.3) then
c               wav_comp(k)(4:4)=wav_comp(k)(3:3)
c               wav_comp(k)(3:3)=' '
c             endif
             wav_time_error(k)=' '
             wav_time_error(k)(1:1)=chanhead(29:29)
             read(chanhead(17:28),'(4(1x,i2))') wav_month(k),wav_day(k),
     *       wav_hour(k),wav_min(k)
             read(chanhead(30:35),'(f6.3)') wav_sec(k)
c If more than one wavfile in sfile for continuous data then can't set wav_file_nr_chan here.
c This routine only knows about one wavfile at a time. Set in cwav_read_header instead (RL)
             if (.not.cwav) then
               wav_file_nr_chan(k)=ifile
             endif
             wav_chan_nr_file(k)=i
c
c   location and network
c
             wav_location(k)='  '
             if (chanhead(9:9).ne.' ') then    ! only if 4th component code not empty, earthworm 
               wav_location(k)(1:1)=chanhead(8:8)  ! part of component
               wav_location(k)(2:2)=chanhead(13:13)
             endif
             wav_network(k)='  '
             wav_network(k)(1:1)=chanhead(17:17)
             wav_network(k)(2:2)=chanhead(20:20)
c
             call timsec(wav_year(k),wav_month(k),wav_day(k),
     *       wav_hour(k),wav_min(k),wav_sec(k),wav_abs_time(k))

             read(chanhead,'(43x,i7)') wav_nsamp(k)
             if(wav_nsamp(k).ge.max_sample) then
                 write(6,*) ' Warning: Too many samples in trace ',
     &            wav_nsamp(k),max_sample
c                 write(6,*) ' Enter to stop'
c                 read(5,'(a)') k
c                 stop
             endif
             read(chanhead(37:43),'(f7.3)') wav_rate(k)
             wav_cbyte(k)=chanhead(77:77)
c             wav_duration(k)=(wav_nsamp(k)-1)/wav_rate(k)
             wav_duration(k)=(wav_nsamp(k))/wav_rate(k)  ! jh aug 2008
          enddo
          close(95)
          wav_nchan=k    ! save number of channels
      elseif (wav_file_format(ifile)(1:3).eq.'GSE') then
c
c  GSE format, read header
c
         call header_gse_to_seisan(ifile,ierr)

c
c SAC BINARY, read header
c
      elseif (wav_file_format(ifile)(1:6).eq.'SACBIN') then
         wav_nchan=wav_nchan+1
         k=wav_nchan
         call read_sacbin_to_seisan(wav_filename(ifile),k,1,ierr)
c RL
         if (.not.cwav) then
           wav_file_nr_chan(k)=ifile
         endif
         wav_chan_nr_file(k)=1

c
c SAC ASCII 
c
      elseif (wav_file_format(ifile)(1:6).eq.'SACASC') then
         wav_nchan=wav_nchan+1
         k=wav_nchan
         call read_sacasc_to_seisan(wav_filename(ifile),k,ierr)
c RL
         if (.not.cwav) then
           wav_file_nr_chan(k)=ifile
         endif
         wav_chan_nr_file(k)=1
      endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Guralp format
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(wav_file_format(ifile)(1:6).eq.'GURALP') then
c
c   read header
c
         call get_guralp_header(ifile)
c
c   header does not have number of samples so whole
c   file must be read to get number of samples
c
         call read_guralp_data(wav_nchan)
      endif


c
c
c   MINISEED or SEED format
c
      if(wav_file_format(ifile)(1:8).eq.'MINISEED'.or.
     *   wav_file_format(ifile)(1:4).eq.'SEED') then
c
        wav_seed_year(1)=0    ! indicate that time limits is from 
                              ! start to end of file                

c       write(6,*) 'fist cont'
        do i=1,max_trace
           wav_seed_stat(i)=' '
          wav_seed_comp(i)=' '
          wav_seed_network(i)=' '
           wav_seed_location(i)=' '
           wav_seed_year(i)=0
           wav_seed_month(i)=0
           wav_seed_day(i)=0
           wav_seed_hour(i)=0
            wav_seed_min(i)=0
           wav_seed_sec(i)=0.0
           wav_seed_rate(i)=0.0
           wav_seed_nsamp(i)=0
           wav_seed_time_error(i)=' '
           wav_seed_file_position_start(i)=0
           wav_seed_file_position_end(i)=0
        enddo

c       write(6,*) wav_filename(ifile),max_trace,wav_seed_year(1),
c    *  wav_seed_month(1),wav_seed_year(2),wav_seed_month(2),
c    *  wav_seed_nchan

c
c   problem
c
c   following call crash in Windows Vista, if a write is added like example 
c   below, it works
c
c       open(111,status='scratch')
c       write(111,*)' '
c
c   following will crash on some w10 systems, open and close added as a fix
c
      if(pc) then
        open(111,file=wav_filename(ifile),status='old')
        close(111)
      endif

        call seed_contents(wav_filename(ifile),max_trace,
     *  wav_seed_nchan,
     *  wav_seed_stat,wav_seed_comp,wav_seed_network,wav_seed_location,
     *  wav_seed_year,wav_seed_month,wav_seed_day,wav_seed_hour,
     *  wav_seed_min,wav_seed_sec,
     *  wav_seed_rate,wav_seed_nsamp,wav_seed_time_error,
     *  wav_seed_file_position_start,wav_seed_file_position_end)

c       write(6,*) 'nchan=',wav_seed_nchan

c
c   loop trough all channel headers to update channel info
c
          k=wav_nchan       ! set total channel counter
          do i=1,wav_seed_nchan   
             k=k+1
             wav_year(k)=wav_seed_year(i)
             wav_month(k)=wav_seed_month(i)
             wav_day(k)=wav_seed_day(i) 
             wav_stat(k)=wav_seed_stat(i)
             wav_comp(k)(1:2)=wav_seed_comp(i)(1:2)
             wav_comp(k)(3:3)=' '  ! normally it should be blank
             wav_comp(k)(4:4)=wav_seed_comp(i)(3:3)  
             wav_location(k)=wav_seed_location(i)
             wav_network(k)=wav_seed_network(i)
             wav_time_error(i)=wav_seed_time_error(i)
             wav_hour(k)=wav_seed_hour(i)
             wav_min(k)=wav_seed_min(i)
             wav_sec(k)=wav_seed_sec(i)
             wav_nsamp(k)=wav_seed_nsamp(i)
             wav_rate(k)=wav_seed_rate(i)
c RL
             if (.not.cwav) then
               wav_file_nr_chan(k)=ifile  ! file for channel number
             endif
             wav_chan_nr_file(k)=i      ! channel number in file
             wav_cbyte(k)='4'
             wav_file_position_start(k)=wav_seed_file_position_start(i)
             wav_file_position_end(k)=wav_seed_file_position_end(i)
             wav_duration(k)=(wav_nsamp(k))/wav_rate(k)  ! jh aug 2008
             call timsec(wav_year(k),wav_month(k),wav_day(k),
     *       wav_hour(k),wav_min(k),wav_sec(k),wav_abs_time(k))
c
c            write(6,*)' seed ind,st,cp,start,stop',
c    *       wav_stat(k),wav_comp(k),wav_file_position_start(k),
c    *       wav_file_position_end(k) 
          enddo
          wav_nchan=k    ! save total number of channels
c          write(6,*)'seed nchan',wav_nchan
      endif
c
c  helmberger
c
      
c
      if(wav_file_format(ifile)(1:7).eq.'HELMBER') 
     *call read_header_helm(ifile) 


 999  continue
c
c   find earliest and latest channels
c
      call wav_index_total 
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c

      subroutine wav_seed_read_header_interval
c
c  read and update header info for current seed file  when
c  position is not at start of file. intended for use with large
c  seed/miniseed files
c  works only with one file, assumes that seed file has been indexed first.
c  
c  input is cwav_abs_start_time and cwav_abs_end_time which defines interval
c  output is times etc corresponding to the desired interval 
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      real*8   old_start_time              ! previous start time
      real tinterval
      character*3 b_comp
      integer  i,k,nsamples,doy,ichan
     
c
c   calculate start time for interval from abs time
c
            call sectim(cwav_abs_start_time,
     *      wav_year(1),doy,wav_month(1), 
     *      wav_day(1),wav_hour(1), wav_min(1), 
     *      wav_sec(1))
c
c   calculate end time for interval from abs time
c
            call sectim(cwav_abs_end_time,
     *      wav_year(2),doy,wav_month(2), 
     *      wav_day(2),wav_hour(2), wav_min(2), 
     *      wav_sec(2))
c
c  get times etc for given interval for all channels
c
c            write(*,*) ' debug lo calling seed_contents '
            call seed_contents(wav_filename(1),       ! only one file 
     *      max_trace,wav_nchan,
     *      wav_stat, wav_seed_comp, wav_network,
     *      wav_location,wav_year,wav_month, 
     *      wav_day,wav_hour,wav_min, 
     *      wav_sec,wav_rate,wav_nsamp,wav_time_error,
     *      wav_file_position_start,wav_file_position_end)
c            write(6,*) 'seed cont after'
c
            do i=1,wav_nchan
               wav_comp(i)=' '
               wav_comp(i)(1:2)=wav_seed_comp(i)(1:2)
               wav_comp(i)(4:4)=wav_seed_comp(i)(3:3)
c
c   update info on abs time
c 
              call timsec(wav_year(i),wav_month(i),
     *        wav_day(i),
     *        wav_hour(i),wav_min(i),
     *        wav_sec(i),wav_abs_time(i))
              wav_duration(i)=(wav_nsamp(i)+1)/wav_rate(i)
            enddo
      

c        write(6,*) wav_nsamp(ichan)
c
c   update for next time window, therefore only done for last channel
c

        if(ichan.eq.wav_nchan) old_start_time=cwav_abs_start_time

ct        write(6,*) wav_year(i),wav_month(i),wav_day(i),
ct   *    wav_hour(i),wav_min(i),wav_sec(i)
c
c   find earliest and latest channels
c
      call wav_index_total
c
      return
      end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine wav_read_channel_one(ichan)
c
c   read one channel of data from one file, number as indexed from
c   all wav files
c   can also read one channel from an arc archive
c
c     ichan: channel number to read
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      include 'seisan.inc'
      include 'codeco_common.f'

      integer ichan                      ! current channel in total data set
      character*80 mainhead(max_trace)   ! seisan main header
      character*1040 chanhead            ! seisan channel header
      integer nchan
      character*1 cbuf(c_bufsize)        ! gse ascii data
      integer*4 iy(c_sigsize)            ! gse integer data
      integer*4 ichecksum                ! gse checksum
      integer*4 ierr                     ! gse error
      integer*4 seed_read_chn            ! seed reader function
      real*8    time_err                 ! time jump from one to next seed block
      integer b_start,b_end              ! block to check for time gaps
      integer j,i,n,k,m,l,kk
      logical present                    ! true if channel in memory
      character*3 b_comp                 ! seed component code
      integer gse_counter                ! gse channel counter
      integer interval                   ! interval for arc channel
      integer seiclen
      integer nsamp                      ! number of samples in this read, no the same 
                                         ! as wav_nsamp if in cwav mode or if
                                         ! time gap
      character*1  time_error            ! returned for seed reading
      real tinterval
      integer nsamples


c
c   initially no errors
c
      wav_error_message=' '
      wav_current_chan(1)=0
c
      wav_resp_action=' '        ! no resp action initially
      wav_resp_seisan_chead=' '  ! no seisan resp info
c
c
c     do i=1,10
c        write(6,'(i2,1x,a30,5x,i)') 
c     *   i,wav_mem_filename(i)(1:30),wav_mem_nsamp(i)
c      enddo
c
c   check if data already in memeory, variable present will then be true
c   not used for large seed file
c
      if(arc) then
c
c   for memory save, a reference must be made to know if this segment already
c   has been read. the reference is like the reference used in s-file
c   and will be stored in filename variable. the referencing is set up to
c   use one channel in each 'file'.
c
         wav_file_nr_chan(ichan)=ichan     ! one 'file' for each channel
         wav_chan_nr_file(ichan)=1         ! one channel in each 'file'
         wav_filename(wav_file_nr_chan(ichan))=' '
         wav_filename(wav_file_nr_chan(ichan))(1:3)='ARC'
         kk=cont_interval
         b_comp(1:2)=wav_comp(ichan)(1:2) ! from seisan to seed component
         b_comp(3:3)=wav_comp(ichan)(4:4)
         write(wav_filename(wav_file_nr_chan(ichan))(5:80),
     *   '(a5,1x,a3,1x,a2,1x,a2,1x,i4,1x,2i2,1x,2i2,1x,i2,i6)')
     *   wav_stat(ichan),b_comp,wav_network(ichan),
     *   wav_location(ichan),wav_year(ichan),wav_month(ichan),
     *   wav_day(ichan),wav_hour(ichan),wav_min(ichan),
     *   int(wav_sec(ichan)),kk
c        write(6,*) 'make name, ichan,wfnch,stat,comp',
c    *   ichan,wav_file_nr_chan(ichan),wav_stat(ichan),wav_comp(ichan)

      endif

c
      if(.not.cseed) then
         call wav_mem_in_memeory(ichan,present,k)
         
c        write(6,*) 'ch, count, mem', ichan,wav_mem_counter,present
c
c   read from memory if data, k is index in header memory
c
         if(present) then
            l=wav_mem_position(k)		! start of sample data in memory
            do i=1,wav_mem_nsamp(k)
               signal1(i)=wav_mem_signal(l+i-1)
            enddo
            wav_current_chan(1)=ichan    ! save last channel read
            wav_rot_comp(ichan)=' '      ! channel is not rotated
            return                       ! return since no data should be read
         endif                           ! from disk
       endif

c--------------------------------------------------------------------------
c    archive in continous mode, currently arc only set by mulplt, wavetool
c    or sample_read_arc
c--------------------------------------------------------------------------
c
      if(arc) then
         b_comp(1:2)=wav_comp(ichan)(1:2) ! from seisan to seed component
         b_comp(3:3)=wav_comp(ichan)(4:4)
         kk=cont_interval                 ! interval is intger
         j=seiclen(arc_archive)
c        write(6,*) wav_year(ichan),wav_month(ichan),wav_day(ichan),
c    *   wav_hour(ichan),wav_min(ichan),wav_sec(ichan),kk
         call  getarchdata(arc_archive,j,arc_type,
     *   wav_stat(ichan),5,
     *   b_comp,3,
     *   wav_network(ichan),2,wav_location(ichan),2,
     *   wav_year(ichan),wav_month(ichan),wav_day(ichan),
     *   wav_hour(ichan),wav_min(ichan),wav_sec(ichan),kk,
     *   signal_int)
         do i=1, wav_nsamp(ichan)
            signal1(i)=signal_int(i)
            signal_int(i)=0.0
         enddo
c        write(6,*) (signal_int(i),i=1,5)
c
c   save number of samples this trace, use for memory save
c
         nsamp=wav_nsamp(ichan)
         goto 999               ! goto to memory save since data read
       endif

c-------------------------------------------------------------------
c    archive, event mode
c----------------------------------------------------------------------
c
      if(wav_file_format(wav_file_nr_chan(ichan))(1:3).eq.'arc') then
         b_comp(1:2)=wav_comp(ichan)(1:2) ! from seisan to seed component
         b_comp(3:3)=wav_comp(ichan)(4:4)
         read(wav_filename(wav_file_nr_chan(ichan)),'(37x,i6)') interval 
         j=seiclen(arc_archive)

         call  getarchdata(arc_archive,j,arc_type,
     *   wav_stat(ichan),5,
     *   b_comp,3,
     *   wav_network(ichan),2,wav_location(ichan),2,
     *   wav_year(ichan),wav_month(ichan),wav_day(ichan),
     *   wav_hour(ichan),wav_min(ichan),wav_sec(ichan),interval,
     *   signal_int)
         do i=1, wav_nsamp(ichan)
            signal1(i)=signal_int(i)
            signal_int(i)=0.0
         enddo
c
c   save number of samples this trace, use for memory save
c
         nsamp=wav_nsamp(ichan)

       endif

c-----------------------------------------------------------------------
c    SEISAN format
c-------------------------------------------------------------------------
c
      if(wav_file_format(wav_file_nr_chan(ichan))(1:6).eq.'SEISAN') 
     *   then
         open(95,file=wav_filename(wav_file_nr_chan(ichan)),
     *   access='direct',recl=2048,status='old',err=100,
     *   form='unformatted')
         wav_resp_seisan_chead=' '
         call seisinc
     *   (95,wav_chan_nr_file(ichan),nchan,1,mainhead,chanhead,
     *    0.0,0.0)
         wav_resp_seisan_chead=chanhead   ! save for use in response
         wav_resp_action=chanhead(79:79)  ! forced header response flag
         close(95)
c
c  get number of samples for this trace. this information is not available if  
c  in cont mode since wav_nsamp then contains the  total number of samples in
c  all segmnets
c
         READ(chanhead,'(43X,I7)') nsamp
c
         goto 101
 100     continue
         wav_error_message(1:14)='File missing: '
         wav_error_message(15:80)=
     *   wav_filename(wav_file_nr_chan(ichan))(1:66)
         return
 101     continue

      elseif (wav_file_format(wav_file_nr_chan(ichan))(1:3).eq.'GSE')
     * then

c---------------------------------------------------------------------------
c   GSE format, read data
c---------------------------------------------------------------------------

         open(95,file=wav_filename(wav_file_nr_chan(ichan)),
     *       status='unknown')
         open(94,file='gsetemp.out',status='unknown')
         gse_counter = 0

 200     continue
c
c  might not work for cont data since wav_nsamp has been used in cont section,
c  must be tested
c
         do j=1,wav_nsamp(ichan) ! added may 24, 2000
           signal1(j)=0.
         enddo
         call gsein( 95, 94, cbuf, iy, ichecksum, ierr )
         if (ierr.ne.0) then
           wav_error_message(1:14)='Error in GSE file'
           goto 250
         endif
         gse_counter = gse_counter + 1
c        if (hdr_station(1:5).eq.
c     *     wav_stat(wav_chan_nr_file(ichan)).and.
c     *     hdr_chan(1:2).eq.wav_comp(wav_chan_nr_file(ichan))(1:2)
c     *     .and.hdr_chan(3:3).eq.wav_comp(wav_chan_nr_file(ichan))(4:4)) 
c     *     then

c         if (hdr_station(1:5).eq.
c     *     wav_stat(ichan).and.

c also check for aux code, lo nov 2001
         if ((hdr_station(1:5).eq.wav_stat(ichan).or.
     *        hdr_stadescr(1:4).eq.wav_stat(ichan)).and.
     *     hdr_chan(1:2).eq.wav_comp(ichan)(1:2)
     *     .and.hdr_chan(3:3).eq.wav_comp(ichan)(4:4).and.
     *     gse_counter.eq.wav_chan_nr_file(ichan)) 
     *     then

            do j=1,hdr_nsamp
              signal1(j)=float(iy(j))
            enddo
c
c   get number of samples for this trace. this information is not available if 
c   in cont mode since wav_nsamp then contains the  total number of samples in
c   all segmnets
c
             nsamp=hdr_nsamp

            goto 250
         endif
         goto 200

 250     continue
         close(95)
         close(94)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    GURALP
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      elseif (wav_file_format(
     *wav_file_nr_chan(ichan))(1:6).eq.'GURALP') then
         call read_guralp_data(ichan)
         nsamp=wav_nsamp(ichan)
c
c   helm
c
       elseif(wav_file_format(
     * wav_file_nr_chan(ichan))(1:7).eq.'HELMBER') then
c          write(6,*) 'read hel data'
          call read_chan_helm(ichan)
          nsamp=wav_nsamp(ichan)


c-----------------------------------------------------------------------------
c SAC BINARY
c------------------------------------------------------------------------------

      elseif (wav_file_format(
     *      wav_file_nr_chan(ichan))(1:6).eq.'SACBIN') then
       call read_sacbin_to_seisan(
     *    wav_filename(wav_file_nr_chan(ichan)),ichan,0,ierr)
          nsamp=wav_nsamp(ichan)
c
c SAC ASCII
c
      elseif (wav_file_format(
     *     wav_file_nr_chan(ichan))(1:6).eq.'SACASC') then
           call read_sacasc_to_seisan(
     *     wav_filename(wav_file_nr_chan(ichan)),ichan,ierr)
           nsamp=wav_nsamp(ichan)
c------------------------------------------------------------------------------
c   MINI SEED or SEED format
c------------------------------------------------------------------------------
c
      elseif (wav_file_format(wav_file_nr_chan(ichan))(1:8).
     *eq.'MINISEED'.
     *     or.wav_file_format(wav_file_nr_chan(ichan))(1:4).
     *eq.'SEED') then 


        if(cseed) then
c
c   it is assumed that all common block time variables etc have been
c   updated to cover current interval
c
            tinterval=cwav_abs_end_time-cwav_abs_start_time
c            write(6,*)'interval',tinterval
c            k=wav_chan_nr_file(ichan)
            b_comp(1:2)=wav_comp(ichan)(1:2)  
            b_comp(3:3)=wav_comp(ichan)(4:4)
c            write(6,*) wav_filename(wav_file_nr_chan(ichan)), 
c     *      wav_stat(ichan), b_comp, wav_network(ichan),
c     *      wav_location(ichan),wav_year(ichan),wav_month(ichan), 
c     *      wav_day(ichan),wav_hour(ichan), wav_min(ichan), 
c     *      wav_sec(ichan)

c pv: signal_int replaced by signal1 seed_channel_read below:

            call seed_channel_read(wav_filename(1),
     *      wav_stat(ichan), b_comp, wav_network(ichan),
     *      wav_location(ichan),wav_year(ichan),wav_month(ichan), 
     *      wav_day(ichan),wav_hour(ichan), wav_min(ichan), 
     *      wav_sec(ichan),
     *      tinterval, max_sample,nsamples, signal1, 
     *      wav_time_error(ichan),
     *      wav_file_position_start(ichan),wav_file_position_end(ichan))
      
           wav_nsamp(ichan)=nsamples
           wav_duration(ichan)=(wav_nsamp(ichan)+1)/wav_rate(ichan) 
c           write(6,*) wav_stat(ichan)
c           write(6,*) wav_nsamp(ichan)
C
        else
c
c   read whole channel indicated by interval=0
c
            tinterval=0.0

            k=wav_chan_nr_file(ichan)
            b_comp(1:2)=wav_comp(ichan)(1:2)  
            b_comp(3:3)=wav_comp(ichan)(4:4)

c           write(6,*)'input to  seed_read_channel'
c           write(6,*) wav_filename(wav_file_nr_chan(ichan)), 
c    *      wav_stat(ichan), b_comp, wav_network(ichan),
c    *      wav_location(ichan),wav_year(ichan),wav_month(ichan), 
c    *      wav_day(ichan),wav_hour(ichan), wav_min(ichan), 
c    *      wav_sec(ichan)
c           write(6,*) 'int,max,start,stop ',tinterval,max_sample,
c    *      wav_file_position_start(ichan),wav_file_position_end(ichan)

c pv: signal_int replaced by signal1 in seed_channel_read below:

            call seed_channel_read(wav_filename
     *      (wav_file_nr_chan(ichan)), 
     *      wav_stat(ichan), b_comp, wav_network(ichan),
     *      wav_location(ichan),wav_year(ichan),wav_month(ichan), 
     *      wav_day(ichan),wav_hour(ichan), wav_min(ichan), 
     *      wav_sec(ichan),
     *      tinterval, max_sample,nsamples, signal1, wav_time_error,
     *      wav_file_position_start(ichan),wav_file_position_end(ichan))
      
            wav_nsamp(ichan)=nsamples
c
c             write(6,*) b_comp,wav_comp(ichan)
c            write(6,*) 'ichan in seed read, nsamp',ichan,
c     *       wav_nsamp(ichan)
c
c   number of samples for memory handling
c
            nsamp=wav_nsamp(ichan)
         endif
         if(wav_nsamp(ichan).eq.0) then
            write(6,*)' No data for channel ', 
     *      wav_stat(ichan),wav_comp(ichan)
            return
         endif
      endif
c
ccccccccccccc end
c
 999  continue
      wav_current_chan(1)=ichan    ! save last channel read
      wav_rot_comp(ichan)=' '      ! channel is not rotated
c
c-------------------------------------------------------------------
c   memory handling
c-------------------------------------------------------------------
c
c   since channel was read, it cannot be in memory so it must be stored,
c   not used with large seed files
c
c      if(cseed.or.arc.or.wav_file_format
c    *   (wav_file_nr_chan(ichan))(1:3).eq.'arc') return
       if(cseed) return
c        write(6,*)'save in mem'
c
c   first find where to store the data: array location for headers and
c   and position in memory ring buffer      
c
      call wav_mem_free_index
c
c   store number of samples in this read, why using nsmap, should be wav_nsamp(ichan)
c
      wav_mem_nsamp(wav_mem_free_header)=nsamp
c      wav_mem_nsamp(wav_mem_free_header)=wav_nsamp(ichan)
c
c   store file name and channel number
c
      wav_mem_filename(wav_mem_free_header)=
     *wav_filename(wav_file_nr_chan(ichan))
      wav_mem_chan_number(wav_mem_free_header)=
     *wav_chan_nr_file(ichan)   ! save channel number in current file  
c      write(6,*) 'name,chan' , wav_filename(wav_file_nr_chan(ichan)),
c     *wav_chan_nr_file(ichan)
c
c   store the waveform data, first check if space at end, if not
c   write from beginning
c
      if((nsamp+wav_mem_next_position-1).gt.max_mem_sample)
     *then
         wav_mem_next_position=1
      endif

c lo 14/5/2013
      if (wav_mem_next_position.eq.0) wav_mem_next_position=1
c
c   copy data samples
c
c     write(6,*) 'mem next',wav_mem_next_position
      do i=1,nsamp
         wav_mem_signal(wav_mem_next_position+i-1)=signal1(i)    
      enddo
c
c   store start position of new channel
c
      wav_mem_position(wav_mem_free_header)=wav_mem_next_position
c
c   check which information has been overwritten and null out
c   corresponding channel headers
c
       call update_mem

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_read_3channel(ichan)
c
c   reads 3 component channels of data, number as indexed from all wav files
c   ichan can be any of the 3 channels, data for z, ns and ew are returned
c   in signal 1, signal2 and signal3 in common block. the corresponding 
c   channels numbers are in wav_current_chan(1-3). If wav_current_chan(i)
c   is zero, then the data channel i was not present
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer ichan                      ! current channel in total data set
      integer comp_index(3)              ! channel numbers of components
      character*5 stat                   ! station code for 3 comp station
      character*4 comp                   ! component
      integer i
c
c   Initially no errors
c
      wav_error_message=' '
      do i=1,3
        wav_current_chan(i)=0
        comp_index(i)=0
      enddo
c
c   find if all 3 channels available
c
      stat=wav_stat(ichan)
      comp=wav_comp(ichan)
      do i=1,wav_nchan
         if(stat.eq.wav_stat(i).and.comp(1:3).eq.wav_comp(i)(1:3))
     *      then      ! component same except for last char
c use channels found first in case of duplication, lo 8/1/2016
            if(wav_comp(i)(4:4).eq.'Z'.and.
     &         comp_index(1).eq.0) comp_index(1)=i
            if(wav_comp(i)(4:4).eq.'N'.and.
     &         comp_index(2).eq.0) comp_index(2)=i
            if(wav_comp(i)(4:4).eq.'E'.and.
     &         comp_index(3).eq.0) comp_index(3)=i
c pv 2009-12-18:
            if(wav_comp(i)(4:4).eq.'1'.and.
     &         comp_index(2).eq.0) comp_index(2)=i
            if(wav_comp(i)(4:4).eq.'2'.and.
     &         comp_index(3).eq.0) comp_index(3)=i

c  jh component fix start

            if(wav_comp(i)(4:4).eq.'1'.and.
     &         comp_index(2).eq.0) then
                 comp_index(2)=i
                 wav_comp(i)(4:4)='N'
            endif
            if(wav_comp(i)(4:4).eq.'2'.and.
     &         comp_index(3).eq.0) then 
                 comp_index(3)=i
                 wav_comp(i)(4:4)='E'
            endif
         endif
      enddo
c
c  check if all 3 components available
c
      do i=1,3
         if(comp_index(i).eq.0) then
            wav_error_message=
     *      'All 3 components not available for '//stat
            return
         endif
      enddo
c
c   read 3 components
c
      call wav_read_channel(comp_index(3))
      wav_current_chan(3)=comp_index(3)
      do i=1, wav_nsamp(comp_index(3))
         signal3(i)=signal1(i)
      enddo
      call wav_read_channel(comp_index(2))
      wav_current_chan(2)=comp_index(2)
      do i=1, wav_nsamp(comp_index(2))
         signal2(i)=signal1(i)
      enddo
      call wav_read_channel(comp_index(1))
c
c      write(17,*)(signal1(i),signal2(i),signal3(i),i=1,10)

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_index_total
c
c  find total time window of all traces and delay of each trace
c  relative to earliest data point
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      double precision first_time   ! ealiest abs start time
      double precision last_time    ! latest abs end time
      integer i
c
      first_time=1.0e20
      last_time=0.0
      do i= 1,wav_nchan
         if(wav_abs_time(i).lt.first_time) then
            first_time=wav_abs_time(i)
            wav_first=i
         endif
         if(wav_abs_time(i)+wav_duration(i).gt.last_time) then 
            last_time=wav_abs_time(i)+wav_duration(i)
            wav_last=i
         endif
      enddo
c
c   find new start times relative to new main header
c
      do i=1,wav_nchan
         wav_delay(i)=wav_abs_time(i)-first_time
      enddo		 	  
c
c   check if total time window is reasonable
c
      wav_total_time=last_time-first_time
      if(wav_total_time.gt.3000000.0) then
c        write(6,*) ' Abs times ',first_time,last_time
         do i=1,wav_nchan
            write(6,'(1x,a,a,1x,i4,4i2,f6.1,f13.1)')
     *      wav_stat(i),wav_comp(i),wav_year(i),
     *      wav_month(i),wav_day(i),wav_hour(i),wav_min(i),wav_sec(i),
     *      wav_abs_time(i)
         enddo
         write(6,*)' Total time window is:',wav_total_time,' secs'
         write(6,*)' This is unrealistic, please check '
c  changed lo 5 nov 2014
c         write(6,*)' This is unrealistic, program will stop'
c         write(6,*)' Return to stop		 '
c         write(6,*)
c         read(5,'(a1)') i		 		 
c         stop
       endif

c
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_sav_index_total
c
c  find total time window of all traces and delay of each trace
c  relative to earliest data point
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      double precision first_time   ! ealiest abs start time
      double precision last_time    ! latest abs end time
      integer i
c
      first_time=1.0e20
      last_time=0.0
      do i= 1,wav_nchan
         if(wav_sav_abs_time(i).lt.first_time) then
            first_time=wav_sav_abs_time(i)
            wav_sav_first=i
         endif
         if(wav_sav_abs_time(i)+wav_sav_duration(i).gt.last_time) then 
            last_time=wav_sav_abs_time(i)+wav_sav_duration(i)
            wav_sav_last=i
         endif
      enddo
c
c   find new start times relative to new main header
c
      do i=1,wav_nchan
         wav_sav_delay(i)=wav_sav_abs_time(i)-first_time
      enddo		 	  
c
c   check if total time window is reasonable
c
      wav_sav_total_time=last_time-first_time
      if(wav_sav_total_time.gt.3000000.0) then
c        write(6,*) ' Abs times ',first_time,last_time
         do i=1,wav_nchan
            write(6,'(1x,a,a,1x,i4,4i2,f6.1,f13.1)')
     *      wav_sav_stat(i),wav_sav_comp(i),wav_sav_year(i),
     *      wav_sav_month(i),wav_sav_day(i),wav_sav_hour(i),
     *      wav_sav_min(i),wav_sav_sec(i),
     *      wav_sav_abs_time(i)
         enddo
         write(6,*)' Total time window is:',wav_sav_total_time,' secs'
         write(6,*)' This is unrealistic, please check '
c  changed lo 5 nov 2014
c         write(6,*)' This is unrealistic, program will stop'
c         write(6,*)' Return to stop		 '
c         write(6,*)
c         read(5,'(a1)') i		 		 
c         stop
       endif

c
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_out_index_total
c
c  find total time window of all output traces and delay of each trace
c  relative to earliest data point
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      double precision first_time   ! ealiest abs start time
      double precision last_time    ! latest abs end time
      integer i,k
c
      first_time=wav_out_start(1)
      last_time=wav_out_start(1)
      do i= 1,wav_out_nchan
         k=wav_out_chan(i)
         if(wav_out_start(k).lt.first_time) then
            first_time=wav_out_start(k)
         endif
         if(wav_out_start(k)+wav_out_duration(k).gt.last_time) then
            last_time=wav_out_start(k)+wav_out_duration(k)
         endif
      enddo
c
c   check if total time window is reasonable
c
      wav_out_total_time=last_time-first_time

      return
      end
	   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine wav_get_interval
c
c   Routine to check availibility of data for different channels and adjust
c   prameters accordingly
c
c   Currently (aug 2008) used in wavetool and mul_spec
c
c   Input:
c
c         wav_out_nchan         : number of selected channels
c         wav_out_chan          : the channel numbers (original index) 
c         wav_out_start         : start from earlist overall channel
c         wav_out_duration      : desired duration
c
c   The return values are 
c         wav_out_year etc..... : time of exact sample to start with
c         wav_out_start_sample  : corresponding sample number
c         wav_out_duration      : interval available of each channel. 
c         wav_out_status        : indicates status of selection: 
c                                 0: no data, 1: some data, 
c   start time ok but data missing at end, 2: some data, end time ok, but
c   some data missing in the beginning, 3: some data, but mising at both
c   ends, 4: data ok
      
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      double precision dtime1,dtime2       ! abs start and end time for selection
      integer i,k,doy

      do i=1,wav_out_nchan
         k=wav_out_chan(i)
         wav_out_status(k)=4                   ! assume ok
c
c   check if start time time1 is within data window for channel
c
         dtime1=wav_abs_time(wav_first)+wav_out_start(k)
         if(dtime1.ge.wav_abs_time(k)+wav_duration(k)) then
            wav_out_status(k)=0                ! after end, deselect channel
            goto 100
c           return
         else
            if(dtime1.lt.wav_abs_time(k)) then  ! start time cannot be before
               dtime1=wav_abs_time(k)           ! first sample, use first
               wav_out_status(k)=1
            endif
         endif
c
c   find nearest first sample. first sample at or after given time is used. roundoff
c   is now set to 1/1000 of a sample
c
c         wav_out_first_sample(k)=(0.5/wav_rate(k)+   jh aug 27 08
         wav_out_first_sample(k)=(0.001/wav_rate(k)+
     *                   dtime1-wav_abs_time(k))*wav_rate(k)
         wav_out_first_sample(k)=wav_out_first_sample(k)+1
c         write(6,*) 'first sample',wav_out_first_sample(k)
c
c   calculate exact time of first sample
c
         dtime1=wav_abs_time(k)+(wav_out_first_sample(k)-1)/wav_rate(k)
   
c
c   date and time of first sample
c
         call sectim(dtime1,wav_out_year(k),doy,wav_out_month(k),
     *   wav_out_day(k),wav_out_hour(k),wav_out_min(k),wav_out_sec(k))
c        write(6,*)'time of first sample', wav_out_sec(k) 
c
c   check if end time time2 is outside data window for channel
c
         dtime2=wav_abs_time(wav_first)+wav_out_duration(k)+
     *   wav_out_start(k)
         if(dtime2.le.wav_abs_time(k)) then
            wav_out_status(k)=0                      ! before start, deselect
            goto 100
c            return
         endif
         if(dtime2.gt.wav_abs_time(k)+wav_duration(k)) then
            dtime2=wav_abs_time(k)+wav_duration(k)     ! after end
            if(wav_out_status(k).eq.1) then
                wav_out_status(k)=3                  ! data missing at both ends
            else
                wav_out_status(k)=2                   ! data mising end only
            endif
         endif
         wav_out_duration(k)=dtime2-dtime1   ! corrected value 
         wav_out_rate(k)=wav_rate(k)
c
c   round off set to 1/10 of a sample. Using a smaller number can potentially give 
c   a problem since duration is single precision. E.g. 1 hour of data at 100 hz has a
c   duration of 3600.00. Adding 1/1000 of sample in time gives 3600.00001 which is outside
c   the accuarcy of single precision so no roundoff would be made and potentially
c   one sample would be missing
c
c         write(6,*) 'out duration',wav_out_duration(k)
c         wav_out_nsamp(k)=(wav_out_duration(k)+0.4/wav_out_rate(k))
c     &*wav_out_rate(k)+1
         wav_out_nsamp(k)=(wav_out_duration(k)+0.1/wav_out_rate(k))
     &*wav_out_rate(k)     ! jh aug 27 08
c       write(6,*) 
c     *(wav_out_duration(1)+0.01/wav_out_rate(1))*wav_out_rate(1)
c       write(6,*) wav_out_nsamp(k)
100    continue
      enddo
c
      return
      end
	  
	  
	  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_read_2channel(ichan)
c
c   reads 2 horizontal channels of data, number as indexed from all wav files
c   ichan can be any of the 3 channels, data for ns and ew are returned
c   signal2 and signal3 in common block. the corresponding 
c   channels numbers are in wav_current_chan(2-3). If wav_current_chan(i)
c   is zero, then the data channel i was not present. An error message
c   is also returned.
c   the routine also calculates the common interval between the 2 signals
c   and return results in wav_out ....
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer ichan                      ! current channel in total data set
      integer comp_index(3)              ! channel numbers of components
      character*5 stat                   ! station code for 3 comp station
      character*4 comp                   ! component
      integer i
c
c   initially no errors
c
      wav_error_message=' '
      do i=1,3
        wav_current_chan(i)=0
        comp_index(i)=0
      enddo
c
c   find if 2 horizontal channels available
c
      stat=wav_stat(ichan)
      comp=wav_comp(ichan)
      do i=1,wav_nchan
         if(stat.eq.wav_stat(i).and.comp(1:3).eq.wav_comp(i)(1:3))
     *      then      ! component same except for last char
            if(wav_comp(i)(4:4).eq.'Z'.and.comp_index(1).eq.0) 
     &         comp_index(1)=i
            if(wav_comp(i)(4:4).eq.'N'.and.comp_index(2).eq.0) 
     &         comp_index(2)=i
            if(wav_comp(i)(4:4).eq.'E'.and.comp_index(3).eq.0) 
     &         comp_index(3)=i

c jh component fix start

            if(wav_comp(i)(4:4).eq.'1'.and.
     &         comp_index(2).eq.0) then
                 comp_index(2)=i
                 wav_comp(i)(4:4)='N'
            endif
            if(wav_comp(i)(4:4).eq.'2'.and.
     &         comp_index(3).eq.0) then 
                 comp_index(3)=i
                 wav_comp(i)(4:4)='E'
            endif

c jh component fix end

         endif
      enddo
c
c  check if  2 components available
c
      do i=1,2
         if(comp_index(i).eq.0) then
            wav_error_message=
     *      'Both horizontal components not available for '//stat
            return
         endif
      enddo
c
c   read 2 components
c
      call wav_read_channel(comp_index(3))
      wav_current_chan(3)=comp_index(3)
      do i=1, wav_nsamp(comp_index(3))
         signal3(i)=signal1(i)
      enddo
      call wav_read_channel(comp_index(2))
      wav_current_chan(2)=comp_index(2)
      do i=1, wav_nsamp(comp_index(2))
         signal2(i)=signal1(i)
      enddo

        wav_out_nchan=2     ! there are 2 channels to check
        wav_out_chan(1)=comp_index(2)  ! put in 2 horizontal channels
        wav_out_chan(2)=comp_index(3)
c
c   check intervals
c
      call wav_get_max_interval          
c
c   return if a problem
c
      if(wav_out_status(1).ne.4)  then
         wav_error_message=
     *   'Data interval not available both horizontal channels '//stat
          return
      endif
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_get_max_interval
c
c   Finds largest common time window where data from all 
c   selected channels available
c
c   Input:
c
c         wav_out_nchan         : number of selected channels
c         wav_out_chan          : the channel numbers (original index) 
c
c   The return values are 
c         wav_out_year(1) etc.. : time of start of window
c         wav_out_start(1)      : delay relative main window of total data
c                                 set
c         wav_out_start_sample  : corresponding sample number each channel
c         wav_out_duration      : available duration
c         wav_out_status(1)     : indicates status of selection: 
c                                 0: no data, 4: ok 
      
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      double precision dtime1,dtime2   !    start and end time for selection
      integer i,k,k1

      dtime1=0.0
      dtime2=1.0e20
c
      wav_out_status(1)=4
      do i=1,wav_out_nchan
         k=wav_out_chan(i)
c
c   find latest start time among selected channels
c
         if(wav_abs_time(k).gt.dtime1) then
            dtime1=wav_abs_time(k)
            k1=k
         endif
c
c  find earliest end
c
         if(wav_abs_time(k)+wav_duration(k).lt.dtime2) then
            dtime2=wav_abs_time(k)+wav_duration(k)
         endif
         if(dtime2.le.dtime1) then
            wav_out_status(1)=0
            return
         endif
      enddo
c
c   set start date and time
c
      wav_out_year(1)=wav_year(k1)
      wav_out_month(1)=wav_month(k1)
      wav_out_day(1)=wav_day(k1)
      wav_out_hour(1)=wav_hour(k1)
      wav_out_min(1)=wav_min(k1)
      wav_out_sec(1)=wav_sec(k1)
      wav_out_duration(1)=dtime2-dtime1    
      wav_out_start(1)=dtime1-wav_abs_time(wav_first)
c
c   find nearest first sample
c
       do i=1,wav_out_nchan
         k=wav_out_chan(i)
         wav_out_first_sample(k)=(0.5/wav_rate(k)+
     *                   dtime1-wav_abs_time(k))*wav_rate(k)
         wav_out_first_sample(k)=wav_out_first_sample(k)+1
      enddo
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine put_chead(chanhead)
c
c   read one seisan channel header and put it into index 1 which also becomes 
c   current channel, used for seisan response
c
c  but what is it used for ??????
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
c      character*80 mainhead(max_trace)   ! seisan main header
      character*1040 chanhead            ! seisan channel header
      integer i,j,ifile,k
c
      wav_error_message=' '

      read(chanhead(10:12),'(i3)',err=999) j      ! year - 1900
c bjb
c add following if statement to allow progs to read BGS VAX wave files
c that do not support the century 
      if (j.lt.50)then
         j=j+100
      endif
      wav_year(1)=j+1900
      wav_stat(1)=chanhead(1:5)
      wav_comp(1)=chanhead(6:9)
      wav_time_error(1)=' '
      wav_time_error(1)(1:1)=chanhead(29:29)
      read(chanhead(17:28),'(4i3)',err=999) wav_month(1),wav_day(1),
     *wav_hour(1),wav_min(1)
      read(chanhead(30:35),'(f6.3)',err=999) wav_sec(1)
cfix      wav_file_nr_chan(1)=ifile
cfix      wav_chan_nr_file(1)=i
      call timsec(wav_year(1),wav_month(1),wav_day(1),
     *wav_hour(1),wav_min(1),wav_sec(1),wav_abs_time(1))
      read(chanhead,'(43x,i7)',err=999) wav_nsamp(1)
      read(chanhead(37:43),'(f7.3)',err=999) wav_rate(1)
      wav_cbyte(1)=chanhead(77:77)
c      wav_duration(1)=(wav_nsamp(1)-1)/wav_rate(1)   jh aug 2008
      wav_duration(1)=(wav_nsamp(1))/wav_rate(1)
c
c
c  only one channel
c
      wav_current_chan(1)=1
      wav_current_chan(2)=0
      wav_current_chan(3)=0
c
c  abs time
c
      call timsec(wav_year(1),wav_month(1),wav_day(1),
     *wav_hour(1),wav_min(1),wav_sec(1),wav_abs_time(1))
 
      wav_resp_type='SEISAN'

      goto 1000
 999  continue
      wav_error_message='Read error seisan chead'
 1000 continue

c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_init
c
c initialize variables from waveform.inc
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i

      wav_nchan = 0
      wav_nfiles = 0
      wav_resp_file = ' '
      do i=1,max_trace
         wav_rot_comp(i)=' '
cc         wav_seed_start_block(i)=0
cc         wav_seed_stop_block(i)=0
      enddo
      do i=1,200
         wav_header_text(i)=' '
      enddo
      call wav_mem_init ! added lo 31 May 2016
      
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_find_chan(station,component,channel)
c
c   finds channel number corresponding to a given station
c   and component. error message if not found. If componet
c   is blank, just look for first station. if component 2 and 3
c   are blank, only check 1 and 4. if component only has orientation
c   only check that
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      character*5 station
      character*4 component
      integer channel
      integer i

      wav_error_message=' '
      channel=0
c
c   component and stations
c
c      
c  complete stat and component
c
         do i=1,wav_nchan
            if(component.eq.wav_comp(i).and.
     *      station.eq.wav_stat(i)) then
               channel=i
               return
            endif
          enddo
c      
c  complete stat and 2 chars of component
c
       if(component(2:3).eq.'  ') then
         do i=1,wav_nchan
            if(component(1:1).eq.wav_comp(i)(1:1).and.
     *         component(4:4).eq.wav_comp(i)(4:4).and.
     *         station.eq.wav_stat(i)) then
               channel=i
               return
            endif
          enddo
      endif

c      
c  complete stat and only orientation of component
c
       if(component(1:3).eq.'   ') then
         do i=1,wav_nchan
            if(component(4:4).eq.wav_comp(i)(4:4).and.
     *      station.eq.wav_stat(i)) then
               channel=i
               return
            endif
          enddo
      endif
c
c   blank component
c
      if(component.eq.'    ') then
         do i=1,wav_nchan
            if(station.eq.wav_stat(i)) then
               channel=i
               return
            endif
          enddo
      endif
c
      wav_error_message='No data for '//station//' '//component
      return
      end


      subroutine wav_sheads(ichan,net_code,outfile,mainhead,chead)
c
c call sheads with wav_block as input
c this is to write seisan format
c
c   add location and network code after write
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      integer ichan
      character*5 net_code
      character*80 outfile
      character*80 mainhead(max_trace)
      character*1040 chead
      
c 
c call sheads
c
      call sheads(wav_year,wav_month,wav_day,wav_hour,wav_min,wav_sec,
     *  wav_nchan,ichan,net_code,wav_header_text,wav_stat,wav_comp,
     *  wav_nsamp,wav_rate,wav_cbyte,outfile,mainhead,chead)
c
c   add location nad network code
c
       chead(8:8)=wav_location(ichan)(1:1)
       chead(13:13)=wav_location(ichan)(2:2)
       chead(17:17)=wav_network(ichan)(1:1)
       chead(20:20)=wav_network(ichan)(2:2)
       

      return
      end

c-----------------------------------------------------------------------

      subroutine wav_sav_sheads(ichan,net_code,outfile,mainhead,chead)
c
c call sheads with wav_sav block as input
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      integer ichan
      character*5 net_code
      character*80 outfile
      character*80 mainhead(max_trace)
      character*1040 chead
      
c 
c call sheads
c
      call sheads(wav_sav_year,wav_sav_month,wav_sav_day,
     *  wav_sav_hour,wav_sav_min,wav_sav_sec,
     *  wav_sav_nchan,ichan,net_code,wav_sav_header_text,
     *  wav_sav_stat,wav_sav_comp,
     *  wav_sav_nsamp,wav_sav_rate,wav_sav_cbyte,
     *  outfile,mainhead,chead)
c
c   add location and network code
c
       chead(8:8)=wav_sav_location(ichan)(1:1)
       chead(13:13)=wav_sav_location(ichan)(2:2)
       chead(17:17)=wav_sav_network(ichan)(1:1)
       chead(20:20)=wav_sav_network(ichan)(2:2)
       

      return
      end
c ----------------------------------------------------------------------
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_select_sav(i,j)
c
c   copy some of content of wav array (i) to out array (j) in sav
c   i and j are channel numbers so file number and channel number
c   in file are different
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i,j
c
cx      wav_sav_nchan=wav_nchan
      wav_sav_stat(i)=wav_stat(j)
      wav_sav_comp(i)=wav_comp(j)
      wav_sav_year(i)=wav_year(j)
      wav_sav_month(i)=wav_month(j)
      wav_sav_day(i)=wav_day(j)
      wav_sav_hour(i)=wav_hour(j)
      wav_sav_min(i)=wav_min(j)
      wav_sav_sec(i)=wav_sec(j)
      wav_sav_abs_time(i)=wav_abs_time(j)
      wav_sav_rate(i)=wav_rate(j)
      wav_sav_cbyte(i)=wav_cbyte(j)
      wav_sav_nsamp(i)=wav_nsamp(j)
      wav_sav_duration(i)=wav_duration(j)
      wav_sav_delay(i)=wav_delay(j)
      wav_sav_first=wav_first
      wav_sav_last=wav_last
      wav_sav_total_time=wav_total_time
      wav_sav_location(i)=wav_location(j)
      wav_sav_network(i)=wav_network(j)
      wav_sav_file_nr_chan(i)=wav_file_nr_chan(j)  ! file number for channel
      wav_sav_filename(wav_sav_file_nr_chan(i))=       
     *wav_filename(wav_file_nr_chan(j))
      wav_sav_chan_nr_file(i)=wav_chan_nr_file(j)      ! channel number in file
      return
      end
c ----------------------------------------------------------------------


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wav_copy_sav(i)
c
c   copy some of content of wav array to sav array for index i
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i
c
cx      wav_sav_nchan=wav_nchan
      wav_sav_stat(i)=wav_stat(i)
      wav_sav_comp(i)=wav_comp(i)
      wav_sav_year(i)=wav_year(i)
      wav_sav_month(i)=wav_month(i)
      wav_sav_day(i)=wav_day(i)
      wav_sav_hour(i)=wav_hour(i)
      wav_sav_min(i)=wav_min(i)
      wav_sav_sec(i)=wav_sec(i)
      wav_sav_abs_time(i)=wav_abs_time(i)
      wav_sav_rate(i)=wav_rate(i)
      wav_sav_cbyte(i)=wav_cbyte(i)
      wav_sav_nsamp(i)=wav_nsamp(i)
      wav_sav_duration(i)=wav_duration(i)
      wav_sav_delay(i)=wav_delay(i)
      wav_sav_first=wav_first
      wav_sav_last=wav_last
      wav_sav_total_time=wav_total_time
      wav_sav_location(i)=wav_location(i)
      wav_sav_network(i)=wav_network(i)

cxx
c      write(6,*) i,wav_file_nr_chan(i)
      if(wav_file_nr_chan(i).gt.0) then   ! this is a fix, have to find why 0
      wav_sav_file_nr_chan(i)=wav_file_nr_chan(i)  ! file number for channel
      wav_sav_filename(wav_sav_file_nr_chan(i))=       
     *wav_filename(wav_file_nr_chan(i))
      wav_sav_chan_nr_file(i)=wav_chan_nr_file(i)      ! channel number in file
      endif
      return

      return
      end
c ----------------------------------------------------------------------



      subroutine wav_copy_wav(i)
c
c   copy some of content of sav array to wav array for index i
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i

cx      wav_nchan=wav_sav_nchan
      wav_stat(i)=wav_sav_stat(i)
      wav_comp(i)=wav_sav_comp(i)
      wav_year(i)=wav_sav_year(i)
      wav_month(i)=wav_sav_month(i)
      wav_day(i)=wav_sav_day(i)
      wav_hour(i)=wav_sav_hour(i)
      wav_min(i)=wav_sav_min(i)
      wav_sec(i)=wav_sav_sec(i)
      wav_abs_time(i)=wav_sav_abs_time(i)
      wav_rate(i)=wav_sav_rate(i)
      wav_cbyte(i)=wav_sav_cbyte(i)
      wav_nsamp(i)=wav_sav_nsamp(i)
      wav_duration(i)=wav_sav_duration(i)
      wav_delay(i)=wav_sav_delay(i)
      wav_first=wav_sav_first
      wav_last=wav_sav_last
      wav_total_time=wav_sav_total_time
      wav_location(i)=wav_sav_location(i)
      wav_network(i)=wav_sav_network(i)
cxx
c  fix
      if(wav_sav_file_nr_chan(i).gt.0) then
      wav_file_nr_chan(i)=wav_sav_file_nr_chan(i)  ! file number for channel
      wav_filename(wav_file_nr_chan(i))=       
     *wav_sav_filename(wav_sav_file_nr_chan(i))
      wav_chan_nr_file(i)=wav_sav_chan_nr_file(i)      ! channel number in file
      endif
 

      return
      end



c ======================================================================

      subroutine cwav_read_bases
c
c  read all headers for all files in cont data set,
c
c  IMPORTANT   This routine calculates all output time intervals and start
c              times including time gaps between files and put this in wav_sav array
c              ready for output so that array should not be used before output
c              However time gaps within a seed file is not checked here, but should 
c              be ok since current routine only deals with start time and total
c              number of samples


      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'waveform.inc'

      integer base_cnt                       ! data base counter
      integer n_out_sample                   ! 
      real diff                              ! difference between two times
      double precision tfirst                ! abs first time
      integer i,l,j,n1,n2,doy,kk

      n_cont_trace=0

c
c loop over databases, to read header information
c
      do base_cnt=1,n_cont_base
c
c   make sure '_' is there, was missing
c
          do i=2,5
            if(cont_base(base_cnt)(i:i).eq.' ')
     *      cont_base(base_cnt)(i:i)='_'
          enddo
cx
cc        write(*,'(i2,1x,a)') base_cnt,cont_base(base_cnt)
c
c read header info from databases and append to continuous common block
c
        call cwav_read_header(cont_base(base_cnt))
      enddo
c
c   put headers back to normal headers for possible output
c
      do i=1,n_cont_trace
         wav_stat(i)=cwav_stat(i)
         wav_comp(i)=cwav_comp(i)
         wav_location(i)=cwav_location(i)
         wav_network(i)=cwav_network(i)
         wav_year(i)=cwav_year(i,1)
         wav_month(i)=cwav_month(i,1)
         wav_day(i)=cwav_day(i,1)
         wav_hour(i)=cwav_hour(i,1)
         wav_min(i)=cwav_min(i,1)
         wav_sec(i)=cwav_sec(i,1)
         wav_abs_time(i)=cwav_abs_time(i,1)
         wav_rate(i)=cwav_rate(i,1)
         wav_cbyte(i)=cwav_cbyte(i)
c
c   find number of samples including gaps for sum of original whole blocks,
c   check for timing errors or missing blocks, start with 2. file
c   only check for missing data or gaps. sample can be inaccurate
c   so gap must be more then 2 s to be considered real
c
         n_out_sample=0
         do j=1,cwav_nseg(i)
            if(j.gt.1) then
               diff=cwav_abs_time(i,j)-cwav_abs_time(i,j-1)-
     *         cwav_duration(i,j-1)
               if(diff.gt.2.0) then
                  diff=diff+0.5/cwav_rate(i,j-1)   ! round off
                  kk=int(diff)*cwav_rate(i,j-1)
                  do l=1,kk
                     n_out_sample=n_out_sample+1
                  enddo
               endif
            endif
            n_out_sample=n_out_sample+cwav_nsamp(i,j)
            if(n_out_sample.ge.max_sample) then
                write(6,*)' Too many samples, will stop'
                write(6,*)' Enter to stop'
                read(5,'(a)') l
                stop
            endif
         enddo
         wav_nsamp(i)=n_out_sample
c
c   adjust all headers for start and end times
c
c
c   find real start time, if no data, use first sample of data available
c
         n1=1   
         if(cwav_abs_start_time.gt.wav_abs_time(i)) then  ! start after first sample
             diff=cwav_abs_start_time-wav_abs_time(i)
c
c   adjust for round off, jh august 2008
c
             diff=diff+0.004
c
             n1=diff*wav_rate(i) + 1    ! +1 added august 5, 2008, jh
c
c   adjust time of first sample
c
             tfirst=wav_abs_time(i)+
     *       (n1+0)/wav_rate(i) ! exact time on first sample, -1 to 0 jh aug 08
             call sectim(tfirst,wav_year(i),doy,wav_month(i),wav_day(i),
     *       wav_hour(i),wav_min(i),wav_sec(i))
             wav_abs_time(i)=tfirst
         endif
c
c   find last sample
c
c   case of end time contained in last block
c
         if(cwav_abs_time(i,cwav_nseg(i))+cwav_duration(i,cwav_nseg(i)).
     *   gt.cwav_abs_end_time) then
            n2=n1+(cwav_abs_end_time-wav_abs_time(i)+0.0004)*wav_rate(i)-1
c
c   round off 0.004 added jh aug 2008
c
c   case of end time after end of last available sample
c
         else
             n2=wav_nsamp(i)
c            n2=n1+(cwav_abs_time(i,cwav_nseg(i))+
c     *      cwav_duration(i,cwav_nseg(i)))*wav_rate(i)-1
         endif
c
c   check if enough samples
c
c         if(n2.gt.wav_nsamp(i)) n2=wav_nsamp(i)

         wav_nsamp(i)=n2-n1+1
c
c   find new duration
c
         wav_duration(i)=wav_nsamp(i)/wav_rate(i) 
c
c   save in sav array since other routnes also use these 
c   variables for reading individual events. doen again below, needed here ???
c
         call wav_copy_sav(i)   

      enddo

      wav_nchan=n_cont_trace
      wav_sav_nchan=wav_nchan
c
c   find earliest and latest channels
c
      call wav_index_total

cx copy this result, simpler way ?
c
c  this must be saved for cont use since same variabels are used
c  when reading individual files in cont data base
c

      do i=1,wav_nchan
         call wav_copy_sav(i)
      enddo
c
c   check if data
c
      if(wav_nchan.eq.0) then
          write(6,*)' No data found'
          write(6,*)
     *' Check if parameter CONT_BEFORE in SEISAN.DEF is large enough'
c commented out lot 6/8/08
c          write(6,*)' Return to stop'
c          read(5,'(a)') i
c          stop
      endif

      return
      end



c ======================================================================
      subroutine cwav_time_limits(start)
c
c   calculate abs start and end times and extended start time,
c   cwav_data_start_time
c
c   if start = 0, start with year month...
c      else
c   start with abs time
c
c
      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'waveform.inc'

      integer year,month,day,doy,hour,min,isec  ! date and time
      real sec                                  ! --------
      double precision msec,start_msec          ! time in seconds
      integer i,start

c
c   compute start and end time in seconds
c   (cwav_abs_start_time and cwav_abs_end_time)
c
      if(start.eq.0) then
c         read(cwav_start_time,'(i4,5(i2))') year,month,day,hour,min,isec
         read(cwav_start_time,'(i4,5(i2))') year,month,day,hour,min,isec
         if (cwav_start_time(15:15).eq.'.') then
           read(cwav_start_time,'(14x,f4.3)') sec
           sec=isec + sec
         else
c         sec=isec + 0.0001
           sec=float(isec)
         endif
c         write(*,*) sec
c        write(6,*) 'cw',year,month,day,hour,min,sec
         call timsec(year,month,day,hour,min,sec,cwav_abs_start_time)
      endif

      cwav_abs_end_time=cwav_abs_start_time+cont_interval
c
c   normal start time for cseed and arc
c
      call sectim(cwav_abs_start_time,year,doy,month,day,hour,min,sec)
c     write(6,*) 'cw1',year,month,day,hour,min,sec
      isec=sec+0.0001            ! jh add nov 22 2010
      write(cwav_start_time(1:14), '(i4,5(i2))') ! time to get data  
     *year,month,day,hour,min,isec       

c
c   start time to start searching, only relvant for seisan cont
c   variable cwav_data_start_time
c
      start_msec=cwav_abs_start_time-cont_before*60.0
      call sectim(start_msec,year,doy,month,day,hour,min,sec)     
c     write(6,*) 'cw1',year,month,day,hour,min,sec
      isec=sec+0.0001            ! jh add nov 22 2010
      write(cwav_data_start_time(1:14), '(i4,5(i2))') ! time to get data in cont
     *year,month,day,hour,min,isec       !- to make sure start is in data   
c
c   saving the end time
c
      call sectim(cwav_abs_end_time,year,doy,month,day,hour,min,sec)     ! end time
      isec=sec+0.0001  ! jh add nov 22 2010
      write(cwav_end_time(1:14), '(i4,5(i2))') year,month,day,hour,min,
     *isec
c
      do i=1,14                ! fill up blancs with 0
        if(cwav_end_time(i:i).eq.' ') cwav_end_time(i:i)='0'
        if(cwav_start_time(i:i).eq.' ') cwav_start_time(i:i)='0'
        if(cwav_data_start_time(i:i).eq.' ')
     *  cwav_data_start_time(i:i)='0'
      enddo
cx
cc      write(6,*) cwav_start_time,cwav_end_time 
      return
      end



c --------------------------------------------------
c                                                                               
c
      subroutine cwav_read_header(cbase)

c   Subroutine to read header information from cont data bases and
c   append to continuous common block 

      implicit none                                                 
c    
c    input: cbase                  - name of continuous data base
c           cwav_data_start_time   - start time, in common block  
c           cwav_end_time          - end time, in common block
c
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'
       include 'seisan.inc'
       include 'waveform.inc'
C
C
      character*80  file_out       !complete wav name
      integer       nstat,nphase   !See routine: 'indata'
      character*1   type,exp       !See routine: 'indata
      integer       id             !id line number
      integer       n_wav_file     !number wavform files in s-file
      integer       read1          !read unit 1
      logical       flag,b_flag    !flag
      character*40  base_name      !data base name
      character*5   cbase          !data base name
      integer       from_eev       !indicate to routine findevin that call is from eev
      character*80  evfile         !event file name       
      integer       event_no       !event number
      integer       nhead,nrecord  ! for indata
c      character*80  data(100)      ! s-file content
      character*80  data(max_phase)! s-file content
      integer       status         !status of event search               
      integer       code           !returned code 
      integer       new_month      !new month indicator              
      integer       fstart         !see base               
      integer       k,i,j,l        !counter
      integer       ns(max_ctrace) !sample counter
      character*10  key            !key in findevin
      logical       dup(500)       !indicate duplicate files
      integer       ichan          !channel number
 

      base_name(1:5)=cbase
c
c  make sure current base is searched for first, jh dec 5, 2012
c
      wav_base(1)=cbase
c
      key= ' '
      from_eev=0
c
c   start of reading loop
c
 10   continue

c
c find s-file name, go through database
c
      call findevin
     &   (base_name,cwav_data_start_time,cwav_end_time,key,
     *    from_eev,event_no,evfile,fstart,new_month,status)
cc      write(6,*) base_name,cwav_start_time,cwav_end_time
cc      write(*,*)'sfile ', evfile,status
c
c return at end of time period
c
      if (status.eq.3) goto 20

  25  continue
c
c open and read s-file
c
       nhead=0
       call sei open( old$,                   ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  evfile,               ! This file.
     &                  read1,                ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
       call indata(read1,nstat,nphase,nhead,nrecord,type,exp,data,id)
       call sei close(close$,read1,code)    ! Close (stop on error).
c
c   get waveform headers
c
c    -- Find waveform file (wav_filename) in sfile (data)
c
          call auto_tr(data,nhead,nrecord,n_wav_file,wav_filename)
cx
cc        write(6,*) 'nf ',n_wav_file, wav_filename(1)
c
c   remove duplicate waveform files, should not be there
c
          do k=1,n_wav_file-1   ! find duplicate
             do i=k+1,n_wav_file
                dup(i)=.false.
                if(wav_filename(k).eq.wav_filename(i)) then
                   dup(i)=.true.
                   write(6,*) ' Duplicate file *******', wav_filename(i)
                endif
             enddo
          enddo
          j=0
c
c   remove duplicate
c
          do k=1,n_wav_file
             if(.not.dup(k)) then
                 j=j+1
                 wav_filename(j)=wav_filename(k)
             endif
          enddo            
          n_wav_file=j
c         write(6,*)' number of wav files',n_wav_file
c
c   loop over waveform files
c
	  do k=1,n_wav_file !there can be more than one wavform file in a s-file 
	     call get_full_wav_name(wav_filename(k),file_out)
	     wav_filename(k)=file_out
c
c init wave structure
c
             call wav_init

             call read_wav_header(k)
cx             write(6,*) wav_filename(k)
c
c   copy waveform header information to continuous common block of all traces
c
             do j=1,wav_nchan
c
c   check which trace, new if not found
c
               flag=.false.
               do i=1,n_cont_trace
                 if (cwav_stat(i).eq.wav_stat(j).and.
     &              cwav_comp(i).eq.wav_comp(j).and.
     *              cwav_location(i).eq.wav_location(j).and.
     *              cwav_network(i).eq.wav_network(j)) then
                    flag=.true.
                    ichan=i
                 endif
               enddo
c
c   move component from 3 to 4, needed ?
c
c               if(wav_comp(j)(3:3).ne.' ') then
c                  wav_comp(j)(4:4)=wav_comp(j)(3:3)
c                  wav_comp(j)(3:3)=' '
c               endif
c
c   check if new channel
c
               if(.not.flag.and.n_cont_trace.lt.max_ctrace) then !max_ctrace def in continuous.inc
                 n_cont_trace=n_cont_trace+1
                 cwav_stat(n_cont_trace)=wav_stat(j)
                 cwav_comp(n_cont_trace)=wav_comp(j)
                 cwav_location(n_cont_trace)=wav_location(j)
                 cwav_network(n_cont_trace)=wav_network(j)
                 ichan=n_cont_trace
cx
                 cwav_nseg(ichan)=0        ! start counter
                 ns(ichan)=0
cc                 write(6,*) (cwav_stat(l),l=1,n_cont_trace)
cc                 write(*,*) n_cont_trace,'  ',cwav_stat(i),cwav_comp(i)
               endif
c  Was set in read_wav_header but it only knows about one wavfile - set correctly here (RL)
               wav_file_nr_chan(ichan)=k 
c
c check if number of maximum traces exceeded 
c
               if (n_cont_trace.gt.max_ctrace) then  
                  write(*,*)' Number of maximum traces exceeded'
                  write(6,*)' Return to continue'
                  read(5,'(a)') i
               endif
c
c check if memory full
c
               if (cwav_nseg(ichan).gt.max_cseg) then
                   write(*,*)'*** ', wav_stat(j),wav_comp(j),
     &             ': Number of maximum waveform files exceeded ***'
                   write(6,*)' Return to continue'
                   read(5,'(a)') i
               endif

               if (ns(ichan)+wav_nsamp(j).ge.max_sample) then
                   write(*,*)'*** ', wav_stat(j),wav_comp(j),
     &             ': Number of maximum samples exceeded ***'
                   write(6,*)' Return to continue'
                   read(5,'(a)') i
               endif

               ns(ichan)=ns(ichan)+wav_nsamp(j)
cx               write(6,*)'ns,chan,j',ns(ichan),ichan,j
c
c   store header information in continuous block
c
               cwav_nseg(ichan)=cwav_nseg(ichan)+1
               cwav_filename(ichan,cwav_nseg(ichan))=wav_filename(k)
cx cx
cc               write(6,*) 'seg:', cwav_nseg(ichan),
cc     *         cwav_filename(ichan,cwav_nseg(ichan))
               cwav_file_format(ichan,cwav_nseg(ichan))=
     *         wav_file_format(k)
cc               write(6,*)'format in ',wav_file_format(k)
               cwav_abs_time(ichan,cwav_nseg(ichan))=wav_abs_time(j)
               cwav_nsamp(ichan,cwav_nseg(ichan))=wav_nsamp(j)
               cwav_rate(ichan,cwav_nseg(ichan))=wav_rate(j)
               cwav_duration(ichan,cwav_nseg(ichan))=wav_duration(j)
               cwav_chan_nr_file(ichan,cwav_nseg(ichan))=
     *         wav_chan_nr_file(j)
               cwav_time_error(ichan,cwav_nseg(ichan))=
     *         wav_time_error(j)
               cwav_year(ichan,cwav_nseg(ichan))=wav_year(j)
               cwav_month(ichan,cwav_nseg(ichan))=wav_month(j)
               cwav_day(ichan,cwav_nseg(ichan))=wav_day(j)
               cwav_hour(ichan,cwav_nseg(ichan))=wav_hour(j)
               cwav_min(ichan,cwav_nseg(ichan))=wav_min(j)
               cwav_sec(ichan,cwav_nseg(ichan))=wav_sec(j)
               cwav_file_position_start(ichan,cwav_nseg(ichan))=
     *          wav_file_position_start(j)
               cwav_file_position_end(ichan,cwav_nseg(ichan))=
     *          wav_file_position_end(j)
               cwav_cbyte(ichan)=wav_cbyte(j)
             enddo
          enddo
c
c next event file
c
      key='next'
      goto 10

20    continue

      return
      end



c ---------------------------------------------------------------

      subroutine cwav_read_channel_one(ichan)
c
c    routine to read signal from one channel ichan of continuous database
c    ichan is the total number indexed over all cont data bases
c    data is output in usual signal1 and headers are also in the usual place
c    gaps are filled with dc-level
c


      implicit none
c
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'seidim.inc'
      include 'seisan.inc'
      include 'waveform.inc'
      include 'libsei.inc'                ! Open file definitions
C
C    ============= end of list ==========
C
      integer ichan                       ! cont channel number
      integer i,j,l,k                     ! counter
      real diff                           ! time difference
      real dc                             ! dc level
      integer n_out_sample                ! sample counter

c
c   extract samples for all files for channel ichan
c
      n_out_sample=1    ! sample counter

      do j=1,cwav_nseg(ichan)
        wav_filename(1)=cwav_filename(ichan,j)  ! put current file to read in first index
c
c       write(6,*) 'ichan, j cont file name',ichan,j,wav_filename(1)
c       wav_file_nr_chan(cwav_chan_nr_file(ichan,j))=1  ! always first file
c
c        write(6,*)'file to read ',wav_filename(1)(1:40)
c
c   read one file, it is assumed there is only one file so index
c   if fixed to 1. No storage of file numbers is used for cont reading
c
        wav_file_format(1)=     ! jh  index to a fixed 1, aug 2008
     *  cwav_file_format(ichan,j)
c        write(6,*)'chan ',cwav_chan_nr_file(ichan,j),
c     *  'format',wav_file_format(cwav_chan_nr_file(ichan,j))
c        write(6,*) 'wav_file_nr_chan',wav_file_nr_chan(1)

        call wav_init
c
c       write(6,*) 'cont chan,# of segs',cwav_chan_nr_file(ichan,j),
c    *  cwav_nseg(ichan)

c
c  restore start time etc  for segment, used by new mseed reading jh oct 20 2011
c  ichan is overall channel in cont selection, j is segment number
c
               k=cwav_chan_nr_file(ichan,j)   ! channel number in segment file
               wav_year(k)=cwav_year(ichan,j)
               wav_month(k)=cwav_month(ichan,j)
               wav_day(k)=cwav_day(ichan,j)
               wav_hour(k)=cwav_hour(ichan,j)
               wav_min(k)=cwav_min(ichan,j)
               wav_sec(k)=cwav_sec(ichan,j)
               wav_file_position_start(k)=
     *         cwav_file_position_start(ichan,j)
               wav_file_position_end(k)=cwav_file_position_end(ichan,j)

               wav_stat(k)=  cwav_stat(ichan)
               wav_comp(k)=  cwav_comp(ichan)
               wav_location(k)=  cwav_location(ichan)
               wav_network(k)=  cwav_network(ichan)

        call wav_read_channel_one(cwav_chan_nr_file(ichan,j)) !signal1 put in commom block
c
c   check for timing errors or missing blocks, start with 2. file
c   only check for missing data or gaps. sample can be inaccurate
c   so gap must be more then 2 s to be considered real
c
c        write(*,*) ' debug ',j,cwav_nsamp(ichan,j)
        if(j.gt.1) then
            diff=cwav_abs_time(ichan,j)-cwav_abs_time(ichan,j-1)-
     *      cwav_duration(ichan,j-1)
            if(diff.gt.2.0) then
               diff=diff+0.5/cwav_rate(ichan,j-1)   ! round off
c   find dc
               dc=0.0
               do l=1,cwav_nsamp(ichan,j-1)
                   dc=dc+signal1(l)
               enddo
               dc=dc/cwav_nsamp(ichan,j-1)

               do i=1,int(diff*cwav_rate(ichan,j-1))
                  signal2(n_out_sample)=dc
                  n_out_sample=n_out_sample+1
               enddo
            endif
        endif
               
        do i=1,cwav_nsamp(ichan,j)   ! number of samples in this segment
            signal2(n_out_sample)=signal1(i)
            n_out_sample=n_out_sample+1
        enddo 
      enddo
c
c   restore header info for cont. operation since the variabels have
c   been used for reading individual files
c
      wav_nchan=wav_sav_nchan
cx 
cx      write(17,*)'restore for ',wav_nchan
      do i=1,wav_nchan
        call wav_copy_wav(i)
      enddo
      wav_current_chan(1)=ichan
c
c   cut out desired time interval
c
c
c  start time is within first block
c
      if(cwav_abs_start_time.gt.cwav_abs_time(ichan,1)) then
         k=(cwav_abs_start_time-
     *   cwav_abs_time(ichan,1)+0.0001)*wav_rate(ichan)+1
cc       write(6,*) 'n1,n2', k,wav_nsamp(ichan)
c
c   start time is before first block
c
      else
         k=1
      endif
      if (k.gt.0) then  ! lo 3/12/2014
        do i=k,k+wav_nsamp(ichan)-1
           signal1(i-k+1)=signal2(i)
        enddo
      endif

      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccc    memory handeling


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c ----------------------------------------------------------------------

      subroutine wav_mem_free_index
c
c   find location of next channel to store in memory, the index returned
c   through a common is wav_free_header
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i
c
c  if no data yet in memeory, just initilize to 1
c
       if(wav_mem_counter.eq.0) then
          wav_mem_counter=1
          wav_mem_free_header=1
          goto 10
       endif
c
c   see if any previous used header index are free

      do i=1,wav_mem_counter
        if(wav_mem_nsamp(i).eq.-1) then   ! number of sampels is -1 if no data
           wav_mem_free_header=i
           goto 10
        endif
      enddo
c
c  none are free, add one
c
      wav_mem_counter=wav_mem_counter+1
      wav_mem_free_header=wav_mem_counter

c
c   enter here if one was found
c
 10   continue
c     write(6,*)'free header',wav_mem_free_header
c     write(6,*)'mem_counter',wav_mem_counter
c     write(6,*)'next pos',wav_mem_next_position
c     write(27,*)'free header',wav_mem_free_header
c     write(27,*)'mem_counter',wav_mem_counter
c     write(27,*)'next pos',wav_mem_next_position
c
c  check if not too many
c
      if (wav_mem_counter.gt.max_trace) then
        write(*,*) ' internal memory exceeded, wav_mem_counter ',
     &     wav_mem_counter 
        stop
      endif
      return
      end

c--------------------------------------------------------------------------

      subroutine wav_mem_in_memeory(ichan,present,wav_mem_chan)
c
c   check if channel ichan already is in memory and therefore can be read from
c   memory. if in memory, return present as true and 
c   position in header ring buffer wav_mem_chan
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i,ichan
      integer wav_mem_chan       ! channel number in memory ring buffer
      integer j                  ! channel number in orignal waveform file
      logical present
c
      present=.false.
c
c  return if not in memory
c

c       write(6,*) wav_mem_counter
      if(wav_mem_counter.eq.0) return
c
      j=wav_file_nr_chan(ichan)
c
c   check all segments, file name, channel number in file must be equal and
c   number of samples positive
cxxx
      do i=1,wav_mem_counter
c           write(6,'(i3,1x,a45,1x,a45,1x,2i3)')
c     *      i,wav_mem_filename(i)(1:45),
c     *             wav_filename(wav_file_nr_chan(ichan))(1:45),
c     *             wav_mem_chan_number(i),wav_chan_nr_file(ichan)
c           write(6,*)'ichan,mem samp = ',ichan,wav_mem_nsamp(i)
c           write(27,'(i3,1x,a30,1x,a30,1x,2i3)')
c     *      i,wav_mem_filename(i)(1:30),
c     *             wav_filename(wav_file_nr_chan(ichan))(1:30),
c     *             wav_mem_chan_number(i),wav_chan_nr_file(ichan)
  
         if(wav_mem_filename(i).
     *      eq.wav_filename(wav_file_nr_chan(ichan))  .and.
     *      wav_mem_chan_number(i).eq.wav_chan_nr_file(ichan).and.
     *      wav_mem_nsamp(i).gt.0) goto 1

      enddo
      goto 2
 1    continue
      present=.true.
      wav_mem_chan=i
 2    continue
c     write(27,*) 'present ',present
      return
      end

c---------------------------------------------------------------------------

      subroutine wav_mem_init
c
c   initilizes counters for memory buffer, done only when program starts
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i

      wav_mem_free_header=0         ! lot may 2016
      wav_mem_next_position=1       ! position of first sample to write
      wav_mem_counter=0             ! number of segments stored, start with 0

      do i=1,max_trace
        wav_mem_nsamp(i)=-1         ! no samples
      enddo

      return
      end
c
c----------------------------------------------------------------------------
c
       subroutine update_mem
c
c  after writing samples in memory, some channels migh have been overwritten.
c  find out which, and mark the channels no longer in memory
c  this means that that index is free for another write in header
c
c  also update next position to write
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer i,first,last,n1,n2

      first=wav_mem_position(wav_mem_free_header)     ! postion of first sample just written
      last=first+wav_mem_nsamp(wav_mem_free_header)-1 ! -----------last------------------
c     write(27,*)'first,last',first,last
c
c   if only one segment written, just update sample counter and return
c
      if(wav_mem_counter.eq.1) goto 10
      
c
c   check all previous headers corresponding to segments stored
c
      do i=1,wav_mem_counter
          n1=wav_mem_position(i)      ! start of old segment
          n2=n1+wav_mem_nsamp(i)-1    ! end of old segment
c          write(6,*) 'n1,n2',n1,n2
c
c   skip the one just written
c
          if(i.ne.wav_mem_free_header) then     
c
c  check if start of last write is in segnment
c
            if(first.ge.n1.and.first.le.n2) wav_mem_nsamp(i)=-1
c
c  check if end of write is in segnment
c
            if(last.ge.n1.and.last.le.n2) wav_mem_nsamp(i)=-1
c
c  check if segment is enclosed by the new write 
c
             if(n1.gt.first.and.n2.lt.last) wav_mem_nsamp(i)=-1
          endif
       enddo
 10    continue
c
c   update next postion to write in memory ring buffer
c
       wav_mem_next_position=last+1
c
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine wav_read_arc_headers
c
c  read all headers in archive as defined in SEISAN.DEF 
c
      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'waveform.inc'
      integer i,j,ichan,k,kk
      integer terror             ! time error
      integer year,month,day,hour,min,isec
      integer seiclen
      double precision arc_time ! abs time of start of request
      real sec

c
c     return if no archive
c
      if(arc_nchan.eq.0) then
        write(6,*) 'No archive defined'
        return
      endif
c----------------------------------------------------------------------
c   section for reading all headers
c----------------------------------------------------------------------
c
c
c
c   loop trough all channel headers to get channel info
c
      ichan=0
c
      do i=1,arc_nchan   ! count channels with data
         read(cwav_start_time,'(i4,5i2)') year,month,day,hour,min,isec
         sec=isec
         call TIMSEC (YEAR,month,DAY,hour,1,0.0,arc_time)
c
c   check if archive channel active for this time, if not
c   jump check of data in archive
c
c         write(*,*) ' debug lo yes ',arc_time
c commented out 11 Nov 2017
c         if(arc_time.lt.arc_start(i).or.arc_time.gt.arc_stop(i)) 
c     *   goto 200
         if(arc_time.lt.arc_start(i).and.arc_time.gt.arc_stop(i)) 
     *   goto 200
c         write(*,*) ' debug lo no '

         ichan=ichan+1    ! count channels with data
c 
         kk=cont_interval
c        write(6,*) 'cwav1 arch',year,month,day,hour,min,sec,kk
c        write(6,*) arc_archive,arc_stat(i)
         j=seiclen(arc_archive)
         call  getarchinfo(arc_archive,j,arc_type
     *   ,arc_stat(i),5,arc_comp(i),3,
     *   arc_net(i),2,arc_loc(i),2,
     *   year,month,day,hour,min,sec,kk,
     *   wav_nsamp(ichan),wav_rate(ichan),terror,wav_year(ichan),
     *   wav_month(ichan),wav_day(ichan),wav_hour(ichan),
     *   wav_min(ichan),wav_sec(ichan))
c        write(6,*)wav_nsamp(ichan)
c
         wav_stat(ichan)=arc_stat(i)
         wav_comp(ichan)(1:2)=arc_comp(i)(1:2)
         wav_comp(ichan)(3:3)=' '
         wav_comp(ichan)(4:4)=arc_comp(i)(3:3)
         wav_network(ichan)=arc_net(i)
         wav_location(ichan)=arc_loc(i)
         wav_time_error(ichan)=' '
c         write(*,*) ' debug lo stat ',wav_stat(ichan)
c
c  if no data available, wav_nsamp is zero, channel not returned
c
         if(wav_nsamp(ichan).eq.0) then
            write(6,'(a,a5,1x,a3,1x,a2,1x,a2,1x)') 'no data for ',  
     *      arc_stat(i),arc_comp(i),arc_net(i),arc_loc(i)
            ichan=ichan-1
         else
            call timsec(wav_year(ichan),wav_month(ichan),
     *      wav_day(ichan),
     *      wav_hour(ichan),wav_min(ichan),wav_sec(ichan),
     *      wav_abs_time(ichan))
            if(wav_nsamp(ichan).ge.max_sample) then
                 write(6,*) ' Warning: Too many samples in trace ',
     &            wav_nsamp(k),max_sample
c                 write(6,*) ' Enter to stop'
c                 read(5,'(a)') k
c                 stop
            endif
            wav_cbyte(ichan)='4'
            wav_duration(ichan)=wav_nsamp(ichan)/wav_rate(ichan)  ! jh aug 2008
         endif
 200     continue        ! get here if channel not active
      enddo
      wav_nchan=ichan    ! save number of channels
c
c   find earliest and latest channels
c
      call wav_index_total 
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        

      subroutine wav_read_arc_one_header(ichan,filename)
c
c  read one header in a in archive, info in filename 
c
      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'waveform.inc'
      integer i,j,ichan,k,interval
      integer terror             ! time error
      character*80 filename      ! variable with arc info
c  arc info
      character*5 stat
      character*3 comp
      character*2 net
      character*2 loc
      integer seiclen
      integer year,month,day,hour,min,isec
      real sec

c
c     return if no archive
c
      if(arc_nchan.eq.0) then
        write(6,*) 'No archive defined'
        return
      endif
c
c  advance channel one
c
      ichan=ichan+1
c
c   read info from filername variable
c
      read(filename,'(3x,1x,a5,1x,a3,1x,a2,1x,a2,1x,i4,1x,2i2,
     *1x,2i2,1x,i2,i6)') 
     *stat,comp,net,loc,year,month,day,hour,min,isec,interval
c
c        write(6,*) year,month,day,hour,min,isec,interval
c        write(6,*) arc_archive,arc_stat(i),
c    *   arc_comp(i),arc_net(i),arc_loc(i)
         j=seiclen(arc_archive)
         sec=isec
         call  getarchinfo(arc_archive,j,arc_type
     *   ,stat,5,comp,3,
     *   net,2,loc,2,
     *   year,month,day,hour,min,sec,interval,
     *   wav_nsamp(ichan),wav_rate(ichan),terror,wav_year(ichan),
     *   wav_month(ichan),wav_day(ichan),wav_hour(ichan),
     *   wav_min(ichan),wav_sec(ichan))
c        write(6,*) 'arc sec',wav_sec(ichan)
c        write(6,*)wav_nsamp(ichan)
c
         wav_stat(ichan)=stat
         wav_comp(ichan)(1:2)=comp(1:2)
         wav_comp(ichan)(3:3)=' '
         wav_comp(ichan)(4:4)=comp(3:3)
         wav_network(ichan)=net
         wav_location(ichan)=loc
         wav_time_error(ichan)=' '
c
c  if no data available, wav_nsamp is zero, channel not returned
c
         if(wav_nsamp(ichan).eq.0) then
            write(6,'(a,a5,1x,a3,1x,a2,1x,a2,1x)') 'no data for ',  
     *      stat,comp,net,loc
            ichan=ichan-1
         else
            call timsec(wav_year(ichan),wav_month(ichan),
     *      wav_day(ichan),
     *      wav_hour(ichan),wav_min(ichan),wav_sec(ichan),
     *      wav_abs_time(ichan))
            if(wav_nsamp(ichan).ge.max_sample) then
                 write(6,*) ' Warning: Too many samples in trace ',
     &            wav_nsamp(k),max_sample
c                 write(6,*) ' Enter to stop'
c                 read(5,'(a)') k
c                 stop
            endif
            wav_cbyte(ichan)='4'
            wav_duration(ichan)=wav_nsamp(ichan)/wav_rate(ichan)  ! jh aug 2008
         endif
c
c   same new number of channels
c
         wav_nchan=ichan
c
      return
      end
        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c  helmberger format

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine detect_helm(file,format)
c
c  detect if helmberg file
c
      implicit none
      character*80 file
      character*10 format
      character*80 text
      integer i
c     
      format=' '
      open(77,file=file,status='old')
      read(77,'(a)') text
      read(text,'(i8)',err=10) i
c      if(text(20:80).ne.' ') goto 10
      read(77,'(a)') text
      if(text(1:1).ne.'(') goto 10
      read(77,'(a)') text
      read(77,'(i8)',err=10) i

c
c   format is helberger
c
      format(1:7)='HELMBER'
      close(77)
      return

 10   continue     
      close(77)      
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_header_helm(ifile)
c
c   read modified  hemlberger asciii format modeling type files
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      character*80 file        ! file to read from
      integer ifile            ! file number to read from
      character*8 format
      integer nsamp            ! number of samples
      real dt                  ! sample interval
      integer kchan            ! channels in file
      character*80 text
      integer i,k,m

      
      file=wav_filename(ifile)
      open(77,file=file,status='old')
      read(77,'(i8)') kchan
      read(77,'(a,1x,a)') format,wav_header_text(1)
  
c
      i=wav_nchan   ! start from where the counting was before
c
      do m=1,kchan
         i=i+1

         read(77,'(a)') text
c
c  here it should have been wav_nsamp(i), but it does not work, so it is nsamp 
c  instead ????
c
         read(77,
     *   '(i8,f10.3,13x,a5,1x,a4,1x,i4,1x,2i2,1x,2i2,1x,f7.3)')
     *   nsamp,dt,wav_stat(i),wav_comp(i),wav_year(i),
     *   wav_month(i),
     *   wav_day(i),wav_hour(i),wav_min(i),wav_sec(i)
c         write(6,*)'m,i,stat',m,i,wav_stat(i)
cxx
c
c  check if synthetic, then overlay
c
         if(wav_comp(i)(1:2).eq.'SY') wav_overlay(i)=.true.
c
c   if no channel info, put in def 
c

         if(wav_stat(i)(1:3).eq.'   ') then
            wav_stat(i)(1:3)='CHA'
            write(wav_stat(i)(4:5),'(i2)') i
            if(wav_stat(i)(4:4).eq.' ') wav_stat(i)(4:4)='0'
         endif
         if(wav_comp(i).eq.' ') then
             wav_comp(i)='BB B'
         endif
         read(77,format)(signal1(k),k=1,nsamp)
c
c   if no time, put in 2000
c
         if(wav_year(i).eq.0) then
            wav_year(i)=2000
            wav_month(i)=1
            wav_day(i)=1
            wav_hour(i)=1
            wav_min(i)=0
            wav_sec(i)=0.0
         endif

         wav_nsamp(i)=nsamp
         wav_rate(i)=1/dt

c         write(17,*) i,wav_nsamp(i),wav_rate(i)

         call timsec(wav_year(i),wav_month(i),wav_day(i),
     *   wav_hour(i),wav_min(i),wav_sec(i),wav_abs_time(i))
         wav_duration(i)=(wav_nsamp(i))/wav_rate(i)
cxx        write(6,*) i,wav_stat(i),wav_comp(i),wav_year(i),wav_nsamp(i) 
          
         wav_chan_nr_file(i)=m
         wav_file_nr_chan(i)=ifile
      enddo

      close(77)

      wav_nchan=wav_nchan+kchan
c      write(6,*) kchan,wav_nchan

      return
      end



      subroutine read_chan_helm(ichan)
c
c   read channel ichan from helmberger
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      character*80 text
      character*8 format
      integer ichan  ! channel to read
      integer i,k,nsamp
c      
      open(77,file=wav_filename(wav_file_nr_chan(ichan)),status='old')
c 
      read(77,'(a)') text
      read(77,'(a)') format
      do i=1,wav_chan_nr_file(ichan)
         read(77,'(a)') text
         read(77,'(i8)') nsamp    
c
c   nsamp might be different from wav_nsamp(ichan) which is nsamp for last 
c   channel read
c
         read(77,format)(signal1(k),k=1,nsamp)
      enddo
      close(77)
      return  
      end

      subroutine write_chan_helm(ichan,nchan,nfile,npoints,sfile)
c
c
c   write a one channel of a helmberger file, the data is in
c   signal1
c
c
c     ichan	channel to write as indexed in wav block
c     nchan	number of channels in file, if positive, write main header
c               if negative, assume header is written and only write channel
c     nfile	file unit to write to
c     npoints	number fo points to write from start of signal1
c     sfile	optinal, sfile name
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      character*80 text
      character*8 format
      integer ichan  ! channel to write
      integer nchan,nfile,npoints
      character*(*) sfile
      integer i,k
   
      
c
c  write first main header if first time for file
c
      format='(7e14.5)'
      if(nchan.ge.1) then
         write(nfile,'(i8,1x,a)') nchan,sfile
         write(nfile,'(a,1x,a)') format,wav_header_text(1)(1:28)
      endif

c
c   write data channel header
c
      write(nfile,'(a)') 
     *'     0.0000e+00     0.0000e+00      0  0  0.00'
      write(nfile,
     *'(i8,f10.3,a,1x,a5,1x,a4,1x,i4,1x,2i2,1x,2i2,1x,f7.3)')
     *npoints,1.0/wav_rate(ichan),
     *'  0.0000e+00',wav_stat(ichan),wav_comp(ichan), 
     *wav_year(ichan),wav_month(ichan),wav_day(ichan),
     *wav_hour(ichan),wav_min(ichan),wav_sec(ichan)
c
c   write data
c
      write(nfile,format) (signal1(i),i=1,npoints)


      return  
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine write_chan_helm_out(ichan,nchan,nfile,npoints,sfile)
c
c
c   write a one channel of a helmberger file, the data is in
c   signal1 and parameter in out array
c
c
c     ichan	channel to write as indexed in wav block
c     nchan	number of channels in file, if positive, write main header
c               if negative, assume header is written and only write channel
c     nfile	file unit to write to
c     npoints	number fo points to write from start of signal1
c     sfile	optinal, sfile name
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      character*80 text
      character*8 format
      integer ichan  ! channel to write
      integer nchan,nfile,npoints
      character*(*) sfile
      integer i,k
   
      
c
c  write first main header if first time for file
c
      format='(7e14.5)'
      if(nchan.ge.1) then
         write(nfile,'(i8,1x,a)') nchan,sfile
         write(nfile,'(a,1x,a)') format,wav_header_text(1)(1:28)
      endif

c
c   write data channel header
c
      write(nfile,'(a)') 
     *'     0.0000e+00     0.0000e+00      0  0  0.00'
      write(nfile,
     *'(i8,f10.3,a,1x,a5,1x,a4,1x,i4,1x,2i2,1x,2i2,1x,f7.3)')
     *npoints,1.0/wav_rate(ichan),
     *'  0.0000e+00',wav_out_stat(ichan),wav_out_comp(ichan), 
     *wav_out_year(ichan),wav_out_month(ichan),wav_out_day(ichan),
     *wav_out_hour(ichan),wav_out_min(ichan),wav_out_sec(ichan)
c
c   write data
c
      write(nfile,format) (signal1(i),i=1,npoints)


      return  
      end

