      SUBROUTINE SEISINC
     *(UNIT,ICH,NCHAN,MODE,MAINHEAD,TRACEHEAD,START,WINDOW)
c
C                                                                               
C   READS ONE TRACE FROM SEISAN FILE, OUTPUT TRACE IN X                        
C   AS REALS. UNIT IS UNIT TO READ FROM, ICH IS CHANNEL                        
C   WANTED, NCHAN IS NUMBER OF CHANNLES AVAILABLE.                              
C   MAINHEAD AND TRACEHEAD ARE SEISAN HEADERS
C   START IS START TIME RELATIVE TO MAIN HEADER AND WINDOW IS WINDOW
C   IN SECS. IF BOTH ZERO, USE WHOLE TRACE.                                  
C   IF A CHANNEL IS ASKED FOR WHICH IS NOT THERE,  a stop command is issued             
c
c   The file must be opened with direct access and record lenght of 2048
c
c   If rotate is set, and both horizontal channels are there, R is 
c   returned instead of N and T instead of E. The rotation angle is in
c   backazimuth.
c                                                                               
C   mode: 0: read main header only                                                 
c         2: read channel only
c         3: read channel header only                                                  
c                                                                               
C                                                                               
C    J. HAVSKOV MAR 91                                                          
C                                                                               
c    updates
c    feb 1 92 by j.h.: add 4 byte integer option
c    aug 93          : fix up
c    sep 16          : check for neg or zero number of samples
c    sep 27          : reduce more if maxdim approached
c    dec 93          : also use start tiem and window, fix output if too many
c    jun 94          : 999 channels
C!JAB(BGS)Dec94      : Install File & error handlers..
c    mar 95          : 3 component rotation
c    apr 28, 94      : az bug
c    oct 95          : new direct access routine
c    nov 20          : change comparison old-new
c    dec 13          : compare 250 chars instead of 80, failed with 80
c    feb 11 97       : in buf_read, check of enough channels moved down so no
c                      stop when mode =0
c    dec 9           : take gain constant into new consideration
c    mar 24 98       : reset old_chan if rotate
c    sep 98 by jh    : --------------   version 7.0 check ---------------------
c                      correct century, change for 5 character stations
c    oct 23          : bug when using rotation and 30 channels
c    nov2      jh    : add linux capability
c    feb 4  99 jh    : bug with correction of oct 23
c    feb 28          : read gain constant from 148:159 instad of 149:160
c                      there was a conflict with signalling in mulplt
c    sep 24 99       : a wrong eof casused crash sometimes
c    sep 25 99       : stop if not a seisan file
c    april 8 2000    : enable to read channel header only
c    nov 17 2004 jh  : buffer not dimensioned to enough channels
c    june 10 2008 jh : enable reading of 64 bit write, only tested on linux
c    2010-04-29 pv   : fixed length of data read at the end of each trace to match the true size.
c    may 10 2010 jh  : fixed gfortran end of file problem. For each channel
c                      file size is obtained and in last read, it can then
c                      be calculated how much data should be read. not used
c                      on pc and solaris
c    jun 14 2012 jh  : put peters fix also when readign old pc files on
c                      linux
c    dec 19 2012 jh  : seems there was a bug in above
c    2015-06-02 pv   : add signal_int to signalx common block due to
c                      compiler warning
c
      implicit none
      save
c-- array dimentions
c
      include 'seidim.inc'              ! array dimentions
c-- MAIN HEADERS                                
      CHARACTER*80 	MAINHEAD(*)		
c-- SINGLE TRACE HEADER                           
      CHARACTER*1040	TRACEHEAD		
      character*35 chead_first          ! part of trace head used with rotation
c-- FILE UNIT TO READ FROM                                  
      INTEGER		UNIT			
c-- NUMBER OF CHANNELS                             
      INTEGER		NCHAN,nchannel		
c-- CHANNEL CHOSEN                                  
      INTEGER           ICH			
c-- operating mode                                          
      integer		mode			
c-- ALL DATA, one channel                                      
      REAL		X(max_sample)			
      real      y(max_sample)   ! work space used when rotating
      integer signal_int(max_sample)
      character*5 stat(max_trace)  ! stations
      character*4 comp(max_trace)  ! components
      character*5 station       ! station to look for
      real        del( max_trace) ! delay each trace
      real        dur( max_trace) ! duration each trace
      real        del_rot       ! delay rotated trace
      real        dur_rot       ! duration --------
      integer     nstart1,nstart2,nstop1,nstop2 ! start and stop point each ch.
      character*4 other_component  ! other horizontal component
      integer other_chan        ! channel number of other hozontal channel
c-- NUMBER OF SAMPLES                                      
      INTEGER		NSAMP	
c-- time of main header and channel header
      integer myear,mmonth,mday,mhour,mmin,cyear,
     *cmonth,cday,chour,cmin,doy
      real msec,csec
      double precision mtime,ctime
c-- start and window
      real start,window
      integer nstart,nstop
c-- sample rate and delay
      real rate,delay
c-- for component rotation
cx      real caz,saz
cx      real baz(max_trace)
cx      character*1 rot_comp(max_trace)  ! T or R
cx      logical rotate
c-- COUNTERS                                          
      INTEGER		I,k,j
      integer           file_status(13)  ! info on file
      integer           file_size        ! size of file
c
cx      common /signal/x,rotate,baz,rot_comp,del_rot
c     common /signalx/x
      common /signalx/x,signal_int
C
C    Seisan library inserts and routines...
C    ======================================
C
       include  'libsei.inc'               ! Library definitions & data defns.
       external  sei code                  ! Error encoder.
C
C    ============= end of list ==========
c
c
c
c   get size of file
c
       call fstat(unit,file_status,i)
       file_size=file_status(8)
c      write(6,*)'File size',file_size
c
       other_chan=0    ! initially only read one channel
cx       del_rot=-9999.9 ! no delay due to rotation initially
c
c  read data
c
      call buf_read(unit,ich,nchannel,mode,mainhead,tracehead,file_size)
c     write(6,*) ich,nchannel
c                                                                               
c   return nchan and save nchannel                                              
c                                                                               
      nchan=nchannel                                                            
c                                                                               
c   return if only main header read                                             
c                                                                               
      if(mode.eq.0) return                                                      
c
c   read number of samples
c
      READ(TRACEHEAD,'(43X,I7)') NSAMP
c
c---------------------------------------------------------------------
c   check if a time window is selected                                                              
c---------------------------------------------------------------------
c                                    
      if(window.ne.0.0.and.start.ne.0.0) then
c
c   get header times
c
         read(mainhead(1)(34:59),
     *  '(i3,1x,i3,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3)')
     *   myear,doy,mmonth,mday,mhour,mmin,msec                        
         myear=myear+1900
         call timsec(myear,mmonth,mday,mhour,mmin,msec,mtime)                      
         read(tracehead(10:35),
     *   '(i3,1x,i3,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3)')
     *   cyear,doy,cmonth,cday,chour,cmin,csec
         cyear=cyear+1900      ! correct for missing century                                           
         call timsec(cyear,cmonth,cday,chour,cmin,csec,ctime)                      
         read(tracehead(37:43),'(f7.3)') rate
c
c   calculate samples to extract, first check if start point available
c
         delay=ctime-mtime
         if(delay.gt.start) then
            write(6,*)' Data start not available'
            goto 800                         
         endif

         nstart=(start-delay)*rate+1
         nstop=nstart+window*rate
c
c   check if enough data
c
         if(nstop.ge.nsamp) then
            write(6,*)' Too much data required, beyound end of trace'
            goto 800
         endif
c
         nsamp=window*rate+1
c
c   select data
c
         do i=1,nsamp
           x(i)=x(i+nstart-1)
         enddo        
         goto 900
c
c   put in dummy samples if somthing is wrong
c
 800  continue
         nsamp=3                         
         x(1)=1.0
         x(2)=1.001
         x(3)=1.01
 900  continue
c
c   update trace head with samples written in output array
c
         write(tracehead(44:50),'(i7)') nsamp
      endif
cx      if(.not.rotate) rot_comp(ich)=tracehead(9:9)
c
c--------------------------------------------------------------------
c  check if rotation
c--------------------------------------------------------------------
c
cx      if(.not.rotate) return
c
c   check if a horizontal component
c
cx      if(tracehead(9:9).ne.'N'.and.tracehead(9:9).ne.'E') return
c 
c   check if angle is available, if not, return
c
cx      if(baz(ich).eq.999.0) return
c
c   look for availability of other channel
c
cx      other_component=tracehead(6:9)
cx      station=tracehead(1:5)
cx      if(tracehead(9:9).eq.'N') then 
cx         other_component(4:4)='E'
cx      else
cx         other_component(4:4)='N'
cx      endif
cxc
c   read header info
c
cx      j=1
cx      do i= 3,(nchannel-1)/3+3
cx         read(mainhead(i),100)
cx     *   stat(j)  (1:4),comp(j),  stat(j)  (5:5),del(j),  dur(j),
cx     *   stat(j+1)(1:4),comp(j+1),stat(j+1)(5:5),del(j+1),dur(j+1),
cx     *   stat(j+2)(1:4),comp(j+2),stat(j+2)(5:5),del(j+2),dur(j+2)
cx100      format(1x,a4,a4,a1,f7.2,1x,f8.2,1x,a4,a4,
cx     *   a1,f7.2,1x,f8.2,1x,a4,a4,a1,f7.2,1x,f8.2)
cx         j=j+3
cx      enddo
cxc
c   find if channel is there
c
cx      other_chan=0
cx      do i=1,nchan
cx         if(station.eq.stat(i).and.other_component.eq.comp(i))
cx     *   other_chan=i
cx      enddo
c
c   return if channel not found, the horizontal channel remain unchanged
c
cx      if(other_chan.eq.0) return
cx      nstart1=1
cx      nstart2=1
cx      read(tracehead(37:43),'(f7.3)') rate  ! assume same s. rate N and E
cx      nstop1=dur(ich)*rate+1    
cx      nstop2=dur(other_chan)*rate+1
c
c   check if both channels have the same start time and duration
c
cx      if(dur(other_chan).ne.dur(ich).or.del(other_chan).ne.del(ich)) 
cx     *then                     ! channels different, must be corrected
c
c   find common start time, first and last point in each channel
c   if other chan starts later, use that for header time, if current
c   channel start later, save that time and put into header for rotated
c   channel after rotation
c
cx         if(del(other_chan).gt.del(ich)) then
cx            chead_first='                                   ' ! not start time
cx            del_rot=del(other_chan)
cx            nstart1=(del(other_chan)-del(ich))*rate+1
cx         else
cx            del_rot=del(ich)
cx            chead_first=tracehead(1:35)   ! save rotated channel start time
cx            nstart2=(del(ich)-del(other_chan))*rate+1
cx         endif
c
c   find end points
c
cx         if((dur(ich)+del(ich)).gt.(dur(other_chan)+del(other_chan)))
cx     *   then
cx            nstop1=(dur(ich)-
cx     *      ((dur(ich)+del(ich))-(dur(other_chan)+del(other_chan))))*
cx     *      rate
cx         else
cx            nstop2=(dur(other_chan)-
cx     *      ((dur(other_chan)+del(other_chan))-(dur(ich)+del(ich))))*
cx     *      rate
cx          endif
cx          dur_rot=float(nstop1-nstart1-1)/rate
cx      endif
c
c   save first channel
c
cx      k=1
cx      do i=nstart1,nstop1
cx         y(k)=x(i)
cx         k=k+1
cx      enddo
c
c   read second horizontal channel 
c
cx      call buf_read(unit,other_chan,nchannel,mode,mainhead,tracehead)
c
c   check if both channels have the same start time and duration
c   if not, select appropriate window
c
cx      if(nstart1.ne.nstart2.and.nstop1.ne.nstop2) then
cx         k=1
cx         do i=nstart2,nstop2
cx            x(k)=x(i)
cx            k=k+1
cx         enddo
cx         nsamp=nstop1-nstart1+1
cxc
c   update trace head with samples written in output array
c
cx         write(tracehead(44:50),'(i7)') nsamp
c
c   get correct start time for channel
c
cx         if(chead_first(11:19).ne.'         ')
cx     *   tracehead(11:35)=chead_first(11:35)
cx      endif
c
c   do rotation, two components are in x and y, however they can be
c   either N or S, so check when rotating
c
cx      caz = -cos(baz(ich)*3.14159/180.)
cx      saz = -sin(baz(ich)*3.14159/180.)
c
c       making the radial component (away from the source)
c
cx      if(other_component(4:4).eq.'E') then
cx         do i=1,nsamp
cx           x(i) = caz*y(i) + saz*x(i)    ! y is N
cx         enddo
cx         tracehead(9:9)='R'              ! indicate different component
cx      else
c
c       making the transversal component
c
cx         do i=1,nsamp
cx            x(i) = -saz*x(i) + caz*y(i)  ! x is N
cx         enddo
cx         tracehead(9:9)='T'
cx       endif
c
c   clear other_chan so a new rotation can be made
c
cx       other_chan=0
cx       rot_comp(ich)=tracehead(9:9)
c

      RETURN                                                                    
      END                                                                       
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine buf_read
     *(unit,chan_wanted,nchan,mode,main_head,chan_head,file_size)
c
c   reads one channel of seisan waveform data from a binary file written
c   on sun, linux or pc, not tested on other computers
c
      implicit none
      save
      include 'seidim.inc'
      integer unit                        ! unit to read from
      integer mode                        ! see seisinc
      character*29000 buffer              ! buffer when reading
      character*250   old_buffer          ! first chars of old file
      character*80 main_head(*)
      character*1040 chan_head            ! channel header
      character*3 computer                ! computer where file was written
      integer rec_size,size               ! number of bytes in one read
      character*4 swap4                   ! used for swapping 4 byte data
      character*1 swap2                   ! used for swapping 2 byte data
      integer i,j,k,l,m
      integer record_to_read              ! next record to read, also current
      integer record_read                 ! last record read
      integer byte_nr_used                ! last byte nr used in file
      integer byte_nr_read                ! last byte nr read in file
      integer buf_byte_nr_read            ! last byte read into buffer
      integer buf_byte_nr_used            ! last byte used in buffer
      integer head_bytes                  ! number of bytes in main header
      integer head_lines                  ! number of lines in main header
      integer nsamp                       ! number of samples in buffer
      integer nchan                       ! number of channels in file
      integer nbuf                        ! number of buffers to read in one go
      integer chan_bytes                  ! number of bytes, one channel
      integer bytes_sample                ! number of bytes pr sample
      integer byte_nr_chan(max_trace+1)   ! byte nr of start of channel
      integer chan_current                ! current channel number
      integer old_chan                    ! previous channel number
      integer chan_wanted                 ! channel nr wanted
      integer khead                       ! number of bytes in block separater
      logical bit64                       ! if true, a 64 bit system
      character *1 data_byte(max_sample*4) ! data as characters
      integer*2    data2(max_sample*2)     ! data as 2 byte integers
      integer*4    data4(max_sample)       ! data as 4 byte integers
      real gain                            ! gain factor to get real sample values
      integer           end_byte         ! last byte to read in one buffer
      integer  file_size                   ! file size in bytes
      real x(max_sample)                   ! data as reals
      integer signal_int(max_sample)
      logical sun,linux,pc                 ! current computer type
c-- for component rotation, not used in this routine
cx      real baz(max_trace)
cx      real        del_rot       ! delay rotated trace
cx      character*1 rot_comp(max_trace)  ! T or R
cx      logical rotate
c
      equivalence (data2,data4)
      equivalence (data_byte,data2)
c     equivalence (data4,x)
cx      common /signal/x,rotate,baz,rot_comp,del_rot
c     common /signalx/x ! lo 18/03/2010
      common /signalx/x,signal_int
c      common /signalx/x,data4
c
c   get computer type
c
      call computer_type(sun,pc,linux)
c
c   set record size, at least 2048
c
      rec_size=2048
      record_to_read=1
c
c   read first record
c
      read(unit,rec=record_to_read,err=10) buffer(1:rec_size)
c
c   determine where file was written
c
      computer='   '
      bit64=.false.
      khead=4
c
      if(buffer(1:2).eq.'KP')  computer='pc '
      if(buffer(1:1).eq.'P')   computer='lin'
      if(buffer(4:4).eq.'P')   computer='sun'
      if((buffer(89:89).eq.'P'.and.buffer(185:185).eq.'P'.and.
     *   buffer(281:281).eq.'P').or. 
     *  (buffer(96:96).eq.'P'.and.buffer(192:192).eq.'P'.and.
     *   buffer(288:288).eq.'P')) then
         bit64=.true.
         khead=8
      endif
      if(computer.eq.'   ') then
         write(6,*)
     *  ' The input waveform file does not have SEISAN format'
         write(6,*)' Return to stop'
         read(5,'(a)') computer
         stop
      endif
c
c   check if file already has been used by comparing old and new buffer.
c   for some reason, the non initialized
c   text string old_buffer will compare equal to the read text string
c   buffer, so the strings cannot be compared and the sum of the ascii
c   values are used instead
c
      k=0
      l=0
c     do i=1,80
c        k=k+ichar(old_buffer(i:i))
c     enddo
c     do i=1,80
c        l=l+ichar(buffer(i:i))
c     enddo
c       if(k.eq.l) then
       if(old_buffer.eq.buffer(1:250)) then
c          write(6,*)'old buffer'
c                                                                               
c   return if same channel is selected as privious time unless rotate is set
c   in which case it might have to be read in again                         
c                                                                               
cx          if(chan_wanted.eq.old_chan.and.mode.eq.2.and.(.not.rotate)) 
          if(chan_wanted.eq.old_chan.and.mode.eq.2) 
     *    return
          if(mode.eq.0) return             ! header already read
c
c   find until which channel file already read
c
         do i=1,chan_wanted
            if(byte_nr_chan(i).eq.-1) then
                chan_current=i-2          ! one added below, so -2
                goto 1                    ! wanted channel not read
            endif
         enddo
         chan_current=chan_wanted-1       ! subtract one since one added below
         goto 1                           ! wanted channel already read
      else
c          write(6,*) 'NEW BUFFER'
         do i=1,max_trace+1
            byte_nr_chan(i)=-1            ! initialize pointer
            old_chan=-1                   ! no old chan
         enddo
      endif   
      old_buffer=buffer(1:250)
c      write(6,'(a,a)') ' File written on ',computer
c
c  get number of channels to decide if more buffers must be read
c  to get complete main header
c
      if(computer.eq.'sun'.or.computer.eq.'lin'.and..not.bit64)
     * read(buffer,'(34x,i3)') nchan
      if((computer.eq.'sun'.or.computer.eq.'lin').and.bit64) 
     * read(buffer,'(38x,i3)') nchan
      if(computer.eq.'pc ') read(buffer,'(32x,i3)') nchan

c      write(6,*) 'nchan',nchan    ! test out
c
c   calculate how many buffers are needed to read all main headers
c
         buf_byte_nr_read=rec_size
         if(nchan.gt.30) then
            head_lines=(nchan-31)/3+13
         else
            head_lines=12
         endif
            if(computer.eq.'pc ') head_bytes=head_lines*82+2
            if(computer.eq.'sun'.or.computer.eq.'lin') 
c64  *        head_bytes=head_lines*88
     *        head_bytes=head_lines*(80+2*khead)
            nbuf=(head_bytes-1)/rec_size+1
            k=rec_size+1
            if(nbuf.gt.1) then
               do record_to_read=2,nbuf
                  read(unit,rec=record_to_read) buffer(k:k+rec_size-1)
                  k=k+rec_size
               enddo
c               buf_byte_nr_read=k-1
               record_to_read=nbuf
            endif
c        endif
         buf_byte_nr_read=record_to_read*rec_size
c
c   put in main header records in variables by stripping away record
c   separators, different on sun and pc and 64 bit systems
c
      if(computer.eq.'sun'.or.computer.eq.'lin') then
c64     k=5
        k=khead+1
        j=k+79
        do i=1,head_lines
           main_head(i)=buffer(k:j)
           buf_byte_nr_used=j
c64        k=k+88
           k=k+80+2*khead
           j=k+79
        enddo
      endif
      if(computer.eq.'pc ') then
        k=3
        j=k+79
        do i=1,head_lines
           main_head(i)=buffer(k:j)
           buf_byte_nr_used=j
           k=k+82
           j=k+79
        enddo
      endif
c     do i=1,head_lines
c       do j=1,80
c         if(ichar(main_head(i)(j:j)).eq.0) main_head(i)(j:j)=' '
c       enddo
c     enddo
c
c   calculate byte nr of first byte in first channel header
c
      byte_nr_chan(1)=buf_byte_nr_used+1
    
c      do i=1,head_lines
c        write(6,'(1x,a)') main_head(i)
c      enddo
c
c   return if only header is read
c
      if(mode.eq.0) return
c                                                                               
c   check if channel is available                                               
c
      if(chan_wanted.gt.nchan) then
         write(6,*)' Too high channel number demanded'
         write(6,*)' Channel wanted: ',chan_wanted
         write(6,*)' Number of channels available ',nchan
         stop
      endif                                                                     
c	  
c   read down to channel chan_wanted
c
      chan_current=0
c
c--------------------------------------------------------------------------
c   start of loop for reading channels
c--------------------------------------------------------------------------
c
       
 1    continue                  
      chan_current=chan_current+1
c
c   calculate record to read to get channel header
c
      record_to_read=(byte_nr_chan(chan_current)-1)/rec_size+1   
c
c   read record into buffer
c
c2000      read(unit,rec=record_to_read,end=60,err=10) buffer(1:rec_size)
c      size=1040+2*khead
c      read(unit,rec=record_to_read,err=60) (data_byte(j),j=1,size)
c      do j=1,size
c        buffer(j:j)=data_byte(j)
c      enddo 
c
c following causes problems with gfortran and short channels, lo
c
c
c   check if read after end of file, file size in bytes is in file_size
c   on pc and solaris not used, always 0
c
      if((record_to_read)*rec_size.gt.file_size.and.file_size.gt.0) 
     *then
         end_byte=file_size-(record_to_read-1)*rec_size
         read(unit,rec=record_to_read,err=60) buffer(1:end_byte)
c        write(6,*)'end byte',end_byte
      else
         read(unit,rec=record_to_read,err=60) buffer(1:rec_size)
c        write(6,*)'end byte2',end_byte
      endif
c
c  allow eof here could be near end of file
c
  60  continue
      byte_nr_read=record_to_read*rec_size
c
c   calculate byte nr where header starts in buffer meaning the first
c   byte of the record separator in front of all header bytes meaning
c   both the end of the previous and the start of the next buffer
c
      k=rec_size - byte_nr_read + byte_nr_chan(chan_current)
c	  write(6,*) 'kstart,bread,bchan',k,byte_nr_read,byte_nr_chan
c
c   calculate if there are enough bytes in the buffer to get the whole
c   header, else read the next record 
c
       buf_byte_nr_read=rec_size
c64    if(rec_size-k.lt.1068) then    ! why 1068, not 1048 ??
       if(rec_size-k.lt.1060+khead*2) then
         record_to_read=record_to_read+1
c2000 pc         read(unit,rec=record_to_read,end=61,err=10) buffer(rec_size+1:
      if((record_to_read)*rec_size.gt.file_size.and.file_size.gt.0) 
     *then
         end_byte=file_size-(record_to_read-1)*rec_size
         read(unit,rec=record_to_read,err=61) buffer(rec_size+1:
     *   rec_size+end_byte)
c        write(6,*)'end byte, second block',end_byte
      else
         read(unit,rec=record_to_read,err=61) buffer(rec_size+1:
     *   rec_size+rec_size)
c        write(6,*)'end byte, second block2',end_byte
      endif
c
c   allow eof here, could be ok
c
 61      continue
	     buf_byte_nr_read=buf_byte_nr_read+rec_size

         byte_nr_read=byte_nr_read+rec_size
      endif       
c
c   fish out the channel record, 1040 bytes long
c
      if(computer.eq.'sun'.or.computer.eq.'lin') then
c64      chan_head=buffer(k+8:k+1047)    ! no separaters
         chan_head=buffer(k+khead*2:k+1039+khead*2)    ! no separaters
c64      buf_byte_nr_used=k+1047
         buf_byte_nr_used=k+1039+khead*2
      else
         j=1
         do i=1,8
           chan_head(j:j+127)=buffer(k+2:k+2+127) ! a separater every 128 bytes
           k=k+130
           j=j+128
         enddo
         chan_head(j:j+15)=buffer(k+2:k+15+2)
         buf_byte_nr_used=k+15+2
      endif

c	  k=1
c	  do i=1,13
c	    l=k+79
c        write(6,'(1x,a80)')chan_head(k:l)
c        k=k+80
c      enddo
c
c   calculate last byte used, this does not include last record separater
c
      byte_nr_used=record_to_read*rec_size-
     *             (buf_byte_nr_read-buf_byte_nr_used)

c
c  check if that was the channel wanted
c
c      write(6,*)'buf_byte_nr_used,byte_nr_used',buf_byte_nr_used,
c     *byte_nr_used
c      write(6,*)'buf_byte_nr',buf_byte_nr_read
c      write(6,*) 'channel',chan_current
c      if(chan_current.lt.chan_wanted) goto 1    ! get next channel
c
c   get number of samples and if 2 or 4 byte samples
c
       read(chan_head(44:50),'(i7)') nsamp

c
c   check for number of samples
c
         if(nsamp.gt.max_sample-100) then
            write(6,*) nsamp, 'Too many samples'
            stop
         endif
c
       if(chan_head(77:77).eq.' ') bytes_sample=2
       if(chan_head(77:77).eq.'2') bytes_sample=2
       if(chan_head(77:77).eq.'4') bytes_sample=4
        
c
c   calculate which record to read to get the start of the next channel header,
c   first calculate how many bytes chan_bytes there are in following channel
c   excluding the channel header and including record separators at start
c   and end.
c
      if(computer.eq.'sun'.or.computer.eq.'lin') then
c64     chan_bytes=nsamp*bytes_sample+8
        chan_bytes=nsamp*bytes_sample+2*khead
      else    ! pc
        chan_bytes=nsamp*bytes_sample  ! number of bytes without separator
        chan_bytes=((chan_bytes-1)/128+1)*2+chan_bytes
      endif
c
c      write(6,*)'nsamp,chan_bytes',nsamp,chan_bytes
c
c   calculate the byte number from start of file where next channel
c   starts (first byte of header meaning first byte of record separater,
c   of last preceeding record)
c
      byte_nr_chan(chan_current+1)=byte_nr_used+chan_bytes+1
      record_read=record_to_read              ! save last buffer number read
      record_to_read=(byte_nr_chan(chan_current+1)-1)/rec_size+1  ! next header

c
c   if this is the channel wanted, jump down to read all data
c
      if(chan_current.eq.chan_wanted) goto 1000
c
c   save position in file of next channel
c
c      byte_nr_chan_all(chan_current+1)=byte_nr_chan
c      record_to_read_all(chan_curent+1)=record_to_read
c
c  go to next channel
c
      goto 1                                    
c
c---------------------------------------------------------------------
c  waveform data read and possibly swapped
c---------------------------------------------------------------------
c
 1000 continue
c
c   if only header wanted, return
c
      if(mode.eq.3) return              ! april 2000
c
c   put remaining buffer data into data buffer and then read the rest
c

      if(computer.eq.'sun'.or.computer.eq.'lin') then
         k=1
c64      do i=buf_byte_nr_used+9,buf_byte_nr_read ! out with record separater
         do i=buf_byte_nr_used+khead*2+1,buf_byte_nr_read ! out with record separater
            data_byte(k)=buffer(i:i)
            k=k+1
         enddo
c         write(6,*)' bytes remaining in header buffer before w',k-1
         do i=record_read+1,record_to_read
c2000 pc            read(unit,rec=i,end=1100) (data_byte(j),j=k,k+rec_size-1)           
c pv- gfortran did not like to read beyond the length of the file at the last trace.
c pv- added this if statement to handle the problem - tested on billy:
       if(k+rec_size-1.ge.chan_bytes)then
c         write(*,*) 'debug rec_size=chan_bytes-k+1-khead'
         rec_size=chan_bytes-k+1-khead
       endif
c      write(6,*)'j,k,i,rec_size,k+rec_size-1',j,k,i,rec_size,k+rec_size-1
c      write(6,*)'nsamp,chan_bytes',nsamp,chan_bytes
            read(unit,rec=i,err=1100) (data_byte(j),j=k,k+rec_size-1)
c      write(6,*)'j',j
c        do j=k,k+rec_size-1
c      write(6,*)'j:',j
c           read(unit,rec=i,err=1100) data_byte(j)
c        enddo
            goto 1101
c
c   end of file hit if here, if the last channel, this is ok since the 
c   number of records normally does not fit the file. However all bytes are read
c   so it should be ok
c
 1100       continue
c
c   error only allowed  if last record of last channel
c
            if(i.ne.record_to_read.and.chan_current.ne.nchan) then
              write(*,*) ' error in seisinc '
              goto 10
            endif
 1101       continue
c      write(6,*)'j khead',j,khead
            k=k+rec_size    ! count total number of bytes for w. data
c            write(6,*)'k reading wav',k, 'rec',i
         enddo 
      endif      
c
c-----------------------------------------------------------------------
c  pc
c-----------------------------------------------------------------------
c
      if(computer.eq.'pc ') then
         k=1
c         write(6,*)'buf_byte_nr_used,read',buf_byte_nr_used,
c     *   buf_byte_nr_read
         do i=buf_byte_nr_used+1,buf_byte_nr_read
            data_byte(k)=buffer(i:i)
            k=k+1
         enddo
c         write(6,*)' bytes remaining in header buffer before w',k-1

         do i=record_read+1,record_to_read
c         do i=record_read+1,record_to_read-1+1  ! was like this before dec 19, 2012 jh
c
c  next 4 lines added by jh june 14, 2012 since reading did not work 
c  
c
c pv- gfortran did not like to read beyond the length of the file at the last trace.
c pv- added this if statement to handle the problem - tested on billy:
c         write(*,*) 'k,rec_size,chan_bytes,khead',
c     *    k,rec_size,chan_bytes,khead
       if(k+rec_size-1.ge.chan_bytes) then
c         write(*,*) 'k,rec_size,chan_bytes,khead',
c     *    k,rec_size,chan_bytes,khead
         rec_size=chan_bytes-k+1-khead
cxx         rec_size=chan_bytes-k+1
       endif
c2000            read(unit,rec=i,end=1102) (data_byte(j),j=k,k+rec_size-1)
            read(unit,rec=i,err=1102) (data_byte(j),j=k,k+rec_size-1)
            goto 1103
 1102       continue
c
c   error only allowed  if last record of last channel
c
            if(i.ne.record_to_read.and.chan_current.ne.nchan) 
     *      goto 10
 1103       continue
            k=k+rec_size
         enddo
c         write(6,*)(ichar(data_byte(i)),i=1,100) 
         
         

c
c   shift all bytes to remove record separaters
c
         j=0
         m=0
         do i=1,k,130
            m=m+2
            do l=1,128
               j=j+1
               m=m+1
               data_byte(j)=data_byte(m)
            enddo
         enddo
      endif
c         write(6,*)'after shift'
c         write(6,*)(ichar(data_byte(i)),i=1,100) 
c      write(6,*)' bytes read',k
c      if(bytes_sample.eq.4) write(6,*) (data4(i),i=1,10)
c      if(bytes_sample.eq.2) write(6,*) (data2(i),i=1,10)
c
c   check if byte swapping, this means that data was written on a
c   computer different from this one. Linux and pc have the same byte order
c
      if( ( pc.and.computer.eq.'sun').or.(sun.  and.computer.eq.'pc ')
     *.or.(sun.and.computer.eq.'lin').or.(linux.and.computer.eq.'sun')) 
     *then
c
c   do byte swap, 2 or 4 byte
c 
         if(bytes_sample.eq.4) then
            do i=1,nsamp
               j=(i-1)*4+1
               swap4(1:1)=data_byte(j)
               swap4(2:2)=data_byte(j+1)
               swap4(3:3)=data_byte(j+2)
               swap4(4:4)=data_byte(j+3)
               data_byte(j)  =swap4(4:4)
               data_byte(j+1)=swap4(3:3)
               data_byte(j+2)=swap4(2:2)
               data_byte(j+3)=swap4(1:1)
            enddo
          else
            do i=1,nsamp
               j=(i-1)*2+1
               swap2=data_byte(j)
               data_byte(j)  =data_byte(j+1)
               data_byte(j+1)=swap2
            enddo
          endif
      endif
 
c      if(bytes_sample.eq.4) write(6,*) (data4(i),i=1,10)
c      if(bytes_sample.eq.2) write(6,*) (data2(i),i=1,10)
c
c   put data in real array
c
      if(bytes_sample.eq.4) then
c          write(6,*)'4 bbytes'
         do i=1,nsamp
            x(i)=data4(i)
c            if(i.lt.10) write(6,*) data4(i),x(i)
         enddo
      else
c         write(6,*) '2 byrtes'
         do i=1,nsamp
            x(i)=data2(i)
c            if(i.lt.10) write(6,*) data2(i),x(i)
         enddo
       endif
c
c   check if a gain constant is used
c
       if(chan_head(76:76).eq.'G') then
          read(chan_head(148:159),'(g12.7)') gain
          do i=1,nsamp
            x(i)=x(i)*gain
          enddo
       endif     
c
c  check for zero or negative number of samples
c
       if(nsamp.le.0) then
          nsamp=3                         
          x(1)=1.0
          x(2)=1.001
          x(3)=1.01
c
c   put new number of samples in header
c
          write(chan_head(44:50),'(i7)') nsamp
      endif
      goto 20
 10   continue
      write(6,*)'error in seisinc'
 20   continue
c
c   save channel number read
c
      old_chan=chan_wanted
c
c   if reurn a rotated channel, order screwed up so reset
c
cx      if(rotate) old_chan=1
	  return
	  end
