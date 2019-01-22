c
c  converts Geosys GRS16 and GBV to SEISAN format
c  has also been tested with GSR 24, only for 3 channels, nver tested with
c  more than 3 channels so hardwired to a a max of 3, would probably
c  also work with more channels if changed
c  channels can only be defined using the grssei.def definition file
c
c  j. havskov, april 98
c
c    may 11  00    lo : use read_resp_head
c
c  Testdata and help was provided by Geosys
c
c  updates:
c  march 24, 99 by jh : read number of channels
c  march 28           : sample rate was wrong
c  may 6, 99 by jh    : -----------   version 7.0 ----------------------
c                       standard conversion program features, 5 char stat
c                       year 2000
c  may 24             : null net code
c  august 23          : use station code defined in file instead of ser. number
c  sep 22 lo          : moved routine bcd to LIB
c  feb 15 2000 jh     : if channels larger than 3, set to 3, needed to read
c                       older gsr file without number of channels given
c  sep 10 2003 jh     : adopt to 4 byte data, correct for preevent, group delay and millisecs
c  oct 22 2004 jh     : bug in above
c  aug 06      jh     : more info output, make it work with GSR24
c  mar 22 2011 jh     : do not use station code, often not set. use serial number
c                       always, not set where it ws programmed, maybe format has
c                       changed, test with data from 2004-2010
c

      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'version.inc'
c-----------------------------------------------------------------------
c   start block of definitions for most conversion programs
c
c-- main header------------------------------------------
      character*80 mainhead(max_trace)
      character*29 mainhead_text   ! text to be put into main header
c-- channel header
      character*1040 chahead
c-- output file name 
      character*80 outfile	  
c-- number of stations
      integer nchan	  
c-- channel to calculate channel header for
      integer ichan
c-- network code
      character*5 net_code	  
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)	  
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)
c--channel header date and times	  
      integer year(max_trace),month(max_trace),day(max_trace),
     *hour(max_trace),min(max_trace),doy
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)
      double precision total_time	  
c
c--end block of definitions most conversion programs
c---------------------------------------------------------------------

c-- output data vector	  
      integer*2  data(max_sample)
      integer*4  data4(max_sample)
      character*80 text   ! general text
      character*512 block  ! input file header
      integer*2 i_rate
      character*2 c_rate
      character*2 c_group_delay
      integer*2 group_delay
      character*2 c_millisec
      integer*2   millisec
      
c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question
c-- 2 or 4 byte
      logical four_byte	  
c--Counters etc
	  integer i,irec
c-- computer type
      logical pc,sun,linux
      integer seiclen	  
c---------------------------
c-- help for swap
      character*4 xs
      integer ixs
      integer i_samp    ! number of samples
      character*4 c_samp! ----------------
      integer*2 pre_event  ! preevent time in seconds
      character*2 c_pre_event
      real time_correction
      integer*2 nch               ! number of channels
      character*2 c_nch
      character*5 s_number        ! seriel number of current station
c
      equivalence(i_rate,c_rate)
      equivalence (c_samp,i_samp)
      equivalence (ixs,xs)
      equivalence (c_nch,nch)
      equivalence (c_group_delay,group_delay)
      equivalence (c_millisec,millisec)
      equivalence (c_pre_event,pre_event)
c

c
c  by default, 2 byte data
c

      four_byte=.false.
c
c print version
c

      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   get def file for station codes, give file name
c
       text='gsrsei.def'
       net_code=' '
       call read_def_chan(text,mainhead_text,net_code)

c
      call computer_type(sun,pc,linux)
c
c   get file name
c
      in=0
      question=' File name, # or filenr.lis for all'
      call filename(question,infile)	    
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis'.or.
     *infile(1:10).eq.'FILENR.LIS') then
         open(8,file='filenr.lis',status='old',err=20)
         goto 21
 20      continue
         write(6,*)' No filenr.lis'
         stop
 21      continue
         in=1
      endif
c
c   file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') infile(1:seiclen(infile))
c
c  open file 
c
       open(1,file=infile,status='old',access='direct',recl=512)
c
c   read header block to get start time etc
c
c
 1     continue
       read(1,rec=1,err=99) block
       close(1)
c
c   open again with record length 2 to read samples later,
c   thsi is for GBV
c
       
      open(1,file=infile,status='old',access='direct',recl=2)

c
c   write instrument type
c
      write(6,'(1x,a,a)') 'Instrument type  ',block(106:115)
c
c   find byte lenght of data from instrument type
c
      if(block(106:111).eq.'GSR-24'.or.
     *   block(106:111).eq.'GSR-18'.or.
     *   block(106:112).eq.'GBD-118')      four_byte=.true.
 
      if(four_byte) then
        write(6,*) '4 byte data'
      else
        write(6,*) '2 byte data'
      endif

c
c   number of channels
c
      if(four_byte) then
         c_nch(1:1)=block(308:308)
         c_nch(2:2)=char(0) 
      else
         c_nch(1:1)=block(22:22)
         c_nch(2:2)=char(0) 
      endif
      if(sun) call swap2_one(nch)
c
      write(6,*)'Number of channels', nch
c
c   number of samples
c
       c_samp=block(9:12)
c
c   seriel number
c
c       s_number(1:3)=block(116:118)   ! maybe it was here in older geosig
       s_number(1:4)=block(133:136)    ! now it seems to be here (2010)
       s_number(5:5)=' '

c       do i=1,280
c          write(6,'(i3,1x,a1)') i, block(i:i)
c       enddo
c
c  replace garbage chars with blank
c
       do i=1,5
         if(ichar(s_number(i:i)).lt.48.or.
     *      ichar(s_number(i:i)).gt.57) s_number(i:i)=' '
       enddo
c
c    code for sample rate
c
          i_rate=200   ! default rate
          call bcd(block(121:121),i)
          if(i.eq.2) i_rate=100
          if(i.eq.1) i_rate=50
          if(i.eq.0) i_rate=25
          write(6,*) 'Sampling code and sample rate',i,i_rate

c
c   swap a few numbers if on sun
c
       if(sun) then
          call swap4_one(i_samp)
       endif
c
c   number of channels
c
       nchan=nch
       if(nchan.gt.3) nchan=3  ! in older files, nchan is always 3
       if(nchan.eq.0) nchan=3  !if read wrong, assume 3 
c
c   get trigger time
c
c   seconds
       call bcd(block(35:35),i)
       sec(1)=i   ! whole sec
       call bcd(block(36:36),i)
       sec(1)=sec(1)+i/100.0   ! hundreds of seconds
c   minutes
       call bcd(block(34:34),min(1))
c   hour
       call bcd(block(33:33),hour(1))
c   day
       call bcd(block(37:37),day(1))
c   month
       call bcd(block(38:38),month(1))
c   year
       call bcd(block(40:40),year(1))
       if(year(1).gt.50) year(1)=year(1)+1900
       if(year(1).le.50) year(1)=year(1)+2000
       write(6,'(a,1x,i4,1x,4i3,1x,f6.3)')' yy,mm,dd,hh,mm,sec',
     * year(1),month(1),day(1),hour(1),min(1),sec(1)
c
c   initially use seriel numbers padded with 0 for station codes, 
       
       write(6,'(1x,a,1x,a)') 'Seriel number',s_number
       write(6,*) 'Number of samples',i_samp
       stat(1)=s_number
c
c   if station defined, use that, does not seem to work anymore, jh mar 11
c
c       if(block(271:273).ne.'   ') then
c          stat(1)=block(271:273)//'  '
c       endif
c
c   preevent time
c
c       call bcd(block(17:17),pre_event)  ! seems that it was bcd before !!!
       c_pre_event(1:1)=block(17:17)
       c_pre_event(2:2)=char(0)

       if(sun) then
          call swap2_one(pre_event)
       endif
       
       write(6,*)'Pre-event time', pre_event
c
c   group delay of filter, byte 81 82
c
        c_group_delay=block(129:130)
c
c   milli seconds
c
        c_millisec(1:1)=block(41:41)
        c_millisec(2:2)=char(0)


c
c   swap a few numbers if on sun
c
       if(sun) then
          call swap2_one(group_delay)
          call swap2_one(millisec)
       endif


        write(6,*) 'Group delay        ', group_delay/1000.0,
     *  '  milli seconds ', millisec

c
c   correct file start time for preevent (negative) etc. 
c
c   not sure why four-byte was used (jh oct 2004)
c         if(four_byte) time_correction=pre_event + group_delay/1000.0
         time_correction=pre_event - group_delay/1000000.0 -
     *   millisec/1000.0
c
c  correct date etc
c
         call timsec
     *   (year(1),month(1),day(1),hour(1),min(1),sec(1),total_time)
         total_time=total_time-time_correction
         write(6,*)' Total time and time correction', total_time,
     *   time_correction
         call sectim(total_time,year(1),doy,month(1),
     *   day(1),hour(1),min(1),sec(1))
       write(6,'(a,1x,i4,1x,4i3,1x,f6.3)')' yy,mm,dd,hh,mm,sec',
     * year(1),month(1),day(1),hour(1),min(1),sec(1)


c   assume same start time etc. for all channels
c
       do i=1,nchan
          year(i)=year(1)
          month(i)=month(1)
          day(i)=day(1)
          hour(i)=hour(1)
          min(i)=min(1)
          sec(i)=sec(1)
          rate(i)=i_rate
          nsamp(i)=i_samp
          cbyte(i)=' '
          if(four_byte) cbyte(i)='4'
          comp(i)='S  Z'
          stat(i)=stat(1)
       enddo
c
c   assume components in case no def file for first 3 components
c       
       comp(1)='S  E'
       comp(2)='S  N'
       comp(3)='S  Z'
c
c  enter loop to define channels if a def file
c
       do i=1,nchan
          call set_def_chan(i,stat(i),comp(i))
       enddo

c
c   make seisan headers
c
      ichan=1      ! only main head
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',outfile
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c
c
      mainhead(1)(2:29)=mainhead_text    ! put in header text
      write(6,'(a)') mainhead(1)
      do i=1,12
         write(2)mainhead(i)
      enddo
c
C-----------------------------------------
C   enter channel  loop
c-----------------------------------------
c

      if(four_byte) then
         close(1)
         open(1,file=infile,status='old',access='direct',recl=4)
      endif
c

      do ichan=1,nchan
c
c   make channel header	
c
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                    net_code,mainhead_text,stat,comp,
     *                    nsamp,rate,cbyte,
     *                    outfile,mainhead,chahead)
         write(6,'(1x,a)') chahead(1:78)
         write(6,'(2a)') ' Outfile: ',outfile
c
c   get response info
c

         call read_resp_head(chahead)                                            
c
c   read samples, one at a time, a bit slow on sun
c
c   if 4 bytes, assume an extra 512 byte header as in GSR 24
c
          if(four_byte) then

             do i=1,i_samp
               irec=257+(i-1)*(nchan+1)+(ichan-1) ! always one extra dummy chann.
               read(1,rec=irec) data4(i)
             enddo
          
c
c   swap
c
             if(sun) then
                call swap4(i_samp,data4)
             endif
c
c   write data
c
             write(2) chahead
             write(2) (data4(i),i=1,i_samp)
         
c
          else
          
             do i=1,i_samp
                irec=257+(i-1)*(nchan+1)+(ichan-1) ! always one extra dummy chann.
                read(1,rec=irec) data(i)
             enddo         
c
c   swap
c
             if(sun) then
                call swap2(i_samp,data)
             endif
c
c   write data
c
c
             write(2)chahead
             write(2) (data(i),i=1,i_samp)
         endif
c
c   end of channels loop
c
      enddo
      write(6,*)
c
c  back for next file if many
c
      goto 101
 99   continue
      write(6,*)' Error with file'
 101  continue
      close(2)
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  

