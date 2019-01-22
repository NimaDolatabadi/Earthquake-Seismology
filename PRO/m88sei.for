c
c  convert mars88 files to seisan format
c
c  j. havskov, sep, 93
c
c
c  Testdata and help was provided by Dieter Stoll at Lennartz
c
c  updates:
c  oct 93   by jh: sun version
c  nov 97        : take gain ranging into account and a few other things
c  jan 23 98     : correct error,response file check from header 1040 to 160
c  april 7 98    : --------------   version 7 changes ----------------------
c                  new general structure
c  jun 11 99     : serial number was not put in for station name
c  jun 13     lo : changed routine intand
c  may 11  00 lo : use read_resp_head
c  mar 23  01 jh : if straight 16 bit data, divide by 2**16 and write as
c                  2 byte integers
c  feb 03  03 jh : commen tout intand, now in compdecomp
c
c  This program can handle a maximum of 3 channels and max_sample samples.
c  Output is always 2 or 4 byte integers so remember scaling factor.
c  Gain and time correction is not accounted for.
c
c  Mars88 channel 0 is one, 1 is 2 and 2 is 3 in the output. Channel
c  1, 2 and 3 is also used in channel definition file, NOT 0....
c
c  Gain ranging:
c  
c  In each block, the variable data format (byte 4) has value 0, 1, 2 or 3
c  Correspondingly 0, 2, 3, or 4 of the lowest bytes in data word is used
c  for gain and the correspondign mantissa is 16, 14, 13 or 12 bytes long.
c  The gain exponent is extracted by using an and between an integer 
c  representing 0 to 4 bytes. If e.g. 3 bytes are used, the masking
c  number is 7=(111) in binary. The mantissa is extracted by dividing
c  the sample with 2**exp. In above example with 3 bytes used, the division is
c  by 8. The final sample is constructed by mantissa * 2 **(16-gain exponent).
c
c  If straigt 16 bit data is used, numbers are divived by 65536 so scaling
c  factor is different for straight 16 bit and gain ranged data (changed
c  march 23, 2001)
c

      implicit none
c-- output data vector
      include 'libsei.inc'
      include 'seidim.inc'
      integer*4  data(3,max_sample)
      integer*2  data2(max_sample)
c
c   block of definitions for most conversion programs
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
     *hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	  
ccccccccccccccccccccccccccccccccccccc  end

      integer*4  data4
c-- one file block
      integer*2 block_data(512)
      character*1024 block
       integer doy   ! day of year
c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
	  integer i,nblock,k,l,nrec
c-- computer type
      logical pc,sun,linux	  
c--------------------------- specific lennartz 
c-- data format
       integer data_format
       character*1 c_data_format
c-- current channel in block
       integer block_channel
       character*1 c_block_channel
c-- log2 sample rate
       integer log2rate
       character*1 c_log2rate
c-- channel order in block
       integer bchan(3)
c-- scale
       integer scale
       character*1 c_scale
       integer first_rec       ! first useful block to use
       integer m(3)            ! number of samples, 3 channels
c-- time since 1970 for start of block
       integer itime
       double precision sec70
c-- help for swap
       character*4 xs
       character*1 xx
       integer ixs
       integer exp       ! exponent from format
       integer exp_mask  ! mask to extract data value exponent
       integer mantissa  ! mantissa of one sample
       integer exp_sample! exponent one sample
       logical s16bit    ! true if straight 16 bit
c
c-- serial numbers
c
      integer*2 is_number
      integer*2 delay             ! time delay in milliseconds 
      character*5 s_number        ! seriel number of current station
c
       equivalence (block,block_data)
       equivalence (block(4:4),c_data_format)
       equivalence (block(9:12),itime)
       equivalence (block(13:14),delay)
       equivalence (block(17:17),c_block_channel)
       equivalence (block(18:18),c_log2rate) 
       equivalence (block(21:21),c_scale) 
       equivalence (block(5:6),is_number)
       equivalence (ixs,xs)


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      call computer_type(sun,pc,linux)
c
c   get def file for station codes, give file name
c
       call read_def_chan('m88sei.def',mainhead_text,net_code)
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
      write(6,'(1x,a)') infile
c
c  open file 
c
       open(1,file=infile,status='old',access='direct',recl=1024)
c
c   read first 3 blocks to get start time etc
c
       nrec=1
       s16bit=.true.   ! initially assume straight 16 bit
c
 1     continue
       read(1,rec=nrec,err=99) block
c
c   check if a setup block, channel number is then 15, skip block
c
       if(sun) then
          xs(4:4)=c_block_channel
       else
          xs(1:1)=c_block_channel
       endif
       block_channel=ixs
       if(block_channel.eq.15) then
          nrec=nrec+1
          goto 1
       endif
c
       bchan(1)=block_channel

c
c   find time in year month etc from 1900 to 1970
c
       call TIMSEC (1969,12,31,23,60,0.0,sec70)
c
c   byte swap itime, delay and seriel number
c
       if(sun) then
          xs(1:1)=block(12:12)
          xs(2:2)=block(11:11)
          xs(3:3)=block(10:10)
          xs(4:4)=block(9:9)
          block(9:12)=xs
c-- serial number
          xs(1:1)=block(5:5)
          block(5:5)=block(6:6)
          block(6:6)=xs(1:1)
c-- delay
          xs(1:1)=block(13:13)
          block(13:13)=block(14:14)
          block(14:14)=xs(1:1)
       endif
c
       sec70=sec70+itime   ! add time since 1970
       call sectim (sec70,year(1),doy,
     * month(1),day(1),hour(1),min(1),sec(1))
       write(6,*) year(1),month(1),day(1),hour(1),min(1),sec(1)
c
c   assume same start time for 3 possible channels
c
       year(2)=year(1)
       year(3)=year(1)
       month(2)=month(1)
       month(3)=month(1)
       day(2)=day(1)
       day(3)=day(1)
       hour(2)=hour(1)
       hour(3)=hour(1)
       min(2)=min(1)
       min(3)=min(1)
       sec(2)=sec(1)
       sec(3)=sec(1)
c
c  get one byte integers, do byte swapping
c
       do i=1,4
         xs(i:i)=char(0)
       enddo
c
       if(sun) then
          xs(4:4)=c_log2rate
       else
          xs(1:1)=c_log2rate
       endif
       log2rate=ixs
c
c   scale
c
       do i=1,4
         xs(i:i)=char(0)
       enddo
       if(sun) then
          xs(4:4)=c_scale
       else
          xs(1:1)=c_scale
       endif
       scale=ixs
 
       write(6,*)'Log2rate',log2rate
       write(6,*)'Data format ',data_format
       write(6,*)'Time delay in ms',delay
       write(6,*)'Log2scale ', scale
c
c-- get seriel number, find corresponding code if defined
c   else use seriel number padded with 0, assume max 4 digits
c
       s_number=' '
       write(6,*)'Seriel number',is_number
       write(s_number(1:5),'(i5)')is_number
       stat(1)=s_number
       do i=1,5
          if(stat(1)(i:i).eq.' ') stat(1)(i:i)='0'
       enddo			 	   	   	    
c
c   read next two blocks to see how many channels are available, assume a
c   maximum of 3
c
      nrec=nrec+1
      read(1,rec=nrec,err=99) block
      if(sun) then
	     xs(4:4)=c_block_channel   ! byte swapping
      else
         xs(1:1)=c_block_channel
      endif
      block_channel=ixs
      bchan(2)=block_channel
      nrec=nrec+1
      read(1,rec=nrec,err=99) block
      if(sun) then
         xs(4:4)=c_block_channel
      else
         xs(1:1)=c_block_channel
      endif
      block_channel=ixs
      bchan(3)=block_channel
      if(bchan(1).eq.bchan(2).and.bchan(2).eq.bchan(3)) nchan=1
      if(bchan(1).ne.bchan(2).and.bchan(3).eq.bchan(1)) nchan=2
      if(bchan(1).ne.bchan(2).and.bchan(2).ne.bchan(3)) nchan=3
      write(6,*)'Channel order',bchan
      write(6,*)'Number of channels =',nchan
      rate(1)=1000.0/(2**log2rate)
c
c   assume same sample rate for 3 possible channels
c
      rate(2)=rate(1)
      rate(3)=rate(1)
c
c  save start record number
c
      first_rec=nrec-2
c
c   read whole file 
c
       do i=1,3
         m(i)=1
       enddo
       nrec=first_rec
c
c--------------------------------------------------------------
c  loop to read whole file
c--------------------------------------------------------------
c
 555   continue

       read(1,rec=nrec,err=600) block
       nrec=nrec+1
c
c  find channel number since channels can be in any order !!!!
c
       if(sun) then
          xs(4:4)=c_block_channel
       else
          xs(1:1)=c_block_channel
       endif
       block_channel=ixs
c
c  if channel is 15, skip since setup block
c
       if(block_channel.eq.15) goto 555
c      write(6,*) block_channel
c
c   find format
c
       if(sun) then 
          xs(4:4)=c_data_format
       else
          xs(1:1)=c_data_format
       endif
c
       data_format=ixs
       if(data_format.eq.0) exp=0
       if(data_format.gt.0) exp=data_format+1
       exp_mask=2**exp-1    ! value to mask with to get exponenet
c
c   do byte swapping if on sun
c
       if(sun) then
          do k=25,1024,2
            xx=block(k:k)
            block(k:k)=block(k+1:k+1)
            block(k+1:k+1)=xx
          enddo
       endif
c
       do l=1,500
c
c   correct each sample for exponent 
c
c
c   new from march 27, 2001: find if exponenet always is 0 indicating
c                            straight 16 bit data, if so divide by
c                            65536
c
          data4=block_data(l+12)
          call intand(data4,exp_mask,exp_sample)  ! exponent one sample
          if(exp_sample.gt.0) s16bit=.false.
          mantissa=data4/(exp_mask+1)             ! get mantissa
          data(block_channel+1,m(block_channel+1))=
     *    mantissa*2**(16-exp_sample)     ! calculate sample                      
          m(block_channel+1)=m(block_channel+1)+1
          if(m(block_channel+1).gt.max_sample)  goto 600
       enddo
c
c   get next block
c
       goto 555
c
c   whole file read
c
 600   continue
       if(s16bit) then
          write(6,*)' Straight 16 bit data'
       else
         write(6,*)' Gain ranged data'
       endif

       nblock=nrec-1
c
       nsamp(1)=(nblock/nchan)*500
       write(6,*)'Number of samples',nsamp(1)
       if(nsamp(1).gt.max_sample) then
            nsamp(1)=max_sample
            write(6,*)' Number of samples has been cut to',max_sample
            nblock=nsamp(1)/(500*nchan)
       endif
c
c   assume same number of samples for 3 possible channels
c
       nsamp(2)=nsamp(1)
       nsamp(3)=nsamp(1)
c
c   assume components in case no def file
c
       comp(1)='S  Z'
       comp(2)='S  N'
       comp(3)='S  E'
c
c   2 or 4 byte integers for data, all 3 possible channels
c
       if(.not.s16bit) then
          cbyte(1)='4'
          cbyte(2)='4'
          cbyte(3)='4'
       else
          cbyte(1)='2'
          cbyte(2)='2'
          cbyte(3)='2'
       endif

c
c   assume same stations initially
c  
        stat(2)=stat(1)
        stat(3)=stat(1)

c
c  enter loop to define channels if a def file
c
       do ichan=1,nchan
         call set_def_chan(ichan,stat(ichan),comp(ichan))
       enddo
c
c   make seisan headers
c
      ichan=1
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
c   ******** next write and rewind is to fix a problem ******
c   if not done, garbage will be insert between the first and 
c   second record, at least on some implementations
c
      mainhead(1)(2:29)=mainhead_text    ! put in header text
c     write(2)mainhead(i)
      write(2)mainhead(1)
      rewind(2)
      do i=1,12
         write(2)mainhead(i)
         write(6,'(a80)') mainhead(i)
      enddo
C
C   enter channel  loop
c
      do ichan=1,nchan
c
c   make channel header	
c
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                    net_code,mainhead_text,
     *                    stat,comp,nsamp,rate,cbyte,
     *                    outfile,mainhead,chahead)
         write(6,'(1x,a)') chahead(1:78)
c                                                                               
c   get response            
c                                                                               
         call read_resp_head(chahead)                      
c
c   write data
c
         write(2)chahead
c
c   truncate if only 16 bit
c
         if(s16bit) then
            do i=1,nsamp(ichan)
                data2(i)=data(ichan,i)/65535
            enddo
            write(2) (data2(i),i=1,nsamp(ichan))
         else
            write(2) (data(ichan,i),i=1,nsamp(ichan))
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
								  
C**********************************************************************
C                                                                     *
C      SUBROUTINE INTAND(I1,I2,I3)                                    *
C                                                                     *
C      SUBROUTINE BITWISE "ANDS" THE INTEGERS IN I1 AND I2            *
C      AND RETURNS THE RESULT IN INTEGER I3                           *
C      FOR EXAMPLE                                                    *
C      IF THE 32 BITS IN I1 ARE 11001100 1110001110 11110000 11111111 *
C      AND IN I2 ARE            10101010 1100110011 00110011 00110011 *
C      I3 WILL BECOME           10001000 1100000010 00110000 00110011 *
C                                                                     *
C      NOTE "AND" IS NOT STANDARD FORTRAN 77 FUNCTION SO THIS IS A    *
C      MACHINE DEPENDANT SUBROUTINE                                   *
C**********************************************************************
C
c       SUBROUTINE INTAND(I1,I2,I3) 
c       INTEGER *4 I1,I2,I3
c       logical sun,pc,linux
c       call computer_type(sun,pc,linux)
COMPUTERTYPE
C##SUN## REMOVE COMMENT FROM NEXT LINE FOR SUN COMPUTERS
c          if(sun.or.linux) I3 = AND(I1,I2)
C##DEC## REMOVE COMMENT FROM NEXT LINE FOR PDP AND VAX COMPUTERS,+sun
c          if(sun.or.linux)I3 = JIAND(I1,I2)
C##IBM## REMOVE COMMENT FROM NEXT LINE FOR IBM PC'S (I THINK), now also sun !
c        if(pc)I3 = IAND(I1,I2)
c       RETURN
c       END
c
