c$DEBUG
c
c  converts Sprengnether DR3024 and DR3016 to SEISAN format
c  formats slight different, automatically corrected for
c  only essential info read, no instrument correction, no channel info read
c  channels can only be defined using the drsei.def definition file, NOTE,
c  only 4 lowest digits of seriel number is used. if a station code is given,
c  it will be used, else the serial number is used for station code.
c  default componenents are s  z, s  n, s  e
c  Output is always 4 byte integers 
c
c  j. havskov, april 98
c
c    may 11  00    lo : use read_resp_head
c
c  Testdata and help was provided by Sprengnether
c
c  updates:
c  --------------------- Version 7 ---------------------
c sep 22 99 lo: changes for Version 7
c               changed to run on Sun
c sep 30    lo: tested under Linux
c dec 99    jh: define mainhead to max_trace, was 12, read fraction of second,
c               use station codes when available, hardwire default components
c

      implicit none
      include 'seidim.inc'
c-- output data vector	  
      integer*4  data(max_sample)
      character*256 block  ! input file header
c-- main header------------------------------------------
      character*80 mainhead(max_trace)
c-- channel header
      character*1040 chahead
c-- output file name 
      character*80 outfile,deffile
c-- number of stations
      integer nchan	  
c-- channel to calculate channel header for
      integer ichan
c-- network code
      character*5 net_code	  
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)	  
c--channel header date and times	  
      integer year(max_trace),month(max_trace),day(max_trace),
     &     hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      integer*2 i_rate
      character*2 c_rate
      real srate(max_trace)	  
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)
c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
	  integer i,irec
c-- computer type
      logical pc,sun,linux
c--------------------------- specific lennartz 
       integer ishift          ! one header relative to other in two models
c-- help for swap
       character*4 xs
       integer ixs
       integer i_samp    ! number of samples
       character*4 c_samp! ----------------
       integer*2 i2
       character*2 c2
      character*2 cs_number       ! seriel number
      integer*2 is2_number        ! -------------
      integer*4 is_number
      character*8 s_number        ! seriel number of current station
c-- corresponding station codes
      character*29 mainhead_text
      logical no_net                    ! flag if net_code set
      integer seiclen

c
      equivalence(is2_number,cs_number)
      equivalence(i_rate,c_rate)
      equivalence (c_samp,i_samp)
      equivalence (ixs,xs)
      equivalence (i2,c2)



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c init
c
      mainhead_text = ' '
c
      call computer_type(sun,pc,linux)

c
c   get def file for station codes, give file name
c
      deffile='drsei.def'
      no_net = .FALSE.
      net_code=' '

      call read_def_chan(deffile,mainhead_text,net_code)

      if (net_code.eq.' ') no_net = .true.

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
       open(1,file=infile,status='old',access='direct',recl=256)
c
c   read header block to get start time etc
c
c
 1     continue
c       read(1,rec=1,end=99) block
       read(1,rec=1) block
       close(1)
c
c   open again with record length 4 to read samples later
c
       
      open(1,file=infile,status='old',access='direct',recl=4)

c
c   get name of recorder and find which kind, bytes are shifted
c   3 bytes in one relative to other
c
       if(block(53:54).eq.'DR') then
          ishift=3   ! dr3024
       else
          ishift=0
       endif
       write(6,'(a,a)')' Recorder ',block(50+ishift:55+ishift)
c
c   number of samples
c
       c_samp=block(7:10)
c
c   seriel number
c
       cs_number=block(57+ishift:58+ishift)
c
c   sample rate
c
       c_rate=block(11:12)
c
c   swap a few numbers if on sun
c
       if(sun) then
c  seriel number
          xs(1:1)=cs_number(1:1)
          cs_number(1:1)=cs_number(2:2)
          cs_number(2:2)=xs(1:1)
c  sample rate
          xs(1:1)=c_rate(1:1)
          c_rate(1:1)=c_rate(2:2)
          c_rate(2:2)=xs(1:1)

c  number of samples, change lo
          xs(1:1)=c_samp(1:1)
          c_samp(1:1)=c_samp(4:4)
          c_samp(4:4)=xs(1:1)
          xs(1:1)=c_samp(2:2)
          c_samp(2:2)=c_samp(3:3)
          c_samp(3:3)=xs(1:1)
       endif
c
c   number of channels
c
       nchan=ichar(block(13:13))
       write(6,*)'Number of channels',nchan
c
c   get times
c
c   fractions of seconds
       c2=block(32+ishift:33+ishift)
       if(sun) call swap2_one(i2)
       write(6,*)'Samples offset',i2
c   seconds
       call bcd(block(34+ishift:34+ishift),i)
       sec(1)=i+float(i2)/float(i_rate)
c   minutes
       call bcd(block(35+ishift:35+ishift),min(1))
c   hour
       call bcd(block(36+ishift:36+ishift),hour(1))
c   day
       call bcd(block(37+ishift:37+ishift),day(1))
c   month
       call bcd(block(38+ishift:38+ishift),month(1))
c   year
       call bcd(block(39+ishift:39+ishift),year(1))

       if (year(1).gt.50) then
         year(1) = year(1) + 1900
       else
         year(1) = year(1) + 2000
       endif

c
c   initially use seriel numbers padded with 0 for station codes, 
c   assume max 5 digits, if more, only use last 5 digits of number
c   seriel number is unsigned integer, correct by putting into
c   4 byte word and null 2 top bytes
c
       ixs=is2_number
       if(sun) then
          xs(1:1)=char(0)
          xs(2:2)=char(0)
       else
          xs(3:3)=char(0)
          xs(4:4)=char(0)
       endif
       is_number=ixs
       write(s_number,'(i8)') is_number
       write(6,'(a14,5x,a5)') ' Serial number ',s_number(4:8)
       write(6,*) 'Number of samples',i_samp
c
c   use code if there, else use serial number
c
       if(block(59+ishift:63+ishift).ne.' ') then
          stat(1)=block(59+ishift:63+ishift)
          do i=1,5
             if(stat(1)(i:i).eq.char(0)) stat(1)(i:i)=' '
          enddo
       else
          stat(1)=s_number(4:8)
          do i=1,5
             if(stat(1)(i:i).eq.' ') stat(1)(i:i)='0'
          enddo			 	   	   	    
       endif
c
c   assume same start time etc. for all channels
c
       do i=1,nchan
          year(i)=year(1)
          month(i)=month(1)
          day(i)=day(1)
          hour(i)=hour(1)
          min(i)=min(1)
          sec(i)=sec(1)
          srate(i)=i_rate
          nsamp(i)=i_samp
          cbyte(i)='4'
          comp(i)(1:4) = '    '
c          write(comp(i)(4:4),'(i1)') i
          stat(i)=stat(1)
       enddo
        write(*,*) year(1),month(1),day(1),hour(1),min(1),
     &   sec(1),srate(1),nsamp(1),' ',cbyte(1),' ',comp(1),' ',
     &   stat(1)
c
c   assume components in case no def file for first 3 components
c       
        comp(1)='S  Z'
        comp(2)='S  N'
        comp(3)='S  E'
c
c  enter loop to define channels if a def file
c

c
c   look for values in def file
c
       do i=1,nchan
         call set_def_chan(i,stat(i),comp(i)) 
       enddo

c
c   make seisan headers
c
      ichan=1
      if (no_net) then
        net_code = stat(1)
      endif

      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,nsamp,
     *                 srate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',
     &           outfile(1:seiclen(outfile))
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c
      mainhead(1)(2:29)=mainhead_text    ! put in header text
      write(6,'(a80)') mainhead(1)
      do i=1,12
         write(2)mainhead(i)
c         write(6,'(a80)') mainhead(i)
      enddo
c
C-----------------------------------------
C   enter channel  loop
c-----------------------------------------
c
      do ichan=1,nchan
c
c   make channel header	
c
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                    net_code,mainhead_text,
     *                    stat,comp,nsamp,srate,cbyte,
     *                    outfile,mainhead,chahead)
         write(6,'(1x,a)') chahead(1:78)
c                                                                               
c   get response, save date etc since removed when calling read_resp            
c                                                                               
         call read_resp_head(chahead)                                            
c
c   read samples
c
          do i=1,i_samp
            irec=65+(i-1)*nchan+(ichan-1)
            read(1,rec=irec) data(i)
          enddo
 
          if (sun) then
             call swap4(i_samp,data)
          endif
 
c
c   write data
c
         write(2)chahead
         write(2) (data(i),i=1,i_samp)
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
								  

