c$DEBUG
c
c  convert Nanometrics files to seisan format
c
c
c  j. havskov, march 99
c
c  updates:
c
c  sep 29  bmt : update name of agency with station
c  oct 21  bmt : update for sun and pc
c  dec 09  lo  : read component from last 3 chars in string
c  may 11  lo  : use read_resp_head
c  oct 12  jh  : fix pc verison with pc y5dump, requires 2 dll files
c                add component to output file name
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
c-- input data vector	  
      integer*4 data(max_sample)
c-- input file name	  
      character*80 infile
      integer seiclen      ! function
      character*80 text
c
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
     *hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	  
c index
      integer ind
c
c--end block of definitions most conversion programs
c---------------------------------------------------------------------
c

c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
	  integer i
      integer code   ! error code
c-- computer type
      logical pc,sun,linux	  
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)
c
c   get def file for station codes, give file name
c
       text='nansei.def'
      call read_def_chan(text,mainhead_text,net_code)
c
c   get file name
c
      in=0
      question=' File name, # or filenr.lis for all'
      call filename(question,infile)	    
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis'.or.infile(1:10).eq.
     *'FILENR.LIS') then
         open(8,file='filenr.lis',status='old',err=20)
         goto 21
 20      continue
         write(6,*)' No filenr.lis'
         stop
 21      continue
         in=1
      endif
c
c  file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') infile
	  i=seiclen(infile)   ! length of file name
c
c   convert file to ascii using nanometrics program y5dump
c      
      call systemc
     *('y5dump -h -d '//infile(1:i)//'> nan.out',22+i)
c
c  open file and read header info
c
       open(1,file='nan.out',status='old')
c
c  read in a loop to get needed info
c
       comp(1)='S  Z'
  60   continue
       read(1,'(a)') text
c
c assume that 3 char component in the end of string
c
       if(text(9:17).eq.'StnLocChn') then
          stat(1)=text(20:24)
          ind=seiclen(text)
          comp(1)=' '
          comp(1)(1:1)=text(ind-2:ind-2)
          comp(1)(2:2)=text(ind-1:ind-1)
          comp(1)(4:4)=text(ind:ind)
c          comp(1)(1:1)=text(25:25)
c          comp(1)(4:4)=text(27:27)
       endif
c
c   number of samples 
c
       if(text(1:17).eq.'Number of Samples') then
          call sei get values(1,text(20:30),code)
          nsamp(1)=array$(1)
       endif

c
       if(text(7:17).eq.'Sample Rate') then
          read(text(19:30),'(f8.2)') rate(1)
       endif
c
       if(text(8:17).eq.'Start Time') then
          read(text(20:43),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f7.2)') 
     *    year(1),month(1),day(1),
     *    hour(1),min(1),sec(1)
       endif
c
c   continue if not at the data section
c
       if(text(4:17).ne.'Format Version') goto 60
c
c   header read
c
       write(6,'(1x,a,1x,a4)') stat(1),comp(1)
       write(6,*)' rate,nsamp',rate(1),nsamp(1)
       write(6,*) year(1),month(1),day(1),hour(1),min(1),sec(1)
 
c
c   put in definition from def file
c
       call set_def_chan(1,stat(1),comp(1))
c
c   make seisan headers
c
      ichan=1
      nchan=1
      cbyte(1)='4'   ! 4 byte data
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,nsamp,
     *                 rate,cbyte,
     *                 outfile,mainhead,chahead)

c
c   add component name to avoind overwriting
c
      outfile(30:30)='_'
      outfile(31:34)=comp(1)
      do i=31,34
          if(outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      write(6,200) outfile(1:34)                                             
 200  format(/,' Output file name is: ',a)                                
c
c   read response curve into header
c

      call read_resp_head(chahead)                                            
c
c   open output file
c
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   read data
c
c
      do i=1,nsamp(1)
          data(i)=0
      enddo
      read(1,*,END=222) (data(i),i=1,nsamp(1))
      goto 223
c
 222  data(i)=0
 223   continue
   
c   end of file on solaris is incoplete causing an end of file, so the
c   end=222 is added above and all samples are set to 0 before reading.
c   in this wasy any missing samples will be zero
c
c
c   write data
c
      write(6,'(1x,a)')mainhead(1)(1:79)
      write(6,'(1x,a)')chahead(1:79)
      do i=1,12
         write(2)mainhead(i)
      enddo
      write(2) chahead
c
      write(2)(data(i),i=1,nsamp(1))
      write(6,*)
      close(1)
      close(2)
c
c  back for next file if many
c
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  

