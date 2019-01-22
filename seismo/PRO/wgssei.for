c  Program name: wgssei
c
c  convert wgsn files to seisan format, PC, Sun and Linux
c
c  this is a fist version, only tested with a few files
c
c  j. havskov, aug 2001
c
c
c   program do not seem to work on sun !!!!!!
c
c  updates:
c 
c  apr 04   jh: fix on component read, a mess, seems it can be anywhere


      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
      integer*4  data(max_sample)     ! output samples
      character*80 text               ! general text
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

c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
          integer i,k,l,np,nz
c-- computer type
      logical pc,sun,linux	  
c


c
c print version
c
      include 'version.inc'
      out_version_date='ASugust 28, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)
c
c   get def file for station codes, give file name
c
       call read_def_chan('wgssei.def',mainhead_text,net_code)
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
c   file loop if many files, here use text header files
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
       open(1,file=infile,status='old')
	   
c
c   read all header lines
c
       nchan=1
 1     continue
c
c   read first line                                 
c
       read(1,'(a)') text
c
c   station  and component
c
       read(1,'(a)') text
       stat(1)=text(1:5)
       comp(1)=' '
       comp(1)(1:1)=text(15:15)
       comp(1)(4:4)=text(16:16)
       if(text(17:17).ne.' ') then
          comp(1)(2:2)=text(16:16)
          comp(1)(4:4)=text(17:17)
       endif
c
c   try another one for comp
c
       if(comp(1)(1:1).eq.' ') then
          comp(1)(1:1)=text(10:10)
          comp(1)(4:4)=text(12:12)
       endif
c
c   put in definition from def file
c
       call set_def_chan(1,stat(1),comp(1))

c
c   time, sec has to be fixed, for now assume integer
c
       read(1,'(i4,5i2)') year(1),month(1),day(1),hour(1),min(1),i
       sec(1)=i
c
c   number of samples
c
       read(1,*) nsamp(1)
c
c   sample rate in ms
c
       read(1,*) rate(1)
       rate(1)=1000.0/rate(1)
c
c   response info, maybe, not used
c
       do i=1,5
          read(1,'(a)') text
       enddo
c
c   number of zeros and poles, not used
c
       read(1,*) nz,np
c
c   zeros and poles, not used
c
       do i=1,nz+np
           read(1,*) text
       enddo
c
c   station coord, not used
c
       read(1,'(a)') text
c
c  end of reading header file
c
       write(6,*)' year, month, day, hour, nsamp, rate',
     * year(1),day(1),hour(1),nsamp(1),rate(1)
c
c   make seisan headers
c
      ichan=1
      cbyte(1)='4'
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   add component code to output file
c
      outfile(1:32)=outfile(1:29)//'_'//comp(1)(1:1)//comp(1)(4:4)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',outfile
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c
      mainhead(1)(2:29)=mainhead_text    ! put in header text
c
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
c   read data samples for one channel
c
         read(1,*)(data(i),i=1,nsamp(ichan))
c
c   write data
c
         write(2)chahead
         write(2) (data(i),i=1,nsamp(ichan))
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
								  
