c
c  convert pceq files files to seisan format, only tested on pc
c
c  j. havskov, sep 16, 93
c
c  updates:
c
c oct 2000 jh: compiled with digital compiler, several modifications
c  
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
c-- input data vector	  
      integer*2  data(max_sample)
c-- one file block
      integer*2 block(258)
      character*516 tblock
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
c-- input file name	  
      character*80 infile,text
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- gain
      integer gain(30)
c-- question
      character*80 question	  
c--- no of samples in channel with most samples
      integer maxsamp
c--Counters etc
	  integer i,nblock,k,l,m
c-- computer type
      logical pc,sun,linux
      equivalence(block,tblock)
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
       text='pcqsei.def'
      call read_def_chan(text,mainhead_text,net_code)
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
c  open file and read header, the file is opened with recl 516, NOT 512 as
c  given in the pceq format specs (did not work). So either the spec is not
c  correct or the the Belgian programmaking the pceq files is not
c  according to spec !!!!!!!!!!!!!!!!!!!!
c
       open(1,file=infile,status='old',access='direct',recl=516)
c
c   read first header
c
       read(1,rec=1)block
       rate(1)=block(10)
       maxsamp=block(11)
       nchan=block(12)
       write(6,*) 'maxsamp=',maxsamp,'  number of channels = ',nchan
c
c  enter loop to read all channel header info
c
       k=6
       do ichan=1,nchan
          read(1,rec=k) block
          k=k+1
          stat(ichan)(1:4)=tblock(5:8)
          stat(ichan)(5:5)=' '
c
c   assume that 0 1 and 2 have to do with components, remove
c
          if(stat(ichan)(4:4).eq.'0'.or.stat(ichan)(4:4).eq.'1'
     *    .or.stat(ichan)(4:4).eq.'2') stat(ichan)(4:4)=' '
c
c   component, this is not well specified in input file, just make
c   some assumptions, fix to your system
c
          if(nchan.eq.3.and.ichan.eq.1) comp(ichan)='S  Z'
          if(nchan.eq.3.and.ichan.eq.2) comp(ichan)='S  N'
          if(nchan.eq.3.and.ichan.eq.3) comp(ichan)='S  E'
          if(nchan.ne.3) comp(ichan)='S  Z'
          gain(ichan)=block(5)
          year(ichan)=block(6)
          if(year(ichan).lt.50) then
              year(ichan)=year(ichan)+2000
          else
              year(ichan)=year(ichan)+1900
          endif
          month(ichan)=block(7)
          day(ichan)=block(8)
          hour(ichan)=block(9)
          min(ichan)=block(10)
          sec(ichan)=block(11)+float(block(12))/1000.0
          rate(ichan)=rate(1)
          cbyte(ichan)=' '
          nblock=block(13)
c
c   it seems that number of samples is maxsamp and not, as commented out below
c   related to nblock as it should according to spec, however, 
c   the empty blocks are there
c
c          nsamp(ichan)=nblock*256
          nsamp(ichan)=maxsamp
c
          write(6,'(1x,a5,5i4,f7.1)') stat(ichan),
     *    year(ichan),month(ichan),
     *    day(ichan),hour(ichan),min(ichan),sec(ichan)
          write(6,*)' nblock=',nblock,'  gain=',gain(ichan)
          k=k+nblock
       enddo
c
c   put in definition from def file
c
       do i=1,nchan
          call set_def_chan(i,stat(i),comp(i))
       enddo
c
c   make seisan headers
c
      ichan=1
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,
     *                 stat,comp,nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',outfile
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c
      write(6,'(1x,a)')mainhead(1)(1:75)
      do i=1,12
         write(2)mainhead(i)
      enddo
C
C   enter channel  loop
c
      k=7
      do ichan=1,nchan
c
c   make channel header	
c
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
      write(6,'(1x,a)') chahead(1:78)
c
c  read data
c
      m=1
      do i=1,nblock
         read(1,rec=k)block
         k=k+1
         do l=1,256
           data(m)=block(l)
           m=m+1
         enddo
      enddo
c
c   skip next channel header
c
      k=k+1
      write(2)chahead
      write(2) (data(m),m=1,nsamp(ichan))
c
c   end of channels loop
c
      enddo
      close(2)
      write(6,*)
c
c  back for next file if many
c
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  
