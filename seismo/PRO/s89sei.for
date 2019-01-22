c$DEBUG
c
c  convert SEIS89 files to seisan format
c
c
c  j. havskov, sep 97
c
c  updates:
c  oct 9 by jh      : new file name
c  oct  20          : bug with day and hour
c  mar 4    lo      : bug with SUN filenames
c  mar 17 99 by jh  : ------------ version 7.0 changes --------------------
c                     stat to 5, new general structure
c  may 11  00    lo : use read_resp_head
c
c  
      implicit none
      include 'seidim.inc'
c-- input data vector	  
      integer*2 data(max_sample)
c-- input file name	  
      character*80 infile
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
c
c--end block of definitions most conversion programs
c---------------------------------------------------------------------
c

c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c-- input file header
      character*512 head,head1
      integer*2 i256(256)
      integer*4 i128(128)
      real f(20),gain(20),phase(20)  ! response function, gain is nm/count
c--Counters etc
	  integer i,j,k,j1,j2,i1,i2,i3,i4,i5,i6
c-- computer type
      logical pc,sun,linux	  
      equivalence(head1,i256)
      equivalence(head1,i128)


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
       text='s89sei.def'
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
c
c  open file and read header
c
       open(1,file=infile,status='old',access='direct',recl=512)
       read(1,rec=1) head
       head1=head
       close(1)
c
c   extract information
c
       stat(1)(1:4)=head(5:8)
       stat(1)(5:5)=' '
       comp(1)=head(21:24)             ! component, often Z, N or E in first column
c
c   assume something about component
c
       if(comp(1)(1:1).eq.'Z'.or.comp(1)(1:1).eq.'N'.
     *or.comp(1)(1:1).eq.'E') then
          comp(1)(4:4)=comp(1)(1:1)
          comp(1)(2:3)='  '
          comp(1)(1:1)='S'
       endif
c
c   put in definition from def file
c
        call set_def_chan(1,stat(1),comp(1))
c
c   number of samples 
c
       if(sun) call swap4_one(i128(114))
       nsamp(1)=i128(114)-256

c
c   swap remaining 2 byte if on sun
c
       if(sun) call swap2(256,i256)
c
c   extract response info
c
c       write(6,*) (i256(i),i=15,40)
       k=1
       do i=1,20
          f(i)=i256(14+k)+i256(15+k)/10000.0
          gain(i)=i256(16+k)*10.0**i256(17+k)
          phase(i)=i256(18+k)+i256(19+k)/10000.0
          k=k+6
c          write(6,*)f(i),gain(i),phase(i)
       enddo

c
       rate(1)=i256(13)+i256(14)/10000.0  ! sample rate
c
       year(1)=i256(135)
       month(1)=i256(136)
       day(1)=i256(137)
       hour(1)=i256(139)
       min(1)=i256(140)
       sec(1)=i256(142)+i256(143)/10000.0
c
c       write(6,'(1x,a,1x,a4)') stat(1),comp(1)
c       write(6,*)' rate',rate(1)
c       write(6,*) year(1),month(1),day(1),hour(1),min(1),sec(1)
 
c
c   make seisan headers
c
      ichan=1
      nchan=1
      cbyte(1)=' '   ! 2 byte data
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,nsamp,
     *                 rate,cbyte,
     *                 outfile,mainhead,chahead)

      write(6,200) outfile(1:29)                                             
 200  format(/,' Output file name is: ',a29)                                
c
c   write response from header if there
c
       if(f(1).ne.0.and.gain(1).ne.0.0) then
          chahead(201:208)= ' 1.00e9 '   ! factor is 10**9 counts/m
          chahead(78:78)='T'             ! use table only
          do i=1,2
            j1=(i-1)*10 + 1
            j2=j1+9
            i1=320+(i-1)*240+1
            i2=i1+79
            i3=400+(i-1)*240+1
            i4=i3+79
            i5=480+(i-1)*240+1
            i6=i5+79
            write(chahead(i1:i2),'(10g8.3)')(f(j),j=j1,j2)
            write(chahead(i3:i4),'(10g8.3)')(1.0/gain(j),j=j1,j2)
            write(chahead(i5:i6),'(10f8.3)')(phase(j),j=j1,j2)
         enddo
       else
         write(6,*)' No response info in input file, will try CAL'
c
c   read response curve into header
c

           call read_resp_head(chahead)                         
       endif
c
c   open output file
c
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   read data
c
      open(1,file=infile,access='direct',recl=2)
c
      do i=1,nsamp(1)
         read(1,rec=i+256) data(i)
      enddo
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
c   swap if on sun
c
      if(sun) call swap2(nsamp(1),data)
c
      write(2)(data(i),i=1,nsamp(1))
      write(6,*)
c
c  back for next file if many
c
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  

