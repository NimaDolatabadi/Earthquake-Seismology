c
c  convert Lennartz ascii files to seisan format
c
c
c  j. havskov, October 2000
c
c  updates:
c
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
          integer i,k
      integer code   ! error code
      real x         ! read one real value
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
       text='lensei.def'
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
c read header info from first line
c
       open(1,file=infile,status='old')
c
       read(1,'(a)') text
       stat(1)(1:4)=text(12:15)
       stat(1)(5:5)=' '
       comp(1)='S  Z'
c
c   use file name info to get component
c
       if(infile(12:12).eq.'1') comp(1)='S  N'
       if(infile(12:12).eq.'2') comp(1)='S  E'
c
c   strip all text from line
c
       call remove_letters(text)
c
c   read all numbers
c
       call sei get values(7,text,code)
c
c   sample interval in ms, convert to rate 
c
       rate(1)=1000.0/array$(1)

c
       year(1)=array$(2)
       month(1)=array$(3)
       day(1)=array$(4)
       hour(1)=array$(5)
       min(1)=array$(6)
       sec(1)=array$(7)

c
c   read all samples
c
       i=1
 1     continue
         read(1,*,end=2) text
c
c   remove last character, could be gunk
c
         k=index(text,' ')
         text(k-1:k-1)=' '
         read(text,'(f20.3)') x
         data(i)=x
         i=i+1
         goto 1
 2     continue
       nsamp(1)=i-1

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
								  

