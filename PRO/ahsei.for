c
c  converts ah ascii files to SEISAN
c
c  jh  aug 2009, little tested
c
c  component hardwired to S  Z
c
c
c
c  
c  updates:
c  
c
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
      include 'version.inc'

c-- input data vector	  
      integer data(max_sample)
c-- main header------------------------------------------
      character*80 mainhead(max_trace)
      character*29 mainhead_text
c-- channel header
      character*1040 chahead
c-- output file name, definition file 
      character*80 outfile, deffile
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
     *hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	  
      character*80 text         ! text 
      character*1 time_error    ! if blank, no error

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
          integer i,code

      logical no_net                    ! flag if net_code set



c
c print version
c

      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c

      mainhead_text=' '
      time_error=' '
c
c   get def file for station codes, give file name
c
      deffile='ahsei.def'
      no_net = .FALSE.
      net_code=' '

      call read_def_chan(deffile,mainhead_text,net_code)

      if (net_code.eq.' ') no_net = .true.

c
c   get file name
c
19    continue
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
c   open file to read
c
       open(1,file=infile,status='old')
c
c   read 1 header line
c
       read(1,'(a)') text
c
c   station code
c
       read(1,'(6x,a5)') stat(1)
       write(6,*) stat(1)
c
c  for now component is blank, read own to number samples
c
  30   continue
       read(1,'(a)') text
       write(6,*) text
       if(text(1:5).eq.'ndata') then
          write(6,*) 'read n'
          call sei get values(1, text(8:80), code )
c          read(1,'(7x,i)')nsamp(1)
          nsamp(1)=array$(1)
       endif
c
c   sample interval
c
       if(text(1:5).eq.'delta') then
          call sei get values(1, text(8:80), code )
          rate(1)=array$(1)
          rate(1)=1.0/rate(1)
       endif

c
c    start time
c
       if(text(1:5).eq.'start') then
          call sei get values( 6, text(12:80), code )
          year(1)=array$(1) 
          month(1)=array$(2)
          day(1)=array$(3)
          hour(1)=array$(4)
          min(1)=array$(5)          
          sec(1)=array$(6)         
       endif
c
c  samples
c
       if(text(1:5).eq.'data:') goto 40
c
c    back for next header line
c
       go to 30
       
       write(6,*) year(1),month(1),day(1),hour(1),min(1),sec(1)

c
c   read data
c
 40    continue

       read(1,*) (data(i),i=1, nsamp(1))

       nchan=1

       write(6,*) 'nchan=',nchan,'   nsamp=',nsamp(1),'   rate',rate(1)
c
c   define header info
c
       cbyte(1)='4'

c
c   first put in defaults in case no def file
c
           
           
       comp(1)='S  Z'

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
      if (no_net) net_code=stat(1)(1:5)
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,
     *                 stat,comp,nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
      write(6,'(a22,a40)')' Output file name is: ',outfile(1:40)
      open(2,file=outfile,status='unknown',form='unformatted')	  
      mainhead(1)(2:29)=mainhead_text
c
c   write main header
c
      write(6,'(1x,a)') mainhead(1)(1:75)
      do i=1,12
         write(2)mainhead(i)
      enddo
C
C   enter channel  loop
c
      do ichan=1,nchan
c
c   make channel header	
c
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,
     *                 stat,comp,nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c                                                                               
c   get response            
c                                                                                                                
        call read_resp_head(chahead)

        write(2)chahead
        write(6,'(1x,a)') chahead(1:70)
        write(2) (data(i),i=1,nsamp(ichan))
 
c   end of channels loop
c
      enddo
      write(6,*)
      close(2)
c
c  back for next file if many
c
      if (in.eq.1) goto 1000                                                               
      goto 19
      stop
      end	   	  	  	         	  	  
								  
