
c
c  converts Kinemetrics film record acceleraiton files to SEISAN, 
c  type *.v1 files
c
c  jh  december 2002, little tested
c
c   it is assumed that:
c 
c            channel 1 is N, 2 is Z and 3 is E
c            there is always 3 channels in file
c            input values are in 1/10 g, the output is in 1/1 000 000 g
c            station code is taken from file name as given in first 
c                 line of input file
c            the 3 channels can have different number of samples, 
c                 however it is assumed
c                 that they all start at the same time
c   
c
c  
c  updates:
c  
c
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
c-- input data vector	  
      real    rdata1(max_sample),rdata2(max_sample),rdata3(max_sample)


c-- main header------------------------------------------
      character*80 mainhead(max_trace)
      character*29 mainhead_text
c-- channel header
      character*1040 chahead
      real duration            ! duration in secs of record
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
      character*3 cmon(12),xmon ! months
c--Counters etc
      integer i,j
      logical no_net                    ! flag if net_code set
      data cmon /'JAN','FEB','MAR','APR','MAY','JUN',
     *           'JUL','AUG','SEP','OCT','NOV','DEC'/


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c

      mainhead_text=' '
      time_error=' '
c
c   get def file for station codes, give file name
c
      deffile='kacsei.def'
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
c   read first line
c
       read(1,'(a)') text
c
c   fish out station code
c
       j=-1
       stat(1)='XXXXX'
       do i=1,15
          if(text(i:i).eq.':') j=i
       enddo
       if(j.gt.0) then
          stat(1)(1:3)=text(j+2:j+4)
          stat(1)(4:5)=' '
       endif
c
c  read next 2 lines
c
       do i=1,2
          read(1,'(a)') text
       enddo
c      
c   set month to 1 if no month found
c

      month(1)=1
      xmon=text(1:3)
       do i=1,12
          if(cmon(i).eq.xmon) month(1)=i
       enddo
c
c   time line, remove separators and month
c
       do i=1,30
         if(ichar(text(i:i)).lt.46.or.ichar(text(i:i)).gt.57.
     *   or.ichar(text(i:i)).eq.47) text(i:i)=' '
       enddo


c
c  get times
c
       open (99,file='t1',status='unknown')
       write(99,'(a)') text(1:30)
       
       rewind 99
       
       
       read(99,*) 
     * day(1),year(1),hour(1),min(1),sec(1)
       
       close (99,status='delete')
c

       do i=1,8
         read(1,'(a)') text
       enddo

c
c   read number of samples and time duration
c

c
c   remove separators and text
c
       text(1:10)=' '
       do i=1,50
         if(ichar(text(i:i)).lt.46.or.ichar(text(i:i)).gt.57.
     *   or.ichar(text(i:i)).eq.47) text(i:i)=' '
       enddo
c
c  get values
c
       open (99,file='t1',status='unknown')
       write(99,'(a)') text(1:50)
       
       rewind 99
       
       
       read(99,*) nsamp(1), duration 
     * 
       
       close (99,status='delete')
c

 
c
c sample rate
c    
       
       rate(1)=nsamp(1)/duration
c  
       do i=1,16
         read(1,'(a)') text
       enddo    
c
c   assume 3 channels
c
        nchan=3       

       write(6,*) '   nsamp=',nsamp(1),'   rate',rate(1)
       write(6,*) '   date and time =',year(1),month(1),day(1),hour(1),
     *                                 min(1),sec(1)    

c
c   read first channel of data
c
       read(1,'(10g13.6)')(rdata1(i),i=1,nsamp(1))

c
c   read number of samples for second channel
c
       do i=1,12
          read(1,'(a)') text
       enddo
       read(text(16:24),'(i9)') nsamp(2)
       write(6,*)' Number of samples, second channel', nsamp(2)  

c
c   read second channel of data
c
       do I=1,16
         read(1,'(a)') text
       enddo

       read(1,'(10g13.6)')(rdata2(i),i=1,nsamp(2))

c
c   read number of samples for third channel
c
       do i=1,12
          read(1,'(a)') text
       enddo
       read(text(16:24),'(i9)') nsamp(3)  
       write(6,*)' Number of samples, third channel', nsamp(3)

c
c   read third channel of data
c
       do i=1,16
         read(1,'(a)') text
       enddo

       read(1,'(10g13.6)')(rdata3(i),i=1,nsamp(3))

c
c  enter loop to define header info
c
       do ichan=1,nchan
c
c   first put in defaults
c
          
            stat(ichan)=stat(1)          
            if(ichan.eq.1) comp(ichan)='A  N'
            if(ichan.eq.2) comp(ichan)='A  Z'
            if(ichan.eq.3) comp(ichan)='A  E'

c
c   assume same start time and sample rate for all channels
c            
         year(ichan)=year(1)
         month(ichan)=month(1)
         day(ichan)=day(1)
         hour(ichan)=hour(1)
         min(ichan)=min(1)
         sec(ichan)=sec(1)
         rate(ichan)=rate(1)
         cbyte(ichan)='4'
c
         write(6,'(1x,a5,i4,4i3,f7.1)') stat(ichan),
     *   year(ichan),month(ichan),
     *   day(ichan),hour(ichan),min(ichan),sec(ichan)
       enddo
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
        if(ichan.eq.1) write(2) (int(100000.0*rdata1(i)),
     *  i=1,nsamp(ichan))
        if(ichan.eq.2) write(2) (int(100000.0*rdata2(i)),
     *  i=1,nsamp(ichan))
        if(ichan.eq.3) write(2) (int(100000.0*rdata3(i)),
     *  i=1,nsamp(ichan))

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
								  
