
c
c  converts Terra ascii files to SEISAN
c
c  jh  july 2001, little tested
c mar 10 2011 jh: changed format to a newer version of asci file, migh not work
c                 with data from before 2000. then use version 8.3 or earlier 
c                 seisan programs
c
c  station are defualt character 11-14 in first line,
c  components S  Z, S  N, S  E, can be changed with
c  def file, assume 1-3 channels, all channels smae rate and number of samples
c
c  if no gsp correction, use IDS time, else use Corrected event start
c
c  if a channel is missing, no check on components
c
c  
c  updates:
c  
c
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
c-- input data vector	  
      integer data1(max_sample),data2(max_sample),data3(max_sample)
      real x1,x2,x3

c-- main header------------------------------------------
      character*80 mainhead(max_trace)
      character*29 mainhead_text
c-- channel header
      character*1040 chahead
      character*50 save_head   ! save part of header when looking for resp info
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
      integer nkinhead          ! number of headers in kinemetrics header file
      character*80 text         ! text 
      integer ndef_stat         ! number of stations/lines in def file
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
      integer year1,month1,day1,hour1,min1
      real sec1
c--Counters etc
          integer i,k,kk

      logical no_net                    ! flag if net_code set



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
      deffile='tersei.def'
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
c   read whole file, always assume 3 channels
c
       i=0
       read(1,'(10x,a4)') stat(1)(1:4)
       stat(1)(5:5)=' '
       read(1,'(a)') text
       read(1,'(a)') text
       read(1,'(24x,i2,1x,i2,1x,i4,1x,i2,1x,i2,1x,f6.3)')
     * month(1),day(1),year(1),hour(1),
     * min(1),sec(1)

c       if(year(1).gt.50) then
c         year(1)=year(1)+1900
c       else
c         year(1)=year(1)+2000
c       endif
       year1=year(1)
       month1=month(1)
       day1=day(1)
       hour1=hour(1)
       min1=min(1)
       sec1=sec(1)
c
       do i=1,3
         read(1,'(a)') text
       enddo
c
c   check if GPS corrected
c
       if(text(25:27).eq.'NOT') then
           write(6,*)' No gps signal available, use IDS time'
           time_error='E'
       else
           write(6,*) ' GPS ok, use corrected time'
           read(text,'(24x,5(i2,1x),f7.3)',err=98)
     *     month(1),day(1),year(1),hour(1),min(1),sec(1)

           if(year(1).gt.50) then
              year(1)=year(1)+1900
           else
              year(1)=year(1)+2000
           endif
           goto 200
 98        continue
           write(6,*)' Corrupted corected time, use IDS time'
           time_error='E'
c
c   put back old
c
           year(1)=year1
           month(1)=month1
           day(1)=day1
           hour(1)=hour1
           min(1)=min1
           sec(1)=sec1

 200       continue
           
        endif
c
       do i=1,3
         read(1,'(a)') text
       enddo

c
c sample rate
c
       kk=0
       if(text(30:40).ne.' ') then
       do i=1,70
         if(text(i:i+3).eq.'SPS=') k=i
       enddo
       read(text(k+4:k+6),'(i3)') i
       else
c       if(i.eq.0) then        ! try next
          read(1,'(a)') text
          read(1,'(a)') text
          do i=1,70
            if(text(i:i+3).eq.'SPS=') k=i
          enddo
          read(text(k+4:k+6),'(i3)') i
          kk=2
       endif
       
       rate(1)=i
c
c   number of samples
c 
       read(1,'(i10)') nsamp(1)
c   
       do i=1,5-kk
         read(1,'(a)') text
       enddo
       
       nchan=1
c
c   read first channel of data
c
c       read(1,'(a)') text
c       write(6,*) text
c       backspace 1
       do i=1,nsamp(1)
       read(1,'(12x,3f18.6)',end=988),x1,x2,x3

       data1(i)=x1*1000
       data2(i)=x2*1000
       data3(i)=x3*1000
       enddo
c
c   try to read more channels, if end of file only 1
c
c       read(1,'(a)',end=988) text
c       read(1,'(a)',end=988) text
c       read(1,*,end=988)(data2(i),i=1,nsamp(1))
c       nchan=2
c
c       read(1,'(a)',end=988) text
c       read(1,'(a)',end=988) text
c       read(1,*,end=988)(data3(i),i=1,nsamp(1))
       nchan=3
c
 988  continue
       write(6,*) i

       write(6,*) 'nchan=',nchan,'   nsamp=',nsamp(1),'   rate',rate(1)
c
c  enter loop to define header info
c
       cbyte(1)='4'
       do ichan=1,nchan
c
c   first put in defaults in case no def file
c
          
            stat(ichan)=stat(1)
           
            if(ichan.eq.1) comp(ichan)='S  Z'
            if(ichan.eq.2) comp(ichan)='S  N'
            if(ichan.eq.3) comp(ichan)='S  E'

c
c   assume same start and stop time and sample rate for all channels
c            
         year(ichan)=year(1)
         month(ichan)=month(1)
         day(ichan)=day(1)
         hour(ichan)=hour(1)
         min(ichan)=min(1)
         sec(ichan)=sec(1)
         rate(ichan)=rate(1)
         cbyte(ichan)='4'
         nsamp(ichan)=nsamp(1)
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
c
c   put in timing status
c
        chahead(29:29)=time_error

        write(2)chahead
        write(6,'(1x,a)') chahead(1:70)
        if(ichan.eq.1) write(2) (data1(i),i=1,nsamp(ichan))
        if(ichan.eq.2) write(2) (data2(i),i=1,nsamp(ichan))
        if(ichan.eq.3) write(2) (data3(i),i=1,nsamp(ichan))

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
								  
