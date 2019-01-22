
c  converts Turkish national network ascii acceleration records
c  to SEISAN 
c
c  jh  january 2013
c
c   it is assumed that:
c 
c            there is always 3 channels in file
c            input values are in gal, the output is in 1/10 000 gal
c            or 0.001 mm/s2
c            station code is taken from file
c
c            output file with hypocenter info from input files  
c  updates:
c  
c
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
c-- input data vector	  
      real    rdata1(max_sample),rdata2(max_sample),rdata3(max_sample)
      integer x(max_sample)
      integer seiclen

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
      integer format_file      ! 1 seisan, 2 mseed
c
c   hypocenter info
c
      integer year1,month1,day1,hour1,min1,sec1
      real lat,lon,depth,mag
      character*50 location	
      character*80 head_old
      integer nfile           ! number of files converted  
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
      character*80 text,text1   ! text 
      character*1 time_error    ! if blank, no error

      integer appendflag        ! for seed
      character*3 seed_comp     ! seed channel
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
      integer i,j,l

c
c print version
c
      include 'version.inc'
      out_version_date='Jan, 2013'
      if (version_new) out_version_date=version_date
      call print_ver
      head_old=' '
c
      nfile=0
      mainhead_text='AFAD acceleration '
      time_error=' '
c
c   get def file for station codes, give file name
c
      deffile='afadsei.def'
      net_code='AFAD'

      call read_def_chan(deffile,mainhead_text,net_code)
c
c   file with hypocenters
c
      open(3,file='afadsei.out',status='unknown')     

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
 33   continue
      write(6,*)' Give format: 1: seisan, 2: mseed'
      read(5,*) format_file
      if(format_file.ne.1.and.format_file.ne.2) goto 33
c
c   file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') then
           write(3,*)' '
           stop
         endif
      endif
      write(6,'(1x,a)') infile
c
c   open file to read
c
       open(1,file=infile,status='old')

c
c   read and write hypocenter info
c
       read(1,'(a)') text
       read(1,'(26x,a)') location
       read(1,'(26x,i4,5(1x,i2))') year1,month1,day1,hour1,min1,sec1
       read(1,'(26x,f8.5,2x,f8.5)') lat,lon
       read(1,'(26x,f5.2)') depth
       mag=0.0
       read(1,'(26x,f3.1)',err=180) mag
 180   continue 
       write(6,'(i4,5i2,1x,3f7.3,1x,f3.1)') 
     * year1,month1,day1,hour1,min1,sec1,lat,lon,depth,mag
       text=' '
       text1=' '
       write(text1(1:48),
     *'(1x,i4,1x,2i2,1x,2i2,1x,i2,a4,2f8.3,f5.1,2x,a3)') 
     * year1,month1,day1,hour1,min1,sec1,'.0 L',lat,lon,depth,'OLA'
       write(text1(73:75),'(f3.1)') mag
       text1(76:80)='LOLD1'
       if(nfile.ne.0.and.text1.ne.head_old) write(3,*)' '
       if(text1.ne.head_old) then
         write(3,'(a)') text1
         write(3,'(a)') text1
       endif
       text=' '
       text(1:10)=' LOCALITY:'
       text(12:61)=location
       text(80:80)='3'
       if(text1.ne.head_old) write(3,'(a)') text
       head_old=text1
       nfile=nfile+1

c       do i=1,6
c          read(1,'(a)') text
c       enddo
c
c   read station code
c
       read(1,'(26x,a5)') stat(1)
c
c  read next 4 lines
c
       do i=1,4
          read(1,'(a)') text
       enddo

c
c  get times
c

       read(1,'(26x,i2,1x,i2,1x,i4,1x,i2,1x,i2,1x,f4.1)')
     * day(1),month(1),year(1),hour(1),min(1),sec(1)



c
c   read number of samples and time duration
c
      read(1,'(26x,i4)') nsamp(1)

c
c sample rate
c    
       
       read(1,'(26x,f4.2)') rate(1)
       if(rate(1).eq.0.0) rate(1)=0.01
       rate(1)=1.0/rate(1)
c  
       do i=1,4
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
c   read data
c
       do i=1,nsamp(1)
          read(1,'(3f15.6)',err=200) rdata1(i),rdata2(i),rdata3(i)
c           write(6,*) i,rdata1(i)
           goto 300
 200       continue
c
c   put in zero if error
c
           rdata1(i)=0.0
           rdata2(i)=0.0
           rdata3(i)=0.0
           write(6,*)'error in line ',i
 300       continue
       enddo


c
c  enter loop to define header info
c
       do ichan=1,nchan
c
c   first put in defaults
c
          
         stat(ichan)=stat(1)          
         if(ichan.eq.1) comp(ichan)='HN N'
         if(ichan.eq.2) comp(ichan)='HN E'
         if(ichan.eq.3) comp(ichan)='HN Z'
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
         nsamp(ichan)=nsamp(1)
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
      
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,
     *                 stat,comp,nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   write response file for each channel
c
      do i=1,nchan
         text(1:5)=stat(i)
         text(6:9)=comp(i)
         text(10:29)='.1900-01-01-0000_SEI'
         do l=1,9
            if(text(l:l).eq.' ') text(l:l)='_'
         enddo
         open(9,file=text(1:29),status='unknown')
         text=' '
         write(text(1:9),'(a,a)') stat(i),comp(i)
         text(78:78)='P'
         write(9,'(a)') text
         text=' '
         write(9,'(a)') text
         write(9,'(a)')
     *'     0    2 0.1000E+07  0.000      0.000      0.000      0.000'

         text=' '
         do l=1,10
            write(9,'(a)') text
         enddo
         close(9)

      enddo
c
c------------------------------------------------------
c   write out seisan format
c-------------------------------------------------------
c
      if(format_file.eq.1) then
c
c   open output file
c

      open(2,file=outfile,status='unknown',form='unformatted')	  
      mainhead(1)(2:29)=mainhead_text

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
c   network code
c
        chahead(17:17)='T'
        chahead(20:20)='U'
c
c   response, zeros for accelrometer, gain to get nm/s**2
c
        chahead(78:78)='P'
        chahead(161:222)=
     *'     0    2 0.1000E+07  0.000      0.000      0.000      0.000'

        write(2)chahead
        write(6,'(1x,a)') chahead(1:70)
        if(ichan.eq.1) write(2) (int(10000.0*rdata1(i)),
     *  i=1,nsamp(ichan))
        if(ichan.eq.2) write(2) (int(10000.0*rdata2(i)),
     *  i=1,nsamp(ichan))
        if(ichan.eq.3) write(2) (int(10000.0*rdata3(i)),
     *  i=1,nsamp(ichan))
c
c   end of channels loop
c
      enddo

      write(6,*)
      close(2)
      endif

      if(format_file.eq.2) then
c
c-----------------------------------------------------------
c  mseed format
c-----------------------------------------------------------
c
          do ichan=1,nchan
            seed_comp(1:2)=comp(ichan)(1:2)
            seed_comp(3:3)=comp(ichan)(4:4)
c
c   transefer data to integer array
c
            do l=1,nsamp(ichan)
              if(ichan.eq.1) x(l)=int(10000.0*rdata1(l))
              if(ichan.eq.2) x(l)=int(10000.0*rdata2(l))
              if(ichan.eq.3) x(l)=int(10000.0*rdata3(l))
            enddo     
c
c  Append flag        0 - Replace    1 - Append
c
            if(ichan.eq.1) then
              appendFlag = 0
            else
              appendFlag = 1
            endif

c
            outfile(19:19)='M'   ! indicate seed file

            text=' '
            call seed_channel_write(outfile,
     *      stat(ichan), seed_comp, 'TU', '  ',
     *      year(ichan),month(ichan),day(ichan),hour(ichan),min(ichan),
     *      sec(ichan),rate(ichan),
     *      nsamp(ichan),x,
     *      appendFlag,0,4096,text)
          enddo
      endif
c
c   write filename in s-file
c
      text=' '
      text(2:seiclen(outfile) + 1)=outfile(1:seiclen(outfile))
      text(80:80)='6'
      write(3,'(a)') text
      write(6,'(a22,a40)')' Output file name is: ',outfile(1:40)
c      
c
c
c  back for next file if many
c
      if (in.eq.1) goto 1000                                                               
      goto 19
c
      write(6,*)
      write(6,*) ' Number of files converted: ',nfile
      WRITE(6,*)
     *' Output file with hypocenter information is afasei.out'
      stop
      end	   	  	  	         	  	  
								  
