c
c  Converts public seismic network files to SEISAN format
c  channels can be defined using the psnsei.def definition file
c  
c  simple version 
c
c  j. havskov, feb 98
c
c
c  Program assumes that input file only has one channel. Station and
c  componenet name is from standard def file. However, if component is 
c  given as part of the file name, that overrides the componenet given
c  by the def file. The coponenet must be S, L, I or B for type and
c  Z,N or E for orientation and given in input file name in position 10 and 11,
c  repectively. The output file name is standard with component added and
c  also a number for easy sorting to merge with seisei. 
c  
c
c  updates:
c  mar 9, 99 by jh : fix bug with time
c  mar 21          : special fix to get componenet name from file name
c  may 9           : --------------   verision 7.0 ------------------
c                    year 2000, 5 char station, seisna dimension
c  july 9          : use component from suffix
c  may 11  00   lo : use read_resp_head
c  aug 12       lo : read files with more than 65000 samples
c  apr 19  01   jh : accept instrument type in column 12, change it to
c                    upper case if in lowe case
c

      implicit none
      include 'seidim.inc' ! dimensions
c-- output data vector	  
      integer*2  data(max_sample)
      character*1 cdata(max_sample*2)
      character*107 block  ! input file header
      integer*4 nsmp         ! number of samples 
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


      integer year2,month2,day2,hour2,min2
      real sec2
      character*80 text      ! general text
      real time_interval     ! length of file in seconds 
      character*1 o

c------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
          integer i,ndef_stat,x
c-- computer type
      logical pc,sun,linux
c---------------------------
c-- help for swap
       character*2 xs
       character*4 xxs
       integer*2 ixs
       integer*4 ixxs
c -- suffix which is type of instrument
       character*1 com
       integer seiclen

c
      equivalence (ixs,xs)
      equivalence (ixxs,xxs)
      equivalence (cdata,data)


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
       text='psnsei.def'
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
c  open file 
c
       open(1,file=infile,status='old',access='direct',recl=207)
c
c   read header block to get start time etc
c
c
 1     continue
       read(1,rec=1,err=99) block
       close(1)

c       do x=0,106,2
c         xs=block(x:x+1)
c         if (x.eq.0) block(x:x) = ' '
c         if(sun) call swap2_one(ixs)
c         write(*,*) x,ixs
c         if (x/10.eq.int(x/10)) read(*,*)
c       enddo

c
c   number of channels, not given in input file, assume 1
c
       nchan=1
c
c   get file start time
c
c   seconds
       xs=block(20:21)
       if(sun) call swap2_one(ixs)
       sec(1)=ixs
c
       xs=block(22:23)
       if(sun) call swap2_one(ixs)
       sec(1)=sec(1)+ixs/10.0
c   minutes
       xs=block(18:19)
       if(sun) call swap2_one(ixs)
       min(1)=ixs
c   hour
       xs=block(16:17)
       if(sun) call swap2_one(ixs)
       hour(1)=ixs       
c   day
       xs=block(14:15)
       if(sun) call swap2_one(ixs)
       day(1)=ixs
c   month
       xs=block(12:13)
       if(sun) call swap2_one(ixs)
       month(1)=ixs
c   year
       xs=block(10:11)
       if(sun) call swap2_one(ixs)
       year(1)=ixs


c
c   get file end time
c
c   seconds
c
       xs=block(28:29)
       if(sun) call swap2_one(ixs)
       sec2=ixs
       xs=block(30:31)
       if(sun) call swap2_one(ixs)
       sec2=sec2+ixs/10.0
c
c   minutes
c
       xs=block(26:27)
       if(sun) call swap2_one(ixs)
       min2=ixs
c
c   hour
c
       xs=block(24:25)
       if(sun) call swap2_one(ixs)
       hour2=ixs
  
       day2=day(1)
       month2=month(1)
       year2=year(1)       
c
c   get file length
c
       xs=block(32:33)
       if(sun) call swap2_one(ixs)
       nsmp=ixs-100
       
c
c check if more than 65000 samples
c
       if (nsmp.lt.-101) then
         xs(1:2)=block(32:33)
         if(sun) call swap2_one(ixs)
         ixxs=ixs 
         xs(1:2)=block(6:7)
         if(sun) call swap2_one(ixs)
         ixxs=ixs-ixxs
         nsmp=ixxs-100
       elseif (nsmp.eq.-101) then
         xs(1:2)=block(2:3)
         if(sun) call swap2_one(ixs)
         ixxs=ixs
         xs(1:2)=block(4:5)
         if(sun) call swap2_one(ixs)
         ixxs=ixs*65536+ixxs
         nsmp=ixxs-100
       endif
c
c get orientation
c
       comp(1) = '    '
       comp(1)(4:4) = block(40:40)

c
c get location name ( first 4 out of 15 )
c
       stat(1) = '     '
       stat(1)(1:1) = block(58:58)
       stat(1)(2:2) = block(60:60)
       stat(1)(3:3) = block(62:62)
       stat(1)(4:4) = block(64:64)
c       stat(1)(5:5) = block(66:66)

c
c   open again with record length equal to lenght of file
c
       
      open(1,file=infile,status='old',access='direct',
     *recl=nsmp*2+207)
c
c   calculate interval in secs
c
       if(hour2.lt.hour(1)) hour2=hour2+24

       time_interval=(hour2-hour(1))*3600+(min2-min(1))*60+sec2-sec(1)
c 
       write(6,'(a,1x,i4,4i3,f6.3)')' yy,mm,dd,hh,mm,sec',
     * year(1),month(1),day(1),hour(1),min(1),sec(1)
c
       write(6,*)' Data time interval', time_interval
       write(6,*)' Number of samples as given in header', nsmp
c
c   assume same start time etc. for all channels, although right now only
c   one channel
c
       if (infile(8:8).eq.'.') com=infile(9:9)
       if (infile(9:9).eq.'.') com=infile(10:10)
       if (infile(11:11).eq.'.') com=infile(12:12)
       if (infile(27:27).eq.'_') com=infile(29:29)
       if(com.eq.'l') com='L'
       if(com.eq.'b') com='B'
       if(com.eq.'s') com='S'
       if(com.eq.'a') com='A'

       write(comp(1)(1:1),'(a1)') com(1:1)

       do i=1,nchan
          year(i)=year(1)
          month(i)=month(1)
          day(i)=day(1)
          hour(i)=hour(1)
          min(i)=min(1)
          sec(i)=sec(1)
          cbyte(i)=' '
       enddo

c
c change station or component names according to def file
c
       call set_def_chan(1,stat(1),comp(1))

c
c   read all data
c


        read(1,rec=1)   (cdata(i),i=1,nsmp*2+207)
c
c   shuffle bytes
c
        do i=1, nsmp*2
          cdata(i)=cdata(i+207)
        enddo
c
c   swap
c
        if(sun) call swap2(nsmp,data)

        nsamp(1)=nsmp
c
c   calculate sample rate
c
        rate(1)=(nsamp(1)-1)/time_interval        
        write(6,*)' Sample rate     ', rate(1)

c
c   make seisan headers
c
      ichan=1

      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   fix name
c
      outfile(30:30)='_'
      outfile(31:34)=comp(1)
      do i=31,34
         if(outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo        
      write(6,'(a,a)')' Output file name is: ',outfile
c
c   open output file
c
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c

      do i=1,12
         write(2)mainhead(i)
         write(6,'(a80)') mainhead(i)
      enddo
c
C-----------------------------------------
C   enter channel  loop, for the moment one
c-----------------------------------------
c
      do ichan=1,nchan
c
c   make channel header	
c
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                    net_code,mainhead_text,stat,comp,
     *                    nsamp,rate,cbyte,
     *                    outfile,mainhead,chahead)
c
c   write rate a bit more accurate
c
         write(chahead(37:43),'(f7.3)') rate(1)
         if(rate(1).lt.100.0) write(chahead(37:43),'(f7.4)') rate(1)
         if(rate(1).lt.10.0) write(chahead(37:43),'(f7.5)') rate(1)
         write(6,'(1x,a)') chahead(1:78)
c                                                                               
c   get response, save date etc since removed when calling read_resp            
c                                                                               
         call read_resp_head(chahead)                                            
c
c   write data
c
         write(2)chahead
         write(2) (data(i),i=1,nsmp)
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
								  
c
