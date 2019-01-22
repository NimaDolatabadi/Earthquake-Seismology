c$DEBUG
c
c  convert neis files to seisan  format
c
c  j. havskov, december 1994
c
c  updates:
c  feb 14 96 jh: only writing out half the zeroes
c  nov 99      : recompile under version 7.0
c  converts only one input ascii file
c
c  
      implicit none
c-- input data vector	  
      real signal(150000)
c-- input file name	  
      character*80 infile
c-- output file name
      character*80 outfile	  
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
      real lat,lon,height   ! latitude,longitude and height
c--Seisan headers
      character*80 main_head(12)
      character*1040 cha_head
      character*80   chead(13)
      real poles(100),zero(100)      ! poles and zeroes in resp.
      integer npole,nzero            ! # of ------------------
      real norm                      ! normalization factor, resp
c-- sample rate
      real rate	  
c-- total duration of signal
      real total_time
c-- date and time
      character*2 month,day,hour,min
      character*4 year
      integer iyear,imonth,iday,idoy,ihour,imin
c-- day of year
      character*3 doy
c-- start sec
      real sec
      integer isec
c-- station and component
      character*4 comp
      character*5 stat
c-- one line of text
      character*120 text
c--Counters etc
	  integer nsamp,i,k,isamp,line
      equivalence(chead,cha_head)


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c
      open(17,file='scratch')
c
c   get file name
c
      in=0
      question=' File name, # or filenr.lis for all'
      call filename(question,infile)	    
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis') then
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
c
c  open file and read header
c
      open(1,file=infile,status='old')
c
c   find date etc from header
c	   
c-- station 
       read(1,'(a)') text
       stat=text(1:4)
       stat(5:5)=' '
       comp(1:1)=text(6:6)
       comp(4:4)=text(8:8)
       comp(2:3)='  '
       write(17,'(a)') text(9:120)
       rewind 17
       read(17,*) lat,lon,height,iyear,idoy,ihour,imin,sec,rate,norm
c       write(6,*) lat,lon,height,iyear,idoy,ihour,imin,sec,rate,norm
c
c   the normalization constant, should be counts/m
c
       norm=norm*1.0e12 

      rewind 17
c
c   poles and zeroes
c
       read(1,*) npole
       read(1,*)(poles(i),i=1,2*npole)
       read(1,*) nzero
       if(nzero.gt.0) then
          read(1,*)(zero(i),i=1,2*nzero)
       endif
       if(npole+nzero.gt.37) then
           write(6,*)' Too many poles and zeros, edit input file'
           stop
        endif
c
c   read data
c
       nsamp=0
 50    continue
       read(1,*,end=60) isamp
c       write(6,*) isamp
       read(1,*)(signal(i),i=nsamp+1,nsamp+isamp)
       nsamp=nsamp+isamp
       goto 50
c
c   all data read
c
 60    continue
c
c   write seisan file, first make headers
c
c
c   calculate month and day
c
c      if(iyear.gt.1900) iyear=iyear-1900
      call dte(idoy,iday,imonth,iyear)
      write(year,'(i4)') iyear
      write(day,'(i2)') iday
      write(doy,'(i3)') idoy
      write(month,'(i2)') imonth
      write(hour,'(i2)') ihour
      write(min,'(i2)') imin
      write(6,*) ' Number of samples and rate',nsamp,rate 
      write(6,'(i4,1x,4i3,1x,f6.2)') 
     *iyear,imonth,iday,ihour,imin,sec
c
c   blank headers
c
       do i=1,1040
	     cha_head(i:i)=' '
       enddo
       do i=1,12
         do k=1,80
            main_head(i)(k:k)=' '
         enddo
       enddo
       total_time=nsamp/rate	   
c-- number of channels
       main_head(1)(33:33)='1'	   
c-- year
       main_head(1)(35:36)=year(3:4)
c-- month
       main_head(1)(42:43)=month
c-- day of year
       main_head(1)(38:40)=doy
c-- day
       main_head(1)(45:46)=day	   
c-- hr
       main_head(1)(48:49)=hour
c-- min
       main_head(1)(51:52)=min
c-- sec
       write(main_head(1)(54:59),'(f6.3)')sec
c-- total time
       write(main_head(1)(61:69),'(f9.3)') total_time
c-- station code
       main_head(3)(2:5)=stat(1:4)
c-- channel
       main_head(3)(6:9)=comp
c-- start time relative to main header	   	          	   
       main_head(3)(11:17)='    0.0'
c-- total time
       write(main_head(3)(19:26),'(f8.2)') total_time
c                                                                               
c  make file name on
c             
c-- year                          
         outfile(1:4)=year
         outfile(5:5)='-'               
c-- month                                    
         outfile(6:7)=month		
         outfile(8:8)='-'
c-- day                                      
         outfile(9:10)=day		
         outfile(11:11)='-'
c-- hr                                       
         outfile(12:13)=hour		
c-- min                                    
         outfile(14:15)=min	
         outfile(16:16)='-'
c-- sec               
         isec=sec
         write(outfile(17:18),'(i2)') isec		                    
         outfile(19:20)='S.'
c-- network or station code                  
         outfile(21:24)=stat(1:4)		
         outfile(25:25)='-'
c-- normally number of channels, here use component number for easy sorting
c-- when later merging
         if(comp(4:4).eq.'Z') outfile(26:30)='1'//comp(1:4)
         if(comp(4:4).eq.'N') outfile(26:30)='2'//comp(1:4)
         if(comp(4:4).eq.'E') outfile(26:30)='3'//comp(1:4)
c-- check for blanks                                        
         do i=1,27				
            if(outfile(i:i).eq.' ') outfile(i:i)='0'                            
         enddo                                     
         do i=27,30
		    if(outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo                             
         write(6,200) outfile(1:30)                                             
 200     format(/,' Output file name is: ',a30)                                
c
c   open output file
c
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write data
c
      do i=1,12
         write(2)main_head(i)
      enddo
      close(1)
c
c   channel
c
c   make channel header	
c
c-- station
         cha_head(1:4)=stat(1:4)
c-- component
         cha_head(6:9)=comp
c-- data and time
         cha_head(11:35)=main_head(1)(35:59)	  	     	   	   	   	   	   	   	   		 					 	      	   		  
c-- sample rate
         write(cha_head(37:43),'(f7.3)') rate
c-- number of samples
         write(cha_head(45:50),'(i6)') nsamp	  	  
c-- indicate 4 byte integer
         cha_head(77:77)='4'
c-- indicate that poles and zeros are written
         cha_head(78:78)='P'
c
         write(cha_head(161:182),'(1x,2i5,g11.4)') npole,nzero,norm
         k=23
         line=3
         do i=1,npole*2
            write(chead(line)(k:k+10),'(g11.4)') poles(i)
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
         enddo
         do i=1,nzero*2
            write(chead(line)(k:k+10),'(g11.4)') zero(i)
            k=k+11
            if(k.eq.78) then
              k=1
              line=line+1
            endif
         enddo
c
c   open channnel file, first make name
c
         write(2) cha_head
         write(2)(int(signal(i)),i=1,nsamp)
      close(2)
      write(6,*)
c
c  back for next file if many
c
      if(in.gt.0) goto 1000	  	  	  	  		 	     	  
 99   continue
      stop
      end	   	  	  	         	  	  
