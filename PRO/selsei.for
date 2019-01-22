c
c  program to  read headers in wav files
c
c  j. havskov, march 93
c  Updates:
c  aug 20 93 by jh: clean up                
c  may 95         : more than 30 chan
c  nov 1 95       : use sesinc
c  -----------------------------------------------------------------------
c  oct 17 98 jh   : --------------   version 7.0 check ------------------
c                   year 2000, 5 char station
c
c
c  program is for sun, pc and vax, look for word COMPUTERTYPE, and ajust
c  as shown
c
      implicit none
      include 'seidim.inc'
c-- main header
      character*80 mainhead(max_trace)
      character*1040 chead             ! channel header
c-- input file names
      character*80 infile
c-- number of files, one event
      integer ifile	  
c-- station counter
      integer istat,nstat	  
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)	  
c-- main header date and time
      integer myear,mmonth,mday,mhour,mmin
      real msec	  
c-- channel start time and interval
      real cstart(max_trace)
      double precision cinter(max_trace)
c-- counters
      integer i,l,k,nout,nchan
c-- station to select
      character*5 station



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c	  	  
      nout=0
c
c  open output file
c
      open(8,file='selsei.out',status='unknown')
c
      write(6,*)' This program assumes that there is a filenr.lis file'
      write(6,*)' of waveform files to search'
      write(6,*)
      write(6,*)'Give station to find'
      read(5,'(a)') station
c
c   open file with file names
c	  	  
      open(7,file='filenr.lis',status='old',err=10)
      goto 11
 10   continue
        write(6,*)' No filenr.lis file, make a dirf first'
        stop
 11   continue
c
c   read headers 
c
      ifile=1
 20   continue 		
      read(7,'(7x,a)', end=999) infile
      write(6,'(a)') infile 
c-- check if end of group of events 	  
      if(infile(1:4).eq.'    ') goto 999  	   	  	  
c
c   now read headers for one event
c	  
      open(1,file=infile,status='old',access='direct',recl=2048)
       call seisinc                                                             
     *(1,0,nchan,0,MAINHEAD,chead,0.0,0.0)                     
c
c-- get number of stations
      read(mainhead(1)(31:33),'(i3)',err=95) istat
c-- date and time of first channel
      read(mainhead(1)(34:59),'(i3,5x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3)'
     *,err=95)
     *k,mmonth,mday,mhour,mmin,msec 	   	  	 
      myear=k+1900
c
c-- get channel codes, start times and durations
c
      nstat=1
      do i=3,(istat-1)/3+3
         read(mainhead(i),'(3(1x,2a4,a1,f7.2,1x,f8.2))',err=95)
     *   stat(nstat)  (1:4),comp(nstat),  stat(nstat)  (5:5),
     *   cstart(nstat),cinter(nstat),
     *   stat(nstat+1)(1:4),comp(nstat+1),stat(nstat+1)(5:5),
     *   cstart(nstat+1),cinter(nstat+1),	 
     *   stat(nstat+2)(1:4),comp(nstat+2),stat(nstat+2)(5:5),
     *   cstart(nstat+2),cinter(nstat+2)	 
         nstat=nstat+3		   		 
      enddo
      goto 96
 95   continue
      write(6,*)' Error with internal read$$$$$$$$$$$$$$$$$$$'
      close(1)
      goto 20
 96   continue
c
c   find if station is present
c
      do i=1,istat
         if(stat(i).eq.station) then
           write(8,'(7x,a)') infile
           write(6,*)'Selected *********************************'
           nout=nout+1
           goto 15
         endif
      enddo
 15   continue
      close(1)	  
c-- go to get next event file
      ifile=ifile+1
      goto 20
 999  continue
      write(6,*)
      write(6,*)nout,' events selected, see file selsei.out'
      stop
      end	  	  	 
