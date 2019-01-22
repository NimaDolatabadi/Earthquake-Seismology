      subroutine sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 file,mainhead,chahead)
c
c
c   make a seisan main header, one channel header  and file name
c
c   updates
c
c   sep 98 by jh : ----------  version 7.0 check ----------------------
c                  correction for year 2000
c                  5 char station names
c                  5 char network name
c   nov 5 98  jh : remover computer type, not needed
c   march 17, 99 jh: bug with file name, change to more than 32 channesl
c                    put in mainhead_text, change how net name is used
c   may 24         : small change to net_CODE
c   sep 99         : do not alter net code on return
c   apr 2000 jh    : change realistic time window to 999000 secs
c   may 15 2000 jh : make sure channel numbe rin file name always is 3 digitis
c                    like 003
c   jun 30 2003 lo : increased precision on sample rate if less than 1 Hz
c   nov 19 2003 jh : number fo samples to i6
c   aug 6 2008  jh : adjust duration by one sample, now nsamp/rate
c
c
c
c   input: year, month...  : date etc of each channel
c          nchan:          : number of channels
c          ichan:          : channle to calculate channel head for
c          net_code        : network code for file name
c          mainhead_text   : text for main header, if no text, net_code used
c          stat,comp       : station and channels for each channel
c          nsamp           ; number of samples each channel
c          rate            ; sample rate -----------------
c          cbyte           : 2 or 4 byte
c  output: file            : file name
c          mainhead        : main header
c          chahead         : channel header for ichan
c 
c
c  If a network name is given in net_code, that is used in file name. If no
c  network name is given, station code if first station is used
c           
c          
c          
      implicit none
      include 'seidim.inc'
c-- main header
      character*80 mainhead(max_trace)
c-- channel header
      character*1040 chahead
c-- output file name 
      character*80 file	  
c-- number of stations
      integer nchan	  
      character*29 mainhead_text        ! text for main header
c-- channel to calculate channel header for
      integer ichan
c-- network code
      character*5 net_code	  
c-- stations and components
      character*4 comp(max_trace)	  
      character*5 stat(max_trace)
c--channel header date and times	  
      integer year(max_trace),month(max_trace),day(max_trace),
     *doy,hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	  
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)
c-- main header date and time
      integer myear,mmonth,mday,mdoy,mhour,mmin
      real msec
c-- channel start time 
      double precision cstart(max_trace)
c-- channel absolute start time relative earliest channel, from  1900, in sec 
      double precision cabstim(max_trace)     	  	  
c-- channel end time absolute
      double precision cendtim(max_trace)
      real ctim         ! time interval of one channel
c-- maximum and minumum of above and their difference
      double precision first_time,last_time, total_window
      character*5 net_code_local  ! net code used locally
      integer i,j,k,l
c
      net_code_local=net_code

c
c   find abs times for each channel and extreme values
c
      first_time=1.0e20
      last_time=0.0	  
      do i=1,nchan
         call timsec
     *   (year(i),month(i),day(i),hour(i),min(i),sec(i),cabstim(i))
cold         cendtim(i)=cabstim(i)+(nsamp(i)-1)/rate(i)
         cendtim(i)=cabstim(i)+(nsamp(i))/rate(i)  ! jh august 2008
         if(cabstim(i).lt.first_time) first_time=cabstim(i)
         if(cendtim(i).gt.last_time)  last_time=cendtim(i)
      enddo

c
c   find new start times relative to new main header
c
      do i=1,nchan
         cstart(i)=cabstim(i)-first_time
      enddo		 	  
c
c   check if total time window is reasonable
c
      total_window=last_time-first_time
      if(total_window.gt.999000.0) then
         write(6,*)' Total time window is:',total_window,' secs'
         write(6,*)' This is unrealistic, event is skipped'
         stop
      endif		 		 		 	  	   		 		 	  


c
c   make main header, clear first
c
      do i=1,max_trace
         do j=1,80
            mainhead(i)(j:j)=' '
         enddo
      enddo		 					 	  
c-- name or code
      if(mainhead_text.ne.' ') mainhead(1)(2:30)=mainhead_text 
      if(mainhead_text.eq.' '.and.net_code.ne.' ') 
     *mainhead(1)(2:6)=net_code
      if(mainhead_text.eq.' '.and.net_code.eq.' ') 
     *mainhead(1)(2:6)=stat(1)
      if(net_code.eq.' ') net_code_local=stat(1)
c-- number of channels
      write(mainhead(1)(31:33),'(i3)') nchan
c-- start time	  
      call sectim(first_time,myear,mdoy,mmonth,mday,mhour,mmin,msec)
      if(total_window.gt. 99999.0) then 
         write(mainhead(1)(34:69),'(i3,1x,i3,1x,i2,1x,i2,1x,i2,1x,
     *   i2,1x,f6.3,1x,f9.2)') myear-1900,mdoy,mmonth,mday,mhour,mmin,
     *   msec,total_window
      else
         write(mainhead(1)(34:69),'(i3,1x,i3,1x,i2,1x,i2,1x,i2,1x,
     *   i2,1x,f6.3,1x,f9.3)') myear-1900,mdoy,mmonth,mday,mhour,mmin,
     *   msec,total_window
      endif
c
c   write channel info
c	  
      l=2
      k=3	  
      do j=1,nchan
cold        ctim=float(nsamp(j)-1)/rate(j)
        ctim=float(nsamp(j))/rate(j) ! jh aug 2008
        if(ctim.gt.99999.0) then
           write(mainhead(k)(l:l+24),'(2a4,a1,f7.2,1x,f8.1)')
     *     stat(j)(1:4),comp(j),stat(j)(5:5),cstart(j),
     *     ctim   
        else
           write(mainhead(k)(l:l+24),'(2a4,a1,f7.2,1x,f8.2)')
     *     stat(j)(1:4),comp(j),stat(j)(5:5),cstart(j),
     *     ctim   
        endif
        l=l+26
        if(l.gt.60) then
           l=2
           k=k+1
        endif
      enddo			
c                                                                               
c  make file name 
c             
         file=' '
c             
c-- year                          
         write(file(1:4),'(i4)') myear
         file(5:5)='-'               
c-- month                                    
         file(6:7)=mainhead(1)(42:43)		
         file(8:8)='-'
c-- day                                      
         file(9:10)=mainhead(1)(45:46)		
         file(11:11)='-'
c-- hr                                       
         file(12:13)=mainhead(1)(48:49)		
c-- min                                    
         file(14:15)=mainhead(1)(51:52)		
         file(16:16)='-'
c-- sec                                    
         file(17:18)=mainhead(1)(54:55)		
         file(19:20)='S.'
c-- network                   
         file(21:25)=net_code_local
         file(26:26)='_'
c-- number of channels
         file(27:29)=mainhead(1)(31:33)
c-- check for blanks                                        
         do i=1,20				
            if(file(i:i).eq.' ') file(i:i)='0'                            
         enddo    
         do i=27,28
            if(file(i:i).eq.' ') file(i:i)='0'
         enddo
         do i=21,26
            if(file(i:i).eq.' '.or.ichar(file(i:i)).eq.0) file(i:i)='_'
         enddo
c
c   check whole main header for null chars
c
         do i=1,max_trace
            do j=1,80
               if(ichar(mainhead(i)(j:j)).eq.0) mainhead(i)(j:j)=' '
            enddo
         enddo		 					 	  

c
c   make one channel header
c
c
c   clear
c
        do i=1,1040
           chahead(i:i)=' '
        enddo
c
c   station and component
c
        chahead(1:5)=stat(ichan)
        chahead(6:9)=comp(ichan)
c--  year
        write(chahead(10:12),'(i3)') year(ichan)-1900
c
c  calculate doy
c
      call date_doy(doy,day(ichan),month(ichan),year(ichan))
c-- doy
        write(chahead(14:16),'(i3)') doy
c-- month
        write(chahead(18:19),'(i2)') month(ichan)
c-- day
        write(chahead(21:22),'(i2)') day(ichan)
c-- hour
        write(chahead(24:25),'(i2)') hour(ichan)
c-- min
        write(chahead(27:28),'(i2)') min(ichan)
c-- sec
        write(chahead(30:35),'(f6.3)') sec(ichan)
c-- sample rate
c        write(chahead(37:43),'(f7.2)') rate(ichan)
c changed lot 30/06/2003
        if (rate(ichan).gt.1.) then
          write(chahead(37:43),'(f7.2)') rate(ichan)
        else
          write(chahead(37:43),'(f7.5)') rate(ichan)
        endif
c-- number of samples
        write(chahead(44:50),'(i7)') nsamp(ichan)
c-- 2 or 4 byte
        chahead(77:77)=cbyte(ichan)
c
c   check that no null chars in header
c
        do i=1,1040
          if(ichar(chahead(i:i)).eq.0) chahead(i:i)=' '
        enddo
      return
      end
