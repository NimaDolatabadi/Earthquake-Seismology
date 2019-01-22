c$debug

      subroutine mfhead(year,month,day,hour,min,sec,window,nchan,
     *                 net_code,stat,comp,cstart,cinter,file,mainhead)
c
c   make a seisanr main header and file name, output is
c   filename file and main header
c
c
c   jan 95  ny jh   ***************** version 5.0 ************************
c   sep 98 jh       ------------ version 7.0 check ----------------------
c                   eliminate pc part
c                   change output file name to use century, 5 char network code,
c                   5 char station codes in header
c   oct 6   bmt     line 82 linux changed 
c   dec 20  jh      if blank net code, replace with _
c   may 2000 jh     alow time intervals up to 999999 secs
c
c   aug 22 by jh : filename to 80 chars
      implicit none
      include 'seidim.inc'
c-- main header
      character*80 mainhead(max_trace)
c-- output file name 
      character*80 file	  
c-- number of stations
      integer nchan	  
c-- total time window of file
      real window
c-- network code
      character*5 net_code	  
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)	  
c-- main header date and time
      integer year,month,day,doy,hour,min
      integer year_seisan ! year - 1900
      real sec	  
c-- channel start time and interval
      real cstart(max_trace)
      real cinter(max_trace)
      integer i,j,k,l
c
c   make main header, clear first
c
      do i=1,max_trace
         do j=1,80
            mainhead(i)(j:j)=' '
         enddo
      enddo		 					 	  
c-- name or code
      mainhead(1)(2:6)=net_code
c-- number of channels
      write(mainhead(1)(31:33),'(i3)') nchan
c-- start time
c
c  calculate doy
c
      call date_doy(doy,day,month,year)
c	  
c      write(6,*) hour
c
c   subtract 1900 to be able to write year in 3 places
c
       year_seisan=year-1900
c
      if(window.gt.99999.0) then
         write(mainhead(1)(34:69),'(i3,1x,i3,1x,i2,1x,i2,1x,i2,1x,
     *   i2,1x,f6.3,1x,f9.2)',err=464) year_seisan,doy,month,
     *   day,hour,min,sec,window
      else
         write(mainhead(1)(34:69),'(i3,1x,i3,1x,i2,1x,i2,1x,i2,1x,
     *   i2,1x,f6.3,1x,f9.3)',err=464) year_seisan,doy,month,
     *   day,hour,min,sec,window
      endif
      goto 465
 464  continue
      write(6,*)' Something wrong with dates etc,'
     *,'yy,doy,mm,dd,hh,mm,ss,window'
      write(6,'(i4,1x,i3,1x,i2,1x,i2,1x,i2,1x,
     *i2,1x,f6.3,1x,f9.3)',err=464) year,doy,month,day,hour,min,sec,
     *window
      stop
 465  continue
 
c
      l=2
      k=3	  
      do j=1,nchan
        if(cinter(j).gt.99999.0) then
           write(mainhead(k)(l:l+24),'(2a4,a1,f7.2,1x,f8.1)')	  	 
     *     stat(j)(1:4),comp(j)(1:4),stat(j)(5:5),cstart(j),cinter(j)   
        else
           write(mainhead(k)(l:l+24),'(2a4,a1,f7.2,1x,f8.2)')	  	 
     *     stat(j)(1:4),comp(j)(1:4),stat(j)(5:5),cstart(j),cinter(j)   
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
c-- year                          
         write(file(1:4),'(i4)') year
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
c-- network or station code                  
         file(21:25)=mainhead(1)(2:6)		
         do i=21,25
           if(file(i:i).eq.' ') file(i:i)='_'
         enddo
         file(26:26)='_'
c-- number of channels
         file(27:29)=mainhead(1)(31:33)
c-- check for blanks                                        
         do i=1,29				
            if(file(i:i).eq.' ') file(i:i)='0'                            
         enddo                                                                  
c         write(6,200) file(1:29)                                             
c 200     format(/,' Output file name is: ',a23)                                
      return
      end
