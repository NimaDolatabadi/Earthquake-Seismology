      subroutine systime(t,t1)
c
c   routines returns system date and time in string t in a simple form
c   and in t1 nicely arranged    	  	  
c
c   updates
c
c   sep 98 by jh : ---------- version 7.0 check -----------------
c                  change for year 2000
c   nov 4 ,98 jh : linux logic
c   jan 3 2000   : wrong year for year 2000
c   jan 26       : above was only fixed on sun !!
c   dec 18 2010  : gfortran on pc, no longer pc and linux version
c                  
      implicit none
c
      character*12 t
      character*14 t1
      integer*2 hr,min,sec,year,mon,day,sec100
      integer time(3),date(3),i

      call itime(time)
      call idate(date)
c
c   assume 1900
c
         if(date(3).ge.2000) then
            date(3)=date(3)-2000
         else
            date(3)=date(3)-1900
         endif
         write(t,'(6i2)') 
     *   date(3),date(2),date(1),time(1),time(2),time(3)

c
c   check for blanks
c
       do i=1,12
          if(t(i:i).eq.' ') t(i:i)='0'
       enddo
c
c   set up nicely
c
       t1(1:2)=t(1:2)
       t1(3:3)='-'
       t1(4:5)=t(3:4)
       t1(6:6)='-'
       t1(7:8)=t(5:6)
       t1(9:9)=' '
       t1(10:11)=t(7:8)
       t1(12:12)=':'
       t1(13:14)=t(9:10)
       
      return
      end	  	  	  

      subroutine datetimex(dattim)
c
c routine to give data and time, replaces routine in velest
c
      implicit none
      character*12 time1
      character*14 time2
      character*20 dattim
     
      dattim = ' '
      call systime(time1,time2)
      dattim(1:14) = time2

      return
      end

