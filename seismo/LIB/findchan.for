      subroutine findchan
     *(filehead,stat,com,chan,year,month,day,hour,min,sec,error)
c
c     Routine to find channel number corresponding to a station and component
c     Written by C. Lindholm  and j. havskov, Jan. - Apr -90
c
c     updates
c     oct 95 by jh    : do not read file directly 
c     nov 98 jh       : ----------   version 7.0 check --------------------
c                       5 char station
c     jan 26 lo         fixed bug with reading 5th station character
c     mar 2, 99 jh    : fix year also
c
c     input:       
c                  stat,com	   Station and component
c     output       chan            channel number
c                  year....        date and time of channel
c                  error           0: no error, 1: error, not found
c
       implicit none
       include 'seidim.inc'
c
c-- data file header
      character*80 filehead(max_trace)			
c-- Port and channel focused
      integer chan 				
c-- No. of channels in file
      integer number_of_channels		
c-- Time parameters of eventfile
      integer year,doy,month,day,hour,min	
c-- Time parameters of eventfile
      integer year1,month1,day1,hour1,min1	
c-- ----------------------------
      real    sec,sec1
c-- counters
      integer i,j,k		
c-- Station and component info
      character*5 station(max_trace),stat
      character*4 comp(max_trace),com	
c-- 1: error, 0: no error
      integer error				
c-- berit
      double precision sein(max_trace)
c
c

c
cccccc     read datafile headers     cccccc
c
ccccc     read fileheader info ccccccccccccccccccc
c
      read(filehead(1),50)number_of_channels,year,doy,
     +                            month,day,hour,min,sec
      year=year+1900
50    format(8x,22x,i3,i3,1x,i3,4(1x,i2),1x,f6.3)
      j = 1
c
c  read individual channel delays to calculate channel start times
c
      k=(number_of_channels-1)/3+1
      do i= 3,k+2
         read(filehead(i),100)
     *   station(j)  (1:4),comp(j),  station(j)(5:5),sein(j),  ! bugfix lo
     *   station(j+1)(1:4),comp(j+1),station(j+1)(5:5),sein(j+1),
     *   station(j+2)(1:4),comp(j+2),station(j+2)(5:5),sein(j+2)
100      format(1x,a4,a4,a1,f7.2,10x,a4,a4,a1,f7.2,10x,a4,a4,a1,f7.2)
C
ctest            write(6,201)station(j),sein(j),station(j+1),sein(j+1),
ctest     +      station(j+2),sein(j+2)
 201        format(3(1x,a4,1x,f8.2))
C
        j = j + 3
      enddo
c
c   find channel corresponding to station
c
      error=0
      do i=1,number_of_channels
         if(stat.eq.station(i).and.comp(i).eq.com) then
            chan=i
C
c       WRITE(*,*)year,month,day,hour,min,sec
C
c    berit
            call timadd(year,month,day,hour,min,sec,sein(i),
     +  year1,month1,day1,hour1,min1,sec1)

        year=year1
        month=month1
        day=day1
        hour=hour1
        min=min1
        sec=sec1
C
c      WRITE(6,*)year,month,day,hour,min,sec
C
            return
         endif
      enddo
c
      error=1
      return
      end
