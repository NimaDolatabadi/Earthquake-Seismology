      subroutine shead(year,month,day,hour,min,sec,
     *                 stat,comp,nsamp,rate,cbyte,
     *                 chahead)
c
c   make a seisan channel header, dec 97 
c
c   updates
c   sep 98 jh : --------------    version 7.0 check ---------------
c               year 2000, 5 char station name
c
c   input: year, month...  : date etc of each channel
c          stat,comp       : station and channels for channel
c          nsamp           ; number of samples for channel
c          rate            ; sample rate -----------------
c          cbyte           : 2 or 4 byte
c          chahead         : channel header for ichan
c          
c          
c          
      implicit none
c-- channel header
      character*1040 chahead
c-- stations and components
      character*5 stat
      character*4 comp	  
c--channel header date and times	  
      integer year,month,day,doy,hour,min
      real sec
c-- channel samples and sample rate
      integer nsamp
      real rate	  
c-- channel 2 or 4 byte
      character*1 cbyte
      integer i
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
        chahead(1:5)=stat
        chahead(6:9)=comp
c--  year
        write(chahead(10:12),'(i3)') year-1900
c
c  calculate doy
c
      call date_doy(doy,day,month,year)
c-- doy
        write(chahead(14:16),'(i3)') doy
c-- month
        write(chahead(18:19),'(i2)') month
c-- day
        write(chahead(21:22),'(i2)') day
c-- hour
        write(chahead(24:25),'(i2)') hour
c-- min
        write(chahead(27:28),'(i2)') min
c-- sec
        write(chahead(30:35),'(f6.3)') sec
c-- sample rate
        write(chahead(37:43),'(f7.2)') rate
c-- number of samples
        write(chahead(45:50),'(i6)') nsamp
c-- 2 or 4 byte
        chahead(77:77)=cbyte
      return
      end
