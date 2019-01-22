c$debug
c
c   converts geophysical instittute of israel bukletin files to nordic format
c
c   jh august 1998
c   updates:
c   oct 6  jh : fix blank station
c   june 1999 jh ----------------  version 7 -----------------------
c   oct 2 2001 change SP to SZ


c
       implicit none
C  array with original readings
      character*80 data(1000)
      character*80 text
      character*3 agency
      character*4 trms
      integer idist                ! distance
      real dist                    ! distance
      character*80 infile
      real mag1                    ! magnitude
c  number of different stations in data
      integer nstat
      integer year,month,day,hour,min
      real sec
c  decimal latitude and longitude
      real dlat,dlon
c  depth,  rms of residuals
      real depth,rms
      integer i,NEVENT,ID


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      nevent=0

c
      write(6,*)' Input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old')
      open(2,file='giinor.out',status='unknown')
c
c   next event
c
 50   continue
c
c   find next header line
c
      read(1,'(a)',end=99) text
      if(text(1:5).ne.'=====') goto 50
c
c   event reading loop
c

 51   continue
      read(1,'(a)',end=99) text
      nevent=nevent+1
      read(text(7:10) ,'(i4)') year
      read(text(12:13),'(i2)') month
      read(text(15:16),'(i2)') day

      if(text(20:20).eq.':') then
         read(text(18:19),'(i2)') hour
         read(text(21:22),'(i2)') min
         read(text(24:29),'(f6.3)') sec
      else
         read(text(19:20),'(i2)') hour
         read(text(22:23),'(i2)') min
         read(text(25:30),'(f6.3)') sec
       endif
c
c   lat,lon,depth
c
      read(1,'(a)') text
      read(text(7:12), '(f6.3)') dlat
      read(text(22:28),'(f7.3)') dlon
      read(text(38:42),'(f5.1)') depth
      if(text(14:14).eq.'S') dlat=-dlat
      if(text(30:30).eq.'W') dlon=-dlon
      write(6,*) dlat,dlon,depth
c
c   magnitude
c
      read(1,'(3x,f4.1)') mag1
c
c  rms
c
      read(1,'(4x,f7.3)') rms
c
c   write header line
c
         do i=1,80
           data(1)(i:i)=' '
         enddo

         id=1

         agency='XXX'
         write(data(1)(2:20),'(i4,1x,2i2,1x,2i2,1x,f4.1)')
     *   year,month,day,hour,min,sec
         trms='    '
         if(rms.gt.0.0)write(trms,'(f4.1)') rms
         if(nstat.gt.999) nstat=999
         write(data(1)(46:55),'(a3,i3,A4)') agency,nstat,trms
         write(data(1)(24:43),'(f7.3,f8.3,f5.1)') dlat,dlon,depth
         write(data(1)(57:63),'(f3.1,a4)')mag1,agency 
         data(1)(22:22)='L'
c
c   6 input lines
c
      do i=1,6
        read(1,'(a)') text
      enddo
c
 20   continue
c
c   read phases
c
      read(1,'(a)') text
      if(text(1:5).eq.'=====') then
        write(2,'(a)') (data(i),i=1,id)
        write(2,*)'                                 '
        go to 50    ! next event
      endif
c
      id=id+1
      do i=1,80
        data(id)(i:i)=' '
      enddo
c
      data(id)(2:5)=text(2:5)              ! station
      if(data(id)(2:5).eq.'    ') data(id)(2:5)=data(id-1)(2:5)
      data(id)(7:8)='SZ'
      data(id)(10:14)=text(28:32)          ! phase
      data(id)(15:15)=text(52:52)          ! weight
      if(text(35:35).eq.'d') data(id)(17:17)='D'   ! polarity
      if(text(35:35).eq.'c') data(id)(17:17)='C'
      if(text(38:38).eq.':') then
         data(id)(19:20)=text(36:37)          ! hour
         data(id)(21:22)=text(39:40)          ! min
         data(id)(24:28)=text(42:46)          ! sec
      else
         data(id)(19:20)=text(37:38)          ! hour
         data(id)(21:22)=text(40:41)          ! min
         data(id)(24:28)=text(43:47)          ! sec
      endif
      data(id)(64:68)=text(54:58)          ! residual
      read(text(15:20),'(f5.1)') dist      ! distance
      idist=dist+0.5
c
c   only write dist and axz for p-phase, s has none
c
      if(text(29:29).ne.'S') write(data(id)(71:75),'(i5)') idist
      if(text(29:29).ne.'S') data(id)(77:79)=text(23:25)          ! azimuth
c
c   next phase
c
      goto 20

 99   continue

      write(6,*)' Number of events converted', nevent
      write(6,*) 'Output file name is giinor.out'
      stop
      end 
