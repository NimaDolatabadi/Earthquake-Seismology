c$debug
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  KINNOR.FOR
c
c  Converts .PCK file output of EDPPICK to to file in SEISAN format. 
c  Many events are converted from one file.
c  The program is based on program from Kinemetrics by:
c
c  Christopher S. Lim, Kinemetrics Systems
c  Date: June 15, 1987
c
c  Revision A
c  by M. Ciudad-Real
c  Date: March 27, 1996
c
c  SEISAN version by J. Havskov, November 1997
c  
c
c  Assumptions: First 3 chars of station name is station, last character
c               is component. All component are assumed to be SP. So 
c               e.g. LPGZ is converted to LPG  S  Z
c               The coda given with S is not used.
c               If a P or S field is blank, no travel time is given.
c               A new event start with c* St
c               There is no check if minute > 60, however it is now 
c               checked for both P and S that second is not more than 60.
c               Polarity U is changed to C.
c               waveform file names can be generated assuming file name
c               corresponds to time in input file header.
c
c   updates:
c  april 13, 1999 jh : ----------- version 7.0 check ---------------
c                      year 2000, long wave form file name
c  jun 99            : small format bug
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      implicit none
      character*256 line  
      character*80 infil,text
      character*80 wavname
      character*1 ppol,spol 
      character*5 code             ! waveform agency code
      integer ichan                ! number of channels in waveform file
c     character*6 cdate
      real lat,lon,depth           ! hypocenter
      integer ndata,iyear,imonth,iday,i,day
      character*4 sta
      real ptime,stime,psec,second,ssec
      integer pwt,swt,iminute,ihour,iocheck,pmin,smin
      real ftime,fsec


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c      
c
c---- Get input file name
c
      iocheck = 0
50    write(6,*) ' '
      write(6,'(1x,a)') 'Enter input filename: '
      read(5,'(a)') infil
      write(6,*) 'Enter 1-5 letter code for automatic waveform name',
     *' generation, return for none'
      read(5,'(a)') code
      if(code.ne.' ') then
         write(6,*)' Number of channels in waveform file'
         read(5,*) ichan
      endif
      open(unit=10,file=infil,status='OLD',iostat=iocheck,err=50)
      open(unit=11,file='kinnor.out',status='UNKNOWN')
c
      write(6,*) ' '  
      write(6,'(1x,2a)') 'Reading phase list from file: ',infil(1:50)
c
c------------------------------------------------------------------
c  here starts reading of a new event, loop
c------------------------------------------------------------------
c
      ndata = 0
 1000 continue
c
c
c---- Read in time information line
c
90    read(10,400,end=9000) line
  400 format(a)
      if (line(1:1) .ne. ' ') goto 90
c
c   header
c
      read(line,100) iyear,iday,ihour,iminute,second,lat,lon,depth
100   format(2x,i4,1x,i3,1x,i2,1x,i2,1x,f6.3,13x,f8.4,f10.4,f6.1)
c
c.....Calculate year,month,day     
c
c     call date(iyear,imonth,iday)
      call DTE (iday,DAY,imonth,iyear)
      iday=day

c
c....Calculate date
c
c     kdate = (iyear - 1900)*10000+imonth*100+iday
c     write(cdate,'(i6)') kdate
c     if (iyear-1900 .lt. 10) cdate(1:1) = '0'
c     if (iyear-1900 .eq. 0) cdate(2:2) = '0'
c     if (imonth .lt. 10) cdate(3:3) = '0'
c     read(cdate(5:6),'(i2)') kdate
      
c
c   write nordic header
c
      text=' '
      write(text,'(1x,i4,1x,2i2,1x,2i2,1x,f4.1,1x,a
     *)')iyear,
     *imonth,iday,ihour,iminute,second,'L'
      if(.not.(lat.eq.0.0.and.lon.eq.0.0.and.depth.eq.0.0))
     *write(text(24:43),'(f7.3,f8.3,f5.1)') lat,lon,depth
      text(80:80)='1'
      write(11,'(a)') text
c
c   make waveform file name id desired
c
      if(code.ne.' ') then
c        if(iyear.lt.100) then
c          i=iyear
c        else
c          i=iyear-1900
c        endif
         write(wavname,'(i4,a1,i2,a1,i2,a1,2i2,a1,i2,a2,a5,a1,i3)')
     *   iyear,'-',imonth,'-',iday,'-',ihour,iminute,'-',
     *   int(second),'S.',code,'_',ichan
         do i=1,19
           if(wavname(i:i).eq.' ') wavname(i:i)='0'
         enddo
         do i=27,29
           if(wavname(i:i).eq.' ') wavname(i:i)='0'
         enddo
         do i=20,26
           if(wavname(i:i).eq.' ') wavname(i:i)='_'
         enddo
         text=' '
         text(2:30)=wavname
         text(80:80)='6'
         write(11,'(a)') text
      endif
      ndata = ndata + 1
c              
c.....Read in phase list
c
110   read(10,400,end = 9000) line
      if (line(1:5) .eq. 'c* St') then
         write(11,'(a)') '   '            ! event separater
         goto 1000    ! end of one event
      endif
      if (line(1:1) .ne. ' ') goto 110
      read(line,120) sta,ptime,pwt,ppol,stime,swt,spol,ftime
120   format(7x,a4,1x,f8.3,1x,i1,1x,a1,1x,f8.3,1x,i1,1x,a1,2x,f8.3)      
c
c
      text=' '
c
c   for station assume first 3 chars is station and last char component
c
      text(2:4)=sta(1:3)
      text(7:7)='S'
      text(8:8)=sta(4:4)
c
c.....Calculate and write P-arrival time if field not blank
c
      if(line(15:22).ne.'        ') then
         psec = second + ptime
         pmin = iminute
125      if (psec .ge. 60.) then 
           psec = psec - 60.
           pmin = pmin + 1
           goto 125
         endif
c
c   phase
c
         text(10:11)='IP'
         write(text(19:28),'(2i2,1x,f5.2)') ihour,pmin,psec
c
c   calculate coda if there, p must be there
c
         if(ftime.gt.ptime) then
            fsec = ftime - ptime         ! coda length
            i=fsec+0.5
            write(text(30:33),'(i4)') i
         endif
c
c   polarity and weight
c
         if(ppol.eq.'U') ppol='C'        
         text(17:17)=ppol
         write(text(15:15),'(i1)') pwt
c
c   write P-line
c
         write(11,'(a)') text

      endif

c
c.....Calculate and  write S-arrival time if field not blank, assune no 
c     polarity with S and no coda
c        
      if (line(28:33).ne.'      ') then
         text(9:80)=' '   ! assume same station and component
         smin = iminute
         ssec = stime + second
126      if (ssec .ge. 60.) then 
           ssec = ssec - 60.
           smin = smin + 1
           goto 126
         endif
c
c   phase
c
         text(10:11)='IS'
         write(text(19:28),'(2i2,1x,f5.2)') ihour,smin,ssec
c
c   weight
c
         write(text(15:15),'(i1)') swt
c
c   write S-line
c
         write(11,'(a)') text

      endif
      goto 110
c
c---- End of file encountered
9000  continue
c      
      close(10)
      close(11)
      write(6,'(1x,/,a,1x,i5)') ' Number of events converted:',ndata
      write(6,*)'Output file name is kinnor.out'
      stop
      end
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  DATE(iyear,imonth,iday)
c
c     Given day of year, calculates month and day
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine date(iyear,imonth,iday)
      integer iyear,imonth,iday
      integer imon(12)
      data imon/31,28,31,30,31,30,31,31,30,31,30,31/
c
c... Hypo71 assumes 20th century
c
      iyear = mod(iyear,100) + 1900
c
c....Leap years: Century years divisible by 400
c                Other years divisible by 4
      if ((mod(iyear,100) .eq. 0 .and. mod(iyear,400) .eq. 0) .or.
     & (mod(iyear,100) .ne. 0 .and. mod(iyear,4) .eq. 0) ) then
        imon(2) = 29
      else
        imon(2) = 28
      endif  
c
      imonth = 1
100   if (iday .le. imon(imonth)) return
      iday = iday - imon(imonth)
      imonth = imonth + 1
      goto 100
      end               

