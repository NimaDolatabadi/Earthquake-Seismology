c
c   converts rsa bulletin files to nordic format
c   if a filenr.lis file present with old wav file references, it will
c   then try to make wav names if time is similar
c
c   jh august 2000
c
c   updates
c
c   oct 5, 2000 jh  : fix exp flag
c   nov 10          : put C for coda mag

c
       implicit none
C  array with original readings
      character*80 data(1000)
      integer wyear(20000),wmonth(20000),wday(20000),
     *whour(20000),wmin(20000),wsec(20000) ! s-file names to make wav-names
      character*80 text
      character*3 agency
      character*4 trms
      real erlt,erln,erz           ! errors
      integer gap
      real w                       ! weight used
      integer iw,iwav
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
      real coda    ! coda
      character*40 wavname     ! wav file name
      integer wav_used(20000)  ! wav files found
      double precision twav(20000), tsfil   ! absolute time of sfile and wav
      integer icoda
      integer i,NEVENT,ID,k
      integer nwav       ! number of wav files


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      nevent=0
      iwav=0   ! count files found
c
c  open and read s-file names for wav
c
      i=0
      open(1,file='filenr.lis',status='old',err=888)
 900  continue
      i=i+1
      read(1,'(7x,i4,1x,i2,1x,i2,1x,2i2,3x,i2)',end=905,err=888)
     *wyear(i),wmonth(i),wday(i),whour(i),wmin(i),wsec(i)
c
c   make abs time
c
      call timsec(wyear(i),wmonth(i),wday(i),whour(i),wmin(i),
     *float(wsec(i)),twav(i))

      if(wyear(i).eq.0) goto 905
      goto 900
 905  continue
      i=i-1
      write(6,*)' Number of wav-files', i
      nwav=i
      do i=1,nwav
        wav_used(i)=0   ! nothing found yet
      enddo
      close(1)
 888  continue
     
                                
      write(6,*)' Input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old')
      open(2,file='rsanor.out',status='unknown')
c
c   next event
c
 50   continue
c
c   find next header line
c
      read(1,'(a)',end=99) text
      if(text(17:17).ne.'/') goto 50
c
c   event reading loop
c

 51   continue
c
c  clean header
c
         do i=1,80
           data(1)(i:i)=' '
         enddo

      nevent=nevent+1
      read(text(15:16) ,'(i2)') year
      if(year.lt.2) then
         year=year+2000
      else
         year=year+1900
      endif
      if(text(69:69).eq.'E'.or.text(69:69).eq.'e') data(1)(23:23)='P'  ! explosion
      read(text(18:19),'(i2)') month
      read(text(21:22),'(i2)') day

      read(text(24:25),'(i2)') hour
      read(text(27:28),'(i2)') min
      read(text(30:34),'(f5.3)') sec
c   lat,lon,depth
c
      read(text(37:44), '(f8.4)') dlat
      read(text(46:54),'(f9.4)') dlon
      read(text(56:60),'(f5.1)') depth
c      write(6,*) dlat,dlon,depth
c
c   magnitude
c
      read(text(64:67),'(f4.2)') mag1
      read(1,'(a)') text 
c
c  rms
c
      read(text(1:5),'(f5.2)') rms
      read(text(6:23),'(3f6.2)') erlt,erln,erz
c      write(6,*) erlt
      read(text(31:33),'(i3)') gap
c      write(6,*) gap
c
c   one  input line
c
      read(1,'(a)',end=99) text
      read(text(4:6),'(i3)') nstat
c      write(6,*) nstat
c
c   write header line
c

         id=1

         agency='ADA'
         write(data(1)(2:20),'(i4,1x,2i2,1x,2i2,1x,f4.1)')
     *   year,month,day,hour,min,sec
         trms='    '
         if(rms.gt.0.0)write(trms,'(f4.1)') rms
         if(nstat.gt.999) nstat=999
         write(data(1)(46:55),'(a3,i3,A4)') agency,nstat,trms
         write(data(1)(24:43),'(f7.3,f8.3,f5.1)') dlat,dlon,depth
         write(data(1)(57:63),'(f3.1,a1,a3)')mag1,'C',agency 
         data(1)(22:22)='L'
         data(1)(80:80)='1'
         id=id+1
         data(id)=data(1)   ! save old location
         id=id+1
         data(id)=' '
         write(data(id)(6:8),'(i3)') gap
         write(data(id)(25:43),'(f6.1,f8.1,f5.1)') erlt,erln,erz
         data(id)(2:5)='GAP='
         data(id)(80:80)='E'
         id=id+1
         data(id)=data(id-1)
         data(id)(50:73)=' Old RSA error estimate'
         data(id)(80:80)='3'
c
c   make abs time
c
         call timsec(year,month,day,hour,min,sec,tsfil)
c
c   compare wav file refernces
c
         wavname=' '
         if(nwav.gt.0) then
            do i=1,nwav
               if(dabs(twav(i)-tsfil).lt.100) then
                  wav_used(i)=1    ! indicate this one used
                  id=id+1
                  write(wavname,'(i4,a,i2,a,i2,
     *             a1,2i2,a1,i2,a7)') wyear(i),'-',
     *            wmonth(i),'-',wday(i),'-',whour(i),wmin(i),'-',
     *            wsec(i),'S.REDSA'
                  do k=1,20
                     if(wavname(k:k).eq.' ') wavname(k:k)='0'
                  enddo
                  data(id)=' '
                  data(id)(2:26)=wavname(1:25)
                  data(id)(80:80)='6'
               endif
            enddo
            if(wavname.ne.' ') iwav=iwav+1
         endif
c
         id=id+1
         write(data(id),243)
 243     FORMAT(
     *   ' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO ',
c     *   'SNR AR TRES W  DIS CAZ7')
     *   'AIN AR TRES W  DIS CAZ7')
c
 20   continue
c
c   read phases
c
      read(1,'(a)',end=99) text
       if(text(1:15).eq.'               ') then ! end of phases
           write(6,'(a)') data(1)(1:79)
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
c   P- phase
c
      data(id)(2:5)=text(1:4)              ! station
c      if(data(id)(2:5).eq.'    ') data(id)(2:5)=data(id-1)(2:5)
      data(id)(7:8)='SZ'
      data(id)(10:11)=text(22:23)          ! phase
      data(id)(15:15)=text(20:20)          ! weight
      data(id)(17:17)=text(24:24)          ! polarity
      data(id)(19:20)=text(27:28)          ! hour
      data(id)(21:22)=text(30:31)          ! min
      data(id)(24:28)=text(33:37)          ! sec
      data(id)(64:68)=text(45:49)          ! residual
      read(text(7:12),'(f6.1)') dist       ! distance
      read(text(40:42),'(f3.1)')w
      iw=w*10
      if(iw.gt.99) iw=99
      write(data(id)(69:70),'(i2)') iw
      idist=dist+0.5
c
c
      write(data(id)(71:75),'(i5)') idist
      data(id)(77:79)=text(15:17)          ! azimuth
c
c  S-phase
c
      read(1,'(a)',end=99) text
c
      if(text(22:23).eq.' ') goto 20    ! next station
      id=id+1
      do i=1,80
        data(id)(i:i)=' '
      enddo
c
      data(id)(2:5)=data(id-1)(2:5)              ! station, same as before
      data(id)(7:8)='SZ'
      data(id)(10:11)=text(22:23)          ! phase
      data(id)(15:15)=text(20:20)          ! weight
      data(id)(17:17)=text(24:24)          ! polarity
      data(id)(19:20)=text(27:28)          ! hour
      data(id)(21:22)=text(30:31)          ! min
      data(id)(24:28)=text(33:37)          ! sec
      data(id)(64:68)=text(45:49)          ! residual
      read(text(7:12),'(f6.1)')coda         ! coda
      if(coda.gt.0.0) then
        icoda=coda+0.5
        write(data(id-1)(30:33),'(i4)') icoda
      endif
      read(text(40:42),'(f3.1)')w
      iw=w*10
      if(iw.gt.99) iw=99
      write(data(id)(69:70),'(i2)') iw
      idist=dist+0.5
c
c
      write(data(id)(71:75),'(i5)') idist
      data(id)(77:79)=data(id-1)(77:79)    ! azimuth same as before
c
c   next phase
c
      goto 20

 99   continue
c
c   list files not found
c
      if(nwav.ne.0) then
         write(6,*)
         write(6,*) ' Number of S-files with wav-files ', iwav
         write(6,*) ' Wav files not found'
         do i=1,nwav
            if(wav_used(i).eq.0) write(6,'(2x,i4,2x,5i3)')
     *      wyear(i),wmonth(i),wday(i),whour(i),wmin(i),wsec(i)
         enddo
         write(6,*)
      endif

      write(6,*)' Number of events converted', nevent
      write(6,*) 'Output file name is rsanor.out'
      stop
      end 
