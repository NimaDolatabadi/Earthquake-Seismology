
	program seigmt    

c
c program to convert seisan nordic files into gmt psxy input
c
c
c ---------------- new in SEISAN version 7.0 ---------------------
c lo 15 mar 1999: include output for psvelomeca
c lo 16 mar 1999: magnitude order as input
c jh 24 jun 1999: comment out first format statement below
c lo 16 sep 1999: small fixes
c lo 16 may 2005: added output for station coordinates and travel paths 
c jh 21 oct 2005: write out -dep as f6.1 instead of f5.1
c jh 24 may 2007: increaze dimesion of data array
c pv 2010-08-18 : increaze dimesion of file from 40 to 80
c

      implicit none

      character*80 file
      real scale
      real lat,lon,magout,mag(5),dep,stlat,stlon,stel
      character*1 cmag(5)
      integer nevent,nfp
      logical compact
      real strike,dip,rake
      character*8 datetext

c-- event arrays
      character*80      data(7000)    ! jh may 2007, increased
c-- no of stations for event
      integer           nstat
c-- no of phases for one event
      integer nphase
c-- no of records for event
      integer           nrecord
c-- no of headers for event
      integer           nhead
c-- event ids--------------
      character*1       exp,evid
      integer id,l,c,x
c magnitude order
      character*5 magorder
c magnitude found, new station
      logical flag,new
c number of stations
      integer nstatpath,nstations
c station names
      character*5 pathstations(999),stations(999)
c model indicator in station filename
      character*1 file_ind
c time string for gmt psxy time axis
      character*22 time
      integer year,month,day,hour,min
      real sec
      double precision msec1,msec2
      logical first
      integer i
      integer days
      real mom
      integer phases
      integer seiclen


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      first=.true.
      NEVENT=0
      nstations=0
      nfp = 0

      WRITE(6,*) ' Nordic input file'
      READ(5,'(A80)') FILE

      magorder = ' '
      write(*,*) ' Return to use largest magnitude, or enter magnitude o
     &rder (e.g. CLBSW) '
      read(*,'(a)') magorder

      write(*,*) ' scale: size = mag * scale (for example 0.01)'
      read(*,*) scale

      if (scale.eq.0) then
        write(*,*) 'scale fixed to 0.01'
        scale = 0.01
      endif

      OPEN(1,FILE=FILE,STATUS='OLD')
      OPEN(2,FILE='gmtxy.out',STATUS='unknown')
      OPEN(12,FILE='gmtstxy.out',STATUS='unknown')
      OPEN(13,FILE='gmttime.out',STATUS='unknown')
      OPEN(14,FILE='maxmag.out',STATUS='unknown')
      OPEN(15,FILE='gmtdays.out',STATUS='unknown')
      open(3,file='psmeca.out',status='unknown')
      OPEN(4,FILE='gmtxyz.out',STATUS='unknown')
      OPEN(11,FILE='gmtpath.out',STATUS='unknown')

      call nortype(1,compact)

      if (compact) then
          write(*,*) 'Input file is compact.'
      else
          write(*,*) 'Input file is Nordic.'  
      endif
c      write(*,*)

1     CONTINUE

      if (compact) then
          read(1,'(A80)',END=666) data(1)
          nhead = 1
      else

          call indata
     *      (1,nstat,nphase,nhead,nrecord,evid,exp,data,id)
            if(nrecord.eq.0) goto 666   ! end of file
      endif

      do c=1,5
         mag(c)=-999.
        cmag(c)= ' '
      enddo
c
c count P or S phases
c
      phases=0
      do c=1,nphase
        if (data(nhead+c)(11:11).eq.'P'.or.
     &      data(nhead+c)(11:11).eq.'S') then
         phases=phases+1
        endif
      enddo
c      write(*,*) ' debug ',nphase,phases

c      write(*,*) data(1)
      read(data(1)(2:20),'(i4,1x,2i2,1x,2i2,1x,f4.1)')
     &        year,month,day,hour,min,sec
      if (first) then
        call timsec(year,month,day,hour,min,sec,msec1)
        first=.false.
      endif
      call timsec(year,month,day,hour,min,sec,msec2)
      days=int((msec2-msec1)/(3600.*24.))
c      write(*,*) days,msec2,msec1
      read(data(1)(21:21),'(a1)') file_ind
      read(data(1)(24:30),'(f7.3)') lat
      read(data(1)(31:38),'(f8.3)') lon
      read(data(1)(39:43),'(f5.1)') dep
      if (data(1)(56:59).ne.'    ')
     &  read(data(1)(56:59),'(f4.1)') mag(1)
      cmag(1) = data(1)(60:60)
      datetext= ' '
      datetext(1:4)=data(1)(2:5)
      datetext(5:6)=data(1)(7:8)
      datetext(7:8)=data(1)(9:10)
      do c=1,8
        if (datetext(c:c).eq.' ') datetext(c:c) = '0'
      enddo
      if (data(1)(64:67).ne.'    ')
     &  read(data(1)(64:67),'(f4.1)') mag(2)
      cmag(2) = data(1)(68:68)
      if (data(1)(72:75).ne.'    ')
     &  read(data(1)(72:75),'(f4.1)') mag(3)
      cmag(3) = data(1)(76:76)
c
c  read from 2nd header
c
      if (.not.compact) then
        if (data(1)(1:23).eq.data(2)(1:23)) then
      if (data(1)(56:59).ne.'    ')
     &    read(data(2)(56:59),'(f4.1)') mag(4)
          cmag(4) = data(2)(60:60)
      if (data(1)(64:67).ne.'    ')
     &    read(data(2)(64:67),'(f4.1)') mag(5) 
          cmag(5) = data(2)(68:68)
        endif
      endif

      magout=-999.  ! changed to negative
      flag = .true.
c
c  find largest mag
c
      if (seiclen(magorder).gt.0) then
c.ne.''.and.magorder.ne.' ') then
        flag=.false.

        do c=1,5
          do x=1,5
            if (magorder(c:c).eq.cmag(x).and.
     &           magorder(c:c).ne.' ') then
              magout = mag(x)
              flag = .true.
              goto 10
            endif
          enddo
        enddo

      else
c
c find largest magnitude
c
        magout=-9.
        do c=1,5
c        write(*,*) mag(c), magout
          if (mag(c).gt.magout) magout=mag(c)
        enddo
      endif
c      if (mag(1).lt.0.) then
c        stop
c      endif

  10  continue

      if (data(1)(24:38).eq.'               ') goto 1

      do l=2,nhead
          if(data(l)(80:80).eq.'F') then
             read(data(l),'(3f10.1)')strike,dip,rake
 
c output to file
c             write(*,'(8(1x,f7.2))')lon,lat,strike,dip,
c     &                 rake,magout,lon,lat
             write(3,'(9(1x,f7.2),1x,a8)')lon,lat,dep,strike,dip,
     &                 rake,magout,lon,lat,datetext
             nfp = nfp + 1
          endif
      enddo

      if (flag.and.magout.ne.-999.) then
        write(14,'(f10.5)') magout
c compute moment
        mom=10.**(magout*3./2.+9.1)
        magout = magout *scale
c        write(*,'(F8.3,1X,F7.3,1X,F10.5)') lon,lat,magout
        write(2,'(F8.3,1X,F7.3,1X,F10.5)') lon,lat,magout
        write(15,'(F8.3,1X,F7.3,1X,i8)') lon,lat,days
        time=' '
        write(time(1:22),'(i4.4,"/",i2.2,"/",i2.2,"T",
     &   i2.2,":",i2.2,":",f5.2)')
     &      year,month,day,hour,min,sec
        do i=18,22
          if (time(i:i).eq.' ') time(i:i)='0'
        enddo
        write(13,'(a22,1x,f8.3,1x,f8.3,1x,f10.5,1x,g9.2,1x,i4)') 
     &    time,lon,lat,magout,mom,phases
        write(4,'(F8.3,1X,F7.3,1X,F6.1,F10.5)') lon,lat,-dep,magout
        nevent=nevent+1
      else
        write(*,*) ' selected magnitude type(s) not found: ',
     &            data(1)(1:23)
      endif
c
c get stations
c
      do c=nhead+1,nrecord
        new=.true.
        do l=1,nstations
          if (
c             data(c)(3:6).eq.stations(l).or.
     &        data(c)(2:6).eq.stations(l)) then
            new=.false.
          endif
        enddo
        if (new) then
          nstations=nstations+1
c          if (data(c)(2:2).eq.' ') then
c            stations(nstations)=data(c)(3:6)
c          else
           stations(nstations)=' '
           stations(nstations)=data(c)(2:6)
c          endif
          call stat_loc(stations(nstations),file_ind,stlat,stlon,stel)
          if (stlat.ne.0..and.stlon.ne.0.) then 
c
c bjb 3/2/2006 output station name to output file also.
c
            write(12,'(f8.3,1x,f8.3,1x,a5)') stlon,stlat,
     &	    stations(nstations)
          endif
        endif
      enddo

c
c get path information
c
      nstatpath=0
      do c=nhead+1,nrecord
        new=.true.
        do l=1,nstatpath
          if (data(c)(3:6).eq.pathstations(l).or.
     &        data(c)(2:6).eq.pathstations(l)) then
            new=.false.
          endif 
        enddo
        if (new) then
          nstatpath=nstatpath+1
          if (data(c)(2:2).eq.' ') then
            pathstations(nstatpath)=data(c)(3:6)
          else
            pathstations(nstatpath)=data(c)(2:6)
          endif
        endif
      enddo
      if (nstatpath.ne.0.and.lat.ne.0..and.lon.ne.0.) then
        do c=1,nstatpath
          call stat_loc(pathstations(c),file_ind,stlat,stlon,stel)
          if (stlat.ne.0..and.stlon.ne.0.) then 
            write(11,'(f8.3,1x,f8.3)') lon,lat
            write(11,'(f8.3,1x,f8.3)') stlon,stlat
            write(11,'(a1)') '>'
          endif
        enddo
      endif
      goto 1

666   close(1)
      close(2)
      close(3)
      close(4)
      close(11)
      close(12)
      close(13)
      close(14)
      close(15)

      write(*,'(a,i5)') ' Number of events:                          '
     &         ,nevent
      write(*,'(a,i5)') ' Number of events with fault plane solution: '
     &            ,nfp
      WRITE(*,*) ' epicenter location input file for GMT psxy/psxyz: '
     &               //'gmtxy.out/gmtxyz.out'
      WRITE(*,*)' epicenter locations with day counter from first event'
     &               //' gmtdays.out'
      WRITE(*,*) ' station location input file for GMT psxy: '
     &               //'gmtstxy.out'
      WRITE(*,*) ' time and magnitude to be used with GMT psxy: '
     &               //'gmttime.out'
      WRITE(*,*) ' travel path input file for GMT psxy (option -M): '
     &               //'gmtpath.out'
      write(*,*) ' Coordinates are given as longitude - latitude'
      write(*,*) ' input file for psmeca (Aki and Richards convention):'
     &               // ' psmeca.out'

      STOP
      END

