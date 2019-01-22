c$debug
c
c   select isc stations from isc station file corresponding
c   to isc events in user specified input file
c    
c   station list can be both isc or seisan format
c
c   j. havskov nov 93
c
c   changes:
c   jan 96 by jh: fixed to always convert to seisian format
c   apr 18      : bug in output format
c   jun 1999 jh   ------------   version 7.0 -----------------
C   jan 6 99    : change so that station is not selected more than once,
c                 idea thanks to Mohammad Raesi
c
      implicit none
      character*4 outsta(30000)    ! stations selected
      integer selected(30000)      ! indicator for stations found
      character*80 iscfile,norfile
      character*80 text
      integer nout,i,k,nwrite
      real latsec,lonsec          ! seconds part of latitude and longitude
      integer latmin,lonmin       ! lat and lon minutes
      integer nstat,nphase,nhead,nrecord,id
      character*1 type,exp
      character*80 data(5000)
c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      write(6,*)' This program will read an S-file, find how many'
      write(6,*)' different stations there are and select those'
      write(6,*)' stations out of a station file, which can either'
      write(6,*)' be in SEISAN (=HYPO71) format or ISC format. The'
      write(6,*)' output is in SEISAN format. If no S-file is given'
      write(6,*)' the input station file is assumed to be in ISC'
      write(6,*)' format and the whole file will be converted to'
      write(6,*)' SEISAN format'
      write(6,*)' ONLY 4 letter station codes supported '
      write(6,*)
c

      write(6,*)' Give file with all stations( ISC or SEISAN format)'
      read(5,'(a)') iscfile
      write(6,*)
     *' Give file with readings, Nordic format, if none, return'
      read(5,'(a)') norfile
      open(2,file=iscfile,status='old')
      open(3,file='iscsta.out',status='unknown')
c
c   go through events if available
c
      nout=0
      nwrite=0
      do i=1,6000
         selected(i)=0
      enddo
      if(norfile(1:3).ne.'   ') then 
         open(1,file=norfile,status='old')
         open(4,file='isc_miss.out',status='unknown')

 1       continue
         call indata(1,nstat,nphase,nhead,nrecord,type,exp,data,id)              
         if(nrecord.eq.0) goto 10
c
         do i=nhead+1,nrecord-1
            do k=1,nout
               if(data(i)(2:5).eq.outsta(k)) goto 2
            enddo
            nout=nout+1
            outsta(nout)=data(i)(2:5)
c
c   enter here if station found
c
 2          continue
         enddo
         goto 1
      endif
c
c   end of event list
c
 10   continue
c
c   find station in isc list, check both isc and seisan formats
c   convert to seisan format, if no input readings file, just
c   convert 
c
      read(2,'(a80)',end=99) text
      if(norfile(1:3).ne.'   ') then    
        do k=1,nout     
          if(selected(k).ne.1) then             ! only select new stations
           if(text(1:4).eq.outsta(k)) then      ! isc format
              selected(k)=1                     ! indicate that station found
              read(text(47:48),'(i2)') latmin
              read(text(56:57),'(i2)') lonmin
              read(text(49:51),'(f3.1)')latsec
              read(text(58:60),'(f3.1)')lonsec
              write(3,'(2x,a,a,f5.2,a,a,f5.2,a)')
     *        text(1:4),text(45:46),latmin+latsec/60.0,
     *        text(52:52),text(53:55),lonmin+lonsec/60.0,text(61:65)
              nwrite=nwrite+1
           endif
c
c   seisan format input
c
           if(text(3:6).eq.outsta(k)) then
              selected(k)=1                     ! indicate that station found
              write(3,'(a)') text
              nwrite=nwrite+1
           endif
         endif
        enddo
      else
        read(text(47:48),'(i2)') latmin
        read(text(56:57),'(i2)') lonmin
        read(text(49:51),'(f3.1)')latsec
        read(text(58:60),'(f3.1)')lonsec
        write(3,'(2x,a,a,f5.2,a,a,f5.2,a)')
     *  text(1:4),text(45:46),latmin+latsec/60.0,
     *  text(52:52),text(53:55),lonmin+lonsec/60.0,text(61:65)
        nwrite=nwrite+1
      endif
      goto 10
c
 99   continue

      if(norfile(1:3).ne.'   ') then
         write(6,*)' Number of stations found in readings file',nout
         k=0
         do i=1,nout
            if(selected(i).ne.1) then
               write(4,'(2x,a)') outsta(i)
               k=k+1
            endif
         enddo
         write(6,*)' Number of stations not found in station file',k
      endif
      write(6,*)' Number of stations selected in station file',nwrite
      write(6,*)' Output file is iscsta.out'
      if(norfile(1:3).ne.'   ') 
     *write(6,*)' Output file with missing stations is isc_miss.out'
      stop
      end
