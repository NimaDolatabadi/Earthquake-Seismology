c
c    program to convert station coordinates from usgs to seisan format
c    jh august 2006
c
c  upgrade
c
c    nove 16, 06  jh : e and w was reversed
c    oct 10 12    jh : new isc station format

      implicit none
      character*180 text,text2
      real lat,lon,depth         ! latitude longitude depth
      integer ilat,ilon
      character*1 status         ! station status
      character*1 answer
      real dlat,dlon             ! minutes of latitude and longitude
      character*5 stat           ! station code
      character*2 ns,ew          ! N, S E, or W
      character*80 infile        ! input file name 
      integer nstat              ! station counter
      character*4 type           ! input file type
      integer i,j,k,l
c
c
c
      write(6,*)
      write(6,*) 
     *'This program converts the station file from USGS to SEISAN'
      write(6,*)'http://neic.usgs.gov/neis/gis/station_comma_list.asc'
      write(6,*)
     *'The file must have the comma format, format now seems unused'
      write(6,*) 'Program also converts from the similat station book',
     *           ' format used by ISC found at'
      write(6,*) 'http://www.isc.ac.uk/cgi-bin/stations?lista'
      write(6,*)
      write(6,*) 'Input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old')
      open(2,file='stasei.out',status='unknown')
      nstat=0
c
c    determine type
c
 1    continue
      read(1,'(a)',end=100) text
      if(text(1:4).eq.'CODE') then
         type='usgs'
         goto 2
      endif 
      if(text(1:4).eq.'Code') then
         type='isc '
         write(6,*)'Include closed stations (y/n)'
         read(5,'(a)') answer
         goto 2
      endif
      goto 1
c    
 2    continue

      write(6,*)' File type ', type

      if(type.eq.'usgs') then
 10      continue
         read(1,'(a)',end=100) text
c
c   reformat string to columns without comma
c
         k=1
         j=1     
         do i=1,60
           if(text(i:i).ne.',') then
              text2(k:k)=text(i:i)
              k=k+1
              j=j+1
           else
              if(j.ne.10) then
                 do l=j,10
                    text2(k:k)=' '
                    k=k+1
                 enddo
              endif
              j=1       ! start of a new field
           endif
         enddo
c         write(17,*) text2
c
c   read data
c
         read(text2,'(a5,4x,2f10.4,f8.2)') stat,lat,lon,depth
c         write(6,*) stat
         if(depth.le.-999.0) depth=-999.0
         if(lat.eq.0.0.and.lon.eq.0.0) goto 10
         ns='N'
         if(lat.lt.0.0) ns='S'
         ilat=abs(lat)
         dlat=(abs(lat)-ilat)*60.0
         ew='E'
         if(lon.lt.0.0) ew='W'
         ilon=abs(lon)
         dlon=(abs(lon)-ilon)*60.0
         i=depth
         if(stat(5:5).ne.' ') then
            write(2,'(1x,a5,i2,f5.2,a1,i3,f5.2,a1,i4)')
     *      stat,ilat,dlat,ns,ilon,dlon,ew,i
         else
            write(2,'(2x,a4,i2,f5.2,a1,i3,f5.2,a1,i4)')
     *      stat(1:4),ilat,dlat,ns,ilon,dlon,ew,i
         endif
         nstat=nstat+1
         goto 10
      endif
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if(type.eq.'isc ') then
 20      continue
         read(1,'(a)',end=100) text
         if(text(1:13).eq.'International') goto 100
         read(1,'(a)',end=100) text
         if(text(1:13).eq.'International') goto 100

c
c   read data
c
         read(1,'(a)',end=100) text
         if(text(1:13).eq.'International') goto 100
c
c   reformat so . is in right column, problem with a few lines
c

         if(text(62:62).ne.'.') then
             text(6:58)=' '            ! there could be a . in text
             i=index(text,'.')
             text2=text
             text=' '
             text(1:5)=text2(1:5)
             text(59:100)=text2(i-3: i+38)
c             write(6,*) text(1:70)
         endif
         read(text,'(a5,53x,3f10.5,16x,a1)') stat,lat,lon,depth,status
         write(6,*) stat,lat,lon,depth,status
         if(depth.le.-999.0) depth=-999.0
         if(lat.eq.0.0.and.lon.eq.0.0) goto 20
c
c  check for closed station
c
         if(status.eq.'C'.and.answer.eq.'n') goto 20 
         ns='N'
         if(lat.lt.0.0) ns='S'
         ilat=abs(lat)
         dlat=(abs(lat)-ilat)*60.0
         ew='E'
         if(lon.lt.0.0) ew='W'
         ilon=abs(lon)
         dlon=(abs(lon)-ilon)*60.0
         i=depth
         if(stat(5:5).ne.' ') then
            write(2,'(1x,a5,i2,f5.2,a1,i3,f5.2,a1,i4)')
     *      stat,ilat,dlat,ns,ilon,dlon,ew,i
         else
            write(2,'(2x,a4,i2,f5.2,a1,i3,f5.2,a1,i4)')
     *      stat(1:4),ilat,dlat,ns,ilon,dlon,ew,i
         endif
         nstat=nstat+1
         goto 20
      endif
 100  continue
      write(6,*)' Number of stations', nstat
      write(6,*)' Output file name is stasei.out'
      stop
      end
