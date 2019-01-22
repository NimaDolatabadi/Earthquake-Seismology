c
c    selecting subsection of mapfiles within a given grid, to be
c    used with the very detail large files of world coordinates
c
c    jh april 99
c    changes
c
c    oct 6 1999 by jh : fix selection bug
c    dec 21 2016   jh : add skip option

      implicit none
      real lat(500000),lon(500000)
      integer n    ! number fo points in each segment
      real minlat,maxlat,minlon,maxlon  ! grid to search in
      logical select
      character*80 infile
      integer nseg,nmap,skip
      integer i,k


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      nseg=0
      nmap=0
c
      write(6,*) 'Input file name'
      read(5,'(a)') infile
      open(1,file=infile,status='old')
      open(2,file='selmap.out',status='unknown')
      write(6,*)'Latitude range, min to max'
      read(5,*)minlat,maxlat
      write(6,*)'Longitude range, min to max'
      read(5,*)minlon,maxlon
      write(6,*)'Skip points, 1 mean use all'
      read(5,*) skip
c
 1    continue
c
c   read one segment
c
      select=.false.
      read(1,'(i4)',end=99) n
      nmap=nmap+1
      read(1,'(10f8.3)')(lat(i),lon(i),i=1,n)
c
c  find if any point is inside
c
      do i=1,n
        if(lat(i).le.maxlat.and.lat(i).ge.minlat.and.
     *     lon(i).le.maxlon.and.lon(i).ge.minlon) select=.true.
      enddo

       
      if(skip.gt.1) then
         k=0
         do i=1,n,skip
            k=k+1
            lat(k)=lat(i)
            lon(k)=lon(i)
         enddo
c
c  put in last original value in last point to close  closed figures 
c

         k=k+1
         lat(k)=lat(n)
         lon(k)=lon(n)
         n=k
c
c  if too few, skip
c
         if(n.lt.3) select=.false.
       endif


      if(select) then
         nseg=nseg+1
         write(6,*)' Area selected',n
         write(2,'(i4)') n
         write(2,'(10f8.3)')(lat(i),lon(i),i=1,n) 
      endif
c
      goto 1
c
 99   continue
c
      write(6,*)'Number of segments in input file',nmap
      write(6,*)'Number of segments selected     ',nseg
      write(6,*)'Output file name is selmap.out'
      stop
      end
