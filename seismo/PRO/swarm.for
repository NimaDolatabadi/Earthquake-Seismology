c                                                                               
C                       
C  Program to identify seismic swarms
c
c  Input to the program is a CAT file with many events and some manually
c  entrred parameters. Output is identified swarms. The output file swarm.out
c  contains all swarms organized as 'events'. In the header line is given
c  center for area identified and the 'magnutde' is the number of events in
c  area divided by 10. The rest of line is info from first event in swarm.                                   
c
c  Principle of selection:
c
c  Area is divided into a lat-lon grid. Around each gridpoint, there is a 
c  cell with radius small_r. The program first checks how many events there 
c  are in each cell for the whole catalog. It then checks each cell which
c  has more than the minimum number of events to constitute a swarm, if 
c  enough events are within the required time window. For each time window 
c  with enough events, a swarm is dclared so a swarm lasting e.g. twice the 
c  time window will be declared as two swarms.
c  An additional condition is that the number of event is larger than
c  the normalized background activity. The normalized activity is calculated
c  as the activity in the large cell normalized for area to the small cell,
c  and normalized in time to the window for the swarm
C                                                                               
C  J. Havskov  Feb 2000
c
c  updates
c
      implicit none
      include 'seidim.inc'        ! dimensions
      character*80 data(max_data)                                              
      integer nq                  ! max number of events
      integer ng                  ! max number of gridpoints in each direction
      parameter (nq=50000)
      parameter (ng=200)
      character*1 type,exp                                                      
      character*80 text           ! general text
      integer nstat,nphase,nhead,nrecord
      integer n,id,i,k,l,ii
      character*80 infile     ! input file name
      logical compact !compact or not
      character*80 header(nq)
      real lat(nq),lon(nq),sec(nq) ! hypocenter parameters
      integer year(nq),month(nq),day(nq),hour(nq),min(nq) ! hypocenter pars.
      real small_r,large_r    ! radius for count and radius for background
      integer nswarm                   ! number of swarms
      character*80 out_file            ! output file name of individual swarm
      character*80 parm                ! parameters in text string
      real minlat,maxlat,minlon,maxlon ! area to search
      real dellat,dellon               ! step in latitude and longitude
      integer nlat,nlon                ! number of gridpoints in lat and lon
      real grid_point(2,ng,ng)         ! grid points of lat and lon
      integer n_grid_small(ng,ng)      ! number of events in small cells
      integer n_grid_large(ng,ng)      ! --------------------large ---- 
      integer n_grid(1000,ng,ng)       ! event #'s of events in small cells
      real delta                       ! distance, km
      real ratio                       ! area ratio between large and small cell
      double precision t(nq)           ! absolute time of event
      double precision t1,t2,tw        ! time of events and time window
      double precision total_time      ! total time of catalog in secs
      real total_days                  ! ------------------------ days
      integer n1,n2,nw                 ! index of events and # ev. in time win.
      integer min_nw                   ! minimum number of events in swarm
      real background                  ! normalized background activity


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   initialize
c
        nswarm=0
c   
        write(6,*) ' Input file name '
        read(5,'(a)') infile
c
c   open input file 
c
        open(1,file=infile,status='old')
        write(6,*) ' Latitude range and grid size (deg)'
        read(5,*) minlat,maxlat,dellat 
        write(6,*)' Longitude range and grids size (deg)'
        read(5,*) minlon,maxlon,dellon
        write(6,*)
     *  ' Radius of cell and radius of background cell (deg)'
        read(5,*)small_r,large_r
        write(6,*)' Time window in days'
        read(5,*) tw
        tw=tw*24*3600.0    ! convert to seconds
        write(6,*)' Minimum number of events in window'
        read(5,*) min_nw
        write(6,*) 
     *  ' Minimum ratio of number of events to background activity'
        read(5,*) ratio
c
c   put all parameter on parm line
c
        parm=' '
        write(parm,'(1x,9f7.3,i4,f7.3)') 
     *  minlat,maxlat,dellat,minlon,maxlon,dellon,
     *  small_r,large_r,tw/(24*3600.0),min_nw,ratio
        parm(80:80)='3'
c
c   check if a compact file
c
        call nortype(1,compact)
        if(compact) then
           write(6,*)' Input file compact'
        endif
c
c   open output file
c
        open(2,file='swarm.out',status='unknown')
c
c   calculate grid points and initilize counters
c
        nlat=(maxlat-minlat)/dellat+1
        nlon=(maxlon-minlon)/dellon+1
        if(nlat.gt.ng) then
           write(6,*)' Too many latitude grid points, max is ',ng
           stop
        endif
        if(nlon.gt.ng) then
           write(6,*)' Too many longitude grid points, max is ',ng
           stop
        endif
        do i=1, nlat
           do k=1,nlon
              grid_point(1,i,k)=minlat+(i-1)*dellat
              grid_point(2,i,k)=minlon+(k-1)*dellon
              n_grid_small(i,k)=0
              n_grid_large(i,k)=0
           enddo
        enddo 
        write(6,*)' Number of grid points', nlat*nlon
c
c   read to end of file
c
        n=0
        write(6,*)
        write(6,*)' Reading events'
  10    continue
        if(compact) then
           read(1,'(a)',end=99) data(1)
        else
           call indata(1,nstat,nphase,nhead,nrecord,type,exp,data,id)
           if(nrecord.eq.0) goto 99
        endif
c
c   read parameters
c
        n=n+1
        read(data(1)(2:20),'(i4,1x,2i2,1x,2i2,1x,f5.1)') 
     *  year(n),month(n),day(n),hour(n),min(n),sec(n)
        read(data(1)(24:38),'(f7.3,f8.3)') lat(n),lon(n)
        header(n)=data(1)
c
c   calculate time since 1900
c
        call timsec(year(n),month(n),day(n),hour(n),min(n),sec(n),
     *              t(n))
        goto 10
c
 99     continue
        write(6,*)
        write(6,*)' The input file had ',n,' events'
        total_time=t(n)-t(1)
        total_days=total_time/(24*3600.0)
        write(6,*)' Total number of days in catalog', total_days
        write(6,*)
        write(6,*)' Count events in cells'
        write(6,*)
c
c   count number of events in large and small cells and index the events
c
        do l=1,n
        do i=1,nlat
            do k=1,nlon
               call dist_cl(grid_point(1,i,k),grid_point(2,i,k),
     *         lat(l),lon(l),delta)
               if(delta.le.small_r) then
                  n_grid_small(i,k)=n_grid_small(i,k)+1 
                  n_grid(n_grid_small(i,k),i,k)=l    ! store which event in grid
               endif
               if(delta.le.large_r)
     *         n_grid_large(i,k)=n_grid_large(i,k)+1 
            enddo
        enddo
        enddo
c

        write(6,*) ' Make time sequece'
c
c
c   find clusters in time
c
c
        do i=1,nlat
            do k=1,nlon
               if(n_grid_small(i,k).gt.min_nw) then
                  n1=1          ! first event in series
                  n2=2          ! second event
                  nw=1          ! number of events in window
 1000             continue                  
                  if(n2.gt.n_grid_small(i,k)) goto 2000 ! no more data this one
                  t1=t(n_grid(n1,i,k))  ! get times
                  t2=t(n_grid(n2,i,k))
                  if(t2-t1.le.tw) then
                     nw=nw+1            ! one more in window
                     n2=n2+1            ! move pointer to next event
                     goto 1000          ! test again
                  else                  ! next event was not in window
                     background=
     *               n_grid_large(i,k)*                     ! number in large
     *               ((small_r*small_r)/(large_r*large_r))* ! norm. area
     *               (tw/(total_days*24*3600.0))            ! norm time

                     if(nw.gt.min_nw.and.   ! enough to write out swarm
c
c   before declaring a swarm, also check if actvity is larger than 
c   background activity
c
     *                (nw.gt.(ratio*background)))
     *                then
                       nswarm=nswarm+1       ! count number of swarms
                       write(6,*)' Swarm number ',nswarm
                       write(6,*)grid_point(1,i,k),
     *                 grid_point(2,i,k),n_grid_small(i,k),nw         
c                      write(6,*) n_grid_large(i,k), 
c    *                 large_r,small_r,tw,total_days
                       text=header(n_grid(n1,i,k))
                       text(44:79)=' '
                       write(text(24:38),'(f7.3,f8.3)') 
     *                 grid_point(1,i,k),grid_point(2,i,k)
                       ii=nw
                       if(ii.gt.99) then    ! cannot write a number larger 99 
                         ii=99
                       endif
                       write(text(57:59),'(f3.1)') ii/10.0
                       text(60:64)='NSWM'
                       write(2,'(a)') text
                       if(nswarm.eq.1) write(2,'(a)') parm   ! parameters 
                       text=' '
                       text(20:37)='Number of events: '
                       write(text(39:42),'(i4)') nw
                       text(45:56)='Background: '
                       write(text(57:65),'(f9.5)') background
                       text(2:10)='Swarm No '
                       write(text(11:14),'(i4)') nswarm
                       text(80:80)='3'
                       write(2,'(a)') text
c
c   open individual swarm file, make sequential name
c
                       out_file(1:5)='swarm'
                       write(out_file(6:8),'(i3)') nswarm
                       out_file(9:12)='.out'
                       do ii=6,7
                         if(out_file(ii:ii).eq.' ') 
     *                   out_file(ii:ii)='_'
                       enddo
                       open(8,file=out_file,status='unknown')
                       do l=n1,n2-1         ! write all in swarm
                          write(2,'(a)') header(n_grid(l,i,k))
                          write(8,'(a)') header(n_grid(l,i,k))
                       enddo
                       close(8)
                       write(2,*) ' '
                       nw=1             ! reset
                       n1=n2            ! start a new series at end of old
                       n2=n2+1
                     else               ! no cluster
c                      n1=n2            ! start at end of current interval
c                      nw=1
                       n1=n1+1          ! move start one event ahead only
                       nw=nw-1          ! one less event in window
                       if(nw.lt.1) nw=1
c                      n2=n2+1 
                       if(n2.eq.n1) n2=n2+1
                     endif
                     goto 1000
                  endif
 2000             continue
               endif
            enddo
        enddo

        stop
        end



      subroutine dist_cl(alats,alongs,alatr,alongr,delta)
c
c   simplified version of azibazi in LIB
c
c
c   changes
c
c   sep 98 by jh ----------- version 7.0 check ------------------
c                no changes
c    
c   INPUT:  alats, alongs: latitude and longitude of the source
c           alatr, alongr: latitude and longitude of the receiver
c   OUTPUT: delta: the epicentral distance
c           azim : the azimuth at the source
c           bazim: the back-azimuth at the station
c   Note: all the elements are angles given in degrees. 
c         this subroutine does not account for the Earth's ellipticity
c
c   There is no garantee that the subroutine will work in absolutely all
c   cases, but it has been tested and should work across the line of change
c   of day for example.
c   December 1994. Valerie Maupin.
c
      degpi = 180./3.14159
c
      alon = alongr-alongs
      if (alon.lt.-180.) alon=alon+360.
      if (alon.gt.180.) alon=alon-360.
      if (alon.ge.0.)  then
        c = alon/degpi
        aa = (90. - alats)/degpi
        bb = (90. - alatr)/degpi
      else
        c = -alon/degpi
        aa = (90. - alatr)/degpi
        bb = (90. - alats)/degpi
      endif
      cc = cos(aa)*cos(bb) + sin(aa)*sin(bb)*cos(c)
      cc = acos(cc)
      a = (cos(aa) - cos(bb)*cos(cc))/(sin(bb)*sin(cc))
      a = acos(a)
      b = (cos(bb) - cos(aa)*cos(cc))/(sin(aa)*sin(cc))
      b = acos(b)
c
c ... calculate the epicentral distance 
c                 you must choose between degrees or kilometers 
c                 and comment out one of these two lines 
      delta = cc*degpi
c     delta = cc*6371. 
c
c ... calculate the azimut and backazimuth
c
c     if (alon.ge.0.)   then
c       azim = b*degpi
c       bazim = 360. - a*degpi
c     else
c       azim = 360. - a*degpi
c       bazim = b*degpi
c     endif
c
c.......calculate the coordinates of the mid-point
c                          (if this element is of interest to you,
c                          add (alat,along) in the argument list)
c
c     cc=cc/2.
c     dd = cos(aa)*cos(cc) + sin(aa)*sin(cc)*cos(b)
c     dd = acos(dd)
c     alat= 90. - dd*degpi
c     c3 = (sin(bb)*cos(cc)- sin(cc)*cos(bb)*cos(a))/sin(dd)
c     c3 = acos(c3)
c     if (alon.ge.0.)  along = alongr - c3*degpi
c     if (alon.lt.0.)  along = alongs - c3*degpi
c     if (along.lt.-180.) along=along+360.
c     if (along.gt.180.) along=along-360.
c
      return 
      end

