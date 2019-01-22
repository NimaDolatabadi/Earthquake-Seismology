
      program statstat
c
c program to do station statistics:
c
c     - number of detections in catalog file for each station
c
c Lars Ottemoller, January 2003
c
c changes:
c
c  11 April 2014 lo - major changes to also work out station uptime
c                     and produce yearly output

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock
      integer seiclen

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer i,j                         ! counter

      integer maxstation                  ! maximum number in arrays
      parameter (maxstation=1000)
      character*5 stations(maxstation)    ! names of stations
      logical stationflag(maxstation)     ! true if station already found
      integer stationcount(maxstation)
      integer nstations                   ! number of stations
      integer ind                         ! index
      real slat,slon,height               ! station coordinates
      integer maxdetect                   ! maximum number of events detected
      integer year,month,day,hour,min,doy
      real    sec
      double precision msec,lastmsec(maxstation),first(maxstation),
     &       last(maxstation),missed(maxstation),total(maxstation)
      real days
      real downdays
      logical endoffile
      logical newyear
      integer prevyear

c
c print version
c
      include 'version.inc'
      out_version_date='September 4, 2003'
      if (version_new) out_version_date=version_date
      call print_ver
                                          ! one 1 station
c
c   open output file
c

       open(2,file='statstat.xy',status='unknown')
       open(3,file='statstat.out',status='unknown')
       open(4,file='statstat.gis',status='unknown')
    
c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue

      write(*,*) ' days without phase before station declared down '
      read(5,*) downdays

      all=.true.                  ! read all parameters
      endoffile=.false.
      prevyear=0
      year=0
      newyear=.false.
30    continue
      nevent=0                    ! initialize counter
      nstations=0
      do i=1,maxstation
        stationcount(i)=0
      enddo

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event form file unit 1
c
      if (.not.newyear) then ! don't read if event already read but in new year
        call rea_event_in(1,all,data,code)
      endif
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) then
        endoffile=.true.
        goto 1000
      endif
      nevent=nevent+1               ! count events
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)

      do i=1,maxstation 
        stationflag(i)=.true.
      enddo 
      do i=1,rea_nphase
c
c add station to array if not included yet
c
        ind=0
        do j=1,nstations
          if (rea_stat(i).eq.stations(j)) ind=j
        enddo
        if (ind.eq.0) then
          nstations=nstations+1
          ind=nstations
          lastmsec(ind)=0.
          first(ind)=0.
          missed(ind)=0.
          if (nstations.eq.maxstation) then
             write(*,*) ' too many stations '
             stop
          endif
          stations(nstations)=rea_stat(i)
        endif
c
c count any phases
c
          if (stationflag(ind)) then
            stationcount(ind)=stationcount(ind)+1
            stationflag(ind)=.false.
c work out time and remember
            year=hyp_year(1)
            if (prevyear.eq.0) prevyear=year
            if (year.gt.prevyear) then
               newyear=.true.
               goto 1000
            else
              newyear=.false.
            endif
            month=hyp_month(1)
            day=hyp_day(1)
            hour=hyp_hour(1)
            min=hyp_min(1)
            sec=hyp_sec(1)
            call timsec(year,month,day,hour,min,sec,msec)
            if (first(ind).eq.0.) then
c set to start and end of year
c              first(ind)=msec
c              last(ind)=msec+1.
               call timsec(year,1,1,0,0,0.,first(ind))
               call timsec(year+1,1,1,0,0,0.,last(ind))
            endif
            if (lastmsec(ind).eq.0.) lastmsec(ind)=first(ind)
c
c check if large gap
c
            days=(msec-lastmsec(ind))/3600./24.
            if (rea_stat(i).eq.'FOO') write(12,*) ' days ',days 
            if (days.gt.downdays) then
              missed(ind)=missed(ind)+msec-lastmsec(ind) ! in seconds
              write(11,*) ' large gap ',rea_stat(i),' ',days
            endif
            lastmsec(ind)=msec
          endif
      enddo
c
c     end of file
c
 1000 continue

      if (newyear.or.endoffile) then
        maxdetect=1
        do i=1,nstations
          if (stationcount(i).gt.maxdetect) maxdetect=stationcount(i)
        enddo
c
c find total time where station was operational
c
        do i=1,nstations
          total(i)=last(i)-first(i)-missed(i)
        enddo
c
c write output
c
      write(3,'(a,i7)') ' Number of events in input file', nevent
      write(3,'(a)') ' List of stations with less than 10% '
     &   // ' detections of the maximum number detected on one '
     &   // ' station: '
      write(4,'(a)') 'station,longitude,latitude,detections'
        do i=1,nstations
          slat=0.
          slon=0.
          if (seiclen(stations(i)).gt.0) then
            call stat_loc(stations(i),' ',slat,slon,height)
          endif
          if (slat.ne.0..and.slon.ne.0.) then
            write(2,'(f8.3,1x,f8.3,1x,i8,1x,f7.2,1x,f7.2,1x,
     &           f7.1,1x,a1,1x,a5,1x,i4)') 
     &      slon,slat,stationcount(i),
     &      total(i)/3600./24.,total(i)/3600./24./stationcount(i),
     &      total(i)/(last(i)-first(i))*100.,
     &      '#',stations(i),prevyear
          write(4,'(a5,2x,a1,f9.3,a1,f8.3,a1,i10)') 
     &      stations(i),',',slon,',',slat,',',stationcount(i)

            if (stationcount(i).le.maxdetect*.1) then
              write(3,'(a5,1x,i5)') stations(i),stationcount(i)
            endif
          endif
        enddo
        prevyear=year
      endif
c
c   get next event
c
      if (newyear) goto 30
      if (.not.endoffile) goto 50

      close(2)              ! close output file
      close(3)              ! close output file
      close(4)              ! close output file

      write(6,*)            ! blank line
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is statstat.out'
      write(6,*) 'Input file for psxy is statstat.xy'
      write(6,*) 'Input file for arcview is statstat.gis'

      stop
      end
