c--------------------------------------------------------------------------
c  running epimap without questions
c  file name can be first argument or if no argument is asked for
c  both normal and compact files can be used
c  the border of the map is determined from the content of the input file
c  lat-lon divisions are 1/5 of the range, however not less than 1 deg
c  the parameters which can be set in SEISAN.DEF are:
c
c  projection number, default 3 for Mercator
c  map source, default WORLD.MAP
c  plot stations, defualt no
c  these parameters are also used for map option in EEV
c
c
c  j. havskov december 2017
c--------------------------------------------------------------------------
c
c
c
c  changes
c
c  may 23 2018 jh: restrict limits to 90 and 180

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! seisan general
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      real lat(100000),lon(100000)        ! lat and lon
      real lamin,lamax,lomin,lomax        ! lat-lon grid with the data
      integer ladel,lodel                 ! grid size for lat lon
      integer code                        ! error code
      character*60 top_directory          ! seisan top directory
      character*1 dchar                   ! dir separation char

      integer nars                        ! number of arguments
      character*80 infile(5)              ! arguments

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer nevent                      ! number of events in file
      character*120 text                  ! general text
      logical compact                     ! true if compact file
      integer i,k                         ! counter

c
c print version
c
      include 'version.inc'
      out_version_date=' '
      if (version_new) out_version_date=version_date
      call print_ver

      call get_seisan_def
      call topdir(top_directory)
      call dir_char(dchar)
c
c   get arguments
c
      call get_arguments(nars,infile)
c
c   get input file name if no argument
c
      if(nars.eq.0) then   
          write(*,*) ' Give input file'  
          read(*,'(a)') infile(1)
      endif 
                    
      open(1,file=infile(1),status='old',err=5)
      goto 6
 5    continue
      write(6,*)'Input file not found'
      stop
 6    continue

c
c   check if a compact file
c
      call nortype(1,compact)
      if(compact) then
         write(6,*)'Input file  compact'
      endif

      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter of events
      k=0                         ! ---------------------------- with location
      rewind (1)
c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue

c
c   read all parameters for one event from file unit 1
c
      if(compact) then
         read(1,'(a)',end=1000) text
         if(text(24:38).ne.' ') then   ! check if located
            k=k+1
            read(text,'(23x,f7.3,f8.3)') lat(k),lon(k)
          endif
      else
         call rea_event_in(1,all,data,code)

c
c   check if end of file (code=1), if so jump out of loop
c
         if(code.eq.1) goto 1000
c
c   save lat lon if there
c
         if (hyp_lat(1).ne.-999.0) then
            k=k+1
            lat(k)=hyp_lat(1)
            lon(k)=hyp_lon(1)
         endif
      endif
c
      nevent=nevent+1               ! count events
c
c   write the whole first header line
c
c      write(6,'(a)') data(1)(1:79)

c
c   get next event
c
      goto 50
c
c     end of file 
c
      close(1)

 1000 continue
c
c     find area of data
c
      lamax=-100.0
      lamin=100.0
      lomax=-400.0
      lomin=400.0

      do i=1,k
        if(lat(i).gt.lamax) lamax=lat(i)
        if(lat(i).lt.lamin) lamin=lat(i)                
        if(lon(i).gt.lomax) lomax=lon(i)
        if(lon(i).lt.lomin) lomin=lon(i)
      enddo
                 
      write(6,*)            ! blank line
      close(2)              ! close output file

      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Number of events with location',k
      write(6,*) 'Lattitude range ',lamin,lamax
      write(6,*) 'Longitude range ', lomin,lomax
c
c   find outline to use, increase a bit
c
      lamax=lamax+0.5
      lamin=lamin-0.5
      lomax=lomax+0.5
      lomin=lomin-0.5
c
c  check limits
c
      if(lamax.gt.90.0) lamax=89.0
      if(lamin.lt.-90.0) lamin=-89.0
      if(lomax.gt.180.0) lomax=180.0
      if(lomin.lt.-180.0) lomin=-180.0
c
c   grid, assume 5 divisions, only integer, smallest is 1 deg
c
      ladel=(lamax-lamin)/5.0
      if(ladel.lt.1) ladel=1     
      lodel=(lomax-lomin)/5.0
      if(lodel.lt.1) lodel=1
      write(6,*)'lat-lon grid size ', ladel,lodel
c
c   make epimap input file
c
      if(k.gt.0) then
         open(2,file='map.inp',status='unknown')
         if(map_proj.eq.0) map_proj=3  ! user mercator by default
         write(2,'(i2)') map_proj
         write(2,'(f6.1,1x,f6.1)') lamin,lamax
         write(2,'(f7.1,1x,f7.1)') lomin,lomax
         write(2,*)
         write(2,'(2i3)') ladel,ladel
         write(2,'(2i3)') lodel,lodel
         write(2,*)
         write(6,*) 'map file ',map_file
         if(map_file.eq.' ') map_file='WORLD'
         text=top_directory(:seiclen(top_directory))
     &           // dchar // "DAT" // dchar //
     &           map_file(1:seiclen(map_file)) // ".MAP"
         write(2,'(a)') text
         write(2,'(a31)') '$$$$$$end of contour list$$$$$$'
         write(2,*)'File plotted: '//infile(1)(1:seiclen(infile(1)))
         write(2,*)
         write(2,*)
         write(2,'(a1)') map_stations
         write(2,'(a)') infile(1)
         write(2,'(a38)') '$$$$$$end of epicentre file list$$$$$$'
         write(2,*)
         write(2,*)
         write(*,*) 'Writing map.inp for running epimap'
         close(2)  
c
c   plot
c
         call systemc("epimap map.inp",14)
         write(6,*)
         write (6,*)'You can edit parameters in map.inp and rerun plot'
     *   ,' with command epimap map.inp'
 
      endif

      stop
      end
