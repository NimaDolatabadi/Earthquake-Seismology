c--------------------------------------------------------------------------
c make a simple gmt map, jh january 2018
c help from mathilde b. sorensen
c--------------------------------------------------------------------------c
c The program can make a standard GMT plot of epicenters, stations and 
c fault plane solutions. The input can be standard input from a file, data
c base or index file. Both standard s-files and compact files can be input.
c If the program is started without argument, it will ask for input data
c and wheter stations and fault plane solutions should be plotted. The program
c uses the standar SEIGMT program to prepare the data for GMT and the 
c stations plotted is therefore the stations found in the input data and not
c all possible stations in the area. The program can also be started with input
c on the prompt line, but then only with input from a file like
c
c      mapg select.out
c
c With input from the prompt line there is also more options like
c
c      mapg select.out f s
c
c which will plot fault plane solutions and stations. All options are:
c
c   s: plot stations as triangles
c   S: -------------------------  with station code above
c   f: plot fault plane solutions, size proportional with magnitude
c   F  --------------------------, size constant
c   m: plot epicenter symbol with constant size, default is proportional to
c      to magnitude
c
c The output is a PostScript file mapg.ps and a script file mapg.gmt. The script
c file can be used to do modifcations to the plot, not possible with the program.
c Some changes are obvious but others require knowledge of GMT. On Linux, the file
c can be executed by command 'source mapg.gmt'. On windows, rename file to e.g. m.bat
c and exevuter by typing a. Do not use name mapg.bat since the program would no longer
c run by using the mapg command, only the script if startet from directory with
c the script.
c
c Some parameters for the program is set in SEISAN.def:
c The GMT mapfile used is parameter GMT_GRIDFILE
c The command used for displaying the ps file is parameter PLOT_PS_COMMAND 
c
c the map projection is hardwired to Mercator
c 
c The program uses latest version of GMT
c

c
c  changes
c
c   mar 19 2018 jh: use seisan.def extra limits if only one event
c 
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! seisan includes
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(max_data)         ! s-file with data in text array
c
c   for gmt
c
      real lat1,lat2,lon1,lon2,dellat,dellon  ! map outline and grid size
      real lat(1000000),lon(1000000)        ! lat and lon
      character*36 outline                ! outline of map in gmt format
      character*18 midpoint               ! midpoint of map
      character*200 text                  ! help string
      character*80 title                  ! title of plot
      character*14 gridx,gridy            ! grid specification
      real lamin,lamax,lomin,lomax        ! lat-lon grid with the data
      real ladel,lodel                    ! grid size for lat lon
      integer hsize                       ! horizontal size, cm
      character*2 thsize                  ! ----------in text
      real factor                         ! ratio between vertical and horizontal size

      logical stations                    ! if true, plot stations
      logical faultplane                  ! if true, plot fault plane solutions
      logical scale_magnitude             ! if true, scale magnitude
      logical scale_fps                   ! if true, scal fps with mag.
      logical plot_station_code           ! if true plot station code

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      character*10 keys                   !next choice key for routine findevin
      character*14 starttime,endtime      ! time interval to read
      character*80 basename               ! data base or file name
      character*80 infile                 ! input file or base name
      character*80 eventfile              ! single event file name
      integer status,eventno,newmonth,fstart ! for routine findevin
      integer base                        ! type of input 0: seisan data base,                                           !     1: index file, 2: single file
      integer nars                        ! number of arguments
      character*80 args(5)                ! arguments
      logical compact                     ! if true a compact file
      character*6 com                     ! comment type
      logical pc,linux,sun                ! computer type 
      real x                              ! help variable
      integer i,k,j                       ! counter
 
      k=0   

c
c   seisan defaults, that includes fixed map extension limits if 
c   one event
c
      call get_seisan_def
c
c   get computertype
c    
      call computer_type(sun,pc,linux)
      com='REM   '
      if(.not.pc) com='#     '

      stations=.false.
      faultplane=.false.
      scale_magnitude=.true.
      plot_station_code=.false.
      scale_fps=.true.
      
      call get_arguments(nars,args)
c
c   if more arguments, it is station and/or fault plane plotting etc
c
      if(nars.gt.1) then
        do i=2,nars
          if(args(i).eq.'s') stations=.true.
          if(args(i).eq.'S') then
             stations=.true.
             plot_station_code=.true.
          endif 
          if(args(i).eq.'f') faultplane=.true.
          if(args(i).eq.'F') then
             faultplane=.true.
             scale_fps=.false.
          endif
          if(args(i).eq.'m') scale_magnitude=.false.
        enddo
      endif     
c
c   if at least one argument, the first is a file name
c
      if(nars.gt.0) then
         infile=args(1)
         base=2
         goto 2
      endif
c
c   get input file name, or data base with time interval
c

      write(*,*) ' Event data input - select one:'
      write(*,*)
      WRITE(*,*) 
     *'    SEISAN default data base or                     :',
     *'Enter '
      write(*,*) 
     *'    Alternative data base, give 1-5 letter code or  :'  
      WRITE(*,*) 
     *'    Local index file, name must start with index or :'
      WRITE(*,*) 
     *'    Local data base, write ,, or                    :'
      WRITE(*,*) 
     *'    Filename for one file, min. 6 chars or with a . : '
      
      write(*,*)
      read(*,'(a)') infile
      basename=infile(1:80)
                       
      write(*,*)
C
c   check if this is a single multiple event file (base=2),
c   general data base (base=0) or  a local data base or 
c   index file (base=1)
c

      starttime=' '
      endtime=' '

      keys(1:4)='NEXT'    ! always use next event

c
c   initially assume a file
c
      base=2
c
c   a SEISAN 5 letter data base, blank is default data base, ',,'
c   is a local data base. the name cannot have a '.' and must
c   be less than 6 chars long
c
      if(seiclen(basename).lt.6.and.index(basename,'.').eq.0) then 
         base=0
      endif
c
c   case of index file or local data base
c
 
      if(basename(1:5).eq.'INDEX'.or.basename(1:5).eq.'index'.
     *     or.basename(1:2).eq.',,') then
        base=1
      endif
                              
c
c  get time interval for a seisan data base, no time interval used for
c  index file or local data base
c
      if(base.eq.0) then
         write(*,'('' Start Time           (YYYYMMDDHHMMSS): '',$)')
         read(*,'(a14)') starttime
         write(*,'('' End Time, enter is to end of month:    '',$)')
         read(*,'(a14)') endtime
         write(*,*)
      endif
c
c   plot stations and/or fault plane solutions
c
 1    continue
      write(6,*)
     *' Plot stations(s), stations with code(S),'//
     *' fixed size for magnitude symbols(m) and'
      write(6,*)
     *' fault plane solutions(f)'//
     *' e.g. sf, enter for none'
      read(5,'(a)') text
      if(text.eq.' ') goto 2
      do i=1,seiclen(text)
          if(text(i:i).eq.'s') stations=.true.
          if(text(i:i).eq.'S') then
             stations=.true.
             plot_station_code=.true.
          endif 
          if(text(i:i).eq.'f') faultplane=.true.
          if(text(i:i).eq.'m') scale_magnitude=.false.
      enddo
      
c
c   open access for the the relevant input option
c

 2    continue
c
c  a file
c
      if(base.eq.2) then
         open(1,file=infile,status='old',err=5)
         goto 6
 5       continue
         write(6,*)' Input file not found'
         stop
 6       continue
c
c   check if a compact file
c
         call nortype(1,compact)
           if(compact) then
           write(6,*)'Input file is compact'
         endif
      endif
c
c   open output s-file to be used for input
c
      open(2,file='mapg.out',status='unknown')
c
c    open script file
c
      open(4,file='mapg.gmt',status='unknown')    


      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue

c
c   read from relevant input type
c
      if(base.lt.2) then     ! data base  or index file event
         call findevin
     *   (basename,starttime,endtime,keys,0,eventno,
     *   eventfile,fstart,newmonth,status)
c
c   check if end of time interval or errors, then
c   stop
C     
         if(status.gt.0) then
            write(6,*)' STOP WITH STATUS=',status
            goto 1000  ! stop
         endif
C
C   open data base input single event file
c

         open(1,file=eventfile,status='old',err=7)
         goto 8
 7       continue
         write(6,*)' Input file not found: ',eventfile
         stop
 8       continue
      endif

c
c   read all parameters for one event from file unit 1
c
      if(compact) then
         read(1,'(a)',end=1000) data(1)
         if(text(24:38).ne.' ') then   ! check if located
            k=k+1
            read(data(1),'(23x,f7.3,f8.3)') lat(k),lon(k)
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
         if(hyp_lat(1).ne.-999.0) then
           k=k+1
           lat(k)=hyp_lat(1)
           lon(k)=hyp_lon(1)
         endif
      endif

c
c   close file if from data base since only one event
c
      if(base.lt.2) close(1)

      nevent=nevent+1               ! count events
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)

c
c   write out
c
      if(compact) then
         write(2,'(a80)') data(1)
      else
         call rea_event_out(2,all,data,code)
      endif     
c
c   get next event
c
      goto 50
c
c     end of file or data base
c

 1000 continue

c
c-------------------------------------------------------------
c  Plotting section
c-------------------------------------------------------------
c

      close(1)
      close(2)

      write(6,'(a)')'Grid file to use: '
     * //gmt_gridfile(1:sei clen(gmt_gridfile))
c
c   generate input for gmt from s-file with seigmt
c
      open(3,file='seigmt.run',status='unknown')
      write(3,'(a)')'mapg.out'
      write(3,*)                ! use larges magnitude
      write(3,'(a)') '0.05'     ! symbole size for magnitudes
      close(3)
      call systemc('seigmt < seigmt.run',19)
      
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

      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Number of events with location',k
c
c   if only one event, used hardwired extra limits from seisan.def
c

      if(k.eq.1) then
         lamax=lamax+map_lat
         lamin=lamin-map_lat
         lomax=lomax+map_lon
         lomin=lomin-map_lon
      endif
c
c   find outline to use, increase a bit
c
      x=(lamax-lamin)/10.0
      if(x.gt.0.5) x=0.5
      if(x.lt.0.1) x=0.1
      lamax=lamax+x+0.05   ! to round upwards
      i=lamax*10
      lamax=i/10.0
      lamin=lamin-x
      i=lamin*10
      lamin=i/10.0

      x=(lomax-lomin)/10.0
      if(x.gt.0.5) x=0.5
      if(x.lt.0.1) x=0.1

      lomax=lomax+x+0.05
      i=lomax*10
      lomax=i/10.0
      lomin=lomin-x
      i=lomin*10
      lomin=i/10.0

      write(6,'(a,2f7.1)') 'Lattitude range to use ', lamin,lamax
      write(6,'(a,2f7.1)') 'Longitude range to use ', lomin,lomax

c
c   grid, assume 5 divisions,  smallest is 0.25
c
      ladel=(lamax-lamin)/5.0
      if(ladel.le.0.25) ladel=0.25
      if(ladel.gt.0.25.and.ladel.le.0.5) ladel=0.5
      if(ladel.gt.0.5.and.ladel.le.1.0) ladel=1.0
      if(ladel.gt.1.0.and.ladel.le.2.0) ladel=2.0
      if(ladel.gt.2.0.and.ladel.le.5.0) ladel=5.0
      if(ladel.gt.5.0.and.ladel.le.10.0) ladel=10.0
      if(ladel.gt.10.0.and.ladel.le.20.0) ladel=20.0
      if(ladel.gt.20.0.and.ladel.le.30.0) ladel=30.0
      if(ladel.gt.30.0) ladel=60.0
c
c  lodel
c  
      lodel=(lomax-lomin)/5.0
      if(lodel.le.0.25) lodel=0.25
      if(lodel.gt.0.25.and.lodel.le.0.5) ladel=0.5
      if(lodel.gt.0.5.and.lodel.le.1.0) lodel=1.0
      if(lodel.gt.1.0.and.lodel.le.2.0) lodel=2.0
      if(lodel.gt.2.0.and.lodel.le.5.0) lodel=5.0
      if(lodel.gt.5.0.and.lodel.le.10.0) lodel=10.0
      if(lodel.gt.10.0.and.lodel.le.20.0) lodel=20.0
      if(lodel.gt.20.0.and.lodel.le.30.0) lodel=30.0
      if(lodel.gt.30.0) lodel=60.0
      write(6,'(a,2f6.2)')'Grid size in lat and lon: ', ladel,lodel

c
c     now make plot
c

c   horizontal size. initally 17 cm. adjust so  vertical size
c   is max 1.5 times horizontal size
c
      hsize=17
      factor=(lamax-lamin)/((lomax-lomin)*cos(lamax/57.3))
      if(factor.gt.1.5) hsize=hsize/factor 
      write(thsize,'(i2)')hsize
      if(thsize(1:1).eq.' ') thsize(1:1)='0'
    
c
c   write outline coordinates to a text string
c
      write(outline,'(4(1x,f8.3))') lomin,lomax,lamin,lamax
c
c   put in separaters
c
      outline(10:10)='/'
      outline(19:19)='/'
      outline(27:27)='/'
c
c   remove blanks
c
      j=0
      do i=1,36
         if(outline(i:i).ne.' ') then
            j=j+1
            outline(j:j)=outline(i:i)
         endif
      enddo 
      if(j.ne.36) outline(j+1:36)=' '
c
c   put in midpoint to a string
c
      write(midpoint,'(2(1x,f8.3))') 
     *(lomin+lomax)/2.0,(lamin+lamax)/2.0
c
c   put in separaters
c
      midpoint(9:9)='/'
      midpoint(18:18)='/'
c
c   remove blanks
c
      j=0
      do i=1,18
         if(midpoint(i:i).ne.' ') then
            j=j+1
            midpoint(j:j)=midpoint(i:i)
         endif
      enddo 
      if(j.ne.18) midpoint(j+1:18)=' '

c
c   font and size of title
c
      text='gmt gmtset  FONT_TITLE = 14p,Helvetica,black' 
      write(4,'(a)')com
      write(4,'(a)')com//
     *'Set font, size and color of title'
      write(4,'(a)')com
      write(4,'(a)') text
      call systemc(text(1:seiclen(text)),seiclen(text))

c
c   cut out desired area from grid file to temporary file mapg.grid
c     
      text=' '
      text='gmt grdcut '//gmt_gridfile(1:sei clen(gmt_gridfile))//
     *' -R'//outline//' -Gmapg.grid' 
      write(4,'(a)')com
      write(4,'(a)')com//
     *'Cut out desired area from grid file, area is '//outline
      write(4,'(a)')com
      write(4,'(a)') text
      call systemc(text(1:seiclen(text)),seiclen(text))
c
c   make the color table, very default, -CGlobe is colors for global 
c   bathymetry/topography relief
c
      text=' '

      text='gmt makecpt -Cglobe -T-8500/8500/50 -Z > mapg.cpt'
      write(4,'(a)')com
      write(4,'(a)')com//'Make color table, global colors'
      write(4,'(a)')com
      write(4,'(a)') text  
      call systemc(text(1:seiclen(text)),seiclen(text))  
c
c   plot base map grid
c
      text=' '
      title(2:11)='Epicenters'
      title(1:1)='"'
      title(12:12)='"'
c
c   grid, not smaller resolution than 0.2 deg, same resolution
c   for grid and annotation
c
      write(gridx,'(2(1x,f6.2))')lodel,lodel
      gridx(1:1)='a'   ! anotation
      gridx(8:8)='g'   ! grid 

      write(gridy,'(2(1x,f6.2))')ladel,ladel
      gridy(1:1)='a'   ! anotation
      gridy(8:8)='g'   ! grid 
c
c
c   replace blank by 0
c
      do i=1,14
         if(gridx(i:i).eq.' ') gridx(i:i)='0'
         if(gridy(i:i).eq.' ') gridy(i:i)='0'
      enddo 
c

      text='gmt psbasemap -Bx'//gridx//' -By'//gridy//' -B+t'//
     *title(1:seiclen(title))
     *//' -JM'//midpoint(1:seiclen(midpoint))//thsize//'c -R'//
     *outline//' -K -P >mapg.ps'

      write(4,'(a)')com
      write(4,'(a)')com//'Plot base map, midpoint is '//midpoint//
     *'  horizontal size in cm is '//thsize
      write(4,'(a,2f7.2)')com//'Longitude and latitude grids are: ',
     *lodel,ladel
      write(4,'(a)')com
      write(4,'(a)') text
      call systemc(text(1:seiclen(text)),seiclen(text))
c
c   Compute directional gradients from a grid
c
      write(4,'(a)')com
      write(4,'(a)')com//' Compute directional gradients from a grid'
      write(4,'(a)')com
      text=' '
      text='gmt grdgradient mapg.grid -Nt1 -A225 -Gmapg.gradient'
      write(4,'(a)') text
      call systemc(text(1:seiclen(text)),seiclen(text))
c
c   map
c
      text=' '
      text='gmt grdimage -JM'//midpoint(1:seiclen(midpoint))
     *//thsize//'c -R'//outline//  
     *' mapg.grid  -Imapg.gradient  -O -K  -Cmapg.cpt >> mapg.ps'
      write(4,'(a)')com
      write(4,'(a)')com//'Make the map'
      write(4,'(a)')com
      write(4,'(a)')text
      call systemc(text(1:seiclen(text)),seiclen(text))
c
c   coast
c
      text=' '
      text='gmt pscoast -JM'//midpoint(1:seiclen(midpoint))
     *//thsize//'c -B -R'//outline//
     *' -Dh -N1/1.00p,- -K -O -W >> mapg.ps'
      write(4,'(a)')com
      write(4,'(a)')com//'Plot coast line'
      write(4,'(a)')com
      write(4,'(a)') text
      call systemc(text(1:seiclen(text)),seiclen(text))
c
c   epicenters,lon, lat and mag in gmtxy.out
c
      text=' '
      write(4,'(a)')com  
      if(scale_magnitude) then
         text='gmt psxy gmtxy.out -R -J -Sc'      ! plot accordidng to size
         write(4,'(a)')com//
     *   'Plot epicenters, size proportinal to magnitude'
      else
         text='gmt psxy gmtxy.out -R -J -Sc0.14'  ! fixed size
         write(4,'(a)')com//
     *   'Plot epicenters, size is 0.14'
      endif
      
      text=text(1:seiclen(text))//' -G200/0/0 -W0.3 -O -K >> mapg.ps' 

      write(4,'(a)')com
      write(4,'(a)') text
      call systemc(text(1:seiclen(text)),seiclen(text)) 
c
c   stations if chosen, station file is gmtstxy.out, color is blue
c
      if(stations) then
         text=' '
         text='gmt psxy gmtstxy.out -R -J -St0.4 -Gblue -W1 -O -K'
     *   //' >> mapg.ps'
         write(4,'(a)')com
         write(4,'(a)')com//'Plot stations, blue triangles, size 0.4'
         write(4,'(a)')com
         write(4,'(a)') text
         call systemc(text(1:seiclen(text)),seiclen(text)) 

c   plot station codes if chosen
c
         if(plot_station_code) then
            write(4,'(a)')com
            write(4,'(a)')com//'Station codes'
            write(4,'(a)')com//
     *      'The code is dx=0.0 and dy=0.5 from station location'
            write(4,'(a)')com
            text=' '
            text=
     *      'gmt pstext gmtstxy.out -D0.0/0.5 -R -J -O -K >> mapg.ps' 
            call systemc(text(1:seiclen(text)),seiclen(text)) 
            write(4,'(a)') text
            write(4,'(a)')com
          endif
      endif
c
c  fault plane solutions if chosen
c
       if(faultplane) then
         text=
     *   'gmt psmeca psmeca.out -R -J -V -Sa1.0/-1  -P -O >> mapg.ps'
         if(.not.scale_fps)
     *   text=
     *   'gmt psmeca psmeca.out -R -J -V -Sa1.0/-1 -M -P -O >> mapg.ps'
         write(4,'(a)')com
         write(4,'(a)')
     *   com//'Plot fault plane solutions, -1 to avoid date on top of'
         write(4,'(a)')
     *   com//'of fps, 1.0 is size proportinal to magnitude, to make '
         write(4,'(a)')com//
     *   'fixed size, add parameter -M'
         write(4,'(a)')com
         write(4,'(a)') text
         write(4,'(a)')com
         call systemc(text(1:seiclen(text)),seiclen(text))
       endif 

c
c
c   convert from ps to PDF, only works if ghostscrip is installed
c
      text=' '
      text='gmt psconvert mapg.ps -Tf'
c         write(4,'(a)')com
c      write(4,'(a)')com//'Convert'
c         write(4,'(a)')com
c      write(4,'(a)') text
         write(4,'(a)')com
c      call systemc(text(1:seiclen(text)),seiclen(text))
c
c   plot file on screen with ps veiwer as defined in SEISAN.DEF
c   most common is ghostscript
c
      text=' '
      text=plot_ps_command(1:seiclen(plot_ps_command))//' mapg.ps'
      write(4,'(a)')com
      write(4,'(a)')com//'Plot map on screen'
      write(4,'(a)')com
      write(4,'(a)') text
      call systemc(text(1:seiclen(text)),seiclen(text))
      write(4,'(a)')com

      close(4)


      write(6,*)            ! blank line
      write(6,*) 'Number of events in input file or data base', nevent
      write(6,*) 
     *'Output of events in input file with location: mapg.out'
      write(6,*) 'Output of postscript file with map: mapg.ps'
      write(6,*) 'Output of gmt script: mapg.gmt'


      stop
      end
