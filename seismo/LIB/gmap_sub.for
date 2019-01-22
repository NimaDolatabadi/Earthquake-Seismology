c
c     gmap subroutines
c     
c     todo move subroutines from gmap.for to here
c
c     2011-11-14 pv: gmap_sub.for created
c     2012-04-19 pv: added subroutine kmlerrorellipse
c     2014-04-29 jh: catch error readıng residual
c     2015-02-11 pv: bug fixed when checking for last line
c     2015-02-11 pv: added station code to gmap_auto
c     2015-06-22 pv: cleanup
c     2015-08-21 jh: change search for STAT to STAT SP IPHASW
c
c-------------------------------------
c     
      subroutine gmap_auto(data_old,data)
c                    data_old is the s-file header line with the old location
c                    data is the current s-fil (new location)
c
c     2011-11-14 pv: gmapactive first beta version, called from hyp
c     2011-11-26 pv: renamed to gmap_auto, defaults added to SEISAN.DEF
c     2011-12-22 pv: added staturl, fileaction and actiononfile
c     2012-01-24 pv: added LookAt
c     2012-02-01 lo: add close of SEISAN.DEF after reading
c                    SEISAN.DEF content should stay in menory, the
c                    reading part should be moved to general reading routine
c     2012-04-18 pv: added xmlns:gx="http://www.google.com/kml/ext/2.2" and
c                    <gx:drawOrder>1</gx:drawOrder> to fix ploting order
c     2012-04-19 pv: added call to subroutine kmlerrorellipse
c
c     subroutine gmapactive(level,ofile,data_old,data,nstations)
c
C
C    Seisan library inserts and routines...
C    ======================================
C
      include 'libsei.inc'                ! Open file definitions
      include 'seidim.inc'                ! dimentions
c
      external sei open,                  ! Open file routine.
     &         sei close,                 ! Close file routine.
     &         sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
      character*80 data_old
      character*80 data(2000)

      integer i,j,k

      character*120 defdata
      integer def_unit
      logical  b_eof                         ! End of file?.
c returned code
      integer          code

      integer io
      real MSIZE
      real ZSIZE
      real YSIZE
      logical errorellipse
      logical raypath
      character*8 pathcolor
      real lookataltitude
      real pathwidth
      real eqlon, eqlat,ery,erx,erz,cvxy,cvxz,cvyz
      logical showstat
      real STATSIZE
      real residualgood
      real residualbad
      logical showoldlocation
      character*8 coloroldlocation
      character*80 dummy
      real rdum
      character*80 url
      character*80 staturl
      real mag, res
      real dist, azi, ele
      real newlon, newlat
      character*8 color
      character*8 statcolorgood, statcolorok, statcolorbad
      character*7 magnitude
      character*5 oldstat, newstat
      logical lastline
      logical statline
      character*20 gmapcoor
      logical fileaction
      character*80 actiononfile
      character*8 ellipsecolor
      real ellipsewidth

c     open(9,file='gmapdata.tmp')
c     write(9,'(a80,1x)')data_old(1:80)
c     write(9,*)''

c  set default parameters
c
      url="http://maps.google.com/mapfiles/kml/pal2/icon26.png"
      color="ff0000ff"
      MSIZE=0.5
      XSIZE=0.2
      YSIZE=0.5
      lookataltitude=2000000.0
      showstat=.TRUE.
      errorellipse=.TRUE.
      staturl="http://maps.google.com/mapfiles/kml/shapes/triangle.png"
      STATSIZE=1.0
      residualgood=0.5
      residualbad=1.5
      statcolorgood="ff00ff00"
      statcolorok="ff00ffff"
      statcolorbad="ff0000ff"
      raypath=.TRUE.
      pathcolor="ff929292"
      pathwidth=2.5
      showoldlocation=.TRUE.
      coloroldlocation="ffff0000"
      fileaction=.FALSE.
      ellipsecolor="50780AF0"
      ellipsecolor="ff000000"
      ellipsewidth=2.0

c
c end default parameters

      do i=1,80
        dummy(i:i)=' '
      enddo

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   open and read default file
c   ---------------------------
c     n_gmap_append_kml=0
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   def_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'SEISAN.DEF' )    ! For this file.
c
c   read file if there...
c   ---------------------
c
c     if(code.ne.e_ok$)  return

 333  continue
c
      read(def_unit,'(a)',iostat=code) defdata    ! Read from file.
      call sei code( fort$,                    ! Process fortran i/o condition.
     &               code,                     ! Condition.
     &               def_unit,                 ! On unit.
     &               b_eof )                   ! End of file?.
c
      if( .not.b_eof ) then                    ! Not end of file.
c
c   Look for GMAP parameters
c
c      write(9,*) 'defdata :',defdata
       if (defdata(1:13).eq.'GMAP_AUTO_RUN') then
            read(defdata(41:43),'(f3.1)') rdum
            if(rdum.LT..5) return
       elseif (defdata(1:20).eq.'GMAP_AUTO_ICON_EVENT') then
	    read(defdata(41:120),'(a)') url(1:80)
       elseif (defdata(1:20).eq.'GMAP_AUTO_ICON_COLOR') then
            read(defdata(41:48),'(a)') color
       elseif (defdata(1:25).eq.'GMAP_AUTO_LOOKAT_ALTITUDE') then
            read(defdata(41:55),'(f15.5)') lookataltitude
       elseif (defdata(1:19).eq.'GMAP_AUTO_SHOW_STAT') then
            read(defdata(41:43),'(f3.1)') rdum
            if(rdum.LT..5)showstat=.FALSE.
            if(rdum.GT..5)showstat=.TRUE.
       elseif (defdata(1:23).eq.'GMAP_AUTO_ERROR_ELLIPSE') then
            read(defdata(41:43),'(f3.1)') rdum
            if(rdum.LT..5)errorellipse=.FALSE.
            if(rdum.GT..5)errorellipse=.TRUE.
       elseif (defdata(1:23).eq.'GMAP_AUTO_ELLIPSE_COLOR') then
            read(defdata(41:48),'(a8)') ellipsecolor
       elseif (defdata(1:23).eq.'GMAP_AUTO_ELLIPSE_WIDTH') then
            read(defdata(41:45),'(f5.2)') ellipsewidth
       elseif (defdata(1:18).eq.'GMAP_AUTO_STAT_URL') then
            read(defdata(41:120),'(a)') staturl(1:80)
       elseif (defdata(1:19).eq.'GMAP_AUTO_STAT_SIZE') then
            read(defdata(41:55),'(f15.5)') STATSIZE
       elseif (defdata(1:28).eq.'GMAP_AUTO_STAT_RESIDUAL_GOOD') then
            read(defdata(41:55),'(f15.5)') residualgood
       elseif (defdata(1:28).eq.'GMAP_AUTO_STAT_RESIDUAL_BAD') then
            read(defdata(41:55),'(f15.5)') residualbad
       elseif (defdata(1:25).eq.'GMAP_AUTO_STAT_COLOR_GOOD') then
            read(defdata(41:48),'(a)') statcolorgood
       elseif (defdata(1:23).eq.'GMAP_AUTO_STAT_COLOR_OK') then
            read(defdata(41:48),'(a)') statcolorok
       elseif (defdata(1:24).eq.'GMAP_AUTO_STAT_COLOR_BAD') then
            read(defdata(41:48),'(a)') statcolorbad
       elseif (defdata(1:20).eq.'GMAP_AUTO_ICON_MSIZE') then
            read(defdata(41:55),'(f15.5)') MSIZE
       elseif (defdata(1:20).eq.'GMAP_AUTO_ICON_XSIZE') then
            read(defdata(41:55),'(f15.5)') XSIZE
       elseif (defdata(1:20).eq.'GMAP_AUTO_ICON_YSIZE') then
            read(defdata(41:55),'(f15.5)') YSIZE
       elseif (defdata(1:27).eq.'GMAP_AUTO_SHOW_OLD_LOCATION') then
            read(defdata(41:43),'(f3.1)') rdum
            if(rdum.LT..5)showoldlocation=.FALSE.
            if(rdum.GT..5)showoldlocation=.TRUE.
       elseif (defdata(1:28).eq.'GMAP_AUTO_OLD_LOCATION_COLOR') then
            read(defdata(41:48),'(a8)') coloroldlocation
       elseif (defdata(1:19).eq.'GMAP_AUTO_SHOW_PATH') then
            read(defdata(41:43),'(f3.1)') rdum
            if(rdum.LT..5)raypath=.FALSE.
            if(rdum.GT..5)raypath=.TRUE.
       elseif (defdata(1:20).eq.'GMAP_AUTO_PATH_COLOR') then
            read(defdata(41:48),'(a8)') pathcolor
       elseif (defdata(1:20).eq.'GMAP_AUTO_PATH_WIDTH') then
            read(defdata(41:48),'(f8.3)') pathwidth
       elseif (defdata(1:21).eq.'GMAP_AUTO_FILE_ACTION') then
            read(defdata(41:43),'(f3.1)') rdum
            if(rdum.LT..5)fileaction=.FALSE.
            if(rdum.GT..5)fileaction=.TRUE.
       elseif (defdata(1:24).eq.'GMAP_AUTO_ACTION_ON_FILE') then
            read(defdata(41:120),'(a)') actiononfile(1:80)
       endif
c
c   go to next line
c
      goto 333
      endif
      call sei close( close$, def_unit, code ) ! Close (Default stop on error).

c      write(9,*) 'AUTO MSIZE:',MSIZE
c      write(9,*) 'AUTO XSIZE:',XSIZE
c      write(9,*) 'AUTO YSIZE:',YSIZE

c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cc

c get lat long of eq
      read(data(1)(24:30),'(f7.3)') eqlat
      read(data(1)(31:38),'(f8.3)') eqlon

      magnitude="       "
c get magnitude
      if(data(1)(73:79).ne.'       ')magnitude=data(1)(73:79)
      if(data(1)(65:71).ne.'       ')magnitude=data(1)(65:71)
      if(data(1)(57:63).ne.'       ')magnitude=data(1)(57:63)
      if(magnitude.ne."       ")then
       mag=ichar(magnitude(1:1))-48.0
       mag=mag+0.1*(ichar(magnitude(3:3))-48.0)
      else
       mag=MSIZE
      endif
      if(mag.lt.MSIZE)mag=MSIZE
      mag=XSIZE*mag**YSIZE

      io=2
      open(io,file='gmap.cur.kml')
      write(io,'(a38,1x)')'<?xml version="1.0" encoding="UTF-8"?>'
      write(io,'(a44,a45,1x)')
     +'<kml xmlns="http://www.opengis.net/kml/2.2" ',
     +'xmlns:gx="http://www.google.com/kml/ext/2.2">'
c    +'<kml xmlns="http://www.opengis.net/kml/2.2">'
c
      write(io,'(a12,1x)')'<Document>'
      write(io,'(a24,1x)')'<Style id="raypath">'
      write(io,'(a17,1x)')'<LineStyle>'
      write(io,'(a15,a8,a8,1x)')'<color>',pathcolor,'</color>'
      write(io,'(a15,f8.3,a8,1x)')'<width>',pathwidth,'</width>'
      write(io,'(a18,1x)')'</LineStyle>'
      write(io,'(a12,1x)')'</Style>'
      do i=24,38
        dummy(i:i)=data(1)(i:i)
      enddo
      write(io,'(a12,1x)')'<LookAt>'
      write(io,108)dummy(31:38)
      write(io,109)dummy(24:30)
c     write(io,'(a34,1x)')'<altitude>2000000</altitude>'
      write(io,'(a16,1x)')'<altitude>'
      write(io,*)"     ",lookataltitude
      write(io,'(a17,1x)')'</altitude>'
      write(io,'(a43,1x)')'<altitudeMode>absolute</altitudeMode>'
      write(io,'(a13,1x)')'</LookAt>'
c     write(io,'(a12,1x)')''
c     url="http://maps.google.com/mapfiles/kml/pal2/icon26.png"
c     color="ff0000ff"
c     mag=1.0
      write(io,'(a26,1x)')'<!-- Current location: -->'
      call makeplacemark(io,color,mag,url)
      write(io,'(a15,1x)')'<Point>'
      write(io,'(a55,1x)')
     +'<altitudeMode>relativeToGround</altitudeMode>'
      write(io,'(a40,1x)')
     +'<gx:drawOrder>1</gx:drawOrder>'
      write(io,110)dummy(31:38),dummy(24:30)
      write(io,'(a16,1x)')'</Point>'
      write(io,'(a18,1x)')'</Placemark>'
c
      if(errorellipse)then
        j=0
        DO i = 2, 2000                             ! Loop records.
          IF(data(i)(80:80).EQ.'E') THEN          ! If ellipses.
            READ(data(i),'(23X,F7.3,1X,F7.3,F5.1,3E12.4)')
     *                   ery,erx,erz,cvxy,cvxz,cvyz
            j=1
          endif
        enddo
        if(j.ge.1) then
          write(io,'(a23,1x)')'<!-- Error ellipse: -->'
c         call kmlerrorellipse(io,eqlon,eqlat,ery,erx,erz,cvxy,cvxz,cvyz)
        call kmlerrorellipse(io,eqlon,eqlat,ery,erx,erz,cvxy,cvxz,cvyz,
     +                       ellipsecolor,ellipsewidth)

        endif
      endif
c
      if(showoldlocation.AND.data_old(24:38).NE.'               ')then
      write(io,'(a22,1x)')'<!-- OLD location: -->'
      call makeplacemark(io,coloroldlocation,mag,url)
      write(io,'(a15,1x)')'<Point>'
      write(io,'(a55,1x)')
     +'<altitudeMode>relativeToGround</altitudeMode>'
      do i=24,38
        dummy(i:i)=data_old(i:i)
      enddo
      write(io,111)dummy(31:38),dummy(24:30)
      write(io,'(a16,1x)')'</Point>'
      write(io,'(a18,1x)')'</Placemark>'
      endif
c
c  map stations:
      oldstat='     '
      lastline=.FALSE.
      statline=.FALSE.
      do i=1,2000
c     write(111,'(a80,1x)')data(i)(1:80)
      if(data(i)(80:80).EQ.' '.AND.
     *data(i)(1:39).EQ.'                                       ')
     *lastline=.TRUE.
c     write(9,*) 'showstat  :',showstat
c     write(9,*) 'statline  :',statline
c     write(9,*) 'lastline  :',lastline
      if(.NOT.lastline.AND.statline)then
        newstat=data(i)(2:6)
c       write(9,*)'newstat,oldstat:',newstat,oldstat
        if(newstat.NE.oldstat)then


c        write(111,'(a80,1x)')data(i)(1:80)
c          write(*,*) ' debug ',data(i)(71:75)
          read(data(i)(71:75),'(f5.0)') dist
          read(data(i)(77:79),'(f3.0)') azi
          read(data(i)(77:79),'(f3.0)') stat
          call getstatres(newstat,data,res)
          call stat_loc(newstat,data(1)(21:21),newlat,newlon,ele)
c         write(9,*)eqlon,eqlat,dist,azi,newlon,newlat
c         write(9,*)'epic:',eqlon,eqlat
c         write(9,*)'stat:',newlon,newlat,newstat
c         write(9,*)'res :',res
          oldstat=data(i)(2:5)
c     url="http://maps.google.com/mapfiles/kml/shapes/triangle.png"
c     color="ffff0000"
      if(abs(res).GT.residualbad) then 
        color=statcolorbad
      else if(abs(res).LT.residualgood) then
        color=statcolorgood
      else
        color=statcolorok
      endif
      if(showstat)then
      write(io,'(a21,1x)')'<!-- Add station: -->'
      call makeplacemark(io,color,STATSIZE,staturl)
c     write(io,113) res
      write(io,'(a21,1x)')'<description>'
      write(io,'(a21,1x)')newstat
      write(io,'(a22,1x)')'</description>'
      write(io,'(a15,1x)')'<Point>'
      write(io,112)newlon,newlat
      write(io,'(a16,1x)')'</Point>'
      write(io,'(a18,1x)')'</Placemark>'
      endif
c map path
      if(raypath)then
      write(io,'(a21,1x)')'<!-- Add raypath: -->'
      write(io,'(a17,1x)')'<Placemark>'
      write(io,'(a37,1x)')'<styleUrl>#raypath</styleUrl>'
      write(io,'(a19,1x)')'<LineStyle>'
      write(io,'(a33,1x)')'<color>ff0000ff</color>'
      write(io,'(a28,1x)')'<width>2.5</width>'
      write(io,'(a20,1x)')'</LineStyle>'
      write(io,'(a20,1x)')'<LineString>'
      write(io,'(a36,1x)')'<tessellate>1</tessellate>'
      write(io,'(a23,1x)')'<coordinates>'
c     do j=1,80
c       dummy(j:j)=' '
c     enddo
c     k=31
c     do j=1,8
c       dummy(j:j)=data(1)(k:k)
c       k=k+1
c     enddo
c     dummy(9:9)=","
c     k=24
c     do j=10,16
c       dummy(j:j)=data(1)(k:k)
c       k=k+1
c     enddo
c     dummy(17:17)="0"
c     dummy(18:18)="0"
c     dummy(19:19)="0"
c     k=1
c     do j=1,19
c       if(dummy(j:j).NE." ") then
c         dummy(k:k)=dummy(j:j)
c         k=k+1
c       endif
c     enddo
c     dummy(17:17)=","
c     write(io,'(a18,1x)') dummy(1:18)
      call makegmapcoor(data(1)(1:80),gmapcoor)
c     write(9,'(a20,a20,1x)')'gmapcoordinate:',gmapcoor
      write(io,'(a20,1x)') gmapcoor
c     write(9,'(a20,a20,1x)')'gmapcoordinate:',gmapcoor
c     dummy(17:19)="000"
c     k=1
c     do j=1,19
c       if(dummy(j:j).NE." ") then
c         dummy(k:k)=dummy(j:j)
c         k=k+1
c       endif
c     enddo
c     dummy(17:18)=",0"
c     write(io,'(a18,1x)') dummy(1:18)
      write(dummy(31:38),'(f8.3)') newlon
      write(dummy(24:30),'(f7.3)') newlat
      call makegmapcoor(dummy,gmapcoor)
      write(io,'(a20,1x)') gmapcoor
      write(io,'(a24,1x)')'</coordinates>'
      write(io,'(a21,1x)')'</LineString>'
      write(io,'(a18,1x)')'</Placemark>'
      endif
c end map path : if(raypath)then
c end map stations
        endif
      endif
c
c   change chaeck here, jh aug 2015, somebody had written
c   STATUS here
c
      if(data(i)(2:15).EQ.'STAT SP IPHASW ')statline=.TRUE.
      enddo
c
      write(io,'(a13,1x)')'</Document>'
      write(io,'(a6,1x)')'</kml>'
      close(io)  

c     write(9,*) 'fileaction:',fileaction
c     write(9,*) actiononfile
c     if(fileaction) call systemc('echo DAVDAV',11)
c     if(fileaction) call systemc(actiononfile,80)
      if(fileaction) call systemc(actiononfile,24)
c     if(fileaction) call systemc('cp gmap.cur.kml slet2.tmp',25)
c     if(fileaction) call systemc(actiononfile,seiclen(actiononfile))
c
 108  FORMAT('      <longitude>',a8,'</longitude>')
 109  FORMAT('      <latitude>',a7,'</latitude>')
 110  FORMAT('          <coordinates>',a8,',',a7,',1.0</coordinates>')
 111  FORMAT('          <coordinates>',a8,',',a7,',0.0</coordinates>')
c112  FORMAT('       <coordinates>',f8.3,',',f8.3,',0,</coordinates>')
 112  FORMAT(10x,'<coordinates>',f8.3,',',f8.3,',0,</coordinates>')
 113  FORMAT('       <name> residual [s]:',f5.1,'</name>')
      end


      subroutine gmapcoordinate(data,string)
      character*80 data
      character*20 string
      integer k,j
      do j=1,20
        string(j:j)='0'
      enddo
c get long
      k=1
      do j=31,38
        if(data(j:j).NE." ") then
          string(k:k)=data(j:j)
          k=k+1
        endif
      enddo
c add comma
      k=k+1
      string(k:k)=","
c get lat
      do j=24,30
        if(data(j:j).NE." ") then
          string(k:k)=data(j:j)
          k=k+1
        endif
      enddo
c add comma
      k=k+1
      string(k:k)=","
c add zero
      k=k+1
      string(k:k)="0"
      k=k+1
      string(k:k)="."
      return
      end

      subroutine makegmapcoor(line,gmapcoor)
      character*20 gmapcoor
      character*80 line
      integer k,j
      do j=1,20
        gmapcoor(j:j)='0'
      enddo
c get long
      k=1
      do j=31,38
        if(line(j:j).NE." ") then
          gmapcoor(k:k)=line(j:j)
          k=k+1
        endif
      enddo
c add comma
      gmapcoor(k:k)=","
      k=k+1
c get lat
      do j=24,30
        if(line(j:j).NE." ") then
          gmapcoor(k:k)=line(j:j)
          k=k+1
        endif
      enddo
c add comma
      k=k+1
      gmapcoor(k:k)=","
c add zero
      k=k+1
      gmapcoor(k:k)="0"
      k=k+1
      gmapcoor(k:k)="."
      return
      end

      subroutine makeplacemark(io,color,size,url)
      integer io
      character*80 url
      real size
      character*8 color
      write(io,'(a17,1x)')'<Placemark>'
      write(io,'(a34,1x)')'<visibility>1</visibility>'
      write(io,'(a15,1x)')'<Style>'
      write(io,'(a21,1x)')'<IconStyle>'
      write(io,'(a19,a8,a8,1x)')'<color>',color,'</color>'
      write(io,'(a41,1x)')'<colorMode>normal</colorMode>'
      write(io,'(a19,f6.2,a8,1x)')'<scale>',size,'</scale>'
      write(io,'(a18,1x)')'<Icon>'
      write(io,'(a20)')'<href>'
      write(io,'(a16,a80)')"                ",url
      write(io,'(a21,1x)')'</href>'
      write(io,'(a19,1x)')'</Icon>'
      write(io,'(a22,1x)')'</IconStyle>'
      write(io,'(a16,1x)')'</Style>'
      return
      end

c---------------------------------------------------------------
c
      subroutine getstatres(stat,data,res)
c     recives stat and data and
c     will return the largest traveltime residual, in res
      character*5 stat,tstat
      character*80 data(*)
      real res,tres
c
      res=0.0
      tres=9999999.0
c
      i=1
    1 if(data(i)(80:80).EQ." ".AND.
     +data(i)(1:25).EQ."                         ") goto 9
      if(data(i)(80:80).EQ." ".OR.data(i)(80:80).EQ."4") then
      read(data(i)(2:6),'(a5)') tstat
      if(tstat.EQ.stat)then
        if(data(i)(64:68).NE."     ")then
c         read(data(i)(64:68),'(f5.1)',err=100) tres
          read(data(i)(64:68),'(f5.2)',err=100) tres
          if(abs(tres).gt.abs(res)) res=tres
 100      continue
c         write(6,*)' errror reading residual'
c         write(6,'(a)')data(1)(1:78)
        endif
      endif
      endif
      i=i+1
      goto 1
    9 continue
      if(tres.gt.9999998.) res=tres
      return
      end

c---------------------------------------------------------------
c
      subroutine kmlerrorellipse(io,eqlon,eqlat,
     &       ery,erx,erz,cvxy,cvxz,cvyz,ecolor,ewidth)
c    &                          ery,erx,erz,cvxy,cvxz,cvyz)
c     pv: based on subroutine hyperr, from epimap
      implicit none
      real slon,slat,a,b,ang,pi,theta,tmp
      real MINLON,MINLAT,MAXLON,MAXLAT
      integer np,i
      real x2,y2,sy,sx,X0,Y0,x1,y1,xprev,yprev
      real eqlon, eqlat,ery,erx,erz,cvxy,cvxz,cvyz
      real emaj,emin,eang
      integer io
      character*80 dummy
      character*20 gmapcoor
      character*8 ecolor
      real ewidth

      pi=3.141593
      np=50

      slon=eqlon
      slat=eqlat

               call xy_ellipse( erx,  ery,  erz,          ! Get details
     &                          cvxy, cvxz, cvyz,         !
     &                          emaj, emin, eang )         ! For drawing.

      a=emaj
      b=emin
      ang=eang

      theta=0.0
      sy=b*sin(theta)/110.93
      sx=a*cos(theta)/(111.3*cos((sy+slat)*pi/180.))
      tmp=slon+sx*cos(ang)-sy*sin(ang)
      yprev=slat+sy*cos(ang)+sx*sin(ang)
      xprev=tmp
c
c plot the ellipse
      write(io,'(a17,1x)')'<Placemark>'
      write(io,'(a15,1x)')'<Style>'
      write(io,'(a21,1x)')'<LineStyle>'
c     write(io,'(a35,1x)')'<color>ff000000</color>'
      write(io,'(a27,2a8,1x)')'<color>',ecolor,'</color>'
c     write(io,'(a28,1x)')'<width>2</width>'
      write(io,'(a20,f5.2,a8,1x)')'<width>',ewidth,'</width>'
      write(io,'(a22,1x)')'</LineStyle>'
      write(io,'(a16,1x)')'</Style>'
      write(io,'(a20,1x)')'<LineString>'
      write(io,'(a36,1x)')'<tessellate>1</tessellate>'
      write(io,'(a23,1x)')'<coordinates>'
      do i=1,np
       theta=float(i)*2.*pi/float(np)
c convert km to degrees
       sy=b*sin(theta)/110.93
       sx=a*cos(theta)/(111.3*cos((sy+slat)*pi/180.))
c
       tmp=slon+sx*cos(ang)-sy*sin(ang)
       y2=slat+sy*cos(ang)+sx*sin(ang)
       x2=tmp
       if(i.EQ.1) then
        x1=x2
        y1=y2
       endif
c plot point
       write(dummy(31:38),'(f8.3)') x2
       write(dummy(24:30),'(f7.3)') y2
       call makegmapcoor(dummy,gmapcoor)
       write(io,'(a20,1x)') gmapcoor
      enddo
      write(dummy(31:38),'(f8.3)') x1
      write(dummy(24:30),'(f7.3)') y1
      call makegmapcoor(dummy,gmapcoor)
      write(io,'(a20,1x)') gmapcoor
      write(io,'(a24,1x)')'</coordinates>'
      write(io,'(a21,1x)')'</LineString>'
      write(io,'(a18,1x)')'</Placemark>'
c
      return
      end

c---------------------------------------------------------------
c
c
      subroutine getcoor(lat,lon,alt,txt)
c     2015-10-20 pv: added high accuracy station coordinates
      character*30 txt
      integer i
      real lat,lon,alt
      lat=0.
      do i=7,27
        if(txt(i:i).eq.' ')txt(i:i)='0'
      enddo
c  lat : 
c  min : 
c     lat=(ichar(txt(9:9))-48)*10.
c     lat=lat+(ichar(txt(10:10))-48)*1.
c     lat=lat+(ichar(txt(12:12))-48)*.1
c     lat=lat+(ichar(txt(13:13))-48)*.01
      if(txt(11:11).NE.".") then           ! HIGH ACCURACY
        lat=(ichar(txt(9:9))-48)*10.
        lat=lat+(ichar(txt(10:10))-48)*1.
        lat=lat+(ichar(txt(11:11))-48)*.1
        lat=lat+(ichar(txt(12:12))-48)*.01
        lat=lat+(ichar(txt(13:13))-48)*.001
      else
        lat=(ichar(txt(9:9))-48)*10.
        lat=lat+(ichar(txt(10:10))-48)*1.
        lat=lat+(ichar(txt(12:12))-48)*.1
        lat=lat+(ichar(txt(13:13))-48)*.01
      endif
c  deg :
      lat=lat/60.
      lat=lat+(ichar(txt(8:8))-48)*1.
      lat=lat+(ichar(txt(7:7))-48)*10.
      if(txt(14:14).eq.'S')lat=-1.*lat
c     print*,"lat real : ",lat
c  lon : 
c  min : 
c     lon=(ichar(txt(18:18))-48)*10.
c     lon=lon+(ichar(txt(19:19))-48)*1.
c     lon=lon+(ichar(txt(21:21))-48)*.1
c     lon=lon+(ichar(txt(22:22))-48)*.01
      if(txt(20:20).NE.".") then           ! HIGH ACCURACY
        lon=(ichar(txt(18:18))-48)*10.
        lon=lon+(ichar(txt(19:19))-48)*1.
        lon=lon+(ichar(txt(20:20))-48)*.1
        lon=lon+(ichar(txt(21:21))-48)*.01
        lon=lon+(ichar(txt(22:22))-48)*.001
      else
        lon=(ichar(txt(18:18))-48)*10.
        lon=lon+(ichar(txt(19:19))-48)*1.
        lon=lon+(ichar(txt(21:21))-48)*.1
        lon=lon+(ichar(txt(22:22))-48)*.01
      endif
c  deg :
      lon=lon/60.
      lon=lon+(ichar(txt(17:17))-48)*1.
      lon=lon+(ichar(txt(16:16))-48)*10.
      lon=lon+(ichar(txt(15:15))-48)*100.
      if(txt(23:23).eq.'W')lon=-1.*lon
c     print*,"lon real : ",lon
c  alt : 
      alt=0.
      if(txt(24:27).ne."    ") then
      alt=alt+(ichar(txt(27:27))-48)*1.
      alt=alt+(ichar(txt(26:26))-48)*10.
      alt=alt+(ichar(txt(25:25))-48)*100.
      if(txt(23:23).eq.'-') then
        alt=-1.*alt
      else
        alt=alt+(ichar(txt(24:24))-48)*1000.
      endif
      endif
c     print*,"alt real : ",alt

      return
      end

c
c---------------------------------------------------------------
c
      subroutine gmappoly
c
c program to convert SEISAN polygone (DAT/EUROPE.MAP) files to earth.google kml format.
c to compile try : g77 gmappoly.for -o gmappoly
c peter voss, 26 nov 2007
c
c version 0.0.1 Beta
c
c 2007-11-26 pv: changed icon for stations
c                added style id  
c
      character*80 file
      character*20 koor
      character*20 coor
      integer i,j,k,npoints
      real lat(500000),lon(500000)
c     real mag, scale, lat, lon, alt
c     logical new, cp


      write(6,*)' INPUT FILE NAME - e.g. SALVADOR.MAP'
      read(5,'(a)') file

      open(2,file='gmappoly.kml')
      write(2,'(a38,1x)')'<?xml version="1.0" encoding="UTF-8"?>'
      write(2,'(a45,1x)')'<kml xmlns="http://earth.google.com/kml/2.0">'
      write(2,'(a12,1x)')'<Document>'
      write(2,'(a21,1x)')'<Style id="poly">'
      write(2,'(a17,1x)')'<LineStyle>'
      write(2,'(a31,1x)')'<color>ff0000ff</color>'
      write(2,'(a26,1x)')'<width>2.5</width>'
      write(2,'(a18,1x)')'</LineStyle>'
      write(2,'(a17,1x)')'<PolyStyle>'
      write(2,'(a22,1x)')'<fill>0</fill>'
      write(2,'(a18,1x)')'</PolyStyle>'
      write(2,'(a12,1x)')'</Style>'

      open(1,file=file)

      write(2,'(a13,a80,1x)')'<name><B>'
      write(2,*)file
      write(2,'(a15,1x)')'</B></name>'
      write(2,'(a12,1x)')'<Folder>'

      write(2,'(a18,1x)')'<open>0</open>'

    1 read(1,'(i4)',end=99)npoints
      if(npoints.le.0)goto 99
      read(1,'(10f8.3)')(lat(i),lon(i),i=1,npoints)

      write(2,'(a15,1x)')'<Placemark>'
      write(2,'(a32,1x)')'<visibility>1</visibility>'
      write(2,'(a32,1x)')'<styleUrl>#poly</styleUrl>'
      write(2,'(a15,1x)')'<Polygon>'
      write(2,'(a25,1x)')'<outerBoundaryIs>'
      write(2,'(a22,1x)')'<LinearRing>'
      write(2,'(a25,1x)')'<coordinates>'

      do i=1,npoints
      do k=1,20
        coor(k:k)=" "
        koor(k:k)=" "
      enddo
        write(koor,10)lon(i),lat(i)
      j=1
      do k=1,20
        if(koor(k:k).ne." ") then
          coor(j:j)=koor(k:k)
          j=j+1
        endif
      enddo
      write(2,*)coor
      enddo
      write(2,'(a28,1x)')'</coordinates>'
      write(2,'(a25,1x)')'</LinearRing>'
      write(2,'(a28,1x)')'</outerBoundaryIs>'
      write(2,'(a18,1x)')'</Polygon>'
      write(2,'(a18,1x)')'</Placemark>'
      goto 1
  99  continue                                            
      close(1)

      write(2,'(a13,1x)')'</Folder>'
      write(2,'(a13,1x)')'</Document>'
      write(2,'(a6,1x)')'</kml>'
      close(2)

      write(6,*)' OUTPUT FILE NAME : gmappoly.kml '

  10  FORMAT(f8.3,',',f8.3,',0')
      return
      end

c
c---------------------------------------------------------------
c
      subroutine gmapstat
c
c program to convert SEISAN STATION0.HYP files to earth.google kml format.
c to compile try : g77 gmapstat.for -o gmapstat
c peter voss, 25 nov 2007
c
c version 0.0.2 Beta
c
c 2007-11-26 pv: changed icon for stations
c                added style id  
c
      character*80 text
      character*80 txt
      character*80 file
      character*30 stxt
      character*80 koor
      character*80 coor
      character*5 stat
      integer i
      real scale, lat, lon, alt
      logical new

      scale=8.838
      scale=0.838
      scale=1.0
      new=.TRUE.

      write(6,*)' INPUT FILE NAME - e.g. STATION0.HYP'
      read(5,'(a)') file

      open(2,file='gmapstat.kml')
      write(2,'(a38,1x)')'<?xml version="1.0" encoding="UTF-8"?>'
      write(2,'(a45,1x)')'<kml xmlns="http://earth.google.com/kml/2.0">'
      write(2,'(a12,1x)')'<Document>'
      write(2,'(a21,1x)')'<Style id="stat">'
      write(2,'(a24,1x)')'<scale>1.0</scale>'
      write(2,'(a17,1x)')'<IconStyle>'
      write(2,'(a31,1x)')'<color>ff0000ff</color>'
      write(2,'(a37,1x)')'<colorMode>normal</colorMode>'
      write(2,'(a14,1x)')'<Icon>'
      txt(1:41)='<href>http://maps.google.com/mapfiles/kml'
      txt(42:68)='/shapes/triangle.png</href>'
      write(2,'(a78,1x)')txt(1:68)
      write(2,'(a15,1x)')'</Icon>'
      write(2,'(a19,1x)')'</IconStyle>'
      write(2,'(a12,1x)')'</Style>'

      open(1,file=file)

      write(2,'(a13,a80,1x)')'<name><B>',file
      write(2,'(a15,1x)')'</B></name>'

      write(2,'(a18,1x)')'<open>1</open>'

    6 read(1,'(a80,1x)',end=999)text                                     
      if(text(1:10).eq.'RESET TEST') goto 6
    7 read(1,'(a80,1x)',end=999)text                                     
      if(text(1:10).eq.'          '.and.new) goto 7
      new=.FALSE.
      if(text(1:10).eq.'          ') goto 999
      stxt(1:30)=text(1:30)
      stat(1:5)=text(2:6)
      do i=1,80
        coor(i:i)=" "
        koor(i:i)=" "
        txt(i:i)=text(i:i)
      enddo
      call getcoor(lat,lon,alt,txt(1:30))

      write(2,'(a15,1x)')'<Placemark>'
      write(2,'(a12,a5,a7,1x)')'<name>',stat,'</name>'
      write(2,'(a33,1x)')'<styleUrl>#stat</styleUrl>'
      write(2,'(a13,1x)')'<Point>'
      write(koor,10)lon,lat,alt
      do i=1,8
        coor(i:i)=koor(i:i)
      enddo
      j=9
      do i=9,58
        if(koor(i:i).ne." ") then
          coor(j:j)=koor(i:i)
          j=j+1
        endif
      enddo
      write(2,*)coor
      write(2,'(a14,1x)')'</Point>'
      write(2,'(a34,1x)')'<description> <![CDATA[<pre>'
      write(2,'(a30,1x)')text(1:30)
      write(2,'(a29,1x)')'</pre>]]></description>'
      write(2,'(a16,1x)')'</Placemark>'
      goto 7
  999 continue                                            
      close(1)

      write(2,'(a13,1x)')'</Document>'
      write(2,'(a6,1x)')'</kml>'
      close(2)

      write(6,*)' OUTPUT FILE NAME : gmapstat.kml '

  10  FORMAT
     +('      <coordinates>',f9.4,',',f8.4,',',f6.1,'</coordinates>')
      return
      end

c
c---------------------------------------------------------------
c
      subroutine wkml(io,text,max_data,nr,url,visible,color,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth)
c
c       addes/writes the KML code of a singel event/s-file to the file io
c
c     io is a integer with the output file unit nr.
c     text is a character array with the content of the sfile
c     max_data a integer with the maximum allowed number of lines in the sfile
c     nr a integer with the number of lines in the sfile
c     url is a character array with the URL for the icon
c     visible is a logical that if true hides the events
c     color is a character array with the kml color string
c     sdata is a logical that will add the sfile to the kml file if true
c     ttag is a logical that will add a timetag to the kml file if true
c     errorellipse is a logical that will add a the errorellipse if true
c     MSIZE,XSIZE,YSIZE are reals that define the size of the epicenter icon
c
c     2015-06-22 pv: fix bug setting size on event id Q
c
      character*80 text(max_data)
      character*80 dummy,url
      character*8 color
      character*7 magnitude
      integer io,i,j
      real mag
      real eqlon,eqlat,ery,erx,erz,cvxy,cvxz,cvyz
      real MSIZE,XSIZE,YSIZE
      logical visible
      logical sdata
      logical ttag			! timetag / timespan
      logical errorellipse  ! kml file will include error ellipse
      character*8 ellipsecolor
      real ellipsewidth

c
      magnitude="       "
c
c get lat long of eq
      read(text(1)(24:30),'(f7.3)') eqlat
      read(text(1)(31:38),'(f8.3)') eqlon

c remove chars like backspace
      do i=1,nr
        do j=1,80
          if(ichar(text(i)(j:j)).lt.32) text(i)(j:j)=" "
        enddo
      enddo

c get magnitude
      if(text(1)(73:79).ne.'       ')magnitude=text(1)(73:79)
      if(text(1)(65:71).ne.'       ')magnitude=text(1)(65:71)
      if(text(1)(57:63).ne.'       ')magnitude=text(1)(57:63)

      if(text(1)(7:7).eq.' ')text(1)(7:7)='0'
      if(text(1)(9:9).eq.' ')text(1)(9:9)='0'
      if(text(1)(17:17).eq.' ')text(1)(17:17)='0'
      if(text(1)(28:28).eq.' ')text(1)(28:28)='0'
      if(text(1)(29:29).eq.' ')text(1)(29:29)='0'
      if(text(1)(30:30).eq.' ')text(1)(30:30)='0'
      if(text(1)(36:36).eq.' ')text(1)(36:36)='0'
      if(text(1)(37:37).eq.' ')text(1)(37:37)='0'
      if(text(1)(38:38).eq.' ')text(1)(38:38)='0'
c mag is size of circle
      if(magnitude.ne."       ")then
       mag=ichar(magnitude(1:1))-48.0
       mag=mag+0.1*(ichar(magnitude(3:3))-48.0)
      else
       mag=MSIZE
      endif
      if(mag.lt.MSIZE)mag=MSIZE
      mag=XSIZE*mag**YSIZE
      if(text(1)(23:23).ne."Q".AND.text(1)(23:23).ne." ")mag=mag*2.0
      write(io,'(a17,1x)')'<Placemark>'
      if(visible) then
	write(io,'(a34,1x)')'<visibility>1</visibility>'
      else
	write(io,'(a34,1x)')'<visibility>0</visibility>'
      endif
      if(sdata) then
      write(io,'(a14,a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a6,a1,a7,a7,1x)')
     +'<name>',text(1)(2:5),'-',text(1)(7:8),'-',text(1)(9:10),'_',
     +text(1)(12:13),':',text(1)(14:15),':',text(1)(17:22),'_',
     +text(1)(57:63),'</name>'
      else
      write(io,
     +'(a21,a3,a2,a1,a1,a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a4,a14,1x)')
     +'<description>',
     +magnitude(1:3),' M',magnitude(4:4),' ',
     +text(1)(2:5),'-',text(1)(7:8),'-',text(1)(9:10),'_',
     +text(1)(12:13),':',text(1)(14:15),':',text(1)(17:20),
     +'</description>'
      endif
      write(io,'(a15,1x)')'<Style>'
      write(io,'(a21,1x)')'<IconStyle>'
      write(io,'(a19,a8,a8,1x)')'<color>',color,'</color>'
      write(io,'(a41,1x)')'<colorMode>normal</colorMode>'
      write(io,'(a19,f6.2,a8,1x)')'<scale>',mag,'</scale>'
      write(io,'(a18,1x)')'<Icon>'
      write(io,'(a20)')'<href>'
      write(io,'(a16,a80)')"                ",url
      write(io,'(a21,1x)')'</href>'
      write(io,'(a19,1x)')'</Icon>'
      write(io,'(a22,1x)')'</IconStyle>'
      write(io,'(a16,1x)')'</Style>'
      write(io,'(a15,1x)')'<Point>'
      do i=24,38
        dummy(i:i)=text(1)(i:i)
      enddo
      write(io,111)dummy(31:38),dummy(24:30)
      write(io,'(a16,1x)')'</Point>'
      if(ttag) then
        write(io,'(a36,1x)')'<TimeSpan>'
        write(io,'(a16,a4,a1,a2,a1,a2,a9,1x)')
     +'<begin>',text(1)(2:5),'-',text(1)(7:8),'-',text(1)(9:10),
     +'</begin>'
        write(io,'(a16,a4,a1,a2,a1,a2,a6,1x)')
     +'<end>',text(1)(2:5),'-',text(1)(7:8),'-',text(1)(9:10),
     +'</end>'
        write(io,'(a36,1x)')'</TimeSpan>'
      endif
      if(sdata) then
        write(io,'(a36,1x)')'<description> <![CDATA[<pre>'
        do i=1,nr
          write(io,'(a80)')text(i)(1:80)
        enddo
        write(io,'(a31,1x)')'</pre>]]></description>'
      endif
      write(io,'(a18,1x)')'</Placemark>'
      if(errorellipse) then
        j=0
        DO i = 2,nr                               ! Loop records.
          IF(text(i)(80:80).EQ.'E') THEN          ! If ellipses.
            READ(text(i),'(23X,F7.3,1X,F7.3,F5.1,3E12.4)')
     *                     ery,erx,erz,cvxy,cvxz,cvyz
            j=1
          endif                                   !
        enddo
        if(ery.GT.500.OR.erx.GT.500.0.OR.erz.GT.500.0) j=-1
        if(j.eq.-1) then
          write(io,'(a66,1x)')
     +'<!-- No Error ellipse: Error is too large -->'
        endif 
        if(j.ge.1) then
          write(io,'(a23,1x)')'<!-- Error ellipse: -->'
c         call kmlerrorellipse(io,eqlon,eqlat,ery,erx,erz,cvxy,cvxz,cvyz)
c         ellipsecolor="ff000000"
c         ellipsewidth=2.0
        call kmlerrorellipse(io,eqlon,eqlat,ery,erx,erz,cvxy,cvxz,cvyz,
     +                       ellipsecolor,ellipsewidth)
        endif 
      endif

 111  FORMAT('          <coordinates>',a8,',',a7,',0,</coordinates>')

      return
      end

c
c---------------------------------------------------------------
c
      subroutine wkmlheader(io)
      integer io
      write(io,'(a38,1x)')'<?xml version="1.0" encoding="UTF-8"?>'
      write(io,'(a45,1x)')
     +'<kml xmlns="http://earth.google.com/kml/2.1">'
      write(io,'(a10,1x)')'<Document>'
      write(io,'(a18,1x)')'<open>0</open>'
      write(io,'(a31,1x)')
     +'<name>SEISAN</name>'
      return
      end
c
c---------------------------------------------------------------
c
      subroutine wkmltail(io)
      integer io
      write(io,'(a13,1x)')'</Document>'
      write(io,'(a6,1x)')'</kml>'
      return
      end
c
c---------------------------------------------------------------
c
