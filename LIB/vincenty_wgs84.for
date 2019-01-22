c
c Feb 2015 first version
c
c Routines for Geodetic computations (WGS84)
c
c This file contain the vincenty_forward(lon,lat,azi,dist)
c and vincenty_inverse(lo,la,lo2,la2) subroutines
c
c vincenty_forward(lon,lat,azi,dist) computes the lat and lon
c for a point at an azimuth and distance (meter) from
c the point given by input lon and lat. Output lon and lat are 
c returned at the position of input lon and lat
c
c vincenty_inverse(lo,la,lo2,la2) computes the azimuth, back-azumith and
c distance between the two points lo,la,lo2,la2. Outout azimuth, 
c back-azumith and distance (meters) are returned at the position
c of lo,la,lo2.
c
c the code is based on the code at 
c http://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/
c see also http://www.movable-type.co.uk/scripts/latlong-vincenty.html
c
c please check for bugs before use.
c
C program example that show how to call the Vincenty routines:
c     double precision lo,la,azi,dist,lo2,la2
c     real lon,lat
c     lon=12.8833
c     lat=56.54
c     azi=0.0D0
c     dist=111000.0D0   ! meters
c     lo=DBLE(lon)
c     la=DBLE(lat)
c     lo2=DBLE(lon)
c     la2=DBLE(lat)
c     write(*,*)"lonlat:",lo,la
c     do i=1,3
c       write(*,*)"FORWARD:"
c       lo=DBLE(lon)
c       la=DBLE(lat)
c       write(*,*)"Input [lon,lat,azi,dist]:",lo,la,azi,dist
c       call vincenty_forward(lo,la,azi,dist)
c       write(*,*)"Output [lon,lat]:",lo,la
c       lo2=lo
c       la2=la
c       lo=DBLE(lon)
c       la=DBLE(lat)
c       write(*,*)"INVERSE:"
c       write(*,*)"Input: lo1 la1 lo2 la2:",lo,la,lo2,la2
c       call vincenty_inverse(lo,la,lo2,la2)
c       write(*,*)"Output: [azi,baz,dist]",lo,la,lo2
c       azi=azi+DBLE(90.0)
c       write(*,*)"-----------------------------------"
c     enddo
c     end
C End of example

      subroutine vincenty_forward(lon,lat,azi,dist)
c fixed to WGS84
c have removed common blocks for pi,rad,a and f
c
cb::forward
c
c     program forward
c
c********1*********2*********3*********4*********5*********6*********7**
c
c name:      forward
c version:   200208.19 
c author:    stephen j. frakes
c purpose:   to compute a geodetic forward (direct problem)
c            and then display output information
c
c input parameters:
c -----------------
c
c output parameters:
c ------------------
c
c local variables and constants:
c ------------------------------
c answer           user prompt response
c arc              meridional arc distance latitude p1 to p2 (meters)      
c b                semiminor axis polar (in meters)
c baz              azimuth back (in radians)
c blimit           geodetic distance allowed on ellipsoid (in meters)
c buff             input char buffer array
c dd,dm,ds         temporary values for degrees, minutes, seconds
c dmt,d_mt         char constants for units (in meters)        
c dd_max           maximum ellipsoid distance -1 (in meters)        
c edist            ellipsoid distance (in meters)
c elips            ellipsoid choice
c esq              eccentricity squared for reference ellipsoid
c faz              azimuth forward (in radians)
c filout           output file name
c finv             reciprocal flattening
c hem              hemisphere flag for lat & lon entry  
c ierror           error condition flag with d,m,s conversion
c lgh              length of buff() array
c option           user prompt response             
c r1,r2            temporary variables    
c ss               temporary value for ellipsoid distance
c tol              tolerance for conversion of seconds    
c
c name1            name of station one
c ld1,lm1,sl1      latitude  sta one - degrees,minutes,seconds
c ald1,alm1,sl1    latitude  sta one - degrees,minutes,seconds
c lat1sn           latitude  sta one - sign (+/- 1)
c d_ns1            latitude  sta one - char ('N','S')
c lod1,lom1,slo1   longitude sta one - degrees,minutes,seconds
c alod1,alom1,slo1 longitude sta one - degrees,minutes,seconds
c lon1sn           longitude sta one - sign (+/- 1)
c d_ew1            longitude sta one - char ('E','W')
c iaz1,maz1,saz1   forward azimuth   - degrees,minutes,seconds
c isign1           forward azimuth   - flag  (+/- 1)
c azd1,azm1,saz1   forward azimuth   - degrees,minutes,seconds
c iazsn            forward azimuth   - flag  (+/- 1)
c glat1,glon1      station one       - (lat & lon in radians )
c
c name2            name of station two
c ld2,lm2,sl2      latitude  sta two - degrees,minutes,seconds
c lat2sn           latitude  sta two - sign (+/- 1)
c d_ns2            latitude  sta two - char ('N','S')
c lod2,lom2,slo2   longitude sta two - degrees,minutes,seconds
c lon2sn           longitude sta two - sign (+/- 1)
c d_ew2            longitude sta two - char ('E','W')
c iaz2,maz2,saz2   back azimuth      - degrees,minutes,seconds
c isign2           back azimuth      - flag  (+/- 1)
c glat2,glon2      station two       - (lat & lon in radians )
c
c global variables and constants:
c -------------------------------
c a                semimajor axis equatorial (in meters)
c f                flattening
c pi               constant 3.14159....
c rad              constant 180.0/pi  
c
c    this module called by:  n/a
c
c    this module calls:      elipss, getrad, dirct1, todmsp
c    gethem, trim,   bufdms, gvalr8, gvali4, fixdms, gpnarc
c    datan,  write,  read,   dabs,   open,   stop
c
c    include files used:     n/a
c
c    common blocks used:     const, elipsoid
c
c    references:             see comments within subroutines
c
c    comments:
c
c********1*********2*********3*********4*********5*********6*********7**
c::modification history
c::1990mm.dd, sjf, creation of program           
c::199412.15, bmt, creation of program on viper
c::200203.08, crs, modified by c.schwarz to correct spelling of Clarke
c::                at request of Dave Doyle
c::200207.18, rws, modified i/o & standardized program documentation
c::                added subs trim, bufdms, gethem, gvali4, gvalr8      
c::200207.23, rws, added sub gpnarc
c::200208.15, rws, fixed an error in bufdms
c::              - renamed ellips to elipss "common error" with dirct2
c::              - added FAZ & BAZ to printed output 
c::200208.19, rws, added more error flags for web interface 
c::              - added logical nowebb                      
c::200208.xx, sjf, program version number 2.0                   
c********1*********2*********3*********4*********5*********6*********7**
ce::forward
c
      implicit double precision (a-h, o-z)
      implicit integer (i-n)
      double precision lon,lat, azi, dist
      double precision pi,rad
c
      logical  nowebb
c
      character*1  answer,option,dmt,buff(50),hem
      character*6  d_ns1, d_ew1, d_ns2, d_ew2, d_mt
      character*30 filout,name1,name2,elips
c
      integer*4    ierror
      integer*4    lgh
c
      integer np
c
c     common/const/pi,rad
c     common/elipsoid/a,f
c
c     ms_unix      0 = web version
c                  1 = ms_dos or unix
c
      parameter   ( ms_unix = 0 )
c
      pi=4.d0*datan(1.d0)
      rad=180.d0/pi
      dmt='m'
      d_mt='Meters'
c
      if( ms_unix.eq.1 )then
        nowebb = .true.
      else
        nowebb = .false.
      endif
c
    1 do 2 i=1,25
c       write(*,*) '  '
    2 continue
 
c   5 write(*,*) '  Program Forward  -  Version 2.0 '
c     write(*,*) '  '
c     write(*,*) '  Ellipsoid options : '
c     write(*,*) '  '
c     write(*,*) '  1) GRS80 / WGS84  (NAD83) '
c     write(*,*) '  2) Clarke 1866    (NAD27) '
c     write(*,*) '  3) Any other ellipsoid '
c     write(*,*) '  '
c     write(*,*) '  Enter choice : '
c     read(*,10) option
      option='1'
   10 format(a1)
c
c      if(option.eq.'1') then
        a=6378137.d0
        f=1.d0/298.25722210088d0
        elips='GRS80 / WGS84  (NAD83)'
c      elseif(option.eq.'2') then
c        a=6378206.4d0
c        f=1.d0/294.9786982138d0
c        elips='Clarke 1866    (NAD27)'
c      elseif(option.eq.'3') then
c        call elipss (elips)
c      else
c        write(*,*) '  Enter 1, 2, or 3 !   Try again --'
cc       goto 5
c      endif
c
      esq = f*(2.0d0-f)
c
      r1  = 0.0d0
      r2  = pi/2.0d0
      call vincenty_gpnarc ( a, f, esq, pi, r1, r2, arc )
c
c     compute the geodetic limit distance (blimit), it is equal
c     to twice the distance to the pole minus one meter
c
      blimit = 2.0d0*arc-1.0d0
c
c     maximum distance allowed on ellipsoid
c
      dd_max = blimit                  
c
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,*) '  '
c
c  15 write(*,*) '  Enter First Station '
c     write(*,16)
c  16 format(18x,'(Separate D,M,S by blanks or commas)')
c     write(*,*) 'hDD MM SS.sssss  Latitude :        (h default = N )'
c
   11 format(50a1)
c
   22 continue
      if(lat.gt.0.0D0+00)then
        hem='N'
      else
        hem='S'
        lat=DABS(lat)
      endif

c     read(*,11) buff
c     call trim (buff,lgh,hem)
c     call bufdms (buff,lgh,hem,dd,dm,ds,ierror)
      ierror=0
      dd=DINT(lat)
c     write(*,*) 'lat',lat
c     write(*,*) 'dd',dd
c     dd=56.0D0+00
      dm=DINT((lat-dd)*60.0D0+00)
c     write(*,*) 'dm',dm
c     dm=0.0D0+00
      ds=(((lat-dd)*60.0D0+00)-dm)*60.0D0+00
c     write(*,*) 'ds',ds
c     ds=0.0D0+00
c
      if( ierror.eq.0 )then
        irlat1 = 0
      else
        irlat1 = 1
        write(*,*) ' Invalid Latitude ... Please re-enter it '
        write(*,*) '  '
        if( nowebb )then
          goto 22
        endif
      endif
c
      ald1 = dd
      alm1 = dm
      sl1  = ds
c
      if( hem.eq.'N' ) then
        lat1sn = +1
      else
        lat1sn = -1
      endif
c
c     write(*,*) 'hDDD MM SS.sssss Longitude :       (h default = W )'
c
   23 continue
      if(lon.gt.0.0D0+00)then
        hem='E'
      else
        hem='W'
        lon=DABS(lon)
      endif
c     read(*,11) buff
c     call trim (buff,lgh,hem)
c     call bufdms (buff,lgh,hem,dd,dm,ds,ierror)
c     dd=12.0D0+00
c     dm=0.0D0+00
c     ds=0.0D0+00
      dd=DINT(lon)
c     write(*,*) 'lon',lon
c     write(*,*) 'dd',dd
      dm=DINT((lon-dd)*60.0D0+00)
c     write(*,*) 'dm',dm
      ds=(((lon-dd)*60.0D0+00)-dm)*60.0D0+00
c     write(*,*) 'ds',ds
c
      if( ierror.eq.0 )then
        irlon1 = 0
      else
        irlon1 = 1 
        write(*,*) ' Invalid Longitude ... Please re-enter it '
        write(*,*) '  '
        if( nowebb )then
          goto 23
        endif
      endif
c
      alod1 = dd
      alom1 = dm
      slo1  = ds
c
      if( hem.eq.'E' ) then
        lon1sn = +1
      else
        lon1sn = -1
      endif
c
      call vincenty_getrad(ald1, alm1, sl1, lat1sn, glat1)
      call vincenty_getrad(alod1,alom1,slo1,lon1sn, glon1)
c
c  20 write(*,*) 'DDD MM SS.sss    Forward Azimuth :     (from north)'
c
   24 hem='A'
c     read(*,11) buff
c     call trim (buff,lgh,hem)
c     call bufdms (buff,lgh,hem,dd,dm,ds,ierror)

c     open(4,file='cir.kml')
c     np=-1
c 999 continue
c     np=np+1
c       write(*,*) 'pv-np:',np
c     if(np.GE.359) stop
c     
c     dd=np*1.0D0+00
c     dm=0.0D0+00
c     ds=0.0D0+00
      dd=DINT(azi)
      dm=DINT((azi-dd)*60.0D0+00)
      ds=(((azi-dd)*60.0D0+00)-dm)*60.0D0+00
c
      if( ierror.eq.0 )then
        iazsn  = 1
        irazi1 = 0
      else
        irazi1 = 1
        write(*,*) ' Invalid Azimuth  ... Please re-enter it '
        write(*,*) '  '
        if( nowebb )then
          goto 24
        endif
      endif
c
      azd1 = dd
      azm1 = dm
      saz1 = ds
c
      call vincenty_getrad(azd1,azm1,saz1,iazsn,faz)
c
c     write(*,*) 'DDDDDD.dddd      Ellipsoidal Distance : (in meters)'
c
   25 ss = 0.0d0
c     read(*,*) ss
      ss=100000.0D0+00  ! 100km
      ss=1000000.0D0+00 ! 1000km
      ss=6000000.0D0+00 ! 6000km
      ss=dist
      ss = dabs(ss)
c
      if( ss.lt.dd_max )then
        edist  = ss
        irdst1 = 0
      else
        irdst1 = 1
        write(*,*) ' Invalid Distance ... Please re-enter it '
        write(*,*) '  '
        if( nowebb )then
          goto 25
        endif
        edist  = 0.001d0
      endif
c
      call vincenty_dirct1 (glat1,glon1,glat2,glon2,faz,baz,edist)
c
c     set the azimuth seconds tolerance
c
      tol = 0.00005d0
c
      call vincenty_todmsp(faz,iaz1,maz1,saz1,isign1)
      if(isign1.lt.0) then
        iaz1=359-iaz1
        maz1=59-maz1
        saz1=60.d0-saz1
      endif
      call vincenty_fixdms ( iaz1, maz1, saz1, tol )
c
      call vincenty_todmsp(baz,iaz2,maz2,saz2,isign2)
      if(isign2.lt.0) then
        iaz2=359-iaz2
        maz2=59-maz2
        saz2=60.d0-saz2
      endif
      call vincenty_fixdms ( iaz2, maz2, saz2, tol )
c
      call vincenty_todmsp(glat1, ld1,  lm1,  sl1,  lat1sn)
      call vincenty_todmsp(glon1, lod1, lom1, slo1, lon1sn)
      call vincenty_todmsp(glat2, ld2,  lm2,  sl2,  lat2sn)
      call vincenty_todmsp(glon2, lod2, lom2, slo2, lon2sn)
      lon=lon2sn*(lod2+lom2/60.+slo2/3600.)
      lat=lat2sn*(ld2+lm2/60.+sl2/3600.)
c
c     call hem_ns ( lat1sn, d_ns1 )
c     call hem_ew ( lon1sn, d_ew1 )
c     call hem_ns ( lat2sn, d_ns2 )
c     call hem_ew ( lon2sn, d_ew2 )
c
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,49) elips
   49 format('  Ellipsoid : ',a30)
      finv=1.d0/f
      b=a*(1.d0-f)
c     write(*,50) a,b,finv
   50 format('  Equatorial axis,    a   = ',f15.4,/,
     *       '  Polar axis,         b   = ',f15.4,/,
     *       '  Inverse flattening, 1/f = ',f16.11)
c
   18 format('    LAT = ',i3,1x,i2,1x,f8.5,1x,a6)
   19 format('    LON = ',i3,1x,i2,1x,f8.5,1x,a6)
c
c     write(*,*) '  '
c     write(*,*) '  First  Station : '
c     write(*,*) '  ---------------- '
c     write(*,18) ld1, lm1, sl1, d_ns1       
c     write(*,19) lod1,lom1,slo1,d_ew1
c
c     write(*,*) '  '
c     write(*,*) '  Second Station : '
c     write(*,*) '  ---------------- '
c     write(*,18) ld2, lm2, sl2, d_ns2
c     write(*,19) lod2,lom2,slo2,d_ew2
c
c
   32 format('  Ellipsoidal distance     S = ',f14.4,1x,a1)
   34 format('  Forward azimuth        FAZ = ',i3,1x,i2,1x,f7.4,
     1       ' From North')
   35 format('  Back azimuth           BAZ = ',i3,1x,i2,1x,f7.4,
     1       ' From North')
c
c     write(*,*) '  '
c     write(*,34) iaz1,maz1,saz1
c     write(*,35) iaz2,maz2,saz2
c     write(*,32) edist,dmt
c     write(*,*) '  '

c 998 format(f8.3,',',f7.3,',0.0')
c     write(4,998) lon2sn*(lod2+lom2/60.+slo2/3600.),
c    +lat2sn*(ld2+lm2/60.+sl2/3600.)

c     goto 999
c     write(*,*) '  Do you want to save this output into a file (y/n)?'
c     read(*,10) answer
c
c     if( answer.eq.'Y'.or.answer.eq.'y' )then
c  39   write(*,*) '  Enter the output filename : '
c       read(*,40) filout
c  40   format(a30)
c       open(3,file=filout,status='new',err=99)
c       goto 98
c
c  99   write(*,*) '  File already exists, try again.'
c       go to 39
c
c  98   continue
c       write(3,*) '  '
c       write(3,49) elips
c       write(3,50) a,b,finv
c       write(*,*) '  Enter the First Station name : '
c       read(*,40) name1
c       write(*,*) '  Enter the Second Station name : '
c       read(*,40) name2
c
   41   format('  First  Station : ',a30)
   42   format('  Second Station : ',a30)
   84   format('  Error:  First  Station ... Invalid Latitude  ')
   85   format('  Error:  First  Station ... Invalid Longitude ')
   86   format('  Error:  Forward Azimuth .. Invalid Entry     ')
   87   format('  Error:  Ellipsoid Distance .. Invalid Entry  ')
   88   format(1x,65(1h*))  
   91   format('          DD(0-89) MM(0-59) SS(0-59.999...)    ')
   92   format('          DDD(0-359) MM(0-59) SS(0-59.999...)  ')
   93   format('          Geodetic distance is too long        ')    
c
c       write(3,*) '  '
c       write(3,41) name1
c       write(3,*) '  ---------------- '
c
c       if( irlat1.eq.0 )then
c         write(3,18) ld1, lm1, sl1, d_ns1
c       else
c         write(3,88)
c         write(3,84)
c         write(3,91)                       
c         write(3,88)
c       endif
c 
c       if( irlon1.eq.0 )then      
c         write(3,19) lod1,lom1,slo1,d_ew1
c       else
c         write(3,88)
c         write(3,85)
c         write(3,92)                         
c         write(3,88)
c       endif
c
c       write(3,*) '  '
c       write(3,42) name2
c       write(3,*) '  ---------------- '
c       write(3,18) ld2, lm2, sl2, d_ns2
c       write(3,19) lod2,lom2,slo2,d_ew2
c
c       write(3,*) '  '
c       if( irazi1.eq.0 )then
c         write(3,34) iaz1,maz1,saz1
c       else
c         write(3,88)
c         write(3,86)
c         write(3,92)                
c         write(3,88)
c       endif
c
c       write(3,35) iaz2,maz2,saz2
c
c       if( irdst1.eq.0 )then
c         write(3,32) edist,dmt
c       else
c         write(3,88)
c         write(3,87)
c         write(3,93)            
c         write(3,88)
c       endif
c
c       write(3,*) '  '
c     endif
c
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,*) '  '
c     write(*,*) '  1) Another forward, different ellipsoid.'
c     write(*,*) '  2) Same ellipsoid, two new stations.'
c     write(*,*) '  3) Same ellipsoid, same First Station.'
c     write(*,*) '  4) Let the Second Station be the First Station.'
c     write(*,*) '  5) Done.'
c     write(*,*) '  '
c     write(*,*) '  Enter choice : '
c     read(*,10) answer
c
c     if(    answer.eq.'1')then
c       goto 1
c     elseif(answer.eq.'2')then
c       goto 15
c     elseif(answer.eq.'3')then
c       goto 20
c     elseif(answer.eq.'4')then
c       glat1 = glat2
c       glon1 = glon2
c       goto 20
c     else
c       write(*,*) '  Thats all, folks!'
c     endif
c
c     stop
      end

c      subroutine bufdms (buff,lgh,hem,dd,dm,ds,ierror)
c      implicit double precision (a-h, o-z)
c      implicit integer (i-n)
cc
c      logical     done,flag
cc
c      character*1 buff(*),abuf(21)
c      character*1 ch
c      character*1 hem
c      integer*4   ll,lgh
c      integer*4   i4,id,im,is,icond,ierror
c      real*8      x(5)
cc
cc     set the "error flag" 
cc
c      ierror = 0
c      icond  = 0
cc
cc     set defaults for dd,dm,ds
cc
c      dd = 0.0d0
c      dm = 0.0d0
c      ds = 0.0d0
cc
cc     set default limits for "hem" flag
cc
c      if(     hem.eq.'N' .or. hem.eq.'S' )then
c        ddmax = 90.0d0
c      elseif( hem.eq.'E' .or. hem.eq.'W' )then
c        ddmax = 360.0d0
c      elseif( hem.eq.'A' )then
c        ddmax = 360.0d0
c      elseif( hem.eq.'Z' )then
c        ddmax = 180.0d0
c      elseif( hem.eq.'*' )then
c        ddmax  = 0.0d0
c        ierror = 1
c      else
c        ddmax = 360.0d0
c      endif
cc
c      do 1 i=1,5
c        x(i) = 0.0d0
c    1 continue
cc
c      icolon = 0
c      ipoint = 0
c      icount = 0
c      flag   = .true.
c      jlgh   = lgh
cc
c      do 2 i=1,jlgh
c        if( buff(i).eq.':' )then
c          icolon = icolon+1
c        endif
c        if( buff(i).eq.'.' )then
c          ipoint = ipoint+1
c          flag   = .false.
c        endif
c        if( flag )then
c          icount = icount+1
c        endif
c    2 continue
cc
c      if( ipoint.eq.1 .and. icolon.eq.0 )then
cc
cc       load temp buffer
cc
c        do 3 i=1,jlgh
c          abuf(i) = buff(i)
c    3   continue
c        abuf(jlgh+1) = '$'
c        ll = jlgh
cc
c        call gvalr8 (abuf,ll,r8,icond)
cc
c        if( icount.ge.5 )then
cc
cc         value is a packed decimal of ==>  DDMMSS.sssss       
cc
c          ss = r8/10000.0d0
c          id = idint( ss )
cc
c          r8 = r8-10000.0d0*dble(float(id))
c          ss = r8/100.0d0
c          im = idint( ss )
cc
c          r8 = r8-100.0d0*dble(float(im))
c        else
cc
cc         value is a decimal of ==>  .xx   X.xxx   X.  
cc
c          id = idint( r8 )
c          r8 = (r8-id)*60.0d0
c          im = idint( r8 )
c          r8 = (r8-im)*60.0d0
c        endif
cc
cc       account for rounding error
cc
c        is = idnint( r8*1.0d5 )
c        if( is.ge.6000000 )then
c           r8 = 0.0d0
c           im = im+1
c        endif
cc
c        if( im.ge.60 )then
c          im = 0
c          id = id+1
c        endif
cc
c        dd = dble( float( id ) )
c        dm = dble( float( im ) )
c        ds = r8
c      else
cc
cc       buff() value is a d,m,s of ==>  NN:NN:XX.xxx    
cc
c        k    = 0
c        next = 1
c        done = .false.
c        ie   = jlgh
c
c        do 100 j=1,5
c          ib = next
c          do 90 i=ib,ie
c            ch   = buff(i)
c            last = i
c            if( i.eq.jlgh .or. ch.eq.':' )then
c              if( i.eq.jlgh )then
c                done = .true.
c              endif
c              if( ch.eq.':' )then
c                last = i-1
c              endif
c              goto 91
c            endif
c   90     continue
c          goto 98
cc
c   91     ipoint = 0
c          ik     = 0
c          do 92 i=next,last
c            ik = ik+1
c            ch = buff(i)
c            if( ch.eq.'.' )then
c              ipoint = ipoint+1
c            endif
c            abuf(ik) = buff(i) 
c   92     continue
c          abuf(ik+1) = '$' 
cc
c          ll = ik
c          if( ipoint.eq.0 )then
c            call gvali4 (abuf,ll,i4,icond)
c            r8 = dble(float( i4 )) 
c          else
c            call gvalr8 (abuf,ll,r8,icond)
c          endif
cc
c          k    = k+1
c          x(k) = r8
cc
c   98     if( done )then
c            goto 101
c          endif
cc
c          next = last
c   99     next = next+1     
c          if( buff(next).eq.':' )then
c            goto 99
c          endif
c  100   continue
cc
cc       load dd,dm,ds
cc
c  101   if( k.ge.1 )then
c          dd = x(1)
c        endif
cc
c        if( k.ge.2 )then
c          dm = x(2)
c        endif
cc
c        if( k.ge.3 )then
c          ds = x(3)
c        endif
c      endif
c
c      if( dd.gt.ddmax  .or.
c     1    dm.ge.60.0d0 .or.
c     1    ds.ge.60.0d0 )then
c        ierror = 1
c        dd = 0.0d0
c        dm = 0.0d0
c        ds = 0.0d0
c      endif
cc
c      if( icond.ne.0 )then
c        ierror = 1
c      endif
cc
c      return
c      end

      SUBROUTINE vincenty_dirct1(GLAT1,GLON1,GLAT2,GLON2,FAZ,BAZ,S)
c     SUBROUTINE DIRCT1(GLAT1,GLON1,GLAT2,GLON2,FAZ,BAZ,S)
C
C *** SOLUTION OF THE GEODETIC DIRECT PROBLEM AFTER T.VINCENTY
C *** MODIFIED RAINSFORD'S METHOD WITH HELMERT'S ELLIPTICAL TERMS
C *** EFFECTIVE IN ANY AZIMUTH AND AT ANY DISTANCE SHORT OF ANTIPODAL
C
C *** A IS THE SEMI-MAJOR AXIS OF THE REFERENCE ELLIPSOID
C *** F IS THE FLATTENING OF THE REFERENCE ELLIPSOID
C *** LATITUDES AND LONGITUDES IN RADIANS POSITIVE NORTH AND EAST
C *** AZIMUTHS IN RADIANS CLOCKWISE FROM NORTH
C *** GEODESIC DISTANCE S ASSUMED IN UNITS OF SEMI-MAJOR AXIS A
C
C *** PROGRAMMED FOR CDC-6600 BY LCDR L.PFEIFER NGS ROCKVILLE MD 20FEB75
C *** MODIFIED FOR SYSTEM 360 BY JOHN G GERGEN NGS ROCKVILLE MD 750608
C
      IMPLICIT REAL*8 (A-H,O-Z)
c     COMMON/CONST/PI,RAD
c     COMMON/ELIPSOID/A,F
      PI=4.d0*datan(1.d0)
      RAD=180.d0/PI
        A=6378137.d0
        F=1.d0/298.25722210088d0
      DATA EPS/0.5D-13/
      R=1.-F
      TU=R*DSIN(GLAT1)/DCOS(GLAT1)
      SF=DSIN(FAZ)
      CF=DCOS(FAZ)
      BAZ=0.
      IF(CF.NE.0.) BAZ=DATAN2(TU,CF)*2.
      CU=1./DSQRT(TU*TU+1.)
      SU=TU*CU
      SA=CU*SF
      C2A=-SA*SA+1.
      X=DSQRT((1./R/R-1.)*C2A+1.)+1.
      X=(X-2.)/X
      C=1.-X
      C=(X*X/4.+1)/C
      D=(0.375D0*X*X-1.)*X
      TU=S/R/A/C
      Y=TU
  100 SY=DSIN(Y)
      CY=DCOS(Y)
      CZ=DCOS(BAZ+Y)
      E=CZ*CZ*2.-1.
      C=Y
      X=E*CY
      Y=E+E-1.
      Y=(((SY*SY*4.-3.)*Y*CZ*D/6.+X)*D/4.-CZ)*SY*D+TU
      IF(DABS(Y-C).GT.EPS)GO TO 100
      BAZ=CU*CY*CF-SU*SY
      C=R*DSQRT(SA*SA+BAZ*BAZ)
      D=SU*CY+CU*SY*CF
      GLAT2=DATAN2(D,C)
      C=CU*CY-SU*SY*CF
      X=DATAN2(SY*SF,C)
      C=((-3.*C2A+4.)*F+4.)*C2A*F/16.
      D=((E*CY*C+CZ)*SY*C+Y)*SA
      GLON2=GLON1+X-(1.-C)*D*F
      BAZ=DATAN2(SA,BAZ)+PI
      RETURN
      END

c      subroutine elipss (elips)
c      implicit double precision(a-h,o-z)
c      double precision a,f
c      character*1  answer
c      character*30 elips
cc     common/elipsoid/a,f
c        a=6378137.d0
c        f=1.d0/298.25722210088d0
c      write(*,*) '  Other Ellipsoids.'
c      write(*,*) '  -----------------'
c      write(*,*) '  '
c      write(*,*) '  A) Airy 1858'
c      write(*,*) '  B) Airy Modified'
c      write(*,*) '  C) Australian National'
c      write(*,*) '  D) Bessel 1841'
c      write(*,*) '  E) Clarke 1880'
c      write(*,*) '  F) Everest 1830'
c      write(*,*) '  G) Everest Modified'
c      write(*,*) '  H) Fisher 1960'
c      write(*,*) '  I) Fisher 1968'
c      write(*,*) '  J) Hough 1956'
c      write(*,*) '  K) International (Hayford)'
c      write(*,*) '  L) Krassovsky 1938'
c      write(*,*) '  M) NWL-9D (WGS 66)'
c      write(*,*) '  N) South American 1969'
c      write(*,*) '  O) Soviet Geod. System 1985'
c      write(*,*) '  P) WGS 72'
c      write(*,*) '  Q-Z) User defined.'
c      write(*,*) '  '
c      write(*,*) '  Enter choice : '
c      read(*,10) answer
c   10 format(a1)
cc
c      if(answer.eq.'A'.or.answer.eq.'a') then
c        a=6377563.396d0
c        f=1.d0/299.3249646d0
c        elips='Airy 1858'
c      elseif(answer.eq.'B'.or.answer.eq.'b') then
c        a=6377340.189d0
c        f=1.d0/299.3249646d0
c        elips='Airy Modified'
c      elseif(answer.eq.'C'.or.answer.eq.'c') then
c        a=6378160.d0
c        f=1.d0/298.25d0
c        elips='Australian National'
c      elseif(answer.eq.'D'.or.answer.eq.'d') then
c        a=6377397.155d0
c        f=1.d0/299.1528128d0
c        elips='Bessel 1841'
c      elseif(answer.eq.'E'.or.answer.eq.'e') then
c        a=6378249.145d0
c        f=1.d0/293.465d0
c        elips='Clarke 1880'
c      elseif(answer.eq.'F'.or.answer.eq.'f') then
c        a=6377276.345d0
c        f=1.d0/300.8017d0
c        elips='Everest 1830'
c      elseif(answer.eq.'G'.or.answer.eq.'g') then
c        a=6377304.063d0
c        f=1.d0/300.8017d0
c        elips='Everest Modified'
c      elseif(answer.eq.'H'.or.answer.eq.'h') then
c        a=6378166.d0
c        f=1.d0/298.3d0
c        elips='Fisher 1960'
c      elseif(answer.eq.'I'.or.answer.eq.'i') then
c        a=6378150.d0
c        f=1.d0/298.3d0
c        elips='Fisher 1968'
c      elseif(answer.eq.'J'.or.answer.eq.'j') then
c        a=6378270.d0
c        f=1.d0/297.d0
c        elips='Hough 1956'
c      elseif(answer.eq.'K'.or.answer.eq.'k') then
c        a=6378388.d0
c        f=1.d0/297.d0
c        elips='International (Hayford)'
c      elseif(answer.eq.'L'.or.answer.eq.'l') then
c        a=6378245.d0
c        f=1.d0/298.3d0
c        elips='Krassovsky 1938'
c      elseif(answer.eq.'M'.or.answer.eq.'m') then
c        a=6378145.d0
c        f=1.d0/298.25d0
c        elips='NWL-9D  (WGS 66)'
c      elseif(answer.eq.'N'.or.answer.eq.'n') then
c        a=6378160.d0
c        f=1.d0/298.25d0
c        elips='South American 1969'
c      elseif(answer.eq.'O'.or.answer.eq.'o') then
c        a=6378136.d0
c        f=1.d0/298.257d0
c        elips='Soviet Geod. System 1985'
c      elseif(answer.eq.'P'.or.answer.eq.'p') then
c        a=6378135.d0
c        f=1.d0/298.26d0
c        elips='WGS 72'
c      else
c        elips='User defined.'
cc
c        write(*,*) '  Enter Equatorial axis,   a : '
c        read(*,*) a
c        a = dabs(a)
cc
c        write(*,*) '  Enter either Polar axis,   b  or '
c        write(*,*) '  Reciprocal flattening,   1/f : '
c        read(*,*) ss
c        ss = dabs(ss)
cc
c        f = 0.0d0
c        if( 200.0d0.le.ss .and. ss.le.310.0d0 )then
c          f = 1.d0/ss   
c        elseif( 6000000.0d0.lt.ss .and. ss.lt.a )then
c          f = (a-ss)/a
c        else
c          elips = 'Error: default GRS80 used.'
c          a     = 6378137.0d0
c          f     = 1.0d0/298.25722210088d0
c        endif
c      endif
cc
c      return
c      end

      subroutine vincenty_fixdms (ideg, min, sec, tol )
c     subroutine fixdms (ideg, min, sec, tol )
c
      implicit double precision (a-h, o-z)
      implicit integer (i-n)
c
c     test for seconds near 60.0-tol
c
      if( sec.ge.( 60.0d0-tol ) )then
        sec  = 0.0d0
        min  = min+1
      endif
c
c     test for minutes near 60
c
      if( min.ge.60 )then
        min  = 0
        ideg = ideg+1
      endif 
c
c     test for degrees near 360
c
      if( ideg.ge.360 )then
        ideg = 0
      endif 
c
      return
      end 

c      subroutine hem_ns ( lat_sn, hem )
c      implicit integer (i-n)
c      character*6  hem
cc
c      if( lat_sn.eq.1 ) then
c        hem = 'North '
c      else
c        hem = 'South '
c      endif
cc
c      return
c      end
c      subroutine hem_ew ( lon_sn, hem )
c      implicit integer (i-n)
c      character*6  hem
cc
cc      if( lon_sn.eq.1 ) then
c        hem = 'East  '
c      else
c        hem = 'West  '
c      endif
cc
c      return
c      end

      subroutine vincenty_getrad(d,m,sec,isign,val)
 
*** comvert deg, min, sec to radians
 
      implicit double precision(a-h,j-z)
      double precision pi,rad
c     common/const/pi,rad
      pi=4.d0*datan(1.d0)
      rad=180.d0/pi
 
      val=(d+m/60.d0+sec/3600.d0)/rad
      val=dble(isign)*val
 
      return
      end

CB::GPNARC
C
      SUBROUTINE vincenty_gpnarc (AMAX,FLAT,ESQ,PI,P1,P2,ARC)
c     SUBROUTINE GPNARC (AMAX,FLAT,ESQ,PI,P1,P2,ARC)
C
C********1*********2*********3*********4*********5*********6*********7*
C
C NAME:        GPNARC
C VERSION:     200005.26
C WRITTEN BY:  ROBERT (Sid) SAFFORD
C PURPOSE:     SUBROUTINE TO COMPUTE THE LENGTH OF A MERIDIONAL ARC 
C              BETWEEN TWO LATITUDES
C
C INPUT PARAMETERS:
C -----------------
C AMAX         SEMI-MAJOR AXIS OF REFERENCE ELLIPSOID
C FLAT         FLATTENING (0.0033528 ... )
C ESQ          ECCENTRICITY SQUARED FOR REFERENCE ELLIPSOID
C PI           3.14159...
C P1           LAT STATION 1
C P2           LAT STATION 2
C
C OUTPUT PARAMETERS:
C ------------------
C ARC          GEODETIC DISTANCE 
C
C LOCAL VARIABLES AND CONSTANTS:
C ------------------------------
C GLOBAL VARIABLES AND CONSTANTS:
C -------------------------------
C
C    MODULE CALLED BY:    GENERAL 
C
C    THIS MODULE CALLS:   
C       LLIBFORE/ OPEN,   CLOSE,  READ,   WRITE,  INQUIRE
C                 DABS,   DBLE,   FLOAT,  IABS,   CHAR,   ICHAR
C
C    INCLUDE FILES USED:
C    COMMON BLOCKS USED:  
C
C    REFERENCES: Microsoft FORTRAN 4.10 Optimizing Compiler, 1988
C                MS-DOS Operating System
C    COMMENTS:
C********1*********2*********3*********4*********5*********6*********7*
C::MODIFICATION HISTORY
C::197507.05, RWS, VER 00 TENCOL RELEASED FOR FIELD USE
C::198311.20, RWS, VER 01 MTEN   RELEASED TO FIELD
C::198411.26, RWS, VER 07 MTEN2  RELEASED TO FIELD
C::1985xx.xx, RWS, CODE   CREATED               
C::198506.10, RWS, WRK    ENHANCEMENTS RELEASED TO FIELD
C::198509.01, RWS, VER 11 MTEN3  RELEASED TO FIELD
C::198512.18, RWS, CODE   MODIFIED FOR MTEN3
C::198708.10, RWS, CODE   MODIFIED TO USE NEW MTEN4 GPN RECORD FORMAT
C::199112.31, RWS, VER 20 MTEN4 RELEASED TO FIELD
C::200001.13, RWS, VER 21 MTEN4 RELEASED TO FIELD
C::200005.26, RWS, CODE   RESTRUCTURED & DOCUMENTATION ADDED             
C::200012.31, RWS, VER 23 MTEN5 RELEASED                                 
C********1*********2*********3*********4*********5*********6*********7*
CE::GPNARC
C ---------------------------
C     M T E N  (VERSION 3)
C     M T E N  (VERSION 5.23)
C ---------------------------
C 
      IMPLICIT REAL*8 (A-H,O-Z)
C
      LOGICAL  FLAG
C
      DATA TT/5.0D-15/
C
C     CHECK FOR A 90 DEGREE LOOKUP
C
      FLAG = .FALSE.
C
      S1 = DABS(P1)
      S2 = DABS(P2)
C
      IF( (PI/2.0D0-TT).LT.S2 .AND. S2.LT.(PI/2.0D0+TT) )THEN
        FLAG = .TRUE.
      ENDIF
C
      IF( S1.GT.TT )THEN
        FLAG = .FALSE.
      ENDIF
C
      DA = (P2-P1)
      S1 = 0.0D0
      S2 = 0.0D0
C
C     COMPUTE THE LENGTH OF A MERIDIONAL ARC BETWEEN TWO LATITUDES
C
      E2 = ESQ
      E4 = E2*E2
      E6 = E4*E2
      E8 = E6*E2
      EX = E8*E2
C
      T1 = E2*(003.0D0/4.0D0)
      T2 = E4*(015.0D0/64.0D0)
      T3 = E6*(035.0D0/512.0D0)
      T4 = E8*(315.0D0/16384.0D0)
      T5 = EX*(693.0D0/131072.0D0)
C
      A  = 1.0D0+T1+3.0D0*T2+10.0D0*T3+35.0D0*T4+126.0D0*T5
C
      IF( FLAG )THEN
        GOTO 1
      ENDIF
C
      B  = T1+4.0D0*T2+15.0D0*T3+56.0D0*T4+210.0D0*T5
      C  = T2+06.0D0*T3+28.0D0*T4+120.0D0*T5
      D  = T3+08.0D0*T4+045.0D0*T5
      E  = T4+010.0D0*T5
      F  = T5
C
      DB = DSIN(P2*2.0D0)-DSIN(P1*2.0D0)
      DC = DSIN(P2*4.0D0)-DSIN(P1*4.0D0)
      DD = DSIN(P2*6.0D0)-DSIN(P1*6.0D0)
      DE = DSIN(P2*8.0D0)-DSIN(P1*8.0D0)
      DF = DSIN(P2*10.0D0)-DSIN(P1*10.0D0)
C
C     COMPUTE THE S2 PART OF THE SERIES EXPANSION
C
      S2 = -DB*B/2.0D0+DC*C/4.0D0-DD*D/6.0D0+DE*E/8.0D0-DF*F/10.0D0
C
C     COMPUTE THE S1 PART OF THE SERIES EXPANSION
C
    1 S1 = DA*A
C
C     COMPUTE THE ARC LENGTH
C
      ARC = AMAX*(1.0D0-ESQ)*(S1+S2)
C
      RETURN
      END

c      subroutine gvali4 (buff,ll,vali4,icond)
c      implicit     integer (i-n)
cc
c      logical      plus,sign,done,error
c      character*1  buff(*)
c      character*1  ch
cc
cc     integer*2    i
cc     integer*2    l1
cc
c      integer*4    ich,icond
c      integer*4    ll    
c      integer*4    vali4
cc
c      l1    = ll
c      vali4 = 0
c      icond = 0
c      plus  = .true.
c      sign  = .false.
c      done  = .false.
c      error = .false.
cc
c      i = 0
c   10 i = i+1
c      if( i.gt.l1 .or. done )then
c        go to 1000
c      else
c        ch  = buff(i)
c        ich = ichar( buff(i) )
c      endif
cc
c      if(     ch.eq.'+' )then
cc
cc       enter on plus sign
cc
c        if( sign )then
c          goto 150
c        else 
c          sign = .true.
c          goto 10
c        endif
c      elseif( ch.eq.'-' )then
cc
cc       enter on minus sign
cc
c        if( sign )then
c          goto 150
c        else
c          sign = .true.
c          plus = .false.
c          goto 10
c        endif
c      elseif( ch.ge.'0' .and. ch.le.'9' )then
c        goto 100
c      elseif( ch.eq.' ' )then
cc
cc       enter on space -- ignore leading spaces
cc
c        if( .not.sign )then
c          goto 10
c        else
c          buff(i) = '0'
c          ich = 48
c          goto 100
c        endif
c      elseif( ch.eq.':' )then
cc
cc       enter on colon -- ignore 
cc
c        if( .not.sign )then
c          goto 10
c        else
c          goto 1000
c        endif
c      elseif( ch.eq.'$' )then
cc
cc       enter on dollar "$"      
cc
c        done = .true.
c        goto 10
c      else
cc
cc       something wrong
cc
c        goto 150
c      endif
cc
cc     enter on numeric
cc
c  100 vali4 = 10*vali4+(ich-48)
c      sign  = .true.
c      goto 10
cc
cc     treat illegal character
cc
cc  150 buff(i) = '0'
c      vali4 = 0
c      icond = 1
cc
c 1000 if( .not.plus )then
c        vali4 = -vali4
c      endif
cc
c      return
c      end
c      subroutine gvalr8 (buff,ll,valr8,icond)
c      implicit     integer (i-n)
cc
c      logical      plus,sign,dpoint,done
cc
c      character*1  buff(*)
c      character*1  ch
cc
cc     integer*2    i, ip
cc     integer*2    l1
cc     integer*2    nn, num, n48
cc
c      integer*4    ich,icond
c      integer*4    ll
cc
c      real*8       ten
c      real*8       valr8
c      real*8       zero
cc
c      data zero,ten/0.0d0,10.0d0/
cc
c      n48     =  48
c      l1      =  ll
c      icond   =   0
c      valr8   =  zero  
c      plus    = .true.
c      sign    = .false.
c      dpoint  = .false.
c      done    = .false.
cc
cc     start loop thru buffer
cc
c      i = 0
c   10 i = i+1
c      if( i.gt.l1 .or. done )then
c        go to 1000
c      else 
c        ch  = buff(i)
c        nn  = ichar( ch )
c        ich = nn
c      endif 
cc
c      if(     ch.eq.'+' )then
cc
cc       enter on plus sign
cc
c        if( sign )then
c          goto 150
c        else
c          sign = .true.
c          goto 10
c        endif
c      elseif( ch.eq.'-' )then
cc
cc       enter on minus sign
cc
c        if( sign )then
c          goto 150
c        else
c          sign = .true.
c          plus = .false.
c          goto 10
c        endif
c      elseif( ch.eq.'.' )then
cc
cc       enter on decimal point
cc
c        ip     = 0
c        sign   = .true.
c        dpoint = .true.
c        goto 10
c      elseif( ch.ge.'0' .and. ch.le.'9' )then
c        goto 100
c      elseif( ch.eq.' ' )then
cc
cc       enter on space
cc
c        if( .not.sign )then
c          goto 10
c        else
c          buff(i) = '0'
c          ich = 48
c          goto 100
c        endif
c      elseif( ch.eq.':' .or. ch.eq.'$' )then
cc
cc       enter on colon or "$" sign
cc
c        done = .true.
c        goto 10
c      else
cc
cc       something wrong
cc
c        goto 150
c      endif
cc
cc     enter on numeric
cc
c  100 sign = .true.
c      if( dpoint )then
c        ip = ip+1
c      endif
cc
c      num   = ich
c      valr8 = ten*valr8+dble(float( num-n48 ))
c      goto 10
cc
cc     treat illegal character
cc
c  150 buff(i) = '0'
c      valr8   =  0.0d0
c      icond   =  1
cc
c 1000 if( dpoint )then
c        valr8 =  valr8/(ten**ip)
c      endif
cc
c      if( .not.plus )then
c        valr8 = -valr8
c      endif
cc
c      return
c      end
c
      subroutine vincenty_todmsp(val,id,im,s,isign)
c     subroutine todmsp(val,id,im,s,isign)
 
*** convert position radians to deg,min,sec
*** range is [-pi to +pi]
 
      implicit double precision(a-h,o-z)
      double precision pi,rad
c     common/const/pi,rad
      pi=4.d0*datan(1.d0)
      rad=180.d0/pi
 
    1 if(val.gt.pi) then
        val=val-pi-pi
        go to 1
      endif
 
    2 if(val.lt.-pi) then
        val=val+pi+pi
        go to 2
      endif
 
      if(val.lt.0.d0) then
        isign=-1
      else
        isign=+1
      endif
 
      s=dabs(val*rad)
      id=idint(s)
      s=(s-id)*60.d0
      im=idint(s)
      s=(s-im)*60.d0
 
*** account for rounding error
 
      is=idnint(s*1.d5)
      if(is.ge.6000000) then
        s=0.d0
        im=im+1
      endif
      if(im.ge.60) then
        im=0
        id=id+1
      endif
 
      return
      end

c      subroutine trim (buff,lgh,hem)
c      implicit integer (i-n)
c      character*1 ch,hem
c      character*1 buff(*)
c      integer*4   lgh
cc
c      ibeg = 1
c      do 10 i=1,50
c        if( buff(i).ne.' ' )then
c          goto 11
c        endif
c        ibeg = ibeg+1
c   10 continue
c   11 continue
c      if( ibeg.ge.50 )then
c        ibeg = 1
c        buff(ibeg) = '0'
c      endif
cc
c      iend = 50
c      do 20 i=1,50
c        j = 51-i
c        if( buff(j).eq.' ' )then
c          iend = iend-1
c        else
c          goto 21
c        endif
c   20 continue
c   21 continue
cc
c      ch = buff(ibeg)
c      if( hem.eq.'N' )then
c        if( ch.eq.'N' .or. ch.eq.'n' .or. ch.eq.'+' )then
c          hem = 'N'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'S' .or. ch.eq.'s' .or. ch.eq.'-' )then
c          hem = 'S'
c          ibeg = ibeg+1
c        endif
cc
cc       check for wrong hemisphere entry
cc
c        if( ch.eq.'E' .or. ch.eq.'e' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'W' .or. ch.eq.'w' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c      elseif( hem.eq.'W' )then
c        if( ch.eq.'E' .or. ch.eq.'e' .or. ch.eq.'+' )then
c          hem = 'E'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'W' .or. ch.eq.'w' .or. ch.eq.'-' )then
c          hem = 'W'
c          ibeg = ibeg+1
c        endif
cc
cc       check for wrong hemisphere entry
cc
c        if( ch.eq.'N' .or. ch.eq.'n' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'S' .or. ch.eq.'s' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c      elseif( hem.eq.'A' )then
c        if( .not.('0'.le.ch .and. ch.le.'9') )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c      else
cc        do nothing
c      endif
cc
cc
c      do 30 i=ibeg,iend
c        ch = buff(i)
cc
c        if(     ch.eq.':' .or. ch.eq.'.' )then
c          goto 30
c        elseif( ch.eq.' ' .or. ch.eq.',' )then
c          buff(i) = ':'
c        elseif( '0'.le.ch .and. ch.le.'9' )then
c          goto 30      
c        else
c          buff(i) = ':'
c        endif
cc
c   30 continue
cc
cc     left justify buff() array to its first character position
cc     also check for a ":" char in the starting position,
cc     if found!!  skip it
cc
c      j  = 0
c      ib = ibeg
c      ie = iend
cc
c      do 40 i=ib,ie
c        if( i.eq.ibeg .and. buff(i).eq.':' )then
cc
cc         move the 1st position pointer to the next char &
cc         do not put ":" char in buff(j) array where j=1    
cc
c          ibeg = ibeg+1
c          goto 40
c        endif
c        j = j+1
c        buff(j) = buff(i)
c   40 continue
cc
cc
c      lgh = iend-ibeg+1
c      j   = lgh+1
c      buff(j) = '$'
cc
cc     clean-up the rest of the buff() array
cc
c      do 50 i=j+1,50   
c        buff(i) = ' '    
c   50 continue
cc
cc     save a maximum of 20 characters
cc
c      if( lgh.gt.20 )then
c        lgh = 20
c        j   = lgh+1
c        buff(j) = '$'
c      endif
cc
c      return
c      end
c
c      real*8 function zvalue ( ss, tol )
cc
c      implicit double precision (a-z)
cc
cc     to check for dx,dy,dz or 
cc     dn,de,du values below tol.
cc
c      if( dabs(ss).lt.tol )then
c        ss = 0.0d0
c      endif
cc
c      zvalue = ss
cc
c      return
c      end
c
      subroutine vincenty_inverse(lon1,lat1,lon2,lat2)
ccb::inverse
cc
c      program inverse
cc
cc********1*********2*********3*********4*********5*********6*********7**
cc
cc name:      inverse
cc version:   201105.xx
cc author:    stephen j. frakes
cc last mod:  dr. dennis milbert
cc purpose:   to compute a geodetic inverse  
cc            and then display output information
cc
cc input parameters:
cc -----------------
cc
cc output parameters:
cc ------------------
cc
cc local variables and constants:
cc ------------------------------
cc answer           user prompt response
cc b                semiminor axis polar (in meters)
cc baz              azimuth back (in radians)
cc buff             input char buffer array
cc dd,dm,ds         temporary values for degrees, minutes, seconds
cc dlon             temporary value for difference in longitude (radians)   
cc dmt,d_mt         char constants for meter units         
cc edist            ellipsoid distance (in meters)
cc elips            ellipsoid choice
cc esq              eccentricity squared for reference ellipsoid
cc faz              azimuth forward (in radians)
cc filout           output file name
cc finv             reciprocal flattening
cc hem              hemisphere flag for lat & lon entry  
cc ierror           error condition flag with d,m,s conversion
cc lgh              length of buff() array
cc option           user prompt response             
cc r1,r2            temporary variables    
cc ss               temporary variable     
cc tol              tolerance for conversion of seconds
cc
cc name1            name of station one
cc ld1,lm1,sl1      latitude  sta one - degrees,minutes,seconds
cc ald1,alm1,sl1    latitude  sta one - degrees,minutes,seconds
cc lat1sn           latitude  sta one - sign (+/- 1)
cc d_ns1            latitude  sta one - char ('N','S')
cc lod1,lom1,slo1   longitude sta one - degrees,minutes,seconds
cc alod1,alom1,slo1 longitude sta one - degrees,minutes,seconds
cc lon1sn           longitude sta one - sign (+/- 1)
cc d_ew1            longitude sta one - char ('E','W')
cc iaz1,maz1,saz1   forward azimuth   - degrees,minutes,seconds
cc isign1           forward azimuth   - flag  (+/- 1)
cc glat1,glon1      station one       - (lat & lon in radians )
cc p1,e1            standpoint one    - (lat & lon in radians )
cc
cc name2            name of station two
cc ld2,lm2,sl2      latitude  sta two - degrees,minutes,seconds
cc ald2,alm2,sl2    latitude  sta two - degrees,minutes,seconds
cc lat2sn           latitude  sta two - sign (+/- 1)
cc d_ns2            latitude  sta two - char ('N','S')
cc lod2,lom2,slo2   longitude sta two - degrees,minutes,seconds
cc alod2,alom2,slo2 longitude sta one - degrees,minutes,seconds
cc lon2sn           longitude sta two - sign (+/- 1)
cc d_ew2            longitude sta two - char ('E','W')
cc iaz2,maz2,saz2   back azimuth      - degrees,minutes,seconds
cc isign2           back azimuth      - flag  (+/- 1)
cc glat2,glon2      station two       - (lat & lon in radians )
cc p2,e2            forepoint two     - (lat & lon in radians )
cc
cc global variables and constants:
cc -------------------------------
cc a                semimajor axis equatorial (in meters)
cc f                flattening
cc pi               constant 3.14159....
cc rad              constant 180.0/pi  
cc
cc    this module called by:  n/a
cc
cc    this module calls:      elipss, getrad, inver1, todmsp
cc    gethem, trim,   bufdms, gvalr8, gvali4, fixdms, gpnhri ***********
cc    gethem, trim,   bufdms, gvalr8, gvali4, fixdms, invers <----------
cc    datan,  write,  read,   dabs,   open,   stop
cc
cc    include files used:     n/a
cc
cc    common blocks used:     const, elipsoid
cc
cc    references:             see comments within subroutines
cc
cc    comments:
cc
cc********1*********2*********3*********4*********5*********6*********7**
cc::modification history
cc::1990mm.dd, sjf, creation of program           
cc::199412.15, bmt, creation of program on viper
cc::200203.08, crs, modified by c.schwarz to correct spelling of Clarke
cc::200207.15, rws, modified i/o & standardized program documentation
cc::                added subs trim, bufdms, gethem, gvali4, gvalr8      
cc::200207.23, rws, replaced sub inver1 with gpnarc, gpnloa, gpnhri
cc::200208.15, rws, fixed an error in bufdms
cc::              - renamed ellips to elipss "common error" with dirct2
cc::              - added FAZ & BAZ to printed output      
cc::200208.19, rws, added more error flags for web interface code
cc::              - added logical nowebb                             
cc::200208.xx, sjf, program version number 2.0                   
cc::201105.xx, dgm, program version number 3.0
cc::              - replaced sub gpnarc, gpnloa, gpnhri with invers
cc::              - tested for valid antipodal solutions (+/- 0.1 mm)
cc::              - tested for polar solutions (+/- 0.1 mm)
cc::              - needs improvement for long-line/antipodal boundary
cc********1*********2*********3*********4*********5*********6*********7**
cce::inverse
cc
      implicit double precision (a-h, o-z)
      implicit integer (i-n)
      double precision lon1,lat1,lon2,lat2
      double precision pi,rad
      double precision a,f
cc
      logical  nowebb
cc
      character*1  answer,option,dmt,buff(50),hem
      character*6  d_ns1, d_ew1, d_ns2, d_ew2, d_mt
      character*30 filout,name1,name2,elips
cc
      integer*4    ierror
      integer*4    lgh
cc
c      common/const/pi,rad
c      common/elipsoid/a,f
cc
cc     ms_unix      0 = web version
cc                  1 = ms_dos or unix version
cc
      parameter   ( ms_unix = 0 )
cc
      pi   = 4.d0*datan(1.d0)
      rad  = 180.d0/pi
      dmt  = 'm'
      d_mt = 'Meters'
cc
c      if( ms_unix.eq.1 )then
c        nowebb = .true. 
c      else
        nowebb = .false.
c      endif
cc
c    1 do 2 i=1,25
c        write(*,*) '  '
c    2 continue
cc 
c    5 write(*,*) '  Program Inverse  -  Version 3.0 '
c      write(*,*) '  '
c      write(*,*) '  Ellipsoid options : '
c      write(*,*) '  '
c      write(*,*) '  1) GRS80 / WGS84  (NAD83) '
c      write(*,*) '  2) Clarke 1866    (NAD27) '
c      write(*,*) '  3) Any other ellipsoid '
c      write(*,*) '  '
c      write(*,*) '  Enter choice : '
c      read(*,10) option
c   10 format(a1)
cc
c      if(option.eq.'1') then
        a=6378137.d0
        f=1.d0/298.257222100882711243162836600094d0
        elips='GRS80 / WGS84  (NAD83)'
c      elseif(option.eq.'2') then
c        a=6378206.4d0
c        f=1.d0/294.9786982138d0
c        elips='Clarke 1866    (NAD27)'
c      elseif(option.eq.'3') then
c        call elipss (elips)
c      else
c        write(*,*) '  Enter 1, 2, or 3 !   Try again --'
c        goto 5
c      endif
cc
      esq = f*(2.0d0-f)
cc
c      write(*,*) '  '
c      write(*,*) '  '
c      write(*,*) '  '
c      write(*,*) '  '
cc
c   15 write(*,*) '  Enter First Station '
c      write(*,16)
c   16 format(18x,'(Separate D,M,S by blanks or commas)')
c      write(*,*) 'hDD MM SS.sssss  Latitude :        (h default = N )'
c   11 format(50a1)
cc
c   22 hem='N'
c      read(*,11) buff
c      call trim (buff,lgh,hem)
c      call bufdms (buff,lgh,hem,dd,dm,ds,ierror)
cc
c      if( ierror.eq.0 )then
        irlat1 = 0
c      else
c        irlat1 = 1
c        write(*,*) ' Invalid Latitude ... Please re-enter it '
c        write(*,*) '  '
c        if( nowebb )then
c          goto 22
c        endif
c      endif
cc
      if(lat1.gt.0.0D0+00)then
        hem='N'
      else
        hem='S'
        lat1=DABS(lat1)
      endif

c     read(*,11) buff
c     call trim (buff,lgh,hem)
c     call bufdms (buff,lgh,hem,dd,dm,ds,ierror)
      ierror=0
c     write(*,*) 'lat1',lat1
      dd=DINT(lat1)
c     write(*,*) 'dd',dd
      dm=DINT((lat1-dd)*60.0D0+00)
c     write(*,*) 'dm',dm
      ds=(((lat1-dd)*60.0D0+00)-dm)*60.0D0+00
      ald1 = dd
      alm1 = dm
      sl1  = ds
cc
       if( hem.eq.'N' ) then
         lat1sn = +1
       else
         lat1sn = -1
       endif
cc
c      write(*,*) 'hDDD MM SS.sssss Longitude :       (h default = W )'
cc
c   23 hem='W'
c      read(*,11) buff
c      call trim (buff,lgh,hem)
c      call bufdms (buff,lgh,hem,dd,dm,ds,ierror)
cc
c      if( ierror.eq.0 )then
        irlon1 = 0
c      else
c        irlon1 = 1
c        write(*,*) ' Invalid Longitude ... Please re-enter it '
c        write(*,*) '  '
c        if( nowebb )then
c          goto 23
c        endif
c      endif
c
      if(lon1.gt.0.0D0+00)then
        hem='E'
      else
        hem='W'
        lon1=DABS(lon1)
      endif

c     write(*,*) 'lon1',lon1
      dd=DINT(lon1)
c     write(*,*) 'dd',dd
      dm=DINT((lon1-dd)*60.0D0+00)
c     write(*,*) 'dm',dm
      ds=(((lon1-dd)*60.0D0+00)-dm)*60.0D0+00
cc
      alod1 = dd
      alom1 = dm
      slo1  = ds
c
      if( hem.eq.'E' ) then
        lon1sn = +1
      else
        lon1sn = -1
      endif
c
      call vincenty_getrad(ald1, alm1, sl1, lat1sn, glat1)
      call vincenty_getrad(alod1,alom1,slo1,lon1sn, glon1)
c
c      write(*,*) '  '
c      write(*,*) '  '
cc
c   20 write(*,*) '  Enter Second Station '
c      write(*,16)
c      write(*,*) 'hDD MM SS.sssss  Latitude :        (h default = N )'
cc
c   24 hem='N'
c      read(*,11) buff
c      call trim (buff,lgh,hem)
c      call bufdms (buff,lgh,hem,dd,dm,ds,ierror)
cc
c      if( ierror.eq.0 )then
        irlat2 = 0
c      else
c        irlat2 = 1
c        write(*,*) ' Invalid Latitude ... Please re-enter it '
c        write(*,*) '  '
c        if( nowebb )then
c          goto 24
c        endif
c      endif
cc
      if(lat2.gt.0.0D0+00)then
        hem='N'
      else
        hem='S'
        lat2=DABS(lat2)
      endif

c     write(*,*) 'lat2',lat2
      dd=DINT(lat2)
c     write(*,*) 'dd',dd
      dm=DINT((lat2-dd)*60.0D0+00)
c     write(*,*) 'dm',dm
      ds=(((lat2-dd)*60.0D0+00)-dm)*60.0D0+00
      ald2 = dd
      alm2 = dm
      sl2  = ds
cc
      if( hem.eq.'N' ) then
        lat2sn = +1
      else
        lat2sn = -1
      endif
cc
c      write(*,*) 'hDDD MM SS.sssss Longitude :       (h default = W )'
cc
c   25 hem='W'
c      read(*,11) buff
c      call trim (buff,lgh,hem)
c      call bufdms (buff,lgh,hem,dd,dm,ds,ierror)
cc
c      if( ierror.eq.0 )then
        irlon2 = 0
c      else
c        irlon2 = 1
c        write(*,*) ' Invalid Longitude ... Please re-enter it '
c        write(*,*) '  '
c        if( nowebb )then
c          goto 25
c        endif
c      endif
cc
      if(lon2.gt.0.0D0+00)then
        hem='E'
      else
        hem='W'
        lon2=DABS(lon2)
      endif

c     write(*,*) 'lon2',lon2
      dd=DINT(lon2)
c     write(*,*) 'dd',dd
      dm=DINT((lon2-dd)*60.0D0+00)
c     write(*,*) 'dm',dm
      ds=(((lon2-dd)*60.0D0+00)-dm)*60.0D0+00
c     write(*,*) 'ds',ds
c
      alod2 = dd
      alom2 = dm
      slo2  = ds

      if( hem.eq.'E' )then
        lon2sn = +1
      else
        lon2sn = -1
      endif
cc
      call vincenty_getrad(ald2, alm2, sl2, lat2sn, glat2)
      call vincenty_getrad(alod2,alom2,slo2,lon2sn, glon2)
cc
      p1 = glat1
      e1 = glon1
      p2 = glat2
      e2 = glon2
c
      if( e1.lt.0.0d0 )then
        e1 = e1+2.0d0*pi
      endif
      if( e2.lt.0.0d0 )then
        e2 = e2+2.0d0*pi
      endif
c
c     compute the geodetic inverse
c
c ************************************************************
c *   replaced subroutine inver1 with gpnhri
c *  
c *   call inver1 (glat1,glon1,glat2,glon2,faz,baz,edist)
c *
c ************************************************************
c ************************************************************
c *   replaced subroutine inver1 with gpnhri with invers -- dgm
c *  
c *   call gpnhri (a,f,esq,pi,p1,e1,p2,e2,faz,baz,edist)
c *
c ************************************************************

      finv=1.d0/f
      call vincenty_invers(a,finv,p1,e1,p2,e2,faz,baz,edist,
     *            numiter,sigmaout,dlambdaout,iflagout)

c     check for a non distance ... p1,e1 & p2,e2 equal zero ?
c
      if( edist.lt.0.00005d0 )then
        faz = 0.0d0
        baz = 0.0d0
      endif
c
c     set the tolerance (in seconds) for the azimuth conversion
c
      tol = 0.00005d0

      call vincenty_todmsp(faz,iaz1,maz1,saz1,isign1)
      if(isign1.lt.0) then
        iaz1=359-iaz1
        maz1=59-maz1
        saz1=60.d0-saz1
      endif
      call vincenty_fixdms ( iaz1, maz1, saz1, tol )
c
      call vincenty_todmsp(baz,iaz2,maz2,saz2,isign2)
      if(isign2.lt.0) then
        iaz2=359-iaz2
        maz2=59-maz2
        saz2=60.d0-saz2
      endif
      call vincenty_fixdms ( iaz2, maz2, saz2, tol ) 

      call vincenty_todmsp(glat1, ld1,  lm1,  sl1,  lat1sn)
      call vincenty_todmsp(glon1, lod1, lom1, slo1, lon1sn)
      call vincenty_todmsp(glat2, ld2,  lm2,  sl2,  lat2sn)
      call vincenty_todmsp(glon2, lod2, lom2, slo2, lon2sn)
cc
c      call hem_ns ( lat1sn, d_ns1 )
c      call hem_ew ( lon1sn, d_ew1 )
c      call hem_ns ( lat2sn, d_ns2 )
c      call hem_ew ( lon2sn, d_ew2 )
cc 
c      write(*,*) '  '
c      write(*,*) '  '
c      write(*,*) '  '
c      write(*,*) '  '
c      write(*,*) '  '
c      write(*,49) elips
c   49 format('  Ellipsoid : ',a30)
c      finv=1.d0/f
c      b=a*(1.d0-f)
c      write(*,50) a,b,finv
c   50 format('  Equatorial axis,    a   = ',f15.4,/,
c     *       '  Polar axis,         b   = ',f15.4,/,
c     *       '  Inverse flattening, 1/f = ',f16.11)
cc
   18 format('    LAT = ',i3,1x,i2,1x,f8.5,1x,a6)
   19 format('    LON = ',i3,1x,i2,1x,f8.5,1x,a6)
c
c      write(*,*) '  '
c     write(*,*) '  First  Station : '
c     write(*,*) '  ---------------- '
c     write(*,18) ld1, lm1, sl1, d_ns1       
c     write(*,19) lod1,lom1,slo1,d_ew1
cc
c      write(*,*) '  '
c     write(*,*) '  Second Station : '
c     write(*,*) '  ---------------- '
c     write(*,18) ld2, lm2, sl2, d_ns2
c     write(*,19) lod2,lom2,slo2,d_ew2
cc
   32 format('  Ellipsoidal distance     S = ',f14.4,1x,a1)
   34 format('  Forward azimuth        FAZ = ',i3,1x,i2,1x,f7.4,
     1       ' From North')
   35 format('  Back azimuth           BAZ = ',i3,1x,i2,1x,f7.4,
     1       ' From North')
cc
c      write(*,*) '  '
c     write(*,34) iaz1,maz1,saz1
c     write(*,35) iaz2,maz2,saz2
c     write(*,32) edist,dmt
      lon1=iaz1+maz1/60.0D0+saz1/3600.0D0
      lat1=iaz2+maz2/60.0D0+saz2/3600.0D0
      lon2=edist
c      write(*,*) '  '
c      write(*,*) '  Do you want to save this output into a file (y/n)?'
c      read(*,10) answer
cc
c      if( answer.eq.'Y'.or.answer.eq.'y' )then
c   39   write(*,*) '  Enter the output filename : '
c        read(*,40) filout
c   40   format(a30)
c        open(3,file=filout,status='new',err=99)
c        goto 98
cc
c   99   write(*,*) '  File already exists, try again.'
c        go to 39
cc
c   98   continue
c        write(3,*) '  '
c        write(3,49) elips
c        finv=1.d0/f
c        b=a*(1.d0-f)
c        write(3,50) a,b,finv
c        write(*,*) '  Enter the First Station name : '
c        read(*,40) name1
c        write(*,*) '  Enter the Second Station name : '
c        read(*,40) name2
cc
c   41   format('  First  Station : ',a30)
c   42   format('  Second Station : ',a30)
c   84   format('  Error: First  Station ... Invalid Latitude  ')
c   85   format('  Error: First  Station ... Invalid Longitude ')
c   86   format('  Error: Second Station ... Invalid Latitude  ')
c   87   format('  Error: Second Station ... Invalid Longitude ')
c   88   format(1x,65(1h*))
c   91   format('         DD(0-89) MM(0-59) SS(0-59.999...)  ')
c   92   format('         DDD(0-359) MM(0-59) SS(0-59.999...)  ')
cc
c        write(3,*) '  '
c        write(3,41) name1
c        write(3,*) '  ---------------- '
c
c        if( irlat1.eq.0 )then
c          write(3,18) ld1, lm1, sl1, d_ns1
c        else
c          write(3,88)
c          write(3,84)
c          write(3,91)                        
c          write(3,88)
c        endif
cc
c        if( irlon1.eq.0 )then       
c          write(3,19) lod1,lom1,slo1,d_ew1
c        else
c          write(3,88)
c          write(3,85)
c          write(3,92)                       
c          write(3,88)
c        endif
cc
c        write(3,*) '  '
c        write(3,42) name2
c        write(3,*) '  ---------------- '
cc
c        if( irlat2.eq.0 )then
c          write(3,18) ld2, lm2, sl2, d_ns2
c        else
c          write(3,88)
c          write(3,86)
c          write(3,91)                       
c          write(3,88)
c        endif
cc
c        if( irlon2.eq.0 )then
c          write(3,19) lod2,lom2,slo2,d_ew2
c        else
c          write(3,88)
c          write(3,87)
c          write(3,92)                       
c          write(3,88)
c        endif
cc
c        write(3,*) '  '
c        write(3,34) iaz1,maz1,saz1
c        write(3,35) iaz2,maz2,saz2
c        write(3,32) edist,dmt
c        write(3,*) '  '
c      endif
cc
c   80 write(*,*) '  '
c      write(*,*) '  '
c      write(*,*) '  '
c      write(*,*) '  1) Another inverse, different ellipsoid.'
c      write(*,*) '  2) Same ellipsoid, two new stations.'
c      write(*,*) '  3) Same ellipsoid, same First Station.'
c      write(*,*) '  4) Done.'
c      write(*,*) '  '
c      write(*,*) '  Enter choice : '
c      read(*,10) answer
cc
c      if(     answer.eq.'1' )then
c        goto 1
c      elseif( answer.eq.'2' )then
c        goto 15
c      elseif( answer.eq.'3' )then
c        goto 20
c      else
c        write(*,*) '  Thats all, folks!'
c      endif
c
cc     stop
c      end
      return
      end
c
c      subroutine bufdms (buff,lgh,hem,dd,dm,ds,ierror)
c      implicit double precision (a-h, o-z)
c      implicit integer (i-n)
cc
c      logical     done,flag
cc
c      character*1 buff(*),abuf(21)
c      character*1 ch
c      character*1 hem
c      integer*4   ll,lgh
c      integer*4   i4,id,im,is,icond,ierror
c      real*8      x(5)
cc
cc     set the "error flag" 
cc
c      ierror = 0
c      icond  = 0
cc
cc     set defaults for dd,dm,ds
cc
c      dd = 0.0d0
c      dm = 0.0d0
c      ds = 0.0d0
cc
cc     set default limits for "hem" flag
cc
c      if(     hem.eq.'N' .or. hem.eq.'S' )then
c        ddmax = 90.0d0
c      elseif( hem.eq.'E' .or. hem.eq.'W' )then
c        ddmax = 360.0d0
c      elseif( hem.eq.'A' )then
c        ddmax = 360.0d0
c      elseif( hem.eq.'Z' )then
c        ddmax = 180.0d0
c      elseif( hem.eq.'*' )then
c        ddmax  = 0.0d0
c        ierror = 1
c      else
c        ddmax = 360.0d0
c      endif
cc
c      do 1 i=1,5
c        x(i) = 0.0d0
c    1 continue
cc
c      icolon = 0
c      ipoint = 0
c      icount = 0
c      flag   = .true.
c      jlgh   = lgh
cc
c      do 2 i=1,jlgh
c        if( buff(i).eq.':' )then
c          icolon = icolon+1
c        endif
c        if( buff(i).eq.'.' )then
c          ipoint = ipoint+1
c          flag   = .false.
c        endif
c        if( flag )then
c          icount = icount+1
c        endif
c    2 continue
cc
c      if( ipoint.eq.1 .and. icolon.eq.0 )then
cc
cc       load temp buffer
cc
c        do 3 i=1,jlgh
c          abuf(i) = buff(i)
c    3   continue
c        abuf(jlgh+1) = '$'
c        ll = jlgh
cc
c        call gvalr8 (abuf,ll,r8,icond)
cc
c        if( icount.ge.5 )then
cc
cc         value is a packed decimal of ==>  DDMMSS.sssss       
cc
c          ss = r8/10000.0d0
c          id = idint( ss )
cc
c          r8 = r8-10000.0d0*dble(float(id))
c          ss = r8/100.0d0
c          im = idint( ss )
cc
c          r8 = r8-100.0d0*dble(float(im))
c        else
cc
cc         value is a decimal of ==>  .xx   X.xxx   X.  
cc
c          id = idint( r8 )
c          r8 = (r8-id)*60.0d0
c          im = idint( r8 )
c          r8 = (r8-im)*60.0d0
c        endif
cc
cc       account for rounding error
cc
c        is = idnint( r8*1.0d5 )
c        if( is.ge.6000000 )then
c           r8 = 0.0d0
c           im = im+1
c        endif
cc
c        if( im.ge.60 )then
c          im = 0
c          id = id+1
c        endif
cc
c        dd = dble( float( id ) )
c        dm = dble( float( im ) )
c        ds = r8
c      else
cc
cc       buff() value is a d,m,s of ==>  NN:NN:XX.xxx    
cc
c        k    = 0
c        next = 1
c        done = .false.
c        ie   = jlgh
cc
c        do 100 j=1,5
c          ib = next
c          do 90 i=ib,ie
c            ch   = buff(i)
c            last = i
c            if( i.eq.jlgh .or. ch.eq.':' )then
c              if( i.eq.jlgh )then
c                done = .true.
c              endif
c              if( ch.eq.':' )then
c                last = i-1
c              endif
c              goto 91
c            endif
c   90     continue
c          goto 98
cc
c   91     ipoint = 0
c          ik     = 0
c          do 92 i=next,last
c            ik = ik+1
c            ch = buff(i)
c            if( ch.eq.'.' )then
c              ipoint = ipoint+1
c            endif
c            abuf(ik) = buff(i) 
c   92     continue
c          abuf(ik+1) = '$' 
cc
c          ll = ik
c          if( ipoint.eq.0 )then
c            call gvali4 (abuf,ll,i4,icond)
c            r8 = dble(float( i4 )) 
c          else
c            call gvalr8 (abuf,ll,r8,icond)
c          endif
cc
c          k    = k+1
c          x(k) = r8
cc
c   98     if( done )then
c            goto 101
c          endif
cc
c          next = last
c   99     next = next+1     
c          if( buff(next).eq.':' )then
c            goto 99
c          endif
c  100   continue
cc
cc       load dd,dm,ds
cc
c  101   if( k.ge.1 )then
c          dd = x(1)
c        endif
cc
c        if( k.ge.2 )then
c          dm = x(2)
c        endif
cc
c        if( k.ge.3 )then
c          ds = x(3)
c        endif
c      endif
cc
c      if( dd.gt.ddmax  .or.
c     1    dm.ge.60.0d0 .or.
c     1    ds.ge.60.0d0 )then
c        ierror = 1
c        dd = 0.0d0
c        dm = 0.0d0
c        ds = 0.0d0
c      endif
cc
c      if( icond.ne.0 )then
c        ierror = 1
c      endif
cc
c      return
c      end
c
c      subroutine elipss (elips)
c      implicit double precision(a-h,o-z)
c      character*1  answer
c      character*30 elips
c      common/elipsoid/a,f
c      write(*,*) '  Other Ellipsoids.'
c      write(*,*) '  -----------------'
c      write(*,*) '  '
c      write(*,*) '  A) Airy 1858'
c      write(*,*) '  B) Airy Modified'
c      write(*,*) '  C) Australian National'
c      write(*,*) '  D) Bessel 1841'
c      write(*,*) '  E) Clarke 1880'
c      write(*,*) '  F) Everest 1830'
c      write(*,*) '  G) Everest Modified'
c      write(*,*) '  H) Fisher 1960'
c      write(*,*) '  I) Fisher 1968'
c      write(*,*) '  J) Hough 1956'
c      write(*,*) '  K) International (Hayford)'
c      write(*,*) '  L) Krassovsky 1938'
c      write(*,*) '  M) NWL-9D (WGS 66)'
c      write(*,*) '  N) South American 1969'
c      write(*,*) '  O) Soviet Geod. System 1985'
c      write(*,*) '  P) WGS 72'
c      write(*,*) '  Q-Z) User defined.'
c      write(*,*) '  '
c      write(*,*) '  Enter choice : '
c      read(*,10) answer
c   10 format(a1)
cc
c      if(answer.eq.'A'.or.answer.eq.'a') then
c        a=6377563.396d0
c        f=1.d0/299.3249646d0
c        elips='Airy 1858'
c      elseif(answer.eq.'B'.or.answer.eq.'b') then
c        a=6377340.189d0
c        f=1.d0/299.3249646d0
c        elips='Airy Modified'
c      elseif(answer.eq.'C'.or.answer.eq.'c') then
c        a=6378160.d0
c        f=1.d0/298.25d0
c        elips='Australian National'
c      elseif(answer.eq.'D'.or.answer.eq.'d') then
c        a=6377397.155d0
c        f=1.d0/299.1528128d0
c        elips='Bessel 1841'
c      elseif(answer.eq.'E'.or.answer.eq.'e') then
c        a=6378249.145d0
c        f=1.d0/293.465d0
c        elips='Clarke 1880'
c      elseif(answer.eq.'F'.or.answer.eq.'f') then
c        a=6377276.345d0
c        f=1.d0/300.8017d0
c        elips='Everest 1830'
c      elseif(answer.eq.'G'.or.answer.eq.'g') then
c        a=6377304.063d0
c        f=1.d0/300.8017d0
c        elips='Everest Modified'
c      elseif(answer.eq.'H'.or.answer.eq.'h') then
c        a=6378166.d0
c        f=1.d0/298.3d0
c        elips='Fisher 1960'
c      elseif(answer.eq.'I'.or.answer.eq.'i') then
c        a=6378150.d0
c        f=1.d0/298.3d0
c        elips='Fisher 1968'
c      elseif(answer.eq.'J'.or.answer.eq.'j') then
c        a=6378270.d0
c        f=1.d0/297.d0
c        elips='Hough 1956'
c      elseif(answer.eq.'K'.or.answer.eq.'k') then
c        a=6378388.d0
c        f=1.d0/297.d0
c        elips='International (Hayford)'
c      elseif(answer.eq.'L'.or.answer.eq.'l') then
c        a=6378245.d0
c        f=1.d0/298.3d0
c        elips='Krassovsky 1938'
c      elseif(answer.eq.'M'.or.answer.eq.'m') then
c        a=6378145.d0
c        f=1.d0/298.25d0
c        elips='NWL-9D  (WGS 66)'
c      elseif(answer.eq.'N'.or.answer.eq.'n') then
c        a=6378160.d0
c        f=1.d0/298.25d0
c        elips='South American 1969'
c      elseif(answer.eq.'O'.or.answer.eq.'o') then
c        a=6378136.d0
c        f=1.d0/298.257d0
c        elips='Soviet Geod. System 1985'
c      elseif(answer.eq.'P'.or.answer.eq.'p') then
c        a=6378135.d0
c        f=1.d0/298.26d0
c        elips='WGS 72'
c      else
c        elips = 'User defined.'
cc
c        write(*,*) '  Enter Equatorial axis,   a : '
c        read(*,*) a
c        a  = dabs(a)
cc
c        write(*,*) '  Enter either Polar axis, b or '
c        write(*,*) '  Reciprocal flattening,   1/f : '
c        read(*,*) ss
c        ss = dabs(ss)
cc
c        f = 0.0d0
c        if( 200.0d0.le.ss .and. ss.le.310.0d0 )then
c          f = 1.d0/ss  
c        elseif( 6000000.0d0.lt.ss .and. ss.lt.a )then
c          f = (a-ss)/a
c        else
c          elips = 'Error: default GRS80 used.'
c          a     = 6378137.0d0
c          f     = 1.0d0/298.25722210088d0
c        endif
c      endif
cc
c      return
c      end
c
c      subroutine fixdms (ideg, min, sec, tol )
cc
c      implicit double precision (a-h, o-z)
c      implicit integer (i-n)
cc
cc     test for seconds near 60.0-tol
cc
c      if( sec.ge.( 60.0d0-tol ) )then
c        sec  = 0.0d0
c        min  = min+1
c      endif
cc
cc     test for minutes near 60
cc
c      if( min.ge.60 )then
c        min  = 0
c        ideg = ideg+1
c      endif 
cc
cc     test for degrees near 360
cc
c      if( ideg.ge.360 )then
c        ideg = 0
c      endif 
cc
c      return
c      end 
c
c      subroutine hem_ns ( lat_sn, hem )
c      implicit integer (i-n)
c      character*6  hem
cc
c      if( lat_sn.eq.1 ) then
c        hem = 'North '
c      else
c        hem = 'South '
c      endif
cc
c      return
c      end
c      subroutine hem_ew ( lon_sn, hem )
c      implicit integer (i-n)
c      character*6  hem
cc
c      if( lon_sn.eq.1 ) then
c        hem = 'East  '
c      else
c        hem = 'West  '
c      endif
cc
c      return
c      end
c      subroutine getrad(d,m,sec,isign,val)
c 
c*** comvert deg, min, sec to radians
c 
c      implicit double precision(a-h,j-z)
c      common/const/pi,rad
c 
c      val=(d+m/60.d0+sec/3600.d0)/rad
c      val=dble(isign)*val
c 
c      return
c      end
c      subroutine gvali4 (buff,ll,vali4,icond)
c      implicit     integer (i-n)
cc
c      logical      plus,sign,done,error
c      character*1  buff(*)
c      character*1  ch
cc
cc     integer*2    i
cc     integer*2    l1
cc
c      integer*4    ich,icond
c      integer*4    ll    
c      integer*4    vali4
cc
c      l1    = ll
c      vali4 = 0
c      icond = 0
c      plus  = .true.
c      sign  = .false.
c      done  = .false.
c      error = .false.
cc
c      i = 0
c   10 i = i+1
c      if( i.gt.l1 .or. done )then
c        go to 1000
c      else
c        ch  = buff(i)
c        ich = ichar( buff(i) )
c      endif
cc
c      if(     ch.eq.'+' )then
cc
cc       enter on plus sign
cc
c        if( sign )then
c          goto 150
c        else 
c          sign = .true.
c          goto 10
c        endif
c      elseif( ch.eq.'-' )then
cc
cc       enter on minus sign
cc
c        if( sign )then
c          goto 150
c        else
c          sign = .true.
c          plus = .false.
c          goto 10
c        endif
c      elseif( ch.ge.'0' .and. ch.le.'9' )then
c        goto 100
c      elseif( ch.eq.' ' )then
cc
cc       enter on space -- ignore leading spaces
cc
c        if( .not.sign )then
c          goto 10
c        else
c          buff(i) = '0'
c          ich = 48
c          goto 100
c        endif
c      elseif( ch.eq.':' )then
cc
cc       enter on colon -- ignore 
cc
c        if( .not.sign )then
c          goto 10
c        else
c          goto 1000
c        endif
c      elseif( ch.eq.'$' )then
cc
cc       enter on dollar "$"      
cc
c        done = .true.
c        goto 10
c      else
cc
cc       something wrong
cc
c        goto 150
c      endif
cc
cc     enter on numeric
cc
c  100 vali4 = 10*vali4+(ich-48)
c      sign  = .true.
c      goto 10
cc
cc     treat illegal character
cc
c  150 buff(i) = '0'
c      vali4 = 0
c      icond = 1
cc
c 1000 if( .not.plus )then
c        vali4 = -vali4
c      endif
cc
c      return
c      end
c      subroutine gvalr8 (buff,ll,valr8,icond)
c      implicit     integer (i-n)
cc
c      logical      plus,sign,dpoint,done
cc
c      character*1  buff(*)
c      character*1  ch
cc
cc     integer*2    i, ip
cc     integer*2    l1
cc     integer*2    nn, num, n48
cc
c      integer*4    ich,icond
c      integer*4    ll
cc
c      real*8       ten
c      real*8       valr8
c      real*8       zero
cc
c      data zero,ten/0.0d0,10.0d0/
cc
c      n48     =  48
c      l1      =  ll
c      icond   =   0
c      valr8   =  zero  
c      plus    = .true.
c      sign    = .false.
c      dpoint  = .false.
c      done    = .false.
cc
cc     start loop thru buffer
cc
c      i = 0
c   10 i = i+1
c      if( i.gt.l1 .or. done )then
c        go to 1000
c      else 
c        ch  = buff(i)
c        nn  = ichar( ch )
c        ich = nn
c      endif 
cc
c      if(     ch.eq.'+' )then
cc
cc       enter on plus sign
cc
c        if( sign )then
c          goto 150
c        else
c          sign = .true.
c          goto 10
c        endif
c      elseif( ch.eq.'-' )then
cc
cc       enter on minus sign
cc
c        if( sign )then
c          goto 150
c        else
c          sign = .true.
c          plus = .false.
c          goto 10
c        endif
c      elseif( ch.eq.'.' )then
cc
cc       enter on decimal point
cc
c        ip     = 0
c        sign   = .true.
c        dpoint = .true.
c        goto 10
c      elseif( ch.ge.'0' .and. ch.le.'9' )then
c        goto 100
c      elseif( ch.eq.' ' )then
cc
cc       enter on space
cc
c        if( .not.sign )then
c          goto 10
c        else
c          buff(i) = '0'
c          ich = 48
c          goto 100
c        endif
c      elseif( ch.eq.':' .or. ch.eq.'$' )then
cc
cc       enter on colon or "$" sign
cc
c        done = .true.
c        goto 10
c      else
cc
cc       something wrong
cc
c        goto 150
c      endif
cc
cc     enter on numeric
cc
c  100 sign = .true.
c      if( dpoint )then
c        ip = ip+1
c      endif
cc
c      num   = ich
c      valr8 = ten*valr8+dble(float( num-n48 ))
c      goto 10
cc
cc     treat illegal character
cc
c  150 buff(i) = '0'
c      valr8   =  0.0d0
c      icond   =  1
cc
c 1000 if( dpoint )then
c        valr8 =  valr8/(ten**ip)
c      endif
cc
c      if( .not.plus )then
c        valr8 = -valr8
c      endif
cc
c      return
c      end
c
c      subroutine todmsp(val,id,im,s,isign)
c 
c*** convert position radians to deg,min,sec
c*** range is [-pi to +pi]
c 
c      implicit double precision(a-h,o-z)
c      common/const/pi,rad
c 
c    1 if(val.gt.pi) then
c        val=val-pi-pi
c        go to 1
c      endif
c 
c    2 if(val.lt.-pi) then
c        val=val+pi+pi
c        go to 2
c      endif
c 
c      if(val.lt.0.d0) then
c        isign=-1
c      else
c        isign=+1
c      endif
c 
c      s=dabs(val*rad)
c      id=idint(s)
c      s=(s-id)*60.d0
c      im=idint(s)
c      s=(s-im)*60.d0
c 
c*** account for rounding error
c 
c      is=idnint(s*1.d5)
c      if(is.ge.6000000) then
c        s=0.d0
c        im=im+1
c      endif
c      if(im.ge.60) then
c        im=0
c        id=id+1
c      endif
c 
c      return
c      end
c
c      subroutine trim (buff,lgh,hem)
cc
c      implicit integer (i-n)
c      character*1 ch,hem
c      character*1 buff(*)
c      integer*4   lgh
cc
c      ibeg = 1
c      do 10 i=1,50
c        if( buff(i).ne.' ' )then
c          goto 11
c        endif
c        ibeg = ibeg+1
c   10 continue
c   11 continue
c      if( ibeg.ge.50 )then
c        ibeg = 1
c        buff(ibeg) = '0'
c      endif
cc
c      iend = 50
c      do 20 i=1,50
c        j = 51-i
c        if( buff(j).eq.' ' )then
c          iend = iend-1
c        else
c          goto 21
c        endif
c   20 continue
c   21 continue
cc
c      ch = buff(ibeg)
c      if( hem.eq.'N' )then
c        if( ch.eq.'N' .or. ch.eq.'n' .or. ch.eq.'+' )then
c          hem = 'N'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'S' .or. ch.eq.'s' .or. ch.eq.'-' )then
c          hem = 'S'
c          ibeg = ibeg+1
c        endif
cc
cc       check for wrong hemisphere entry
cc
c        if( ch.eq.'E' .or. ch.eq.'e' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'W' .or. ch.eq.'w' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c      elseif( hem.eq.'W' )then
c        if( ch.eq.'E' .or. ch.eq.'e' .or. ch.eq.'+' )then
c          hem = 'E'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'W' .or. ch.eq.'w' .or. ch.eq.'-' )then
c          hem = 'W'
c          ibeg = ibeg+1
c        endif
cc
cc       check for wrong hemisphere entry
cc
c        if( ch.eq.'N' .or. ch.eq.'n' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c        if( ch.eq.'S' .or. ch.eq.'s' )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c      elseif( hem.eq.'A' )then
c        if( .not.('0'.le.ch .and. ch.le.'9') )then
c          hem = '*'
c          ibeg = ibeg+1
c        endif
c      else
cc        do nothing
c      endif
cc
cc
c      do 30 i=ibeg,iend
c        ch = buff(i)
cc
c        if(     ch.eq.':' .or. ch.eq.'.' )then
c          goto 30
c        elseif( ch.eq.' ' .or. ch.eq.',' )then
c          buff(i) = ':'
c        elseif( '0'.le.ch .and. ch.le.'9' )then
c          goto 30      
c        else
c          buff(i) = ':'
c        endif
cc
c   30 continue
cc
cc     left justify buff() array to its first character position
cc     also check for a ":" char in the starting position,
cc     if found!!  skip it
cc
c      j  = 0
c      ib = ibeg
c      ie = iend
cc
c      do 40 i=ib,ie
c        if( i.eq.ibeg .and. buff(i).eq.':' )then
cc
cc         move the 1st position pointer to the next char &
cc         do not put ":" char in buff(j) array where j=1    
cc
c          ibeg = ibeg+1
c          goto 40
c        endif
c        j = j+1
c        buff(j) = buff(i)
c   40 continue
cc
cc
c      lgh = iend-ibeg+1
c      j   = lgh+1
c      buff(j) = '$'
cc
cc     clean-up the rest of the buff() array
cc
c      do 50 i=j+1,50   
c        buff(i) = ' '    
c   50 continue
cc
cc     save a maximum of 20 characters
cc
c      if( lgh.gt.20 )then
c        lgh = 20
c        j   = lgh+1
c        buff(j) = '$'
c      endif
cc
c      return
c      end
*-----------------------------------------------------------------------
      subroutine vincenty_invers(a,rf,
     +b1,l1,b2,l2,faz,baz,s,it,sig,lam,kind)

*** inverse for long-line and antipodal cases.
*** latitudes may be 90 degrees exactly.
*** latitude positive north, longitude positive east, radians.
*** azimuth clockwise from north, radians.
*** original programmed by thaddeus vincenty, 1975, 1976
*** removed back side solution option, debugged, revised -- 2011may01 -- dgm
*** this version of code is interim -- antipodal boundary needs work

*** output (besides faz,baz, and s):
*** it,   iteration count
*** sig,  spherical distance on auxiliary sphere
*** lam,  longitude difference on auxiliary sphere
*** kind, solution flag:  kind=1, long-line;  kind=2, antipodal

      implicit double precision(a-h,o-z)
      double precision l1,l2,l,lam
      parameter(pi=3.1415926535897932384626433832795d0)
      parameter(tol=1.d-14, eps=1.d-15)

      boa =  1.d0-1.d0/rf
***** sinu1 = boa*dsin(b1)/dsqrt((boa*dsin(b1))**2+dcos(b1)**2)
***** cosu1 = dsqrt(-sinu1**2+1.d0)                               !*** roundoff
***** sinu2 = boa*dsin(b2)/dsqrt((boa*dsin(b2))**2+dcos(b2)**2)
***** cosu2 = dsqrt(-sinu2**2+1.d0)                               !*** roundoff

      beta1 = datan(boa*dtan(b1))             !*** better reduced latitude
      sinu1 = dsin(beta1)
      cosu1 = dcos(beta1)

      beta2 = datan(boa*dtan(b2))             !*** better reduced latitude
      sinu2 = dsin(beta2)
      cosu2 = dcos(beta2)

      l = l2-l1                               !*** longitude difference [-pi,pi]
      if(l.gt. pi) l=l-pi-pi
      if(l.lt.-pi) l=l+pi+pi
      prev = l
      test = l
      it =0
      kind=1
      lam=l                                              !*** v13   (rapp part II)

*** top of the long-line loop (kind=1)

    2 sinlam = dsin(lam)
***** if(dabs(pi-dabs(l)).lt.0.2d-10) sinlam=0.d0        !*** no--troublesome
      coslam = dcos(lam)
      temp = cosu1*sinu2-sinu1*cosu2*coslam
      sinsig = dsqrt((cosu2*sinlam)**2+temp**2)          !*** v14   (rapp part II)
      cossig = sinu1*sinu2+cosu1*cosu2*coslam            !*** v15   (rapp part II)
      sig = datan2(sinsig,cossig)
***** sinal = cosu1*cosu2*sinlam/sinsig                  !*** v17   (rapp part II)
      if(dabs(sinsig).lt.eps) then
        sinal = cosu1*cosu2*sinlam/dsign(eps,sinsig)     !*** avoid div 0
      else
        sinal = cosu1*cosu2*sinlam/sinsig
      endif
      cosal2 = -sinal**2+1.d0
***** costm = -2.d0*sinu1*sinu2/(cosal2+tol)+cossig      !*** v18   (rapp part II)
      if(dabs(cosal2).lt.eps) then
        costm= -2.d0*(sinu1*sinu2/dsign(eps,cosal2))+cossig    !*** avoid div 0
      else
        costm= -2.d0*(sinu1*sinu2/cosal2)+cossig
      endif
      costm2=costm*costm
      c=((-3.d0*cosal2+4.d0)/rf+4.d0)*cosal2/rf/16.d0    !*** v10   (rapp part II)

*** entry point of the antipodal loop (kind=2)

    6 it=it+1
      d=(((2.d0*costm2-1.d0)*cossig*c+costm)*sinsig*c+sig)*(1.d0-c)/rf  !*** v11
      if(kind.eq.1) then
        lam=l+d*sinal
        if(dabs(lam-test).lt.tol) go to 100
        if(dabs(lam).gt.pi) then
          kind=2
          lam=pi
          if(l.lt.0.d0) lam=-lam
          sinal = 0.d0
          cosal2 = 1.d0
          test = 2.d0
          prev = test
          sig= pi - dabs(datan(sinu1/cosu1) + datan(sinu2/cosu2))
          sinsig = dsin(sig)
          cossig = dcos(sig)
          c=((-3.d0*cosal2+4.d0)/rf+4.d0)*cosal2/rf/16.d0
          if(dabs(sinal-prev).lt.tol) go to 100
*****     costm = -2.d0*sinu1*sinu2/(cosal2+tol)+cossig
          if(dabs(cosal2).lt.eps) then
            costm= -2.d0*(sinu1*sinu2/dsign(eps,cosal2))+cossig    !*** avoid div 0
          else
            costm= -2.d0*(sinu1*sinu2/cosal2)+cossig
          endif
          costm2=costm*costm
          go to 6
        endif
        if(((lam-test)*(test-prev)).lt.0.d0.and.it.gt.5)
     *       lam=(2.d0*lam+3.d0*test+prev)/6.d0            !*** refined converge.
        prev = test
        test = lam
        go to 2
      else
        sinal= (lam-l)/d
        if(((sinal-test)*(test-prev)).lt.0.d0.and.it.gt.5)
     *       sinal=(2.d0*sinal+3.d0*test+prev)/6.d0        !*** refined converge.
        prev = test
        test = sinal
        cosal2= -sinal**2+1.d0
        sinlam= sinal*sinsig/(cosu1*cosu2)
        coslam = -dsqrt(dabs(-sinlam**2 +1.d0))
        lam= datan2(sinlam,coslam)
        temp = cosu1*sinu2-sinu1*cosu2*coslam
        sinsig = dsqrt((cosu2*sinlam)**2+temp**2)
        cossig = sinu1*sinu2+cosu1*cosu2*coslam
        sig = datan2(sinsig,cossig)
        c=((-3.d0*cosal2+4.d0)/rf+4.d0)*cosal2/rf/16.d0
        if(dabs(sinal-prev).lt.tol) go to 100
*****   costm = -2.d0*sinu1*sinu2/(cosal2+tol)+cossig
        if(dabs(cosal2).lt.eps) then
          costm= -2.d0*(sinu1*sinu2/dsign(eps,cosal2))+cossig    !*** avoid div 0
        else
          costm= -2.d0*(sinu1*sinu2/cosal2)+cossig
        endif
        costm2=costm*costm
        go to 6
      endif

*** convergence

  100 if(kind.eq.2) then                        !*** antipodal
        faz= sinal/cosu1
        baz= dsqrt(-faz**2+1.d0)
        if(temp.lt.0.d0) baz=-baz
        faz=datan2(faz,baz)
        tem1=-sinal
        tem2=sinu1*sinsig-cosu1*cossig*baz
        baz=datan2(tem1,tem2)
      else                                      !*** long-line
        tem1=cosu2*sinlam
        tem2=cosu1*sinu2-sinu1*cosu2*coslam
        faz=datan2(tem1,tem2)
        tem1=-cosu1*sinlam
        tem2=sinu1*cosu2-cosu1*sinu2*coslam
        baz=datan2(tem1,tem2)
      endif
      if(faz.lt.0.d0) faz= faz+pi+pi
      if(baz.lt.0.d0) baz= baz+pi+pi

*** Helmert 1880 from Vincenty "Geodetic inverse solution between antipodal points"

      ep2 =1.d0/(boa*boa)-1.d0
      bige=dsqrt(1.d0+ep2*cosal2)                                         !*** 15
      bigf=(bige-1.d0)/(bige+1.d0)                                        !*** 16
      biga=(1.d0+bigf*bigf/4.d0)/(1.d0-bigf)                              !*** 17
      bigb=bigf*(1.d0-0.375d0*bigf*bigf)                                  !*** 18
      z   =bigb/6.d0*costm*(-3.d0+4.d0*sinsig**2)*(-3.d0+4.d0*costm2)
      dsig=bigb*sinsig*(costm+bigb/4.d0*(cossig*(-1.d0+2.d0*costm2)-z))   !*** 19
      s=(boa*a)*biga*(sig-dsig)                                           !*** 20

      return
      end
