c
c
c   special version of ttim for generating synthetic travel times
c   for use in mulplt
c
c
c  update 
c  dec 30 by jh: exclude more phases
c  jan 2       : stop after 150 phases
c  jan 3       : do not use dist in s-file
c  jul 25      : geographic to geocentric latitude, read from other header line
c  oct 26      : bug reading depth
c  dec 94       ************ version 5.0 *****************  no change
c  jan 95   jh : look for any station file, also alternative
c  mar 31, 95,jh: Fix bug if calculated phase is next day relative to o-time
c  may 23 95    : Now min 500 phases, generate more if just return
c  september 96 : write at least one blank line if no phases
c  feb 99    jh : ---------------   verison 7.0 -------------------------
c                 year and station modified, rm computer type
c  jul 24       : make a definitions file for phases
c  sep 09,   lo : dont calculate travel times if station not in station file
c  sep 26    jh : comment change,include all phases since selection now
c                 can be made with iasp.def
c  dec 07       : changed name of routine distaz to iasp_distaz
c  mar 01       : smal change in reading def phases
c  jan 21 10 jh : y to Y for phase prefix
c  dec 7  15 jh : was not working from eev ???, not reading stations
c                 correctly.
c
c   reads nordic file name from the environmnetal variable
c   get origin time, epicenter depth,  from nordic file
c   and calculates iasp times. Output is in a nordic file with name 
c   iasp.out. Calculate distance using STATION0.HYP
c   file for station coordinates.
c
      save
      parameter (max=100)
      logical prnt(3),outp
      character*8 phcd(max),phlst(10),oldphase
      character*80 modnam,comp_modnam
      dimension tt(max),dtdd(max),dtdh(max),dddp(max),mn(max),ts(max)
      dimension xcor(max),tcor(max),usrc(2)
      integer year,month,day,hour,min,nhead,nrecord,nphase,id
      integer pday,pmonth,pyear     ! date of seismic phase
      real sec
      character*80 infile           ! nordic file name
      integer hyear,hmonth,hday     ! header date
      character*1 typ,exp
      character*80 data(500)
c-- station and component
      character*5 stat,stat_old
      character*2 outcomp
	  charACTER*1 ANSWER
c-- directory separator
      character*1 dchar
      integer ndef                   ! number of phases in def file
      character*80 deffile           ! def file
      character*8 defphase(200)      ! defined phases for output
      character*80 text
      integer doy
      double precision psec,osec
c--- file exist
      logical exist
      character*60 top_directory
      integer seiclen
      integer nistat
      character*5 istat(9999)
c      

      data in/33/,phlst(1)/'query'/,prnt(3)/.true./


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c      
      nphase=0
      oldphase='xxxxxxxx'
      call dir_char(dchar)
c
c   open out put file
c
      open(8,file='iasp.out',status='unknown')
      call topdir(top_directory)
      ITP=index(top_directory,' ')-1
c
c  get def file if it exists
c
      inquire(file='IASP.DEF',exist=exist)
      if(exist) then
         deffile='IASP.DEF'
      else
         deffile=
     *   top_directory(1:itp)//dchar//'DAT'//dchar//'IASP.DEF'
      endif
c
c   if def file, open and read
c
      ndef=0
      inquire(file=deffile,exist=exist)
      if(exist) then
        open(25,file=deffile,status='old')
 221    continue
        read(25,'(a)',end=222) text
        if(text(1:10).eq.'IASP-PHASE'.and. text(13:20).ne.' ') then
          ndef=ndef+1
          read(text(13:20),'(a8)') defphase(ndef)
        endif
        goto 221
 222    continue
        write(6,*) ' Number of defined phases ',ndef
        close(25)
      else
        write(6,*)' No IASP.DEF file, will use all phases'
      endif

c
c try to read iasp.inp which has station list to overwrite sfile
c
      nistat=0
      inquire(file='iasp.inp',exist=exist)
      if(exist) then
        write(*,*) ' reading stations from iasp.inp '
        nistat=1
        open(26,file='iasp.inp',status='old')
224     continue
        read(26,'(a)',end=225) text
        istat(nistat)=text(1:5)
        nistat=nistat+1
        goto 224
      
225     continue
        nistat=nistat-1
        close(26)
      endif 

c
c   get event file with hypocenter and distances
c
      call get_env_event(infile)
c
c   open and read event from data base
c
      open(21,file=infile,status='old')
c
c   set flag to indicate that only one event
c
      nrecord=-1
      CALL INDATA(21,NSTAT,NPHAS,NHEAD,NRECORD,TYP,EXP,DATA,id)
c
c    check if any hypocenter etc, first use first header line after first
c
      do i=2,nhead
        if(data(i)(80:80).eq.'1') then
           if(data(i)(17:20).eq.'    '.or. 
     *     data(i)(24:30).eq.'       '.
     *     or.data(i)(39:43).eq.'     ') then
              write(6,*)' Insufficient hypocenter info in second ',
     *        'header line'
              goto 3645   ! try main header
            else
              write(6,*)' Use second header line'
              goto 3646   ! read data
            endif
         endif
      enddo
 3645 continue
c
c   now check main header
c
        if(data(1)(17:20).eq.'    '.or. 
     *  data(1)(24:30).eq.'       '.
     *  or.data(1)(39:43).eq.'     ') then
           write(6,*)' Insufficient hypocenter info in main ',
     *     'header line'
          stop 
        endif
        write(6,*)' Use main header line'
        i=1

 3646 continue 
c
c   read header time, might be different from origin time
c
      read(data(1),'(1x,i4,1x,2i2)') hyear,hmonth,hday
c
c   read origin time
c
      read(data(i),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1)') year,month,day,
     *hour,min,sec
c      write(*,*) ' debug iasp ',year,month,day,hour,min,sec
c
c   read epicenter
      read(data(i)(24:38),'(f7.3,f8.3)') elat,elon
c
c   read depth
c
      read(data(i)(39:43),'(f5.1)') zs
c
c   gegraphic to geocentric lat
c
      call fold_lat(elat)
c
c   write header in output file, use s-file header
c
      write(8,'(1x,i4,1x,2i2,11x,a1,57x,a1)')hyear,hmonth,hday,'D','1'
c
c   calculate abs time
c
      call timsec(year,month,day,hour,min,sec,osec)

c
c
c   check for iaspei files
c
      call iasp91_filename(comp_modnam)
      inquire(file=comp_modnam
     &   (:seiclen(comp_modnam))//'.TBL',
     &    exist=exist)
      if(exist) then
         modnam=comp_modnam(1:seiclen(comp_modnam))//' '
      else
         modnam=
     *top_directory(1:itp)//dchar//'DAT'//dchar//
     *   comp_modnam(1:seiclen(comp_modnam))//' '
      endif
c
      iucff=25
      il=index(modnam,' ')-1
c     &recl=60)
      call assign(10,2,'ttim1.lis'//char(0))
c     call tabin(in,iucff,modnam)
      call tabin(in,modnam)
      prnt(1)=.false.
c      prnt(2)=.false.
      phlst(1)='all     '
c      phlst(1)='basic   '
c      phlst(2)='SS      '
      call brnset(1,phlst,prnt)
c
 3    continue 
c     call depset(zs,usrc,iucff)
      call depset(zs,usrc)
c
c
c
c-----------------------------------------------------------------------
c    make a station loop for calculating phases and writing out
c-----------------------------------------------------------------------
c
      stat_old='xxxxx'
      if (nistat.eq.0) then   ! take stations from sfile if not in iasp.inp
        do irec=nhead+1,nrecord-1
           read(data(irec)(2:6),'(a5)') stat
c
c   do not calculate more than once for same station, assume follow each other
c
c         if(stat.eq.stat_old) goto 888
c          stat_old=stat
          if (stat.ne.stat_old) then
            nistat=nistat+1
            istat(nistat)=stat
            stat_old=stat
          endif
        enddo
      endif
      write(6,*) 'Number of stations used:',nistat

      do irec=1,nistat    !  loop over stations
            stat=istat(irec)
      
c
c  calculate distance
c
            call stat_loc(stat,data(1)(21:21),slat,slon,elev)
c
c   convert from geographic to geocentric coordinates
c
            call fold_lat(slat)
            call iasp_distaz (slat,slon,elat,elon,delta,azi,baz)
c           write(*,*) slat,slon,delta

c
c  check if loaction, otherwise skip station
c  
         if (slat.eq.0.and.slon.eq.0) then
           write(*,'(2a)') 'station not in station file: ',stat
           goto 888
         endif

         if(delta.lt.0.) go to 3
         call trtm(delta,max,n,tt,dtdd,dtdh,dddp,phcd)
         if(n.le.0) then 
            write(*,101)delta
 101        format(/1x,'No arrivals for delta =',f7.2)
            goto 888
         endif
c
         do 4 i=1,n
c
c   calculate time in hr min etc
c
           psec=tt(i)+osec
           call sectim(psec,pyear,doy,pmonth,pday,hour,min,sec)
c
c   check if next day relative to s-file header, then add 24 hours
c
           if(pyear.gt.hyear.or.pmonth.gt.hmonth.or.pday.gt.hday) 
     *     hour=hour+24
c
c   write to nordic file, but filter out more exotic phases in no
c   def file is given
c
           outcomp='  '
           if(ndef.gt.0) then   !case of def file
             outp=.false.
             do k=1,ndef
               if(phcd(i).eq.defphase(k)) outp=.true.
c              write(6,'(a,1x,a)') phcd(i),defphase(k)
c              if(outp) write(6,*) 'selectec'
             enddo
           else                 ! no def file
              outp=.true.
c             if(phcd(i)(1:6).eq.'pPKiKP') outp=.false.
c             if(phcd(i)(1:6).eq.'sPKiKP') outp=.false.
c             if(phcd(i)(1:4).eq.'SnSn') outp=.false.
           endif
           if(phcd(i).eq.oldphase) outp=.false.
           if(outp) then
		       nphase=nphase+1
c commented out lo 16 March 2016
c			   if(nphase.eq.500) then
c                  write(6,*)' You have now generated 500 phases'
c                  write(6,*)' Continue (y=default/n)?'
c                  read(5,'(a1)') answer
c                  if(answer.eq.'n'.or.answer.eq.'N')
c     *            stop
c	           endif
               write(8,200)stat,outcomp,phcd(i)(1:8),
     +          hour,min,sec
                oldphase=phcd(i)
           endif
200        format(1x,a5,a2,1x,'Y',a8,i2,i2,1x,f5.2)


           xcor(i)=0.
           tcor(i)=0.
           mn(i)=tt(i)/60.
           ts(i)=.01*int(100.*(tt(i)-mn(i)*60.)+.5)
           if(ts(i).lt.60.) go to 4
           mn(i)=mn(i)+1
           ts(i)=ts(i)-60.
 4       continue
 888     continue
c
c----------------------------------------------------------------------
      enddo
c----------------------------------------------------------------------
c
c
c   write at least one blank line
c
      write(8,*)' '
      write(6,*)nphase, ' phases generated' 
      call retrns(in)
      call retrns(10)
      call vexit(0)
      end

c
c   routine to calculate dist az in nordic file
      subroutine iasp_distaz (alat1,alon1,alat2,alon2, delta,azi,baz)
c
c Calculate angular distance, azimuth and backazimuth between two
c  points on a sphere.
c
c Input
c
c   ALAT1,ALON1  =  latitude and longitude of point 1.
c   ALAT2,ALON2  =  latitude and longitude of point 2.
c
c Output
c
c   DELTA  =  angular distance between points 1 and 2.
c   AZI    =  azimuth from north of point 2 w.r.t. point 1.
c   BAZ    =  azimuth from north of point 1 w.r.t. point 2.
c
c All arguments are in degrees.
c Latitude, longitude and DELTA are geocentric.
c Latitude is zero at equator and positive north.
c Longitude is positive toward the east.
c AZI and BAZ are positive and measured clockwise from local north.
c
c      implicit double precision (a-h,o-z)

c     real alat1,alat2,alon1,alon2,delta,azi,baz
      pi=3.14159265
      rtod=180./pi
      dtor=pi/180.
c
      rlat1 = dtor*alat1
      rlat2 = dtor*alat2
      rdlon = dtor*(alon2-alon1)
c
      clat1 = cos(rlat1)
      clat2 = cos(rlat2)
      slat1 = sin(rlat1)
      slat2 = sin(rlat2)
      cdlon = cos(rdlon)
      sdlon = sin(rdlon)
c
      cdel = slat1 * slat2 + clat1 * clat2 * cdlon
      cdel = min(cdel,1.0)
      cdel = max(cdel,-1.0)
      yazi = sdlon * clat2
      xazi = clat1 * slat2 - slat1 * clat2 * cdlon

      ybaz = -sdlon * clat1
      xbaz = clat2 * slat1 - slat2 * clat1 * cdlon
c
      delta = rtod * acos(cdel)
c     write(16,*)'delta ',delta
c      write(*,*)' xazi yazi ',xazi,yazi
      if(yazi.ne.0.0.or.xazi.ne.0.0)then
        azi = rtod * atan2(yazi,xazi)
        baz = rtod * atan2(ybaz,xbaz)
      else
        azi=0.0
        baz=0.0
      endif
      if (azi.lt.0.) azi=azi+360.
      if (baz.lt.0.) baz=baz+360.
c
      return
      end

      subroutine fold_lat(alat)
c
c-------- given geographic lat compute geocentric lat
c
c input:  la     degree portion of latitude in degrees
c         ins    n for north, s for south
c         ala    minutes portion of latitude
c         lo     degree portion of longitude
c         iew    e for east, w for west
c         alo    minutes portion of longitude
c output: alat   geocentric latitude in radians
c         alon   longitude in radians
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
      parameter (c1 = (1.0 - flat)**2)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
c
      alat = alat*rad
c  convert from geographic to geocentric latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 201
         alat = alat/c1-sign(c2,alat)
         goto 202
  201    alat = atan(c1*tan(alat))
  202    continue
         alat=alat/rad
      return
      end


