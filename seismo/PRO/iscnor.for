c
c
c   Convert ISC  Fixed File format to Nordic format
c   J. Havskov, November 1993
c
c   changes:

c   sep 22, 94 by jh: add polarity and number of stations
c   dec 95          : many changes, complete overhaul
c   jan 21  96      : agency file separately
c   feb 18          : only write out isc format if selected, bug
c   jan 3   97      : adopt for cd rom, small fixes
c   feb 5           : use isc phases optionally
c   mar 5           : some error in phase selection
c   aug 98          : limit output on screen, bug with magnitude and
c                     area selection, skip events with ewrrros, seems to
c                     be erratic. Taking CD in and out might cure the problem.
c   sep 25  99       -----------------    version 7.0 ------------------------
c                    minor changes
c   sep 10  03      : read from both old and new cd structure 
c   nov 21  03      : seems like read of station coordinates was wrong
c                     secons were 10 times too large
c     2009-05-06 pv : bug fixed - error when reading surface wave amplitudes line 939
c   apr 27 10 jh    : B to b and S to s for mag
c   mar 5 2014 jh   : fix endof file (gfortran), fix upper and lowe case ffb (has changed in CD)
c                     fix that empty months can be skipped
c
c   to get output in isc format, set debug=1. the output file name is iscnor.isc
c 
      implicit none
      include 'libsei.inc'           ! for libsei.for
      integer readings
      parameter (readings=20000)     ! max number of number of lines, one event
      character*80 data(readings)    ! nordic format data        
      character*80 infile            ! input file name
      character*80 text              ! general text
      integer code                   ! error code
      integer nrecord,nrecord1,nhead   ! # records and headers in isc event
      character*96 iscd              ! one line in isc file
      character*96 isc(readings)     ! one event, isc format
      character*7  agency(readings)  ! agencies reporting hypocenter
      integer ilatmin,ilonmin        ! whole minutes of lat and lon
      logical wout                   ! if true, write out selected event on screen
      character*1 answer               
      real latmin,lonmin             ! minutes and seconds of lat and lon
      real minlat,maxlat,minlon,maxlon  ! range if selected
      real mindepth,maxdepth         ! range of depths to select in
      logical select                 ! true if in selection window
      integer in                     ! 0: single file, 1: from filenr.lis
                                     ! 2: from cdrom
      character*1 drive              ! cdrom drive letter
      integer year1,year2,month1,month2  ! time interval used on cdrom
      integer hypo                   ! 0: all event output, 1: only header line
      integer phase_select           ! selection of phases
      logical first_file             ! true if a first fiel is read, used to jump empty months
      real minmag,maxmag             ! magnitude range
      integer i,nr,nevent,debug,k    ! counters etc
      logical pc,sun,linux                 ! computer type


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      call computer_type(sun,pc,linux)
	  
      debug=0                        ! set to 1 and more aoutput available
      nevent=0
      if(debug.eq.1) then 
         open(27,file='iscnor.dbg',status='unknown')
         open(20,file='iscnor.isc',status='unknown')
      endif
c
 22   continue
      write(6,*)' Phases selected can be:'
      write(6,*)'   User reported phases (default=return)          : 1'
      write(6,*)'   ISC identified phases only                     : 2'
      write(6,*)'   ISC identified phases and user reported phases    '
      write(6,*)'       when not identified by ISC                 : 3'
      read(5,'(a)') text
      if(text(1:2).ne.' ') then
             call sei get values(1,text,code)
             phase_select=array$(1)
      else
	         phase_select=1
      endif
      if(phase_select.gt.3.or.phase_select.lt.1) goto 22
c
c   set defaults
c
        mindepth=-10.0
        maxdepth=1000.0
        minlat=-99.0
        maxlat=99.0
        minlon=-500.0
        maxlon=500.0
        hypo=0
        minmag=-99
        maxmag=99
        first_file=.false.
c
 11     continue
        write(6,*) ' Output: All hypocenters and phases : Return'
		write(6,*) '         All hypocenters            : 1'
        write(6,*) '         Prime hypocenter           : 2'
        read(5,'(a1)') answer
        if(answer.ne.' ') read(answer,'(i1)',err=11) hypo
        if(hypo.gt.2.or.hypo.lt.0) goto 11
c
c   get choises
c
        write(6,*)' Latitude range, return for all '
        read(5,'(a)') text
         if(text(1:2).ne.' ') then
             call sei get values(2,text,code)
             minlat=array$(1)
             maxlat=array$(2)
         endif
         write(6,*)' Longitude range, return for all'
         read(5,'(a)') text
         if(text(1:2).ne.' ') then
             call sei get values(2,text,code)
             minlon=array$(1)
             maxlon=array$(2)
         endif
         write(6,*)' Depth range, return for all'
         read(5,'(a)') text
         if(text(1:2).ne.' ') then
             call sei get values(2,text,code)
             mindepth=array$(1)
             maxdepth=array$(2)
         endif
         write(6,*)' Magnitude range, return for all'
         read(5,'(a)') text
         if(text(1:2).ne.' ') then
             call sei get values(2,text,code)
             minmag=array$(1)
             maxmag=array$(2)
         endif
         write(6,*)' Write selected events on screen (y/no=return)'
         read(5,'(a)') answer
         if(answer.eq.'y'.or.answer.eq.'Y') then
            wout=.true.
         else
            wout=.false.
         endif
c
c   output file in nordic format
c
      open(2,file='iscnor.out',status='unknown')
c
c   open station file
c
      open(3,file='isc.sta',status='unknown')
c
c   check if an agency file, if so read
c
      do i=1,readings
         agency(i)='       '
      enddo
      open(8,file='agency.isc',status='old',err=2233)
      write(6,*)' An agency file is present'
         k=0
 2220    continue
         read(8,'(a)',end=2299) iscd
         if(iscd(1:2).eq.'90') then
            read(iscd(11:13),'(i3)') i   ! agency number
            agency(i)=iscd(14:20)
            k=k+1
         endif
         goto 2220
 2299    continue
         write(6,*)' Number of agencies in agency.isc file',k
         goto 2240
 2233    continue
         write(6,*)' No agency.isc file present'
 2240 continue
      close(8)
c
c------------------------------------
c   get to here to read next file
c------------------------------------
c
      drive=' '
c
c   check for CD
c
      if(pc) then
         write(6,*)' If ISC CDROM, give drive letter, else return'
         read(5,'(a)') drive
      endif
      if(drive.ne.' ') then
         write(6,'(a,$)')' Give first year and month, e.g. 199501 '
         read(5,'(i4,i2)')year1,month1
         write(6,'(a,$)')' Give  last year and month, e.g. 199602 '
         read(5,'(i4,i2)')year2,month2
         in=2           ! input from cd room
         month1=month1-1 ! initialize for loop add
         if(month1.eq.0) then
            year1=year1-1
            month1=12
         endif
      endif
c
 500  continue
      if(in.eq.0) then                        ! interactive input or first time
         write(6,*)' Input file name, filenr.lis for a list of names'
         read(5,'(a)') infile
         if(infile(1:4).eq.'    ') goto 199   ! no more files, stop
      endif
c
c   check if a list of names
c
      if(infile(1:10).eq.'filenr.lis') then
         open(8,file='filenr.lis',status='old',err=20)
         goto 21
 20      continue
         write(6,*)' No filenr.lis'
         stop
 21      continue
         in=1
      endif
c
c   go straight to read
c
      if(in.eq.0) then
         open(1,file=infile,status='old',err=505)
         goto 506
      endif
c
c   file loop if many files
c
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
         open(1,file=infile,status='old',err=505)
         goto 506
      endif
c
c   loop over cdrom
c
      if(in.eq.2) then
         month1=month1+1
         if(month1.gt.12) then
            month1=1
            year1=year1+1
         endif
         if(year1.gt.year2.or.
     *   year1.eq.year2.and.month1.gt.month2) then
             goto 199
         endif
c
c   make CD file name, two possibilities, old and new structure
c   2009 plus one more
c
c
c  old structure
c

         write(infile,'(a,a,a,i4,a,i4,i2,a)')
     *   drive,':',char(92),year1,char(92),year1,month1,'.FFB'
         do i=1,16
           if(infile(i:i).eq.' ') infile(i:i)='0'
         enddo
         open(1,file=infile,status='old',err=504)
         goto 506
c
c  new structure
c
 504     continue

         write(infile,'(a,a,a,a,a,i4,a,i4,i2,a)')
     *   drive,':',char(92),'ffb',char(92),year1,char(92),
     *   year1,month1,'.FFB'
          do i=1,20
           if(infile(i:i).eq.' ') infile(i:i)='0'
         enddo
         open(1,file=infile,status='old',err=5005)
         goto 506
 5005    continue
         infile(20:22)='ffb'
         open(1,file=infile,status='old',err=505)
         goto 506

      endif
c
c   error condition
c

 505  continue
      if(first_file) goto 89  ! one has already been read so maybe a gap

      write(6,'(a,a)')' No such file:  ', infile

      if(in.eq.1.or.in.eq.2) then             ! stop if error in list of files
         goto 199
      else
         goto 500
      endif

 506  continue
      write(6,'(1x,a,a)') 'Now reading from: ',infile
c
c   set flag that first file was present
c
      first_file=.true.
c
c   read agency names and stations from within parameter file, might be
c   read twice
c    
      k=0
      read(1,'(a)') iscd
      do while(iscd(1:2).ne.' 1')     ! continue until first hypocenter record
         read(1,'(a)',end=99) iscd
         if(iscd(1:2).eq.'90') then
            read(iscd(11:13),'(i3)') i   ! agency number
            agency(i)=iscd(14:20)
            k=k+1
         endif
         if(iscd(1:2).eq.'91') then
            read(iscd(64:68),'(i2,i3)') ilatmin,i
            latmin=ilatmin+i/600.0    ! was 60.0 before nov 03
            read(iscd(73:77),'(i2,i3)') ilonmin,i
            lonmin=ilonmin+i/600.0   !  was 60.0 before nov 03 
            write(3,'(2x,a4,a2,f5.2,a1,a3,f5.2,a5)')
     *      iscd(15:18),iscd(62:63),latmin,
     *      iscd(69:69),iscd(70:72),lonmin,iscd(78:82)
         endif
      enddo
c      write(6,*)' Number of agencies in input file',k
      backspace 1                        ! point again to first hypocenter line
c
c   event read loop
c
 10   continue
c
c   read one event in isc format
c
      call read_one_isc(1,isc,nrecord,nhead)
      if(nrecord.eq.0) goto 89           ! end of file
      if(nrecord.gt.readings) then
         write(6,*)' Too small dimension in program'
         stop
      endif
      nrecord1=nrecord                   ! save number of isc lines for output

c
c   convert to nordic format and write out
c
      call  read_isc(isc,nrecord,agency,nr,data,minlat,maxlat,
     *minlon,maxlon,mindepth,maxdepth,select,
     *minmag,maxmag,phase_select,debug)
c
c   only write out if event is selected
c
      if(select) then 
         if(hypo.eq.0) then
            write(2,'(a)') (data(i),i=1,nr)  ! whole event
         elseif(hypo.eq.1) then              ! hypocenter lines
            do i=1,nr
              if(data(i)(80:80).eq.'1') write(2,'(a)') data(i)    
              if(data(i)(80:80).eq.'7') goto 44
            enddo
            text=' '
  44        continue
			write(2,'(a)') text              ! blank line to separate events
         else                                ! only main hypocenter
              write(2,'(a)') data(1)
         endif
         nevent=nevent+1
         if(wout) write(6,'(a,i7,2x,a,1x,a,1x,i4)')
     *' # ',nevent, data(1)(1:15),'# lines ',nr
         if(debug.eq.1) then
            write(20,'(a)')(isc(i),i=1,nrecord1)
            write(20,*)
         endif
       else
c          write(6,'(a,a,i6)')data(1)(1:15),
c     *    ' not selected ',
c     *    nrecord1
       endif
         
      goto 10   ! next event
c
 89   continue
      close(1)
      write(6,*) ' Number of events selected: ',nevent
      goto 500  ! next input file
c
c   get here if an error
c
 99   continue
      write(6,*)' Missing data in input file'
      close(1)
      goto 500
c
    
 199  continue
c      write(6,*)nevent,' events converted'
      write(6,*)' Output file name is iscnor.out'
      write(6,*)' File with stations is isc.sta'
      stop
      end

      subroutine read_isc(isc,nrecord,agency,nr,data,minlat,maxlat,
     *minlon,maxlon,mindepth,maxdepth,
     *select,minmag,maxmag,phase_select,debug)
c
c    isc: data in isc format
c    nrecord: number of records in isc format
c    agency: agency list
c    nr: number of records in nordic format returned
c    data: nordic format data returned
c    ninlat ...: parameters for selection
c    select: true if event selected
c    phase_select: selection of phases, see main program
c    debug: if 1, more debug out
c	  
c
c   convert one event in Fixed file ISC format and returns event in Nordic 
c   format in array data
c
c   principles:
c
c   phases: user phase, isc numerical or both can be output
c           if users phases are used then:
c           parenthsis are removed, if P/PKP etc is give, it is replaced by
c           P etc
c   times:  if day is incremented relative to origin time day, it is carried 
c           into the hours, which can be more than 24
c   agency: it is assumed that it is the same agency for hypocenter and first
c           magnitude. 2. magnitude is checked for agency, if blank, assumed
c           also to be the same. only first 3 chars of code used.
c   depth:  if no error on depth, a depth fix flag is set.
c   first motion: only c or d, isc codes J an B are ignored.
c   hypocenter orders: isc put best solution last, here the order is reversed
c           and the prime estimate is first
c   duration magnitude: change D to C for type
c   event type: if station furthest away is 1000 km, L, 1000-3000 km. R and 
c           more than 3000 km , D
c
      implicit none
      character*80 data(*)          ! Nordic array of data
      integer nrecord               ! number of lines in input isc
      character*7  agency(*)        ! agency name
      character*96 isc(*)           ! one event in isc format
      character*8  phase            ! the phase id
      integer      isc_phase        ! isc numerical phase code
      character*8  phase_out        ! ------------
      integer      phase_select     ! types of phases selected
      character*4  oldstat          ! save station code to count stations
      integer      nstat            ! number of stations
      integer      id               ! line type of isc file
      character*1  type             ! event type e.g. E for exploson
      real         sec              ! secs
      integer      origin_day       ! day of origin
      integer      phase_day        ! day of phase
      integer      phase_hour       ! hour of phase
      real         lat,lon,depth    ! hypocenter
      real         oerror,laterror,lonerror,herror    ! hypocenter errors
      real         mag,mag1,mag2    ! magnitude
      real         amp              ! amplitude
      integer      iamp             ! amntissa of amplitude
      real         period           ! period
      character*4  aperiod          ! period
      real         distance         ! epicentral distance
      real         max_distance     ! maximum -----------
      real         min_distance     ! minimum -----------
      real         residual         ! isc residual
      character*1  ev_type          ! distance indicator
      integer      nr               ! number of output records
      integer      ihead            ! headers out
      integer      iphase           ! number of phases
      integer      iagency          ! agency number
      integer      debug            ! 0. no debug, 1: debug
      integer      i,k,ir           ! counters etc
      real         minlat,maxlat,minlon,maxlon  ! window to select
      real         mindepth,maxdepth ! range of depths to select
	  real         minmag,maxmag    ! magnitude range to select
      logical      select           ! true if inside lat lon window
      character*7  piphas               ! isc phases
      common /piphas2/ piphas (0:100)   ! define isc numeric phases
	  
c
c   define isc numerical phases 
c
      call setfis
c
      select=.true.
      iphase=0
      ihead=0
      nr=0
      ir=1
      nstat=0
      oldstat='    '
      max_distance=0.0
      min_distance=99999.0
      mag1=-99.0
      mag2=-99.0
c
  1   continue                      ! back here for another line
c
      read(isc(ir)(1:2),'(i2)',err=8000) id
c
c----------------------------------
c  first hypocenter line
c----------------------------------
c
      if(id.eq.1) then
         nr=nr+1
         ihead=ihead+1
         do i=1,79
            data(nr)(i:i)=' '
         enddo
         data(nr)(80:80)='1'
         data(nr)(22:22)='D'         ! assume initially all to be distant
c
         read(isc(ir)(23:25),'(i3)',err=8000)   iagency ! agency number
         if(nr.lt.1) goto 8000
         data(nr)(46:48)=agency(iagency)(1:3)
         data(nr)(2:5)=isc(ir)(5:8)     ! year
         data(nr)(7:8)=isc(ir)(9:10)    ! month
         data(nr)(9:10)=isc(ir)(11:12)  ! day
         read(isc(ir)(11:12),'(i2)',err=8000) origin_day  ! save to compare with phase day
         data(nr)(12:13)=isc(ir)(13:14) ! hour
         data(nr)(14:15)=isc(ir)(15:16) ! min
         read(isc(ir)(17:20),'(f4.2)',err=8000) sec 
         if(debug.eq.1) 
     *   write(27,'(a,1x,i6,a)')data(nr)(1:17),iagency,agency(iagency)
         write(data(nr)(17:20),'(f4.1)') sec
         read(isc(ir)(27:33),'(f7.4)',err=8000) lat
         write(data(nr)(24:30),'(f7.3)') lat
         read(isc(ir)(36:43),'(f8.4)',err=8000) lon
         write(data(nr)(31:38),'(f8.3)') lon
         read(isc(ir)(46:49),'(f4.1)',err=8000) depth
         write(data(nr)(39:43),'(f5.1)') depth
c
c   check for window
c
         if((lat.gt.maxlat.or.lat.lt.minlat.or.lon.lt.minlon.
     *   or.lon.gt.maxlon.or.depth.gt.maxdepth.
     *   or.depth.lt.mindepth).and.nr.eq.1) then
c
c   outside window, return
c
            select=.false.
            return
         endif
            
         if(isc(ir)(52:55).ne.'    ') then
            read(isc(ir)(52:55),'(f4.2)',err=8000) mag              ! first magnitude
            if(nr.eq.1) mag1=mag                           ! save for later check
            write(data(nr)(56:59),'(f4.1)') mag
            data(nr)(60:60)=isc(ir)(62:62)                  ! type of magnitude
c
c   fix to new b and s
c

           if(data(nr)(60:60).eq.'B') data(nr)(60:60)='b'
           if(data(nr)(60:60).eq.'S') data(nr)(60:60)='s'
c
c   if a duration magnitude, write as C, seisan style
c
            if(data(nr)(60:60).eq.'d'.or.data(nr)(60:60).eq.'D')
     *      data(nr)(60:60)='C'
            data(nr)(61:63)=data(nr)(46:48)               ! mag agency, same as
          endif                                         ! hypocenter agency
      endif
c
c------------------------------
c  hypocenter continuation line
c------------------------------
c
      if(id.eq.2) then
            if(isc(ir)(11:14).ne.'    ') then
               read(isc(ir)(11:14),'(f4.2)',err=8000) mag        ! second magnitude
               write(data(nr)(64:67),'(f4.1)') mag
               if(nr.eq.1)mag2=mag
               data(nr)(68:68)=isc(ir)(21:21)            ! type of magnitude
           if(data(nr)(68:68).eq.'B') data(nr)(68:68)='b'
           if(data(nr)(68:68).eq.'S') data(nr)(68:68)='s'
c
c   if a duration magnitude, write as C, seisan style
c
            if(data(nr)(68:68).eq.'d'.or.data(nr)(68:68).eq.'D')
     *      data(nr)(68:68)='C'
               if(isc(ir)(15:17).ne.'   ')then
                   data(nr)(69:71)=isc(ir)(15:17)            ! mag agency
               else
                   data(nr)(69:71)=data(nr)(46:48)
               endif
            endif


            read(isc(ir)(32:36),'(f5.3)',err=8000) oerror        ! origin time error
            read(isc(ir)(39:44),'(f6.4)',err=8000) laterror      ! latitude error
            read(isc(ir)(47:52),'(f6.4)',err=8000) lonerror      ! longitde error
            read(isc(ir)(55:58),'(f4.1)',err=8000) herror        ! depth error
c
c   make error line
c
            if(oerror.ne.0.0.or.laterror.ne.0.0) then
               nr=nr+1
               ihead=ihead+1
               data(nr)=' '
               write(data(nr)(15:20),'(f6.2)') oerror
               write(data(nr)(25:30),'(f6.1)') laterror*111.195
               write(data(nr)(33:38),'(f6.1)') 
     *         lonerror*111.195*cos(lat/57.3)   ! convert to km
               if(herror.ne.0.0) then
                   write(data(nr)(39:43),'(f5.1)') herror
               else
                   data(nr-1)(44:44)='F'        ! depth fixed
               endif
               data(nr)(80:80)='E'
            endif
c
            type=isc(ir)(61:61)
            if(type.eq.'N'.or.type.eq.'H') type='E' ! explosion
c
c   make felt line
c
           if(type.eq.'F') then
              type=' '
              nr=nr+1
              ihead=ihead+1
              data(nr)=' '
              data(nr)(28:29)=isc(ir)(86:87)
              data(nr)(31:31)=isc(ir)(88:88)
              data(nr)(80:80)='2'
           endif
c
c   put in event type
c
           data(nr)(23:23)=type
        endif

c
c------------------------------------------------
c   first comments line
c------------------------------------------------
c
      if(id.eq.3) then                            ! more hypocneter info
           nr=nr+1
           ihead=ihead+1
           read(isc(ir)(21:23),'(i3)',err=8000) i     ! agency number
           if(i.gt.0) then
              data(nr)(2:8)=agency(i)
           else
              data(nr)(2:8)='       '
           endif
           data(nr)(8:8)=isc(ir)(24:24)     ! quality
           data(nr)(9:9)=' '
           data(nr)(10:79)=isc(ir)(25:94)
           data(nr)(1:1)=' '
           data(nr)(80:80)='3'
c
c   if comments left, put on new line
c
           if(isc(ir)(95:96).ne.'  ') then
             nr=nr+1
             data(nr)(1:1)=' '
             data(nr)(2:3)=isc(ir)(95:96)
             do i=3,79
               data(nr)(i:i)=' '
             enddo
             data(nr)(80:80)='3'
           endif
       endif
c
c---------------------------------------
c  second comment line
c---------------------------------------
c
      if(id.eq.4) then                            ! more hypocneter info
           nr=nr+1
           ihead=ihead+1
           data(nr)(2:79)=isc(ir)(13:90)
           data(nr)(1:1)=' '
           data(nr)(80:80)='3'
c
c   if comments left, put on new line
c
           if(isc(ir)(91:96).ne.'      ') then
             nr=nr+1
             data(nr)(1:1)=' '
             data(nr)(2:7)=isc(ir)(91:96)
             do i=8,79
               data(nr)(i:i)=' '
             enddo
             data(nr)(80:80)='3'
           endif
      endif
c
c--------------------------------------------------
c  initial phase record
c--------------------------------------------------
c
      if(id.eq.5) then
         if(iphase.eq.0) then  ! write header line for first phase
            nr=nr+1
            write(data(nr),243)
 243        FORMAT(
     *     ' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO ',
c     *     'SNR AR TRES W  DIS CAZ7')
     *     'AIN AR TRES W  DIS CAZ7')
          endif
          iphase=iphase+1
c
         nr=nr+1
         do i=1,80
           data(nr)(i:i)=' '
         enddo
         data(nr)(2:5)=isc(ir)(11:14)               ! station
         if(isc(ir)(11:14).ne.oldstat) then
            nstat=nstat+1                        ! count stations
            oldstat=isc(ir)(11:14)
         endif
         data(nr)(8:8)=isc(ir)(70:70)               ! component
         read(isc(ir)(61:63),'(i3)',err=8000) isc_phase      ! isc numeric code
         phase=' '
c
c  select type and weight of phase
c 
         phase=isc(ir)(49:56)           ! user phase by default
         if(phase_select.gt.1) then     ! isc phase if defined
             if(isc_phase.le.100) then  ! isc defined
                phase(1:7)=piphas(isc_phase)
             endif
         endif
c
c  only isc phase, rest will get zero weight
c
         if(phase_select.eq.2) then     ! only isc 1. arrival
             if(isc_phase.gt.100) then
                if(phase(5:5).eq.' ') then
                   phase(5:5)='4'          ! weight in phase name
                else
                   data(nr)(9:9)='4'       ! weigh in alternativ position
                endif
              endif
         endif
c
c   check if any parenthesis, remove if there 
c
         k=1
         phase_out='       '
         do i=1,8
           if(phase(i:i).ne.'('.and.phase(i:i).ne.')') then
              phase_out(k:k)=phase(i:i)
              k=k+1
           endif
         enddo
         phase=phase_out
c
c   remove any phase id behind /
c

         k=index(phase,'/')
         if(k.gt.0) then
            do i=k,8
              phase(i:i)=' '
            enddo
         endif           
c
         data(nr)(10:10)=isc(ir)(71:71)             ! onset
         data(nr)(11:18)=phase
         data(nr)(19:22)=isc(ir)(36:39)             ! hour min
c
c   check if phase is from same day as origin time
c
         read(isc(ir)(34:35),'(i2)',err=8000) phase_day
         if(phase_day.ne.origin_day.and.phase_day.ne.0) then
           if(phase_day.eq.origin_day+1) then
              read(data(nr)(19:20),'(i2)',err=8000) phase_hour
              phase_hour=phase_hour+24
              write(data(nr)(19:20),'(i2)') phase_hour
              if(debug.eq.1) write(27,'(a,a,a)')
     *        ' add 24 hrs',data(1)(1:45), data(nr)(1:8)
           else
              if(debug.eq.1) then 
                 write(27,'(a,a,a)')
     *           ' error time',data(1)(1:45), data(nr)(1:8)
                 write(27,*)'phase_day',phase_day
                 write(27,*)'origin_day',origin_day
               endif
           endif
         endif
         if(isc(ir)(40:43).ne.'    ') then
            read(isc(ir)(40:43),'(f4.2)',err=8000) sec
            write(data(nr)(24:28),'(f5.2)') sec
         endif
c
c   residual
c
         if(isc(ir)(64:67).ne.'    '.and.isc(ir)(64:67).ne.
     &       '9999') then
            read(isc(ir)(64:67),'(f4.1)',err=8000) residual
            write(data(nr)(64:68),'(f5.1)') residual
         endif
c
c  amplitude and period
c
         if(isc(ir)(78:81).ne.'   '.and.isc(ir)(78:81).ne.'9999') then
            aperiod='    '
            read(isc(ir)(78:81),'(f4.3)',err=8000) amp  ! mantissa
            read(isc(ir)(82:83),'(i2)',err=8000) iamp   ! exponent
            amp=amp*(10**iamp)              ! amp in nm
            aperiod='    '
            if(isc(ir)(86:89).ne.'    ') then
               read(isc(ir)(86:89),'(f4.1)',err=8000) period
               write(aperiod,'(f4.1)') period
            endif
            if(amp.lt.100000.)                                                 
     +      write(data(nr)(34:45),'(f7.1,1x,a4)')
     +      amp,aperiod
            if(amp.ge.100000.)then
               amp=amp/10000.0                 
               write(data(nr)(34:45),'(f5.1,a,1x,a4)')
     +         amp,'e4',aperiod
            endif
            if(amp.ge.10000000.)then
               amp=amp/1000000.0                 
               write(data(nr)(34:45),'(f5.1,a,1x,a4)')
     +         amp,'e6',aperiod
            endif
          endif
c
c   distance
c
          distance=0.0
          if(isc(ir)(26:30).ne.'     ') then
             read(isc(ir)(26:30),'(f5.2)',err=8000) distance
             distance=distance*111.195 +0.5  ! convert to km, round off
             if(distance.gt.max_distance) max_distance=distance
             if(distance.lt.min_distance) min_distance=distance
             write(data(nr)(71:75),'(i5)') int(distance)
          endif
c
c   polarity
c
         if(isc(ir)(68:68).eq.'+'.or.isc(ir)(68:68).eq.'1'.or.
     *      isc(ir)(68:68).eq.'A'.or.isc(ir)(68:68).eq.'C')
     *      data(nr)(17:17)='C'
         if(isc(ir)(68:68).eq.'-'.or.isc(ir)(68:68).eq.'2'.or.
     *      isc(ir)(68:68).eq.'K'.or.isc(ir)(68:68).eq.'D')
     *      data(nr)(17:17)='D'
      endif
c
c      
c--------------------------------------
c    supplementry phase line
c--------------------------------------
c
      if(id.eq.6) then
         nr=nr+1
         do i=1,80
           data(nr)(i:i)=' '
         enddo
         data(nr)(2:5)=data(nr-1)(2:5)           ! station, same as before
         data(nr)(8:8)=isc(ir)(49:49)               ! component
         data(nr)(10:10)=isc(ir)(50:50)             ! onset
         read(isc(ir)(40:42),'(i3)',err=8000) isc_phase      ! isc numeric code
         phase=' '

c
c  select type and weight of phase
c 
         phase=isc(ir)(28:35)                       ! users phase
         if(phase_select.eq.3) then     ! isc first arrival and user 2. arrival
             if(isc_phase.le.100) then  ! isc defined
                phase(1:7)=piphas(isc_phase)
             endif
         endif
c
c  only isc first arrival, rest will get zero weight
c
         if(phase_select.eq.2) then     ! only isc 1. arrival
             if(isc_phase.gt.100) then
                if(phase(5:5).eq.' ') then
                   phase(5:5)='4'          ! weight in phase name
                else
                   data(nr)(9:9)='4'       ! weigh in alternativ position
                endif
              endif
         endif
c
c   check if any parenthesis, remove if there 
c
         k=1
         phase_out='       '
         do i=1,8
           if(phase(i:i).ne.'('.and.phase(i:i).ne.')') then
              phase_out(k:k)=phase(i:i)
              k=k+1
           endif
         enddo
         phase=phase_out
c
c   remove any phase id behind /
c

         k=index(phase,'/')
         if(k.gt.0) then
            do i=k,8
              phase(i:i)=' '
            enddo
         endif           
c
         data(nr)(11:18)=phase
         data(nr)(19:22)=isc(ir)(15:18)             ! hour min
         if(isc(ir)(19:22).ne.'    ') then
            read(isc(ir)(19:22),'(f4.2)',err=8000) sec
            write(data(nr)(24:28),'(f5.2)') sec
         endif
c
c   check if phase is from same day as origin time
c
         read(isc(ir)(13:14),'(i2)',err=8000) phase_day
         if(phase_day.ne.origin_day.and.phase_day.ne.0) then
           if(phase_day.eq.origin_day+1) then
              read(data(nr)(19:20),'(i2)',err=8000) phase_hour
              phase_hour=phase_hour+24
              write(data(nr)(19:20),'(i2)') phase_hour
              if(debug.eq.1) write(27,'(a,a,a)')
     *        ' add 24 hrs',data(1)(1:45), data(nr)(1:8)
           else
              if(debug.eq.1) then 
                write(27,'(a,a,a)') 
     *        ' error time',data(1)(1:45), data(nr)(1:8)
                 write(27,*)'phase_day',phase_day
                 write(27,*)'origin_day',origin_day
              endif
           endif
         endif
c
c   polarity
c
         if(isc(ir)(47:47).eq.'+'.or.isc(ir)(47:47).eq.'1'.or.
     *      isc(ir)(47:47).eq.'A'.or.isc(ir)(47:47).eq.'C')
     *      data(nr)(17:17)='C'
         if(isc(ir)(47:47).eq.'-'.or.isc(ir)(47:47).eq.'2'.or.
     *      isc(ir)(47:47).eq.'K'.or.isc(ir)(47:47).eq.'D')
     *      data(nr)(17:17)='D'
c
c   residual
c
         if(isc(ir)(43:46).ne.'    '.and.isc(ir)(43:46).ne.'9999') then
            read(isc(ir)(43:46),'(f4.1)',err=8000) residual
            write(data(nr)(64:68),'(f5.1)') residual
         endif
c
c  amplitude and period
c
         if(isc(ir)(57:60).ne.'   '.and.isc(ir)(57:60).ne.'9999') then
            aperiod='    '
            read(isc(ir)(57:60),'(f4.3)',err=8000) amp  ! mantissa
            read(isc(ir)(61:62),'(i2)',err=8000) iamp   ! exponent
c peter voss 2009-05-06 correction :
c           read(isc(ir)(63:64),'(i2)',err=8000) iamp   ! exponent
            amp=amp*(10**iamp)              ! amp in nm
            aperiod='    '
            if(isc(ir)(65:68).ne.'    ') then
               read(isc(ir)(65:68),'(f4.1)',err=8000) period
               write(aperiod,'(f4.1)') period
            endif
            if(amp.lt.100000.)                                                 
     +      write(data(nr)(34:45),'(f7.1,1x,a4)')
     +      amp,aperiod
            if(amp.ge.100000.)then
               amp=amp/10000.0                 
               write(data(nr)(34:45),'(f5.1,a,1x,a4)')
     +         amp,'e4',aperiod
            endif
            if(amp.ge.10000000.)then
               amp=amp/1000000.0                 
               write(data(nr)(34:45),'(f5.1,a,1x,a4)')
     +         amp,'e6',aperiod
            endif
          endif
c
          if(distance.ne.0.0)write(data(nr)(71:75),'(i5)')int(distance)
      endif
c
c   next input
c
      ir=ir+1
      if(ir.gt.nrecord) then        ! end of event
          if(nstat.gt.1000) then
             if(debug.eq.1) write(27,*) 'nstat',nstat
             nstat=999                  ! there is only room for 3 digits
          endif
          if(nstat.gt.0) write(data(1)(49:51),'(i3)') nstat ! number of stations
c
c   find which distance for event, if no distance, leave event as
c   distant
c
         ev_type='D'
         if(max_distance.lt.3000.0.and.max_distance.ge.1000.0)
     *   ev_type='R'   ! regional
         if(max_distance.lt.1000.0.and.max_distance.gt.0.0)
     *   ev_type='L'   ! local
         do i=1,ihead
            if(data(i)(80:80).eq.'1') data(i)(22:22)=ev_type
         enddo
c
c   last blank line
c
         nr=nr+1
         do i=1,80
           data(nr)(i:i)=' '
         enddo
      else
         goto 1        ! get next line
      endif
c
c----------------------------------
c   check for magnitude selection
c----------------------------------
c
          if(minmag.ne.-99.and.maxmag.ne.99) then
                if((mag1.lt.minmag.and.mag2.lt.minmag)
     *         .or.(mag1.gt.maxmag.and.mag2.gt.maxmag)) then
                   select=.false.                        ! magnitude outside
                   return
                endif
          endif
      goto 8001

 8000 continue
      write(6,*)' Error in input data, event skipped'
      select=.false.
c
 8001 continue
      return
      end
cccccccccccccccccccccccccccccc
 
      subroutine read_one_isc(unit,isc,nrecord,nhead)
c
c   unit: unit to read from
c   isc : isc data lines
c   nrecord: number of lines one event
c   nhead: number of headers one isc event
c
c   reads one isc event in isc format, if nrecord=0 or nhead=0, end of file
c
      implicit none
      integer nhead              ! number if isc type 1 or 2 headers
      integer nrecord            ! number of records for event
      character*96 isc(*)        ! event records
      character*96 epihead(50,300)! lines for each estimate
      integer np(100)            ! # lines each estimate
      logical header_prime       ! true if prime header read
      integer unit               ! unit to read from
      integer nepi               ! number of estimates
      integer id                 ! line type
      integer i,k,l
      
      nhead=0
      nrecord=1
      header_prime=.false.
      nepi=0
      do i=1,50
        np(i)=0
      enddo
      
c
c   read loop
c
 1    continue
      read(unit,'(a)',end=99,err=99) isc(nrecord)
      read(isc(nrecord)(1:2),'(i2)',err=99) id
      if(id.gt.0.and.id.lt.5) then         ! a header record
c
c  catch a comment line within phases, throw it out
c
         if(header_prime.and.id.eq.3) goto 1
         if(id.eq.1) then
            if(header_prime) then          ! this was into next event
               backspace unit
               goto 99
            endif
            nepi=nepi+1                    ! a new epicenter estimate
            if(isc(nrecord)(26:26).eq.'A') then
               header_prime=.true.
            endif
         endif
         if(nepi.eq.0) then
           write(6,*)' No prime'
           nepi=1
         endif
c         write(6,*) id,nepi
         np(nepi)=np(nepi)+1               ! count lines for one estimate
         epihead(nepi,np(nepi))=isc(nrecord)  ! save estimate
         nhead=nhead+1
      endif
      nrecord=nrecord+1
      goto 1
c
c
 99   continue
      nrecord=nrecord-1
c
c  return if no data
c
      if(nrecord.eq.0) return
c
c  put prime estimates on top,  assume prime estimate is last
c
      l=1
      do k=nepi,1,-1
         do i=1,np(k)
           isc(l)=epihead(k,i)
           l=l+1
         enddo
      enddo
c
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine setfis

c set up ISC phase identifications with upper and lower case

      common /piphas2/ piphas (0:100)
      character*7 piphas
      
      piphas(  0) = 'P      '
      piphas(  1) = 'PP     '
      piphas(  2) = 'PPP    '
      piphas(  3) = 'PcP    '
      piphas(  4) = 'PKP    '
      piphas(  5) = 'PKP2   '
      piphas(  6) = 'PKPPKP '
      piphas(  7) = 'PcPPKP '
      piphas(  8) = 'PS     '
      piphas(  9) = 'PPS    '
      piphas( 10) = 'PcS    '
      piphas( 11) = 'PKS    '
      piphas( 12) = 'PKKS   '
      piphas( 13) = 'PcSPKP '
      piphas( 14) = 'PKPPKS '
      piphas( 15) = 'PKPSKS '
      piphas( 16) = 'PKKP   '
      piphas( 17) = '3PKP   '
      piphas( 18) = 'PKIKP  '
      piphas( 19) = 'PP2    '
      piphas( 20) = 'PPP2   '
      piphas( 21) = 'PKS2   '
      piphas( 22) = 'PSS    '
      piphas( 23) = 'PSS2   '
      piphas( 24) = 'SSP2   '
      piphas( 25) = 'PcPPKP2'
      piphas( 26) = 'PcSPKP2'
      piphas( 27) = 'SS2    '
      piphas( 28) = 'PKKP2  '
      piphas( 29) = 'PKKS2  '
      piphas( 30) = 'ScSPKP3'
      piphas( 31) = 'ScSPKP2'
      piphas( 32) = 'ScSP2  '
      piphas( 33) = 'SKSP2  '
      piphas( 34) = 'SSS2   '
      piphas( 35) = 'S      '
      piphas( 36) = 'SS     '
      piphas( 37) = 'SSS    '
      piphas( 38) = 'ScS    '
      piphas( 39) = 'SKS    '
      piphas( 40) = 'SKKS   '
      piphas( 41) = 'SKKKS  '
      piphas( 42) = 'ScSPKP '
      piphas( 43) = 'SKSSKS '
      piphas( 44) = 'ScSP   '
      piphas( 45) = 'SKSP   '
      piphas( 46) = 'ScP    '
      piphas( 47) = 'SP     '
      piphas( 48) = 'SKP    '
      piphas( 49) = 'SKKP   '
      piphas( 50) = 'SKPPKP '
      piphas( 51) = 'SSP    '
      piphas( 52) = 'SKP2   '
      piphas( 53) = 'SKS2   '
      piphas( 54) = 'SKKS2  '
      piphas( 55) = 'SKKS3  '
      piphas( 56) = 'SKKKS2 '
      piphas( 57) = 'sPKP2  '
      piphas( 58) = 'pPcP   '
      piphas( 59) = 'pPKP   '
      piphas( 60) = 'pP     '
      piphas( 61) = 'pPP    '
      piphas( 62) = 'sP     '
      piphas( 63) = 'sPKP   '
      piphas( 64) = 'sS     '
      piphas( 65) = 'sSS    '
      piphas( 66) = 'sPP    '
      piphas( 67) = 'sPcP   '
      piphas( 68) = 'sScS   '
      piphas( 69) = 'pPKP2  '
      piphas( 70) = 'P*     '
      piphas( 71) = 'S*     '
      piphas( 72) = 'Pg     '
      piphas( 73) = 'Sg     '
      piphas( 74) = 'Pn     '
      piphas( 75) = 'Sn     '
      piphas( 76) = 'PgPg   '
      piphas( 77) = 'SgSg   '
      piphas( 78) = 'LR     '
      piphas( 79) = 'LQ     '
      piphas( 80) = 'L      '
      piphas( 81) = 'PKKP3  '
      piphas( 82) = 'PKKS3  '
      piphas( 83) = 'SPP    '
      piphas( 84) = 'Phase84'
      piphas( 85) = 'P diff '
      piphas( 86) = 'QM     '
      piphas( 87) = 'RM     '
      piphas( 88) = 'T      '
      piphas( 89) = 'T(Max) '
      piphas( 90) = 'North  '
      piphas( 91) = 'South  '
      piphas( 92) = 'East   '
      piphas( 93) = 'West   '
      piphas( 94) = 'Up     '
      piphas( 95) = 'Down   '
      piphas( 96) = 'e      '
      piphas( 97) = 'i      '
      piphas( 98) = 'Maximum'
      piphas( 99) = 'Final  '
      piphas(100) = '       '
      return
      end
