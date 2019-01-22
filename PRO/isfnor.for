
      program isfnor
c
c convert between ISF and Nordic format
c the program uses ISC library routines to read and write ISF,
c and the routines from rea.for to read and write Nordic files
c
c Lars Ottemoller, BGS, Edinburgh, UK
c 21.05.2003
c
c   changes
c
c  jun 23 2003 jh : add all1
c  jul  3 2003 lo : tested with BER 2002 data and sorted bugs
c  may 12 2009 lo : init rea_comp and changed az conversion isf->nordic
c  2013-06-212 pv : output strike from xy_ellipse is now in rad (and not deg), 
c                   so a change is added.
c

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock
      include 'isf_bul.h'                 ! isf header file
      include 'isfnor.inc'                ! isf variables

      real dist2deg

      integer seiclen
      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file

      logical all,all1                    ! true: read all data, false: headers
      integer code                        ! error return code
      integer recognized                  ! return code
      integer nevent                      ! number of events in file
      integer norigin                     ! counter for origin
      integer narr                        ! counter for arrivals
      integer i,j,ind                     ! counter
      integer stdout                      ! 3 to file, 6 to screen
      logical flag,origin_flag(100),maghead_flag,hyp_flag,
     &    out_flag

      integer choice                      ! 1: nor-isf, 2: isf-nor
      character*80 outfile,text
c-- network code
      character*5 net_code
      character*29 mainhead_text

      stdout=6   ! this can be changed to 3, for output to file isfnor.log

c
c   get def file for station codes, give file name
c
      text='isfnor.def'
      call read_def_chan(text,mainhead_text,net_code)

 1    continue
      write(*,*) ' Convert (1) Nordic -> ISF or (2) ISF -> Nordic ' 
      read(5,'(i1)') choice
      if (choice.ne.1.and.choice.ne.2) goto 1

      if (choice.eq.1) outfile = 'norisf.out'
      if (choice.eq.2) outfile = 'isfnor.out'

c
c   open output file
c
      open(2,file=outfile,status='unknown')
      open(3,file='isfnor.log',status='unknown')
    
c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input file'
      read(5,'(a)') infile
      if (seiclen(infile).le.0) stop
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

      if (choice.eq.2) goto 500
c
c Nordic to ISF
c
  40  continue
      write(*,*) ' converting Noridc to ISF '
c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c
      code=write_data_type(2,'BULLETIN','','IMS1.0','')

  50  continue
c
c   read all parameters for one event form file unit 1
c
      call rea_event_in(1,all,data,code)
      write(stdout,'(a78)') data(1)(2:79)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      hyp_flag=.false.
      if (hyp_lat(1).gt.-900.) hyp_flag=.true.
c      if (hyp_flag) then
        nevent=nevent+1               ! count events
        write(evid,'(i8)') nevent
c      write(stdout,*)' Number of headers and number of records',
c     *rea_nhead,rea_nrecord

        code=write_event_id(2,evid,'')
        if (code.ne.0) then
          write(stdout,'(a)') 'write_event_id: '//isf_bulletin_error
     &          (1:seiclen(isf_bulletin_error))
        endif
c      endif
      norigin=0
c
c write origin
c
      do i=1,rea_nhyp
        flag=.true.
        origin_flag(i)=.false.
        call nullify_origin(
     &       yyyy,mm,dd,hh,mi,ss,msec,
     &       timfix,stime,sdobs,
     &       lat,lon,epifix,
     &       smaj,smin,strike,
     &       depth,depfix,
     &       sdepth,ndef,nsta,gap,
     &       mindist,maxdist,antype,loctype,
     &       etype,author,origid)
        
        yyyy=hyp_year(i)
        mm=hyp_month(i)
        dd=hyp_day(i)
        hh=hyp_hour(i)
        mi=hyp_min(i)
        ss=int(hyp_sec(i))
        if (hh.eq.-999.or.mi.eq.-999) then
          if (i.eq.2) then
            hh=hyp_hour(1)
            mi=hyp_min(1)
            ss=int(hyp_sec(1))
            hyp_sec(i)=hyp_sec(1)
          else
            flag=.false.
          endif 
        endif
        if (hh.ge.24.or.mi.eq.60.or.ss.eq.60) then
          call timsec(yyyy,mm,dd,hh,mm,ss,dpsec)
          call sectim(dpsec,yyyy,doy,mm,dd,hh,mm,ss)
        endif
        msec=int(1000.*(hyp_sec(i)-int(hyp_sec(i))))
        if (hyp_rms(i).ge.0.) then
          sdobs=hyp_rms(i)
        else
          sdobs=ISF_NULL
        endif
        if (hyp_epi_flag(i).eq.'F') then
          epifix='f'  
        else
          epifix=' '  
        endif
        if (hyp_depth_flag(i).eq.'F') then
          depfix='f'  
        else
          depfix=' ' 
        endif
        if (hyp_fix_org(i).eq.'F') then
          timfix='f'  
        else
          timfix=' ' 
        endif
        if (hyp_type(i).eq.'P') then
          etype='sh'
        elseif (hyp_type(i).eq.'E') then
          etype='kh'
        endif
        author=hyp_agency(i)
        if (seiclen(author).le.0) author='unknown'
        if (i.eq.1) then
          mindist=99999.
          maxdist=0.
          if (hyp_flag) then
            do j=1,rea_nphase
              dist=dist2deg(rea_dist(j),hyp_lat(1),hyp_lon(1))
              if (dist.gt.maxdist) maxdist=dist
              if (dist.lt.mindist) mindist=dist
            enddo
          endif
          if (mindist.ge.99999..or.mindist.le.0.) mindist=ISF_NULL
          if (maxdist.le.0.) maxdist=ISF_NULL
          stime=hyp_sec_err(i)
          sdepth=hyp_depth_err(i)
          if (hyp_lon_err(i).gt.0.) then
            call xy_ellipse(hyp_lon_err(i),hyp_lat_err(i),
     &       hyp_depth_err(i),hyp_cov(1,i),hyp_cov(2,i),
     &       hyp_cov(3,i),smaj,smin,rstrike)
c     convert to deg:
            rstrike=rstrike*360./(2.*3.141592654)
c     strike cannot be negative in isf
            if(rstrike.lt.0.0)rstrike=rstrike+360.0
            strike=int(rstrike)
            if (smaj.gt.999..or.smin.gt.999) then
              smaj=ISF_NULL
              smin=ISF_NULL
              rstrike=ISF_NULL
            endif
          endif
        else
          maxdist=ISF_NULL
          mindist=ISF_NULL
          stime=ISF_NULL
          sdepth=ISF_NULL
        endif
        if (sdepth.gt.99..or.sdepth.lt.0.) sdepth=ISF_NULL
        if (stime.gt.999.) stime=ISF_NULL
        if (hyp_lat(i).eq.-999.) then
          flag=.false.
        else
          lat=hyp_lat(i)
          lon=hyp_lon(i)
          depth=hyp_depth(i)
        endif
        nsta=hyp_nstat(i)
        gap=int(hyp_gap(i))
        norigin=norigin+1
        write(origid,'(i8)') norigin
        origin_flag(i)=.true.
        if (i.eq.1) then
             code=write_origin_head(2)
             if (code.ne.0) 
     &         write(stdout,'(a)') 'write_origin_head: '//
     &           isf_bulletin_error
     &          (1:seiclen(isf_bulletin_error))
        endif
        if (flag) then
          code=write_origin(2,
     &       yyyy,mm,dd,hh,mi,ss,msec,
     &       timfix,stime,sdobs,
     &       lat,lon,epifix,
     &       smaj,smin,strike,
     &       depth,depfix,
     &       sdepth,ndef,nsta,gap,
     &       mindist,maxdist,antype,loctype,
     &       etype,author,origid)
             if (code.ne.0) 
     &       write(stdout,'(a)') 'write_origin: '//isf_bulletin_error
     &          (1:seiclen(isf_bulletin_error))
             if (norigin.eq.1) then
               code=write_origin_prime(2)
               if (code.ne.0) 
     &           write(stdout,'(a)') 'write_origin_prime: '//
     &             isf_bulletin_error(1:seiclen(isf_bulletin_error))
             endif
        else
c
c write origin line without hypocenter
c 
          code=write_origin_nohyp(2,
     &       yyyy,mm,dd,hh,mi,ss,msec,
     &       timfix,stime,sdobs,
     &       lat,lon,epifix,
     &       smaj,smin,strike,
     &       depth,depfix,
     &       sdepth,ndef,nsta,gap,
     &       mindist,maxdist,antype,loctype,
     &       etype,author,origid)
             if (code.ne.0)
     &       write(stdout,'(a)') 'write_origin_nohyp: '//
     &         isf_bulletin_error
     &          (1:seiclen(isf_bulletin_error))
             if (norigin.eq.1) then
               code=write_origin_prime(2)
               if (code.ne.0)
     &           write(stdout,'(a)') 'write_origin_prime: '//
     &             isf_bulletin_error(1:seiclen(isf_bulletin_error))
             endif
        endif
      enddo
c
c write fault plane solutions
c      
      do i=1,rea_nfault
        if (i.eq.1) code=write_fault_plane_head(2)
        read(rea_fault(i),'(1x,3f10.1)') fpstrike,fpdip,fprake
        code=write_fault_plane(2,' ',fpstrike,fpdip,
     &    fprake,ISF_NULL,ISF_NULL,' ',author)
        if (code.ne.0)
     &    write(stdout,'(a)') 'write_fault_plane: '//
     &      isf_bulletin_error(1:seiclen(isf_bulletin_error))

      enddo

c
c write magnitudes
c
      norigin=0
      maghead_flag=.false.
      do i=1,rea_nhyp
        if (origin_flag(i)) then
          norigin=norigin+1
          do j=1,6
            write(origid,'(i8)') norigin
            if (hyp_mag(j,i).ge.-900..and.
     &        seiclen(hyp_mag_type(j,i)).gt.0) then
              if (.not.maghead_flag) then
                maghead_flag=.true.
                code=write_netmag_head(2)
                if (code.ne.0) 
     &             write(stdout,'(a)') 'write_netmag_head: '//
     &             isf_bulletin_error(1:seiclen(isf_bulletin_error))
              endif
              mag_type=' '
              mag_type='M'//hyp_mag_type(j,i)
              code=write_netmag(2,mag_type,' ',
     &          hyp_mag(j,i),float(ISF_NULL),ISF_NULL,
     &          hyp_mag_agency(j,i),origid)
            endif
          enddo
        endif
      enddo

c
c write macroseismic data
c
      do i=1,rea_nmacro
        if (i.eq.1) then
          code = write_effects_head(2)
          if (code.ne.0)
     &      write(stdout,'(a)') 'write_effects_head: '//
     &        isf_bulletin_error(1:seiclen(isf_bulletin_error))
        endif
        felt='F' 
        casualties='_'
        damage='_'
        heard='_'
        if (rea_macro(i)(25:25).eq.'C') then
          casualties='C'
        elseif (rea_macro(i)(25:25).eq.'D') then
          damage='D'
        elseif (rea_macro(i)(25:25).eq.'H') then
          heard='H'
        endif
        uplift='_'
        subsidence='_'
        fault='_'
        if (rea_macro(i)(22:22).eq.'F') then
          fault='F'
        elseif (rea_macro(i)(22:22).eq.'U') then
          uplift='U'
          subsidence='S'
        elseif (rea_macro(i)(22:22).eq.'F') then
          fault='F'
          uplift='U'
          subsidence='S'
        endif
        tsunami='_'
        if (rea_macro(i)(23:23).ne.' ') then
          tsunami=rea_macro(i)(23:23)
        endif
        seiche='_'
        if (rea_macro(i)(24:24).ne.' ') then
          seiche=rea_macro(i)(24:24)
        endif
        liquification='_'
        geyser='_'
        landslide='_'
        sandblow='_'
        cracks='_'
        lights='_'
        if (rea_macro(i)(26:26).eq.'L') then
          liquification='L'
        elseif (rea_macro(i)(26:26).eq.'G') then
          geyser='G'
        elseif (rea_macro(i)(26:26).eq.'S') then
          landslide='S'
        elseif (rea_macro(i)(26:26).eq.'B') then
          sandblow='B'
        elseif (rea_macro(i)(26:26).eq.'C') then
          cracks='C'
        elseif (rea_macro(i)(26:26).eq.'V') then
          lights='V'
        endif
c        mlat=float(ISF_NULL)
c        mlon=float(ISF_NULL)
        mlat=0.
        mlon=0.
        mloctype='LatLon'
        if (rea_macro(i)(34:39).ne.'      ') then
          read(rea_macro(i)(34:47),'(f6.2,1x,f7.2)')
     &       mlat,mlon
        endif
        intensity1=float(ISF_NULL)
        if (rea_macro(i)(28:29).ne.'  ') then
          read(rea_macro(i)(28:29),'(f2.0)') intensity1
          modifier=rea_macro(i)(30:30)
          scale=rea_macro(i)(31:32)
        endif  
        if (rea_macro(i)(73:75).ne.'   ') then
          author=rea_macro(i)(73:75)
        else
          author=hyp_agency(1)
        endif
        if (seiclen(author).le.0) author='unknown'

        code = write_effects(2,heard,felt,damage,casualties,uplift,
     &     subsidence,fault,tsunami,
     &     seiche,'_','_','_',
     &     '_',liquification,geyser,landslide,sandblow,cracks,lights, 
     &     '_',mloctype,mlat,mlon,
     &     float(ISF_NULL),float(ISF_NULL),'na','na','_','_',
     &     intensity1,modifier,float(ISF_NULL),scale,author) 
          if (code.ne.0)
     &      write(stdout,'(a)') 'write_effects: '//
     &        isf_bulletin_error(1:seiclen(isf_bulletin_error))
      enddo

c
c write phases
c
c      if (hyp_flag) then  ! only if hypocenter given in 1st line
        narr=0
        author=hyp_agency(1)
        if (author.eq.' ') author='na'
        do i=1,rea_nphase
          call nullify_phase(
     &      sta,dist,esaz,phase,hh,mi,ss,msec,
     &      timeres,azim,azimres,slow,
     &      slowres,timedef,azimdef,slowdef,
     &      snr,amp,per,picktype,
     &      sp_fm,detchar,magtype,magind,mag,arrid) 
          narr=narr+1
          write(arrid,'(i8)') narr
          sta=rea_stat(i)
          if (rea_dist(i).gt.0.) then
            dist=dist2deg(rea_dist(i),hyp_lat(1),hyp_lon(1))
            if (dist.eq.0.) dist=float(ISF_NULL)
          endif
          if (rea_az(i).ge.0.) azim=rea_az(i) ! check this
c        if (rea_baz_obs(i).gt.-999.) azim=rea_baz_obs(i) ! check this
c        if (rea_baz_res(i).gt.-999.) azimres=rea_baz_res(i)
          phase=rea_phase(i)
          hh=rea_hour(i)
          mi=rea_min(i)
          ss=rea_sec(i)
          if (hh.ge.24.or.mi.eq.60.or.ss.eq.60) then
            call timsec(yyyy,mm,dd,hh,mm,ss,dpsec)
            call sectim(dpsec,yyyy,doy,mm,dd,hh,mm,ss)
          endif
          msec=int(1000.*(rea_sec(i)-int(rea_sec(i))))
          timeres=rea_res(i)
          azim=float(ISF_NULL)    ! in Nordic format back azimuth is used
          if (rea_vel(i).gt.0.) 
     &    slow=1./rea_vel(i)      ! in Nordic format app velocity
          amp=rea_amp(i)
          per=rea_per(i)
 
          if (i.eq.1) then
            code=write_phase_head(2)
            if (code.ne.0)
     &        write(stdout,'(a)') 'write_phase_head: '//
     &        isf_bulletin_error(1:seiclen(isf_bulletin_error))
          endif  
  
          code=write_phase(2,
     &      sta,dist,esaz,phase,hh,mi,ss,msec,
     &      timeres,azim,azimres,slow,
     &      slowres,timedef,azimdef,slowdef,
     &      snr,amp,per,picktype,
     &      sp_fm,detchar,magtype,magind,mag,arrid) 
            if (code.ne.0)
     &        write(stdout,'(a)') 'write_phase: '//
     &        isf_bulletin_error(1:seiclen(isf_bulletin_error))
          value=' '
          if (rea_coda(i).gt.0.) then
            write(value,'(f6.1)') rea_coda(i)
            do j=1,6
              if (value(j:j).eq.' ') value(j:j)='0'
            enddo
            code=write_phase_measure(2,'DURATION',value,' ',1)
            if (code.ne.0)
     &        write(stdout,'(a)') 'write_phase_measure: '//
     &        isf_bulletin_error(1:seiclen(isf_bulletin_error))
          endif
        enddo
c      endif ! flag
c
c write additional phase info in sub block
c
c      if (.not.hyp_flag) then
c        code=write_data_type(2,'ARRIVAL','','IMS1.0','')
c      endif
      narr=0
      do i=1,rea_nphase
        narr=narr+1
        write(arrid,'(i8)') narr
        chan='   '
        write(chan,'(a1,a1,a1)') rea_co(i)(1:1),'H',rea_co(i)(2:2)
        yyyy=hyp_year(1)
        mm=hyp_month(1)
        dd=hyp_day(1)
        if (rea_year(i).gt.0.) then
          yyyy=rea_year(i)
          mm=rea_month(i)
          dd=rea_day(i)
        endif
c        if (rea_onset(i).ne.' ') then
c          phase=rea_onset(i)//rea_phase(i)
c        else 
          phase=rea_phase(i)
c        endif
        hh=rea_hour(i)
        mi=rea_min(i)
        ss=rea_sec(i)
c
c change date if hh gt 24
c
        if (hh.ge.24) then
          call timsec(yyyy,mm,dd,hh,mm,ss,dpsec)
          call sectim(dpsec,yyyy,doy,mm,dd,hh,mm,ss)
        endif
        msec=int(1000.*(rea_sec(i)-int(rea_sec(i))))

        if (i.eq.1) then
            code=write_phase_info_head(2)
            if (code.ne.0)
     &        write(stdout,'(a)') 'write_phase_info_head: '//
     &        isf_bulletin_error(1:seiclen(isf_bulletin_error))
        endif  
        code=write_phase_info(2,' ',chan,' ',float(ISF_NULL),
     &      float(ISF_NULL),' ',yyyy,mm,dd,float(ISF_NULL),
     &      float(ISF_NULL),float(ISF_NULL),float(ISF_NULL),
     &      float(ISF_NULL),float(ISF_NULL),float(ISF_NULL),
     &      float(ISF_NULL),float(ISF_NULL),author,arrid)
      enddo

c
c   get next event
c
      goto 50
c
c     end of file
c


c
c ISF to Nordic
c
  500 continue
      write(*,*) ' converting ISF to Noridc '

      j=1
      do while (j.eq.1)
        out_flag=.true.
        rea_nhyp=0
        rea_nfault=0
        rea_nmacro=0
        rea_nphase=0
        rea_ncomment=0
        do i=1,100
          do j=1,6
            hyp_mag(j,i)=-999.
          enddo
        enddo
        call isf2nordic(1,j)
        if (j.eq.1) then
          all1=.true.
c write event
          write(*,*) ' writing event ' 
          call rea_event_out(2,all1,data,code)
          nevent=nevent+1
        endif
      enddo

c      out_flag=.false.
c 990  continue
c
c write out last event if in memory
c
c       all1=.true.
c      call rea_event_out(2,all1,data,code)

 1000 continue
c
      write(6,*)            ! blank line
      close(1)
      close(2)              ! close output file
      close(3)
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is '//outfile(1:seiclen(outfile))

      stop
      end

      real function dist2deg(distkm,lat,lon)
c
c convert distance in km to degrees      
c
      implicit none
      real distkm,lat,lon
      real pi,rad,deg,equrad,polrad,radius
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
 
      if (lat.le.-99..or.lon.le.-181) then
        dist2deg=0.
        return
      endif
      radius = (cos(lat)**2/equrad**2 +
     *sin(lat)**2/polrad**2)**(-.5)
      dist2deg=distkm/radius*deg
      return
      end
      
c
c copy of write_origin which works without hypocenter parameters
c
      integer function write_origin_nohyp(file,yyyy,mm,dd,hh,mi,ss,msec,
     + timfix,stime,sdobs,lat,lon,epifix,smaj,smin,strike,depth,
     + depfix,sdepth,ndef,nsta,gap,mindist,maxdist,antype,loctype,
     + etype,author,origid)

      integer file
      character author*(*), origid*(*), etype*(*)
      character*1 timfix, epifix, depfix, antype, loctype
      integer yyyy, mm, dd, hh, mi, ss, msec
      integer strike, ndef, nsta, gap
      real stime, sdobs, lat, lon, smaj, smin, depth, sdepth
      real mindist, maxdist

      include 'isf_bul.h'
      integer partline, check_prev_line_type, is_null, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: date. Char 11: space.
      if (is_null(real(yyyy)) .eq. 1) then
          isf_bulletin_error = 'missing year'
          write_origin = 20
          return
      end if

      if (yyyy .lt. 1000 .or. yyyy .gt. 9999) then
          write (isf_bulletin_error,"('bad year ',i4)") yyyy
          write_origin = 20
          return
      end if

      if (is_null(real(mm)) .eq. 1) then
          isf_bulletin_error = 'missing month'
          write_origin = 20
          return
      end if

      if (mm .lt. 1 .or. mm .gt. 12) then
          write (isf_bulletin_error,"('bad month ',i2)") mm
          write_origin = 20
          return
      end if

      if (is_null(real(dd)) .eq. 1) then
          isf_bulletin_error = 'missing day'
          write_origin = 20
          return
      end if

      if (dd .lt. 1 .or. dd .gt. 31) then
          write (isf_bulletin_error,"('bad day ',i2)") dd
          write_origin = 20
          return
      end if
      write (file,"(i4.4,'/',i2.2,'/',i2.2,' ',$)") yyyy,mm,dd

c     Chars 12-19: time.
      if (is_null(real(hh)) .eq. 1) then
          isf_bulletin_error = 'missing year'
          write_origin = 20
          return
      end if

      if (hh .lt. 0 .or. hh .gt. 23) then
          write (isf_bulletin_error,"('bad hour ',i2)") hh
          write_origin = 20
          return
      end if

      if (is_null(real(mi)) .eq. 1) then
          isf_bulletin_error = 'missing minute'
          write_origin = 20
          return
      end if

      if (mi .lt. 0 .or. mi .gt. 59) then
          write (isf_bulletin_error,"('bad minute  ',i2)") mi
          write_origin = 20
          return
      end if

      if (is_null(real(ss)) .eq. 1) then
          isf_bulletin_error = 'missing second'
          write_origin = 20
          return
      end if

      if (ss .lt. 0 .or. ss .gt. 59) then
          write (isf_bulletin_error,"('bad second ',i2)") ss
          write_origin = 20
          return
      end if
      write (file,"(i2.2,':',i2.2,':',i2.2,$)") hh,mi,ss

c     Chars 20-22 msec - put blanks here if no msec provided.
      if (is_null(real(msec)) .eq. 1) then
          write(file,"('   ',$)")
      else
          if (msec .lt. 0 .or. msec .gt. 999) then
              write (isf_bulletin_error,"('bad msec ',i3)") msec
              write_origin = 20
              return
          end if
          write(file,"('.',i2.2,$)") msec/10
      end if

c     Char 23: fixed time flag. Char 24: space.
      if (timfix .ne. ' ' .and. timfix .ne. 'f') then
          write (isf_bulletin_error,"('bad timfix: ',a1)") timfix
          write_origin = 20
          return
      end if
      write(file,"(a1,' ',$)") timfix

c     Chars 25-29: optional origin time error. Char 30: space.
c     Give at least 2 decimal places but less if number > 99.
      write(file,"('      ',$)")

c     31-35: optional rms (sdobs). Char 36: space.
c     Give 2 decimal places but less if number > 99.
      write(file,"('      ',$)")

c     37-44: lattitude. Char 45: space.
      write (file,"('         ',$)") 

c     Chars 46-54: longitude.
      write (file,"('         ',$)") 

c     Char 55: fixed epicentre flag.
      if (epifix .ne. ' ' .and. epifix .ne. 'f') then
          write (isf_bulletin_error,"('bad epifix: ',a1)") epifix
          write_origin = 20
          return
      end if
      write(file,"(a1,$)") epifix

c     Char 56 should be a space but then can't have 5 digit smaj.
c     Chars 56-60: optional semi-major axis. Char 61: space.
c     Give 1 decimal place but less if number > 999.
      write (file,"('      ',$)")

c     Chars 62-66: optional semi-minor axis. Char 67: space.
c     Give 1 decimal place but less if number > 999.
      write (file,"('      ',$)")

c     Chars 68-70: optional strike. Char 71: space.
c     Strike can be -1, when it's a flag to signify that smaj,smin
c     are really slat,slon.
      write (file,"('    ',$)")

c     Chars 72-76: optional depth.
      write (file,"('     ',$)")

c     Char 77: fixed depth flag. Char 78: space.
      if (depfix .ne. ' ' .and. depfix .ne. 'f' .and. depfix .ne. 'd')
     +then
          write (isf_bulletin_error,"('bad depfix: ',a1)") depfix
          write_origin = 20
          return
      end if
      write(file,"(a1,' ',$)") depfix

c     Chars 79-82: optional depth error. Char 83: space.
c     Give 1 decimal place or 0 if number > 99.
      write (file,"('     ',$)")

c     Chars 84-87: optional ndef. Char 88: space.
      if (is_null(real(ndef)) .eq. 1) then
          write (file,"('     ',$)")
      else
          if (ndef .lt. 0 .or. ndef .gt. 9999) then
              write (isf_bulletin_error,"('bad ndef: ',f8.2)") ndef
              write_origin = 20
              return
          end if
          write (file,"(i4,' ',$)") ndef
      end if

c     Chars 89-92: optional nsta. Char 93: space.
      if (is_null(real(nsta)) .eq. 1) then
          write (file,"('     ',$)")
      else
          if (nsta .lt. 0 .or. nsta .gt. 9999) then
              write (isf_bulletin_error,"('bad nsta: ',f8.2)") nsta
              write_origin = 20
              return
          end if
          write (file,"(i4,' ',$)") nsta
      end if

c     Chars 94-96: optional gap. Char 97: space.
      write (file,"('    ',$)")

c     Chars 98-103: optional minimum distance. Char 104: space.
c     Gives 2 decimal places or less if number > 999.
      write (file,"('       ',$)")

c     Chars 105-110: optional maximum distance. Char 111: space.
c     Gives 2 decimal places or less if number > 999.
      write (file,"('       ',$)")

c     Char 112: analysis type. Char 113 space.
      if (antype .ne. ' ' .and. antype .ne. 'a' .and.
     +    antype .ne. 'm' .and. antype .ne. 'g') then

          write (isf_bulletin_error,"('bad antype: ',a1)") antype
          write_origin = 20
          return
      end if
      write(file,"(a1,' ',$)") antype

c     Char 114: location method. Char 115 space.
      if (loctype .ne. ' ' .and. loctype .ne. 'i' .and. loctype .ne.
     +    'p' .and. loctype .ne. 'g' .and. loctype .ne. 'o') then

          write (isf_bulletin_error,"('bad loctype: ',a1)") loctype
          write_origin = 20
          return
      end if
      write(file,"(a1,' ',$)") loctype

c     Chars 116-117: event type. Char 118 space.
      numchar = partline(substr,etype,0,0)
      if (numchar .eq. 0 ) then
          write(file,"('   ',$)")
      else
          if (numchar .ne. ISF_ETYPE_LEN) then
              write (isf_bulletin_error,"('bad etype: ',a)") etype
              write_origin = 20
              return
          end if
          write(file,"(a2,' ',$)") etype
      end if

c     Chars 119-127: author. Char 128: space.
      numchar = partline(substr,author,0,0)
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_bulletin_error,"('author too long: ',a)") author
          write_origin = 20
          return
      end if
      if (numchar .eq. 0) then
          write (isf_bulletin_error,"('missing author')")
          write_origin = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//author
          write_origin = 20
          return
      end if
      write(file,"(a9,' ',$)") author

c     Chars 129-136: origid.
      numchar = partline(substr,origid,0,0)
      if (numchar .gt. ISF_ORIGID_LEN) then
          write (isf_bulletin_error,"('origid too long: ',a)") origid
          write_origin = 20
          return
      end if
      if (numchar .eq. 0) then
          write (isf_bulletin_error,"('missing origid')")
          write_origin = 20
          return
      end if
      if ( check_whole(origid) .eq. 1 ) then
          isf_bulletin_error = 'bad origid: '//origid
          write_origin = 20
          return
      end if
      write(file,"(a8)") origid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin') .ne. 0) then
          write_origin = 10
          return
      end if

      write_origin_nohyp = 0
      return
      end

c
c read origin line without hypocenter
c
      integer function read_origin_nohyp(line,yyyy,mm,dd,hh,mi,ss,msec,
     + timfix,stime,sdobs,lat,lon,epifix,smaj,smin,strike,depth,
     + depfix,sdepth,ndef,nsta,gap,mindist,maxdist,antype,loctype,
     + etype,author,origid)

      character line*(*), author*(*), origid*(*), etype*(*)
      character*1 timfix, epifix, depfix, antype, loctype
      integer yyyy, mm, dd, hh, mi, ss, msec
      integer strike, ndef, nsta, gap
      real stime, sdobs, lat, lon, smaj, smin, depth, sdepth
      real mindist, maxdist

      include 'isf_bul.h'
      integer partline, check_int, atoi, isdigit
      integer check_real,check_whole
      real ator

      character substr*(ISF_LINE_LEN)

c     Chars 1-4: year.
      if (partline(substr,line,1,4) .eq. 0) then
          isf_bulletin_error = 'missing year: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
          isf_bulletin_error = 'bad year: '//line
          read_origin = 20
          return
      end if
      yyyy = atoi(substr)

c     Char 5: '/' character.
      if (line(5:5) .ne. '/') then
          isf_bulletin_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars  6-7: month.
      if (partline(substr,line,6,2) .eq. 0) then
          isf_bulletin_error = 'missing month: '//line
          read_origin = 20
          return
      end if
      if (check_int(substr) .eq. 1) then
        isf_bulletin_error = 'bad month: '//line
          read_origin = 20
          return
      end if
      mm = atoi(substr)

c     Char 8: '/' character.
      if (line(8:8) .ne. '/') then
          isf_bulletin_error = 'bad date: '//line
         read_origin = 20
          return
      end if

c     Chars  9-10: day.
      if (partline(substr,line,9,2) .eq. 0) then
          isf_bulletin_error = 'missing day: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
          isf_bulletin_error = 'bad day: '//line
          read_origin = 20
          return
      end if
      dd = atoi(substr)

c     Char 11: space.
      if (line(11:11) .ne. ' ') then
          isf_bulletin_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars  12,13: hour.
      if (partline(substr,line,12,2) .eq. 0) then
          isf_bulletin_error = 'missing hour: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
        isf_bulletin_error = 'bad hour: '//line
          read_origin = 20
          return
      end if
      hh = atoi(substr)

c     Char 14:  ':' character.
      if (line(14:14) .ne. ':') then
          isf_bulletin_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars 15,16: minute.
      if (partline(substr,line,15,2) .eq. 0) then
          isf_bulletin_error = 'missing minute: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
        isf_bulletin_error = 'bad minute: '//line
          read_origin = 20
          return
      end if
      mi = atoi(substr)

c     Char 17:  ':' character.
      if (line(17:17) .ne. ':') then
          isf_bulletin_error = 'bad date: '//line
          read_origin = 20
          return
      end if

c     Chars 18,19: integral second.
      if (partline(substr,line,18,2) .eq. 0) then
          isf_bulletin_error = 'missing second: '//line
          read_origin = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
        isf_bulletin_error = 'bad second: '//line
          read_origin = 20
          return
      end if
      ss = atoi(substr)
c     Char 20-22: msec or spaces.
c     Allow decimal point without any numbers after it.
      if (partline(substr,line,20,3) .ne. 0) then
c       Char 20: '.' character.
          if (line(20:20) .ne. '.') then
              isf_bulletin_error = 'bad date: '//line
              read_origin = 20
              return
          end if

c       Chars 21,22: 10s of msec.
          if (isdigit(line(21:21)) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_origin = 20
              return
          end if
          msec = (ichar(line(21:21)) - ichar('0'))*100

          if (isdigit(line(22:22)) .ne. 0) then
              msec = msec + (ichar(line(22:22)) - ichar('0'))*10
          else if (line(22:22) .ne. ' ' ) then
              isf_bulletin_error = 'bad msec: '//line
              read_origin = 20
              return
          end if
      else
c       Char 20: '.' character or space.
          if (line(20:20) .ne. '.' .and. line(20:20) .ne. ' ') then
              isf_bulletin_error = 'bad date: '//line
              read_origin = 20
              return
          end if
          msec = ISF_NULL
      end if

c     Char 23: timfix - either f or space.
      if (line(23:23) .eq. 'f' .or. line(23:23) .eq. ' ') then
          timfix = line(23:23)
      else
          isf_bulletin_error = 'bad timfix: '//line
          read_origin = 20
          return
      end if
c     Char 24: space.
      if (line(24:24) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 24: '//line
          read_origin = 20
          return
      end if

c     Chars 25-29: origin time error - real if anything.
      if (partline(substr,line,25,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad stime: '//line
              read_origin = 20
              return
          end if
          stime = ator(substr)
      else
          stime =ISF_NULL
      end if

c     Char 30: space.
      if (line(30:30) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 30: '//line
          read_origin = 20
          return
      end if

c     Chars 31-35: rms (sdobs) - real if anything.
      if (partline(substr,line,31,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad sdobs: '//line
              read_origin = 20
              return
          end if
          sdobs = ator(substr)
      else
          sdobs =ISF_NULL
      end if

c     Char 36: space.
      if (line(36:36) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 36: '//line
          read_origin = 20
          return
      end if
c      Chars 37-44: lattitude 
      if (partline(substr,line,37,8) .ne. 0) then
          isf_bulletin_error = 'latitude: '//line
          read_origin = 20
          return
      else
        lat = -999.
      end if


c     Char 45: space.
      if (line(45:45) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 45: '//line
          read_origin = 20
          return
      end if

c      Chars 46-54: longitude - must be there.
      if (partline(substr,line,46,9) .ne. 0) then
          isf_bulletin_error = 'longitude: '//line
          read_origin = 20
          return
      else
          lon = -999.
      end if

c     Char 55: epifix - either f or space.
      if (line(55:55) .eq. 'f' .or. line(55:55) .eq. ' ') then
          epifix = line(55:55)
      else
          isf_bulletin_error = 'bad epifix: '//line
          read_origin = 20
          return
      end if
c     Chars 56-60: semi-major axis of error ellipse - real if there.
c     This is departure from format but smaj < smin is daft.
      if (partline(substr,line,56,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad smaj: '//line
              read_origin = 20
              return
          end if
          smaj = ator(substr)
      else
          smaj =ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 61: '//line
          read_origin = 20
          return
      end if

c     Chars 62-66: semi-minor axis of error ellipse - real if there.
      if (partline(substr,line,62,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad smin: '//line
              read_origin = 20
              return
          end if
          smin = ator(substr)
      else
          smin =ISF_NULL
      end if

c     Char 67: space.
      if (line(67:67) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 67: '//line
          read_origin = 20
          return
      end if
c     Chars 68-70: strike - integer if there.
c     Strike can be -1, when its a flag to signify that smaj,smin
c     are really slat,slon.
      if (partline(substr,line,68,3) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad strike: '//line
              read_origin = 20
              return
          end if
          strike = atoi(substr)
      else
          strike =ISF_NULL
      end if

c     Char 71: space.
      if (line(71:71) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 71: '//line
          read_origin = 20
          return
      end if

c     Chars 72-76: depth - real if there.
      if (partline(substr,line,72,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad depth: '//line
              read_origin = 20
              return
          end if
          depth = ator(substr)
      else
          depth =ISF_NULL
      end if

c     Char 77: depfix - either d,f, or space.
      if (line(77:77) .eq. 'f' .or. line(77:77) .eq. ' ' .or.
     +    line(77:77) .eq. 'd') then
          depfix = line(77:77)
      else
          isf_bulletin_error = 'bad depfix: '//line
          read_origin = 20
          return
      end if
c     Char 78: space.
      if (line(78:78) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 78: '//line
          read_origin = 20
          return
      end if

c     Chars 79-82: depth error - real if there.
      if (partline(substr,line,79,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad sdepth: '//line
              read_origin = 20
              return
          end if
          sdepth = ator(substr)
      else
          sdepth =ISF_NULL
      end if

c     Char 83: space.
      if (line(83:83) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 83: '//line
          read_origin = 20
          return
      end if

c     Chars 84-87: ndef - integer if there.
      if (partline(substr,line,84,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad ndef: '//line
              read_origin = 20
              return
          end if
          ndef = atoi(substr)
      else
          ndef =ISF_NULL
      end if

c     Char 88: space.
      if (line(88:88) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 88: '//line
          read_origin = 20
          return
      end if
c     Chars 89-92: nsta - integer if there.
      if (partline(substr,line,89,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad nsta: '//line
              read_origin = 20
              return
          end if
          nsta = atoi(substr)
      else
          nsta =ISF_NULL
      end if

c     Char 93: space.
      if (line(93:93) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 93: '//line
          read_origin = 20
          return
      end if

c     Chars 94-96: gap - integer if there.
      if (partline(substr,line,94,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad gap: '//line
              read_origin = 20
              return
          end if
          gap = atoi(substr)
      else
          gap =ISF_NULL
      end if

c     Char 97: space.
      if (line(97:97) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 97: '//line
          read_origin = 20
          return
      end if
c     Chars 98-103: minimum distance - real if there.
      if (partline(substr,line,98,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mindist: '//line
              read_origin = 20
              return
          end if
          mindist = ator(substr)
      else
          mindist =ISF_NULL
      end if

c     Char 104: space.
      if (line(104:104) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 104: '//line
          read_origin = 20
          return
      end if

c     Chars 105-110: maximum distance - real if there.
      if (partline(substr,line,105,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad maxdist: '//line
              read_origin = 20
              return
          end if
          maxdist = ator(substr)
      else
          maxdist =ISF_NULL
      end if

c     Char 111: space.
      if (line(111:111) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 111: '//line
          read_origin = 20
          return
      end if
c     Char 112: analysis type - either space, a, m, or g.
      if (line(112:112) .eq. 'a' .or. line(112:112) .eq. 'm' .or.
     +    line(112:112) .eq. 'g' .or. line(112:112) .eq. ' ') then

          antype = line(112:112)
      else
          isf_bulletin_error = 'bad antype: '//line
          read_origin = 20
          return
      end if

c     Char 113: space.
      if (line(113:113) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 113: '//line
          read_origin = 20
          return
      end if

c     Char 114: location method - either space, i, p, g, or o.
      if (line(114:114) .eq. 'i' .or. line(114:114) .eq. 'p' .or.
     +    line(114:114) .eq. 'g' .or. line(114:114) .eq. 'o' .or.
     +    line(114:114) .eq. ' ') then

          loctype = line(114:114)
      else
          isf_bulletin_error = 'bad loctype: '//line
          read_origin = 20
          return
      end if

c     Char 115: space.
      if (line(115:115) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 115: '//line
          read_origin = 20
          return
      end if

c     Chars 116-117: event type, any characters allowed but must be there
      if (partline(etype,line,116,2) .eq. 0) then
          etype = " "
      else if( len(etype) .ne. 2) then
          isf_bulletin_error = 'bad etype: '//line
          read_origin = 20
          return
      end if
c     Char 118: space.
      if (line(118:118) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 118: '//line
          read_origin = 20
          return
      end if

c     Chars 119-127: author, any characters allowed but must be there.
      if (partline(author,line,119,9) .eq. 0) then
          isf_bulletin_error = 'missing author: '//line
          read_origin = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//line
          read_origin = 20
          return
      end if

c     Char 128: space.
      if (line(128:128) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 128: '//line
          read_origin = 20
          return
      end if

c     Chars 129-136: origin ID, any characters allowed but must be there.
      if (partline(origid,line,129,8) .eq. 0) then
          isf_bulletin_error = 'missing origid: '//line
          read_origin = 20
          return
      end if

      if ( check_whole(origid) .eq. 1 ) then
          isf_bulletin_error = 'bad origid: '//line
          read_origin = 20
          return
      end if

c     Check for extra characters after char 136.
      if (partline(substr,line,137,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_origin = 20
          return
      end if

      read_origin_nohyp = 0
      return
      end


      subroutine isf2nordicxxx(iunit,out)
      implicit none
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock
      include 'isf_bul.h'                 ! isf header file
      include 'isfnor.inc'                ! isf variables
      integer recognized,out,iunit
      integer stdout                      ! 3 to file, 6 to screen
      integer seiclen
      integer i,j,ind

      stdout=6
      recognized=1
      out=0
      do while (recognized.eq.1)
        read(iunit,'(a)',end=99) line
c       write(*,*) line
        recognized=0
c
c check for valid headers
c
        if (read_origin_head(line).eq.0) then
          write(stdout,*) ' reading origin head '
          recognized=1
          if (out.eq.1) then
            backspace(iunit)
            goto 99
          endif
c
c write out event, then goto next
c
        elseif (read_fault_plane_head(line).eq.0) then
          recognized=1
          write(stdout,*) ' reading fault plane head '
        elseif (read_axes_head(line).eq.0) then
          recognized=1
          write(stdout,*) ' reading axes head '
        elseif (read_axes_err_head(line).eq.0) then
          recognized=1
          write(stdout,*) ' reading axes err head '
        elseif (read_netmag_head(line).eq.0) then
          recognized=1
          write(stdout,*) ' reading netmag head '
        elseif (read_effects_head(line).eq.0) then
          recognized=1
          write(stdout,*) ' reading effects head '
        elseif (read_phase_head(line).eq.0) then
          recognized=1
          write(stdout,*) ' reading phase head '
        elseif (read_phase_info_head(line).eq.0) then
          recognized=1
          write(stdout,*) ' reading phase info head '
c
c check for valid parameter lines
c
        elseif (read_origin(line,yyyy,mm,dd,hh,mi,ss,
     &    msec,timfix,stime,sdobs,lat,lon,epifix,smaj,smin,
     &    strike,depth,depfix,sdepth,ndef,nsta,gap,
     &    mindist,maxdist,anytype,loctype,etype,
c     &    author,origid).eq.0 .or. 
     &    author,origid).eq.0) then
c     &      read_origin_nohyp(line,yyyy,mm,dd,hh,mi,ss,
c     &    msec,timfix,stime,sdobs,lat,lon,epifix,smaj,smin,
c     &    strike,depth,depfix,sdepth,ndef,nsta,gap,
c     &    mindist,maxdist,anytype,loctype,etype,
c     &    author,origid).eq.0) then

c
c extract Nordic parameters
c
          rea_nhyp=rea_nhyp+1
          call rea_hyp_clear(rea_nhyp)
          recognized=1
          out=1
          write(stdout,*) ' reading origin '
          write(*,*) yyyy,mm,dd,hh,mi,ss,msec
          hyp_origid(rea_nhyp)=origid
          if (yyyy.ne.ISF_NULL) hyp_year(rea_nhyp)=yyyy
          if (mm.ne.ISF_NULL) hyp_month(rea_nhyp)=mm
          if (dd.ne.ISF_NULL) hyp_day(rea_nhyp)=dd
          if (hh.ne.ISF_NULL) hyp_hour(rea_nhyp)=hh
          if (mi.ne.ISF_NULL) hyp_min(rea_nhyp)=mi
          if (ss.ne.ISF_NULL.and.msec.ne.ISF_NULL) 
     &      hyp_sec(rea_nhyp)=float(ss)+float(msec)/1000.
          if (timfix.eq.'f') hyp_fix_org(rea_nhyp)='F'
          if (stime.ne.ISF_NULL) hyp_sec_err(rea_nhyp)=stime
          if (sdobs.ne.ISF_NULL) hyp_rms(rea_nhyp)=sdobs
          if (lat.ne.ISF_NULL) hyp_lat(rea_nhyp)=lat
          if (lon.ne.ISF_NULL) hyp_lon(rea_nhyp)=lon
          if (epifix.eq.'f') hyp_epi_flag(rea_nhyp)='F'
          if (depth.ne.ISF_NULL) hyp_depth(rea_nhyp)=depth
          if (sdepth.ne.ISF_NULL) hyp_depth_err(rea_nhyp)=sdepth
          if (depfix.eq.'f') hyp_depth_flag(rea_nhyp)='F'
          if (nsta.ne.ISF_NULL) hyp_nstat(rea_nhyp)=nsta
          if (gap.ne.ISF_NULL) hyp_gap(rea_nhyp)=float(gap)
          if (maxdist.gt.0.and.maxdist.le.3000.) then
            hyp_dist_id(rea_nhyp)='L'
          else
            hyp_dist_id(rea_nhyp)='L'
          endif
          hyp_agency(rea_nhyp)=author(1:3)
c
c create ID line
c
          if (rea_nhyp.eq.1) then
            rea_id_line=
     &' ACTION:                   OP:     STATUS:'//
     &'               ID:                   I'
            write(rea_id_line(61:74),'(i4.4,5i2.2)') yyyy,mm,dd,hh,mi,ss
          endif
c
c event id
c 
        elseif (read_event_id(line, evid, region).eq.0) then
          recognized=1
c          rea_nhyp=0 ! lot 27/5/2007
          if (seiclen(region).gt.0) then
            rea_ncomment=rea_ncomment+1
            rea_comment(rea_ncomment)=
     &     '                                        '//
     &     '                                       3'
            write(rea_comment(rea_ncomment)(2:79),'(a)') region
          endif
        elseif (read_origin_prime(line).eq.0) then
          recognized=1
c
c read macroseismic effects
c
        elseif (read_effects(line,heard,felt,damage,casualties,uplift,
     &     subsidence,fault,tsunami,seiche,volcano,acoustic,gravity,
     &     t_wave,liquification,geyser,landslide,sandblow,cracks,lights,
     &     odours,loctype,lat,lon,dist,azim,country,postcode,net,
     &     sta,intensity1,modifier,intensity2,scale,author).eq.0) then

          recognized=1
          rea_nmacro=rea_nmacro+1
          rea_macro(rea_nmacro)=
     &     '                                        '//
     &     '                                       2'
          rea_macro(rea_nmacro)(23:23)=tsunami
          rea_macro(rea_nmacro)(24:24)=seiche
          if (mloctype.eq.'LatLon') then
            write(rea_macro(rea_nmacro)(34:47),'(f6.2,1x,f7.2)')
     &         mlat,mlon
          endif
          if (intensity1.ne.ISF_NULL) then
            write(rea_macro(rea_nmacro)(30:32),'(i2,a1)')
     &        int(intensity1),modifier
          endif
          write(rea_macro(rea_nmacro)(73:75),'(a3)')
     &        author

c
c read phases
c
        elseif (read_phase(line,sta,dist,esaz,phase,hh,mi,ss,msec,
     &    timeres,azim,azimres,slow,slowres,timedef,azimdef,
     &    slowdef,snr,amp,per,picktype,sp_fm,detchar,
     &    magtype,magind,mag,arrid).eq.0) then

          write(stdout,*) ' reading phase '
          recognized=1
          rea_nphase=rea_nphase+1
          rea_auto(rea_nphase)=' '
          rea_onset(rea_nphase)=' '
          rea_polarity(rea_nphase)=' '
          rea_weight_in(rea_nphase)=' '
          rea_weight_out(rea_nphase)=' '
          rea_az(rea_nphase)=-999.
          rea_baz_obs(rea_nphase)=-999.
          rea_baz_res(rea_nphase)=-999.
          rea_vel(rea_nphase)=-999.
          rea_ain(rea_nphase)=-999.
          rea_dist(rea_nphase)=-999.
          rea_arrid(rea_nphase)=arrid
          rea_stat(rea_nphase)=sta
          rea_comp(rea_nphase)='    '   ! lo 12/5/09
          rea_co(rea_nphase)='  '
c          if (azim.ne.ISF_NULL) rea_az(rea_nphase)=azim
          if (azim.ne.ISF_NULL) rea_baz_obs(rea_nphase)=azim
          if (phase(1:1).eq.'E'.or.phase(1:1).eq.'I') then
            rea_onset(rea_nphase)=phase(1:1)
            rea_phase(rea_nphase)=phase(2:)
          else
            rea_phase(rea_nphase)=phase
          endif
          rea_hour(rea_nphase)=hh
          rea_min(rea_nphase)=mi
          rea_sec(rea_nphase)=float(ss)+float(msec)/1000.
          rea_res(rea_nphase)=timeres
          if (slow.gt.0.) rea_vel(rea_nphase)=1./slow
          rea_amp(rea_nphase)=amp
          rea_per(rea_nphase)=per
c
c read phase measure
c
       elseif (read_phase_measure(line,param,value,error,numparam)
     &    .eq.0) then
          recognized=1
          if (param.eq.'DURATION') then
            read(value,'(f6.1)') rea_coda(rea_nphase)
          endif
c
c read phase info
c
       elseif (read_phase_info(line,net,chan,filter,filter_min,
     &    filter_max,phase,yyyy,mm,dd,time_unc,time_weight,
     &    azim_unc,azim_weight,slow_unc,slow_weight,amp_unc,
     &    per_unc,mag_unc,author,arrid).eq.0) then
           
          write(stdout,*) ' reading phase info '
          recognized=1
          ind=0
          do i=1,rea_nphase
            if (arrid.eq.rea_arrid(i)) then
              ind=i
            endif
          enddo
          if (ind.gt.0) then
            rea_co(ind)(1:1)=chan(1:1)
            rea_co(ind)(2:2)=chan(3:3)
            if (yyyy.gt.0) rea_year(ind)=yyyy
            if (mm.gt.0) rea_month(ind)=mm
            if (dd.gt.0) rea_day(ind)=dd
          endif
c
c read magnitudes
c
        elseif (read_netmag(line,magtype,magind,mag,
     &            magerr,nsta,author,origid).eq.0) then
            
          write(*,*) line
          recognized=1
          ind=0
          do j=1,rea_nhyp
            if (origid.eq.hyp_origid(j)) ind=j
          enddo
          do j=1,6
            if (hyp_mag(j,ind).eq.-999..and.ind.gt.0) then
                hyp_mag(j,ind)=mag      
                if (magtype(1:1).eq.'M'.or.
     &            magtype(1:1).eq.'m') then
                  hyp_mag_type(j,ind)=magtype(2:)
                else
                  hyp_mag_type(j,ind)=magtype
                endif
                hyp_mag_agency(j,ind)=author
                ind=0
            endif
          enddo
c
c read fault plane solution
c
        elseif (read_fault_plane(line,f_type,fpstrike,fpdip,
     &        fprake,np,ns,f_plane,author).eq.0) then

          recognized=1
            rea_nfault=rea_nfault+1
            rea_fault(rea_nfault)=
     &       '                                        ' //
     &       '                                       F' 
            write(rea_fault(rea_nfault)(2:31),'(3f10.1)')
     &        fpstrike,fpdip,fprake
        elseif (seiclen(line).eq.0) then
c empty line
          recognized=1
        endif
        if (recognized.eq.0) then
          write(stdout,'(a)') ' following line not recognized: '
          write(stdout,'(a)') line(1:seiclen(line))
          recognized=1
        endif
      enddo
99    continue

      return
      end
