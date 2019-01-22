c  changes
c  may 31 2011 jh : sn to ain

      subroutine isf2nordic(iunit,out)
      implicit none
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock
      include 'isf_bul.h'                 ! isf header file
      include 'isfnor.inc'                ! isf variables
      integer recognized,out,iunit
      integer stdout                      ! 3 to file, 6 to screen
      integer seiclen
      integer i,j,ind
      character*4 comp

      stdout=6
      recognized=1
      out=0
      do while (recognized.eq.1)
        read(iunit,'(a)',end=99) line
        recognized=0
        if (line.eq.'STOP') goto 99
c
c check for valid headers
c
        if (read_origin_head(line).eq.0) then
          write(stdout,*) ' reading origin head '
          recognized=1
          if (out.eq.1) then
            backspace(iunit)
            write(*,*) ' new header, writing previous event '
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
c          write(*,*) ' debug ',rea_nphase
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
c
c change station or component names according to def file
c
          comp=' '
          call set_def_chan(1,sta,comp)
          rea_stat(rea_nphase)=sta
          if (seiclen(comp).le.0) then
            rea_comp(rea_nphase)='    '   ! lo 12/5/09
            rea_co(rea_nphase)='  '
          else
            rea_comp(rea_nphase)=comp
            rea_co(rea_nphase)=comp(1:1)//comp(4:4)
          endif
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
          write(55,*) magtype
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
c still remain in reading loop
        endif
c        if (seiclen(line).eq.0) then 
c          write(*,*) ' end of event '
c          recognized=0 
c        endif 
      enddo
99    continue

      return
      end
