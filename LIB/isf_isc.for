c     Subroutines to set all arguments sent to the isf_writ functions to null.
c     Real and float variables are set to the common block parameter ISF_NULL,
c     this parameter is set in 'isf_bul.h'.
c     Strings are set to a single space.
c     The write routines check for these values and use them to distinguish 
c     between zero and undefined arguments.

c
c   jun 16  jh small fix  jh

      subroutine nullify_data_type(data_type,subtype,data_format,
     +                                                    subformat)

      character data_type*(*), subtype*(*), data_format*(*)
      character subformat*(*)
      include 'isf_bul.h'

      data_type   = " "
      subtype     = " "
      data_format = " "
      subformat   = " "

      return
      end

      subroutine nullify_event_id(evid,region)

      character evid*(*),region*(*)

      evid   = " "
      region = " "

      return
      end

      subroutine nullify_origin(yyyy,mm,dd,hh,mi,ss,msec,timfix,stime,
     + sdobs,lat,lon,epifix,smaj,smin,strike,depth,depfix,
     + sdepth,ndef,nsta,gap,mindist,maxdist,antype,loctype,
     + etype,author,origid)

      character author*(*), origid*(*), etype*(*)
      character*1 timfix, epifix, depfix, antype, loctype
      integer yyyy, mm, dd, hh, mi, ss, msec
      integer strike, ndef, nsta, gap
      real stime, sdobs, lat, lon, smaj, smin, depth, sdepth
      real mindist, maxdist
      include 'isf_bul.h'

      yyyy   = ISF_NULL
      mm     = ISF_NULL
      dd     = ISF_NULL
      hh     = ISF_NULL
      mi     = ISF_NULL
      ss     = ISF_NULL
      msec   = ISF_NULL
      timfix = ' '
      stime  = ISF_NULL
      sdobs  = ISF_NULL
      lat    = ISF_NULL
      lon    = ISF_NULL
      epifix = ' '
      smaj   = ISF_NULL
      smin   = ISF_NULL
      strike = ISF_NULL
      depth  = ISF_NULL
      depfix = ' '
      sdepth = ISF_NULL
      ndef   = ISF_NULL
      nsta   = ISF_NULL
      gap    = ISF_NULL
      mindist = ISF_NULL
      maxdist = ISF_NULL
      antype = ' '
      loctype = ' '
      etype  = " "
      author = " "
      origid = " "

      return
      end

      subroutine nullify_origin_param(param,value,numparam)

      integer numparam
      character param(*)*(*),value(*)*(*)

      numparam = 0
      return
      end

      subroutine nullify_momten(scale_factor,scalar_moment,fclvd,
     + mrr,mtt,mpp,mrt,mtp,mpr,nsta1,nsta2,author,scalar_moment_unc,
     + fclvd_unc,mrr_unc,mtt_unc,mpp_unc,mrt_unc,mtp_unc,mpr_unc,
     + ncomp1,ncomp2,duration)

      character author*(*)
      integer scale_factor,nsta1,nsta2,ncomp1,ncomp2
      real scalar_moment,fclvd,mrr,mtt,mpp,mrt,mtp,mpr
      real scalar_moment_unc,fclvd_unc,mrr_unc,mtt_unc,mpp_unc
      real mrt_unc,mtp_unc,mpr_unc,duration
      include 'isf_bul.h'

      scale_factor  = ISF_NULL
      scalar_moment = ISF_NULL
      fclvd         = ISF_NULL
      mrr           = ISF_NULL
      mtt           = ISF_NULL
      mpp           = ISF_NULL
      mrt           = ISF_NULL
      mtp           = ISF_NULL
      mpr           = ISF_NULL
      nsta1         = ISF_NULL
      nsta2         = ISF_NULL
      author = " "

      scalar_moment_unc = ISF_NULL
      fclvd_unc         = ISF_NULL
      mrr_unc           = ISF_NULL
      mtt_unc           = ISF_NULL
      mpp_unc           = ISF_NULL
      mrt_unc           = ISF_NULL
      mtp_unc           = ISF_NULL
      mpr_unc           = ISF_NULL
      ncomp1            = ISF_NULL
      ncomp2            = ISF_NULL
      duration          = ISF_NULL

      return
      end

      subroutine nullify_fault_plane (f_type,strike,dip,rake,np,ns,
     + f_plane,author)

      character f_plane*(*), f_type*(*), author*(*)
      integer np,ns
      real strike,dip,rake
      include 'isf_bul.h'

      f_type = " "
      strike = ISF_NULL
      dip    = ISF_NULL
      rake   = ISF_NULL
      np     = ISF_NULL
      ns     = ISF_NULL
      f_plane = " "
      author  = " "

      return
      end

      subroutine nullify_axes(scale_factor,t_val,t_azim,t_pl,b_val,
     + b_azim,b_pl,p_val,p_azim,p_pl,author)

      character author*(*)
      integer scale_factor
      real t_val,t_azim,t_pl,b_val,b_azim,b_pl,p_val,p_azim,p_pl

      include 'isf_bul.h'

      scale_factor = ISF_NULL
      t_val    = ISF_NULL
      t_azim   = ISF_NULL
      t_pl     = ISF_NULL
      b_val    = ISF_NULL
      b_azim   = ISF_NULL
      b_pl     = ISF_NULL
      p_val    = ISF_NULL
      p_azim   = ISF_NULL
      p_pl     = ISF_NULL
      author = " "

      return
      end

      subroutine 
     +nullify_axes_err(t_val_unc,t_azim_unc,t_pl_unc,b_val_unc,
     + b_azim_unc,b_pl_unc,p_val_unc,p_azim_unc,p_pl_unc,fclvd)

      real t_val_unc,t_azim_unc,t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc
      real p_val_unc,p_azim_unc,p_pl_unc,fclvd

      include 'isf_bul.h'

      t_val_unc  = ISF_NULL
      t_azim_unc = ISF_NULL
      t_pl_unc   = ISF_NULL
      b_val_unc  = ISF_NULL
      b_azim_unc = ISF_NULL
      b_pl_unc   = ISF_NULL
      p_val_unc  = ISF_NULL
      p_azim_unc = ISF_NULL
      p_pl_unc   = ISF_NULL
      fclvd      = ISF_NULL

      return
      end

      subroutine nullify_effects(heard,felt,damage,casualties,uplift,
     + subsidence,fault,tsunami,seiche,volcano,acoustic,gravity,
     + t_wave,liquification,geyser,landslide,sandblow,cracks,lights,
     + odours,loctype,lat,lon,dist,azim,country,postcode,net,
     + sta,intensity1,modifier,intensity2,scale,author)

      character author*(*),loctype*(*)
      character scale*(*),country*(*),postcode*(*),net*(*),sta*(*)
      character heard,felt,damage,casualties,uplift,subsidence
      character fault,tsunami,seiche,volcano
      character acoustic,gravity,t_wave,liquification,geyser
      character landslide,sandblow,cracks,lights,odours
      character modifier
      real lat,lon,dist,azim,intensity1,intensity2

      include 'isf_bul.h'

      heard      = ' '
      felt       = ' '
      damage     = ' '
      casualties = ' '
      uplift     = ' '
      subsidence = ' '
      fault      = ' '
      tsunami    = ' '
      seiche     = ' '
      volcano    = ' '
      acoustic   = ' '
      gravity    = ' '
      t_wave     = ' '
      liquification = ' '
      geyser     = ' '
      landslide  = ' '
      sandblow   = ' '
      cracks     = ' '
      lights     = ' '
      odours     = ' '
      loctype    = " "
      lat   = ISF_NULL
      lon   = ISF_NULL
      dist  = ISF_NULL
      azim  = ISF_NULL
      country  = " "
      postcode = " "
      net = " "
      sta = " "
      intensity1 = ISF_NULL
      modifier = ' '
      intensity2 = ISF_NULL
      scale  = " "
      author = " "

      return
      end

      subroutine nullify_netmag(magtype,magind,mag,magerr,nsta,
     +                                              author,origid)

      character magtype*(*), author*(*), origid*(*)
      character magind
      real mag,magerr
      integer nsta

      include 'isf_bul.h'

      magtype = " "
      magind = ' '
      mag    = ISF_NULL
      magerr = ISF_NULL
      nsta   = ISF_NULL
      author = " "
      origid = " "

      return
      end

      subroutine nullify_netmag_sta(sta,n)

      integer n
      character sta(*)*(*)

      n = 0
      return
      end

      subroutine nullify_netmag_basis(param,value)

      character param*(*), value*(*)

      param = " "
      value = " "

      return
      end

      subroutine nullify_phase(sta,dist,esaz,phase,hh,mi,ss,msec,
     + timeres,azim,azimres,slow,slowres,timedef,azimdef,slowdef,
     + snr,amp,per,picktype,sp_fm,detchar,magtype,magind,mag,arrid)

      character sta*(*),arrid*(*),phase*(*),magtype*(*)
      character timedef,azimdef,slowdef,sp_fm,detchar,magind,picktype
      real dist,esaz,timeres,azim,azimres,slow,slowres,snr,amp,per,mag
      integer hh,mi,ss,msec

      include 'isf_bul.h'

      sta     = " "
      dist    = ISF_NULL
      esaz    = ISF_NULL
      phase   = " "
      hh      = ISF_NULL
      mi      = ISF_NULL
      ss      = ISF_NULL
      msec    = ISF_NULL
      timeres = ISF_NULL
      azim    = ISF_NULL
      azimres = ISF_NULL
      slow    = ISF_NULL
      slowres = ISF_NULL
      timedef = ' '
      azimdef = ' '
      slowdef = ' '
      snr     = ISF_NULL
      amp     = ISF_NULL
      per     = ISF_NULL
      picktype = ' '
      sp_fm    = ' '
      detchar  = ' '
      magtype  = " "
      magind   = ' '
      mag      = ISF_NULL
      arrid    = " "

      return
      end

      subroutine nullify_phase_measure(param,value,numparam)

      integer numparam
      character param(*)*(*),value(*)*(*)

      numparam = 0

      return
      end

      subroutine nullify_phase_origid(origid)

      character origid*(*)

      origid = " "

      return
      end

      subroutine nullify_phase_info(net,chan,filter,filter_min,
     + filter_max,phase,yyyy,mm,dd,time_unc,time_weight,
     + azim_unc,azim_weight,slow_unc,slow_weight,amp_unc,
     + per_unc,mag_unc,author,arrid)

      character net*(*),chan*(*),author*(*),arrid*(*),phase*(*)
      character filter
      real filter_min,filter_max,time_unc,time_weight,azim_unc
      real azim_weight
      real slow_unc,slow_weight,amp_unc,per_unc,mag_unc
      integer yyyy,mm,dd

      include 'isf_bul.h'

      net    = " "
      chan   = " "
      filter = ' '
      filter_min  = ISF_NULL
      filter_max  = ISF_NULL
      phase  = " "
      yyyy        = ISF_NULL
      mm          = ISF_NULL
      dd          = ISF_NULL
      time_unc    = ISF_NULL
      time_weight = ISF_NULL
      azim_unc    = ISF_NULL
      azim_weight = ISF_NULL
      slow_unc    = ISF_NULL
      slow_weight = ISF_NULL
      amp_unc     = ISF_NULL
      per_unc     = ISF_NULL
      mag_unc     = ISF_NULL
      author = " "
      arrid  = " "

      return
      end

      subroutine nullify_phase_min(timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      real timeoffset,azoffset,slowoffset,ampoffset,peroffset,magoffset

      include 'isf_bul.h'

      timeoffset = ISF_NULL
      azoffset   = ISF_NULL
      slowoffset = ISF_NULL
      ampoffset  = ISF_NULL
      peroffset  = ISF_NULL
      magoffset  = ISF_NULL

      return
      end

      subroutine nullify_phase_max(timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      real timeoffset,azoffset,slowoffset,ampoffset,peroffset,magoffset

      include 'isf_bul.h'

      timeoffset = ISF_NULL
      azoffset   = ISF_NULL
      slowoffset = ISF_NULL
      ampoffset  = ISF_NULL
      peroffset  = ISF_NULL
      magoffset  = ISF_NULL

      return
      end

      subroutine nullify_phase_correc(timecorr,azcorr,
     +                        slowcorr,ampcorr,percorr,magcorr)

      real timecorr,azcorr,slowcorr,ampcorr,percorr,magcorr

      include 'isf_bul.h'

      timecorr = ISF_NULL
      azcorr   = ISF_NULL
      slowcorr = ISF_NULL
      ampcorr  = ISF_NULL
      percorr  = ISF_NULL
      magcorr  = ISF_NULL

      return
      end

      subroutine nullify_phase_original(chan,sta,yyyy,mm,
     + dd,hh,mi,ss,msec,azim,slow,amp,per,mag)

      character sta*(*),chan*(*)
      real azim,slow,amp,per,mag
      integer yyyy,mm,dd,hh,mi,ss,msec

      include 'isf_bul.h'

      chan  = " "
      yyyy  = ISF_NULL
      mm    = ISF_NULL
      dd    = ISF_NULL
      hh    = ISF_NULL
      mi    = ISF_NULL
      ss    = ISF_NULL
      msec  = ISF_NULL
      azim  = ISF_NULL
      slow  = ISF_NULL
      amp   = ISF_NULL
      per   = ISF_NULL
      mag   = ISF_NULL

      return
      end

      subroutine nullify_comment(comment)

      character comment*(*)

      comment = " "

      return
      end


c     To check whether the current line type is of an ISF type consistent with 
c     the type of the previous line written.
c     The exception is line_type 'data_type' where previous line type is
c     assumed to be undefined.

c     Checks and resets the global variable isf_prev_line_type. 
c     Returns 0 if the current line type is expected to follow the previous one.  
c     Returns 1 if this line type should not follow the previous line type.

      integer function check_prev_line_type(line_type)

      character line_type*(*)

      include 'isf_bul.h'
      character allowed(10)*(ISF_LINE_LEN)
      integer i,n

      i = 1
      if ( line_type(1:9) .eq. 'data_type') then
          isf_prev_line_type = line_type
          check_prev_line_type = 0
          return
      else if ( line_type(1:8) .eq. 'event_id') then
          allowed(i) = 'data_type'
          i=i+1
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
          i=i+1
          allowed(i) = 'phase_info'
      else if ( line_type(1:11) .eq. 'origin_head') then
          allowed(i) = 'event_id'
      else if ( line_type(1:10) .eq. 'origin_com') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'momten'
      else if ( line_type(1:6) .eq. 'origin') then
          allowed(i) = 'origin_head'
          i=i+1
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
      else if ( line_type(1:11) .eq. 'momten_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
      else if ( line_type(1:6) .eq. 'momten') then
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'momten_head'
          i=i+1
          allowed(i) = 'origin_com'
      else if ( line_type(1:16) .eq. 'fault_plane_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'momten'
      else if ( line_type(1:11) .eq. 'fault_plane') then
          allowed(i) = 'fault_plane_head'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'origin_com'
      else if ( line_type(1:9) .eq. 'axes_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'momten'
      else if ( line_type(1:13) .eq. 'axes_err_head') then
          allowed(i) = 'axes_head'
      else if ( line_type(1:8) .eq. 'axes_err') then
          allowed(i) = 'axes'
      else if ( line_type(1:4) .eq. 'axes') then
          allowed(i) = 'axes_head'
          i=i+1
          allowed(i) = 'axes_err_head'
          i=i+1
          allowed(i) = 'origin_com'
      else if ( line_type(1:11) .eq. 'netmag_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
      else if ( line_type(1:10) .eq. 'netmag_com') then
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
      else if ( line_type(1:6) .eq. 'netmag') then
          allowed(i) = 'netmag_head'
          i=i+1
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
      else if ( line_type(1:12) .eq. 'effects_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
      else if ( line_type(1:7) .eq. 'effects') then
          allowed(i) = 'effects_head'
          i=i+1
          allowed(i) = 'effects'
          i=i+1
          allowed(i) = 'effects_com'
      else if ( line_type(1:10) .eq. 'phase_head') then
          allowed(i) = 'origin'
          i=i+1
          allowed(i) = 'origin_com'
          i=i+1
          allowed(i) = 'momten'
          i=i+1
          allowed(i) = 'axes'
          i=i+1
          allowed(i) = 'axes_err'
          i=i+1
          allowed(i) = 'fault_plane'
          i=i+1
          allowed(i) = 'netmag'
          i=i+1
          allowed(i) = 'netmag_com'
          i=i+1
          allowed(i) = 'effects'
          i=i+1
          allowed(i) = 'effects_com'
      else if ( line_type(1:12) .eq. 'phase_origid') then
          allowed(i) = 'phase_head'
          i=i+1
          allowed(i) = 'phase_info_head'
      else if ( line_type(1:15) .eq. 'phase_info_head') then
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
      else if ( line_type(1:14) .eq. 'phase_info_com') then
          allowed(i) = 'phase_info'
          i=i+1
          allowed(i) = 'phase_info_com'
      else if ( line_type(1:10) .eq. 'phase_info') then
          allowed(i) = 'phase_origid'
          i=i+1
          allowed(i) = 'phase_info'
          i=i+1
          allowed(i) = 'phase_info_com'
          i=i+1
          allowed(i) = 'phase_info_head'
      else if ( line_type(1:5) .eq. 'phase') then
          allowed(i) = 'phase_origid'
          i=i+1
          allowed(i) = 'phase_head'
          i=i+1
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
      else if ( line_type(1:4) .eq. 'stop') then
          allowed(i) = 'phase'
          i=i+1
          allowed(i) = 'phase_com'
          i=i+1
          allowed(i) = 'phase_info'
          i=i+1
          allowed(i) = 'phase_info_com'
      end if
      n = i

      do i=1,n
          if (isf_prev_line_type .eq. allowed(i)) then
              isf_prev_line_type = line_type
              check_prev_line_type = 0
              return
          end if
      end do

      isf_bulletin_error = 'out of sequence: '//line_type//' following 
     +'//isf_prev_line_type
      isf_prev_line_type = line_type
      check_prev_line_type = 1
      return
      end

c     Writes a real a bit more flexibly than can be achieved with a format.
c     If a number is too big for the ideal precision it is printed with less
c     precision until it fills the field width without a decimal point at all.
c     For example might want 99.9999 => 99.99 but 999.9999 => 999.9.

      subroutine write_real(file,x,width,max_prec)

      integer file,width,max_prec
      real x
      character form*(20)

      integer prec,spare

      if ( x .gt. 0 ) then
          spare = width - 1 - log10(abs(x))
      else if ( x .lt. 0 ) then
          spare = width - 2 - log10(abs(x))
      else
          spare = max_prec
      end if

      if (spare .le. 0) then
          write (form,"('(i',i2,',$)')") width
          write (file,form) int(x)
      else
          if (spare .ge. max_prec) then
              prec = max_prec
          else
              prec = spare
          end if
          write (form,"('(f',i1,'.',i1,',$)')") width,prec
          write (file,form) x
      end if

      end


c     Get a substring, removing leading white space.
c     Expects a string, an offset from the start of the string, and a maximum 
c     length for the resulting substring. If this length is 0 it will take up
c     to the end of the input string.

c     Need to allow for ')' to come after the required field at the end of a 
c     comment.  Discard ')' at end of a string  as long as there's no '(' c     before it anywhere.

c     Returns the length of the resulting substring.

      integer function partline (substr,line,offset,numchars)

      character substr*(*), line*(*)
      integer offset, numchars
      integer i, start, end
      integer length
      integer bracket

      length = len(line)
      if (length .le. offset) then
          partline=0
          return
      end if

      start=offset
      if (start .le. 0) then
          start=1
      end if

      if (numchars .eq. 0) then
          end = length
      else
          end = offset + numchars - 1
      end if

      do while (line(start:start) .eq. ' ' .and. start .lt. end)
          start=start+1
      end do

      bracket = 0
      do i=start,end
          if (line(i:i) .eq. '(' ) then
              bracket=1
          end if
      end do    

      if (bracket .eq. 1) then
          do while ((line(end:end) .eq. ' ') .and. (end .ge. start))
              end=end-1
          end do
      else
          do while ( ((line(end:end) .eq. ' ') .or. (line(end:end)
     +              .eq. ')') ) .and. (end .ge. start))    
              end=end-1
          end do
      end if

      substr = line(start:end)
      partline = end-start+1
      return

      end


c     To check that a string has no spaces in it.
c     Returns 0 if there are no spaces or 1 if there is a space.

      integer function check_whole(str)

      character str*(*)
      include 'isf_bul.h'
      character substr*(ISF_LINE_LEN)
      integer i,length
      integer partline

      length = partline(substr,str,0,0)

      do i=1,length
          if (substr(i:i) .eq. ' ') then
              check_whole = 1
              return
          end if
      end do

      check_whole = 0
      return
      end


c     Check if a string is composed entirely of white space or not.
c     Returns 1 if it is, 0 if it isn't.

      integer function all_blank(str)

      character str*(*)
      include 'isf_bul.h'
      character substr*(ISF_LINE_LEN)
      integer i,length
      integer partline

      length = partline(substr,str,0,0)

      do i=1,length
          if (substr(i:i) .ne. ' ' .and. substr(i:i) .ne. '    ') then
              all_blank = 0
              return
          end if
      end do

      all_blank = 1
      return
      end


c     Check whether a real or integer is null or not.
c     Returns 1 if it is, 0 if it isn't.

      integer function is_null(x)

      real x
      include 'isf_bul.h'

      if (int(x) .eq. ISF_NULL) then
          is_null = 1
          return
      end if

      is_null = 0
      return
      end


c     To check that a string contains only sign/number characters and so 
c     is suitable for atoi - atoi itself does no checking.

c     Returns 0 if OK,  1 if not.

      integer function check_int(str)

      character str*(*)
      include 'isf_bul.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i
      integer partline,isdigit

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-' .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      do i=start, length
          if (isdigit(substr(i:i)) .eq. 0) then
              check_int = 1
              return
          end if
      end do

      check_int = 0
      return
      end


c     To check if a character is between 1 and 9
c     Returns 0 if it is,  1 if not.

      integer function isdigit(a)

      character a
      integer i

      i =  ichar(a) - ichar('0')
      if (i .gt. 9 .or. i .lt. 0) then
          isdigit = 0
          return
      end if

      isdigit = 1
      return
      end


c     To check if a character is between A and Z
c     Returns 0 if it is,  1 if not.

      integer function isupper(a)

      character a
      integer i

      i =  ichar(a) - ichar('A')
      if (i .gt. 26 .or. i .lt. 0) then
          isupper = 0
          return
      end if

      isupper = 1
      return
      end


c     Converts a string of numbers into an integer.
c     No checking done so need to run check_int on the string first.

      integer function atoi(str)

      character str*(*)
      include 'isf_bul.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i
      integer partline

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-' .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      atoi = 0
      do i=start, length
          atoi = atoi + (ichar(substr(i:i))-ichar('0')) * 
     +(10**(length-i))
      end do

      if (substr(1:1) .eq. '-') then 
          atoi = atoi*(-1)
      end if

      return
      end


c     To check that a string is suitable for ator
c     Returns 0 if OK,  1 if not.

      integer function check_real(str)

      character str*(*)
      include 'isf_bul.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i
      integer partline,isdigit

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-' .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      do i=start, length
          if (isdigit(substr(i:i)) .eq. 0) then
              if (substr(i:i) .ne. '.' ) then
                  check_real = 1
                  return
              end if
          end if
      end do

      check_real = 0
      return
      end


c     Converts a string of numbers into a real.
c     No checking done so need to run check_real on the string first.

      real function ator(str)

      character str*(*)
      include 'isf_bul.h'
      character substr*(ISF_LINE_LEN)
      integer length,start,i,point
      integer partline

      length = partline(substr,str,0,0)

      start = 1
      if (substr(1:1) .eq. '-'  .or. substr(1:1) .eq. '+') then
          start = 2
      end if

      point = length+1
      do i=1,length
          if (substr(i:i) .eq. '.') then
              point = i
          end if
      end do

      ator = 0
      do i=start, point-1
          ator = ator + (ichar(substr(i:i))-ichar('0')) * 
     +(10.0**(point-i-1))
      end do

      do i=point+1,length
          ator = ator + (ichar(substr(i:i))-ichar('0')) * 
     +(10.0**(point-i))
      end do

      if (substr(1:1) .eq. '-') then 
          ator = ator*(-1.)
      end if

      return
      end

c     Parses a line asuming it to be a data type line.
c     Format is:  DATA_TYPE data_type:subtype data_format:subformat
c     Only data_type is required.
c     If there is extra text it is ignored. 
c     No checks are made as to whether data_type is valid or not.

c     Returns 0 if the line is a properly formatted data type line 
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_data_type (line,data_type,subtype,
     +                                          data_format,subformat)

      character line*(*),data_type*(*),subtype*(*),data_format*(*)
      character subformat*(*)
      include 'isf_bul.h'
      integer start,end,mid

      if (line(1:10) .ne. "DATA_TYPE ") then
          isf_bulletin_error = 'not a data_type line: '//line
          read_data_type = 20
          return
      end if

c     Initialise strings - some of which may not be set.
      data_type   = ' '
      subtype     = ' '
      data_format = ' '
      subformat   = ' '

      start=11
      do while (line(start:start) .eq. ' ' .and. start .le. len(line))
          start=start+1
      end do

      end = index(line(start:),' ')
      if (end .ne. 0) then
          end = start+end
          mid = index(line(start:end),':')

          if (mid .ne. 0) then
              mid=start+mid
              data_type = line(start:mid-2)
              subtype = line(mid:end-2)
          else
              data_type = line (start:end-2)
          end if
          start = end
      else
          data_type = line(start:)
      end if

      do while (line(start:start) .eq. ' ' .and. start .le. len(line))
          start=start+1
      end do

      end = index(line(start:),' ')
      if (end .ne. 0) then
          end = start+end
          mid = index(line(start:end),':')
          if (mid .ne. 0) then
              mid=start+mid
              data_format = line(start:mid-2)
              subformat = line(mid:end-2)
          else
              data_format = line (start:end-2)
          end if
      else
          data_format = line(start:)
      end if

      read_data_type = 0
      return

      end

c     Parses a line asuming it to be an event title line.
c     Requires event ID to be present but allows lines with no region.

c     Returns 0 if the line is a properly formatted event ID line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_event_id(line, evid, region)
      include 'isf_bul.h'
      integer partline,check_whole

      character line*(*), evid*(*), region*(*)
      character substr*(ISF_LINE_LEN)
      integer i

c     Chars 1-5: must be the word 'Event'. Char 6: must be a space. 
      if ( (line(1:6) .ne. 'EVENT ') .and. (line(1:6) .ne. 'Event ') ) 
     +then
          isf_bulletin_error = 'not an event title line: '//line
          read_event_id = 20
          return
      end if

c     Chars 7-14: event ID
      if (partline(evid,line,7,8) .eq. 0) then
          isf_bulletin_error = 'missing evid: '//line
          read_event_id = 20
          return
      end if

      if ( check_whole(evid) .eq. 1 ) then
          isf_bulletin_error = 'bad evid: '//line
          read_event_id = 20
          return
      end if

c     Not quite right but lots of people hit CR after evid
      if (len(line) .lt. 15) then
          read_event_id = 0
          return
      end if

c     Char 15: must be a space
      if (line(15:15) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 15: '//line
          read_event_id = 20
          return
      end if

c     Chars 16-80: geographic region if there
      i=partline(region,line,16,65)

c     Check for extra characters after char 80.
      if (partline(substr,line,80,0) .ne. 0) then
          print *,i
          isf_bulletin_error = 'extra characters at end: '//line
          read_event_id = 20
          return
      end if

      read_event_id = 0
      return
      end

c     Tests a line to discover if it is an origin header line.

c     Returns 0 if the line is a properly formatted origin header line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_origin_head(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(136)
      integer headlen /136/

      head = '   Date       Time        Err   RMS Latitude Longitude  Sm
     +aj  Smin  Az Depth   Err Ndef Nsta Gap  mdist  Mdist Qual   Author
     +      OrigID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not an origin header: '//line
          read_origin_head = 20
          return
      end if

c     Check for extra characters after char 136.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_origin_head = 20
          return
      end if

      read_origin_head = 0
      return
      end


c     Parses a line asuming it to be an origin line.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted origin line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_origin(line,yyyy,mm,dd,hh,mi,ss,msec,
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
c     	Char 20: '.' character.
          if (line(20:20) .ne. '.') then
              isf_bulletin_error = 'bad date: '//line
              read_origin = 20
              return
          end if

c     	Chars 21,22: 10s of msec.
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
c     	Char 20: '.' character or space.
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

c      Chars 37-44: lattitude - must be there.
      if (partline(substr,line,37,8) .eq. 0) then
          isf_bulletin_error = 'missing lattitude: '//line
          read_origin = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_bulletin_error = 'bad lattitude: '//line
          read_origin = 20
          return
      end if
      lat = ator(substr)

c     Char 45: space.
      if (line(45:45) .ne. ' ') then
          isf_bulletin_error = 'bad format, char 45: '//line
          read_origin = 20
          return
      end if

c      Chars 46-54: longitude - must be there.
      if (partline(substr,line,46,9) .eq. 0) then
          isf_bulletin_error = 'missing longitude: '//line
          read_origin = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_bulletin_error = 'bad longitude: '//line
          read_origin = 20
          return
      end if
      lon = ator(substr)

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

      read_origin = 0
      return
      end


c     Parses a line to test whether it is a prime origin label.

c     Returns 0 if the line is a properly formatted prime origin line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error if not.

      integer function read_origin_prime(line)

      character line*(*)

      include 'isf_bul.h'
      integer partline

      character substr*(ISF_LINE_LEN)

      if (line(1:9) .ne. " (#PRIME)") then
          isf_bulletin_error = 'not a prime comment: '//line
          read_origin_prime = 20
          return
      end if

      if (partline(substr,line,10,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_origin_prime = 20
          return
      end if

      read_origin_prime = 0
      return
      end


c     Parses a line to test whether it is a centroid origin label.

c     Returns 0 if the line is a properly formatted centroid origin line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error if not.

      integer function read_origin_centroid(line)

      character line*(*)

      include 'isf_bul.h'
      integer partline

      character substr*(ISF_LINE_LEN)

      if (line(1:12) .ne. " (#CENTROID)") then
          isf_bulletin_error = 'not a centroid comment: '//line
          read_origin_centroid = 20
          return
      end if

      if (partline(substr,line,13,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_origin_centroid = 20
          return
      end if

      read_origin_centroid = 0
      return
      end


c     Parses a line assuming it to be an origin parameter formatted comment.
c     Accepts any number of parameter=value pairs as long as the line is
c     short enough.

c     Returns 0 if the line is a properly formatted origin paramter line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error if not.

      integer function read_origin_param(line,param,value,error,
     +                                                     numparam)

      character line*(*)
      character param(*)*(*),value(*)*(*),error(*)*(*)
      integer numparam
      include 'isf_bul.h'
      integer i,start,end,break,mid
      character substr*(ISF_LINE_LEN)

      if (line(1:9) .ne. " (#PARAM ") then
          isf_bulletin_error = 'not an origin parameter line: '//line
          read_origin_param = 20
          return
      end if

      start=10
      do while (line(start:start) .eq. ' ')
          start=start+1
      end do

      end=len(line)
      do while (line(end:end) .eq. ' ' .or. line(end:end) .eq. ')')
          if (line(end:end) .eq. ')') then
              line(end:end) = ' '
          end if
          end=end-1
      end do

      if (end .gt. ISF_COMM_LEN+10) then
          isf_bulletin_error = 'line too long: '//line
          read_origin_param = 20
          return
      end if

c     Go through the rest of the line one character at a time, separating c     words on ' ' to get param=value pairs and on '=' to get the
c     individual parameters and vales.
      numparam=0
      break = index(line(start:),' ')

      do while (break .ne. 0 .and. start .le. end)
          break = break + start
          mid = index(line(start:break),'=')

          if (mid .eq. 0) then
              isf_bulletin_error = 'param without value: '//line
              read_origin_param = 20
              return
          end if

          mid = mid + start
          numparam = numparam+1
          param(numparam) = line(start:mid-2)
          value(numparam) = line(mid:break-2)
          start = break
          break = index(line(start:),' ')
      end do

c     For each resulting value check whether includes an error part.
      do i=1,numparam
          mid = index(value(i),'+')
          if (mid .ne. 0) then
              substr = value(i)
              value(i) = substr(1:mid-1)
              error(i) = substr(mid+1:)
          else
              error(i) = " "
          end if
      end do

      read_origin_param = 0
      return
      end


c     Tests a line to discover if it is a first moment tensor header comment.

c     Returns 0 if the line is a first moment tensor header line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_momten_head_1(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(88)
      integer headlen /88/

      head = ' (#MOMTENS sc    M0 fCLVD    MRR    MTT    MPP    MRT    M
     +TP    MPR NST1 NST2 Author   )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not a first moment tensor header: 
     +'//line
          read_momten_head_1 = 20
          return
      end if

c     Check for extra characters after char 88.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_momten_head_1 = 20
          return
      end if

      read_momten_head_1 = 0
      return
      end


c     Tests a line to discover if it is a second moment tensor header comment.

c     Returns 0 if the line is a second moment tensor header line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_momten_head_2(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(88)
      integer headlen /88/

      head = ' (#             eM0 eCLVD    eRR    eTT    ePP    eRT    e
     +TP    ePR NCO1 NCO2 Duration )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not a second moment tensor header: 
     +'//line
          read_momten_head_2 = 20
          return
      end if

c     Check for extra characters after char 88.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_momten_head_2 = 20
          return
      end if

      read_momten_head_2 = 0
      return
      end


c     Parses a line asuming it to be a first moment tensor data comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted first moment tensor data line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_momten_line_1(line,scale_factor,
     + scalar_moment,fclvd,mrr,mtt,mpp,mrt,mtp,mpr,nsta1,nsta2,
     + author)

      character line*(*), author*(*)
      integer scale_factor,nsta1,nsta2
      real scalar_moment,fclvd,mrr,mtt,mpp,mrt,mtp,mpr
      include 'isf_bul.h'
      integer partline,check_int,check_real,atoi,check_whole
      real ator
      character substr*(ISF_LINE_LEN)


c     Chars 1-11: should be the string  ' (#        '
      if (line(1:11) .ne. " (#        ") then
          isf_bulletin_error = 'not a first moment tensor data line: 
     +'//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 12,13: scale factor - integer */
      if (partline(substr,line,12,2) .eq. 0) then
          isf_bulletin_error = 'missing scale factor: '//line
          read_momten_line_1 = 20
          return
      end if

      if (check_int(substr) .eq. 1) then
           isf_bulletin_error = 'bad scale factor: '//line
          read_momten_line_1 = 20
          return
      end if
      scale_factor = atoi(substr)

c     Char 14: space.
      if (line(14:14) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 14: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 15-19: scalar seismic moment - must be real.
      if (partline(substr,line,15,5) .eq. 0) then
          isf_bulletin_error = 'missing moment: '//line
          read_momten_line_1 = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_bulletin_error = 'bad moment: '//line
          read_momten_line_1 = 20
          return
      end if
      scalar_moment = ator(substr)

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 20: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 21-25: fCLVD, real if anything.
      if (partline(substr,line,21,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad fclvd: '//line
              read_momten_line_1 = 20
              return
          end if
          fclvd = ator(substr)
      else
          fclvd =ISF_NULL
      end if

c     Char 26: space.
      if (line(26:26) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 26: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 27-32: radial-radial element, real if anything.
      if (partline(substr,line,27,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mrr: '//line
              read_momten_line_1 = 20
              return
          end if
          mrr = ator(substr)
      else
          mrr =ISF_NULL
      end if

c     Char 33: space.
      if (line(33:33) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 33: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 34-39: theta-theta element, real if anything.
      if (partline(substr,line,34,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mtt: '//line
              read_momten_line_1 = 20
              return
          end if
          mtt = ator(substr)
      else
          mtt =ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 40: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 41-46: phi-phi element, real if anything.
      if (partline(substr,line,41,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mpp: '//line
              read_momten_line_1 = 20
              return
          end if
          mpp = ator(substr)
      else
          mpp =ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 47: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 48-53: radial-theta element, real if anything.
      if (partline(substr,line,48,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mrt: '//line
              read_momten_line_1 = 20
              return
          end if
          mrt = ator(substr)
      else
          mrt =ISF_NULL
      end if

c     Char 54: space.
      if (line(54:54) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 54: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 55-60: theta-phi element, real if anything.
      if (partline(substr,line,55,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mtp: '//line
              read_momten_line_1 = 20
              return
          end if
          mtp = ator(substr)
      else
          mtp =ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 61: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 62-67: phi-radial element, real if anything.
      if (partline(substr,line,62,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mpr: '//line
              read_momten_line_1 = 20
              return
          end if
          mpr = ator(substr)
      else
          mpr =ISF_NULL
      end if

c     Char 68: space.
      if (line(68:68) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 68: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 69-72: nsta1, int if anything.
      if (partline(substr,line,69,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad nsta1: '//line
              read_momten_line_1 = 20
              return
          end if
          nsta1 = atoi(substr)
      else
          nsta1 =ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 73: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 74-77: nsta2, int if anything.
      if (partline(substr,line,74,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad nsta2: '//line
              read_momten_line_1 = 20
              return
          end if
          nsta2 = atoi(substr)
      else
          nsta2 =ISF_NULL
      end if

c     Char 78: space.
      if (line(78:78) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 78: '//line
          read_momten_line_1 = 20
          return
      end if

c     Chars 79-87: author, any characters allowed but must be there.
      if (partline(author,line,79,9) .eq. 0) then
          isf_bulletin_error = 'missing author: '//line
          read_momten_line_1 = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//line
          read_momten_line_1 = 20
          return
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,88,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_momten_line_1 = 20
          return
      end if

      read_momten_line_1 = 0
      return
      end

c     Parses a line asuming it to be a second moment tensor data comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if a properly formatted second moment tensor data line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_momten_line_2(line,scalar_moment_unc,
     + fclvd_unc,mrr_unc,mtt_unc,mpp_unc,mrt_unc,mtp_unc,mpr_unc,
     + ncomp1,ncomp2,duration)

      character line*(*)
      integer ncomp1,ncomp2
      real scalar_moment_unc,fclvd_unc,mrr_unc,mtt_unc,mpp_unc
      real mrt_unc,mtp_unc,mpr_unc,duration
      include 'isf_bul.h'
      integer partline,check_int,check_real,atoi
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-14: should be the string  ' (#           '
      if (line(1:14) .ne. " (#           ") then
          isf_bulletin_error = 'not a second moment tensor data line: 
     +'//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 15-19:  uncertainty in scalar seismic moment - real if there.
      if (partline(substr,line,15,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad scalar_moment_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          scalar_moment_unc = ator(substr)
      else
          scalar_moment_unc = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 20: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 21-25: uncertainty in fCLVD, real if anything.
      if (partline(substr,line,21,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad fclvd_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          fclvd_unc = ator(substr)
      else
          fclvd_unc =ISF_NULL
      end if

c     Char 26: space.
      if (line(26:26) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 26: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 27-32: uncertainty in radial-radial element, real if anything.
      if (partline(substr,line,27,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mrr_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mrr_unc = ator(substr)
      else
          mrr_unc =ISF_NULL
      end if

c     Char 33: space.
      if (line(33:33) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 33: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 34-39: uncertainty in theta-theta element, real if anything.
      if (partline(substr,line,34,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mtt_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mtt_unc = ator(substr)
      else
          mtt_unc =ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 40: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 41-46: uncertainty in phi-phi element, real if anything.
      if (partline(substr,line,41,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mpp_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mpp_unc = ator(substr)
      else
          mpp_unc =ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 47: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 48-53: uncertainty in radial-theta element, real if anything.
      if (partline(substr,line,48,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mrt_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mrt_unc = ator(substr)
      else
          mrt_unc =ISF_NULL
      end if

c     Char 54: space.
      if (line(54:54) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 54: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 55-60: uncertainty in theta-phi element, real if anything.
      if (partline(substr,line,55,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mtp_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mtp_unc = ator(substr)
      else
          mtp_unc =ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 61: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 62-67: uncertainty in phi-radial element, real if anything.
      if (partline(substr,line,62,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad mpr_unc: '//line
              read_momten_line_2 = 20
              return
          end if
          mpr_unc = ator(substr)
      else
          mpr_unc =ISF_NULL
      end if

c     Char 68: space.
      if (line(68:68) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 68: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 69-72: ncomp1, int if anything.
      if (partline(substr,line,69,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad ncomp1: '//line
              read_momten_line_2 = 20
              return
          end if
          ncomp1 = atoi(substr)
      else
          ncomp1 =ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 73: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 74-77: ncomp2, int if anything.
      if (partline(substr,line,74,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
            isf_bulletin_error = 'bad ncomp2: '//line
              read_momten_line_2 = 20
              return
          end if
          ncomp2 = atoi(substr)
      else
          ncomp2 =ISF_NULL
      end if

c     Char 78: space.
      if (line(78:78) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 78: '//line
          read_momten_line_2 = 20
          return
      end if

c     Chars 79-86:  duration, real if anything.
      if (partline(substr,line,79,8) .ne. 0) then
          if (check_real(substr) .eq. 1) then
            isf_bulletin_error = 'bad duration: '//line
              read_momten_line_2 = 20
              return
          end if
          duration = ator(substr)
      else
          duration =ISF_NULL
      end if

c     Check for extra characters - not including close bracket.  */
      if (partline(substr,line,87,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_momten_line_2 = 20
          return
      end if

      read_momten_line_2 = 0
      return
      end


c     Tests a line to discover if it is a fault plane header comment.

c     Returns 0 if the line is a fault plane header.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_fault_plane_head(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(64)
      integer headlen /64/

      head = ' (#FAULT_PLANE Typ Strike   Dip    Rake  NP  NS Plane Auth
     +or   )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not a fault plane header: '//line
          read_fault_plane_head = 20
          return
      end if

c     Check for extra characters after char 64.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_fault_plane_head = 20
          return
      end if

      read_fault_plane_head = 0
      return
      end


c     Parses a line asuming it to be a fault plane data comment.
c     Could be first or second plane, the only difference is whether
c     author field is expected or not.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted fault plane data line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_fault_plane(line,f_type,strike,dip,
     +                                    rake,np,ns,f_plane,author)

      character line*(*)
      character f_plane*(*), f_type*(*), author*(*)
      integer np,ns
      real strike,dip,rake

      include 'isf_bul.h'
      integer partline,check_int,check_real,check_whole,atoi
      real ator
      character substr*(ISF_LINE_LEN)
      integer line_num

c     Chars 1-3: the strings  ' (#' or ' (+',
c     depending on whether this is the first or second plane given.
c     Chars 4-15: spaces.
      if (line(1:15) .eq. " (#            ") then
          line_num = 1
      else if (line(1:15) .eq. " (+            ") then
          line_num = 2
      else
          isf_bulletin_error = 'not a fault plane line: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 16-18: fault plane solution type.
      if (partline(f_type,line,16,3) .ne. 0) then
          if (f_type .ne. 'FM' .and. f_type .ne. 'BB' 
     +                         .and. f_type .ne. 'BDC') then
              isf_bulletin_error = 'bad f_type: '//line
              read_fault_plane = 20
              return
          end if
      else
          f_type = ' '
      end if

c     Char 19: space.
      if (line(19:19) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 19: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 20-25: strike, must be real.
      if (partline(substr,line,20,6) .eq. 0) then
          isf_bulletin_error = 'missing strike: '//line
          read_fault_plane = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_bulletin_error = 'bad strike: '//line
          read_fault_plane = 20
          return
      end if
      strike = ator(substr)

c     Char 26: space.
      if (line(26:26) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 26: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 27-31: dip, must be real.
      if (partline(substr,line,27,5) .eq. 0) then
          isf_bulletin_error = 'missing dip: '//line
          read_fault_plane = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
           isf_bulletin_error = 'bad dip: '//line
          read_fault_plane = 20
          return
      end if
      dip = ator(substr)

c     Char 32: space.
      if (line(32:32) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 32: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 33-39: rake, real - need not be there if both planes given.
      if (partline(substr,line,33,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad rake: '//line
              read_fault_plane = 20
              return
          end if
          rake = ator(substr)
      else
          rake = ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 40: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 41-43: np, int if there.
      if (partline(substr,line,41,3) .ne. 0) then
          if (check_int(substr) .eq. 1) then
               isf_bulletin_error = 'bad np: '//line
              read_fault_plane = 20
              return
          end if
          np = atoi(substr)
      else
          np = ISF_NULL
      end if

c     Char 44: space.
      if (line(44:44) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 44: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 45-47: ns, int if there.
      if (partline(substr,line,45,3) .ne. 0) then
          if (check_int(substr) .eq. 1) then
               isf_bulletin_error = 'bad np: '//line
              read_fault_plane = 20
              return
          end if
          ns = atoi(substr)
      else
          ns = ISF_NULL
      end if

c     Char 48: space.
      if (line(48:48) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 48: '//line
          read_fault_plane = 20
          return
      end if

c     Chars 49-53: plane identification.
      if (partline(f_plane,line,49,5) .ne. 0) then
          if (f_plane .ne. 'FAULT' .and. f_plane .ne. 'AUXIL' ) then
              isf_bulletin_error = 'bad f_plane: '//line
              read_fault_plane = 20
              return
          end if
      else
          f_plane = ' '
      end if

c     Chars  54-63: First plane has author, don't read for second plane.
      if (line_num .eq. 1) then

c     	Char 54: space.
          if (line(54:54) .ne. ' ' ) then
              isf_bulletin_error = 'bad format, char 54: '//line
              read_fault_plane = 20
              return
          end if

c     	Chars 55-63: author, any characters allowed but must be there.
          if (partline(author,line,55,9) .eq. 0) then
              isf_bulletin_error = 'missing author: '//line
              read_fault_plane = 20
              return
          end if

          if ( check_whole(author) .eq. 1 ) then
              isf_bulletin_error = 'bad author: '//line
              read_fault_plane = 20
              return
          end if
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,88,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_fault_plane = 20
          return
      end if

      read_fault_plane = 0
      return
      end


c     Tests a line to discover if it is a principal axes header comment.

c     Returns 0 if the line is a principal axes header.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_axes_head(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(83)
      integer headlen /83/

      head = ' (#PRINAX sc  T_val T_azim  T_pl  B_val B_azim  B_pl  P_va
     +l P_azim  P_pl Author   )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not a principal axes header: '//line
          read_axes_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_axes_head = 20
          return
      end if

      read_axes_head = 0
      return
      end

c     Tests a line to discover if it is a principal axes error header comment.
c     This line may or may not be present regardless of whether there is an
c     error data line or not.

c     Returns 0 if the line is a principal axes error header.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_axes_err_head(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(83)
      integer headlen /83/

      head = ' (+             eTv    eTa   eTp    eBv    eBa   eBp    eP
     +v    ePa   ePp fCLVD    )'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not a principal axes header: '//line
          read_axes_err_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_axes_err_head = 20
          return
      end if

      read_axes_err_head = 0
      return
      end


c     Parses a line asuming it to be a principal axes data comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted principal axes data line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_axes(line,scale_factor,t_val,t_azim,t_pl,
     +b_val,b_azim,b_pl,p_val,p_azim,p_pl,author)

      character line*(*), author*(*)
      integer scale_factor
      real t_val,t_azim,t_pl,b_val,b_azim,b_pl,p_val,p_azim,p_pl

      include 'isf_bul.h'
      integer partline,check_int,check_real,check_whole,atoi
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: should be the string  ' (#       '
      if (line(1:10) .ne. " (#       ") then
          isf_bulletin_error = 'not an axes line: '//line
          read_axes = 20
          return
      end if

c     Chars 11,12: scale factor - integer if there.
      if (partline(substr,line,11,2) .ne. 0) then
          if (check_int(substr) .eq. 1) then
               isf_bulletin_error = 'bad scale factor: '//line
              read_axes = 20
              return
          end if
          scale_factor = atoi(substr)
      else
          scale_factor = ISF_NULL
      end if

c     Char 13: space.
      if (line(13:13) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 13: '//line
          read_axes = 20
          return
      end if

c     Chars 14-19: t value - real if there.
      if (partline(substr,line,14,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad t_val: '//line
              read_axes = 20
              return
          end if
          t_val = ator(substr)
      else
          t_val = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 20: '//line
          read_axes = 20
          return
      end if

c     Chars 21-26: t azim, must be real.
      if (partline(substr,line,21,6) .eq. 0) then
          isf_bulletin_error = 'missing t_azim: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_bulletin_error = 'bad t_azim: '//line
          read_axes = 20
          return
      end if
      t_azim = ator(substr)

c     Char 27: space.
      if (line(27:27) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 27: '//line
          read_axes = 20
          return
      end if

c     Chars 28-32: t plunge, must be real.
      if (partline(substr,line,28,6) .eq. 0) then
          isf_bulletin_error = 'missing t_pl: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_bulletin_error = 'bad t_pl: '//line
          read_axes = 20
          return
      end if
      t_pl = ator(substr)

c     Char 33: space.
      if (line(33:33) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 33: '//line
          read_axes = 20
          return
      end if

c     Chars 34-39: b value - real if there.
      if (partline(substr,line,34,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad b_val: '//line
              read_axes = 20
              return
          end if
          b_val = ator(substr)
      else
          b_val = ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 40: '//line
          read_axes = 20
          return
      end if

c     Chars 41-46: b azim, must be real.
      if (partline(substr,line,41,6) .eq. 0) then
          isf_bulletin_error = 'missing b_azim: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_bulletin_error = 'bad b_azim: '//line
          read_axes = 20
          return
      end if
      b_azim = ator(substr)

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 47: '//line
          read_axes = 20
          return
      end if

c     Chars 48-52: b plunge, must be real.
      if (partline(substr,line,48,5) .eq. 0) then
          isf_bulletin_error = 'missing b_pl: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_bulletin_error = 'bad b_pl: '//line
          read_axes = 20
          return
      end if
      b_pl = ator(substr)

c     Char 53: space.
      if (line(53:53) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 53: '//line
          read_axes = 20
          return
      end if

c     Chars 54-59: p value - real if there.
      if (partline(substr,line,54,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad p_val: '//line
              read_axes = 20
              return
          end if
          p_val = ator(substr)
      else
          p_val = ISF_NULL
      end if

c     Char 60: space.
      if (line(60:60) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 60: '//line
          read_axes = 20
          return
      end if

c     Chars 61-66: p azim, must be real.
      if (partline(substr,line,61,6) .eq. 0) then
          isf_bulletin_error = 'missing p_azim: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_bulletin_error = 'bad p_azim: '//line
          read_axes = 20
          return
      end if
      p_azim = ator(substr)

c     Char 67: space.
      if (line(67:67) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 67: '//line
          read_axes = 20
          return
      end if

c     Chars 68-72: p plunge, must be real.
      if (partline(substr,line,68,6) .eq. 0) then
          isf_bulletin_error = 'missing p_pl: '//line
          read_axes = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_bulletin_error = 'bad p_pl: '//line
          read_axes = 20
          return
      end if
      p_pl = ator(substr)

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 73: '//line
          read_axes = 20
          return
      end if

c     Chars 74-82: author, any characters allowed but must be there.
      if (partline(author,line,74,9) .eq. 0) then
          isf_bulletin_error = 'missing author: '//line
          read_axes = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//line
          read_axes = 20
          return
      end if

c     Check for extra characters - not including close bracket.
      if (partline(substr,line,83,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_axes = 20
          return
      end if

      read_axes = 0
      return
      end


c     Parses a line asuming it to be a principal axes error comment.
c     Values are asigned to variables which have been sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted principal axes error line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_axes_err(line,t_val_unc,t_azim_unc,
     + t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc,p_val_unc,p_azim_unc,
     + p_pl_unc,fclvd)

      character line*(*)
      real t_val_unc,t_azim_unc,t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc
      real p_val_unc,p_azim_unc,p_pl_unc,fclvd

      include 'isf_bul.h'
      integer partline,check_real
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-14: should be the string  ' (+           '.
      if (line(1:14) .ne. " (+           ") then
          isf_bulletin_error = 'not an axes error line: '//line
          read_axes_err = 20
          return
      end if

c     Chars 15-19: t value uncertainty.
      if (partline(substr,line,15,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad t_val_unc: '//line
              read_axes_err = 20
              return
          end if
          t_val_unc = ator(substr)
      else
          t_val_unc = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 20: '//line
          read_axes_err = 20
          return
      end if

c     Chars 21-26: t azim uncertainty.
      if (partline(substr,line,21,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad t_azim_unc: '//line
              read_axes_err = 20
              return
          end if
          t_azim_unc = ator(substr)
      else
          t_azim_unc = ISF_NULL
      end if

c     Char 27: space.
      if (line(27:27) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 27: '//line
          read_axes_err = 20
          return
      end if

c     Chars 28-32: t plunge uncertainty.
      if (partline(substr,line,28,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad t_pl_unc: '//line
              read_axes_err = 20
              return
          end if
          t_pl_unc = ator(substr)
      else
          t_pl_unc = ISF_NULL
      end if

c     Char 33,34: must be a spaces.
      if (line(33:34) .ne. '  ' ) then
          isf_bulletin_error = 'bad format, char 33,34: '//line
          read_axes_err = 20
          return
      end if

c     Chars 35-39: b value uncertainty.
      if (partline(substr,line,35,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad b_val_unc: '//line
              read_axes_err = 20
              return
          end if
          b_val_unc = ator(substr)
      else
          b_val_unc = ISF_NULL
      end if

c     Char 40: space.
      if (line(40:40) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 40: '//line
          read_axes_err = 20
          return
      end if

c     Chars 41-46: b azim uncertainty.
      if (partline(substr,line,41,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad b_azim_unc: '//line
              read_axes_err = 20
              return
          end if
          b_azim_unc = ator(substr)
      else
          b_azim_unc = ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 47: '//line
          read_axes_err = 20
          return
      end if

c     Chars 48-52: b plunge uncertainty.
      if (partline(substr,line,48,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad b_pl_unc: '//line
              read_axes_err = 20
              return
          end if
          b_pl_unc = ator(substr)
      else
          b_pl_unc = ISF_NULL
      end if

c     Char 53,54: must be a spaces.
      if (line(53:54) .ne. '  ' ) then
          isf_bulletin_error = 'bad format, char 53,54: '//line
          read_axes_err = 20
          return
      end if

c     Chars 55-59: p value uncertainty.
      if (partline(substr,line,55,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad p_val_unc: '//line
              read_axes_err = 20
              return
          end if
          p_val_unc = ator(substr)
      else
          p_val_unc = ISF_NULL
      end if

c     Char 60: space.
      if (line(60:60) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 60: '//line
          read_axes_err = 20
          return
      end if

c     Chars 61-66: p azim uncertainty.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad p_azim_unc: '//line
              read_axes_err = 20
              return
          end if
          p_azim_unc = ator(substr)
      else
          p_azim_unc = ISF_NULL
      end if

c     Char 67: space.
      if (line(67:67) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 67: '//line
          read_axes_err = 20
          return
      end if

c     Chars 68-72: p plunge uncertainty.
      if (partline(substr,line,68,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad p_pl_unc: '//line
              read_axes_err = 20
              return
          end if
          p_pl_unc = ator(substr)
      else
          p_pl_unc = ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 73: '//line
          read_axes_err = 20
          return
      end if

c     Chars 74-78: fclvd.
      if (partline(substr,line,74,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad fclvd: '//line
              read_axes_err = 20
              return
          end if
          fclvd = ator(substr)
      else
          fclvd = ISF_NULL
      end if

c     Check for extra characters - not including close bracket.
      if (partline(substr,line,79,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_axes_err = 20
          return
      end if

      read_axes_err = 0
      return
      end

c     Tests a line to discover if it is a magnitude block header line.

c     Returns 0 if the line is a magnitude block header.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_netmag_head(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(38)
      integer headlen /38/

      head = 'Magnitude  Err Nsta Author      OrigID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not a magnitude header: '//line
          read_netmag_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_netmag_head = 20
          return
      end if

      read_netmag_head = 0
      return
      end

c     Parses a line assuming that it is a magnitude sub-block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted magnitude line,
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_netmag(line,magtype,magind,mag,
     +    magerr,nsta,author,origid)

      character line*(*), magtype*(*), author*(*), origid*(*)
      character magind
      real mag,magerr
      integer nsta

      include 'isf_bul.h'
      integer partline,check_real,check_int,check_whole,atoi
      real ator
      character substr*(ISF_LINE_LEN)


c     Chars 1-5: magnitude type, any characters allowed but must be there.
      if (partline(magtype,line,1,5) .eq. 0) then
          isf_bulletin_error = 'missing magtype: '//line
          read_netmag = 20
          return
      end if

      if ( check_whole(magtype) .eq. 1 ) then
          isf_bulletin_error = 'bad magtype: '//line
          read_netmag = 20
          return
      end if

c     Char 6: less than or greater than indicator or space only.
      if (line(6:6) .ne. ' ' .and. line(6:6) .ne. '<' .and. line(6:6) 
     +.ne. '>') then
          isf_bulletin_error = 'bad magind: '//line
          read_netmag = 20
          return
      end if
      magind = line(6:6)

c     Chars 7-10: magnitude, must be real.
      if (partline(substr,line,7,4) .eq. 0) then
          isf_bulletin_error = 'missing magnitude: '//line
          read_netmag = 20
          return
      end if

      if (check_real(substr) .eq. 1) then
          isf_bulletin_error = 'bad magnitude: '//line
          read_netmag = 20
          return
      end if
      mag = ator(substr)

c     Char 11: space.
      if (line(11:11) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 11: '//line
          read_netmag = 20
          return
      end if

c     Chars 12-14: magnitude error, real if anything.
      if (partline(substr,line,12,3) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad magnitude error: '//line
              read_netmag = 20
              return
          end if
          magerr = ator(substr)
      else
          magerr = ISF_NULL
      end if

c     Char 15: space.
      if (line(15:15) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 15: '//line
          read_netmag = 20
          return
      end if

c     Chars 16-19: number of stations, integer if anything.
      if (partline(substr,line,16,4) .ne. 0) then
          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad nsta: '//line
              read_netmag = 20
              return
          end if
          nsta = atoi(substr)
      else
          nsta = ISF_NULL
      end if

c     Char 20: space.
      if (line(20:20) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 20: '//line
          read_netmag = 20
          return
      end if

c     Chars 21-29: author, any characters allowed but must be there.
      if (partline(author,line,21,9) .eq. 0) then
          isf_bulletin_error = 'missing author: '//line
          read_netmag = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//line
          read_netmag = 20
          return
      end if

c     Char 30: space.
      if (line(30:30) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 30: '//line
          read_netmag = 20
          return
      end if

c     Chars 31-38: origin ID, any characters allowed but must be there.
      if (partline(origid,line,31,8) .eq. 0) then
          isf_bulletin_error = 'missing origid: '//line
          read_netmag = 20
          return
      end if

      if ( check_whole(origid) .eq. 1 ) then
          isf_bulletin_error = 'bad origid: '//line
          read_netmag = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,39,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_netmag = 20
          return
      end if

      read_netmag = 0
      return
      end

c     The stations contributing to a given netmag can follow in one or 
c     more formated comment lines and are separated by spaces.
c     The array of station code strings sta has size n, if the subroutine
c     is being run on a second or subsequent line then n will already be 
c     set and will be updated by this routine as more codes are added.

      integer function read_netmag_sta(line,sta,n)

      character line*(*), sta(*)*(*)
      integer n,isupper

      include 'isf_bul.h'
      integer start,break,end

c     If it is the first then initialise array of station codes.
c     If not it must be a follow on line or something is wrong.
      if (line(1:12) .eq. " (#STATIONS ") then
          n = 0
      else if (line(1:12) .eq. " (+         ") then
          if (n .gt. ISF_NUM_STA) then
              isf_bulletin_error = 'too many stations: '//line
              read_netmag_sta = 20
              return
          end if
      else
          isf_bulletin_error = 'bad station list format: '//line
          read_netmag_sta = 20
          return
      end if

c     Don't read close bracket, if there is one there.
      start = 13
      end = len(line)
      do while ((line(end:end) .eq. ' ' .or. line(end:end) .eq. ')' ) 
     +            .and. end .gt. start)
          end = end - 1
      end do

      do while (line(start:start) .eq. ' ' .and. start .lt. end)
          start = start + 1
      end do

      if ( end .gt. ISF_LINE_LEN ) then 
          isf_bulletin_error = 'line too long: '//line
          read_netmag_sta = 20
          return
      end if

c     Fill array of station codes.
      break = index(line(start:),' ')
      do while (break .ne. 0 .and. start .le. end)
          break = break + start

          if ( break - start - 1 .gt. ISF_NET_LEN + ISF_STA_LEN ) then
              isf_bulletin_error = 'station code too long: '//line
              read_netmag_sta = 20
              return
          end if

          n = n + 1
          sta(n) = line(start:break-2)

c     	Check that this looks like a station code.
          if (isupper(sta(n)(1:1)) .eq. 0) then
              isf_bulletin_error = 'illegal station: '//sta(n)
              read_netmag_sta = 20
              return
          end if

          start = break
          break = index(line(start:),' ')
      end do

      read_netmag_sta = 0
      return
      end

c     Parses a line assuming it to be an netmag basis formatted comment.

c     Returns 0 if the line is a properly formatted netmag basis line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error if not.

      integer function read_netmag_basis(line,param,value)

      character line*(*), param*(*), value*(*)

      include 'isf_bul.h'
      integer check_whole
      integer start,break,end

c     Chars 1-8: should be the string ' (#BASIS '.
      if (line(1:9) .ne. " (#BASIS ") then
          isf_bulletin_error = 'not a netmag basis line: '//line
          read_netmag_basis = 20
          return
      end if

c     Don't read close bracket, if there is one there.
      start = 10
      end = len(line)
      do while ((line(end:end) .eq. ' ' .or. line(end:end) .eq. ')' ) 
     +            .and. end .gt. start)
          end = end - 1
      end do

      do while (line(start:start) .eq. ' ' .and. start .lt. end)
          start = start + 1
      end do

      if ( end .gt. ISF_LINE_LEN ) then 
          isf_bulletin_error = 'line too long: '//line
          read_netmag_basis = 20
          return
      end if

      break = index(line(start:),'=')
      break = start+break
      param = line(start:break-2)
      value = line(break:end)

c     value is everything after = so make sure not too much
      if ( check_whole(value) .eq. 1 ) then
          isf_bulletin_error = 'bad value: '//line
          read_netmag_basis = 20
          return
      end if

      read_netmag_basis = 0
      return
      end

c     Tests a line to discover if it is a effects block header line.

c     Returns 0 if the line is a effects block header.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_effects_head(line)

      character line*(*)
      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(69)
      integer headlen /69/

      head = 'Effects              Loctyp Location           Intensity S
     +cale Author'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not an effects header: '//line
          read_effects_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_effects_head = 20
          return
      end if

      read_effects_head = 0
      return
      end

c     Parses a line assuming that it is an effects block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted effects line,
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_effects(line,heard,felt,damage,casualties,
     + uplift,subsidence,fault,tsunami,seiche,volcano,acoustic,
     + gravity,t_wave,liquification,geyser,landslide,sandblow,cracks,
     + lights,odours,loctype,lat,lon,dist,azim,country,postcode,net,
     + sta,intensity1,modifier,intensity2,scale,author)

      character line*(*),author*(*),loctype*(*)
      character scale*(*),country*(*),postcode*(*),net*(*),sta*(*)
      character heard,felt,damage,casualties,uplift,subsidence
      character fault,tsunami,seiche,volcano
      character acoustic,gravity,t_wave,liquification,geyser
      character landslide,sandblow,cracks,lights,odours
      character modifier
      real lat,lon,dist,azim,intensity1,intensity2

      include 'isf_bul.h'
      integer partline,check_real,check_whole
      real ator
      character substr*(ISF_LINE_LEN)

c     Char 1: heard flag.
      if (line(1:1) .ne. '_' .and. line(1:1) .ne. 'H' ) then
          isf_bulletin_error = 'bad heard flag: '//line
          read_effects = 20
          return
      end if
      heard = line(1:1)

c     Char 2: felt flag.
      if (line(2:2) .ne. '_' .and. line(2:2) .ne. 'F' ) then
          isf_bulletin_error = 'bad felt flag: '//line
          read_effects = 20
          return
      end if
      felt = line(2:2)

c     Char 3: damage flag.
      if (line(3:3) .ne. '_' .and. line(3:3) .ne. 'D' ) then
          isf_bulletin_error = 'bad damage flag: '//line
          read_effects = 20
          return
      end if
      damage = line(3:3)

c     Char 4: casualties flag.
      if (line(4:4) .ne. '_' .and. line(4:4) .ne. 'C' ) then
          isf_bulletin_error = 'bad casualties flag: '//line
          read_effects = 20
          return
      end if
      casualties = line(4:4)

c     Char 5: uplift flag.
      if (line(5:5) .ne. '_' .and. line(5:5) .ne. 'U' ) then
          isf_bulletin_error = 'bad uplift flag: '//line
          read_effects = 20
          return
      end if
      uplift = line(5:5)

c     Char 6: subsidence flag.
      if (line(6:6) .ne. '_' .and. line(6:6) .ne. 'S' ) then
          isf_bulletin_error = 'bad subsidence flag: '//line
          read_effects = 20
          return
      end if
      subsidence = line(6:6)

c     Char 7: surface faulting flag.
      if (line(7:7) .ne. '_' .and. line(7:7) .ne. 'F' ) then
          isf_bulletin_error = 'bad surface faulting flag: '//line
          read_effects = 20
          return
      end if
      fault = line(7:7)

c     Char 8: tsunami flag.
      if (line(8:8) .ne. '_' .and. line(8:8) .ne. 'T' .and. line(8:8) 
     +.ne. 'Q') then
          isf_bulletin_error = 'bad tsunami flag: '//line
          read_effects = 20
          return
      end if
      tsunami = line(8:8)

c     Char 9: seiche flag.
      if (line(9:9) .ne. '_' .and. line(9:9) .ne. 'S' .and. line(9:9) 
     +.ne. 'Q') then
          isf_bulletin_error = 'bad seiche flag: '//line
          read_effects = 20
          return
      end if
      seiche = line(9:9)

c     Char 10: volcano flag.
      if (line(10:10) .ne. '_' .and. line(10:10) .ne. 'V' ) then
          isf_bulletin_error = 'bad volcano flag: '//line
          read_effects = 20
          return
      end if
      volcano = line(10:10)

c     Char 11: acoustic flag.
      if (line(11:11) .ne. '_' .and. line(11:11) .ne. 'A' ) then
          isf_bulletin_error = 'bad acoustic flag: '//line
          read_effects = 20
          return
      end if
      acoustic = line(11:11)

c     Char 12: gravity flag.
      if (line(12:12) .ne. '_' .and. line(12:12) .ne. 'G' ) then
          isf_bulletin_error = 'bad gravity flag: '//line
          read_effects = 20
          return
      end if
      gravity = line(12:12)

c     Char 13: t_wave flag.
      if (line(13:13) .ne. '_' .and. line(13:13) .ne. 'T' ) then
          isf_bulletin_error = 'bad t_wave flag: '//line
          read_effects = 20
          return
      end if
      t_wave = line(13:13)

c     Char 14: liquification flag.
      if (line(14:14) .ne. '_' .and. line(14:14) .ne. 'L' ) then
          isf_bulletin_error = 'bad liquification flag: '//line
          read_effects = 20
          return
      end if
      liquification = line(14:14)

c     Char 15: geyser flag.
      if (line(15:15) .ne. '_' .and. line(15:15) .ne. 'G' ) then
          isf_bulletin_error = 'bad geyser flag: '//line
          read_effects = 20
          return
      end if
      geyser = line(15:15)

c     Char 16: landslide flag.
      if (line(16:16) .ne. '_' .and. line(16:16) .ne. 'S' ) then
          isf_bulletin_error = 'bad landslide flag: '//line
          read_effects = 20
          return
      end if
      landslide = line(16:16)

c     Char 17: sandblow flag.
      if (line(17:17) .ne. '_' .and. line(17:17) .ne. 'B' ) then
          isf_bulletin_error = 'bad sandblow flag: '//line
          read_effects = 20
          return
      end if
      sandblow = line(17:17)

c     Char 18: cracks flag.
      if (line(18:18) .ne. '_' .and. line(18:18) .ne. 'C' ) then
          isf_bulletin_error = 'bad cracks flag: '//line
          read_effects = 20
          return
      end if
      cracks = line(18:18)

c     Char 19: lights flag.
      if (line(19:19) .ne. '_' .and. line(19:19) .ne. 'V' ) then
          isf_bulletin_error = 'bad lights flag: '//line
          read_effects = 20
          return
      end if
      lights = line(19:19)

c     Char 20: odours flag.
      if (line(20:20) .ne. '_' .and. line(20:20) .ne. 'V' ) then
          isf_bulletin_error = 'bad odours flag: '//line
          read_effects = 20
          return
      end if
      odours = line(20:20)

c     Char 21: space.
      if (line(21:21) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 21: '//line
          read_effects = 20
          return
      end if

c     Chars 22-27: loctype. Checked below to see if sensible.
      if (partline(loctype,line,22,6) .eq. 0) then
          isf_bulletin_error = 'missing loctype: '//line
          read_effects = 20
          return
      end if

c     Char 28: space.
      if (line(28:28) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 28: '//line
          read_effects = 20
          return
      end if

c     Chars 29-46: depend on loctype.
      if (loctype .eq. 'Summar') then

c     	Chars 29-46 should be blank.
          if (partline(substr,line,29,18) .ne. 0) then
              isf_bulletin_error = 'bad summar format: '//line
              read_effects = 20
              return
          end if

      elseif (loctype .eq. 'LatLon') then

c     	Chars 29-36: lattitude - must be real.
          if (partline(substr,line,29,8) .eq. 0) then
              isf_bulletin_error = 'missing lattitude: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad lattitude: '//line
              read_effects = 20
              return
          end if
          lat = ator(substr)

c     	Char 37: space.
          if (line(37:37) .ne. ' ' ) then
              isf_bulletin_error = 'bad format, char 37: '//line
              read_effects = 20
              return
          end if

c     	Chars 38-46: longitude - must be real.
          if (partline(substr,line,38,9) .eq. 0) then
              isf_bulletin_error = 'missing longitude: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad longitude: '//line
              read_effects = 20
              return
          end if
          lon = ator(substr)

      elseif (loctype .eq. 'DistAz') then

c     	Chars 29-36: distance - must be real.
          if (partline(substr,line,29,8) .eq. 0) then
              isf_bulletin_error = 'missing distance: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad distance: '//line
              read_effects = 20
              return
          end if
          dist = ator(substr)

c     	Char 37: space.
          if (line(37:37) .ne. ' ' ) then
              isf_bulletin_error = 'bad format, char 37: '//line
              read_effects = 20
              return
          end if

c     	Chars 38-42: azimuth.
          if (partline(substr,line,38,5) .eq. 0) then
              isf_bulletin_error = 'missing azimuth: '//line
              read_effects = 20
              return
          end if

          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad azimuth: '//line
              read_effects = 20
              return
          end if
          azim = ator(substr)

c     	Chars 43-46 should be blank.
          if (partline(substr,line,43,4) .ne. 0) then
              isf_bulletin_error = 'bad DistAz format: '//line
              read_effects = 20
              return
          end if

      elseif (loctype .eq. 'CoPost') then

c     	Chars 29-31: country code.
          if (partline(country,line,29,3) .eq. 0) then
              isf_bulletin_error = 'missing country: '//line
              read_effects = 20
              return
          end if

c     	Char 32: space.
          if (line(32:32) .ne. ' ' ) then
              isf_bulletin_error = 'bad format, char 32: '//line
              read_effects = 20
              return
          end if

c     	Chars 33-42: post code.
          if (partline(postcode,line,33,10) .eq. 0) then
              isf_bulletin_error = 'missing post code: '//line
              read_effects = 20
              return
          end if

c     	Chars 43-46 should be blank.
          if (partline(substr,line,43,4) .ne. 0) then
              isf_bulletin_error = 'bad CoPost format: '//line
              read_effects = 20
              return
          end if

      elseif (loctype .eq. 'StaNet') then

c     	Chars 29-37: network code.
          if (partline(net,line,29,9) .eq. 0) then
              isf_bulletin_error = 'missing network: '//line
              read_effects = 20
              return
          end if

c     	Char 38: space.
          if (line(38:38) .ne. ' ' ) then
              isf_bulletin_error = 'bad format, char 38: '//line
              read_effects = 20
              return
          end if

c     	Chars 39-43: station code.
          if (partline(sta,line,39,5) .eq. 0) then
              isf_bulletin_error = 'missing station code: '//line
              read_effects = 20
              return
          end if

c     	Chars 44-46 should be blank.
          if (partline(substr,line,43,3) .ne. 0) then
              isf_bulletin_error = 'bad StaNet format: '//line
              read_effects = 20
              return
          end if
      else
          isf_bulletin_error = 'unknown loctype: '//line
          read_effects = 20
          return
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 47: '//line
          read_effects = 20
          return
      end if

c     48-51: first intensity.
c     If first intensity null then don't allow second one or scale.
      if (partline(substr,line,48,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad intensity: '//line
              read_effects = 20
              return
          end if
          intensity1 = ator(substr)

c     	Char 52: intensity modifier.
          if (line(52:52) .ne. ' ' .and. line(52:52) .ne. '-' .and.
     +    line(52:52) .ne. '+' ) then
              isf_bulletin_error = 'bad intensity modifier: '//line
              read_effects = 20
              return
          end if
          modifier = line(52:52)

c     	Chars 53-56: second intensity, only allowed if modifier is '-'.
          if (modifier .eq. '-') then

              if (partline(substr,line,53,4) .eq. 0) then
                  isf_bulletin_error = 'missing intensity 2: '//line
                  read_effects = 20
                  return
              end if

              if (check_real(substr) .eq. 1) then
                  isf_bulletin_error = 'bad intensity 2: '//line
                  read_effects = 20
                  return
              end if
              intensity2 = ator(substr)
          else 
              if (partline(substr,line,53,4) .ne. 0) then
                  isf_bulletin_error = 'bad intensity format: '//line
                  read_effects = 20
                  return
              end if
              intensity2 = ISF_NULL
          end if

c     	Char 57: space.
          if (line(57:57) .ne. ' ' ) then
              isf_bulletin_error = 'bad format, char 57: '//line
              read_effects = 20
              return
          end if

c     	Chars 58-62: intensity scale.
          if (partline(scale,line,57,5) .ne. 0) then
              if ( check_whole(scale) .eq. 1 ) then
                  isf_bulletin_error = 'bad intensity scale: '//line
                  read_effects = 20
                  return
              end if
          else
              scale = ' '
          end if
      else
          if (partline(substr,line,52,11) .ne. 0) then
              isf_bulletin_error = 'bad intensity format: '//line
              read_effects = 20
              return
          end if
          intensity1 = ISF_NULL
          modifier = ' '
          intensity2 = ISF_NULL
          scale = ' '
      end if

c     Char 63: space.
      if (line(63:63) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 63: '//line
          read_effects = 20
          return
      end if

c     Chars 64-72: author, any characters allowed but must be there.
      if (partline(author,line,64,9) .eq. 0) then
          isf_bulletin_error = 'missing author: '//line
          read_effects = 20
          return
      end if

      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//line
          read_effects = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,73,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_effects = 20
          return
      end if

      read_effects = 0
      return
      end


c     Tests a line to discover if it is a phase block header line.

c     Returns 0 if the line is a phase block header.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_phase_head(line)

      character line*(*)

      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(122)
      integer headlen /122/

      head = 'Sta     Dist  EvAz Phase        Time      TRes  Azim AzRes
     +   Slow   SRes Def   SNR       Amp   Per Qual Magnitude    ArrID'


      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not an phase header: '//line
          read_phase_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_head = 20
          return
      end if

      read_phase_head = 0
      return
      end


c     Parses a line assuming that it is a phase block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted phase line,
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_phase(line,sta,dist,esaz,phase,hh,mi,ss,
     + msec,timeres,azim,azimres,slow,slowres,timedef,azimdef,slowdef,
     + snr,amp,per,picktype,sp_fm,detchar,magtype,magind,mag,arrid)

      character line*(*),sta*(*),arrid*(*),phase*(*),magtype*(*)
      character timedef,azimdef,slowdef,sp_fm,detchar,magind,picktype
      real dist,esaz,timeres,azim,azimres,slow,slowres,snr,amp,per,mag
      integer hh,mi,ss,msec

      include 'isf_bul.h'
      integer partline,check_real,check_int,check_whole,isdigit,atoi
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-5: station code.
      if (partline(sta,line,1,5) .eq. 0) then
          isf_bulletin_error = 'missing sta: '//line
          read_phase = 20
          return
      end if

      if ( check_whole(sta) .eq. 1 ) then
          isf_bulletin_error = 'bad sta: '//line
          read_phase = 20
          return
      end if

c     Char 6: space.
      if (line(6:6) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 6: '//line
          read_phase = 20
          return
      end if

c     Chars 7-12: distance, real if there.
      if (partline(substr,line,7,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad dist: '//line
              read_phase = 20
              return
          end if
          dist = ator(substr)
      else
          dist = ISF_NULL
      end if

c     Char 13: space.
      if (line(13:13) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 13: '//line
          read_phase = 20
          return
      end if

c     Chars 14-18: event to sta azimuth, real if there.
      if (partline(substr,line,14,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad esaz: '//line
              read_phase = 20
              return
          end if
          esaz = ator(substr)
      else
          esaz = ISF_NULL
      end if

c     Chars 20-27: phase code - can be null.
      if (partline(phase,line,20,8) .ne. 0) then
          if ( check_whole(phase) .eq. 1 ) then
              isf_bulletin_error = 'bad phase: '//line
              read_phase = 20
              return
          end if
      else
          phase=' '
      end if

c     Char 28: space.
      if (line(28:28) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 28: '//line
          read_phase = 20
          return
      end if

c     Chars 29-40: time - can be null.
      if (partline(substr,line,29,12) .ne. 0) then
c     	Chars 29,30: hour.
          if (partline(substr,line,29,2) .eq. 0) then
              isf_bulletin_error = 'missing hour: '//line
              read_phase = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad hour: '//line
              read_phase = 20
              return
          end if
          hh = atoi(substr)

c     	Char 31: ':' character.
          if (line(31:31) .ne. ':' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase = 20
              return
          end if

c     	Chars 32,33: minute.
          if (partline(substr,line,32,2) .eq. 0) then
              isf_bulletin_error = 'missing minute: '//line
              read_phase = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad minute: '//line
              read_phase = 20
              return
          end if
          mi = atoi(substr)

c     	Char 34: ':' character.
          if (line(34:34) .ne. ':' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase = 20
              return
          end if
      
c     	Chars 35,36: integral second.
          if (partline(substr,line,35,2) .eq. 0) then
              isf_bulletin_error = 'missing second: '//line
              read_phase = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad second: '//line
              read_phase = 20
              return
          end if
          ss = atoi(substr)

c     	Char 37-40: msec or spaces.
c     	Allow decimal place without any numbers after it.
          if (partline(substr,line,38,3) .ne. 0) then

c     		Char 37: '.' character.
              if (line(37:37) .ne. '.' ) then
                  isf_bulletin_error = 'bad date: '//line
                  read_phase = 20
                  return
              end if

c     		Chars 38-40: msec.
              if (isdigit(line(38:38)) .eq. 0) then
                  isf_bulletin_error = 'bad msec: '//line
                  read_phase = 20
                  return
              end if
              msec = (ichar(line(38:38)) - ichar('0'))*100

              if (isdigit(line(39:39)) .ne. 0) then
                  msec = msec + (ichar(line(39:39)) - ichar('0'))*10
              else if (line(39:39) .ne. ' ' .or. line(40:40) .ne. ' ') 
     +        then
                  isf_bulletin_error = 'bad msec: '//line
                  read_phase = 20
                  return
              end if

              if (isdigit(line(40:40)) .ne. 0) then
                  msec = msec + (ichar(line(40:40)) - ichar('0'))
              else if (line(40:40) .ne. ' ') then
                  isf_bulletin_error = 'bad msec: '//line
                  read_phase = 20
                  return
              end if
          else
c     		Char 37: '.' character or space.
              if (line(37:37) .ne. ' ' .and. line(37:37) .ne. '.' ) 
     +        then
                  isf_bulletin_error = 'bad date: '//line
                  read_phase = 20
                  return
              end if
              msec = ISF_NULL
          end if
      else
          hh = ISF_NULL
          mi = ISF_NULL
          ss = ISF_NULL
          msec = ISF_NULL
      end if

c     Char 41: space.
      if (line(41:41) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 41: '//line
          read_phase = 20
          return
      end if

c     Chars 42-46: time residual, real if there.
      if (partline(substr,line,42,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad timeres: '//line
              read_phase = 20
              return
          end if
          timeres = ator(substr)
      else
          timeres = ISF_NULL
      end if

c     Char 47: space.
      if (line(47:47) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 47: '//line
          read_phase = 20
          return
      end if

c     Chars 48-52: observed azimuth, real if there.
      if (partline(substr,line,48,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad azim: '//line
              read_phase = 20
              return
          end if
          azim = ator(substr)
      else
          azim = ISF_NULL
      end if

c     Char 53: space.
      if (line(53:53) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 53: '//line
          read_phase = 20
          return
      end if

c     Chars 54-58: azimuth residual, real if there.
      if (partline(substr,line,54,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad azimres: '//line
              read_phase = 20
              return
          end if
          azimres = ator(substr)
      else
          azimres = ISF_NULL
      end if

c     Char 59: space.
      if (line(59:59) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 59: '//line
          read_phase = 20
          return
      end if

c     Chars 60-65: slowness, real if there.
      if (partline(substr,line,60,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad slow: '//line
              read_phase = 20
              return
          end if
          slow = ator(substr)
      else
          slow = ISF_NULL
      end if

c     Char 66: space.
      if (line(66:66) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 66: '//line
          read_phase = 20
          return
      end if

c     Chars 67-72: slowness residual, real if there.
      if (partline(substr,line,67,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad slowres: '//line
              read_phase = 20
              return
          end if
          slowres = ator(substr)
      else
          slowres = ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 73: '//line
          read_phase = 20
          return
      end if

c     Char 74: time defining flag.
      if (line(74:74) .eq. '_' .or. line(74:74) .eq. 'T' ) then
          timedef=line(74:74)
      else if (line(74:74) .eq. ' ') then
          timedef='_'
      else
          isf_bulletin_error = 'bad timedef flag: '//line
          read_phase = 20
          return
      end if

c     Char 75: azimuth defining flag.
      if (line(75:75) .eq. '_' .or. line(75:75) .eq. 'A' ) then
          azimdef=line(75:75)
      else if (line(75:75) .eq. ' ') then
          azimdef='_'
      else
          isf_bulletin_error = 'bad azimdef flag: '//line
          read_phase = 20
          return
      end if

c     Char 76: slowness defining flag.
      if (line(76:76) .eq. '_' .or. line(76:76) .eq. 'S' ) then
          slowdef=line(76:76)
      else if (line(76:76) .eq. ' ') then
          slowdef='_'
      else
          isf_bulletin_error = 'bad slowdef flag: '//line
          read_phase = 20
          return
      end if

c     Char 77: space.
      if (line(77:77) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 77: '//line
          read_phase = 20
          return
      end if

c     Chars 78-82: signal-to noise, real if there.
      if (partline(substr,line,78,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad snr: '//line
              read_phase = 20
              return
          end if
          snr = ator(substr)
      else
          snr = ISF_NULL
      end if

c     Char 83: space.
      if (line(83:83) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 83: '//line
          read_phase = 20
          return
      end if

c     Chars 84-92: amplitude, real if there.
      if (partline(substr,line,84,9) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad amp: '//line
              read_phase = 20
              return
          end if
          amp = ator(substr)
      else
          amp = ISF_NULL
      end if

c     Char 93: space.
      if (line(93:93) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 93: '//line
          read_phase = 20
          return
      end if

c     Chars 94-98: period, real if there.
      if (partline(substr,line,94,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad per: '//line
              read_phase = 20
              return
          end if
          per = ator(substr)
      else
          per = ISF_NULL
      end if

c     Char 99: space.
      if (line(99:99) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 99: '//line
          read_phase = 20
          return
      end if

c     Char 100: picktype.
      if (line(100:100) .eq. 'a' .or. line(100:100) .eq. 'm' .or.
     +    line(100:100) .eq. '_') then

          picktype=line(100:100)
      else if (line(100:100) .eq. ' ') then
          picktype='_'
      else
          isf_bulletin_error = 'bad picktype: '//line
          read_phase = 20
          return
      end if

c     Char 101: sp_fm.
      if (line(101:101) .eq. 'c' .or. line(101:101) .eq. 'd' .or.
     +    line(101:101) .eq. '_') then

          sp_fm=line(101:101)
      else if (line(101:101) .eq. ' ') then
          sp_fm='_'
      else
          isf_bulletin_error = 'bad sp_fm: '//line
          read_phase = 20
          return
      end if

c     Char 102: detchar.
      if (line(102:102) .eq. 'i' .or. line(102:102) .eq. 'e' .or.
     +    line(102:102) .eq. 'q' .or. line(102:102) .eq. '_') then

          detchar=line(102:102)
      else if (line(102:102) .eq. ' ') then
          detchar='_'
      else
          isf_bulletin_error = 'bad detchar: '//line
          read_phase = 20
          return
      end if

c     Char 103: space.
      if (line(103:103) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 103: '//line
          read_phase = 20
          return
      end if

c     Chars 104-108: magnitude type.
      if (partline(magtype,line,104,5) .ne. 0) then
          if ( check_whole(magtype) .eq. 1 ) then
              isf_bulletin_error = 'bad magtype: '//line
              read_phase = 20
              return
          end if
      else
          magtype=' '
      end if

c     Char 109: less than or greater than indicator or space only.
      if (line(109:109) .eq. ' ' .or. line(109:109) .eq. '>' .or.
     +    line(109:109) .eq. '<' ) then

          magind=line(109:109)
      else
          isf_bulletin_error = 'bad magind: '//line
          read_phase = 20
          return
      end if

c     Chars 110-113: magnitude, real if there.
      if (partline(substr,line,110,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad mag: '//line
              read_phase = 20
              return
          end if
          mag = ator(substr)
      else
          mag = ISF_NULL
      end if

c     Char 114: space.
      if (line(114:114) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 114: '//line
          read_phase = 20
          return
      end if

c     Chars 115-122: arrival ID, any characters allowed but must be there.
      if (partline(arrid,line,115,9) .eq. 0) then
          isf_bulletin_error = 'missing arrid: '//line
          read_phase = 20
          return
      end if

      if ( check_whole(arrid) .eq. 1 ) then
          isf_bulletin_error = 'bad arrid: '//line
          read_phase = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,123,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase = 20
          return
      end if

      read_phase = 0
      return
      end


c     Parses a line assuming it to be a phase origid line.

c     Returns 0 if the line is a phase orig line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_phase_origid(line,origid)

      character line*(*), origid*(*)

      include 'isf_bul.h'
      integer partline,check_whole
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: comment start string and space.
      if (line(1:10) .ne. " (#OrigID ") then
          isf_bulletin_error = 'not a phase origin line: '//line
          read_phase_origid = 20
          return
      end if

c     Chars 11-18: origin ID.
      if (partline(origid,line,11,8) .eq. 0) then
          isf_bulletin_error = 'missing origid: '//line
          read_phase_origid = 20
          return
      end if

      if ( check_whole(origid) .eq. 1 ) then
          isf_bulletin_error = 'bad origid: '//line
          read_phase_origid = 20
          return
      end if

c     Check for extra characters - not including close bracket.
      if (partline(substr,line,19,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_origid = 20
          return
      end if

      read_phase_origid = 0
      return
      end


c     Tests a line to discover if it is a phase info block header line.

c     Returns 0 if the line is a phase info block header.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_phase_info_head(line)

      character line*(*)

      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)
      character head*(125)
      integer headlen /124/

      head = 'Net      Chan F Low_F HighF AuthPhas    Date     eTime wTi
     +me eAzim wAzim  eSlow wSlow      eAmp  ePer eMag Author     ArrID'

      if (line(1:headlen) .ne. head(1:headlen)) then
          isf_bulletin_error = 'not a phase info header: '//line
          read_phase_info_head = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,headlen+1,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_info_head = 20
          return
      end if

      read_phase_info_head = 0
      return
      end


c     Parses a line assuming that it is a phase info block data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.
                                      
c     Returns 0 if the line is a properly formatted phase info line,
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_phase_info(line,net,chan,filter,filter_min,
     +                filter_max,phase,yyyy,mm,dd,time_unc,time_weight, 
     +                azim_unc,azim_weight,slow_unc,slow_weight,
     +                amp_unc,per_unc,mag_unc,author,arrid)

      character line*(*),net*(*),chan*(*),author*(*),arrid*(*)
      character phase*(*),filter
      real filter_min,filter_max,time_unc,time_weight,azim_unc
      real azim_weight,slow_unc,slow_weight,amp_unc,per_unc,mag_unc
      integer yyyy,mm,dd

      include 'isf_bul.h'
      integer partline,check_real,check_int,check_whole,atoi
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-9: network code.
      if (partline(net,line,1,9) .ne. 0) then
          if ( check_whole(net) .eq. 1 ) then
              isf_bulletin_error = 'bad net: '//line
              read_phase_info = 20
              return
          end if
      else
          net = ' '
      end if

c     Char 10: space.
      if (line(10:10) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 10: '//line
          read_phase_info = 20
          return
      end if

c     Chars 11-13: channel.
      if (partline(chan,line,11,3) .ne. 0) then
          if ( check_whole(chan) .eq. 1 ) then
              isf_bulletin_error = 'bad chan: '//line
              read_phase_info = 20
              return
          end if
      else
          chan = ' '
      end if

c     Char 14: space.
      if (line(14:14) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 14: '//line
          read_phase_info = 20
          return
      end if

c     Char 15: filter.
      if (line(15:15) .eq. '0' .or. line(15:15) .eq. 'C' 
     +                         .or. line(15:15) .eq. ' ') then
          filter=line(15:15)
      else
          isf_bulletin_error = 'bad filter: '//line
          read_phase_info = 20
          return
      end if

c     Char 16: space.
      if (line(16:16) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 16: '//line
          read_phase_info = 20
          return
      end if

c     Chars 17-21: minimum filter frequency, real if there.
      if (partline(substr,line,17,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad filter_min: '//line
              read_phase_info = 20
              return
          end if
          filter_min = ator(substr)
      else
          filter_min = ISF_NULL
      end if

c     Char 22: space.
      if (line(22:22) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 22: '//line
          read_phase_info = 20
          return
      end if

c     Chars 23-27: maximum filter frequency, real if there.
      if (partline(substr,line,23,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad filter_max: '//line
              read_phase_info = 20
              return
          end if
          filter_max = ator(substr)
      else
          filter_max = ISF_NULL
      end if

c     Char 28: space.
      if (line(28:28) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 28: '//line
          read_phase_info = 20
          return
      end if

c     Chars 29-36: author's phase.
      if (partline(phase,line,29,8) .ne. 0) then
          if ( check_whole(phase) .eq. 1 ) then
              isf_bulletin_error = 'bad phase: '//line
              read_phase_info = 20
              return
          end if
      else
          phase=' '
      end if

c     Char 37: space.
      if (line(37:37) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 37: '//line
          read_phase_info = 20
          return
      end if

c     Chars 38-47: arrival date.
      if (partline(substr,line,38,10) .ne. 0) then

c     	38-41: year.
          if (partline(substr,line,38,4) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if
          yyyy = atoi(substr)

c     	Char 42: '/' character.
          if (line(42:42) .ne. '/' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

c     	Chars 43,44: month.
          if (partline(substr,line,43,2) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if
          mm = atoi(substr)

c     	Char 45: '/' character.
          if (line(45:45) .ne. '/' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

c     	Chars 46,47: day.
          if (partline(substr,line,46,2) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_info = 20
              return
          end if
          dd = atoi(substr)

      else
          yyyy = ISF_NULL
          mm = ISF_NULL
          dd = ISF_NULL
      end if

c     Char 48: space.
      if (line(48:48) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 48: '//line
          read_phase_info = 20
          return
      end if

c     Chars  49-54: uncertainty in arrival time.
      if (partline(substr,line,49,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad time_unc: '//line
              read_phase_info = 20
              return
          end if
          time_unc = ator(substr)
      else
          time_unc = ISF_NULL
      end if

c     Char 55: space.
      if (line(55:55) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 55: '//line
          read_phase_info = 20
          return
      end if

c     Chars 56-60: time weight.
      if (partline(substr,line,56,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad time_weight: '//line
              read_phase_info = 20
              return
          end if
          time_weight = ator(substr)
      else
          time_weight = ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 61: '//line
          read_phase_info = 20
          return
      end if

c     Chars 62-66: azimuth uncertainty.
      if (partline(substr,line,61,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad azim_unc: '//line
              read_phase_info = 20
              return
          end if
          azim_unc = ator(substr)
      else
          azim_unc = ISF_NULL
      end if

c     Char 67: space.
      if (line(67:67) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 67: '//line
          read_phase_info = 20
          return
      end if

c     Chars 68-72: azimuth weight.
      if (partline(substr,line,68,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad azim_weight: '//line
              read_phase_info = 20
              return
          end if
          azim_weight = ator(substr)
      else
          azim_weight = ISF_NULL
      end if

c     Char 73: space.
      if (line(73:73) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 73: '//line
          read_phase_info = 20
          return
      end if

c     Chars 74-79: slowness uncertainty.
      if (partline(substr,line,73,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad slow_unc: '//line
              read_phase_info = 20
              return
          end if
          slow_unc = ator(substr)
      else
          slow_unc = ISF_NULL
      end if

c     Char 80: space.
      if (line(80:80) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 80: '//line
          read_phase_info = 20
          return
      end if

c     Chars 81-85: slowness weight.
      if (partline(substr,line,81,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad slow_weight: '//line
              read_phase_info = 20
              return
          end if
          slow_weight = ator(substr)
      else
          slow_weight = ISF_NULL
      end if

c     Char 86: space.
      if (line(86:86) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 86: '//line
          read_phase_info = 20
          return
      end if

c     Chars 87-95: amplitude unceratinty.
      if (partline(substr,line,87,9) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad amp_unc: '//line
              read_phase_info = 20
              return
          end if
          amp_unc = ator(substr)
      else
          amp_unc = ISF_NULL
      end if

c     Char 96: space.
      if (line(96:96) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 96: '//line
          read_phase_info = 20
          return
      end if

c     Chars 97-101: period uncertainty.
      if (partline(substr,line,97,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad per_unc: '//line
              read_phase_info = 20
              return
          end if
          per_unc = ator(substr)
      else
          per_unc = ISF_NULL
      end if

c     Char 102: space.
      if (line(102:102) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 102: '//line
          read_phase_info = 20
          return
      end if


c     Chars 103-105: uncertainty in station magnitude.
      if (partline(substr,line,103,3) .ne. 0) then
          if (check_real(substr) .eq. 1) then
              isf_bulletin_error = 'bad mag_unc: '//line
              read_phase_info = 20
              return
          end if
          mag_unc = ator(substr)
      else
          mag_unc = ISF_NULL
      end if

c     Char 106: space.
      if (line(106:106) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 106: '//line
          read_phase_info = 20
          return
      end if

c     Chars 107-115: author.
      if (partline(author,line,107,9) .eq. 1) then
          if ( check_whole(author) .eq. 1 ) then
              isf_bulletin_error = 'bad author: '//line
              read_phase_info = 20
              return
          end if
      else
          author = ' '
      end if

c     Char 116: space.
      if (line(116:116) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 116: '//line
          read_phase_info = 20
          return
      end if

c     Chars 117-124: arrival ID, any characters allowed but must be there.
      if (partline(arrid,line,117,9) .eq. 0) then
          isf_bulletin_error = 'missing arrid: '//line
          read_phase_info = 20
          return
      end if

      if ( check_whole(arrid) .eq. 1 ) then
          isf_bulletin_error = 'bad arrid: '//line
          read_phase_info = 20
          return
      end if

c     Check for extra characters.
      if (partline(substr,line,125,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_info = 20
          return
      end if

      read_phase_info = 0
      return
      end


c     Parses a line assuming it to be an additional phase measurement line.
c     Accepts any number of parameter=value pairs as long as the line is
c     short enough.

c     Returns 0 if the line is a properly formatted phase measurement line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error if not.

      integer function read_phase_measure(line,param,value,error,
     +                                                      numparam)

      character line*(*)
      character param(*)*(*),value(*)*(*),error(*)*(*)
      integer numparam
      include 'isf_bul.h'
      integer start,end,break,mid,i
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: should be the comment format string
      if (line(1:11) .ne. " (#MEASURE ") then
          isf_bulletin_error = 'not a phase measurement line: '//line
          read_phase_measure = 20
          return
      end if

      start=12
      do while (line(start:start) .eq. ' ')
          start=start+1
      end do

      end=len(line)
      do while (line(end:end) .eq. ' ' .or. line(end:end) .eq. ')')
          if (line(end:end) .eq. ')') then
              line(end:end) = ' '
          end if
          end=end-1
      end do

      if (end .gt. ISF_COMM_LEN+11) then
          isf_bulletin_error = 'line too long: '//line
          read_phase_measure = 20
          return
      end if

c     Go through the rest of the line one character at a time, separating c     words on ' ' to get param=value pairs and on '=' to get the
c     individual parameters and vales.
      numparam=0
      break = index(line(start:),' ')

      do while (break .ne. 0 .and. start .le. end)
          break = break + start
          mid = index(line(start:break),'=')

          if (mid .eq. 0) then
              isf_bulletin_error = 'param without value: '//line
              read_phase_measure = 20
              return
          end if

          mid = mid + start
          numparam = numparam+1
          param(numparam) = line(start:mid-2)
          value(numparam) = line(mid:break-2)
          start = break
          break = index(line(start:),' ')
      end do

c     For each resulting value check whether includes an error part.
      do i=1,numparam
          mid = index(value(i),'+')
          if (mid .ne. 0) then
              substr = value(i)
              value(i) = substr(1:mid-1)
              error(i) = substr(mid+1:)
          else
              error(i) = " "
          end if
      end do

      read_phase_measure = 0
      return
      end


c     Parses a line asuming it to be a minimum phase range line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly phase_min data line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_phase_min(line,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      character line*(*)
      real timeoffset,azoffset,slowoffset,ampoffset,peroffset,magoffset

      include 'isf_bul.h'
      integer partline,check_real
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-6: comment format string.
      if (line(1:6) .ne. " (#MIN") then
          isf_bulletin_error = 'not a phase_min line: '//line
          read_phase_min = 20
          return
      end if

c     Chars 7-47: spaces.
      if (partline(substr,line,7,41) .ne. 0) then
          isf_bulletin_error = 'not a phase_min line: '//line
          read_phase_min = 20
          return
      end if

c     Chars 48-54: time offset.
      if (partline(substr,line,48,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad timeoffset: '//line
              read_phase_min = 20
              return
          end if
          timeoffset = ator(substr)
      else
          timeoffset = ISF_NULL
      end if

c     Chars 55-60: spaces.
      if (partline(substr,line,55,5) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 55-60: '//line
          read_phase_min = 20
          return
      end if

c     Chars 61-66: azimuth offset.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad azoffset: '//line
              read_phase_min = 20
              return
          end if
          azoffset = ator(substr)
      else
          azoffset = ISF_NULL
      end if

c     Chars 67-72: spaces.
      if (partline(substr,line,67,6) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 67-72: '//line
          read_phase_min = 20
          return
      end if

c     Chars 73-79: slowness offset.
      if (partline(substr,line,73,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad slowoffset: '//line
              read_phase_min = 20
              return
          end if
          slowoffset = ator(substr)
      else
          slowoffset = ISF_NULL
      end if

c     Chars 80-85: spaces.
      if (partline(substr,line,80,6) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 80-85: '//line
          read_phase_min = 20
          return
      end if

c     Chars 86-95: amplitude offset.
      if (partline(substr,line,86,10) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad ampoffset: '//line
              read_phase_min = 20
              return
          end if
          ampoffset = ator(substr)
      else
          ampoffset = ISF_NULL
      end if

c     Chars 96-101: period offset.
      if (partline(substr,line,96,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad peroffset: '//line
              read_phase_min = 20
              return
          end if
          peroffset = ator(substr)
      else
          peroffset = ISF_NULL
      end if

c     Chars 102-105: magnitude offset.
      if (partline(substr,line,102,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad magoffset: '//line
              read_phase_min = 20
              return
          end if
          magoffset = ator(substr)
      else
          magoffset = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,106,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_min = 20
          return
      end if

      read_phase_min = 0
      return
      end

c     Parses a line asuming it to be a maximum phase range line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly phase_max data line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_phase_max(line,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      character line*(*)
      real timeoffset,azoffset,slowoffset,ampoffset,peroffset,magoffset

      include 'isf_bul.h'
      integer partline,check_real
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-6: comment format string.
      if (line(1:6) .ne. " (#MAX") then
          isf_bulletin_error = 'not a phase_max line: '//line
          read_phase_max = 20
          return
      end if

c     Chars 7-47: spaces.
      if (partline(substr,line,7,41) .ne. 0) then
          isf_bulletin_error = 'not a phase_max line: '//line
          read_phase_max = 20
          return
      end if

c     Chars 48-54: time offset.
      if (partline(substr,line,48,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad timeoffset: '//line
              read_phase_max = 20
              return
          end if
          timeoffset = ator(substr)
      else
          timeoffset = ISF_NULL
      end if

c     Chars 55-60: spaces.
      if (partline(substr,line,55,5) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 55-60: '//line
          read_phase_max = 20
          return
      end if

c     Chars 61-66: azimuth offset.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad azoffset: '//line
              read_phase_max = 20
              return
          end if
          azoffset = ator(substr)
      else
          azoffset = ISF_NULL
      end if

c     Chars 67-72: spaces.
      if (partline(substr,line,67,6) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 67-72: '//line
          read_phase_max = 20
          return
      end if

c     Chars 73-79: slowness offset.
      if (partline(substr,line,73,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad slowoffset: '//line
              read_phase_max = 20
              return
          end if
          slowoffset = ator(substr)
      else
          slowoffset = ISF_NULL
      end if

c     Chars 80-85: spaces.
      if (partline(substr,line,80,6) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 80-85: '//line
          read_phase_max = 20
          return
      end if

c     Chars 86-95: amplitude offset.
      if (partline(substr,line,86,10) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad ampoffset: '//line
              read_phase_max = 20
              return
          end if
          ampoffset = ator(substr)
      else
          ampoffset = ISF_NULL
      end if

c     Chars 96-101: period offset.
      if (partline(substr,line,96,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad peroffset: '//line
              read_phase_max = 20
              return
          end if
          peroffset = ator(substr)
      else
          peroffset = ISF_NULL
      end if

c     Chars 102-105: magnitude offset.
      if (partline(substr,line,102,4) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad magoffset: '//line
              read_phase_max = 20
              return
          end if
          magoffset = ator(substr)
      else
          magoffset = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,106,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_max = 20
          return
      end if

      read_phase_max = 0
      return
      end

c     Parses a line asuming it to be a phase correction line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly phase correction line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_phase_correc(line,timecorr,azcorr,
     +                        slowcorr,ampcorr,percorr,magcorr)

      character line*(*)
      real timecorr,azcorr,slowcorr,ampcorr,percorr,magcorr

      include 'isf_bul.h'
      integer partline,check_real
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-8: comment format string.
      if (line(1:8) .ne. " (#COREC") then
          isf_bulletin_error = 'not a phase correction line: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 9-47: spaces.
      if (partline(substr,line,9,39) .ne. 0) then
          isf_bulletin_error = 'not a phase correction line: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 48-54: time correction.
      if (partline(substr,line,48,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad timecorr: '//line
              read_phase_correc = 20
              return
          end if
          timecorr = ator(substr)
      else
          timecorr = ISF_NULL
      end if

c     Chars 55-60: spaces.
      if (partline(substr,line,55,5) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 55-60: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 61-66: azimuth correction.
      if (partline(substr,line,61,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad azcorr: '//line
              read_phase_correc = 20
              return
          end if
          azcorr = ator(substr)
      else
          azcorr = ISF_NULL
      end if

c     Chars 67-72: spaces.
      if (partline(substr,line,67,6) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 67-72: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 73-79: slowness correction.
      if (partline(substr,line,73,7) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad slowcorr: '//line
              read_phase_correc = 20
              return
          end if
          slowcorr = ator(substr)
      else
          slowcorr = ISF_NULL
      end if

c     Chars 80-85: spaces.
      if (partline(substr,line,80,6) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 80-85: '//line
          read_phase_correc = 20
          return
      end if

c     Chars 86-95: amplitude correction.
      if (partline(substr,line,86,10) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad ampcorr: '//line
              read_phase_correc = 20
              return
          end if
          ampcorr = ator(substr)
      else
          ampcorr = ISF_NULL
      end if

c     Chars 96-101: period correction.
      if (partline(substr,line,96,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad percorr: '//line
              read_phase_correc = 20
              return
          end if
          percorr = ator(substr)
      else
          percorr = ISF_NULL
      end if

c     Chars 102-106: magnitude correction.
      if (partline(substr,line,102,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad magcorr: '//line
              read_phase_correc = 20
              return
          end if
          magcorr = ator(substr)
      else
          magcorr = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,107,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_correc = 20
          return
      end if

      read_phase_correc = 0
      return
      end


c     Parses a line asuming it to be an original phase data line.
c     Values are asigned to variables sent as arguments.
c     If an optional parameter is not given then the corresponding variable
c     will have ISF_NULL assigned to it.

c     Returns 0 if the line is a properly formatted original phase data line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_phase_original(line,chan,sta,yyyy,mm,
     + dd,hh,mi,ss,msec,azim,slow,amp,per,mag)

      character line*(*),sta*(*),chan*(*)
      real azim,slow,amp,per,mag
      integer yyyy,mm,dd,hh,mi,ss,msec

      include 'isf_bul.h'
      integer partline,check_int,atoi,check_real,check_whole,isdigit
      real ator
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: comment format string.
      if (line(1:10) .ne. " (#ORIG   ") then
          isf_bulletin_error = 'not an original phase line: '//line
          read_phase_original = 20
          return
      end if

c     Chars 11-13: original channel.
      if (partline(chan,line,11,3) .ne. 0) then
          if ( check_whole(chan) .eq. 1 ) then
              isf_bulletin_error = 'bad chan: '//line
              read_phase_original = 20
              return
          end if
      else
          chan=' '
      end if

c     Char 14: space.
      if (line(14:14) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 14: '//line
          read_phase_original = 20
          return
      end if

c     Chars 15-22: original station code.
      if (partline(sta,line,15,8) .ne. 0) then
          if ( check_whole(sta) .eq. 1 ) then
              isf_bulletin_error = 'bad sta: '//line
              read_phase_original = 20
              return
          end if
      else
          sta=' '
      end if

c     Chars 23-37: spaces.
      if (partline(substr,line,23,15) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 23-37: '//line
          read_phase_original = 20
          return
      end if

c     Chars 38-60: arrival date and time.
      if (partline(substr,line,38,10) .ne. 0) then

c     	38-41: year.
          if (partline(substr,line,38,4) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          yyyy = atoi(substr)

c     	Char 42: '/' character.
          if (line(42:42) .ne. '/' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 43,44: month.
          if (partline(substr,line,43,2) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          mm = atoi(substr)

c     	Char 45: '/' character.
          if (line(45:45) .ne. '/' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 46,47: day.
          if (partline(substr,line,46,2) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          dd = atoi(substr)

c     	Char 48: space.
          if (line(48:48) .ne. ' ' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 49,50: hour.
          if (partline(substr,line,49,2) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          hh = atoi(substr)

c     	Char 51: ':' character.
          if (line(51:51) .ne. ':' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 52,53: minute.
          if (partline(substr,line,52,2) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          mi = atoi(substr)

c     	Char 54: ':' character.
          if (line(54:54) .ne. ':' ) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

c     	Chars 55,56: integral second.
          if (partline(substr,line,55,2) .eq. 0) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if

          if (check_int(substr) .eq. 1) then
              isf_bulletin_error = 'bad date: '//line
              read_phase_original = 20
              return
          end if
          ss = atoi(substr)

c     	Char 57-60: msec or spaces.
c     	Allow decimal place without any numbers after it.
          if (partline(substr,line,58,3) .ne. 0) then

c     		Char 57: '.' character.
              if (line(57:57) .ne. '.' ) then
                  isf_bulletin_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if

c     		Chars 58-60: msec.
              if (isdigit(line(58:58)) .eq. 0) then
                  isf_bulletin_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if
              msec = (ichar(line(58:58)) - ichar('0'))*100

              if (isdigit(line(59:59)) .ne. 0) then
                  msec = msec + (ichar(line(59:59)) - ichar('0'))*10
              else if (line(59:59) .ne. ' ' .or. line(60:60) .ne. ' ') 
     +        then
                  isf_bulletin_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if

              if (isdigit(line(60:60)) .ne. 0) then
                  msec = msec + (ichar(line(60:60)) - ichar('0'))
              else if (line(60:60) .ne. ' ') then
                  isf_bulletin_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if
          else
c     		Char 57: '.' character or space.
              if (line(57:57) .ne. ' ' .and. line(57:57) .ne. '.' ) 
     +        then
                  isf_bulletin_error = 'bad date: '//line
                  read_phase_original = 20
                  return
              end if

              msec = ISF_NULL
          end if
      else
          yyyy = ISF_NULL
          mm   = ISF_NULL
          dd   = ISF_NULL
          hh   = ISF_NULL
          mi   = ISF_NULL
          ss   = ISF_NULL
          msec = ISF_NULL
      end if

c     Char 61: space.
      if (line(61:61) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 61: '//line
          read_phase_original = 20
          return
      end if

c     Chars 62-66: original azimuth.
      if (partline(substr,line,62,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad azim: '//line
              read_phase_original = 20
              return
          end if
          azim = ator(substr)
      else
          azim = ISF_NULL
      end if

c     Chars 67-73: spaces.
      if (partline(substr,line,67,7) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 67-73: '//line
          read_phase_original = 20
          return
      end if

c     Chars 74-79: original slowness.
      if (partline(substr,line,74,6) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad slow: '//line
              read_phase_original = 20
              return
          end if
          slow = ator(substr)
      else
          slow = ISF_NULL
      end if

c     Chars 80-86: spaces.
      if (partline(substr,line,80,7) .ne. 0) then
          isf_bulletin_error = 'bad format, chars 80-86: '//line
          read_phase_original = 20
          return
      end if

c     Chars 87-95:  original amplitude.
      if (partline(substr,line,87,9) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad amp: '//line
              read_phase_original = 20
              return
          end if
          amp = ator(substr)
      else
          amp = ISF_NULL
      end if

c     Char 96: space.
      if (line(96:96) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 96: '//line
          read_phase_original = 20
          return
      end if

c     Chars 97-101: original period.
      if (partline(substr,line,97,5) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad per: '//line
              read_phase_original = 20
              return
          end if
          per = ator(substr)
      else
          per = ISF_NULL
      end if

c     Char 102: space.
      if (line(102:102) .ne. ' ' ) then
          isf_bulletin_error = 'bad format, char 102: '//line
          read_phase_original = 20
          return
      end if

c     Chars  103-105: original station magnitude.
      if (partline(substr,line,103,3) .ne. 0) then
          if (check_real(substr) .eq. 1) then
               isf_bulletin_error = 'bad mag: '//line
              read_phase_original = 20
              return
          end if
          mag = ator(substr)
      else
          mag = ISF_NULL
      end if

c     Check for extra characters - could be close bracket somewhere.  */
      if (partline(substr,line,106,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_phase_original = 20
          return
      end if

      read_phase_original = 0
      return
      end


c     Parses a line asuming it to be a comment line.

c     Returns 0 if the line is a properly formatted comment line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error on error.

      integer function read_comment(line,comment)

      character line*(*),comment*(*)

      include 'isf_bul.h'
      integer partline

c     Chars 1-2: comment format string.
      if (line(1:2) .ne. " (") then
          isf_bulletin_error = 'not a comment line: '//line
          read_comment = 20
          return
      end if

c     partline will clean off final bracket.
      if (partline(comment,line,3,0) .gt. ISF_LINE_LEN) then
          isf_bulletin_error = 'comment too long: '//line
          read_comment = 20
          return
      end if

      read_comment = 0
      return
      end


c     Tests a line to discover if it is a stop line.

c     Returns 0 if the line is a stop line.
c     Returns 20 and writes a diagnostic to isf_bulletin_error otherwise.

      integer function read_stop(line)

      character line*(*)

      include 'isf_bul.h'
      integer partline
      character substr*(ISF_LINE_LEN)

c     Chars 1-2: comment format string.
      if (line(1:4) .ne. "STOP") then
          isf_bulletin_error = 'not a stop line: '//line
          read_stop = 20
          return
      end if

      if (partline(substr,line,5,0) .ne. 0) then
          isf_bulletin_error = 'extra characters at end: '//line
          read_stop = 20
          return
      end if

      read_stop = 0
      return
      end

c     Writes the data type line at the top of a GSE report.
c     Format is:  DATA_TYPE data_type:subtype data_format:subformat
c     Only data_type is required.  Only other limitation is that a
c     subformat is not allowed without a data_format.

c     This is the only write routine that does not check the value of
c     'isf_prev_line_type' -  this is the first line of a new report.

c     Returns 0 on a successful write.  
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_data_type(file,data_type,subtype,
     +                                    data_format,subformat)

      integer file
      character data_type*(*),subtype*(*),data_format*(*),subformat*(*)

      include 'isf_bul.h'
      integer partline, check_prev_line_type, check_whole
      integer numchar
c     line length so far ('DATA DATA_TYPE ')
      integer length /10/                
      character substr*(ISF_LINE_LEN)

c     Check and write data_type.
      numchar = partline(substr,data_type,1,0)
      if (numchar .eq. 0) then
          isf_bulletin_error = 'null data_type given'
          write_data_type = 20
          return
      end if

      length = length + numchar
      if (length .gt. ISF_LINE_LEN ) then
          isf_bulletin_error = 'data_type too long '//data_type
          write_data_type = 20
          return
      end if
      if ( check_whole(data_type) .eq. 1 ) then
          isf_bulletin_error = 'bad data_type: '//data_type
          write_data_type = 20
          return
      end if
      write (file,"('DATA_TYPE ',a,$)") data_type(1:numchar)

c     Check and write subtype - if there is one.
      numchar = partline(substr,subtype,0,0)
      if (numchar .ne. 0) then
          length = length + numchar
          if (length .gt. ISF_LINE_LEN ) then
              isf_bulletin_error = 'data subtype too long 
     +'//subtype(1:numchar)
              write_data_type = 20
              return
          end if
          if ( check_whole(subtype) .eq. 1 ) then
              isf_bulletin_error = 'bad subtype: '//subtype(1:numchar)
              write_data_type = 20
              return
          end if
          write (file,"(':',A ,$)") subtype(1:numchar)
      end if

c     Check and write format - if there is one.
      numchar = partline(substr,data_format,0,0)
      if (numchar .ne. 0) then
          length = length + numchar
          if (length .gt. ISF_LINE_LEN ) then
              isf_bulletin_error = 'line too long 
     +'//data_format(1:numchar)
              write_data_type = 20
              return
          end if
          if ( check_whole(data_format) .eq. 1 ) then
              isf_bulletin_error = 'bad data_format: 
     +'//data_format(1:numchar)
              write_data_type = 20
              return
          end if
          write (file,"(' ',A ,$)") data_format(1:numchar)

c     	Check and write subformat - if there is one.
          numchar = partline(substr,subformat,0,0)
          if (numchar .ne. 0) then
              length = length + numchar
              if (length .gt. ISF_LINE_LEN ) then
                  isf_bulletin_error = 'line too long 
     +'//subformat(1:numchar)
                  write_data_type = 20
                  return
              end if
              if ( check_whole(subformat) .eq. 1 ) then
                  isf_bulletin_error = 'bad subformat: 
     +'//subformat(1:numchar)
                  write_data_type = 20
                  return
              end if
              write (file,"(':',A ,$)") subformat(1:numchar)
          end if
      else if (partline(substr,subformat,0,0) .ne. 0) then
          isf_bulletin_error = 'subformat given without format'
          write_data_type = 20
          return
      end if
      write (file,"()")

c     Set 'isf_prev_line_type' for future calls to check_prev_line_type.
c     Do no actual checking for this line type.
      if (check_prev_line_type('data_type') .ne. 0) then
          write_data_type = 20
          return
      end if

      write_data_type = 0
      return
      end

c     Writes an event title line with a preceding blank line.
c     Requires event ID but will write a line without a region if required.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_event_id(file,evid,region)

      integer file
      character evid*(*),region*(*)

      include 'isf_bul.h'
      integer partline, check_prev_line_type, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Chars 1-5: the word 'Event'. Chars 7-14: event ID.
      numchar = partline(substr,evid,0,0)
      if (numchar .eq. 0) then
          isf_bulletin_error = 'missing evid'
          write_event_id = 20
          return
      end if
      if (numchar .gt. ISF_EVID_LEN) then
          isf_bulletin_error = 'evid too long: '//evid
          write_event_id = 20
          return
      end if
      if ( check_whole(evid) .eq. 1 ) then
          isf_bulletin_error = 'bad evid: '//evid
          write_event_id = 20
          return
      end if
      write (file,"()")
      write (file,"('Event ',a8,$)") evid

c     Chars 16-80: geographic region if given.
      numchar = partline(substr,region,0,0)
      if (numchar .ne. 0) then
          if (numchar .gt. ISF_REGION_LEN) then
              isf_bulletin_error = 'region too long: '//region
              write_event_id = 20
              return
          end if
          write (file,"(' ',a,$)") region(1:numchar)
      end if
      write (file,"()")

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('event_id') .ne. 0) then
          write_event_id = 10
          return
      end if

      write_event_id = 0
      return
      end


c     Writes an origin header line.
c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_origin_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(136)

      head = '   Date       Time        Err   RMS Latitude Longitude  Sm
     +aj  Smin  Az Depth   Err Ndef Nsta Gap  mdist  Mdist Qual   Author
     +      OrigID'

      write (file,"(A)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin_head') .ne. 0) then
          write_origin_head = 10
          return
      end if

      write_origin_head = 0
      return
      end

c     Writes an origin line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_origin(file,yyyy,mm,dd,hh,mi,ss,msec,
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
      if (is_null(stime) .eq. 1) then
          write(file,"('      ',$)")
      else
          if (stime .lt. 0 .or. stime .gt. 99999) then
              write (isf_bulletin_error,"('bad stime: ',f8.2)") stime
              write_origin = 20
              return
          end if
          call write_real(file,stime,5,2)
          write (file,"(' ',$)")
      end if

c     31-35: optional rms (sdobs). Char 36: space.
c     Give 2 decimal places but less if number > 99.
      if (is_null(sdobs) .eq. 1) then
          write(file,"('      ',$)")
      else
          if (sdobs .lt. 0 .or. sdobs .gt. 99999) then
              write (isf_bulletin_error,"('bad sdobs: ',f8.2)") sdobs
              write_origin = 20
              return
          end if
          call write_real(file,sdobs,5,2)
          write (file,"(' ',$)")
      end if

c     37-44: lattitude. Char 45: space.
      if (is_null(lat) .eq. 1) then
          isf_bulletin_error = 'missing latitude'
          write_origin = 20
          return
      end if

      if (lat .lt. -90 .or. lat .gt. 90) then
          write (isf_bulletin_error,"('bad latitude: ',f8.2)") lat
          write_origin = 20
          return
      end if
      write (file,"(f8.4,' ',$)") lat

c     Chars 46-54: longitude.
      if (is_null(lon) .eq. 1) then
          isf_bulletin_error = 'missing longitude'
          write_origin = 20
          return
      end if

      if (lon .lt. -180 .or. lon .gt. 180) then
          write (isf_bulletin_error,"('bad longitude: ',f8.2)") lon
          write_origin = 20
          return
      end if
      write (file,"(f9.4,$)") lon

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
      if (is_null(smaj) .eq. 1) then
          write (file,"('      ',$)")
      else
          if (smaj .lt. 0 .or. smaj .gt. 99999) then
              write (isf_bulletin_error,"('bad smaj: ',f8.2)") smaj
              write_origin = 20
              return
          end if
          call write_real(file,smaj,5,1)
          write (file,"(' ',$)")
      end if

c     Chars 62-66: optional semi-minor axis. Char 67: space.
c     Give 1 decimal place but less if number > 999.
      if (is_null(smin) .eq. 1) then
          write (file,"('      ',$)")
      else
          if (smin .lt. 0 .or. smin .gt. 99999) then
              write (isf_bulletin_error,"('bad smin: ',f8.2)") smin
              write_origin = 20
              return
          end if
          call write_real(file,smin,5,1)
          write (file,"(' ',$)")
      end if

c     Chars 68-70: optional strike. Char 71: space.
c     Strike can be -1, when it's a flag to signify that smaj,smin
c     are really slat,slon.
      if (is_null(real(strike)) .eq. 1) then
          write (file,"('    ',$)")
      else
          if (strike .lt. -1 .or. strike .gt. 360) then
              write (isf_bulletin_error,"('bad strike: ',f8.2)") strike
              write_origin = 20
              return
          end if
          write (file,"(i3,' ',$)") strike
      end if

c     Chars 72-76: optional depth.
      if (is_null(depth) .eq. 1) then
          write (file,"('     ',$)")
      else
          if (depth .lt. 0 .or. depth .gt. 999) then
              write (isf_bulletin_error,"('bad depth: ',f8.2)") depth
              write_origin = 20
              return
          end if
          write (file,"(f5.1,$)") depth
      end if

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
      if (is_null(sdepth) .eq. 1) then
          write (file,"('     ',$)")
      else
          if (sdepth .lt. 0 .or. sdepth .gt. 9999) then
              write (isf_bulletin_error,"('bad sdepth: ',f8.2)") sdepth
              write_origin = 20
              return
          end if
          call write_real(file,sdepth,4,1)
          write (file,"(' ',$)")
      end if

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
      if (is_null(real(gap)) .eq. 1) then
          write (file,"('    ',$)")
      else
          if (gap .lt. 0 .or. gap .gt. 360) then
              write (isf_bulletin_error,"('bad gap: ',f8.2)") gap
              write_origin = 20
              return
          end if
          write (file,"(i3,' ',$)") gap
      end if

c     Chars 98-103: optional minimum distance. Char 104: space.
c     Gives 2 decimal places or less if number > 999.
      if (is_null(mindist) .eq. 1) then
          write (file,"('       ',$)")
      else
          if (mindist .lt. 0 .or. mindist .gt. 999999) then
              write (isf_bulletin_error,"('bad mindist: ',f8.2)") mindist
              write_origin = 20
              return
          end if
          call write_real(file,mindist,6,2)
          write (file,"(' ',$)")
      end if

c     Chars 105-110: optional maximum distance. Char 111: space.
c     Gives 2 decimal places or less if number > 999.
      if (is_null(maxdist) .eq. 1) then
          write (file,"('       ',$)")
      else
          if (maxdist .lt. 0 .or. maxdist .gt. 999999) then
              write (isf_bulletin_error,"('bad maxdist: ',f8.2)") maxdist
              write_origin = 20
              return
          end if
          call write_real(file,maxdist,6,2)
          write (file,"(' ',$)")
      end if

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

      write_origin = 0
      return
      end

c     Writes the comment that can follow an origin line to mark it is as prime.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_origin_prime(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      write (file,"(a)") ' (#PRIME)'

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin_com') .ne. 0) then
          write_origin_prime = 10
          return
      end if

      write_origin_prime = 0
      return
      end


c     Writes the comment that can follow an origin line to mark it is a centroid.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_origin_centroid(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      write (file,"(a)") ' (#CENTROID)'

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('origin_com') .ne. 0) then
          write_origin_centroid = 10
          return
      end if

      write_origin_centroid = 0
      return
      end


c     Writes an origin parameter formatted comment.
c     Writes any number of parameter=value pairs, starting new line if necessary.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_origin_param(file,param,value,
     +                                            error,numparam)

      integer file,numparam
      character param(*)*(*),value(*)*(*),error(*)*(*)
      include 'isf_bul.h'
      integer partline,check_prev_line_type
      integer i,len,space_left
      integer numchar_param,numchar_value,numchar_error
      character substr*(ISF_LINE_LEN)

      write (file,"(a,$)") ' (#PARAM'
      space_left = ISF_COMM_LEN

      do i=1,numparam
          numchar_param = partline(substr,param(i),1,0)
          numchar_value = partline(substr,value(i),1,0)
          numchar_error = partline(substr,error(i),1,0)
          len = numchar_param + numchar_value + 1
          if (numchar_error .ne. 0) then
              len = len + numchar_error + 1
          end if
          if ( len .gt. ISF_COMM_LEN ) then
              write (isf_bulletin_error,"('param=value too long')")
              write_origin_param = 20
              return
          end if

          if ( space_left .lt. len ) then
              write (file,"(')')")
              write (file,"(a,$)") ' (#PARAM'
            space_left = ISF_COMM_LEN
          end if

          write (file,"(' ',a,$)") param(i)(1:numchar_param)
          write (file,"('=',a,$)") value(i)(1:numchar_value)
          if (numchar_error .ne. 0) then
              write (file,"('+',a,$)") error(i)(1:numchar_error)
          end if
          space_left = space_left - len
      end do
      write (file,"(')')")

      if (check_prev_line_type('origin_com') .ne. 0) then
          write_origin_param = 10
          return
      end if

      write_origin_param = 0
      return
      end

c     Writes both moment tensor header lines.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_momten_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head1*(88)
      character head2*(88)

      head1= ' (#MOMTENS sc    M0 fCLVD    MRR    MTT    MPP    MRT    M
     +TP    MPR NST1 NST2 Author   )'
      head2= ' (#             eM0 eCLVD    eRR    eTT    ePP    eRT    e
     +TP    ePR NCO1 NCO2 Duration )'

      write (file,"(a)") head1
      write (file,"(a)") head2

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('momten_head') .ne. 0) then
          write_momten_head = 10
          return
      end if

      write_momten_head = 0
      return
      end


c     Writes both moment tensor data lines.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_momten(file,scale_factor,scalar_moment,
     + fclvd,mrr,mtt,mpp,mrt,mtp,mpr,nsta1,nsta2,author,
     + scalar_moment_unc,fclvd_unc,mrr_unc,mtt_unc,
     + mpp_unc,mrt_unc,mtp_unc,mpr_unc,ncomp1,ncomp2,duration)

      integer file
      character author*(*)
      integer scale_factor,nsta1,nsta2, ncomp1,ncomp2
      real scalar_moment,fclvd,mrr,mtt,mpp,mrt,mtp,mpr
      real scalar_moment_unc,fclvd_unc,mrr_unc,mtt_unc,mpp_unc
      real mrt_unc,mtp_unc,mpr_unc,duration

      include 'isf_bul.h'
      integer partline, check_prev_line_type, is_null, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Line 1

c     Chars 1-11: comment start string#
      write (file,"(a,$)") ' (#        '

c     Chars 12,13: scale factor. Char 14: space.
      if (is_null(real(scale_factor)) .eq. 1) then
          isf_bulletin_error = 'missing scale_factor'
          write_momten = 20
          return
      end if

      if ( scale_factor .lt. 0 .or. scale_factor .gt. 99 ) then
          write (isf_bulletin_error,"('bad scale_factor: ',f8.2)") 
     + scale_factor
          write_momten = 20
          return
      end if
      write (file,"(i2,' ',$)") scale_factor

c     Chars 15-19: scalar seismic moment. Char 20: space.
      if (is_null(scalar_moment) .eq. 1) then
          isf_bulletin_error = 'missing scalar_moment'
          write_momten = 20
          return
      end if

      if ( scalar_moment .lt. 0 .or. scalar_moment .gt. 9.999 ) then
          write (isf_bulletin_error,"('bad scalar_moment: ',f8.2)") 
     + scalar_moment
          write_momten = 20
          return
      end if
      write (file,"(f5.3,' ',$)") scalar_moment

c     Chars 21-25: fCLVD. Char 26: space.
      if (is_null(fclvd) .eq. 1) then
          write (file,"('      ',$)")
      else
          if ( fclvd .lt. 0 .or. fclvd .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad fclvd: ',f8.2)") fclvd
              write_momten = 20
              return
          end if
          write (file,"(f5.3,' ',$)") fclvd
      end if

c     Chars 27-32: radial-radial element. Char 33: space.
      if (is_null(mrr) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mrr .lt. -9.999 .or. mrr .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mrr: ',f8.2)") mrr
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mrr
      end if

c     Chars 34-39: theta-theta element. Char 40: space.
      if (is_null(mtt) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mtt .lt. -9.999 .or. mtt .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mtt: ',f8.2)") mtt
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mtt
      end if

c     Chars 41-46: phi-phi element. Char 47: space.
      if (is_null(mpp) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mpp .lt. -9.999 .or. mpp .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mpp: ',f8.2)") mpp
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mpp
      end if

c     Chars 48-53: radial-theta element. Char 54: space.
      if (is_null(mrt) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mrt .lt. -9.999 .or. mrt .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mrt: ',f8.2)") mrt
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mrt
      end if

c     Chars 55-60: theta-phi element. Char 61: space.
      if (is_null(mtp) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mtp .lt. -9.999 .or. mtp .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mtp: ',f8.2)") mtp
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mtp
      end if

c     Chars 62-67: phi-radial element. Char 68: space.
      if (is_null(mpr) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mpr .lt. -9.999 .or. mpr .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mpr: ',f8.2)") mpr
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mpr
      end if

c     Chars 69-72: nsta1. Char 73: space.
      if (is_null(real(nsta1)) .eq. 1) then
          write (file,"('     ',$)")
      else
          if ( nsta1 .lt. 0 .or. nsta1 .gt. 999 ) then
              write (isf_bulletin_error,"('bad nsta1: ',i5)") nsta1
              write_momten = 20
              return
          end if
          write (file,"(i4,' ',$)") nsta1
      end if

c     Chars 74-77: nsta2. Char 78: space.
      if (is_null(real(nsta2)) .eq. 1) then
          write (file,"('     ',$)")
      else
          if ( nsta2 .lt. 0 .or. nsta2 .gt. 999 ) then
              write (isf_bulletin_error,"('bad nsta2: ',i5)") nsta2
              write_momten = 20
              return
          end if
          write (file,"(i4,' ',$)") nsta2
      end if

c     Chars 79-87 author. Char 87 ')'.
      numchar = partline(substr,author,0,0)
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_bulletin_error,"('author too long: ',a)") author
          write_momten = 20
          return
      end if
      if (numchar .eq. 0) then
          write (isf_bulletin_error,"('missing author')")
          write_momten = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//author
          write_momten = 20
          return
      end if
      write(file,"(a9,')')") author

c     Line 2.

c     Chars 1-14: comment start string
      write (file,"(a,$)") ' (#           '

c     Chars 15-19: uncertainty in scalar seismic moment. Char 20: space.
      if (is_null(scalar_moment_unc) .eq. 1) then
          write (file,"('      ',$)")
      else
          if ( scalar_moment_unc .lt. 0 .or. scalar_moment_unc .gt. 
     + 9.999 ) then
              write (isf_bulletin_error,"('bad moment unc ',f8.2)") 
     + scalar_moment_unc
              write_momten = 20
              return
          end if
          write (file,"(f5.3,' ',$)") scalar_moment_unc
      end if

c     Chars 21-25: uncertainty in fCLVD. Char 26: space.
      if (is_null(fclvd_unc) .eq. 1) then
          write (file,"('      ',$)")
      else
          if ( fclvd_unc .lt. 0 .or. fclvd_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad fclvd_unc: ',f8.2)") 
     + fclvd_unc
              write_momten = 20
              return
          end if
          write (file,"(f5.3,' ',$)") fclvd_unc
      end if

c     Chars 27-32: uncertainty in radial-radial element. Char 33: space.
      if (is_null(mrr_unc) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mrr_unc .lt. 0 .or. mrr_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mrr_unc: ',f8.2)") mrr_unc
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mrr_unc
      end if

c     Chars 34-39: uncertainty in theta-theta element. Char 40: space.
      if (is_null(mtt_unc) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mtt_unc .lt. 0 .or. mtt_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mtt_unc: ',f8.2)") mtt_unc
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mtt_unc
      end if

c     Chars 41-46: uncertainty in phi-phi element. Char 47: space.
      if (is_null(mpp_unc) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mpp_unc .lt. 0 .or. mpp_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mpp_unc: ',f8.2)") mpp_unc
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mpp_unc
      end if

c     Chars 48-53: uncertainty in radial-theta element. Char 54: space.
      if (is_null(mrt_unc) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mrt_unc .lt. 0 .or. mrt_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mrt_unc: ',f8.2)") mrt_unc
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mrt_unc
      end if

c     Chars 55-60: uncertainty in theta-phi element. Char 61: space.
      if (is_null(mtp_unc) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mtp_unc .lt. 0 .or. mtp_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mtp_unc: ',f8.2)") mtp_unc
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mtp_unc
      end if

c     Chars 62-67: uncertainty in phi-radial element. Char 68: space.
      if (is_null(mpr_unc) .eq. 1) then
          write (file,"('       ',$)")
      else
          if ( mpr_unc .lt. 0 .or. mpr_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad mpr_unc: ',f8.2)") mpr_unc
              write_momten = 20
              return
          end if
          write (file,"(f6.3,' ',$)") mpr_unc
      end if

c     Chars 69-72: ncomp1. Char 73: space.
      if (is_null(real(ncomp1)) .eq. 1) then
          write (file,"('     ',$)")
      else
          if ( ncomp1 .lt. 0 .or. ncomp1 .gt. 999 ) then
              write (isf_bulletin_error,"('bad ncomp1: ',i5)") ncomp1
              write_momten = 20
              return
          end if
          write (file,"(i4,' ',$)") ncomp1
      end if

c     Chars 74-77: ncomp2. Char 78: space.
      if (is_null(real(ncomp2)) .eq. 1) then
          write (file,"('     ',$)")
      else
          if ( ncomp2 .lt. 0 .or. ncomp2 .gt. 999 ) then
              write (isf_bulletin_error,"('bad ncomp2: ',i5)") ncomp2
              write_momten = 20
              return
          end if
          write (file,"(i4,' ',$)") ncomp2
      end if

c     Chars 79-86: duration. Char 77: space. Char 88 ')'.
      if (is_null(duration) .eq. 1) then
          write (file,"('         )')")
      else
          if ( duration .lt. 0 .or. duration .gt. 99999 ) then
              write (isf_bulletin_error,"('bad duration: ',f8.2)") 
     + duration
              write_momten = 20
              return
          end if
          write (file,"(f8.2,' )')") duration
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('momten') .ne. 0) then
          write_momten = 10
          return
      end if

      write_momten = 0
      return
      end

c     Writes a fault plane header line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_fault_plane_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(64)

      head = ' (#FAULT_PLANE Typ Strike   Dip    Rake  NP  NS Plane Auth
     +or   )'

      write (file,"(a)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('fault_plane_head') .ne. 0) then
          write_fault_plane_head = 10
          return
      end if

      write_fault_plane_head = 0
      return
      end

c     Writes a fault plane data line.
c     Either first or second plane - only the comment marker at the start changes.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_fault_plane(file,f_type,strike,dip,
     +                                    rake,np,ns,f_plane,author)

      integer file
      character f_plane*(*), f_type*(*), author*(*)
      integer np,ns
      real strike,dip,rake

      integer line_num

      include 'isf_bul.h'
      integer partline, check_prev_line_type, is_null,check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)


c     Check if this is the second fault plane to be writen.
c     Chars 1-15 are the respective comment start strings.
      numchar = partline(substr,isf_prev_line_type,1,0)
      if (numchar .eq. 11 .and. substr(1:11) .eq. 'fault_plane') then
          line_num = 2
          write (file,"(a,$)") ' (+            '
      else
          line_num = 1
          write (file,"(a,$)") ' (#            '
      end if

c     Chars 16-18: Fault plane type. Char 19: space.
      numchar = partline(substr,f_type,0,0)
      if (numchar .ne. 0) then
          if (numchar .gt. ISF_F_TYPE_LEN) then
              write (isf_bulletin_error,"('f_type too long: ',a)") 
     + f_type
              write_fault_plane = 20
              return
          end if
          if ( check_whole(f_type) .eq. 1 ) then
              isf_bulletin_error = 'bad f_type: '//f_type
              write_fault_plane = 20
              return
          end if
          write(file,"(a3,' ',$)") f_type
      else
          write(file,"('    ',$)")
      end if

c     Chars 20-25: strike. Char 26 space.
      if (is_null(strike) .eq. 1) then
          isf_bulletin_error = 'missing strike'
          write_fault_plane = 20
          return
      end if

      if ( strike .lt. 0 .or. strike .gt. 360 ) then
          write (isf_bulletin_error,"('bad strike: ',f8.2)") strike
          write_fault_plane = 20
          return
      end if
      write (file,"(f6.2,' ',$)") strike

c     Chars 27-31: dip. Char 32 space.
      if (is_null(dip) .eq. 1) then
          isf_bulletin_error = 'missing dip'
          write_fault_plane = 20
          return
      end if

      if ( dip .lt. 0 .or. dip .gt. 90 ) then
          write (isf_bulletin_error,"('bad dip: ',f8.2)") dip
          write_fault_plane = 20
          return
      end if
      write (file,"(f5.2,' ',$)") dip

c     Chars 33-39: optional rake. Char 40 space.
      if (is_null(rake) .eq. 1) then
          write (file,"('        ',$)")
      else
          if ( rake .lt. -180 .or. rake .gt. 180 ) then
              write (isf_bulletin_error,"('bad rake: ',f8.2)") rake
              write_fault_plane = 20
              return
          end if
          write (file,"(f7.2,' ',$)") rake
      end if

c     Chars 41-43: optional np. Char 44 space.
      if (is_null(real(np)) .eq. 1) then
          write (file,"('    ',$)")
      else
          if ( np .lt. 0 .or. np .gt. 999 ) then
              write (isf_bulletin_error,"('bad np: ',d8.2)") np
              write_fault_plane = 20
              return
          end if
          write (file,"(i3,' ',$)") np
      end if

c     Chars 45-47: optional ns. Char 48 space.
      if (is_null(real(ns)) .eq. 1) then
          write (file,"('    ',$)")
      else
          if ( ns .lt. 0 .or. ns .gt. 999 ) then
              write (isf_bulletin_error,"('bad ns: ',d8.2)") ns
              write_fault_plane = 20
              return
          end if
          write (file,"(i3,' ',$)") ns
      end if

c     Chars 49-53: Plane identification. Char 54: space.
      numchar = partline(substr,f_plane,0,0)
      if (numchar .ne. 0) then
          if (numchar .gt. ISF_F_PLANE_LEN) then
              write (isf_bulletin_error,"('f_plane too long: ',a)") 
     + f_plane
              write_fault_plane = 20
              return
          end if
          if ( check_whole(f_plane) .eq. 1 ) then
              isf_bulletin_error = 'bad f_plane: '//f_plane
              write_fault_plane = 20
              return
          end if
          write(file,"(a5,' ',$)") f_plane
      else
          write(file,"('      ',$)")
      end if

c     Chars 55-63: author if this is 1st fault plane. Char 64: ')'.
      if (line_num .eq. 1) then
          numchar = partline(substr,author,0,0)
          if (numchar .eq. 0) then
              write (isf_bulletin_error,"('missing author')")
              write_fault_plane = 20
              return
          end if
          if (numchar .gt. ISF_AUTHOR_LEN) then
              write (isf_bulletin_error,"('author too long: ',a)") 
     + author
              write_fault_plane = 20
              return
          end if
          if ( check_whole(author) .eq. 1 ) then
              isf_bulletin_error = 'bad author: '//author
              write_fault_plane = 20
              return
          end if
          write(file,"(a9,')')") author
      else
          write (file,"('         )')")
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('fault_plane') .ne. 0) then
          write_fault_plane = 10
          return
      end if

      write_fault_plane = 0
      return
      end


c     Writes a principal axes header line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_axes_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(83)

      head = ' (#PRINAX sc  T_val T_azim  T_pl  B_val B_azim  B_pl  P_va
     +l P_azim  P_pl Author   )'

      write (file,"(a)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes_head') .ne. 0) then
          write_axes_head = 10
          return
      end if

      write_axes_head = 0
      return
      end

c     Writes a principal axes error header line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_axes_err_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(83)

      head = ' (+             eTv    eTa   eTp    eBv    eBa   eBp    eP
     +v    ePa   ePp fCLVD    )'

      write (file,"(a)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes_err_head') .ne. 0) then
          write_axes_err_head = 10
          return
      end if

      write_axes_err_head = 0
      return
      end

c     Writes a principal axes data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_axes(file,scale_factor,t_val,t_azim,
     +              t_pl,b_val,b_azim,b_pl,p_val,p_azim,p_pl,author)

      integer file
      character author*(*)
      integer scale_factor
      real t_val,t_azim,t_pl,b_val,b_azim,b_pl,p_val,p_azim,p_pl

      include 'isf_bul.h'
      integer partline, check_prev_line_type, is_null, check_whole
      integer numchar
      character substr*(ISF_LINE_LEN)

c     Chars 1-10: Comment start string.
      write (file,"(a,$)") ' (#       '

c     Chars 11,12: scale factor. Char 13: space.
      if (is_null(real(scale_factor)) .eq. 1) then
          write (file,"(a,$)") '   '
      else
          if ( scale_factor .lt. 0 .or. scale_factor .gt. 99 ) then
              write (isf_bulletin_error,"('bad scale_factor: ',i5)") 
     + scale_factor
              write_axes = 20
              return
          end if
          write (file,"(i2,' ',$)") scale_factor
      end if

c     Chars 14-19: t_val. Char 20: space.
      if (is_null(t_val) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( t_val .lt. -9.999 .or. t_val .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad t_val: ',f8.2)") t_val
              write_axes = 20
              return
          end if
          write (file,"(f6.3,' ',$)") t_val
      end if
  
c     Chars 21-26: t_azim. Char 27 space.
      if (is_null(t_azim) .eq. 1) then
          write (isf_bulletin_error,"('missing t_azim')")
          write_axes = 20
          return
      end if
      if ( t_azim .lt. 0 .or. t_azim .gt. 360 ) then
          write (isf_bulletin_error,"('bad t_azim: ',f8.2)") t_azim
          write_axes = 20
          return
      end if
      write (file,"(f6.2,' ',$)") t_azim

c     Chars 28-32: t_pl. Char 33 space.
      if (is_null(t_pl) .eq. 1) then
          write (isf_bulletin_error,"('missing t_pl')")
          write_axes = 20
          return
      end if
      if ( t_pl .lt. 0 .or. t_pl .gt. 90 ) then
          write (isf_bulletin_error,"('bad t_pl: ',f8.2)") t_pl
          write_axes = 20
          return
      end if
      write (file,"(f5.2,' ',$)") t_pl

c     Chars 34-39: b_val. Char 40: space.
      if (is_null(b_val) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( b_val .lt. -9.999 .or. b_val .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad b_val: ',f8.2)") b_val
              write_axes = 20
              return
          end if
          write (file,"(f6.3,' ',$)") b_val
      end if

c     Chars 41-46: b_azim. Char 47 space.
      if (is_null(b_azim) .eq. 1) then
          write (isf_bulletin_error,"('missing b_azim')")
          write_axes = 20
          return
      end if
      if ( b_azim .lt. 0 .or. b_azim .gt. 360 ) then
          write (isf_bulletin_error,"('bad b_azim: ',f8.2)") b_azim
          write_axes = 20
          return
      end if
      write (file,"(f6.2,' ',$)") b_azim

c     Chars 48-52: b_pl. Char 53 space.
      if (is_null(b_pl) .eq. 1) then
          write (isf_bulletin_error,"('missing b_pl')")
          write_axes = 20
          return
      end if
      if ( b_pl .lt. 0 .or. b_pl .gt. 90 ) then
          write (isf_bulletin_error,"('bad b_pl: ',f8.2)") b_pl
          write_axes = 20
          return
      end if
      write (file,"(f5.2,' ',$)") b_pl

c     Chars 54-59: p_val. Char 60: space.
      if (is_null(p_val) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( p_val .lt. -9.999 .or. p_val .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad p_val: ',f8.2)") p_val
              write_axes = 20
              return
          end if
          write (file,"(f6.3,' ',$)") p_val
      end if

c     Chars 61-66: p_azim. Char 67 space.
      if (is_null(p_azim) .eq. 1) then
          write (isf_bulletin_error,"('missing p_azim')")
          write_axes = 20
          return
      end if
      if ( p_azim .lt. 0 .or. p_azim .gt. 360 ) then
          write (isf_bulletin_error,"('bad p_azim: ',f8.2)") p_azim
          write_axes = 20
          return
      end if
      write (file,"(f6.2,' ',$)") p_azim

c     Chars 68-72: p_pl. Char 73 space.
      if (is_null(p_pl) .eq. 1) then
          write (isf_bulletin_error,"('missing p_pl')")
          write_axes = 20
          return
      end if
      if ( p_pl .lt. 0 .or. p_pl .gt. 90 ) then
          write (isf_bulletin_error,"('bad p_pl: ',f8.2)") p_pl
          write_axes = 20
          return
      end if
      write (file,"(f5.2,' ',$)") p_pl

c     Chars 74-82: author. Char 83: close bracket.
      numchar = partline(substr,author,0,0)
      if (numchar .eq. 0) then
          write (isf_bulletin_error,"('missing author')")
          write_axes = 20
          return
      end if
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_bulletin_error,"('author too long: ',a)") author
          write_axes = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          isf_bulletin_error = 'bad author: '//author
          write_axes = 20
          return
      end if
      write(file,"(a9,')')") author

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes') .ne. 0) then
          write_axes = 10
          return
      end if

      write_axes = 0
      return
      end


c     Write principal axes error line - allows anything and everthing to be null.
c     Would be possible to want to write only fCVLD or only errors.
c     Trust user not to send for it if have nothing at all to write.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_axes_err(file,t_val_unc,t_azim_unc,
     + t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc,p_val_unc,
     + p_azim_unc,p_pl_unc,fclvd)

      integer file
      real t_val_unc,t_azim_unc,t_pl_unc,b_val_unc,b_azim_unc,b_pl_unc
      real p_val_unc,p_azim_unc,p_pl_unc,fclvd

      include 'isf_bul.h'
      integer check_prev_line_type, is_null

c     Chars 1-14: Comment start string.
      write (file,"(a,$)") ' (+           '

c     Chars 15-19: t_val_unc. Char 20: space.
      if (is_null(t_val_unc) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( t_val_unc .lt. 0 .or. t_val_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad t_val_unc: ',f8.2)") 
     + t_val_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f5.3,' ',$)") t_val_unc
      end if

c     Chars 21-26: t_azim_unc. Char 27: space.
      if (is_null(t_azim_unc) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( t_azim_unc .lt. 0 .or. t_azim_unc .gt. 360 ) then
              write (isf_bulletin_error,"('bad t_azim_unc: ',f8.2)") 
     + t_azim_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f6.2,' ',$)") t_azim_unc
      end if

c     Chars 28-32: t_pl_unc. Char 33,34: spaces.
      if (is_null(t_pl_unc) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( t_pl_unc .lt. 0 .or. t_pl_unc .gt. 90 ) then
              write (isf_bulletin_error,"('bad t_pl_unc: ',f8.2)") 
     + t_pl_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f5.2,'  ',$)") t_pl_unc
      end if

c     Chars 35-39: b_val_unc. Char 40: space.
      if (is_null(b_val_unc) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( b_val_unc .lt. 0 .or. b_val_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad b_val_unc: ',f8.2)") 
     + b_val_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f5.3,' ',$)") b_val_unc
      end if

c     Chars 41-46: b_azim_unc. Char 47: space.
      if (is_null(b_azim_unc) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( b_azim_unc .lt. 0 .or. b_azim_unc .gt. 360 ) then
              write (isf_bulletin_error,"('bad b_azim_unc: ',f8.2)") 
     + b_azim_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f6.2,' ',$)") b_azim_unc
      end if

c     Chars 48-52: b_pl_unc. Char 53,54: spaces.
      if (is_null(b_pl_unc) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( b_pl_unc .lt. 0 .or. b_pl_unc .gt. 90 ) then
              write (isf_bulletin_error,"('bad b_pl_unc: ',f8.2)") 
     + b_pl_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f5.2,'  ',$)") b_pl_unc
      end if

c     Chars 55-59: p_val_unc. Char 60: space.
      if (is_null(p_val_unc) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( p_val_unc .lt. 0 .or. p_val_unc .gt. 9.999 ) then
              write (isf_bulletin_error,"('bad p_val_unc: ',f8.2)") 
     + p_val_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f5.3,' ',$)") p_val_unc
      end if

c     Chars 61-66: p_azim_unc. Char 67: space.
      if (is_null(p_azim_unc) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( p_azim_unc .lt. 0 .or. p_azim_unc .gt. 360 ) then
              write (isf_bulletin_error,"('bad p_azim_unc: ',f8.2)") 
     + p_azim_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f6.2,' ',$)") p_azim_unc
      end if

c     Chars 68-72: p_pl_unc. Char 73: space.
      if (is_null(p_pl_unc) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( p_pl_unc .lt. 0 .or. p_pl_unc .gt. 90 ) then
              write (isf_bulletin_error,"('bad p_pl_unc: ',f8.2)") 
     + p_pl_unc
              write_axes_err = 20
              return
          end if
          write (file,"(f5.2,' ',$)") p_pl_unc
      end if

c     Chars 74-78: fclvd. Chars 79-82: spaces to line up close brackets.
      if (is_null(fclvd) .eq. 1) then
          write (file,"(a)") '         )'
      else
          if ( fclvd .lt. 0 .or. fclvd .gt. 90 ) then
              write (isf_bulletin_error,"('bad fclvd: ',f8.2)") fclvd
              write_axes_err = 20
              return
          end if
          write (file,"(f5.3,'    )')") fclvd
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('axes_err') .ne. 0) then
          write_axes_err = 10
          return
      end if

      write_axes_err = 0
      return
      end


c     Writes  magnitude header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_netmag_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(38)

      head = 'Magnitude  Err Nsta Author      OrigID'

      write (file,"()")
      write (file,"(a)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('netmag_head') .ne. 0) then
          write_netmag_head = 10
          return
      end if

      write_netmag_head = 0
      return
      end

c     Writes a  magnitude data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_netmag(file,magtype,magind,mag,
     +        magerr,nsta,author,origid)

      integer file
      character magtype*(*), author*(*), origid*(*)
      character magind
      real mag,magerr
      integer nsta

      include 'isf_bul.h'
      integer partline,check_prev_line_type, is_null, check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-5: magtype.
      numchar = partline(substr,magtype,0,0)
      if (numchar .eq. 0) then
          write (isf_bulletin_error,"('missing magtype')")
          write_netmag = 20
          return
      end if
      if (numchar .gt. ISF_MAGTYPE_LEN) then
          write (isf_bulletin_error,"('magtype too long: ',a)") magtype
          write_netmag = 20
          return
      end if
      if ( check_whole(magtype) .eq. 1 ) then
          isf_bulletin_error = 'bad magtype: '//magtype
          write_netmag = 20
          return
      end if
      write(file,"(a5,$)") magtype

c     Char 6: less than or greater than indicator.
      if (magind .ne. ' ' .and. magind .ne. '<' .and. magind .ne. '>') 
     +then
          isf_bulletin_error = 'bad magind: '//magind
          write_netmag = 20
          return
      end if
      write(file,"(a1,$)") magind

c     Chars 7-10: magnitude value. Char 11 space.
      if (is_null(mag) .eq. 1) then
          isf_bulletin_error = 'missing mag'
          write_netmag = 20
          return
      end if
      if ( mag .lt. -1 .or. mag .gt. 12 ) then
          write (isf_bulletin_error,"('bad mag: ',f8.2)") mag
          write_netmag = 20
          return
      end if
      write (file,"(f4.1,' ',$)") mag

c     Chars 12-14: optional magnitude error. Char 15: space.
      if (is_null(magerr) .eq. 1) then
          write(file,"('    ',$)")
      else
          if ( magerr .lt. 0 .or. magerr .gt. 9.9 ) then
              write (isf_bulletin_error,"('bad magerr: ',f8.2)") magerr
              write_netmag = 20
              return
          end if
          write (file,"(f3.1,' ',$)") magerr
      end if

c     Chars 16-19 optional number of stations. Char 20: space.
      if (is_null(real(nsta)) .eq. 1) then
          write(file,"('     ',$)")
      else
          if ( nsta .lt. 0 .or. nsta .gt. 9999 ) then
              write (isf_bulletin_error,"('bad nsta: ',f8.2)") nsta
              write_netmag = 20
              return
          end if
          write (file,"(i4,' ',$)") nsta
      end if

c     Chars 21-29 author. Char 30 space.
      numchar = partline(substr,author,0,0)
      if (numchar .eq. 0) then
          write (isf_bulletin_error,"('missing author')")
          write_netmag = 20
          return
      end if
      if (numchar .gt. ISF_AUTHOR_LEN) then
          write (isf_bulletin_error,"('author too long: ',a)") author
          write_netmag = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          write (isf_bulletin_error,"('bad author: ',a)") author
          write_netmag = 20
          return
      end if
      write(file,"(a9,' ',$)") author

c     Chars 31-38 origid.
      numchar = partline(substr,origid,0,0)
      if (numchar .eq. 0) then
          write (isf_bulletin_error,"('missing origid')")
          write_netmag = 20
          return
      end if
      if (numchar .gt. ISF_ORIGID_LEN) then
          write (isf_bulletin_error,"('origid too long ',a)") origid
          write_netmag = 20
          return
      end if
      if ( check_whole(origid) .eq. 1 ) then
          write (isf_bulletin_error,"('bad origid ',a)") origid
          write_netmag = 20
          return
      end if
      write(file,"(a8)") origid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('netmag') .ne. 0) then
          write_netmag = 10
          return
      end if

      write_netmag = 0
      return
      end


c     Writes a list of the stations that were used to calculate a magnitude.
c     Will write any number, starting new lines as necessary.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_netmag_sta(file,sta,n)

      integer file, n
      character sta(*)*(*)

      include 'isf_bul.h'
      integer partline,check_prev_line_type, check_whole
      character substr*(ISF_LINE_LEN)
      integer i,numchar
      integer data_len

      write(file,"(' (#STATIONS',$)")

c     Don't include the space after #STATIONS
      data_len = -1        
      do i=1,n
          numchar = partline(substr,sta(i),0,0)
          if (numchar .gt. ISF_NET_LEN+ISF_STA_LEN) then
              write (isf_bulletin_error,"('net/sta code too long ',a)") 
     + sta(i)
              write_netmag_sta = 20
              return
          end if
          if (check_whole(sta(i)) .eq. 1) then
              write (isf_bulletin_error,"('bad net/sta code ',a)") 
     + sta(i)
              write_netmag_sta = 20
              return
          end if    
          data_len = data_len + numchar + 1
          if (data_len .gt. ISF_COMM_LEN) then
              write(file,"(')')")
              write(file,"(' (+        ',$)")
              data_len = numchar + 1
          end if
          write(file,"(' ',a,$)") sta(i)(1:numchar)
      end do
      write(file,"(')')")

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('netmag_com') .ne. 0) then
          write_netmag_sta = 10
          return
      end if

      write_netmag_sta = 0
      return
      end

c     Writes a netmag basis data line.
c     Only expects one parameter=value pair.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_netmag_basis(file,param,value)

      integer file
      character param*(*), value*(*)

      include 'isf_bul.h'
      integer partline,check_prev_line_type, check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar_param, numchar_value

      numchar_param = partline(substr,param,0,0)
      numchar_value = partline(substr,value,0,0)
      if (check_whole(param) .eq. 1) then
          write (isf_bulletin_error,"('bad param: ',a)") param
          write_netmag_basis = 20
          return
      end if
      if (check_whole(value) .eq. 1) then
          write (isf_bulletin_error,"('bad value: ',a)") value
          write_netmag_basis = 20
          return
      end if
      if (numchar_param + numchar_value + 1 .gt. ISF_COMM_LEN) then
          write (isf_bulletin_error,"('too long: ',a,$)") 
     + param(1:numchar_param)
          write (isf_bulletin_error,"('=',a)") value(1:numchar_value)
          write_netmag_basis = 20
          return
      end if

      write(file,"(' (#BASIS ',a,$)") param(1:numchar_param)
      write(file,"('=',a,')')") value(1:numchar_value)

      if (check_prev_line_type('netmag_com') .ne. 0) then
          write_netmag_basis = 10
          return
      end if

      write_netmag_basis = 0
      return
      end

c     Writes  effects header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_effects_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(69)

      head = 'Effects              Loctyp Location           Intensity S
     +cale Author'

      write (file,"()")
      write (file,"(a)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('effects_head') .ne. 0) then
          write_effects_head = 10
          return
      end if

      write_effects_head = 0
      return
      end

c     Writes an effects block data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_effects(file,heard,felt,damage,casualties,
     + uplift,subsidence,fault,tsunami,seiche,volcano,acoustic,gravity,
     + t_wave,liquification,geyser,landslide,sandblow,cracks,lights,
     + odours,loctype,lat,lon,dist,azim,country,postcode,net,
     + sta,intensity1,modifier,intensity2,scale,author)

      integer file
      character author*(*),loctype*(*)
      character scale*(*),country*(*),postcode*(*),net*(*),sta*(*)
      character heard,felt,damage,casualties,uplift,subsidence
      character fault,tsunami,seiche,volcano,acoustic,gravity
      character t_wave,liquification,geyser
      character landslide,sandblow,cracks,lights,odours
      character modifier
      real lat,lon,dist,azim,intensity1,intensity2

      include 'isf_bul.h'
      integer partline,check_prev_line_type,is_null,check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Char 1: heard flag.
      if (heard .ne. '_' .and. heard .ne. 'H' ) then
          isf_bulletin_error = 'bad heard flag: '//heard
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") heard

c     Char 2: felt flag.
      if (felt .ne. '_' .and. felt .ne. 'F' ) then
          isf_bulletin_error = 'bad felt flag: '//felt
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") felt

c     Char 3: damage flag.
      if (damage .ne. '_' .and. damage .ne. 'D' ) then
          isf_bulletin_error = 'bad damage flag: '//damage
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") damage

c     Char 4: casualties flag.
      if (casualties .ne. '_' .and. casualties .ne. 'C' ) then
          isf_bulletin_error = 'bad casualties flag: '//casualties
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") casualties

c     Char 5: uplift flag.
      if (uplift .ne. '_' .and. uplift .ne. 'U' ) then
          isf_bulletin_error = 'bad uplift flag: '//uplift
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") uplift

c     Char 6: subsidence flag.
      if (subsidence .ne. '_' .and. subsidence .ne. 'S' ) then
          isf_bulletin_error = 'bad subsidence flag: '//subsidence
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") subsidence

c     Char 7: surface faulting flag.
      if (fault .ne. '_' .and. fault .ne. 'F' ) then
          isf_bulletin_error = 'bad surface faulting flag: '//fault
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") fault

c     Char 8: tsunami flag.
      if (tsunami .ne. '_' .and. tsunami .ne. 'T' .and. tsunami .ne. 
     + 'Q') then
          isf_bulletin_error = 'bad tsunami flag: '//tsunami
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") tsunami

c     Char 9: seiche flag.
      if (seiche .ne. '_' .and. seiche .ne. 'S' .and. seiche .ne. 'Q') 
     +then
          isf_bulletin_error = 'bad seiche flag: '//seiche
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") seiche

c     Char 10: volcano flag.
      if (volcano .ne. '_' .and. volcano .ne. 'V' ) then
          isf_bulletin_error = 'bad volcano flag: '//volcano
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") volcano

c     Char 11: acoustic flag.
      if (acoustic .ne. '_' .and. acoustic .ne. 'A' ) then
          isf_bulletin_error = 'bad acoustic flag: '//acoustic
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") acoustic

c     Char 12: gravity flag.
      if (gravity .ne. '_' .and. gravity .ne. 'G' ) then
          isf_bulletin_error = 'bad gravity flag: '//gravity
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") gravity

c     Char 13: t_wave flag.
      if (t_wave .ne. '_' .and. t_wave .ne. 'T' ) then
          isf_bulletin_error = 'bad t_wave flag: '//t_wave
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") t_wave

c     Char 14: liquification flag.
      if (liquification .ne. '_' .and. liquification .ne. 'L' ) then
          isf_bulletin_error = 'bad liquification flag: 
     +'//liquification
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") liquification

c     Char 15: geyser flag.
      if (geyser .ne. '_' .and. geyser .ne. 'G' ) then
          isf_bulletin_error = 'bad geyser flag: '//geyser
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") geyser

c     Char 16: landslide flag.
      if (landslide .ne. '_' .and. landslide .ne. 'S' ) then
          isf_bulletin_error = 'bad landslide flag: '//landslide
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") landslide

c     Char 17: sandblow flag.
      if (sandblow .ne. '_' .and. sandblow .ne. 'B' ) then
          isf_bulletin_error = 'bad sandblow flag: '//sandblow
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") sandblow

c     Char 18: cracks flag.
      if (cracks .ne. '_' .and. cracks .ne. 'C' ) then
          isf_bulletin_error = 'bad cracks flag: '//cracks
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") cracks

c     Char 19: lights flag.
      if (lights .ne. '_' .and. lights .ne. 'V' ) then
          isf_bulletin_error = 'bad lights flag: '//lights
          write_effects = 20
          return
      end if
      write (file,"(a1,$)") lights

c     Char 20: odours flag. Char 21 space.
      if (odours .ne. '_' .and. odours .ne. 'V' ) then
          isf_bulletin_error = 'bad odours flag: '//odours
          write_effects = 20
          return
      end if
      write (file,"(a1,' ',$)") odours

c     Chars 22-27 loctype. Char 28 space.
c     Chars 29-46 depend on loctype. Char 47: space.
      if (loctype .eq. 'Summar') then

          write (file,"(a,' ',$)") loctype

c     	Chars 29-46 blank.
          write (file,"('                   ',$)")

      elseif (loctype .eq. 'LatLon') then

          write (file,"(a,' ',$)") loctype

c     	Chars 29-36: lattitude. Char 37: space.
        if ( is_null(lat) .eq. 1 ) then
              isf_bulletin_error = 'missing lattitude'
              write_effects = 20
              return
          end if
          if ( lat .le. -90 .or. lat .gt. 90 ) then
              write (isf_bulletin_error,"('bad lat: ',f8.2)") lat
              write_effects = 20
              return
          end if
          write (file,"(f8.4,' ',$)") lat

c     	Chars 38-46: longitude. Char 47: space.
        if ( is_null(lon) .eq. 1 ) then
              isf_bulletin_error = 'missing longitude'
              write_effects = 20
              return
          end if
          if ( lon .lt. -180 .or. lon .gt. 180 ) then
              write (isf_bulletin_error,"('bad lon: ',f8.2)") lon
              write_effects = 20
              return
          end if
          write (file,"(f9.4,' ',$)") lon

      elseif (loctype .eq. 'DistAz') then

          write (file,"(a,' ',$)") loctype

c     	Chars 29-36: distance. Char 37: space.
        if ( is_null(dist) .eq. 1 ) then
              isf_bulletin_error = 'missing dist'
              write_effects = 20
              return
          end if
          if ( dist .lt. 0 .or. dist .gt. 99999 ) then
              write (isf_bulletin_error,"('bad dist: ',f8.2)") dist
              write_effects = 20
              return
          end if
          write (file,"(f5.1,' ',$)") dist

c     	Chars 38-42: azimuth. Chars 43-47 space.
        if ( is_null(azim) .eq. 1 ) then
              isf_bulletin_error = 'missing azim'
              write_effects = 20
              return
          end if
          if ( azim .lt. 0 .or. azim .gt. 360 ) then
              write (isf_bulletin_error,"('bad azim: ',f8.2)") azim
              write_effects = 20
              return
          end if
          write (file,"(f5.1,'     ',$)") azim


      elseif (loctype .eq. 'CoPost') then

          write (file,"(a,' ',$)") loctype

c     	Chars 29-31: country code. Chars 32 space.
          numchar = partline(substr,country,0,0)
          if (numchar .eq. 0) then
              write (isf_bulletin_error,"('missing country')")
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_COUNTRY_LEN) then
              write (isf_bulletin_error,"('country too long: ',a)") 
     + country
              write_effects = 20
              return
          end if
          write(file,"(a3,' ',$)") country

c     	Chars 33-42: post code. Chars 43-47 space.
          numchar = partline(substr,postcode,0,0)
          if (numchar .eq. 0) then
              write (isf_bulletin_error,"('missing postcode')")
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_POSTCODE_LEN) then
              write (isf_bulletin_error,"('postcode too long: ',a)") 
     + postcode
              write_effects = 20
              return
          end if
          write(file,"(a10,'     ',$)") postcode

      elseif (loctype .eq. 'StaNet') then

          write (file,"(a,' ',$)") loctype

c     	Chars 29-37: network code. Char 38: space.
          numchar = partline(substr,net,0,0)
          if (numchar .eq. 0) then
              write (isf_bulletin_error,"('missing net')")
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_NET_LEN) then
              write (isf_bulletin_error,"('net too long: ',a)") net
              write_effects = 20
              return
          end if
          write(file,"(a9,' ',$)") net

c     	Chars 39-43: station code. Chars 44-47: spaces.
          numchar = partline(substr,sta,0,0)
          if (numchar .eq. 0) then
              write (isf_bulletin_error,"('missing sta')")
              write_effects = 20
              return
          end if
          if (numchar .gt. ISF_STA_LEN) then
              write (isf_bulletin_error,"('sta too long: ',a)") sta
              write_effects = 20
              return
          end if
          write(file,"(a5,'    ',$)") sta

      else
          isf_bulletin_error = 'unknown loctype: '//loctype
          write_effects = 20
          return
      end if

c     Chars 48-51: first intensity.
c     If first intensity null then don't allow second one or scale.
      if ( is_null(intensity1) .ne. 1 ) then
          if ( intensity1 .lt. 0 .or. intensity1 .gt. 12 ) then
              write (isf_bulletin_error,"('bad intensity1: ',f8.2)") 
     + intensity1
              write_effects = 20
              return
          end if
          write (file,"(f4.1,$)") intensity1

c     	Char 52 intensity modifier.
          if (modifier .ne. ' ' .and. modifier .ne. '+' .and. modifier 
     + .ne. '-') then
              isf_bulletin_error = 'bad intensity modifier: '//modifier
              write_effects = 20
              return
          end if
          write (file,"(a1,$)") modifier

c     	Chars 53-56: second intensity, only allowed if modifier is '-'.
c     	Char 57: space.
          if (modifier .eq. '-') then
              if ( is_null(intensity2) .eq. 1 ) then
                  isf_bulletin_error = 'missing intensity2'
                  write_effects = 20
                  return
              end if
              if ( intensity2 .lt. 0 .or. intensity2 .gt. 12 ) then
                  write (isf_bulletin_error,"('bad intensity2: ',f8.2)") 
     + intensity2
                  write_effects = 20
                  return
              end if
              write (file,"(f4.1,' ',$)") intensity2
          else
              if ( is_null(intensity2) .eq. 0 ) then
                  isf_bulletin_error = 'bad modifier if want 
     + intensity2'
                  write_effects = 20
                  return
              end if
              write (file,"('     ',$)")
          end if

c     	Chars 58-62: intensity scale. Char 63 space.
          numchar = partline(substr,scale,0,0)
          if ( numchar .ne. 0 ) then
              if ( numchar .gt. ISF_I_SCALE_LEN ) then
                  write (isf_bulletin_error,"('scale too long: ',a)") 
     + scale
                  write_effects = 20
                  return
              end if
              if ( check_whole(scale) .eq. 1 ) then
                  write (isf_bulletin_error,"('bad scale: ',a)") scale
                  write_effects = 20
                  return
              end if
              write(file,"(a5,' ',$)") scale
          else
              write(file,"('      ',$)")
          end if
      else
          if ( modifier .ne. ' ' ) then
              isf_bulletin_error = 'modifier without intensity1'
              write_effects = 20
              return
          end if
          if ( is_null(intensity2) .eq. 0 ) then
              isf_bulletin_error = 'intensity2 without intensity1'
              write_effects = 20
              return
          end if

          numchar = partline(substr,scale,0,0)
          if ( numchar .ne. 0 ) then
              isf_bulletin_error = 'scale without intensity1'
              write_effects = 20
              return
          end if
          write(file,"(a16,$)") ' '
      end if

c     Chars 64-72 author.
      numchar = partline(substr,author,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_bulletin_error,"('missing author')")
          write_effects = 20
          return
      end if
      if ( numchar .gt. ISF_AUTHOR_LEN ) then
          write (isf_bulletin_error,"('author too long: ',a)") author
          write_effects = 20
          return
      end if
      if ( check_whole(author) .eq. 1 ) then
          write (isf_bulletin_error,"('bad author: ',a)") author
          write_effects = 20
          return
      end if
      write(file,"(a9)") author

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('effects') .ne. 0) then
          write_effects = 10
          return
      end if

      write_effects = 0
      return
      end


c     Writes  phase header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(122)

      head = 'Sta     Dist  EvAz Phase        Time      TRes  Azim AzRes
     +   Slow   SRes Def   SNR       Amp   Per Qual Magnitude    ArrID'


      write (file,"()")
      write (file,"(a)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_head') .ne. 0) then
          write_phase_head = 10
          return
      end if

      write_phase_head = 0
      return
      end


c     Writes a phase block data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase(file,sta,dist,esaz,phase,hh,mi,
     + ss,msec,timeres,azim,azimres,slow,slowres,timedef,azimdef,
     + slowdef,snr,amp,per,picktype,sp_fm,detchar,magtype,magind,
     + mag,arrid)

      integer file
      character sta*(*),arrid*(*),phase*(*),magtype*(*)
      character timedef,azimdef,slowdef,sp_fm,detchar,magind,picktype
      real dist,esaz,timeres,azim,azimres,slow,slowres,snr,amp,per,mag
      integer hh,mi,ss,msec

      include 'isf_bul.h'
      integer partline,check_prev_line_type,is_null,check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-5: station code. Char 6: space.
      numchar = partline(substr,sta,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_bulletin_error,"('missing sta')")
          write_phase = 20
          return
      end if
      if ( numchar .gt. ISF_STA_LEN ) then
          write (isf_bulletin_error,"('sta too long: ',a)") sta
          write_phase = 20
          return
      end if
      if ( check_whole(sta) .eq. 1 ) then
          write (isf_bulletin_error,"('bad sta: ',a)") sta
          write_phase = 20
          return
      end if
      write(file,"(a5,' ',$)") sta

c     Chars 7-12: distance. Char 13: space.
      if (is_null(dist) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( dist .lt. 0 .or. dist .gt. 999.99 ) then
              write (isf_bulletin_error,"('bad dist: ',f8.2)") dist
              write_phase = 20
              return
          end if
          write (file,"(f6.2,' ',$)") dist
      end if

c     Chars 14-18: event to sta azimuth. Char 19: space.
      if (is_null(esaz) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( esaz .lt. 0 .or. esaz .gt. 360 ) then
              write (isf_bulletin_error,"('bad esaz: ',f8.2)") esaz
              write_phase = 20
              return
          end if
          write (file,"(f5.1,' ',$)") esaz
      end if

c     Chars 20-27: phase code - can be null. Char 28: space.
      numchar = partline(substr,phase,0,0)
      if ( numchar .eq. 0 ) then
          write (file,"(a8,' ',$)") ' '
      else
          if ( numchar .gt. ISF_PHASE_LEN ) then
              write (isf_bulletin_error,"('phase too long: ',a)") phase
              write_phase = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_bulletin_error,"('bad phase: ',a)") phase
              write_phase = 20
              return
          end if
          write(file,"(a8,' ',$)") phase
      end if

c     Chars 29-40: time. Char 41: space.
c     Time can be completely null.
      if (is_null(real(hh)) .eq. 1 .and. is_null(real(mi)) .eq. 1 .and.
     + is_null(real(ss)) .eq. 1) then

          write(file,"(a,$)") '             '
      else
          if (is_null(real(hh)) .eq. 1) then
              isf_bulletin_error = 'missing hour'
              write_phase = 20
              return
          end if

          if (hh .lt. 0 .or. hh .gt. 23) then
              write (isf_bulletin_error,"('bad hour ',i2)") hh
              write_phase = 20
              return
          end if

          if (is_null(real(mi)) .eq. 1) then
              isf_bulletin_error = 'missing minute'
              write_phase = 20
              return
          end if

          if (mi .lt. 0 .or. mi .gt. 59) then
              write (isf_bulletin_error,"('bad minute  ',i2)") mi
              write_phase = 20
              return
          end if

          if (is_null(real(ss)) .eq. 1) then
              isf_bulletin_error = 'missing second'
              write_phase = 20
              return
          end if

          if (ss .lt. 0 .or. ss .gt. 59) then
              write (isf_bulletin_error,"('bad second ',i2)") ss
              write_phase = 20
              return
          end if
          write (file,"(i2.2,':',i2.2,':',i2.2,$)") hh,mi,ss

c     	Chars 37-40 msec - put blanks here if no msec provided.
          if (is_null(real(msec)) .eq. 1) then
              write(file,"('     ',$)")
          else
              if (msec .lt. 0 .or. msec .gt. 999) then
                  write (isf_bulletin_error,"('bad msec ',i3)") msec
                  write_phase = 20
                  return
              end if
              write(file,"('.',i3.3,' ',$)") msec
          end if
      end if
      
c     Chars 42-46: time residual. Char 47: space.
      if (is_null(timeres) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( timeres .lt. -9999 .or. timeres .gt. 9999 ) then
              write (isf_bulletin_error,"('bad timeres: ',f8.2)") timeres
              write_phase = 20
              return
          end if
          call write_real(file,timeres,5,1)
          write (file,"(' ',$)")
      end if

c     Chars 48-52: observed azimuth. Char 53: space.
      if (is_null(azim) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( azim .lt. 0 .or. azim .gt. 360 ) then
              write (isf_bulletin_error,"('bad azim: ',f8.2)") azim
              write_phase = 20
              return
          end if
          write (file,"(f5.1,' ',$)") azim
      end if

c     Chars 54-58: azimuth residual. Char 59: space.
      if (is_null(azimres) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( azimres .lt. -360 .or. azimres .gt. 360 ) then
              write (isf_bulletin_error,"('bad azimres: ',f8.2)") azimres
              write_phase = 20
              return
          end if
          call write_real(file,azimres,5,1)
          write (file,"(' ',$)")
      end if

c     Chars 60-65: slowness. Char 66: space.
      if (is_null(slow) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( slow .lt. 0 .or. slow .gt. 999.99 ) then
              write (isf_bulletin_error,"('bad slow: ',f8.2)") slow
              write_phase = 20
              return
          end if
          write (file,"(f6.2,' ',$)") slow
      end if

c     Chars 67-72: slowness residual. Char 73: space.
      if (is_null(slowres) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( slowres .lt. -99999 .or. slowres .gt. 99999 ) then
              write (isf_bulletin_error,"('bad slowres: ',f8.2)") slowres
              write_phase = 20
              return
          end if
          call write_real(file,slowres,6,1)
          write (file,"(' ',$)")
      end if

c     Char 74: time defining flag.
      if (timedef .eq. ' ') then
          timedef = '_'
      else if (timedef .ne. '_' .and. timedef .ne. 'T' ) then
          isf_bulletin_error = 'bad timedef flag: '//timedef
          write_phase = 20
          return
      end if
      write (file,"(a1,$)") timedef

c     Char 75: azimuth defining flag.
      if (azimdef .eq. ' ') then
          azimdef = '_'
      else if (azimdef .ne. '_' .and. azimdef .ne. 'A' ) then
          isf_bulletin_error = 'bad azimdef flag: '//azimdef
          write_phase = 20
          return
      end if
      write (file,"(a1,$)") azimdef

c     Char 76: slowness defining flag. Char 77: space.
      if (slowdef .eq. ' ') then
          slowdef = '_'
      else if (slowdef .ne. '_' .and. slowdef .ne. 'S' ) then
          isf_bulletin_error = 'bad slowdef flag: '//slowdef
          write_phase = 20
          return
      end if
      write (file,"(a1,' ',$)") slowdef

c     Chars 78-82: signal-to noise. Char 83: space.
      if (is_null(snr) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( snr .lt. 0 .or. snr .gt. 999 ) then
              write (isf_bulletin_error,"('bad snr: ',f8.2)") snr
              write_phase = 20
              return
          end if
          write (file,"(f5.1,' ',$)") snr
      end if

c     Chars 84-92: amplitude. Char 93: space.
      if (is_null(amp) .eq. 1) then
          write (file,"(a,$)") '          '
      else
          if ( amp .lt. 0 .or. amp .gt. 9999999.9 ) then
              write (isf_bulletin_error,"('bad amp: ',f8.2)") amp
              write_phase = 20
              return
          end if
          write (file,"(f9.1,' ',$)") amp
      end if

c     Chars 94-98: period. Char 99: space.
      if (is_null(per) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( per .lt. 0 .or. per .gt. 99.99 ) then
              write (isf_bulletin_error,"('bad per: ',f8.2)") per
              write_phase = 20
              return
          end if
          write (file,"(f5.2,' ',$)") per
      end if

c     Char 100: picktype.
      if (picktype .eq. ' ') then
          picktype = '_'
      else if (picktype .ne. '_' .and. picktype .ne. 'a' .and. picktype 
     + .ne. 'm') then
          isf_bulletin_error = 'bad picktype: '//picktype
          write_phase = 20
          return
      end if
      write (file,"(a1,$)") picktype

c     Char 101: sp_fm.
      if (sp_fm .eq. ' ') then
          sp_fm = '_'
      else if (sp_fm .ne. '_' .and. sp_fm .ne. 'c' .and. sp_fm .ne. 
     + 'd') then
          isf_bulletin_error = 'bad sp_fm: '//sp_fm
          write_phase = 20
          return
      end if
      write (file,"(a1,$)") sp_fm

c     Char 102: detchar. Char 103: space.
      if (detchar .eq. ' ') then
          detchar = '_'
      else if (detchar .ne. '_' .and. detchar .ne. 'i'  
     +         .and. detchar .ne. 'e' .and. detchar .ne. 'q') then
          isf_bulletin_error = 'bad detchar: '//detchar
          write_phase = 20
          return
      end if
      write (file,"(a1,' ',$)") detchar

c     Chars 104-108: magnitude type.
      numchar = partline(substr,magtype,0,0)
      if ( numchar .eq. 0 ) then
          write (file,"(a5,$)") ' '
      else
          if ( numchar .gt. ISF_MAGTYPE_LEN ) then
              write (isf_bulletin_error,"('magtype too long: ',a)") 
     + magtype
              write_phase = 20
              return
          end if
          if ( check_whole(magtype) .eq. 1 ) then
              write (isf_bulletin_error,"('bad magtype: ',a)") magtype
              write_phase = 20
              return
          end if
          write(file,"(a5,$)") magtype
      end if

c     Char 109: magnitude indicator.
      if (magind .ne. ' ' .and. magind .ne. '>' .and. magind .ne. '<') 
     + then
          isf_bulletin_error = 'bad magind: '//magind
          write_phase = 20
          return
      end if
      write (file,"(a1,$)") magind

c     Chars 110-113: magnitude. Char 114: space.
      if (is_null(mag) .eq. 1) then
          write (file,"(a,$)") '     '
      else
          if ( mag .lt. 0 .or. mag .gt. 10 ) then
              write (isf_bulletin_error,"('bad mag: ',f8.2)") mag
              write_phase = 20
              return
          end if
          write (file,"(f4.1,' ',$)") mag
      end if

c     Chars 115-122: arrival ID.
      numchar = partline(substr,arrid,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_bulletin_error,"('missing arrid')")
          write_phase = 20
          return
      end if
      if ( numchar .gt. ISF_ARRID_LEN ) then
          write (isf_bulletin_error,"('arrid too long: ',a)") arrid
          write_phase = 20
          return
      end if
      if ( check_whole(arrid) .eq. 1 ) then
          write (isf_bulletin_error,"('bad arrid: ',a)") arrid
          write_phase = 20
          return
      end if
      write(file,"(a8)") arrid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase') .ne. 0) then
          write_phase = 10
          return
      end if

      write_phase = 0
      return
      end

c     Writes a phase origin-id comment line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_origid(file,origid)

      integer file
      character origid*(*)

      include 'isf_bul.h'
      integer check_prev_line_type,partline,check_whole
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-10: comment start string and space.
      write (file,"(a,$)") ' (#OrigID '

c     Chars 11-18: origin ID.
      numchar = partline(substr,origid,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_bulletin_error,"('missing origid')")
          write_phase_origid = 20
          return
      end if
      if ( numchar .gt. ISF_ORIGID_LEN ) then
          write (isf_bulletin_error,"('origid too long',a)") origid
          write_phase_origid = 20
          return
      end if
      if ( check_whole(origid) .eq. 1 ) then
          write (isf_bulletin_error,"('bad origid',a)") origid
          write_phase_origid = 20
          return
      end if
      write(file,"(a8,')')") origid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_origid') .ne. 0) then
          write_phase_origid = 10
          return
      end if

      write_phase_origid = 0
      return
      end


c     Writes phase info header complete with preceding blank line.

c     Returns 0 on a successful write.  
c     Returns 10 if these lines should not follow the previous line written.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_info_head(file)

      integer file
      include 'isf_bul.h'
      integer check_prev_line_type

      character head*(123)

      head = 'Net      Chan F Low_F HighF AuthPhas    Date     eTime wTi
     +me eAzim wAzim  eSlow wSlow      eAmp  ePer eMag Author     ArrID'

      write (file,"()")
      write (file,"(a)") head

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_head') .ne. 0) then
          write_phase_info_head = 10
          return
      end if

      write_phase_info_head = 0
      return
      end

c     Writes a phase info block data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_info(file,net,chan,filter,
     + filter_min,filter_max,phase,yyyy,mm,dd,time_unc,time_weight, 
     + azim_unc,azim_weight,slow_unc,slow_weight,amp_unc,
     + per_unc,mag_unc,author,arrid)

      integer file
      character net*(*),chan*(*),author*(*),arrid*(*),phase*(*)
      character filter
      real filter_min,filter_max,time_unc,time_weight,azim_unc
      real azim_weight
      real slow_unc,slow_weight,amp_unc,per_unc,mag_unc
      integer yyyy,mm,dd

      include 'isf_bul.h'
      integer check_prev_line_type,partline,check_whole,is_null
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-9: network code. Char 10: space.
      numchar = partline(substr,net,0,0)
      if ( numchar .eq. 0 ) then
          write(file,"(a10,$)") ' '
      else
          if ( numchar .gt. ISF_NET_LEN ) then
              write (isf_bulletin_error,"('net too long: ',a)") net
              write_phase_info = 20
              return
          end if
          if ( check_whole(net) .eq. 1 ) then
              write (isf_bulletin_error,"('bad net: ',a)") net
              write_phase_info = 20
              return
          end if
          write(file,"(a9,' ',$)") net
      end if

c     Chars 11-13: channel. Char 14: space.
      numchar = partline(substr,chan,0,0)
      if ( numchar .eq. 0 ) then
          write(file,"('    ',$)")
      else
          if ( numchar .gt. ISF_CHAN_LEN ) then
              write (isf_bulletin_error,"('chan too long: ',a)") chan
              write_phase_info = 20
              return
          end if
          if ( check_whole(chan) .eq. 1 ) then
              write (isf_bulletin_error,"('bad chan: ',a)") chan
              write_phase_info = 20
              return
          end if
          write(file,"(a3,' ',$)") chan
      end if

c     Char 15: filter. Char 16: space.
      if (filter .ne. '0' .and. filter .ne. 'C' .and. filter .ne. ' ') 
     + then
          isf_bulletin_error = 'bad filter: '//filter
          write_phase_info = 20
          return
      end if
      write (file,"(a1,' ',$)") filter

c     Chars 17-21: minimum filter frequency. Char 22: space.
      if (is_null(filter_min) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( filter_min .lt. 0 .or. filter_min .gt. 99.99 ) then
              write (isf_bulletin_error,"('bad filter_min: ',f8.2)") 
     + filter_min
              write_phase_info = 20
              return
          end if
          write (file,"(f5.2,' ',$)") filter_min
      end if

c     Chars 23-27: maximum filter frequency. Char 28: space.
      if (is_null(filter_max) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( filter_max .lt. 0 .or. filter_max .gt. 99.99 ) then
              write (isf_bulletin_error,"('bad filter_max: ',f8.2)") 
     + filter_max
              write_phase_info = 20
              return
          end if
          write (file,"(f5.2,' ',$)") filter_max
      end if

c     Chars 29-36: author's phase. Char 37: space.
      numchar = partline(substr,phase,0,0)
      if ( numchar .eq. 0 ) then
          write(file,"(a9,$)") ' '
      else
          if ( numchar .gt. ISF_PHASE_LEN ) then
              write (isf_bulletin_error,"('phase too long: ',a)") phase
              write_phase_info = 20
              return
          end if
          if ( check_whole(phase) .eq. 1 ) then
              write (isf_bulletin_error,"('bad phase: ',a)") phase
              write_phase_info = 20
              return
          end if
          write(file,"(a8,' ',$)") phase
      end if


c     Chars 38-47: arrival date. Char 48: space.
      if (is_null(real(yyyy)) .eq. 1 .and. is_null(real(mm)) .eq. 1 
     + .and. is_null(real(dd)) .eq. 1) then

          write(file,"(a,$)") '           '
      else
          if (is_null(real(yyyy)) .eq. 1) then
              isf_bulletin_error = 'date but no year'
              write_phase_info = 20
              return
          end if

          if (yyyy .lt. 0 .or. yyyy .gt. 9999) then
              write (isf_bulletin_error,"('bad year ',i4)") yyyy
              write_phase_info = 20
              return
          end if

          if (is_null(real(mm)) .eq. 1) then
              isf_bulletin_error = 'year but no month'
              write_phase_info = 20
              return
          end if

          if (mm .lt. 0 .or. mm .gt. 12) then
              write (isf_bulletin_error,"('bad month ',i2)") mm
              write_phase_info = 20
              return
          end if

          if (is_null(real(dd)) .eq. 1) then
              isf_bulletin_error = 'year but no day'
              write_phase_info = 20
              return
          end if

          if (dd .lt. 0 .or. dd .gt. 31) then
              write (isf_bulletin_error,"('bad day ',i2)") dd
              write_phase_info = 20
              return
          end if
          write (file,"(i4.4,'/',i2.2,'/',i2.2,' ',$)") yyyy,mm,dd
      end if

c     Chars 49-54: uncertainty in arrival time. Char 55 space.
      if (is_null(time_unc) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( time_unc .lt. 0 .or. time_unc .gt. 99 ) then
              write (isf_bulletin_error,"('bad time_unc: ',f8.2)") 
     + time_unc
              write_phase_info = 20
              return
          end if
          write (file,"(f6.3,' ',$)") time_unc
      end if

c     Chars 56-60: time weight. Char 61 space.
      if (is_null(time_weight) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( time_weight .lt. 0 .or. time_weight .gt. 1 ) then
              write (isf_bulletin_error,"('bad time_weight: ',f8.2)") 
     + time_weight
              write_phase_info = 20
              return
          end if
          write (file,"(f5.3,' ',$)") time_weight
      end if

c     Chars 62-66: azimuth uncertainty. Char 67 space.
      if (is_null(azim_unc) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( azim_unc .lt. 0 .or. azim_unc .gt. 360 ) then
              write (isf_bulletin_error,"('bad azim_unc: ',f8.2)") 
     + azim_unc
              write_phase_info = 20
              return
          end if
          write (file,"(f5.1,' ',$)") azim_unc
      end if

c     Chars 68-72: azimuth weight. Char 73 space.
      if (is_null(azim_weight) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( azim_weight .lt. 0 .or. azim_weight .gt. 1 ) then
              write (isf_bulletin_error,"('bad azim_weight: ',f8.2)") 
     + azim_weight
              write_phase_info = 20
              return
          end if
          write (file,"(f5.3,' ',$)") azim_weight
      end if

c     Chars  74-79: slowness uncertainty. Char 80 space.
      if (is_null(slow_unc) .eq. 1) then
          write (file,"(a,$)") '       '
      else
          if ( slow_unc .lt. 0 .or. slow_unc .gt. 9999.9 ) then
              write (isf_bulletin_error,"('bad slow_unc: ',f8.2)") 
     + slow_unc
              write_phase_info = 20
              return
          end if
          write (file,"(f6.1,' ',$)") slow_unc
      end if

c     Chars 81-85: slowness weight. Char 86 space.
      if (is_null(slow_weight) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( slow_weight .lt. 0 .or. slow_weight .gt. 1 ) then
              write (isf_bulletin_error,"('bad slow_weight: ',f8.2)") 
     + slow_weight
              write_phase_info = 20
              return
          end if
          write (file,"(f5.3,' ',$)") slow_weight
      end if

c     Chars 87-95: amplitude unceratinty. Char 96 space.
      if (is_null(amp_unc) .eq. 1) then
          write (file,"(a,$)") '          '
      else
          if ( amp_unc .lt. 0 .or. amp_unc .gt. 9999999.9 ) then
              write (isf_bulletin_error,"('bad amp_unc: ',f8.2)") amp_unc
              write_phase_info = 20
              return
          end if
          write (file,"(f9.1,' ',$)") amp_unc
      end if

c     Chars 97-101: period uncertainty. Char 102 space.
      if (is_null(per_unc) .eq. 1) then
          write (file,"(a,$)") '      '
      else
          if ( per_unc .lt. 0 .or. per_unc .gt. 99 ) then
              write (isf_bulletin_error,"('bad per_unc: ',f8.2)") per_unc
              write_phase_info = 20
              return
          end if
          write (file,"(f5.2,' ',$)") per_unc
      end if

c     Chars 103-105: uncertainty in station magnitude. Char 106 space.
      if (is_null(mag_unc) .eq. 1) then
          write (file,"(a,$)") '    '
      else
          if ( mag_unc .lt. 0 .or. mag_unc .gt. 9.9 ) then
              write (isf_bulletin_error,"('bad mag_unc: ',f8.2)") mag_unc
              write_phase_info = 20
              return
          end if
          write (file,"(f3.1,' ',$)") mag_unc
      end if

c     Chars 107-115: author. Char 116: space.
      numchar = partline(substr,author,0,0)
      if ( numchar .eq. 0 ) then
          write(file,"(a,$)") '          '
      else
          if ( numchar .gt. ISF_AUTHOR_LEN ) then
              write (isf_bulletin_error,"('author too long: ',a)") 
     + author
              write_phase_info = 20
              return
          end if
          if ( check_whole(author) .eq. 1 ) then
              write (isf_bulletin_error,"('bad author: ',a)") author
              write_phase_info = 20
              return
          end if
          write(file,"(a9,' ',$)") author
      end if

c     Chars 117-124: arrival ID.
      numchar = partline(substr,arrid,0,0)
      if ( numchar .eq. 0 ) then
          write (isf_bulletin_error,"('missing arrid')")
          write_phase_info = 20
          return
      end if
      if ( numchar .gt. ISF_ARRID_LEN ) then
          write (isf_bulletin_error,"('arrid too long: ',a)") arrid
          write_phase_info = 20
          return
      end if
      if ( check_whole(arrid) .eq. 1 ) then
          write (isf_bulletin_error,"('bad arrid: ',a)") arrid
          write_phase_info = 20
          return
      end if
      write(file,"(a8)") arrid

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info') .ne. 0) then
          write_phase_info = 10
          return
      end if

      write_phase_info = 0
      return
      end

c     Writes a phase measure formatted comment.
c     Writes any number of parameter=value pairs, starting new line if necessary.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_measure(file,param,value,
     +                                          error,numparam)

      integer file,numparam
      character param(*)*(*),value(*)*(*),error(*)*(*)
      include 'isf_bul.h'
      integer partline,check_prev_line_type
      integer i,len,space_left
      integer numchar_param,numchar_value,numchar_error
      character substr*(ISF_LINE_LEN)

      write (file,"(a,$)") ' (#MEASURE'
      space_left = ISF_COMM_LEN

      do i=1,numparam
          numchar_param = partline(substr,param(i),0,0)
          numchar_value = partline(substr,value(i),0,0)
          numchar_error = partline(substr,error(i),0,0)
          len = numchar_param + numchar_value + 1
          if (numchar_error .ne. 0) then
              len = len + numchar_error + 1
          end if
          if ( len .gt. ISF_COMM_LEN ) then
              write (isf_bulletin_error,"('param=value too long')")
              write_phase_measure = 20
              return
          end if

          if ( space_left .lt. len ) then
              write (file,"(')')")
              write (file,"(a,$)") ' (#MEASURE'
            space_left = ISF_COMM_LEN
          end if

          write (file,"(' ',a,$)") param(i)(1:numchar_param)
          write (file,"('=',a,$)") value(i)(1:numchar_value)
          if (numchar_error .ne. 0) then
              write (file,"('+',a,$)") error(i)(1:numchar_error)
          end if
          space_left = space_left - len
      end do
      write (file,"(')')")

c     Check that this line's type should follow the previous line's type.
      if (isf_prev_line_type(1:10) .eq. "phase_info" .or.    
     +    isf_prev_line_type(1:14) .eq. "phase_info_com") then

          if (check_prev_line_type('phase_info_com') .ne. 0) then
              write_phase_measure = 10
              return
          end if
      else
          if (check_prev_line_type('phase_com') .ne. 0) then
              write_phase_measure = 10
              return
          end if
      end if

      write_phase_measure = 0
      return
      end

c     Writes a minimum phase range line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_min(file,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      integer file
      real timeoffset,azoffset,slowoffset,ampoffset,peroffset,magoffset

      include 'isf_bul.h'
      integer check_prev_line_type,is_null

c     Chars 1-6: comment format string. Chars 7-47: spaces.
      write (file,"(a,a41,$)") ' (#MIN',' '

c     Chars 48-54: time offset. Chars 55-60: spaces.
      if (is_null(timeoffset) .eq. 1) then
          write (file,"(a13,$)") ' '
      else
          if ( timeoffset .lt. -99.999 .or. timeoffset .gt. 0 ) then
              write (isf_bulletin_error,"('bad timeoffset: ',f8.2)") 
     + timeoffset
              write_phase_min = 20
              return
          end if
          write (file,"(f7.3,a6,$)") timeoffset,' '
      end if

c     Chars 61-66: azimuth offset. Chars 67-72: spaces.
      if (is_null(azoffset) .eq. 1) then
          write (file,"(a12,$)") ' '
      else
          if ( azoffset .lt. -360 .or. azoffset .gt. 0 ) then
              write (isf_bulletin_error,"('bad azoffset: ',f8.2)") 
     + azoffset
              write_phase_min = 20
              return
          end if
          write (file,"(f6.1,a6,$)") azoffset, ' '
      end if

c     Chars 73-79: slowness offset. Chars 80-85: spaces.
      if (is_null(slowoffset) .eq. 1) then
          write (file,"(a13,$)") ' '
      else
          if ( slowoffset .lt. -9999.9 .or. slowoffset .gt. 0 ) then
              write (isf_bulletin_error,"('bad slowoffset: ',f8.2)") 
     + slowoffset
              write_phase_min = 20
              return
          end if
          write (file,"(f7.1,a6,$)") slowoffset, ' '
      end if

c     Chars 86-95: amplitude offset.
      if (is_null(ampoffset) .eq. 1) then
          write (file,"(a10,$)") ' '
      else
          if ( ampoffset .lt. -9999999.9 .or. ampoffset .gt. 0 ) then
              write (isf_bulletin_error,"('bad ampoffset: ',f8.2)") 
     + ampoffset
              write_phase_min = 20
              return
          end if
          write (file,"(f10.1,$)") ampoffset
      end if

c     Chars 96-101: period offset.
      if (is_null(peroffset) .eq. 1) then
          write (file,"(a6,$)") ' '
      else
          if ( peroffset .lt. -999.9 .or. peroffset .gt. 0 ) then
              write (isf_bulletin_error,"('bad peroffset: ',f8.2)") 
     + peroffset
              write_phase_min = 20
              return
          end if
          write (file,"(f6.1,$)") peroffset
      end if

c     Chars 102-105: magnitude offset.
      if (is_null(magoffset) .eq. 1) then
          write (file,"(a4,')')") ' '
      else
          if ( magoffset .lt. -9.9 .or. magoffset .gt. 0 ) then
              write (isf_bulletin_error,"('bad magoffset: ',f8.2)") 
     + magoffset
              write_phase_min = 20
              return
          end if
          write (file,"(f4.1,')')") magoffset
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_min = 10
          return
      end if

      write_phase_min = 0
      return
      end

c     Writes a maximum phase range line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_max(file,timeoffset,azoffset,
     +                        slowoffset,ampoffset,peroffset,magoffset)

      integer file
      real timeoffset,azoffset,slowoffset,ampoffset,peroffset,magoffset

      include 'isf_bul.h'
      integer check_prev_line_type,is_null

c     Chars 1-6: comment format string. Chars 7-47: spaces.
      write (file,"(a,a41,$)") ' (#MAX',' '

c     Chars 48-54: time offset. Chars 55-60: spaces.
      if (is_null(timeoffset) .eq. 1) then
          write (file,"(a13,$)") ' '
      else
          if ( timeoffset .lt. 0 .or. timeoffset .gt. 99.999 ) then
              write (isf_bulletin_error,"('bad timeoffset: ',f8.2)") 
     + timeoffset
              write_phase_max = 20
              return
          end if
          write (file,"(f7.3,a6,$)") timeoffset,' '
      end if

c     Chars 61-66: azimuth offset. Chars 67-72: spaces.
      if (is_null(azoffset) .eq. 1) then
          write (file,"(a12,$)") ' '
      else
          if ( azoffset .lt. 0 .or. azoffset .gt. 360 ) then
              write (isf_bulletin_error,"('bad azoffset: ',f8.2)") 
     + azoffset
              write_phase_max = 20
              return
          end if
          write (file,"(f6.1,a6,$)") azoffset, ' '
      end if

c     Chars 73-79: slowness offset. Chars 80-85: spaces.
      if (is_null(slowoffset) .eq. 1) then
          write (file,"(a13,$)") ' '
      else
          if ( slowoffset .lt. 0 .or. slowoffset .gt. 9999.9 ) then
              write (isf_bulletin_error,"('bad slowoffset: ',f8.2)") 
     + slowoffset
              write_phase_max = 20
              return
          end if
          write (file,"(f7.1,a6,$)") slowoffset, ' '
      end if

c     Chars 86-95: amplitude offset.
      if (is_null(ampoffset) .eq. 1) then
          write (file,"(a10,$)") ' '
      else
          if ( ampoffset .lt. 0 .or. ampoffset .gt. 9999999.9 ) then
              write (isf_bulletin_error,"('bad ampoffset: ',f8.2)") 
     + ampoffset
              write_phase_max = 20
              return
          end if
          write (file,"(f10.1,$)") ampoffset
      end if

c     Chars 96-101: period offset.
      if (is_null(peroffset) .eq. 1) then
          write (file,"(a6,$)") ' '
      else
          if ( peroffset .lt. 0 .or. peroffset .gt. 999.9 ) then
              write (isf_bulletin_error,"('bad peroffset: ',f8.2)") 
     + peroffset
              write_phase_max = 20
              return
          end if
          write (file,"(f6.1,$)") peroffset
      end if

c     Chars 102-105: magnitude offset.
      if (is_null(magoffset) .eq. 1) then
          write (file,"(a4,')')") ' '
      else
          if ( magoffset .lt. 0 .or. magoffset .gt. 9.9 ) then
              write (isf_bulletin_error,"('bad magoffset: ',f8.2)") 
     + magoffset
              write_phase_max = 20
              return
          end if
          write (file,"(f4.1,')')") magoffset
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_max = 10
          return
      end if

      write_phase_max = 0
      return
      end

c     Writes a phase correction line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_correc(file,timecorr,azcorr,
     +                        slowcorr,ampcorr,percorr,magcorr)

      integer file
      real timecorr,azcorr,slowcorr,ampcorr,percorr,magcorr

      include 'isf_bul.h'
      integer check_prev_line_type,is_null

c     Chars 1-8: comment format string. Chars 9-47: spaces.
      write (file,"(a,a39,$)") ' (#COREC',' '

c     Chars 48-54: time correction. Chars 55-60: spaces.
      if (is_null(timecorr) .eq. 1) then
          write (file,"(a13,$)") ' '
      else
          if ( timecorr .lt. -99.999 .or. timecorr .gt. 99.999 ) then
              write (isf_bulletin_error,"('bad timecorr: ',f8.2)") 
     + timecorr
              write_phase_correc = 20
              return
          end if
          write (file,"(f7.3,a6,$)") timecorr,' '
      end if

c     Chars 61-66: azimuth correction. Chars 67-72: spaces.
      if (is_null(azcorr) .eq. 1) then
          write (file,"(a12,$)") ' '
      else
          if ( azcorr .lt. -360 .or. azcorr .gt. 360 ) then
              write (isf_bulletin_error,"('bad azcorr: ',f8.2)") azcorr
              write_phase_correc = 20
              return
          end if
          write (file,"(f6.1,a6,$)") azcorr, ' '
      end if

c     Chars 73-79: slowness corr. Chars 80-85: spaces.
      if (is_null(slowcorr) .eq. 1) then
          write (file,"(a13,$)") ' '
      else
          if ( slowcorr .lt. -9999.9 .or. slowcorr .gt. 9999.9 ) then
              write (isf_bulletin_error,"('bad slowcorr: ',f8.2)") 
     + slowcorr
              write_phase_correc = 20
              return
          end if
          write (file,"(f7.1,a6,$)") slowcorr, ' '
      end if

c     Chars 86-95: amplitude corr.
      if (is_null(ampcorr) .eq. 1) then
          write (file,"(a10,$)") ' '
      else
          if ( ampcorr .lt. -9999999.9 .or. ampcorr .gt. 9999999.9 ) 
     + then
              write (isf_bulletin_error,"('bad ampcorr: ',f8.2)") ampcorr
              write_phase_correc = 20
              return
          end if
          write (file,"(f10.1,$)") ampcorr
      end if

c     Chars 96-101: period corr.
      if (is_null(percorr) .eq. 1) then
          write (file,"(a6,$)") ' '
      else
          if ( percorr .lt. -999.9 .or. percorr .gt. 999.9 ) then
              write (isf_bulletin_error,"('bad percorr: ',f8.2)") percorr
              write_phase_correc = 20
              return
          end if
          write (file,"(f6.1,$)") percorr
      end if

c     Chars 102-106: magnitude correction.
      if (is_null(magcorr) .eq. 1) then
          write (file,"(a5,')')") ' '
      else
          if ( magcorr .lt. -9.99 .or. magcorr .gt. 9.99 ) then
              write (isf_bulletin_error,"('bad magcorr: ',f8.2)") magcorr
              write_phase_correc = 20
              return
          end if
          write (file,"(f5.2,')')") magcorr
      end if

      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_correc = 10
          return
      end if

      write_phase_correc = 0
      return
      end

c     Writes an original phase data line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_phase_original(file,chan,sta,yyyy,
     +                  mm,dd,hh,mi,ss,msec,azim,slow,amp,per,mag)

      integer file
      character chan*(*),sta*(*)
      real azim,slow,amp,per,mag
      integer yyyy,mm,dd,hh,mi,ss,msec

      include 'isf_bul.h'
      integer check_prev_line_type,is_null,check_whole,partline
      character substr*(ISF_LINE_LEN)
      integer numchar

c     Chars 1-10: comment format string.
      write (file,"(a,$)") ' (#ORIG   '

c     Chars 11-13: original channel. Char 14: space.
      numchar = partline(substr,chan,0,0)
      if ( numchar .eq. 0 ) then
          write(file,"('    ',$)")
      end if
      if ( numchar .gt. ISF_CHAN_LEN ) then
          write (isf_bulletin_error,"('chan too long: ',a)") chan
          write_phase_original = 20
          return
      end if
      if ( check_whole(chan) .eq. 1 ) then
          write (isf_bulletin_error,"('bad chan: ',a)") chan
          write_phase_original = 20
          return
      end if
      write(file,"(a3,' ',$)") chan

c     Chars 15-19: original station code. Char 20-37: space.
c     Format gives 8 chars for sta but sta can only be 5 chars long in IMS.
      numchar = partline(substr,sta,0,0)
      if ( numchar .eq. 0 ) then
          write(file,"(a23,$)") ' '
      end if
      if ( numchar .gt. ISF_STA_LEN ) then
          write (isf_bulletin_error,"('sta too long: ',a)") sta
          write_phase_original = 20
          return
      end if
      if ( check_whole(sta) .eq. 1 ) then
          write (isf_bulletin_error,"('bad sta: ',a)") sta
          write_phase_original = 20
          return
      end if
      write(file,"(a5,a18,$)") sta,' '

c     Chars 38-60: arrival date and time.
c     Chars 38-47: date. Char 48: space.
      if (is_null(real(yyyy)) .eq. 1 .and. is_null(real(mm)) .eq. 1 
     + .and. is_null(real(dd)) .eq. 1) then

          write(file,"(a11,$)") ' '
      else
          if (is_null(real(yyyy)) .eq. 1) then
              isf_bulletin_error = 'date but no year'
              write_phase_original = 20
              return
          end if

          if (yyyy .lt. 0 .or. yyyy .gt. 9999) then
              write (isf_bulletin_error,"('bad year ',i5)") yyyy
              write_phase_original = 20
              return
          end if

          if (is_null(real(mm)) .eq. 1) then
              isf_bulletin_error = 'year but no month'
              write_phase_original = 20
              return
          end if

          if (mm .lt. 0 .or. mm .gt. 12) then
              write (isf_bulletin_error,"('bad month  ',i5)") mm
              write_phase_original = 20
              return
          end if

          if (is_null(real(dd)) .eq. 1) then
              isf_bulletin_error = 'year but no day'
              write_phase_original = 20
              return
          end if

          if (dd .lt. 0 .or. dd .gt. 31) then
              write (isf_bulletin_error,"('bad day',i5)") dd
              write_phase_original = 20
              return
          end if
          write (file,"(i4.4,'/',i2.2,'/',i2.2,' ',$)") yyyy,mm,dd
      end if

c     Chars 49-60: time. Char 61: space.
      if (is_null(real(hh)) .eq. 1 .and. is_null(real(mi)) .eq. 1 .and.
     +    is_null(real(ss)) .eq. 1) then

          write(file,"(a13,$)") ' '
      else
          if (is_null(real(hh)) .eq. 1) then
              isf_bulletin_error = 'missing hour'
              write_phase_original = 20
              return
          end if

          if (hh .lt. 0 .or. hh .gt. 23) then
              write (isf_bulletin_error,"('bad hour ',i5)") hh
              write_phase_original = 20
              return
          end if

          if (is_null(real(mi)) .eq. 1) then
              isf_bulletin_error = 'missing minute'
              write_phase_original = 20
              return
          end if

          if (mi .lt. 0 .or. mi .gt. 59) then
              write (isf_bulletin_error,"('bad minute  ',i5)") mi
              write_phase_original = 20
              return
          end if

          if (is_null(real(ss)) .eq. 1) then
              isf_bulletin_error = 'missing second'
              write_phase_original = 20
              return
          end if

          if (ss .lt. 0 .or. ss .gt. 59) then
              write (isf_bulletin_error,"('bad second ',i5)") ss
              write_phase_original = 20
              return
          end if
          write (file,"(i2.2,':',i2.2,':',i2.2,$)") hh,mi,ss

c     	Chars 57-60 msec - put blanks here if no msec provided.
          if (is_null(real(msec)) .eq. 1) then
              write(file,"('     ',$)")
          else
              if (msec .lt. 0 .or. msec .gt. 999) then
                  write (isf_bulletin_error,"('bad msec ',i5)") msec
                  write_phase_original = 20
                  return
              end if
              write(file,"('.',i3.3,' ',$)") msec
          end if
      end if

c     Chars 62-66: original azimuth. Chars 67-73: spaces.
      if (is_null(azim) .eq. 1) then
          write (file,"(a12,$)") ' '
      else
          if ( azim .lt. 0 .or. azim .gt. 360 ) then
              write (isf_bulletin_error,"('bad azim: ',f8.2)") azim
              write_phase_original = 20
              return
          end if
          write (file,"(f5.1,a7,$)") azim, ' '
      end if

c     Chars 74-79: original slowness. Chars 80-86: spaces.
      if (is_null(slow) .eq. 1) then
          write (file,"(a13,$)") ' '
      else
          if ( slow .lt. -9999.9 .or. slow .gt. 9999.9 ) then
              write (isf_bulletin_error,"('bad slow: ',f8.2)") slow
              write_phase_original = 20
              return
          end if
          write (file,"(f6.1,a7,$)") slow, ' '
      end if

c     Chars 87-95: original amplitude.  Char 96: space.
      if (is_null(amp) .eq. 1) then
          write (file,"(a10,$)") ' '
      else
          if ( amp .lt. 0 .or. amp .gt. 9999999.9 ) then
              write (isf_bulletin_error,"('bad amp: ',f8.2)") amp
              write_phase_original = 20
              return
          end if
          write (file,"(f9.1,' ',$)") amp
      end if

c     Chars 97-101: original period. Char 102: space.
      if (is_null(per) .eq. 1) then
          write (file,"(a6,$)") ' '
      else
          if ( per .lt. 0 .or. per .gt. 99.99 ) then
              write (isf_bulletin_error,"('bad per: ',f8.2)") per
              write_phase_original = 20
              return
          end if
          write (file,"(f5.2,' ',$)") per
      end if

c     Chars 103-105: original station magnitude.
      if (is_null(mag) .eq. 1) then
          write (file,"('   )')")
      else
          if ( mag .lt. 0 .or. mag .gt. 9.99 ) then
              write (isf_bulletin_error,"('bad mag: ',f8.2)") mag
              write_phase_original = 20
              return
          end if
          write (file,"(f3.1,')')") mag
      end if

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('phase_info_com') .ne. 0) then
          write_phase_original = 10
          return
      end if

      write_phase_original = 0
      return
      end


c     Writes a plain IMS comment.
c     Takes string as its argument, cuts it into lines of less than the maximium
c     allowed length and adds brackets to the start and end of each line.

c     Returns 0 on a successful write.  
c     Returns 10 if this line should not follow the previous line written.
c     Returns 20 if any of the variables passed are unsuitable for writing.
c     On error writes a diagnostic to isf_bulletin_error.

      integer function write_comment(file,comment)

      integer file
      character comment*(*)

      include 'isf_bul.h'
      integer check_prev_line_type,partline
      character substr*(ISF_LINE_LEN)
      integer numchar,i
      character word_break

      numchar = partline(substr,comment,0,0)
      do while (numchar .gt. ISF_LINE_LEN-3)
          i=ISF_LINE_LEN-3
          do while (word_break .ne. ' ')
              word_break = substr(i:i)
              i = i-1
          end do

          write (file,"(' (',a,')')") substr(1:i)
          substr = substr(i+1:)
          numchar = numchar - i
      end do
      write (file,"(' (',a,')')") substr(1:numchar)

c     Check that this line's type should follow the previous line's type.
c     line_type depends on previous line_type.
      if (isf_prev_line_type(1:6) .eq. "origin" .or.    
     +    isf_prev_line_type(1:4) .eq. "axes" .or.
     +    isf_prev_line_type(1:8) .eq. "axes_err" .or.
     +    isf_prev_line_type(1:11) .eq. "fault_plane" .or.
     +    isf_prev_line_type(1:6) .eq. "momten" .or.
     +    isf_prev_line_type(1:10) .eq. "origin_com") then

          if (check_prev_line_type('origin_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:6) .eq. "netmag" .or.    
     +    isf_prev_line_type(1:10) .eq. "netmag_com") then

          if (check_prev_line_type('netmag_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:7) .eq. "effects" .or.    
     +    isf_prev_line_type(1:11) .eq. "effects_com") then

          if (check_prev_line_type('effects_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:10) .eq. "phase_info" .or.    
     +    isf_prev_line_type(1:14) .eq. "phase_info_com") then

          if (check_prev_line_type('phase_info_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else if (isf_prev_line_type(1:5) .eq. "phase" .or.    
     +    isf_prev_line_type(1:9) .eq. "phase_com") then

          if (check_prev_line_type('phase_com') .ne. 0) then
              write_comment = 10
              return
          end if
      else
          if (check_prev_line_type('comment') .ne. 0) then
              write_comment = 10
              return
          end if
      end if

      write_comment = 0
      return
      end


c     Writes STOP line with a preceding blank line.

      integer function write_stop(file)

      integer file
      integer check_prev_line_type

      write (file,"()")
      write (file,"(a)") 'STOP'

c     Check that this line's type should follow the previous line's type.
      if (check_prev_line_type('stop') .ne. 0) then
          write_stop = 10
          return
      end if

      write_stop = 0
      return
      end
