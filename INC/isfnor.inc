c
c isf read routines
c
      integer read_origin_head,read_origin,read_fault_plane_head,
     &   read_axes_head,read_axes_err_head,read_netmag_head,
     &   read_effects_head,read_phase_head,read_phase_info_head,
     &   read_effects,read_netmag,read_fault_plane,read_phase,
     &   read_phase_info,read_event_id,read_phase_measure,
     &   read_origin_prime,read_origin_nohyp
 
c
c isf write routines
c
      integer write_event_type,write_origin,write_origin_prime,
     &   write_origin_head,write_data_type,write_event_id,
     &   write_fault_plane,write_fault_plane_head,
     &   write_netmag,write_netmag_head,write_phase,
     &   write_phase_head,write_effects_head,write_effects,
     &   write_phase_info,write_phase_info_head,write_comment,
     &   write_phase_measure,write_origin_nohyp
      character line*(ISF_LINE_LEN*2),region*(ISF_REGION_LEN)
c
c ISF variables
c
      real sdobs,smaj,smin,sdepth,maxdist,mindist,stime,rstrike
      integer strike,ndef
      character author*(ISF_AUTHOR_LEN),error*(80),
     &    param*(80)
      character*1 loctype,antype,epifix,depfix,timfix,
     &    anytype
      integer yyyy,mm,dd,hh,mi,ss,msec,gap,nsta,numparam,doy
      real lat,lon,depth
      double precision dpsec
      character origid*(ISF_ORIGID_LEN),evid*(ISF_EVID_LEN)
      character*(ISF_ORIGID_LEN) hyp_origid(100)
      real fpstrike,fpdip,fprake
      character*(ISF_ARRID_LEN) rea_arrid(max_data)
      character sta*(ISF_STA_LEN),arrid*(ISF_ARRID_LEN),
     &    phase*(ISF_PHASE_LEN),magtype*(ISF_MAGTYPE_LEN),
     &    etype*(ISF_ETYPE_LEN),
     &    chan*(ISF_CHAN_LEN),comment*(ISF_COMM_LEN),value*(10),
     &    f_plane,f_type
      integer np,ns
      character*(1) timedef,azimdef,slowdef,
     &    sp_fm,detchar,magind,picktype
      real dist,esaz,timeres,azim,azimres,slow,slowres,amp,per,mag,snr,
     &     magerr
      character scale*(ISF_I_SCALE_LEN),mloctype*(6)
      character felt,casualties,damage,uplift,subsidence,fault,tsunami,
     &  heard
      character seiche,liquification,geyser,filter
      character landslide,sandblow,cracks,lights,modifier
      real mlat,mlon,intensity1,intensity2
      character country*(ISF_COUNTRY_LEN),postcode*(ISF_POSTCODE_LEN),
     &     net*(ISF_NET_LEN)
      character volcano,acoustic,gravity,t_wave,odours
      real filter_min,filter_max,time_unc,time_weight,
     &   azim_unc,azim_weight
      real slow_unc,slow_weight,amp_unc,per_unc,mag_unc
      character*2 mag_type

