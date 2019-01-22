
      subroutine spec_dist
     *(spec_phase,edistance,edepth,factor)
c
c   calculate equivalent geometrical spreading distance
c
c
c     input : spec_phase,Phase type (P or S),                             
c             edistance, epicentral distance
c             edepth,hypocentral depth,
c             geo_depth1, geo_depth2 :depth range for type of 
c                                     geometrical spreading,from common 
c             herkij_distance: distance where spreading goes from
c                              body to surface wave
c     output: the geometrical spreading factor to mulitply with
c
c     If no phase type, S is assumed
c     The geometrical spreading changes from Herrmann-Kijko or body
c     wave spreading in the depth range geo_depth1 to geo_depth2, see
c     comment below.
c
c   updates:
c
c   mar 5 01 jh : allow distance to be less than 1.0
c   oct 12 07 jh: add teleseismic spreading
c
c     
      implicit none
      include 'seisan.inc'
      character*1 s_phase,spec_phase ! phase ids
      real factor
      real edepth,edistance,sdistance
c
c   check if distance larger than 30 deg, then use teleseismic approximation to spreading
c
      if(edistance.ge.30.0*111.2) then
         factor=0.0048/(30.0+(edistance/111.2))
         return
      endif
         

      sdistance=sqrt(edepth*edepth+edistance*edistance)
      factor=1.0
c
c   check phase type
c
      s_phase=spec_phase
      if(spec_phase.ne.'S'.and.spec_phase.ne.'P') s_phase ='S'

c
c   check that hypocentral distance is not zero
c
      if(sdistance.lt.0.00001) sdistance=0.00001
c
c   check depth limits
c
      if(geo_depth2.le.geo_depth1) then
         write(6,*)' Wrong limits for geo_depth:', 
     *   geo_depth1,geo_depth2
         write(6,*)' Return to stop'
         read(5,'(a)') s_phase
         stop
      endif
c
c   if a P wave, geometrical spreading is always 1/hypocentral distance
c
      if(s_phase.eq.'P') then
        factor=1.0/sdistance
        return
      endif
c
c   if a S-phase, geometrical spreading is 1/hypocentral distance if
c   epicentral distance is less than herkij_distance km or depth larger than 
c   geo_depth2
c
      if((edistance.lt.herkij_distance.and.s_phase.eq.'S').or.
     *   edepth.ge.geo_depth2) then
        factor=1.0/sdistance
        return
      endif
c
c   if a S-phase, epicentral distance larger than 100 km and depth
c   is less than geo_depth1, then Herrmann-Kijko attenuation is used
c
      if(edistance.ge.herkij_distance.and.s_phase.eq.'S'.
     *and.edepth.lt.geo_depth1)
     *then
         factor=1.0/sqrt(herkij_distance*edistance)
         return
      endif      
c
c   if a S-phase, epicentral distance larger than herkij_distance km and depth
c   between geo_depth1 and geo_depth2), an interpolation between
c   Herrmann-Kijko and body wave spreading is used.
c
       if(edistance.ge.herkij_distance.and.s_phase.eq.'S'.and.
     *    edepth.ge.geo_depth1.and.edepth.lt.geo_depth2) then
         factor=(1.0-((edepth-geo_depth1)/(geo_depth2-geo_depth1)))
     *          *sqrt(edistance*herkij_distance)+
     *          ((edepth-geo_depth1)/(geo_depth2-geo_depth1))*
     *          sdistance
         factor=1.0/factor
       endif
c
       return

       end
