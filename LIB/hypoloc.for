cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

c

c                 HYPOLOC version 4.0 6/98

c

c         Locates an earthquake from a set of phase data 

c         using adaptively damped least-squares.

c

c         Uses calls to function 'rmsv' which evaluates

c         the rms, travel times and derivatives at any point. 

c         rmsv is also used by subroutine 'startloc', to find the

c         best starting location.

c

c

c         Author:           Barry R. Lienert

c                           Hawaii Institute of Geophysics,

c                           University of Hawaii at Manoa,

c                           2525 Correa Rd., Honolulu HI 96822

c

c Changes:

c

c  May 27, 92  BRL  :

c         Now use Bratt's routine, distaz, instead of delaz.

c         delaz was calculating delta using local radius

c         whereas I want it on the geocentric sphere (R=6378km)

c         Fixed test (56) logic to disable starting location

c         test(57) no longer used for this

c

c  Jun 9, 1992, BRL:

c         Added fixepi to parameter list in window

c         Startloc was still being called when fixepi='S'

c

c  Nov, 1992, BRL

c         If no convergence, try with alternate epicenter

c

c  Oct, 1993, BRL

c         Put travel time calculation in subroutine trtime

c         Used this subroutine to initially test arrivals for

c         bad times (e.g., 1 hour off) by comparing arrival

c         time differences between station pairs with travel

c         times between them.

c         Added PARAMETER statement for dimensions

c         Added check for Moho specification

c         added fixdep  to input window

c         changed depth origin to sea level if test(40)=0.0

c

c 10/93   added fixdep, data, parm & isort to window  

c

c 4/94    put common/comm1 in 'comm1.inc' 

c         added nconrad, added origin time fix if fixor='F'

c may 19 1994 by jh: put in icd=6 and return in all non locatable cases

c sep 94  BRL moved consistency test into subroutine ttcheck

c oct 94  BRL added oterr (origin time error) and nzcount to window

CJAB(BGS)Jan95 : Installed error encoding...

c feb 95  BRL added ires & fixor to rmsv window

C sep 97  mario azimuth changees

c nov 27 97 by jh : count correctly # of stations if not in order

c Jan 98  mv:   now calculates correctly the azimuth from event

c            to station and a new additional variable baz0(narriv)

c            is used to calculate the backazimuth.--->call hypoloc(...

c Jun 98  brl: incorporated changes from my version (spher. harm. sta. corrs, etc)

c              Added section to locate using single stat. azimuth & appar. vel.

C              Added appar_veloc(narriv) to window

c sep 98 jh  : ---------- seisan verison 7.0 changes ----------------------

c              year 2000, 5 char station names

c

c oct 28 1998 bmt: linux changed, save option and (* vs 1) include

c nov 179  99 jh : remove v,vs,d,nl and parm form windows, in common block

c dec 10      jh : fix year 2000 printout

c jan-feb 2000   : fix for multiple models etc

c may 2008    bl : fix problem with error calculation

c oct 28 2012 jh : do not initilize ips here since in case of multi depths,

c                  the indcator isp for s-p should be unchanged
c may 04 2013 bl : fix overflow problem
c jun 25 2013 jh : wrong ain in some special cases with incresing rms with 
c                  free depth
c mar 12 2016 jh : add error messages for se, add nmessage and message
c jan 25 2017 jh : make sure the correction to epicenter is not VERY large
c                  variable ddd



cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

c

      subroutine hypoloc(nphase,st,maxelv,phase,tp,dtwt,ip,x0,y0,

     &iselv,dl,ires,xnear,xfar,xh0,tph0,nmoho,nconrad,test,

     &iulst,xh1,tph,std1,dtwt1,tpc,im,dt,dlt,az0,aninc,ips,rms1,

     &xk1,ndata,icd,fixepi,fixdep,fixor,dtw1,distind,trphs,vapp,azapp,

     &alatm,alonm,data,isort,di,oterr,nzcount,baz0,scorr,

     &appar_veloc,nmessage,message)

c

cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

c

c                          INPUTS

c  Variable  Type

c

c  nphase    integer        Number of phases counted separately 

c                            (e.g., p, s & azimuth are each 1 phase)

c  st(i)     character*5    Station IDs

c  maxelv    integer        maximum elevation of ALL stations (not just

c                            the ones in the phase array). Needed

c                            because the velocity model is referenced to this.

c  phase(i)  character*8    Phase identifiers (e.g., PN, SG, etc)

c  tp(i)     SP real        Arrival times measured in seconds from 1/1/1900

c  dtwt(i)   SP real        Input phase weights

c  ip(i)     integer        Integer HYPO71 style weights: set P phase ip(i) 

c                            to 9 to force  S-P, L-S differences for 

c                            subsequent phases at this station.

c  x0(i)     SP real        Station longitudes in geocentric radians

c  y0(i)     SP real          "     latitudes       "

c  iselv(i)   integer          "     elevations in meters for each phase

c  dl(i)     SP real        Station corrections in seconds  "        "

c   i=1..nphase              NOTE: station data must be matched to each phase, 

c                            i.e., each phase  needs its own station info.

c

c  ires      integer     Hypocenter fixing index:

c                             1    -    Fix depth to xh0(3)

c                             2    -    Fix origin time to tph0

c                             7    -    Fix hypocenter at xh0

c                             8    -    Fix epicenter at xh0(1),xh0(2)

c                             9    -    Fix hypocenter and origin time

c

c  xnear     SP real      Distance in km from epicenter where distance

c                         weighting is 1

c  xfar      SP real      Distance in km from epicenter beyond which

c                         distance weighting is 0

c  xh0(j)    SP real      Starting hypocenter (long., lat., depth)

c  j=1,2,3

c  tph0      DP real      Starting origin time in seconds from 1/1/1900

c  nl        integer      Number of layers in velocity model

c  v(k)      SP real      P velocities in km/sec

c  vs(k)     SP real      S velocities     "

c  d(k)      SP real      depths to layer boundaries in km

c  k=1..nl 

c                          (e.g., d(1) is depth to bottom of 1st layer)

c  nmoho     integer      Layer number having moho velocity

c  nconrad   integer       "             "    Pb     "

c

c  test(l)   SP real      parameters controlling weighting, damping,

c  l=1..ntest

c                          convergence, etc. See subroutine "settest"

c                          for definitions of these.

c  iulst                  Listing unit# - if < 0, no listing

c  fixepi    character*1  fix epicenter at xh0(1),xh0(2) if ='F',

c                         use as start location if ='S' 

c  fixdep    character*1  fix depth "             "                   

c  fixor     character*1  fix origin time

c

c****************************************************************************

c

c                          OUTPUTS

c

c  xh1(j)    SP real      Final hypocenter solution in geocentric radians

c                         (j=1,2,3:long., lat., depth)

c  tph       DP real      Origin time in seconds from 1/1/1900

c  std1(j)   SP real      Final solution standard deviations in km

c  dtwt1(i)  SP real      Final residual weights 

c  dtw1(i)   SP real      Residual weights determined by trtime 

c                         (include distance weighting, residual 

c                         weighting, etc). Set to -1 if no valid travel time.

c  tpc(i)    SP real      Calculated travel times in seconds for solution

c  im        integer      Index of first arrival

c  dt(i)     SP real      arrival time residuals in seconds (i=1,nphase)

c                         for final solution (unweighted)

c  dlt(i)    SP real      Distances of final epicenter from station in km

c  az0(i)    SP real      Azimuths from earthquake to station (measured

c                          clockwise from North in degrees)

c  aninc(i)  SP real      Angles of incidence of rays leaving earthqake

c                          measured up from the (downward) vertical,

c                         i.e., a refraction is < 90, a direct is > 90 deg  

c  ips(i)    integer      1 for S-P or L-P  difference, 0 otherwise

c  trphs(i)  character*8  phase used for travel time calculation

c  rms1      SP real      Weighted root mean square residual in seconds

c  xk1       SP real      Last value of damping used for solution

c  ndata     integer      Number of phases used in final solution

c  icd       integer      Convergence of solution:    

c                          2    # increases in damping > test(37)

c                          3    hypocentral change < test(32)

c                          5    test(11) iterations reached

c                          6    less than 3 valid phases (no solution)

c                          7    more than 20 attempts to make depth<0

c                               or to exceed the parameter change limit

c******************************************************************************

       save

       include 'hypparm.inc'

c      include 'comm1.inc'

c      include 'param.inc'

c

      include 'libsei.inc'                    ! Library definitons.

      external sei code                       ! Error encoder.

      integer  code,                          ! Condition.

     &         iulst                          ! Output diagnostics.

      logical  b_flag                         ! Flag it?.

      logical tst                             ! for station test

c





      double precision tsmin,tphh,tph1,tph2,tph3

      character*5 statchk

      character*4 dr1,dr2,thrmin
      integer nmessage
      character*80 message(50)

      character*1 prm2,ins1,iew1,ucase,fd,fo,fixepi,

     &fixdep

c

c    Initialise...

c    =============

c   

      code = e_ok$                               ! Initialise condition.

c

      pi=3.141593

      degtorad=pi/180.

      rearth=6371.                         



c save starting depth, lon and lat

      startdep=xh0(3)



c removed 4/92: starting depth is always relative to assumed origin, 

c i.e., sea level if test(40)=0 or maxelv if test(40)>0

c      if(test(40).eq.0.0)startdep=xh0(3)+float(maxelv)*.001

      startlon=xh0(1)

      startlat=xh0(2)

c      write(iulst,*,iostat=code)startlon/degtorad,startlat/degtorad

      call sei code( fort$, code, iulst, b_flag ) ! Process outcome.



c 10/93: added minflag as test parameter

      minflag=int(test(63))      

      

c 4/94: added new parameter depthlim which is limited to moho and conrad

c       depths for layered model (returned by rmsv & trtime)



      depthlim=test(70)



c-----------------------------------------------------------------

c change 10/93: check for non-locatable cases

c 4/94: disable for fixed hypocenter

      if(ires.lt.7)then

       naz=0

       nvp=0

       nappv=0  !added 6/98            



c find # diff stats: added 6/98

       ndiffstat=0

       statchk='     '

       do i=1,nphase

        if(st(i).ne.statchk.and.dtwt(i).gt.0.0)then

         statchk=st(i)

         ndiffstat=ndiffstat+1

        endif

       enddo  



       statchk='     '

c

c   count no of azimuths naz and other phases nvp

c

       do i=1,nphase

c         call distaz(y0(i)/degtorad,x0(i)/degtorad,

c     &   xh0(2)/degtorad,xh0(1)/degtorad,dedeg,azz,azb)

c         dlt(i)=dedeg*degtorad*rearth

c         write(iulst,*,iostat=code)st(i),dlt(i)

         call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

c change 10/94: added statchk test to make sure azimuths are from different stations

         if(phase(i)(1:2).eq.'AZ'.and.dtwt(i).gt.0.0.and.st(i).ne.

     &    statchk)then

           statchk=st(i)

           naz=naz+1       

           if(appar_veloc(i).gt.0.0)nappv=nappv+1

         elseif(dtwt(i).gt.0.0)then

           nvp=nvp+1

         endif

       end do

c       write(6,*)'nz,nv',naz,nvp

c   

c   case of 2 phases-no azimuthts, or , one phase and one azimuth

c

       if((nvp.eq.2.and.naz.eq.0).or.((nappv.eq.0.or.distind.ne.'D')

     &  .and.nvp.eq.1.and.naz.le.1))then

         if(iulst.gt.0.and.(.not.multi_model))then

           if(dataprev.ne.data(1))then

c             write(iulst,*,iostat=code)data(1)(1:78)

              call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

           endif

           write(iulst,*,iostat=code)

     &     ' less than 2 phases and no azimuths:',

     &     ' not locatable'

           call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

           write(*,*)' less than 2 phases and no azimuths:',

     &     ' not locatable'

           icd=6
           nmessage=nmessage+1
           message(nmessage)=
     *     ' less than 2 phases and no azimuth, not locatable'

           return

         endif

         if(dataprev.ne.data(1))then

c           write(*,*)data(1)(1:78)

           dataprev=data(1)

         endif

         write(*,*)'less than 2 phases and no azimuths:',

     &   ' not locatable'
           nmessage=nmessage+1
           message(nmessage)=
     *    ' less than 2 phases and no azimuth, not locatable'
         icd=6

         return

       endif

c

c   case of one azimuth and one phase and no apparent velocities

c

       if(nvp.lt.2.and.naz.eq.1.and.(nappv.eq.0.or.distind.ne.'D'))then

         if(iulst.gt.0)then

           if(dataprev.ne.data(1))then

c             write(iulst,*,iostat=code)data(1)(1:78)

              call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

           endif

           write(iulst,*,iostat=code)

     &     ' less than 2 phases and 1 azimuth:',

     &     ' not locatable'

           call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

           write(*,*)' less than 2 phases and 1 azimuth:',

     &     ' not locatable'

         icd=6
           nmessage=nmessage+1
           message(nmessage)=
     *    ' less than 2 phases and no azimuth, not locatable'
         return

         endif

         if(dataprev.ne.data(1))then

c           write(*,*)data(1)(1:78)

           dataprev=data(1)

         endif

         write(*,*)'less than 2 phases and 1 azimuth:',

     &   ' not locatable'

         icd=6
           nmessage=nmessage+1
           message(nmessage)=
     *     ' less than 2 phases and no azimuth, not locatable'

         return

       endif

c

c   case of no azimuths

c

       if(naz.eq.0)then

         statchk=st(1)

         k=1

c

c   jh change nov 97, count correctly # stations, also if not in order

c

         do i=2,nphase

           tst=.false.  ! initially not same station

           do l=1,i-1

             if(st(i).eq.st(l).and.dtwt(l).ne.0.0) tst=.true.   ! already counted

           enddo

           if(dtwt(i).gt.0.0.and..not.tst) k=k+1

c           write(6,*) k,st(i),dtwt(i)

           if(k.eq.3)go to 7000  !  3 stations now

         end do

c        

         if(iulst.gt.0.and.(.not.multi_model))then

           if(dataprev.ne.data(1))then

c             write(iulst,*,iostat=code)data(1)(1:78)

              call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

           endif

           write(iulst,*,iostat=code)' not locatable: data needed at'

     &     ,' three different stations if no azimuths'

           call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

           write(*,*)' not locatable: data needed at'

     &     ,' three different stations if no azimuths'

         icd=6
         nmessage=nmessage+1
         message(nmessage)=' not locatable: data needed at'//
     &   ' three different stations if no azimuths'

         return

         endif

         if(dataprev.ne.data(1))then

c           write(*,*)data(1)(1:78)

           dataprev=data(1)

         endif

         write(*,*)' not locatable: data needed at'

     &   ,' three different stations if no azimuths'

         icd=6
         nmessage=nmessage+1
         message(nmessage)=' not locatable: data needed at'//
     &   ' three different stations if no azimuths'

         return

       endif

      endif

      

7000  continue

c-----------------------------------------------------------------------------



c    set L velocity to test(51)

      vl=test(51)



c layer thicknesses

      do i=1,nl-1

        parm(nl+i)=d(i+1)-d(i)



c need to add maxelv to 1st layer thickness if test(40)=0

        if(i.eq.1.and.test(40).eq.0.0)parm(nl+i)=d(i+1)-d(i)+

     &  float(maxelv)*.001



      end do



      nn=2*nl-1

c--------------------------------------------------------------

c change 10/93, BRL

c find travel time order and store in new variable, isort(i) 

c      call r4sort(nphase,dlt,isort)

      call r8sort(nphase,tp,isort)



c change 10/93:  set ips(i) before checking for bad times



      do i=1,nphase



c 5/94: use dt2<0 to mark weighted out arrivals

        dt2(i)=0.0

c jh oct 28 2012        ips(i)=0

      end do



c 19/94: removed loop setting ips(i)=1 for differences



c      write(iulst,*,iostat=code)test(60)

      call sei code( fort$, code, iulst, b_flag ) ! Process outcome.



c this is a call to initialize rmsv & set ips(i)

       dtav=0.0 

       tph=0.0     !added 6/98

       rms=rmsv(nphase,st,phase,tp,x0,y0,xhh0,ndata,nmoho,

     & nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc1,

     & dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc1,

     & trphs,dtav,0,data,1,depthlim,iflagd,ires,fixor,scorr)



      if(test(60).ne.0.0.and.(.not.multi_model))then

       write(iulst,'(/,'' Consistency check performed'')',

     &iostat=code)          

       call sei code( fort$, code, iulst, b_flag ) ! Process outcome.



c change 9/94: moved consistency check into subroutine ttcheck

       call ttcheck(nphase,isort,dtwt,ips,phase,st,iulst,dataprev,

     & data,xh,x0,y0,iselv,maxelv,dt2,test,tp,nmoho,nconrad,tpc1,

     & xnear,xfar,ip,dt,tph,aninc,trphs,distind,depthlim,iflagd,

     & minflag1,imatch,statchk,ndata,dtw1,dl,tt,xh1,

     & dtdl,dtdh,dddp,phcd,scorr)

      endif



c--------------------------------------------------------------------

c    find the first non-zero weight arrival and if there are any 'N' phases 

      tsmin=1.0d30

      mohoflag=0                                  

      conradflag=0

      nzcount=0



      do i=1,nphase

c------------------------------------------------------------------

c change 10/93, BRL

c use variable nzcount to count the number of valid absolute times

c 10/94; changed dtwt(i) to dtw1(i)

c          write(iulst,*,iostat=code)st(i),phase(i),ips(i),dtw1(i)

          call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

        if(phase(i)(1:2).ne.'AZ'.and.ips(i).eq.0.and.ip(i).ne.9

     &  .and.dtw1(i).ge.0.0.and.dtwt(i).gt.0)then

          nzcount=nzcount+1

        endif

c------------------------------------------------------------------

        prm2=ucase(phase(i)(2:2))

c-------------------------------------------------------------

c change 10/93 BRL added dtwt(i).gt.0.0 to next two statements

c to prevent zero-weight and PS phases being used

        if(prm2.eq.'N'.and.dtwt(i).gt.0.0)mohoflag=1

        if(prm2.eq.'B'.and.dtwt(i).gt.0.0)conradflag=1

        if(dtwt(i).gt.0.0.and.tp(i).lt.tsmin.and.

     &    phase(i)(1:2).ne.'AZ'.and.ip(i).ne.9.and.ips(i).ne.1)then

c-------------------------------------------------------------

          im=i

          tsmin=tp(i)

        endif

      end do



c    print the first arrival 

      if(nzcount.gt.0)then

        call sectim(tsmin,iyear,jday,imonth,iday,ihr,imin,sec)

        if(iulst.gt.0.and.(.not.multi_model))write(iulst,1000

     &                      ,iostat=code)st(im),iyear,imonth,

     &  iday,ihr,imin,sec

        call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

c 2000 1000    format(/,3x,'First arrival: ',a4,1x,3i2,2x,2i3,f6.2,/)

1000    format(/,3x,'First arrival: ',a4,1x,i4,1x,2i2,2x,2i3,f6.2,/)



      else

        iyear=0

        if(iulst.gt.0)then

          write(iulst,'(/''********** No absolute times: Origin time'',

     &    '' not calculated **********''/)'

     &                ,iostat=code)

        call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

        endif

      endif



c    start with origin time at either the fixed value or the 1st arrival time

c  changed 6/98,BRL      

      if(ires.eq.2.or.ucase(fixor).eq.'F'.or.test(31).eq.0.0)then

        tph=tph0

      elseif(im.gt.0)then

        tph=tp(im)

      else

        tph=0.0

      endif 



c change 10/93: set origin time to 0 when no absolute times (all PS)

c      if(nzcount.eq.0.and.ucase(fixor).ne.'F')tph=0.0



c 4/94: limit starting depth to test(70)

      if(startdep.gt.test(70))startdep=test(70)

      

c change 10/93: pass starting depth to startloc in xhh0(3)

      xhh0(3)=startdep

      

c 6/98: section to locate with slowness & azimuth

c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      if(nphase.eq.2)then                         

       dlt1=find_delta(phase(1),110.7/appar_veloc(1),tt1,imat)

       if(imat.ne.0)then

        trphs(1)=phase(1)

       else

        trphs(1)='????' 

       endif 

       dtwt1(1)=1.0

       dtwt1(2)=1.0 

       azm1=sngl(tp(2))

       call latlon(y0(1)/degtorad,x0(1)/degtorad,dlt1,azm1,alata,alona)

       xh1(1)=alona*degtorad

       std(1)=0.0

       xh1(2)=alata*degtorad

       xh1(3)=startdep

       std(1)=0.0

       std(2)=0.0

       std(3)=0.0

       tph=tp(1)-tt1

       dt(1)=0.0

       dlt(1)=dlt1*degtorad*rearth 

       dlt(2)=dlt1*degtorad*rearth 

       xx1=xh1(1)

       xx2=xh1(2)

       if(xx1.gt.pi)xx1=-(2.*pi-xx1)

       if(xx1.lt.-pi)xx1=2.*pi+xx1

       call unfold(xx2,xx1,la1,ins1,ala1,lo1,iew1,alo1)

       lo2=lo1

       la2=la1

       if(iew1.eq.'W')lo2=-lo1

       if(ins1.eq.'S')la2=-la1

       fo=' ' 

       fd='*'

       dp2=startdep

       m=2

       ndata=3

       call distaz(y0(1)/degtorad,x0(1)/degtorad,xh1(2)/degtorad,

     &  xh1(1)/degtorad,dedeg,azb,azz)

c       call delaz(xh1(2),xh1(1),dekm,dedeg,azz,y0(1),x0(1))

       baz0(1)=azb

       baz0(2)=azb

       az0(1)=azz

       az0(2)=azz



c find angle of incidence using IASPEI91 model

       dddd=110.7/appar_veloc(1)/rearth/degtorad      

       delz=xh1(3)

       rr=(rearth-delz)/rearth

       call emiask(rr,ro,vpp,vss)

       vpp=vpp*6.8501006

       vss=vss*6.8501006

       if(phase(1)(1:1).eq.'P'.or.phase(1)(2:2).eq.'P')vvv=vpp

       if(phase(1)(1:1).eq.'S'.or.phase(1)(2:2).eq.'S')vvv=vss



       arg=vvv*dddd

       if(arg.gt.1.)arg=1.

       aninc(1)=asin(arg)/degtorad



       call sectim(tph,iyr,jday,imnth,idy,ihn,imm,xhm)

       if(iulst.gt.0.and.(.not.multi_model))

     * write(iulst,950,iostat=code)

       call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c2000       if(iulst.gt.0)write(iulst,951,iostat=code)iyr,imnth,idy,

      if(iyr.lt.2000) i=iyr-1900

      if(iyr.ge.2000) i=iyr-2000



       if(iulst.gt.0.and.(.not.multi_model))

     * write(iulst,951,iostat=code)i,imnth,idy,

     & ihn,imm,xhm,fo,la1,

     & ala1,ins1,lo1,alo1,iew1,dp2,fd,ndata,m,rmssv,xk2,

     & (std1(i),i=1,3),icd

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

       tpc(1)=tt1

       tpc(2)=azm1

       return

      endif

c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

       

c    Find starting epicenter (if not fixed)  Added 'F' 4/92

c 7/94: add fixepi='N' option to force closest station start location

      if(ires.lt.7.and.ucase(fixepi).ne.'S'.and.ucase(fixepi)

     &.ne.'F'.and.ucase(fixepi).ne.'N'.and.nphase.gt.2)then

        

        test36=test(36)  

        test88=test(88)

        test(36)=0.0      !temporarily disable residual weighting

        test(88)=0.0

c        test(36)=10000.

c        test(88)=10000.



c 6/98 BRL: set xfar to 10000 temporarily for start location. 

c           This was causing problems due to most or all phases 

c           being weighted out by the distance weighting giving 

c           an artificially low rms when distance of the start

c           location became large

        xfar_temp=xfar

        xfar=50000.0

        call startloc(nphase,st,phase,tp,x0,y0,xhh0,xh2,nn,

     &  nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,

     &  tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,rmsmin,distind,

     &  nsol,aninc,trphs,vapp,azapp,alatm,alonm,isort,data,fixdep,

     &  depthlim,iflagd,ires,fixor,scorr,appar_veloc)

        xfar=xfar_temp

        test(36)=test36

        test(88)=test88

c        write(iulst,*,iostat=code)xhh0(1)/degtorad,xhh0(2)/degtorad

        call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

        do i=1,2

          xh0(i)=xhh0(i)

        end do

        

        startlon=xhh0(1)   !added 6/98

        startlat=xhh0(2)

        

c added depth phase determination 1/94

        startdep=xhh0(3)



c 4/94: limit starting depth to depthlim        

        if(startdep.gt.depthlim)then

         startdep=depthlim

         xhh0(3)=depthlim

        endif 

        

c  fixed starting depth

      else



c this is a call to initialize rmsv   -removed BRL 6/98

c        dtav=0.0                                         

c        rms=rmsv(nphase,st,phase,tp,x0,y0,xhh0,ndata,nmoho,

c     &  nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc1,

c     &  dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc1,

c     &  trphs,dtav,0,data,1,depthlim,iflagd,ires,fixor,scorr)



c  leave depth as it was

         xh0(1)=startlon

         xh0(2)=startlat

         xh0(3)=startdep

      endif



      xh0(3)=startdep

c       write(*,*)'startdep',startdep

c   set initial hypocenter, xh, to starting location

      do i=1,3

        xh(i)=xh0(i)

      end do                                           

c      write(iulst,*,iostat=code)xh(1)/degtorad,xh(2)/degtorad

      call sei code( fort$, code, iulst, b_flag ) ! Process outcome.

      fact=0.5

      irs=0

      dtav=0.0



c    fix depth initially (unless ires=8)

      if(ires.eq.8)then

        m=3

      else

        m=2

      endif



      izneg=0



c    initial value of damping

      xks=test(30)

      xk=xks



      m1=0

      nitr=test(11)                       

      

c 10/94: limit test(31) to 3      

      if(test(31).gt.3.)test(31)=3.0



c 6/94: only 1 iteration if everything fixed

      if(test(31).eq.0.0)nitr=1             

      if(ires.eq.8.or.ires.eq.9)nitr=1

      idec=0

      iend=0

      i1=0

      i2=0

      m1=m

      eps=test(32)



c   parameter change limit increased by 10 initially to free depth 

c   more quickly

      if(test(31).eq.3)eps=test(32)*10.



c    xke is the damping used for error calculations

      xke=xks



      kntt=0



c    irmsmx is the maximum increases allowed in xk

      irsmx=test(37)



      pcm=0.0



c    istart is used to count the number of convergences 

      istart=0

      std(3)=0.0

      tph1=tph                          

      tph3=tph

      tph2=tph                     

      

c ifirst is used to set minimum values on 1st iteration

      ifirst=1      



c 1/95: counts # of depth variation trials at end (only 1 allowed)

      idpcount=0



c  !!!!!!!!!!!!!!!!!!!!!!!!   iteration loop   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      if(test(31).eq.0.0)icflag=1 !added 6/98 BRL



      do 98 it=1,nitr



5000    continue



c   find ne1, the present hypocenter layer 

        do i=1,nl-1

          if(xh(3).lt.d(i+1))go to 28

        end do



        ne1=nl

        go to 29

28      ne1=i

29      continue



        vph=v(ne1)

        vsh=vs(ne1)



        do k=1,3

          ss(k)=0.0

        end do



c  <<<<<<<<<<<<<<<<<<<<<<<<<<<  phase loop  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



c        write(iulst,*,iostat=code)'xh ',(xh(i)/degtorad,i=1,2) 

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

      imatch=1

      if((it.ge.int(test(73)).or.nitr.lt.int(test(73)))

     & .and.test(72).ne.0.0)imatch=0

c      if(minflag.eq.5)imatch=1

      

c      write(iulst,*,iostat=code)imatch,test(72),test(73)

      call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c update origin time using previous mean residual, dtav

c 6/94 don't change for fixed origin time

      if(fixor.eq.'F'.or.ires.eq.2)then   !changed 6/98, BRL

       tph=tph0

      endif  



      rms=rmsv(nphase,st,phase,tp,x0,y0,xh,ndata,nmoho,nconrad,

     &iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc1,dl,iselv,

     &maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc1,trphs,dtav,

     &minflag,data,imatch,depthlim,iflagd,ires,fixor,scorr)   

      iupdate=1

c      write(iulst,'(''st'',i3,6f9.3)',iostat=code)ndata,rms,tph1-tph3,

c     & (xh(i)/degtorad,i=1,2),xh(3)

      call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c      write(iulst,'(10f8.3)',iostat=code)(dtw1(i),i=1,nphase)

      call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c      write(iulst,'(10f8.3)',iostat=code)(dt(i),i=1,nphase)

      call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c 6/98,BRL: added icflag to reset minimum rms when minutes corrected

      if(imflag.eq.1.or.it.eq.1.or.icflag.eq.1)then

c         write(iulst,*)' imflag=1'

          rms1=rms

          rms2=rms

          res2=res

          tph2=tph

          tph1=tph

          np=m

          np1=np

          do i=1,nphase

            dt1(i)=dt(i)

          end do



          do i=1,3

           xh1(i)=xh(i)

           xh2(i)=xh(i)

          end do 

          icflag=0

      endif 

c      write(iulst,'(10f8.3)')(dtw1(i),i=1,nphase)

c      write(iulst,'(10f8.3)')(dt(i),i=1,nphase)



        if(ndata.lt.3)then

          if(iulst.gt.0)then

            if(dataprev.ne.data(1))then

c              write(iulst,*,iostat=code)data(1)(1:78)

              call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

              dataprev=data(1)

            endif

            call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

            write(iulst,303,iostat=code)

            write(*,303)     !added 6/98 BRL

          endif

303       format(1x,'Less than three valid phases for this ',

     &    'event at its present location: no result')

          icd=6
          nmessage=nmessage+1
          message(nmessage)='Less than three valid phases for this '//
     &                      'event at its present location: no result'


          return

        endif



        if(ifirst.eq.1.and.iulst.gt.0.and.(.not.multi_model))then

          write(iulst,940,iostat=code)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

940       format(/,' iter   origin      lat      long depth  no  m',

     &    '    rms   damp.  erlg  erlt  erdp')

          write(iulst,972,iostat=code)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

972       format('         (sec)  (dg mn)   (dg mn)  (km)         ',

     &    '(sec)          (km)  (km)  (km)')

        endif



c    set rms1 = rms on first iteration

c    changed 7/94 to use ifirst instead of it=1

        if(ifirst.eq.1)then

          rms1=rms

          rms2=rms

          res2=res

          tph2=tph

          tph1=tph

          np=m

          np1=np

          do i=1,nphase

            dt1(i)=dt(i)

          end do



c 5/94: set xh1 & xh2 to xh

          do i=1,3

           xh1(i)=xh(i)

           xh2(i)=xh(i)

          end do 

          ifirst=0          

        endif



c  ----------------------  rms decrease  ---------------------------------



 741     continue

c        write(iulst,*,iostat=code)rms,rms1,m,np,np1,xh(3)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c change 5/94: for autopick, store minimum values at test(73) iterations

c even when rms is not minimum

        if(rms.le.rms1.or.ires.eq.9.or.it.eq.1.or.(test(72).ne.0.0.and.

     &  it.eq.int(test(73))))then



c save the relevant old values for minimum rms

          xk1=xk

          res1=res

          rms1=rms

          tph1=tph                        





c 10/93: disabled this - set tph=tph+dtav1 at very end

c Centering: any change in the mean residual, dtav, is added to the origin time

c          if(ires.ne.9.and.ires.ne.2)then

c            tph=tph+dble(dtav)

c          endif



c----------------------------------------------------------

c change 10/93: set origin time and dtav1 to 0 when no absolute times

c         if(nzcount.eq.0)then

c          tph=0.0

c          dtav1=0.0

c         endif

c----------------------------------------------------------

          

          do j=1,3

            std1(j)=std(j)

            xh1(j)=xh(j) 

            eig1(j)=eig(j)

            alpha1(j)=alpha(j)

          end do

          

c          write(iulst,'(''rms decr-eigs:'',3f8.3)'

c     &                 ,iostat=code)(eig1(i),i=1,3)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



           do i=1,nphase

             tpc(i)=tpc1(i)

             aninc(i)=aninc1(i)

             dtwt1(i)=dtw1(i)

             dt1(i)=dt(i)                            



c save minimum rms data importances

             di1(i)=di(i)



           end do



          do i=1,3

            ss(i)=0.0

          end do



c 10/93: set rms2=rms1 as well if rms1 is lower

         if(rms1.lt.rms2)then                  

             irs=0

c             write(iulst,*,iostat=code)'rms2 set to ',rms1,xh1(3)

             call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

             rms2=rms1

             tph2=tph1                            

             res2=res1

             do i=1,3

              xh2(i)=xh1(i)

             end do

          endif                                            

c    .................  centering and scaling  ........................



c 10/93 residual centering removed - now in rmsv



c    calculate the means, ss(j), of the weighted columns 

c    of the partial derivative matrix, g(i,j). 

c    wsum1 = sum of the valid weights (no azimuths or differences)



          wsum1=0.0



          do i=1,nphase

            ww=dtw1(i)*dtw1(i)  !change 6/98, BRL



c 10/93 don't include azimuth and difference phases in wsum1

            if(dtw1(i).lt.0.0.or.ips(i).eq.1.or.phase(i)(1:2).eq.

     &      'AZ')ww=0.0



            wsum1=wsum1+ww

            do j=1,3

              ss(j)=ss(j)+g(i,j)*ww

            end do

          end do



400       format(10f7.2)



          do j=1,3

            if(wsum1.ne.0.0)ss(j)=ss(j)/wsum1

          end do



c    center and weight the columns of g

          do i=1,nphase

            ww=dtw1(i)

            if(dtw1(i).lt.0.0)ww=0.0



c            write(iulst,*,iostat=code)g(i,3)

            call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

            do j=1,3



c 10/93 don't center azimuths & differences (modified to exlude ires=2, 4/94)

              if(dtw1(i).gt.0.0.and.ips(i).eq.0.and.phase(i).ne.

     &        'AZ  '.and.ires.ne.2)then

                g(i,j)=(g(i,j)-ss(j))*ww

              else

                g(i,j)=g(i,j)*ww

              endif

            end do

c            write(iulst,*,iostat=code)(g(i,j),j=1,3),ww

            call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

          end do



c   now scale the centered columns of g so they have an rms of 1

c   11/94: use ss1 for scaling factors instead of overwriting ss

          do j=1,3

            ss1(j)=0.0

            do i=1,nphase

              ss1(j)=ss1(j)+g(i,j)*g(i,j)

            end do

          end do



          do k=1,3

            ss1(k)=sqrt(ss1(k))

c            ss1(k)=ss(k)

          end do



          do i=1,nphase

            do k=1,3

              if(ss1(k).eq.0.0)then

                g(i,k)=0.0

              else

                g(i,k)=g(i,k)/ss1(k)

              endif

c      set g1=g

              g1(i,k)=g(i,k)

            end do                   

c           write(iulst,'(1x,5f12.6)',iostat=code)(g(i,k),k=1,3)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

          end do



c .....................................................................



c    output the current solution (changed 4/94 to allow fixing origin time)

          if(ucase(fixor).eq.'F'.or.ires.eq.2)then ! BRL 6/98 remove ires.eq9

           tphh=tph0

          else

c         origin time now updated in rmsv

           tphh=tph !+dble(dtav)                  ! BL Feb95, 6/98

cJAB(BGS)Feb95           tphh=tph1+dble(dtav)   ! BL Feb95

          endif  

          

          call sectim(tphh,iyear,jday,imnth,iday,ihr,imin,secs)

c          write(16,*)xh(1)/degtorad,xh(2)/degtorad

          call unfold(xh(2),xh(1),la1,ins1,ala1,lo1,iew1,alo1)



c change 10/93, 4/94 : correct depth to maxelv origin if test(40)>0

          dp2=xh(3)

c          if(test(40).gt.0.)dp2=xh(3)+float(maxelv)*.001

          rmssv=rms1

          if(rmssv.gt.999.999)rmssv=999.999

          if(dp2.gt.999.9)dp2=999.9

          xk2=xk

          if(xk2.gt.9999.9)xk2=9999.9

          do i=1,3

            if(std(i).gt.9999.9)std(i)=9999.9

          end do

          if(iulst.gt.0.and.(.not.multi_model)) write(iulst,941

     &                      ,iostat=code)it,secs,la1,ala1,ins1,lo1,

     &    alo1,iew1,dp2,ndata,m,rmssv,xk2,(std(i),i=1,3)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

  

c 10/93: minor format change

941       format(i5,f9.2,i3,f5.2,a1,i4,f5.2,a1,f6.1,i4,i3,f7.2,f8.3,

     &    4f6.1)



          kntt=0



c        check convergence criteria

          if(rms.lt.0.01.and.it.gt.1)then !change 6/98, BRL

            rms1=rms

            tph1=tph

            icd=1

            go to 97

          endif



          if(it.eq.nitr)then

            icd=5

            if(rms.lt.rms1)rms1=rms

            go to 97

          endif



          if(it.ne.1)then



c    decrease xk 

            if(irs.eq.0.and.it.gt.1)xk=xk/4.

  

c    more cautiously if rms has increased

            if(irs.ne.0)xk=xk/1.5



c    but don't go less than xks

            if(xk.lt.xks)xk=xks



          endif



          kk=2



        else

       

c  --------------------  rms increase  -------------------------------------

c      rms has increased - return to minimum rms values 

c      irs counts the attempts to increase rms 

          irs=irs+1



c          write(iulst,'(''rms incr'',i3,6f8.2)'

c      &                ,iostat=code)irs,rms,rms1,

c     &     (xh(i)/degtorad,i=1,2),xh(3),xk

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c          write(iulst,'(''eigs:'',3f8.3)',iostat=code)(eig1(i),i=1,3)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c change 10/93: allows rms to increase test(65) times to rms1*test(64)

c setting test(64)=0.0 disables this feature

          irskip=int(test(65))



          if(irs.le.irskip.and.rms.lt.rms1*test(64).and.m.eq.3)then



c change 4/94: save the (real) minimum rms in rms2 and hypocenter in xh2

            if(rms1.lt.rms2.and.irs.eq.1)then

c             write(iulst,*,iostat=code)'rms2 set to ',rms1,xh1(3)

             call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

             rms2=rms1

             tph2=tph1                            

             res2=res1

             do i=1,3

              xh2(i)=xh1(i)

             end do

            endif                                            

            

c set minimum rms temporarily to this (increased) value

            rms1=rms    

            tph1=tph



c might as well increase the damping a bit 

            xk=xk*2.

            

c now act as if rms had decreased

            go to 741

          endif

                      

c   return to minimum rms values

          if(it.ne.1)then

           res=res1

c          tph=tph1  

c          np=np1

           if(np.eq.np1)then

            do i=1,16

              ve(i)=ve1(i)

            end do

           endif

          endif

          do i=1,3

           xh(i)=xh1(i)

           if(it.ne.1.and.np.eq.np1)then

            eig(i)=eig1(i)

            alpha(i)=alpha1(i)



c 6/98, BRL restore scaling factors

            ss(i)=ss01(i)

            ss(i)=ss11(i)     



           endif

          end do



          do i=1,nphase

           dt(i)=dt1(i)

          end do



          if(irs.gt.irsmx)then

            icd=2



c    rms has still increased after test(37) increases of xk: fix depth

            if(m.eq.2)go to 97

            iend=1

            xk=xks

            irs=0

            m=2

c            if(iulst.gt.0)write(iulst,2002,iostat=code)icd

            call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

2002        format(1x,'depth fixed: icd=',i3) 



c change 4/94: reset to xh2 if rms2<rms1

            if(rms2.lt.rms1)then  

             rms1=rms2

             tph1=tph2             

             res1=res2

             rms2=999.

             do i=1,3

              xh(i)=xh2(i)

             end do

             go to 5000

            endif  

            

            dth(3)=0.0

            std(3)=0.0



          else   



c    increase xk

            xk=xk*test(39)

c            write(iulst,*,iostat=code)' k increased to ',xk,

c     &                                ' rms = ',rms

            call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

            if(xk.eq.0.0)xk=xks

 8           np=m

            go to 189



          endif



        endif



c **************************************************************************

41      continue



c    SVD of partial derivative matrix

        res2=0.0

        ndata2=0

        do i=1,nphase

c          write(iulst,'(3f10.3)',iostat=code)(g1(i,j),j=1,m)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

          ww=dtw1(i)

          if(ww.gt.0.0)ndata2=ndata2+1

          if(dtw1(i).lt.0.0)ww=0.0



c centered differences, dt2(i) are used to calculate parameter corrections

c modified 4/94 to allow for fixed origin time

           dt2(i)=dt1(i)*ww

           res2=res2+dt2(i)*dt2(i)

c          write(iulst,'(2f10.1)',iostat=code)dt1(i),dtw1(i)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

        end do

c        write(iulst,*,iostat=code)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c 9/94: added di & dtw1 to window

        call minv(m,nphase,np,dt2,di,dtw1)

        

c        write(iulst,*,iostat=code)(eig(i),i=1,3),np

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c 10/93: now need to calculate res, the sum of the squared residuals,

c from rms. Although nphase is not always the number of phases used, res 

c is divided by nphase in subroutine corr and this keeps the mean square 

c residual correct for the error calculation



c 2008        res=res2*float(nphase)/float(ndata2)

        res=res2     ! bl may 2008

         

c    save minimum rms SVD values

        if(it.eq.1.or.rms.le.rms1)then

          np1=np

c        write(iulst,*,iostat=code)' np1 ',np1

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

          res1=res



c 6/94 save minimum values at this point - moved from earlier point

c where they were still zero

          do i=1,3

           alpha1(i)=alpha(i)

           eig1(i)=eig(i)

           ss01(i)=ss(i)   !j changed to i, BRL 6/98

           ss11(i)=ss1(i)

          end do                        

          do i=1,16

            ve1(i)=ve(i)

          end do

        endif  

           

c        write(iulst,*,iostat=code)' res1 ',res1,np1,nphase,m

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c     calculate parameter corrections (dth) and standard deviations (std)

189     continue

c        write(iulst,*,iostat=code)' res1 ',res1,m,nphase,xk

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c        write(iulst,'(3f10.3)',iostat=code)(alpha(j),j=1,3)         

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c        write(iulst,'(9f9.3)',iostat=code)(ve(j),j=1,9)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c 9/94 changed nphase to ndata2 (now no of non-zero phases - only used

c      for errors)        

c 10/94 added distind, iflagd to corr window

        call corr(m,ndata2,np,xk,res1,test,oterr,distind,iflagd)



c 6/94: set dth(1), dth(2)=0 if fixepi=0

         if(fixepi.eq.'F')then

           dth(1)=0.0

           dth(2)=0.0

         endif

           

c        write(iulst,*,iostat=code)(ss1(j),j=1,m),res1,xk

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c         write(iulst,'(''corrs'',3f8.2)'

c      &            ,iostat=code)(dth(j)/(ss1(j)*degtorad)

c     &    ,j=1,m)

         call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c         write(iulst,*,iostat=code)' np,res, xk ',np,res,xk,ndata2

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



900     format(5f12.4)

c       write(iulst,*,iostat=code)(eig(i),i=1,3)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c    calculate std devs and unscaled parameter corrs in km

        sum=0.0

        

        do ib=1,m

          if(ires.ne.8.and.ss1(ib).ne.0.0)then

c            std(ib)=rearth*sqrt(var(ib)/ss1(ib))

            std(ib)=rearth*sqrt(var(ib,ib))

            sum=sum+(rearth*dth(ib)/ss1(ib))**2

          endif

        end do                             

c        write(iulst,'(3f10.3)',iostat=code)(std(i),i=1,m)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c    pcm = magnitude of parameter change in km

        pcm=sqrt(sum)

c        write(iulst,'(''pcm '',2f8.2)',iostat=code)pcm,xk

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

        dfact=1.0



c     apply parameter corrections to xh to solution, xh

        do j=1,m

          if(ires.ne.8.or.j.eq.3)then

            if(ss1(j).ne.0.0)then
c
c   make sure the correction is not very big, jh jan 2017
c
              ddd=dth(j)/ss1(j)
              if(abs(ddd).gt.0.1.and.(j.eq.1.or.j.eq.2)) then
               if(ddd.gt.0.0) then
                  ddd=0.1
               else
                  ddd=-0.1
               endif
            endif



c 11/94: correct dtav for hypocentral adjustment

c              dtav=dtav+ddd*ss(j)



c  convert depth correction to km

              if(j.eq.3)ddd=ddd*rearth

              

              xh(j)=xh(j)+ddd                                       

              

            endif

          endif

        end do



c change 6/98, BRL: update origin time

c         if(fixor.ne.'F'.and.ires.ne.2.and.test(31).gt.0.0)then

         if(fixor.ne.'F'.and.ires.ne.2.and.abs(test(31)).gt.0.0)then

          if(iupdate.eq.1)then

           iupdate=0

c           write(iulst,*)' dtav added: ',dtav

c           tph=tph+dtav       

          endif 

         else

          tph=tph0

         endif  



c 2/95: prevent latitude > 90 or <-90

        if(xh(2).gt.0.5*pi)then

         xh(1)=xh(1)+pi

         xh(2)=pi-xh(2)

        endif

        if(xh(2).lt.-0.5*pi)then

         xh(1)=xh(1)+pi

         xh(2)=-pi-xh(2)

        endif



c 6/94 prevent longitude>180 deg        

        if(xh(1).gt.pi)xh(1)=-(2.*pi-xh(1))

        if(xh(1).lt.-pi)xh(1)=2.*pi+xh(1)

c       write(iulst,'(''dth '',3f8.2)'

c      &         ,iostat=code)(dth(j)/(ss1(j)*degtorad),

c     &  j=1,2),rearth*dth(3)/ss1(3)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c       write(iulst,'(''new sol'',3f10.2,f10.3)'

c      &          ,iostat=code)(xh(j)/degtorad,j=1,2)

c     &  ,xh(3),rms

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c    if depth < 0 or pcm > test(2) increase xk and try again



c change 10/93: correct depth origin if test(40)>0

        dptest=xh(3)

c        if(test(40).gt.0.0)dptest=xh(3)+float(maxelv)*.001



c 6/94: don't use test(2) limit for distant events        

        if(dptest.lt.0.0.or.(pcm.gt.test(2).and.distind.ne.'D'))then



c          write(16,*)rms,xh(1)/degtorad,xh(2)/degtorad,xh(3)

          xk=xk*test(39)

c          write(iulst,*,iostat=code)'d<0 or pcm>test(2), 

c     &      xk increased to ',xk

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c    kntt counts the attempts to cause depth < 0 and/or make pcm > test(2)

          kntt=kntt+1



c    return to minimum rms solution

          do j=1,3

            xh(j)=xh1(j)

          end do



c    redo the corrections if kntt<10

          np=m

          if(kntt.le.10)go to 189



c    fix depth if kntt > 10

          m=2

          xk=xks

          irs=0

          std(3)=0.0



c change 10/93: no error2 message, just terminate with icd=7

          icd=7

          do j=1,3

            xh(j)=xh1(j)

          end do

          go to 97 

        endif



c change 1/94 BRL: limit depth to depthlim

        if(xh(3).gt.depthlim)then

         if(depthlim.eq.test(70))then

          if(iulst.gt.0.and.(.not.multi_model))

     *    write(iulst,'('' depth fixed at '',

     &    f6.1,'' km'')',iostat=code)depthlim

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

          write(*,'('' depth fixed at '',

     &    f6.1,'' km'')')depthlim
          nmessage=nmessage+1
          write(message(nmessage),
     &    '(a,f6.1,a)')'depht fixed at ', depthlim, ' km'

         endif

         xh(3)=depthlim

         iend=1

         xk=xks

         irs=0

         m=2

        endif



c     write(iulst,*,iostat=code)(dth(kk),kk=1,3)

      call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c    don't go more than test(41) km from nearest station

c         call distaz(y0(im)/degtorad,x0(im)/degtorad,xh(2)/degtorad,

c     &   xh(1)/degtorad,dedeg,azz0,azb)

c        delta=rearth*dedeg*degtorad

        call delaz(y0(im),x0(im),delta,dedeg,azz0,xh(2),xh(1))

c        write(iulst,*,iostat=code)delta

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

        if(delta.gt.test(41).and.distind.ne.'D')then

          icd=4

          if(iulst.gt.0)write(iulst,2127,iostat=code)test(41)

          call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

          write(*,2127)test(41)

2127      format(1x,/,' event is >',f7.1,' km from closest ',

     &    'station: no output ',/)
          nmessage=nmessage+1
          write(message(nmessage),'(a,f7.1,a)')
     *    'event is ',test(41),' km from closest station, not output'

          if(rms.lt.rms1)rms1=rms

          go to 97

        endif



c   convergence: pcm < eps or last iteration

c        write(iulst,*,iostat=code)pcm,eps,istart

        call sei code(fort$,code,iulst,b_flag)

        if(pcm.le.eps.or.it.eq.nitr)then

c          istart=istart+1  !removed 6/98, BRL

c          icd=3



          if(it.eq.nitr)then

            write(iulst,

     &      '(/'' ******* Maximum Iterations Exceeded''/)',

     &      iostat=code)

            call sei code(fort$,code,iulst,b_flag)

            icd=5

            if(rms.lt.rms1)rms1=rms

            go to 97

          endif



c this section changed 6/98, BRL<<<<<<<<<<<<<<<<<<<<<<<<<<<<

           istart=istart+1

           icflag=0

           

c check for minute errors

          if(istart.eq.1.and.m.eq.2)then

           icflag=1



c recalculate rms with minute check

           go to 5000

          endif 

          

          icd=3



          if(istart.eq.4)then

            if(rms.lt.rms1)rms1=rms

            go to 97

          endif

c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

c   free depth on first time thru (istart=1)

          if(istart.eq.2)then



c   change 10/93: don't free depth if ndata<4

            if(ndata.ge.4)m=abs(test(31))

c            write(iulst,*,iostat=code)

c     &             'ires,fixdep,ndata',ires,fixdep,ndata

            call sei code(fort$,code,iulst,b_flag)

            if(ires.eq.1.or.fixdep.eq.'F')m=2

            if(m.eq.3.and.iulst.gt.0.and.(.not.multi_model))then

             write(iulst,'(1x,''depth freed: icd= '',i3)'

     &                  ,iostat=code)icd

             call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

            endif 



c 6/94 changed to only do this if m=3 - otherwise exit

            if(m.eq.3)then   

             irs=0

             eps=test(32)

c             xk=xks

             

c          redo the SVD

             go to 41

            endif 



            icd=3

            if(rms.lt.rms1)rms1=rms

            go to 97



          elseif(istart.eq.3)then



c     fix depth on 2nd time thru

            if(m.eq.2)then



c     depth already fixed - exit

              icd=3

              if(rms.lt.rms1)rms1=rms

              go to 97

            endif

    

c      fix depth

            m=2

c            if(iulst.gt.0)write(iulst,2002,iostat=code)icd

            call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

            dth(3)=0.0

            std(3)=0.0



c     set xk to starting value

            xk=xks

            iend=1

            irs=0

            izneg=0                



c change 4/94: reset to rms2 and xh2 if rms2<rms1

            if(rms2.lt.rms1)then

c             write(iulst,*,iostat=code)' rms reset to ',rms2

             call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

             rms1=rms2      

             res1=res2

             tph1=tph2

             do i=1,3

              xh1(i)=xh2(i)

             end do

            endif                                



c 1/95: section to find minimum rms depth testing at 10 km intervals

            if (test(96).gt.0.0)then !added condition 6/98, BRL

             dincr=10.

             dpmin=xh1(3)

             rmsdt=rms1

             rmsmin=rms1

             dpmin=xh1(3)

             xh1(3)=xh1(3)-dincr

             if(xh1(3).lt.0.0)xh1(3)=0.0

c             do while (xh1(3).lt.dpmin.and.rmsdt.le.rmsmin.and.xh1(3).

c     &       gt.0.0)

             do while (xh1(3).lt.dpmin.and.xh1(3).

     &       gt.0.0)

              rmsdt=rmsv(nphase,st,phase,tp,x0,y0,xh1,ndata,

     &        nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,

     &        dtwt,dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,

     &        tph,distind,aninc1,trphs,dtav,minflag,data,imatch,

     &        depthlim,iflagd,ires,fixor,scorr)

c              write(iulst,*)rmsmin,dpmin,rmsdt,xh1(3)

              if(rmsdt.le.rmsmin)then

               rmsmin=rmsdt

               dpmin=xh1(3)

              endif

              xh1(3)=xh1(3)-dincr

              if(xh1(3).lt.0.0)xh1(3)=0.0

             enddo

             rms1=rmsmin

             xh1(3)=dpmin

            

c recalculate everything

            go to 41

           endif

c            go to 5000



          endif



        endif



c.....................................................................



c    don't count rms increase as an iteration

        if(rms.gt.rms1.and.ires.lt.7)then

c          write(*,*)rms,rms1,xk,pcm

          go to 5000

        endif



c    set new minimum rms value to rms

        rms1=rms

        tph1=tph 



c added 4/94 to also set rms2 & xh2

        if(rms1.lt.rms2)then

c         write(iulst,*,iostat=code)' rms2 set to ',rms1

         call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c         rms2=rms1

         do i=1,3

c          xh2(i)=xh1(i)

         end do

        endif  



98    continue



c  !!!!!!!!!!!!!!!!!!!!!  end of iteration loop  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



97    continue

c

c change 4/94: reset to rms2 and xh2 if rms2<rms1
c      write(6,*)'rms1,rms2',rms1,rms2

      if(rms2.lt.rms1)then

c       write(iulst,*,iostat=code)' rms reset to ',rms2
c        write(6,*)' rms reset to ',rms2

       call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

       rms1=rms2      

       res1=res2

       tph1=tph2

       do i=1,3

        xh1(i)=xh2(i)

       end do

      endif                                



c 6/94 recalculate rms at minimum rms solution - residuals in dt1

      rms1=rmsv(nphase,st,phase,tp,x0,y0,xh1,ndata,nmoho,

     & nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,

     &tpc1,dl,iselv,maxelv,ips,ip,dt1,xfar,xnear,tph1,distind,

     &aninc1,trphs,dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,

     &scorr)


cjh
c   ain came out wrong since, in case of increasing rms when depth was freed, the
c   ain with increased dept was used instead of the value for minimum rms. apparently
c   ain was only set in a section far above while the phases is set later. the section
c   above has been copied to below and other variabels that migh also have to updated
c   are commented out with cjh. di was also wrong so that has been added.

           do i=1,nphase

cjh             tpc(i)=tpc1(i)

             aninc(i)=aninc1(i)

cjh             dtwt1(i)=dtw1(i)

cjh             dt1(i)=dt(i)                            



c save minimum rms data importances

cjh             di1(i)=di(i)
              di(i)=di1(i)



           end do

c
c      do i=1,nphase
c      write(6,*) st(i),phase(i),aninc1(i),trphs(i)
c      enddo

c  calculate errors for final solution with m=3

      m=3

      

      

c 6/98: fix depth if ndiffstat=1

      if(ndiffstat.eq.1)m=2                          



c 10/94: fix depth if ndata2<4

      if(ndata2.lt.4)m=2



c xke is the damping used to calculate the errors

      xke=xks                                    

      

c 6/94 set m=2 if depth fixed

      if(ndata.eq.3.or.ires.eq.1.or.fixdep.eq.'F')m=2

      

c if test(38)=0, set it to 0 (least squares errors)      

      if(test(38).eq.0.0)xke=0.0                   



c 6/94 need to recalculate weighted residuals for error calculation

      res2=0.0

      ndata2=0

      do i=1,nphase

c        write(iulst,'(3f10.3)',iostat=code)(g1(i,j),j=1,m)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

        ww=dtw1(i)



c 10/94: set dtwt1(i)=dtw1(i)

        dtwt1(i)=dtw1(i)

        

        if(ww.gt.0.0)ndata2=ndata2+1

        if(dtw1(i).lt.0.0)ww=0.0

        dt2(i)=dt1(i)*ww

        res2=res2+dt2(i)*dt2(i)

c        write(iulst,'(2f10.1)',iostat=code)dt1(i),dtw1(i)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

      end do

c 2008      res1=res2*float(nphase)/float(ndata2)

        res1=res2             ! added bl may 2008

c      write(iulst,*,iostat=code)' res1 ',res1,np1,nphase,m

      call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c 9/94: added di & dtw1 to window

      call minv(m,nphase,np1,dt2,di,dtw1)



c 9/94 changed nphase to ndata2

      call corr(m,ndata2,np1,xke,res1,test,oterr,distind,iflagd)

      

c      write(iulst,*,iostat=code)(ss1(j),j=1,m),res1,xke

      call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

      

      do i=1,3

       std1(i)=rearth*sqrt(var(i,i))

       if(std1(i).gt.999.9)std1(i)=999.9



c 7/94: set error to 999.9 if zero - unconstrained case

       if(std1(i).eq.0.0)std1(i)=999.9                 

       

      end do

      if(m.eq.2)std1(3)=0.0

       

c   calculate resolution matrix, resl

      do i=1,m

       do j=1,m

        sum=0.0

        do k=1,np1

         ii=(k-1)*m+i

         jj=(k-1)*m+j



c change 10/93: prevent divide by zero

         if(eig1(k).gt.0.0)then

          fact=1./(1.+xk1/eig1(k)**2)

          sum=sum+ve(ii)*ve(jj)*fact

         endif



        end do

        resl(i,j)=sum

       end do

      end do



c 7/94 calculate error ellipsoid

      ic=1

      do  j=1,m

        do  k=1,j

          a(ic)=var(j,k)

          ic=ic+1

        end do

      end do

      call eigen(a,ve,m,0)    

      dip1=atan2(ve(3),sqrt(ve(1)*ve(1)+ve(2)*ve(2)))

      iazyx1=int(0.49999+atan2(ve(1),ve(2))/degtorad)

c      write(iulst,*,iostat=code)'var ',rearth*sqrt(a(1)),

c     &rearth*sqrt(a(3)),

c     & rearth*sqrt(a(6))  

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c      write(iulst,*,iostat=code)ve(1),ve(2),ve(3)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c      write(iulst,*,iostat=code)ve(4),ve(5),ve(6)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c      write(iulst,*,iostat=code)ve(7),ve(8),ve(9)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

      

c    convert epicenter to geographic lat/long

      xx1=xh1(1)

      xx2=xh1(2)



c change 3/94 to correct longitude > 180 deg  or < -180   

      if(xx1.gt.pi)xx1=-(2.*pi-xx1)

      if(xx1.lt.-pi)xx1=2.*pi+xx1



      call unfold(xx2,xx1,la1,ins1,ala1,lo1,iew1,alo1)

      lo2=lo1

      la2=la1

      if(iew1.eq.'W')lo2=-lo1

      if(ins1.eq.'S')la2=-la1



c   center the origin time and convert it to standard format (4/94 if not fixed)

c      write(iulst,*,iostat=code)'dtav1=',dtav1

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

      if(ires.ne.9.and.ires.ne.2.and.ucase(fixor).ne.'F')then

       tphh=tph1 !+dble(dtav)  changed 6/98, BRL

      else

       tphh=tph0

      endif

        

      call sectim(tphh,iyr,jday,imnth,idy,ihn,imm,xhm)



c    output final solution

      if(test(50).eq.0.0.and.iulst.gt.0)then

        write(iulst,953,iostat=code)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

953     format(/'    *************   Azimuth',

     &  '  data ignored *************')

      endif

      if(iulst.gt.0.and.(.not.multi_model))

     *write(iulst,950,iostat=code)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

950   format(/,'   date hrmn   sec      lat      long depth   no m',

     &'    rms  damp erln erlt erdp ic')

      fd=' '

      if(ires.eq.1.or.ires.eq.9.or.abs(test(31)).eq.2.)fd='*'

      fo=' '

      if(ires.eq.2.or.ucase(fixor).eq.'F')fo='*'



c change 10/93: correct for depth origin

      dp2=xh1(3)



c 4/94: removed this - depth is always relative to defined origin

c      if(test(40).gt.0)dp2=xh1(3)+float(maxelv)*.001



      if(dp2.gt.999.9)dp2=999.9

      rmssv=rms1

      if(rmssv.gt.999.99)rmssv=999.99

      

c 6/94 set damping to value used for errors      

      xk2=xke

      if(xk2.gt.99.999)xk2=99.999



      do i=1,3

        if(std1(i).gt.999.9)std1(i)=999.9

      end do



c change BRL 11/95: add zeros to hr,min format

      if(nzcount.gt.0)then

       write(thrmin,'(2i2)')ihn,imm          

      else

       thrmin='    '

       xhm=0.0

      endif  

      do i=1,4

       if(thrmin(i:i).eq.' ')thrmin(i:i)='0'

      enddo

      if(iyr.lt.2000) i=iyr-1900

      if(iyr.ge.2000) i=iyr-2000

      if(iulst.gt.0.and.(.not.multi_model))

     *write(iulst,951,iostat=code)i,imnth,idy,

     &ihn,imm,xhm,fo,la1,

     &ala1,ins1,lo1,alo1,iew1,dp2,fd,ndata,m,rmssv,xk2,

     &(std1(i),i=1,3),icd

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



951   format(1x,3i2,1x,2i2,f6.2,a1,i2,f5.2,a1,i4,f5.1,a1,f6.1,a1,i4,i2,

     &f7.2,f6.3,3f5.1,i3)

     

c 10/94: add origin time  error printout

      if(iulst.gt.0.and.nzcount.gt.0.and.(.not.multi_model))

     & write(iulst,'(/'' Origin time error: '',f10.2)'

     &            ,iostat=code)oterr      

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c 3/94: remove screen printout of solution

c      write(*,951)iyr,imnth,idy,ihn,imm,xhm,fo,la1,

c     &ala1,ins1,lo1,alo1,iew1,dp2,fd,ndata,m,rmssv,xk2,

c     &(std1(i),i=1,3),icd



c  calculate changes in rms for changes of test(13) km in lat long and depth

c  the final value of ir (7) is for the solution, ensuring that the correct

c  values of the phases and times are returned to hypocent



c added if statement 1/94 to disable when test(13)=0.0

      if(test(13).gt.0.0)then

       if(iulst.gt.0.and.(.not.multi_model))

     *write(iulst,'(/,'' DRMS Values: d='',f8.2,

     &  '' km'',/)',iostat=code)test(13)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

       dtav=0.0
       
       do  ir=1,7

c xhd1 is the original solution, xhd is the perturbed solution
         do  j=1,3

           xhd(j)=xh1(j)

         end do

         f=test(13)

c f=-test(13) for even ir, f=test(13) for odd 
         if((ir/2)*2.eq.ir)f=-f

         f1=f
c**************************************************         
c if ir<7 add and subtract f1 from xhd(ind) to get drms

c  ir  ind    xhd(ind)

c   1   1   xhd1(ind)+f1
c   2   1   xhd1(ind)-f1
c   3   2   xhd1(ind)+f1
c   4   2   xhd1(ind)-f1
c   5   3   xhd1(ind)+f1
c   6   3   xhd1(ind)-f1
c   7  no perturbation: exclude next section

c if condition added 5/3/2013 by BRL
         if (ir.lt.7)then

            ind=(ir+1)/2

c            write(*,*) ' debug xhd ind = ',ind

c

            if(ind.gt.3) then

               ind=3

c               write(6,*) 'wrong ind hypoloc'

            endif

            if(ir.eq.6.and.xhd(ind).lt..05)f1=0.0

            if(ind.ne.3)f1=atan(f/rearth)

            if(ind.eq.1)f1=f1/abs(cos(xh1(2)))

            if(ind.le.3)xhd(ind)=xhd(ind)+f1


c   if depth < 0 set it to 0

            if(ind.eq.3.and.xhd(ind).lt.0.0)xhd(ind)=0.0
            
        endif
c******************************************************

 

c 6/94 changed to allow for fixed origin time

         if(ires.ne.9.and.ires.ne.2.and.ucase(fixor).ne.'F')then

          tph=tph1

         else

          tph=tph0

         endif



c 6/94 changed tpc1 to tpc so minimum residuals don't get overwritten         

         rmss(ir)=rmsv(nphase,st,phase,tp,x0,y0,xhd,ndata,nmoho,

     &   nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc,

     &   dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc1,trphs,

     &   dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,scorr)

c      write(iulst,'(''st'',i3,6f9.3)',iostat=code)ndata,rmss(ir),

c     &  tph1-tph3,

c     & (xhd(i)/degtorad,i=1,2),xhd(3)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c         write(iulst,*,iostat=code)'rmss ',rmss(ir)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

       end do

       dr1='DRMS'

       dr2='pos'



       do i=1,6

        rmss(i)=rmss(i)-rmss(7)

        if(rmss(i).lt.0.0)dr2='neg'

        if(rmss(i).gt.99.99)rmss(i)=99.99

        if(rmss(i).lt.-99.99)rmss(i)=-99.99

       end do

      endif



      do i=1,nphase

        call distaz(y0(i)/degtorad,x0(i)/degtorad,xh1(2)/degtorad,

     &  xh1(1)/degtorad,dedeg,azb,azz)

        delta=rearth*dedeg*degtorad

c

c This part changed by M. Villagran Aug 1997 (temporary solution)

c---------------------------------------------------------------

c



c       calculate azimuths and distances from stations to earthquake

c       azimuth here is relative to the source

c       write(6,*)xh1(2)*180/3.1415926,xh1(1)*180/3.1415926,dekm,

c    *  dedeg,azz,y0(i)*180/3.1415926,x0(i)*180/3.1415926

c       call delaz(xh1(2),xh1(1),dekm,dedeg,azz,y0(i),x0(i))

        call delaz(y0(i),x0(i),dekm,dedeg,azz,xh1(2),xh1(1))



c       write(6,*)xh1(2)*180/3.1415926,xh1(1)*180/3.1415926,dekm,

c    *  dedeg,azz,y0(i)*180/3.1415926,x0(i)*180/3.1415926

c       write(6,*)'--------------------------------------------'

        dlt(i)=dekm



c       az0(i) is calculated relative to the source

c       az0(i)=azz-180.

        az0(i)=azz

        if(az0(i).lt.0.0)az0(i)=360.+az0(i)

c here gets the backazimuth to use it after in hyposub1.for for

c the calculation of the residuals.....

        call delaz(xh1(2),xh1(1),dekm,dedeg,azz,y0(i),x0(i))

        baz0(i)=azz



c---------------------------------------------------------------

c       center residuals if ips(i)<>0. Changed 4/94 for fixed origin case

        if(ips(i).eq.0.and.phase(i)(1:2).ne.'AZ'.and.ires.ne.9

     &  .and.ucase(fixor).ne.'F')then

         dt(i)=dt1(i)!-dtav

        else

         dt(i)=dt1(i)

        endif  

        

c 10/94: put minimum rms data importances back in di

        di(i)=di1(i)        



c        write(iulst,*,iostat=code)az0(i)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c        write(iulst,*,iostat=code)' dt ',dt(i)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

      end do



      tph=tphh

      

c      write(iulst,*,iostat=code)' hy dtav1 ',dtav1

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



      if(test(13).gt.0.0)then

       if(iulst.gt.0.and.(.not.multi_model))

     * write(iulst,1502,iostat=code)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

1502   format(1x,'DRMS:',7x,'lon+d     lon-d     lat+d     lat-d',

     @ 3x,'depth+d   depth-d')

       if(iulst.gt.0.and.(.not.multi_model))

     * write(iulst,1501,iostat=code)dr1,dr2,

     &  (rmss(k),k=1,6)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

1501   format(1x,2a5,1x,6(f6.2,4x))

      endif



      if(iulst.gt.0.and.(.not.multi_model))

     *write(iulst,998,iostat=code)xk1

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

998   format(1x,/,1x,'Resolution matrix: k = ',f10.3//,

     &29x,'Long',5x,'Lat',3x,'Depth')

971   format(21x,'Long',3f8.3,/,22x,'Lat',3f8.3,/,20x,'Depth',3f8.3)

      if(iulst.gt.0.and.(.not.multi_model))

     *write(iulst,971,iostat=code)(resl(1,j),j=1,3),

     &(resl(2,j),j=1,3),(resl(3,j),j=1,3)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

      return

      end



c  ************  travel time consistency check subroutine *****************



      subroutine ttcheck(nphase,isort,dtwt,ips,phase,st,iulst,dataprev,

     &data,xh,x0,y0,iselv,maxelv,dt2,test,tp,nmoho,nconrad,tpc1,

     &xnear,xfar,ip,dt,tph,aninc,trphs,distind,depthlim,iflagd,

     &minflag1,imatch,statchk,ndata,dtw1,dl,tt,xh1,

     &dtdl,dtdh,dddp,phcd,scorr)

     

      implicit none

      save

      include 'hypparm.inc'

      include 'libsei.inc'                    ! Library definitons.

c      include 'param.inc'

      external sei code                       ! Error encoder.

      integer  code                           ! Condition.

      logical  b_flag                         ! Flag it?.

      real dltmin1

      real pi,degtorad,rearth,ph

      real azz,azb,dedeg,delta,depthlim

      real xnear,xfar,dltmin2

      real dltmin3

      integer iulst,ii,i,jj,j,nphase

      integer maxelv,j1,iflagd,minflag1,imatch,nconrad,nmoho

      integer j2,j3,ndata

      character*5 statchk 

      pi=3.141593

      degtorad=pi/180.

      rearth=6378.                         

      

c 10/93: added check that secondary phases do not precede 'P   '

        do ii=1,nphase



           i=isort(ii)

c          write(*,*)st(i),phase(i),ips(i),dtwt(i)



c change 3/94: changed to only check 'L???', 'S   ' and 'P   ' - otherwise tests

c core phases, PP's, etc

c 10/94: changed to exclude azimuths

          if(dtwt(i).gt.0.0.and.ips(i).eq.0.and.(phase(i)(1:4).eq.

     &     'S   '.or.phase(i)(1:1).eq.'L'.and.phase(i)(1:4).ne.'AZ  '

     &     ))then

c            write(iulst,*,iostat=code)st(i),phase(i)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

            statchk=st(i)

            do jj=ii+1,nphase

              j=isort(jj)

              if((dtwt(j).gt.0.0.and.ips(j).eq.0).and.phase(j)(1:4)

     &         .eq.'P   '.and.st(i).eq.st(j))then

c               write(iulst,*,iostat=code)phase(j),tp(j)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

                if(iulst.gt.0.and.(.not.multi_model))then

                  if(dataprev.ne.data(1))then

c                    write(iulst,*,iostat=code)data(1)(1:78)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

                  endif

                  write(iulst,*,iostat=code)' time for ',st(i),

     &            ' phase ',phase(i),

     &            ' cannot precede P phase: weighted out'

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

                endif

                if(dataprev.ne.data(1))then

c                  write(*,*)data(1)(1:78)

                  dataprev=data(1)

                endif

                write(iulst,'('' time for '',a4,'' phase '',a8,

     &           '' cannot precede P phase: weighted out'')')

     &           st(i),phase(i)

                dtwt(i)=0.0

              endif

            end do

          endif

        end do



c---------------------------------------------------------------------

c change 10/93 BRL: added next section to test for bad times

c 5/94 changed this test completely

c  I now calculate arrival time differences between each station and

c  the station closest to it having the same phase. 

c  If the two stations have an arrival time difference > 

c  the travel time between them + test(60), the test fails. To find which 

c  of the two arrivals is bad, I repeat the test with each of the two stations

c  using the stations closest to each of them, but excluding each other.  

c  The results of these two tests are then used to weight either (or both)

c  of the arrivals to zero



        do i=1,nphase



c         set the 'earthquake' to the i-station position,

c         provided it is not a PS or PL difference or azimuth,

c         has zero weight or is blank. The depth origin is left at maxelv

c         for this test

          if(ips(i).eq.0.and.ip(i).ne.9.and.dtwt(i).gt.0.0

     &    .and.phase(i)(1:4).ne.'    '.and.phase(i)(2:2).ne.'c'

     &    .and.phase(i)(2:2).ne.'K'.and.phase(i)(1:1).ne.'p'

     &    .and.phase(1)(1:1).ne.'s'.and.phase(i)(1:4).ne.'AZ  ')then

c            write(iulst,'('' 1'',i8,1x,a4,1x,a8)',iostat=code)i,

c     &      st(i),phase(i)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

            xh(1)=x0(i)

            xh(2)=y0(i)

            xh(3)=float(maxelv-iselv(i))*.001                     

              

c 5/94: changed to find closest station having the same arrival              



c dltmin1 is the distance to the closest station

            dltmin1=1.e32



c          find the closest station with same non-zero weight absolute phase

            j1=0

            iflagd=0

            do j=i+1,nphase



c 4/94: changed this so that all 4 phase letters match

              if(st(i).ne.st(j).and.phase(j).eq.phase(i).and.dt2(j).eq.

     &         0.0.and.ips(j).ne.1.and.ip(j).ne.9)then



c             get distance to this station

                call distaz(y0(j)/degtorad,x0(j)/degtorad,

     &          xh(2)/degtorad,xh(1)/degtorad,dedeg,azz,azb)

                delta=dedeg*degtorad*rearth



c 10/94: don't allow iflagd=1 for local events 

                if(delta.gt.test(57).and.distind.ne.'L')iflagd=1

                

                if(delta.lt.dltmin1)then

                  dltmin1=delta



c             save closest phase index in j1

                  j1=j                                        

                

                endif 

              endif

            end do     



            if(j1.gt.0)then 

c  set dt2(i) to 1 to indicate its been tested

              dt2(i)=1.0                   



c calculate the travel time to the closest station



c calculate minimum travel time for 1st phase letter

              minflag1=1

              imatch=0

c              write(iulst,*,iostat=code)st(i),phase(i),st(j1),

c     &                                  phase(j1),dltmin1

              call trtime(j1,1,st,phase,tp,x0,y0,xh,ndata,

     &        nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,

     &        dtwt,dtw1,tpc1,dl,iselv,maxelv,ip,dt,xfar,xnear,tph,

     &        aninc,trphs,iflagd,minflag1,data,'L',imatch,

     &        depthlim,scorr,ph,dx)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c              write(iulst,'(1x,''1'',a4,1x,a4,1x,a4,3f10.1)'

c     &                   ,iostat=code)

c     &        st(i),st(j1),phase(i),tp(i)-tp(j1),tpc1(j1),dltmin1

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c 6/94: changed to exclude cases where only one phase is PKP or PKiKP

c 9/94: exclude cases where tpc1(j1)=0.0 (no calculated time)

              if(dabs(tp(i)-tp(j1)).gt.tpc1(j1)+test(60)

     &        .and.dtwt(j1).gt.0.0.and.((trphs(j1)(2:2).ne.'K'

     &        .and.trphs(i)(2:2).ne.'K').or.trphs(j1)(2:2).eq.

     &        trphs(i)(2:2)).and.tpc1(j1).ne.0.0)then

              

C           test has failed: find st(j2), the closest station to st(i) 

c          (excluding st(j1)) and st(j3), the closest station to st(j1) 

c          (excluding st(i))

                xh1(1)=x0(j1)

                xh1(2)=y0(j1)

                xh1(3)=float(maxelv-iselv(j1))*.001                     

                dltmin2=1.e32                   

                j2=0                                    

                dltmin3=1.e32

                j3=0

                

                do j=1,nphase

                  if(st(j1).ne.st(j).and.phase(j).eq.phase(j1).and.

     &            dtwt(j).gt.0.0.and.ips(j).ne.1.and.ip(j).ne.9.

     &            and.st(j).ne.st(i).and.dt2(j).ge.0.0)then



c                get distance of st(i) to st(j)

                    call distaz(y0(j)/degtorad,x0(j)/degtorad,

     &              xh(2)/degtorad,xh(1)/degtorad,dedeg,azz,azb)

                    delta=dedeg*degtorad*rearth

                    if(delta.lt.dltmin2)then

                      dltmin2=delta

                      j2=j

                    endif 



c                get distance to st(j1) to st(j)

                    call distaz(y0(j)/degtorad,x0(j)/degtorad,

     &              xh1(2)/degtorad,xh1(1)/degtorad,dedeg,azz,azb)

                    delta=dedeg*degtorad*rearth

                    if(delta.lt.dltmin3)then

                      dltmin3=delta

                      j3=j

                    endif 

                  endif

                end do

                if(j2.gt.0)then



c                 calculate the travel time of st(i) to st(j2)

                  iflagd=0

                  if(delta.gt.test(57).or.distind.eq.'D')iflagd=1

                  minflag1=1

                  imatch=0

c                 write(iulst,*,iostat=code)st(i),phase(i),st(j),

c     &              phase(j),dltmin

                  call trtime(j2,1,st,phase,tp,x0,y0,xh,ndata,

     &            nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,

     &            test,dtwt,dtw1,tpc1,dl,iselv,maxelv,ip,dt,xfar,

     &            xnear,tph,aninc,trphs,iflagd,minflag1,data,'L',

     &            imatch,depthlim,scorr,ph,dx)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c                  write(iulst,'(1x,''2'',a4,1x,a4,1x,a4,3f10.1)'

c     &                       ,iostat=code)

c     &            st(i),st(j2),phase(j2),tp(i)-tp(j2),tpc1(j2),dltmin2

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



c 9/94: exclude case where tpc1(j2)=0.0 and delta>1000 km

                  if(dabs(tp(i)-tp(j2)).gt.tpc1(j2)+test(60)

     &            .and.dtwt(j2).gt.0.0.and.tpc1(j2).ne.0.0

     &            .and.dltmin2.lt.1000.)then

              

C               this test has failed too: the ith arrival is bad              

c               set its weight to 0 

                    write(iulst,*,iostat=code)' ',st(i),' ',phase(i),

     &              ' weighted out:',' inconsistent time:'

                    write(iulst,'(1x,''Arrival time difference from'',

     &              '' station '',a4,/,'' observed:'', f10.2,

     &              '' calculated: '',f10.2,'' delta: '',f10.1)'

     &              ,iostat=code)

     &              st(j2),dabs(tp(j2)-tp(i)),tpc1(j2),dltmin2

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c                   write(*,*)data(1)(1:78)

                    dataprev=data(1)

                    write(iulst,'(a4,1x,a8,'' weighted out:''

     &               ,'' inconsistent time'')')st(i),phase(i)

                    dtwt(i)=0.0

                    dt2(i)=-1.0

                  endif

                endif

                if(j3.gt.0)then



c                 calculate the travel time from st(j1) to st(j3)

                  iflagd=0

                  if(delta.gt.test(57).or.distind.eq.'D')iflagd=1

                  minflag1=1

                  imatch=0

c                 write(iulst,*,iostat=code)st(i),phase(i),st(j),

c     &            phase(j),dltmin

                  call trtime(j3,1,st,phase,tp,x0,y0,xh1,ndata,

     &            nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,

     &            test,dtwt,dtw1,tpc1,dl,iselv,maxelv,ip,dt,xfar,

     &            xnear,tph,aninc,trphs,iflagd,minflag1,data,'L',

     &            imatch,depthlim,scorr,ph,dx)

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c                  write(iulst,'(1x,''3'',a4,1x,a4,1x,a4,3f10.1)'

c     &                       ,iostat=code)

c     &            st(j1),st(j3),phase(j1),tp(j1)-tp(j3),tpc1(j3),dltmin3

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.



                  if(dabs(tp(j1)-tp(j3)).gt.tpc1(j3)+test(60)

     &             .and.dtwt(j3).gt.0.0.and.dltmin3.lt.1000.)then

              

C                  this test has failed too: the j1th arrival is bad              

c                  set its weight to 0 

                    write(iulst,*,iostat=code)' ',st(j1),' ',phase(j1),

     &              ' weighted out:',' inconsistent time:'

                    write(iulst,'(1x,''Arrival time difference from'',

     &              '' station '',a4,/,'' observed:'', f10.2,

     &              '' calculated: '',f10.2,'' delta: '',f10.1)'

     &               ,iostat=code)

     &              st(j3),dabs(tp(j3)-tp(j1)),tpc1(j3),dltmin3

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c                   write(*,*)data(1)(1:78)

                    dataprev=data(1)

                    write(iulst,'(a4,1x,a8,'' weighted out:''

     &               ,'' inconsistent time'')')st(j1),phase(j1)

                    dtwt(j1)=0.0

                    dt2(j1)=-1.0

                  endif

                endif

                if(dt2(i).ge.0.0.and.dt2(j1).ge.0.0.and.dltmin1

     &          .lt.1000.)then



c cannot distinguish bad arrival: weight both to zero

                  write(iulst,*,iostat=code)' ',st(i),' ',phase(i),

     &            ' weighted out:',' inconsistent time:'

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

                  write(iulst,*,iostat=code)' ',st(j1),' ',phase(j1),

     &            ' weighted out:',' inconsistent time:'

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

                  write(iulst,'(1x,''Arrival time difference from'',

     &            '' station '',a4,/,'' observed:'', f10.2,

     &            '' calculated: '',f10.2,'' delta: '',f10.1)'

     &            ,iostat=code)

     &            st(j1),dabs(tp(j1)-tp(i)),tpc1(j1),dltmin1

        call sei code( fort$, code, iulst, b_flag ) ! process the outcome.

c                 write(*,*)data(1)(1:78)

                  dataprev=data(1)

                    write(iulst,'(a4,1x,a8,'' weighted out:''

     &               ,'' inconsistent time'')')st(i),phase(i)

                    write(iulst,'(a4,1x,a8,'' weighted out:''

     &               ,'' inconsistent time'')')st(j1),phase(j1)

                  dtwt(i)=0.0

                  dt2(i)=-1.0

                  dtwt(j1)=0.0

                  dt2(j1)=-1.0

                endif  

              endif

            endif

          endif    

        end do

c       end phase loop ******************************************

      return

      end

