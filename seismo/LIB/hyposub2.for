cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
c
c       HYPOSUB2   version 4.0    6/98
c
cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
c
c changes:
c
c 6/92 (BRL) several fairly minor changes to getphasn
c 4/94: put common/comm1/ block in 'comm1.inc'
CJAB(BGS)Mar95 : Remove some causes of ieee_flags in routine "betai" 
cBRL Jun98  : few changes to limit conditions   
cBRL &/98: removed dx from window
c   sep 98   jh : ------------- seisan version 7.0 check --------------
c                 year 2000, 5 char station 
c  oct 28 98 bmt: linux changed, save and *
c  nov 19 99 jh: remove parm and nn form call window, add in common block
c  jan         : multi_model
c  jul 21 03 jh: small text change realted to Pg
c  jun 16 11 jh: limit number of iterations for direct ray to 200
c                there were cases of infinite loops
c  oct 4  12 jh: barry suggests changing range for  range=1.0e-7 to 1e-12
c                for better convergence
c 2014-03-20 pv: replaced PAUSE statement, not a fortran standard feature
c 2014-03-20 pv: replaced IF (Expression) Label1, Label2, Label3 statements, 
c                the Arithmetic IF is considered harmful.
c
      subroutine dtdx2(xh,x0,ptype,nmoho,nconrad,iulst,tmin,
     &dx,delta,ann,iflag,phsid)

C***********************************************************************
C
c   subroutine to calculate travel time and derivatives for a one-
c   dimensional nl-layered model
c
c       Barry R. Lienert    March, 1991
c
c   Updates:
c   Feb 17, 92 by jh: Fix negative sqrt, look for jh fix
C
c   May 27, 92 BRL: removed this whole section (increases
c                   calculation time a bit)               
c
c   April, 1994 BRL: added b phases
c
c   Adapted for variable station elevations Jan, 1987
c   can now use x0(3) with any positive value i.e. station
c   can be positioned in any layer. However the top of the velocity
c   model must then correspond to the highest elevation station IN THE
c   WHOLE ARRAY.
c
c   changed to work with geocentric lat/long instead of km 3/14/91
c   Lat./Long. are now in geocentric radians on a sphere having the
c   earth's MEAN radius (6371 km)
c
c   inputs:    nn:     number of parameters (=2*nl-1), from common via nl
c              parm(i),i=1,nl:     layer velocities in km/sec, from common
c              parm(i),i=nl+1,nn:  layer thicknesses in km,from common
c              xh(3):      event coordinates in geocentric radians
c                      (depth still in km)
c                      Depth origin is always the maximum elevation station
c              x0(3):     station coordinates in geocentric radians
c                      (depth in km). x0(3) origin is the maximum elevation station, i.e.,
c                      x0(3) is the 'depth' of the station below the maximum elevation one
c              ptype:  G        : direct arrival
c                      N        : moho refracted arrival                                  
c                      B        : Conrad refracted arrival
c                      blank    : minimum arrival 
c              nmoho:  layer# having moho velocity 
c              nconrad   "      "    conrad  "
c              iulst:  output listing unit# for error messages
c                      (if <= 0, no output)
c
c   outputs    tmin:   minimum travel time
c              dx(1):  spatial derivative of tmin wrt long. in secs/radian
c              dx(2):  spatial derivative of tmin wrt lat. in secs/radian
c              dx(3)   spatial derivative of tmin wrt depth in secs/radian
c                      (the depth derivative in secs/km is multiplied by 6367,
c                       the earth's mean radius, to keep it similar in size 
c                       to dx(1) and dx(2))
c              delta:  epicentral distance in km
c              ann:    angle (measured upward from vertical) at which
c                      ray leaves the source (< 90 <=> refracted)
c              iflag:  1 for valid phase, 0 otherwise
c              phsid:  8 char ID for phase calculated: 
c                      PG  or Pg for direct 
c                      PB  or Pb for Conrad diffraction
c                      PN2 for refraction # is layer#, if not moho
c                      left blank otherwise
c
c   subroutines called:
c
c              del :calculates horizontal offset for direct path
c                   - modified version for variable station depth
c
c 
c
C***********************************************************************
      save
      include 'hypparm.inc'  
c      real dx(3)
      character*1 ptype
      character*8 phsid
      dimension dcrit(nlayer)
      double precision t1,t2,uu,u1,u2,dtan,datan,dsqrt,del,dl1,dl2,dll,
     +p,p1,p2,dabs,dble,pi,degtorad

c added this to prevent overflows in atan2
      if(abs(xh(3)-x0(3)).lt.0.001)xh(3)=x0(3)

c     write(*,*) ' debug dtdx2 '
      pi=3.14159265d0
      degtorad=pi/180.d0
      rearth=6371.
      eps=0.01d0
      sthi=0.0
      cthi=1.0
      dx(1)=0.0
      dx(2)=0.0

c    nl=no of layers

c nov 99      nl=(nn+1)/2
      nn=2*nl-1     ! new nov 99 jh
      ne2=1
      ne1=1
      eta2=x0(3)
      if(eta2.eq.0.0)eta2=0.01 !added 6/98 brl
      tmin=1.e22
      if(nl.eq.1)then
        ne1=1
        go to 59
      endif

c    find find layer, ne2, that station is in

      sum=0.0

      do 3 i=1,nl-1
        sss=parm(nl+i)
        sum=sum+sss
        if(x0(3).le.sum)go to 4
3     continue
      sss=0.0
      i=nl
4     ne2=i

c    eta2 is the depth to the station from the top of this layer

      eta2=x0(3)-sum+sss

C    IF JUST AT OR BELOW BOUNDARY, PLACE 1 METER ABOVE (added 6/98, brl)
c
      if(eta2.lt.0.001.and.ne2.ne.1)then
       eta2=eta2-parm(nl+ne2)-0.001
       ne2=ne2-1
      endif    

c   now find layer, ne1, that event is in

      sum=0.0

      do 1 i=1,nl-1
        sss=parm(nl+i)
        sum=sum+sss
        if(xh(3).le.sum)go to 2
1     continue

      sss=0.0
      i=nl
2     ne1=i

c    eta1 is the depth to the event from the top of this layer

      eta1=xh(3)-sum+sss

c    if eta1 < 0.001, set ne1 to previous layer 

      if(abs(eta1).lt.0.1.and.ne1.ne.1)then !changed eta1 to abs(eta1) 6/98, brl
         eta1=eta1+parm(nl+ne1)
         ne1=ne1-1
c        eta1=parm(nl+ne1)+eta1*parm(ne1+1)/parm(ne1)
      endif
      na=ne2
      ea=parm(nl+ne2)-eta2
      eb=eta1
      nb=ne1

c    station is below hypocenter

      if(ne2.gt.ne1)then
        nb=ne2
        ea=parm(nl+ne1)-eta1
        na=ne1
        eb=eta2
      endif

59    continue

c     use J. Lahr's routine to get delta from geocentric distances
c     azz0 is the azimuth of the station from the epicenter

      call delaz(x0(2),x0(1),delta,dedeg,azz0,xh(2),xh(1))
      delta=abs(delta)
c      write(iulst,*)' Delta = ',delta
      dedeg=abs(dedeg)

c    cfact is local conversion factor between km and radians in rad/km

      cfact=1./rearth
c      cfact=sngl(degtorad)*dedeg/delta
      dellat=delta*cos(azz0*sngl(degtorad))
      dellon=cos(abs(xh(2)))*delta*sin(azz0*sngl(degtorad))
      dist=sqrt(delta**2+(xh(3)-x0(3))**2)

c*************************************************************************
c                       trivial cases

c  1.  ptype='N' and no refractions

      if(ptype.eq.'N'.and.ne1.ge.nmoho)then
        if(iulst.gt.0.and.(.not.multi_model))
     *  write(iulst,*)' dtdx2 error: event below moho - ',
     &  'N phase not possible'
        iflag=0
        ann=0.0            
        phsid='        '
        return
      endif

      if(ptype.eq.'B'.and.ne1.ge.nconrad)then
        if(iulst.gt.0.and.(.not.multi_model))
     *  write(iulst,*)' dtdx2 error: event below Conrad',
     &  ' - B phase not possible'
        phsid='        '
        iflag=0
        ann=0.0
        return
      endif

      if(nl.eq.1.and.ptype.eq.'N')then
        if(iulst.gt.0)write(iulst,*)'dtdx2 error: N phase not possible',
     &  ' in a single layer'
        phsid='        '
        iflag=0
        ann=0.0
        return
      endif

      if(nl.eq.1.and.ptype.eq.'B')then
        if(iulst.gt.0)write(iulst,*)'dtdx2 error: B phase not possible',
     &  ' in a single layer'
        phsid='        '
        iflag=0
        ann=0.0
        return
      endif

      if(dist.lt.0.001)then

c  2. event coincident with station

        do 17 i=1,2
17      dx(i)=-1.0/(cfact*parm(ne1))

        dx(1)=dx(1)*cos(abs(xh(2)))
        dx(3)=rearth/parm(ne1)

c  incidence angle set to zero (???) for this case

        phsid='        '
        ann=0
        iflag=1
        if(ptype.eq.'N'.or.ptype.eq.'B')then
            iflag=0
          if(iulst.gt.0)write(iulst,*)' dtdx2 error: event coincident',
     &    ' with station - no refracted phase'
          phsid='        '
          iflag=0
          return
        endif
        phsid='        '
        iflag=1
        tmin=0.0
        ann=0.0
        return
      endif

      if(delta.lt.0.001)go to 24


c  3.  single layer case: only direct wave possible

      if(nl.eq.1)then
        tmin=dist/parm(1)
        dx(1)=-dellon/(cfact*dist*parm(1))
        dx(2)=-dellat/(cfact*dist*parm(1))
        dx(3)=(xh(3)-x0(3))*rearth/(dist*parm(1))
        if(xh(3).ne.x0(3))then
          dd=atan(delta/(xh(3)-x0(3)))/sngl(degtorad)
          if(dd.ge.0.0)then
            ann=180.0-dd
          else
            ann=abs(dd)
          endif
        else
          ann=90.0
        endif
        phsid(2:8)='G      '
        iflag=1
        return
      endif

      if(ptype.eq.'G')go to 66

c  4.  station or event in bottom layer - no refractions

      if(ne1.eq.nl.or.ne2.eq.nl)then
        if(ptype.eq.'N'.or.ptype.eq.'B')then
          if(iulst.gt.0)write(iulst,*)' dtdx2 error: refracted phase',
     &    ' not possible for event in bottom layer'
          phsid='        '
          iflag=0
          ann=0.0
          return
        else
          go to 66
        endif
      endif
c*******************************************************************

c   nep1 is the shallowest layer in which refraction can occur

      nep1=ne1+1
      if(ne2+1.gt.nep1)nep1=ne2+1
c      write(iulst,*)'nep1,nl,ne1,ne2,nmoho',nep1,nl,ne1,ne2,nmoho
      
      if(ptype.eq.'N'.and.nep1.ge.nl)then
        if(iulst.gt.0)write(iulst,*)' dtdx2: no N phase',
     &  ' for event below Moho'
        phsid='        '
        iflag=0
        ann=0.0
        return
c   changed 4/94 to include 'B'
      elseif(ptype.eq.'B'.and.nep1.gt.nconrad)then
        if(iulst.gt.0)write(iulst,*)' dtdx2: no B phase',
     &  ' for event below Conrad'
        phsid='        '
        iflag=0
        ann=0.0
        return
      endif
      
      if(delta.eq.0.0)go to 24
      if(ptype.eq.'N'.and.(ne1.eq.nl.or.ne2.eq.nl))then
        if(iulst.gt.0)write(iulst,*)' dtdx2: no N phase',
     &  ' when event or station is in bottom layer'
        phsid='        '
        iflag=0
        ann=0.0
        return
      elseif(ptype.eq.'B'.and.(ne1.gt.nconrad.or.ne2.gt.nconrad))then
        if(iulst.gt.0)write(iulst,*)' dtdx2: no B phase',
     &  ' when event or station is in or below Conrad'
        phsid='        '
        iflag=0
        ann=0.0
        return
      endif

c  find the refracted wave travel times

c  kmin is the fastest refraction layer, tmin is its travel time

      kmin=0

c   find critical distances, dcrit(k), for all refracted waves. if no
c   refracted wave is possible, set dcrit(k)=-1.0

      n1=ne2

c  need this section for when station is below hypocenter

      if(ne2.gt.ne1)n1=ne1
      n2=ne1-1
      if(ne2.gt.ne1)n2=ne2-1

c 4/94: added for 'B' case
      if(ptype.eq.'B')n1=nconrad

      do 50 k=nep1,nl
        sum=0.0
        do 40 i=nep1-1,k-1
          if(parm(k).le.parm(i))go to 49
40      sum=sum+parm(nl+i)*2./sqrt((parm(k)/parm(i))**2-1.)

        if(parm(k).le.parm(ne1).or.parm(k).le.parm(ne2))go to 49
        dcrit(k)=sum-eta1/sqrt((parm(k)/parm(ne1))**2-1.)
     &  -eta2/sqrt((parm(k)/parm(ne2))**2-1.)

        if(ne1.ne.ne2)then
          do 42 i=n1,n2
            if(parm(k).le.parm(i))go to 49
42        dcrit(k)=dcrit(k)+parm(nl+i)/sqrt((parm(k)/parm(i))**2-1.)
        endif
c        write(iulst,*)'k,dcrit,delta',k,dcrit(k),delta
        if(dcrit(k).le.delta)go to 50
49      dcrit(k)=-1.0
50    continue


c   find the travel times, t, for all possible refracted waves

      do 60 k=nep1,nl
        if(dcrit(k).lt.0.0)go to 60
        sum=0.0

        do 52 i=nep1-1,k-1
          eee=parm(nl+i)*2.
52      sum=sum+eee*sqrt(1./(parm(i)*parm(i))-1./(parm(k)*parm(k)))

        if(parm(ne1).gt.parm(k))go to 60
        t=delta/parm(k)-eta1*sqrt(1./(parm(ne1)*parm(ne1))
     &  -1./(parm(k)*parm(k)))+sum-eta2*sqrt(1./(parm(ne2)*parm(ne2))
     &  -1./(parm(k)*parm(k)))

        if(ne1.ne.ne2)then
          do 53 i=n1,n2
53        t=t+parm(nl+i)*sqrt(1./(parm(i)*parm(i))-1./1/(parm(k)*
     &    parm(k)))
        endif

c   if t < tmin, set tmin = t

        if(t.lt.tmin)then
          kmin=k

c added output id 4/94
          if(k.eq.nconrad)phsid(2:8)='B      '
          if(k.eq.nmoho)phsid(2:8)='N      '

          tmin=t
        endif

c    N phase: if k = nmoho  then exit

        if(ptype.eq.'N'.and.nmoho.eq.k)then
          phsid='N'
          kmin=k
          tmin=t
          go to 62
        endif

c    B phase: if k = nconrad  then exit

        if(ptype.eq.'B'.and.nconrad.eq.k)then
          phsid='B'
          kmin=k
          tmin=t
          go to 62
        endif

60    continue

c     exit if ptype='N' or 'B' and  no refracted wave is possible

      if(ptype.eq.'N'.or.ptype.eq.'B'.and.kmin.eq.0)then
          if(iulst.gt.0)write(iulst,*)' dtdx2: refracted phase',
     &    ' not possible, delta < critical distance' 
        phsid='        '
        iflag=0
        ann=0.0
        return
      endif

66    continue

c    Direct travel times


c    Trivial cases: (i) same depth

      if(abs(xh(3)-x0(3)).lt.0.001)go to 36

c                   (ii) station and event in same layer

      if(ne1.eq.ne2)then
        td=dist/parm(ne1)
        if(td.lt.tmin.or.ptype.eq.'G')then
          tmin=td
          dx(1)=-dellon/(cfact*dist*parm(ne1))
          dx(2)=-dellat/(cfact*dist*parm(ne1))
          dx(3)=(xh(3)-x0(3))*rearth/(dist*parm(ne1))
          if(xh(3).ne.x0(3))then
            dd=atan(delta/(xh(3)-x0(3)))/sngl(degtorad)
            if(dd.ge.0.0)then
              ann=180.-dd
            else
              ann=abs(dd)
            endif
          else
            ann=90.0
          endif
          phsid(2:8)='G      '
          iflag=1
          return
        endif
      endif


c   calculate the direct path travel time, td

c    find tangents of maximum and minimum takeoff angles,t1 and t2

      et=eta1
      ne=ne1
      if(ne2.gt.ne1)et=eta2
      if(ne2.gt.ne1)ne=ne2
      if(ne1.ne.ne2)go to 39
      if(xh(3).eq.x0(3))go to 36
      td=dist/parm(ne1)
      thi=atan(delta/abs(xh(3)-x0(3)))
      cthi=cos(thi)
      sthi=sin(thi)
      go to 37
39    continue
      t1=dble(delta)/dble(abs(xh(3)-x0(3)))
      if(et.eq.0.0)et=1.e-30               !added 6/98, brl
      t2=dble(delta)/dble(et)
      u1=dble(et)*t1
      u2=dble(delta)

c   find raypath parameters, p1 and p2 for direct waves

      p1=1.d0/(dsqrt(1.d0+1.d0/(t1*t1))*dble(parm(ne)))
      p2=1.d0/(dsqrt(1.d0+1.d0/(t2*t2))*dble(parm(ne)))
      p=p1
      sthi=sngl(p1)*parm(ne)
      cthi=sqrt(1.-sthi*sthi)

c   find corresponding distances, dl1 and dl2 for the 2 rays

      dl1=del(p1,eta1,ne1,eta2,ne2,nn,parm)
      if(dabs(dl1-dble(delta)).le.eps)go to 30
c
cjh  make sure no more than 200 iterations, jh 16.6.11
c
      kkk=1
41    if(dl1.le.dble(delta))go to 43
      t1=dtan(2.d0*datan(t1)-datan(dl1/dble(abs(xh(3)-x0(3)))))
      p1=1.d0/(dsqrt(1.d0+1.d0/(t1*t1))*dble(parm(ne)))
      dl1=del(p1,eta1,ne1,eta2,ne2,nn,parm)
      kkk=kkk+1
      if(kkk.gt.200) goto 43
      go to 41
43    continue
      dl2=del(p2,eta1,ne1,eta2,ne2,nn,parm)
      if(dl1.eq.dl2)go to 30
      u1=dble(et)*t1
      u2=dble(delta)

c   now find takeoff angle thi and distance delta1 of a ray p which
c   lies between p1 and p2

      ii=0
10    uu=u1+(u2-u1)*(dble(delta)-dl1)/(dl2-dl1)
      ii=ii+1
      if(dabs(uu).gt.1.d100)uu=1.d100*uu/dabs(uu)    !added 6/98, brl
      p=uu/(dble(parm(ne))*dsqrt(uu*uu+dble(et)*dble(et)))
      sthi=sngl(p)*parm(ne)
      cthi=sqrt(1.0-sthi*sthi)
      dll=del(p,eta1,ne1,eta2,ne2,nn,parm)

c   iterate until abs(delta-dll)<eps

      if(dabs(dll-dble(delta)).le.eps)go to 30
      if(dabs(dl1-dble(delta)).gt.eps)go to 770
      dll=dl1
      uu=u1
      go to 30
770   if(dabs(dl2-dble(delta)).gt.eps)go to 771
      dll=dl2
      uu=u2
      go to 30
771   if(ii.gt.50)go to 26
      if(dll.gt.dble(delta))go to 20
      dl1=dll
      if(dl1.eq.dl2)dl1=dl1-.001     !added 6/98, brl
      u1=uu
      go to 10
20    dl2=dll
      u2=uu
      go to 10
26    continue
      if(iulst.gt.0)then
        write(iulst,100)
        write(iulst,101)delta,dll
100     format(' no direct path convergence after 50 iterations')
101     format(' delta = ',f12.4,' dll = ',f12.4)
      endif
      phsid='        '
      iflag=0
      ann=0.0
      return

24    p=0.0d0
30    continue

c   calculate the travel time,td,for the direct path

      td=0.0

      do 35 i=na,nb
        pm=parm(nl+i)
        if(i.eq.na)pm=ea
        if(i.eq.nb)pm=eb
        if(dble(parm(i))*p.ge.1.0d0)go to 38
35    td=td+pm/(parm(i)*sqrt(1.-p*p*parm(i)*parm(i)))
      go to 37
38    continue
      if(iulst.gt.0)then
        write(iulst,'(''dtdx2 error: p > 1/velocity in layer '',
     &  i3,'' p = '',f6.4)')i,p
      endif
      phsid='        '
      iflag=0
      ann=0.0
      return

36    td=delta/parm(ne1)
      cthi=0.0
      sthi=1.0

37    if(td.gt.tmin.and.ptype.ne.'G')go to 62

c    set minimum travel time to td

      tmin=td

c   direct path derivatives

      dx(1)=0.0
      dx(2)=0.0
      if(delta.lt.0.001)go to 69
      dx(1)=-dellon*sthi/(cfact*delta*parm(ne1))
      dx(2)=-dellat*sthi/(cfact*delta*parm(ne1))
69    dx(3)=cthi*rearth/parm(ne1)
      if(x0(3).gt.xh(3))dx(3)=-dx(3)
      if(cthi.ne.0.0)then
        ann=180.-abs(atan(sthi/cthi))/sngl(degtorad)
      else
        ann=90.0
      endif
      phsid(2:8)='G      '
      iflag=1
      return

c   refracted path is minimum - find spatial derivatives of tmin and exit

62    k=kmin
      dx(1)=-dellon/(cfact*delta*parm(k))
      dx(2)=-dellat/(cfact*delta*parm(k))
c 4/94: added section to write # of refraction layer
c        if(k.eq.nmoho)then
c         phsid(2:8)='N      '
c        elseif(k.eq.nconrad)then
c         phsid(2:8)='B      '
c        else
         phsid(2:8)='N      '
         if(k.lt.10)then
          write(phsid(3:3),'(i1)')k 
         elseif(k.lt.100)then 
          write(phsid(3:4),'(i2)')k 
         elseif(k.lt.1000)then 
          write(phsid(3:5),'(i3)')k 
         elseif(k.lt.10000)then 
          write(phsid(3:6),'(i4)')k 
         endif
c        endif                                       
      if(parm(k).lt.parm(ne1))then
        if(iulst.gt.0)then
          write(iulst,'(''dtdx2 error:'',
     &     '' Illegal velocity inversion '')')
          write(iulst,500)ne1,kmin,xh(3),parm(ne1),parm(k)
        endif
500     format(2i8,3f12.4)
        phsid='        '
        ann=0.0
        iflag=0
        return
      endif
      dx(3)=-rearth*sqrt(1./(parm(ne1)*parm(ne1))-1./(parm(k)
     &*parm(k)))
      dd=sqrt(parm(k)*parm(k)-parm(ne1)*parm(ne1))
      if(dd.ne.0.0)then
        ann=atan(parm(ne1)/dd)/sngl(degtorad)
      else
        ann=90.0
      endif
      iflag=1
      return

      end

      subroutine azdist(alat1,alon1,az,alat2,alon2,dist,alata,alona,
     &alatb,alonb,nsol)
c************************************************************************
c   Intersection of one distance and one azimuth at different stations
c
c  finds the latitudes/longitudes of the intersections of a distance, dist,
c  from a point at (alat2,alon2) and a line having azimuth az (deg)
c  through a point at (alat1,alon1). Solved for plane case, but works
c  roughly (100 km error at delta=20 deg) on a sphere
c
c  nsol = no of intersections
c  (alata,alona) is the first intersection, (alatb,alonb) the second
c     - the latter only valid when nsol=2
c
c                   Barry R. Lienert  1991
c***********************************************************************
          save
          degtorad=0.0174533
          nsol=0
          x1=alon1*degtorad
          y1=alat1*degtorad
          y2=alat2*degtorad
          x2=alon2*degtorad
          call delaz(y2,x2,dekm,dedeg,azz,y1,x1)
c          write(*,*)' dekm azz ',dekm,azz
          theta=azz-az
          d=abs(dist*sin(theta*degtorad))
          if(d.gt.dist)return
          del=sqrt(dist*dist-d*d)
          delt1=dekm*cos(theta*degtorad)
c          write(*,*)' del delt ',del,delt1
          if(delt1.ge.0.0)then
            if(delt1+del.ge.0.0)then
              nsol=1
              delt=(delt1+del)/(6371.*degtorad)
              call latlon(alat1,alon1,delt,az,alata,alona)
            endif
            if(delt1-del.ge.0.0)then
              nsol=nsol+1
              delt=(delt1-del)/(6371.*degtorad)
              call latlon(alat1,alon1,delt,az,alatb,alonb)
            endif
          else
            del=dekm/dist
            arg=1.0+del*del-2.0*del*cos(theta*degtorad)
            if(arg.lt.0)return
            nsol=1
            r=dist*sqrt(arg)
            delt=r/(6371.*degtorad)
            call latlon(alat1,alon1,delt,az,alata,alona)
          endif
          return
          end

      subroutine minv(m,n,np,dt1,di,dtw1)

C****************************************************************************
C
c  inversion subroutine: finds svd of partial derivative matrix, g1
C
c                   Barry R. Lienert  1991
C
c this routine is independent of damping and only needs to be performed
c when a new step is taken. For the adaptive dampi8ng technique, used in
c HYPOCENTER, this is only when the rms decreases. Consequently, it uses
c the minimum rms partial derivative matrix, g1(i,j)

C***************************************************************************
C
c   inputs:     g1 = partial derivative matrix
c               n  = no of rows in g1 (i.e., no of data)
c               m  = no of columns in g1 (i.e., no of parameters)
c               dt1 = data residuals          
c               dtw1 = data weights
C
c   outputs:    ve    = (mxm) eigenvector matrix of gt*g
c               u     = (nxn)    "     "     "      g*gt
c               di    = (nx1) data importances
C
C***************************************************************************

c modified 9/94 to calculate information matrix, si=uTu, also
c changed to structured format
C
      save
      include 'hypparm.inc'
c      include 'comm1.inc'
c      dimension dt1(*),dtw1(*),di(*)

c  first find g1t*g1 and put it in a in correct format for eigen
      ic=1

      do j=1,m
        do k=1,j
          sum=0.0
          do kc=1,n
            g3=g1(kc,j)
            g2=g1(kc,k)
            sum=sum+g3*g2
          enddo  
          a(ic)=sum
          ic=ic+1
        enddo
      enddo    

c      do i=1,n
c       write(6,'(4e10.3)')(g1(i,j),j=1,m)
c      enddo

c get eigenvalues (diag(a)) and eigenvectors (ve)
      call eigen(a,ve,m,0)

      sum=0.0
      ii=1
      np=0

      do i=1,m
c     sum=sum+dabs(a(ii))
        sum=sum+abs(a(ii))

c 6/94 changed 1e-7 to 1e-20
        if(a(ii).gt.1.0e-20)then

          np=np+1
        endif

C       eig(i)=dabs(a(ii))
        eig(i)=abs(a(ii))
        if(eig(i).ne.0.0)then
         sgn(i)=a(ii)/eig(i)
        endif
        ii=ii+i+1
      enddo  

      if(np.lt.m)m=np

c   now find eigenvectors of g1*g1t, u
      do i=1,np
        do j=1,n
          sum=0.0
          do k=1,m
            ii=(i-1)*m+k
            f=1.0
            gg=g1(j,k)
            sum=sum+sgn(i)*ve(ii)*gg
          enddo  
          u(j,i)=sum/sqrt(abs(eig(i)))
        enddo
      enddo                        
      
c 9/94: data importance calculation added      
      do i=1,n       
       di(i)=0.0
       if(dtw1(i).gt.0.0)then
        do j=1,np
         di(i)=di(i)+u(i,j)*u(i,j)
        enddo
        
c normalize by # data and make it %        
        di(i)=di(i)*100./float(np) 
        
       endif 
      enddo
        
c  eigenvalues of g = sqrt(eigenvalues of g1t*g1
      do i=1,m
       eig(i)=sqrt(eig(i))
      enddo 

c  determine  alpha=vt*dt (Jackson, 1972)
      do j=1,np
       alpha(j)=0.0
       ax(j)=0.0
      enddo 

      do k=1,n
        do j=1,np
          aa=dt1(k)
          alpha(j)=alpha(j)+u(k,j)*aa/eig(j)
          ax(j)=ax(j)+u(k,j)*aa
        enddo
      enddo    

      return
      end

c************************************************************************
      subroutine corr(m,n,np,xk,res,test,oterr,distind,iflagd)
c
c   subroutine to calculated damped least squares parameter corrections
c   and variances using SVD eigenvalues and eigenvectors
c
c             Barry R. Lienert  1991
c
c  reference: 'interpretation of inaccurate, insufficient and inconsistent
c              data'.  D.D. Jackson, Geophys. J. Roy. Astr. Soc., 28, 97-109,
C              1972
C
c        m is the number of columns in the SVD matrix
c        n is the number of rows
c        xk is the damping factor
c        res is their squared weighted sum
c
c        np (output) is the number of eigenvectors used
c
c************************************************************************
c
      save
      include 'hypparm.inc'
c      include 'comm1.inc'
      double precision dble             
c      dimension test(200)
c      character*1 distind

c change 10/93: prevents divide by zero in some situations
      ndeg=0
      do i=1,3

c 6/94 changed 1e-7 to 1e-20
        if(eig(i).gt.1.e-20)ndeg=ndeg+1
        dth(i)=0.0
      end do
        
      if(np.gt.ndeg)np=ndeg

c changed 9/94 to use Bayesian statistics and F-distribution      
c local event degs freedom & error
      degfb=test(86)    !changed test(87) to test(86) 6/98 BRL
      errb=test(85) 

c distant case
      if(distind.eq.'D'.or.iflagd.eq.1)then  !changed 6/98, brl
       degfb=test(92)
       errb=test(91)
      endif                 
      
      ndeg=int(degfb+0.499999)
c  changed brl, 6/98
      cres=(degfb*errb**2+res)/(degfb+float(n)-float(np+1))
c      cres=(degfb*errb**2+res)/(test(86)+float(n)-float(np+1))

c use F-distribution to get confidence interval for residuals  
c corrected an error to include the total degrees of freedom, np+1 6/98 BRL
      rs=float(np+1)*cres*fdist(np+1,ndeg+n-np-1,test(87))
c      rs=cres*fdist(np+1,ndeg+n-np-1,test(87))
      
c 10/24/94: added origin time error. Same as confidence interval
c           for residuals
      oterr=sqrt(rs)      

      do i=1,m
        dth(i)=0.0
        do j=1,np
          ii=(j-1)*m+i
          f1=0.0
          f2=0.0
          sx=ss1(i)
c         write(6,*)ve(ii),eig(j),alpha(j)
          f2=sngl(dble(ve(ii)*alpha(j))/(1.0d0+dble(xk)/dble
     &    (eig(j))**2))
          dth(i)=dth(i)+f2
        end do

c changed 8/94 to calculate complete unscaled covariance matrix var(i,j)
c common block comm1.inc changed to accomodate this
        do j=1,m
          var(i,j)=0.0
          do k=1,np
            ii=(k-1)*m+i
            jj=(k-1)*m+j
            if(ss1(i).gt.0.0.and.ss1(j).gt.0.0.and.eig(k).gt.0.0)then
             f1=rs*ve(ii)*ve(jj)/((eig(k)+xk/eig(k))**2*ss1(i)*ss1(j))
             var(i,j)=var(i,j)+f1
            endif 
          end do
        end do    
      end do  

      return
      end

c BRL 6/98: added ips check
      subroutine median(a,wt,phase,n,am,ips)
      save
      dimension a(*),wt(*),ips(*)
      character*8 phase(*)
      no=n/2+1
      am=a(1)
      do 10 i=1,n
      if(wt(i).le.0.0.or.phase(i)(1:2).eq.'AZ'.or.ips(i).eq.1)go to 10
      if(a(i).gt.am)am=a(i)
10    continue
      do 50 i=1,n
      if(wt(i).le.0.0)go to 50
      knt=0
      do 40 j=1,n
      if(wt(i).le.0.0)go to 40
      if(a(j).le.a(i))knt=knt+1
40    continue
      if(knt.ge.no.and.a(i).lt.am)am=a(i)
      if(knt.eq.no)go to 60
50    continue
60    return
      end

c**************************************************************************
c Get a set of phases in Nordic format<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c change 10/93: removed iunit1 and init from window
c change 3/94 added narriv and check that its not exceeded
      
      subroutine getphasn(nphase,iazflag,st,phase,prm1,prm2,prm3,ip,
     &tp,coda,fixdep,depth,fixepi,xlat,xlon,orgsec,fixor,iulst,data,
     &test,distind,narriv1,imap,xmag,magtype,oldrms,appar_veloc)
c
c   after change nov 23, iunit1 is not used, replaced by array data	 
c
c  BRL 6/92   Added test parameters to window
c             Now only use phase data if apparent veloc.< test(58)
c             distind added to window
c
c 11/92 BRL added orgsec to window
c 4/94: added fixor to window              
c 9/94: added xmag & magid to window              
c 10/94: added oldrms                                          
c 6/98: added appar_veloc
c
C***************************************************************************
c
c  Updates:
c  May 21 -91  C.L. : Possibility to select start location facilitated
c                     by use of test(57). Nordic format changed
C  NOV 23 91   J.H. : change to new nordic format, line indicator on same line
c                     use data array data instead of read
c  May 92      BRL  : added comments to make termination conditions clearer
c  April 94    BRL   : changed to read 8 char phase ID's if col 6 is not blank
c                      (col 6 is then the WEIGHT)
c******************************************************************************
c
c      get a set of phases - Nordic format

      save
      include 'hypparm.inc'
c      include 'param.inc'                     ! added 6/98
      double precision secs,orgsec
      character*8 phased
      character*4 type   
      character*5 depth,azimuth
      character*1 prm1(narriv),prm2(narriv),prm3(narriv),
     & fixepi,expl,fixdep,
     &cont,pp,magtype
      dimension coda(*)
      type='    '
      fmp1=0.0
      nphase=0

c     i is the phase counter

 10   i=1

c
c   counter for record to read in data array
c
      krecord=1

c     read header record

c change BRL 1/94: 43x changed to 34x 
c 4/94: added fixor in col 11
c added imap to record position in data array
c 9/94: added magnitude, changed f5.2 to f4.1
c 10/94: added oldrms
c2000      read(data(1),'(3x,i2,1x,2i2,a1,2i2,1x,f4.1,1x,2a1,f7.3,f8.3,a5,a1,
      read(data(1),'(1x,i4,1x,2i2,a1,2i2,1x,f4.1,1x,2a1,f7.3,f8.3,a5,a1,
     &a1,6x,f4.1,f3.0,a1,19x,a1)',end=99,err=15)iyr,imnth,idy,fixor,
     &iohr,iomin,orsec,distind,expl,xlat,xlon,depth,fixdep,fixepi,
     &oldrms,xmag,magtype,cont
c      write(iulst,*)data(1)
      if(iulst.gt.0.and.(.not.multi_model))then
        write(iulst,'(a80)')data(1)
c        write(iulst,'(3x,i2,1x,2i2,a1,2i2,f5.2,1x,2a1,f7.3,f8.3,a5,
c     &  a1,a1)')iyr,imnth,idy,fixor,iohr,iomin,orsec,distind,expl,
c     &  xlat,xlon,depth,fixdep,fixepi
      endif

c calculate header origin time, orgsec

      call timsec(iyr,imnth,idy,iohr,iomin,orsec,orgsec)
      go to 815

c change BRL 12/29/93: print header if error
 15   continue
      write(iulst,*)' getphasn: Error in header record:'
      write(iulst,*)data(1)
815   continue
c      write(iulst,*)data(krecord)(1:20),krecord,i,imap(i)
      krecord=krecord+1                                   
      

c  skip over headings

      read(data(krecord),200,end=99)cont
200   format(79x,a1)
c      write(iulst,*)data(krecord)(1:20),krecord,i,imap(i)

c  skip over non-phase records

c change BRL 1/94: changed 15 to 815
      if(cont.ne.' '.and.cont.ne.'4')go to 815

c    read the phase 

c change BRL 1/94: print message if error in phase record
 20   read(data(krecord),100,end=99,err=166) 
     &st(i),pp,prm1(i),phased,
     &ihrr,imn,tsec,icoda,azimuth,veloc,cont
 100  format(1x,a5,2x,2a1,a8,2i2,f6.1,1x,i4,13x,a5,f5.1,
     &23x,a1)
      
c change 4/94 to read 8 char phase ID if col 6 not blank
      if(pp.eq.' ')then
       pp=phased(5:5)
       prm3(i)=phased(7:7)

c set remaining 4 chars to blank in normal case
c       phased(5:8)='    '
      else

c set first motion to ' ' for 8 char phase ID
       prm3(i)=' '                           
       
      endif
       
      imap(i)=krecord
      go to 816
166   continue
      write(iulst,*)' getphasn: Error in phase record:'
      write(iulst,*)data(krecord)

c 7/94: added this section here, since error record gets processed - set weight to 4 
      pp='4'
      imap(i)=krecord
816   continue

c modifification (BRL 6/13/92) to trap error due to additional
c phase letters in weight column - previously this caused the
c phase to be rejected

      read(pp,'(i1)',err=88)ip(i)
      
c change:10/94: reset weights>4 to 4
      if(ip(i).gt.4.and.ip(i).ne.9)ip(i)=4
            
      go to 89
88    ip(i)=0
89    krecord=krecord+1 

c this prints the input phases in the output print file: enabled by test(74)=1 4/94
      if(iulst.gt.0.and.test(74).ne.0.0.
     *and.(.not.multi_model))then

c 10/94: changed position of pp & removed prm3(i)
        if(st(i).ne.'     ')write(iulst,100) st(i),pp,prm1(i),phased,
     &  ihrr,imn,tsec,icoda,azimuth,veloc,cont

      endif

c blank record signifies end of this phase set

      if(st(i).eq.'     ')then
        nphase=i-1
c        init=2
c        if(nphase.eq.0)init=-1
        return
      endif

c     do 30 k=1,4
c30   phased(k:k)=ucase(phased(k:k))

      call timsec(iyr,imnth,idy,ihrr,imn,tsec,secs)
      tp(i)=secs
      phase(i)=phased
      prm2(i)=phased(1:1)
      coda(i)=float(icoda)                         
      appar_veloc(i)=veloc     !added 6/98

c    treat azimuth as a separate phase - return it in sec(i)
c
c   why veloc not eq 0.0, commented out by jh
c   changed to test for both azimuth and velocity non-zero (BRL 92)

c     if(azimuth.ne.'    '.and.iazflag.eq.1.and.veloc.lt.test(58))then
c      write(*,*)azimuth,iazflag
        read(azimuth,'(f5.1)')azz

c -1 test added 6/92 BRL

      if(azimuth.ne.'     '.and.iazflag.eq.1.and.azz.ne.-1..and.veloc.
     &lt.test(58).and.(veloc.ne.0..or.azz.ne.0.)) then
        i=i+1
        phase(i)='AZ      '
        st(i)=st(i-1)
        read(azimuth,'(f5.1)')tp(i)
        prm1(i)=prm1(i-1)
        prm2(i)=prm2(i-1)
        prm3(i)=prm3(i-1)
        imap(i)=imap(i-1)
        appar_veloc(i)=appar_veloc(i-1)
        
c give azimuth the same weight as its parent phase
c 10/94: set=0 if difference phase
        if(ip(i-1).ne.9)then
         ip(i)=ip(i-1)
        else
         ip(i)=0
        endif  
      endif

c  bump the phase count and get another record

      i=i+1

c added 3/94 to test for exceeding maximum arrivals      
      if(i.gt.narriv1)then
       write(*,*)'Maximum arrivals exceeded - edit hypparm.inc and reco'
     & ,'mpile sources'
       stop
      endif  

      go to 20
c      nphase=i-1
c      init=2
c      if(nphase.eq.0)init=-1
c      return

c       end of file

99    nphase=i-1
c      init=-1
      return

98    write(iulst,'('' Phase record out of sequence'')')
      write(*,'('' Phase record out of sequence'')')
      go to 10

97    write(iulst,'('' Header record out of sequence'')')
      write(*,'('' Header record out of sequence'')')
      go to 20

      end

c**************************************************************************
c   subroutine to calculate the direct path horizontal displacement for an
c   n-layered one-dimensional model
c
c      Barry R. Lienert    October, 1982
c      modified for variable elevation stations 2/17/87
c
c   inputs:   p       raypath parameter = sin(theta)/velocity
c             eta     depth to event from the top of the layer
c                     it is in
c             n       layer event is in
c             eta1    depth to station from top of the layer
c                     it is in
c             n1      layer station is in
c             nn      no of parameters
c             parm(i) i=1,nn   layer velocities, then thicknesses
c
c*******************************************************************
      double precision function del(p,eta,n,eta1,n1,nn,parm)
      save
      dimension parm(*)
      double precision e,p
      nl=(nn+1)/2
      del=0.0d0
      na=n1
      ea=parm(nl+n1)-eta1
      nb=n
      eb=eta
      if(nb.ge.na)go to 5
      na=n
      ea=parm(nl+n)-eta
      nb=n1
      eb=eta1
5     continue
      do 10 i=na,nb
         e=dble(parm(nl+i))
         if(i.eq.na)e=dble(ea)
         if(i.eq.nb)e=dble(eb)
         if(dble(parm(i))*p.ge.1.0d0)go to 11
10    del=del+e/dsqrt(1.0d0/(dble(parm(i))*p)**2-1.0d0)
      return
11    del=1000.
      write(*,100)i
100   format(1x,/,' dtdx2 error: p >= 1/v(',i2,') ')
      return
      end
c***************************************************************************
c        subroutine eigen
c
c        purpose
c           compute eigenvalues and eigenvectors of a real symmetric
c           matrix
c
c        usage
c           call eigen(a,r,n,mv)
c
c        description of parameters
c           a - original matrix (symmetric), destroyed in computation.
c               resultant eigenvalues are developed in diagonal of
c               matrix a in descending order.
c           r - resultant matrix of eigenvectors (stored columnwise,
c               in same sequence as eigenvalues)
c           n - order of matrices a and r
c           mv- input code
c                   0   compute eigenvalues and eigenvectors
c                   1   compute eigenvalues only (r need not be
c                       dimensioned but must still appear in calling
c                       sequence)
c
c        remarks
c           original matrix a must be real symmetric (storage mode=1)
c           matrix a cannot be in the same location as matrix r
c
c        subroutines and function subprograms required
c           none
c
c        method
c           diagonalization method originated by Jacobi and adapted
c           by Von Neumann for large computers as found in 'Mathematical
c           Methods for Digital Computers', edited by A. Ralston and
c           H.S. Wilf, John Wiley and sons, New York, 1962, chapter 7
c
c     ..................................................................
c
      subroutine eigen(a,r,n,mv)
      save
      dimension a(*),r(*)
c
c        ...............................................................
c
c        if a double precision version of this routine is desired, the
c        c in column 1 should be removed from the double precision
c        statement which follows.
c
c      double precision a,r,anorm,anrmx,thr,x,y,sinx,sinx2,cosx,
c     1                 cosx2,sincs,range
c
c        the c must also be removed from double precision statements
c        appearing in other routines used in conjunction with this
c        routine.
c
c        the double precision version of this subroutine must also
c        contain double precision fortran functions.  sqrt in statements
c        40, 68, 75, and 78 must be changed to dsqrt.  abs in statement
c        62 must be changed to dabs. the constant in statement 5 should
c        be changed to 1.0d-12.
c
c        ..............................................................
c
c        generate identity matrix
c
c old    5 range=1.0e-7
   5  range=1.0e-12       ! jh change 4-10-2012 as suggested by barry 
c     if(mv-1) 10,25,10
      if(mv-1.EQ.0) GOTO 25
   10 iq=-n
      do 20 j=1,n
      iq=iq+n
      do 20 i=1,n
      ij=iq+i
      r(ij)=0.0
c     if(i-j) 20,15,20
      if(i-j.NE.0) GOTO 20
   15 r(ij)=1.0
   20 continue
c
c        compute initial and final norms (anorm and anormx)
c
   25 anorm=0.0
      do 35 i=1,n
      do 35 j=i,n
c     if(i-j) 30,35,30
      if(i-j.EQ.0) GOTO 35
   30 ia=i+(j*j-j)/2
      anorm=anorm+a(ia)*a(ia)
   35 continue
c     if(anorm) 165,165,40
      if(anorm.LE.0) GOTO 165
40    anorm=sqrt(2.0*anorm)
      anrmx=anorm*range/dfloat(n)
c
c        initialize indicators and compute threshold, thr
c
      ind=0
      thr=anorm
   45 thr=thr/dfloat(n)
   50 l=1
   55 m=l+1
c
c        compute sin and cos
c
   60 mq=(m*m-m)/2
      lq=(l*l-l)/2
      lm=l+mq
c  62 if(abs(a(lm))-thr) 130,65,65
   62 if(abs(a(lm))-thr.LT.0) GOTO 130
   65 ind=1
      ll=l+lq
      mm=m+mq
      x=0.5*(a(ll)-a(mm))
   68 y=-a(lm)/sqrt(a(lm)*a(lm)+x*x)
c     if(x) 70,75,75
      if(x.GE.0) GOTO 75
   70 y=-y
   75 sinx=y/sqrt(2.0*(1.0+(sqrt(abs(1.0-y*y)))))
      sinx2=sinx*sinx
   78 cosx= sqrt(1.0-sinx2)
      cosx2=cosx*cosx
      sincs =sinx*cosx
c
c        rotate l and m columns
c
      ilq=n*(l-1)
      imq=n*(m-1)
      do 125 i=1,n
      iq=(i*i-i)/2
c     if(i-l) 80,115,80
      if(i-l.EQ.0) GOTO 115
c  80 if(i-m) 85,115,90
   80 if(i-m.EQ.0) GOTO 115
      if(i-m.GT.0) GOTO 90
   85 im=i+mq
      go to 95
   90 im=m+iq
c  95 if(i-l) 100,105,105
   95 if(i-l.GE.0) GOTO 105
  100 il=i+lq
      go to 110
  105 il=l+iq
  110 x=a(il)*cosx-a(im)*sinx
      a(im)=a(il)*sinx+a(im)*cosx
      a(il)=x
c 115 if(mv-1) 120,125,120
  115 if(mv-1.EQ.0) GOTO 125
  120 ilr=ilq+i
      imr=imq+i
      x=r(ilr)*cosx-r(imr)*sinx
      r(imr)=r(ilr)*sinx+r(imr)*cosx
      r(ilr)=x
  125 continue
      x=2.0*a(lm)*sincs
      y=a(ll)*cosx2+a(mm)*sinx2-x
      x=a(ll)*sinx2+a(mm)*cosx2+x
      a(lm)=(a(ll)-a(mm))*sincs+a(lm)*(cosx2-sinx2)
      a(ll)=y
      a(mm)=x
c
c        tests for completion
c
c        test for m = last column
c
c 130 if(m-n) 135,140,135
  130 if(m-n.EQ.0) GOTO 140
  135 m=m+1
      go to 60
c
c        test for l = second from last column
c
c 140 if(l-(n-1)) 145,150,145
  140 if(l-(n-1).EQ.0) GOTO 150
  145 l=l+1
      go to 55
c 150 if(ind-1) 160,155,160
  150 if(ind-1.NE.0) GOTO 160
  155 ind=0
      go to 50
c
c        compare threshold with final norm
c
c 160 if(thr-anrmx) 165,165,45
  160 if(thr-anrmx.GT.0) GOTO 45
c
c        sort eigenvalues and eigenvectors
c
  165 iq=-n
      do 185 i=1,n
      iq=iq+n
      ll=i+(i*i-i)/2
      jq=n*(i-2)
      do 185 j=i,n
      jq=jq+n
      mm=j+(j*j-j)/2
c     if(a(ll)-a(mm)) 170,185,185
      if(a(ll)-a(mm).GE.0) GOTO 185
  170 x=a(ll)
      a(ll)=a(mm)
      a(mm)=x
c     if(mv-1) 175,185,175
      if(mv-1.EQ.0) GOTO 185
  175 do 180 k=1,n
      ilr=iq+k
      imr=jq+k
      x=r(ilr)
      r(ilr)=r(imr)
  180 r(imr)=x
  185 continue
      return
      end


c**********************************************************************
c function to calculate the F-distribution function for nu1 parameters,
c having nu2 degrees of freedom at confidence level alpha

c Barry Lienert 1994

c determined using 'Numerical recipes' functions betai, betacf, gammln
c and (modified) bisection function, rtbis, to find the root of qf

      FUNCTION fdist(nu1,nu2,alpha)
      external qf

c lower and upper limit of F
      f1=0
      f2=1.e6

c accuracy desired
      facc=1.e-6

c find the root of qf(f,nu1,nu2,alpha)
      fdist=rtbis(qf,f1,f2,facc,nu1,nu2,alpha)

      return
      end

c****************************************************************
c equation of F-distribution probability minus confidence
c level alpha: the root of this is the F-distribution value
c at nu1,nu,alpha

c Barry Lienert 1994

      FUNCTION qf(f,nu1,nu2,alpha)
      a=float(nu2)*.5
      b=float(nu1)*.5
      x=float(nu2)/(float(nu2)+float(nu1)*f)

c equation to find the root of
      qf=betai(a,b,x)-alpha

      return
      end

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c numerical recipes functions

      FUNCTION betai(a,b,xx)                        !JAB(BGS)Mar95.
      REAL betai,a,b,x
      real       xx,                                !JAB(BGS)Mar95.Argument.
     &           xlogmax,                           !JAB(BGS)Mar95.Max log.
     &           xexpmin                            !JAB(BGS)Mar95.Min exp.
      parameter (xexpmin = 1.0e-30)                 !JAB(BGS)Mar95.& value.
      parameter (xlogmax = 69.0)                    !JAB(BGS)Mar95.
c
CU    USES betacf,gammln
      REAL bt,betacf,gammln
c
      x = xx                                        !JAB(BGS)Mar95.Working copy.
c     if(x.lt.0..or.x.gt.1.)pause 'bad argument x in betai'
      if(x.lt.0..or.x.gt.1.) then
        write(*,*) 'bad argument x in betai'
        read(*,'()')
      endif
c
c    Prevent "log" underflow/overflow/divide by zero/inexact etc...
c
      if( x .lt. xexpmin ) then                     !JAB(BGS)Mar95.
      bt = gammln(a+b)                              !JAB(BGS)Mar95.
     &   - gammln(a)                                !JAB(BGS)Mar95.
     &   - gammln(b)                                !JAB(BGS)Mar95.
     &   - a*xlogmax                                !JAB(BGS)Mar95.
     &   + b*log(1.-x)                              !JAB(BGS)Mar95.
c
      else if( 1.0-x .lt. xexpmin ) then            !JAB(BGS)Mar95.
      bt = gammln(a+b)                              !JAB(BGS)Mar95.
     &   - gammln(a)                                !JAB(BGS)Mar95.
     &   - gammln(b)                                !JAB(BGS)Mar95.
     &   + a*log(x)                                 !JAB(BGS)Mar95.
     &   - b*xlogmax                                !JAB(BGS)Mar95.
c
      else
      bt = gammln(a+b)                              !JAB(BGS)Mar95.
     &   - gammln(a)                                !JAB(BGS)Mar95.
     &   - gammln(b)                                !JAB(BGS)Mar95.
     &   + a*log(x)                                 !JAB(BGS)Mar95.
     &   + b*log(1.-x)                              !JAB(BGS)Mar95.
      endif
c
      if( abs(bt) .gt. xlogmax ) then               !JAB(BGS)Mar95.
      bt = exp(sign(xlogmax,bt))                    !JAB(BGS)Mar95.
      else                                          !JAB(BGS)Mar95.
      bt = exp( bt )                                !JAB(BGS)Mar95.
      end if                                        !JAB(BGS)Mar95.
c
CJAB(BGS)Mar95.     bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.-x))
c
      if(x.lt.(a+1.)/(a+b+2.))then
        betai=bt*betacf(a,b,x)/a
        return
      else
        betai=1.-bt*betacf(b,a,1.-x)/b
        return
      endif
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ">29,)KL4.
      FUNCTION betacf(a,b,x)
      INTEGER MAXIT
      REAL betacf,a,b,x,EPS,FPMIN
      PARAMETER (MAXIT=100,EPS=3.e-7,FPMIN=1.e-30)
      INTEGER m,m2
      REAL aa,c,d,del,h,qab,qam,qap
      qab=a+b
      qap=a+1.
      qam=a-1.
      c=1.
      d=1.-qab*x/qap
      if(abs(d).lt.FPMIN)d=FPMIN
      d=1./d
      h=d
      do 11 m=1,MAXIT
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
c     pause 'a or b too big, or MAXIT too small in betacf'
        write(*,*) 'a or b too big, or MAXIT too small in betacf'
        read(*,'()')
1     betacf=h
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ">29,)KL4.
      FUNCTION gammln(xx)
      REAL gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ">29,)KL4.

c********************************************************************
c this function modified (BRL, 94) to include the
c extra variables nu1, nu2 & alpha needed for the F-distribution
      FUNCTION rtbis(func,x1,x2,xacc,nu1,nu2,alpha)
      INTEGER JMAX
      REAL rtbis,x1,x2,xacc,func
      EXTERNAL func
      PARAMETER (JMAX=40)
      INTEGER j,nu1,nu2
      REAL dx,f,fmid,xmid,alpha
      fmid=func(x2,nu1,nu2,alpha)
      f=func(x1,nu1,nu2,alpha)
c     if(f*fmid.ge.0.) pause 'root must be bracketed in rtbis'
      if(f*fmid.ge.0.) then
        write(*,*) 'root must be bracketed in rtbis'
        read(*,'()')
      endif
      if(f.lt.0.)then
        rtbis=x1
        dx=x2-x1
      else
        rtbis=x2
        dx=x1-x2
      endif
      do 11 j=1,JMAX
        dx=dx*.5
        xmid=rtbis+dx
        fmid=func(xmid,nu1,nu2,alpha)
        if(fmid.le.0.)rtbis=xmid
        if(abs(dx).lt.xacc .or. fmid.eq.0.) return
11    continue
c     pause 'too many bisections in rtbis'
      write(*,*) 'too many bisections in rtbis'
      read(*,'()')
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ">29,)KL4.
      
