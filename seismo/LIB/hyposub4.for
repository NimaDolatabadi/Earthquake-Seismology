cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
c
c                HYPOSUB4  version 4.0  6/98
c
cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
c
c changes:
c
c 6/92 (BRL) startloc has been extensively modified from version 2.0
c it now performs a regression on similar phase times at 3 or more
c different stations and converts the resulting slowness and azimuth
c into a starting location.
c All candidate starting locations now have their rms calculated
c (using the new function rmsv) and the one having the smallest
c rms is used.         

c 2/95: added ires & fixor
c june 95 by jh and bl: problem with start location, nearest station
c
c sep 98 ny jh : ------------- version 7.0 check -------------------
c                5 character station codes
c
c oct 28 1999 bmt : linux changed, save and *
c nov 19  99  jh  : v,vs,nl,parm removed from call windows, in common block
c jan             : multi model
c sep 10 2001 jh+bl: fix so a blnak phase with azimuth can be the
c                    first line
c
c aug 2002    jh+bl: fix problem of error when locating in a sequence
c
c may 2008    bl   : search for closest station location i 4 quadrants to
c                    improve start location
c jun 2011    jh   : do not use app vel and az in starting location
c                    if event is local, can make a start location
c                    very far from network.
c nov 2013    jh   : fix closest station output, was wrong and the position
c                    with lowes rms was not used
c feb 2017    jh   : above did not work if first quadrant had lowest rms. Now searching
c                    in 12 points around nerest station at distances 10, 50 and 150 km.
c                    seems to have improved start location.

c Subroutines from 'Numerical Recipes' have been added to perform
c the regressions in startloc
 
      subroutine startloc(nphase,st,phase,tp,x0,y0,xh0,xh2,nn,
     &nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,
     &tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,rmsmin,distind,nsol,
     &aninc,trphs,vapp,azapp,alatm,alonm,isort,data,fixdep,depthlim,
     &iflagd,ires,fixor,scorr,appar_veloc)

c**********************************************************************
c
c    find a starting location for a set of earthquake phases 
c             using azimuths and travel times
c
c              Barry R. Lienert  version 3.1 10/93
c
c    The travel times must already be matched to their station IDs
c    and coordinates (geocentric) and must each have a 4 character 
c    phase identifier PN, PG, P , SN, SG, S, L or AZ,
c    where a blank second letter means an unidentified 
c    first arrival. The arrival times/azimuths are stored in the
c    double precision array tp(i). Times must be seconds referenced
c    to a fixed datum (e.g., seconds from 1/1/1900), while azimuths
c    are in degrees, clockwise from North, measured relative to the 
c    receiver. Station latitude and longitude are in geocentric radians. 
c
c    The strategy proceeds as follows:
c
c    1. Determine the index of the phase recorded at the maximum 
c       number of stations (maxno) and its associated velocity, vv 
c       If maxno>2 perform a linear regression on the arrival times
c       with respect to their latitudes and longitudes to determine
c       the azimuth. 
c
c    2. Find the number of azimuthal phases, naz
c       Exit if maxno=0 and naz=0 (not locatable)
c       Use the azimuth data to define a starting location
c       based on intersections with other azimuths, including
c       those defined in (1).
c
c    3. Find nmp, the number of multi-phase (S-P, etc.) stations.
c       If one of these multi-phase stations has an azimuth, 
c       calculate the distance and use it to define the starting
c       location. Else, use the first two multi-phase distances to define
c       intercepts with the lines defined by any azimuth phases, or
c       by azimuths determined in (1). Then use any additional distances
c       to select one of the intercepts as a starting location.
c
c    4. Search for any refracted (N) phases, and make sure
c       that their critical distances are satisfied.
c
c   external subroutines: 
c
c   delaz (J. Lahr)         - calculates azimuth and delta between two 
c                             sets of geocentric coordinates
c   latlon (S. Bratt)       - calculates new latitude/longitude corresponding
c                              to a shift of delta along a given azimuth
c   azcros (Bratt/Williams) - calculates the intersection point of two 
c                             great circles
c
c   Changes:
c
c   May 21      J.H.: Duality in start location and test(55) = 1 accounted for
c
c   May 21, 1992  BRL: added same-phase regression to estimate azimuth,
c                      slowness and starting location
c
c                      Fixed bug in locations using multiple distances
c                      Changed logic so that dual solutions do not occur
c                      as frequently
c                      Fixed test(56) logic so <=0.0 disables starting location
c
c   Jun 19, 1992  BRL: added parameters to window which enable calls to
c                      new function rmsv, which evaluates rms at any location
c
c                      rms is now the major criterion for selecting a start
c                      location. It is evaluated for each solution, and if
c                      less than the present minimum value, rmsmin, this
c                      solution is the one saved in xh0
c

c 11/92 (BRL) changed the step size in the minimum rms search
c added a test for outliers in the "plane wave" regression
c which removes extreme outliers. Also added the alternate
c starting location xh2 for use when no convergence is
c obtained using xh0

c 10/93, BRL: removed statement numbers where possible
c             as well as making many (commented) modifications

c             used sort array 'isort' to sort arrivals in order
c             preventing input order from changing start location
c             set depth to initial xh0(3) value for all starting
c             location determinations
c     added minflag=1 to rmsv window to force minimum travel time rms
c
c 4/94: added fixdep to window: fix depth for apparent velocity starting location
c 5/94: only use depth phases when depth not fixed

c****************************************************************************

c change 10/93: added parameter statement and dimensioned
c arrays using it.
      save
      include 'hypparm.inc'
      character*5 stat,stataz,stemp
      character*5 statdp
      character*8 st1(narriv),stmax
      character*1 prm1,prm2,prm3,prm4,ucase,fixdep,fixdepn
      character*1 distinds
      character*1 ins,iew
      double precision firstarriv,lastarriv,ttpred
      double precision sumt,sumx,sumy,sumx2,sumy2,sumxy,asl(3,3),
     &ya(3,3),c(3),cinv(3),dd,sumt2,xno,tmean,r2,xx,yy,sumxt,sumyt
      double precision sum   !added 6/98
      dimension ind1(narriv),ind2(narriv),nmp(narriv),delmp(narriv),
     &ind3(narriv,20),nmp1(narriv),indx(3),xhtr(3),
     &tdev(narriv),dpps(500)

      real usrc(2)             ! added aug 2002 by jh+bl

      pi=3.141593
      degtorad=0.0174533
      dist=0.1
      vl=test(51)
      dcrit=test(53)

      relaz=0.0
      rmsmin=9999.
      iulst1=0                  
      imatch=1       
      fixdepn=' '

c 5/94 BRL: only use depth phases if depth not fixed
c 6/95 BRL: also only if test(89)>0

      if(fixdep.ne.'F'.and.test(89).gt.0.0)then
      
c added depth phases 1/94 BRL

       sumdepth=0.0
       kdd=0
       statdp='     '

       do i=1,nphase
        if(st(i).ne.statdp.and.phase(i)(1:1).eq.'p'
     &   .and.dtwt(i).gt.0.0)then
         statdp=st(i)
         i1=i
         i2=i1-1
         ifound=0
          if(i2.gt.0)then
           stemp=st(i2)
          else
           stemp='     ' 
          endif 
          do while (statdp.eq.stemp.and.i2.ge.1)
           if(phase(i1)(2:4).eq.phase(i2)(1:3))then
             dppp=sngl(tp(i1)-tp(i2))*test(68)
             write(iulst,'(1x,a4,1x,a4,''-'',a4,f7.1,'' km'')')
     &        st(i),phase(i1),phase(i2),dppp
             if(dppp.gt.0.0)then
               sumdepth=sumdepth+dppp
               kdd=kdd+1
               dpps(kdd)=dppp          
             endif
           endif
           i2=i2-1
           if(i2.gt.0)then
            stemp=st(i2)
           else
            stemp='     '
           endif  
           ifound=1
          enddo
         if(ifound.eq.0)then
           i2=i1+1
           do while (statdp.eq.st(i2).and.i2.le.nphase)
             if(phase(i1)(2:4).eq.phase(i2)(1:3))then
               dppp=sngl(tp(i1)-tp(i2))*test(68)
               if(dppp.gt.0.0)then
                 sumdepth=sumdepth+dppp
                 kdd=kdd+1           
                 dpps(kdd)=dppp
               endif
             endif
             i2=i2+1
           enddo
         endif
        endif
       enddo

c change 11/94: minimum of 3 depth phases needed
c 6/98: changed to >test(103) depth phases
       if(kdd.gt.test(103)-1.)then                         
       
         startdp=sumdepth/float(kdd) 

c change 3/94 to exclude severe outliers         
         ddrms=0.0
         if(kdd.gt.test(103)-1.)then
          do k=1,kdd
           ddrms=ddrms+(dpps(k)-startdp)**2
          enddo
          ddrms=sqrt(ddrms/float(kdd))
c         write(iulst,*)' ddrms=',ddrms,startdp 
          sumdepth=0.0
          kd1=0
          do k=1,kdd         
c          write(iulst,*)dpps(k)
           if(abs(dpps(k)-startdp).lt.2.*ddrms)then
            kd1=kd1+1
            sumdepth=sumdepth+dpps(k)
           endif
          enddo
          if(kd1.gt.1)then
           startdp=sumdepth/float(kd1)  
          else
           kd1=kdd
          endif  
         else
          kd1=kdd
         endif
          
c 4/94: limit start depth to test(70)
         if(startdp.gt.test(70))startdp=test(70)
         
         if(iulst.gt.0.and.(.not.multi_model))
     *   write(iulst,'(/i3,'' Depth phases used: '',
     &    ''depth = '',f6.1,'' km''/)')kd1,startdp
         xh0(3)=startdp
       endif
      endif
      
c 10/93: use minimum travel time for rms calculations
      minflag=0
      stdp=xh0(3)
c      if(test(40).eq.0.0)stdp=xh0(3)-float(maxelv)*.001
      if(.not.multi_model)
     *write(iulst,'('' Starting location depth = '',f7.1,'' km'')')
     &stdp
      xhtr(3)=xh0(3)
      xh2(3)=xh0(3)
        
c  nsol is the number of solutions found - ranges from 0 to 2
      nsol=0

c      read(33,3278)latt,xlatt,lont,xlont
c3278  format(18x,i2,1x,f5.2,2x,i2,1x,f5.2)
c      write(iulst,*)latt,xlatt,lont,xlont
c      call fold(ytheor,xtheor,latt,'N',xlatt,lont,'W',xlont)

c change 10/93: find the number of PS phases, npsphase
      npsphase=0
      do i=1,nphase
       if(ips(i).eq.1.and.dtwt(i).ne.0.0)npsphase=npsphase+1
      end do

c **********  1. find the number of  multi-station phases, nphs ******

c  zero  counters
      do i=1,nphase               
c         write(iulst,'(a8,2i3,f6.3)')phase(i),ip(i),ips(i),dtwt(i)
         ind1(i)=0
         ind2(i)=0
      end do
      k=0
      nphs=0

c   bump the phase index
30    k=k+1

c   check for azimuths and last phase
      if(k.le.nphase)then

c 1/95: exclude depth phases
c  don't count azimuths, zero weight phases, PS phases or depth phases
        if(phase(k)(1:1).eq.'p')go to 30
        if(phase(k)(1:1).eq.'s')go to 30
        if(phase(k)(1:2).eq.'AZ')go to 30
        if(phase(k)(1:4).eq.'    ')go to 30 !added 6/98
        if(ucase(phase(k)(2:2)).eq.'X')go to 30
        if(dtwt(k).le.0.0)go to 30    !change 6/98
c 10/94: exclude  difference phases
        if(ip(k).eq.9.or.ips(k).eq.1)go to 30

c   do we already have this phase ?
        do i=1,nphs
c         write(iulst,'(2i3,2a8)')k,i,phase(k),st1(i)
c allow 4th letter of phase (PKPa, PKPb, etc) to be matched with a blank
          if(phase(k)(1:3).eq.st1(i)(1:3).and.(phase(k)(4:4).eq.' '.or.
     &    st1(i)(4:4).eq.' '.or.phase(k)(4:4).eq.st1(i)(4:4))
     &    )then
c   Yes:  bump its multi-station counter and get another
            ind1(i)=ind1(i)+1
            go to 30         
c           endif                   
           
          endif
        end do

c   No: bump the  phase count, store phase ID 
c       in st1 and get another phase
        nphs=nphs+1
        st1(nphs)=phase(k)
        go to 30

      endif

c   now find their associated apparent velocity, vv
      maxno=0
      maxind=0
      n1=nmoho
      if(n1.le.0.or.n1.gt.nl)n1=1

      do i=1,nphs
c        write(iulst,*)'phase,ind1,maxno: ',st1(i),ind1(i),maxno
        if(ind1(i).gt.maxno)then
          vv=0.0
          if(ucase(st1(i)(1:1)).eq.'P')vv=v(n1)
          if(ucase(st1(i)(1:1)).eq.'S')vv=vs(n1)

c 1/95: added Pg & Sg
          if(st1(i)(1:2).eq.'PG'.or.st1(i)(1:2).eq.'Pg')vv=v(1)
          if(st1(i)(1:2).eq.'SG'.or.st1(i)(1:2).eq.'Sg')vv=vs(1)
          
          if(st1(i)(1:1).eq.'L')vv=vl
          if(vv.gt.0)then
            maxind=i

c 10/94: removed +1
            maxno=ind1(i)+1

            vv=vv*1.5
          endif
        endif
c        write(iulst,*)st1(i)
      end do

      stmax='xxxx'
      if(maxind.gt.0)stmax=st1(maxind)

      i1=0
      i2=0                        
      firstarriv=1.d99
      lastarriv=-1.d99
      
      do i=1,nphase
c------------------------------------------------------------
c change 10/93: exclude PS differences
        if(dtwt(i).ne.0.0.and.ip(i).ne.9.and.ips(i).ne.1)then
c------------------------------------------------------------
          if(phase(i)(1:4).eq.stmax(1:4).and.maxind.gt.0)then
            if(tp(i).gt.lastarriv)then
              i2=i
              lastarriv=tp(i)
            endif
c 6/95 BRL: exclude invalid phases            
            if(tp(i).lt.firstarriv.and.dtwt(i).gt.0.)then
              i1=i
              firstarriv=tp(i)
            endif
          endif
        endif
      end do

c  coordinates of nearest station
c      write(16,*)'i1,i2',i1,i2
      if(i1.eq.0)i1=1
      if(i2.eq.0)i2=1
c
c   jh 2017  changed so 12 points around nearest station is tested, currently in
c   a distance of 10, 50 and 150 km
c
      rmsmin=9999.0

      do kkkk=1,3       ! loop for 3 distances
      if(kkkk.eq.1) dist_to_move=10.0
      if(kkkk.eq.2) dist_to_move=50.0
      if(kkkk.eq.3) dist_to_move=150.0

      dely=(dist_to_move/111.2)/57.3       ! convert to radians
      if(abs(y0(i1)).lt.1.56) then  ! 90 deg
         delx=dely/cos(abs(y0(i1)))         ! correct for latitude
      else
         delx=1.0                  ! near north or south pole
      endif



      do kkk=1,4                   ! loop for 4 points in grid
       if(kkk.eq.1)then
        xhtr(1)=x0(i1)+delx
        xhtr(2)=y0(i1)+dely
       endif
       if(kkk.eq.2)then
        xhtr(1)=x0(i1)+delx
        xhtr(2)=y0(i1)-dely
       endif
       if(kkk.eq.3)then
        xhtr(1)=x0(i1)-delx
        xhtr(2)=y0(i1)+dely
       endif
       if(kkk.eq.4)then
        xhtr(1)=x0(i1)-delx
        xhtr(2)=y0(i1)-dely
       endif


      rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,nmoho,
     &nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,
     &tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc,
     &trphs,dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,scorr)



c changed 4/94 to not set rmsmin=rms if 'N' phase
cxx
c       if(ucase(phase(i1)(2:2)).ne.'N')rmsmin=rms

c       xh0(1)=xhtr(1)
c       xh0(2)=xhtr(2)
c       nsol=1

       if(iulst.gt.0)then
c
c   convert from geocentric to geographical coordinates
c
        call unfold(xhtr(2),xhtr(1),la,ins,ala,lo,iew,alo)
        xla=la+ala/60.0
        if(ins.eq.'S') xla=-xla
        xlo=lo+alo/60.0
        if(iew.eq.'W') xlo=-xlo

        write(iulst,'(a,f6.1,a,3f8.2)')
     *  'Starting location ',dist_to_move,
     *  ' km from closest station and rms ',xla,xlo,rms

       endif

c
c   find point with smallest rms
c
       if(rms.lt.rmsmin) then
           xh0(1)=xhtr(1)
           xh0(2)=xhtr(2)
           nsol=1
           rmsmin=rms
       endif
      enddo      ! end of loop trying 4 different points around first arriving station
      enddo      ! end of look trying 3 different distances 

c  use closest station solution if test(56) <= 0 (changed so 0 disables 6/94)

      if(test(56).le.0.0) then

          write(iulst,'(a)') 
     *   ' Use closest station with lowest rms as start loction'     
         return
      endif        
      


ccccccccccccccccccccccccccccccccccccccccc barry cccccccccccccccccccccccccccc
c*************** 1.  common phase station section ********************

      if(maxno.gt.1)then

c   find the mean position (slonm,slatm) of the maximum common-phase stations
        sumlat=0.0
        sumlon=0.0
        icnt=0             

c 1/95: find first common phase arrival
        firstarriv=1.d99
        icpfirst=0
        
        do  i=1,nphase
c          write(iulst,*)st(i),x0(i)/degtorad,y0(i)/degtorad

c changed 10/93 to exclude PS phases
          if(dtwt(i).ne.0.0.and.ips(i).ne.1.and.ip(i).ne.9)then
            if(phase(i)(1:4).eq.stmax(1:4).and.maxind.gt.0)then
              if(tp(i).lt.firstarriv)then
               firstarriv=tp(i)
               icpfirst=i
              endif 
              icnt=icnt+1
              sumlat=sumlat+y0(i)
              xsave=x0(i)
              if(xsave.gt.pi)xsave=xsave-2*pi
              sumlon=sumlon+xsave
            endif
          endif  
        end do     

c        write(iulst,*)' maxno,icnt ',maxno,icnt
        slatm=sumlat/float(maxno)
        slonm=sumlon/float(maxno)
c        write(iulst,*)maxno,slonm/degtorad,slatm/degtorad
        if(iulst.gt.0.and.(.not.multi_model))then
          write(iulst,'(/'' maximum multi-station phase: '',a4,i8/)')
     &    st1(maxind),maxno
        endif

c 1/95: Find common phase having maximum delta/abs(tp(i)-tp(icpfirst))
        dllmax=0.0
        do  i=1,nphase 
          if(dtwt(i).ne.0.0.and.ips(i).ne.1.and.ip(i).ne.9)then
c 6/98 BRL: exclude PKP if test(90)=0.0        
            if((phase(i)(1:4).eq.stmax(1:4).or.(stmax(1:1).eq.'P'
     &      .and.phase(i)(1:3).eq.'PKP'.and.test(90).gt.0.0))
     &      .and.maxind.gt.0.and.st(i).ne.st(icpfirst))then
              call delaz(y0(icpfirst),x0(icpfirst),delstat,dedeg,
     &        azstat,y0(i),x0(i))
              deltt=abs(sngl(tp(i)-tp(icpfirst)))
              if(deltt.eq.0.0)deltt=1.0
              dll=dedeg/sqrt(deltt)
c              write(iulst,*)x0(i)/degtorad,y0(i)/degtorad,dll,dedeg
              if(dll.gt.dllmax)then
               dllmax=dll     
               azmax=azstat
               dedegmx=dedeg*.5
               imaxx=i
              endif 
            endif
          endif  
        end do               
c        write(*,*)st(imaxx),x0(imaxx)/degtorad,y0(imaxx)/degtorad

c now find the midpoint between the icpfirst and dlmax station
        if (imaxx.gt.0)then       !condition added 6/98 BRL      
         call latlon(y0(imaxx)/degtorad,x0(imaxx)/degtorad,
     &   dedegmx,azmax,alata,alona)
c        write(*,*)alona,alata
        
c and try it as a start location
         xhtr(1)=alona*degtorad
         xhtr(2)=alata*degtorad
      
         rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,nmoho,
     &   nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,
     &   tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc,
     &   trphs,dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,
     &   scorr)

         if(rms.lt.0.0.and.ndata.gt.2)then
          rmsmin=rms
          xh0(1)=xhtr(1)
          xh0(2)=xhtr(2)
          nsol=1

          if(iulst.gt.0.and.(.not.multi_model))then
           write(iulst,'('' Starting location from midpoint of'',
     &     '' two most distant stations: '',5x,3f8.2)')xhtr(2)/degtorad,
     &      xhtr(1)/degtorad
           write(iulst,'('' rms = '',f8.2)')rms
          endif
         endif 
        endif
      endif

c 10/94: moved ntime=0 to above 1799 - was causing infinite loop
      ntime=0
1799  continue
c
c   if a local event, do not used regression for start location
c   jh jun 2011
c
      if(distind.eq.'L') goto 3746
      if(maxno.gt.2)then

c  perform linear regressions on the x and y coordinates to
c  obtain apparent azimuth and apparent velocity, vappar
        sumt=0
        sumt2=0.0
        sumx=0.0
        sumy=0.0
        sumxt=0.0
        sumyt=0.0
        sumx2=0.0
        sumy2=0.0
        sumxy=0.0

        do i=1,nphase
          if(dtwt(i).ne.0.0.and.ip(i).ne.9.and.ips(i).ne.1)then
            if(phase(i)(1:3).eq.stmax(1:3).and.maxind.gt.0)then
              call delaz(y0(i),x0(i),delstat,dedeg,azstat,slatm,
     @        slonm)
              xx=delstat*sin(azstat*degtorad)
              yy=delstat*cos(azstat*degtorad)
              ttt=tp(i)-tp(i1)
c              write(iulst,*)st(i),phase(i),azstat,xx,yy,ttt
              sumt=sumt+ttt
              sumt2=sumt2+ttt*ttt
              sumx=sumx+xx
              sumy=sumy+yy
              sumx2=sumx2+xx*xx
              sumy2=sumy2+yy*yy
              sumxt=sumxt+xx*ttt
              sumyt=sumyt+yy*ttt
              sumxy=sumxy+xx*yy
            endif
          endif
        end do

        asl(1,1)=float(maxno)
        asl(1,2)=sumx
        asl(2,1)=asl(1,2)
        asl(1,3)=sumy
        asl(3,1)=asl(1,3)
        asl(2,2)=sumx2
        asl(2,3)=sumxy
        asl(3,2)=asl(2,3)
        asl(3,3)=sumy2
        c(1)=sumt
        c(2)=sumxt
        c(3)=sumyt

        do i=1,3
          do j=1,3
            ya(i,j)=0.
          end do
          ya(i,i)=1.
        end do

        call ludcmp(asl,3,3,indx,dd)

        do j=1,3
          call lubksb(asl,3,3,indx,ya(1,j))
        end do

        do i=1,3
          cinv(i)=0.0
          do j=1,3
            cinv(i)=cinv(i)+ya(i,j)*c(j)
          end do
        end do

        xno=dfloat(maxno)
        tmean=sumt/xno
        r2=xno*cinv(1)**2+(cinv(2)**2)*sumx2+(cinv(3)**2)*sumy2
     &  +2.*cinv(1)*cinv(2)*sumx+2.*cinv(1)*cinv(3)
     &  *sumy+2.*cinv(2)*cinv(3)*sumxy-2.*tmean*xno*cinv(1)-2.*tmean*
     &  cinv(2)*sumx-2.*tmean*cinv(3)*sumy+xno*tmean**2

        if(sumt2.ne.xno*tmean**2)then
          r2=r2/(sumt2-xno*tmean**2)
        endif

c section added 11/19/92 (BRL) to find and remove outliers in this
c regression
c 7/94: exclude local events                            
        if(ntime.eq.0.and.distind.ne.'L')then
          sum=0.0
          nsum=0
          do i=1,nphase
            if(dtwt(i).ne.0.0.and.ip(i).ne.9.and.ips(i).ne.1)then
              if(phase(i)(1:3).eq.stmax(1:3).and.maxind.gt.0)then
                call delaz(y0(i),x0(i),delstat,dedeg,azstat,slatm,
     @          slonm)
                xx=delstat*sin(azstat*degtorad)
                yy=delstat*cos(azstat*degtorad)
                ttt=tp(i)-tp(i1)
                ttpred=cinv(1)+cinv(2)*xx+cinv(3)*yy
                tdev(i)=sngl(ttt-ttpred)
                nsum=nsum+1
                sum=sum+tdev(i)*tdev(i)
c               write(iulst,*)st(i),ttt,ttpred
              endif
            endif
          end do

          trms=sngl(dsqrt(sum/float(nsum)))  !changed 6/98
c         write(iulst,*)'trms = ',trms
          noutl=0
  
          do i=1,nphase
            if(dtwt(i).ne.0.0.and.ip(i).ne.9.and.ips(i).ne.1)then
c             write(iulst,*)tdev(i)
              if(phase(i)(1:3).eq.stmax(1:3).and.maxind.gt.0)then

c change 10/93: made test(61)*trms the criterion instead of 2.0*trms
c setting test(61)=0.0 disables any outlier rejection here
                if(abs(tdev(i)).gt.trms*test(61).and.test(61)
     &          .gt.0.0)then

                  write(*,*)st(i),phase(i),' removed: Apparent',
     &            ' velocity deviation = ',tdev(i)
                  write(iulst,*)st(i),phase(i),' removed: Apparent',
     &            ' velocity deviation = ',tdev(i)
                  write(iulst,*)
                  dtwt(i)=0.0
                  noutl=noutl+1
                  maxno=maxno-1
                endif
              endif
            endif
          end do

        endif

        ntime=ntime+1

c repeat the regression once with the outlier removed
        if(noutl.gt.0.and.ntime.eq.1.and.maxno.ge.2)go to 1799
c        write(iulst,*)' regression R2 = ',r2
c        write(iulst,*)'slopex,slopey,R2: ',cinv(1),cinv(2),cinv(3),r2
        denom=sqrt(cinv(2)**2+cinv(3)**2)
        vappar=0.0
        azm1=0.0
        if(denom.ne.0.0)then
          vappar=1./denom
          vapp=vappar
          azm1=atan2(-cinv(2),-cinv(3))/degtorad
          azapp=azm1

c 4/94: added printout of azimuth & appar. vel.
          if(iulst.gt.0.and.(.not.multi_model))then
            write(iulst,'(/,'' Regression azimuth='',f6.1,
     &       '' Apparent velocity='',f7.2,'' km/s'',/)')azm1,vappar
          endif

          nsolm=1
          azz=azm1
        endif

c alatm, alonm are the mean latitude and longitude (deg) of the array to
c which the azimuths determined here are referred
        alatm=slatm/degtorad
        alonm=slonm/degtorad

        
        if (vappar.gt.0.0)then  !condition added 6/98
c the values in the following formula were determined empirically, using
c a set of distant event arrivals collected by the Norwegian array
c They may need changing in different geological regions
         slowness=110.7/vappar
c         delta=(10.46-slowness)/0.067
c added 6/98 BRL Now use this routine to find delta using IASP91
         delta=find_delta('        ',slowness,tt1,imat)

         if(delta.lt.0.0)delta=1.0
         if(delta.gt.180.)delta=179.
         if(.not.multi_model)
     *   write(iulst,'('' delta = '',f6.1,'' R2 = '',f6.2)')delta,r2
         dlt1=delta 
         if(dlt1.lt.test(104))dlt1=test(104)
         dltmin=delta
         sign1=1.0
         rms1=9999.
         kk=0
c         call latlon(alatm,alonm,dlt1,azm1,alata,alona)
         
c  search at increments azinc and dinc in 
c azimuth and distance, respectively, for the minimum rms location, rms1

c change BRL 11/92: azinc made approximately the same distance 
c as dinc, dinc reduced to 1.0
         dinc=1.0
         azinc=dinc/(degtorad*delta)
         ifirst=1
         
5400     continue
         call latlon(alatm,alonm,dlt1,azm1,alata,alona)
c         call latlon(y0(icpfirst)/degtorad,x0(icpfirst)/degtorad,
c     &    dlt1,azm1,alata,alona)
c         write(iulst,*)'alatatr,alonatr',alata,alona
         xhtr(1)=alona*degtorad
         xhtr(2)=alata*degtorad

c 6/94 save distind in distinds and use 'D' for this section
         distinds=distind                                   
c         distind='D'
         
         rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,nmoho,
     &   nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc1,
     &   dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc,
     &   trphs,dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,
     &   scorr)
c         write(iulst,*)rms1,rms,alata,alona,dlt1,k
         if(ifirst.eq.1)rms1=rms
         
c change 10/93, BRL
c if initial rms value is > rms .15 km from nearest station (rmsmin)
c skip the minimum rms search. This was previously finding
c local minima at large distances in some cases
         ifirst=0
         if(rms1.le.rmsmin.or.distinds.eq.'D')then
         
          if(rms.le.rms1)then  !changed from .lt. 6/98
            kk=kk+1
            rms1=rms
            dltmin=dlt1
            dlt1=dlt1+dinc*sign1
            if(kk.lt.30)go to 5400
          elseif(kk.eq.1)then
            sign1=-1.0
            dlt1=delta-dinc
            kk=kk+1
            if(kk.lt.30)go to 5400
          endif
c          dlt1=dltmin
          kk=0
          sign1=1.0
          azmin=azm1                   
          
          azmm=azm1+azinc
5500      call latlon(alatm,alonm,dltmin,azmm,alata,alona)
          xhtr(1)=alona*degtorad
          xhtr(2)=alata*degtorad
          ndataprev=ndata
          rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,nmoho,
     &    nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc1,
     &    dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc,
     &    trphs,dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,
     &    scorr)

          nsol=1
c          write(iulst,*)rms,alata,alona,azmm,kk
c          write(iulst,*)xhtr(1)/degtorad,xhtr(2)/degtorad

          if(kk.eq.0)then
            sign1=-1.0
            azmm=azm1-azinc
            kk=kk+1
            go to 5500
          endif
          if(rms.lt.rms1.and.kk.lt.30)then  ! changed 6/98  to prevent 
            kk=kk+1                         ! infinite loop
            rms1=rms
            azmin=azmm
            azmm=azmm+sign1*azinc
            go to 5500
          endif
         endif
c         write(iulst,*)dltmin,azmin
         call latlon(alatm,alonm,dltmin,azmin,alata,alona)
         xhtr(1)=alona*degtorad
         xhtr(2)=alata*degtorad
 
         if(r2.gt..8)then
          if(dltmin.gt.10..and.rms1.lt.rmsmin)fixdepn='F'
          if(rms1.lt.rmsmin)then
            rmsmin=rms1
            xh2(1)=xh0(1)
            xh2(2)=xh0(2)
            xh0(1)=xhtr(1)
            xh0(2)=xhtr(2)
c            nsol=2
          endif
          if(rms.lt.rms1)rms1=rms
           if(iulst.gt.0.and.(.not.multi_model))then
            write(iulst,'('' Starting location from regression '',
     &      ''azimuth and slowness: '',5x,3f8.2)')alata,alona
            write(iulst,'('' rms = '',f8.2)')rms1
           endif
           apprms=rms1
         else
          xh2(1)=xhtr(1)
          xh2(2)=xhtr(2)
          nsol=2
         endif

c 6/94 restore original value
         distind=distinds
        endif
      endif
c
c   get here if previous section skipped due to event
c   being local
c
 3746 continue
c
c *************** 2.  azimuthal phases  ********************

      naz=0
      nazsol=0
      rmsaz=999.
c     write(6,*) nphase
      do  i=1,nphase
c
c       write(6,*)st(i),phase(i),ip(i),dtwt(i)
        if(phase(i)(1:2).eq.'AZ'.and.dtwt(i).gt.0.0)naz=naz+1
      end do
c      write(iulst,*)'naz',naz
      if(naz.eq.0.and.maxno.eq.0.and.npsphase.eq.0)then
    
c  no azimuths and only one station: not locatable 
        if(iulst.gt.0)write(*,*)' Only one common phase station',
     &  ' and no azimuths: not locatable'
        xh0(1)=-999.
        xh0(2)=-999.
        nsol=0
        return
      endif
    
c change 10/93: removed condition maxno > 0
c      if(maxno.gt.0)then

c  k is used to check the number of azimuths found
        kk=0

        do ii=1,nphase

c change 10/93, BRL: i is now arrival time order
          i=isort(ii)
          if(phase(i)(1:2).eq.'AZ'.and.dtwt(i).gt.0.0)then
            if(kk.eq.0)then
              nsolz=0

c  first azimuth:
c  save the station ID to test for a different station
              stat=st(i)
c              write(iulst,*)' 1 ',stat,phase(i),i,tp(i)
              t1=sngl(tp(i))
              x1=x0(i)/degtorad
              y1=y0(i)/degtorad
              kk=kk+1
              if(maxno.gt.2)then

c  find this azimuth's intersections with the apparent velocity azimuth, azm1 
                alat2=y0(i)/degtorad
                alon2=x0(i)/degtorad
                az2=sngl(tp(i))
                call azcros(alatm,alonm,azm1,alat2,alon2,az2,dist1,
     &          dist2,alat1,alon1,ierr)
                ierr1=ierr
                if(ierr1.eq.0)then
                  nsolz=1
                  alata=alat1
                  alona=alon1
                endif

c     set the new epicenter values
                if(nsolz.eq.1)then
                  xhtr(1)=alona*degtorad
                  xhtr(2)=alata*degtorad
                  rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,
     &            nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,
     &            test,dtwt,dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,
     &            xnear,tph,distind,aninc,trphs,dtav,minflag,data,
     &            imatch,depthlim,iflagd,ires,fixor,scorr)
                  nsol=1
                  if(rms.lt.rmsmin)then
                    fixdepn=' '
                    rmsmin=rms
                    xh2(1)=xh0(1)
                    xh2(2)=xh0(2)
                    xh0(1)=xhtr(1)
                    xh0(2)=xhtr(2)
                    nsol=2
                  endif
                  if(iulst.gt.0.and.(.not.multi_model))then
                    write(iulst,'('' Starting location from one '',
     &              ''azimuth and apparent velocity azimuth:''
     &              ,2f8.2)')alata,alona
                    write(iulst,'('' rms = '',f8.2)')rms
                  endif
                endif
              endif 
c added apparent_vel-distance location 6/98
              if(appar_veloc(i).gt.0.0.and.distind.eq.'D')then
                dlt1=find_delta('        ',110.7/appar_veloc(i),
     &           tt1,imat)
                az2=sngl(tp(i))
                call latlon(y0(1)/degtorad,x0(1)/degtorad,dlt1,
     &           az2,alata,alona)
                xhtr(1)=alona*degtorad
                xhtr(2)=alata*degtorad
                rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,
     &          nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,
     &          test,dtwt,dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,
     &          xnear,tph,distind,aninc,trphs,dtav,minflag,data,
     &          imatch,depthlim,iflagd,ires,fixor,scorr)
                nsol=1
                if(rms.lt.rmsmin)then
                  fixdepn=' '
                  rmsmin=rms
                  xh2(1)=xh0(1)
                  xh2(2)=xh0(2)
                  xh0(1)=xhtr(1)
                  xh0(2)=xhtr(2)
                  nsol=2
                endif
                if(iulst.gt.0.and.(.not.multi_model))then
                  write(iulst,'('' Starting location from one '',
     &            ''azimuth and apparent velocity distance:''
     &            ,2f8.2)')alata,alona
                  write(iulst,'('' rms = '',f8.2)')rms
                endif
              endif
            endif

c 10/94: added azimuth check
           if(kk.eq.1.and.st(i).ne.stat)then
c            write(iulst,*)' 2 ',st(i),phase(i),i,tp(i)
c    two azimuths: find the intersection of their great circles
            t2=sngl(tp(i))
            x2=x0(i)/degtorad
            y2=y0(i)/degtorad
c            write(iulst,*)x1,y1,x2,y2
            call azcros(y1,x1,t1,y2,x2,t2,dist1,dist2,
     &      alat1,alon1,ierr)

            if(ierr.ne.0.and.iulst.gt.0.and.(.not.multi_model))
     *      then

c             write(*,8000)t1,t2
c8000          format(' Azimuths ',f6.1,' and ',f6.1,
c     &        ' do not intersect')
            else
              nazsol=nazsol+1
              xhtr(1)=alon1*degtorad
              xhtr(2)=alat1*degtorad
              rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,
     &        nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,
     &        dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,
     &        distind,aninc,trphs,dtav,minflag,data,imatch,depthlim,
     &        iflagd,ires,fixor,scorr)
              if(rms.lt.rmsaz)then
                rmsaz=rms
                t1min=t1
                t2min=t2
                alataz=alat1
                alonaz=alon1
c                write(iulst,*)'*',alat1,alon1,rms
              endif

c try the antipode as well (if non-local)
              if(distind.ne.'L')then
                xhtr(1)=(alon1+180.)*degtorad
                xhtr(2)=-alat1*degtorad
                rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,
     &          nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,
     &          dtwt,dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,
     &          tph,distind,aninc,trphs,dtav,minflag,data,imatch,
     &          depthlim,iflagd,ires,fixor,scorr)
                nsol=1
                if(rms.lt.rmsaz)then
                  rmsaz=rms
                  t1min=-t1
                  t2min=-t2
                  alataz=-alat1
                  alonaz=alon1+180.
                  if(alonaz.gt.180.)alonaz=alonaz-360.
                endif
              endif
            endif
c            write(iulst,*)'X ',alon1,alat1
           endif
          endif 
        end do

        if(nazsol.gt.0.and.k.eq.1)then
          if(rmsaz.lt.rmsmin)then
            fixdepn=' '
            rmsmin=rmsaz
            xh2(1)=xh0(1)
            xh2(2)=xh0(2)
            xh0(1)=alonaz*degtorad
            xh0(2)=alataz*degtorad
            alata=alataz
            alona=alonaz
            nsol=2
          endif
            if(iulst.gt.0.and.(.not.multi_model))then
              write(iulst,'('' Starting location from two'',
     &        '' azimuths:'',14x,2f8.2)')alataz,alonaz
c              write(iulst,*)'az1,az2 ',t1min,t2min
              write(iulst,'('' rms = '',f8.2)')rmsaz
            endif
        endif
c      endif

c  ***************** 3. Find the multi-phase stations *******************

c  zero  counters
      do i=1,nphase
        ind1(i)=0
        ind2(i)=0
      end do

      kk=0

c  nstat is the number of different stations
      nstat=0
      ndist=0

c   bump the phase index
c change 10/93, BRL: now use arrival time order for k
130   kk=kk+1
      k=isort(kk)
c      write(iulst,*)st(k)

c   check for azimuths, blank phases and last phase
      if(kk.le.nphase)then
c before sep 10, 2001        if(phase(k)(1:2).eq.'AZ')go to 130
        if(phase(k)(1:2).eq.'AZ'.or.phase(k)(1:1).eq.' ')go to 130

c 10/94: keep these in
c change 10/93 BRL: include PS P-phases
c        if(dtwt(k).eq.0.0.and.ip(k).ne.9)go to 130

c   do we already have this station ?
        do i=1,nstat
c          write(iulst,*)i,st(k),st1(i)
          if(st(k).eq.st1(i))then

c   Yes:  bump the counter and get another phase

c change 10/93, BRL: changed nstat to i in following lines
c 4/94: changed phase limit to 20
            if(ind1(i).eq.20)then
              write(iulst,*)' Limit of 20 phases/station exceeded'
              go to 135
            endif  
            ind1(i)=ind1(i)+1
c            write(iulst,*)i,k,st1(i),ind1(i)

c  store the index of the second phase in ind3
            ind3(i,ind1(i))=k

            go to 130
          endif
135       continue
        end do

c   New station: bump the  station count, store the first phases 
c  index in ind2, the station ID in st1, then get another phase
        nstat=nstat+1
        ind2(nstat)=k
        st1(nstat)=st(k)
c        write(iulst,*)' new st ',nstat,k,st1(nstat)
        go to 130
      endif

c find the number of multiple phase stations, nmult
      nmult=0

      do i=1,nstat
        if(ind1(i).gt.0)nmult=nmult+1
      end do

c   stataz is used to store the ID of the azimuth 
c   station having a multiple phase (i.e. a distance)
      stataz='     '

      if(nmult.gt.0)then

        if(iulst.gt.0.and.(.not.multi_model))then
          write(iulst,'(/,i4,'' multiple-phase stations'')')nmult
        endif


        jaz=0

        if(naz.gt.0)then

c   find the first azimuth station having a multiple phase

c change 10/93, BRL: j now arrival time order
          do jj=1,nphase
            j=isort(jj)

            if(phase(j)(1:2).eq.'AZ'.and.dtwt(j).ne.0.0)then
              do i=1,nstat
                if(st(j).eq.st1(i).and.ind1(i).gt.0)then
                  jaz=j
                  stataz=st(j)
                  go to 591
                endif
              end do
            endif
          end do

591       continue

        endif

c   store the indices (nmp(k),nmp1(k)) and distances 
c  (delmp(k))  of all the multiple phases
        ii=nl
        if(nmoho.ne.0)ii=nmoho
        k=0

c iaz and kaz are used to store the indices of the first multiple-phase
c station which also has an azimuth
        iaz=0
        kaz=0

        if(iulst.gt.0.and.(.not.multi_model))write(iulst,*)

        do i=1,nstat
          do j=1,ind1(i)
            j1=ind2(i)
            j2=ind3(i,j)
            if(ind1(i).gt.0)then
              prm1=ucase(phase(j1)(1:1))
              prm2=ucase(phase(j1)(2:2))
              prm3=ucase(phase(j2)(1:1))
              prm4=ucase(phase(j2)(2:2))
            
c    Use the  multiple phase  to estimate an
c    approximate distance using surface or moho velocities (or vl)
              vvv=0.0
              vvv1=0.0

c Set the velocities of the two phases using the phase ID's
              if(prm3.eq.'S'.and.prm4.eq.'G')vvv=vs(1)
              if(prm3.eq.'S'.and.prm4.eq.'N')vvv=vs(ii)
              if(prm3.eq.'S'.and.prm4.eq.' ')vvv=vs(ii)
              if(prm3.eq.'P'.and.prm4.eq.'G')vvv1=v(1)
              if(prm3.eq.'P'.and.prm4.eq.'N')vvv1=v(ii)
              if(prm3.eq.'P'.and.prm4.eq.' ')vvv1=v(ii)
              if(prm3.eq.'L')vvv=vl
              if(prm1.eq.'P'.and.prm2.eq.'N')vvv1=v(ii)
              if(prm1.eq.'P'.and.prm2.eq.' ')vvv1=v(ii)
              if(prm1.eq.'P'.and.prm2.eq.'G')vvv1=v(1)
              if(prm1.eq.'S'.and.prm2.eq.'G')vvv=vs(1)
              if(prm1.eq.'S'.and.prm2.eq.'N')vvv=vs(ii)
              if(prm1.eq.'S'.and.prm2.eq.' ')vvv=vs(ii)
              if(prm1.eq.'L')vvv=vl
               
              if(vvv.gt.0.0.and.vvv1.gt.0.0)then

c bump multiple phase count and calculate distance
                k=k+1

c                write(iulst,*)' ',ip(j2),ip(j1),tp(j2),tp(j1)

c change 10/93, BRL
c  time difference for PS phase is already in tp(j2)
                if(ip(j1).ne.9)then
                  delmp(k)=abs(sngl(tp(j2)-tp(j1))
     &             /((1./vvv)-(1./vvv1)))
                else
                  delmp(k)=abs(sngl(tp(j2))/((1./vvv)-(1./vvv1)))
                endif

                nmp(k)=j1
                nmp1(k)=j2
                if(iulst.gt.0.and.(.not.multi_model))then
                  write(iulst,'(8x,a4,2x,2a1,''-'',2a1,4x,f8.1,'' km''
     &            )')st1(i),prm1,prm2,prm3,prm4,delmp(k)
                endif

                if(st1(i).eq.stataz)then
                  if(iaz.eq.0)iaz=i
                  if(kaz.eq.0)kaz=k
                endif
              endif
            endif
          end do
        end do

        if(iulst.gt.0.and.(.not.multi_model))write(iulst,*)
        ndist=k

c change 10/93 BRL
c I now calculate rms for all the following cases, rather
c than for only one of them. The minimum rms criterion
c is used to select the starting location

c   case A:  distance + 1 azimuth from  the same station
        if(iaz.ne.0.and.kaz.ne.0)then
          nsol=1
          ii=jaz
          azloc=sngl(tp(ii))

c          write(iulst,*)ii,st(ii),tp(ii),delmp(kaz)
          delt=delmp(kaz)/(6371.*degtorad)
          call latlon(y0(ii)/degtorad,x0(ii)/degtorad,delt,azloc,
     &    alata,alona)
          xhtr(1)=alona*degtorad
          xhtr(2)=alata*degtorad                                    
          rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,nmoho,
     &    nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc1,
     &    dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc,
     &    trphs,dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,
     &    scorr)
          nsol=1
          if(rms.lt.rmsmin)then
            fixdepn=' '
            rmsmin=rms
            xh2(1)=xh0(1)
            xh2(2)=xh0(2)
            xh0(1)=xhtr(1)
            xh0(2)=xhtr(2)
            nsol=2
          endif
            if(iulst.gt.0.and.(.not.multi_model))then
              write(iulst,'('' Starting location from distance +'',
     &        '' 1 azimuth: '',5x,2f8.2)')alata,alona
               write(iulst,'('' rms = '',f8.2)')rms
            endif
        endif

c 6/94 removed distance+apparent velocity azimuth - caused global location problems

        if(ndist.gt.1)then

c  case C:  more than one distance: 
c     find the length and direction of the line joining
c     the first two distance stations (indices nmp(k) and nmp(k1))
          k1=2
          k=1
600       continue
          j1=nmp(k)
          j2=nmp(k1)
c          write(iulst,*)' j1,j2 ',j1,j2
          ctheta=0.0
          if(st(j1).ne.st(j2))then
            call delaz(y0(j2),x0(j2),dekm,dedeg,az1,
     &      y0(j1),x0(j1))

c now find the orientation of the intersection point(s)
c******************************************************
c change brl 12/22/93: check that dekm and delmp(k) > 0
            if(delmp(k).gt.0.0.and.dekm.gt.0.0)then
              ctheta=(delmp(k)**2-delmp(k1)**2+dekm*dekm)/
     &        (2.*delmp(k)*dekm)

c 2/95: limit ctheta to +/- 1
              if(ctheta.gt.1.0)ctheta=1.0
              if(ctheta.lt.-1.0)ctheta=-1.0
              
            endif
c*********************************************************
          endif
          
c          write(iulst,*)ctheta,delmp(k),delmp(k1),dekm
          if(abs(ctheta).gt.1.0.or.st(j1).eq.st(j2))then 
            if(k1+1.le.ndist)then
              k1=k1+1
              go to 600
            elseif(k+1.le.ndist.and.k+1.ne.k1)then
              k=k+1
              go to 600
            else
              ndist=1
            endif
          else
c************************************************************
c change brl 12/22/93: when ctheta=0, azz=90
            if(ctheta.ne.0.0)then
              azz=atan(sqrt(1./(ctheta*ctheta)-1.))/degtorad
            else
              azz=90.
            endif
c*************************************************************
            if(ctheta.lt.0.0)azz=180.-azz
c           write(iulst,*)' dekm az1 azz ',dekm,az1,azz
            alat1=y0(j1)/degtorad
            alon1=x0(j1)/degtorad
            delt=delmp(k)/(6371.*degtorad)
            call latlon(alat1,alon1,delt,az1+azz,alata,alona)
            call latlon(alat1,alon1,delt,az1-azz,alatb,alonb) 

c 2/95: if ctheta=1 or -1 try other distance
            if(ctheta.eq.1.0.or.ctheta.eq.-1.0)then
             alat1=y0(j2)/degtorad
             alon1=x0(j2)/degtorad
             delt=delmp(k1)/(6371.*degtorad)
             call latlon(alat1,alon1,delt,az1+azz,alatb,alonb)
            endif 
c           write(iulst,*)alata,alona,alatb,alonb

c change 10/93: test rms of both solutions
            do ns=1,2
              if(ns.eq.1)then
                xhtr(1)=alona*degtorad
                xhtr(2)=alata*degtorad
              elseif(ns.eq.2)then
                xhtr(1)=alonb*degtorad
                xhtr(2)=alatb*degtorad
                alata=alatb
                alona=alonb
              endif
              rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,
     &        nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,
     &        dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,
     &        distind,aninc,trphs,dtav,minflag,data,imatch,depthlim,
     &        iflagd,ires,fixor,scorr)
              nsol=2
              if(rms.lt.rmsmin)then
                fixdepn=' '
                rmsmin=rms
                xh2(1)=xh0(1)
                xh2(2)=xh0(2)
                xh0(1)=xhtr(1)
                xh0(2)=xhtr(2)
                nsol=2
              endif
              if(iulst.gt.0.and.(.not.multi_model))then
                write(iulst,'('' Starting location from 2 distances:''
     &          ,2f8.2)')alata,alona
                write(iulst,'('' rms = '',f8.2)')rms
              endif
            end do

c change 10/93:removed 3 distance test - no longer necessary
          endif
        endif



        if(ndist.eq.1.and.iaz.eq.0.and.naz.gt.0)then

c Case D: one distance and one azimuth at different stations
          ii=nmp(1)

c  find the azimuth of the line joining the distance station 
c  to the reference point (azimuth station if naz > 0)
c  the coordinates of the first azimuth t1, and its station 
c  coordinates (x1,y1) have already been saved
          alat2=y0(ii)/degtorad
          alon2=x0(ii)/degtorad
          dist=delmp(1)
          call azdist(y1,x1,t1,alat2,alon2,dist,alata,alona,alatb,
     &    alonb,nsol)
          do ic=1,nsol
            xhtr(1)=alona*degtorad
            xhtr(2)=alata*degtorad
            rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,ndata,nmoho,
     &      nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,
     &      tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc,
     &      trphs,dtav,minflag,data,imatch,depthlim,iflagd,ires,fixor,
     &      scorr)

            if(rms.lt.rmsmin)then
              fixdepn=' '
              rmsmin=rms
              xh2(1)=xh0(1)
              xh2(2)=xh0(2)
              xh0(1)=xhtr(1)
              xh0(2)=xhtr(2)
              nsol=2
            endif
              if(iulst.gt.0.and.(.not.multi_model))then
                write(iulst,'('' Starting location from distance +'',
     &          '' another stat. azimuth: '',5x,2f8.2)')alata,alona
                 write(iulst,'('' rms = '',f8.2)')rms
              endif
            xhtr(1)=alonb*degtorad
            xhtr(2)=alatb*degtorad
          end do
        endif
      endif

c now make sure the selected starting location satisfies the critical
c distance for all stations with azimuths

      do i=1,nphase

        if((ucase(phase(i)(2:2)).eq.'N'.or.phase(i)(2:2).eq.'K'))then
c     &  .and.dtwt(i).ne.0.0)then

          dcrit1=dcrit

c set critical distance to test(59) km for core phases
c          if(phase(i)(2:2).eq.'K')dcrit1=test(59) !removed 6/98 BRL

          if(naz.gt.0)then
            do j=1,nphase

c change 10/93, BRL: exclude zero weight azimuths
c------------------------------------------------------------------
              if(phase(j).eq.'AZ  '.and.st(j).eq.st(i).and.dtwt(j)
     &        .gt.0.0)then
c-----------------------------------------------------------------
c  find the distance, dekm1, to the present starting location from this
c  station
       
                call delaz(alata*degtorad,alona*degtorad,dekm1,
     &          dedeg,azz,y0(i),x0(i))

                if(dekm1.lt.dcrit1)then

c move the start location out to the critical distance

                  delt=dcrit1/(6371.*degtorad)
                  az4=sngl(tp(j))
                  call latlon(y0(i)/degtorad,x0(i)/degtorad,delt,az4,
     &            alata,alona)
c                 write(iulst,*)'alata,alona ',alata,alona
                  xhtr(1)=alona*degtorad
                  xhtr(2)=alona*degtorad
                  rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,nn,
     &            nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,
     &            dtwt,dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,
     &            tph,distind,aninc,trphs,dtav,minflag,data,imatch,
     &            depthlim,iflagd,ires,fixor,scorr)
                  nsol=2
                  if(rms.lt.rmsmin)then
                    fixdepn=' '
                    rmsmin=rms
                    xh2(1)=xh0(1)
                    xh2(2)=xh0(2)
                    xh0(1)=xhtr(1)
                    xh0(2)=xhtr(2)
                    nsol=2
                    if(iulst.gt.0.and.(.not.multi_model))then
                      write(iulst,'('' Critical distance from station ''
     &                ,a5,'' phase '',a4,'' used along azimuth '',f6.1
     &                )')st(i),phase(i),az4
                      write(iulst,'('' rms = '',f8.2)')rms
                      go to 599
                    endif
                  endif
                endif
              endif
            end do
    
          elseif(maxno.gt.2)then

c  no azimuth data: use regression azimuth

            call delaz(alata*degtorad,alona*degtorad,dekm1,
     &      dedeg,azz,slatm,slonm)
            if(dekm1.lt.dcrit1)then

c move the start location out to the critical distance

              delt=dcrit1/(6371.*degtorad)
              call latlon(y0(i)/degtorad,x0(i)/degtorad,delt,azm1,
     &        alata,alona)                                        
     
c             write(iulst,*)'alata,alona ',alata,alona
              xhtr(1)=alona*degtorad
              xhtr(2)=alona*degtorad
              rms=rmsv(nphase,st,phase,tp,x0,y0,xhtr,nn,
     &        nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,
     &        dtw1,tpc1,dl,iselv,maxelv,ips,ip,dt,xfar,xnear,tph,
     &        distind,aninc,trphs,dtav,minflag,data,imatch,depthlim,
     &        iflagd,ires,fixor,scorr)
              nsol=1
              if(rms.lt.rmsmin)then
                fixdepn=' '
                rmsmin=rms
                xh2(1)=xh0(1)
                xh2(2)=xh0(2)
                xh0(1)=xhtr(1)
                xh0(2)=xhtr(2)
                if(iulst.gt.0.and.(.not.multi_model))then
                  write(iulst,'('' Critical distance for phase '',a8,
     &            '' used along regression azimuth '',f6.1
     &             )')phase(i),az1
                  go to 599
                endif
                nsol=2
              endif
            endif
          endif
        endif
      end do

599   continue
c      xh0(1)=alona*degtorad
c      xh0(2)=alata*degtorad

c 4/94: if apparent velocity start location, fix depth
      if(fixdepn.eq.'F')then
c       write(iulst,'(/'' Depth fixed for regression'',
c     & '' starting location''/)')
c       fixdep='F'      
      endif                
      
      return
      end


      subroutine ludcmp(a,n,np,indx,d)
      implicit double precision (a-h,o-z)
      save
      parameter (nmax=100,tiny=1.0d-20)
      dimension a(np,np),indx(n),vv(nmax)
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (dabs(a(i,j)).gt.aamax) aamax=dabs(a(i,j))
11      continue
C        if (aamax.eq.0.) pause 'singular matrix.'
        if(aamax.ne.0.0)vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*dabs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=tiny
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      end

      subroutine lubksb(a,n,np,indx,b)
      implicit double precision (a-h,o-z)
      dimension a(np,np),indx(n),b(n)
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
      end
      character*1 function lcase(a)

c    convert upper case single char to lower case - machine independent

      character*1 chr(26),ucchr(26),a
      data chr/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     &'n','o','p','q','r','s','t','u','v','w','x','y','z'/
      data ucchr/'A','B','C','D','E','F','G','H','I','J','K','L','M',
     &'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      do 10 i=1,26
        if(a.eq.ucchr(i))then
          lcase=chr(i)
          return
        endif
10    continue
      lcase=a
      return
      end
      
      real function find_delta(m_phs,p,tt1,imat)
c  uses iasp91 trtm routine to find offset, delta in degrees, at which
c  horiz. deriv of travel time for phase phs matches slowness p
c  Barry Lienert Jun 98
      implicit none
      save
      include 'hypparm.inc'
      character*8 phs,m_phs
      real p,p1,change,dpdd,delta,del_delta,tt1
      integer i,imat,n,niter,ilen
c find approximate first delta from empirical formula
      delta=(10.46-p)/0.067
      p1=-999.
      if(m_phs.eq.'        ')then
       call trtm(delta,niasp,n,tt,dtdl,dtdh,dddp,phcd)
       imat=0
       i=1
       do while (imat.eq.0.and.i.le.n)
        if(abs(dtdl(i))/abs(p).lt.2..and.abs(p)/abs(dtdl(i)).gt..5)then
         imat=i
        endif
        i=i+1 
       end do                   
       if(imat.ne.0)then
        phs=phcd(imat)
       else
        imat=1
        phs=phcd(imat)
       endif  
      else
       phs=m_phs
      endif 

      ilen=len(m_phs)

      niter=0
      do while (abs(p-p1).gt..01.and.niter.le.50)
c get iasp91 values at this delta      
       call trtm(delta,niasp,n,tt,dtdl,dtdh,dddp,phcd)
      
c look for the phase in the iasp91 phase list
       imat=0            
       i=1
       do while (imat.eq.0.and.i.le.n)
        if(phs(1:ilen).eq.phcd(i)(1:ilen))imat=i
        i=i+1
       end do                              
c no matching phase: return 
       if(imat.eq.0)then
        find_delta=(10.46-p)/0.067
        return
       endif          
c find approx. derivative, dpdd, of p w.r.t. delta       
       change=.05
       p1=dtdl(imat)
       if(dtdl(imat).lt.p)then
        call trtm((1.+change)*delta,niasp,n,tt,dtdl,dtdh,dddp,phcd)
        imat=0            
        i=1
        do while (imat.eq.0.and.i.le.n)
         if(phs.eq.phcd(i))imat=i
         i=i+1
        end do                              
c no matching phase: return 
        if(imat.eq.0)then
         find_delta=(10.46-p)/0.067
         return
        endif          
        dpdd=(dtdl(imat)-p1)/(change*delta)
       else
        call trtm((1.-change)*delta,niasp,n,tt,dtdl,dtdh,dddp,phcd)
        imat=0            
        i=1
        do while (imat.eq.0.and.i.le.n)
         if(phs.eq.phcd(i))imat=i
         i=i+1
        end do                              
c no matching phase: return 
        if(imat.eq.0)then
         find_delta=(10.46-p)/0.067
         return
        endif          
        dpdd=(p1-dtdl(imat))/(change*delta)
       endif
c find new delta by extrapolating this derivative
       if(dpdd.ne.0.0)then
        del_delta=(p-p1)/dpdd
       else
        find_delta=(10.46-p)/0.067
        return
       endif 
c limit change in delta to 5 degrees
       if(del_delta.gt.5.)del_delta=5.
       if(del_delta.lt.-5.)del_delta=-5.
       delta=delta+del_delta 
       niter=niter+1
      end do
      if (niter.eq.20)then
       find_delta=(10.46-p)/0.067
       return
      endif 
      find_delta=delta
      tt1=tt(imat)
      return 
      end
