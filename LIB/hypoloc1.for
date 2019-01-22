c
c
c
c*************************************************************************
c this function is used to calculate derivatives, travel times, weights
c and the rms misfit for a set of phases, tp(i), i=1,nphase, and an event
c location, xh(i), i=1,3
c The derivatives are put in the COMMON array g(i,j)
c
c                   Version 4.0 6/98
c
c         Modified bisquare residual weighting section -
c         this was erroneously removing the average travel time
c         residual, preventing this type of weighting from working 
c         added distind to window and logic to force local or
c         IASP91 travel times/derivatives
c
c       inputs:  
c                hypocenter                xh  
c                average residual          dtav
c                number of phases          nphase
c                layered velocity  model   v,vs,nl,nmoho,nconrad
c                distance indicator        distind
c                arrival times/azimuths    tp  
c                station delays            dl
c                phase ID's                phase
c                station coordinates       x0, y0, iselv
c
c       outputs:
c                mean residual             dtav
c                centered residuals        dt
c                residual weights          dtw1
c                travel times              tpc1
c                matched phase ID's        trphs
c                angles of incidence       aninc
c                partial derivatives       g
c
c
c change 10/93: added minflag to window
c change 4/94: changed to include 'b' phases
c
c 4/94 changed so function now returns with centered residuals & origin time
c dtav no longer needed

c 10/94:  added iflagd to window
CJAB(BGS)Jan95 : Installed error handling to file I/O
c 1/95: added duplicate phase check
c 2/95: added fixor & ires to window
c jul 95 by jh: fix bug pointed out by barry: pz=sqrt(abs...
c jun 98 BRL: updates from my version added          
c 6/98 BRL: changed ielv to iselv
c sep 98 by jh : ------------ seisan version 6.0 check ------------------
c                added double precision az1 to travel time routine                
c sep 23 by bl : changed to rmd calculation to be consistent with 
c                seisan version 6.0
c oct  15   jh : 5 character station codes
c
c oct 28 1998 bmt : linux changed, save and *
c jl 13 99 jh     : weight 4 work again
c oct 19 99 jh    : change test(99) to test(94) for residual weight 
c                   reintroduction of weighted out phases
c nov 99   jh     : remove parm,v,vs,nl from window, in common block
c jan feb         : multi layer 
c jan 8, 2001, jh : fix problem of Pn for deep enevet when using IASP
c                   software
c aug 20 2002 jh+bl : fix problem of wrong location in sequence
c
c apr 8 2005  jh   : more problem with wrong origin time for events
c                    in sequence. event was not properly identified as
c                   distant if onlyu SS PP phases etc, not clear where
c                   problem we was, proably in calculation of ditance.
c                   problem solved by reintroducing force of using iasp91
c                  when event is distant.  look for variable iflagd
c may 2008 bl : fix problem of az weight were previously weighted to zero
c sep 5 2012 lo : don't report lack of travel times for amplitude phases
c oct 12 2012 jh: multiply residual weight with individual weight, look
c                 for 'jh oct 12 2012'
c 2014-03-20 pv: replaced PAUSE statement, not a fortran standard feature 
c 2014-04-29 pv: fixed return statement could not be reached
c
c
      function rmsv(nphase,st,phase,tp,x0,y0,xh,ndata,nmoho,
     &nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,tpc1,dl,
     &iselv,maxelv,ips,ip,dt,xfar,xnear,tph,distind,aninc,trphs,dtav,
     &minflag,data,imatch,depthlim,iflagd,ires,fixor,scorr)

      save
      include 'hypparm.inc'
      include 'libsei.inc'                    ! Library definitons.

      external sei code                       ! Error encoder.
      integer  code,                          ! Condition.
     &         iulst                          ! Output diagnostics.
      logical  b_flag                         ! Flag it?.
c
      character*1 ucase,prm1,prm2,prmd
      character*8 phsid,prevphs
      character*5 prevstat
      
      integer ires
      real usrc(2)    ! added aug 2002 by jh, barry's suggestion
      rmsv=0.0        ! ----------------------------------------

      imflag=0
      pi=3.14159265
      degtorad=pi/180.
 
c 11/94 change from 6378.2 to mean radius
      rearth=6371.

c    set L velocity to test(51)

      vl=test(51)
      nn=2*nl-1

c----------------------------------------------------------
c change 10/93 BRL. iflagd now defined outside the next do loop 
c I now test all phases at this point to determine whether
c to use the IASP91 routines (iflagd=1)

C
      iflagd=0   !removed 6/98, BRL , put in again jh apr 2005

c      do i=1,nphase
c        call distaz(y0(i)/degtorad,x0(i)/degtorad,xh(2)/degtorad,
c     &  xh(1)/degtorad,dedeg,azz,azb)
c        delta=dedeg*degtorad*rearth
c        if(delta.gt.test(57).and.distind.ne.'L')then
c         iflagd=1
c         write(*,*)st(i),phase(i),delta
c        endif 
c      end do

c remove this comment to force IASP91 calculation for 'D' events
c below put in again apr 2005 by jh
      if(distind.eq.'D')iflagd=1
c      write(*,*)' iflagd ',iflagd

c remove this comment for 'R' events to be treated as local
c            if(distind.eq.'R')iflagd=0
c------------------------------------------------------------
c       adjust origin time to input dtav
c      tph=tph+dtav
c 1/95: added prevstat, prevphs & ipcount
      prevstat='    '
      prevphs='        '
 
      do ii=1,nphase
 
c 1/95: added section to get ipcount, the # of duplicate phases at the same stat
       if(st(ii).ne.prevstat)then
        ipcount=0
        prevphs=phase(ii)
       else   !changed 6/98, BRL
        if(phase(ii)(1:4).ne.'    '.and.phase(ii).eq.prevphs
     &   .and.dtwt(ii).gt.0.0)then
         dtwt(ii)=-1.0  !change 6/98 - weight duplicate phases to 0
         write(iulst,'(a5,1x,a8,'' - duplicate phase: weighted to -1''
     &    )')st(ii),phase(ii)
         ipcount=ipcount+1
        else
c 2/95: changed ipcount=1 to ipcount=0
         ipcount=0
        endif
       endif 
       prevstat=st(ii)
       prevphs=phase(ii)

c 11/22 changed to assign iflagd for each phase
c jh april 2005       iflagd=0

       call distaz(y0(ii)/degtorad,x0(ii)/degtorad,xh(2)/degtorad,
     &  xh(1)/degtorad,dedeg,azz,azb)
       delta=dedeg*degtorad*rearth    !added 6/98
       if(azb.lt.0.0)azb=azb+360.
c        azback(ii)=azb
       if(delta.gt.test(57).and.distind.ne.'L')then
        iflagd=1
       endif
c----------------------------------------------------------------
c change 10/93 BRL. Put contents of this loop in subroutine trtime
c         write(16,*)st(i),x0(i)/degtorad,y0(i)/degtorad,iselv(i),
c     &xh(1)/degtorad, xh(2)/degtorad,xh(3)

c trtime returns with dtw1 set to dtwt*distance weight,
c or to -1 for invalid phases 
c 1/95: added ipcount to window
         call trtime(ii,ipcount,st,phase,tp,x0,y0,xh,ndata,
     &   nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,
     &   tpc1,dl,iselv,maxelv,ip,dt,xfar,xnear,tph,aninc,trphs,
     &   iflagd,minflag,data,distind,imatch,depthlim,scorr,ph,dx)
c----------------------------------------------------------------
c         write(iulst,*iostat=code)st(i),tpc1(i),dtw1(i)
         call sei code(fort$,code,iulst,b_flag) ! Process error.
      end do

c <<<<<<<<<<<<<<<<<<<<<<<<<<<  end phase loop  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c      call r4sort(nphase,azback,isortaz)  !added 6/98,BRL

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c                      RMS CALCULATION      
c
c change 10/93: changed this whole section (it's finally in one place!!)

c    S-P and S-L pairs  (P phase has a weight of 9)

      do i=1,nphase
        if(ip(i).eq.9)then
          prm1=ucase(phase(i)(1:1))
          if(prm1.eq.'P')then

c    Use time difference derivatives for  L and S phases at this station

            do j=1,nphase
              if(st(j).eq.st(i).and.i.ne.j)then
                prm2=ucase(phase(j)(1:1))
                if(prm2.eq.'L'.or.prm2.eq.'S')then

c   set the travel times and derivatives equal to their differences
c   ips(j)=1 in the S or L phase position signifies a P-S or P-L difference. 
c   The time difference is stored in the S-phase index position.

c  only set the raw data to the differences once
                  if(ips(j).eq.0)then
                   ips(j)=1
                   tp(j)=tp(j)-tp(i)
c                   write(iulst,*,iostat=code)tp(j)-tp(i)
                   call sei code(fort$,code,iulst,b_flag) ! Process error.
                  endif

c since the theoretical travel times and derivatives are
c recalculated each time, recalculate their differences
c every time
                  tpc1(j)=tpc1(j)-tpc1(i)
c                  write(iulst,*,iostat=code)st(j),phase(j),tpc1(j)
                  call sei code(fort$,code,iulst,b_flag) ! Process error.
                  g(j,1)=g(j,1)-g(i,1)
                  g(j,2)=g(j,2)-g(i,2)
                  g(j,3)=g(j,3)-g(i,3)

c #################  P-S RESIDUALS  #############################
                  dt(j)=tp(j)-tpc1(j)-dl(j)+dl(i)            
                  
c                  write(iulst,*,iostat=code)' dt1 ',
c     &               st(j),phase(j),dt(j)
                  call sei code(fort$,code,iulst,b_flag) ! Process error.
c---------------------------------------------------------------
c change 10/93 BRL. Put wt calculation inside this loop.
c Previously the last phase's wt was being used for all phases

c         distance weight factor, wt

                  call distaz(y0(j)/degtorad,x0(j)/degtorad,
     &            xh(2)/degtorad,xh(1)/degtorad,dedeg,azz0,azb)
                  delta=rearth*dedeg*degtorad
                  wt=1.0
                  if(xfar.ne.0.0)then
                    if(delta.gt.xfar)wt=0.0
                    if(delta.gt.xnear.and.delta.le.xfar)
     &              wt=(xfar-delta)/(xfar-xnear)
                  endif
c------------------------------------------------------------------

c        use S or L weight for time difference

c change 10/93: don't reset negative weights
c start with initial weights, dtwt
                  if(dtw1(j).gt.0.0)then
                    if(iflagd.eq.0)dtw1(j)=dtwt(j)*wt ! distance weight for local events
                    if(iflagd.eq.1)dtw1(j)=dtwt(j)    ! no distance weight for distant ev.
                  endif

c                 write(iulst,*,iostat=code)dtw1(j),dtw1(i),i
                  call sei code(fort$,code,iulst,b_flag) ! Process error.

c     if other phase is invalid, difference is too

                  if(dtw1(i).lt.0.0)dtw1(j)=-1.0
c----------------------------------------------------------------
c change BRL 10/93: put this back in, but using trphs(j) instead
c of phase(j)

                 write(trphs(j),'(a2,''-P'')')phase(j)(1:2)
c---------------------------------------------------------------
                endif
              endif
            end do
          endif
        endif
      end do

c icount is a counter used to repeat the rms calculation after residual
c weighting is applied. This is necessary since the rms is needed first 
c to determine if residual weighting should be applied using test(36).
      icount=0    
c      dtav=0.0   !removed 6/98, BRL                                              

8700  icount=icount+1

c calculate dt(i) of non-zero weight absolute arrivals, 
c and their sum (used for dtav)

c changed to calculate weighted sum 4/94: also only calculate dtav 1st time thru
c      if(icount.eq.1)then
       sum=0.0
       ndata1=0       
       wsum=0.0                                 

c   calculate origin time using the weighted mean of all absolute
c   times (exclude azimuths & P-S diffs) minus their calculated values
       do i=1,nphase
        ww=0.0

        if(dtw1(i).gt.0.0)ww=dtw1(i)*dtw1(i)  !changed 6/98, BRL

c the dt(i) are calculated here using the input origin time
c 10/94: exclude difference phases and azimuths
        if(ips(i).ne.1.and.phase(i)(1:2).ne.'AZ')then
         dt(i)=tp(i)-tpc1(i)-dl(i)-tph
c 6/98, BRL: correct minute errors
         if(abs(dt(i)).gt.50..and.abs(dt(i)).lt.70..and.icflag.eq.1
     &    .and.dtw1(i).ge.0.0.and.test(97).gt.0.0)then
          write(iulst,*)st(i),phase(i),' minute corrected'
          if(dt(i).lt.0.0)then
           dt(i)=dt(i)+60.
           tp(i)=tp(i)+60.
          else
           dt(i)=dt(i)-60.
           tp(i)=tp(i)-60.
          endif              
          dtwt(i)=1.0
          dtw1(i)=1.0
          ww=1.0      
          imflag=1
         endif           
c 6/98 BRL: exclude PS phases   Before fix below, jul 99 (jh)
c        if(dtwt(i).eq.0.0.and.abs(dt(i)).le.5..and.icflag.eq.1
c    &    .and.ip(i).ne.9)then
c         write(iulst,*)st(i),phase(i),' added'
c         dtwt(i)=1.0
c         dtw1(i)=1.0
c        endif 
c  6/99  changed residual limit from 5 to test(99) so re-addition of zero
c  weight phases
c  can be disabled
c  19-10 99 changed test(99) to test(94) since already used
         if(dtwt(i).eq.0.0.and.abs(dt(i)).lt.test(94)
     &     .and.icflag.eq.1.and.ip(i).ne.9)then
          write(iulst,*)st(i),phase(i),' added'
          dtwt(i)=1.0
          dtw1(i)=1.0
         endif

        endif 
c 10/94: change ge.0 to gt.0 also moved below dt(i) calculation
        if(dtw1(i).gt.0.0.and.phase(i)(1:2).ne.'AZ'.and.
     &  ips(i).ne.1)then    !changed eq.0 to ne.1 6/98, BRL
          sum=sum+dt(i)*ww
          wsum=wsum+ww
          ndata1=ndata1+1
        endif
c        tph1=tph
c        if(ips(i).eq.1)tph1=0.0
c        write(iulst,*,iostat=code)st(i),phase(i),ips(i),tpc1(i),
c     &        tp(i)-tph1,dtw1(i)
        call sei code(fort$,code,iulst,b_flag)
       end do
c########################################################################
c      write(iulst,'(10f8.1)',iostat=code)(dt(i),i=1,nphase)
       call sei code(fort$,code,iulst,b_flag) ! Process error.
c      write(iulst,'(10f8.3)',iostat=code)(dtw1(i),i=1,nphase)
       call sei code(fort$,code,iulst,b_flag) ! Process error.
       rmsv=999.
       dtav=0.0
       if(wsum.gt.0.0)dtav=sum/wsum        
c      endif
       
c      write(iulst,*,iostat=code)' dtav',dtav
      call sei code(fort$,code,iulst,b_flag) ! Process error.

c  update average input residual
c       dtav=dtav+dtav1
 
c   update origin time using new (weighted) average residual
c      tph=tph+dtav1   
 
c      write(iulst,*,iostat=code)' dtav',dtav,icount
      call sei code(fort$,code,iulst,b_flag) ! Process error.

c-----------------------------------------------------------------------------
c        calculate rmsv
c
c****************************************************************
c 9/20/98: changed this calculation to make it consistent with the
c definition of weighted mean given in "The Statistical Analysis
c of Experimental Data", John Mandel, Dover Publications, NY, 1964
c p 132. 
c
c Mandel uses mean{t_i}=sum{w_i*t_i}/sum{w_i}
c and rms=sum{w_i*t_i^2}/sum{w_i}
c
c This is different for both the previous methods I used in v 3.2 
c and 4.0, athough the differences are small except when azimuth
c data is included
c******************************************************************

c ndata is the total number of valid data with dtw1(i)>0, wsum is
c the sum of their weights and sum is the sum of squared residuals
      ndata=0
      sum=0.0
      wsum=0.0                     
      wsum1=0.0

      do i=1,nphase
        dtav2=dtav  
        ww=0.0       !added 6/98, BRL
        
c  don't center PS, PL differences
        if(ips(i).eq.1.or.phase(i)(1:2).eq.'AZ')dtav2=0.0


c don't use negative weighted phases
        if(dtw1(i).gt.0.0.and.phase(i)(1:2).ne.'AZ'.and
     &   .ips(i).eq.0)then
          ww=dtw1(i)
          wsum=wsum+ww  !change 9/98, BRL
        endif
c
c added may 2008 bl - was previously weighing azimuths to zeroe in rms, added 
c next

        if(dtw1(i).gt.0.0. and. phase (i)(1:2).eq.'AZ') then
           ww=dtw1(i)
        endif

c include everything (including azimuths) in weighted rms 
        if(dtw1(i).gt.0.0)then
          sum=sum+ww*(dt(i)-dtav2)**2 
          wsum1=wsum1+ww  !change 9/98, BRL
          ndata=ndata+1
        endif
c         write(iulst,'(2a4,2f8.2)',iostat=code)st(i),
c     &           phase(i),dtw1(i),dt(i)-dtav2
        call sei code(fort$,code,iulst,b_flag) ! Process error.
      end do
c      write(iulst,*,iostat=code)' ndata,dtav,tph ',
c     &          ndata,ndata1,dtav,tph
      call sei code(fort$,code,iulst,b_flag) ! Process error.

      if(wsum.ne.0.0)then
       rmsv=sqrt(sum/wsum1)
      else
       rmsv=9999.
      endif  
c      write(iulst,*,iostat=code)' rms, tph ',rmsv,tph
      call sei code(fort$,code,iulst,b_flag) ! Process error.
c      write(iulst,*)icount,dtav,iflagd,tph,rmsv,test(88)

      if(icount.eq.1)then
       iweightflag=0
       if((rmsv.le.test(36).and.iflagd.eq.0).or.(rmsv.le.
     &  test(88).and.iflagd.eq.1))iweightflag=1
      endif






     
c 10/94: changed condition to both distant and local
c      if(icount.eq.2.or.iweightflag.eq.0)then
       do i=1,nphase
        dtav2=dtav
        
c  don't center PS, PL differences
        if(ips(i).eq.1.or.phase(i)(1:2).eq.'AZ')dtav2=0.0

c 2/95: don't center if origin time fixed. ires.eq.9 removed 6/98, BRL
        if(ires.eq.2.or.ucase(fixor).eq.'F'.or.test(31).eq.0.0)dtav2=0.0 

c #################### CENTER THE RESIDUALS  #######################
        dt(i)=dt(i)-dtav2    
       end do 

c 6/98, BRL: update the origin time
       if(ires.ne.2.and.ucase(fixor).ne.'F'
     & .and.test(31).gt.0.0)then
        tph=tph+dtav   
       endif  

       if(iweightflag.eq.0.or.icount.eq.3)return         ! PAU !!

c 10/93: inserted bisquare weighting section - formerly in hyploc

c ++++++++++++++++ bisquare weighting section +++++++++++++++++++++++++++++++
c    bisquare residual weighting... see Anderson, Phys. Earth 
c    Planet. Int. 30 (1982)

c change 10/94: use test(36) for local, test(88) for distant
c calculate the number of non-zero weight absolute arrivals, ndata,
c and their sum (used for dtav)

c change 4/94: repeat this k loop twice to redetermine weights using weighted dtav
         c=test(35)

c find the absolute values of the residuals
         do i=1,nphase
c          write(iulst,*,iostat=code)st(i),phase(i),dt(i),dtav2
          call sei code(fort$,code,iulst,b_flag) ! Process error.
          dt2(i)=abs(dt(i))                 
         end do

c get their median, sm - this is less affected by outliers than the mean
         call median(dt2,dtw1,phase,nphase,sm,ips)                      
         
c set sm to test(34) if sm<test(34)        
         if(sm.lt.test(34))sm=test(34)

c         write(iulst,*,iostat=code)c,sm
         call sei code(fort$,code,iulst,b_flag) ! Process error.

c now calculate the residual weights
         sum=0.0
         wsum=0.0  
         ndata=0 !added 6/98, BRL
         
         do i=1,nphase
          if(dtw1(i).gt.0.0.and.phase(i)(1:2).ne.'AZ')then
             dt2(i)=dt2(i)*0.6745/abs(sm)
c changed 6/98, BRL  <<<<<<<<<<<<<<<<<<<<<<<<<<          
            if(abs(dt2(i)).le.c)then
             ndata=ndata+1
             dtw1(i)=1.-(dt2(i)/c)**2    !*dtw1(i)
             dtw1(i)=dtw1(i)*dtwt(i)     ! jh oct 12 2012
            else
             dtw1(i)=0. 
            endif 
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c 4/94: set residual weights < 0.01 to zero
C            if(dtw1(i).lt.0.01)dtw1(i)=0.0 
c            if(k.eq.2)write(iulst,'(2a4,2f12.2)',
c     &                   iostat=code)st(i),phase(i),dtw1(i)
            call sei code(fort$,code,iulst,b_flag) ! Process error.
c          removed 6/98, BRL
c          if(dtw1(i).gt.0.0.and.phase(i)(1:2).ne.'AZ'.and.
c     &    ips(i).eq.0)then
c            sum=sum+dt(i)*dtw1(i)
c            wsum=wsum+dtw1(i)
          endif  
c         dtav=0.0                  !removed 6/98
        end do

        if (ndata.eq.0)return   !moved outside loop 6/98
c update dtav, the mean residual so that its newly weighted will be used for k=2
c this reduces the "skewing" due to large outliers, as these will be weighted
c to 0 the second time through
c         if(wsum.gt.0.0)dtav=sum/wsum    !removed 6/98

c       recalculate rmsv
c       go to 8700
        if (ndata.NE.0) go to 8700

c------------------------------------------------------------------
      return
      end

c*****************************************************************
c  subroutine added 10/93 to calculate individual travel times
c 4/94: modified depth origin - input xh(3) is now always relative 
c to sea level

c  last parameter, minflag, is defined as follows:
c       minflag = 0:   match 4 char. phase ID in phase(i)
c                      2nd letter is case insensitive
c       minflag = 1    use minimum time for 1st letter phase(i)(1:1)
c       minflag = 2    use refracted 'n' phases, set other phase 
c                      weights=-1
c       minflag = 3    use surface 'g' phases
c       minflag = 4    use Conrad 'b' phases
c       minflag = 5    use only minimum time P, Pn, Pg and pP phases no PKP's
c                      all other phases weighted to 0

c 10/94: included station elevation, xsd(3), in ellipticity correction
c 1/95: added ipcount, the # of duplicate phases at the same station
 
      subroutine trtime(ii,ipcount,st,phase,tp,x0,y0,xh,ndata,
     &   nmoho,nconrad,iulst,dtdl,dtdh,dddp,tt,phcd,test,dtwt,dtw1,
     &   tpc1,dl,iselv,maxelv,ip,dt,xfar,xnear,tph,aninc,trphs,
     &   iflagd,minflag,data,distind,imatch,depthlim,scorr,ph,dx)

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
      character*1 ucase,prm1,prm2,prmd
      character*8 phsid 
      integer ii,ipcount
      double precision az1                              ! added jh sep  18 , 98
      real xsd(3),xhc(3)
c
      pi=3.14159265
      degtorad=pi/180.

c 11/94: changed from 6378.2 to mean radius
      rearth=6371.
      
      nn=2*nl-1      
      
c 4/94: find depth of moho & conrad
      dconrad=0.0
      dmoho=0.0 
      sum=0.0
      do k=1,nl
       if(dconrad.eq.0.0.and.k.eq.nconrad)dconrad=sum
       if(dmoho.eq.0.0.and.k.eq.nmoho)dmoho=sum
       sum=sum+parm(nl+k)                  
      end do                                      
      dcorr=0.0
      if(test(40).eq.0.0)dcorr=float(maxelv)*.001
      
c    set L velocity to test(51)
      vl=test(51)

c    xs(1), xs(2) are the station long. and lat.
      xs(1)=x0(ii)
      xs(2)=y0(ii)

c xs is only used by dtdx2, which uses the maximum elevation station as its depth origin
      xs(3)=float(maxelv-iselv(ii))*0.001

c 10/94: added station coords xsd
      xsd(1)=xs(1)
      xsd(2)=xs(2)
      xsd(3)=-float(iselv(ii))*.001
      
      prm1=ucase(phase(ii)(1:1))
      prm2=ucase(phase(ii)(2:2))

c change 10/93: minimum time calc
      if(minflag.eq.0)prmd=prm2
      if(minflag.eq.1.or.minflag.eq.5)prmd=' '
      if(minflag.eq.2)prmd='N'
      if(minflag.eq.3)prmd='G'
      if(minflag.eq.4)prmd='B'

c 10/93: if phase is not the one specified by minflag, exit with dtw1(ii)=0.0
c 5/94: changed prm2.ne.' ' to prmd.ne.' '
c 7/94: changed weight from -1 to 0
      if(prm2.ne.prmd.and.prmd.ne.' '.and.minflag.ne.5)then
        dtw1(ii)=-1.0
        return
      endif

c 10/94: added test(90) to exclude core phases
      if(test(90).eq.0.0)then
       iflag2=0
       do j=1,8
        if(phase(ii)(j:j).eq.'K')iflag2=1
       enddo 
       if(iflag2.eq.1)then
        dtw1(ii)=-2.0
       endif 
      endif
       
c 4/95: added minflag=5: exclude secondary phases  (saves time at this point)    
      if(minflag.eq.5)then

c 10/94: changed iflag to iflag2 
       iflag2=0

       if(prm1.eq.'S'.or.prm2.eq.'S')iflag2=1
       if(phase(ii)(2:2).eq.'K'.or.phase(ii)(3:3).eq.'K')iflag2=1
       do k=1,4
        if(ucase(phase(ii)(k:k)).eq.'S')iflag2=1
        if(ucase(phase(ii)(k:k)).eq.'I')iflag2=1
        if(ucase(phase(ii)(k:k)).eq.'C')iflag2=1
        if(ucase(phase(ii)(k:k)).eq.'''')iflag2=1
       enddo 
       if(phase(ii)(2:3).eq.'KK')iflag2=1
       if(phase(ii)(1:2).eq.'PP'.or.phase(ii)(2:3).eq.'PP')iflag2=1
       if(iflag2.eq.1)then

c 7/94: flag secondary arrivals with -2 weight - set to zero later
        dtw1(ii)=-2.0
c        return
       endif                               
      endif                  
      
c 10/94: exclude depth phases if test(89)=0.0
      if(test(89).eq.0.0)then
       if(phase(ii)(1:1).eq.'p'.or.phase(ii)(1:1).eq.'s')then
        dtw1(ii)=-2.0
       endif
      endif        
      
c added 4/94 to exit for blank phases with imatch=1
      if(phase(ii)(1:4).eq.'    '.and.imatch.eq.1)then
        dtw1(ii)=-1.0
        return
      endif
      
c get delta and azimuth for this phase
      call distaz(xs(2)/degtorad,xs(1)/degtorad,xh(2)/degtorad,
     &xh(1)/degtorad,dedeg,azz,azb)
      delta=dedeg*degtorad*rearth

c distance weight factor, wt (only used for local events)
      wt=1.0
      if(xfar.ne.0.0)then
        if(delta.gt.xfar)wt=0.0
        if(delta.gt.xnear.and.delta.le.xfar)
     &  wt=(xfar-delta)/(xfar-xnear)
      endif

c change 10/94: no longer set dtw1(ii)=-1.0

c   P and S phases:
      if(prm1.eq.'P'.or.prm1.eq.'S'.or.prm1.eq.' ')then

c    only recognizes 'N', 'G', 'B' or ' ' phases in local case

c icount is used to allow two changes of a refracted "N" phase to a "B"
c then a "G" surface phase
        icount=0

777     continue

c 10/93: distind now forces layered model times
        if(iflagd.eq.0.or.distind.eq.'L'.and.prm1.ne.' ')then

c 7/94: forces depset call for next distant event
          iflagi=0                               
          
c moho, conrad check - transferred from hypoloc 4/94 - only want
c in layered model case
         if(prmd.eq.'B'.and.nconrad.eq.0) then
c          write(*,*)prmd,phase(ii),minflag
          write(*,*)' B phase specified, Conrad not defined'
          if(iulst.gt.0.and.(.not.multi_model))
     *    write(iulst,*,iostat=code)
     &    ' B phase specified,',
     &    ' Conrad not defined'
          call sei code(fort$,code,iulst,b_flag) ! Process error.
          trphs(ii)='        '
          dtw1(ii)=-1.0
          tmin=0.0
          return
         endif

         if(prmd.eq.'N'.and.nmoho.eq.0) then
          write(*,*)' N phase specified, moho not defined'
          if(iulst.gt.0.and.(.not.multi_model))
     *    write(iulst,*,iostat=code)
     &    ' N phase specified,',
     &    ' moho not defined'
          call sei code(fort$,code,iulst,b_flag) ! Process error.
          trphs(ii)='        '
          dtw1(ii)=-1.0
          tmin=0.0
          return
         endif

c    If N phases, don't allow xh(3) > d(nmoho)
c         write(iulst,*)prmd,xh(3),dmoho,dcorr
         call sei code(fort$,code,iulst,b_flag)
         if(prmd.eq.'N'.and.xh(3).gt.dmoho-dcorr)
     &    then
c          write(iulst,*)phase(ii),dmoho,dcorr
          if(test(47).eq.1.0)then  
           xh(3)=0.999*dmoho                                         
           if(test(40).eq.0.0)xh(3)=0.999*(dmoho-dcorr)
           call sei code(fort$,code,iulst,b_flag) ! Process error.
           if(iulst.gt.0.and.(.not.multi_model))
     *     write(iulst,'('' depth limited to moho at '',
     &     f6.1,'' km'')')xh(3)
           write(*,'('' depth limited to moho at '',
     &     f6.1,'' km'')')xh(3)
           depthlim=xh(3)
          endif
         endif 

c    If B phases, don't allow xh(3) > d(nconrad)
         if(prmd.eq.'B'.and.xh(3).gt.dconrad-dcorr)then
c changed 6/98, BRL <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          if(test(47).eq.1.0)then          
           xh(3)=0.999*dconrad
           if(test(40).eq.0.0)xh(3)=0.999*(dconrad-dcorr)
           if(iulst.gt.0.and.(.not.multi_model))
     *     write(iulst,'('' depth limited to Conrad at '',
     &      f6.1,'' km'')',iostat=code)xh(3)
           call sei code(fort$,code,iulst,b_flag) ! Process error.
           write(*,'('' depth limited to Conrad at '',
     &     f6.1,'' km'')')xh(3)
           depthlim=dconrad
          else
           trphs(ii)='        '
           dtw1(ii)=-1.0
           tmin=0.0
           return
          endif            
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         endif

         if(prmd.eq.'N'.or.prmd.eq.'G'.or.prmd.eq.' '.or.prmd.eq.'B'
     &   )then
c BRL 11/95 changed prm1 to prmd so blank phase not reset back to 'N'
           trphs(ii)(1:1)=prmd
           do k=1,nl                      
            if(prm1.eq.'P')parm(k)=v(k)
            if(prm1.eq.'S')parm(k)=vs(k)
           end do
c            write(iulst,*,iostat=code)xs(1)/degtorad,
c     &      xs(2)/degtorad,xh(1)/degtorad,
c     &      xh(2)/degtorad,xh(3)
           call sei code(fort$,code,iulst,b_flag) ! Process error.
           iflag=1
           iu=0

c change 10/93: print dtdx2 errors if test(66)>0
           if(test(66).gt.0.0)iu=iulst
            
           xhc(1)=xh(1)
           xhc(2)=xh(2)
           xhc(3)=xh(3)

c 4/94: correct xhc(3) for dtdx2 if test(40)=0
c       since the dtdx2 origin is at maxelv, maxelv must
c       be added to the hypocentral depth, xh(3), relative
c       to sea level
           if(test(40).eq.0.0)xhc(3)=xh(3)+float(maxelv)*.001

           if(test(105).eq.0.0)then
c            write(*,*) ' debug ',st(ii),phase(ii)
c            write(*,*) ' debug hyp hc ',xhc
c            write(*,*) ' debug hyp xs ',xs
c            write(*,*) ' debug maxelv ',maxelv
            call dtdx2(xhc,xs,prmd,nmoho,nconrad,iu,tmin,
     &      dx,delta,ann,iflag,phsid)               
c            write(*,*) ' debug tmin ',tmin
           endif
c           if(test(105).gt.0.0)then
c            call dtdxg(nn,parm,xhc,xs,prmd,nmoho,nconrad,iu,tmin,
c     &       dx,delta,ann,iflag,phsid)               
c           endif
c          write(iulst,*,iostat=code)st(ii),phase(ii),tp(ii)-tph,
c     &                              tmin,iflag
           call sei code(fort$,code,iulst,b_flag) ! Process error.
         else
            iflag=0
            trphs(ii)='        '
         endif
         if(iflag.eq.0)then

c change 10/93 to only print message 1st time through
            if(iulst.gt.0.and.dtw1(ii).ge.0.0.and.test(66).
     *        ne.0.0.and.(.not.multi_model))then
              write(iulst,'('' No dtdx2 travel time for '',a5,
     &        1x,a4)',iostat=code)st(ii),phase(ii)
              call sei code(fort$,code,iulst,b_flag) ! Process error.
c             write(*,'('' No dtdx2 travel time for '',a4,
c     &       1x,a4)')sti),phase(ii)
            endif
            
            aninc(ii)=0

c set dtw1=-1.0 for invalid phases
            trphs(ii)='        '
            dtw1(ii)=-1.0
            tmin=0.0

         else

c set trphs(ii) to requested phase when iflagd=0 and iflag=1
c change 4/94: set trphs to phase calculated
            trphs(ii)=phsid
            trphs(ii)(1:1)=prm1

            if(dtw1(ii).ne.-2.0)then
             dtw1(ii)=dtwt(ii)*wt       ! distance weight
            else
             dtw1(ii)=0.0
            endif  
            tpc1(ii)=tmin
            aninc(ii)=ann
         endif
      
c 10/94: added dtw1(ii).ne.-2.
        elseif(iflagd.eq.1.or.distind.eq.'D')then

c 10/94: reset depthlim to TEST(70) for distant case
          depthlim=test(70)

c 4/95: added minflag=5 option - exclude 'S' phases
c          if(minflag.eq.5)then
c           if(ucase(phase(ii)(1:1)).ne.'P'.or.(phase(ii)(1:1).eq.'p'
c     &     .and.phase(ii)(2:2).ne.'P'))then
c             trphs(ii)='        '
c             aninc(ii)=0
c             dtw1(ii)=-1.0
c           endif 
c          endif 
          
c   use iasp91 program if iflagd=1
c          write(iulst,*,iostat=code)st(ii),dedeg,xh(3),tmin,dtdd,dtdz
         call sei code(fort$,code,iulst,b_flag) ! Process error.

c change 10/93: only call depset if depth has changed - saves CPU time
c iflagi<>1 signifies 1st call to depset for new run
c 4/94: also added check that dedeg has changed - saves time          
           if(xh(3).ne.olddep.or.iflagi.ne.1)then
            call depset(xh(3),usrc)
c            write(iulst,*,iostat=code)' depset ',xh(3)
            call sei code(fort$,code,iulst,b_flag) ! Process error.
            iflagi=1
            olddep=xh(3)

c 7/94: forces new trtm call when depth changes
            dedegold=-1.0
            
           endif
c          write(iulst,'('' dedeg '',f12.3)',iostat=code)dedeg
           call sei code(fort$,code,iulst,b_flag) ! Process error.
           if(dedeg.ne.dedegold)then
            call trtm(dedeg,niasp,n,tt,dtdl,dtdh,dddp,phcd)
            dedegold=dedeg
           endif 
           if(n.lt.1)go to 784
           iflag=1
c         write(iulst,*,iostat=code)'iasp91 tt ',phcd(1),tt(1),
c     &                        dtdl(1),dtdh(1)
c     &   ,ierr
           call sei code(fort$,code,iulst,b_flag) ! Process error.

c ************** MATCHED PICK (imatch=1)**************************************
c look for the phase in the iasp91 list (n phases)

c 5/94: always match first arrival, even for autopick-add P,S,PKP,Pn
          if(imatch.eq.1.or.(
     &     phase(ii)(1:4).eq.'P   '.or.phase(ii)(1:4)
     &    .eq.'S   '.or.phase(ii)(1:3).eq.'PKP'.or.phase(ii)(1:4).eq.
     &    'Pn  '.or.phase(ii)(1:4).eq.'Sn  '))then
          
c these indices are used to store the first S, P and PKP phases:
c If no match is found for 'P   ', 'S   ', or 'PKP ', this first
c phase is used
c change 3/94: added SKS & PKS indices
c 1/95 added PKiKP index
           indexfs=0
           indexfp=0
           indpkp=0
           indpki=0
           indpks=0
           indskp=0
           indsks=0
           inddpp=0
           inddps=0

c 1/95: indices to find # of P???????, S??????? & PK?????? arrivals found
           infprpt=0
           infsrpt=0
           inpkrpt=0
 
           do 770 j=1,n

c 6/94 use PnPn for PP and SnSn for SS
             if(phcd(j)(1:4).eq.'PnPn'.and.phase(ii)(1:3).eq.'PP ')
     &       go to 765        
             if(phcd(j)(1:4).eq.'SnSn'.and.phase(ii)(1:3).eq.'SS ')then
              go to 765                                                 
             endif 

c change made 11/6/92 to exclude diffracted phases  
c changed 1/5 to match ipcount to infprpt
             if(phcd(j)(1:1).eq.'P'.and.indexfp.eq.0
c jul 2003     &       .and.(phcd(j)(2:5).ne.'diff'.or.dedeg.lt.test(69)
     &       .and.(phcd(j)(2:4).ne.'dif'.or.dedeg.lt.test(69)
     &       ))then
              if(ipcount.eq.infprpt)indexfp=j
              infprpt=infprpt+1
             endif

c 4/94: exclude SKi?? and SKP?? phases
c 7/95 BRL: exclude ScP, added infsrpt increment
c             write(iulst,*)phase(ii),phcd(j),indexfs,infsrpt
             if(phcd(j)(1:1).eq.'S'.and.indexfs.eq.0.
     &        and.phcd(j)(1:3).ne.'SKi'.and.phcd(j)(1:3).ne.'SKP'
     &        .and.phcd(j)(1:3).ne.'ScP')then
              if(ipcount.eq.infsrpt)indexfs=j
              infsrpt=infsrpt+1
             endif

c change 10/93: added 2 indices to look for 1st pP and pS phases
             if(phcd(j)(1:2).eq.'pP'.and.inddpp.eq.0)inddpp=j
             if(phcd(j)(1:2).eq.'pS'.and.inddps.eq.0.and.phcd(j)(2:4)
     &       .ne.'SKi'.and.phcd(j)(2:4).ne.'SKP')inddps=j

c--------------------------------------------------------------------------
c changed following lines 4/94 to stop dual selection of phases in indexfs 
c & indexfp
c changed 4/94 to exclude SKiK?? and SKP??

c I test for both PKP and PKiKP so that the event can be moved out to
c satisfy the minimum delta (about 140 degrees) for PKP, using the
c PKiKP derivatives and travel times
c changed 1/95 to remove j.ne.indexfp.and.dedeg.lt.test(69)
c             if((phcd(j)(1:3).eq.'PKP').and.indpkp.eq.0)then
             if(phcd(j)(1:3).eq.'PKP'.or.phcd(j)(1:5).eq.'PKiKP')then
              inpkrpt=inpkrpt+1
              if(ipcount.eq.inpkrpt)indpkp=j
             endif

c look for first PKS, SKP & SKS 
             if(phcd(j)(1:3).eq.'PKS'.and.indpks.eq.0
     &       .and.j.ne.indexfp)indpks=j
             if(phcd(j)(1:3).eq.'SKP'.and.indskp.eq.0
     &       .and.j.ne.indexfs)indskp=j
             if(phcd(j)(1:3).eq.'SKS'.and.indsks.eq.0
     &       .and.j.ne.indexfs)indsks=j
c--------------------------------------------------------------------------

c change 10/93: make 1st letter case-sensitive
             if(phase(ii)(1:1).ne.phcd(j)(1:1))go to 770

c 7/95 BRL: 'P   ' and 'S   ' are always first arrivals
             if(phase(ii)(1:4).eq.'P   '.or.
     &         phase(ii)(1:4).eq.'S   ')go to 770

c 4/94: minflag=5 cases (only first P & pP)
c             if(minflag.eq.5)go to 770
             
c 4/94: change to 8 char check
             do k=2,8
              if(ucase(phase(ii)(k:k)).ne.ucase(phcd(j)(k:k)))go to 770                        
             end do

765          tmin=tt(j)
             dtdd=dtdl(j)
             dtdz=dtdh(j) 

c change 3/94: set indexfs and indexfp to 0 if phase is found:
c this means 'P   ' and 'S   ' must be next phase after this one
             if(phase(ii)(1:1).eq.'S')indexfs=0
             if(phase(ii)(1:1).eq.'P')indexfp=0
             if(phase(ii)(1:2).eq.'pS')inddps=0
             if(phase(ii)(1:2).eq.'pP')inddpp=0
             if(phase(ii)(1:3).eq.'PKP')indpkp=0
             if(phase(ii)(1:3).eq.'PKi')indpki=0
             if(phase(ii)(1:3).eq.'PKS')indpks=0
             if(phase(ii)(1:3).eq.'SKP')indskp=0
             if(phase(ii)(1:3).eq.'SKS')indsks=0

c set trphs(ii) to IASP91 phase
             trphs(ii)=phcd(j)

c            write(iulst,*,iostat=code)phcd(j)
             call sei code(fort$,code,iulst,b_flag) ! Process error.
c            write(iulst,*,iostat=code)st(ii),phase(ii),
c     &                dedeg,phcd(j),tmin
             call sei code(fort$,code,iulst,b_flag) ! Process error.
             go to 785
770        continue
          endif

c**********************  AUTOPICK (imatch=0)*********************************          
c section added 4/94 to match phase based on closest IASP91 travel time            
c modified this 4/94 to exclude PPP & SSS since IASPEI91 doesn't 
c calculate these
c 1/95: changed elseif(... to if(imatch.eq.0.and....
          if(imatch.eq.0.and.phase(ii)(1:3).ne.'SSS'.and.phase(ii)(1:3).
     &    ne.'PPP'.and.phase(ii)(2:4).ne.'SSS'.and.phase(ii)(2:4).ne.
     &    'PPP')then
           otime=sngl(tp(ii)-tph)-dl(ii)          
           diff=1.e32  
           minind=0
           iflag=1
           k=0
           do while (iflag.eq.1.and.k.le.n)  
            k=k+1
            tdiff=abs(otime-tt(k))
            if(tdiff.lt.diff)then
c            .and.(phase(ii)(1:1).eq.phcd(k)(1:1).or.
c     &       phase(ii)(1:1).eq.' '))then
             diff=tdiff
             minind=k
            else
             iflag=0
            endif
           enddo
c           write(iulst,'(1x,a4,1x,a4,1x,a8,2f10.2)',iostat=code)
c     &                 st(ii),phase(ii),
           call sei code(fort$,code,iulst,b_flag) ! Process error.
c     &     phcd(minind),tt(minind),otime              
           tmin=tt(minind)
           dtdd=dtdl(minind)
           dtdz=dtdh(minind)
           trphs(ii)=phcd(minind)
           go to 785
          endif

          if(minflag.eq.5)then

c 6/94 include operator specified PKP
c 9/94 added Pn phases 
           if((phase(ii)(1:3).eq.'P  '.or.phase(ii)(1:2).eq.'Pn'
     &     .or.phase(ii)(1:3).eq.'PKP').and.indexfp.ne.0)then
            tmin=tt(indexfp)
            dtdd=dtdl(indexfp)
            dtdz=dtdh(indexfp)
            trphs(ii)=phcd(indexfp)                  
            go to 785
           elseif(phase(ii)(1:4).eq.'pP  '.and.inddpp.ne.0)then
            tmin=tt(inddpp)
            dtdd=dtdl(inddpp)
            dtdz=dtdh(inddpp)
            trphs(ii)=phcd(inddpp)                  
            go to 785
           else
c 10/94: calculate time for secondary phases
c            trphs(ii)='        '
c            tmin=-1.0
           endif 
c           if(trphs(ii)(2:5).eq.'diff'.or.trphs(ii)(3:6).eq.'diff'.
c     &     or.trphs(ii)(1:4).eq.'PKiK'.or.trphs(ii)(2:5).eq.'PKiK')
c     &     tmin=-1.0 
c 10/94: moved go to 785 above to recognized 1st arrivals so secondary
c        arrivals still are assigned travel times
          endif
          
c if we get to this point, no match has been found:
c set to the minimum travel time for the following cases
c changed 10/93 to force minimum times when minflag=1
c also use minimum times for 'g' and 'n' phases when
c these are not calculated
c          write(iulst,*,iostat=code)phase(ii),indpkp,
c     &                  indsks,indskp,indpks,n
          call sei code(fort$,code,iulst,b_flag) ! Process error.
c 2/95: added PKP - set to first P if not found
c          write(iulst,*)st(ii),phase(ii),dedeg,indexfp,indexfs
          if(indexfp.ne.0.and.(phase(ii)(1:4).eq.'P   '.or.
     &     (phase(ii)(1:1).eq.'P'.and.minflag.eq.1).or.
     &     (phase(ii)(1:1).eq.'P'.and.ucase(phase(ii)(2:2)).
     &     eq.'N'.and.minflag.eq.0).or.phase(ii)(1:3).eq.'PKP'))then
 
c 11/94: exclude G phases
c     .or.(phase(ii)(1:1).eq.'P'.
c     &     and.ucase(phase(ii)(2:2)).eq.'G'.and.minflag.eq.0)))then

            tmin=tt(indexfp)
            dtdd=dtdl(indexfp)
            dtdz=dtdh(indexfp)
            trphs(ii)=phcd(indexfp)                  
            indexfp=0
            go to 785
          endif

          if(indexfs.ne.0.and.(phase(ii)(1:4).eq.'S   '.or.
     &     (phase(ii)(1:1).eq.'S'.and.minflag.eq.1).or.(phase(ii)(1:1).
     &     eq.'S'.and.ucase(phase(ii)(2:2)).eq.'N'.and.minflag.eq.0).or.
     &     (phase(ii)(1:1).eq.'S'.and.ucase(phase(ii)(2:2)).eq.'G'.and.
     &     minflag.eq.0)))then
            tmin=tt(indexfs)
            dtdd=dtdl(indexfs)
            dtdz=dtdh(indexfs)
            trphs(ii)=phcd(indexfs)
            indexfs=0
            go to 785
          endif

c change BRL 1/94: need this for depth phases
          if(inddpp.ne.0.and.(phase(ii)(1:4).eq.'pP  '.or.
     &     (phase(ii)(1:2).
     &    eq.'pP'.and.minflag.eq.1).or.(phase(ii)(1:2).eq.'pP'.and.
     &    ucase(phase(ii)(3:3)).eq.'N'.and.minflag.eq.0).or.
     &    (phase(ii)(1:2).eq.'pP'.and.ucase(phase(ii)(3:3)).eq.'G'.and.
     &    minflag.eq.0)))then
            tmin=tt(inddpp)
            dtdd=dtdl(inddpp)
            dtdz=dtdh(inddpp)
            trphs(ii)=phcd(inddpp)
            inddpp=0
            go to 785
          endif

           if(inddps.ne.0.and.(phase(ii)(1:4).eq.'pS  '.or.
     &     (phase(ii)(1:2).eq.'pS'.and.minflag.eq.1).or.
     &     (phase(ii)(1:2).eq.'pS'.and.ucase(phase(ii)(3:3))
     &     .eq.'N'.and.minflag.eq.0).or.(phase(ii)(1:2).eq.'pS'
     &     .and.ucase(phase(ii)(3:3)).eq.'G'.and.minflag.eq.0)))then
             tmin=tt(inddps)
             dtdd=dtdl(inddps)
             dtdz=dtdh(inddps)
             trphs(ii)=phcd(inddps)
             inddps=0
             go to 785
           endif

c 10/93: P' now recognized as PKP
           if(indpkp.ne.0.and.(phase(ii)(1:3).eq.'PKP'.or.phase(ii)(2:2)
     &     .eq.'P'''))then
            tmin=tt(indpkp)
            dtdd=dtdl(indpkp)
            dtdz=dtdh(indpkp)
            trphs(ii)=phcd(indpkp)
            indpkp=0
            go to 785
           endif

c 3/94: added PKS, SKP & SKS
           if(indpks.ne.0.and.phase(ii)(1:3).eq.'PKS')then
            tmin=tt(indpks)
            dtdd=dtdl(indpks)
            dtdz=dtdh(indpks)
            trphs(ii)=phcd(indpks)
            indpks=0
            go to 785
           endif

           if(indskp.ne.0.and.phase(ii)(1:3).eq.'SKP')then
            tmin=tt(indskp)
            dtdd=dtdl(indskp)
            dtdz=dtdh(indskp)
            trphs(ii)=phcd(indskp)
            indskp=0
            go to 785
           endif

           if(indsks.ne.0.and.phase(ii)(1:3).eq.'SKS')then
            tmin=tt(indsks)
            dtdd=dtdl(indsks)
            dtdz=dtdh(indsks)
            trphs(ii)=phcd(indsks)
            indsks=0
            go to 785
           endif

c  phase not identified: set it to blank and tmin=-1
          trphs(ii)='        '

784       tmin=-1.0
785       continue

c 1/95: weight core phases out if test(90)=0
          do j=1,8
           if(test(90).eq.0.0.and.ucase(trphs(ii)(j:j)).eq.'K')then
c            write(iulst,*)phase(ii),trphs(ii)
            dtw1(ii)=-2.0
           endif
          enddo

c weight out core phases in range 135-150 deg if test(95)>0            
          if(test(95).gt.0.0.and.delta.ge.15344.
     &     .and.delta.le.16902.)dtw1(ii)=-2.0
          
          if(tmin.lt.0.0)then
            if(iulst.gt.0.and.dtw1(ii).ge.0.0.and.test(66).
     *        ne.0..and.(.not.multi_model))then
              if(data(1).ne.dataprev)then
c                write(iulst,*,iostat=code)data(1)(1:78)
               call sei code(fort$,code,iulst,b_flag) ! Process error.
              endif
              write(iulst,'('' No IASP91 travel time for '',a5,
     &        1x,a4)',iostat=code)st(ii),phase(ii)
              call sei code(fort$,code,iulst,b_flag) ! Process error.
            endif
            if(dtw1(ii).ge.0.0.and.test(66).ne.0.)then
              if(data(1).ne.dataprev)then
c                write(*,*)data(1)(1:78)
                dataprev=data(1)
              endif
              if(phase(ii)(1:4).ne.'    ')
     &         write(*,'('' No IASP91 travel time''
     &        '' for '',a5,1x,a4)')st(ii),phase(ii)
            endif
c            trphs(ii)='        '
            aninc(ii)=0
            dtw1(ii)=-1.0

          else

c  VALID PHASE-------------------------------------------------------------
c 2/95 BRL: time correction for PKPdf phase
c            write(iulst,*)phase(ii),trphs(ii),delta,imatch
            if(trphs(ii)(1:5).eq.'PKPdf'.or.trphs(ii)(2:6)
     &       .eq.'PKPdf')then
             if(dedeg.ge.137..and.dedeg.le.149.5.and.imatch.eq.1)then
c              write(iulst,*)(dedeg-145.)*1.087
c              tmin=tmin+(dedeg-145.)*1.087
             endif
            endif            


c dddd is the horizontal slowness (dt/ddelta) in sec/km
            dddd=dtdd/rearth/degtorad      
            
c 4/94: add station altitude correction
c I use the velocity in the top layer of the layered model as the velocity
c between sea level and the station
c 10/94:  changed velocity to top layer of IASPEI91 model
            rr=1.
            call emiask(rr,ro,vpp,vss)
            vpp=vpp*6.8501006
            vss=vss*6.8501006
            if(ucase(prm1).eq.'P')vvv=vpp
            if(ucase(prm1).eq.'S')vvv=vss

c            pz=sqrt(abs(1./vvv*vvv-dddd*dddd)) !removed 6/98 BRL

c 7/94: changed to use angle of incidence at station, aninst,
c      to calculate vertical slowness
           if(trphs(ii)(1:2).ne.'Pg'.and.trphs(ii)(1:2).ne.'Sg')then 
            arg=vvv*dddd
            if(arg.gt.1.)arg=1.
            aninst=asin(arg)
            pz=cos(aninst)/vvv
            ph=sin(aninst)/vvv  !added 6/98
            tcorr=float(iselv(ii))*.001*pz
           else
            tcorr=0.0
           endif   

c 11/95 BRL: added spherical harmonic station corrections
            if(test(98).ne.0.0)then
             thetamax=35.
             thetamin=15.                        
             aninstr=aninst/degtorad
             costh=cos(pi*(thetamax-aninstr)/(thetamax-thetamin))
             backaz=azb*degtorad
             stcorr=scorr(ii,1)+scorr(ii,2)*plgndr(1,0,costh)
             stcorr=stcorr+(scorr(ii,3)*cos(backaz)+scorr(ii,4)
     &       *sin(backaz))*plgndr(1,1,costh)        
             stcorr=stcorr+scorr(ii,5)*plgndr(2,0,costh)
             stcorr=stcorr+(scorr(ii,6)*cos(backaz)+scorr(ii,7)
     &       *sin(backaz))*plgndr(2,1,costh)
             stcorr=stcorr+(scorr(ii,8)*cos(2.*backaz)+scorr(ii,9)
     &       *sin(2.*backaz))*plgndr(2,2,costh)
             if(ucase(prm1).eq.'S')stcorr=stcorr*vss/vpp
             tcorr=tcorr+stcorr

c            write(iulst,'(a8,3f10.4)')st(ii),scorr(ii,1),stcorr
            endif

c            write(iulst,'('' tcorr '',a8,f10.2)',iostat=code)
c     &                      trphs(ii),tcorr
            call sei code(fort$,code,iulst,b_flag) ! Process error.
c set dtw1 to the initial phase weight
            if(dtw1(ii).ne.-2.0)then
             dtw1(ii)=dtwt(ii)
            else
             dtw1(ii)=0.0
            endif  

c 6/94: added ellipticity correction. Changed station coord to xsd 10/94
c       station elevation correction now included in ellipticity corrn.
            if(test(84).gt.0.0)then
             ecr=ellipcor(xsd,xh,trphs(ii))
             if(ecr.gt.2.0)then
              write(iulst,*,iostat=code)
     &              ' ellip corr > 2 ',trphs(ii),ecr,xh(3)
              call sei code(fort$,code,iulst,b_flag) ! Process error.
              write(*,*)' ellip corr > 2 ',trphs(ii),ecr,xh(3)
             endif 
            else
             ecr=0.0
            endif  
            tpc1(ii)=tmin+tcorr+ecr
c            write(iulst,*,iostat=code)st(ii),phase(ii),ecr
            call sei code(fort$,code,iulst,b_flag) ! Process error.
            az=azb*degtorad

c change 12/29: multiply dx(3) by earth's radius to keep consistency
c with dtdx2, i.e., keep dx(3) in sec/km,
            dx(3)=dtdz*rearth

            dx(1)=-dtdd*sin(az)*cos(xh(2))/degtorad
            dx(2)=-dtdd*cos(az)/degtorad

c  get velocity at location depth using iasp91 model

c change 10/93: delz is depth of event 
c corrected 4/94: xh(3) is now always relative to sea level
            delz=xh(3)

c find angle of incidence using IASPEI91 model
            rr=(rearth-delz)/rearth
            call emiask(rr,ro,vpp,vss)
            vpp=vpp*6.8501006
            vss=vss*6.8501006
            if(ucase(prm1).eq.'P')vvv=vpp
            if(ucase(prm1).eq.'S')vvv=vss

c 4/94: prevents asin arguments>1            
            arg=vvv*dddd
            if(arg.gt.1.)arg=1.
            aninc(ii)=asin(arg)/degtorad
c
c   iasp91 should calculate g if event deep, but it labels phase n and calcualte
c   wrong angle of incidence. correct here
c
            if(dtdz.gt.0.0) then
               aninc(ii)=180.0-aninc(ii) ! barry+jh jan-01
               if(trphs(ii)(2:2).eq.'n') trphs(ii)(2:2)='G'
            endif

          endif
        endif

c        if(dtw1(ii).ge.0.0)then
c          dt(ii)=sngl(tp(ii)-tph)-tpc1(ii)-dl(ii)
c        else
c          dt(ii)=0.0
c        endif
c        write(16,*)' tt ',st(ii),phase(ii),tmin,dt(ii)

c    L & R phase derivatives
c change 10/93 to include R phases and only use 'G'
c changed 1/94 - don't use 'G' as ISC doesn't         
c 6/98 BRL added T phase

      elseif(prm1.eq.'L'.or.prm1.eq.'R'.or.prm1.eq.'T')then
        call delaz(y0(ii),x0(ii),delta,dedeg,azz,xh(2),xh(1))
c changed BRL 6/98        
        cfact=1.0
        if(delta.gt.0.0)cfact=degtorad*dedeg/delta

c    calculate travel time and derivatives along direct path in single 
c    layer having a  velocity vl
        if(iflagd.eq.0)dtw1(ii)=dtwt(ii)*wt   ! distance weight
        if(iflagd.eq.1)dtw1(ii)=dtwt(ii)
        if(prm1.eq.'L')then
         vl=test(51)
c jul 2003         trphs(ii)='LG      '
         trphs(ii)='Lg      '
         dtw1(ii)=dtw1(ii)*test(99)
        endif       
        if(prm1.eq.'R')then
         vl=test(44)
c jul 2003         trphs(ii)='RG      '
         trphs(ii)='Rg      '
         dtw1(ii)=dtw1(ii)*test(100)
        endif
        if(prm1.eq.'T')then
         vl=test(49)
         trphs(ii)='T       '
         dtw1(ii)=dtw1(ii)*test(101)
        endif
        dist=delta
        tpc1(ii)=dist/vl
        dx(1)=-sin(azz*degtorad)*cos(abs(xh(2)))/(cfact*vl)
        dx(2)=-cos(azz*degtorad)/(cfact*vl)
        dx(3)=0.0
c        dt(ii)=sngl(tp(ii)-tph)-tpc1(ii)-dl(ii)
c        write(16,*)' L',i,dist,vl,tpc1(ii),dtw1(ii)
        
c    Azimuthal derivatives
      elseif(phase(ii)(1:2).eq.'AZ')then
        if(dedeg.ne.0.0)dx(1)=-cos(azb*degtorad)/abs(sin(dedeg
     &  *degtorad))
        dx(1)=dx(1)*cos(abs(xh(2)))
        if(dedeg.ne.0.0)dx(2)=sin(azb*degtorad)/abs(sin(dedeg
     &  *degtorad))
c        write(iulst,*,iostat=code)'dx1,2,azb',dx(1),dx(2),azb
        call sei code(fort$,code,iulst,b_flag) ! Process error.
        dx(3)=0.0

c   Weight the azimuths with the inverse of the specified error
c   in radians. An azimuthal difference equal to this error then 
c   has the same effect as an unweighted time residual of one second.
        wt1=1.0

c change 10/93: keep zero weight when iflagd=1
c also check weight is not < 0
c also put dtwt(ii) (original weight) calculation in hypocent
        if(iflagd.eq.0.or.distind.eq.'L'.and.dtw1(ii).ge.0.0)wt1=wt

        dtw1(ii)=dtwt(ii)*wt1

c 4/94: changed az1 and dtt to double precision
        az1=tp(ii)

c change 10/93: make azimuths > 0 and correct dtt accordingly
c this section was not working before when azimuths were close to
c +/-180 deg
        if(az1.lt.0.d0)az1=az1+360.d0
        if(azz.lt.0.)azz=azz+360. 
        dtt=sngl(az1)-azz
c       write(iulst,*,iostat=code)sngl(az1),azz,dtt,dtwt(ii),wt1
        call sei code(fort$,code,iulst,b_flag) ! Process error.
c these are the cases when, e.g., az1=350 and azz=10 (dtt=-20)
        if(dtt.gt.180.d0)dtt=dtt-360.d0
c case when, e.g., az1=10 and az1=350 (dtt=+20)
        if(dtt.lt.-180.)dtt=360.+dtt
        dt(ii)=dtt*degtorad
        tpc1(ii)=azz
        tp(ii)=az1
        trphs(ii)='        '

c weird first phase letter or blank: set weight to -1
      else
        if(iulst.gt.0.and.dtw1(ii).ge.0.0.and.test(66).
     *    ne.0..and.(.not.multi_model))then
          if(data(1).ne.dataprev)then
c            write(iulst,*,iostat=code)data(1)(1:78)
           call sei code(fort$,code,iulst,b_flag) ! Process error.
          endif
          if (phase(ii)(1:2).ne.'AM') then
          write(iulst,'('' No travel time for '',a5,
     &    '' phase '',a4)',iostat=code)st(ii),phase(ii)
          call sei code(fort$,code,iulst,b_flag) ! Process error.
          endif   ! lo 5/9/2012
        endif
        if(dtw1(ii).ge.0.0.and.test(66).ne.0.)then
          if(data(1).ne.dataprev)then
c            write(*,*)data(1)(1:78)
            dataprev=data(1)
          endif
          if(phase(ii)(1:4).ne.'    '.and.phase(ii)(1:2).ne.'AM')
     &    write(*,'('' No travel time for ''
     &    ,a5,'' phase '',a4)')st(ii),phase(ii)
        endif
        
c set this to blank for unidentified phases
        trphs(ii)='        '                
        
        dtw1(ii)=-1.0
        aninc(ii)=0.0
        tpc1(ii)=0.0
      endif

c  set P weight to zero for P-S, P-L differences:
c  the difference is stored in the other phase's position
      if(ip(ii).eq.9.and.dtw1(ii).gt.0.0)dtw1(ii)=0.0
      if(dtw1(ii).gt.0)ndata=ndata+1

c   set up the row of the partial derivative matrix, g(ii,j)
      do k=1,3
        g(ii,k)=dx(k)
      end do        
c      write(iulst,*,iostat=code)' dx ',(dx(k),k=1,3)
      call sei code(fort$,code,iulst,b_flag) ! Process error.
c      write(iulst,*,iostat=code)dt(ii),(g(ii,j),j=1,3)
         call sei code(fort$,code,iulst,b_flag) ! Process error.
c      write(iulst,*,iostat=code)st(ii),phase(ii),delta,dtw1(ii),dt(ii),iflagd
      call sei code(fort$,code,iulst,b_flag) ! Process error.
      return
      end

c****************************************************************
c version of Buland's routine r4sort modified to handle
c real*8 data

      subroutine r8sort(n,rkey,iptr)
c
c $$$$$ calls no other routine $$$$$
c
c   R8sort sorts the n elements of array rkey so that rkey(i),
c   i = 1, 2, 3, ..., n are in asending order.  R8sort is a trivial
c   modification of ACM algorithm 347:  "An efficient algorithm for
c   sorting with minimal storage" by R. C. Singleton.  Array rkey is
c   sorted in place in order n*alog2(n) operations.  Coded on
c   8 March 1979 by R. Buland.  Modified to handle real*8 data on
c   14 Oct 1993 by B. Lienert
c
      save
      real*8 rkey(n),tmpkey
      dimension iptr(n),il(10),iu(10)
c   Note:  il and iu implement a stack containing the upper and
c   lower limits of subsequences to be sorted independently.  A
c   depth of k allows for n<=2**(k+1)-1.
      if(n.le.0) return
      do 1 i=1,n
 1    iptr(i)=i
      if(n.le.1) return
      r=.375
      m=1
      i=1
      j=n
c
c   The first section interchanges low element i, middle element ij,
c   and high element j so they are in order.
c
 5    if(i.ge.j) go to 70
 10   k=i
c   Use a floating point modification, r, of Singleton's bisection
c   strategy (suggested by R. Peto in his verification of the
c   algorithm for the ACM).
      if(r.gt..58984375) go to 11
      r=r+.0390625
      go to 12
 11   r=r-.21875
 12   ij=i+(j-i)*r
      if(rkey(iptr(i)).le.rkey(iptr(ij))) go to 20
      it=iptr(ij)
      iptr(ij)=iptr(i)
      iptr(i)=it
 20   l=j
      if(rkey(iptr(j)).ge.rkey(iptr(ij))) go to 39
      it=iptr(ij)
      iptr(ij)=iptr(j)
      iptr(j)=it
      if(rkey(iptr(i)).le.rkey(iptr(ij))) go to 39
      it=iptr(ij)
      iptr(ij)=iptr(i)
      iptr(i)=it
 39   tmpkey=rkey(iptr(ij))
      go to 40
c
c   The second section continues this process.  K counts up from i and
c   l down from j.  Each time the k element is bigger than the ij
c   and the l element is less than the ij, then interchange the
c   k and l elements.  This continues until k and l meet.
c
 30   it=iptr(l)
      iptr(l)=iptr(k)
      iptr(k)=it
 40   l=l-1
      if(rkey(iptr(l)).gt.tmpkey) go to 40
 50   k=k+1
      if(rkey(iptr(k)).lt.tmpkey) go to 50
      if(k.le.l) go to 30
c
c   The third section considers the intervals i to l and k to j.  The
c   larger interval is saved on the stack (il and iu) and the smaller
c   is remapped into i and j for another shot at section one.
c
      if(l-i.le.j-k) go to 60
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 80
 60   il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 80
c
c   The fourth section pops elements off the stack (into i and j).  If
c   necessary control is transfered back to section one for more
c   interchange sorting.  If not we fall through to section five.  Note
c   that the algorighm exits when the stack is empty.
c
 70   m=m-1
      if(m.eq.0) return
      i=il(m)
      j=iu(m)
 80   if(j-i.ge.11) go to 10
      if(i.eq.1) go to 5
      i=i-1
c
c   The fifth section is the end game.  Final sorting is accomplished
c   (within each subsequence popped off the stack) by rippling out
c   of order elements down to their proper positions.
c
 90   i=i+1
      if(i.eq.j) go to 70
      if(rkey(iptr(i)).le.rkey(iptr(i+1))) go to 90
      k=i
      kk=k+1
      ib=iptr(kk)
 100  iptr(kk)=iptr(k)
      kk=k
      k=k-1
      if(rkey(ib).lt.rkey(iptr(k))) go to 100
      iptr(kk)=ib
      go to 90
      end

c**************************************************************
c this subroutine taken from the iasp91 module emiasp91.f
      subroutine emiask(x0,ro,vp,vs)
c
c $$$$$ calls no other routine $$$$$
c
c   Emiask returns model parameters for the IASPEI working model 
c   (September 1990.1).  
c   Given non-dimensionalized radius x0, emiasp returns
c   non-dimensionalized density, ro, compressional velocity, vp, and
c   shear velocity, vs.  Non-dimensionalization is according to the
c   scheme of Gilbert in program EOS:  x0 by a (the radius of the
c   Earth), ro by robar (the mean density of the Earth), and velocity
c   by a*sqrt(pi*G*robar) (where G is the universal gravitational
c   constant.
c
c
      save
      dimension r(14),d(13,4),p(13,4),s(13,4)
      data r/0.      ,1217.1  ,3482.0  ,3631.  ,5611.   ,5711.   ,
     1 5961.   ,6161.   ,6251.   ,6336.   ,6351.    ,6371.    ,
     2 6371.,6371./
      data d/13.01219,12.58416, 6.8143 , 6.8143 , 6.8143 ,11.11978,
     1  7.15855, 7.15855, 7.15855,  2.92  , 2.72   , 2*0.,
     2        0.     ,-1.69929,-1.66273,-1.66273,-1.66273,-7.87054,
     2 -3.85999,-3.85999,-3.85999,4*0.,
     3       -8.45292,-1.94128,-1.18531,-1.18531,-1.18531,8*0.,
     4        0.     ,-7.11215,11*0./
      data p/11.24094,10.03904,14.49470,25.1486 ,25.969838,29.38896,
     1 30.78765,25.41389, 8.785412, 6.5   , 5.8   ,2*0.,
     2        0.   , 3.75665, -1.47089,-41.1538, -16.934118,-21.40656,
     2-23.25415,-17.69722,-0.7495294, 4*0.,
     3       -4.09689,-13.67046, 0.0  ,51.9932,9*0.,
     4        0.     , 0.      , 0.     ,-26.6083,9*0./
      data s/ 3.56454, 0.      , 8.16616,12.9303 ,20.768902,17.70732,
     1 15.24213,5.750203, 6.706232, 3.75   , 3.36   ,2*0.,
     2        0.     , 0.      ,-1.58206,-21.2590,-16.531471,-13.50652,
     2-11.08553,-1.274202,-2.248585, 4*0.,
     3       -3.45241, 0.      , 0.0    ,27.8988 ,9*0.,
     4        0.     , 0.      , 0.     ,-14.1080,9*0./
      data xn,rn,vn/6371.,.18125793,.14598326/,i/1/
c
      x=amax1(x0,0.)
      x1=xn*x
 2    if(x1.ge.r(i)) go to 1
      i=i-1
      go to 2
 1    if(x1.le.r(i+1).or.i.ge.11) go to 3
      i=i+1
      if(i.lt.11) go to 1
 3    ro=rn*(d(i,1)+x*(d(i,2)+x*(d(i,3)+x*d(i,4))))
      vp=vn*(p(i,1)+x*(p(i,2)+x*(p(i,3)+x*p(i,4))))
      vs=vn*(s(i,1)+x*(s(i,2)+x*(s(i,3)+x*s(i,4))))
      return
      end

c**********************************************************************
c   ellipticity correction routine
c
c   polynomial fit of corrections given by Dziewonski & Gilbert (GRAS
c   44, p7-17, 1975)
c
c   Barry Lienert 6/94                                          
c
c   inputs:  xs    geocentric long, lat(rad) and depth (km) of station
c            xh    geocentric long, lat(rad) and depth (km) of event
c            phase      P, PKP, PcP, etc                   
c
c   the current phase list is P, PKPab, PKPbc, PKiKP, PcP, S, ScS, SkS
c   for all other phases (except PKPdf - set to PKPab value), the 
c   correction returned is zero

c   function returns the correction in seconds to be added to travel
c   time calculated on the geocentric sphere (radius = 6371 km for
c   IASP91)
c                                  
c 10/94: changed to include station elevation correction

      function ellipcor(xs,xh,phase)
      save
      parameter (pi = 3.14159265)
      character*8 phase
      dimension coeff(9,3),tau(3),xs(*),xh(*)
      slat=xs(2)
      slon=xs(1)
      eqlat=xh(2)
      eqlon=xh(1)

c 10/93 subtract station elevation (relative to sea level)
      depth=xh(3)-xs(3)                                  
      
      if(phase.eq.'P       ')then
        ndeg=9
c   0 km
        if(depth.le.150.)then
          coeff(1,1)=-.000767752
          coeff(2,1)=-.0320965
          coeff(3,1)=-.00115385
          coeff(4,1)=.000129038
          coeff(5,1)=-4.44838e-6
          coeff(6,1)=8.20844e-8
          coeff(7,1)=-8.55154e-10
          coeff(8,1)=4.70402e-12
          coeff(9,1)=-1.06106e-14
          coeff(1,2)=-.00097755
          coeff(2,2)=-.000891082
          coeff(3,2)=-.000462451
          coeff(4,2)=1.74278e-6
          coeff(5,2)=2.79534e-7
          coeff(6,2)=-6.33865e-9
          coeff(7,2)=7.10643e-11
          coeff(8,2)=-4.25976e-13
          coeff(9,2)=1.04665e-15
          coeff(1,3)=-.000505837
          coeff(2,3)=.00211507
          coeff(3,3)=-.000403287
          coeff(4,3)=1.30399e-5
          coeff(5,3)=-4.07861e-7
          coeff(6,3)=7.18521e-9
          coeff(7,3)=-6.69335e-11
          coeff(8,3)=3.19871e-13
          coeff(9,3)=-6.13161e-16

c   300 km
        elseif(depth.le.475.)then
          coeff(1,1)=-.118157
          coeff(2,1)=-.0021144
          coeff(3,1)=-.00265186
          coeff(4,1)=.000163171
          coeff(5,1)=-4.69022e-6
          coeff(6,1)=7.79931e-8
          coeff(7,1)=-7.59002e-10
          coeff(8,1)=3.97837e-12
          coeff(9,1)=-8.64924e-15
          coeff(1,2)=-.0025971
          coeff(2,2)=-.0296334
          coeff(3,2)=.00271935
          coeff(4,2)=-.000164528
          coeff(5,2)=5.03298e-6
          coeff(6,2)=-8.46223e-8
          coeff(7,2)=8.1276e-10
          coeff(8,2)=-4.18461e-12
          coeff(9,2)=8.94339e-15
          coeff(1,3)=-.00137649
          coeff(2,3)=.00443056
          coeff(3,3)=-.000831244
          coeff(4,3)=4.05234e-5
          coeff(5,3)=-1.35084e-6
          coeff(6,3)=2.58875e-8
          coeff(7,3)=-2.79035e-10
          coeff(8,3)=1.58907e-12
          coeff(9,3)=-3.71079e-15

c   650 km
        else
          coeff(1,1)=-.260749
          coeff(2,1)=.00635749
          coeff(3,1)=-.00196931
          coeff(4,1)=.000102862
          coeff(5,1)=-2.65079e-6
          coeff(6,1)=4.10708e-8
          coeff(7,1)=-3.84205e-10
          coeff(8,1)=1.96951e-12
          coeff(9,1)=-4.22157e-15
          coeff(1,2)=-.00100326
          coeff(2,2)=-.0560922
          coeff(3,2)=.00428337
          coeff(4,2)=-.00019472
          coeff(5,2)=4.88834e-6
          coeff(6,2)=-7.01321e-8
          coeff(7,2)=5.85605e-10
          coeff(8,2)=-2.65545e-12
          coeff(9,2)=5.0413e-15
          coeff(1,3)=-8.08438e-6
          coeff(2,3)=.00181273
          coeff(3,3)=-.000443844
          coeff(4,3)=1.2112e-5
          coeff(5,3)=-3.18625e-7
          coeff(6,3)=5.78808e-9
          coeff(7,3)=-6.46629e-11
          coeff(8,3)=4.08766e-13
          coeff(9,3)=-1.08792e-15
        endif

c 6/94 removed PKPdf - occurs outside of fitted range
      elseif(phase.eq.'PKPab   ')then
        ndeg=6
        if(depth.le.150.)then
          coeff(1,1)=-862.874   
          coeff(2,1)=21.6945  
          coeff(3,1)=-.204365  
          coeff(4,1)=.000853327
          coeff(5,1)=-1.33332e-6
          coeff(6,1)=0
          coeff(1,2)=23711.5   
          coeff(2,2)=-750.896   
          coeff(3,2)=9.50569    
          coeff(4,2)=-.0601236 
          coeff(5,2)=.000190001
          coeff(6,2)=-2.40001e-7
          coeff(1,3)=5559.15   
          coeff(2,3)=-174.183 
          coeff(3,3)=2.18112    
          coeff(4,3)=-.0136466 
          coeff(5,3)=4.26665e-5 
          coeff(6,3)=-5.33331e-8

c   300 km
        elseif(depth.le.475.)then
          coeff(1,1)=-10306.5
          coeff(2,1)=327.611  
          coeff(3,1)=-4.16407  
          coeff(4,1)=.0264531  
          coeff(5,1)=-8.39993e-5
          coeff(6,1)=1.06666e-7
          coeff(1,2)=25.4467  
          coeff(2,2)=-.435782 
          coeff(3,2)=.00270001
          coeff(4,2)=-5.92594e-6
          coeff(5,2)=0
          coeff(6,2)=0
          coeff(1,3)=1963.21   
          coeff(2,3)=-66.3119 
          coeff(3,3)=.892319    
          coeff(4,3)=-.00598345
          coeff(5,3)=2.00004e-5 
          coeff(6,3)=-2.66672e-8

c   650 km
        else
          coeff(1,1)=-435.899
          coeff(2,1)=10.9575  
          coeff(3,1)=-.103083  
          coeff(4,1)=.000428888
          coeff(5,1)=-6.66666e-7
          coeff(6,1)=0
          coeff(1,2)=-193.614  
          coeff(2,2)=5.04239  
          coeff(3,2)=-.0486415
          coeff(4,2)=.000207777
          coeff(5,2)=-3.33332e-7
          coeff(6,2)=0
          coeff(1,3)=-187.962   
          coeff(2,3)=4.85321  
          coeff(3,3)=-.0472418  
          coeff(4,3)=.000204815
          coeff(5,3)=-3.33334e-7
          coeff(6,3)=0
        endif

      elseif(phase.eq.'PKPbc   ')then
        ndeg=3
        if(depth.le.150.)then
          coeff(1,1)=17.2001    
          coeff(2,1)=-.219001 
          coeff(3,1)=.000600004
          coeff(1,2)=-23.61    
          coeff(2,2)=.344   
          coeff(3,2)=-.0012     
          coeff(1,3)=-7.64     
          coeff(2,3)=.079 
          coeff(3,3)=-.0002     

c   300 km
        elseif(depth.le.475.)then
          coeff(1,1)=17.0299 
          coeff(2,1)=-.216999
          coeff(3,1)=.000599997
          coeff(1,2)=-23.61   
          coeff(2,2)=.344 
          coeff(3,2)=-.0012
          coeff(1,3)=-7.34     
          coeff(2,3)=.077 
          coeff(3,3)=-.0002     

c   650 km
        else
          coeff(1,1)=12.5    
          coeff(2,1)=-.156  
          coeff(3,1)=.0004  
          coeff(1,2)=-18.96  
          coeff(2,2)=.283  
          coeff(3,2)=-.001
          coeff(1,3)=-11.98   
          coeff(2,3)=.138  
          coeff(3,3)=-.0004  
        endif
      
      elseif(phase.eq.'PcP     ')then
        ndeg=5
        if(depth.le.150.)then
          coeff(1,1)=-1.51245   
          coeff(2,1)=1.58267e-6
          coeff(3,1)=.000538697
          coeff(4,1)=-5.40473e-6
          coeff(5,1)=9.3899e-9
          coeff(1,2)=.00152695 
          coeff(2,2)=-.0231172  
          coeff(3,2)=3.39633e-5 
          coeff(4,2)=6.44827e-6
          coeff(5,2)=-4.0056e-8
          coeff(1,3)=-.0032619 
          coeff(2,3)=.00104715
          coeff(3,3)=-.000371238
          coeff(4,3)=3.41724e-6
          coeff(5,3)=-5.99615e-9

c   300 km
        elseif(depth.le.475.)then
          coeff(1,1)=-1.34528
          coeff(2,1)=-.00390254
          coeff(3,1)=.000672213
          coeff(4,1)=-7.16257e-6
          coeff(5,1)=1.7189e-8
          coeff(1,2)=6.24639e-6
          coeff(2,2)=-.0242644
          coeff(3,2)=6.41413e-5
          coeff(4,2)=6.05076e-6
          coeff(5,2)=-3.82367e-8
          coeff(1,3)=-.00376741
          coeff(2,3)=.00105237
          coeff(3,3)=-.00037939 
          coeff(4,3)=3.59635e-6
          coeff(5,3)=-7.14236e-9

c   650 km
        else
          coeff(1,1)=-1.25364
          coeff(2,1)=-.000760161
          coeff(3,1)=.000576945
          coeff(4,1)=-5.94543e-6
          coeff(5,1)=1.17353e-8
          coeff(1,2)=-.00452048
          coeff(2,2)=-.0247656
          coeff(3,2)=6.89416e-5
          coeff(4,2)=5.95524e-6
          coeff(5,2)=-3.74576e-8
          coeff(1,3)=-.00332639 
          coeff(2,3)=.000729309
          coeff(3,3)=-.000362487
          coeff(4,3)=3.25262e-6
          coeff(5,3)=-4.94785e-9
        endif

      elseif(phase.eq.'S       ')then
        ndeg=7
        if(depth.le.150.)then
          coeff(1,1)=-.00159755 
          coeff(2,1)=-.065464 
          coeff(3,1)=.000393594
          coeff(4,1)=2.9408e-5
          coeff(5,1)=-5.74291e-7
          coeff(6,1)=3.98649e-9
          coeff(7,1)=-1.02641e-11
          coeff(1,2)=.00310562 
          coeff(2,2)=-.00111105 
          coeff(3,2)=-.00108956 
          coeff(4,2)=2.15437e-5
          coeff(5,2)=-1.17914e-7
          coeff(6,2)=3.62493e-10
          coeff(7,2)=-1.46172e-12
          coeff(1,3)=-8.13873e-5
          coeff(2,3)=.0031144 
          coeff(3,3)=-.000578018
          coeff(4,3)=8.49368e-6
          coeff(5,3)=-1.29615e-7
          coeff(6,3)=1.23587e-9
          coeff(7,3)=-3.8947e-12

c   300 km
        elseif(depth.le.475.)then
          coeff(1,1)=-.20426
          coeff(2,1)=-.0217636
          coeff(3,1)=-.00157197
          coeff(4,1)=7.68349e-5
          coeff(5,1)=-1.1999e-6
          coeff(6,1)=8.21411e-9
          coeff(7,1)=-2.16709e-11
          coeff(1,2)=-.0193939
          coeff(2,2)=-.0309887
          coeff(3,2)=.00105685
          coeff(4,2)=-4.59348e-5
          coeff(5,2)=9.39567e-7
          coeff(6,2)=-7.66655e-9
          coeff(7,2)=2.20597e-11
          coeff(1,3)=-.000953789
          coeff(2,3)=.00472556
          coeff(3,3)=-.000797555
          coeff(4,3)=1.65455e-5
          coeff(5,3)=-2.64495e-7
          coeff(6,3)=2.30252e-9
          coeff(7,3)=-7.09337e-12

c   650 km
        else
          coeff(1,1)=-.466331
          coeff(2,1)=.000529105
          coeff(3,1)=-.00154431
          coeff(4,1)=5.67141e-5
          coeff(5,1)=-7.35948e-7
          coeff(6,1)=4.03289e-9
          coeff(7,1)=-8.14629e-12
          coeff(1,2)=-.0170655 
          coeff(2,2)=-.0793724
          coeff(3,2)=.00420797
          coeff(4,2)=-.000134252
          coeff(5,2)=2.18743e-6
          coeff(6,2)=-1.6348e-8
          coeff(7,2)=4.56869e-11
          coeff(1,3)=.00106618  
          coeff(2,3)=.00249243
          coeff(3,3)=-.000629529
          coeff(4,3)=9.36621e-6
          coeff(5,3)=-1.2012e-7
          coeff(6,3)=9.80553e-10
          coeff(7,3)=-2.60446e-12
        endif

      elseif(phase.eq.'ScS     ')then
        ndeg=7
        if(depth.le.150.)then
          coeff(1,1)=-2.74057   
          coeff(2,1)=.000616927
          coeff(3,1)=.00066294 
          coeff(4,1)=3.43379e-6
          coeff(5,1)=-1.86419e-7
          coeff(6,1)=1.23651e-9
          coeff(7,1)=-2.12018e-12
          coeff(1,2)=.000242482
          coeff(2,2)=-.0393147  
          coeff(3,2)=-.00029074 
          coeff(4,2)=2.6228e-5 
          coeff(5,2)=-3.4253e-7
          coeff(6,2)=2.34549e-9
          coeff(7,2)=-7.76302e-12
          coeff(1,3)=.000891464
          coeff(2,3)=.000161816
          coeff(3,3)=-.000580489
          coeff(4,3)=4.37621e-6
          coeff(5,3)=-1.06131e-8
          coeff(6,3)=3.0902e-10
          coeff(7,3)=-2.14893e-12

c   300 km
        elseif(depth.le.475.)then
          coeff(1,1)=-2.50928
          coeff(2,1)=-.00142982
          coeff(3,1)=.000978636
          coeff(4,1)=-1.03682e-5
          coeff(5,1)=8.66059e-8
          coeff(6,1)=-1.30288e-9
          coeff(7,1)=6.87284e-12
          coeff(1,2)=3.86162e-5
          coeff(2,2)=-.041681 
          coeff(3,2)=-.000125393
          coeff(4,2)=1.88057e-5
          coeff(5,2)=-1.8032e-7
          coeff(6,2)=6.84434e-10
          coeff(7,2)=-1.36169e-12
          coeff(1,3)=.00106879 
          coeff(2,3)=-.000195485
          coeff(3,3)=-.000520055
          coeff(4,3)=1.15282e-6
          coeff(5,3)=5.66433e-8 
          coeff(6,3)=-3.07556e-10
          coeff(7,3)=-5.74159e-14

c   650 km
        else
          coeff(1,1)=-2.27944
          coeff(2,1)=-.00101086
          coeff(3,1)=.000890774
          coeff(4,1)=-4.4587e-6
          coeff(5,1)=-6.93411e-8
          coeff(6,1)=4.45341e-10
          coeff(7,1)=-1.17211e-13
          coeff(1,2)=-.000300892
          coeff(2,2)=-.0442828
          coeff(3,2)=1.42522e-5
          coeff(4,2)=1.28274e-5
          coeff(5,2)=-4.56e-8
          coeff(6,2)=-7.7526e-10
          coeff(7,2)=4.63287e-12
          coeff(1,3)=.000920091 
          coeff(2,3)=-.000135877
          coeff(3,3)=-.000509683
          coeff(4,3)=-4.07251e-7
          coeff(5,3)=1.07329e-7
          coeff(6,3)=-9.48686e-10
          coeff(7,3)=2.75919e-12
        endif

      elseif(phase.eq.'SKS     ')then
        ndeg=7
        if(depth.le.150.)then
          coeff(1,1)=54.3423   
          coeff(2,1)=-3.88174 
          coeff(3,1)=.109657  
          coeff(4,1)=-.00161249
          coeff(5,1)=1.31439e-5
          coeff(6,1)=-5.66686e-8
          coeff(7,1)=1.00996e-10
          coeff(1,2)=-13.3418
          coeff(2,2)=.810794   
          coeff(3,2)=-.0221222  
          coeff(4,2)=-.00031254
          coeff(5,2)=-2.31667e-6
          coeff(6,2)=8.68295e-9
          coeff(7,2)=-1.32088e-11
          coeff(1,3)=2.55499   
          coeff(2,3)=-.236244 
          coeff(3,3)=.00751661  
          coeff(4,3)=-.000132837
          coeff(5,3)=1.23634e-6 
          coeff(6,3)=-5.63696e-9
          coeff(7,3)=9.99772e-12

c   300 km
        elseif(depth.le.475.)then
          coeff(1,1)=38.8272
          coeff(2,1)=-2.55553 
          coeff(3,1)=.0662939  
          coeff(4,1)=-.000892496
          coeff(5,1)=6.66681e-6
          coeff(6,1)=-2.65247e-8
          coeff(7,1)=4.403e-11
          coeff(1,2)=-7.85805 
          coeff(2,2)=.391446 
          coeff(3,2)=-.00944131
          coeff(4,2)=.000115228
          coeff(5,2)=-6.41705e-7
          coeff(6,2)=1.30509e-9
          coeff(7,2)=0
          coeff(1,3)=7.60246   
          coeff(2,3)=-.500365 
          coeff(3,3)=.013152    
          coeff(4,3)=-.000196341
          coeff(5,3)=1.64085e-6 
          coeff(6,3)=-7.04163e-9
          coeff(7,3)=1.21076e-11

c   650 km
        else
          coeff(1,1)=19.2705
          coeff(2,1)=-1.38981 
          coeff(3,1)=.0377979  
          coeff(4,1)=-.000522355
          coeff(5,1)=3.97266e-6
          coeff(6,1)=-1.61102e-8
          coeff(7,1)=2.73351e-11
          coeff(1,2)=-13.4103  
          coeff(2,2)=.798484  
          coeff(3,2)=-.021582
          coeff(4,2)=.000302366
          coeff(5,2)=-2.21747e-6
          coeff(6,2)=8.20113e-9
          coeff(7,2)=-1.22915e-11
          coeff(1,3)=8.46482   
          coeff(2,3)=-.535302 
          coeff(3,3)=.0134536  
          coeff(4,3)=-.000191011
          coeff(5,3)=1.51973e-6
          coeff(6,3)=-6.201e-9
          coeff(7,3)=1.00894e-11
        endif

      
      else
       ellipcor=0.0
       return
      endif   
        
      call delaz(slat, slon, dekm, dedeg, az0, eqlat, eqlon)
      baz=az0*pi/180.
      do i=1,3
        tau(i)=0.0
        do j=1,ndeg
          tau(i)=tau(i)+coeff(j,i)*dedeg**(j-1)  
        end do
      end do                               
c      write(*,*)dedeg,tau(1),tau(2),tau(3)
      colat=.5*pi-eqlat
      ellipcor=.25*(1.+3.*cos(2.*colat))*tau(1)+.8660254*sin(2.*
     &colat)*cos(baz)*tau(2)+.8660254*sin(2.*colat)**2*
     &cos(2.*baz)*tau(3)
c      ellipcor=0.0
c      write(*,*)phase,dedeg,ellipcor 
      return
      end

      real function plgndr(l,m,x)
      integer l,m
      integer i,ll
      real fact,pll,pmm,pmmp1,somx2
c     if(m.lt.0.or.m.gt.l.or.abs(x).gt.1.)pause
c    *'bad arguments in plgndr'
      if(m.lt.0.or.m.gt.l.or.abs(x).gt.1.) then
        write(*,*) 'bad arguments in plgndr'
        read(*,'()')
      endif
      pmm=1.
      if(m.gt.0) then
        somx2=sqrt((1.-x)*(1.+x))
        fact=1.
        do 11 i=1,m
          pmm=-pmm*fact*somx2
          fact=fact+2.
11      continue
      endif
      if(l.eq.m) then
        plgndr=pmm
      else
        pmmp1=x*(2*m+1)*pmm
        if(l.eq.m+1) then
          plgndr=pmmp1
        else
          do 12 ll=m+2,l
            pll=(x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m)
            pmm=pmmp1
            pmmp1=pll
12        continue
          plgndr=pll
        endif
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software ">29,)KL4.
        
