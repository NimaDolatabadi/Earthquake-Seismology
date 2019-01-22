c**************************************************************
c subroutine to find travel times for a given offset using a
c gradient model. Inputs are the same as for dtdx2          
c Presently, the routine only calculates the minimum
c travel time, regardless of ptype
c Refraction can also only occur in the bottom (half-space)
c layer               
c no phase ID is returned in phsid
c
c Barry Lienert June 1998
c
c changes
c
c sep 98 by jh : ----------- seisan version 7.0 check ----------------
c                no changes                         
c
c  oct 28 99 bmt : linux changed, save and *
c  nov 19 99 jh  : nl and parm in common block
c  may 15 00 jh  : fix bugs cause by above
c
      subroutine dtdxg(xh,x0,ptype,nmoho,nconrad,iulst,tmin,
     &delta,ann,iflag,phsid)
      save
      include 'hypparm.inc'  
      character*1 ptype
      character*8 phsid  
      dimension dcrit(200),z(200),grad(200),vr(200)
c      data pi2/1.57079/
      data pi2/1.57079633/
c get epicentral distance from event to station      
      call delaz(x0(2),x0(1),delta,dedeg,azz0,xh(2),xh(1))
      delta=abs(delta)
c calculate needed parameters
      nn=nl*2-1
      n_layer=nl
c      n_layer=(nn+1)/2   ! nov 19 jh            
      n_iter=20         !# iterations to find delta
      phi_incr=.001     !slowness increment in iterations
      sum=0.0
      do i=1,n_layer
cx       v(i)=parm(i)     !layer velocity
cx       d(i)=parm(nl+i)  !layer thickness
       z(i)=sum         !layer depth
       sum=sum+parm(n_layer+i)
      end do                                   

      z_hypo=xh(3)

c find n_hypo, the layer that the hypocenter is in
      n_hypo=n_layer
      do i=1,n_layer-1
       d(i)=z(i+1)-z(i)             
       vr(i)=v(i+1)/v(i)
       grad(i)=(v(i+1)-v(i))/d(i)
       if(z(i).le.z_hypo)n_hypo=i
      end do 
c now find v_hypo
      if(n_hypo.lt.n_layer)then
       v_hypo=v(n_hypo)+grad(n_hypo)*(z_hypo-z(n_hypo))
      else 
       v_hypo=v(n_layer)
      endif 
c find v_max, max vel above hypocenter
      v_max=0.0
      do i=1,n_hypo
       if(v(i).gt.v_max)v_max=v(i)
      end do 

      if(delta.gt.0.0)then
c find the critical distance, dcrt & time, tcrt
       pcrit=1./v(n_layer)
       phi=2.*pi2-asin(pcrit*v(n_hypo))
       call ttcal(n_layer,v,z,d,vr,grad,z_hypo,pcrit,phi,dcrt,tcrt,
     &  n_hypo,v_hypo,v_max,iflagd)                           
c extend the travel time from dcrt to delta using v(n_layer)
       if(delta.ge.dcrt)then
        tmin=tcrt+(delta-dcrt)/v(n_layer)
        iflagd=1
        return
       endif 
c find p for the ray that exits the hypocenter layer at a
c horizontal offset delta (see Lee & Stewart, 1981, p 93)
       eta=z_hypo-z(n_hypo)
       xc=0.5*(delta*delta-2.*eta*v(n_hypo)/grad(n_hypo)-eta*eta)/delta
c      zc=v(1)*d(1)/(v(2)-v(1))
       zc=v(n_hypo)/grad(n_hypo)
c phi is the ray direction measured clockwise from the upward vertical
       phi=atan2(-zc-eta,-xc)
       p=abs(sin(phi))/v(n_hypo)
c       phi=atan(2.*zc/delta)
      else
c zero delta case
       phi=2.*pi2
       p=0.0
       if(z_hypo.eq.0.0)then
        tmin=0.0
        return
       endif 
      endif  

      if(z_hypo.ne.0.0)then
       if(delta.eq.0.0)then
        call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tmin,
     &   n_hypo,v_hypo,v_max,iflagd)
c p=0: don't need to iterate here
        return
       else 
c try a ray at the p calculated to make the ray leave the
c hypocenter layer at distance delta
        phi0=phi
        p0=p
        call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tmin,
     &   n_hypo,v_hypo,v_max,iflagd)
c ray did not come out: try smaller phi's        
        if(iflagd.eq.0)then 
         itr=0 
         phi=phi0
         do while(iflagd.eq.0.and.itr.lt.n_iter)
          phi=phi-phi_incr
          p=abs(sin(phi))/v(n_hypo) 
          call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tmin,
     &     n_hypo,v_hypo,v_max,iflagd)
          itr=itr+1
         end do    
         if(iflagd.eq.0)then !try other direction
          itr=0
          phi=phi0
          do while(iflagd.eq.0.and.itr.lt.niter)
           phi=phi+phi_incr
           p=abs(sin(phi))/v(n_hypo) 
           call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tmin,
     &      n_hypo,v_hypo,v_max,iflagd)
           itr=itr+1
          end do
         endif
        else
c         dist0=dist
         phi=phi0
         do while(iflagd.eq.0.and.itr.lt.niter)
          phi=phi-phi_incr
          p=abs(sin(phi))/v(n_hypo) 
          call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tmin,
     &     n_hypo,v_hypo,v_max,iflagd)
c         write(iulst,'(3f12.4)')delta,dist0,dist
         end do
         if(iflagd.eq.0)then
          do while(iflagd.eq.0.and.itr.lt.niter)
           phi=phi+phi_incr
           p=abs(sin(phi))/v(n_hypo) 
           call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tmin,
     &     n_hypo,v_hypo,v_max,iflagd)
          end do
         endif
         itr=0
         do while (abs(dist-delta).gt.0.01.and.itr.lt.niter)
          if(dist.ne.dist0)then
           slope=(phi-phi0)/(dist-dist0) 
           if(slope.gt..1)slope=.1
           if(slope.lt.-.1)slope=-.1
          endif 
          phi0=phi
          p0=p
          phi=phi0+slope*(delta-dist)
          p=abs(sin(phi))/v(n_hypo)
          dist0=dist
          call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,
     &     tmin,n_hypo,v_hypo,v_max,iflagd)
c          write(iulst,'(i3,4f10.4)')itr,p,phi,dist0,dist
          itr=itr+1
         end do
        endif                            
       endif
      else
       call ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tmin,n_hypo,
     &  v_hypo,v_max,iflagd)
      endif                            
c      write(iulst,*)p,delta,dist,itr
      return
      end

c********************************************************************
c  calculates distance and travel time as a function of ray parameter
c  for a gradient model
c  modified from Fred Klein's TTCAL routine by Barry Lienert, June 98
c********************************************************************
      subroutine ttcal(n_layer,v,z,d,vr,grad,z_hypo,p,phi,dist,tp,
     & n_hypo,v_hypo,v_max,iflagd)
c     Inputs: n_layer=number of layers in velocity model
c             v(i)=velocities in km/s
c             z(i)=layer top positions in km
c             z_hypo=hypocentral depth in km
c             p=ray parameter in s/km
c
c     Outputs: dist=delta in km
c              tp=travel time in s
c              iflagd=1 for valid arrival, zero otherwise
c
      save
      parameter (maxlayer=200)
      real cp(maxlayer),vr(maxlayer)
      real v(maxlayer),z(maxlayer),d(maxlayer),grad(maxlayer)
c      data pi2/1.57079/
      data pi2/1.57079633/
c--case of a ray travelling straight up (p=0)
      if(p.eq.0.0)then
       phid=0.
       dist=0.
       tp=(z_hypo-z(n_hypo))/v(n_hypo)
       if (grad(n_hypo).ne.0.)then
        tp=alog(v_hypo/v(n_hypo))/grad(n_hypo)
       endif 
       if (n_hypo.gt.1)then
        do i=1,n_hypo-1
         temp=d(i)/v(i)
         if (grad(i).ne.0.) temp=alog(vr(i))/grad(i)
         tp=tp+temp
        end do 
       endif
       tr=tp
       iflagd=1
       return
      endif 

c sph is sin(phi) where phi is angle at which ray leaves the hypocenter
      sph=sin(phi)
      cph=cos(phi)                                       
      p=sph/v_hypo
      
c case of no upward ray
      if(p.ge.1./v_hypo)then
       tp=-1.
       dist=-1.
       iflagd=0
       return
      endif 

c vb is the bottoming velocity
      vb=1./p
c--n_bottom is the layer in which downgoing ray bottoms
      n_bottom=1
      do while (v(n_bottom+1).lt. vb.and.n_bottom.lt.n_layer)
       n_bottom=n_bottom+1
      end do 
c--calc depth at ray bottom, zb
      zb=z(n_bottom)
      if (grad(n_bottom).ne.0.) zb=zb+(vb-v(n_bottom))/grad(n_bottom)

c++++++++++++ ray lost to waveguide ++++++++++++
c--now have case of ray going into waveguide formed by a lvz
c--flag the nonexistent time and dist for this case
      if (vb.le.v_max .and. v_hypo.lt.v_max)then
       tp=-1.
       dp=-1.
       tr=-1.
       n_bottom=0
       iflagd=0
       return
      endif 

c++++++++++++ calculate time and distance ++++++++++++
c--calc t and d since it is valid to do so. consider all 3 cases:
c  1 ray nearly horiz in layer of zero gradient
c  2 upgoing ray
c  3 downgoing ray
c--for each term must consider zero and non-zero gradients as sep cases

c--case 1. nearly horiz non-curving ray+++++++++++
      if (grad(n_hypo).eq.0..and.phi.le.0.001)then
c--set dist to be some arbitrary but large no
       dist=100000.
       tp=100000./v(n_hypo)
       iflagd=1
       return
      endif 

c--case 2.   upgoing ray+++++++++++++++++++++++++++
c      if (phi.le.pi2+.0005)then
      if (phi.le.pi2)then
c--calc cosines of emergence angles at each interface
       ss=1.0
       if(phi.gt.pi2.and.phi.le.2.*pi2)ss=-1.
       if(phi.gt.2.*pi2.and.phi.le.3.*pi2)ss=-1.
       if(phi.gt.3.*pi2.and.phi.le.4.*pi2)ss=1.
       
       do i=1,n_hypo
        cp(i)=ss*sqrt(abs(1.-(v(i)*p)**2))
       end do 

c--contribution to tp and dist of layer in which hypo lies
       if (grad(n_hypo).gt.0.)then
        dist=(cp(n_hypo)-cph)/(p*grad(n_hypo))
        if(cph.gt.-1.0)then
         tp=alog(v_hypo*(1.+cp(n_hypo))/(v(n_hypo)
     &    *(1.+cph)))/grad(n_hypo)
        else
         tmin=-1.0
         dist=-1.0
         iflagd=0
         return
        endif 
       else
        dist=(z_hypo-z(n_hypo))*sph/cph
        tp=(z_hypo-z(n_hypo))/(cph*v_hypo)
       endif

c--add contributions from layers above hypo
       if (n_hypo.gt.1)then
        do i=1,n_hypo-1
         if (grad(i).gt.0.)then
          dist=dist+(cp(i)-cp(i+1))/(p*grad(i))
          tp=tp+alog(vr(i)*(1.+cp(i))/(1.+cp(i+1)))/grad(i)
         else
          dist=dist+d(i)*p*v(i)/cp(i)
          tp=tp+d(i)/(cp(i)*v(i))
         endif
        end do
       endif  
       iflagd=1
       return
      
      else
       
c case 3  downgoing ray +++++++++++++++++++++++++++++
c--calc cosines of emergence angles at layer interfaces
       do i=1,n_bottom
        cp(i)=sqrt(1.-(v(i)*p)**2)
       end do 
c--contribution to t and d from layer in which ray bottoms
       if (grad(n_bottom).gt.0.)then
        dist=2.*cp(n_bottom)/(p*grad(n_bottom))
        tp=2.*alog((1.+cp(n_bottom))/(v(n_bottom)*p))/grad(n_bottom)
       else
c--a ray can't bottom in a homogeneous layer
c--if this case should arise, reflect ray from top of the layer
        dist=0.
        tp=0.
       endif 
c--subtract contribution from path immed above hypo in its layer
       if (grad(n_hypo).gt.0.)then
        dist=dist-(cp(n_hypo)-cph)/(p*grad(n_hypo))
        tp=tp-alog(v_hypo*(1.+cp(n_hypo))/(v(n_hypo)
     &   *(1.+cph)))/grad(n_hypo)
       else
        dist=dist-(z_hypo-z(n_hypo))*sph/cph
        tp=tp-(z_hypo-z(n_hypo))/(cph*v_hypo)
       endif 
c--sum terms over layers above hypo
       if (n_hypo.gt.1)then
        do i=1,n_hypo-1
         if (grad(i).gt.0.)then
          dist=dist+(cp(i)-cp(i+1))/(p*grad(i))
          tp=tp+alog(vr(i)*(1.+cp(i))/(1.+cp(i+1)))/grad(i)
         else
          dist=dist+d(i)*p*v(i)/cp(i)
          tp=tp+d(i)/(cp(i)*v(i))
         endif
        end do
       endif   
c--sum terms over layers between hypo and ray bottom (if any)
c--include layer in which hypo lies
       if (n_hypo.ne.n_bottom)then 
        do i=n_hypo,n_bottom-1
         if (grad(i).ne.0.)then
          dist=dist+2.*(cp(i)-cp(i+1))/(p*grad(i))
          tp=tp+2.*alog(vr(i)*(1.+cp(i))/(1.+cp(i+1)))/grad(i)
         else
          dist=dist+2.*d(i)*p*v(i)/cp(i)
          tp=tp+2.*d(i)/(cp(i)*v(i))
         endif
        end do
       endif
       iflagd=1
       return  
      endif 
      end

