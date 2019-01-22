      program hspec8 
c
c  may  999 by jh -----------  verison 7.0 check ---------------
c  jan 20 3009 jh: change open statement for unit 2
c
c                 include routines numarg and mchdep70
c---------------------------------------------------------------------c
c                                                                     c
c      COMPUTER PROGRAMS IN SEISMOLOGY                                c
c      VOLUME VI                                                      c
c                                                                     c
c      PROGRAM: HSPEC8                                                c
c                                                                     c
c      COPYRIGHT 1985                                                 c
c      R. B. Herrmann                                                 c
c      Department of Earth and Atmospheric Sciences                   c
c      Saint Louis University                                         c
c      221 North Grand Boulevard                                      c
c      St. Louis, Missouri 63103                                      c
c      U. S. A.                                                       c
c                                                                     c
c---------------------------------------------------------------------c
      parameter(LER=0, LIN=5, LOT=6)
      parameter(NL=100)
      common/source/depth,lmax,dph  ,vamin,vamax,vbmin,vbmax,hvert
      common/damp/alpha 
      common/model/d(NL),a(NL),b(NL),rho(NL),mmax,qa(NL),qb(NL) 
      common/jout/jsrc(10) , jbdry
      character*50 name2 
      character*3 istat2
      dimension ffreq(8) 


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c
c-----
c     call machine dependent initialization
c-----
      call mchdep()
   59 format(' ',a) 
   60 format(a) 
      read(LIN,18)name2
      read(LIN,18)istat2
      read(LIN,19)ierr,ifreq
   18 format(a)
   19 format(2i5)
      read(LIN,20) alpha,depth,fl,fu,dt,n,n1,n2,df,nyq2 ,mmax
   20 format(5e15.7/3i10,e15.7,i10,i10)
      read(LIN,21)jsrc , jbdry
   21 format(11i5)
      read(LIN,22)(d(i),a(i),b(i),rho(i),qa(i),qb(i),i=1,mmax)
   22 format(6e11.4)
      read(LIN,23)lmax,dph,vamin,vamax,vbmin,vbmax,hvert
   23 format(i10,6e11.4)
      read(LIN,24)xleng, xfac
   24 format(2e15.7)
c-----
c      open(unit=2,file=name2,status=istat2,access='sequential', 
c     1 form='unformatted') 
      open(unit=2,file=name2,status='unknown',access='sequential', 
     1 form='unformatted') 
      rewind 2 
      if(ierr.eq.0)write(LOT,*)' file exists and is complete'
      if(ierr.eq.0)goto 9999
      df = 1./(n*dt) 
      nyq = nyq2/2
      write(LOT,2)  fl,fu,df,n1,n2,depth,n 
    2 format(1h ,4hfl =,f10.5,5x,4hfu =,f10.5,5x,4hdf =,f10.5,/ 
     1       5x,4hn1 =,i4,5x,4hn2 =,i4,5x,7hdepth =,f10.2,4h n =,i5) 
      write(LOT,5)alpha,dt 
    5 format(1h ,7halpha =,f10.5,5x,4hdt =,f10.3) 
      write(LOT,4) 
    4 format(1h ,39hfrequencies for which response computed     ) 
      rewind 2 
      if(ierr .lt. 4)then
         write(2) alpha,depth,fl,fu,dt,n,n1,n2,df,nyq2 
         write(2)jsrc 
         write(2)d,a,b,rho,mmax,qa,qb 
      else
         read(2)alpha,depth,fl,fu,dt,n,n1,n2,df,nyq2
         read(2)jsrc
         read(2)d,a,b,rho,mmax,qa,qb
         call skip(n1,ifreq,jsrc)
      endif
      ilow=mod(n1,8) 
       if(ilow.eq.0)ilow=8 
      iup=mod(n2,8) 
      if(iup.eq.0)iup=8 
      do 101 i=1,8 
  101 ffreq(i)=-1.0 
      n11 = n1
      if(ifreq.gt.n1)n11 = ifreq + 1
      do 100 i = n11,n2 
      freq=(i-1)*df 
      if(freq.lt.0.001) freq = 0.001 
      call excit(freq,xleng,xfac) 
      index=mod(i,8) 
      if(index.eq.0)index=8 
      ffreq(index)=freq 
      if (index.eq.8) then 
                write(LOT,3)ffreq 
                do 102 ii=1,8 
  102   ffreq(ii)=-1. 
        endif 
    3 format(1h ,8f10.5) 
  100 continue 
      write(LOT,3)ffreq 
 9999 continue
      close(2) 
      stop 
      end 
     
      subroutine aten(omega,qa,qb,xka,xkb,alpha,a,b,atna,atnb)
       real*4 qa,qb,alpha,a,b
      double complex omega,at,atna,atnb,xka,xkb
      real*8 pi, om1, oml, fac
c     reference frequency is 1.0 hz
      om1=6.2831853d+00
c     low frequency cutoff is 0.01 hz
      oml=0.062831853d+00
      pi=3.1415927d+00
      at=dcmplx(0.0d+00,0.0d+00)
      if(zabs(omega).gt.oml) at=zlog(omega/om1)/pi
      if(zabs(omega).gt.oml) go to 40
      fac=dsqrt(oml*oml + dble(alpha*alpha))/oml
      at=zlog(dcmplx(dble(oml),-dble(alpha))/(om1*fac))/pi
   40 continue
      atna=dcmplx(1.0d+00,0.0d+00)
      atnb=dcmplx(1.0d+00,0.0d+00)
      if(qa.gt.0.0) atna=(1.+dble(qa)*at+dcmplx(0.0d+00,dble(qa/2.)))
      if(qb.gt.0.0) atnb=(1.+dble(qb)*at+dcmplx(0.0d+00,dble(qb/2.)))
      xka=omega/(dble(a)*atna)
      xkb=omega/(dble(b)*atnb)
      return
      end
     
      subroutine cmult(e,ca,exa)
      double complex ca(5,5)
      real*8 exa
      real *8 xnorm
      double complex e(5)
      double complex c, ee(5)
      do 1350 i=1,5
	c = dcmplx(0.0d+00,0.0d+00)
      do 1349 j=1,5
      c=c+ e(j) * ca(j,i)
 1349 continue
      ee(i)=c
 1350 continue
      call normc(ee,exa,xnorm)
      do 1351 i=1,5
            e(i) = ee(i)*xnorm
 1351 continue
      return
      end
     
      subroutine dmult(da,aa)
      double complex aa(4,4)
      double complex sumd,ea(4,4),da(4,4)
      do 1360 i=1,4
      	do 1361 j=1,4
      		sumd = dcmplx(0.0d+00,0.0d+00)
      		do 1362 jj=1,4
      			sumd=sumd+da(i,jj) * aa(jj,j)
 1362 		continue
      		ea(i,j)=sumd
 1361 	continue
 1360 continue
      do 1363 j=1,4
      do 1364 i=1,4
            da(i,j)=ea(i,j)
 1364 continue
 1363 continue
      return
      end
     
      subroutine dnka(ca,wvno2,gam,rho)
      double complex gam,ca,cpcq,cpy,cpz,cqw,cqx,xy,xz,wy,wz,gam2,
     1 gamm1,gamm2,a0c,xz2,wy2,temp
      common/ ovrflw / a0,cpcq,cpy,cpz,cqw,cqx,xy,xz,wy,wz
      real *8 a0
      double complex wvno2
	double complex cqww2, cqxw2, g1wy2, gxz2, g2wy2, g2xz2
	double complex gg1, a0cgg1
      dimension ca(5,5)
      double complex zrho, zrho2
      zrho = dcmplx(dble(rho),0.0d+00)
      zrho2= dcmplx(dble(rho*rho),0.0d+00)
      gam2  = gam*gam
      gamm1 = gam-1.
      gamm2 = gamm1*gamm1
	cqww2 = cqw * wvno2
	cqxw2 = cqx / wvno2
	gg1 = gam*gamm1
      a0c  = dcmplx(2.0d+00,0.0d+00)*(dcmplx(a0,0.0d+00)-cpcq)
      xz2  = xz/wvno2
	gxz2 = gam*xz2
	g2xz2 = gam2 * xz2
	a0cgg1 = a0c*(gam+gamm1)
      wy2  = wy*wvno2
	g2wy2 = gamm2 * wy2
	g1wy2 = gamm1 * wy2
	temp = wvno2*(a0c + wy2) + xz
      ca(1,5) = -temp/zrho2
	temp = dcmplx(0.5d+00,0.0d+00)*a0cgg1 + gxz2 + g1wy2
      ca(1,3) = -temp/zrho
	temp = a0c*gg1 + g2xz2 + g2wy2
      ca(3,3) = a0 + temp + temp
      ca(1,1) = cpcq-temp
      temp =dcmplx(0.5d+00,0.0d+00)*a0cgg1*gg1 + gam2*gxz2 + gamm2*g1wy2
      ca(3,1) = dcmplx(2.0d+00,0.0d+00)*temp*zrho
	temp = gamm2*(a0c*gam2 + g2wy2) + gam2*g2xz2
      ca(5,1) = -zrho2*temp/wvno2
	ca(1,4) = (-cqww2+cpz)/zrho
      ca(2,1) = (-gamm2*cqw + gam2*cpz/wvno2)*zrho
	ca(2,3) = -(gamm1*cqww2 - gam*cpz)/wvno2
      ca(1,2) = (-cqx + wvno2*cpy)/zrho
	ca(3,2) = wvno2*(gam*cqxw2 - gamm1*cpy)*dcmplx(2.0d+00,0.0d+00)
	ca(4,1) = (-gam2*cqxw2 + gamm2*cpy)*zrho
      ca(2,2) = cpcq
      ca(2,4) = -wz
      ca(4,2) = -xy
      ca(2,5)=ca(1,4)
      ca(5,5)=ca(1,1)
      ca(5,4)=ca(2,1)
      ca(5,3)=-ca(3,1)/(dcmplx(2.0d+00,00d+00)*wvno2)
      ca(5,2)=ca(4,1)
      ca(4,3)= -ca(3,2)/(dcmplx(2.0d+00,00d+00)*wvno2)
      ca(4,5)=ca(1,2)
      ca(4,4)=ca(2,2)
      ca(3,4)=-dcmplx(2.0d+00,00d+00)*wvno2*ca(2,3)
      ca(3,5)=-dcmplx(2.0d+00,00d+00)*wvno2*ca(1,3)
      return
      end
     
      subroutine excit(freq,xleng,xfac)
c-----
c     sample response for all wavenumbers at a given frequency
c     using Bouchon equal wavenumber sampling = dk
c     with offset of 0.218dk
c-----
      parameter(LER=0,LIN=5,LOT=6)
      parameter(NL=100)
      complex gg(10)
      common/source/depth,lmax,dph ,vamin,vamax,vbmin,vbmax,hvert
      common/model/d(NL),a(NL),b(NL),rho(NL),mmax,qa(NL),qb(NL)
      common/jout/jsrc(10) , jbdry
      common/damp/alpha
      double complex wvn,om
c-----
c     setup wavenumber sampling, choosing two large values to
c     control the low frequency asymptotic integration technique
c
c     we will permit a depth = 0, and change the sampling in this
c     case as a fraction of thickness of the first layer
c-----
      do 10 i=mmax-1,1,-1
            if(d(i).gt.0.0)deep=d(i)
   10 continue
      if(depth.lt. 0.1*deep)then
           dpth = 0.1*deep
      else
           dpth = depth
      endif
      omega=6.2831853*freq
      wvbm = omega/vbmin
      dk = 6.2831853/xleng
      wvmm = (5.0/dpth) + xfac*wvbm
      wvzmx = wvmm * depth
      nk = wvmm / dk
      mk=nk+2
      mk1=nk+1
      write(2)omega,mk
c-----output wavenumber in reverse order
c     also output first two wavenumbers to control
c     low frequency asymptotic integration
c-----to save space in sequential i/o buffer output stream
      call bufini(1,ierr)
      do 3998 ii=mk,1,-1
            if(ii.eq.mk)then
                  if(wvzmx.gt.5.0)then
                       wv = 6.0/dpth
                  else
                       wv = wvmm + 10.0*dk
                  endif
            elseif(ii.eq.mk1)then
                  if(wvzmx.gt.5.0)then
                       wv = 2.5/dpth
                  else
                       wv = wvmm + 5.0*dk
                  endif
            else
                  wv = (ii-1)*dk + 0.218*dk
            endif
            wvn=dcmplx(dble(wv),0.0d+00)
            om=dcmplx(dble(omega),-dble(alpha))
            call rshof(gg,om,wvn,jbdry)
            call bufwr(wv)
            do 3998 j=1,10
                  if(jsrc(j).eq.1)then
                  call bufwr(real(gg(j)))
                  call bufwr(aimag(gg(j)))
            endif
 3998             continue
      call buflsh
      return
      end
     
      subroutine hska(aa,w,x,y,z,cosp,cosq,wvno2,gam,gamm1,rho)
      double complex wvno2
      double complex aa(4,4),w,x,y,z,cosp,cosq,gam,gamm1
	double complex cpq, gcpq, zw2, gzw2, g1w, g1y, gx
      real*8 drho
        drho = dble(rho)
	cpq = cosp-cosq
	gcpq = gam*cpq
	zw2 = z/wvno2
	gzw2 = gam*zw2
	g1w = gamm1*w
	g1y = gamm1*y
	gx = gam*x
	aa(1,1) = gcpq + cosq
        aa(1,3) = -cpq/dcmplx(drho,0.0d+00)
	aa(1,2)=-g1w+gzw2
      aa(1,4)=(wvno2*w-z)/dcmplx(drho,0.0d+00)
	aa(2,1)= gx - wvno2*g1y
	aa(2,2) = -gcpq + cosp
      aa(2,3)=(-x+wvno2*y)/dcmplx(drho,0.0d+00)
      aa(2,4)= - wvno2*aa(1,3)
	aa(3,1)= dcmplx(drho,0.0d+00)*gamm1*gcpq
	aa(3,2)=dcmplx(drho,0.0d+00)*((-gamm1*g1w)+(gam*gzw2))
      aa(3,4)=-wvno2*aa(1,2)
      aa(3,3)=aa(2,2)
	aa(4,1)=dcmplx(drho,0.0d+00)*(((gam*gx)/wvno2) - (gamm1*g1y))
      aa(4,2)=-aa(3,1)/wvno2
      aa(4,3)=-aa(2,1)/wvno2
      aa(4,4)=aa(1,1)
      return
      end
     
      subroutine lmult(d11,d12,cosql,yl,zl,rho,b,atnb)
      double complex d11,d12,cosql,yl,zl,atnb,h,e1,e2
      h= dcmplx(dble(rho*b*b),0.0d+00)*atnb*atnb
      yl=yl/h
      zl=zl*h
      e1=d11
      e2=d12
      d11=e1*cosql + e2*zl
      d12=e1*yl + e2*cosql
      return
      end
     
      subroutine normc(e,ex,xnorm)
      real*8 ex
      real *8 test,testt,x,y,fac,xnorm
      double complex e(*)
      test = 0.0D+00
      testt = 0.0D+00
      do 2 i = 1,5
      if(dabs(dreal(e(i))).gt.testt) testt =dabs(dreal(e(i)))
      if(dabs(dimag(e(i))).gt.testt) testt =dabs(dimag(e(i)))
    2 continue
      if(testt.lt.1.0e-30)testt=1.0
      do 1 i =1,5
      x=dreal(e(i))/testt
      y=dimag(e(i))/testt
      fac = x*x + y*y
      if(test.lt.fac) test = fac
    1 continue
      test = testt*dsqrt(test)
      if(test.lt.1.0d-30) test=1.0
      xnorm = 1./test
      ex =-dlog(xnorm)
      return
      end
     
      subroutine rshof(gg,om,wvno,jbdry)
      parameter(NL=100)
      common/model/d(NL),a(NL),b(NL),rho(NL),mmax,qa(NL),qb(NL)
      common/source/depth,lmax,dph ,vamin,vamax,vbmin,vbmax,hvert
      common/damp/alpha
        double complex ggg(10)
      double complex wvno,wvno2,wvno3
      double complex cd(5),da(4,4),fr,y(4,2)
      complex gg(10)
      double complex om,fourpi,ka2,kb2
      double complex d11,d12,fl
      double complex s12,s21,s32,s14,s23,s34,s32e,s34e
      double complex atna,atnb
	double complex wv4pi
      real *8 fact,facx,exe,exl,exel,exll,elj
      fourpi=12.5663706d+00*om*om
      if(zabs(wvno).lt.1.0d-8) go to 100
	wv4pi = 2.0d+00 * wvno / fourpi
      wvno2=wvno*wvno
      wvno3 = wvno2*wvno
      call aten(om,qa(lmax),qb(lmax),ka2,kb2,alpha,a(lmax),b(lmax),
     1 atna,atnb)
      ka2=ka2*ka2
      kb2=kb2*kb2
c     for frequencies less than 0.02 hz, use haskell formulation
c     otherwise use the dunkin formulation
      ifreq=1
      call scoef(cd,da,fr,om,wvno,exe,exl,
     1 fl,d11,d12,exel,exll,ifreq,jbdry)
      do 50 j=1,2
      y(1,j)= cd(1)*da(2,j) + cd(2)*da(3,j) - wvno2*(cd(3)*da(4,j))
      y(2,j)=-cd(1)*da(1,j) + cd(3)*da(3,j) + cd(4)*da(4,j)
      y(3,j)=-cd(2)*da(1,j) - cd(3)*da(2,j) + cd(5)*da(4,j)
      y(4,j)= wvno2*(cd(3)*da(1,j)) - cd(4)*da(2,j) - cd(5)*da(3,j)
   50 continue
      s12=dcmplx(0.0d+00,0.0d+00)
      s14=-wv4pi
      s21=2.0d+00*kb2/(dble(rho(lmax))*fourpi)
      s23=s12
      s32=wv4pi*2.*ka2/dble(rho(lmax))
      s34=wv4pi*dble( (2.*b(lmax)/a(lmax))**2 - 3.)
      s32e=ka2*wv4pi/dble(rho(lmax))
      s34e=2.0d+00*wv4pi*(ka2/kb2)
      do 60 j=1,2
           ggg(j)=s32*y(2,j) + s34*y(4,j)
           ggg(j+2)=s21*y(1,j) + s23*y(3,j)
           ggg(j+4)=s12*y(2,j) + s14*y(4,j)
           ggg(j+6)=s32e*y(2,j)+s34e*y(4,j)
   60 continue
      fact = 0.0D+00
      elj=exe-exl
      if(elj.lt.55.) fact=dexp(-elj)
      do 70 j=1,8
   70 gg(j) = (-ggg(j) * fact/fr)
      facx = 1./(12.5663706*b(lmax)*b(lmax))
      gg(9) = 2.0d+00*d11/dble(rho(lmax))
      gg(10) = -2.0d+00*wvno*d12*atnb*atnb*dble(b(lmax)*b(lmax))
      elj=exll-exel
      fact = 0.0
      if(elj.gt.-40.) fact = dexp(elj)
      facx = fact*facx
      gg(9) =(gg(9)*facx) /(fl*(atnb*atnb))
      gg(10)=(gg(10)*facx)/(fl*(atnb*atnb))
      return
  100 continue
      do 101 i = 1,10
  101 gg(i) = cmplx(0.0,0.0)
      return
      end
     
      subroutine scoef(cd,da,fr,omega,wvno,exe,exl,
     1 fl,d11,d12,exel,exll,ifreq,jbdry)
      parameter(NL=100)
      common/model/d(NL),a(NL),b(NL),rho(NL),mmax,qa(NL),qb(NL)
      common/source/depth,lmax,dph ,vamin,vamax,vbmin,vbmax,hvert
      common/damp/alpha
      double complex omega,xka,xkb,ra,rb,gam,gamm1,p,q,w,x,y
      double complex cosq,z,cosp
      double complex  da,ca,aa
            dimension da(4,4),ca(5,5),aa(4,4)
      double complex   cd(5),e(5),fr
      double complex atna,atnb
      double complex d11,d12,e1,e2,fl
      double complex yl,zl,cosql
      real *8 exe,exl,exel,exll,ex,exa,exb
      double complex wvno,wvno2
c-----
c     initialize matrices
c-----
      exe=0.0
      exl=0.0
      do 2 i = 1,4
            do 3 j = 1,4
                  da(i,j)=dcmplx(0.0d+00,0.0d+00)
    3       continue
            da(i,i) = dcmplx(1.0d+00,0.0d+00)
    2 continue
      exel = 0.0
      exll = 0.0
c-----
c     set up halfspace conditions
c-----
      wvno2=wvno*wvno
      call aten(omega,qa(mmax),qb(mmax),xka,xkb,alpha,a(mmax),
     1 b(mmax),atna,atnb)
      ra=zsqrt(wvno2-xka*xka)
      rb=zsqrt(wvno2-xkb*xkb)
      gam = dble(b(mmax))*wvno/omega
      gam = gam * atnb
      gam = 2.0d+00 * (gam * gam)
      gamm1 = gam - dcmplx(1.0d+00,0.0d+00)
c-----
c     set up halfspace boundary conditions
c
c     jbdry = -1  RIGID
c           =  0  ELASTIC
c           = +1  FREE SURFACE
c
c-----
      if(jbdry.eq.0)then
            e(1)=dble(rho(mmax)*rho(mmax))*
     1                  (-gam*gam*ra*rb+wvno2*gamm1*gamm1)
            e(2)=-dble(rho(mmax))*(wvno2*ra)
            e(3)=dble(rho(mmax))*(-gam*ra*rb+wvno2*gamm1)
            e(4)=dble(rho(mmax))*(wvno2*rb)
            e(5)=wvno2*(wvno2-ra*rb)
            e1 = dble(rho(mmax))*rb
            e2 = dcmplx(1.0d+00,0.0d+00)
     1		/(dble(b(mmax)*b(mmax))*atnb*atnb)
      elseif(jbdry.lt.0)then
            e(1) = dcmplx(1.0d+00,0.0d+00)
            e(2) = dcmplx(0.0d+00,0.0d+00)
            e(3) = dcmplx(0.0d+00,0.0d+00)
            e(4) = dcmplx(0.0d+00,0.0d+00)
            e(5) = dcmplx(0.0d+00,0.0d+00)
            e1 = dcmplx(1.0d+00,0.0d+00)
            e2 = dcmplx(0.0d+00,0.0d+00)
      elseif(jbdry.gt.0)then
            e(1) = dcmplx(0.0d+00,0.0d+00)
            e(2) = dcmplx(0.0d+00,0.0d+00)
            e(3) = dcmplx(0.0d+00,0.0d+00)
            e(4) = dcmplx(0.0d+00,0.0d+00)
            e(5) = dcmplx(1.0d+00,0.0d+00)
            e1 = dcmplx(0.0d+00,0.0d+00)
            e2 = dcmplx(1.0d+00,0.0d+00)
      endif
      do 11 i=1,5
            cd(i)=e(i)
   11 continue
      d11=e1
      d12=e2
c-----
c     matrix multiplication from bottom layer upward
c-----
      mmm1 = mmax - 1
      do 1340 k = 1,mmm1
            m = mmax - k
            call aten(omega,qa(m),qb(m),xka,xkb,alpha,a(m),b(m),
     1       atna,atnb)
            gam=dble(b(m))*(wvno/omega)
            gam = gam * atnb
            gam = dcmplx(2.0d+00,0.0d+00)*gam*gam
            gamm1 = gam - dcmplx(1.0d+00,0.0d+00)
            ra=zsqrt(wvno2-xka*xka)
            rb=zsqrt(wvno2-xkb*xkb)
            p=ra*dble(d(m))
            q=rb*dble(d(m))
            dpth=d(m)
            call var(p,q,ra,rb,w,x,y,z,
     1            cosp,cosq,ex,exa,exb,yl,zl,cosql)
            call dnka(ca,wvno2,gam,rho(m))
            exe=exe+exa
            call cmult(e,ca,exa)
            exe=exe + exa
            exel = exel + exb
            call lmult(e1,e2,cosql,yl,zl,rho(m),b(m),atnb)
            l1=lmax+1
            if(l1.eq.m) then
                  do 1352 i=1,5
                        cd(i)=e(i)
 1352             continue
                  exl=exe
                  exll = exel
                  d11=e1
                  d12=e2
            endif
c-----
c     source layer contributions
c-----
            if(m.eq.lmax)then
                  dpth=dph
                  p=ra*dble(dpth)
                  q=rb*dble(dpth)
                  call var(p,q,ra,rb,w,x,y,z,
     1                  cosp,cosq,ex,exa,exb,yl,zl,cosql)
                  call dnka(ca,wvno2,gam,rho(lmax))
                  exl=exl+exa
                  call cmult(cd,ca,exa)
                  exl=exl+exa
                  call lmult(d11,d12,cosql,yl,zl,rho(lmax),b(lmax),atnb)
                  exll=exll+exb
                  dpth=d(lmax)-dph
                  p=ra*dble(dpth)
                  q=rb*dble(dpth)
                  call var(p,q,ra,rb,w,x,y,z,
     1                  cosp,cosq,ex,exa,exb,yl,zl,cosql)
                  call hska(aa,w,x,y,z,cosp,cosq,wvno2,gam,
     1                  gamm1,rho(lmax))
                  call dmult(da,aa)
                  exl=exl+ex
            endif
            if(m.lt.lmax) then
                  call hska(aa,w,x,y,z,cosp,cosq,wvno2,gam,gamm1,rho(m))
                  call dmult(da,aa)
                  exl=exl+ex
            endif
 1340 continue
      fl=e1
      fr=e(1)
      return
      end
     
      subroutine skip(n1,ifreq,jsrc)
      complex gg(10)
      dimension jsrc(10)
      do 1300 i=n1,ifreq
          read(2)omega,nk
      call bufini(0,ierr)
          do 1400 mk=1,nk
            call bufrd(wvno,ierr)
             do 1500 j=1,10
                 if(jsrc(j).eq.1)then
            call bufrd(xr,ierr)
            call bufrd(xi,ierr)
            gg(j)=cmplx(xr,xi)
      endif
 1500        continue
 1400     continue
 1300 continue
      return
      end
     
      subroutine var(p,q,ra,rb,w,x,y,z,cosp,cosq,ex,
     1exa,exl,yl,zl,cosql)
c     not modified for negative p,q
c     this assumes that real p and real q have same signs
      common/ovrflw/a0,cpcq,cpy,cpz,cqw,cqx,xy,xz,wy,wz
      double complex cpcq,cpy,cpz,cqw,cqx,xy,xz,wy,wz
      double complex p,q,ra,rb,w,x,y,z,cosp,cosq
      double complex yl,zl,cosql
      double complex eqp,eqm,epp,epm,sinp,sinq
      real *8 a0,pr,pi,qr,qi,fac,qmp,ex,exa,exl
      ex=0.0d+00
      exl = 0.0d+00
      a0=0.0d+00
      pr=real(p)
      pi=dimag(p)
      qr=real(q)
      qi=dimag(q)
      epp=dcmplx(dcos(pi),dsin(pi))/2.
      epm=dconjg(epp)
      eqp=dcmplx(dcos(qi),dsin(qi))/2.
      eqm=dconjg(eqp)
      ex=pr
      exl=qr
      fac=0.0
      if(pr.lt.15.) fac=dexp(-2.*pr)
      cosp=epp + fac*epm
      sinp=epp - fac*epm
      w=sinp/ra
      x=ra*sinp
      fac=0.0d+00
      if(qr.lt.15.) fac=dexp(-2.*qr)
      cosql=eqp + fac*eqm
      sinq=eqp - fac*eqm
      yl=sinq/rb
      zl=rb*sinq
      exa=pr + qr
      cpcq=cosp*cosql
      cpy=cosp*yl
      cpz=cosp*zl
      cqw=cosql*w
      cqx=cosql*x
      xy=x*yl
      xz=x*zl
      wy=w*yl
      wz=w*zl
      fac=0.0d+00
      qmp=qr-pr
      if(qmp.gt.-40.) fac=dexp(qmp)
      cosq=cosql*fac
      y=fac*yl
      z=fac*zl
      fac=0.0d+00
      if(exa.lt.60.) a0=dexp(-exa)
      return
      end
     
      subroutine bufini(irdwr,ierr)
c------initialize buffer pointer
c------irdwr = 0 read initialize
c------irdwr = 1 write initialize
      integer BUFMAX
       parameter(BUFMAX=2000)
       common/buf/iptr,max,buffer(BUFMAX)
c       save /buf/
      iptr = 1
      if(irdwr.eq.0)call getbuf(ierr)
      return
      end
     
        subroutine buflsh
c------flush output buffer
      integer BUFMAX
      parameter(BUFMAX=2000)
        common/buf/iptr,max,buffer(BUFMAX)
c     save /buf/
      ipt = iptr -1
      if(ipt.gt.0)write(2)ipt,(buffer(i),i=1,ipt)
        iptr = 1
      return
      end
     
        subroutine bufwr(x)
c------fill buffer with floating point variable x,
c------flush buffer as necessary
      integer BUFMAX
      parameter(BUFMAX=2000)
      common/buf/iptr,max,buffer(BUFMAX)
c     save /buf/
      buffer(iptr) = x
      iptr = iptr + 1
      if(iptr.gt.BUFMAX)call buflsh
      return
      end
     
        subroutine getbuf(ierr)
c------read in file contents into buffer, taking care not to
c------read beyond the contents of the file
      integer BUFMAX
      parameter(BUFMAX=2000)
      common/buf/iptr,max,buffer(BUFMAX)
c     save /buf/
c------ierr = 0 successful read
c------     = 1 read error
c------     = 2 end of file
c------
      read(2,err=1000,end=2000)max,(buffer(i),i=1,max)
      iptr = 1
      ierr = 0
      return
 1000 ierr = 1
      return
 2000 ierr = 2
      return
      end
     
      subroutine bufrd(x,ierr)
c-----retrieve a value from buffer array, red in new array
c-----as necessary
c-----iptr is here the next array element to be read
c-----it is always >= 1. We do not worry the upper limit
c-----since the calling program must worry about this
c-----because read always follows a complete write
      integer BUFMAX
      parameter(BUFMAX=2000)
      common/buf/iptr,max,buffer(BUFMAX)
c     save /buf/
c       only yank in new data if actually required
      if(iptr.gt.max)call getbuf(ierr)
      x = buffer(iptr)
      iptr = iptr + 1
      return
      end

	function numarg()
	integer numarg
	numarg = iargc()
	return
	end

      subroutine mchdep()
c---------------------------------------------------------------------c
c                                                                     c
c      COMPUTER PROGRAMS IN SEISMOLOGY                                c
c      VOLUME VI                                                      c
c                                                                     c
c      PROGRAM: MCHDEP                                                c
c                                                                     c
c      COPYRIGHT 1985                                                 c
c      R. B. Herrmann                                                 c
c      Department of Earth and Atmospheric Sciences                   c
c      Saint Louis University                                         c
c      221 North Grand Boulevard                                      c
c      St. Louis, Missouri 63103                                      c
c      U. S. A.                                                       c
c                                                                     c
c---------------------------------------------------------------------c
      return
      end
