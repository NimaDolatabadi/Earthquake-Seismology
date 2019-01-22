c     program rhwvinta
c
c  may 99 by jh --------------- version 7.0 check --------------
c               include numarg and mchdep70
c  jan 20 2009 jh : change open statement unit 4 from new to unknown
c
c---------------------------------------------------------------------c
c                                                                     c
c      COMPUTER PROGRAMS IN SEISMOLOGY                                c
c      VOLUME VI                                                      c
c                                                                     c
c      PROGRAM: RHWVINTA                                              c
c                                                                     c
c      COPYRIGHT 1985 R. B. Herrmann                                  c
c                                                                     c
c      Department of Earth and Atmospheric Sciences                   c
c      Saint Louis University                                         c
c      221 North Grand Boulevard                                      c
c      St. Louis, Missouri 63103                                      c
c      U. S. A.                                                       c
c                                                                     c
c---------------------------------------------------------------------c
      parameter (LER=0,LIN=5,LOT=6)
      parameter(NL=100)
      common/c/cmax,c1,c2,cmin 
      common/model/d(NL),a(NL),b(NL),rho(NL),mmax,qa(NL),qb(NL) 
      common/jout/jsrc(10) 
      dimension ar(10),ai(10) 
      common/virt/ data(10,2) 
      character*4 icchar(11) 
      character*50 names 
c-----rhwvinta rearranges the hspec8 jsrc mapping 
      dimension lsrc(10) 
      data lsrc/1,2,3,4,9,5,6,10,7,8/ 
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c-----
c     call machine dependent initialization
c-----
      call mchdep()
      write(LOT,*)' enter name of input file' 
      read(LIN,60)names 
   60 format(a) 
      open(unit=2,file=names,access='sequential',form= 
     1 'unformatted',status='old') 
      write(LOT,*)' enter name of output file' 
      read(LIN,60)names 
      open(unit=4,file=names,access='sequential',form= 
     1 'unformatted',status='unknown') 
      rewind 2 
      icchar(1)=' z1 ' 
      icchar(2) = ' r1 ' 
      icchar(3) = ' z2 ' 
      icchar(4) = ' r2 ' 
      icchar(5) = ' t2 ' 
      icchar(6) = ' z3 ' 
      icchar(7) = ' r3 ' 
      icchar(8) = ' t3 ' 
      icchar(9) = ' z4 ' 
      icchar(10) = ' r4 ' 
      icchar(11) = ' xx ' 
      read(LIN,1) cmax,c1,c2,cmin 
      write(LOT,2)cmax,c1,c2,cmin 
c     cosine taper between cmax-c1 and c2-cmin 
c     unity response between c1 and c2 
c      zero response outside these windows 
c     the cmax,c1,c2, and cmin are inverse phase velocities 
      read(2)  alpha,depth,fl,fu,dt,n,n1,n2,df,nyq2 
      read(2)jsrc 
      read(2) d,a,b,rho,mmax,qa,qb 
      write(4) alpha,depth,fl,fu,dt,n,n1,n2,df,nyq2 
      write(4)jsrc 
c-----write out lsrc indexing since rhwvinta changes order 
      write(4)lsrc 
      write(4)d,a,b,rho,mmax,qa,qb 
  500 continue 
      rewind 2 
      read(LIN,1) r,tshift,vred 
c 
      read(2)  alpha,depth,fl,fu,dt,n,n1,n2,df,nyq2 
      read(2)jsrc 
      read(2)d,a,b,rho,mmax,qa,qb 
    1 format(4f10.5,i5) 
      write(LOT,2)r,tshift,vred 
    2 format(1h ,4f10.4) 
c     r = epicentral distance 
c     r .le. 0 terminate phase 
c     tshift .eq. 0 use reduced travel time,otherwise use tshift read in 
      if(vred.eq.0.0) vred=1.0e+10 
      t0 = tshift + r/vred
      write(4) r,t0 
      if(r.lt.0.0) go to 9998 
        call setup(depth,r) 
c     reduced travel time time shift 
c     evaluation of wavenumber integrals 
      do 250 jj=1,10 
  250 write(4) icchar(jj) 
      do 300 i=n1,n2 
      freq=(i-1)*df 
      fac=6.2831853*freq*t0 
      xr = cos(fac) 
      xi = sin(fac) 
      call wvint(r,ar,ai,depth) 
      j=1 
      k=2 
      do 310 jj=1,10 
      data(jj,j) = xr*ar(jj) - xi*ai(jj) 
      data(jj,k) = xr*ai(jj) + xi*ar(jj) 
  310 continue 
      do 305 jj=1,10 
      if(jsrc(lsrc(jj)).eq.1)write(4) data(jj,j),data(jj,k) 
  305 continue 
  300 continue 
      go to 500 
 9998 continue 
      close(2) 
      close(4) 
      stop 
      end 
     
      subroutine wvint(r,ar,ai,depth) 
c-----to work with potentially large disk files, we cannot read in
c-----all wavenumbers at once. We only work with neighboring
c-----points at any time. The first two are for the DC correction,
c-----followed by wavenumbers in decreasing order
      common/c/cmax,c1,c2,cmin 
      common/jout/jsrc(10) 
      common/fct/fact ,fact0
        common/asym/j0k0,j0k1,j0k2,j0k3,j1k0,j1k1,j1k2,j1k3, 
     1  j2k0,j2k1,j2k2,j2k3 
        real j0k0,j0k1,j0k2,j0k3,j1k0,j1k1,j1k2,j1k3,j2k0,j2k1, 
     1  j2k2,j2k3 
      dimension ar(10),ai(10) 
        complex aa(10),bb(10),cc(10) 
      complex gg,gg1,sumc 
      complex h0,h1 
      complex g(10,2) 
      complex smm(10) 
      complex sumd 
      complex f(2)
      dimension wvn(2) 
      real j0,j1,j01,j11 
      logical iasymp
      read(2)  omega,nk 
       call bufini(0,ierr)
      nkm2 = nk -2
c choose proper strategy for integration, e.g. 
c whether or not to use asymptotic technique 
c-----get the last wavenumber and g values
      call getgk(g,2,jsrc,wvn(2))
c-----get the second last values
      call getgk(g,1,jsrc,wvn(1))
c-----
c     find asymptotic coefficients using these values
c-----
      nlow = 1
      nupp = 2
      do 102 j=1,10
            call solu(g(j,nlow),g(j,nupp),wvn(nlow),wvn(nupp),
     1                  depth,j,aa(j),bb(j),cc(j) ) 
  102 continue
c-----
c     we no longer need the first value to shift
      wvnosv = wvn(2)
      call shift(g,wvn)
c-----
c-----
c     get first usable wavenumber which is the largest
c-----
      call getgk(g,1,jsrc,wvn(1))
c-----
c     decide whether to invoke asymptotic correction
c-----
      if(wvn(1).gt.wvnosv)then
            iasymp = .false.
      else
            iasymp = .true.
      endif
c-----
c     initialize integral
c-----
      call intini(smm,aa,bb,cc,iasymp,r)
c-----
c     we now can procede with integration
c-----
c-----
c     in the variables below the t0,j0,j1,sum refer to upper limit
c     of integration and t01,j01,j11 and sum1 refer to the lower limit
c-----
      wvcm=omega*cmax 
      wvc1=omega*c1 
      wvc2=omega*c2 
      wvcn=omega*cmin 
      wvno = wvn(1)
      t0 =wvno*r
      call hank(t0,1.0,h0,h1)
      j0 = real(h0)
      j1 = real(h1)
      if(iasymp)then
            call gasym(g,aa,bb,cc,wvn,1,depth,jsrc)
      endif
c-----
c     perform wavenumber filtering
c-----
      call wvfilt(cmax,wvcm,wvc1,wvc2,wvcn,wvno,fact)
      do 200 ik = nkm2,1,-1
      i = 2
      if(ik.lt.nkm2)then
      call shift(g,wvn)
      call getgk(g,1,jsrc,wvn(1))
      endif
c------to avoid trouble ignore sets of identical waveumbers
      if(wvn(1).eq.wvn(2))goto 200
c-----
c     now g(2) and wvn(2) refer to upper limit
c     now g(1) and wvn(1) refer to lower limit
c-----
c-----for the first wavenumber small use asymptotic values
                if(wvno.lt.1.0e-8.and.ik.eq.2)then 
                        g(4,1)=g(4,2) 
                        g(9,1)=g(9,2) 
                endif 
      i1 = i-1
      wvno1 = wvn(i1)
      t01 = wvno1 * r
      dkk = (wvno - wvno1) * r
      call hank(t01,1.0,h0,h1) 
      j01=real(h0) 
      j11=real(h1) 
c-----
c     if the asymptotic technique is invoked, the functional
c     values must be changed
c-----
      if(iasymp)then
            call gasym(g,aa,bb,cc,wvn,1,depth,jsrc)
      endif
c-----
c     perform windowing in wavenumber domain to pass
c     certain ranges of phase velocity
c-----
      call wvfilt(cmax,wvcm,wvc1,wvc2,wvcn,wvno,fact0)
      if(jsrc(1).eq.1)then
        call fmake(f,1,0,i,wvn,0,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,0,dkk,ik) 
        smm(1) = smm(1) + sumd 
      endif
      if(jsrc(2).eq.1)then
        call fmake(f,2,0,i,wvn,1,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,1,dkk,ik) 
        smm(2) = smm(2) + sumd 
      endif
      if(jsrc(3).eq.1)then
        call fmake(f,3,0,i,wvn,0,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,1,dkk,ik) 
        smm(3) = smm(3) + sumd 
      endif
      sumc = cmplx(0.0,0.0)
c-----
c           only include near field term if both SH and P-SV
c           computed
c-----
      if(jsrc(4).eq.1.and.jsrc(9).eq.1)then
        call fmake(f,4,9,i,wvn,0,g,gg,gg1)
        call wint(sumc,gg,gg1,j0,j01,j1,j11,t0,t01,1,dkk,ik) 
      endif
      if(jsrc(4).eq.1)then
        call fmake(f,4,0,i,wvn,1,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,0,dkk,ik) 
        smm(4) = smm(4) + sumd - sumc/r 
      endif
      if(jsrc(9).eq.1)then
        call fmake(f,9,0,i,wvn,1,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,0,dkk,ik) 
        smm(5) = smm(5) + sumd - sumc/r 
      endif
      if(jsrc(5).eq.1)then
        call fmake(f,5,0,i,wvn,0,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,2,dkk,ik) 
        smm(6) = smm(6) + sumd 
      endif
      sumc = cmplx(0.0,0.0)
c-----
c           only include near field term if both SH and P-SV
c           computed
c-----
      if(jsrc(6).eq.1.and.jsrc(10).eq.1)then
        call fmake(f,6,10,i,wvn,0,g,gg,gg1)
        call wint(sumc,gg,gg1,j0,j01,j1,j11,t0,t01,2,dkk,ik) 
      endif
      if(jsrc(6).eq.1)then
        call fmake(f,6,0,i,wvn,1,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,1,dkk,ik) 
        smm(7) = smm(7) + sumd - 2.*sumc/r 
      endif
      if(jsrc(10).eq.1)then
        call fmake(f,10,0,i,wvn,1,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,1,dkk,ik) 
        smm(8) = smm(8) + sumd - 2.*sumc/r 
      endif
      if(jsrc(7).eq.1)then
        call fmake(f,7,0,i,wvn,0,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,0,dkk,ik) 
        smm(9) = smm(9) + sumd 
      endif
      if(jsrc(8).eq.1)then
        call fmake(f,8,0,i,wvn,1,g,gg,gg1)
        call wint(sumd,gg,gg1,j0,j01,j1,j11,t0,t01,1,dkk,ik) 
        smm(10) = smm(10) + sumd 
      endif
  201 continue 
      t0=t01
      j1=j11 
      j0 = j01
      wvno = wvno1
      fact = fact0
  200 continue 
c     sign change due to k j(-1) 
      smm(2) = -smm(2) 
      smm(10) = -smm(10) 
      do 300 i = 1,10 
      smm(i)=smm(i)/r 
      ar(i) = real(smm(i)) 
      ai(i) = aimag(smm(i)) 
  300 continue 
      return 
      end 
     
      subroutine wint(smm,g,g1,j0,j01,j1,j11,t0,t01,n,dkk,ik) 
      common/fct/fact1 ,fact0
      complex smm,g,g1 
      real j0,j1,j01,j11 
      real j2,j21
      nn = n + 1 
c-----
c     trapezoidal rule
c-----
c     if(ik.eq.1)then
c           fact = 0.5*fact0
c     else
c           fact = fact0
c     endif
c-----
c       if commented out then rectangular rule
c-----
      fact = fact0
      go to (1,2,3),nn 
    1 continue 
c      integral (c + d z) * j0(z) dz 
      smm = fact * g1 * j01 * dkk
      return 
    2 continue 
c      integral (c + d z) j1(z) dz 
      smm = fact * g1 * j11 * dkk
      return 
    3 continue 
      if(t0.eq.0.0)then
            j2 = 0.0
      else
            j2 = 2.*j1/t0 - j0
      endif
      if(t01.eq.0.0)then
            j21 = 0.0
      else
            j21 = 2.*j11/t01 - j01
      endif
c      integral (c + d z) j2(z) dz 
      smm = fact * g1 * j21 * dkk
      return 
      end 
     
      subroutine hank(wvno,r,h0,h1) 
      complex h0,h1 
      real j0,j1,j1z 
      z = wvno*r 
      if(z.gt.0.0) go to 100 
      h0=cmplx(1.0,0.0) 
      h1=cmplx(0.0,0.0) 
      return 
  100 if(z.gt.3.0) go to 200 
      x = (z/3.)*(z/3.) 
      j0 = 1.-x*(2.2499997-x*(1.2656208-x*(.3163866-x*( 
     1.0444479-x*(.0039444-x*(.0002100)))))) 
      j1z = 0.5-x*(.56249985-x*(.21093573-x*(.03954289-x*( 
     1.00443319-x*(.00031761-x*(.00001109)))))) 
      j1 = z * j1z 
      h0 = cmplx(j0,0.0) 
      h1 = cmplx(j1,0.0) 
      return 
  200 continue 
      x = 3./z 
      fac = 1./sqrt(z) 
      f0 = .79788456+x*(-.00000077 + x*(-.00552740 + x*( 
     1-.00009512+x*(.00137237+x*(-.00072805+x*(.00014476)))) 
     2)) 
      t0 = z - .78539816+x*(-.04166397+x*(-.00003954+x*( 
     1.00262573+x*(-.00054125+x*(-.00029333+x*(.00013558)))) 
     2)) 
      f1 = .79788456+x*(.00000156+x*(.01659667+x*(.00017105+ 
     1x*(-.00249511+x*(.00113653+x*(-.00020033)))))) 
      t1 = z-2.35619449+x*(.12499612+x*(.00005650+x*( 
     1 -.00637879+x*(.00074348+x*(.00079824+x*(-.00029166))) 
     2))) 
      j0 = fac * f0 * cos(t0) 
      j1 = fac * f1 * cos(t1) 
      h0 = cmplx(j0,0.0) 
      h1 = cmplx(j1,0.0) 
      return 
      end 
     
        subroutine solu(y1,y2,x1,x2,h,j,a,b,c) 
c we ndo not solve for a,b,c together, only two at most 
c thus we only need two values of wavenumber, x1 and x2 
        complex y1,y2,a,b,c 
        c=cmplx(0.0,0.0) 
        go to (300,200,300,200,300,200,300,200,200,200),j 
  100   continue 
c---------aexp(-kh) 
        b=cmplx(0.0,0.0) 
        a=y1*exp(x1*h) 
        return 
  200   continue 
c---------[ a + b k  ]exp(-kh) 
        u1=x1*h 
        u2=x2*h 
        det=x2-x1 
        a= x2*y1*exp(u1)-x1*y2*exp(u2) 
        a=a/det 
        b= y2*exp(u2) - y1*exp(u1) 
        b=b/det 
        return 
  300   continue 
c---------[ a + b k  ]  k exp(-kh) 
        u1=x1*h 
        u2=x2*h 
        det=x2-x1 
        a = cmplx(0.0,0.0) 
        b = x2*y1*exp(u1)/x1 - x1*y2*exp(u2)/x2 
        b = b/det 
        c= y2*exp(u2)/x2 - y1*exp(u1)/x1 
        c = c/det 
        return 
  400   continue 
c-------- a k*k*exp(-kh) 
        a = cmplx(0.0,0.0) 
        b = cmplx(0.0,0.0) 
        c = y1 * exp(x1*h)/(x1)**2 
        return 
  500   continue 
c-------- a k exp(-kh) 
        a = cmplx(0.0,0.0) 
        b = y1 * exp(x1*h)/ x1 
        return 
        end 
     
        subroutine setup(zz,rr) 
c---------------------------------------------------------- 
c 
c       jnkm = r integral exp(-kh) ksup m j sub n (kr) dk 
c 
c       the r in front takes into account the 1/r in the 
c       do 300 of subroutine wvint 
c---------------------------------------------------------- 
        implicit double precision (a-h,o-z)
        real*4 rr,zz
        common/asym/j0k0,j0k1,j0k2,j0k3,j1k0,j1k1,j1k2,j1k3, 
     1  j2k0,j2k1,j2k2,j2k3 
        real*4 j0k0,j0k1,j0k2,j0k3,j1k0,j1k1,j1k2,j1k3,j2k0,j2k1, 
     1  j2k2,j2k3 
        r = dble(rr)
        z = dble(zz)
        dist=dsqrt(r*r + z*z) 
        dist3=dist**3 
        dist5=dist**5 
        dist7=dist**7 
        rz=r*z 
        z2=z*z 
        r2=r*r 
        r3=r*r2 
        z3=z*z2 
        rz2=r*z2 
        rz3=r*z3 
        zor = z/dist
        zor2= zor*zor
        zor3= zor*zor2
        j0k0 = sngl( r/dist   )
        j0k1 = sngl( rz/dist3   )
        j0k2 = sngl( (2.*rz2 - r3)/dist5   )
        j0k3 = sngl( (6.*rz3 - 9.*z*r3)/dist7   )
        j1k0 = sngl( 1. -z/dist   )
        j1k1 = sngl( r2/dist3   )
        j1k2 = sngl( 3.*z*r2/dist5   )
        j1k3 = sngl( 3.*r2*(4.*z2 - r2)/dist7   )
        j2k0 = sngl(  ( 1. -zor)*(1.-zor)*(dist/r)  )
        j2k1 = sngl(  (1-zor)*(1-zor)*(2.+zor)/r  )
        j2k2 = sngl( 3.*r3/dist5   )
        j2k3 = sngl( 15.*z*r3/dist7   )
        return 
        end 
     
      subroutine fmake(f,j,k,i,wvn,l,g,gg,gg1)
      dimension wvn(*)
      complex f(2),g(10,2)
      complex gg,gg1
      do 100 ii=1,2
          ll= i - 2 + ii
          f(ii) = g(j,ll)
          if(k.gt.0) f(ii) = f(ii) + g(k,ll)
          if(l.gt.0) f(ii) = f(ii)*wvn(ll)
  100 continue
      gg = f(2)
      gg1= f(1)
      return
      end
     
      subroutine getgk(g,j,jsrc,wvno)
c-----
c     read input to obtain elements of g(10,j) array
c-----
      complex g(10,2)
      dimension jsrc(10)
      call bufrd(wvno,ierr)
      do 101 i=1,10
            if(jsrc(i).eq.1)then
                  call bufrd(xr,ierr)
                  call bufrd(xi,ierr)
                  g(i,j)=cmplx(xr,xi)
            endif
  101 continue
      return
      end
     
      subroutine shift(g,wvn)
c-----
c     do interchange g(j,1) -> g(j,2)
c                 wvn(1)-> wvn(2)
c-----
      complex g(10,2)
      dimension wvn(*)
      wvn(2)=wvn(1)
      do 100 i=1,10
  100 g(i,2)=g(i,1)
      return
      end
     
      subroutine intini(smm,aa,bb,cc,iasymp,r)
        common/asym/j0k0,j0k1,j0k2,j0k3,j1k0,j1k1,j1k2,j1k3, 
     1  j2k0,j2k1,j2k2,j2k3 
      common/jout/jsrc(10) 
        real j0k0,j0k1,j0k2,j0k3,j1k0,j1k1,j1k2,j1k3,j2k0,j2k1, 
     1  j2k2,j2k3 
      complex aa(*),bb(*),cc(*),smm(*),sumd
      logical iasymp
      if(iasymp)then
c-----set up sum arrays, but put in asymptotic value now 
c-----of setting to zero and then resetting 
                smm(1)=        aa(1)*j0k0 + bb(1)*j0k1 + cc(1)*j0k2 
                smm(2)=        aa(2)*j1k1 + bb(2)*j1k2 + cc(2)*j1k3 
                smm(3)=        aa(3)*j1k0 + bb(3)*j1k1 + cc(3)*j1k2 
            if(jsrc(9).eq.1 .and. jsrc(4).eq.1)then
                sumd  = (aa(4)+aa(9))*j1k0 + (bb(4)+bb(9))*j1k1 + 
     1                  (cc(4)+cc(9))*j1k2 
            else
                  sumd = cmplx(0.0,0.0)
            endif
                sumd        = -sumd/r 
                smm(4)=sumd + aa(4)*j0k1 + bb(4)*j0k2 + cc(4)*j0k3 
                smm(5)=sumd + aa(9)*j0k1 + bb(9)*j0k2 + cc(9)*j0k3 
                smm(6)=  aa(5)*j2k0 + bb(5)*j2k1 + cc(5)*j2k2 
            if(jsrc(6).eq.1 .and. jsrc(10).eq.1)then
                sumd= (aa(6)+aa(10))*j2k0 + (bb(6)+bb(10))*j2k1 + 
     1               (cc(6)+cc(10))*j2k2 
            else
                  sumd = cmplx(0.0,0.0)
            endif
                sumd        = -2.*sumd/r 
                smm(7)=sumd + aa(6)*j1k1 + bb(6)*j1k2 + cc(6)*j1k3 
                smm(8)=sumd + aa(10)*j1k1+ bb(10)*j1k2+cc(10)*j1k3 
                smm(9)=aa(7)*j0k0        + bb(7)*j0k1 + cc(7)*j0k2 
                smm(10)=      aa(8)*j1k1 + bb(8)*j1k2 + cc(8)*j1k3 
      else
            do 100 i=1,10
  100       smm(i)=cmplx(0.0,0.0)
        endif 
      return
      end
     
      subroutine gasym(g,aa,bb,cc,wvn,i,depth,jsrc)
c-----
c     remove asymptotic trend from integrands
c-----
      complex g(10,2),aa(*),bb(*),cc(*)
      dimension jsrc(10)
      real wvn(*)
      wvno = wvn(i)
      ex = exp(-wvno*depth)
      do 130 j=1,10
            if(jsrc(j).eq.1)then
                  g(j,i)=g(j,i) - ex*(aa(j)+wvno*(bb(j)+
     1                        wvno*(cc(j))))
            endif
  130 continue
      return
      end
     
      subroutine bufini(irdwr,ierr)
c------initialize buffer pointer
c------irdwr = 0 read initialize
c------irdwr = 1 write initialize
      integer BUFMAX
       parameter(BUFMAX=2000)
       common/buf/iptr,max,buffer(BUFMAX)
       save /buf/
      iptr = 1
      if(irdwr.eq.0)call getbuf(ierr)
      return
      end
     
        subroutine buflsh
c------flush output buffer
      integer BUFMAX
      parameter(BUFMAX=2000)
        common/buf/iptr,max,buffer(BUFMAX)
      save /buf/
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
      save /buf/
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
      save /buf/
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
      save /buf/
c       only yank in new data if actually required
      if(iptr.gt.max)call getbuf(ierr)
      x = buffer(iptr)
      iptr = iptr + 1
      return
      end
      subroutine wvfilt(cmax,wvcm,wvc1,wvc2,wvcn,wvno,fact)
      pi = 3.1415927 
      if(cmax.lt.0.0)then
            fact = 1.0
      elseif(wvno.ge.wvc1.and.wvno.le.wvc2) then
            fact=1.0 
      elseif(wvno.ge.wvcm.and.wvno.lt.wvc1)then
            fact=(1.-cos(pi*(wvno-wvcm)/ (wvc1-wvcm)))/2. 
      elseif(wvno.gt.wvc2.and.wvno.le.wvcn)then
            fact=(1.-cos(pi*(wvno-wvcn)/ (wvc2-wvcn)))/2. 
      else
            fact = 0.0
      endif
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
