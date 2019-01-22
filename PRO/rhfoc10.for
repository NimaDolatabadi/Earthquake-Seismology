        program rhfc10
c
c  may  99 by jh --------- verison 7.0 check --------------------
c                include mchdep70, numarg
c  jan 20 2009 jh: fix for pc
c  feb 20 2011 jh: remove fix, not needed with gfortran
c---------------------------------------------------------------------c
c                                                                     c
c      COMPUTER PROGRAMS IN SEISMOLOGY                                c
c      VOLUME VI                                                      c
c                                                                     c
c      PROGRAM: RHFOC10                                               c
c                                                                     c
c      COPYRIGHT 1985, 1992                                           c
c      R. B. Herrmann                                                 c
c      Department of Earth and Atmospheric Sciences                   c
c      Saint Louis University                                         c
c      221 North Grand Boulevard                                      c
c      St. Louis, Missouri 63103                                      c
c      U. S. A.                                                       c
c                                                                     c
c---------------------------------------------------------------------c
      parameter (LER=0, LIN=5, LOT=6)
c-----
c
c     rhfoc10 -f FILE [ -v  ] [ -t -o -p -i ] -a ALP -l L [ -D -V -A ]
c
c       This convolves output of rhwinvta with the
c       far-field displacement
c       pulses to yield velocity time
c       histories
c     The symmetric parabolic pulse ( -p )
c     of Wang and Herrmann (1980) is used.
c     A simple triangular pulse ( -t ) is also available.
c     as is the Ohnaka source time function ( -o )
c     The total duration of the pulse is 4L dt for the parabolic pulse
c     and 2L dt for the triangular pulse.
c
c     L is the number of dt's per tau, e.g.
c           tau = L dt . If -n is not specified,
c           then tau = dt by default
c     
c     ALP is the constant in the ohnaka time function whose
c     far field displacement is alpha*alpha*t*exp(-alpha*t)
c           and whose Fourier  spectrum is
c           [ ALP / ( j2pif + ALP ] ** 2
c           This has a corner frequency of fc=ALP/2pi
c
c     Other pulses might be able to be used. But this
c     one is stable. Note that for tau = dt,
c     -v indicates verby output on device LER
c
c     -i = impulse source
c     -D = output displacement time history
c     -V = output velocity time history (default)
c     -A = output acceleration time history
c
c     The pulses are such that the far-field dispacements would
c     have unit area
c
c     The pulses used correspond to the dislocation
c               far-field displacement to get velocity history
c
c
c-----
      parameter(NL=100)
      common/jout/jsrc(10),lsrc(10) 
      common/source/z(2048)
      common/model/d(NL),a(NL),b(NL),rho(NL),mmax,qa(NL),qb(NL) 
      complex data(4096),datas(4097)
      dimension ar(10),ai(10)
      character*50 names , rfile
      character*4 icchar (11)
      integer idva
c-- computer type
      logical pc,sun,linux
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
c
c   get computertype
c    
      call computer_type(sun,pc,linux)
c     
c-----
c     call machine dependent initialization
c-----
      call mchdep()
      call gcmdln(ntau,ipt,iverby,alp,ifile,names,idva,xmult,rfile)
        npuls=1 
      open(unit=3,file=names,access='sequential',form='unformatted' 
     1 ,status='old') 
      open(unit=4,status='scratch',form='unformatted')
      rewind 3
      rewind 4
   19 format(5f10.5) 
      do 99 i=1,n 
   99 z(i)=0.0 
      read(3) alpha,depth,fl,fu,dt,n,n1,n2,df,nyq2 
      read(3)jsrc 
      read(3)lsrc 
      read(3)d,a,b,rho,mmax,qa,qb 
      nyq=nyq2/2 
c default for filter
	if(ntau.le.0)then
		tau = dt
	else
		tau = ntau * dt
	endif
	if(ipt.eq.0)then
		call pultd(tau,dt,n,l)
	elseif(ipt.eq.1)then
		call pulpd(tau,dt,n,l)
	elseif(ipt.eq.2)then
		call pulod(alp,dt,n,l)
	elseif(ipt.eq.3)then
		call puldd(dt,n,l)
	else if(ipt.eq.4)then
		call pulud(rfile,n,tau)
	endif
	if(iverby.eq.1)write(LER,*)' tau=',tau,' dt=',dt
	if(iverby.eq.1)write(LER,*)' ipt=',ipt,' l=',ntau
	fmax = fu 
	inst = 0 
c-----
c	plot source pulse and spectra 
c	since omega is complex time series has to be damped 
c-----
      fac = 1.0 
      dfac = exp(-alpha*dt) 
      do 200 i = 1,n 
      data(i) = cmplx(fac*z(i)*xmult,0.0)
      fac = fac * dfac 
  200 continue
      call four(data,n,-1,dt,df) 
      do 310 i = 1,nyq 
      freq=(i-1)*df 
      if(freq.gt.fmax) go to 309 
      if(i.ge.n1.and.i.le.n2) go to 310 
  309 continue 
      data(i)=cmplx(0.0,0.0)
  310 continue 
      do 206 i = 1,nyq 
  206 datas(i) = data(i) 
c-----
c    beginning of distance loop
c-----
 9997 continue 
      read(3,err=9999,end=9999)r,t0
      if(r.lt.0.0) go to 9999 
c-----
c    set up output stream
c-----
      yr = 0.0
c      if(pc) then
c         write(LOT,1110)r,yr,depth,n,t0,dt,tau
c 1110    format(1x,3e16.9,i10/1x,3e16.9)            ! jh
c      else
         write(LOT,110)r,yr,depth,n,t0,dt,tau
  110    format(3e16.9,i10/3e16.9)            ! jh
c      endif
      do 249 jj=1,10
  249 read(3,err=9999,end=9999)icchar(jj)
      rewind 4
      do 300 i=n1,n2
        do 305 j=1,10
        ar(j)=0.0
        ai(j)=0.0
        if(jsrc(lsrc(j)).eq.1)read(3,err=9999,end=9999)ar(j),ai(j)
  305   continue
        write(4)ar,ai
  300 continue
c-----
c     Now that all spectra for a given distance are in
c     make time history and output in desired order
c-----
      do 1300 jk=1,10
      do 1301 i=1,n
         data(i)=cmplx(0.0,0.0)
 1301 continue
      rewind 4
      do 1302 i=n1,n2
      read(4)ar,ai
      data(i)=cmplx(ar(jk),ai(jk)) * datas(i)
c-----
c	if integrate
c-----
	if(idva.eq.0)then
		omega = 6.2831853*(i-1)*df	
		data(i) = data(i) / cmplx(alpha,omega)
	elseif(idva.eq.2)then
		omega = 6.2831853*(i-1)*df
		data(i) = data(i) * cmplx(alpha,omega)
	endif
c-----
c    make a real time series
c-----
      if(i.ne.1)data(n+2-i)=conjg(data(i))
 1302 continue
      data(nyq)=cmplx(real(data(nyq)),0.0)
      call four(data,n,+1,dt,df)
c-----
c    output the time series
c-----
c     correct for damping 
      fac = exp(alpha*t0) 
      dfac = exp(alpha*dt) 
      do 425 i = 1,n 
      data(i)= data(i) * fac
      fac = fac * dfac 
  425 continue
 
c      if(pc) then
c         write(LOT,1111)(real(data(i)),i=1,n)
c 1111    format(1x,5e16.9)
c      else
         write(LOT,111)(real(data(i)),i=1,n)
  111    format(5e16.9)
c      endif

 1300 continue
 9000 continue 
      go to 9997 
 9999 continue 
      close(3) 
      close(4)
      stop 
      end 

      subroutine four(data,nn,isign,dt,df) 
      dimension data(*) 
      n = 2 * nn 
      if(dt.eq.0.0) dt = 1./(nn*df) 
      if(df.eq.0.0) df = 1./(nn*dt) 
      if(dt.ne.(nn*df)) df = 1./(nn*dt) 
      j = 1 
      do 5 i=1,n,2 
      if(i-j)1,2,2 
    1 tempr = data(j) 
      tempi = data(j+1) 
      data(j) = data(i) 
      data(j+1)=data(i+1) 
      data(i) = tempr 
      data(i+1) = tempi 
    2 m = n/2 
    3 if(j-m) 5,5,4 
    4 j = j-m 
      m = m/2 
      if(m-2)5,3,3 
    5 j=j+m 
      mmax = 2 
    6 if(mmax-n) 7,10,10 
    7 istep= 2 *mmax 
      theta = 6.283185307/float(isign*mmax) 
      sinth=sin(theta/2.) 
      wstpr=-2.*sinth*sinth 
      wstpi=sin(theta) 
      wr=1.0 
      wi=0.0 
      do 9 m=1,mmax,2 
      do 8 i=m,n,istep 
      j=i+mmax 
      tempr=wr*data(j)-wi*data(j+1) 
      tempi=wr*data(j+1)+wi*data(j) 
      data(j)=data(i)-tempr 
      data(j+1)=data(i+1)-tempi 
      data(i)=data(i)+tempr 
    8 data(i+1) = data(i+1)+tempi 
      tempr = wr 
      wr = wr*wstpr-wi*wstpi + wr 
    9 wi = wi*wstpr+tempr*wstpi + wi 
      mmax = istep 
      go to 6 
   10 continue 
      if(isign.lt.0) go to 1002 
c     frequency to time domain 
      do 1001 iiii = 1,n 
 1001 data(iiii) = data(iiii) * df 
      return 
 1002 continue 
c     time to frequency domain 
      do 1003 iiii = 1,n 
 1003 data(iiii) = data(iiii) * dt 
      return 
      end 

      subroutine pulpd(tau,dt,nt,l)
c     far field displacement
      common /source/ f(2048)
      ltest = 0
      tl = tau
      t1 = 0.01*dt
      t2 = t1 + tau
      t3 = t2 + tau
      t4 = t3 + tau
      t5 = t4 + tau
      do 100 i = 1,nt
      y=(i-1)*dt
      z = y - t1
      f(i) = 0.0
      if(y.gt.t1)go to 101
  101 if(y.gt.t2) go to 102
      f(i) = 0.5*(z/tl)**2
      go to 100
  102 if(y.gt.t3) go to 103
      f(i)= -0.5*(z/tl)**2 + 2.*(z/tl) - 1
      go to 100
  103 if(y.gt.t4) go to 104
      f(i)= -0.5*(z/tl)**2 + 2.*(z/tl) - 1.
      go to 100
  104 if(y.gt.t5) go to 105
      f(i)= 0.5*(z/tl)**2 - 4.*(z/tl) + 8.
      go to 100
  105 continue
      ltest = ltest + 1
      if(ltest.eq.1) l = i
  100 continue
c     pulse normalized so first integral has area of unity
      do 200 i = 1,nt
  200 f(i) = f(i)/(2.*tl)
      return
      end

      subroutine pulod(alp,dtt,nt,l)
c     far field displacement
c     for the ohnaka slip history
c     Harkrider (1976) Geophys J. 47, p 97.
      common/source/v(2048)
      ltest = 0
      do 100 i=1,nt
      t=(i-1)*dtt
      v(i)=0.0
      arg= alp*t
      al2=alp*alp
      v(i)=0.0
      if(arg.gt.25.)goto 101
        v(i)= al2*t*exp(-arg)
      goto 100
  101 continue
      ltest = ltest +1
      if(ltest.eq.1)l = i
  100 continue
      return
      end

      subroutine pultd(tau,dtt,nt,l)
c waveform is far-field displacement
c triangular pulse
c this is really a general trapezodal pulse, but only
c the symmetric triangular pulse is invoked
      common/source/d(2048)
      ltest = 0
      dt1=tau
      dt2=0.0
      dt3=tau
      t1=dt1
      t2=t1+dt2
      t3=t2+dt3
      fac = 0.5*dt1 + dt2 + 0.5*dt3
      fac = 1./fac
      v1 = fac/dt1
      v2 = 0.0
      v3 = - fac/dt3
      do 100 i=1,nt
      t = (i-1)*dtt
      d(i)=0.0
      if(t.le.t1)then
            dt = t - 0.0
            d(i) = dt*fac/dt1
      elseif(t.gt.t1.and.t.le.t2)then
            dt = t - t1
            d(i) = fac
      elseif(t.gt.t2.and.t.le.t3)then
            dt = t - t2
            d(i)= fac + v3*dt
      elseif(t.gt.t3)then
            d(i)=0.0
            ltest = ltest + 1
            if(ltest.eq.1)l = i
      endif
  100  continue
      return
      end

      subroutine puldd(dt,n,l)
      common/source/d(2048)
c-----
c	Dirac Delta Pulse
c-----
	do 100 i=1,n
		d(i) = 0.0
  100	continue
	d(1) = 1.0/dt
	return
	end

	subroutine pulud(rfile,n,tau)
	character rfile*(*)
	common/source/d(2048)
	do 100 i=1,n
		d(i) = 0.0
  100	continue
	open(1,file=rfile,status='unknown',form='formatted',
     1		access='sequential')
	rewind 1
	read(1,'(i5,f10.3)')np,dtt
	read(1,10)(d(i),i=1,np)
   10	format(4e15.7)
	close(1)
	tau = np*dtt
	return
	end

	subroutine gcmdln(ntau,ipt,iverby,alp,ifile,names,idva,
     1		xmult,rfile)
c-----
c     parse command line arguments
c     requires subroutine getarg() and function numarg()
c-----
	character*(*) names
	integer*4 numarg
	character*50 name, rfile
	integer idva 
	ntau = -1
	ipt = -1
	iverby = 0
	alp = -1.0
        ifile = -1
	idva = 1
	rfile = ' '
	xmult = 1.0
	nmarg=numarg()
   11	i = i + 1
		if(i.gt.nmarg)goto 13
		call getarg(i,name)
		if(name(1:2).eq.'-l')then
			i=i+1
			call getarg(i,name)
			read(name,'(i10)')ntau
		elseif(name(1:2).eq.'-f')then
			ifile=1
			i=i+1
			call getarg(i,names)
		else if(name(1:2).eq.'-a')then
			i=i+1
			call getarg(i,name)
			read(name,'(f10.0)')alp
		else if(name(1:2).eq.'-v')then
			iverby=1
		else if(name(1:2).eq.'-t')then
			ipt = 0
		else if(name(1:2).eq.'-p')then
			ipt = 1
		else if(name(1:2).eq.'-o')then
			ipt = 2
		else if(name(1:2).eq.'-i')then
			ipt = 3
		else if(name(1:2).eq.'-D')then
			idva = 0
		else if(name(1:2).eq.'-V')then
			idva = 1
		else if(name(1:2).eq.'-A')then
			idva = 2
		else if(name(1:2).eq.'-F')then
			ipt = 4
                        i=i+1
                        call getarg(i,rfile)
		else if(name(1:2).eq.'-m')then
			i=i+1
			call getarg(i,name)
			read(name,'(f20.0)')xmult
		else if(name(1:2).eq.'-?')then
			call usage('Help')
		endif
	goto 11
   13	continue
c-----
c     do some error checking, e.g., we cannot generate velocity
c     for triangular pulse
c-----
        if(ifile.lt.0)call usage('No binary data file')
	if(ipt.eq.2 .and. alp.lt.0.0)
     1		call usage('No alpha for Ohnaka pulse')
	if(ipt.lt.0)
     1		call usage('No pulse shape defined')
	return
	end

	subroutine usage(str)
	parameter (LER=0, LIN=5, LOT=6)
	character str*(*)
	write(LER,*)'rhfoc10:',str
        write(LER,*)'USAGE: ',
     1	'rhfoc10 -f FILE [ -v  ] [ -t -o -p -i ] -a alpha',
     2	' -l L [ -D -V -A]  [-F rfile ] [ -m mult]'
	write(LER,*)
     1	' -f FILE      Name of binary omega-r file'
	write(LER,*)
     1	' -v           Verbose output'
	write(LER,*)
     1	' -t           Triangular pulse of base 2L dt'
	write(LER,*)
     1	' -p           Parabolic Pulse of base  4L dt'
	write(LER,*)
     1	' -o           Ohnaka pulse with parameter alpha'
	write(LER,*)
     1	' -i           Impulse (this will some ripple'
	write(LER,*)
     1	' -a alpha     Shape parameter for Ohnaka pulse'
	write(LER,*)
     1	' -D           Output is ground displacment'
	write(LER,*)
     1	' -V           Output is ground velocity (default)'
	write(LER,*)
     1	' -A           Output is ground acceleration'
	write(LER,*)
     1	' -F rfile     User supplied pulse'
	write(LER,*)
     1	' -m mult      Multiplier (default 1.0)'
	write(LER,*)
     1	' -?           Write this help message'
        
	stop
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

	function numarg()
	integer numarg
	numarg = iargc()
	return
	end
