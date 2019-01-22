      program mech
c
c  may 99 by jh --------------- version 7.0 --------- 
c               add old files mchdep70, numarg
c  jan20, 2009 jh : fix for pc, main problem was format when using redirect
c  feb 20 2011 jh : remove fix for gfortrtan pc
c
c---------------------------------------------------------------------c
c                                                                     c
c      COMPUTER PROGRAMS IN SEISMOLOGY                                c
c      VOLUME V                                                       c
c                                                                     c
c      PROGRAM: MECH                                                  c
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
      parameter (LER=0, LIN=5, LOT=6)
c usage mech -d D -s S -l L -m M -x -a A -b B -v
c
c
c
      character*4 icom(3)
      dimension fz(3),fr(3),ft(3)
      dimension x(2048)
      dimension xr(4096),xt(4096),z(4096)
c-- computer type
      logical pc,sun,linux
      data icom/' Z  ',' N  ',' E  '/


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
c-----
c     call machine dependent initialization
c-----
      call mchdep()
      call gcmdln(dip,slip,strike,iexpl,iverby,xmom,az,baz,irot)
	if(irot.eq.1)then
		icom(2) = ' R  '
		icom(3) = ' T  '
		baz = 180.0
	endif
      if(iverby.eq.1)then
      write(LER,*)'dip =',dip,' slip= ',slip,' str= ',strike
      write(LER,*)'xmom= ',xmom,' az=',az,' baz=',baz,'exp=',iexpl
      endif
      str=strike
      slp=slip
      call trans(dip,str,slp,f1,f2,f3,v1,v2,v3)
      degrad = 0.01745329
      sint = sin(baz*degrad)
      cost = cos(baz*degrad)
      sina = sin(az*degrad)
      cosa = cos(az*degrad)
      sin2a = sin(2.*az*degrad)
      cos2a = cos(2.*az*degrad)
      fz(1) = f3 * v3
      fr(1) = fz(1)
      fz(2) = (f1*v3+f3*v1)*cosa + (f2*v3+f3*v2)*sina
      fr(2) = fz(2)
      fz(3) = (f1*v1-f2*v2)*cos2a + (f1*v2+f2*v1)*sin2a
      fr(3) = fz(3)
      ft(1) = 0.0
      ft(2) = (f1*v3+f3*v1)*sina - (f2*v3+f3*v2)*cosa
      ft(3) = (f1*v1-f2*v2)*sin2a - (f1*v2+f2*v1)*cos2a
c following the notation of Wang and Herrmann (1980)
      if(iverby.eq.1)then
            write(LER,*)' fz=',fz
            write(LER,*)' fr=',fr
            write(LER,*)' ft=',ft
      endif
 9995 continue
      read(LIN,10,end=9999,err=9999)r,yr,hs,nt,ti,dt,tau
   10 format(3e16.9,i10/3e16.9)
      
c      if(pc) then
c         write(LOT,13)r,yr,hs,nt,ti,dt,tau,dip,strike,slip,xmom,az,baz,
c     1            iexpl,icom
c   13    format(1x,3e16.9,i10,e16.9/1x,5e16.9/1x,3e16.9,i10,3a4)
c      else
         write(LOT,113)r,yr,hs,nt,ti,dt,tau,dip,strike,slip,xmom,az,baz,
     1            iexpl,icom
  113    format(3e16.9,i10,e16.9/5e16.9/3e16.9,i10,3a4)
c      endif

      do 1005 i=1,nt
      z(i)=0.0
      xr(i)=0.0
      xt(i)=0.0
 1005 continue
c read in the ten basic solutions

      do 1001 j=1,10
c initialize input array

      do 300 i=1,nt
  300 x(i)=0.0

c       write(17,*)j
       read(LIN,11)(x(i),i=1,nt)
 11    format(5e16.9)
c       write(17,*) j,(x(i),i=1,nt)

      do 1002 i=1,nt
        x(i)=x(i)*xmom
 1002 continue

      do 9200 i=1,nt
      if(iexpl.le.0)then
            if(j.eq.1)then
                  z(i)=z(i) + fz(1)*x(i)
            elseif(j.eq.2)then
                  xr(i)=xr(i) + fr(1)*x(i)
            elseif(j.eq.3)then
                  z(i)=z(i) + fz(2)*x(i)
            elseif(j.eq.4)then
                  xr(i)=xr(i) + fr(2)*x(i)
            elseif(j.eq.5)then
                  xt(i)=xt(i) + ft(2)*x(i)
            elseif(j.eq.6)then
                  z(i)=z(i) + fz(3)*x(i)
            elseif(j.eq.7)then
                  xr(i)=xr(i) + fr(3)*x(i)
            elseif(j.eq.8)then
                  xt(i)=xt(i) + ft(3)*x(i)
            endif
      elseif(iexpl.eq.1)then
            if(j.eq.9)then
                  z(i)= x(i)
            elseif(j.eq.10)then
                  xr(i)=x(i)
            endif
      endif
 9200  continue
 1001 continue

c       write(17,*)' 1001 loop finished'

c rotate horizontal components into receiver coordinates N and E
	if(irot.eq.0)then
      do 9210 i = 1,nt
      yr = xr(i)
      yt = xt(i)
      xr(i) = - cost*yr + sint*yt
      xt(i) = -sint*yr - cost*yt
 9210 continue
	endif

c      if(pc) then
c         write(LOT,119)(z(i),i=1,nt)
c         write(LOT,119)(xr(i),i=1,nt)
c         write(LOT,119)(xt(i),i=1,nt)
c 119     format(1x,5e16.9)  !jh
c      else
         write(LOT,11)(z(i),i=1,nt)
         write(LOT,11)(xr(i),i=1,nt)
         write(LOT,11)(xt(i),i=1,nt)
c      endif


      go to 9995    
 9999 continue
      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine trans(dip,stk,slip,f1,f2,f3,v1,v2,v3)
      degrad=0.01745329
      sins=sin(stk*degrad)
      coss=cos(stk*degrad)
      sind=sin(dip*degrad)
      cosd=cos(dip*degrad)
      sinf=sin(slip*degrad)
      cosf=cos(slip*degrad)
      a11=cosf*coss + sinf*cosd*sins
      a12=cosf*sins - sinf*cosd*coss
      a13= - sinf*sind
      a21= -sins*sind
      a22= coss*sind
      a23= - cosd
      f1=a11
      f2=a12
      f3=a13
      v1=a21
      v2=a22
      v3=a23
      return
      end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine gcmdln(dip,slip,strike,iexpl,iverby,xmom,
     1            az,baz,irot)
      parameter (LER=0, LIN=5, LOT=6)
c-----
c     parse command line arguments
c
c     requires subroutine getarg() and function numarg()
c
c-----
      character*25 name
      integer*4 numarg
      dip = 0.0
      slip = 0.0
      strike = 0.0
      iexpl=0
      iverby=0
      xmom=1.0
      az=0.0
      baz=0.0
	irot = 0.0
      nmarg=numarg()
      if(nmarg.le.0)then
            write(LER,*)'mech -d D -s S -l L -m M -x -a A -b B -v -r'
            stop
      endif
      i = 0
   11 i = i + 1
      if(i.gt.nmarg)go to 13
            call getarg(i,name)
            if(name(1:2).eq.'-d')then
                  i=i+1
                  call getarg(i,name)
                  read(name,'(f10.0)')dip
            else if(name(1:2).eq.'-s')then
                  i=i+1
                  call getarg(i,name)
                  read(name,'(f10.0)')strike
            else if(name(1:2).eq.'-l')then
                  i=i+1
                  call getarg(i,name)
                  read(name,'(f10.0)')slip
            else if(name(1:2).eq.'-m')then
                  i=i+1
                  call getarg(i,name)
                  read(name,'(f10.0)')xmom
            elseif(name(1:2).eq.'-a')then
                  i=i+1
                  call getarg(i,name)
                  read(name,'(f10.0)')az
            elseif(name(1:2).eq.'-b')then
                  i=i+1
                  call getarg(i,name)
                  read(name,'(f10.0)')baz
            else if(name(1:2).eq.'-x')then
                  iexpl = 1
            else if(name(1:2).eq.'-v')then
                  iverby = 1
		else if(name(1:2).eq.'-r')then
			irot = 1
            endif
      goto 11
   13 continue
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

	function numarg()
	integer numarg
	numarg = iargc()
	return
	end

