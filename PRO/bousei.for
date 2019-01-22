C  c-----------------------------------------------------------------------
c
c  original Bouchon program has been modified in output, will now use
c  SEISAN files to make a SEISAN binary output file with both the original
c  and synthetic data, lined up using the residuals in the hyp.out file
c  Nothing in the working of the Bouchon program has been changed,
c  only graphics cut out
c
c  J Havskov, February 1993
c
c  updates:
c  apr 13 93 by jh: bug in syntsel
c  jul 16 93      : interactive channel delay
c  aug 30         : seisinc instead fo seisin to save memory for pc
c  dec 93         : more arguments to seisinc,
c                   read from hyp.out instead of synt.inp, bug in pc
c  jan 94         ; fix moment, new valerie version
c  apr 95         : only use one triangular source
c  nov 95         : use seidim
c  may 99 jh      : -----------------   version 7.0 check -----------------
c                   stat to 5 char
c  jan 12, 09 jh  : read with indata, syntsel fixed for delays and new wav,
c                   increased nft from 513 to 2049 
c
c
c        implicit none, tied, too many to define !! jh
        include 'seidim.inc'
        parameter (nfq=2049,nst=32)
        DIMENSION  utdat(3,2*nfq)
C                  utdat(j,i)  collects the output data, to be written out later
C                              is real, with three components,
c
      character*80 data(max_data)
      integer nstat,ndata
      character*5 stat(nst)
      dimension u(nfq,nst,3),yy(2*nfq),source(nfq),y(4*nfq)
      dimension r(nst)
c   dimension liâe å la source complexe
      dimension dtt(20),tau(20),ah(20)
      complex *16 q1,u,source,omega,ai,q2,q3,ait0
      real t0synt(max_trace)
      real*4 maxgen
c---- more parameters for s-file
      integer nphase,nhead,nrecord,id
      character*1 type,ex
c-- code for component orientation
      logical radial
c
c   integer not declared before implicit none
c
c      integer nfq,nst,i,k,nt,nr,nfreq1
c
c   reals not declared before implicit none
c
c      real utdat,u,yy,y,r,pi2,t00dat,window,t0,redvel,tl,q


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c  pgm de calcul du sismogramme synthetique produit par la mâthode de Bouchon
c  des sources triangulaires peuvent etre introduites
c  le moment sismique pris en compte est de 10**15 Newton-metres
c  les entrees sont en km et km/s et la masse volumique en tonne/m**3
c  sorties en metres
c
      ai=(0.,1.)
      pi=asin(1.)*2.
      pi2=pi+pi
      maxgen=0.
c
c   open files
c
c   output from Bouchon synthetic program, bouch.for
c		
      open(10,form='formatted',file='bouch.out',status='unknown')
c
c   a scratch file where the synthetic time signal are written out
c   temporarely before being merged with seisan binary file
c
      open(20,status='scratch')
c      open(20,file='bousei.txt',status='unknown')
c
c  standard hyp.out file from Seisan
c
      open(1 ,file='hyp.out',status='unknown')
c
c   read data from pick file
c
      call indata
     *(1,nstat,nphase,nhead,ndata,type,ex,data,id)
      close(1)
c
c   find stations modelled
c
      nstat=0
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: STATION:')
     *   then
            do k=1,nstat
              if(stat(k).eq.data(i)(17:21)) goto 81
            enddo
c
c   station was not counted before
c
            nstat=nstat+1
            stat(nstat)=data(i)(17:21)
 81         continue
         endif
      enddo
      write(6,*)'Number of stations', nstat
c
c   read if Radial-Transverse or North-East components
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: COMPON-:') then
            radial=.false.
            if  (data(i)(20:25).eq.'RADIAL') radial=.true.
         endif
      enddo

c   half-duration of the source
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: DT-Tsou:') then
            read(data(i),'(25x,f10.3)') t00dat
         endif
      enddo

c   total time window, initial time
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: TIMES--:') then
            read(data(i),'(15x,3(10x,f10.1))') window,t0
         endif
      enddo

c   reduction velocity
c
      redvel=0.
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: REDVELO:') then
            read(data(i),'(15x,3f10.1)') redvel
         endif
      enddo

c      write(6,*)'sfile read'
c
c   read from bouch.out file
c
      read(10,*) nt,tl,q,nr
      write(*,*)' nt,tl,nr',nt,tl,nr
      nfreq=nt/2
      nfreq1=nfreq+1
      if(nfreq1.gt.nfq.or.nr.gt.nst) then
          write(*,*)' error dimentions nfq ns - max: ',nfq,nst
          stop
      endif
      read(10,*)(r(ir),ir=1,nr)
c
c   calculate origin times at different epicentral distances
c
c     t0=0.
      if (redvel.eq.0.)  then
        do ir=1,nr
          t0synt(ir)=t0
        enddo
      else  
        epimin=r(1)
        do ir =2,nr   
          epimin=amin1(epimin,r(ir))
        enddo
        do ir =1,nr   
          t0synt(ir)=t0 +(r(ir)-epimin)/redvel
        enddo
      endif
c
      dt=tl/nt
      aw=-pi/q
      write(*,*) 'displacement=1, velocity=2, acceleration=3'
      read(*,*) ind
c ====lecture fichier donnees======================
      bidm=0
      do 55 if=1,nfreq1
        read(10,2500)((u(if,ir,j),j=1,3),ir=1,nr)
        read(10,*) bidon
2500    format(10e12.5)
        if(bidon.gt.bidm) bidm=bidon
 55   continue
c      write(*,*)' m max et bidon der:',bidm,bidon !jh
c ============================ ====================
      dfreq=1./tl
c===================================================
c     description de la source
c       write(*,*)' Bouchon source model (1)'
c       read(*,*) ibou
        am0=10000.0
        ibou=2
        if(ibou.eq.1) then
c            write(*,*) 'Enter rise time '
c            read(*,*) t00
             t00=2.*t00dat
c             write(*,*) 'Enter moment '
c             read(*,*) am0
      else
c  sources triangulaires
c            write(*,*)' Number of sources,if one use default'
c            read(*,*) nbs
             nbs=1
             surf=0.
             if(nbs.eq.1) then
               dtt(1)=0.0
               tau(1)=t00dat
               ah(1)=1.0
               surf=surf+tau(1)*ah(1)
             else
                do 800 i=1,nbs
c  decalage milieu base triangle par rapport tps origine
                    write(*,*)' Delay, half-length and hight'
                    read(*,*) dtt(i),tau(i),ah(i)
                    surf=surf+tau(i)*ah(i)
cvalerie apr 95                if(dtt(i).le.0.) dtt(i)=tau(i)
800             continue
             endif
c    calcul du moment sismique total
             surf=1./surf
        endif
        write(6,*)' Source half duration ',tau(1) 
c
c-------------------------------------------------------------------------
c--------------loop over stations
c-------------------------------------------------------------------------
c
      do 56 ir=1,nr
c
c   skip the initialisation of source and other functions if 
c             origin time identical at the different stations
c
      if(redvel.eq.0..and.ir.ne.1) goto 59
c
c   calcul de la source
c
         ait0=ai*t0synt(ir)
         freq=0.
         do 1 if=1,nfreq1
              rw=pi2*freq
              omega=cmplx(rw,aw)
              if(ibou.eq.1) then
                    if(t00.lt.1.e-5) then
                        q3=ai/omega
                    else
                        q1=omega*pi/4.
                        q2=q1*t00
                        q2=cdexp(q2)
                        q3=ai*t00*pi/2.
                        q3=q3/(q2-1./q2)
                    endif
                    q2=omega*ait0
                    source(if)=q3*cdexp(q2)
              else
                    source(if)=0.
c    bouch1= calcul fonction de Green pour deplacement
                    do 850 iy=1,nbs
                         q2=omega*ai*(t0synt(ir)-dtt(iy))
                         q1=omega*tau(iy)/2.
c   integration du signal triangulaire derivee du moment sismique
                         source(if)=source(if)-ah(iy)*tau(iy)*
     *                   (cdsin(q1)/q1)**2*cdexp(q2)/omega/ai
850                  continue
                     source(if)=source(if)*surf
              endif
              if(ind.eq.2) source(if)=source(if)*(ai*omega)
              if(ind.eq.3) source(if)=-source(if)*omega**2
1        freq=freq+dfreq
         n3=2*nt+3
         tex1=exp(-aw*dt)
         ex7=exp(-aw*t0synt(ir))
c        ex7=1.
         do 140 i=1,nt
            yy(i)=ex7
140      ex7=ex7*tex1
 59   continue
c---------------------------------------------------
c   loop over 3 channels
c--------------------------------------------------
c
c   set counter for interchanging channels, factor for polarity on z
c   since that is reversed
c
      ichan=1
      pol=1.
c
      do 516 j=1,3
      ichan=ichan+1
      if(ichan.eq.4) then
        ichan=1
        pol=-1.
      endif
c
c  spectre source et propagation
c
      do 518 i=1,nfreq+1
         q1=u(i,ir,j)*source(i)
         i2=i+i
         i1=i2-1
         y(i1)=real(q1)
c first for pc, seocnd for sun
c        y(i2)=aimag(q1)
         y(i2)=dimag(q1)
         if(i1.eq.nt+1) go to 518
         y(n3-i1)=-y(i2)
         y(n3-i2)=y(i1)
518   continue
      call four1(y,nt,1)
      do 521 i=1,nt
         y(i)=y(i+i-1)*yy(i)
521   continue
c
c   save data
c
         do 522 i=1,nt
           utdat(ichan,i) = y(i)*pol*100000.*am0
522      continue
c------------------------------------------------------------end comp loop
516   continue
c------------------------------------------------------------end comp loop
c
c   write data in output file, identify stations and channels
c
        write(20,'(a5,a4)')stat(ir),'   Z'
        write(20,*)(utdat(1,i),i=1,nt)
        if (radial) write(20,'(a5,a4)')stat(ir),'   R'
        if (.not.radial) write(20,'(a5,a4)')stat(ir),'   N'
        write(20,*)(utdat(2,i),i=1,nt)
        if (radial) write(20,'(a5,a4)')stat(ir),'   T'
        if (.not.radial) write(20,'(a5,a4)')stat(ir),'   E'
        write(20,*)(utdat(3,i),i=1,nt)
56      continue
c------------------------------------------------------------end station loop
c
c   integrate synthetic and real data in a seisan file
c
c       tsamp=tl/(nt-1)
c       write (*,*) 'syntsel',(t0synt(ir),ir=1,nr)
c        write(*,*)'syn',ndata,t0,window,t0synt,dt,nt
        call syntsel(data,ndata,nhead,t0,window,t0synt,dt   ,nt,'SB ')

80    stop
      end
c***************************************************************************
      subroutine four1(data,nn,isign)
      real*8 wr,wi,wpr,wpi,wtemp,theta
      dimension data(1)
      n=2*nn
      j=1
      do 11 i=1,n,2
      if(j.gt.i) then
      tempr=data(j)
      tempi=data(j+1)
      data(j)=data(i)
      data(j+1)=data(i+1)
      data(i)=tempr
      data(i+1)=tempi
      endif
      m=n/2
1     if((m.ge.2).and.(j.gt.m)) then
      j=j-m
      m=m/2
      go to 1
      endif
      j=j+m
11    continue
      mmax=2
2     if(n.gt.mmax) then
      istep=2*mmax
      theta=6.28318530717959d0
      theta=theta/(isign*mmax)
      wpi=dsin(theta)
      wpr=-2.0d0*dsin(0.5d0*theta)**2
      wr=1.d0
      wi=0.d0
      do 13 m=1,mmax,2
      do 12 i=m,n,istep
      j=i+mmax
      tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
      tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
      data(j)=data(i)-tempr
      data(j+1)=data(i+1)-tempi
      data(i)=data(i)+tempr
      data(i+1)=data(i+1)+tempi
12    continue
      wtemp=wr
      wr=wr*wpr-wi*wpi+wr
      wi=wi*wpr+wtemp*wpi+wi
13    continue
      mmax=istep
      go to 2
      endif
      return
      end
