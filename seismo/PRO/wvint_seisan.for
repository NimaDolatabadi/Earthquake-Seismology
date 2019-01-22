c
c    generates the final greens ascii function files for inversion
c    the input is binary file from fkrprog
c    currently writes out in helmberger format, readable by seisan
c
c    jh dec 2011
c
c    has been tested with fbounds check
c
c  this program is based on wvint9. option for multiple cpu removed
c 
c
c old comment:
c last modified oct 11 1999 to change ivel field from 1 to 100
c 2014-04-14 pv: cleanup Warning: Obsolete: arithmetic IF statement
c
      real alpha,depth,fl,fu,dt
      integer n1,n2
      real df
      integer nyq,nrange,nskip
      integer lin,lot,nl,nder

      integer year,month,day,hour,min
      real sec

      parameter (lin=5,lot=6,nl=70,nder=1)
      character*80 rep*1,ivel*100
      character*80 outfile       ! ascii files with greens functions
      character*80 sfile
      character*80 text 
      common/source/z(8192)
      common/aup/gg(100,4097,10)
      common/model/d(nl),a(nl),b(nl),rho(nl),mmax,qa(nl),qb(nl)
      complex data(8192),gg
      real y(8192)    ! time domain trace
      dimension isrc(10),omega(4097)
      dimension range(100),vred(100),t0(100)
      character*5 stat(100)     ! station codes
      character*4 comp(8)       ! components
      integer order(8)		! order of channels when writing out
      real*8 aa,bb
      real temp(4097),totint,prtint
      integer seiclen
c
c  for reading sfile
c
      integer nstat1,nphase1,nhead1,nrecord1,id
      character*1 evid1,exp1
      character*80 data1(2000)

      character*12 mt_dispvel ! velocity or acceleration

      real gcomp(10,4097)   ! components of greens function, time domain
      integer i,j,l,m,k 
c
      open(12,file='green.out',status='old',access='sequential',
     $             form='unformatted',err=33)

      goto 34
 33   continue
      write(6,*)' No green.out file'
      stop
 34   continue
      read(12) sfile
c
c  get time and date
c
           
c
      read(12) alpha,depth,fl,fu,dt,n1,n2,df,nyq,nrange,nskip
      read(12) isrc
      read(12) d,a,b,rho,mmax,qa,qb
      read(12) stat,range,vred,t0         ! stat is a seisan addition
c
c  set order of output channels and components
c

      order(1)=8
      comp(1)='TS S'
      order(2)=5
      comp(2)='TD S'
      order(3)=7
      comp(3)='XS S'
      order(4)=4
      comp(4)='XD S'
      order(5)=2
      comp(5)='XD D'
      order(6)=6
      comp(6)='ZS S'
      order(7)=3
      comp(7)='ZD S'
      order(8)=1
      comp(8)='ZD D'
c
c    read disp or vel from s-file
c
      open(77,file=sfile,status='old',err=333)
      goto 334
 333  continue
      write(6,'(a,a)')' S-file not found: ',sfile
      stop
 334  continue
c
c   get parameters from s-file
c

      mt_dispvel=' '
      call indata
     *(77,nstat1,nphase1,nhead1,nrecord1,evid1,exp1,data1,id) 
      do i=1,nhead1
c
c origin time
c
      read(data1(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)') 
     *year,month,day,hour,min,sec

         if(data1(i)(2:15).eq.'SYNT: MT-D-V-:') then
            read(data1(i)(17:28),'(a12)') mt_dispvel
         endif
      enddo
      write(6,'(a,a)') ' Event: ',sfile(1:seiclen(sfile))
      
      close(77)

      write(6,'(a,1x,a)') ' Generating greens time domain traces in',
     *mt_dispvel
c
c   file for all greens functions
c
      open(13,file='all.green',status='unknown')
c
c   write main header, 
c
         write(13,'(i8,15x,5x,a)')nrange*8,
     *   sfile ! 
         write(13,'(a,a)') '(6e12.5) ',mt_dispvel            
      
      nm=n2

c      rep='n'
c      ivel='d'
c
c      print*,' Do you want: displ (d) or velocity (v)' 
c      read(lin,'(a100)') ivel
c      if(ivel.eq.'D') ivel='d'
c      if(ivel.eq.'V') ivel='v'
c

      n=2*(nyq-1)
c
c     npoint=n2
      npoint=n
      tau = dt
c
c
      fmax = fu
      inst = 0

      pi=acos(-1.0)
      twpi=2.*pi
cx
c      write(6,*) 'nrange',nrange

c
c  read all data, mn seems to be half the number of data points
c
      do 1 i=1,nm
         read(12) omega(i),nkk
         do 2 j=1,nrange
            do 3 k=1,10
              if(isrc(k).eq.1) then
                 read(12) aa,bb
                 gg(j,i,k)=cmplx(sngl(aa),sngl(bb))
               endif
    3       continue
    2    continue
    1 continue

      close(12)
c
c   data read, now convert to time domain, possibly
c   correct for Q 
c
ccccccccccccccccccccccccccccc
c   loop over stations
cccccccccccccccccccccccccccccc

      do 4 nd=1,nrange           ! distance range, same as station loop
         t0x=range(nd)/vred(nd)
         yr = 0.0
c
c         write(6,*) 'range,yr,depth,npoint,t0x,dt,tau',
c     *   range(nd),yr,depth,npoint,t0x,dt,tau
c
            do 5 l=1,10                     ! loop over components
               if(isrc(l).ne.1) go to 444
               do 6 j=1,nm
    6             data(j)=gg(nd,j,l)
               do 7 j=nm+1,nyq
                  data(j)=cmplx(0.0,0.0)
    7          continue
               do 8 j=1,nm
                  freq=(j-1)*df
                  if(freq.lt.df) freq=0.01*df
                  om=twpi*freq
                  if(j.ne.1)data(n+2-j)=conjg(data(j))
    8          continue
               do 9 i=nm+1,nyq
                 data(i)=(0.,0.)
    9          data(n+2-i)=conjg(data(i))
c
               data(1)=cmplx(real(data(1)),0.0)
               data(nyq)=cmplx(real(data(nyq)),0.0)
               data(1)=(0.0,0.0)
               data(nyq)=(0.0,0.0)
               call four(data,n,+1,dt,df)
c
c     correct for damping
c
               fac = exp(alpha*t0x)
               dfac = exp(alpha*dt)
               do 10 i = 1,npoint
                  data(i)= data(i) * fac
                  fac = fac * dfac
   10          continue

               go to 122

  444          do 12 j=1,n
   12             data(j)=(0.0,0.0)

c     Time Domain Integration

cjh  122       if(ivel.eq.'d') then
 122           continue
               if(mt_dispvel.eq.'displacement') then               
                  totint=0.0
   
                  do 123 i=1,npoint
                     temp(i)=real(data(i))
 123              continue

                  do 124 i=1,npoint-1
                     prtint=0.5*dt*(temp(i)+temp(i+1))
                     totint=totint+prtint
                     temp(i)=totint
 124              continue
 
                  do 125, i=1,npoint-1
c not jh             data(i)=temp(i+1) - temp(1)
                     data(i)=temp(i) - temp(1)
 125             continue
                 endif
c 
               if(l.eq.1) then
                  do 200 i=1,npoint
                     data(i)=data(i)*(-1.0)
  200             continue
                  write(*,126) npoint,dt
  126             format("NPTS ",I5,"  DT ",f6.4)
               endif
  
               if(l.eq.4) then
                  do 201 i=1,npoint
                     data(i)=data(i)*(-1.0)
  201             continue
               endif

               if(l.eq.6) then
                  do 202 i=1,npoint
                     data(i)=data(i)*(-1.0)
  202             continue
               endif

               if(l.eq.8) then
                  do 203 i=1,npoint
                     data(i)=data(i)*(-1.0)
  203             continue
               endif
c
c   real data
c
               do i=1,npoint
                  y(i)=real(data(i))
               enddo

   
c
c   save component for later write out
c
               do i=1,npoint
                  gcomp(l,i)=y(i)
               enddo

    5       continue             ! loop over components
c
c   now write out for this station, not all components are used
c   and they come in a different order
c

c         open(10,file=outfile,status='unknown')
c
c   loop over components
c
         do m=1,8
            k=order(m)   ! fix order
c
c   channel header
c
            
            write(13,'(a)')
     *      '     0.0000e+00     0.0000e+00      0  0  0.00'   ! undocumented
            write(13,
     *      '(i8,f10.5,a,1x,a5,1x,a4,1x,i4,1x,2i2,1x,2i2,1x,f7.3)') 
     *      npoint,dt,'  0.0000e+00',  ! last is  ??
     *      stat(nd),comp(m),year,month,day,hour,min,sec
c
c   write data
c
            write(13,'(6e12.5)') (gcomp(k,i)*1.0e07,i=1,npoint) ! convert to nanometer
         enddo
cc         close(10)
       
    4 continue                   ! loop over stations
      close(13)
      

      stop
      end
c
c
      subroutine four(data,nn,isign,dt,df)
      dimension data(*)
      n = 2 * nn
      if(dt.eq.0.0) dt = 1./(nn*df)
      if(df.eq.0.0) df = 1./(nn*dt)
      if(dt.ne.(nn*df)) df = 1./(nn*dt)
      j = 1
      do 5 i=1,n,2
c     if(i-j)1,2,2
      if((i-j).GE.0) GOTO 2
    1 tempr = data(j)
      tempi = data(j+1)
      data(j) = data(i)
      data(j+1)=data(i+1)
      data(i) = tempr
      data(i+1) = tempi
    2 m = n/2
c   3 if(j-m) 5,5,4
    3 if((j-m).LE.0) GOTO 5
    4 j = j-m
      m = m/2
c     if(m-2)5,3,3
      if((m-2).GE.0) GOTO 3
    5 j=j+m
      mmax = 2
c   6 if(mmax-n) 7,10,10
    6 if((mmax-n).GE.0) GOTO 10
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
