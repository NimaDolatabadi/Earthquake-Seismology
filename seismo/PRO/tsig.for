c
c   program to make tests signals in SEISAN format
c   j. havskov  jan 2001
c
c   program makes several traces, all have same length and sample rate
c   trace 1 is the sum of all traces
c   each trace can set parameters frequency, amplitude (remember this is
c   integer numbers in file so use at least 1000), phase, delay (delay time
c   when the signal appears, data before is zero) and damping (to simulate
c   seismometer damping, same meaning as of damping constant, but period is
c   not recalcualated to simualte changing period with damping)
c
c   an additional trace can be made with a brune displacement pulse generated
c   with prameters corner frequency (f0), q and kappa (see mulplt) and travel
c   time. Travel time is used for Q-correction and also places the pulse
c   at travel time distance from the origin, so length of trace must be
c   longer than travel time
c
c   latest update:
c
c   jh June 22 2001 : small clean up
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      real x(200000)         ! signal vector
      real y(10,200000)      ! --------------
      integer n              ! number of points
      character*80 mhead(12) ! main seisan header
      character*1040 chead   ! channel header
      character*80 text      ! general text
      real damping(100)      ! damping like for a seismometer
      integer i,j,k          ! counters
      real freq(100)         ! frequencies to use
      real amp(100)          ! amplitudes
      real phase(100)        ! phase
      real delay(100)        ! delay in seconds before start of signal
      integer nsignal        ! number of signals in same trace
      real time,rate         ! length of signal and sample rate
      real f0,q0,qalpha,kappa ! spectral parmeters
      logical spec           ! if true, make brune signal
      real ph                ! phase to calculate in radians
      real pi                ! phi
      real travel_time       ! p travel time
      integer code           ! error code
      real fs,brune


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      pi=3.1415927


      write(6,*)' Duration of signal in sec and sample rate (hz)'
      read(5,*) time,rate
c
c loop for reading the signal parameters
c
      i=0
 10   continue
      write(6,*) 'Frequency, amplitude(counts), phase(deg), delay,',
     *' damping,  enter to end'
      i=i+1
      read(5,'(a)') text
      if(text.eq.' ') goto 20
      call sei get values(5,text,code)
      code=e_ok$
      freq(i)=array$(1)
      amp(i)=array$(2)
      phase(i)=array$(3)
      delay(i)=array$(4)
      damping(i)=array$(5)
      goto 10
c
c   get Brune parameters, optional
c
 20   continue
      nsignal=i-1
      if(nsignal.eq.0) stop
      write(6,*) 'Number of signals',nsignal
      write(6,*) 'Spectral values f0,q0,qalpha,kappa, travel time,',
     *' enter for none'
      read(5,'(a)') text
      if(text.eq.' ') then
         spec=.false.
      else
         spec=.true.   ! brune signal
         call sei get values(5,text,code)
         code=e_ok$
         f0=array$(1)
         q0=array$(2)
         qalpha=array$(3)
         kappa=array$(4)
         travel_time=array$(5)
      endif
c
c   generate test signals
c
      n=time*rate+1
      do i=1,n
         x(i)=0.0
      enddo
c
      do j=1,nsignal
         do k=1,n
            y(j,k)=0
         enddo
         do k=1,n
            i=delay(j)*rate+k        
            ph=2.0*pi*freq(j)*(k-1)/rate+phase(j)/(180.0/pi)
            if(damping(j).ne.0.0) then
               y(j,i)=exp(-2.0*pi*freq(j)*(k-1)*damping(j)/rate)*
     *         amp(j)*sin(ph)
            else
               y(j,i)=amp(j)*sin(ph)
            endif
            x(i)=x(i)+y(j,i)
         enddo
      enddo
c
c   add if spectrum
c
      if(spec) nsignal=nsignal+1

c
c   write out, first make headers
c
      do i=1,12             ! clear main heades
         mhead(i)=' '
      enddo
      chead=' '             ! clear channel header
c
c  fill in parameters in main header
c
      mhead(1)(34:59)='100   1  1  1  1  1    0.0'  ! fictive data and time
      write(mhead(1)(32:33),'(i2)') nsignal+1       ! add summed channel
      write(mhead(1)(61:69),'(f9.3)')n-1/rate
      mhead(2)(2:9)='TESTS  Z'
c
c  parameters in channel header
c
      chead(1:9)='TEST S  Z'
      chead(10:35)=mhead(1)(34:59)
      write(chead(37:43),'(f7.2)') rate
      write(chead(45:50),'(i6)') n
      chead(77:77)='4'
c
c   open binary SEISAN output file
c
      open(1,file='tsig.out',status='unknown',
     *form='unformatted')
c
c   write main header
c
      do i=1,12
         write(1) mhead(i)
      enddo
c
c   write sum first
c
      write(1) chead             ! write channel header
      write(1)(int(x(i)),i=1,n)  ! SEISAN file only use integers
c
c   write each signal
c
      if(spec) nsignal=nsignal-1  ! one less since Brune sigal is additional to traces
c
      do j=1,nsignal
         write(chead(1:9),'(f5.2,1x,i3)') freq(j),int(phase(j))
         write(1) chead
         write(1)(int(y(j,i)),i=1,n)
      enddo

c
c  make Brune spectrum
c
      if(spec) then
         call make_brune(f0,q0,qalpha,kappa,travel_time,n,rate,x)
         chead(1:9)='BruneS  Z'
         write(1) chead
         write(1)(int(x(i)),i=1,n)
      endif
c
c   make an s-file
c
      open(2,file='tsigs.out',status='unknown')
      text=' '
      text(1:22)=' 2000 0101 0101 00.0 L'
      text(80:80)=' '
      write(2,'(a)') text
      text=' '
      text(2:9)='tsig.out'
      text(80:80)='6'
      write(2,'(a)') text
      text=' '
      text(1:)=' BruneSZ IP       0101'
      write(text(24:28),'(f5.1)') travel_time
      write(2,'(a)') text
      write(2,*)'   '

      close(1)
c
c
      write(6,*)'Output wave form file name is tsig.out'
      write(6,*)'Output s-file name is         tsigs.out'
      stop
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine make_brune(f0,q0,qalpha,kappa,travel_time,n,rate,y)
c
c  make the time signal from a brune spectrum
c
      implicit none
      real y(*)
      real rate               ! sample rate
      complex y_com(100000)
      real f                  ! frequency
      real f0                 ! corner frequency
      integer n               ! number of sample in
      integer nsamp           ! number of samples after padding
      real q0,qalpha,kappa    ! attenuation parameters
      real travel_time        ! p travel time
      integer i,exp2,k



c
c   find nearest 2**i
c
      do i=1,20
        if(2**i.gt.n) goto 10
      enddo
 10   continue

      nsamp=2**i
      exp2=i
c      write(6,*)'exp',exp2


      y_com(1)=0.0

      do i=2,nsamp/2+1
        f=(i-1)/(nsamp/rate)
        y_com(i)=1000000.0/(1.0+(f/f0)**2)
c
c   attenuation
c
        if(q0.gt.0.0) then
           y_com(i)=y_com(i)*exp(-3.1415*f*travel_time/(q0*f**qalpha))
        endif
        if(kappa.gt.0.0) then
           y_com(i)=y_com(i)*exp(-3.1415*f*kappa)
        endif
      enddo
c
c  make the inverse spectrum
c
      call fft(exp2,1.0,y_com)
c
c   normalize, only use same number of samples as put in
c   displace by travel time
c
      k=travel_time*rate
      do i=1,n
         y(i+k) = 1./real(nsamp) * real(y_com(i))
      enddo
      do i=1,k
         y(i)=0
      enddo
      
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real function brune(f,f0)
c
c   calculates the level of the Brune displacement spectrum at 
c   at frequency f . The spectrum has corner frequency f0. Spectral
c   level is 1.0

      implicit none
      real f,f0

      brune=1.0/(1.0+(f/f0)**2)
      return
      end
