c                                                                               
c     routine to plot response function       
c     jh February 1997
c
c  changes
c
c     
c     26 mar 99 bmt : include winplot.inc
c     june 99 jh    :  ------------  version 7.0 -----------------------
c     11 may 00 lo  : include waveform.inc and use read_resp routine
c      16 may 01 jh : fix common block
c     16 june 05 jh : plt to eps
c     29 apr 10  lo : call xcursor
c     27 dec 10  jh : gfortran on pc: remove winplot, put in implicit none, remove hctype
c                     unit check, remove display_type  
c     08 feb 11  jh : adjustment text strings to make it work on pc  
c     22 feb 11  jh : size from color.def                 
c     17 feb 14  lo : read seisan.def
c     2015.06.03 pv : moved line 10 continue, due to compiler warning on GOTO
c
c
c                                
      implicit none
      include 'seiplot.inc'
      include 'seidim.inc'
      include 'waveform.inc'
      real ff ! frequency
c-- number of points in spectrum
      integer nspec
c-- if several data sets, number of points in each
      integer nnspec(2)
c-- counters and help variables                                               
      integer i,j				
c-- response value
      complex respons
      character*80 text
      character*30 xtext,ytext      ! axis text
c--returns from spectral plot
      character*1 c_spec(20)
      real x0                       ! left corner of spectrum
c-- Arguments passed
      character*80     arg(10)
      real cx(20),cy(20)
      integer nc,nars
      real x(1000),y(1000)

      logical gse_resp          ! true if gse format
      logical sei_resp          ! true if seisan format
      integer nfa               ! number of frequencies and amplitude

c   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      real xpole(10)
      REAL FFILT(10)
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
c   sensor type 2: seismometer, 3: accelerometer
      integer sentyp
      real g1hz  ! gain at 1 hz
c   individual response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
      logical ftab                      ! if true, force tabulated values
      integer seiclen
      real xm,ym

c
c response common block
c
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa
      equivalence (xpole,pole)

      include 'version.inc'

      call get_seisan_def
 
c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      nars = 0
c 
c   get argument
c
      call get_arguments(nars,arg)    ! get arguments 
 10   continue
      if(nars .eq. 1) then
           wav_resp_file=arg(1)
      elseif(nars.eq.0) then
          text=' File name, number, ? to get list'
          call filename(text,wav_resp_file)      
          if(wav_resp_file.eq.'EOF') goto 999
      else
          write(6,*)' Enter one argument with file name'
          stop
      endif
 

c
c-- x-window size in % screen
c
      wsize=65
      call get_window_size
      if(size_presp.gt.0) wsize=size_presp ! from color.def
c
c
c  graphic display not yet open
c
      disp_open=0

c
c read response file
c
         call read_resp

         plotoption=1
c
c  open plot file
c
         open(2,file='presp.eps',status='unknown')
         plotunit=2
c
c   open plotter
c
         call open_display 
c
c set some postscipt scalings
c
         write(plotunit,*) ' 1.4 0.85 scale'
c
c  the amplitude response
c
      j=0
      do i = -260,190,1    ! log frequencies
         j=j+1
         ff=10**(i/100.0)
         call calc_resp(ff,respons)
         y(j)=log10(cabs(respons))
         x(j) = log10(ff)         
 1243    continue
      enddo
      nspec=j
c
c   plot spectrum
c
      xtext='      Log frequency (Hz)      '
      ytext='Log amplitude                 '
      text=' '                
      write(text(1:29),'(a)')'Amplitude Response, count/nm '
      nnspec(1)=nspec
      x0=100.0    

      nnspec(1)=nspec

            
      call xy_plot
     *(1,nnspec,x,y,text,xtext,ytext,590.0,250.0,
     *x0,420.0,2,1,30.0
     *,0,c_spec,nc,cx,cy)

c
c  the phase response
c
c      text=' '
c      write(text(1:15),'(a)')'Phase response '
      j=0
      do i = -260,190,1    ! log frequencies
         j=j+1
         ff=10**(i/100.0)
         call calc_resp(ff,respons)
         y(j)=180.0*atan2(imag(respons),real(respons))/3.141592654
         x(j) = log10(ff)        
      enddo
      nspec=j
c
c   plot spectrum
c
      ytext='Phase                         '
      text=' '
      write(text(1:15),'(a)')'Phase response '
      nnspec(1)=nspec
      text=' '
      text(1:9)='q to quit'
      call tchars(text(1:9),9,750.0,700.0)
      j=seiclen(wav_resp_file)
      call tchars(wav_resp_file(1:seiclen(wav_resp_file)),j,10.0,750.0)
      call pchars(wav_resp_file(1:seiclen(wav_resp_file)),j,10.0,750.0)
      j=1
      call xy_plot
     *(1,nnspec,x,y,text,xtext,ytext,590.0,250.0,
     *x0,70.0,2,1,30.0
     *,j,c_spec,nc,cx,cy)

c      call xscursr(i,xm,ym)   ! why was this call here, commented out jh dec 2010
c
c   close postscript
c
      call close_post
c
c   close output plot file
c 
      close(2)
c
c   back to alpha
c
      call clear_to_alpha
c
c   if filenr.lis list is used, try another
c   
      if(nars.eq.0) goto 10
c
 999  continue
      write(6,*)' Plot file in presp.eps'
      stop
      end                                                                       

