c   Signal and spectral processing routines. This colllection of routines were
c   put together january, 2001 by gneralizing earlier routines belonging to
c   mulplt, spec and qlg


c     spec_select: select a time window of digitial data acording to given
c                  phase etc
c
c     smooth     : smoothing of a signal, usully a spectrum
c
c     remove_resp: correct a time signal for response
c
c     prepare    : prepare signal for spectral analysis, taper
c
c     spectrum   : make the complex spectrum
c
c     precoh1    : predicted coherence method for 3-comp analysis
c
c     cross      : corss correlaiton
c
c     rotate_comp: component rotation
c
c     spec_values: get corrected spectral levels at given frequencies
c
c     check_clipped: check if signal is clipped
c
c     applytaper: taper signal
c
c
c
c  updates:
c
c 01.02.2001 lo : change in spec_value, dont modify input signal
c 11.03.2001 lo : add ipow to the arguments of spev_values
c 22.06.2001 lo : free dimension of buff in smooth
c 15.08.2008 lo : add applytaper routine
c 28.12.2009 jh : add poles and zeros filter for remove_resp, spectrum
c                 and spec_value
c 19 01 2010 jh:  zero pole_low and pole_high at end of spectrum routine
c                 to ensure filter is not used next time filtering is
c                 done in frequency domain
c 9 04  2010 jh:  Moved out again since this gave problem in spec so
c                 now just outside call to spectrum when used in
c                 connection to mulplt
c 15 1 2012  jh:  remove filtering in frequency domain, filtering now in 
c                 time domain. This also mean that dc removal and tapring is
c                 no longer in routne prepare, now in transfilt in picsub. However
c                 for lf stability, there will be a butterworth hp filter
c                 at 1/5 of the low cut frequency if a filter is used
c 01 3 2012   jh  put dc rem and taper back in routine prepare since used many places !
c 01 4 2012   jh: add argument to auto_tr
c 0612 2013   Jh: q constant for f < 1 hz
c 0218 2014   jh: fix dimension 1 to *
c 0324 2014   jh: constant q below q_below_1hz instead of 1 Hz
c 0717 2015   jh: improve clip routine

c
c-----------------------------------------------------------------------------

      subroutine spec_select(data,nhead,nrecord,rotate,
     *           station,component,
     *           sel_crit,start,tstart,t0,nsamp,error,
     *           err_text)           
c
c                                                                               
c                                                                               
c   selects data for spectral analysis. Given a station and compoenent,
c   the chennel is selected from the waveform file and the time of the first
c   sample to use is calculated from start criterias and the header
c   time in the waveform file
c   the routine assumes that the general waveform reading routines are used
c
c   input:  data     : nordic
c           nhead    : number of headers
c           nrecord  : number of records
c           rotate   : rotate or not
c           station  : station to use
c           component: Component to use
c           nhead    : number of headers in Noridc file
c           nrecord  : number of records in --------
c           sel_crit: 1: ptime, 2: stime, 3: S from P time, 4: abs start
c           start:    start time from origin time in units of                   
c                     p or s-travel times, if sel_crit is 4, in secs                     
c
c   output:                            
c           tstart:   time of first sample in window relative to start          
c                     of waveform data file data trace       
c           t0:       time of origin relative to first sample in wav. file
c                     data trace
c           nsamp:    number of points in selected trace                                 
c           rate:     samples pr sec                                            
c           error:    number of errors found, should be zero                    
c           err_text  text of error
c                                                                               
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      character*80 data(*)
      integer nhead,nrecord
      logical rotate
      character*80 err_text
c-- trace file name                                
      character*80 trace_file
      integer nfile
      integer sel_crit
      real baz(max_trace)  ! back azimuth angles each trace
c-- station code                                  
      character*5  station,sta
      integer plotoption
c-- component                                           
      character*4  component			
c-- phase                                               
      character*1  phasex		
c-- see above                             
      real start,tstart    
c-- start of window relative origin time                                            
      real win_start
c-- number of channels in w-file          
      integer nchan                     
c-- number of samples in one channel      
      integer nsamp                     
c-- 1: test output, 0: no output                            
      integer check			
c-- origin date                                  
      integer oyear,omonth,oday		
c-- origin time                                         
      integer ohour,omin		
c-- origin time                                              
      real    osec			
c-- origin time relative time of first sample
      real t0
c-- phase time                                               
      integer phahour,phamin		
c-- -----                                 
      real    phasec                      
c-- abs times                            
      double precision phatime,wtime,otime	
c-- error variables                                     
      integer error		
c-- channel number to select                                 
      integer chan		
c-- directory separater char
      character*1 dchar	
c-- help variables                                
      integer	i,k,seiclen
      error=0                                                                   
      check=0
      call dir_char(dchar)                                                                   
c                                                                               
c   read origin time                                           
c                                                                               
      read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')                               
     *oyear,omonth,oday,ohour,omin,osec                                         
c                                                                               
c   get absolute origin time in secs                                            
c                                                                               
      call timsec(oyear,omonth,oday,ohour,omin,osec,otime)                      
c                                                                               
c   find station and  p-phase if so required                                                
c                                              
      if(sel_crit.eq.1.or.sel_crit.eq.3) then                                 
         do i=nhead+1,nrecord
            read(data(i),'(1x,a5,4x,a1)') sta,phasex                                                              
            if(sta.eq.station.and.phasex.eq.'P') go to  6                           
         enddo           
      endif                                                     
c      write(*,*) ' debug lo sel_crit ',sel_crit
c                                                                               
c   find station and s-phase if so required                                                
c                                              
      if(sel_crit.eq.2) then                                 
         do i=nhead+1,nrecord
            read(data(i),'(1x,a5,4x,a1)') sta,phasex                                                              
            if(sta.eq.station.and.phasex.eq.'S') go to  6                           
         enddo                                                                
      endif
c                                                                               
c   find only station if abs time is used                                      
c                                              
      if(sel_crit.eq.4) then                                 
         do i=nhead+1,nrecord
            read(data(i),'(1x,a5,4x,a1)') sta,phasex                           
            if(sta.eq.station) go to  6                           
         enddo
c
c   no station was found, can still continue if station in waveform file
c
           goto 7                                                                
      endif
c
c  if here, data not found, write error message
c
      if(sel_crit.eq.1.or.sel_crit.eq.3)
     *write(err_text,200) station,data(1)(1:20)
 200  format(' P for station ',a5,' not found for event ',a20)                  
      if(sel_crit.eq.2) write(err_text,210) station,data(1)(1:20)                                      
 210  format(' S for station ',a5,' not found for event ',a20)                  
      if(sel_crit.eq.4) write(err_text,220) station,data(1)(1:20)                                      
 220  format(' Station ',a5,' not found for event ',a20)                  
      error=error+1                                                             
      return                                                                    
c                                                                               
c   station or (station and phase) found, now read it                                      
c                                                                               
 6    continue                                                                  
      read(data(i),'(1x,a5,4x,a1,7x,2i2,f6.1)')                                
     *sta,phasex,phahour,phamin,phasec
c
 7    continue
c
c   calculate time from origin to start of data window
c
      if(sel_crit.eq.4) then              ! abs time not related to any phase
         win_start=start
      endif
c
      if(sel_crit.eq.1.or.sel_crit.eq.2.or.sel_crit.eq.3) then
c                                                                               
c   calculate abs phase time                                                         
c                                                                               
         call timsec(oyear,omonth,oday,phahour,phamin,phasec,phatime)
c                                                                               
c   calculate start from origin                                         
c      
         phatime=phatime-otime
         if(sel_crit.eq.3) then           ! calculate s time from p time                                                                         
            phatime=phatime*1.78                                
         endif
c
c   phatime is now start time of window from origin time, now multiply
c   by factor to give number of start times or just move it a bit
c
         win_start=phatime*start            
      endif
c                                                                               
c   get trace data file names                                                  
c               
      call auto_tr(data,nhead,nrecord,nfile,wav_filename)
c
c   check if ifles exist and where
c
       k=0
       do i=1,nfile
          call  get_full_wav_name(wav_filename(i),trace_file)
          if(trace_file.eq.' ') then
              write(6,'(1x,a,a)') ' No such file: ',
     *        wav_filename(i)(1:seiclen(wav_filename(i)))
         else
            k=k+1
            wav_filename(k)=trace_file
         endif
       enddo
       nfile=k
c      write(6,*) ' debug lo ', nfile
c
c   check if any name given
c
      if(nfile.eq.0) then
         write(err_text,'(a,a)')
     * ' No waveform file given for event', data(1)(1:20)
         error=error+1
         return
      endif
c                                                                               
c   initialize waveform file info
c
      call wav_init

c          
c   read all header information from all files
c          
       wav_nfiles=k
       do i=1,wav_nfiles
          call read_wav_header(i)
       enddo
c       write(6,'(a,a)')' Read headers from files:'
c       do i=1,wav_nfiles
c          write(6,'(1x,a)') wav_filename(i)(1:80)
c       enddo

c                                                                               
c   find channel number corresponding to station and                            
c   component, also get header time                                
c            
      call wav_find_chan(station,component,chan)
         if(chan.eq.0) then
         error=error+1                                                          
         write(err_text,'(a,a,a)')
     *   ' Station and component not found in trace file  ',
     *   station,component
         return
      endif                                                                     
      write(*,*) ' debug ',chan
c
c   if rotate, find back azimuths, read and rotate
c   else just read channel
c
      wav_out_start(1)=wav_delay(chan)   ! normally no delay
      if(rotate) then
          call get_baz(wav_nchan,wav_stat,data,nhead,nrecord,baz)
          call wav_read_2channel(chan)  ! read 2 channels
          nsamp=wav_out_duration(1)*wav_rate(chan)+1
          if(wav_comp(chan)(4:4).eq.'N') wav_rot_comp(chan)='R'
          if(wav_comp(chan)(4:4).eq.'E') wav_rot_comp(chan)='T'
          call rotate_comp(nsamp,
     *    wav_out_first_sample(wav_current_chan(2)),
     *    wav_out_first_sample(wav_current_chan(3)),
     *    wav_rot_comp(chan),
     *    baz(chan),signal1,signal2,signal3)  ! rotated signal now in signal1
      else
c        write(*,*) wav_year(chan),wav_month(chan),wav_day(chan),
c     &    wav_hour(chan),wav_min(chan),wav_stat(chan),wav_comp(chan)
c commented out by mistake in 8.1.1 lot
          call wav_read_channel(chan)
          nsamp=wav_nsamp(chan)
      endif
c

c                                                                               
c  get abs trace file start time                                                
c                                    
       wtime=wav_abs_time(chan)+wav_out_start(1)-wav_delay(chan)
c                                                                               
c   calculate start time for window relative to first                      
c   sample in trace data                                                        
c                                                                               
      tstart=win_start-(wtime-otime)                                           
c                                                                               
c   extract origin time t0 relative to time of first sample                     
c                                                                               
      t0=otime-wtime                          
c
c   if abs time, correct
c
      if(sel_crit.eq.4) then
         tstart=start
c lot 2005
c        t0=0.0
      endif
c                                                                               
c   close trace data file                                                       
c                                                                               
c                                                                               
      return                                                                    
      end                                                                       
                                                                                
                                                                                
                                                                                
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
C                                                                               
      SUBROUTINE SMOOTH(BUFF,JJ,ISMO)                                           
C                                                                               
C     BUFF  = BUFFER TO BE SMOOTHED --  DIMENSION : BUFF(JJ)                    
C     ISMO  = NUMBER OF PASSES                                                  
C                                                                               
c      DIMENSION BUFF(1)                                                         
      DIMENSION BUFF(*)                                                         
C                                                                               
      IF(ISMO.LE.0)     RETURN                                                  
C                                                                               
      DO 50  IS=1,ISMO                                                          
C                                                                               
      J1=JJ-1                                                                   
      A=.54*BUFF(1)+.46*BUFF(2)                                                 
      B=.54*BUFF(JJ)+.46*BUFF(J1)                                               
      SJ=BUFF(1)                                                                
      SK=BUFF(2)                                                                
C                                                                               
      DO 10  J=2,J1                                                             
      SI=SJ                                                                     
      SJ=SK                                                                     
      SK=BUFF(J+1)                                                              
   10 BUFF(J)=.54*SJ+.23*(SI+SK)                                                
C                                                                               
      BUFF(1)=A                                                                 
      BUFF(JJ)=B                                                                
C                                                                               
   50 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       



      subroutine remove_resp(y,y_com,nsamp,rate,disp_vel,
     *f_low,f_high,pole_low,pole_high,zero,pole,nzero,npole,
     *norm)
c                                                                               
c     Routine to remove instrument response from a trace. The response
c     informaiton is supposed to already have been read in and available
c     in the waveform common block, has to be checked outside this routine.
c     In addtion filters can be applied in frequency domain.
c
c     The trace is restored to nm, nm/s, or nm/s*s
c
c     input:  y           : data vector
c             nsamp       : number of samples in data vector
c             rate        : sample rate
c             disp_vel    : displacement ,velocity or acceelration out (1-3)
c             f_low,_high _ filter limits, Butterworth filters in f-domain
c             pole_low,pole_high: poles of Butterworth filters in f-domain
c             zero, pole  : zero and poles
c             nzero,npole : number of zereo and poles
c             norm        : normalization constant
c             
c     output  Y           : corrected signal
c             y_com       : complex equivalent, only there to use external
c                           memory definition
c                                                                               
c     Written by C. Lindholm Jan. -90, substantially changed by jh                                           
c
c-- common block         
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      real y(*)
c-- percentage of tapering (fixed)                       
      real taper/10./				
c-- length of input vector                                 
      integer nsamp				
c-- length after padding                                    
      integer ipow				
c-- vector padded with npad zeros                           
      integer npad				
c-- Fixed to 1 for system rem.                        
      integer rem_resp/1/			                       
c-- counters,etc                                               
      integer i,j
      real f_low,f_high            ! added filter to use
      integer pole_low,pole_high   ! -------------------

      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros

      real rate                    ! sample rate
      integer disp_vel             ! displacemnt, velocity or acceleration
      complex y_com(*)
      double precision ttime

      ttime=0.
c                                                                               
c------- prepare data.     Pad with zeros and taper 10%.                        
c                                                                               
      call prepare(nsamp,y,taper,ipow,npad,y_com)
c
c-------- Calculate the spectrum, and remove the system response
c         q, qzero, kappa and travel time set to zero so not
c         to be used. however filters might be multiplied in
c
      call spectrum(y_com,ipow,disp_vel,rem_resp,rate,
     +     f_low,f_high,pole_low,pole_high,0.0,0.0,
     *     0.0,ttime,zero,pole,nzero,npole,norm)
c
c   reset filters jh dec 09
c   implemented again here apr 9, 2010
c

c      nzero=0
c      npole=0
       pole_low=0
       pole_high=0
      
c                                                                               
c-------Do inverse FFT -------                                                  
c                                                                               
      j = ipow                                                                  
      do i = 1,20                                                               
        j = j/2                                                                 
          if (j .eq. 1)then                                                     
            j = i                                                               
            go to 222                                                           
          endif                                                                 
      enddo                                                                     
222   continue                                                                  
                                                                                
      call fft(j,1.,y_com)                                                        
c                                                                               
c------Normalize                     
c                                                                               
      do i=1,nsamp                                                               
        y(i) = (1./real(ipow)) * real(y_com(i))                                   
      enddo                                      
      return
      end                                                                       
                                                                                
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc           
      subroutine prepare(nsamp,y,taper,ipow,npad,y_com)                        
c                                                                               
c     Subroutine to pad data with zeros up to a number that is power of 2,      
c     and to taper the data.                                                    
c     Returned is the complex vector with the data prepared for FFT             
c                                                                               
c     Written by C. Lindholm Jan -90
c     jan 2001, jh: Put all input output in call
c                                                                               
c     input:   nsamp        Samples in data vector
c              y            Data vector
c     output:  npad         Number of zeros added                               
c              ipow         Length of new data vector
c              y_com        Complex data vector
c
      implicit none
c-- percentage of tapering                                    
      real taper				
      real pi/3.1415972/
      real y(*)
      complex y_com(*)
c-- dummy variable                                             
      real arg					
c-- number of samples to taper                              
      integer ntap				
      integer nsamp                                                             
c-- max = 2**20                                             
      integer ipow				
      integer npad                                                              
c-- counters                                               
      integer i				
c-- used in dc-removal                                    
      integer ndc
      real dc				
                                                                                
c                                                                               
c---------- Remove dc ------                                                    
c                                                                               
         ndc = 0                                                                   
         call remove_dc(y,ndc,dc,nsamp)                                         
c  
                                                                             
c-------Taper if wanted ------                                                  
c                                                                               
      if (taper.gt.0.0) then                                                  
        call applytaper(y,nsamp,taper)                   
      endif                                                                     
c                                                                               
c-------Now pad with zeros -------                                              
c                                                                               
      ipow = 1                                                                  
      do i = 1,20				                                                           
          ipow = ipow*2                                                            
       if(ipow .ge. nsamp) go to 10                                             
      enddo                                                                     
10    continue                                                                  
                                                                                
      npad = ipow - nsamp                                                       
c                                                                               
c-------fill complex vector                                                     
c                                                                               
      do i = 1,ipow                                                             
        if(i .le. nsamp)then                                                    
          y_com(i) = cmplx(y(i),0.0)                                           
        else                                                                    
          y_com(i) = cmplx(0.0,0.0)                                               
          y(i) = 0.0                                                         
        endif                                                                   
      enddo                                                                     
                                                                                
      return                                                                    
      end      
c
c---------------------------------------------------------------------                                                                 

      subroutine applytaper(y,nsamp,taper)
c
c apply sine taper, originally in subroutine prepare
c
      implicit none
      real y(*)
      real taper     ! per cent 
      integer nsamp,i,ntap
      real pi,arg

      pi=acos(-1.)

      ntap = (nsamp/2) * (taper/100)
      if (ntap.eq.1) ntap=2   ! lo 25/01/2018
      arg = pi / 2 / (ntap-1)
      do i = 1,ntap
         y(i) = y(i) * sin((i-1) * arg)
         y(nsamp-i+1) = y(nsamp-i+1) * sin((i-1) * arg)
      enddo
      return
      end
c
c--------------------------------------------------------------------------------
c                                                                               


      subroutine spectrum(y_com,ipow,disp_vel,rem_resp,srate,
     +     f_low,f_high,pole_low,pole_high,q0,qalpha,
     *     kappa,travel_time,zero,pole,nzero,npole,norm)
c
c     Routine to do the FFT and to remove instrument response
c     from the data. Filters can be added.
c
c     Written by C. Lindholm Jan. -90
c
c     updates:
c     oct 19, 90 by j.h. : fix all kinds of spectra
c     nov 1   90    j.h. : add q correction
c     jul 93        jh   : remove assumption that response cureves for
c                          accellerometers is g, now assume m
c     jan 2001      jh   : all data through window, no reference to mulplt.inc
c
c     input:      y_com       Complex vector with data in real part
c                 ipow        Length of complex vector
c                 disp_vel    response: 1 = displacement
c                                       2 = velocity
c                                       3 = acceleration
c                 rem_resp    Switch to remove or not remove response.
c                                       1 = remove response
c                 srate       Sampling rate
c                 f_low,f_high,pole_low,pole_high: frequecy and poles for filter
c                             from jan 2012 poles no longer used
c                 q,qalpha  : Q
c                 travel_time: travel time needed for q-correction, to start
c                             window
c                 kappa     : near surface term
c                 zero, pole  : zero and poles
c                 nzero,npole : number of zero and poles
c                 norm        : normalization constant
c        
c     
c     output:     y_com
c                 ipow
c
      implicit none
      include 'seisan.inc'
      complex y_com(*)                          ! data vector
      real f_low,f_high                         ! filter band
      integer pole_low,pole_high                ! filter poles

      complex pole(100),zero(100)               ! complex PAZ
      integer npole,nzero                       ! number of poles and zeros
      real norm                                 ! normalization constant for poles and zeros

      real q0,qalpha                            ! q
      integer disp_vel                          ! kind of spectrum
      double precision travel_time              ! changed lo, 10 may 2001
      real kappa                                ! kappa
      complex hs                                ! help variable
      complex respons				! complex respons
      real freq					! frequency corresp. to spectrum
      real srate
      real delta				! 1. / srate
      real xx					! dummy variabel
      real pi/3.1415972/
      integer ipow
      integer iresp				! power factor for response type
      integer rem_resp
      integer i,inv,j				! counter
      integer spstart,spstop			! bounds for spectr. counter

      delta = 1. / srate

c
c   it is assumed that all response curves are in displacement, also
c   for accelerometers
c
         iresp=disp_vel-1               

c
c------- Do FFT -------
c
      j = ipow
      do i = 1,20
        j = j/2
          if (j .eq. 1)then
            j = i
            go to 222
          endif
      enddo
222   continue
      call fft(j,-1.,y_com)
c
c--------Remove response------
c
      spstart = 2
      spstop = ((ipow/2) + 1)
cxx
c         write(6,*)'rem_resp',rem_resp
c         write(6,*)'spec but', f_low,pole_low,f_high,pole_high
c
c         write(6,*)'paz',nzero,npole

      if(rem_resp .eq. 1) then

        xx = ipow*delta
cx                write(6,*) f_low,f_high,pole_low,pole_high
        do i = spstart,spstop
           freq = (i-1)/xx
c
c-------Butterworth filter multiplying ---------------------
c       removed jan 15 2012 jh except for a very low HP filter

c         if((f_low.gt.0.0.and.pole_low. gt.0).or.(
c     *   f_high.gt.0.0.and.pole_high.gt.0)) then

          if(f_low.gt.0.0) then            
               call bworth(freq,f_low/5.0,-4,hs)   ! 1/5 the frequency, 4 poles
               y_com(i) = y_com(i) * hs
c
c  no longer use high cut, jan 2012
c
c           if(f_high.gt.0.and.pole_high.gt.0) then
c               call bworth(freq,f_high,pole_high,hs) 
c               y_com(i) = y_com(i) * hs
c           endif
            inv = ipow - i + 2
            y_com(inv) = conjg (y_com(i))
         endif
c
c-----Poles and zeros filter multiplying---------------------------
c

         if(nzero.gt.0.or.npole.gt.0) then
           call pazresp (freq, norm, nzero, zero,
     $     npole, pole,  hs)
           y_com(i) = y_com(i) * hs
           inv = ipow - i + 2
           y_com(inv) = conjg (y_com(i))
         endif

c
c--------Q--------------------------------------
c
         if(q0.gt.0) then
c            if(freq.gt.1.0) then
           if(q_below_1Hz.eq.0) then ! frequency dependent all freq
              y_com(i)=y_com(i)*exp(pi*freq*travel_time/
     *       (q0*freq**qalpha))
           else ! constat q below q_below_1hz
             y_com(i)=y_com(i)*exp(pi*freq*travel_time/
     *      (q0*(1+(freq/q_below_1hz)**qalpha)))
            endif
         endif
c
c   correct for kappa
c
          if(kappa.gt.0) then
             y_com(i)=y_com(i)*exp(pi*freq*kappa)
          endif

c
c-------- Calculate the response, it is assumed that the response parameters
c         for the current channel has already been read in and available
c         in resonse common block
c
          call calc_resp(freq,respons)
c
c-------Now correct spectrum -----------
c
           hs=(0,1)*(2*pi*freq)
           y_com(i) = (y_com(i) / respons ) *hs**iresp
           inv = ipow - i + 2
           y_com(inv) = conjg (y_com(i))
        enddo				! end of loop
      endif				! end of response removed
c
c   zero butterworth filter, jh jan 19 2010, was done in connection
c   with new magntudes and correction for filter in amgnitude amplitude.
c   however, made problems for other programs like spec, so now it is
c   moved outside again, so far only zeros when used in connection
c   with mulplt
c
c     pole_low=0
c     pole_high=0
    
      return
      end




      SUBROUTINE PRECOH1(Z,MPTS,IPTS,LPTS,PVEL,COHER,
     *AZIM,VELO,RMSAMP)
      REAL Z(MPTS,3)
      real dc(3)
C
C  S/R TO DO 3-COMPONENT AZIMUTH AND VELOCITY ESTIMATION FOR A P-PHASE
C  FOR A FIXED USER SPECIFIED TIME WINDOW.
C  METHOD: PREDICTED COHERENCE (R. ROBERTS, UPPSALA)
c 
c  jan 94 add remove dc
C
C  INPUT:
C     Z(MPTS,3) - DATA MATRIX
C                 Z(*,1) - VERTICAL CHANNEL (POSITIVE DIRECTION UP)
C                 Z(*,2) - NORTH CHANNEL
C                 Z(*,3) - EAST CHANNEL
C     MPTS      - USED FOR DIMENSIONING OF DATA MATRIX
C     IPTS      - FIRST SAMPLE IN TIME INTERVAL
C     LPTS      - LAST SAMPLE IN TIME INTERVAL
C     PVEL      - LOCAL P-WAVE VELOCITY AT SITE
C
C  OUTPUT:
C     COHER     - PREDICTED COHERENCE, SHOULD BE POSITIVE AND
C                 LARGER THAN ABOUT 0.1 FOR A P-PHASE
C     AZIM      - P-PHASE AZIMUTH (TOWARDS EVENT) IN DEGREES
C     VELO      - P-WAVE APPARENT VELOCITY IN KM/SEC
C     RMSAMP    - ROOT MEAN SQUARE AMPLITUDE
C
C  NOTE: IF COHER IS LESS THAN ABOUT 0.1 AZIMUTH AND VELOCITY ESTIMATE SHOULD
C        NOT BE USED   
C
      DATA RADDEG/57.2958/
      DATA PSRAT/1.73/   
c
c   remove dc
c
      do i=1,3
        dc(i)=0.0
      enddo
      do i=ipts,lpts
         do k=1,3
            dc(k)=dc(k)+z(i,k)
         enddo
      enddo
      do i=1,3
        dc(i)=dc(i)/(lpts-ipts+1)
        do k=ipts,lpts
          z(k,i)=z(k,i)-dc(i)
        enddo
      enddo
C
C  MODEL 8 - P WAVES ONLY
C  CALCULATE THE AUTO AND CROSS CORRELATIONS
C
      LENWIN=LPTS-IPTS+1
      CALL CROSS(Z(IPTS,2),Z(IPTS,2),LENWIN,XX)
      CALL CROSS(Z(IPTS,2),Z(IPTS,3),LENWIN,XY)
      CALL CROSS(Z(IPTS,2),Z(IPTS,1),LENWIN,XZ)
      CALL CROSS(Z(IPTS,3),Z(IPTS,3),LENWIN,YY)
      CALL CROSS(Z(IPTS,3),Z(IPTS,1),LENWIN,YZ)
      CALL CROSS(Z(IPTS,1),Z(IPTS,1),LENWIN,ZZ)
      YX=XY
      ZX=XZ
      ZY=YZ
      POWER=(XX+YY+ZZ)/LENWIN
      RMSAMP = SQRT(POWER)
C
C  CALCULATE AZIMUTH AND VERTICAL/RADIAL AMPLITUDE RATIO
C
      AZI=ATAN2(-YZ,-XZ)
      AZIM=AZI*RADDEG
      IF(AZIM.LT.0.) AZIM = AZIM + 360.
      ZOVERR=ZZ/SQRT(XZ*XZ+YZ*YZ)
      A=-ZOVERR*COS(AZI)
      B=-ZOVERR*SIN(AZI)
c     A=ZOVERR*COS(AZI)
c     B=ZOVERR*SIN(AZI)
c     write(6,*)'a,b,',a,b
C
C  CALCULATE PREDICTED COHERENCE
C
      ERR=0.
c     write(6,*) ipts,lpts
      DO 50 II=IPTS,LPTS
        CC=Z(II,1)-A*Z(II,2)-B*Z(II,3)
        ERR=ERR+CC*CC
 50   CONTINUE
      COHER=1.-ERR/ZZ
C
C  SIMPLE BIASED VELOCITY ESTIMATION (TO OBTAIN AN UNBIASED ESTIMATE
C  ONE NEED TO KNOW THE NOISE AMPLITUDE)
C
      SVEL=PVEL/PSRAT
      AI=ATAN(1./ZOVERR)
      VELO=SVEL/SIN(AI/2.)
C
      RETURN
      END


      SUBROUTINE CROSS(X,Y,L,A)
C-----------------------------------------------------------------------
C CALCULATES UNNORMALISED CROSS CORRELATION BETWEEN THE TWO
C REAL TIME SERIES X AND Y OF LENGTH L.
C THE RESULT IS RETURNED IN A.
C-----------------------------------------------------------------------
      DIMENSION X(L),Y(L)
      A=0.
      DO 1 I=1,L
        A=A+X(I)*Y(I)
 1    CONTINUE
      RETURN
      END


      subroutine rotate_comp(nsamp,start1,start2,
     *component,backazi,y1,y2,y3)
c
c   rotates the signal in y2(ns) and y3(ew), the result is put
c   variable y1. The output component must be specifed as T or R. Rotation
c   is done on nsamp samples starting from the first sample first1 and first2
c   in input arrays and starting with first sample in output array.
c   backazi is the backazimuth

      implicit none
      real y1(*),y2(*),y3(*)
      real backazi,caz,saz
      integer i,start1,start2,nsamp
      character*1 component
c
c   rotation parameters
c
      caz = -cos(backazi*3.14159/180.)
      saz = -sin(backazi*3.14159/180.)

      if(component.eq.'R') then
         do i=1,nsamp
            y1(i) = caz*y2(i+start1-1) + saz*y3(i+start2-1)  ! radial component
         enddo
      endif
c
      if(component.eq.'T') then
         do i=1,nsamp
           y1(i) = -saz*y2(i+start1-1) + caz*y3(i+start2-1)  ! transverse comp.
         enddo
      endif
c
      return
      end



      subroutine spec_value(current,sig,nsamp,nsmooth,f_low,
     &  f_high,disp_vel,farray,nfreq,write1,level,y_com,ipow,
     *  zero,pole,nzero,npole,norm)

c
c subroutine to take out spectral level at given frequencies
c lo 2000
c
c genralized by jh jan 2001
c 
c input:  current - current channel ID
c         sig     - input time signal
c         nsamp   - number of samples
c         f_low,f_high - frequency limits for spectrum
c         farray  - frequency value array
c         nfreq   - 
c         write1  - output unit, 0 if none
c         zero, pole  : zero and poles
c         nzero,npole : number of zereo and poles
c         norm        : normalization constant
c
c output: level   - spectral amplitude levels, averaged around frequencies
c                   given in farray
c         y_com   - complex spectrum
c         ipow    - spectral values in y_com between 2 and (ipow/2+1)
c         
c changes
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'waveform.inc'
 
      integer i,j,b,c         ! counters
      integer current         ! current trace number
      integer nsamp           ! number of samples in signal
      real sig(*)             ! the signal, time domain
      real y(max_sample)      ! the signal
      complex y_com(*)        ! complex spectrum
      real f_low,f_high       ! low and high limit for spactrum


      complex pole(100),zero(100)               ! complex PAZ
      integer npole,nzero                       ! number of poles and zeros
      real norm                                 ! normalization constant for poles and zeros

      integer disp_vel
      integer nfreq           ! number of frequencies in farray
      integer n(max_sample)   ! counters
      real farray(*)          ! array with selected frequencies
      real level(*)           ! spectral level atfrequencies given in farray
      integer write1          ! output unit, 0 for no output

      integer npad            ! vector padded with npad zeros
      integer ipow            ! length after padding
      real ff                 ! frequency
      real taper/5./          ! percentage of tapering (fixed)
      integer rem_resp        ! 1 if remove
      integer nsmooth         ! number of times to smooth
      real fdelta             ! width for averaging of spectra around 
                              ! selected frequencies
      real window             ! time length of window
      real level_next_f(max_sample)  ! level at next f outside desired f window
      double precision ttime

      i=current
c      nsmooth=5
cjh      disp_vel=1
      rem_resp=1
      fdelta=0.12

      do j=1,nsamp ! lo keep original signal
        y(j)=sig(j)
      enddo

      window = nsamp / wav_rate(i)

c
c------- prepare data.     Pad with zeros and taper 10%.
c
      call prepare(nsamp,y,taper,ipow,npad,y_com)

c
c remove response and get spectrum, no q or kappa correction,
c no filters
c
      ttime=0.
      call spectrum(y_com,ipow,disp_vel,rem_resp,wav_rate(i),
     +          0.,0.,0,0,0.,0.,0.,ttime,zero,pole,nzero,npole,norm)

c
c   calculate real spectrum, limit to frequencies ffmin and ffmax
c
      c = 0
      do b = 2,((ipow/2) + 1)
       ff = (b-1)*wav_rate(i)/ipow
       if(ff .ge.f_low.and. ff .le. f_high) then
         c = c + 1
         y(c) = ((1/wav_rate(i))**2)*(y_com(b)*conjg(y_com(b)))
         y(c) = sqrt(y(c))         
       endif
      enddo
c
c   smooth
c
c      call smooth(y,c,nsmooth)

c
c now extract values from spectrum
c
      do j=1,nfreq
        level(j)=0.
        level_next_f(j)=-1.
        n(j)=0
      enddo

      c = 0
      do b = 2,((ipow/2) + 1)
       ff = (b-1)*wav_rate(i)/ipow
       if(ff .ge.f_low.and. ff .le. f_high)then
         c = c + 1
         if (write1.ne.0) write(write1,'(f10.3,1x,f15.7)') ff,y(c)
         do j=1,nfreq
           if (ff.ge.farray(j)-fdelta*ff.and.
     &       ff.le.farray(j)+fdelta*ff) then
c           if (ff.ge.farray(j)-sqrt(ff*fdelta*wav_rate(i)/ipow).and.
c     &       ff.le.farray(j)+sqrt(ff*fdelta*wav_rate(i)/ipow)) then
                level(j)=level(j)+y(c)
                n(j) = n(j) + 1
           elseif (ff.gt.farray(j)+fdelta*ff.and.level_next_f(j).eq.-1.) 
     &     then
             level_next_f(j) = y(c)
           endif
         enddo
       endif
      enddo
      if (write1.ne.0) write(write1,'(a1)') '>'

c
c average levels around center frequency
c
      do j=1,nfreq
        if (n(j).gt.0) then
          level(j)=level(j)/float(n(j))
c
c take level at next frequency
c
        else
          level(j)=level_next_f(j)
        endif
      enddo
  
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine check_clipped(x,nsamp,clipped)
c
c routine to check if signal is clipped, which is if the max amplitude is 
c reapeated more than 25 times. there is also a check for near
c constant levels with 2 methods. however, level check is not used if
c the maimum count is less then 1900 assuming no data will be less
c then 12 bit
c
c input:  x       - time series
c         nsamp   - number of samples in x
c output: clipped - true if signal is clipped
c

      implicit none
      real x(*)                         ! data
      integer nsamp,i,max_cnt           ! number of samples, counters
      integer n_level                   ! number of samples at a level
      real max                          ! max value
      integer k, kk
      real dc                           ! dc
      logical clipped                   ! flag to show that signal is clipped

      max=0.0
      k=0
      kk=0
      max_cnt=0.
      n_level=0
      clipped=.false.

      call remove_dc(x,nsamp,dc,nsamp)
      do i=1,nsamp
        if (abs(x(i)).gt.max) then
          max=abs(x(i)) 
        endif
      enddo
c
c   assume max must be larger than 1900 corresponding to a 12
c   bit converter with some dc 
c
c
      if(max.lt.1900) return
c
c   find constant levels, near max
c
      do i=1,nsamp
        if(x(i).gt.0.0) then
          if(x(i).gt.max*0.98.and.x(i).le.max) then
             k=k+1
          else
             if(k.gt.5) then
                n_level=n_level+1
                k=0
             else
                k=0
             endif
          endif
        else
          if(x(i).lt.-max*0.98.and.x(i).ge.-max) then
             kk=kk+1
          else
             if(kk.gt.5) then
                n_level=n_level+1
                kk=0
             else
                kk=0
             endif
          endif
        endif
      enddo

      if(n_level.ge.2) then
         clipped=.true.
      endif
c
c   find levels baased on near constant values near the max
c   within 10 %
c
      k=0
      n_level=0
      do i=2,nsamp
         if(abs(x(i)).gt.max*.9) then
            if(abs(x(i)-x(i-1)).lt.10) then
               k=k+1
            else
               k=0
            endif
            if(k.gt.5) then
              n_level=n_level+1
              k=0
            endif
         endif
      enddo
c     write(77,*)'constant levels', n_level

      if(n_level.gt.3) then
         clipped=.true.
c        write(3,*) 'clipped constant level'
      endif

      do i=1,nsamp
        if (abs(x(i)).gt.max*.98) then  ! changed from .99 to 0.98
          max_cnt=max_cnt+1
        endif
      enddo
c
c if the max amplitude appears several times, the signal is considered
c to be clipped
c

c
c changed to 10, 13 March 2001, lo
c was 100, changed to 20 jul 2015 jh
c
      if (max_cnt.ge.20) then
c        write(3,*) 'number of max',max_cnt
        clipped=.true.
      endif

      return
      end

      real function crosscorr(y1,y2,nsamp,mode)
c
c Lars Ottemoller, 06/03/2008
c
c compute cross-correlation of signals y1 and y2, both
c of length nsamp, cross-correlation output in yout, mode see below
c
c mode 1: 
c              sum (y1(i)-y1_mean) * (y2(i)-y2_mean)
c   yout = -------------------------------------------------
c          sqrt (sum (y1(i)-y1_mean)^2 * (y2(i)-y2_mean)^2 )
c
      implicit none
      include 'seidim.inc'
      real y1(*),y2(*)        ! data vectors
      real yout               ! cc output
      double precision a1,a2  ! dc of the two data vectors
      integer nsamp           ! number of samples
      integer mode            ! operation mode
      real n1,n2              ! auto correlations
      integer i

      a1=0.
      a2=0.
c get mean (a1,a2) of both signals
      do i=1,nsamp
        a1=a1+y1(i)
        a2=a2+y1(i)
      enddo
      a1=a1/float(nsamp)
      a2=a2/float(nsamp)
c remove mean and calculate yout
      yout=0.
      n1=0.
      n2=0.
      do i=1,nsamp
        n1 = n1 + (y1(i)-a1)**2
        n2 = n2 + (y2(i)-a2)**2
        yout = yout + (y1(i)-a1) * (y2(i)-a2) 
      enddo
c normalize
      yout=yout/sqrt(n1*n2)
      crosscorr=yout
      return 
      end

      subroutine cor_time(y1,nsamp1,y2,nsamp2,cc,nsampcc)
c
c Lars Ottemoller, 06/03/2008
c
c compute cross-correlation in time domain 
c
      implicit none
      include 'seidim.inc'
      real y1(*),y2(*),cc(*)
      real y1work(max_sample),y2work(max_sample)
      integer mode
      integer nsamp1,nsamp2,nsampcc,nsamps,nsampl
      integer i,k,n
      real crosscorr

      mode=1 ! cross correlation normalized with 
c              sqrt of both auto-correlation functions

c
c set nsamp to larger of the two samples 
c y1work is used for shorter data vector
c
      if (nsamp1.ge.nsamp2) then
        nsampl=nsamp1 
        nsamps=nsamp2 
        do i=1,nsamp2
          y1work(i)=y2(i)
        enddo
      else
        nsampl=nsamp2 
        nsamps=nsamp1 
        do i=1,nsamp1
          y1work(i)=y1(i)
        enddo
      endif

c compute correlation function
      do i=1,nsampl
        n=0
        do k=i,i-1+nsamps
          n=n+1
          if (nsampl.eq.nsamp2) then
            if (k.le.nsamp2) then
              y2work(n)=y2(k)
            else
              y2work(n)=0.
            endif
          else
            if (k.le.nsamp1) then
              y2work(n)=y1(k)
            else
              y2work(n)=0.
            endif
          endif
        enddo
        cc(i)=crosscorr(y1work,y2work,nsamps,mode)
      enddo
      nsampcc=nsampl
      return
      end

      subroutine cor_freq(y1,nsamp1,y2,nsamp2,yout)
c
c compute cross-correlation function
c
      implicit none
      include 'seidim.inc'
      real y1(*),y2(*),yout(*)
      real y1work(max_sample),y2work(max_sample)
      real max1,max2,option
      double precision scale,a1,a2
      integer nsamp1,nsamp2,nsampout,i,j,k,l,n,ndc,nn
      real rdc
      complex ans(2*max_sample),dummy(2*max_sample)

      scale=0.
      max1=1.
      ndc=0
      a1=0.
c
c find smallest number x=2**y > nsamp1
c
      nn=2
      do while(nn.lt.nsamp1)
        nn=nn*2
      enddo
      write(*,*) ' nn = ',nn
      do i=nsamp1+1,nn
        y1(i)=0.
      enddo
      do i=1,nn
        y1work(i)=y1(i)
      enddo
c
c get auto-correlation of first signal
c
      call correl(y1work,y1work,nn,ans,dummy)
      a1=real(ans(1))
      do i=1,n
        write(33,*) i,real(ans(i))
      enddo
      do i=1,nsamp2
c
c copy signal of length nsamp1
c
        n=0
        do k=i,i-1+nn
          n=n+1
          if (k.le.nsamp2) then
            y2work(n)=y2(k)
          else
            y2work(n)=0.
          endif
        enddo
c remove dc
        call remove_dc(y2work,ndc,rdc,nn)
        call correl(y2work,y2work,nn,ans,dummy)
        a2=real(ans(1))
        call correl(y1work,y2work,nn,ans,dummy)
c normalise so that maximum of auto-correlation is 1
        yout(i)=real(ans(1))/sqrt(a1*a2)
      enddo
      return
      end

      SUBROUTINE correl(data1,data2,n,ans,fft)
      REAL data1(*),data2(*)
      COMPLEX ans(*),fft(*)
CU    USES realft,twofft
      INTEGER i,no2,n
      call twofft(data1,data2,fft,ans,n)
      no2=n/2
      do 11 i=1,no2+1
        ans(i)=fft(i)*conjg(ans(i))/float(no2)
11    continue
      ans(1)=cmplx(real(ans(1)),real(ans(no2+1)))
      call realft(ans,n,-1)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software K25,)5.

      SUBROUTINE realft(data,n,isign)
      INTEGER isign,n
      REAL data(*)
CU    USES four1
      INTEGER i,i1,i2,i3,i4,n2p3
      REAL c1,c2,h1i,h1r,h2i,h2r,wis,wrs
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      theta=3.141592653589793d0/dble(n/2)
      c1=0.5
      if (isign.eq.1) then
        c2=-0.5
        call four1(data,n/2,+1)
      else
        c2=0.5
        theta=-theta
      endif
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      wr=1.0d0+wpr
      wi=wpi
      n2p3=n+3
      do 11 i=2,n/4
        i1=2*i-1
        i2=i1+1
        i3=n2p3-i2
        i4=i3+1
        wrs=sngl(wr)
        wis=sngl(wi)
        h1r=c1*(data(i1)+data(i3))
        h1i=c1*(data(i2)-data(i4))
        h2r=-c2*(data(i2)+data(i4))
        h2i=c2*(data(i1)-data(i3))
        data(i1)=h1r+wrs*h2r-wis*h2i
        data(i2)=h1i+wrs*h2i+wis*h2r
        data(i3)=h1r-wrs*h2r+wis*h2i
        data(i4)=-h1i+wrs*h2i+wis*h2r
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
11    continue
      if (isign.eq.1) then
        h1r=data(1)
        data(1)=h1r+data(2)
        data(2)=h1r-data(2)
      else
        h1r=data(1)
        data(1)=c1*(h1r+data(2))
        data(2)=c1*(h1r-data(2))
        call four1(data,n/2,-1)
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software K25,)5.

      SUBROUTINE twofft(data1,data2,fft1,fft2,n)
      INTEGER n
      REAL data1(*),data2(*)
      COMPLEX fft1(*),fft2(*)
CU    USES four1
      INTEGER j,n2
      COMPLEX h1,h2,c1,c2
      c1=cmplx(0.5,0.0)
      c2=cmplx(0.0,-0.5)
      do 11 j=1,n
        fft1(j)=cmplx(data1(j),data2(j))
11    continue
      call four1(fft1,n,1)
      fft2(1)=cmplx(aimag(fft1(1)),0.0)
      fft1(1)=cmplx(real(fft1(1)),0.0)
      n2=n+2
      do 12 j=2,n/2+1
        h1=c1*(fft1(j)+conjg(fft1(n2-j)))
        h2=c2*(fft1(j)-conjg(fft1(n2-j)))
        fft1(j)=h1
        fft1(n2-j)=conjg(h1)
        fft2(j)=h2
        fft2(n2-j)=conjg(h2)
12    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software K25,)5.
      SUBROUTINE four1(data,nn,isign)
      include 'seidim.inc'
      INTEGER isign,nn
      REAL data(4*max_sample)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=data(j)
          tempi=data(j+1)
          data(j)=data(i)
          data(j+1)=data(i+1)
          data(i)=tempr
          data(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2
2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
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
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep
      goto 2
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software K25,)5.

      subroutine check4gap(signal,nsamp,rate,mingap,gapnsamp,gap)
      implicit none
      real signal(*)
      integer nsamp
      integer gapnsamp
      real rate,gap
      real mingap         ! in seconds
 
      logical gap_flag    ! flag
      integer nz,nzt      ! gap counters
      integer j
 
c
c check for gap, which has constant level for more than 1 s
c
      gap_flag=.false.
      nz=0
      nzt=0
      do j=2,nsamp
        if (signal(j).eq.signal(j-1)) then
          nz=nz+1
          gap_flag=.true.
        else
          if (gap_flag) then
            if (float(nz)/rate.gt.mingap) then
              nzt=nzt+nz+1
            endif
          endif
          nz=0
          gap_flag=.false.
        endif
      enddo
c
c case of gap at the end
c
      if (nz.ne.0) then
        nzt=nzt+nz+1
      endif
      gapnsamp=nzt
      gap=float(nzt)/rate

      return
      end


