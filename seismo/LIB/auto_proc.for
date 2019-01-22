cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c SUBROUTINES for autosig and others
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

Changes

c  22-03-2013 jh   remove msdif, dist, depth, vp, vs,density from 
c                  get_om_fo
c  06-12-2013 jh   back to q constant below 1 Hz
c  18-03-2013 jh   use original function in evel_q so both constant q and 
c                  variable q possible below a given frequency
c                  also check s/n at high frerquency end
c  29-08-2013 jh   fix in above, see 29-08


      subroutine rmean(idata,nlen)
c
c simple routine to remove the mean
c
c input/output: idata - real array with nlen samples
c

      implicit none
C    Seisan library inserts and routines...
C    ======================================
C
      include 'libsei.inc'                ! Open file definitions
      include 'seidim.inc'

      real idata(*)                       ! data
      integer i                           ! counters
      integer nlen                        ! number of samples
      real dc                             ! average or DC level

c
c get dc
c
      dc=0.
      do i=1,nlen
        dc=dc+idata(i)
      enddo
      dc=dc/nlen

c
c remove dc
c
      do i=1,nlen
        idata(i)=idata(i)-int(dc)
      enddo

      return
      end
        



      subroutine integrate(idata,nlen,rate)
c
c integration based on trapezoid rule
c
c by Lars Ottemoeller, Dec 1999
c
c input/output: idata - real data array with nlen samples
c               rate  - sample rate in Hz

      implicit none

C    Seisan library inserts and routines...
C    ======================================
C
      include 'libsei.inc'                ! Open file definitions
      include 'seidim.inc'

      real idata(max_sample),raw_signal(max_sample)
      integer   nlen
      real sum,ri
      double precision rate
      integer i

      sum = 0
      do i=1,nlen

        ri=i
        if(i-2.gt.0) then
          sum = sum + idata(i-1)
        endif
        raw_signal(i) = (ri-1)/2/rate * (idata(1)+idata(i)+2*sum)
      enddo

      do i=1,max_sample
        idata(i)=raw_signal(i)
      enddo

      return
      end
      

      subroutine rtr(data,nlen,rate)
c
c remove linear lsq trend
c
c by Lars Ottemoeller, Dec 1999
c
c input/output: data - real data array with nlen samples
c               rate - sample rate in Hz
c
      implicit none
      include 'seidim.inc'
      real data(*),x(max_sample) ! x and y data
      integer nlen,i                      ! number of samples and counters
      real a,b,corr,rms                   ! lsqlin output
      double precision rate               ! rate

c
c get x coords
c
      do i=1,max_sample
        x(i)=i/rate
      enddo

      call lsqlin(nlen,x,data,a,b,corr,rms)

      write(*,*) 'correction applied: y=',b,'*x+',a
c
c remove trend
c
      do i=1,max_sample
        data(i)=data(i)-b*x(i)-a
      enddo
  
      return
      end


      subroutine differentiate(idata,nlen,rate)
c
c differentiate
c
c by Lars Ottemoeller, Dec 19999
c
      implicit none

      include 'seidim.inc'
      real idata(max_sample),raw_signal(max_sample)
      double precision rate
      integer i,nlen

      do i=2,nlen-1
        raw_signal(i)=(idata(i+1)-idata(i-1))/2*rate
      enddo
      idata(1)=0
      idata(nlen)=0
      do i=2,nlen-1
        idata(i)=raw_signal(i)
      enddo
      
      return
      end


      subroutine char_fu(data,nlen,k)
c
c calculate characteristic function as suggested by Rex Allen,
c output data are y^2 + k*(y')^2, k is given in the parameter file
c
c input/output: data - real data array with nlen samples
c               k    - real number in formula above
c
      implicit none
      include 'seidim.inc'
      real data(*),raw_signal(max_sample)   ! data
      integer nlen,i                            ! number of samples and counter
      real k                                    ! real number in formula above

c
c calculate characteristic function
c
      do i=2,nlen-1
        raw_signal(i)=data(i)*data(i)+k*(data(i+1)-data(i-1))*
     &         (data(i+1)-data(i-1))
      enddo
c
c copy output into data
c
      do i=2,nlen-1
        data(i)=raw_signal(i)
      enddo
      data(1)=0
      data(nlen)=0

      return
      end


      subroutine sta_lta_nrec(data,nlen,rate,lsta,llta,lsq,ratio,
     &     lfreeze)
c
c computation of squared sta/lta nonrecursive, first the start sta and lta 
c levels are determined from the beginning of the trace, the sta and lta 
c windows are not overlapping, reference ...
c
c input: data - real input data with nlen samples
c        nlen - number of samples in trace
c        rate - sample rate in Hz
c        lsta - length of sta window in seconds
c        llta - length of lta window in seconds
c        lsq  - true if squares, false if abs
c        ratio - trigger ratio
c        lfreeze - true if to freeze lta when sta/lta over ratio
c
      implicit none
      include 'seidim.inc'
      real data(*)
      double precision raw_signal(max_sample)   ! data
      integer nlen,i                            ! number of samples and counter
      double precision rate                     ! sample rate
      real lsta,llta                            ! length of sta and lta in secs
      integer nsta,nlta                         ! number of samples for sta and
                                                ! lta
      double precision sta,lta                  ! sta and lta level
      logical lsq                               ! square or abs sta/lta
      integer nstart                            ! number of samples for initial lta
      integer ind
      logical lfreeze                           ! freeze lta if true and trigger
      real ratio                                ! sta/lta required for trigger
       

c
c determine number of samples in lta and sta
c
      nsta=int(lsta*rate)
      nlta=int(llta*rate)
      nstart=nsta * 2

      sta=0
      do i=1,nstart
        if (lsq) then
          sta=sta+data(i)*data(i)
        else
          sta=sta+abs(data(i))
        endif
      enddo
      sta=sta/float(nstart)
      
      lta=0
      do i=1,nstart-nsta
        if (lsq) then
           lta=lta+data(i)*data(i)
        else 
           lta=lta+abs(data(i))
        endif
      enddo
      lta=lta/float(nstart-nsta)

c      do i=nsta+nlta+1,nlen
      do i=nstart+1,nlen

c
c get sta and lta
c
        ind = i-nsta-1-nlta
c        if (ind.le.0) ind = i-nsta-1-nstart
c        if (ind.le.0) ind = 1
        if (lsq) then
          sta=sta+(data(i)*data(i)-data(i-nsta)*data(i-nsta))/nsta
          if ((sta/lta.le.ratio.or..not.lfreeze).and.
     &      ind.gt.0) then
            lta=lta+(data(i-nsta-1)*(data(i-nsta-1))-
     &      data(ind)*data(ind))/nlta
          else
            ! keep it on pre trigger level
          endif
        else
          sta=sta+(abs(data(i))-abs(data(i-nsta)))/nsta
          if ((sta/lta.le.ratio.or..not.lfreeze).and.
     &        ind.gt.0) then
            lta=lta+(abs(data(i-nsta-1))-
c     &       abs(data(i-nsta-nlta-1)))/nlta
     &       abs(data(ind)))/nlta
          else
            ! keep it on pre trigger level
          endif
        endif

c
c sta/lta ratio
c
        if (lta.eq.0.) then
          raw_signal(i) = 0
        else
          raw_signal(i) = sta/lta
c          write(115,*) i,data(i),sta/lta,sta,lta,nsta,nlta
        endif
      enddo

c
c first values are set to 0
c
      do i=1,1+nstart
        data(i)=0
      enddo
c
c write output into data
c
c      do i=nsta+nlta+1,nlen
      do i=1+nstart,nlen
        data(i)=raw_signal(i)
      enddo

      return
      end


      subroutine sta_lta_rec(data,nlen,rate,lsta,llta,lsq)
c
c sta/lta recursive, does not seem to work well, ok now jh
c
c substantially modified, do not require short lta, put in missing
c abs on data and add squared values, initialize over 10 times sta
c time window
c jh dec 2000
c
      implicit none
      include 'seidim.inc'
      real data(*),raw_signal(max_sample)
      logical lsq
      integer nlen,i
      double precision rate
      real lsta,llta
      integer nsta,nlta
      real sta,lta,x,y

      nsta=int(lsta*rate)
      nlta=int(llta*rate)

      sta=0
      do i=1,10*nsta
         if(lsq) then
            sta=sta+data(i)*data(i)
         else
            sta=sta+abs(data(i))
         endif
      enddo
      sta=sta/float(10*nsta)
      lta=sta

      x=nsta
      y=nlta
      do i=nsta*10+1,nlen
         if(lsq) then
            sta=sta+(data(i)*data(i)-sta)/x   ! add lsq, jh dec 2000
         else
            sta=sta+(abs(data(i))-sta)/x   ! add abs, jh dec 2000
         endif
         lta=lta+(sta-lta)/y

         if (lta.eq.0) then
           raw_signal(i) = 0
         else
           raw_signal(i) = sta/lta
         endif

      enddo

      do i=1,nsta*10
        data(i)=0
      enddo
      do i=nsta*10+1,nlen
        data(i)=raw_signal(i)
      enddo

      return
      end


      subroutine sta_lta_rec_old(data,nlen,rate,lsta,llta)
c
c sta/lta nonrecursive, does not seem to work well
c
      implicit none
      include 'seidim.inc'
      real data(*),raw_signal(max_sample)
      integer nlen,i
      double precision rate
      real lsta,llta
      integer nsta,nlta
      real sta,lta,x,y

      nsta=int(lsta*rate)
      nlta=int(llta*rate)

      sta=0
      do i=nlta+1,nlta+nsta
        sta=sta+data(i)
      enddo
      sta=sta/nsta

      lta=0
      do i=1,nlta
        lta=lta+data(i)
      enddo
      lta=lta/nlta

      x=nsta
      y=nlta
      do i=nsta+nlta+1,nlen

        sta=(1-1/x)*sta+1/x*data(i)
        lta=(1-1/y)*lta+1/y*data(i-nsta-1)

        if (lta.eq.0) then
           raw_signal(i) = 0
        else
          raw_signal(i) = sta/lta
        endif

      enddo

      do i=1,nsta+nlta
        data(i)=0
      enddo
      do i=nsta+nlta+1,nlen
        data(i)=raw_signal(i)
      enddo

      return
      end

      
      subroutine find_phases(current,data,raw_signal,nlen,rate,lsta,
     &  ratio,dtrle,start,phases,phase_time,f_sel,fr_type,spike_test)
c
c routine to find phases, which are given by trace value (sta/lta) above 
c the ratio, phases are written to array phases, start form sample after 
c lsta from the trace start, check that signal spectral amplitude is at least 2
c the noise 
c
c input: 
c        current - current index to waveform block
c        data - array of nlen samples, char function
c        raw_signal - original data
c        rate - sample rate in Hz
c        lsta - length of sta window
c        ratio - trigger level
c        dtrle - detrigger level
c        start - array of trace start in seconds
c output:
c        phases - array of phases in Nordic format
c        phase_time - time of phases in seconds
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'    ! Seisan includes
      include 'waveform.inc'  ! ---------------
      real data(*),ratio,lsta,raw_signal(*),dtrle
      integer nlen,i,c,j,nsta,current,ind
      character*80 phases(max_phase)
      double precision phase_time(max_phase)
      double precision rate,drate,start,time
      integer year,month,jday,day,hour,min
      real sec,x,yy
      real fmin
      real f_sel(2)
      character*2 fr_type,fr_type_save
      logical flag,spike_test,spike_flag,spiketest,lresp

      fr_type_save = '0'
      lresp=.true.

      if (wav_resp_status.eq.'9') then
        write(*,*) ' no response for: ' //wav_stat(current)//' '//
     &       wav_comp(current)
        lresp=.false.
c        return
      endif
      drate=rate

      do i=1,max_phase
        phases(i)(1:40) ='                                        '
        phases(i)(41:80)='                                        '
      enddo
  
      nsta=int(lsta*drate)
      c=0
      flag=.false.         ! true if signal above trigger
      phase_time(1)=-1
      do i=1,nlen 
        x=float(i)
        if (data(i).ge.ratio.and.c.lt.max_phase.and..not.flag)then
          c=c+1
          time=x/drate+start
cccc          time=x/drate+start - lsta   ! use start of sta window, lot 04/03/03
          call sectim(time,year,jday,month,day,hour,min,sec)
c
c check if spike
c
          spike_flag=.false.
          if (spike_test) then
            ind=i
            spike_flag=spiketest(raw_signal,ind,nlen,rate)
          endif
c
c check if signal spectrum above noise (by factor 1.6)
c
          fr_type= ' '
c          write(*,'(2i2,1x,f5.2)') hour,min,sec
          yy=lsta*2.
          if (lresp) then
            call freq_event_type(raw_signal,current,f_sel,
     &        time,2.,yy,fr_type)
          else
            fr_type(1:1)='R' ! in case no response, assume detection is not noise or spike
          endif
          if (fr_type(1:1).ne.'N'.and..not.spike_flag) then
            flag=.true.
            phase_time(c)=time
            write(phases(c)(19:28),'(2i2,1x,f5.2)') hour,min,sec
c
c save fr_type for first phase only
c
            if (fr_type_save.eq.'0') fr_type_save=fr_type
          endif
c
c make sure no new trigger within nsta
c
          do j=i+1,nsta
            data(j)=0
          enddo
        elseif (flag.and.data(i).le.dtrle) then
          flag=.false.
        endif
      enddo
      fr_type=fr_type_save

      return
      end
        
      
      subroutine get_autosig_par(t)
c
c read definition file and store variables in common clock
c
      implicit none
      include 'libsei.inc' 
      include 'seidim.inc'
      include 'autosig.inc'
      include 'rea.inc'

      character*80 def_file         ! file names
      integer in                    ! file unit
      integer code                  ! return code from opening def file
      integer i                     ! counter
      character*80 line             ! text line
      real var                      ! value of variables
      integer t                     ! unit of trace file

c
c check for file end open
c

      def_file = 'autosig.par'

      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   def_file )       ! For this filename.

      if(code.ne.e_ok$) then
        write(*,*) ' definition file does not exist: autosig.par'
        stop   
      endif

      write(t,*) ' '
      write(t,*) ' settings from file: ' // def_file
      write(t,*) ' '
c
c init values
c
      lrmean=.FALSE.
      lrtr  =.FALSE.
      lchfu =.FALSE.
      kchfu =3.
      lrec  =.FALSE.
      lwavout = .FALSE.
      lstasq=.FALSE.
      lautocodasq=.FALSE.
      lautospec=.FALSE.
      lautophase=.FALSE.
      lautoamp=.FALSE.
      lclear=.FALSE.
      phase_mode=0.
      phase_infile=1.
      input_mode=0.
      spectrum_length_p=5.
      spectrum_length_s=10.
      spectrum_pre_length=1.
      spfmin=.01
      spfmax=20.
      par_vs=3.6
      par_vp=6.2
      par_q0=0.
      par_p_q0=0.
      par_s_q0=0.
      par_qalpha=0.
      par_p_qalpha=0.
      par_s_qalpha=0.
      par_kappa=0.
      dkappa=0.
      qcorner=0. ! q frequency dependent for all f
      par_density=2.8
      spec_overwrite=.false.
      phase_overwrite=.false.
      ga_pop_size=50
      ga_n_generations=150
      gvel_p(1)=6.5
      gvel_p(2)=8.0
      gvel_s(1)=3.0
      gvel_s(2)=3.7
      spec_dur_choice=0.
      dist_f_sel(1)=1.
      dist_f_sel(2)=10.
      spike_test=.false.
      search_method=2.
      grid_nloop=3
      ngrid_f=100
      ngrid_om=100
      lall_phases=.false.
      ffixl=0.
      ffixh=0.
      i=0

1010  continue

c
c read text line from file, check for code and set variables
c
      read(in,'(a)',end=1090,err=1080) line

      if (line(1:11).eq.'REMOVE MEAN') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lrmean=.TRUE.
          write(t,*) ' REMOVE MEAN = TRUE'
        endif
      elseif (line(1:12).eq.'REMOVE TREND') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lrtr= .TRUE.
          write(t,*) ' REMOVE TREND = TRUE'
        endif
      elseif (line(1:10).eq.'SPIKE TEST') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          spike_test= .TRUE.
          write(t,*) ' SPIKE TEST = TRUE'
        endif
      elseif (line(1:4).eq.'NORM') then
        read(line(41:50),'(f10.1)') norm_fit
      elseif (line(1:16).eq.'SEARCH ALGORITHM') then
        read(line(41:50),'(f10.1)') search_method
        if (search_method.eq.1.) then
          write(t,*) ' SEARCH ALGORITHM = GENETIC '
        elseif (search_method.eq.2.) then
          write(t,*) ' SEARCH ALGORITHM = GRID '
        endif
      elseif (line(1:10).eq.'CHAR FUNCT') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lchfu=.TRUE.
          write(t,*) ' CHAR FUNCT = TRUE'
        endif
      elseif (line(1:15).eq.'WRITE ALL PHASE') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lall_phases=.TRUE.
          write(t,*) ' WRITE ALL PHASE = TRUE'
        endif
      elseif (line(1:15).eq.'K IN CHAR FUNCT') then
        read(line(41:50),'(f10.1)') kchfu
        write(t,*) ' K IN CHAR FUNCT = ',kchfu
      elseif (line(1:15).eq.'STALTA NREC/REC') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lrec=.FALSE.
          write(t,*) ' STALTA NREC = TRUE'
        else
          lrec=.TRUE.
          write(t,*) ' STALTA REC = TRUE'
        endif
      elseif (line(1:17).eq.'STALTA SQUARE/ABS') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lstasq=.TRUE.
          write(t,*) ' STALTA SQUARE/ABS = TRUE'
        else
          lstasq=.FALSE.
          write(t,*) ' STALTA SQUARE/ABS = FALSE'
        endif
      elseif (line(1:19).eq.'AUTOCODA SQUARE/ABS') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lautocodasq=.TRUE.
          write(t,*) ' AUTOCODA SQUARE/ABS = TRUE'
        else
          lautocodasq=.FALSE.
          write(t,*) ' AUTOCODA SQUARE/ABS = FALSE'
        endif
      elseif (line(1:14).eq.'CREATE WAVEOUT') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lwavout=.TRUE.
          write(t,*) ' CREATE WAVEFORM OUT = TRUE'
        endif
      elseif (line(1:10).eq.'GRID NLOOP') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          grid_nloop=int(var)
          write(t,*) ' GRID NLOOP = ',grid_nloop   
        endif
      elseif (line(1:15).eq.'NGRID FREQUENCY') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          ngrid_f=int(var)
          write(t,*) ' NGRID FREQUENCY = ',ngrid_f   
        endif
      elseif (line(1:18).eq.'NGRID SPECTRAL AMP') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          ngrid_om=int(var)
          write(t,*) ' NGRID SPECTRAL AMP = ',ngrid_om  
        endif
      elseif (line(1:18).eq.'GA POPULATION SIZE') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          ga_pop_size=int(var)
          write(t,*) ' GA POPULATION SIZE = ',ga_pop_size
        endif
      elseif (line(1:14).eq.'GA GENERATIONS') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          ga_n_generations=int(var)
          write(t,*) ' GA GENERATIONS = ',ga_n_generations
        endif
      elseif (line(1:18).eq.'GROUP VEL WINDOW P') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          gvel_p(1)=var
        endif
        read(line(51:60),'(f10.1)') var
        if (var.ne.0.) then
          gvel_p(2)=var
        endif
        write(t,*) ' GROUP VEL WINDOW P ',gvel_p 
      elseif (line(1:18).eq.'GROUP VEL WINDOW S') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          gvel_s(1)=var
        endif
        read(line(51:60),'(f10.1)') var
        if (var.ne.0.) then
          gvel_s(2)=var
        endif
        write(t,*) ' GROUP VEL WINDOW S ',gvel_s 
      elseif (line(1:16).eq.'DIST FREQ SELECT') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          dist_f_sel(1)=var
        endif
        read(line(51:60),'(f10.1)') var
        if (var.ne.0.) then
          dist_f_sel(2)=var
        endif
        write(t,*) ' DIST FREQ SELECT = ',dist_f_sel
      elseif (line(1:17).eq.'FIXED FREQUENCIES') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          ffixl=var
        endif
        read(line(51:60),'(f10.1)') var
        if (var.ne.0.) then
          ffixh=var
        endif
        write(t,*) ' FIXED FREQUENCIES = ',ffixl,ffixh
      elseif (line(1:13).eq.'AUTO SPECTRUM') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lautospec=.TRUE.
          write(t,*) ' AUTO SPECTRUM = TRUE'
        endif
      elseif (line(1:14).eq.'AUTO AMPLITUDE') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lautoamp=.TRUE.
          write(t,*) ' AUTO AMPLITUDE = TRUE'
        endif
      elseif (line(1:10).eq.'AUTO PHASE') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          lautophase=.TRUE.
          write(t,*) ' AUTO PHASE = TRUE'
        endif
      elseif (line(1:14).eq.'SPEC OVERWRITE') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          spec_overwrite=.TRUE.
          write(t,*) ' SPEC OVERWRITE = TRUE'
        endif
      elseif (line(1:15).eq.'PHASE OVERWRITE') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          phase_overwrite=.TRUE.
          write(t,*) ' PHASE OVERWRITE = TRUE'
        endif
      elseif (line(1:12).eq.'SELECT PHASE') then
        read(line(41:50),'(f10.1)') phase_mode
        write(t,*) ' SELECT PHASE 1 = ',phase_mode
        if (phase_mode.eq.1.or.phase_mode.eq.2) then
          read(line(51:60),'(f10.1)') phase_infile
        endif
        write(t,*) ' SELECT PHASE 2 = ',phase_infile
      elseif (line(1:17).eq.'SPECTRUM P LENGTH') then
        read(line(41:50),'(f10.1)') spectrum_length_p
        write(t,*) ' SPECTRUM P LENGTH = ',spectrum_length_p
      elseif (line(1:17).eq.'SPECTRUM S LENGTH') then
        read(line(41:50),'(f10.1)') spectrum_length_s
        write(t,*) ' SPECTRUM S LENGTH = ',spectrum_length_s
      elseif (line(1:19).eq.'SPECTRUM PRE LENGTH') then
        read(line(41:50),'(f10.1)') spectrum_pre_length
        write(t,*) ' SPECTRUM PRE LENGTH = ',spectrum_pre_length
      elseif (line(1:14).eq.'SEPCTRUM F LOW') then
        read(line(41:50),'(f10.1)') spfmin
        write(t,*) ' SEPCTRUM F LOW = ',spfmin
      elseif (line(1:15).eq.'SEPCTRUM F HIGH') then
        read(line(41:50),'(f10.1)') spfmax
        write(t,*) ' SEPCTRUM F HIGH = ',spfmax
      elseif (line(1:19).eq.'SPECDURATION CHOICE') then
        read(line(41:50),'(f10.1)') spec_dur_choice
        write(t,*) ' SPECDURATION CHOICE = ',spec_dur_choice
      elseif (line(1:13).eq.'SPECTRAL S-Q0') then
        read(line(41:50),'(f10.1)') par_s_q0
        write(t,*) ' SPECTRAL S-Q0 = ',par_s_q0
      elseif (line(1:13).eq.'SPECTRAL P-Q0') then
        read(line(41:50),'(f10.1)') par_p_q0
        write(t,*) ' SPECTRAL P-Q0 = ',par_p_q0
      elseif (line(1:14).eq.'SPECTRAL KAPPA') then
        read(line(41:50),'(f10.1)') par_kappa
        write(t,*) ' KAPPA = ',par_kappa
        read(line(51:60),'(f10.1)') dkappa
        write(t,*) ' DELTA KAPPA = ',dkappa
      elseif (line(1:16).eq.'SPECTRAL QCORNER') then
        read(line(41:50),'(f10.1)') qcorner   
        write(t,*) ' Q CORNER = ',qcorner
      elseif (line(1:17).eq.'SPECTRAL P-QALPHA') then
        read(line(41:50),'(f10.1)') par_p_qalpha
        write(t,*) ' SPECTRAL P-QALPHA = ',par_p_qalpha
      elseif (line(1:17).eq.'SPECTRAL S-QALPHA') then
        read(line(41:50),'(f10.1)') par_s_qalpha
        write(t,*) ' SPECTRAL S-QALPHA = ',par_s_qalpha
      elseif (line(1:19).eq.'SPECTRAL P-VELOCITY') then
        read(line(41:50),'(f10.1)') par_vp
        write(t,*) ' SPECTRAL P-VELOCITY = ',par_vp
      elseif (line(1:19).eq.'SPECTRAL S-VELOCITY') then
        read(line(41:50),'(f10.1)') par_vs
        write(t,*) ' SPECTRAL S-VELOCITY = ',par_vs
      elseif (line(1:16).eq.'SPECTRAL DENSITY') then
        if (line(41:50).ne.'          ') then
          read(line(41:50),'(f10.1)') par_density
        endif
        write(t,*) ' SPECTRAL DENSITY = ',par_density

c
c if station
c
      elseif (line(1:7).eq.'STATION') then
        i=i+1
cTATION MOL   S  Z    3.0   20.0   50.0   10.0    5.0  5.0 10.0
        read(line(9:63),'(a5,1x,a4,5(1x,f6.1),2(1x,f4.1))',
     &    err=1070)
     &    par_stat(i),par_comp(i),par_sta(i),par_lta(i),
     &    par_ratio(i),par_mincoda(i),par_dtrle(i),par_fill(i),
     &    par_filh(i)
        write(t,*) ' STATION ' // par_stat(i),' ',par_comp(i)

      endif
     
      goto 1010
1070  continue
      write(*,*) 'error in line : ',line(1:62)
      goto 1010

1080  continue
      write(*,*) ' Error in autosig.par file'

1090  continue

      par_nstat=i
      write(t,*) ' number of stations in autosig.par: ',par_nstat
      call sei close( close$, in, code )

      return
      end 


      subroutine get_station(sname,cname,sta,lta,ratio,
     &      mincoda,dtrle,fill,filh,code)
c
c get individual station/component parameters from array
c
c input:  sname  - station name
c         cname  - component name
c
c output: sta    - length of sta window
c         lta    - length of lta window
c         ratio  - trigger ratio
c         mincoda - minimum coda
c         dtrle   - detrigger level for reading autocoda
c         fill    - lower filter limit for autocoda
c         filh    - upper filter limit for autocoda
c         code    - 1 if parameters for station found in array
c

      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'autosig.inc'
      include 'rea.inc'
      character*(*) sname
      character*(*) cname
      real sta,lta,ratio,mincoda,dtrle,fill,filh
      integer code,i
      logical flag

      code = 0
      flag = .FALSE.

      do i=1,par_nstat
        if (.NOT.flag.and.sname.eq.par_stat(i).and.
     &                    cname.eq.par_comp(i)) then
          flag = .TRUE.
          code = 1
          sta=par_sta(i)
          lta=par_lta(i)
          ratio=par_ratio(i)
          mincoda=par_mincoda(i)
          dtrle=par_dtrle(i)
          fill=par_fill(i)
          filh=par_filh(i)
        endif
      enddo

      return
      end
        
      subroutine find_coda(data,nlen,rate,start,lsta,dtrle,ptime,coda,
     &       lsq)
c
c routine to find signal duration (coda) in seismic trace, data are 
c assumed to be filtered, start from p time, lta level is calculated from
c signal (noise) before the p time, the ratio is computed from the squared
c sta divided by the lta, coda is set when detrigger level found if value 
c below dtrle
c

      implicit none

      include 'seidim.inc'

      real data(max_sample),raw_signal(max_sample)

      integer nlen
      double precision rate
      integer coda
      double precision start,ptime
      integer c                                   ! counter
      integer p_sample
      real dtrle
      logical flag
      integer last
      real lta,sta,lsta
      integer nsta
      real rnsta
      logical lsq
 
      coda = -1
      last = 0
      flag=.FALSE.

      rnsta=lsta*rate
      nsta=int(rnsta)

      if (ptime.eq.-1) return

c
c calculate sample of P-phase
c
      p_sample= int((ptime-start) * rate)
      if (p_sample.le.150) return

c
c get noise level
c
      lta=0
      do c=1,p_sample-nsta
        if (lsq) then
          lta = lta + data(c) * data(c)
        else
          lta = lta + abs(data(c))
        endif
      enddo

      lta= lta / (p_sample-nsta)

c
c get start sta
c
      sta=0
      do c=p_sample+1,p_sample+nsta
        if (lsq) then
          sta=sta+data(c)*data(c)
        else
          sta=sta+ abs(data(c))
        endif
      enddo
      sta = sta / nsta

      do c=p_sample+nsta+1,nlen
        if (lsq) then
          sta=sta+(data(c)*data(c)-data(c-nsta)*data(c-nsta))
     *          /rnsta
        else
          sta=sta+(abs(data(c))-abs(data(c-nsta)))/rnsta
        endif
        raw_signal(c)=sta/lta

        if (.not.flag.and.sta/lta.lt.dtrle) then
           flag = .TRUE.
           last = c
        endif
      enddo   

      if (.not.flag.and.sta/lta.gt.dtrle) then
        flag = .TRUE.
        last = nlen
      endif

      c=last-p_sample+1
      if (flag) coda=int(c/rate)

c      do c=1,p_sample+nsta
c        data(c)=lta
c      enddo
c      do c=p_sample+nsta+1,nlen
c        data(c)=raw_signal(c)
c      enddo

      return
      end

      subroutine extract_signal(yy,nsamp,start_time,rate,
     &    time,pre,post,sig,cnt)
c
c extract part of signal in time window time-pre -> time+post 
c
      implicit none
      real yy(*)         ! input signal
      real sig(*)        ! output signal
      integer nsamp
      double precision time,start_time
      real rate
      real pre,post
      integer i,cnt

      cnt=0
      do i=1,nsamp
        if (start_time+i/rate.le.time+post
     &    .and.start_time+i/rate.gt.time-pre)
     &       then
          cnt=cnt+1
          sig(cnt)=yy(i)
        endif
      enddo
      call rmean(sig,cnt)
      return
      end

      character*(*) function which_phase(phase_mode)
      implicit none
      character*1 xx
      real phase_mode
      xx='P'
      if (phase_mode.eq.2..or.phase_mode.eq.4.) then
        xx='S'
      endif
      which_phase=xx
      end

      subroutine get_amp_per(current,type,yy,start,duration,
     &    amp,per,time_amp)
c
c determine auto amplitude
c
      implicit none
      include 'libsei.inc'    ! Seisan includes
      include 'seidim.inc'    ! ---------------
      include 'seisan.inc'    ! ---------------
      include 'waveform.inc'  ! ---------------
      real yy(*),sig(max_sample)
      character*(*) type
      complex y_com(max_sample)
      real y1,y2
      integer i1,i2
      double precision start,time_amp
      real duration,amp,per
      real f(4)
      integer cnt
      integer current
c
c   poles and zeros, not used
c
      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros

      npole=0
      nzero=0
      amp=0.
      per=0.
      if (wav_resp_status.eq.'9') then
        write(*,*) ' no response for: ' //wav_stat(current)//' '//
     &       wav_comp(current)
        return
      endif
c
c  calculate corrected trace
c
      if (type(1:2).eq.'WA') then   ! Wood Anderson
c        f(1)=.8
        f(1)=.5
        f(2)=20.
        f(3)=8.
        f(4)=8.
  
      else
        return
      endif
c
c extract signal
c
      call extract_signal(yy,wav_nsamp(current),wav_abs_time(current),
     &    wav_rate(current),start,1.,duration,sig,cnt)

c
c convert signal to displacement
c
      call remove_resp(sig,y_com,cnt,wav_rate(current),1,
     *  f(1),f(2),int(f(3)),int(f(4)),zero,pole,nzero,npole,
     * norm) 
c
c   calculate maximum amplitude
c
      call auto_amp(sig,cnt,wav_rate(current),
     &     1.0,10.0,3,y1,y2,i1,i2)

      if(i1.eq.0) return    ! no amplitude read

c
c   reading ok, calculate amp and period and absolute start time
c
      amp=abs(y2-y1)/2.0
      per=2.0*abs(i2-i1)/wav_rate(current)
      time_amp=start+i1/wav_rate(current)

      return
      end

c--------------------------------------------------------------------------------------

      subroutine get_om_f0(yy,current,a,b,time,
     &  pre,post,om,f0,ka,omi,f0i,orig_time,
     &  q0,qalpha,qcorner,kappa,dkappa,
     &  pop_size,generations,
     &  ngrid_f,ngrid_om,grid_nloop,
     &  res_m_a,search_method,norm,
     &  ffixl,ffixh,fsn1,fsn2)
c
c automatically find spectral parameters om and f0 by using genetic algorithm,
c the process is done in xx steps: 
c
c   1) extract signal and noise time series of given start time and
c      duration, and get spectra using spec_values
c   2) find lowest frequency at which noise is half the max between signal
c      and noise
c   3) find initial om and f0, not used at the moment
c   4) call ga_om_f0 which is the genetic algorithm
c
c Lars Ottemoeller, March 2001
c
c input: yy - data vector
c        current - index of current waveform channel
c        a and b - lower and higher frequency limits
c        time - phase arrival, absolute time in seconds, phase for spectrum
c        pre - pre time in seconds, time before pahse to use
c        post - post time in seconds or duration after phase
c        orig_time - abs origin time in seconds
c        q0,qalpha,kappa, - parameters needed to correct for Q
c        dkappa - used in grid search +/- kappa, if kappa ne 0
c        pop_size - size of population in genetic algorithm
c        generations - number of generations to be done
c        search_method - 1=GA, 2=GRID
c        norm
c        ffixl,ffixh - fixed frequency range
c
c outout: fmin,fmax
c         om - log spectral displacement flat level
c         f0 - corner frequency
c         ka - delta kappa
c         omi,f0i - inital values of om and f0, as input assumed to be manual
c         fsn1,fsn2 - frequency range with good sn
c
      implicit none
      include 'libsei.inc'    ! Seisan includes
      include 'seidim.inc'    ! ---------------
      include 'seisan.inc'    ! ---------------
      include 'waveform.inc'  ! ---------------
c      include 'autosig.inc'   ! ---------------
c      include 'rea.inc'       ! ---------------
      integer current         ! current index to wav structure
      real yy(*)              ! data vector
      real a,b                ! min and max frequencies
      real om,f0,omi,f0i
      real omi_save,f0i_save
      real f_low,f_high       ! low and high limit for spectrum
      integer maxl            ! dimension of spectral levels to be determined
      parameter (maxl=100)
      real level_signal(maxl),level_noise(maxl)  ! spectral levels for signal and noise
      real farray(maxl)       ! frequency array
      real max                ! max spec level
      real norm
      real fsn1,fsn2          ! frequency range with good s/n
      real ffixl,ffixh
      complex y_com(max_sample) ! complex spectrum
      real log_spec(max_sample)  ! log10 of amplitude spectrum of signal
      real y_full(max_sample) ! log10 of amplitude spectrum of signal
      real ff(max_sample)     ! frequncy array
      integer nff             ! number of elements in ff
      real sig(max_sample)    ! temp signal vector
      real search_method      ! 1=ga, 2=grid
      integer i,j,cnt,c,k      ! counters
      integer nsmooth         ! number of times for smooth
      double precision time   ! travel time
      double precision orig_time ! origin time
      real pre,post           ! pre and posrt time interval in seconds
      real xx                ! some value
      integer ind,min_ind,max_ind
      integer ipow
      real dist,depth
      real trtime   
      character*1 phase
      real q0,qalpha,kappa,vp,vs,density,qcorner,dkappa,ka
      real geom_corr
      integer pop_size,generations
      integer ngrid_f,ngrid_om,grid_nloop
      integer seiclen
      character*80 outfile
      real res,res_m_a(*)
      integer year,month,day,doy,hour,min
      real sec
      real pi
      parameter (pi=3.141592654)
      real eval_q
   
      integer          YR1,MN1,DA1,HR1,MI1,ISC1
      integer          YR2,MN2,DA2,HR2,MI2,ISC2
      
      character*12 time1
      character*14 time2
c
c   poles and zeros, not used
c
      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm1                   ! normalization constant for poles and zeros

      npole=0
      nzero=0
c
c set initial values
c
      nsmooth = 0         ! no smoothing
      f0 = 0.
      om = 0.
      fsn1=0.0
      fsn2=0.0
c
c manual readings can be passed through f0i and omi
c
      f0i_save=f0i
      omi_save=omi
      f0i=0.
      omi=0.
      trtime=time-orig_time
      call sectim(orig_time,year,doy,month,day,hour,min,sec)

      if (wav_resp_status.eq.'9') then
        write(*,*) ' no response for: ' //wav_stat(current)//' '//
     &       wav_comp(current)
        return
      endif

      if (time.eq.-1) return
c
c init values
c
      f_low=0.001                  ! these are the lower and upper limit
      f_high=wav_rate(current)/2.  !    used in the computation of the spectra
c
c set up frequency array, use log equidistant spacing
c
c make sure low limit is higher than actual frequency content in signal
c
      if(2.0/(pre+post).gt.a) then
        farray(1)=2.0/(pre+post)  ! jh added 
      else
        farray(1)=a
      endif

      farray(maxl)=b
      do i=2,maxl-1
        farray(i)=log10(farray(1))+
     &    (log10(farray(maxl))-log10(farray(1)))/(maxl-1)*(i-1)
        farray(i)=10**farray(i)
      enddo
 
c
c signal
c
      call extract_signal(yy,wav_nsamp(current),wav_abs_time(current),
     &    wav_rate(current),time,pre,post,sig,cnt)
c
c write out signal
c
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.wave'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
c      do i=1,cnt
c        write(27,*) dist/(trtime+i/wav_rate(current)),sig(i)
c      enddo
c      close(27)
c
c   calculate spectrum and get spectrum descete values
c
      call spec_value(current,sig,cnt,nsmooth,
     &    f_low,f_high,1,farray,maxl,0,level_signal,y_com,ipow,
     *     zero,pole,nzero,npole,norm1)
c
c save full signal spectrum, also corrected for attenuation
c
      c = 0
      do i = 2,((ipow/2) + 1)
        c = c + 1
        ff(c) = (i-1)*wav_rate(current)/ipow
        y_full(c) = (1/wav_rate(current)**2)*
     &     y_com(i)*conjg(y_com(i))
        y_full(c) = sqrt (y_full(c)) 

        xx=1.
        if (q0.gt.0.) then
          xx=xx*
     &      exp(-pi*ff(c)*trtime /
     &      eval_q(q0,ff(c),qalpha,qcorner))
        endif

        if (kappa.ne.0.) then
          xx=xx*exp(-pi*kappa*ff(c))
        endif
        y_full(c) = log10(y_full(c)/xx)

      enddo
      nff=c

c
c noise
c

c
c set noise window, after wav_abs_time
c
      xx=post+pre
      if (wav_abs_time(current)+xx.ge.time) then
        xx=time-pre-wav_abs_time(current) 
      endif

      call extract_signal(yy,wav_nsamp(current),wav_abs_time(current),
     &    wav_rate(current),wav_abs_time(current),0.,xx,sig,cnt)
c
c   get corrected smmothed spectrum at descrete values
c
      call spec_value(current,sig,cnt,nsmooth,
     &    f_low,f_high,1,farray,maxl,0,level_noise,y_com,ipow,
     *     zero,pole,nzero,npole,norm1)

      do i=1,maxl
        xx=1.
        if (q0.gt.0.) then
c correct for Q
            xx=xx*exp(-pi*farray(i)*trtime/
     &         eval_q(q0,farray(i),qalpha,qcorner))
        endif

        if (kappa.ne.0.) then
c correct for kappa, near surface attenuation
          xx=xx*exp(-pi*kappa*farray(i))
        endif

        level_signal(i)=level_signal(i)/xx

        level_noise(i)=level_noise(i)/xx  ! jh added 2013
c
c take log10 and put into log_spec
c
        log_spec(i)=log10(level_signal(i))
      enddo
c
c write out date
c
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.date'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
      write(27,'(i4,2(i2),1x,3(i2))') year,month,day,hour,min,int(sec)
      write(27,'(i5)') int(dist)
      close(27)

c
c write out noise spectrum descete values 
c
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.noise'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
      do i=1,maxl
        write(27,*) farray(i),log10(level_noise(i))
      enddo
      close(27)

c
c find max difference between signal and noise, and return if signal is not
c more than twice the noise at the maximum
c
      max = 0.
      do i=1,maxl
        xx=log10(level_signal(i))-log10(level_noise(i))
        if (xx.gt.max) max = xx
      enddo

      if (max.le..4) then
        write(*,*) ' ******* Too low S/N ******* log max S/N is: ',max
        return
      endif
c
c find frequency with minimum spec level, assume to be highest 
c frequency to use
c
      xx=99.
      ind=0
      do i=1,maxl
c        write(*,*) i,farray(i),log10(level_signal(i))
        if (log10(level_signal(i)).lt.xx) then
          ind=i
          xx=log10(level_signal(i))
        endif
      enddo
      if (ind.eq.0) then
        write(*,*) ' not finding spectral minimum '
        return
      endif
c
c   ind must be at least 2
c
      if(ind.eq.1) ind=2   ! fix for next test jh aug 29-08, 2014

      max_ind=ind   ! max frequency
c
c  but this could be a bit primitive since s/n could be low
c  so check more, jh march 2014
c  s/n must not be less than 2 for more than 2 points
c
      k=0
      do i=max_ind/2,max_ind
        xx=log10(level_signal(i))-log10(level_noise(i))
        if(xx.lt.0.3) then 
           k=k+1
        else
           k=k-1
           if(k.lt.0)k=0
        endif
        if(k.gt.2) then
           max_ind=i   ! found low limit, jump out
           goto 666
        endif
      enddo
 666  continue        
c
c find lowest frequency at which signal is half 
c the max dif of signal and noise,(variable max),
c after which at least once (5 times ?)
c the diff between the noise and signal has 
c been less than half the max
c
      ind = 0
      cnt = 0

      do i=1,maxl
        xx=log10(level_signal(i))-log10(level_noise(i))-max/2.
        if ((xx.gt.0.
c also 1 log unit difference is good enough, lot 22/02/2009
     &    .or.log10(level_signal(i))-log10(level_noise(i)).gt.1.) 
     &    .and.ind.eq.0) then
          ind = i              ! first frequency with good s/n
          cnt=cnt+1
        elseif (ind.ne.0.and.(xx.gt.0.0 
     &    .or.log10(level_signal(i))-log10(level_noise(i)).gt.1.)) then ! jh add 4 1013
          cnt=cnt+1            ! first already found, count
        endif
c
c   if within the next 5 amplitudes there is not at least 1 value with good
c   s/n, reset the counter for good lower frequency. this will start testing
c   from the current frequency
c   jh added apr 2013
c
        if(i-ind.gt.5.and.cnt.lt.2) then
           ind=0
           cnt=0
        endif
      enddo
c
c reset if signal close to noise, factor 2
c
c change lot 05/2006
        if (log10(level_signal(ind))-log10(level_noise(ind)).lt..3.and.
     &    ind.lt.max_ind) then
          ind=0
        endif

      if (ind.le.0.or.cnt.lt.5) then
        write(*,'(a)') ' Difference between signal and noise does ' //
     &      'not allow to find frequency window'
        return
      endif
c
c check at what smaller frequency, the level is lower than at ind
c added lo, June 2, 2001, due to not using low f with DECP data set
c
      min_ind=ind   ! min frequency

cjh      ind=0
cjh      do i=1,min_ind-1
cjh        if (log10(level_signal(i)).lt.log10(level_signal(min_ind)).and.
cjh     &    ind.eq.0) then
cjh          ind=i
cjh        endif
cjh      enddo
cjh      if (ind.ne.0) min_ind=ind

c
c case of fixed frequency interval
c
      if (ffixl.ne.0..and.ffixh.ne.0.) then
        max_ind=0
        min_ind=0
        do i=1,maxl
          if (farray(i).ge.ffixl.and.min_ind.eq.0) min_ind=i 
        enddo
        do i=maxl,1,-1
          if (farray(i).le.ffixh.and.max_ind.eq.0) max_ind=i 
        enddo
          write(*,*) farray(min_ind),farray(max_ind)
      endif
c
c  initial om from level
c
      om = log_spec(min_ind)

c
c if max_ind smaller than min_ind
c
      if (min_ind.gt.max_ind) max_ind=maxl
c
c check that f window large enough
c
c changed .1 to .3, lot 070702
c      if (log10(farray(max_ind))-log10(farray(min_ind)).lt..1) then

      if (log10(farray(max_ind))-log10(farray(min_ind)).lt..3) then
        write(6,'(a,f7.2,a,f7.2)') 
     *  ' Frequency window smaller than a factor of 2   fmin=',
     *  farray(min_ind),'  fmax=', farray(max_ind)
        return
      endif
c
c reduce frequency window to 3 log units, added 07/07/2002
c
      ind=0
      if (log10(farray(max_ind))-log10(farray(min_ind)).gt.3.) then
        do i=min_ind+1,max_ind
          if (log10(farray(i))-log10(farray(min_ind)).gt.3) then
            if (ind.eq.0) then
              ind=i
              write(*,*) ' new fmax: ',farray(ind)
            endif
          endif
        enddo
        if (ind.ne.0) max_ind=ind
      endif


c
c check that average residual between signal and noise is large in f range
c
      res = 0.
      do i=min_ind,max_ind
        res=res+1/(float(max_ind-min_ind+1)) * 
     &    (log10(level_signal(i))-log10(level_noise(i)))
c      write(*,*) farray(i),log10(level_signal(i)),log10(level_noise(i)),
c     & res
      enddo
      if (res.le..2) then
c         write(*,*) ' min_ind,max_ind,res ',min_ind,max_ind,res
        write(*,*) ' *************** Bad S/N ************* '
        return
      endif

      write(*,'(a,f8.2,a,f8.2,a)') ' ****** signal between: ',
     *farray(min_ind),' and ',farray(max_ind),' Hz '

      fsn1=farray(min_ind)
      fsn2=farray(max_ind)

c
c find frequency after which average does not go up again
c
      j=0
      xx=0.
      ind=0
c
c  avrage of first 5 descrete values
c
      do i=min_ind,min_ind+5
        xx=xx+log10(level_signal(i))
      enddo
      xx=xx/5.

      ind=min_ind
      do i=min_ind,maxl
c        if (log10(level_signal(i)).gt.xx) then
          if (log10(level_signal(i)).gt.xx*0.7) then   ! jh change
          ind=i
        endif
      enddo
        
c        j=j+1
c        xx=xx+log10(level_signal(i))
c        if (log10(level_signal(i)).lt.log10(level_signal(min_ind))-.2
c     &      .and.ind.eq.0) then
c          ind = i
c        endif
c        if (log10(level_signal(i)).gt.log10(level_signal(min_ind))-.2
c     &      .and.ind.ne.0) then
c          ind = 0
c        endif
c      enddo
c
c   initial corner frequency
c
      f0 = farray(ind)
      f0i = f0
        
c
c find initial flat level as average between min_ind and f0
c
      om=0.
      cnt=0
      do i=min_ind,ind
        cnt=cnt+1
        om = om + log10(level_signal(i)) 
c        write(*,*) ' xxx ',i,farray(i),log10(level_signal(i)),om
      enddo
      om = om / float(cnt)
      omi=om
      write(*,'(a,f6.2,a,f8.2)') ' *** initial om = ',om,' f0 = ',f0

c
c call genetic algorithm to serach for om and f0
c
c      write(*,*) ' call grid ',om,f0,farray(min_ind),farray(max_ind)

      if (search_method.eq.1.) then
        call ga_om_fo(current,log_spec,farray,maxl,farray(min_ind),
     &         farray(max_ind),om,f0,pop_size,
     &         generations,norm)
      elseif (search_method.eq.2.) then
        if (kappa.eq.0.) dkappa=0. ! dont use dkappa if kappa=0.
        call grid_om_fo(current,log_spec,farray,maxl,farray(min_ind),
     &         farray(max_ind),om,f0,ka,
     &         ngrid_f,ngrid_om,grid_nloop,norm,dkappa)
      endif

c
c write out amp spectrum
c moved lot 22/02/2009
c
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.full'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
      do i=1,nff
        write(27,*) ff(i),y_full(i)+ka*pi*ff(i)
      enddo
      close(27)
c
c get comparison of residuals
c
      if (omi_save.gt.0.) then
        call spec_residual(omi_save,f0i_save,log_spec,farray,maxl,
     &      farray(min_ind),farray(max_ind),res_m_a(1),norm)
      endif
c
c get residual for obtained values f0 and om, dkappa was applied in grid_om_f0
c
        call spec_residual(om,f0,log_spec,farray,maxl,
     &      farray(min_ind),farray(max_ind),res_m_a(2),norm)

      return
      end


      subroutine spec_residual(om,f0,log_spec,farray,nfreq,fmin,
     &      fmax,res,norm)
      implicit none
      character*24 dec_to_bin
      real eval_spec_fit,eval_spec_fit1
      real om,f0,res
      real bin_to_dec
      real log_spec(*),farray(*)
      integer nfreq
      real fmin,fmax
      real norm
      real k

      integer lj1,lj2
      parameter (lj1=8)
      parameter (lj2=9)
      integer j1(lj1),j2(lj2)
      character*17 a

      k=0.

      a=' '
      a(1:lj1)=dec_to_bin(om/20.*(2.**float(lj1)-1.),lj1)
      a(lj1+1:lj1+lj2)=dec_to_bin(f0/40.*(2.**float(lj2)-1.),lj2)

      call text_to_array(a(1:lj1),j1,lj1)
      call text_to_array(a(lj1+1:lj1+lj2),j2,lj2)
c      res = -eval_spec_fit(log_spec,farray,nfreq,fmin,fmax,j1,
c     &        j2,lj1,lj2,norm) + 30.

      res= -eval_spec_fit1(log_spec,farray,nfreq,fmin,
     &       fmax,f0,om,k,norm)+30.
c      res = -eval_spec_fit(log_spec,farray,nfreq,fmin,fmax,j1,
c     &        j2,lj1,lj2,norm)-30. 

      return
      end

      subroutine ga_om_fo(current,log_spec,farray,nfreq,fmin,fmax,om,f0,
     &   pop_size,nloop,norm)
c
c subroutine to fit spectrum by Brune model, parameters are flat 
c level and corner frequency, the genetic algorithm follows the most
c simple case presented in Genetic Algor. + Data Structures = Evol. Programs,
c Zbigniew Michalewicz, 1992, ISBN 0-387-55387-8
c
c Lars Ottemoeller, March 2001
c
c input: 
c        current - current waveform channel, index to waveform common block
c        log_spec - signal
c        farray - array with frequency values
c        nfreq - number of frequencies in farray
c        fmin - min f to use
c        fmax - max --------
c        norm - norm fit
c
c output:
c        om - spectral flat level in log10 units
c        fo - corner frequency
c

      implicit none
      include 'libsei.inc'    ! Seisan includes
      include 'seidim.inc'    ! ---------------
      include 'waveform.inc'  ! ---------------
      integer seiclen

      integer current         ! current index to wav structure
      real bin_to_dec
      real log_spec(*),farray(*),y_brune(2048)
      integer nfreq
      real fmin,fmax
      real f0,om
      real norm

      integer nloop
      integer pop_size,max_pop_size
      integer lj1,lj2
      parameter (lj1=8)
      parameter (lj2=9)
      parameter (max_pop_size=300)
      integer i,j,j1(lj1),j2(lj2),loop,nc,y(max_pop_size),gen
      character*17 v(max_pop_size),v_new(max_pop_size),a
      real result(max_pop_size),pi(max_pop_size),qi(max_pop_size)
      real x
      real pc,pm
      parameter (pc=0.25)
      parameter (pm=0.01)
      real eval_spec_fit
      real omax(5)
      real fitness
      real rand
      character*24 dec_to_bin
      character*80 outfile
      real ka
 
      ka=0.

      x=rand(1)
      gen=0
      open(26,file='ga.out',status='unknown')
c
c init population, set half the population to initial values
c
      a=' '
      a(1:lj1)=dec_to_bin(om/20.*(2.**float(lj1)-1.),lj1)
      a(lj1+1:lj1+lj2)=dec_to_bin(f0/40.*(2.**float(lj2)-1.),lj2)
      do i=1,pop_size
        v(i)=' '
      enddo

      do i=2,pop_size-1
        do j=1,lj1+lj2
          write(v(i)(j:j),'(i1)') int(rand(0)+.5)
        enddo
c        v(i+1)=a
c        write(*,*) i,' ',v(i)
c        write(*,*) i+1,' ',v(i+1)
      enddo
      v(1)=a
      v(pop_size)=a

      do i=1,5
        omax(i)=-999.
      enddo
      do loop=1,nloop
        gen=gen+1
c        write(*,*) ' Generation ',gen
c
c evaluate population
c
        omax(5)=-999.
        do i=1,pop_size
          call text_to_array(v(i)(1:lj1),j1,lj1)
          call text_to_array(v(i)(lj1+1:lj1+lj2),j2,lj2)
          result(i) = eval_spec_fit(log_spec,farray,nfreq,fmin,fmax,j1,
     &        j2,lj1,lj2,norm)
c          write(222,*)i,-5.+bin_to_dec(j1,lj1)*20./(2.**float(lj1)-1.),
c     &      bin_to_dec(j2,lj2) *40./(2.**float(lj2)-1.),
c     &      result(i)

c commented out lo 22/7/2008
c          if (bin_to_dec(j2,lj2) *40./(2.**float(lj2)-1.).gt.0.and.
c     &      result(i).lt.30.)
c     &    write(87,*) 
c     &      bin_to_dec(j2,lj2) *40./(2.**float(lj2)-1.),
c     &      -5.+bin_to_dec(j1,lj1) *20./(2.**float(lj1)-1.),
c     &      30.-result(i)

c
c find overall maximum
c
          if (result(i).gt.omax(5)) then
            omax(5)=result(i)
          endif
          if (result(i).gt.omax(1)) then
            omax(1)=result(i)
            omax(2)=-5.+bin_to_dec(j1,lj1) *20./(2.**float(lj1)-1.)
            omax(3)=bin_to_dec(j2,lj2) *40./(2.**float(lj2)-1.)
            omax(4)=float(loop)
          endif
        enddo

c
c compute fitness
c
        fitness=0.
        do i=1,pop_size
          fitness=result(i)+fitness
        enddo
        write(26,*) ' Generation ',loop,' Fitness ',fitness,
     &       fitness/pop_size,omax(5)

c
c compute individual probablities
c
        do i=1,pop_size
          pi(i)=result(i)/fitness
        enddo

c
c compute cumulative probablities
c
        do i=1,pop_size
          qi(i)=0.
          do j=1,i
            qi(i)=qi(i)+pi(j)
          enddo
        enddo

c
c spin selection wheel
c
        do i=1,pop_size
          x=rand(0)
          v_new(i)=' '
          if (x.le.qi(1)) then
            v_new(i)=v(1)
          else
            do j=2,pop_size
              if (qi(j-1).lt.x.and.x.le.qi(j).and.v_new(i).eq.' ') then
                v_new(i)=v(j)
              endif
            enddo
          endif
        enddo
c        do i=1,pop_size
c          write(*,*) i,' ',v_new(i)
c        enddo

c
c crossover
c
        nc=0
        do i=1,pop_size
          x=rand(0) 
          if (x.le.pc) then
            nc=nc+1
            y(nc)=i
          endif
        enddo
        if (int(nc/2.).ne.(nc/2.)) nc=nc-1
        do i=1,nc,2
          j=int(rand(0)*float(lj1+lj2)+1.)
c          write(*,*) ' crossover ',y(i),y(i+1),j
          a=v_new(y(i))
          v_new(y(i))(j:lj1+lj2) = v_new(y(i+1))(j:lj1+lj2)
          v_new(y(i+1))(j:lj1+lj2) = a(j:lj1+lj2)
        enddo

c
c mutation
c
        do i=1,pop_size
          do j=1,lj1+lj2
            x=rand(0) 
            if (x.le.pm) then
c              write(*,*) ' changing ',i,j
              if (v_new(i)(j:j).eq.'1') then
                v_new(i)(j:j)='0'
              else
                v_new(i)(j:j)='1'
              endif
            endif
          enddo
        enddo

c
c copy v_new into v array
c
        do i=1,pop_size
          v(i)=v_new(i)
        enddo
      enddo
c
c write out result
c
      x=0.
      do i=1,pop_size
        call text_to_array(v(i)(1:lj1),j1,lj1)
        call text_to_array(v(i)(lj1+1:lj1+lj2),j2,lj2)
        result(i) = eval_spec_fit(log_spec,farray,nfreq,fmin,fmax,j1,j2,
     &        lj1,lj2,norm)

c        write(*,*) i,result(i),
c     &    -5.+bin_to_dec(j1,lj1)*20./(2.**float(lj1)-1.),
c     &    bin_to_dec(j2,lj2)*40./(2.**float(lj2)-1.)

c
c take average of last population
c
c         x=x+1.
c         om=om-5.+bin_to_dec(j1,lj1)*20./(2.**float(lj1)-1.) 
c         f0=f0+bin_to_dec(j2,lj2)*40./(2.**float(lj2)-1.)

        if (result(i).gt.x) then
          x=result(i)
          om=-5.+bin_to_dec(j1,lj1)*20./(2.**float(lj1)-1.)
          f0=bin_to_dec(j2,lj2)*40./(2.**float(lj2)-1.)
        endif
      enddo
      write(*,*) ' last generattion best om,f0 ',om,f0
c
c use best ever solution
c
      om=omax(2)
      f0=omax(3)

c      write(300,*) ' station '//wav_stat(current),wav_comp(current)
c      write(300,*) ' last generattion best om,f0 ',om,f0
      write(*,*) ' best ever solution ',om,f0
      write(*,*) ' result/generation ',omax(1),omax(4)
      close(26)
c
c write out log spectral amplitude
c
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.obs'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
      do i=1,nfreq
        write(27,*) farray(i),log_spec(i)
      enddo
      close(27)

      call compute_brune(om,f0,farray,nfreq,y_brune)
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.synth'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
      do i=1,nfreq
        if (farray(i).ge.fmin.and.farray(i).le.fmax) 
     &    write(27,*) farray(i),y_brune(i)
      enddo
      close(27)

      return
      end

      subroutine grid_om_fo(current,log_spec,farray,nfreq,fmin,fmax,
     &   om,f0,ka,ngrid_f,ngrid_om,nloop,norm,dkappa)
c
c subroutine to fit spectrum by Brune model applying simple grid search
c
c Lars Ottemoeller, July 2002
c
c input: 
c        current - current waveform channel, index to waveform common block
c        log_spec - signal
c        farray - array with frequency values
c        nfreq - number of frequencies in farray
c        fmin - min f to use
c        fmax - max --------
c        norm - fitting norm
c        ngrid_f - number of grid points for f
c        ngrid_om - number of grid points for mo 
c        grid_nloop - number of loops in which grid size is made smaller
c        dkappa - search for best kappa, +/- dkappa 
c
c output:
c        om - spectral flat level in log10 units
c        fo - corner frequency
c        ka - delta kappa
c

      implicit none
      include 'libsei.inc'    ! Seisan includes
      include 'seidim.inc'    ! ---------------
      include 'waveform.inc'  ! ---------------
      integer seiclen

      integer xmax,ymax,xdim,ydim 
      parameter(xdim=500)
      parameter(ydim=500)
      integer nloop,loop
      integer current         ! current index to wav structure
      real bin_to_dec
      real log_spec(*),farray(*),y_brune(2048)
      integer nfreq
      real fmin,fmax
      real f0,om
      real norm

      integer i,jloop,nc,j,l
      real result(xdim,ydim),max
      real x,y,xstart,ystart
      real dkappa,k,ka
      integer nkappa
      real xstep,ystep
      real eval_spec_fit1
      real fitness
      real rand
      character*24 dec_to_bin
      character*80 outfile
      integer ngrid_f,ngrid_om
      real pi
      parameter (pi=3.141592654)

      xmax=ngrid_f
      ymax=ngrid_om

      open(26,file='grid.out',status='unknown')

c      nloop=3
      max=0.
      nkappa=1
      if (dkappa.ne.0.) nkappa=3

      do loop=1,nloop

c step size
        xstep=(fmax-fmin)/float(xmax)/(loop**2.)
        ystep=15./float(ymax)/(loop**2.)
        if(dkappa.eq.0.0) write(6,'(a,i2,1x,3f8.3)') 
     *' loop, f-step, om-step ',loop,xstep,ystep
        if(dkappa.gt.0.0) write(6,'(a,i2,1x,4f8.3)') 
     *' loop, f-step, om-step, kappa-step ',loop,xstep,ystep,dkappa
        if (loop.eq.1) then
          xstart=fmin
          ystart=-3.
        else
c set grid around previous best solution

          xstart=f0-(xmax*xstep/2.)
          if(xstart.le.fmin) xstart=fmin    ! jh apr 2013
          ystart=om-(ymax*ystep/2.)
c          write(6,*) f0,xstart,ystart,xmax,ymax,xstep,ystep
        endif

        do l=1,nkappa
          k=float(l-2)*dkappa
c          write(*,*) ' xxx dkappa ',k,dkappa,nkappa
          do i=1,xmax
            x=xstart+(i-1)*xstep

            do j=1,ymax
              y=ystart+(j-1)*ystep
              result(i,j) = eval_spec_fit1(log_spec,farray,
     &            nfreq,fmin,fmax,x,y,k,norm)
c commented out lo 22/7/2008
c            write(99,*) i,j,x,y,result(i,j)
c            if (loop.eq.3.and.x.gt.0.) write(87,*) x,y,30.-result(i,j)
              if (result(i,j).gt.max) then
                max=result(i,j)
                f0=x
                om=y
                ka=k
              endif       
            enddo
          enddo
        enddo

      enddo
      write(6,'(a,4f7.2)') 
     *' Best ever solution for om, f0, ka, max of fit ',
     *om,f0,ka,max
      close(26)
c
c write out log spectral amplitude
c
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.obs'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
c
c apply dkappa if not 0
c
      if (ka.ne.0.) then
        do i=1,nfreq
          log_spec(i)=log_spec(i)+pi*ka*farray(i)
        enddo
      endif

      do i=1,nfreq
        write(27,*) farray(i),log_spec(i)
      enddo
      close(27)

      call compute_brune(om,f0,farray,nfreq,y_brune)
      outfile=wav_stat(current)(1:seiclen(wav_stat(current)))
     &    // '.' // wav_comp(current)(1:seiclen(wav_comp(current)))
     &    // '.synth'
      do i=1,seiclen(outfile)
        if (outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo
      open(27,file=outfile,status='unknown')
      do i=1,nfreq
        if (farray(i).ge.fmin.and.farray(i).le.fmax) 
     &    write(27,*) farray(i),y_brune(i)
      enddo
      close(27)

      return
      end

      subroutine compute_brune(om,f0,farray,nfreq,brune)
c 
c compute Brune displacement spectrum for given om and f0
c
      implicit none
      real f0,om
      real farray(*)
      integer nfreq
      real brune(*)
      real pi
      parameter (pi=3.141592654)
      integer i
      do i=1,nfreq 
        brune(i)=log10(10**om/((1.+(farray(i)/f0)**2)))
      enddo
      return
      end

      real function eval_spec_fit(log_spec,farray,nfreq,fmin,fmax,
     &       j1,j2,lj1,lj2,norm)
c
c the evaluation function, computes residual between Brune spectrum and 
c observed spectrum
c
      implicit none
      real log_spec(*)
      real farray(*)
      real x,res
      integer nfreq
      integer j1(*),j2(*)
      real bin_to_dec
      real om,f0
      real fmin,fmax
      integer i,cnt,lj1,lj2
      real norm
c
c convert om and f0 to real numbers
c
      om=-5.+bin_to_dec(j1,lj1) *20./(2.**float(lj1)-1.)
      f0=bin_to_dec(j2,lj2) *40./(2.**float(lj2)-1.)
      res = 0.
      cnt = 0
c
c add squared residuals over all frequencies
c
      do i=1,nfreq
        if (farray(i).ge.fmin.and.farray(i).le.fmax.and.
     &       f0.gt.0.and.f0.lt.fmax) then
          x = om - log10(1.+(farray(i)/f0)**2)         ! source
          cnt = cnt +1
          res= res+abs(x-log_spec(i))**norm
        endif
      enddo
c
c make result negative, so that res can be maximized and add 
c constant to make values positive
c
      if (cnt.gt.0) then 
        x=-(res/float(cnt))**(1./norm) + 30.
      else
        x=0.
      endif
      eval_spec_fit = x   ! return negative residual
      end


      real function eval_spec_fit1(log_spec,farray,nfreq,fmin,fmax,
     &       f0,om,k,norm)
c
c the evaluation function, computes residual between Brune spectrum and
c observed spectrum
c
      implicit none
      real log_spec(*)
      real farray(*)
      real x,res
      real norm
      integer nfreq
      real bin_to_dec
      real om,f0
      real k
      real fmin,fmax
      integer i,cnt,lj1,lj2
      real pi
      parameter (pi=3.141592654)

      res = 0.
      cnt = 0
c
c add squared residuals over all frequencies
c
      do i=1,nfreq
        if (farray(i).ge.fmin.and.farray(i).le.fmax.and.
     &       f0.gt.0.and.f0.lt.fmax) then
          x = om - log10(1.+(farray(i)/f0)**2)         ! source
          x = x - pi * k * farray(i)
          cnt = cnt +1
          res= res+abs(x-log_spec(i))**norm
        endif
      enddo

      if (cnt.gt.0) then 
        x=-(res/float(cnt))**(1./norm) + 30.
      else
        x=0.
      endif
      eval_spec_fit1 = x   ! return negative residual
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine freq_event_type(yy,current,f_sel,time,pre,post,
     &   fr_type)
c
c determin type of event (Local, Distant or Noise), by comparing 
c spectral levels at low and high frequency, if level 
c difference from noise higher at lower frequency,
c event is assumed to be distant, otherwise local; 
c if signal and noise spectra have 
c no difference, signal is assumed to be noise
c
c Lars Ottemoeller, March 2001
c
c input: yy - data vector
c        current - index of carrent waveform channel
c        a and b - lower and higher frequency limits
c        time - phase arrival
c        pre - pre time in seconds
c        post - post time in seconds
c        f_sel - frequencies used to compare if dist or local
c
c output: fr_type - type of event, L: Local
c                                  D: Distant
c                                N: Noise (signal spectral levels same as noise)
c
c
      implicit none
      include 'libsei.inc'    ! Seisan includes
      include 'seidim.inc'    ! ---------------
      include 'waveform.inc'  ! ---------------
      integer current         ! current index to wav structure
      real yy(*)              ! data vector
      character*2 fr_type     ! type of event: L, D or N
      real a,b                ! min and max frequencies
      real f_low,f_high       ! low and high limit for spectrum
      real f_sel(2)
      integer maxl            ! dimension of spectral levels to be determined
      parameter (maxl=50)
      real level_signal(maxl),level_noise(maxl)  ! spectral levels for signal and noise
      real farray(maxl)       ! frequency array
      real max                ! max spec level
      complex y_com(max_sample) ! complex spectrum
      real sig(max_sample)    ! temp signal vector
      integer i,cnt           ! counters
      integer nsmooth         ! number of times for smooth
      double precision time   ! travel time
      real pre,post           ! pre and posrt time interval in seconds
      real xx                 ! some value
      integer ipow
c
c   poles and zeros, not used
c
      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm1                   ! normalization constant for poles and zeros

c
c set initial values
c
      nsmooth = 0         ! no smoothing
      fr_type = 'R '
      
      if (wav_resp_status.eq.'9') then
        write(*,*) ' no response for: ' //wav_stat(current)//' '//
     &       wav_comp(current)
        return
      endif

      if (time.eq.-1) return
c
c init values
c
      f_low=0.01
      f_high=wav_rate(current)/2.
      farray(1)=f_sel(1)
      farray(maxl)=f_sel(2)
      do i=2,maxl-1
        farray(i)=log10(farray(1))+
     &    (log10(farray(maxl))-log10(farray(1)))/(maxl-1)*(i-1)
        farray(i)=10**farray(i)
      enddo

c
c signal
c
      call extract_signal(yy,wav_nsamp(current),wav_abs_time(current),
     &    wav_rate(current),time,pre,post,sig,cnt)
      call spec_value(current,sig,cnt,nsmooth,
     &    f_low,f_high,1,farray,maxl,0,level_signal,y_com,ipow,
     *     zero,pole,nzero,npole,norm1)
c
c noise, set noise window, after wav_abs_time
c
      xx=post+pre
      if (wav_abs_time(current)+xx.ge.time) then
        xx=time-pre-wav_abs_time(current) 
      endif
      call extract_signal(yy,wav_nsamp(current),wav_abs_time(current),
     &    wav_rate(current),wav_abs_time(current)+pre,0.,xx,sig,cnt)
 
      call spec_value(current,sig,cnt,nsmooth,
     &    f_low,f_high,1,farray,maxl,0,level_noise,y_com,ipow,
     *     zero,pole,nzero,npole,norm1)

c
c find max difference between signal and noise
c
      max = 0.
      xx  = 0.
      do i=2,maxl-1
          xx=abs(log10(level_signal(i))-log10(level_noise(i)))
c          write(*,*) ' xxx ',farray(i),log10(level_signal(i)),
c     *    log10(level_noise(i))
          if (xx.gt.max) then
             max = xx
c             write(*,*) ' max ',max
          endif
      enddo

      if (max.le..3) then
c        write(*,*) ' xxx Noise ',max,log10(level_signal(i)),
c     &         log10(level_noise(i))
        fr_type(1:2) = 'N '
        write(*,*) ' *************** Noise ************* '
      elseif (log10(level_signal(1))-log10(level_noise(1)).gt.
     &   log10(level_signal(maxl))-log10(level_noise(maxl))+max/2.)
     &   then
        fr_type(1:2) = 'D '
c        write(*,*) ' *************** Distant *********** '
      elseif (log10(level_signal(maxl))-log10(level_noise(maxl)).gt.
     &   max/2.) then
        fr_type(1:2) = 'L '
c        write(*,*) ' *************** Local ************* '
      else
c
c unknown type
c
        fr_type(1:2) = 'R '
      endif

c      write(111,*) wav_stat(current)//' '//wav_comp(current)//' ',
c     &         log10(level_signal(1)),log10(level_noise(1)),
c     &         log10(level_signal(2)),log10(level_noise(2)),
c     &         log10(level_signal(1))-log10(level_noise(1)),
c     &         log10(level_signal(2))-log10(level_noise(2)),
c     & fr_type

      return
      end


c      subroutine auto_moment(current,xdata,nsamp,code,ffmin,ffmax)
c      implicit none
c      include 'libsei.inc'
c      include 'seidim.inc'
c      include 'waveform.inc'
c
c      integer current         ! current channel ID
c      real xdata(max_sample)  ! data
c      real y(max_sample)  ! data
c      integer nsamp           ! number of samples
c      real srate              ! sample rate
c      integer code            ! error code
c      integer i,j             ! counter
c      real taper/10./         ! percentage of tapering (fixed)
c      integer ipow            ! length after padding
c      real ff                 ! frequency
c      real x_work(max_sample) ! log x scale
c      real f_scale(max_sample)! frequ scale
c      integer rem_resp        ! 1 if remove
c      integer seiclen
c      complex y_com(max_sample)
c      real q0,qalpha          ! q
c      integer disp_vel        ! kind of spectrum
c      real kappa              ! kappa
c      real f_low,f_high       ! filter band
c      integer pole_low,pole_high ! filter poles
c      real ffmin,ffmax
c      real travel_time
c
c
cc
cc set variables
cc
c      srate=wav_rate(current)
c      do i=1,nsamp
c        y(i)=xdata(i)
c      enddo
c      wav_current_chan(1)=current
c      disp_vel=1
c      rem_resp=1
c      travel_time=1.
c      f_low=0.0001
c      f_high=1000.
c      pole_low=4
c      pole_high=4
cc
cc hardwire
cc
c      kappa=0.0
c      q0=0.   
c      qalpha=.0
c
c      if (nsamp.le.20) then
c        code = -1
c        return
c      endif
c
cc
cc read response
cc
c      wav_resp_file = ' '
cc      call read_resp
c
c      if(wav_resp_status(1:1).eq.'9') then
c         write(*,'(a)')' No response info ***'
c         return
c      endif
c      if(wav_resp_status(1:1).eq.'8') then
c         write(*,'(a)')' Response from waveform header ***'
c      endif
c
cc
cc------- prepare data.     Pad with zeros and taper 10%.
cc
cc      call prepare(nsamp,taper,ipow,npad)
c
c
cc
cc calculate displacement spectrum, remove response, correct for kappa, not
cc for Q
cc
c      call spectrum(y_com,ipow,disp_vel,rem_resp,srate,
c     +     f_low,f_high,pole_low,pole_high,q0,qalpha,
c     *     kappa,travel_time)
cc      call spectrum(ipow,rem_resp,srate,
cc     +     spstart,spstop)
c
cc
cc   calculate real spectrum, limit to frequencies ffmin and ffmax
cc
c      open(15,file=wav_stat(current)(1:seiclen(wav_stat(current)))
c     &     //'.out',status='unknown')
c      j = 0
c      do i = 2,((ipow/2) + 1)
c       ff = (i-1)*srate/ipow
c       if(ff .ge.ffmin.and. ff .le. ffmax)then
c         j = j + 1
c         y(j) = ((1/srate)**2)*(y_com(i)*conjg(y_com(i)))    ! transient signal
c         y(j) = sqrt(y(j))              ! j.h. change
c         y(j) = log10( y(j) )           ! take the logarithm
c         x_work(j) = log10(ff)          ! this after use of com, equivalent !!!
c         f_scale(j) = ff
c         write(15,*) x_work(j),y(j)         
c       endif
c      enddo
c      close(15)
c
cc
cc call the inversion routine
cc
cc      call invert_spectrum(current,f_scale,ipow/2-1)
c
c      return
c      end
c
c
c      subroutine invert_spectrum(current,f_scale,maxs)
cc
cc on Linux routines auto_moment and invert_spectrum cause problems
c      implicit none
c
c      include 'libsei.inc'
cc      include 'mulplt.inc'
c      include 'seidim.inc'
c      include 'waveform.inc'
c      
c      integer current
c      integer maxs
c      integer maxx
c      parameter (maxx=1000)
c      real tol
c      parameter (tol=1.E-5)
c      real y(max_sample)
c      real thresh,wmax
c      real gn(maxx,maxx)          ! 
c      real m(maxx),m1(maxx)
c      real d(maxx),ds(maxx)
c      integer i,j,x
c      real f_scale(maxx)
c      real s,ff,f0
c      real v(maxx,maxx),w(maxx)
c      real density
c      real spec_velocity
c      integer seiclen
c
c      density=2.5
c      spec_velocity=6.2
c      m(1)=4.      ! start value for f0
c      m(2)=10**14.     ! start value for Moment
c      do i=3,maxs
c        m(i)=0.
c      enddo
c
cc
cc iteration loop
cc
c      do i=1,5
c
cc
cc compute data vector = y(i) - S(fi)
cc
c      open(15,file=wav_stat(current)(1:seiclen(wav_stat(current)))
c     &     //'.tsp',status='unknown')
c         do j=1,maxs
c           ff=f_scale(j)
c           f0=m(1)
c           s = m(2)/(1+ff*ff/(f0*f0))  ! source
c           s = log10( s )      ! take the logarithm
c           s = s+log10(1.2)   ! rad. pat, free surface
c           s = s+log10(1.0/   ! the rest, 1000 to convert metr.
c     *               (4*3.14*(density*1000)*((1000*spec_velocity)**3)))
c           ds(j)=s
c           d(j)=y(j)-s
c           write(15,'(4(g15.5))') log10(ff),s
c         enddo
c         close(15)
cc
cc compute kernel G
cc
c         do x=1,maxs
c           do j=1,maxs
c             gn(x,j) = 0.
c           enddo
c         enddo
c         do j=1,maxs
cc
cc compute ds/df0
cc
c           ff=f_scale(j)
c           s=log(10.)*2*ff**2/(m(1)**3)*(1+ff**2/m(1)**2)
c           s=4*ff**2/((m(1)**2)*log(10.)*(1+ff**2/m(1)**2))
cc           s = s+log10(1.2)   ! rad. pat, free surface
cc           s = s+log10(1.0/   ! the rest, 1000 to convert metr.
cc     *               (4*3.14*(density*1000)*((1000*spec_velocity)**3)))
c           gn(1,j) = s
cc
cc compute ds/dM
cc 
c           s = 1/(log(10.)*m(2))
cc           s = s+log10(1.2)   ! rad. pat, free surface
cc           s = s+log10(1.0/   ! the rest, 1000 to convert metr.
cc     *               (4*3.14*(density*1000)*((1000*spec_velocity)**3)))
c           gn(2,j) = s
c
cc           write(*,*) 'xxx ',gn(1,j),gn(2,j)
c         enddo
cc
cc call SVD
cc
c         call svdcmpx(gn,maxs,maxs,maxs,maxs,w,v)
cc         write(*,*) w ,v
c 
cc
cc edit w, remove small values
cc
c         wmax=0.
c         do j=1,maxs
c           if(w(j).gt.wmax)wmax=w(j)
c         enddo
c         thresh=tol*wmax
c         do j=1,maxs
c           if (w(j).lt.thresh)w(j)=0.
c         enddo
c
cc
cc call svbksb
cc
c         call svbksbx(gn,w,v,maxs,maxs,maxs,maxs,d,m1)
c 
cc
cc add differences
cc
c         m(1) = m(1) + m1(1)
c         m(2) = m(2) + m1(2)
c         write(*,*) ' iteration ',i,m(1),m(2)
c
c      enddo
c
c      return
c      end


      subroutine normalize_signal(yy,n,xx)
c
c normalize data (yy) with n samples to maximum of xx
c
      implicit none
      real yy(*)
      integer n,i
      real max,xx

      max=0.
      do i=1,n
        if (abs(yy(i)).gt.max) max = abs(yy(i))
      enddo
      do i=1,n
        yy(i)=yy(i)/max*xx
      enddo
      return
      end


      subroutine gvel_to_time(dist,abs_time,
     &          time_out,duration)
c
c routine to convert group velocities in start time and duration
c

      implicit none
      include 'autosig.inc'

      real dist                          ! distance in km
      double precision abs_time,time_out ! origin time and phase start time
      real duration                      ! duration
      real start,stop                    ! help variables
      character*1 which_phase            ! function to give P or S
      real vg(2)                         ! group velocities

      if (which_phase(phase_mode).eq.'P') then
        vg(1)=gvel_p(1)
        vg(2)=gvel_p(2)
      elseif (which_phase(phase_mode).eq.'S') then
        vg(1)=gvel_s(1)
        vg(2)=gvel_s(2)
      else
        time_out=abs_time
        duration=0.
        return
      endif
 
      start=dist/vg(2)
      stop =dist/vg(1)
      time_out=abs_time+start
      duration=stop-start

      return
      end

      real function eval_q(q0,f,qalpha,fc)
      implicit none
      real q0,f,qalpha,fc
c
c next is original comment, however this is not what it was
c compute q=q0*f**qalpha for f>1 Hz, use fixed Q0 for f<1 
c
 
        

c  next 5 lines was how it was before dec 2013, jh 
c  now back jh mar 2014
c 
       if (fc.eq.0.) then
         eval_q=q0*f**qalpha
       else
         eval_q=q0*(1+(f/fc)**qalpha)
       endif

c
c   now use below, was commented out, jh, dec 2013
c   now out again mar 2014
c
c      if (f.ge.1.) then

c        eval_q=q0*f**qalpha

c      else
c        eval_q=q0
c      endif
      return
      end 

      logical function spiketest(data,c,nsamp,srate)
c
c check if signal is spike
c
      implicit none
      include 'seidim.inc'
      logical flag
      real data(*),data1(max_sample)
      integer c,nsamp,ind,i,j,i1,i2
      real average,min,max
      double precision srate

      flag=.false.

      min=999999999999999999.
      do i=1,nsamp
        if (abs(data(i)).lt.min) min=abs(data(i))
      enddo
      
      do i=1,nsamp
        data1(i)=abs(data(i))
      enddo

      max=-999999999999999999.
c
c find previous maximum within 3 seconds
c
      ind=c
      do i=c,1,-1
        if ((c-i)/srate.le.3.) then
          if (data1(i).gt.max) then
             max=data1(i)
             ind=i
          endif
        endif
      enddo 
      c=ind

      if (c.le.srate*.2) return

      average=0.
      j=0
      i1=c-int(srate*.2)
      i2=c+int(srate*.2)
      do i=i1,i2
        if (abs(c-i)/srate.gt..1) then
           average=average+data1(i)
           j=j+1
        endif
      enddo
      average=average/float(j)
      if (data1(c).gt.(10.*average)) then
        flag=.true.
      endif

      spiketest=flag
      return
      end

