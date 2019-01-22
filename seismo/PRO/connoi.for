
      program connoi
c
c program to compute noise spectra from continuous data
c
c Lars Ottemoller, June 2009
c

c
c changes:
c
c  dec 30 2009 jh: add paz to call spectrum
c  oct 22 2010 lo: add gap check
c  may 14 2013 lo: version by wcc
c  may 28 2014 lo: added reading of arc, read more data than 1 hour, started
c                  to use cont_step in addition to cont_interval to give
c                  the minutes to step forward between reads
c  jan 17 2015 lo: work around large gaps in arc
c  jan 18 2015 lo: checked and changed reading for pdf run
c

      implicit none 

c
c read SEISAN include files
c
      include 'seidim.inc'   ! dimensions
      include 'seisan.inc'   ! general seisan parameters
      include 'libsei.inc'
      include 'waveform.inc' ! for waveform handling
      include 'rea.inc'      ! for waveform handling
      include 'noise.inc'
      integer seiclen

      character*80 outfile   ! name of output seisan  waveform file
      character*80 monthfile ! name of output seisan  waveform file
      character*80 text  
      character*1040 theader ! seisan channel header
      character*80 fheader(max_trace) ! seisan waveform main header
      character*29 headtext
      character*5 net_code   ! code of network
      real duration_min      ! duration of data to read in in minutes
      integer i,j,l,k,m,n,o,p,ii ! counter
      integer maxstat,maxtrig
      parameter (maxstat=250)
      character*5 station(maxstat),cbase(maxstat)
      character*4 component(maxstat)
      integer ind
      double precision msec_start,msec_stop,msec,trigger_msec,msec1
      character*14 stop_time,start_time,time
      integer year1,month1,day1,hour1,min1,isec1,doy1
      integer year2,month2,day2,hour2,min2,isec2,doy2
      real sec1,sec2
      integer write1,write2,write3,read1
      logical b_flag
      integer code
      integer nchan,nbase
      integer narg
      character*80 arg(40)
      real rate
      integer nz,nzt
      logical gap_flag,flag,skip
      real expected_time(max_trace)
      real actual_time(max_trace),atime
      logical multi
      character*80 sys
      logical overwrite    ! true if output files should be overwritten
      logical allinone

      complex y_com(max_sample)                 ! data vector
      real y(max_sample),x(max_sample)
      integer pole_low,pole_high                ! filter poles
      integer disp_vel                          ! kind of spectrum
c-- percentage of tapering (fixed)
      real taper/10./
      integer rem_resp
      real ff,pff
      real logf(nf),yf(nf),dff,aa,bb
      real nlogf(nf)
      integer seg_nsamp
      character*4 base_type  ! type of data base like cont
      character*5 arcstat    ! station selected to be read from archive

c
c   response common block
c
      logical gse_resp          ! true if gse format
      logical sei_resp          ! true if seisan format
      logical seed_resp         ! true if seed response
      logical ftab
      integer ipow,npad,pow
      integer nfa               ! number of frequencies and amplitude
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      REAL FFILT(10)
      real pp(max_resp_value*2),pz(max_resp_value*2) ! PAZ, real and im.
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
c gaps
      real gap
      integer gapnsamp
c
c   paz for filter function in spctrum, not used here
c
      complex pole_filt(100),zero_filt(100)  ! complex PAZ
      integer npole_filt,nzero_filt          ! number of poles and zeros
      real norm_filt                         ! normalization constant 

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
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa
     
c	ev : allow spectragrams (spacing defines offset between spectra, not spectra length;
c		more efficient output format ("compact"), option to choose just one channel.
c		Note: times are beginning of data (not calculated/overlapped around center of
c			time period)
	  real cont_spacing		! WCC: spacing between spectra (1=no overlap, 0.5=50% overlap...)
          real cont_step        ! step between two computation (reads)
	  logical onestation	! WCC: do for just one station
	  logical compactfile	! WCC: compact file output format, not tested for multiple bases
	  logical noheaderyet	! WCC: put header line in compact file
	  character*5 thestation
	  character*4 thecomponent
          logical empty ! true if no arc data
c
c init
c
	  compactfile=.false.	! WCC: default is same as standard connoi
	  noheaderyet=.true.	! WCC:
	  onestation=.false.	! WCC: default is same as standard connoi
c	  cont_spacing=0.5	! WCC: default is same as standard connoi
	  cont_spacing=1.0	! lo: changed to 1, reason for .5?
c
c   get seisan defaults including names of continuous waveform data bases
c
      call get_seisan_def 

      multi=.false.
      nbase=0
      start_time=' '
      stop_time=' '
      overwrite=.false.
      allinone=.false.
      duration_min=60.
      pole_high=0.
      pole_low=0.
      rem_resp=1
      disp_vel=3
      npole_filt=0
      nzero_filt=0
c
c write Petersen curves
c
      open(1,file='petersen_low.out',status='unknown')
      do i=1,22
        write(1,*) low_f(i),low_l(i)
      enddo
      close(1)
      open(1,file='petersen_high.out',status='unknown')
      do i=1,14
        write(1,*) high_f(i),high_l(i)
      enddo
      close(1)

c
c set output frequencies for average event spectra
c
      dff=(alog10(f_high)-alog10(f_low))/float(nf-1)
      do i=1,nf
        logf(i)=alog10(f_low)+dff*float(i-1)
c        write(22,*) i,logf(i),10.**logf(i)
      enddo
c
c get arguments
c
      call get_arguments(narg,arg)
      do j=1,narg
        if (arg(j).eq.'-start') then   
          start_time=arg(j+1)
        elseif (arg(j).eq.'-stop') then   
          stop_time=arg(j+1)
        elseif (arg(j).eq.'-cbase') then
          cbase(1)=arg(j+1)
          nbase=1
        elseif (arg(j).eq.'-sgram') then	! WCC
        	if (j.gt.(narg-2)) then
        		write(*,*) 'You must have two arguments after -sgram!'
        		stop
        	else
        		text=arg(j+1)
        		if (text(1:1).eq.'-') then
        			write(*,*) text,' is not a valid station name for -sgram'
        			stop
        		endif
        		text=arg(j+2)
        		if (text(1:1).eq.'-') then
        			write(*,*) text,' is not a valid component name for -sgram'
        			stop
        		endif
        	endif
        	cont_spacing=1.					! WCC
        	compactfile=.true.				! WCC
        	onestation=.true.				! WCC
        	thestation=arg(j+1)				! WCC, no more than 5 letter
        	thecomponent=arg(j+2)			! WCC, must be 3 letters
        	write(*,*) 'SPECTRAGRAM MODE: Output compact, no overlap: '
     &        	,thestation,thecomponent
        elseif (arg(j).eq.'-overwrite') then
          overwrite=.true.
        elseif (arg(j).eq.'-allinone') then
          allinone=.true.
        elseif (arg(j).eq.'-compact') then	! WCC
          compactfile=.true.				! WCC
        elseif (arg(j).eq.'-spectlen') then
        	if (j.gt.(narg-1)) then
        		write(*,*) 'You must have an argument after -spectlen!'
        		stop
        	else
        		text=arg(j+1)
        		if (text(1:1).eq.'-') then
        			write(*,*) text,' is invalid # minutes for -spectlen'
        			stop
        		endif
        	endif
        	read(arg(j+1),'(f5.0)') duration_min
        	write(*,*) 'spect duration changed to ',duration_min
        elseif (arg(j).eq.'-arc') then	
          arc=.true.	
          arcstat=arg(j+1)
          cbase(1)='arc'
          nbase=1
          if (arcstat.eq.'def') then
            onestation=.false.
          else
            onestation=.true.
          endif
c          write(*,*) ' debug archive ...'
        elseif (arg(j).eq.'-spectspace') then
        	if (j.gt.(narg-1)) then
        		write(*,*) 'You must have an argument after -spacing!'
        		stop
        	else
        		text=arg(j+1)
        		if (text(1:1).eq.'-') then
        			write(*,*) text,' is invalid for -spacing'
        			stop
        		endif
        	endif
        	read(arg(j+1),'(f5.0)') cont_spacing
        	write(*,*) 'spect spacing changed to ',cont_spacing
        endif
      enddo
c
c init variables 
c
      if (cbase(1).eq.' ') then
        write(*,*) ' Name of continuous database '
        read(5,'(a)') cbase(1)
      endif

      if (start_time.eq.' ') then
        write(*,*) ' Start time (yyyymmdd...) '
        read(5,'(a)') start_time
      endif

      if (stop_time.eq.' ') then
        write(*,*) ' Stop time (yyyymmdd...) '
        read(5,'(a)') stop_time
      endif

      if (duration_min.eq.0.) then
        write(*,*) ' Duration in min '
        read(5,*) duration_min
        write(*,*) 													! WCC
     & ' Spacing between spectra (1.0=no overlap,0.5=50% overlap)'  ! WCC
        read(5,*) cont_spacing										! WCC
      endif

      if (station(1).eq.' ') then
        write(*,*) ' Station '
        read(5,'(a)') station(1)
      endif

      if (component(1).eq.' ') then
        write(*,*) ' Component '
        read(5,'(a)') component(1)
      endif

      write(*,*) ' start time: ',start_time
      write(*,*) ' stop time:  ',stop_time

c
c read databases if cbase(1) is def
c
      if (cbase(1).eq.'def') then
        nbase=0
        multi=.true.
        do k=1,n_cont_base
          if (cont_base_def(k).eq.1.) then
            nbase=nbase+1
            cbase(nbase)=cont_base(k)
          endif
        enddo
        write(*,*) ' continuous databases to process: ',nbase 
      endif
c
c loop over stations
c
      nchan=0
      write1=0
      do k=1,nbase
c
c init
c
        call wav_init
        cont_base(1)=cbase(k)
        n_cont_trace=0

        write(*,*) ' Database: ',cbase(k)

c
c  signal that reading is from a continous data base
c
        if (.not.arc) cwav=.true.
        n_cont_base=1

        cseed=.false.

c
c   calculate end time and extended start time, needed in order to
c   make sure enough files read in
c
        cwav_start_time=start_time

        read(start_time,'(i4,5i2)') year1,month1,day1,hour1,min1,isec1
        sec1=float(isec1)
        call timsec(year1,month1,day1,hour1,min1,sec1,msec_start)

c 
c compute overall endtime
c
        read(stop_time,'(i4,5i2)') year2,month2,day2,hour2,min2,isec2
        sec2=float(isec2)
        call timsec(year2,month2,day2,hour2,min2,sec2,msec_stop)

c
c set initial values
c
        call cwav_time_limits(0)
        cwav_abs_start_time=msec_start
c
c added 15 minutes to read a bit over segment
        cont_interval=(duration_min+15.)*60. ! changed back, lo 28/5/2014
c        cont_interval=duration_min*cont_spacing*60. ! WCC
        cont_step=(duration_min)*cont_spacing*60. ! WCC
c        write(*,*) ' debug ',cont_interval,cont_spacing

c        do while(cwav_abs_start_time+cont_interval.le.msec_stop)
        do while(cwav_abs_start_time+duration_min*60..le.msec_stop) !WCC
c
c check if monthly output file exists, skip if so
c
          skip=.false.
          if (.not.overwrite.and..not.allinone) then  ! allinone 17012015
            monthfile=' '
            write(monthfile,'(a,"_",i4.4,"_",i2.2,a)') 
     &      cont_base(1),year1,month1,'_connoi.out'
            do i=1,seiclen(monthfile)
              if (monthfile(i:i).eq.' ') monthfile(i:i)='_'
            enddo
            inquire(file=monthfile,exist=flag)
            if (flag) then
              day1=1
              hour1=0
              min1=0
              sec1=0.
              isec1=0
              if (month1.le.11) then
                month1=month1+1
              else
                month1=1
                year1=year1+1
              endif
              call timsec(year1,month1,day1,hour1,min1,sec1,msec_start)
              cwav_abs_start_time=msec_start
              skip=.true.
              write(*,*) ' file exists, no overwrite '//
     &           monthfile(1:seiclen(monthfile))
            endif
          endif
c
c open files
c
          if (write1.eq.0.and..not.skip)
     &      call sei open( unknown$,    ! Open old file.
     &               ' ',               ! No prompt.
     &               'connoi.out',      ! This filename.
     &               write1,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)

c	if compact file, write header lines: 1-parameters, 2- frequencies
          if (compactfile .and. noheaderyet) then							! WCC
            write(write1,'("CONNOI: NF=",i4," START=",a14," STOP=",a14,
     &         " DURATION=",f5.1,"min, SPACING=",f4.0,"%")') 			
     &         nf,start_time,stop_time,duration_min,cont_spacing*100.
         write(write1,													! WCC
     &       '(a5,1x,a3,1x,a4,2a2,1x,2a2,a5,1x,a9,1x)', advance='no')
     &       'STATN','CMP','yyyy','mm','dd','HH','MM','SS.FF','SPECLEN'
      	 do i=1,nf
         	write(write1,'(1x,f8.5)',advance='no') logf(i)				! WCC
      	 enddo															! WCC
      	 write(write1,'(1x)')	! Puts in end-of-line					! WCC
      	 noheaderyet=.false.											! WCC
      endif																! WCC
c
c set end of interval to be read
c
          empty=.false.
          call cwav_time_limits(1)
c
c  read the header information for all files in bases in time
c  interval, assume info available in common block
c
c          cwav_abs_end_time=cwav_abs_start_time+cont_interval
c          cwav_abs_end_time=cwav_abs_start_time+duration_min*60.	! WCC
          cwav_abs_end_time=cwav_abs_start_time+(duration_min+15.)*60.	! lo
          if (skip) goto 99
          if (arc) then
            call wav_read_arc_headers
            if (onestation.and.compactfile) then
              j=0
              do i=1,wav_nchan
                if (wav_stat(i).eq.thestation.and.
     & wav_comp(i).eq.thecomponent(1:2)//' '//thecomponent(3:3)) then
                  j=i
                 endif
              enddo
              if (j.eq.0) wav_nchan=0
            endif
            if(wav_nchan.eq.0) then
               write(6,*) ' No data, will stop'
               empty=.true.
               wav_nsamp(1)=0
               wav_stat(1)=thestation
               wav_comp(1)=thecomponent(1:2)//' '//thecomponent(3:3)
               n_cont_trace=1
c               goto 777
c               stop
            else
               empty=.false.
               write(6,*) ' Number of archive channels with data:',
     *         wav_nchan
               n_cont_trace=wav_nchan
            endif
          else
            call cwav_read_bases     
          endif

          call sectim(cwav_abs_start_time,year1,doy1,month1,day1,hour1,
     &    min1,sec1)
          call sectim(cwav_abs_end_time,year2,doy2,month2,day2,hour2,
     &    min2,sec2)
          write(*,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' start time: ',year1,month1,day1,hour1,min1,sec1
          write(*,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' stop time:  ',year2,month2,day2,hour2,min2,sec2
c
c find channel index
c
c          write(*,*) ' debug ',n_cont_trace
          do i=1,n_cont_trace
            if (arc) then
              cwav_stat(i)=wav_stat(i)
              cwav_comp(i)=wav_comp(i)
            endif
c      write(*,*) ' stream: ',wav_stat(i),wav_comp(i)
            ind=0
            do j=1,nchan
              if (cwav_stat(i).eq.station(j).and.
     &            cwav_comp(i).eq.component(j)) 
     &        then
                ind=j
              endif
            enddo
c new channel
            if (ind.eq.0) then
              nchan=nchan+1
              ind=nchan
              station(nchan)=cwav_stat(i)
              component(nchan)=cwav_comp(i)
c              write(*,*) ' debug new station ',cwav_stat(i),cwav_comp(i)
            endif
              wav_current_chan(1)=1
              wav_stat(1)=cwav_stat(i)
              wav_comp(1)=cwav_comp(i)
              wav_abs_time(1)=cwav_abs_end_time
c changed lo 28/5/2014
c              wav_current_chan(i)=i
c              wav_stat(i)=cwav_stat(i)
c              wav_comp(i)=cwav_comp(i)
c              wav_abs_time(i)=cwav_abs_end_time

c              write(*,*) ' debug new station ',wav_stat(i),wav_comp(i)
            
            if ((.not.onestation).or.								! WCC 
     &			(station(ind).eq.thestation.and.thecomponent.eq.
     &          (component(ind)(1:2)//component(ind)(4:4)) ) .or.
     &           (arc.and.station(ind).eq.arcstat.and.
     &            .not.compactfile).or.
     &           (arc.and.station(ind).eq.arcstat.and.
     &            thecomponent.eq.
     &            component(ind)(1:2)//component(ind)(4:4))) then ! lo 16012015
c
c  read the waveform data, one trace at a time
c
c            write(*,*) ' debug read ',cwav_stat(i)
c            write(*,*) ' debug empty ',empty
            if (.not.empty) call wav_read_channel(i)    ! read trace
c            write(*,*) ' debug signal1 ',signal1(1)

            rate = wav_rate(i)
c            write(*,*) ' debug rate ',rate
            if (wav_nsamp(i).lt.0) then
              wav_nsamp(i)=0
            endif
c            write(*,*) ' debug nsamp ',wav_nsamp(i)
c
c read response
c
            if (wav_nsamp(i).gt.0) then
              call read_resp
              if(wav_resp_status(1:1).eq.'8') then
                write(*,*) ' *** response from header *** '
              elseif(wav_resp_status(1:1).eq.'9') then
                write(*,*) wav_stat(1),wav_comp(1),   			! WCC
     &            ' *** no response, stop *** '					! WCC
c                write(*,*) ' *** no response, stop *** '		! WCC replaced
              endif
            endif
c
c check for gaps
c
            gap=0.
            call check4gap(signal1,wav_nsamp(i),rate,1.,gapnsamp,gap)
            if (gap.ge.1.) then
              write(*,*) ' gap found ',gap
            endif
c            write(*,*) ' debug signal ',signal1(1)
c
c compute spectrum
c
            if (wav_nsamp(i).gt.0.and.gap.eq.0) then
              !seg_nsamp=int(rate*900.) ! Hard wired 15-min segments for 60-min total duration
              seg_nsamp=int(rate*duration_min*15.) 	!WCC
              do n=1,nf
                yf(n)=0.
                nlogf(n)=0.
              enddo
c
c compute fft for 13x15 minute segments, with 75% overlap (old version)
c compute fft for 13 duration_min/4 minute segments, with 75% overlap (WCC)
c
              do o=1,13
c                write(*,*) ' o = ',o
                pow = 1
                do while(pow*2.le.seg_nsamp)
                  pow = pow*2
                enddo
c extract segment
                do n=1,pow
                  m=int((o-1)*.25*seg_nsamp)
                  if (m+n.le.wav_nsamp(i)) then
                    x(n)=signal1(m+n)
                  else 
                    x(n)=0. ! lo 28/5/2014
                  endif
c                  write(24,*) n,x(n)
c               if (n.eq.1)write(*,*) ' debug seg_nsamp ',seg_nsamp ,
c     &           wav_nsamp(i),x(n)
                enddo
c                write(*,*) ' m = ',m,m+seg_nsamp
                call rtr(x,pow,rate)
                call prepare(pow,x,taper,ipow,npad,y_com)
                call spectrum(y_com,ipow,disp_vel,rem_resp,rate,
     +          f_low,f_high,pole_low,pole_high,0.,0.,0.,0.,
     *          zero_filt,pole_filt,nzero_filt,npole_filt,norm_filt)
c                write(*,*) ' debug ycom ',o,y_com(1)
c
c   calculate real spectrum
c
                j = 0
                pff = 0.
                do m = 2,((ipow/2) + 1)
                  ff = (m-1)*rate/ipow
                  j = j + 1
                  y(j) =2*y_com(m)*conjg(y_com(m))/(rate*pow) 
                  y(j)=y(j)*1.142857 ! to account for 10% taper
                  y(j)=alog10(y(j))
                  y(j)=(y(j)-18.0)*10.0  ! db (m/s**2)**2
c                  if (m.eq.2) write(*,*) ' debug ',j,y(j)
c                  if (component(ind)(4:4).eq.'Z') then
c                    write(21,*) alog10(ff),y(j)
c                  endif
c
c find log f interval 
c
                  ii=0
cccccc check
                  ii=int((alog10(ff)-alog10(f_low)+dff/2.)/dff)+1
c                  write(*,*) ii
                  if (ii.gt.0.and.ii.lt.nf) then
c              write(23,*) ' ii ',ii,alog10(ff),alog10(f_low)+dff/2.
                    yf(ii)=yf(ii)+y(j)
                    nlogf(ii)=nlogf(ii)+1
                  endif
c 
c fill empty cells due to logarithmic f spacing
c
                  if (pff.ne.0..and.ii.gt.0) then
c            write(*,*) ' debug filling empty cells ',pff
                    if (alog10(ff)-alog10(pff).gt.dff) then
c                      write(*,*) alog10(pff),alog10(ff),
c     &                 alog10(ff)-alog10(pff),dff
                      p=int((alog10(ff)-alog10(pff))/dff)
                      do n=ii-p,ii+p
                        if (n.gt.0.and.n.le.nf.and.n.ne.ii) then
c                        write(*,*) ' filling ',ii,n
                          yf(n)=yf(n)+y(j)
                          nlogf(n)=nlogf(n)+1
c                write(*,*) ' debug ',n,yf(n),nlogf(n)
                        endif
                      enddo
                    endif
                  endif
                  pff=ff
                enddo
c                if (component(ind)(4:4).eq.'Z') then
c                  write(21,'(a1)')'>'
c                endif
              enddo ! loop over 13 segments
              if (compactfile) then	! WCC write line header (stat comp time)
                  write(write1,
     &       		'(a5,1x,a3,1x,i4,2i2.2,1x,2i2.2,f5.2,1x,f9.2,1x)',
     &         	      advance='no')
     &        		station(ind),component(ind)(1:2)//component(ind)(4:4),
     &        		year1,month1,day1,hour1,min1,sec1,duration_min*60.
     		  endif
              do n=1,nf
c compute average
                if (nlogf(n).ne.0) then
                	yf(n)=yf(n)/nlogf(n)
                	if (.not.compactfile) then	! WCC
                  		write(write1,
     &       '(a5,1x,a3,1x,i4,2i2.2,1x,2i2.2,f5.2,1x,
     &         f9.2,1x,f8.5,1x,f10.2)')
     &        station(ind),component(ind)(1:2)//component(ind)(4:4),
     &        year1,month1,day1,hour1,min1,sec1,duration_min*60,
     &        10.**logf(n),yf(n)
c     &        year1,month1,day1,hour1,min1,sec1,cont_interval,	! WCC replaced
                	else
                                if (yf(n).lt.-999.) yf(n)=-999.   ! lo
                                if (yf(n).gt.999.) yf(n)=999.   ! lo
                 		write(write1,'(1x,f8.2)',advance='no') yf(n)	! WCC
   					endif
                elseif (compactfile) then
                 	write(write1,'(1x,f8.2)',advance='no') -999.	! WCC
                endif
              enddo ! loop over frequencies
              if (compactfile) write(write1,'(1x)')	! Force carriage return
             else ! NOT wav_nsamp(i).gt.0.and.gap.eq.0  
             	if (compactfile) then	! WCC write empty data line
                  write(write1,
     &       		'(a5,1x,a3,1x,i4,2i2.2,1x,2i2.2,f5.2,1x,f9.2,1x)',
     &         	      advance='no')
     &        		station(ind),component(ind)(1:2)//component(ind)(4:4),
     &        		year1,month1,day1,hour1,min1,sec1,duration_min*60.
             	  do n=1,nf
                    write(write1,'(1x,f8.2)',advance='no') -999.
                  enddo
                endif
                write(write1,'(1x)')	! Force carriage return           			
            endif ! wav_nsamp(i).gt.0.and.gap.eq.0
           endif ! WCC not onestation or is the desired station
          enddo ! loop over traces
c jump here if no archive data
777       continue
c
c rename files if multi mode, and end of month
c 
          if (.not.allinone.and.
     &        (month2.gt.month1.or.
     &        year2.gt.year1.or.
     &      cwav_abs_start_time.ge.msec_stop)) then
c
c close files
c
            call sei close( close$, write1, code )
            write1=0
            sys=' '
            write(sys,'(a,a,"_",i4.4,"_",i2.2,a)') 
     &         'mv connoi.out ',
     &         cont_base(1),year1,month1,'_connoi.out'
            do i=15,seiclen(sys)
              if (sys(i:i).eq.' ') sys(i:i)='_'
            enddo
            write(*,*) sys
            call systemc(sys,seiclen(sys))
            noheaderyet=.true.		! WCC: if compact, put header in new connoi.out
          endif
c
c set start of next interval to read
c
99        continue
c          cwav_abs_start_time=cwav_abs_end_time-
c     &       duration_min*60.*.5     ! 50 percent overlap
c          cwav_abs_start_time=cwav_abs_start_time+cont_interval	! WCC
          cwav_abs_start_time=cwav_abs_start_time+cont_step	! lo 29/5/2013
        enddo ! loop over time
      enddo ! end of loop over stations
      if (write1.ne.0) call sei close( close$, write1, code )

      stop
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
      real rate               ! rate

c
c get x coords
c
      do i=1,max_sample
        x(i)=i/rate
      enddo

      call lsqlin(nlen,x,data,a,b,corr,rms)

c      write(*,*) 'correction applied: y=',b,'*x+',a
c
c remove trend
c
      do i=1,max_sample
        data(i)=data(i)-b*x(i)-a
      enddo

      return
      end

