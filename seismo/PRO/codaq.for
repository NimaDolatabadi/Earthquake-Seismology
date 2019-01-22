C***********************************888888888********************************* 
C                                                                               
c       PROGRAM  codaq                                                          
c                                                                               
c       By  Jens Havskov, 1985                                                        
c                                                                               
c       Calculates coda q and plots the filtered traces with envelope fits 
c                                                                               
c       Latest uppdate:                                                         
c       April 85: last uw version                                               
c       apr 90   by j.h. : uib adaption                                         
c       jul 92           : tektronics adoption                                  
c       aug 92           : combine qplot and invq to codaq, cut out
c                          multiple envelope inversion
c       nov 12 92        : dimension to 50000
c       nov 26           : increase dimension of q to 1000
c       jul 8 93 by jh   : version 3.0 ********************* 
c       jul 21           : xdrwvec to xdrwvec_old, use routines xt and vt
c       aug 23 93        : filenames to 80 char
c       oct 93           : moved to pc, bugs: wrong arg in call to timadd
c       nov 16           : error check s, enable use of all components
c       nov 17           ; error of s/n when plotting
c       jun 94           : remove dc before filter, fix weighting
c       dec 7            : include seidim
c       feb 95           :  ************** version 5.0*********************
c       may 95           : new seisinc common block
c                          new wsize
c       nov 1 95         : new seisinc routine
c       mar 25, 96       : adjust call filter to one or 2 passes
c       mar 26           : calculate average lapse times
c       aug 96           : only look for first wav file
c       aug 30           : write also filer codaq1.out
c       sep 5 96         : error when sending plot to laser
c       mar 1 97         : fix so components also used in mode with
c                          stations selected pr event, clean up a few things
c                          related to output and check for non blank input
c       mar 4            : more output fixing
c       dec 19           : fix so all waveform files can be used
c       mar 30, 1998     : reject data over a max count value to reject 
c                          clipped data, check in coda window, increase to 30
c                          stations, calculate average correlation coeffecient
c       nov 98 jh        :   -----------   version 7.0 check ---------------
c                          year 2000, one_line to oneline, 5 char stat
C                                                                               
C      mar 22 99  bmt     : include winplot.inc                                                                            
c      sep 7 99 jh       : bug in graphics error message
c      sep 15            : pause before stop on pc
c      may 2000 jh       : new waveform struecture
c      may 18            : small bug
c      oct 25            : high accuracy, still one 4 char station
c      jan 5, 2001       : variable time scale, SEISAN.DEF was not read in,
c                          remove dc before plot
c      mar19             : variable cof was not an array, crash on pc
c      april 26          : optinally use center frequecy, new clipping routine
c      jun 20 2005  jh   : plt tp eps
c      nov 10, 2008 jh   : input also as arguments, optionally enter vpvs
c      jan 12  2010 jh   : check if all arguments there, else stop
c      aug 13  2010 jh   : increase to 50 stations
c      sep  8  2010 jh   : output an index file with events with good q
c      sep 11  2010 jh   : initilize waveform memory handling for each event
c                          to prevent pointer overflow to channel-waveform
c                          combinations
c      sep 20  2010 jh   : overflow in output, increase dim to 15 000
c      dec 27  2010 jh   : gfortran on pc, remove winplot, put in implicit none, unit 15
c                          to 25, unit 16 to 26, remove hctype
c      2010-12-30 pv     : removed spaces at 188 lines, to avoide compiler warnings
c      2011-02-22 jh     : size from color.def
c      2011-07-21 jh     : bug with 5 char station
c      2012-02-01 jh     : option to write out codaq.area
c      2012-02-01 jh     : if comp blank, read first comp found
c      2012-03 21  h     : fix output
c      jan 4 2012 jh:    : add argument to auto_tr, fix bad dimension
c      jan 14 2014 jh:   : add option to run the same event set with several stations 
c                          individually
c      jan 25 2014 jh    : also output results from each station separately for all runs
c      mar 24 2014 jh    : some corrections to output
c      jul    2015 jh    : add codaq.statis output, better detection of clipping, make 
c                          envlope window constant instead of depending
c                          on rms window. has increased low frequncy Q
c      oct 27 2015 jh    : dimension to 45000
c      jan 22 2015       : sd on q1.5 and and q10 hz, only use q>10
c      apr 30 2017 jh    : Errror calculation were wrong on q0 and qalpha. this also affected
c                          determination of q0 and qalpha and sd of calculated q
c                          this error was found thanks to luis matias who also suggested how to
c                          do it correctly. 
c                          see https://en.wikipedia.org/wiki/Propagation_of_uncertainty 
c      oct 10 2017 jh    : add sd to codaq in codaq.area
c      nov 30 2017 jh    : fix some dimensions

C
       implicit none   
       include 'seidim.inc'
       include 'waveform.inc'
       include 'seiplot.inc'
       include 'libsei.inc'
c--- event name from SEISAN data base
	character*80 eventid  
        character*80   trace_file  ! trace file name                                                  
c--- help strings
	character*80 head80                                         
	character*120 headp
	character*80 head
c-- text from control file
	character*39 text
c-- general test
        character*80 txt
        integer code             ! error code
c-- min s/n ratio 
	real minratio
	character*5 station(300)
	character*4 component(300),dummy
        real dist                ! epicentral distance                                     
c-- dc level
	real dc
c-- date and time
	character*18 date,date1
	real flag(3),mincorr
c-- number of good q values each frequency
	integer nq(10)
	real av_corr(10),sd_corr(10)
c-- q-values saved at each frequency
      real qall(10,45000)
      real tcoda_all(45000)   ! lapse times
      real corr_all(10,45000) ! correlation coefficient
      integer ntcoda         ! number of lapse times
      integer old_ntcoda     ! number of lapse time from previous event
      real av, sd            ! average and standard deviation
      integer nsamp          ! number of samples in one trace
c-- noise and signals used
      dimension xnoise(45000),xsignal(max_sample)                                  
      character*80 message(5)   ! messages in graphics
      CHARACTER NAME*80                                                       
      REAL WLEN                 ! window lenght                               
      REAL FRE(10),BAND(10)     ! frequency bands                                        
      real freq1(10),freq2(10)  ! real limits used 
      real noctave(10)          ! octave width of filter used
      real octave               ! octave of filter required
      REAL RR,FQ1,FQ2,f1,q1                                                       
      INTEGER nrate             ! smaple rate                                              
      INTEGER NN,K,kk1,kk2,m,n                                                              
      logical clipped           ! true if clipped signals
      logical one_data_set      ! true if only one data set
      logical sun,pc,linux
      logical write_screen      ! true of write to screen in routine output
      integer check             ! if one, test output
      logical channel_average   ! true: give Q-values for each channel, false: average per station
c--parameters used in xtek library                                              
	integer ny,nx,nstep,i,nfreq,ndefsta,nsta,ista,ierr,
     *  idepth,
     *  ifr,ntotal,nnoise,nss,irms,iratio,iq,isec,nsignal,nstart
      real start,absstart,cs,wout,tp,tcoda,tstart,depth,t0,
     *  x0,y0,s,q,tnoise,del,rms,corr,trms,ratio,tt,g,xsignal,       
     *  xnoise,smag,cof(8)
c
        real xsc      ! xscale in tek units/sec
        real vpvs     ! vpvs ratio
        integer maxcount  ! max count to use trace
        character*80 answer
c
        character*10 statcomp(1000),stacom  ! station and componet
        integer nstatcomp            ! number of different statcomps
        integer ncombo               ! total number of q-results
      real sdq                       ! standard deviation in one value of q
c       
c---number of arguments
      integer nars
c-- arguments
      character*80 arguments(5)
      integer ndataset   ! number of individual data sets used
      real qlat,qlon  ! mid point between station and event
      include 'version.inc'
c -----------------------------------------------------------------             



c
c print version
c
      out_version_date='July 2015'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)
c
c   seisan definitions
c
      call get_seisan_def

c                                                                               
c       Definition of some variables:                                           
c                                                                               
c       nx:      number of plots in x direction                                 
c       ny:      number of plots in y direction                                 
c       nstep:   every nstep samples are plotted                                
c                                                                               

c                                                              
c---unit for plot file
       plotunit=65
c---initial xwindow size
      wsize=70
      call get_window_size
      if(size_codaq.gt.0) wsize=size_codaq ! from color.def
c---display not yet open
      disp_open=0
      ndataset=0
c                                                                               
	ny=1
	nx=1
	nstep=4
        check=0                       
c       check=1
        ntcoda=0
        old_ntcoda=0                                          
c
c  initialize counter for q
c
	do i=1,10
	  nq(i)=0
	enddo
c
c  check if input as arguments
c
       call get_arguments(nars,arguments)
c
c  check for component average
c
       channel_average=.true.
       if(nars.gt.0.and.arguments(1)(1:2).eq.'-c') then
c          write(6,*)' Average by channel'
          channel_average=.false.
          if(nars.eq.1) nars=0
          if(nars.gt.1) then     ! restore as if 3 arguments
             do i=1,3
               arguments(i)=arguments(i+1)
             enddo
             nars=nars-1
          endif
       endif

       if(nars.gt.0.) then
           call sei get values(1,arguments(1), code )  
           plotoption=array$(1)
           name=arguments(2)
c
c   check if enough arguments
c
           if(nars.ne.3) then
              write(6,*) 'Give 3 or 4 arguments'
              write(6,*) 'First optional is -c for channel values'
              write(6,*) 'Option 0-3'
              write(6,*) 'Parameter file'
              write(6,*) 'Data file'
              stop
           endif
       else
c
c  get input from keyboard
c
                                                                                          
          write(6,*)' No plot, only Q-calculation (def) (0) '                            
          write(6,*)' Plot on screen                    (1) '                            
          write(6,*)' Plot on screen+laser              (2) '                            
          write(6,*)' Plot on laser                     (3) '                            
          read(5,'(a)') text
          if(text.eq.' ')  then
             plotoption=0
          else
             read(text,*) plotoption
          endif
       endif                                                     
       plotoption=plotoption-1                                                  
       if(plotoption.gt.0) then                                                  
	      open(65,file='codaq.eps',status='unknown')
	      plotunit=65
       endif                                                                    
										
c
c   open output files with results and error messages
c
       open(3,file='codaq.out',status='unknown')
       open(4,file='codaq1.out',status='unknown')
       open(7,file='codaq.index',status='unknown')
       open(8,file='codaq.area',status='unknown')
       open(9,file='codaq.summary',status='unknown')
       open(12,file='codaq.channel', status='unknown')

       if(nars.eq.0) then 
          WRITE(6,*)'Parameter file, codaq.par is default (return)'                
          READ (*,'(a)') NAME
       endif                                                    
       if(name(1:2).eq.'  ') name='codaq.par'                                  
C                                                                               
c   open parameter filer giving specs for processing                           
C                                                                               
	OPEN(UNIT=1,FILE=NAME,STATUS='OLD')
c                                                                               
c   read parameters shared by all data sets                                     
c   each data line is separated by a comment line read by text                
c                                                                               
	read(1,'(a)') text
c	read(1,*) start
        read(1,'(a)') txt
        B_F_MESSAGE$=.false.   ! do not write error message if only start
        call sei get values(2,txt, code )  ! optionally check for 2 values, start and vpvs
        start=array$(1)  
        vpvs=1.78         ! default vpvs value
        if(code.eq.e_ok$) vpvs=array$(2)
c       Write(6,*) vpvs
	write(3,'(1x,a39,2f10.2)') text,start, vpvs
	write(4,'(1x,a39,f10.2)') text,start
	read(1,'(a)') text
	read(1,*) absstart
	write(3,'(1x,a39,f10.2)') text,absstart
	write(4,'(1x,a39,f10.2)') text,absstart
	read(1,'(a)') text
	read(1,*) wlen
	write(3,'(1x,a39,f10.2)') text,wlen
	write(4,'(1x,a39,f10.2)') text,wlen
	read(1,'(a)') text
	read(1,*) rr
	write(3,'(1x,a39,f10.2)') text,rr
	write(4,'(1x,a39,f10.2)') text,rr
	read(1,'(a)') text
	read(1,*) cs
	write(3,'(1x,a39,f10.2)') text,cs
	write(4,'(1x,a39,f10.2)') text,cs
	read(1,'(a)') text
	read(1,*) minratio
	write(3,'(1x,a39,f10.2)') text,minratio
	write(4,'(1x,a39,f10.2)') text,minratio
	read(1,'(a)') text
	read(1,*) tnoise,trms
	write(3,'(1x,a39,2f10.2)') text,tnoise,trms
	write(4,'(1x,a39,2f10.2)') text,tnoise,trms
	read(1,'(a)') text
	read(1,*) mincorr
	write(3,'(1x,a39,f10.2)') text,mincorr
	write(4,'(1x,a39,f10.2)') text,mincorr
	read(1,'(a)') text
	read(1,*) maxcount
	write(3,'(1x,a39,i10)') text,maxcount
	write(4,'(1x,a39,i10)') text,maxcount
	read(1,'(a)') text
        read(1,'(a)') txt
        octave=0
        B_F_MESSAGE$=.false.   ! do not write error message
        call sei get values(2,txt, code )  ! optionally check for 2
        nfreq=array$(1)
        if(code.eq.e_ok$) octave=array$(2)
c        write(6,*) octave
c
	read(1,'(a)') text
c
c  there could be one or two values
c
	do 2 i=1,nfreq
           band(i)=0.0
           read(1,'(a)') txt
           B_F_MESSAGE$=.false.   ! do not write error message 
           call sei get values(2,txt, code )  ! optionally check for 2
           fre(i)=array$(1)
           if(code.eq.e_ok$) band(i)=array$(2)
c          read(1,*) fre(i),band(i)
 2      continue
c
c  check for input
c
        if(octave.eq.0.0.and.band(1).eq.0.0) then
          write(6,*) 
     *   'Frequency band and number of octaves cannot both be zero'
         stop
        endif
	read(1,'(a)') dummy

        one_data_set=.false.    ! by default there could be more data sets

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   back here for new set of stations for same events, will make a new average
c------------------------------------------------------------------------------
c
 5000   continue
c
c   check if one data set, a user might have put in more blank lines
c   than 2
c
        if(one_data_set) goto 6000

        ntcoda=0
        old_ntcoda=0 
c
c  initialize counter for q
c
        do i=1,10
          nq(i)=0
        enddo  
c                                                                               
c   read line containing up to 300 default stations to use with all events,      
c   used only if no station names are supplied with the                
c   s-file names. Components can be given in next line, if not there
c   use first component found in wav file                                                             
c               
c   MAKE SURE ALL ARRAYS CONTAIN BLANKS IF NO STATIONS OR COMPONENTS
C
	do i=1,300
	   station(i)=  '     '
	   component(i)='    '
	enddo

	read(1,'(300a5)',end=363)     (station(i),i=1,300)
	read(1,'(300(a4,1x))',end=6000) (component(i),i=1,300) ! 6000 is end of all processing

        if(station(1).eq.' ') one_data_set=.true.
        goto 365
c
 363    continue
        if(ndataset.gt.0) goto 6000 ! many data sets
        do i=1,300
          station(i)='     '
          component(i)='    '
        enddo

 365    continue
c                                                                               
c   find how many default stations there are, check for non printable
c   characters, particular ctl z sometimes seem to sneek in on pc's                                    
c                                                                                
       ndefsta=0
       do i=1,300   
       do k=1,5
          if(ichar(station(i)(k:k)).lt.32) station(i)(k:k)=' '
       enddo 
       do k=1,4
          if(ichar(component(i)(k:k)).lt.32) component(i)(k:k)=' '
       enddo                                                            
	   if(station(i).eq.'     ') go to 177
	   ndefsta=ndefsta+1
	  enddo
 177  continue 
c
c   write in summary file
c
      write(9,'(300(a,a,1x))')(station(i),component(i),i=1,ndefsta)                                                               
                                                      
      if(ndefsta.gt.0) then          
        write(4,'(300a5)') (station(i),i=1,300)                                 
        write(3,'(300a5)') (station(i),i=1,300)                                 
        write(3,'(300(a4,1x))') (component(i),i=1,300)
        write(4,'(300(a4,1x))') (component(i),i=1,300)
      endif
c
c   set x-scale as a function of window length
c                                                                               
       xsc=60.0/wlen
c
c   open file with specs of event station combinations                          
c   read until eof, only do first time events are used                                                              
c
       if(ndataset.eq.0) then
        if(nars.gt.1) then
          name=arguments(3)
        else                                                                               
	   write(6,*)
     *     'File with event-stations,',
     *     ' name codaq.inp is default (return)'         
           read(5,'(a)') name       
        endif                                               
	if(name(1:2).eq.' ') name='codaq.inp'
	open(2,file=name,status='old')
       endif

        ndataset=ndataset+1
c                                                                               
c   open and position plot                                                      
c                                                                               
	if(plotoption.ge.0)  then 
	    call open_display
	endif

C-----------------------------------------------------------------------
C
C   reading loop for events to process starts her **********************        
C                                                                               
c-----------------------------------------------------------------------
c
 3      CONTINUE                                                                     
C                                                                               
C   Read new event ID from file until eof.                                      
C   The event ID might be followed by up to 300 station                          
C   names (next line), if no station name, use default stations given           
C   in parameter file                                                          
                                                                               
	nsta=ndefsta        
c
c   output event id's used if good q, so ntcoda has increased
c
        if(ntcoda.gt.old_ntcoda) then
            write(7,'(7x,a73)') eventid(1:73)
        endif      

        old_ntcoda=ntcoda                                                    
	read(2,'(7x,a80)',end=99) eventid
        write(6,*)
        write(6,*)'New event *****************************************'
        write(6,'(a)') eventid(1:70)
        write(6,*)
        write(3,*)
        write(3,*)'New event *****************************************'
        write(3,'(a)') eventid(1:70)
        write(3,*)

c
c   intilize waveform memory storage, program might otherwise crash 
c   when many waveform events are used since counter for number of station
c   events overflow
c
        call wav_mem_init
c                                         
        if(eventid(1:10).eq.'          ') goto 99 ! trap blank line
        if(check.eq.1) write(3,*)'ndefsta',ndefsta
	if(ndefsta.eq.0) then
           do i=1,300
              station(i)='     '
              component(i)='    ' 
           enddo
           read(2,'(300a5))',end=99) (station(i),i=1,300)
           read(2,'(300(a4,1x))',end=99)(component(i),i=1,300)
c                                                                               
c   find how many stations there are for that event                             
c
           nsta=0
           do 6 i=1,300
	     if(station(i).eq.'     ') go to 7
	     nsta=nsta+1
 6         continue                                                              
 7         continue                                                             
           if(check.eq.1) write(3,'(i5,1x,300a5)') nsta,station                  
	endif
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc              
c                                                                c              
c   station loop for this event starts here   DO 21              c              
c                                                                c              
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc              
c                                                                               
	do 21 ista=1,nsta
	nn=0
C                                                                               
C   now enter data, first increase window length to make sure
C   enough for filter plus rms              
C       
	wout=wlen   ! not used in routine since all data read
c
c   extract all data from waveform file
c   nsamp is number of samples
c
	call codaselect
     *  (eventid,station(ista),component(ista),
     *  start,absstart,vpvs,wout,3,date,tp,             
     *  tcoda,tstart,nsamp,nrate,ierr,6,plotoption,trace_file,qlat,qlon,
     *  dist)

        if(check.eq.1) write(3,*)'codaselect',trace_file
        if(check.eq.1) write(3,*)'nrate,nsamp',nrate,nsamp
C                                                                               
c                                                                               
c   check for errors                                                            
C              
      if(check.eq.1) write(3,*)' errors after calling codaselect',ierr
      if(ierr.ne.0) then                                                      
	    if(plotoption.eq.0.or.plotoption.eq.1) then
	       write(message(1),700) ' Data missing ',
     *         eventid(1:50),station(ista)                              
               call xmessage(message,1,80,50.0,50.0)
	    else
	      write(6,700) ' Data mising or coda window too early ',
     *        eventid(1:60),station(ista)
 700          format(1x,a,1x,a,1x,a5)                                     
	    endif
	    write(3,700) ' Data missing or coda window too early ',
     *      eventid(1:60),station(ista)
	    go to 21
	  endif
c
      ierr=0
c
c  check if no data saturation in coda window, check max value first                            
c
      k=0
      kk1=tstart*nrate+1
      kk2=(tstart+wlen)*nrate
      do i=kk1,kk2
         k=k+1
         wav_y1(k)=signal1(i)                  ! save for second clip test
         if(abs(signal1(i)).ge.maxcount) then
	       if(plotoption.eq.0.or.plotoption.eq.1) then
	          write(message(1),700) 
     *            ' Data clipped, max value exceeded ',
     *        eventid(1:50),station(ista)                              
	          call xmessage(message,1,80,50.0,50.0)
	        else
	          write(6,700) ' Data clipped, max vlaue exceeded ',
     *        eventid,station(ista)
  	        endif
	        write(3,700) ' Data clipped, max value exceeded ',
     *          eventid,station(ista)         
	        go to 21
         endif
      enddo                                                             
c
c   second clip test
c
         call check_clipped(wav_y1,k,clipped)
         if(clipped) then
               if(plotoption.eq.0.or.plotoption.eq.1) then
                  write(message(1),700) ' Data clipped, auto check ',
     *            eventid(1:50),station(ista)
                  call xmessage(message,1,80,50.0,50.0)
                else
                  write(6,700) ' Data clipped, auto check ',
     *            eventid,station(ista)
                endif
                write(3,700) ' Data clipped, auto check ',
     *          eventid,station(ista)
                go to 21
         endif

c
c   first plot unfiltered data with coda window indicated,                      
c   here always start at left hand side, nx=1                                   
c                                                                               
	 nx=1
	 if(plotoption.ge.0) then
	    if(ny.gt.9) then
	      ny=1
	      if(plotoption.ne.2.and.plotoption.ne.-1) then 
                 text(1:39)=' Return to get next plot, q to quit    '
                 call oneline(text,39,answer,2,300.0,50.0)
                 if(answer(1:1).eq.'q'.or.answer(1:1).eq.'Q') goto 199
	     endif
             if(plotoption.ge.1) then
  	         call close_post
		 close(65)
                 if(plotoption.ne.2.and.plotoption.ne.-1) 
     *           call clear_to_alpha
	         call send_plot('codaq.eps',9)
                 open(65,file='codaq.eps',status='unknown')
	      endif		 
	      call open_display
	    endif
	  endif                                        
c
c   output which wav file
c
      if(plotoption.eq.1.or.plotoption.eq.0) then
	    write(message(1),'(1x,a)')trace_file(1:70)
	    call xmessage(message,1,80,50.0,50.0)
      else
	    write(6,'(1x,a)')trace_file
      endif
      write(3,'(1x,a)')trace_file
                         
c                                                                               
c   extract origin time t0 relative to time of first sample                     
c                                                                               
	 t0=tstart-tcoda
c                                                                               
c   get depth and magnitude from pick file                                      
c                                                                               
	 open(27,file=eventid,status='old')
	 read(27,'(38x,f5.2,13x,f3.1)') depth,smag
	 close(27)
	 idepth=depth
	 do i=1,80
	    head80(i:i)=' '
	 enddo
	 do i=1,120
	    headp(i:i)=' '
	 enddo
	 write(head80,207)
     *   station(ista),component(ista),
     *   date(1:14),idepth,smag,                                  
     *   tp,tcoda,wlen,start                                                    
 207     format(a5,a4,1x,a14,1x,'H=',i3,2x,'M=',f3.1,1x,                     
     *   1x,'TP=',f5.1,2x,'TC=',                                                
     *   f6.1,2x,'WIN=',f6.1,2x,'ST=',f4.1)                                     
c                                                                               
c   small tricks to make post script header nicer                                
c                                                                               
	 write(headp,217)
     *   station(ista),component(ista),date(1:4),
     *    date(5:8),date(9:12),date(13:14)               
     *   ,idepth,smag,                                                          
     *   tp,tcoda,wlen,start                                                    
 217     format(a5,a4,1x,a4,2x,a4,2x,a4,
     *   2x,a2,4x,'H=',i3,4x,'M=',f3.1,4x,          
     *   4x,'TP=',f5.1,4x,'TC=',                                                
     *   f6.1,4x,'WIN=',f6.1,4x,'START=',f4.1)                                  
	 if(plotoption.eq.2.or.plotoption.eq.-1)
     *   write(6,'(/,1x,a)') head80
	 flag(1)=tstart
	 flag(2)=tstart+wlen
	 flag(3)=t0
         if(check.eq.1) write(3,*)'call trace plot first time'          
	 x0=1
	 x0=10.0+(nx-1)*350
	 y0=780.0-ny*85.0
c                                                                               
c   now plot whole trace                                                                    
c                                        
	 if(plotoption.ge.0) call traceplot
     *   (x0,y0,0.0,1000.0,0.0,58.0,nsamp,                                 
     *   signal1,0.0,xsc,nrate,nstep,head80,headp,3,flag,                      
     *   0,s,tcoda,rr,1.0,q,date)                                               
c
	 ny=ny+1   ! next plot position one down
c
         if(check.eq.1)write(3,*)' traceplot called'                            
	 nnoise=tnoise*nrate

	 nsignal=wlen*nrate
	 nstart=tstart*nrate
c
c   check that not too much data has been selected
c
      if(nstart+nsignal.gt.nsamp) then
	 if(plotoption.eq.0.or.plotoption.eq.1) then
	    write(message(1),'(a)')' Window too long, skip trace'
	    call xmessage(message,1,80,50.0,50.0)
	 else
	    write(6,*)' Window too long, skip trace',
     *      station(ista),component(ista)
	 endif
	 write(3,*)' Window too long, skip trace',
     *      station(ista),component(ista)
c         write(3,*)'nstart,nsignal,nsamp',nstart,nsignal,nsamp
	 goto 21 
      endif
c
	 nn=nnoise+nsignal
	 del=1000.0/float(nrate)
	 y0=y0-150.0
c                                                                               
c   position new series of  filtered plots                                      
c                                                                               
	   if(plotoption.ge.0) then
	      if(ny.gt.9) then
		nx=1                                                            
		ny=1                                                            
		if(plotoption.ne.2.and.plotoption.ne.-1) then 
		   text(1:39)=' Return to get next plot, q to quit    '
		   call oneline(text,39,answer,2,300.0,50.0)
		   if(answer(1:1).eq.'q'.or.answer(1:1).eq.'Q') goto 199
		endif

                if(plotoption.ge.1) then
                  call close_post
                  close(65)
                  if(plotoption.ne.2.and.plotoption.ne.-1) 
     *            call clear_to_alpha
                  call send_plot('codaq.eps',9)
                  open(65,file='codaq.eps',status='unknown')
               endif
               call open_display
             endif
	  endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc               
c                                                               c               
c   go through filter and plotting loop                         c               
c                                                               c               
	  do 30 ifr=1,nfreq
c                                                               c               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc               
C                                                                               
C   CALCULATE FILTER COEFFICIENTS                                               
C                                                                               
c
c   first get correct frequency limits
c
c   linear band, if band is negative 
c
       if(band(ifr).lt.0.0) then
          FQ2=FRE(ifr)-BAND(ifr)/2.                                            
          FQ1=FRE(ifr)+BAND(ifr)/2.  
       else
          FQ1=FRE(ifr)-BAND(ifr)/2.                                            
          FQ2=FRE(ifr)+BAND(ifr)/2.  
       endif
c
c
c   geometric center frequncy is FRE, only use if band is positive
c


       if(Band(ifr).gt.0.0) then
          fq1=(-band(ifr)+sqrt(band(ifr)*band(ifr)+4.0*fre(ifr)*
     *         fre(ifr)))/2.0
          fq2=fq1+band(ifr)
c           write(3,*) fq1,fq2
       endif

c
c  if octave given, use that to calculate frequencies
c
       if(octave.gt.0) then
          fq1=fre(ifr)*2.0**(-octave/2.0)
          fq2=fre(ifr)*2.0**(octave/2.0)
       endif

c
c
c   geometric center frequncy is FRE, only use if band is positive
c


       noctave(ifr)= (alog10(fq2)-alog10(fq1))/alog10(2.0)

c
c   check if frequency too high
c
       if(fq2.ge.nrate/2.0) then
	   write(6,*)' Too high filter frequncy ****** ',fq2
	   write(3,*)' Too high filter frequncy ****** ',fq2
	   goto 30
       endif                                          
c
c   save bands for write out
c
       freq1(ifr)=fq1
       freq2(ifr)=fq2
       CALL BNDPAS(FQ1,FQ2,DEL,COF,G)                                       
c                                                                               
c   now filter whole signal from first sample to cxx 
c                                                                               
             ntotal=nstart+nsignal
             do 12 i=1,nsamp
		xsignal(i)=signal1(i) ! put in new array to not overwrite
 12          continue
             if(check.eq.1) write(3,*)'ntotal,nstart,nsignal',
     *       ntotal,nstart,nsignal
             if(check.eq.1)write(3,*)'xsig',(xsignal(i),i=1,10)
	     call remove_dc(xsignal,ntotal,dc,ntotal)
             if(check.eq.1)write(3,*)'dc',(xsignal(i),i=1,10)
             call filter(xsignal,nsamp,cof,g,2)
             if(check.eq.1) write(3,*)'signal selected,ntotal',ntotal
             if(check.eq.1)write(3,*)'filt',(xsignal(i),i=1,10)
c
c   save filtered noise
c
	     do 13 i=1,nnoise
		xnoise(i)=xsignal(i)                                            
 13          continue
c
c   save filtered coda signal, with extra points at end so rms envelope
c   can have correct window length
c                                                      
c     
	     do 14 i=1,nsamp-nstart
		xsignal(i)=xsignal(i+nstart)                                    
 14          continue                                                           
c                                                                               
c   calculate q                                                                 
c
           nss=nsignal
           if(check.eq.1) write(3,*) 'call codaq,nss',nss
           if(check.eq.1) write(3,*)(xsignal(i),i=1,10)

	   call codaq
     *     (nss,1,tcoda,xsignal,nrate,fre(ifr),rr,s,q,rms,corr,sdq)                 
	   iq=q+0.5
           if(check.eq.1) write(3,*)'codaq called',q
c                                                                               
c   determine signal to noise ratio                        
c   using the first trms secs of noise and last trms secs of the                   
c   coda signal                                                                 
c                                                                               
	   irms=trms*nrate
c          write(19,*)'irms',irms,'nsignal=',nsignal
c          write(19,*) 'signal'
c          write(19,*)(xsignal(i),i=1,100)                                                      
c          write(19,*)'noise'
c          write(19,*) (xnoise(i),i=1,100)

	   call sn
     *     (irms,nsignal-2*nrate-irms,xsignal,1,xnoise,ratio)                 
c                                                                               
c   join noise and coda signals for plotting
c             
	   do i=nsignal,1,-1
	      xsignal(i+nnoise)=xsignal(i)
	   enddo
	   do i=1,nnoise
	      xsignal(i)=xnoise(i)
	   enddo
c                                                                               
c   plot trace, first position plot window                                      
c                                                                               
	 if(plotoption.ge.0) then
	     if(nx.eq.4) then
	     nx=1
	     ny=ny+1                                                    
      
	     if(ny.gt.9) then
		nx=1                                                            
		ny=1                                                            
		 call close_post
		
                if(plotoption.ne.2.and.plotoption.ne.-1) then
		   text(1:39)=' Return to get next plot, q to quit    '
		   call oneline(text,39,answer,2,300.0,50.0)
		   if(answer(1:1).eq.'q'.or.answer(1:1).eq.'Q') goto 199
c		   call open_display
		endif
c
               if(plotoption.ge.1) then
  	         call close_post
		 close(65)
		 if(plotoption.ne.2.and.plotoption.ne.-1) 
     *           call clear_to_alpha
	         call send_plot('codaq.eps',9)
	         open(65,file='codaq.eps',status='unknown')
	      endif
  	      call open_display
	     endif
	   endif
	 endif
	 iratio=ratio
	 if(iq.ge.9999.or.iq.lt.0) iq=0
	 do 333 i=1,80
	    head(i:i)=' '
	    headp(i:i)=' '
 333     continue                                                               
	 write(head,202)fre(ifr),iq,corr,iratio
 202     format(f4.1,1x,'Q=',i4,1x,'C',f4.2,' SN',i3)                           
	 write(headp,292)fre(ifr),iq,corr,iratio
 292     format('F=',f4.1,4x,'Q=',i4,4x,'CO=',f4.2,4x,' S/N=',i3,'  ')          
	 flag(1)=tnoise
c
c   save good q-values
c
	 if((q.gt.10.0.and.q.lt.10000).and.abs(corr).ge.mincorr.
     *       and.ratio.ge.minratio) then
         ntcoda=ntcoda+1
         tcoda_all(ntcoda)=tcoda             ! save lapse time
	     nq(ifr)=nq(ifr)+1
	     qall(ifr,nq(ifr))=q
         corr_all(ifr,nq(ifr))=abs(corr)     ! save correlation coefficient
	     write(3,270)date(1:14),station(ista),component(ista),tcoda,
     *       fre(ifr),ratio,
     *       iq,corr,rms
 270         format(2x,a14,1x,a5,1x,a4,
     *       ' tc',f6.1,2x,'f ',f4.1,2x,
     *       's/n',f5.1,2x,'Q ',i4,2x,'corr ',
     *       f5.2,2x,'rms ',f4.2)
c
c   write geographical location of coda and Q value
c
             write(8,'(a14,1x,a5,1x,a4,2f7.2,6f7.1)') 
     *       date(1:14),station(ista),component(ista),qlat,qlon,
     *       fre(ifr),q,sdq,depth,dist,sqrt(depth*depth+dist*dist)
	 endif 
	 if(plotoption.eq.2.or.plotoption.eq.-1)
     *   write(6,'(1x,a40)') headp(1:40)                                        
c                                                                               
         if(check.eq.1) write(3,*)' traceplot called, noise:',nnoise            
	 x0=10.0+(nx-1)*350
	 y0=780.0-ny*85.0
c                                                                               
c   fix sec in date so plot axis time is ok, find second value                  
c   of plot start time remebering to correct for noise in front                 
c   note ,hr and min might not be right                                         
c                                                                               
	 if(ifr.eq.1) then
	    date1=date
	    read(date,'(12x,f6.3)') tt
	    tt=tt+tstart-tnoise
	    isec=tt
	    tt=tt-isec
	    isec=mod(isec,60)                                                 
	    tt=tt+isec
	    write(date1(13:18),'(f6.3)') tt                                    
ctest       write(37,*) tt,tstart,tnoise                                   
	 endif
cxx ?
	 if(plotoption.ge.0) call traceplot
     *   (x0,y0,0.0,300.0,0.0,58.0,nn-2*nrate,                                  
     *   xsignal,0.0,xsc,nrate,nstep,head,headp,1,flag,                       
     *   nnoise,s,tcoda,rr,fre(ifr),q,date1)                                    
	 nx=nx+1
c
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                  
c                                                            c                  
c                      END OF FREQUENCY LOOP                 c                  
 30     continue                                                                
c                                                            c                  
c                                                                               
c  move down one trace                                                          
c                                                                               
      ny=ny+1                                                                   
c                                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                  
c                                                                               
c                      END OF STATION LOOP                   c                  
 21     continue                                                                
c                                                            c                  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                  
c                                                                               
										
										
c                                                                               
c   back for next event station combination,                                    
c   make sure that next event starts on a new page                              
c                                                                               
	 ny=10
	 go to 3
c                                                                               
c   here after all loops                                                       
c
 99   continue										
	if(plotoption.ne.2.and.plotoption.ne.-1) then 
	    text(1:39)=' Return to continue                    '
	    call oneline(text,39,answer,2,300.0,50.0)
	endif
 199     continue                                                                
c
c   write frequencies and bands used in output file
c
	write(3,*)
	write(3,*)
	write(3,'(a5,10f12.2)')' Freq ',(fre(i),i=1,nfreq)
	write(3,'(a5,10f12.2)')' Band ',(band(i),i=1,nfreq)
	write(4,*)
	write(4,*)
	write(4,'(a5,10f12.2)')' Freq ',(fre(i),i=1,nfreq)
	write(4,'(a5,10f12.2)')' Band ',(band(i),i=1,nfreq)
c
c   close plot
c

	if(plotoption.ge.0) then 
	   if(plotoption.ne.2) call clear_to_alpha
c	   if(plotoption.eq.2) call close_post
	   if(plotoption.ge.1) then 
	     call close_post
	     close(65)
	     call send_plot('codaq.eps',9)
	  endif
	endif
c
c   calculate average q
c 
        statcomp(1)=' '   ! indicate no output in codaq.channel
	call output(0,nq,qall,nfreq,fre,cs,statcomp(1),.true.)

c
c  average correlation coefficients
c

      write(6,*)
      write(4,*)
      write(3,*)
c
      do k=1,nfreq
        do i=1,nq(k)
           xsignal(i)=corr_all(k,i)
        enddo
        call sdv(nq(k),xsignal,av_corr(k),sd_corr(k)) 
      enddo
c
      write(6,'(a,10(f7.2,1x,f4.2))')'  Corr:',
     *(av_corr(i),sd_corr(i),i=1,nfreq)
      write(3,'(a,10(f7.2,1x,f4.2))')'  Corr:',
     *(av_corr(i),sd_corr(i),i=1,nfreq)
      write(4,'(a,10(f7.2,1x,f4.2))')'  Corr:',
     *(av_corr(i),sd_corr(i),i=1,nfreq)
c
c  calculate average lapse time
c
      call sdv(ntcoda,tcoda_all,av,sd)
      write(3,*)
      write(4,*)
      write(6,*)
      write(3,*)' Average lapse time with sd', av,sd
      write(4,*)' Average lapse time with sd', av,sd
      write(6,*)' Average lapse time with sd', av,sd
      write(6,'(a,10f12.2)')'Octaves in filt. ',
     *(noctave(i),i=1,nfreq)
      write(6,'(a,10f6.2)') 'Freq. bands used ',
     *                      (freq1(i),freq2(i),i=1,nfreq)
c
c  this is end of one round of all events with  particular station(s), check
c  if another startion(s)
c
      rewind(2)   ! start from first event again
      goto 5000
c
c  end of all processing
c
 6000 continue
c
c   
      write(6,*)
      Write(6,*) '****************************************************'
      write(6,*) 'Calculate average Q for each channel****************'
      write(6,*) 


c
c--------------------------------------------------------------------------
c   get individuall channel values, first count number of different channels
c   or stations
c
     
      nstatcomp=0
      do i=1,1000
        statcomp(i)=' '
      enddo

      rewind 8
      ncombo=0
      n=5        ! average per station

      if(.not.channel_average) n=10     ! average per channel
 6500 continue
      read(8,'(15x,a10)',end=7000) stacom
c
c   find if already counted
c
      do k=1,nstatcomp
         if(stacom(1:n).eq.statcomp(k)(1:n)) goto 6600
      enddo
c
c   not counted, add
c
      nstatcomp=nstatcomp+1
      statcomp(nstatcomp)(1:n)=stacom(1:n)
     
 6600 continue
      ncombo=ncombo+1     ! count all results           
      goto 6500 ! get next

 7000 continue
      if(n.gt.5) then
           write(6,*) 'Number of different channels ',nstatcomp
      else
           write(6,*) 'Number of different stations ',nstatcomp
      endif

c      write(6,*) (statcomp(k),k=1,nstatcomp)
c      write(6,*)'n',n
c
c-------------------------------------------------------------------
c   average results for each combination, read through file codaq.area which
c   has results for each channel and frequency
c
      
      do i=1,nstatcomp
         rewind(8)
         write(3,*)
         write(3,*)'****************** ',statcomp(i)(1:n),'***********'
         write(3,*)
         do k=1,nfreq
           nq(k)=0
           do m=1,45000
             qall(k,m)=0.0
           enddo
         enddo
c
c   read whole file again
c
         do m=1,ncombo           
            read(8,'(15x,a10,14x,2f7.0)') stacom,f1,q1
            if(stacom(1:n).eq.statcomp(i)(1:n)) then
c
c   find index for frequency
c
                do k=1,nfreq
                   if(f1.eq.fre(k)) goto 7001
                enddo
 7001           continue
c
c   save for this channel
c
                nq(k)=nq(k)+1
                qall(k,nq(k))=q1
c              write(6,*)statcomp(i),k,nq(k),q1               
            endif
          enddo
c         write(6,*) 'next station'

          if(n.eq.5) statcomp(i)(6:10)=' '        
          call output(0,nq,qall,nfreq,fre,cs,statcomp(i),.false.)
c          read(5,'(a)') text
      enddo

c
c   collect individual q-values for each frequecy
c   for making statistics
c
c   open one file for each frequency
c     
      rewind(8)       
      do k=1,nfreq
         i=fre(k)
         write(text,'(a,i2,a)') 'codaq',i,'.statis'
         if(text(6:6).eq.' ') text(6:6)='0'
         open(k+70,file=text,status='unknown')
      enddo
   
c
c   read whole file again
c
         do m=1,ncombo
            read(8,'(15x,a10,14x,4f7.0)') stacom,f1,q1,depth,dist
c
c   find index for frequency
c
                do k=1,nfreq
                   if(f1.eq.fre(k)) goto 7011
                enddo
 7011           continue
c
c   write
c
                i=fre(k)
                write(70+k,*)dist,q1
          enddo

cx      write(6,*)(statcomp(i),i=1,nstatcomp)

      write(3,*)
      write(4,*)
      write(6,*)

	write(6,*)' Output of whole run in codaq.out'
	write(6,*)' Output of results in codaq1.out'
        write(6,*)" Output of index file of good q's in codaq.index"
        write(6,*)' Output of Q midpoints in codaq.area'
        write(6,*)
     *' Output of each Q-values at each frequency in codaqxx.statis'
        write(6,*)
     *' Output of summary values for each set of channels in',
     *' codaq.summary'
        write(6,*)
     *' Output of summary results for each channel in codaq.channel'
	close(3)
	close(4)
c      if(pc.and.nars.eq.0) then
c        write(6,*)' Return to stop'
c        read(5,'(a)') text
c      endif
	STOP
	END

c



	subroutine qzero
     *  (nfreq,fre,q,nq,sd,cs,cq,crms,sdcq,q0,sdq0,ss,sdss,corr,rms)
c
c   may 2017 error calculation corrected
c
c   calculates the parameters in the relationship
c   q = q0*f**ss with ss variable or ss fixed
c 
c   input:
c   nfreq:      number of frequencies
c   fre:        the frequencies
c   q:          q - values
c   sd:         standard deviation
c   nq:         number of q values for that frequncy
c   cs:         constant ss value
c
c   output:
c   crms        rms when using a constant ss
c   sdqc:       standard deviation in qc
c   cq          q0 whan using a constant ss
c   q0:         q0
c   sdq0        standard deviation in q0
c   ss:         ss
c   sdss:       standard deviation in ss
c   corr:       correlation coefficient
c   rms:        rms error of fit
c
	dimension q(10),sd(10),fre(10),nq(10),x(45000),y(45000),z(45000)
	dimension sigmay(45000)
	integer check
   
	check=0
	nn=0
	ndif=0
	cq=0.0

c
c   weight by using each value nq times jun 94, now use weight by number, also wrong,
c   now use standard deviation/q-value
c

c   at least 2 values so sd is not zero

	do 20 i=1,nfreq
	   number=nq(i)
	   if(q(i).gt.0.0.and.sd(i).gt.0) then
	      ndif=ndif+1
		 nn=nn+1
		 if(check.eq.1) 
     *           write(3,*)' qzero: fre(i),q(i)',fre(i),q(i)

		 x(nn)=alog(fre(i))
		 y(nn)=alog(q(i))
c		 sigmay(nn)=1.0/sqrt(float(number))   ! wrong, used for many years

                 sigmay(nn)=sd(i)/q(i)

		 cq=cq+y(nn)-cs*x(nn)
	   endif
 20      continue
	 if(ndif.gt.1) then
            if(check.eq.1) write(3,*)' call linfit'
	    call linfit(x,y,sigmay,nn,1,q0,sigmaq0,ss,sdss,corr)

            if(check.eq.1)write(3,*)' linfit called,q0,ss',q0,ss

            q0=exp(q0)
c
c   this from may 2017
c
            sdq0=sigmaq0*q0
c
c   this was also wrong
c	    sdq0=(10**sigmaq0-1.0)*q0  !same as 10**(sigma+q0) -10**q0 with q0 the log value

	 else
	    q0=0.0
	    sdq0=0.0
	    ss=0.0
 	    sdss=0.0
	    corr=0.0
	    rms=0.0
	 endif
	 if(nn.gt.0) then
	    q1=cq/float(nn)
c 	    cq=10.0**q1
            cq=exp(q1)
	    do 30 l=1,nn
	       crms=crms+(y(l)-q1-cs*x(l))*(y(l)-q1-cs*x(l))
	       z(l)= exp((y(l)-cs*x(l)))
 30         continue
	    if(check.eq.1)  write(6,*)' call sdv for cq'
	    call sdv(nn,z,av,sdcq)
	    crms=sqrt(crms/float(nn)) 
	 else
	    cq=0.0
	    sdcq=0.0
	    crms=0.0
	 endif
	 return
	 end



	subroutine qzero_old
     *  (nfreq,fre,q,nq,cs,cq,crms,sdcq,q0,sdq0,ss,sdss,corr,rms)
c

c  old wrong routine, not used
c
c   calculates the parameters in the relationship
c   q = q0*f**ss with ss variable or ss fixed
c 
c   input:
c   nfreq:      number of frequencies
c   fre:        the frequencies
c   q:          q - values
c   nq:         number of q values for that frequncy
c   cs:         constant ss value
c
c   output:
c   crms        rms when using a constant ss
c   sdqc:       standard deviation in qc
c   cq          q0 whan using a constant ss
c   q0:         q0
c   sdq0        standard deviation in q0
c   ss:         ss
c   sdss:       standard deviation in ss
c   corr:       correlation coefficient
c   rms:        rms error of fit
c
	dimension q(10),fre(10),nq(10),x(45000),y(45000),z(45000)
	dimension sigmay(45000)
	integer check
   
	check=0
	nn=0
	ndif=0
	cq=0.0
c
c   weight by using each value nq times jun 94, now use weight by number
c
	do 20 i=1,nfreq
	   number=nq(i)
	   if(q(i).gt.0.0) then
	      ndif=ndif+1
		 nn=nn+1
		 if(check.eq.1) 
     *           write(3,*)' qzero: fre(i),q(i)',fre(i),q(i)
		 x(nn)=alog10(fre(i))
		 y(nn)=alog10(q(i))
		 sigmay(nn)=1.0/sqrt(float(number))
		 cq=cq+y(nn)-cs*x(nn)
	   endif
 20      continue
	 if(ndif.gt.1) then
            if(check.eq.1) write(3,*)' call linfit'
	    call linfit(x,y,sigmay,nn,1,q0,sigmaq0,ss,sdss,corr)
            
            if(check.eq.1)write(3,*)' linfit called,q0,ss',q0,ss
	    q0=10.0**q0
	    sdq0=(10**sigmaq0-1.0)*q0
	 else
	    q0=0.0
	    sdq0=0.0
	    ss=0.0
	    sdss=0.0
	    corr=0.0
	    rms=0.0
	 endif
	 if(nn.gt.0) then
	    q1=cq/float(nn)
	    cq=10.0**q1
	    do 30 l=1,nn
	       crms=crms+(y(l)-q1-cs*x(l))*(y(l)-q1-cs*x(l))
	       z(l)= 10**(y(l)-cs*x(l))
 30         continue
	    if(check.eq.1)  write(6,*)' call sdv for cq'
	    call sdv(nn,z,av,sdcq)
	    crms=sqrt(crms/float(nn)) 
	 else
	    cq=0.0
	    sdcq=0.0
	    crms=0.0
	 endif
	 return
	 end
c
c
c
	 subroutine output(in,nq,q,nf,f,cc,statcomp,write_screen)
c 
c   from a vector of q values at differenat frequencies
c   the routine will calculate the different averages and
c   sums and print out in a file
c
c   in:     1: only one data set, don't calculate inverse averages
c           any other number: more than one data set
c   nq:     number of values at each frequncy
c   q:      q val 
c   nf:     number of frequncies, if nf is negative, do not write to screen
c   f:      the frequncies
c   cc:     value for fixed q frequncy dependence
c   statcomp:   channel used, if blank do not write out channel info
c
      character*80 text(2)
      character*10 statcomp
      dimension iav(10),isd(10),av(10),sd(10)
      dimension q(10,45000),nq(10),f(10)
      dimension x(45000)
      real x1,x2,x3,x4,del,sd15,sd100,sdq
      integer check
      logical write_screen  ! true: write to screen
      check=0

c
c   check if any data at all
c
      ntotal=0
      do 9 i=1,nf
	 ntotal=ntotal+nq(i)


 9    continue
      if(ntotal.lt.1)then 
	    write(3,*) 'No results'
	    write(4,*) 'No results'
	    return
      endif

      do i=1,10
        iav(i)=0
        isd(i)=0
      enddo

c
c  channel info if there
c
      if(statcomp.ne.' ') then
          write(4,*)
          write(4,'(a,a)') '****************** Channel ',statcomp
          write(4,*)
      endif

c
c   write header
c
      text(1)(1:12)='  AV Q    SD'
      write(3,205)(text(1)(1:12),i=1,nf)
      write(4,205)(text(1)(1:12),i=1,nf)
 205  format(/,'       ',8a12)
c
c  first calculate average q and standard deviation
c
      ntotal=0
      do 1 if=1,nf
	    n=nq(if)
c
c   check if any values at all
c
	    if(n.eq.0) then
	    av(if)=0.0
	    sd(if)=0.0
	    goto 1
	 endif
	 if(in.eq.1) n=1
	  do 2 k=1,n
	     ntotal=ntotal+1
             if(check.eq.1) write(3,*) 'k,if',k,if
	     x(k)=q(if,k)
 2    continue
         if(check.eq.1) write(3,*)' output,call sdv q,ntotal',ntotal
	 call sdv(n,x,av(if),sd(if))
         if(check.eq.1) write(3,*)' output: avq=',av(if)
	 iav(if)=av(if)+0.5
	 isd(if)=sd(if)+0.5
 1    continue
       
      if(check.eq.1) write(3,*)' output: call qzero av'
      call qzero
     *(nf,f,av,nq,sd,cc,cq,crms,sdcq,q0,sdq0,ss,sdss,corr,rms)
      icq=cq+0.5
      isdcq=sdcq+0.5
      iq0=q0+0.5
      isdq0=sdq0+0.5
c
c  write first header line with number of values
c
      if(in.ne.1) then
	 write(3,200) ntotal,(nq(l),l=1,nf)
	 write(4,200) ntotal,(nq(l),l=1,nf)
 200     format(/,1x,'NT=',i5,2x,10('N=',i6,4x))
      endif
c
      if(isdq0.gt.999) isdq0=999
      write(3,201)
     *(iav(i),isd(i),i=1,nf)
      write(4,201)
     *(iav(i),isd(i),i=1,nf)
 201  format('    q  ',10(i6,i6))
      write(text(1),233)'    q  ',icq,isdcq,iq0,isdq0,ss,sdss,corr
 233  format(a7,' cq0=',i3,2x,'sd=',i3,2x,
     *'q0=',i3,2x,'sd=',i3,2x,'v=',f5.2,2x,
     *'sd=',f5.2,2x,'cor=',f5.2)
c
c  calculate average of inverse values
c
      if(in.eq.1) return
      do 3 if=1,nf
	 n=nq(if)
	 if(n.ne.0) then
	    do 4 k=1,n
               if(check.eq.1) write(3,*)' q,if,k',q(if,k),if,k
	       x(k)=1.0/q(if,k)
 4          continue
            if(check.eq.1) write(3,*)' call sdv for 1/q, n=',n
	    call sdv(n,x,av(if),sd(if))

            
	    av(if)=1.0/av(if)
	    iav(if)=av(if)+0.5
	    sd(if)=av(if)*av(if)*sd(if)
   
            
	    isd(if)=sd(if)+0.5
	  else
	    iav(if)=0
	    isd(if)=0
	  endif
 3       continue
      if(check.eq.1) write(3,*)' cal qzero, 1/q, av=',av
      call qzero
     *(nf,f,av,nq,sd,cc,cq,crms,sdcq,q0,sdq0,ss,sdss,corr,rms)
      icq=cq+0.5
      isdcq=sdcq+0.5
      iq0=q0+0.5
      isdq0=sdq0+0.5
      write(3,202)
     *(iav(i),isd(i),i=1,nf)
      write(4,202)
     *(iav(i),isd(i),i=1,nf)
 202  format('  1/q  ',10(i6,i6))
      write(text(2),233)'  1/q  ',icq,isdcq,iq0,isdq0,ss,sdss,corr
c
c   write in file codaq.channel, this is average per station
c      
      if(statcomp.ne.' ') then
         icq=q0*10.0**ss+0.5   ! calculate at 10 hz
         write(12,1233)statcomp,ntotal,icq,iq0,isdq0,ss,sdss,corr
      endif
 1233 format(a10,' n=',i5,1x,'q10=',i5,2x,
     *'q0=',i3,2x,'sd=',i3,2x,'v=',f5.2,2x,
     *'sd=',f5.2,2x,'cor=',f5.2)
c
c   this is summary per data set so no station info,
c   only write if statcomp is blank indicating output isd not
c   per station
c

      if(statcomp.eq.' ')
     *write(9,243)'ntotal=',ntotal,iq0,isdq0,ss,sdss,corr

 243  format(a7,i5,2x,
     *'q0=',i3,2x,'sd=',i3,2x,'v=',f5.2,2x,
     *'sd=',f5.2,2x,'cor=',f5.2)
c
c   calculate q at given frequencies from q = q0*f**ss
c   may 2017 add correct error estimates
c
      if(q0.gt.0.00001) then
	 do 6 i=1,nf
            call calq(f(i),q0,sdq0,ss,sdss,av(i),sdq)  ! new may 2017
c may 2017	    av(i)=q0*f(i)**ss
	    iav(i)=av(i)+0.5
c may 2017	    isd(i)=av(i)*sdq0/q0
            isd(i)=sdq
 6       continue
      else
	 do 7 i=1,nf
	    iav(i)=0
	    isd(i)=0
 7       continue
      endif
      write(3,203)
     *(iav(i),isd(i),i=1,nf)
      write(4,203)
     *(iav(i),isd(i),i=1,nf)
 203  format('f:1/q  ',10(i6,i6))

      write(3,*)
c
c   calculate error on q 10 and q 1.5 Hz assuming the log log line 
c   moves parralel to lsq line, may 2017 not anymore
c
     
c may 2017      x1=alog10(q0+sdq0)
c      x2=alog10(q0-sdq0)
c      del=(x1-x2)/2.0
c      x2=alog10(q0)+ss*alog10(1.5)  ! 1.5 hz
c      x3=10**(x2+del)
c may 2017      x4=10**(x2-del)
c new may 2017

      call calq(1.5,q0,sdq0,ss,sdss,x2,sd15)
      call calq(10.0,q0,sdq0,ss,sdss,x2,sd100)

c      sd15=(x3-x4)/2

c      x2=alog10(q0)+ss*alog10(10.0)
c      x3=10**(x2+del)
c      x4=10**(x2-del)
c      sd100=(x3-x4)/2

      write(3,'(a,2f6.1,3x,2f6.1)')'Q at 1.5 and 10 Hz with sd ',
     *q0*1.5**ss,sd15,q0*10.0**ss,sd100
      write(4,*)
      write(4,'(a,4f6.1)')'Q at 1.5 and 10 Hz with sd ',
     *q0*1.5**ss,sd15,q0*10.0**ss,sd100
c
c  write regresion results
c
      write(3,*)'  '
      write(3,'(a)') text(1)
      write(3,'(a)') text(2)
      write(4,*)'  '
      write(4,'(a)') text(1)
      write(4,'(a)') text(2)
c
c   write on screen
c
      if(write_screen) then
        do i=1,16
	     backspace 3
        enddo
        do i=1,16
	     read(3,'(a)')text(1)
	     write(6,'(a)') text(1)
        enddo
      endif

      
      return
      end
c
c
c
C

c      SUBROUTINE LINFIT_old
C

c   used before may 2017
c  
C  PURPOSE
C   MAKE A LEAST-SQUARES FIT TO DATA WITH A STRAIGHT LINE
C       Y = A + B*X
C
C  USAGE
C    CALL LINFIT (X, Y, SIGMAY, NPTS, MODE, A, SIGMAA, B, SIGMAB ,R)
C
C  DESCRIPTIONMETERS
C      
C       X       - ARRAY OF DATA POINTS FOR INDEPENDENT VARIABLE 
C       Y       - ARRAY OF DATA POINTS FOR DEPENDENT VARIABLE
C       SIGMAY  - ARRAY OF STANDARD DEVIATIONS FOR Y DATA POINTS
C       NPTS    - NUMBER OF PAIRS OF DATA POINTS
C       MODE    - DETERMINES METHOD OF WEIGHTING LEAST-SQUARES FIT
C                 +1 (INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C                  0 (NO WEIGHTING) WEIGHT(I)=1.
C                 -1 (STATISTICAL)  WEIGHT(I)=1./Y(I)
C       A       - Y INTERCEPT OF FITTED STRAIGHT LINE
C       SIGMAA  - STANDARD DEVIATION OF A
C       B       - SLOPE OF FITTED STRAIGHT LINE 
C       SIBMAB  - STANDARD DEVIATION OF B
C       R       - LINEAR CORRELATION COEFFICIENT
C
C  SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C    NONE
C  
C  MODIFICATIONS FOR FORTRAN II
C    OMIT DOUBLE PRECISION SPECIFICATIONS
C    CHANGE DSQRT TO SQRTF IN STATEMENTS 67,68, AND 71

      subroutine linfit_old(x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
      dimension x(1),y(1),sigmay(1)
c
c   accumulate weighed sums
c
 11   sum=0.0
      sumx=0.0
      sumy=0.0
      sumx2=0.0
      sumxy=0.0
      sumy2=0.0
 21   continue
      do 50 i=1,npts
	x1=x(i)
	y1=y(i)
	if(mode) 31,36,38
 31     if(y1) 34,36,32
 32     weight=1.0/y1
	go to 41
 34     weight=1.0/(-y1)
	go to 41
 36     weight=1.0
	go to 41
 38     weight=1.0/sigmay(i)**2
 41     sum=sum+weight
	sumx=sumx+weight*x1
	sumy=sumy+weight*y1
	sumx2=sumx2+weight*x1*x1
	sumxy=sumxy+weight*x1*y1
	sumy2=sumy2+weight*y1*y1
 50   continue
c
c   calculate coefficients and standard deviations
c
 51   delta=sum*sumx2-sumx*sumx
      a=(sumx2*sumy-sumx*sumxy)/delta
 53   b=(sumxy*sum-sumx*sumy)/delta
c
c  patched up to use same varnce at all times jh jun 94
c
c61   if(mode) 62,64,62 
c62   varnce=1.0
c     go to 67
 64   c=npts-2
c
c   modified to not crash with only 2 points
c
      if(c.gt.0) then
	 varnce=(sumy2+a*a*sum+b*b*sumx2
     *   -2.0*(a*sumy+b*sumxy-a*b*sumx))/c
      else
	 varnce=0.0
      endif
 67   sigmaa=sqrt(varnce*sumx2/delta)
 68   sigmab=sqrt(varnce*sum/delta)
 71   r=(sum*sumxy-sumx*sumy)/
     *sqrt(delta*(sum*sumy2-sumy*sumy))
      return
      end

c      SUBROUTINE LINFIT
c
c   used after may 2017
C  
C  PURPOSE
C   MAKE A LEAST-SQUARES FIT TO DATA WITH A STRAIGHT LINE
C       Y = A + B*X
C
C  USAGE
C    CALL LINFIT (X, Y, SIGMAY, NPTS, MODE, A, SIGMAA, B, SIGMAB ,R)
C
C  DESCRIPTIONMETERS
C      
C       X       - ARRAY OF DATA POINTS FOR INDEPENDENT VARIABLE 
C       Y       - ARRAY OF DATA POINTS FOR DEPENDENT VARIABLE
C       SIGMAY  - ARRAY OF STANDARD DEVIATIONS FOR Y DATA POINTS
C       NPTS    - NUMBER OF PAIRS OF DATA POINTS
C       MODE    - DETERMINES METHOD OF WEIGHTING LEAST-SQUARES FIT
C                 +1 (INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C                  0 (NO WEIGHTING) WEIGHT(I)=1.
C                 -1 (STATISTICAL)  WEIGHT(I)=1./Y(I)
C       A       - Y INTERCEPT OF FITTED STRAIGHT LINE
C       SIGMAA  - STANDARD DEVIATION OF A
C       B       - SLOPE OF FITTED STRAIGHT LINE 
C       SIBMAB  - STANDARD DEVIATION OF B
C       R       - LINEAR CORRELATION COEFFICIENT
C
C  SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C    NONE
C  
C  MODIFICATIONS FOR FORTRAN II
C    OMIT DOUBLE PRECISION SPECIFICATIONS
C    CHANGE DSQRT TO SQRTF IN STATEMENTS 67,68, AND 71

      subroutine linfit(x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
      dimension x(*),y(*),sigmay(*)
c
c   accumulate weighed sums
c
 11   sum=0.0
      sumx=0.0
      sumy=0.0
      sumx2=0.0
      sumxy=0.0
      sumy2=0.0
 21   continue
      do 50 i=1,npts
	x1=x(i)
	y1=y(i)
	if(mode) 31,36,38
 31     if(y1) 34,36,32
 32     weight=1.0/y1
	go to 41
 34     weight=1.0/(-y1)
	go to 41
 36     weight=1.0
	go to 41
 38     weight=1.0/sigmay(i)**2
 41     sum=sum+weight
	sumx=sumx+weight*x1
	sumy=sumy+weight*y1
	sumx2=sumx2+weight*x1*x1
	sumxy=sumxy+weight*x1*y1
	sumy2=sumy2+weight*y1*y1
 50   continue
c
c   calculate coefficients and standard deviations
c
 51   delta=sum*sumx2-sumx*sumx
      a=(sumx2*sumy-sumx*sumxy)/delta
 53   b=(sumxy*sum-sumx*sumy)/delta
c
c  patched up to use same varnce at all times jh jun 94
c
 61   if(mode) 62,64,62 ! was c
 62   varnce=1.0        !was c
      go to 67          ! was c
 64   c=npts-2
c
c   modified to not crash with only 2 points
c
      if(c.gt.0) then
	 varnce=(sumy2+a*a*sum+b*b*sumx2
     *   -2.0*(a*sumy+b*sumxy-a*b*sumx))/c
      else
	 varnce=0.0
      endif
 67   sigmaa=sqrt(varnce*sumx2/delta)
 68   sigmab=sqrt(varnce*sum/delta)
 71   r=(sum*sumxy-sumx*sumy)/
     *sqrt(delta*(sum*sumy2-sumy*sumy))
      return
      end

C############################################################################
C
c
      subroutine codaselect(eventid,station,component,start,absstart,
     *           vpvs,window,select,date,tp,tcoda,tstart,n,rate,error,
     *           out,plotoption,trace_file,qlat,qlon,dist)           
c                                                                               
c                                                                               
c   select a window for coda Q analysis                                         
c                                                                               
c   input:  eventid:  eventid (file name)                                       
c           station:  station to use
c           component: component to use                                            
c           start:    start time from origin time in units of                   
c                     s-travel times, usually 2. See below.                     
c           absstart: absolute start time from origin in secs. If               
c                     less than what is determined by start, return             
c                     with error. If = 0, only user parameter start as          
c                     criteria.                                                 
c           window:   time window (secs)
c           vp/vs:    vp vs ratio                                        
c           select:   1: only the coda window is selected                       
c                     2: the trace to end of coda window                        
c                     3: the whole trace                                        
c           out:      logical unit number for written output                    
c                                                                               
c   output: date:     date of event y-m-d-h-m-sec.xxx                           
c           signal:   trace data                                                
c           tp:       P travel time                                             
c           tcoda:    time from origin (secs) of first sample in window         
c           tstart:   time of first sample in window relative to start          
c                     of data file                                              
c           n:        number of points in window                                
c           rate:     samples pr sec                                            
c           error:    number of errors found, should be zero                    
c           plotoption: see main prog
c           trace-file: trace file name
c           qlat,qlon : lat-lon of mid pont event station
c           dist      : distance
c                                                                               
      implicit none                                                             
      include 'seidim.inc'
      include 'waveform.inc'
      include 'seisan.inc'
      character*80 eventid                                                      
c-- trace file name                                
      character*80 trace_file
c-- dummy                                               
c     character*80 dummy                
c-- station code                                  
      character*5  station,sta          
      integer plotoption
c-- component                                           
      character*4  comp,component                       
c-- date and time                                       
      character*18 date                 
c-- phase                                               
      character*1  phase                
c-- selector, see above                                    
      integer select                    
c-- see above                             
      real tcoda,start,tstart,window    
c-- ---------                                            
      real absstart,tp                  
c-- ---------                             
      integer n                         
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
c-- ptime                                               
      integer phour,pmin                
c-- -----                                 
      real    psec                      
c-- abs times                            
      double precision ptime,wtime,otime        
c-- print out logical unit                                    
      integer out                       
c-- sample rate                                              
      integer rate                      
c-- ------------                                            
      real    rrate                     
c-- error variables                                     
      integer error                
c-- channel number to select
      integer chan                      
c-- start time and window(secs) selected                    
      real t1,windo 
c-- vpvs
      real vpvs                    
c-- help variables                                
      integer   i,j,k,n1,n2     
c-- seismic signal                                         
      character*80 message(2)    ! ERROR MESSAGES
c-- output from remove_padding,not usedB  
c     double precision  pfsec           
      integer   n_trace_file     ! number of trace files for one event
      character*80 data(max_data)     ! number of header lines in s-file
      real olat,olon   ! epicenter
      real qlat,qlon   ! midpoint station-epiceter
      real az,dist     ! azimuth and distance
      integer NSTAT,NPHAS,NHEAD,NRECORD,ID  ! for indata
      character*1,TYPE,EXP
c                            
						   
      error=0                                                                   
      check=0                  
c                                                                               
c   open and read data from pick file                                           
c                                                                               
      open(25,file=eventid,status='old',err=66)
      goto 67
 66   continue
	 error=error+1
	 if(plotoption.eq.1.or.plotoption.eq.0) then
	    write(message(1),'(a,a)')'No such file: ',eventid(1:60)
	    call xmessage(message,1,80,50.0,50.0)
	 else
	    write(6,'(a,a)')'No such file: ',eventid
	 endif
	 write(3,'(a,a)')'No such file: ',eventid
	 return
 67   continue
      read(25,'(1x,i4,1x,2i2,1x,2i2,f5.1,3x,f7.3,f8.3)')                                     
     *oyear,omonth,oday,ohour,omin,osec,olat,olon                                         
c                                                                               
c   get absolute origin time in secs                                            
c                                                                               
      call timsec(oyear,omonth,oday,ohour,omin,osec,otime)                      
c                                                                               
c   find station and read p-time                                                
c                                                                               
 4    continue                                                                  
	 read(25,'(1x,a5,4x,a1)',end=5)
     *   sta,phase                                                              
	 if(sta.eq.station.and.phase.eq.'P') go to  6
	 go to 4
 5    continue                                                                  
      if(plotoption.eq.0.or.plotoption.eq.1) then
	     write(message(1),200) station,eventid(1:40)
	     call xmessage(message,1,80,50.0,50.0)
      else
	     write(out,200) station,eventid(1:40)
 200     format('P for station ',a5,' not found for event',a40)                  
      endif
      write(3,200) station,eventid(1:40)                                      
      error=error+1                                                             
      close(25)                                                                 
      return                                                                    
c                                                                               
c   station and p-phase found, now read it                                      
c                                                                               
 6    continue                                                                  
      backspace 25                                                              
      read(25,'(1x,a5,4x,a1,7x,2i2,f6.1,42x,f5.0,1x,f3.0)',end=5)                                
     *sta,phase,phour,pmin,psec,dist,az                                                 
c                                                                               
c   calculate abs ptime                                                         
c                                                                               
      call timsec(oyear,omonth,oday,phour,pmin,psec,ptime)                      
c                                                                               
c   calculate start of coda from origin                                         
c                                                                               
      tcoda=(ptime-otime)*start*vpvs                                            
c                                                                               
c   calculate P travel time                                                     
c                                                                               
      tp= ptime-otime 
c
c   calculate midpoint between station and event
c
       qlat=olat+(dist/222.4)*cos(az/57.3)
       qlon=olon+(dist/222.4)*sin(az/57.3)/cos(olat/57.3)
c 
c       write(17,*) olat,olon,qlat,qlon,dist,az                                                          
c                                                                               
c   check if absolut start time is used                                         
c                                                                               
      if(absstart.gt.0.0) then                                                  
 	   if(absstart.lt.tcoda) then                                             
	    if(plotoption.eq.0.or.plotoption.eq.1) then
		   write(message(1),'(a,a)') sta,eventid(1:50)                           
		   call xmessage(message,1,80,50.0,50.0)
	    else
		   write(out,202) sta,eventid
 202       format(1x,'abs time too early, event too far', 2x,a4,2x,a50)                         
	    endif
            write(3,202) sta,eventid                                          
	    error=error+1
	    close(25) 
	    return
	 else
	    tcoda=absstart
	 endif
      endif                                                                    
c                                                                               
c   get trace data file names, first read header lines    
c                                                                               
      rewind 25                                                                 
      call indata
     *(25,NSTAT,NPHAS,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      close(25)
c      i=i-1

      call auto_tr(data,nhead,nrecord,n_trace_file,wav_filename)
c
c   check if files exist and where
c
       k=0
       do i=1,n_trace_file
          call  get_full_wav_name(wav_filename(i),trace_file)
          if (trace_file.eq.' ') then
             if(plotoption.eq.0.or.plotoption.eq.1) then
                write(message(1),'(a,1x,a)') ' Missing file: ',
     *          wav_filename(i)(1:60)
                call xmessage(message,1,80,50.0,50.0)
             else
                write(6,'(a,1x,a)') ' Missing file: ', 
     *          wav_filename(i)
             endif
             write(3,'(a,1x,a)') ' Missing file: ', 
     *       wav_filename(i)
         else
            k=k+1
            wav_filename(k)=trace_file
         endif
       enddo
       n_trace_file=k
       write(6,*)' Number of wav files', n_trace_file  
       if(check.eq.1) write(6,*)' Number of wav files', n_trace_file
c
c   check if any name given
c
      if(n_trace_file.eq.0) then
         error=error+1
         call clear_to_alpha
         write(6,'(a,a)')
     * ' No waveform file given for event', data(1)(1:20)
         error=error+1
         if(plotoption.eq.0.or.plotoption.eq.1) call open_display
         return
      endif

c          
c   initialize waveform file info
c
      call wav_init
c         
c   read all header information from all files
c         
      wav_nfiles=n_trace_file
       if(check.eq.1) write(3,'(a,a)')' Read headers from files:'
       do i=1,wav_nfiles
          if(check.eq.1)write(3,'(1x,a)') wav_filename(i)(1:80)
          call read_wav_header(i)
       enddo
       if(check.eq.1) then
         do i=1,wav_nchan
           write(3,*) wav_stat(i)
         enddo
       endif          

c   find channel number corresponding to station and                            
c   component, also get header time                                
c            
      comp=component                                                        
                                  
      call wav_find_chan(station,comp,chan)

      if(chan.eq.0) then
         error=error+1                                                  
        if(plotoption.eq.1.or.plotoption.eq.0) then
           write(message(1),'(a,a,a)')
     *     ' Station and component not found in trace file(s)  ',
     *     station,comp
           call xmessage(message,1,80,50.0,50.0)
        else
           write(6,'(a50,a5,1x,a4)')
     *     ' Station and component not found in trace file(s): ',
     *     station,comp
        endif
        write(3,'(a50,a5,1x,a4)')
     *      ' Station and component not found in trace file(s)  ',
     *      station,comp
	    return
       endif
c                                                                               
c   save date                                                                   
c                                                                               
      write(date,'(i4,4i2,f6.3)') 
     *wav_year(chan),wav_month(chan),wav_day(chan),
     *wav_hour(chan),wav_min(chan),wav_sec(chan)                
c                                                                               
c   calculate start time for coda window relative to first                      
c   sample in trace data                                                        
c                                                                               
      wtime=wav_abs_time(chan)
      tstart=tcoda-(wtime-otime)                                                
c                                                                               
      if(check.eq.1)                                                            
     *write(3,*)'check: ptime,otime,tcoda,tstart,wtime',                      
     * ptime,otime,tcoda,tstart,wtime                                           
c                                                                               
c  get trace data, first check how much should be selected                      
c                                                                               
      t1=tstart                                                                 
      if(select.eq.2) then                                                      
          t1=0.0                                                                 
          windo=window+tstart                                                    
      endif                                                                     
      if(select.eq.3) then                                                      
	     t1=0.0                                                          
c-- indicating whole trace                                    
	     windo=0.0              
      endif                                                                     
c                                                                               
c   read whole trace                      
c           
      if(check.eq.1) write(3,*)'Before read, chan',chan                                                                    
      call wav_read_channel(chan)
c
c  save component since it might have been blank
c

      component=wav_comp(chan)
       rrate=wav_rate(chan)
       nsamp=wav_nsamp(chan)
      if(check.eq.1) write(3,*)'wav-read,nsamp',nsamp
c
c   whole trace
c
      if(windo.eq.0.0) windo=(nsamp-1)/rrate                                    
c                                                                               
c   select window                                                               
c                                                                               
      n1=t1*rrate+1                                                             
      n2=(t1+windo)*rrate+1                                                     
      j=0                                                                       
      do i=n1,n2                                                                
        j=j+1                                                                   
        signal1(j)=signal1(i)                                                     
      enddo                                                                     
      n=j                                                                       
      if(check.eq.1) write(3,*) 'n1,n2',n1,n2
c                                                                               
      rate=rrate                                                                
c                                                                               
      if(check.eq.1)write(3,*)'signal1',(signal1(i),i=1,20)                     
      if(check.eq.1)write(3,*)' rate,t1,n',rate,t1,n                           
c                                                                               
      return                                                                    
      end                                                                       
										
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
										
										
										
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
      subroutine codaq(n,n1,t1,signal,nrate,f,v,s,q,rms,corr,sdq)                   
c                                                                               
c   calculates codaq from a filtered signal                                     
c                                                                               
c   input:   n:      number of points to analyze                                
c            n1:     first point to use in data vector signal                   
c            t1:     time from origin of point n1                               
c            signal: the filtered trace data                                    
c            nrate:  sample rate                                                
c            f:      frequncy of bandpass filter                                
c            v:      spreading parameter                                        
c                                                                               
c   output:     s:   envelope amplitude at t=0                                  
c               q:   q                                                          
c             rms:   rms of the coda q fit                                      
c            corr:   correlation coefficient of fit                             
c
       implicit none
       real signal(*),env(10000),sigmay(10000)                                       
       real x(10000)
       integer ismo,nrms,i,n1,n,nenv,nrate
       integer check
       real f,v,s,q,rms,corr,a,b,delt,t1
       real sigmaa, sigmab,sdq   ! standard deviation in a and b and q
       common /work/x,env
c                                                                               
c   some variables and parameters                                               
c                                                                               
c   ismo:  number of times envelope is smoothed                                 
c   nenv:  number of points in evevelope                                        
c   nrms:  number of points in sliding 5 cycle rms window                       
c   x   :  work array                                                           
c
       check=0
       if(check.eq.1) write(3,*)'n,n1,t1,nrate',n,n1,t1,nrate
       ismo=4 
       nrms=nrate*5.0/f 
c      nrms=nrate*3 !test with fixed 3 s window                                                       
c                                                                    
c   find envelope, add extra points for rms                                                        
c                                                                    
       do 1 i=1,n+nrms                                                               
	  x(i)=signal(i+n1-1)
 1     continue
       if(check.eq.1) write(3,*)' call erms,nrms=',nrms
c
c   add nrms to n so returned nenv is equal to n. window length
c   then does not depend on f. this was not done before july 2015 
c   resulting in a bit lower q0
c
       call erms(x,n+nrms,nrms,ismo,env,nenv)
       if(check.eq.1) write(6,*) 'erms called'
c                                                                               
c   genearate time trace                                                        
c                                                                               
	delt=1.0/float(nrate)
	x(1)=t1+float(nrms)*delt/2.0
	do 2 i=2,nenv
	   x(i)=x(i-1)+delt
 2      continue
        if(check.eq.1) write(3,*)'n,nenv',n,nenv
        if(check.eq.1) write(3,*)'delt',delt
c                                                                               
c   calculate q                                                                 
c                                                                               
	 do 3 i=1,nenv
	    if(x(i).le.0.0 .or. env(i).le.0.0) then
	       x(i)=0.1
	       env(i)=0.1
                write(6,*)' x(i) zero or negative *******'                      
	    endif
	    env(i)=alog(env(i))+v*alog(x(i))
 3       continue
c
c   write out envelope if testing, only for  one frequency
c
         check=0
         if(check.eq.1) then                             
            if(abs(8.0-f).lt.0.2) then
              do i=1,nenv
                write(33,*) x(i),env(i)
              enddo
            endif
         endif

c	 call lsqlin(nenv,x,env,a,b,corr,rms)
c   change to linfit to be able to calcualte sd in Q

         call linfit(x,env,sigmay,nenv,0,a,sigmaa,b,sigmab,corr)
	 q=-3.1416*f/b
         sdq=(q*sigmab/(-b))

c         write(6,*) q,sdq
	 if(abs(a).gt.20.) then
	   a=20.0
c           write(6,'(a,a,a)')
c     *      ' Station and component not found in trace file  ',
c     *      station,comp
	   write(6,*)' Very large envelope **********'
	   write(3,*)' Very large envelope **********'
	 endif
	 s=exp(a)
	 return
	 end
										
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
      subroutine sn(n,n1,x1,n2,x2,ratio)                                        
c                                                                               
c   calculates rms signal to noise ratioes between two                          
c   data sets                                                                   
c                                                                               
c   input:  n:     number of points used for rms calculation                    
c           n1:    index of first point to use in data set 1                    
c           x1:    data set 1                                                   
c           n2:    index of first point to use in data set 2                    
c           x2:    data set 2                                                   
c           ratio: rms ratio between data set 1 and 2                           
c                                                                               
c                                                                               
      dimension x1(*),x2(*)                                                     
      integer check                                                             
      check=0                                                                   
      rms1=0                                                                    
      rms2=0                                                                    
      do 1 i=1,n                                                                
	 rms1=rms1+x1(i+n1-1)*x1(i+n1-1)
	 rms2=rms2+x2(i+n2-1)*x2(i+n2-1)
 1    continue                                                                  
      if(check.eq.1)write(3,*)                                                  
     *'rms1,rms2,n,n1,n2',rms1,rms2,n,n1,n2                                     
      if(rms2.eq.0.0)then                                                              
	    rms2=10000.0
	    write(6,*)' zero rms ****************'
	    write(3,*)' zero rms ****************'                           
      endif                                                                     
      ratio=rms1/rms2                                                           
      ratio=sqrt(ratio)                                                         
      return                                                                    
      end                                                                       
										
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
      subroutine traceplot                                                      
     *(x0,y0,x1,x2,y1,y2,n,y,xo,xsc,rate,nstep,head,headp,                      
     *nflag,flag,noise,s,tcoda,rr,f,q,date)                                  
c                                                                               
c                                                                               
c   jul 92 by jh   changed to tektronics                                        
c                                                                               
c   plots a trace of data in a fixed frame                                      
c   note: plot is rotated 90 deg counter clockwise                              
c                                                                               
c   input:   x1,x2,y1,y2: frame and size of plot                                
c            n:     number of points  in input vector y                         
c            y:     n samples                                                   
c            xo:    time of first sample (sec)                                  
c            xsc:   x-scale in tek units/sec                                           
c            nstep: steps between samples to be plotted, 1 plots all            
c            rate:  sample rate                                                 
c            head:  header information to be plotted above trace                
c            headp: ---------------------- on post script                       
c            nflag: number of flags to be plotted along trace                   
c            flag:  time of flags to plotted along trace, in same               
c                   time scale as xo.                                           
c            noise: number of noise samples in front of signal, if              
c                   zero, do not plot following relation                        
c            s,tcoda,rr,f,q : parameters in the theoretical coda                
c                   decay curve  a = s*(tcoda**-rr)*exp(-pi*tcoda*f/q)          
c                   to be plotted on top of trace                               
c            date   date and time                                               
c                   
      include 'seiplot.inc'                                                            
      integer n,rate,nflag,check                                                
      dimension env(10000)
      real y(*)                                                                 
      integer delsec                                                            
      character*18 date                                                         
      character*120 headp                                                       
      real x0,y0,xo,ym,xsc,flag(*)                                              
      character*80 head,text                                                    
										
       dimension yy(10000)                                                       
       common /work/yy,env
c -----------------------------------------------------------------             
       check=0                                                                    
c                                                                               
c   plot frame, define size by x1,x2,y1,y2                                      
c                                                                               
      ym=(y2-y1)/2.0      
      call xset_color(color_frame)                    
      call xmovabs(x1+x0,y1+y0)                                                 
      call xdrwabs(x1+x0,y2+y0)                                                 
      call xdrwabs(x2+x0,y2+y0)                                                 
      call xdrwabs(x2+x0,y1+y0)                                                 
      call xdrwabs(x1+x0,y1+y0)                                                 
c                                                                               
c   find how much data there is room for, scale and plot it                     
c                                                                               
      dx=xsc/(float(rate))                                                      
      nx=(x2-x1)/dx                                                             
c      write(6,*)'n,nx',n,nx                                                    
c      write(6,*)(y(i),i=1,20000)                                               
      if(nx.gt.n) nx=n                                                          
      ymax=0.0                                                                  
      do 1 i=1,nx                                                               
	 if(abs(y(i)).gt.ymax) ymax=abs(y(i))
 1    continue                                                                  
c      write(6,*)'ymax',ymax                                                    
c                                                                               
c   check for ymax equal zero                                                   
c                                                                               
      if(ymax.lt.10e-20) ymax=10e-20                                            
      factor=(y2-y1)/(ymax*2.0)                                                 
c                                                                               
c   plot ymax at end of trace                                                   
c                                 
      call xset_color(color_axis_not)         
      i=ymax+0.5
      write(text,'(i10)') i                                                
      call tchars(text,10,x2-140.0+x0,y2-25+y0)                                 
c                                                                               
c  not standard fontsize and location for post script                           
c                                                                               
      call fontsize(0,2.0)                                                      
      call pchars(text,10,x2-60.0+x0,y2-10+y0)                                  
										
c                                                                               
c   plot trace and calculate envelope values                                    
c                                                                               
      dd=1.0/float(rate)                                                        
c      x=x1                                                                      
      t=tcoda                                                                   
      if(check.eq.1) write(3,*)'env(1)',env(1)                                  
      j=0                                                                       
      do 3 i=1,nx,nstep                                                         
	 j=j+1
c         x=x+dx*nstep                                                           
	 if(i.ge.noise.and.noise.ne.0) then                             
      
	    env(i)=s*t**(-rr)*exp(-3.1416*f*t/q)
	    t=t+dd*nstep
	 endif
	 yy(j)=y(i)*factor+ym+y0
 3    continue                       
      call xset_color(color_trace)         
      call xdrwvec(j,x1+x0,yy,dx*nstep)
c                                                                               
c   plot envelope fit, skip some values                                         
c                                                                               
       if(check.eq.1) write(3,*)' traceplot: env',(env(i),i=1,10)               
       if(noise.gt.0) then                                                      
	  newstep=nstep*5
c          x=x1+x0+dx*noise                                                               
	  j=0
	  do 7 i=1,nx,newstep
	     if(i.ge.noise) then
		j=j+1                                                           
		yy(j)=env(i)*factor+ym+y0                                       
	     endif
 7        continue                
	  call xset_color(color_spec)         
	  call xdrwvec(j,x1+x0+dx*noise,yy,dx*newstep)
	endif
c                                                                               
c   plot header, different for tek and postscript                               
c                                                                               
      continue                       
      call xset_color(color_title)         
      call tchars(head,80,x1+x0,y2+1+y0)                                        
      call fontsize(0,2.7)                                                      
      call pchars(headp,120,x1+x0,y2+5+y0)                                   
c                                                                               
c   plot flags                                                                  
c                                                                               
      if (nflag.ne.0) then        
	 call xset_color(color_pic)           
	 do 4 i=1,nflag
	    x=(flag(i)-xo)*float(rate)*dx+x1
	    call xmovabs(x+x0,y1+y0)
	    call xdrwabs(x+x0,y2+y0)
	    call xdrwabs(x-9.0+x0,y2-9.0+y0)
	    call xmovabs(x+x0,y2+y0)
	    call xdrwabs(x+9+x0,y2-9.0+y0)
 4       continue
       endif                                                                    
c                                                                               
c   plot tics on axis                                                           
c                                                                               
      read(date,'(12x,f6.3)') sec                                               
c                                                                               
c   find distance in secs between tics assuming min 100 tectronics              
c   units between tics                                                          
c                                                                               
	 i=50.0/xsc
c                                                                               
c  find distance between points to nearest 1,2,5,10 or 20 sec                   
c                                                                               
	 if(i.le.1) delsec=1
	 if(i.gt.1) delsec=2
	 if(i.gt.2) delsec=5
	 if(i.gt.5) delsec=10
	 if(i.gt.10) delsec=20
	 if(i.ge.20) delsec=30
c-- number of secs of axis                            
	 nsec=(x2-x1)/xsc                       
c-- first whole sec on axis                          
	 isec=sec/delsec+1                      
c-- time of first tic                                 
	 isec=isec*delsec                       
c-- x position first tic                           
	 x=(isec-sec)*xsc+x0                    
c-- find second tic value below 60                   
	 isec=mod(isec,60)                      
c                                                                       
	 call fontsize(0,1.5)
	 call xset_color(color_axis_not)
	 do 5 i=1,nsec/delsec
	    if(x.gt.1010) goto 5
	    write(text,'(i2)') isec
	    call pchars(text,2,x-6.0,y1+y0-8.0)
	    tsize=5
	    if(isec.eq.60.or.isec.eq.0) then
	      tsize=10
	      isec=0
	    endif
	       call xmovabs(x,y1+y0)
	       call xdrwabs(x,y1+y0+tsize)
	    isec=isec+delsec
	    x=x+xsc*delsec
 5       continue                                                               
										
       return                                                                   
       end                                                                      
										
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
C                                                                               
      SUBROUTINE ERMS(DATA,N,NRMS,ISMO,ENV,NENV)                                
C                                                                               
C   ROUTINE MAKES AN RMS RUNNING WINDOW ENVELOPE                                
C                                                                               
C   DATA: INPUT DATA                                                            
C   N   : NUMBER OF INPUT DATA                                                  
C   NRMS: NUMBER OF POINTS IN SLIDING RMS WINDOW                                
C   ISMO: NUMBER OF TIMES ENVELOPE IS SMOOTHED                                  
C   ENV : ENVELOPE                                                              
C   NENV: NUMBER OF POINTS IN ENVELOPE = N-NRMS                                 
C
      implicit none
      real DATA(*),ENV(*)
      integer n,nrms,ismo,nenv,k,i,check
      real rms
      check=0
c
      NENV=N-NRMS                                                               
      
      if(check.eq.1) then
         write(3,*)'nrms inside,nenv',nrms,nenv
         write(3,*)(data(i),i=1,10)
      endif

      DO 10 I=1,NENV
         RMS=0.0                                                                   
C                                                                               
C   RMS VALUE OVER NRMS POINTS                                          
C                                                                                       
         DO 5 K=1,NRMS                                                             
            RMS=RMS+DATA(K+I-1)*DATA(K+I-1)                                           
 5       CONTINUE                                                                  
         ENV(I)=SQRT(RMS/FLOAT(NRMS))                                              
 10   CONTINUE                                                                  
C                                                                               
C   SMOOTHE ENVELOPE                                                            
C                                                                               
      CALL SMOOTH(ENV,NENV,ISMO)                                                
      RETURN                                                                    
      END                                                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        


      subroutine calq(f,q0,sdq0,qalp,sdqalp,q,sdq)
c
c   calculate q in relation q=q0*f**qalpha and coreesponding sd error
c   j havskov with help from luis matias
c
      implicit none
      real f              ! frequency
      real q0,sdq0        ! q0 and sd
      real qalp,sdqalp    ! qalpha and and sd
      real q,sdq          ! q calcualted with sd
      real B,sdB          ! help variables
c
c   this a product of two terms A and B so final sd is
c   A*B*sqrt((sdA/A)**2+ (sdB/B)**2) where sdA and B are sd of A and 
c   B respectively. This assumes that the covariance between A and B
c   is zero. A=q0 and B=f**qalpha. So first the sd of B must be calculated
c   as sdB=(f**qalp)*alog(f)*sdqalp
c
c   see https://en.wikipedia.org/wiki/Propagation_of_uncertainty 
c
      B=f**qalp     
      sdB=(f**qalp)*alog(f)*sdqalp
      q=q0*f**qalp

      sdq=q*sqrt((sdB/B)**2+(sdq0/q0)**2)

      return
      end     
