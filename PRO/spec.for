c***********************************888888888********************************* 
C                                                                               
c	  PROGRAM SPEC  
c                                                                                   
c       By  Jens Havskov  apr 1994                                                        
c                                                                               
c       Calculates spectra, relative spectra, tstar and Q
c
c
c
c  Some notes on how Q is selected and averaged, 2 station method

c  Individual Q-calculation: Q values lower than 1 and higher than 5000
c                            are not used, the Q(f) plot might then display
c                            a long straight line. The Q=Q0*f**qalpha
c                            is calcualated from the 'good' values'.

c  Average Q of last plot:   The linear 1/Q values are averaged, 
c                            standard deviation
c                            is calcualted on the 1/Q average and 
c                            1/Q +/- standard
c                            deviation calcualated. From this, Q +/- sd is 
c                            calculated.
c                            Only the Q-values from above are used. In the 
c                            average Q 
c                            Q +/- sd, Q-values below 1 are rejected. 
c                            The Q=Q0*f**qalpha
c                           is calcualated from the 'good' values' 
c
c
c   NB:
c   variable tstar is the same as kappa, however tstar is determined and
c   kappa is input. In print out it is called kappa
c
c       Latest uppdate:                                                         
c       dec 7 94 by jh     : maxdim to max_sample
c       dec 28             : ********** version 5.0 *******************
c       feb 95             : fix
c       april              : component rotation
c       may 95 by    jh    : also make it possible to make average spectras
c                            without making relative spectras
c       may 22             : make file spec_ave.out
c       jun 2              : replace find_chan2 with findchan
C!JAB(BGS)Jun95: Removed SDV since in ../LIB/lsqlin.for
c       oct 95        jh   : new seisinc
c       mar 12 96          : new convert routine, now in picsub
c       mar 25             : check for missing response file and resp in header
c       aug 2 96           : calculate distance only in convert
c       feb 97             : fix if average spectra is zero, then no log
c       feb 24 97          : remove disp_vel from call to spec
c       feb 29             : fix bug when start time was before start of
c       nov 25             : more spectral output, interactive freq. output
c       dec    98          : calculate q
c                            waveform file, then skip trace
c       mar 2 99  jh       : -------------- version 7.0 check-------------
c                            stat to 5 , year
c       mar 10             : bug in q-calculation
C
c       mar 26 bmt         : include winplot.inc
c       may 30 jh          : power density spectrum
c       september 15       : bugs with q-option
c       may 2000 jh        : new waveform structure
c       september 15, 2000 : clear wav_resp_file before call to read_resp
c                            calculate dc for each new trace plot
c       october 24         : high accuracy, sec read wrong
c       december 14        : add peterson noise curves,more prec. spec_ave.out
c       december 20        : calculate t*
c       jan                : small bug, fix routine spec_select so do not
c                            need mulplt.inc
c       jan 30             : new parmeters mincor, maxsn, kappa
c       may 28             : correct power spectrum, small fixes
c       jun 21             : small text changes, t* to kappa
c       sep 30             : possibel to run directly from waveform files
c       jan 20 2002 jh     : gmt standard output
c       feb 11             : fix q calculation
c       feb25              : more gmt fixing
c       mar 6              : remove gmt subroutine from main program
c       apr 17             : multiple windows in same trace, parmeter n_window_total
c       may 8 2002 jh      : sd was calculated wrong in gmt output for q
c       may 27             : small bug in output of number of windows
c       jan 6, 2003        : overlap of wndows possible, add parameter w_overlap to n_window
c       feb 5         jh   : bug in multiple windows, multiple files
c       june 11            : change in file question
c       june 20 2004  jh   : plt to eps
c       november 17 06 jh  : when calculating Q, the correction for geometrical
c                            spreading had the wrong sign adding the correction
c                            instead of subtractin. thsi resulted in too low
c                            Q-values
c       7 may 2009 lo      : compute averages for stations if single event
c       oct23 2009 jh      : spectra to 300, above change introduced a bug in
c                            storage of kappa
c                            ERROR in calculation of kappa so now values are
c                            20 % smaller than before
c                            skip final plot for kappa if no events
c                            add t_star_select
c       dec 30 2009 jh     : add paz to call spectrum window, not used
c       dec 29 2010 jh     : gfortran on pc: remove winplot, implicit none
c                            remove hcplot, unir 16 to 26, 17 to 27, 
c                            remove reference to computer type
c                            remove call to tsend
c       feb 21 2011 jh     : fix colors, message about picking f and amp, small 
c                            adjustment of spectral position
c       feb 22 2011 jh     : input window size from color.def
c       mar 10 2011 jh     : argumernt to tchars must be variable
c       oct 14 2011 jh     : write log spectra in an output file spec_amp.out,
c                            write before smoothing etc, but corrected if specified,
c                            add option 5
c       nov 2 2011  jh     : fixed some array bounds , put in wav_mem_init
c                            remove more graphiscs call for option 5
c       jan 23 2012 jh     : a title had been deleted
c       mar 3  2012 jh     : graphicks bug option 0, sdistance soemtimes
c                            not calculated                         
c       nov 26 2012 jh     : remove some test output
c       jun 13 2014 jh     : initilize ymag, not done, caused zereo trace
c       sep  8 2014 jh     : fix correct component plot when rotating
c
c
      implicit none         
      include 'mulplt.inc'  ! mulplt variables
      include 'seiplot.inc' ! general plotting variables
      include 'gmt_xy_par.inc' ! for gmt input file
      include 'seisan.inc'
c
c  parameters
c
      real          av               ! average
      integer       nspectra         ! number of specras to store
      integer       nspec_points     ! number of points in each spectrum
      parameter (nspectra=300)
      parameter (nspec_points=30000)
      real out_amp(10), out_f(10)    ! specific output values
      real out_ff(10)                ! spectral frequencies selected
c
      logical       clipped          ! true if signal clipped
      character*4   component(2 )    ! component to use                                                    
      character*4   old_comp(2)      ! component read in
      real          cx(1),cy(1  )    ! return from spectra plot, not used
      character*1   c_spec(1)        ! return from spectral plot, not used
      character*80  question  ! question for one_line
      real          dist_cor  ! distance correction exponent
      character*1   exp       ! explosion indicator
      real          factor    ! correction factor for spectrum
      real          flag(5)   ! flags to be plotted e.g. origin time
      logical       first_run ! indicator of first run with one station
      logical	    power     ! acceleration power density spectrum
      logical       pc,linux,sun
      real          gain_chan1 ! factor multiplied by channel 1
      real          ttime1, ttime2     ! travel time for the 2 traces
      integer       i,ierr,nstart,j
      real          ff
      character*80  infile           ! wav input file
      integer       ievent           ! number of events for one station pairs
      integer       itstar           ! count good tstar events
      integer       k,l,ichan,kk,kkk ! help variables
      integer       make_noise_spec  ! make noise spectra(1=y,0=n)
      integer       make_rel_spec    ! make relative spectra
      integer       mag_spec         ! indicator if magnitude spectrum
c                                    ! 1: moment, 2: moment  magnitude
      real          mincor           ! minumum correlation coef. t*
      real          minsn            ! minimum signal to noise ratio, t*
      character*80  name             ! Sfile name
      integer       nset             ! number of data sets to plot
      integer       n_stat           ! numbr of stations to process, 1 or 2
      integer       npoint_set(nspectra)    ! number of points pr data set    
      integer	    ngmt_all         ! total number for traces for gmt plot
      integer nsmooth                ! number of times smoothed
      integer       nc               ! number of chars return in cx and cy
      integer       nflag            ! number of flags to be plotted
      integer       nspec            ! number of points in plot
      integer       nsamp,nsignal    ! number of samples
      integer nstat,nphase           ! see indata routine
      integer	    n_window         ! count windows used in same trace
      integer       n_window_total   ! total number of windows in one trace
      real          w_overlap	     ! amount of overlap, 1.0: no overlap, 1.3: 30% gap, 0.8: 20 % overlap	
      character*80  out_txt(30)      ! text to graphics screen
      integer       old_poption      ! save plot option
      integer       plot_pics        ! plot pics
      real          rel_spec(nspectra,nspec_points) ! relative spectras
      real          sd               ! standard deviation
      integer       sel_crit  ! seletion criteria for start time of window
      logical       sn_ok            ! indicate if sn is ok
      integer do_rotate              ! rotation or not (1 or 0)
      real          start            ! start of window in time or # of times
      character*5   station(2)       ! station to use
      character*18  main_time ! time in waveform file main header
      real          t0               ! origin time
      real          tstart           ! start of window in secs after orig. time
      character*1   type             ! event type
      character*80  text             ! help text
      real          qq0, qqalpha     ! calculate q and qalpha
      real          corr,rms         ! correlation and rmd for qq0 and qqalpha
      character*80  title            ! part of title
      character*30  xtext,ytext      ! x and y-axis text
      character*80  err_text         ! error text
      logical       wav_list         ! if true, input from waveform file name
      real          wlen             ! window length to analyse      
      real          x0,y0            ! plot lower left hand cornor
      real          x_length         ! lenght of x_axis for spectral plot
      real          x_work(max_sample)
      real          xx_work(nspec_points) ! work array
      real	    yy_work(nspec_points) ! work array
      real          x_size           ! size in tek units of x-axis spec plot
      real          y_size           ! size in tek units of y-axis spec plot
      real          start_org        ! save start time
      real          tstar(nspectra)  ! t* values
      logical       t_star           ! if true, calcualte t*
      logical       t_star_select    ! true if select after corr test
      logical       xlin             ! if true, linear x-axis
      real          aa,bb,aaa,bbb    ! help variables
      real          y_work(nspectra*nspec_points)
      integer       key              ! key return
      logical       plot             ! true if plotting 
      real          xx,yy            ! -- position
      character*19  wtext            ! part of output file name
      integer       nwtext           ! number of characters in
      real          event_spec(nspectra,nspec_points) ! save spectra from 
                                     ! traces per event
      integer       n_event_spec     ! number of traces for the above
      real          event_rate(nspectra) ! sampling rate
      integer       nlogff           ! number of frequencies
      parameter (nlogff=100)
      real          event_ff(nlogff),dff! frequencies in even log space for average spectra
                ! normalization constant for poles and zeros
c
c   peterson noise curves
c
      real low_f(22),high_f(14),low_l(22),high_l(14) ! petersen noise
      data high_f/   1.000000,    6.575773E-01,    4.948500E-01,
     *           9.691001E-02,   -5.797836E-01,   -6.627579E-01,
     *           -7.993407E-01,  -8.976271E-01,   -1.187521,
     *           -1.301030,       -2.549984,       -4.000000,
     *            -4.477121, -5.0/
      data high_l/ -91.5,-97.4,-110.5,-120.0,-97.9,-96.5,-101.0,
     *             -113.5,-120.0,-138.5,-126.0,-80.1,-65.0,-48.5/ 

      data low_f/    1.000000,    7.695511E-01,    3.979400E-01,
     *             9.691001E-02, -9.342168E-02,   -3.802112E-01,
     *            -6.334685E-01,  -6.989700E-01,  -7.781512E-01,
     *             -1.000000,       -1.079181,       -1.193125,
     *              -1.340444, -1.50, -1.65, -1.89, -2.0,
     *              -2.19,-2.51,-2.78,-4.0,-5.0/
      data low_l/   -168.0,-166.7,-166.7,-169.2,-163.7,-148.6,
     *              -141.1,-141.1,-149.0,-163.8,-166.2,-162.1,
     *              -177.5,-185.0,-187.5,-187.5,-185.0,-184.9,
     *              -187.5,-184.4,-151.9,-103.1/ 

      data out_f/0.125,0.25,0.50,1.0,2.0,4.0,6.0,8.0,10.0,16.0/
c
c   scaling values from xy_plot
c
      real xfirst,yfirst,xscale1,yscale1,ymin,ymax,xmin,xmax
      common/xy_plot_common/xfirst,yfirst,xscale1,yscale1,ymin,
     *                       ymax,xmin,xmax
c
      equivalence(x_work,com)
      include 'version.inc'
    
c                                              
      n_event_spec=0
      plot=.true.
      ymag=1.0


c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c                                                                                
c                                                            
c---unit for plot file
      plotunit=65
c---plot resolution
      resolu=3000
c---initial xwindow size in % horizontal full size
      wsize= 80

c---display not yet open
      disp_open=0
      first_run=.true.
      n_window_total=1    ! normally only one window
c
c   get seisan defaults
c
      call get_seisan_def
      itstar=0
      ngmt_all=0
c
c
      t_star=.false.       ! do not calculate t*
      xlin=.false.         ! normally log x-axis

      call gmt_xy_init     ! set default gmt parameters

      xtext=' Frequency (Hz)              '
      ytext=' Log amplitude               '
c
c   interactive input
c                                                                               
      write(6,*)' Only final plot on screen   (0) '                            
      write(6,*)' Plot all on screen          (1) '                            
      write(6,*)' Plot all on screen+laser    (2) '                            
      write(6,*)' Plot all on laser           (3) '                            
      write(6,*)' Only final plot on laser    (4) '
      write(6,*)' No plot                     (5) '                            
      read(5,*) plotoption                         
      if(plotoption.eq.5) then
        plot=.false.
        plotoption=0
      endif                            
      plotoption=plotoption-1                                                  
c
c   get size of window
c
      if(plot) then
         call get_window_size
         if(size_spec.gt.0) wsize=size_spec ! from color.def
      endif
c
c
c   open output file with summary results
c
       open(3,file='spec.out',status='unknown')
c
c   file with all gmt  spectra
c
       open(18,file='spec.scratch',status='unknown')
c
c   amplitude spectra
c
       open(25,file='spec_amp.out',status='unknown')
c
c   get inputs
c
 4847  continue
       write(6,*)'Parameter file, spec.par is default (return)'                
       read (*,'(a)') name                                                    
       if(name(1:2).eq.'  ') name='spec.par'                                  
C                                                                               
c   open parameter filer giving specs for processing                           
C                                                                               
      open(1,file=name,status='old', err=4848)
      goto 4849
 4848 continue
      write(6,*)' No such file'
      goto 4847
 4849 continue
c                                                                               
c   read parameters shared by all data sets                                     
c   each data line is separated by a comment line read by text                
c                                                                               
        read(1,'(a)') text
        read(1,*,err=4850) sel_crit
        write(3,'(1x,a59,i10)') text(1:59),sel_crit
        read(1,'(a)') text
        read(1,*,err=4850) start
        start_org=start
        write(3,'(1x,a59,f10.2)') text(1:59),start
        read(1,'(a)') text
        read(1,*,err=4850) wlen,n_window_total,w_overlap
        write(3,'(1x,a59,f6.1,i6,f6.2)') 
     *  text(1:59),wlen,n_window_total,w_overlap
        read(1,'(a)') text
        read(1,*,err=4850) nsmooth
        write(3,'(1x,a59,i10)') text(1:59),nsmooth
        read(1,'(a)') text
        read(1,*,err=4850) gain_chan1 
        write(3,'(1x,a59,f10.3)') text(1:59),gain_chan1
        read(1,'(a)') text
        read(1,*,err=4850) make_noise_spec
        write(3,'(1x,a59,i10)') text(1:59),make_noise_spec
        read(1,'(a)') text
        read(1,*,err=4850) make_rel_spec
        write(3,'(1x,a59,i10)') text(1:59),make_rel_spec
        read(1,'(a)') text
        read(1,*,err=4850) plot_pics
        write(3,'(1x,a59,i10)') text(1:59),plot_pics
        read(1,'(a)') text
        read(1,*,err=4850) ffmin,ffmax
        write(3,'(1x,a59,2f10.2)') text(1:59),ffmin,ffmax
        read(1,'(a)') text
        read(1,*,err=4850) remove_response
        write(3,'(1x,a59,i10)') text(1:59),remove_response
        read(1,'(a)') text
        read(1,*,err=4850) do_rotate
        write(3,'(1x,a59,i10)') text(1:59),do_rotate
        read(1,'(a)') text
        read(1,*,err=4850) q0,qalpha,kappa
        write(3,'(1x,a59,3f10.2)') text(1:59),q0,qalpha,kappa
        read(1,'(a)') text
        read(1,*,err=4850) dist_cor
        write(3,'(1x,a59,1f10.2)') text(1:59),dist_cor
        read(1,'(a)') text
        read(1,*,err=4850) mincor,minsn
        write(3,'(1x,a59,2f10.2)') text(1:59),mincor,minsn
        minsn=alog10(minsn)
        read(1,'(a)') text
        read(1,*,err=4850) svelocity,density
        write(3,'(1x,a59,2f10.2)') text(1:59),svelocity,density
        read(1,'(a)') text
        read(1,*,err=4850) mag_spec
        write(3,'(1x,a59,i10)') text(1:59),mag_spec
        read(1,'(a)') text
        goto 4851
 4850   continue
        write(6,*)' Error in parameter file for parameter line:'
        write(6,'(1x,a)') text
        write(6,*)' Return to stop'
        read(5,'(a)')i
        stop
 4851   continue
c
c   check parameters for internal consistency
c
        if(sel_crit.eq.4.and.n_window_total.lt.1) then
           write(6,*) ' Number of windows must be selected (>0)'
           write(6,*)' Return to stop'
           read(5,'(a)')i
           stop
        endif
        if(sel_crit.eq.4.and.n_window_total.gt.1.and.w_overlap.le.0.0)
     *  then
           write(6,*) ' Overlap must be more 0, 1.0 is no overlap'
           write(6,*)' Return to stop'
           read(5,'(a)')i
           stop
        endif
        if((svelocity.ne.0.0.and.density.eq.0.0).or.
     *  (svelocity.eq.0.0.and.density.ne.0.0)) then
           write(6,*)
     *     'Give both density and velocity, or none'
           write(6,*)' Return to stop'
           read(5,'(a)')i
            stop
        endif
        if(svelocity.ne.0.0.or.mag_spec.eq.1) then
           if(svelocity.eq.0.0) then
              write(6,*)' Give velocity and density',
     *        ' for moment or magnitude'
              write(6,*)' Return to stop'
              read(5,'(a)')i
              stop
            endif
            if(dist_cor.eq.0.0) then
               write(6,*)' Give distance correction',
     *         'for moment or magnitude'
                write(6,*)' Return to stop'
                read(5,'(a)')i
               stop
            endif
            if(remove_response.ne.1) then
               write(6,*)' Response removal must be 1 ',
     *         'for moment or magnitude'
                write(6,*)' Return to stop'
                read(5,'(a)')i
                stop
            endif
         endif
         if(q0.lt.0) then
             if(dist_cor.le.0.0) then
                write(6,*)
     *    ' You must have a distance correction when determining q'
               write(6,*)' Return to stop'
               read(5,'(a)')i
               stop
              endif
         endif
c
         power=.false.
         if(remove_response.eq.4) then   ! power spectrum
            power=.true.
            remove_response=3            ! acceleration
            ytext='db'
         endif
         if(remove_response.eq.5) then
            t_star=.true.
            xlin=.true.
            remove_response=1             ! must be displacement
         endif
c
c   set rotation flag
c
         rotate=.false.
         if(do_rotate.eq.1) rotate=.true.
c
c   determine title of spectrum
c
        if(remove_response.eq.0) title(1:12)='uncorrected '
        if(remove_response.eq.1) title(1:12)='displacement'
        if(remove_response.eq.2) title(1:12)='velocity    '
        if(remove_response.eq.3) title(1:12)='acceleration'
        if(remove_response.eq.3.and.power) title(1:12)='Acc. pow. db'
        if(mag_spec.eq.1)        title(1:12)='Moment      '
        if(mag_spec.eq.2)        title(1:12)='Magnitude Mw'
c
c set output frequencies for average event spectra
c
        dff=(alog10(ffmax)-alog10(ffmin))/float(nlogff-1)
        do i=1,nlogff
          event_ff(i)=alog10(ffmin)+dff*float(i-1)
        enddo
c                                                                               
c   open file with list of event's, process until end of file                          
c

      write(6,'(a,a)')                                                              
     *  ' CAT File or filenr.lis type file with events, ',
     *'name spec.inp is default (return)'
c      write(6,*)'filenr.lis means input is waveform files only'         
      read(5,'(a)') name                  
c      if(name(1:10).eq.'FILENR.LIS') name='filenr.lis'

        if(name(1:2).eq.' ') name='spec.inp'                                   
        open(2,file=name,status='old')
c
c   find if a filenr.lis type
c
        wav_list=.true.
        do i=1,10
           read(2,'(2x,i3,a2)',err=2837,end=2839) k,text(1:2)
           if(text(1:2).ne.'  ') wav_list=.false.
           goto 2838
 2837      continue
           wav_list=.false.
 2838      continue
        enddo
 2839   continue
        rewind(2)

c
c   check parameter
c
      if(wav_list.and.sel_crit.ne.4) then
          write(6,*)' When choosing only waveform file input,'
          write(6,*)' parameter sel_crit mut be set to 4'
	  write(6,*)' edit your parameter file and start again'
          stop
      endif

c
        if(wav_list) write(6,*) ' Input file is list of waveform files'
c
c----------------------------------------------------------------------
c   loop for pairs of station starts here label 1
c----------------------------------------------------------------------
c
 1    continue
c
c   reset start time in case multiple windows are used with sel crit 4
c
      start=start_org
c
c   this is the interactive part of the last screen of a
c   set of calcualtions idenfied by first_run being false
c
     
      question(1:42)='Cont.: any char, q: quit, mouse: f and amp'
      if(plotoption.le.1.and.(.not.first_run)) then
         if(plot) call tchars(question,42,500.0,20.0)
 111     continue
         if(.not.plot) then
           goto 99
         else
            if(plot)call xscursr(key,xx,yy)
         endif
         if (char(key).eq.'#') goto 111
         if(char(key).eq.' ') then
            xx=(xx-x0)/xscale1+xfirst
            yy=(yy-y0)/yscale1+yfirst
            if(.not.power) yy=10.0**yy
            write(out_txt(1),'(a3,2f8.2)')'f,a',10.0**xx,yy
            if(plot)call xmessage(out_txt,1,19,700.0,30.0)
            goto 111
         endif
         if(char(key).eq.'q'.or.char(key).eq.'Q') goto 99 !stop
      endif
c     first_run=.false.
c                                                                               
c   lines with pairs to process now follows, one pair pr line
c               
      write(3,*)
      read(1,'(a5,1x,a4,1x,a5,1x,a4)',end=99) 
     *station(1),component(1),station(2),component(2)  
      if(station(1).eq.' '.or.component(1).eq.' ') goto 99                                  
c
c   save component in case changed by rotation
c
      old_comp(1)=component(1)
      old_comp(2)=component(2)
c
c   a blank line should also stop program
c
      if(station(1)(1:5).eq.'     ') goto 99
c
c   reset parameters
c
      rewind 2     ! start again in event file
      ievent=1     ! event counter
      n_window=1   ! windows counter ( only if option 4 for sel_crit)
c
C-----------------------------------------------------------------------
C
C   reading loop for events (can also be windows for one trace)  to process starts here *********************        
C                                                                               
c-----------------------------------------------------------------------
c
 10   continue
c
c   initialize memory handling
c
      call wav_mem_init                                                                     
c
c   reset component in case of rotation
c
      component(1)=old_comp(1)
      component(2)=old_comp(2)
C                                                                               
C   Read next event from file until eof. If option with many windows in same file
c   is used, read next
c   segment of same file instead until end of segments                                      
C                                
      if(sel_crit.eq.4.and.n_window.le.n_window_total.   ! select next segment
     *   and.n_window.ge.2) then
         start=start+wlen*w_overlap   ! move window one down
      else                                               ! select next file or event
         if(wav_list) then   ! waveform files only
            read(2,'(7x,a)',end=60) infile 
c
c   reset overlapping windows
c
            n_window=1
            start=start_org
c
            if(infile.eq.' ') goto 60      ! no more events
            call make_new_sdata(infile,data,nhead,nrecord)
         else
C                                                                               
C   Read next event from file until eof.                                      
C                                   
            call indata
     *      (2,nstat,nphase,nhead,nrecord,type,exp,data,i)
         endif
      endif

      n_window=n_window+1
c
c   for each event, one pair of stations are processed, which gives one page
c   of plot if plot is asked for. Plot must be opened every time here if
c   plots are made for each data pair.
c                                                                               
c   if hardcopy is wanted, open plot file here
c
      if(plotoption.gt.0.and.plotoption.lt.3) then                             
         open(65,file='spec.eps',status='unknown')                            
      endif                                                                    
c
c    screen plot opened
c
      if(plotoption.ge.0)  then 
          if(plot) call open_display 
      endif
c
c   go to average spectral ratios if end of events
c
      if(nrecord.eq.0) goto 60
c
c   check that event number not out of array bounds
c
      if(ievent.gt.nspectra) then
         if(plot) call clear_to_alpha
         write(6,*)' Too many events, can be maximum ',nspectra
         goto 99
      endif
c
c   output
c
      write(3,'(1x,a)') data(1)(1:79)
      if(plotoption.eq.-1.or.plotoption.gt.1) write(6,'(1x,a)')
     *data(1)(1:79)
c                                                                               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc              
c                                                                     c              
c   process pairs of stations in a loop if relative spectra, DO 50    c              
c                                                                     c          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc          
c
      n_stat=2
      if(make_rel_spec.eq.0) n_stat=1
      do 50 ichan=1,n_stat
c                                                                               
c   read data for channel and get travel time etc., check if station is there
c
        if(station(ichan).eq.'     ') goto 55 ! go to clear, then next event                                                                               
c
c   output
c
         write(3,'(1x,a,1x,a)') station(ichan),component(ichan)
         if(plotoption.eq.-1.or.plotoption.gt.1) write(6,'(1x,a,1x,a)')
     *   station(ichan),component(ichan)
c
c   select data in CAT file, read from waveform file
c
        call spec_select(data,nhead,nrecord,rotate,                                                        
     *  station(ichan),component(ichan),
     *  sel_crit,start,tstart,t0,nsamp,ierr,err_text)
  
c                                                                               
c   check for errors                                                            
C                                                                               
        if(ierr.ne.0) then                                                      
           if(plotoption.eq.0.or.plotoption.eq.1) call clear_to_alpha 
           write(3,'(a)') err_text
           write(6,'(a)') err_text
           write(6,700) station(ichan),component(ichan),' Error CAT'
           write(3,700) station(ichan),component(ichan),' Error CAT'
           if(plotoption.ge.0) then
              write(6,*) ' Return to continue'
              read(5,'(a)') i
           endif
 700       format(1x,a,1x,a,1x,a)                                     
            if(plotoption.eq.0.or.plotoption.eq.1) call open_display 
           close(65) ! close plot file
           goto 10   ! next event                                       
        endif
c
c   put in sample rate
c
         rate=wav_rate(wav_current_chan(1))
         current_chan=wav_current_chan(1)        ! maybe needed for mulplt routines
c
c   set marker for origin time and window
c                                             
         flag(1)=tstart                                                         
         flag(2)=tstart+wlen                                                    
         flag(3)=t0                                     
         nflag=3                        
c
c   set plotting parameters
c
         x0=60.0                
         ptitle=1                ! plot title                                     
c
c  reset fonts
c
         if(plotoption.gt.0.and.plotoption.lt.3) 
     *   call fontsize(0,3.0)
         paxisnumb=0             ! no axis numbers for first trace
         if(ichan.eq.1) then
            if(make_rel_spec.eq.1) y0=665.0
            if(make_rel_spec.eq.0) y0=600.0
            if(make_rel_spec.eq.0) paxisnumb=1                                                       
         endif
         if(ichan.eq.2) then
            if(make_rel_spec.eq.1) y0=595.0
            if(make_rel_spec.ne.1) y0=565.0
            ptitle=0             ! no title for second trace
            paxisnumb=1          ! axis numbers for second trace
         endif
         height=70.0             ! trace height
         if(make_rel_spec.ne.1) height=120.0
c                                                                               
c   transfer data to working array, correct for a possible gain                 
c               
         if(gain_chan1.ne.1.0.and.ichan.eq.1) then
            do i=1,nsamp
               signal1(i)=signal1(i)*gain_chan1
            enddo
         endif
         do i=1,nsamp
           y(i)=signal1(i)
         enddo               
c
c   make plot top header
c
         do i=1,80
            head(i:i)=' '
         enddo
         head(1:12)='Plot start: '
         write(head(13:16),'(i4)') wav_year(wav_first)
         write(head(18:34),'(2i2,1x,2i2,1x,f4.1)') 
     *   wav_month(wav_first),wav_day(wav_first),
     *   wav_hour(wav_first),wav_min(wav_first),wav_sec(wav_first)
         head(36:39)='Lat='
         head(40:46)=data(1)(24:30)
         head(48:52)='Lon='
         head(53:60)=data(1)(31:38)
         head(62:63)='H='
         head(64:68)=data(1)(39:43)
         head(70:73)=data(1)(57:60)
c
c   call convert was below befor mar 4, 2012, so sdisance was not
c   calcualted if no plot, how could that work ??
c
         call convert(wav_abs_time(wav_first),station(ichan),
     *   component(ichan),-1)
c        write(6,*)'call convert,po',plotoption
         if(plotoption.ge.0.and.plotoption.lt.3)then 
c
c   plot one trace
c
            call one_traceplot                                                 
     *      (x0,y0,nsamp,station(ichan),component(ichan),nflag,flag)           
cxx            call convert(wav_abs_time(wav_first),station(ichan),
cxx     *      component(ichan),-1)                             
c
c   plot pics if requested
c
            if(plot_pics.eq.1) then
                call tecpic                                                    
            endif
            call xset_color(xblack)
         endif
c
c   select window and do spectral analysis
c
         nsignal=wlen*rate                                                 
c
c   check that not too much data for dimentions
c
         if(nsignal.ge.nspec_points) then
            if(plot)call clear_to_alpha 
            write(6,'(1x,a,f6.1,1x,a)') 
     *      'Too long window, can be max', 
     *       (nspec_points-1)/rate, 'secs'
             goto 99   !stop
         endif
         nstart=tstart*rate                                                    
c
c   check that not too much data has been selected or start is before first
c   sample
c
         if(nstart+nsignal.gt.nsamp.or.nstart.le.0) then
            if(plotoption.eq.0.or.plotoption.eq.1) call clear_to_alpha 
            if(nstart+nsignal.gt.nsamp) then
               write(6,*)' Window too late, skip trace'
               write(3,*)' Window too late, skip trace'
            endif
            if(nstart.le.0) then
               write(6,*)' Window starts before start of trace'
               write(3,*)' Window starts before start of trace'
            endif
	    if(plotoption.ge.0) then
                write(6,*) ' Return to continue'
                read(5,'(a)') i
            endif
             if(plotoption.eq.0.or.plotoption.eq.1)call open_display 
c           if( make_rel_spec.eq.1) then
               goto 10   ! next event                                                             
c           else
c               goto 50   ! next trace
c           endif
         endif
c
c  if response removal, check if response curve available and read, 
c  if not go to next event
c
         if(remove_response.gt.0) then
            wav_resp_file=' '
            call read_resp                 ! read response file
            if(wav_resp_status(1:1).eq.'8') then ! using header resp. values
                text='Response from header ***'
                if(plot)call tchars(text,24,710.0,230.0)

            endif
               if(wav_resp_status(1:1).eq.'9') then
               if(plotoption.eq.0.or.plotoption.eq.1) 
     *         call clear_to_alpha
               write(6,'(1x,a,1x,a,1x,a)') 
     *         'No response info ***',station(ichan),component(ichan)
               write(3,'(1x,a,1x,a,1x,a)') 
     *         'No response info ***',station(ichan),component(ichan)
              if(plotoption.ge.0) then
                write(6,*) ' Return to continue'
                read(5,'(a)') i
              endif
c               if( make_rel_spec.eq.1) then
                   goto 10   ! next event                                                             
c               else
c                   goto 50   ! next trace
c               endif
            endif
         endif
c
c  check if distance correction wanted
c
         factor=1.0
         if(dist_cor.gt.0.0) then
c
c    check that distance exists
c
c
            if(sdistance.eq.0) then
               if(plot) call clear_to_alpha 
               write(6,'(1x,a,1x,a,1x,a)') 'No distance in CAT file', 
     *         station(ichan),component(ichan)
               write(6,*)' Will skip trace'
               write(3,'(1x,a,1x,a,1x,a)') 'No distance in CAT file', 
     *         station(ichan),component(ichan)
               write(3,*)' Will skip trace'
               if(plotoption.ge.0) then
                  write(6,*) ' Return to continue'
                  read(5,'(a)') i
               endif
               goto 10   ! next event                                          
             endif
            write(3,*)' Hypocentral distance',sdistance
            factor=factor*sdistance**dist_cor
         endif
c
c   check if calculation of moment
c
         if(mag_spec.gt.0) then
c
c  moment is in Nm, 1.2 is surface and source correction
c
             factor= 4*3.14*(density*1000.0)*
     *               ((svelocity*1000.0)**3)*factor
             factor = factor*(1000.0)/(1.2*1.0e9)
         endif
c
         factor=alog10(factor)
c
c   travel time for q correction
c
         travel_time=tstart-t0
         if(ichan.eq.1) ttime1=travel_time
         if(ichan.eq.2) ttime2=travel_time
c
c   set first and last point to use
c
         first=nstart
         last=nstart+nsignal
c                                                                               
c    put part of the signal into local datavector                           
c                                                                               
         j =  0                                                                
         do i = first,last                                                     
           j = j + 1                                                           
           y(j) = signal1(i)                                                    
         enddo
c
c   check for clipped signals
c
c         call check_clipped(y,j,clipped)
         clipped=.false.
         if(clipped) then
            if(plotoption.eq.0.or.plotoption.eq.1) call clear_to_alpha 
            write(6,*) 'Signal clipped'
            write(3,*) 'Signal clipped'
            if(plotoption.ge.0) then
                write(6,*) ' Return to continue'
                read(5,'(a)') i
            endif
             if(plotoption.eq.0.or.plotoption.eq.1)call open_display
            goto 10
         endif
c
c   spectrum 
c
         call amp_spec_gen(j,nspec,x_work,power)
c
c   write out spectrum
c
c
         write(25,'(a5,1x,a4,2x,a12)') 
     *   station(ichan),component(ichan),title(1:12)
         do i=1,nspec
            write(25,*) x_work(i),y(i)
         enddo         
c
c   write out travel time used for q correction or q calculation 
c
         write(3,*)' Travel time=',travel_time
c
c   correct for distance etc
c
         do i = 1,nspec
             y(i) = y(i)+factor      ! factor is sum of all corrections
         enddo
c
c   smooth
c
         call smooth(y,nspec,nsmooth)                                           
c
c  spectrum title
c
         write(text,'(a5,1x,a4,2x,a12)') 
     *   station(ichan),component(ichan),title(1:12)
c
c
c   convert to magnitude
c
         if(mag_spec.eq.2) then
            do i=1,nspec
              y(i)=y(i)*0.667-6.06
            enddo
         endif
c
c   select spectral values at nearest fixed 10 output frequencies
c
         do i=1,10
           out_amp(i)=0.0
           out_ff(i)=0.0
           do k=1,nspec
              if(10.0**x_work(k).ge.out_f(i)) then
                 out_ff(i)=10.0**x_work(k)
                 out_amp(i)=y(k)
                 goto 3645
              endif
           enddo
 3645      continue
         enddo        
         write(3,'(a,1x,10f7.1)')' f   ',(out_ff(i),i=1,10)                    
         write(3,'(a,1x,10f7.1)')' amp ',(out_amp(i),i=1,10)
c
c------------------------------------------------------------------------
c   calcualte t* if parameter set
c------------------------------------------------------------------------
c

         if(t_star) then
             do i=1,nspec
                x_work(i)=10.0**x_work(i)    ! get linear frequencies
             enddo
             call lsqlin(nspec,x_work,y,aa,bb,corr,rms)
c
c   wrong constant used here before oct 23 2009, was 0.879, should
c   have been 0.733
c
             bb=bb*0.733     ! 0.733=1/(pi*log10(e))
             t_star_select=.false.
             if(abs(corr).gt.mincor) then   ! corr is negative or positive
c            if(abs(corr).gt.mincor.and.corr.lt.0.0) then   ! corr is negative
               itstar=itstar+1
               tstar(itstar)=bb
               t_star_select=.true.
               write(3,'(a,3f10.4)') 
     *         'Selected       kappa,corr, rms ',bb,corr,rms
             else      
               write(3,'(a,3f10.4)') 
     *         'Not selected   kappa,corr, rms ',bb,corr,rms
             endif
          endif
c
c-------------------------------------------------------------------
c   save spectrum if first spectrum, else make relative spectrum,
c   this is always done, even when not used
c
         nset=1                     ! at least one data set to plot
         npoint_set(1)=nspec
         if(ichan.eq.1) then        ! first channel
            do i=1,nspec
              rel_spec(ievent,i)=y(i)
            enddo
         else                       ! second channel, only if relative spectrum
            if(make_rel_spec.eq.1) then
               k=0
               do i=1,nspec
                 rel_spec(ievent,i)=rel_spec(ievent,i)-y(i)  ! 1. spectrum divided by 2
c                 write(3,*) 're, t2,t1',rel_spec(ievent,i),ttime2,ttime1
c
c----------------------------------------------------------------------------
c   calculate q if q0 is negative indicating q-calculation, put in rel_spec
c----------------------------------------------------------------------------
c
                 if(q0.lt.0.0) then
                    rel_spec(ievent,i)=
     *              10.0**x_work(i)*3.14*(ttime2-ttime1)/
     *              (2.30*(rel_spec(ievent,i))-   ! sign to - nov 06, jh
     *              dist_cor*2.30*log10(ttime2/ttime1))
c
c   mark crazy values with -1
c
                    if(rel_spec(ievent,i).lt.1.0) rel_spec(ievent,i)=-1.0
                    if(rel_spec(ievent,i).gt.5000.0) 
     *                     rel_spec(ievent,i)=-1.0
c
c   take log
c
                    if(rel_spec(ievent,i).gt.1.0)
     *                 rel_spec(ievent,i)=alog10(rel_spec(ievent,i))
c                    write(3,*) 10.0**x_work(i),rel_spec(ievent,i)

c
c   save for linear relation, but only 'good' values
c
                    if(rel_spec(ievent,i).gt.0.0) then
                       k=k+1
                       y_work(k)=rel_spec(ievent,i)
                    endif
                  endif
               enddo
c
c   calculate q0 and qalpha
c
               if(q0.lt.0.0.and.k.gt.2) then
                 call lsqlin(k,x_work,y_work,qq0,qqalpha,corr,rms)
                 qq0=10.0**qq0
                 write(3,*)'q0 and qalpha',qq0,qqalpha
               endif           
            endif
         endif
c
c   also save for plotting, thsi is only 1. single spectrum
c
         do i=1,nspec
           y_work(i)=y(i)
         enddo
c
c save spectra to compute average over traces for one event using 
c evenly spaced frequency
c
         n_event_spec=n_event_spec+1
         do j=1,nlogff
           aaa=event_ff(j)-dff/2.
           bbb=event_ff(j)+dff/2.
           l=0
           k=0
           yy=99999.
           event_spec(n_event_spec,j)=0.
           do i=1,nspec
             xx=abs(event_ff(j)-x_work(i))
             if (xx.lt.yy) then
               yy=xx
               k=i
             endif
             if (x_work(i).le.bbb.and.
     &           x_work(i).ge.aaa) then
               l=l+1
               event_spec(n_event_spec,j)=
     &                    event_spec(n_event_spec,j)+y(i)
             endif
           enddo
           if (l.gt.0) then
             event_spec(n_event_spec,j)=
     &            event_spec(n_event_spec,j)/float(l)
           else
c             write(*,*) ' nearest frequency ',10.**event_ff(k),y(k)
             event_spec(n_event_spec,j)=y(k)
           endif
c           write(3,*) event_ff(j),event_spec(n_event_spec,j)
         enddo
c
c---------------------------------------------------------------------------
c   now do noise spectrum, take noise from same window in start of signal
c---------------------------------------------------------------------------
c

         if(make_noise_spec.eq.1) then
            first=1
            last=nsignal

c                                                                               
c   put part of the signal into local datavector                           
c                                                                               
            j =  0                                                                    
            do i = first,last                                                         
               j = j + 1                                                               
               y(j) = signal1(i)                                                            
            enddo                                                                     
                                                                                

            call amp_spec_gen(j,nspec,x_work,power)                                                 
            do i=1,nspec
                y(i) = y(i)+factor
            enddo
c
c   smooth
c
            call smooth(y,nspec,nsmooth)                                           
c
c   convert to magnitude
c
            if(mag_spec.eq.2) then
               do i=1,nspec
                 y(i)=y(i)*0.667-6.06
               enddo
            endif
c
c   check s/n ratio if tstar, but only for selected events
c
            sn_ok=.true.        ! initially ok
            if(t_star.and.t_star_select) then
               do i=1,nspec
                  if((y_work(i)-y(i)).lt.minsn) sn_ok=.false.
               enddo
               if(.not.sn_ok) then
                 write(3,*) ' Removed, bad s/n'
                 itstar=itstar-1    ! take out last event
                 ievent=ievent-1
                 if(itstar.lt.0) itstar=0   ! cannot be less than 0 events
                 if(ievent.lt.0) ievent=0   ! these two needed now  ??
                                            ! since adding t_star_select
               endif
               ievent=itstar                ! make sure the two tally, event
                                            ! could have been taken out under
                                            ! corr check
            endif
c
c   put in plotting vector
c
            do i=1,nspec
               y_work(i+nspec)=y(i)
               if(xlin) x_work(i)=10.0**x_work(i)  ! antilog if linear x-axis      
               x_work(i+nspec)=x_work(i)  
            enddo
            nset=2                        ! 2 data sets to plot
            npoint_set(2)=npoint_set(1)   ! same number of points in both data sets
         endif  !      end noise part
            if(.not.plot)go to 50 

c
c   set plotting parameters
c
         y0=350.0   
         x_size=350.0
         x0=(ichan-1)*500.0+120.0
         y_size=170.0
         if(make_rel_spec.ne.1) then
            y0=110.0
            y_size=370.0      ! was 380
            x_size=500.0
         endif
c
c   plot signal and possibly noise spectrum, individual spectra
c
         aaa=-x0
         if(xlin) aaa=-aaa      ! linear scale
         if(plotoption.ge.0.and.plotoption.lt.3) then
            call xset_color(xblack)
            call xy_plot(nset,npoint_set,x_work,y_work,
     *      text,xtext,ytext,x_size,
     *      y_size,aaa,y0,2,0,30.0,0,c_spec,nc,cx,cy)
         endif
c                                 
c---------------------------------------------------------------------                                                                               
c End of channel loop  50, max 2 channels
c---------------------------------------------------------------------
c
 50   continue
c
c   set up output screen text for spectra
c
      if(q0.lt.0) then
         write(out_txt(1),'(a,f7.1)') 'Q0=',qq0
         write(out_txt(2),'(a,f5.2)') 'Qalp=',qqalpha
      else
         write(out_txt(1),'(a,f7.1)') 'Q0=',q0
         write(out_txt(2),'(a,f5.2)') 'Qalp=',qalpha
      endif
      write(out_txt(3),'(a,f3.1)') 'DistCo=',dist_cor
      write(out_txt(4),'(a,i4)')   'Hdist=',int(sdistance+0.5)
      write(out_txt(5),'(a,f5.2)') 'Velo=',svelocity
      write(out_txt(6),'(a,f5.2)') 'Dens=',density
      write(out_txt(7),'(a,i2)')   'SelCrit=',sel_crit
      write(out_txt(8),'(a,i4)')   'Start=',int(start)
      write(out_txt(9),'(a,i3)')   'Wind=',int(wlen)
      write(out_txt(10),'(a,f5.1)') 'Gain=',gain_chan1
      write(out_txt(11),'(a,i3)')   'Smooth=',nsmooth
      write(out_txt(12),'(a,f5.3)')  'kk_I=',kappa
      do i=13,30
         write(out_txt(i),'(a)')' '
      enddo
      if(t_star) then
         write(out_txt(13),'(a,f6.4)') 'kk0=',Bb
         write(out_txt(14),'(a,f6.3)') 'cor=',corr
         write(out_txt(15),'(a,f6.3)') 'rms=',rms
      endif
c
c   plot text for spectra
c
      if(plotoption.ge.0.and.plotoption.lt.3) then
c        if(make_rel_spec.eq.1) then
           call out_text(out_txt,15,12,2,-20.0,710.0,200.0) ! text at end
c        else
c           call out_text(out_txt,15,12,5,20.0,5.0,10.0)   ! text at bottom
c        endif
      endif
c
c----------------------------------------------------------------------
c   start section on plotting relative spectrum or q, first
c   check if any plotting at all, count events if relative spetra made
c----------------------------------------------------------------------
c
      if(make_rel_spec.lt.2) ievent=ievent+1 ! number is now for next event
      if(plotoption.eq.-1.or.plotoption.eq.3)   goto 10   ! no plotting, next event
c
c   plot relative spectrum if so specified
c
      if(make_rel_spec.eq.1) then   
c
c   set up plotting parameters
c
         x0=120.0
         y0=55.0
         k=0
         do i=1,nspec
            if(q0.lt.0.and.rel_spec(ievent-1,i).ge.0.0) then
                k=k+1
                y_work(k)=rel_spec(ievent-1,i)
             else
                y_work(i)=rel_spec(ievent-1,i)
             endif
         enddo
         if(q0.ge.0.0) then
            write(text,'(6(a,1x))')'Relative spectrum of ',
     *      station(1),component(1),' and ',station(2),component(2)
            ytext=' Log amplitude               '
         else
c
c   plot Q-values
c
            npoint_set(1)=k
            write(text,'(6(a,1x))')'Q calculated from ',
     *      station(1),component(1),' and ',station(2),component(2)
            ytext=' log Q                     '
         endif

         if(power) ytext='db'
         if(mag_spec.eq.1) ytext='Moment'
         if(mag_spec.eq.2) ytext='Mw'
c
         call xy_plot(1,npoint_set,x_work,y_work,
     *   text,xtext,ytext,550.0,185.0,-x0,y0+10.0,2,0,30.0,  !y was 195
     *   0,c_spec,nc,cx,cy)


         ytext=' Log amplitude               '
         if(mag_spec.eq.1) ytext='Moment'
         if(mag_spec.eq.2) ytext='Mw'
c
         if(power) ytext='db'
c
c   calculate average spectrum or average log Q, only 'good' q-values
c
         if(q0.lt.0.0) then
            call sdv(k,y_work,av,sd)
         else
            call sdv(nspec,y_work,av,sd)
         endif
c
         if(q0.ge.0.0) then
            write(3,*)' Average log spectrum and sd: ',av,sd
            do i=1,nspec
               y_work(i)=10**y_work(i)
            enddo
            call sdv(nspec,y_work,av,sd)
            write(3,*)' Average lin spectrum and sd: ',av,sd
         else
            write(3,*)' Average log Q and sd', av,sd
         endif
         write(text,'(a,f6.4,1x,a,f6.4)') 'Av: ',av,'Sd: ',sd
         call xchars(text,23,710.0,60.0)
      endif
c
 55   continue
c
c   close plot if plotting is done, pause before next plot if interactive mode
c
      if(plotoption.ge.0.and.plotoption.lt.3) then 
         if(plotoption.ge.1) then 
            call close_post
            close(65)
            call send_plot('spec.eps',8) ! plot to laser
         endif
         if(plotoption.eq.0.or.plotoption.eq.1) then
c        call xset_color(xyellow)
         call tchars(question,42,700.0,20.0)   ! question was 24, now 24  jh bef 2011
 112     continue
         call xscursr(key,xx,yy)
         if (char(key).eq.'#') goto 112
         if(char(key).eq.' ') then
            xx=(xx-x0)/xscale1+xfirst
            yy=(yy-y0)/yscale1+yfirst
            if(.not.power) yy=10.0**yy
            write(out_txt(1),'(a3,2f8.2)')'f,a',10.0**xx, yy
            call xmessage(out_txt,1,19,700.0,230.0)
            goto 112
         endif
     
         if(char(key).eq.'q'.or.char(key).eq.'Q') goto 99 !stop
        endif
      endif
c
c   back for next event
c
      goto 10

c-------------------------------------------------------------------
c------------------------------------------------------------------                                                                               
c   here after Finishing with one station pair, many events,
c   now plot all spectra and calculate average spectra
c------------------------------------------------------------------
c------------------------------------------------------------------
c------------------------------------------------------------------
c                                                                
 60   continue

 
                      
c
c   average spectra are made, if not go to next station
c
c      if(make_rel_spec.gt.1) goto 1   ! next pair of stations
      ievent=ievent-1
c
c  check if enough spectra are available, else go to next set of stations
c
cfeb 2002 jh
c     if(ievent.le.0) then
c         write(3,*)
c         write(3,*)' No events available for average'
c         first_run=.true.     ! do not ask for clear
c         close(65)            ! a plot file might have been opened above
c         goto 1          ! next pair of stations
c     endif
c
c   plot all relative or individual spectras, if no previous plots, 
c   open plot here
c
      if(plotoption.eq.-1.or.plotoption.eq.3)  then 
         old_poption=plotoption        ! save plot option
         open(65,file='spec.eps',status='unknown')
c
c   temporarely make sure hardcopy is made
c
         if(plotoption.eq.-1) plotoption=1
         if(plotoption.eq.3) plotoption=2  
      endif
c
c   check if data for final plot for kappa, else stop
c
      if(ievent.eq.0.and.t_star) then
         write(6,*) 'No spectra selected for final plot for kappa'
         stop
      endif

       if(plot) then
         call open_display
       endif

c
      x0=100.0
      y0=540.0
c
      wtext=station(1)//component(1)//'_'//station(2)//component(2)  ! for file name
      nwtext=19

      if(make_rel_spec.eq.1) then
c
c  non q
c
         if(q0.ge.0) then
            write(text,'(i3,6(a,1x))')ievent,' relative spectra for',
     *      station(1),component(1),'and',station(2),component(2)
            write(3,'(1x,a)') text
            gmt_ytitle='Spectral ratio, '//title(1:12)
         else

c
c  q
c
            write(text,'(i3,6(a,1x))')ievent,'  Q curves for',
     *      station(1),component(1),'and',station(2),component(2)
            write(3,'(1x,a)') text
            gmt_ytitle='Q'
            ytext='Log Q'
         endif
      endif

      if(make_rel_spec.eq.0) then
         nwtext=9         ! only one stat comp for name
         write(text,'(i3,6(a,1x))')ievent,' spectra for',
     *   station(1),component(1)
         if(t_star) 
     *   write(text,'(i3,6(a,1x))')ievent,' displacement spectra for',
     *   station(1),component(1) 
         write(3,'(1x,a)') text
         gmt_ytitle='Amplitude, '//title(1:12)
      endif
      x_length=700.0
c
c   fill out in name so no blanks
c
      do i=1,nwtext
         if(wtext(i:i).eq.' ') wtext(i:i)='_'
      enddo
c
c-----------------------------------------------------------------------
c   set main gmt parameters and open output file
c-----------------------------------------------------------------------
c
      gmt_ofilename='spec_all_'//wtext(1:nwtext)//'.gmt.ps'
      gmt_maintitle=text
      gmt_xtitle='Frequency (Hz)'
      gmt_xsize=12.0
      gmt_xlog=1
      gmt_ysize=10.0
      gmt_ylog=1
      gmt_ntraces=ievent
      if(.not.power.and.xlin) gmt_xlog=0  ! lin frequency
      if(power) then
          gmt_ylog=0                ! linear dD scale
          gmt_ytitle='Power spectral density (dB)'
          gmt_ntraces=gmt_ntraces+2
      endif
      if(mag_spec.eq.1) gmt_ytitle='Moment'
      if(mag_spec.eq.2) then
         gmt_ytitle='Mw'
         gmt_ylog=0
      endif
c
c  open gmt file and write out main parameters
c
c       write(6,'(a)') nwtext,wtext(1:nwtext)
c        open(26,file='spec_all_'//wtext(1:nwtext)//'.out',
c     * status='unknown')
     
      call gmt_xy_par(26,1,1,xx_work,yy_work)
c
c  output in file with all runs
c
      if(first_run) then
         gmt_ofilename='spec_all_gmt.ps'
         gmt_maintitle='All results'
         call gmt_xy_par(18,1,1,xx_work,yy_work)
      endif
c
c--------------------------------------------------------------------------
c   transfer to plotting array and gmt output array
c--------------------------------------------------------------------------
c
      kkk=0                    ! total number of points for q
      if(q0.ge.0.0) then       ! normal case
         do k=1,ievent
            npoint_set(k)=nspec
            do i=1,nspec
               y(i+nspec*(k-1))=rel_spec(k,i)
               x_work(i+nspec*(k-1))=x_work(i)
               j=i+nspec*(k-1)
c
               if(.not.power.and.xlin) then       ! tstar
                   xx_work(i)=x_work(i)          
                   yy_work(i)=10.0**rel_spec(k,i)
               endif 
               
               if(.not.power.and..not.xlin) then  ! log freqyency, normal spectra
                   xx_work(i)=10.0**x_work(i)     ! 
                   yy_work(i)=10.0**rel_spec(k,i)
               endif

               if(power.or.mag_spec.eq.2) then  ! noise power spectra or Mw
                   xx_work(i)=10.0**x_work(i)
                   yy_work(i)=rel_spec(k,i)
               endif
            enddo

            write(gmt_tracetitle,'(i4)') k 
            gmt_tracesymbol=' '
            if(k.lt.gmt_ncolor) then
                gmt_tracecolor=gmt_color(k)
            else
                gmt_tracecolor='black'
            endif
c
c   write out trace parameters and one trace
c
            call gmt_xy_par(26,0,nspec,xx_work,yy_work)
            gmt_tracetitle=station(1)//' '//component(1)
            call gmt_xy_par(18,0,nspec,xx_work,yy_work)
            ngmt_all=ngmt_all+1
c 
         enddo
c
c   q, check  for 'good values'
c
      else
         do k=1,ievent
            kk=0
            do i=1,nspec
               if(rel_spec(k,i).ge.0.0) then
                  kk=kk+1   ! good points in this set
                  kkk=kkk+1 ! good points in all sets
                  y(kkk)=rel_spec(k,i)
                  x_work(kkk)=x_work(i)
                  xx_work(kkk)=10.0**x_work(kkk)
                  yy_work(kkk)=10.0**y(kkk)
               endif
            enddo
            npoint_set(k)=kk
            write(gmt_tracetitle,'(i3)') k
            gmt_tracesymbol=' '
            if(k.lt.gmt_ncolor) then
                gmt_tracecolor=gmt_color(k)
            else
                gmt_tracecolor='black'
            endif
c
c   write out trace parameters and one trace
c
            call gmt_xy_par(26,0,nspec,xx_work,yy_work)
            gmt_tracetitle=station(1)//' '//component(1)
            call gmt_xy_par(18,0,nspec,xx_work,yy_work)
            ngmt_all=ngmt_all+1
c 
         enddo 
c 
      endif
c
c-------------------------------------------------------   
c   add petersen noise spectra if noise spectra
c--------------------------------------------------------
c
      if(power) then
        k=0
        do i=1,22
          ff = 10.0**low_f(i)
          if(ff .ge. ffmin .and. ff .le. ffmax) then
             j=j+1
             k=k+1
             x_work(j)=low_f(i)
             xx_work(k)=10.0**low_f(i)
             y(j)=low_l(i)
             yy_work(k)=low_l(i)
           endif
        enddo
c
        gmt_tracetitle='NHNM'
        gmt_tracesymbol=' '
        gmt_tracecolor='red'
c
c
c   gmt output for peterson
c
        call gmt_xy_par(26,0,k,xx_work,yy_work)
        if(first_run) then
            call gmt_xy_par(18,0,k,xx_work,yy_work)
            ngmt_all=ngmt_all+1
        endif
        npoint_set(ievent+1)=k
        k=0
        do i=1,14
          ff = 10.0**high_f(i)
          if(ff .ge. ffmin .and. ff .le. ffmax) then
             j=j+1
             k=k+1
             x_work(j)=high_f(i)
             xx_work(k)=10.0**high_f(i)
             y(j)=high_l(i)
             yy_work(k)= high_l(i)
          endif
        enddo
        gmt_tracetitle='NLNM'
        gmt_tracesymbol=' '
        gmt_tracecolor='red'
c
c   gmt output
c
        call gmt_xy_par(26,0,k,xx_work,yy_work)
        if(first_run) then
           call gmt_xy_par(18,0,k,xx_work,yy_work)
           ngmt_all=ngmt_all+1
        endif

        npoint_set(ievent+2)=k
       endif

       close(26)
        
c
c---------------------------------------------------------------------------
c   plot individual spectra
c---------------------------------------------------------------------------
c
       
      k=ievent
      if(power) k=k+2
      aa=-x0            !  log axis
      if(xlin)aa=x0     ! linear axis

      if(plot) then
          call xy_plot(k,npoint_set,x_work,y,
     *    text,xtext,ytext,x_length,200.0,aa,y0,2,0,30.0,  ! ysize was 200
     *    0,c_spec,nc,cx,cy)
      endif
c
c--------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c   calculate average spectra with standard deviation if at least 2 spectra,
c   the average is done in linear spectra, for q in 1/q linear,
c   then back to log q
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c
c   set titles
c


      if(ievent.gt.1) then   ! the endif is far below
         if(make_rel_spec.eq.1) then
            if(q0.ge.0) then
               write(text,'(6a)')
     *         ' Average spectral ratio with sd for ',
     *         station(1),component(1),' and ',station(2),component(2)
               gmt_ytitle='Relative amplitude, '//title(1:12)
            else
               write(text,'(6a)')
     *         ' Average Q with sd for ',
     *         station(1),component(1),' and ',station(2),component(2)
               gmt_ytitle='Q'
            endif
         endif
         if(make_rel_spec.eq.0) then
            if(t_star) then
               write(text,'(3a)')
     *         ' Average spectra with sd for ', 
     *         station(1),component(1)
                gmt_ytitle='Amplitude, '//title(1:12)
            else  
              write(text,'(3a)')
     *         ' Average spectra with sd for ', 
     *          station(1),component(1)
                gmt_ytitle='Amplitude, '//title(1:12)
            endif
         endif
c
c   open output gmt file
c
         open(27,file='spec_ave_'//wtext(1:nwtext)//'.out',
     *   status='unknown')
c
c   set main gmt parameters that have changed realtive to above
c
         gmt_ofilename='spec_ave_'//wtext(1:nwtext)//'.gmt.out'
         gmt_maintitle=text
         gmt_ntraces=3
         if(power) then
             gmt_ylog=0                ! linear dD scale
             gmt_ytitle='Power spectral density (dB)'
             gmt_ntraces=gmt_ntraces+2
         endif
         if(mag_spec.eq.1) gmt_ytitle='Moment'
         if(mag_spec.eq.2) then
             gmt_ytitle='Mw'
             gmt_ylog=0
         endif
c


c
c  write out main gmt parameters
c     
         call gmt_xy_par(27,1,1,xx_work,yy_work)
cx
c


c
c-------------------------------------------------------------------
c  calculate average and transfer to plotting arrays
c-------------------------------------------------------------------
c


c
c   calcucalate and store average value
c
         kkk=0               ! total number to plot in all data sets, Q only
         do k=1,nspec        ! loop over number of frequencies
            kk=0             ! number of values to average, Q only
            do l=1,ievent    ! loop over number of spectra/events
               if(q0.ge.0.and.(.not.power).and.(.not.t_star)) then
                  y_work(l)=10**rel_spec(l,k)
               else
                  if(q0.lt.0.0) then
                     if(rel_spec(l,k).gt.0.0) then   ! only 'good' Q-values at this f
                         kk=kk+1                   
                         y_work(kk)=1.0/10.0**rel_spec(l,k) ! linear 1/q
                     endif
                  else
                     y_work(l)=rel_spec(l,k)         ! linear values, power and t-star
                  endif
               endif
            enddo
c
            if(q0.ge.0.0) then      ! by definition, always at least 2 spectra
               call sdv(ievent,y_work,rel_spec(1,k),sd)
            else
               if(kk.ge.2) then     ! only q average if 2 'good' values
                  kkk=kkk+1
                  call sdv(kk,y_work,rel_spec(1,kkk),sd) 
                  rel_spec(2,kkk)=rel_spec(1,kkk)+sd
                  rel_spec(3,kkk)=rel_spec(1,kkk)-sd
c
c  this must be larger than 0 to take log
c
                  if(rel_spec(3,kkk).le.0.0) rel_spec(3,kkk)=1.0
                  rel_spec(1,kkk)=alog10(1.0/rel_spec(1,kkk))
                  rel_spec(2,kkk)=alog10(1.0/rel_spec(2,kkk))
                  rel_spec(3,kkk)=alog10(1.0/rel_spec(3,kkk))

                  x_work(kkk)=x_work(k)
c
c  save for gmt, linear
c
                  xx_work(kkk)=10.0**x_work(kkk)     
                  yy_work(kkk)=10.0**rel_spec(1,kkk)
               endif
            endif

c
c   calculate sd values, q done above
c

c
c   take log, if not negative or zero, them make log sd zero, NOT Q, done above
c
            if(q0.ge.0.and.(.not.power).and.(.not.t_star)) then
                if((rel_spec(1,k)+sd).gt.0.0.
     *          and.rel_spec(1,k).gt.0.0) then
                   sd=alog10(rel_spec(1,k)+sd)-alog10(rel_spec(1,k))
                else
                   sd=0.0
                endif
                if(rel_spec(1,k).gt.0.0) then
                   rel_spec(1,k)=alog10(rel_spec(1,k))
                else
                   rel_spec(1,k)=0
                endif
            endif
c
c  store sd values, for tstar the original sd can be used since linear, 
c  q done above
c
            if(q0.ge.0.0) then
                rel_spec(2,k)=rel_spec(1,k)+sd
                rel_spec(3,k)=rel_spec(1,k)-sd
            endif
c
c   select for gmt output, not q, done above
c

            if(.not.power.and.xlin) then   ! lin frequency, tstar
                xx_work(k)=x_work(k)
                yy_work(k)=10.0**rel_spec(1,k)
            endif 
               
            if(.not.power.and..not.xlin.and.q0.ge.0.0) then  ! log freqyency, 
c                                                              normal spectra
                xx_work(k)=10.0**x_work(k)     
                yy_work(k)=10.0**rel_spec(1,k)
            endif

            if(power.or.mag_spec.eq.2) then                ! noise power spectra
                xx_work(k)=10.0**x_work(k)
                yy_work(k)=rel_spec(1,k)
            endif            
        enddo

         gmt_tracetitle='average '
         gmt_tracesymbol=' '
         gmt_tracecolor='black'
c
c   write out trace parameters and average trace
c
         if(q0.ge.0) then
            call gmt_xy_par(27,0,nspec,xx_work,yy_work)
         else
            call gmt_xy_par(27,0,kkk,xx_work,yy_work)
         endif
c
c   select for gmt output, average + sd
c
         do k=1,nspec

            if(.not.power.and.xlin) then   ! lin frequency, tstar
                xx_work(k)=x_work(k)
                yy_work(k)=10.0**rel_spec(2,k)
            endif 
               
            if(.not.power.and..not.xlin) then  ! log freqyency, normal spectra
                xx_work(k)=10.0**x_work(k)     ! also log(q)
                yy_work(k)=10.0**rel_spec(2,k)
            endif

            if(power.or.mag_spec.eq.2) then         ! noise power spectra
                xx_work(k)=10.0**x_work(k)
                yy_work(k)=rel_spec(2,k)
            endif            
         enddo
         gmt_tracetitle='av + sd '
         gmt_tracesymbol=' '
         gmt_tracecolor='blue'
c
c   write out trace parameters and average trace + sd
c
         if(q0.ge.0.0) then
            call gmt_xy_par(27,0,nspec,xx_work,yy_work)
         else
            call gmt_xy_par(27,0,kkk,xx_work,yy_work)
         endif
c 
c
c   select for gmt output, average - sd
c
         do k=1,nspec

            if(.not.power.and.xlin) then   ! lin frequency, tstar
                xx_work(k)=x_work(k)
                yy_work(k)=10.0**rel_spec(3,k)
            endif 
               
            if(.not.power.and..not.xlin) then  ! log freqyency, normal spectra
                xx_work(k)=10.0**x_work(k)     ! also log(q)
                yy_work(k)=10.0**rel_spec(3,k)
            endif

            if(power.or.mag_spec.eq.2) then        ! noise power spectra
                xx_work(k)=10.0**x_work(k)
                yy_work(k)=rel_spec(3,k)
            endif            
         enddo
         gmt_tracetitle='av - sd'
         gmt_tracesymbol=' '
         gmt_tracecolor='blue'
c
c   write out trace parameters and average trace - sd
c
         if(q0.ge.0.0) then
            call gmt_xy_par(27,0,nspec,xx_work,yy_work)
         else
            call gmt_xy_par(27,0,kkk,xx_work,yy_work)
         endif  

c
c   transfer to plotting array
c
         if(q0.ge.0.0) then
            do k=1,3
               npoint_set(k)=nspec
               do i=1,nspec
                  y(i+nspec*(k-1))=rel_spec(k,i)
                  x_work(i+nspec*(k-1))=x_work(i)
                  j=i+nspec*(k-1)
               enddo
            enddo
         else        ! for Q
            do k=1,3
               npoint_set(k)=kkk
               do i=1,kkk
                  y(i+kkk*(k-1))=rel_spec(k,i)
                  x_work(i+kkk*(k-1))=x_work(i)
                  j=i+kkk*(k-1)
               enddo
            enddo
         endif
c
c   if t* calculation, do it here on the average spectrum
c
         if(t_star) then
            call lsqlin(nspec,x_work,y,aa,bb,corr,rms)
            aa=bb*0.879     ! 0.879=1/(pi*log10(e))
            write(3,*)
            write(3,'(1x,a,3f10.3)')
     *     'kappa from average spectrum, corr,rms', aa,corr,rms
         endif

c
c   add petersen noise spectra if noise spectra
c
         if(power) then
            k=0
            do i=1,22
               ff = 10.0**low_f(i)
               if(ff .ge. ffmin .and. ff .le. ffmax) then
                  j=j+1
                   k=k+1
                   x_work(j)=low_f(i)
                   xx_work(k)=10.0**low_f(i)
                   y(j)=low_l(i)
                   yy_work(k)=low_l(i)
               endif
            enddo
            gmt_tracetitle='NHNM'
            gmt_tracesymbol=' '
            gmt_tracecolor='red'
c
c
c   gmt output
c
            call gmt_xy_par(27,0,k,xx_work,yy_work)

            npoint_set(4)=k
            k=0
            do i=1,14
               ff = 10.0**high_f(i)
               if(ff .ge. ffmin .and. ff .le. ffmax) then
                  j=j+1
                  k=k+1
                  x_work(j)=high_f(i)
                  xx_work(k)=10.0**high_f(i)
                  y(j)=high_l(i)
                  yy_work(k)= high_l(i)
               endif
            enddo
            gmt_tracetitle='NLNM '
            gmt_tracesymbol=' '
            gmt_tracecolor='red'
c
c   gmt output
c
            call gmt_xy_par(27,0,k,xx_work,yy_work)

            npoint_set(5)=k
         endif
         close(27)
c        
c
c   plot average relative spectra/spectra/Q with standard deviation
c
c
         y0=200
         x_length=700
         k=3
         if(power) k=5
         aa=-x0            !  log axis
         if(xlin)aa=x0     ! linear axis


       
         if(plot) then
            call xy_plot(k,npoint_set,x_work,y,
     *      text,xtext,ytext,x_length,200.0,aa,y0,2,0,30.0,
     *      0,c_spec,nc,cx,cy)
         endif
c
c   maybe calculate q
c
         if(q0.lt.0.0) then
c
c   calculate q0 and qalpha
c
            call lsqlin(kkk,x_work,y,qq0,qqalpha,corr,rms)
            qq0=10.0**qq0
            write(text,'(a,f7.1,1x,a,f6.2)') 'q0 ',qq0,'qalpha ',
     *      qqalpha
            if(plot) call xchars(text,23,100.0,20.0)
            write(3,*)
            write(3,*)'q0 and qalpha from average',qq0,qqalpha
            write(3,*)
         endif
c
c   calculate average spectrum/Q/tstar/spectrum
c
         do i=1,nspec
            y_work(i)=rel_spec(1,i)
         enddo

         if(q0.ge.0.0) then
            call sdv(nspec,y_work,av,sd)
            write(3,*)' Average log spectrum and sd: ',av,sd
         else
            call sdv(kkk,y_work,av,sd)
            write(3,*)' Average log Q and sd: ',av,sd
         endif
         write(text,'(a,f13.3)') 'Sd: ',sd
         if(plot) call xchars(text,29,810.0,200.0)
         write(text,'(a,f13.3)') 'Av: ',av
         if(plot) call xchars(text,29,810.0,230.0)

         if(.not.power) then
            do i=1,nspec
               y_work(i)=10**y_work(i)
            enddo
         endif

         if(q0.ge.0.0) then
            call sdv(nspec,y_work,av,sd)
            if(.not.power)
     *       write(3,*)' Average lin spectrum and sd: ',av,sd
             if(power)
     *       write(3,*)' Average power density spectrum and sd: ',av,sd
         else
            call sdv(kkk,y_work,av,sd)
            write(3,*)' Average lin Q and sd: ',av,sd
         endif
         write(text,'(a,a)')'Event file: ',name(1:40)
         if(plot) call xchars(text,51,80.0,110.0)

c        call out_text(out_txt,11,12,5,-20.0,5.0,90.0)   ! text at bottom
c
c   calculate average t* if that option
c
         if(t_star) then
            write(6,*)
            if(itstar.gt.1) then
               call sdv(itstar,tstar,av,sd)
            else
               av=tstar(1)
               sd=0.0
            endif
            write(3,*)' Number of values for kappa',itstar
            if(itstar.gt.0) then
               write(3,'(a,2f10.3)') 'Average kappa and sd',av,sd
               write(text,'(a,f8.3,a,f8.3,a,i5)') 
     *         'kappa=',av,' sd=',sd,'  n=',itstar
               if(plot) call xchars(text,55,80.0,40.0)
            endif
         endif
c
c average spectra of number of stations from on event
c
      elseif (ievent.eq.1.and.n_event_spec.gt.0) then
         open(27,file='spec_ave.out',
     *   status='unknown')
c check sampling rate
         l=1
c         do i=2,n_event_spec
c           if (event_rate(i).ne.event_rate(i-1)) l=0
c         enddo
         if (l.eq.1) then
           do k=1,nlogff     ! loop over number of frequencies
              do i=1,n_event_spec
                 y_work(i)=event_spec(i,k)
              enddo
              call sdv(n_event_spec,y_work,av,sd)
              if (av.gt.0.) write(27,'(f12.3,1x,f12.3,1x,f12.3)') 
     &     10.**event_ff(k),av,sd
           enddo
         endif
         close(27)
      else
c
c  not enough spectra are available for average
c
          write(3,*)
          write(3,*)' No events available for average'
c         first_run=.true.     ! do not ask for clear
      endif
c-----------------------------------------------------------------------
c   end of whole average block
c------------------------------------------------------------------------

c
c   close plot, here there must always be a plot unless no plotting
c
c     write(6,*) 'final close,po', plotoption                              
      if(plotoption.ge.0) then 
         if(plotoption.ge.1) then 
            call close_post
            close(65)
            if(old_poption.ne.-1)
     *      call send_plot('spec.eps',8) ! plot to laser
         endif
      endif
c
c   reset plot option
c
      if(old_poption.eq.-1.or.old_poption.eq.3) plotoption=old_poption 

c
c---------------------------------------------------------------------
c   back for next station pair, goto 1
c---------------------------------------------------------------------
c
      first_run=.false.

      goto 1
c
c   end of program
c

 99   continue                                                                
c
c   fix number of traces in spec_all_gmt.out
c
      rewind 18
      open(19,file='spec_all_gmt.out',status='unknown')
 2929 continue
         read(18,'(a)',end=2930) text
         if(text(3:9).eq.'NTRACES') then
             text(20:80)=' '
             write(text(25:34),'(i10)') ngmt_all 
         endif
         write(19,'(a)') text
         goto 2929
 2930 continue
      close(18,status='delete')
       
      if(plot) call clear_to_alpha 
      write(6,*)
     *' Output files are:',
     *' spec.out, spec_amp.out, spec_all... and spec_ave...'

      stop
      end
c
                                                                                
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc             
      subroutine amp_spec_gen(nsamp,nspec,x_work,power)                                                 
c                                   
c
c     routine to make a real spectrum, more general than routine in
c     mul_spec                
c     power: true if power density spectrum
c     The tapering of  the data is fixed to 10%                                 
c                                                                               
c     Jh april 94                                         
c                                                                               
                                                                                
c-- common block     
c
      implicit none
      include 'mulplt.inc'			
      logical power
c
c-- percentage of tapering (fixed)                       
      real taper/10./				
c-- length of input vector                                 
      integer nsamp				
c-- length after padding                                    
      integer ipow				
c-- vector padded with npad zeros                           
      integer npad				
c-- indicator for response removal or not (1 or 0).                        
      integer rem_resp			
c-- indicator for type of spectrum                  
c     integer disp_vel! now in mulplt.inc			
c-- number of points in spectrum
      integer nspec
c-- frequencies
      real x_work(*)
c-- counters and help variables                                               
      integer i,j				
      real ff                    ! help variables
c
c  paz, not used
c

      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros
		
c
c   no frequency domain filters
c
      filt=0
      npole=0
      nzero=0
c
c   check if response is removed 
c
      if(remove_response.gt.0) then
         rem_resp=1                   ! response is removed
         disp_vel=remove_response     ! type of response
c        opmode=0                     ! get origin time form data array
      else
         rem_resp=0                   ! response is not removed
         disp_vel=0
c        opmode=1
      endif
c                                                                               
c--------put part of the signal into local datavector                           
c                                                                               
c      j =  0                                                                    
c      do i = first,last                                                         
c        j = j + 1                                                               
c        y(j) = signal1(i)                                                            
c      enddo                                                                     
c                                                                                
c      nsamp = j                                                                 
c                                                                               
c------- prepare data.     Pad with zeros and taper 10%.                        
c                                                                               
      call prepare(nsamp,y,taper,ipow,npad,com)                                
c
c   check for max number of points
c
      if(ipow.ge.max_sample/2) then
c          call clear_to_alpha
           write(6,*)' Too many points in spectrum'
           write(6,*)' Max number is ', max_sample/2
           stop
      endif                                
c
c-------- Calculate the spectrum, and remove the system response
c         if desired, no frequency domain filters, therfore the zeros                
c

      call spectrum(com,ipow,disp_vel,rem_resp,rate,0.0,0.0,0,0,
     +     q0,qalpha,kappa,travel_time,zero,pole,nzero,npole,norm)
c                                                                               
c
c   calculate real spectrum
c
      j=0
      do i = 2,((ipow/2) + 1)
        ff=(i-1)*rate/ipow
        if(ff.ge.ffmin.and.ff.le.ffmax) then
           j=j+1
           if(power) then
             y(j) = 2.0*(com(i)*conjg(com(i)))/(nsamp*rate)    ! power spectral density
           else
             y(j) = ((1/rate)**2)*(com(i)*conjg(com(i)))    ! transient signal
           endif

           if(.not.power)y(j) = sqrt(y(j))                ! j.h. change
           y(j) = log10( y(j))                       ! take the logarithm
           if(power) y(j)=(y(j)-18.0)*10.0  ! db (m/s**2)**2
           x_work(j)=log10(ff)
        endif
      enddo
      nspec=j                                                                 
      return                                                                    
      end                                                                       



      subroutine one_traceplot                                                  
     *(x0,y0,nsamp,stat,comp,nflag,flag)                                  
c                                                                               
c                                                                               
c   plots a trace of data in a fixed frame                                      
c                                                                               
c   input:   x1,x2,y1,y2: frame and size of plot                                
c            n:     number of points  in input vector y                         
c            y:     n samples                                                   
c            xo:    time of first sample (sec)
c            nsamp: nun\mbe rfosmaple to plot
c            stat:  station
c            comp:  component, if rotate it will be changed                      
c            nflag: number of flags to be plotted along trace                   
c            flag:  time of flags to plotted along trace, in same               
c                   time scale as xo.                                           
      implicit none
      include 'seiplot.inc'
      include 'mulplt.inc'
      integer nsamp,nflag,imax                                                
      real x0,y0,flag(*)                                              
      character*80 text
      character*5 stat
      character*4 comp
      real x                    ! help variable
      integer i
c                                                                               
c  define some plot options                                                    
c                                                                               
      pframe=1                                                                  
      paxis=1                                                                   
      pmax=3             ! plot max count at end of trace                                                                    
      ppick=0                                                                   
      over_scale=1.0                                                            
      trace_delay=0.0                                                           
      first=1                                                                   
      last=nsamp   
c
c   set first second and minute of plot axis, use main header values
c
      fsec=wav_sec(wav_first)
      fmin=wav_min(wav_first)
c
c   get page time and trace delay
c
      page_time=wav_total_time
c
c   the trace delay is normally given by wav_delay, however if channels
c   rotated, there might be a further delay due to horizontal 
c   channels not starting at the same time
c
       if(rotate) then
          trace_delay=wav_out_start(1)
       else
          trace_delay=wav_delay(wav_current_chan(1))
       endif
c                                                             
      xpos=x0                                                                  
      ypos=y0
      numb_samp=nsamp
c                                                                               
c   plot header on top trace                                                    
c                                                                               
      if(ptitle.eq.1) then                                                      
         call xchars(head,80,xpos,ypos+height+20)                               
      endif                
c
c   plot station and component
c
      if(rotate.and.comp(4:4).eq.'N') comp(4:4)='R'
      if(rotate.and.comp(4:4).eq.'E') comp(4:4)='T'


      call xchars(stat,5,5.0,ypos+height/2+15.0)
      call xchars(comp,4,5.0,ypos+height/2-15.0)  
c
c   plot trace
c
      dc_chan=0.0        ! to ensure that dc is calculated for each new plot
      call plotw                                                                
c
c   plot max value
c
      call fontsize(0,1.5)
      imax=max+0.5
      write(text,'(i10)') imax
c-- tektronics
      call tchars(text,10,880.0,ypos+height-22)
c-- postscript
      call pchars(text,8,930.0,ypos+height-5)
      call fontsize(0,3.0)
c
c   plot baz if rotation
c
      if(rotate) then
         write(text,'(a,i4)') 'baz',int(baz(current_chan))
         call tchars(text,7,140.0,ypos+height-22)
         call pchars(text,7,140.0,ypos+height-5)
      endif
c
c                                                                               
c   plot flags                                                                  
c                                                                               
      if (nflag.ne.0) then                                                      
         do  i=1,nflag                                                         
            x=(flag(i)+trace_delay)*xscale+xpos
            call xmovabs(x,ypos)                                            
            call xdrwabs(x,ypos+height)                                        
            call xdrwabs(x-9.0,ypos-9.0+height)                                 
            call xmovabs(x,ypos+height)                                         
            call xdrwabs(x+9.0,ypos-9.0+height)                                
         enddo                                                               
       endif                                                                  
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
      subroutine out_text
     *(text,ntext,column_width,ncolumn,line_dist,x0,y0)
c
c  plotting out lines of text. each element in text contains one
c  column of text of width column_width. ncolumn groups are then plotted
c  out pr line starting at position
c  lower left hand cornor x0,y0 (tek units). distance between lines is 
c  line_dist, can be negative to move other way
c
      implicit none
      character*80 text(*)
      character*80 one_line
      integer ntext,ncolumn,k,l,column_width
      real x0,y0,x,y
      real line_dist
c
      x=x0
      y=y0
      k=1
      dowhile(k.le.ntext)
         do l=1,ncolumn
            one_line((l-1)*column_width+1:l*column_width)=
     *      text(k)(1:column_width)
            k=k+1
         enddo
         call xchars(one_line(1:ncolumn*column_width),
     *   ncolumn*column_width,x,y)
         y=y+line_dist
      enddo
c
      return
      end
c

      subroutine make_new_sdata(infile,data,nhead,nrecord)
c
c   makes an internal s-file with just the waveform file name

c       input:  infile : waveform file name
c       output: data   : standard data array
c               nhead, nrecord: standard  variables
c
       implicit none
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'
       include 'waveform.inc'
       character*80 data(*)
       integer nrecord, nhead
       character*80 infile

      wav_filename(1)=infile
c
c   read main header for one file
c	  
      call wav_init
      call read_wav_header(1)
      if(wav_error_message.ne.' ') then
         write(6,'(1x,a)') wav_error_message
         stop
      endif
c
c   output header line 
c
      data(1)=' '
      write(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1,1x,a1)')     
     *wav_year(wav_first),wav_month(wav_first),
     *wav_day(Wav_first),wav_hour(wav_first),
     *wav_min(wav_first),wav_sec(wav_first),'L'
      data(1)(80:80)='1'
c
c  waveform file name
c
      data(2)=' '
      write(data(2)(2:79),'(a)') infile(1:78)
      data(2)(80:80)='6'
      data(3)=' '
      nhead=2
      nrecord=3
c
      return
      end




ccccccccccccccccccccccccccccccccccccc
