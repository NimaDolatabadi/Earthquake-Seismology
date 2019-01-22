c 
c      subroutines for plotting of seismic signals, originally written
c      in tektronics, so all scaling on screen is in tektronics units,
c      1023 X 780                     
c                                                                               
c    jens havskov 1989                                                       
c
c    updates
c    oct 02 91 by j.h.: for unix and new cursor routine cursor.c,
c                       it a tsend has been put in several places
c    oct 17            : remove variables s1 and s2 in traceplot
c    nov 6             :change traceplot and chasel to make it possible
c                       to register when plotoption is 1
c    dec 7             :fix header titles, wight for sun
c    feb 1 92 by j.h.  :max amp plotted to 12 chars, patch up plotting when skipping
c    jun 3 92          : fix bug with header plotting in multi event modennn
c    jul 23            : bug first minute tic mark
c    oct 15            : move axis number up a bit in postscript
c    oct 27  by jh 92  : fix bug in chasel
c    feb 17 93         : more fixed filters, bug plotting pics in traceplot
c    mar 9  93         : chasel: whole window selection, plot lines when sel.
c    apr 13            : remove dc before filtering
c    apr-may           : many new options
c    may 18            : bug
c    july 23 93        : fix bug with multichannel filtering
c    aug 22            : filename to 80 char, some clean up
c    aug 31            : seisin to seisinc
c    nov 93            : allow any phase name
c    dec 93            : version 4.0
c    mar 15 94         : plotting of synt phases below
c    jun 94            : mb response, 999 channls
c                      : indicate dc level, bugs
c    dec 94            : version 5.0
c    feb 95            : add fixed scaling
CJAB(BGS)Mar95         : Incorrect logic in IF statement (traceplot).
CJAB(BGS)Mar95         : plotw....hard-wired xyellow, xblue changed to
CJAB(BGS)Mar95           seisan object colours
c    apr 21 95 jh      : component rotation
c    apr 16            : hc all channels
c    may 5             : rewind plot file in ZOZO mode, 
c    may 9             : bugs with max in plotw
c    jun 7  95         : chasel routine: include G and g option
c    jul 24            : also plots dc level (number) on trace plot
c    sep 21 95         : adjust position of rotate and max count
c    oct               : fix help text print out, distance sort
c    nov9              : polarity probelsm in multi trace, many chanels
c    dec 95            : continous plotting, new filter
c    jan 14 96         : small correction to amplitude picking
c    jan 23            : enable noise spectrum selection
c    jan 31            : put nhead, nrecord in mulplt.inc
c    feb 6             : check for cursor movement changed, bug cont. plot
c    mar 10            : partly back to old filter
c    mar 12            : put routine convert in this file instead of mulplt
c    jun 12            : some patch up of skipping
c    aug 1             : put call to tecpic in 3 comp into picc, save first
c    aug 2             : calculaste hypocentral distance in convert
c    aug 28            : tics at wrong posion  cont plot
c    nov 6             : epi location, color synt, delete A for aumatic
c    nov 24            : in spectral analysis, traveltime was wrongly calculated
c                        when window selected in zoomed mode on top trace
c    dec 27, 96        : also locate with 'L'
c    feb 3   97        : wrong hour on axis in zoom
c    feb 10            : hardcopy to ;, ms filter to K
c    feb 21            : menu, fix position of pics to nearest sample (Baptie)
c    mar  10           : bug with menu
c    mar 19            : weight in multitrace mode
c    sep 30            : add option merge for waveform files
c    oct 29            : check char input filter, zoom limits, check that
c                        2 amplitudes entered
c    dec 3             : Out option to write a new file
c    feb 11 98         : enable continoues plot from eev
c    feb  18           : p and s velocities in spectra, fix bug and improve
c    feb 25            : fix eev connection
c    mar 9             : add OTWA choise
c    mar 18            : fix problem with wrong amplitude picks in
c                        multi trace mode
c    mar 24            : problems with rotate 
c    april 7           : 3 row menu, header from eev etc
c    jun 98   lo       : automatic coda
c    sep 15            : indicate timing errors
c    sep 24            : a simple linear taper when filtering
c ---------------------------------------------------------------------------
c    sep 30 98 jh      : ------------  version 7.0 check ---------------------
c                        year 2000, 5 char station codes
c    oct 7 by jh       : bug
c    nov 4    jh       : add linux logic
c    nov 5             : change c-routines with '_' in name
c    nov 10  bmt       : add save command
c    dec 21  jh        : only taper when filter filter more then 500 points
c    feb 28 99 jh      : shift position of gain in output
c    mar 1  99 jh      : plot bug with continous
c    mar 2  99 jh      : routine convert had variable time at 16 instead of 18
c    may 28            : add choise for FK
c    jun10             : add phase AMP, diable amplitude reading on A
c    jun13             : routine convert, no century if run without data base
c    jun 16            : fix font size for uncertain time
c    sep 9             : fix channel time error for output file
c    sep 17            :  ---------- write out for more than 30 channels
c    sep 19            : put in edepth and edistance in convert
c    sep 21            : replace on_old with common block variable show_menu
c    sep 23            : put in geo_distance instead of sdistnace in spec out
c    sep 27            : output kappa instead of travel time in spectral output
c    oct 01 lo         : fixed some text in the menu, toggl and 15-23
c           jh         : wrong automatric p or s selection for spec
c    oct 22            : make it impossible to select a channel number
c                        outside range in chasel
c    nov 9             : small text change, DG to GD in spec output
c    nov 28            : fix error picking amplitude in multi trace mode, in chasel
c                        chan changed to channelname and put into common
c    dec 10            : add choise for reponse spectrum, problem with 
c                        channel number for picking amplitudes in multi mode
c    apr -may 2000 jh  : new waveform structure, fix so continuous plot
c                        has no gaps
c    may 31 jh         : zoom not possible with cursor between menu boxes
c    jun 5             : remove option othw in single trace mode
c    oct 24            : add 0.1 sec mark to axis in plotw
c                        make reading of phase times more accurate
c                        iasp calculation
c                        station and comp on same line in multi trace plot
c                        optinally high accuracy output
c    jan 7    2001  jh : auto amplitude pick made with A
c    jan 12            : make it possible to pick weight with long phase names
c                        and fix w-plot, remove all call tsend( a dummy now)
c                        do not allow polarity in long phase names
c    jan 21            : modify trans_filt for new remove_response routine
c                        which do not use mulplt.inc, add variable filters for
c                        mb,ms and ml, < for del-S
c    mar 5 2001 jh     : sdistance as real, real distance
c    mar 9             : no polarity on automatic amp
c    may  4            : do not pics phase if cursor before start of trace
c                        attempt to fix the phase non delete problem
c    may 19            : more accuracy in sdrop and radius
c    may 22            : more accuracy for period for amp
c    may 27            : overwrite spec of same type and same station
c    may 31            : small fix in above
c    july  1           : If  a new readign or inspection of an old
c                        reading, keep distace. Was deleted, however now that
c                        s-file is rewritten for each trace plottet in
c                        single trace mode, thsi is a bad option, routine
c                        convert
c    july 6            : make sure reading are written out after quit, else
c                        deleted readings might remain with a wrong time
c    july 9            : put output in file signal.out
c    sep 10            : also delete azim, per and amp if phase delete
c                        spectrum must be defined
c    sep 21            : bugs in above, now define spectrum velocity in mul_spec
c    nov 11            : null last 3 point in plotw to avoid abnormal end points
c    nov 17            : b for back in multi trace mode, all filters only
c                        case
c    nov 23   2001 jh  : do not stop in cont mode if one s-file has no wav
c    jan 25   2002 jh  : amplitudes in multi trace mode WAS NOT corrected for
c                        overscaling, small adjust of amplitude values, make it
c                        possible to plot one trace in multi trace mode,
c                        note: if in skipping routine averaging is used instead of
c                        skipping, amplitudes might be reduced a bit
c    jan 28            : do not read weight 2 times for same phase, clear amplitude
c                        before going to next trace if not used
c    feb 6             : change y scale bck to 2 trace maximum in multi trace
c    feb 14        lot : added amplitude selection
c    aug 16        lot : changed amplitude selection
c    april 03       jh : extract resposne corrected traces, multiple screens etc
c    may 8 2003    lo  : changed AMPx to AMx
c    may16 2003    lo  : fix bug with polarity for AMx phase
c    june 21       jh  : remove channel numbers
c    july 10       lo  : add search for stat/comp in plot_cont
c    may 5 2004    jh  : do not plot last 3 points in trace instead
c                        of setting them to a wrong value (plotw)
c    june 18       jh  : put event number back in
c    aug 4 2004    jh  : in convert, check for reading on next day, did no
c                        work for next month or year
c    aug 17        jh  : but in event number/window number
c    dec 2004      jh  : cont interval increase or decrease
c    jan 10, 2005  jh  : change position of event number
c    jan 12, 2005      : fix bugs in menu on off
c    may 23  2005  lot : routine convert, check for empty component codes
c    jun 1         jh  : file number on plot also form filenr.lis
c    jun 7             : amp bugs in multitrace mode
c    june 20       jh  : plt to eps
c    oct 3 2006    jh  : fix so no overflow in writing app. velocity
c    mar 15 2006   jh  : change distance order flag, can now only
c                        be turned on
c    oct 21 2007   jh  : put in location and network
c    feb 13 2008   lo  : add particle motion
c    apr 23 2008   pv  : added possibility to extract time windows in cont mode
c    apr 25 2008   jh  : add .bat to mulplt.ext if pc
c    may 17 2008   lo  : change @ for P, partcile motion
c    aug 15 2008   lo  : apply taper before filtering
c    aug 25 2008   lo  : moved particle motion to box 39
c    sep 12 2008   jh  : remove variabel chahead, fix anotation for R and T
c    oct 2  2008   jh  : AMb to AMB
c    oct 27 2008   jh  : cornerf written depending on size
c    oct 2  2009   jh  : new magnitudes MBB and MSS
c    dec 22 2009   jh  : rearrange menu
c    dec 28 2009   jh  : - implement poles and zeros for remove_response, used 
c                          now for fixed magnitude filters, 
c                        - make sure phases only can be picked inside 
c                          active window for single trace mode
c                        - do not allow selection of w etc in lower trace in 
c                          singel trace mode
c    jan 04 2010   jh  : use both paz and filter for ml, correct mb and ms
c                        for gain
c    jan 19 2010   jh  : also correct to filter used with wa
c    jan 26 2010   Jh  : do not null filters after response, now done in 
c                        spectral routine in sig_spec
c    feb 28 2010   jh: : change amplitude phase names to latest standard
c    mar 2 2010    jh: : change poles for mB and MS from 4 to 8, enebale gain 
c                        correction ofr ml for any filter, make it impossible
c                        to combine filter with mb,mB,ms and MS
c    mar 10 2010   jh  : wring filter with mss and mbb
c    may 10 2010   wcc: new options for display
c    may 10 2010   jh: fix bug with question on amplitudes not in nm, move 
c                      filter info and give it , initilize variable ch
c                      to get rid of squares
c    jun  1 2010   lo : removed ; form end of line
c    jul  9 2010   pv : fix bug with extracting timewindows in continous mode
c    sep 28 2010   jh : add new variabels spec_nphas, spec_phas,spec_tim to
c                       automatically find phase type for spectrum, if phase is
c                       read on a different component from the component used
c                       for the spectrum
c    oct 5  2010   jh : there was no check if both horizontal channels present
c                       for rotation so if a singel horizontal was there, it would
c                       be rotated with something in memory !!!!
c    nov  5 2010   lo : write 5 character station code to SPEC line
c    dec 27 2010   jh : gfortran on pc: remove hctype, cheng unit 15 to 25 and unit
c                       17 to 27 to avoid conflickt with dislin, remove display_type
c    jan 16 2011   jh : add all component mode, AllC
c                       fix variable filter size in main menu
c    jan 21 2011   jh : change synthetic y to Y and use y for all channel mode
c    mar 10 2011   jh : tchars must have text in variable, crash when uncertain time
c    apr 01 2011   jh: small letters for displacemnt etc
c    jun 20 2011   jh: filt variabel out of bounds
c    oct 28 2011   jh: work in progress for overlay plotting
c    jan    2012   jh: mt
c    jan 15 2012   jh: all filtering is now done in time domain, also when doing
c                      spectral analysis and response removal
c    feb 29 2012   Jh: make sure a 0 0 filter is not used, put dc rem and taper
c                      back to routine prepare, only with filter here. this means 
c                      that if filter and remove resp, dc and taper is done 2
c                      times
c    mar 23 2012   jh: option N to togle number of chans per screen, multi channel
c                      select with right mouse click
c    april 4 2012  jh: option R to change area radius
c    june 15 2012  jh: chec for error when reading s-file
c    aug   8 2012  lo: add amplitude apga
c    oct 4   2012  jh: avoid storing amplitude phase if deselcted, see
c                       jh oct 4 2012
c    jan 4 2012    jh: add argument to auto_tr
c    jan 10 2013   jh: change convert so if first letter of component
c                      code does not match, check if only the second
c                      character (orientation match)
c    jan 20 2013   jh: amp_title from 80 to 120 chars, make sure 
c                      skip not too large
c    feb 19 2013   lo: add change of magnification
c    feb 21 2013   lo: use of arrow keys
c    mar 19 2013   lo: set choise to 0 in pick menu box when menu on/off
c    apr 17 2013   jh: better accuracy when writing OM (omega0)
c    nov 28 2013   lo: dimension check when skipping samples for plotting
c    feb 14 2014   jh: making it impossible to read amplitude for magnitude
c                      if no response
c    feb 21 2014   pv: change due to warning: flip_rotate=.false. could not be reached due to goto
c    mar 8  2014   jh: undo change of jan 13, 2013, reset amplitude to zero
c                      when delete an amplitude to avoid per and amp to go to
c                      next reading, see 10-3-14 jh.
c    mar 24 2014   jh: add call to get spec model here instead of in mul_spec, 
c                      write out q-below-1hz in s-file
c    may 31 2014   jh: add in mul_spec again since question about unknown phase
c                      is only in mul_spec
c    2015.05.19    pv: added subroutine subroutine mulplt_cont_picking
c    2015.05.19    pv: added logging for mulplt cont 
c    2016 02 05    jh: bug with component and coda
c    2016 02 23    jh: add option U for multi station all component mode
c    2016 02 25    jh: default filter for single with eev option poo
c    2016.11.03    pv: in chasel, if only right click on station, plot all above
c                      plot operator and action
c    2016 11 20    jh: clear above area for mulplt.wav
c    2017 12 18    jh: make s general, some changes
c    2018 01 22    Jh: multichannels spectrogram
c    2018 02 05    jh: -----------------, fix filter and zoom
c    2018 03 05    jh: add distance to multi trace plot
c    
c
c    The routines can be divided into main routines which                       
c    again are built from secondary routines. The main routines are:            
c                                                                               
c           traceplot:   multiple trace plot                                    
c           picc:         interactive picking, one trace and zoom                
c                                                                               
c    Some of the secondary routines are:                                                
c                                                                               
c           tecpic:     interactive picking and cursor, one trace               
c           plotw:      plot one trace including axis and frame                 
c           trans_filt: transfer and filter one trace before it is              
c                       used by plotw                                           
c           chasel:     selects channels using traceplot                        
c                                                                               
c    The secondary routines are not intended for general use, but               
c    rather as building blocks for main routines.                               
c                                                                               
c    All routines communicate with each other through                 
c    common blocks. The main routine picc also uses the same
c    common blocks to communicate to a main program. The variables in           
c    the common blocks and the common blocks are defined in an                  
c    include file called mulplt.inc, Look in that file for definition
c    of variables.                                                              
c
c    Most graphics calls pass through a routines called x---
c    where x--- stands for the tectronics name. E.g. drwabs is
c    not called directly, but called xdrwabs. At the same time
c    arguments for coordinates have been changed from integer to real.
c    The reason is that the x-routines also make postscript
c    files. For more details, see xlib.c library.
c
c    Note that several parameters must be set before using especially           
c    the secondary routines. Most are set in the main routines, but             
c    check the following (see common block): over_scale, resolu,                
c    first, last, xpos, ypos, height, page_time and all plot options            
c    (pframe etc)                                                               
c                                                                               
c--------------------------------------------------------------                 
c                                                                               
c                                                                               
      subroutine picc                                                            
c                                                                               
c   plots and pick phases arrival times and some more                                         
c                                                                               
c   J. Havskov, November 1989                       
c                                                                               
c                                                                               
c                                                                               
c   All plotting is based on a tektronics 4010 screen, all coordinates          
c   used are in the standard range of x from 0 to 1023 and y from 0 to          
c   780. No transformations whatsoever is used.                                 
c   One trace from one station is first plotted on top part of screen           
c   together with previous picks. From then on ALL communication with           
c   the routine is done in graphichs input mode by defining keys on the         
c   keyboard for each action. The keys 1 to ?*... gives phases and              
c   upper case 1,2,3... weights. The mouse is used for moving the cursor        
c   and chosing the zoom window on the top trace. The zoomed window is          
c   plotted below and picks, delete etc can be done on both.                    
c   The routine plots exixting picks if any. These can be deleted by            
c   placing the cursor near the pick and pressing d. Once all pick/deletes      
c   are finished, new picks are returned and the old inputs are lost.           
c   However, the input vector is not changed.                                   
c
c   As of version 4 and more of seisan, this routine has manay more 
c   options, see seisan manual
c                                                                               
c   input/output: see common block description                                  
c
      implicit none
      save
c                                                                               
c   common block                                                                
c                                                                               
      include 'mulplt.inc'                                                      
c                                                                               
c   local varaibles                                                             
c                                                                               
c-- text string for plotting                             
      character*80 text
c--- old zoom
      integer old_zoom
      real xx,yy       ! help variable 
c-- counter                                                  
      integer 	   i
c
c   graphichs common block
c
      include 'seiplot.inc'
c                                                                               
c  define other plot options                                                    
c                                                                               
      pframe=1                                                                  
      paxis=1                                                                   
      paxisnumb=1                                                               
      phelp=1                                                                   
      pmax=1                                                                    
      ppick=1                                                                   
      ptitle=1                                                                  
      over_scale=1.0                                                            
      xpos=1                                                                    
      trace_delay=0.0        
      flip_rotate=.false.
 
c
c   clear filter if not set permanenet or default form mulplt
c
      if(.not.filter_perm.and.filt_single.eq.0) filt=0
      
      current_seq_chan=0    ! must be defined to get question about D,Vor A
c                                                                               
c-- start a new plot here, rewind plotting file if selected,so
c   all plots are not on top of each other                                
c
 1    continue			
      if(choice.eq.'REPL') then   ! check for window size change in x
         if(plotoption.eq.1) then
            rewind plotunit
            call init_post
         endif
c         if(display_type.eq.1) call updatewindowsize()
      endif
      old_zoom=0    ! clear old zooms 

c                                                                               
c   use start values specified from outside                                     
c                                                                               
c-- set flags                                
c
c lo tested to uncomment again, Aug 2012
      remove_response = 0				
      disp_vel=0
      do_spectrum=.false.
      do_3comp=.false.
      do_pmp=.false.
      do_wa=.false.
      do_mb=.false.
      do_ms=.false.
      do_mss=.false.
      do_mbb=.false.            
c      write(*,*) ' debug call picc disp_vel = ',disp_vel
                                                                                
      first=1                                                                   
      last=numb_samp                                                            
      page_time=(last-first+1)/rate                                             
c                                                                               
c   erase and position top plot                                                 
c              
 2    continue      ! enter here for a zoom in zoom                                                                 
      if(plotoption.eq.1) then  ! rewind plot file
         rewind plotunit
         call init_post
      endif
      call clear_display
      ypos=450                                                                  
      height=200

c                                                                               
c   plot help text                                                              
c
      call xset_color(color_title)                                                       
c
      if(phelp.eq.1) then
         write(text,200)                                                        
 200     format('F:Fin Q:Qui R:Rep Z-M:Flt G:Grd W:WA S:Spc O:Oth',                   
     *   ' A:Amp H:3C C:Cod D:Del')                                       
c-- plot help text                        
c         call xchars(text,80,0.0,760.0)	
c                                                                               
c   plot keyboard phases help line                                              
c                                                                               
         write(text,'(12(a1,1x,a3,1x))')(ascip(i),phs(i)(1:3),i=1,12)           

c         call xchars(text,72,0.0,720.0)                                          
         call xchars(text,72,0.0,700.0)                                          
      endif   

c
c  plot current mode
c
        call xset_color( xred )  
        call tchars(current_mode,34,600.0,700.0)
        call xset_color( xblack )                                                                  
c                                                                               
c   plot header on top trace                                                    
c                                                                               
      if(ptitle.eq.1) then                                                      
c-- find how many chars there is room for                 
c         i=65-xpos/16			
c         i=80
c         write(78,'(a)') head
c		 write(78,*)(ichar(head(i:i)),i=1,80)

         call xchars(head,80,xpos,ypos+height+20)                                
      endif                                                                     
c                                                                               
c   transfer and/or  filter  signal values                                      
c                                                                               
      call trans_filt(1)                                                           
c                                                                               
c   plot top trace, first give start time (second) if not calculated below                              
c                                                                               
      if(choice.ne.'ZOZO') then
         fsec=wav_sec(wav_current_chan(1))                                                              
         fmin=wav_min(wav_current_chan(1))+
     *   wav_sec(wav_current_chan(1))/60.0
         fhour=wav_hour(wav_current_chan(1))
      endif
c

      call plotw   
      
c
c   all this moved down here jan 2012 jh to enable response rem in top trace
c lo tried to move back aug 2012
c      remove_response = 0				
c      disp_vel=0
c      do_spectrum=.false.
c      do_3comp=.false.
c      do_pmp=.false.
c      do_wa=.false.
c      do_mb=.false.
c      do_ms=.false.
c      do_mss=.false.
c      do_mbb=.false.                                                             

c
c   clear setting of filter
c

      npasses=0

c                                                                               
c   invoke cursor input including phase pick                                    
c                                                                               

      call tecpic                                                               
 550  continue                  ! here from plot response
c                                                                               
c   check if finished or replot                                                 
c                                                                               
c-- finished                                   
      if(choice.eq.'FINI') goto 99	
c-- finished, make new selection               
      if(choice.eq.'OTHE') goto 99	
      if(choice.eq.'BACK') goto 99	
c-- finished, no update of readings       
      if(choice.eq.'QUIT') goto 99      
c-- change rotate existing channel without doing 3 comp, exit and reread
      if((flip_rotate)
     *.and.(.not.do_3comp).and.choice.eq.'REPL')  then
          flip_rotate=.false.
          goto 99
      endif
c-- replot, clear and replot top trace          
      if(choice.eq.'REPL') goto 1
c-- togl mode
      if(choice.eq.'TOGL') goto 99
c-- all channel mode
      if(choice.eq.'ALLC') goto 99 
c-- multi station all channel mode
      if(choice.eq.'ALCC') goto 99     
c-- help
      if(choice.eq.'HELP') then
         call mulplt_help
         goto 1
      endif
c-- response function
      if(choice.eq.'RESP') then
         call plot_resp
         goto 550
      endif
c                                                                               
c    calculate new first and last point, could be plotted above
c    or below                  
c                                                                               
      first=time1*rate+old_zoom + 0.5    ! 0.5 to find nearest sample
      last=first+(time2-time1)*rate + 0.5                                       
      page_time=time2-time1
      if(page_time.lt.1.0/rate) page_time=1.0/rate
c
c  check if zoom in zoom on top trace
c
      if(choice.eq.'ZOZO') then
         fsec=wav_sec(wav_current_chan(1))+(first-1)/rate                                               
         fmin=wav_min(wav_current_chan(1))+(first-1)/(rate*60.0)
         fhour=wav_hour(wav_current_chan(1))+(first-1)/(rate*3600.0)
         old_zoom=first                ! in case there is another zoom
         goto 2
      endif

c-------------------------------------------------
c   position lower plot, could be a spectrum
c--------------------------------------------------

      ypos=150                                                                  
      height=200                                                                
c                                                                               
c   transfer and/or  filter, response removal,  signal values                                      
c  
                                                                             
      call trans_filt(1)                                                           

c
c   if no response and requirement was a response for amplitude picking, do not plot
c
        if(do_mb.or.do_wa.or.do_ms.or.
     *  do_mbb.or.do_mss) then
          if(wav_resp_status(1:1).eq.'9')then
              text='Press any key to continue'

              call tchars(text,25,600.0,ypos-50.0)
c
c  blank out if a filter has been plotted
c
	      call xset_color(xwhite)          
	      call fillbox(600.0,0.0,800.0,40.0) 
	      call xset_color(xblack)                 ! Text colour.
              filt=0
c
c   call up cursor, any char valid
c
             call xscursr(i,xx,yy)
             goto 1
         endif
      endif

      if(abs(spec_out).eq.1) then
         open(25,file='signal.out',status='unknown')
         write(25,*)
     *   'Signal from MULPLT zoom s. trace mode'
         write(25,*)' Number of points:', last-first
         do i=first,last-1
            write(25,*) y(i)
         enddo
         close(25)
      endif
c
c   3 component analysis
c
      if(do_3comp) then
          call get_3_chan
c
c   get picking routine up for associating azimuth and velocity with a phase
c
          first=old_zoom  ! make sure pics are made realtive to zoom window,
          call tecpic     ! - and not the 3 component window
c
          goto 500
      endif
c
c  do spectrum if chosen
c
      if(do_spectrum) then
         swindow=page_time            ! window for spectrum
         stime=first/rate	  	      ! time of first point in spec. rel. start
         call xset_color(color_spec)  !JAB(BGS)Jan95
         call amp_spec
         call xset_color(color_def)   !JAB(BGS)Jan95.
         filt=0 
         goto 500
      endif
c                                                                               
c   now is must be just zoom, calculate start second for lower plot        
c                                                                               
         fsec=wav_sec(wav_current_chan(1))+(first-1)/rate                                               
         fmin=wav_min(wav_current_chan(1))+(first-1)/(rate*60.0)
         fhour=wav_hour(wav_current_chan(1))+(first-1)/(rate*3600.0)
c                                                                               
c   plot lower plot                                                             
c                                                                               
         call plotw
                                                      
c                                                                               
c   invoke cursor including phase pick                                          
c                                                                               
         call tecpic                                                            
c                                                                               
c   check if end or new plot                                                    
c                                                                               
c
 500  continue
c
c  clear filter passes
c
      npasses=0
c
c-- FINISHED                                   
      if(choice.eq.'FINI') goto 99	
c-- REPLOT                                     
      if(choice.eq.'REPL') go to 1	
c-- togl mode
      if(choice.eq.'TOGL') goto 99
c-- help
      if(choice.eq.'HELP') then
         call mulplt_help
         goto 1                     ! replot
      endif
                                                                                
 99   continue                                                                  
c-- clear                                 
      call clear_display			
c
c   make sure no flags set
c     
      remove_response = 0
      do_spectrum=.false.
      do_3comp=.false.
      do_wa=.false.
      do_mb=.false.
      do_ms=.false.
      do_mbb=.false.
      do_mss=.false.
      disp_vel=0

      return                                                                    
      end                                                                       
                                                                                
c                                                                               
c############################################################################   
c                                                                               
       subroutine tecpic                                                        
                                                                                
c                                                                               
c   pick and delete phases with the keyboard keys, a total                      
c   of numb_keys phases are presently defined.                                         
c                                                                               
c                                                                               
c-- common block 
       implicit none   
       save                                  
       include 'mulplt.inc'	
       include 'seiplot.inc'
c                                                                               
c   local variables                                                             
c                                                                               
c-- string for plotting chars                            
       character*80 text	
c-- temporary period                                            
       real 	per		
c-- temporary amplitude                                          
       real	amp		
c-- return x and y from cursor                                 
       real	ix,iy		
c-- save of ix and iy                             
       real     ix1,iy1,ix2	
c-- return asci value from cursor                             
       integer	ich		
c-- logical for filter selection
       logical filsel
c-- x-position of a pick                                       
       real	picix		
c-- indexes for phases
       integer new_index,old_index,new_phase
       integer ichan_pic        ! sequectial channel number
c-- check if old phase there
       logical old_pic
c-- counters                                                  
       integer	j,k,kphas,i
c-- count number of times title is in same position
       integer ntit
       character*80 answer
c-- help variable
       real xmin
       real xmin_p,xmin_s   ! for finding which phase is nearest spectrum
c      integer on(50)        ! 0: box not selected, 1:box selected   ! now in coimmon block, jan 05
      logical sun,pc,linux
c-- amplitude text
      character*80 amp_text(20)
c-- title
      character*120 amp_title
c-- last char clicked
      character*1  last_char
c-- selection
      integer on_amp(50)
c-- flag
      logical amp_flag,flag
c-- number of elements in amp_text
      integer n_amp_text
c--gain of sp or lp, real and complex
      real gain
      complex hs
c-- local selected start time                         
      real      local_start	
c-- local selected stop time                      
      real      local_stop      
c
c   poles and zeroes for correction of reading 
c
      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros
      integer pole_low,pole_high   ! single filter constants
      real f_high,f_low                ! -----------------------
      integer pole_low_save,pole_high_save  ! for ml filter

      common /trans_filt_com/pole,zero,npole,nzero,norm,
     *pole_low,pole_high,f_low,f_high,pole_low_save,pole_high_save


c
      call computer_type(sun,pc,linux)
c                                                                               
c                  
c      write(*,*) ' debug tecpic '
      ntit=0    
      call xset_color(color_pic)                                                   
c
c set amplitude text
c
      do i=1,12
        amp_text(i)=' '
      enddo
c
      amp_text(1)(1:3)='AMP'
      amp_text(2)(1:4)='IAML'
      amp_text(3)(1:4)='IAmb'  
      amp_text(4)(1:7)='IAMs_20'
      amp_text(5)(1:4)='AMPG'
      amp_text(6)(1:4)='AMSG'
      amp_text(7)(1:4)='AMPN'
      amp_text(8)(1:4)='AMSN'
      amp_text(9)(1:1)='E'
      amp_text(10)(1:1)='I'
      amp_text(11)(1:7)='IVmB_BB'
      amp_text(12)(1:7)='IVMs_BB'
      amp_text(13)(1:7)='APGA'
      n_amp_text=13

c
c   clear all choises for menu
c
       do i=1,42
         onbox(i)=0
       enddo
c
c   if called from mulplt in traceplot mode, jump directly to picking phases
c   assume one pick already made
c
       if(last_ix.ne.0) then
          ix=last_ix         ! load in values from traceplot
          iy=last_iy
          ich=last_ich
          goto 1111          ! go directly to checking pick
       endif
c                                                                               
c   plot any old picks. If page_start is zero, call comes from pic, else
c   from traceplot, and variable first is not quite the same. If ppick is
c   2, do not plot old picks.               
c                                                                               
      if(ppick.ne.2) then                                                  
       do 13 i=1,nphas                                                          
          if(page_start.ne.0.0) then   ! from traceplot
             ix=(pictim(i)-page_start)*xscale+xpos               
          else
             ix=(pictim(i)-(first-1)/rate)*xscale+xpos  ! from pic
          endif
c-- check that inside window         
          if(phase(i)(1:1).eq.'Y') call xset_color(color_syn)   ! synthetic                 call xset_color(color_pic)   ! back to normal color                     
          if(ix.gt.xpos.and.ix.lt.1023) then	
             call xmovabs(ix,ypos+height/5)                                     
             call xdrwabs(ix,ypos+height-height/5)                              
          if(phase(i)(1:1).eq.'Y') call xset_color(color_pic)   ! synthetic                 call xset_color(color_pic)   ! back to normal color                     
c                                                                               
c   plot on tek device                                                          
c                                                                               
c-- phase, if first letter is y, a test phase, plot below            
c
             if(phase(i)(1:1).ne.'Y') then
                call tchars(phase(i)//alt_weight(i),
     *          10,ix-15,ypos+height-25.0)
             else
                call xset_color(color_syn)   ! synthetic has a different color
                call tchars(phase(i)//alt_weight(i),10,ix-15,ypos+5.0)    
                call xset_color(color_pic)   ! back to normal color                     
             endif
c
c  plot on post script since different position                                  
c                                                                               
c-- phase             
             if(phase(i)(1:1).ne.'Y') then
                call pchars(phase(i)//alt_weight(i),
     *          10,ix-15,ypos+height-5.0)
             else
                call xset_color(color_syn)   ! synthetic has a different color
                call pchars(phase(i)//alt_weight(i),10,ix-15,ypos+5.0)    
                call xset_color(color_pic)   ! back to normal color                     
             endif
c                                                                               
c  if any amp, per, vel or azi present, indicate on plot                        
c                                                                               
             if(amplitude(i).ne.0.0.or.period(i).ne.0.0.                        
     *       or.velocity(i).ne.0.0.or.azimuth(i).ne.0.0) then                   
                call xmovabs(ix,ypos+height/5)                                  
                call xdrwabs(ix+15,ypos-15)                                     
                call xmovabs(ix,ypos+height/5)                                  
                call xdrwabs(ix-15,ypos-15)                                     
             endif                                                              
          endif                                                                 
  13   continue                                                                 
       page_start=0.0
      endif
c                                                                               
c   return if not in picking mode                                                
c                                                                               


        if(ppick.eq.0) return   ! jh 18-10-02  to make picking az from fk work again
c
c                                                    
c-- clear choice, action at return, see PIC                 
       choice='    '		
c
c   clear filter flag to make sure next plot has no filter if not selected below
c   unless permanent set. do not clear if first time def filter from
c   mulplt
c
       if(.not.filter_perm.and.filt_single.eq.0) filt=0
       if(filt_single.eq.1) filt_single=0   ! clear filter next time
c
c  call up menu box, put in x and y outside range to make sure no choise
c
      onbox(14)=1
      if(plotoption.lt.2)
     *call pick_menu_box(onbox,14,63.0,16.0,'single',
     *       3.0,758.0,0.0,999.0,mouse,mouse,show_menu,flow,fhigh)
c
c-------------------------------------------------------------------------
c                                                                               
c   start picking phases or giving commands in GIN input mode,                  
c   return here after reading one phase or GIN input                            
c                                                                               
c
 11    continue
c
c-------------------------------------------------------------------------
c-------------------------------------------------------------------------
c
c   if this call originates in traceplot mode, return when getting here the 
c   second time
c
       if(last_ix.ne.0) then
          last_ix=0
c         goto 999
          goto 99
       endif
c
c-- call up cursor, stop here until an input from keyboard or mouse                               
c
       call xscursr(ich,ix,iy)			
 
 1111  continue               ! enter here from start if jumping from traceplot
c
c
      if(plotoption.lt.2)
     *call pick_menu_box(onbox,14,63.0,16.0,'single',
     *     3.0,758.0,ix,iy,ich,mouse,show_menu,flow,fhigh)
c                                                                               
c  check if quit as specified by Q or q, no phase update                        
c                                                                               
c-- Q or q                               
       if(ich.eq.81.or.ich.eq.113) then		
          choice='QUIT'                                                         
c jul 5 20001 jh         goto 999                                                                
          goto 99
       endif     
c       write(*,*) ' xxx ich ', ich
                                                               
c---------------------------------------------------------------                                                                               
c  check if finished with the trace as specified by F or f                      
c---------------------------------------------------------------                
c-- F or f                               
c       if(ich.eq.70.or.ich.eq.102.or.ich.eq.3) then		
       if(ich.eq.70.or.ich.eq.102) then		
          choice='FINI'     
c-- update readings array                                    
          goto 99				
       endif                                                                    
                                                                                
c--------------------------------------------------------------------        
c   check if back one channel               
c--------------------------------------------------------------------           
       if(char(ich).eq.'B'.or.ich.eq.1) then
          choice='BACK'                                                         
          goto 99                                                     
       endif                                                         
c
c--------------------------------------------------------------------        
c   check if other channels should be selected indicated by o               
c--------------------------------------------------------------------           
       if(char(ich).eq.'o') then                                         
          choice='OTHE'                                                         
          goto 99                                                     
       endif                                                         
c
c--------------------------------------------------------------------
c  check if other waveform file  is wanted indicated by W
c--------------------------------------------------------------------
c
       if(char(ich).eq.'W') then
          choice='OTWA'
c jul5 2001 jh         goto 999
          goto 99
       endif
c
c--------------------------------------------------------------------
c    check if help
c--------------------------------------------------------------------
       if(char(ich).eq.'?') then
          choice='HELP'
          goto 99
       endif           
c
c--------------------------------------------------------------------
c    check if plot of response function
c--------------------------------------------------------------------
       if(char(ich).eq.':') then
          choice='RESP'
          goto 99
       endif           
c--------------------------------------------------------------------
c    check if component rotation
c--------------------------------------------------------------------
       if(char(ich).eq.'U') then
          flip_rotate=.true.   ! new change in rotate
          if(rotate) then
              rotate=.false.
          else
              rotate=.true.
          endif
          goto 11
       endif
c
c----------------------------------------------------------------
c   check if going back to multi trace mode from single trace mode
c   indicated by t 
c---------------------------------------------------------------
c
       if(char(ich).eq.'t') then		
c         write(*,*) ' char ',char(ich) ! xxx
          choice='TOGL'
          goto 99
       endif

c
c----------------------------------------------------------------
c   check if going back to multi trace mode from all component mode from
c   indcated by y
c---------------------------------------------------------------
c
       if(char(ich).eq.'y') then		
          choice='ALLC'
          goto 99
       endif

c
c----------------------------------------------------------------
c   check if going back to multi trace mode from all component mode
c   many stations indcated by U
c---------------------------------------------------------------
c
       if(char(ich).eq.'u') then		
          choice='ALCC'
          goto 99
       endif
c
c------------------------------------------------------------------                                                                               
c  check if wood anderson response simulated specified by w.
c  in this and follwing similar seelctions, do not allow this
c  option in single trace mode lower window, check by position              
c---------------------------------------------------------------------             
c
c  for next many choises, change color
c
       call xset_color(color_title)		
                                                                                
c-- w                               
       if(char(ich).eq.'w'.and.ypos.ne.150.0.and.xpos.lt.10.0) then
          write(text,201)                                                       
 201      format('Sel. window for Wood And. ')                                 
c-- response flag set  
          call xchars(text,25,680+xpos,ypos-55-ntit*12)	
		  ntit=ntit+1
          remove_response = 1                                                   
          do_wa=.true.
       endif                                                                    

c increase or decrease plot magnification
       if(ich.eq.2) then
         ymag=ymag*2.
         choice='REPL' 
         goto 99
       endif
       if(ich.eq.4) then
         ymag=ymag/2.
         choice='REPL' 
         goto 99
       endif

c
c--------------------------------------------------------------
c if default to automatic coda then change 'c' to 'C' or 'C' to 'c'

       if (do_auto_coda) then

         if(char(ich).eq.'c') then
           ich=67   ! 'C'
         elseif (char(ich).eq.'C') then
           ich=99   ! 'c'
         endif

       endif

c
c--------------------------------------------------------------
c  check if automatic coda
c

       if(char(ich).eq.'C') then

         call auto_coda

         write(text,'(a14)') 'Automatic Coda'
         call xchars(text,25,680+xpos,ypos-55-ntit*12)
c
c calculate ix and set ich to 'c'
c
         
c         ix=(auto_coda_time) * xscale + xpos  ! from pic

          if(page_start.ne.0.0) then   ! from traceplot
             ix=(auto_coda_time-page_start)*xscale+xpos
          else
             ix=(auto_coda_time-(first-1)/rate)*xscale+xpos  ! from pic
          endif


       endif

c
c------------------------------------------------------------------                                                                               
c  check if mb type response simulated specified by J or j.              
c---------------------------------------------------------------------             
                                                                                
       if(char(ich).eq.'j'.and.ypos.ne.150.0.and.xpos.lt.10.0) then		
          write(text,209)                                                       
 209      format('Sel. window for mb filter. ')                                 
c-- response flag set  
          call xchars(text,27,680+xpos,ypos-55-ntit*12)	
		  ntit=ntit+1
          remove_response = 1                                                   
          do_mb=.true.
       endif 
c--------------------------------------------------
c   mB
c---------------------------------------------------
       if(char(ich).eq.'J'.and.ypos.ne.150.0.and.xpos.lt.10.0) then		
          write(text,2209)                                                       
 2209     format('Sel. window for mB filter. ')                                 
c-- response flag set  
          call xchars(text,27,680+xpos,ypos-55-ntit*12)	
		  ntit=ntit+1
          remove_response = 1                                                   
          do_mbb=.true.
       endif                                                                      
c------------------------------------------------
c  ms
c------------------------------------------------
       if(char(ich).eq.'k'.and.ypos.ne.150.0.and.xpos.lt.10.0) then		
          write(text,1209)                                                       
 1209     format('Sel. window for Ms filter. ')                                 
c-- response flag set  
          call xchars(text,27,680+xpos,ypos-55-ntit*12)	
		  ntit=ntit+1
          remove_response = 1                                                   
          do_ms=.true.
       endif  
c------------------------------------------------------
c  mS
c------------------------------------------------------

       if(char(ich).eq.'K'.and.ypos.ne.150.0.and.xpos.lt.10.0) then		
          write(text,1210)                                                       
 1210     format('Sel. window for MS filter. ')                                 
c-- response flag set  
          call xchars(text,27,680+xpos,ypos-55-ntit*12)	
		  ntit=ntit+1
          remove_response = 1                                                   
          do_mss=.true.
       endif                                                                        
c
c------------------------------------------------------------------                                                                               
c  check ground motion shall be made, specified by g or G.              
c---------------------------------------------------------------------             
                                                                                
c-- G or g                               
       if((char(ich).eq.'g'.or.char(ich).eq.'G').
     *  and.ypos.ne.150.0.and.xpos.lt.10.0) then		
          write(text,'(a)')'Sel. wind. for ground mo. '
c-- response flag set  
          call xchars(text,25,680+xpos,ypos-55-ntit*12)	
          ntit=ntit+1
          remove_response = 1                                                   
       endif                                                                    
c
c-----------------------------------------------------------------
c   check if spectrum
c-----------------------------------------------------------------
c

c-- S or s                               
       if(ich.eq.83.or.ich.eq.115.and.ypos.ne.150.0.and.xpos.lt.10.0) 
     *    then		
          noise_spectrum=0                ! no noise spectrum
          write(text,'(a)')'Sel. window for spectrum '                               
          call xchars(text,25,680+xpos,ypos-55-ntit*12)	
          ntit=ntit+1
          do_spectrum=.true.
          if(char(ich).eq.'s') noise_spectrum=1   ! noise spectrum                                     
       endif                                                                    
c
       call xset_color(color_pic)
c
c-------------------------------------------------------------------
c   check if filter specified
c---------------------------------------------------------------------
c                                                                               
       call get_filter(ich,filsel)
c
c   if filter was selected, print on screen and return for
c   next input
c
       if(filsel) then
	      call xset_color(color_title)
          write(text,202) flow(filt),fhigh(filt)                                
 202      format('Next filter ',f6.3,1x,f6.3)                                          
c-- plot filter set               
          call xchars(text,25,340+xpos,ypos-55)	
c-- back for next cursor input                               
          call xset_color(color_pic)
          goto 11				
       endif                                                                    

c	   
c----------------------------------------------------------------
c   check if 3 component analysis, only selectable on top trace
c----------------------------------------------------------------
c
       if((char(ich).eq.'h'.or.char(ich).eq.'H').and.iy.gt.400.0) then
          do_3comp=.true. 
c
c   indicate on plot
c
          write(text,'(a)')' Select window for 3COMP'
          call xchars(text,24,680.0,ypos-55.0-ntit*12)
          ntit=ntit+1
          goto 11
       endif                                                         
c                   
c------------------------------------------------------------                                                                               
c   check if replot option indicated by r                                   
c------------------------------------------------------------
c                                                                               
c       if(ich.eq.114) then                                         
       if(char(ich).eq.'r') then                                         
          choice='REPL'                                                         
c-- return from routine, save first                           
          goto 99			
       endif                                                                    

c------------------------------------------------------------
c check for resize
c------------------------------------------------------------
c lot 30/06/2003
c       if(char(ich).eq.'#') then                                         
       if(char(ich).eq.'r') then                                         
          choice='REPL'                                                         
c-- return from routine, save first                           
          goto 99			
       endif                                                                    
c
c   ix and iy have strange values on PC because cursor has not yet been
c   moved, just ignore, since following choices need a position
c
       if(ix.eq.0.0) goto 11
                                                                                
c                                                                               
c  check if delete phase if cursor is near existing pick                        
c  note, this deletes all data for this phase inc. amp, azimuth etc.                                                                 
c                                                                               
c-- D or d for delete                   
       if(ich.eq.100.or.ich.eq.68) then	 	
c-- check if any pics are near                          
          do 17 i=1,nphas				
c-- screen x-pos of old p
             picix=(pictim(i)-first/rate-chan_delay)*xscale + xpos 
c-- found a pick to delete              
             if(abs(ix-picix).lt.20) then	
c-- position of pic found  
                ix=(pictim(i)-(first-1)/rate-chan_delay)*xscale+xpos  !jj
c-- indicate deletion of pick    
                call xmovabs(ix,ypos+height-40)  
                call xdrwabs(ix,ypos+height+20)                                 
                text(1:1)='D'                                                   
c-- plot D for delete      
                call xchars(text,1,ix-5,ypos+height+25)
c-- delete                                       
                pictim(i)=-100000.0		 	
c
c   also delete amp, per and azim, jh 9-01
c
                amp=0.0
                per=0.0
                azim=0.0
                veli=0.0
c change 10-3-14 jh
                amplitude(i)=0.0
                period(i)=0.0
                azimuth(i)=0.0
                velocity(i)=0.0

c-- do not check more phases                           
                goto 18				
             endif                                                              
 17       continue                                                              
 18       continue                                                              
c-- beep to indicate finished                      
calk          call ancho(7)				
c-- up to next cursor input                                  
          goto 11				
       endif                                                                    
c
c--------------------------------------------------------------------------
c   check if selection of nearest predefined synthetic phase
c--------------------------------------------------------------------------
c
       if(char(ich).eq.'Y') then	 	
          xmin=999999.0
          k=0                ! indicate nearest synthetic phase
c-- find nearest synthetic pick                           
          do i=1,nphas				
c-- screen x-pos of synthetic picks only
             if(phase(i)(1:1).eq.'Y') then
                picix=(pictim(i)-first/rate-chan_delay)*xscale + xpos 
                if(abs(ix-picix).lt.xmin) then
                   xmin=abs(ix-picix)
                   k=i
                endif
             endif
          enddo	
c
c   save nearest synthetic pick if any
c
          if(k.gt.0) then
             pictim(k)=first/rate+(ix-xpos)/xscale  ! time of pick
             pictim(k)=pictim(k)+chan_delay
             phase(k)(1:1)='E'                      ! make sure phase saved                                   
c                                                                               
c   check for  polarity reading indicated by y - cursor position
c   do not read if phase name occupy posiiton
c                      
             if(phase(k)(8:8).eq.' ') then                                                         
                if(iy.gt.ypos+height*0.85) phase(k)(8:9)='C '
                if(iy.lt.ypos+height*0.15) phase(k)(8:9)='D '
             endif
c                                                                               
c   plot phase with name and polarity                                           
c                                                                               
             call xmovabs(ix,ypos+40)                                                 
             call xdrwabs(ix,ypos+height-40)                                          
             call tchars(phase(k),9,ix-15,ypos+height-25.0)    
          endif
c-- up to next cursor input                                  
          goto 11				
       endif                                                                    
c
c--------------------------------------------------------------------------                                                                               
c   check if this was input of weight                                           
c--------------------------------------------------------------------------
c                                                                               
c-- check all weight codes                                    
       do j=1,10				
c-- was a weight, find pick       
          if(ich.eq.ichar(key_weight(j))) then 	
c-- check if any pics are near                         
             do  i=1,nphas				
c-- screen x-pos of old 
                picix=(pictim(i)-first/rate-chan_delay)*xscale+xpos 
c-- found a pick to weight           
                if(abs(ix-picix).lt.20) then	
c-- position of pic found
                   ix=(pictim(i)-first/rate-chan_delay)*xscale +xpos 
c-- plot w
                   call xchars(i_weight(j),1,ix+55+xpos,ypos+height-25) 
c-- save weight, but only if the character location is blank or a number
c   since there could be a long phase name. in that case store in 
c   alt_weight
c
                   if(phase(i)(6:6).eq.' '.or.
     *             (ichar(phase(i)(6:6)).gt.47.and.
     *              ichar(phase(i)(6:6)).lt.58)) then
                      phase(i)(6:6)=i_weight(j)  
                      phase(i)(7:7)=' '
                   else
                      alt_weight(i)=i_weight(j)
                   endif
c-- do not check more phases                        
                   goto 118			
                endif                                                           
             enddo                                                              
          endif                                                                 
        enddo                                                                   
 118    continue                                                                
c
c------------------------------------------------------------------
c   check if this was input of amplitude, will also read period                 
c------------------------------------------------------------------
c
c-- a                                   
      amp_flag=.false.
      if(char(ich).eq.'a') then	
           if (ymag.ne.1.) then
             write(*,*) ' not reading amp if magnification is not 1 '
             goto 11
           endif

c          write(6,'(a)') char(ich)
c-- save first amp                                             
          amp=iy			
c-- save first x-position                                          
          per=ix			
c-- indicate position of pick                  
          call xmovabs(ix-10,iy)  	
          call xdrwabs(ix+10,iy)                                                
          call xmovabs(ix,iy-10)                                                
          call xdrwabs(ix,iy+10)                                                
c-- call up cursor for second reading           
 66       continue                                                              
          call xscursr(ich,ix,iy)	
c-- second reading must be a   
          if(ich.ne.97) then
c
c   plot message below if single mode (pmax=1), else at beginning of trace
c
             text='You must enter one more amplitude reading a'
             if(pmax.eq.1) then
                call xmessage(text,1,34,10.0,60.0)
             else
                call xmessage(text,1,34,xpos+10.0,ypos+height/2.0)
             endif
             go to 66	
          endif
c-- indicate position of pick                  
          call xmovabs(ix-15,iy)  	
          call xdrwabs(ix+15,iy)                                                
          call xmovabs(ix,iy-15)                                                
          call xdrwabs(ix,iy+15)                                                
c
c-- amplitude is zero to peak, if in multi trace mode, use
c   value from scaling of current channel
c   in multi trace mode, channel number must be recalculated since it could be
c   one channel off. this is because amplitude might reach into trace on either side
c   when picking the first 'a', so channel number must be calculated from an
c   average of the two amplitude pics
c          
          if(last_ix.ne.0) then     ! call from multi trace mode
c
c   find channel, assume top of plot frame is at y=max_ypos
c
             ichan_pic=(max_ypos-(iy+amp)/2.0)/height+1  ! sequential number plotted
             ypos=max_ypos-ichan_pic*height  ! find position of trace
             current_chan=channelname(ichan_pic)    ! channel number in file
             amp=(abs(amp-iy)+0.5)
     *       /(over_scale*yscale_mul(current_chan)*2.0)	
          else      
             amp=(abs(amp-iy)+0.5)/(yscale*2)   ! 0.5 to account for round off error	
          endif
c-- read half period only                      
          per=2*abs(per-ix)/xscale	
          amp_flag=.true.
      endif


c
c--------------------------------------------------------------
c  check if automatic amplitude, only do if no manual amp present
c  and only in single trace mode
c--------------------------------------------------------------
c

       if(char(ich).eq.'A'.and.amp.eq.0.0.and.per.eq.0.0.
     *  and.last_ix.eq.0) then

         call auto_amp_mulplt(ix,amp,per)
c
c   check if amplitude made
c
         if(amp.gt.0.0) then
            write(text,'(a25)') 'Automatic amplitude made'
            call xchars(text,25,680+xpos,ypos-55-ntit*12)
            amp_flag=.true.
         else
            write(text,'(a29)') 'Automatic amplitude not made'
            call xchars(text,29,680+xpos,ypos-55-ntit*12)
            goto 11
         endif
       endif
c
c---------------------------------------------------------
c  check if predefined amplitude CAN BE USED
C---------------------------------------------------------
C
       do j=1,n_amp_text
         on_amp(j)=0
       enddo
109    continue
c
c check if Ml, Mb or Ms filter active
c
       if (amp_flag) then
          if (do_wa) then
            on_amp(2)=1
          elseif (do_mb) then
            on_amp(3)=1
          elseif (do_ms) then
            on_amp(4)=1
          elseif (do_mbb) then
            on_amp(11)=1
          elseif (do_mss) then
            on_amp(12)=1
          else
c
c selection
c
c            choice='REPL' 
            amp_title='Select amplitude type'
            call input_box(n_amp_text,on_amp,1,100.,30.,amp_text,8,
     *                     800.,650.,amp_title,last_char,0,1)
            if (last_char.eq.'q'.or.last_char.eq.'Q')return  
c            if (last_char.eq.'r'.or.last_char.eq.'R')return   
          endif
c
c check that only one amplitude selectd
c
          flag=.false.
          do j=1,n_amp_text
            if (flag.and.on_amp(j).eq.1) then
              goto 109
            elseif (on_amp(j).eq.1) then
              flag=.true.
            endif
          enddo
c
c save amp phase
c
          phs(99)=' '
          do j=1,n_amp_text
            if (on_amp(j).eq.1) then
              new_phase=99
              phs(new_phase)=' '//amp_text(j)
            endif
          enddo
          if (phs(new_phase).eq.' '.and.last_ix.ne.0) then
            new_phase=99
            phs(new_phase)=' AMP'
          endif
          goto 5    ! below. save and quit routine
        endif

c
c-------------------------------------------------------------------------
c   check if zoom window select indicated by mouse click,                       
c   must be selected on upper plot, if not return cursor
c-------------------------------------------------------------------------
c                                                                               
c-- check if mouse click                          
       if(ich.eq.mouse) then				
c-- not on upper plot              
         if(iy.lt.400.or.ypos.lt.400) goto 11		
c-- menu on, cursor cannot be in menu boxes
         if(onbox(14).ne.0.and.iy.gt.720) goto 11
         call xset_color(color_zoom)
c                                                                               
c   plot and save first position                                                
c                                                                               
         ix1=ix                                                                 
         iy1=iy                                                                 
         call xmovabs(ix1,ypos)                                                 
         call xdrwabs(ix1,ypos+height)                                          
 111     continue
c                                                                               
c   read second position                                                        
c                                                                               
         call xscursr(ich,ix2,iy)                                               
                                                                                
c                                                                               
c  check if ok, last must be 3 pixels later than first                                   
c                                                                               
c-- not right                                        
         if((ix2-ix1).lt.3) then	
c-- do it again, call cursor                                 
            goto 111		
         else    ! plot
           call xmovabs(ix2,ypos)                                                 
           call xdrwabs(ix2,ypos+height)                                          
           call xset_color(color_pic)
         endif                                                                  
c
c   if selection for a spectrum, check if near P or S- phase
c
c
       if(do_spectrum) then	 	
          xmin_s=999999.0
          xmin_p=999999.0
c-- find nearest P or S pick                           
          do i=1,spec_nphas				
c-- screen x-pos of start of spectral window
             if(spec_phs(i)(2:2).eq.'S') then
                picix=(spec_tim(i)-first/rate-chan_delay)*xscale + xpos 
                if(abs(ix-picix).lt.xmin_s) then
                   xmin_s=abs(ix-picix)
                endif
             endif
             if(spec_phs(i)(2:2).eq.'P') then
                picix=(spec_tim(i)-first/rate-chan_delay)*xscale + xpos 
                if(abs(ix-picix).lt.xmin_p) then
                   xmin_p=abs(ix-picix)
                endif
             endif
          enddo	
c
c  check if p or s is near if at all
c
          spec_phase='N'        ! initially no phase
c
c   check if within 10 secs
c
          if(xmin_s.le.xmin_p) then
            if(xmin_s.lt.10.0*xscale) spec_phase='S'
          else
            if(xmin_p.lt.10.0*xscale) spec_phase='P'
          endif

comment out may 30 2014
c           if(spec_phase.eq.'N') then
c 9765        continue
c             text='Spectrum not defined, enter p or s'
c             call oneline(text,35,answer,2,500.0,500.0)
c             spec_phase=answer(1:1)
c             if(spec_phase.eq.'p') spec_phase='P'
c             if(spec_phase.eq.'s') spec_phase='S'
c
c             if(spec_phase.ne.'S'.and.spec_phase.ne.'P') goto 9765
c          endif
       endif

c
c   if spectral model set, all parameters come from spec model and overides values
c   from MULPLT.DEF. q0, qalpha and kappa are returned for p or s repectively.
c   if no spectral model, then use mulplt.def values
c
      spectral_model_used=2   ! def from mulplt.def
      if(spectral_model.eq.1.0) 
     *call get_att_vel(edepth,spec_phase,q0,qalpha,kappa,
     * pvelocity,svelocity,density,spectral_model_used)                                                                                
c                                                                               
c   calculate times                                                              
c                                                                               
         time1=(ix1-xpos)/xscale                                                
         time2=(ix2-xpos)/xscale                                                

c-- return with choice equal zoom                         
         if(iy.le.650.0) then
             choice='ZOOM'		
         else
             choice='ZOZO'    ! zoom in zoom if cursor above top plot
         endif
c-- end of window selection, return after saving                
         goto 99		
c-- - new readings                                            
       endif	     		
c                                                                                
c----------------------------------------------------------------------------                                                                               
c   if you get to here, graphic input might be a phase, determine phase type     
c   assume only  predefined phases, phase pick must be made inside trace
c---------------------------------------------------------------------------
c

       if(ix.lt.xpos)        goto 11  ! cursor outside horizontally, try again
c       write(6,*) 'ypos,height,iy', ypos,height,iy
       if(iy.gt.ypos+height) goto 11 ! cursor over window, jh dec 09
       if(iy.lt.ypos)        goto 11 ! cursor under window, jh dec 09
c
c  curser inside window
c
       do 3 new_phase=1,numb_keys                                                              
          if(char(ich).eq.ascip(new_phase)) goto 5                                            
 3     continue                                                                 
c                                                                               
c   phase not found, invalid input, try again                    
c                                                                               
c
       go to 11
c
c   phase found
c
 5     continue          
c
c   check if there was an old pick for that phase, only check first 5 chars since 
c   weight should not be part of phase name. However for long phase names, this could 
c   be a problem,
c   currently in iaspei amplitudes and long phase manes read with ISP funtions. 
c   something to fix.
c
c   remember, phase name included i or e 
c
       old_pic=.false.
       do k=1,nphas
         if((phase(k)(1:5).eq.phs(new_phase)(1:5).and.
     *       phase(k)(2:3).ne.'IA'.and.phase(k)(2:3).ne.'IV').or.                ! compare 5 chars for normal phase
     *      (phase(k)(1:8).eq.phs(new_phase)(1:8).and.   ! compare 7 chars for IASPEI amplitudes
     *      (phase(k)(2:3).eq.'IA'.or.phase(k)(2:3).eq.'IV'))) then    
            old_pic=.true.    
            old_index=k
c
c   keep weight 9, could be that it still is needed. however for long amplitude
c   phase do not blank out weight
c
            if(phase(k)(6:6).ne.'9'.and.phase(k)(2:3).ne.'IA') 
     *      phase(k)(6:6)=' '  ! keep weight 9

            if(phase(k)(7:7).eq.'A') phase(k)(7:7)=' '  ! not automatic now
         endif
       enddo
c                                                                               
c   valid phase, store reading in seconds from time of first sample in trace,   
c   if there was a phase from before, mark it as deleted on plot                
c                                                                               
c-- mark old pic as deleted                   
       if(old_pic) then  		
c-- screen x-pos of old pick  
          picix=(pictim(old_index)-(first-1)/rate-chan_delay)*   !jj
     *    xscale+xpos
c-- indicate deletion of pick    
          call xmovabs(picix,ypos+height-40)     
          call xdrwabs(picix,ypos+height+20)                                    
          text(1:1)='D'                                                         
          call xchars(text,1,picix-5,ypos+height+25)	                           
c
c   also delete amp, per and azim ,09-01jh
c
c          amp=0.0
c          per=0.0
c          azim=0.0
       endif                     
c
c   save new reading in same array element as old if there
c
       if(old_pic) then
          new_index=old_index
          amplitude(new_index)=0.0
          period(new_index)=0.0
          velocity(new_index)=0.0
          azimuth(new_index)=0.0
       else
          nphas=nphas+1
          new_index=nphas
          phase(new_index)=phs(new_phase)
       endif                                               
       pictim(new_index)=(first-1)/rate+(ix-xpos)/xscale+chan_delay    !jj
c
c------------------------------------------------------------------
c   check for  polarity reading indicated by y - cursor position
c--------------------------------------------------------------------

c
c   no polarity for iaspei amplitudes
c
      if(phase(new_index)(2:3).ne.'IA'.
     *and.phase(new_index)(2:3).ne.'IV') then
        phase(new_index)(8:9)='  '
        if(iy.gt.ypos+height*0.85) phase(new_index)(8:9)='C '
        if(iy.lt.ypos+height*0.15) phase(new_index)(8:9)='D '
c
c    do not put polarity on phase AMP
c
        if(phase(new_index)(2:3).eq.'AM') 
     *  phase(new_index)(8:9)='  '
      endif

c
c   blank out residuls and weight used if a new pick
c
        data_end(new_index)(5:14)='          '                                   
c                                                                               
c  if any amplitude or period or azimuth, indicate on plot                                 
c                                                                               
       if(amp.ne.0.0.or.per.ne.0.0.or.azim.ne.0.0) then                        
          call xmovabs(ix,ypos+40)                                              
          call xdrwabs(ix+15,ypos-15)                                           
          call xmovabs(ix,ypos+40)                                              
          call xdrwabs(ix-15,ypos-15)                                           
       endif                                                                    
c                                                                               
c   plot phase with name and polarity                                           
c                                                                               
       call xmovabs(ix,ypos+40)                                                 
       call xdrwabs(ix,ypos+height-40)                                          
c                                                                               
c   tektronics if wanted                                                        
c                                                                               
       call tchars(phase(new_index),9,ix-15,ypos+height-25)	                              
c
c   print amp and period on trace
c
       if(amp.ne.0.0) then
          text=' '
          write(text(1:12),'(f12.1)') amp
          text(14:19)='counts'
          if(disp_vel.eq.1) text(14:22)='nm       '
          if(disp_vel.eq.2) text(14:22)='nm/sec   '
          if(disp_vel.eq.3) text(14:22)='nm/sec**2'
          write(text(24:29),'(f6.2)') per
          text(31:34)='secs'
c
c   plot below if single mode (pmax=1), else at beginning of trace
c
          if(pmax.eq.1) then
             call xmessage(text,1,34,10.0,60.0)
          else
             call xmessage(text,1,34,xpos+10.0,ypos+height/2.0)
          endif
c                                                                               
c-- save last picked amplitude, warn user if not in nm or nm/s if mB or MS
c
          if(disp_vel.eq.0.0) then
      	      text(1:67)=
     *        'Amplitude not in nm, nm/s'//
     *        ' or nm/s*s, sure you want to save (y/n) ?'    

             if(pmax.eq.1) then
	            call oneline(text,67,answer,2,10.0,40.0)
             else
	            call oneline(text,67,answer,2,xpos+15,
     *          ypos+height/2-15.0)
             endif    
cfix             if(sun.or.linux) call xscursr(ich,ix,iy)  ! on sun x-win, to get rid of one char
             iy=ypos  ! make sure no polarity


             if(answer(1:1).eq.'y'.or.answer(1:1).eq.'Y') then
                 amplitude(new_index)=amp				
c-- save last picked period                              
                 period(new_index)=per
             else
                 amplitude(new_index)=0.0
                 period(new_index)=0.0
                 nphas=nphas-1         ! jh oct 4 2012 to avoid storing phase name
             endif
          else 
             amplitude(new_index)=amp				
             period(new_index)=per
c
c   if mB or MS, convert velocity to displacement
c
c            if(do_mbb.or.do_mss) 
c    *          amplitude(new_index)=amplitude(new_index)*
c    *          period(new_index)/6.283
c
c   if mb or Ms, correct for gain
c
             if(do_mb.or.do_ms) then
c                write(6,*) 'npoles',npole,nzero
c                write(6,*) 'norm',norm
c                write(6,*) (pole(k),k=1,npole)
                call pazresp (1.0/period(new_index), norm, nzero, zero,
     *          npole, pole,  hs)
c                write(6,*) 'complex gain', hs
                gain=  cabs(hs)
c
c               write(6,*) 'mb,ms: gain,period',gain, period(new_index)
                amplitude(new_index)=amplitude(new_index)/gain
             endif
c
c  if wa and extra filter, correct for that filter
c
c           write(6,*) pole_low_save,pole_high_save, 
c    *      f_low,f_high
            if(do_wa.and.(
     *      pole_low_save.gt.0.or.pole_high_save.gt.0)) then
               gain=1.0
               if(f_low.gt.0.0.and.pole_low_save.gt.0) then
                  call bworth(1.0/period(new_index),
     *            f_low,-pole_low_save,hs) 
                  gain=gain*cabs(hs)
               endif
               if(f_high.gt.0.and.pole_high_save.gt.0) then
                  call bworth(1.0/period(new_index),
     *            f_high,pole_high_save,hs) 
                  gain=gain*cabs(hs)
               endif
c
c  reset ml filter
c
               pole_low_save=0
               pole_high_save=0

c               write(6,*) 'wa: gain,period',gain, period(new_index)
c                amplitude(new_index)=amplitude(new_index)/gain
             endif
         endif

       endif

c-- save last picked azimuth and velocity
       azimuth(new_index)=azim
       velocity(new_index)=veli
c-- clear amplitude storage                                    
       amp=0.0					
c-- clear period,azimuth and app. velocity storage                                       
       per=0.0				
       azim=0.0
       veli=0.0	
                                                       
c                                                                                
c-- pick next phase or give another command
c
c       if (amp_flag) return
       go to 11				
c                                                                               
c  count phases including coda and save all before returning                                                             
c                                                                               
  99   continue                                                                 
c                                                                               
c   save picks before returning                                                 
c                
       kphas=0                           
       do 20 i=1,nphas                                                           
          if(pictim(i).ne.-100000.0) then !cjh                                            
cold          if(pictim(i).ge.0.0) then                                             
             kphas=kphas+1                                                      
             phase(kphas)=phase(i)                                                
             pictim(kphas)=pictim(i)                                           
             amplitude(kphas)=amplitude(i)                                        
             period(kphas)=period(i)     
             azimuth(kphas)=azimuth(i)
             velocity(kphas)=velocity(i)                                      
             data_end(kphas)=data_end(i)
          endif                                                                 
 20    continue         
       nphas=kphas                                                     
c
c   return here always
c
 999   continue
       amp=0.0   ! do not carrry to next trace if not used
       per=0.0
       azim=0.0
       veli=0.0
       if (remove_response.eq.1) then
         ymag=1. ! reset y magnification
c        write(*,*) ' reset magnification '
       endif
       call xset_color(color_def)
       return                                                                   
       end                                                                      
                                                                                
                                                                                
                                                                                
      subroutine plotw                                                          
c                                                                               
c  plot one window, one trace, all coodinates are in tectronics 4010            
c  points are skipped so only about RESOLU points are plotted                   
c                                                                               
c                                                                               
c-- common block
      implicit none   
      save                                    
      include 'mulplt.inc'
      include 'seiplot.inc'     	
c                                                                               
c                                                                               
c   local variables                                                             
c                                                                               
                                                                                
c-- use for plotting                                            
      real      ix,iy,ixx,delix    
c-- x-distance between plotted points, tec scale           
      real      xstep		
c-- plotting string                                      
      character*80 text		
      integer ntext
c-- dc value                                                      
      real 	dc		
c-- number of secs between axis                             
      integer	delsec		
c-- skip of points for plotting                               
      integer	skip		
c-- skip of points for plotting -1                           
      integer	skip1		
c-- axis tic size                                            
      integer	tsize		
c-- seconds or minute tic counter                                       
      integer	isec,imin,ihour		
c-- number of seconds in plot                                 
      integer	nsec		
c-- number of samples which can be plotted                  
      integer	npoint		
c-- y-axis position coresponding to y=0                            
      real	y0		
c--            
      real      ymx            
      integer method     ! method choise for skipping
c-- counters                                                 
      integer	i,k,nplus,nminus,ktic		
      character*3 time_text
c
                                                                          
c                                                                               
c   set some defaults                                                           
c  

                                                                                     
      y0=ypos+height/2                                                          
c-- maximum number of points which can be plotted  
      npoint=page_time*rate    
      if(npoint.eq.0) npoint=1    ! there should be at least one point
c-- tek units pr second                   
      xscale=(1023-xpos)*rate/npoint	
c
c   spectrogram
c

      if(choice.eq.'SGM') goto 1000  ! for multi trace spectrogram skip trace plot   
c
c      write(6,*) 'page_time, npoint,resolu',page_time,npoint,resolu
      skip=npoint/resolu+1             
      if(skip.gt.50) skip=50                                         
c-- assume plot x axis go from         
      xstep=(1023-xpos)*skip/float(npoint) 
      dc=0.0
c                                                                               
c
c  find dc if not already removed in filter routine
c
      if(dc_chan.eq.0.0) then
           do 1 i=first,last
              dc=dc+y(i)                                                        
 1         continue                                                            
c                                                                               
c   remove dc, find max value          
c                                                                               
           dc=dc/(last-first+1)
c
c   save dc in common
c
           dc_chan=dc
      endif
c
      max=0                                                                     
      do 2 i=first,last
          y(i)=y(i)- dc                                                         
          if(abs(y(i)).gt.max) max=abs(y(i))                                    
 2    continue                                                                  
c
c  patch up the values used when skipping so it looks better, should find
c  at better way in the future. Only fix if skipping is larger than 1. 
c  The idea is to use the larges value in skip interval, however if there
c  has been a zero crossing and the maximum value is the same as the
c  previous plotted one, switch sign.
c  if max small, do no patch up
c
c  an alternative method is simply to average values in skip interval and
c  rescsale. which method has been used depends on value of method.
c
c   method = 1 : zero crossing etc
c   method = 2 : average 
c

c      text='Reading data ...'
c      call xcharswin(text,16,500.,120.)

      method=2
      skip1=skip-1
c      if(skip.gt.9999) then   ! to disable
       if(skip.gt.1.and.(last-first).gt.10.and.max.gt.2) then ! do not do if few points
         if(method.eq.1.and.skip.gt.2) then
         do i=first,last,skip
            ymx=0.0
            nplus=0
            nminus=0
            do k=i,i+skip1
c--            check if any zero crossings
               if(y(k).gt.0.0) nplus=nplus+1
               if(y(k).lt.0.0) nminus=nminus+1
               if(abs(y(k)).gt.abs(ymx)) ymx=y(k)
            enddo
            y(i)=ymx
            if(nplus.ne.0.and.nminus.ne.0.and.i.gt.first+skip)then
               if(sign(1.0,y(i-skip)).eq.sign(1.0,y(i))) then
                  y(i)=-y(i)
               endif 
            endif
         enddo
         endif
c
c   average skipped values
c
        if(method.eq.2) then
        do i=first,last-skip,skip 
           ymx=0.0
           do k=i,i+skip-1
             ymx=ymx+y(k)
           enddo
           y(i)=ymx/skip
        enddo
        ymx=0.0
c
c   scale again, do not use last 3 values, could be too large
c
        do i=first,last-3*skip,skip
          if(abs(y(i)).gt.ymx) ymx=abs(y(i))                                    
        enddo
        ymx=max/ymx
        
        do i=first,last,skip
          y(i)=y(i)*ymx
        enddo
       endif
c
c-- last values could be wrong, so fix it                
c
c
c       write(6,*) last,skip
       y(last)=y(last-3*skip)
       y(last-skip)=y(last)
       y(last-2*skip)=y(last)
      endif

                                                                                
c                                                                               
c   write max value                                                             
c                                                                               
      if(pmax.eq.1) then   
         if(max.ge.10.0) write(text,'(a,f12.1)')'Max amp:',max   
         if(max.lt.10.0) write(text,'(a,g12.3)')'Max amp:',max                                                                                               
         call xchars(text,20,35+xpos,ypos-55)                                   
      endif                                                                     
c                                                                               
c   scale, check not zero. use outside max value (max_all) if supplied. used    
c   with multiple pages to ensure constant scale                                
c--!!!!!!!!!!!                                    
c   ASSUME same dc on all pages 
c
c   fixed scale can also override. if max_count  not zero this is the max used
c   overrides others
c                                                                               
c     if(max.eq.0.0) max=1.0                                                    
      if(max_count.ne.0) max_all=max_count
      if(max_all.ne.0.0) then                                                   
         yscale=(height/2.0)/abs(max_all)
      else                                                                      
         if(max.eq.0.0) then
             yscale=1.0
         else
             yscale=(height/2.0)/abs(max)
         endif
      endif     
c      write(*,*) ' debug ymag = ',ymag
      yscale=yscale*ymag 
c
c   save scaling factor for this channel if in multi trace mode
c   used when picking amplitudes in multi trace mode
c
      if(current_chan.gt.0) yscale_mul(current_chan)=yscale
c
c    enter here if spectrogram
c
 1000 continue

      call xset_color(color_frame)   ! frame color                                                                
c                                                                               
c   draw whole frame                                                            
c                                                                               
      if(pframe.eq.1) then                                                      
         call xmovabs(xpos,ypos)                                                
         call xdrwabs(xpos,ypos+height)                                         
         call xdrwabs(1023.0,ypos+height)                                       
         call xdrwabs(1023.0,ypos)                                              
         call xdrwabs(xpos,ypos)                                                
      endif                                                                     
c                                                                               
c   draw end of frame                                                           
c                                                                               
      if(pframe.eq.2) then                                                      
         call xmovabs(xpos,ypos)                                                
         call xdrwabs(xpos,ypos+height)                                         
         call xmovabs(1023.0,ypos+height)                                       
         call xdrwabs(1023.0,ypos)                                              
      endif                                                                     
c                                                                               
c   draw frame minus bottom line                                                
c                                                                               
      if(pframe.eq.3) then                                                      
         call xmovabs(xpos,ypos)                                                
         call xdrwabs(xpos,ypos+height)                                         
         call xdrwabs(1023.0,ypos+height)                                       
         call xdrwabs(1023.0,ypos)                                              
      endif                                        
                             
c                                                                               
c   draw limits for up and down (polarity) limits if in picking mode            
c                                                                               
      if(ppick.ne.0) then                                                       
         call xmovabs(xpos,ypos+30)                                             
         call xdrwabs(30+xpos,ypos+30)                                          
         call xmovabs(xpos,ypos+height-30)                                      
         call xdrwabs(30+xpos,ypos+height-30)                                   
         call xmovabs(1023.0,ypos+30)                                           
         call xdrwabs(994.0,ypos+30)                                            
         call xmovabs(1023.0,ypos+height-30)                                    
         call xdrwabs(994.0,ypos+height-30)                                     
      endif                                                                     
c                                                                               
c   draw seconds tics and numbers, ticks are either at 1,2,5,                   
c   10 or 20 secs intervals                                                     
c                                                                               
      if(paxis.ge.1) then                                                       
c                                                                               
c   draw axis line if no frame                                                  
c  
         call xset_color(color_frame)
         if(paxis.eq.1.or.paxis.eq.3) then                                      
            call xmovabs(xpos,ypos)                                             
            call xdrwabs(1023.0,ypos)                                           
         endif                                                                  
c                                                                               
c   find distance in secs between tics assuming min 100 tectronics              
c   units between tics                                                          
c                                                                               
c-- ca distance between tics                           
         i=100.0/xscale				
c                                                                               
c  find distance between points to nearest 1,2,5,10 or 20 sec                   
c                                                                               
         if(i.le.1) delsec=1                                                    
         if(i.gt.1) delsec=2                                                    
         if(i.gt.2) delsec=5                                                    
         if(i.gt.5) delsec=10                                                   
         if(i.gt.10) delsec=20                                                  
         if(i.ge.20) delsec=60                                                  
         if(i.ge.60) delsec=120                                                 
         if(i.ge.120) delsec=300                                                
         if(i.ge.300) delsec=600                                                
         if(i.ge.600) delsec=1200                                             
         if(i.ge.1200) delsec=3600
         if(i.ge.3600) delsec=7200
                                                  
         call xset_color(color_axis_not)     !JAB(BGS)Mar95., from yellow

c-- number of secs of axis                            
         nsec=npoint/rate			
c-- first whole sec or min on axis                         
         isec=fsec/delsec+1			
         imin=fmin/(delsec/60.0)+1
         ihour=fhour/(delsec/3600.0)+1
c-- time of first tic                                 
         isec=isec*delsec			
         imin=imin*(delsec/60.0)
         ihour=ihour*(delsec/3600.0)
c-- x position first tic                     
         if(delsec.lt.60) ix=(isec-fsec)*xscale+xpos		
         if(delsec.ge.60) ix=(imin-fmin)*60*xscale+xpos		
         if(delsec.ge.3600) ix=(ihour-fhour)*3690*xscale+xpos
c-- find second tic value below 60                   
         isec=mod(isec,60)			
         imin=mod(imin,60)
         ihour=mod(ihour,24)
         time_text='SEC'
         if(delsec.ge.60) then
            time_text='MIN'
         endif
         if(delsec.ge.3600) then
            time_text='HR '
         endif
         if(paxisnumb.eq.1) then
            call tchars(time_text,3,950.0,ypos+5)                              
            call pchars(time_text,3,950.0,ypos+5)                              
         endif
c                                                                               
c   set symbol size, only for postscript, now only option, gfortran pc                                        
c                                                                               
         if(paxisnumb.eq.1) call fontsize(0,2.5)                
c                                                                               
         do 5 i=1,nsec/delsec+1                                                 
c            if(ix.gt.1010) goto 5                                               
c-- check if plot numbers                    
            if(paxisnumb.eq.1) then		
            	! NEATEN time axis
            	ntext=2
            	if (delsec.lt.60) then
            		if (isec.eq.0.or.i.eq.1) then
            			if (isec.lt.10) then
            				write(text,'(i2,a,i1)') imin,'m0',isec
            			else
            				write(text,'(i2,a,i2)') imin,'m',isec
            			endif
            			ntext=5
            		else
            			write(text,'(i2)') isec
            		endif
            	elseif (delsec.lt.3600) then
					if (imin.eq.0.or.i.eq.1) then
            			if (imin.lt.10) then
            				write(text,'(i2,a,i1)') ihour,'h0',imin
            			else
            				write(text,'(i2,a,i2)') ihour,'h',imin
            			endif
						ntext=5
					else
						write(text,'(i2)') imin
					endif
            	else
					write(text,'(i2)') ihour
				endif
               call tchars(text,ntext,ix-10.0,ypos-30)                  
               call pchars(text,ntext,ix-10.0,ypos-15)                         
            endif                                                               
c                                                                               
c-- determine tic size                               
            tsize=10			      
            if(delsec.lt.60.and.(isec.eq.60.or.isec.eq.0).or.
     *         delsec.ge.60.and.(imin.eq.60.or.imin.eq.0)) then            
              tsize=20                                                          
c-- reset seconds  or minute counter            
c              isec=0                  	! WCC takes care of this below        
c              imin=0
            endif                                                               
c-- plot marks  down                
            if(paxis.eq.1.or.paxis.eq.3) then 
               call xmovabs(ix,ypos)                                            
               call xdrwabs(ix,ypos+tsize)
               if(delsec.le.1) then   ! mark 0.1 sec
                  delix=(xscale*delsec)/10.0    ! 0.1 sec distance
                  do k=1,9
                     ixx=ix+delix*k
                     call xmovabs(ixx,ypos)                                     
                     call xdrwabs(ixx,ypos+5.0)
                  enddo
                  if(i.eq.1) then
                     ixx=ix
                      do k=1,10
                        ixx=ix-delix*k
                        if(ixx.lt.xpos) goto 3747
                        call xmovabs(ixx,ypos)                                     
                        call xdrwabs(ixx,ypos+5.0)
                     enddo
 3747                continue
                  endif
               endif
            endif                                                               
c-- draw ticks up                   
            if(paxis.eq.2.or.paxis.eq.3) then 
               call xmovabs(ix,ypos+height)                                     
               call xdrwabs(ix,ypos+height-tsize)
               if(delsec.le.1) then   ! mark 0.1 sec
                  delix=(xscale*delsec)/10.0    ! 0.1 sec distance
                  do k=1,9
                     ixx=ix+delix*k
                     call xmovabs(ixx,ypos+height)                                     
                     call xdrwabs(ixx,ypos+height-5.0)
                  enddo
                  if(i.eq.1) then
                     ixx=ix
                     k=1
                     do k=1,10
                        ixx=ix-delix*k
                        if(ixx.lt.xpos) goto 3748
                        call xmovabs(ixx,ypos+height)                                     
                        call xdrwabs(ixx,ypos+height-5.0)
                     enddo
 3748                continue
                  endif
               endif
            endif                                                               
            isec=isec+delsec 
            ! NEATEN time axis
            if (isec.ge.60) imin=imin+isec/60
            if (imin.ge.60) ihour=ihour+imin/60
			if (isec.ge.60) isec=isec-60*(isec/60)
c            write(*,*) ihour,imin,isec,delsec,delsec/60	! DEBUG WCC
			if (imin.ge.60) imin=imin-60*(imin/60)
			if (ihour.ge.24) ihour=ihour-24*(ihour/24)
           ix=ix+xscale*delsec                                                
 5       continue                                                               
      endif
c
c  spectrogram, then skip plotting traces
c                                                          
      if(choice.eq.'SGM') then
         call spectrogram(i)
c
c   plot nyquist frequency
c
         i=rate/2.0
         write(text,'(i3)')i  

         call xset_color(xred)                                                                                           
         call tchars(text,3,xpos,ypos+height-25.0)
         call xset_color(xblack)    
         return    ! no trace plot
      endif                                                                                
                                                                                
c                                                                               
c   plot data                                                                   
c
      call xset_color(color_trace)                                                                               
      ix=xpos+trace_delay*xscale                                                
c      write(*,*) ' xxx ',ix,xpos,trace_delay
      iy=y(first)*yscale*over_scale+y0                                          
      if(iy.gt.780) iy=780                                                      
      if(iy.lt.0) iy=0                                                          
      if(ix.gt.1023) ix=1023                                                    
      if(ix.lt.1) ix=1                                                          
      y(1)=iy                                                                  
      k=1                                                                       
      do 10 i=first+skip,last,skip                                              
         k=k+1                                                                  
         if (i.gt.0) then ! added 28/11/2013 lo
           iy=y(i)*yscale*over_scale+y0 
         else
           iy=0
         endif
         if(iy.lt.1) iy=1                                                       
         if(iy.gt.780) iy=780                                                   
         y(k)=iy                                                               
 10   continue
c
c   do not use last 2 values, could be very large
c

c     y(k)=y0
c     y(k-1)=y0
c
c   if skip, do not use last 3 values
c
      if(skip.gt.1) k=k-3   ! jh may 4 2004
      call xdrwvec(k,ix,y,xstep)
c                          
c   indicate zero level
c     
      dc=-dc*yscale*over_scale+y0
      if(dc.gt.750) dc=750.0
      if(dc.lt.0) dc=1.0
      call xset_color(color_frame)           !JAB(BGS)Mar95, from blue.
      call xmovabs(xpos,dc)
      call xdrwabs(50.0+xpos,dc)                                                
      call xset_color(color_def)
c
c   indicate if a timing uncertainty
c
      
      if(wav_time_error(wav_current_chan(1))(1:1).eq.'E') then
         text="UNCERTAIN TIME"
         call tchars(text,14,xpos+10.0,y0+10.0)                    
         call fontsize(0,2.0)                                                  
         call pchars(text,14,xpos+10.0,y0+10.0)
         call fontsize(0,2.5)
      endif                              

      return                                                                    
      end                                                                       
c                                                                               
c###########################################################################    
                                                                                
      subroutine trans_filt(icomp)                                                     
c                                                                               
c  Copy of trace data from external array SIGNAL to internal                    
c  array Y. If FILT is not zero, data is also bandpass filterted.               
c  Filter constants are set in this routine for fixed filters.
c  Response removal is also done here.
c
c     icomp is component 1, 2 or 3
c               
      implicit none    
      save                                                            
c                                                                               
c   include common block                                                        
c                                                                               
      include 'mulplt.inc'
      include 'seiplot.inc'
      include 'seisan.inc'
c                                                                               
c   local variables                                                             
c                                                                               
c-- for plotting text                                    
      character*80 text
c--messages
      character*80 message(3)
c-- filter coefficients                                       
      real 	cof(8)		
c-- filter gain                                                  
      real	gain		
c-- filter type, hp or lo or bp used in recfil routine
      character*8 filt_name
      real xx,yy        ! help varibale
      character*1 ch    ! answer, one char
      integer pole_low,pole_high       ! single filter constants
      real f_high,f_low                ! -----------------------
      integer pole_low_save,pole_high_save ! for ml correction
      integer nsamp                    ! number of samples to use
c
c   poles and zeroes to add to resposnse
c
      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros
     
      common /trans_filt_com/pole,zero,npole,nzero,norm,pole_low,
     *pole_high,f_low,f_high,pole_low_save,pole_high_save

c-- dc
      real dc
c-- counter       mp_text(3)                                               
      integer   i,j
      integer icomp


      dc_chan=0.0
      npole=0
      nzero=0
      norm=0.0                                                                               
c                                                                               
c   transfer data                                                               
c     
      if(icomp.eq.1) then                                                                               
         do i=1,numb_samp                                                          
            y(i)=signal1(i)                                                         
         enddo     
      endif                                                                
      if(icomp.eq.2) then                                                                               
         do i=1,numb_samp                                                          
            y(i)=signal2(i)                                                         
         enddo     
      endif                                                                
      if(icomp.eq.3) then                                                                               
         do i=1,numb_samp                                                          
            y(i)=signal3(i)                                                         
         enddo     
      endif   
  

c
c   if extra filter for wa, put in here, was a  freq. dom filter before
c                                                     
            if((ml_low.gt.0.or.ml_high.gt.0).and.do_wa) then

c   check, poles mut be the same
c
                if (ml_low_pole.ne.ml_high_pole) then
                   if(ml_low_pole.lt.ml_high_pole) then
                      ml_high_pole=ml_low_pole
                   else 
                      ml_low_pole=ml_high_pole
                   endif
                endif
c               write(6,*) 'Use filt ml'
                filt=9
                flow(9)=ml_low
                flow_pole(9)=ml_low_pole 
                fhigh(9)=ml_high
                fhigh_pole(9)=ml_high_pole 
             endif
c
c   set up filter and velocity response for mB 
c
          if(do_mbb) then
             remove_response=1
             disp_vel=2
             filt=9
             flow(9)=0.0333   ! 30 sec
             flow_pole(9)=4   
             fhigh(9)=5.0
             fhigh_pole(9)=4   
          endif
c
c   set up filter and velocity response for MS 
c
          if(do_mss) then
              disp_vel=2
              filt=9
              flow(9)=0.0167
              flow_pole(9)=4  
              fhigh(9)= 0.33
              fhigh_pole(9)=4 
           endif
c
c  make sure both filters are not zero
c
c      write(*,*) ' debug icomp,filt ',icomp,filt
      if(filt.gt.0) then
         if(flow(filt).eq.0.0.and.fhigh(filt).eq.0.0) then
            filt=0   ! disable filter
         endif 
      endif
c
c  if filter, remove response or spectrum, remove dc and taper
c
      if(filt.ne.0.or.remove_response.gt.0.or.do_spectrum) then
        dc=0.0
        do i=1,numb_samp
          dc=dc+y(i)
        enddo
        if(numb_samp.ne.0) then
          dc=dc/numb_samp
        else
          dc=0.0
        endif
        do i=1,numb_samp
           y(i)=y(i)-dc
        enddo
c
c  taper signal, lot 15/08/08
c
        call applytaper(y,numb_samp,10.)   ! apply taper, width 10% of half the samples
c
c  save dc
c 
        dc_chan=dc
      endif     
c
c--------------------------
c   filter
c--------------------------
c 
      if(filt.ne.0) then
  
c      write(6,*) 'transfilt',
c     *flow(filt),fhigh(filt),flow_pole(filt),fhigh_pole(filt)
c
c   check if filtering possible
c
c   the high corner
c
         if(plotoption.ne.2) then   ! lot 30/09/2011
            if(fhigh(filt).ge.rate/2.0) then
               text=' '
               write(text,'(a5,1x,a4,1x,a,f5.1,a,f5.1,a)') 
     *         wav_stat(wav_current_chan(1)),
     *         wav_comp(wav_current_chan(1)),
     *         'S. rate is: ',rate,' Filter is: ',fhigh(filt),
     *         ' cannot filter'
               call tchars(text,60,200.0,ypos+5.0)
               return   ! no filtering done
            endif
         endif
c
c   the low corner if a hp filter
c
         
         if(plotoption.ne.2) then   ! lot 30/09/2011
            if(flow(filt).ge.rate/2.0.and.fhigh(filt).eq.0.0) then
               text=' '
               write(text,'(a5,1x,a4,1x,a,f5.1,a,f5.1,a)') 
     *         wav_stat(wav_current_chan(1)),
     *         wav_comp(wav_current_chan(1)),
     *         'S. rate is: ',rate,' Filter is: ',flow(filt),
     *         ' cannot filter'
               call tchars(text,60,200.0,ypos+5.0)
               return   ! no filtering done
            endif
         endif
c
c   check that not too low frequency for band reject
c
         if(plotoption.ne.2) then   ! lot 30/09/2011
            if(flow(filt).lt.0.0.and.abs(flow(filt)).lt.0.5) then
               text=' '
               write(text,'(a)')'Cannot use BR filter below 0.5 Hz' 
               call tchars(text,33,200.0,ypos+5.0)
               return   ! no filtering done
            endif
         endif
c
c
c   low and high pass filters, set up filters  
c   jh jan 2012 only use recfil for bp and br for frequencies above 0.5, 
c   band pass filters below 0.5 still use bndpas since recfil is unstable 
c   
         if(npasses.eq.0) npasses=1   ! make sure defined
         recfil_pole=4        ! the default
         filt_name='BP      ' ! default a bp filter
c
c  if a bp or br filter, both poles must be the same
c
         if(flow(filt).ne.0.0.and.fhigh(filt).ne.0.0) then
            if(flow_pole(filt).ne.0.and.fhigh_pole(filt).ne.0)then
               recfil_pole=flow_pole(filt)
            endif
         endif

         if(flow(filt).eq.0.0) then
            filt_name='LP      '
            if(fhigh_pole(filt).ne.0) recfil_pole=fhigh_pole(filt) ! it could be different from 4
         endif

         if(fhigh(filt).eq.0.0) then
            filt_name='HP      '
            if(flow_pole(filt).ne.0) recfil_pole=flow_pole(filt) ! it could be different from 4
         endif
      
         if(flow(filt).lt.0.0.and.fhigh(filt).ne.0.0) then
            filt_name='BR      '                 
            recfil_pole=flow_pole(filt)
            write(6,'(a,2f8.3)') 'Band reject ',flow(filt), fhigh(filt)
         endif
c         write(6,*) 'recfil_pole, npas',recfil_pole,npasses

         if((filt_name.eq.'BR      '.or.
     *   filt_name.eq.'BP      ').and.flow(filt).le.0.5.and.
     *   flow(filt).gt.0.0) then
c
c         write(6,*)' uses bndpas',npasses
c
c   use bndpas if low frequency and a bp filter
c        
            recfil_pole=4
            flow_pole(filt)=4
            fhigh_pole(filt)=4
            call bndpas(flow(filt),fhigh(filt),1000.0/rate,  
     *      cof,gain)                                                   
c-- apply filter
            call filter(y,numb_samp,cof,gain,npasses) 
         else
c
c   use recfil
c
c            write(6,*) 'recfil',
c     *      flow(filt),fhigh(filt),recfil_pole,npasses
            if(filt_name.eq.'BR      ') then
               call recfil(y,numb_samp,y,'BU      ',0.0,0.0,recfil_pole,
     *         filt_name,-flow(filt),fhigh(filt),1.0/rate,npasses)
            else
               call recfil(y,numb_samp,y,'BU      ',0.0,0.0,recfil_pole,
     *         filt_name,flow(filt),fhigh(filt),1.0/rate,npasses)
            endif
         endif        
         filt_name='BP      '  ! defualt again not sure why
c                                                                               
c   plot filter constants used if in pic mode                                   
c                                                                               
         if(ppick.eq.1.or.do_3comp) then                                                    
            write(text,200) flow(filt),fhigh(filt),recfil_pole,npasses                              
 200        format('Filter: ',f6.3,1x,f6.3,i3,i2)                                     
c
c   on top plot, plot just below trace
c
            if(ypos.gt.300) call xchars(text,26,350+xpos,ypos-55)              
c
c  on lower plot, plot in lower left hand corner
c
            if(ypos.lt.299) call xchars(text,26,600.0,5.0)
        endif     ! end of filter section
c
c  now there is a new dc
c    

         dc=0.0
         do i=1,numb_samp
           dc=dc+y(i)
         enddo
        if(numb_samp.ne.0) then
           dc=dc/numb_samp
         else
          dc=0.0
         endif
          do i=1,numb_samp
           y(i)=y(i)-dc
        enddo
c
c  save dc
         dc_chan=dc                                 
      endif           ! end of filter section

c
c   save filter index
c
      filt_old=filt                                                                     
c
c----------------------------------------------------------------------------
c--------- call remove response if flag is set to 1                             
c----------------------------------------------------------------------------
c
      if (remove_response .eq. 1) then

c
c   first check which kind of response if not wa or N or E in 3comp analysis
c   if in multi trace mode, only ask the first time
c   do not ask if waveform mt write out 
c
         if((.not.do_mb).and.(.not.do_wa).and.(.not.do_ms).and.
     *       (.not.do_3comp).and.(.not.do_mbb).and.(.not.do_mss).
     *       and.(current_seq_chan.le.1).and..not.wave_out.and..not.
     *       mt_out) then
             disp_vel=0
             text(1:45)='Displacement: d, Velocity: v, Acceleration: a'
             call xset_color(color_prompt)        !JAB(BGS)Apr95.
             call tchars(text,45,10.0,365.0)
             call xset_color(color_title)         !JAB(BGS)Apr95.
c
c   get answer
c
 22          continue
             call xscursr(i,xx,yy)
             ch=char(i)
             if(ch.eq.'A'.or.ch.eq.'a') disp_vel=3
             if(ch.eq.'V'.or.ch.eq.'v') disp_vel=2
             if(ch.eq.'D'.or.ch.eq.'d') disp_vel=1
             if(disp_vel.eq.0) then
                goto 22             ! no good answer
             endif
         endif
c
c   first check if response info available and from where
c
          wav_resp_file = ' '
          call read_resp

          if(wav_resp_status(1:1).eq.'9') then
              write(text(1:31),'(a,1x,a5,1x,a4)')
     *        'No response info for',
     *        wav_stat(wav_current_chan(1)),
     *        wav_comp(wav_current_chan(1))
              call tchars(text,31,600.0,ypos)
              return
          endif
          if(wav_resp_status(1:1).eq.'8') then
             write(text(1:33),'(a)')'Response from waveform header ***'
             call tchars(text,33,600.0,320.0)
          endif

c
c   set displacement response for wa
c
          if(do_wa) then
             remove_response=1
             disp_vel=1

c               write(6,*) 'Use paz ml'
c
c  use poles and zeros
c
                 npole=2
                 nzero=2
                 norm=1.0
                 pole(1)=(-6.283,-4.712)
                 pole(2)=(-6.283, 4.712)
                 zero(1)=(0.0,0.0)
                 zero(2)=(0.0,0.0)
c            endif
          endif
c
c   set up displacement response for mb
c
          if(do_mb) then
             remove_response=1
             disp_vel=1

c
c   use poles and zeros
c
c                write(6,*) 'in mb paz'
                npole=5
                nzero=3
                norm=384.0
                pole(1)=(-3.725, 6.22)
                pole(2)=(-3.725,-6.22)
                pole(3)=(-5.612, 0.0 )
                pole(4)=(-13.24, 0.0 )
                pole(5)=(-21.08, 0.0 )
                zero(1)=(0.0,0.0)
                zero(2)=(0.0,0.0)
                zero(3)=(0.0,0.0)
c             endif
          endif
c         write(6,*) 'paz in mul first',nzero,npole 

c
c   set up filter response for Ms 
c
          if(do_ms) then
             remove_response=1
              disp_vel=1
c
c   use poles and zeros
c
                npole=4
                nzero=3
                norm=0.83
                pole(1)=(-0.40180, 0.0859)
                pole(2)=(-0.40180,-0.0859)
                pole(3)=(-0.04841, 0.0 )
                pole(4)=(-0.08816, 0.0 )
                pole(5)=(-21.08, 0.0 )
                zero(1)=(0.0,0.0)
                zero(2)=(0.0,0.0)
                zero(3)=(0.0,0.0)
c             endif
           endif

c
c  transfer filter variables to variables used by remove response
c  filt is index of fixed filter constants, index 9 for a variable filter
c  currently (jan 2012) no filtereing in frequecy domain, so eventually this has to go
c
c   addtional filtering is only allowed in combination with ml
c   however mss and mbb uses filters, now done in time domain
c
c           write(6,*) 'filt in rem_resp',filt
           if(filt.gt.0.and.(.not.do_mb).
     *        and.(.not.do_ms)) then
              f_low=flow(filt)
              f_high=fhigh(filt)
              pole_low=flow_pole(filt)
              pole_high=fhigh_pole(filt)
c
c  the standard time domain filters are 4 poles and no poles are normally
c  given for corresponding filters when used with response removal. so here 
c  the number of poles is set to 4 if not defined
c
c  since only time domain used from jan 2012, all poles are by default the same
c  if a bp filter, but can be different for a lp and a hp. nuber of poles can be 
c  set interctively
c  
c
              if(pole_low.lt.1.or.pole_low.gt.8) pole_low=4  ! make sure def.
              if(pole_high.lt.1.or.pole_high.gt.8) pole_high=4
c
c  same for possible gain correction for ml
c
              pole_high_save=pole_high
              pole_low_save=pole_low
           endif
c           write(6,*)'flow,fhigh,poles', f_low,f_high,pole_low,pole_high
c                                                                               
c--------put part of the signal into local datavector                           
c                                                                               
           j =  0                                                                    
           do i = first,last                                                         
              j = j + 1                                                               
              y(j)=y(i)
           enddo
           nsamp=j
c
c   check if enough points
c
           if((nsamp).lt.20) then
              text=' Window too short for transformation, nothing done'
              call xmessage(text,1,80,10.0,60.0)
              return
           endif

c
c   check for max number of points
c
           if(nsamp.ge.max_sample/2) then
               call clear_to_alpha
               write(6,*)' Too many points in instrument removal'
               write(6,*)' Max number is ', max_sample/2
               stop
           endif

c        
c           write(6,*) 'paz in mul',nzero,npole                                                                       
c          write(6,*) 'filter before rem resp',f_low,f_high,pole_low,
c     *     pole_high  

c
           call remove_resp(y,com,nsamp,rate,disp_vel,
     *     f_low,f_high,pole_low,pole_high,zero,pole,nzero,npole,norm)
c
c  make sure, if a filter has been used, that it is reset to next time
c
c          filt=0    ! now done in spectral routine in sig_spec

c
c   put data in where it came from in vector in order to plot correctly
c
           do i=nsamp,1,-1                                                               
              y(first-1+i) = y(i)                                   
           enddo                                      

c
c   write text
c
           if(disp_vel.eq.3) write(text,'(a)')'Acceleration'
           if(disp_vel.eq.2) write(text,'(a)')'Velocity     '
           if(disp_vel.eq.1) write(text,'(a)')'Displacement '
           if(do_wa) write(text,'(a)')        'Wood Anderson'
           if(do_mb) write(text,'(a)')        'mb response  '
           if(do_ms) write(text,'(a)')        'ms response  '
           if(do_mbb) write(text,'(a)')        'mB response  '
           if(do_mss) write(text,'(a)')        'mS response  '
           call tchars(text,13,430.0,1.0)
      endif
c
c------------------------------------------------------------------------
c  also save in work_array
c	                                                            
      if(icomp.eq.1) then                                                                               
         do i=1,numb_samp                                                          
            wav_y1(i)=y(i)                                                         
         enddo     
      endif                                                                
      if(icomp.eq.2) then                                                                               
         do i=1,numb_samp                                                          
            wav_y2(i)=y(i)                                                         
         enddo     
      endif                                                                
      if(icomp.eq.3) then                                                                               
         do i=1,numb_samp                                                          
            wav_y3(i)=y(i)                                                         
         enddo     
      endif            
c
c
c   save all filter values for a possible wave output
c      
c      filt_old=filt
c
c      write(6,*)'filt_old',filt_old


      if(filt_old.gt.0) then               ! check on bound, jh  jun 2011
c      write(6,*)flow(filt_old),fhigh(filt_old)
         filt_old_low=flow(filt_old)
         filt_old_high=fhigh(filt_old)
         filt_old_low_pole=flow_pole(filt_old)
         filt_old_high_pole=fhigh_pole(filt_old)
         npasses_old=npasses
      endif
      disp_vel_old=disp_vel
      remove_response_old=remove_response
      do_wa_old=do_wa
      do_mb_old=do_mb
      do_mbb_old=do_mbb
      do_ms_old=do_ms
      do_mss_old=do_mss  
      ml_low_old=ml_low
      ml_high_old=ml_high                                                  

      return                                                                    
      end                                                                       
c---------------------------------------------------------------------------------------


                                                                                
      subroutine traceplot                                                      
     *(nchan,time_scale,start_time,stop_time,delay,pheader)           
c                                                                               
c                                                                               
c     j havskov jan 91                                                          
c                                                                               
c     latest update:                                                            
c     mar  19 by j.h. prepare for pc, one channle read in this routine          
CJAB(BGS)Mar95       : Incorrect logic in IF statement.
CJAB(BGS)Mar95       : Decouple header*80 with argument.
c                                                                               
c   Plots nchan channels of data x.                                             
c   delay in individual channel delay relative to main header.                  
c   Header header is plotted                                                    
c   above plot                     
c   Time_scale is hardcopy scale in cm/sec, if zero auto x-scale                
c   on one page.                                                                
c--!!                    
c   The hardcopy time scale is device dependent, so check
c--!!              
c   If time_scale is not 0.0, more than one page may be plotted
c   The routine can also plot phase pics if present. These are inputted         
c   through array data.                 
c                                                                               
c   input:                                 
c           start_time:	   Start time of plot relative main header              
c           stop_time:     Stop time ----------------------------               
c           nchan:         Number of channels to plot                           
c           channelname    Channel numbers to plot,now in common                              
c           time_scale:    Time scale in cm/sec (laser), 0.0 all on one page    
c           delay:         Delay (sec) each channel relative to time            
c           header:        Plot header                                          
c           data:          One event nordic format                              
c                                                                               
c    note; duration of each channel and sample rate is obtained directly        
c          when readinf the channel data from file with seisin                  
c                                                                               
c   output: The plot                                                            
c   
       implicit none    
       save          
       include 'mulplt.inc'                                                     
       include 'libsei.inc'
       include 'seiplot.inc'
       external   seiclen                       !JAB(BGS)Mar95. String length.
       integer    seiclen,                      !JAB(BGS)Mar95. Function.
     &            ix                            !JAB(BGS)Mar95. Local variable.
       character  pheader  *(*)                 !JAB(BGS)Mar95. Argument.
c                                                                               
c   local variables                                                             
c                                                                               
c
c-- number of channels                                      
       integer nchan		
c-- channel numbers to plot                       
c      integer channelname(*)  
c-- start time of page relative to main header            
c      real page_start		
c-- stop -------------------------------------             
       real page_stop	
c-- see above                                             
       real start_time		
c-- ---------                                     
       real stop_time           
c-- time scale, see above                                 
       real time_scale		
c-- individual channel delay (sec)                         
       real delay(max_trace)		
       real delay_used
c-- channels start times and durations
       real cstart(max_trace),cinter(max_trace)
c-- station and component 
       character*5 stat(max_trace)
       character*4 comp(max_trace)
c-- header        increased dimension to 90 (bjb 22/02/2001) 
       character*90 header	
       integer year,month,day,hr,min,year1,month1,day1,hr1,min1             
c-- seconds, like fsec defined in common block                 
       real sec1 		
c      integer w_unit_out            ! wave unit for output
c-- for time conversion routines             
       double precision secdif	
c-- text string for plotting                             
       character*80 text	
       character*80 label
c       integer on(50)  ! if menu boxes on or off
c-- dc                                                           
       real dc			
c-- maximum y converted to integer                
       integer imax
c-- plot page counter                                       
       integer ipage		
       integer file_no   ! file number in filenr.lis
       real edist,hdist   ! epicentral and hypocentral distance
c-- counters                                       
       integer i,ich,jc,doy,k,m
c
c-- for mt
c
       real mt_rate
       integer skip  ! skip for resampling
       real xskip
       character*5   mt_stat
       character*4   mt_comp
       integer mt_nsamp
       real shift

       character*4 operator
c                                                                               
c       include 'seiplot.inc'
       common/filenumberlis/file_no
c
c     initialise...
c     =============
c
       header = pheader                     ! Working header.
c                                                                               
c  height of each channel in tek units                                          
c                                                                               
c       height=685.0/nchan                                                       
       height=(max_ypos-30)/nchan    

       if (forward) then
c         write(*,*) ' xxx forward '
         shift=(stop_time-start_time)/5.
         start_time=start_time+shift
         stop_time=stop_time+shift
         forward=.false.
       endif

       if (backward) then
c         write(*,*) ' xxx backward '
         shift=(stop_time-start_time)/5.
         start_time=start_time-shift
         stop_time=stop_time-shift
         backward=.false.
       endif


c
c   if overlay channels, do not count them when calculating height
c
       k=0
       do i=1,nchan
          if(wav_overlay(i)) k=k+1
       enddo      
       height=(max_ypos-30)/(nchan-k)      

c
c  if only 2 channels, make it larger
c
       if(nchan.lt.2) height=(max_ypos-30)/2.0
       if(nchan.eq.3.and.do_pmp) height=(max_ypos-30)/2.0/3.
       over_scale=1.0 
c       if(nchan.lt.4) over_scale=1.0
c                                                                               
c  set plot defaults and initial parameters, see common block for explanation   
c                                                                               
       pframe=3                                                                 
       paxisnumb=0                                                              
       paxis=2                                                                  
       pmax=2                                                                   
       ppick=0                                                                  
       xpos=120                                                                 
       ypos=max_ypos                                                                 
c       ypos=665                                                                 
       first=1                                                                  
c       over_scale=1.1                                                           
c                                                                               
c   check if just one page and set constants                                    
c                                                                               
       if(time_scale.eq.0.0) then                                               
          page_time=stop_time-start_time                                        
       else                                                                     
c-- x axis is 22.85 cm long, tektronics       
          page_time=22.85/time_scale	
c-- post script              
cc          if(hctype.eq.1) page_time=17.55/time_scale	
            page_time=17.55/time_scale  ! gfortran
       endif                                                                    
c                                                                               
c   add start_time to main header time                              
c                                                                               

       secdif=start_time+wav_abs_time(wav_first)                                                        
       call sectim(secdif,year,doy,month,day,hr,min,sec1)                                    
       year1=year                                                               
       month1=month                                                             
       day1=day                                                                 
       hr1=hr                                                                   
       min1=min                                                                 
       fsec=sec1                                                            
       fmin=min+sec1/60.0
       fhour=hr+fmin/60.0
       if (remove_response.ne.0) ymag=1.
c
c   save in common block these start values
c
       ipage=1                                                                  
c                                                                               
c----------------------------------------------------                           
c    main loop for pages, back here for new page                                
c----------------------------------------------------                           
c                                                                               
 1     continue                                                                 
       if(plotoption.eq.2) write(6,*)' Page',ipage                              
c                                                                               
c   plot header                         
c                                                                      
       ix = seiclen(header) + 2                        !JAB(BGS)Mar95. Next char
       ix=60
       if(ipage.gt.1) write(header(ix:ix+3),235) ipage !JAB(BGS)Mar95.
 235   format('p ',i2)
cc-- standard font and size                           
       call fontsize(0,0.0)		
       call xset_color( color_title )                 !JAB(BGS)Jan95.
       call xchars(header,seiclen(header),xpos,755.0) !JAB(BGS)Mar95.
c
c   plot event number 
c
       text=' '
       if(opmode.eq.1) event_no=file_no  
       if(opmode.eq.0.or.opmode.eq.1) then
         write(text,'(a,i5)') 'Event #',event_no
         call tchars(text,12,1.0,1.0)
       endif
c
c   plot screen number
c
        if(total_n_screen.gt.1) then
           text=' '
           write(text(1:8),'(a,i2,a,i2)')'W',n_screen,' of',
     *     total_n_screen           
c           do i=1,7
c             if(text(i:i).eq.' ') text(i:i)='-'
c           enddo
            call tchars(text,13,4.0,max_ypos+10.0)
        endif
c
c   plot header from S-file
c
       text=' '
       text=data(1)(2:79)
       do i=1,nhead    ! lot 07052003
         if (data(i)(2:10).eq.'VOLC MAIN') then
           write(text(23:23),'(a1)') data(i)(12:12)
         endif
       enddo
c       call xchars(data(1)(2:79),78,xpos,max_ypos+8.0)
c       call xchars(text(1:78),78,xpos,max_ypos+8.0)
       call xchars(text(1:78),78,xpos,max_ypos+20.0)	! WCC moved up, jh a bit more
c
c   plot last operator id from S-file
c
       if(mulplt_multi_label.gt.0.5)then
         call get_env_op(operator)
c        write(6,*) 'Operator:',operator
         label=' '
         label(1:4)='OP: '
         write(label(5:8),'(a4)') operator(1:4)
         label(9:22)=' Last ACTION: '
         label(26:30)=' by: '
         do i=1,nhead
           if (data(i)(2:8).eq.'ACTION:') then
             write(label(23:25),'(a3)') data(i)(9:11)
             write(label(31:34),'(a4)') data(i)(31:34)
           endif
         enddo
         call xset_color(xred)
         call xchars(label(1:34),34,xpos+650.0,max_ypos+20.0)
         call xset_color(color_def)
       endif
c
c   if both screen and hardcopy, write text only on screen by
c   temporaraly turning hc option off
c
       if(plotoption.gt.0) then
          call xchars(header,seiclen(header),xpos,755.0)    !JAB(BGS)Mar95.
          if(plotoption.eq.1) then
             if(ipage.eq.1)  header(ix:) = 'Help ?'         !JAB(BGS)Mar95
             plotoption=0
             call xchars(header,seiclen(header),xpos,755.0) !JAB(BGS)Mar95.
             plotoption=1
          endif
       endif
c                                                                               
c   plot start time                                                             
c                                                                               
       write(text,'(a17,i4,2x,i2,1x,i2,3x,i2,a1,i2,2x,f6.3)')                   
     *'Plot start time:  ',year,month,day,hr,':',min,fsec
       call xchars(text,64,xpos,max_ypos+3.0)
c                                                                               
c   filter info   moved down jh jan 2012                                                              
c                                                                               
c       if(filt.ne.0) then
c          text=' '                                                       
c          write(text(1:22),201) flow(filt),fhigh(filt),recfil_pole                         
c 201      format('Filt: ',f6.3,1x,f6.3,i3)                                                                                                                                                   
c          call xset_color(xred)      
c          call xchars(text,22,xpos+600.0,max_ypos+3.0) 	! WCC moved down, JH LEFT
c          call xset_color(color_def)
c       endif                                      
c

c   put menu box if in graphics screen mode, set input mouse postion outsde
c   range so no choise is made (0.0,999.0)
c
       if(plotoption.lt.2)
     * call pick_menu_box(onbox,14,63.0,16.0,'multip',
     *    3.0,758.0,0.0,999.0,mouse,mouse,show_menu,flow,fhigh)

c
c   write header for mt file
c
       if(mt_out) then

          text=' '
c          write(6,*) remove_response,disp_vel
          if(disp_vel_old.eq.1) text(1:12)='displacement'
          if(disp_vel_old.eq.2) text(1:12)='velocity    '
          if(disp_vel_old.eq.3) text(1:12)='acceleration'
c
c   open mulplt.wav, check first if input is not same file
c
          if(wav_filename(1)(1:10).eq.'mulplt.wav') then
             write(6,'(a)')'Cannot use mulplt.wav for output'
             write(6,'(a)')'File is input'
             mt_out=.false.
             text(1:22)='mulplt.wav is input !!'
             call xset_color( xred )  
             call tchars(text,22,770.0,max_ypos+3.0)
             call xset_color( xblack )  
          else
             open(18,file='mulplt.wav',status='unknown')
             write(6,*)' Writing out mulplt.wav'
          
             write(18,'(i8,1x,2f7.3,2i2,1x,a)') 
     *       nchan, filt_old_low,filt_old_high,
     *       recfil_pole,npasses_old,sfile
             write(18,'(a,1x,a)') '(7e14.5)',text(1:12)
c
c  reset filter and response, there might not be if no filter
c
             if(filt_old.ne.0) then
                filt=filt_old
                flow(filt)=filt_old_low
                fhigh(filt)=filt_old_high
                flow_pole(filt)=filt_old_low_pole
                fhigh_pole(filt)=filt_old_high_pole
             endif
             npasses=npasses_old
             disp_vel=disp_vel_old
             remove_response=remove_response_old
             do_wa=do_wa_old
             do_mb=do_mb_old
             do_mbb=do_mbb_old
             do_ms=do_ms_old
             do_mss=do_mss_old  
             ml_low=ml_low_old
             ml_high=ml_high_old    
          endif
       endif

c-------------------------------------------------------------
c   start channel plot loop                                                     
c-------------------------------------------------------------
c  
                                                                        
       do jc=1,nchan                                                            
          current_seq_chan=jc              ! save current sequential channel
          ich=channelname(jc)                                                   
          current_chan=ich                 ! put current channel in common
          if(plotoption.eq.2) write(6,*)' Channel:',ich                         
c                                                                               
c  check if only end of frame plotted                                           
c                                                                               
          if(jc.gt.1) pframe=2                                                  
c                                                                               
c   last channel turns axis on                                                  
c                                                                               
          if(jc.eq.nchan) then                                                  
             if(nchan.eq.1) then                                                
                paxis=3                                                         
             else                                                               
                paxis=1                                                         
             endif                                                              
             paxisnumb=1                                                        
          endif                                                                 
c
c   if overlay channel, do not move it down
c
          if(.not.wav_overlay(ich)) then
              ypos=ypos-height        
c
c   save position of channel
c
              wav_chan_position(jc)= ypos+height/2
          else
              wav_chan_position(jc)= ypos-16.0+height/2  ! overlay channel notation
          endif                                 ! is further down
                                                                                
c                                                                               
c   get one channel of data                                                     
c                               
          wav_rot_comp(ich)=' '
          if(rotate.and.baz(ich).lt.400.0.
     *       and.wav_comp(ich)(4:4).ne.'Z') then
             call wav_read_2channel(ich)
             numb_samp=wav_out_duration(1)*wav_rate(ich)+1
c
c   check if both channles there
c
             if(wav_error_message.ne.' ') then
                 write(6,*)wav_error_message
c                 goto 3030                   ! jump to normal reading
                 call wav_read_channel(ich)
                 delay_used=delay(ich)
                 numb_samp=wav_nsamp(ich)
             else   ! changed goto above lo 19/03/2013
               if(wav_comp(ich)(4:4).eq.'N') wav_rot_comp(ich)='R'
               if(wav_comp(ich)(4:4).eq.'E') wav_rot_comp(ich)='T'
               call rotate_comp(numb_samp,
     *         wav_out_first_sample(wav_current_chan(2)),
     *         wav_out_first_sample(wav_current_chan(3)),
     *         wav_rot_comp(ich),
     *         baz(ich),signal1,signal2,signal3)
c
c   replace parmeters for current channel due to rotation, time
c   start and interval might be different if 2 horiznatal channels
c   have different start times
c
               delay_used=wav_out_start(1)
             endif
          else
c 3030        continue     
             call wav_read_channel(ich)
             delay_used=delay(ich)
             numb_samp=wav_nsamp(ich)
          endif
          rate=wav_rate(ich)
c                                                                               
c  find time for start and stop of the window, relative main header             
c                                                                               
          page_start=(ipage-1)*page_time+start_time                             
          page_stop=page_start+page_time                                        
          if(page_stop.gt.stop_time) page_stop=stop_time                        
c         trace_delay=delay(ich)-start_time-(ipage-1)*page_time                 
          trace_delay=delay_used-start_time-(ipage-1)*page_time
          if(trace_delay.lt.0.0) trace_delay=0.0                                
c                                                                               
c   find corresponding sample number to use                                     
c                                                                               
          first=(page_start-delay_used)*rate+1+0.5  ! +0.5 to round off to
          if(first.le.0) first=1                    ! nearest sample      
          last=(page_stop-delay_used)*rate+1+0.5    !jj                                    
c-- check if data for this window                
          if(last.lt.0) last=1			
c-- check if enough data           
          if(last.gt.numb_samp) last=numb_samp
c
c  save in case of write out, probably not used anymore
c
          cstart(jc)=trace_delay   ! start time realative current window start
          cinter(jc)=(last-first+1)/rate  ! interval in window
c                                                                               
c   transfer data to plotting array, possibly filter and response correction                            
c                                                                               
c          write(*,*) ' debug before trans_ilt ',disp_vel
          call trans_filt(1)  
c
c   find dc, needed for mt write out an dmulti page plot
c
             dc=0.0                                                             
             do i=1,numb_samp                                                   
                dc=dc+y(i)                                                      
             enddo                                                              
             dc=dc/numb_samp                                                      
c                                                                               
c   find max and remove dc if multi page plot                                   
c
                                                                               
          max_all=0.0                                                           
          if(time_scale.ne.0.0) then
             do i=1,numb_samp                                                   
               y(i)=y(i)-dc                                                     
               if(abs(y(i)).gt.max_all) max_all=abs(y(i))                       
             enddo                                                              
             if(max_all.eq.0.0) max_all=1.0                                    
          endif
                                                                 
c                                                                               
c   plot data, first check if an overlay channel, if so
c   change color                                                                
c
          if(wav_overlay(ich)) color_trace=xred
c
c  write one trace of mt data
c
          if(mt_out) then
             mt_stat=wav_stat(ich)
             mt_comp=wav_comp(ich)
             mt_rate=wav_rate(ich)
c
c   make sure rotation is indicated
c
             if(wav_rot_comp(ich).ne.' ') 
     *       mt_comp(4:4)=wav_rot_comp(ich)

c
c   indicate if filtered or instrument corrected
c
             if(filt.gt.0.or.remove_response.gt.0) mt_comp(1:3)='RR '  
c
c   put in dc first if no data to get same start time for all
c   traces. 
c

             k=1
             if(trace_delay.gt.0.0) then
                k=trace_delay*wav_rate(ich)
                do i=1,k
                   wav_y1(i)=dc
                enddo
                k=k+1
             endif
             do i=first,last
                wav_y1(k)=y(i)
                k=k+1
             enddo
             mt_nsamp=k-1
c
c   check if trace end before end of plot, then put in dc
c
             k=(stop_time-start_time)*wav_rate(ich)-mt_nsamp-2
             if(k.gt.0) then
                do i=1,k
                  wav_y1(i+mt_nsamp)=dc
                enddo
                mt_nsamp=mt_nsamp+k
             endif
c
c  write channel headers
c
             write(18,'(a)') 
     *       '     0.0000e+00     0.0000e+00      0  0  0.00'
             write(18,
     *       '(i8,f10.3,a,1x,a5,1x,a4,1x,i4,1x,2i2,1x,2i2,1x,f7.3)')
     *       mt_nsamp,1/mt_rate,
     *       '  0.0000e+00',mt_stat,mt_comp, 
     *       year,month,day,hr,min,fsec
c
c   write data
c
             write(18,'(7e14.5)') (wav_y1(i),i=1,mt_nsamp)
          endif
c
c  plot one trace or spectrogram
c
                                                                         
          call plotw

c   back to normal color
c
          if(wav_overlay(ich)) color_trace=xblack
          
c                                                                               
c   turn axis pics off                                                          
c                                                                               
          paxis=0                                                               
c                                                                               
c   plot single channel anotation, add max amp if flag is on                    
c   first change size for max, only effective for postscript                    
c                                                                               
          call xset_color( color_title )    !JAB(BGS)Jan95.
          call fontsize(0,1.5)                                                  
          if(pmax.eq.2) then                                                    
            imax=max+0.5                                                        
            if(max.gt.10.0)write(text,'(i12)') imax
            if(max.le.10.0)write(text,'(g12.3)') max                                            

c-- screen
            if(wav_overlay(ich)) then  
               call xset_color( xred )             
               call tchars(text,12,870.0,ypos+height-38)
c-- postscript                
               call pchars(text,12,930.0,ypos+height-15)
               call xset_color( xblack )	
            elseif(choice.ne.'SGM') then           ! not if spectrogram
               call tchars(text,12,870.0,ypos+height-22)
c-- postscript                
               call pchars(text,12,930.0,ypos+height-5)	
            endif
c
c   use same variables for dc
c
            imax=dc_chan+0.5                                                        
            write(text,'(i12)') imax                                            

            if(wav_overlay(ich)) then
c-- screen  
               call xset_color( xred )                 
               call tchars(text,12,100.0,ypos+height-38)	
c-- postscript                
               call pchars(text,12,130.0,ypos+height-15)
               call xset_color( xblack )     	
            elseif(choice.ne.'SGM') then
c-- screen              
               call tchars(text,12,100.0,ypos+height-22)	
c-- postscript                
               call pchars(text,12,130.0,ypos+height-5)	
             endif
          endif                                                                 
c
c   write rotation angle if rotation
c
          if(rotate) then
             write(text,'(a,i4)') 'baz',int(baz(ich))
             call tchars(text,7,260.0,ypos+height-22)
             call pchars(text,7,260.0,ypos+height-5)
          endif

c
c   get epicentral and hypocentral distance
c
          edist=0
          hdist=0
          if(plot_distance.eq.1.0.or.plot_distance.eq.2.0) then
             call get_dist(wav_stat(ich),edist,hdist)
             if(plot_distance.eq.2.0) edist=hdist   ! use hypocentral instead of epicentral dist
             if(edist.gt.0.0) then
                if(edist.lt.1000.0)  write(text,'(f5.1)') edist
                if(edist.ge.1000.0)  write(text,'(i5)') int(edist+0.5)
                call tchars(text,5,200.0,ypos+height-22)                
                call pchars(text,5,220.0,ypos+height-5)
             endif
          endif	
c                                                                               
c  plot channel anotation, change font size                                     
c                                                                               

          call fontsize(0,2.5)                                                  

          write(text,'(a5,1x,a2,a1,1x,a2,1x,a2)')
     *    wav_stat(ich),wav_comp(ich)(1:2),wav_comp(ich)(4:4),
     *    wav_location(ich),wav_network(ich)
c
c   make sure rotation is plotted
c
          if(wav_rot_comp(ich).ne.' ') text(9:9)=wav_rot_comp(ich)
c
c   save component and station in case of write out, used anymore ?
c
          stat(jc)=text(1:5)
          comp(jc)=wav_comp(ich)(1:3)//text(9:9)

          if(wav_overlay(ich)) then
c
c    screen plot
c
             call xset_color( xred )                  
             call tchars(text,15,2.0,ypos+height/2-21.0)           
c
c   postscript
c
             call pchars(text,15,5.0,ypos+height/2-15.0)
             call xset_color( xblack )  
          else
 
c
c    screen plot
c                
             call tchars(text,15,2.0,ypos+height/2-5.0)           
c
c   postscript
c
             call pchars(text,15,5.0,ypos+height/2-5.0)                                                                                
          endif                                                                               
c   plot picks if present                                                       
c                                                                               
          if(nrecord.ne.0) then                                                 
c                                                                               
c   change symbol size                                                          
c                                                                               
             call fontsize(0,2.0)                                               
c                                                                               
c   find readings for station, if any, and convert to plot vectors           
c   for tecpic                                                                  
c                                                                               
             call convert(wav_abs_time(wav_first),wav_stat(ich),               
     +       wav_comp(ich),-1)
c                                                                               
c   plot pics                                                                   
c                                                                               
             call tecpic                                                        
          endif                                                                 
       enddo  !**************   end channels plotting loop ***************
c                                                                               
c   filter info                                                                 
c                                                                               
       if(filt.ne.0) then
          text=' '                                                       
          write(text(1:24),201) flow(filt),fhigh(filt),recfil_pole,
     *    npasses                         
 201      format('Filt: ',f6.3,1x,f6.3,i3,i2)                                                                                                                                                   
          call xset_color(xred)      
          call xchars(text,24,xpos+650.0,max_ypos+3.0) 	! WCC+jh moved down
          call xset_color(color_def)
       endif              
c
c   close mt file and turn mt_out off
c
        if(mt_out)  then
           text(1:24)='File mulplt.wav finished'
           call xset_color(5)          ! white to clear
         call fillbox(xpos+650.0,max_ypos+18.0,xpos+900.0,max_ypos+32.0) ! & fill.
           call xset_color( xred )  
           call tchars(text,24,xpos+650.0,max_ypos+18.0)
           call xset_color( xblack )  
           close(18)                                                
           mt_out=.false.
        endif
                                                             
c
c---------------------------------------------------------------------------                                                                               
c   check if more pages to plot and reset                                       
c---------------------------------------------------------------------------
c                                                                   
       if(stop_time-start_time.gt.ipage*page_time+1) then                         
         ipage=ipage+1                                                          
         ypos=max_ypos                                                               
         paxis=2                                                                
         paxisnumb=0                                                            
         pframe=3                                                               
c                                                                               
c   calculate time of next                                                
c                                                                               
         secdif=page_time                                                       
         call timadd(year1,month1,day1,hr1,min1,sec1,secdif,                  
     *              year,month,day,hr,min,sec1)                              
         year1=year                                                             
         month1=month                                                           
         day1=day                                                               
         hr1=hr                                                                 
         min1=min                                                               
         fsec=sec1                                                             
         fmin=min+sec1/60.0
         fhour=hr+fmin/60.0
c                                                                               
c   erase and plot next page                                                    
c                                                                               
         call xnewpag
         goto 1	                                                                
       endif                                                                    
c
c  plot finished, reset scaling factor in case pic is called                    
c                                                                               
       max_all=0.0                                                              
       max_count=0
c
c  reset filter passes
c 
       npasses=0
c                                                                               
       return                                                                   
       end                                                                      
c-----------------------------------------------------------------------------  
      subroutine chasel(nchan,wt1,wt2,start_time,stop_time)                
c                                                                               
c   select channels from a traceplot and much more                                            
c                                                                               
c     j. havskov, feb 91                                                        
c
c   nov 7 91 by j.h. : turn plot off when hardcopy
c   apr   93         : put in filter etc
c   jul   94         : version 5
c                                                                               
c-- number of channels on plot                              
c   input.    nchan:		
c-- channel numbers on plot                       
c             chan:           , now in common  
c-- start time active when called                        
c             start_time	
c-- stop time active when called                         
c             stop_time		
c-- number of channels selected                              
c   output:   nchan		
c-- channel numbers selected                      
c             chan              
c-- time window selected, for future use                   
c             wt1,wt2		
c-- start time selected for multi chan plot              
c             start_time	
c-- stop -----------------------                  
c             stop_time         
c                                                                               
c                                                                               
      implicit none
      save
      include 'mulplt.inc'                                                      
      include 'seisan.inc'
c                                                                               
c   local variables                                                             
c                                                                               
c-- cursor position                                            
      real 	ix,iy		
c-- character read at cursor position                          
      integer	ich		
c-- multitrace window start time                       
      real      start_time	
c-- multitrace window stop selected               
      real      stop_time       
c-- text
      character*80 text
c-- time window selected, single channels              
      real	wt1(max_trace),wt2(max_trace) 
c-- more positions                                    
      real      ix1,ix2,iy1	
c-- number of channels                             
      integer 	nchan           
c-- channel numbers                               
c     integer   chan(max_trace)   now in common as channelname        
c-- channels selected here, temporay storage       
      integer   local_chan(max_trace)	
c-- local selected start time                         
      real      local_start	
c-- local selected stop time                      
      real      local_stop      
c-- channels selected                               
      integer   ichan_pic
c-- prompt
      character*80 prompt
c      integer on(50)     ! munu butons on or off
c-- plotoption on entry
      integer save_option
c-- indicater for filter
      logical filsel
      integer ichan1,ichan2, sei integer
c--- indicator if filter, channels or window has been selected, indicating
c    replot of same event
      logical replot
      logical valid_key   ! if valid key for pick
c-- counter                                              
      integer   i,ichan,j,l,k		
      integer code 
      real dif  ! help variable
      logical save_wa,save_mb,save_ms,save_mss, save_mbb ! temporary save flags
c-- common plot variabls
      include 'seiplot.inc'
c
c   save plotoption and turn hcopy off

c
      save_option=plotoption
      plotoption=0
c
c   initially do not assume replot
c
      replot=.false.
c                                                                               
c   initialize                                                                  
c                                                                               
      do i=1,max_trace                                                              
         local_chan(i)=0                                                        
         wt1(i)=0.0                                                             
         wt2(i)=0.0                                                             
      enddo                                                                     
      local_start=0.0                                                           
      local_stop=0.0                                                            
c
c    no picks made in traceplot
c
      last_ix=0
      last_iy=0
      last_ich=0
      current_chan=0
c
c   save flags in case used for amplitude
c
      save_wa=do_wa
      save_ms=do_ms
      save_mb=do_mb
      save_mss=do_mss
      save_mbb=do_mbb
c
c   clear previous choices
c
      do_wa=.false.
      do_mb=.false.
      do_ms=.false.
      do_mbb=.false.
      do_mss=.false.
      mt_out=.false.

      remove_response=0
      if(.not.filter_perm) then
         filsel=.false.
         filt=0
      endif
c
c   clear all choises
c
      do i=1,42
        onbox(i)=0
      enddo
c
c   call up cursor                                                              
c                                                                               
 11   continue                                                                  
      call xscursr(ich,ix,iy)
      if(char(ich).eq.'r') then                                         
        choice='REPL'                                                         
        goto 99
      endif
c
c  next is to fix a bug not solved. when A get into tecpic, it somehow makes 
c  the phase AMP, so to prevent this A should not be accepted at all in multi
c  trace mode
c
      if(char(ich).eq.'A') goto 11                                                                    
c
c   check if menu input
c
      if(plotoption.lt.2)
     *call pick_menu_box(onbox,14,63.0,16.0,'multip',
     *        2.0,758.0,ix,iy,ich,mouse,show_menu,flow,fhigh)
                                                   
c
c   check if a pick, if so determine which channel and  save pic and position
c   if amplitude, determine later when both amplitudes have been picked
c
c
c   check if a valid key for pick routine
c
      valid_key=.false.
      do i=1,numb_keys
         if(char(ich).eq.ascip(i)) valid_key=.true.
      enddo
      do i=1,10
         if(ich.eq.ichar(key_weight(i))) valid_key=.true.
      enddo
      if(char(ich).eq.'a'.or.
     *char(ich).eq.'Y') valid_key=.true.
      if((char(ich).eq.'d').
     *and.iy.lt.max_ypos) valid_key=.true.
c
c   if not valid_key, skip section on phases
c
      if(ich.eq.mouse) valid_key=.false.
      if(char(ich).eq.'&') valid_key=.false.
      if(ich.eq.6) valid_key=.false.  ! mouse right click
c
      if(valid_key) then
c
c   if amplitude, restore flags
c
         if(char(ich).eq.'a') then
            do_wa=save_wa
            do_mb=save_mb
            do_ms=save_ms
            do_mbb=save_mbb
            do_mss=save_mss
          endif
c                                                                               
c   find channel, assume top of plot frame is at y=max_ypos                          
c                                                                               
         if(char(ich).ne.'a') then
            ichan_pic=(max_ypos-iy)/height+1     ! sequential number plotted       
c
c   if channel not valid, call up cursor again, 
c
            if(ichan_pic.le.0.or.ichan_pic.gt.nchan) goto 11
            ypos=max_ypos-ichan_pic*height       ! find position of trace
            current_chan=channelname(ichan_pic)    ! channel number in file
         endif

         last_ix=ix
         last_iy=iy
         last_ich=ich
         choice='PICP'
c        write(6,*) choice,last_ix,last_iy,last_ich,current_chan
         goto 99   ! go to  pick routine 
       endif


c
c---------------------------------------------------------------------
c   check if help
c---------------------------------------------------------------------
c
       if(char(ich).eq.'?') then
          choice='HELP'
          goto 99
       endif
c
c----------------------------------------------------------------------
c   hard copy all channels
c----------------------------------------------------------------------
c
       if(char(ich).eq.'>') then
          choice='HCAL'
          goto 99
       endif
c
c---------------------------------------------------------------------
c  check if iasp calculated indicated by I
c-----------------------------------------------------------------------
c
       if(char(ich).eq.'I') then
           choice='IASP'
           goto 99
       endif
c
c---------------------------------------------------------------------
c   check if filter specified by z,x ....
c---------------------------------------------------------------------
c
      call get_filter(ich,filsel)
      if(filsel) then
         replot=.true.
         goto 11
      endif
c
c--------------------------------------------------------------------
c    check if component rotation
c--------------------------------------------------------------------
       if(char(ich).eq.'U') then
          if(rotate) then
              rotate=.false.
          else
              rotate=.true.
          endif
          goto 11
       endif
c
c------------------------------------------------------------------                                                                               
c  check if system response shall be removed, specified by w              
c  to simulate wa seismogram
c------------------------------------------------------------------
c                    
                                                                                
c-- w                               
       if(char(ich).eq.'w') then		
          remove_response = 1                                                   
          do_wa=.true.
          replot=.true.
       endif                                                                    


c------------------------------------------------------------------                                                                               
c  check if output of mt data
c------------------------------------------------------------------
c                    
                                                                                
c-- T                               
       if(char(ich).eq.'T') then		
          mt_out=.true.                                                   
          choice='REPL'  ! replot immediately to write out values
          goto 99
       endif
c
c----------------------------------------------------------------
c   check if max number of channels plotted per screen should be changed,
c   toggles default on off 
c---------------------------------------------------------------
c
       if(char(ich).eq.'N') then
          if(n_chan_screen_org.eq.n_chan_screen) then
              n_chan_screen=900
          else
              n_chan_screen=n_chan_screen_org
          endif
          choice='NCSC'
          goto 99		
       endif           
c
c-----------------------------------------------------------------
c  check if system response shall be removed, specified by G or g.
c-----------------------------------------------------------------
c
c-- G or g
       if(ich.eq.71.or.ich.eq.103) then
          remove_response = 1
          replot=.true.
       endif

c------------------------------------------------
c   mb filter
c------------------------------------------------
c j                               
       if(char(ich).eq.'j') then		
          remove_response = 1                                                   
          do_mb=.true.
          replot=.true.
       endif                                                                    

c
c   mB filter
c
c J                                
       if(char(ich).eq.'J') then		
          remove_response = 1                                                   
          do_mbb=.true.
          replot=.true.
       endif      
c
c   ms filter
c
c-- k                               
       if(char(ich).eq.'k') then		
          remove_response = 1                                                   
          do_ms=.true.
          replot=.true.
       endif    

c
c   mS filter
c
c-- K                               
       if(char(ich).eq.'K') then		
          remove_response = 1                                                   
          do_mss=.true.
          replot=.true.
       endif                                                                  
c
c-----------------------------------------                                                                               
c  check if quit as specified by Q or q                                         
c-----------------------------------------                                                                               
c-- Q or q                               
       if(ich.eq.81.or.ich.eq.113) then	
          choice='QUIT'                                                         
          goto 99                                                               
       endif                                                                    
c
c-------------------------------------------------------------
c  check if other channels are wanted indicated by o 
c-------------------------------------------------------------
c
       if(char(ich).eq.'o') then
          choice='OTHE'
          goto 99
       endif
c
c-------------------------------------------------------------
c  check if back  (event or window) indicated by B 
c-------------------------------------------------------------
c
       if(char(ich).eq.'B') then
c       if(char(ich).eq.'B'.or.ich.eq.1) then
          choice='BACK'
          goto 99
       endif

c--------------------------------------------------------------------
c  check if increasing cont interval
c--------------------------------------------------------------------
c
       if(char(ich).eq.'Z') then
          cont_interval=cont_interval*1.3
          cont_window=.true.     ! window changed
          goto 11
       endif

c--------------------------------------------------------------------
c  check if decreasing cont interval
c--------------------------------------------------------------------
c
       if(char(ich).eq.'X') then
          cont_interval=cont_interval/1.3
          cont_window=.true.
          goto 11
       endif

c
c-------------------------------------------------------------
c  check if other waveform file is wanted indicated by W 
c-------------------------------------------------------------
c
       if(char(ich).eq.'W') then
          choice='OTWA'
          goto 99
       endif
c
c---------------------------------------------------------------
c  check if write out indicated by O
c---------------------------------------------------------------
c
       if(char(ich).eq.'O') then
          choice='WOUT'
          wave_out=.true.
          goto 99
       endif
c
c---------------------------------------------------------------
c  check if FK indicated by F
c---------------------------------------------------------------
c
       if(char(ich).eq.'F') then
          choice='FK  '
          wave_out=.true.
          fk=.true.
          goto 99
       endif
c
c--------------------------------------------------------------------
c    check if plot of particle motion
c--------------------------------------------------------------------
       if(char(ich).eq.'P') then
          if (do_pmp) then
            do_pmp=.false.
          else
            do_pmp=.true.
          endif
          choice='REPL'
          goto 99
       endif
c
c---------------------------------------------------------------
c  check if next window indicated by tab 
c---------------------------------------------------------------
c
       if(ich.eq.9) then
          choice='NEXW'
          goto 99
       endif
c
c
c---------------------------------------------------------------
c  check if response spectrum indicated by R, currently not used
c---------------------------------------------------------------
c
c       if(char(ich).eq.'R') then
c          choice='RSPE'
c          wave_out=.true.
c          rspec=.true.
c          goto 99
c       endif
c
c                                                                               
c---------------------------------------------------------------
c  check if finished with selection as specified by F or f                      
c  if selection has been made, assume same event to be plotted
c  in multichannel mode
c----------------------------------------------------------------                                                                               
c-- F or f                               
       if(ich.eq.70.or.ich.eq.102) then		
          choice='FINI'
c         if(replot) choice='REPL'
          goto 99				
       endif
c
c-----------------------------------------------------------------
c   input fixed scaling
c-----------------------------------------------------------------
c
       if(char(ich).eq.'*') then
        prompt=
     *'   Give number of counts for full scale'
          call oneline(prompt,50,text,30,20.0,500.0)
c         read(text,'(i)') max_count
          max_count=sei integer(text,code)
       endif
c
c----------------------------------------------------------------
c   check if replot with current choices
c----------------------------------------------------------------
c
       if(char(ich).eq.'r') then
          choice='REPL'
          goto 99
       endif
c
c---------------------------------------------------------------
c   check if locate event
c----------------------------------------------------------------
c
       if(char(ich).eq.'L'.or.char(ich).eq.'l') then
          choice='LOCA'
          goto 99
       endif
c
c-------------------------------------------------------------
c   check if merge waveform files
c-------------------------------------------------------------
c
       if(char(ich).eq.'M') then
          choice='MERG'
          goto 99
       endif
c
c-------------------------------------------------------------
c   check if single spectrogram
c-------------------------------------------------------------
c
       if(char(ich).eq.'E ') then   ! correct key
          choice='SG'
c                                                                               
c   find channel, assume top of plot frame is at y=max_ypos                          
c                                                                               

          ichan_pic=(max_ypos-iy)/height+1     ! sequential number plotted       
c
c   if channel not valid, call up cursor again, 
c
          if(ichan_pic.le.0.or.ichan_pic.gt.nchan) goto 11
          ypos=max_ypos-ichan_pic*height       ! find position of trace
          current_chan=channelname(ichan_pic)    ! channel number in file
c
c  get time of cursor, indicate start of selection for spectrogram
c
         trace_delay=(ix-xpos)/xscale+start_time 
          goto 99
       endif

c
c-------------------------------------------------------------
c   check if spectrogram for all channels
c-------------------------------------------------------------
c
       if(char(ich).eq.'s') then   ! correct key
          choice='SGM'
c
c  if filter set in mulplt.def, put it in
c
          if (filt.eq.0.and.spectrogram_filter.ne.0.0) then
            filt=9             ! put in filter from mulplt.def
            npasses=1          ! for local events
            flow(9)=spectrogram_filter
            fhigh(9)= 0.
            flow_pole(9)=4
            fhigh_pole(9)=4
          endif
          goto 99
       endif
c
c------------------------------------------------------------
c   check if channel order flag changed
c   currently (mar 07, jh) it can only be turned on
c------------------------------------------------------------
c
       if(char(ich).eq.'_') then
          nsort_distance=1
c         if(nsort_distance.eq.1) then
c             nsort_distance=0   
c             goto 5757
c         endif
c         if(nsort_distance.eq.0) nsort_distance=1   
c5757     continue
       endif 
cjh3-07   if(nsort_distance_save.gt.0) then
cjh3-07       if(nsort_distance.eq.0) then
cjh3-07          nsort_distance=nsort_distance_save   ! was off, now onn
cjh3-07       else
cjh3-07          nsort_distance=0                 ! was, on, now off
cjh3-07       endif
cjh3-07   else              ! not in mulplt.def, ask for value
c              prompt=
c     *        '   Give number of stations to be present '
c              call oneline(prompt,50,text,30,20.0,500.0)
c              nsort_distance=sei integer(text,code)
c              nsort_distance=1                         ! not quite sure why a number
cjh3-07       nsort_distance_save=nsort_distance        ! was asked for, changed may 2003 jh
cjh3-07   endif
cjh3-07endif

c
c------------------------------------------------------------
c   check if choice is pick mode indicated by t, togl mode
c----------------------------------------------------------------
c
       if(char(ich).eq.'t') then
          choice='TOGL'
          goto 99
       endif    
c increase or decrease plot magnification
       if(ich.eq.2) then
         ymag=ymag*2.
         choice='REPL' 
         goto 99
       endif
       if(ich.eq.4) then
         ymag=ymag/2.
         choice='REPL' 
         goto 99
       endif
c forward/backward
       if(ich.eq.3) then
         forward=.true.
         goto 99
       endif
       if(ich.eq.1) then
         backward=.true.
         goto 99
       endif
c
c------------------------------------------------------------
c   check if choice selecting another radius for area selection
c----------------------------------------------------------------
c
       if(char(ich).eq.'R') then
          prompt=
     *  ' Enter radius (deg) for area selection'//char(0)
          call oneline(prompt,38,text,30,20.0,500.0)
c 
          if(text(1:5).ne.'     ') then
             read(text,*) mulplt_radius
             mulplt_area_save=2   ! signal update of radius 
             choice='OTHE'        ! go to channel selection screen
             goto 99
          endif
       endif    

c
c------------------------------------------------------------
c   check if choice selecting station area selection
c----------------------------------------------------------------
c
       if(char(ich).eq.'S') then
          prompt=
     *  ' Enter station for area selection'//char(0)
          call oneline(prompt,33,text,30,20.0,500.0)
c 
          if(text(1:5).ne.'     ') then
             mulplt_stat=text(1:5)
c
c   check if area selection was on
c      
             if(mulplt_area.le.0.or.mulplt_area.gt.8) then
                mulplt_area=6     ! turn on as if data from mulplt.def
c
c   if no radius has been set in mulplt.def, ask now
c
               if(mulplt_radius.gt.179.0) then   
                  prompt=
     *            ' Enter radius (deg) for area selection'//char(0)
                  call oneline(prompt,38,text,30,20.0,500.0)
c 
                  if(text(1:5).ne.'     ') then
                     read(text,*) mulplt_radius
                  endif
               endif
             endif                           
             mulplt_area_save=0   ! signal tah tno old values are used 
             choice='OTHE'        ! go to channel selection screen
             goto 99
          endif
       endif    


c
c------------------------------------------------------------
c   check if choice is all component  mode for one station indicated  by y
c----------------------------------------------------------------
c
       if(char(ich).eq.'y') then
          choice='ALLC'
          goto 99
       endif                                                                  
c
c------------------------------------------------------------
c   check if choice is all component  mode for many stations
c   indicated  by u
c----------------------------------------------------------------
c
       if(char(ich).eq.'u') then
          choice='ALCC'
          goto 99
       endif           
c
c--------------------------------------------------------------------                                                                               
c  check if delete file indicated by d or D, cursor must be outside
c  plotting area                                          
c----------------------------------------------------------------------                                                                               
c-- d                                
       if(char(ich).eq.'d'.and.iy.gt.max_ypos) then		
          choice='DEL '                                                         
c-- update readings array                                    
          goto 99				
       endif                                                                    
c                                                                               
c-------------------------------------------------------------------
c-- D   Delete S-file
c----------------------------------------------------------------------
c                                
       if((char(ich).eq.'D'.and.iy.gt.max_ypos).or.
     * char(ich).eq.'<') then		
          choice='DELS'                                                         
c-- update readings array                                    
          goto 99				
       endif                                                                    
c
c--------------------------------------------------------------------------
c  check if registration as specified by P or p (put)                                 
c                                                                               
c--                                
       if(char(ich).eq.'p') then		
          choice='PUT '
          goto 99
       endif                                                         

       if(char(ich).eq.'P') then		
          choice='PUTS'                                                         

c-- update readings array                                    
          goto 99				
       endif                                                                    
c                                                                               
c--------------------------------------------------------------------
c   check if mouse indicating channel select or zoom window                              
c--------------------------------------------------------------------
c
c
c       write(6,*) 'char',char(ich)                                                                               
       if(ich.eq.mouse) then
c         write(*,*) ' xxx mouse '
         call xset_color( color_frame )        !JAB(BGS)Jan95.
c                                                                               
c   plot and save first position                                                
c                                                                               
         ix1=ix                                                                 
         iy1=iy                                                                 
c                                                                               
c   find if channel to select, assume top of plot frame is at y=max_ypos and mouse
c   click is below max_ypos and left of y-axis
c
         if(iy.lt.max_ypos.and.ix.lt.xpos) then                          
c                                                                               
c            ichan1=(max_ypos-iy)/height+1
               dif=2000.0
               do i=1,nchan
                 if(abs(iy-wav_chan_position(i)).lt.dif) then
                    dif=abs(iy-wav_chan_position(i))
                    ichan1=i
                 endif
               enddo     
                                            
c           write(6,*) ichan1,nchan
            if(ichan1.lt.1) ichan1=1
            if(ichan1.gt.nchan) ichan1=nchan
c                                                                               
c   plot that channel is selected                   
c                                                                            
c            iy=max_ypos-(ichan1*height)
            iy=wav_chan_position(ichan1)-height/2                                            
            call xmovabs(xpos,iy)
            call xdrwabs(0.5,iy)
            call xdrwabs(0.5,iy+height)                                           
            call xdrwabs(xpos,iy+height)                                         
c                                                                               
c   store channel selected                                                   
c                                                                               
            local_chan(ichan1)=1                                                
c
c   assume replot
c
            replot=.true.
            goto 11
         endif		

	
	
c
c--------------------------------------------------------------                                                                               
c   check if whole plot zoom by mouse click in trace section                   
c--------------------------------------------------------------
c                                                                               
         if(iy.lt.max_ypos.and.ix.gt.xpos) then
c
c   plot first line
c
            call xmovabs(ix1,30.0)
            call xdrwabs(ix1,max_ypos)
c
c   get next position, do not accept a very small interval
c
 3000       continue   ! get here if interval was too small
            call xscursr(ich,ix2,iy)                                         
c
c   if 2. cursor left of first cursor, plot whole trace
c
            if(ix2.lt.ix1)then
               start_time=0.0
               stop_time=wav_total_time
               local_stop=0.0      ! do not change start and stop below
            else
               local_start=(ix1-xpos)/xscale+start_time                         
c               write(*,*) ' xxx ',start_time,ix1,xpos
               if((ix2-ix1).le.2.0) goto 3000    ! try again, too close
               local_stop=(ix2-xpos)/xscale+start_time                          
            endif
            call xmovabs(ix1,30.0)                                           
            call xdrwabs(ix1,max_ypos)                                          
            call xmovabs(ix2,30.0)                                           
            call xdrwabs(ix2,max_ypos)                                          
c
c  assume replot if a window has been selected
c

            if(choice.eq.'SGM') then
               choice='REPLSGM'   ! lo 22/3/2018 - change later to REPL,
c                                                  but where, still bug
                goto 99  ! if multi trace spectrogram, zoom in spectrogram
            endif
            choice='REPL'
            goto 99         ! go directly to replot                         
         endif                           
       endif                                                                    

c                                                                               
c--------------------------------------------------------------------
c   check if mouse right clik or - indicating many channels select                             
c--------------------------------------------------------------------
c
c       write(6,*)'at right click ich=',ich                                                                               
       if(ich.eq.6) then
         call xset_color( color_frame )        !JAB(BGS)Jan95.
c                                                                               
c   plot and save first position                                                
c                                                                               
         ix1=ix                                                                 
         iy1=iy                                                                 
c                                                                               
c   find if channel to select, assume top of plot frame is at y=max_ypos and mouse
c   click is below max_ypos and left of y-axis
c
         if(iy.lt.max_ypos.and.ix.lt.xpos) then                          
c                                                                               
               dif=2000.0
               do i=1,nchan
                 if(abs(iy-wav_chan_position(i)).lt.dif) then
                    dif=abs(iy-wav_chan_position(i))
                    ichan2=i
                 endif
               enddo     
                                            
c            write(6,*) ichan2,nchan
            if(ichan2.lt.1) ichan2=1
            if(ichan2.gt.nchan) ichan2=nchan
c                                                                               
c   plot that channel is selected                   
c                                                                            
            iy=wav_chan_position(ichan2)-height/2                                            
c            call xmovabs(xpos,iy)
c            call xdrwabs(0.5,iy)
c            call xdrwabs(0.5,iy+height)                                           
c            call xdrwabs(xpos,iy+height)                                         
c                                                                               
c   store channels selected, assume from previous mouse left clik
c
c   if only right click plot all above
c
            if(ichan1.lt.1) ichan1=1
c   if ichan1 has not been reset
c
c           if(ichan1.gt.0) then  
               k=1
              
               if(ichan2.lt.ichan1) k=-1                 
               do i=ichan1,ichan2,k
                  
c                  write(6,*) 'ichan sel',i                                                            
                  local_chan(i)=1
                  if(i.ne.ichan1)iy=iy+k*height
                  call xmovabs(xpos,iy)
                  call xdrwabs(0.5,iy)
                  call xdrwabs(0.5,iy+height)                                           
                  call xdrwabs(xpos,iy+height)   
               enddo
               ichan1=0
c           endif                                                
c
c   assume replot
c
            replot=.true.
            goto 11
         endif
      endif
c                                                                               
c--------------------------------------------------------------------
c   check if selection of an overlay channel
c--------------------------------------------------------------------                              
c                                                                             
       if(char(ich).eq.'&') then
         call xset_color( color_frame )        !JAB(BGS)Jan95.
         ix1=ix                                                                 
         iy1=iy                                                                 
c                                                                               
c   find channel to select, assume top of plot frame is at y=max_ypos and mouse
c   click is below max_ypos and left of y-axis
c
         if(iy.lt.max_ypos.and.ix.lt.xpos) then                          
c                                                                               
            ichan1=(max_ypos-iy)/height+1                                               
c            write(6,*) 'overlay channel selected, nchan',ichan1,nchan

            if(ichan1.lt.1) ichan1=1
            if(ichan1.gt.nchan) ichan1=nchan
c
c   cannot start with channel 1
c
            if(ichan1.eq.1) then
               text='Channel selcted for overlay must be # 2 or later'       
               call xmessage(text,1,48,60.0,iy)
               replot=.true.
               goto 11
            endif
c                                                                               
c   plot that channel is selected                   
c                                                                            
            iy=max_ypos-(ichan1*height)                                              
            call xmovabs(xpos,iy)
            call xdrwabs(0.5,iy)
            call xdrwabs(0.5,iy+height)                                           
            call xdrwabs(xpos,iy+height)
            call xdrwabs(0.5,iy)                                         
c                                                                               
c   store that channel is selected for overlay, refer back to true 
c   channel number since channels usually are sorted, thefore use
c   channelselect below                                                   
c                                                                               
            wav_overlay(channelname(ichan1))=.true.
c           local_chan(ichan1)=1                                                
c
c   assume replot
c
            replot=.true.
            goto 11
         endif
       endif


c                                                                               
c   go and select again                                                         
c                                                                               
      goto 11                                                                   
c                                                                               
c  end of select, get corrcet channel numbers                                   
c                                                                               
 99   continue                                                                  




c
c   check if any channels selected
c
      nchan_selected=0
      ichan=0
      do i=1,nchan
        if(local_chan(i).eq.1) ichan=ichan+1
      enddo
c
c   save how many selected for option y
c
      nchan_selected=ichan

c      write(27,*) 'ichan',ichan
c
c  blank out the channels not selected if any selection, all channels
c
      if(ichan.gt.0) then
         do i=1,nchan
            if(local_chan(i).lt.1) then     ! channel deselected, put 0 in original array
               do j=1,nchan_org
                  if(channelname(i).eq.channelname_org(j)) 
     *            channelname_org(j)=0
               enddo
            endif
         enddo
c
c  shift out deselected channels, all channels
c
         ichan=0
         do i=1,nchan_org
           if(channelname_org(i).gt.0) then
              ichan=ichan+1
              channelname_org(ichan)=channelname_org(i)
           endif
         enddo
         nchan_org=ichan
                                                                              
c                                                                               
c   null rest of array                     
c                                                                               
c      if(ichan.ne.0) nchan=ichan                                                
         do i=nchan_org+1,max_trace                                                    
            channelname_org(i)=0                         !xxnew                                     
         enddo       
      endif

c----------------------------------------------------
c   now select out channels for current window only
c----------------------------------------------------

      ichan=0
      do i=1,nchan                                                          
         if(local_chan(i).eq.1) then                                            
            ichan=ichan+1                                                       
            channelname(ichan)=channelname(i)
         endif                                                                  
      enddo                                                                     
c                                                                               
c   if no channels selected, the old selection will remain                     
c                                                                               
      if(ichan.ne.0) nchan=ichan                                                

      do i=nchan+1,max_trace                                                    
         channelname(i)=0                                                              
      enddo   
 
c      write(27,*)'nchan out pic',nchan                                                             
c                                                                               
c   transfer new multitrace window if any                                       
c                                                                               
      if(local_stop.ne.0.0) then                                                
        start_time=local_start                                                  
        stop_time=local_stop                                                    
      endif                                                                     
c
c   turn hc on again
c
      plotoption=save_option
      return                                                                    
      end                                                                       
c------------------------------------------------------------------
      subroutine get_filter(ich,filsel)
c
c   get filter from keyboard vie one char input, the keyboard
c   input is obtained outside routine
c   ich is ascii value returned from key input, filsel is true
c   if a valid filter was found
c
      implicit none
      save
      include 'mulplt.inc'
      include 'libsei.inc'
c
      integer ich,code
      real ix,iy
      logical filsel
      character*80 text, prompt
      logical sun,pc,linux

      filsel=.false.
c
      call computer_type(sun,pc,linux)
c
c-------------------------------------------------------                                                                               
c   check if filter specified by z, x, v, b, n, m  etc                           
c                                                                               
c-- z                               
       if(char(ich).eq.'z') filt=1 
c-- x                               
       if(char(ich).eq.'x') filt=2 
c-- v                               
       if(char(ich).eq.'v') filt=3 
c-- b                               
       if(char(ich).eq.'b') filt=4 
c-- n 
       if(char(ich).eq.'n') filt=5 
c-- m 
       if(char(ich).eq.'m' ) filt=6	
c-- 
       if(char(ich).eq.';') filt=7
c-- togle permanent filter
       if(char(ich).eq.',') then
          if(filter_perm) then
             filter_perm=.false.
             filt=0
          else
             filter_perm=.true.
             filsel=.true.
          endif
       endif
c
c   chose a variable filter
c
       if(char(ich).eq.'.') then
 3001     continue          ! enter here if wrong filter limits
          prompt=
     *  ' Low and high cut for filter, enter for no filter'//char(0)
          call oneline(prompt,50,text,30,20.0,500.0)
c 
           if(text(2:5).eq.'    ') then                                        
              filt=0                                                           
           else                                                                
c-- use index 9 for variable filter   
                call sei get values( 2, text, code )   ! Extract 2 values.
                code = e_ok$                           ! re-store.
                flow(9) = array$(1)                    ! Retrieve.
                fhigh(9)= array$(2)                    ! Ditto.
                flow_pole(9)=4
                fhigh_pole(9)=4
c
c  check if reasonable filter
c
                if((fhigh(9).lt.0).or.
     *          (flow(9).ne.0.and.fhigh(9).ne.0.0.
     *          and.fhigh(9).le.abs(flow(9)))) then
                   filt=0
                   goto 3001
                else
                   filt=9
                   filsel=.true.
                endif
           endif
c
cfix    if(sun.or.linux) call xscursr(ich,ix,iy)  ! on sun x-win, to get rid of one char
c                                          in buffer
       endif
c
c   chose a variable filter,  poles and passes
c
       if(char(ich).eq."'") then
          npasses=1
 3011     continue          ! enter here if wrong filter limits
          prompt=
     *  ' Low and high cut, poles and passes for filter,'//
     *  ' enter for no filter'
     *  //char(0)
          call oneline(prompt,67,text,30,20.0,500.0)
c 
           if(text(2:5).eq.'    ') then                                        
              filt=0                                                           
           else                                                                
c-- use index 9 for variable filter
                npasses=0   
                call sei get values( 4, text, code )   ! Extract 4 values.
                code = e_ok$                           ! re-store.
                flow(9) = array$(1)                    ! Retrieve.
                fhigh(9)= array$(2)                    ! Ditto.
                flow_pole(9)=array$(3)
                if(flow_pole(9).eq.0) flow_pole(9)=4
                fhigh_pole(9)=flow_pole(9)
                npasses=array$(4)
                if(npasses.gt.2) npasses=2
c
c  check if reasonable filter
c
                if((fhigh(9).lt.0).or.
     *          (flow(9).ne.0.and.fhigh(9).ne.0.0.
     *          and.fhigh(9).lt.abs(flow(9)))) then
                   filt=0
                   goto 3011
                else
                   filt=9
                   filsel=.true.
                endif
                if(npasses.eq.0) npasses=1
           endif

cfix    if(sun.or.linux) call xscursr(ich,ix,iy)  ! on sun x-win, to get rid of one char
c                                          in buffer
       endif
c
c
       filsel=.false.
       if(char(ich).eq.'z'.or.char(ich).eq.'x'.or.                           
     *    char(ich).eq.'v'.or.char(ich).eq.'b'.or.
     *    char(ich).eq.'n'.or.char(ich).eq.'m'.or.
     *    char(ich).eq.'.'.or.char(ich).eq.';') filsel=.true.
c
c  number of passes, 1 is forward, 2 is both ways
c
       if(filsel) npasses=npasses+1
       if(npasses.gt.2) npasses=2
c
       return
       end
c
c
cccccccccccccc
      subroutine mulplt_help
c
c   write help text 
c
      implicit none
      save
      include 'libsei.inc'
      include 'seiplot.inc' ! plotting include
      integer i,k           ! help variables
      integer unit          ! file unit
      integer code
      character*78 text     ! help text
c
c  clear
c
      call clear_to_alpha
c
c  print help text
c
c  open file
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   unit,             ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'MULPLT.HLP' )    ! For this file.
         if( code .ne. e_ok$ ) then        ! Problem
         code = e_ok$                      ! Re-set.
         write(*,*)
         write(*,*)
     &'****                                              ****'
         write(*,*)
     &'**** WARN: help file... MULPLT.HLP does not exist ****'
         write(*,*)
     &'****       ....Press <return> to continue         ****'
         write(*,*)
     &'****                                              ****'
         write(*,*)
         read(*,'(A)') text
         goto 9999                         ! return to caller.
         end if
c
         i=1
         do k=1,1000
            i=i+1
            if(i.eq.20)  then
               write(6,*)' Return for next page, q to quit help'
               read(5,'(a)') text
               if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') goto 1002
               i=1
            endif
            read(unit,'(a)',end=1001) text
            write(6,'(2x,a)') text
         enddo
 1001    continue
        write(6,*)' Return to continue with mulplt'
        read(5,'(a)') i
 1002   continue
         call sei close( close$, unit, code ) ! Close (Default stop on error).
c
c   reopen display
c
9999  call open_display
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c      subroutine plot_cont(max_line,start_file)
      subroutine plot_cont(max_line,start_file,
     &   station_code,component_code)
c                                                                               
c  plot continoues seismograms with max_line lines  pr pages and several
c  pages if needed. Only one channel is plotted. Events or continous segments
c  come from files in filenr.lis. if there is a time shift between the end of
c  one file and the beginning of the next, the trace is shifted correspondingly.
c  Explanation of the variables are given in mulplt.inc.
c                       
c  the first file to use in filenr.lis is given in start_file                                                        
c                                                                               
c-- common block                                       
c
      implicit none
      save
      include 'seisan.inc'	
      include 'mulplt.inc'	
C
C    Seisan library details...
C    -------------------------
C
      INCLUDE   'libsei.inc'                   ! Definitions and data defns.
      external  sei open,                      ! Open file handler.
     &          sei close,                     ! Close file handler.
     &          sei code,                      ! Error handler.
     &          sei clen                       ! String length
      integer   sei clen                       ! Function.
c
      integer   code,                          ! Local error condition.
     &          read1                          ! Read unit 1.
      logical   b_flag                         ! Flag existance.?
C
C    -------- end ------------
      character*5 station_code
      character*4 component_code
c                                                                               
c                                                                               
c   local variables                                                             
c                                                                               
                                                                                
c-- used for logging, systime
      character*12 timestring
      character*14 timestring1
      character*35 extunitlogfile
c-- use for plotting                                            
      real	ix,iy		
c-- x-distance between plotted points, tec scale           
      real      xstep		
c-- plotting string                                      
      character*80 filen     ! waveform file name
      character*80 start_file
      character*80 text      ! for plotting
      character*80 text1     ! for plotting
c-- dc value                                                      
      real 	dc		
c-- skip of points for plotting                               
      integer	skip		
c-- number of samples which can be plotted                  
      integer	npoint		
      real xfirst                       ! position where segment starts
      real y_upper                      ! position top frame
      real y_lower                      ! -------- BOTTOM---
      character*1 dummy                 ! help variable
      integer ich                       ! retuen from cursor
      integer max_line                  ! maximum number of lines pr page
c-- counter for lines plotted
      integer iline    
      integer ifile                          ! file counter
      integer ipage                          ! counter for pages plotted
      double precision   file_start_time     ! abs file start time
      double precision   file_end_time       ! abs file end time
      double precision   page_start_time     ! abs page start time
      double precision   line_start_time     ! abs line start time
      character*8        filt_name           ! filter type
      real               page_total_time     ! all lines on page
      real               line_time           ! time pr line
      real               start_time          ! start time on page rel. 1. point
      logical            new_page            ! new page flag
      logical sun,pc,linux
      integer year1,doy1,month1,day1,hour1,min1    ! time etc
      real sec1
c-- y-axis position coresponding to y=0                            
      real	y0		
c-- counters and help varibales
      real t1                                                 
      integer	i,k		
c-- filter corner from mulplt.ext.tmp
      real high_pass,low_pass
c
c for logging
      character*5 def_base
      character*60 top_directory
      character*1 dchar
      character*120 txt
      logical folder_exist
      character*4 logyear
      character*4 logmdr
c
c   for findevin and indata routines
c
      character*10 keys
      integer new_month,id1,nstat1,nrecord1,nhead1,status,fstart,
     *        from_eev,nphase1
      character*80 data1(200)
      character*40 base_name
      character*80 evfile
      integer ev_no
      character*1 exp1,type1
c   dummy variables for extracting time windows in cont mode
      double precision   dummyx, dummyy, dummyb, dummye
c
      include 'seiplot.inc'       ! for plotting routines
c
c   communication with plot_cont_frame
c
      common /plotcont/y_upper,y_lower,npoint,ipage,page_start_time
c                                                                               
c  plot, set resolution
c                                                                               
      resolu=resolu_x
      if(plotoption.gt.0) resolu=resolu_x
c
c   set some defaults
c
      xpos=1.0                    ! left side of frame
      iline=1                     ! first line
      ipage=1
      ifile=0
      new_page=.true.             ! start with a new page
      y_upper=740.0               ! upper frame
      y_lower=30.0                ! lower frame                                                  
      call computer_type(sun,pc,linux)

c get system time used for logging, systime
c
      call systime(timestring,timestring1)

      height=(y_upper-y_lower)/max_line
      file_start_time=0.0         ! start time                                                     
      line_time=page_time         ! value came in via page_time
      page_total_time=line_time*max_line
c
c   scale is fixed by outside value max count
c
      yscale=(height/2)/max_count                                         
c
c   from eev
c
      if(opmode.eq.0) then
c
c   put base name
c
         base_name=' '
         base_name(1:5)=seisan_base 
         keys=' '
c
c   get s-file name of first file
c
         from_eev=0
c        write(6,*)eev_start_time,eev_end_time, base_name,event_no,
c    *   keys
c
c   get list of events
c
         call findevin
     *   (base_name,eev_start_time,eev_end_time,keys,
     *   from_eev,ev_no,
     *   evfile,fstart,new_month,status)
c        write(6,*)'en',ev_no
c
c   position at correct start event
c   put in current event number
c
         keys(1:1)='#'
         write(keys(2:5),'(i4)') event_no 
         call findevin
     *   (base_name,eev_start_time,eev_end_time,keys,
     *   from_eev,ev_no,
     *   evfile,fstart,new_month,status)
c
         keys(1:5)='SAME '    ! use this event the first time
c        write(6,*)'en',ev_no
      endif
 
c
c    open file containing list of files numbered                                
c    -------------------------------------------
c                                                                               
      if(opmode.ne.0) then
         call sei open( old$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               'filenr.lis',         ! Filename.
     &               read1,                ! Open on unit.
     &               b_filenr$,            ! Flag existance & usage?.
     &               code )                ! Condition.
         chr_err_msg$ = 
     &   '**** FATAL: problem opening "filenr.lis"'
       
c
c   position pointer at first file to use or take file from eev
c
         filen=' '
         do while(start_file(1:seiclen(start_file)).ne.
     *   filen(1:seiclen(filen)))
            read(read1,'(7x,a)',end=999) filen              ! Read a record.
            call sei code( fort$, code, read1, b_flag )     ! Process outcome.
            if( b_flag ) goto 999                           ! On end of file.
c-- no more files                        
            if(filen(1:5).eq.'     ') goto 999	
         enddo
         backspace read1 
      endif

c   Open file for extracting time windows in cont mode
      call sei open( unknown$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               'mulplt.ext.tmp',     ! Filename.
     &               extunittmp,           ! Open on unit.
     &               b_flag,               ! Flag existance & usage?.
     &               code )                ! Condition.

c   Write header to tmp file for extracting time windows in cont mode
      write(extunittmp,*)'station_code ',station_code
      write(extunittmp,*)'component_code ',component_code
      if(filt.gt.0) then
        write(extunittmp,'(1x,a6,2(1x,f6.3))')
     &  'filter',flow(filt),fhigh(filt)
      else
        write(extunittmp,*)'filter    -1.0 -1.0'   ! No filter, raw data
      endif
      write(extunittmp,*)'max_count',max_count
      write(extunittmp,*)'max_line',max_line
      write(extunittmp,*)'line_time',line_time
      write(extunittmp,*)'height',height

c
c-----------------------------------------------------------
c    get here to read next file
c-----------------------------------------------------------
c
 1000 continue
c
c   initialize waveform handling
c
      call wav_init
c
c   get next file name if not from evv
c
      if(opmode.ne.0) then
         read(read1,'(7x,a)',end=999) filen               ! Read a record.
         call sei code( fort$, code, read1, b_flag )      ! Process outcome.
         if( b_flag ) goto 999                            ! On end of file.
c-- no more files                        
         if(filen(1:5).eq.'     ') goto 999	
      else
c
c   get next event from data base
c
c        write(6,*)'keys ',keys
         call findevin
     *   (base_name,eev_start_time,eev_end_time,keys,
     *   from_eev,ev_no,
     *   evfile,fstart,new_month,status)
c        write(6,*) evfile
c        write(6,*) ev_no,eev_start_time,' ',eev_end_time,status
c
c   check if end of time interval or somthing wrong
c
         if(status.gt.0) goto 999
c
         keys='          '           ! next time take next event
c
c   open and get waveform file names, assume first name in s-file
c
         call sei open( old$,                 ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  evfile,               ! This file.
     &                  read1,                ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
         call indata(read1,nstat1,nphase1,nhead1,
     *   nrecord1,type1,exp1,data1,id1)
         call sei close(close$,read1,code)    ! Close (stop on error).
c
         call auto_tr(data1,nhead1,nrecord1,wav_nfiles,wav_filename)
         if(wav_nfiles.eq.0) then   ! if no waveform, stop
            write(6,*)' No waveform file name in S-file'
c           stop
           goto 1000
         endif
         filen=wav_filename(1)
      endif
c
c   check if and where file is
c
      call  get_full_wav_name(filen,wav_filename(1))
c
c    if file not there
c
      if(wav_filename(1).eq.' ') then
c       call clear_to_alpha
         write(6,'(1x,a,a)') ' No such file: ',
     *   wav_filename(i)(1:seiclen(wav_filename(1)))
c        write(6,*)' Return to stop'
c        read(5,'(a)') filen
c        stop
         goto 1000  ! try next file:wq
      endif

c
c   read header info
c
      call read_wav_header(1)       ! only one file
c
c   check if ok
c
      if(wav_error_message.ne.' ') then   
         call clear_to_alpha
         write(6,'(1x,a)') wav_filename(i)(1:seiclen(wav_filename(1)))
         write(6,'(1x,a)') wav_error_message
         write(6,*)' Return to stop'
         read(5,'(a)') filen
         stop
      endif

      ifile=ifile+1                               ! count number of files
      dc=0.0
c
c find channel number
c
      current_chan=0
      do k=1,wav_nchan
        if (current_chan.eq.0) then
          if (wav_stat(k).eq.station_code.and.
     &        wav_comp(k).eq.component_code) then
            current_chan=k 
          endif
        endif
      enddo
      if (current_chan.eq.0) then
        write(*,*) ' station and channel not found '
        stop
      endif

c
c   read data
c
      call wav_read_channel(current_chan)
      if(wav_error_message.ne.' ') then   
         call clear_to_alpha
         write(6,'(1x,a)') wav_filename(i)(1:seiclen(wav_filename(1)))
         write(6,'(1x,a)') wav_error_message
         write(6,*)' Return to stop'
         read(5,'(a)') filen
         stop
      endif
c                                                                               
c   get srate and number of samples                                            
c
      rate=wav_rate(current_chan)
      numb_samp=wav_nsamp(current_chan)

      file_start_time=wav_abs_time(current_chan)

      file_end_time=file_start_time+(numb_samp-1)/rate
c
c   if this is the first file, page start time will be equal to file
c   start time
c
      if(ifile.eq.1) page_start_time=file_start_time

      first=1           ! first point to use
      last=numb_samp    ! last point to use
c-- maximum number of points which can be plotted  
      npoint=line_time*rate       !  across page, one line    
      if(npoint.eq.0) npoint=1    ! there should be at least one point
      skip=npoint/resolu+1        ! plot every skip points                                                      
c-- assume plot x axis go from xpos        
      xstep=(1023-xpos)*skip/float(npoint) 
c-- tek units pr second                   
      xscale=(1023-xpos)*rate/npoint	   
c
c   transfer data
c
      do i=1,numb_samp
         y(i)=signal1(i)
      enddo
c
c  calculate dc for first file
c
c     if(ifile.eq.1) then
         do i=1,numb_samp
           dc=dc+y(i)
         enddo
         dc=dc/numb_samp
         dc_chan=dc   ! save in common
c     endif
c                                                                               
c   remove dc          
c                                                                               
      do i=1,numb_samp
          y(i)=y(i)- dc_chan                                                  
      enddo                                                                  
      if(filt.gt.0) then
         filt_name='BP      '
         if(flow(filt) .eq.0.0) filt_name='LP      '
         if(fhigh(filt).eq.0.0) filt_name='HP      '
         if(ifile.gt.1) npasses=-npasses  ! signal filter routine to use buffer 
c                                         ! - from previous file
         call recfil(y,numb_samp,y,'BU      ',0.0,0.0,4,
     *   filt_name,flow(filt),fhigh(filt),1.0/rate,npasses)
      endif
c
c   calculate where data is to be plotted on the page, could be a different
c   page
c
      start_time=file_start_time-page_start_time   ! time from start of page
      iline=start_time/line_time + 1
      line_start_time=page_start_time+(iline-1)*line_time
c
c   check for new page
c
      if(iline.gt.max_line) then
c   Write tmp file for extracting time windows in cont mode
c        write(extunittmp,*)'line_start_time',line_start_time,' 0.0'
         write(extunittmp,*)'page_start_time',page_start_time,' 0.0'
         call plot_cont_hardcopy(ipage)         ! make hard copy/pause 
c        if(choice.eq.'QUIT') return
c   Goto section for extracting time windows in cont mode
         if(choice.eq.'QUIT') goto 998
         ipage=ipage + iline/max_line           ! new page number
         page_start_time=page_start_time+(iline/max_line)*
     *   page_total_time                        ! start time of current page
         start_time=start_time-(iline/max_line)*
     *   page_total_time        
         iline=iline-(iline/max_line)*max_line  ! line number on current page
         new_page=.true.                        ! plot frame etc
      endif
c
c   plot frame etc if a new page
c
      if(new_page) then
         call plot_cont_frame
         new_page=.false.
      endif
c
c   calculate start time on line from beginning of line
c
      t1=start_time-(iline-1)*line_time
      xfirst=xpos+t1*xscale
      y0=y_upper-height/2 - (iline-1)*height        ! position of trace   
c
c   calculate time of start of line
c
      call sectim(line_start_time,
     *year1,doy1,month1,day1,hour1,min1,sec1)
      write(text,'(i2,a,i2)') hour1,':',min1
      call xchars(text,5,xpos+12.0,y0+height/8+5.0)                       
c                                                                               
c   plot data                                                                   
c
      call xset_color(color_trace)           
      ix=xfirst
      iy=y(first)*yscale+y0                                          
      if(iy.gt.780) iy=780                                                      
      if(iy.lt.0) iy=0
      y(1)=iy                                                                  
      k=1
c
c   main plotting loop
c                                                                       
      do 10 i=first+skip,last,skip                                              
         k=k+1         
         ix=ix+xstep
         if(ix.gt.1023) then                 ! at end of trace, new line
            call xdrwvec(k-1,xfirst,y,xstep) ! plot vector
            ix=xpos                                                
            xfirst=ix
            y0=y0-height       
            iy=y(i-skip)*yscale+y0           ! use line end point also as start
                                             ! - point for next line
            y(1)=iy                          ! first  point
            k=2                              ! next new is 2.
            iline=iline+1
            line_start_time=line_start_time+line_time
            if(iline.gt.max_line) then          ! end of page
c   Write tmp file for extracting time windows in cont mode
c        write(extunittmp,*)'line_start_time',line_start_time,' 0.0'
         write(extunittmp,*)'page_start_time',page_start_time,' 0.0'
               call plot_cont_hardcopy(ipage)   ! make hard copy
c              if(choice.eq.'QUIT') return
c   Goto section for extracting time windows in cont mode
               if(choice.eq.'QUIT') goto 998
               iline=1                     ! initialize for new page
               xfirst=xpos
               y0=y_upper-height/2         ! position of first trace  
               ipage=ipage+1
               page_start_time=page_start_time+page_total_time                        ! start time of current page
               call plot_cont_frame             ! plot frame etc
            endif
c
c   calculate time of start of line
c
            call sectim(line_start_time,
     *      year1,doy1,month1,day1,hour1,min1,sec1)
            write(text,'(i2,a,i2)') hour1,':',min1
            call xchars(text,5,xpos+12.0,y0+height/8+5.0)                       
         endif

         iy=y(i)*yscale+y0                                           
         if(iy.lt.1) iy=1                                                       
         if(iy.gt.780) iy=780                                                   
         y(k)=iy                                                               
 10   continue                                                                  
c
c  plot remaining data
c
      call xdrwvec(k,xfirst,y,xstep)
      xfirst=ix+xstep                           ! x-position to start next time
c
c   go and get next file 
c
      goto 1000
c
c
 999  continue
c
c   end of all plotting, call up cursor to halt
c
      write(extunittmp,*)'page_start_time',page_start_time,' 0.0'
 889  continue
      call xscursr(ich,ix,iy)			
 
c   Input Start and End times for extracting time windows in cont mode
c   or P and S readings (Local or Teleseismic):
      call mulplt_cont_picking(ich,ix,iy,height,extunittmp)

c b is reserved for going back in time
c        if(char(ich).ne.'b') goto ??????????
         if(char(ich).eq.'s') goto 889  ! not a valid char
         if(char(ich).eq.'e') goto 889  ! not a valid char
         if(char(ich).eq.'1') goto 889  !
         if(char(ich).eq.'2') goto 889  !
         if(char(ich).eq.'3') goto 889  !
         if(char(ich).eq.'4') goto 889  !
         if(char(ich).eq.'7') goto 889  !
         if(char(ich).eq.'8') goto 889  !
         if(char(ich).eq.'9') goto 889  !
         if(char(ich).eq.'0') goto 889  !

c                                                                               
c   make hard copy plot                                                         
c                                                                               
      if(plotoption.gt.0) then                                               
c
c   close plot in post script file
c
          call close_post

          call sei close( close$, plotunit, code ) ! Close 
          call clear_to_alpha
          dummy=' '
          if(plotoption.eq.1) then
             write(6,'(a,$)')' Submit plot (y=return/n) '                      
             read(5,'(a)') dummy                                               
          endif
          if(dummy.eq.' '.or.dummy.eq.'Y'.or.dummy.eq.'y') 
     *    call send_plot("mulplt.eps",10)
      endif

      call xset_color(color_def)

 998  continue

c
c   Open file for logging of extracting time windows in cont mode
c
      if ( seisan_logging.GE.1 ) then
       extunitlogfile(1:12)='mulplt.cont.'
       extunitlogfile(13:14)='20'              ! year
       extunitlogfile(15:16)=timestring(1:2)   ! year
       extunitlogfile(17:17)='-'
       extunitlogfile(18:19)=timestring(3:4)   ! mdr
       extunitlogfile(20:20)='-'
       extunitlogfile(21:22)=timestring(5:6)   ! day
       extunitlogfile(23:23)='-'
       extunitlogfile(24:27)=timestring(7:10)  ! hour min
       extunitlogfile(28:28)='-'
       extunitlogfile(29:30)=timestring(11:12) ! sec
       extunitlogfile(31:35)='.log'

       call sei open( unknown$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               extunitlogfile,           ! Filename.
     &               extunitlog,              ! Open on unit.
     &               b_flag,               ! Flag existance & usage?.
     &               code )                ! Condition.
      endif
c

c   Open output file for extracting time windows in cont mode
c
      if(.not.pc) then
      call sei open( unknown$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               'mulplt.ext',         ! Filename.
     &               extunit,              ! Open on unit.
     &               b_flag,               ! Flag existance & usage?.
     &               code )                ! Condition.
      else
      call sei open( unknown$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               'mulplt.ext.bat',     ! Filename.
     &               extunit,              ! Open on unit.
     &               b_flag,               ! Flag existance & usage?.
     &               code )                ! Condition.
      endif

c   Write output file for extracting time windows in cont mode
      rewind(extunittmp)
      if ( seisan_logging.GE.1 ) then
        write(extunitlog,*)
     &'This log file is output from SEISAN MULPLT continous mode.'
        write(extunitlog,*)
     &'Time of analysis: ','20',timestring(1:2),'-',timestring(3:4),
     &'-',timestring(5:6),' ',timestring(7:8),
     &':',timestring(9:10),':',timestring(11:12)
        read(extunittmp,'(a80)')text              ! station_code
        write(extunitlog,'(a80)')text
        read(extunittmp,'(a80)')text              ! component_code
        write(extunitlog,'(a80)')text
        read(extunittmp,'(a80)')text              ! high_pass,low_pass
        write(extunitlog,'(a80)')text
        read(extunittmp,'(a80)')text              ! max_count
        write(extunitlog,'(a80)')text
        read(extunittmp,'(a80)')text              ! max_line
        write(extunitlog,'(a80)')text
        read(extunittmp,'(a80)')text              ! line_time
        write(extunitlog,'(a80)')text
        read(extunittmp,'(a80)')text              ! height
        write(extunitlog,'(a80)')text
      endif
c
 997  read(extunittmp,'(a80)',end=996)text1
      read(text1,*)text,dummyx,dummyy
      if ( seisan_logging.GE.1 ) then
        write(extunitlog,'(a80)')text1
      endif
      if(text(1:3).eq.'pag') then
        line_start_time=dummyx
        call sectim(dummyx,year1,doy1,month1,day1,hour1,min1,sec1)
        write(text,'(i4,i2,i2,i2,i2,f8.5)')
     *  year1,month1,day1,hour1,min1,sec1
        write(extunitlog,993) text
      endif
c  
      if(text(2:3).eq.'P-'.OR.text(2:3).eq.'S-')then
        i=1
        do while(((710.-(710./max_line)*i)+30.0).ge.dummyy)
          i=i+1
        enddo
        dummyb=line_start_time+
     +(i-1)*line_time+dummyx/1024*line_time
        call sectim(dummyb,year1,doy1,month1,day1,hour1,min1,sec1)
        write(text,'(i4,i2,i2,i2,i2,f8.5)')
     *  year1,month1,day1,hour1,min1,sec1
        write(extunitlog,992) text1(1:12),text(1:20)
      endif
c
      if(text(1:3).eq.'beg'.or.text(1:3).eq.'end')then
      i=1
c determine the line the courser is in
c       do while(740-(740.-15.)/max_line*i.ge.dummyy)
        do while(((710.-(710./max_line)*i)+30.0).ge.dummyy)
          i=i+1
        enddo
      endif
c  
      if(text(1:3).eq.'beg')dummyb=line_start_time+
     +(i-1)*line_time+dummyx/1024*line_time
c   
      if(text(1:3).eq.'end') then
        dummye=line_start_time+
     +(i-1)*line_time+dummyx/1024*line_time-dummyb
c    
        call sectim(dummyb,year1,doy1,month1,day1,hour1,min1,sec1)
        write(text,'(i4,i2,i2,i2,i2,f5.2)')
     *year1,month1,day1,hour1,min1,sec1
        do i=1,17
          if(text(i:i).eq.' ')text(i:i)='0'
        enddo
        write(6,994)text,dummye
        write(extunit,994)text,dummye
        if ( seisan_logging.GE.1 ) then
          write(extunitlog,994)text,dummye
        endif
      endif
      goto 997
 996  continue
      call sei close( close$, extunittmp, code ) ! Close 
      call sei close( close$, extunit, code )    ! Close 
c
c Logging: cont mode analysis saved in REA/def_base/LOG/YEAR/MDR
c
      if ( seisan_logging.GE.1 ) then
        call sei close( close$, extunitlog, code )    ! Close
c move log file to REA/def_base/LOG/year/mdr
        logyear='2015'                   ! year of implementation
        read(timestring(1:2),'(a2)') logyear(3:4)
        read(timestring(3:4),'(a2)') logmdr
        call topdir(top_directory)
        call dir_char(dchar)
        call get_def_base(def_base)
        txt=top_directory(1:seiclen(top_directory))//dchar//'REA'
     +//dchar//def_base//dchar//'LOG'//dchar
     +//logyear//dchar//logmdr
c check if the log folder exist, if not create
        inquire(file=txt, exist=folder_exist)
        if ( .NOT.folder_exist ) then
          txt='mkdir -p '//
     +top_directory(1:seiclen(top_directory))//dchar//'REA'
     +//dchar//def_base//dchar//'LOG'//dchar
     +//logyear//dchar//logmdr
          if(pc) txt(1:9)='mkdir    '
          call systemc(txt,seiclen(txt))
        endif
c move log to log folder
c       if( pc ) then
          txt='mv   '//extunitlogfile(1:seiclen(extunitlogfile))
     +//' '//
     +top_directory(1:seiclen(top_directory))//dchar//'REA'
     +//dchar//def_base//dchar//'LOG'//dchar
     +//logyear//dchar//logmdr
          if(pc) txt(1:5)='move '
          call systemc(txt,seiclen(txt))
c       else
c         txt='mv '//extunitlogfile(1:seiclen(extunitlogfile))
c    +//' '//
c    +top_directory(1:seiclen(top_directory))//dchar//'REA'
c    +//dchar//def_base//dchar//'LOG'//dchar
c    +//logyear//dchar//logmdr
c         call systemc(txt,seiclen(txt))
c       endif
      endif
c end logging
c
 995  FORMAT(i4,a,i3,a,i2,a,i2,a,f5.2,1x,f15.4,1x)
 993  FORMAT(' PAGE_START:     ',a20)
 992  FORMAT(a12,'time:',a20)
 994  FORMAT('wavetool -start ',a14,' -duration ',f15.4,
     +' -cwav -wav_out_file SEISAN')

      return                                                                    
      end                                                                       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine plot_cont_frame
c
c   plot the frame etc for continous plotting
c
      implicit none
      save
      include 'mulplt.inc'	
c                                                                               
c   local variables                                                             
c                                                                               
                                                                                
c-- use for plotting                                            
      real	ix		
c-- plotting string                                      
      character*80 text		
      integer ntext			! WCC added
c-- number of secs between axis                             
      integer	delsec		
c-- seconds or minute tic counter                                       
      integer	isec,imin		
c-- number of seconds in plot                                 
      integer	nsec		
c-- number of samples which can be plotted                  
      integer	npoint		
      real y_upper                      ! position top frame
      real y_lower                      ! -------- BOTTOM---
      real tsize                        ! time tick size
      integer ihour                     ! for time scale
c-- counters                                                 
      integer	i,ktic		
      character*3 time_text
      double precision page_start_time
      integer year,month,doy,day,hour,min   ! header time
      real secc
      integer ipage
c
      include 'seiplot.inc'       ! for plotting routines                                                                               
c
c   communication with plot_cont
c
      common /plotcont/y_upper,y_lower,npoint,ipage,page_start_time

         call xset_color(color_frame)   ! frame color                                                                
c                                                                               
c   draw whole frame                                                            
c                                                                               
         call xmovabs(xpos,y_lower)                                                
         call xdrwabs(xpos,y_upper)                                         
         call xdrwabs(1023.0,y_upper)                                       
         call xdrwabs(1023.0,y_lower)                                              
         call xdrwabs(xpos,y_lower)                                                
c
c  calculate page start time
c
         call sectim(page_start_time,
     *   year,doy,month,day,hour,min,secc)
c
c   make title
c
         text=' '
         text(1:5)=wav_stat(current_chan)
         text(6:9)=wav_comp(current_chan)
         write(text(11:31),'(i4,1x,2i2,1x,2i2,1x,f6.3)') year,month,
     *   day,hour,min,secc
         write(text(34:43),'(a,i8)')'DC',int(dc_chan)
         write(text(46:59),'(a,i8)')'Scale:',max_count
c         text(65:67)='F Q'
         fsec=secc
         fmin=min + fsec/60.0
         fhour=hour +fmin/60.0
         write(text(77:80),'(a,i3)') 'P',ipage
         if(filt.gt.0) then
            write(text(62:75),'(a,2f6.3)') 'F:',flow(filt),fhigh(filt)
         endif
c                                                                               
c   plot header                                                            
c   
         call xchars(text,80,xpos,760.0)                       
c                                                                               
c   draw seconds tics and numbers, ticks are either at 1,2,5,                   
c   10 or more secs intervals                                                     
c                                                                               
c                                                                               
c   find distance in secs between tics assuming min 100 tectronics              
c   units between tics                                                          
c                                                                               
c-- ca distance between tics                           
         i=100.0/xscale				
c                                                                               
c  find distance between points to nearest 1,2,5,10 or 20 sec                   
c                                                                               
         if(i.le.1) delsec=1                                                    
         if(i.gt.1) delsec=2                                                    
         if(i.gt.2) delsec=5                                                    
         if(i.gt.5) delsec=10                                                   
         if(i.gt.10) delsec=20                                                  
         if(i.ge.20) delsec=60                                                  
         if(i.ge.60) delsec=120                                                 
         if(i.ge.120) delsec=300                                                
         if(i.ge.300) delsec=600                                                
         if(i.ge.600) delsec=1200                                             
         if(i.ge.1200) delsec=3600
         if(i.ge.3600) delsec=7200
                                                  
         call xset_color(color_axis_not)     !JAB(BGS)Mar95., from yellow

c-- number of secs of axis                            
         nsec=npoint/rate			
c-- first whole sec or min on axis                         
         isec=fsec/delsec+1			
         imin=fmin/(delsec/60.0)+1
         ihour=fhour/(delsec/3600.0)+1
c-- time of first tic                                 
         isec=isec*delsec			
         imin=imin*(delsec/60.0)
         ihour=ihour*(delsec/3600.0)
c-- x position first tic                     
         if(delsec.lt.60) ix=(isec-fsec)*xscale+xpos		
         if(delsec.ge.60) ix=(imin-fmin)*60*xscale+xpos		
         if(delsec.ge.3600) ix=(ihour-fhour)*3690*xscale+xpos		
c-- find second tic value below 60                   
         isec=mod(isec,60)			
         imin=mod(imin,60)
         ihour=mod(ihour,24)
         time_text='SEC'
         if(delsec.ge.60) then
            time_text='MIN'
         endif
         if(delsec.ge.3600) then
            time_text='HR '
         endif
         call tchars(time_text,3,950.0,y_lower+20.0)                              
         call pchars(time_text,3,950.0,y_lower+15.0)                              
c                                                                               
c   set symbol size, only for postscript                                        
c                                                                               
cc         if(hctype.eq.1) call fontsize(0,2.5)    
           call fontsize(0,2.5)            
c                                                                               
         do 5 i=1,nsec/delsec+1                                                 
            if(ix.gt.1010) goto 5                                               
c-- check if plot numbers
            paxisnumb=1                    
            if(paxisnumb.eq.1) then	
            	! NEATEN time axis
            	ntext=2
            	if (delsec.lt.60) then
            		if (isec.eq.0.or.i.eq.1) then
            			if (isec.lt.10) then
            				write(text,'(i2,a,i1)') imin,'m0',isec
            			else
            				write(text,'(i2,a,i2)') imin,'m',isec
            			endif
            			ntext=5
            		else
            			write(text,'(i2)') isec
            		endif
            	elseif (delsec.lt.3600) then
					if (imin.eq.0.or.i.eq.1) then
            			if (imin.lt.10) then
            				write(text,'(i2,a,i1)') ihour,'h0',imin
            			else
            				write(text,'(i2,a,i2)') ihour,'h',imin
            			endif
						ntext=5
					else
						write(text,'(i2)') imin
					endif
            	else
					write(text,'(i2)') ihour
				endif
            	            	
C               ktic=isec
C               if(delsec.ge.60) then
C                 ktic=imin
C               endif
C               if(delsec.ge.3600) then
C                 ktic=ihour
C               endif
C               write(text,'(i2)') ktic                                          
               call tchars(text,ntext,ix-10.0,y_lower-30)                  
               call pchars(text,ntext,ix-10.0,y_lower-15)                         
            endif                                                               
c                                                                               
c-- determine tic size                               
            tsize=10			      
            if(delsec.lt.60.and.(isec.eq.60.or.isec.eq.0).or.
     *         delsec.ge.60.and.(imin.eq.60.or.imin.eq.0)) then            
              tsize=20                                                          
c-- reset seconds  or minute counter            
c              isec=0       	! WCC takes care of below                   
c              imin=0
            endif                                                               
c-- plot marks at bottom
            call xmovabs(ix,y_lower)                                            
            call xdrwabs(ix,y_lower+tsize)                                      
c-- plot tics at top                   
            call xmovabs(ix,y_upper)                                     
            call xdrwabs(ix,y_upper-tsize)                               
            isec=isec+delsec    
            ! NEATEN time axis
            if (isec.ge.60) imin=imin+isec/60
            if (imin.ge.60) ihour=ihour+imin/60
			if (isec.ge.60) isec=isec-60*(isec/60)
			if (imin.ge.60) imin=imin-60*(imin/60)
			if (ihour.ge.24) ihour=ihour-24*(ihour/24)
             ix=ix+xscale*delsec                                                
 5       continue                                                               
         return
         end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine plot_cont_hardcopy(ipage)
c
c  send hard copy to printer in plot_cont
c
      implicit none
      save
      include 'mulplt.inc'	
      include 'seiplot.inc'       ! for plotting routines                                                                               
C
C    Seisan library details...
C    -------------------------
C
      INCLUDE   'libsei.inc'                   ! Definitions and data defns.
      external  sei open,                      ! Open file handler.
     &          sei close,                     ! Close file handler.
     &          sei code,                      ! Error handler.
     &          sei clen                       ! String length
      integer   sei clen                       ! Function.
c
      integer   code                           ! Local error condition.
      logical   b_flag                         ! Flag existance.?
c                                                                               
c   local variables                                                             
c                                                                               
c-- use for plotting                                            
      real	ix,iy		
      integer ipage                     ! page counter
      character*1 dummy                 ! help variable
      integer ich                       ! retuen from cursor
c
c   get up cursor
c
      if(plotoption.lt.2) then 
 10      continue
         call xscursr(ich,ix,iy)			
         if(char(ich).eq.'q'.or.char(ich).eq.'Q') then
            choice='QUIT'
            return
         endif 

c   Input Start and End times for extracting time windows in cont mode
c   or P and S readings (Local or Teleseismic):
         call mulplt_cont_picking(ich,ix,iy,height,extunittmp)
   
         if(char(ich).ne.'f') goto 10  ! not a valid char
      endif
c                                                                               
c   make hard copy plot                                                         
c                                                                               
      if(plotoption.gt.0) then                                               
c
c   close plot in post script file
c
          call close_post
          call sei close( close$, plotunit, code ) ! Close 
          dummy=' '    ! send plot by default
          if(plotoption.eq.1) then   ! only ask if screen plot
             call clear_to_alpha
             write(6,'(a,$)')' Submit plot (y=return/n) '                      
             read(5,'(a)') dummy             
          endif                                  
          if(dummy.eq.' '.or.dummy.eq.'Y'.or.dummy.eq.'y') 
     *    call send_plot("mulplt.eps",10)
          if(plotoption.eq.2) then     ! write that page sent
              write(6,*) ' Page sent to plotter',ipage
          endif
c
c  open plot file again
c
          call sei open( unknown$,         ! Open file.
     &                      ' ',           ! No prompt.
     &                      'mulplt.eps',  ! This file.
     &                      plotunit,      ! On this unit.
     &                      b_flag,        ! Existance.
     &                      code )         ! Condition not applicable.
       endif
c
      call open_display
      return
      end
***************************************************************************     
      subroutine convert(reference_time,station,compon_in,iswitch)                                                   
c                                                                               
c     Routine to convert between vectors with old/new picks                     
c     for plotting on the trace to/from S file. this is for station
c     station and component compon_in                                
c                                                                               
c     Written by C. Lindholm, Oct. -89                                          
c jul 23 by jh ; increase amo field to 7, change reading format to f7,1
c dec 93       : major rewrite
c                                                                               
c     If iswitch =-1    : info in data records converted into vecors, data 
c                         unchanged
c     If iswitch = 0    : info in data records are converted into vectors,
c                         data is stripped of the extracted data       
c     If iswitch = 1    : Info in vectors are converted into data records       
c                                                                               
c       
      implicit none
      save
      include 'mulplt.inc'
      include 'seisan.inc'
      integer seiclen
						
c-- Absolute time of filestart, either main or for one channel, if rotated,
c   add delay due to rotation 
      double precision reference_time			
c-- Absolute time of pick                          
      double precision pfsec			
c-- Station code, for which readings are wanted       
      character*5 station			
c-- Component code                               
      character*4 compon,compon_in
c-- Comp. code; S file type                      
      character*2 outcomp,comp			
c-- Station code                                      
      character*5 stacode			
c-- phase information                               
c-- Phase in file including onset (i or e), polarity and weight, 
c   is only normally 4 chars but can occupy all 8, then weight and polarity
c   cannot be given.                                      
      character*9 pphase			
c-- Timing values of event              
      integer year,month,doy,day,hour,min	
c-- Timing of pick                                     
      integer phour,pmin,pyear,pmonth,pday
c-- Seconds                                                
      real secc,psec				
c-- Additional information                            
      real amp,per,az,vel			
c-- Number of old phases                                   
c      integer nphas				
c-- Coda                                                    
      integer coda				
c-- real value of coda                                        
      real xcoda
c-- azimuth form station to event
      real azimu
c-- first p
      real first_p
c-- index of first p
      integer first_p_index				
c-- counters                                      
      integer kkk,nkj,jjj			
c-- Counters                                             
      integer i,j,k,ipow				
      integer ichannel        ! channel number
      integer kspec           ! line number to write spectrum
c-- See  'indata'                     
      integer nnrecord		
c-- flag for records taken out for plotting                           
       logical*1 record(max_data)
c-- Switch; see above                                    
      integer iswitch				
c
c   check that any data
c
c      if((nrecord-nhead).lt.2) return                                                           
      j = 0
      jjj=0                                                                     
      k = 0                                                                     
      do i=1,nrecord
         record(i)=.true.
      enddo                      
c
      compon=compon_in    ! use local variable since it might be changed
c
c   this could be a rotated component in which case the component name
c   must be changed, first find channel number in waveform channel list
c
      call wav_find_chan(station,compon,ichannel)

      if(wav_rot_comp(ichannel).ne.' ') 
     *compon(4:4)=wav_rot_comp(ichannel)                            
c
      ffsec=reference_time+wav_rot_delay(ichannel)                             
                                                                                
c
cccccc   Iswitch = 0  ;  Read into vectors for plotting   ccccccccccccc                      
c                                                                                
        if(iswitch.eq.0.or.iswitch.eq.-1)then                                               
c                                                                               
cccccc   blank earlier variables    ccccccccc                                   
c                                                                               
         nphas = 0                                                              

         sdistance=0
         edistance=0
         edepth=0
         do i = 1,max_phase                                                    
           amplitude(i) = 0.
           alt_weight(i)=' '
           period(i)    = 0.                                                    
           azimuth(i)   = 0.                                                    
           velocity(i)  = 0.                                                    
           phase(i)     = '         '                                          
           data_end(i)  = '                       '
         enddo                                                                  
c
c   get nordic file header year, month,day
c
         read(data(1),'(1x,i4,1x,2i2)') pyear,pmonth,pday
c                                                                               
cccccc   Read the data   ccccccccccccccccccccc                                  
c                      
      do 22 i = (nhead+1),nrecord                                               
        read (data(i),50,err=8484)stacode,comp,pphase,                         
     +      phour,pmin,psec,coda,amp,per,az,vel                                 
50      format(1x,a5,a2,1x,a9,i2,i2,f6.3,1x,i4,g7.0,                
     +        1x,f4.0,1x,f5.0,1x,f4.0)
        goto 8485
 8484   continue   ! check for error added by jh 15-6-12
        write(6,*)' Something wrong with S-file'
        write(6,'(a)') data(i)(1:79)
        write(6,*)' All observations not read'
        goto 22
 8485   continue
c
c   do not save synthetic readings
c
         if(pphase(1:1).eq.'Y') record(i)=.false.
c                                                                               
ccccc   Search for existing readings from that station    ccccccccc             
c                                                                               
         call component(compon,outcomp) ! convert 4 char compon to 2 char outcomp
c                                        
         if(stacode.eq.station) then
           if(data(i)(71:75).ne.'     ') then
              read(data(i)(71:75),'(f5.0)') edistance   ! for spectra            
              sdistance=edistance      
c
c   calculate hypocentral distance

              read(data(1)(39:43),'(f5.1)') edepth
              sdistance=sqrt(edistance*edistance+edepth*edepth)
           endif
         endif

         if(stacode.eq.station) then  
c   check 2 letter component code
           if(outcomp.eq.comp.or.pphase(1:1).eq.'Y') goto 778
c   check just orientation, uncomment line below
c           if(outcomp(2:2).eq.comp(2:2).or.pphase(1:1).eq.'Y') goto 778
           goto 779 
 778       continue
           j = j + 1                                                            
           record(i)=.false.
           call timsec(pyear,pmonth,pday,phour,pmin,psec,pfsec)
           pictim(j) = pfsec - ffsec
           alt_weight(j)=data(i)(9:9)
           phase(j) = pphase                                               
           amplitude(j) = amp                                                   
           period(j) = per                                                      
           azimuth(j) = az                                                      
           velocity(j) = vel

           data_end(j)=data(i)(57:79)
         endif
 779     continue      ! do not match
c
c  find any readings for this station, to be used with identification
c  of type of spectrum, assume all channels start at the same time for
c  one station
c
         if(stacode.eq.station) then
           jjj = jjj + 1                                                            
           call timsec(pyear,pmonth,pday,phour,pmin,psec,pfsec)
           spec_tim(jjj) = pfsec - ffsec
           spec_phs(jjj) = pphase  
        endif


c                                                                               
ccccc   Search for existing coda from that station    ccccccccc                 
c                                                                               
c        if((stacode.eq.station) .and. (outcomp(2:2).eq.comp(2:2)))then ! why this ? jh 050216                  
         if((stacode.eq.station) .and. (outcomp(1:2).eq.comp(1:2)))then                   
           if(coda.ne.0)then                                                    
             j = j + 1                                                          
             pictim(j) = pfsec - ffsec + coda                                  
             phase(j) = 'CODA  '                                                
           endif                                                                
         endif                                                                  
22    continue ! end of loop for checking if any reading fits this stat-comp                                                                  
                                                                                
      nphas = j
      spec_nphas=jjj
                                                                 
c
c   return here if data extracted =only for display purposes
c
      if(iswitch.eq.-1) return
c                                                                               
c removing the extracted info from data, later added if not deleted by pic     
c
      k=nhead
      do i=nhead+1,nrecord
         if(record(i)) then
           k=k+1
           data(k)=data(i)               ! shift data to use
         endif
      enddo        
      nnrecord=k                         ! save new number of records         
c
c   blank remaining data
c
      do i=nnrecord+1,nrecord
         data(i)=' '
      enddo
      nrecord=nnrecord                 ! this is now new number of records
c                                                                                
c-- end iswitch = 0                                   
c
      endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                                                                                
cccccc   Iswitch = 1  ;  Read into data records   ccccccccccccc                 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if(iswitch.eq.1)then                                               
      if(nphas .eq. 0) go to 5555  ! there could be a spectrum                                                   
c                                                                               
c  Search for coda                                          
c                                                                               
      do 55 i = 1,nphas                                                         
        if(phase(i).eq.'CODA  ')then                                            
           xcoda = pictim(i)                                                    
           go to 20                                                             
        endif                                                                   
55    continue                                                                  
                                                                                
c-- No coda found                                              
      go to 77					
                                                                                
c                                                                               
c  Search for first P-phase for coda if coda exists 
c               
20    continue
      first_p=10000.0
      do 66 i = 1,nphas                                                         
        if(phase(i)(2:2).eq.'P'.and.phase(i)(1:1).ne.'Y') then                       
           if(pictim(i).lt.first_p)then                                        
              first_p=pictim(i)                                                
              first_p_index = i                                                
           endif
        endif                                                                   
66    continue                                                                  
      coda = int(xcoda-first_p)                                                       
                                                                                
c                                                                               
ccccc    Now fill character string    cccccccccc                                
c                                                                               
                                                                                
77    continue					                                                             
                                                                                
      do 222 i = 1,nphas                                                        
c                                                                               
cccccc   Change to upper case characters for first char, see if it works                                        
c                                                                               
c-- Upper case
c                                              
         do nkj=1,1						
          if(ichar(phase(i)(nkj:nkj)).gt.96)then                               
            kkk=ichar(phase(i)(nkj:nkj))                                       
            kkk=kkk-32                                                          
            phase(i)(nkj:nkj)=char(kkk)                                         
          endif                                                                 
        enddo                                  
        if(phase(i)(1:1).eq.' '.or.phase(i)(1:1).eq.'I'.or.                     
     +    phase(i)(1:1).eq.'E')then                   
          pfsec = ffsec + pictim(i)                                             
          call sectim(pfsec,year,doy,month,day,hour,min,secc)                   
c
c   check if day  is next day relative to readings header, if so add 24 hours
c
           if(day.gt.pday.or.month.gt.pmonth.or.year.gt.pyear)
     *     hour=hour+24
           call component(compon,outcomp)
           if (seiclen(outcomp).le.0.or.seiclen(station).le.0) then
             write(*,*) ' Warning: no component or station code '
             goto 222
           endif
           if(high_accuracy) then   ! seconds written to 1 ms
              write(data(nrecord),201)station,outcomp,phase(i),                   
     +                       hour,min,secc                 
201           format(1x,a5,a2,1x,a9,i2,i2,f6.3)
           else
              write(data(nrecord),200)station,outcomp,phase(i),                   
     +                       hour,min,secc                 
200           format(1x,a5,a2,1x,a9,i2,i2,1x,f5.2)
           endif

           if(i.eq.first_p_index.and.coda.gt.0) then ! write coda              
               write(data(nrecord)(30:33),'(i4)')coda                          
           endif
c
c   check size of amplitude                                                 
c        
          if(amplitude(i).ne.0..and.amplitude(i).lt.100000.)                   
     +    write(data(nrecord)(34:45),'(f7.1,1x,f4.1)')
     +    amplitude(i),period(i)
          if(amplitude(i).ne.0..and.amplitude(i).ge.1.0e5
     +       .and.amplitude(i).lt.1.0e7)then
             amplitude(i)=amplitude(i)/1.0e4                 
             write(data(nrecord)(34:45),'(f5.1,a,1x,f4.1)')
     +       amplitude(i),'e4',period(i)
          endif
          if(amplitude(i).ne.0..and.amplitude(i).ge.1.0e7
     +       .and.amplitude(i).lt.1.0e9)then
             amplitude(i)=amplitude(i)/1.0e6                 
             write(data(nrecord)(34:45),'(f5.1,a,1x,f4.1)')
     +       amplitude(i),'e6',period(i)
          endif
          if(amplitude(i).ne.0..and.amplitude(i).ge.1.0e9
     +       .and.amplitude(i).lt.1.0e11)then
             amplitude(i)=amplitude(i)/1.0e8                 
             write(data(nrecord)(34:45),'(f5.1,a,1x,f4.1)')
     +       amplitude(i),'e8',period(i)
          endif
          if(period(i).lt.10.0.and.amplitude(i).gt.0.0)
     *         write(data(nrecord)(42:45),'(f4.2)')
     *        period(i)
          if(amplitude(i).ge.1.0e11) then
              call clear_to_alpha
              write(6,*)' Amplitude larger than 100 meters, unrealistic'
              stop
           endif

          if(azimuth(i).ne.0.)then                                             
c
c   check that velocity is not too large, jh oct 2006
c
             if(velocity(i).gt.99.0) velocity(i)=99.9
             write(data(nrecord)(47:56),'(f5.1,1x,f4.1)')
     +       azimuth(i),velocity(i)
          endif
c
c   check if weight should be put in column 9 in case of long phase names
c   in which case alt_weight has the weight
c
          if(alt_weight(i).ne.' ') then
             data(nrecord)(9:9)=alt_weight(i)
             alt_weight(i)=' '
          endif
c
c   put in end of record with distance  from previous run, coment
c   if not wanted
c
          data(nrecord)(57:79)=data_end(i)
          nrecord=nrecord+1
      endif
222   continue                                                                  
c                                                                                
c-- last blank record                              
c
      data(nrecord) = '   '				
c
c-------------------------------------------------------------
c  spectral parameters
c--------------------------------------------------------------
c
 5555 continue
c
c   add spectral values if any
c
      if(omega0.ne.0.0) then
c
c   check if spectrum already there , jh may 27,2001
c
         kspec=0
         do i=1,nhead
            if(data(i)(2:5).eq.'SPEC'.and.
     *       data(i)(11:14).eq.compon.and.
     *       data(i)(80:80).eq.'3'.and.
     *       (data(i)(7:10).eq.station(1:4).or.
     *        data(i)(6:10).eq.station(1:5))) then  ! lo 5/11/2010
              if (data(i+1)(2:5).eq.'SPEC'.and.
     *          data(i+1)(11:14).eq.compon.and.
     *          data(i+1)(41:41).eq.spec_phase.and.
     *          data(i+1)(80:80).eq.'3'.and.
     *         (data(i+1)(7:10).eq.station(1:4).or.
     *          data(i+1)(6:10).eq.station(1:5))) then  ! lo 5/11/2010
                kspec=i   ! save line number
              endif
            endif
c           if(data(i)(2:5).eq.'SPEC'.and.data(i)(7:10).eq.
c    *      station(1:4).and.data(i)(11:14).eq.compon.and.
c    *      data(i+1)(2:5).eq.'SPEC'.and.data(i+1)(7:10).eq.
c    *      station(1:4).and.data(i+1)(11:14).eq.compon.and.
c    *      data(i+1)(41:41).eq.spec_phase.and.data(i)(80:80).
c    *      eq.'3'.and.data(i+1)(80:80).eq.'3') kspec=i   ! save line number
         enddo
c
c   make room for new if not there before
c
         if(kspec.eq.0) then
            do i=nrecord,2,-1          ! move existing info down 2 lines
               data(i+2)=data(i)
            enddo
            nrecord=nrecord+2
            nhead=nhead+2
            kspec=2
         endif
c
c   calculate start time for spectrum
c
         pfsec = ffsec + stime                                             
         call sectim(pfsec,year,doy,month,day,hour,min,secc)                   
c
c   check for overflow
c
         if(moment.gt.999.0) moment=999.9
         if(sdrop.gt.999.0) sdrop=999.9
         if(omega0.gt.999.0) omega0=999.9
         if(abs(sslope).gt.999.0) sslope=999.9
         if(abs(travel_time).gt.999.0) travel_time=999.9
         if(geo_distance.gt.99999.) geo_distance=99999.0
         if(radius.gt.999.9) radius=999.99
         if(swindow.gt.999.9) swindow=999.9
c
c   write values
c
c        write(data(kspec),300)
c    *   station(1:4),compon,moment,sdrop,omega0,
c    *   radius,sslope,swindow,mw

         if (seiclen(station).lt.5) then
           write(data(kspec),300)
     *   station(1:4),compon,moment,sdrop,omega0,
     *   radius,sslope,swindow,mw
         else
           write(data(kspec),305)
     *   station(1:5),compon,moment,sdrop,omega0,
     *   radius,sslope,swindow,mw
         endif
c lo 5/11/2010 add 5 char stations
305      format(' SPEC',a5,a4,' MO',f5.1,' ST',f5.1,' OM',f5.1,
     *   ' f0',5x,' R',f6.2,' AL',f5.2,' WI',f5.1,' MW',f5.1)

300      format(' SPEC ',a4,a4,' MO',f5.1,' ST',f5.1,' OM',f5.1,
     *   ' f0',5x,' R',f6.2,' AL',f5.2,' WI',f5.1,' MW',f5.1)
         data(kspec)(80:80)='3'
         if(cornerf.lt.1.0) write(data(kspec)(42:46),'(f5.3)') cornerf
         if(cornerf.lt.10.0.and.cornerf.ge.1.0) 
     *   write(data(kspec)(42:46),'(f5.2)') cornerf
         if(cornerf.ge.10.0.and.cornerf.lt.100.0) 
     *   write(data(kspec)(42:46),'(f5.1)') cornerf
         if(cornerf.ge.100.0) write(data(kspec)(42:46),'(f5.0)') cornerf
         if(sdrop.lt.1.0) write(data(kspec)(26:30),'(f5.3)') sdrop
         if(radius.lt.10.0) write(data(kspec)(49:54),'(f6.4)') radius
         if(omega0.lt.1.0) write(data(kspec)(34:38),'(f5.3)') omega0 
         if(omega0.lt.10.0) write(data(kspec)(34:38),'(f5.2)') omega0

         if (seiclen(station).lt.5) then
           write(data(kspec+1),301)
     *   station(1:4),compon,hour,min,int(secc),kappa,  ! kappa instad of ttime
c    *   station(1:4),compon,hour,min,int(secc),int(travel_time),
     *   int(geo_distance),
     *   spec_velocity,density,q0,qalpha,q_below_1Hz
         else
           write(data(kspec+1),306)
     *   station(1:5),compon,hour,min,int(secc),kappa,  ! kappa instad of ttime
c    *   station(1:4),compon,hour,min,int(secc),int(travel_time),
     *   int(geo_distance),
     *   spec_velocity,density,q0,qalpha,q_below_1hz
         endif

306      format(' SPEC',a5,a4,' T',3i2,' K ',f5.3,' GD',
     *   I5,' V ',f5.2,
     *   ' DE',f5.2,' Q0',f5.1,' QA',f5.2,' Q1',f5.2)
         data(kspec+1)(41:41)=spec_phase  ! put in which phase has been used
         data(kspec+1)(80:80)='3'


c        write(data(kspec+1),301)
c    *   station(1:4),compon,hour,min,int(secc),kappa,  ! kappa instad of ttime
cold *   station(1:4),compon,hour,min,int(secc),int(travel_time),
c    *   int(geo_distance),
c    *   spec_velocity,density,q0,qalpha,svelocity
301      format(' SPEC ',a4,a4,' T',3i2,' K ',f5.3,' GD',
     *   I5,' V ',f5.2,
     *   ' DE',f5.2,' Q0',f5.1,' QA',f5.2,' Q1',f5.2)
         data(kspec+1)(41:41)=spec_phase  ! put in which phase has been used
         data(kspec+1)(80:80)='3'
c
c   make sure no rewrite for next channel
c
         omega0=0.0
      endif
c
c   if output of spectrum for surface wave inversion, put it
c   out here
c
      if(spec_out.eq.-1) then
c
c   find azimuth
c
         do i=nhead+1,nrecord
            if(station.eq.data(i)(2:6)) 
     *      read(data(i)(77:79),'(f3.0)') azimu
         enddo
c
c   read old values
c
              
         open(25,file='com_spec.out',status='old',err=3333)
         read(25,*)ipow,rate,swindow
         read(25,'(4e15.8)')(com(i),i=1,ipow/2+1)
c
c   rewrite all
c
         rewind 15
         k=swindow*rate
         write(25,'(a5,a4,a20)') station,compon,data(1)(1:20)
         write(25,'(3i5,3e20.13)') ipow,ipow/2+1,
     *   k,sdistance,sdistance/111.2,180.0-azimu
         write(25,'(3e20.13)')azimu,travel_time,1.0/rate
         write(25,'(4e15.8)')(com(i),i=1,ipow/2+1)
         close(25)
         spec_out=1      ! reset parameter
      endif
 3333 continue
      
c
c  if this is a run without data base, put in time in header from
c  channel header currently used, no check on change of day !!!!!!!!
c
cfix   why here, already in main ????
c
      if(opmode.ne.0.and.data(1)(2:10).eq.'         ') then
         write(data(1)(2:5),'(i4)') wav_year(wav_first)
         write(data(1)(7:20),'(2i2,1x,2i2,f5.1)') wav_month(wav_first),
     *   wav_day(wav_first),wav_hour(wav_first),wav_min(wav_first),
     *   wav_sec(wav_first)
         data(1)(22:22)='L'
         nhead=1
      endif
                                                                                
c-- end iswitch = 1                                   
      endif				
                                                                                
      return                                                                    
      end
c                                                                       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine pick_menu_box(on,nx,xsize,ysize,mode,
     *     x0,y0,xin,yin,choise,mouse,show_menu,flow,fhigh)
c
c   displays a series of boxes, by clicking with the mouse
c   the values can be set or unset, show nby inverse video. 
c   In and out definition below 
c
c   the mouse click happens outside routine. after an outside mouse click, the 
c   routine is called and all buttons on form outside (variabel on) are highlighed.
c   then the muse click is higlighted which can be turning on button on or off.
c   the routines the return to wait for a possible new mouse click from outside.
c
C Feb 97         : first version
C
      implicit none
      save
      include 'seiplot.inc'
      include 'seidim.inc'
c
c   call parameters
c
      integer on(*)      ! 0: box not selected, 1:box selected
      integer nx         ! --------------- in x and y directions
      real xsize,ysize   ! box size
      character*6 mode   ! single or multiple mode
      real x0,y0         ! upper left hand cornor of boxes
	  real xin,yin       ! position in from mouse in
      integer choise     ! character code for choise returned, it is char pushed
      integer mouse      ! mouse definition, from mulplt.inc
      character*1 ch(50) ! character for choise
      integer nbox       ! number of boxes
      character*80 text(50)   ! text in box
      integer  ntext         ! number of characters to plot
      real x1(50),y1(50),
     *x2(50),y2(50)          ! cornors in squares of boxes
      real del,x,y
      integer ix,i,ib
      integer show_menu      ! save if show menu
      integer color_in       ! color when routine called
      integer color_fr       ! color of frame
      integer hc_old         ! hard copy type
      integer plotoption_old ! save plot option
c-- lower cutoff frequency of fixed filters (bjb 2001/02/13)             
       real	flow(9)		
c-- higher cutoff frequency of fixed filters    (bjb 2001/02/13)              
       real	fhigh(9)	

c
c   return if not a mouse click
c  
c       write(*,*) ' menu box ich ',choise,mouse
       if(choise.ne.mouse) return

c   return if not in box section, assume two rows of boxes,
c   however, if y is 999 assume first time so plot
c
      x=del+ysize
      if(mode.ne.'single') x=2*x   ! to position bottom of boxes
      if(yin.eq.999.0) then
         continue
      elseif(yin.lt.y0-x) then
         return
      endif
c
c    Initialise...
c    =============
c
      color_in=color_current ! save current color
c
c   save hc setting, since hc must be turned off in this routine
c
cc      hc_old=hctype
      plotoption_old=plotoption
cc      hctype=0
      plotoption=0
      x=x0
      y=y0
      del=9.4        ! distance between boxes
c      del=5.0        ! distance between boxes
      on(14)=show_menu   ! use previous on state
c
c   5 chars in each box
c
c       ntext=5
c  increased to 9 bjb 2001/02/13
       ntext=9

       if(mode.eq.'single') then
         nbox=28
       else
         nbox=42
       endif
c
c   set all titles
c
       
       do i=1,nbox
          text(i)='     '
cjh          ch(i)=char(1)   !   why   ?
          ch(i)=' '
       enddo

       text(28)=' Plot'
       ch(28)='r'
       text(27)=' Next'
       ch(27)='f'
       text(26)='Toggl'
       ch(26)='t'
       text(25)='Quit'
       ch(25)='q'
       text(24)=' Help'
       ch(24)='?'
       text(23)='Oth C'
       ch(23)='o'
c
       text(1)=' Filt'
       ch(1)='.'

c
c define filter text depending on filters given in MULPLT.DEF. Also allowing
c for LP, HP or BP filters bjb 2001/01/13
c
       do i=2,8
          if (flow(i-1).eq.0.0.and.fhigh(i-1).ne.0.0) then
             write(text(i),'(a3,f4.1)') 'LP ',fhigh(i-1)
          else if(flow(i-1).lt.0.0.and.fhigh(i-1).ne.0.0) then
             write(text(i),'(f4.1,a1,f4.1,a1)') -1*flow(i-1),
     1            '-',fhigh(i-1),'R'
          else if(fhigh(i-1).eq.0.0.and.flow(i-1).ne.0.0) then
             write(text(i),'(a3,f4.1)') 'HP ',flow(i-1)
          else
             text(i)=' '
             text(i)(3:3)='-'
             if(flow(i-1).ge.10.0) 
     *          write(text(i)(1:2),'(i2)') int(flow(i-1))
             if(flow(i-1).ge.1.0.and.flow(i-1).lt.10.0) 
     *          write(text(i)(1:2),'(i2)') int(flow(i-1))
             if(flow(i-1).lt.1.0.and.flow(i-1).ge.0.1) 
     *          write(text(i)(1:2),'(f2.1)') flow(i-1)
             if(flow(i-1).lt.0.1) then
                write(text(i)(1:2),'(i2)') int(flow(i-1)*100+0.1)
                if(text(i)(1:1).eq.' ') text(i)(1:1)='0'
             endif
             if(fhigh(i-1).ge.10.0) 
     *          write(text(i)(4:5),'(i2)') int(fhigh(i-1))
             if(fhigh(i-1).ge.1.0.and.fhigh(i-1).lt.10.0) 
     *          write(text(i)(4:5),'(i2)') int(fhigh(i-1))
             if(fhigh(i-1).lt.1.0) 
     *          write(text(i)(4:5),'(f2.1)') fhigh(i-1)
          endif
       enddo



c       text(2)='01-.1'
c       ch(2)='z'
c       text(3)='.1-1.'
c       ch(3)='x'
c       text(4)='1 - 5'
c       ch(4)='v'
c       text(5)='2 - 4'
c       ch(5)=';'
c       text(6)='5-10'
c       ch(6)='b'
c       text(7)='10-15'
c       ch(7)='n'
c       text(8)='15-23'
c       ch(8)='m'
c
c problem with filter coefficients solved by modifying keystrokes
c
c       text(2)='01-.1'
       ch(2)='z'
c       text(3)='.1-1.'
       ch(3)='x'
c       text(4)='1 - 5'
       ch(4)='v'
c       text(5)='2 - 4'
       ch(5)='b'
c       text(6)='5-10'
       ch(6)='n'
c       text(7)='10-15'
       ch(7)='m'
c       text(8)='15-23'
       ch(8)=';'

       text(9)= ' WA '
       ch(9)='w'
       text(10)=' mb '
       ch(10)='j'
       text(11)=' mB '  
       ch(11)='J'
       text(12)=' Ms '  
       ch(12)='k'
       text(13)=' MS ' 
       ch(13)='K'
       text(14)=' MENU'
       text(21)='Groun'
       ch(21)='g'
       text(22)='Back '
       ch(22)='B'
       text(19)='FixF '
       ch(19)=','
       text(20)='Rotat'
       ch(20)='U'
       if(mode.eq.'single') then
         text(15)=' Azim'
         ch(15)='h'
         text(16)=' Resp'
         ch(16)=':'
         text(17)=' Spec'
         ch(17)='s'
         text(18)='AllC '
         ch(18)='y'
       else
         text(15)='Regis'
         ch(15)='p'
         text(16)='Locat'
         ch(16)='l'
         text(17)=' Dist'
         ch(17)='_'
         text(18)='Scale'
         ch(18)='*'
         text(42)='Print'
         ch(42)='>'
         text(34)=' FK  '
         ch(34)='F'
         text(35)='NextW'
         ch(35)=char(9)
         text(29)='Del W'
         ch(29)='d'
         text(30)='Del S'
         ch(30)='D'
         text(31)='Merge'
         ch(31)='M'
         text(32)=' Out '
         ch(32)='O'
         text(33)='Iasp '
         ch(33)='I'
         text(21)='Groun'
         ch(21)='g'
         text(36)='Oth W'
         ch(36)='W'
         text(37)=' <W> '
         ch(37)='Z'
         text(38)=' >W< '
         ch(38)='X'
         text(39)='PartM'
         ch(39)='P'
         text(40)='AllC '
         ch(40)='y'
         text(41)='OutW'
         ch(41)='T'
       endif
c
c   indicate that color changes come from this routine, since
c   black and white must be togled when changing selection
c
	 from_input_box=1
c
c   draw, the frame must always contrast the background, so it is either
c   black or white, if box 14 not on, only make 14
c
	 color_fr=xblack
	 if(color_screen.eq.0) then    ! no color screen
	    if(color_back.eq.xblack) color_fr=xwhite
	    if(color_back.eq.xwhite) color_fr=xblack
	 endif
c
c   clear area if not only menu button
c
      if(on(14).ne.0) then
         call xset_color(color_back)
         call fillbox(0.0,y0-del-ysize-5.0,1024.0,780.0)
      endif

 300  continue   ! jump here from below if menu button pushed to draw all
                 ! or to display only menu botton

c
c   draw boxes...
c
      x=x0
      y=y0
      ix=1
c
c
      do ib=1,nbox
c
c   corners in box
c
  	    x1(ib)=x
	    y1(ib)=y
	    x2(ib)=x+xsize
	    y2(ib)=y+ysize
c
            if(on(14).ne.1.and.ib.ne.14) goto 500   ! only plot box 14
c
	    call xset_color(color_fr)
	    call xmovabs(x1(ib)-3,y1(ib)-3)
	    call xdrwabs(x2(ib)+3,y1(ib)-3)
	    call xdrwabs(x2(ib)+3,y2(ib)+3)
	    call xdrwabs(x1(ib)-3,y2(ib)+3)
	    call xdrwabs(x1(ib)-3,y1(ib)-3)
c
c   -------------------------------
c   Allocate and set the colours...
c   -------------------------------
c   Display boxes on or off...
c   --------------------------
c   On...
c
            if(ib .le. nbox ) then                       ! A valid box.
	       if( on(ib) .eq. 1 ) then                   ! Is on.
	          call xset_color(color_box_letter)          ! Interior fill colour.
	          call fillbox(x1(ib),y1(ib),x2(ib),y2(ib)) ! & fill.
	          call xset_color(color_box)                 ! Text colour.
	          call xchars(text(ib),ntext,x1(ib)+0.0,     ! Write text.
     &                              y2(ib)-15.0)    !
	          call xset_color(color_box)                 ! Write shortcut
	          call xchars(ch(ib),1,x2(ib)-8.0,     ! Write text.
     &                              y2(ib)-15.0)    !
c
c   Off...
c
	       else                                       ! Otherwise off.
	          call xset_color(color_box)                 ! Interior fill colour.
	          call fillbox(x1(ib),y1(ib),x2(ib),y2(ib)) ! & fill.
	          call xset_color(color_box_letter)          ! Text colour.
	          call xchars(text(ib),ntext,x1(ib)+0.0,     ! Write text.
     &                              y2(ib)-15.0)    !
	          call xset_color(color_box_letter)                 ! Write shortcut
	          call xchars(ch(ib),1,x2(ib)-8.0,     ! Write text.
     &                              y2(ib)-15.0)    !
	       end if                                     !
            else                                               ! Otherwise off.
               call xset_color(color_box)                         ! Interior fill colour.
               call fillbox(x1(ib),y1(ib),x2(ib),y2(ib))         ! & fill.
               call xset_color(color_box_letter)                  ! Text colour.
               call xchars(text(ib),ntext,x1(ib)+0.0,y2(ib)-15.0) ! & write text.
	          call xset_color(color_box_letter)                 ! Write shortcut
	          call xchars(ch(ib),1,x2(ib)-8.0,     ! Write text.
     &                              y2(ib)-15.0)    !
            end if                                             !
c
 500    continue
c
c---------------------------------
c
c   check if box on other line
c
	    ix=ix+1
	    x=x+xsize+del
	    if(ix.gt.nx) then
	       ix=1
	       x=x0
	       y=y-ysize-del
	    endif
      enddo                           ! finished initial plot of all boxes
c
c   check if a click in a box
c
c     x=xin
c     y=yin
      do i=1,nbox
        if(on(14).ne.1.and.i.ne.14) goto 600   ! only box 14
c 	    if(x.gt.x1(i).and.x.lt.x2(i).and.y.gt.y1(i).and.y.lt.y2(i))
        if(xin.gt.x1(i).and.xin.lt.x2(i).
     *  and.yin.gt.y1(i).and.yin.lt.y2(i))
     *  then
                    
	    if( on(i) .eq. 0 ) then      ! Turn on.
  	       on(i)=1
c
c   if menu, plot whole menu
c         
              if(i.eq.14) then
c                  write(*,*) ' switching menu on xxx '
                  xin=0         ! make sure not selected again
                  yin=0
c                  choise=1
                  choise=0
   	          call xset_color(color_back) ! clear menu area
                  call fillbox(0.0,y0-del-ysize-5.0,1024.0,780.0)
                  goto 300      ! start again to make all
              endif
c
c  make sure 37 and 38 not on at the same time
c
              if(i.eq.37.and.on(38).eq.1) on(37)=0
              if(i.eq.38.and.on(37).eq.1) on(38)=0
c
c   set choise
c
              if(on(i).eq.1) then
              choise=ichar(ch(i))
	      call xset_color(color_box_letter)
	      call fillbox(x1(i),y1(i),
     *                    x2(i),y2(i))
	      call xset_color(color_box)
              call xchars(text(i),ntext,x1(i)+0.0,y2(i)-15.0)
	          call xset_color(color_box)                 ! Write shortcut
	          call xchars(ch(i),1,x2(i)-8.0,     ! Write text.
     &                              y2(i)-15.0)    !
              endif
c
c   if on is 1 to 11, make sure only one is on in that range
c
              if(i.le.11) then
               do ib=1,11
                 if(ib.ne.i) then
                   on(ib)=0
  	               call xset_color(color_box)                 ! Interior fill colour.
	               call fillbox(x1(ib),y1(ib),x2(ib),y2(ib)) ! & fill.
	               call xset_color(color_box_letter)          ! Text colour.
	               call xchars(text(ib),ntext,x1(ib)+0.0,     ! Write text.
     &                              y2(ib)-15.0)    !
	          		call xset_color(color_box_letter)         ! Write shortcut
	          		call xchars(ch(ib),1,x2(ib)-8.0,     ! Write text.
     &                              y2(ib)-15.0)    !
                 endif
               enddo
              endif
c
c    Turn an "on" box "off"...
c    -------------------------
c
	    else                              !
  	       on(i)=0
               if(i.lt.8) then
                  choise=ichar(',')           ! cancel filter
               else
c                  choise=1    ! no choise
                  choise=0    ! no choise
               endif
c
c   if menu turned off, clear and redraw only menu
c
           if(i.eq.14) then
c             write(*,*) ' turning menu on/off xxx '
  	       call xset_color(color_back)
c              call fillbox(0.0,y0-del-ysize-5.0,1024.0,780.0)
             call fillbox(0.0,y0-2*del-2*ysize-5.0,1024.0,780.0)
              xin=0
              yin=0
              goto 300   ! draw menu button
           endif
c		   
c    now turn off one of the others
c
               call xset_color(color_box)
	       call fillbox(x1(i),y1(i),
     *                    x2(i),y2(i))
	       call xset_color(color_box_letter)
	       call xchars(text(i),ntext,x1(i)+0.0,y2(i)-15.0)
	          call xset_color(color_box_letter)    ! Write shortcut
	          call xchars(ch(i),1,x2(i)-8.0,     ! Write text.
     &                              y2(i)-15.0)    !
	    endif
 	   endif                             !
 600   continue
      enddo                    ! finished turning boxes on or off
c
c
 99   continue
c
      from_input_box=0
c
c   reset color and hc type
c
      if( color_in .eq. 0 ) then                ! No colour installed.
         call xset_color(color_def)                ! Put in default.
      else                                      ! Otherwise.
         call xset_color(color_in)                 ! Put back colour.
      end if                                    !
c
c   turn off temporary choices
c
      do i=23,28
        on(i)=0
      enddo
c
      show_menu=on(14)  ! save menu setting
c
c   if command is to plot, turn off all
c
      if(char(choise).eq.'r') then
        do i=1,nbox
           on(i)=0
        enddo
      endif
cc      hcype=hc_old
      plotoption=plotoption_old

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine new_resp(chead_new)
c
c  this routine is currently not used, is kept since it might be used in future
C
C   J. HAVSKOV    dec 97
c
c   input chead :     header of original file ( through mulplt.inc)
c   output chead_new: header of new file
C
c   informaiton about filters etc are in mulplt.inc common block
c
C   When a new waveform file is generated from an old file, the response might
c   have changed. If e.g. the output file is in displacement, the original 
c   response file cannot be used  anymore and a new will be put into the header
c   of the output file. In this particular case, the response will be very, 
c   a constant value. However, if the output is in velocity or filters have been
c   used, a more complex function must be made. The convetion is as elswhere in
c   SEISAN, that dividing by this function, the output will be displacement
c   in nanometers.
c
C   ALL UNITS ARE IN METERS AND SECONDS EXCEPT ACCELERATION
C   WHICH IS SUPPOSED TO BE IN G.
C
C   LAST UPDATE:
C
      IMPLICIT NONE
      save
      include 'mulplt.inc'
C
C   VARIABLES
C
C   COMPLEX RESPONSE OF SYSTEM FOR NF VALUES
      COMPLEX RESPON(61)
      INTEGER NF
C   FIRST AND LAST FREQUENCY TO USE
      REAL F1,F2
C   REAL AMPLITUDE AND PHASE VALUES, GAIN AT 1HZ, LOG OF AMP
      REAL AMP(61),xphase(61), AMP1HZ
      CHARACTER*80 TEXT  ! GENERAL TEXT
C   FREQUENCIES FOR CALCULATIONS, LOG OF F
      REAL F(61)
C   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      integer poles      ! number of poles in filter
C   SEISAN HEADER
      CHARACTER*1040 chead_new
      character*80  save_head  ! part of header
C   TEXT FOR RESPONSE AND SENSOR TYPE
      CHARACTER*15 RESTXT(3)
C   CONSTANT
      REAL PI
C   HELP VARIABLES
      COMPLEX HS
      REAL XX,YY
      INTEGER I,J,K,I1,I2,I3,I4,I5,I6,J1,J2
c
      DATA PI/3.141592654/
c
c   check if a filter has been put in, but no response, then a response
c   must be read in and combined with the filter
c
c
c   the response will always be read in, also when not used in order to make
c   sure the output file has the correct response
c
         save_head=chead(1:80)     ! save start of header, is overwritten
c
c   first check if response info available and from where
c
         wav_resp_file = ' '
         call read_resp
         chead(1:80)=save_head     ! original info back
         if(chead(160:160).eq.'9') then
            write(text(1:42),'(a)')
     *      'No response info, output response wrong***'
            call tchars(text,42,200.0,320.0)
            chead_new(81:1040)=' '  ! no response given
            return
         endif
c
         if(chead(160:160).eq.'8') then
            write(text(1:33),'(a)')'Response from waweform header ***'
            call tchars(text,33,600.0,320.0)
         endif
c
c   check if anything has changed in the response
c
      if(remove_response.eq.0.and.filt.eq.0) then
         chead_new(81:1040)=chead(81:1040)   ! no change, use old response
         return                              ! - first 80 chars made in outside
      endif
c
c   if here, assume a new response function must be made
c

C
C   TEXT FOR OPTIONS
C
      RESTXT(1)='DISPLACEMENT   '
      RESTXT(2)='VELOCITY       '
      RESTXT(3)='ACCELERATION   '      
C
C   FREQUENCIES FOR CALCULATION, ROUND OFF VALUES
C
      NF=60
      F1=0.005
      F2=100.0
      DO 5 I=1,NF
         XX=ALOG10(F1)+(I-1)*(ALOG10(F2)-ALOG10(F1))/(NF-1)
         F(I)=10**XX
         if(f(i).le.0.01.and.f(i).gt.0.001) k=10000
         IF(F(I).LE.0.1.AND.F(I).GT.0.01) K=1000
         IF(F(I).LE.1.0.AND.F(I).GT.0.1) K=100
         IF(F(I).LE.10.0.AND.F(I).GT.1.0) K=10
         IF(F(I).GE.10.0) K=1
         J=F(I)*K+0.5
         F(I)=J/FLOAT(K)          
 5    CONTINUE
C
C   PUT 1 HZ RESPONSE IN NUMBER 61
C
      F(61)=1.0
      NF=61
C
C  ENTER LOOP OF NF PREDEFINED FREQUENCES TO CALCULTE REPONSE
C   
      DO 100 I=1,NF
C
c
c-------- Get the response if not already corrected
c
         if(remove_response.ne.1) then
            call calc_resp(f(i),respon(i))
         else
            respon(i)=1.0                     ! response already corrected for
         endif
C
C   FILTERS
C
         IF(FILT.GT.0) THEN
            if(remove_response.eq.1) then     ! always use 8 poles when filter
               poles=8                        ! - used in frequency domain
            else
               poles=4                        ! normally 4 poles in time domain
               if(npasses.eq.2) poles=8       ! - however, could be 8 
            endif
            if(flow(filt).ne.0) then
               call bworth(f(i),flow(filt),-poles,hs)	
               respon(i) = respon(i) * hs
            endif
            if(fhigh(filt).ne.0) then
                call bworth(f(i),fhigh(filt),poles,hs)	
                respon(i) = respon(i) * hs
            endif
          endif
c
c   correct for different types like velocity or accelleration
c   disp_vel is 1 for displacement, 2 for velocity. and 3 for acc.
c
          hs=(0,1)*(2*pi*f(i))
          respon(i) = respon(i)*hs**(disp_vel-1)
C
C   COMPLEX RESPONSE FINISHED, CALCULATE REAL RESPONSE
C        
          AMP(I)=CABS(RESPON(I))
          XX=AIMAG(RESPON(I))
          YY=REAL(RESPON(I))
          IF(YY.EQ.0.0) THEN
             xphase(I)=0.0
          ELSE
             xphase(I)=ATAN2(XX,YY)
          ENDIF
 100  CONTINUE
C
C  FINISHED RESPONSE CALCULATION. NORMALIZE TO 1.0 AT 1 HZ
C  AND SAVE 1HZ VALUE, use the real value
C
      AMP1HZ=amp(61)
C
C   FINISHED WITH 1 HZ, NF BACK TO 60
C
      NF=60
      DO 110 I=1,NF
         AMP(I)=AMP(I)/AMP1HZ
 110  CONTINUE
c
c   since trace is corrected to nm, must get the constant back to m
c
      amp1hz=amp1hz*1.0e9
C
C   WRITE OUT SEISAN RESPONSE in header, FIRST BLANK
C
      do 64 i=81,1040
         chead_new(i:i)=' '
 64   continue

c
c   write type of response if instrument corrected
c
      if(disp_vel.gt.0) chead_new(82:94)=restxt(disp_vel)(1:13)
c
c   if filter used, write out
c
      if(filt.gt.0) write(chead_new(97:131),'(a8,2f7.3,i3)')
     *'Filter: ',flow(filt),fhigh(filt),poles
C  
C   CONSTANTS
C
c
c   put in part of old values, even if not used
c
      chead_new(161:320)=chead(161:320)
c
c   Only tabulated values can be used, indicate by T in header
c   Force use of header, indicated by 'F' 
c
      chead_new(78:79)='TF'
c
c   The new gain at 1 hz
c
      WRITE(chead_new(201:208),'(G8.3)') AMP1HZ
C
C  RESPONSE VALUES
C
        do 60 i=1,3
          j1=(i-1)*20 + 1
          j2=j1+19
          i1=320+(i-1)*240+1
          i2=i1+79
          i3=400+(i-1)*240+1
          i4=i3+79
          i5=480+(i-1)*240+1
          i6=i5+79
          write(chead_new(i1:i2),'(10g8.3)')(f(j),j=j1,j2,2)
          write(chead_new(i3:i4),'(10g8.3)')(amp(j),j=j1,j2,2)
          write(chead_new(i5:i6),'(10f8.3)')(xphase(j),j=j1,j2,2)
 60     continue
C
C   END
C
      return
      END


c
c  pick amplitudes automatically and plot the amplitudes, jh jan 2001
c
c  output: ix:     pixel where amplitude is picked
c          amp:    amplitude
c          per:    period
c
      subroutine auto_amp_mulplt(ix,amp,per)
      implicit none
      include 'mulplt.inc'
      real amp,per                     ! amplitude and period
      real ix,iy,max_amp,i_max_amp,av  ! help variables
      real fcmin,fcmax,yfirst,ylast    ! frequency range and first and last
                                       ! amplitude picked
      integer ifirst,ilast             ! sample number corrosponding to
                                       ! yfirst and ylast
      integer i,n                      ! counters
      integer maxcros                  ! maximum number of zero crossings
c
c  set auto amp parameters

      fcmin=0.01
      fcmax=10.0
      maxcros=2

      av=0.0
c
c  select data and subtract dc
c
      do i=first,last
        wav_y1(i-first+1)=wav_y1(i)
        av=av+wav_y1(i)
      enddo
c
      n=last-first+1
      av=av/n
      do i=1,n
        wav_y1(i)=wav_y1(i)-av
      enddo
c
c   autopick amplitude
c

      call auto_amp
     *(wav_y1,n,rate,fcmin,fcmax,maxcros,yfirst,ylast,ifirst,ilast)
c
c     if ifist is zero, no amplitude picked
c
      if(ifirst.eq.0) then
         amp=0.0
         return
      endif
c
c   calculate amplitude and period
c
       amp=abs(yfirst-ylast)/2.0
       per=2.0*abs(ilast-ifirst)/rate
c
c   plot first amplitude
c
       ix=(ifirst/rate)*xscale+xpos-1
       iy=yfirst*yscale+ypos+height/2.0
c       write(99,*)'ys,ypos,hei',yscale,ypos,height
c       write(99,*)ifirst,ix,yfirst,iy
       i=ix       ! save position of first value
c
c   plot
c
        call xmovabs(ix,iy+10.0)
        call xdrwabs(ix,iy-10.0)
        call xmovabs(ix-10.0,iy)
        call xdrwabs(ix+10.0,iy)
        call xdrwabs(ix,iy+10.0)
        call xdrwabs(ix-10.0,iy)
c
c   plot last amplitude
c
       ix=(ilast/rate)*xscale+xpos-1
       iy=ylast*yscale+ypos+height/2.0
c       write(99,*)ilast,ix,ylast,iy
c
c   plot
c
        call xmovabs(ix,iy+10.0)
        call xdrwabs(ix,iy-10.0)
        call xmovabs(ix-10.0,iy)
        call xdrwabs(ix+10.0,iy)
        call xdrwabs(ix,iy+10.0)

        ix=i       ! return position of first value

      return
      end

c
c subroutine for seting markers in cont mode for extract windows
c and for phase readings
c 2015.05.19 pv: first version
c
      subroutine mulplt_cont_picking(ich,ix,iy,height,extunittmp)
      real ix,iy,height
      integer ich,extunittmp
c
c   Input Start and End times for extracting time windows in cont mode
c s to mark the start time of the time window that should be extracted
         if(char(ich).eq.'s') then
           write(extunittmp,*)'begin_position',ix,iy
           call xmovabs(ix,iy-height*.5)                                     
           call xdrwabs(ix,iy+height*.5)                              
           call xchars('Start',6,ix,iy+height*.4)
         endif
c e to mark the end time of the time window that should be extracted
         if(char(ich).eq.'e') then
           write(extunittmp,*)'end_position',ix,iy
           call xmovabs(ix,iy-height*.5)                                     
           call xdrwabs(ix,iy+height*.5)                              
           call xchars('End',3,ix,iy+height*.4)
         endif
c b is reserved for going back in time
c        if(char(ich).ne.'b') goto ??????????
c
c Input P and S readings (Local or Teleseismic):
c
c 1 to mark time of IP from local eq
         if(char(ich).eq.'1') then
           write(extunittmp,*)'IP-local    ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('IP local ',9,ix,iy+height*.4)
         endif
c 2 to mark time of EP from local eq
         if(char(ich).eq.'2') then
           write(extunittmp,*)'EP-local    ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('EP local ',9,ix,iy+height*.4)
         endif
c 7 to mark time of IS from local eq
         if(char(ich).eq.'7') then
           write(extunittmp,*)'IS-local    ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('IS local ',9,ix,iy+height*.4)
         endif
c 8 to mark time of ES from local eq
         if(char(ich).eq.'8') then
           write(extunittmp,*)'ES-local    ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('ES local ',9,ix,iy+height*.4)
         endif
c 3 to mark time of IP from distant eq
         if(char(ich).eq.'3') then
           write(extunittmp,*)'IP-distant  ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('IP Distant',10,ix,iy+height*.4)
         endif
c 4 to mark time of EP from distant eq
         if(char(ich).eq.'4') then
           write(extunittmp,*)'EP-distant  ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('EP Distant',10,ix,iy+height*.4)
         endif
c 9 to mark time of IS from distant eq
         if(char(ich).eq.'9') then
           write(extunittmp,*)'IS-distant  ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('IS Distant',10,ix,iy+height*.4)
         endif
c 0 to mark time of ES from distant eq
         if(char(ich).eq.'0') then
           write(extunittmp,*)'ES-distant  ',ix,iy
           call xmovabs(ix,iy-height*.5)
           call xdrwabs(ix,iy+height*.5)
           call xchars('ES Distant',10,ix,iy+height*.4)
         endif
c
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_dist(station,edist,hdist)
c
c   find epicentral, edist, and hypocentral, hdist, distance for a given station, station
c   jh, feb 2018
c
      implicit none
      include 'mulplt.inc'
      character*5 station
      real hdist,edist
      integer i

      do i=nhead+1,nrecord                                        
         if(data(i)(2:6).eq.station) then
           if(data(i)(71:75).ne.'     ') then
              read(data(i)(71:75),'(f5.0)') edist                 
c
c   calculate hypocentral distance
c
              read(data(1)(39:43),'(f5.1)') edepth
              hdist=sqrt(edist*edist+edepth*edepth)
              goto 10
           endif
         endif
      enddo
 10   continue
      return
      end
