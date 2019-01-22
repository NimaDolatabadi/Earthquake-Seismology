c--------------------------------------------------------------------------
c Program for automatic processing: Phase picking, location, magnitude 
c and fault plane solution determination
c
c jh december 2016
c
c 
c   
c   This program run the picking program autophase first (optionally
c   autopic), locates events with or without outlier removal with hyp
c   and then determines magnitudes with automag. Automatic amplitudes
c   can be done with autoratio. Fault plane solution
c   can also be done with FPFIT. The default is to 
c   to do all steps minus  autoratio and fps, using default parameters. Individual steps,
c   like doing magnitudes, can optionally be deselected and some non
c   default parameters for automag can be selected. All changes are
c   made using arguments, see list below. Thus the program can also be
c   used to do any one of the operations. However to do only magnitudes,
c   a more rational choise would be to use AUTOMAG only. Similarly
c   for doing only locations, it is more logical to use HYP.
c   The input can be all SEISAN types: a file, a data base or an 
c   index file.
c   Outlier removal by HYP can be deselected. An alternative to 
c   outlier removal by HYP is to use the HYP option for residual
c   weighting.
c
c   For more detail of the individual programs, see program 
c   descriptions elsewhere in SEISAN manual.
c
c   There are always two output files:
c
c   auto.out: all events with the final results with updated values
c             of location and magnitude.
c   auto.log: a summary of what has been done to each event. 
c
c   If input from a data base or index file, the results are also written 
c   back to the data base, overwriting what was there from before.
c
c   The ID line is not updated by AUTO.

c
c  When picking automatically, a few bad picks can throw the solution off so 
c  for a local event, the distance might wrongly be very large. It is then
c  important that the range used for distance weighting in HYP is large so
c  the initial wrong location can be made and the outlier rejection gets
c  a chance to eliminate bad picks.
c
c  doing fault plane solution is only recommend with very good data. it is 
c  then important to use at least thwe default values set for both number
c  of polarities and gap. note the gap is not the gap as in a hypocenter
c  solutons but the gap when polaritirs are plotted on the projection of 
c  the fault plane solution. 
c  
c  see list of argument below
c
c  examples:
c
c   auto          : pick phases, locate and do magnitudes
c   auto l m      : only pick phases with autophase
c   auto j l m    : only pick phases with autopic
c   auto i l      : only do magnitudes
c   auto s 30     : all defaults excepth that spectral window for magniude is 30 s
c   auto l m i f  : only do fault plane solutions with fpfit
c   auto l m i ar h f: automatic amplitudes and fault plane solution with hash
c
c--------------------------------------------------------------------------c
c
c  For detail on some parameters and variables names, see rea.inc
c
c  changes
c
c  2018 05 27 jh: small change in output to log file
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      integer nars                        ! number of arguments
      character*80 arg(100)               ! arguments

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer npol                        ! number of polarities
      integer npol_min                    ! min ---- to make fps
      character*120 text                  ! general text
      character*10 keys                   ! next choice key for routine findevin
      character*14 starttime,endtime      ! time interval to read
      character*80 basename               ! data base or file name
      character*80 infile                 ! input file or base name
      character*80 eventfile              ! single event file name
      integer status,eventno,newmonth,fstart ! for routine findevin
      character*60 tx(100)                ! help lines
      integer nhelp                       ! number of help lines
      integer base                        ! type of input 0: seisan data base, 
                                          !     1: index file, 2: single file
      real flow,fhigh                     ! filters for autoratio
      real duration                       ! duration analysis window for autoratio
      real maxdist                        ! maximum distance for autoratio
      logical s_on_z                      ! if true, use Z for s   -------
      integer ground                      ! ground motion to use for -----
      

      logical phase_pick                  ! true if phase pick
      logical locate                      ! if true, locate
      logical outlier                     ! if true, remove outlier
      logical do_mag                      ! if true, do magnitudes
      logical autophase                   ! if true use autophase, else autopic
      logical fault_plane                 ! if true, do fps
      logical fpfit                       ! if true, use fpfit to fps
      logical hash                        ! if true, use hash for fps
      logical autoratio                   ! if true, do autoratio
      character*1 comp_select             ! component to use
      character*1 spectrum_type           ! P or S spectrum, defualt P
      real wa_window                      ! window for ml
      real spec_window                    ! window for spectrum
      real gap                            ! gap when doing fps
      real gap_max                        ! maximum ----------
      real strike,dip,rake,fit            ! fps parameters
      integer i                           ! counter


c
c   arguments (case sensitive):
c              
      tx(1)= 's xxx : do spectrum for Mw, xxx is window length, default'
      tx(2)= '        is 20 s. if 0, no spectrum. default is spectrum'
      tx(3)= 'w xxx : amplitude for Ml, xxx is window length'
      tx(4)= '        if zero, no amplitude. default is do amplitude'
      tx(5)= '        default window lenght is 50 s'
      tx(6)= 'n or e: use N or E component for magnitude, respectively.' 
      tx(7)= '        Default is Z'
      tx(8)= 'p     : use P for Mw, default is S'
      tx(9)= 'l     : do not locate, def. is to locate with rejection'
      tx(10)='        if magnitudes are done, location is also done'
      tx(11)='        after dtermination of amps and spectra in order'
      tx(12)='        to update magnitudes on header line'
      tx(13)='r     : do not remove outliers when locating'
      tx(14)='m     : no magnitude, default is to do magnitude'
      tx(15)='i     : no phase pick, default is to do phase pick'
      tx(16)='j     : if phase pick, use autopic, default is autophase'
      tx(17)='ar    : do autoratio, default is not to do'
      tx(18)='af x x : filter band for autoratio, default is 2-4 Hz'
      tx(19)='ad xx : max distance for autoratio, def 100km'
      tx(20)='at xx : time window for autoratio, def 2 s'
      tx(21)='ag xx : ground motion for autoratio, def 0 for none'
      tx(22)='az    : if given, use z for s in autoratio'
      tx(23)='f     : do fault plane solution with fpfit, def. is not'                           
      tx(24)='h     : do fault plane solution with hash, def. is not'
      tx(25)='n     : minimum number of polarities for fps, default 10'
      tx(26)='g     : maximum gap for fps, default 180'
      nhelp=26
c
c
c  no arguments are needed if all defaults are used
c
c  set default values
c

      wa_window=50.0     ! window for ml
      spec_window=20.0   ! windoew for mw
      autophase=.true.   ! pick phases with autophase
      locate=.true.      ! do loaction
      outlier=.true.     ! remove outliers
      do_mag=.true.      ! calculate magnitudes
      phase_pick=.true.  ! pick phases
      fault_plane=.false.! do not do fps
      fpfit=.false.      ! do not do fpfit
      hash=.false.       ! do not do hash
      autoratio=.false.  ! do not calculate amplitudes with autoratio
      spectrum_type=' '  ! default S
      comp_select=' '    ! default use Z
      npol_min=10        ! min polarities for fps
      gap_max=180.0      ! max gap for polarities
      duration=2.0       ! durtion for auto amplitudes
      ground=0           ! no ground motion correctio for auto amplitude
      maxdist=100.0      ! maximum distance for auto amplitude
      flow=2.0           ! low filter for---------------------
      fhigh=4.0          ! hightr filter----------------------
      s_on_z=.false.     ! do not use s on z -----------------
 
      write(6,*)'Write auto help to get list of arguments. If no'
      write(6,*)'arguments, all defaults are used. This is autophase,'
      write(6,*)'location with outlier rejection and ML and spectral Mw'
      write(6,*)
c
c   get arguments
c
      call get_arguments(nars,arg)


      if(nars.gt.0) then
c
c   check if help function
c        
          if(arg(1)(1:4).eq.'help') then
             do i=1,nhelp
               write(6,'(2x,a)') tx(i)
             enddo
             stop
           endif

        do i=1,nars
c  length of spec window
          if(arg(i)(1:1).eq.'s') then
            read(arg(i+1),*) spec_window
          endif
c  use p-spectrum 
          if(arg(i)(1:1).eq.'p') then
            spectrum_type='p'
          endif
c  wa window
          if(arg(i)(1:1).eq.'w') then
            read(arg(i+1),*) wa_window
          endif
c  use N component
          if(arg(i)(1:1).eq.'n') then
            comp_select='n'
          endif
c  use E component
          if(arg(i)(1:1).eq.'e') then
            comp_select='e'
          endif
c  do not locate
          if(arg(i)(1:1).eq.'l') then
            locate=.false.
          endif
c  do not remove outliers when locating
          if(arg(i)(1:1).eq.'r') then
            outlier=.false.
          endif
c  do not pick phases
          if(arg(i)(1:1).eq.'i') then
            phase_pick=.false.
          endif
c  use autopic
          if(arg(i)(1:1).eq.'i') then
            autophase=.false.
          endif
c  do not calculate magnitude
          if(arg(i)(1:1).eq.'m') then
            do_mag=.false.
          endif
c  calculate fault plane solution with fpfit
          if(arg(i)(1:1).eq.'f') then
            fault_plane=.true.
            fpfit=.true.
          endif
c  calculate fault plane solution with hash
          if(arg(i)(1:1).eq.'h') then
            fault_plane=.true.
            hash=.true.
          endif
c  minimum number of polarities for fault plane solution
          if(arg(i)(1:1).eq.'n') then
            read(arg(i+1),*) npol_min
          endif
c  maximum gap for fault plane solution
          if(arg(i)(1:1).eq.'g') then
            read(arg(i+1),*) gap_max
          endif
c  do autoratio
          if(arg(i)(1:2).eq.'ar') then
            autoratio=.true.
          endif
c  autoratio max distance
          if(arg(i)(1:2).eq.'ad') then
             read(arg(i+1),*) maxdist
          endif
c  autoratio filter
          if(arg(i)(1:2).eq.'af') then
             read(arg(i+1),*) flow
             read(arg(i+2),*) fhigh
          endif
c  autoratio time window
          if(arg(i)(1:2).eq.'at') then
            read(arg(i+1),*) duration
          endif
c  autoratio time ground motion
          if(arg(i)(1:2).eq.'ag') then
            read(arg(i+1),*) ground
          endif
c  autoratio, s on z
          if(arg(i)(1:2).eq.'az') then
            s_on_z=.true.
          endif
        enddo
      endif
c
c  no magnitude if both time windows are zero
c

      if(wa_window.eq.0.0.and.spec_window.eq.0.0) do_mag=.false.
c
c   check if not all deselected
c
      if(.not.do_mag.and..not.locate.and..not.phase_pick.and.
     *   .not.fault_plane.and..not.autoratio) then
          write(6,*)' All options deselected'
          stop
      endif
c
c
c   open output file
c
      open(2,file='auto.out', status='unknown')
c
c   open  work output file
c
      open(3,file='work.out',status='unknown')
c
c   open log file
c
      open(7,file='auto.log',status='unknown')
    
c
c   get input file name, or data base with time interval
c

      write(*,*) ' Event data input - select one:'
      write(*,*)
      WRITE(*,*) 
     *'    SEISAN default data base or                     :',
     *'Enter '
      write(*,*) 
     *'    Alternative data base, give 1-5 letter code or  :'  
      WRITE(*,*) 
     *'    Local index file, name must start with index or :'
      WRITE(*,*) 
     *'    Local data base, write ,, or                    :'
      WRITE(*,*) 
     *'    Filename for one file, min. 6 chars or with a . : '
      
      write(*,*)
      read(*,'(a)') infile
      basename=infile(1:80)
                       
      write(*,*)
C
c   check if this is a single multiple event file (base=2),
c   general data base (base=0) or  a local data base or 
c   index file (base=1)
c

      starttime=' '
      endtime=' '

      keys(1:4)='NEXT'    ! always use next event

c
c   initially assume a file
c
      base=2
c
c   a SEISAN 5 letter data base, blank is default data base, ',,'
c   is a local data base. the name cannot have a '.' and must
c   be less than 6 chars long
c
      if(seiclen(basename).lt.6.and.index(basename,'.').eq.0) then 
         base=0
      endif
c
c   case of index file or local data base
c
 
      if(basename(1:5).eq.'INDEX'.or.basename(1:5).eq.'index'.
     *     or.basename(1:2).eq.',,') then
        base=1
      endif
                              
c
c  get time interval for a seisan data base, no time interval used for
c  index file or local data base
c
      if(base.eq.0) then
         write(*,'('' Start Time           (YYYYMMDDHHMMSS): '',$)')
         read(*,'(a14)') starttime
         write(*,'('' End Time, enter is to end of month:    '',$)')
         read(*,'(a14)') endtime
         write(*,*)
      endif
c
c   open access for the the relevant input option
c


c
c  a file
c
      if(base.eq.2) then
         open(1,file=infile,status='old',err=5)
         goto 6
 5       continue
         write(6,*)' Input file not found'
         stop
 6       continue
      endif


      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue

c
c   read from relevant input type
c
      if(base.lt.2) then     ! data base  or index file event
         call findevin
     *   (basename,starttime,endtime,keys,0,eventno,
     *   eventfile,fstart,newmonth,status)
c
c   check if end of time interval or errors, then
c   stop
C     
         if(status.gt.0) then
            write(6,*)' STOP WITH STATUS=',status
            goto 1000  ! stop
         endif
C
C   open data base input single event file
c

         open(1,file=eventfile,status='old',err=7)
         goto 8
 7       continue
         write(6,*)' Input file not found: ',eventfile
         stop
 8       continue
      endif

c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)

c
c   close file if from data base since only one event
c
      if(base.lt.2) close(1)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1               ! count events
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)
      write(7,*)
      write(7,'(a)') data(1)(1:79)
c
c   write data to a temporary file for processing
c
      rewind(3)
      call rea_event_out(3,all,data,code)
      rewind(3)
c     
c------------------------------------------------------      
c  auto pick, two different programs can be used, default
c  is autophase
c------------------------------------------------------
c
      if(phase_pick) then
c
c   run autophase
c
         if(autophase) then
            write(7,*)'Pick with autophase'
c
c  make one miniseed file from all input files
c
            text=' '
            text='wavetool -sfile work.out'//
     *      ' -format MSEED -wav_out_file seed.out'
            write(6,'(a)') text
c
c  run wavetool   extract mes always there ?
c
            call systemc(text,seiclen(text))
c
c  check if conversion ok, if not skip event for processing
c  but write out again
c
            open(30,file='extract.mes',status='old',err=20)
            goto 21
 20         continue
            write(6,*)'Something wrong with waveform data, no pick'
            write(7,*)'Something wrong with waveform data, no pick'
            goto 100
 21         continue
            close(30)
c
c   run autophase, this overwrites s-file
c
            text=' '
            text='autophase -sfile work.out'//
     *      ' -wavefile seed.out'
            write(6,'(a)') text
            call systemc(text,seiclen(text))
c
c  read in data again
c
            rewind(3)
            call rea_event_in(3,all,data,code)
            write(7,*)'Number of stations and phases using autophase:',
     *      rea_nstat,rea_nphase
            rewind(3)
        else
c
c   run autopick but only if no readings from before since
c   readings are appended
c
           write(7,*)'Pick with autopic if no redings. nread=',rea_nstat
           if(rea_nstat.lt.1) then
              call put_env_event('work.out')
              call systemc("autopic",7)
c
c  read in data again
c
              rewind(3)
              call rea_event_in(3,all,data,code)
              write(7,*)'Number of stations and phases using autopic:',
     *        rea_nstat,rea_nphase
              rewind(3)
           else
              write(6,*) 
     *        'Autopic not executed since readings from before'   
              write(7,*) 
     *        'Autopic not executed since readings from before'   
           endif
        endif        
      endif
c
c-----------------------------------------------------------------
c  locate
c------------------------------------------------------------------
c
      if(locate) then
         if(outlier) then
            write(7,*)'Locate with outlier removal'
            write(7,*)
     *   'Number of stations and phases before outlier removal:',
     *    rea_nstat,rea_nphase
            call systemc('hyp work.out -reject -eev',25)
         else
            write(7,*)'Locate without outlier removal'
            call systemc('hyp work.out',12)
         endif
c
c   read in located event and write in work.out
c
         open(11,file='hyp.out',status='old')
         call rea_event_in(11,all,data,code)
         if(outlier)write(7,*)
     *   'Number of stations and phases after outlier removal: ',
     *    rea_nstat,rea_nphase
         rewind(3)
         call rea_event_out(3,all,data,code)
         rewind(3)
         close(11)   
         write(7,'(a)') data(1)(1:79)        
      endif

c
c--------------------------------------------------------------------
c   magnitudes, any previous results of same stat-comp is owerwritten
c--------------------------------------------------------------------
c
      if(do_mag) then
         text=' '
         text(1:15)= ' Do magnitudes: '
         if(wa_window.gt.0.0) text(16:17)='Ml'
         if(spec_window.gt.0.0) text(19:20)='Mw'
         write(7,'(a)')text(1:20)
         text=' '
         text(1:19)='automag work.out s ' 
         write(text(20:26),'(f7.1)') spec_window
         text(27:29)=' w '
         write(text(30:36),'(f7.1)') wa_window
         text(37:47) =' overwrite'   ! overwite old results
         i=47
c   possibly change component used
         if(comp_select.eq.'e'.or.comp_select.eq.'n') then
            text(i+1:i+3)=' '//comp_select//' '
            i=i+3
         endif
c   possibly use p instead of s         
         if(spectrum_type.eq.'p') then
            text(i+1:i+3)=' '//spectrum_type//' '
            i=i+3
         endif

         write(6,'(a70)') text
         call systemc(text,i)
c
c   locate to update magnitudes
c
         call systemc('hyp automag.out',15)
c
c   read results
c
         open(11,file='hyp.out',status='old')
         call rea_event_in(11,all,data,code)
         write(7,'(a)') data(1)(1:79)
         close(11)
c
c   write in work.out
c

         rewind(3)
         call rea_event_out(3,all,data,code)
         rewind(3)
         close(11)   
      endif
c
c------------------------------------------------------------------
c   autoratio
c------------------------------------------------------------------
c
      if(autoratio) then
c
c   make string for options, write out is assumed
c
         text=' '
         text(1:19)='autoratio work.out '
         write(text(20:27),'(a,f5.1)') ' t ', duration
         write(text(28:42),'(a,2f6.2)')' f ',flow,fhigh
         write(text(43:51),'(a,f6.1)') ' d ', maxdist
         write(text(52:56),'(a,i2)')   ' g ', ground
         if(s_on_z) text(58:58)='z'
         write(6,*) text
         write(7,*) text

         call systemc(text,58)
c
c   read in results
c
         rewind(3)
         call rea_event_in(3,all,data,code)
      endif
c
c------------------------------------------------------------------
c   fault plane solution
c------------------------------------------------------------------
c
      if(fault_plane) then
c
c   count polarities and determine gap on focal sphere
c
        call fps_gap(npol,gap)
        
        write(6,*)'Number of polarities and gap',npol,gap
        text=' '
        text(1:36)='Fault plane solution, npol and gap: '
        write(text(38:49),'(i5,f7.1)') npol,gap
        write(7,'(1x,a)') text(1:47)
c
c   delete output file
c
        if(fpfit) then
           open(97,file='fpfit.fps',status='unknown')
           close(97,status='delete')
        endif
c
c   only make solution if more than npol_min and gap smaller
c   then gap_max
c
        if(npol.ge.npol_min.and.gap.le.gap_max) then
c
c   locate to make sure hyp.out and print.out are for this event
c
           call systemc('hyp work.out',12) 

           if(fpfit) then
c
c   use fpfit for fps
c
              call systemc('fpfit',5)

c
c   get solution if there, just for log file
c
              open(97,file='fpfit.fps',status='old',err=30)
              goto 31
 30           continue
              write(6,*)'No fps solution, error opening fpfit.fps'
              write(7,*)'No fps solution, error opening fpfit.fps'
              close(97)
              goto 34   ! jump out since no solution
 31           continue
c
c   read solution
c
              read(97,'(83x,f3.0,1x,f2.0,f4.0,2x,f4.2)',end=32) 
     *        strike,dip,rake,fit
              write(7,*)
     *        'Fpfit:  Strike=',strike,' Dip=',dip,' Rake=',rake,
     *        ' Fit=',fit
              goto 33
 32           continue
              write(6,*) 'Error reading fpfit.fps'
              write(7,*) 'Error reading fpfit.fps'
              goto 34
 33           continue
              close(97)
           endif
c
c   fps with hash
c
           if(hash) then
              call systemc('hash d',6)  
c
c   read solution if there
c
c   get parameters
c

              open(77,file='hash.out',status='old',err=55)
c
c   read solution, for log
c
              read(77,'(27x,3f8.1)',end=55)
     *        strike,dip,rake
              read(77,'(a)',end=55) text   
              read(77,'(32x,f5.2)',end=55) fit
              goto 56
 55           continue
              write(6,*)'Hash: No fps solution'
              write(7,*)'Hash: No fps solution'
              goto 57
 56           continue
              write(7,*)
     *        'Hash:  Strike=',strike,' Dip=',dip,' Rake=',rake,
     *        ' Fit=',fit
 57           continue
              close(77)  
           endif
c
c   read in event with a possible fps 
c
           open(11,file='hyp.out',status='old')
           call rea_event_in(11,all,data,code)
           close(11)
c
c  indicate on F-line that this is an automatic solution
c        
           do i=1,rea_nfault
              if(rea_fault(i)(71:75).eq.'FPFIT'.and.
     *        rea_fault(i)(67:69).eq.'   '.and.fpfit) then
                 rea_fault(i)(67:69)='AUT'
                 goto 34      ! only correct first appearence of fps
              endif
              if(rea_fault(i)(71:74).eq.'HASH'.and.
     *        rea_fault(i)(67:69).eq.'   '.and.hash) then
                 rea_fault(i)(67:69)='AUT'
                 goto 34      ! only correct first appearence of fps
              endif
           enddo
c
c   here if no solution or first solution in hyp.out has been edited above
c              
 34        continue
         else
           write(7,*)'Not enough data for fps using current conditions'
         endif
      endif
c------------------------------------------------------------------
c   write out processed event, it is assumed to be in memory
c------------------------------------------------------------------
c

 100  continue
c
c   write in one file
c
      call rea_event_out(2,all,data,code)
c
c   write in data base if input from data base
c
      if(base.lt.2) then
C
C   open data base input single event file
c
        open(11,file=eventfile,status='old')
        call rea_event_out(11,all,data,code)
      endif
c
c----------------------------------------------------
c   get next event
c---------------------------------------------------

      goto 50
c
c     end of file or data base
c
 1000 continue
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file or data base', nevent
      write(6,*) 'Output file name is auto.out'
      write(6,*) 'Summary of run in auto.log'

      stop
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine fps_gap(npol,gap)
c
c   calculate gap in azimuth used by fps taking into account that direct rays
c   must have added 180 deg. weighted out phases are included
c
c  jh dec 2016
c   
c   input data comes from rea structure
c
c   gap is gap, npol is number of polarities
c

      implicit none
      include 'seidim.inc'
      include 'rea.inc'   
      include 'seisan.inc'       
      
      real az(2000)           ! azimuths available for fps
      integer isort(2000)     ! pointers for sorting
      integer npol            ! number of polarities  from P on Z
      real gap
      integer i

c
c   find number of polarities and az used in fps
c
      npol=0
      do i=1,rea_nphase
         if(rea_phase(i)(1:1).eq.'P'.and.rea_co(i)(2:2).eq.'Z'.
     *   and.(rea_polarity(i).eq.'D'.or.rea_polarity(i).eq.'C').and.
     *   rea_ain(i).gt.0.0) then
           npol=npol+1
           if(npol.gt.2000) then
              write(6,*)' more than 2000 polarities, stop'
              stop
           endif
           az(npol)=rea_az(i)
c
c   add 180 deg to az from direct waves
c
           if(rea_ain(i).gt.90.0) then
               az(npol)=az(npol)+180.0
               if(az(npol).gt.360.0) az(npol)=az(npol)-360.0
           endif
         endif
      enddo
c
c  sort the az
c
      call r4sort(npol,az,isort)
      do i=1,npol
c        write(6,*) isort(i)
c        write(6,*) az(isort(i))
      enddo
c
c   find max gap, add one value to represent
c   gap between smallest and largest value
c
      isort(npol+1)=npol+1
      az(npol+1)=az(isort(1))+360.0  ! add first value to complete                          
      
      gap=0.0
      do i=1,npol
         if(az(isort(i+1))-az(isort(i)).gt.gap) 
     *   gap=az(isort(i+1))-az(isort(i))
      enddo

      return
      end
   