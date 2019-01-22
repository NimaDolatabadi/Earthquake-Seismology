C**************************************************************************
C                                                                               
C  PROGRAM MAP DRAWS A MAP WITH EPICENTERS                                      
C  COMMON BLOCK IS DEFINED IN the include file 'epimap.inc' which includes
C                                              'seiplot.inc' for colour
C                                               information.
C                                                                               
C  J. Havskov                                                                   
c                                                                               
c  Feb 89 by j.h.  : simplification and adaption to tectronics                  
c  Mar 27          : Post script posibility, auto file type                     
c  Apr 2           : Bugs                                                       
c  Apr 3           : Postscript scaling and symbol size                         
c  Oct 24 91       : x-windows open and close
c  Nov 26 91       : new nordic format and indata
c  Jan 24 92       : New station.hyp name, unified version
c  Feb 25 92       : always write epimap.out with selected epicenters and #
c  Sep 17 92       : Profile
c  Oct 14 92       : Bug with open station file
c  nov 6  92       : Put in more stokes in postscript
c  nov 8  92       : Fix rectangle plotting when outside frame
c  nov 13 92       : bug when selecteing depths for profile
c  nov 15 92       : Stdinput from a file if given on prompt line,sun
c  mar 8  93       : --------------------------------------------,pc
c  mar 19 93       : bug in ---
c  may 12 93       : add iso option, bergen special
c  jun 7  93       : one digit only for intensity, station file in local dir.
c                    new system_c routine
c  jun 14 93       : dim to 20 000
c  jul  6 93 by jh : VERSION 3.0
c  jul  19         : filename to 80
c  aug 27 93 by jh : station to triangel
c  sep 5, 93       : list of map files as input, null to end call string
c  sep 16,21       : error traps, enable noneven grid, bug
c  sep 23          : stroke after each epicenter
c  apr 25 94       : save profile values for later use, o-option
c  oct 06 94       : no read blank lines in map file
c  oct 23          : multiple sections
C!JAB(BGS)Nov94    : Install file & error handling and BGS map projections
C!JAB(BGS)Dec94    : Install extra facilities exhibited by "emap".
C                    Also multiple epicentre files.
c  jan 95          : bugs on pc
c  march 24 95 by jh: bugs in contor
C!JAB(BGS)May95    : When reading magnitudes from data(1), decode full field to
C!JAB(BGS)May95      allow negative magnitudes to be read .
C!JAB(BGS)May95    : Increase functionality to include, places (like stations),
C!JAB(BGS)May95      multiple decimal place geo-labels, constant depth/horizon-
C!JAB(BGS)May95      tal scale for small region cross-sections. Allow, negative
C!JAB(BGS)May95      magnitudes to be represented. There will be no impact on
C!JAB(BGS)May95      the current usage.
CJAB(BGS)May95   20th: Bug in reading mag from data(1).
CJAB(BGS)May95   20th: Extra facilities requested by BGS operation
C!JAB(BGS)Jun95    : tidied up the coastline / map boundary problem
C!JAB(BGS)Jun95    : In "contor", placed the label 790 "message and return to
C!JAB(BGS)Jun95      contour loop", inside the label 700 contour loop.
c mar 12, 96 jh    : bug when storing epicenter file name, not used
c---------------------------------------------------------------------------
c oct 17 98 jh     : -------------------   version7.0 check ------------------
c                    stations to 5 chars
c nov 5  98 jh     : linux logicm system_c to systemc
c mar 23 99 bmt    : include winplot.inc 
c jun 11 99 lo     : get ps scaling
c aug 17    jh     : make output file epimap.out with all selected data,
c                    change old epimap.out to epimap.num
c                    modify routine search so complete event info is 
c                    written out
c oct 27     jh    : compact file type was wrongly identified sometimes
c dec  9     lo    : changed eigen to epimap_eigen
c mar  24 00 jh    : add file profile.num with profile coordinates in km
c oct 20     jh    : read station coordinates with one more digit, H-line
c                    if it is there
c may 27 2005 jh   : new macroseismic format
c jun 20 2005 jh   : plt to eps
c 2010-04-06 pv  : in epimap.inc changed from 200 to 9000 : PLOTSTAT_N$ = 9000
c 2010-05-04 pv  : changed some tab-formatted source line that gave warnings in f90
c 2010-10-13 jh  : add plot of fps
c 2010-12-22 jh  : gfortran on pc: some if pc changed, winplot include removed, 
c                  implicit none put in, remove hctype, unit check, 
c                  computer type check, remove call to tsend
c 2011 02 22 jh  : size from color.def
c 2011 10 14 jh  : error message if errror when reading type 1 line
c 2013 01 20 jh  : fix dimension (1)
c 2013 06 12 pv  : I have removed subroutine ellipse_x, it is now in LIB/err_ellipse
c                  I have removed subroutine xy_ellipse, it is now in LIB/err_ellipse
c                  The two subroutines are still in the code by the name old_<name>
c 2014 02 17 jh  : read seisan_def
c 2014 02 23 jh  : fix dim of epiorder,chr_epi_col
c 2014 04 13 jh  : dim problem, see 'jh fix 13-4-14 to avoid ix-1'
c 2014 01 06 jh  : new argument to plot_foc
c 2017 02 08 jh  : change radius for fps since new fill routine
c 2017 03 10 jh  : add coordinates of corners of profile outline in profile.out
c                  numbers if profile.num were wrong
c 2017 04 17 jh  : bug in above
C
C    SEISAN library/JAB inclusions...
c    --------------------------------
      implicit none
      include 'libsei.inc'                 ! Library definitions & data defns.
      include 'epimap.inc'                 ! EPIMAP definitions.
      include 'seidim.inc'                 ! array dimentions
      external new_diagram                 ! Set up new diagram.
      external map_proc,                   ! Projection routine.
     &         sei get file,               ! Find & open file.
     &         sei open,                   ! File open handler.
     &         sei close,                  ! & closure.
     &         sei clen,                   ! String length.
     &         sei real num,               ! Get real number.
     &         sei cmd set,                ! Set up command file i/o.
     &         sei cmd pro,                ! Command file processor (local!!).
     &         sei integer,                ! Decode text to integer.
     &         geo deg,                    ! Decode geo-string into degrees.
     &         sei code                    ! Error condition handler.
      integer  sei clen,                   ! & function.
     &         sei integer                 ! Ditto.
      real     geo deg                     ! Et ditto.
     &        ,sei real num                ! Et ditto.
      integer  code,                       ! Condition.
     &         jx, jy, jz, kx, jepi,       ! Very local variable.
     &         text_c,                     ! Text length.
     &         form_c,                     ! Format string.
     &         read1,                      ! Read unit1.
     &         read2,                      ! Ditto 2.
     &         read3,                      ! & 3.
     &         read4,                      ! & 4.
     &         read5,                      ! Complement write7.
     &         read6,                      ! Complemeng write3.
     &         write1,                     ! Write unit1.
     &         write2,                     ! Ditto 2.
     &         write3,                     ! & 3.
     &         write4,                     ! & 4.
     &         write5,                     ! & 5.
     &         write6,                     ! & 6.
     &         write7,                     ! & 7.
     &         write8                      ! & 8.
      parameter (text_c = 80)              ! & value.
      parameter (form_c = 8)               ! Ditto.
      integer  sym_ext                     ! Symbol function extension.
c
      logical  b_flag,                     ! Flag!!
     &         b_flag2                     !
      character chr_text   *(text_c),      ! Text string.
     &          chr_a      *(1)            ! Test character.
C
      real     right_edge                  ! Right edge of map boundary.
     &        ,r_edge                      ! & right edge of text.
      logical  b_start                     ! Start to setup plot?.
     &        ,b_page                      ! Indicate new page?.
     &        ,b_place                     ! Plot places?.
     &        ,b_epiload                   ! Load epicentres?.
     &        ,b_area                      ! Area plot?.
     &        ,b_zoom                      ! Flagged to zoom?.
C
C    ------- End of details -------
C
      INTEGER FILETYPE                                                     
      character*80 title,text                                                  
      character*60 top_directory                                                
      CHARACTER*80 DATA(max_data)                                               
      CHARACTER*80 MAG_TEXT(11)                                                 
C
      CHARACTER    FILE_NAME(EPIFILE_N$) *(80),   ! Epicentre filenames.
     &             chr_epi_col(max_epi)  *(2),    ! Colours.
     &             chr_epi_plt(max_epi)  *(1),    ! Plot this event?.
     &             chr_epi_typ(max_epi)  *(1),    ! Filetype from event file.
     &             contour_file          *(80)    ! File of contour levels.
      integer      colour, old_colour             ! Current epifile colour.
      integer      epiunit  (epifile_n$)          ! Epifile units.
     &            ,epitype  (epifile_n$)          ! & file types.
     &            ,epicolour(epifile_n$)          ! & their colour indices.
     &            ,epiorder (max_epi)             ! & magnitude order pointers.
      INTEGER      N_EPIFILE                      ! Epifile count.
      integer      place_u(place_n$),             ! Placename file units.
     &             place_col(place_n$),           ! & colours.
     &             n_place                        ! & # placenames.
      real         mag_min, mag_max               ! Max & minimum magnitudes.
      real         strike(max_epi),dip(max_epi),rake(max_epi) ! fps solution
      logical      fault                          ! if true, plot fps
C
      CHARACTER*1 TYPE,EXP,dchar                                             
      CHARACTER*1 answer_stat,answer                                           
      CHARACTER*1 number_plot                                     
      CHARACTER*2 EW,NS                                                         
      CHARACTER*5 STA                                                           
c
      CHARACTER*5 PLOT_STA(plotstat_n$)       !
c
      real xlonm(2),ylatm(2),xlonmz(2),ylatmz(2)
      real srclon,srclat
      integer nevent1,nevent2,nstation_plot,i,nphase
      integer nstat,nhead,nrecord,nyr,nmon,nday,nhr,nmin,ilat,ilon
      real   slat,slon,depth,rms,smag,smag2,smag3,x,y
      INTEGER STATION_OK
      DIMENSION X(max_epi),Y(max_epi)                                               
      dimension slat(20),slon(20)                                               
      real symb_size                                                         
      integer k,kst,iter,ncon,id                                                       
c---for depth section
c---indicator if autoscaling depth
      logical auto_depth
c---number of profiles
      integer numb_profiles
c---tec xy for epicenters
      real xx(max_epi),yy(max_epi)
c
c   polygon arrays...
c   -----------------
c
      real  xsrc (max_polyn$), ysrc (max_polyn$),! Polygon arrays.
     &      xzoom(max_polyn$), yzoom(Max_polyn$) ! Ditto zoom.
c
c   Saved common items...
c   ---------------------
c
      real latdel, londel, latbase, lonbase
c
c   Some previously undefined variables...
c   --------------------------------------
c
c---surrounding seismic source area points and # events in
      integer pt,evin
c
c---magnitudes, depths and distances
      real xmag(max_epi),xdepth(max_epi),dist(max_epi)
c---event numbers
      integer number(max_epi)
c--covariance matrix elements
       real erx,ery,erz,cvxy,cvxz,cvyz,
     *      erx_a(max_epi),ery_a(max_epi),erz_a(max_epi),
     *      cvxy_a(max_epi),cvxz_a(max_epi),cvyz_a(max_epi)
c--profile azimuth
      real dirnab
c--dimention of error ellipse
      real emaj,emin,ang
c--- dir structure
      character*80 dir
c---section  definitions in tec coordinates
      real x1,x2,x3,y1,y2,y3           
      real     lat_profile(3), lon_profile(3),   ! Lat-long equivalents.
     &         x_profile(3),   y_profile(3)      ! Of these associations.
      equivalence (x_profile(1), x1),            ! :....
     &            (y_profile(1), y1),            !
     &            (x_profile(2), x2),            !
     &            (y_profile(2), y2),            !
     &            (x_profile(3), x3),            !
     &            (y_profile(3), y3)             !
C
c---width of profile 
      real width
c---number of points selected
      integer nout
c
c---mapfiles to  use
      character*80 mapfile(map_n$),mfile   !
c
c---question
      character*80 question
c--rectangel outile
      real xr1,xr2,xr3,xr4,yr1,yr2,yr3,yr4
c--help variables 
      real xx1,xx2,yy1,yy2
      character*1 cc
c---number of mapfiles and counter for mapfiles
      integer ncontour,icontour
c---character returned by cursor
      integer ichar
c-- max depth for profile
      real max_depth
c-- slope of section line and line constants
      real a,a1,b1
c-- use old profile values
      logical old_profile
c---flag for plotting error ellipses
      logical elip
c-----variables for plotting positions------------------                        
      real ix,iy                                                                
      logical sun,pc,linux

      include 'version.inc'



c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call get_seisan_def

C    Initialise...
C    =============
C
      code          = e_ok$                     ! Local condition.
      chr_dbg_file$ = 'epimap.dbg'              ! Debug filename.
      b_f_debug$    = .false.                   ! Debug mode.
      b_zoom        = .false.                   ! Zoomed?.
      b_area        = .false.                   ! Area searched?./Plot all
      b_start       = .true.                    ! Flagged to start?.
      nevent1       = 0                         ! Total events.
c
C     File units...
C     -------------
C
      do 1 jx = 1, epifile_n$                   ! Loop epicentre units.
      epiunit(jx) = 0                           ! & initialise unit.
1     continue                                  !
c
      do jx = 1, place_n$                       ! Loop place filename units.
      place_u(jx) = 0                           ! & initialsie.
      end do                                    !
C
      read1  = 0                                ! & unit.
      read2  = 0                                ! & unit.
      read3  = 0                                ! & unit.
      read4  = 0                                ! & unit.
      read5  = 0                                ! & unit.
      read6  = 0                                ! & unit.
c
      write1 = 0                                ! Ditto.
      write2 = 0                                ! Ditto.
      write3 = 0                                ! Ditto.
      write4 = 0                                ! Ditto.
      write5 = 0                                ! Ditto.
      write6 = 0                                ! Ditto.
      write7 = 0                                ! Ditto.
      write8 = 0
c                                                                               
c   graphics options, see routines inout and xtek for more details              
c   --------------------------------------------------------------
c                                                                               
c-- make hard copy file always       
      plotoption=1

      wsize  = X_SCREEN_SIZE$                ! Initialise window size.

      call get_window_size
      if(size_epimap.gt.0) wsize=size_epimap ! from color.def
c
c   display not initially open
c
      disp_open=0		
c
      call computer_type(sun,pc,linux)
      call dir_char(dchar)
c                                                                               
c-- INITIALIZE EVENT COUNTER FOR EVENTS IN FILE                  
      NEVENT1=0		
c-- INITIALIZE EVENT COUNTER FOR SELECTED EVENTS                 
      NEVENT2=0		
C
C   Set up initial input stream (command processor)...
C   ==================================================
C
      call sei cmd set( 'epimap' )          ! For this routine.
C
C   Determine map projection details including scaling factors...
C   -------------------------------------------------------------
C
      call map_dim                                 ! Get details.
      ylatm(1) = MINLATN$                          ! & save.
      ylatm(2) = MAXLATN$        
      xlonm(1) = MINLONG$         
      xlonm(2) = MAXLONG$         
      latdel   = GRID_SPACE_LATN$ 
      londel   = GRID_SPACE_LONG$ 
      latbase  = GRID_BASE_LATN$  
      lonbase  = GRID_BASE_LONG$  
C
C   Continue set up...
C   ==================
C                                                                               
      call sei cmd pro(                                  ! Process.
     &'DO YOU WANT THE EVENTS NUMERATED? (Y/N=RETURN)',  ! Prompt.
     &                 number_plot )                     ! & return string.
      call sei upc( number_plot )                        ! In uppercase.
c 
C   Determine map files to use...
C   =============================
c   get top dir name and make DAT dir name
C   --------------------------------------
c
      if( cmdunit$ .eq. std_in$ ) then               ! Screen is standard input.
      call topdir(top_directory)
      write(6,*) top_directory
      dir = top_directory(:seiclen(top_directory)) //
     &      dchar                                  //
     &      'DAT'                                  //
     &      dchar                                  //
     &      '*.MAP'
c
c   make listing of map files in  DAT
C   ---------------------------------
c
      ncontour=0
      write(*,*)
      write(6,*)' Searching for available map files...'
      write(6,*)
cxx
      text='dirf '//dir(:seiclen(dir))
      write(6,*) text, seiclen(dir)
cxx      call systemc('dirf '//dir(:seiclen(dir)),seiclen(dir)+5)
      call systemc(text,seiclen(text))
      question=
     *' Select number or return for no more selection'                  
c
c   get contour files to use
C   ------------------------
c
555   call filename(question,mfile)
         if(mfile(1:3).ne.'EOF') then
         ncontour=ncontour+1
c
c    Write filename to the output file...
C
            if( ncontour .le. map_n$) then         ! Valid entry.
cc  gfortran pc             if( pc ) then                       ! For pc.
cc               mapfile(ncontour)=dir(1:seiclen(dir)-5)//
cc     &                           mfile             !
cc               else if( sun.or.linux ) then                 ! Or for sun.
               mapfile(ncontour)=mfile
               write(6,*) mfile             !
cc               end if                              !
c
            call sei cmd pro( ' ', mapfile(ncontour) ) ! Store response.
            goto 555
c
c   No more files can be input...
c
            else                                   ! Otherwise.
            write(*,*)                             ! Special message.
     & '**** WARN: list of map files is full...continuing ****'
            end if                                 !
         endif                                     !
C
C    Put end of list marker in output file when finished...
C    ------------------------------------------------------
C
      call sei cmd pro( ' ','$$$$$$end of contour list$$$$$$')  !
C
C   Get polyline (map/contour) files from command stream...
C   =======================================================
C
      else                                      ! Otherwise.
      b_flag2  = .false.                        ! Initialise list flag.
      ncontour = 0                              ! Initialise item count.
556   call sei cmd pro(' ',chr_text)            ! Get text.
c
c    End of list...
c    --------------
c
         if( chr_text(:6) .eq. '$$$$$$') then       ! End of list.
         b_flag2 = index(chr_text,'contour') .eq. 0 ! Skip if incorrect?.
c
            if( b_flag2 ) then                      ! Incorrect marker.
            write(*,*)                              ! Warn.
     & '**** WARN: terminator for list of map/contour     ****'
            write(*,*)                              !
     & '****       files is not as expected, ...skipping  ****'
            write(*,*)                              !
     & '****       remaining items in list...             ****'
            write(*,*)                              !
     & '...........', chr_text(:seiclen(chr_text))
            goto 556                                !
            end if                                  !
C
C    Too many items flagged, skip item...
C    ------------------------------------
C
         else if( b_flag2  ) then                  ! Skip item.
         write(*,*)                             !
     & '...........', chr_text(:seiclen(chr_text)) 
         goto 556                               !
c
c   Enter map contour details...
c   ----------------------------
c
         else if( ncontour .lt. map_n$) then    ! Otherwise valid entry.
         ncontour = ncontour + 1                ! Increment entry.
         mapfile(ncontour) = chr_text           ! & enter it.
         goto 556                               ! Next entry.
c
c   Invalid list items...
C   ---------------------
c
         else                                   ! Otherwise. 
         b_flag2 = .true.                       ! Flag this condition.
         write(*,*)                             ! Special message.
     & '**** WARN: list of map/contour files is too large ****'
         write(*,*)                             !
     & '****       skipping remaining items in list...    ****'
         write(*,*)                             !
     & '...........', chr_text(:seiclen(chr_text)) 
         goto 556                               !
         end if                                 !
      end if                                    !
C                                                                               
C   INPUT title                                                                 
C   ===========
C                                                                               
      call sei cmd pro(                                   ! Process.
     &'Plot title (max 60 chars), or press <return> for none: ', ! Prompt.
     &                 TITLE )                            ! & get a response.
c
c   input if elliptical errors plotted
c   ==================================
c
      call sei cmd pro(                                    ! Process.,
     &              'Plot error ellipses: e or fps: f (n=return)?: ', ! Prompt.
     &                 answer )                            ! & get an answer.
      call sei upc( answer )                               ! In uppercase.
C
C    Do we process ellipses or fault plane solutions?...
C    --------------------------
C
      elip=.false.
      fault=.false.

      if(answer.eq.'E') elip=.true.
      if(answer.eq.'F') fault=.true.
c      elip = answer .eq. 'Y'                               ! Process?.
c
c   input if contour levels plotted
c   ==================================
c
3692  call sei cmd pro(                                    ! Process.,
     &'File name for contour levels, or press <return> for none: ', ! Prompt.
     &                 contour_file )                      ! & get an answer.
c
c   check if file there, if chosen....
c   (...only checks, look at b_flag, not code which is zero since no error
c       if not there...(JAB))
c
      if( contour_file .ne. ' ' ) then     ! File response given.
      call sei open( check$,               ! Check file exists.
     &               ' ',                  ! No prompt.
     &               contour_file,         ! File to open.
     &               ncon,                 ! On unit.
     &               b_flag,               ! File exists.
     &               code )                ! Returned condition.
C
C     File does not exist...
C
         if( .not.b_flag ) then
         write(*,*)
         write(*,*)
     &'**** WARN: specified file does not exist... try again ****'
         write(*,*)
         goto 3692
         endif
      endif
C                                                                               
C   =================================================================
C   CHECK IF PLOT ANY OR ALL STATIONS, OR OTHER PLACES WITH NAMES....
C   =================================================================
C   DEFAULT IS TO PLOT no stations                                              
C                                                                               
      n_place = 0                                            ! Place name count.
      b_place = .false.                                      ! Not found!
c
8787  write(*,*)
      if( .not. b_place ) then                               ! Places not chosen
      write(*,*)
     &' Plot place names (P) or'
      end if                                                 !
c
      write(*,*)
     &' Plot all (a) or some (s) stations with a label,'
      write(*,*)
     &' all stations without a label (x), or none <return>?'
      write(*,*)
     &' ...Enter in uppercase if you wish the symbols to be filled'
      write(*,*)
     &' and this facility is available...'
      call sei cmd pro(                                      ! Process.
     &                 ': ',                                 ! Prompt.
     &                 ANSWER_STAT )                         ! For an answer.
      b_sym_stat_fill$ = answer_stat .ge. 'A'   .and.        ! Fill in uppercase
     &                   answer_stat .le. 'Z'                !
      call sei upc( answer_stat )                            ! In uppercase.
      b_place = answer_stat .eq. 'P' .and. n_place .eq. 0    ! Place names?.
C
C     ========================
C     Get place names files...
C     ========================
C
      if( b_place ) then                   ! Place files required
      b_flag2   = .false.                  ! No skipping or end of list
      write(*,*)
      write(*,*)'Available colour index values are:'
      do 8789 jx = 1, xcolour_n$           ! Loop colours.
      write(*,*)' ',colour$(jx),') ',chr_colour$(jx)
8789  continue                             !
      write(*,*)
c
8785  write(*,*)
      write(*,*)
     &' Enter place location filename and colour index,'
      write(*,*)
     &' separated by a blank, otherwise press <return>'
C
C    Read from screen...
C    ===================
C
         if( cmdunit$ .eq. std_in$ ) then            ! Read from screen.
         read(*,'(a)') chr_text                      ! Read item.
c
c     End of list...
c     --------------
c
8786         if( chr_text .eq. ' ' ) then                ! Add marker.
             b_flag2 = .true.                            ! Flag end of list.
             call sei cmd pro( ' ',                      !
     &         '$$$$$$end of placename file list$$$$$$') !
c
c     Placenames file array is full...
c     --------------------------------
c
             else if( n_place .eq. place_n$) then     ! Too many entries.
             chr_text = ' '                           ! empty.
             write(*,*)                               ! Special message.
     &'**** WARN: list of placename files is full...continuing  ****'
             goto 8786                                ! End of list marker.
C
C     Enter Details in batch file...
C     ------------------------------
C
             else                                     ! Otherwise.
             call sei cmd pro( ' ', chr_text )        ! Make entry.
             end if                                   !
C
C    Read from command file...
C    =========================
C
         else                                     ! Otherwise from command file.
         call sei cmd pro( ' ', chr_text )           ! Get entry.
c
c     End of file...
c     --------------
c
            if( chr_text(:6) .eq. '$$$$$$') then         ! End of list.
            b_flag2 = index(chr_text,'placename') .eq. 0 ! Invalid, then skip?
c
               if( b_flag2 ) then                      ! Invalid terminator
               write(*,*)                              ! Warn.
     & '**** WARN: terminator for list of placenames is not ****'
               write(*,*)
     & '****       as expected, ...skipping remaining       ****'
               write(*,*)                              !
     & '****       items in list...                         ****'
               write(*,*)                              !
     & '...........', chr_text(:seiclen(chr_text))
               goto 8785                               !
c
               else                                    ! Otherwise finished.
               b_flag2 = .true.                        ! Flag but no next item.
               end if                                  !
c
c     Skip list items...
c     ------------------
c
            else if( b_flag2 ) then                ! Flag to skip.
            write(*,*)                             !
     & '...........', chr_text(:seiclen(chr_text))
            goto 8785                              !
c
c     Too many placename files in command file...
c     -------------------------------------------
c
            else if( n_place .eq. place_n$ ) then         ! Too many entries.
            b_flag2 = .true.                              ! Set to skip.
            write(*,*)                                    ! Special message.
     & '**** WARN: list of placename files is too large   ****'
            write(*,*)                                    !
     & '****       skipping remaining items in list...    ****'
            write(*,*)                                    !
     & '...........', chr_text(:seiclen(chr_text))
            goto 8785                                     !
            end if                                        !
         end if                                           !
C
C     Decode the item to retrieve epicentre filename and colour index...
C     ==================================================================
C     End of list...
C     ==============
C
         if( b_flag2 ) then                               ! End of list
         continue                                         ! No action.
C
C     Get colour to plot with...
C     ==========================
C
         else                                        ! Otherwise an entry.
         jy = seiclen(chr_text)                      ! Length of string.
         jx = index(chr_text(:jy),' ')               ! Delimiter.
     &      + 1                                      ! & next character.
C
C     Default...
C     ----------
C
            if( jx .eq. 1 ) then                     ! Default.
            colour = -1                              ! Colour.
C
C     Retrieve and validate colour index supplied...
C     ----------------------------------------------
C
            else                                     ! Otherwise.
            colour = sei integer( chr_text(jx:),     ! Decode colour.
     &                            code )             ! With condition.
               if( code .ne. e_ok$ ) then            ! Error return.
               goto 8785                             ! & try again on error.
C
               else                                  ! Otherwise get colour.
               do 8686 jy = 1, xcolour_n$            ! Loop colours.
                  if( colour .eq. colour$(jy) ) then ! Found in list.
                  goto 8687                          ! Skip out.
                  end if                             !
8686           continue                              !
C
C     invalid colour...
C     -----------------
C
               write(*,*)
               write(*,*)                            !
     &'**** WARN: invalid colour index, ...try aain...  ****'
               goto 8785                             ! & again.
8687           continue                              !
               end if                                !
            end if                                   !
C
C     Open epicentre file...
C     ----------------------
C
         if(jx .gt. 1) chr_text = chr_text(:jx-1)! Just file detail.
         call sei get file( check$,              ! Find file.
     &                      read1,               ! On unit.
     &                      code,                ! Code (n/a).
     &                      'DAT',               ! Alternative dir to search.
     &                      chr_text )           ! For epicentre file.
C
C     Problem with file...
C
            if( code .ne. e_ok$ ) then         ! Does not exist.
            write(*,*)
            write(*,*)
     &'**** WARN: specified file does not exist... try again ****'
            write(*,*)
            goto 8785                          ! Try again.
C
C     File valid and open, store entry...
C
            else                               ! Otherwise.
            call sei open( old$,               ! Open file (stop on error).
     &                     ' ',                ! No prompt.
     &                     chr_text,           ! This filename.
     &                     read1,              ! On unit.
     &                     b_flag,             ! Exists!!
     &                     code )              !
            write(*,*)
            write(*,*)'.... opened "', chr_text(:seiclen(chr_text)),
     &                '" for plotting...'
c
               if( colour .gt. 0 ) then
               write(*,*)'.... colour ',chr_colour$(colour)
               else
               write(*,*)'.... default colour (COLOR.DEF)'
               end if
c
            write(*,*)
            n_place = n_place + 1                  ! Increment.
            place_u(n_place) = read1               ! & unit.
            place_col(n_place) = colour            ! & symbol colour.
C
C    Get next entry...
C    =================
C
            read1 = 0                              ! Re-initialise.
            goto 8785                              ! Next entry.
            end if                                 !
         end if                                    !
c
c    Back to stations list...
c    ========================
c
      b_place = n_place .gt. 0                    ! Some places required.
      goto 8787                                   ! Look at stations.
C
C     ========================
C     Get selected stations...
C     ========================
C
       ELSE IF( ANSWER_STAT .EQ. 'S' ) THEN                        
       write(*,*)
     &' Enter stations (one per line) & end list with <return>'
       b_flag2 = .false.                         ! Skip not required yet!.
       NSTATION_PLOT=0                                                       
c
c     Read from screen...
c     ===================
c
435       if( cmdunit$ .eq. std_in$ ) then       ! Read from screen.
          read(*,'(a)') chr_text                 ! Read item.
c
c     End of list...
c     --------------
c
436          if( chr_text .eq. ' ' ) then                ! Add marker.
             call sei cmd pro( ' ',                      ! 
     &                '$$$$$$end of station list$$$$$$') !
c
c     Stations array is full...
c     -------------------------
c
             else if( nstation_plot .eq. plotstat_n$) then ! Too many entries.
             chr_text = ' '                                ! empty.
             write(*,*)                                    ! Special message.
     &'**** WARN: list of stations to plot is full...continuing ****'
             goto 436                                      ! End of list marker.
c
c    Enter station in the plot list...
c    ---------------------------------
c
             else                                          ! Otherwise.
             nstation_plot = nstation_plot + 1             ! Increment.
             plot_sta(nstation_plot) = chr_text(:5)        ! & fill.
             call sei cmd pro( ' ', chr_text )             ! Write to batch.
             goto 435                                      ! Next item.
             end if                                        !
c
c     Read from command file...
c     =========================
c
          else                                        ! Otherwise read file.
          call sei cmd pro( ' ', chr_text )           ! Get station.
c
c     End of file...
c     --------------
c
             if( chr_text(:6) .eq. '$$$$$$') then       ! End of list.
             b_flag2 = index(chr_text,'station') .eq. 0 ! Invalid, then skip?
c
                if( b_flag2 ) then                      ! Invalid terminator
                write(*,*)                              ! Warn.
     & '**** WARN: terminator for list of stations is not ****'
                write(*,*)                              !
     & '****       as expected, ...skipping remaining     ****'
                write(*,*)                              !
     & '****       items in list...                       ****'
                write(*,*)                              !
     & '...........', chr_text(:seiclen(chr_text))
                goto 435                                !
                end if                                  !
c
c     Skip list items...
c     ------------------
c
             else if( b_flag2 ) then                ! Flag to skip.
             write(*,*)                             !
     & '...........', chr_text(:seiclen(chr_text))
             goto 435                               !
c
c     Too many stations in command file...
c     ------------------------------------
c     
             else if( nstation_plot .eq. plotstat_n$) then ! Too many entries.
             b_flag2 = .true.                              ! Set to skip.
             write(*,*)                                    ! Special message.
     & '**** WARN: list of stations is too large          ****'
             write(*,*)                                    !
     & '****       skipping remaining items in list...    ****'
             write(*,*)                                    !
     & '...........', chr_text(:seiclen(chr_text))
             goto 435                                      !
c
c    Enter station in the plot list...
c    ---------------------------------
c
             else                                          ! Otherwise.
             nstation_plot = nstation_plot + 1             ! Increment.
             plot_sta(nstation_plot) = chr_text(:5)        ! & fill.
             goto 435                                      ! Next item.
             end if                                        !
          end if                                           !
       ENDIF                                                                    
C
C     ==================
C     Epicentre files...
C     ==================
c     Preliminaries...
C     ================
c           
      n_epifile = 0                        ! Initialise epifile count.
      b_flag2   = .false.                  ! No skipping or end of list
      write(*,*)
      write(*,*)'Available colour index values are:'
      do 1110 jx = 1, xcolour_n$           ! Loop colours.
      write(*,*)' ',colour$(jx),') ',chr_colour$(jx)
1110  continue                             !
      write(*,*)
C
C    ========================
C    Loop epi centre files...
C    ========================
C
735   write(*,*)
      write(*,*)
     &' Enter epicentre filename and colour index,'
      write(*,*)
     &' separated by a blank, otherwise press <return>'
C
C    Read from screen...
C    ===================
C
      if( cmdunit$ .eq. std_in$ ) then            ! Read from screen.
      read(*,'(a)') chr_text                      ! Read item.
c
c     End of list...
c     --------------
c
736       if( chr_text .eq. ' ' ) then                ! Add marker.
          b_flag2 = .true.                            ! Flag end of list.
          call sei cmd pro( ' ',                      !
     &                '$$$$$$end of epicentre file list$$$$$$') !
c
c     Epicentre file array is full...
c     -------------------------------
c
          else if( n_epifile .eq. epifile_n$) then ! Too many entries.
          chr_text = ' '                           ! empty.
          write(*,*)                               ! Special message.
     &'**** WARN: list of epicentre files is full...continuing  ****'
          goto 736                                 ! End of list marker.
C
C     Enter Details in batch file...
C     ------------------------------
C
          else                                     ! Otherwise.
          call sei cmd pro( ' ', chr_text )        ! Make entry.
          end if                                   !
C
C    Read from command file...
C    =========================
C
      else                                        ! Otherwise from command file.
      call sei cmd pro( ' ', chr_text )           ! Get entry.
c
c     End of file...
c     --------------
c
         if( chr_text(:6) .eq. '$$$$$$') then         ! End of list.
         b_flag2 = index(chr_text,'epicentre') .eq. 0 ! Invalid, then skip?
c
            if( b_flag2 ) then                      ! Invalid terminator
            write(*,*)                              ! Warn.
     & '**** WARN: terminator for list of epicentres is not ****'
            write(*,*)                              !
     & '****       as expected, ...skipping remaining       ****'
            write(*,*)                              !
     & '****       items in list...                         ****'
            write(*,*)                              !
     & '...........', chr_text(:seiclen(chr_text))
            goto 735                                !
c
            else                                    ! Otherwise finished.
            b_flag2 = .true.                        ! Flag but no next item.
            end if                                  !
c
c     Skip list items...
c     ------------------
c
         else if( b_flag2 ) then                ! Flag to skip.
         write(*,*)                             !
     & '...........', chr_text(:seiclen(chr_text))
         goto 735                               !
c
c     Too many epicentre files in command file...
c     -------------------------------------------
c
         else if( n_epifile .eq. epifile_n$ ) then     ! Too many entries.
         b_flag2 = .true.                              ! Set to skip.
         write(*,*)                                    ! Special message.
     & '**** WARN: list of epicentre files is too large   ****'
         write(*,*)                                    !
     & '****       skipping remaining items in list...    ****'
         write(*,*)                                    !
     & '...........', chr_text(:seiclen(chr_text))
         goto 735                                      !
         end if                                        !
      end if                                           !
C
C     Decode the item to retrieve epicentre filename and colour index...
C     ==================================================================
C     End of list...
C     ==============
C
      if( b_flag2 ) then                               ! End of list
      continue                                         ! No action.
C
C     Get colour to plot with...
C     ==========================
C
      else                                        ! Otherwise an entry.
      jy = seiclen(chr_text)                      ! Length of string.
      jx = index(chr_text(:jy),' ')               ! Delimiter.
     &   + 1                                      ! & next character.
C
C     Default...
C     ----------
C
         if( jx .eq. 1 ) then                     ! Default.
         colour = -1                              ! Colour.
C
C     Retrieve and validate colour index supplied...
C     ----------------------------------------------
C
         else                                     ! Otherwise.
         colour = sei integer( chr_text(jx:),     ! Decode colour.
     &                         code )             ! With condition.
            if( code .ne. e_ok$ ) then            ! Error return.
            goto 735                              ! & try again on error.
C
            else                                  ! Otherwise get colour.
            do 1112 jy = 1, xcolour_n$            ! Loop colours.
               if( colour .eq. colour$(jy) ) then ! Found in list.
               goto 1113                          ! Skip out.
               end if                             !
1112        continue                              !
C
C     invalid colour...
C     ----------------- 
C
            write(*,*)
            write(*,*)                            !
     &'**** WARN: invalid colour index, ...try aain...  ****'
            goto 735                              ! & again.
1113        continue                              !
            end if                                !
         end if                                   !
C
C     Open epicentre file...
C     ----------------------
C
      if(jx .gt. 1) chr_text = chr_text(:jx-1)! Just file detail.
      call sei get file( check$,              ! Find file.
     &                   read1,               ! On unit.
     &                   code,                ! Code (n/a).
     &                   'DAT',               ! Alternative dir to search.
     &                   chr_text )           ! For epicentre file.
C
C     Problem with file...
C
         if( code .ne. e_ok$ ) then         ! Does not exist.
         write(*,*)
         write(*,*)
     &'**** WARN: specified file does not exist... try again ****'
         write(*,*)
         goto 735                           ! Try again.
C
C     File valid and open, store entry...
C
         else                               ! Otherwise.
         call sei open( old$,               ! Open file (stop on error).
     &                  ' ',                ! No prompt.
     &                  chr_text,           ! This filename.
     &                  read1,              ! On unit.
     &                  b_flag,             ! Exists!!
     &                  code )              !
         write(*,*)
         write(*,*)'.... opened "', chr_text(:seiclen(chr_text)),
     &             '" for plotting...'
c
            if( colour .gt. 0 ) then
            write(*,*)'.... colour ',chr_colour$(colour)
            else
            write(*,*)'.... default colour (COLOR.DEF)'
            end if
c
         write(*,*)
         n_epifile = n_epifile + 1              ! Increment.
c         file_name(n_epifile) = chr_text(:jx-1) ! Store filename. ! not used ??
         epiunit  (n_epifile) = read1           ! & unit.
         epicolour(n_epifile) = colour          ! & symbol colour.
C
C    Determine filetype...
C    =====================
C
C   filetype 2 is macroseismic file (iso file)
C   filetype 1 is an S-file
C   filetype 0 is a compact file
C
C   find file type  assume macro filename to end with macro 
C   --------------------------------------------------------
C
            i=index(chr_text,'.')
            if( chr_text(i+1:i+3) .eq. 'mac'  .or.            !
     &          chr_text(i+1:i+3) .eq. 'MAC') then            !
            filetype=2
C
C   Filename does not start with "ISO"...
C   -------------------------------------
C
            else                                          !
            filetype=0
            do i=1,10
            read(read1,'(a80)',iostat=code) data(i)      ! Read record.
            call sei code( fort$, code, read1, b_flag  ) ! Process outcome.
            if( b_flag ) goto 345                        ! End of file!.
C
            if(data(i)(22:22).ne.'L'.and.data(i)(22:22).ne.'R'.
     *      and.data(i)(22:22).ne.'D'.and.data(i)(22:22).ne.' ')
     *      filetype=1
	        if(data(i)(1:30).eq.'                              ')
     *      filetype=1
            enddo
            endif
C
C    Inform user of file type...
C    ---------------------------
C
            if( filetype .eq. 0 ) then
            write(*,*)' Input file is compact'
            else if( filetype .eq. 1 ) then
            write(*,*)' Input file is Nordic'
            else if( filetype .eq. 2 ) then
            write(*,*)' Input file is iso'
            end if
C
C    Get next entry...
C    =================
C
345      epitype(n_epifile) = filetype          ! Initialise filetype.
         read1 = 0                              ! Re-initialise.
         goto 735                               ! Next entry.
         end if                                 !
      end if                                    !
C
C    =======================================================
C    Prompt user to indicate symbols plotted by magnitude or
C    by magnitude range...
C    =======================================================
C
      if( n_epifile .gt. 0 )then                ! Files exist.
      write(*,*)
c
         if( b_sym_range$ ) then                ! Default by range.
         write(*,*)
     &'Enter the following in uppercase if you wish the symbols'
         write(*,*)
     &'to be filled and this facility is available...'
         write(*,*)
     &'By default, symbols will be plotted according to'
         call sei cmd pro(
     &'magnitude range, do you wish to plot by magnitude?: ',
     &                    chr_text )            !
         b_sym_epic_fill$ = chr_text(1:1) .eq. 'Y' .or.
     &                      chr_text(1:1) .eq. 'N'
         call sei upc( chr_text )               ! In uppercase.
         call sei left( chr_text )              ! Left justify.
         b_sym_range$ = chr_text(1:1) .ne. 'Y'  ! Range value?.
c
         else                                   ! Otherwise by magnitude.
         write(*,*)
     &'Enter the following in uppercase if you wish the symbols'
         write(*,*)
     &'to be filled and this facility is available...'
         write(*,*)
     &'By default, symbols will be plotted according to'
         write(*,*)
     &'magnitude, do you wish them to be plotted according'
         call sei cmd pro(
     &                    'to magnitude range?: ',
     &                    chr_text )
         b_sym_epic_fill$ = chr_text(1:1) .eq. 'Y' .or.
     &                      chr_text(1:1) .eq. 'N'
         call sei upc( chr_text )               ! In uppercase.
         call sei left( chr_text )              ! Left justify.
         b_sym_range$ = chr_text(1:1) .eq. 'Y'  ! Range value?.
         end if                                 !
      end if                                    !
C
C    =============================================
C    Completed input information....now process...
C    =============================================
C
c      plotoption=1                                                           
c                                                                               
c   Open plot file...
c   -----------------
c                                                                               
      call sei open( unknown$,             ! Open (default stop on error).
     &               ' ',                  ! No prompt.
     &               'epimap.eps',         ! File name to open.
     &               write2,               ! On unit.
     &               b_flag,               ! Flag exists.
     &               code )                ! Condition (n/a).
      plotunit = write2                    ! Store unit for plot file.
c
c   open file for storage surrounding points...
c   -------------------------------------------
c
      call sei open( unknown$,             ! Open (default stop on error).
     &               ' ',                  ! No prompt.
     &               'epimap.cor',         ! File name to open.
     &               write6,               ! On unit.
     &               b_flag,               ! Flag exists.
     &               code )                ! Condition (n/a).
c
c   open output epi file...
c   -----------------------
c
      call sei open(unknown$,              ! Open (default is stop on error).
     &              ' ',                   ! No prompt.
     &              'epimap.num',          ! Filename to open.
     &              write3,                ! On unit.
     &              b_flag,                ! Flag existance.
     &              code )                 ! Condition.
c
      call sei open(unknown$,              ! Open (default is stop on error).
     &              ' ',                   ! No prompt.
     &              'epimap.out',         ! Filename to open.
     &              write8,                ! On unit.
     &              b_flag,                ! Flag existance.
     &              code )                 ! Condition.
c
c   open the area contents file...
c   ------------------------------
c
      call sei open( unknown$,             ! Open (default stop on error).
     &               ' ',                  ! No prompt.
     &               'epimap.are',         ! File name to open.
     &               write7,               ! On unit.
     &               b_flag,               ! Flag exists.
     &               code )                ! Condition (n/a).
c
c----------------------------------------------------------------------
C   Plots & replots...
c----------------------------------------------------------------------
C   MOVE ORIGIN, WHICH IS IN LOWER LEFT HAND CORNER.
C   ALL PLOT UNITS ARE IN tectronix 4010 screen units units
c   REFX0$,REFY0$ is lower left hand corner of widest possible map
C-------------------------------------------------
c
      epicentre_n$ = 0                        ! Initialise maximum epicentres.
      b_epiload    = n_epifile .gt. 0         ! Load epifiles?.
C
C   Re-set scaling...
C   -----------------
C
2000  call map_dim                           ! Find required scaling.
C
C   Rewind all output files...
C   --------------------------
C
      rewind( write6, iostat=code )          ! Rewind epimap.cor.
      call sei code(fort$,code,write6,b_flag)! Process outcome.
      rewind( write7, iostat=code )          ! Rewind epimap.are.
      call sei code(fort$,code,read5,b_flag) ! Process outcome.
c                                                                               
c  Re-set the PostScript...
c  ------------------------
c
      call new_diagram( map$,                        ! Map plot.
     &                  b_start,                     ! Starting new plot?.
     &                  b_page,                      ! On new page?.
     &                  write2 )                     ! PostScript output.
c
      call xset_color(color_title)                   ! Set title colour.
      call xchars(title,seiclen(title),              ! Insert the plot title.
     &            border_x$,title_bottom$)           !
c
c  plot contour levels...
c  ----------------------
c
      if(contour_file(1:1).ne.' ') then
      call sei open( old$,                           ! Open and stop if error.
     &               ' ',                            ! No prompt.
     &               contour_file,                   ! File to open.
     &               ncon,                           ! On unit.
     &               b_flag,                         ! File exists.
     &               code )                          ! Returned condition.
      call contor(ncon)                              ! & contour...
      call sei close( close$, ncon, code )           ! Close contour level file
      endif
C                                                                               
C   =========================
C   Load in the epicentres...
C   =========================
C                                                                               
      IF( B_EPILOAD ) THEN                           ! Load epicentre files.
      b_epiload = .false.                            ! Re-set flag.
cc   gfortran pc      if( pc ) then
cc         continue
cc         else
         write(*,*)
         write(*,*)
     &   '==== Loading Epicentres...'
         write(*,*)
cc         end if
c
c   Preliminaries...
c   ================
c
      do 10 jy = 1, n_epifile         ! Loop epifiles.
      read1   = epiunit(jy)           ! & unit.
      filetype= epitype(jy)           ! & filetype.
      colour  = epicolour(jy)         ! & colour.
c
c   File preliminaries...
c   ---------------------
c
      rewind( read1, iostat=code )           ! Rewind file.
      call sei code(fort$,code,read1,b_flag) ! Process outcome.
C
         if(filetype.eq.2) then                 !
         read(read1,'(a)',iostat=code) text     !
         read(read1,'(a)',iostat=code) text     !
         call sei code(fort$,code,read1,b_flag) ! Process outcome.
         end if                                 !
C                                                                               
C   Input an event...
C   =================
C   Compact file...
C   ---------------
c                                                                               
9        IF(FILETYPE.EQ.0) THEN                      !
         READ(read1,'(A80)',iostat=code) DATA(1)     ! Read record.
         nrecord=1
         call sei code( fort$, code, read1, b_flag ) ! Process outcome.
c
c   End of file...
c
            if( b_flag ) then                        ! End of file.
            goto 10                                  ! Next file.
C
C   Decode the record...
C  
            else                                     ! Otherwise.
            READ(DATA(1),'(21X,2A1,25X,I3)') TYPE,EXP,NSTAT
            end if                                   !
C
C    S-file...
C    ---------
C
         ELSE IF( FILETYPE .EQ. 1 ) THEN               ! a whole event
         CALL INDATA(read1,NSTAT,NPHASE,NHEAD,NRECORD, !
     &               TYPE,EXP,DATA,ID)                 !
c
c    End of file...
c
            IF( NRECORD .EQ. 0 ) THEN                  ! End of file.
            GOTO 10	                               ! Next file.
c
c    Ellipses requested...Get error covariance matrix...
c
            else if( elip ) then                       ! Requested.
            erx = -1.0                                 ! Initialise.
            DO I = 2, NHEAD                            ! Loop records.
               IF(DATA(i)(80:80).EQ.'E') THEN          ! If ellipses.
               READ(DATA(I),'(23X,F7.3,1X,F7.3,F5.1,3E12.4)')
     *                        ERY,ERX,ERZ,cvxy,cvxz,cvyz
               endif                                   !
            enddo

c    Ellipses not requested...
c
            else                                       ! Otherwise.
            erx = -1.0                                 ! Disable
            end if                                     !

c
c   fault plane solutions requested
c           
         
            if(fault) then
               strike(nevent2+1)=1000.0   ! no fps initially
               do i=2,nhead
                 if(data(i)(80:80).eq.'F') then
                    read(data(i),'(3f10.1)') strike(nevent2+1),
     *              dip(nevent2+1),rake(nevent2+1)
                    goto 4527
                 endif
               enddo
 4527          continue
            endif
          
c
c    Macroseismic (ISO) file...
c    --------------------------
c
         ELSE IF( FILETYPE .EQ. 2 ) THEN                ! Macroseismic file.
c old format         read(read1,'(28X,2F6.2,10X,F3.1)',iostat=code) ! Read data.
c  old format     *                slat(1),slon(1),smag              !
         read(read1,'(2F10.3,F5.1)',iostat=code) ! Read data.
     *                slat(1),slon(1),smag              !
c           write(*,*) slat(1),slon(1),smag
         call sei code( fort$, code, read1, b_flag  )   ! Process outcome.
c
c    End of file...
c
            if( b_flag ) then                           ! End of file.
            goto 10                                     ! Next file
            end if                                      !
         end if                                         !
C                                                                               
C     Read parameters from header if not isoseismic data...
C     -----------------------------------------------------
c
         if( filetype .ne. 2 ) then                     ! Not macroseismic.
         read(data(1),100,err=2290)nyr,nmon,nday,nhr,nmin,                               
     *   slat(1),slon(1),depth,rms,smag,smag2,smag3                             
 100     format(1x,i4,1x,2i2,1x,2i2,8x,f7.3,f8.3,f5.1,8x,f4.1,               
     *   f4.1,5x,f3.1,5x,f3.1)                          !JAB(BGS)May95.
         goto 2291
 2290    continue
         write(6,*)' Error reading hypocenter etc'
         write(6,'(a)') data(1)
         stop
 2291    continue
c   
c   check if any high accuracy data and read if there
c
          if(nhead.gt.2) then
             do i=2,nhead
                if(data(i)(80:80).eq.'H') then
                    read(data(i)(24:60),'(f9.5,1x,f10.5,1x,f8.3)')
     *              slat(1),slon(1),depth
                endif
              enddo
          endif
c
c    Since very early Nordic format (BGS) had integer depths, use the following
c    to get depth irrespective of format in this field...
c
         depth = sei real num( data(1)(39:43), code )   ! Get depth again.
C                                                                               
C     if coda-magnitude = 0.0 another magnitude is used...
C                                                                               
            if( smag  .eq. 0.0   .and.                  ! Re-set?.
     &          smag3 .gt. 0.0 ) then                   !
            smag = smag3                                !
            end if                                      !
c
            if( smag  .eq. 0.0   .and.                  ! Re-set?.
     &          smag3 .eq. 0.0   .and.                  !
     &          smag2 .gt. 0.0 ) then                   !
            smag = smag2                                !
            end if                                      !
         endif
C                                                                               
C   Check if event is in the geographical window, count events in input file..
C   ==========================================================================
C                                                                               
      nevent1 = nevent1 + 1                     ! Increment total events
c
c   Latitude...
c   -----------
c
         if( slat(1) .gt. maxlatn$   .or.       ! Out of latitude range?.
     &       slat(1) .lt. minlatn$ ) then       !
         goto 9                                 ! Next event.
c
c   Longitude...
c   ------------
C   Account for traversing the International dateline...
c
         else                                   ! Otherwise.
            if( maxlong$ .gt. 180.0   .and.
     &          minlong$ .lt. 180.0   .and.
     &          minlong$ .gt.   0.0   .and.
     &          slon(1)  .lt.   0.0 ) then
            slon(1) = slon(1) + 360.0
c
            else if( minlong$ .lt. -180.0   .and.
     &               slon(1)  .gt.    0.0 ) then 
            slon(1) = slon(1) - 360.0
            end if
c
c    & check within range...
c
C
            if( slon(1) .gt. maxlong$   .or.     !
     &          slon(1) .lt. minlong$ ) then     !
            goto 9                               ! Next event.
            end if                               !
         end if                                  !
c                                                                               
c   count events found inside window...
c   -----------------------------------
c                                                                               
      epicentre_n$ = epicentre_n$ + 1               ! & increment.
c
         if( epicentre_n$ .gt. max_epi ) then       ! Exceeds array size.
cc   gfortan pc         if( pc ) then
cc            continue
cc            else
            write(*,*)                              !
            write(*,*)                              ! Message.
     &'**** WARN: epicentre array size exceeded...continuing'
            write(*,*)                              !
cc            end if
         goto 6666                                  ! & skip out.
c
         else                                       ! Otherwise.
         nevent2 = epicentre_n$                     ! Current plot events.
         end if                                     !
c
c    Store Epicentre values...
c    =========================
c    Write out details to output epifile if not macroseismic...
c    ----------------------------------------------------------
c
         if( filetype .ne. 2 ) then                   ! Not macroseismic.
         write( write3,                               ! Write details.
     &          '(i5,1x,a72)',                        ! On format.
     &          iostat=code)                          ! Condition.
     &          epicentre_n$, data(1)(1:72)           !
         call sei code( fort$, code, write3, b_flag ) ! Process condition.
         write( write8,                               ! Write details.
     &          '(a)',                        ! On format.
     &          iostat=code)                          ! Condition.
     &          (data(i),i=1,nrecord)    !
         call sei code( fort$, code, write3, b_flag ) ! Process condition.
         call sei code( fort$, code, write8, b_flag ) ! Process condition.
         end if                                       !
c                                                                
c    Ellipse values...
c    -----------------
c
         if( elip ) then                              ! Ellipse required.
         erx_a(nevent2)  = erx
         ery_a(nevent2)  = ery
         erz_a(nevent2)  = erz
         cvxy_a(nevent2) = cvxy
         cvxz_a(nevent2) = cvxz
         cvyz_a(nevent2) = cvyz
         endif
c
c    Other values...
c    ---------------
c
      xx(nevent2)     = slon(1)              ! Longitude.
      yy(nevent2)     = slat(1)              ! Latitude.
      xdepth(nevent2) = depth                ! Depth.
      xmag(nevent2)   = smag                 ! Magnitude.
      mag_min         = amin1(mag_min,smag)  ! Minimum magnitude.
      mag_max         = amax1(mag_max,smag)  ! Maximum magnitude.
c
      chr_epi_plt(nevent2) = 'P'                  ! Set to plot.
      write(chr_epi_col(nevent2),'(i2)') colour   ! Save colour.
      write(chr_epi_typ(nevent2),'(i1)') filetype ! & filetype.
c
c    Bubble sort the magnitude, largest first...
c
      epiorder(nevent2) = nevent2              ! Magnitude pointer.
         if( nevent2 .gt. 1 ) then             ! Bubble sort.
         do jx = nevent2, 2, -1                ! Look back.
            if(xmag(jx) .gt. xmag(jx-1) ) then ! Swap.
            jz             = epiorder(jx)      ! Store location.
            epiorder(jx)   = epiorder(jx-1)    ! Replace.
            epiorder(jx-1) = jz                ! Ditto.
            end if                             !
         end do                                !
         end if                                !
c
c    Another event...
c    ----------------
c                                                                               
      goto 9                                                                   
10    continue                                                                  
c
c    Close epimap.num file...& open up for reading...
c    ------------------------------------------------
c
6666  call sei close( close$, write3, code ) ! Close epimap.num.
      call sei close( close$, write8, code ) ! Close epimap.out.
      call sei code(fort$,code,write3,b_flag)! Process outcome.
      call sei open( old$,                   ! Open (stop on error).
     &               ' ',                    ! No prompt.
     &               'epimap.num',           ! File name to open.
     &               read6,                  ! On unit.
     &               b_flag,                 ! Flag exists.
     &               code )                  ! Condition (n/a).
      call sei open( old$,                   ! Open (stop on error).
     &               ' ',                    ! No prompt.
     &               'epimap.out',           ! File name to open.
     &               write8,                  ! On unit.
     &               b_flag,                 ! Flag exists.
     &               code )                  ! Condition (n/a).
c
         if( b_f_debug$ ) then
         write(dbgunit$,*)'epicentre values...'
         write(dbgunit$,*)'nevent2...', nevent2
         write(dbgunit$,*)'xmag...', (xmag(jx),jx=1,nevent2)
         write(dbgunit$,*)'xdepth.', (xdepth(jx),jx=1,nevent2)
         write(dbgunit$,*)'colour.', (chr_epi_col(jx),jx=1,nevent2)
         write(dbgunit$,*)'order..', (epiorder(jx),jx=1,nevent2)
         write(dbgunit$,*)'.....end of values..'
         end if
C
C    ==================================================
C    Start with no epifiles...dummy open for reading...
C    ==================================================
C
      ELSE IF( write3 .gt. 0 ) THEN          ! Otherwise close epimap.num.
      call sei close( close$, write3, code ) ! Close epimap.num.
      call sei code(fort$,code,write3,b_flag)! Process outcome.
      call sei open( old$,                   ! Open (stop on error).
     &               ' ',                    ! No prompt.
     &               'epimap.num',           ! File name to open.
     &               read6,                  ! On unit.
     &               b_flag,                 ! Flag exists.
     &               code )                  ! Condition (n/a).
      END IF                                 ! Finished loading epicentres.
C
C    =======================
C    Plotting starts here...
C    =======================
c    Places...
c    =========
c
      if( n_place .gt. 0 ) then                      ! Places exist.
c
c   Loop place name files...
c   ========================
c   Prelimaries...
c   ==============
c
      right_edge = refx$ + xsize$ - 2.0*lat_txt_dispx$ ! Right edge of plot.
c
      do jy = 1, n_place                             ! Loop places.
      read1   = place_u(jy)                          ! & unit.
      colour  = place_col(jy)                        ! & colour.
      call xset_color(colour)                        ! Set title colour.
c
      rewind( read1, iostat=code )                   ! Rewind unit.
      call seicode( fort$,code,read1,b_flag)         ! Process outcome.
c
c   Read entries for this file...
c   =============================
c
8585  read( read1, '(a)', iostat=code ) chr_text     ! Get entry.
      call seicode( fort$,code,read1,b_flag)         ! Process outcome.
c
c   End of file...
c   --------------
c  
         if( b_flag ) then                           ! End of file.
         continue                                    !
c
c   Extract details...
c   ------------------
c
         else                                        ! Otherwise.
         call sei left( chr_text )                   ! Left justify record.
         jx = index( chr_text, '  ' )                ! Search for double space.
         call sei left( chr_text(jx+1:) )            ! Left justify lat string.
         kx = jx + index(chr_text(jx+1:),' ')        ! & next delimiter.
         call sei left( chr_text(kx+1:) )            ! Left justify long string.
         slat(1) = geo deg(chr_text(jx+1:kx), code ) ! Get latitude.
         slon(1) = geo deg(chr_text(kx+1:),   code ) ! & longitude.
         chr_text(jx:) = ' '                         ! & isolate text.
c
c   Check within range of project area...
c   -------------------------------------
c   Latitude...
c   -----------
c
            if( slat(1) .gt. maxlatn$   .or.       ! Out of latitude range?.
     &          slat(1) .lt. minlatn$ ) then       !
            goto 8585                              ! Next event.
c
c   Longitude...
c   ------------
C   Account for traversing the International dateline...
c
            else                                   ! Otherwise.
               if( maxlong$ .gt. 180.0   .and.
     &             minlong$ .lt. 180.0   .and.
     &             minlong$ .gt.   0.0   .and.
     &             slon(1)  .lt.   0.0 ) then
               slon(1) = slon(1) + 360.0
c
               else if( minlong$ .lt. -180.0   .and.
     &                  slon(1)  .gt.    0.0 ) then
               slon(1) = slon(1) - 360.0
               end if
c
c    & check within range...
c
               if( slon(1) .gt. maxlong$   .or.     !
     &             slon(1) .lt. minlong$ ) then     !
               goto 8585                            ! Next event.
               end if                               !
            end if                                  !
c
c    Now plot....
c    ------------
c
         call map_proj( slon, slat, x, y, 1 )        ! Projection.
         ix = x(1) + REFX0$                          ! Screen location.
         iy = y(1) + REFY0$                          ! Ditto.
c
         call xsymbol(sym_place$+fill$,              ! Plot filled symbol.
     &                2.0*symbol_incr$,
     &                ix,iy)
c
         r_edge = ix + seiclen(chr_text)*nom_chr_size$ ! Nominal text right edge
            if( r_edge .gt. right_edge ) then          ! Plots outside edge.
            ix = ix + right_edge - r_edge              ! Displace to left.
            end if                                     !
         call xchars(chr_text,seiclen(chr_text),ix,iy+10)
         goto 8585                                   ! Another place.
         end if                                      !
      end do                                         !
      end if                                         !
C
C     =============
C     Epicentres...
C     =============
C
      small_scale_y$ = title_bottom$ - 180.0  ! For no epicentres.
c
      if( epicentre_n$ .gt. 0 ) then          ! Epicentres exist.
      colour  = 0                             ! Initialise colour.
      jepi    = nevent2                       ! Store current events.
      nevent2 = 0                             ! & re-initialise.
C
C     Key...
C     ======
C
      call xset_color(color_symbol_key)       ! Set colours.
      if( b_sym_epic_fill$ )then              ! Fill symbol.
      sym_ext = fill$                         ! Fill extension.
      else                                    ! Otherwise.
      sym_ext = 0                             ! None.
      end if                                  !
C
C     Plot magnitude symbol sizes...
C
      b_sym_small_range$ = mag_max .le. max_mag_small_range$ ! Small range?.
c
      if( b_sym_range$ ) then                        ! Plot by magnitude range?.
         if( b_sym_small_range$ )then                ! Small magnitudes.
         MAG_TEXT(1) = '     M< 0.0'
         MAG_TEXT(2) = '0.0<=M< 0.5'
         MAG_TEXT(3) = '0.5<=M< 1.0'
         MAG_TEXT(4) = '1.0<=M< 1.5'
         MAG_TEXT(5) = '1.5<=M< 2.0'
         MAG_TEXT(6) = '2.0<=M< 2.5'
         MAG_TEXT(7) = '     M>=2.5'
         else                                        ! Otherwise.
         MAG_TEXT(1) = '  M<=1'
         MAG_TEXT(2) = '1<M<=2'
         MAG_TEXT(3) = '2<M<=3'
         MAG_TEXT(4) = '3<M<=4'
         MAG_TEXT(5) = '4<M<=5'
         MAG_TEXT(6) = '5<M<=6'
         MAG_TEXT(7) = '  M> 6'
         end if                                      !
c
      else                                    ! Plot by magnitude?
      MAG_TEXT(1)='Unknown'
      MAG_TEXT(2)='M = 1'
      MAG_TEXT(3)='M = 2'
      MAG_TEXT(4)='M = 3'
      MAG_TEXT(5)='M = 4'
      MAG_TEXT(6)='M = 5'
      MAG_TEXT(7)='M = 6'
      end if                                  !
C
      IX = symbol_keyx$
      IY = title_bottom$ - 140.0
c
      text = 'Magnitudes:'
      CALL XCHARS(TEXT,seiclen(text),border_x$,iy)
      iy = iy - 30.0
c
c     Allocate start & end ranges depending on input magnitudes...
c
      if( b_sym_range$ ) then                 !
         if( b_sym_small_range$ ) then        ! Small range.
            if( mag_min .lt. 0.0d0 ) then
            jx = 1
            else if( mag_min .lt. 0.5d0 ) then
            jx = 2
            else if( mag_min .lt. 1.0d0 ) then
            jx = 3
            else if( mag_min .lt. 1.5d0 ) then
            jx = 4
            else if( mag_min .lt. 2.0d0 ) then
            jx = 5
            else if( mag_min .lt. 2.5d0 ) then
            jx = 6
            else
            jx = 7
            end if
         else if( mag_min .le. 1.0d0 ) then
         jx = 1
         else if( mag_min .le. 2.0d0 ) then
         jx = 2
         else if( mag_min .le. 3.0d0 ) then
         jx = 3
         else if( mag_min .le. 4.0d0 ) then
         jx = 4
         else if( mag_min .le. 5.0d0 ) then
         jx = 5
         else if( mag_min .le. 6.0d0 ) then
         jx = 6
         else
         jx = 7
         end if
c
         if( b_sym_small_range$ ) then          ! Small range.
            if( mag_max .lt. 0.0d0 ) then
            jy = 1
            else if( mag_max .lt. 0.5d0 ) then
            jy = 2
            else if( mag_max .lt. 1.0d0 ) then
            jy = 3
            else if( mag_max .lt. 1.5d0 ) then
            jy = 4
            else if( mag_max .lt. 2.0d0 ) then
            jy = 5
            else if( mag_max .lt. 2.5d0 ) then
            jy = 6
            else
            jy = 7
            end if
         else if( mag_max .le. 1.0d0 ) then
         jy = 1
         else if( mag_max .le. 2.0d0 ) then
         jy = 2
         else if( mag_max .le. 3.0d0 ) then
         jy = 3
         else if( mag_max .le. 4.0d0 ) then
         jy = 4
         else if( mag_max .le. 5.0d0 ) then
         jy = 5
         else if( mag_max .le. 6.0d0 ) then
         jy = 6
         else
         jy = 7
         end if
c
         do k = jx, jy                          ! Loop magnitude ranges
         b_force_space$ = .true.                ! Force if non-proportional font
         CALL XCHARS(mag_text(k),seiclen(mag_text(k)),
     &               border_x$,IY)
c
            if( k .eq. 1 ) then                 ! Smallest range "mag" symbol.
            symb_size = 4.0 * symbol_incr$      !
               if( b_sym_small_range$   .and.
     &             mag_min .lt. 0.0d0 ) then    !
               call xsymbol(sym_magneg$+sym_ext,symb_size,
     &                      ix,(iy+0.5*symb_size))
               else
               call xsymbol(sym_mag0$+sym_ext,symb_size,
     &                      ix,(iy+0.5*symb_size))
               end if
c
            else if( k .eq. 7 ) then               ! Largest range "mag" symbol.
               if( b_sym_small_range$ ) then
               symb_size = float(k)  *symbol_incr$ !
               else
               symb_size = float(k-1)*symbol_incr$ !
               end if
            call xsymbol(sym_magx$+sym_ext,symb_size,
     &                ix,(iy+0.5*symb_size))
c
            else if( b_sym_small_range$ ) then
            symb_size = float(k+1)*symbol_incr$ !
            call xsymbol(sym_mag$+sym_ext,symb_size,
     &                   ix,(iy+0.5*symb_size))
C
            else
            symb_size = float(k)*symbol_incr$   ! Other "mag" ranges.
            call xsymbol(sym_mag$+sym_ext,symb_size,
     &                   ix,(iy+0.5*symb_size))
            end if
c
         iy = iy - 30.0             ! Next line on page.
         end do                     !
c
c    Magnitudes are plotted to symbol size depending on magnitude size,
c    Display at given sizes...
c
      else
         do k = 1, 7                            ! Loop sizes.
         b_force_space$ = .true.                ! Force if non-proportional font
c
            if( k .eq. 1 ) then                 ! Smallest range "mag" symbol.
               if( mag_min .lt. 1.0 ) then      !
               CALL XCHARS(mag_text(k),seiclen(mag_text(k)),
     &                     border_x$,IY)
               symb_size = 4.0 * symbol_incr$      !
               call xsymbol(sym_mag0$+sym_ext,symb_size,
     &                      ix,(iy+0.5*symb_size))
               iy = iy - 30.0                      ! Next line on page.
               end if
c
            else if( k .ge. int(mag_min)+1   .and. ! Otherwise.
     &               k .le. int(mag_max)+1 ) then   
            CALL XCHARS(mag_text(k),seiclen(mag_text(k)),
     &                  border_x$,IY)
            symb_size = float(k-1)*symbol_incr$ ! Other "mag" ranges.
            call xsymbol(sym_mag$+sym_ext,symb_size,
     &                   ix,(iy+0.5*symb_size))
            iy = iy - 30.0                      ! Next line on page.
            end if
c
         end do                     !
      end if
c
c     Install Y location of the scale bar for small-scale plots...
c     ------------------------------------------------------------
c
      small_scale_y$ = iy - 40.0
      call xset_color(color_def)
c
c     Loop stored epicentres...
c     =========================
c
      do 7777 jz = 1, epicentre_n$                    ! Loop valid epicentres.
      jx = epiorder(jz)                               ! Get pointer.
         if( chr_epi_plt(jx) .eq. 'P' ) then          ! Must plot.
c
c     ---------------------------------------------------
c     Check points exists within the geographical area...
c     ---------------------------------------------------
c
         slon(1) = xx(jx)                             ! Longitude.
         slat(1) = yy(jx)                             ! Latitude.
c
c   Latitude...
c   -----------
c
            if( jepi       .ne. epicentre_n$ ) then   ! Not full list.
               if( slat(1) .gt. maxlatn$   .or.       ! Out of latitude range?.
     &             slat(1) .lt. minlatn$ ) then       !
               goto 7777                              ! Next event.
c
c   Longitude...
c   ------------
C   Account for traversing the International dateline...
c
               else                                   ! Otherwise.
                  if( maxlong$ .gt. 180.0   .and.
     &                minlong$ .lt. 180.0   .and.
     &                minlong$ .gt.   0.0   .and.
     &                slon(1)  .lt.   0.0 ) then
                  slon(1) = slon(1) + 360.0
c
                  else if( minlong$ .lt. -180.0   .and.
     &                     slon(1)  .gt.    0.0 ) then
                  slon(1) = slon(1) - 360.0
                  end if
c
c    & check within range...
c
C
                  if( slon(1) .gt. maxlong$   .or.     !
     &                slon(1) .lt. minlong$ ) then     !
                  goto 7777                            ! Next event.
                  end if                               !
               end if                                  !
            end if                                     !
c
c    Setup colour for the point...
c    -----------------------------
c
         nevent2    = nevent2 + 1                     ! Increment current events
         old_colour = colour                          ! Store last point colour.
         read(chr_epi_col(jx),'(i2)',err=7778) colour ! Get this colour.
7778        if( colour .ne. old_colour ) then         ! Change in colour.
               if( colour .lt. 0 ) then               ! Default.
               call xset_color(color_epi)             ! Colour.
               else                                   ! Override.
               call xset_color( colour )              ! Set it.
               end if                                 !
            end if                                    !
c
c    Get screen co-ordinates...
c    --------------------------
c
         call map_proj( slon, slat, x, y, 1 )          ! Projection.
         ix = x(1) + REFX0$                            ! Screen location.
         iy = y(1) + REFY0$                            ! Ditto.
c
c   plot fault plane solutions
c  
cxx
         if(fault.and.strike(jx).lt.1000.0) then   
            call plot_foc(strike(jx),dip(jx),rake(jx),ix,iy,16.0,0)
         endif    
c
c    Ellipse values...
c    -----------------
c
            if( elip.and.(.not.fault)) then              ! Ellipse required.
            erx  = erx_a(jx)                           ! Details.
            ery  = ery_a(jx)                           ! Ditto.
            erz  = erz_a(jx)                           !
            cvxy = cvxy_a(jx)                          !
            cvxz = cvxz_a(jx)                          !
            cvyz = cvyz_a(jx)                          !
c
c   plot if errors present, but not if larger than 500.0
c
               if( erx .gt. 0.0     .and.                 !
     &             erx .le. 500.0   .and.                 !
     &             ery .le. 500.0 ) then                  !
               call xy_ellipse( erx,  ery,  erz,          ! Get details
     &                          cvxy, cvxz, cvyz,         !
     &                          emaj, emin, ang )         ! For drawing.
               call hyperr( slon, slat,                   ! Ellipses.
c     &                      float(refx0$), float(refy0$), ! 
     &                      refx0$, refy0$, ! 
     &                      emaj, emin, ang,              !
     &                      minlong$,      maxlong$,      !
     &                      minlatn$,      maxlatn$ )     !
               endif                                      !
            endif                                         !
c
c    Other values...
c    ---------------
c
         smag  = xmag(jx)                       ! Magnitude.
         read(chr_epi_typ(jx),'(i1)') filetype  ! & filetype.
c
c    plot normal magnitude symbols if not ellipse or no errors present
c    or errors too large...
c
            if( (.not.elip.or.(fault.and.strike(jz).gt.500)) .or.     !
     &          (elip           .and.           !
     &           erx .lt. 0.0 )        .or.     !
     &          (elip           .and.           !
     &           erx .gt. 500.0)       .or.     !
     &          (elip           .and.           !
     &           ery .gt. 500.0) )     then     !
c
c    plot events, if type is 2 plot intensities...
c
               if( filetype .eq.2) then
               write(text,'(i1)') int(smag)
               call xchars(text,1,ix+10,iy+10)
c
c    plot by magnitude...pro-rata...
c
               else if( .not.b_sym_range$ ) then   ! Plot by size.
                  if( smag .le. 0.0 ) then         ! Special sysmbol.
                  call xsymbol(sym_mag0$+sym_ext,  !
     &                         4.0*symbol_incr$,
     &                         ix,iy)
                  else                             ! Pro-rata.
                  symb_size = smag*symbol_incr$    !
                  call xsymbol(sym_mag$+sym_ext,symb_size,ix,iy) !
                  end if                           !
c
c    Plot by range allocations...
c
               else if( b_sym_small_range$ ) then
                  if( smag .lt. 0.0d0 ) then
                  symb_size = 4.0*symbol_incr$    !
                  call xsymbol(sym_magneg$+sym_ext,
     &                         symb_size,
     &                         ix,iy)
c
                  else if( smag .ge. 2.5 ) then     !
                  symb_size = 7.0*symbol_incr$    !
                  call xsymbol(sym_magx$+sym_ext,symb_size,ix,iy)!
c
                  else
                     if( smag .lt. 0.5d0 ) then
                     jy = 2
                     else if( smag .lt. 1.0d0 ) then
                     jy = 3
                     else if( smag .lt. 1.5d0 ) then
                     jy = 4
                     else if( smag .lt. 2.0d0 ) then
                     jy = 5
                     else if( smag .lt. 2.5d0 ) then
                     jy = 6
                     end if
c
                  symb_size = float(1+jy)*symbol_incr$          !
                  call xsymbol(sym_mag$+sym_ext,symb_size,ix,iy)!
                  end if
c
               else if( smag .le. 1.0d0) THEN
               call xsymbol(sym_mag0$+sym_ext,
     &                      4.0*symbol_incr$,
     &                      ix,iy)
c
               else if( smag .gt. 6.0d0) THEN
               symb_size = 6.0*symbol_incr$            !
               call xsymbol(sym_magx$+sym_ext,symb_size,ix,iy) !
c
               else
                  if( smag .le. 2.0d0 ) then
                  jy = 2
                  else if( smag .le. 3.0d0 ) then
                  jy = 3
                  else if( smag .le. 4.0d0 ) then
                  jy = 4
                  else if( smag .le. 5.0d0 ) then
                  jy = 5
                  else if( smag .le. 6.0d0 ) then
                  jy = 6
                  end if
c
               symb_size = float(jy)*symbol_incr$            !
               call xsymbol(sym_mag$+sym_ext,symb_size,ix,iy)!
               endif
            endif
c
c   plot event number if required
c
            if( number_plot.EQ.'Y' ) then
            write(text,'(i6)') jx
            call sei left(text)
            if(fault) then
               call xchars(text,seiclen(text),ix,iy-30)
            else
               call xchars(text,seiclen(text),ix,iy-20)
            endif
            endif
c
c  make a stroke
c
         call xout(10.0,10.0)                 !
         end if                               !
7777  continue                                !
      end if                                  !
C
C    Events detail...
C    ================
C
      if( epicentre_n$ .gt. 0 ) then          ! Epicentres exist.
      ix = border_x$                          ! Left   edge.
      iy = event_title_y$                     ! Bottom edge.
      call xset_color(color_symbol_key)       ! Allocate colour.
c
      write(chr_text(:6),'(i6)') nevent1
      call sei left( chr_text(:6) )
      text = 'Total    events: ' // chr_text(:6)
      call xchars( text, seiclen(text), ix, iy )
c
      iy = event_title_y$ - 4.0*allowance$    ! Adjust.
      write(chr_text(:6),'(i6)') nevent2
      call sei left( chr_text(:6) )
      text = 'Selected events: ' // chr_text(:6)
      call xchars( text, seiclen(text), ix, iy)
      end if                                  !
C                                                                               
C   ================
C   Plot Stations...
C   ================
C                                                                               
      if( answer_stat .ne. ' ' ) then         ! Stations required.
      call xset_color(color_station)          ! Setup their colour.
         if( b_sym_stat_fill$ ) then          ! Fill symbols.
         sym_ext = fill$                      ! Symbol extension indicator.
         else                                 ! Otherwise.
         sym_ext = 0                          ! No extension.
         end if                               !
C
C    Open up the station file...
C    ---------------------------
C
         if( read2 .eq. 0 ) then               ! Must open
         chr_text = 'STATION0.HYP'             ! Station filename.
         call sei get file( open$+ignore$,     ! Find & open file.
     &                      read2,             ! On unit.
     &                      code,              ! Code (n/a).
     &                      'DAT',             ! Alternative dir to search.
     &                      chr_text)          ! For station file.
c
            if( code .ne. e_ok$ ) then         ! Does not exist.
            chr_err_msg$ = chr_text(:seiclen(chr_text)) //
     &                     ' does not exist'
            call sei code( stop$,              ! Halt the program with user-.
     &                     e_misf$,            ! Message; dummy use of code.
     &                     0,                  ! Unit (n/a).
     &                     b_flag )            ! Flag (n/a).
            end if                             !
c
         else                                  ! Otherwise.
         rewind( read2, iostat=code )          ! Rewind file.
         call sei code(fort$,code,read3,b_flag)! Process outcome.
         end if                                !
C                                                                               
C   Find first line in file with stations...
C   ----------------------------------------
C                                                                               
      sta = '*****'
      do while (sta .ne. '     ')                !
      read(read2,'(A4)',iostat=code) sta       ! Stations!!?.
      call sei code(fort$,code,read2,b_flag)   ! Process outcome.
      enddo                                                                     
C                                                                               
C   Read and plot station...
C   ------------------------
C                                                                               
      sta = '*****'
c
c-- Indicator whether station should be plotted...
c
      station_ok = 1		
      do while (sta .ne. '    ')
      read(read2,'(a)',iostat=code) text
      if(text(2:2).eq.' ') then    
         read(text,'(2x,a4,i2,f5.3,a1,i3,f5.3,a1)',iostat=code)                
     *             sta(1:4),ilat,slat(1),ns,ilon,slon(1),ew                                    
         sta(5:5)=' '
      else
         read(text,'(1x,a5,i2,f5.3,a1,i3,f5.3,a1)',iostat=code)                
     *             sta,ilat,slat(1),ns,ilon,slon(1),ew                                    
      endif
      call sei code(fort$,code,read2,b_flag)   ! Process outcome.
C                                                                               
C   Check if station should be plotted...
C                                                                               
         if( answer_stat .eq. 'S' ) then                      
         station_ok = 0   
         do kst = 1, nstation_plot                                              
c
c-- Check input stations...
c
         if( sta .eq. plot_sta(kst) ) station_ok = 1 	
         enddo                                                               
         endif                                                                  
c                                                                               
c  plot station...
c                                                                               
         if( station_ok .eq. 1) then 
         slat(1) = ilat + slat(1)/60.0
         slon(1) = ilon + slon(1)/60.0
         if( ew .eq. 'W' ) slon(1) = -slon(1)
         IF( ns .eq. 'S' ) slat(1) = -slat(1)
C
C   Account for traversing the International dateline...
C
            if( maxlong$ .gt. 180.0   .and.
     &          minlong$ .lt. 180.0   .and.
     &          minlong$ .gt.   0.0   .and.
     &          slon(1)  .lt.   0.0 ) then
            slon(1) = slon(1) + 360.0
            else if( minlong$ .lt. -180.0   .and.
     &               slon(1)  .gt.    0.0 ) then
            slon(1) = slon(1) - 360.0
            end if
c
c    Plot if in geographical range...
c
            if( slon(1) .le. maxlong$   .and.   !
     &          slon(1) .ge. minlong$   .and.                 
     &          slat(1) .le. maxlatn$   .and.
     &          slat(1) .ge. minlatn$ ) then                     
            call map_proj(slon,slat,x,y,1)                                   
            ix = x(1) + refx0$
            iy = y(1) + refy0$ 
            call xsymbol(sym_station$+sym_ext,
     &                   sym_stat_size$,
     &                   ix,iy)
               if( answer_stat .ne. 'X') then       ! Plot name.
               call xchars(sta,5,ix-22,iy-25)       !
               end if                               !
            endif                                                               
         endif                                                                  
      enddo                                                                     
      end if                                         ! End of stations.
c                                                                               
c  =================================
c  Plot longitude and latitude lines                                            
c  =================================
c                                                                               
      CALL MAP_GRID                                                             
c
C                                                                               
C   ===================================================
C   OPEN FILES WITH MAP CONTOURS and plot if desired...
C   ===================================================
C             
      if(ncontour.gt.0) then
         do icontour=1,ncontour                                                 
         call sei open( old$,               ! Open (default stop on error).
     &                  ' ',                ! No prompt.
     &                  mapfile(icontour),  ! Map file.
     &                  read3,              ! On unit.
     &                  b_flag,             ! Flag existance.
     &                  code )              ! Condition (n/a).
C
         rewind( read3, iostat=code )          ! Rewind file.
         call sei code(fort$,code,read3,b_flag)! Process outcome.
C                                                                               
C   INPUT CONTOURS, ONE AREA AT A TIME                                          
C   ----------------------------------
c   set color of map contours
c
         call xset_color(color_map_contour)
c                                                               
            ITER=0                                                              
5           READ(read3,'(I4)',iostat=code) NCON       ! 
            call sei code(fort$,code,read3,b_flag)    ! Process outcome.
               if( b_flag ) then                      ! End of file
               call sei close(close$,read3,code)      ! Close (stop on error)
C
               else if( ncon .ne. 0 ) then            ! Data to read.
               read(read3,'(10f8.3)',iostat=code)     ! & read.
     &              (X(I),Y(I),I=1,NCON)              !
               call sei code(fort$,code,read3,b_flag) ! Process outcome.
               CALL CONTOUR(Y,X,NCON)                 ! & contour.
               goto 5                                 ! Another contour!.
C
               else                                   ! Otherwise.
               goto 5                                 ! Aother contour.
               end if                                 !
         enddo
      endif   
      call xset_color(color_def)                      ! default color
c
c if is plotting a choosen area then get back from here
c
      if( b_area ) then                        !
      call area_plot(nevent2,pt,xzoom,yzoom)
c
c   get up cursor and check if replot or quit
c
      ix = 650.0
      iy = screen_sizey$ - 2.5*allowance$
c
      call xset_color(color_prompt)        ! 
      text = 'Enter Q to quit or'
      call tchars(text,seiclen(text),ix,iy)
      text = 'press <return> to continue or'
      iy = iy - 4.0*allowance$
      call tchars(text,seiclen(text),ix,iy)
      text = 'R to <return> and restart plot'
      iy = iy - 4.0*allowance$
      call tchars(text,seiclen(text),ix,iy)
      call xscursr(ichar,ix,iy)
c     call ancho(7)
      chr_a = char(ichar)
      call sei upc(chr_a)
c
         if( chr_a .eq. 'Q' ) then           ! Finished.
         goto 999                            ! & exit.
         else                                ! Otherwise.
         chr_a = ' '                         ! Set to forwards.
         goto 1111                           ! & test again.
         end if                              !
      end if                                 !
c
5555  call xset_color(color_prompt)
c
      ix = 300.0
      text = 'Select:'
      call tchars(text,seiclen(text),border_x$,ix)
c
      ix = ix - 8.0*allowance$
      text = 'Q to  Quit'
      call tchars(text,seiclen(text),border_x$,ix)
c
      ix = ix - 4.0*allowance$
      text = 'P for Profile'
      call tchars(text,seiclen(text),border_x$,ix)
c
      ix = ix - 4.0*allowance$
      text = 'O for Old profile'
      call tchars(text,seiclen(text),border_x$,ix)
c
      ix = ix - 4.0*allowance$
      text = 'A for Area'
      call tchars(text,seiclen(text),border_x$,ix)
c
      ix = ix - 4.0*allowance$
      text = 'Z for Zoom'
      call tchars(text,seiclen(text),border_x$,ix)
c
        if(b_zoom)then
        ix = 650.0
        iy = screen_sizey$ - 2.5*allowance$
        text = 'Press <return> for original map or'
        call tchars(text,seiclen(text),ix,iy)
        text = 'R to <return> and restart plot'
        iy = iy - 4.0*allowance$
        call tchars(text,seiclen(text),ix,iy)
        endif
c
      call xset_color(color_def)
c
c   plot section, get cursor up to select window or quit
c
14    call xscursr(ichar,ix,iy)
c     call ancho(7)
c
c   ======================
c   Process the options...
c   ======================
c
      chr_a = char(ichar)                    ! Character.
      call sei upc(chr_a)                    ! In uppercase.
c
c   Exit the routine...
c   ===================
c   
1111  if( chr_a .eq. 'Q' ) then              ! Quit.
      goto 999                               !
c
c   Profile...
c   ==========
c
      else if( chr_a .eq. 'P'   .or.         ! Profile.
     &         chr_a .eq. 'O' ) then         ! Last profile.
      max_depth=0.0                          ! No pre fixed depth scaling
c
c   Last profile...
c   ---------------
c
         if( chr_a .eq. 'O' ) then           ! Last profile.#
         call sei open( old$+ignore$,        ! Open & ignore.
     &                  ' ',                 ! No prompt.
     &                  'profile.out',       ! Input file.
     &                  read4,               ! On unit.
     &                  b_flag,              ! Flag existance?.
     &                  code )               ! Returned condition.
         if( code .ne. e_ok$ ) goto 5555     ! A problem with file.
C
C    Get profile geographicals...
C
         read(read4,'(6f10.5)',iostat=code) lat_profile(1),
     &                                      lon_profile(1),
     &                                      lat_profile(2),
     &                                      lon_profile(2),
     &                                      lat_profile(3),
     &                                      lon_profile(3)
         call sei code(fort$,code,read4,b_flag)   ! Process outcome.
C
            if( b_flag ) then                     ! End of file.
            goto 5555                             ! Problem with file.
            else                                  ! Otherwise encode co-ords.
c
               if( b_f_debug$ ) then              !
               write(dbgunit$,'(A)',iostat=code)
     &         'Retrieved lat-lon arrays from profile.out...'
                call sei code(fort$,code,dbgunit$,b_flag) ! Process outcome.
               write(dbgunit$,'(3f10.5)',iostat=code)
     &         lat_profile, lon_profile
                call sei code(fort$,code,dbgunit$,b_flag) ! Process outcome.
               end if                             !
c
            call map_proj(lon_profile,lat_profile,!
     &                    x_profile,  y_profile,  !
     &                    3 )                     ! For these 3 points.
            do 1114 jx = 1, 3                     ! Add displacements.
            x_profile(jx) = x_profile(jx) + REFX0$!
            y_profile(jx) = y_profile(jx) + REFY0$!
1114        continue                              !
c
               if( b_f_debug$ ) then              !
               write(dbgunit$,'(A)',iostat=code)
     &         '...& converted screen (x,y) arrays....'
                call sei code(fort$,code,dbgunit$,b_flag) ! Process outcome.
               write(dbgunit$,'(3f10.5)',iostat=code)
     &         x_profile, y_profile
                call sei code(fort$,code,dbgunit$,b_flag) ! Process outcome.
               end if                             !
            end if                                !
C
C    & other details...
C
         read(read4,'(f10.1)',iostat=code) max_depth
         call sei code(fort$,code,read4,b_flag) ! Process outcome.
         if( b_flag ) goto 5555              ! End of file.
C
         read(read4,'(i10)',iostat=code) numb_profiles
         call sei code(fort$,code,read4,b_flag) ! Process outcome.
         old_profile=.true.                  ! & flag.
C
         call sei close(close$,read4,code)   ! Close (default is stop on error)
c
c   New profile...
c   --------------
c
         else                                ! Otherwise set-up.
         call xset_color(color_prompt)
         old_profile = .false.               ! New, not old profile.
         text = 'Select 3 points for'
         call tchars(text,seiclen(text),border_x$,95.0)
         text = 'section PROFILE:'
         call tchars(text,seiclen(text),border_x$,70.0)
c
cc   gfortan pc         if( pc ) then
cc            continue
cc            else
            write(*,*)
            write(*,*)
     &'==== For more than one profile, enter the number of'
            write(*,*)
     &'     profiles you require at the third point.'
            write(*,*)
cc            end if
c
         call xscursr(ichar,ix,iy)
         x1=ix
         y1=iy
c        call ancho(7)
         end if                              !
c
c   ----------------------
c   Combined processing...
c   ----------------------
c   plot start of profile...
c   ------------------------
c
      call xset_color(color_section)
      call xmovabs(x1-10,y1-10)
      call xdrwabs(x1-10,y1+10)
      call xdrwabs(x1+10,y1+10)
      call xdrwabs(x1+10,y1-10)
      call xdrwabs(x1-10,y1-10)
      call xdrwabs(x1+10,y1+10)
      call xmovabs(x1-10,y1+10)
      call xdrwabs(x1+10,y1-10)
c
c   Get end of centre line if a new profile...
c   ------------------------------------------
c
         if( .not.old_profile ) then   ! New profile.
         call xscursr(ichar,ix,iy)
c         call beep
         x2=ix
         y2=iy
         end if                        !
c
c   plot center line
c
      call xmovabs(x1,y1)
      call xdrwabs(x2,y2)
c
c   calculate profile azimuth, dirnab, positive east of north
c   it is assumed that the map plots linearly in x and y, which is
c   not the case using e.g. mercator at higer latitudes, however, this has
c   little effect on error elipses, which is the only thing dirnab is used for
c
      dirnab=atan2(y2-y1,x2-x1)
c
         if( write4 .gt. 0 ) then             ! Just rewind.
         rewind( write4, iostat=code )        !
         call sei code(fort$,code,write4,b_flag) ! Process outcome.
c
         else                                 ! Otherwise.
         call sei open( unknown$,             ! Open (default is stop on error).
     &                  ' ',                  ! No prompt.
     &                  'azimuth.out',        ! File name.
     &                  write4,               ! On unit.
     &                  b_flag,               ! Flag existance (n/a).
     &                  code )                ! Condition (n/a).
         end if                               !
c
      write(write4,*,iostat=code)'azimuth = ', dirnab*57.3
      call sei code(fort$,code,write4,b_flag) ! Process outcome.
c
c   get width of profile and number of profiles if not old values used
c
      if(.not.old_profile) then
         call xscursr(ichar,ix,iy)
c         call beep
         cc=char(ichar)
         numb_profiles=1   ! default only one profile
         if(cc.eq.'2'.or.cc.eq.'3'.or.cc.eq.'4'.or.cc.eq.'5'.or.
     *      cc.eq.'6'.or.cc.eq.'7'.or.cc.eq.'8'.or.cc.eq.'9') then
            read(cc,'(i1)') numb_profiles
         endif
         x3=ix
         y3=iy
      endif
c
c   calculates line constants, check that slope is not infinite
c
      if(x2.eq.x1) x1=x1+0.0001
      a1=(y2-y1)/(x2-x1)
      if(a1.eq.0.0) a1=0.0001
      a=-1.0/a1
      a=atan(a)
      b1=y1-a1*x1
c
c  calculate half width
c
      width=abs((y3-a1*x3-b1)/sqrt(a1**2+1.0))
c
c  start position of profile(s)
c
      xx1=x1
      xx2=x2
      yy1=y1
      yy2=y2
c
c   plot all sections
c
      do k=1,numb_profiles
         xr1=xx1+width*cos(a)
         xr2=xx1-width*cos(a)
         xr3=xx2-width*cos(a)
         xr4=xx2+width*cos(a)
         xx1=xx1+2*width*cos(a)
         xx2=xx2+2*width*cos(a)
         yr1=yy1+width*sin(a)
         yr2=yy1-width*sin(a)
         yr3=yy2-width*sin(a)
         yr4=yy2+width*sin(a)
         yy1=yy1+2*width*sin(a)
         yy2=yy2+2*width*sin(a)
c
c  draw sections outlines
c
         call xdrwl(xr1,yr1,xr2,yr2)
         call xdrwl(xr2,yr2,xr3,yr3)
         call xdrwl(xr3,yr3,xr4,yr4)
         call xdrwl(xr4,yr4,xr1,yr1)
      enddo
c
c   wait to see selection and select if autoscale or same length-depth scale
c
      call xscursr(ichar,ix,iy)
c      call beep
      chr_a = char(ichar)         ! Get returned character.
      call sei upc( chr_a )       ! In upercase.
      auto_depth = chr_a .ne. 'F' ! Autoscaling?.
c
      if( chr_a .eq. 'Q' ) goto 999 ! Finished.
c
c  initialize section selections
c
      xx1=x1
      xx2=x2
      yy1=y1
      yy2=y2
c
c    Profiles...
c    -----------
c    Set-up PostScript positioning for each plot...
c
      do k=1,numb_profiles                           ! Loop profiles.
      call new_diagram( depth$,                      ! Depth plot.
     &                  b_start,                     ! Starting new plot?.
     &                  b_page,                      ! On new page?.
     &                  write2 )                     ! PostScript output.
c
c   calculate section
c   -----------------
c
      call profile
     *(epicentre_n$,epiorder,nout,xx,yy,max_depth,
     &xdepth,number,
     *xx1,yy1,xx2,yy2,width,dist)
c
c  file with profile numbers
c
         call sei open( unknown$,          ! Open (default is stop on error).
     &                  ' ',               ! No prompt.
     &                  'profile.num',     ! File name.
     &                  write5,            ! On unit.
     &                  b_flag,            ! Flagged existsance.
     &                  code )             ! Returned condition (n/a).
C
c
c   convert from tek units to km
c

      do i=1,nout    
         dist(i)=eq_deg_to_km$*dist(i)/SCALE$
         if(i.ne.nout) write(write5,*) dist(i),xdepth(number(i)) ! one extra for scaling
      enddo
c
      call sei close(close$,write5,code) ! Close (default is stop on error).

C
C    Profile to plot....
c    -------------------
c
         if(nout.gt.1) then   ! remember last value is scaling, n gt 1
            call depth_plot(write2, nout,dist,xdepth,
     &        max_depth,xmag,
     *        erx_a,ery_a,erz_a,cvxy_a,cvxz_a,cvyz_a,dirnab,
     *        number_plot,number,auto_depth,title,k,elip,
     &        chr_epi_col,strike,dip,rake,fault)
c
            if( b_f_debug$ ) then
            write(dbgunit$,*)'depth plot values...'
            write(dbgunit$,*)'nout...', nout
            do 8881 jy = 1, nout
            jx = number(jy)
            write(dbgunit$,*)'xmag...', xmag(jx)
            write(dbgunit$,*)'xdepth.', xdepth(jx)
            write(dbgunit$,*)'dist...', dist(jx)
            write(dbgunit$,*)'number.', number(jx)
            write(dbgunit$,*)'colour.', chr_epi_col(jx)
            write(dbgunit$,*)'order..', jx
8881        continue
            write(dbgunit$,*)'.....end of values..'
            end if
c
c    No profile to plot...
c    ---------------------
c
         else
cc            if( pc ) then
cc            continue
cc            else
            write(6,*)' No points to plot'
            end if
cc         endif
         yy1=yy1+2*width*sin(a)
         yy2=yy2+2*width*sin(a)
         xx1=xx1+2*width*cos(a)
         xx2=xx2+2*width*cos(a)
c
c   save section parameters....
c   ---------------------------
c   First, get geographical co-ordinates defining profile area...
c
         if( k .eq. 1 ) then                    ! 1st profile. 
            if( b_f_debug$) then
            write(dbgunit$,'(3f10.5)',iostat=code) x_profile,
     &                                             y_profile,
     &                                             lon_profile,
     &                                             lat_profile
            call sei code(fort$,code,dbgunit$,b_flag )
            end if
c
         do 3333 jx = 1, 3                 ! Loop the points.
         call xy_lonlat( x_profile(jx),    ! Decode co-ordinate.
     &                   y_profile(jx),    !
     &                   lon_profile(jx),  ! Into longitude.
     &                   lat_profile(jx),  ! & latitude.
     &                   proj$ )           ! On this projection.
3333     continue                          !

c
c    Now open the profile detail file..
c
         call sei open( unknown$,          ! Open (default is stop on error).
     &                  ' ',               ! No prompt.
     &                  'profile.out',     ! File name.
     &                  write5,            ! On unit.
     &                  b_flag,            ! Flagged existsance.
     &                  code )             ! Returned condition (n/a).
C
         write(write5,'(6f10.5)',iostat=code) ! Enter lat-longs (in degrees).
     &                                      lat_profile(1),
     &                                      lon_profile(1),
     &                                      lat_profile(2),
     &                                      lon_profile(2),
     &                                      lat_profile(3),
     &                                      lon_profile(3)
         call sei code(fort$,code,write5,b_flag )
C
               if( b_f_debug$ ) then              !
               write(dbgunit$,'(A)',iostat=code)
     &         'Saved lat-lon arrays in profile.out...'
                call sei code(fort$,code,dbgunit$,b_flag) ! Process outcome.
               write(dbgunit$,'(3f10.5)',iostat=code)
     &         lat_profile, lon_profile
                call sei code(fort$,code,dbgunit$,b_flag) ! Process outcome.
               end if                             !
C
         write(write5,'(f10.1)',iostat=code) max_depth
         call sei code(fort$,code,write5,b_flag )
C
         write(write5,'(i10)',  iostat=code) numb_profiles
         call sei code(fort$,code,write5,b_flag )
c
c   decode corners of profile
c
         call xy_lonlat( xr1,yr1,   ! Decode co-ordinate.
     &   lon_profile(1),lat_profile(1),                ! Into longitude and latitude
     &   proj$ )                                       ! On this projection.
         write(write5,*) lat_profile(1),lon_profile(1)

         call xy_lonlat( xr2,yr2,   ! Decode co-ordinate.
     &   lon_profile(1),lat_profile(1),                ! Into longitude and latitude
     &   proj$ )                                       ! On this projection.
         write(write5,*) lat_profile(1),lon_profile(1)
   
         call xy_lonlat( xr3,yr3,   ! Decode co-ordinate.
     &   lon_profile(1),lat_profile(1),                ! Into longitude and latitude
     &   proj$ )                                       ! On this projection.
         write(write5,*) lat_profile(1),lon_profile(1) 

         call xy_lonlat( xr4,yr4,   ! Decode co-ordinate.
     &   lon_profile(1),lat_profile(1),                ! Into longitude and latitude
     &   proj$ )                                       ! On this projection.
         write(write5,*) lat_profile(1),lon_profile(1)

C
         call sei close(close$,write5,code) ! Close (default is stop on error).
         end if                                  !
c
c   get up cursor and check if replot or quit
c   -----------------------------------------
c
         call xset_color(color_prompt)
         ix = 600.0
         iy = 200.0
         text = 'Enter Q to quit or'
         call tchars(text,seiclen(text),ix,iy)
         text = 'press <return> to continue'
         iy = iy - 4.0*allowance$
         call tchars(text,seiclen(text),ix,iy)
c
         call xscursr(ichar,ix,iy)
         call xset_color(color_def)
c         call beep
c
         chr_a = char(ichar)
         call sei upc( chr_a )
            if( chr_a .eq. 'Q' ) then        ! Exit.
            goto 999                         ! 
            end if                           !
      enddo                 ! end of profile loop
c
c   back for another epimap plot and depth selection
c
      b_page = .true.                        ! Starts on a new page.
      goto 2000
c
c   Choose an area...
c   =================
c
      else if( chr_a .eq. 'A' ) then         ! Area.
      call xset_color(color_prompt)
      text = 'Select points to enclose'
      call tchars(text,seiclen(text),border_x$,95.0)
      text = 'the area of your choice'
      call tchars(text,seiclen(text),border_x$,75.0)
      text = 'and then enter "F."'
      call tchars(text,seiclen(text),border_x$,55.0)
c
c CHOOSE AREA STARTS HERE !!!
c
      call xset_color(color_prompt)
      do 19 pt=1,max_polyn$
c
c clean buffer.
      call xscursr(ichar,ix,iy)
c     call ancho(7)
c
      chr_a = char(ichar)
      call sei upc(chr_a)
c
         if( chr_a .eq. 'F' ) then
            if( pt .gt. 1 ) then
            call xmovabs(xsrc(pt-1),ysrc(pt-1))
            call xdrwabs(xsrc(1),   ysrc(1))
            endif
         goto 25               ! Skip the loop.
         end if                !
c
      xsrc(pt)=ix
      ysrc(pt)=iy
c
c plot choosen point.
c
         if(pt.gt.1)then
            if(abs(ix-(xsrc(1))).lt.15.00  .and.
     *         abs(iy-(ysrc(1))).lt.15.00) then
            ix=xsrc(1)
            iy=ysrc(1)
            xsrc(pt)=xsrc(1)
            ysrc(pt)=ysrc(1)
            endif
         endif
c
      call xmovabs(ix-5,iy-5)
      call xdrwabs(ix,iy+5)
      call xdrwabs(ix+5,iy-5)
      call xdrwabs(ix-5,iy+3)
      call xdrwabs(ix+5,iy+3)
      call xdrwabs(ix-5,iy-5)
      call xdrwabs(ix,iy)
c
c from the second point and forward plot line.
c
         if( pt .gt. 1 ) then
         call xmovabs(xsrc(pt-1),ysrc(pt-1))
         call xdrwabs(ix,iy)
         endif
c
c  Get latitude-longitude & write to output...
c
      call xy_lonlat (ix,iy,srclon,srclat,proj$)
      write(write6,'(f6.2,1x,f7.2)',iostat=code) srclat,srclon
      call sei code(fort$,code,write6,b_flag)      ! Process outcome.
c
      xzoom(pt)=srclon
      yzoom(pt)=srclat
19    continue
c
c search for events inside choosen area
c send number of events in window (cartecian coord.)
c corners of surrounding area, the infile(geographic)
c and outfile.
c
25    pt=pt-1
      if(xsrc(pt).ne.xsrc(1).or.ysrc(pt).ne.ysrc(1))then
      pt=pt+1
      xsrc(pt)=xsrc(1)
      ysrc(pt)=ysrc(1)
c
      call xy_lonlat (xsrc(1),ysrc(1),srclon,srclat,proj$)
      write(write6,'(f6.2,1x,f7.2)',iostat=code) srclat,srclon
      call sei code(fort$,code,write6,b_flag)                 ! Process outcome.
c
      xzoom(pt)=srclon
      yzoom(pt)=srclat
      endif
c
c  make a stroke
c
      call xout(10.0,10.0)
c
c ------------------------------
c get earthquakes inside area...
c ------------------------------
c
         if( epicentre_n$ .gt. 0 ) then               ! Epicentres exist.
         call search(epicentre_n$,xx,yy,pt,xsrc,ysrc, !
     &             write8,write7,evin, chr_epi_plt,data,filetype)  
         nevent2 = evin                               ! Events in the area.
         end if                                       !
c
c zoom to get area (in upper part of the page of the postscript)...
c -----------------------------------------------------------------
c
      call zoom(pt,xzoom,yzoom,1)
      b_area = .true.                                 ! Ensure an area plot.
      goto 2000
c
c    Zoom...
c    =======
c
      else if( chr_a .eq. 'Z' ) then         ! Zoom.
      call xset_color(color_prompt)
      text = 'Select two diagonally'
      call tchars(text,seiclen(text),border_x$,130.0)
      text = 'opposite corners of the'
      call tchars(text,seiclen(text),border_x$,110.0)
      text = 'zoom box'
      call tchars(text,seiclen(text),border_x$,90.0)
c
      call xset_color(color_prompt)
      do i=1,2
      call xscursr(ichar,ix,iy)
c     call ancho(7)
      call xmovabs(ix-5,iy-5)
      call xdrwabs(ix,iy+5)
      call xdrwabs(ix+5,iy-5)
      call xdrwabs(ix-5,iy+3)
      call xdrwabs(ix+5,iy+3)
      call xdrwabs(ix-5,iy-5)
      call xdrwabs(ix,iy)
      call xy_lonlat(ix,iy,xlonmz(i),ylatmz(i),proj$)
      enddo
c
      call zoom(2,xlonmz,ylatmz,0)
      b_zoom = .true.                        ! Flag zoom!.
      nevent2 = 0                            ! Force re-search.
      goto 2000                              ! & again.
c
c    Forwards to original map...
c    ===========================
c
      else                           ! Otherwise.
      call xset_color(color_def)
      MINLATN$         = ylatm(1)
      MAXLATN$         = ylatm(2)
      MINLONG$         = xlonm(1)
      MAXLONG$         = xlonm(2)
      GRID_SPACE_LATN$ = latdel
      GRID_SPACE_LONG$ = londel
      GRID_BASE_LATN$  = latbase
      GRID_BASE_LONG$  = lonbase
      b_zoom           = .false.
      b_area           = .false.
      b_start          = chr_a .eq. 'R'
c
c    Re-set for all epicentre plotting...
c
      do 8888 jx = 1, epicentre_n$           ! Loop epicentres.
      chr_epi_plt(jx) = 'P'                  ! Must plot again.
8888  continue                               !
c
      nevent2 = epicentre_n$                 ! Set to full count.
      goto 2000                              ! Re-plot.
      end if                                 !
c
c   quitting program
c   ================
c
 999  continue
c                                                                               
C   CLOSE PLOT                                                                  
C                
      call clear_to_alpha                                                      
c
c   clese postscript
c
      call close_post
c
      write(*,*)
      write(*,*)
     &' Numbered hypocenters inside the main window are in epimap.num'
      write(*,*)
     &' All data inside the main window are in epimap.out'
      write(*,*)
     &' Coordinates of the surrounding area are in epimap.cor'
      write(*,*)
     &' Area-selected data are in epimap.are'
      write(*,*)
     &' Plot file is called epimap.eps'
      write(*,*)' Coordinates of profile outline in profile.out'
      write(*,*)' Profile x and y in profile.num'
C
C    Close any remaining files...
C    ----------------------------
C
9999  call sei close(close$+all$,0,code)      ! Close all open files.
        STOP 
        END
C
c---------------------------------------------
      subroutine area_plot(evin,n,xzoom,yzoom)
c   makes a earthquake area zoom
c   m. villagran, 12/93.
c     input:      n: number of point of surrounding area
c                 x,y: longitude and latitude in tek units
c                 plot title and statistics (in epimap.cor...JAB(BGS))
C!JAB(BGS)Dec94 : Install EMAP functions into EPIMAP
C
      include 'epimap.inc'                 ! EPIMAP definitions.
      include 'seidim.inc'                 ! Seisan array definitions.
      include 'libsei.inc'                 ! Library definitions & data defns.
      external map_proj,                   ! Projection routine.
     &         sei left,                   ! Left justify string.
     &         sei clen                    ! String length.
      integer  sei clen                    ! & fnction.
c
c--help variables
      character*80 text
      integer i,evin,n
      real ax, ay,
     &     bx(max_polyn$), by(max_polyn$),
     &     x(1),y(1),xzoom(n),yzoom(n)
C
C    Initialise...
C    =============
C
      call xset_color( color_prompt )      !
c
c    Convert lat-longs to screen units...
c    ====================================
c
      do i=1,n
      CALL MAP_PROJ(xzoom(i),yzoom(i),x,y,1)
      bx(i) = x(1) + REFX0$
      by(i) = y(1) + REFY0$
        if(i.lt.n)then
         call xmovabs(bx(i)-5,by(i)-5)
         call xdrwabs(bx(i),by(i)+5)
         call xdrwabs(bx(i)+5,by(i)-5)
         call xdrwabs(bx(i)-5,by(i)+3)
         call xdrwabs(bx(i)+5,by(i)+3)
         call xdrwabs(bx(i)-5,by(i)-5)
         call xdrwabs(bx(i),by(i))
         endif
         if(i.gt.1)then
         call xmovabs(bx(i-1),by(i-1))
         call xdrwabs(bx(i),by(i))
         endif
      enddo
c
      ax = border_x$
      ay = event_title_y$ - 4.0*allowance$
      call xset_color( color_symbol_key )
c
c    If there are any events for this session, not just if no events
c    in the area, then enter events selected message...
c
      if( epicentre_n$ .gt. 0 ) then               ! Events exist.
      text = 'Selected events: '
      write(text(18:23),'(i6)') evin
      call sei left( text(18:23) )
      call xchars( text, seiclen(text), ax, ay)
      end if                                       !
c
c    Write all corners to file...
c
      call xset_color( color_prompt )
      text = 'Area plot...'
      CALL XCHARS(text,seiclen(text),ax,140.0)
      text = 'See epimap.cor'
      CALL XCHARS(text,seiclen(text),ax,120.0)
      text = 'for locations'
      CALL XCHARS(text,seiclen(text),ax,100.0)
      text = 'of corners'
      CALL XCHARS(text,seiclen(text),ax,80.0)
c
c    Return to Caller...
c    ===================
c
99    call xset_color(color_def)                      ! default color
      return
      end
c
      subroutine bdy_val(xa,ya,xb,yb,xc,yc,xd,yd,lam_seg,lam_bdy)
CSTART**************************************************************************
C                                                                              *
C   Supplier          : BGS/GSRG Applications Programming Unit                 *
C   System            : SEISAN                                                 *
C   Name              : BDY_VAL                                                *
C   Purpose           : To determine if line c -> d crosses line a -> b.       *
C   Note              : The equation of a line is given by:                    *
C                                                                              *
C                                 r = a + bdy_val * (b - a)                    *
C                                                                              *
C                       where (r,a,b) are position vectors.                    *
C                                                                              *
C   Arguments-Input   : XA,   YA   (R) co-ordinates of start of line1.         *
C                     : XB,   YB   (R) co-ordinates of end of line1.           *
C                     : XC,   YC   (R) co-ordinates of start of line2.         *
C                     : XD,   YD   (R) co-ordinates of end of line2.           *
C           -Output   : LAM_SEG    (R) Fractional crossing along segment.      *
C                     : LAM_BDY    (R) Ditto boundary segment.                 *
C                                                                              *
C   Author            : J A Bolton  (based on PGG CROSS, written by the author *
C                                    on 15 July 1991 in BGS/Petroleum Geology) *
C   Date              : 15 July 1991                                           *
C   Version           : V01                                                    *
C                                                                              *
CEND****************************************************************************
c
c    Arguments...
c    ============
c
      real      XA       ,YA              ! Co-ords of start of line a -> b.
     &         ,XB       ,YB              ! Co-ords of end of line a -> b.
     &         ,XC       ,YC              ! Co-ords of start of line c -> d.
     &         ,XD       ,YD              ! Co-ords of end of line c -> d.
     &         ,LAM_SEG  ,LAM_BDY         ! Fractional crossing lengths.
c
c    Local Variables...
c    ==================
c
      real*8      temp1    ,temp2         ! Local precision variables.
     &           ,temp3    ,temp4         ! Ditto.
     &           ,lam_ab   ,lam_cd        ! Fractional crossings.
      logical     b_ok                    ! Lines will cross?
c
c    Initialise...
c    =============
c
      temp1 = XB - XA                       ! Floating vector.
      temp2 = YB - YA                       ! Of line a -> b.
      temp3 = XD - XC                       ! Floating vector.
      temp4 = YD - YC                       ! Of line c -> d.
c
c    Determine if the lines cross  either internally or externally....
c    -----------------------------------------------------------------
c
      b_ok = temp1*temp4-temp2*temp3 .ne. 0.0d0 ! Rotation of line 1 to line 2.
c
c    The lines cross somewhere....
c    =============================
c
      if( b_ok ) then                                     ! Lines will cross.
c
c    Look at Special cases (singularity avoidence)....
c    -------------------------------------------------
c
         if( temp1 .eq. 0.0d0 ) then                      !
         lam_cd = (XA-XC)/temp3                           !
         lam_ab = lam_cd*(temp4/temp2) + (YC-YA)/temp2    !
c
         else if( temp2 .eq. 0.0d0 ) then                 !
         lam_cd = (YA-YC)/temp4                           !
         lam_ab = lam_cd*(temp3/temp1) + (XC-XA)/temp1    !
c
         else if( temp3 .eq. 0.0d0 ) then                 !
         lam_ab = (XC-XA)/temp1                           !
         lam_cd = lam_ab*(temp2/temp4) + (YA-YC)/temp4    !
c
         else if( temp4 .eq. 0.0d0 ) then                 !
         lam_ab = (YC-YA)/temp2                           !
         lam_cd = lam_ab*(temp1/temp3) + (XA-XC)/temp3    !
c
c    Crossing is clean.....
c    ----------------------
C
         else                                             ! Otherwise.
1000     lam_cd = ((YA-YC) + (XC-XA)*(temp2/temp1))       !
     &          / (temp4   - temp3  *(temp2/temp1))       !
         lam_ab = lam_cd*(temp3/temp1) + (XC-XA)/temp1    !
         end if                                           !
c
c    Otherwise the lines are either parallel or both points.....
c    ===========================================================
c
      else                                                ! Lines are parallel.
2000  lam_ab = 100.0d0                                    ! & out of limits.
      lam_cd = 100.0d0                                    ! Ditto.
      end if                                              !
c
c    Set up return values...
c    =======================
c
      lam_seg = lam_ab               ! Segment.
      lam_bdy = lam_cd               ! Boundary.
c
c    Return to caller....
c    ====================
c
9999  return
      end


      subroutine boundary(x1,y1,x2,y2,X0,Y0,XMIN,XMAX,YMIN,YMAX)

c plots to the point (x2,y2) from the previous point (x1,y1)
c provided both are within the limits (XMIN,YMIN)-(XMAX,YMAX)

c if either point is outside the boundary, interpolates and
c plots to or from the boundary

c all coordinates except (X0,Y0) are geographic (degrees), X0,Y0 is plot origin

      implicit none 
      real x1,y1,x2,y2,XMIN,XMAX,YMIN,YMAX,b,c,X0,Y0,sx,sy

c (x1,y1) within limits
      if(x1.ge.XMIN.and.x1.le.XMAX.and.y1.ge.YMIN.and.y1.le.YMAX)then

       if(x2.ge.XMIN.and.x2.le.XMAX.and.y2.ge.YMIN.and.y2.le.YMAX)then

c (x2,y2) also inside: plot point and exit
        call map_proj(x1,y1,sx,sy,1)
c        call plot(X0+sx,Y0+sy,3)
c        call map_proj(x2,y2,sx,sy,1)
c        call plot(X0+sx,Y0+sy,2)
        call xmovabs(X0+sx,Y0+sy)
        call map_proj(x2,y2,sx,sy,1)
        call xdrwabs(X0+sx,Y0+sy)
        return

       else

c (x2,y2) outside: interpolate to boundary
        b=(y1-y2)/(x1-x2)
        c=y1-b*x1
        if(x2.gt.XMAX)then
         x2=XMAX
         y2=b*x2+c
        elseif(x2.lt.XMIN)then
         x2=XMIN
         y2=b*x2+c
        elseif(y2.gt.YMAX)then
         y2=YMAX
         x2=(y2-c)/b
        elseif(y2.lt.YMIN)then
         y2=YMIN
         x2=(y2-c)/b
        endif
        call map_proj(x1,y1,sx,sy,1)
c        call plot(X0+sx,Y0+sy,2)
        call xmovabs(X0+sx,Y0+sy)
        call map_proj(x2,y2,sx,sy,1)
c        call plot(X0+sx,Y0+sy,2)
        call xdrwabs(X0+sx,Y0+sy)
       endif

      elseif(x2.ge.XMIN.and.x2.le.XMAX.and.y2.ge.YMIN.and.y2.le.
     &YMAX)then

c (x1,y1) outside (x2,y2) inside: interpolate to boundary
       b=(y1-y2)/(x1-x2)
       c=y1-b*x1
       if(x1.gt.XMAX)then
        x1=XMAX
        y1=b*x1+c
       elseif(x1.lt.XMIN)then
        x1=XMIN
        y1=b*x1+c
       endif
       if(y1.gt.YMAX)then
        y1=YMAX
        x1=(y1-c)/b
       elseif(y1.lt.YMIN)then
        y1=YMIN
        x1=(y1-c)/b
       endif
        call map_proj(x1,y1,sx,sy,1)
c        call plot(X0+sx,Y0+sy,3)
        call xmovabs(X0+sx,Y0+sy)
        call map_proj(x2,y2,sx,sy,1)
c        call plot(X0+sx,Y0+sy,2)
        call xdrwabs(X0+sx,Y0+sy)
      endif
      return
      end
C
       SUBROUTINE CONTOR(ncon)                              
c       SUBROUTINE CONTOR (CLEV,PLEV,NCLEV,IIYMX,IIXMX,ARRAY)                             
C  *****************************                                                
C  ** CONTOUR PLOT SUBROUTINE **                                                
C  *****************************                                                
C                                                                               
C     THIS SUBROUTINE PLOTS 'NCLEV' CONTOURS EACH TIME IT IS CALLED             
c
c     Some modifications: reads contour values directly from a file
c     on unit ncon. the routineis now used with latitude and longitude
c     values with the epimap program
C                                                                               
C     ARRAY(IR,IC) = DATA INPUT ARRAY                                           
C     IR  CORRESPONDS TO ROWS FROM IYMN TO IYMX                                 
C     IC  CORRESPONDS TO COLUMNS FROM IXMN TO IXMX                              
C     CLEV(I) = THE CONTOUR LEVELS TO BE PLOTTED                                
C     PLEV(I) = THE NUMBERS WHICH ARE PRINTED ON THE PLOT. PLEV(I)              
C     CORRESPONDS WITH CLEV(I)                                                  
C     NCLEV = THE NUMBER OF LEVELS TO PLOT                                      
C     IXMX = number of x elements                                     
C     IYMY = number of y elements                                             
C     THE ROWS EXTEND IN THE HORIZONTAL DIRECTION, THE COLUMNS IN THE           
C     VERTICAL DIRECTION.                                                       
C
      real minlat,maxlat,minlon,maxlon
      COMMON/AA/ IXMN,IXMX,IYMN,IYMX,NLX,NRX,NTY,NBY  
      common/bb/ minlon,maxlon,minlat,maxlat                          
      integer idim
      character*80 text    ! help text
      integer ncon         ! unit to read contours from
c      parameter (idim=50)
      dimension  ARRAY(71,71)                                                  
      DIMENSION ENDPT(10000),STPT(10000),                 
     *          XGRID(71),YGRID(71),CLEV(20),PLEV(20)                             
      INTEGER STPT,ENDPT
      character*10 xlev(20)  ! contour levels
      real xcolor(20)        ! color codes as reals
      real xnlat,xnlon       ! real for integers
c
c   read contour file
c 
      do while(text(2:14).ne.'Fields to use')
         read(ncon,'(a)',end=88) text   ! first lines are comments
      enddo
      goto 89
 88   continue
      call clear_to_alpha
      write(6,*)' The contour file did not have the',
     *' Fields to use delimiter'
      stop
 89   continue
      read(ncon,'(40x,3f10.0)',err=3535) minlat,maxlat,xnlat
      read(ncon,'(40x,3f10.0)',err=3535) minlon,maxlon,xnlon
c
c  read contour levels until no more
c
       i=1
  50   continue
          read(ncon,'(a40,a10,f10.0)',err=3535)
     *   text(1:40),xlev(i),xcolor(i)
c	      write(27,'(a)') text(2:14)
          if(text(2:14).ne.'Contour level') then
             backspace ncon
             goto 51
          endif
          read(xlev(i),'(f10.0)') clev(i)
          plev(i)=clev(i)
          i=i+1
       goto 50
 3535  continue
       call clear_to_alpha
       write(6,*)' Error in contour level file'
       stop
  51   continue
       nclev=i-1
c	   write(27,*) nclev
c	   write(27,*) (clev(i),i=1,nclev)
c
c   now read contour grid 
c
      nlat=xnlat
      nlon=xnlon
      do i=1,nlat
         do k=1,nlon
            read(ncon,*) slat,slon,array(i,k)
         enddo
      enddo
c
	  iymn=1
      iymx=nlat
	  ixmn=1
      ixmx=nlon
c
c   set old versatec dimensions to somehting reasonble so it is easy to
c   scale afterwards
c
      xdim=1.0
      ydim=1.0
C                                                                               
C   INITIALIZE ARRAYS                                                           
C                                                                               
      IDIM=71                                                                   
C                                                                               
      KMAX=IDIM*IDIM                                                            
 80   DO 90 I=1,KMAX                                                            
      STPT(I)=0                                                                 
 90   ENDPT(I)=0                                                                
C                                                                               
C   COMPUTE QUADRANT SIZE AND GRID DISPLACEMENTS                                
C                                                                               
      XSCAL = XDIM/(IXMX-IXMN)                                                  
      YSCAL = YDIM/(IYMX-IYMN)                                                  
      DO 110 I=1,IDIM                                                           
      XGRID(I) = (I-1)* XSCAL                                                   
 110  YGRID(I) = (I-1)* YSCAL                                                   
      NLX = XGRID(IXMN) *100. + .5                                              
      NRX = XGRID(IXMX) *100. + .5                                              
      NTY = YGRID(IYMN) *100. + .5                                              
      NBY = YGRID(IYMX) *100. + .5                                              
      IXMN1 = IXMN + 1                                                          
      IYMN1 = IYMN + 1                                                          
C                                                                               
C                                                                               
      DO 700 J=1,NCLEV                                                          
C   LOOP ONCE FOR EACH DB CONTOUR  ***************M4                            
      K=0                                                                       
C                                                                               
C   TEST ALL SQUARES FOR DB LEVEL CROSSING                                      
C                                                                               
      DO 690 N=IYMN1,IYMX                                                       
      DO 690 I=IXMN1,IXMX                                                       
      PT1 = RATIO(ARRAY(N-1,I-1),ARRAY(N-1,I),XGRID(I-1),XGRID(I),              
     1CLEV(J))                                                                  
      PT2 = RATIO(ARRAY(N-1,I-1),ARRAY(N,I-1),YGRID(N-1),YGRID(N),              
     1CLEV(J))                                                                  
      PT3 = RATIO(ARRAY(N,I-1),ARRAY(N,I),XGRID(I-1),XGRID(I),CLEV(J))          
      PT4 = RATIO(ARRAY(N-1,I),ARRAY(N,I),YGRID(N-1),YGRID(N),CLEV(J))          
C                                                                               
C   TEST IF ANY SIDE HAS AN INTERSECTION                                        
C                                                                               
      IF (PT1+PT2+PT3+PT4) 450,690,450                                          
C                                                                               
C     TEST IF ALL SIDES HAVE INTERSECTIONS                                      
C                                                                               
 450  IF (PT1*PT2*PT3*PT4) 660,460,660                                          
C                                                                               
C     THIS IS THE TWO SIDE CASE - DETERMINE WHICH TWO                           
C                                                                               
 460  IF (PT1) 480,470,480                                                      
 470  IF (PT2) 500,560,500                                                      
 490  IF (PT3) 520,530,520                                                      
 480  IF (PT2) 510,490,510                                                      
 500  IF (PT3) 540,550,540                                                      
 510  LDIR=1                                                                    
      GO TO 570                                                                 
 520  LDIR=2                                                                    
      GO TO 570                                                                 
 530  LDIR=3                                                                    
      GO TO 570                                                                 
 540  LDIR=4                                                                    
      GO TO 570                                                                 
 550  LDIR=5                                                                    
      GO TO 570                                                                 
 560  LDIR=6                                                                    
C                                                                               
C                                                                               
C   ASSIGN LINE START COORDINATES BY LINE DIRECTION                             
 570  GO TO (580,580,580,590,590,600),LDIR                                      
 580  STX = PT1                                                                 
      STY = YGRID(N-1)                                                          
      GO TO 610                                                                 
 590  STX = XGRID(I-1)                                                          
      STY = PT2                                                                 
      GO TO 610                                                                 
 600  STX = PT3                                                                 
      STY = YGRID(N)                                                            
C                                                                               
C   ASSIGN LINE END COORDINATES BY LINE DIRECTION                               
C                                                                               
 610  GO TO (620,630,640,630,640,640),LDIR                                      
 620  ENDX = XGRID(I-1)                                                         
      ENDY = PT2                                                                
      GO TO 650                                                                 
 630  ENDX = PT3                                                                
      ENDY = YGRID(N)                                                           
      GO TO 650                                                                 
 640  ENDX = XGRID(I)                                                           
      ENDY = PT4                                                                
C                                                                               
C   OUTPUT LINE SEGMENT                                                         
C                                                                               
 650  CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K) = MSTRT                                                           
      ENDPT(K) = MEND                                                           
      IF (K-KMAX ) 690,790,790                                                  
C                                                                               
C   THIS IS THE FOUR SIDE CASE - DETERMINE MOST PROBABLE PAIR OF LINES          
C   USING DIAGONALS                                                             
C                                                                               
 660  IF (ABS(CLEV(J)-(ARRAY(N-1,I)+ARRAY(N,I-1))/2.)-ABS(CLEV(J)-              
     1(ARRAY(N-1,I-1)+ARRAY(N,I))/2.)) 680,670,670                              
C                                                                               
C   LINES DIRECTION PAIR 1 AND 6                                                
C                                                                               
 670  STX = PT1                                                                 
      STY = YGRID(N-1)                                                          
      ENDX = XGRID(I-1)                                                         
      ENDY = PT2                                                                
C                                                                               
C   OUTPUT LINE SEGMENT                                                         
C                                                                               
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K)= MSTRT                                                            
      ENDPT(K)= MEND                                                            
      IF (K-KMAX ) 675,790,790                                                  
 675  STX = PT3                                                                 
      STY = YGRID(N)                                                            
      ENDX = XGRID(I)                                                           
      ENDY = PT4                                                                
C                                                                               
C   OUTPUT LINE SEGMENT                                                         
C                                                                               
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K)=MSTRT                                                             
      ENDPT(K) = MEND                                                           
      IF (K-KMAX ) 690,790,790                                                  
C                                                                               
C   LINE DIRECTION PAIR 4 AND 3                                                 
C                                                                               
 680  STX = PT1                                                                 
      STY = YGRID(N-1)                                                          
      ENDX = XGRID(I)                                                           
      ENDY = PT4                                                                
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K) = MSTRT                                                           
      ENDPT(K) = MEND                                                           
      IF (K-KMAX ) 685,790,790                                                  
 685  STX = XGRID(I-1)                                                          
      STY = PT2                                                                 
      ENDX = PT3                                                                
      ENDY = YGRID(N)                                                           
      CALL PUT(STX,STY,ENDX,ENDY,MSTRT,MEND)                                    
      K=K+1                                                                     
      STPT(K)=MSTRT                                                             
      ENDPT(K)=MEND                                                             
      IF (K-KMAX ) 690,790,790                                                  
 690  CONTINUE                                                                  
C                                                                               
C   ANY DATA THIS LEVEL                                                         
C                                                                               
C     IF (K) 810,810,9100                                               DBUG    
      IF (K) 810,810,910                                                        
 810  continue
c      WRITE(6,140) PLEV(J)                                                      
c 140  FORMAT(' NO DATA FOR DB LEVEL ',F8.4)                                     
      GO TO 700                                                                 
C                                                                               
C   CONNECT POINTS AND PLOT                                                     
C                                                                               
C9100 WRITE(3,9101) J                                                   DBUG    
C9101 FORMAT('1DATA FOR LEVEL',I5/)                                     DBUG    
C     WRITE(3,9102) (STPT(IKJ),ENDPT(IKJ),IKJ=1,K)                      DBUG    
C9102 FORMAT(1X,5(2I11,3X))                                             DBUG    
 910  NN=1                                                                      
C                                                                               
C   DOES START OR END POINT START ON THE EDGE                                   
C                                                                               
 915  IST = 0                                                                   
      MX = MAX0(NN-1,K-NN)                                                      
      DO 925 LL=1,MX                                                            
      L=NN + LL                                                                 
      IF (L-K) 916,916,920                                                      
 916  IF (STPT(L)) 930,920,917                                                  
 917  IF (ENDPT(L)) 950,918,918                                                 
 918  IF (IST) 920,919,920                                                      
 919  IST=L                                                                     
 920  L = NN - LL                                                               
      IF (L-1) 925,921,921                                                      
 921  IF (STPT(L)) 930,925,922                                                  
 922  IF (ENDPT(L)) 950,923,923                                                 
 923  IF (IST) 925,924,925                                                      
 924  IST = L                                                                   
 925  CONTINUE                                                                  
C                                                                               
C   ANY MORE DATA VALUES LEFT                                                   
C                                                                               
      IF (IST) 926,700,926                                                      
 926  L = IST                                                                   
      NIS = STPT(L)                                                             
C                                                                               
C   BEGIN ON START EDGE                                                         
C                                                                               
 930  STPT(L) = IABS(STPT(L))                                                   
      CALL PT(STPT(L),XS,YS)                                                    
C     WRITE(3,9103) L,XS,YS                                             DBUG    
C9103 FORMAT(' START ',I5,2(2X,F8.2))                                   DBUG    
C  JF  CHANGES MADE FOR THE SUBMATRIX CASE                                      
 2133 IF(IXMN-1) 790, 2001, 2000                                                
 2000 XS=XS-(IXMN-1)*XSCAL                                                      
 2001 IF(IYMN-1) 790, 2003, 2002                                                
 2002 YS=YS+(IYMN-1)*YSCAL                                                      
 2003 CALL PLOT(XS,YS,3)                                                        
      XMX = XS                                                                  
      YMY = YS                                                                  
C                                                                               
C                                                                               
C   CONTINUE ON START VALUES                                                    
C                                                                               
 940  LAST = ENDPT(L)                                                           
      GO TO 970                                                                 
C                                                                               
C   BEGIN ON END EDGE                                                           
C                                                                               
 950  ENDPT(L) = IABS(ENDPT(L))                                                 
      CALL PT(ENDPT(L),XS,YS)                                                   
C     WRITE(3,9103) L,XS,YS                                             DBUG    
 2134 IF(IXMN-1) 790, 2005, 2004                                                
 2004 XS=XS-(IXMN-1)*XSCAL                                                      
 2005 IF(IYMN-1) 790, 2007, 2006                                                
 2006 YS=YS+(IYMN-1)*YSCAL                                                      
 2007 CALL PLOT(XS,YS,3)                                                        
      XMX = XS                                                                  
      YMY = YS                                                                  
C                                                                               
C   CONTINUE ON END VALUES                                                      
C                                                                               
 960  LAST = STPT(L)                                                            
 970  STPT(L) = 0                                                               
      ENDPT(L) = 0                                                              
C                                                                               
C   IS THIS THE LAST VALUE                                                      
C                                                                               
      IF (LAST) 975,975,980                                                     
 975  LAST = IABS(LAST)                                                         
      CALL PT(LAST,XS,YS)                                                       
C     WRITE(3,9104) L,XS,YS                                             DBUG    
C9104 FORMAT('   END ',I5,2(2X,F8.2))                                   DBUG    
 2135 IF(IXMN-1) 790, 2009, 2008                                                
 2008 XS=XS-(IXMN-1)*XSCAL                                                      
 2009 IF(IYMN-1) 790, 2011, 2010                                                
 2010 YS=YS+(IYMN-1)*YSCAL                                                      
 2011 CALL PLOT(XS,YS,2)                                                        
      NN = L                                                                    
      IF (YMY - YS) 976,1010,1010                                               
 976  XMX = XS                                                                  
      YMY = YS                                                                  
C   START ON NEW POINT AFTER LABEL ROUTINE                                      
C                                                                               
C   START ON NEW POINT AFTER LABEL ROUTINE                                      
C                                                                               
      GO TO 1010                                                                
C                                                                               
C   CONTINUE IN ROUTINE                                                         
 980  CALL PT(LAST,XS,YS)                                                       
C     WRITE(3,9105) L,XS,YS                                             DBUG    
C9105 FORMAT('       ',I5,2(2X,F8.2))                                   DBUG    
 2136 IF(IXMN-1) 790, 2013, 2012                                                
 2012 XS=XS-(IXMN-1)*XSCAL                                                      
 2013 IF(IYMN-1) 790, 2015, 2014                                                
 2014 YS=YS+(IYMN-1)*YSCAL                                                      
 2015 CALL PLOT(XS,YS,2)                                                        
      IF (YMY-YS) 981,985,985                                                   
 981  XMX = XS                                                                  
      YMY = YS                                                                  
 985  MX = MAX0(L-1,K-L)                                                        
      NN = L                                                                    
      DO 1000 LL=1,MX                                                           
      L = NN + LL                                                               
      IF (L-K)  986,986,990                                                     
 986  IF (STPT(L)) 987,990,987                                                  
 987  IF (STPT(L)-LAST) 988,940,988                                             
 988  IF (ENDPT(L)-LAST) 990,960,990                                            
 990  L = NN - LL                                                               
      IF (L-1) 1000,991,991                                                     
 991  IF (STPT(L)) 992,1000,992                                                 
 992  IF (STPT(L)-LAST) 993,940,993                                             
 993  IF (ENDPT(L)-LAST) 1000,960,1000                                          
 1000 CONTINUE                                                                  
      CALL PT(NIS,XS,YS)                                                        
C     WRITE(3,9104) L,XS,YS                                             DBUG    
 2137 IF(IXMN-1) 790, 2017, 2016                                                
 2016 XS=XS-(IXMN-1)*XSCAL                                                      
 2017 IF(IYMN-1) 790, 2019, 2018                                                
 2018 YS=YS+(IYMN-1)*YSCAL                                                      
 2019 CALL PLOT(XS,YS,2)                                                        
C     CALL PLOT (XS,YS,2)                                                       
C                                                                               
C   LABEL HIGHEST POINT ON CONTOUR                                              
C                                                                               
 1010 CRT = PLEV(J)                                                             
c
c   here is some number displace emnts which is assumed to be in inches
c   since whowle plot is now from 0 to 1 in both directions, must be changed
c   somehow. for the time being is just disabled
c
c     ADD=0.02
      IF(PLEV(J)-10.0) 4000,4001,4001                                           
 4000 ADD=0.1                                                                   
      GO TO 4002                                                                
 4001 ADD=0.15                                                                  
 4002 YMY=YMY+0.02                                                              
c     XMX=XMX-ADD                                                               
c      CALL NUMBER (XMX,YMY,.1,CRT,0.,-1)
      call number(xlev(j),xmx,ymy)
      GO TO 915                                                                 
C
 790  WRITE ( 3,8100)
 8100 FORMAT(' TOO MANY POINTS, THIS LEVEL IGNORED')
c
 700  CONTINUE                                                                  
c
c    Return to Caller...
c    ===================
c
9999  return
      END   
                                                                    
      subroutine contour( x, y, n )
CSTART**************************************************************************
C                                                                              *
C   Supplier          : BGS/GSRG Applications Programming Unit                 *
C   System            : SEISAN                                                 *
C   Name              : CONTOUR                                                *
C   Purpose           : To plot contours in the map frame.                     *
C                                                                              *
C   Arguments  -input : x(1), y(1)   (R) Arrays of longitude and latitude pairs*
C                                        in degrees                            *
C                     : n            (I) Number of points in array             *
C             -Output : None                                                   *
C                                                                              *
C   Note              : Unlike the previous version which only plotted whole   *
C                       segments inside the map frame, this takes account of   *
C                       partial segments in contact with the edge of the frame.*
C                                                                              *
C   Author            : J. A. Bolton                                           *
C   Date              : 2 June 1995                                            *
C   Version           : V02                                                    *
C                                                                              *
CEND****************************************************************************
c
      external     map_bdy                 ! Map boundary routine.
     &            ,map_proj                ! Map projection.
     &            ,xout                    ! PostScript re-set.
c
c    System definitions...
c    =====================
c
      include      'epimap.inc'            ! Epimap definitions.
c
c    Arguments...
c    ============
c
      integer      n                       ! # co-ordinate pairs in block.
      real         x(*)                    ! Longitudes in degrees.
     &            ,y(*)                    ! & latitude.
c
c    Local variables...
c    ==================
c
      integer      ix                      ! Very local.
     &            ,count                   ! Count points.
     &            ,post_count              ! PostScript reset interval.
      parameter   (post_count = 100)       ! & value.
      real         xx  ,yy                 ! Temporary locations.
     &            ,xl  ,yl                 ! & projected location.
     &            ,xs                      ! & saved projected location (long).
      logical      b_this                  ! This point inside?
     &            ,b_last                  ! Last point inside?.
     &            ,b_plot                  ! Point plotted?.
     &            ,b_start                 ! 1st point in sequence?.
     &            ,b_delimiter             ! Delimiter point?.
     &            ,b_skip                  ! Skip this point?
     &            ,b_wrap                  ! Wrap around?
     &            ,b_revolve               ! Full longitude range?.
     &            ,b_out                   ! Outside drawing frame?.
c
c    Initialise...
c    =============
c
      count     = 0                                 ! Interval plot count.
      b_start   = .true.                            ! Flag to start (default)!.
      b_wrap    = abs(maxlong$-minlong$) .gt. 180.0 ! Possible wrap around?
      b_revolve = abs(maxlong$-minlong$) .eq. 360.0 ! Full revolution?
c
c    Loop points in this sequence...
c    ===============================
c
      do ix = 1, n                          ! Loop points.
      b_delimiter = abs(y(ix)) .ge. 90.0    ! A delimiter (outside poles!).
c
c    Prepare for action...
c    =====================
c    None yet...
c    -----------
c
      if( b_delimiter ) then                ! No action.
      b_start = .true.                      ! Must start sometime!
      b_this  = .false.                     ! This point not inside.
c
c    Account for International dateline...
c    -------------------------------------
c
      else                                  ! Otherwise.
         if( b_revolve ) then               ! Full revolution.
         x(ix) = mod(x(ix)+360.0,360.0)     ! In its 360 degree space.
c
         else if( maxlong$ .gt. 180.0   .and.
     &            minlong$ .lt. 180.0   .and.
     &            minlong$ .gt.   0.0   .and.
     &            x(ix)    .lt.   0.0 ) then
                  x(ix) = x(ix) + 360.0
c
         else if( minlong$ .lt. -180.0   .and.
     &            x(ix)    .gt.    0.0 ) then
         x(ix) = x(ix) - 360.0
         end if
c
c     Determine if this point is in the map region...
c     -----------------------------------------------
c
      if(ix.gt.1) then                      ! jh fix 13-4-14 to avoid ix-1  .eq 0
      b_skip = ix    .ne. 1       .and.     ! Skip this point?.
     &         x(ix) .eq. x(ix-1) .and.     !
     &         y(ix) .eq. y(ix-1)           !
      endif
c
         if( b_skip ) then                  ! No action.
         continue                           !
c
         else                               !
         b_last = b_this                    ! Last point inside?.
         b_this = b_revolve           .or.  ! This point inside?.
     &           (x(ix) .gt. minlong$ .and. !
     &            x(ix) .lt. maxlong$)      !
         b_this = b_this              .and. ! Truely?.
     &            y(ix) .gt. minlatn$ .and. ! This point inside?
     &            y(ix) .lt. maxlatn$       !
         end if                             !
      end if 
c
c    This point is a delimiter, or same a last...
c    ============================================
c
      if( b_delimiter .or. b_skip ) then    ! No action.
      b_plot = .false.                      !
c
c    1st point of a sequence...
c    ==========================
c
      else if( b_start ) then                   ! Flagged to start.
      b_start = .false.                         ! Now started.
c
         if( b_this ) then                      ! Start with this point inside.
         call map_proj(x(ix), y(ix), xl, yl, 1) ! Project this point.
         b_out = xl .lt. 0.0     .or.           ! Outside drawing area?.
     &           yl .lt. 0.0     .or.           !
     &           xl .gt. xsize$  .or.           !
     &           yl .gt. ysize$                 !
            if( b_out ) then                    ! No plot.
            b_this = .false.                    !
            else                                ! Otherwise.
            call xmovabs( xl+refx0$, yl+refy0$ )! & move there.
            call xout   ( xl+refx0$, yl+refy0$ )! & postscript stroke & reset.
            count = 0                           ! PostScript command count.
            end if                              !
         end if                                 !
c
c     Line segment is totally inside the map area...
c     ==============================================
c
      else if( b_last .and. b_this ) then        ! Totally inside.
      xs = xl                                    ! Save location.
      call map_proj(x(ix), y(ix), xl, yl, 1)     ! Project this point.
      b_out = xl .lt. 0.0     .or.               ! Outside drawing area?.
     &        yl .lt. 0.0     .or.               !
     &        xl .gt. xsize$  .or.               !
     &        yl .gt. ysize$                     !
c
         if( b_wrap                       .and.  ! Assume wrap around.
     &      (.not.b_out)                  .and.  ! But not outside.
     &       abs(xs-xl) .gt. xsize$*0.5 ) then   !
         call xmovabs( xl+refx0$, yl+refy0$ )    ! & move there.
         call xout   ( xl+refx0$, yl+refy0$ )    ! & postscript stroke & reset.
         count = 0                               ! PostScript command count.
c
         else if( b_out ) then                   ! Outside drawing.
         b_this = .false.                        !
c
         else                                    !
         call xdrwabs( xl+refx0$, yl+refy0$ )    ! & draw to there.
         count = count + 1                       ! PostScript reset count.
         end if                                  !
c
c     Line is leaving the map area...
c     ===============================
c
      else if( b_last ) then                 ! Last point inside.
      xs = xl                                ! Store point.
      call map_bdy( x(ix-1) ,y(ix-1)         ! Last point.
     &             ,x(ix)   ,y(ix)           ! This point.
     &             ,xl      ,yl )            ! Projected crossing point
      b_out = xl .lt. 0.0     .or.           ! Outside drawing area?.
     &        yl .lt. 0.0     .or.           !
     &        xl .gt. xsize$  .or.           !
     &        yl .gt. ysize$                 !
      b_plot = .not.b_out                    ! Identify plotted point.
c
         if( b_wrap                       .and. ! Assume wrap around.
     &       b_plot                       .and. !
     &       abs(xs-xl) .gt. xsize$*0.5 ) then  !
         call xmovabs( xl+refx0$, yl+refy0$ )   ! & move there.
         call xout   ( xl+refx0$, yl+refy0$ )   ! & postscript stroke & reset.
         count = 0                              ! PostScript command count.
c
         else if( b_plot ) then                 !
         call xdrwabs( xl+refx0$, yl+refy0$ )   ! & draw to there.
         count = count + 1                      ! PostScript reset count.
         end if                                 !
c
c     Line is entering the map area...
c     ================================
c
      else if( b_this ) then                 ! This point inside.
      call map_bdy( x(ix-1) ,y(ix-1)         ! Last point.
     &             ,x(ix)   ,y(ix)           ! This point.
     &             ,xl      ,yl )            ! Projected crossing point
      b_out = xl .lt. 0.0     .or.           ! Outside drawing area?.
     &        yl .lt. 0.0     .or.           !
     &        xl .gt. xsize$  .or.           !
     &        yl .gt. ysize$                 !
c
      b_plot = .not.b_out                    ! Identify plotted point.
         if( b_plot ) then                   !
         call xmovabs( xl+refx0$, yl+refy0$ )! & move there.
         call xout   ( xl+refx0$, yl+refy0$ )! & do it at current point.
         count = 0                           ! PostScript reset count.
         end if                              !
c
      xs = xl                                   ! Store x location.
      call map_proj( x(ix), y(ix), xl, yl, 1 )  ! Project this point.
      b_out = xl .lt. 0.0     .or.              ! Outside drawing area?.
     &        yl .lt. 0.0     .or.              !
     &        xl .gt. xsize$  .or.              !
     &        yl .gt. ysize$                    !
      b_this = .not.b_out                       ! This point really inside?.
c
         if( b_wrap                       .and. ! Assume wrap around.
     &       b_plot                       .and. ! Last plotted.
     &       b_this                       .and. ! This point inside?.
     &       abs(xs-xl) .gt. xsize$*0.5 ) then  !
         call xmovabs( xl+refx0$, yl+refy0$ )   ! & move there.
         call xout   ( xl+refx0$, yl+refy0$ )   ! & postscript stroke & reset.
         count = 0                              ! PostScript command count.
c
         else if( b_this .and. b_plot ) then    ! Draw.
         call xdrwabs( xl+refx0$, yl+refy0$ )   ! & draw to there.
         count = count + 1                      ! PostScript reset count.
c
         else if( b_this ) then                 ! Just move.
         b_plot = .true.                        !
         call xmovabs( xl+refx0$, yl+refy0$ )   ! & move there.
         call xout   ( xl+refx0$, yl+refy0$ )   ! & postscript stroke & reset.
         count = 0                              ! PostScript command count.
         end if                                 !
c
c     Line outside but might clip the map area...
c     ===========================================
c     Determine crossings from either direction..
c     ------------------------------------------- 
c     Must have at least one point...
c
      else
      call map_bdy( x(ix-1) ,y(ix-1)           ! Last point.
     &             ,x(ix)   ,y(ix)             ! This point.
     &             ,xx      ,yy )              ! Projected crossing point
      b_out = xx .lt. 0.0     .or.             ! Outside drawing area?.
     &        yy .lt. 0.0     .or.             !
     &        xx .gt. xsize$  .or.             !
     &        yy .gt. ysize$                   !
c
c     Area is clipped?...
c    
         if( .not.b_out ) then                 ! Clip?
         call map_bdy( x(ix)   ,y(ix)          ! This point.
     &                ,x(ix-1) ,y(ix-1)        ! To last point.
     &                ,xl      ,yl )           ! Projected crossing point.
         b_out = xl .lt. 0.0     .or.          ! Outside drawing area?.
     &           yl .lt. 0.0     .or.          !
     &           xl .gt. xsize$  .or.          !
     &           yl .gt. ysize$                !

         b_plot = (.not.b_out)   .and.         ! Valid point and
     &            xx  .ne. xl    .and.         ! Not the same.
     &            yy  .ne. yl                  ! Point.
c
c         ...clip the area...
c
            if( b_plot ) then                      ! Area clipped.
            call xmovabs( xx+refx0$, yy+refy0$ )   ! & move there.
            call xdrwabs( xl+refx0$,yl+refy0$ )    ! Other crossing.
            call xout   ( xl+refx0$,yl+refy0$ )    ! Postscript stroke & reset.
            count = 0                              ! PostScript command count.
            end if                                 !
c
c     Area is not clipped?...
c
         else
         b_plot = .false.                      ! No plotting.
         end if                                !
      end if                                   !
c
c    Clear PostScript array...
c    =========================
c
      b_plot = b_this .or. b_plot           ! Flagged to plot or is inside.
      if( b_plot                  .and.     ! This point plotted.
     &    count .eq. post_count ) then      ! Re-set reached.
      call xout( xl+refx0$, yl+refy0$ )     ! & do it at current point.
      count = 0                             ! & re-set.
      end if                                !
      end do                                !
c
c    Return to Caller...
c    ===================
c
9999  return
      end
c
        subroutine depth_ellipse
     *  (erx,ery,erz,cvxy,cvxz,cvyz,dirnab,emaj,emin,ang)

c calculate major and minor axes, ad(i) & bd(i) (km) and angle relative
c to horizontal, angd(i) in direction, dirnab (rad E of N)

c Barry Lienert Oct 94

       implicit none
       real ae(9),ve(9),emaj,emin,ang,dirnab
       real anga,a,ah,av,angb,bh,bv,b,angc,ch,cv,c
       real erx,ery,erz,cvxy,cvxz,cvyz,var(3,3)
c
c   covarriance matrix
c
        var(1,1)=erx*erx
        var(2,2)=ery*ery
        var(3,3)=erz*erz
        var(1,2)=cvxy
        var(1,3)=cvxz
        var(2,3)=cvyz
        var(2,1)=var(1,2)
        var(3,1)=var(1,3)
        var(3,2)=var(2,3)
c
c
        call ellipse_x(var,ae,ve)

c find the projection of components of the major ellipse axes (the 3
c eigenvalues of the covariance matrix) on the AB-Z plane
        anga=atan2(ve(3),sqrt(ve(1)**2+ve(2)**2))
        ah=sqrt(abs(ae(1)))*cos(anga)
        av=sqrt(abs(ae(1)))*sin(anga)
        ah=ah*cos(atan2(ve(2),ve(1))-dirnab)
        a=sqrt(ah*ah+av*av)
        anga=atan2(av,ah)

c        write(*,*)ah,av,dirnab*180./pi,atan2(ve(2),ve(1))*180./pi

        angb=atan2(ve(6),sqrt(ve(4)**2+ve(5)**2))
        bh=sqrt(abs(ae(3)))*cos(angb)
        bv=sqrt(abs(ae(3)))*sin(angb)
        bh=bh*cos(atan2(ve(5),ve(4))-dirnab)
        b=sqrt(bh*bh+bv*bv)
        angb=atan2(bv,bh)
c        write(*,*)b

        angc=atan2(ve(9),sqrt(ve(7)**2+ve(8)**2))
        ch=sqrt(abs(ae(6)))*cos(angc)
        cv=sqrt(abs(ae(6)))*sin(angc)
        ch=ch*cos(atan2(ve(8),ve(7))-dirnab)
        c=sqrt(ch*ch+cv*cv)
        angc=atan2(cv,ch)
c        write(*,*)c

c major & minor axes are the largest two of these
        if(a.ge.b.and.a.ge.c)then
         emaj=a

c angle between major axis, a, and horiz axis
         ang=anga

         if(b.ge.c)then
          emin=b
         else
          emin=c
         endif
        elseif(b.ge.a.and.b.ge.c)then
         emaj=b
         ang=angb
         if(c.ge.a)then
          emin=c
         else
          emin=a
         endif
        elseif(c.ge.a.and.c.ge.b)then
         emaj=c
         ang=angc
         if(b.ge.a)then
          emin=b
         else
          emin=a
         endif
        endif
        return
        end
c------------------------------------------------------------------
      subroutine depth_plot(plot_unit,n,x,y,max_depth,mag,
     *erx_a,ery_a,erz_a,cvxy_a,cvxz_a,cvyz_a,dirnab,
     *number_plot,number,auto_depth,title,p_number,elip,
     &chr_epi_col,strike,dip,rake,fault )
c
c   makes a earthquake depth profile
c   j havskov, sep 92 
C!JAB(BGS)Jan95   : epicentre file colours installed.
C!JAB(BGS)May95   : Bug in working xmax, use k instead of i array pointer.
C!JAB(BGS)May95   : Introduce distance scaling in the event max-depth > max
C!JAB(BGS)May95     distance, when not in auto_depth mode.
C!JAB(BGS)Jun95   : Put inscaling inside this routine.
c
c     input:      n: number of earthquakes+1, last value is for scaling, thus
c                    n must be at least 2
c                 x,y: distance and depths (positive) in km
c                 max_depth: maximum depth used for scaling, if zero use
c                            maximum depth in data and return it in max_depth
c                 mag; magnitudes
c                 er.. : element of covariance matrix, sort of
c                 dirnab: azimuth of profile
c                 number_plot: indicator for plotting numbers
c                 number: event numbers to plot
c                 title: plot title
c                 p_number: profile number
c
c
       implicit none
       include 'libsei.inc'               ! Library definitions.
       include 'epimap.inc'               ! Epimap & plotting parameters.
       external sei clen                  ! String length.
     &         ,sei code                  ! Error handler.
       integer  sei clen                  ! & function.
       integer  sym_ext                   ! Symbol indicator extension
c
      integer    plot_unit                        ! PostScript unit.
     &          ,code                             ! Condition code.
      logical    b_flag                           ! Operations flag!.
      real       depth_scaling                    ! Postscript depth scale.
     &          ,depth_sym_scale                  ! & symbol scaling.
      parameter (depth_scaling = 0.4)             ! & value.
c      parameter (depth_sym_scale = plot_scalex$   ! Ditto.
c     &                           / depth_scaling) !
c
       integer   colour, old_colour       ! Epimap colours & old colours.
c
       real x(*),y(*),mag(*),max_depth
       character    chr_epi_col(*)*2    ! Epicentre colours.
       integer n,number(*),p_number
       character*1 number_plot
c--auto scaling of depth or fixed to same SCALE$ as horizontal axis
       logical auto_depth
       character*80 title
c--length of axis in tek units
       real xlength,ylength
c--scaling factors
       real xSCALE,ySCALE
c--max values
       real XMAX,YMAX
c--for fps
       real strike(*),dip(*),rake(*)
       logical fault
       real YMIN,                          ! Minimum depth.
     &      deltaY                         ! Working depth range.
c
c--position of 0,0 of frame
       real X0,Y0
c--x and y axix divisions
       real xdiv,ydiv
c--permitted numbers on axix
       real anumber(10)                      
c--covariance matrix elements
       real erx_a(*),ery_a(*),erz_a(*),cvxy_a(*),cvxz_a(*),cvyz_a(*)
c--azimuth of profile, major and minor axis of error ellipse, angle
c  with horizontal of ellipse 
       real dirnab,emaj,emin,ang
c--symbol size
       real symb_size
c--ellipse plotting flag
       logical elip
c--help variables
       character*80 text
       real xd,yd
       integer i,j,k, jy, jx
       data anumber/1.,2.,5.,10.,20.,
     &              50.,100.,200.,500.,1000./

         call get_env_psscale(PLOT_SCALEX$,PLOT_SCALEY$)
         depth_sym_scale = plot_scalex$ / depth_scaling

c
c    Scale down...
c    -------------
c
      write(plot_unit,'(2(1x,f6.3),'' scale'')',iostat=code) ! Details.
     &      depth_scaling, depth_scaling
      call sei code( fort$, code, plot_unit, b_flag  )! Process outcome.
c
c    Allocate extension if we need to fill symbols...
c    ------------------------------------------------
c
       if( b_sym_epic_fill$ ) then               ! Fill symbols?.
       sym_ext = fill$                           ! Yes.
       else                                      ! Otherwise.
       sym_ext = 0                               ! No extension.
       end if                                    !
c
c    --------
c
       XMAX=0.0
       YMAX=0.0
       YMIN = 1.0d6                              ! Large value.
       xlength = 900.0
       ylength = 700.0
       X0=80.0
       Y0=50.0
c
c     Scale according to auto_depth...
c     --------------------------------
c
       if( auto_depth ) then                   !JAB(BGS)May95 No action.
       continue                                !JAB(BGS)May95
       else                                    !JAB(BGS)May95 Otherwise.
       x0 = x0 + 0.5 * (xlength - ylength)     !JAB(BGS)May95 Adjust left edge.
       xlength = ylength                       !JAB(BGS)May95 & scale 1 to 1.
       end if                                  !JAB(BGS)May95
c
c   find min & max values
c
       do i=1,n
         k=number(i)
         if(x(k).gt.XMAX) XMAX=x(k)              !JAB(BGS)May95.
         if(k.ne.n .and. y(k).lt.YMIN) YMIN=y(k) !JAB(BGS)May95.
         if(y(k).gt.YMAX) YMAX=y(k)
       enddo
       if(YMAX.eq.0.0) YMAX=10.0
       if(XMAX.eq.0.0) XMAX=10.0
c
c   add 5% to make plot look nicer
c
       YMIN = amax1( 0.0, ymin-0.05*ymax )      !JAB(BGS)May95 Not above ground.
       XMAX=XMAX*1.05
       YMAX=YMAX*1.05
c
c   save max value in depth if not input, else save 
c
       if(max_depth.eq.0.0) then
          max_depth=YMAX
       else
          YMAX=max_depth
       endif
c
c  check if same SCALE in x and z, if so set max depth equal max x-distance
c
       if( auto_depth ) then                  ! Automatic scaling.
       ymin = 0.0                             ! Ground level.
       deltaY = ymax                          ! Depth range.
c
       else if( ymax .gt. xmax ) then         ! Some extra scaling required?.
       ymin = int(ymin)                       ! Shallow depth to upper km.
       deltaY = ymax - ymin                   ! Reduced depth range.
          if( deltaY .gt. xmax ) then         ! Still too large, scale x.
          xdiv = xmax * xlength / deltaY      ! Xrange scaled to depth range.
          x0   = x0 + 0.5*(xlength-xdiv)      ! New start location.
          xlength = xdiv                      ! & range.
          else                                ! Otherwise.
          deltaY = xmax                       ! Make same as x range.
          end if                              !
c
       else                                   ! No extra scaling requied.
       ymin = 0.0                             ! Start at the ground.
       deltay = xmax                          ! & same range as distance.
       end if                                 !
c
c   find axis divisions, assume max 10 tics
c
       xdiv = XMAX/10.0
       ydiv = deltaY / 10.0                   ! Use true depth range.
c
       xd   = anumber(1)                      !JAB(BGS)May95 Initialise.
       yd   = anumber(1)                      !JAB(BGS)May95 Ditto.
       do i=1,9
          if(xdiv.gt.anumber(i)) xd=anumber(i+1)
          if(ydiv.gt.anumber(i)) yd=anumber(i+1)
       enddo
c
c    Small ranges....
c
       if( deltaY .lt. 0.5 ) then
       ydiv = 0.1
       else if( deltaY .lt. 1.0 ) then
       ydiv = 0.2
       else if( deltaY .lt. 2.5 ) then
       ydiv = 0.5
       else
       ydiv = yd
       end if
c
       if( xmax .lt. 0.5 ) then
       xdiv = 0.1
       else if( xmax .lt. 1.0 ) then
       xdiv = 0.2
       else if( xmax .lt. 2.5 ) then
       xdiv = 0.5
       else
       xdiv = xd
       end if
c
c   scaling factors
c
       xSCALE = xlength/XMAX
       ySCALE = ylength/deltaY                  !JAB(BGS)May95.
c
c   plot title...
c
       call xset_color(color_title)
       call xchars(title,seiclen(title),X0,Y0+ylength+20) !JAB(BGS)May95.
c
c   plot profile number
c
       write(text(1:1),'(i1)') p_number
       call xchars(text,1,X0+xlength+30.0,Y0+ylength) !JAB(BGS)May95.
c
c   plot frame
c
       call xset_color(color_frame)
       call xmovabs(X0,Y0)
       call xdrwabs(X0,Y0+ylength)
       call xdrwabs(X0+xlength,Y0+ylength)
       call xdrwabs(X0+xlength,Y0)
       call xdrwabs(X0,Y0)
c
c   plot axis tics with number, first x axis...
c   -------------------------------------------
c
       call xset_color(color_frame)                ! color of axis ticks
       yd = Y0 - 40.0                              ! Height position.
       jx = (XMAX/xdiv) + 1                        ! # ticks.
c
       do i = 1, jx                                ! Loop ticks.
       j  = xdiv*(i-1)                             ! Tick value.
c
          if( i .gt. 1 ) then                      ! Not the start.
          call xmovabs(X0+(i-1)*xdiv*xSCALE,Y0)    ! Move to tick location.
          call xdrwabs(X0+(i-1)*xdiv*xSCALE,Y0+10) ! & stroke it.
          end if                                   !
c
c       ....label the tick...
c
          text = ' '                                        ! Empty label.
          if( i .eq. jx ) then                              ! Last tick
             if( xdiv .lt. 1.0 ) then                       !
             write(text(1:8),'(f3.1,''   km'')') xdiv*(i-1) !
             else                                           ! Otherwise.
             write(text(1:8),'(i5,'' km'')') j              ! With units.
             end if                                         !
c
          else if( xdiv .lt. 1.0 ) then            !
          write(text(1:3),'(f3.1)') xdiv*(i-1)     !
          else                                     ! Otherwise.
          write(text(1:5),'(i5)') j                ! No units.
          end if                                   !
c
       xd = X0 + (i-1)*xdiv*xSCALE                 ! Width location.
     &    - 0.5*seiclen(text(1:5))*15.0            ! Centralise number.
       call xchars(text,seiclen(text),xd,yd)       ! & write.
       enddo
c
c   plot y axis...
c   --------------
c
       jx = (deltaY/ydiv) + 1                      ! # tick locations.
c
       do i = 1, jx                                ! Loop tick locations.
       yd = ylength+Y0 - (i-1)*ydiv*ySCALE - 5.0   ! Height location of tick.
       j = ymin + ydiv*(i-1)                       ! Label.
c
          if( i .gt. 1 ) then                                ! Make a tick mark.
          call xmovabs(X0,ylength+Y0-(i-1)*ydiv*ySCALE)      ! Move to left.
          call xdrwabs(X0+10.0,ylength+Y0-(i-1)*ydiv*ySCALE) ! & stroke it.
          end if                                             !
c
       text = ' '           
          if( ydiv .lt. 1.0 ) then                 !
          write(text(1:3),'(f3.1)') ymin+ydiv*(i-1)!
          else                                     !
          write(text(1:5),'(i5)') j                !
          end if                                   !
       xd = X0 - 25.0                              ! Left edge of tick.
     &    - seiclen(text(1:5))*15.0                !
       call xchars(text,5,xd,yd)
       enddo
c
c   plot events, remember last value was just for scaling so n-1
c   ------------------------------------------------------------
c
       colour = 0                           ! Allocation.
       do i=1,n-1                           ! Loop events.
       k=number(i)                          ! Event #.
c
       old_colour = colour                         ! Save last colour.
       read(chr_epi_col(k),'(i2)',err=1000) colour ! Get this colour.
1000      if( colour .ne. old_colour ) then        ! Change colour.
             if( colour .lt. 0 ) then              ! Default.
             call xset_color(color_epi)            ! Colour.
             else                                  ! Override.
             call xset_color( colour )             ! Set it.
             end if                                !
          end if                                   !
c
         xd = x(i)*xSCALE + X0                     ! Locate point.
         yd = ylength+Y0 - (y(k)-ymin)*ySCALE      ! Ditto.
c
c  calculate major and minor axis, but only if error less than 500
c
        if((elip.and.erx_a(k).gt.0.0).and.erx_a(k).le.500.0.and.
     *     ery_a(k).le.500.0.and.erz_a(k).le.500.0) then
           call depth_ellipse
     *     (erx_a(k),ery_a(k),erz_a(k),cvxy_a(k),cvxz_a(k),cvyz_a(k),
     *     dirnab,emaj,emin,ang)
c
c   plot ellipse
c
            call plot_depth_ellipse(30,X0,Y0,xSCALE,ySCALE,xlength,
     &      ylength,emaj,emin,ang,xd,yd)
 
        elseif(fault.and.strike(number(i)).lt.1000.0) then
c
c   plot fps
c
            call plot_foc(strike(number(i)),dip(number(i)),
     *      rake(number(i)),xd,yd,20.0,0)
        else
c
c    plot by magnitude...pro-rata...
c
           if( .not.b_sym_range$ ) then        ! Plot by size.
              if( mag(k) .le. 0.0 ) then       ! Special sysmbol.
              symb_size = 4.0*symbol_incr$     !
     &                  * depth_sym_scale      !
              call xsymbol(sym_mag0$+sym_ext,  !
     &                     symb_size,
     &                     xd,yd)
              else                             ! Pro-rata.
              symb_size = mag(k)*symbol_incr$  !
     &                  * depth_sym_scale      !
              call xsymbol(sym_mag$+sym_ext,symb_size,xd,yd) !
              end if                           !
c
c    plot by range allocation...
c
           else if( b_sym_small_range$ ) then
              if( mag(k) .lt. 0.0d0 ) then         !
              symb_size = 2.0*symbol_incr$         !
     &                  * depth_sym_scale          !
              call xsymbol(sym_magneg$+sym_ext,
     &                     symb_size,
     &                     xd,yd)
c
              else if( mag(k) .ge. 2.5d0) THEN
              symb_size = 7.5*symbol_incr$                    !
     &                  * depth_sym_scale                     !
              call xsymbol(sym_magx$+sym_ext,symb_size,xd,yd) !
c
              else
                 if( mag(k) .lt. 0.5d0 ) then
                 jy = 2
                 else if( mag(k) .lt. 1.0d0 ) then
                 jy = 3
                 else if( mag(k) .lt. 1.5d0 ) then
                 jy = 4
                 else if( mag(k) .lt. 2.0d0 ) then
                 jy = 5
                 else if( mag(k) .lt. 2.5d0 ) then
                 jy = 6
                 end if
              symb_size = (float(jy)+1.5)*symbol_incr$       !
     &                  * depth_sym_scale                    !
              call xsymbol(sym_mag$+sym_ext,symb_size,xd,yd) !
              end if
c
c    Normal ranges...
c
           ELSE IF(mag(k).le. 1.0d0) THEN 
           symb_size = 4.0*symbol_incr$     !
     &               * depth_sym_scale      !
           call xsymbol(sym_mag0$+sym_ext,
     &                  symb_size,
     &                  xd,yd)
c
           else if( mag(k) .gt. 6.0d0) THEN
           symb_size = 6.0*symbol_incr$                    !
     &               * depth_sym_scale                     !
           call xsymbol(sym_magx$+sym_ext,symb_size,xd,yd) !
c
           ELSE
              if( mag(k) .le. 2.0d0 ) then
              jy = 2
              else if( mag(k) .le. 3.0d0 ) then
              jy = 3
              else if( mag(k) .le. 4.0d0 ) then
              jy = 4
              else if( mag(k) .le. 5.0d0 ) then
              jy = 5
              else if( mag(k) .le. 6.0d0 ) then
              jy = 6
              end if
c
           symb_size = float(jy)*symbol_incr$              !
     &               * depth_sym_scale                     !
	   call xsymbol(sym_mag$+sym_ext,symb_size,xd,yd)  !
           endif
        endif                                                                   
c
        IF((number_plot.EQ.'Y').OR.(number_plot.EQ.'y')) THEN                     
           write(text,'(i6)') number(i)
           call sei left(text)
           if(fault) then
           call xchars(text,seiclen(text),xd,yd-30.0) 
           else
           call xchars(text,seiclen(text),xd,yd-20.0)
           endif                                            
        ENDIF                                                                     
        call xout(10.0,10.0)            
      enddo
      call xset_color(color_def)
c
c  ...scale back up..(reciprocal scaling)...
c
      write(plot_unit,'(2(1x,f6.3),'' scale'')',iostat=code) ! Details.
     &      (1.0/depth_scaling), (1.0/depth_scaling)
      call sei code( fort$, code, plot_unit, b_flag  )! Process outcome.
c
c    Return to Caller...
c    ===================
c
      return
      end                                         
c***************************************************************************
c        subroutine eigen
c
c        purpose
c           compute eigenvalues and eigenvectors of a real symmetric
c           matrix
c
c        usage
c           call eigen(a,r,n,mv)
c
c        description of parameters
c           a - original matrix (symmetric), destroyed in computation.
c               resultant eigenvalues are developed in diagonal of
c               matrix a in descending order.
c           r - resultant matrix of eigenvectors (stored columnwise,
c               in same sequence as eigenvalues)
c           n - order of matrices a and r
c           mv- input code
c                   0   compute eigenvalues and eigenvectors
c                   1   compute eigenvalues only (r need not be
c                       dimensioned but must still appear in calling
c                       sequence)
c
c        remarks
c           original matrix a must be real symmetric (storage mode=1)
c           matrix a cannot be in the same location as matrix r
c
c        subroutines and function subprograms required
c           none
c
c        method
c           diagonalization method originated by Jacobi and adapted
c           by Von Neumann for large computers as found in 'Mathematical
c           Methods for Digital Computers', edited by A. Ralston and
c           H.S. Wilf, John Wiley and sons, New York, 1962, chapter 7
c
c     ..................................................................
c
      subroutine epimap_eigen(a,r,n,mv)
      dimension a(*),r(*)
c
c        ...............................................................
c
c        if a double precision version of this routine is desired, the
c        c in column 1 should be removed from the double precision
c        statement which follows.
c
c      double precision a,r,anorm,anrmx,thr,x,y,sinx,sinx2,cosx,
c     1                 cosx2,sincs,range
c
c        the c must also be removed from double precision statements
c        appearing in other routines used in conjunction with this
c        routine.
c
c        the double precision version of this subroutine must also
c        contain double precision fortran functions.  sqrt in statements
c        40, 68, 75, and 78 must be changed to dsqrt.  abs in statement
c        62 must be changed to dabs. the constant in statement 5 should
c        be changed to 1.0d-12.
c
c        ..............................................................
c
c        generate identity matrix
c
    5 range=1.0e-7
      if(mv-1) 10,25,10
   10 iq=-n
      do 20 j=1,n
      iq=iq+n
      do 20 i=1,n
      ij=iq+i
      r(ij)=0.0
      if(i-j) 20,15,20
   15 r(ij)=1.0
   20 continue
c
c        compute initial and final norms (anorm and anormx)
c
   25 anorm=0.0
      do 35 i=1,n
      do 35 j=i,n
      if(i-j) 30,35,30
   30 ia=i+(j*j-j)/2
      anorm=anorm+a(ia)*a(ia)
   35 continue
      if(anorm) 165,165,40
40    anorm=sqrt(2.0*anorm)
      anrmx=anorm*range/dfloat(n)
c
c        initialize indicators and compute threshold, thr
c
      ind=0
      thr=anorm
   45 thr=thr/dfloat(n)
   50 l=1
   55 m=l+1
c
c        compute sin and cos
c
   60 mq=(m*m-m)/2
      lq=(l*l-l)/2
      lm=l+mq
   62 if(abs(a(lm))-thr) 130,65,65
   65 ind=1
      ll=l+lq
      mm=m+mq
      x=0.5*(a(ll)-a(mm))
   68 y=-a(lm)/sqrt(a(lm)*a(lm)+x*x)
      if(x) 70,75,75
   70 y=-y
75    sinx=y/sqrt(2.0*(1.0+(sqrt(abs(1.0-y*y)))))
      sinx2=sinx*sinx
   78 cosx= sqrt(1.0-sinx2)
      cosx2=cosx*cosx
      sincs =sinx*cosx
c
c        rotate l and m columns
c
      ilq=n*(l-1)
      imq=n*(m-1)
      do 125 i=1,n
      iq=(i*i-i)/2
      if(i-l) 80,115,80
   80 if(i-m) 85,115,90
   85 im=i+mq
      go to 95
   90 im=m+iq
   95 if(i-l) 100,105,105
  100 il=i+lq
      go to 110
  105 il=l+iq
  110 x=a(il)*cosx-a(im)*sinx
      a(im)=a(il)*sinx+a(im)*cosx
      a(il)=x
  115 if(mv-1) 120,125,120
  120 ilr=ilq+i
      imr=imq+i
      x=r(ilr)*cosx-r(imr)*sinx
      r(imr)=r(ilr)*sinx+r(imr)*cosx
      r(ilr)=x
  125 continue
      x=2.0*a(lm)*sincs
      y=a(ll)*cosx2+a(mm)*sinx2-x
      x=a(ll)*sinx2+a(mm)*cosx2+x
      a(lm)=(a(ll)-a(mm))*sincs+a(lm)*(cosx2-sinx2)
      a(ll)=y
      a(mm)=x
c
c        tests for completion
c
c        test for m = last column
c
  130 if(m-n) 135,140,135
  135 m=m+1
      go to 60
c
c        test for l = second from last column
c
  140 if(l-(n-1)) 145,150,145
  145 l=l+1
      go to 55
  150 if(ind-1) 160,155,160
  155 ind=0
      go to 50
c
c        compare threshold with final norm
c
  160 if(thr-anrmx) 165,165,45
c
c        sort eigenvalues and eigenvectors
c
  165 iq=-n
      do 185 i=1,n
      iq=iq+n
      ll=i+(i*i-i)/2
      jq=n*(i-2)
      do 185 j=i,n
      jq=jq+n
      mm=j+(j*j-j)/2
      if(a(ll)-a(mm)) 170,185,185
  170 x=a(ll)
      a(ll)=a(mm)
      a(mm)=x
      if(mv-1) 175,185,175
  175 do 180 k=1,n
      ilr=iq+k
      imr=jq+k
      x=r(ilr)
      r(ilr)=r(imr)
  180 r(imr)=x
  185 continue
      return
      end


      subroutine old_ellipse_x(var,ae,ve)
c     subroutine ellipse_x(var,ae,ve)
c
c 2013-06-12 pv: subroutine is replaced by the same subroutine in LIB/err_ellipse.for
c 


C calculates major axis lengths and their orientations
C from covariance matrix using subroutine eigen

C Barry Lienert  Oct 94

C the input covariance matrix is
C  varxx=var(1,1), varyy=var(2,2), varzz=var(3,3),... etc

C on output, major axis lengths in descending order
C of size are in ae(1), ae(3) & ae(6)

C x,y,z direction cosines of their (x,y,z) orientations are
C   ve(1),ve(2),ve(3) 
C   ve(4),ve(5),ve(6)
C   ve(7),ve(8),ve(9)

      implicit none
      real ae(*),var(3,3),ve(*)
      integer ic,j,k

C put var in ae in input format used by eigen
        ic=1
        do  j=1,3
         do  k=1,j
          ae(ic)=var(j,k)
          ic=ic+1
         end do
        end do

        call epimap_eigen(ae,ve,3,0)    
C       write(*,*)ae(1),ae(2),ae(3),ae(4),ae(5),ae(6)
        return
        end
C
      REAL FUNCTION GEO DEG( CHR_TEXT, CODE )
C
CSTART*************************************************************************
C                                                                             *
C   System          : SEISAN                                                  *
C   Supplier        : BGS/GSRG Applications Programming Unit                  *
C   Procedure Name  : GEO DEG                                                 *
C   Purpose         : To parse the input text string, containing delimiters   *
C                     and return the geographical co-ordinate in degrees.     *
C                     If no delimiters, assume dddmmss.s...                   *
C                     If 1  delimiter,  assume degrees and minutes            *
C                     If 2  delimiters, assume degrees minutes and seconds.   *
C                     Blanks must not be used as a delimiter                  *
C                                                                             *
C   Arguments-Input : CHR_TEXT   (C*)  Input text string.                     *
C           -Output : CODE       (I*4) Returned condition                     *
C                                                                             *
C              Note : Based on PGG GEO RAD (LIBUGEO @ BGS/GSRG) written by the*
C                     author)                                                 *
C                                                                             *
C   Author          : J. A. Bolton                                            *
C   Date            : 1 December 1994                                         *
C   Version         : V01                                                     *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL   SEI CLEN, SEI UPC, SEI LEFT
      INTEGER*4  SEI CLEN
C
C    System inserts....
C    ==================
C
      INCLUDE  'libsei.inc'                ! Library definitions & data defns.
      include 'epimap.inc'                 ! EPIMAP definitions.
C
C    Arguments.....
C    ==============
C
      INTEGER*4    CODE                     ! Returned condition.
      CHARACTER    CHR_TEXT *(*)            ! Input text string.
C
C    Local variables....
C    ===================
C
      INTEGER*4    IX,  IY,                 ! Very local.
     &             IEND,                    ! End of string.
     &             IDEC,                    ! Decimal place.
     &             COUNT,                   ! Separator count.
     &             SEPARATE(2),             ! Upto 2 seperators
     &             TEXT_C,                  ! Text length
     &             FORM_C,                  ! Format length.
     &             TEMP_C                   ! Working string length.
      PARAMETER    (TEMP_C = 20)            ! & value.
      PARAMETER    (TEXT_C = 80)            ! Ditto.
      PARAMETER    (FORM_C = 8)             ! Et ditto.
      CHARACTER    CHR_TEMP *(TEMP_C),      ! & string.
     &             CHR_MESS *(TEXT_C),      ! & message string
     &             CHR_FORM *(FORM_C),      ! & format string.
     &             CHRA     *1              ! Single character.
      REAL*8       DEG, MIN, SEC, VALUE     ! Very local.
      LOGICAL*4    B_CARD,                  ! Cardinal co-ordinate?.
     &             B_NEG,                   ! Negative hemisphere (S,W)?
     &             B_SIGN                   ! Signed?.
C
C    Local variables....
C    ===================
C    Preliminaries.....
C    ==================
C
      CODE     = E_OK$                      ! Initialise returned condition.
      VALUE    = 0.0 D0                     ! Initialise.
      CHR_TEMP = CHR_TEXT                   ! Copy input string.
      CALL SEI UPC( CHR_TEMP)               ! Enforce uppercase.

C    Separate out the hemisphere type (+,-) and prepare an unsigned,
C    ===============================================================
C    non-CARDINAL & left justified co-ordinate string....
C    ====================================================
C    Determine the type of co-ordinate...
C    ------------------------------------
C
      IX     = SEI CLEN( CHR_TEMP )                ! Length of string.
      CHRA   = CHR_TEMP(IX:IX)                     ! End character.
      B_CARD = CHRA .EQ. 'E' .OR.                  ! Cardinal point?
     &         CHRA .EQ. 'N' .OR.                  !
     &         CHRA .EQ. 'S' .OR.                  !
     &         CHRA .EQ. 'W'                       !
      B_SIGN = INDEX( CHR_TEMP, '+' ) .NE. 0  .OR. ! Sign in string?
     &         INDEX( CHR_TEMP, '-' ) .NE. 0       ! Ditto.
C
C    Fatal construct.....
C    --------------------
C
      IF( B_CARD .AND. B_SIGN ) THEN                  ! Signed cardinal!(L11).
      CHR_MESS =                                      !
     &'**** WARN: unable to determine the co-ordinate           ****'
      GOTO 9000                                       !
C
C    CARDINALtype geographicals...
C    -----------------------------
C
      ELSE IF( B_CARD ) THEN                ! Cardinal format.          (L11).
      CHR_TEMP = CHR_TEMP(:IX-1)            ! Left justify.
      B_NEG    = CHRA .GE. 'S'              ! Negative hemisphere?.
C
C    Signed co-ordinate.....
C    -----------------------
C
      ELSE IF( B_SIGN ) THEN                ! Signed co-ordinate.       (L11).
      IX    = INDEX( CHR_TEMP, '+' )        ! Find it.
      IY    = INDEX( CHR_TEMP, '-' )        ! Ditto.
      B_NEG = IY .GT. 0                     ! Negative hemisphere?.
C
C     Invalid double sign....
C
         IF( IX .GT. 0   .AND.              ! Invalid.                  (L21).
     &       IY .GT. 0 ) THEN               !
         CHR_MESS =                         !
     &'**** WARN: invalid sign entered                          ****'
         GOTO 9000                          !
C
C     Left justify to exclude sign.....
C
        ELSE IF( IX .GT. 0 ) THEN           ! Left justify.             (L21).
        CHR_TEMP = CHR_TEMP(IX+1:)          !
        ELSE IF( IY .GT. 0 ) THEN           ! Ditto.                    (L21).
        CHR_TEMP = CHR_TEMP(IY+1:)          !
        END IF                                                           (L21).
C
C    No indication of the hemisphere...assume positive...
C    ----------------------------------------------------
C
      ELSE                                  ! No sign.                  (L11).
      B_NEG = .FALSE.                       ! Assume +ve hemisphere (N,E).
      END IF                                !                           (L11).
C
C     Left justify the unsigned string...
C     -----------------------------------
C     Empty string....
C
1000  IF( CHR_TEMP .EQ. ' ' ) THEN                    ! Totally blank.  (L13).
      CHR_MESS =                                      !
     &'**** WARN: co-ordinate must be entered                   ****'
      GOTO 9000                                       !
C
C     Left justify...
C
      ELSE                                             ! Otherwise      (L13).
      CALL SEI LEFT( CHR_TEMP )                        ! Left justify.
      END IF                                           !                (L13).
C
C     Blanks must not occur in the string...
C
      IF(INDEX(CHR_TEMP(:SEICLEN(CHR_TEMP)),' ') .NE. 0 ) THEN ! Found.
      CHR_MESS =                                      !
     &'**** WARN: co-ordinate must not contain blank characters ****'
      GOTO 9000                                       !
      END IF                                          !
C
C     Find the decimal point and any delimiters....
C     =============================================
C
      IEND = SEI CLEN( CHR_TEMP )           ! Length of string.
      COUNT = 0                             ! Initialise separator count.
C
C     Loop Characters in string....
C     -----------------------------
C
      DO 1100 IX = 1, (IEND-1)              ! Loop characters.
      CHRA = CHR_TEMP(IX:IX)                ! Sample a character.
C
C    Found decimal point....
C    -----------------------
C
      IF( CHRA .EQ. '.' ) THEN                        ! Decimal point.   (L14).
         IF( INDEX(CHR_TEMP(IX+1:),'.') .GT. 0 ) THEN ! Another found   .(L23).
         CHR_MESS =                                   !
     &'**** WARN: invalid number of decimal points              ****'
         GOTO 9000                                    !
         END IF                                       !
C
C    Found a separator.....
C    ----------------------
C
      ELSE IF( CHRA .LT. '0'   .OR.         ! Separator found.          (L14).
     &         CHRA .GT. '9' ) THEN         ! Ditto.
      COUNT = COUNT + 1                     ! Increment count.
C
C    Invalid....
C
         IF( COUNT .GT. 2 ) THEN            ! Bad conversion.           (L24).
         CHR_MESS =                                   !
     &'**** WARN: invalid number of co-ordinate elements        ****'
         GOTO 9000                                    !
C
C    Valid....store posiion...
C
        ELSE                                ! Otherwise.                (L24).
        SEPARATE(COUNT) = IX                ! Store current position.
        END IF                              !                           (L24).
      END IF                                !                           (L14).
1100  CONTINUE                              !
C
C    Process the string...
C    =====================
C
      DEG  = 0.0 D0                         ! Initialise.
      MIN  = 0.0 D0                         ! Ditto.
      SEC  = 0.0 D0                         ! Et ditto.
      CHR_MESS = 
     &'**** WARN: unable to convert co-ordinate to degrees      ****'
C
1111  IDEC = INDEX( CHR_TEMP, '.' )         ! Decimal point.
      IEND = SEI CLEN( CHR_TEMP )           ! Length of string.
C
C    Ensure that delimiters are added where necessary to non-delimited
C    =================================================================
C    Strings to allow consistent processing..
C    ========================================
C
      IF( COUNT .EQ. 0 ) THEN               ! No delimiter            (L15).
         IF( IDEC .EQ. 0 ) THEN             ! Integer.
         IY = IEND                          ! Whole string.
         ELSE                               ! Integer portion.
         IY = IDEC - 1                      !
         END IF                             !
C
         IF( IY .EQ. 7 ) THEN                  ! Full length.
         COUNT = 2                             ! # delimiters.
         SEPARATE(1) = 4                       ! & position.
         SEPARATE(2) = 7                       ! Ditto.
         CHR_TEMP    = CHR_TEMP(:3)  // ':' // ! Insert delimiter.
     &                 CHR_TEMP(4:5) // ':' // !
     &                 CHR_TEMP(6:)            !
         GOTO 1111                             ! Try again.
C
         ELSE IF( IY .EQ. 6 ) THEN             !
         COUNT = 2                             ! # delimiters.
         SEPARATE(1) = 3                       ! & position.
         SEPARATE(2) = 6                       ! Ditto.
         CHR_TEMP    = CHR_TEMP(:2)  // ':' // ! Insert delimiter.
     &                 CHR_TEMP(3:4) // ':' // !
     &                 CHR_TEMP(5:)            !
         GOTO 1111                             ! Try again.
C
         ELSE IF( IY .EQ. 5 ) THEN             ! Degrees & minutes.
         COUNT = 2                             ! # delimiters.
         SEPARATE(1) = 2                       ! & position.
         SEPARATE(2) = 5                       ! Ditto.
         CHR_TEMP    = CHR_TEMP(:1)  // ':' // ! Insert delimiter.
     &                 CHR_TEMP(2:3) // ':' // !
     &                 CHR_TEMP(4:)            !
         GOTO 1111                             ! Try again.
C
         ELSE IF( IY .EQ. 4 ) THEN             ! Degrees & minutes.
         COUNT = 1                             ! # delimiters.
         SEPARATE(1) = 3                       ! & position.
         CHR_TEMP    = CHR_TEMP(:2)  // ':' // ! Insert delimiter.
     &                 CHR_TEMP(3:)            !
         GOTO 1111                             ! Try again.
         END IF                                !
C
C    Degrees...
C    ==========
C    Integer...
C    ----------

         IF( IDEC .EQ. 0 ) THEN                          ! Integer.     (L25).
            IF( IEND .LT. 10 ) THEN                      !
            WRITE(CHR_FORM,'(''(I'',I1,'')'')') IEND     ! Format.
            ELSE                                         !
            WRITE(CHR_FORM,'(''(I'',I2,'')'')') IEND     ! Format.
            END IF                                       !
         READ(CHR_TEMP(1:IEND),CHR_FORM(:5),ERR=9000) IX ! Get value.
         DEG = DBLE(IX)                                  ! Really!.
C
C    Real....
C    --------
C
         ELSE                                           ! Otherwise real(L25).
         IY   = IEND - IDEC                             ! Decimal places.
            IF( IEND .LT. 10 ) THEN                      !
            WRITE(CHR_FORM,'(''(F'',I1,''.'',I1,'')'')')
     &                     IEND, IY                      ! Construct format.
            ELSE IF( IY .LT. 10 ) THEN                   !
            WRITE(CHR_FORM,'(''(F'',I2,''.'',I1,'')'')')
     &                     IEND, IY                      ! Construct format.
            ELSE
            WRITE(CHR_FORM,'(''(F'',I2,''.'',I2,'')'')')
     &                     IEND, IY                      ! Construct format.
            END IF
C
         READ(CHR_TEMP(1:IEND),CHR_FORM,ERR=9000 ) DEG  ! Get degrees.
         END IF                                         !               (L25).
C
C    Degrees and minutes.....
C    ========================
C    Degrees....
C    -----------
C
      ELSE IF( COUNT .EQ. 1 ) THEN                  ! Single separator (L15).
      IX = SEPARATE(1) - 1                          ! Length of integer degrees
          IF( IX .LT. 10 ) THEN                     !
          WRITE(CHR_FORM,'(''(I'',I1,'')'')') IX    ! Format.
          ELSE                                      !
          WRITE(CHR_FORM,'(''(I'',I2,'')'')') IX    ! Format.
          END IF                                    !
      READ(CHR_TEMP(1:IX),CHR_FORM(:5),ERR=9000) IX ! Get value.
      DEG = DBLE(IX)                                ! Really!.
C
C    Minutes....
C    -----------
C    Integer....
C
        IF( IDEC .EQ. 0 ) THEN                         ! Integer.     (L26).
        IX = IEND - SEPARATE(1)                        ! Length minutes string.
          IF( IX .LT. 10 ) THEN                     !
          WRITE(CHR_FORM,'(''(I'',I1,'')'')') IX    ! Format.
          ELSE                                      !
          WRITE(CHR_FORM,'(''(I'',I2,'')'')') IX    ! Format.
          END IF                                    !
        IX = SEPARATE(1) + 1                           ! Start of string.
        IY = IEND                                      ! End of string.
        READ(CHR_TEMP(IX:IY),CHR_FORM(:5),ERR=9000) IX ! Get value.
        MIN = DBLE(IX)                                 ! Really!.
C
C    Real....
C
        ELSE                                         ! Real            (L26).
        IY = IEND - IDEC                             ! Decimal places.
        IX = IEND - SEPARATE(1)                      ! Length minutes string.
          IF( IX .LT. 10 ) THEN
          WRITE(CHR_FORM,'(''(F'',I1,''.'',I1,'')'')') IX,IY
          ELSE IF( IY .LT. 10 ) THEN
          WRITE(CHR_FORM,'(''(F'',I2,''.'',I1,'')'')') IX,IY
          ELSE
          WRITE(CHR_FORM,'(''(F'',I2,''.'',I2,'')'')') IX,IY
          END IF
C
        IX = SEPARATE(1) + 1                         ! Start of string.
        IY = IEND                                    ! End of string.
        READ(CHR_TEMP(IX:IY),CHR_FORM,ERR=9000 ) MIN ! Get minutes.
        END IF                                       !                 (L26).
C
C    Degrees, minutes & seconds.....
C    ===============================
C    Degrees....
C    -----------
C
      ELSE IF( COUNT .EQ. 2 ) THEN                  ! Single separator (L15).
      IX = SEPARATE(1) - 1                          ! Lengthinteger degrees.
         IF( IX .LT. 10 ) THEN                     !
          WRITE(CHR_FORM,'(''(I'',I1,'')'')') IX    ! Format.
          ELSE                                      !
          WRITE(CHR_FORM,'(''(I'',I2,'')'')') IX    ! Format.
          END IF                                    !
      READ(CHR_TEMP(1:IX),CHR_FORM(:5),ERR=9000) IX ! Get value.
      DEG = DBLE(IX)                                ! Really!.
C
C    Minutes....
C    -----------
C
      IY = SEPARATE(2) - 1                           ! End of string.
      IX = IY - SEPARATE(1)                          ! Length of minutes string
          IF( IX .LT. 10 ) THEN                     !
          WRITE(CHR_FORM,'(''(I'',I1,'')'')') IX    ! Format.
          ELSE                                      !
          WRITE(CHR_FORM,'(''(I'',I2,'')'')') IX    ! Format.
          END IF                                    !
      IX = SEPARATE(1) + 1                           ! Start of string.
      READ(CHR_TEMP(IX:IY),CHR_FORM(:5),ERR=9000) IX ! Get value.
      MIN = DBLE(IX)                                 ! Really!.
C
C    Seconds....
C    -----------
C    Integer....
C
        IF( IDEC .EQ. 0 ) THEN                         ! Integer      (L27).
        IX = IEND - SEPARATE(2)                        ! Length seconds string.
          IF( IX .LT. 10 ) THEN                        !
          WRITE(CHR_FORM,'(''(I'',I1,'')'')') IX       ! Format.
          ELSE                                         !
          WRITE(CHR_FORM,'(''(I'',I2,'')'')') IX       ! Format.
          END IF                                       !
        IX = SEPARATE(2) + 1                           ! Start of string.
        IY = IEND                                      ! End of string.
        READ(CHR_TEMP(IX:IY),CHR_FORM(:5),ERR=9000) IX ! Get value.
        SEC = DBLE(IX)                                 ! Really!.
C
C    Real....
C
        ELSE                                         ! Real            (L27).
        IY = IEND - IDEC                             ! Decimal places.
        IX = IEND - SEPARATE(2)                      ! Length seconds string.
          IF( IX .LT. 10 ) THEN
          WRITE(CHR_FORM,'(''(F'',I1,''.'',I1,'')'')') IX,IY
          ELSE IF( IY .LT. 10 ) THEN
          WRITE(CHR_FORM,'(''(F'',I2,''.'',I1,'')'')') IX,IY
          ELSE
          WRITE(CHR_FORM,'(''(F'',I2,''.'',I2,'')'')') IX,IY
          END IF
C
        IX = SEPARATE(2) + 1                         ! Start of string.
        IY = IEND                                    ! End of string.
        READ(CHR_TEMP(IX:IY),CHR_FORM,ERR=9000 ) SEC ! Get seconds.
        END IF                                       !                 (L27).
      END IF                                                            (L15).
C
C    Check the elements of the Co-ordinate & write out...
C    ====================================================
C    Bad conversion....
C    ------------------
C
      IF( DABS(DEG) .GT. 180.0D0   .OR.     ! Outside range.           (L16).
     &    DABS(MIN) .GE.  60.0D0   .OR.     ! Ditto mins.
     &    DABS(SEC) .GE.  60.0D0 ) THEN     ! & secs.
      GOTO 9000                             ! Error.
C
C    Aggregate....
C    -------------
C
      ELSE                                  ! Otherwise.               (L16).
      VALUE   = DABS(DEG)                   ! Accumulate positive value.
     &        + DABS(MIN) / 60.0D0          ! Ditto.
     &        + DABS(SEC) / 3600.0D0        ! Et ditto.
      END IF                                !                          (L16).
C
C    Final Check....
C    ===============
C    Bad conversion....
C    ------------------
C
      IF( VALUE .GT. 180.0D0 ) THEN                    ! Positive value(L17).
      GOTO 9000                                        ! Force error.
C
C    Sign transfer....
C    -----------------
C
      ELSE IF( B_NEG ) THEN                 ! Transfer sign            (L17).
      VALUE = - VALUE                       !
      END IF                                !                          (L17).
      GOTO 9999                             ! Back to caller.
C
C    Errors....

C    ==========
C
9000  CODE = E_CONV$                          ! Unable to convert.
      WRITE(*,*)                              !
      WRITE(*,*) CHR_MESS(:SEICLEN(CHR_MESS)) ! Write message.
      GOTO 9999                               !
C
C    Return to caller...
C    ===================
C
9999  GEO DEG = VALUE                         ! Allocate value.
      RETURN
      END



      subroutine hyperr(slon,slat,X0,Y0,a,b,ang,MINLON,MAXLON,MINLAT,
     &                  MAXLAT)
      implicit none
      real slon,slat,a,b,ang,pi,theta,tmp
      real MINLON,MINLAT,MAXLON,MAXLAT
      integer np,i
      real x2,y2,sy,sx,X0,Y0,x1,y1,xprev,yprev
      pi=3.141593
      np=50

      theta=0.0
      sy=b*sin(theta)/110.93
      sx=a*cos(theta)/(111.3*cos((sy+slat)*pi/180.))
      tmp=slon+sx*cos(ang)-sy*sin(ang)
      yprev=slat+sy*cos(ang)+sx*sin(ang)
      xprev=tmp

c plot the ellipse
      do i=1,np
       theta=float(i)*2.*pi/float(np)

c convert km to degrees
       sy=b*sin(theta)/110.93
       sx=a*cos(theta)/(111.3*cos((sy+slat)*pi/180.))

       tmp=slon+sx*cos(ang)-sy*sin(ang)
       y2=slat+sy*cos(ang)+sx*sin(ang)
       x2=tmp
       x1=xprev
       y1=yprev
       xprev=x2
       yprev=y2

c plot point, interpolating if neccesary
       call boundary(x1,y1,x2,y2,X0,Y0,MINLON,MAXLON,MINLAT,MAXLAT)
      enddo

      return
      end
c
      subroutine map_bdy( xl, yl, xu, yu, xx, yy )
CSTART**************************************************************************
C                                                                              *
C   Supplier          : BGS/GSRG Applications Programming Unit                 *
C   System            : SEISAN                                                 *
C   Name              : MAP_BDY                                                *
C   Purpose           : To find the next map-projected boundary along the line *
C                       segement.                                              *
C   Arguments  -input : xl,yl,xu,yu  (R) Lower and upper lat-longs in degrees. *
C             -Output : xx,yy        (R) Crossing in projected co-ords. If no  *
C                                        crossing then set to 1.0e6            *
C                                                                              *
C   Note              : There may be upto two crossings along a segment. This  *
C                       routine will pass back the first if it exists.         *
C                       It is ensured that in this case, the crossing is on    *
C                       the map projected boundary.                            *
C                                                                              *
C   Author            : J. A. Bolton                                           *
C   Date              : 2 June 1995                                            *
C   Version           : V01                                                    *
C                                                                              *
CEND****************************************************************************
c
      external     bdy_val                 ! Get boundary values.
     &            ,map_proj                ! Projection interface.
     &            ,sei code                ! Error encoder.
c
c    System definitions...
c    =====================
c
      include      'libsei.inc'            ! Library definitions.
      include      'epimap.inc'            ! Epimap definitions.
c
c    Arguments...
c    ============
c
      real         xl ,yl ,xu ,yu          ! Low & upper long-lats.
     &            ,xx ,yy                  ! & returned values (projected).
c
c    Local variables...
c    ==================
c
      real         xlow  ,ylow             ! Limits of segment.
     &            ,xupp  ,yupp             ! Ditto.
     &            ,xxl(4),yyl(4)           ! Lat-long array.
     &            ,xxp(4),yyp(4)           ! Projected array.
     &            ,lamx  ,lamy             ! Fractional crossings.
     &            ,lamda ,lamd             ! Ditto.
     &            ,lam_seg ,lam_bdy        ! Ditto.
     &            ,difx    ,dify           ! Test values.
     &            ,step                    ! Step size.
     &            ,low   ,upp              ! & lower & upper step values.
      logical      b_low ,b_upp            ! Lower or upper boundaries?.
     &            ,b_lhs ,b_rhs            ! Left or right boundaries?.
     &            ,b_clip                  ! Clipping internally or externally?
     &            ,b_flag                  ! Operations flag!
     &            ,b_revolve               ! Full longitude revolution?.
c
c    Initialise...
c    =============
c
      b_revolve = abs(maxlong$-minlong$) .eq. 360.0 ! Full revolution?.
c
c    Input line segment...
c    ---------------------
c
      xxl(1) = xl                          ! Array values (lat-longs).
      xxl(2) = xu                          !
      yyl(1) = yl                          !
      yyl(2) = yu                          !
c
c    Special date-line cases...
c    --------------------------
c
      if(abs(xxl(1)-xxl(2)) .ge. 180.0 ) then ! Otherwise coordinates over.
      xxl(1) = mod(xxl(1)+360.0, 360.0)       ! Adjust co-ordinate.
      xxl(2) = mod(xxl(2)+360.0, 360.0)       ! Ditto.
      end if                                  !
c
c    Get limits of segment...
c    ------------------------
c
      xlow = amin1( xxl(1), xxl(2) )       ! Limit of segment.
      ylow = amin1( yyl(1), yyl(2) )       !
      xupp = amax1( xxl(1), xxl(2) )       ! Ditto.
      yupp = amax1( yyl(1), yyl(2) )       !
c
c    Determine if there is a crossing point...
c    -----------------------------------------
c
      b_low = minlatn$ .ge. ylow .and.     ! Low boundary?
     &        minlatn$ .le. yupp           !
      b_upp = maxlatn$ .ge. ylow .and.     ! Upper boundary?.
     &        maxlatn$ .le. yupp           !
      b_lhs = minlong$ .ge. xlow .and.     ! Left boundary?
     &        minlong$ .le. xupp .and.     !
     &       (.not.b_revolve)              !
      b_rhs = maxlong$ .ge. xlow .and.     ! Right boundary?.
     &        maxlong$ .le. xupp .and.     !
     &       (.not.b_revolve)              !
      b_clip= (b_low .and. b_lhs) .or.     ! Clipping?.
     &        (b_low .and. b_rhs) .or.     !
     &        (b_upp .and. b_lhs) .or.     !
     &        (b_upp .and. b_rhs)          !
c
c    No crossing...
c    ==============
c
      if( (.not.b_low)   .and.             ! No crossing.
     &    (.not.b_upp)   .and.             !
     &    (.not.b_lhs)   .and.             !
     &    (.not.b_rhs) ) then              !
      xx = 1.0e6                           ! Absurd coordinate.
      yy = 1.0e6                           ! Ditto.
c
c    Crossing...
c    ===========
c    Longitude boundaries...
c    -----------------------
c
      else                                    ! Otherwise.
         if( b_lhs .or. b_rhs ) then          ! Treat longitudes.
         step = (maxlatn$-minlatn$)           ! Latitude steps.
     &        / float(grid_step_n$)           !
         low = ylow - amod(ylow,step)         ! Lower value.
         if(ylow .lt. 0.0) low = low - step   ! Adjust for -ves.
         upp = low                            ! Initialise.
100      upp = upp + step                     ! Provisional upper value.
            if( upp .lt. yupp ) then          ! But adjust.
            goto 100                          ! Again.
            end if                            !
c
            if( b_lhs ) then                  ! This longitude boundary.
            xxl(3) = minlong$                 ! & insert.
            xxl(4) = minlong$                 !
            else                              ! Or this.
            xxl(3) = maxlong$                 !
            xxl(4) = maxlong$                 !
            end if                            !
c
c    Loop steps & see if the contour segment crosses...
c
         difx = 1000.0                             ! Ridiculous value.
         lamx = 100.0                              ! Ditto.
         lamd = 100.0                              ! Et ditto.
c
         low  = low - step                         ! Initialise.
1000     low  = low + step                         ! Value.
            if( low  .lt. upp  ) then              !
            yyl(3) = low                           ! Latitude value.
            yyl(4) = low + step                    ! Ditto.
            call map_proj( xxl, yyl, xxp, yyp, 4 ) ! Transform the points.
c
            call bdy_val( xxp(1),  yyp(1),    ! Segment in mapspace.
     &                    xxp(2),  yyp(2),    ! Ditto.
     &                    xxp(3),  yyp(3),    ! Boundary in mapspace.
     &                    xxp(4),  yyp(4),    ! Et ditto.
     &                    lam_seg, lam_bdy )  !
c
               if( lam_seg .ge. 0.0   .and.              ! Internal crossing.
     &             lam_seg .le. 1.0 ) then               !
               difx = 0.0                                !
               lamx = lam_seg                            ! & save.
c
               else if( lam_seg .gt. 1.0           .and. ! Find min difference.
     &                  difx    .gt. lam_seg-1.0 ) then  ! Otherwise.
               lamd = lam_seg                            ! Save fraction.
               difx = lam_seg - 1.0                      !
               goto 1000                                 ! Next boundary segment
c
               else if( lam_seg .lt. 0.0        .and.    ! Find min difference.
     &                  difx    .gt. -lam_seg ) then     ! Otherwise.
               lamd = lam_seg                            ! Save fraction.
               difx = -lam_seg                           !
               goto 1000                                 ! Next boundary segment
c
               else                                      ! Otherwise.
               goto 1000                                 ! Next boundary segment
               end if                                    !
            end if                                       !
c
c     Allocate fractional crossing if one not found...Only if clipping
c     might this not apply (only internal crossings considered)...
c     
            if( b_clip ) then                         ! No action
            continue                                  !
c
            else if( difx .eq. 1000.0 ) then          ! Ridiculous value.
            write(*,*)
     &'**** WARN: "map_bdy" arguments ',xl,yl,xu,yu
            write(*,*)
     &'****       Limits..............',minlong$,minlatn$
     &                                 ,maxlong$,maxlatn$
            write(*,*)
     &'****       (longitude boundary)'
            chr_err_msg$ =                            !
     &'**** WARN: invalid map boundary calculation         ****' !
            call sei code( stop$,e_init$,0,b_flag)    ! & abort.
c
            else if( difx .ne. 0.0 ) then             !
            lamx = lamd                               !
            end if                                    !
         end if                                       !
c
c    Latitude boundaries...
c    ----------------------
c
         if( b_low .or. b_upp ) then          ! Latitude boundaries.
         step = (maxlong$-minlong$)           ! Longitude steps.
     &        / float(grid_step_n$)           !
         low = xlow - amod(xlow,step)         ! Low value.
         if(xlow .lt. 0.0) low = low - step   ! Adjust for -ves.
         upp = low                            ! Initialise.
1100     upp = upp + step                     ! Provisional value.
            if( upp .lt. xupp ) then          ! Adjust.
            goto 1100                         ! Again.
            end if                            !
c
            if( b_low ) then                  ! This latitude boundary.
            yyl(3) = minlatn$                 !
            yyl(4) = minlatn$                 !
            else                              ! Or this.
            yyl(3) = maxlatn$                 !
            yyl(4) = maxlatn$                 !
            end if                            !
c
c    Loop steps & see if the contour segment crosses...
c
         lamy = 100.0                              ! Ridiculous value.
         dify = 1000.0                             ! Ditto.
         lamd = 100.0                              ! Ditto.
c
         low  = low - step                         ! Initialise.
2000     low  = low + step                         ! Value.
            if( low  .lt. upp  ) then              !
            xxl(3) = low                           ! Latitude value.
            xxl(4) = low + step                    ! Ditto.
            call map_proj( xxl, yyl, xxp, yyp, 4 ) ! Transform the points.
c
            call bdy_val( xxp(1),  yyp(1),    ! Segment in mapspace.
     &                    xxp(2),  yyp(2),    ! Ditto.
     &                    xxp(3),  yyp(3),    ! Boundary in mapspace.
     &                    xxp(4),  yyp(4),    ! Et ditto.
     &                    lam_seg, lam_bdy )  !
c
               if( lam_seg .ge. 0.0   .and.              ! Internal crossing.
     &             lam_seg .le. 1.0 ) then               !
               dify = 0.0                                !
               lamy = lam_seg                            ! & save.
c
               else if( lam_seg .gt. 1.0           .and. ! Find min difference.
     &                  dify    .gt. lam_seg-1.0 ) then  ! Otherwise.
               lamd = lam_seg                            ! Save fraction.
               dify = lam_seg - 1.0                      !
               goto 2000                                 ! Next boundary segment
c
               else if( lam_seg .lt. 0.0        .and.    ! Find min difference.
     &                  dify    .gt. -lam_seg ) then     ! Otherwise.
               lamd = lam_seg                            ! Save fraction.
               dify = -lam_seg                           !
               goto 2000                                 ! Next boundary segment
c
               else                                      ! Otherwise.
               goto 2000                                 ! Next boundary segment
               end if                                    !
            end if                                       !
c
c     Allocate fractional crossing if one not found...Only if clipping
c     might this not apply (only internal crossings considered)...
c    
            if( b_clip ) then                         ! No action.
            continue                                  !
c
            else if( dify .eq. 1000.0 ) then          ! Ridiculous value.
            write(*,*)
     &'**** WARN: "map_bdy" arguments ',xl,yl,xu,yu
            write(*,*)
     &'****       Limits..............',minlong$,minlatn$
     &                                 ,maxlong$,maxlatn$
            write(*,*)
     &'****       (latitude boundary)'
            chr_err_msg$ =                            !
     &'**** WARN: invalid map boundary calculation         ****' !
            call sei code( stop$,e_init$,0,b_flag)    ! & abort.
c
            else if( dify .ne. 0.0 ) then             !
            lamy = lamd                               !
            end if                                    !
         end if                                       !
c
c     Allocated map-space crossing point...
c     =====================================
c     Clipping...
c     -----------
c
         if( b_clip ) then                      !
         step  = amin1( difx, dify )            ! Smallest differences
            if( step .eq. 0.0 ) then            ! Internal boundary found.
            lamda = amin1( lamx, lamy )         ! Smallest value.
            xx = xxp(1) + lamda*(xxp(2)-xxp(1)) ! x on map.
            yy = yyp(1) + lamda*(yyp(2)-yyp(1)) ! & y.
c
            else                                ! Otherwise.
            xx = 1.0e6                          ! Absurd coordinate.
            yy = 1.0e6                          ! Ditto.
            end if                              !
c
c     Other crossings...
c     ------------------
c
         else if( b_low .or. b_upp ) then     !
         xx = xxp(1) + lamy*(xxp(2)-xxp(1))   ! x on map.
         yy = yyp(1) + lamy*(yyp(2)-yyp(1))   ! & y.
c
         else if( b_lhs .or. b_rhs ) then     !
         xx = xxp(1) + lamx*(xxp(2)-xxp(1))   ! x on map.
         yy = yyp(1) + lamx*(yyp(2)-yyp(1))   ! & y.
         end if                               !
      end if                                  !
c
c    Return to Caller...
c    ===================
c
9999  return
      end
C                                                                               
      SUBROUTINE MAP_DIM                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                               
C                                                                               
C   Calculates dimensions and SCALE$ factors for map drawing.                    
C   Programmed by RAH March 1988.                                               
C                                                                               
C   ALL PARAMETERS ARE GIVEN IN COMMON BLOCK:                                   
C   MINLONG$: MINIMUM LONGITUDE                                                 
C   MAXLONG$: MAXIMUM LONGITUDE                                                 
C   GRID_SPACE_LONG$: LONGITUDE GRID STEP                                       
C   MINLATN$,MAXLATN$,GRID_SPACE_LATN$: SAME AS FOR LONGITUDE                   
C   REFLONG$,REFLATN$  : CENTER (now reference...(jab)) COORDINATES OF MAP
C   XSIZE$:  HORIZONTAL SIZE ON PAPER OF PLOT(INC)                             
C   XMIN$,XMAX$,YMIN$,YMAX$: MIN AND MAX VALUES OF LINEAR COORDINATES(KM)       
C          OF PLOT REFERENCED TO AN ARBITRARY COORDINATE SYSTEM                 
C   PROJ$:  PROJECTION NUMBER, SEE ROUTINE MAP_PROJ                             
C   SCALE$: SCALING FACTOR BETWEEN COORDINATES IN KM AND tek units              
C                                                                               
c   Western and Southern hemispheres are negative.                              
C                                                                               
C   ORIGIN OF VIRTUAL COORDINATE SYSTEM IS IN THE LOWER LEFT HAND CORNER        
C   OF THE MAP.                                                                 
C                                                                               
C   outputs:                                                                    
c      xmn - minimum value of x dimension                                       
c      xmx - maximum value of x dimension                                       
c      ymn - minimum value of y dimension                                       
c      ymx - maximum value of y dimension                                       
c      dcale - length in user units of 1 km                                    
c                                                                               
C                
C    BGS Map projection software...
C    ------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      include 'epimap.inc'                 ! EPIMAP definitions.
      external map_proc,                   ! Projection routine.
     &         sei code                    ! Error condition handler.
      integer  code                        ! Condition.
      logical  b_flag                      ! Dummy flag!!
C
C    ------- End of details -------
C
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
      real    latstep,
     &        lonstep,
     &        lonmin, lonmax,
     &        value,                                   ! Working co-ordinate.
     &        xmn, ymn, xmx, ymx,
     &        xlat, xlon, xl, yl,  x, y
      integer nstep, i
      logical last                                                      
C                                                      
C     Find the latitude-longitude centre of the map... 
C     ================================================ 
C                                                      
      call map_proc( detail$,                          ! Get centre details.
     &               0.,0.,0.,0.,0.,0.,                ! (n/a).
     &               code )                            ! Condition
      call sei code( stop$, code, 0, b_flag )          ! Error handler.
c                                                                               
c   set coordinates for min and max plot values                                 
C   FIRST, CALL MAP_PROJ TO DETERMINE THE COORDINATES (UNITS OF 111.2 KM OR ONE 
C   DEGREE) Maximum Dimensions in plot units.                                   
C                                                                               
      XMIN$=0.0                                                                 
      YMIN$=0.0                                                                 
      SCALE$=1.0                                                                
c                                                                               
      xmn=1.e6                                                                  
      ymn=1.e6                                                                  
      xmx=-1.e6                                                                 
      ymx=-1.e6                                                                 
      nstep=101                                                                  
      latstep=(MAXLATN$-MINLATN$)/nstep
      last=.false.
c
c    Make sure longitude changes are in the correct sense...
c
      if( minlong$ .gt. maxlong$ ) then       ! Crosses date line.
      lonmin = minlong$ - 360.0               !
      lonmax = maxlong$                       !
c
      else                                    ! Ok.
      lonmin = minlong$                       !
      lonmax = maxlong$                       !
      end if                                  !
c
c    Initialise starting longitude for increment....
c
      value = grid_base_long$                 ! Initialise.
1111  if( value .gt. lonmin ) then            ! Inside plot.
      value = value - grid_space_long$        ! Adjust down.
      goto 1111                               ! Try again.
      end if                                  !
c
c    Adjust to be on the plot...
c
      xlon = amax1( value, lonmin )           ! Initial longitude.
C
      DO WHILE (XLON.LE.MAXLONG$)                                               
         DO 1 I=1,nstep
         if( xlon .lt. -180.0 ) then
         xlon = xlon + 360.0
         end if
c
            XL=XLON                                                             
            YL=MINLATN$ + LATSTEP*(I-1)                                         
            CALL MAP_PROJ(XL,YL,X,Y,1)                                          
            if(x.gt.xmx) xmx=x                                                  
            if(x.lt.xmn) xmn=x                                                  
            if(y.gt.ymx) ymx=y                                                  
            if(y.lt.ymn) ymn=y                                                  
  1         CONTINUE     
C
            value = value + GRID_SPACE_LONG$           ! Increment working coord
            if(value.gt.MAXLONG$.and.(.not.last)) then !
               value = MAXLONG$                        !
               last=.true.
            endif
            xlon = value                               !
      ENDDO                                                                     
c                                                                               
c     Repeat on latitude ranges...
c     ============================
c
      lonstep = (lonmax - lonmin)/nstep                ! Step size.
      last = .false.                                   ! Last latitude line?.
      value = grid_base_latn$                          ! Initialise.
2222  if( value .gt. minlatn$ ) then                   ! Inside plot.
      value = value - grid_space_latn$                 ! Adjust down.
      goto 2222                                        ! Try again.
      end if                                           !
c
      xlat = amax1( value, minlatn$ )                  ! Initial longitude.
      DO WHILE (xlat .le. maxlatn$)
         DO 2 I=1,nstep
            YL=xlat
            XL=lonmin + LONSTEP*(I-1)
c
               if( xl .lt. -180.0 ) then
               xl = xl + 360.0
               end if
c
            CALL MAP_PROJ(XL,YL,X,Y,1)
            if(x.gt.xmx) xmx=x
            if(x.lt.xmn) xmn=x
            if(y.gt.ymx) ymx=y
            if(y.lt.ymn) ymn=y
  2         CONTINUE
C
            value = value + GRID_SPACE_LATN$           ! Increment working coord
            if(value.gt.MAXLATN$.and.(.not.last)) then !
               value = MAXLATN$                        !
               last=.true.
            endif
            xlat = value                               !
      ENDDO
c
c     Now set the scaling...
c     ======================
c
      XMIN$=xmn                                                                 
      XMAX$=xmx                                                                 
      YMIN$=ymn                                                                 
      YMAX$=ymx                                                                 
      SCALE$ = XSIZE$/(XMAX$-XMIN$)                                               
      value=abs(SCALE$*(YMAX$-YMIN$))                                         
c                                                                               
c   check if map fits screen, if not change SCALE in x direction                
c                                                                               
      if( value .gt. YSIZE$ ) then
      SCALE$ = SCALE$ * YSIZE$/value
      endif                                                                     
c
c   Centralise the map...
c   =====================
c
      value = scale$ * (xmax$-xmin$)
         if( value .lt. xsize$ ) then
         REFX0$ = REF0X$ + 0.5*(xsize$-value)
         else
         REFX0$ = REF0X$ 
         end if
c
      value = scale$ * (ymax$-ymin$)
         if( value .lt. ysize$ ) then
         REFY0$ = REF0Y$ + 0.5*(ysize$-value)
         else
         REFY0$ = REF0Y$
         end if
c
c   Return to Caller...
c   ===================
c                                                                               
      return                                                                    
      END                                                                       
C                                                                               
      SUBROUTINE MAP_GRID                                                       
C   PLOTS THE MAP GRID IN VARIOUS PROJECTIONS.                                  
C   ALL PARAMETERS ARE GIVEN IN COMMON BLOCK:                                   
C   MINLONG$: MINIMUM LONGITUDE                                                 
C   MAXLONG$: MAXIMUM LONGITUDE                                                 
C   GRID_SPACE_LONG$: LONGITUDE GRID STEP                                       
C   MINLATN$,MAXLATN$,GRID_SPACE_LATN$: SAME AS FOR LONGITUDE                   
C   REFLONG$,REFLATN$  : CENTRE (now reference..(JAB)) COORDINATES OF MAP       
C   XSIZE$: HORIZONTAL SIZE ON PAPER OF PLOT(INC)
C   YSIZE$: Vertical size of plot on paper (inches)
C   XMIN$,XMAX$,YMIN$,YMAX$: MIN AND MAX VALUES OF LINEAR COORDINATES(KM)       
C          OF PLOT REFERENCED TO AN ARBITRARY COORDINATE SYSTEM                 
C   PROJ$:   PROJECTION NUMBER, SEE ROUTINE MAP_PROJ                            
C   SCALE$:  SCALING FACTOR BETWEEN COORDINATES IN KM tec units
c   REFX0$,REFY0$:  plot start coordinates (tek units) of map lower left corner 
C                                                                               
C   INPUT PARAMETERS: MINLONG$, MAXLONG$, GRID_SPACE_LONG$, MINLATN$, MAXLATN$,
C                     GRID_SPACE_LATN$, XSIZE$
C                                                                               
C   ORIGIN OF VIRTUAL COORDINATE SYSTEM IS IN THE LOWER LEFT HAND CORNER        
C   OF THE MAP.                                                                 
C
CJAB(BGS)Jan95   : Draw border of map also
CJAB(BGS)May95   : Choose label decimal places according to grid size and
CJAB(BGS)May95     determine small scale plots where scale bars required.
CJAB(BGS)Jun95   : Parametrized drawing grid stepping.
C                                                                               
      include 'epimap.inc'                 ! EPIMAP definitions.
      external   sei clen,                 ! String length function.
     &           sei code,                 ! Error handler.
     &           map_proc                  ! Projection primitive routine.
      integer    sei clen                  ! & function.
c
      integer    array_n                   !JAB(BGS)Jun95. Array size.
      parameter (array_n = grid_step_n$+1) ! & value.
c
      real ix,iy, xlat, xlon, lamda
      integer i, jy, decimal
      character*80 text
      logical last                                                         
      REAL LATSTEP,LONSTEP                                                      
      real delx, dely
      real value,                         ! Working co-ordinate.
     &     xd, scale,                     ! Very local.
     &     distance                       ! Working distance.
      real X(array_n), Y(array_n),        ! Working co-ordinates.
     &     XL(array_n),YL(array_n)        ! Ditto.   
      character    chr_latform *(6),      ! Latitude formatter.
     &             chr_lngform *(6)       ! & longitude.
      logical      b_integer              ! Integral km scale?
C
C   Initialise...
C   =============
C
      decimal = 1                                     ! No decimal places.
      do i = 1, 3                                     ! Loop these.
      if( grid_space_latn$ .lt. 10.0**float(1-i))then !
      decimal = decimal + 1                           !
      end if                                          !
      end do                                          !
c
      write(chr_latform,'(''(F'',i1,''.'',i1,'')'')') ! latitude formatter.
     &                  (4+decimal), decimal
c
      decimal = 1                                     ! No decimal places.
      do i = 1, 3                                     ! Loop these.
      if( grid_space_long$ .lt. 10.0**float(1-i))then !
      decimal = decimal + 1                           !
      end if                                          !
      end do                                          !
c
      write(chr_lngform,'(''(F'',i1,''.'',i1,'')'')') ! longitude formatter.
     &                  (5+decimal), decimal
C
C   Draw border of the map....
C   ==========================
C
       delx = xsize$ - 2.0*lat_txt_dispx$ ! Frame width.
       dely = ysize$ - 2.0*lon_txt_dispy$ ! & height.
c
       call xset_color(color_title)
       call xmovabs( refx$, refy$ )
       call xdrwabs( refx$, refy$+dely )
       call xdrwabs( refx$+delx, refy$+dely )
       call xdrwabs( refx$+delx, refy$ )
       call xdrwabs( refx$, refy$ )
C
C   Is this small-scale?....
C   ========================
C
      xl(1) = minlong$
      yl(1) = minlatn$
      xl(2) = maxlong$
      yl(2) = minlatn$
      xl(3) = maxlong$
      yl(3) = maxlatn$
      xl(4) = minlong$
      yl(4) = maxlatn$
      CALL MAP_PROJ(XL,YL,X,Y,4)          ! Get co-ordinates for 4 corners.
c
c    Get minimum side length...
c    --------------------------
c
      distance = amin1( abs(x(2)-x(1))                   ! Minimum side in km.
     &                 ,abs(x(3)-x(4))                   !
     &                 ,abs(y(4)-y(1))                   !
     &                 ,abs(y(3)-y(2)) )                 !
     &         * eq_deg_to_km$                           !
     &         /scale$                                   !
      b_small_scale$ = distance .le. small_scale_length$ ! & small scale?.
C                                                                               
C   WHEN DRAWING LATITUDES AND LONGITUDES, DIVIDE IN SMALL PIECES,              
C   TRANSFORM EACH POINT AND PLOT.                                              
C                                                                               
      LATSTEP=(MAXLATN$-MINLATN$)/float(grid_step_n$)
      LONSTEP=(MAXLONG$-MINLONG$)/float(grid_step_n$)
C                                                                               
C   DRAW LONGITUDES                                                             
C                 
      last=.false.                                                              
      value=GRID_BASE_LONG$                ! Initialise working co-ordinate.
      xlon = amax1(value,minlong$)         ! Take in boundary
c
      DO WHILE (XLON.LE.MAXLONG$)                                               
         DO 1 I=1,array_n
            XL(I)=XLON                                                          
            YL(I)=MINLATN$ + LATSTEP*(I-1)                                      
  1      CONTINUE                                                               
         CALL MAP_PROJ(XL,YL,X,Y,array_n)
c        do i=1,array_n
c          write(88,*) xl(i),yl(i),x(i),y(i)
c        enddo
         ix=x(1)+REFX0$                                                         
         iy=y(1)+REFY0$
         call xset_color(color_label_grid)     ! color of axis numbers
c
c    Ensure displayed longitudes in numerical range -180 < long <= + 180...
c
         lamda = xlon                              ! Working copy.
         if( lamda .le. -180.0 ) then              ! 
         lamda = lamda + 360.0                     !
         else if( lamda .gt. 180.0 ) then          !
         lamda = lamda - 360.0                     !
         end if                                    !
c
         write(text,chr_lngform) lamda
         call xchars(text,seiclen(text),
     &               ix+lon_txt_dispx$,
     &               iy+lon_txt_dispy$)              
         call xset_color(color_map_grid)            ! color of grid                      
         CALL PLOT_LINES(X,Y,REFX0$,REFY0$,array_n)
c
         value = value + GRID_SPACE_LONG$           ! Increment working co-ord.
         if(value.gt.MAXLONG$.and.(.not.last)) then !
            value=MAXLONG$                          !
            last=.true.
         endif
      xlon = value                                  !
      ENDDO                                                                     
C                                                                               
C   PLOT LATITUDES                                                              
C
      last=.false.                                                              
      value = GRID_BASE_LATN$                   ! Initialise.
      xlat = amax1(value,minlatn$)              ! Take in boundary
c
      DO WHILE (XLAT.LE.MAXLATN$)                                               
         DO 2 I=1,array_n
            XL(I)=MINLONG$+LONSTEP*(I-1)                                        
            YL(I)=XLAT                                                          
 2       CONTINUE                                                               
         CALL MAP_PROJ(XL,YL,X,Y,array_n)
         ix=x(1)+REFX0$          
         iy=y(1)+REFY0$                                                         
         call xset_color(color_label_grid)      ! color of axis numbers                                                         
         write(text,chr_latform) xlat                                              
         call xchars(text,seiclen(text),
     &               ix+lat_txt_dispx$,
     &               iy+lat_txt_dispy$)                                     
         call xset_color(color_map_grid)            ! color of grid                      
         CALL PLOT_LINES(X,Y,REFX0$,REFY0$,array_n)
c
         value = value + GRID_SPACE_LATN$           ! Increment working co-ord.
         if(value.gt.MAXLATN$.and.(.not.last)) then !
            value = MAXLATN$                        !
            last=.true.
         endif
      xlat  = value                                 ! Ditto.
      ENDDO                                                                     
c
c   If small-scale, plot a scale bar and label it...
c   ================================================
c
      value = 0.25*xsize$*eq_deg_to_km$/scale$      ! Assumed scale bar distance
      if( b_small_scale$ .and. value .ge. 0.1 ) then! Plot scale bar.
         if( value .ge. 5.0 ) then                  ! 
         value = 5.0                                !
         scale = 1.0                                ! 1 km.
         else if( value .ge. 1.0 ) then             !
         value = int( value )                       ! Truncate.
         scale = 1.0                                !
         else if( value .ge. 0.5 ) then             !
         value = 0.5                                !
         scale = 0.1                                ! In 100m .
         else                                       !
         value = int(value*10.0)/10.0               !
         scale = 0.1                                !
         end if                                     !
c
c    Plot scale axis...
c
      x(1) = border_x$                              ! Scale start.
      y(1) = small_scale_y$                         !
      x(2) = x(1)                                   ! & end.
     &     + value*scale$/eq_deg_to_km$             !
      y(2) = y(1)                                   !
      call xset_color( color_frame )                ! Set colour.
      call xmovabs( x(1), y(1) )                    ! Start of scale.
      call xdrwabs( x(2), y(2) )                    ! End of scale.
c 
c    Plot ticks and 1st & last values...
c
      jy = (value/scale) + 0.5                      ! # intervals.
      b_integer = scale .gt. 0.9                    ! Integer?.
      scale = scale*scale$/eq_deg_to_km$            ! Pixel distance.
c
      do i = 1, jy+1                                ! Loop ticks.
      xd = x(1) + float(i-1)*scale                  ! Locations.
      call xmovabs( xd, y(1) )                      ! Move to base.
c
         if( i .eq. 1 ) then                        ! 1st
         call xdrwabs( xd, y(1)+10 )                ! Long tick
            if( b_integer ) then                    !
            call xchars('0',1,xd-5.,y(1)+15.0)      !
            else                                    !
            call xchars('0.0',3,xd-15,y(1)+15.0)    !
            end if                                  !
c
         else if( i .eq. jy+1 ) then                ! Last.
         call xdrwabs( xd, y(1)+10 )                ! Long tick
            if( b_integer ) then                    !
            write(text(1:1),'(i1)') int(value+0.5)  !
            text(2:) = ' km'                        !
            call xchars(text,seiclen(text),xd-5.,y(1)+15.0) !
c
            else                                    !
            write(text(1:3),'(f3.1)') value         !
            text(4:) = ' km'                        !
            call xchars(text,seiclen(text),xd-15.,y(1)+15.0) !
            end if                                  ! 
c
         else                                       ! Otherwise.
         call xdrwabs( xd, y(1)+5 )                 ! Short tick.
         end if                                     !
      end do                                        !
      end if                                        !
C                                                                               
C   MAP IS LIMITED TO WHOLE INTERVALS                                           
C   OF GRID_SPACE_LATN$ AND GRID_SPACE_LONG$                                   
C                                                                               
      call xset_color(color_def)                    ! default color
      RETURN                                                                    
      END                                                                       
c
      subroutine map_proc( func, plat,  plon, vy, ux,
     &                           polat, polon,
     &                           code )
CSTART*************************************************************************
C                                                                             *
C                    BGS/GSRG Applications Programming Unit                   *
C                    ======================================                   *
C                                                                             *
C    System           : SEISAN                                                *
C    Module Name      : map_proc                                              *
C    Routine          : To perform forward & reverse map projections on the   *
C                       the sphere and also find map centre and pole details, *
C                       depending on function.                                *
C    Note             : The routines are based on spherical radius and central*
C                       SCALE$ both equal to unity.                            *
C                     : Based on "Map Projections - A working Manual"         *
C                                - Snyder. USGS Prof.Paper 1395               *
C                                                                             *
C    Arguments -Input : func     (i*4) Function to perform as found in        *
C                                      map_proc.inc. Values are:              *
C                                      projection # + one of the following    *
C                                      major keys...                          *
C                                                                             *
C                                      ENCODE$ - encode (x,y) from lat-long   *
C                                      DECODE$ - decode (x,y) to   lat-long   *
C                                      DETAIL$ - find user requirements.      *
C                                                                             *
C                     : plat     (r*4) Point latitude  in degrees             *
C                     : plon     (r*4) Point longitude in degrees.            *
C                     : polat    (r*4) Reference latitude  in degrees         *
C                     : polon    (r*4) Reference longitude in degrees         *
C              -Output: code     (i*4) Returned condition of the operation    *
C                     : plat     (r*4) Point latitude  in degrees             *
C                     : plon     (r*4) Point longitude in degrees.            *
C                     : ux, vy   (r*4) Map co-ordinates                       *
C                                                                             *
C    Author           : J. A. Bolton                                          *
C    Date             : 16 October 1994                                       *
C    Version          : V01                                                   *
C                                                                             *
CEND***************************************************************************
C
      external   geo deg                   ! Get geographical in degrees.
      real       geo deg                   ! & function.
      external   sei cmd pro,              ! Command file processor.
     &           sei clen                  ! String length.
      integer    sei clen                  ! & function.
C
C    System inclusions...
C    ====================
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      include 'epimap.inc'                 ! EPIMAP definitions.
C
C    Arguments...
C    ============
C
      integer     code,                      ! Returned condition.
     &            func                       ! Function to perform.
      real        plat,  plon,               ! Generally point lat-longs.
     &            polat, polon,              ! Reference lat-longs.
     &            ux,    vy                  ! Output co-ordinates.
C
C    Local variables...
C    ==================
C
      integer     function,                  ! Operation type.
     &            proj,                      ! Projection number.
     &            count                      ! Iteration count.
C
      real        lat,  lon,                 ! Working latitude & longitude.
     &            lat0, lon0,                ! & reference latitude & longitude.
     &            dlon,                      ! Longitude displacement.
     &            theta,                     ! Parametric angle.
     &            value,                     ! Temporary value.
     &            clatn,clong,               ! Local central lat-long.
     &            azim                       ! Azimuth of central line.
C
      real        A, B, C, D, E, F, G, R,    ! processing parameters.
     &            x, y,                      ! Map location.
     &            cosd, sind, cosl, sinl,    ! Trigs on displacement & lat.
     &            cos0, sin0                 ! & on lat0.
C     
      integer     ix, iy,                    ! Very local.
     &            text_c                     ! Text length.
      parameter  (text_c = 70)               ! & value.
      character   chr_text *(text_c),        ! & text string.
     &            chr_temp *(text_c)         ! & alternative.
      logical     b_dateline,                ! Passes over dateline?.
     &            b_revol                    ! Full longitude revolution?.
C
C    =============
C    Initialise...
C    =============
C
      code     = e_ok$                       ! Local condition.
      function = (func/128)*128              ! Operational function.
      proj     = mod( func, 128 )            ! Projection number.
C
      lat0 = polat * deg_to_rad$             ! Reference latitude in radians.
      lon0 = polon * deg_to_rad$             ! & longitude
C
      lat0 = amin1( max_lat$, lat0 )         ! Truncate at pole.
      lat0 = amax1(-max_lat$, lat0 )         ! Or south pole.
C
C    ==========================
C    Forward Transformations...
C    ==========================
C    Preliminaries...
C    ================
C
      IF( FUNCTION .EQ. ENCODE$) THEN        ! Get (x,y) from (lat-long).
      lat  = plat * deg_to_rad$              ! Point latitude in radians.
      lon  = plon * deg_to_rad$              ! Ditto.
      dlon = lon - lon0                      ! Longitude displacement.
C
      lat  = amin1( max_lat$, lat )          ! Truncate at pole.
      lat  = amax1(-max_lat$, lat )          ! Or south pole.
C
C    Polar Stereographic...
C    ======================
C
         IF( PROJ .EQ. POSTEREO$ ) THEN      ! Polar.
            if( lat0 .ge. 0.0 ) then         ! Northern.
            lat   = amax1( 0.0, lat )        ! Truncate at equator.
            theta = 0.5*(0.5*pi$ - lat)      ! Parametric latitude.
            x     = 2.0*tan(theta)*sin(dlon) ! Map location eastings.
            y     =-2.0*tan(theta)*cos(dlon) ! & northings.
C
            else                             ! Otherwise southern.
            lat   = amin1( 0.0, lat )        ! Truncate at equator.
            theta = 0.5*(0.5*pi$ + lat)      ! Parametric latitude.
            x     = 2.0*tan(theta)*sin(dlon) ! Map location eastings.
            y     = 2.0*tan(theta)*cos(dlon) ! & northings.
            end if                           !
C
C    Orthographic azimuthal...
C    =========================
C
         ELSE IF( PROJ .EQ. ORTHOGRAPH$ ) THEN ! Othographic azimuthal.
20       cosd = cos(dlon)                      ! Store.
         sind = sin(dlon)                      !
         cosl = cos(lat)                       !
         sinl = sin(lat)                       !
         cos0 = cos(lat0)                      !
         sin0 = sin(lat0)                      !
C
         A = cos0*sinl - sin0*cosl*cosd      ! Parameters.
         B = sin0*sinl + cos0*cosl*cosd      ! Ditto.
         C =                  cosl*sind      ! Et ditto.
C
C    If B < 0, the location does not plot. However, adjust latitude or
C    -----------------------------------------------------------------
C    longitude displacement so that B is valid...
C    --------------------------------------------
C
           if( B .lt. 0.0 ) then                  ! Invalid
           value = tan(lat0) * tan(lat)           ! Interim.
              if( lat0 .eq. 0.0   .or.            ! Fix displacement.
     &            lat  .eq. 0.0 ) then            !
              dlon = sign(0.5*pi$-tolerance$,dlon)! On limit.
              else if( abs(value) .le. 1.0 ) then ! Valid displacement.
              dlon = acos( -value )               !
              else                                ! Valid latitude.
              lat = atan(-cosd/tan(lat0))         !
              end if                              !
           goto 20                                ! Try again.
C
C    Calculate the co-ordinates...
C    -----------------------------
C
           else                              ! Valid.
           x = C                             ! map co-ordinate eastings.
           y = A                             ! & northings.
           end if                            !
C
C    Mercator...
C    ===========
C
         ELSE IF( PROJ .EQ. MERCATOR$ ) THEN ! Mercator.
            if( dlon .lt. -pi$ ) then        ! Adjust to +-180 degrees.
            dlon = dlon + 2.0*pi$            !
            else if( dlon .gt. pi$ ) then    ! Ditto.
            dlon = dlon - 2.0*pi$            !
            end if                           !
C
         theta = 0.5*(0.5*pi$ + lat)         ! Parametric latitude.
         x     = dlon                        ! Map co-ordinate eastings.
         y     = alog(tan(theta))            ! & northings.
C
C    Lambert azimtuthal equal area...
C    ================================
C
         ELSE IF( PROJ .EQ. LAMBEQAREA$ ) THEN ! Lambert azimuthal.
         cosl = cos(lat)                       !
         sinl = sin(lat)                       !
         cos0 = cos(lat0)                      !
         sin0 = sin(lat0)                      !
C
40       cosd = cos(dlon)                    ! Store.
         sind = sin(dlon)                    !
C
         A = cos0*sinl - sin0*cosl*cosd      ! Parameters.
         B = sin0*sinl + cos0*cosl*cosd      ! Ditto.
         C =                  cosl*sind      ! Et ditto.
C
C    If B = -1, Point exists at lat=-lat0, dlon=+-180 degrees and can't
C    ------------------------------------------------------------------
C    be plotted, make a small adjustment to longitude displacement..
C    ---------------------------------------------------------------
C
            if( B .eq. -1.0 ) then           ! Invalid.
               if( dlon .le. pi$ ) then      ! Small adjustment.
               dlon = dlon + tolerance$      !
               else                          ! Ditto.
               dlon = dlon - tolerance$      !
               end if                        !
            goto 40                          ! Try again.
C
C    Calculate the co-ordinates...
C    -----------------------------
C
            else                             ! Otherwise valid.
            value = sqrt(2.0/(1.0+B))        ! Parameter.
            x     = value*C                  ! Map co-ordinate eastings.
            y     = value*A                  ! & northings.
            end if                           !
C
C    Gnomonic...
C    ===========
C
         ELSE IF( PROJ .EQ. GNOMONIC$ ) THEN ! Gnomonic.
         cosl = cos(lat)                     !
         sinl = sin(lat)                     !
         cos0 = cos(lat0)                    !
         sin0 = sin(lat0)                    !
C
50       cosd = cos(dlon)                    ! Store.
         sind = sin(dlon)                    !
C
         A = cos0*sinl - sin0*cosl*cosd      ! Parameters.
         B = sin0*sinl + cos0*cosl*cosd      ! Ditto.
         C =                  cosl*sind      ! Et ditto.
C
C    If B <= 0.0 then invalid..adjust longitude displacement to
C    ----------------------------------------------------------
C    to make valid...
C    ----------------
C
            if( B .le. 0.0 ) then            ! Invalid.
               if( lat .eq. 0.0 ) then       ! On limit.
               dlon = sign( max_lat$, dlon ) !
               else if( dlon .le. 0.0 ) then ! Small adjustment.
               dlon = dlon + tolerance$      !
               else                          ! Ditto.
               dlon = dlon - tolerance$      !
               end if                        !
            goto 50                          ! Try again.
C
C    Calculate the co-ordinates...
C    -----------------------------
C
            else                             ! Otherwise valid.
            x = C/B                          ! Map co-ordinate easting.
            y = A/B                          ! & northings.
            end if                           !
C
C    Azimuthal equidistant...
C    ========================
C
         ELSE IF( PROJ .EQ. AZIMEQDIST$ ) THEN ! Azimuthal.
         cosl = cos(lat)                       !
         sinl = sin(lat)                       !
         cos0 = cos(lat0)                      !
         sin0 = sin(lat0)                      !
C
60       cosd = cos(dlon)                    ! Store.
         sind = sin(dlon)                    !
C
         A = cos0*sinl - sin0*cosl*cosd      ! Parameters.
         B = sin0*sinl + cos0*cosl*cosd      ! Ditto.
         C =                  cosl*sind      ! Et ditto.
C
C    If B = +- 1.0 then invalid..adjust longitude displacement to
C    ------------------------------------------------------------
C    to make valid...
C    ----------------
C
            if( B .eq. 1.0 ) then            ! Invalid.
            x = 0.0                          ! Centre point.
            y = 0.0                          ! Ditto.
C
            else if( B .eq. -1.0 ) then      ! Invalid.
               if( dlon .le. -pi$ ) then     ! Small adjustment.
               dlon = dlon + tolerance$      !
               else                          ! Ditto.
               dlon = dlon - tolerance$      !
               end if                        !
            goto 60                          ! Try again.
C
C    Calculate the co-ordinates...
C    -----------------------------
C
            else                             ! Otherwise valid.
            value = acos(B)                  ! Parameter.
            value = value / sin(value)       !
C
            x = value*C                      ! Map co-ordinate easting.
            y = value*A                      ! & northings.
            end if                           !
C
C    Stereographic (general oblique)...
C    ==================================
C
         ELSE IF( PROJ .EQ. STEREOGRAPH$ ) THEN ! Stereographic.
         cosl = cos(lat)                        !
         sinl = sin(lat)                        !
         cos0 = cos(lat0)                       !
         sin0 = sin(lat0)                       !
C
70       cosd = cos(dlon)                    ! Store.
         sind = sin(dlon)                    !
C
         A = cos0*sinl - sin0*cosl*cosd      ! Parameters.
         B = sin0*sinl + cos0*cosl*cosd      ! Ditto.
         C =                  cosl*sind      ! Et ditto.
C
C    If B = - 1.0 then invalid..adjust longitude displacement to
C    -----------------------------------------------------------
C    to make valid...
C    ----------------
C
            if( B .eq. -1.0 ) then           ! Invalid.
               if( dlon .le. -pi$ ) then     ! Small adjustment.
               dlon = dlon + tolerance$      !
               else                          ! Ditto.
               dlon = dlon - tolerance$      !
               end if                        !
            goto 70                          ! Try again.
C
C    Calculate the co-ordinates...
C    -----------------------------
C
            else                             ! Otherwise valid.
            value = 2.0/(1.0+B)              ! Parameter.
            x = value*C                      ! Map co-ordinate easting.
            y = value*A                      ! & northings.
            end if                           !
C
C    Cylindrical equidistant...
C    ==========================
C
         ELSE IF( PROJ .EQ. EQDISTCYLIN$ ) THEN ! Cylindrical equidistant.
            if( dlon .lt. -pi$ ) then           ! Adjust to +-180 degrees.
            dlon = dlon + 2.0*pi$               !
            else if( dlon .gt. pi$ ) then       ! Ditto.
            dlon = dlon - 2.0*pi$               !
            end if                              !
C
         x = cos(lat0)*dlon                     ! Map co-ordinate eastings.
         y = lat                                ! & northings.
C
C    Oblique Mercator...
C    ===================
C    
C    Note that if central line azimuth = 0, then Transverse mercator,
C         otherwise if azimuth = 90 degrees, then mercator projection
C         when centre of map is on the equator...
C
         ELSE IF( PROJ .EQ. OBLIQUEMERC$ ) THEN ! Oblique mercator.
         cosl = cos(lat)                        !
         sinl = sin(lat)                        !
         cos0 = cos(latp$)                      ! Pole.
         sin0 = sin(latp$)                      !
C
90       cosd = cos(dlon)                     ! Store.
         sind = sin(dlon)                     !
         F    = tan(lat)*cos0  + sin0*sind    ! Parameter.
         G    = sin0*sinl - cos0*cosl*sind    ! Ditto.
C
C    x location...
C    -------------
C
            if( abs(dlon) .eq. 0.5*pi$ ) then ! Edge of map.
            x = sign( 0.5*pi$, F )            ! 
            else                              ! Otherwise.
            x = atan( F/cosd )                !
            end if                            !
C
C    y location...
C    -------------
C
            if( G .ge. 1.0 ) then               ! Invalid.
            dlon = dlon + sign(tolerance$,-dlon)!
            goto 90                             ! Try again.
C
            else if( G .le. -1.0 ) then         ! Ditto.
            dlon = dlon + sign(tolerance$, dlon)!
            goto 90                             ! Try again.
C
            else                                ! Otherwise.
            y = 0.5*alog((1.0+G)/(1.0-G))       ! Map northings.
            end if                              !
C
C    Mollweide equal area...
C    =======================
C    Use Newton-Raphson method to find parametric latitude...
C    --------------------------------------------------------
C
         ELSE IF( PROJ .EQ. MOLLWEIDE$ ) THEN! Mollweide.
         theta = abs(lat)                    ! Initial solution.
         count = 0                           ! No iterations yet!.
C
100      count = count + 1                   ! Increment.
         value = -(theta + sin(theta)        ! Solution deviation.
     &           - pi$*sin(abs(lat)))        !
     &         /  (1.0   + cos(theta))       !
C
C     Test the deviation against tolerance...
C
            if( count      .lt. max_count$  .and. ! Valid iteration.
     &          abs(value) .gt. max_diff$ ) then  ! But outside tolerance.
            theta = theta + value                 ! Next approximation.
            goto 100                              ! Try again.
C
C     Calculate co-ordinates...
C     -------------------------
C
            else                                  ! Otherwise.
            theta = 0.5*sign(theta,lat)           ! Parametric angle.
            x     = 2.0*sqrt(2.0)*dlon*cos(theta) ! Map co-ordinate eastings
     &            / pi$                           !
            y     = sqrt(2.0)*sin(theta)          ! & northings.
            end if                                !
C
C    Sinusoidal (sometimes known as Sanson's)...
C    ===========================================
C
         ELSE IF( PROJ .EQ. SINUSOIDAL$ ) THEN ! Sinusoidal.
            if( dlon .lt. -pi$ ) then          ! Adjust to +-180 degrees.
            dlon = dlon + 2.0*pi$              !
            else if( dlon .gt. pi$ ) then      ! Ditto.
            dlon = dlon - 2.0*pi$              !
            end if                             !
C
         x = cos(lat)*dlon                   ! Map co-ordinate eastings.
         y = lat                             ! & northings.
C
C    Invalid projection...
C    =====================
C
         ELSE                                ! Otherwise.
         code = e_init$                      ! Bad initialisation.
         goto 9999                           ! Return to caller.	
         END IF                              !
C
C    Rotate to final axes...
C    =======================
C
         IF( ROTATE$ .NE. 0.0 ) THEN          ! Rotate back.
         ux = x*cos(rotate$) + y*sin(rotate$) ! Final co-ordinate.
         vy = y*cos(rotate$) - x*sin(rotate$) ! Ditto.
C
         ELSE                                 ! Otherwise.
         ux = x                               ! Copy.
         vy = y                               ! Ditto.
         END IF                               !
C
C    ==========================
C    Reverse transformations...
C    ==========================
C    Rotate...
C    =========
C
      ELSE IF( FUNCTION .EQ. DECODE$ ) THEN      ! Get (lat-long from (x,y).
         IF( ROTATE$ .NE. 0.0 ) THEN             ! Rotate back.
         x = ux*cos(-rotate$) + vy*sin(-rotate$) ! Raw co-ordinate.
         y = vy*cos(-rotate$) - ux*sin(-rotate$) ! Ditto.
C
         ELSE                                    ! Otherwise.
         x = ux                                  ! Copy.
         y = vy                                  ! Ditto.
         END IF                                  !
C
C    See if Centre of the projection...
C    ==================================
C
      R = sqrt( x*x + y*y )                   ! Map radius.
         IF( R .EQ. 0.0 ) THEN                ! At centre.
         plat = polat                         !
         plon = polon                         !
C
C    Polar Stereographic...
C    ======================
C
         ELSE IF( PROJ .EQ. POSTEREO$ ) THEN           ! Polar.
         F = 2.0*atan(0.5*R)                           ! Parametric angle.
         D = cos(F)*sin(lat0) + (y/R)*sin(F)*cos(lat0) !
C 
         plat = rad_to_deg$ * asin( D )                ! Latitude degrees.
            if( lat0 .ge. 0.0 ) then                   ! North polar.
            plon = polon + rad_to_deg$*atan(x/(-y))    !
            else                                       ! South polar.
            plon = polon + rad_to_deg$*atan(x/y)       !
            end if                                     !
C
C    Orthographic azimuthal...
C    =========================
C
         ELSE IF( PROJ .EQ. ORTHOGRAPH$ ) THEN         ! Othographic azimuthal.
         F = asin(R)                                   ! Parametric angle.
         D = cos(F)*sin(lat0) + (y/R)*sin(F)*cos(lat0) !
         E = (x/R) *sin(F)                             !
     &     /(cos(F)*cos(lat0) - (y/R)*sin(F)*sin(lat0))!
C
         plat =         rad_to_deg$ * asin(D)          ! Latitude degrees.
         plon = polon + rad_to_deg$ * atan(E)          ! & longitude.
C
C    Mercator...
C    ===========
C
         ELSE IF( PROJ .EQ. MERCATOR$ ) THEN           ! Mercator.
         plat = 90.0  - rad_to_deg$*2.0*atan(exp(-y))  ! latitude degrees.
         plon = polon + rad_to_deg$ * x                ! Longitude degrees,
C
C    Lambert azimtuthal equal area...
C    ================================
C
         ELSE IF( PROJ .EQ. LAMBEQAREA$ ) THEN         ! Lambert azimuthal.
         F = 2.0*asin(0.5*R)                           ! Parametric angle.
         D = cos(F)*sin(lat0) + (y/R)*sin(F)*cos(lat0) !
         E = (x/R) *sin(F)                             !
     &     /(cos(F)*cos(lat0) - (y/R)*sin(F)*sin(lat0))!
C
         plat =         rad_to_deg$ * asin(D)          ! Latitude degrees.
         plon = polon + rad_to_deg$ * atan(E)          ! & longitude.
C
C    Gnomonic...
C    ===========
C
         ELSE IF( PROJ .EQ. GNOMONIC$ ) THEN           ! Gnomonic.
         F = atan(R)                                   ! Parametric angle.
         D = cos(F)*sin(lat0) + (y/R)*sin(F)*cos(lat0) !
         E = (x/R) *sin(F)                             !
     &     /(cos(F)*cos(lat0) - (y/R)*sin(F)*sin(lat0))!
C
         plat =         rad_to_deg$ * asin(D)          ! Latitude degrees.
         plon = polon + rad_to_deg$ * atan(E)          ! & longitude.
C
C    Azimuthal equidistant...
C    ========================
C
         ELSE IF( PROJ .EQ. AZIMEQDIST$ ) THEN         ! Azimuthal.
         F = R                                         ! Parametric angle.
         D = cos(F)*sin(lat0) + (y/R)*sin(F)*cos(lat0) !
         E = (x/R) *sin(F)                             !
     &     /(cos(F)*cos(lat0) - (y/R)*sin(F)*sin(lat0))!
C
         plat =         rad_to_deg$ * asin(D)          ! Latitude degrees.
         plon = polon + rad_to_deg$ * atan(E)          ! & longitude.
C
C    Stereographic (general oblique)...
C    ==================================
C
         ELSE IF( PROJ .EQ. STEREOGRAPH$ ) THEN        ! Stereographic.
         F = 2.0*atan(0.5*R)                           ! Parametric angle.
         D = cos(F)*sin(lat0) + (y/R)*sin(F)*cos(lat0) !
         E = (x/R) *sin(F)                             !
     &     /(cos(F)*cos(lat0) - (y/R)*sin(F)*sin(lat0))!
C
         plat =         rad_to_deg$ * asin(D)          ! Latitude degrees.
         plon = polon + rad_to_deg$ * atan(E)          ! & longitude.
C
C    Cylindrical equidistant...
C    ==========================
C
         ELSE IF( PROJ .EQ. EQDISTCYLIN$ ) THEN        ! Cylindrical.
         plat =         rad_to_deg$ * y                ! Latitude degrees.
         plon = polon + rad_to_deg$ * (x/cos(lat0))    ! & longitude.
C
C    Oblique Mercator...
C    ===================
C    
C    Note that if central line azimuth = 0, then Transverse mercator,
C         otherwise if azimuth = 90 degrees, then mercator projection
C         when centre of map is on the equator...
C
         ELSE IF( PROJ .EQ. OBLIQUEMERC$ ) THEN        ! Oblique mercator.
         plat = rad_to_deg$                            ! Latitude degrees.
     &        * asin(                                  !
     &              ( sin(latp$)*sinh(y)               !
     &              + cos(latp$)*sin (x))/cosh(y)      !
     &              )                                  !
         plon = polon                                  ! & longitude.
     &        + rad_to_deg$                            !
     &        * atan(                                  !
     &              ( sin(latp$)*sin (x)               !
     &              - cos(latp$)*sinh(y))/cos(x)       !
     &              )                                  !
C
C    Mollweide equal area...
C    =======================
C
         ELSE IF( PROJ .EQ. MOLLWEIDE$ ) THEN          ! Mollweide.
         theta = asin(y/sqrt(2.0))                     ! Parametric angle.
         plat  = rad_to_deg$                           ! Latitude degrees.
     &         * asin((2.0*theta+sin(2.0*theta))/pi$)  !
C
            if( abs(plat) .eq. 90.0 ) then             ! At pole.
            plon = polon                               ! & longitude.
            else                                       ! Otherwise.
            plon = polon                               ! & longitude.
     &           + rad_to_deg$                         !
     &           *(pi$*x / (2.0*sqrt(2.0)*cos(theta))) !          
            end if                                     !
C
C    Sinusoidal (sometimes known as Sanson's)...
C    ===========================================
C
         ELSE IF( PROJ .EQ. SINUSOIDAL$ ) THEN         ! Sinusoidal.
         plat = rad_to_deg$ * y                        ! Latitude degrees.
            if( abs(y) .eq. 0.5*pi$ ) then             ! & longitude at pole.
            plon = polon                               !
            else                                       !
            plon = polon + rad_to_deg$ * (x/cos(y))    ! & longitude.
            end if                                     !
C
C    Invalid projection...
C    =====================
C
         ELSE                                ! Otherwise.
         code = e_init$                      ! Bad initialisation.
         goto 9999                           ! Return to caller.        
         END IF                              !
C
C    =================================================
C    Find central lat-long & other necessary detail...
C    =================================================
C    Only if details have not yet been found...
C    ==========================================
C
      ELSE IF( FUNCTION .EQ. DETAIL$ ) THEN  ! Get details.
         IF( B_DETAIL$ ) THEN                ! Already found.
         continue                            ! No action.
C
C    ------------------------
C    Prompt for Projection...
C    ------------------------
C    Menu...
C    -------
C
         ELSE                                ! Otherwise.
1000     WRITE(*,*)                          !
         write(*,*)'Projection menu'         !
         write(*,*)'==============='         !
         write(*,*)                          !
         chr_text = ' '                      ! Empty text string.
         do 1100 ix = 1, proj_last$          ! Loop projections.
         write(chr_text(3:4),'(i2)') ix      ! Insert projection #
         chr_text(5:) = ': ' // chr_proj$(ix)! & text.
         write(*,*)chr_text                  ! INform user.
1100     continue                            !
         WRITE(*,*)
C 
C    & prompt...
C    -----------
C
1150     call sei cmd pro( 'Please enter projection number   : ',
     &                     chr_text)
         PROJ$ = 0                                       ! Initialise.
         call sei get values( 1, chr_text, code )        ! Parse text.
         proj$ = nint(array$(1))                         ! & retrieve.
C
C    Validate...
C    -----------
C
1200        if( proj$ .lt. 1            .or. ! Outside?.
     &          proj$ .gt. proj_last$ ) then !
            write(*,*)                       !
            write(*,*)                       !
     &'**** ERROR: invalid projection ****'  ! Message.
            goto 1000                        ! Try again.
            end if                           !
C
C    Determine the extent of the map in geographical space...
C    ========================================================
C    Latitude range...
C    =================
C
2000     write(*,*)
         call sei cmd pro( 'Enter latitude  range of the map :',
     &                     chr_text )
         iy = seiclen( chr_text )                      ! Length of text.
         ix = index(chr_text(:iy),' ')                 ! Find internal blank.
         call sei left(chr_text(ix+1:))                ! Left justify remainder.
         iy = seiclen( chr_text )                      ! Length of text.
         iy = index(chr_text(ix+1:iy),' ' )            ! Embedded blanks?.
c
c    Too many/too few items...
c    ------------------------
c
            if( iy .gt. 0 ) then                       ! Too many items.
            write(*,*)                                 !
            write(*,*)
     &'**** WARN: too many values have been given, try again....  ****'
            goto 2000                                  !
c
            else if( ix .eq. 0 ) then                  ! Only one item given.
            write(*,*)                                 !
            write(*,*)                                 ! Message.
     &'**** WARN: each latitude must be separated by one or more  ****'
            write(*,*)                                 ! Message.
     &'****       blanks, try again...                            ****'
            goto 2000                                  !
c
c    decode each sub-string...
c    -------------------------
c    minimum latitude...
c
            else                                       ! Otherwise.
            minlatn$ = geo deg( chr_text(:ix), code )  !
c
c    cant't decode...
c
               if( code .ne. e_ok$  ) then                ! Invalid.
               goto 2000                                  ! Try again.
               else if( abs(minlatn$) .gt. 90.0d0 ) then  ! Ditto.
               write(*,*)                                 !
               write(*,*)                                 ! Message.
     &'**** WARN: invalid minimum latitude, try again...          ****'
               goto 2000                                  !
               end if                                     !
c
c    maximum latitude...
c
            maxlatn$ = geo deg( chr_text(ix:), code )  !
c
c    cant't decode...
c
               if( code .ne. e_ok$  ) then                ! Invalid.
               goto 2000                                  ! Try again.
               else if( abs(maxlatn$) .gt. 90.0d0 ) then  ! Ditto.
               write(*,*)                                 !
               write(*,*)                                 ! Message.
     &'**** WARN: invalid maximum latitude, try again...          ****'
               goto 2000                                  !
               end if                                     !
            end if                                        !
C
C    Validate...
C    -----------
C
            if( minlatn$ .ge. maxlatn$ ) then          ! Invalid.
            write(*,*)                                 !
            write(*,*)                                 ! Message.
     &'**** WARN: southern edge must be to south of northern edge ****'
            goto 2000                                  !
            end if                                     !
C
C    Longitude range...
C    ==================
C
3000     write(*,*)
         call sei cmd pro( 'Enter longitude range of the map :',
     &                     chr_text )
         iy = seiclen( chr_text )                      ! Length of text.
         ix = index(chr_text(:iy),' ')                 ! Find internal blank.
         call sei left(chr_text(ix+1:))                ! Left justify remainder.
         iy = seiclen( chr_text )                      ! Length of text.
         iy = index(chr_text(ix+1:iy),' ' )            ! Embedded blanks?.
c
c    Too many/too few items...
c    ------------------------
c 
            if( iy .gt. 0 ) then                       ! Too many items.
            write(*,*)                                 !
            write(*,*)
     &'**** WARN: too many values have been given, try again....  ****'
            goto 3000                                  !
c
            else if( ix .eq. 0 ) then                  ! Only one item given.
            write(*,*)                                 !
            write(*,*)                                 ! Message.
     &'**** WARN: each longitude must be separated by one or more ****'
            write(*,*)                                 ! Message.
     &'****       blanks, try again...                            ****'
            goto 3000                                  !
c
c    decode each sub-string...
c    -------------------------
c
            else                                       ! Otherwise.
            minlong$ = geo deg( chr_text(:ix), code )  !
            if( code .ne. e_ok$ ) goto 3000            ! Invalid.
c
            maxlong$ = geo deg( chr_text(ix:), code )  !
            if( code .ne. e_ok$ ) goto 3000            ! Invalid.
            end if                                     !
C
C    Validate...
C    -----------
C
            b_dateline = minlong$ .gt. maxlong$  .and. ! Over dateline?.
     &                   maxlong$ .lt. 0.0             !
            b_revol    = minlong$ .eq. maxlong$        ! Full revolution?.
c
            if( b_dateline )maxlong$ = maxlong$ + 360.0! Adjust.
c
            if( minlong$ .gt. maxlong$ ) then          ! Invalid.
            write(*,*)                                 !
            write(*,*)                                 ! Message.
     &'**** WARN: western edge must be to west of eastern edge  ****'
            write(*,*)                                 !
     &'           or the same longitude (through one revolution)****'
            goto 3000                                  !
            end if                                     !
C
C    Reference Latitude-Longitude...
C    ===============================
C    Determine the centre of the range and prompt to accept these
C    as the reference co-ordinates..
C    ------------------------------------------------------------
C
         CLATN = MINLATN$                               ! Central latitude.
     &         + 0.5*(MAXLATN$ - MINLATN$)              !
            IF( PROJ$ .EQ. POSTEREO$ ) THEN             ! Polar stereographic.
               IF( CLATN .GE. 0.0 ) THEN                ! Northern hemisphere.
               CLATN = 90.0                             ! Pole.
               ELSE                                     ! Otherwise southern.
               CLATN = -90.0                            ! Pole.
               END IF                                   !
            END IF                                      !
C
         CLONG = MINLONG$                               ! Central longitude.
     &         + 0.5*(MAXLONG$ - MINLONG$)              !
            IF( CLONG .GT. 180.D0 ) THEN                ! Adjust.
            CLONG = CLONG - 360.0D0                     ! To hemisphere.
            END IF                                      !
C
C    Re-set min and max longitudes for full revolution...
C    ----------------------------------------------------
C
            IF( B_REVOL ) THEN                          ! Flagged.
            MAXLONG$ = MAXLONG$ + 180.0                 !
            MINLONG$ = MINLONG$ - 180.0                 !
            END IF                                      !
C
C    Inform user of centre of geographical space...
C    ----------------------------------------------
C
         WRITE(CHR_TEMP(:14),'(''('',F5.1,'','',F6.1,'')'')')
     &         CLATN, CLONG
         CHR_TEXT = 'Centre latitude-longitude is ' //
     &              CHR_TEMP(:SEICLEN(CHR_TEMP))           //
     &              ' degrees.'
         WRITE(*,*)
         WRITE(*,*) CHR_TEXT
C
C    Prompt to accept...
C    -------------------
C
         chr_text = 
     &'Press <return> to accept this as the reference point of ' //
     &'the projection:'
         call sei cmd pro( chr_text,                         ! Prompt.
     &                     chr_text )                        ! For a reply.
C
C    Accepted...
C    -----------
C
            IF( chr_text(1:1) .eq. 'y'   .or.         ! Accepted.
     &          chr_text(1:1) .eq. 'Y'   .or.         !
     &          chr_text      .eq. ' ' ) then         ! 
            REFLATN$ = CLATN                          ! Install
            REFLONG$ = CLONG                          ! Ditto.
C
C    User must supply the co-ordinates...
C    ------------------------------------
C
            else                                       ! Otherwise.
            write(*,*)
4000        call sei cmd pro(
     &      'Enter reference latitude-longtude:', 
     &                       chr_text )
            iy = seiclen( chr_text )                   ! Length of text.
            ix = index(chr_text(:iy),' ')              ! Find internal blank.
            call sei left(chr_text(ix+1:))             ! Left justify remainder.
            iy = seiclen( chr_text )                   ! Length of text.
            iy = index(chr_text(ix+1:iy),' ' )         ! Embedded blanks?.
c
c    Too many/too few items...
c    ------------------------
c 
               if( iy .gt. 0 ) then                    ! Too many items.
               write(*,*)                              !
               write(*,*)
     &'**** WARN: too many values have been given, try again....  ****'
               goto 4000                               !
c
               else if( ix .eq. 0 ) then               ! Only one item given.
               write(*,*)                              !
               write(*,*)                              ! Message.
     &'**** WARN: each co-ordinate must be separated by one or    ****'
               write(*,*)                                 ! Message.
     &'****       more blanks, try again...                       ****'
               goto 4000                                  !
c
c    decode each sub-string...
c
               else                                         ! Otherwise.
               reflatn$ = geo deg( chr_text(:ix), code )    !
                  if( code .ne. e_ok$  ) then               ! Invalid.
                  goto 4000                                 ! Try again.
                  else if( abs(reflatn$) .gt. 90.0d0 ) then ! Ditto.
                  write(*,*)                                !
                  write(*,*)                                ! Message.
     &'**** WARN: invalid latitude, try again...                  ****'
                  goto 4000                                 !
                  end if                                    !
c
c     Adjust for polar projections...
c
                  IF( PROJ$ .EQ. POSTEREO$ ) THEN         ! Polar stereographic.
                     IF( reflatn$ .GE. 0.0 ) THEN         ! Northern hemisphere.
                     reflatn$ = 90.0                      ! Pole.
                     ELSE                                 ! Otherwise southern.
                     reflatn$ = -90.0                     ! Pole.
                     END IF                               !
                  END IF                                  !
C
c      Get reference longitude and adjust for traversing International
c      dateline....
c
               reflong$ = geo deg( chr_text(ix:), code )  !
               if( code .ne. e_ok$ ) goto 4000            !
               if( b_dateline .and. reflong$ .lt. 0.0)    ! Adjust for dateline.
     &         reflong$ = reflong$ + 360.0                !
               end if                                     !
            end if                                        !
C
C    Grid Spacing...
C    ===============
C    Latitude...
C    ===========
C
5000     write(*,*)
         write(*,*)
     &' Enter latitude of ANY grid line and also the grid spacing'
         call sei cmd pro( ':',
     &                     chr_text )
         iy = seiclen( chr_text )                      ! Length of text.
         ix = index(chr_text(:iy),' ')                 ! Find internal blank.
         call sei left(chr_text(ix+1:))                ! Left justify remainder.
         iy = seiclen( chr_text )                      ! Length of text.
         iy = index(chr_text(ix+1:iy),' ' )            ! Embedded blanks?.
c
c    Too many/too few items...
c    ------------------------
c 
            if( iy .gt. 0 ) then                       ! Too many items.
            write(*,*)                                 !
            write(*,*)
     &'**** WARN: too many values have been given, try again....  ****'
            goto 5000                                  !
c
            else if( ix .eq. 0 ) then                  ! Only one item given.
            write(*,*)                                 !
            write(*,*)                                 ! Message.
     &'**** WARN: each value must be separated by one or more     ****'
            write(*,*)                                 ! Message.
     &'****       blanks, try again...                            ****'
            goto 5000                                  !
c
c    decode each sub-string...
c
            else                                         ! Otherwise.
            grid_base_latn$ = geo deg(chr_text(:ix),code)!
               if( code .ne. e_ok$  ) then               ! Invalid.
               goto 5000                                 ! Try again.
               else if( abs(grid_base_latn$) .gt. 90.0d0 ) then ! Ditto.
               write(*,*)                                !
               write(*,*)                                ! Message.
     &'**** WARN: invalid latitude, try again...                  ****'
               goto 5000                                 !
               end if                                    !
C
            grid_space_latn$ = geo deg(chr_text(ix:),code)!
               if( code .ne. e_ok$  ) then               ! Invalid.
               goto 5000                                 ! Try again.
               else if( abs(grid_space_latn$) .gt. 90.0d0 ) then ! Ditto.
               write(*,*)                                !
               write(*,*)                                ! Message.
     &'**** WARN: invalid spacing, try again...                   ****'
               goto 5000                                 !
               else if( grid_space_latn$ .le. 0.0d0 ) then ! Ditto.
               write(*,*)                                !
               write(*,*)                                ! Message.
     &'**** WARN: spacing must not be > 0.0, try again...         ****'
               goto 5000                                 !
               end if                                    !
            end if                                       !
C
C    Adjust to base latitude...
C
5001        if( grid_base_latn$ .lt. minlatn$ ) then     ! Needs adjustment.
            grid_base_latn$ = grid_base_latn$            !
     &                      + grid_space_latn$           !
            goto 5001                                    ! More?
            end if                                       !
C
5002        if( grid_base_latn$ .gt. minlatn$ ) then     ! Needs adjustment.
            grid_base_latn$ = grid_base_latn$            !
     &                      - grid_space_latn$           !
            goto 5002                                    ! More?
            end if                                       !
C
C    Longitude..
C    ===========
C
5100     write(*,*)
     &' Enter longitude of ANY grid line and also the grid spacing'
         call sei cmd pro( ':',
     &                     chr_text )
         iy = seiclen( chr_text )                      ! Length of text.
         ix = index(chr_text(:iy),' ')                 ! Find internal blank.
         call sei left(chr_text(ix+1:))                ! Left justify remainder.
         iy = seiclen( chr_text )                      ! Length of text.
         iy = index(chr_text(ix+1:iy),' ' )            ! Embedded blanks?.
c
c    Too many/too few items...
c    ------------------------
c 
            if( iy .gt. 0 ) then                       ! Too many items.
            write(*,*)                                 !
            write(*,*)
     &'**** WARN: too many values have been given, try again....  ****'
            goto 5100                                  !
c
            else if( ix .eq. 0 ) then                  ! Only one item given.
            write(*,*)                                 !
            write(*,*)                                 ! Message.
     &'**** WARN: each value must be separated by one or more     ****'
            write(*,*)                                 ! Message.
     &'****       blanks, try again...                            ****'
            goto 5100                                  !
c
c    decode each sub-string...
c
            else                                         ! Otherwise.
            grid_base_long$ = geo deg(chr_text(:ix),code)!
            if( code .ne. e_ok$  ) goto 5100             ! Invalid.
C
            grid_space_long$ = geo deg(chr_text(ix:),code) !
               if( code .ne. e_ok$  ) then               ! Invalid.
               goto 5000                                 ! Try again.
               else if( grid_space_long$ .le. 0.0d0 ) then ! Ditto.
               write(*,*)                                !
               write(*,*)                                ! Message.
     &'**** WARN: spacing must not be > 0.0, try again...         ****'
               goto 5000                                 !
               end if                                    !
            end if                                       !
C
C    Adjust to base longitude...
C
            if( b_dateline )                             ! Adjust for dateline.
     &      grid_base_long$ = grid_base_long$ + 180.0    !
C
5101        if( grid_base_long$ .lt. minlong$ ) then     ! Needs adjustment.
            grid_base_long$ = grid_base_long$            !
     &                      + grid_space_long$           !
            goto 5101                                    ! More?
            end if                                       !
C
5102        if( grid_base_long$ .gt. minlong$ ) then     ! Needs adjustment.
            grid_base_long$ = grid_base_long$            !
     &                      - grid_space_long$           !
            goto 5102                                    ! More?
            end if                                       !
C
C    Re-set Centre of polar stereographic projection...
C    --------------------------------------------------
C
            if( proj$ .eq. POSTEREO$ ) then     ! Polar.
               if( reflatn$ .ge. 0.0 ) then     ! North.
               reflatn$ = 90.0                  !
               else                             ! South.
               reflatn$ = -90.0                 !
               end if                           !
C
C    Obtain the pole of an Oblique Mercator projection...
C    ----------------------------------------------------
C    Set up..
C
            else if( proj$ .eq. OBLIQUEMERC$ ) then           ! Oblique mercator
            CLATN = reflatn$ * deg_to_rad$                    ! Central location
            CLONG = reflong$ * deg_to_rad$                    ! Ditto.
C
6000        call sei cmd pro(                                 ! Process.
     & 'Enter azimuth of the central line through centre: ',  ! Prompt.
     &                       chr_text )                       ! For reply.
            call sei get values( 1, chr_text, code )          ! Parse text.
            if( code .ne. e_ok$ ) goto 6000                   ! & again.
            azim = array$(1) * deg_to_rad$                    ! In radians.
C
C    Pole details...
C    ...longitude...
C
               if( CLATN .eq. 0.0   .or.         ! Avoid singularity.
     &             azim  .eq. 0.0 ) then         !
               lonp$ = CLONG - 0.5*pi$           ! Longitude of pole.
C
               else                              ! Otherwise calculate.
               lonp$ = CLONG                     ! Longitude of pole.
     &               + atan(                     !
     &                 -cos(azim)/(-sin(CLATN)*sin(azim))
     &                     )                     !
               end if                            !
C
C    ...latitude....
C
      latp$ = asin(cos(CLATN)*sin(azim))        ! Projection pole latitude.
C
C    Map rotation...
C
            rotate$ = rotate$ - 0.5*pi$         ! Final rotation.
     &              + azim                      !
            end if                              !
C
C    -----------
C    finished...
C    -----------
C
         b_detail$ = .true.                  ! Now found.
         write(*,*)                          !
            if( b_f_debug$ ) then            ! Debug information.
            write(dbgunit$,*)'Map-proj detail...'
            write(dbgunit$,*)'minlat,maxlatn:',
     &                        minlatn$,maxlatn$
            write(dbgunit$,*)'minlon,maxlon :',
     &                        minlong$,maxlong$
            write(dbgunit$,*)'grid base latn:',
     &                        grid_base_latn$
            write(dbgunit$,*)'grid spce latn:',
     &                        grid_space_latn$
            write(dbgunit$,*)'grid base long:', 
     &                        grid_base_long$
            write(dbgunit$,*)'grid spce long:', 
     &                        grid_space_long$
            write(dbgunit$,*)'...end of detail'
            write(dbgunit$,*)' '
            end if                           !
         END IF                              !
C
C    ===================
C    Invalid function...
C    ===================
C
      ELSE                                   ! Otherwise.
      code = e_init$                         ! Set bad initialisation.
      END IF                                 !
C
C    ===================
C    Return to caller...
C    ===================
C
9999  return  
      end
C                                                                               
C#####################################################################          
C                                                                               
                                                                                
      SUBROUTINE MAP_PROJ(LON,LAT,X,Y,N)                                        
C                                                                               
C  PERFORMS THE TRANSFORMATION FROM LATITUDE-LONGITUDE TO X-Y                   
C   X-Y ARE IN VERTUAL UNITS WITH ORIGO IN THE LOWER LEFT HAND CORNER OF MAP.   
C   INPUT:  LAT,LON,N: N VALUES TO BE TRANSFORMED                               
C   OUTPUT: X,Y: N TRANSFORMED VALUES                                           
C                                                                               
C   FOR REST OF PARAMETERS, SEE EXPLANATION OF COMMON BLOCK IN                  
C   ROUTINE MAP_GRID                                                            
C                                                                               
C    BGS Map projection software...
C    ------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      include 'epimap.inc'                 ! EPIMAP definitions.
      external map_proc,                   ! Projection routine.
     &         sei code                    ! Error condition handler.
      integer  code                        ! Condition.
      logical  b_flag                      ! Dummy flag!!
C
C    ------- End of details -------
C
      REAL vy, ux                                                              
      integer   i, n
      real X(*),Y(*),LAT(*),LON(*)                                         
C
      DO 51 I=1,N                                                               
      call map_proc( encode$+proj$,            ! Encode projection.
     &               lat(i),   lon(i),         ! From lat-longs.
     &               vy,       ux,             ! To map co-ordinates.
     &               REFLATN$, REFLONG$,       ! Reference lat-longs.
     &               code )                    ! Returned condition.
      call sei code( stop$, code, 0, b_flag )  ! Process outcome.
C
         X(I)=(ux*rad_to_deg$-XMIN$)*SCALE$    ! Convert to screen co-ordinates.
         Y(I)=(vy*rad_to_deg$-YMIN$)*SCALE$    !
 51   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
c
      subroutine new_diagram( function, b_start, b_new_page, unit )
cstart*************************************************************************
c                                                                             *
c                         BGS/GSRG Programming Unit                           *
c                         =========================                           *
c                                                                             *
c    System           : SEISAN                                                *
c    Module Name      : NEW_DIAGRAM                                           *
c    Purpose          : To set up a new diagram on the page. It will nullify  *
c                       any scaling and translations made by the library      *
c                       primitives for PostScript (seiplot.for) before setting*
c                       up its own environment.                               *
c    Arguments -input : function   (I) Function top perform. Values are:      *
c                                      map$    - map diagram                  *
c                                      depth$  - depth profile diagram        *
c                                      as found in epimap.inc                 *
c                     : b_start    (L) Start a new plot?.                     *
c                     : b_new_page (L) Start a new page?.                     *
c                     : unit       (I) PostScript unit to write to            *
c             -output : b_start    (L) Start a new plot?.                     *
c                     : b_new_page (L) Start a new page?.                     *
c    Author           : J. A. Bolton                                          *
c    Date             : 30 January 1995                                       *
c    Version          : V01                                                   *
c                                                                             *
cend***************************************************************************
c
      external      open_display,          ! Routine to setup display & ps.
     &              xnewpag                ! Ditto subset for new page.
      external      sei code,              ! Error encoder.
     &              sei clen               ! Length of text string.
      integer       sei clen               ! ... function,
c
c   System includes...
c   ==================
c
      include 'libsei.inc'                 ! Library definitions & data defns.
      include 'epimap.inc'                 ! EPIMAP definitions.
c
c   Arguments...
c   ============
c
      integer     function,                ! Function to perform.
     &            unit                     ! PostScript output unit.
      logical     b_new_page,              ! Setup new page?.
     &            b_start                  ! Starting new plot?.
c
c   Local variables...
c   ==================
c
      integer     code                     ! Local condition code.
      real        map_transx, map_transy,  ! Translations for map in x & y.
     &            map_intvaly,             ! Interval translation in y.
     &            dep_transx, dep_transy,  ! Ditto for depth profile.
     &            dep_intvaly              ! & interval.
      parameter  (map_transx =   0.0)      ! & values.
      parameter  (map_transy = 720.0)      !
      parameter  (map_intvaly=   0.0)      !
      parameter  (dep_transx = 100.0)      !
      parameter  (dep_transy = 400.0)      !
      parameter  (dep_intvaly=  70.0)      !
c
      real        transx,  transy,         ! Translations in x & y.
     &            intvaly,                 ! & interval.
     &            scalex,  scaley          ! Scaling in x and y.
      character   chr_text *(80)           ! Text string.
      logical     b_flag                   ! Processing flag?.
c
      integer     last_function            ! Last function type.
      common/newdiag/ last_function        ! & saved.
c
c   Initialise...
c   =============
c
      code = e_ok$                         ! Local condition.
      if( b_start ) b_new_page = .true.    ! Force a new page.
c
c   Set up translations...
c   ----------------------
c   Map plot...
c
      if( function .eq. map$ ) then        ! Map diagram.
      transx  = map_transx                 ! Translation in x.
      transy  = map_transy                 ! & y.
      intvaly = map_intvaly                ! & diagram interval.
c
c   Depth profile plot...
c
      else if( function .eq. depth$ ) then ! Depth profile.
      transx  = dep_transx                 ! Translation in x.
      transy  = dep_transy                 ! & y.
      intvaly = dep_intvaly                ! & diagram interval.
c
c   Invalid function...
c
      else                                 ! Otherwise invalid.
      chr_err_msg$ =                             ! Set up message.
     &'**** ERROR: invalid function in "new_diagram" ****'
      call sei code( stop$, e_init$, 0, b_flag ) ! & force abort.
      end if                                     !
c
c   New Page...
c   ===========
c
      b_new_page = b_new_page                    .or.  ! For a page?.
     &            (function      .eq. map$       .and. ! This is a map.
     &             last_function .eq. depth$)          ! & last was depth plot.
c
      if( b_new_page ) then                ! New page.
      last_function = function             ! Copy function.
c
c   Starting up a new plot...
c   -------------------------
c
         if( b_start ) then                    ! New plot.
         b_start = .false.                     ! Recognised!.
         rewind( unit, iostat=code )           ! Rewind epimap.eps.
         call sei code(fort$,code,unit,b_flag) ! Process outcome.
c
c   Starting new page...
c   --------------------
c   Primitive to setup..
c
         else                                           ! Otherwise.
         call xnewpag                                   !
c
c   Nullify scaling & translations made in primitve...
c
         call get_env_psscale(PLOT_SCALEX$,PLOT_SCALEY$)
         scalex = 1.0/plot_scalex$                      ! Scale back
         scaley = 1.0/plot_scaley$                      ! Ditto.
c
         write(chr_text(:12),'(2F6.3)') scalex, scaley  ! & set-up.
         chr_text(13:) = ' scale'                       !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
c
         write(chr_text(1:14),'(2f7.1)') -plot_transx$, ! Back to correct zero.
     &                                   -plot_transy$  !
         chr_text = chr_text(1:14) // ' translate'
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
         end if                                         !
c
c   Open up the diagram...
c
         if( function .eq. map$ ) then                  ! Map situation.
         call get_env_psscale(PLOT_SCALEX$,PLOT_SCALEY$)
         scalex = 1.0                                   ! No scale in x.
         scaley = plot_scalex$                          ! Scale y as in x.
c
         else                                           ! Depth situation.
         scalex = 1.0/plot_scalex$                      ! Scale back.
         scaley = 1.0/plot_scaley$                      ! Ditto.
         end if                                         !
c
      call open_display                                 !
      write(chr_text(:12),'(2F6.3)') scalex, scaley     ! Set up page
      chr_text(13:) = ' scale'                          !
      write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
      call sei code( fort$, code, unit, b_flag  )       ! Process outcome.
c
c   If depth profile plot, return axes back to zero before proceeding...
c
         if( function .eq. depth$ ) then                ! Depth profile.
         write(chr_text(1:14),'(2f7.1)') -plot_transx$, ! Back to correct zero.
     &                                   -plot_transy$  !
         chr_text = chr_text(1:14) // ' translate'      !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
         end if                                         !
c
c    Move axes to new plotting point...
c
      write(chr_text(1:14),'(2f7.1)') transx,           ! Translate.
     &                               (transy+intvaly)   ! With interval.
      chr_text = chr_text(1:14) // ' translate'         !
      write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
      call sei code( fort$, code, unit, b_flag  )       ! Process outcome.
c
c   Re-set page directive...
c
      b_new_page = .not.b_new_page                      ! Not a new page?.
c
c   Other diagram on page...
c   ========================
c   Open up the diagram...
c   ----------------------
c   Nullify previous scaling & translation...
c   -----------------------------------------
c   Map plots...
c
      else                                              ! 2nd diagram.
         if( function .eq. map$ ) then                  ! Map situation.
         last_function = function                       ! Copy function.
         call get_env_psscale(PLOT_SCALEX$,PLOT_SCALEY$)
         scalex = 1.0/plot_scalex$                      ! Both set to scale x.
         scaley = 1.0/plot_scalex$                      !
         write(chr_text(:12),'(2F6.3)') scalex, scaley  ! Set up page
         chr_text(13:) = ' scale'                       !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
c
         write(chr_text(1:14),'(2f7.1)') -plot_transx$, ! Back to correct zero.
     &                                   -plot_transy$  !
         chr_text = chr_text(1:14) // ' translate'      !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
c
         scalex = 1.0                                   ! No scale in x.
         scaley = plot_scalex$                          ! Scale y as in x.
c
c   Depth plot on map plot page..
c
         else if( last_function .eq. map$ ) then        !
         last_function = function                       ! Copy.
         write(chr_text(1:14),'(2f7.1)') -map_transx,   ! Translate back.
     &                      -(map_transy+map_intvaly)   ! With interval.
         chr_text = chr_text(1:14) // ' translate'      !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
c
         scalex = 1.0/plot_scalex$                      ! Both set to scale x.
         scaley = 1.0/plot_scalex$                      !
         write(chr_text(:12),'(2F6.3)') scalex, scaley  ! Set up page
         chr_text(13:) = ' scale'                       !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
c
         write(chr_text(1:14),'(2f7.1)') -plot_transx$, ! Back to correct zero.
     &                                   -plot_transy$  !
         chr_text = chr_text(1:14) // ' translate'      !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
c
         write(chr_text(1:14),'(2f7.1)') transx,         ! Translate depth axes.
     &                                  (transy+intvaly) ! With interval.
         chr_text = chr_text(1:14) // ' translate'       !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )     ! Process outcome.
c
         scalex = 1.0/plot_scalex$                      ! Both set to scale x.
         scaley = 1.0                                   !
c
c    Depth profile...
c
         else                                           ! Depth situation.
         last_function = function                       ! Copy function.
         scalex = 1.0/plot_scalex$                      ! Scale back.
         scaley = 1.0/plot_scaley$                      ! Ditto.
         end if                                         !
c
      call open_display                                 !
      write(chr_text(:12),'(2F6.3)') scalex, scaley     ! Set up page
      chr_text(13:) = ' scale'                          !
      write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
      call sei code( fort$, code, unit, b_flag  )       ! Process outcome.
c
         if( function .eq. depth$ ) then                ! Depth profile.
         write(chr_text(1:14),'(2f7.1)') -plot_transx$, ! Back to correct zero.
     &                                   -plot_transy$  !
         chr_text = chr_text(1:14) // ' translate'      !
         write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
         call sei code( fort$, code, unit, b_flag  )    ! Process outcome.
         end if                                         !
c
c    Move axes to new plotting point, down the page...
c    -------------------------------------------------
c
      write(chr_text(1:14),'(2f7.1)') 0.0, -transy      ! No translation in x.
      chr_text = chr_text(1:14) // ' translate'         !
      write(unit,'(a)',iostat=code) chr_text(:seiclen(chr_text)) ! & action.
      call sei code( fort$, code, unit, b_flag  )       ! Process outcome.
c
c   Re-set page directive...
c   ------------------------
c
      b_new_page = .not.b_new_page                      ! Not a new page?.
      end if                                            !
c
c   Return to Caller...
c   ===================
c
9999  return
      end
c
c    
      subroutine number(xnumber,x,y)
c
c  plot a number xnumber at position x,y
c
      include 'epimap.inc'
      character*10 xnumber      ! the text string to plot with the number
      real xx(1),yy(1)          ! to projection routine
      real xlat,xlon            ! true latitude and longitude
      real minlat,maxlat,minlon,maxlon
      common/bb/ minlon,maxlon,minlat,maxlat                          
c	  write(1,*) 'initial', x,y
c
c  convert from contor values to lat and lon assuming these have been scaled
c  to between 0 and 1
c
       xlat=minlat+y*(maxlat-minlat)
       xlon=minlon+x*(maxlon-minlon)
c
c   calculate epicenter screen coordinates
c
      CALL MAP_PROJ(xlon,xlat,xx,yy,1)                                            
      xx(1)=xx(1)+REFX0$                                                            
      yy(1)=yy(1)+REFY0$
      if(xlat.ge.minlatn$.and.xlat.le.maxlatn$.and.
     *    xlon.ge.minlong$.and.xlon.le.maxlong$) then
     	  call xchars(xnumber,10,xx(1),yy(1))
      endif
      return
      end



      subroutine plot(x,y,n)
c
c  x and y  : input values to plot
c  n        : 2: pen down  3: pen up
c
c  used with contor routine to use verstec calls and do the scaling and
c  and map projection
c
      include 'epimap.inc'
      real xx(1),yy(1)          ! to projection routine
      real xlat,xlon            ! true latitude and longitude
      real minlat,maxlat,minlon,maxlon
      integer n,nold,nn         ! pen up or down
      common/bb/ minlon,maxlon,minlat,maxlat                          
c
c  convert from contor values to lat and lon assuming these have been scaled
c  to between 0 and 1
c
       nn=n
       xlat=minlat+y*(maxlat-minlat)
       xlon=minlon+x*(maxlon-minlon)
c      write(27,*)'before,nn,n,nold',nn,n,nold
       if(xlat.ge.minlatn$.and.xlat.le.maxlatn$.and.
     *    xlon.ge.minlong$.and.xlon.le.maxlong$) then
          if(nold.eq.3) then
            nn=3
            nold=2  ! draw to next time
          endif
       else
          nold=3    ! move to for next value inside
          return    ! point is outside, do not plot
       endif
c      write(27,*)'after,nn,n,nold',nn,n,nold


          
c	   write(27,*)'ltlo',xlat,xlon
c
c   calculate epicenter screen coordinates
c
      CALL MAP_PROJ(xlon,xlat,xx,yy,1)                                            
      xx(1)=xx(1)+REFX0$                                                            
      yy(1)=yy(1)+REFY0$
c	  write(27,*)xx(1),yy(1)

      if(nn.eq.3) call xmovabs(xx(1),yy(1))
      if(nn.eq.2) call xdrwabs(xx(1),yy(1))
      return
      end

      subroutine plot_depth_ellipse(np,X0,Y0,xSCALE,ySCALE,xlength,
     &ylength,ad,bd,angd,x,y)

c plots a depth profile error ellipse using np points

c ad & bd are the major & minor axes lengths (km) and angd is
c the angle between the major axis and horizontal (rad)
c x & y are the horizontal and vertical distance of the
c center point (i.e., the hypocenter) from the origin

      implicit none
      real X0,Y0,xSCALE,ySCALE,xlength,ylength,ad,bd,angd
      real theta,xp,yp,xprev,yprev,xsave,ysave,pi
      real sx,sy,b,c,xd,yd,x,y
      integer np,j
      pi=3.141593

c (xd,yd) is hypocenter position in plot units
      xd=x
      yd=y

c      call newpen(1)
      theta=0.0

      do j=1,np+1
       theta=float(j-1)*2.*pi/float(np)
       sx=ad*cos(theta)
       sy=bd*sin(theta)
       xp=xd+(sx*cos(-angd)-sy*sin(-angd))*xSCALE
       yp=yd+(sy*cos(-angd)+sx*sin(-angd))*ySCALE
       if(j.eq.1)then
        xprev=xp
        yprev=yp
c        call plot(xp,yp,3)
        call xmovabs(xp,yp)
       else

c handle interpolation to and from boundaries
        if(xprev.ge.X0.and.xprev.le.X0+xlength.and.yprev.ge.Y0.
     &  and.yprev.le.Y0+ylength)then
         if(xp.ge.X0.and.xp.le.X0+xlength.and.yp.ge.Y0.and.yp.le.
     &   Y0+ylength)then

c both points inside
c          call plot(xprev,yprev,3)
           call xmovabs(xprev,yprev)
c          call plot(xp,yp,2)
           call xdrwabs(xp,yp)
          xprev=xp
          yprev=yp

         else

c (xp,yp) outside: interpolate it to boundary

c save original point
          xsave=xp
          ysave=yp

c linear interpolation
          b=(yp-yprev)/(xp-xprev)
          c=yp-b*xp
          if(xp.lt.X0)then
           xp=X0
           yp=b*xprev+c
c           call plot(xp,yp,2)
           call xdrwabs(xp,yp)
          elseif(xp.gt.X0+xlength)then
           xp=X0+xlength
           yp=b*xp+c
c           call plot(xp,yp,2)
            call xdrwabs(xp,yp)
          elseif(yp.lt.Y0)then
           yp=Y0
           if(b.ne.0)then
            xp=(yp-c)/b              
           else
            xp=xp
           endif
          elseif(yp.gt.Y0+ylength)then
           yp=Y0+ylength
           if(b.ne.0)then
            xp=(yp-c)/b              
           else
            xp=xprev
           endif
c           call plot(xp,yp,2)
            call xdrwabs(xp,yp)
          endif
          xprev=xsave
          yprev=ysave 
         endif

c previous point is outside, (xp,yp) is inside
        elseif(xp.ge.X0.and.xp.le.X0+xlength.and.yp.ge.Y0.
     &  and.yp.le.Y0+ylength)then
         if(xp.ne.xprev)then
          b=(yp-yprev)/(xp-xprev)
          c=yp-b*xp
          if(xprev.lt.X0)then
           xprev=X0
           yprev=b*xprev+c
          elseif(xp.gt.X0+xlength)then
           xprev=X0+xlength
           yprev=b*xprev+c
          elseif(yprev.lt.Y0)then
           yprev=Y0
           xprev=(yprev-c)/b              
          elseif(yprev.gt.Y0+ylength)then
           yprev=Y0+ylength
           xprev=(yprev-c)/b              
          endif
         else
          if(yprev.lt.Y0)yprev=Y0
          if(yprev.gt.Y0+ylength)yprev=Y0+ylength
         endif
c         call plot(xprev,yprev,3)
          call xmovabs(xprev,yprev)
c         call plot(xp,yp,2)
          call xdrwabs(xp,yp)
         xprev=xp
         yprev=yp         
        else
         xprev=xp
         yprev=yp
        endif            
       endif
      enddo
c      call newpen(2)
      return
      end
C                                                                               
C#######################################################                        
C                                                                               
      SUBROUTINE PLOT_LINES(X,Y,X0,Y0,N)
C                                                                               
C   N POINTS X,Y ARE JOINED BY STRAIGHT LINES                                   
C   ROUTINE SIMILAR TO CALL LINE EXCEPT THAT NO                                 
C   SCALING IS DONE                                                             
C                                                                               
      real X(*),Y(*)                                                       
      integer n, i
      real X0,Y0                                                             
      x(1)=x(1)+X0                                                              
      y(1)=y(1)+Y0                                                              
      DO 1 I=2,N                                                                
         x(i)=x(i)+X0                                                           
         y(i)=y(i)+Y0                                                           
 1    CONTINUE                                                                  
      call xdrwlin(n,x,y)                                                       
      RETURN                                                                    
      END                                                                       
c----------------------------------------------------------------------------
      subroutine profile
     *(n,epiorder,nout,xx,yy,max_depth,depth,number,
     *x1,y1,x2,y2,width,dist)
c
c   calculate distances to a profile line
c
c   j havskov september 1992
c
c   input  n:    number of points
c          epiorder: list of events in decreasing magnitude order.
c          x,y:  x and y value
c          max_depth: max depth to plot, if zero use all
c          depth    : event depths
c          x1,y1,x2,y2: end points of profile line, distances are calculated
c                       from x1,y1
c          x3,y3: point defining half width of profile, which is distance
c                 to line defined by x1,y1,x2,y2
c   
c   output nout: number of points selected
c          number: event number of events selected
c          dist: distances
c          width: half width of profile
c
c
      implicit none
      include 'seidim.inc'
      include 'epimap.inc'
c
      integer   epiorder(*)                        ! Magnitude ordered event.
     &         ,jjj                                ! Loop variable.
c
      real x1,x2,y1,y2,width
      real xx(*),yy(*),dist(*),depth(*),max_depth
      real x(max_epi), y(max_epi)
      integer number(*)
      integer n,i
c---half length of profile
      real length
c---profile and vertical to profile line constants
      real a1,b1,a2,b2
c---midline constants
      real a3,b3
c---normalization constants
      real ab1,ab2,ab3
c---half width and distance calculated for each point
      real w,d
c---number of points selected
      integer nout
c
c   Initialise....
c   ==============
c
      call map_proj( xx, yy, x, y, n ) ! Get co-ordinates.
c
c   calculates line constants, check that slope is not infinite
c
      if(x2.eq.x1) x1=x1+0.0001
      a1=(y2-y1)/(x2-x1)
      if(a1.eq.0.0) a1=0.0001
      a2=-1.0/a1
      a3=a2
      b1=y1-a1*x1
      b2=y1-a2*x1
      b3=(y1+y2)/2.0-a3*(x1+x2)/2.0
      ab1=sqrt(a1**2+1.0)
      ab2=sqrt(a2**2+1.0)
      ab3=sqrt(a3**2+1.0)
c
c  calculate half width and half length of profile
c
c      width=abs((y3-a1*x3-b1)/ab1)
      length=sqrt((x2-x1)**2+(y2-y1)**2)/2.0
c
c   calculate and plot outline of section window
c
c
c   calculate distances, first check that point is within width of
c   profile. If so, check that distance is within length of profile
c   by calculating distance to midle line.
c
      nout=0
      if(n.eq.0) then
        nout=1
        return
      endif
c
      do jjj=1,n                         ! Loop points.
      i = epiorder(jjj)                  ! Get event pointer.
      x(i) = x(i) + refx0$               ! Screen co-ordinates.
      y(i) = y(i) + refy0$               ! Screen co-ordinates.
c
        w=abs((y(i)-a1*x(i)-b1)/ab1)
        if(w.le.width) then
           d=abs((y(i)-a3*x(i)-b3)/ab3)
           if(d.le.length.and.
     *     (max_depth.eq.0.0.or.depth(i).le.max_depth)) then
              nout=nout+1
              dist(nout)=abs((y(i)-a2*x(i)-b2)/ab2)
              number(nout)=i
           endif
        endif
      enddo
c
c   add length of profile as an extra point for scaling purposes
c
      nout=nout+1
      dist(nout)=length*2
      number(nout)=nout
c
      return
      end
C                                                                               
      SUBROUTINE PT(IUV,X,Y)                                                    
C                                                                               
       X   = IUV/100000 * .01                                                   
      Y    = MOD(IUV,100000) * .01                                              
cjh       Y = - Y                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                               
      SUBROUTINE PUT (STX,STY,ENDX,ENDY,MSTRT,MEND)                             
C                                                                               
      COMMON/AA/ IXMN,IXMX,IYMN,IYMX,NLX,NRX,NTY,NBY                            
      MSTRTX = STX*100. + .5                                                    
      MSTRTY = STY*100. + .5                                                    
      MENDX = ENDX*100. + .5                                                    
      MENDY = ENDY*100. + .5                                                    
      MSTRT = MSTRTX*100000 + MSTRTY                                            
      MEND = MENDX*100000 + MENDY                                               
      IF (MSTRT-MEND) 5,200,5                                                   
 5    IF(MSTRTX-NLX) 10,40,10                                                   
 10   IF(MSTRTY-NTY) 20,40,20                                                   
 20   IF(MSTRTY-NBY) 30,40,30                                                   
 30   IF(MSTRTX-NRX) 50,40,50                                                   
 40   MSTRT = - MSTRT                                                           
 50   IF(MENDX-NRX) 60,90,60                                                    
 60   IF(MENDX-NLX) 70,90,70                                                    
 70   IF(MENDY-NBY) 80,90,80                                                    
 80   IF(MENDY-NTY) 100,90,100                                                  
 90   MEND = - MEND                                                             
 100  RETURN                                                                    
 200  MEND=0                                                                    
      MSTRT=0                                                                   
      RETURN                                                                    
      END                                                                       
C                                                                               
      FUNCTION RATIO(A,B,C,D,VAL)                                               
C                                                                               
      IF(A-VAL) 30,20,20                                                        
 20   IF(B-VAL) 50,60,60                                                        
 30   IF(B-VAL) 60,50,50                                                        
 50   RATIO = ((VAL-A)/(B-A))*(D-C) + C                                         
      RETURN                                                                    
 60   RATIO = 0                                                                 
      RETURN                                                                    
C                                                                               
C                                                                               
C  ORIGINAL 'RATIO'                                                             
C. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .          
C     ABIAS=0                                                                   
C     BBIAS=0                                                                   
C     IF (A-VAL) 30,10,20                                                       
C10   ABIAS =  .00001                                                           
C20   IF (B-VAL) 50,60,60                                                       
C40   BBIAS = .00001                                                            
C30   IF (B-VAL) 60,40,50                                                       
C50   RATIO = ((VAL-A-ABIAS)/(BBIAS+B-A-ABIAS))*(D-C)+C                         
C     GO TO 70                                                                  
C60   RATIO = 0                                                                 
C70   RETURN                                                                    
      END                                                                       
C
c-------------------------------------------------------
      SUBROUTINE SEARCH(n,xx,yy,lines,xp,yp,in,out,evin,
     &                  chr_epi_plt,data,filetype )
c
c   By Mario Villagran 12.93   UiB
C!JAB(BGS)Dec94 : Replace by call to "POLOS", to be consistant
C                 with the rest of SEISAN.
c
c search points inside a choosen area (input is cartesian)
c done for tektronics coordinates units.
c input are,
c n=# of events
c x(n),y(n)= coordinates of events
c lines=# of lines of surrounding choosen area + 1
c xp(lines),yp(lines)= coordinates of points
c in=file of geographic data, out=writing unit
c data = array with on event
c filetype = compact or multi line event
c
      include 'epimap.inc'                 ! EPIMAP definitions.
      include 'seidim.inc'                 ! Seisan array definitions.
      include 'libsei.inc'                 ! Library definitions & data defns.
      external polos,                      ! Polygon determinator.
     &         sei code                    ! Error encoder.
      integer  code                        ! Local condition.
      logical  b_flag                      ! Dummy.
c
      integer  n,                          ! # events.
     &         lines,                      ! Points in poylgon.
     &         in, out,                    ! Fortran i/o units.
     &         evin                        ! Count of events inside polygon.
      character chr_epi_plt(*) *(*)        ! Plot epicentres?.
c
      CHARACTER*80 DATA(*)                             !jh 8-99                       
      integer filetype,id,nstat,nphase,nrecord,nhead,k !jh 8-99
      character*1 exp,type                             !jh 8-99
                             

      real x(1), y(1)                      ! Co-ordinates.
      real xx(*),yy(*)
      real xp(*),yp(*)
      character text*80
c
      integer  i, ix                       ! Very local.
      real     poly(max_polyn$,2),         ! Polygon array.
     &         miny, minx,                 ! Min x,y.
     &         maxy, maxx                  ! & maximum.
c
c    Initialise...
c    =============
c
      evin=0
      miny = yp(1)                       ! Min y.
      minx = xp(1)                       ! &   x.
      maxy = yp(1)                       ! Max y.
      maxx = xp(1)                       ! &   x.
c
c    Only look at earthquakes which are within the window...
c    =======================================================
c    Load polygon into 2D array...in cartesian space...
c    --------------------------------------------------
c
      if( n .gt. 0 ) then                  !
         do 1 ix = 1, lines                   ! Loop closed polygon.
         poly(ix,1) = xp(ix)                  ! X co-ordinate.
         poly(ix,2) = yp(ix)                  ! & y.
c
         miny = amin1(miny,yp(ix))            ! New miny.
         minx = amin1(minx,xp(ix))            ! &   minx.
         maxy = amax1(maxy,yp(ix))            ! New maxy.
         maxx = amax1(maxx,xp(ix))            ! &   maxx.
1     continue                             !
c
      if( b_f_debug$ ) then                ! Debug information.
      write(dbgunit$,*)'SEARCH (min,max)(x,y)....'
      write(dbgunit$,*)'...',minx,maxx,miny,maxy
      end if                               !
c
c    Rewind input quake data...
c    --------------------------
c
      rewind(in,iostat=code)               ! Re-start events.
c      call sei code(fort$,code,in,b_flag ) ! Process the outcome.
c
c    Loop the events & check if in polygon or not...
c    ===============================================
c    The events correspond to the co-ordinates supplied....
c    ------------------------------------------------------
c
      do 1000 i = 1, n                     ! Loop events.
         if(filetype.eq.0) then            ! compact file
            read(in,'(a)',iostat=code) data(1)      ! Read text from input.
            call sei code(fort$,code,in,b_flag)     ! Process the outcome.
            nrecord=1
         else
            CALL INDATA(in,NSTAT,NPHASE,NHEAD,NRECORD, !
     &               TYPE,EXP,DATA,ID)                 !
         endif
c
c    Premature end of file...
c    ------------------------
c
c         if( b_flag.or.nrecord.eq.0 ) then                 ! Premature end of file.
c             WRITE(*,*)
c     &      '****                                                ***'
c             WRITE(*,*)                        ! 
c     &       '**** ERROR: contents of epimap.out are incomplete ***'
c             CHR_ERR_MSG$ =
c     &       '****                                              ****'
c             write(6,*)' Return to continue'
c             read(5,'(a)') text
c             CALL SEI CODE( STOP$,             ! Force halt.
c     &                  E_PEOF$,           ! On premature eof.
c     &                  0,                 ! Unit (n/a).
c     &                  B_FLAG )           ! Flag (n/a).
c        endif
c
c    Test proximity to polygon...
c    ----------------------------
c
                       
         call map_proj( xx(i), yy(i),      ! Get screen co-ordinates.
     &                  x(1),   y(1),      !
     &                  1 )                ! For 1 point.
         x(1) = x(1) + refx0$              ! Real co-ordinate.
         y(1) = y(1) + refy0$              ! Ditto.
c
c    Filter out those outside min & max area bounds...
c
         if( x(1) .lt. minx  .or.
     &       x(1) .gt. maxx  .or.
     &       y(1) .lt. miny  .or.
     &       y(1) .gt. maxy ) goto 1000    ! Not this point.
c
          call polos( y(1), x(1),           ! Check if event location.
     &               poly,                 ! Is inside the polygon.
     &               lines,                ! Of this number of points.
     &               b_flag )              ! Point inside?.
c
c    If point lies inside, write to output and increment event count...
c
            if( b_flag ) then                   ! Point lies inside polygon.
               write( out,'(a)',iostat=code )      ! Write to output.
     &         (data(k),k=1,nrecord)               ! The event.
c
               call sei code( fort$,          ! Process outcome of fortran i/o.
     &                     code,           ! With this condition.
     &                     out,            ! On this unit.
     &                     b_flag )        ! Flag (n/a).
               evin = evin + 1                ! Increment the event #.
               chr_epi_plt(i) = 'P'           ! Set for plotting.
c
            else                           ! Otherwise not.
               chr_epi_plt(i) = ' '           ! Set.
            end if                         !
1000  continue
      end if
c
c    Return to Caller...
c    ===================
c
9999  return
      end

      subroutine xdrwl(x1,y1,x2,y2)
c
c   draws part of line inside screen
c
      implicit none
      include  'epimap.inc'
c        implicit undefined(a-z)
c--- end of line input
      real x1,x2,y1,y2
c--- points to plot
      real x(101),y(101)
c--- counters
      integer i,k
c
c  calculate many point on line
c
      
      do i=1,101
         if((y2-y1).ne.0.0) then
            x(i)=x1+((i-1)*(y2-y1)/100.0)*((x2-x1)/(y2-y1))
         else
            x(i)=x1+(i-1)*((x2-x1)/100.0)
         endif
         if((x2-x1).ne.0.0) then
            y(i)=y1+((i-1)*(x2-x1)/100.0)*((y2-y1)/(x2-x1))
         else
            y(i)=y1+(i-1)*((y2-y1)/100.0)
         endif
      enddo
c
c   now plot while checking limits
c     
      do i=1,101
        if( x(i) .gt. 0.0            .and.
     &      x(i) .lt. screen_sizex$  .and.
     &      y(i) .gt. 0.0            .and.
     &      y(i) .lt. screen_sizey$) then
           call xmovabs(x(i),y(i))
           k=i+1
           goto 10
        endif
      enddo
c
c   check if all outside
c
 10   continue
      if(k.eq.102) return
c
c   now draw
c
      do i=k,101
        if( x(i) .gt. 0.0            .and.
     &      x(i) .lt. screen_sizex$  .and.
     &      y(i) .gt. 0.0            .and.
     &      y(i) .lt. screen_sizey$) then
           call xdrwabs(x(i),y(i))
         endif
      enddo
c
c   do a stroke in postscript
c
      call xout(10.0,10.0)
c
      return
      end

      subroutine old_xy_ellipse
     *(erx,ery,erz,cvxy,cvxz,cvyz,emaj,emin,ang)
c     subroutine xy_ellipse(erx,ery,erz,cvxy,cvxz,cvyz,emaj,emin,ang)
c
c 2013-06-12 pv: subroutine is replaced by the same subroutine in LIB/err_ellipse.for
c 

c calculate maximum horizontal dimensions of error ellipse 
c using ellipsoid dimensions & orientations
c  in ae & ve (output of subroutine ellipse)

c calls subroutine hyperr to perform actual plotting

c Barry Lienert Oct 94

        implicit none
        real ae(9),ve(9)
        real a,b,c,emaj,emin,ang
        real erx,ery,erz,cvxy,cvxz,cvyz,var(3,3)
c
c   covarriance matrix
c
        var(1,1)=erx*erx
        var(2,2)=ery*ery
        var(3,3)=erz*erz
        var(1,2)=cvxy
        var(1,3)=cvxz
        var(2,3)=cvyz
        var(2,1)=var(1,2)
        var(3,1)=var(1,3)
        var(3,2)=var(2,3)
c
c
        call ellipse_x(var,ae,ve)
c
c find the projection of components of the major ellipse axes (the 3
c eigenvalues of the covariance matrix) on the xy plane
        a=sqrt(abs(ae(1)))*cos(atan2(ve(3),sqrt(ve(1)*ve(1)+ve(2)
     &  *ve(2))))
        b=sqrt(abs(ae(3)))*cos(atan2(ve(6),sqrt(ve(4)*ve(4)+ve(5)
     &  *ve(5))))
        c=sqrt(abs(ae(6)))*cos(atan2(ve(9),sqrt(ve(7)*ve(7)+ve(8)
     &  *ve(8))))

c major & minor axes are the largest two of these
        if(a.ge.b.and.a.ge.c)then
         emaj=a

c angle between major axis, a, and x-axis
         ang=atan2(ve(2),ve(1))

         if(b.ge.c)then
          emin=b
         else
          emin=c
         endif
        elseif(b.ge.a.and.b.ge.c)then
         emaj=b
         ang=atan2(ve(5),ve(4))
         if(c.ge.a)then
          emin=c
         else
          emin=a
         endif
        elseif(c.ge.a.and.c.ge.b)then
         emaj=c
         ang=atan2(ve(8),ve(7))
         if(b.ge.a)then
          emin=b
         else
          emin=a
         endif
        endif

c        write(*,*)emaj,emin,ang*180/pi
        return
        end
C
c-----------------------------------------------------
        subroutine xy_lonlat (x,y,srclon,srclat,iproj)
c
c The input is Tek units and give back geographic
c coordinates.
c  By M. Villagran 10.11.93
c only for mercator projection.
c
c  Modifications:
c  1994
c    Jan 13   m.v. : now possible to use in all projections
C!JAB(BGS)Dec94    : Replace by call to central projection routine..
c
c Input is x=ix
c          y=iy
c ix,iy are in tek coordinates
c
c Output in srclon= longitude
c           srclat= latitude
c srclon,srclat are geographic coordinates
C
      include 'epimap.inc'                 ! EPIMAP definitions.
      include 'libsei.inc'                 ! Library definitions & data defns.
      external map_proc,                   !
     &         sei code                    ! Error encoder.
      integer  code                        ! Local condition.
      logical  b_flag                      ! Dummy.
c
      real     x, y, srclon, srclat
      integer  iproj
      real     xx, yy                      ! Local working copies.
C 
C   Reduce input TEK co-ordinates to radians...
c   ===========================================
C
      xx = x - REFX0$                      !
      yy = y - REFY0$                      !
      xx = (XMIN$+(xx/scale$))*deg_to_rad$ !
      yy = (YMIN$+(yy/scale$))*deg_to_rad$ !
C                                          !
C    Call map-proc in decode mode...       !
C    ===============================       !
C                                          !
      call map_proc( decode$+iproj,        !
     &               srclat,  srclon,      !
     &               yy,      xx,          !
     &               REFLATN$,REFLONG$,    !
     &               code )                !
      if( b_f_debug$ ) write(dbgunit$,*)
     &  'Decode (x,y) in degrees..',srclon, srclat
      call sei code( stop$,                ! Warn user of.
     &               code,                 ! Conversion error.
     &               0,                    ! Fortran unit (n/a).
     &               b_flag )              ! Flag (n/a).
C                                         
C   Return to caller... 
C   ===================                    
C                                          
9999  RETURN                               
      END                      
C
c----------------------------------
      subroutine zoom(n,xp,yp,case)
c     m. villagran 12.93
c  get new coordinates to plot
c  as well as new mad dimension and origin coordinates
C!JAB(BGS)Dec94  : Install central definitions
C!JAB(BGS)May95  : Rework the grid spacing algorithm.
c
      integer n,i,case
      real xp(n),yp(n),xa,xb,ya,yb,x,y
c
      include 'epimap.inc'                ! EPIMAP definitions.
      include 'libsei.inc'                ! & those for library.
c
      real*8   sc,                        ! Unit scale.
     &         val                        ! & value.
      logical  b_dateline                 ! Crosses dateline?.
c
c    Debug values...
c    ---------------
c
      if( b_f_debug$ ) then
      write(dbgunit$,*)'Input to Zoom...'
      write(dbgunit$,*)'minlat...', MINLATN$
      write(dbgunit$,*)'maxlat...', MAXLATN$
      write(dbgunit$,*)'minlon...', MINLONG$
      write(dbgunit$,*)'maxlon...', MAXLONG$
      write(dbgunit$,*)'dellat...', GRID_SPACE_LATN$
      write(dbgunit$,*)'dellon...', GRID_SPACE_LONG$
      write(dbgunit$,*)'baselat..', GRID_BASE_LATN$
      write(dbgunit$,*)'baselon..', GRID_BASE_LONG$
      write(dbgunit$,*)'...finish'
      end if
c
c    Determine extent of the map...
c    ==============================
c
10    xa=180.00
      xb=-180.00
      ya=90.00
      yb=-90.00
        do i=1,n
          if(xp(i).lt.xa)xa=xp(i)
          if(xp(i).gt.xb)xb=xp(i)
          if(yp(i).lt.ya)ya=yp(i)
          if(yp(i).gt.yb)yb=yp(i)
        enddo
c
      MINLONG$ = xa
      MAXLONG$ = xb
      MINLATN$ = ya
      MAXLATN$  =yb
c
      b_dateline = minlong$ .gt. maxlong$  .and. ! Over dateline?.
     &             maxlong$ .lt. 0.0             !
      if( b_dateline )maxlong$ = maxlong$ + 360.0! Adjust.
c
      if(case.eq.2)goto 99
c
c    Determine new grid intervals...
c    ===============================
c
      x = abs(MAXLONG$-MINLONG$)
      y = abs(MAXLATN$-MINLATN$)
c
c    Re-set longitude grid interval...
c    =================================
c
      sc = 10.0d0                               ! Initial scaling.
1     val = dble( x ) / sc                      ! # intervals.
      if( val .lt. 4.0d0 ) then                 ! Take in down a level.
      sc = sc * 0.1d0                           !
      goto 1                                    ! & try again.
C
      else if( sc .gt. 0.1d0 ) then             ! Degrees.
         if( val .gt. 24.0d0 ) then             ! Try 5.
         grid_space_long$ = 5.0d0*sc            !
         else if( val .gt. 16.0d0 ) then        ! Try 3.
         grid_space_long$ = 3.0d0*sc            !
         else if( val .gt. 8.0d0 ) then         ! Try 2.
         grid_space_long$ = 2.0d0*sc            !
         else                                   ! Otherwise unit.
         grid_space_long$ = sc                  !
         end if                                 !
c
      else if( val .gt. 15.0d0 ) then           ! Try 5.
      grid_space_long$ = 5.0d0*sc               !
c
      else if( val .gt. 5.0d0 ) then            ! Try 2.
      grid_space_long$ = 2.0d0*sc               !
c
      else                                      ! Otherwise unit.
      grid_space_long$ = sc                     !
      end if                                    !
c
c    Re-set latitude grid interval...
c    ================================
c
      sc = 10.0d0                               ! Initial scaling.
2     val = dble( y ) / sc                      ! # intervals.
      if( val .lt. 4.0d0 ) then                 ! Take in down a level.
      sc = sc * 0.1d0                           !
      goto 2                                    ! & try again.
c
      else if( sc .gt. 0.1d0 ) then             ! Degrees.
         if( val .gt. 24.0d0 ) then             ! Try 5.
         grid_space_latn$ = 5.0d0*sc            !
         else if( val .gt. 16.0d0 ) then        ! Try 3.
         grid_space_latn$ = 3.0d0*sc            !
         else if( val .gt. 8.0d0 ) then         ! Try 2.
         grid_space_latn$ = 2.0d0*sc            !
         else                                   ! Otherwise unit.
         grid_space_latn$ = sc                  !
         end if                                 !
c
      else if( val .gt. 15.0d0 ) then           ! Try 5.
      grid_space_latn$ = 5.0d0*sc               !
c
      else if( val .gt. 5.0d0 ) then            ! Try 2.
      grid_space_latn$ = 2.0d0*sc               !
c
      else                                      ! Otherwise unit.
      grid_space_latn$ = sc                     !
      end if                                    !
C
C    Adjust to base latitude...
C    --------------------------
C
99    continue
5001  if( grid_base_latn$ .lt. minlatn$ ) then     ! Needs adjustment.
      grid_base_latn$ = grid_base_latn$            !
     &                + grid_space_latn$           !
      goto 5001                                    ! More?
      end if                                       !
C
5002  if( grid_base_latn$ .gt. minlatn$ ) then     ! Needs adjustment.
      grid_base_latn$ = grid_base_latn$            !
     &                - grid_space_latn$           !
      goto 5002                                    ! More?
      end if                                       !
C
C    Adjust to base longitude...
C
      if( b_dateline )                             ! Adjust for dateline.
     &grid_base_long$ = grid_base_long$ + 180.0    !
C
5101  if( grid_base_long$ .lt. minlong$ ) then     ! Needs adjustment.
      grid_base_long$ = grid_base_long$            !
     &                + grid_space_long$           !
      goto 5101                                    ! More?
      end if                                       !
C
5102  if( grid_base_long$ .gt. minlong$ ) then     ! Needs adjustment.
      grid_base_long$ = grid_base_long$            !
     &                - grid_space_long$           !
      goto 5102                                    ! More?
      end if                                       !
c
c    Debug values...
c    ---------------
c
      if( b_f_debug$ ) then
      write(dbgunit$,*)'Output from Zoom...'
      write(dbgunit$,*)'minlat...', MINLATN$
      write(dbgunit$,*)'maxlat...', MAXLATN$
      write(dbgunit$,*)'minlon...', MINLONG$
      write(dbgunit$,*)'maxlon...', MAXLONG$
      write(dbgunit$,*)'dellat...', GRID_SPACE_LATN$
      write(dbgunit$,*)'dellon...', GRID_SPACE_LONG$
      write(dbgunit$,*)'baselat..', GRID_BASE_LATN$
      write(dbgunit$,*)'baselon..', GRID_BASE_LONG$
      write(dbgunit$,*)'...Zoom finish'
      end if
c
c    Return to Caller...
c    ===================
c
      return
      end
C
CSTART*************************************************************************
C                                                                             *
C                    BGS/GSRG Applications Programming Unit                   *
C                                                                             *
C      System  : SEISAN                                                       *
C      Name    : EPIMAP_BD                                                    *
C      Purpose : Block data for EPIMAP                                        *
C      Author  : J. A. Bolton                                                 *
C      Date    : 16 October 1994                                              *
C      Version : V01                                                          *
C                                                                             *
C      Modification : Include colour map and change Block data name from      *
C                     BD_MAP_PROC to BD_EPIMAP                                *
C                                                                             *
CEND***************************************************************************
C
      BLOCK DATA      BD_EPIMAP
C
C
C    System variables...
C    ===================
      include 'epimap.inc'     ! EPIMAP definitions (contains seiplot.inc).
c     include 'symbol.inc'     ! Symbol definitions.
CJAB(BGS)Jun95 : 7th... in conjunction with latest epimap enhancements..
CSTART********************************************************************
C                     BGS/GSRG Programming Unit                          * 
C                                                                        * 
C     System       : SEISAN                                              * 
C     Module       : SYMBOL.INC                                          * 
C     Purpose      : Definitions of symbols constructed in real-time     *
C     Note         : Must be placed after last dimension statement in    *
C                    routine which uses thes definitions due to the data *
C                    statement.                                          *
C     Author       : J. A. Bolton                                        *
C     Date         : 26 January 1996                                     *
C     Version      : V01                                                 *
CEND**********************************************************************
C
C    Define data arrays & symbol codes...
C    ====================================
C
      INTEGER      S_UCROSS$                    ! Upright cross.
     &            ,S_OSQUARE$                   ! Open square.
     &            ,S_OTRIANGLE$                 ! Open triangle.
     &            ,S_DCROSS$                    ! Diagonal cross.
     &            ,S_ASTERIX$                   ! Circular asterix.
     &            ,S_OCTOGON$                   ! Octogon/vertical radius.
     &            ,S_OCIRCLE$                   ! open circle.
     &            ,S_UXCIRCLE$                  ! Circle with upright cross.
     &            ,S_DXCIRCLE$                  ! Circle with diagonal cross.
     &            ,S_ODIAMOND$                  ! Open diamond.
     &            ,S_OITRIANGLE$                ! Open inverted triangle.
     &            ,S_LAST$                      ! *** must be last ***.
C
      PARAMETER   (S_UCROSS$     = 1)                ! & values.
      PARAMETER   (S_OSQUARE$    = S_UCROSS$     + 1)!
      PARAMETER   (S_OTRIANGLE$  = S_OSQUARE$    + 1)!
      PARAMETER   (S_DCROSS$     = S_OTRIANGLE$  + 1)!      
      PARAMETER   (S_ASTERIX$    = S_DCROSS$     + 1)!
      PARAMETER   (S_OCTOGON$    = S_ASTERIX$    + 1)!
      PARAMETER   (S_OCIRCLE$    = S_OCTOGON$    + 1)!
      PARAMETER   (S_UXCIRCLE$   = S_OCIRCLE$    + 1)!
      PARAMETER   (S_DXCIRCLE$   = S_UXCIRCLE$   + 1)!
      PARAMETER   (S_ODIAMOND$   = S_DXCIRCLE$   + 1)!
      PARAMETER   (S_OITRIANGLE$ = S_ODIAMOND$   + 1)!
      PARAMETER   (S_LAST$       = S_OITRIANGLE$)    ! *** must be last ***.
C
C    Symbol encoder instructions...
C    ------------------------------
C
      INTEGER      SYMBOL_C$,                    ! Length of symbol instruction.
     &             SYMBOL_N$                     ! # of symbols.
      PARAMETER   (SYMBOL_C$ = 13)               ! & values.
      PARAMETER   (SYMBOL_N$ = S_LAST$)          !
      CHARACTER    CHR_SYMBOL$(SYMBOL_N$) *(SYMBOL_C$)
      COMMON /CHRSYMB/CHR_SYMBOL$
C
C    & encoded strings...
C    --------------------
C    The format is as follows...
C
C    boolean - radius required?
C    # radii (0 to 9)
C    chord % internal angle initial displacement  (00 to 99) (+ve!)
C    scale change % of radius (00 to 99)
C    boolean - draw circumference?
C    # vertices (000 to 999)
C    chord % internal angle initial displacement  (+00 to +99 or -99)
C
      DATA    CHR_SYMBOL$ /'T40000F000+00',   ! Upright cross.
     &                     'F00000T004+50',   ! Open square.
     &                     'F00000T003+75',   ! Open triangle.
     &                     'T45000F000+00',   ! Diagonal cross.
     &                     'T80000F000+00',   ! Circular asterix.
     &                     'T12500T008+50',   ! Octogon/vertical radius.
     &                     'F00000T050+00',   ! open circle (use 50 vertices!).
     &                     'T40000T050+00',   ! Circle with upright cross.
     &                     'T45000T050+00',   ! Circle with diagonal cross.
     &                     'F00000T004+00',   ! Open diamond.
     &                     'F00000T003-75'/   ! Open inverted triangle.
C
C************ END of symbol.dsd ************************
C    Projection text...
C    ==================
C
      DATA       CHR_PROJ$ /               ! Projection text.
     &'POLAR STEREOGRAPHIC     conformal, azimuthal                ',
     &'ORTHOGRAPHIC            view from infinity, azimuthal       ',
     &'MERCATOR                cylindrical, conformal              ',
     &'LAMBERT EQUAL AREA      azimuthal                           ',
     &'GNOMONIC                Great Circles are straight lines    ',
     &'AZIMUTHAL EQUIDISTANT   distance from origin is to SCALE$   ',
     &'STEREOGRAPHIC           conformal, azimuthal                ',
     &'EQUIDISTANT CYLINDRICAL                                     ',
     &'OBLIQUE MERCATOR        cylindrical, conformal              ',
     &'MOLLWEIDE ELLIPTICAL    pseudocylindrical, equal area       ',
     &'SANSON''S SINUSOIDAL     pseudocylindrical, equal Area      '
     &                    /                ! End of list.
C
C    Other items...
C    ==============
C
      DATA       PROJ$               / 0 /,
     &           LATP$               / 0.0 /,
     &           LONP$               / 0.0 /,
     &           ROTATE$             / 0.0 /,
     &           SMALL_SCALE_LENGTH$ / 20.0 /,   ! In km (see b_small_scale$).
     &           MAX_MAG_SMALL_RANGE$/ 4.0 /,    ! Maximum magnitude (small).
     &           B_DETAIL$           /.FALSE./,
     &           B_SMALL_SCALE$      /.FALSE./   ! Applicable to small scale.
c
c   Colours list...
c   ---------------
c
      data       COLOUR$     / 
     &                         BLUE$,
     &                         GREEN$,
     &                         RED$,
     &                         YELLOW$,
     &                         WHITE$,
     &                         BLACK$ /
      data       CHR_COLOUR$ /
     &                        'Blue           ',
     &                        'Green          ',
     &                        'Red            ',
     &                        'Yellow         ',
     &                        'White          ',
     &                        'Black          '/
c
c   Symbols list...
c   ---------------
c
      data       b_sym_range$      /  .FALSE.      /,   ! Symbols by mag range?
     &           b_sym_stat_fill$  /  .FALSE.      /,   ! Fill fillable symbols?
     &           b_sym_epic_fill$  /  .FALSE.      /,   ! Ditto.
     &           b_sym_small_range$/  .FALSE.      /,   ! Ditto.
     &           sym_magneg$       /  S_OSQUARE$   /,   ! Square.
     &           sym_mag0$         /  S_UCROSS$    /,   ! Upright cross.
     &           sym_mag$          /  S_OCTOGON$   /,   ! Octogon/vert radius.
     &           sym_magx$         /  S_ODIAMOND$  /,   ! Open diamond
     &           sym_place$        /  S_OCIRCLE$   /,   ! Open circle.
     &           sym_station$      /  S_OTRIANGLE$ /    ! Open triangle.
 

      END








