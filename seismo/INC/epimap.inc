C     MAP_PROC.INC Definitions & data definitions
C
CSTART*************************************************************************
C                    BGS/GSRG Computer Unit                                   *
C                                                                             *
C      System        : SEISAN                                                 *
C      Name          : MAP_PROC.INC                                           *
C      Purpose       : Definitions and data definitions for spherical map     *
C                      projections.                                           *
C      Author        : J. A. Bolton                                           *
C      Date          : 16 October 1994                                        *
C      Version       : V01                                                    *
C                                                                             *
C      Modifications : Include flag for multple decimal place representation  *
C                      of geographicals, and constant scale cross-sections.   *
C      Author        : J. A. Bolton                                           *
C      Date          : 20 May 1995                                            *
C      Version       : V01                                                    *
c-------------------------------------------------------------------------------
c      oct 98 jh     :            version 7.0 check 
c                    :   change name of common block map$ to mapp$
C                                                                             *
CEND***************************************************************************
      include 'seiplot.inc'                ! Definitions for plotting (colour)
C
C    Functional definitions (major keys)...
C    ======================================
C    The functional key argument in subroutine call is given by..
C    FUNCTION = Major key + projection number.
C
      INTEGER    ENCODE$,                  ! Encode (x,y) from lat-long.
     &           DECODE$,                  ! Decode (x,y) to   lat-long.
     &           DETAIL$                   ! Find map detail.
      PARAMETER (ENCODE$ = 0)              ! Default.
      PARAMETER (DECODE$ = 128)            ! Major key.
      PARAMETER (DETAIL$ = DECODE$*2)      ! Ditto.
C
C    Projection definitions...
C    =========================
C
      INTEGER    PROJ_C$                   ! Text length.
      PARAMETER (PROJ_C$ = 60)             ! & value.
C
      INTEGER    POSTEREO$,                ! Polar stereographic.
     &           ORTHOGRAPH$,              ! Orthographic.
     &           MERCATOR$,                ! Mercator.
     &           LAMBEQAREA$,              ! Lambert equal area.
     &           GNOMONIC$,                ! Gnomonic.
     &           AZIMEQDIST$,              ! Azimuthal equidistant.
     &           STEREOGRAPH$,             ! Oblique (general) stereographic.
     &           EQDISTCYLIN$,             ! Equidistant cylindrical.
     &           OBLIQUEMERC$,             ! Oblique mercator.
     &           MOLLWEIDE$,               ! Mollweide elliptical.
     &           SINUSOIDAL$,              ! Sinusoidal (Sansan's).
     &           PROJ_LAST$                ! *** must be last ***
C
      PARAMETER (POSTEREO$    = 1 )               ! & values.
      PARAMETER (ORTHOGRAPH$  = POSTEREO$    + 1) !
      PARAMETER (MERCATOR$    = ORTHOGRAPH$  + 1) !
      PARAMETER (LAMBEQAREA$  = MERCATOR$    + 1) !
      PARAMETER (GNOMONIC$    = LAMBEQAREA$  + 1) !
      PARAMETER (AZIMEQDIST$  = GNOMONIC$    + 1) !
      PARAMETER (STEREOGRAPH$ = AZIMEQDIST$  + 1) !
      PARAMETER (EQDISTCYLIN$ = STEREOGRAPH$ + 1) !
      PARAMETER (OBLIQUEMERC$ = EQDISTCYLIN$ + 1) !
      PARAMETER (MOLLWEIDE$   = OBLIQUEMERC$ + 1) !
      PARAMETER (SINUSOIDAL$  = MOLLWEIDE$   + 1) !
      PARAMETER (PROJ_LAST$   = SINUSOIDAL$)      ! *** must be last ***
C
C    Conversion and processing definitions...
C    ========================================
C
      INTEGER    MAX_COUNT$                     ! Maximum iteration count.
      PARAMETER (MAX_COUNT$ = 100)              ! & value.
C
      REAL       PI$,                           ! Radians in semi-circle.
     &           MAX_DIFF$,                     ! Small angular tolerance.
     &           MAX_LAT$,                      ! Maximum numeric latitude (rad)
     &           DEG_TO_RAD$,                   ! Degrees to radians.
     &           RAD_TO_DEG$,                   ! Radians to degrees,
     &           TOLERANCE$                     ! Extreme tolerance.
C
      PARAMETER (PI$         = 3.141592654)     ! & values.
      PARAMETER (MAX_DIFF$   = PI$ * 0.1        ! 10th second of arc.
     &                       /(180.*60.*60.))   !
      PARAMETER (MAX_LAT$    = PI$ * 0.5        ! Maximum latitude considered.
     &                       - MAX_DIFF$)       !
      PARAMETER (DEG_TO_RAD$ = PI$ / 180.0)     ! Degrees to radians.
      PARAMETER (RAD_TO_DEG$ = 1.0/DEG_TO_RAD$) ! Radians to degrees. 
      PARAMETER (TOLERANCE$  = 0.5*DEG_TO_RAD$) ! Extreme tolerance.
C
C    User interface validation directives...
C    ---------------------------------------
C
      INTEGER*4  LATN$, LONG$,                  ! Latitude & longitude.
     &           GRID$                          ! & grid directive.
      PARAMETER (LATN$ = 0)                     ! & value.
      PARAMETER (LONG$ = LATN$   + 1)           ! Ditto.
      PARAMETER (GRID$ = LONG$   + 1)           ! Et ditto.
C
C    EPIMAP screen dimensions...
C    ---------------------------
C
      INTEGER    X_SCREEN_SIZE$                 ! Screen size (% monitor screen)
      PARAMETER (X_SCREEN_SIZE$ = 80.0)         ! Nominal value.
      REAL       REFX$,    REFY$,               ! Plot zero(allowing for text).
     &           REF0X$,   REF0Y$,              ! Plotting space zero.
     &           XSIZE$,   YSIZE$               ! Plot size in tek units.
C
      REAL       BORDER_X$,                     ! Border in x.
     &           BORDER_Y$,                     ! & in y.
     &           TITLE_BOTTOM$,                 ! Title start in y.
     &           EVENT_TITLE_Y$                 ! Location of event_title in y.
C
      REAL       PLOT_LEFTX$,                   ! Left hand edge of plot.
     &           LAT_TXT_DISPX$,                ! Latitude text displacement.
     &           LAT_TXT_DISPY$,                ! Ditto.
     &           LON_TXT_DISPX$,                ! Longitude text displacement.
     &           LON_TXT_DISPY$,                ! Ditto.
     &           ALLOWANCE$                     ! Small displacement.
      REAL       SYM_STAT_SIZE$                 ! Station symbol size.
     &          ,SYMBOL_KEYX$                   ! Symbol key location.
     &          ,SYMBOL_INCR$                   ! Symbol increment.
     &          ,NOM_CHR_SIZE$                  ! Nominal character size.
      PARAMETER (BORDER_X$     = 40.0)          !
      PARAMETER (BORDER_Y$     = 40.0)          !
      PARAMETER (PLOT_LEFTX$   = 240.0)         !
      PARAMETER (LAT_TXT_DISPX$= -70.0)         !
      PARAMETER (LAT_TXT_DISPY$= -10.0)         ! Smaller than lon value.
      PARAMETER (LON_TXT_DISPX$= -30.0)         ! Smaller than lat value.
      PARAMETER (LON_TXT_DISPY$= -25.0)         !
      PARAMETER (ALLOWANCE$    = 5.0)           !
      PARAMETER (SYMBOL_KEYX$  = 180.0)         ! 
      PARAMETER (SYMBOL_INCR$  = 2.5)           !
      PARAMETER (SYM_STAT_SIZE$=SYMBOL_INCR$*6.0)
C
      PARAMETER (TITLE_BOTTOM$ = SCREEN_SIZEY$  !
     &                         - BORDER_Y$ )    !
      PARAMETER (EVENT_TITLE_Y$= TITLE_BOTTOM$  !
     &                         - ALLOWANCE$*15.0)!
      PARAMETER (REFX$         = PLOT_LEFTX$    !
     &                         + ALLOWANCE$ )   !
      PARAMETER (REFY$         = BORDER_Y$      !
     &                         + ALLOWANCE$ )   !
      PARAMETER (REF0X$        = REFX$          !
     &                         - LAT_TXT_DISPX$)!
      PARAMETER (REF0Y$        = REFY$          !
     &                         - LON_TXT_DISPY$ ! 
     &                         + ALLOWANCE$ )   !
      PARAMETER (XSIZE$        = SCREEN_SIZEX$  !
     &                         - BORDER_X$      !
     &                         + LAT_TXT_DISPX$ !
     &                         - ALLOWANCE$*2.0 !
     &                         - REF0X$ )       !
      PARAMETER (YSIZE$        = SCREEN_SIZEY$  !
     &                         - BORDER_Y$      !
     &                         + LON_TXT_DISPY$ !
     &                         - ALLOWANCE$*2.0 !
     &                         - REF0Y$ )       !
      PARAMETER (NOM_CHR_SIZE$ = (XSIZE$        ! Nominal character width.
     &                         - LAT_TXT_DISPX$ !
     &                         *2.0)            !
     &                         /57.0 )          !
C
C    Grid drawing step size...
C    -------------------------
C
      INTEGER    GRID_STEP_N$                   ! Steps to draw grid-line.
      PARAMETER (GRID_STEP_N$ = 200)            ! & value.
C
C    Length of 1 degree at equator (used for distance calculations)... 
C    -----------------------------------------------------------------
C    Must only be changed if the same value in other programs is changed
C    unilaterally...
C
      REAL*8     EQ_DEG_TO_KM$                  ! Equator degrees to km.
      PARAMETER (EQ_DEG_TO_KM$ = 111.2d0)       ! & values.
C
C    EPIMAP array sizes...
C    ---------------------
C
      INTEGER    PLOTSTAT_N$,                   ! Max # plot stations.
     &           MAP_N$,                        ! Max # contour files.
     &           PLACE_N$,                      ! Max # placename files.
     &           EPIFILE_N$                     ! Max # epicentre files.
      PARAMETER (PLOTSTAT_N$ = 9000)             ! & values.
      PARAMETER (MAP_N$      = 20)              ! Ditto.
      PARAMETER (PLACE_N$    = 20)              ! Ditto.
      PARAMETER (EPIFILE_N$  = 20)              ! Ditto.
C
C    EPIMAP colours...
C    -----------------
C
      integer    xcolour_c$                           ! Text length.
      parameter (xcolour_c$ = 15)                     ! & value.
      integer   colour$(xcolour_n$)                   ! Values.
      character chr_colour$(xcolour_n$) *(xcolour_c$) ! & text.
      common /epi_col$/ colour$, chr_colour$          !
C
C    EPIMAP symbols...
C    -----------------
C
      integer    sym_station$,                  ! Station symbol.
     &           sym_place$,                    ! Place symbol.
     &           sym_magneg$,                   ! Epicentre < 0 symbol.
     &           sym_mag0$,                     ! Epicentre mag 0 symbol.
     &           sym_magx$,                     ! Ditto above maximum symbol.
     &           sym_mag$                       ! Other epicentre magnitudes.
      real       max_mag_small_range$           ! Magnitude limit, small range.
      logical    b_sym_range$                   ! Plot by magnitude range?
     &          ,b_sym_stat_fill$               ! Fill station symbols?.
     &          ,b_sym_epic_fill$               ! Ditto epicentre?
     &          ,b_sym_small_range$             ! Small mag range symbol set?
      common /epi_sym$/ sym_station$, sym_place$ 
     &                 ,sym_magneg$ 
     &                 ,sym_mag0$ 
     &                 ,sym_magx$,    sym_mag$ 
     &                 ,max_mag_small_range$
     &                 ,b_sym_range$
     &                 ,b_sym_stat_fill$
     &                 ,b_sym_epic_fill$
     &                 ,b_sym_small_range$
C
C    EPIMAP Plot directives...
C    =========================
C
      integer    map$,                          ! Map plot.
     &           depth$                         ! Depth plot.
      parameter (map$   = 0)                    ! & values.
      parameter (depth$ = map$ + 1)             !
C
C    ===================
C    Projection text...
C    ------------------
C
      CHARACTER  CHR_PROJ$(PROJ_LAST$)*(PROJ_C$) ! Projections.
C
C    Other items...
C    --------------
C
      INTEGER    PROJ$                         ! Projection number.
      REAL       LATP$, LONP$,                 ! Pole location for obl.mercator.
     &           ROTATE$                       ! Axial rotation.
      LOGICAL    B_DETAIL$                     ! Found map centre co-ordinates?.
C
      COMMON /MAPP$/ PROJ$,                     ! & reserve.
     &              LATP$, LONP$,              !
     &              ROTATE$,                   !
     &              B_DETAIL$,                 !
     &              CHR_PROJ$                  !
C
C    Map plotting definitions used by epimap...
C    ------------------------------------------
C
      INTEGER       EPICENTRE_N$               ! # epicentres plotted.
      REAL          MINLONG$, MAXLONG$,        ! Min & max longitude.
     &              MINLATN$, MAXLATN$,        ! & latitude.
     &              REFLONG$, REFLATN$,        ! Reference lat-long.
     &              GRID_BASE_LONG$,           ! Base grid longitude.
     &              GRID_BASE_LATN$,           ! & latitude base.
     &              GRID_SPACE_LONG$,          ! Longitude spacing.
     &              GRID_SPACE_LATN$           ! & latitude spacing.
      REAL          REFX0$,   REFY0$,          ! Plot zero.
     &              XMIN$,    XMAX$,           ! X range.
     &              YMIN$,    YMAX$,           ! & y range.
     &              SCALE$,                    ! & scaling.
     &              SMALL_SCALE_Y$,            ! Key level for scale bar.
     &              SMALL_SCALE_LENGTH$        ! Applicable scales for...
      LOGICAL       B_SMALL_SCALE$             ! Small scale treatment?.
C
      COMMON /MAP_PLOT$/ EPICENTRE_N$,
     &                   B_SMALL_SCALE$,
     &                   SMALL_SCALE_Y$,
     &                   SMALL_SCALE_LENGTH$,
     &                   MINLONG$, MAXLONG$, MINLATN$, MAXLATN$,
     &                   GRID_SPACE_LONG$,   GRID_SPACE_LATN$,
     &                   GRID_BASE_LONG$,    GRID_BASE_LATN$,
     &                   REFLONG$, REFLATN$,  
     &                   REFX0$,   REFY0$,
     &                   XMIN$,    XMAX$,    YMIN$,    YMAX$,
     &                   SCALE$
C
C********************** End of list ***************************
