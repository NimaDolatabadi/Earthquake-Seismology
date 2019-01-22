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
