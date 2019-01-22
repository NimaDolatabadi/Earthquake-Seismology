CSTART********************************************************************
C                    BGS/GSRG Computer Unit                              *
C                                                                        *
C      System  : SEISAN                                                  *
C      Name    : FILEIO.INC                                              *
C      Purpose : File operations                                         *
C      Author  : J. A. Bolton                                            *
C      Date    : 23 November 1994                                        *
C      Version : V01                                                     *
C                                                                        *
CEND**********************************************************************
C
C    ==========================
C    Define File operations....
C    ==========================
C
C    Function keys for file operations are composites based on one or
C    more of the following sub-function keys (one from each group).
C
C    Basic functions are...
C    ======================
C
      INTEGER*4    OPEN$,                   ! Open file.
     &             CLOSE$,                  ! Close file.
     &             READ$,                   ! Read record.
     &             READLAST$,               ! Read last record.
     &             WRITE$,                  ! Write record.
     &             DELETE$,                 ! Delete file.
     &             BACKSPACE$,              ! Back a record.
     &             REWIND$                  ! Rewind the file.
C
C    Function qualifier keys are...
C    ==============================
C
      INTEGER*4    NEW$,                    ! New file (default).
     &             OLD$,                    ! Old file.
     &             UNKNOWN$,                ! Unknown status of file.
     &             SCRATCH$,                ! Scratch file.
     &             ALL$,                    ! All files.
     &             FORT$,                   ! Force fortran I/O.
     &             DIRECTORY$               ! Directory name.
C
C    Condition directives are...
C    ===========================
C
      INTEGER*4    STOP$,                   ! Halt routine on error (default).
     &             WARN$,                   ! Warn of error, do not stop.
     &             IGNORE$,                 ! Ignore error, no message.
     &             CHECK$                   ! Check existance.
C
C    Parameters for function operations...
C    =====================================
C 
      PARAMETER (OPEN$      = 0)              ! & values.
      PARAMETER (CLOSE$     = OPEN$      + 1) !
      PARAMETER (READ$      = CLOSE$     + 1) !
      PARAMETER (READLAST$  = READ$      + 1) !
      PARAMETER (WRITE$     = READLAST$  + 1) !
      PARAMETER (DELETE$    = WRITE$     + 1) !
      PARAMETER (BACKSPACE$ = DELETE$    + 1) !
      PARAMETER (REWIND$    = BACKSPACE$ + 1) !
C    
      PARAMETER (NEW$       = 0)              ! Ditto.
      PARAMETER (OLD$       = 32)             !
      PARAMETER (UNKNOWN$   = OLD$       * 2) !
      PARAMETER (SCRATCH$   = UNKNOWN$   * 2) !
      PARAMETER (ALL$       = SCRATCH$   * 2) !
      PARAMETER (FORT$      = ALL$       * 2) !
      PARAMETER (DIRECTORY$ = FORT$      * 2) !
C
      PARAMETER (STOP$      = 0)              ! Ditto.
      PARAMETER (WARN$      = 64 * 64)        !
      PARAMETER (IGNORE$    = WARN$      * 2) !
      PARAMETER (CHECK$     = IGNORE$    * 2) !
C
C******************** End of definitions *************************
CSTART********************************************************************
C                    BGS/GSRG Computer Unit                              *
C                                                                        *
C      System  : SEISAN                                                  *
C      Name    : ERROR.INC                                               *
C      Purpose : Error codes                                             *
C      Author  : J. A. Bolton                                            *
C      Date    : 23 November 1994                                        *
C      Version : V01                                                     *
C                                                                        *
CEND**********************************************************************
C
C    Error definitions...
C    ====================
C
      INTEGER    E_OK$,                    ! No error condition.
     &           E_CONV$,                  ! Unable to convert.
     &           E_INIT$,                  ! Bad initialisation.
     &           E_CDER$,                  ! System command error.
     &           E_INVO$,                  ! Invalid option.
     &           E_RDWR$,                  ! Read-write error.
     &           E_INDX$,                  ! Unable to open index file.
     &           E_PEOF$,                  ! Premature end of file.
     &           E_MISF$,                  ! Missing file.
     &           E_DNOM$,                  ! Invalid denominator value.
     &           E_LAST$                   ! *** must be last ***
      PARAMETER (E_OK$   = 0)              ! & value.
      PARAMETER (E_CONV$ = E_OK$   + 1)    ! Ditto.
      PARAMETER (E_INIT$ = E_CONV$ + 1)    ! Ditto.
      PARAMETER (E_CDER$ = E_INIT$ + 1)    ! Ditto.
      PARAMETER (E_INVO$ = E_CDER$ + 1)    ! Ditto.
      PARAMETER (E_RDWR$ = E_INVO$ + 1)    ! Ditto.
      PARAMETER (E_INDX$ = E_RDWR$ + 1)    ! Ditto.
      PARAMETER (E_PEOF$ = E_INDX$ + 1)    ! Ditto.
      PARAMETER (E_MISF$ = E_PEOF$ + 1)    ! Ditto.
      PARAMETER (E_DNOM$ = E_MISF$ + 1)    ! Ditto.
      PARAMETER (E_LAST$ = E_DNOM$)        ! *** must be last ***
C
C******************** End of definitions *************************
CSTART***************************************************************
C                                                                   *
C                    BGS/GSRG Computer Unit                         *
C                                                                   *
C      System  : SEISAN                                             *
C      Name    : FORTIO.INC                                         *
C      Purpose : File open data definitions                         * 
C      Author  : J. A. Bolton                                       *
C      Date    : 23 November 1994                                   *
C      Version : V01                                                *
C                                                                   *
CEND*****************************************************************
C
C     ========================= 
C     File open information...
C     ========================
C
      INTEGER*4  F_MAXU$,                 ! Max units in system considered.
     &           F_STATC$,                ! File status in characters.
     &           F_ACCSC$,                ! File access type.
     &           F_FORMC$,                ! Format type.
     &           F_ACTNC$                 ! Read or write only action
C
      PARAMETER (F_MAXU$  = 64)           ! Max legal unit plus 1.
C                                         ! (not including error unit 0)
      PARAMETER (F_STATC$ = 7)            ! & values.
      PARAMETER (F_ACCSC$ = 10)           !
      PARAMETER (F_FORMC$ = 11)           !
      PARAMETER (F_ACTNC$ = 5)            !
C
C    ==================================================
C    General information & protected device numbers....
C    ==================================================
C
      INTEGER*4    STD_IN$,                   ! Input unit.
     &             STD_OUT$,                  ! Output unit.
     &             DBG_OUT$,                  ! Debug unit.
     &             F_LEN$,                    ! File name length.
     &             F_MAX$                     ! Maximum # protected units.
      PARAMETER    (DBG_OUT$ = 4)             ! Set for debug.
      PARAMETER    (STD_IN$  = 5)             ! Set for input.
      PARAMETER    (STD_OUT$ = 6)             ! Set for output.
      PARAMETER    (F_LEN$   = 60)            ! Filename length.
      PARAMETER    (F_MAX$   = 2)             ! Maximum # of protected units.
C
      CHARACTER    CHR_SET_ACTION$*(F_ACTNC$) ! & action.
      PARAMETER   (CHR_SET_ACTION$= '     ')  ! Ditto.    
C
C      File information...
C      ===================
C
      INTEGER*4    F_UNIT$,                   ! Last file open unit.
     &             F_RECL$                    ! Dam file record length.
      CHARACTER    CHR_F_STAT$   *(F_STATC$), ! File status.
     &             CHR_F_ACCESS$ *(F_ACCSC$), ! File access type.
     &             CHR_F_FORM$   *(F_FORMC$), ! Format type.
     &             CHR_F_ACTION$ *(F_ACTNC$)  ! Read or write only action
      LOGICAL*4    B_F_EXIST$,                ! File exists?.
     &             B_F_DEBUG$,                ! Debug option?.
     &             B_FILENR$                  ! Filenr.lis exists & usage?.
C
C    Flag for PC graphics mode...
C    ============================
C
      LOGICAL      B_F_GRAPH$                 ! In graphics mode?
     &            ,B_F_PC$                    ! PC platform?.
     &            ,B_F_MESSAGE$               ! Issue a message?
C
C    Protected device numbers.....
C    =============================
C
      INTEGER*4    F_PROTECT$(F_MAX$)         ! & array.
C
C    Input comman file unit and output parameter file units...
C    ---------------------------------------------------------
C    For batch stream or standard input..
C    ------------------------------------
C
      INTEGER*4    CMDUNIT$,                  ! Command file unit.
     &             PARUNIT$                   ! Parameter file unit.
C
C    Debug file...
C    -------------
C
      INTEGER*4    DBGUNIT$                   ! Debug file unit.
      CHARACTER    CHR_DBG_FILE$*(F_LEN$)     ! & filename.
C
C      File COMMON.....
C      ================
C
      COMMON / FIL_COM$ /
C
     &             B_F_EXIST$                ! File exists?.
     &            ,B_F_DEBUG$                ! Debug opyion?.
     &            ,B_F_GRAPH$                ! In graphics?.
     &            ,B_F_PC$                   ! PC platform?.
     &            ,B_F_MESSAGE$              ! Issue a message?.
     &            ,B_FILENR$                 ! Filenr.lis exists & usage?.
     &            ,CMDUNIT$                  ! Command file unit.
     &            ,PARUNIT$                  ! Parameter file unit.
     &            ,DBGUNIT$                  ! Debug file unit.
     &            ,F_UNIT$                   ! Last file open unit.
     &            ,F_RECL$                   ! Dam file record length.
     &            ,F_PROTECT$                ! Protected file units.
     &            ,CHR_F_STAT$               ! File status.
     &            ,CHR_F_ACCESS$             ! File access type.
     &            ,CHR_F_FORM$               ! Format type.
     &            ,CHR_F_ACTION$             ! Read or write only actions
     &            ,CHR_DBG_FILE$             ! Debug file.
C
C    ====================
C    File unit mapping...
C    ====================
C
      INTEGER*4    MAP_C$                         ! Map size.
      PARAMETER   (MAP_C$ = F_MAXU$-1)            ! & value.
      CHARACTER    CHR_MAP$             *(MAP_C$),! Open file mapping.
     &             CHR_FILE_OPEN$(MAP_C$)*(F_LEN$)! & open files
      COMMON      /FIO_MAP$/CHR_MAP$,             ! Saved area.
     &             CHR_FILE_OPEN$                 !
C
C********************* End of data definitions *********************
C
CSTART*************************************************************************
C                                                                             *
C                    BGS/GSRG Applications Programming Unit                   *
C                                                                             *
C      Name    : Special functions and operations.                            *
C      Author  : J. A. Bolton                                                 *
C      Date    : 6 December 1994                                              *
C      Version : V01                                                          *
C                                                                             *
C      Changes : Add symbol special function (filling)                        *
C      Author  : J. A. Bolton                                                 *
C      Date    : 1 June 1995                                                  *
C      Version : V02                                                          *
C                                                                             *
CEND***************************************************************************
C
C    Special functions...
C    ====================
C
      INTEGER      FILL$                         ! Flag to fill symbol.
      PARAMETER   (FILL$ = 128)                  ! & value.
C
C    Variables for decode COMMON...
C    ==============================
C
      INTEGER      ARRAY_N$                       ! Array items.
c      PARAMETER   (ARRAY_N$ = 15)                 ! & value.
      PARAMETER   (ARRAY_N$ = 99)                 ! & value.
      REAL         ARRAY$( ARRAY_N$ )             ! Decode array.
C
C    Named common area...
C    ====================
C
      COMMON / DECODE_COM$ / ARRAY$               !
C
C********************* End of special functions *************
C
CSTART************************************************************************* 
C                                                                             *
C                    BGS/GSRG Applications Programming Unit                   *
C                                                                             * 
C      Name    : SEISERR Error data definitions                               * 
C      Author  : J. A. Bolton                                                 * 
C      Date    : 23 November 1994                                             *
C      Version : V01                                                          * 
C                                                                             *
CEND*************************************************************************** 
C                                                                               
C    Variables for Error COMMON...                                              
C    =============================                                              
C                                        
      INTEGER*4    ERR_TOT$                       ! Total # error fields.
      PARAMETER   (ERR_TOT$ = 2 )                 ! Local & system.
      INTEGER*4    ERROR$(ERR_TOT$)               ! & array.                  
C                                  
C    Variables for error message common...
C    =====================================                                      
C                     
      INTEGER*4    ERR_MC$,                         ! Error message length.
     &             ERR_LEN$                         ! Local message length.
      PARAMETER   (ERR_MC$ = 60)                    ! & value.
C
      CHARACTER    CHR_ERR_TXT$(E_LAST$)*(ERR_MC$), ! Strings.
     &             CHR_ERR_MSG$         *(ERR_MC$)  ! & current string.         
C
C    Variables for Unrecoverable forced errors....
C    =============================================
C
      INTEGER*4     E_FAIL_MX$                      ! Maximum # available.
      PARAMETER    (E_FAIL_MX$ = 20)                ! & current value.
      INTEGER*4     E_FAIL$(E_FAIL_MX$)             ! & array.
C
C    Named common area...
C    ====================
C
      COMMON / ERR_COM$ / ERROR$,                   ! Local & system errors.    
     &                    E_FAIL$,                  ! List of unrecoverables.   
     &                    CHR_ERR_MSG$, ERR_LEN$,   ! Local message & length.
     &                    CHR_ERR_TXT$              ! Local message texts.
C
C********************* End of error data definitions *************
C
