c*************************************************************************
C      PROGRAM SELECT
c
c
c  arguments: If only one argument, it is name of select.inp parameter
c             file. If several arguments (at least 3), all inputs via
c             arguments. The arguments are:
c 
c             -base   : 5 letter data base 
c             -seisweb: if set, input from seisweb, output to seisweb
c             -time   : time interval
c             -web_out: complete path to where data is placed, only
c                        active if seisweb set. 3 filers made: 
c                       web_out.id  : id's, like index.out without # 
c                       web_out.all : like select.out
c                       web_out.head: header lines
c             -area   : lat-lon grid, minlat,maxlat,minlon,maxlon
c             -depth  : depth range, mindepth,maxdepth
c             -mag    : magnitude range, minmag,maxmag
c             -nstat  : range of number of stations, min,max
c             -gap    : range of gap, min,max
c             -rms    : range of rms, min,max
c             -mgtypes  : up to 5 mag types, one string
c             -disttype : -------dist------------------
c             -eventtype: ------event------------------
  
C
C     WRITTEN BY:   C. LINDHOLM
C     MODIFIED BY : J. HAVSKOV
C     LAST REVISED: AUGUST 18, 1987
C     LAST REVISED: DEC 20, 1988.  POSSIBILITY TO RUN ON COMPACT FILES INCLUDED
C     LAST REVISED: jan 6, 1988.  POSSIBILITY TO RUN EVENTS WITH
C                                 DISTANCEID=BLANK
C     LAST REVISED  APRIL 19 -89  POSSIBILITY RUN ON DATABASE INCLUDED
C
C                   1990-01-30,R.N.ARVIDSSON, TO PC-DOS
C     LAST REVISED  1990-01-30,RNA, COMMON BLOCK STRUCTURE CHANGED BECAUSE
C                                   IT IS NOT POSSIBLE TO MIX CHARACTERS
C                                   WITH INTEGER TYPES.
C                   1990-02-16,RNA, COMMON BLOCK STRUCTURE AGAIN THE SAME
c                                   PROBLEM.
C                   1990-02-22,RNA, FILENR.LIS OPTION REMOVED
C                   JUL 27, 90, JH, MAJOR REVISION
C                   AUG 2   90  J.H BUGS
C                       6           BUGS
C                      17           REMOVE REFERENCE TO DCL
c                      24           back to vax, card from 120 to 140 chars
C                   OCT 18  90   J.H: MULTIPLE DATA BASE OPTION
c                   nov 19  90      : I5 for event number in index file
c                   sep 19  91   jh : include call to topdirectory
c                   oct 07          : pc adoption, see vax comments, 12 char time
c                                     main data base is rea not base
c                   nov 4           : findev.. to find_ev...
c                   nov 24          : new version of indata, some clean up
C                   AUG 18, 92    JH: NEW SELECT ON EVENT TYPE
c                   sep 15 92       : select on earthquakes (q) and magnitude type
c                   oct 7,12        : bug with magnitude selection
C                   OCT 29          : ADD OUTPUT OF WAVEFORM FILE NAMES
c                   oct6 30         : bug
c                   nov 15          : check for wrong header lines in episel
C                   MAR 25, 93      : BUG WITH MAG SELECTION
c                   jul 93 by jh    : version 3.0*******************************
c                                     REA to agency, EPI to CAT
c                   aug 20          : bug in episelect when one file only
c                                   : filenames to 80
c                                   : bug with station selection
c                   sep 10          : bug with filnr.lis on pc, put null char
c                   sep 17          : bug wirht index file open, more info
c                       18          : do not select non locate deqs, select no mags
c                       22          ; neg rms means larger than for rms
c                   dec 29          : split question about nag and rms
c                   mar 2, 94       : bug when reading mag-rms limits
c                   mar 21 94       : make filenr.lis in local dir
c                   may 31 94       : write out number of events selected, each
c                                     new data base file
c                   nov 07,94 by Carlos Eduardo Vieira
c                                     routine SELCRIT changed allowing
c                                     choose which defaults are going to be
c                                     changed using menu (routine DRAW_SCREEN)
C!JAB(BGS)Nov94                     : Installed file & error handling (libsei)
c
c                  dec 94           : ********** version 5.0 ****************
c                  dec 13           : last check
c                  jan 11   95 jh   : new call to get base
c                  feb 7            : also use lolat and lolon in def
c                  feb16            : make index file also when searching in
c                                     a file
c                  apr 5    95 jh   : fix selection when no magnitude type
c                  may 8            : bog in above
c                  may 11   95      : select any event types
c                  may 15           : use actual number of station if event file
c                                     instead of number in header
c                  dec 7 95 jh      : if selecting in a file, do not stop if
c                                     data is not in chronological order
c                  feb 8 96         : possible to select in all header lines
c                  feb 16           : fix error limits for hypocneter errors,
c                  feb 20           : put in waveform file select
c                  mar 4 96         : bug
c                  june 10, 96      : do not use event type to make file name
c                                     from a second header line
c                  oct 25, 96       : stop if no data in data base
c                  dec 96           : do not use filenr.lis
c                  feb 27   97      : bug in above
c                  mar 25           : bug in magnitude selection
c                  jan 13, 98       : use century as default,  ***year 2000***
c                                     and a few more improvements
c------------------------------------------------------------------------------
c                  oct-nov 98 by jh :  version 7.0 check, commands input file
c                  jan 99 by bmt    :  initialization of magtypest,disttypet,eventtypet 
c                  jun 16 99 jh     : select.cmd to selec.inp
c                  aug 18,99        :  selection for gap
c                  aug 22,99        : selection for epicenter distance
c                  sep 7            : bug with gap selection
c                  sep 14           : bug with magnitude agency in select.inp
c                                     hypocenter agency
c
c                  sep 22           : bug with start in year 2000
c                  oct 18           : for polarity selection, only select if
c                                     polarity is for P, do not use + and -
c                  oct 21           : century selection wrong, combinaitons
c                                     gap and other not exclusive
c jan 13, 2000 jh : another year 2000 problem, end date not found if after
c                   year 2000, thanks fernando
c mar 13       jh : input with prompt
c mar 22            more of the above
c may 11       jh : always stop if wrong time interval
c nov 8           : do not write out in index file if input form a file,
c                   allow filenames shorter than 6 if containing a .
c jan 10, 2001 lo : init seisweb, since true on Linux
c mar 5   2001 jh : distance to real
c mar 08  2001 lo : change in default area setting
c apr 30  2001 jh : select by phase, individually and by station
c may 10          : distce acan be either epicentral or hypocentral
c may 25          : select did not work with negative magnitudes !!!!!!!
c jun 25          : comment change
c jul 5           : fix so distance type and mag types do not cance each
c                   other, seems to only be a display problem
c nov 7   2001 bm : now can use output file given in the prompt line after 
c                   given "select.inp" (make same output like SEISWEB command)
c                   e.i. select  select.inp  select.out
c nov 7   2001 bm : clear the output file when nothing is found
c nov 23       jh : bug: station selection did not non located events
c mar20           : option to use s-files
c apr 15          : bug in above, did not work with select.inp
c aug 22  2002 jh : select stations, even when all have weight 4
c sep 12  2002 lo : select volcanic sub-classes
c apr 22  2003 jh : small format change
c may 27          : wavefor.out to waveform_names.out
c sep 11  2003 jh : increase fild for number of evets
c feb 20  2007 jh : header_check was not initilized so all headers were
c                   always checked
c sep 30  2008 jh : blank input of time is now between 1980 and 2015
c jan 4   2009 jh : accep magntude types b and s
c sep 14  2009 jh : change question for stat comp
c sep 23  2010 jh : add option to select fps solution for quality
c feb 25  2011 jh : remove  s as a valid event ID to make it possible
c                   so search for small s
c feb 1   2012 jh : option CODAQ for station to make input files for codaq
c                   err check for reading distance
c feb 29  2012 jh: more error check
c mar 20  2012 jh: fix errors with fps quality selection
c nov 27  2012 jh: abs time from year 0000 so normal operation to 0000
c dec 12  2013 jh: make it work across the data line, start year was one day wrong if
c                  no month and day given
c mar 24  2014 jh; also select components when using CODAQ option
c 2015-12-07 pv  : fixed problem with reading Fault plane solution from select.inp
c feb 10 2016  jh: change def endtime from 2015 to 2020
c jan 25 2017  jh: Include min number of station in CODQ option. can be used
c                  to select events with a minimum number of stations in a given
c                  distance range
c
      IMPLICIT NONE
      include 'seidim.inc'        ! dimensions
      integer seiclen             ! function
      CHARACTER*1 DISTANCEID,EXP,EVENTID,DEPTHID,MAGTYPES(5),
     *DISTTYPE(5),EVENTTYPE(5),magtype
      character*6 volcanictype(10)   ! volcanic subclasses
      character*3 AGENCY,MAGAGENCY
      CHARACTER*80 DATA(max_data)
      character*80 web_out           ! path and file name for web output
      character*6 basetype           ! type of base CAT or sfile
      logical seisweb                ! true if call from seisweb
      integer narg                   ! number fo arguments
      INTEGER NRECORD,NHEAD
c-- see subroutine find...       
      integer		status,new_month,fstart,event_no 
c-- select key                                       
      character*10     key
c-- event file name                                   
      character*80	evfile
      character*80 choise_file       ! file with choises, none if blank
      integer max_cat_files          ! maximum number of cat files
      real dist                      ! epicentral distance
      parameter(max_cat_files=5000)
      character*80 cat_files(max_cat_files)   ! cat file nemes
      integer ncat_files             ! number of cat_files
C  NUMBER OF STATIONS TO BE PRESENT AND number of stations for event
      INTEGER		stat_present,NSTAT
      logical       all_stat         ! if true,  all must be present
c  number of hypocenter agencies present
      integer           hypa_present
c  number of magnitude agencies present
      integer           maga_present
C  NUMBER OF PHASES FOR ONE EVENT
      INTEGER NPHASE	  
c  id line number
      integer id
C  1: EVENT CONTAIN REQ. STATION, 0: NO
      INTEGER		STATIONOK	
C  STATIONS CODES TO SELCT BY
      CHARACTER*7	STATSELECT(50)
      character*4       stat_phase(50)  ! phases to select with stations
      integer mindist(50),maxdist(50)   ! distance range for station 
      logical hyp_dist                  ! if true, use hypocentral distance
c  hypocenter agencies
      character*3       hypaselect(10)
c  magnitude agencies
      character*3       magaselect(10)
C  ONE STATION CODE and component
      CHARACTER*7	STATIONCODE	
C  INPUT FILE NAME IN DATA BASE 
      CHARACTER*80	SFILE
C  Name of database
      CHARACTER*80	basename
      character*5 agency_loc
c  first and last file in data base
C  starttime for processing data
      CHARACTER*14      starttime
c  century
      CHARACTER*14	endtime
C  NUMBER OF EVENTS IN FILES
      INTEGER		NEVENT		
C  NUMBER OF LOCAL EVENTS IN FILES
      INTEGER		NLOCAL		
C  NUMBER OF REGIONAL EVENTS IN FILES
      INTEGER		NREGIONAL	
C  NUMBER OF DISTANT EVENTS IN FILES
      INTEGER		NDISTANT	
C  NUMBER OF LOC. AND REG. EVENTS SELECTED
      INTEGER           NOUT		
C  NUMBER OF WAVEFORM FILES
      INTEGER NWAVE
c  number of indexes out
      integer nid
c  end and start of time indicator 
      logical end_of_time,start_of_time
c  logical to indicate compact file  
      logical compact
c  logical to indicate fault plane solution found
      logical fpsolutf
c  logical to indicate earthquake felt found
      logical eqfeltf
c  logical to indicate hypocenter error latitude found
      logical helatf
c  logical to indicate hypocenter error longitude found
      logical helonf
c  logical to indicate hypocenter error depth found
      logical hedepthf
c  variable reading from the file
      real filevar
C  OPTION OF INPUT
      INTEGER           inputoption
C  COUNTERS
      integer		j,i,k,n
C  SELECTION CRITERIAS: LATITUDE, LONGITUDE, MAGNITUDE, DEPTH, MAX AND MIN RMS
C                       MAX AND MIN HYPOCENTER ERROR LAT, LONG AND DEPTH
      REAL MAG, HILAT,LOLAT,HILON,LOLON,MAXMAG,MINDEPTH,MAXDEPTH,
     *     maxrms,minrms,maxgap,mingap,
     *     minhelat, maxhelat,
     *     minhelon, maxhelon,
     *     minhedepth, maxhedepth
      integer igap   ! gap read from file
     
C  SELECTION CRITERIAS: TIME INTERVAL,MIN AND MAX # OF STATIONS
      INTEGER MINSTATION, MAXSTATION 
      double precision time1,time2,timcen
C  STATUS PARAMETER, EPIBASE
      INTEGER nstatus
C  EVENT ACCEPTED OR NOT
      LOGICAL  ACCEPTED,default
C  DATE,TIME AND NUMBER OF STAITONS
      integer YEAR,MONTH,DAY,HOUR,MINUTES,NUMBEROFSTATIONS
C  SECOND, LATITUDE,LONGITUDE,DEPTH, TIME, MINUMUM MAG AND RMS
      real    SEC,LATI,LONG,DEPTH,minmag,rmsvalue
      integer isec
c  top directory name
      character*60 top_directory
c   directory delimiter
      character*1 dchar
c   text
      character*80 text
c   dir name for EPI now cat
      character*80 dir
c   fault plane solution
      logical fpsolut
c   earthquake felt
      logical eqfelt
      logical header_check       ! if or not all header lines checked
      logical wave               ! true if correct wave form file name found
      character*40 wave_check    ! string to identify wave from file name
      integer n_header_check     ! number of header lines to check
      real ypol(max_polyn$,2)    ! array defining polygon
      integer iant               ! number of points of the polygon
      integer mnupol             ! minumum number of polarities
      integer cnupol             ! counter for number of polarities
      character*4 phase(6)       ! phase names to select
      logical singleout          ! true if output file is given in the command line
      character*80 outfile       ! output file  name given in the command line
      character*5  fpquality     ! quality of fps solution
      integer ncoda              ! number of stations for codaq
      character*5 coda_stat(200) ! stations for codaq

c
      COMMON/EPIVALUE/YEAR,MONTH,DAY,HOUR,MINUTES,SEC,LATI,LONG,DEPTH,
     *                 NUMBEROFSTATIONS,RMSVALUE,MAG,ACCEPTED,nstat
C
      COMMON/EPICHAR/DISTANCEID,EVENTID,DEPTHID,AGENCY,
     *                    MAGTYPE,MAGAGENCY
C
      common/epicrite/hilat,lolat,hilon,
     *                lolon,minmag,maxmag,minrms,maxrms,mindepth,
     *                maxdepth,minhelat,maxhelat,minhelon,maxhelon,
     *                minhedepth,maxhedepth,ypol,iant,mnupol
      common/epicrit2/fpsolut,eqfelt,statselect
      common/epicrit1/mindist,maxdist,
     *                hypaselect,magaselect 
      common/epicrit3/default,eventtype,disttype,magtypes,volcanictype
      common/epicrit4/minstation,maxstation,stat_present,hypa_present,
     *                maga_present,header_check,wave_check
      common/epicrit5/time1,time2,timcen,all_stat,mingap,maxgap,phase,
     *                stat_phase,hyp_dist,fpquality
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
       integer  write1,                    ! Output unit 1.
     &          write3,                    ! Ditto 3.
     &          write4,                    ! Ditto 4.
     &          write5,                    ! Ditto 4.
     &          read1,                     ! Input unit1.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
C
C    ============= end of list ==========
C


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      b_f_debug$=.false.                   ! file io debugging
C
C   INITIALIZE 
C
      NEVENT=0
      NLOCAL=0
      NREGIONAL=0
      NDISTANT=0
      NOUT=0
      NWAVE=0
      nid=0
      basetype='CAT'
      do i=1,6
         phase(i)=' '
      enddo
      compact=.false.
      seisweb=.false.
      singleout=.false.
      header_check=.false.      ! 2-2007 jh
      wave_check=' '
      hyp_dist=.false.
      call dir_char(dchar)
      call get_def_base(agency_loc)

C   USE STANDARD DATA BASE BY DEFAULT
C
      BASENAME(1:10)='          ' 
c
c   check if prompt input with file name of input file with choises
c
      choise_file=' '
c
      call get_arguments(narg,data)
c
c     check if  output file is given
c
      if(narg.eq.2) then
        outfile = data(2)
        singleout = .true.
        narg = 1
      endif
c
      if(narg.eq.1) then
         choise_file=data(1)
         write(6,'(a,a)')' Choises from file: ',choise_file
         open(1,file=choise_file,status='old',err=3334)
         goto 3335
 3334    continue
         write(6,'(a,a)')' File does not exist: ',choise_file
         stop
 3335    continue
         read(1,'(32x,a)') basename
         read(1,'(32x,a)') basetype
         close(1)
      endif
c
c   find out if all choises from promt line and get them if true
c
      call selcrit_prompt
     *(narg,data,starttime,endtime,basename,seisweb,
     * web_out)


c
c     INQUIRE TYPE OF INPUT if not from file or argument string
C
 435  CONTINUE
      if(choise_file.eq.' '.and.narg.lt.3) then
         WRITE(*,*)' POSSIBLE INPUT IS: '
         WRITE(*,*)'    STANDARD CAT DATA BASE: RETURN'
         WRITE(*,*)'    ALTERNATIVE DATA BASE, GIVE 1-5 LETTER CODE:'                
         WRITE(*,*)'    FILENAME FOR ONE FILE, ',
     *              'MUST BE 6 OR MORE CHARACTERS OR HAVE A .'
         READ(5,'(A)') BASENAME
c
c   do not ask for type of base if input is a file
c
         i=index(basename,' ')-1
         j=index(basename,'.')
         if(.not.(I.GT.5.or.j.ne.0)) then
            write(6,*) ' Type of base: CAT (Return) or Sfiles (s):'
            read(5,'(a)') text
            if(text(1:1).eq.'s'.or.text(1:1).eq.'S') 
     *      basetype='SFILES'
         endif
      endif
C
C  DETERMINE TYPE OF INPUT
C
      if(.not.seisweb) then
      i=index(basename,' ')-1
      j=index(basename,'.')
      IF(BASENAME(1:5).EQ.'     ') THEN
         INPUTOPTION=1
         BASENAME(1:5)=agency_loc
      ENDIF
c
      IF(I.lt.6.and.j.eq.0) then
         INPUTOPTION=1          ! data base
         do k=2,5
           if(basename(k:k).eq.' ') basename(k:k)='_'
         enddo
      endif
      IF(I.GT.5.or.j.ne.0) INPUTOPTION=2  ! file

c
c   set type of base
c
      if(inputoption.eq.1.and.basetype.eq.'SFILES') inputoption=3
c
C   OPEN FILE
C
      IF(INPUTOPTION.EQ.2) THEN
          call sei open( old$+warn$,       ! Open file & warn of erro.
     &                   ' ',              ! Prompt (n/a).
     &                   BASENAME,         ! Filename.
     &                   read1,            ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
          if( code .ne. e_ok$ ) then       ! An error or does not exist.
             code = e_ok$                     ! Re-set condition.
             if(choise_file.eq.' ') then
                goto 435                         ! & re-prompt.
             else
                write(6,'(a,a)') ' File does not exist: ',basename
                stop                          ! stop if input from choise file
             endif                           !
           endif
      ENDIF
      else
c         i=seiclen(basename)
c         top_directory=basename(1:i-9)   ! put in path
c         basename(1:5)=basename(i-4:i)   ! put in base name
c         basename(6:80)=' '
c         call put_env_seistop(top_directory) ! put in environment
         inputoption=1      ! only choise from seisweb
c         write(6,'(0x,a24)')'Content-type: text/plain' ! needed for seisweb'
c         write(6,'(0x,a0)') ''     
      endif

c
c   data base indexing
c
      IF(INPUTOPTION.EQ.1) THEN
c
         write(6,'(a,a)')' Updating data base ',basename(1:5)
c
c   get top dir name and make cat dir name
c
         call topdir(top_directory)
         i=index(top_directory,' ')-1
         dir(1:i)=top_directory(1:i)
         dir(i+1:i+1)=dchar
         dir(i+2:i+4)='REA'
         dir(i+5:i+5)=dchar
         dir(i+6:i+10)=basename(1:5)
         dir(i+11:i+11)=dchar
         dir(i+12:i+14)='CAT'
c
c   make listing of files in  CAT
c
         call getfiles(dir,i+14,cat_files,max_cat_files,n)
c
         k=0
c
c   sort out non cat files
c
         do i=1,n
            if(cat_files(i)(7:10).eq.'.CAT') then
               k=k+1
               cat_files(k)=cat_files(i)
            endif
         enddo
c
c   stop if no files
c
         if(k.lt.1) then
           write(6,'(a,a)')
     *     ' No data in data base specified: ', basename(1:5)
           if(seisweb) write(6,*)
     *     'select_terminated: error no data in base'
           stop
         endif
c
         write(6,'(a,a5,a,i4,a)')
     *' The data base ',basename(1:5),' has ',k,' files'
         write(6,'(a,a)')' The first file starts: ',cat_files(1)(1:6) 
         write(6,'(a,a)')' The  last file starts: ',cat_files(k)(1:6)
         write(6,*)
         ncat_files=k
      ENDIF
c
c   open output files
c
      if(singleout) then
        seisweb=.true.
        web_out = outfile
      endif

      if(seisweb) then

           call sei open( unknown$,        ! Open file (default stop on error).
     &               ' ',                  ! Prompt (n/a).
     &               web_out(1:seiclen(web_out))//'.all',  ! Filename.
     &               write1,               ! Unit opened on.
     &               b_flag,               ! Flag (n/a).
     &               code )                ! Returned condition (n/a).
           endfile write1                  ! need to be clear up
           rewind write1
         call sei open( unknown$,         ! Open file (default=stop on error)
     &                  ' ',              ! Prompt (n/a).
     &       web_out(1:seiclen(web_out))//'.id',      
     &                  write3,           ! Unit opened on.
     &                  b_flag,           ! Flag (n/a).
     &                  code )            ! Returned condition (n/a).
           endfile write3                 ! need to be clear up
           rewind write3
c
c   file with header lines
c
         call sei open( unknown$,         ! Open file (default=stop on error)
     &                  ' ',              ! Prompt (n/a).
     &       web_out(1:seiclen(web_out))//'.head',      
     &                  write5,           ! Unit opened on.
     &                  b_flag,           ! Flag (n/a).
     &                  code )            ! Returned condition (n/a).
           endfile write5                 ! need to be clear up
           rewind write5
      else
C
             call sei open( unknown$,      ! Open file (default stop on error).
     &               ' ',                  ! Prompt (n/a).
     &               'select.out',         ! Filename.
     &               write1,               ! Unit opened on.
     &               b_flag,               ! Flag (n/a).
     &               code )                ! Returned condition (n/a).
c
c   open and make sure it is empty
c
             call sei open( unknown$,      ! Open file (default=stop on error)
     &                  ' ',              ! Prompt (n/a).
     &                  'index.out',      ! Filename.
     &                  write3,           ! Unit opened on.
     &                  b_flag,           ! Flag (n/a).
     &                  code )            ! Returned condition (n/a).
             call sei close(delete$,write3,code)  ! delete
             call sei open( unknown$,      ! Open file (default=stop on error)
     &                  ' ',              ! Prompt (n/a).
     &                  'index.out',      ! Filename.
     &                  write3,           ! Unit opened on.
     &                  b_flag,           ! Flag (n/a).
     &                  code )            ! Returned condition (n/a).
         open(90,file='index.codaq',status='unknown')
      endif

C
C   GET SELECTON CRITERIA
C

         CALL SELCRIT(STARTTIME,ENDTIME,basename,basetype,
     *   choise_file,narg)
c
c   open file with waveform file names
c
       call sei open( unknown$,         ! Open file (default=stop on error)
     &                ' ',              ! Prompt (n/a).
     &                'waveform_names.out',   ! Filename.
     &                write4,           ! Unit opened on.
     &                b_flag,           ! Flag (n/a).
     &                code )            ! Returned condition (n/a).
C
C     INPUT IS NORDIC COMPACT FILE OR NORDIC FILE
C
      if(inputoption.eq.2) then
c
c   find if compact or not
c
          call nortype(read1,compact)
          if(compact) THEN
             write(6,*)
             write(6,*)' File type is compact'
             write(6,*)
          endif
          GOTO 10
      endif


C---------------------------------------------------------------------
C  MAIN READING LOOP STARTS HERE 
C---------------------------------------------------------------------
c
10    CONTINUE
      nstat=0
C
C    CAT DATA BASE
C
      IF(INPUTOPTION.EQ.1) THEN
         call epibase(basename,starttime,endtime,
     *   nstat,nphase,nhead,nrecord,distanceid,exp,data,id,nstatus,
     *   compact,nout,ncat_files,cat_files)
         IF(NSTATUS.EQ.1) GOTO 999
      ENDIF	  
C
C   single file
C
      IF(INPUTOPTION.EQ.2)THEN	
         if(compact) then
            nhead=1
            read(read1,'(a80)',iostat=code ) data(1) ! Read record.
            call sei code(fort$,                     ! Stop on error.
     &                    code,                      ! Condition code.
     &                    read1,                     ! For fortran unit.
     &                    b_flag )                   ! Flag end of file.
            if( b_flag ) goto 999                    ! End of file, skip.
            DISTANCEID=DATA(1)(22:22)
            NRECORD=1
         else
            CALL INDATA(read1,nstat,NPHASE,NHEAD,NRECORD, !
     *      DISTANCEID,EXP,DATA,ID)
c            write(6,*)DATA(1)
            if(nrecord.eq.0) goto 999
         endif
      ENDIF

c
c  s-file data base
c

      if(inputoption.eq.3) then
         key=' '        ! always use next event
         CALL findevin                                                       
     *   (basename,starttime,endtime,key,0,
     *   event_no,evfile,fstart,new_month,status)                                 
         if(status.eq.0) then                                                     
            nrecord=-1
            open(97,file=evfile,status='unknown')
            call indata(97,nstat,nphase,nhead,nrecord,
     *      distanceid,exp,data,id)
            close(97)
         else
            goto 999
         endif
      endif
 
c
c	  
C------------------------------------------------------------------------
C   HERE STARTS EVENT CHECKING
C------------------------------------------------------------------------
C
C   CHECK FOR INFO IN HEADERS
C
      data(1)(80:80)='1'       ! older data might not have a 1 in first line
      if(header_check) then    ! find if all header are checked
         n_header_check=nhead
      else
         n_header_check=1
      endif
      do i=1,n_header_check
         if(data(i)(80:80).eq.'1') then
            CALL EPISELEC(DATA(i),start_of_time,end_of_time)
            if(accepted) goto 6968    ! jump out of loop if selected
         endif
      enddo
 6968 continue
c
c   if end of time, no more data input unless a single file is used
c   and the data is mixed up chronologically
c
      if(end_of_time.and.inputoption.eq.2) goto 10
      if(end_of_time) goto 999
c
c   if not yet at start of time interval, get next event
c
      if(start_of_time) goto 10
C
C   EVENT COUNTING only for events within time interval
C
      NEVENT=NEVENT+1
C
C   COUNT LOCAL EVENTS
C	  			
      IF(DISTANCEID.EQ.'L'.OR.DISTANCEID.EQ.' ') THEN
          IF(DISTANCEID.EQ.'L') NLOCAL=NLOCAL+1
      ENDIF
C	  	  	
C  COUNT REGIONAL  EVENTS
C
      IF(DISTANCEID.EQ.'R') NREGIONAL=NREGIONAL+1	
C
C   COUNT DISTANT EVENTS
C
      IF(DISTANCEID.EQ.'D') NDISTANT=NDISTANT+1
c
c   check for gap
c
      if(accepted) then
      if(mingap.ne.0.0.or.maxgap.ne.360.0) then
         igap=-1                ! initailly no gap
         accepted=.false.
         if(.not.compact) then
            do i=1, nhead
              if(data(i)(80:80).eq.'E'.and.data(i)(2:4).eq.'GAP') 
     *           then
                 read(data(i)(6:8),'(i3)') igap
                 if(float(igap).le.maxgap.and.float(igap).ge.mingap)
     *           accepted=.true.
              endif
            enddo
         endif
      endif
      endif

c
c check for volcanic subclass
c
      if (accepted) then
        if (seiclen(volcanictype(1)).gt.0) then
          accepted=.false.
          do i=1,nhead
            if (data(i)(1:10).eq.' VOLC MAIN') then
              do j=1,10
                if (data(i)(12:17).eq.volcanictype(j)) then
                  accepted=.true.
                endif
              enddo          
            endif
          enddo
        endif
      endif

c-------------------------------------------------------
c   check if output for codaq
c-------------------------------------------------------

      IF(.not.compact.AND.STAT_present.GT.0.and.accepted) THEN
         if(statselect(1)(1:5).eq.'CODAQ') then
            ncoda=0
            DO J=NHEAD+1,NRECORD-1
c
c   select all stations less than given distance for codaq
c
              if(data(j)(71:75).ne.' ') then
                 read(data(j)(71:75),'(f5.0)',err=8485) dist
                 goto 8486
 8485            continue
                 write(6,*)'err reading dist'
                 write(6,'(a)') data(1)(1:78)
                 write(6,'(a)') data(j)(1:78)
                 dist=-1
                 write(6,*)' Continue (n/y=enter)'
                 read(5,'(a)') text
                 if(text.eq.' '.or.text(1:1).eq.'y') then
                    continue
                 else
                    stop
                 endif
 8486            continue
              else
                 dist=-1   ! indicate no distance available
              endif
c
c   if hypocentral distance, calculate here
c
              if(hyp_dist) then
c                write(88,*) dist,depth
                 if(dist.gt.-0.1) dist=sqrt(dist*dist+depth*depth)
c                write(88,*) dist
              endif

c
c  there must be a p or s-phase also
c
              if(dist.le.maxdist(1).and.dist.ge.mindist(1).and.
     *        (data(j)(11:11).eq.'P'.or.data(j)(11:11).eq.'S')) then
c
c   check that station has not been selected before
c
                 if(ncoda.gt.0) then
                    do i=1,ncoda
                       if(data(j)(2:6).eq.coda_stat(i)) goto 4848
                    enddo
                 endif
c
c   use station for codaq
c
                 ncoda=ncoda+1
                 coda_stat(ncoda)=data(j)(2:6)
 4848            continue
              endif
           enddo

c         write(6,*)'coda',ncoda,minstation
           if(ncoda.gt.0.and.ncoda.ge.minstation) then
             accepted=.true.
           else
             accepted=.false.
           endif

        endif   
      endif                            
C
C   CHECK FOR REQUIRED STATIONS IF WANTED, AT LEAST ONE OF STATIONS IN
C   STATSELECT MUST BE PRESENT TO SELECT EVENT, skip if compact file
C
C  CHECK IF DEFAULT OR ALL STATIONS INCLUDED, ELSE CHECK WHICH STATION
c  also check distance range. If no distance, reject station
C
      if(stat_present.gt.0) then
      if(statselect(1)(1:5).ne.'CODAQ') then
      if(accepted) then
      STATIONOK=1
c     write(6,*)'nstat=',nstat,' stat_pr',stat_present
      IF(.not.compact.AND.STAT_present.GT.0) THEN
         STATIONOK=0
         if(nstat.ge.0) then
            DO I=1,STAT_PRESENT
                DO J=NHEAD+1,NRECORD-1
                   READ(DATA(J),'(1X,A7)') STATIONCODE
                   if(data(j)(71:75).ne.' ') then
                       read(data(j)(71:75),'(f5.0)') dist
                   else
                       dist=-1   ! indicate no distance available
                   endif
c
c   case of both station and component  and phase
c
c
c
c   if hypocentral distance, calculate here
c
                   if(hyp_dist) then
c                     write(88,*) dist,depth
                      if(dist.gt.-0.1) dist=sqrt(dist*dist+depth*depth)
c                     write(88,*) dist
                   endif
c
c   case of station and component
c
                   IF( statselect(i)(1:5).ne.'     '.and.
     *                 statselect(i)(6:7).ne.'  '   .and.
     *                 STATSELECT(I).EQ.STATIONCODE .and.
     *                 ((dist.ge.mindist(i).and.dist.le.maxdist(i))
     *                  .or.dist.lt.0).
     *                 and.((data(j)(11:14).eq.stat_phase(i).and.
     *                 stat_phase(i).ne.' ').or.(stat_phase(i).eq.
     *                 '____'.and.data(j)(11:14).eq.' ')
     *                 .or.stat_phase(i).eq.' ')) then
                          STATIONOK=stationok+1
                          goto 1616             ! only count first occurrence
                   endif
c
c   case of only station
c
                   IF( statselect(i)(1:5).ne.'     '.and.
     *                 statselect(i)(6:7).eq.'  '   .and.
     *                 STATSELECT(I)(1:5).EQ.STATIONCODE(1:5).and. 
     *                 ((dist.ge.mindist(i).and.dist.le.maxdist(i))
     *                  .or.dist.lt.0).
     *                 and.((data(j)(11:14).eq.stat_phase(i).and.
     *                 stat_phase(i).ne.' ').or.(stat_phase(i).eq.
     *                 '____'.and.data(j)(11:14).eq.' ')
     *                 .or.stat_phase(i).eq.' ')) then

                          STATIONOK=stationok+1
                          goto 1616
                   endif
c
c   case of only component
c
                   IF( statselect(i)(1:5).eq.'     '.and.
     *                 statselect(i)(6:7).ne.'  '   .and.
     *                 STATSELECT(I)(6:7).EQ.STATIONCODE(6:7).and.
     *                 ((dist.ge.mindist(i).and.dist.le.maxdist(i))
     *                  .or.dist.lt.0).
     *                 and.((data(j)(11:14).eq.stat_phase(i).and.
     *                 stat_phase(i).ne.' ').or.(stat_phase(i).eq.
     *                 '____'.and.data(j)(11:14).eq.' ')
     *                 .or.stat_phase(i).eq.' ')) then
                          STATIONOK=stationok+1
                          goto 1616
                   endif
                enddo
 1616           continue
            enddo
         endif
      ENDIF
c
c   case of all stations and components must be present
c
      IF(all_stat.and.STATIONOK.NE.stat_present)  ACCEPTED = .FALSE.
c
c   case of any one of stations and components must be present
c
      IF(.not.all_stat.and.STATIONOK.eq.0      )  ACCEPTED = .FALSE.


c
      endif
      endif
      endif

c--------------------------------------------------
c  polarity
c--------------------------------------------------

       if (accepted) then
          if (.not. compact .and. mnupol .gt. 0) then
              cnupol = 0
              do j = 1,nrecord
                 if ((data(j) (80:80) .eq. ' ' .or. 
     *               data(j) (80:80) .eq. '4').and.
     *               data(j)(11:11).eq.'P') then
                     if (data(j) (17:17) .eq. 'C' .or.
     *                   data(j) (17:17) .eq. 'D') 
c    *                   data(j) (17:17) .eq. '-' .or.
c    *                   data(j) (17:17) .eq. '+') 
     *                   cnupol = cnupol + 1
                 end if
              end do
              if (cnupol .lt. mnupol) accepted = .false.
          end if
      end if

c
c------------------------------------------------------------
c  phases
c------------------------------------------------------------
c
        if (accepted) then
           if (phase(1).ne.' ') then
              do j = nhead+1,nrecord-1
                  do k=1,6
                     if((data(j)(11:14).eq.phase(k).and.
     *               phase(k).ne.' ').or.
     *               (data(j)(11:14).eq.' '.and.phase(k).eq.'____'))
     *               goto 3746
                   enddo
              end do
              accepted=.false.  ! went through whole loop without finding phase
 3746         continue
            end if
        end if

      fpsolutf = .false.
      eqfeltf = .false.
      helatf = .false.
      helonf = .false.
      hedepthf = .false.
      if (accepted) then
          if (.not. compact .and. (minhelat .ne. 0.0 .or. 
     *        maxhelat .ne. 99999.)) then
              do j = 1,nrecord
                 if (data(j) (80:80) .eq. 'E' .or.
     *               data(j) (80:80) .eq. 'e') then
                     read (data(j) (24:30),'(f7.0)') filevar
                     if (filevar .ge. minhelat .and.
     *                   filevar .le. maxhelat) then
                         helatf = .true.
                     end if
                 end if
              end do
              if (.not. helatf) accepted = .false.
          end if
      end if
      if (accepted) then
          if (.not. compact .and. (minhelon .ne. 0. .or. 
     *        maxhelon .ne. 99999.)) then
              do j = 1,nrecord
                 if (data(j) (80:80) .eq. 'E' .or.
     *               data(j) (80:80) .eq. 'e') then
                     read (data(j) (31:38),'(f8.0)') filevar
                     if (filevar .ge. minhelon .and.
     *                   filevar .le. maxhelon) then
                         helonf = .true.
                     end if
                 end if
              end do
              if (.not. helonf) accepted = .false.
          end if
      end if
      if (accepted) then
          if (.not. compact .and. (minhedepth .ne. -99 .or. 
     *        maxhedepth .ne. 99999.0)) then
              do j = 1,nrecord
                 if (data(j) (80:80) .eq. 'E' .or.
     *               data(j) (80:80) .eq. 'e') then
                     read (data(j) (39:43),'(f5.0)') filevar
                     if (filevar .ge. minhedepth .and.
     *                   filevar .le. maxhedepth) then
                         hedepthf = .true.
                     end if
                 end if
              end do
              if (.not. hedepthf) accepted = .false.
          end if
      end if
      if (accepted) then
          if (.not. compact .and. fpsolut) then
              do j = 1,nrecord
                 if (data(j) (80:80) .eq. 'F' .or.
     *               data(j) (80:80) .eq. 'f') then
c
c   check for a possible quality indicator, only select event with this
c   quality, can be a combination of 5 different. only check first appearence
c   of fps solution
c
                     if(fpquality.ne.' ') then
                         fpsolutf=.false.
                         do i=1,5
                           if(fpquality(i:i).ne.' '.and.fpquality(i:i).
     *                     eq.data(j)(78:78)) then
                              fpsolutf=.true.
                           endif
                         enddo
                     else
                         fpsolutf=.true.
                     endif
c
c   since at least one fps found, do not check more
c
                     goto 3344
                 end if

              end do
 3344         continue
              if (.not. fpsolutf) accepted = .false.
          end if
      end if
      if (accepted) then
          if (.not. compact .and. eqfelt) then
              do j = 1,nrecord
                 if (data(j) (80:80) .eq. '2') then
                     eqfeltf = .true.
                 end if
              end do
              if (.not. eqfeltf) accepted = .false.
          end if
      end if
c
c   wave form file names
c
      if (accepted) then
          if (.not.compact.and.wave_check.ne.' ') then
              wave=.false.
              k=sei clen(wave_check)
              do j = 2,nhead
                 if (data(j) (80:80) .eq. '6') then
                    if(wave_check(1:1).eq.'*') then ! any wave form
                       wave=.true.
                    else      ! loook for part of string
                       read(data(j)(2:79),'(a)') text    ! wave form name
                       do i=1,sei clen(text)-k+1
                         if(text(i:i+k-1).eq.wave_check(1:k))wave=.true.
                       enddo
                    endif
                 end if
              end do
              if (.not. wave) accepted = .false.
          end if
      end if


C
C
C   WRITE OUT EVENT IF SELECTED
C
      IF (ACCEPTED)THEN
C
C   FIND WAVEFORM FILE NAMES IF A WHOLE FILE IS USED
C
         IF(.not.compact) THEN
            DO I=2,NHEAD
               IF(DATA(I)(80:80).EQ.'6') THEN
               K=INDEX(DATA(I)(2:79),' ')
C
               write(write4,'(i5,2x,A)',iostat=code)
     *         nwave,DATA(I)(2:k)
               call sei code(fort$,              ! stop on error.
     &                       code,               ! Error condition.
     &                       write4,             ! For fortran unit.
     &                       b_flag )            ! Flag (n/a).
c
               NWAVE=NWAVE+1
               ENDIF
            ENDDO
         ENDIF	  
C
         write(write1,'(A80)',iostat=code)(DATA(I),I=1,NRECORD)
         call sei code(fort$,                    ! stop on error.
     &                 code,                     ! Error condition.
     &                 write1,                   ! For fortran unit.
     &                 b_flag )                  ! Flag (n/a).
c
c  if input from data base, both compact and full nordic files can
c  be input, therefore always have a blank line between events
c
         if(compact.and.inputoption.eq.1)
     &   write(write1,'(a5)',iostat=code)'     '
         call sei code(fort$,                    ! stop on error.
     &                 code,                     ! Error condition.
     &                 write1,                   ! For fortran unit.
     &                 b_flag )                  ! Flag (n/a).
c
c   if seisweb option, also make a compact file
c
         if(seisweb) then
            write(write5,'(a80)',iostat=code) data(1)
         endif
C
C   COUNT SELECTED EVENTS AND WRITE INDEXES IF id is PRESENT 
c   if input is not from data base, no data base reference
c   can be made
C
         NOUT=NOUT+1		
         IF(.not.compact)THEN
            if(id.ne.0) then
               nid=nid+1
               read(data(id)(61:74),'(i4,5i2)') 
     *         year,month,day,hour,minutes,isec
               call sfilname
     *         (year,month,day,hour,minutes,isec,basename(1:5),
     *          distanceid,sfile,k)
C
               if(seisweb) then
                  write(write3,'(a)') sfile(1:k)
               else
                  if(inputoption.ne.2) then   ! only write out if from data base
                    write(write3,'(I5,2X,A)',iostat=code) 
     &              NID,SFILE(1:k)
c
c   if codaq, write index and stations to use
c
                    if(ncoda.gt.0) then
                      
                       write(90,'(I5,2X,A)') 
     &                 NID,SFILE(1:k)
                       if(statselect(1)(7:7).ne.'A') then
                          if(ncoda.gt.300) then
                             write(6,*)
     *                      'Too many stations, reduced to 300'
                             ncoda=300
                          endif
                          write(90,'(300a5)')(coda_stat(i),i=1,ncoda)
                       else
                          if(ncoda.gt.100) then
                             write(6,*)
     *                       'Too many stat-comps, reduced to 300'
                             ncoda=100
                          endif 
                          write(90,'(300a5)')
     *                    (coda_stat(i),coda_stat(i),coda_stat(i),
     *                     i=1,ncoda)
                       endif

                       if(statselect(1)(7:7).eq.'Z') write(90,'(300a5)')
     *                                     ('   Z ',i=1,ncoda)
                       if(statselect(1)(7:7).eq.'N') write(90,'(300a5)')
     *                                     ('   N ',i=1,ncoda)
                       if(statselect(1)(7:7).eq.'E') write(90,'(300a5)')
     *                                     ('   E ',i=1,ncoda)
                       if(statselect(1)(7:7).eq.'A') 
     *                 write(90,'(200a15)')
     *                                    ('   Z    N    E ',i=1,ncoda)
                       if(statselect(1)(7:7).eq.' ') write(90,*)
                    endif
                    call sei code(fort$,               ! stop on error.
     &                       code,                   ! Error condition.
     &                       write3,                 ! For fortran unit.
     &                       b_flag )                ! Flag (n/a).
                  endif
               endif
            endif
         ENDIF
      ENDIF

998   CONTINUE

C
C   BACK TO GET NEXT EVENT
C
      id=0
      GO TO 10		
c
C-------------------------------------------------------------------
C   FINISHED, WRITE OUT STATISTICS
C-------------------------------------------------------------------
999   CONTINUE
c
c   write last file statisitcs if from epibase
c
      if(inputoption.eq.1.and.nstatus.ne.1) then
         nstatus=-1
         call epibase(basename,starttime,endtime,
     *   nstat,nphase,nhead,nrecord,distanceid,exp,data,id,nstatus,
     *   compact,nout,ncat_files,cat_files)
      endif

      write(6,*)' '
      WRITE(6,*)' TOTAL NUMBER OF EVENTS IN TIME INTERVAL ',NEVENT
      WRITE(6,*)' NUMBER OF DISTANT EVENTS   - - - - -    ',NDISTANT
      WRITE(6,*)' NUMBER OF REGIONAL EVENTS  - - - - -    ',NREGIONAL
      WRITE(6,*)' NUMBER OF LOCAL EVENTS     - - - - -    ',NLOCAL
      write(6,*)' ---------------------------------------------------'
      WRITE(6,*)' NUMBER OF EVENTS SELECTED ************* ',NOUT
      WRITE(6,*)' NUMBER OF WAVEFORM FILES SELECTED       ',NWAVE
      write(6,*)' NUMBER OF INDEXES SELECTED              ',NID
      WRITE(6,*)' SELECTED EARTHQUAKES ARE IN:         select.out'
      WRITE(*,*)' LOCAL INDEX FILE IS:                  index.out'
      if(statselect(1)(1:5).eq.'CODAQ')
     *WRITE(*,*)' CODAQ INPUT FILE IS:                index.codaq'
      WRITE(6,*)
     *          ' NAMES FOR WAVEFORM FILES IN: waveform_names.out'
      write(6,*)' SELECT COMMANDS IN:                  select.inp'
      write(6,*) ' '
c
c   message for seisweb
c
      if(seisweb) then
         write(6,*)  'select_terminated: ok ',nout 
      endif

      STOP
      END
C -------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     ROUTINE INQUIRES EPICENTER SELECTION CRITERIA
C     AND RETURN THESE PARAMETERS TO CALLING PROGRAM
C     PARAMETERS TO RETURN IN COMMON BLOCK: EPICRITE , EPICRIT2
C                                           EPICRIT3 AND EPICRIT4
C     THE ROUTINE IS INTENDED TO BE USED TOGETHER WITH
C     ROUTINE EPISELECT AND ROUTINE INDATA
C     WRITTEN BY: C. LINDHOLM
C     MODIFIED BY J. HAVSKOV TO INCLUDE STATION SELECTION and a lot more
C     LAST REVISED:  AUGUST 18, 1987
c                    may -89. Poosibility to run on database facilitated c.l.
C     CHANGED:       1990-01-30, R.N.ARVIDSSON TO PC-DOS.
C     LAST REVISED:  1990-01-30, RNA, COMMON BLOCK STRUCTURE.
C     LAST REVISED:  1990-02-01, RNA, INPUT OF YYYMMDDHHMM CHANGED.
C                    1990-02-16, RNA, COMMON BLOCK STRUCTURE.
C                    1990-04-27, JH,  SPECIAL SEISNOR STATION Q REMOVED
C                    1991-04-03, CL,  FACILITATE USE OF BLANK ENDTIME
C                    1991-19-09  JH   MAJOR CLEANUP WITH START AND STOP TIMES
C                    OCT 91           MORE CLEANUP
C
C     OUTPUT: ALL PARAMETERS IN COMMON BLOCK:'CRITERIA'
C             TIME1,TIME2 :  TIME-LIMITS
C             HILON,LOLON,HILAT,LOLAT: LONGITUDE/LATITUDE -LIMITS
C             MINSTATION: MINIMUM NUMBER OF STATIONS
C             MAXSTATION: MAXIMUM NUMBER OF STATIONS
C             MINMAG,MAXMAG: MAGNITUDE LIMITS
C             MINDEPTH,MAXDEPTH: DEPTH LIMITS
C             MINRMS: MINIMUM RMSVALUE
C             MAXRMS: MAXIMUM RMSVALUE
c             minhelat, maxhelat: hypocenter error lat limits
c             minhelon, maxhelon: hypocenter error long limits
c             minhedepth, maxhedepth: hypocenter error depth limits
C             DEFAULT: LOGICAL WITH VALUE=.TRUE. IF NO SELECTION
C                                         -PARAMETERS ARE SET
C             STAT_present:  NUMBER OF STATIONS THAT MUST BE PRESENT
C                      IF ZERO, NO IMPORTANCE
C             hypa_present:  NUMBER OF HYPOCENTER AGENCIES THAT MUST BE PRESENT
C                      IF ZERO, NO IMPORTANCE
C             maga_present:  NUMBER OF MAGNITUDE AGENCIES THAT MUST BE PRESENT
C                      IF ZERO, NO IMPORTANCE
C             STATSELECT: STATIONS WHICH MUST BE PRESENT TO SELECT EVENT
C             HYPASELECT: HYPOCENTER AGENCIES WHICH MUST BE PRESENT
C             MAGASELECT: MAGNITUDE AGENCIES WHICH MUST BE PRESENT
C     

		
c-this subroutine reads the starttime and endtime for the selction
c-from the database and calls the subroutine which shows the screen
c-with the options to be changed.
c
      subroutine selcrit(starttime,endtime,basename,basetype,
     *choise_file,narg)
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      character*14 starttime, endtime,stime
      character*6 basetype  ! type of data base, CAT or SFILES
      character*80 choise_file  ! file with choises, if blank, no file
c  start time and end time inc. century
c  abs times
      double precision time1,time2,timcen
c  century
      integer cen1,cen2,month,day,year
c  number of stations to be present
      integer           stat_present
      integer           narg         ! number of arguments
      logical       all_stat         ! if true,  all must be present
c  number of hypocenter agencies to be present
      integer           hypa_present
c  number of magnitude agencies to be present
      integer           maga_present
      logical default
      character*80 option
      real minmag, minrms, maxrms, hilat, lolat, hilon, lolon, maxmag,
     *     mindepth, maxdepth,maxgap,mingap,
     *     minhelat, maxhelat,
     *     minhelon, maxhelon,
     *     minhedepth, maxhedepth
      integer minstation, maxstation, i
      character*1 eventtype(5), disttype(5), magtypes(5)
      character*7 statselect(50)
      character*4 stat_phase(50)
      integer mindist(50),maxdist(50)  ! distance range for station	
      character*3 hypaselect(10), magaselect(10)
      character*80 basename      ! data base of file name of input
c   fault plane solution
      logical fpsolut
c   earthquake felt
      logical eqfelt
      character*40 wave_check    ! string to identify wave form file name
      character*80 text
      logical header_check
      real ypol(max_polyn$,2)    ! array defining polygon
      integer iant               ! number of points of the polygon
      integer mnupol             ! minumum number of polarities
      integer w1                 ! file unit
      integer code               ! for sei open
      logical hyp_dist           ! if true, use hypocentral distance
      logical b_flag             ! ------------
      character*4 phase(6)       ! phase names to select
      character*6 volcanictype(10)   ! volcanic subclasses
      character*5  fpquality     ! quality of fps solution
c

      common/epicrite/hilat,lolat,hilon,
     *                lolon,minmag,maxmag,minrms,maxrms,mindepth,
     *                maxdepth,minhelat,maxhelat,minhelon,maxhelon,
     *                minhedepth,maxhedepth,ypol,iant,mnupol
      common/epicrit2/fpsolut,eqfelt,statselect
      common/epicrit1/mindist,maxdist,
     *                hypaselect,magaselect 
      common/epicrit3/default,eventtype,disttype,magtypes,volcanictype
      common/epicrit4/minstation,maxstation,stat_present,hypa_present,
     *                maga_present,header_check,wave_check
      common/epicrit5/time1,time2,timcen,all_stat,mingap,maxgap,phase,
     *                stat_phase,hyp_dist,fpquality

      option(1:3) = '000'

c
c   if input of choises is from a file, open and read from that file
c
       if(choise_file.ne.' ') then
c
c   read choises
c
      call sei open( unknown$,             ! Open file (default stop on error).
     &               ' ',                  ! Prompt (n/a).
     &               choise_file,         ! Filename.
     &               w1,                   ! Unit opened on.
     &               b_flag,               ! Flag (n/a).
     &               code )                ! Returned condition (n/a).
      read(w1,'(31x,1x,a)')  text          ! is base name, read before
      read(w1,'(31x,1x,a)') basetype
      read(w1,'(31x,1x,a)')  starttime     
      read(w1,'(31x,1x,a)') endtime
      read(w1,'(31x,i15)')  minstation           
      read(w1,'(31x,i15)')   maxstation         
      read(w1,'(31x,f15.3)') lolat
      read(w1,'(31x,f15.3)') hilat               
      read(w1,'(31x,f15.3)') lolon
      read(w1,'(31x,f15.3)') hilon  
      read(w1,'(31x,f15.3)') minmag   
      read(w1,'(31x,f15.3)') maxmag   
      maga_present=0
      hypa_present=0
      read(w1,'(31x,1x,10a3)')  magaselect
      read(w1,'(31x,1x,10a3)')  hypaselect
      do i=1,10
        if(magaselect(i).ne.'   ') maga_present=maga_present+1
        if(hypaselect(i).ne.'   ') hypa_present=hypa_present+1
      enddo
      read(w1,'(31x,f15.3)') minrms 
      read(w1,'(31x,f15.3)') maxrms
      read(w1,'(31x,f15.3)') mindepth   
      read(w1,'(31x,f15.3)') maxdepth 
      read(w1,'(31x,f15.3)') minhelat
      read(w1,'(31x,f15.3)') maxhelat
      read(w1,'(31x,f15.3)') minhelon 
      read(w1,'(31x,f15.3)') maxhelon 
      read(w1,'(31x,f15.3)') minhedepth 
      read(w1,'(31x,f15.3)') maxhedepth
      read(w1,'(31x,1x,5a1)')magtypes
      read(w1,'(31x,1x,5a1)')disttype
      read(w1,'(31x,1x,5a1)')eventtype
      read(w1,'(31x,i15)')   mnupol           
      read(w1,'(31x,14x,l1)')eqfelt 
      read(w1,'(31x,8x,a5,1x,l1)')fpquality,fpsolut 
      read(w1,'(31x,14x,l1)')header_check 
      read(w1,'(31x,1x,a)')  wave_check
      read(w1,'(31x,f15.3)') mingap
      read(w1,'(31x,f15.3)') maxgap
      read(w1,'(31x,1x,6a4)')   phase
      read(w1,'(31x,1x,10a6)') volcanictype
      read(w1,'(a)') text                      ! explanation line
      read(w1,'(31x,13x,2l1)') all_stat,hyp_dist        
c
c   read and find how many stations present
c
      
      do i=1,9999
         read(w1,'(1x,a7,2i7,1x,a4)')statselect(i),mindist(i),
     *   maxdist(i),
     *   stat_phase(i)
         if(statselect(i).eq.'       ') goto 2999
      enddo
 2999 continue
      stat_present=i-1
c
      read(w1,'(a)') text                      ! explanation line
c      write(6,*) basename
c      write(6,'(1x,a,a)') starttime,endtime
      do i=1,9999
        read(w1,'(2f10.3)',end=9876) ypol(i,2),ypol(i,1)
      enddo
 9876 continue
      iant=i-1

c
c      write(6,*)' max',maxhedepth
      call sei close(close$,w1,code ) ! Close but stop on error.
      endif


C
C     NOW INQUIRE BOUNDARY VALUES
C

 111   continue
       if(choise_file.eq.' '.and.narg.lt.3) then
          write(6,'(1x,a,$)')  
     *  ' Start time (blank is 1980), yyyymmddhhmmss:'
          read(5,'(a14)') starttime
          if(starttime.eq.' ') then
             write(6,'(1x,a,$)')
     *     ' End time, enter for  2020                 :'
          else 
             write(6,'(1x,a,$)')
     *  ' End time, enter for end of month          :'
          endif
          read(5,'(a14)') endtime
       endif
c
c   if start time has ny year, put in 1980
c
       if(starttime(1:4).eq.' ') then
          starttime(1:4)='1980'
          if(endtime(1:4).eq.' ') endtime(1:4)='2020'
       endif
c
c   if start time has no month, put in january
c
      if(starttime(5:6).eq.'  ') starttime(5:6)='01'
c
c   if start time has no day, put in 1
c
      if(starttime(7:8).eq.'  ') starttime(7:8)='01'    
      do i=9,14
        if(starttime(i:i).eq.' ') starttime(i:i)='0'
      enddo
c
c   if end time year is blank, must be to end of month, put in
c   same year-month as start time
c
c-- end of month
      if(endtime(1:4).eq.'    ')  endtime(1:6)=starttime(1:6)
c
c   make sure end time is to end of month, day etc if left blank
c
      if(endtime(5:6).eq.'  ') endtime(5:6)='12'
      if(endtime(7:8).eq.'  ') then
         read(endtime(1:6),'(i4,i2)') year,month
         call month_day(year,month,day)
         write(endtime(7:8),'(i2)') day
         if(endtime(7:7).eq.' ') endtime(7:7)='0'
      endif
      if(endtime(9:10).eq.'  ') endtime(9:10)='23'
      if(endtime(11:12).eq.'  ' ) endtime(11:12)='59'
      if(endtime(13:14).eq.'  ') endtime(13:14)='59'
ctest      write(6,*) endtime
c
c   get century
c
cxx         read(starttime,'(i2)') cen1
cxx         read(endtime, '(i2)') cen2
c
c   check if time interval is valid, get abs time from 1900 if time later
c   then 1900
c
CC2000   if(cen1.lt.19) then
            stime=starttime
cxx            stime(1:2)='19'      ! make sure calculation goes from 1900,
            call abstim(stime,0,time1)    ! - century corrected for below
CC       else
CC          call abstim(starttime,0,time1)
CC       endif
c
CC       if(cen2.lt.19) then
            stime=endtime
cxx            stime(1:2)='19'
            call abstim(stime,0,time2)
CC       else
CC          call abstim(endtime,0,time2)
CC       endif
ctest            WRITE(6,'(a,a)') STARTTIME,ENDTIME
ctest            WRITE(6,*) TIME1,TIME2
c
c   add century
c
cxx         call abstim('19991231235960',0,timcen)  ! time of one century
cxx         time1=time1+cen1*timcen
cxx         time2=time2+cen2*timcen
ctest            write(6,*) 'C1,C2,TC,T1,T2', CEN1,CEN2,TIMCEN,TIME1,TIME2
c
c   check interval
c
         if(time1.ge.time2) then
            write(6,*)' ********* wrong time interval *************'
c            if(choise_file.ne.' ') then
               stop
c           else
c              goto 111
c           endif
         endif
c
c   return if input from choise file or argument string
c
         if(choise_file.ne.' '.or.narg.gt.2) return
c
c-calling the subroutine which draws the screen with options
c
         call draw_screen(option)
c-if enter, default is choosen
         if (option(1:1).eq.' ') then
            default = .true.
         end if

      do while (option(1:3) .ne. '000')

      if (option(1:3) .eq. '1  ' .or. option(1:3) .eq. '2  ' .or.
     *    option(1:3) .eq. '3  ' .or. option(1:3) .eq. '4  ' .or.
     *    option(1:3) .eq. '5  ' .or. option(1:3) .eq. '6  ' .or.
     *    option(1:3) .eq. '7  ' .or. option(1:3) .eq. '8  ' .or.
     *    option(1:3) .eq. '9  ' .or. option(1:3) .eq. '10 ' .or.
     *    option(1:3) .eq. '11 ' .or. option(1:3) .eq. '12 ' .or.
     *    option(1:3) .eq. '13 ' .or. option(1:3) .eq. '14 ' .or.
     *    option(1:3) .eq. '15 ' .or. option(1:3) .eq. '16 ' .or.
     *    option(1:3) .eq. '17 ' .or. option(1:3) .eq. '18 ' .or.
     *    option(1:3) .eq. '19 ' .or. option(1:3) .eq. '20 ' .or.
     *    option(1:3) .eq. '21 ' .or. option(1:3) .eq. '22 ' .or.
     *    option(1:3) .eq. '23 ' .or. option(1:3) .eq. '24 ') then
         call draw_screen(option)
      else if (option(1:1) .eq. ' ') then 
           option(1:3) = '000'
      else if (option(1:3) .ne. '1  ' .and.
     *         option(1:3) .ne. '2  ' .and.
     *         option(1:3) .ne. '3  ' .and.
     *         option(1:3) .ne. '4  ' .and.
     *         option(1:3) .ne. '5  ' .and.
     *         option(1:3) .ne. '6  ' .and.
     *         option(1:3) .ne. '7  ' .and.
     *         option(1:3) .ne. '8  ' .and.
     *         option(1:3) .ne. '9  ' .and.
     *         option(1:3) .ne. '10 ' .and.
     *         option(1:3) .ne. '11 ' .and.
     *         option(1:3) .ne. '12 ' .and.
     *         option(1:3) .ne. '13 ' .and.
     *         option(1:3) .ne. '14 ' .and.
     *         option(1:3) .ne. '15 ' .and.
     *         option(1:3) .ne. '16 ' .and.
     *         option(1:3) .ne. '17 ' .and.
     *         option(1:3) .ne. '18 ' .and.
     *         option(1:3) .ne. '19 ' .and.
     *         option(1:3) .ne. '20 ' .and.
     *         option(1:3) .ne. '21 ' .and.
     *         option(1:3) .ne. '22 ' .and.
     *         option(1:3) .ne. '23 ' .and.
     *         option(1:3) .ne. '24'        ) then
           option(1:1) = '?'
         call draw_screen(option)
      end if    
      end do
c
c   write out choices
c
      call sei open( unknown$,             ! Open file (default stop on error).
     &               ' ',                  ! Prompt (n/a).
     &               'select.inp',         ! Filename.
     &               w1,                   ! Unit opened on.
     &               b_flag,               ! Flag (n/a).
     &               code )                ! Returned condition (n/a).
      write(w1,'(a,1x,a)')   ' Base or file name            :',
     *basename
      write(w1,'(a,1x,a)')   ' Base type                    :',
     *basetype
      write(w1,'(a,1x,a  )') ' Start time                   :',
     *starttime     
      write(w1,'(a,1x,a)')   ' End time                     :',
     *endtime
      write(w1,'(a,i15)')    ' Minimum number of stations   :',
     *minstation           != 0
      write(w1,'(a,i15)')    ' Maximum number of stations   :',
     *maxstation           != 999
      write(w1,'(a,f15.3)')  ' Minimum latitude             :',
     *lolat                != -90.0 
      write(w1,'(a,f15.3)')  ' Maximum latitude             :',
     * hilat               != 90.0
      write(w1,'(a,f15.3)')  ' Minimum longitude            :',
     *lolon                != -360.0
      write(w1,'(a,f15.3)')  ' Maximum longitude            :',
     * hilon               != 360.0
      write(w1,'(a,f15.3)')  ' Minimum magnitude            :',
     *minmag               != -990.0
      write(w1,'(a,f15.3)')  ' Maximum magnitude            :',
     *maxmag               != +999.0
      write(w1,'(a,1x,10a3)')   ' Magnitude agencies           :',
     *magaselect
      write(w1,'(a,1x,10a3)')   ' Hypocenter agencies          :',
     *hypaselect
      write(w1,'(a,f15.3)')  ' Minimum rms                  :',
     *minrms               != 0
      write(w1,'(a,f15.3)')  ' Maximum rms                  :',
     *maxrms               != 999.0
      write(w1,'(a,f15.3)')  ' Minimum depth                :',
     *mindepth             != -99.0
      write(w1,'(a,f15.3)')  ' Maximum depth                :',
     *maxdepth             != 99999.0 
      write(w1,'(a,f15.3)')  ' Minimum error in latitude    :',
     *minhelat              != .0
      write(w1,'(a,f15.3)')  ' Maximum error in latitude    :',
     *maxhelat              != 99999.0
      write(w1,'(a,f15.3)')  ' Minimum error in longitude   :',
     *minhelon             != .0
      write(w1,'(a,f15.3)')  ' Maximum error in longitude   :',
     *maxhelon              != 99999.0
      write(w1,'(a,f15.3)')  ' Minimum error in depth       :',
     *minhedepth            != -99.0
      write(w1,'(a,f15.3)')  ' Maximum error in depth       :',
     *maxhedepth            != 99999.0
      write(w1,'(a,1x,5a1)') ' Magnitude types (L,C,B,S,W)  :',
     *magtypes
      write(w1,'(a,1x,5a1)') ' Distance (ID) types (L,R,D)  :',
     *disttype
      write(w1,'(a,1x,5a1)') ' Event types (e.g. E,V,P)     :',
     *eventtype

      write(w1,'(a,i15)')    ' Minimum number of polarities :',
     *mnupol           
c      default = .false.
c      fpsolut = .false.
      write(w1,'(a,14x,l1)') ' Felt earthquakes             :',
     *eqfelt 
      write(w1,'(a,8x,a5,1x,l1)') ' Fault plane solution         :',
     *fpquality,fpsolut 
      write(w1,'(a,14x,l1)') ' Check all header lines       :',
     *header_check 
      write(w1,'(a,1x,a)')   ' Waveform files to check      :',
     *wave_check
      write(w1,'(a,f15.3)')  ' Minimum gap                  :',
     *mingap               != 0
      write(w1,'(a,f15.3)')  ' Maximum gap                  :',
     *maxgap               != 999.0
      write(w1,'(a,1x,6a4)') ' Phases                       :',
     *phase
      write(w1,'(a,1x,10a6)')' Volcanic subclasses          :',
     *volcanictype
      write(w1,'(a,a)') 
     *' Stat., comp. dist range, phase (1x,a5,a2,2i7,1x,a4)',
     *' one pr line, end blank line:'
      write(w1,'(a,2l1)') 
     *' STAT CO Mindis Maxdis Phas All stat hdist->',
     * all_stat,hyp_dist
      if(stat_present.gt.0) then
         do i=1,stat_present
           write(w1,'(1x,a7,2i7,1x,a4)')statselect(i),mindist(i),
     *                                  maxdist(i),stat_phase(i)
         enddo
      endif
      write(w1,*)
      write(w1,'(a,a)')        ' Polygon points (lat,lon), one pair pr',
     *' line, end with blank line :'
      do i=1,iant
        write(w1,'(2f10.3)') ypol(i,2),ypol(i,1)
      enddo

      return
      end 
   

 
c-this subroutine checks which option was selected and makes
c-all the questions necessaries for the selection depending of
c-the option. By CARLOS EDUARDO VIEIRA
      subroutine draw_screen(option)
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      real minmag, maxmag         ! minimum and maximum magnitude
      real lolat, hilat           ! lower and upper latitude
      real lolon, hilon           ! lower and upper longitude
      real mindepth, maxdepth     ! minimum and maximum depth
      real minrms                 ! minimum rms
      real maxrms                 ! maximum rms
      real maxgap,mingap          ! gap range
      integer minstation          ! minimum number of stations
      integer maxstation          ! maximum number of stations
      real minhelat               ! minimum hypocenter error lat
      real maxhelat               ! maximum hypocenter error lat
      real minhelon               ! minimum hypocenter error long
      real maxhelon               ! maximum hypocenter error long
      real minhedepth             ! minimum hypocenter error depth
      real maxhedepth             ! maximum hypocenter error depth
      logical fpsolut             ! fault plane solution
      logical eqfelt              ! earthquake felt
      character*1 eventtype(5)    ! event id
      character*1 disttype(5)     ! distance id
      character*1 magtypes(5)     ! magnitude type
      character*7 statselect(50)  ! station code
      character*4 stat_phase(50)  ! phase to select with station
      integer mindist(50),maxdist(50)  ! distance range for station	
      character*3 hypaselect(10)  ! hypocenter agencies
      character*3 magaselect(10)  ! magnitude agencies
      character*80 option         ! option
      integer stat_present        ! number of stations codes
      logical       all_stat         ! if true,  all must be present
      integer hypa_present        ! number of hypocenter agencies
      integer maga_present        ! number of magnitude agencies
      real ypol(max_polyn$,2)     ! array defining polygon
      integer iant                ! number of points of the polygon
      integer idist               ! indicator if distances requred
      integer mnupol              ! minumum number of polarities
      integer status, statusaux   ! iostatus
      character*1 eventtypet(5)   ! aux screen for event id
      character*1 disttypet(5)    ! aux screen for distance id
      character*1 magtypest(5)    ! aux screen for magnitude type
      character*80 reading        ! aux for read integer/real variable
      character*3 minstationa     ! aux char for minimum number of stations
      character*3 maxstationa     ! aux char for maximum number of stations
      character*4 minrmsa         ! aux char for minimum rms
      character*4 maxrmsa         ! aux char for maximum rms
      character*5 mindeptha, maxdeptha ! aux char for min and max depth
      character*6 minmaga, maxmaga ! aux char for min and max magnitude
      character*7 lolata, hilata  ! aux char for lower and upper latitude
      character*8 lolona, hilona  ! aux char for lower and upper longitude
      character*8 maxgapa, mingapa  ! aux char for lower and upper gap
      character*5 minhedeptha     ! aux char for minimum hypocenter error depth
      character*5 maxhedeptha     ! aux char for maximum hypocenter error depth
      character*7 minhelata       ! aux char for minimum hypocenter error lat
      character*7 maxhelata       ! aux char for maximum hypocenter error lat
      character*8 minhelona       ! aux char for minimum hypocenter error long
      character*8 maxhelona       ! aux char for maximum hypocenter error long
      character*5 mnupola         ! aux char minumum number of polarities
      integer stcodes             ! status codes
      integer count               ! counter for blank input
      integer index, inde1,       
     *        inde2, inde3        ! aux index
      character*1 fpsolutin       ! reading for ftpsolut
      character*1 eqfeltin        ! reading for eqfelt
      character*40 wave_check    ! string to identify wave form file name
      logical header_check        ! checking all headers
      logical found               ! aux for loop
      logical default             ! Previously undefined (JAB).
      integer ind                 ! Indicator            (JAB).
      integer  i,code
      logical hyp_dist           ! if true, use hypocentral distance

      character*4 phase(6)       ! phase names to select
      character*6 volcanictype(10)   ! volcanic subclasses
      character*5  fpquality     ! quality of fps solution
c
      integer seiclen

      double precision time1,time2,timcen
C
      common/epicrite/hilat,lolat,hilon,
     *                lolon,minmag,maxmag,minrms,maxrms,mindepth,
     *                maxdepth,minhelat,maxhelat,minhelon,maxhelon,
     *                minhedepth,maxhedepth,ypol,iant,mnupol
      common/epicrit2/fpsolut,eqfelt,statselect
      common/epicrit1/mindist,maxdist,
     *                hypaselect,magaselect 
      common/epicrit3/default,eventtype,disttype,magtypes,volcanictype
      common/epicrit4/minstation,maxstation,stat_present,hypa_present,
     *                maga_present,header_check,wave_check
      common/epicrit5/time1,time2,timcen,all_stat,mingap,maxgap,phase,
     *                stat_phase,hyp_dist,fpquality


      idist=0
c      do i=1,5  
c  5-7-200 jh    magtypest(i)=' '
c  ----------    disttypet(i)=' '
c  ---------    eventtypet(i)=' '
c      end do                        ! not sure why it was here

c-formats used in the subroutine

 900  format ('  SELECT NUMBER TO CHANGE PARAMETER, RETURN TO SEARCH: '
     *,$)
 901  format ('  Maximum RMS, return for default: ',$)
 902  format ('  Minimum Number of Stations, return for default: ',$)
 903  format ('  Magnitude Type. Any combinations of C, L, b, B, s, S,' 
     *        ,' e.g. Cb for CODA and mb.'
     *       /,'  Return for all types.',
     *        ' Enter with the value for Magnitude Type: ',$)
 904  format ('  Distance ID. Any combinations of L, R, D, E.G. LR '
     *        ,'for Local and Regional.'
     *       /,'  Return for all types.',
     *        ' Enter with the value(s) for Distance ID: ',$)
 905  format ('  Event ID. Any combinations of E, P, V, S, Q, E.G. '
     *        ,'QE for Quakes and Explosions.'
     *       /,'  Return for all types.',
     *        ' Enter with the value(s) for Event ID: ',$)
 906  format ('  Minimum Magnitude, return for default: ',$)
 907  format ('  Maximum Magnitude, return for default: ',$)
 908  format ('  Lower Latitude Limit, return for default: ',$)
 909  format ('  Upper Latitude Limit, return for default: ',$)
 910  format ('  Lower Longitude Limit, return for default: ',$)
 911  format ('  Upper Longitude Limit, return for default: ',$)
 912  format ('  Minimum Depth Limit, return for default: ',$)
 913  format ('  Maximum Depth Limit, return for default: ',$)
 914  format ('            ',$)
 915  format ('  Something wrong. Default value assumed.')
 916  format ('  Some wrong value not considerated.')
 917  format ('  Default value assumed.')
 918  format ('  Ok. Input realized successfully.')
 919  format 
     *       ('  Station, component, distance range and phase',
     *        ' which at least one '
     *        ,'should be present.'
     *       /, '  Example is BERG LZ, BERGE,     LZ'
     *       /,'  One per line, at least STAT or CO, end with return',
     *         ', or return for defaults: '
     *       /,'            STAT CO MINDIS MAXDIS PHAS') 
 920  format (6x,'10  - RMS Limits',30x,f5.1,6x,f5.1)
 921  format (6x,'6   - Magnitude Limits',23x,f6.1,5x,f6.1)
 922  format (6x,'7   - Latitude Limits',23x,f7.3,4x,f7.3)
 923  format (6x,'8   - Longitude Limits',21x,f8.3,3x,f8.3)
 924  format (6x,'9   - Depth Limits',25x,f8.1,3x,f8.1)
 925  format ('  Fault Plane Solution. Yes or No: ',$)
 926  format ('  Earthquake Felt. Yes or No: ',$)
 927  format ('  Minimum RMS, return for default: ',$)
 928  format ('  Maximum Number of Stations, return for default: ',$)
 929  format (6x,'12  - Hypocenter Errors Latitude Limits',5x,f9.2,
     *        4x,f9.2)
 930  format (6x,'13  - Hypocenter Errors Longitude Limits',4x,f9.2,
     *        4x,f9.2)
 931  format (6x,'14  - Hypocenter Errors Depth Limits',10x,f8.1,
     *        6x,f8.1)
 932  format ('  Minimum hypocenter error latitude, return for  
     *        default: ',$)
 933  format ('  Maximum hypocenter error latitude, return for  
     *        default: ',$)
 934  format ('  Minimum hypocenter error longitude, return for  
     *        default: ',$)
 935  format ('  Maximum hypocenter error longitude, return for  
     *        default: ',$)
 936  format ('  Minimum hypocenter error depth, return for  
     *        default: ',$)
 937  format ('  Maximum hypocenter error depth, return for  
     *        default: ',$)
 938  format
     *(6x,'18  - Station Codes, components, distance range and phase')
 939  format (6x,'18  - Stat-comp Codes: ',6(a7,1x))
 940  format (29x,6(a7,1x))
 941  format ('  Hypocenter agencies for agencies of which at least '
     *        ,'one should be present.'
     *       /,'  One per line, end with return, or return for '
     *        ,'defaults: ')
 942  format ('  Magnitude agencies for agencies of which at least '
     *        ,'one should be present.'
     *       /,'  One per line, end with return, or return for '
     *        ,'defaults: ')
 943  format (6x,'16  - Hypocenter Agencies')
 944  format (6x,'16  - Hypocenter Agencies: ',10a4)
 945  format (6x,'17  - Magnitude Agencies')
 946  format (6x,'17  - Magnitude Agencies:  ',10a4)
 947  format (6x,'19  - Polygon')
 948  format ('  Minimum number of polarities, return for  
     *        default: ',$)
 949  format (6x,'15  - Minimum Number of Polarities')
 950  format (6x,'15  - Minimum Number of Polarities: ',10x,i5)
 951  format ('  Points of polygon where events should be inside. '
     *        ,'Minimum of 3 points required.'
     *       /,'  One (lat-lon) pair per line, end with return,',
     *          ' or return for '
     *        ,'defaults: ')
 952  format ('  Some wrong point not considered.') 
 953  format ('  Less than 3 points defined. Default value assumed.')
 954  format (14x,'Latitude:  ',f7.1,1x,f7.1,1x,f7.1,1x,f7.1,1x,f7.1,1x)
 955  format (14x,'Longitude: ',f7.1,1x,f7.1,1x,f7.1,1x,f7.1,1x,f7.1,1x)
 956  format ('  Check all header lines. Yes or No: ',$)
 957  format ('  Give waveform file name chars, * for any name ',$)
 958  format ('  Minimum gap, return for default: ',$)
 959  format ('  Maximum gap, return for default: ',$)
 960  format
     *('  Phases, max 6 phases of 4 chars each ( e.g. P   SSS S): ',$)
 961  format ('  Volcanic subclasses, max 10 classes of 6 char, press en
     *ter after each type: ')


c-variables inicialization

      stcodes = 9
      statusaux = 0

c-for each option where the reading variable is integer or real,
c-it checks iostatus. if iostatus not equal 0, sets variable 
c-stcodes for an error message and current variable reading with default.
c-checks if the current variable reading is blank. If yes, sets variable
c-stcodes for a default message and current variable reading with default.
c-If not, sets variable stcodes for a succesfully input.

c-checking if option equal 1 
         fpsolut = .false.
         fpquality='     '
         if (option(1:3) .eq. '1  ') then
c-writing message and reading value for fault plane solution
c            fpsolut = .false.
             write (6,*) ' '
             write (6,925)   ! asks for fault plane solution
             read (5,'(a)') reading
             call aligncha(reading)
             fpsolutin(1:1) = reading(1:1)
             if (fpsolutin .eq. 'y' .or. fpsolutin .eq. 'Y') then
                 fpsolut = .true.
c
c  now ask for quality
c
c                fpquality=' '
                 write(6,*) 
     *        'Give quality, e.g. A or ABC, max 5 chars, enter for all'
                 read(5,'(a)') fpquality
                 stcodes = 0
             else if (fpsolutin .eq. 'n' .or. fpsolutin .eq. 'N') then 
                 stcodes = 0
             else 
                 stcodes = 2
             end if
         else if (option(1:3) .eq. '20 ') then
c-writing message and reading value for checking all header lines
             header_check = .false.
             write (6,*) ' '
             write (6,956)
             read (5,'(a)') reading
             call aligncha(reading)
             fpsolutin(1:1) = reading(1:1)
             if (reading(1:1) .eq. 'y' .or. reading(1:1) .eq. 'Y') then
                 header_check = .true.
                 stcodes = 0
             else if (reading(1:1) .eq. 'n' .or. reading(1:1) .eq. 'N')
     *       then 
                 stcodes = 0
             else 
                 stcodes = 2
             end if
         else if (option(1:3) .eq. '21 ')then
c-writing message and reading value for getting events with given wave file 
c name
             wave_check = ' '
             write (6,*) ' '
             write (6,957)
             read (5,'(a)') wave_check
             stcodes = 0

c-checking if option equal 2
         else if (option(1:3) .eq. '2  ') then
c-writing message and reading value for earthquake felt
             eqfelt = .false.
             write (6,*) ' '
             write (6,926)
             read (5,'(a)') reading
             call aligncha(reading)
             eqfeltin = reading
             if (eqfeltin .eq. 'y' .or. eqfeltin .eq. 'Y') then
                 eqfelt = .true.
                 stcodes = 0
             else if (eqfeltin .eq. 'n' .or. eqfeltin .eq. 'N') then
                 stcodes = 0
             else 
                 stcodes = 2
             end if

c-checking if option equal 3
         else if (option(1:3) .eq. '3  ') then
c-writing message and reading value for magnitude type
             count = 0
             write (6,*) ' '
             write (6,903)
             read (5,'(a)') reading
             call aligncha(reading)
             do ind = 1,5
                magtypes(ind) = reading(ind:ind)
             end do
c             read (5,'(5a1)') magtypes
             do ind = 1,5
                magtypest(ind) = ' '
c-checking if some wrong value was typed
                if (magtypes(ind) .ne. ' ' .and.
     *              magtypes(ind) .ne. '_' .and.    ! blank magnitudes
     *              magtypes(ind) .ne. 'C' .and. 
     *              magtypes(ind) .ne. 'c' .and.
     *              magtypes(ind) .ne. 'N' .and.    ! no magnitude
     *              magtypes(ind) .ne. 'n' .and.
     *              magtypes(ind) .ne. 'B' .and.
     *              magtypes(ind) .ne. 'b' .and.
     *              magtypes(ind) .ne. 'L' .and.
     *              magtypes(ind) .ne. 'l' .and. 
     *              magtypes(ind) .ne. 'S' .and.
     *              magtypes(ind) .ne. 's' .and.
     *              magtypes(ind) .ne. 'W' .and.
     *              magtypes(ind) .ne. 'w') then
                    stcodes = 3
                    magtypes(ind) = ' '
                else 
c-making upper case for valid values
                    if (magtypes(ind) .eq. 'c') then
                        magtypes(ind) = 'C'
c                   else if (magtypes(ind) .eq. 'b') then
c                       magtypes(ind) = 'B'
                    else if (magtypes(ind) .eq. 'l') then
                        magtypes(ind) = 'L'
                    else if (magtypes(ind) .eq. 'n') then
                        magtypes(ind) = 'N'
c                   else if (magtypes(ind) .eq. 's') then
c                       magtypes(ind) = 'S'
                    else if (magtypes(ind) .eq. 'w') then
                        magtypes(ind) = 'W'
                    end if
                end if
             end do
             index = 5
             do ind = 5,1,-1
                if (magtypes(ind) .ne. ' ') then
                    magtypest(index) = magtypes(ind)
                    index = index - 1
c-counting blank magnitude types
                else if (magtypes(ind) .eq. ' ') then
                    count = count + 1
                end if
             end do
c-checking if number of magnitude type equal 5. If yes, sets 
c-variable stcodes for default message. If no, checks if stcodes
c-still has the original value (9) avoiding to overwrite stcodes
c-in case of different value.
             if (count .eq. 5) then
                 stcodes = 2
             else 
                  if (stcodes .eq. 9) stcodes = 0
             end if
c
c  replace blank types with a *
c
             do i=2,5
               if(magtypes(i).eq.' ') magtypes(i)='*'
             enddo

c-checking if option equal 4
         else if (option(1:3) .eq. '4  ') then
             count = 0 
c-writing message and reading value for distance id
             write (6,*) ' '
             write (6,904)
             read (5,'(a)') reading
             call aligncha(reading)
             do ind = 1,5
                disttype(ind) = reading(ind:ind)
             end do
c             read (5,'(5a1)') disttype
             do ind = 1,5
                disttypet(ind) = ' '
c-checking if some wrong value was typed
                if (disttype(ind) .ne. ' ' .and.
     *              disttype(ind) .ne. 'L' .and.
     *              disttype(ind) .ne. 'l' .and.
     *              disttype(ind) .ne. 'R' .and.
     *              disttype(ind) .ne. 'r' .and.
     *              disttype(ind) .ne. 'D' .and.
     *              disttype(ind) .ne. 'd') then
                    stcodes = 3 
                    disttype(ind) = '%'
                else 
c-making upper case for valid values
                    if (disttype(ind) .eq. 'l') then
                        disttype(ind) = 'L'
                    else if (disttype(ind) .eq. 'r') then
                        disttype(ind) = 'R'
                    else if (disttype(ind) .eq. 'd') then
                        disttype(ind) = 'D'
                    end if
                end if
             end do
             index = 5
             do ind = 5,1,-1
                if (disttype(ind) .ne. ' ' .and. 
     *              disttype(ind) .ne. '%') then
                    disttypet(index) = disttype(ind)
                    index = index - 1
c-counting blank distance id
                else if (disttype(ind) .eq. ' ' .or.
     *                   disttype(ind) .eq. '%') then
                    count = count + 1
                end if
             end do

c-checking if number of magnitude type equal 5. If yes, sets
c-variable stcodes for default message. If no, checks if stcodes
c-still has the original value (9) avoiding to overwrite stcodes
c-in case of different value.
             if (count .eq. 5) then
                 stcodes = 2
             else
                  if (stcodes .eq. 9) stcodes = 0
             end if
             if (disttype(1) .ne. ' ') then
                 do ind = 1,5
c-if blank, making sure the type is set with something not used
                   if (disttype(ind) .eq. ' ') then
                       disttype(ind) = '%'
                   end if
                 end do 
             end if

c-checking if option equal 5
         else if (option(1:3) .eq. '5  ') then
             count = 0
c-writing message and reading value for event id
             write (6,*) ' '
             write (6,905)
             read (5,'(a)') reading
             call aligncha(reading)
             do ind = 1,5
                eventtype(ind) = reading(ind:ind)
             end do
c             read (5,'(5a1)') eventtype
             do ind = 1,5
                eventtypet(ind) = ' '
c-checking if some wrong value was typed
c               if (eventtype(ind) .ne. ' ' .and.
c    *              eventtype(ind) .ne. 'E' .and.
c    *              eventtype(ind) .ne. 'e' .and.
c    *              eventtype(ind) .ne. 'P' .and.
c    *              eventtype(ind) .ne. 'p' .and.
c    *              eventtype(ind) .ne. 'V' .and.
c    *              eventtype(ind) .ne. 'v' .and.
c    *              eventtype(ind) .ne. 'S' .and.
c    *              eventtype(ind) .ne. 's' .and.
c    *              eventtype(ind) .ne. 'Q' .and.
c    *              eventtype(ind) .ne. 'q') then
c                   stcodes = 3
c                   eventtype(ind) = '%'
c               else
c-making upper case for valid values
                    if (eventtype(ind) .eq. 'e') then
                        eventtype(ind) = 'E'
                    else if (eventtype(ind) .eq. 'p') then
                        eventtype(ind) = 'P'
                    else if (eventtype(ind) .eq. 'v') then
                        eventtype(ind) = 'V'
c                   else if (eventtype(ind) .eq. 's') then
c                       eventtype(ind) = 'S'
                    else if (eventtype(ind) .eq. 'q') then
                        eventtype(ind) = 'Q'
                    end if
c               end if
             end do
             index = 5
             do ind = 5,1,-1
                if (eventtype(ind) .ne. ' ' .and.
     *              eventtype(ind) .ne. '%') then
                    eventtypet(index) = eventtype(ind)
                       index = index - 1
c-counting blank event id
                else if (eventtype(ind) .eq. ' ' .or.
     *                   eventtype(ind) .eq. '%') then
                    count = count + 1
                end if
             end do
c-checking if number of magnitude type equal 5. If yes, sets
c-variable stcodes for default message. If no, checks if stcodes
c-still has the original value (9) avoiding to overwrite stcodes
c-in case of different value.
             if (count .eq. 5) then
                 stcodes = 2
             else
                  if (stcodes .eq. 9) stcodes = 0
             end if
             if (eventtype(1) .ne. ' ') then
                 do ind = 1,5
c-if blank, making sure the type is set with something not used
                   if (eventtype(ind) .eq. ' ') then
                       eventtype(ind) = '%'
                   end if
                 end do
             end if


c-checking if option equal 23            ! phase names
         else if (option(1:3) .eq. '23  ') then
             count = 0
c-writing message and reading value for phases
             write (6,*) ' '
             write (6,960)
             read (5,'(6a4)') phase
         else if (option(1:3) .eq. '24  ') then
             write (6,*) ' '
             write (6,961)
             ind=0
1961         continue
             ind=ind+1
             read(5,'(a6)') volcanictype(ind)
             if (seiclen(volcanictype(ind)).gt.0) goto 1961

c-------------------------------------------------------------------------



c-checking if option equal 6
         else if (option(1:3) .eq. '6  ') then
c-writing message and reading value for minimum magnitude 
             write (6,*) ' '
             write (6,906)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:6),'(a6)') minmaga 
             read (reading (1:6),'(f6.0)',err=103,iostat=status) minmag
 103         continue
             if (status .ne. 0) then  
                 stcodes = 1
                 statusaux = status
                 minmag = -990.0
             else
                  if (ichar(minmaga(1:1)) .eq. 32) then
                      minmag = -990.0
                      stcodes = 2
                  else if (ichar(minmaga(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum magnitude
             write (6,*) ' '
             write (6,907)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:6),'(a6)') maxmaga 
             read (reading (1:6),'(f6.0)',err=104,iostat=status) maxmag
 104         continue
             if (status .ne. 0) then  
                 maxmag = 999.0
                 stcodes = 1
             else
                  if (ichar(maxmaga(1:1)) .eq. 32) then
                      maxmag = 999.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxmaga(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if

c-checking if option equal 7
         else if (option(1:3) .eq. '7  ') then
c-writing message and reading value for minimum latitude
             write (6,*) ' '
             write (6,908)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:7),'(a7)') lolata 
             read (reading (1:7),'(f7.0)',err=105,iostat=status) lolat
 105         continue
             if (status .ne. 0) then    
                 statusaux = status
                 lolat = -90.0
                 stcodes = 1
             else
                  if (ichar(lolata(1:1)) .eq. 32) then
                      lolat = -90.0
                      stcodes = 2
                  else if (ichar(lolata(1:1)) .ne. 32) then
                           stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum latitude
             write (6,*) ' '
             write (6,909)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:7),'(a7)') hilata 
             read (reading (1:7),'(f7.0)',err=106,iostat=status) hilat
 106         continue
             if (status .ne. 0) then    
                 hilat = 90.0
                 stcodes = 1
             else
                  if (ichar(hilata(1:1)) .eq. 32) then
                      hilat = 90.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(hilata(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0 
                      end if
                  end if
             end if


c-checking if option equal 22
         else if (option(1:3) .eq. '22 ') then
c-writing message and reading value for minimum gap
             write (6,*) ' '
             write (6,958)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:8),'(a8)') mingapa
             read (reading (1:8),'(f8.0)',err=1077,iostat=status) mingap
 1077        continue
             if (status .ne. 0) then    
                 statusaux = status
                 mingap = 0.0
                 stcodes = 1
             else
                  if (ichar(mingapa(1:1)) .eq. 32) then
                      mingap = 0.0
                      stcodes = 2
                  else if (ichar(mingapa(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum longitude
             write (6,*) ' '
             write (6,959)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:8),'(a8)') maxgapa 
             read (reading (1:8),'(f8.0)',err=1088,iostat=status) maxgap
 1088        continue
             if (status .ne. 0) then     
                 maxgap = 360.0
                 stcodes = 1
             else
                  if (ichar(maxgapa(1:1)) .eq. 32) then
                      maxgap = 360.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxgapa(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if


c-checking if option equal 8
         else if (option(1:3) .eq. '8  ') then
c-writing message and reading value for minimum longitude
             write (6,*) ' '
             write (6,910)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:8),'(a8)') lolona 
             read (reading (1:8),'(f8.0)',err=107,iostat=status) lolon
 107         continue
             if (status .ne. 0) then    
                 statusaux = status
                 lolon = -360.0
                 stcodes = 1
             else
                  if (ichar(lolona(1:1)) .eq. 32) then
                      lolon = -360.0
                      stcodes = 2
                  else if (ichar(lolona(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum longitude
             write (6,*) ' '
             write (6,911)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:8),'(a8)') hilona 
             read (reading (1:8),'(f8.0)',err=108,iostat=status) hilon
 108         continue
             if (status .ne. 0) then     
                 hilon = 360.0
                 stcodes = 1
             else
                  if (ichar(hilona(1:1)) .eq. 32) then
                      hilon = 360.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(hilona(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if


c-checking if option equal 9
         else if (option(1:3) .eq. '9  ') then
c-writing message and reading value for minimum depth
             write (6,*) ' '
             write (6,912)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:5),'(a5)') mindeptha 
             read (reading (1:5),'(f5.0)',err=109,iostat=status) 
     *             mindepth
 109         continue
             if (status .ne. 0) then   
                 statusaux = status
                 mindepth = -99.0
                 stcodes = 1
             else
                  if (ichar(mindeptha(1:1)) .eq. 32) then
                      mindepth = -99.0
                      stcodes = 2
                  else if (ichar(mindeptha(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum depth
             write (6,*) ' '
             write (6,913)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:5),'(a5)') maxdeptha 
             read (reading (1:5),'(f5.0)',err=110,iostat=status) 
     *             maxdepth
 110         continue
             if (status .ne. 0) then    
                 maxdepth = 99999.0
                 stcodes = 1
             else
                  if (ichar(maxdeptha(1:1)) .eq. 32) then
                      maxdepth = 99999.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxdeptha(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if

c-checking if option equal 10
         else if (option(1:3) .eq. '10 ') then
c-writing message and reading value for minimum rms
             write (6,*) ' '
             write (6,927)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:4),'(a4)') minrmsa
             read (reading (1:4),'(f4.0)',err=112,iostat=status) minrms
 112         continue
             if (status .ne. 0) then
                stcodes = 1
                statusaux = status
                minrms = 0
             else
                  if (ichar(minrmsa(1:1)) .eq. 32) then
                      minrms = 0
                      stcodes = 2
                  else if (ichar(minrmsa(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum rms
             write (6,*) ' '
             write (6,901)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:4),'(a4)') maxrmsa
             read (reading (1:4),'(f4.0)',err=101,iostat=status) maxrms
 101         continue
             if (status .ne. 0) then
                stcodes = 1
                maxrms = 999
             else
                  if (ichar(maxrmsa(1:1)) .eq. 32) then
                      maxrms = 999
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxrmsa(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if

c-checking if option equal 11
         else if (option(1:3) .eq. '11 ') then
c-writing message and reading value for minimum number of stations
             write (6,*) ' '
             write (6,902)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:3),'(a3)') minstationa
             read (reading (1:3),'(i3)',err=102,iostat=status)
     *             minstation
 102         continue
             if (status .ne. 0) then
                 stcodes = 1
                 minstation = 0
             else
                  if (ichar(minstationa(1:1)) .eq. 32) then
                      minstation = 0
                      stcodes = 2
                  else if (ichar(minstationa(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum number of stations
             write (6,*) ' '
             write (6,928)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:3),'(a3)') maxstationa
             read (reading (1:3),'(i3)',err=113,iostat=status)
     *             maxstation
 113         continue
             if (status .ne. 0) then
                 stcodes = 1
                 statusaux = status
                 maxstation = 999
             else
                  if (ichar(maxstationa(1:1)) .eq. 32) then
                      maxstation = 999
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxstationa(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if

c-checking if option equal 12 
         else if (option(1:3) .eq. '12 ') then
c-writing message and reading value for minimum hypocenter error latitude
             write (6,*) ' '
             write (6,932)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:7),'(a7)') minhelata
             read (reading (1:7),'(f7.0)',err=114,iostat=status) 
     *             minhelat
 114         continue
             if (status .ne. 0) then
                 statusaux = status
                 minhelat = .0
                 stcodes = 1
             else
                  if (ichar(minhelata(1:1)) .eq. 32) then
                      minhelat = .0
                      stcodes = 2
                  else if (ichar(minhelata(1:1)) .ne. 32) then
                      stcodes=0
                  end if
             end if
c-writing message and reading value for maximum hypocenter error latitude
             write (6,*) ' '
             write (6,933)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:7),'(a7)') maxhelata
             read (reading (1:7),'(f7.0)',err=115,iostat=status) 
     *             maxhelat
 115         continue
             if (status .ne. 0) then
                 maxhelat = 99999.0
                 stcodes = 1
             else
                  if (ichar(maxhelata(1:1)) .eq. 32) then
                      maxhelat = 99999.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxhelata(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if

c-checking if option equal 13 
         else if (option(1:3) .eq. '13 ') then
c-writing message and reading value for minimum  hypocenter error longitude
             write (6,*) ' '
             write (6,934)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:8),'(a8)') minhelona
             read (reading (1:8),'(f8.0)',err=116,iostat=status) 
     *             minhelon
 116         continue
             if (status .ne. 0) then
                 statusaux = status
                 minhelon = .0
                 stcodes = 1
             else
                  if (ichar(minhelona(1:1)) .eq. 32) then
                      minhelon = .0
                      stcodes = 2
                  else if (ichar(minhelona(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum hypocenter error longitude
             write (6,*) ' '
             write (6,935)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:8),'(a8)') maxhelona
             read (reading (1:8),'(f8.0)',err=117,iostat=status) 
     *             maxhelon
 117         continue
             if (status .ne. 0) then
                 maxhelon = 99999.0
                 stcodes = 1
             else
                  if (ichar(maxhelona(1:1)) .eq. 32) then
                      maxhelon = 99999.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxhelona(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if

c-checking if option equal 14 
         else if (option(1:3) .eq. '14 ') then
c-writing message and reading value for minimum  hypocenter error depth
             write (6,*) ' '
             write (6,936)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:5),'(a5)') minhedeptha
             read (reading (1:5),'(f5.0)',err=118,iostat=status)
     *             minhedepth
 118         continue
             if (status .ne. 0) then
                 statusaux = status
                 minhedepth = -99.0
                 stcodes = 1
             else
                  if (ichar(minhedeptha(1:1)) .eq. 32) then
                      minhedepth = -99.0
                      stcodes = 2
                  else if (ichar(minhedeptha(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if
c-writing message and reading value for maximum hypocenter error depth
             write (6,*) ' '
             write (6,937)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:5),'(a5)') maxhedeptha
             read (reading (1:5),'(f5.0)',err=119,iostat=status)
     *             maxhedepth
 119         continue
             if (status .ne. 0) then
                 maxhedepth = 99999.0
                 stcodes = 1
             else
                  if (ichar(maxhedeptha(1:1)) .eq. 32) then
                      maxhedepth = 99999.0
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 2
                      end if
                  else if (ichar(maxhedeptha(1:1)) .ne. 32) then
                      if (stcodes .ne. 1 .and. stcodes .ne. 2) then
                          stcodes = 0
                      end if
                  end if
             end if

c-checking if option equal 15
         else if (option(1:3) .eq. '15 ') then
c-writing message and reading value for minimum number of polarities
             write (6,*) ' '
             write (6,948)
             read (5,'(a)') reading
             call alignnum(reading)
             read (reading (1:5),'(a5)') mnupola
             read (reading (1:5),'(i5)',err=126,iostat=status) mnupol
 126         continue
             if (status .ne. 0) then
                 stcodes = 1
                 statusaux = status
                 mnupol = 0
             else
                  if (ichar(mnupola(1:1)) .eq. 32) then
                      mnupol = 0
                      stcodes = 2
                  else if (ichar(mnupola(1:1)) .ne. 32) then
                      stcodes = 0
                  end if
             end if

c-checking if option equal 16
         else if (option(1:3) .eq. '16 ') then
             do ind = 1,10
                hypaselect(ind) = ' '
             end do
c-writing message and reading value for hypocenter agencies
             hypa_present = 0
             write (6,*) ' '
             write (6,941)
 120  continue
             write (6,914)
             read (5,'(a)') reading
             call aligncha(reading)
             hypaselect(hypa_present + 1) = reading
c             read (5,'(a3)') hypaselect(hypa_present + 1)
             if (hypaselect(hypa_present + 1) .ne. '    ') then
                 hypa_present = hypa_present + 1
                 if (hypa_present .lt. 10) then
                    goto 120
                 else
                    write (6,*) ' '
                    write (6,*) '  Array completed'
                 end if
             end if
             if (hypa_present .eq. 0) then
                 stcodes = 2
             else
                 stcodes = 0
             end if

c-checking if option equal 17
         else if (option(1:3) .eq. '17 ') then
             do ind = 1,10
                magaselect(ind) = ' '
             end do
c-writing message and reading value for hypocenter agencies
             maga_present = 0
             write (6,*) ' '
             write (6,942)
 121  continue
             write (6,914)
             read (5,'(a)') reading
             call aligncha(reading)
             magaselect(maga_present + 1) = reading
c             read (5,'(a3)') magaselect(maga_present + 1)
             if (magaselect(maga_present + 1) .ne. '    ') then
                 maga_present = maga_present + 1
                 if (maga_present .lt. 10) then
                    goto 121
                 else
                    write (6,*) ' '
                    write (6,*) '  Array completed'
                 end if
             end if
             if (maga_present .eq. 0) then
                 stcodes = 2
             else
                 stcodes = 0
             end if

c-checking if option equal 18
         else if (option(1:3) .eq. '18 ') then
             do ind = 1,50
                statselect(ind) = ' '
                mindist(ind)=0
                maxdist(ind)=99999
                stat_phase(ind)=' '
             end do
c-writing message and reading value for station codes
             stat_present = 0
             write (6,*) ' '
             write (6,919)
 100  continue
             write (6,914)
             read (5,'(a)') reading
c             call aligncha(reading)
             statselect(stat_present + 1) = reading(1:7)
             if(reading(8:14).ne.' ') then
                 read(reading(8:14),'(i7)') mindist(stat_present+1) 
                 idist=idist+1    ! count if any distance required
             endif
             if(reading(15:21).ne.' ')  then
                read(reading(15:21),'(i7)') maxdist(stat_present+1) 
                idist=idist+1
             endif
             stat_phase(stat_present+1)=reading(23:26)
c             read (5,'(a5)') statselect(stat_present + 1)
c
             if (statselect(stat_present + 1) .ne. '       ') then
                 stat_present = stat_present + 1
                 if (stat_present .lt. 50) then
                    goto 100
                 else
                    write (6,*) ' '
                    write (6,*) '  Array completed'
                 end if
             end if
             if (stat_present .eq. 0) then
                 stcodes = 2
             else
c
c   enter if all must be present
c
                 write(6,'(a)')
     *           ' Must all stat-comps be present (y/n=return)'
                 read(5,'(a)') reading
                 all_stat=.false.
                 if(reading(1:1).eq.'Y'.or.reading(1:1).eq.'y')
     *           all_stat=.true.
                 stcodes = 0
c
c  if distances used, ask if epicentral or hypocentreal
c
                if(idist.gt.0) then
                    write(6,'(a)')
     *             ' Epecentral (Return) or hypocentral distance (h) ?'
                    read(5,'(a)') reading
                    hyp_dist=.false.
                    if(reading(1:1).eq.'h'.or.reading(1:1).eq.'H')
     *              hyp_dist=.true.
                endif
             end if

c-checking if option equal 19
         else if (option(1:3) .eq. '19 ') then
             do ind = 1,20
                ypol(ind,1) = 0
                ypol(ind,2) = 0
             end do
             count = 0
c-writing message and reading value for polygon points
             write (6,*) ' '
             write (6,951)
 124  continue
             inde1 = 0
             inde2 = 0
             inde3 = 0
             found = .false.
             count = count + 1
             write (6,914)
             read (5,'(a)') reading
             if(reading(1:5).eq.'     ') go to 888   ! end of input
             call sei get values(2,reading,code)
             ypol(count,2)=array$(1)
             ypol(count,1)=array$(2)
             goto 124

ccccccccccccccc skipped, some problems
             
c-finding the exactly position for x, (,), y in the variable
             do while (.not. found .and. inde1 .ne. 80)
               inde1 = inde1 + 1
c               write(6,*) inde1
               if (reading (inde1:inde1) .ne. ' ') found = .true.
             end do
             inde2 = inde1
             if (reading (inde1:inde1) .eq. ',') then

ccc in following section, all 81 has been replaced by 80 feb 98
                 inde1 = 80
             else
                 found = .false.
             end if
             do while (.not. found .and. inde2 .ne. 80)
               inde2 = inde2 + 1
               if (reading (inde2:inde2) .eq. ',' .or.
     *             reading (inde2:inde2) .eq. ' ') found = .true.
             end do
             found = .false.
             inde3 = inde2
             do while (.not. found .and. inde3 .ne. 80)
               inde3 = inde3 + 1
               if (reading (inde3:inde3) .ne. ',' .and.
     *             reading (inde3:inde3) .ne. ' ') found = .true.
             end do
c-reading from the auxiliary variable to the array
             read (reading (inde1:inde2 - 1),'(f5.0)',err=125,
     *             iostat=status) ypol(count,2)   
             read (reading (inde3:inde3 + 5),'(f5.0)',err=125,
     *             iostat=status) ypol(count,1)
 125  continue
c-checking iostatus. if not zero, goes 1 position back in the array
             if (status .ne. 0 .and. (inde1 .ne. 80 .or.
     *           inde2 .ne. 80 .or. inde3 .ne. 80)) then
                 stcodes = 4
                 ypol(count,1) = 0
                 ypol(count,2) = 0
                 count = count - 1
             end if
             if (count .lt. 20) then
c-if inde1/2/3 not 80 means that return was not pressed for end of reading
                 if (inde1 .ne. 80 .or. inde2 .ne. 80 .or.
     *               inde3 .ne. 80) go to 124
             else
                 if (inde1 .ne. 80 .or. inde2 .ne. 80 .or.
     *               inde3 .ne. 80) then
                     write (6,*) ' '
                     write (6,*) '  Array completed'
                     count = count + 1
                 end if
             end if
 888         continue                  ! to jump section not working
             iant = count - 1
             if (iant .eq. 0) then
c-no points were written
                stcodes = 2
             else
                  if (iant .lt. 3) then
c-less than 3 points were written
                      do ind = 1,20
                        ypol(ind,1) = 0
                        ypol(ind,2) = 0
                      end do
                      stcodes = 5
                      iant = 0
                  else 
c-input ok 
                       if (stcodes .ne. 4) stcodes = 0
                  end if
             end if
         end if

c-writing main screen. When default, value is not printed

         write (6,*) ' '
         write (6,*) '            PARAMETERS'
         write (6,*) ' '
         if (fpsolut) then
             write (6,'(6x,a,21x,a)') '1   - Fault Plane Solution',
     *                                ' Yes'
         else
             write (6,'(6x,a)') '1   - Fault Plane Solution'
         end if
         if (eqfelt) then
             write (6,'(6x,a,27x,a)') '2   - Earthquake Felt','Yes' 
         else
             write (6,'(6x,a)') '2   - Earthquake Felt'
         end if
         write (6,'(6x,a,23x,5a1)') '3   - Magnitude Type(s)',
     *                            magtypest
         write (6,'(6x,a,26x,5a1)') '4   - Distance ID(s)',disttypet
         write (6,'(6x,a,29x,5a1)') '5   - Event ID(s)',eventtypet
         if (minmag .ne. -990 .or. maxmag .ne. 999) then
             write (6,921) minmag, maxmag
         else
             write (6,'(6x,a)') '6   - Magnitude Limits'
         end if
         if (lolat .ne. -90 .or. hilat .ne. 90) then
             write (6,922) lolat, hilat
         else 
             write (6,'(6x,a)') '7   - Latitude Limits'
         end if
         if (lolon .ne. -360.0 .or. hilon .ne. 360.0) then
             write (6,923) lolon, hilon
         else
             write (6,'(6x,a)') '8   - Longitude Limits'
         end if
         if (mindepth .ne. -99 .or. maxdepth .ne. 99999) then
             write (6,924) mindepth, maxdepth
         else 
             write (6,'(6x,a)') '9   - Depth Limits'
         end if
         if (minrms .ne. 0 .or. maxrms .ne. 999) then
             write (6,920) minrms, maxrms
         else
             write (6,'(6x,a)') '10  - RMS Limits'
         end if  
         if (minstation .ne. 0 .or. maxstation .ne. 999) then
             write (6,'(6x,a,a,17x,i3,8x,i3)') '11  - Number of '
     *              ,'Stations Limits', minstation, maxstation
         else
             write (6,'(6x,a)') '11  - Number of Stations Limits'
         end if
         if (minhelat .ne. 0. .or. maxhelat .ne. 99999.) then
             write (6,929) minhelat, maxhelat
         else
             write (6,'(6x,a,a)') '12  - Hypocenter Errors Latitude',
     *                          ' Limits'
         end if
         if (minhelon .ne. 0. .or. maxhelon .ne. 99999.) then
             write (6,930) minhelon, maxhelon
         else
             write (6,'(6x,a,a)') '13  - Hypocenter Errors Longitude',
     *                          ' Limits'
         end if
         if (minhedepth.ne. -99 .or. maxhedepth .ne. 99999) then
             write (6,931) minhedepth, maxhedepth
         else
             write (6,'(6x,a,a)') '14  - Hypocenter Errors Depth Limits'
         end if
         if (mnupol .eq. 0) then
             write (6,949)
         else
             write (6,950) mnupol
         end if
         if (hypa_present .eq. 0) then
             write (6,943)
         else 
             write (6,944) (hypaselect(ind),ind = 1,10)
         end if
         if (maga_present .eq. 0) then
             write (6,945)
         else
             write (6,946) (magaselect(ind),ind = 1,10)
         end if
         if (stat_present .eq. 0) then
             write (6,938)
         else if (stat_present .ge. 1 .and.
     *            stat_present .le. 6) then
             write (6,939) (statselect(ind),ind = 1,6)
         else if (stat_present .ge. 7 .and.
     *            stat_present .le. 12) then
             write (6,939) (statselect(ind),ind = 1,6)
             write (6,940) (statselect(ind),ind = 7,12)
         else if (stat_present .ge. 13 .and.
     *            stat_present .le. 16) then
             write (6,939) (statselect(ind),ind = 1,6)
             write (6,940) (statselect(ind),ind = 7,12)
             write (6,940) (statselect(ind),ind = 13,18)
         else if (stat_present .ge. 19 .and.
     *            stat_present .le. 24) then
             write (6,939) (statselect(ind),ind = 1,6)
             write (6,940) (statselect(ind),ind = 7,12)
             write (6,940) (statselect(ind),ind = 13,18)
             write (6,940) (statselect(ind),ind = 19,24)
         else if (stat_present .ge. 25 .and.
     *            stat_present .le. 30) then
             write (6,939) (statselect(ind),ind = 1,6)
             write (6,940) (statselect(ind),ind = 7,12)
             write (6,940) (statselect(ind),ind = 13,18)
             write (6,940) (statselect(ind),ind = 19,24)
             write (6,940) (statselect(ind),ind = 25,30)
         end if
         if (iant .eq. 0) then
             write (6,947)
         else if (iant .le. 5) then
              write (6,947)
              write (6,954) (ypol(ind,2),ind = 1,iant)
              write (6,955) (ypol(ind,1),ind = 1,iant)
         else if (iant .le. 10) then
              write (6,947)
              write (6,954) (ypol(ind,2),ind = 1,5)
              write (6,955) (ypol(ind,1),ind = 1,5)
              write (6,*) ' '
              write (6,954) (ypol(ind,2),ind = 6,iant)
              write (6,955) (ypol(ind,1),ind = 6,iant)
         else if (iant .le. 15) then
              write (6,947)
              write (6,954) (ypol(ind,2),ind = 1,5)
              write (6,955) (ypol(ind,1),ind = 1,5)
              write (6,*) ' '
              write (6,954) (ypol(ind,2),ind = 6,10)
              write (6,955) (ypol(ind,1),ind = 6,10)
              write (6,*) ' '
              write (6,954) (ypol(ind,2),ind = 11,iant)
              write (6,955) (ypol(ind,1),ind = 11,iant)
         else if (iant .le. 20) then
              write (6,947)
              write (6,954) (ypol(ind,2),ind = 1,5)
              write (6,955) (ypol(ind,1),ind = 1,5)
              write (6,*) ' '
              write (6,954) (ypol(ind,2),ind = 6,10)
              write (6,955) (ypol(ind,1),ind = 6,10)
              write (6,*) ' '
              write (6,954) (ypol(ind,2),ind = 11,15)
              write (6,955) (ypol(ind,1),ind = 11,15)
              write (6,*) ' '
              write (6,954) (ypol(ind,2),ind = 16,iant)
              write (6,955) (ypol(ind,1),ind = 16,iant)
         end if
         if (header_check) then
            write (6,'(6x,a,27x,a)') '20  - Use all header lines','Yes' 
         else
             write (6,'(6x,a)') '20  - Use all header lines'
         end if
         if(wave_check.ne.' ') then
            write(6,'(6x,a,a,7x,a)') '21  - Look for wave form file',
     *      ' names ',wave_check
         else
            write(6,'(6x,a,a,7x)')    '21  - Look for wave form file',
     *      ' names'
         endif

         if(maxgap.ne.360.0.or.mingap.ne.0.0) then
            write(6,'(6x,a,a,7x,f6.1,6x,f6.1)') 
     *      '22  - Gap range               ',
     *      '       ',mingap,maxgap
         else
            write(6,'(6x,a,a,7x)')    '22  - Gap range              ',
     *      '     '
         endif
         write(6,'(6x,a,a,7x,6a4)')   '23  - Phases                 ',
     *      '     ',phase
         write(6,'(6x,a,a,7x,10a6)')   '24  - Volcanic subclasses    ',
     *      '     ',volcanictype


         write (6,*) ' '

c-checkins status codes and printing messages.
c-stcodes equal 0 - everything is ok
c-stcodes equal 1 - status not equal 0
c-stcodes equal 2 - default assumed
c-stcodes equal 3 - some wrong value
c-stcodes equal 4 - wrong polygon point not considerated 
c-stcodes equal 5 - less then 3 points defined
         if (stcodes .eq. 0) then
             if (statusaux .ne. 0) then
                 write (6,915)
                 statusaux = 0 
             else if (statusaux .eq. 0) then
                 write (6,918)
             end if
         else if (stcodes .eq. 1) then
             write (6,915)
         else if (stcodes .eq. 2) then
             write (6,917)
         else if (stcodes .eq. 3) then
             write (6,916)
         else if (stcodes .eq. 4) then
             write (6,952)
         else if (stcodes .eq. 5) then
             write (6,953)
         end if
         if (option(1:1) .eq. '?') then
             write (6,*) '  Invalid Option.'
         end if
         write (6,900)
         read (5,'(a)') option
         call alignnum(option)

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SUBROUTINE THAT SELECTS EVENTS AFTER GIVEN
C     CRITERIAS GIVEN IN COMMON BLOCKS
C
C     INPUT: STRING.  FIRST LINE IN THE FILE (NORDIC FORMAT)
C     OUTPUT: ALL PARAMETERS OF EVENT IN COMMON BLOCKS:'EPIVALUES'
C             AND 'EPICHAR'
C             ACCEPTED: LOGICAL THAT CAN BE TESTED ON IN THE CALLING PROGRAM
C             TIME,YEAR,MONTH,DAY,HOUR,MINUTES,SEC: TIME OF THE EVENT
C             LATI,LONG,DEPTH: LOCATION OF EVENT
C             MAG1,MAG2,MAG3,RMSVALUE,NUMBEROFSTATIONS,
C             AGENCY,MAGAGENCY,MAGTYPE
C             DEPTHID :  OTHER EVENT CHARACTERISTICS
c             start_of_time: true if not yet at start of time interval
c             end_of_time: true if behind end  of time interval
C
      SUBROUTINE EPISELEC(STRING,start_of_time,end_of_time)
      implicit none
      include 'seidim.inc'
      CHARACTER*80 STRING
	  CHARACTER*1 EVENTTYPE(5),DISTTYPE(5),MAGTYPES(5)
C--MAGNITUDE TYPE AND AGENCY
      CHARACTER*1 MAGT(3),MAGA(3)
C--MAGNITUDE IN ASCII
      CHARACTER*4 XMAG(3)
C--COUNTER FOR MAGNITUDES
      INTEGER IMAG
      CHARACTER DISTANCEID*1,EVENTID*1,DEPTHID*1,AGENCY*3,
     *           MAGTYPE*1,MAGAGENCY*3
      character*1 distanceid_local   ! for use inside routine

C  NUMBER OF STATIONS TO BE PRESENT
      INTEGER 		STAT_present		
      logical       all_stat         ! if true,  all must be present
c  number of hypocenter agencies to be present
      integer           hypa_present
c  number of magnitude agencies to be present
      integer           maga_present
C  STATION CODES FOR ABOVE
      CHARACTER*7 	STATSELECT(50)
      character*4       stat_phase(50)
      integer mindist(50),maxdist(50)  ! distance range for station	
c  hypocenter agencies
      character*3       hypaselect(10)
c  magnitude agencies
      character*3       magaselect(10)
      logical start_of_time,end_of_time
      INTEGER YEAR,MONTH,DAY,MINUTES,NUMBEROFSTATIONS,HOUR,
     *          MINSTATION,MAXSTATION,century
      double precision time1,time2,timcen,time
      REAL SEC,LATI,LONG,DEPTH,RMSVALUE,SMAG,LOLAT,
     *        HILAT,LOLON,HILON,MINMAG,MINRMS,MAXRMS,MAXMAG,
     *        MAXDEPTH,MINDEPTH,mag,maxgap,mingap,
     *     minhelat, maxhelat,
     *     minhelon, maxhelon,
     *     minhedepth, maxhedepth
      LOGICAL ACCEPTED,DEFAULT,MGT
      integer i,ind,nstat
c   fault plane solution
      logical fpsolut
c   earthquake felt
      logical eqfelt
      character*40 wave_check    ! string to identify wave form file name
      logical header_check        ! checking all headers
      real ypol(max_polyn$,2)    ! array defining polygon
      integer iant               ! number of points of the polygon
      logical insidepol          ! true - event is inside polygon
      logical hyp_dist           ! if true, use hypocentral distance

      integer mnupol             ! minumum number of polarities
      character*4 phase(6)       ! phase names to select
      character*6 volcanictype(10)   ! volcanic subclasses
      character*5  fpquality     ! quality of fps solution
c

      COMMON/EPIVALUE/YEAR,MONTH,DAY,HOUR,MINUTES,SEC,LATI,LONG,DEPTH,
     *                 NUMBEROFSTATIONS,RMSVALUE,MAG,ACCEPTED,nstat
C
      COMMON/EPICHAR/DISTANCEID,EVENTID,DEPTHID,AGENCY,
     *                    MAGTYPE,MAGAGENCY
C
      common/epicrite/hilat,lolat,hilon,
     *                lolon,minmag,maxmag,minrms,maxrms,mindepth,
     *                maxdepth,minhelat,maxhelat,minhelon,maxhelon,
     *                minhedepth,maxhedepth,ypol,iant,mnupol
      common/epicrit2/fpsolut,eqfelt,statselect
      common/epicrit1/mindist,maxdist,
     *                hypaselect,magaselect 
      common/epicrit3/default,eventtype,disttype,magtypes,volcanictype
      common/epicrit4/minstation,maxstation,stat_present,hypa_present,
     *                maga_present,header_check,wave_check
      common/epicrit5/time1,time2,timcen,all_stat,mingap,maxgap,phase,
     *                stat_phase,hyp_dist,fpquality
C
      READ(STRING,10,ERR=20)
cxx    *CENTURY,YEAR,MONTH,DAY,HOUR,MINUTES,SEC,DISTANCEID_LOCAL,
     *YEAR,MONTH,DAY,HOUR,MINUTES,SEC,DISTANCEID_LOCAL,
     *           EVENTID,LATI,LONG,DEPTH,DEPTHID,AGENCY,
     *           NUMBEROFSTATIONS,RMSVALUE,XMAG(1),MAGT(1),MAGA(1),
     *           XMAG(2),MAGT(2),MAGA(2),XMAG(3),MAGT(3),MAGA(3)
cxx10    FORMAT(1X,2I2,1x,2I2,1X,I2,I2,1X,F4.0,1X,A1,A1,F7.0,F8.0,F5.0,
10    FORMAT(1X,i4,1x,2I2,1X,I2,I2,1X,F4.0,1X,A1,A1,F7.0,F8.0,F5.0,
     *      A1,1X,A3,I3,F4.1,A4,A1,A3,A4,A1,A3,A4,A1,A3)
c
c   if number of stations is there, there might be stations since there will
c   only be a number in the header if the event could be located, therefore
c   check with nstat
c
      if(numberofstations.eq.0) numberofstations=nstat
      goto 25
c
c  give error message and try next record
c
20    CONTINUE
         write(6,*)'Something wrong with input record'
         write(6,'(1x,a)') string
         accepted=.false.
         goto 9999                        ! Return to caller.
25    continue


      ACCEPTED=.FALSE.
      end_of_time=.false.
      start_of_time=.false.
c                                                                               
c   convert to seconds since 1900, even if century is not 1900
c   since this is corrected for below. 
c                                                                               
c
c   add century, assume 1900 if not there 
c
cxx      if(century.eq.0) century=19
cxx      year=year+1900
      call timsec(year,month,day,hour,minutes,sec,time)                        
cxx      time=time+century*timcen
ctest     write(6,*)'temcen,century',timcen,century
ctest             write(6,*)'time,time1,time2',time,time1,time2
         if(time.gt.time2) end_of_time=.true.
         if(time.lt.time1) start_of_time=.true.
         if(end_of_time.or.start_of_time) goto 99
C
C
C     RETURN EPIVALUESPARAMETERS IF NO SELECTION PARAMETERS
C     EXCEPT TIME ARE ACTIVE
C    
      IF(DEFAULT)THEN
            ACCEPTED=.TRUE.
            goto 9999                        ! Return to caller.
      ENDIF
C
C     MAGNITUDE SELECTION, TYPE MUST BE CORRECT OR DEFAULT INDICATED
C     BY MAGTYPES(1).EQ.' '. IF TYPE IS OK, LARGEST MAGNITUDE IS SELECTED
C     OF THE TYPES PERMITTED. IF XMAG IS BLANK, THERE IS NO MAGNITUDE
C
c
c   special option, if magtype is N, only events with no magnitude in 
c   first position is chosen
c
      if(magtypes(1).eq.'N'.and.xmag(1).eq.'    ') goto 501
      if(magtypes(1).eq.'_') magtypes(1)='_'
   
      MAG=-1000.0
C
C   FIRST CHECK IF MAG TYPE OK
C
      MGT=.FALSE.
      IF(MAGTYPES(1).EQ.' ') MGT=.TRUE.
      IF(.NOT.MGT) THEN
         DO IMAG=1,3
            IF(XMAG(IMAG).NE.'    ') THEN
               DO I=1,5
                  IF(MAGT(IMAG).EQ.MAGTYPES(I)) MGT=.TRUE.
                  if(magt(imag).eq.' '.and.magtypes(i).eq.'_') 
     *            mgt=.true.   ! magnitudes with no magnitude type
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      IF(.NOT.MGT) GOTO 99
C
C   SELECT FOR MAGNITUDE SIZE, IF MINMAG IS -990, and
c   maxmag is 999.0, NO SELECTION ON MAG
C
      IF(.not.(MINMAG.eq.-990.0.and.maxmag.eq.999.0)) then
         DO IMAG=1,3
            IF(XMAG(IMAG).NE.'    ') THEN
               DO I=1,5
                  IF(MAGT(IMAG).EQ.MAGTYPES(I).OR.MAGTYPES(1).EQ.' ')
     *            THEN
                     READ(XMAG(imag),'(F4.1)',err=9464) SMAG
                     goto 9465
 9464                continue
                     write(6,*)'error in header line with mag'
                     write(6,'(a)') string(1:79)
 9465                continue
                     IF(SMAG.GT.MAG) THEN
                        MAG=SMAG
                        MAGAGENCY=MAGA(IMAG)
                        MAGTYPE=MAGT(IMAG)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IF (MAG.LT.MINMAG.OR.MAG.GT.MAXMAG)GO TO 99
      ENDIF
C
  501 continue                  ! enter here if special mag selection
c
      if (numberofstations .lt. minstation .or.
     *    numberofstations .gt. maxstation) go to 99
C	  
C   SELECT FOR EVENT TYPES
C
      if(disttype(1).eq.'_'.or.disttype(1).eq.'%') then
         disttype(1)='L' 
         disttype(2)='R'
         disttype(3)='D'
      endif
      IF(DISTTYPE(1).NE.' ') THEN
         DO I=1,5
           IF(DISTANCEID_LOCAL.EQ.DISTTYPE(I)) GOTO 50
         ENDDO
         GOTO 99
      ENDIF
 50   CONTINUE
C
C   SELECT ON EVENTTYPE, A BLANK IN NORDIC FILE MEANS EARTHQUAKE,
C   WHICH IS INDICATED BY Q IN SELECTION CRITERIAS. IF EVENTTYPE(1)
C   IS BLANK, ALL EVENT TYPES ARE USED
c
c   if eventtype(1) is '_', is same as blank
C
      if(eventtype(1).eq.'_') eventtype(1)=' '
	  
      IF(EVENTTYPE(1).NE.' ') THEN
	     DO I=1,5
		    IF(EVENTID.EQ.EVENTTYPE(I)) GOTO 55
            IF(EVENTID.EQ.' '.AND.EVENTTYPE(I).EQ.'Q') 
     *      GOTO 55
         ENDDO
         GOTO 99
      ENDIF
 55   CONTINUE
c
c   if there is no location and lat lon depth is not default
c   event is not selected
c
      if (string(24:30).eq.'       ') then
c changed lo 08 March 2001 for seisweb
        if
     *    ((hilat.ne.90.0.or.hilon.ne.360.0.or.lolat.ne.-90.0.
     *    or.lolon.ne.-360.0).and.     
     *    (hilat.ne.90.0.or.hilon.ne.180.0.or.lolat.ne.-90.0.
     *    or.lolon.ne.-180.0)) then
            goto 99
        endif
      endif
      IF (LATI.LT.LOLAT.OR.LATI.GT.HILAT) GO TO 99
      if (lolon.gt.0.0.and.hilon.lt.0.0) then           ! case if across date line
         if(long.gt.0.0.and.long.lt.lolon) goto 99
         if(long.lt.0.0.and.long.lt.lolon) goto 99
      else
         IF (LONG.LT.LOLON.OR.LONG.GT.HILON) GO TO 99
      endif
      IF (DEPTH.LT.MINDEPTH.OR.DEPTH.GT.MAXDEPTH) GO TO 99
c
c   negative rms means larger than
c
      IF (maxrms .lt. 0.0) then
          if (rmsvalue .ge. abs(minrms) .and.
     *        rmsvalue .le. abs(maxrms)) goto 999
          goto 99
      else
          if (rmsvalue .lt. minrms .or.
     *        rmsvalue .gt. maxrms) go to 99
      endif

c
c   calling routine for check if event is inside polygon
c
      if (iant .ge. 3) then
          call polos(lati,long,ypol,iant,insidepol)
          if (.not. insidepol) go to 99
      end if

c  
c   check for required hypocenter agencies if wanted, at least one of agencies 
c   in hypaselect must be present to select event
      if (hypa_present .gt. 0) then
          do ind=1,hypa_present
            if (hypaselect(ind) .eq. agency) go to 999
          end do
          go to 99
      end if
c 
c   check for required magnitude agencies if wanted, at least one of agencies
c   in magaselect must be present to select event
      if (maga_present .gt. 0) then
          do ind=1,maga_present
            if (magaselect(ind) .eq. string (61:63)) go to 999
            if (magaselect(ind) .eq. string (69:71)) go to 999
            if (magaselect(ind) .eq. string (77:79)) go to 999
          end do
          go to 99
      end if
C
      GO TO 999
99    ACCEPTED=.FALSE.         
      goto 9999                                ! Return to caller.
999   ACCEPTED=.TRUE.
C
C    Return to caller...
C    ===================
C
9999  return
      END

      subroutine epibase(basename,starttime,endtime,
     *nstat,nphase,nhead,nrecord,typ,exp,data,id,status,compact,nout,
     *ncat_files,cat_files)
c
c     Subroutine to operate on the epicenter database.
c     Routine is called in a loop, and returns one event
c     return variables nstat etc as in main program
c
c     input: basename: e.g. agency 
c            starttime: 14 char standard time
c            endtime:   --------------------- end time
c            century1,century2: start and end century
c            ncat_files : number of cat files
c            cat_files  : cat file names
c     output: standard
c             status: 0: ok
c                     1: end of data base, if -1 on input, just write statistics
      implicit none
c--standard variables
      integer nstat,nphase,nhead,nrecord,id
      character*1 typ,exp
      character*80 data(*)
      character*80 cat_files(*)
      integer ncat_files
      integer nout_start                 ! # events at start of new cat file
      integer n_event_cat                ! coute events in one cat file
c-- number of events selected so far
      integer nout
c-- name of file to read
      character*80 filename	
c! data base name
      character*80 basename	  	
c-- status of read 0: ok 1: end of data
      integer status				
c-- times
      integer time1,time2,time			
      character*14 starttime,endtime
c  help text
c-- epi file name
      character*80 epifile
c  top directory structure
      character*60 top_directory	
c-- century
c-- logicals for file open etc
      logical epi_open,base_open,compact
c--help variables
      integer i2,nfile
      character*1 dchar
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
      integer*4 
     &          read2,                     ! Ditto 2.
     &          code                       ! Local condition.
      logical*4 b_flag                     ! Flag end of file?.
      character chr_text *(80)             ! Text string.
C
C     *********** End of inserts *********
c
c     inizialize
c
      if(status.eq.-1) goto 999
      status=0
      call dir_char(dchar)
c
c   open epicenter data base meaning finding correct position in list
c   of data base file names according to start time
c
         if(.not.base_open) then
            nout_start=0
c
c  get starttime and end time to nearest month
c
            read(starttime(1:6),'(i6)') time1
            read(endtime(1:6),'(i6)') time2
            write(6,*)' '
c
c  get topdirectory structure
c
            call topdir(top_directory)
            i2=index(top_directory,' ')-1
            base_open=.true.
c
c   find base index file name
c
            filename(1:i2)=top_directory(1:i2)
            filename(i2+1:i2+1)=dchar
            filename(i2+2:i2+4)='REA'
            filename(i2+5:i2+5)=dchar
            filename(i2+6:i2+10)=basename(1:5)
            filename(i2+11:i2+11)=dchar
            filename(i2+12:i2+14)='CAT'
            filename(i2+15:i2+15)=dchar
c           
c   position pointer at first cat file to use 
c
            nfile=0
 450        continue                                    ! loop to find file
               nfile=nfile+1                            ! next file
               if(nfile.gt.ncat_files)     goto 460     ! last file
               epifile=cat_files(nfile)                 ! select file
               read(epifile,'(i6)') time                ! start time of file
C
c   check if containing start time
c
               if(time.gt.time1) then                   ! after start time
                  nfile=nfile-1                         ! back one file
                  if(nfile.le.0) nfile=1
                  goto 470                              ! found, go out
               else
                   goto 450                             ! try next
               endif
c
 460        continue
c
c   no file was found that has a start time equal to or smaller than 
c   requested time. However, if there was at least one cat file covering  
c   several months, start time could be in that file. Therefore try to use 
c   cat file before.
c
            if(nfile.gt.0) then
                 nfile=nfile-1
               if(nfile.le.0) nfile=1
            endif
 470        continue
            nfile=nfile-1     ! -1 since added again before oepning below
         endif                ! end of block finding right first epi file 
c
c  check if epifile is open
c
 475  continue
C
      if(.not.epi_open) then
         nfile=nfile+1
         if(nfile.gt.ncat_files) goto 480
c
c   check that next file is not beyond time limit
c
            epifile=cat_files(nfile)
            read(epifile,'(i6)') time
            if(time.gt.time2) goto 480
            chr_text = filename(1:i2+15) //  ! Construct filename.
     &                 epifile               !
            call sei open( old$,             ! Open file (default=stop on error)
     &                     ' ',              ! Prompt (n/a).
     &                     chr_text,         ! Filename.
     &                     read2,            ! Unit opened on.
     &                     b_flag,           ! Flag.
     &                     code )            ! Returned condition.
c
c   check if compact file
c
            call nortype(read2,compact)      !
            if(compact) write(6,*)' File type is compact'
            epi_open=.true.
            n_event_cat=0                    ! count number of events in file
            nout_start=nout                  ! save # of events selected
            goto 485
C
C    End of list...Close down filenr.lis and return to caller...
C    -----------------------------------------------------------
C
480         continue 
            status=1
            goto 9999                          ! Return to caller.
C
 485        continue
         endif
c
c   read one event
c
        if(.not.compact) then
           call indata(read2,nstat,nphase,nhead,  !
     &                 nrecord,typ,exp,data,id)
        else
           read(read2,'(a)',iostat=code) data(1)  ! Read record.
           call sei code(fort$,                   ! Stop on error.
     &                   code,                    ! Condition.
     &                   read2,                   ! Of fortran unit.
     &                   b_flag)                  ! Flag end of file.
           if( b_flag ) goto 150                  ! & go there!!.
C
           nstat=0
           nphase=0
           nhead=1
           nrecord=1
           typ=data(1)(22:22)
           exp=data(1)(23:23)
           goto 151
 150       continue
              nrecord=0
 151       continue
        endif
c
c   check if end of file
c
 999    continue
        if(nrecord.eq.0.or.status.eq.-1) then
           epi_open=.false.
           if(status.eq.-1) n_event_cat=n_event_cat-1
c
c   write out number of events selected so far and events in cat file
c
           write(6,'(1x,a,2x,a,i7,2x,a,i6,2x,a,i7)') 
     *     epifile(1:10),'No of events:', 
     *     n_event_cat,'Selected:',nout-nout_start,
     *     'Selected total:', nout
           if(status.ne.-1) then
              call sei close(close$,read2,code ) ! Close but stop on error.
              goto 475
           else
              return
           endif
        endif
        n_event_cat=n_event_cat+1              ! count number of events
        status=0
C
C    Return to Caller...
C    ===================
C
9999    return
        end



      subroutine alignnum(reading)
      implicit none
      integer ind, index                    ! index
      integer counter                       ! counter
      character*80 auxreading               ! aux for reading
      character*80 reading                  ! reading

      index = 0
      counter = 0

c-selecting only numeric characters
      do ind = 1,80
         if (reading(ind:ind) .eq. '0'.or. 
     *       reading(ind:ind) .eq. '1' .or.
     *       reading(ind:ind) .eq. '2' .or.
     *       reading(ind:ind) .eq. '3' .or.
     *       reading(ind:ind) .eq. '4' .or.
     *       reading(ind:ind) .eq. '5' .or.
     *       reading(ind:ind) .eq. '6' .or.
     *       reading(ind:ind) .eq. '7' .or.
     *       reading(ind:ind) .eq. '8' .or.
     *       reading(ind:ind) .eq. '9' .or.
     *       reading(ind:ind) .eq. '-' .or.
     *       reading(ind:ind) .eq. '+' .or.
     *       reading(ind:ind) .eq. '.') then
             index = index + 1
             auxreading (index:index) = reading (ind:ind)
         else
             if (reading(ind:ind) .eq. ' ') counter = counter + 1
         end if
      end do
c-if counter ne 80 means some not blank character has been reading
c-if index eq 0 means no valid character has been reading
c-checking if there is some unvalid character
      if (counter .ne. 80 .and. index .eq. 0) then
          index = index + 1
          auxreading (index:index) = '?'
      end if
c-completing variable whit blank
      do while (index .ne. 80)
         index = index + 1
         auxreading (index:index) = ' '
      end do
      reading = auxreading
      end


      subroutine aligncha(reading)
      implicit none
      integer ind, index                    ! index
      character*80 auxreading               ! aux for reading
      character*80 reading                  ! reading

      index = 0

c-moving non blank characters for the begining
      do ind = 1,80
         if (reading(ind:ind) .ne. ' ') then
             index = index + 1
             auxreading (index:index) = reading (ind:ind)
         end if
      end do
c-completing variable whit blank
      do while (index .ne. 80)
         index = index + 1
         auxreading (index:index) = ' '
      end do
      reading = auxreading
      end


      subroutine selcrit_prompt
     *(narg,arg,starttime,endtime,basename,seisweb,
     * web_out)
c
c   routine fills in choises if from prompt line
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      integer   seiclen    ! function
      integer narg         ! number of arguments
      character*80 arg(*)  ! arguments
      character*14 starttime, endtime !  start time and end time inc. century
c  abs times

      double precision time1,time2,timcen
c  number of stations to be present
      integer           stat_present
      logical       all_stat         ! if true,  all must be present
c  number of hypocenter agencies to be present
      integer           hypa_present
c  number of magnitude agencies to be present
      integer           maga_present
      logical default
      logical seisweb  ! true if called from seisweb
      real minmag, minrms, maxrms, hilat, lolat, hilon, lolon, maxmag,
     *     mindepth, maxdepth,maxgap,mingap,
     *     minhelat, maxhelat,
     *     minhelon, maxhelon,
     *     minhedepth, maxhedepth
      integer minstation, maxstation, i, ind, j
      character*1 eventtype(5), disttype(5), magtypes(5)
      character*7 statselect(50)
      character*4 stat_phase(50)
      integer mindist(50),maxdist(50)  ! distance range for station	
      character*3 hypaselect(10), magaselect(10)
      character*80 basename      ! data base of file name of input
      character*80 userid        ! id for user, used by seisweb
c   fault plane solution
      logical fpsolut
c   earthquake felt
      logical eqfelt
      character*40 wave_check    ! string to identify wave form file name
      character*80 web_out       ! path and file name of web output
      logical header_check
      real ypol(max_polyn$,2)    ! array defining polygon
      integer iant               ! number of points of the polygon
      integer mnupol             ! minumum number of polarities
      integer code               ! error code
      logical hyp_dist           ! if true, use hypocentral distance
      character*4 phase(6)       ! phase names to select
      character*6 volcanictype(10)   ! volcanic subclasses
      character*5  fpquality     ! quality of fps solution
c

      common/epicrite/hilat,lolat,hilon,
     *                lolon,minmag,maxmag,minrms,maxrms,mindepth,
     *                maxdepth,minhelat,maxhelat,minhelon,maxhelon,
     *                minhedepth,maxhedepth,ypol,iant,mnupol
      common/epicrit2/fpsolut,eqfelt,statselect
      common/epicrit1/mindist,maxdist,
     *                hypaselect,magaselect 
      common/epicrit3/default,eventtype,disttype,magtypes,volcanictype
      common/epicrit4/minstation,maxstation,stat_present,hypa_present,
     *                maga_present,header_check,wave_check
      common/epicrit5/time1,time2,timcen,all_stat,mingap,maxgap,phase,
     *                stat_phase,hyp_dist,fpquality

      starttime = '0500'
      endtime ='2050'
      minstation = 0
      maxstation = 999
      hilat = 90.0 
      lolat = -90.0
      hilon = 360.0
      lolon = -360.0
      minmag = -990.0
      maxmag = +999.0
      minrms = 0
      maxrms = 999.0
      mindepth = -99.0
      maxdepth = 99999.0 
      minhelat = .0
      maxhelat = 99999.0
      minhelon = .0
      maxhelon = 99999.0
      minhedepth = -99.0
      maxhedepth = 99999.0
      mingap=0.0
      maxgap=360.0
      do ind = 1,5
         magtypes(ind) = ' '
         disttype(ind) = ' '
         eventtype(ind) = ' '
      end do
      do ind = 1,10
        magaselect(ind)=' '
        hypaselect(ind)=' '
      enddo
      default = .false.
      fpsolut = .false.
      eqfelt = .false.
c
c   return if not enough arguments
c
      if(narg.lt.3) return

c
c   read arguments to fill in choises
c
      do i=1,narg
         if(arg(i)(1:7).eq.'-userid') then
             userid=arg(i)
         endif
         if(arg(i)(1:8).eq.'-web_out') then
            web_out=arg(i+1) 
c
c        clear output file for seisweb call
c
           open(1,file=web_out(1:seiclen(web_out))
     &//'.head',status='unknown')
           endfile 1 
           close(1)                  ! need to be clear up
         endif

         if(arg(i)(1:8).eq.'-seisweb') then
             seisweb=.true.
         endif
         if(arg(i)(1:5).eq.'-base') then
             basename=arg(i+1)
         endif
         if(arg(i)(1:7).eq.'-mgtype') then
             do j=1,5
                magtypes(j)=arg(i+1)(j:j)
             enddo
         endif
         if(arg(i)(1:9).eq.'-disttype') then
             do j=1,5
                disttype(j)=arg(i+1)(j:j)
             enddo
         endif
         if(arg(i)(1:10).eq.'-eventtype') then
             do j=1,5
                eventtype(j)=arg(i+1)(j:j)
                if(eventtype(j).eq.' ') eventtype(j)='%'
             enddo
         endif

         if(arg(i)(1:5).eq.'-area') then
             call sei get values(1,arg(i+1),code)
             lolat=array$(1)
             call sei get values(1,arg(i+2),code)
             hilat=array$(1)
             call sei get values(1,arg(i+3),code)
             lolon=array$(1)
             call sei get values(1,arg(i+4),code)
             hilon=array$(1)
         endif
         if(arg(i)(1:4).eq.'-mag') then
             call sei get values(1,arg(i+1),code)
             minmag=array$(1)
             call sei get values(1,arg(i+2),code)
             maxmag=array$(1)
         endif
         if(arg(i)(1:6).eq.'-depth') then
             call sei get values(1,arg(i+1),code)
             mindepth=array$(1)
             call sei get values(1,arg(i+2),code)
             maxdepth=array$(1)
         endif
         if(arg(i)(1:6).eq.'-nstat') then
             call sei get values(1,arg(i+1),code)
             minstation=array$(1)
             call sei get values(1,arg(i+2),code)
             maxstation=array$(1)
         endif
         if(arg(i)(1:4).eq.'-gap') then
             call sei get values(1,arg(i+1),code)
             mingap=array$(1)
             call sei get values(1,arg(i+2),code)
             maxgap=array$(1)
         endif
         if(arg(i)(1:4).eq.'-rms') then
             call sei get values(1,arg(i+1),code)
             minrms=array$(1)
             call sei get values(1,arg(i+2),code)
             maxrms=array$(1)
         endif
         if(arg(i)(1:5).eq.'-time') then
             starttime=arg(i+1)(1:14)
             endtime  =arg(i+2)(1:14)
         endif
      enddo
c
      return
      end
