c
c   converts pde monthly and weekly bulletins files to nordic format
c   files can be received on email or picked up  at 
c   hazards.cr.usgs.gov, dir pde for monthly files and
c   hazards.cr.usgs.gov, weekly  for weekly files, must be ehdf type.
c
c   reads monthly filefes in both formats before 2004 and after
c
c   jh august 1993
c   updates:
c   apr 14, 96 by jh: add automatic year,month reading, weekly files
c   apr 19          : fix name, append weekly files
c   apr 25          : fix monthly if retransmitted mail
c   may 30, 97      : format read for weekly files was wrong in latitude
c   jun 23          : new PDE format
c   sep 9           : bug
c   dec 19          : bug with s and w for weekly files
c                     be able to read yearly files in monthly format
c   fen 4   99 jh   : ---------   version 7.0 ---------------------
c   jan 27 2002 jh  : make all events type D, put weekly file sin PDEWE base
c   feb 19          : also split to s-files
c   mar 12          : more fixing in above
c   apr 4           _ --------------------
c   aug 20          : put in line type 1
c   mar 24 2003     : for monthly files, number of stations was GAP, fixed
c   may 10 2004     : more comments
c   jul 7  2005     : small format change for monthly files
c   dec 7  2005 jh  :  more changes to monthly files, some #$@%^% changed
c                     the format jan 01, 2004, 
c   oct 28 2008 jh  : file_open initial value of false was commented out,
c                     so it did not work on linux, rms gives wrong values
c                     on weekly files and linux, do not write out of larger
c                     then 5.0
c   nov 28          : initial file_open was again commented out ?
c   mar 1 2010  jh  : change magnitudes B to b and S to s


c
       IMPLICIT NONE
c  ARRAY WITH ORIGINAL READINGS, only header here
      CHARACTER*80 DATA
      character*80 data1(10)   ! more lines
c-- TOP DIR FOR REA
      CHARACTER*40 TOP_DIRECTORY
c   length of top_directory
      integer topdirlen
      character*80 text,new_text
      character*5 base         ! data base to write to
c-- dir separator
      character*1 dchar
      CHARACTER*3 AGENCY
      integer id                ! id line number
      character*4 trms
      character*80 outfile
      logical file_open          ! true if outfile is open
      character*3 tmonth(12)     ! first 3 letters of month
      character*80 infile
      character*4  type          ! file type
      character*4  magaga1,magaga2 ! magnitude type and agency
      character*4  magaga3,magaga4 ! magnitude type and agency
      real mag1,mag2,mag3,mag4     ! magnitude
c---time of writing s-file
      character*12 p_time
      character*14 proc_time
c---operator id
      character*4 operator
C  NUMBER OF DIFFERENT STATIONS IN DATA
      INTEGER NSTAT
      INTEGER YEAR,MONTH,DAY,HOUR,MIN
      integer old_month            ! previous month
      REAL SEC
C  DECIMAL LATITUDE AND LONGITUDE
      REAL DLAT,DLON
C  DEPTH,  RMS OF RESIDUALS
      REAL DEPTH,RMS
      INTEGER IDEPTH
      integer nhead, nrecord
c magntudes
      character*3 ms,mb
      INTEGER I,nevent,l,k
      data tmonth/'JAN','FEB','MAR','APR','MAY','JUN',
     *            'JUL','AUG','SEP','OCT','NOV','DEC'/



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

C
c
c   get system time so time of update can be time stamped
c
      call systime(p_time,proc_time)


      nevent=0
      file_open=.false.
      old_month=999
      call topdir(top_directory)
      topdirlen=index(top_directory,' ') -1
      call dir_char(dchar)
      write(6,*)' Input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old')

 6666    continue 
         WRITE(6,*)'OPERATOR ID (max 4 char):'
         READ(5,'(A4)') OPERATOR
c
         IF(OPERATOR.EQ.'    ') then
           write(6,*) ' You must give operator'
           goto 6666
         endif
c
c   find which type of file, monthly or weekly, have different format
c
      type='    '
      do i=1,80
        read(1,'(a)') text
c      enddo
      if(text(1:2).eq.'GS') then
         type='week'
         write(6,*)' Weekly file'
         write(6,*)' Enter to continue'
         read(5,'(a)') k
         goto 3333
      endif
      if(text(3:4).eq.'  '.and.
     *(text(24:24).eq.'N'.or.text(24:24).eq.'S')) then
         type='mont'
         write(6,*) ' Monthly file'
         write(6,*)' Enter to continue'
         read(5,'(a)') k
         goto 3333
      endif
      enddo
 3333 continue
c
      if(type.eq.'mont') then
c
c   find year and month
c
         rewind 1
c
c   find first header line
c
  20     continue
         read(1,'(a)') text
         if(text(1:3).ne.'DAY') goto 20
c
c   find line with year and month
c
 30      continue
         backspace 1
         backspace 1
         backspace 1
         backspace 1
         backspace 1
         read(1,'(a)') text
         if(text.eq.' ') goto 30
c
c  get rid of blanks
c
         do i=1,80
          if(text(i:i).ne.' ') goto 40
         enddo
40       continue
         k=0
         new_text=' '
         do l=i,80,2
           k=k+1
           new_text(k:k)=text(l:l)
         enddo
c        write(6,*) new_text
c
c  find year and month
c
         do i=1,80
           if(new_text(i:i).eq.' ') then
              read(new_text(i+1:i+4),'(i4)') year
              goto 45
           endif
          enddo
          write(6,*) 'Year and month ',year,month
 45       continue
c
c   find month
c
         do i=1,12
           if(tmonth(i).eq.new_text(1:3)) then
             month=i
             goto 2233
           endif
         enddo
         write(6,*)' Could not find a valid month'
         stop
 2233    continue
         write(6,*) 'Year and month ',year,month
      endif
      if(type.eq.'    ') then
         write(6,*)' Not a PDE type input file'
         stop
      endif
      rewind 1
c
c   reading loop
c
 50   continue 
      read(1,'(a)',end=99) text
      if(text(1:20).eq.'                    ') goto 50
      if(text(24:24).ne.'N'.and.text(24:24).ne.'S'.and.
     *text(26:26).ne.'N'.and.text(26:26).ne.'S') goto 50
      if(text(35:35).ne.'W'.and.text(35:35).ne.'E'.and.
     *text(33:33).ne.'W'.and.text(33:33).ne.'E') goto 50
      mag1=-10.0
      mag2=-10.0
      mag3=-10.0
      mag4=-10.0
      magaga1='    '
      magaga2='    '
      magaga3='    '
      magaga4='    '
      nevent=nevent+1
      if(type.eq.'mont') then
c        write(6,*) text(1:70)
         read(text(1:14),'(i2,2x,i2,1x,i2,1x,f4.1)') day,hour,min,sec
         read(text(17:22),'(f6.3)')dlat 
         read(text(27:33),'(f7.3)')dlon
         if(year.lt.2004) read(text(38:40),'(i3)') idepth
         if(year.ge.2004) read(text(37:39),'(i3)') idepth
         depth=idepth
         if(year.lt.2004) read(text(45:51),'(a3,1x,a3)')mb,ms
         if(year.ge.2004) read(text(44:50),'(a3,1x,a3)')mb,ms  ! changed dec7, 2005 , jh
         if(mb.ne.'   ') then
            read(mb,'(f3.1)') mag1
            magaga1='bPDE'
         endif
         if(ms.ne.'   ') then
            read(ms,'(f3.1)') mag2
            magaga2='sPDE'
         endif
         if(year.lt.2004) read(text(53:56),'(f4.1)') rms
         if(year.ge.2004) read(text(54:57),'(f4.1)') rms    ! change july 2005 jh
         if(year.lt.2004) read(text(62:66),'(i5)') nstat    
         if(year.ge.2004) read(text(63:67),'(i5)') nstat    ! change dec 7, 2005 jh
         if(text(24:24).eq.'S') dlat=-dlat
         if(text(35:35).eq.'W') dlon=-dlon
      else
         read(text(5:25),'(i4,4i2,f4.2,f5.3)')
     *   year,month,day,hour,min,sec,dlat
c
c   check if a new month
c
         if(month.ne.old_month) then
            file_open=.false.
            old_month=month
         endif
         read(text(27:32),'(f6.3)') dlon
         mb(1:2)=text(48:49)
         ms(1:2)=text(52:53)
         if(mb(1:2).ne.' ') then
            read(mb(1:2),'(f2.1)') mag1
            magaga1='bPDE'
         endif
         if(ms(1:2).ne.' ') then
            read(ms(1:2),'(f2.1)') mag2
            magaga2='sPDE'
         endif
         read(text(34:37),'(f4.1)') depth
         if(text(26:26).eq.'S') dlat=-dlat
         if(text(33:33).eq.'W') dlon=-dlon
         if(text(57:59).ne.'   ') then
            read(text(57:59),'(f3.2)') mag3
            magaga3=text(61:64)
            if(magaga3(1:1).eq.'D') magaga3(1:1)='C'
         endif
         if(text(67:69).ne.'   ') then
            read(text(67:69),'(f3.2)') mag4
            magaga4=text(71:74)
            if(magaga4(1:1).eq.'D') magaga4(1:1)='C'
         endif
         read(text(41:43),'(i3)') nstat
      endif
C
         do i=1,80
           data(i:i)=' '
         enddo
         agency='PDE'
         WRITE(DATA(2:20),'(I4,1X,2I2,1X,2I2,1X,F4.1)')
     *   YEAR,MONTH,DAY,HOUR,MIN,SEC
         trms='    '
         if(rms.gt.0.0.and.rms.lt.5.0)write(trms,'(f4.1)') rms ! jh oct 08
         if(nstat.gt.999) nstat=999
         WRITE(DATA(46:55),'(A3,I3,a4)') AGENCY,NSTAT,TRMS
         WRITE(DATA(24:43),'(F7.3,F8.3,F5.1)') DLAT,DLON,DEPTH
c
c   reshuffel magnitudes
c
         if(mag3.gt.-9.9) then
            if(mag1.eq.-10.0) then
               mag1=mag3
               magaga1=magaga3
               mag3=-10.0
            elseif(mag2.eq.-10.0) then
               mag2=mag3
               magaga2=magaga3
               mag3=-10.0
            endif
         endif
         if(mag4.gt.-10.0) then
            if(mag1.eq.-10.0) then
               mag1=mag4
               magaga1=magaga4
               mag4=-10.0
            elseif(mag2.eq.-10.0) then
               mag2=mag4
               magaga2=magaga4
               mag4=-10.0
            elseif(mag3.eq.-10.0) then
               mag3=mag4
               magaga3=magaga4
               mag4=-10.0
            endif
         endif
      if(mag1.gt.-9.0) write(data(57:63),'(f3.1,a4)')mag1,magaga1 
      if(mag2.gt.-9.0) write(data(65:71),'(f3.1,a4)')mag2,magaga2 
      if(mag3.gt.-9.0) write(data(73:79),'(f3.1,a4)')mag3,magaga3 
c
c   open file if not already open
c
      if(.not.file_open) then
         write(outfile,'(i4,i2)') year,month
         do i=1,6
           if(outfile(i:i).eq.' ') outfile(i:i)='0'
         enddo
         outfile(7:10)='.CAT'
c
c   set path to where PDE is, if a weekly file, append to a possible older file
c

         write(6,*)'Open file for year and month', year,month
         if(type.eq.'week') then
          open(2,file=top_directory
     *    (1:topdirlen)//dchar//'REA'//dchar//'PDEWE'//dchar//'CAT'
     *    //dchar//outfile,
     *    status='unknown',access='append')
          base='PDEWE'
         else
          open(2,file=top_directory
     *    (1:topdirlen)//dchar//'REA'//dchar//'PDE__'//dchar//'CAT'
     *    //dchar//outfile,
     *    status='unknown')
          base='PDE__'
         endif
         file_open=.true.
      endif
c
c   set event type to distant
c
      data(22:22)='D'
      write(2,'(a)') data
c
c   put in line type
c
      data(80:80)='1'
c
c   put in s-file
c
       data1(1)=data
       data1(2)=' '
       nhead=1
       nrecord=2
       id=0         ! no id line
       call write_sfile
     *(data1,base,proc_time,operator,nhead,nrecord,id)

      goto 50
99    continue
      write(6,*)
      write(6,*)' Number of events converted', nevent
      write(6,'(1x,a,a)') 'Output file name ',outfile
      stop
      end 



      subroutine write_sfile
     *(data,base,proc_time,operator,nhead,nrecord,id)
c
c  routine runs the split function on one event in seisan data base base.
c  data in data
c  only one event is written out. 
c  proc_time and operator as in program update.
c  special for this version is that the file name and id is always taken
c  from the origin time since it is used in connection with update.
c
c
       implicit none

C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'seidim.inc'
       include 'libsei.inc'                ! Open file definitions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c-- OUTPUT FILE NAME                                   
      CHARACTER*80 OUTFL
      integer nf            ! length of file name
c-- EVENT TYPE, L,R OR D                              
      CHARACTER*1  EVTYPE			
c-- idline
      character*80 idline
c-- TOP DIR FOR REA               
      CHARACTER*40 TOP_DIRECTORY                
c-- dir separator
      character*1 dchar
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
      character*14 proc_time 
C-- OPERATOR ID
      CHARACTER*4 OPERATOR
c-- HELP FORMAT LINE 0: NO, 1: YES               
      INTEGER FORMAT_INDICATOR			
c-- RECORDS IN FILE                                
      CHARACTER*80 DATA(*)			
c-- DATES AND TIMES                             
      INTEGER century,YEAR,MONTH		        
     *,DAY,HR,MIN,ISEC                                                          
c-- SECONDS                                                    
      REAL SEC					
      character*1 answer
c-- INDICATOR FOR USE OF DATA BASE                      
      CHARACTER*5 BASE				
c-- NUMBER OF RECORDS FOR EVENT                          
      INTEGER NRECORD				
c-- NUMBER OF HEADERS FOR EVENT                            
      INTEGER NHEAD				
C-- ID LINE NUMBER
      INTEGER ID
c-- TIME IN SEC SINCE 1900 EVENT                    
      DOUBLE PRECISION TIME			
c-- MINIMUM TIME --------------   
      DOUBLE PRECISION MIN_TIME                 
c-- DAY OF YEAR, NOT USED                                    
      INTEGER DOY				
c-- COUNTER                                                  
      INTEGER I,K	
c   length of top_directory
      integer topdirlen
c  agency
      character*3 agency
      character*80 sfil    ! name of sfile returned
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c write unit #1
       integer          write01
c file name
       character*80     chr_file
C                                                                               
C    SET DEFAULTS AND INITIAL PARAMETERS                                        
C
      call topdir(top_directory)
      call get_def_base(agency)
      topdirlen=index(top_directory,' ') -1
      call dir_char(dchar)
      b_f_debug$=.false.                  ! file debugging

C                                                                               
C   CHECK IF FORMAT EXPLANATORY LINE IN FILE                        
C                                                                               
      FORMAT_INDICATOR=0                                                        
      DO K=1,NHEAD                                                              
         IF(DATA(K)(80:80).EQ.'7') FORMAT_INDICATOR=1                           
      ENDDO                                                                     
C                                                                               
C   GET ABS TIME FOR  EVENT, READ YEAR,MONTH AND DAY FROM HEADER,               
C   HR, MIN AND SEC FROM EARLIEST STATION IF NOT GIVEN IN HEADER
C                                                                               
      READ(DATA(1),'(1X,2I2,1X,2I2,1X,2I2,1X,F4.1)')                             
     *CENTURY,YEAR,MONTH,DAY,HR,MIN,SEC
C
C   ASSUME 1900 IF NO CENTURY GIVEN
C
      IF(CENTURY.EQ.0) CENTURY=19
      year=century*100+year                                                               
      evtype=data(1)(22:22)
C                                                                               
C   CHECK IF HR AND MIN IN HEADER                                               
C                                                                               
c-- FIND EARLIEST TIME if phase records available                  
      IF(DATA(1)(12:15).EQ.'    '.and.(nrecord-1).gt.nhead) THEN		
         MIN_TIME=10E20                                                         
         DO I=NHEAD+1,NRECORD-1                                                 
            READ(DATA(I),'(18X,2I2,1X,F5.1)') HR,MIN,SEC                        
c-- ABS TIME              
            CALL TIMSEC(YEAR,MONTH,DAY,HR,MIN,SEC,TIME)	
            IF(TIME.LT.MIN_TIME) MIN_TIME=TIME                                  
         ENDDO                                                                  
         CALL SECTIM(MIN_TIME,YEAR,DOY,MONTH,DAY,HR,MIN,SEC)                    
c--  EARLIEST TIME  
         WRITE(DATA(1)(1:20),'(1X,I4,1X,2I2,1X,2I2,1X,F4.1)') 
     *   YEAR,MONTH,DAY,HR,MIN,SEC                   
         DATA(1)(80:80)='1'
      ENDIF                                                                     
c
C                                                                               
C  GENERATE OUTPUT FILE NAME                                                    
C     
      ISEC=SEC      
C
      IF(BASE.NE.'     '.and.base(1:2).ne.',,') THEN
         WRITE                                                                  
     *   (OUTFL,307)BASE,YEAR,MONTH,DAY,HR,MIN,ISEC,EVTYPE,YEAR,MONTH           
 307     FORMAT(
     *   '/',A5,'/',I4,'/',I2,'/',I2,'-',2i2,'-',i2,a1,'.S',i4,I2)               
C
         DO I=1,34                                                              
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                
         ENDDO   
         nf=34                                                               
      ELSE                                                                      
         WRITE(OUTFL,303)DAY,HR,MIN,ISEC,EVTYPE,'S',YEAR,MONTH                      
 303     FORMAT(I2,'-',2i2,'-',i2,a1,'.',a1,i4,I2)                                  
         DO I=1,19                                                                 
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                   
         ENDDO
         nf=19          
      ENDIF
c
c   make id line if not there, else update current id line
c
      if(id.ne.0) idline=data(id)
      if(id.eq.0) THEN
         idline(1:40)= ' ACTION:                   OP:     STATU'
         idline(41:80)='S:               ID:                   I'
         WRITE(IDLINE(31:34),'(A)')OPERATOR
         WRITE(IDLINE(13:26),'(A)')PROC_TIME
         WRITE(IDLINE(9:11),'(A)')'SPL' 
      endif
c
c  update id line
c
      WRITE(IDLINE(61:75),'(i4,5I2)')
     *YEAR,MONTH,DAY,HR,MIN,ISEC
      DO I=61,74
         IF(IDLINE(I:I).EQ.' ') IDLINE(I:I)='0'
      ENDDO
C                                                                               
C  OPEN AND WRITE OUTPUT FILE                                                   
C
 3    continue                 ! get here from below when file did exist
      IF(BASE(1:2).NE.',,') THEN                                                    
         chr_file=' '
         chr_file = top_directory(1:topdirlen)//dchar//'REA'//
     *   outfl(1:nf)
c        write(6,*) chr_file
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    chr_file,              ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            sfil=chr_file
      ELSE
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    outfl,                 ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            sfil=outfl
      ENDIF
      write(6,'(a)') data(1)(1:78)
      
c
c   make new index
c
      if(b_old) then
          write(6,*)' Creating a new id for event'
          write(6,*)' Please confirm (y/n)'
          read(5,'(a)') answer
          if(answer.eq.'y') then
             call inc_id(idline,outfl,nf)
             call sei close (close$,write01,code) ! close
             goto 3                            ! go and try to open again
          endif
      endif
c
  10  continue 
C
C   WRITE FILE
C                                  

C                                                                     
c-- ORIGINAL HEADER LINES FIRST
C   IF NO ID LINE, PUT IN AS NUMBER 2
C
          write(write01,'(a80)',iostat=code) data(1)
          call sei code(fort$,code,write01,b_eof)
c
c  if new id line, write as #2, else where it was before
c
      if(id.eq.0) then
          write(write01,'(a80)',iostat=code) idline
          call sei code(fort$,code,write01,b_eof)
      else
         data(id)=idline
      endif
c
c   write rest of header lines, skip format help lines
c
      if(nhead.ge.2) then
	     do i=2,nhead 
            if(data(i)(80:80).ne.'7') then
               write(write01,'(a80)',iostat=code) data(i)
            endif
	     enddo
      endif
      call sei code(fort$,code,write01,b_eof)
C
c-- write help line 
C
      write(write01,243,iostat=code)
      call sei code(fort$,code,write01,b_eof)
 243  FORMAT(
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO ',
c     *'SNR AR TRES W  DIS CAZ7')              
     *'AIN AR TRES W  DIS CAZ7')              
c-- WRITE REST OF EVENT         
       write(write01,'(a80)',iostat=code)
     * (data(i),i=nhead+1,nrecord)
      call sei code(fort$,code,write01,b_eof)
      call sei close (close$,write01,code)
c
 2    continue
      return                                                                      
      END                                                                       
                                                                                
