c$debug
c------------------------------------------------
c  Program: VELMENU
c
c  MENU AND CONVERSION PROGRAM TO USE THE INVERSION PROGRAM VELEST
c  IN CONNECTION WITH SEISAN
c
c
c  by Lars Ottemoeller Sept. 96
c
c  last updates:   
c     
c    26 oct 98 lo: conversion from velest to nordic, first station code blank
c --------------- SEISAN 7.0 ----------------------
c    08 jun 99 lo: velest formats do not support 5 char station code, therefore
c                  when reading from STATION0.HYP, only the first chars are
c                  taken, if 5 char station code, this will be a problem, when
c                  after inversion going back to Nordic format
c    24 ju jh      remove reference to external routines
c    19 10 99  lo  check if phase minutes more than 60 and calc hours
c    30 08 00  lo  set vthet to 999 in JHD, also see invertratio in JHD
c    25 10 00  jh  read secs as f6.3, high accuracy
c    05 03 01  jh  distance as real
c
c
c

      implicit none
c menu choice
      character*1 choice
c filename for calling editor
      character*80 fname
c earthquakedata in SEISAN format file name
      character*80 qname
c model file name
      character*80 mname
c index
      integer ind

C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       integer  read01,                    ! Input unit 1.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
C
C    ============= end of list ==========


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver


c
   1  CONTINUE 
   5  Format(a,$)
      write(*,*)
      write(*,*)'File name of earthquake data in Nordic Format : '
      read(*,'(a)') qname 

	  call sei open( old$+warn$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   qname,            ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

       if (code.NE.0) then
	 goto 1
       else

	 call sei close( close$, read01, code ) ! Close file again
       endif

      if (qname(1:8).EQ.'data.nor')THEN
	write(*,'(a)') '!!! Rename file !!!'
	goto 9999 
      ENDIF

c
c model name hardwired
c
      mname='input.mod'
      write(*,*)
      write(*,*) 'VELEST MENU'
      write(*,*) '-----------'
  10  CONTINUE
      write(*,*)
c     ind=index(qname,' ')-1
c     write(*,'(a,a,a)') '0. Change earthquake data file name ('
c    *,qname(1:ind),')'
      write(*,*) '1. Create VELEST command file (velest.cmn)'
      write(*,*) '2. Edit/change VELEST command file (velest.cmn)'
      write(*,*) '3. Create station select file (selstat.lis)' 
      write(*,*) '4. Edit/change station select file (selstat.lis)' 
      write(*,*) '5. Create model file'
      write(*,*) '6. Edit/change model file'
      write(*,*) 'A. RUN VELEST '
      write(*,*) 'B. Edit inversion output file '
      write(*,*) 'C. Convert VELEST output to Nordic format',
     &' and make diff-file'
      write(*,*) 'Q. End '
      write(*,*)
      write(*,*) 'Choice ? '
      read(*,'(a1)') choice

c     IF (choice.eq.'0') THEN
c     write(*,*)'Give file name of earthquake data in Nordic Format : '
c       read(*,'(a)') qname
      IF (choice.eq.'1') THEN
	CALL create_cmn(qname,mname)
      ELSEIF (choice.eq.'2') THEN
	fname='velest.cmn'
	CALL EDITOR(fname,10)
      ELSEIF (choice.eq.'3') THEN
	CALL create_stationlist(qname)
      ELSEIF (choice.eq.'4') THEN
	fname='selstat.lis'
	CALL EDITOR(fname,11)
      ELSEIF (choice.eq.'5') THEN
	CALL create_model(mname)
      ELSEIF (choice.eq.'6') THEN
	ind=INDEX(mname,' ')
	fname=mname
	CALL EDITOR(fname,ind-1)
      ELSEIF (choice.eq.'A'.or.choice.eq.'a') THEN
	CALL conv_stat
	CALL seivel(qname)
	call systemc('velest',6)     
      ELSEIF (choice.eq.'B'.or.choice.eq.'b') THEN
	fname='invers.out'
	call EDITOR(fname,13)
      ELSEIF (choice.eq.'c'.or.choice.eq.'C') THEN
	CALL velsei
      ELSEIF (choice.eq.'q'.or.choice.eq.'Q') THEN
	GOTO 9999
      ENDIF
      GOTO 10
 
9999  continue
      stop
      end

c--------------------------------------------------------
      SUBROUTINE conv_stat
c
c
c THIS SUBROUTINE CONVERTS STATIONS INCLUDED IN SELECTION FILE
c FROM STATION0.HYP FORMAT TO VELEST-STATION FORMAT
c
c
c    INPUT:  STATION0.HYP FILE IN NEAR STANDARD HYPO71 FORMAT
c            STATION SELECTION selstat.lis
c    OUTPUT: STATION FORMAT USED IN THE VELEST INVERSION PROGRAM
c
      implicit none
c text strings, in- and output-files
      character*80 text,textout,out
c characters for North-South and East-West
      character*1  ns,ew
c stationname 
      character*4  stname
c coordinates
      real latmin,longmin
      integer latdeg,longdeg
      integer k,l,height
c flag used to find beginning of station list
      logical flag
c array for selection of stations
      character*4  selstat(999)
c number of stations in station select file
      integer      stat_number
c counter 
      integer c

C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       integer  read01,                    ! Input unit 1.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       character*80 chr_text               ! Text string.
       character*60 top_directory
c---directory separator character
       character*1 dchar
       integer ind                         ! index

C
C    ============= end of list ==========
c
c open files
c

      out='station.sta'
      open(2,file=out,status='unknown')
      open(3,file='selstat.lis',status='old')
      write(*,*) 'CONVERSION: STATION0.HYP -> station.sta'
      write(*,*)  

      call topdir(top_directory)
      ind=index(top_directory,' ')-1

      call dir_char(dchar)         ! dirctory delimiter character
c
      chr_text = top_directory(:ind)//
     &           dchar // 'DAT' //dchar                 //
     &           'STATION0.HYP'

      ind=index(chr_text,' ')-1


c
c read STATION0.HYP file to get velocity model
c


	  call sei open( old$+warn$,       ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'STATION0.HYP',   ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

	if (code.ne.0) then
	  write(*,*) 'STATION0.HYP from:'
	  write(*,*) chr_text(:ind)
	  call sei open( old$+warn$,       ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   chr_text(:ind),   ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

       endif

       if (code.NE.0) then
	 write(*,*) 'STATION0.HYP not found !!!'
	 stop
       endif



 5    Format(a,$)
c
c read selection of stations
c
      k=0
  2   read(3,'(a)',end=4) text
c     read(text(1:1),'(a1)') c1
      IF (text(1:1).EQ.'#') THEN
	go to 3
      ENDIF     
      k=k+1
      read (text(1:4),'(a4)') selstat(k)
  3   continue
      go to 2
  4   continue
      stat_number=k
c
c  write format line 
c
      write(2,'(a57)') 
     &'(a4,f7.4,a1,1x,f8.4,a1,1x,i4,1x,i1,1x,i3,1x,f5.2,2x,f5.2)' 
      flag=.false.
      k=0
 10   continue
      read(read01,'(a)',end=20) text

c
c  find line with station after blank line
c
      if (text(1:1).ne.'R') THEN
	if (.NOT.flag) then
	      flag=.true.
c
c  read line after blank line, first station line
c
	  read(read01,'(a)') text
c	elseif (index(text,'  ').ne.1) then
        elseif (text.eq.''.or.text.eq.' ') then
	  go to 20
	  endif 
      endif
c
c  convert station data
c  read and write
c
      if (flag) then 

        if (text(2:2).eq.' ') then 
          read(text(3:6),'(a4)') stname 
        else
          read(text(2:5),'(a4)') stname
        endif
c
c compare if stname in selstat
c 
	l=1
 11     if (stname.EQ.selstat(l)) THEN
	  go to 12
	endif
	if (l.EQ.stat_number) THEN
	  go to 10
	endif
	l=l+1
	go to 11 
 12     continue
	do c=1,80
	  textout(c:c)=' '
	enddo
	textout(1:4)=stname
	read(text(7:8),'(i2)') latdeg
	write(textout(5:6),'(i2)') latdeg
c	read(text(9:13),'(f5.2)') latmin
	read(text(9:13),'(f5.3)') latmin
	latmin=latmin/60
	write(textout(7:11),'(f5.4)') latmin
	read(text(14:14),'(a1)') ns
	textout(12:12)=ns
	textout(13:13)= ' '
 
	read(text(15:17),'(i3)') longdeg
	write(textout(14:16),'(i3)') longdeg
c	read(text(18:22),'(f5.2)') longmin
	read(text(18:22),'(f5.3)') longmin
	longmin=longmin/60
	write(textout(17:21),'(f5.4)') longmin
	read(text(23:23),'(a1)') ew
	textout(22:22)= ew

	read(text(24:27),'(i4)') height
	textout(23:23)= ' '
	write(textout(24:27),'(i4)') height
	textout(28:30)= ' 1 '
 
        k=k+1 
        if (k.gt.999) k=0
	write(textout(31:33),'(i3)') k
	textout(34:50)='  0.00   0.00   1'

	write(2,'(a80)') textout
      endif
c     read(text(12:16),'(f2.0,f2.0)') hh,mm
c     if (hh*100+mm.GE.mintime) then
c       if (hh*100+mm.LE.maxtime) then
c         write(2,'(a)') text
c       end if
c     end if
      go to 10
 20   continue
c  write line
      write(2,*)
      call sei close( close$, read01, code ) ! Close file again
      close(2)
      close(3)

      RETURN
      END


c-------------------------------------------------------
       SUBROUTINE SEIVEL(qname) 
c
c      THIS SUBROUTINE CONVERTS THE EARTHQUAKE DATA FILE IN  
c      SEISAN FORMAT TO CNV FORMAT USED IN THE VELEST INVERSION
c      PROGRAM.
c
c
c

      implicit none
c-- event data
      character*80      data(2000)      
c-- for storing the station names from the station file 
      character*80     sdata(1000)
c-- helping variable to store the output lines 
      character*80     line,cnvline
c phasename
      character*4 pha(500)
c-- exp indicator                                         
      character*1       exp     ,wg(500)  !wg is the weight     
      character*80     outfile   ! output file name
c time residual, too high res are not used
      real tres(500)
c max residual
      real maxres
c earthquakedata in SEISAN format file name 
      character*80 qname
      character*80     stafile   !station file name
c-- event type                                      
      character*1      type, ns,ew !ns is north or south,
				   !ew is east or west,
c-- number of records for event                           
      integer           nrecord         
c-- number of header lines                                 
      integer           nhead           
c-- number of stations                                     
      integer           nstat           
c-- number of phases
      integer nphase
c-- counter                                                   
      integer x,p,g,s,i,t !some helping variables for counting          
c -- index
      integer ind
c-- id line number
      integer id ,hour1(500),min1(500) !hour and min at time of reading 
      integer year,month,day,hour,min  ! year, month,day,hour and minute 
      real sec,lat,long,dep,sec1(500),rsec(500)  !seconds,latitude,longtude,depth 
					!sec1 time of reading 
					!rsec relative time 
      integer hh
      character*4 sta(500),stat(1000)      !to store the stations
					   !once from the station file
					   !and once from the input file 
c helping variables for control
      logical flag 
c phase output boolean
      logical phaout(500)
c number of stations in select file
      integer stlistn
c logicals station in list, correct phase type and weight OK,output
c if all are true
      logical phstat(500),phph(500),phwg(500)
c number of phases in cnvline
      integer ncnv
c computer type
      logical sun,pc,linux
c choice
      character*1 choice
c
c    open input file
c
      call computer_type(sun,pc,linux)
      maxres=5.0

   5  format(a,$)
c
      open(1,file=qname,status='old')

c 
c   open output file CNV format 
c 

      outfile='data.cnv' 
      ind=index(qname,' ') 
      write(*,*) 'CONVERSION: ',qname(1:ind),' -> ',
     *'data.cnv'
      write(*,*)
      open(2,file=outfile,status='unknown')

c
c  open output file NORDIC format
c

      open(4,file='data.nor',status='unknown')

c
c  open script control file for hyp
c
      open(8,file='ttt.ttt',status='unknown')
      write(8,'(a8)') 'data.nor'
      write(8,'(a1)') 'n'
      close(8)

c
c    open station file
c
      stafile='selstat.lis'
      open(3,file=stafile,status='old')
c
c read station file, array of stations is stat(...)
c
      p=1
   50 read(3,'(a80)',end=60) sdata(p)
      p=p+1
      GOTO 50
   60 CONTINUE
      g=1 
      DO 70 s=1,p
      IF (sdata(s)(1:1).NE.'#') THEN !to skip the comment lines 
	read(sdata(s),'(a4)') stat(g) 
	g=g+1
      ENDIF
      stlistn=g-1
   70 CONTINUE
c                                                                               
c  read one event
c



   80 continue 
      call indata(1,nstat,nphase,nhead,nrecord,type,exp,data,id)
c
c jump out if end of file
c

c
c overwrite fixed depth and location
c

c      write(data(1)(44:45),'(a2)') '  '

      IF (nrecord.EQ.0) THEN 
	 GOTO 500
      ENDIF

c
c overwrite event ID explosion (P,E)
c

      if (data(1)(23:23).EQ.'P'.OR.data(1)(23:23).EQ.'E')THEN
	data(1)(23:23)=' '
      ENDIF

      do 85 p=1,nhead
	write(4,'(a80)') data(p)

   85 CONTINUE

c
c   read year,month,day,hour,minute,seconds,latitude,longitude
c   and depth from header
c
      read(data(1),'(3x,i2,1x,i2,i2,1x,i2,i2,1x,f4.0,3x,
     +f7.0,f8.0,f5.0)')
     +  year,month,day,hour,min,sec,lat,long,dep
	flag=.FALSE. 
      IF (lat.GE.0) THEN !to make direction north or south
	  ns='N'
      ELSE
	ns='S'
	lat=-lat 
      ENDIF
      IF (long.GE.0) THEN !to make direction east or west 
	ew='E'
      ELSE
	ew='W'
	long=-long
      ENDIF
c
c write the header line in output file
c
      write(2,'(3i2,1x,2i2,1x,f5.2,1x,f7.4,a1,1x,f8.4,a1,
     + f7.2)') year,month,day,hour,min,sec,lat,ns,long,ew,dep

c
c read the phases (station, phase, weight and time)
c
  100 CONTINUE
      do 110 i=nhead+1,nrecord-1,1 
	 x=i-nhead
	 read(data(i),'(1x,a4,5x,a4,a1,3x,i2,i2,f6.3)')
     *   sta(x),pha(x),wg(x),hour1(x),min1(x),sec1(x) 

         if (min1(x).gt.59) then
           hh = int(min1(x)/60)
           if (hh.gt.0) then 
             hour1(x) = hour1(x) + hh
             min1(x) = min1(x) - hh *60
           endif
         endif

	 read(data(i)(64:68),'(f5.1)') tres(x)  ! read residual
c if tres larger than maxres set weight to 4
	 if (abs(tres(x)).ge.maxres) then
	   wg(x)='4'
	 endif
c 
c set weight to 0 if blank
c
	 IF (wg(x).EQ.' ') THEN
	     wg(x)='0'
	 ENDIF
c
c check if weight between 0 and 3, no output for w=4 from
c velest
c
	 phwg(x)=.FALSE.

	 IF (wg(x).EQ.'0'.OR.wg(x).EQ.'1'.OR.wg(x).EQ.'2'
     *   .OR.wg(x).EQ.'3') THEN
	    phwg(x)=.TRUE.
	 ENDIF 
 
c
c calc relative time 
c
	 if (hour1(x).EQ.0) THEN
	   hour1(x)=24
	 ENDIF
	 if (hour.EQ.0) THEN 
	   hour=24
	 ENDIF
 
	 rsec(x)=(3600*hour1(x)+60*min1(x)+sec1(x))
     *          -(3600*hour+60*min+sec)
     
c
c set phaout to false
c
	 phaout(x)=.FALSE.


     
  110 CONTINUE

c
c select phases for output and set logical phaout(x)
c
      ncnv=0
      do i=1,80
	cnvline(i:i)=' '
      enddo
      do 150 i=1,x,1
c
c check if station in list
c
 
       phstat(i)=.FALSE.
       do 115 p=1,stlistn,1
	 if (sta(i).EQ.stat(p)) then
	   phstat(i)=.TRUE.
	   GOTO 115
	 ENDIF

c
c check if weight of 9 at this station
c
	   DO 111 t=1,x,1
	      IF (sta(i).EQ.sta(p)) THEN
		IF (wg(p).EQ.'9') THEN
		  phwg(i)=.FALSE.
		ENDIF
	      ENDIF

  111 CONTINUE

  115 CONTINUE    
      
c
c check if correct phase
c

      phph(i)=.FALSE.
      if ((pha(i)(1:1).EQ.'P').OR.(pha(i)(1:1).EQ.'S')) 
     *THEN
	 phph(i)=.TRUE.
      ENDIF


c
c select first arrival of phases with phph and phstat TRUE
c

      
      if (phph(i).AND.phwg(i) 
     *.AND.phstat(i).AND.rsec(i).LT.100.AND.
     * rsec(i).gT.0) THEN
	phaout(i)=.TRUE.
	do 130 p=1,x
	   if ((sta(i).EQ.sta(p))
     *.AND.(i.NE.p).AND.(pha(i)(1:1).EQ.pha(p)(1:1)))THEN
	     if (rsec(i).LT.rsec(p)) THEN
	       phaout(p)=.FALSE.
	     ELSE
	       phaout(i)=.FALSE.
	     ENDIF
	   ENDIF  

  130 CONTINUE
      ENDIF


c
c output
c
      if (phaout(i)) THEN

c
c write to NORDIC file
c
      line=data(nhead+i)
      write(line(10:14),'(a5)') '     '
      write(line(11:11),'(a1)') pha(i)(1:1)
      write(4,'(a80)') line 

c
c write to CNV file
c

      ncnv=ncnv+1
      if (ncnv.LE.6) then
	write(cnvline((ncnv-1)*12+1:(ncnv-1)*12+12)
     *,'(a4,a1,a1,1x,f5.2)') sta(i),pha(i),
     *wg(i),rsec(i)
      ELSE
	write(2,'(a80)') cnvline
	cnvline=' '
	ncnv=1
	write(cnvline((ncnv-1)*12+1:(ncnv-1)*12+12)
     *    ,'(a4,a1,a1,1x,f5.2)') sta(i),pha(i),
     *    wg(i),rsec(i)
 
      ENDIF

      
      ENDIF


  150 CONTINUE 

      write(4,'(a)')
      write(2,'(a80)') cnvline
      write(2,*)
      GO TO 80

 500  CONTINUE                                                                                
      close(1)
      close(2)
      close(3) 
      close(4)


c
c run hyp on Inversion Input in Nordic format
c

      write(*,'(a)') ' Do you want to relocate velest input with hyp '//
     &          '(y/n)?'
      write(*,'(a)') ' This is needed if you wish to compare the ' //
     &          'computation of velest and hyp. '
      read(*,*) choice
      if (choice.eq.'y'.or.choice.eq.'Y') then
        call systemc('hyp < ttt.ttt',13)


        if (sun.or.linux) then
           call systemc('rm ttt.ttt',11)
        elseif (pc) then
           call systemc('del ttt.ttt',11)
        endif
      endif

      RETURN
      END 
     





c
c---------------------------------------------------
c
      SUBROUTINE velsei
c
c     THIS SUBROUTINE CONVERTS BACK FROM CNV FORMAT GIVEN AS OUTPUT FROM
c     THE VELEST PROGRAM TO SEISAN FORMAT AND WRITES AN OUTPUT FILE
c     SHOWING THE DIFFERENCES BETWEEN INVERSION INPUT AND OUTPUT.
C
c
      implicit none
c-- event data
       character*80     line,data(1000)
c-- exp indicator
       character*1      exp  
c-- event type
       character*1      type
c-- number of recors for event
       integer          nrecord
c-- number of header lines
       integer          nhead
c-- number of stations
       integer          nstat
c-- number of phases
       integer nphase
c-- id line number
       integer id 
c time after inversion
       integer year,month,day,hour,min          
c time before inversion NORDIC
       integer year1,month1,day1,hour1,min1     
c time before inversion CNV
       real sec,lat,long,dep                    !seconds,latitude,longtude,depth
       real sec1,lat1,long1,dep1                !seconds,latitude,longtude,depth
       real dsec,dlat,dlong,ddep                !seconds,latitude,longtude,depth
c characters for NORTH-SOUTH and EAST-WEST
       character*1 nsc,ewc
c counter
       integer i 
c phase reading from cnv file
       character*12 phase
c output line
       character*80 lineout
c phase reading times
       real psec
c time
       real time,timeout
     
c
c      open input file
c
   1   format(a,$)
c
c open inversion input in NORDIC format
c

       open(1,file='hyp.out',status='old')
c
c open INVERSION output-file
c

       open(3,file='fin_hyp.cnv',status='old')
c
c   open file to convert from CNV to Compact format 
c
       open(4,file='velout.nor',status='unknown')
C
c   open difference output file
c
       open(2,file='velout.dif',status='unknown')
       write(2,'(a)') 'differences: inversion output - SEISAN input'
       write(2,*) 'first line input, second line output,',
     &' third line difference'
       write(2,*)

c
c open cnv input file
c
c      open(7,file='data.cnv',status='old')

c
c  read one event
c



   10  continue
       call indata(1,nstat,nphase,nhead,nrecord,type,exp,data,id)

c
c jump out if end of file
c
       IF (nrecord.EQ.0) THEN
	 GOTO 500
       ENDIF

c
c   read time and epicentre location from inversion output
c
	read(3,'(i2,i2,i2,1x,i2,i2,1x,f5.2,1x,
     +f7.4,a1,1x,f8.4,a1,f7.2)')
     +  year,month,day,hour,min,sec,lat,nsc,long,ewc,dep
c
c get time in sec
c
      time=hour*3600+min*60+sec
       read(data(1),'(3x,i2,1x,i2,i2,1x,i2,i2,1x,f4.0,3x,
     +f7.3,f8.3,f5.1)',end=500)
     +  year1,month1,day1,hour1,min1,sec1,lat1,long1,dep1 

c       read(7,'(i2,i2,i2,1x,i2,i2,1x,f5.2,1x,
c    +f7.4,a1,1x,f8.4,a1,f7.2)',end=500)
c    +  yearc,monthc,dayc,hourc,minc,secc,
c    *  latc,nscc,longc,ewcc,depc
       
c     timec=hourc*3600+minc*60+secc

c
c get differences between inversion in- and output
c
c  SOUTH = minus
c  WEST  = minus
c
      if (nsc.EQ.'S') THEN
	 lat=-lat
      endif
      if (ewc.EQ.'W') THEN
	 long=-long
      endif
       dsec = (sec-sec1)
       dlat = (lat-lat1)
       dlong= (long-long1)
       ddep = (dep-dep1)
c
c write output
c             
       write(2,'(a)') (data(1))
c
c write inversion output in headerline
c
       write(data(1)(4:20),'(i2.2,1x,i2.2,i2.2,1x,i2.2,i2.2,1x,f4.1)') 
     + year,month,day,hour,min,sec 
       write(data(1)(24:43),'(f7.3,f8.3,f5.1)')
     + lat,long,dep
c
c clear magnitudes
c
c       write(data(1)(57:79),'(a23)') '                       '
c
c clear rms
c
       data(1)(53:55)= '   '

       write(2,'(a)') (data(1))


c
c write headerlines to file
c
       do 15 i=1,nhead 
	 write(4,'(a)') (data(i))

   15  CONTINUE

       write(2,'(a,f6.1,a,f7.3,a,f8.3,a,f6.1)')
     &  'time: ',dsec,' latitude: '
     & ,dlat,' longitude: ',dlong,' depth: ',ddep

   20 CONTINUE 
      read(3,'(a)',end=500) line 
c     read(7,'(a)',end=500) linec
      IF (line(1:80).EQ.' ') THEN 
	write(2,*)  
	write(4,'(a)')
	GO TO 10
      ELSE
c
c read and write phases
c
      do 30 i=1,6,1
	read(line((i-1)*12+1:((i-1)*12+12)),'(a12)') phase
c take out residuals,since they are 0
c       read(linec((i-1)*12+1:((i-1)*12+12)),'(a12)') phasec
	if (phase(1:4).NE.'    ') THEN
	   lineout=' '
	   write(lineout(1:1),'(a1)') ' '
	   write(lineout(2:5),'(a4)') phase(1:4)
	   write(lineout(11:11),'(a1)') phase(5:5)
	   write(lineout(15:15),'(a1)') phase(6:6)
	   read(phase(8:12),'(f5.2)') psec
c          read(phasec(8:12),'(f5.2)') psecc
c          res=timec+psecc-psec-time
c          write(lineout(64:68),'(f5.2)') res
	   timeout=time+psec
	   write(lineout(19:20),'(i2)') int(timeout/3600)
	   write(lineout(21:22),'(i2)')
     *       int((timeout-int(timeout/3600)*3600)/60)
	   write(lineout(24:28),'(f5.2)')
     *       timeout-int(timeout/3600)*3600
     *       -int((timeout-int(timeout/3600)*3600)/60)*60
	   write(4,'(a)') lineout 
	endif

   30  CONTINUE
   
	 GO TO 20 
       END IF

c
c end of subroutine
c

 500  CONTINUE

      write(*,*) 'Output files:   velout.nor: final hypocentre'
     +  ,' locations in Nordic format'
      write(*,*) '                velout.dif: differences '
     +  ,'(inversion output minus input)'

      close(1)
      close(2)
      close(3)
      close(4)
c     close(7)
      RETURN
      END


c----------------------
      SUBROUTINE EDITOR(fname,enum)
c
c     THIS SUBROUTINE CALLS THE VI-EDITOR.
C
C
      implicit none
      integer enum
      character*80 fname,text,vel_edit
      integer x,ind
c
c call vi + filename 
c
      do x=1,80
	text(x:x)=' '
      enddo
      call get_editor(vel_edit)
      ind=index(vel_edit,' ')
c     write(text,'(a,a)') 'vi ',fname(1:enum)  
      text=vel_edit(1:ind) // fname(1:enum)
      call systemc(text,ind+enum)

      RETURN
      END

c----------------------
      SUBROUTINE CREATE_CMN(qname,mname)
c
c     THIS SUBROUTINE WRITES THE VELEST COMMAND (velest.cmn) FILE.
c     THE EARTHQUAKE DATA FILE IS READ TO GET THE NUMBER OF EVENTS AND
c     TO CALCULATE THE LOCATION CENTRE. PARAMETERS ARE SET FOR INVERSION OR
c     JHD.
c
c

c velest.cmn text
      character*80 text(107)
c earthquakedata in SEISAN format file name
      character*80 qname
c model file name
      character*80 mname
c choice inversion-JHD
      character*1 choice
c inversion or JHD 
      logical jhd
c maximaum distance between epicentre and station
      integer dmax
c velocity model damping
      real vthet
c counter
      integer i
c number of earthquakes
      integer neqs
c centre of earthquake locations
      real olon,olat
c sums of positive lat and lon
      real slon,slat 
c number of lat,lon
      integer nlat,nlon
c lat and lon
      real lat,lon
c-- event type
       character*1      type
c-- number of recors for event
       integer          nrecord
c-- number of header lines
       integer          nhead
c-- number of stations
       integer          nstat
c-- number of phases
       integer nphase
c-- id
       integer id
c-- exp indicator
       character*1      exp
c-- event data
       character*80     data(1000)
c length of mname
       integer mind
c nsinv: invert for station corrections?
       integer nsinv
c
c
  101 format(a,$) 
  102 continue 
      write(*,*)
      write(*,*) 'Inversion or JHD (I / J) ? '
      read(*,'(a1)') choice 
      IF (choice.EQ.'i'.or.choice.EQ.'I') THEN
         jhd=.false.
	 goto 103
      ELSEIF (choice.EQ.'j'.or.choice.EQ.'J') THEN
	 jhd=.TRUE.
	 goto 103
      ENDIF
      write(*,*) 'wrong type! '
      goto 102
  103 CONTINUE

c
c read earthquake data file to get olat,olon,neqs
c
      open(2,file=qname,status='old')
      neqs=0
      slat=0
      slon=0
      nlat=0
      nlon=0
  200 CONTINUE 
      call indata(2,nstat,nphase,nhead,nrecord,type,exp,data,id)
      IF (nrecord.EQ.0) THEN
	GOTO 300
      ELSEIF (nrecord.NE.0) THEN
	neqs=neqs+1 
	read(data(1)(24:30),'(f7.3)') lat
	read(data(1)(31:38),'(f8.3)') lon 
	lat=lat+90
	lon=lon+180
c         write(*,*) lat,',',lon
	slat=slat+lat
	nlat=nlat+1
c         write(*,*) slat,' ',nlat
	slon=slon+lon
	nlon=nlon+1
c         write(*,*) slon,' ',nlon
c         read(*,*)
      ENDIF
      GOTO 200
  300 CONTINUE
      olat=slat/nlat-90
      olon=slon/nlon-180
c
c East is negative
c
      olon=-olon
      write(*,'(a,a15,f7.3,a1,f8.3)') 'Origin of cartesian',
     &' coordinates : ',olat,' ',olon
      write(*,'(a,i5)') 'number of events: ',neqs

c
c write VELEST - command file
c

      data(text(i),i=1,10) /
     &'******* CONTROL-FILE FOR PROGRAM  V E L E S T  *******',
     &'***',
     &'*** ( all lines starting with  *  are ignored! )',
     &'*** ( where no filename is specified,',
     &'***   leave the line BLANK. Do NOT delete!)',
     &'***',
     &'*** next line contains a title (printed on output):',
     &' ',
     &'***      starting model 1 based on',
     &'***  olat       olon   icoordsystem      zshift   itrial ztrial
     &    ised'/

      data(text(i),i=11,20) /
     &'  60.3840     -5.3340      0            0.000      0     0.00
     &     0',
     &'***',
     &'*** neqs   nshot   rotate',
     &'      96      0      0.0',
     &'***',
     &'*** isingle   iresolcalc',
     &'       0          0',
     &'***',
     &'*** dmax    itopo    zmin     veladj    zadj   lowveloclay',
     &'    500.0     0      0.00       0.20    5.00       0'/

c write latitude, longitude, number of quakes
c
      write(text(11)(3:9),'(f7.3)') olat
      write(text(11)(15:22),'(f8.3)') olon
      write(text(14)(4:8),'(i5)') neqs
      
c
c set dmax to 500 for inversion and to 9999 for jhd
c
      if (.not.jhd) THEN
	dmax=500
      ELSEIF (jhd) THEN
	dmax=9999
      ENDIF
      write(text(20)(5:9),'(i5)') dmax

      data(text(i),i=21,30) /
     &'***',
     &'*** nsp    swtfac   vpvs       nmod',
     &'     2      0.50    1.740        1',
     &'***',
     &'***   othet   xythet    zthet    vthet   stathet',
     &'      0.01    0.01      0.01      1.0    0.01',
     &'***',
     &'*** nsinv   nshcor   nshfix     iuseelev    iusestacorr',
     &'       1       0       0           1            0',
     &'***'/

c
c set vthet to 1.0 for inversion and to 999 for jhd
c
      if (.not.jhd) THEN
	vthet=1.0
      ELSEIF (jhd) THEN
	vthet=999.0
      ENDIF
      write(text(26)(34:38),'(f5.1)') vthet 

c
c set nsinv to 1 for inversion and to 0 for jhd (no inversion for station 
c corrections
c
      if (.not.jhd) THEN
	nsinv=1
      ELSEIF (jhd) THEN
	nsinv=1           ! invert for station correction, also for JHD
      ENDIF
      write(text(29)(8:8),'(i1)') nsinv 

      data(text(i),i=31,40) /
     &'*** iturbo    icnvout   istaout   ismpout',
     &'       1         1         1         0',
     &'***',
     &'*** irayout   idrvout   ialeout   idspout   irflout   irfrout   
     &iresout',
     &'       0         0         0         0         0         0    
     &     0',
     &'***',
     &'*** delmin   ittmax   invertratio',
     &'    0.010       5          1',
     &'***',
     &'*** Modelfile:'/
 
c set invertratio to 6 for JHD, no, otherwise no station correction
c      IF (jhd) THEN
c         write(text(38)(28:28),'(a1)') '6' 
c      ENDIF

      mind=INDEX(mname,' ')
      data(text(i),i=41,50) /
     &' ',
     &'***',
     &'*** Stationfile:',
     &'station.sta',
     &'***',
     &'*** Seismofile:',
     &' ',
     &'***',
     &'*** File with region names:',
     &' '/

      write(text(41),'(a)') mname

      data(text(i),i=51,60) /
     &'***',
     &'*** File with region coordinates:',
     &' ',
     &'***',
     &'*** File #1 with topo data:',
     &' ',
     &'***',
     &'*** File #2 with topo data:',
     &' ',
     &'***'/

      data(text(i),i=61,70) /
     &'*** DATA INPUT files:',
     &'***',
     &'*** File with Earthquake data:',
     &'data.cnv',
     &'***',
     &'*** File with Shot data:',
     &' ',
     &'***',
     &'*** OUTPUT files:',
     &'***'/

      data(text(i),i=71,80) /
     &'*** Main print output file:',
     &'invers.out',
     &'***',
     &'*** File with single event locations:',
     &' ',
     &'***',
     &'*** File with final hypocenters in *.cnv format:',
     &'fin_hyp.cnv',
     &'***',
     &'*** File with new station corrections:'/

      data(text(i),i=81,90) /
     &'sta_cor.out',
     &'***',
     &'*** File with summary cards (e.g. for plotting):',
     &' ',
     &'***',
     &'*** File with raypoints:',
     &' ',
     &'***',
     &'*** File with derivatives:',
     &' '/

      data(text(i),i=91,100) /
     &'***',
     &'*** File with ALEs:',
     &' ',
     &'***',
     &'*** File with Dirichlet spreads:',
     &'CALDIRSP.DIS',
     &'***',
     &'*** File with reflection points:',
     &' ',
     &'***'/

      data(text(i),i=101,107) /
     &'*** File with refraction points:',
     &' ',
     &'***',
     &'*** File with residuals:',
     &' ',
     &'***',
     &'******* END OF THE CONTROL-FILE FOR PROGRAM  V E L E S T  
     &*******'/


      open(1,file='velest.cmn',status='unknown')
      do 999 i=1,107 
	 write(1,'(a)') text(i)
 999  CONTINUE
      close(1)
      close(2)
      RETURN
      END


c -------------------------------------------------
      SUBROUTINE CREATE_STATIONLIST(qname)
c
c     THIS SUBROUTINE READS THE VELEST.CMN FILE TO GET THE DMAX PARAMETER
c     AND THEN READS THE EARTHQUAKE DATA FILE TO GET A LIST OF STATIONS 
c     WITH A DISTANCE TO THE EPICENTRE LOCATION SMALLER THAN DMAX.
c
c
      implicit none
c
      character*80 qname
c-- event type
      character*1      type
c-- number of recors for event
      integer          nrecord
c-- number of header lines
      integer          nhead
c-- number of stations
      integer          nstat
c-- number of phases
      integer nphase
c-- id
      integer id
c-- exp indicator
      character*1      exp
c-- event data
      character*80     data(2000)
c lines of velest.cmn 
      character*80 text
c counters
      integer i,k
c max distance between stations and epicentre location
      integer dmax
c stationlist
      character*4 stlist(500)
c station 
      character*4 stat
c distance
      real dist
c current maximum station number
      integer stmax
c
c
c   READ DMAX FROM VELEST.CMN FILE
c
      open(1,file='velest.cmn',status='OLD')
      i=0
 10   CONTINUE
      READ(1,'(a80)') text
      IF (text(1:1).NE.'*') THEN
	  i=i+1
	  IF (i.EQ.5) THEN
	     read(text(5:9),'(i5)') dmax 
	     goto 20
	  ENDIF
      ENDIF
      goto 10
 20   CONTINUE
      close(1)

c
c initalize station list
c
      do i=1,500
	stlist(i)=' '
      enddo

c
c read earthquake data and get stations with a distance of less than dmax 
c
      open(2,file=qname,status='OLD')
      stmax=0
  200 CONTINUE
      call indata(2,nstat,nphase,nhead,nrecord,type,exp,data,id)
      IF (nrecord.EQ.0) THEN
	GOTO 300
      ENDIF
      i=nhead+1
  210 CONTINUE 
	read(data(i)(2:5),'(a4)') stat 
	read(data(i)(71:75),'(f5.0)',err=250) dist
	k=0
	IF (DIST.LE.dmax) THEN
  220   CONTINUE 
	  k=k+1
	  IF (stat.EQ.stlist(k)) THEN
	    GOTO 230
	  ENDIF
	  if (k.GT.stmax) THEN
	    stlist(k)=stat
	    stmax=stmax+1
	    write(*,*) 'station :',k,'. ',stat 
	    goto 230
	  ENDIF
	  goto 220
	ENDIF
  230   CONTINUE
	i=i+1
	if (i.EQ.nrecord) THEN
	  GOTO 250 
	ENDIF
	goto 210
  250 CONTINUE 
      GOTO 200
  300 CONTINUE
      close(2)
c 
c write selstat.lis (station selection file)
c
      open(2,file='selstat.lis',status='unknown')
  400 CONTINUE
      write(*,*)
      write(*,*) 'create station selection file: selstat.lis'
      write(2,'(a)') '#'
      write(2,'(a)') '# STATION SELECT FILE FOR PROGRAM VELEST'
      write(2,'(a)') '#'
      write(2,'(a)') '# STATIONS WILL BE USED IN THE VELEST'
      write(2,'(a)') '#      INVERSION PROGRAM'
      write(2,'(a)') '#'
      write(2,'(a)') '# COMMENT LINES START WITH #'
      write(2,'(a)') '#'
  410 CONTINUE
      DO 500 i=1,stmax
	write(2,'(a4)') stlist(i)
  500 CONTINUE
      close(2)
 
      RETURN
      END


c --------------------------------------------------
      SUBROUTINE create_model(mname)
c
c     THIS SUBROUTINE CONVERTS THE MODEL GIVEN IN THE STATION0.HYP FILE
c     TO THE MODEL USED AS INPUT IN VELEST.
c
c
c model file name
      character*80 mname
c length of model file name
      integer mind
c depth of top of layer
      real depth(100)
c velocity in layer
      real vel(100)
c counter for blank lines
      integer blank
c text line
      character*80 text
c number of layers
      integer layers
c ratio between p and s velocity
      real psratio

C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       integer  read01,                    ! Input unit 1.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       character*80 chr_text               ! Text string.
       character*60 top_directory

C
C    ============= end of list ==========
c---directory separator character
      character*1 dchar
c
      integer ind

      write(*,*)
c
c   get directory structure
c
      call topdir(top_directory)
      ind=index(top_directory,' ')-1

      call dir_char(dchar)         ! dirctory delimiter character
c
      chr_text = top_directory(:ind)//
     &           dchar // 'DAT' //dchar                 //
     &           'STATION0.HYP'

      ind=index(chr_text,' ')-1
 

c
c read STATION0.HYP file to get velocity model
c


	  write(*,*) 'STATION0.HYP from local directory'
	  call sei open( old$+warn$,       ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'STATION0.HYP',   ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      if (code.ne.0) then
	    write(*,*) 'STATION0.HYP from:'       
	    write(*,*) chr_text(:ind) 
	  call sei open( old$+warn$,       ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   chr_text(:ind),   ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

       endif

       if (code.NE.0) then
	 write(*,*) 'STATION0.HYP not found !!!'
	 stop
       endif

      mind=INDEX(mname,' ')-1
      open(2,file=mname(1:mind),status='unknown') 
      blank=0
      layers=0
c 
c write title
c
      write(2,'(a,a)') ' Model: ',mname(1:mind)
     
 100  CONTINUE
      READ(read01,'(a)',end=500) text 
      if (text.EQ.' ') then
	blank=blank+1  
      endif
      if (blank.EQ.2.AND.(text.NE.' ')) then
	layers=layers+1 
	read(text(3:7),'(f5.2)') vel(layers)
	read(text(9:14),'(f6.1)') depth(layers) 
      endif
      if (blank.EQ.3.AND.(text.NE.' ')) then
	read(text(16:20),'(f5.1)') psratio 
	blank=4
      endif

      goto 100
 500  CONTINUE

c
c write layer model
c
      write(text,'(a,a)') '           vel,depth,vdamp,',
     &'phase(f5.2,5x,f7.2,2x,f7.3,3x,a1) '
      if (layers.LT.10) THEN
	 write(text(1:2),'(1a,i1)') ' ',layers
      ELSEIF (layers.LT.100) THEN
	 write(text(1:3),'(1a,i2)') ' ',layers
      ELSEIF (layers.GE.100) THEN
	 write(text(1:4),'(1a,i3)') ' ',layers
      ENDIF

      write(2,'(a)') text
      text=' '

 600  CONTINUE
      write(*,*)
      write(*,*) 'Create model file: ',mname(1:mind)
      DO 700 i=1,layers
	write(text(1:5),'(f5.2)') vel(i)
	write(text(11:17),'(f7.2)') depth(i)
	write(text(20:26),'(f7.3)') 1.0
	IF (i.EQ.1) THEN
	  write(text(38:53),'(a)') 'P-VELOCITY MODEL'
	  IF (depth(i).LE.0) THEN 
	    write(text(11:17),'(a7)') '  -5.00'
	  ENDIF 
	ENDIF
	write(2,'(a)') text
	text=' '
 700  CONTINUE
      text=' '

      if (layers.LT.10) THEN
	 write(text(1:2),'(1a,i1)') ' ',layers
      ELSEIF (layers.LT.100) THEN
	 write(text(1:3),'(1a,i2)') ' ',layers
      ELSEIF (layers.GE.100) THEN
	 write(text(1:4),'(1a,i3)') ' ',layers
      ENDIF

      write(2,'(a)') text
      text=' ' 
      DO 800 i=1,layers
	write(text(1:5),'(f5.2)') vel(i)/psratio
	write(text(11:17),'(f7.2)') depth(i)
	write(text(20:26),'(f7.3)') 1.0
	IF (i.EQ.1) THEN
	  write(text(38:53),'(a)') 'S-VELOCITY MODEL'
	  IF (depth(i).LE.0) THEN
	    write(text(11:17),'(a7)') '  -5.00'
	  ENDIF
	ENDIF
	write(2,'(a)') text
	text=' '
  800 CONTINUE
      write(2,*)  
      CLOSE(read01)
      CLOSE(2)

      RETURN
      END
