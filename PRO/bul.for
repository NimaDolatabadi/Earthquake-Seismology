c     Program to make Seismological Bulletins
c
c     January 1993, Conrad Lindholm
c     Updates:
c     jul 8 93 by jh: VERSION 3.0 *****
c     jul 16        : bul.inp to BUL.INP
c     aug   93      : conrad fixes ?????
c     aug26    by jh: pc adoption, many changes and bugs !!!!
c     sep 10        ; bug, scratch file 8, pc
c     sep 16        : check month range, bug with -onlyhypo
c     sep 21,22     : check for non existing phases, residual 
c     apr 27 94     : print one line pr event
c     jul           : bug with several months
c     nov 16        : august
c     jan 95        : ************ version 5.0 ****************
c     jan 20        : amplitude to 8 digits
c     feb 7         : check for ctl z, pc problem
c     mar 27  95  jh: add page selection
c     april 25      : always give explosion info
c     aug 4         : write amplitudes lt 1
c     aug 16        : bug page selection
c     may 31 96     : enable 8 character phase names
c     sept 96     lo: dont print probable explosions without readings 
c     jan 97      ha: bug in weight in phase
c     May 97      mv: Now possible to change default fontsize for
c                   : event data (see %fontsize=)
c     Nov 5 97    jh: do not print an explosion if only one phase
c     Jan 13 98     : do not print explosions if no epicenter
c-----------------------------------------------------------------------------
c     oct 17 98   jh: -------------   verson 7.0 check -------------      
c                     5 char station check, no change
c     sep 19 2001 jh: dist to integer
c     jan 30 2003 jh: allow negative magnitudes
c     feb 19          diabled temporarely
c     jan 6, 04 jh  : also do not print explosions without location
c                     and/or readings
c     jan 27 04 jh  : do not print event with no phases
c     jan 5  07 jh  : now really fix so neg magnitudes can be printed
c     jan 28 11 jh  : add parameter skip, so it is possible to print all
c     feb  7 12 pv  : changed tmonth(12)*9 to tmonth(12)*10
c     may 23 17 lo  : print model character
c     aug 11 17 lo  : print comments that end on 'B3'
c
c     To make a defintion in PostScript:  /t {postscript_command} def
c         t is after this synonymous with postscript_command
c
C  Font index
C         1 = Times-Roman
C         2 = Times-Italic
C         3 = Times-Bold
C         4 = Helvetica
C         5 = Helvetica-Oblique
C         6 = Helvectica-Bold
C         7 = Courier               Must be used on tabular data !!!!
C         8 = Courier-Oblique
C         9 = Courier-Bold
c
c  Input files:
c         A Nordic formatted F-file
c         A file called "BUL.INP" which is expected to be found in ~/DAT
c  Output file:
c         A PostScript bulletin file: "bul.ps"
c
      
      implicit none
      include 'seidim.inc'
      character*80 data(max_data),infile/' '/
      character*60 top_directory,timecard,hypcard,agcard,depcard
      character*60 modcard
      character*150 card,dummy,dummy2,arg,outcard
      character*1 type, exp,t1,t2,t3,cult,answer
      character*1 ucase,fixed,ns,ew,fixhyp
      character*1 model
      character*2 intscale
      character*3 agency,ag1,ag2,ag3
      character*10 disttype
      character*80 args(20)
      character tmonth(12)*10
      integer nstat,nhead,nrecord,nphas
      integer year,month,day,hour,min
      integer oport,font
      integer nargs, nars,iargc
      integer maxint,id
      character*1 skip                ! if no, use all events
      real sec
      real xdummy
      real lat,lon,depth,rms
      real size,x,y,dx,dy,xleft,ytop,ybottom,fontsize
      real m1,m2,m3,xminmag
      real rmspos,hyppos,agpos,indent,deppos,modpos
      integer firstmonth,lastmonth,npage
      integer i,j,k,nbend
      logical pc,unix,vax,frontpage,center,exist
      logical onlyhypo,nofrontpage,minmag
      integer*2 i2,istatus
      integer seiclen

c
c ------ Initialize
c
c      data tmonth / 'Enero       ', 'Febrero     ', 
c     &'Marzo       ', 'Abril       ',
c     &'Mayo        ', 'Junio       ', 'Julio     ', 
c     &'Agosto      ', 'Septiembre  ',
c     &'Octubre     ', 'Noviembre   ', 'Diciembre  ' /
      data tmonth / 'January  ', 'February ', 
     &'March    ','April     ',
     &'May      ', 'June      ', 'July     ', 
     &'August   ', 'September ',
     &'October  ', 'November  ', 'December ' /


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      frontpage = .false.
      nofrontpage = .false.
      onlyhypo = .false.
      center = .false.
      minmag = .false.
c
c------- Define standard top bottom of the page and line spacing in incheottoms
c------- Default font is Times-Roman  (1)
c
c------- rmspos =x position of rms information in inches
c------- hyppos =x position of hypoinformation in inches
c------- agpos  =x position of agencyinformation in inches
      deppos = 5.5
      rmspos = 5.5
      hyppos = 3.5
      modpos=6.15
      agpos = 6.65

      indent = 1.1
      xleft = 1.0
      ytop = 10.5
      ybottom = 1.00
      dx=0
      dy = 0.15
      font = 1
      fontsize = 0.15
      npage = 1
c
c------ Find arguments passed
c
       call get_arguments(nars,args)
       if(nars .ne. 0)then
	do i = 1,nars
      i2=i
	 if(args(i)(1:2).eq.'-h'.or.args(i)(1:2).eq.'-H')then
	   write(*,*)'  Options:'
	   write(*,*)' -frontpage: Only frontpages are printed.'
	   write(*,*)' -nofrontpage: No frontpages are printed.'
	   write(*,*)' -onlyhypo: Only hypocenter solutions are printed.'
           write(*,*)' -minmag x.x : Only hypocenter ',
     &'solutions are printed',
     &'          if magnitude is lower than the requested.'
	   stop
	 endif
	 if(args(i)(1:10) .eq. '-frontpage')then
	   frontpage = .true.
	 endif
	 if(args(i)(1:9) .eq. '-onlyhypo')then
	   onlyhypo = .true.
	 endif
	 if(args(i)(1:12) .eq. '-nofrontpage')then
	   nofrontpage = .true.
	 endif
	 if(args(i)(1:7) .eq. '-minmag')then
           i2 = i2 + 1
            read(args(i2),'(f5.1)') xminmag
	   minmag = .true.
	 endif
	enddo
      endif
c
c------- Get first and last month in the data
c
      write(*,'(a,$)')' Enter name of your file (Nordic format): '
      read(*,'(a)')infile
      write(6,*) 
     *' Skip explosions,events with less than 3 readings (y=def,n)'
      read(5,'(a)') skip

      open(1,file=infile,status='old')
c
c   scratch file for interactive input
c
      open(8,status='scratch')
      i = 0
1     call indata(1,nstat,nphas,NHEAD,NRECORD,TYPE,EXP,DATA,id)
      if(nrecord .eq. 0) go to 2
      read(data(1)(2:8),'(i4,1x,i2)')year,month
      i = i + 1
      if(i .eq. 1)firstmonth = month
      go to 1
2     lastmonth = month
      close(1)
      if(firstmonth.le.0) then
         write(6,*)' Something wrong with data of first event'
         stop
      endif
      if(lastmonth.gt.12) then
         write(6,*)' Something wrong with date of last event'
         stop
      endif
      write(6,'(5a)') ' You are going to make a bulletin for the ','
     *months ',tmonth(firstmonth), ' to ',tmonth(lastmonth)
      write(6,'(a,$)')' Is that ok (y/n)'
      read(5,'(a1)') answer
      if(answer.ne.'Y'.and.answer.ne.'y') stop
      

c
c-------- Make the first page
c
      oport = 2
      call PSOPEN('bul.ps',6,oport,font)

      if(nofrontpage)then
	  call psfont(oport,font,fontsize)
          go to 10
      endif

      call topdir(top_directory)
      i=index(top_directory,' ') - 1
      inquire(file='BUL.INP',exist=exist)
      if(exist)then
        open(1,file='BUL.INP',status='old')
      else
        open(1,file=top_directory(1:i)//'/DAT/BUL.INP',status='old')
      endif
c
c----- Top left corner of page is x = 1 inch and y = 11 inches
c
      x = xleft
      y = ytop

3     card= ' '
      read(1,'(a)',end=10)card
c
c   check for ctlz, a problem on pc files moved forth and back on the net
c
      do i=1,80
        if(ichar(card(i:i)).eq.26) goto 3
      enddo
      if( card(1:1) .eq. '#') go to 3
      if( card(1:10) .eq. '%Next Page') go to 5
c
c----- Read position, size etc. of the next line
c
      if( card(1:6) .eq. '%xpos=')then
         rewind 8
         write(8,'(a)') card(7:80)
         rewind 8
         read(8,*)x
conrad	 read(card(7:80),*)x
	 go to 3
      endif
      if( card(1:6) .eq. '%ypos=')then
conrad	 read(card(7:80),*)y
         rewind 8
         write(8,'(a)') card(7:80)
         rewind 8
         read(8,*)y
         go to 3
      endif
      if( card(1:6) .eq. '%font=')then
conrad	  read(card(7:80),*)font
         rewind 8
         write(8,'(a)') card(7:80)
         rewind 8
         read(8,*)font
	  call psfont(oport,font,size)
          go to 3
      endif
      if( card(1:6) .eq. '%size=')then
conrad	  read(card(7:80),*)size
         rewind 8
         write(8,'(a)') card(7:80)
         rewind 8
         read(8,*)size
	  call psfont(oport,font,size)
          go to 3
      endif
c changes default fontsize value
      if( card(1:10) .eq. '%fontsize=')then
         rewind 8
         write(8,'(a)') card(11:80)
         rewind 8
         read(8,*)fontsize
         goto 3
      endif
      if( card(1:7) .eq. '%Months') then
conrad	write(card,*)(tmonth(i),i=firstmonth,lastmonth)
        
c       write(card,'(a)')(tmonth(i),i=firstmonth,lastmonth)
        card(1:9)=tmonth(firstmonth)
        if(lastmonth.ne.firstmonth) then
           card(10:13)=' to '
           card(14:22)=tmonth(lastmonth)
        endif
        if(center)then
  	  call pscenter(oport,x,y,card,NBEND(card))
          center = .false.
        else
          call pswrite(oport,x,y,card,NBEND(card))
        endif
	go to 3
      endif
      if( card(1:5) .eq. '%Year') then
	write(card,'(i4)')year
        if(center)then
  	  call pscenter(oport,x,y,card,NBEND(card))
          center = .false.
        else
          call pswrite(oport,x,y,card,NBEND(card))
        endif
	go to 3
      endif
      if( card(1:7) .eq. '%center') then
	center = .true.
	go to 3
      endif

      if(center)then
	call pscenter(oport,x,y,card,NBEND(card))
      else
        call pswrite(oport,x,y,card,NBEND(card))
      endif
      call position(x,y,dx,dy)
      center = .false.

      go to 3

5     call psclear(oport)
      x = xleft
      y = ytop
      go to 3

10    continue
      if(frontpage) then
         write(6,*)' You have selected to only print front pages'
         go to 999
      endif
c
c---------------------------------------------
c---------- Now start to work with the data
c---------------------------------------------
c
      call psclear(oport)
      x = xleft
      y = ytop


      open(1,file=infile,status='old')
7     call indata(1,nstat,nphas,NHEAD,NRECORD,TYPE,EXP,DATA,id)
      if(nrecord .eq. 0) go to 999
c
c dont print events that are probable explosions or explosion
c without phase readings
c
      if(skip.ne.'n') then
      if (EXP.EQ.'P'.or.exp.eq.'p'.or.exp.eq.'E') THEN
        if (NRECORD-1-NHEAD.EQ.0) THEN
          GOTO 7
        ENDIF

        if(nphas.lt.2) goto 7   
  
c
c   also do not print if no hypocenter
c
        if(data(1)(24:38).eq.'               ') goto 7
      ENDIF
c
c   if no phases at all, skip
c
      if(nphas.lt.1) goto 7   !jh jan 27 03
      endif
        
      write(6,'(a)')data(1)
      if(type .eq. 'L')disttype='  Local   '
      if(type .eq. 'R')disttype='  Regional'
      if(type .eq. 'D')disttype='  Distant '

c      read(data(1),100)year,month,day,hour,min,sec,
      read(data(1),100)year,month,day,hour,min,sec,model,
     & lat,lon,depth,fixed,fixhyp,agency,rms,m1,t1,ag1,m2,
     & t2,ag2,m3,t3,ag3
c100   format(1x,i4,1x,2i2,1x,2i2,1x,f4.0,3x,f7.3,f8.3,
100   format(1x,i4,1x,2i2,1x,2i2,1x,f4.1,a1,2x,f7.3,f8.3,
     & f5.0,a1,a1,a3,3x,f4.0,3(f4.1,a1,a3))
c
c------- Write event header card
c
      write(timecard,300)tmonth(month),day,year,hour,min,sec
300   format(a,' ',i2,'  ',i4,'   Hour:  ',i2,':',i2,' ',f4.1)

      ns = 'N'
      ew = 'E'
      if(lat .lt. 0.001)then
         ns = 'S'
         lat = -lat
      endif
      if(lon .lt. 0.001)then
         ew = 'W'
         lon = -lon
      endif
      write(hypcard,400) fixhyp,lat,ns,lon,ew
c400   format(a1,' Lat:  ',f6.2,a1,'    Lon:  ',f7.2,a1)
400   format(a1,' Lat:  ',f6.2,a1,'  Lon:  ',f7.2,a1)

c      write(depcard,500)int(depth),fixed
      write(depcard,500)depth,fixed
c500   format('Depth:',i3,a1)
c500   format('Dep:',i3,a1)
500   format('D:',f5.1,a1)

      write(modcard,550) model
550   format('Mod: ',a1)

      write(agcard,600)'Ag: ',agency,disttype
c      write(agcard,600)'Agency: ',agency,disttype
600   format(a,a3,'  ',a10)

      if(lat .eq. 0.0 .and. lon .eq. 0.0)then
        hypcard = ' '
        depcard = ' '
      endif

c
c----- New page if the header comes too low
c
      if(onlyhypo)then
        xdummy = ybottom + 0.75
      else
        xdummy = ybottom + 1.0
      endif
      if(y .lt. xdummy)then
	write(dummy,'(i3)')npage
	x = 4.0
	y = 0.5
	call psfont(oport,font,fontsize)
	CALL PSWRITE(oport,x,y,dummy,3)
	npage = npage + 1
	CALL PSCLEAR(oport)
	call psfont(oport,3,fontsize)
	x = xleft
	y = ytop
      else
	call psfont(oport,3,fontsize)
      endif
c
c------- write main header; Time and position
c
      call position(x,y,dx,dy)
      call pswrite(oport,x,y,timecard,NBEND(timecard))
      call pswrite(oport,hyppos,y,hypcard,NBEND(hypcard))
      call pswrite(oport,deppos,y,depcard,NBEND(depcard))
      if (model.ne.' ') then
        call pswrite(oport,modpos,y,modcard,NBEND(depcard))
      endif
      call pswrite(oport,agpos,y,agcard,NBEND(agcard))
c
c------- Write Magnitudes and RMS INFO
c
      card = ' '
      write(card,301)m1,'M',t1,ag1,m2,'M',t2,
     + ag2,m3,'M',t3,ag3
301   format('Magnitudes:  ',3(f4.1,2a1,1x,a3,4x))
cjh to be fixd to report neg mag
c     if(m1 .le. 0.0)card(14:26) = '       '
c     if(m2 .le. 0.0)card(27:39) = '       '
c     if(m3 .le. 0.0)card(40:52) = '       '
c
c   report magnitude if agency is not blank
c   assuming then the magnitude is not valid
c
      if(ag1 .eq.'   ')card(14:26) = '       '
      if(ag2 .eq.'   ')card(27:39) = '       '
      if(ag3 .eq.'   ')card(40:52) = '       '

      if(m1.ne.0.0 .or. m2.ne.0.0 .or. m3.ne.0.0 )then
        call position(x,y,dx,dy)
        call pswrite(oport,indent,y,card,NBEND(card))
      endif
c
c---- RMS iNfo written on fixed 5.0 inches x-coordinate
c
      if(lat .ne. 0.0 .and. lon .ne. 0.0)then
        write(card,302)rms
302     format('Rms:',f4.1,' secs')
c        if(exp .eq. 'E')card(20:29)='Explosion'
c        if(exp .eq. 'P')card(20:36)='Likely Explosion'
        if (exp.ne.' ') card(20:26) = 'Type: '//exp
        if(m1.eq.0.0 .and. m2.eq.0.0 .and. m3.eq.0.0 )then
          call position(x,y,dx,dy)
        endif
        call pswrite(oport,rmspos,y,card,NBEND(card))
      else
        if(exp.eq.'E'.or.exp.eq.'P') then
            card(1:11)='           '   ! take magnitude away
c            if(exp .eq. 'E')card(20:29)='Explosion'
c            if(exp .eq. 'P')card(20:36)='Likely Explosion'
            if (exp.ne.' ') card(10:16) = 'Type: '//exp
            if(m1.eq.0.0 .and. m2.eq.0.0 .and. m3.eq.0.0 )then
c               call position(x,y,dx,dy)
            endif
            call pswrite(oport,rmspos,y,card,NBEND(card))
        endif
      endif
c
c write comment lines data(79:80) = 'B3'
c
      do i = 2,nhead
	if(data(i)(79:80).eq.'B3')then
	  write(card,'(a)')data(i)(2:seiclen(data(i)(2:79)))
	  call position(x,y,dx,dy)
	  call pswrite(oport,indent,y,card,NBEND(card))
	endif
      enddo
c
c------- Write extra "Bul-headers" and macro information
c
      do i = 2,nhead
	if(data(i)(80:80).eq.'3'.and.data(i)(1:5).eq.' Bul:')then
	  write(card,'(a)')data(i)(6:79)
	  call position(x,y,dx,dy)
	  call pswrite(oport,indent,y,card,NBEND(card))
	endif
        if(data(i)(80:80) .eq. '2')then
          read(data(i),200)cult,maxint
200       format(24x,a1,2x,i2)
	  dummy = ' '
	  if(cult .eq. 'C')dummy = '  Casualties reported'
	  if(cult .eq. 'D')dummy = '  Damage reported'
	  if(maxint .gt. 0.)then
            write(card,401)maxint,dummy(1:21)
401         format('Earthquake was felt;   Maximum intensity: ',
     +       i2,'  ',a)
          else
            write(card,'(a)')'Earthquake was felt'
          endif
        call position(x,y,dx,dy)
        call pswrite(oport,indent,y,card,NBEND(card))
        endif
	   
      enddo
c
c-------- If only hypocenter wanted, then skip the rest
c
      xdummy = max(m1,m2,m3)
      if(onlyhypo .or. (minmag .and. xdummy .lt. xminmag))then
        card = ' '
        call position(x,y,dx,dy)
        call pagecheck(x,y,ybottom,xleft,ytop,npage,oport,
     +                                        font,fontsize)
        call psfont(oport,font,fontsize)
        call pswrite(oport,x,y,card,NBEND(card))
        go to 7
      endif

c
c-------- Now read and write phase information; Font Courirer
c
      call psfont(oport,7,fontsize)
      outcard=' STAT CO DIST AZI PHASE '
      dummy  =' P HRMN SECON TRES CODA'
      dummy2 =' AMPL PERI BAZ ARES VELO  WT'
      card = outcard(1:24)//dummy(1:23)//dummy2(1:28)
      call position(x,y,dx,dy)
      call pagecheck(x,y,ybottom,xleft,ytop,npage,oport,
     +                                        font,fontsize)
      call pswrite(oport,x,y,card,NBEND(card))
      do i = nhead+1,nrecord
	card = data(i)
	call position(x,y,dx,dy)
        call pagecheck(x,y,ybottom,xleft,ytop,npage,oport,
     +                                        font,fontsize)
        call reformat(card,outcard)
	call pswrite(oport,x,y,outcard,NBEND(outcard))
      enddo
c
c--------- Back for a new event; Font is reset   
c

      go to 7

999   continue
      write(dummy,'(i3)')npage
      x = 4.0
      y = 0.5
      call psfont(oport,font,fontsize)
      CALL PSWRITE(oport,x,y,dummy,3)

      write(*,*)' Bulletin is now written in the file: bul.ps'
      call select_pages(oport)  ! page selection
      call psclose(oport)

      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine position(x,y,dx,dy)
      real x,y,dx,dy
c     Position is given in units of inches where
c     origo is in the lower left corner of an A4 page

      y = y - dy

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE PSOPEN(CNAME,ILEN,IUNIT,font)
      CHARACTER*(*) CNAME
      integer font
C Input: CNAME, ILEN
C Output: IUNIT
      open(unit=iunit,file=cname(1:ilen),status='unknown')

      WRITE(IUNIT,'(A23)') '%!PS-Adobe-2.0 EPSF-2.0'
      WRITE(IUNIT,'(A26)') '%%BoundingBox: 0 0 539 539'
      WRITE(IUNIT,'(A13)') '%%EndComments'
C defintions necessary for pscenter routine
      WRITE(IUNIT,'(A12)') '/WIDTH 8 def'
      WRITE(IUNIT,'(A37)') '% Usage: ypos pagewidth string center'
      WRITE(IUNIT,'(A35)') '/center { stringwidth pop sub 2 div'
      WRITE(IUNIT,'(A17)') 'exch moveto } def'
C transform from 1 unit = 1/72 inch to 1 unit = 1 inch
      WRITE(IUNIT,'(A11)') '72 72 scale'
      CALL PSFONT(IUNIT,font,0.15)
C save these definitions
      WRITE(IUNIT,'(A5)') 'gsave'
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE PSCLOSE(IUNIT)
      WRITE(IUNIT,'(A8)') 'showpage'
      WRITE(IUNIT,'(A8)') 'grestore'
      close(unit=IUNIT)
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE PSCLEAR(IUNIT)
      WRITE(IUNIT,'(A8)') 'showpage'
C restore old definitions      
      WRITE(IUNIT,'(A8)') 'grestore'
C save these definitions
      WRITE(IUNIT,'(A5)') 'gsave'
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE PSCENTER(IUNIT,X,Y,STR,ILEN)
      REAL X,Y
C Units in inches, (0,0) at lower-left corner of page
      CHARACTER*(*) STR
      INTEGER ILEN
      WRITE(IUNIT,*) X, Y, ' moveto'
conrad      WRITE(IUNIT,*) Y,' WIDTH '//'('//STR(1:ILEN)//')',' center'
conad      WRITE(IUNIT,*) '('//STR(1:ILEN)//') show'
      WRITE(IUNIT,200) Y,STR(1:ILEN)
 200  format(f10.2,' WIDTH (',a,') center')
      WRITE(IUNIT,201) STR(1:ILEN)
 201  format('(',a,') show')
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE PSWRITE(IUNIT,X,Y,STR,ILEN)
      REAL X,Y
C Units in inches, (0,0) at lower-left corner of page
      CHARACTER*(*) STR
      INTEGER ILEN
      WRITE(IUNIT,*) X, Y, ' moveto'
conrad      WRITE(IUNIT,*) '('//STR(1:ILEN)//') show'
      write(iunit,201) str(1:ilen)
 201  format('(',a,') show')
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE PSFONT(IUNIT,IFONT,SIZE)
      INTEGER IUNIT,IFONT
      REAL SIZE
C IUNIT - File unit number
C IFONT - Font index
C         1 = Times-Roman
C         2 = Times-Italic
C         3 = Times-Bold
C         4 = Helvetica
C         5 = Helvetica-Oblique
C         6 = Helvectica-Bold
C         7 = Courier
C         8 = Courier-Oblique
C         9 = Courier-Bold
C SIZE  - Size of fonts in inches
C
C Note that the Courier font family (font 7-9) is a monospaced,
C or fixed-pitch (fixed width) font suitable for use in
C tabular material, program listings, or word processing.
C
      CHARACTER*17 CFONTS(9)
      INTEGER      LFONTS(9)
      DATA CFONTS / 'Times-Roman',
     +              'Times-Italic',
     +              'Times-Bold',
     +              'Helvetica',
     +              'Helvetica-Oblique',
     +              'Helvectica-Bold',
     +              'Courier',
     +              'Courier-Oblique',
     +              'Courier-Bold' /
      DATA LFONTS / 11, 12, 10, 9, 17, 15, 7, 15, 12 /
 
      IF2 = MIN0(9,MAX0(1,IFONT))
 
      WRITE(IUNIT,'(A)') '/'//CFONTS(IF2)(1:LFONTS(IF2))//' findfont'
      WRITE(IUNIT,*) SIZE,' scalefont'
      WRITE(IUNIT,'(A7)') 'setfont'
      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C NBEND    UT-F-U   Created: 8/11/89 Copied : 08/28/89 Made at NORSAR
      INTEGER FUNCTION NBEND(STRING)
      CHARACTER*(*) STRING
C.======================================================================
C.    PURPOSE
C     find_Non_Blank_END_of_string                                  UT<<
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C..   STRING - Character string
C
C.    OUTPUT
C..   NBEND  - Position of last non blank character in string
C.----------------------------------------------------------------------
C.    PROGRAMMER    Stein Holger Pettersen
C.    CREATION_DATE 19 Jun 1989
C.    MADE_AT  NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
 
      LS = LEN(STRING)
      DO 10 I = LS,1,-1
         IF (STRING(I:I).NE.' ') GO TO 20
   10 CONTINUE
      I = 1
 
   20 NBEND = I
      RETURN
      END
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      character*1 function ucase(a)
c     convert lower case single char to upper case - machine independent
      
      character*1 chr(26),ucchr(26),a
      data chr/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     &'n','o','p','q','r','s','t','u','v','w','x','y','z'/
      data ucchr/'A','B','C','D','E','F','G','H','I','J','K','L','M',
     &'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      do 10 i=1,26
        if(a.eq.chr(i))then
          ucase=ucchr(i)
          return
        endif
10    continue
      ucase=a
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine pagecheck(x,y,ybottom,xleft,ytop,npage,oport,
     +                                        font,size)
      real x,y,ybottom,xleft,ytop,size
      integer font
      character*100 dummy
	  integer oport

      if(y .lt. ybottom )then
	write(dummy,'(i3)')npage
	x = 4.0
	y = 0.5
	call psfont(oport,font,size)
	CALL PSWRITE(oport,x,y,dummy,3)
	npage = npage + 1
	CALL PSCLEAR(oport)
c----- Font must be 7 (Courier)
	call psfont(oport,7,size)
	x = xleft
	y = ytop
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine reformat(incard,outcard)
      character*80 incard,outcard
      character*1 uncertain
      real ampl,peri,bazi,wt
      real xdist 		! distance
      integer idist		! distance

c      outcard = 
c  +' STAT CO DIST AZI PHAS P HRMN SECON TRES CODA AMPL PERI BAZ ARES VELO  WT'
c
      outcard = ' '

c
c  put in if time not accurate
c
      uncertain=' '
      if(incard(15:15) .eq. '9')then
        uncertain = '*'
      endif
c
c   remove input weight from incard if not a phase
c
      if(incard(15:15).eq.'0'.or.incard(15:15).eq.'1'.or.incard(15:15)
     *.eq.'3'.or.incard(15:15).eq.'4'.or.incard(15:15).eq.'9'.or.
     *incard(15:15).eq.'2') then
      if(incard(16:16).ne.' ') then
        continue             ! character 15 must be part of phase name
      else 
        incard(15:15)=' '    ! character 15 is probably an input weight
      endif
      endif
      outcard(1:8) = incard(1:8)
c
c   distance to integer, jh sep 2001
c
      read(incard(71:75),'(f5.0)') xdist
      xdist=xdist+0.5
      idist=xdist
      if(xdist.gt.0.51) write(outcard(9:13),'(i5)') idist
cjh sep 2001      outcard(9:13) = incard(71:75)
      outcard(15:17) = incard(77:79)
      outcard(19:27) = incard(10:18)
      outcard(28:31) = incard(19:22)
      outcard(33:37) = incard(24:28)
      outcard(38:42) = incard(64:68)
      outcard(44:47) = incard(30:33)
      if(outcard(27:27).eq.' ') outcard(27:27)=uncertain ! assume long phase 
                                                         ! name has ok time
c      write(*,*) ' debug ',incard(34:40)
      read(incard(34:40),'(g8.0)')ampl
      if(ampl.ne.0.0) then
         if(ampl.gt.1.0) then
            write(outcard(45:52),'(i8)')nint(ampl)
         else
            write(outcard(45:52),'(f8.2)') ampl
         endif
      endif
      read(incard(42:45),'(f4.0)')peri
      if(peri.ne.0.0)write(outcard(54:57),'(f4.1)')peri
      read(incard(47:51),'(f5.0)')bazi
      if(bazi.ne.0.0)write(outcard(58:61),'(i4)')nint(bazi)
      outcard(64:66) = incard(61:63)
      outcard(68:71) = incard(53:56)
      read(incard(69:70),'(i2)')iwt
       wt = real(iwt)/10.
      write(outcard(73:75),'(f3.1)')wt
c
c   check for nonexisting phase
c
      if(incard(69:70).eq.'-9') then
         outcard(73:75)='  '
         outcard(15:17)='   '
         outcard(39:42)='    '
       endif
      if(incard(69:70) .eq. ' ')outcard(73:75)=' '

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine select_pages(oport)
c
c  edit output file to select certain pages
c
      implicit none
      include 'libsei.inc'
      character*80 text
      integer first_page, last_page   ! first and last page to use
      integer code   ! error code
      integer npage  ! page counter
      integer oport  ! file unit for original file
c
      write(6,*)' Select pages, give first and last, return for all'
      read(5,'(a)') text
      if(text(1:2).eq.'  ') return           ! stop if default
      call sei get values( 2, text, code )   ! Extract 2 values.
      code = e_ok$                           ! re-store.
      first_page = array$(1)+1                    ! Retrieve.
      last_page =  array$(2)+1                    ! Ditto.
c
c   open selected page output file
c
      open(22,file='bul1.ps',status='unknown')
      rewind oport
c
c   read foreward to pages wanted, always leave first
c
      npage=-2
 1    continue
      read(oport,'(a)',end=99) text
      if((npage.ge.first_page.and.npage.le.last_page).or.npage.eq.-2) 
     *write(22,'(a)') text               ! write out page if set
      if(text(1:8).eq.'showpage') then
         npage=npage+1
         write(6,*)' Page ',npage
      endif
      goto 1
c
c  end of file
c
  99  continue
      backspace oport
	  backspace 22
      call psclose(22)

      close(22)
      write(6,*)' Selected pages in file bul1.ps'
      return
      end
