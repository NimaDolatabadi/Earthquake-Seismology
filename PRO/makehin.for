c
c   Program makes a input files for hypoinvers using curent
c   seisan station file. The files made are:
c
c   hypinv.sta   : station file
c   hypinv.mod   : model
c   hypinst      : instruction file   
c
c
c   J Havskov October 93
c
c
c   updates:
c
c   nov 5 93 by jh : bug
c   aug 8 94       : new hypocenter, fix minimum number of stations to 3
c   feb 99 jh      : ------------- version 7.0 upgrade -----------------
c                    rem ref to computer, use first 4 chars of 5 char codes
c   sep 2014     jh: update to new hypoinverse2000
c   oct 28, 2016 jh: fix so program also works for high accuracy, fix
c                    format error reading model
c
      implicit none
      character*80 inst(100)    ! for hypinST FILE
      character*80 line         ! one line
c--- name of station-model file in current directory
      character*80 cur_file
c--- name of stationmodel in DAT directory
      character*80 dat_file	  	  
      character*60 top_directory   ! seisan top dir
      character*1 dchar         ! dir sep. char
      real d1,d2,vel,depth,z,amag,bmag,cmag,vpvs
      real latmin,lonmin        ! minutes of lat and lon
      integer i,minsta,itp
      logical hexist
      character*200 text
	  

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      text=' '

c
      bmag=0.0
	  
c	  	  	  	  	  	  
      call dir_char(dchar)
      call topdir(top_directory)
      ITP=index(top_directory,' ')-1
c
c   find station file to use
c
c
c  make file name for both files in current and dat directory
c
      cur_file(1:12)='STATION0.HYP'
      dat_file(1:itp)=top_directory(1:itp)	  
      dat_file(itp+1:itp+1)=dchar
      dat_file(itp+2:itp+4)='DAT'	  
      dat_file(itp+5:itp+5)=dchar
      dat_file(itp+6:itp+17)=cur_file(1:12)	  	  
      open(1,file=cur_file,status='old',err=1)
      goto 2
 1    continue	  
      OPEN(1,FILE=dat_file,STATUS='OLD',err=3)
      goto 2
 3    continue
      write(6,*)' No station file'
      stop
 2    continue
c
c   open output files
c
      open(2,file='hypinv.sta',status='unknown')
      open(3,file='hypinv.mod',status='unknown')
      inquire(file='hypinst',exist=hexist)
      if(hexist) then
         write(6,*)' Will use exising hypinst file'
         write(6,*)' Assume hypiniv.mod and hypinv.sta also generated'
         stop
      else
         open(4,file='hypinst',status='unknown')
      endif
c
c   set some defaults
c
      do i=1,100
         inst(i)=' '
      enddo
      inst(1)(1:6)='CRH1 '
      inst(1)(7:22)='''hypinv.mod'''
      inst(2)(1:16)='STA '//'''hypinv.sta'''
      inst(3)(1:15)='PRT '//'''print.out'''
      inst(4)(1:16)='SUM '//'''hypinv.sum'''
      inst(5)(1:19)='COP 3              '     ! input format to archive
      inst(6)(1:19)='ERF T              '     ! give error messgaes
      inst(7)(1:19)='LST 0              '     ! no print of station location
      inst(8)(1:19)='KPR 1              '     ! print all staitons
      inst(9)(1:19)='CON 99 0.04 0.001  '     ! iteration stop
      inst(10)(1:19)='CAR 1              '     ! input format to archive
      inst(11)(1:19)='LET 5 0 0 0 0      '     ! only use station name
c
c   read to end of seisan station file and fish out relevant info
c
c
c   some reset test parameters
c
      read(1,'(a)') line
c
 10   continue
      if(line(1:14).eq.'RESET TEST(07)')
     *read(line(16:25),'(f10.3)') amag
      if(line(1:14).eq.'RESET TEST(08)')
     *read(line(16:25),'(f10.3)') bmag
      if(line(1:14).eq.'RESET TEST(09)')
     *read(line(16:25),'(f10.3)') cmag
      read(1,'(a)')line
      if(line(1:10).ne.'          ') goto 10
c
c   station coordinates
c
      read(1,'(a)') line
 20   continue   
      text=' '   
c
c   station
c
      if(line(2:2).ne.' ') then
         text(1:5)=line(2:6)
      else
         text(1:5)=line(3:6)
      endif
c
c  lat
c
      text(16:17)=line(7:8)
      read(line(9:13),'(f5.3)') latmin
      write(text(19:24),'(f6.3)') latmin
c      text(19:23)=line(9:13)
      text(26:26)=line(14:14)
c
c   lon
c
      text(27:29)=line(15:17)
      read(line(18:22),'(f5.3)') lonmin
      write(text(31:36),'(f6.3)') lonmin
c      text(31:35)=line(18:22)
      text(38:38)=line(23:23)
      text(39:42)=line(24:27)
      write(2,'(a)') text(1:79)
      read(1,'(a)')line
      if(line(1:10).ne.'          ') goto 20
c
c   model
c
      read(1,'(a)') line
      write(3,*)'Seisan model'
 30   continue
      read(line,'(2f7.3)') vel,depth
      write(3,'(2f5.1)') vel,depth
      read(1,'(a)') line
      if(line(1:10).ne.'          ') goto 30
c
c  vp/vs, start depth and distance weight
c
      read(1,'(f5.1,3f5.2)') z,d1,d2,vpvs
c
c  mininum number of stations
c
c     do i=1,3
c        read(1,'(a)') line
c     enddo
c     read(1,'(47x,i3)') minsta
   
c
c   finish control file and write out if not exisitng
c
      if(minsta.lt.3) minsta=3   ! minimum required

      write(inst(12),'(a,i3)')'MIN ',minsta
      i=z
      write(inst(13),'(a,i3,a)')'ZTR ',i,' F'
      write(inst(14),'(a,f5.2)') 'POS ',vpvs
c
c   calcualte how many times nearestr distance correspond to xfar (d2)
c
      i=d2/d1
      if(i.le.1) i=2
      write(inst(15),'(a,f6.1,a,i2)') 'DIS 4 ',d1,' 1',i
c
c    use w lee mag
c
C      if(bmag.eq.0.0) then
C        amag=-0.87
c        bmag=2.0
c        cmag=0.0035
c      endif
c      i=0
c      write(inst(14),'(a,2f7.4,i2,1x,f7.4,a)')'DUR ',
c     *amag,bmag,i,cmag,', 4*0, 9999'
c
c       inst(16)='DUR   -.87 2 0 .0035 0, 5*0, 9999 0'
      
c
      write(inst(16),'(a)')'PHS '//'''norhin.out'''
      write(inst(17),'(a)')'ARC '//'''hypinv.out'''
      write(inst(18),'(a)')'LOC '
      write(inst(19),'(a)')'STO '
      if(.not.hexist) then
         do i=1,19
            write(4,'(a)') inst(i)
         enddo
      endif
      stop
      end



