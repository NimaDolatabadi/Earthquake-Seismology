c$DEBUG
c
c   Subroutine makes a input files for hypo71 using curent
c   seisan station file and the pahse readings. The file made
c   is called hypo71.input
c   It also returns the test parameters used in the magnitude
c   calculation in array test.
c
c   updates:
c
c     20-02-2001 lo  : added extension flag for UK grid in make_hypo71_brief
c     2014.03.12 pv  : cleanup warnings
c     2014.03.14 pv : clean up common block:
c               Warning: Padding of 2 bytes required before 'id' in COMMON 'readings'
c     2014.04.29 pv  : cleanup warnings
c
c
      subroutine make_hypo71_input(test,agency,error)

      implicit none

      character*80 line            ! one line
      character*80 cur_file        ! station file in current directory
      character*80 dat_file        ! station file in DAT directory
      character*60 top_directory   ! seisan top dir
      character*1 dchar            ! '/' character

      character*5 oldsta,newsta
      CHARACTER*5 station(100)
      CHARACTER*80 data(2500)
      CHARACTER*1 typ,exp
c---agency for magnitude  
      character*3 agency
      character*80 testdat
      character*10 stat


      integer nstat,nphase,nhead,nrecord
      integer id
      integer i,j,k
      integer itp
      integer nlayers
      integer testnum
      integer lat1,lon1
      integer min

      real xnear,xfar,startz,vpvs
      real vel(100), z(100)
      real startlat,startlon,lat2,lon2,sec
c-- test variables
      real testj
      real test(200)

      character*6 stat_name
      character*1 dlat,dlon
      integer lat,lon,elev
      real mlat,mlon,del,mcor,dec_lon
      real hypo71_offset
      logical offset
      
      logical error,found
      logical trial_dep,fix_dep,trial_epi,fix_epi

      common/readings/NSTAT,NPHASE,NHEAD,NRECORD,ID,DATA,TYP,EXP
      common/model/vel,z,nlayers,startz,xnear,xfar,vpvs
      common/hypo71/ hypo71_offset


      trial_dep=.false.
      trial_epi=.false.
      fix_dep=.false.
      fix_epi=.false.


      if (error) return

      call dir_char(dchar)
      call topdir(top_directory)
      ITP=index(top_directory,' ')-1

c check for x offset to be applied to
c station coords of events near Greenwich Meridian
c      call get_hypo71_offset(coffset)
c      read(coffset,'(f3.0)')xoffset
      if(hypo71_offset.gt.0.0) then
         offset=.true.
         write(*,*) 'Applying offset ',hypo71_offset,
     1        'to station coordinates'
      else
         offset=.false.
      endif

c
c   find station file to use
c
c
c  make file name for both file in current and dat directory
c
      cur_file(1:12)='STATION0.HYP'
      dat_file(1:itp)=top_directory(1:itp)	  
      dat_file(itp+1:itp+1)=dchar
      dat_file(itp+2:itp+4)='DAT'	  
      dat_file(itp+5:itp+5)=dchar
      dat_file(itp+6:itp+17)=cur_file(1:12)	  	  
      open(1,file=cur_file,status='old',err=1)
c      write(6,2001) cur_file(1:12)
      goto 2
 1    continue	  
      OPEN(1,FILE=dat_file,STATUS='OLD',err=3)
c      write(6,2001) dat_file(1:itp+17)
      goto 2
 3    continue
      write(6,*)' No station file found'
      error=.true.
      return
 2    continue
 2001 format (' Using station file ', a)

c
c create array of stations, making sure that station don't 
c occur more than once. This seems a bit torturous but it works
c
      k=0
      do i=nhead+1,nrecord-1
         oldsta=data(i)(2:6)
         found=.false.
         do j=i+1,nrecord-1
            newsta=data(j)(2:6)
            if(oldsta.eq.newsta) then
               found=.true.
            endif
         enddo
         if(.not.found) then
            k=k+1
            station(k)=data(i)(2:6)
         endif
      enddo

c   open output files
c
      open(2,file='hypo71.input',status='unknown')

      write(2,'(a4)') 'HEAD'

c
c   read to end of seisan station file and fish out relevant info
c
c
c   reset test parameters if neccesary
c
        stat='xxxxx'
        j=-1
        do while (stat.ne. '     '.and.j.ne.0)
           read(1,'(a80)')testdat
           if(testdat(14:14).eq.')')then
              read(testdat,'(a5,t12,i2,t16,f9.4)')stat,j,testj
           elseif(testdat(13:13).eq.')')then
              read(testdat,'(a5,t12,i1,t15,f9.4)')stat,j,testj
           else
              read(testdat,'(a5)')stat
              j=-1
           endif
           if(j.gt.0)then
              test(j)=testj
           endif
        enddo
        
c
c set step length damping control to less than 100 in case hypocenter default value is being used
c
      if(test(2).gt.100.0) test(2)=100.0
c
c set critical F value to 0.5
c
        test(3)=0.5

      if(test(3).lt.2.0) test(6)=1.0
c
c set Mc parameters a,b,c to defaults if not already set.
c
      if(test(7).eq.0.0) test(7)=-0.87
      if(test(8).eq.0.0) test(8)=2.0
      if(test(9).eq.0.0) test(9)=0.0035
c
c set test(57) parameter to default value if required
c
      if(test(57).eq.0.0) test(57)=1500.

c
c set test(71) parameter to default value if required
c this enable distance sorting of output
c
      if(test(71).eq.0.0) test(71)=1.0

c
c set Ml parameters a,b,c,d to defaults (Hutton and Boore, 1987)
c if not already set.
c
      if(test(75).eq.0.0) test(75)=1.0
      if(test(76).eq.0.0) test(76)=1.11
      if(test(77).eq.0.0) test(77)=0.00189
      if(test(78).eq.0.0) test(78)=-2.09

      do j=1,13
         if(test(j).ne.0.0) then
            write(2,'(a11,i2.2,a2,f10.4)') 'RESET TEST(',j,')=', test(j)
         endif
      enddo

      write(2,*)
c
c   station coordinates
c
      read(1,'(a)') line
 20   continue     


c correction for offset if required bjb 
      if(line(2:2).ne.' ') then
         read(line,'(1x,a5,i2,f5.2,a1,i3,f5.2,a1,i4,f6.2,f5.2)')
     1        stat_name,lat,mlat,dlat,lon,mlon,dlon,elev,del,mcor
         if(offset) then
            dec_lon=real(lon)+(mlon/60.0)
            if(dlon.eq.'W') then
               dec_lon=-1.0*dec_lon+hypo71_offset
               dlon='E'
            else
               dec_lon=dec_lon+hypo71_offset
            endif
            lon = int(dec_lon)
            mlon = (dec_lon - real(lon)) * 60.0
         endif
         do i=1,nphase
         if(line(2:6).eq.station(i)(1:5)) then
            write(2,'(1x,a5,i2,f5.2,a1,i3,f5.2,a1,i4,f6.2,f5.2)')
     *           stat_name,lat,mlat,dlat,lon,mlon,dlon,elev,del,mcor
         endif
         enddo
      else
         read(line,'(2x,a4,i2,f5.2,a1,i3,f5.2,a1,i4,f6.2,f5.2)')
     1        stat_name,lat,mlat,dlat,lon,mlon,dlon,elev,del,mcor
         if(offset) then
            dec_lon=real(lon)+(mlon/60.0)
            if(dlon.eq.'W') then
               dec_lon=-1.0*dec_lon+hypo71_offset
               dlon='E'
            else
               dec_lon=dec_lon+hypo71_offset
            endif
            lon = int(dec_lon)
            mlon = (dec_lon - real(lon)) * 60.0
         endif
         do i=1,nphase
         if(line(3:6).eq.station(i)(1:4)) then
            write(2,'(2x,a4,i2,f5.2,a1,i3,f5.2,a1,i4,f6.2,f5.2)')
     *           stat_name,lat,mlat,dlat,lon,mlon,dlon,elev,del,mcor
         endif
         enddo
      endif

      read(1,'(a)')line
      if(line(1:10).ne.'          ') goto 20
c
c   model
c
      i=0
      write(2,*)
      read(1,'(a)') line
 30   continue
      i=i+1
      read(line,'(2f8.3)') vel(i),z(i)
      write(2,'(2f7.3)') vel(i),z(i)
      read(1,'(a)') line
      if(line(1:10).ne.'          ') goto 30
      nlayers=i
      write(2,*)

c
c  vp/vs, start depth and distance weight
c
      read(1,'(3f5.0,f5.2)') startz,xnear,xfar,vpvs
c
c Check for flags for starting location and fixing location
c
      if(data(1)(44:44).eq.'S') then
         read(data(1)(39:43),'(f5.1)') startz
         trial_dep=.true.
      else if(data(1)(44:44).eq.'F') then
         read(data(1)(39:43),'(f5.1)') startz
         fix_dep=.true.
      endif
      if(data(1)(45:45).eq.'S') then
         read(data(1)(24:30),'(f7.3)') startlat
         read(data(1)(31:38),'(f8.3)') startlon
         startlon=abs(startlon)
         trial_epi=.true.
      else if(data(1)(45:45).eq.'F') then
         read(data(1)(24:30),'(f7.3)') startlat
         read(data(1)(31:38),'(f8.3)') startlon
         startlon=abs(startlon)
         fix_epi=.true.
      endif

      if(trial_epi) then
         lat1=int(startlat)
         lat2=(startlat-real(lat1))*60.0
         lon1=int(startlon)
         lon2=(startlon-real(lon1))*60.0
         write(2,'(f5.0,f5.0,f5.0,f5.2,4x,a1,4x,a1,
     *        24x,a1,1x,a4,2x,i2,1x,f5.2,1x,i3,1x,f5.2)') 
     *        startz,xnear,xfar,vpvs,'4','1','1','1111',
     *        lat1,lat2,lon1,lon2
      else
         write(2,'(f5.0,f5.0,f5.0,f5.2,4x,a1,4x,a1,24x,a1,1x,a4)') 
     *        startz,xnear,xfar,vpvs,'4','1','1','1111'
      endif
c
c read agency
c
      read(1,'(a3)') agency

c
c   convert to hypo71 and write out 
c
      call nor2hypo71(2,nrecord,nhead,data)
        

c
c   write instruction card
c
      if(fix_dep.and.fix_epi) then
         read(data(1)(14:15),'(i2)') min
         read(data(1)(17:20),'(f4.1)') sec
         lat1=int(startlat)
         lat2=(startlat-real(lat1))*60.0
         lon1=int(startlon)
         lon2=(startlon-real(lon1))*60.0
         write(2,'(17x,a2)')'19'
         write(2,'(f5.0,f5.2,i5,f5.2,i5,f5.2,f5.2)')
     *        real(min),sec,lat1,lat2,lon1,lon2,startz
      else if(fix_dep) then
         write(2,'(17x,a2,f5.2)')'11',startz
      else
         write(2,'(17x,a2)')'10'
      endif


      close(1)
      close(2)

      return
      end



      subroutine nor2hypo71(unit,nrecord,nhead,data)
	  
c
c   gets one event in nordic format in array data and writes it to unit
c   in hypo71 format
c
      implicit none
      character*80 data(*)
      character*80 pline(100),sline(100)
      integer nrecord,unit,nhead
      integer year,month,day,hour,min
      real sec
      integer i,j,k
      integer np,ns
      integer phour,pmin,shour,smin,psdiff
      real ssec

      logical pfound
c
c   read reference time, put sec to zero so all following times 
c   are referred to a whole minute
c
      sec=0.0
      read(data(1),'(1x,i4,1x,2i2,1x,2i2)') 
     *year,month,day,hour,min
      if(year.lt.2000) year=year-1900
      if(year.ge.2000) year=year-2000
      
c
c   read both p and s phases to end of event
c
      np=0
      ns=0
      do k=nhead+1,nrecord-1
         read(data(k)(19:28),'(i2,i2,1x,f5.1)') hour,min,sec

         if(data(k)(11:11).eq.'P'.or.data(k)(10:10).eq.'p') then
            np=np+1
            write(pline(np),
     *           '(a4,a2,2a1,1x,i2.2,i2.2,i2.2,i2.2,i2.2,f5.2)') 
     *           data(k)(2:5),data(k)(10:11),data(k)(17:17),
     *           data(k)(15:15),year,month,day,hour,min,sec
         endif

         if(data(k)(11:11).eq.'S'.or.data(k)(10:10).eq.'s') then
            ns=ns+1
            write(sline(ns),'(a4,5x,i2.2,i2.2,i2.2,i2.2,i2.2,f5.2,
     *           7x,f5.2,a2,a1,a1)') 
     *           data(k)(2:5),year,month,day,hour,min,sec,sec,
     *           data(k)(10:11),data(k)(17:17),data(k)(15:15)
         endif

      enddo

c
c for each p phase, check to see if an s phase exists
c if so then write p and s on same line to output
c if no matching s then just output the p phase
c
      do 100 i=1,np
         do 200 j=1,ns
            if(pline(i)(1:4).eq.sline(j)(1:4)) then
c check that the minute for both p and s match
c otherwise add 60 seconds to s second
               if(pline(i)(18:19).ne.sline(j)(18:19)) then
                  read(pline(i)(16:17),'(i2)') phour 
                  read(pline(i)(18:19),'(i2)') pmin 
                  read(sline(j)(16:17),'(i2)') shour
                  read(sline(j)(18:19),'(i2)') smin 
                  read(sline(j)(32:36),'(f5.2)') ssec 
                  psdiff=(shour*60.0+smin)-(phour*60.0+pmin)
                  ssec=ssec+(60.0*psdiff)
                  write(sline(j)(31:36),'(f6.2)') ssec 
               endif
               write(unit,'(a24,6x,a10)') 
     *              pline(i)(1:24),sline(j)(31:40)
               goto 100
            else
               goto 200
            endif            
 200     continue
         write(unit,'(a24)')pline(i)(1:24)
 100  continue


c
c check for s picks without a p pick at a given station
c
      do i=1,ns
         pfound=.false.
         do j=1,np
            if(pline(j)(1:4).eq.sline(i)(1:4)) then
               pfound=.true.
            endif
         enddo
         if (.not.pfound) then
            write(unit,'(a4,a5,a10,11x,a10)')
     *           sline(i)(1:4),'   4 ',sline(i)(10:19),sline(i)(31:40)
         endif
      enddo
         


      return
      end




C-----------------------------------------------------------------------
C Takes file HYPO71.output and updates the phase readings
C
      subroutine update_location(agency,error)

      implicit none

      logical error
c      logical bgs_seisan


      character n_or_s, w_or_e
      character*132 line
      character*80 data(2500)
      character q*1, sqd*3
      character*1 typ,exp
      character*4 station(100)
      character*2 phase(100)
      character*3 agency

      real sec
      real lat2, lon2, depth
      real* 8 dec_lat, dec_lon
      double precision grid_east,grid_north
      real rms,erh,erz
      real xnear,xfar,startz,vpvs
      real vel(100), z(100)
      real distance(100)
      real residual(100)
      real weight(100)
      real sdlat,sdlon,sdz
      
      integer azimuth(100)
      integer nrecord,nhead,nphase,nstat,id
      integer year,month,day,hour,min
      integer lat1, lon1
      integer no,dm,gap,m
      integer nlayers
      integer nused
      integer iter
      integer i,j,k
      character*10 extension

      real hypo71_offset
      logical offset
      
      common/readings/NSTAT,NPHASE,NHEAD,NRECORD,ID,DATA,TYP,EXP
      common/model/vel,z,nlayers,startz,xnear,xfar,vpvs
      common/hypo71/ hypo71_offset

      if (error) return

c
c call function to determine ig BGS seisan outputs are to be used
c bjb 2001/02/14
c
c      call get_bgs_seisan(bgs_seisan)
      call get_env_seisan_extension(extension)

c read environment variable continaing x offset to be applied to
c station coords of events near Greenwich Meridian
c      call get_hypo71_offset(coffset)
c      read(coffset,'(f3.0)')xoffset
      if(hypo71_offset.gt.0.0) then
         offset=.true.
         write(*,*) 'Correcting offset ',hypo71_offset
      else
         offset=.false.
      endif


      open (unit = 1, file = 'hypo71.output', status = 'old',
     1     err=91)
	
 1    read (1,'(a132)',end=92) line
      if ( line (1:9) .eq. '  I  ORIG') then
         do i=1,200
            read (1,'(a132)') line
            read (line(1:3),'(i3)') iter
           if (iter.eq.0) then
               goto 10
            endif
         enddo
c backspace twice to get back to last iteration line
 10      backspace(1)
         backspace(1)
         read (1,'(a132)') line
         read(line(96:113),'(3f6.2)') sdlat,sdlon,sdz
         goto 2
      else
         goto 1
      endif

 2    do i = 1 , 1000
         read (1,'(a132)',end=92) line
         if ( line (1:6) .eq. '  DATE') goto 3
      end do

 3    n_or_s = line (25:25)
      w_or_e = line (35:35)
      read (1, '(a132)') line
c
c read time string
c
      read(line(2:3), '(i2)') year
      if (year.gt.50) year=year+1900
      if (year.lt.50) year=year+2000
      read(line(4:5), '(i2)') month
      read(line(6:7), '(i2)') day
      read(line(9:10), '(i2)') hour
      read(line(11:12), '(i2)') min
      read(line(14:18), '(f5.2)') sec

c
c     convert to decimal lat and lon
c
      read (line (20:37),'(i2,1x,f5.2,1x,i3,1x,f5.2)',err=93)
     *     lat1, lat2, lon1, lon2

      dec_lat = lat1 + lat2/60.0
      dec_lon	= lon1 + lon2/60.0

      if(offset) then
         dec_lon=dec_lon-hypo71_offset
         if(dec_lon.lt.0.0) then
            w_or_e='W'
            dec_lon=abs(dec_lon)
         endif
      endif


      if (n_or_s .eq. 'S') dec_lat = -dec_lat
      if (w_or_e .eq. 'W') dec_lon = -dec_lon

      if(extension(1:3).eq.'BGS') then    ! lot 14-02-2002
        call latlon2ukgrid(error,dec_lat,dec_lon,grid_east,grid_north)
      endif

      read(line(38:44),'(f7.2)') depth


c
c read error params
c
      read(line(53:84),1001) no,dm,gap,m,rms,erh,erz,q,sqd
 1001 format(2(i2,1x),i3,1x,i1,f5.2,2f5.1,1x,a1,1x,a3)

      do i = 1 , 1000
         read (1,'(a132)',end=92) line
         if ( line (1:5) .eq. '  STN') goto 4
      end do

      nused=0
 4    do i = 1,1000
         read (1, '(a132)') line

         if(line(1:5).ne.'     ') then
            nused=nused+1
            read(line(2:15),'(a4,f6.1,1x,i3)') 
     1           station(nused),distance(nused),azimuth(nused)
            read(line(21:22),'(a2)') 
     1           phase(nused)
            read(line(55:65),'(f5.2,2x,f4.2)') 
     1              residual(nused),weight(nused)

            if(line(101:101).eq.'S') then
               nused=nused+1
               read(line(2:15),'(a4,f6.1,1x,i3)') 
     1              station(nused),distance(nused),azimuth(nused)
               read(line(100:101),'(a2)') 
     1              phase(nused)
               read(line(117:121),'(f5.2)') 
     1              residual(nused)
               read(line(124:127),'(f4.2)',err=11) 
     1              weight(nused)
               goto 12
 11            weight(nused)=0.0
 12            continue
            endif

         else
            goto 5
         endif
      enddo

 5    continue

c      do i=1,nused
c      write(*,*) i,station(i),distance(i), azimuth(i), 
c     1     residual(i), weight(i)
c      enddo
c
c write parameters to data line 1
c
      write(data(1)(2:10),'(i4,1x,i2.2,i2.2)') year,month,day
      write(data(1)(12:20),'(i2.2,i2.2,1x,f4.1)') hour,min,sec
      write(data(1)(24:43),'(f7.3,f8.3,f5.1)') dec_lat,dec_lon,depth
      write(data(1)(46:48),'(a3)') agency
      write(data(1)(49:51),'(i3)') no
      write(data(1)(52:55),'(f4.1)') rms

c
c add error line
c
c Error record should have been removed by this point
c but check anyway
      k=2                                          
      do while (data(k)(79:80).ne.'83'.and.k.le.nhead)
         k=k+1
      enddo         

c no error record: move data array below header down 1
      if(data(k)(79:80).ne.'83')then
         do i=2,nrecord
            data(nrecord+3-i)=data(nrecord+2-i)
         enddo
         nrecord=nrecord+1
         nhead=nhead+1                                

c write errors in new record 83
         k=2                               
         do i=1,78
            data(k)(i:i)=' '
         enddo 
         data(k)(79:80)='83'
      endif              
        
c write out errors into data(k)        
      if(extension(1:3).eq.'BGS') then
        write(data(k)(1:17),'(f8.3,1x,f8.3)') grid_east,grid_north   ! lot 14-02-2002
      else
        write(data(k)(2:14),'(a13)') 'HYPO71 Errors'   
      endif
      write(data(k)(19:19),'(a1)')q
      write(data(k)(21:23),'(a3)')sqd
      write(data(k)(25:27),'(i3)')no
      write(data(k)(28:30),'(i3)')dm
      write(data(k)(32:34),'(i3)')gap
      write(data(k)(36:36),'(i1)')m
      write(data(k)(38:41),'(f4.2)')rms
      write(data(k)(43:46),'(f4.1)')erh
      write(data(k)(48:51),'(f4.1)')erz


      if(extension(1:3).eq.'BGS') then

c
c add velocity model line
c
c Velocity model record should have been removed by this point
c but check anyway
         k=3
         do while (data(k)(79:80).ne.'93'.and.k.le.nhead)
            k=k+1
         enddo         
         
c no velocity model record: move data array below header down 1
         if(data(k)(79:80).ne.'93')then
            do i=3,nrecord
               data(nrecord+4-i)=data(nrecord+3-i)
            enddo
            nrecord=nrecord+1
            nhead=nhead+1                                

c write errors in new record 3         
            k=3
            do i=1,78
               data(k)(i:i)=' '
            enddo 
            data(k)(79:80)='93'
         endif              
        
         do i=1,nlayers
            j=1+(i-1)*12
            write(data(k)(j:j+11),'(f5.2,1x,f5.2,a1)')vel(i),z(i),'/'
         enddo

c
c add location parameter line
c
c Location parameter record should have been removed by this point
c but check anyway
         k=4
         do while (data(k)(79:80).ne.'A3'.and.k.le.nhead)
            k=k+1
         enddo         

c no location parameter record: move data array below header down 1
         if(data(k)(79:80).ne.'A3')then
            do i=4,nrecord
               data(nrecord+5-i)=data(nrecord+4-i)
            enddo
            nrecord=nrecord+1
            nhead=nhead+1                                

c write errors in new record A3
            k=4
            do i=1,78
               data(k)(i:i)=' '
            enddo 
            data(k)(79:80)='A3'
         endif              
         
         write(data(k)(3:5),'(f3.0)') startz
         write(data(k)(7:10),'(f4.0)') xnear
         write(data(k)(12:15),'(f4.0)') xfar
         write(data(k)(17:20),'(f4.2)') vpvs
         
      endif


c output azimuths and distances
      do 100 i=nhead+1,nrecord-1
         do 200 j=1,nused
            if(data(i)(2:5).eq.station(j).and.
     1           data(i)(10:11).eq.phase(j)) then
               write(data(i)(64:68),'(f5.1)') residual(j)
c               write(data(i)(69:70),'(i2)') 10*nint(weight(j))
               write(data(i)(71:75),'(i5)') nint(distance(j))
               write(data(i)(77:79),'(i3)') azimuth(j)
               goto 100
            else if(data(i)(2:5).eq.station(j).and.
c     1           data(i)(10:11).eq.'E ') then
     1         (data(i)(10:11).eq.'E '.or.data(i)(11:12).eq.'AM'))then
               write(data(i)(71:75),'(i5)') nint(distance(j))
               write(data(i)(77:79),'(i3)') azimuth(j)
               goto 100
            endif
 200     continue
 100  continue

      close (unit=1)
      return


C Error making output
      

 91   write(*,*) 'Hypo71.output does not exist'
      error= .true.
      return

 92   write (2,'(a)') ' No location obtained with this data'
      close (unit = 1)
      error= .true.
      return

 93   write (2,'(a)') ' Error reading location'
      close (unit = 1)
      error= .true.
      return

      end
C--------------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C Takes file HYPO71.output and produces a summary called HYPO71.BRIEF
C ie condenses the useful information of HYPO71.output into a smaller
C file, also with grid-references and magnitude

      subroutine make_hypo71_brief(data,nhead,nrecord,error)

      logical error

      character n_or_s, w_or_e
c      character ans
      character * 132 line1, line2
      character*5 stat
c      character * 4 stations(30) ! store station names from which
c                                 ! magnitudes were obtained
c      character * 4 mags( 30)    ! stores individual magnitudes
      CHARACTER*80 DATA(2500)                                                  

c      integer yea,yr,mon,mt,day,dy,hou,hr,min,mn ! time parameters
c      integer istat             ! number of channels
      integer count, nml, nmc
      integer lat1, lon1
c      integer counter           ! do loop counter
      integer year              ! JE for year 2K
      integer icoda
      integer idist
      integer nrec,nhead,nrecord

      real lat2, lon2
      real xmag, cmag
      real amp,period
      real * 8 dec_lat, dec_lon
      double precision grid_east, grid_north  ! lo 11/04/2003
      character*10 extension

      real hypo71_offset
      logical offset
      

      common/hypo71/ hypo71_offset

 
      if (error) return

      call get_env_seisan_extension(extension)

c read environment variable continaing x offset to be applied to
c station coords of events near Greenwich Meridian
c      call get_hypo71_offset(coffset)
c      read(coffset,'(f3.0)')xoffset
      if(hypo71_offset.gt.0.0) then
         offset=.true.
         write(*,*) 'Correcting offset ',hypo71_offset
      else
         offset=.false.
      endif


      nml=0
      nmc=0


c	call obtain_magnitude (error, cmag, ccoda_mag, num_readings,
c     * num_coda_readings, stations, mags,  readings_file)

      DO 100 NREC=NHEAD+1,NRECORD-1
         READ(DATA(NREC),'(1x,a5,23X,i4,g7.1,1X,F4.1,25X,I5)'
     *        ,err=1234)
     *        stat,icoda,AMP,PERIOD,IDIST
         goto 1235
 1234    continue
         write(6,*)' Something wrong with input record:'
         write(6,'(a)') data(nrec)
         amp=0.0
 1235    continue 

c
c   check if coda and distance available
c
         if(idist.gt.0.and.icoda.gt.0) then
            nmc=nmc+1
         endif
         
c     
c     check if amplitude is available, assume local magnitude if
c     1: phase is blank, AMP, L or S, period less than 5 secs 
c     and distance less than test(57)
c     
         if(amp.gt.0.0.and.period.lt.5.0.and.
     *        idist.gt.0.and.
     *        (data(nrec)(11:11).eq.' '.or.data(nrec)(11:11).
     *        eq.'L'.or.data(nrec)(11:12).eq.'S '
     *        .or.data(nrec)(11:12).eq.'Sg'
c     *        .or.data(nrec)(11:12).eq.'AM'
     *        .or.data(nrec)(11:13).eq.'AML'
     *        .or.data(nrec)(11:14).eq.'AMPL'
     *        .or.data(nrec)(11:12).eq.'SG')) then
            nml=nml+1
         endif

 100  continue

c
c   check if coda mag is available
c
      if(nmc.ne.0) then
         if(data(1)(60:60).eq.'C') then
            read(data(1)(56:63),'(f4.1,1x,a3)')cmag,agency
         endif
      endif
c
c     check if Ml magnitude is available, if  mag  in first pos. write
c     in second postion, else first.
c     
      if(nml.ne.0) then
         if(data(1)(60:60).eq.'L') then
            read(data(1)(56:63),202) xmag,agency
         else
            read(data(1)(64:71),202) xmag,agency
         endif
 202     format(f4.1,'L',a3)
      endif



      open (unit = 1, file = 'hypo71.output', status = 'old')
      open (unit = 2, file = 'hypo71.brief')
      
      do count = 1 , 1000
         read (1,'(a132)',end=99) line1
         if ( line1 (1:6) .eq. '  DATE') goto 1
      end do
      write (2,'(a)') ' No location obtained with this data'
      close (unit = 1)
      close (unit = 2)


 1    n_or_s = line1 (25:25)
      w_or_e = line1 (35:35)
      read (1, '(a132)') line2

      read(line2(2:3), '(i2)') year
      write (2,'(''Date        : '',a2,''/'',a2,''/'',i2.2)')
     *     line2 (6:7), line2 (4:5), year
      write (2,'(''Origin time : '',a2,'':'',a2,'':'',a5)')
     *     line2(9:10), line2(11:12),line2(14:18)
      
C     convert to decimal lat and lon
       
      read (line2 (20:37),'(i2,1x,f5.2,1x,i3,1x,f5.2)',err=44)
     *     lat1, lat2, lon1, lon2

      if (offset) then
         dec_lat = lat1 + lat2/60.0
         dec_lon	= (lon1 + lon2/60.0) - hypo71_offset


         if (dec_lon .lt. 0.0)then
            w_or_e = 'W'
            dec_lon = -dec_lon
            lon1 = dec_lon
            lon2 = (dec_lon - lon1) * 60.0
            dec_lon = -dec_lon
         else
            w_or_e = 'E'
            lon1 = dec_lon
            lon2 = (dec_lon - lon1) * 60.0
         end if
      else

         dec_lat = lat1 + lat2/60.0
         dec_lon	= lon1 + lon2/60.0
         if (n_or_s .eq. 'S') dec_lat = -dec_lat
         if (w_or_e .eq. 'W') dec_lon = -dec_lon
         
      end if

      write(2,'(''Epicentre   : '',a8,'' deg '',a1,1x,i3,''-'',f5.2,
     *     '' deg '',a1,''   ('',f9.4,'' / '',f9.4,'' )'')')
     *     line2(20:27),n_or_s,lon1,lon2,w_or_e,dec_lat, dec_lon
      
      if (extension(1:3).eq.'BGS') then
        call latlon2ukgrid(error,dec_lat,dec_lon,grid_east,grid_north)
        write(2,'(''Grid ref    : '',f9.3,'' East /'',f9.3,'' North'')')
     *     grid_east, grid_north
      endif

      if (line2(45:45) .eq. '*') then
c        write(2,'(''Depth       : '',a$,'' kms  (fixed)'')')
         write(2,'(''Depth       : '',a7,'' kms  (fixed)'')')
     *        line2(38:44)
         
      else
c        write(2,'(''Depth       : '',a$,'' kms'')') line2(38:44)
         write(2,'(''Depth       : '',a7,'' kms'')') line2(38:44)
      endif
      

      write(2,'(''Quality     : '',a)') line2 (80:84)
      
      write(2, *)' '
      write(2,'(''              '',a)') line1(53:79)
      write(2,'(''Statistics  : '',a)') line2(53:79)
      
      
      if (nml.gt. 0) then
         write(2, '(''Magnitude   : '',f4.1, '' ML     (from '',i2,
     *        '' readings)'')' ) xmag, nml

      else
         write(2, '(a)')'Magnitude   : No valid amplitude readings'
      end if

      if (num_coda_readings .gt. 0) then
         write(2, '(''            : '',f4.1, ''  Coda  (from '',i2,
     *        '' readings)'')' ) cmag, nmc
      else
         write(2, '(a)')'Magnitude   : No valid coda readings'
      end if


      read (1, '(a132)') line1
      write (2, *)' '
	
      read (1, '(a132)') line1
      write (2,'(a19,2x,a11,2x,a12)') 
     *     line1(1:19), line1(55:65), line1(116:127)
      
      do count = 1, 1000	
         read (1, '(a132)') line1
         if ((line1.eq. ' ') .or. (line1(3:7).eq.'*****')) goto 2
         write (2,'(a19,2x,a11,2x,a12)') 
     *        line1(1:19), line1(55:65), line1(116:127)
      end do
      
 2    write (2, *) ' '

 99   close (unit=1)
      close (unit=2)
C need to check if the file is empty..
      if(n_or_s.eq.' ') then    ! hypo71.brief is empty
         error = .true.
         return
      end if       
 
C now write in the individual readings in hypo71.mags
c      open (unit = 1, file = 'hypo71.mags')
c      do counter = 1, num_readings
c         write(1, '(27x, a4,2x,a1,1x,a4,1x,a2)')
c     *        stations(counter), ':' , mags(counter), 'ML'
c      end do    
    
      goto 199


C Error making output
44      error= .true.
        close(unit=2)

199     close (unit = 1)           
	return
	end
C--------------------------------------------------------------------------

C--------------------------------------------------------------------
        subroutine latlon2ukgrid(error, slat,slon,lgmee,lgmne)
        implicit  double precision(a-h,o-z)
        logical error
        real*8 slat,slon
        double precision lgmee,lgmne
c icnv is 1 for latlong to ng, 2 for viceversa
        icnv=1
        error = .false.

        f0=0.99960127d0
        b=6356256.9d0*f0
        conv=0.0000048481368d0
        phi1=0.85521133d0
        s=dsin(conv)
        a=6377563.4d0*f0
        esq=0.00667054d0
        if (icnv-1.GT.0) goto 2
    1   continue
        slat=slat*3600.00
        slon=slon*3600.00
        phi2=slat*conv
        pa=phi2-phi1
        pb=phi2+phi1
        xm=b*(1.0016767d0*pa-0.005028071d0*dsin(pa)*dcos(pb)+0.000005250d
     1		0*dsin(2*pa)*dcos(2*pb))
        gnu=a/dsqrt(1-esq*(dsin(phi2)**2))
        two=gnu*0.5*(s**2)*dsin(phi2)*dcos(phi2)*(10.**8)
        rho=gnu*(1-esq)/(1-esq*(dsin(phi2)**2))
        t=dsin(phi2)/dcos(phi2)
        etasq=gnu/rho-1
        three=(gnu/24.)*(s**4)*dsin(phi2)*(dcos(phi2)**3)*(5.-
     1		(dsin(phi2)/dcos(phi2))**2+9*etasq)*(10.**16)
        threa=(gnu/720.)*(s**6)*dsin(phi2)*(dcos(phi2)**5)*(61.-
     1		58.*t*t+t**4)*(10.**24)
        p=(slon+7200.)/10000.
        gmn=xm-100000.+two*p*p+three*p**4+threa*p**6
        four=gnu*s*dcos(phi2)*(10.**4)
        five=-(gnu/6.)*(s**3)*(dcos(phi2)**3)*(gnu/rho-t*t)
     1		*(10.**12)
        six=-(gnu/120.)*(s**5)*(dcos(phi2)**5)*(5.-18*t*t+t**4
     1		+14.*etasq-58.*t*t*etasq+2*(t**4)*etasq)*(10.**20)
        gme=400000.+p*four-five*p**3-six*p**5
        goto12
    2   continue
   12   continue
        gme=gme/1000.00
        slat=slat/3600.00
        slon=slon/3600.00
        gmn=gmn/1000.00
        lgmee=sngl(gme)
        lgmne=sngl(gmn)


        return
        end
