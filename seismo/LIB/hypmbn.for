c
c routines use for mbn computation, Lars Ottemoller, 12 Jan 2018
c


      subroutine read_mbn(input_unit)
      implicit none
      include 'mbn.inc'
      character*80 line
      integer seiclen,input_unit,i

c
c init
c lo moved to hypmain to also set if file not read
c      mbn_nstat=0
c      mbn_nsource=0
c      mbpnscale(1)=0.
c      mbsnscale(1)=9999.
      

c
c read input
c
10    continue

      read(input_unit,'(a)',end=20) line

      if (line(1:10).eq.'SCALE MBPN') then
        read(line(41:70),'(3f10.3)') mbpnscale(1),mbpnscale(2),
     &                               mbpnscale(3)  
      elseif (line(1:10).eq.'SCALE MBSN') then
        read(line(41:70),'(3f10.3)') mbsnscale(1),mbsnscale(2),
     &                               mbsnscale(3)  
      elseif (line(1:12).eq.'GROUP VEL PN') then
        read(line(41:60),'(2f10.3)') mbpn_gv(1),mbpn_gv(2)
      elseif (line(1:12).eq.'GROUP VEL SN') then
        read(line(41:60),'(2f10.3)') mbsn_gv(1),mbsn_gv(2)
      elseif (line(1:18).eq.'STATION CORRECTION'.and.
     &        line(21:25).ne.'     ') then
        mbn_nstat=mbn_nstat+1 
        mbn_stations(mbn_nstat)=line(21:25)
        read(line(41:60),'(2f10.3)') mbn_s(mbn_nstat,1),
     &        mbn_s(mbn_nstat,2)
      elseif (line(1:17).eq.'SOURCE CORRECTION'.and.
     &        line(21:21).ne.' ') then
        mbn_nsource=mbn_nsource+1 
        i=mbn_nsource
        mbn_source_name(mbn_nsource)=line(21:40)
        read(line(41:),'(4(f6.1,1x),f6.3,f6.3)') mbn_e(i,1),
     &        mbn_e(i,2),
     &        mbn_e(i,3),mbn_e(i,4),mbn_e(i,5),mbn_e(i,6)
c         write(*,*) ' debug lo ',i,mbn_e(i,5),mbn_e(i,6)
      endif
      goto 10

20    continue

      return
      end

      real function get_station_correction(station,i)
      implicit none
      include 'mbn.inc'
      character*5 station
      integer i,j
      real x  ! correction
      x=0.
      do j=1,mbn_nstat
        if (station.eq.mbn_stations(j)) then
          x=mbn_s(j,i)
c          write(*,*) ' debug ',j,mbn_stations(j),x
          goto 10
        endif
      enddo
10    continue
      get_station_correction=x
      return 
      end


      real function get_source_correction(lat,lon,i)
      implicit none
      include 'mbn.inc'
      character*5 station
      integer i,j
      real lat,lon
      real x  ! correction
      x=-999. ! indicate not in source region
      do j=1,mbn_nsource
        if (lon.ge.mbn_e(j,1).and.lon.le.mbn_e(j,2).and.
     &      lat.ge.mbn_e(j,3).and.lat.le.mbn_e(j,4)) then
          x=mbn_e(j,4+i)
          goto 10
        endif
      enddo
10    continue
      get_source_correction=x
      return
      end 

c standard deviation
      subroutine sd(x,n,y,z)
      implicit none
      real x(*),y,z,sum
      integer n,i 
      sum=0.
      do i=1,n
        sum=sum+(x(i)-y)**2
      enddo
      z=sqrt(sum/n)
      return
      end

c mean
      subroutine av(x,n,z)
      implicit none
      real x(*),z,sum
      integer n,i
      sum=0.
      do i=1,n
        sum=sum+x(i)
      enddo
      z=sum/float(n)
      return
      end


      real function compute_mbn(amp,period,lat,lon,hdist,
     &         vg,stat,s,e)
      implicit none
      include 'mbn.inc'
      real amp,period,lat,lon,hdist,vg,x
      real s,e                            ! station and event corrections
      real get_source_correction,get_station_correction
      character*5 stat
      x=-999
      s=0.
      e=0.
c      write(*,*) ' debug vg ',vg
c P case
      if (vg.le.mbpn_gv(2).and.vg.ge.mbpn_gv(1)) then  ! mbpn
c get station correction
        s=get_station_correction(stat,1)   ! 1 for pn
c get source area correction
        e=get_source_correction(lat,lon,1)
c compute magnitude
        x=alog10(amp)-mbpnscale(1)*alog10(mbpnscale(2)/hdist)
     &    +s+e+mbpnscale(3)

c S case
      elseif (vg.le.mbsn_gv(2).and.vg.ge.mbsn_gv(1)) then ! mbsn
c get station correction
        s=get_station_correction(stat,2)   ! 2 for sn
c get source area correction
        e=get_source_correction(lat,lon,2)
c compute magnitude
        x=alog10(amp)-mbsnscale(1)*alog10(mbsnscale(2)/hdist)
     &                  +s+e+mbsnscale(3)
      endif

      compute_mbn=x
      return
      end 
