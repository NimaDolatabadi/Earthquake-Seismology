c
c convert neic csv format to noridc, lo 25 March 2014
c
      program neiccsv2nor
      implicit none

      include 'seidim.inc'
      include 'rea.inc'

      integer cm(20) ! place of comma
      integer qm(20) ! place of "
      integer i,j,seiclen,x,y
      real z
      character*80 infile,outfile
      character*250 line
      character*22 timestring
      integer code,nevent
      character*80 data(1000)      
      character*77 locality
      logical all

      all=.true.
      nevent=0

c before 2016
c time,latitude,longitude,depth,mag,magType,nst,gap,dmin,rms,net,id,updated,place,type
c 2014-03-23T18:20:03.580Z,-19.7438,-70.8107,34.61,6,mb,,44,0.664,1.38,us,usc000nm96,2014-03-24T16:09:05.000Z,"87km NW of Iquique, Chile",earthquake
c
c after 2016
c time,latitude,longitude,depth,mag,magType,nst,gap,dmin,rms,net,id,updated,place,type,horizontalError,depthError,magError,magNst,status,locationSource,magSource

      write(6,*) 'Input file'
      read(5,'(a)') infile
      j=seiclen(infile)
      open(1,file=infile,status='old')
      open(2,file='neiccsv2nor.out',status='unknown')
10    continue
      read(1,'(a)') line
c      if (line(1:seiclen(line)).ne.'time,latitude,longitude,'//
c 84  
      if (line(1:84).ne.'time,latitude,longitude,'//
     & 'depth,mag,magType,nst,gap,dmin,rms,net,id,updated,place,type')
     & then
         write(*,*) ' wrong format '
         goto 999
      endif
20    continue
      read(1,'(a)',end=999) line
      if (seiclen(line).le.0) goto 999
      write(*,*) line(1:seiclen(line))
      nevent=nevent+1
      x=0
      y=0
      j=seiclen(line)
      do i=1,j
        if (line(i:i).eq.',') then
          x=x+1
          cm(x)=i
        endif 
        if (line(i:i).eq.'"') then
          y=y+1
          qm(y)=i
c          write(*,*) y,qm(y)
        endif 
      enddo
c
c clear hypocenter line
c
      call rea_hyp_clear(1)
      rea_ncomment=0
c
c read line
c
      timestring=line(1:cm(1)-2)
      read(timestring(1:4),'(i4)') hyp_year(1)
      read(timestring(6:7),'(i4)') hyp_month(1)
      read(timestring(9:10),'(i4)') hyp_day(1)
      read(timestring(12:13),'(i4)') hyp_hour(1)
      read(timestring(15:16),'(i4)') hyp_min(1)
      read(timestring(18:22),'(f5.2)') hyp_sec(1)
c      write(*,*)  hyp_year(1),hyp_month(1),hyp_day(1),
c     &    hyp_hour(1), hyp_min(1), hyp_sec(1)

      read(line(cm(1)+1:cm(2)-1),*) hyp_lat(1)
      read(line(cm(2)+1:cm(3)-1),*) hyp_lon(1)
      read(line(cm(3)+1:cm(4)-1),*) hyp_depth(1)
c      write(*,*) hyp_lat(1),hyp_lon(1),hyp_depth(1)

      read(line(cm(4)+1:cm(5)-1),*) hyp_mag(1,1)
      if(cm(6)-cm(5).gt.2)read(line(cm(5)+2:cm(6)-1),*)hyp_mag_type(1,1)
      hyp_mag_agency(1,1)='PDE'
      hyp_nstat(1)=0
      if (cm(8)-cm(7).ge.2) then
        read(line(cm(7)+1:cm(8)-1),*) z
        hyp_nstat(1)=int(z)
      endif
c      write(*,*) line(cm(9)+1:cm(10)-1)
      if(cm(10)-cm(9).gt.1) read(line(cm(9)+1:cm(10)-1),*) hyp_rms(1)
      rea_locality=' '
      read(line(qm(1)+1:qm(2)-1),'(a)') rea_locality

      rea_ncomment=0
      rea_nphase=0
      rea_nwav=0
      rea_id_line=' '
      hyp_dist_id(1)='D' 
      hyp_agency(1)='PDE'
      rea_nhyp=1
      call rea_event_out(2,all,data,code)

      goto 20
999   continue
      close(1)
      close(2)

      write(*,*) ' Number of events: ',nevent
      write(*,*) ' Output file: neiccsv2nor.out'

      stop
      end



