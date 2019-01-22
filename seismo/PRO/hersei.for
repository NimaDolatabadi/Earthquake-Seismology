c     Program to read a NOrdic file and write out
c     the parameters in a Herrmann file ready for hspec8 modelling
c
c apr 95 by jh : use only triangular pulse
c may 99 jh    : ------------------   veriosn 7.0 check ----------------
c jan 20 2009 jh : fix for pc, new syntsel
c feb 20 2011 jh : fix removal command pc, remove some test write out
c mar 24 2012 wcc: the command line argument "-l L" to rhfoc10 weas missing

c                5 char stat code
c
      include 'seidim.inc'
      parameter (nnstat=32)
      character*80 sfile,data(max_data)
      character*110 string
      character*1 TYPE,EXP
c--- fault plane solution
      real strike,dip,rake,azimuth(nnstat),backazi(nnstat)
c--- component orientation
      logical radial
c--- station information 
      real distance(nnstat)
c--- stations to model
      character*5 stat(nnstat)
      character*14 filename
c
      character*4 comp
      real thick(20),vp(20),vs(20),dens(20),qp(20),qs(20)
      real t0synt(nnstat)
c-- computer type
      logical pc,sun,linux
      

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   get computertype
c    
      call computer_type(sun,pc,linux)

c      open(1,file='transfer.idx',status='old')
c      read(1,'(a)')sfile
c      close(1)
      k = 0

      open(1,file='hyp.out',status='old')

      call indata(1,NSTAT,NPHAS,NHEAD,NRECORD,TYPE,EXP,DATA,ID)

      do i = 1, nhead
        if(data(i)(80:80).eq.'3'.and.data(i)(2:6).eq.'SYNT:') then
          if(data(i)(8:15) .eq. 'MODEL--:' )then 
            if(data(i)(16:25) .ne. '     THICK')then
              k = k + 1
              read(data(i)(16:79),'(6f10.3)')thick(k),vp(k),vs(k),
     +                          dens(k),qp(k),qs(k)
              if(qp(k) .eq. 0. )qp(k) = 500.
              if(qs(k) .eq. 0. )qs(k) = 500.
              qp(k) = 1./qp(k)
              qs(k) = 1./qs(k)
            endif
          endif
          if(data(i)(8:15) .eq. 'DT-Tsou:')then
            read(data(i)(26:79),'(f10.1)')t_source
          endif
          if(data(i)(8:15) .eq. 'NPOINTS:')then
            read(data(i)(16:79),'(I10)')npoints
          endif
          if(data(i)(8:15) .eq. 'ST-D-RK:')then
            read(data(i)(16:79),'(3f10.1)')strike,dip,rake
          endif
          if(data(i)(8:15) .eq. 'TIMES--:')then
            read(data(i)(16:79),'(3(10x,f10.1))')time,t0,tsynt
          endif
          if(data(i)(8:15) .eq. 'REDVELO:')then
            read(data(i)(16:79),'(f10.4)')vred
          endif
        endif
      enddo
c
c   find number of stations and stations to model
c
      nstat=0
      do i=1,nhead
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: STATION:')
     *   then
            do istat=1,nstat
              if(stat(istat).eq.data(i)(17:21)) goto 80
            enddo
c
c   station was not counted before
c
            nstat=nstat+1
            stat(nstat)=data(i)(17:21)
 80         continue
         endif
      enddo
c
c   get station data
c
       do istat=1,nstat
         do i=1,nhead
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3')
     *      then
               read(data(i)(36:45),'(f10.1)') distance(istat)
            endif
            if(stat(istat)(1:5).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'AZIMUTH:'.and.data(i)(80:80).eq.'3')
     *      then
               read(data(i)(36:45),'(f10.1)') azimuth(istat)
               read(data(i)(56:65),'(f10.1)') backazi(istat) 
            endif
         enddo
      enddo
c
c   read if Radial-Transverse or North-East components
c
      do i=1,nhead
         if(data(i)(2:15).eq.'SYNT: COMPON-:') then
            radial=.false.
            if  (data(i)(20:25).eq.'RADIAL')  radial=.true.
         endif
      enddo
c

      write(*,*) 'displacement=1, velocity=2, acceleration=3'
      read(*,*) ind

c     write(*,*) ' Possible Source time functions :'
c     write(*,*) ' 1 ==> Parabolic pulse (default)'
c     write(*,*) ' 2 ==> Triangular pulse'
c     write(*,*) ' 3 ==> Ohnaka pulse   '
c     write(*,*) ' 4 ==> Spike pulse'
c     read(*,*)ipulse
      ipulse=2
c     write(*,*)' Pulse duration multiplier (L*dt)(1=default)'
c     read(*,*)lpulse
      lpulse=t_source/(tsynt/float(npoints))
      if(lpulse.lt.1) lpulse=1
      write(6,*)' Pulse half time ', lpulse*(tsynt/float(npoints))
      if(ipulse .eq. 3)then
        write(*,*)' Enter corner frequency of Ohnaka pulse'
        read(*,*)freqc
      endif
c
c  loop on stations 
      open(20,status='scratch')
c
      do istat=1,nstat
        write(string(1:1),'(i1)') istat
        write(string(1:13),'(a)') 'rhwvinta.out'//string(1:1)
        write(filename,'(a)') string(1:13)
        string = ' '

        if(pc) then
           call systemc('del file10',10)
        else
           call systemc('\\rm file10',11)
        endif

        write (*,*) 'file10 removed' 
c
c
c---- Now run rhfoc10
c
        write(*,*)'*********************************'
        write(*,*)' rhfoc10 is now running....'
        write(*,*)' Input  file: ',filename
        write(*,*)' Output file: file10'
        write(*,*)'*********************************'
        string(1:11) = 'rhfoc10 -f '
        string(12:25) = filename
        if(ind .eq. 1)string(26:29)=' -D '
        if(ind .eq. 2)string(26:29)=' -V '
        if(ind .eq. 3)string(26:29)=' -A '
        if(ipulse .eq. 1)string(30:32)='-p '
        if(ipulse .eq. 2)string(30:32)='-t '
        if(ipulse .eq. 3)string(30:32)='-o '
        if(ipulse .eq. 4)string(30:32)='-i '
        write(string(33:40),'(a3,i5)') '-l ',lpulse! WCC added
        string(41:49) = ' > file10'! WCC modified

c-old        string(33:41) = ' > file10'
        if(ipulse .eq. 3 .and. freqc .ne. 0.)then
           write(string(33:40),'(a3,f5.1)')'-a ',freqc
c-old        string(41:49)=' > file10'
        endif

        write(*,*)string


        call systemc(string,49)

        string = ' '
        if(pc) then
          call systemc('del file3',9)
        else
          call systemc('\\rm file3',10)
        endif

        write(6,*)' file 3 removed'

c
c---- Now calculate 3C seismogram given the mechanism
c
        write(*,*)'*********************************'
        write(*,*)' mech is now running....'
        write(*,*)' Input  file: file10 '
        write(*,*)' Output file: file3 '
        write(*,*)'*********************************'
        string(1:5)='mech '
        if (radial)   then
          write(string(6:54),200)strike,dip,rake,
     *                azimuth(istat),backazi(istat)
200       format('-s ',f5.1,' -d ',f5.1,' -l ',f5.1,' -a ',f5.1,
     *        ' -b ',f5.1,' -r ')
        else
          write(string(6:54),201)strike,dip,rake,
     *                azimuth(istat),backazi(istat)
201       format('-s ',f5.1,' -d ',f5.1,' -l ',f5.1,' -a ',f5.1,
     *        ' -b ',f5.1)
        endif
        write(string(55:72),'(a)')' < file10 > file3'

        write(*,*)string


        call systemc(string,72)
        string = ' '
c        write(6,*) 'Read herrmann'

        call readherr(stat(istat),nsamp,window)

c        write(*,*)stat(istat),nsamp,window

c        write(*,*)data(1),nrecord,window,nsamp
c
      enddo
c
      distmin=distance(1)
      do istat=2,nstat
        distmin=amin1(distmin,distance(istat))
      enddo
      do istat=1,nstat
        if (vred.ne.0.)  then
          t0synt(istat) = t0+(distance(istat)-distmin)/vred
        else
          t0synt(istat) = t0
        endif
      enddo
      dtt=window/(nsamp-1)
cc      write (*,*) (t0synt(istat),istat=1,nstat)
c
      call syntsel(data,nrecord,nhead,t0,time,t0synt,dtt,nsamp,'SH ')

      close(20)
      close(1)
      end


C########################################################################### 
   

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Routine to  read output data from Herrmann's program to

      subroutine readherr(stat,nsamp,window)

      character*80 infile,outfile1,outfile2,outfile3
      character*80 card
      character*4 icom(3)
      character*5 stat
      character*4 comp
      integer nnt(2)
      integer year,month,day,hour,min,sec,msec
      real yr,hs
      integer gmt(5),doy
      real tti(2), ddt(2), xmom(2)
      real srate
      dimension y(4096)
      dimension a(4096),b(4096),c(4096)
      real rfreq(2),amp(2),phase(2)
      integer ntrip,nsamp
      real xlat,xlon,xelev

      logical append,incresp

c      write(*,*)' Which file to convert?'
c      read(*,'(a)')infile

      infile='file3'

      open(12,file=infile,status='old')

c
c---- First read header information
c
120   continue

c      write (*,*) 'lecture'
	read(12,13)r,yr,hs,nnt(1),tti(1),ddt(1),
     1		tau,dip,strike,slip,xmom(1),
     2            az,baz,iexpl,icom
   13 format(3e16.9,i10,e16.9/5e16.9/3e16.9,i10,3a4)
c      write (*,*) 'fin lecture'
    
      idist = int(r)
      srate = 1./ddt(1)
      nsamp = nnt(1)
      window = ddt(1)*nsamp

c      write(*,*)' srate and nsamp:',srate,nsamp
c
c---- Top of read and write 
c
      do 1000 i=1,nnt(1)
      y(i)=0.0
 1000 continue

      comp = 'S  '//icom(1)(2:2)

      read(12,11)(y(i),i=1,nnt(1))
   11 format(5e16.9) 
c     ymin = 10000.
c     do i = 1,nnt(1)
c       if(y(i).lt.ymin.and.y(i).ne.0.)factor = abs(2./y(i))
c     enddo
      factor=1.e10 
      do i = 1,nnt(1)
        a(i) = y(i) * factor
      enddo

      comp = 'S  '//icom(2)(2:2)
      do 2000 i=1,nnt(1)
       y(i)=0.0
 2000 continue
      read(12,11)(y(i),i=1,nnt(1))
      do i = 1,nnt(1)
        b(i) = y(i) * factor
      enddo

      comp = 'S  '//icom(3)(2:2)
      do 3000 i=1,nnt(1)
      y(i)=0.0
 3000 continue
      read(12,11)(y(i),i=1,nnt(1))
      do i = 1,nnt(1)
        c(i) = y(i) * factor
      enddo

      close (12)

      write(20,'(a5,a4)')stat,'   '//icom(1)(2:2)
      write(20,*)(a(i),i=1,nnt(1))
      write(20,'(a5,a4)')stat,'   '//icom(2)(2:2)
      write(20,*)(b(i),i=1,nnt(1))
      write(20,'(a5,a4)')stat,'   '//icom(3)(2:2)
      write(20,*)(c(i),i=1,nnt(1))

      end

