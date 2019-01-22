c     Program to read a NOrdic file and write out
c     the parameters in a Herrmann file ready for hspec8 modelling
c
c
c  may 99 by jh : --------------  version 7.0 check --------------------
c  jan 20 2009 jh: fix to pc
c
c
      parameter (nnstat=32)
c--- stations to model
      character*5 stat(nnstat)
      real distance (nnstat)
c
      character*80 sfile,data(200),string
      character*1 TYPE,EXP
      character*4 comp
      real thick(20),vp(20),vs(20),dens(20),qp(20),qs(20)
c-- computer type
      logical pc,sun,linux
c
c
      

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
              read(data(i)(16:79),'(6f10.0)')thick(k),vp(k),vs(k),
     +                          dens(k),qp(k),qs(k)
              if(qp(k) .eq. 0. )qp(k) = 500.
              if(qs(k) .eq. 0. )qs(k) = 500.
              qp(k) = 1./qp(k)
              qs(k) = 1./qs(k)
            endif
          endif
          if(data(i)(8:15) .eq. 'DEPTH--:')then
            read(data(i)(16:79),'(f10.0)')depth
          endif
          if(data(i)(8:15) .eq. 'NPOINTS:')then
            read(data(i)(16:79),'(i10)')npoints
          endif
          if(data(i)(8:15) .eq. 'TIMES--:')then
            read(data(i)(16:79),'(3(10x,f10.1))')window,t0,time
          endif
          if(data(i)(8:15) .eq. 'REDVELO:')then
            read(data(i)(16:79),'(f10.3)')vred 
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
         enddo
      enddo
      close(1)
c
c VM  look for largest epicentral distance (used to calculate the spectrum)
c              smallest                    (for origin times)
      distmax=0.
      do istat=1,nstat
        distmax=amax1(distmax,distance(istat))
      enddo
      distmin=distmax
      do istat=1,nstat
        distmin=amin1(distmin,distance(istat))
      enddo

      dt = time/npoints
      write (*,*) time,dt,npoints
c
      dummy = 0.
      lmax = 0
      vpmin = 100.
      vsmin = 100.
      vpmax = 0.
      vsmax = 0.
      thick(k) = 0.
      do i = 1,k
        dummy = dummy + thick(i)
        if(depth .lt. dummy .and. lmax .eq. 0)then
          lmax = i
          dph = dummy - depth
        endif
        if(depth .eq. dummy)then
          write(*,*)' Focal depth on layer interface'
          write(*,*)' Change parameter file'
          stop
        endif
        if(vp(i) .gt. vpmax)vpmax = vp(i)
        if(vs(i) .gt. vsmax)vsmax = vs(i)
        if(vp(i) .lt. vpmin)vpmin = vp(i)
        if(vs(i) .lt. vsmin)vsmin = vs(i)
      enddo 
      hvert = dummy
      if(lmax .eq. 0)stop' Focal depth not possible with this model'
      alpha = 3./(npoints*dt)
      n1 = 1
      n2 = int((npoints/2) + 1.001)
      df = 1./(npoints*dt)
      nyq2 = npoints + 1
      xleng = 3*distmax 
      dummy = vpmax*time
      if(xleng .lt. dummy)xleng = dummy
      xfac = 1.5
      fl = 0.
      fu = 1./(dt*2.)
      mmax = k
c
c---- Now write hspec8 parameter file
c
      write(*,*)' Writing parameter file for hspec8'
      open(11,file='hspec8.inp',status='unknown')

      if(pc) then
         call systemc('\\del response.ssp',19)
      else
         call systemc('\\rm response.ssp',18)
      endif

      write(11,'(a)')'response.ssp'
      write(11,'(a)')'new'
      write(11,'(a)')'   -1    0'
      write(11,'(5e15.7)')alpha,depth,fl,fu,dt
      write(11,'(3i10,e15.7,2i10)')npoints,n1,n2,df,nyq2,mmax
      j = 1
      jbdry = 0
      write(11,'(11i5)')j,j,j,j,j,j,j,j,j,j,jbdry
      do i = 1,k
        write(11,'(6e11.4)')thick(i),vp(i),vs(i),dens(i),qp(i),qs(i)
      enddo
      write(11,'(i10,6e11.4)')lmax,dph,vpmin,vpmax,vsmin,vsmax,hvert
      write(11,'(2e15.7)')xleng,xfac

      close(11)

      write(*,*)' The computation of earth respons will now start,'
      write(*,*)' The input parameter file is:  hspec8.inp'
      write(*,*)' A respons file  response.ssp  will be made'
      write(*,*)' You may wish to stop now and manually edit the'
      write(*,*)' input parameter file, or directly continue the'
      write(*,*)' calculation of the earth response.'
      write(*,*)' 1  ==> Stop now'
      write(*,*)' 2  ==> Continue with calculations'
      read(*,*) icont
      if(icont .eq. 1)stop
c
c---- Continue generating the response spectrum F(f,k)
c
      write(*,*)'*********************************'
      write(*,*)' hspec8 is now running....'
      write(*,*)' Input  file: hspec8.inp '
      write(*,*)' Output file: response.ssp'
      write(*,*)'*********************************'
      string='hspec8 < hspec8.inp'
      call systemc(string,19)
c
c--- Now start the integration along wavenumber to obtain F(f,r)
c
      do istat=1,nstat 
c
      if (vred.ne.0.)  then
        tshift = t0-distmin/vred
      else
        tshift = t0
      endif
      open(11,file='rhwvinta.inp',status='unknown')
      write(11,'(a)')'response.ssp'
      write(string(1:1),'(i1)') istat
      write(string(1:13),'(a)') 'rhwvinta.out'//string(1:1)
      write(11,'(a)')string(1:13)
      write(11,'(a)')'-1.0       -1.0       100.0      100.0'
      write(11,'(4f10.5)')distance(istat),tshift,vred
      write(11,'(a)')'    -1.0   0.0       0.0'
      close(11)
      write(*,*)'*********************************'
      write(*,*)' rhwvinta is now running....'
      write(*,*)' Input  file: rhwvinta.inp '
      write(*,*)' Output file: ',string(1:13)
      write(*,*)'*********************************'
      if(pc) then
         string(1:17)='del '//string(1:13)
         call systemc(string(1:17),17)
      else
         string(1:18)='\\rm '//string(1:13)
         call systemc(string(1:18),18)
      endif

      string = 'rhwvinta < rhwvinta.inp'
      call systemc(string,23)
c
      enddo
c
c
      write(*,*)'*********************************'
      write(*,*)' Finished'
      write(*,*)' Command to continue:      hersei'
      write(*,*)'*********************************'

      end


C########################################################################### 
   
