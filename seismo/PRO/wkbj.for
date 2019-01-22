c
c
c   make a WKBJ input file from hyp.out
c
c   (based on subroutine makebou)
c
c  updates:
c  dev 94   by jh: use system_c instead of system
c  jan 94        : check for zereo number of stations and zereo depth
c                  not free, 6 phases per line
c  may 95        : bug
c  april 99 by jh: ------------------- verison 7.0 check-----------------
c                  year to 4 digits, system_c to systemc, 5 char stat
c  may,13 99   lo: changed two format statements
c  jan 16 2009 jh: fixed problem of day shift, put in new syntsel, read
c                  with indata
c  feb 2  2011 jh: change y to Y for synt pahses
c
      include 'seidim.inc'
      parameter (nnrays=30,nnptst=40000,nnstat=32,nnint=20)
c--- the hyp file
      character*80 data(max_data)
c--- number of lines in hyp file
      integer ndata
c---- more parameters for s-file
      integer nphase,nhead,nrecord,id
      character*1 type,exp
c--- code for Moho definition 
      character*1 comoho(nnint)
c--- model parameters in input
      real thick(nnint),alp1(nnint),bet1(nnint),rho1(nnint)
c--- fault plane solution
      real strike,dip,rake,azimuth(nnstat)
c--- component orientation
      logical radial
c--- focal depth
      real sdepth
c--- stations to model
      character*5 stat(nnstat)
      real distance(nnstat),backazi(nnstat)
c--- codes for phases which can be synthesized with WKBJ
      character*4 catfas(24)
c--- codes for phases to be synthesized with WKBJ
      character*4 codfsi(nnrays)
      character*4 codfas(nnrays)
      character*3 codhed
      character*5 coiasp
c--- true if the receiver is at the free surface
      logical freesf
c--- index of the reflective interface for each phase
      integer imoho0(nnrays)
c--- indexes to make the phases
      integer idx(5,13),idx2(13)
      character*13 chain,chain2
c--- arrays for the Green functions and the seismograms
      real temp(nnptst),green(13,nnptst),sismo(3,nnptst)
c--- array for the origin time of the seismograms
      real t0synt(nnstat),dtt
c--- array for the source time function
      real ss(nnptst)
c--- origin times
      integer oyear,omonth,odoy,oday,ohour,omin
      real osec
      real header_day   ! day in header of iasp.out
      double precision otime,ptime
c--- counters and pointers and help variables
c      character*1 liquid
      character*2 cbid
      logical logic
c
      data pi /3.14159/
      data catfas/
     &'  Pg','  Sg','    ','    ',' PmP',' SmS',' SmP',' PmS',
     &'    ','    ','    ','    ','pPmP','pSmS','pSmP','pPmS',
     &'    ','    ','    ','    ','sPmP','sSmS','sSmP','sPmS'/


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   open output file for phase times
c
       open(16,file='iasp.out',status='unknown')
       close(16,status='delete')
       open(16,file='iasp.out',status='unknown')
c
c----------------------------  READING IN PARAMETRS  --------------
c
c   open and read hyp.out file, store in array 'data'
c
      open(1,file='hyp.out',status='old')
cw 10   continue
cw        read(1,'(a)',end=20)data(ndata)
cw        ndata=ndata+1
cw        goto 10
cw  20  continue
cw      ndata=ndata-1

         call indata
     *   (1,nstar,nphase,nhead,ndata,type,exp,data,id)
         close(1)

c
c  get model, first count number of layers, assume first line header
c
         kl=-1
         do i=1,ndata
            if(data(i)(2:15).eq.'SYNT: MODEL--:') then
               kl=kl+1
               if(kl.eq.0) khead=i
            endif
         enddo
         do i=1,kl
            iline=khead+i
            read(data(iline)(16:25),'(f10.1)') thick(i)
            read(data(iline)(26:35),'(f10.1)') alp1 (i)
            read(data(iline)(36:45),'(f10.1)') bet1 (i)
            read(data(iline)(46:55),'(f10.1)') rho1 (i)
            read(data(iline)(77:77),'(a1   )') comoho(i)  
         enddo
c
c   find number of stations and stations to model
c
      nstat=0
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.'SYNT: STATION:') 
     *   then
            do k=1,nstat
              if(stat(k).eq.data(i)(17:21)) goto 80
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
c   if number of stations are zero stop
c
      if(nstat.eq.0) then
         write(6,*)' No stations given for modelling'
         stop
      endif
c
c   get station data
c
       do istat=1,nstat
c
c   find distance for stations, either given by SYNT, or by hyp
c
         do i=1,ndata
            if(stat(istat).eq.data(i)(17:21).and.
     *      data(i)(28:35).eq.'DISTANC:'.and.data(i)(80:80).eq.'3') 
     *      then
               read(data(i)(36:45),'(f10.1)') distance(istat)
            endif
            if(stat(istat).eq.data(i)(17:21).and.
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
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: COMPON-:') then
            radial=.false.
            if  (data(i)(20:25).eq.'RADIAL')  radial=.true.
         endif
      enddo
c
c   find source depth 
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: DEPTH--:') then
            read(data(i)(16:25),'(f10.1)') sdepth
c
c   if depth zero, stop
c
         if(sdepth.eq.0) then
            write(6,*)' Depth zero not allowed'
            stop
         endif
         endif
      enddo
c
c   get strike slip and dip         
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: ST-D-RK:') then
            read(data(i),'(15x,3(f10.1))') strike,dip,rake
         endif
      enddo
c
c   total duration of the plot window (the plot starts at the smallest
c         initial time of the synthetics)
c   origin time for the seismogram at the smallest epicentral distance 
c   duration of the time window at each station              
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: TIMES--:') then
            read(data(i),'(15x,3(10x,f10.1))') time,t0,twind
         endif
       enddo
c
c   find out if the station is at the free surface 
c     (by specifying that the station is NOT at the free surface, you will
c      modify the amplitudes of the respective phases, but you will also
c      take away some of the "free surface head waves". This option has
c      therefore mainly be put here so that you may check on a "free surface
c      seismogram" which phases are "free surface head waves".
c
      freesf=.true.
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: STAT-AT:') then
            if (data(i)(16:25).eq.'NOT FREE  ') freesf=.false.
            if (data(i)(16:25).eq.' NOT FREE ') freesf=.false.
            if (data(i)(16:25).eq.'  NOT FREE') freesf=.false.
            if (data(i)(16:25).eq.'not free  ') freesf=.false.
            if (data(i)(16:25).eq.' not free ') freesf=.false.
            if (data(i)(16:25).eq.'  not free') freesf=.false.
         endif
      enddo
	  if(.not.freesf)write(6,*)' No free surface used ************'
c
c   phases to be synthesized with WKBJ
c
      nphas=0
      npline=6
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: PHASES-:') then
            read(data(i),'(15x,6(6x,a4))')
     &               (codfsi(j),j=nphas+1,nphas+npline)
            do j=nphas+1,nphas+npline
              if(codfsi(j).eq.'    ')  then
                nphas=j-1
                goto 190
              endif
            enddo
            nphas=nphas+npline
 190        continue
         endif
      enddo
      write(6,*)' Number of phases to model: ',nphas
c
c      sampling interval, half-duration of the source 
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: DT-Tsou:') then
            read(data(i),'(15x,2f10.3)') dtt,halfdur
         endif
      enddo
c
c   reduction velocity 
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: REDVELO:') then
            read(data(i),'(15x,f10.3)') redvel
         endif
      enddo
c
c----------------------------  WRITING OUT PARAMETERS  --------------
c
c
c   open WKBJ input file
c
      open(2,file='wkbj.inp',status='unknown')
c
c   looking for the Moho
c
      nmoho=0
      do i=1,kl
        if(comoho(i).eq.'N')  nmoho=i
      enddo
      if(nmoho.eq.0) stop 'Moho not defined'
C
c   find where the source is in the model 
c   find in which layer or on which real interface 
c   label the possibly dummy interface corresponding to the source
c
         y=0.0
         do i=1,kl-1
           y=y+thick(i)
c    if the source is inside a  layer
           if((y-sdepth).gt.0.001) then
              isrc=i
              iints=i+1
              goto 180
           endif
c    if the source is on a real interface
           if(abs(y-sdepth).le.0.001) then
              isrc=0
              iints=i+1
              goto 180
           endif
         enddo
c    if the source is under the last interface
         if(sdepth.gt.y) then
            isrc=kl
            iints=kl+1
            goto 180
         endif
         stop 'problem with source depth '
 180     continue
c
c   writing information on the model :
c
c   first write:
c   2 reading units in wkbj.f for model and phase data
c   code for interpolation law in layers (2=linear)
c   number of depth points in model
c   F for flat model, F for depth axis positive downwards, dummy radius
c
      inunit=2
      logic=.false.
      nl=2*(kl-1)+1
      if (isrc.ne.0) nl=nl+2
      write (2,1001) inunit,inunit,2,nl,logic,logic,6371.
c
c   writing the model with depth:
c
      dep=0.
      index=0
      do i=1,kl-1
        write (2,1002) dep, alp1(i),bet1(i),rho1(i)
        if (isrc.eq.i)  then
          write (2,1002) sdepth, alp1(i),bet1(i),rho1(i)
          write (2,1002) sdepth, alp1(i),bet1(i),rho1(i)
        endif
        dep= dep+thick(i)
        write (2,1002) dep, alp1(i),bet1(i),rho1(i)
      enddo
      write(2,1002) dep, alp1(kl),bet1(kl),rho1(kl)
      if (iints.gt.kl)  then
        write (2,1002) sdepth,alp1(kl),bet1(kl),rho1(kl)
        write (2,1002) sdepth,alp1(kl),bet1(kl),rho1(kl)
      endif
c
c  -------------- make the two input lines for each phase
c
      do ip=1,nphas
c
c   look if the phase is in the catalogue of phases, and associate indexes
c
        do j=1,24
          if (codfsi(ip).eq.catfas(j))  then
            codfas(ip)=codfsi(ip)
            jp=j
            imoho0(ip)=nmoho
            goto 210
          endif
        enddo
        do j=1,24
          if (codfsi(ip)(1:2).eq.catfas(j)(1:2).and.
     &        codfsi(ip)(4:4).eq.catfas(j)(4:4))  then
            codfas(ip)=catfas(j) 
            jp=j
            read  (codfsi(ip)(3:3),'(i1)') imoho0(ip)
            if (imoho0(ip).gt.(kl-1)) then
              write (*,'(a,i1,a,a)') ' no interface ',imoho0(ip),
     &         ' in this model; will skip ',codfsi(ip)
              codfsi(ip)='    '
              goto 299
            endif
c            add one because free surface = interface 1 in the routines
            imoho0(ip) = imoho0(ip)+1
            goto 210
          endif
        enddo
        write (*,'(1x,a,a)') codfsi(ip),
     *' does not exist in the catalogue'
        codfsi(ip)='    '
        go to 299
 210    continue
        jp2= mod((jp-1),2)+1
        jp8= mod((jp-1),8)+1
        jdep=int((jp-1)/8)
c
c   prepare code for the wave type at the source
c
        if (jp8.eq.1) ksou=5
        if (jp8.eq.2) ksou=6
        if (jp8.eq.5.or.jp8.eq.8) ksou=1
        if (jp8.eq.6.or.jp8.eq.7) ksou=2
        if (jdep.eq.1) ksou=5
        if (jdep.eq.2) ksou=6
c
c   prepare indexes for the reflective interface
c   index shifts if there is a dummy interface at the source 
c   indexes at the source and around the Moho for calculating the slownesses
c
        imoho=imoho0(ip)
        if (isrc.ne.0.and.isrc.lt.imoho) imoho=imoho+1
c
c         if the source is under the interface, PmP etc... impossible
        if (jp8.gt.4.and.iints.ge.imoho)  then
          write (*,'(1x,a,a)') codfsi(ip),' impossible for that source'
          codfsi(ip)='    '
          goto 299
        endif
c
        jsourc=2*(iints-1)
        if (ksou.le.2) jsourc=jsourc+1
        jmoho0=2*(imoho-1)
        jmoho1=jmoho0+1
c
c   prepare code for the 2 first interfaces if P or S wave at the receiver
c            and wavetype at the receiver
c
        k=1
        idx(k,1)=1
        if (jp2.eq.1)  then 
          krec=9
          if (.not.freesf) krec=1
          chain='10 0000000000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if (jp2.eq.2)  then 
          krec=10
          if (.not.freesf) krec=2
          chain='01 0000000000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
c
        if (iints.gt.2)  then
          k=k+1
          idx(k,1)=2
          if (jp2.eq.1)  then 
            chain='10 0100000000'
            read (chain,1020) (idx(k,j),j=2,13)
          endif
          if (jp2.eq.2)  then 
            chain='01 0000100000'
            read (chain,1020) (idx(k,j),j=2,13)
          endif
        endif
c
c   prepare code for the interface at the source
c
        k=k+1
        idx(k,1)=iints
        if (jp8.eq.1.or.jp8.eq.2)  then
          chain='00 0000000000'
          read (chain,1020) (idx(k,j),j=2,13)
          goto 239
        endif
        if (jp8.eq.5)  then
          chain='20 0100000000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if (jp8.eq.6)  then
          chain='02 0000100000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if (jp8.eq.7)              then
          chain='11 0100000000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if (jp8.eq.8)              then
          chain='11 0000100000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
c
c   prepare code for the interface below the source
c
        if (jp8.eq.5.and.(iints+1).ne.imoho)  then
          k=k+1
          idx(k,1)=iints+1
          chain='20 0200000000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if (jp8.eq.6.and.(iints+1).ne.imoho)  then
          k=k+1
          idx(k,1)=iints+1
          chain='02 0000200000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if ((jp8.eq.7.or.jp8.eq.8).and.(iints+1).ne.imoho)  then
          k=k+1
          idx(k,1)=iints+1
          chain='11 0100100000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
c
c   prepare code for the last interface
c
        k=k+1
        idx(k,1)=imoho
        if (jp8.eq.5)  then
          chain='00 1000000000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if (jp8.eq.6)  then
          chain='00 0001000000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
        if (jp8.eq.7.or.jp8.eq.8)  then
          chain='00 0000001000'
          read (chain,1020) (idx(k,j),j=2,13)
        endif
c
 239    continue
c
c    add elements on the first two lines in case of depth phases
c
        if (jdep.eq.0) goto 249
c
c  pS or sP type
        chain='11 0000000001'
        chain2='11 0100100000'
c  pP type - interface 1
        if (jdep.eq.1.and.(jp8.eq.3.or.jp8.eq.5.or.jp8.eq.8)) then
          chain='20 0010000000'
          chain2='20 0200000000'
        endif
c  sS type - interface 1
        if (jdep.eq.2.and.(jp8.eq.4.or.jp8.eq.6.or.jp8.eq.7)) then
          chain='02 0000010000'
          chain2='02 0000200000'
        endif
        read (chain,1020) (idx2(j),j=2,13)
        do j=2,13
          idx(1,j)=idx(1,j)+idx2(j)
        enddo
        read (chain2,1020) (idx2(j),j=2,13)
        if (iints.gt.2)  then 
          do j=2,13
            idx(2,j)=idx(2,j)+idx2(j)
          enddo
        endif
c
c   add a transmission on line iints in case of depth phases
c
        do j=2,k-1
          if(idx(j,1).eq.iints)  ij=j
        enddo
        if (jp8.eq.3.or.jp8.eq.5.or.jp8.eq.8) chain='00 0100000000'
        if (jp8.eq.4.or.jp8.eq.6.or.jp8.eq.7) chain='00 0000100000'
        read (chain,1020) (idx2(j),j=2,13)
        do j=2,13
          idx(ij,j)=idx(ij,j)+idx2(j)
        enddo
c
c   if it exists, supress line iints+1, and shifts in k the last interface
c            in case of depth phases
c
        if ((ij+1).ne.k) then
          do j=1,13
            idx(k-1,j) = idx(k,j)
          enddo
          k=k-1
        endif
c
 249    continue
c
c   write out the final code for the phase
c
        logic=.true.
        write(2,1005) logic,idx(k,1),1,ksou,krec,
     &                       ((idx(l,j),j=1,13),l=1,k)
c
c   calculate the slowness range for the phase
c
        npp=200
c.................. P
        if (jp8.eq.1)
     &   write(2,1004) npp,1,jsourc,0,0,1,jsourc,1,jsourc,0.,0.,0.
c.................. S
        if (jp8.eq.2) then
          if(alp1(1).le.bet1(iints-1)) then 
         write(2,1004) npp,1,jsourc,0,0,2,jsourc,2,jsourc,0.,0.,0.
         else
         write(2,1004) npp,1,jsourc,0,0,1,     1,2,jsourc,0.,0.,0.
        endif
       endif
c.................. PmP
        if (jp8.eq.5) 
     &   write(2,1004) npp,1,jsourc,0,0,1,jmoho1,1,jmoho0,0.,0.,0.
c.................. SmS
        if (jp8.eq.6)  then
          write(2,1004) npp,1,jsourc,0,0,1,jmoho1,1,jmoho0,0.,0.,0.
          write(2,1005) logic,idx(k,1),1,ksou,krec,
     &                       ((idx(l,j),j=1,13),l=1,k)
          ii1=2
          jj1=jmoho1
          ii2=2
          jj2=jmoho0
          if (jdep.eq.1)  then
            if (alp1(iints-1).gt.bet1(imoho0(ip)-1))  then
              ii2=1
              jj2=jsourc
              if (alp1(iints-1).gt.bet1(imoho0(ip)))  then
                ii1=1
                jj1=jsourc
              endif
            endif
          endif
          write(2,1004) npp,1,jsourc,1,jmoho0,ii1,jj1,ii2,jj2,0.,0.,0.
        endif
c.................. SmP
        if (jp8.eq.7)  then 
          write(2,1004) npp,1,jsourc,0,0,1,jmoho1,1,jmoho0,0.,0.,0.
        endif
c.................. PmS
        if (jp8.eq.8)   then
          write(2,1004) npp,1,jsourc,0,0,1,jmoho1,1,jmoho0,0.,0.,0.
        endif
c       
 299    continue
      enddo
      write(2,1005) logic,0
c
c   -------------- distances, origin time, sampling intervals etc.
c
c  Calculate the number of points per seismogram (max=nnptst)
c
      npts=int(twind/dtt) + 1
      if (npts.gt.nnptst) stop ' Not enough dimension in time'
C
C  Look for the smallest epicentral distance and calculate the starting times
C
      epi0=distance(1)
      do  i=2,nstat
        epi0=amin1(epi0,distance(i))
      enddo
      if (redvel.eq.0.)  then
      do  i=1,nstat
        t0synt(i) = t0
      enddo
      else
      do  i=1,nstat
        t0synt(i) = t0+(distance(i)-epi0)/redvel
      enddo
      endif
c
      write (2,1006) nstat,(distance(istat),istat=1,nstat)
      write (2,1007) (t0synt(istat),istat=1,nstat)
      write (2,1008) dtt,npts

      close (2)
 1001 format (3I2,I4,2L2,F10.2)
 1002 format (4F10.5)
 1006 format (i2,10f7.1)
 1007 format (10f7.2)
 1008 format (F8.2,I4)
 1004 format (3I4,3(I2,I4),3F10.5)
 1005 format (L1,I3,3I2,5(I2,12I1))
c removed $ after last a, lo
 1010 format (a,f5.1,a,i2,a)   
 1020 format (2i1,1x,10i1)
c
c----------------------------  EXECUTION OF THE WKBJ PROGRAM  -----

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
      call systemc ('wkbj_or',7)
      write (*,*) ' wkbj executed '
c
c   open input file with synthetics
c
      open(4,file='wkbj.out',status='old')
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c----------------------------  CALCULATING THE ARRIVAL TIMES  -----
c
      open (3,file='wkbj.tab',status='old')
c
c   read seism origin time
c
      read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')

     *oyear,omonth,oday,ohour,omin,osec
c
c   get seism origin time in secs
c
      call timsec(oyear,omonth,oday,ohour,omin,osec,otime)
c
c
c   write header in output file
c
      write(16,'(1x,i4,1x,2i2,11x,a1,57x,a1)')
     *oyear,omonth,oday,'L','1'
c
c   save header day
c
      header_day=oday

      do istat=1,nstat
        epi=distance(istat)
        write (*,1031) stat(istat),epi
        rewind(3)
        do iphas=1,nphas
          if (codfsi(iphas).eq.'    ') goto 398
c
c    prepare elements for head waves Pn, Sn, etc..
c
          pcrit=0.
          if (codfas(iphas)(2:4).eq.'PmP') then
            pcrit=1./alp1(imoho0(iphas))
            codhed='Pn '
            if (codfas(iphas).ne.codfsi(iphas)) 
     &                         codhed(3:3)=codfsi(iphas)(3:3)
          endif
          if (codfas(iphas)(2:4).eq.'SmS') then
            pcrit=1./bet1(imoho0(iphas))
            codhed='Sn '
            if (codfas(iphas).ne.codfsi(iphas)) 
     &                         codhed(3:3)=codfsi(iphas)(3:3)
          endif
c
          iray=0
 301      continue
          iray=iray+1
 302      continue
          read (3,'(a2)',end=399) cbid
          if (cbid.ne.'0P')  goto 302
          read(3,1030) pp2,time2,epi2
          do ip=2,npp
            pp1=pp2
            time1=time2
            epi1=epi2
            read(3,1030) pp2,time2,epi2
c
c    head wave
c
            if (pcrit.ne.0.)  then
              if(pp1.le.pcrit.and.pp2.ge.pcrit)  then
                tcrit=time1+(time2-time1)*(pcrit-pp1)/(pp2-pp1)
                xcrit= epi1+( epi2- epi1)*(pcrit-pp1)/(pp2-pp1)
                if (epi.ge.xcrit)   then
                  tt = tcrit + (epi-xcrit)*pcrit
                  write( *,1032) codfas(iphas)(1:1)//codhed,tt
                  ptime = otime + tt   ! phase arrival time
                  call sectim          ! phase arrival time in day m hr etc
     *            (ptime,oyear,odoy,omonth,oday,ohour,omin,osec)
c
c  check for 24 h shift
c
                  if(oday.ne.header_day) then
                     if(oday-header_day.eq.1.or.         ! next day
     *                  header_day-oday.ge.27)           ! next month
     *                  ohour=ohour+24
                  endif
                          
                  coiasp='Y'//codfas(iphas)(1:1)//codhed
                  if (codfas(iphas)(1:1).eq.' ') coiasp='Y'//codhed//' '
                  write(16,222)stat(istat),'  ',coiasp,
     +            ohour,omin,osec
222               format(1x,a5,a2,1x,a5,4x,i2,i2,1x,f5.2)
                endif
              endif
            endif
c
c    "normal" wave
c
            if ((epi1.le.epi.and.epi2.ge.epi).or.
     &          (epi2.le.epi.and.epi1.ge.epi))   then
              call interpo(epi1,pp1,time1,epi2,pp2,time2,epi,pp,tt)
              write ( *,1032) codfsi(iphas),tt
                  ptime = otime + tt   ! phase arrival time
                  call sectim          ! phase arrival time in day m hr etc
     *            (ptime,oyear,odoy,omonth,oday,ohour,omin,osec)
c
c  check for 24 h shift
c
                  if(oday.ne.header_day) then
                     if(oday-header_day.eq.1.or.         ! next day
     *                  header_day-oday.ge.27)           ! next month
     *                  ohour=ohour+24
                  endif
                         
                  coiasp='Y'//codfsi(iphas)
                  if (codfsi(iphas)(1:1).eq.' ') 
     &                      coiasp='Y'//codfsi(iphas)(2:4)//' '
                  if (codfsi(iphas)(1:2).eq.'  ') 
     &                      coiasp='Y'//codfsi(iphas)(3:4)//'  '
                  write(16,223)stat(istat),'  ',coiasp,
     +            ohour,omin,osec
223               format(1x,a5,a2,1x,a5,4x,i2,i2,1x,f5.2)
            endif
          enddo
          if(codfas(iphas)(2:4).eq.'SmS'.and.iray.eq.1)   goto 301
 398      continue
        enddo
 399    continue
      enddo
      close(3)
 1030 format(G13.5,2F15.4)
 1031 format(1x,a4,' (',f6.1,'km) arrival times: ')
 1032 format(30x,a4,'  ',f8.2)
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c----------------  COMBINING GREEN FUNCTIONS WITH SOURCE ELEMENTS  ------- 
c
c   making the time series for the source rate function
c    (a triangle with half-duration halfdur, sampled from its onset time)
c
      read (4,*) i,dd,k,tst,dt,nt
      rewind(4)
      if (halfdur.le.dt)   goto 410 
      ns0= int(halfdur/dt)
      do i=1,ns0+1
        ss(i)=(i-1)*dt/halfdur
      enddo
      do i=ns0+2,2*ns0+2
        ss0 = 2.-float(i-1)*dt/halfdur
        if (ss0.lt.0.)  goto 410
        nss=i
        ss(i)=ss0
      enddo
 410  continue
c
c  open scratch file for writing the seismograms 
c
      open(20,status='scratch')
c
      do istat=1,nstat
c
c   reading the Green functions for the current epicentral distance
c
        do igr=1,13
          read (4,*) i,dd,k,tst,dt,nt,smaxx
          if (dd.ne.distance(istat)) 
     &            stop 'problem in the ordering of the Green functions'
          if (k.ne.igr)  stop 'problem in reading the Green functions'
          read (4,*) (green(igr,it),it=1,nt)
        enddo
c
c   calculating the moment tensor in case of an explosion 
c        (coded with all angles =0. in strike-dip-rake)
c
        if (strike.eq.0..and.dip.eq.0..and.rake.eq.0.) then
           em11=1.
           em22=1.
           em33=1.
           em12=0.
           em23=0.
           em13=0.
        else
c
c   calculating the moment tensor for the current azimuth
c         see box 4.4 in Aki and Richards for formulae 
c         + sign change on m13 and m23 due to z-upwards here
c
          stra=(strike-azimuth(istat))*pi/180.
          dipa=dip*pi/180.
          raka=rake*pi/180.
          sind=sin(dipa)
          cosd=cos(dipa)
          sin2d=2.*sind*cosd
          cos2d=cosd**2-sind**2
          sinr=sin(raka)
          cosr=cos(raka)
          sins=sin(stra)
          coss=cos(stra)
          sin2s=2.*sins*coss
          cos2s=coss**2-sins**2
c
          em11=-sind*cosr*sin2s-sin2d*sinr*sins**2
          em12=sind*cosr*cos2s+.5*sin2d*sinr*sin2s
          em13=-(-cosd*cosr*coss-cos2d*sinr*sins)
          em22=sind*cosr*sin2s-sin2d*sinr*coss**2
          em23=-(-cosd*cosr*sins+cos2d*sinr*coss)
          em33=sin2d*sinr
c
        endif
c
c   combine moment tensor with Green functions
c
        do ic=1,3
c         write (8,1050) ic,dd,tst,dt,nt
          if (ic.eq.1) then
           do it=1,nt
            temp(it)=
     &         em33*green(4,it)+em11*green(5,it)+2.*em13*green(6,it)
           enddo
          endif
          if (ic.eq.2) then
           do it=1,nt
            temp(it)=
     &         em33*green(1,it)+em11*green(2,it)+2.*em13*green(3,it)
           enddo
          endif
          if (ic.eq.3) then
           do it=1,nt
            temp(it)= em12*green(11,it)+em23*green(12,it)
           enddo
          endif
c
c   convolve with the source time function
c
          amoment=1.0e12
          if (halfdur.le.dt)    then
            do it=1,nt
              sismo(ic,it)=temp(it)*amoment
            enddo
          else
            do it=1,nt
              sismo(ic,it)=0.
              do j=1,min0(it,nss)
                sismo(ic,it)=sismo(ic,it)+ss(j)*temp(it+1-j)*amoment
              enddo
            enddo
          endif

c         write (8,1051) (sismo(ic,it),it=1,nt)
        enddo
c
c   write data in output file, identify stations and channels
c
        write(20,'(a5,a4)')stat(istat),'   Z'
        write(20,*)(sismo(1,i),i=1,nt)
c
        if (radial)   then
c   write the radial and transverse components
          write(20,'(a5,a4)')stat(istat),'   R'
          write(20,*)(sismo(2,i),i=1,nt)
          write(20,'(a5,a4)')stat(istat),'   T'
          write(20,*)(sismo(3,i),i=1,nt)
        else
c   rotate to North-East components, and write down
c   (note component 1 used as a buffer for component 2)
          cosb=cos(backazi(istat)*3.14159/180.)
          sinb=sin(backazi(istat)*3.14159/180.)
          do i=1,nt
            sismo(1,i)=-cosb*sismo(2,i)+sinb*sismo(3,i)
            sismo(3,i)=-sinb*sismo(2,i)-cosb*sismo(3,i)
          enddo
          write(20,'(a5,a4)')stat(istat),'   N'
          write(20,*)(sismo(1,i),i=1,nt)
          write(20,'(a5,a4)')stat(istat),'   E'
          write(20,*)(sismo(3,i),i=1,nt)
        endif
c
      enddo
c
      window=time
      call syntsel(data,ndata,nhead,t0,window,t0synt,dtt,npts,'SW ')
c
 1050 format (i3,f8.2,f15.4,g13.5,i5)
 1051 format (5g13.5)
      close (1)
      stop
c     return
      end
c********************************************************************
      subroutine interpo(x1,p1,t1,x2,p2,t2,x,p,t)
c
c     interpolate in x the function t(x),
c     given the function and its derivative p(x)
c     at two points x1 and x2
c
      dx=x2-x1
      y1=p1*dx
      y2=p2*dx
c
      a =-2.*(t2-t1) +y1 +y2
      b = 3.*(t2-t1) -2.*y1 -y2
      c = y1
      d = t1
c
      xn=(x-x1)/(x2-x1)
      t = xn*(xn*(xn*a + b) +c) +d
      p = xn*(xn*3.*a/dx + 2.*b/dx) + c/dx
c
      return
      end
