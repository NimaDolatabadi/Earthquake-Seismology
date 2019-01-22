c**********************************************************************
c This program generates a set of travel times using the one-dimensional
c velocity in the station0.hyp file 
c
c  Barry Lienert June 1998
c
c***********************************************************************
c ------------------- SEISAN 7.0 -------------------
c june 10, 99 lo: check for station file in DAT directory
c june 24     jh: cut out dx of argument list to dtdg
c sep  21  99 lo: improve output
c may 15 2000 jh: remove nn,parm from calls to dtdx...,add hypparm.inc.
c                 and change a bit to mak eit work, use real vs instead of
c                 vs/pos
c apr 21 2015 lo: added option to compute travel time for changing depth
c
      include 'hypparm.inc'
      character*80 infile,outfile  
      character*80 indat,testdat
      character*1 reff,ucase,prmd,prm2
      character*4 stat
      character*8 phsid
      real vpp(nlayer)
      logical exist
cx in comm block      parameter (nlayer=50)
cx    real v(nlayer),vs(nlayer),d(nlayer),test(200)
cx      real xh(3),x0(3),dx(3)
      character*60 top_directory
      character*1 dchar,choice
      real rearth
      integer seiclen


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      pi=3.141593
      degtorad=pi/180.
      rearth=6371.                         

c
c get directory structure
c

      call topdir(top_directory)

      call dir_char(dchar)         ! dirctory delimiter character

      
      write(*,*)' Program to generate a travel time table in'
      write(*,*)' a named file using a STATION0.HYP format '
      write(*,*)' velocity model'
      write(*,*)
      write(*,'('' Input Filename ?(RET for STATION0.HYP)'')')
      read(*,'(a40)')infile
      if(infile.eq.'                                        ')then
       infile='STATION0.HYP'
      endif 

c
c
c
c  TO BE ADDED IS SEARCH IN DAT DIRECTORY
c
c
      inquire(file=infile, exist=exist)
      if (.not.exist) then

         infile = top_directory(1:seiclen(top_directory)) // dchar // 
     &         'DAT' // dchar //
     &         infile(1:seiclen(infile))

         inquire(file=infile, exist=exist)
         if (.not.exist) then
           write(*,*) 'station file does not exist !'
           stop
         endif
      endif
      open(9,file=infile)
      outfile = 'ttlayer.out'
c      outfile='test'
      open(10,file=outfile)
      iustat=9         
      open(14,file='out')
c    set test parameter defaults
        call settest(test)

c    read test parameter changes and station data
c    changed 4/94 to read character string and check for ")" so reset test(1) is OK
        stat='xxxx'
        j=-1
        
        do while (stat.ne. '    '.and.j.ne.0)
         read(iustat,'(a80)')testdat
         if(testdat(14:14).eq.')')then
          read(testdat,'(a4,t12,i2,t16,f9.4)')stat,j,testj
         elseif(testdat(13:13).eq.')')then
          read(testdat,'(a4,t12,i1,t15,f9.4)')stat,j,testj
         else
          read(testdat,'(a4)')stat
          j=-1
         endif
         if(j.gt.0)then
          test(j)=testj
c          if(iulst.gt.0)write(iulst,'(1x,''Reset test('',i2,'')='',
c     &    f10.4)')j,testj
         endif
        end do

      indat='XXXXXXXX'
      do while (indat(1:8).ne.'        ')
       read(9,'(a80)')indat
c       write(*,'(a80)')indat
      end do

c read the velocity model
        i=1
        iustat=9
        
c    reff is used to specify the moho layer for PN calculation
5       read(iustat,101,end=99)v(i),d(i),vs(i),reff
101     format(3f7.3,a1)
        if(ucase(reff).eq.'N')nmoho=i

c 4/94: added nconrad variable
        if(ucase(reff).eq.'B')nconrad=i
        
        if(v(i).eq.0.0)go to 6
        i=i+1
        go to 5

c    nl is the number of layers in the velocity model
6       nl=i-1

      
c    read in trial depth and vp/vs ratio
        read(iustat,'(3f5.0,f5.2)',end=99)ztr,xnear,xfar,pos

c    if vs(1)=0 then set vs(i)=v(i)/pos
        if(vs(1).eq.0.0)then
          do i=1,nl
            vs(i)=v(i)/pos
          end do
        endif

c  store thicknesses in parm
        do i=1,nl-1
          parm(nl+i)=d(i+1)-d(i)

c    change 10/93: add maximum elevation to upper layer thickness
c    if test(40)=0.0 Need this because dtdx2 origin is always at maxelv
c          if(i.eq.1.and.test(40).eq.0.0)then
c            parm(nl+i)=d(i+1)-d(i)
c          endif
        end do
        do i=1,nl
          parm(i)=v(i)
        end do
c       nn=2*nl-1

      minflag=int(test(63))      
        
c    xs(1), xs(2) are the station long. and lat.
      x0(1)=0.0
      x0(2)=0.0
      x0(3)=0.0
      
      prmd=' '                        
      prm2=' '

      write(*,'(a)') 
     &' travel time for change in distance (1) or depth (2) '
      read(5,*) choice
      if (choice.eq.'1') then
        write(*,'('' Maximum delta in km:'')')
        read(*,*)delta_max
        write(*,'('' Delta increment in km:'')')
        read(*,*)delta_dist 
        write(*,'('' Hypocentral depth in km:'')')
        read(*,*)depth 
      else
        write(*,'('' Maximum depth in km:'')')
        read(*,*)delta_max
        write(*,'('' Depth increment in km:'')')
        read(*,*)delta_dist 
        write(*,'('' Delta in km:'')')
        read(*,*)depth 
      endif
      write(*,'('' Phase type (N, G, RET for minimum):'')')
      read(*,'(a1)')prmd
      iulst=10
      nd=100

      xh(2)=0.0
      dist=0.0
c in case of choice=2, depth and distance are swapped
      if (choice.eq.'1') then
        xh(3)=depth
      else
        xh(1)=depth/rearth
      endif
c
c write some info
c
      write(10,'(a)') 'TTLAYER OUTPUT FILE'
      write(10,'(a)') '----------------------------------------' //
     &    '----------------------------------------'
      write(10,'(a)') ' input '
      if (choice.eq.'1') then
        write(10,'(a,f6.1)') ' Maximum delta in km: ',delta_max
        write(10,'(a,f6.1)') ' Delta increment in km: ',delta_dist
        write(10,'(a,f6.1)') ' Hypocentral depth in km: ',depth
      else
        write(10,'(a,f6.1)') ' Maximum depth in km: ',delta_max
        write(10,'(a,f6.1)') ' Depth increment in km: ',delta_dist
        write(10,'(a,f6.1)') ' Delta in km: ',depth
      endif  
      write(10,'(a,a1)') ' Phase type: ',prmd
      write(10,'(a)') '----------------------------------------' //
     &    '----------------------------------------'
      write(10,'(a)') ' output (layer=layer model, grad=gradient model'
      if (choice.eq.'1') then
        write(10,'(a)') '    distance  tp (layer)   tp (grad)' //
     &      '  ts (layer)   ts (grad)'
      else
        write(10,'(a)') '    depth     tp (layer)   tp (grad)' //
     &      '  ts (layer)   ts (grad)'
      endif
      write(10,'(a)') '----------------------------------------' //
     &    '----------------------------------------'

      do while (dist.le.delta_max) 
       do i=1,nl
         parm(i)=v(i)
       end do
       if (choice.eq.'1') then
         xh(1)=dist/rearth
       else
         xh(3)=dist
       endif
       call dtdx2(xh,x0,prmd,nmoho,nconrad,iulst,tmin,
     &  dx,delta,ann,iflag,phsid)
       if(iflag.eq.0)tmin=0.0
       tpp=tmin
       call dtdxg(xh,x0,prmd,nmoho,nconrad,iulst,ttg,
c     &  dx,delta,ann,iflag,phsid)
     &  delta,ann,iflag,phsid)
       do i=1,nl
cx        parm(i)=v(i)/pos
          parm(i)=vs(i)
          vpp(i)=v(i)
       end do
       call dtdx2(xh,x0,prmd,nmoho,nconrad,iulst,tmin,
     &  dx,delta,ann,iflag,phsid)
       if(iflag.eq.0)tmin=0.0
       
       do i=1,nl
         v(i)=vs(i)
       enddo
       call dtdxg(xh,x0,prmd,nmoho,nconrad,iulst,ttsg,
c     &  dx,delta,ann,iflag,phsid)
     &  delta,ann,iflag,phsid)
c       write(10,'(5f12.3,i3)')dist,tpp,ttg,tmin,ttsg,iflag
       write(10,'(5f12.3)')dist,tpp,ttg,tmin,ttsg
       dist=dist+delta_dist
       do i=1,nl
         v(i)=vpp(i)
       enddo
      end do
      
99    continue
      close(9)
      close(10)

      write(*,'(a)') ' Output file: ttlayer.out'
      end
      
