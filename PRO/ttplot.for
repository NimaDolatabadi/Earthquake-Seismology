c--------------------------------------------------------------------------
c  plot travel times make by lo
c--------------------------------------------------------------------------c
c
c changes:
c
c dec 28 2010 jh: gfortran on pc: remove winplot, implicit none, remove hctype, check
c                 units and computertype
c feb 22 2010 jh: size from color.def
c feb 17 2014 jh: read seisan.def, commnented out line 128, seems not
c                 to be used
c 2015.06.03 pv : moved line 9 continue, due to compiler warning on GOTO
c 
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block
      include 'version.inc'
      include 'libsei.inc'

      include 'seiplot.inc'               ! seisan graphics
      character*1 cha(10)                 ! dummy for xy_plot

      character*1 make_plot

      character*80 text(1000)             ! general text
      character*80 txt(100)
      character*80 title                  ! title for plot
      character*80 answer                 ! input text
      character*30 xtext,ytext            ! axis title
      character*80 data(max_data)         ! s-file with data in text array
      character*80 infile                 ! input file
      character*10 phase(max_data)        ! phase name     
      character*5  stat(max_data)         ! stations with p
      real         time(max_data)         ! abs arrival time
      real         res(max_data)          ! travel time residual
      real         dist(max_data)         ! distance from earthquake
      real         timecalp(max_data),timecals(max_data)      
                                          ! calclualeted first times
      real         distcal(max_data)
      character*10 sel_phase(max_data)    ! ------------------------
      character*5  sel_stat(max_data)     ! same as data above for
      real         sel_time(max_data)     ! selected stations
      real         sel_res(max_data)      ! selected stations
      real         sel_dist(max_data)     ! ------------------------
      real corr, rms                      ! correlation coefficient and rms
      real xm,ym                          ! cursor input
      real xc,yc                          ! scaling factors
      real*8 ot                           ! origin time
      real mindist,maxdist,xdist          ! distance range
      real maxtime
      integer ndist                       ! number of distances to calculate phases

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer seiclen
      integer il                          ! line counter
      integer i,k,l,n,j                   ! counter
      real y                              ! y plot coordinate
      real dist1,dist2                    ! distance range

      dist1=0.
      dist2=99999.

      call get_seisan_def
c
c   start writing from line 1
c
c   check if input via argumants
c
      call get_arguments(n,infile)
c
 9    continue
      if(n.lt.1) then
c
c   get input file name, check if exist
c
         write(6,*) 'Give input file'
         read(5,'(a)') infile
      endif
c
c check that event local or regional
c 
      open(1,file=infile,status='old',err=10)
      call rea_event_in(1,all,data,code)
      close(1)
      if (hyp_dist_id(1).ne.'L'.and.hyp_dist_id(1).ne.'R') then
        write(*,*) ' ttplot only works for local and regional events '
        stop
      endif
c
c   set text for no write out for hyp for some operations
c
      txt(1)='hyp non interactive'
      write(6,*)
      write(6,*)' **** now locating with hyp as a preparation ***'
      write(6,*)
      call put_seisan_message(txt(1))
      call systemc('hyp',3)            ! locate
      open(1,file='hyp.out',status='old',err=10)

      goto 11
 10   continue
      write(6,*)' No such input file'
      goto 9
 11   continue

      all=.true.                  ! read all parameters
c
c   open output file
c
      open(2,file='ttplot.out',status='unknown')  
c
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so terminate program
c
      if(code.eq.1) goto 1000
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)
c
c   comment out following line, seems not to be used, jh feb 2014
c
c      write(text(il),'(a)') data(1)(1:79)
c
c compute abs origin time
c
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &   hyp_hour(1),hyp_min(1),hyp_sec(1),ot)
c      write(*,*) ' time ',hyp_year(1),hyp_month(1),hyp_day(1),
c     &   hyp_hour(1),hyp_min(1),hyp_sec(1),ot
c
c store phases
c
      mindist=9999.
      maxdist=0.
      maxtime=0.
      k=1
      do i=1,rea_nphase
        if (rea_phase(i)(1:2).ne.'AM'.and.
     *      rea_phase(i)(1:1).ne.'E'.and. rea_phase(i)(1:1).ne.'I') then
c travel time
         if (rea_dist(i).ne.-999.) then
           call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &       rea_hour(i),rea_min(i),rea_sec(i),rea_abs_time(i))
           time(k)=rea_abs_time(i)-ot
           stat(k)=rea_stat(i)
           dist(k)=rea_dist(i)
           res(k)=rea_res(i)
           phase(k)=rea_phase(i)
c
c find distance range
c
           if (dist(k).gt.maxdist) maxdist=dist(k)
           if (dist(k).lt.mindist) mindist=dist(k)
           if (time(k).gt.maxtime) maxtime=time(k)
           k=k+1
         endif
        endif
      enddo
      k=k-1
c
c write all phases to output
c
      do i=1,k
        write(2,'(a5,1x,a10,1x,f6.1,1x,f6.1,1x,f6.2)')
     &      stat(i),phase(i),dist(i),time(i),res(i)
      enddo

c
c calculate phases
c
      ndist=100
      call traveltimes(hyp_model(1),mindist,maxdist,ndist,hyp_depth(1),
     *   timecalp,timecals,distcal)
c
c   put into x and y variables for least squares
c
c      do i=2,k
c        x(i-1)=stat_y(i)/stat_x(i)
c        y(i-1)=time_p(i)/stat_x(i)
c      enddo
c      call lsqlin(k,x,y,py,px,corr,rms)


c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='ttplot.eps',status='unknown')
          plotunit=65
          plotoption=1
          wsize=60
          call get_window_size
          if(size_ttplot.gt.0) wsize=size_ttplot ! from color.def
c
c
c   open plotter
c
c
c   set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'

          xtext=' Distance-km '
          ytext=' Travel time-sec '

c
c   open display
c
        call open_display
50      continue
        call clear_display
c
c selection
c
       l=0
       do i=1,k  
         if (dist(i).ge.dist1.and.dist(i).le.dist2) then
           l=l+1
           sel_dist(l)=dist(i)
           sel_phase(l)=phase(i)
           sel_stat(l)=stat(i)
           sel_time(l)=time(i)
           sel_res(l)=res(i)
         endif
       enddo

c
c   plot points
c
        call xy_plot
     *    (1,l,sel_dist,sel_time,title,xtext,ytext,
     *    600.0,600.0,100.0,100.0,1,1,20.0,
     *    0,cha,i,dist,time)
c
c plot calculated times
c
        k=0
        do j=1,ndist
          if (distcal(j).le.maxdist.and.
     *        timecalp(j).le.maxtime) k=j
        enddo
        call xy_plot
     *    (1,k,distcal,timecalp,title,xtext,ytext,
     *    0.,600.0,100.0,100.0,2,1,20.0,
     *    0,cha,i,dist,time)
        k=0
        do j=1,ndist
          if (distcal(j).le.maxdist.and.
     *        timecals(j).le.maxtime) k=j
        enddo
        call xy_plot
     *    (1,k,distcal,timecals,title,xtext,ytext,
     *    0.,600.0,100.0,100.0,2,1,20.0,
     *    0,cha,i,dist,time)

c  plot line
c
c          call xy_plot_line(px,py,100.0,100.0)
c
c plot text
c
          txt(1)='Select station by'
          txt(2)='clicking near symbol'
          call xmessage(txt,2,25,720.0,650.0)
c
c help text
c
          txt(1)='Press   R for Replot   D for Distance selection'//
     &           '   Q for Quit '
          call xmessage(txt,1,seiclen(txt(1)),200.0,720.0)

c
c  call up cursxor so plots remains
c
 55       continue
          call xy_plot_input(100.0,100.0,cha(1),xm,ym,xc,yc)
c
c check if distance selection
c
          if (cha(1).eq.'d'.or.cha(1).eq.'D') then
            txt(1)='Give distance range (d1,d2) '
            call oneline(txt(1),seiclen(txt(1)),answer,20,150.,500.)
            call sei get values(2,answer,code)
            dist1=ARRAY$(1)
            dist2=ARRAY$(2)
            goto 50
          elseif (cha(1).eq.'r'.or.cha(1).eq.'R') then
            dist1=0.
            dist2=99999.
            goto 50
          endif
 
c
c   find corresponding station
c
          txt(1)='Too far from symbol,'
          txt(2)='try again'
          do i=3,10
            txt(i)=' '
          enddo
          j=2
          y=500.
          call xmessage(txt,10,40,720.0,y)
          do i=1,l
              if(abs(xm-sel_dist(i))*xc.lt.10.0.and.   ! distance in pixels
     *           abs(ym-sel_time(i))*yc.lt.8.0) then
                 txt(1)='STAT  PHASE      DIST   TIME    RES'
                 write(txt(j),
     *             '(a5,1x,a8,1x,f6.1,1x,f6.1,1x,f6.2)')
     *  sel_stat(i),sel_phase(i),sel_dist(i),sel_time(i),sel_res(i)
                 if (j.eq.2) j=1
                 call xmessage(txt,10,40,720.0,y)
                 txt(1)=' '
                 txt(2)=' '
                 y=y-20.
              endif
          enddo

          if(cha(1).ne.'q') goto 55   ! next input

c 
c   close postscript
c
          call close_post
c
c   close output plot file
c
          close(65)
c
c   close display and back to alpha screen
c
         write(6,*)
         write(6,*)' Plot file is ttplot.eps'
 30   call clear_to_alpha

c
c     end of file
c
 1000 continue
c
      write(6,*)            ! blank line
      write(6,*) ' Output file is ttplot.out'


      stop
      end


      subroutine xy_plot_input(x0,y0,c,x,y,xc,yc)
c
c  reads positions from the screen much like the main routine
c  all scaling values from routine xy_plot via common
c  x0,y0 are lower left hand corner of plot
c
c  x0,y0 : same input as main routine
c  x,y   : output in scaled units
c  c     : character pressed
c  xc    : xscale
c
       implicit none
       character*1 c
       integer ichar
       reaL xc,yc   ! same as x and yscale
       real x1,x,y1,y   ! help variables
       real xfirst,yfirst,x0,y0,xscale,yscale,ymin,ymax,xmin,xmax ! see xy_plot
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax
c
c   call up cursor
c

   1        continue
            call xscursr(ichar,x1,y1)
            if (char(ichar).eq.'#') goto 1
            c=char(ichar)
            x=(x1-x0)/xscale+xfirst
            y=(y1-y0)/yscale+yfirst
            xc=xscale
            yc=yscale
c
      return
      end

      subroutine traveltimes(model,mindist,maxdist,ndist,depth,
     *   timep,times,distance)

c**********************************************************************
c subroutine to calculate travel times for given model and distance range
c modified from program ttlayer
c***********************************************************************
     
      implicit none
      include 'hypparm.inc'
      character*1 model
      real mindist,maxdist,depth,delta_dist
      integer ndist
      real timep(*),times(*),distance(*)
      character*80 infile,outfile  
      character*80 indat,testdat
      character*1 reff,ucase,prmd,prm2
      character*4 stat
      character*8 phsid
      real vpp(nlayer)
      logical exist
      character*60 top_directory
      character*1 dchar
      real rearth
      integer seiclen
      real pi,degtorad,testj,xnear,xfar,dist,tmin,delta,tpp,ann
      integer i,j,nmoho,nconrad,iulst,nd,iflag,minflag,iustat

      pi=3.141593
      degtorad=pi/180.
      rearth=6371.                         

c
c get directory structure
c
      call topdir(top_directory)
      call dir_char(dchar)         ! dirctory delimiter character
      infile='STATION0.HYP'
      if (model.ne.' ') write(infile(8:8),'(a1)') model

c
c find station file
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
         endif
        end do

      indat='XXXXXXXX'
      do while (indat(1:8).ne.'        ')
       read(9,'(a80)')indat
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

      iulst=10
      nd=100

      xh(2)=0.0
      xh(3)=depth

c
c write some info
c
      delta_dist=(maxdist-mindist)/float(ndist)
      dist=mindist
      ndist=0    ! now used as counter
      do while (dist.le.maxdist) 
       ndist=ndist+1
       do i=1,nl
         parm(i)=v(i)
       end do
       xh(1)=dist/rearth
c
c p time
c
       call dtdx2(xh,x0,prmd,nmoho,nconrad,iulst,tmin,
     &  dx,delta,ann,iflag,phsid)
       if(iflag.eq.0)tmin=0.0
       tpp=tmin
       do i=1,nl
          parm(i)=vs(i)
          vpp(i)=v(i)
       end do
c
c s time
c
       call dtdx2(xh,x0,prmd,nmoho,nconrad,iulst,tmin,
     &  dx,delta,ann,iflag,phsid)
       if(iflag.eq.0)tmin=0.0
       
       do i=1,nl
         v(i)=vs(i)
       enddo
       timep(ndist)=tpp
       times(ndist)=tmin
       distance(ndist)=dist
       write(10,'(5f12.3)')dist,tpp,tmin
       dist=dist+delta_dist
       do i=1,nl
         v(i)=vpp(i)
       enddo
      end do
      
99    continue
      close(9)
      close(10)

      end
      

