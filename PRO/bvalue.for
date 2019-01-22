C
C   PROGRAM TO CALCUALTE AND PLOT b-VALUE USING ONE FILE
C   IN NORDIC OR NORDIC COMPACT FORMAT
C
C   JENS HAVSKOV, DEC 90
C
c --------------------------------------------
c
c   feb 92 by rna:  first sun version
c   aug 92 by jh:   clean up and bugs fixed
c   sep 8 92 by jh: xscaling, make sure only one plot file
c   nov 12        : bug when writing out scaling
c   jun 14 93     : dimention to 50000
c   jun 21        : move posiiton of text in plot
c   aug 23 93     : filenames to 80
c   oct 17 94     : w magnitudes, read(end=1  instead of end=3
c   nov 3, 94     : do not accept no mag type, use indata routine
c   nov 7         : allow no magnitude type
c   dec 8         : include seidim
C   dec 8         : Installed file & error handling (libsei)
c   dec 14        : *************  version 5.0 ****************
c   feb 7         : close postscript file
c   apr 4, 95, jh : bad bugs
c   april 18, 95--: put vertical scaling back in in postscript, why taken out ?
c   jun 95  jh    : use fixed slope
c   jun 13        : normalizing
c   nov 9 98 jh   : -----------------  version 7.0 check -------------------
c                   one_line to oneline 
c   mar 22 99 bmt : include winplot.inc 
c   sep 17 99 jh  : set get routnes had a problem with plotunit, 
c                   use simple open
c   jun 20 05 jh  : plt to eps
c   dec 27 10 jh  : gfortran on pc: remove winplot include, put in implicit none
c                   hctype and display_type removed, remove tsend
c   feb 22 11 jh  : size from color.def
c   nov 23 11 jh  : accept mb,mB,ms and mS
c   feb 17 14 jh  : read seisan.def   
c --------------------------------------------

      implicit none
      include 'seidim.inc'         ! dimensions
c
c  magnitude
      real          mag(50000),xmag
c  data
      character*80 data(max_data)
c  number of magnitudes each interval, cumulative and log of same
      integer nmag(100),cmag(100)
      real lnmag(100),lcmag(100)
c  magnitude step in summation
      real magstep
c  number magnitudes selected and used for b-value
      integer nm,nused
c  magnitude type selected
      character*1    magtype
c  a,b value and sd
      real a_value,b_value,b_sd,a_least,b_least
      real b_value_fixed    ! the fixed value to use
      real a_fixed,a_sdv    ! a with b fixed with sd
      real ax,bx            ! a and b-values plotted
      integer choice        ! choice of above
c  correlation coefficient and rms
      real cor,rms
c  input file name
      character*80 filename
c  magnitude range
      real m1,m2
c  magnitude from one event
      real mg(3)
c  magnitude type from one event
      character*1 mtyp(3)
c  number of values for least squares analysis
      integer kl
c  help variables for least squares,etc
      real x(100),y(100),x1,x2,x3
c  text
      character*80 text
c  answer
      character*80 answer
c  cvalues for indata, not used
      integer nstat,nphase,nhead,nrecord,id
      character*1 exp,evid
c  logical for file exist
      logical exist
      integer sei integer  ! a function
      integer year,month,day,hour    ! time and date
      real xyear,max_year,min_year   ! absolute years
c  file type indicator
      logical compact
      integer i,k 
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       integer  write01,                   ! Output unit 1.
     &          read01,                    ! Input unit1.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
C
C    ============= end of list ==========

c -----------------------------------------------------------------
       
      include 'version.inc'
      include 'seiplot.inc'

      call get_seisan_def

c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c ----------------------------------------------------------------

c
c
c-- window size in % of screen, set in color.def
c
      wsize=65
      call get_window_size
      if(size_bvalue.gt.0) wsize=size_bvalue ! from color.def
c
c  initialize year range
c
      max_year=0.0
      min_year=3000.0
c
c  graphic display not yet open
c
      disp_open=0

c  open input file
c
      write(6,*)
     *' Input file name, select.out or collect.out are defaults'
      read(5,'(a)') filename
      if(filename(1:1).eq.' ') then
         inquire(file='select.out', exist=exist)
         if(exist) then
            filename='select.out'
         else
            inquire(file='collect.out',exist=exist) 
            if(exist) then
              filename='collect.out'
            else
              write(6,*)' No default files available'
c             call sei close(close$+all$,read01, code)       
              stop
            endif
         endif
      endif
          call sei open( old$+warn$,       ! Open file & warn of erro.
     &                   ' ',              ! Prompt (n/a).
     &                   filename,         ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
          if (code .ne. e_ok$) go to 99
c
c   determine type of file
c
      call nortype(read01,compact)
      if(compact) write(6,*)' Input file compact'
c
c   open output file
c
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   'bvalue.out',     ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
c
c  magnitude type
c
 55   continue
      write(6,*)' Which magnitude type, C,L,b,B,W,s or S, ',
     *'return for no type'
      read(5,'(a)') magtype
      if(magtype.eq.'c') magtype='C'
      if(magtype.eq.'l') magtype='L'
c      if(magtype.eq.'b') magtype='B'
c      if(magtype.eq.'s') magtype='S'
      if(magtype.eq.'w') magtype='W'
      if(magtype.eq.' ') then
         write(6,*)' Sure you want no magnitude type(y/n)'
         read(5,'(a)') text
         if(text(1:1).ne.'y'.and.text(1:1).ne.'Y') goto 55
      endif
c
c  read magnitudes to end of file and select 
c  the right types, do not use zero mags
c
      nm=0
 1    continue
      if(compact) then
         read(read01,'(a)',end=10) data(1)
      else
         call indata
     *  (read01,nstat,nphase,nhead,nrecord,evid,exp,data,id)
        if(nrecord.eq.0) goto 10
      endif
      read(data(1),'(56x,3(f3.1,a1,4x))')(mg(i),mtyp(i),i=1,3)
 3    continue
      do 2 i=1,3
         if(magtype.eq.mtyp(i).and.mg(i).ne.0.0) then
           nm=nm+1
           mag(nm)=mg(i)
c
c   get time
c
           read(data(1),'(1x,i4,1x,2i2,1x,i2)') year,month,day,hour
           if(year.lt.100) year=year+1900   ! make sure century is put in
           xyear=year+(month-1)/12.0+(day-1)/365.0+(hour-1)/(24.0*365.0)
           if(xyear.gt.max_year) max_year=xyear
           if(xyear.lt.min_year) min_year=xyear
           goto 1
         endif
 2    continue
      goto 1
c
c end of read
c
 10   continue
      xyear=max_year-min_year
      write(6,*)' Number of events selected from file:   ',nm
      write(6,'(a,1x,f9.4)')'  Duration of catalog in years', xyear
      write(write01,*)' Number of events selected from file:   ',nm
      write(write01,'(a,1x,f8.4)')' Duration of catalog in years', xyear
      write(write01,*)' -------------------------------------------'
      write(write01,*)
      xyear=alog10(xyear)
c
c   if no events, stop
c
      if(nm.eq.0) then
         write(6,*) 'No event selected, STOP'
         stop
      endif
c------------------------------------------------------------------
c  back here for new choice of parameters, a new plot file is also made
c  deleting the previous one
c------------------------------------------------------------------
 50   continue
c
c  magnitude step in summation
c
      write(6,*)' Magnitude step 1.0, 0.5, 0.25, 0.2 or 0.1'
      read(5,*) magstep
c
c  magnitude range and fixed b-value
c
      write(6,*)' Magnitude range m1,m2 for b-value and fixed b-value'
      read(5,*) m1,m2,b_value_fixed

c
c  calculate b-value after max liklihood
c
      call b_max_like(nm,mag,m1,m2,b_value,b_sd,nused)
c
c
c   calculate cumulative numbers, assume max of 100 different
c   corresponding to magnitude 0-10 in 0.1 magstep intervals
c
      kl=0
c
c  blank arrays
c
      do 13 i=1,100
        nmag(i)=0
        cmag(i)=0
 13   continue
c
c  sum
c
      do 15 i=1,nm
         k=(mag(i)+0.01)/magstep+1
         nmag(k)=nmag(k)+1
 15   continue
c
c  cumulative sum
c
      cmag(100)=nmag(100)
      do 16 i=100,2,-1
         cmag(i-1)=cmag(i)+nmag(i-1)
         if(nmag(i-1).ne.0) then
            lnmag(i-1)=alog10(float(nmag(i-1)))
         else
            lnmag(i-1)=-1.0
         endif
         if(cmag(i-1).ne.0) then
            lcmag(i-1)=alog10(float(cmag(i-1)))
c
c   select for least squares
c
cold            xmag=(i-1)*magstep
            xmag=(i-2)*magstep
            if(xmag.ge.m1.and.xmag.le.m2) 
c            if(xmag.ge.m1.and.xmag.lt.m2) 
     *      then
               kl=kl+1
               x(kl)=xmag
cold               y(kl)=lcmag(i)
               y(kl)=lcmag(i-1)
c               write(*,*) ' x/y ',kl,x(kl),y(kl)
            endif
         else
cold            lcmag(i)=-1.0
            lcmag(i-1)=-1.0
         endif
 16   continue
c
c   calculate max likelihood a_value
c
      a_value=0.0
      k=0
      do 18 i=1,100
         xmag=i*magstep
         if(xmag.ge.m1.and.xmag.le.m2.and.cmag(i).ne.0) then
            a_value=a_value+(lcmag(i)+b_value*xmag)
            k=k+1
         endif
 18   continue
      a_value=a_value/k
c
c  make least squares a and b-values
c
      call lsqlin(kl,x,y,a_least,b_least,cor,rms)
      b_least=-b_least
c
c  make a-value when b is fixed
c
      do i=1,kl
         y(i)=y(i)+b_value_fixed*x(i)
      enddo
      call sdv(kl,y,a_fixed,a_sdv)
c
c output
c
      write(6,200)
      write(write01,200)
 200  format('     n    m1    m2 mxl a mxl b    sd',
     *' lsq a lsq b   cor   rms  bfix  afix    sd')
      write(6,201) nused,m1,m2,a_value,b_value,b_sd,a_least,
     *b_least,cor,rms,b_value_fixed,a_fixed,a_sdv
      write(write01,201) nused,m1,m2,a_value,b_value,b_sd,a_least,
     *b_least,cor,rms,b_value_fixed,a_fixed,a_sdv
 201  format(i6,f6.1,f6.1,10f6.2)
c
c   normalized a values print out, zero mag
c
      x1=0.0
      x2=0.0
      x3=0.0
      if(a_value.ne.0.0) x1=a_value-xyear
      if(a_least.ne.0.0) x2=a_least-xyear
      if(a_fixed.ne.0.0) x3=a_fixed-xyear
      write(6,205)x1,x2,x3
 205  format(' Normalized ',f12.2,6x,f12.2,18x,f12.2) 
      write(write01,205)x1,x2,x3

      if(x1.ne.0.0) x1=x1-m1*b_value
      if(x2.ne.0.0) x2=x2-m1*b_least
      if(x3.ne.0.0) x3=x3-m1*b_value_fixed
      write(6,206)x1,x2,x3
 206  format(' Normalized m1',f10.2,6x,f12.2,18x,f12.2)
      write(write01,206)x1,x2,x3
      x1=10**x1
      x2=10**x2
      x3=10**x3
      if(x1.eq.1.0) x1=0.0 
      if(x2.eq.1.0) x2=0.0 
      if(x3.eq.1.0) x3=0.0 
      write(6,207)x1,x2,x3
 207  format(' Norm. lin. m1',f10.1,6x,f12.1,18x,f12.1)
      write(write01,207)x1,x2,x3
c
c  print sum and cumulative sum, only print non zero values
c
      write(6,*)
      write(6,*)'   mag  nmag  cmag'
      write(write01,*)'   mag  nmag  cmag'
      do 17 i=1,100
         if(nmag(i).ne.0) then
            write(6,202) float(i-1)*magstep,nmag(i),cmag(i)
            write(write01,202) float(i-1)*magstep,nmag(i),cmag(i)
 202        format(1x,f6.1,i6,i6)
         endif
 17   continue
c
c  plot
c
      write(6,*)' Plot(y/n)'
      read(5,'(a1)') answer(1:1)
      if(answer(1:1).eq.'y'.or.answer(1:1).eq.'Y') then
c
c   chose bvalue to plot
c
         write(6,*)' Which b-value: 1: Least squares (default)'
         write(6,*)'                2: Fixed bvalue'
         write(6,*)'                3: Maximum likelihood'
         read(5,'(a)') text
         if(text(1:2).eq.'  ') then 
            choice=1
         else
            choice=sei integer(text,code)
         endif
         if(choice.eq.1) then
            ax=a_least
            bx=b_least
         endif
         if(choice.eq.2) then
            ax=a_fixed
            bx=b_value_fixed
         endif
         if(choice.eq.3) then
            ax=a_value
            bx=b_value
         endif
c
c  set defaults for output on tek screen and one hardcopy file
c
         plotoption=1
         plotunit=70
         open(70,file='bvalue.eps',status='unknown')
c
c   open plotter
c
         call open_display 
c
c set some postscipt scalings
c
         write(plotunit,*) ' 1.0 0.55 scale'
         call bval_tekplot(nmag,cmag,lnmag,lcmag,magstep,
     *   ax,bx,m1,m2,choice)
c
c   close postscript
c
         call close_post
c
c   close output plot file
c 
         close(plotunit)
c
c   redo if requested
c
      text(1:40)=' Run again with other parameters (y/n)  '
      call oneline(text,40,answer,20,200.0,50.0)
c
c   close display and back to alpha screen
c
         call clear_to_alpha
      else
         write(6,*)' Run again with other parameters (y/n) '
         read(5,'(a)') answer
      endif

      if(answer(1:1).eq.'y'.or.answer(1:1).eq.'Y') then
         goto 50
      endif
c
c   finished
c
      write(6,*)' Output file in bvalue.out'
      write(6,*)' Last plot in file bvalue.eps'
      goto 999
99    continue
      write(*,*) ' No such file!'
999   continue
      call sei close(close$+all$,read01, code)       ! Close all open files
      stop
      end
c
      subroutine b_max_like(nm,mag,m1,m2,b_value,b_sd,n)
c
c  calculate maximum likelihood method, after h. bungum
c
c  j. havskov dec 90
c
c
      implicit none
c  number of magnitudes
      integer nm
c  magnitudes
      real mag(*)
c  magnitude interval to use
      real m1,m2
c  a-value,b-value and sd
      real b_value,b_sd
c  help valiable
      real sum,mean,sqrsum,variance
c  number of events within magnitude limits
      integer n
c  counter
      integer i
c
c   find  sum
c
      sum=0.0
      n=0
      do 1 i=1,nm
c         write(6,*)'mag,m1,m2',mag(i),m1,m2
         if(mag(i).ge.m1.and.mag(i).le.m2) then
            sum=sum+mag(i)
            n=n+1
         endif
 1    continue
      if(n.ne.0) then
         mean=sum/n
      else
         b_value=0.0
         b_sd=0.0
         return
      endif
c
c  square sum
c
      sqrsum=0.0
      do 2 i=1,nm
         if(mag(i).ge.m1.and.mag(i).le.m2) then
            sqrsum=sqrsum+((mag(i)-mean)**2)
         endif
 2    continue
c
c  b-value
c
      variance=sqrsum/(n-1)
      b_sd=sqrt(variance)
      b_value=0.43429448/(mean-m1)
c
c  deviation after bullen and bolt
c
c      b_sd=2.30*(b_value**2)*b_sd
      return
      end
c###################################################################
c
      subroutine bval_tekplot(nmag,cmag,lnmag,lcmag,magstep,
     *a_value,b_value,m1,m2,choice)
c 
c   make a tektronics b-value plot
c
c   j. havskov, dec 90
c
c
c -------------------------
      implicit none
c   following variables, see main prog
      integer nmag(*),cmag(*)
      real lnmag(*),lcmag(*),magstep,a_value,b_value,m1,m2
c   text for plotting
      character*80 text
c   counters and help variables
      integer i,k,jmag
      integer mag
      integer choice
c   for xtek
      real iy, ix, x1, x2, y1, y2
c
      include 'seiplot.inc'            ! plotting parameters, colors etc
c
c   plot y-axis
c
      call xset_color(color_frame)
      ix=100
      iy=200
      k=1
      call xmovabs(200.0,200.0)
      do 1 i=1,4
        iy=iy+100
        call xdrwabs(200.0,iy)
        call xdrwabs(210.0,iy)
        call xmovabs(200.0,iy)
        write(text(1:4),'(i4)') k
        call xchars(text,4,130.0,iy-5)
        call xmovabs(200.0,iy)
        k=k*10
 1    continue
c
c   plot x-axis and data, but only as long as there is data
c
      ix=200
c
c   find how many values and scale x-axis
c
      do i=1,99
         if(cmag(i).eq.0) goto 22
      enddo
 22   continue	  
      k=500/i	  
      call xmovabs(ix,200.0)
      mag=0
      jmag=0
      do 2 i=1,99
         if(mag.eq.10) then
            jmag=jmag+1
            mag=0
            call xset_color(color_axis_not)
            write(text(1:1),'(i1)') jmag
            call xset_color(color_frame)
            call xchars(text(1:1),1,ix-10.0,170.0)
            call xmovabs(ix,200.0)
            call xdrwabs(ix,190.0)
            call xmovabs(ix,200.0)
         endif
c
c   plot number of events if gt 0
c
         call xset_color(color_bval_np)
         iy=300+lnmag(i)*100
         if(lnmag(i).ge.0.0) then 
            call xmovabs(ix-0.4*k,200.0)
            call xdrwabs(ix-0.4*k,iy)
            call xdrwabs(ix+0.4*k,iy)
            call xdrwabs(ix+0.4*k,200.0)
         endif			
c
c   plot cumulative number of events
c
         call xset_color(color_bval_ac)
         iy=300+lcmag(i)*100
         call xmovabs(ix-5,iy)
         call xdrwabs(ix+5,iy)
         call xmovabs(ix,iy-5)
         call xdrwabs(ix,iy+5)
         call xmovabs(ix,200.0)
c
c   check if there is more data
c
         if(cmag(i+1).eq.0) goto 3
         ix=ix+k
         mag=mag+magstep*10
         call xdrwabs(ix,200.0)
 2    continue
 3    continue
      call xdrwabs(ix+2*k,200.0)
c
c   plot regression line
c
      call xset_color(color_bval_line)
      x1=k*m1/magstep+200
      x2=k*m2/magstep+200
      y1=(a_value-b_value*m1)*100+300
      y2=(a_value-b_value*m2)*100+300
      call xmovabs(x1,y1)
      call xdrwabs(x2,y2)
c
c   plot b value
c
      call xset_color(color_title)
      if(choice.eq.1) text(1:34)='Least squares a and b-values:     '
      if(choice.eq.2) text(1:34)='Fixed b-value a and b-values:     '
      if(choice.eq.3) text(1:34)='Maximum likelihood and b-values:  '
      write(text(35:50),250) a_value,b_value
 250  format(2f8.2)
      call xchars(text,50,250.0,650.0)
      call xset_color(color_def)
      return
      end
c
