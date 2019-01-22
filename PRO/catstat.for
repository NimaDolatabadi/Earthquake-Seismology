c     Program to list the yearly, monthly and daily
c     number of events. Input file is Nordic compact.
c
c     by Mario Villagran, March 1995
c     Updates
c     June 95 by jh   : Put on PC
c     jul 20 by jh, time of day statistics
c     feb 96          : fixing and graphics
c     jul 24 96       : fix error in time of day distribution
c     aug 10 98 jh    : fix so years without century in s-file are sorted ok
c     feb    99 jh    : ---------   version 7.0 check ---------------------
c
c     mar 26 bmt      :include winplot.inc
c     sep 30    jh    : edit text
c     april 25  2002  : gmt output
c     feb 25 2003 jh  : increase dimensions
c     jun 20 2005 jh  : plt to eps
c     oct 24 2006 jh  : but in time of day
c     dec 30 2010 jh  : remove winpot, put in implicit none, unit 16
c                       to 26, 17 to 27, remove hctype
c     feb 22 2011 jh  : size from color.def
c     jan 2  2012 jh  : fix time limits so daily statistice is ok, remove
c                       question on writing out daily statistics, always write out
c     feb 17 2014 jh  : read seisan.def   
c
c
      implicit none  
      include 'seiplot.inc'   ! seisan graphics
      include 'seidim.inc'
      include 'gmt_xy_par.inc'
      integer nrecord,nstat,nhead,id,nphas
      character*1 exp,type
      character*80 data(max_data)
      integer sei integer     ! function to get integer from a text string
      logical compact        ! true if compact file
      INTEGER YEA,MO,DA,hour
      character*8 start_time,end_time ! time interval for statistics
c      integer dayabs,dayabs1,dayabs2  ! days from year 0
      double precision dayabs,dayabs1,dayabs2  ! sec from year 1900
      integer year1,month1,day1,year2,month2,day2  ! time inerval
      integer nday            ! number of days in a month
      CHARACTER event(150000)*13,infile*30
      integer time_of_day(24)    ! to time of day disitribution
      integer i,j,k,m,kk,l,nev
      real xx                    ! help variable
      integer syr(20000),smo(20000,12),sda(20000,12,31)
      real x(150000),y(150000)              ! for plotting
      real x_gmt(150000), y_gmt(150000)     ! for gmt plotting
      integer n_gmt                       ! number of points for gmt plotting
      character*80 filename_gmt           ! file with gmt input
      character*1 cha(10)                 ! dummy for xy_plot
      character*80 text                   ! general text
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      character*5  make_plot              ! indicator for making plot
      character*1  daily                  ! is y, print out daily statisitcs
      integer      iplot                  ! type of plot
      real size     	                      ! half the column size
      include 'version.inc'
C-------------------------------


c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call get_seisan_def
c
c
      call gmt_xy_init     ! set default gmt parameters

      size=0.2
      do i=1,24
        time_of_day(i)=0
      enddo
c
      write(6,*)' Input file name ?'
      read(5,'(a)')infile
      open(unit=27,file=infile,status='old')
c
c  find which type of input file
c
      call nortype(27,compact)
      if(compact) write(6,*)' Input file is compact'

      open(unit=19,file='catyear.out',status='unknown')
      open(unit=20,file='catmonth.out',status='unknown')
      open(unit=21,file='catday.out',status='unknown')
      open(unit=22,file='cathour.out',status='unknown')
c
      write(6,'(a,$)')' Start time (ccyymmdd.eg.19550101) '
      read(5,'(a)') start_time
      write(6,'(a,$)')' End time                          '
      read(5,'(a)') end_time
      read(start_time,'(i4,2i2)') year1,month1,day1
      if(month1.eq.0) month1=1
      if(day1.eq.0) day1=1
      read(end_time,  '(i4,2i2)') year2,month2,day2
      if(month2.eq.0) month2=12
c
c   find number of days in month
c
      call month_day(year2,month2,nday)
      if(day2.eq.0) day2=nday
 
      write(6,'(a, i4, 1x,2i2,5x,i4,1x,2i2)') 
     *' Time limits used: ',year1,month1,day1,year2,month2,day2

c      write(6,'(a,$)')' Write out dayly statistics (y/n)'
c      read(5,'(a)') daily
      
c
c    calculate sec since year 1900
c
c      dayabs1=year1*365+(month1-1)*31+day1
c      dayabs2=year2*365+(month2-1)*31+day2
       call timsec(year1,month1,day1,0,0,0.0,dayabs1)
       call timsec(year2,month2,day2,24,59,59.99,dayabs2)
      
      nev=0
c
c   lop for reading events
c

 5    nev=nev+1

c
c   read one event or one header line
c
      if(compact) then
         read(27,'(a)',end=10) data(1)
      else
         call indata(27,nstat,nphas,nhead,nrecord,type,exp,data,id)
         if(nrecord.le.0) then
            goto 10   ! process data
         endif
      endif
      event(nev)=data(1)(1:13)
c
c   next event
c
      goto 5
c
c------------------------------------
c   read all events, start processing
c------------------------------------
c
 10   continue
      close(27)
      nev=nev-1
        do i=year1,year2
           write(6,*)'year',i
             do j=1,nev
               read(event(j),400)yea,mo,da,hour
c         write(*,*) event(j),hour
 400           format (1X,I4,1X,2I2,1x,i2)
               if(yea.lt.100) yea=yea+1900    ! missing century, 8-98
c               dayabs=yea*365+(mo-1)*31+da
                call timsec(yea,mo,da,0,0,0.1,dayabs)
               if(dayabs.ge.dayabs1.and.dayabs.le.dayabs2) then
c
c   get time of day distribution, but only for thje first time in loop
c
                  if(i.eq.year1) then
                     if(hour.gt.23.or.hour.lt.0) then
                          write(6,*)' wrong hour'
                     else
                        hour=hour+1
                        time_of_day(hour)=time_of_day(hour)+1
                     endif
                  endif
               endif

               if(yea.eq.i)then
               syr(i)=syr(i)+1    ! yearly statistics
               do k=1,12
                  if(dayabs.ge.dayabs1.and.dayabs.le.dayabs2.
     *            and.mo.eq.k)then
                     smo(i,k)=smo(i,k)+1  ! monthly statistics
                     do kk=1,31
                       if(dayabs.ge.dayabs1.and.dayabs.
     *                 le.dayabs2.and.da.eq.kk) then                    
                          sda(i,k,kk)=sda(i,k,kk)+1 ! daily statistics
c                          write(6,*) sda(i,k,kk)
                        endif
                     enddo
                  endif
               enddo
            endif
          enddo
        write(19,*)i,syr(i)  ! write yearly statistics
      enddo
c
c  write monthly distribution
c
      do i=year1,year2
        do k=1,12
c          dayabs=i*365+(k-1)*31+31   ! add 31 to get even to end of month
          call timsec(i,k,1,0,0,0.1,dayabs)
c          write(22,*) i,k, dayabs,dayabs1,dayabs2
          if(dayabs.ge.dayabs1.and.dayabs.le.dayabs2) 
     *    write(20,'(i4,1x,i2,i6)')i,k,smo(i,k)
        enddo

        do k=1,12
c
c   find number of days in month
c
           call month_day(i,k,nday)
c
c   write events/day distribution
c
c           if(daily.eq.'y'.or.daily.eq.'Y') then
           do kk=1,nday
c               write(6,*) nday
c              dayabs=i*365+(k-1)*31+kk
              call timsec(i,k,kk,0,0,0.1,dayabs)
              if(dayabs.ge.dayabs1.and.dayabs.le.dayabs2)
     *        write(21,'(i4,2(1x,i2),i6)')i,k,kk,sda(i,k,kk)
c            write(6,*)i,k,kk
           enddo
c           endif
        enddo
      enddo
c
c   write time of day distribution
c
      do i=1,24
         write(22,*) i,time_of_day(i)
      enddo
c
c
 1000    continue
         write(6,*) ' Plot yearly distribution      (1)'
         write(6,*) ' Plot monthly distribution     (2)'
         write(6,*) ' Plot daily distribution       (3)'
         write(6,*) ' Plot time of day distribution (4)'
         write(6,*) ' No plot, return'
         read(5,'(a)') make_plot
         if(make_plot.ne.'     ') then
           iplot=sei integer(make_plot,kk)    ! get number, kk is error code
           if(iplot.gt.4) then
              write(6,*)' Not a valid number'
              goto 1000     ! not a valid number    
           endif
        
           n_gmt=0
c
c   make yearly histogram 
c
           k=1
           size=0.3
           if(iplot.eq.1) then
              do i=year1,year2
c
c   gmt
c
                 n_gmt=n_gmt+1
                 x_gmt(n_gmt)=i
                 y_gmt(n_gmt)=syr(i)
c
                 x(k)=i-size
                 y(k)=0
                 k=k+1
                 x(k)=i-size
                 y(k)=syr(i)
                 k=k+1
                 x(k)=i+size
                 y(k)=syr(i)
                 k=k+1
                 x(k)=i+size
                 y(k)=0
                 k=k+1
              enddo
              x(k)=year1-size
              y(k)=0
           endif
c
c   monthly distribution
c
           if(iplot.eq.2) then
              size=size/12.0
              do i=year1,year2
                 do l=1,12
c                    dayabs=i*365+(l-1)*31+31
                     call timsec(i,l,1,0,0,0.1,dayabs)
                    if(dayabs.ge.dayabs1.and.dayabs.le.dayabs2) then
                    xx=i+l/12.0    ! convert to fraction of year
                    x(k)=xx-size
                    y(k)=0
                    k=k+1
                    x(k)=xx-size
c
c   gmt
c
                    n_gmt=n_gmt+1
                    x_gmt(n_gmt)=xx
                    y_gmt(n_gmt)=smo(i,l)

                    y(k)=smo(i,l)
                    k=k+1
                    x(k)=xx+size
                    y(k)=smo(i,l)
                    k=k+1
                    x(k)=xx+size
                    y(k)=0
                    k=k+1
                    endif
                 enddo
              enddo
              x(k)=x(1)
              y(k)=0
              size=size*12.0
           endif
c
c   daily distribution
c
           kk=1                  ! count days
           if(iplot.eq.3) then
              do i=year1,year2
                 do l=1,12
                    call month_day(i,l,nday)
                    do m=1,nday
c                       dayabs=i*365+(l-1)*31+m
                        call timsec(i,l,m,0,0,0.1,dayabs)
                       if(dayabs.ge.dayabs1.and.dayabs.le.dayabs2)then
                       x(k)=kk-size
                       y(k)=0
                       k=k+1
                       x(k)=kk-size
                       y(k)=sda(i,l,m)
                       k=k+1
                       x(k)=kk+size
                       y(k)=sda(i,l,m)
c
c   gmt
c
                       n_gmt=n_gmt+1
                       x_gmt(n_gmt)=n_gmt
                       y_gmt(n_gmt)=sda(i,l,m)

                       k=k+1
                       x(k)=kk+size
                       y(k)=0
                       k=k+1
                       kk=kk+1    ! one more day
                       endif
                    enddo
                 enddo
              enddo
              x(k)=x(1)
              y(k)=0
           endif
c
c   time of day
c
           if(iplot.eq.4) then
              do i=1,24
                 x(k)=i-size
                 y(k)=0
                 k=k+1
                 x(k)=i-size
                 y(k)=time_of_day(i)
c
c   gmt
c
                 n_gmt=n_gmt+1
                 x_gmt(n_gmt)=i
                 y_gmt(n_gmt)=time_of_day(i)
                 k=k+1    ! jh oct 06

                 x(k)=i+size
                 y(k)=time_of_day(i)
                 k=k+1
                 x(k)=i+size
                 y(k)=0
                 k=k+1
              enddo
              x(k)=1-size
              y(k)=0
           endif
c
c  set defaults for output on tek screen and one hardcopy file
c
           open(65,file='catstat.eps',status='unknown')
           plotunit=65
           plotoption=1
           wsize=60
           call get_window_size
           if(size_catstat.gt.0) wsize=size_catstat ! from color.def
c
c   open plotter
c
           call open_display 
c
c set some postscipt scalings
c
           write(65,*) ' 1.0 0.55 scale'
           if(iplot.eq.1) then
              title=  'Yearly number of events'
              xtext='                 Year               '
              ytext='     Number             '
              filename_gmt='catstat.gmt.year.out'   ! output file name for gmt
              gmt_ofilename='catstat.gmt.year.ps'   ! gmt output file name
           endif
           if(iplot.eq.2) then
              title=  'Monthly number of events'
              xtext='                 Year               '
              ytext='     Number             '
              filename_gmt='catstat.gmt.month.out'   ! output file name for gmt
              gmt_ofilename='catstat.gmt.month.ps'   ! gmt output file name
           endif
           if(iplot.eq.3) then
              write(title,'(a,i4,1x,2i2)')
     *        'Daily number of events starting from ',year1,month1,day1
              xtext='                 Days               '
              ytext='     Number             '
              filename_gmt='catstat.gmt.day.out'   ! output file name for gmt
              gmt_ofilename='catstat.gmt.day.ps'   ! gmt output file name
           endif
           if(iplot.eq.4) then
              title=  'Time of day distribution'
              xtext='                 Hour               '
              ytext='     Number             '
              filename_gmt='catstat.gmt.hour.out'   ! output file name for gmt
              gmt_ofilename='catstat.gmt.hour.ps'   ! gmt output file name
           endif
c
c   gmt stuff
c
           gmt_xtitle=xtext
           gmt_ytitle=ytext
           gmt_maintitle=title              ! title
           gmt_xsize=12.0                   ! y-size in cm
           gmt_xlog=0                       ! linear
           gmt_ysize=10.0                   ! x-size in cm
           gmt_ylog=0                       ! linear
           gmt_ntraces=1                    ! one data set

c
c  open gmt file and write out main parameters
c
           open(26,file=filename_gmt,
     *     status='unknown')

           call gmt_xy_par(26,1,1,x_gmt,y_gmt)
c
c   write out data
c
           call gmt_xy_par(26,0,n_gmt,x_gmt,y_gmt)
c
c   close gmt file
c
           close(26)


c
c   plot points
c
           call xy_plot
     *     (1,k,x,y,title,xtext,ytext,
     *     600.0,600.0,100.0,100.0,2,1,20.0,
     *     0,cha,i,x,y)
           text='Push any key to stop'
           call  tchars(text,20,750.0,700.0)
c
c  call up cursxor so plots remains
c
495        continue
           call xscursr(i,x(1),y(1))
           if (char(i).eq.'#') goto 495

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
          call clear_to_alpha
          goto 1000            ! plot again
        endif
        
      if(iplot.gt.0) write(6,*)' Plot file is catstat.eps'
      write(6,*) ' Output files are: catyear.out : yearly data'
      print*, '                   catmonth.out: monthly data'
      print*, '                   catday.out  : daily data'
      print*, '                   cathour.out : hourly distribution' 
      print*, '    In addition, GMT prepared files                 ' 
      stop
      end
c-----------------------------------------------------------------------------
