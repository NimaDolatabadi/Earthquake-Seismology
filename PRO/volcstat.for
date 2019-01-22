
      program volcano_statistics
c
c program to generate GMT histograms of counts and energy distribution
c over time for selected volcanic subclasses, the programs reads s-files
c directly from the database
c
c Lars Ottemoller, 27/09/2002, Montserrat
c
c changes:
c   28012003 lot - added total counts
c                  added output of countes by date, changes done at MVO
c   20022003 lot - renamed program
c   20030704 lot - added date on time axis and changed filename to volcstat
c   20050124 lot - fix bug with grid and range for count plot
c
      implicit none
      include 'seidim.inc'
c
c    Seisan library inserts and routines...
c    ======================================
      include 'libsei.inc'                ! Open file definitions
      external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
      integer seiclen
C    ======================================
c-- event data
      character*80	data(max_data)	
c-- exp indicator                                         
      character*1	exp		
c-- event type                                      
      character*1      type		
c-- output indicator for comp. or norm.	  
      character*1      outf            
c-- number of recors for event                           
      integer		nrecord		
c-- number of header lines                                 
      integer		nhead		
c-- number of stations                                     
      integer		nstat		
c-- number of phases
      integer nphase
c-- start and end time of select      
      character*14    start_time,end_time 
c-- data base name if not agency (blank)               
      character*40	base_name	
c-- event file name                                   
      character*80	evfile		
c-- select key                                       
      character*10     key		
c-- see subroutine find...       
      integer		status,new_month,fstart,event_no 
c-- counters for events, records etc.      
      integer nr,nd,nl,records,nevent	
c-- counter                                                   
      integer i,j,ind,k,l,m
c-- true if file exists (CLK)             
      logical          exist           
c-- Arguments passed
      character*80     arg(20)
c-- id line number
      integer id
c---number of arguments and function
      integer nars
c logical for end of file
      logical          b_eof
c logical for existing file or not
      logical          b_old
c returned code
      integer          code
c read unit #1
      integer          read01
c write unit  #1
      integer          write01,write02,write03,write04,write05
c max number of subclasses
      integer maxsubclass
      parameter (maxsubclass=10)
c volcanic subclasses
      character*1 subclass(maxsubclass)
      integer nsubclass
c maximum number of days
      integer maxdays
      parameter (maxdays=10000)
c subclass counts
      integer totalcount(maxsubclass)
      integer daycount(maxsubclass,1000000)
      integer bin_count(maxsubclass,maxdays) 
      real bin_energy(maxsubclass,maxdays)
      integer bin_max
      integer dailycount
c date and time
      integer year,month,day,hour,min,doy
      real sec
c time in sec
      double precision msec
c time of first day in year of first event in seconds
      double precision ystart_sec,end_sec,start_sec
c day after first 
      integer daycounter
      integer last_day,first_day,prev_day
c bin width in days
      integer bindays
c text for bin width setting
      character*5 width_text
c text
      character*80 input
c gmt command
      character*240 gmt_text,gmt_var_text
      character*10 text
      character*80 range_text,grid_text,symbol_text,xx
      integer day_grid
      real y_grid
c vertical scale
      real ysize
c selected station
      character*5 selstat
      character*4 selcomp
      real energy,max_energy(10)
      real bin,binhigh
      character*10 outdate
      character*1 choice
     
c
c print version
c
      include 'version.inc'
      out_version_date='September 27, 2002'
      if (version_new) out_version_date=version_date
      call print_ver

c init
      nevent=0
      nsubclass=0
      ystart_sec=0.
      call timsec(2099,12,31,0,0,0.,start_sec)
      end_sec=0.
      bindays=1
      last_day=0
      selstat='     '
      selcomp='    '
      first_day=9999999
      do i=1,maxsubclass
        subclass(i)=' '
        daycount(i,1)=0
        totalcount(i)=0
        do j=1,maxdays
          bin_energy(i,j)=0.
          bin_count(i,j)=0
        enddo
        max_energy(i)=0.
      enddo

      start_time = ' '
      nars = 0
      call get_arguments(nars,arg)    ! get arguments 
      if(nars .ne. 0) then
         end_time = ' '
         base_name = ' '
         outf = ' '
         do i = 1,nars
            if(arg(i)(1:11) .eq. '-start_time')then
                read(arg(i+1),'(a14)') start_time
            endif
            if(arg(i)(1:9) .eq. '-end_time')then
              read(arg(i+1),'(a14)') end_time
            endif
            if(arg(i)(1:10).eq. '-base_name')then
              read(arg(i+1),'(a40)') base_name
            endif
            if(arg(i)(1:10).eq. '-bin_width')then
              read(arg(i+1),'(i10)') bindays
            endif
            if(arg(i)(1:8).eq. '-classes')then
              read(arg(i+1),'(10a1)') subclass
            endif
            if(arg(i)(1:5).eq. '-stat')then
              read(arg(i+1),'(a5)') selstat
            endif
            if(arg(i)(1:5).eq. '-comp')then
              read(arg(i+1),'(a4)') selcomp
            endif
            if(arg(i)(1:2).eq.'-h')then
              write(*,*)' Command line input:'
              write(*,*)'   -base_name XXX (blank=default)'
              write(*,*)'   -start_time yyyymmdd...'
              write(*,*)'   -end_time yyyymmdd...(blank=end of month)'
              write(*,*)'   -classes, max 10a1'
              write(*,*)'   -bin_width days'
c              write(*,*)'   -stat, max a5'
c              write(*,*)'   -comp, max a4'
              stop
            endif
         enddo
      endif
      if(start_time .ne. ' ') go to 50 
c                                             
c   input base name and time interval                                      
c                                                                               
      write(6,*)' Base name, ,, for local directory, ',
     *'name of index file'
      write(6,*)' or return for default base'                               
      read(5,'(a40)') base_name                                                
      write(6,'(1x,a,$)')' Start time                       : ' 
      read(5,'(a14)') start_time                                               
      write(6,'(1x,a,$)')' End time, return for end of month: ' 
      read(5,'(a14)') end_time                                                 
      write(6,'(1x,a,$)')' Bin width in days (Return for 1 day): ' 
      read(5,'(a10)') input
      if (seiclen(input).gt.0) read(input,'(i10)') bindays

      write(6,'(1x,a,$)') ' Give subclasses, max 20a1: ' 
      read(5,'(10a1)') subclass
c
c this part not used until apmengfft writes to sfiles again
c
c      write(6,'(1x,a,$)') ' Select station, max a5: ' 
c      read(5,'(a5)') selstat
c      write(6,'(1x,a,$)') ' Select component, max a4: ' 
c      read(5,'(a4)') selcomp
 1    continue                                                                 
	                                                                    
                                                                               
 50   continue                                                                 
      do i=1,10
        if (subclass(i).ne.' ') then
          nsubclass=nsubclass+1
        endif
      enddo
      do i=1,4
        if (selcomp(i:i).eq.'_') selcomp(i:i)=' '
      enddo

c                                                                               
c  start read and write loop                                                    
c                                                                               
 5     continue                                                                 
c-- always use next event                                
       key='          '		
       CALL findevin                                                       
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile,fstart,new_month,status)                                 
       if(status.eq.0) then                                                     
c--   (CLK)     
         inquire(file=evfile,exist=exist)                         
c--   (CLK)     
         if (exist) then                                          
            call sei open(old$+warn$,            ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    evfile,                ! File name
     &                    read01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
           call indata(read01,nstat,nphase,nhead,nrecord,
     &            type,exp,data,id)
           nevent=nevent+1                                                      
           read(data(1)(2:20),'(i4,1x,2i2,1x,2i2,1x,f4.1)')
     &       year,month,day,hour,min,sec
       write(*,*) data(1)(1:78)
c check that year is i4
           if (year.le.50) then
             year=year+1900
c           else
c             year=year+2000
           endif
           if (nevent.eq.1) then
             call timsec(year,1,1,0,0,0.,ystart_sec)
           endif
c
c find day
c
           msec=0.
           call timsec(year,month,day,hour,min,sec,msec)
           daycounter=int((msec-ystart_sec)/(60.*60.*24.))+1
           if (daycounter.gt.last_day) last_day=daycounter
           if (daycounter.lt.first_day) first_day=daycounter
c           if (msec.gt.end_sec) end_sec=msec
           if (msec.le.start_sec) then
             call timsec(year,month,day,0,0,1.,msec)
             start_sec=msec
           endif

c
c check for volcanic subclass
c
           if (data(1)(23:23).eq.'V') then
c             do i=2,nhead
             do i=2,nrecord    ! lot 06/02/2003
               if (data(i)(2:10).eq.'VOLC MAIN') then
                 ind=0
                 do j=1,nsubclass
                   if (data(i)(12:12).eq.subclass(j)) then
                     ind=j
                   endif
                 enddo
c
c count total of events
c
                 totalcount(ind)=totalcount(ind)+1
c
c store date in array
c
                 if (ind.ne.0) then
                   daycount(ind,1)=daycount(ind,1)+1
                   daycount(ind,daycount(ind,1)+1)=daycounter
                 endif
               endif
             enddo
c             do i=2,nhead
             do i=2,nrecord    ! lot 06/02/2003
               if (data(i)(2:5).eq.'VOLC'.and.
     &                 data(i)(7:11).eq.selstat.and.
     &                 data(i)(12:15).eq.selcomp) then
                 read(data(i)(30:37),'(e8.2)') energy
                 if (ind.ne.0) then
                   bin_energy(ind,daycounter)=
     &                 bin_energy(ind,daycounter)+energy
                 endif
               endif
             enddo
           endif
           call sei close (close$,read01,code)
         else                                                      
           write(6,'(1x,a60,a)') evfile(1:60),' doesn''t exist.'         
         endif                                                     
c-- back for next event                                         
         goto 5			
       else                                                                     
c-- 3 is end of time period                       
          if(status.eq.2) write(6,*)' End of index file'
          if(status.gt.3)       
     *    write(6,*)' ****** Something wrong, status= ', status                 
       endif                                                                    

c
c open outputfile for counts
c
       call sei open(unknown$+warn$,           ! Open a unknown status file.

     &                    ' ',                   ! Prompt file name (n/a).
     & 'volcstat_counts_total.out',  ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
c
c write out totals
c
      do i=1,nsubclass
        write(write01,'(a1,1x,i10)') subclass(i),totalcount(i)
      enddo
      call sei close(close$,write01,code)
      do i=1,nsubclass
c                                                                               
c   open output file                                                            
c                                                                               
        if (subclass(i).ne.' ') then
          call sei open(unknown$+warn$,           ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     & 'volcstat_counts_'//subclass(i)//'.out',  ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
          call sei open(unknown$+warn$,           ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     & 'volcstat_daily_'//subclass(i)//'.out',  ! File name
     &                    write04,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
c          call sei open(unknown$+warn$,           ! Open a unknown status file.
c     &                    ' ',                   ! Prompt file name (n/a).
c     & 'volcstat_energy_'//subclass(i)//'.out',  ! File name
c     &                    write02,               ! Write unit #1
c     &                    b_old,                 ! Already exists? (n/a).
c     &                    code)                  ! Returned condition.
          prev_day=first_day
          dailycount=0
          do j=2,daycount(i,1)+1
            write(write01,*) daycount(i,j)
c
c count events until new date
c
            if (daycount(i,j).eq.prev_day) then
              dailycount=dailycount+1
            else
c
c write out dates and number of events
c
              call sectim(ystart_sec+(prev_day-1)*86400,
     &            year,doy,month,day,hour,min,sec)
              write(write04,'(i4,2i2.2,1x,i6)') 
     &         year,month,day,dailycount
              dailycount=1
              prev_day=daycount(i,j)
            endif
          enddo
          if (daycount(i,1).eq.0) then
            write(write01,*) -1
          endif
          energy=0.
          bin=bindays
          if (bindays.gt.2) bin=0
          binhigh=bin+bindays/2.
          do j=1,maxdays
            if (j.ge.binhigh) then
c              write(write02,*) bin,energy
              if (energy.gt.max_energy(i))
     &                max_energy(i)=energy
              energy=0.
              bin=bin+float(bindays)
              binhigh=bin+bindays/2.
            endif
            energy=energy+bin_energy(i,j)
          enddo
        endif
        call sei close (close$,write01,code)
c        call sei close (close$,write02,code)
      enddo
c
c plot graphs using GMT
c
      call sei open(unknown$,            ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    'volcstat.batch',      ! File name
     &                    write03,               ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
      gmt_text=' '
      gmt_text='gmtset MEASURE_UNIT cm'
      call systemc(gmt_text,seiclen(gmt_text))
      write(write03,*) gmt_text(1:seiclen(gmt_text))
      gmt_text=' '
      gmt_text='gmtset PAPER_MEDIA a4+'
      call systemc(gmt_text,seiclen(gmt_text))
      write(write03,*) gmt_text(1:seiclen(gmt_text))
      gmt_text=' '
      gmt_text='gmtset ANOT_FONT Helvetica'
      call systemc(gmt_text,seiclen(gmt_text))
      write(write03,*) gmt_text(1:seiclen(gmt_text))
      gmt_text=' '
      gmt_text='gmtset ANOT_FONT_SIZE 10p'
      call systemc(gmt_text,seiclen(gmt_text))
      write(write03,*) gmt_text(1:seiclen(gmt_text))
      gmt_text=' '
      gmt_text='gmtset LABEL_FONT Helvetica'
      call systemc(gmt_text,seiclen(gmt_text))
      write(write03,*) gmt_text(1:seiclen(gmt_text))
      gmt_text=' '
      gmt_text='gmtset LABEL_FONT_SIZE 10p'
      call systemc(gmt_text,seiclen(gmt_text))
      write(write03,*) gmt_text(1:seiclen(gmt_text))
      gmt_text=' '
      gmt_text='gmtset TICK_LENGTH .05c'
      call systemc(gmt_text,seiclen(gmt_text))
      write(write03,*) gmt_text(1:seiclen(gmt_text))

      if (bindays.lt.10) then
        write(width_text,'(a2,i1)') '-W',bindays
      elseif (bindays.lt.100) then
        write(width_text,'(a2,i2)') '-W',bindays
      endif

c
c set figure size 
c
c      ysize=(24.-((nsubclass-2)*1.5))/nsubclass 
      ysize=(22.-((nsubclass-2)*.5))/nsubclass 
      if (ysize.gt.5.) ysize=5.
c
c delete output files
c
      gmt_text=' '
      gmt_text='rm -f volcstat_counts.ps'
      call systemc(gmt_text,seiclen(gmt_text))
      gmt_text=' '
      gmt_text='rm -f volcstat_energy.ps'
      call systemc(gmt_text,seiclen(gmt_text))
      do i=1,nsubclass

c
c find bin range for counts plot
c
          bin_max=0
          do m=first_day,last_day,bindays
            l=0
            do k=2,daycount(i,1)+1
              if (daycount(i,k).ge.m.and.
     &           daycount(i,k).le.m+bindays-1) then
                l=l+1
              endif
            enddo
            if (l.gt.bin_max) bin_max=l+int(l*.1)
          enddo
          if (bin_max.eq.0) bin_max=1

c set day grid
          day_grid=int((last_day-first_day)/10.)
          if (day_grid.eq.0) day_grid=1

          if (i.eq.1) then
            write(text,'(f4.1)') 27.-ysize 
            gmt_var_text='-P -K -X2.5 -Y'//text(1:4)//' > '
c          elseif (i.eq.nsubclass) then
c            write(text,'(f4.1)') -ysize -1.5
c            gmt_var_text='-O -X0. -Y'//text(1:4)//' >> '
          else
c            write(text,'(f4.1)') -ysize -1.5
            write(text,'(f4.1)') -ysize -0.5
            gmt_var_text='-K -O -X0. -Y'//text(1:4)//' >> '
          endif
  
c
c use pshistogram for count plots
c
          gmt_text=' '
          gmt_text='gmtset D_FORMAT %lg'
          call systemc(gmt_text,seiclen(gmt_text))

c set grid text
          y_grid=float(bin_max)/3.
          if (int(y_grid).eq.0.) y_grid=1.
          write(grid_text,*)
c     &     '-B',day_grid,':"days":/',int(y_grid),':"counts  '
c     &      //subclass(i)//'":WSne '
     &     '-B',day_grid,'/',int(y_grid),':"counts  '
     &      //subclass(i)//'":Wsne '
          xx=' '
          k=0
          do m=1,seiclen(grid_text)
            if (seiclen(grid_text(m:m)).ne.0.or.
     &         grid_text(m-1:m-1).eq.'y') then
              k=k+1
              xx(k:k)=grid_text(m:m)
            endif
          enddo
          grid_text=xx(1:seiclen(xx))
c          do m=1,seiclen(grid_text)
c            if (grid_text(m:m).eq.' ') 
c     &          grid_text=grid_text(1:m-1)//grid_text(m+1:)
c          enddo
c set range text
          write(range_text,*)'-R',first_day-1,'/',last_day+1,'/0/',
     &       int(bin_max*1.1)
c          do m=1,seiclen(range_text)
c            if (range_text(m:m).eq.' ') 
c     &          range_text=range_text(1:m-1)//range_text(m+1:)
c          enddo
          xx=' '
          k=0
          do m=1,seiclen(range_text)
            if (seiclen(range_text(m:m)).ne.0) then
              k=k+1
              xx(k:k)=range_text(m:m)
            endif
          enddo
          range_text=xx(1:seiclen(xx))

          write(text,'(f3.1)') ysize
          gmt_text=' '
          gmt_text='pshistogram volcstat_counts_'//subclass(i)//
     &      '.out '//width_text(1:seiclen(width_text))//' '//
     &      range_text(1:seiclen(range_text))//' '//
     &      grid_text(1:seiclen(grid_text))//
     &      ' -G200 -JX18./'//text(1:3)//
     &      ' -C -L '
     &      //gmt_var_text(1:seiclen(gmt_var_text))//
     &      ' volcstat_counts.ps'
          call systemc(gmt_text,seiclen(gmt_text))
          write(write03,*) gmt_text(1:seiclen(gmt_text))
c
c add date x-axis after last plot
c
          if (i.eq.nsubclass) then
c
c work out spacing, axis is 18 cm, number of days is last_day-first_day
c plot 10 dates 
c
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &              'volcstat_date_axis.out',   ! File name
     &                    write05,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            start_sec=start_sec-(24.*3600.)
            end_sec=start_sec+(last_day-first_day+2)*(24.*3600.)
            msec=start_sec

            k=0
            do m=first_day-1,last_day+1
              call sectim(msec,year,doy,month,day,hour,min,sec)
              if (int((m-first_day)/float(day_grid))
     &          .eq.(m-first_day)/float(day_grid).and.
     &          m.ne.first_day-1.and.m.ne.last_day+1) then
              outdate=' '
              write(outdate,'(i4.4,a1,i2.2,a1,i2.2)')
     &          year,'/',month,'/',day
              write(write05,'(f4.1,1x,f3.1,1x,a,a10)') 
     &          k*18./(last_day-first_day+2),
     &          2.,' 10 90. 0 MR ',outdate
              endif
              msec=msec+24.*3600.
              k=k+1
            enddo
            call sei close (close$,write05,code)
            gmt_text=' '
            gmt_text='pstext volcstat_date_axis.out -JX18/3 -R0/18/-3/3'
     &       // ' -P -O -X0. -Y-3. >>  volcstat_counts.ps '
            write(write03,*) gmt_text(1:seiclen(gmt_text))
            call systemc(gmt_text,seiclen(gmt_text))
          endif

          goto 9000
c
c use psxy for energy plots
c
          gmt_text=' '
          gmt_text='gmtset D_FORMAT %4.2e'
          call systemc(gmt_text,seiclen(gmt_text))

          if (i.eq.nsubclass) then
            write(text,'(f4.1)') -ysize -1.5
            gmt_var_text='-O -K -X0. -Y'//text(1:4)//' >> '
          endif
c set grid text
          y_grid=max_energy(i)/3.
          if (y_grid.eq.0) then
            y_grid=1.
            max_energy(i)=1.
          endif
          write(grid_text,*)
c     &     '-B',day_grid,':"days":/',y_grid,':"energy '
     &     '-B/',y_grid,':"energy '
     &      //subclass(i)//'":WSne '
          xx=' '
          k=0
          do m=1,seiclen(grid_text)
            if (seiclen(grid_text(m:m)).ne.0.or.
     &         grid_text(m-1:m-1).eq.'y') then
              k=k+1
              xx(k:k)=grid_text(m:m)
            endif
          enddo
          grid_text=xx(1:seiclen(xx))
c set range text
          range_text=' '
          write(range_text,*)
     &       '-R',first_day-1,'/',last_day+1,'/0/',
     &       max_energy(i)*1.1
          xx=' '
          k=0
          do m=1,seiclen(range_text)
            if (seiclen(range_text(m:m)).ne.0) then
              k=k+1
              xx(k:k)=range_text(m:m)
            endif
          enddo
          range_text=xx(1:seiclen(xx))
c set symbol text
          xx=' '
          k=0
          write(symbol_text,*)'-Sb',
     &       18./float(last_day-first_day+2)*float(bindays)
          do m=1,seiclen(symbol_text)
            if (seiclen(symbol_text(m:m)).ne.0) then
              k=k+1
              xx(k:k)=symbol_text(m:m)
            endif
          enddo
          symbol_text=xx(1:seiclen(xx))

          write(text,'(f3.1)') ysize
          gmt_text=' '
          gmt_text='psxy volcstat_energy_'//subclass(i)//
     &      '.out '//width_text(1:seiclen(width_text))//' '//
     &      range_text(1:seiclen(range_text))//' '//
     &      grid_text(1:seiclen(grid_text))//' '//
     &      symbol_text(1:seiclen(symbol_text))//
     &      ' -W.25 -G200 -JX18./'//text(1:3)//
     &      ' '
     &      //gmt_var_text(1:seiclen(gmt_var_text))//
     &      ' volcstat_energy.ps'
          call systemc(gmt_text,seiclen(gmt_text))
          write(write03,*) gmt_text(1:seiclen(gmt_text))

c
c plot day scale, since on energy plot y axis is 4.2e
c
          gmt_text=' '
          gmt_text='gmtset D_FORMAT %lg'
          call systemc(gmt_text,seiclen(gmt_text)) 
          if (i.eq.nsubclass) then
            gmt_var_text='-O >> '
          else
            gmt_var_text='-K -O >> '
          endif
c set range text
          range_text=' '
          write(range_text,*)
     &       '-R',first_day-1,'/',last_day+1,'/0/1'
          xx=' '
          k=0
          do m=1,seiclen(range_text)
            if (seiclen(range_text(m:m)).ne.0) then
              k=k+1
              xx(k:k)=range_text(m:m)
            endif
          enddo
          range_text=xx(1:seiclen(xx))
          write(grid_text,*)
     &     '-B',day_grid,':"days":S '
          xx=' '
          k=0
          do m=1,seiclen(grid_text)
            if (seiclen(grid_text(m:m)).ne.0.or.
     &         grid_text(m-1:m-1).eq.'y') then
              k=k+1
              xx(k:k)=grid_text(m:m)
            endif
          enddo
          grid_text=xx(1:seiclen(xx))
c set range text
          gmt_text='psxy volcstat_energy_'//subclass(i)//
     &        '.out '//width_text(1:seiclen(width_text))//' '//
     &        range_text(1:seiclen(range_text))//' '//
     &        grid_text(1:seiclen(grid_text))//' '//
     &        symbol_text(1:seiclen(symbol_text))//
     &        ' -G200 -JX18./0.01'//
     &        ' '
     &        //gmt_var_text(1:seiclen(gmt_var_text))//
     &        ' volcstat_energy.ps'
          call systemc(gmt_text,seiclen(gmt_text))
          write(write03,*) gmt_text(1:seiclen(gmt_text))
9000    continue
      enddo
      call sei close (close$,write03,code)

      write(*,*)
      write(6,'(a)') ' Output files: '
      write(6,'(a)') '                volcstat.batch '//
     &        '- batch file to produce GMT output '
      write(6,'(a)') '                volcstat_counts_x.out '//
     &        '- counts data for subclass x '
      write(6,'(a)') '                volcstat_energy_x.out '//
     &        '- energy data for subclass x '
      write(6,'(a)') '                volcstat_daily_x.out '//
     &        '- counts per day for subclass x '
      write(6,'(a)') '                volcstat_counts_total.out '//
     &        '- total counts for subclasses '
      write(6,'(a)') '                volcstat_counts.ps '//
     &        '- counts Postscript output file '
c      write(6,'(a)') '                volcstat_energy.ps '//
c     &        '- energy Postscript output file '
      if(nars .eq. 0) then
        write(*,*)
        write(*,'(a,$)') ' Plot results with ghostscript ? '
        read(5,'(a1)') choice
        if (choice.eq.'y'.or.choice.eq.'Y')
     &     call systemc('gs volcstat_counts.ps',21)
      endif
      stop                                                                      
      end                                                                       
                                                                                
                                                                                
