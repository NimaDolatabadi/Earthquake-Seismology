c program rmsdep
c m. villagran
c 31/aug/94
c
c
c  updates:
c  jun 8 by jh     give range
c  oct 98 jh  :------------------------------------------------------------
c             :  ------------------   version 7.0 check, no change --------
c  nov 5 98   : system_c to systemc
c  jun 20 o5 jh: plt to eps
c  oct 3 10 jh: use steps to 0.1, do not confirm plot, not checke if offline
c               program option work, see variable step which hqas been changed
c               from integer to real throughout
c  dec 20 10 jh: gfortran for pc changes. Formatted read problem, remove pc inc
c                remove variable hctype, unit check
c  feb 22 11 jh: size from color.def
c  oct 10 12 jh: fix non eev run of rmsdep so depth can be above 
c                100 km and decimal, fix a few more problems 
c                related to real step
c  feb 17 14 jh: read seisan.def   
c
c include the following soubroutines
c ch_depth, change and fix depth step by step to all events
c get_results, get rms values and write gnuplot dat files
c fil_gnu write gnuplot command file
c
c Reliable variables in main program are:
c
c nev = number of events
c idep = depth running
c step = step in magnitude
c maxdep = maximun depth to reach
c nstep = number of steps to do ((maxdep-mindep)/step + 1) it includes 0.0 depth
c
c Reliable variables in subroutine get_results are:
c
c maxstep = it is a parameter to control max number of step in program
c maxev = it is a parameter to control max number of events in program
c output*6005 = char. variable to write matrix. 
c
c input is an s-file with one or many (1000 max) events, step and max depth
c
c outputs are:
c
c 1. rmsdep.out   (is a command file prepared for gnuplot)
c if the s-file exceed 10 events, no title for each curve.
c
c 2. rmsdep.dat including depth in the 1st. column and as many columns 
c as events (if less than 51) with the respective obtained rms's.
c
c 3. rmsdep.hst data for histogram, n of events with lower rms's per depth
c
c Number of events is restricted to 200 depths and 1000 events, 
c
c To increase them change the following:
c
c line of main program --> if(nstep.gt.200)then
c line in subroutine ch_depth: if(k.gt.1000)goto 80
c In subroutine get_results increase parameters maxstep and maxev
c and character variable output*6005 should be (nev*6+5)
c
c

      implicit none
c input file
      character infile*80,text,head*80
c loop, maximum depth and depth step variables
      integer i,idep,maxdep,mindep
      real step,xdep
c number of events,and number of step in process,length of the string
      integer nev,nstep,len,maxnminr
c the highest values
      real highrms
      logical from_eev    ! if from eev
c I honorably represent my name
      real dummy
c
      include 'version.inc'

      call get_seisan_def
c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   check if called from eev
c
      from_eev=.false.
      call get_env_event(infile)
      if(infile(1:1).ne.' '.and.ichar(infile(1:1)).ne.0) then
         from_eev=.true.
         text=' '
         call put_env_event(text)
      endif
c write(6,*) from_eev,infile
c
c ask for s-file, and input parameters
c
      if(.not.from_eev) then
         write(6,*)'name of s-file ??'
         read(5,'(a)')infile
      endif
c
c   get depth range
c
      write(6,*)'Depth range to use ??'
      read(5,*)mindep,maxdep
c     maxdep=dummy
      write(6,*)'Step, not smaller than 0.1 ?'
      read(5,*)step

c
c 
c test.out is a dummy file to avoid questions of hyp program.
c
      open(1,file='killme',status='unknown')
      write(1,'(a)') 'test.out'
      write(1,'(a1)')'n'
      close(1)
      nstep=((maxdep-mindep)/step)+1
      if(nstep.gt.200)then
         write(6,*)' Max. number of steps exceeded !!'
         write(6,*)' '
         stop
      endif
c
c------------------------------------------------------
c   if program is started from eev, go to section eev
c------------------------------------------------------
c
      if(from_eev) goto 2000
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      open(2,file='rmsdep.dat',status='unknown')
      open(4,file='rmsdep.out',status='unknown')
      open(5,file='rmsdep.hst',status='unknown')
c
      highrms=0.0
      maxnminr=0
c
c     LOOP STARTS HERE!!!
c
      do 10 i=1,nstep
         xdep=(i-1)*step+mindep
         open(3,file=infile,status='old')                                   
c change depth to s-file
         call ch_depth (xdep,infile,nev)
c locate events
         call systemc('hyp < killme',12)
         len=nev*6+6
c
c get rms's from the print.out and write in rmsdep.dat, get higher value 
c and number of events with lower value to write in rmsdep.hst 
c
         call get_results(xdep,nev,highrms,len,step,nstep,maxnminr)
         rewind 3
10    continue
c
c create rmsdep.out file
c and screenout outputs of the program
c also writes statistics at the bottom of rmsdep.hst
c
      call fil_gnu (maxdep,highrms,nev,maxnminr,step)
      write(6,*)' '
      write(4,*)'#'
      write(6,'(a12,i3,a7)')'There are   ',nev,' events'
      write(4,'(a13,i3,a7)')'#There are   ',nev,' events'
      write(6,'(a12,i3,a13)')'Located at  ',nstep,' diff. depths'
      write(4,'(a12,i3,a13)')'#Located at  ',nstep,' diff. depths'
      write(6,'(a12,f5.1,a4)')'in steps of ',step,' kms'
      write(4,'(a13,f5.1,a4)')'#in steps of ',step,' kms'
      step=step*(nstep-1)
      write(6,'(a21,f5.1,a4)')'starting from 0.0 to ',step,' kms'
      write(4,'(a22,f5.1,a4)')'#starting from 0.0 to ',step,' kms'
      write(6,*)' '
      write(6,*)'output files are:'
      write(6,*)' '
      write(6,*)'  rmsdep.dat --> matrix: first column is the depth,'
      write(6,*)'                 the rest are the rms per event.'
      if(nev.gt.50)then
      write(6,*)'                 NOT POSSIBLE TO DISPLAY IN GNUPLOT'
      endif
      write(6,*)' '
      write(6,*)'  rmsdep.out --> now invoke gnuplot and use'
      write(6,*)'                 the command load "rmsdep.out"'
      write(6,*)'    '
      write(6,*)'  rmsdep.hst --> histogram data'
      write(6,*)'    '
      goto 99
c
c-----------------------------------------------------------------
c   section when program is started from eev
c-----------------------------------------------------------------
c
 2000 continue
c
c   generate file with many depths
c
      call many_depths(infile,mindep,nstep,step,head)
c
c locate events
c
      call systemc('hyp < killme',12)
c
c   get rms and depths
c
      call get_depth_rms(head)


99    stop
      end
c
c that's it
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c
      subroutine ch_depth(xdep,infile,k)
c
c fix depth=j for all events in the s-file
c m.v 31/aug/94
c
      implicit none
      character all*80,text*38,infile*30
      integer i,j,k
      real xdep
      k=1
      open(10,file='test.out',status='unknown')
c because the first event the same thing 2 times
      read(3,'(a80)',end=80)all(1:80)                                           
      write(all(39:45),'(f5.1,a2)')xdep,'F '
      write(10,'(a80)')all(1:80)
c big loop after the first line starts here
10    read(3,'(a80)',end=20)all(1:80)                                           
          if(all(1:12).eq.'            '.and.all(80:80).eq.' ')then
          write(10,'(a80)')all(1:80)
          read(3,'(a80)',end=20)all(1:80)   
          write(all(39:45),'(f5.1,a2)')xdep,'F '
          k=k+1
          if(k.gt.1000)goto 80
          endif
      write(10,'(a80)')all(1:80)
      goto 10                                                                   
20    close(1,status='delete') 
      close(10)
c      text(1:8)='mv test '
c      do i=1,30
c         if(infile(i:i).eq.' ')goto 30
c      enddo
c30    text(9:i+7)=infile(1:i-1)
c      call systemc(text(1:i+7),i+7)                                      
      goto 99
80    write(6,*)' '
      write(6,*)'empty s-file or more events than expected !!'
      write(6,*)' '
      stop
99    return                                                                    
      end                                                                       
c
      subroutine get_results(xdep,nev,highrms,len,step,nstep,maxnminr)
c  
c capture y=rms from the print.out, get max and min rms value
c and create histogram file rmsdep.hst
c write the matrix in rmsdep.dat if n of events does not exceed 50
c m.v 31/aug/94
c
      implicit none
      integer maxstep,maxev
      parameter(maxev=1000)
      parameter(maxstep=200)
      character all*90,output*6005    ! all to 90, gfortran pc
      integer j,nev,len,k,i,idep,nstep
      real step,xdep
      integer a(maxstep),maxnminr
      real y(maxstep,maxev),highrms,x
      open(10,file='print.out',status='old')
c      j=idep/step+1
      j=xdep/step+1
      k=1
20    read(10,'(a)',end=30)all                  ! gfortran pc
          if(all(1:12).eq.'   date hrmn')then
          read(10,'(a)',end=30)all
          read(all(1:57),300,end=30)y(j,k)
          if(highrms.lt.y(j,k))highrms=y(j,k)
          k=k+1
          endif
      goto 20
30    close(10)
c
c writes data in rmsdep.dat
c
         k=7
         write(output(1:6),'(f5.1,1x)')xdep
               do i=1,nev
               write(output(k:k+5),400)y(j,i)
               k=k+6
               enddo
         write(2,'(a)')output(1:len-1) 
c
c if last result, write in rmsdep.hst the histogram data
c
      if(j.eq.nstep)then
         do 50 k=1,nev
         x=100.0
            do i=1,nstep
            if(y(i,k).lt.x) x=y(i,k)
            enddo
            do i=1,nstep
            if(y(i,k).eq.x) a(i)=a(i)+1
            enddo
50       continue
         do i=1,nstep
            write(5,'(f5.1,1x,i4)')step*(i-1),a(i)
            if(a(i).gt.maxnminr)maxnminr=a(i)
         enddo
      endif
 300  format(52x,f5.2)
 400  format(f5.2,1x)
99    return
      end
c
      subroutine fil_gnu (maxdep,highrms,nev,maxnminr,step)
c
c writes rmsdep.out to be used by gnuplot
c m.v. 2 aug 94 
c 
      implicit none
      character output*1550
      integer i,j,k,nev,maxnminr
      real step
      integer maxdep
      real highrms
c
c write obligatory lines
c
      write(4,'(a)')'set xlabel " Depth Kms."'
      write(4,'(a15,i4,a1)')'set xrange [-1:',maxdep,']'
      write(4,'(a)')'set grid'
      write(4,'(a)')'set data style line'
      if(nev.gt.10)write(4,'(a)')'set nokey'
      write(4,'(a)')'#set term postscript'
      write(4,'(a)')'#set output "gnuplot.eps"'
      output(1:5)='plot '
c if rmsdep.dat
      if(nev.lt.51)then
      highrms=highrms+1
      i=highrms
      write(4,'(a14,i3,a1)')'set yrange [0:',i,']'
      write(4,'(a)')'set ylabel " RMS"'
      write(4,'(a)')'set title "RMS-Depth"'
      j=6
15       do k=1,nev+1
         if(k.lt.9)then
         write(output(j:j+18),500)k+1
         j=j+19
         if(k.eq.nev+1)j=j-21
         endif
         if(k.gt.8)then
         write(output(j:j+19),510)k+1
         j=j+20
         if(k.eq.nev+1)j=j-22
         endif
c        if(k.gt.98)then
c        write(output(j:j+20),520)k+1
c        j=j+21
c        if(k.eq.nev+1)j=j-23
c        endif
         enddo
      write(4,'(a)')output(1:j)
      write(4,'(a)')'pause -1 "Hit Return (Histogram next)"'
      endif
      if(nev.lt.11)write(4,'(a)')'set nokey'
      write(4,'(a13,f5.2)')'set boxwidth ',0.5*step
      write(4,'(a)')'set ylabel "Number of earthquakes"'
      write(4,'(a)')'set title "Minimum RMS of travel time residuals"'
      write(4,'(a14,i4,a1)')'set yrange [0:',maxnminr+1,']'
      write(4,'(a)')'plot "rmsdep.hst" w boxes'
      write(4,'(a)')'#----------STATISTICS----------'
 500  format('"rmsdep.dat" u 1:',i1,',')
 510  format('"rmsdep.dat" u 1:',i2,',')
c520  format('"rmsdep.dat" u 1:',i3,',')
99    return
      end
c
      subroutine many_depths(evfile,mindep,ndepth,step,text)
      
      include 'seidim.inc'
      integer nstep
      real step
      real depth
      integer nstat,nphase,nhead,nrecord,id
      character*1 evid,exp
      character*80 data(max_data),evfile
      character*80 text
c
c   read event file
c
      open(27,file=evfile,status='old')
      call indata
     *(27,nstat,nphase,nhead,nrecord,evid,exp,data,id)
      close(27)      
      text=data(1)
c
c   generate output file with all depths
c
      open(27,file='test.out',status='unknown')
      depth=mindep
      do i=1,ndepth
         write(data(1)(39:44),'(f5.1,a)')depth,'F'
         write(27,'(a)')(data(k),k=1,nrecord)
         depth=depth+step
      enddo
      close(27)
      return
      end
c
      subroutine get_depth_rms(head)
c
c  get depths and rms from print.out file
c
      implicit none
      include 'seiplot.inc'
      character*90 text                   ! general text ! gfortran to 90
      real x(1000),y(1000)                ! depth and rms
      real xx(5),yy(5)                    ! dummy
      integer ndepth,i                    ! counters
      character*1 cha(10)                 ! dummy for xy_plot
      character*80 title,head             ! title for plot      
      character*30 xtext,ytext            ! axis title
      character*1  do_plot
c
      open(27,file='print.out',status='old')
c
c   read loop
c
      ndepth=1
c
 10   continue
      read(27,'(a)',end=99) text    ! gfortran pc
c     write(6,*) text 
      if(text(1:12).eq.'   date hrmn') then
          read(27,'(a)',end=99)text
          read(text(39:43),'(f5.1)') x(ndepth)
          read(text(53:57),'(f5.2)') y(ndepth)
          ndepth=ndepth+1
      endif
      goto 10
c
c   end of reading
c
  99  continue
      ndepth=ndepth-1
      do i=1,ndepth
        write(6,*) x(i),y(i)
      enddo
c
c   plot results
c
      write(6,*) 'Plot results (y=default/n)'
      read(5,'(a)') do_plot
      if(do_plot.eq.'y'.or.do_plot.eq.'Y'.or.do_plot.eq.' ') then
c
c  set defaults for output on tek screen and one hardcopy file
c
           open(65,file='rmsdep.eps',status='unknown')
           plotunit=65
           plotoption=1
           wsize=50
           call get_window_size
           if(size_rmsdep.gt.0) wsize=size_rmsdep ! from color.def
c 
c
c   open plotter
c
           call open_display 
           title=head
           xtext='              depth(km)  '
           ytext='              RMS(sec)   '
c
c   plot points
c
           text='q to quit'
           call tchars(text,9,750.0,700.0)
           call xy_plot
     *     (1,ndepth,x,y,title,xtext,ytext,
     *     600.0,600.0,100.0,100.0,3,1,10.0,
     *     1,cha,i,xx,yy)
           call clear_to_alpha
           call close_post
           write(6,*)' Plot file is rmsdep.eps'

c
c   clear
c
      endif

      return
      end
c      include 'pcplot.inc'
