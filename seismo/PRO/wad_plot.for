c
c   wadati plot, input is one file in nordic format 
c
c
c   j. havskov, september 2000
c
c   updates
c   oct 8 2004 jh : do not plot if no data
c   jun  20 05 jh: plt to eps
c   feb 9  05  jh: higher accuracy in plot out of s-p
c   dec 20 10  jh: remove winplot include for gfortran and pc, unit check
c   feb 22 11  jh: input size from color.def
c   apr 29 13  jh: fıx output overflow
c   feb 17 14  jh: read seisan.def

      implicit none
      include 'seidim.inc'
      include 'seisan.inc'

      include 'seiplot.inc'   ! seisan graphics
      real x,y
      character*1 cha(10)                 ! dummy for xy_plot
      character*80 text                   ! general text
      character*80 txt(100)
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      real xc,yc                          ! screen scaling
      character*1 make_plot 
      integer n,i,id,nwad,l,k 
      integer*4 hh
c
c   nordic file parameters
c
      CHARACTER*80 DATA(max_data)                                                    
      CHARACTER*1 TYPE,EXP
      logical compact
      integer nstat,nphase,nhead,nrecord,nars
      character*80 file(10)
c
      real a,b,corr,rms 
      real vps,t_origin
      real tp(500)   ! p-times
      real tsp(500)  ! s-p times
      character*30 phase_p(500),phase_s(500) ! info on phases used
      real tor       ! arbitrary origin time, only for line plot
      character*5 stat(500)   ! station codes
c
c   origin time
c
      integer hour,min
      real sec

      include 'version.inc'

      call get_seisan_def

c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   check if input via argument
c
      call get_arguments(nars,file)
      if(nars.lt.1) then
         write(6,*)'File name'
         read(5,'(a)') file(1)
      endif

      open(1,file=file(1),err = 30,status='old') 

c
c   check that not a compact file
c
      call nortype(1,compact)
      if(compact) then
         write(6,*)' Input file compact, cannot use'
         stop
      endif
c
c   read event
c
      call indata(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)

      if(nrecord.eq.0) then
         write(6,*)' No data'
         stop
      endif
c
      close(1)
c
c   do wadati analysis
c
      call wadati_s(data,nhead,nrecord,3,nwad,vps,hour,
     *  min,sec,rms,corr,tp,tsp,tor,stat,phase_p,phase_s)

c
c   write result for wadati
c
      if(nwad.ge.2) then
           write(6,'(1x,a,a,2i2,f6.1,a,i4,a,f5.2,a,f6.2,a,f6.3)')
     *     data(1)(1:20),' T0: ',hour,min,sec,' N: ',nwad,' VPS: ',
     *     vps,' RMS: ',rms,' CORR: ',corr
      else
           write(6,'(1x,a,a)') data(1)(1:20),' No data for Wadati'
           stop
      endif

      write(6,*) ' Plot relation (y=default/n)'
      read(5,'(a)') make_plot
      if((make_plot.eq.'y') .or. (make_plot.eq.' ')) then 
c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='wad_plot.eps',status='unknown')
          plotunit=65
          plotoption=1
          wsize=60
          call get_window_size
          if(size_wad_plot.gt.0) wsize=size_wad_plot ! from color.def
c
c
c   open plotter
c
c
c   set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'

          write(title,'(a,2i2,1x,f5.1,a,f6.2)')
     *    ' Wadati diagram:  T0 ',hour,min,sec,'  VP/VS ', vps
          xtext='     P-time, seconds'
          ytext='S-P time    seconds           '
c
c   open display
c
	call open_display 
c
c   plot points
c
          call xy_plot
     *    (1,nwad,tp,tsp,title,xtext,ytext,
     *    600.0,600.0,100.0,100.0,1,1,20.0,
     *    0,cha,i,x,y)	   
c             
c  plot line
c
          call xy_plot_line(vps-1.0,tor,100.0,100.0)
          txt(1)='Select station by'
          txt(2)='clicking near symbol'
          call xmessage(txt,2,25,720.0,650.0)
c
c   plot all stations
c
          i=1
          l=2
          txt(1)='STAT PS  S-P  STAT PS  S-P'
 554      continue
          txt(l)=' '
          do k=1,2
             if(k.eq.1) then
                write(txt(l)(1:13),'(a5,a1,a1,f5.1)')
     *          stat(i),phase_p(i)(11:11),phase_s(i)(11:11),tsp(i)
                i=i+1
                if(i.gt.nwad) goto 555
             endif
             if(k.eq.2) then
                write(txt(l)(16:28),'(a5,a1,a1,f5.1)')
     *          stat(i),phase_p(i)(11:11),phase_s(i)(11:11),tsp(i)
                i=i+1
                l=l+1
                if(i.gt.nwad) goto 555
                goto 554
             endif
          enddo
 555      continue
          if(k.eq.2) l=l-1
          call xmessage(txt,l,28,710.0,10.0)
c
c  call up cursxor so plots remains
c
 55       continue
          call xy_plot_input(100.0,100.0,cha(1),x,y,xc,yc)
c
c   find corresponding station
c
          txt(1)=' '
          txt(1)='Too far from symbol,'
          txt(2)='try again'
          txt(3)=' '
c          write(27,*) x,y
          do i=1,nwad
              if(abs(x-tp(i))*xc.lt.10.0.and.   ! distance in pixels
     *           abs(y-tsp(i))*yc.lt.8.0) then
                 txt(1)(1:25)='STAT CO IPHASW HRMM   SEC'
                 txt(2)(1:25)=phase_p(i)(1:25)
                 txt(3)(1:25)=phase_s(i)(1:25)
c                 write(27,*) x,y,stat(i),phase_p(i),phase_s(i)
c                 write(27,*) txt(1),txt(2),txt(3)
              endif
          enddo
          call xmessage(txt,3,25,720.0,650.0)
          if(cha(1).ne.'q') goto 55   ! next input
           
c          call xscursr(i,xm,ym)                

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
      endif
 30   call clear_to_alpha
      write(6,*)' Plot file is wad_plot.eps'
        
      stop
      end
C
c-----------------------------------------------------------------------------
      subroutine wadati_s
     *(data,nhead,nrecord,min_weight,nwad,vps,hour,min,sec,rms,
     *corr,tp,tsp,tor,statt,phase_p,phase_s)
c
c   routine makes wadati analysis of one event, special version for
c   graphics program
c   jh, february 1998
c
c   input
c
c   data,nhead,nrecord: data, number of headers, number of records 
c   min_weight        : smallest weight to use (0,1,2,3)
c   
c   output
c
c   nwad              : number of points used
c   vps               : vp to vs ratio
c   hour,min,sec      : origin time
c   rms               : rms of fit
c   corr              : correlation coefficient of fit
c   tp,tsp            : p and s-p times
c   tor               : arbitrary origin time
c   statt             : station codes
c   phase_p           : P-phase etc
c   phase_s           : S-phase etc
c
      implicit none
      character*80 data(*)
      integer nhead,nrecord,min_weight,nwad
      real vps,t_origin,rms,corr
      real tp(500)   ! p-times
      real tsp(500)  ! s-p times
      character*5 statt(500) ! station codes
      character*30 phase_p(500),phase_s(500)  ! info phases used
      real sec
      real tor       ! origin
      integer timep  ! hours and minutes in secs
      integer hour,min
      integer hourmin,minmin,tmin  ! earliest p
      character*5 stat   ! one station code
      character*2 phase,phases  ! phase
      character*1 phtype ! if not blank, used all
      integer i,k,iw
      
c
      write(6,*) 'Only use same phase type (e.g. PG and SG) n/y=return'
      read(5,'(a)') phtype
c
c
      nwad=0
      tmin=9999999.0
c
c   find pairs of readings, assume same phase, weight can be in column 9 if
c   phase is long
c
      do i=nhead+1,nrecord
        read(data(i)(15:15),'(i1)',err=50) iw     ! read assigned weight
        goto 51
 50     continue
        read(data(i)(9:9),'(i1)') iw              ! read assigned weight
 51     continue
c
c   find p and s-p for wadati
c
          
        if(data(i)(11:11).eq.'P'.and.iw.le.min_weight) then  ! if valid P
           stat=data(i)(2:6)
           phase=data(i)(11:12)  ! p-phase
           if(phtype.ne.' '.and.phtype.ne.'y') phase(2:2)=' ' ! check if same
           phase_p(nwad+1)=' '
           phase_p(nwad+1)(1:14)=data(i)(2:15)
           phase_p(nwad+1)(16:25)=data(i)(19:28)
           phase(1:1)='S'        ! corresponding s-phase
           do k=nhead+1,nrecord
             read(data(i)(15:15),'(i1)',err=60) iw     ! read assigned weight
             goto 61
  60         continue
             read(data(i)(9:9),'(i1)') iw              ! read assigned weight
  61         continue
             phases=data(k)(11:12)
             if(phtype.ne.' '.and.phtype.ne.'y') phases(2:2)=' '! check if same
             if(phases.eq.phase.and.data(k)(2:6).eq.stat.
     *       and.iw.le.min_weight) then  ! same S
                nwad=nwad+1
                phase_s(nwad)=' '
                phase_s(nwad)(1:14)=data(k)(2:15)
                phase_s(nwad)(16:25)=data(k)(19:28)
                read(data(i)(19:28),'(2i2,f6.1)') hour,min,sec
                tp(nwad)=hour*3600.0+min*60.0+sec
                if(tp(nwad).lt.tmin) then
                   hourmin=hour           ! find earliest for scaling
                   minmin=min
                   tmin=tp(nwad)
                endif
                read(data(k)(19:28),'(2i2,f6.1)') hour,min,sec
                tsp(nwad)=hour*3600.0+min*60.0+sec-tp(nwad)
                statt(nwad)=stat
             endif
           enddo
        endif
      enddo
      timep=hourmin*3600+minmin*60  
      do i=1,nwad
        tp(i)=tp(i)-timep    ! make values smaller so better fit in lsq
      enddo

c
c  all readings done, now do wadati analysis
c
      if(nwad.ge.2) then
         call lsqlin(nwad,tp,tsp,t_origin,vps,corr,rms)
         tor=t_origin
         t_origin=-t_origin/vps+timep
         vps=vps+1
         hour=t_origin/3600
         min=(t_origin-hour*3600)/60
         sec=t_origin-hour*3600-min*60
      endif

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine xy_plot_input(x0,y0,c,x,y,xc,yc)
c
c  reads positions from the screen much like the main routine
c  all scaling values from routine xy_plot via common
c  x0,y0 are lower left hand corner of plot
c
c  x0,yo : same input as main routine
c  x,y   : output in scaled units
c  c     : character pressed
c  xc    : xscale
c
       implicit none
       character*1 c
       integer ichar
       reaL xc,yc   ! same as x and yscale
       real x1,x,y1,y   ! help variables
       real xfirst,yfirst,x0,y0,xscale,yscale,ymin,ymax,xmin,xmax   ! see xy_plot
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax
c
c   call up cursor
c

1           continue
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
