
c
c program to plot traces using GMT
c
c by Lars Ottemoeller, June 2001
c
c
c   april 2003  jh : change call to remove dc

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock
      include 'waveform.inc'              ! waveform include
      include 'trace_plot.inc'            ! trace_plot include

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer i,j,nfiles                  ! counters
      character*80 outfile                ! output filename
      character*80 xfile                  ! waveform filename
      integer seiclen                     ! length of a string
      logical trace_flag(max_trace)       ! true if trace selected
      real trace_dist(max_trace)          ! distances in km
      double precision origin_time        ! origin time in seconds
      real max
      integer last_trace                  ! index to the last selected trace
      character*80 script_file            ! name of script
      character*240 longline
      character*80 line
      real      cof(8)                    ! filter coefficients
      real      gain                      ! filter gain
      integer npasses                     ! number of passes, 1 forward,
                                          ! 2 both ways
      real x(3),y(3)                      ! hypocenter and station location
      real degtorad,distdeg,az0
      real dc
      integer ndc


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call get_seisan_def

      call get_traceplot_def
      npasses=1
      degtorad=0.0174533

c
c   open output file
c
      outfile='trace_plot.data'
      script_file='trace_plot.bat'
      open(2,file=outfile,status='unknown')
    
c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give Nordic input filename '
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event form file unit 1
c
      call rea_event_in(1,all,data,code)
      x(1)=hyp_lat(1)*degtorad
      x(2)=hyp_lon(1)*degtorad
      x(3)=hyp_depth(1)

      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &   hyp_hour(1),hyp_min(1),hyp_sec(1),origin_time)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
      nevent=nevent+1               ! count events

c
c read all headers
c
      call wav_init
      nfiles=0
      last_trace=0
      wav_nfiles=0
      do i=1,rea_nwav
         xfile=' '
         xfile=rea_wav(i)(2:79)
         xfile=xfile(1:seiclen(xfile))
         call get_full_wav_name(xfile,wav_filename(i))
         write(*,*) ' waveform file: '//wav_filename(i)
     &       (1:seiclen(wav_filename(i)))
         call read_wav_header(i)
         if(wav_error_message.ne.' ') then
            write(6,'(1x,a)') wav_error_message
         endif
      enddo

c
c select channels
c
      do i=1,max_trace
        trace_flag(i)=.true.
      enddo
c
c get distances
c
      do i=1,wav_nchan
        trace_dist(i)=0.
        call stat_loc(wav_stat(i),data(1)(21:21),y(1),y(2),y(3))
        y(1)=y(1)*degtorad
        y(2)=y(2)*degtorad

        if (y(1).ne.0..and.y(2).ne.0.) then
        call delaz(x(1),x(2),
     &        trace_dist(i),distdeg,az0,y(1),y(2))
        else
          write(*,*) ' location unknown for station: '//wav_stat(i)
          trace_flag(i)=.false.
        endif
   
        if (sfileonly_flag) then        
c
c check if station listed in s-file
c
          j=1
          trace_flag(i)=.false.
          do while(j.le.rea_nphase.and..not.trace_flag(i))
            if (wav_stat(i).eq.rea_stat(j)) then
  
              trace_flag(i)=.true.

            endif
            j=j+1
          enddo
        endif
c
c check if channel selected
c
        if (comp_list_cnt.ne.0) then
          j=1
          trace_flag(i)=.false.
          do while(j.le.comp_list_cnt.and..not.trace_flag(i))
            if (wav_comp(i).eq.comp_list(j)) then
              trace_flag(i)=.true.
            endif
            j=j+1
          enddo
          if (.not.trace_flag(i)) write(*,*)
     &         ' channel not selected '// wav_comp(i)
        else
c
c only vertical channels
c
          if (wav_comp(i)(seiclen(wav_comp(i)):seiclen(wav_comp(i)))
     &      .ne.'Z')
     &         trace_flag(i)=.false.
        endif

        if (trace_dist(i).eq.0.) trace_flag(i)=.false.
        if (trace_dist(i).lt.min_dist) trace_flag(i)=.false.
        if (trace_dist(i).gt.max_dist) trace_flag(i)=.false.
c
c set last trace
c
        if (trace_flag(i)) last_trace=i
      enddo
c
c write out trace data
c
      do i=1,wav_nchan
        if (trace_flag(i).eqv..true.) then
          write(*,*) ' reading ... ' ,i,
     &       ' '//wav_stat(i)//' '//wav_comp(i)
c
c read data
c
          call wav_read_channel(i)
c
c remove DC
c
          ndc=0
          call remove_dc(signal1,wav_nsamp(i),dc,wav_nsamp(i))
c
c filter
c
          if (f_low.ne.0..and.f_high.ne.0.) then
            write(*,*) ' filter ', f_low,f_high
c-- calc. filt. cof.
             call bndpas(f_low,f_high,1000.0/wav_rate(i),
     *              cof,gain)
c-- and filter
             call filter(signal1,wav_nsamp(i),cof,gain,npasses)
          endif

c
c get maximum amplitude
c
          max=-1.
          do j=1,wav_nsamp(i)
            if (wav_abs_time(i)+(j-1)/wav_rate(i).ge.
     &        origin_time+min_time.and.
     &        wav_abs_time(i)+(j-1)/wav_rate(i).le.
     &        origin_time+max_time) then

                if (abs(signal1(j)).gt.max) max=abs(signal1(j))

            endif
          enddo
          if (max.eq.0.) max=1.

c
c write data within selected time interval
c
          do j=1,wav_nsamp(i)

            if (wav_abs_time(i)+(j-1)/wav_rate(i).ge.
     &          origin_time+min_time.and.
     &          wav_abs_time(i)+(j-1)/wav_rate(i).le.
     &          origin_time+max_time) then
              write(2,'(2f15.5)') 
     &          wav_abs_time(i)+(j-1)/wav_rate(i)-origin_time,
     &          (signal1(j)/max*amp_scale)+trace_dist(i)
            endif
          enddo
          if (i.le.last_trace) write(2,'(a)') '>'
        endif
      enddo


c
c   get next event
c
      goto 50
c
c     end of file
c
 1000 continue

      write(6,*)            ! blank line
      close(2)              ! close output file

c
c write script file
c
      open(2,file=script_file,status='unknown')
      do i=1,gmt_set_cnt
        write(2,'(a)') set_gmt_def(i)(1:seiclen(set_gmt_def(i)))
      enddo

      longline='psxy trace_plot.data -JX15/25 -X2.5 -Y2.5 -P -W1 -M '

      line=' '
      write(line,'(a,f5.0,a,f5.0,a)') 
     & ' -B:Time:a',(max_time-min_time)/10.,
     & '/:Distance:a',(max_dist-min_dist)/15.,'sWNe '
      do i=2,seiclen(line)
        if(line(i:i).eq.' ') line(i:i)='0'
      enddo
      longline=longline(1:seiclen(longline))//line(1:seiclen(line))

      line=' '
      write(line,'(a4,3(f6.0,a1),f6.0)') ' -RX',min_time,'/',
     &  max_time,'/',min_dist,'/',max_dist
      do i=2,seiclen(line)
        if(line(i:i).eq.' ') line(i:i)='0'
      enddo
      longline=longline(1:seiclen(longline))//line(1:seiclen(line))
     &  //' > trace_plot.ps '
      write(2,'(a)') longline(1:seiclen(longline))
      close(2)
      line='chmod u+x '//script_file(1:seiclen(script_file))
      write(*,'(a)') line(1:seiclen(line))
      call systemc(line,seiclen(line))
      line='./'//script_file(1:seiclen(script_file))
      write(*,'(a)') line(1:seiclen(line))
      call systemc(line,seiclen(line))
c      line='gv trace_plot.ps'
c      write(*,'(a)') line(1:seiclen(line))
c      call systemc(line,seiclen(line))

      write(6,*) 'Number of events in input file', nevent

      stop
      end

      subroutine get_traceplot_def
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
      include 'trace_plot.inc' 
      real var                      ! parameter values
      character*80 def_file         ! file names
      integer in                    ! file unit
      integer code                  ! return code from opening def file
      integer i                     ! counter
      character*80 line             ! text line


c
c init values
c
      f_low=0.
      f_high=0.
      min_dist=0.
      max_dist=1000.
      min_time=0.
      max_time=1500.
      amp_scale=10.
      comp_list_cnt=0
      sfileonly_flag=.false.
c
c GMT defaults
c
      gmt_set_cnt=4
      set_gmt_def(1)='gmtset PAPER_MEDIA A4+'
      set_gmt_def(2)='gmtset ANOT_FONT_SIZE 10'
      set_gmt_def(3)='gmtset LABEL_FONT_SIZE 12'
      set_gmt_def(4)='gmtset TICK_LENGTH 0.1c'

      def_file = 'trace_plot.par'

      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   def_file )       ! For this filename.

      if(code.ne.e_ok$) then
        write(*,*) ' definition file does not exist: trace_plot.par'
        return
      endif

10    continue

c
c read text line from file, check for code and set variables
c
      read(in,'(a)',end=100,err=100) line

      if (line(1:6).eq.'FILTER') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          f_low=var
        endif
        read(line(51:60),'(f10.1)') var
        if (var.ne.0.) then
          f_high=var
        endif
      elseif (line(1:5).eq.'TIME') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          min_time=var
        endif
        read(line(51:60),'(f10.1)') var
        if (var.ne.0.) then
          max_time=var
        endif
      elseif (line(1:8).eq.'DISTANCE') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          min_dist=var
        endif
        read(line(51:60),'(f10.1)') var
        if (var.ne.0.) then
          max_dist=var
        endif
      elseif (line(1:15).eq.'AMPLITUDE_SCALE') then
        read(line(41:50),'(f10.1)') var
        if (var.ne.0.) then
          amp_scale=var
        endif
      elseif (line(1:18).eq.'STATION_SFILE_ONLY') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) then
          sfileonly_flag=.true.
        endif
      elseif (line(1:9).eq.'COMPONENT') then
        if (line(41:44).ne.' ') then
          comp_list_cnt=comp_list_cnt+1  
          comp_list(comp_list_cnt)=' '
          comp_list(comp_list_cnt)=line(41:44)
        endif
      endif
      goto 10
      
100   continue

      call sei close( close$, in, code )


      return
      end
