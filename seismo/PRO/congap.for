
      program congap
c
c program to check completeness of continuous data,
c all input is read from congap.par
c
c
c Lars Ottemoller, BGS, July 2008
c

c
c changes:
c
c   15/05/2009 lo - count gap at end of trace, fixed problem with trace that was all 0
c   05/06/2010 wc - changes to init wav and output
c   05/11/2013 lo - fix bug related to potential gap at the end of file

      implicit none 

c
c read SEISAN include files
c
      include 'seidim.inc'   ! dimensions
      include 'seisan.inc'   ! general seisan parameters
      include 'libsei.inc'
      include 'waveform.inc' ! for waveform handling
      include 'rea.inc'      ! for waveform handling
      integer seiclen

      character*80 outfile   ! name of output seisan  waveform file
      character*80 monthfile ! name of output seisan  waveform file
      character*80 text  
      character*1040 theader ! seisan channel header
      character*80 fheader(max_trace) ! seisan waveform main header
      character*29 headtext
      character*5 net_code   ! code of network
      real duration_min      ! minute
      integer i,j,l,k,c      ! counter
      integer maxstat,maxtrig
      parameter (maxstat=100)
      character*5 station(maxstat),cbase(maxstat)
      character*4 component(maxstat)
      integer ind
      double precision msec_start,msec_stop,msec,trigger_msec,msec1
      character*14 stop_time,start_time,time
      integer year1,month1,day1,hour1,min1,isec1,doy1
      integer year2,month2,day2,hour2,min2,isec2,doy2
      real sec1,sec2
      integer write1,write2,write3,read1
      logical b_flag
      integer code
      integer nchan,nbase
      integer narg
      character*80 arg(40)
      real rate
      integer nz,nzt
      logical gap_flag,flag,skip
      real expected_time(max_trace)
      real actual_time(max_trace),atime
c      logical multi
      character*80 sys
      logical overwrite    ! true if output files should be overwritten
      character*5 arcstat    ! station selected to be read from archive
      character*5 arcdef(maxstat)   ! list of stations defined as ARC channels
      logical onestation 
      character*5 name ! used in output filename
c
c init
c
c      multi=.false.
      nbase=0
      start_time=' '
      stop_time=' '
      overwrite=.false.
      duration_min=60.
      onestation=.false.

c  initilize memory handling	WCC added 6/2010
      call wav_mem_init
c

c   get seisan defaults including names of continuous waveform data bases
c   and archive data base
c
      call get_seisan_def 
c
c get arguments
c
      call get_arguments(narg,arg)
      do j=1,narg
        if (arg(j).eq.'-start') then   
          start_time=arg(j+1)
        elseif (arg(j).eq.'-stop') then   
          stop_time=arg(j+1)
        elseif (arg(j).eq.'-cbase') then
          cbase(1)=arg(j+1)
          nbase=1
        elseif (arg(j).eq.'-overwrite') then
          overwrite=.true.
        elseif (arg(j).eq.'-interval') then
           call sei get values(1,arg(j+1), code )
           duration_min=array$(1)
        elseif (arg(j).eq.'-arc') then
          arc=.true.
          arcstat=arg(j+1)
          if (arcstat.ne.'def') then
            onestation=.true.
          endif
          cbase(1)='arc'
          nbase=1
c          write(*,*) ' debug archive mode '
        endif
      enddo
      if (start_time.eq.' '.or.stop_time.eq.' '.or.
     &   duration_min.eq.0..or.nbase.eq.0) 
     &call get_contdet_def(nbase,cbase,start_time,stop_time,
     &   duration_min)

c
c init variables 
c
      if (cbase(1).eq.' ') then
        write(*,*) ' Name of continuous database '
        read(5,'(a)') cbase(1)
      endif

      if (start_time.eq.' ') then
        write(*,*) ' Start time (yyyymmdd...) '
        read(5,'(a)') start_time
      endif

      if (stop_time.eq.' ') then
        write(*,*) ' Stop time (yyyymmdd...) '
        read(5,'(a)') stop_time
      endif

      if (duration_min.eq.0.) then
        write(*,*) ' Duration in min '
        read(5,*) duration_min
      endif

      if (station(1).eq.' ') then
        write(*,*) ' Station '
        read(5,'(a)') station(1)
      endif

      if (component(1).eq.' ') then
        write(*,*) ' Component '
        read(5,'(a)') component(1)
      endif

      write(*,*) ' start time: ',start_time
      write(*,*) ' stop time:  ',stop_time

      do i=1,max_trace
        expected_time(i)=0.
        actual_time(i)=0.
      enddo
c
c read databases if cbase(1) is def
c
      if (.not.arc) then
        if (cbase(1).eq.'def') then
          nbase=0
c        multi=.true.
          do k=1,n_cont_base
            if (cont_base_def(k).eq.1.) then
              nbase=nbase+1
              cbase(nbase)=cont_base(k)
            endif
          enddo
          write(*,*) ' continuous databases to process: ',nbase 
        endif
      else 
c set arc databases, only the one for arcstat if not set to def
          nbase=0
          do k=1,arc_nchan
            ind=0
            do l=1,nbase
              if (arc_stat(k).eq.arcdef(l).and.
     &            ind.eq.0) then
                ind=l
              endif
            enddo
            if (ind.eq.0) then
              if (arcstat.eq.'def'.or.
     &            arc_stat(k).eq.arcstat) then
                nbase=nbase+1
                arcdef(nbase)=arc_stat(k) 
c                write(*,*) ' debug ',nbase,arcdef(nbase)
              endif
            endif
          enddo
      endif
c copy arc bases to arc_stat if not all archives to be read
      if (arc.and.arcstat.ne.'def') then
        c=0
        do k=1,arc_nchan
          ind=0
          do l=1,nbase
            if (arc_stat(k).eq.arcdef(l)) then
              ind=l
              c=c+1
              arc_stat(c)=arc_stat(k)
              arc_comp(c)=arc_comp(k)
              arc_net(c)=arc_net(k)
              arc_loc(c)=arc_loc(k)
            endif
          enddo
        enddo
        arc_nchan=3
        write(*,*) ' number of arc channels selected: ',arc_nchan
      endif
c
c loop over stations
c
      nchan=0
      write1=0  ! moved here lo 6/7/2010

      do k=1,nbase
c
c init
c
        if (arc) then
          write(*,*) ' Arc station: ',arcdef(k)
          cont_base(1)=arcdef(k)
          name=arcdef(k)
        else
          write(*,*) ' Database: ',cbase(k)
          cont_base(1)=cbase(k)
          name=cbase(k)
        endif
        n_cont_trace=0


c
c  signal that reading is from a continous data base
c
        if (.not.arc) cwav=.true.
        n_cont_base=1
        cseed=.false.
c
c   calculate end time and extended start time, needed in order to
c   make sure enough files read in
c
        cwav_start_time=start_time

        read(start_time,'(i4,5i2)') year1,month1,day1,hour1,min1,isec1
        sec1=float(isec1)
        call timsec(year1,month1,day1,hour1,min1,sec1,msec_start)

c 
c compute overall endtime
c
        read(stop_time,'(i4,5i2)') year2,month2,day2,hour2,min2,isec2
        sec2=float(isec2)
        call timsec(year2,month2,day2,hour2,min2,sec2,msec_stop)

c
c set initial values
c
        call cwav_time_limits(0)
        cwav_abs_start_time=msec_start
        cont_interval=duration_min*60.

        do while(cwav_abs_start_time.lt.msec_stop)
c
c check if monthly output file exists, skip if so
c
          skip=.false.
          if (.not.overwrite) then
            monthfile=' '
            write(monthfile,'(a,"_",i4.4,"_",i2.2,a)') 
     &      cont_base(1),year1,month1,'_congap.out'
            do i=1,seiclen(monthfile)
              if (monthfile(i:i).eq.' ') monthfile(i:i)='_'
            enddo
            inquire(file=monthfile,exist=flag)
            if (flag) then
              day1=1
              hour1=0
              min1=0
              sec1=0.
              isec1=0
              if (month1.le.11) then
                month1=month1+1
              else
                month1=1
                year1=year1+1
              endif
		write(*,*) year1,month1,day1,hour1,min1,sec1,msec_start
              call timsec(year1,month1,day1,hour1,min1,sec1,msec_start)
              cwav_abs_start_time=msec_start
              skip=.true.
              write(*,*) ' file exists, no overwrite '//
     &           monthfile(1:seiclen(monthfile))
            endif
          endif
c
c open files
c
          if (write1.eq.0.and..not.skip)
     &      call sei open( unknown$,          ! Open old file.
     &               ' ',               ! No prompt.
     &               'congap.out',    ! This filename.
     &               write1,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)

c
c set end of interval to be read
c
          call cwav_time_limits(1)
c
c  read the header information for all files in bases in time
c  interval, assume info available in common block
c
          cwav_abs_end_time=cwav_abs_start_time+cont_interval
          if (skip) goto 99
          
          if (.not.arc) then
            call cwav_read_bases     
          else
c
c   read from archive
c
            call wav_read_arc_headers
c
c   stop if no data
c
            if(wav_nchan.eq.0) then
               write(6,*) ' No data, will stop'
               n_cont_trace=0
c               stop
            else
               write(6,*) ' Number of archive channels with data:',
     *         wav_nchan
               n_cont_trace=wav_nchan
            endif
          endif

          call sectim(cwav_abs_start_time,year1,doy1,month1,day1,hour1,
     &    min1,sec1)
          call sectim(cwav_abs_end_time,year2,doy2,month2,day2,hour2,
     &    min2,sec2)
          write(*,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' start time: ',year1,month1,day1,hour1,min1,sec1
          write(*,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' stop time:  ',year2,month2,day2,hour2,min2,sec2
          write(*,*)
     & ' chan# | total samps | samp rate |  duration |      gap'
          write(*,*) 
     & '-------+-------------+-----------+-----------+---------'
c
c find channel index
c
          do i=1,n_cont_trace
            if (arc) then
              if (wav_stat(i).ne.arcdef(k).and.
     &          arcstat.ne.wav_stat(i)) goto 999
              cwav_stat(i)=wav_stat(i)
              cwav_comp(i)=wav_comp(i)
            endif
            ind=0
            do j=1,nchan
              if ((cwav_stat(i).eq.station(j).and.
     &            cwav_comp(i).eq.component(j)))
     &        then
                ind=j
              endif
            enddo
c new channel
            if (ind.eq.0) then
              nchan=nchan+1
              ind=nchan
              station(nchan)=cwav_stat(i)
              component(nchan)=cwav_comp(i)
            endif
c
c count expected time
c
            expected_time(ind)=expected_time(ind)+cont_interval 

c
c  read the waveform data, one trace at a time
c
            call wav_read_channel(i)    ! read trace


            if (arc) wav_nsamp(i)=wav_nsamp(i)-1  ! removed 12/08/2008
            rate = wav_rate(i)
            if (wav_nsamp(i).lt.0.or.wav_nsamp(i).gt.1e10) then
              wav_nsamp(i)=0
            endif
c
c check for gap, which has constant level for more than 1 s
c
            gap_flag=.false.
            nz=0
            nzt=0
            do j=2,wav_nsamp(i)
              if (signal1(j).eq.signal1(j-1)) then
                nz=nz+1
c                if (.not.gap_flag) then
c                  write(*,*) ' gap, start sample ',j
c                  write(*,*) signal1(j),signal1(j-1)
c                endif
                gap_flag=.true.
              else
                if (gap_flag) then
                  if (float(nz)/rate.gt.1.) then
                    nzt=nzt+nz+1
c                    write(*,*) ' gap, end sample  ',j,nz
                  endif
                endif
                nz=0
                gap_flag=.false.
              endif
            enddo
c
c case of gap at the end
c
            if (float(nz)/rate.gt.1.) then
              nzt=nzt+nz+1
            endif
c            write(*,*) ' nzt ',nzt

c            rate = cwav_rate(i,cwav_nseg(i))

            write(*,'(i7," | ",i11," | ",f9.2," | ",f9.2," | ",f8.2)')
     &           i,wav_nsamp(i),rate,float(wav_nsamp(i))/rate,
     &           float(nzt)/rate
!           write(*,*) ' channel number: ',i
!            write(*,*) ' total samples: ',wav_nsamp(i)
!            write(*,*) ' sampling rate: ',rate
!            write(*,*) ' duration: ',float(wav_nsamp(i))/
!     &                               rate
!            write(*,*) ' gap duration: ',float(nzt)/rate
            if (rate.gt.0.) then
              atime=float(wav_nsamp(i)-nzt)/rate
            else 
              atime=0.
            endif
            actual_time(ind)=actual_time(ind)+atime
             write(write1,
     &       '(a5,1x,a3,1x,i4,2i2.2,1x,2i2.2,f5.2,1x,f9.2,1x,f9.2)')
     &        station(ind),component(ind)(1:2)//component(ind)(4:4),
     &        year1,month1,day1,hour1,min1,sec1,atime,cont_interval
999         continue
          enddo ! loop over traces
c
c rename files if end of month or year, or end of time interval
c 
          if (month2.gt.month1.or.
     &        year2.gt.year1.or.
     &        cwav_abs_start_time+cont_interval.ge.msec_stop) then
c
c close files
c
            call sei close( close$, write1, code )
            write1=0
            sys=' '
            write(sys,'(a,a,"_",i4.4,"_",i2.2,a)') 
     &         'mv congap.out ',
     &         name,year1,month1,'_congap.out'
            do i=15,seiclen(sys)
              if (sys(i:i).eq.' ') sys(i:i)='_'
            enddo
            write(*,*) sys
            call systemc(sys,seiclen(sys))
          endif
c
c set start of next interval to read
c
99        continue
          cwav_abs_start_time=cwav_abs_end_time
        enddo ! loop over time
      enddo ! end of loop over stations
c      if (.not.multi) call sei close( close$, write1, code )

      write(*,'(a)') ' -------------------------------------------- '
      write(*,'(a)') '  # stat comp       actual     expected     % '
      write(*,'(a)') ' -------------------------------------------- '
      do i=1,nchan
        write(*,'(i3,1x,a5,1x,a3,1x,f12.2,1x,f12.2,1x,f5.1)') 
     &      i,station(i),
     &      component(i)(1:2)//component(i)(4:4),
     &      actual_time(i),expected_time(i),
     &      actual_time(i)/expected_time(i)*100.
      enddo
      write(*,'(a)') ' -------------------------------------------- '
     
      stop
      end

      subroutine get_contdet_def(nbase,cbase,startdate,stopdate,
     &   duration)

      implicit none
      include 'libsei.inc'
      include 'seidim.inc'

      character*80 def_file         ! file names
      integer in                    ! file unit
      integer code                  ! return code from opening def file
      integer i                     ! counter
      character*80 line             ! text line
      real var                      ! value of variables
      integer nbase                 ! station counter
      integer seiclen

      character*(*) cbase(*),startdate,stopdate
      real duration
   
c
c check for file end open
c

      def_file = 'congap.par'

      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   def_file )       ! For this filename.

      if(code.ne.e_ok$) then
        write(*,*) ' definition file does not exist: congap.par'
        stop
      endif
c
c init values
c
      cbase(1)=' '
      startdate=' '
      stopdate=' '
      duration=60.
      nbase=0  ! station counter


100   continue

c
c read text line from file, check for code and set variables
c
      read(in,'(a)',end=300,err=200) line

      if (line(1:10).eq.'START DATE') then
        read(line(41:54),'(a)') startdate
      elseif (line(1:9).eq.'STOP DATE') then
        read(line(41:54),'(a)') stopdate
      elseif (line(1:9).eq.'CONT BASE') then
        nbase=nbase+1
        read(line(41:45),'(a)') cbase(nbase)
      elseif (line(1:8).eq.'INTERVAL') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) duration=var
      endif

      goto 100

200   continue
      write(*,*) ' Error in congap.par file'

300   continue
      call sei close( close$, in, code )

      return
      end

