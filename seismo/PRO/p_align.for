c--------------------------------------------------------------------------
c  p_align.for
c  This program timeshift data from P-phase arrival time to the psudo time 
c  2070-11-27 12:00
c  Input is a nordic file (e.g. select.out) and a station name. 
c  Output is timeshifted waveform files and a file tsd.out with timeshifted 
c  phase readings and names of the new waveform files.
c
c  TODO: timeshift tsd.out back in time with new phase readings
c        read+write miniseed 
c
c  2008-06-19 pv : good luck 
c  2008 06 25 jh : code to code1
c  2010-07-30 pv : khead fixed to 12, input files with khead>12 failed.
c  2011-12-05 jh : change sn to ain
c  2014-12-11 pv : added S phase and fixed for many wav files in each sfile
c  2015-06-02 pv : add signal_int to signalx common block due to
c                  compiler warning
c--------------------------------------------------------------------------c
 
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block input files
      include 'rea2.inc'                  ! parameter common block output file

      character*80 data(5000)             ! s-file with data in text array
      character*80 data2(5000)             ! s-file with data in text array
      character*80 infile                 ! input file
      character*80 wavfile                 ! input file
      character*80 wavfile_old_stat
      character*80 outfile
      character*80 text      ! general text string
      character*5 old_stat, new_stat      ! station code for input and output station
      character*5 stat(max_trace)  ! staions in header
      character*4 comp(max_trace)  ! components in header
      character*80 mainhead(max_trace)
      character*1040 chahead
      character*12 time1                      ! time strings
      character*14 time2
      character*1 ts_phase 

      real cinter(max_trace),cstart(max_trace) ! start and interval each trace

      logical shift_pphase
      logical all                         ! true: read all data, false: headers
      logical verbose
      logical wav_found
      logical data_found
      logical phase_found
      integer code1                       ! error return code
      integer nevent                      ! number of events in file
      integer i,j,k                       ! counter
      integer khead           ! number of main headers
      integer pindex                      ! the rea_phase number where P-phase is found
      integer nchan
      integer nstat
      integer nsamp
      integer nnewchan
      integer newfileno
      integer*4 data4i(max_sample) ! samples
      integer*2 data2i(2*max_sample)   ! samples

c--- time and dates
      integer year,month,day,hour,min,year1,month1,day1,hour1,min1
      integer doy,doy1
      real sec, sec1
      double precision msec

      double precision shift_time
      double precision dummy

c
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code,                  ! Error encoder.
     &          sei real num               ! Get real number
       integer  seiclen
C
       integer
     &          read02,                    ! Ditto 2.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       real sei real num                   ! Real number
C
C    ============= end of list ==========
      real x(max_sample)
      integer signal_int(max_sample)
c     common /signalx/x
      common /signalx/x,signal_int

       verbose=.TRUE.
c
c   open output file

       open(3,file='tsd.out',status='unknown')
    
c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
      shift_pphase=.true.   
      nevent=0                    ! initialize counter
      rea2_ncomment=0
      rea2_nwav=0
      rea2_nphase=0
      rea2_nfault=0
      rea2_nmacro=0
      rea2_nspec=0
      rea2_av_moment=0.0
      rea2_av_omega0=0.0

      old_stat="     "
      new_stat="     "
      write(6,*) 'Enter station code to be time shifted:'
      read(5,'(a)') old_stat
      write(6,*) 'Enter phase name (e.g. P/S) to be time shifted:'
      read(5,'(a)') ts_phase
c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event form file unit 1
c
      call rea_event_in(1,all,data,code1)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code1.eq.1) goto 1000
c
      nevent=nevent+1               ! count events
c     
c----------------------------------------------------
      new_stat(1:3)=old_stat(1:3)
      if(nevent.le.9) then
        new_stat(4:4)="0"
        new_stat(5:5)=char(nevent+48)
c       new_stat(5:5)=char(nevent+65)
      elseif(nevent.ge.10) then
        i=int(nevent/10.)
        new_stat(4:4)=char(i+48)
        new_stat(5:5)=char(nevent-i*10+48)
c       new_stat(4:4)=char(i+65)
c       new_stat(5:5)=char(nevent-i+65)
c     elseif(nevent.ge.100)stop
      endif
      if(verbose) write(6,*) "Event #",nevent,
     &" time shifting station : ",old_stat

c----------------------------------------------------
c  look for selected phase in s-file
      pindex=-1
      phase_found=.FALSE.
      do i=1,rea_nphase
        if(rea_stat(i).eq.old_stat)then
c         if(rea_phase(i)(1:1).eq.'P') then
          if(rea_phase(i)(1:1).eq.ts_phase) then
c           If more than one P/S (e.g. Pn and Pg) select the first:
            if(.not.phase_found) pindex=i
            phase_found=.TRUE.
          endif
        endif
      enddo
c
c----------------------------------------------------
c
c time shift with respect to p-phase time
c
      if(shift_pphase)then
        if(pindex.eq.-1)then
          write(6,*)" NO P-PHASE FROM STATION ",old_stat,
     &" IN S-FILE - STOP !!"
          write(6,*)" Add p-phase with weight 4 and try again."
          stop
        else
        call timsec(2070,11,27,12,00,0.0,dummy)
        call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     *rea_hour(pindex),rea_min(pindex),rea_sec(pindex),msec)
c       write(6,*),hyp_year(1)
c       write(6,*),hyp_month(1)
c       write(6,*),hyp_day(1)
c       write(6,*),rea_hour(pindex)
c       write(6,*),rea_min(pindex)
c       write(6,*),rea_sec(pindex)
        shift_time=dummy-msec
        endif
      endif
c
c----------------------------------------------------
c
c  Add phases from old_stat to new s-file
c
      do i=1,rea_nphase
        if(rea_stat(i).eq.old_stat)then
          call timadd(hyp_year(1),hyp_month(1),hyp_day(1),
     *     rea_hour(i),rea_min(i),rea_sec(i),shift_time,
     *     year1,month1,day1,hour1,min1,sec1)  ! add correction
          rea2_nphase=rea2_nphase+1
c         rea2_stat(rea2_nphase)=rea_stat(i)(1:5)
          rea2_stat(rea2_nphase)=new_stat(1:5)
          rea2_comp(rea2_nphase)=rea_comp(i)(1:4)
          rea2_co(rea2_nphase)=rea_co(i)(1:2)
          rea2_phase(rea2_nphase)=rea_phase(i)(1:8)
          rea2_onset(rea2_nphase)=rea_onset(i)(1:1)
          rea2_weight_in(rea2_nphase)=rea_weight_in(i)(1:1)
          rea2_weight_out(rea2_nphase)=rea_weight_out(i)(1:2)
          rea2_polarity(rea2_nphase)=rea_polarity(i)(1:1)
          rea2_year(rea2_nphase)=rea_year(i)
          rea2_month(rea2_nphase)=rea_month(i)
          rea2_day(rea2_nphase)=rea_day(i)
c         rea2_hour(rea2_nphase)=rea_hour(i)
          rea2_hour(rea2_nphase)=hour1
c         rea2_min(rea2_nphase)=rea_min(i)
          rea2_min(rea2_nphase)=min1
c         rea2_sec(rea2_nphase)=rea_sec(i)
          rea2_sec(rea2_nphase)=sec1
          rea2_abs_time(rea2_nphase)=rea_abs_time(i)
          rea2_coda(rea2_nphase)=rea_coda(i)
          rea2_amp(rea2_nphase)=rea_amp(i)
          rea2_per(rea2_nphase)=rea_per(i)
          rea2_baz_obs(rea2_nphase)=rea_baz_obs(i)
          rea2_baz_cal(rea2_nphase)=rea_baz_cal(i)
          rea2_vel(rea2_nphase)=rea_vel(i)
          rea2_ain(rea2_nphase)=rea_ain(i)
          rea2_baz_res(rea2_nphase)=rea_baz_res(i)
          rea2_res(rea2_nphase)=rea_res(i)
          rea2_dist(rea2_nphase)=rea_dist(i)
          rea2_az(rea2_nphase)=rea_az(i)
          rea2_auto(rea2_nphase)=rea_auto(i)(1:20)
        else
            rea_phase(i)(1:6)='DELETE'
        endif
      enddo

c----------------------------------------------------
c
      rea2_ncomment=rea2_ncomment+1
      rea2_comment(rea2_ncomment)(1:40)=
     *' TSD:                                   '
      rea2_comment(rea2_ncomment)(41:80)=
     *'                                       3'
c     rea2_comment(rea2_ncomment)(1:79)=data(1)(1:79)
      rea2_comment(rea2_ncomment)(6:70)=data(1)(1:65)
      rea2_comment(rea2_ncomment)(64:64)=' '
      rea2_comment(rea2_ncomment)(65:69)=old_stat(1:5)
      rea2_comment(rea2_ncomment)(70:79)='          '

      rea2_ncomment=rea2_ncomment+1
      rea2_comment(rea2_ncomment)(1:40)=
     *' TSD:                                   '
      rea2_comment(rea2_ncomment)(41:80)=
     *'                                       3'
      rea2_comment(rea2_ncomment)(7:11)=old_stat(1:5)
      rea2_comment(rea2_ncomment)(13:17)=new_stat(1:5)
      write(rea2_comment(rea2_ncomment)(20:34),'(f15.4)')shift_time


c----------------------------------------------------
      rea2_nhyp=1
c     rea2_nhead=1
      hyp2_year(1)=2070
      hyp2_month(1)=11
      hyp2_day(1)=27
      hyp2_hour(1)=12
      hyp2_min(1)=0
      hyp2_sec(1)=0.0
      hyp2_model(1)=" "
      hyp2_dist_id(1)=hyp_dist_id(1)
      hyp2_type(1)=hyp_type(1)
      hyp2_fix_org(1)=hyp_fix_org(1)
      hyp2_lat(1)=hyp_lat(1)
      hyp2_lat(1)=-999.0
      hyp2_lon(1)=hyp_lon(1)
      hyp2_lon(1)=-999.0
      hyp2_depth(1)=hyp_depth(1)
      hyp2_depth(1)=-1.0
      hyp2_depth_flag(1)=hyp_depth_flag(1)
      hyp2_epi_flag(1)=hyp_epi_flag(1)
      hyp2_agency(1)=old_stat(1:3)
      hyp2_nstat(1)=0
      hyp2_rms(1)=-1.0
      hyp2_high_accuracy(1)=.FALSE.
      hyp2_error(1)=.FALSE.
c
      do i=1,6
        hyp2_mag_type(i,1)=hyp_mag_type(i,1)
        hyp2_mag_agency(i,1)=hyp_mag_agency(i,1)
      enddo
      hyp2_mag(1,1)=-1000.
      hyp2_mag(2,1)=-1000.
      hyp2_mag(3,1)=-1000.
      hyp2_mag(4,1)=-1000.
      hyp2_mag(5,1)=-1000.
      hyp2_mag(6,1)=-1000.
c     (1)=(1)
c     (1)=(1)
c     data2(1)(1:80)=data(1)(1:80)
c     rea2_id_line(1:80)=rea_id_line(1:80)
      call systime(time1,time2)
      rea2_id_line=' ACTION:TSD '//time2//' OP:     '//
     &       'STATUS:               ID:20701127120000     I'
c----------------------------------------------------

c   write on screen a bit info on event, number of headers and
c   total number of records
c
c     write(6,*)' Number of headers and number of records',
c    *rea_nhead,rea_nrecord

c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)
c----------------------------------------------------
c check wav files:
      wavfile_old_stat=' '
      do j=1,rea_nwav
              wavfile(1:78)=rea_wav(j)(2:79)
c
c  check if file exists
c
              call  get_full_wav_name(wavfile,text)
              if(text.eq.' ') then
                 write(6,'(a,a)')
     *           ' File does not exist : ',wavfile(:seiclen(wavfile))
                 wav_found=.FALSE.
              else
c                write(6,'(a,a)')
c    *           ' Full path name      : ', text(:seiclen(text))
                 wavfile=text(:seiclen(text))
                 wav_found=.TRUE.
              endif

c
c check if wavfile has been procesed before
c
      if(wav_found)then
        if(wavfile(:seiclen(wavfile)).eq.
     *wavfile_old_stat(:seiclen(wavfile))) wav_found=.FALSE.
      endif

c if wav file is found check file for old_stat data 
c
      if(wav_found)then
        wavfile_old_stat(:seiclen(wavfile))=wavfile(:seiclen(wavfile))
      
          chr_f_access$='direct'
          f_recl$=2048
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   wavfile,          ! Filename.
     &                   read02,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
      if(verbose)write(6,'(a,a)') ' Reading file name   : ',
     *wavfile(1:55)
       call seisinc                                                             
     *(read02,0,nchan,0,mainhead,chahead,0.0,0.0)

c
c   read header channel names, might be needed
c
      nstat=1
      do i=3,(nchan-1)/3+3
            read(mainhead(i),'(3(1x,2a4,a1,f7.2,1x,f8.2))')
     *      stat(nstat)(1:4),  comp(nstat),  stat(nstat)(5:5),  
     *      cstart(nstat),cinter(nstat),
     *      stat(nstat+1)(1:4),comp(nstat+1),stat(nstat+1)(5:5),
     *      cstart(nstat+1),cinter(nstat+1),     
     *      stat(nstat+2)(1:4),comp(nstat+2),stat(nstat+2)(5:5),
     *      cstart(nstat+2),cinter(nstat+2)      
            nstat=nstat+3                                
      enddo
c
c Check if Station is in wavform file otherwise goto next file
c
c     skip_wav_file=.TRUE.
      data_found=.FALSE.
      nnewchan=0
      do i=1,nchan
        if(stat(i)(1:5).eq.old_stat(1:5))then
          data_found=.TRUE.
          nnewchan=nnewchan+1
        endif
      enddo

      if(data_found)
     *    write(6,*)"Found wavefrom data - ",
     *old_stat," is renamed to : ",new_stat
c
c     write(6,'(a)') 'mainhead : ',mainhead(1:12)(1:80)
c     write(6,'(a11,1x,5i)') 'nstat    : ',nstat
c     write(6,'(a11,1x,5i)') 'nchan    : ',nchan

      if(data_found) then
        call newheader(mainhead,nnewchan,shift_time,old_stat,new_stat)
c       call newfilename(text,mainhead(1)(1:80),new_stat,nnewchan) 
        newfileno=newfileno+1
        call newfilename(text,mainhead(1)(1:80),new_stat,newfileno)

        outfile=text(:seiclen(text))
      rea2_nwav=rea2_nwav+1
      rea2_wav(rea2_nwav)='                                        '
     &//'                                       6'
      write(rea2_wav(rea2_nwav)(2:1+seiclen(outfile)),'(a)')
     &outfile(1:seiclen(outfile))
        open(2,file=outfile,status='unknown',form='unformatted')
          if(verbose)
     *    write(6,'(a,a)')" Output file name    : ",
     *    outfile(1:seiclen(outfile))
c
c   write main header
c
c        khead=(nchan-1)/3+3
c        if(khead.lt.12) khead=12    ! never less than 12 main headers
         khead=12    ! fixed to 12 since one station will not need more
         do i=1,khead
            write(2) mainhead(i)
c           write(6,*) mainhead(i)
         enddo
c
c   read each channel, fix time, channel names and rewrite
c
         do i=1,nchan            ! fix headers in all channels
c           write(6,*) i
            call seisinc
     *      (read02,i,nchan,2,mainhead,chahead,0.0,0.0)
         if(chahead(1:5).eq.old_stat) then
            read (chahead(10:35),'(i3,1x,i3,1x,4(i2,1x),f6.3)')
     *      k,doy,month,day,hour,min,sec
            year=k+1900
            call timadd(year,month,day,hour,min,sec,shift_time,
     *             year1,month1,day1,hour1,min1,sec1)  ! add correction
            call date_doy(doy1,day1,month1,year1)
            k=year1-1900
            write (chahead(1:5),'(a5)')new_stat(1:5)
            write (chahead(10:35),'(i3,1x,i3,1x,4(i2,1x),f6.3)')
     *      k,doy1,month1,day1,hour1,min1,sec1
c
            read(chahead(44:50),'(i8)') nsamp
c
            write(2) chahead   ! write header
            if(chahead(77:77).eq.'4') then
               do k=1,nsamp
                  data4i(k)=x(k)
               enddo
               write(2)(data4i(k),k=1,nsamp)
            else
               do k=1,nsamp
                  data2i(k)=x(k)
               enddo
               write(2)(data2i(k),k=1,nsamp)
            endif
         endif
      enddo                 ! do i=1,nchan
         close(2)



      endif

      call sei close(close$,read02,code1)
      endif                 ! if(wav_found)then
      enddo                 ! do j=1,rea_nwav
      goto 50
c
 1000 continue
c
c
c     do i=1,rea2_nphase
c       write(6,'(i2,1x,a5,1x,a2,1x,2i2,f6.1,1x,a8,1x,
c    *1x,i4,1x,i2,1x,i2,1x,f15.3,1x)')
c    *      i,rea2_stat(i),rea2_co(i),rea2_hour(i),
c    *        rea2_min(i),rea2_sec(i),rea2_phase(i),
c    *        rea2_year(i),
c    *        rea2_month(i),
c    *        rea2_day(i),
c    *        rea2_abs_time(i)
c     enddo
c999  FORMAT(i2,1x,a5,1x,a2,1x,2i2,f6.1,1x,a8,1x,
c    *1x,i4,1x,i2,1x,i2,1x,f15.3,1x)
      write(6,*)            ! blank line
      write(6,*) 'Number of new phasereadings ', rea2_nphase
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is tsd.out'
      write(6,*)            ! blank line

      call rea2_event_out(3,all,data2,code1)
      close(3)              ! close output s-file
c
      stop
      end


c       call newheader(mainhead(1)(1:80),nnewchan,shift_time)
      subroutine newheader(mainhead,nc,st,oldstat,newstat)
      include 'seidim.inc'                ! dimensions for rea block
      character*80 mainhead(max_trace)
      character*5 newstat, oldstat
      integer nchan
      integer nstat
      integer nc
      integer year,month,day,hour,min,year1,month1,day1,hour1,min1
      integer doy,doy1,k,i,j,l
      character*5 stat(max_trace)  ! staions in header
      character*4 comp(max_trace)  ! components in header      
      real cinter(max_trace),cstart(max_trace) ! start and interval each trace
      real sec, sec1
      double precision st

      write (mainhead(1)(2:19),'(a5,a13)')oldstat," time shifted"
      read (mainhead(1)(31:33),'(i3)')nchan
      write (mainhead(1)(31:33),'(i3)')nc
      read (mainhead(1)(34:59),'(i3,1x,i3,1x,4(i2,1x),f6.3)')
     *year,doy,month,day,hour,min,sec
      year=year+1900       
      call timadd(year,month,day,hour,min,sec,st,
     *year1,month1,day1,hour1,min1,sec1)  ! add correction
      call date_doy(doy1,day1,month1,year1)
      k=year1-1900
      write (mainhead(1)(34:59),'(i3,1x,i3,1x,4(i2,1x),f6.3)')
     *k,doy1,month1,day1,hour1,min1,sec1
c
c   read header channel names, might be needed
c
      nstat=1
      do i=3,(nchan-1)/3+3
            read(mainhead(i),'(3(1x,2a4,a1,f7.2,1x,f8.2))')
     *      stat(nstat)(1:4),  comp(nstat),  stat(nstat)(5:5),  
     *      cstart(nstat),cinter(nstat),
     *      stat(nstat+1)(1:4),comp(nstat+1),stat(nstat+1)(5:5),
     *      cstart(nstat+1),cinter(nstat+1),     
     *      stat(nstat+2)(1:4),comp(nstat+2),stat(nstat+2)(5:5),
     *      cstart(nstat+2),cinter(nstat+2)      
            nstat=nstat+3                                
      enddo
      j=1
      do i=1,nstat
        if(stat(i)(1:5).eq.oldstat)then
          stat(j)(1:5)=newstat(1:5)
          comp(j)(1:4)=comp(i)(1:4)
          cstart(j)=cstart(i)
          cinter(j)=cinter(i)
          j=j+1
        endif
      enddo
c
c   make new channel header, clear first
c
c     khead=(nchan-1)/3+3
c     if(khead.lt.12) khead=12    ! never less than 12 main headers
      khead=12    ! fixed to 12 since one station will not need more
      do i=3,khead
         do j=1,80
            mainhead(i)(j:j)=' '
         enddo
      enddo
      j=1
      do i=3,(nchan-1)/3+3
        l=2
        do k=1,3
           if(j.le.nc)then
              write(mainhead(i)(l:l+24),'(2a4,a1,f7.2,1x,f8.2)')
     *        stat(j)(1:4),comp(j),stat(j)(5:5),cstart(j),cinter(j)
           endif
           j=j+1
           l=l+26
        enddo
      enddo

      return
      end
 

c       call newfilename(text,mainhead(1)(1:80),new_stat,nnewchan)
      subroutine newfilename(outfile,mainhead,new_stat,nc)
      character*80 mainhead,outfile
      character*5 new_stat
      integer nc
c  make output file name 
c             
      outfile=' '
         
c-- year                          
         outfile(3:4)=mainhead(35:36)
         outfile(1:2)='19'
         if(mainhead(34:34).eq.'1') outfile(1:2)='20'
         outfile(5:5)='-'               
c-- month                                    
         outfile(6:7)=mainhead(42:43)
         outfile(8:8)='-'
c-- day                                      
         outfile(9:10)=mainhead(45:46)
         outfile(11:11)='-'
c-- hr                                       
         outfile(12:13)=mainhead(48:49)
c-- min                                    
         outfile(14:15)=mainhead(51:52)
         outfile(16:16)='-'
c-- sec                                    
         outfile(17:18)=mainhead(54:55)
         outfile(19:20)='S.'
c
         outfile(21:25)=new_stat(1:5)

      write(6,*)"Number fo channels  : ",nc
      if(nc.le.9) then
        outfile(28:28)="0"
        outfile(29:29)=char(nc+48)
      elseif(nc.ge.10) then
        i=int(nc/10.)
        outfile(28:28)=char(i+48)
        outfile(29:29)=char(nc-i*10+48)
c     elseif(nevent.ge.100)stop
      endif

         do i=1,18
            if(outfile(i:i).eq.' ') outfile(i:i)='0'                            
         enddo                                                                  
         do i=19,26
            if(outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         do i=27,29
            if(outfile(i:i).eq.' ') outfile(i:i)='0'
         enddo

      return
      end
