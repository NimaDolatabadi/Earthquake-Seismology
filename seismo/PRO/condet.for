      program condet
c
c program to run sta/lta detection through continuous data stream,
c all input is read from condet.par
c
c two detection algorithms are supported:
c       - squared sta/lta
c       - Carl Johnson's 
c

c
c Lars Ottemoller, BGS, July 2005
c

c
c changes:
c
c 20/03/2008 lot - use several stations, network detection (option -net)
c    08/2008 lot - change write for extract.batch not to break up lines
c 14/08/2008 lot - added cosine taper to master signal when using COR
c                  this may give problem at file boundaries
c  2/10/2008 lot - fixed bug with wrong net detection at end and high ndet num
c 29/01/2009 lot - accept change of channel id
c 08/04/2010 lot - removed taper, and changed passes used in filter
c 30/04/2010 lot - added changes by WCC, mostly to netdet
c 15/09/2010 lot - fixed use of max_ratio
c  7/12/2010 lot - changed gt to ge in lines 1126 and 1135, as suggested by wc
c 14/12/2011 jh  - archive reading, replace reference to cwav and replace by wav
c 28/03/2012 jh  - space missing in extract.batch for network det.
c 19/12/2012 wcc - added corrections to lta and sta calculations
c 04/01/2013 jh  - remove reference to bud and scp, fix so work with arc
c 14/05/2013 lo  - merged last changes by wcc and jh
c  9/05/2014 pv  - fixed bug, condet could not read cor master file in arc mode
c 13 01 2016 jh  - removed subroutine hpsort
c 23 11 2016 lo  - fix to hpsort call and time variable, Lisbon

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
      character*4 base_type  ! type of data base like cont
      character*5 base_out   ! type for wavetool

      character*80 outfile   ! name of output seisan  waveform file
      character*80 text  
      character*1040 theader ! seisan channel header
      character*80 fheader(max_trace) ! seisan waveform main header
      character*29 headtext
      character*5 net_code   ! code of network
      real duration_min      ! minute
      integer i,j,l,k,sc     ! counter
      integer j_lta     	 ! counter WCC
      integer tl             ! number of samples to taper
      real taper,pi          ! taper value
      integer maxstat,maxtrig
      parameter (maxstat=100)
      character*5 station(maxstat),cbase(maxstat)
      character*4 component(maxstat)
      integer ind,ind_save
      double precision msec_start,msec_stop,msec,trigger_msec,msec1
      double precision triggerlen	! WCC
      double precision ptrigger_msec
      character*14 stop_time,start_time
      character*18 time
      integer year1,month1,day1,hour1,min1,isec1,doy1
      integer year2,month2,day2,hour2,min2,isec2,doy2
      real sec1,sec2
      real*8 sta,lta,star,ltar
      real*8 newlta,newsta	! WCC added to remove drift
      integer nsta,nlta
      real sta_duration,lta_duration
      logical startup
      integer write1,write2,write3,read1,write4
      logical b_flag
      integer code
      real trigger_ratio,detrigger_ratio,trigcor
      real trigger_min_int,trigger_min_dur
      logical trigger
      logical freeze
      character*8 filter_proto,filter_type  
      integer npoles,passes
      real flow,fhigh,samp_int
      integer ndc
      real rdc
      logical waveout
      character*80      data(max_data)  ! event array
      real data_save(max_sample),first_sta,first_lta
      real first_stacarl,first_ltacarl
      real ltacarl_save(max_sample),ltacarl(max_sample)
      real master(max_sample),cordata(max_sample)
      integer ntriggers(maxstat)
      character*10 detalg
      real carlratio,carlquiet
      integer nstat
      real pre_event,extduration
      logical netflag
      real netwindow,netminrat,netmaxdelt
      integer netmindet
      integer narg
      character*80 arg(40)
      character*80 master_filename
      integer nmaster ! number of samples in master signal
      real max
      integer cor_nsamp
      real max_ratio    ! WCC added maximum sta/lta ratio (for information only)

c
c   get seisan defaults including names of continuous waveform data bases
c   and archive data base
c
      call get_seisan_def 
c
c   get arguments
c
      call get_arguments(narg,arg)
      i=1
      netflag=.false.
      do while(i.le.narg)
        if (arg(i).eq.'-net') then
          netflag=.true.
        elseif (arg(i).eq.'-help') then
          write(*,*) ' usage: condet [-net] '
          write(*,*) '     -net: network detection using '//
     &      'condet.out as input '
          stop
        endif
        i=i+1
      enddo

      call get_contdet_def(nstat,base_type,cbase,start_time,stop_time,
     &   duration_min,station,component,waveout,flow,fhigh,
     &   sta_duration,lta_duration,trigger_ratio,
     &   detrigger_ratio,trigger_min_dur,trigger_min_int,
     &   freeze,detalg,
     &   carlratio,carlquiet,pre_event,extduration,
     &   netwindow,netmaxdelt,netminrat,netmindet,
     &   master_filename)
c
      base_out='-cwav'   ! default is to read from a a seisan cont
c
c      if(base_type(1:3).eq.'scp'.or.base_type(1:3).eq.'bud') then
c         arc=.true.   
c         arc_type=0   ! bud
c         base_out='-bud '
c         if(base_type.eq.'scp') then
c            arc_type=1
c            base_out='-scp '
c         endif
c      endif
      if(base_type(1:3).eq.'arc') then
        arc=.true.
        base_out='-arc '
      endif

      cseed=.false.

c
c open files
c
      if (.not.netflag) then
        call sei open( unknown$,          ! Open old file.
     &               ' ',               ! No prompt.
     &               'condet.out',    ! This filename.
     &               write1,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)
        call sei open( unknown$,          ! Open old file.
     &               ' ',               ! No prompt.
     &               'nordic.out',        ! This filename.
     &               write2,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)
        call sei open( unknown$,          ! Open old file.
     &               ' ',               ! No prompt.
     &               'condet.trace',    ! This filename.
     &               write4,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)
      else
        call sei open( old$,          ! Open old file.
     &               ' ',               ! No prompt.
     &               'condet.out',    ! This filename.
     &               read1,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)
        call sei open( unknown$,          ! Open old file.
     &               ' ',               ! No prompt.
     &               'net.out',    ! This filename.
     &               write1,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)
        call sei open( unknown$,      ! Open unknown file.
     &               ' ',               ! No prompt.
     &               'condet.out.sort', ! This filename.
     &               write2,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)
        call sei open( unknown$,      ! Open unknown file.
     &               ' ',               ! No prompt.
     &               'net.out.all',     ! This filename.
     &               write4,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)
      endif

      call sei open( unknown$,          ! Open old file.
     &               ' ',               ! No prompt.
     &               'extract.batch',   ! This filename.
     &               write3,            ! On this unit.
     &               b_flag,            ! Existance?.
     &               code )             ! Returned consition (n/a)

c
c call routine in case of network detection
c WCC
      if (netflag) then
        write(*,'(f8.1,1x,a)') extduration, ' extract duration'
                write(*,'(f8.1,1x,a)') pre_event,   ' pre event time'
        write(*,'(f8.1,1x,a)') netwindow,   ' net window sec'
        write(*,'(f8.1,1x,a)') netmaxdelt,  ' net max delta sec'
        write(*,'(f8.1,1x,a)') netminrat,   ' net min ratio'
        write(*,'(i8,1x,a)') netmindet,   ' net min detections'
        write(*,*) '--------------------------------------------'
        call netdet(read1,write1,write2,write3,write4,
     &     pre_event,extduration,
     &     netwindow,netmaxdelt,netminrat,netmindet,base_out)
        call sei close( close$, write1, code )
        call sei close( close$, write2, code )
        call sei close( close$, write3, code )
        call sei close( close$, write4, code )
        call sei close( close$, read1, code )
        stop
      endif


c
c init variables 
c
      npoles=4

c      if (cbase(1).eq.' ') then
      if (cbase(1).eq.' '.and..not.arc) then
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

c      write(*,*) ' start time: ',start_time
c      write(*,*) ' stop time:  ',stop_time
C WCC modified to add output of relevant parameters to stdout and condet.trace
       write(*,*) nstat,           ' stations'
       write(*,*) start_time,      ' start date'
       write(*,*) stop_time,       ' stop date'
       write(*,*) detalg,          ' detection algorithm'
       write(*,*) waveout,         ' waveout'
       write(*,*) duration_min,    ' interval to read (minutes)'
       write(*,*) flow,            ' filter low'
       write(*,*) fhigh,           ' filter high'
       write(*,*) trigger_min_int, ' min trigger interval (seconds)'
       write(*,*) extduration,     ' extract duration (seconds)'
       write(*,*) pre_event,       ' pre event time (seconds)'
       if (detalg(1:3).ne.'COR') then
          write(*,*) 'STA AND CARL ONLY PARAMETERS:'
          write(*,*) sta_duration,    '   sta duration'
          write(*,*) lta_duration,    '   lta duration'
          write(*,*) trigger_ratio,   '   trigger ratio'
          write(*,*) detrigger_ratio, '   detrigger ratio'
          write(*,*) trigger_min_dur, '   min trigger duration'
       else
          write(*,*) 'CORRELATION ONLY PARAMETERS:'
          write(*,*) master_filename, '   master waveform file'
          write(*,*) trigger_ratio,   '   trigger ratio (=100*corr min)'
       endif
       if (detalg(1:3).eq.'STA') then
          write(*,*) 'STA ONLY PARAMETERS:'
          write(*,*) freeze,          '   freeze lta'
       endif
       if (detalg(1:3).eq.'CAR') then
          write(*,*) 'CARL ONLY PARAMETERS:'
          write(*,*) carlratio,      '   carl ratio'
          write(*,*) carlquiet,      '   carl quiet'
       endif
c
c loop over stations
c
      do sc=1,nstat
c init
        ind_save=0
        nsta=0
        nlta=0
        sta=0.
        lta=0.
        star=0.
        ltar=0.
        trigger=.false.
        startup=.true.
        passes=1   ! clear recfil buffer when used first time
        filter_proto='BU'
        do k=1,max_sample
          data_save(k)=0.
          ltacarl_save(k)=0.
        enddo
c
        cont_base(1)=cbase(sc)
        ntriggers(sc)=0
        ptrigger_msec=0.
        trigger_msec=0.
c
c read master signal if detalg is correlation
c
        if (detalg(1:3).eq.'COR') then
          if (seiclen(master_filename).le.0) then
            write(*,*) ' MASTER WAVEFORM must be given in condet.par '
            stop
          endif
          call wav_init
          call get_full_wav_name(master_filename,wav_filename(1))
          call read_wav_header(1)
          ind=0
          do j=1,wav_nchan
            if (station(sc).eq.wav_stat(j).and.
     &          component(sc).eq.wav_comp(j).and.ind.eq.0) then
              ind=j
            endif
          enddo
          if (ind.eq.0) then
            write(*,*) ' channel not found ',station(sc),component(sc)
            stop
          else
            write(*,*) ' master file, channel selected index ',ind
          endif
          if(arc) then
            arc=.FALSE.                  ! turn arc off to read master file fron folder
            call wav_read_channel(ind)
            arc=.TRUE.                   ! turn arc back on
          else
            call wav_read_channel(ind)
          endif
          ndc=0
c put remove_dc back, lot 14/08/08
          call remove_dc(signal1,ndc,rdc,wav_nsamp(ind))
c this can be changed to compute dc and use it for taper, but not remove it
c as that is a problem when moving from one file to another
          do j=1,wav_nsamp(ind)
            write(15,*) j,signal1(j)
          enddo
c
c apply cosine taper 
c
          tl=int(wav_nsamp(ind)*.05)
          pi=acos(-1.)
          do j=1,tl
            taper=(-cos(pi/2.+(j-1)*(pi/2.)/float(tl-1)))
            signal1(j)=signal1(j)*taper
            signal1(wav_nsamp(ind)-j+1)=signal1(wav_nsamp(ind)-j+1)*
     &           taper
c            write(18,*) j,taper
          enddo

c          do j=1,wav_nsamp(ind)
c            write(16,*) signal1(j)
c          enddo
           
c
c filter data
c
          samp_int=1./wav_rate(ind)
c          if (flow.ne.0..or.fhigh.ne.0.) then
          if (flow.ne.0..and.fhigh.ne.0.) then
            filter_type='BP'
            call recfil(signal1,wav_nsamp(ind),signal1,
     &      filter_proto,0.,0.,npoles,filter_type,flow,fhigh,
     &      samp_int,passes)
          else
c
c apply highpass to remove dc
c
            filter_type='BP'   ! changed lo 16/09/2010
            flow=0.1
            fhigh=wav_rate(ind)/2.*.9
            call recfil(signal1,wav_nsamp(ind),signal1,
     &      filter_proto,0.,0.,npoles,filter_type,flow,fhigh,
     &      samp_int,passes)
          endif
c
c copy to master signal
c
          nmaster=wav_nsamp(ind)
          do j=1,wav_nsamp(ind)
            master(j)=signal1(j)
            write(17,*) master(j)
          enddo
        endif

c
c  signal that reading is from a continous data base
c
        cwav=.true.
        n_cont_base=1   ! for cont type
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
c set end of interval to be read
c
c        cwav_abs_end_time=cwav_abs_start_time+cont_interval
        call cwav_time_limits(1)
c
        if(.not.arc) then

c
c  read the header information for all files in cont bases in time
c  interval, assume info available in common block
c
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
               stop
            else
               write(6,*) ' Number of archive channels with data:',
     *         wav_nchan
            endif
          endif 
c
c find channel index
c
        ind=0

        do i=1,wav_nchan
          if (wav_stat(i).eq.station(sc).and.
     &        wav_comp(i).eq.component(sc)) 
     &    then
            ind=i
          endif
        enddo
        if (ind_save.eq.0.and.ind.ne.0) ind_save=ind

        if (ind_save.ne.ind) then
c          write(*,*) ' channel index changed, was/now ',
c     &        ind_save,ind
C WCC modified 2008/04/08 to allow channel index change
           write(*,*) station(sc),component(sc),
     &               ' channel index changed from ',
     &               ind, ' to ', ind_save
           if (ind.ne.0) ind_save=ind
        endif

c        cont_interval=duration_min*60.-
c     &       1./cwav_rate(ind,cwav_nseg(ind))

        cwav_abs_end_time=cwav_abs_start_time+cont_interval
 
        call sectim(cwav_abs_start_time,year1,doy1,month1,day1,hour1,
     &    min1,sec1)
        call sectim(cwav_abs_end_time,year2,doy2,month2,day2,hour2,
     &    min2,sec2)

        write(*,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' start time: ',year1,month1,day1,hour1,min1,sec1
        write(write4,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' start time: ',year1,month1,day1,hour1,min1,sec1
        write(*,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' stop time:  ',year2,month2,day2,hour2,min2,sec2
        write(write4,'(a,i4,"/",i2,"/",i2," ",i2,":",i2,":",f5.2)')
     &    ' stop time:  ',year2,month2,day2,hour2,min2,sec2

        if (ind.ne.0) then
c          do j=1,cwav_nseg(ind)
c            write(*,*) ' segment:  ',cwav_nseg(j)
c            write(*,*) ' duration: ',cwav_duration(ind,j)
c            write(*,*) ' rate:     ',cwav_rate(ind,j)
c            write(*,*) ' samples:  ',cwav_nsamp(ind,j)
c          enddo

c
c  read the waveform data, one trace at a time
c
          call wav_read_channel(ind)    ! read trace
          if (wav_nsamp(ind).lt.0) wav_nsamp(ind)=0

          if (wav_nchan.eq.0) then
            startup=.true.
            goto 9999
          endif

c          write(*,*) ' total samples: ',wav_nsamp(ind)-1
          write(*,*) ' total samples: ',wav_nsamp(ind)
          write(write4,*) ' total samples: ',wav_nsamp(ind)
          write(*,*) ' channel index: ',ind
          write(write4,*) ' channel index: ',ind
C WCC modified 2009/04/08 to output component info
          write(*,*) ' ',station(sc),component(sc), 
     &                          ' channel index: ',ind
          write(write4,*) ' ',station(sc),component(sc), 
     &                          ' channel index: ',ind
c
c filter data
c

          samp_int=1./wav_rate(ind)
          if (flow.ne.0..and.fhigh.ne.0.) then
            filter_type='BP'
            call recfil(signal1,wav_nsamp(ind),signal1,
     &      filter_proto,0.,0.,npoles,filter_type,flow,fhigh,
     &      samp_int,passes)
          else
c
c apply highpass to remove dc
c
            filter_type='HP'
            flow=0.01
            call recfil(signal1,wav_nsamp(ind),signal1,
     &      filter_proto,0.,0.,npoles,filter_type,flow,fhigh,
     &      samp_int,passes)
          endif
          passes=-1   ! dont clear recfil buffer after used first time

c
c set waveform output variables
c
          if (waveout) then
            wav_out_nchan=2
            do j=1,wav_out_nchan
              wav_out_year(j)=wav_year(ind)
              wav_out_month(j)=wav_month(ind)
              wav_out_day(j)=wav_day(ind)
              wav_out_hour(j)=wav_hour(ind)
              wav_out_min(j)=wav_min(ind)
              wav_out_sec(j)=wav_sec(ind)
              wav_out_stat(j)=wav_stat(ind)
              if (j.eq.1) then
                wav_out_nsamp(j)=wav_nsamp(ind)
                wav_out_comp(j)=wav_comp(ind)
              else
                 wav_out_nsamp(j)=wav_nsamp(ind)
c                wav_out_nsamp(j)=wav_nsamp(ind)-1
                wav_out_comp(j)=detalg(1:4)
              endif
              wav_out_rate(j)=wav_rate(ind) 
              wav_out_cbyte(j)=wav_cbyte(ind)
            enddo
c
c   make and write seisan output header
c
            headtext=' '
c            net_code=detalg(1:3)
            net_code=wav_stat(ind)
            call sheads(wav_out_year,wav_out_month,
     *        wav_out_day,wav_out_hour,
     *        wav_out_min,wav_out_sec,wav_out_nchan,1,
     *        net_code,headtext,wav_out_stat,wav_out_comp,
     *        wav_out_nsamp,wav_out_rate,wav_out_cbyte,
     *        outfile,fheader,theader)
            outfile=outfile(1:seiclen(outfile))//'_'//detalg(1:3)
            write(6,*) ' Output file : '//outfile
            open(66,file=outfile,status='unknown',form='unformatted')

c
c   write headers, assume maximum of 30 channels, since only 12 headers are written out
c
            do i=1,12
              write(66) fheader(i)
            enddo
c
c write out filtered data
c
            call sheads(wav_out_year,wav_out_month,
     *        wav_out_day,wav_out_hour,
     *        wav_out_min,wav_out_sec,wav_out_nchan,1,
     *        wav_out_stat(1),headtext,wav_out_stat,wav_out_comp,
     *        wav_out_nsamp,wav_out_rate,wav_out_cbyte,
     *        text,fheader,theader)
            write(66) theader              
c find scale
            max=0.
            do i=1,wav_out_nsamp(1)
              if (abs(signal1(i)).gt.max) max=abs(signal1(i))
            enddo
            max=1000000./max
            write(66)(int(signal1(l)*max),l=1,wav_out_nsamp(1))      
c            write(66)(int(signal1(l))*100,l=1,wav_out_nsamp(1))      
          endif
c
c init rea
c
          call rea_hyp_clear(1)
          rea_id_line=' '
          if (waveout) then
            rea_nwav=1
            rea_wav(1)='                                        '//
     &               '                                       6'
            write(rea_wav(1)(2:79),'(a)') outfile(1:seiclen(outfile)) 
          endif

          hyp_year(1)=wav_year(1)
          hyp_month(1)=wav_month(1)
          hyp_day(1)=wav_day(1)
          hyp_hour(1)=wav_hour(1)
          hyp_min(1)=wav_min(1)
          hyp_sec(1)=wav_sec(1)
          hyp_dist_id(1)='L'
          rea_nphase=0
          rea_nhyp=1

          if (detalg(1:3).ne.'COR') then
            if (nsta.eq.0) then
                nsta=int(sta_duration*wav_rate(ind))
              write(*,*) ' nsta: ',nsta
            endif
            if (nlta.eq.0) then
              nlta=int(lta_duration*wav_rate(ind))
              write(*,*) ' nlta: ',nlta
            endif
            write(*,*) ' nsta/nlta: ',nsta,nlta
          endif
c
c compute cross correlation
c
          if (detalg(1:3).eq.'COR') then
            write(*,*) ' master nsamp ',nmaster,
     &        ' cont segment nsamp ',wav_nsamp(ind)
            call cor_time(master,nmaster,signal1,
c     &         cwav_nsamp(ind,cwav_nseg(ind)),
     &         wav_nsamp(ind),
     &         cordata,cor_nsamp)
c           do j=1,cwav_nsamp(ind,cwav_nseg(ind))
c              write(15,'(f5.2)') abs(cordata(j)*100.)
c           enddo
          endif

c
c compute sta and lta
c

          max_ratio=0.  ! WCC
          newlta=0.		! WCC
          newsta=0.		! WCC
          j_lta=0		! WCC

          do j=1,wav_nsamp(ind)   
          
C Important changes to the next two "if" constructs (WCC 7/2011)
C   1: Removed "+1" from indexing (made nsta and nlta one too short)
C   2: Changed (j.ge...) to (j.gt....) (avoid index 0 with the +1 removed)

c
c set previous data values for sta and lta
c
            if (detalg(1:3).ne.'COR') then
              if (j.gt.nsta) then
                first_sta=signal1(j-nsta)
              else
                first_sta=data_save(nlta-nsta+j)
              endif
              if (j.gt.nlta) then
                first_lta=signal1(j-nlta)
              else
                first_lta=data_save(j)
              endif
            endif
c
c previous values for carltrig
c
            if (detalg.eq.'CAR') then
              if (j.gt.nsta) then
                first_stacarl=ltacarl(j-nsta)
              else
                first_stacarl=ltacarl_save(nlta-nsta+j)
              endif
              if (j.gt.nlta) then
                first_ltacarl=ltacarl(j-nlta)
              else
                first_ltacarl=ltacarl_save(j)
              endif
            endif
c
c get sample time
c
            msec=cwav_abs_start_time+
     &         float(j-1)/wav_rate(ind)
c
c add new sample and delete first sample in sta and lta
c
            if (detalg(1:3).eq.'STA') then
              sta=sta+(signal1(j)**2 - first_sta**2)/float(nsta)
              ! NEW section calculates and synchronizes to sta at start of record
              if (j.lt.nsta) then							! WCC
              	newsta=newsta+signal1(j)**2/float(nsta)		! WCC
              elseif (j.eq.nsta) then						! WCC
              	sta=newsta+signal1(j)**2/float(nsta)		! WCC
              endif											! WCC
              ! End new section !!!!!!!!!!!!!!!!!!!!!!!!!!!!! WCC
              if (.not.trigger.or..not.freeze) then
                lta=lta+(signal1(j)**2 - first_lta**2)/float(nlta)
              	! NEW section calculates and synchronizes to lta at start of record
              	j_lta=j_lta+1								! WCC
              	if (j_lta.lt.nlta) then						! WCC
              		newlta=newlta+signal1(j)**2/float(nlta)	! WCC
              	elseif (j_lta.eq.nlta) then					! WCC
              		lta=newlta+signal1(j)**2/float(nlta) 	! WCC
                endif										! WCC
                ! End new section !!!!!!!!!!!!!!!!!!!!!!!!!!! WCC
              endif

              signal2(j)=sta/lta

            elseif (detalg(1:3).eq.'CAR') then
              sta=sta+signal1(j)/float(nsta)-
     &          first_sta/float(nsta)
              ltacarl(j)=ltacarl(j)+signal1(j)/float(nlta)-
     &            first_lta/float(nlta)
              star=star+abs(signal1(j)-ltacarl(j))/float(nsta)-
     &            abs(first_sta-first_stacarl)/float(nsta)
     
              ltar=ltar+abs(signal1(j)-ltacarl(j))/float(nlta)-
     &            abs(first_lta-first_ltacarl)/float(nlta)

              signal2(j)=
     &          star-carlratio*ltar
     &          -abs(sta-lta)
     &          -carlquiet

            elseif (detalg(1:3).eq.'COR') then
              signal2(j)=cordata(j)*100.
            endif

            if (signal2(j).gt.max_ratio) max_ratio=signal2(j)

            if (startup.and.j.ge.nlta) then
              startup=.false.
            endif
c
c save signal 
c
            if (detalg(1:3).ne.'COR') then
              if (j.eq.wav_nsamp(ind)) then
                do k=1,nlta
                  data_save(k)=signal1(wav_nsamp(ind)-nlta+k)		! WCC removed -1
                  ltacarl_save(k)=ltacarl(wav_nsamp(ind)-nlta+k)	! WCC removed -1
                enddo
              endif
            endif
c
c check for trigger based on sta/lta ratio
c
            if (.not.startup) then 
              if (signal2(j).gt.trigger_ratio.and.
     &        .not.trigger) then
                trigger_msec=msec
                trigger=.true.
                if (detalg.eq.'COR') trigcor=signal2(j)
              endif
              if (trigger) then
c
c check for detrigger, detection if trigger duration long enough
c
                if (signal2(j).le.detrigger_ratio) then
c                  write(*,*) ' detrigger ',signal2(j)

                  if (msec-trigger_msec.lt.trigger_min_dur) then
c                    write(*,*) ' trigger interval too short '
                    trigger=.false.
                  endif
                  if (trigger_msec-ptrigger_msec.lt.
     &              trigger_min_int) then
                      trigger=.false.
                  endif

                  if (detalg.eq.'COR'.and.trigcor.lt.signal2(j)) then
                    trigcor=signal2(j)
                    trigger_msec=msec
                  endif
                  if (trigger) then
c
c write out trigger
c
                    ntriggers(sc)=ntriggers(sc)+1

c
c add phase to nordic.out
c
                    rea_nphase=rea_nphase+1
                    if (rea_nphase.ge.max_phase) then
                      write(*,*) ' too many phases -> stop '
                      stop
                    endif
                    call rea_phase_clear(rea_nphase)
                    rea_stat(rea_nphase)=wav_stat(ind)
                    rea_comp(rea_nphase)=wav_comp(ind)
c                  rea_co(rea_nphase)(1:2)=wav_comp(ind)(1:1)//
c     &                 wav_comp(ind)(4:4)
                    rea_co(rea_nphase)(1:2)=wav_out_comp(2)(1:1)//
     &                 wav_out_comp(2)(4:4)
                    rea_phase(rea_nphase)=detalg(1:1)
                    rea_onset(rea_nphase)=' '
                    rea_weight_out(rea_nphase)='  '
                    rea_weight_in(rea_nphase)=' '
                    rea_polarity(rea_nphase)=' '
                    call sectim(trigger_msec,rea_year(rea_nphase),
     &     doy1,rea_month(rea_nphase),rea_day(rea_nphase),
     &     rea_hour(rea_nphase),rea_min(rea_nphase),
     &     rea_sec(rea_nphase))
  
                    call sectim(trigger_msec,year1,
     &                doy1,month1,day1,hour1,min1,sec1)
                    isec1=int(sec1)
                    time=' '
                    write(time,'(i4.4,5i2.2)') 
     &                year1,month1,day1,hour1,min1,isec1
                    if (detalg(1:3).ne.'COR') then
                      if (max_ratio.gt.9999.) max_ratio=9999.			! WCC
                      triggerlen=msec-trigger_msec						! WCC
                      if (triggerlen.gt.9999.9) triggerlen=9999.9		! WCC
                      write(write1,'(a5,1x,a4,1x,a14,1x,f6.1,1x,f6.1)') ! WCC
     &                  station(sc),component(sc),time,
     &                  triggerlen, max_ratio
         			  max_ratio=0.  ! WCC
                    else
                      write(write1,'(a5,1x,a4,1x,a14,1x,f6.1,1x,f6.1)')
     &                station(sc),component(sc),time,
     &                trigcor,0.  ! WCC
                    endif

c
c write out extract command
c
c                    msec1=trigger_msec-60.
                    msec1=trigger_msec-pre_event
                    call sectim(msec1,year1,
     &                doy1,month1,day1,hour1,min1,sec1)
                    isec1=int(sec1)
                    write(time,'(i4.4,4i2.2,f6.3)') 
c     &                year1,month1,day1,hour1,min1,isec1
     &                year1,month1,day1,hour1,min1,sec1
                     do k=1,18
                       if (time(k:k).eq.' ') time(k:k)='0'
                     enddo
c                    write(write3,*) 'wavetool -cwav -start '
                    write(write3,'(a,a,a,f6.1,a)')
     &                'wavetool ',base_out,' -start '
     &                // time(1:seiclen(time)) 
     &     // ' -duration ',extduration,' -wav_out_file SEISAN'
c
c save time as previous trigger
c
                    ptrigger_msec=trigger_msec
                    trigger=.false.
                  endif
                endif
              endif
            endif
          enddo
   
          if (waveout) then
c
c write out sta/lta
c
            call sheads(wav_out_year,wav_out_month,
     *        wav_out_day,wav_out_hour,
     *        wav_out_min,wav_out_sec,wav_out_nchan,2,
     *        wav_out_stat(2),headtext,wav_out_stat,wav_out_comp,
     *        wav_out_nsamp,wav_out_rate,wav_out_cbyte,
     *        text,fheader,theader)
            write(66) theader              
            write(66)(int(signal2(l)),l=1,wav_out_nsamp(2))      
          endif
c
c write out phases to Nordic file
c
          call rea_event_out(write2,.true.,data,code)
        endif
9999    continue
c
c set start of next interval to read
c
        cwav_abs_start_time=cwav_abs_end_time
      enddo
      enddo ! end of loop over stations
c
c close files
c
      call sei close( close$, write1, code )
      call sei close( close$, write2, code )
      call sei close( close$, write3, code )
      call sei close( close$, write4, code )
      do sc=1,nstat
        write(*,*) ' number of triggers ',station(sc),ntriggers(sc)
      enddo

      stop
      end


      subroutine get_contdet_def(nstat,base_type,
     *   cbase,startdate,stopdate,
     &   duration,station,component,waveout,flow,fhigh,
     &   sta_duration,lta_duration,trigger_ratio,
     &   detrigger_ratio,trigger_min_dur,trigger_min_int,
     &   freeze,detalg,
     &   carlratio,carlquiet,pre_event,extduration,
     &   netwindow,netmaxdelt,netminrat,netmindet,
     &   master_filename)

      implicit none
      include 'libsei.inc'
      include 'seidim.inc'

      character*80 def_file         ! file names
      integer in                    ! file unit
      integer code                  ! return code from opening def file
      integer i                     ! counter
      character*80 line             ! text line
      real var                      ! value of variables
      integer nstat                 ! station counter
      integer seiclen
      character*4 base_type       ! type of data base, archive type or seisan cont

      character*(*) cbase(*),startdate,stopdate,
     &  station(*),component(*),
     &  detalg
      logical waveout,freeze
      real duration,flow,fhigh,sta_duration,lta_duration
      real trigger_ratio,detrigger_ratio
      real trigger_min_int,trigger_min_dur
      real carlratio,carlquiet
      real pre_event,extduration
      real netwindow,netminrat,netmaxdelt
      integer netmindet
      character*80 master_filename
      real cormin
   
c
c check for file end open
c

      def_file = 'condet.par'

      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   def_file )       ! For this filename.

      
      if(code.ne.e_ok$) then
        write(*,*) ' definition file does not exist: condet.par'
        stop
      endif
c
c init values
c
      cbase(1)=' '
      base_type=' '
      startdate=' '
      stopdate=' '
      station(1)=' '
      component(1)=' '
      waveout=.false.
      duration=0.
      sta_duration=0.
      lta_duration=0.
      trigger_ratio=10.
      detrigger_ratio=10.
      trigger_min_int=0.
      trigger_min_dur=10.
      freeze=.false.
      detalg='STA'
      carlratio=4.
      carlquiet=2.
      nstat=0  ! station counter
      pre_event=60.
      extduration=180.
      netwindow=0.
      netmindet=1
      netminrat=0.
      netmaxdelt=0.
      cormin=1.
      master_filename=' '
      flow=0.
      fhigh=0.


100   continue

c
c read text line from file, check for code and set variables
c
      read(in,'(a80)',end=300,err=200) line

      if (line(1:10).eq.'START DATE') then
        read(line(41:54),'(a)') startdate
      elseif (line(1:9).eq.'BASE TYPE') then
        read(line(41:54),'(a)') base_type
      elseif (line(1:9).eq.'STOP DATE') then
        read(line(41:54),'(a)') stopdate
      elseif (line(1:7).eq.'STATION') then
        nstat=nstat+1
        read(line(41:45),'(a)') cbase(nstat)
        read(line(51:55),'(a)') station(nstat)
        read(line(57:60),'(a)') component(nstat)
      elseif (line(1:13).eq.'DET ALGORITHM') then
        read(line(41:50),'(a)') detalg
      elseif (line(1:15).eq.'MASTER WAVEFORM') then
        read(line(41:seiclen(line)),'(a)') master_filename
      elseif (line(1:7).eq.'WAVEOUT') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) waveout=.true.
      elseif (line(1:8).eq.'INTERVAL') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) duration=var
      elseif (line(1:15).eq.'CORRELATION MIN') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) cormin=var
      elseif (line(1:10).eq.'FILTER LOW') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) flow=var
      elseif (line(1:11).eq.'FILTER HIGH') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) fhigh=var
      elseif (line(1:10).eq.'STA LENGTH') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) sta_duration=var
      elseif (line(1:10).eq.'LTA LENGTH') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) lta_duration=var
      elseif (line(1:13).eq.'TRIGGER RATIO') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) trigger_ratio=var
      elseif (line(1:15).eq.'DETRIGGER RATIO') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) detrigger_ratio=var
      elseif (line(1:10).eq.'FREEZE LTA') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) freeze=.true.
      elseif (line(1:17).eq.'MIN TRIG DURATION') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) trigger_min_dur=var
      elseif (line(1:17).eq.'MIN TRIG INTERVAL') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) trigger_min_int=var
      elseif (line(1:10).eq.'CARL RATIO') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) carlratio=var
      elseif (line(1:10).eq.'CARL QUIET') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) carlquiet=var
      elseif (line(1:14).eq.'PRE EVENT TIME') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) pre_event=var
      elseif (line(1:14).eq.'NET WINDOW SEC') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) netwindow=var
      elseif (line(1:15).eq.'NET MAXDELT SEC') then             ! WCC
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) netmaxdelt=var
      elseif (line(1:13).eq.'NET MIN RATIO') then               ! WCC
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) netminrat=var
      elseif (line(1:11).eq.'NET MIN DET') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) netmindet=int(var)
      elseif (line(1:16).eq.'EXTRACT DURATION') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) extduration=var
      endif

      goto 100

200   continue
      write(*,*) ' Error in condet.par file'
      return

300   continue
      call sei close( close$, in, code )

      if (netmaxdelt.eq.0.) netmaxdelt=netwindow    ! WCC 3/2010

      if (detalg(1:3).eq.'CAR') then
        trigger_ratio=0.
        detrigger_ratio=0.
        sta_duration=1.0
        lta_duration=8.0
      elseif (detalg.eq.'COR') then
        trigger_min_dur=0.
        trigger_ratio=cormin*100.
        detrigger_ratio=0.
        sta_duration=0.
        lta_duration=0.
      endif

      return
      end


      subroutine netdet(read1,write1,write2,write3,write4,
     &     pre_event,extduration,netwindow,netmaxdelt,
     &     netminrat,netmindet,base_out)

      implicit none
      character*5 base_out
      integer read1,write1,write2,write3,write4	! WCC added 2&4
      real pre_event,extduration
      real netwindow, netmaxdelt, netminrat
      integer netmindet
      integer i,j,k,n,ind
      integer max
      parameter (max=500000)
      double precision msec(max),hmsec
      integer flag(max)	! WCC replaces used: 0=unused,1=used,2=rejected
      logical rpt
C      logical used(max)
      character*5 stat(max),hstat
      character*4 comp(max)	! WCC
      real coda(max),ratio(max)	! WCC
      integer year,month,day,hour,min,sec,doy
      real rsec
      integer ndet
	  integer ncorrdet	! WCC 3/2010
      character*18 time
      character*80 line

c
c read data    
c
      n=1
10    continue
      read(read1,'(a80)',end=50) line
c      write(*,*) line
      read(line,'(a5,1x,a4,1x,i4,5i2,1x,f6.1,1x,f6.1)') 
     &   stat(n),comp(n),year,month,day,hour,min,sec,coda(n),ratio(n)
      rsec=float(sec)
      call timsec(year,month,day,hour,min,rsec,msec(n))
      flag(n)=0
      n=n+1
      if (n.gt.max) then
      	write(*,*) 'More than ', max,
     &			' , only processing first ', max
      	write(write1,*) 'More than ', max,
     &			' detections, only processing first ', max
     	goto 50
      endif
      goto 10

50    continue
      n=n-1
      write(*,'(i8,1x,a)') n,' detections on all stations'
      write(write1,'(i8,1x,a)') n,' detections on all stations'

c WCC: Use heapsort, much faster
	call xhpsort(n,msec,stat,comp,coda,ratio)
c
c now check if enough different stations in time window
c
      ndet=0
      ncorrdet=0	! WCC 3/2010
      do i=1,n
          hmsec=msec(i)
          call sectim(hmsec,year,
     &             doy,month,day,hour,min,rsec)
	      sec=int(rsec)
		  write (write2,'(a5,1x,a4,1x, i4,5i2.2,1x,f6.1,1x,f6.1)')
     &                 stat(i),comp(i),
     &                 year,month,day,hour,min,sec,coda(i),ratio(i)
          if (flag(i).eq.0 .and. ratio(i).ge.netminrat) then
              ndet=1
              j=i+1
              if (i.lt.n) then
                  do while(msec(j).le.msec(i)+netwindow.and.
     &                   msec(j).le.msec(j-1)+netmaxdelt.and.	! WCC 3/2010
     &                   j.le.n) ! lot 02/10/2008
                      rpt=.false.
C WCC Check that its trigger ratio is big enough
                if (ratio(j).ge.netminrat) then
C WCC verify that it's not an already-picked station
						  do k=i,j-1
							if (stat(j).eq.stat(k)) then
								rpt=.true.
							endif
						  enddo
						  if (.not.rpt) then
							  ndet=ndet+1
							  flag(j)=1
							  flag(i)=1
						  else
							  flag(j)=2		! Rejected
						  endif
				      else
				      	flag(j)=2	! Rejected
				      endif
                      j=j+1
                  enddo
              endif
			  if (ndet.ge.netmindet) then
				  hmsec=msec(i)-pre_event
				  call sectim(hmsec,year,
     &             doy,month,day,hour,min,rsec)
				  sec=int(rsec)
c				  write(time,'(i4,5i2.2)') year,month,day,hour,min,sec
				  write(time,'(i4,4i2.2,f6.3)') year,month,day,hour,min,rsec
				  write(write1,*) time,' ndet = ',ndet
				  write(write3,'(a,a,a,f6.1,a)')
     &             'wavetool ',base_out, ' -start '
     &             // time 
     &             // ' -duration ',extduration,' -wav_out_file SEISAN'
C WCC 3/2010: output picked stations and times
				  ncorrdet=ncorrdet+1
				  write (write4,'(a,i3,a)')
     &  '------------ ', ndet, ' stations ------------'
				  do k=i,j-1
				  	  if (flag(k).eq.1) then
					      hmsec=msec(k)
					      call sectim(hmsec,year,
     &                        doy,month,day,hour,min,rsec)
					      sec=int(rsec)
					      write (write4,'(a,1x,'
     &                      // 'i4,''/'',i2.2,''/'',i2.2,'' '','
     &                      // 'i2.2,'':'',i2.2,'':'',i2.2,'
     &                      // '1x,f6.1,1x,f6.1)') 
     &                           stat(k),year,month,day,hour,min,sec,
     &                           coda(k),ratio(k)
                       endif
				  enddo
C end WCC 3/2010
			  endif
          endif
      enddo
      
C		WCC added 3/2010
	  write(*,'(i8,a,f4.1,a,i3,a)') ncorrdet,
     &        ' events detected within ',
     &        netwindow, 's on ' , 
     &        netmindet , '+ stations'
      return
      end

C HEAPSORT ALGORITHM, FROM NUMERICAL RECIPES
C	Sorts an array ra(1:n) into ascending numerical order using the Heapsort algorithm. n is 
C	input; ra is replaced on output by its sorted rearrangement. 
C   MODIFIED FOR CONDET: ADD accompanying ARRAYS cas cac raa and rab,
C   and make ra double precision
	SUBROUTINE xhpsort(n,ra,cas,cac,raa,rab) 
	INTEGER n 
	double precision ra(n)
	real raa(n),rab(n)
	character*5 cas(n)
	character*4 cac(n)
	INTEGER i,ir,j,l 
	double precision rra
	real rraa,rrab
	character*5 ccas
	character*4 ccac
	integer iia
	
	if (n.lt.2) return 
C	The index l will be decremented from its initial value down to 1 during the
C   ÒhiringÓ (heap creation) phase. Once it reaches 1, the index ir will be
C   decremented from its initial value down to 1 during the
C   Òretirement-and-promotionÓ (heap selection) phase. 
	l=n/2+1 
	ir=n 
10 	continue 
	if (l.gt.1) then
		l=l-1 
		rra=ra(l) 
		ccas=cas(l)
		ccac=cac(l)
		rraa=raa(l)
		rrab=rab(l)
	else
		rra=ra(ir) 
		ccas=cas(ir)
		ccac=cac(ir)
		rraa=raa(ir)
		rrab=rab(ir)
		ra(ir)=ra(1)  
		cas(ir)=cas(1)
		cac(ir)=cac(1)
		raa(ir)=raa(1)
		rab(ir)=rab(1)
		ir=ir-1 
		if (ir.eq.1) then  
			ra(1)=rra 
			cas(1)=ccas
			cac(1)=ccac
			raa(1)=rraa
			rab(1)=rrab
			return 
		endif 
	endif 
	i=l
	j=l+l 
20	if(j.le.ir)then
		if (j.lt.ir) then 
		if (ra(j).lt.ra(j+1)) j=j+1 
		endif 
		if(rra.lt.ra(j)) then 
			ra(i)=ra(j) 
			cas(i)=cas(j)
			cac(i)=cac(j)
			raa(i)=raa(j)
			rab(i)=rab(j)
			i=j 
			j=j+j 
		else
			j=ir+1 
		endif 
		goto 20 
	endif 
	ra(i)=rra
	cas(i)=ccas
	cac(i)=ccac
	raa(i)=rraa
	rab(i)=rrab
	goto 10 
	END 

      subroutine netdet_old(read1,write1,write3,pre_event,extduration,
     &     netwindow,netmindet,base_out)

      implicit none
      character*5 base_out
      integer read1,write1,write3
      real pre_event,extduration
      real netwindow
      integer netmindet
      integer i,j,n,ind
      integer max
      parameter (max=1000000)
      double precision msec(max),hmsec
      logical used(max)
      character*5 stat(max),hstat
      integer year,month,day,hour,min,sec,doy
      real rsec
      integer ndet
      character*14 time

c
c read data    
c
      n=1
10    continue
      read(read1,'(a5,1x,4x,1x,i4,5i2)',end=50) 
     &   stat(n),year,month,day,hour,min,sec
      rsec=float(sec)
      call timsec(year,month,day,hour,min,rsec,msec(n))
      used(n)=.false.
      n=n+1
      if (n.ge.max) then
        write(*,*) ' dimensions exceeded, stop '
        stop
      endif
      goto 10

50    continue
      n=n-1
      write(*,*) ' number of detections: ',n
c
c sort data
c
      do i=1,n-1
        ind=i
        do j=i+1,n
          if (msec(j).lt.msec(ind)) then
            ind=j
          endif
        enddo
c
c swap
c
        if (ind.gt.0) then
          hstat=stat(i)
          hmsec=msec(i)
          stat(i)=stat(ind)
          msec(i)=msec(ind)
          stat(ind)=hstat
          msec(ind)=hmsec
        endif
      enddo

c
c now check if enough different stations in time window
c
      ndet=0
      do i=1,n
c       write(25,*) ' i ',i,netwindow
        j=i+1
        ndet=0
        if (.not.used(i)) then
         ndet=1
         if (i.lt.n) then
          do while(msec(j).le.msec(i)+netwindow.and.
     &             j.le.n) ! lot 02/10/2008
c         write(25,*) j,stat(j),msec(j),i,stat(i),msec(i)+netwindow,
c    &     msec(i)+netwindow-msec(j),ndet
            if (stat(j).ne.stat(i).and..not.used(j)) then
              ndet=ndet+1
c         write(25,*) ' j ',j,msec(j),msec(i)+netwindow,
c     &     msec(i)+netwindow-msec(j),ndet
              used(j)=.true.
              used(i)=.true.
            endif
            j=j+1
          enddo
         endif
        endif
c        write(*,*) ' ndet ',ndet
        if (ndet.ge.netmindet) then
          hmsec=msec(i)-pre_event
          call sectim(hmsec,year,
     &           doy,month,day,hour,min,rsec)
          sec=int(rsec)
          write(time,'(i4,5i2.2)') year,month,day,hour,min,sec
          write(*,*) time,' ndet = ',ndet
c          write(write3,*) 'wavetool -cwav -start '
                    write(write3,'(a,a,a,f6.1,a)')
c     &                'wavetool -cwav -start '
     &                'wavetool ',base_out,' -start '
     &                // time 
     &     // ' -duration ',extduration,' -wav_out_file SEISAN'
        endif
      enddo
      
      return
      end

