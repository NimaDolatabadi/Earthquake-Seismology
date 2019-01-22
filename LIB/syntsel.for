      subroutine syntsel
     *(data,ndata,nhead,t0,window,t0synt,dtt,nsynt,codsyn)
c
c     updates jan 25
c
c     jan 95   jh    *********version 5.0 ******************************
c     feb 95         new findchan
c     april 28 , 95  fix rotaion bug with angle
c     may  8     95  new seisinc common block
c     oct 95 by jh    : new seisinc, use delays by default
c     april 99 by jh  : --------------   version 7.0 check ----------------
c                       stats to 5 char, year 2000
c     august 99       : wrong year in synthetic data
c                       call get_seisan_def
c     may 30          : wrong year write out of synt data for 2000
c     feb 19, 2001 lo : changed common block signal
c     jan 10  2009 jh : fixed time shift, put in new wav reading, clean up
c     feb 20  2011 jh : to prevent errro mess form redign delay, put in a small delay
c     jan 4 2012    jh: add argument to auto_tr

c
c
c   Puts together synthetic data from modelling and real data
c   from corresponding waveform files file to one seisan file. One real
c   trace is followed by one synthetic.
c   The time window starts at the seism origin + t0
c 
c
c   input:  data      the hyp.out file
c           ndata:    number of records in hyp.out
c           nhead:    number of headers ----------
c           t0    :   start time of the plot in seconds from event origin time
c           window:   time window to plot(secs)
c           t0synt:   start time of the synthetics at each station
c                                 in seconds from seism origin time 
c           dtt   :   sampling interval of the synthetics (secs)
c           nsynt:    number of samples in a synthetic signal
c           codsyn:   'SW '(for WKBJ), 
c                           'SH '(for Herrmann) or 'SB '(for Bouchon)
c
c
      implicit none
      include 'seidim.inc'                  ! dimensions
      include 'libsei.inc'                  ! file open etc
      include 'waveform.inc'                  ! waveform data
      include 'seisan.inc'                    ! seisan general parameters
c-- single trace file header
      character*1040	chead,cheadsave
c-- main header
      character*80      mhead(max_trace),mmhead(max_trace),
     *                  mmmhead(max_trace)
c-- data array for s-file
      character*80 data(*)
c-- trace file name
      character*80 trace_file,tt,file
c-- network code
      character*5 net_code		
c-- dummy
      character*80 dummy		
c-- station code
      character*5  stat(max_trace)		
c-- code for labelling the synthetic components
      character*3 codsyn
c-- file units
      integer wav_unit,unit_out
c-- code for component orientation
      logical radial
c-- component
      character*4  comp(max_trace)	,compd(max_trace)
c-- amplitudes of the transfer fonctions at 1Hz
      real ampln,ample,dampl
c-- station and component
      character*9 statcomp(max_trace)
c-- start time of plot and synthetic relative to seism origin time
      real t0,t0synt(max_trace),dtt
c-- start time to plot relative to first sample
      real tstart(max_trace)
c--  time window selected
      real window
c-- ---------
      integer n,ns
c-- read function
      real sei real num
c-- header parameters
      real strike,dip,rake,depth
      integer istrike,idip,irake
c-- path to seismo
      character*60 top_directory		
c-- number of channels in w-file
      integer nchan
c-- .true. if waform file exists
      logical exist			
c-- number of samples in one channel
      integer nsamp
c-- 1: test output, 0: no output
      integer check			
c-- origin date
      integer oyear,omonth,oday,odoy		
c-- origin time
      integer ohour,omin		
c-- origin time
      real    osec
c-- origin date
      integer outyear,outmonth,outday,outdoy		
c-- origin time
      integer outhour,outmin		
c-- origin time
      real    outsec				
c-- date of first sample
      integer wyear,wmonth,wday		
c-- time of ------------
      integer whour,wmin		
c-- --------------------
      real    wsec
c-- date of first sample for synthetics
      integer syear,smonth,sday,sdoy		
c-- time of ------------
      integer shour,smin		
c-- --------------------
      real    ssec
c-- abs times
      double precision wtime,otime,stime
c-- sample rate
      real    rate			
c-- error variables
      integer error,ierr		
c-- channel numbers to select
      integer chan(max_trace)			
c-- interval each channel
      real cinter(max_trace)
c-- delay of first sample selected relative to origin time
      real delay(max_trace)
c-- addtional channel delay
      real delay_chan(max_trace)
c-- text string
      character*80 text
c-- number of signals in synthetic traces
      integer nsynt
c-- sample rate synthetic data
      real syntrate
c-- station number corresponding to a given channel
      integer jstat(max_trace)
c-- p-residual
      real p_residual
c-- azimuth for rotating the horizontal seismograms
      real backazi,caz,saz
c-- help variables
      real c_delay
      integer	i,j,l,n1,n2,k,ichan,kchan,ilec,ndata,nhead,ncbid,code
      integer npresent            ! number of waveform files present
      integer channel       ! channel numbe rof input data for selected stat-comp
c-- seismic signal
      integer*2 isignal2(max_sample*2)
      integer b_flag                         ! used with file opening
      real signal(max_sample)			
      real signaln(max_sample),signale(max_sample)
      integer isignal(max_sample)
      real baz(max_trace)
      character*1 rot_comp(max_trace)  ! T or R
      real        del_rot       ! delay rotated trace
      logical rotate
c
      equivalence(signal,isignal)
c

      error=0
      check=0     ! check=1 writes out test info
c
c   get seisan defaults
c
      call get_seisan_def
c
c   read seism origin time
c
      read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *oyear,omonth,oday,ohour,omin,osec
c
c   get event origin time in secs
c
      call timsec(oyear,omonth,oday,ohour,omin,osec,otime)
c
c   calculate plot start time in secs, t0 is start of plot (data window)
c   after origin time
c
      if(check.eq.1) then
         write(6,*)'t0',t0
         write(6,'(a,12f5.1)')'syn-del',(t0synt(i),i=1,12)
      endif

      otime = otime + t0
      call sectim(otime,oyear,odoy,omonth,oday,ohour,omin,osec)
c
c  oday etc is now plot start time
c
c
c   read if Radial-Transverse or North-East components
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: COMPON-:') then
            radial=.false.
            if  (data(i)(20:25).eq.'RADIAL') radial=.true.
         endif
      enddo
c
c   find number of channels to display, if component is blank, use all,else
c   only use component selected. At least the type of instrument must be
c   specified, e.g. S
c
      nchan=0
      do i=1,ndata
         if(data(i)(80:80).eq.'3'.and.data(i)(2:15).eq.
     *      'SYNT: STATION:'.and.data(i)(22:23).ne.'  ')
     *   then
            do k=1,nchan
              if(statcomp(k).eq.data(i)(17:25)) goto 80
            enddo
c
c   channel(s) was not counted before
c
            nchan=nchan+1
            statcomp(nchan)=data(i)(17:25)
            if(statcomp(nchan)(9:9).eq.' ') then
               statcomp(nchan)(9:9)='Z'
               statcomp(nchan+1)=statcomp(nchan)
               statcomp(nchan+1)(9:9)='N'
               if (radial) statcomp(nchan+1)(9:9)='R'
               statcomp(nchan+2)=statcomp(nchan)
               statcomp(nchan+2)(9:9)='E'
               if (radial) statcomp(nchan+2)(9:9)='T'
               nchan=nchan+2
            endif
 80         continue
         endif
      enddo
c
      write(6,*)'Number of channels to display', nchan
c
c  make codes for real and synthetic channels, synthetics h will be every 
c  second channel (i+1)
c
      k=1
      j=0
      do i=1,nchan*2,2
         stat(i)=statcomp(k)(1:5)
         stat(i+1)=stat(i)
         comp(i)=statcomp(k)(6:9)
         compd(i)=comp(i)
         if (compd(i)(4:4).eq.'R')  compd(i)(4:4)='N'
         if (compd(i)(4:4).eq.'T')  compd(i)(4:4)='E'
         comp(i+1)(1:3)=codsyn
	     comp(i+1)(4:4)=statcomp(k)(9:9)
         if (i.eq.1.or.stat(i).ne.stat(i-2))  j=j+1
         jstat(i+1)=j
         k=k+1
      enddo
c
c   get trace data file names from s-file
c
        call auto_tr(data,nhead,ndata,wav_nfiles,wav_filename)

c
c   find how many waveform files are present,
c   replace the origial names without path, with the present files
c   full path 
c
       npresent=0                      ! no files initially
       exist=.true.
       do i=1,wav_nfiles
          call  get_full_wav_name(wav_filename(i),text)            
          if(text.ne.' ') then
             npresent=npresent+1
             wav_filename(npresent)=text
          endif
      enddo
c
      if(npresent.eq.0) then
         exist=.false.
         write(6,*) ' No waveform files found'
      endif
c
c   read all waveform file headers
c
      do i=1,wav_nfiles
         write(6,'(1x,a)') wav_filename(i)
c
c   read all headers of file i
c
         call read_wav_header(i)
c
c   output possible errors
c
         if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *   wav_error_message
c
c   write out the format
c
         write(6,'(1x,a,a)') 'Format ',wav_file_format(i)

      enddo
c
c---------------------------------------------------------
c   enter loop for channels, first read header only,
c---------------------------------------------------------
c
      do 1000 ichan=1,2*nchan,2       ! skip synthetic channel numbers
        if (ichan.gt.1.and.stat(ichan).eq.stat(ichan-2))   then 
c
c   put delay of previous channels in current channel, only if a new 
c   component of same channel
c
           delay_chan(ichan)= delay_chan(ichan-2)     ! data channel
           delay_chan(ichan+1)= delay_chan(ichan-1)   ! synt channel, needed, null below ?
        else
c
c   find first station to be modelled, read P time residual
c
        do i=2,ndata
           if(data(i)(2:6).eq.stat(ichan).
     &                   and.data(i)(11:11).eq.'P') then
             read(data(i)(64:68),'(f5.1)') p_residual
             goto 300
           endif
        enddo
 350    write(6,*)'No P-reading '
        p_residual=0.0
 300    continue
c
c   read how to delay the data trace to make sure synthetic and real
c   signal are aligned.
c
        delay_chan(ichan)=0.0
c       if (chan(ichan).ne.0)   then  ! channel exist in real data
          write(6,'(a,a,a,f7.3)')
     *    ' At station ',stat(ichan),', the P-residual is :',p_residual
          write(6,'(a)')
     *  ' By how much do you want to delay the data trace ?'
          write(6,'(a$)') 
     &   ' Return for P-residual delay, which align P-phases '
           read(5,'(a)') text

          if(check.eq.1) write(6,*) c_delay
          if(text.eq.' ')text='0.00001'     ! to cheat the routine to think there is a number 
          c_delay=sei real num(text, code)
c          if(text(1:4).eq.'    ') then
          if(c_delay.lt.0.000001) then
              delay_chan(ichan)=-p_residual
          else
              delay_chan(ichan)=c_delay
          endif
c          write(6,*)' Delay used:',delay_chan(ichan)
c
c   do not delay the synthetic traces
c
        delay_chan(ichan+1)=0.0
c
      endif
c
c   find channel number corresponding to station and
c   component, also get header time
c
         if (exist) then
             ierr=0
c
c
c   find channel, chan(ichan), with desired stat and component
c 
           call wav_find_chan(stat(ichan),compd(ichan),chan(ichan))
           if(chan(ichan).eq.0) ierr=1
c
           if(ierr.eq.1) then
              error=error+1
              write(6,'(1x,a5,a4,1x,a)')
     *        stat(ichan),compd(ichan),' Not found in trace file(s)'
           endif
         endif
         if(.not.exist.or.ierr.eq.1) then
c           write(6,*)' Cannot find waveform file',ierr
           chan(ichan)=0
	   cinter(ichan)=0.0
	   tstart(ichan)=0.0
         else
            if(check.eq.1) write(6,*)'chan in wav file',chan(ichan)
c
c
c   get sample rate and number of samples
c
             rate=wav_rate(chan(ichan))
             nsamp=wav_nsamp(chan(ichan))
c
c  get abs data channel start time
c
             wtime=wav_abs_time(chan(ichan))
c
c   calculate start time for window not counting the delay  relative to first
c   sample in trace data and check if window length should be reduced,
c   remember otime is window start time
c
cw           tstart(ichan)=otime-wtime - delay_chan(ichan)
           tstart(ichan)=otime-wtime
           
           if(check.eq.1) write(6,*) 'otime,tstart,delay', otime,
     *     tstart(ichan),delay_chan(ichan)

           delay(ichan)=0.0 

           cinter(ichan)=amin1(window,(nsamp-1)/rate-tstart(ichan))
c
c   case of no data at window start time so gap in start of data
c
           if(tstart(ichan).lt.0.0) then
              cinter(ichan)=amin1(window+tstart(ichan),(nsamp-1)/rate)
              delay(ichan)=abs(tstart(ichan))
              tstart(ichan)=0.0   ! must start with first sample 
           endif
c
c   the time shift due to residual must be added
c
            delay(ichan)=delay(ichan)+delay_chan(ichan)
c
         endif
c-----
c   put in data for synthetic channels
c-----
         if(check.eq.1) write(6,*) 'syn,t0,t0sy,del_chan', 
     *   t0,t0synt(jstat(ichan+1)),delay_chan(ichan+1)
         
         tstart(ichan+1)=t0-t0synt(jstat(ichan+1))-delay_chan(ichan+1)

         delay(ichan+1)=0.0

         cinter(ichan+1)=amin1(window,dtt*(nsynt-1)-tstart(ichan+1))

         if(tstart(ichan+1).lt.0.0) then
            cinter(ichan+1)=amin1(window+tstart(ichan+1),dtt*(nsynt-1))
            delay(ichan+1)=abs(tstart(ichan+1))
            tstart(ichan+1)=0.0
         endif
c
 1000 continue
c
c   make main header and file name, use window start time as start time
c
      net_code='     '
      call mfhead(oyear,omonth,oday,ohour,omin,osec,window,nchan*2,
     *           net_code,stat,comp,delay,cinter,file,mhead)
c
c   get vital info for header
c
      do i=1,ndata
         if(data(i)(2:15).eq.'SYNT: ST-D-RK:') 
     &         read(data(i),'(15x,3f10.2)') strike, dip, rake
         if(data(i)(2:15).eq.'SYNT: DEPTH--:') 
     &         read(data(i),'(15x,f10.2)') depth
      enddo
      istrike=strike+0.5
      idip=dip+0.5
      irake=rake+0.5
c 
      write(mhead(1)(2:30),271) depth,istrike,idip,irake,nsynt
 271  format('H',f5.1,1x,'SDR',i4,i3,i4,1x,'N',i4)

      chr_f_form$ = 'unformatted'


      if (codsyn.eq.'SB ')  
     *    call sei open( unknown$,                ! Open file.
     &                   ' ',                     ! No prompt.
     &                   'bousei.out',            ! On this file.
     &                   unit_out,                ! On this unit.
     &                   b_flag,                  ! File exists?
     &                   code )                   ! Returned condition.

      if (codsyn.eq.'SH ')  
     *    call sei open( unknown$,                ! Open file.
     &                   ' ',                     ! No prompt.
     &                   'hersei.out',            ! On this file.
     &                   unit_out,                ! On this unit.
     &                   b_flag,                  ! File exists?
     &                   code )                   ! Returned condition.
      if (codsyn.eq.'SW ')  
     *    call sei open( unknown$,                ! Open file.
     &                   ' ',                     ! No prompt.
     &                   'wkbjsei.out',           ! On this file.
     &                   unit_out,                ! On this unit.
     &                   b_flag,                  ! File exists?
     &                   code )                   ! Returned condition.
c
c   read channels and write out, first header
c
c
      write(6,'(1x,a)') mhead(1)(1:70)
      do i=1,12
         write(unit_out)mhead(i)
      enddo
c
c
c   write remaining main header lines if more than 30
c
         if(nchan.gt.30) then
            k=(nchan-31)/3+1
            do i=13,k+12
               write(unit_out) mhead(i)
            enddo
         endif
c
c-------------------------------------------------------------------------------
c   channel writing loop
c-------------------------------------------------------------------------------
c
      do 2000 ichan=1,2*nchan,2

c
      kchan=ichan
      ilec=0
c
c      if transverse, skip the reading
c      if radial, read the EAST component after having read the NORTH
c
      if (comp(ichan)(4:4).eq.'T')  goto 61
  60  continue
      if (comp(ichan)(4:4).eq.'R'.and.ilec.eq.1) kchan=ichan+2
c
c   rewind output file from main program
c
      rewind 20
c
c   read one trace
c
c
c   if the channel is there, then read it, else generate some data
c
      if(chan(kchan).ne.0) then
c
c  make seisan channel header for this channel, seems to be used
c
           call wav_sheads(chan(kchan),' ',text,mmmhead,chead)
c
c   read channel
c
           call wav_read_channel(chan(kchan))
c
c   get sample rate and number of samples
c
           rate=wav_rate(chan(kchan))
           nsamp=wav_nsamp(chan(kchan))

         if(check.eq.1) write(6,*)'rate,nsamp',rate,nsamp
c
c   select window
c
         n1=tstart(kchan)*rate+1    ! first sample to use
         n2=(tstart(kchan)+cinter(kchan))*rate+1

         if(check.eq.1) write(6,*) 'first and last point',n1,n2

         j=0
         do i=n1,n2
           j=j+1
           signal(j)=signal1(i)
         enddo
         n=j
      else                 
c
c   data not found, put something so rest can plot
c
         do i=1,1040
           chead(i:i)=' '
         enddo
         chead(1:5)=stat(kchan)
         chead(6:9)=comp(kchan)
         signal(1)=0
         n=1
         rate=50.0
      endif
      if (.not.radial.or.compd(kchan)(4:4).eq.'Z')  then
        cheadsave=chead
        goto 70
      endif
c
c
      if (radial.and.compd(kchan)(4:4).eq.'N')   then
        do i=1,n
          signaln(i)=signal(i)
        enddo
        cheadsave=chead
        ilec=1
        goto 60
      endif
      if (radial.and.compd(kchan)(4:4).eq.'E')   then
c           verify that the two components have compatible parameters
        if(chead(1:5).ne.cheadsave(1:5).or.
     *      chead(10:77).ne.cheadsave(10:77))    then
          write (*,'(a)') 'incompatible headers for horizontal compo:'
          write (*,'(a)') cheadsave(1:77)
          write (*,'(a)') chead(1:77)
          stop
        endif
c           verify that the transfer fonctions are similar at 5%
          read (cheadsave(241:249),'(g8.3)')  ampln
          read (chead(241:249),'(g8.3)')  ample
c              write (*,'(a,a,2g8.3)')
c     *        'transfer functions at ',stat(ichan),ampln,ample
          if(ampln.eq.0..and.ample.eq.0.)   goto 65
          dampl=2.*(ampln-ample)/(ampln+ample)
          if (dampl.gt.0.05) write (*,'(a,a,2g8.3)')
     * 'transfer functions quite different at ',stat(ichan),ampln,ample
 65       continue
        do i=1,n
          signale(i)=signal(i)
        enddo
      endif
c   put to zero the part of the header concerning instrument
      do i=81,1040
        cheadsave(i:i)=' '
      enddo
c
c    reading the backazimuth
c
      do i=1,ndata
        if(stat(ichan).eq.data(i)(17:21).and.
     *      data(i)(47:55).eq.' BAZIMUT:'.and.data(i)(80:80).eq.'3')
     *      read(data(i)(56:65),'(f10.1)') backazi
      enddo
      caz = -cos(backazi*3.14159/180.)
      saz = -sin(backazi*3.14159/180.)
c
c       making the radial component (away from the source)
c
      do i=1,n
        signal(i) = caz*signaln(i) + saz*signale(i)
      enddo
      goto 62
 61   continue
c
c       making the transversal component
c
      do i=1,n
        signal(i) = -saz*signaln(i) + caz*signale(i)
      enddo
 62   continue
c
c  modify channel header for new start time, which should be start of window
c  corrected for the time shift due to residual delay and start of window
c  realtive to start of waveform data.
c
c
c  write the data elements in cheadsave, because chead is used to write
c  the elements related to the synthetics, and we have to write
c  alternatively data R, synthetics R, data E and synthetics E
c  (therefore some mix-up occurs if one uses chead in all cases)
c
 70   continue

       if(chan(kchan).ne.0) then
          stime=wav_abs_time(chan(kchan))+tstart(kchan)+
     *    delay_chan(kchan)
c
c  abs date channel output
c
          call sectim
     *    (stime,outyear,outdoy,outmonth,outday,outhour,outmin,outsec)
       else
c
c  if no data, use main header time
c
          outyear=oyear
          outmonth=omonth
          outdoy=odoy
          outday=oday
          outhour=ohour
          outmin=omin
          outsec=osec
       endif

c
c   write in header
c
      write(cheadsave(10:12),'(i3)') outyear-1900
      write(cheadsave(14:16),'(i3)') outdoy
      write(cheadsave(18:19),'(i2)') outmonth
      write(cheadsave(21:28),'(i2,1x,i2,1x,i2)') outday,outhour,outmin
      write(cheadsave(30:35),'(f6.3)') outsec
      write(cheadsave(45:50),'(i6)') n
      write(cheadsave(37:43),'(f7.2)') rate
      cheadsave(6:9)=comp(ichan)
c
c   write channel header for the data
c
      write(unit_out) cheadsave
      write(6,'(1x,a)') cheadsave(1:70)
c
c   convert to integers
c
      if(cheadsave(77:77).eq.'4') then
         do i=1,n
           isignal(i)=signal(i)
         enddo
         write(unit_out) (isignal(i),i=1,n)
      else
         do i=1,n
            isignal2(i)=signal(i)
         enddo
         write(unit_out) (isignal2(i),i=1,n)
      endif
c
c-------------------
c   find synthetics in output file and write
c---------------------
c   only check that component is ok,
c   assume that stations are in correct order
c   synthetic is unit 20 irrespective which synthetics: wkbj, bouchon or herrmann
c-------------------------
c
c
 700  continue
      read(20,'(a)',end=701) text
        if(text(1:5).eq.stat(ichan).and.
     *     text(9:9).eq.comp(ichan+1)(4:4)) then
           read(20,*) (signal(i),i=1,nsynt)
        else
           goto 700
        endif
      goto 702
  701 continue
      write(6,'(1x,a5,a4,a)') stat(ichan),comp(ichan+1),
     *              ' Not found in synthetic file, will zero'
      do i=1,nsynt
        signal(i)=0
      enddo
  702 continue
c
c   write synthetic channel
c
      chead(6:9)=comp(ichan+1)
      syntrate=1./dtt
      write(chead(37:43),'(f7.2)') syntrate
      do i=81,1040
        chead(i:i)=' '
      enddo
c
c   indicate 4 byte integers
c
      chead(77:77)='4'
c
c  calculate header time for synthetic
c
      if(check.eq.1) write(6,*)'otime syn, delay syn',otime,
     *delay(ichan+1)

      stime=otime+delay(ichan+1)
      call sectim(stime,syear,sdoy,smonth,sday,shour,smin,ssec)
      write(chead(10:35),'(i3,1x,i3,1x,4(i2,1x),f6.3)')
     *               syear-1900,sdoy,smonth,sday,shour,smin,ssec
c
c   shift synthetic signal according to delay
c
      n1=tstart(ichan+1)*syntrate+1
      n2=(tstart(ichan+1)+cinter(ichan+1))*syntrate+1
      if(check.eq.1) write(6,*)' syn n1,n2', n1,n2
      j=0
      do i=n1,n2
        j=j+1
        isignal(j)=signal(i)
      enddo

      ns=j

      write(chead(45:50),'(i6)') ns
c
c  write header and data
c
      write(unit_out) chead
      write(6,'(1x,a)') chead(1:78)
      write(unit_out)(isignal(i),i=1,ns)

      if(check.eq.1) write(6,*)
     *' rate,tstart,ns,nsynt',syntrate,tstart(ichan+1),ns,nsynt
      if(check.eq.1) write(6,*) ' signal',(signal(i),isignal(i),i=1,10)

 2000 continue
c
c   close files
c
cw      if(exist)call sei close( close$, wav_unit, code ) ! Close (Default stop on error).
c     call sei close( close$, unit_out, code ) ! Close (Default stop on error).
      close(unit_out)

      return
      end
c
c----------------------------------------------------------------------------
