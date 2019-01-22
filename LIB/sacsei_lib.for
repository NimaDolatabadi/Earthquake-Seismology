
c changes:
c
c  nov 13, 2001 jh : wrong argument to set_def_chan, check for null chars 
cv                   in stat code
c  dec 18  2002 jh : station and component check not implemented in 
c                    read_sacasc_to_seisan
c  may 8 2003 lo   : check for sacaux in reading routine
c  apr 11 2006 lot : dont set beg to 0 if negative
c  dec 2  2006 lot : new sac routines
c  oct 5  2007 lot : add byte swap for reading
c  nov 14   07 lot : add network code to read/write binary
c  jan 14   09 lot : write out origin time and phases to sac headers
c  mar 22   10 lot : changed sac bin reading to set if haeder variables should be set
c  may 19   11 lot : check if number of samples times delta matches duration
c  jun 18   12 lot : set seisan output component to ' ' if not defined in sac (otherwise -123)
c  oct 23   15 lot : change to outfile name for cont data
c

      subroutine write_seisan_to_sacbin(ichan,keep_path,
     *          wav_out,nerr)
c     *          wav_out,eqla,eqlo,eqel,nerr)
c 
c routine to write sac file for trace of data read by  wav_read_channel
c data and header are known from the wav common block, the SAC libraries
c are required
c
c input:  ichan - channel id from Seisan WAV structure
c         keep_path - true if path remains in output file, otherwise false
c         wav_out - true if data in wav_out structure
c         eqla,eqlo,eqel - event lat, lon and elevation
c         nerr - error code given by SAC, 0 if ok
c
c Lars Ottemoeller, 11 May 2000
c

      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      include 'rea.inc'
      integer seiclen         ! function to get length of string
      integer jday            ! julian day
      integer ichan           ! trace index
      logical keep_path       ! true if complete path in output file
      logical wav_out         ! true if wav_out
      integer i,j             ! counters
      real stla,stlo,stel     ! station location
      character*80 outfile    ! output file name
      character*80 infile     ! input file name
      real cmpaz,cmpinc       ! channel orientation
      real eqla,eqlo,eqel     ! event location
      integer nerr            ! sac header error code
      real data_x(1)          ! x axis data, not used if data evenly spaced
      logical set_comp        ! true if cmpaz and cmpinc set
      integer ind             ! index
      character*1 dchar       ! directory delim.
      real beg                ! begin time
      parameter (beg=0.)
      integer year,month,day,hour,min
      real sec
      character*5 stat
      character*4 comp,comp_org
      character*2 network
      real rate
      integer nsamp
      real duration
      character*80 deffile
      integer nsacphase
      character*3 sacphase
      double precision abs_time,hyp_abs_time
      character*5 net_code              ! network code
      real diff
      character*29 mainhead_text
	common/sachdr/rhdr,ihdr,chdr
	real*4 rhdr(70)
	integer*4 ihdr(40)
	character*8 chdr(24)

      data_x(1) = 1

      call dir_char(dchar)
c
c   get def file for station codes, give file name
c
      deffile='sacsei.def'
c      no_net = .FALSE.
      net_code=' '

      call read_def_chan(deffile,mainhead_text,net_code)

c
c read variables
c
      if (wav_out) then
        year=wav_out_year(ichan)
        month=wav_out_month(ichan)
        day=wav_out_day(ichan)
        hour=wav_out_hour(ichan)
        min=wav_out_min(ichan)
        sec=wav_out_sec(ichan)
        stat=wav_stat(wav_out_chan(ichan))
        comp=wav_comp(wav_out_chan(ichan))
        network=wav_network(wav_out_chan(ichan))
        infile = wav_filename(wav_file_nr_chan
     *                       (wav_out_chan(ichan)))
        nsamp=wav_out_nsamp(ichan)
        rate=wav_out_rate(ichan)
        duration=wav_out_duration(ichan)
        abs_time=wav_abs_time(ichan)
      else
        year=wav_year(ichan)
        month=wav_month(ichan)
        day=wav_day(ichan)
        hour=wav_hour(ichan)
        min=wav_min(ichan)
        sec=wav_sec(ichan)
        stat=wav_stat(ichan)
        comp=wav_comp(ichan)
        network=wav_network(ichan)
        infile = wav_filename(wav_file_nr_chan(ichan))
        nsamp=wav_nsamp(ichan)
        rate=wav_rate(ichan)
        duration=wav_duration(ichan)
        abs_time=wav_abs_time(ichan)
      endif
c
c get julian day
c
      call date_doy(jday,day,month,year)

c
c convert comp and station name
c
      comp_org=comp
      call set_def_chan(1,stat,comp) 

c
c get station location
c
      call stat_loc(stat,'0',stla,stlo,stel)

c
c make sac file name
c
         outfile = ' '

         outfile= infile(1:seiclen(infile)) 
         if (.not.keep_path) then
           ind=-1
           do i=seiclen(outfile),1,-1
             if (outfile(i:i).eq.dchar.and.ind.eq.-1) then
               ind=i+1
             endif
           enddo
           if (ind.ne.-1) then
             outfile = outfile(ind:seiclen(outfile))
           endif
         endif
c  
c overwrite infile, lo 23/10/2015
c     
         write(outfile(1:4),'(i4)') year 
         write(outfile(6:7),'(i2.2)') month 
         write(outfile(9:10),'(i2.2)') day 
         write(outfile(12:13),'(i2.2)') hour 
         write(outfile(14:15),'(i2.2)') min 
         write(outfile(17:18),'(i2.2)') int(sec)
         outfile = outfile(1:seiclen(outfile)) // '_'
     &         // stat(1:5) // '_' //
     &           comp(1:4)  // '_SAC'

         j= seiclen(outfile)
         do i=1,j
           if (outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         write(6,*) ' SAC output filename: ',
     *       outfile(1:seiclen(outfile))

c
c write sac header
c
        call newhdr
c number of points
        call setnhv('NPTS',nsamp,nerr)
c beginning and ending value of data
        call setfhv('B',beg,nerr)
        call setfhv('E',duration,nerr)
c type of file
        call setihv('IFTYPE','ITIME',nerr)
c evenly spaced
        call setlhv('LEVEN',.true.,nerr)
c sample rate
        call setfhv('DELTA',1/rate,nerr)
c date and time
c        call setihv('IZTYPE','IB',nerr) ! new
        call setnhv('NZYEAR',year,nerr)
        call setnhv('NZJDAY',jday,nerr)
        call setnhv('NZHOUR',hour,nerr)
        call setnhv('NZMIN',min,nerr)
        call setnhv('NZSEC',int(sec),nerr)
        call setnhv('NZMSEC',int(1000.*(sec-
     *        int(sec))),nerr)
c station coord
        call setfhv('STLA',stla,nerr)
        call setfhv('STLO',stlo,nerr)
        call setfhv('STEL',stel,nerr)
c station and comp name
        call setkhv('KSTNM',stat,nerr)
        call setkhv('KCMPNM',comp,nerr)
c network code
        call setkhv('KNETWK',network,nerr)
c magnitude
         if (hyp_mag_all(1).ne.-999) then
           call setfhv('MAG',hyp_mag_all(1),nerr)
         endif
c hyp
c        if (eqla.ne.0.or.eqlo.ne.0) then
c          call setfhv('EVLA',eqla,nerr)
c          call setfhv('EVLO',eqlo,nerr)
c          call setfhv('EVEL',eqel,nerr)
c        endif
        if (hyp_lat(1).ne.0.or.hyp_lon(1).ne.0) then
          call setfhv('EVLA',hyp_lat(1),nerr)
          call setfhv('EVLO',hyp_lon(1),nerr)
          call setfhv('EVDP',hyp_depth(1),nerr)
c
c origin time
c
          call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     *      hyp_hour(1),hyp_min(1),hyp_sec(1),hyp_abs_time)
          diff=hyp_abs_time-abs_time
          call setfhv('O',diff,nerr)
          call setkhv('KO','OT',nerr)
        endif
c
c check for phases, add to sac header
c
        nsacphase=0
        do i=1,rea_nphase
          if (stat.eq.rea_stat(i).and.
     &        comp_org(1:1).eq.rea_co(i)(1:1).and.
     &        comp_org(4:4).eq.rea_co(i)(2:2).and.
     &        nsacphase.le.9) then
            write(sacphase,'(a1,i1)') 'T',nsacphase
            diff=rea_abs_time(i)-abs_time
            call setfhv(sacphase(1:2),diff,nerr)
            write(sacphase,'(a2,i1)') 'KT',nsacphase
            call setkhv(sacphase,rea_phase(i),nerr)
            nsacphase=nsacphase+1
          endif
        enddo
 
        set_comp = .false.

c az and ic of components
        if (comp(4:4).eq.'Z'.or.comp(3:3).
     *            eq.'Z') then
          cmpaz=0.
          cmpinc=0. 
          set_comp=.true.
        elseif (comp(4:4).eq.'N'.or.comp(3:3).
     *            eq.'N') then
          cmpaz=0.
          cmpinc=90.
          set_comp=.true.
        elseif (comp(4:4).eq.'E'.or.comp(3:3).
     *            eq.'E') then
          cmpaz=90.
          cmpinc=90.
          set_comp=.true.
        endif
        if (set_comp) then
          call setfhv('CMPAZ',cmpaz,nerr)
          call setfhv('CMPINC',cmpinc,nerr)
        endif

c
c write sac data
c
        call wsac0(outfile,data_x,signal1,nerr)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine read_sacbin_to_seisan(infile,ichan,lhead,nerr)
c
c   read binary sac
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'
      integer seiclen
      integer ichan               ! channel id
      integer lhead               ! read into wav structure when 1
      character*80 infile         ! input file name
      integer nerr                ! sac error code
      real del                    ! delta 
      real max                    ! max number of samples from rsac
      real beg,en                 ! beginning and end of trace
      integer jday                ! julian day
      integer isec,msec           ! sec and millisec
      double precision dpsec      ! absolute time
      integer i                   ! counter
      integer nlen
      character*80 deffile
      character*29 mainhead_text
      character*5 net_code      
      character*80 sacaux
      integer swap
	common/sachdr/rhdr,ihdr,chdr
	real*4 rhdr(70)
	integer*4 ihdr(40)
	character*8 chdr(24)

c      write(*,*) ' reading sacbin '
c
c check if SACAUX defined
c
c      call get_env_sacaux(sacaux)
c     if (seiclen(sacaux).le.0) then
c      return
c      endif

c
c   get def file for station codes, give file name
c
      deffile='sacsei.def'
      net_code=' '

      call read_def_chan(deffile,mainhead_text,net_code)

c
c set defaults      
c
      wav_cbyte(ichan)='4'

      beg=0.
      max=max_sample
      swap=0
c      write(*,*) ' attempting to read sac no byte swapping '
      call rsac1(infile,signal1,nlen,beg,del,max,swap,nerr)
c
c attemt byte swapping
c
      if(nerr.ne.0) then
c        write(*,*) ' attempting to read sac byte swapping '
        swap=1
        call rsac1(infile,signal1,nlen,beg,del,max,swap,nerr)
      endif

      if(nerr.ne.0) then
        nerr=-1
        return
      endif

      if (lhead.eq.0) return  ! lo 22/03/2010
c
c get header values
c

c number of points
        call getnhv('NPTS',wav_nsamp(ichan),nerr)
c begining and ending value of data
        call getfhv('B',beg,nerr)
c 11/04/2006 lot change to add to start time
c          if (beg.lt.0.0001) beg = 0.
        call getfhv('E',en,nerr)
c sample rate
        call getfhv('DELTA',del,nerr)
        wav_rate(ichan) = 1/del
c station and comp name
        call getkhv('KSTNM',wav_stat(ichan),nerr)
c
c check for null
c
        do i=1,5
           if(ichar(wav_stat(ichan)(i:i)).eq.0) wav_stat(ichan)(i:i)=' '
        enddo
c
        call getkhv('KCMPNM',wav_comp(ichan),nerr)
        if (wav_comp(ichan).eq.'-123') wav_comp(ichan)=' '
        do i=seiclen(wav_comp(ichan))+1,4
           wav_comp(ichan)(i:i) = ' '
        enddo
        call set_def_chan(ichan,wav_stat(ichan),wav_comp(ichan))
        do i=seiclen(wav_network(ichan))+1,2
           wav_network(ichan)(i:i) = ' '
        enddo
c network code
        call getkhv('KNETWK',wav_network(ichan),nerr)
c set location code 
        wav_location(ichan)='  '
c date and time
        call getnhv('NZYEAR',wav_year(ichan),nerr)
          if (wav_year(ichan).lt.100) wav_year(ichan)=
     *        wav_year(ichan)+1900
        call getnhv('NZJDAY',jday,nerr)
        call dte(jday,wav_day(ichan),wav_month(ichan),wav_year(ichan))
        call getnhv('NZHOUR',wav_hour(ichan),nerr)
        call getnhv('NZMIN',wav_min(ichan),nerr)
        call getnhv('NZSEC',isec,nerr)
        call getnhv('NZMSEC',msec,nerr)
        wav_sec(ichan)=isec+msec/1000.

c
c add beg to absolute time
c
        call timsec(wav_year(ichan),wav_month(ichan),wav_day(ichan),
     *      wav_hour(ichan),wav_min(ichan),wav_sec(ichan),dpsec)
        dpsec=dpsec+beg
        call sectim(dpsec,wav_year(ichan),jday,wav_month(ichan),
     *    wav_day(ichan),wav_hour(ichan),wav_min(ichan),wav_sec(ichan))

        wav_abs_time(ichan)=dpsec
c
c duration
c
        wav_duration(ichan) = wav_nsamp(ichan) / wav_rate(ichan)
      return
      end



      subroutine read_sacasc_to_seisan(infile,ichan,nerr)

      implicit none
      include 'libsei.inc' 
      external sei open,                  ! Open file routine.
     &         sei close                  ! Close file routine
      integer  read1,                     ! Input unit 1
     &         code                       ! Local condition
      logical  bflag                      ! Flag end of file
      include 'seidim.inc'
      include 'waveform.inc'
      integer seiclen
      integer ichan               ! channel id
      character*80 infile         ! input file name
      character*80 text           ! input text line
      integer nerr                ! sac error code
      real del                    ! delta
      real max                    ! max number of samples from rsac
      real beg,en                 ! beginning and end of trace
      integer jday                ! julian day
      integer isec,msec           ! sec and millisec
      double precision dpsec      ! absolute time
      integer i,n,tt,stoploop     ! counter
      real rate                   ! sample rate
      character*80 deffile
      character*29 mainhead_text
      character*5 net_code

      n=0
      nerr=0
c
c   get def file for station codes, give file name
c
      deffile='sacsei.def'
      net_code=' '



c
c  open ascii file and read variables
c
        call sei open( old$+warn$,             ! Open file (stop on error).
     &                     ' ',                ! No prompt.
     &                     infile,             ! This filename.
     &                     read1,              ! On unit.
     &                     bflag,              ! Exists!!
     &                     code )     
   
        if (.not.bflag) then
          nerr=-1 
          return
        endif

c
c read from asc header
c
c
c line : 1
       read(read1,'(a)',err=998) text

       call SEI GET VALUES( 5, text, CODE )
       if (code.ne.0) goto 998

       rate=ARRAY$(1)
       wav_rate(ichan)=1/rate

c-- read lines
c skip until line 14
       do i=1,13
          read(read1,*,err=998)
       enddo

c-- read start time
c line 15
       read(read1,'(a)',err=998) text

       call SEI GET VALUES( 5, text, CODE )
       if (code.ne.0) goto 998

        wav_year(ichan)=ARRAY$(1)
        jday=ARRAY$(2)
        call dte(jday,wav_day(ichan),wav_month(ichan),wav_year(ichan))
        wav_hour(ichan)=ARRAY$(3)
        wav_min(ichan)=ARRAY$(4)
        isec=ARRAY$(5)

c line 16
        read(read1,'(a)') text
        call SEI GET VALUES( 5, text, CODE )
        msec=ARRAY$(1)
        wav_sec(ichan)=isec+msec/1000.

c-- number of samples
        wav_nsamp(ichan)=ARRAY$(5)

c-- skip until 22
        do i=1,6
          read(read1,*)
        enddo

c-- read station
       read(read1,'(a)') text
       read(text(1:5),'(a5)') wav_stat(ichan)
c
c check for null
c
        do i=1,5
           if(ichar(wav_stat(ichan)(i:i)).eq.0) wav_stat(ichan)(i:i)=' '
        enddo
   

c-- skip lines until 28
       do i=1,5
          read(read1,*)
       enddo

c-- read channel
c line 29
       read(read1,'(a)') text
       read(text(17:20),'(a4)') wav_comp(ichan)
c
c   fix station and component names
c
        call set_def_chan(ichan,wav_stat(ichan),wav_comp(ichan))

       read(read1,'(a)') text
c
c read waveform data
c
       stoploop=int(wav_nsamp(ichan)/5)+1
       do i=1,stoploop
          read(read1,'(a)',end=999) text

          do tt=1,80

            if (text(tt:tt).eq.'.') then
              if (text(tt+1:tt+7).eq.'0000000') then
                text(tt+1:tt+7)='       '
              elseif (text(tt+7:tt+7).eq.'0') then
                text(tt+7:tt+7)=' '
              endif
             endif
          enddo

c227545.0000000-381483.0000000-492502.0000000-646966.0000000-711058.0000000
c827626.0000000-1120953.0000000-1584441.0000000-1508225.0000000-880364.0000000
          if (n.LE.wav_nsamp(ichan)) THEN
             call SEI GET VALUES( 5, text, CODE )
             do tt=1,5
               n=n+1
               if (n.LE.wav_nsamp(ichan)) THEN
                 signal1(n)=ARRAY$(tt)
               endif
             enddo
          ELSE
            GOTO 999
          ENDIF

       enddo

      goto 999

  998 continue
      nerr=-1

  999 CONTINUE
      call sei close( close$, read1, code )
      if (nerr.ne.-1) then
c
c duration
c
        wav_duration(ichan) = wav_nsamp(ichan) / wav_rate(ichan)
        wav_chan_nr_file(ichan)=1
        wav_cbyte(ichan)='4'
      endif

      return
      end


        logical function check_sacbin (IRU,name,LN)
c       modified from brsac (Written by Hafidh A. A. Ghalib, 1988)
c
        integer*4 ihdr(40)
        real*4 rhdr(70)
        character*(*) name
        integer*4 swap
        integer i,j
        logical flag

        flag=.false.
c-----
c  Read real and integer header blocks to find actual number
c  of waveform data points which is stored in ihdr(10).
c
c-----
        nerr = 0
        open (IRU,file=name,form='unformatted',
     &                  access='direct',recl=440,status='old',err=999)
                        read (IRU,rec=1) (rhdr(i), i=1,70),
     &                                   (ihdr(i), i=1,40)
        close (IRU)
c
c swap bytes
c
        do i=0,1
          if (.not.flag) then
            if (i.eq.1.and..not.flag) then ! put swap back, lo 29/2/2012
c            if (swap.eq.1.and..not.flag) then
              call swap4(40,ihdr)
              call swap4r(70,rhdr)
            endif
c            do j=1,40
c              write(*,*) j,ihdr(j)
c            enddo
c            do j=1,70
c              write(*,*) j,rhdr(j)
c            enddo
            if((ihdr(10).le.LN.or.ihdr(10).gt.0).and. ! number of samples 
     &         (rhdr(1).gt..0001.and.rhdr(1).le.1000.).and. ! delta
     &         (rhdr(6).lt.rhdr(7)).and. ! interval b and e
     &         ((ihdr(10)-1)*rhdr(1)-(rhdr(7)-rhdr(6)).lt.1.)) ! duration
     &         flag = .true.
c      write(*,*) ' nsamp,delta ',ihdr(10),rhdr(1),rhdr(6),rhdr(7),
c     &  (ihdr(10)-1)*rhdr(1),rhdr(7)-rhdr(6)
          endif
        enddo
c        write(*,*) name,flag 

        check_sacbin = flag
999     continue
        end


