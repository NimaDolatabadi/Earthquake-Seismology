c
c   subroutines for parameter input - output
c
c   all parameters are going through common block in rea2.inc
c
c
c   rea2_event_in                : read one whole event
c
c   rea2_event_out               : write one whole event
c
c   rea2_hyp1_in                 : read  type 1 line
c
c   rea2_hyph_in                 : read high accuracy hypocenter line
c
c   rea2_hype_in                 : read error line
c
c   rea2_hyp1_out                : write  type 1 line
c
c   rea2_hyph_out                : write high accuracy hypocenter line
c
c   rea2_hype_out                : write error line
c
c   rea2_hypm_out                : write extra magnitudes
c
c   rea2_hyp_clear               : clear hypocenter parameters
c
c   rea2_phase_out               : write phase parameters to phase line
c
c   rea2_phase_in                : read phase parameters from phase line
c
c   rea2_phase_clear             : clear phase parameters
c
c   rea2_spec_out                : write spectral parameters to spec lines
c
c   rea2_av_spec_out             : write average spectral parameter to text line
c
c   rea2_spec_in                 : read  spectral parameters to memory
c
c   rea2_av_spec_out             : read average spectral parameters
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   General info:
c
c   The phase name DELETE is reserved. Giving a phase the name DELETE, will
c                         remove it at the next write out
c
c
c------------------------------------------------------------------------------
c changes:
c   20 jun 2008 pv : first version
c   28 oct 2008 jh : better output of cornerf
c   30 oct 2010 jh : pause before all stops
c   05 dec 2011 jh : sn to ain
c
      subroutine rea2_event_in(unit,all,data,code)
c
c   Read all data from one event in data array and puts it into common block
c   data that canot go into variables in common block are stored in rea2_data
c
c     input :   unit:  File unit to read from, if 0, read directly
c                      from data array, MAKE SURE TO PASS NRECORD AND
C                      NHEAD TO COMMON BLOCK BEFORE CALLL
c               all:   If true, read whole event, if false only headers
c               data:  S-file data array
c
c     output:   code:  0: Ok, 1: End of file, 2: Read error (not implemented)
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      integer unit
      logical all
      integer code
      integer nh,ne                   ! count high accurarcy and errror lines
      integer nphase                  ! as counted by indata
      integer nstat                   ! as counted by indata
      character*80 data(1000)
      character*80 text
      character*1 type,exp            ! for indata
      integer i,k

      ne=0
      nh=0
      code=0
      rea2_nphase=0
c
c  check if reading from a file
c
      if(unit.gt.0) then
         call indata
     *   (unit,nstat,nphase,rea2_nhead,rea2_nrecord,
     *   type,exp,data,k)
         if(rea2_nrecord.eq.0) then
             code=1
             return
         endif
      endif
c
c  now read all data, first headers, the main hypocenter (index 1) also
c  contains error info, high accuracy and extra magnitudes 
c
      rea2_nphase=0      ! count number of phase lines
      rea2_nhyp=0        ! count number of hypocenters
      rea2_nwav=0        ! --------------- waveform files
      rea2_ncomment=0    ! --------------- comment lines
      rea2_nmacro=0      ! --------------- macroseismic data
      rea2_nfault=0      ! --------------- fault plane solutions
      rea2_nspec=0       ! --------------- spectral solutions
      rea2_nmag=0        ! --------------- magnitudes
c
      i=1
      dowhile(i.le.rea2_nhead)
         if(data(i)(80:80).eq.'1'.or.i.eq.1) then  ! allow first line without 1
             rea2_nhyp=rea2_nhyp+1
             k=rea2_nhyp
             call rea2_hyp_clear(k)
             call rea2_hyp1_in(data(i),k)
             goto 10
         endif
         if(data(i)(80:80).eq.'H') then
            call rea2_hyph_in(data(i),1)     ! put high precision info in 1
            nh=nh+1
            goto 10
         endif
         if(data(i)(80:80).eq.'E') then
            call rea2_hype_in(data(i),1)        ! put error info in 1
            ne=ne+1
            goto 10
         endif
c
c   check for additional magnitudes, are in type 1 line with same first
c   23 chars and same hypocenter agency as the prime solution
c
         if(data(i)(1:23).eq.data(1)(1:23).and.data(i)(46:48).eq.
     *      data(1)(46:48).and.data(i)(80:80).eq.'1'.and.i.gt.1) then
            call rea2_hyp_clear(100)
            call rea2_hyp1_in(data(i),100)  ! temporarely use index 100
            do k=1,3                       ! put into last 3 positions
c               hyp2_mag(i+3,1)=hyp2_mag(100,i)  ! changed lo, may 2001
               hyp2_mag(i+3,1)=hyp2_mag(6,i)
c               hyp2_mag_type(i+3,1)=hyp2_mag_type(100,i)
               hyp2_mag_type(i+3,1)=hyp2_mag_type(6,i)
c               hyp2_mag_agency(i+3,1)=hyp2_mag_type(100,1)
               hyp2_mag_agency(i+3,1)=hyp2_mag_agency(6,1)
            enddo
            call rea2_hyp_clear(100)            ! clear again
            goto 10
         endif
         
c
c   spectral average line
c
         if(data(i)(80:80).eq.'3'.and.data(i)(2:13).eq.
     *   'SPEC AVERAGE') then        ! changed from AVARAGE lo, May 2001
            call rea2_av_spec_in(data(i))
            goto 10
         endif
c
c   spectral station lines
c
         if(data(i)(80:80).eq.'3'.and.data(i)(2:5).eq.'SPEC'.and.
     *      data(i)(7:13).ne.'AVERAGE') then
            if(data(i+1)(80:80).eq.'3'.
     *      and.data(i+1)(2:5).eq.'SPEC') then
               rea2_nphase=rea2_nphase+1
               k=rea2_nphase
               call rea2_spec_in(k,data(i),data(i+1))
               rea2_nspec=rea2_nspec+1
               i=i+1                      ! there are 2 lines
               goto 10
            endif
         endif
c
c   id line
c
         if(data(i)(80:80).eq.'I') then
            rea2_id_line=data(i)
            goto 10
          endif
c
c   fault plane solutions
c
         if(data(i)(80:80).eq.'F') then
            rea2_nfault=rea2_nfault+1
            rea2_fault(rea2_nfault)=data(i)
            goto 10
         endif
c
c    waveform file names
c
         if(data(i)(80:80).eq.'6') then
             rea2_nwav=rea2_nwav+1
             rea2_wav(rea2_nwav)=data(i)
             goto 10
         endif
c
c    macroseismic info
c
         if(data(i)(80:80).eq.'2') then
             rea2_nmacro=rea2_nmacro+1
             rea2_macro(rea2_nmacro)=data(i)
             goto 10
         endif

c
c    comment lines
c
         if(data(i)(80:80).eq.'3') then
            rea2_ncomment=rea2_ncomment+1
            rea2_comment(rea2_ncomment)=data(i)
         endif


 10      continue
         i=i+1
c
      enddo
c
c   check that there was not too many high accuracy lines or error lines
c
      if(ne.gt.1.or.nh.gt.1) then
          write(6,*) ' Too many error or high accuracy lines'
          write(6,'(a)') data(i)(1:79)
          write(6,*)' Enter to continue'
          read(5,'(a)') text
cjh apr 08          stop
      endif
c
c   count total number of magnitudes and put into one array
c
      rea2_nmag=0
      do i=1,rea2_nhyp
        do k=1,6
           if(hyp2_mag(k,i).gt.-20.0) then
              rea2_nmag=rea2_nmag+1
              hyp2_mag_all(rea2_nmag)=hyp2_mag(k,i)
              hyp2_mag_type_all(rea2_nmag)=hyp2_mag_type(k,i)
              hyp2_mag_agency_all(rea2_nmag)=hyp2_mag_agency(k,i)
           endif
        enddo
      enddo
         

c
c   read phases if specified
c
      if(all) then
         do i=rea2_nhead+1,rea2_nrecord
            if(data(i).ne.' ') then
               rea2_nphase=rea2_nphase+1
               call rea2_phase_clear(rea2_nphase)          ! clear variables
               call rea2_phase_in(rea2_nphase,data(i))     ! read
            else
               if(i.ne.rea2_nrecord) then                 ! last can be blank
                  write(6,*)' Blank line in phase data'
                  write(6,'(a)')data(1)(1:79)
                  call rea_stop
               endif
            endif
c
c   check if last record is blank as it should be, if not, add one
c
            if(data(rea2_nrecord).ne.' ') then
               rea2_nrecord=rea2_nrecord+1
               data(rea2_nrecord)=' '
            endif
         enddo
      endif
c
c   calcualte abs times if phases read and header read
c
      if(rea2_nphase.gt.1.and.all) then
         do i=1,rea2_nphase
             call timsec(hyp2_year(1),hyp2_month(1),hyp2_day(1),
     *       rea2_hour(i),rea2_min(i),rea2_sec(i),rea2_abs_time(i))
          enddo
      endif

c
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_event_out(unit,all,data,code)
c
c   Write all data to data array and optinally to file also. Variables
c   all come from common block.
c
c
c     input :   unit:  File unit to write from, if 0, only write
c                      to data array
c               all:   If true, read whole event, if false only headers
c               
c
c     output:   data:  S-file data array
c               code:  0: Ok, 1: Write error (not implemented)
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      integer unit,code
      logical all
      character*80 data(1000)
      integer i,k

c
c  now write all data, first main header, the main hypocenter (index 1) also
c  contains error info, high accuracy and extra magnitudes 
c
      rea2_nrecord=0
c
c   there must be at least one hypo line
c
      if(rea2_nhyp.le.0) then
           write(6,*)' No hypocenter lines for output'
           call rea_stop
      endif
      do i=1,rea2_nhyp
         rea2_nrecord=rea2_nrecord+1
         call rea2_hyp1_out(data(rea2_nrecord),i)
c
c   for first prime hypocenter, there could be additional info
c
         if(i.eq.1) then
c
c   there could be an addiitonal high accuracy line
c
             if(hyp2_high_accuracy(i)) then
                 rea2_nrecord=rea2_nrecord+1
                 call rea2_hyph_out(data(rea2_nrecord),i)
             endif
c
c   there could be an error line
c
             if(hyp2_error(i))  then
                 rea2_nrecord=rea2_nrecord+1
                 call rea2_hype_out(data(rea2_nrecord),i)
             endif
c
c   check for additonal magnitudes, are in type 1 line with same first
c   23 chars and same hypocenter agency as the prime solution
c
             if(hyp2_mag(4,1).gt.-100.0.or.hyp2_mag(5,1).gt.-100.0.or.
     *       hyp2_mag(6,1).gt.-100.0) then
                rea2_nrecord=rea2_nrecord+1
                call rea2_hypm_out(data(rea2_nrecord),i)
             endif
          endif
      enddo
c
c   spectral average line
c
      if(rea2_av_moment.gt.0.0.or.rea2_av_omega0.gt.0.0) then
         rea2_nrecord=rea2_nrecord+1
         call rea2_av_spec_out(data(rea2_nrecord))
      endif
      
c
c   spectral lines  
c
      do i=1,rea2_nphase
         if(rea2_phase(i)(1:4).eq.'SPEC') then
             rea2_nrecord=rea2_nrecord+1
           call rea2_spec_out(i,data(rea2_nrecord),data(rea2_nrecord+1))
             rea2_nrecord=rea2_nrecord+1       ! there are 2 spectral lines
          endif
      enddo
c
c   fault plane solutions
c
      if(rea2_nfault.gt.0) then
          do i=1,rea2_nfault
             rea2_nrecord=rea2_nrecord+1
             data(rea2_nrecord)=rea2_fault(i)
          enddo
      endif
c
c    waveform file names
c
      if(rea2_nwav.gt.0) then
          do i=1,rea2_nwav
             rea2_nrecord=rea2_nrecord+1
             data(rea2_nrecord)=rea2_wav(i)
          enddo
      endif

c
c    macroseismic info
c
      if(rea2_nmacro.gt.0) then
          do i=1,rea2_nmacro
             rea2_nrecord=rea2_nrecord+1
             data(rea2_nrecord)=rea2_macro(i)
          enddo
      endif
c
c    comment lines
c
      if(rea2_ncomment.gt.0) then
          do i=1,rea2_ncomment
             rea2_nrecord=rea2_nrecord+1
             data(rea2_nrecord)=rea2_comment(i)
          enddo
      endif
c
c   id line
c
      if(rea2_id_line.ne.' ') then
         rea2_nrecord=rea2_nrecord+1
         data(rea2_nrecord)=rea2_id_line
      endif
c
c    phase explanation line
c
      rea2_nrecord=rea2_nrecord+1
      data(rea2_nrecord)=
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO '//
     *'AIN AR TRES W  DIS CAZ7'
c     *'SNR AR TRES W  DIS CAZ7'
c
c   number of header lines
c
       rea2_nhead=rea2_nrecord
c
c   write phases if specified, spectral related phases not written
c
      if(all) then
         do i=1,rea2_nphase
            if(rea2_phase(i)(1:4).ne.'SPEC'.and.rea2_phase(i)(1:6).ne.
     *         'DELETE') then
               rea2_nrecord=rea2_nrecord+1
               call rea2_phase_out(i,data(rea2_nrecord))     ! write
            endif
         enddo
      endif
c
c   make last record blank
c
      rea2_nrecord=rea2_nrecord+1
      data(rea2_nrecord)=' '
c
c   write out if flag set
c
       if(unit.gt.0) then
           write(unit,'(a)',err=100) (data(i),i=1,rea2_nrecord)
           goto 101
 100       continue
           write(6,*)' Error writing S-file data array, data'
           call rea_stop
 101       continue
      endif
c
      return
      end

                  
      subroutine rea2_hyp1_in(text,ihyp)
c
c   read all hypocenter info from type 1 line in text into
c   common block hypocenter array number ihyp
c   some checking is done
c
c   note: agencies are 5 chars of which only 3 is used
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp,seiclen
c
      if (seiclen(text(2:79)).le.0) goto 60
      if(text(80:80).ne.' '.and.text(80:80).ne.'1') then
         write(6,*) ' Not a type 1 line'
         call rea_stop
      endif
c      write(3,*)' in read1',ihyp,text(1:40)
c
c   read times
c
      err_text='hypocenter time'
      if(text(2:5).ne.' ')   read(text(2:5),   '(i4)',  err=50)
     *hyp2_year(ihyp)
      if(text(7:8).ne.' ' )  read(text(7:8),   '(i2)',  err=50)
     *hyp2_month(ihyp)
      if(text(9:10).ne.' ' ) read(text(9:10),  '(i2)',  err=50)
     *hyp2_day(ihyp)
      if(text(12:13).ne.' ') read(text(12:13), '(i2)',  err=50)
     *hyp2_hour(ihyp)
      if(text(14:15).ne.' ') read(text(14:15), '(i2)',  err=50)
     *hyp2_min(ihyp)
      if(text(17:20).ne.' ') read(text(17:20), '(f4.1)',err=50)
     *hyp2_sec(ihyp)
c      write(3,*)ihyp,hyp2_year(ihyp)

c
c   flags, check a bit
c
      hyp2_model(ihyp)=text(21:21)
      hyp2_dist_id(ihyp)=text(22:22)
      if(hyp2_dist_id(ihyp).ne.'L'.and.hyp2_dist_id(ihyp).ne.'R'.and.
     *hyp2_dist_id(ihyp).ne.'D') then
         err_text='distance id'
         goto 50
      endif
      hyp2_type(ihyp)=text(23:23)
      hyp2_fix_org(ihyp)=text(11:11)
      if(hyp2_fix_org(ihyp).ne.' '.and.hyp2_fix_org(ihyp).ne.'F') then
         err_text='origin time flag'
         goto 50
      endif
c
c  hypocenter
c
      err_text='hypocenter'
      if(text(24:30).ne.' ') read(text(24:30),
     *'(f7.3)',err=50) hyp2_lat(ihyp)
      if(text(31:38).ne.' ') read(text(31:38),
     *'(f8.3)',err=50) hyp2_lon(ihyp)
      if(text(39:43).ne.' ') read(text(39:43),
     *'(f5.1)',err=50) hyp2_depth(ihyp)
c
c   hypocenter flags
c
      hyp2_depth_flag(ihyp)=text(44:44)
      hyp2_epi_flag(ihyp)=text(45:45)
      if( hyp2_depth_flag(ihyp).ne.' '.and.hyp2_depth_flag(ihyp).ne.'F'.
     *and.hyp2_depth_flag(ihyp).ne.'S'.and.hyp2_epi_flag(ihyp)  .ne.' '.
     *and.  hyp2_epi_flag(ihyp).ne.'F'.and.hyp2_epi_flag(ihyp).  ne.'*')
     *then
          err_text='hypocenter flags'
          goto 50
      endif
c
c   agency
c
      hyp2_agency(ihyp)(1:3)=text(46:48)
c
c   number of stations
c
      err_text='number of stations'
      if(text(49:51).ne.' ')
     *read(text(49:51),'(i3)',err=50) hyp2_nstat(ihyp)
c
c   rms
c
      err_text='rms'
      if(text(52:55).ne.' ')
     *read(text(52:55),'(f4.1)',err=50) hyp2_rms(ihyp)
c
c   magnitudes
c
      err_text='magnitudes'
      if(text(56:63).ne.' ')
     *read(text(56:63),'(f4.1,a1,a3)',err=50)hyp2_mag(1,ihyp),
     *hyp2_mag_type(1,ihyp),hyp2_mag_agency(1,ihyp)(1:3)

      if(text(64:71).ne.' ')
     *read(text(64:71),'(f4.1,a1,a3)',err=50)hyp2_mag(2,ihyp),
     *hyp2_mag_type(2,ihyp),hyp2_mag_agency(2,ihyp)(1:3)

      if(text(72:79).ne.' ')
     *read(text(72:79),'(f4.1,a1,a3)',err=50)hyp2_mag(3,ihyp),
     *hyp2_mag_type(3,ihyp),hyp2_mag_agency(3,ihyp)(1:3)

c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' reading hyp line'
      write(6,'(a)') text
      call rea_stop

 60   continue
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_hyph_in(text,ihyp)
c
c   read all hypocenter info from type H line (high accuracy hypoenter
c   line) in text into common block hypocenter array number ihyp
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
      if(text(80:80).ne.'H') then
         write(6,*)' Not a high accuracy hypocenter line'
         call rea_stop
      endif
c
c   read times
c
      hyp2_high_accuracy(ihyp)=.true.
      err_text='hypocenter time'
      if(text(2:5).ne.' ')   read(text(2:5),   '(i4)',  err=50)
     *hyp2_year(ihyp)
      if(text(7:8).ne.' ' )  read(text(7:8),   '(i2)',  err=50)
     *hyp2_month(ihyp)
      if(text(9:10).ne.' ' ) read(text(9:10),  '(i2)',  err=50)
     *hyp2_day(ihyp)
      if(text(12:13).ne.' ') read(text(12:13), '(i2)',  err=50)
     *hyp2_hour(ihyp)
      if(text(14:15).ne.' ') read(text(14:15), '(i2)',  err=50)
     *hyp2_min(ihyp)
      if(text(17:23).ne.' ') read(text(17:23), '(f6.3)',err=50)
     *hyp2_sec(ihyp)

c
c  hypocenter
c
      err_text='hypocenter'
      if(text(24:32).ne.' ') read(text(24:32),
     *'(f9.5)',err=50) hyp2_lat(ihyp)
      if(text(34:43).ne.' ') read(text(34:43),
     *'(f10.5)',err=50) hyp2_lon(ihyp)
      if(text(45:52).ne.' ') read(text(45:52),
     *'(f8.3)',err=50) hyp2_depth(ihyp)
c
c   rms
c
      err_text='rms'
      if(text(54:59).ne.' ')
     *read(text(54:59),'(f6.3)',err=50) hyp2_rms(ihyp)
c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' reading hyp high acc. line'
      write(6,'(a)') text
      call rea_stop

 60   continue
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_hype_in(text,ihyp)
c
c   read all error hypocenter info from type E line (error info
c   line) in text into common block hypocenter array number ihyp
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
      if(text(80:80).ne.'E') then
         write(6,*)' Not a hypocenter error line'
         call rea_stop
      endif
c
      hyp2_error(ihyp)=.true.
c
c   gap
c
      err_text='gap'
      if(text(6:8).ne.' ') then
          read(text(6:8),'(f3.0)',err=50) hyp2_gap(ihyp)
      endif
c
c   read origin time error
c
      err_text='origin time error'
      if(text(15:20).ne.' ') read(text(15:20),'(f6.2)',err=50)
     *hyp2_sec_err(ihyp)
c
c  hypocenter errors
c
      err_text='hypocenter error'
      if(text(25:30).ne.' ') read(text(25:33),
     *'(f6.1)',err=50) hyp2_lat_err(ihyp)
      if(text(33:38).ne.' ') read(text(33:38),
     *'(f6.1)',err=50) hyp2_lon_err(ihyp)
      if(text(39:43).ne.' ') read(text(39:43),
     *'(f5.1)',err=50) hyp2_depth_err(ihyp)
c
c   covariance terms
c
      err_text='hypocent covariance'
      if(text(44:55).ne.' ')
     *read(text(44:55),'(e12.4)',err=50) hyp2_cov(1,ihyp)
      if(text(56:67).ne.' ')
     *read(text(56:67),'(e12.4)',err=50) hyp2_cov(2,ihyp)
      if(text(68:79).ne.' ')
     *read(text(68:79),'(e12.4)',err=50) hyp2_cov(3,ihyp)

c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' reading hyp error line'
      write(6,'(a)') text
      call rea_stop

 60   continue
      return
      end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_hyp1_out(text,ihyp)
c
c   write all hypocenter info to type 1 line text from
c   common block array number ihyp
c
c   note: agencies are 5 chars of which only 3 is used
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
c   clear
c
      text=' '
      text(80:80)='1'
c
c   write times
c
      err_text='hypocenter time'
      if(hyp2_year(ihyp). gt.0  ) write(text(2:5),   '(i4)',  err=50)
     *hyp2_year(ihyp)
      if(hyp2_month(ihyp).gt.0  ) write(text(7:8),   '(i2)',  err=50)
     *hyp2_month(ihyp)
      if(hyp2_day(ihyp).  gt.0  ) write(text(9:10),  '(i2)',  err=50)
     *hyp2_day(ihyp)
      if(hyp2_hour(ihyp). ge.0  ) write(text(12:13), '(i2)',  err=50)
     *hyp2_hour(ihyp)
      if(hyp2_min(ihyp).  ge.0  ) write(text(14:15), '(i2)',  err=50)
     *hyp2_min(ihyp)
      if(hyp2_sec(ihyp).  ge.0.0) write(text(17:20), '(f4.1)',err=50)
     *hyp2_sec(ihyp)
c
c   flags, check a bit
c
      text(21:21) = hyp2_model(ihyp)
      text(22:22)=hyp2_dist_id(ihyp)
      if(hyp2_dist_id(ihyp).ne.'L'.and.hyp2_dist_id(ihyp).ne.'R'.and.
     *hyp2_dist_id(ihyp).ne.'D') then
         err_text='distance id'
         goto 50
      endif
      text(23:23)=hyp2_type(ihyp)
      text(11:11)=hyp2_fix_org(ihyp)
      if(hyp2_fix_org(ihyp).ne.' '.and.hyp2_fix_org(ihyp).ne.'F') then
         err_text='origin time flag'
         goto 50
      endif
c
c  hypocenter
c
      err_text='hypocenter'
      if(hyp2_lat(ihyp).ne.-999.0) write(text(24:30),
     *'(f7.3)',err=50) hyp2_lat(ihyp)
      if(hyp2_lon(ihyp).ne.-999.0) write(text(31:38),
     *'(f8.3)',err=50) hyp2_lon(ihyp)
      if(hyp2_depth(ihyp).ge.0) then
         if(hyp2_depth(ihyp).lt.10.0) then
            write(text(39:43),'(f5.2)',err=50) hyp2_depth(ihyp)
         else
            write(text(39:43),'(f5.1)',err=50) hyp2_depth(ihyp)
         endif
      endif
c
c   hypocenter flags
c
      text(44:44)=hyp2_depth_flag(ihyp)
      text(45:45)=hyp2_epi_flag(ihyp)
      if( hyp2_depth_flag(ihyp).ne.' '.and.hyp2_depth_flag(ihyp).ne.'F'.
     *and.hyp2_depth_flag(ihyp).ne.'S'.and.hyp2_epi_flag(ihyp)  .ne.' '.
     *and.  hyp2_epi_flag(ihyp).ne.'F'.and.hyp2_epi_flag(ihyp).  ne.'*')
     *then
          err_text='hypocenter flags'
          goto 50
      endif
c
c   agency
c
      text(46:48)=hyp2_agency(ihyp)(1:3)
c
c   number of stations, if more than 1000, reduce to 999
c
      err_text='number of stations'
      if(hyp2_nstat(ihyp).gt.1000) hyp2_nstat(ihyp)=999
      if(hyp2_nstat(ihyp).gt.0)
     *write(text(49:51),'(i3)',err=50) hyp2_nstat(ihyp)
c
c   rms
c
      if(hyp2_rms(ihyp).ge.0.0) then
         err_text='rms'
         if(hyp2_rms(ihyp).ge.100.0) hyp2_rms(ihyp)=99.0
         if(hyp2_rms(ihyp).lt.1.0) then
             write(text(53:55),'(f3.2)',err=50) hyp2_rms(ihyp)
         else

             write(text(52:55),'(f4.1)',err=50) hyp2_rms(ihyp)
         endif
       endif
c
c   magnitudes
c
      err_text='magnitudes'
      if(hyp2_mag(1,ihyp).gt.-100.0)
     *write(text(56:63),'(f4.1,a1,a3)',err=50)hyp2_mag(1,ihyp),
     *hyp2_mag_type(1,ihyp),hyp2_mag_agency(1,ihyp)(1:3)

      if(hyp2_mag(2,ihyp).gt.-100.0)
     *write(text(64:71),'(f4.1,a1,a3)',err=50)hyp2_mag(2,ihyp),
     *hyp2_mag_type(2,ihyp),hyp2_mag_agency(2,ihyp)(1:3)

      if(hyp2_mag(3,ihyp).gt.-100.0)
     *write(text(72:79),'(f4.1,a1,a3)',err=50)hyp2_mag(3,ihyp),
     *hyp2_mag_type(3,ihyp),hyp2_mag_agency(3,ihyp)(1:3)


c
c   finish writing without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing hyp line'
      write(6,'(a)') text
      call rea_stop

 60   continue
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_hypm_out(text,ihyp)
c
c   write  extra magnitude info (mag 3 to 6) type 1 to line text from
c   common block array number ihyp. The first 23 chars and the agency
c   is the same as for the first hypocenter line for this hypocenter
c   with index ihyp
c
c   note: agencies are 5 chars of which only 3 is used
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
c   clear
c
      text=' '
      text(80:80)='1'
c
c   write times
c
      err_text='hypocenter time'
      if(hyp2_year(ihyp). gt.0  ) write(text(2:5),   '(i4)',  err=50)
     *hyp2_year(ihyp)
      if(hyp2_month(ihyp).gt.0  ) write(text(7:8),   '(i2)',  err=50)
     *hyp2_month(ihyp)
      if(hyp2_day(ihyp).  gt.0  ) write(text(9:10),  '(i2)',  err=50)
     *hyp2_day(ihyp)
      if(hyp2_hour(ihyp). ge.0  ) write(text(12:13), '(i2)',  err=50)
     *hyp2_hour(ihyp)
      if(hyp2_min(ihyp).  ge.0  ) write(text(14:15), '(i2)',  err=50)
     *hyp2_min(ihyp)
      if(hyp2_sec(ihyp).  ge.0.0) write(text(17:20), '(f4.1)',err=50)
     *hyp2_sec(ihyp)
c
c   flags, check a bit
c
      text(21:21) = hyp2_model(ihyp)
      text(22:22)=hyp2_dist_id(ihyp)
      if(hyp2_dist_id(ihyp).ne.'L'.and.hyp2_dist_id(ihyp).ne.'R'.and.
     *hyp2_dist_id(ihyp).ne.'D') then
         err_text='distance id'
         goto 50
      endif
      text(23:23)=hyp2_type(ihyp)
      text(11:11)=hyp2_fix_org(ihyp)
      if(hyp2_fix_org(ihyp).ne.' '.and.hyp2_fix_org(ihyp).ne.'F') then
         err_text='origin time flag'
         goto 50
      endif
c
c   agency
c
      text(46:48)=hyp2_agency(ihyp)(1:3)
c
c   magnitudes
c
      err_text='magnitudes'
      if(hyp2_mag(4,ihyp).gt.-100.0)
     *write(text(56:63),'(f4.1,a1,a3)',err=50)hyp2_mag(1,ihyp),
     *hyp2_mag_type(1,ihyp),hyp2_mag_agency(1,ihyp)(1:3)

      if(hyp2_mag(5,ihyp).gt.-100.0)
     *write(text(64:71),'(f4.1,a1,a3)',err=50)hyp2_mag(2,ihyp),
     *hyp2_mag_type(2,ihyp),hyp2_mag_agency(2,ihyp)(1:3)

      if(hyp2_mag(6,ihyp).gt.-100.0)
     *write(text(72:79),'(f4.1,a1,a3)',err=50)hyp2_mag(3,ihyp),
     *hyp2_mag_type(3,ihyp),hyp2_mag_agency(3,ihyp)(1:3)
c
c   finish writing without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing hyp line'
      write(6,'(a)') text
      call rea_stop

 60   continue
      return
      end






cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_hyph_out(text,ihyp)
c
c   write all hypocenter info to type H line text from
c   common block array number ihyp
c
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c
c   clear
c
      text=' '
      text(80:80)='H'
c
c   write times
c
      err_text='hypocenter time'
      if(hyp2_year(ihyp). gt.0  ) write(text(2:5),   '(i4)',  err=50)
     *hyp2_year(ihyp)
      if(hyp2_month(ihyp).gt.0  ) write(text(7:8),   '(i2)',  err=50)
     *hyp2_month(ihyp)
      if(hyp2_day(ihyp).  gt.0  ) write(text(9:10),  '(i2)',  err=50)
     *hyp2_day(ihyp)
      if(hyp2_hour(ihyp). ge.0  ) write(text(12:13), '(i2)',  err=50)
     *hyp2_hour(ihyp)
      if(hyp2_min(ihyp).  ge.0  ) write(text(14:15), '(i2)',  err=50)
     *hyp2_min(ihyp)
      if(hyp2_sec(ihyp).  ge.0.0) write(text(17:22), '(f6.3)',err=50)
     *hyp2_sec(ihyp)
c
c  hypocenter
c
      err_text='hypocenter'
      if(hyp2_lat(ihyp).ne.-999.0) write(text(24:32),
     *'(f9.5)',err=50) hyp2_lat(ihyp)
      if(hyp2_lon(ihyp).ne.-999.0) write(text(34:43),
     *'(f10.5)',err=50) hyp2_lon(ihyp)
      if(hyp2_depth(ihyp).ge.0) 
     *write(text(45:52),'(f8.3)',err=50) hyp2_depth(ihyp)
c
c   rms
c
      if(hyp2_rms(ihyp).ge.0.0) then
         err_text='rms'
         if(hyp2_rms(ihyp).gt.100.0) hyp2_rms(ihyp)=99.0
         write(text(54:59),'(f6.3)',err=50) hyp2_rms(ihyp)
       endif
c
c   finish writing without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' writing hyp high accuracy line'
      write(6,'(a)') text
      call rea_stop

 60   continue
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_hype_out(text,ihyp)
c
c   read all error hypocenter info from type E line (error info
c   line) in text into common block hypocenter array number ihyp
c
c   some checking is done
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80   text
      character*20   err_text       ! error text
      integer        ihyp
c

      text=' '
      text(80:80)='E'

c
c   gap
c
      err_text='gap'
      if(hyp2_gap(ihyp).ge.0.0) then
          text(2:5)='GAP='
          write(text(6:8),'(i3)',err=50) int(hyp2_gap(ihyp)+0.5)
      endif
c
c   write origin time error
c
      err_text='origin time error'
      if(hyp2_sec_err(ihyp).ge.0.0) then
         if(hyp2_sec_err(ihyp).gt.1000.0) hyp2_sec_err(ihyp)=999.0
         if(hyp2_sec_err(ihyp).gt.1.0) 
     *   write(text(15:20),'(f6.2)',err=50) hyp2_sec_err(ihyp)
         if(hyp2_sec_err(ihyp).le.1.0) 
     *   write(text(15:20),'(f6.3)',err=50) hyp2_sec_err(ihyp)
       endif
c
c  hypocenter errors
c
      err_text='hypocenter error'
      if(hyp2_lat_err(ihyp).ge.0.0) then
         if(hyp2_lat_err(ihyp).gt.1000.0) hyp2_lat_err(ihyp)=999.0
         if(hyp2_lat_err(ihyp).le.1.0) write(text(25:33),
     *   '(f6.3)',err=50) hyp2_lat_err(ihyp)
         if(hyp2_lat_err(ihyp).gt.1.0) write(text(25:33),
     *   '(f6.1)',err=50) hyp2_lat_err(ihyp)
      endif

      if(hyp2_lon_err(ihyp).ge.0.0) then
         if(hyp2_lon_err(ihyp).gt.1000.0) hyp2_lon_err(ihyp)=999.0
         if(hyp2_lon_err(ihyp).le.1.0) write(text(33:38),
     *   '(f6.3)',err=50) hyp2_lon_err(ihyp)
         if(hyp2_lat_err(ihyp).gt.1.0) write(text(33:38),
     *   '(f6.1)',err=50) hyp2_lon_err(ihyp)
      endif

      if(hyp2_depth_err(ihyp).ge.0.0) then
         if(hyp2_depth_err(ihyp).gt.1000.0) hyp2_depth_err(ihyp)=999.0
         if(hyp2_depth_err(ihyp).le.1.0) write(text(39:43),
     *   '(f5.3)',err=50) hyp2_depth_err(ihyp)
         if(hyp2_depth_err(ihyp).gt.1.0) write(text(39:43),
     *   '(f5.1)',err=50) hyp2_depth_err(ihyp)
      endif
 
c
c   covariance terms
c
      err_text='hypocent covariance'
      if(hyp2_cov(1,ihyp).ge.-1.0e9)
     *write(text(44:55),'(e12.4)',err=50) hyp2_cov(1,ihyp)
      if(hyp2_cov(2,ihyp).ge.-1.0e9)
     *write(text(56:67),'(e12.4)',err=50) hyp2_cov(2,ihyp)
      if(hyp2_cov(3,ihyp).ge.-1.0e9)
     *write(text(68:79),'(e12.4)',err=50) hyp2_cov(3,ihyp)

c
c   finish reading without errors
c
      goto 60
c
c   error
c

 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,
     *' writing hyp error line'
      write(6,'(a)') text
      call rea_stop

 60   continue
      return
      end




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rea2_hyp_clear(ihyp)
c
c   initialize hyp1 line parameters for one entry at index ihyp,
c   character items are set to blanks and numbers to -999 excepth
c   covariace element which is set to -9.9e10
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      integer ihyp,i

      hyp2_year(ihyp)=-999              ! hypocenter year
      hyp2_month(ihyp)=-999
      hyp2_day(ihyp)=-999
      hyp2_hour(ihyp)=-999
      hyp2_min(ihyp)=-999
      hyp2_sec(ihyp)=-999.0
      hyp2_model(ihyp)=' '             ! model indicator
      hyp2_dist_id(ihyp)=' '           ! distance indicator
      hyp2_type(ihyp)=' '              ! event type like E
      hyp2_fix_org(ihyp)=' '           ! fix origin time flag
      hyp2_lat(ihyp)=-999.0            ! latitude
      hyp2_lon(ihyp)=-999.0            ! longitude
      hyp2_depth(ihyp)=-999.0          ! depth
      hyp2_depth_flag(ihyp)=' '        ! depth flag
      hyp2_epi_flag(ihyp)=' '          ! epicenter flag
      hyp2_agency(ihyp)=' '            ! hypocenter agency, use 3 only
      hyp2_nstat(ihyp)=-999            ! number of station
      hyp2_rms(ihyp)=-999.0            ! rms of hypocenter solution
      do i=1,6
         hyp2_mag(i,ihyp)=-999.0       ! magnitudes
         hyp2_mag_type(i,ihyp)=' '     ! magnitude types
         hyp2_mag_agency(i,ihyp)=' '   ! magnitude agencies
      enddo
      hyp2_high_accuracy(ihyp)=.false. ! high accurcy flag
      hyp2_error(ihyp)=.false.         ! error flag
      do i=1,3
         hyp2_cov(i,ihyp)= -9.9e10     ! covarieance elements
      enddo
      hyp2_gap(ihyp)=-999.0            ! gap
      hyp2_sec_err(ihyp)=-999.0        ! origin time error
      hyp2_lat_err(ihyp)=-999.0        ! hypocenter errors
      hyp2_lon_err(ihyp)=-999.0
      hyp2_depth_err(ihyp)=-999.0
      hyp2_auto(ihyp)=' '              ! not an automatic parameter

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      subroutine rea2_hyp_inp(data,nhead)
c
c   read all hypocenter info from data array nhead header lines into
c   common block hypocenter arrays
c
c      implicit none
c      include 'seidim.inc'
c      include 'rea2.inc'
c      return
c     end



      subroutine rea2_phase_out(iphas,text)
c
c   write one phase line from rea common, the phase number is iphas
c   and it is written to text, some checking of the values is
c   done, if errors the program stops
c
c   the time to write out is taken from the hr, min, sec. if not
c   available (still have initial values) use abs time. Be aware that
c   if a phase time is just after midnight, oriign time before midninght,
c   the hour must be 24 or more. So the abs time migh give an undesirded
c   effect
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      include 'seisan.inc'
      character*80 text
      integer iphas,i

      text=' '
c
c   if phase is a spectrum or deleted, do not write out
c
      if(rea2_phase(iphas)(1:4).eq.'SPEC'   .or.
     *   rea2_phase(iphas)(1:6).eq.'DELETE') return

c
c   check which time to use
c
      if((rea2_hour(iphas).eq.-999.0.or.rea2_min(iphas).eq.-999.0).
     * and.rea2_abs_time(iphas).gt.0.0)
     * call sectim(rea2_abs_time(iphas),rea2_year(iphas),i,
     * rea2_month(iphas),rea2_day(iphas),rea2_hour(iphas),
     * rea2_min(iphas),rea2_sec(iphas))
c
c   station
c
      text(2:6)=rea2_stat(iphas)
c
c   component, if short notion is not there, try to make it
c

      if(rea2_co(iphas).eq.' ' ) then
         call component(rea2_comp(iphas),rea2_co(iphas))
      endif
      text(7:8)=rea2_co(iphas)
c
c   onset
c
      text(10:10)=rea2_onset(iphas)


c
c   phase name can be long in which case no input polarity can be given
c   and weight is put in column 9
c
      if(rea2_phase(iphas)(5:8).ne.' ') then
         text(11:18)=rea2_phase(iphas)
         text(9:9)=rea2_weight_in(iphas)
      else
         text(11:14)=rea2_phase(iphas)(1:4)
         text(15:15)=rea2_weight_in(iphas)
         text(17:17)=rea2_polarity(iphas)
      endif
c
c   automatic process
c
      if(rea2_auto(iphas).ne.' ') text(16:16)='A'
c
c   hour
c
      if(rea2_hour(iphas).ge.0) then
          write(text(19:20),'(i2)',err=5) rea2_hour(iphas)
          goto 6
 5        continue
             write(6,*)' Error writing time, hr:',rea2_hour(iphas)
             write(6,'(a)') text
             call rea_stop
 6        continue
       endif
c
c   min
c
      if(rea2_min(iphas).ge.0) then
          write(text(21:22),'(i2)',err=7) rea2_min(iphas)
          goto 8
 7        continue
             write(6,*)' Error writing time, min:',rea2_min(iphas)
             write(6,'(a)') text
             call rea_stop
 8        continue
       endif
       
c
c  seconds, can be high accuracy
c
      if(rea2_sec(iphas).ge.0.0) then
         if(high_accuracy) then
            write(text(23:28),'(f6.3)',err=10) rea2_sec(iphas)
         else
            write(text(24:28),'(f5.2)',err=10) rea2_sec(iphas)
         endif
         goto 11
 10      continue
            write(6,*)' Error writing time, sec:',rea2_sec(iphas)
            write(6,'(a)') text
         call rea_stop
 11      continue
      endif
c
c   coda
c
      if(rea2_coda(iphas).gt.0.0) then
         write(text(30:33),'(i4)',err=15) int(rea2_coda(iphas))
         goto 16
 15      continue
           write(6,*)' Error writing coda:', rea2_coda(iphas)
           write(6,'(a)') text
           call rea_stop
 16      continue
      endif
c
c  amplitude, amplitude must be formated to a range of values
c

      if(rea2_amp(iphas).gt.0.0) then
c
c   check size of amplitude                                                 
c        
         if(rea2_amp(iphas).ne.0..and.rea2_amp(iphas).lt.100000.)
     +   write(text(34:40),'(f7.1)')
     +   rea2_amp(iphas)
         if(rea2_amp(iphas).ne.0..and.rea2_amp(iphas).ge.1.0e5
     +      .and.rea2_amp(iphas).lt.1.0e7)then
            rea2_amp(iphas)=rea2_amp(iphas)/1.0e4                 
            write(text(34:40),'(f5.1,a)')
     +      rea2_amp(iphas),'e4'
         endif
         if(rea2_amp(iphas).ne.0..and.rea2_amp(iphas).ge.1.0e7
     +      .and.rea2_amp(iphas).lt.1.0e9)then
            rea2_amp(iphas)=rea2_amp(iphas)/1.0e6                 
            write(text(34:40),'(f5.1,a)')
     +      rea2_amp(iphas),'e6'
         endif
         if(rea2_amp(iphas).ne.0..and.rea2_amp(iphas).ge.1.0e9
     +      .and.rea2_amp(iphas).lt.1.0e11)then
            rea2_amp(iphas)=rea2_amp(iphas)/1.0e8                 
            write(text(34:40),'(f5.1,a)')
     +      rea2_amp(iphas),'e8'
         endif
         if(rea2_amp(iphas).ge.1.0e11) then
             write(6,*)' Amplitude larger than 100 meters, unrealistic'
             call rea_stop
         endif
      endif
c
c   period
c
      if(rea2_per(iphas).gt.0.0) then
         if(rea2_per(iphas).lt.10.0) then
             write(text(42:45),'(f4.2)',err=20) rea2_per(iphas)
         else
             write(text(42:45),'(f4.1)',err=20) rea2_per(iphas)
         endif
         goto 21
 20      continue
            write(6,*)' Error writing period:', rea2_per(iphas)
            write(6,'(a)') text
            call rea_stop
 21      continue
      endif
c
c   back azimuth
c
      if(rea2_baz_obs(iphas).ne.-999.0) then
         write(text(47:51),'(f5.1)',err=25) rea2_baz_obs(iphas)
         goto 26
 25      write(6,*)' Error writing observed back azimuth:',
     *   rea2_baz_obs(iphas)
         write(6,'(a)') text
         call rea_stop
 26   continue
      endif
c
c  apparent velocity
c
      if(rea2_vel(iphas).ne.-999.0) then
         write(text(53:56),'(f4.1)',err=30) rea2_vel(iphas)
         goto 31
 30      write(6,*)' Error writing apparent velocity:',
     *   rea2_vel(iphas)
         write(6,'(a)') text
         call rea_stop
 31   continue
      endif
c
c  angle of incidence, must be positive
c
      if(rea2_ain(iphas).ge.0.0) then
         write(text(58:60),'(i3)',err=35) int(rea2_ain(iphas)+0.5)
         goto 36
 35      write(6,*)' Error writing ain :',
     *   rea2_ain(iphas)
         write(6,'(a)') text
         call rea_stop
 36   continue
      endif
c
c  azimuth residual, if negative convert to positive if smaller than -99
c
      if(rea2_baz_res(iphas).ne.-999.0) then
         if(rea2_baz_res(iphas).lt.-99.0)
     *   rea2_baz_res(iphas)=rea2_baz_res(iphas)+360.0
         if(rea2_baz_res(iphas).gt.0.0) then
           write(text(61:63),'(i3)',err=40) int(rea2_baz_res(iphas)+0.5)
         else
           write(text(61:63),'(i3)',err=40) int(rea2_baz_res(iphas)-0.5)
         endif
         goto 41
 40      write(6,*)' Error writing back azimuth residual:',
     *   rea2_baz_res(iphas)
         write(6,'(a)') text
         call rea_stop
 41   continue
      endif
c
c   travel time residual, values larger than 100 are truncated to 99.0
c
      if(rea2_res(iphas).ne.-999.0) then
         if(rea2_res(iphas).gt.100.0)  rea2_res(iphas)=99.0
         if(rea2_res(iphas).lt.-100.0) rea2_res(iphas)=-99.0
         if(abs(rea2_res(iphas)).lt.1.0)
     *      write(text(64:68),'(f5.3)',err=45) rea2_res(iphas)
        if(abs(rea2_res(iphas)).lt.10.0.and.abs(rea2_res(iphas)).ge.1.0)
     *      write(text(64:68),'(f5.2)',err=45) rea2_res(iphas)
         if(abs(rea2_res(iphas)).lt.100.0.and.abs(rea2_res(iphas)).
     *      ge.10.0)
     *      write(text(64:68),'(f5.1)',err=45) rea2_res(iphas)
         goto 46
 45         write(6,*)' Error writing travel time residual:',
     *      rea2_res(iphas)
            write(6,'(a)') text
            call rea_stop
 46      continue
      endif
      
c
c   output weight
c
      if(rea2_weight_out(iphas).ne.' ') then
         read(rea2_weight_out(iphas),'(i2)',err=50) i
         text(69:70)=rea2_weight_out(iphas)
         goto 51
 50      continue
            write(6,'(a25,1x,a2)')' Error with output weight:',
     *         rea2_weight_out(iphas)
               write(6,'(a)') text
               call rea_stop
 51         continue
      endif
c
c  azimuth at source, if negative convert to positive
c
      if(rea2_az(iphas).ne.-999.0) then
         if(rea2_az(iphas).lt.0.0)
     *   rea2_az(iphas)=rea2_az(iphas)+360.0
         write(text(77:79),'(i3)',err=55) int(rea2_az(iphas)+0.5)
         goto 56
 55         write(6,*)' Error writing azimuth:',
     *      rea2_az(iphas)
            write(6,'(a)') text
            call rea_stop
 56      continue
      endif
c
c   epicentral distance
c
      if(rea2_dist(iphas).ge.0.0) then
         if(rea2_dist(iphas).lt.10.0)
     *      write(text(71:75),'(f5.3)',err=60) rea2_dist(iphas)
         if(rea2_dist(iphas).lt.100.0.and.rea2_dist(iphas).ge.10.0)
     *      write(text(71:75),'(f5.2)',err=60) rea2_dist(iphas)
         if(rea2_dist(iphas).lt.1000.0.and.rea2_dist(iphas).
     *      ge.100.0)
     *      write(text(71:75),'(f5.1)',err=60) rea2_dist(iphas)
         if(rea2_dist(iphas).ge.1000.0)
     *      write(text(71:75),'(i5)',err=60) int(rea2_dist(iphas))
         goto 61
 60         write(6,*)' Error writing epicentral distance',
     *      rea2_dist(iphas)
            write(6,'(a)') text
            call rea_stop
 61      continue
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine rea2_phase_in(iphas,text)
c
c   reads one phase line from text line text,
c   the phase number to put info in is iphas,
c   data are read to rea-common block variables,
c   if errors the program stops
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80 text
      integer iphas,i
c
      if(text(80:80).ne.' '.AND.text(80:80).ne.'4') then
          write(6,*) ' Not a phase line'
          write(6,*) text
c          stop    ! commented out, lo
      endif
c
c   clear variables with phase parameters
c
      call rea2_phase_clear(iphas)
c
c   station
c
      rea2_stat(iphas)=text(2:6)
c
c   component, if short notion is not there, try to make it
c
      rea2_co(iphas)=text(7:8)
c
c   onset
c
      rea2_onset(iphas)=text(10:10)
c
c   phase name can be long in which case no input polarity can be given
c   and weight is in column 9
c
      if(text(9:9).ne.' '.or.(text(14:14).ne.' '.and.
     * text(15:18).ne.' '.and.text(17:17).ne.'C'.and.text(17:17).
     * ne.'D'.and.text(15:15).ne.' '.and.text(15:15).ne.'0'.and.
     * text(15:15).ne.'1'.and.text(15:15).ne.'2'.and.text(15:15).
     * ne.'3'.and.text(15:15).ne.'4'.and.text(15:15).ne.'9')) then
         rea2_phase(iphas)=text(11:18)
         rea2_weight_in(iphas)=text(9:9)
      else
         rea2_phase(iphas)(1:4)=text(11:14)
         rea2_weight_in(iphas)=text(15:15)
         rea2_polarity(iphas)=text(17:17)
      endif
c
c auto phase
c
      if(text(16:16).eq.'A') rea2_auto(iphas)='auto'

c
c   hour
c
      if(text(19:20).ne.' ') then
          read(text(19:20),'(i2)',err=5) rea2_hour(iphas)
          goto 6
 5        continue
             write(6,*)' Error reading hour'
             write(6,'(a)') text
             call rea_stop
 6        continue
       endif
c
c   min
c
      if(text(21:22).ne.' ') then
          read(text(21:22),'(i2)',err=7) rea2_min(iphas)
          goto 8
 7        continue
             write(6,*)' Error reading min:'
             write(6,'(a)') text
             call rea_stop
 8        continue
       endif
       
c
c  seconds
c
      if(text(23:28).ne.' ') then
         read(text(23:28),'(f6.3)',err=10) rea2_sec(iphas)
         goto 11
 10      continue
            write(6,*)' Error reading secc:'
            write(6,'(a)') text
         call rea_stop
 11      continue
      endif
c
c   coda
c
      if(text(30:33).ne.' ') then
         read(text(30:33),'(f4.0)',err=15) rea2_coda(iphas)
         goto 16
 15      continue
           write(6,*)' Error reading coda:'
           write(6,'(a)') text
           call rea_stop
 16      continue
      endif
c
c  amplitude
c

      if(text(34:40).ne.' ') then
          read(text(34:40),'(g7.1)',err=17) rea2_amp(iphas)
          goto 18
 17       continue
             write(6,*)' Error reading amplitude'
             write(6,'(a)') text
 18       continue
      endif
c
c   period
c
      if(text(42:45).ne.' ') then
         read(text(42:45),'(f4.2)',err=20) rea2_per(iphas)
         goto 21
 20      continue
            write(6,*)' Error reading period'
            write(6,'(a)') text
            call rea_stop
 21      continue
      endif
c
c   back azimuth
c
      if(text(47:51).ne.' ') then
         read(text(47:51),'(f5.1)',err=25) rea2_baz_obs(iphas)
         goto 26
 25      write(6,*)' Error reading observed back azimuth:'
         write(6,'(a)') text
         call rea_stop
 26   continue
      endif
c
c  apparent velocity
c
      if(text(53:56).ne.' ') then
         read(text(53:56),'(f4.1)',err=30) rea2_vel(iphas)
         goto 31
 30      write(6,*)' Error reading apparent velocity:'
         write(6,'(a)') text
         call rea_stop
 31   continue
      endif
c
c  signal to noise ratio
c
      if(text(57:60).ne.' ') then
         read(text(58:60),'(f3.0)',err=35) rea2_ain(iphas)
         goto 36
 35         write(6,*)' Error reading signal to noise ratio:'
            write(6,'(a)') text
            call rea_stop
 36      continue
      endif
c
c  azimuth residual
c
      if(text(61:63).ne.' ') then
         read(text(61:63),'(f3.0)',err=40) rea2_baz_res(iphas)
         goto 41
 40         write(6,*)' Error readin back azimuth residual:'
            write(6,'(a)') text
            call rea_stop
 41      continue
      endif
c
c   travel time residual
c
      if(text(64:69).ne.' ') then
         read(text(64:68),'(f5.3)',err=45) rea2_res(iphas)
         goto 46
 45         write(6,*)' Error reading travel time residual:'
            write(6,'(a)') text
            call rea_stop
 46      continue
      endif
      

c
c   output weight
c
      read(text(69:70),'(i2)',err=50) i
      rea2_weight_out(iphas)=text(69:70)
      goto 51
 50   continue
         write(6,*)' Error reading output weight:'
         write(6,'(a)') text
         call rea_stop
 51   continue
c
c  azimuth at source
c
      if(text(77:79).ne.' ') then
         read(text(77:79),'(f3.0)',err=55) rea2_az(iphas)
         goto 56
 55         write(6,*)' Error reading azimuth:'
            write(6,'(a)') text
            call rea_stop
 56      continue
      endif
c
c   epicentral distance
c
      if(text(71:75).ne.' ') then
         read(text(71:75),'(f5.0)',err=60) rea2_dist(iphas)
         goto 61
 60         write(6,*)' Error reading epicentral distance'
            write(6,'(a)') text
            call rea_stop
 61      continue
      endif

      return
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine rea2_phase_clear(iphas)
c
c   initialize phase line parameters for one entry at index iphas,
c   character items are set to blanks and numbers to -999
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      integer iphas
c
      rea2_stat(iphas)=' '        ! station codes
      rea2_comp(iphas)=' '        ! componenets
      rea2_co(iphas)= ' '         ! 2 letter componenets
      rea2_phase(iphas)= ' '      ! phase name
      rea2_onset(iphas)=  ' '     ! onset I or E or blank
      rea2_weight_in(iphas)= ' '  ! input weight
      rea2_weight_out(iphas)=' '  ! weight out
      rea2_polarity(iphas)=' '    ! polarity, D or C
      rea2_year(iphas)=-999
      rea2_month(iphas)=-999
      rea2_day(iphas)=-999
      rea2_hour(iphas)= -999
      rea2_min(iphas)= -999
      rea2_sec(iphas)= -999
      rea2_abs_time(iphas)=-999   ! abs phase time
      rea2_coda(iphas)= -999      ! coda length in s
      rea2_amp(iphas)=  -999      ! amplitude in nm
      rea2_per(iphas)=  -999      ! period of amplitude
      rea2_baz_obs(iphas)= -999   ! observed back azimuth
      rea2_baz_cal(iphas)= -999   ! calculated back azimuth
      rea2_vel(iphas)= -999       ! observed apparent velocity
      rea2_ain(iphas)=  -999      ! calcualted ain
      rea2_baz_res(iphas)= -999   ! back azimuth residual
      rea2_res(iphas)=-999        ! travel time residual
      rea2_dist(iphas)= -999      ! epicentral distance
      rea2_az(iphas)=   -999      ! azimuth
c

      rea2_moment(iphas)=-999     ! log moment, Nm
      rea2_sdrop(iphas)=-999      ! stress drop, bar
      rea2_omega0(iphas)=-999     ! log spectral flat level, ns
      rea2_cornerf(iphas)=-999    ! corner f
      rea2_radius(iphas)=-999     ! source radius
      rea2_swin(iphas)=-999       ! window lenght used
      rea2_geo_dist(iphas)=-999   ! geo distance, km
      rea2_vs(iphas)=-999         ! s-velocity at source, km/s
      rea2_vp(iphas)=-999         ! p-velocity at source, km/s
      rea2_q0(iphas)=-999         ! q0
      rea2_qalpha(iphas)=-999     ! q alpha
      rea2_kappa(iphas)=-999      ! kappa
      rea2_density(iphas)=-999    ! density g/cm**3
      rea2_slope(iphas)=-999      ! - measured slope of spectrum
      rea2_mc(iphas)=-999         ! coda
      rea2_ml(iphas)=-999         ! local
      rea2_mb(iphas)=-999         ! mb
      rea2_ms(iphas)=-999         ! ms
      rea2_mw(iphas)=-999         ! mw
      rea2_auto(iphas)=' '        ! not an automatic parameter
      

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_spec_in(iphas,text1,text2)
c
c   read spectral info to rea common, the phase number is iphas
c   and it is read from text1 and 2, some checking of the values is
c   done, if errors the program stops
c
c
c   there are two spectral phases defined: SPECP: P-spectrum
c                                          SPECS: S-spectrum
c
c   the time read in is put in normal phase variables
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80 text1,text2
      integer iphas,i
      real x                  ! help variable
      character*20 err_text   ! error text

c
c   clear variables
c
      call rea2_phase_clear(iphas)

c
c   check if text strings are spectral strings and belong together
c
      if(text1(2:5).ne.'SPEC'.or.text2(2:5).ne.'SPEC'.or.
     *   text1(80:80).ne.'3'.or.text2(80:80).ne.'3'.or.
     *   text1(7:14).ne.text2(7:14)) then
         write(6,*)
     *   ' Two spec lines of wrong type or do not belong together'
         write(6,'(a)') text1
         write(6,'(a)') text2
c         stop
       endif
        
c
c   station
c
      rea2_stat(iphas)(1:4)=text1(7:10)  ! not room for 5. letter
c
c   component
c
      rea2_comp(iphas)=text1(11:14)

c
c   moment
c

      err_text='moment'
      if(text1(16:17).ne.'MO') goto 50
      if(text1(18:22).ne.' ') 
     *read(text1(18:22),'(f5.1)',err=50) rea2_moment(iphas)
      
c
c   stress drop
c
      err_text='stress drop'
      if(text1(24:25).ne.'ST') goto 50
      if(text1(26:30).ne.' ') 
     *read(text1(26:30),'(f5.1)',err=50) rea2_sdrop(iphas)
c
c   omega0
c
      err_text='omega0'
      if(text1(32:33).ne.'OM') goto 50
      if(text1(34:38).ne.' ')
     *read(text1(34:38),'(f5.1)',err=50) rea2_omega0(iphas)
c
c   corner frequency
c
      err_text='corner frequency'
      if(text1(40:41).ne.'f0') goto 50
      if(text1(42:46).ne.' ') 
     *read(text1(42:46),'(f5.2)',err=50) rea2_cornerf(iphas)
c
c   radius
c
      err_text='source radius'
      if(text1(48:48).ne.'R') goto 50
      if(text1(50:54).ne.' ') 
     *read(text1(50:54),'(f5.1)',err=50) rea2_radius(iphas)

c
c   specteral slope, abs value
c
      err_text='spectral slope'
      if(text1(56:57).ne.'AL') goto 50
      if(text1(58:62).ne.' ') then 
         read(text1(58:62),'(f5.2)',err=50) rea2_slope(iphas)
         rea2_slope(iphas)=-rea2_slope(iphas)
      endif
c
c   window length
c
      err_text='spectral window'
      if(text1(64:65).ne.'WI') goto 50
      if(text1(66:70).ne.' ') 
     *read(text1(66:70),'(f5.2)',err=50) rea2_swin(iphas)
c
c   moment magnitude
c
      err_text='moment magnitude'
      if(text1(72:73).ne.'MW') goto 50
      if(text1(74:78).ne.' ')
     *read(text1(74:78),'(f5.1)',err=50) rea2_mw(iphas)
c
c-----------------------------
c   second spec line
c-----------------------------
c

c
c   start of window
c
      err_text='window start time'
      if(text2(16:16).ne.'T') goto 51
      if(text2(17:22).ne.' ') then
         read(text2(17:22),'(3i2)',err=51) rea2_hour(iphas),
     *    rea2_min(iphas),i
          rea2_sec(iphas)=i
      endif
c
c   kappa
c
      err_text='kappa'
      if(text2(24:25).ne.'K ') goto 51
      if(text2(26:30).ne.' ')
     *read(text2(26:30),'(f5.3)',err=51) rea2_kappa(iphas)
c
c   geodistance
c
      err_text='geo distance'
      if(text2(32:33).ne.'GD') goto 51
      if(text2(34:38).ne.' ') 
     *read(text2(34:38),'(f5.1)',err=51) rea2_geo_dist(iphas)
c
c   velocity for spectrum and spectral phases
c
      err_text='source velocity'
      if(text2(40:40).ne.'V ') goto 51
      rea2_phase(iphas)='SPEC '
      rea2_phase(iphas)(5:5)=text2(41:41)   ! type of spectrum
      if(text2(42:46).ne.' ') then
         read(text2(42:46),'(f5.2)',err=51) x
c
c   assign velocity to right velocity
c
         if(rea2_phase(iphas)(5:5).eq.'S') rea2_vs(iphas)=x  
         if(rea2_phase(iphas)(5:5).eq.'P') rea2_vp(iphas)=x
      endif
c
c   density
c
      err_text='density'
      if(text2(48:49).ne.'DE') goto 51
      if(text2(50:54).ne.' ') 
     *read(text2(50:54),'(f5.2)',err=51) rea2_density(iphas)
c
c    q0
c
      err_text='q0'
      if(text2(56:57).ne.'Q0') goto 51
      if(text2(58:62).ne.' ') 
     *read(text2(58:62),'(f5.1)',err=51) rea2_q0(iphas)
c
c   q-alpha
c
      err_text='q-alpha'
      if(text2(64:65).ne.'QA') goto 51
      if(text2(66:70).ne.' ') 
     *read(text2(66:70),'(f5.2)',err=51) rea2_qalpha(iphas)
c
c   s-velocity
c
      err_text='S-velocty'
      if(text2(72:73).ne.'VS') goto 51
      if(text2(74:78).ne.' ') 
     *read(text2(74:78),'(f5.2)',err=51) rea2_vs(iphas)
c
      goto 52


 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' reading spec line'
      write(6,'(a)') text1
c      stop
 51   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' reading spec line'
      write(6,'(a)') text2
c      stop

 52   continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_av_spec_in(text)
c
c   read spectral average info to rea common, the phase number is iphas
c   and it is read from text1,  some checking of the values is
c   done, if errors the program stops
c
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80 text
      character*20 err_text   ! error text


c
c  check if correct string
c
      if(text(2:13).ne.'SPEC AVERAGE') then
          write(6,*)' Not a spectral average input string'
          write(6,'(a)') text
          call rea_stop
      endif
c
c   clear variables
c
      rea2_av_moment=-999.0
      rea2_av_sdrop=-999.0
      rea2_av_omega0=-999.0
      rea2_av_cornerf=-999.0
      rea2_av_radius=-999.0
      rea2_av_slope=-999.0
      rea2_av_swin=-999.0
      rea2_av_mw=-999.0
c
c   moment
c

      err_text='moment'
      if(text(16:17).ne.'MO') goto 50
      if(text(18:22).ne.' ') 
     *read(text(18:22),'(f5.1)',err=50) rea2_av_moment
      
c
c   stress drop
c
      err_text='stress drop'
      if(text(24:25).ne.'ST') goto 50
      if(text(26:30).ne.' ') 
     *read(text(26:30),'(f5.1)',err=50) rea2_av_sdrop
c
c   omega0
c
      err_text='omega0'
      if(text(32:33).ne.'OM') goto 50
      if(text(34:38).ne.' ')
     *read(text(34:38),'(f5.1)',err=50) rea2_av_omega0
c
c   corner frequency
c
      err_text='corner frequency'
      if(text(40:41).ne.'f0') goto 50
      if(text(42:46).ne.' ') 
     *read(text(42:46),'(f5.2)',err=50) rea2_av_cornerf
c
c   radius
c
      err_text='source radius'
      if(text(48:48).ne.'R') goto 50
      if(text(50:54).ne.' ') 
     *read(text(50:54),'(f5.1)',err=50) rea2_av_radius

c
c   specteral slope, abs value
c
      err_text='spectral slope'
      if(text(56:57).ne.'AL') goto 50
      if(text(58:62).ne.' ') then 
         read(text(58:62),'(f5.2)',err=50) rea2_av_slope
         rea2_av_slope=-rea2_av_slope
      endif
c
c   window length
c
      err_text='spectral window'
      if(text(64:65).ne.'WI') goto 50
      if(text(66:70).ne.' ') 
     *read(text(66:70),'(f5.2)',err=50) rea2_av_swin
c
c   moment magnitude
c
      err_text='moment magnitude'
      if(text(72:73).ne.'MW') goto 50
      if(text(74:78).ne.' ') 
     *read(text(74:78),'(f5.1)',err=50) rea2_av_mw

      goto 51

 50   continue
      write(6,'(a,a,a)')
     *'Error with ',err_text,' reading average spec line'
      write(6,'(a)') text
      call rea_stop

 51   continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_spec_out(iphas,text1,text2)
c
c   write spectral info from rea common, the phase number is iphas
c   and it is written to text, some checking of the values is
c   done, if errors the program stops
c
c
c   there are two spectral phases defined: SPECP: P-spectrum
c                                          SPECS: S-spectrum

c   the time to write out is taken from the hr, min, sec. if not
c   available (still have initial values) use abs time
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80 text1,text2
      integer iphas,i
      real x                  ! help variable
      character*20 err_text   ! error text

      text1=' '
      text2=' '

c
c   check which time to use
c
      if((rea2_hour(iphas).eq.-999.0.or.rea2_min(iphas).eq.-999.0).
     * and.rea2_abs_time(iphas).gt.0.0)
     * call sectim(rea2_abs_time(iphas),rea2_year(iphas),i,
     * rea2_month(iphas),rea2_day(iphas),rea2_hour(iphas),
     * rea2_min(iphas),rea2_sec(iphas))
c
c   fixed values
c
      text1(2:5)='SPEC'
      text1(80:80)='3'
c
c   check if an auto process
c
      if(rea2_auto(iphas).ne.' ') text1(6:6)='A'
c
c   station
c
      text1(7:10)=rea2_stat(iphas)(1:4)  ! not room for 5. letter
c
c   component
c
      text1(11:14)=rea2_comp(iphas)
c
c   same content
c
      text2=text1
c
c   check for overflow
c
      if(rea2_sdrop(iphas).gt.999.0)  rea2_sdrop(iphas)=999.0
      if(rea2_cornerf(iphas).gt.999.0) rea2_cornerf(iphas)=999.0
      if(rea2_omega0(iphas).gt.999.0) rea2_omega0(iphas)=999.0
      if(abs(rea2_slope(iphas)).gt.999.0)   rea2_slope(iphas)=999.0
      if(rea2_geo_dist(iphas).gt.99999.) rea2_geo_dist(iphas)=99999.0
      if(rea2_radius(iphas).gt.999.0) rea2_radius(iphas)=999.0
      if(rea2_swin(iphas).gt.999.0) rea2_swin(iphas)=999.0

c
c   moment
c
      text1(16:17)='MO'
      if(rea2_moment(iphas).gt.0.0) then
         err_text='moment'
         write(text1(18:22),'(f5.1)',err=50) rea2_moment(iphas)
      endif
c
c   stress drop
c
      text1(24:25)='ST'
      if(rea2_sdrop(iphas).gt.0.0) then
         err_text='stress drop'
         write(text1(26:30),'(f5.1)',err=50) rea2_sdrop(iphas)
      endif
c
c   omega0
c
      text1(32:33)='OM'
      if(rea2_omega0(iphas).gt.0.0) then
         err_text='omega0'
         write(text1(34:38),'(f5.1)',err=50) rea2_omega0(iphas)
      endif
c
c   corner frequency
c
      text1(40:41)='f0'
      if(rea2_cornerf(iphas).gt.0.0) then
         err_text='corner frequency'
         if(rea2_cornerf(iphas).lt.1.0)
     *     write(text1(42:46),'(f5.3)',err=50) rea2_cornerf(iphas)
         if(rea2_cornerf(iphas).lt.10.0.and.rea2_cornerf(iphas).ge.1.0)
     *     write(text1(42:46),'(f5.2)',err=50) rea2_cornerf
         if(rea2_cornerf(iphas).ge.10.0.and.
     *      rea2_cornerf(iphas).lt.100.0)
     *      write(text1(42:46),'(f5.1)',err=50) rea2_cornerf(iphas)
         if(rea2_cornerf(iphas).ge.100.0)
     *      write(text1(42:46),'(f5.0)',err=50) rea2_cornerf(iphas)

c        write(text1(42:46),'(f5.2)',err=50) rea2_cornerf(iphas)
      endif
c
c   radius
c
      text1(48:48)='R'
      if(rea2_radius(iphas).gt.0.0) then
         err_text='source radius'
         if(rea2_radius(iphas).lt.1.0)
     *   write(text1(50:54),'(f5.3)',err=50) rea2_radius(iphas)
         if(rea2_radius(iphas).lt.10.0.and.rea2_radius(iphas).ge.1.0)
     *   write(text1(50:54),'(f5.2)',err=50) rea2_radius(iphas)
         if(rea2_radius(iphas).ge.10.0)
     *   write(text1(50:54),'(f5.1)',err=50) rea2_radius(iphas)
      endif

c
c   specteral slope, abs value
c
      text1(56:57)='AL'
      if(rea2_slope(iphas).gt.-20.0) then
         err_text='spectral slope'
         write(text1(58:62),'(f5.2)',err=50) -rea2_slope(iphas)
      endif
c
c   window length
c
      text1(64:65)='WI'
      if(rea2_swin(iphas).gt.0.0) then
         err_text='spectral window'
         if(rea2_swin(iphas).lt.10.0)
     *   write(text1(66:70),'(f5.2)',err=50) rea2_swin(iphas)
         if(rea2_swin(iphas).ge.10.0)
     *   write(text1(66:70),'(f5.1)',err=50) rea2_swin(iphas)
      endif
c
c   moment magnitude
c
      text1(72:73)='MW'
      if(rea2_mw(iphas).gt.-10.0) then
         err_text='moment magnitude'
         write(text1(74:78),'(f5.1)',err=50) rea2_mw(iphas)
      endif
c
c-----------------------------
c   second spec line
c-----------------------------
c

c
c   start of window
c
      text2(16:16)='T'
      if(rea2_hour(iphas).gt.-1.and.rea2_min(iphas).
     *gt.-1.and.rea2_sec(iphas).gt.-1.0) then
         err_text='window start time'
         write(text2(17:22),'(3i2)',err=51) rea2_hour(iphas),
     *   rea2_min(iphas),int(rea2_sec(iphas)+0.5)
      endif
c
c   kappa
c
      text2(24:25)='K '
      if(rea2_kappa(iphas).gt.0.0) then
         err_text='kappa'
         write(text2(26:30),'(f5.3)',err=51) rea2_kappa(iphas)
      endif
c
c   geodistance
c
      text2(32:33)='GD'
      if(rea2_geo_dist(iphas).gt.0.0) then
         err_text='geo distance'
         if(rea2_geo_dist(iphas).lt.10.0)
     *   write(text2(34:38),'(f5.3)',err=51) rea2_geo_dist(iphas)
         if(rea2_geo_dist(iphas).ge.10.0.and.rea2_geo_dist(iphas).
     *   lt.100.0)
     *    write(text2(34:38),'(f5.2)',err=51) rea2_geo_dist(iphas)
         if(rea2_geo_dist(iphas).ge.100.0)
     *   write(text2(34:38),'(f5.1)',err=51) rea2_geo_dist(iphas)
      endif
c
c   velocity for spectrum
c
      text2(40:41)='V '
      text2(41:41)=rea2_phase(iphas)(5:5)  ! indicate if p or s-spectrum
      x=0.0
      if(rea2_phase(iphas)(5:5).eq.'S') x=rea2_vs(iphas)  ! find which velocity if any
      if(rea2_phase(iphas)(5:5).eq.'P') x=rea2_vp(iphas)
      if(x.gt.0.0) then
         err_text='source velocity'
         write(text2(42:46),'(f5.2)',err=51) x
      endif
c
c   density
c
      text2(48:49)='DE'
      if(rea2_density(iphas).gt.0.0) then
         err_text='density'
         write(text2(50:54),'(f5.2)',err=51) rea2_density(iphas)
      endif
c
c    q0
c
      text2(56:57)='Q0'
      if(rea2_q0(iphas).gt.0.0) then
         if(rea2_q0(iphas).gt.1000.0) rea2_q0(iphas)=999.0
         err_text='q0'
         write(text2(58:62),'(f5.1)',err=51) rea2_q0(iphas)
      endif
c
c   q-alpha
c
      text2(64:65)='QA'
      if(rea2_qalpha(iphas).ge.0.0) then
         err_text='q-alpha'
         write(text2(66:70),'(f5.2)',err=51) rea2_qalpha(iphas)
      endif
c
c   s-velocity
c
      text2(72:73)='VS'
      if(rea2_vs(iphas).gt.0.0) then
         err_text='S-velocty'
         write(text2(74:78),'(f5.2)',err=51) rea2_vs(iphas)
      endif
c
      goto 52


 50   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing spec line'
      write(6,'(a)') text1
      call rea_stop
 51   continue
      write(6,'(a,a,a)') 'Error with ',err_text,' writing spec line'
      write(6,'(a)') text2
      call rea_stop

 52   continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_av_spec_out(text)
c
c   write average spectral info from rea common to text
c
c   jh march 2001
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      character*80 text
      character*20 err_text   ! error text

      text=' '
c
c   fixed values
c
      text(2:13)='SPEC AVERAGE'
      text(80:80)='3'
c
c   check for overflow
c
      if(rea2_av_sdrop.gt.999.0)  rea2_av_sdrop=999.0
      if(rea2_av_cornerf.gt.999.0) rea2_av_cornerf=999.0
      if(rea2_av_omega0.gt.999.0) rea2_av_omega0=999.0
      if(abs(rea2_av_slope).gt.999.0)   rea2_av_slope=999.0
      if(rea2_av_radius.gt.999.0) rea2_av_radius=999.0
      if(rea2_av_swin.gt.999.0) rea2_av_swin=999.0

c
c   moment
c
      text(16:17)='MO'
      if(rea2_av_moment.gt.0.0) then
         err_text='moment'
         write(text(18:22),'(f5.1)',err=50) rea2_av_moment
      endif
c
c   stress drop
c
      text(24:25)='ST'
      if(rea2_av_sdrop.gt.0.0) then
         err_text='stress drop'
         write(text(26:30),'(f5.1)',err=50) rea2_av_sdrop
      endif
c
c   omega0
c
      text(32:33)='OM'
      if(rea2_av_omega0.gt.0.0) then
         err_text='omega0'
         write(text(34:38),'(f5.1)',err=50) rea2_av_omega0
      endif
c
c   corner frequency
c
      text(40:41)='f0'
      if(rea2_av_cornerf.gt.0.0) then
         err_text='corner frequency'
         if(rea2_av_cornerf.lt.1.0)
     *     write(text(42:46),'(f5.3)',err=50) rea2_av_cornerf
         if(rea2_av_cornerf.lt.10.0.and.rea2_av_cornerf.ge.1.0)
     *     write(text(42:46),'(f5.2)',err=50) rea2_av_cornerf
         if(rea2_av_cornerf.ge.10.0.and.rea2_av_cornerf.lt.100.0)
     *      write(text(42:46),'(f5.1)',err=50) rea2_av_cornerf
         if(rea2_av_cornerf.ge.100.0)
     *      write(text(42:46),'(f5.0)',err=50) rea2_av_cornerf

c        write(text(42:46),'(f5.2)',err=50) rea2_av_cornerf
      endif
c
c   radius
c
      text(48:48)='R'
      if(rea2_av_radius.gt.0.0) then
         err_text='source radius'
         if(rea2_av_radius.lt.1.0)
     *   write(text(50:54),'(f5.3)',err=50) rea2_av_radius
         if(rea2_av_radius.lt.10.0.and.rea2_av_radius.ge.1.0)
     *   write(text(50:54),'(f5.2)',err=50) rea2_av_radius
         if(rea2_av_radius.ge.10.0)
     *   write(text(50:54),'(f5.1)',err=50) rea2_av_radius
      endif

c
c   specteral slope, abs value
c
      text(56:57)='AL'
      if(rea2_av_slope.gt.-20.0) then
         err_text='spectral slope'
         write(text(58:62),'(f5.2)',err=50) -rea2_av_slope
      endif
c
c   window length
c
      text(64:65)='WI'
      if(rea2_av_swin.gt.0.0) then
         err_text='spectral window'
         if(rea2_av_swin.lt.10.0)
     *   write(text(66:70),'(f5.2)',err=50) rea2_av_swin
         if(rea2_av_swin.ge.10.0)
     *   write(text(66:70),'(f5.1)',err=50) rea2_av_swin
      endif
c
c   moment magnitude
c
      text(72:73)='MW'
      if(rea2_av_mw.gt.-10.0) then
         err_text='moment magnitude'
         write(text(74:78),'(f5.1)',err=50) rea2_av_mw
      endif

      goto 51

 50   continue
      write(6,'(a,a,a)')
     *'Error with ',err_text,' writing avarage spec line'
      write(6,'(a)') text
      call rea_stop

 51   continue
      return
      end

c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       
