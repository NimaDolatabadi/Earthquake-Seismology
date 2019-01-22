c
c    print out response values in a file cresp.out from all response
c    files in a directory using filenr.lis. Sorting after instrument
c    type and component
c
c    may 95 by jh: a bit clean up
c    mar 3 99 by jh : ----------- version 7.0 check -------------------
c                     station to 5 chars
c    may 11 00  lo  : use read_resp routine
c    may 16 01  jh  : fix common block, change output format
c
      implicit none
      include 'seidim.inc'
      include 'waveform.inc'

      logical gse_resp          ! true if gse format
      logical sei_resp          ! true if seisan format
      integer nfa               ! number of frequencies and amplitude
      character*120 htext       ! explanation text

c   NUMBER OF FILTERS,  POLES,AND FREQUENCIES
      INTEGER NFILT,POLE(10)
      real xpole(10)
      REAL FFILT(10)
      complex pol(max_resp_value),zero(max_resp_value)  ! complex PAZ
      integer npol,nzero        ! number of poles and zeros
      real norm                 ! normalization constant for poles and zeros
C   AMPLIFIER GAIN (DB), SEISMOMETER GENERATOR CONSTANT (LOADED V/M/S)
      REAL GAIN,GENCON
C   SEISMOMETER OR ACCELEROMETER DAMPING RATIO AND SEISMOMETER PERIOD
      REAL DAMPIN,PERIOD
C   RECORDING MEDIA GAIN (COUNTS/V OR M/VOLT)
      REAL REGAIN
c   sensor type 2: seismometer, 3: accelerometer
      integer sentyp
      real g1hz  ! gain at 1 hz
c   individual response values
      real freq_val(max_resp_value),gain_val(max_resp_value),
     *phas_val(max_resp_value)
      
      real g5hz  ! gain at 1 hz
      complex res
c   variables for sorting
      character*80 fil(1000),outf(1000)
      character*14 fils
      integer istat,nstat,nout,nfile,outn(1000)
c
      INTEGER I,J,k,iresp,nfil
      character*80 filename(1000)
      character*27 filt_text(5)         ! text for filters
      real filtl(10)                    ! low pass filters
      real filth(10)                    ! high pass filters
      integer polel(10),poleh(10)       ! poles
      integer nlow,nhigh                ! number of low and high pass filters
      integer nfilt_text                ! number of filter pairs for output
      logical ftab                      ! if true, force tabulated values


c
c response common block
c
      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa

      equivalence (xpole,pole)



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      nfil=0

      write(6,*)' Program writes a table of response values for'
      write(6,*)' response functions found in CAL. Program must be'
      write(6,*)' run in CAL and there must be a filenr.lis file'
      write(6,*)' with a list of the response files to be listed'
      write(6,*)' The output is in file pr_resp.out'
      write(6,*)' Return to continue'
      read(5,'(a)') i
      open(1,file='filenr.lis',status='old')
      open(2,file='pr_resp.out',status='unknown')
      write(2,'(a,a,a)')' STAT  COMP  YEAR DATE  PERIOD  DAMP  GENCON',
     *            '   AGAIN   REGAIN    FLOW  P  FHIG  P   G 1 HZ   ',
     *            'G 5 HZ' 
c
c
c   loop for reading response files
c
      nfile=0
  1   continue
      nfile=nfile+1
      read(1,'(7x,a)',end=9) filename(nfile)
      if(filename(nfile)(1:5).eq.'     ') goto 9
      goto 1
  9   continue
      write(6,*)' Number of response files: ',nfile-1
c
c   find number of identical stations
c
      istat=0
      nstat=1
c
  10  continue
      istat=istat+1
      if(istat.gt.nfile-1) goto 99    ! finished 
      fil(nstat)=filename(istat)
      if(filename(istat)(1:5).ne.filename(istat+1)(1:5)) goto 11
      nstat=nstat+1
      goto 10
  11  continue
c
c  now all identical stations found
c
      write(6,*) nstat
c
c   sort after type and component
c
      do i=1,nstat
        outn(i)=0
      enddo
      nout=1
c
c   short period
c
      do i=1,nstat
         if(fil(i)(5:5).eq.'S'.and.fil(i)(8:8).eq.'Z'.and.outn(i).ne.1)
     *   then
            outn(i)=1
            outf(nout)=fil(i)
            nout=nout+1
            fils=fil(i)(5:18)
            fils(4:4)='N'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first n-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
      
            fils(4:4)='E'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first e-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
         endif
      enddo
c
c   accelerometer
c
      do i=1,nstat
         if(fil(i)(5:5).eq.'A'.and.fil(i)(8:8).eq.'Z'.and.outn(i).ne.1)
     *   then
            outn(i)=1
            outf(nout)=fil(i)
            nout=nout+1
            fils=fil(i)(5:18)
            fils(4:4)='N'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first n-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
      
            fils(4:4)='E'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first e-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
         endif
      enddo
c
c   long period
c
      do i=1,nstat
         if(fil(i)(5:5).eq.'L'.and.fil(i)(8:8).eq.'Z'.and.outn(i).ne.1)
     *   then
            outn(i)=1
            outf(nout)=fil(i)
            nout=nout+1
            fils=fil(i)(5:18)
            fils(4:4)='N'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first n-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
      
            fils(4:4)='E'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first e-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
         endif
      enddo
c
c   broad band
c
      do i=1,nstat
         if(fil(i)(5:5).eq.'B'.and.fil(i)(8:8).eq.'Z'.and.outn(i).ne.1)
     *   then
            outn(i)=1
            outf(nout)=fil(i)
            nout=nout+1
            fils=fil(i)(5:18)
            fils(4:4)='N'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first n-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
      
            fils(4:4)='E'
            do k=1,nstat
               if(fil(k)(5:18).eq.fils.and.outn(k).ne.1) then
                  outf(nout)=fil(k)          ! save first e-component
                  nout=nout+1
                  outn(k)=1                       ! mark file used
               endif
            enddo
         endif
      enddo
c
c    find files not selected
c
        do i=1,nstat
           if(outn(i).ne.1) then
              outf(nout)=fil(i)
              nout=nout+1
           endif
        enddo
c
c----------------------------------------------------------------------
c   sorting finished, now start reading response files
c----------------------------------------------------------------------
c
        do 1000 iresp=1,nstat
        nfil=nfil+1
        write(6,'(i6,2x,a)') nfil,outf(iresp)(1:40)
c
c   open response file
c
c      open(3,file=outf(iresp),status='old')
c
c   read file
c
c      do i=1,13            
c        j=(i-1)*80 + 1     
c        k=j+79             
c        read(3,'(a80)') header(j:k)
c      enddo                
c      close(3)
c
c  read values
c
c      call get_resp(header)
       wav_resp_file = outf(iresp)
       call read_resp
c
c   find how many low and high pass filters
c
c
c   blank filter text
c
      do i=1,5
        filt_text(i)=' '
      enddo
      nlow=0
      nhigh=0
      if(nfilt.gt.0) then
         do i=1,nfilt
             if(pole(i).lt.0) then
                  nhigh=nhigh+1
                  filth(nhigh)=ffilt(i)
                  poleh(nhigh)=iabs(pole(i))
             else
                  nlow=nlow+1
                  filtl(nlow)=ffilt(i)
                  polel(nlow)=iabs(pole(i))
             endif
          enddo 
c
c   make filter text
c
          if(nhigh.gt.0) then
             do i=1,nhigh
                write(filt_text(i)(1:6),'(f6.2)') filth(i)
c                write(filt_text(i)(8:8),'(i1)') poleh(i)
                write(filt_text(i)(8:9),'(i2)') poleh(i)
             enddo
          endif
          if(nlow.gt.0) then
             do i=1,nlow
                write(filt_text(i)(10:15),'(f6.2)') filtl(i)
c                write(filt_text(i)(18:18),'(i1)') polel(i)
                write(filt_text(i)(17:18),'(i2)') polel(i)
             enddo
          endif
          nfilt_text=nlow
          if(nhigh.gt.nlow) nfilt_text=nhigh
        endif
c
c   calculate response at 5 hz
c
      call calc_resp(5.0,res)
      g5hz=cabs(res)
c     write(6,*)'At 5 hz',g5hz 
c
c   now print out in a table
c
c      write(2,200) header(1:9),header(11:12),header(18:19),
c     *             header(21:22),
c     *             period,dampin,gencon,gain,
c     *             int(regain),filt_text(1),
c     *             g1hz/1000000000.0,g5hz,header(81:160)

c
c if instrument constants given
c
      if (period.ne.0) then
        write(2,200) wav_resp_stat(1:5),wav_resp_comp(1:4),
     *             wav_resp_year,wav_resp_month,wav_resp_day,
     *             period,dampin,gencon,gain,
     *             int(regain),filt_text(1)(1:18),
     *             g1hz/1000000000.0,g5hz
      elseif (nfa.gt.0) then
         write(2,201) wav_resp_stat(1:5),wav_resp_comp(1:4),
     *             wav_resp_year,wav_resp_month,wav_resp_day,
     *             ' Response given as FAP '

      elseif (npol.gt.0) then
         write(2,201) wav_resp_stat(1:5),wav_resp_comp(1:4),
     *             wav_resp_year,wav_resp_month,wav_resp_day,
     *             ' Response given as PAZ '

      endif

 200  format(1x,a5,1x,a4,2x,i4,1x,2i2,
c     *       2x,f6.2,2x,f4.2,1x,f7.1,2x,f6.1,
     *       1x,f7.2,2x,f4.2,1x,f7.1,2x,f6.1,
     *       2x,i7,2x,a18,
     *       2x,f7.4,2x,f7.3)
c     htext(2:10)=' STAT  COMP  YEAR DATE  PERIOD  DAMP  GENCON   AGAIN',
c    *            '   REGAIN   FILT  P  FILT  P   G 1 HZ   G 5 HZ' 
 201  format(1x,a5,1x,a4,' | ',i4,'.',i2,'.',i2,' | ',a)
c
c   more filter lines
c
      if(nfilt_text.gt.1) then
        do i=2,nfilt_text
c           write(2,'(69x,a18)') filt_text(i)
           write(2,'(63x,a18)') filt_text(i)
        enddo
      endif
c
 1000 continue
c
c   get next set of identical stations
c
      nstat=1
      goto 10
c
 99   continue
      stop
      end
