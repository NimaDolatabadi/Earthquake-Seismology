c
c program to convert waveform data to time trace input for GMT4.x
c one file is written out per channel
c
c Lars Ottemoller, April 2006
c
      implicit none
      include 'seidim.inc'                    ! for giving dimensions
      include 'libsei.inc'                    ! for all seisan subroutines
      include 'waveform.inc'                  ! waveform data
      include 'seisan.inc'                    ! seisan general parameters
      integer nstat,nphase,nhead,nrecord,id   ! for reading s-file
      character*1 type,exp                    ! -----------------
      character*80 sfilname                   ! s-file name
      character*80 data(max_data)             ! one s-file
      integer npresent                        ! number of wav files present
      character*80 text                       ! general text string
      integer k,i,nz,j,seiclen
      character*5 station
      character*4 component
      real x,max,dc
      logical gap_flag,b_flag
      integer read01,in,code,number,nerr,last_gap
      character*80 question
      character*80 filein,fileout
      character*24 time
      double precision msec
      integer year,doy,month,day,hour,min
      real sec
      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments
      logical normalize
      character*1 c

c
c print version
c
      include 'version.inc'
      out_version_date='May 02, 2006'
      if (version_new) out_version_date=version_date
      call print_ver
      call get_arguments(narg,arg)
      normalize=.false.
      fileout=' '
      if (narg.eq.4) then
        do i=1,4
          if (arg(i).eq.'-infile') filein=arg(i+1)
          if (arg(i).eq.'-outfile') fileout=arg(i+1)
        enddo
        in=0
        goto 11
      endif

 10   continue
      question=' Filename or number, filenr.lis for all'
      call filename(question,filein)
      if(filein(1:3).eq.'EOF') goto 99

c
c   check for one or many files
c
      in=0
      if(filein(1:10).eq.'filenr.lis') then
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'filenr.lis',     ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
         in=1
      endif
      write(*,*) ' Normalize amplitude to trace maximum ? (y/n) '
      read(5,'(a)') c
      if (c.eq.'y') normalize = .true.

c
c  back here if many files to convert using filenr.lis
c
 11   continue
      if(in.eq.1) then
         read(read01,'(2x,i3,2x,a)') number,filein
         write(6,*)
         if(filein(1:4).eq.'    ') goto 99
      endif
      if(in.eq.0) write(6,'(1x,a)') filein

      wav_nfiles=1                   ! there is only one file in this case
      call get_full_wav_name(filein,wav_filename(1))

      wav_nchan=0
c
c   loop to read all headers of all files
c
         write(6,'(1x,a)') wav_filename(1)
c
c   read header
c
         wav_error_message=' '
         call read_wav_header(1)
c
c   output possible errors
c
         if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *   wav_error_message

      write(6,*)' channels:',wav_nchan
      if(wav_nchan.eq.0) then
        write(*,*) ' no channels '
        stop
      endif

      do k=1,wav_nchan
c
c   read whole channel
c
        call wav_read_channel(k)
        call timsec(wav_year(k),wav_month(k),wav_day(k),
     &      wav_hour(k),wav_min(k),wav_sec(k),msec)
 
c
c write out data
c
        if (fileout.eq.' '.or.wav_nchan.ne.1) 
     &  fileout=filein(1:seiclen(filein))//'_'//wav_stat(k)//'_'//
     &      wav_comp(k)
        do i=1,seiclen(fileout)
          if (fileout(i:i).eq.' ') fileout(i:i)='_'
        enddo
c find dc and absolute max
        max=0.
        if (normalize) then
          dc=0.
          do i=1,wav_nsamp(k)
            dc=dc+signal1(i)/wav_nsamp(k)
          enddo
          do i=1,wav_nsamp(k)
            signal1(i)=signal1(i)-dc
            if (abs(signal1(i)).gt.max) max=abs(signal1(i))
          enddo
        else
          max=1.
        endif

        open(1,file=fileout,status='unknown')
c
c output sample time code 2004/07/12T18:08:44.00
c
        do i=1,wav_nsamp(k)
          time=' '
          call sectim(msec,year,doy,month,day,
     &      hour,min,sec)
          write(time(1:22),'(i4.4,"/",i2.2,"/",i2.2,"T",
     &   i2.2,":",i2.2,":",f5.2)')
     &      year,month,day,hour,min,sec
          do j=1,22
            if (time(j:j).eq.' ') time(j:j)='0'
          enddo
          write(1,*) time,signal1(i)/max
          msec=msec+1/wav_rate(k)
        enddo
        close(1)
      enddo
 
      if (in.eq.1) goto 11

99    continue

      stop
      end

