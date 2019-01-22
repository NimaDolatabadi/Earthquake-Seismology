
      program sacsei

c
c by Lars Ottemoeller
c
c program to convert between sac and seisan, detects if ASCII
c or binary, uses SEISAN wav structure
c
c 
c      options are: sac        -> seisan bin
c                   seisan bin -> sac bin
c ---------------------- SEISAN 7.0 ----------------------------------
c 14 ap 99 bmt  : name of the component was changed
c 08/06 99 lo   : use routine filename
c june 11 99 lo : convert station and components using sacsei.def file
c aug  18 99 lo : bug fixed with net_code
c nov  09 99 lo : dimension of fheader changed to max_trace, bug on linux fixed
c nov  11 99 lo : add cname to outfile, using seiclen
c dec  06 99 lo : add 1900 if year lt 100
c --- major change ----------------------- 
c may  12 00 lo : use new routines
c nov 13 2001jh : remove component change since already in basic 
c                 sac reading routine
c dec 18 2002 jh: make sure net_code is blank to get station name in 
c                 file name, note not net code can be used in scasei.def
c                 since sacsei.def is read in sac subroutines

      implicit none

C    Seisan library inserts and routines...
C    ======================================
C
      include 'libsei.inc'                ! Open file definitions
      include 'seidim.inc'
      include 'waveform.inc'
      external computer_type              ! Get platform type.
      external sei open,                  ! Open file routine.
     &         sei close                  ! Close file routine.
      integer sei clen
c --------------------------------------------------------------

      character*80 infile,outfile       ! file name
      integer max                       ! max number of samples
      integer nerr                      ! error 
      integer*4 idata(max_sample)
      character*1 choice                ! choice
      character*80 mainhead(max_trace)  ! file headers
      character*1040 chead              ! trace header
      integer i,c                       ! counter
      integer in                 ! 1 if filenr.lis is input file
      integer nars                      ! number of args
      character*80 args(5)              ! arguments
      logical from_eev                  ! true if from eev
      character*80 evfile               ! name of sfile
      real eqla,eqlo,eqel               ! hypocenter coordinates
      character*80 line                 ! some text line
      logical hyp                       ! if hyp given in sfile
      character*80 question,deffile     ! text
      character*5 net_code              ! network code
c jh dec 2002     logical no_net                    ! flag if net_code set
      character*29 mainhead_text
      


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver


c
c   get def file for station codes, give file name
c
c     deffile='sacsei.def'
c     no_net = .FALSE.
      net_code=' '

c     call read_def_chan(deffile,mainhead_text,net_code)

c     if (net_code.eq.' ') no_net = .true.

c get arguments
      call get_arguments(nars,args)
      in = 0
      max = max_sample 
      from_eev=.false.
      hyp = .false.

      if (nars.eq.3) then

        if (args(1).eq.'eev') then

          infile=args(2)
          call get_env_event(evfile)

          open(1,file=evfile,status='old')
          read(1,'(a80)') line
          close (1)
c
c read eq hypocenter coord
c 
          if (line(24:38).ne.'               ') then
            hyp=.true.
          endif
          read(line(24:30),'(f7.3)') eqla
          read(line(31:38),'(f7.3)') eqlo  
          read(line(39:43),'(f5.2)') eqel 
          from_eev=.true.
          goto 500
        endif
      endif

      write(*,*) ' (1) sac (ASCII or BINARY) -> seisan binary'
      write(*,*) ' (2) seisan binary -> sac binary'
      write(*,*) ' Choice ?'
      read(*,'(a1)') choice 
      call casefold(choice)
c
c jump to seisan input
c
      if (choice.eq.'2') then 
        goto 500
      elseif (choice.eq.'1') then
        goto 21 
      else
        write(*,*) ' wrong choice'
        stop
      endif 

c 
c read SAC
c

21    continue
      question=' Filename or number, filenr.lis for all'
      call filename(question,infile)

      if (infile(1:3).eq.'EOF') STOP
      if(infile(1:10).eq.'filenr.lis') then
        open(1,file='filenr.lis',status='old',err=22)
        in=1
      endif
      goto 23

22    continue

      write(*,*) ' filenr.lis not found'
      stop

23    continue

      if(in.eq.1) then
        read(1,'(7x,a)') infile
        if(infile(1:4).eq.'    ') stop
      endif

c
c first try if file is binary SAC
c
      call read_sacbin_to_seisan(infile,1,nerr)

c
c if not check if SAC ascii
c
      if (nerr.ne.0) then
        nerr=0
        call read_sacasc_to_seisan(infile,1,nerr)
      endif

      if (nerr.ne.0) then
        write(*,*) ' input file is not SAC: ',infile(1:seiclen(infile))
        if (in.eq.1) goto 23
        stop
      endif

c
c write Seisan file
c

c
c write file header
c
      outfile = ' '

c     call set_def_chan(1,wav_stat(1),wav_comp(1))

c
c   make seisan headers
c
       net_code=' '

       wav_nchan=1
       call wav_sheads(1,net_code,outfile,mainhead,chead)

       write(*,*) chead(1:80)

       do i=1,wav_nsamp(1)
         idata(i)=int(signal1(i))
       enddo
       outfile = outfile(1:seiclen(outfile)) // '_' // 
     &   wav_comp(1)(1:seiclen(wav_comp(1)))
       do i=1,seiclen(outfile)
         if (outfile(i:i).eq.' ') outfile(i:i)='_'
       enddo
       write(*,*) ' output file: ',outfile(1:seiclen(outfile))
       open(66,file=outfile,status='unknown',form='unformatted')
       do i=1,12
          write(66) mainhead(i)
       enddo
       write(66) chead
       write(66) (idata(i),i=1,wav_nsamp(1))
       close(66)


      if (in.eq.1) goto 23
      goto 21
      
 499  continue
      stop

 500  continue

c
c this part is for seisan binary to sac binary
c

      if (from_eev) goto 523


521   continue

      question=' Filename or number, filenr.lis for all'
      call filename(question,infile)

      if (infile(1:3).eq.'EOF') STOP
      if(infile(1:10).eq.'filenr.lis') then
          open(1,file='filenr.lis',status='old',err=522)
          in=1
      endif
      eqla=0.
      eqlo=0.
      eqel=0.
      goto 523

522   continue

      write(*,*) ' filenr.lis not found'
      stop

523   continue

      if(in.eq.1) then
        read(1,'(7x,a)') infile
        if(infile(1:4).eq.'    ') stop
      endif

c
c read seisan traces
c
      call wav_init
      wav_filename(1)=infile
      call read_wav_header(1) 

      write(*,*) ' number of traces: ',wav_nchan

      do c=1,wav_nchan

        write(*,*) 'trace: ',c
        call wav_read_channel(c)
        write(*,'(a5,1x,a4,1x,i4,1x,4(i2,1x),f5.2)') 
     *       wav_stat(c),wav_comp(c),
     *       wav_year(c),wav_month(c),wav_day(c),
     *       wav_hour(c),wav_min(c),wav_sec(c)

        call write_seisan_to_sacbin(c,.false.,.false.,
     *        eqla,eqlo,eqel,nerr)
        if (nerr.ne.0) then
          write(*,*) ' Error when writing SAC file '
        endif

      enddo
      if (in.eq.1) goto 523
      if (.not.from_eev) goto 521
 
9999  continue

      stop
      end

