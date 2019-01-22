
      program arcsei

c
c Task:        extract data from Reftek archive to Seisan format in
c              single time window or continuous mode
c
c By:          Lars Ottemoller, November 2001
c
c Description: ARCSEI provides a simple user interface to specify
c              the data request and options. ARCSEI then calls the
c              programs arcfetch for data extraction and rt_seis
c              for data conversion. The data are temporarily stored
c              in the ARCSEI_TEMP directory under the current working
c              directory, which is deleted after the program has
c              completed. It is possible to extract from multiple
c              channels, for multiple time windows, or to extract
c              data in continuous mode. Optional merging of the
c              Seisan files is also possible. Input parameters
c              can be defined in ARCSEI.DEF.
c
c   ArcFetch:  ArcFetch is the program to extract data from the
c              Reftek archive. The data by ArcFetch is written
c              out in Passcal format.
c
c              Options used are:
c                -C     - cooked mode, means that files are created
c                         for given time window
c                -Opath - set output path
c                -Ln    - n output log level (0-3)
c
c   RtSeis:    Converts the Passcal Reftek data into Seisan
c              format. The Seisan files are created in the arcsei_temp
c              directory. The program is executed witout any of the
c              options.
c
c                
c feb 03 2011 jh : eq to eqv

      implicit none
      include 'arcsei.inc'              ! arcsei include file
      integer seiclen                   ! function to get length of string
      integer i,j,ic,is,nfiles          ! counters
      integer n_cont_windows            ! --------
      integer cont_length               ! length of cont window in s
      character*120 sys_line            ! string to call system command
      character*240 arcfetch_line       ! string to call arcfetch
      character*240 rtseis_line         ! string to call rt_seis
      character*12 arcfetch_channel(200)! unit,stream,channel
      integer n_arcfetch_channel        ! counter for
      character*20 arcfetch_start(1000) ! YEAR:DOY:HOUR:MIN:SEC
      integer n_arcfetch_start          ! counter for
      character*20 arcfetch_end         ! YEAR:DOY:HOUR:MIN:SEC
      character*80 files(200)           ! file names
      logical debug_flag                ! keep files if true
      integer year,doy,day,month,hour,min,sec ! time
      real rsec                         ! seconds
      double precision msec,smsec       ! timein seconds
      character*80 line                 ! string
        

      write(*,*) ' ARCSEI - DATA EXTRACTION FROM REFTEK ARCHIVE'
     &           // ' AND CONVERSION TO SEISAN FORMAT '
      write(*,*)

c
c set defaults
c
      call get_arcsei_def
      debug_flag=.FALSE.

c
c read archive directory
c
      write(*,*)
      write(*,*) ' ENTER ARCHIVE, OR <RETURN> TO USE DEAULT '
     &    // '(default: '//archive(1:seiclen(archive)) //')'
      read(5,'(a)') line
      if (seiclen(line).gt.0) then
        archive=line
      endif
c
c read channel
c
      n_arcfetch_channel=1
      arcfetch_channel(n_arcfetch_channel)='DUMMY'
      write(*,*)
      if (n_def_arcfetch_channel.gt.0) then
        write(*,*)   '     AVAILABLE CHANNELS ARE:'
        do i=1,n_def_arcfetch_channel
          write(*,*) '     '//def_arcfetch_channel(i)
     &          (1:seiclen(def_arcfetch_channel(i)))
        enddo
      else
        write(*,*) '                 EXAMPLE: 8020,1,* '
      endif
      write(*,*) ' ENTER CHANNEL SELECTION (UNIT,STREAM,CHANNEL) '
      write(*,*) '       OR '//
     &   'TYPE ALL TO USE ALL AVAILABLE CHANNELS'

      do while(seiclen(arcfetch_channel(n_arcfetch_channel)).ge.1)
        read(5,'(a)') arcfetch_channel(n_arcfetch_channel)
        if (arcfetch_channel(n_arcfetch_channel).eq.'all'.or.
     &      arcfetch_channel(n_arcfetch_channel).eq.'ALL') then
          do i=1,n_def_arcfetch_channel
            arcfetch_channel(i)=def_arcfetch_channel(i)
          enddo
          n_arcfetch_channel=n_def_arcfetch_channel+1
          arcfetch_channel(n_arcfetch_channel)=' '
        else
          if (seiclen(arcfetch_channel(n_arcfetch_channel)).ge.1) then
            n_arcfetch_channel=n_arcfetch_channel+1
            arcfetch_channel(n_arcfetch_channel)='DUMMY'
            write(*,*) ' NEXT CHANNEL OR RETURN TO CONTINUE '
          endif
        endif
      enddo
      n_arcfetch_channel=n_arcfetch_channel-1
      if (n_arcfetch_channel.eq.0) then
        write(*,*) ' NO CHANNEL SELECTED '
        STOP
      endif

c
c read start time
c
      n_arcfetch_start=1
      arcfetch_start(n_arcfetch_start)='DUMMY'
      write(*,*) ' ENTER START-TIME '
     &       //'(YEAR:DAY-OF-YEAR:HOUR:MINUTE:SECOND) '
      write(*,*) '           EXAMPLES: 2000:200:12 '
      write(*,*) '                     2000:200:12:15 '
      write(*,*) '                     2000:200:12:33:15 '

      do while(seiclen(arcfetch_start(n_arcfetch_start)).ge.1)
        read(5,'(a)') arcfetch_start(n_arcfetch_start)
        if (seiclen(arcfetch_start(n_arcfetch_start)).ge.1) then
          n_arcfetch_start=n_arcfetch_start+1
          arcfetch_start(n_arcfetch_start)='DUMMY'
          write(*,*) ' NEXT START-TIME OR RETURN TO CONTINUE '
        endif
      enddo
      n_arcfetch_start=n_arcfetch_start-1
      if (n_arcfetch_start.eq.0) then
        write(*,*) ' NO START TIME SELECTED '
        STOP
      endif


c
c read end time
c
      write(*,*) ' ENTER END TIME USING ONE OF 3 OPTIONS: '
      write(*,*) '    - ABSOLUTE TIME AS YYYY:DDD:HH:MM:SS '
     &             // ' (LIKE START-TIME) '
      write(*,*) '    - +SECONDS FOR TIME INTERVAL (e.g. +300)'
      write(*,*) '    - ++SECONDS FOR MULTIPLE INTERVALS '
     &             // '(CONTINUOUS EXTRACT, e.g. ++300)'

      write(*,*)
      read(5,'(a)') arcfetch_end
      if (seiclen(arcfetch_end).lt.1) then
        write(*,*) ' NO END TIME SELECTED '
        STOP
      endif


c
c if continuous extract
c
      if (arcfetch_end(1:2).eq.'++') then
        arcfetch_end=arcfetch_end(2:)
        write(*,*) ' ENTER NUMBER OF CONTINUOUS WINDOWS '
        read(5,'(i)') n_cont_windows
        if (n_cont_windows.le.0) then
          write(*,*) ' BAD SELECTION '
          STOP
        endif
        read(arcfetch_end(2:),'(i)') cont_length
        if (n_arcfetch_start.gt.1) then
          write(*,*) ' WARNING: ONLY USING FIRST START-TIME '
          n_arcfetch_start=1
        endif
c
c read start time
c
        read(arcfetch_start(1)(1:4),'(i4)') year
        if (arcfetch_start(1)(7:7).eq.':') then
          read(arcfetch_start(1)(6:6),'(i1)') doy
          i=8
        elseif (arcfetch_start(1)(8:8).eq.':') then
          read(arcfetch_start(1)(6:7),'(i2)') doy
          i=9
        elseif (arcfetch_start(1)(9:9).eq.':') then
          read(arcfetch_start(1)(6:8),'(i3)') doy
          i=10
        endif
        read(arcfetch_start(1)(i:),'(i2,1x,i2,1x,i2)')
     &    hour,min,sec
       write(*,*) year,doy,hour,min,sec
        rsec=real(sec)
        call dte(doy,day,month,year)
        call timsec(year,month,day,hour,min,rsec,msec)
        smsec=msec
        do i=0,n_cont_windows-1
          msec=smsec+real(i*cont_length)
          call sectim(msec,year,doy,month,day,hour,min,rsec)
          write(arcfetch_start(i+1),'(i4,a1,i3,a1,i2,a1,i2,a1,i2)')
     &     year,':',doy,':',hour,':',min,':',int(rsec)

          do j=1,17
            if (arcfetch_start(i+1)(j:j).eq.' ')
     &        arcfetch_start(i+1)(j:j)='0'
          enddo
          write(*,*) i+1,' ',arcfetch_start(i+1)
        enddo
        n_arcfetch_start=n_cont_windows


      endif   ! continuous cont

c
c loop over all start times
c
      do is=1,n_arcfetch_start


c
c delete existing arcsei_temp directory
c
       sys_line = ' '
       sys_line = 'rmdir /q /s arcsei_temp'
       call systemc(sys_line,23)

c
c create arcseis_temp directory
c
       sys_line = ' '
       sys_line = 'mkdir arcsei_temp'
       call systemc(sys_line,17)

c
c loop over all selected channels
c
       do ic=1,n_arcfetch_channel

c
c extract data from archive using arcfetch
c
        arcfetch_line = ' '
        arcfetch_line = 'arcfetch '// archive(1:seiclen(archive))//' '//
     &    arcfetch_channel(ic)(1:seiclen(arcfetch_channel(ic))) //','//
     &    arcfetch_start(is)(1:seiclen(arcfetch_start(is))) //','//
     &    arcfetch_end(1:seiclen(arcfetch_end)) //' '//
     &    '-oarcsei_temp -c '
        write(*,*) 'command: ',arcfetch_line(1:seiclen(arcfetch_line))
        call systemc(arcfetch_line,seiclen(arcfetch_line))


c
c get Reftek file names
c
        nfiles=0
        call dirf_pc('arcsei_temp\\*.rt',files,nfiles)
        do i=1,nfiles
c
c convert files to Reftek using rt_seis and delete
c
          rtseis_line=' '
          rtseis_line='rt_seis '//'arcsei_temp/'//
     &      files(i)(1:seiclen(files(i))) //
     &      ' arcsei_temp '
          call systemc(rtseis_line,seiclen(rtseis_line))
          if (.not.debug_flag) then
            sys_line=' '
            sys_line='del arcsei_temp\'//
     &        files(i)(1:seiclen(files(i)))
            call systemc(sys_line,seiclen(sys_line))
          endif


        enddo  ! loop over rt_seis
       enddo  ! loop over all channels

c
c merge files if wanted
c
       if (merge_flag.eqv..true.) then
         call dirf_pc('arcsei_temp\\*.sei',files,nfiles)
         open(1,file='filenr.lis',status='unknown')
         do i=1,nfiles
           write(1,'(7x,a)') 'arcsei_temp\'
     &   //files(i)(1:seiclen(files(i)))
         enddo
         write(1,*)
         close(1)
         open(1,file='seisei.inp',status='unknown')
         write(1,'(a)') '1'
         write(1,'(a)') arcsei_code
         write(1,'(a)') ' '
         close(1)
         sys_line='seisei < seisei.inp'
         call systemc(sys_line,seiclen(sys_line))
         call dirf_pc('*.'//arcsei_code//'*',files,nfiles)

       else
         call dirf_pc('arcsei_temp\\*.sei',files,nfiles)
       endif
c
c move all Seisan files
c

       do i=1,nfiles
         if (merge_flag) then
           sys_line = 'move '
     &       //files(i)(1:seiclen(files(i)))//' '
     &       //outpath(1:seiclen(outpath))
         else
           sys_line = 'move arcsei_temp\'
     &       //files(i)(1:seiclen(files(i)))//' '
     &       //outpath(1:seiclen(outpath))
         endif
         write(*,*) sys_line(1:seiclen(sys_line))
         call systemc(sys_line,seiclen(sys_line))
       enddo
      enddo  ! loop over all start times

      if (.not.debug_flag) then
        sys_line = 'rmdir /q /s arcsei_temp'
        call systemc(sys_line,23)
      endif

      stop
      end


      subroutine get_arcsei_def
c
c read arcsei.def file
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'arcsei.inc'
      integer seiclen
      integer code               ! error code

      character*120 top_dir      ! top directory
      character*1 dchar          ! directory deliminator
      character*160 line         ! text string
      integer def_unit           ! file unit

      call topdir(top_dir)
      call dir_char(dchar)

      archive = '.\'
      outpath = '.\'
      n_def_arcfetch_channel=0
      merge_flag=.false.
      arcsei_code='ARCSE'

c
c open for ARCSEI.DEF from current or DAT directory
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   def_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'ARCSEI.DEF' )    ! For this file.
      if(code.ne.e_ok$) return    ! if file does not exist
                                          
10    continue
      read(def_unit,'(a)',end=20) line ! Read from file
      if (line(1:7).eq.'ARCHIVE') then
        archive = line(41:seiclen(line))
      elseif (line(1:7).eq.'OUTPATH') then
        outpath=line(41:seiclen(line))
      elseif (line(1:12).eq.'NETWORK_CODE') then
        arcsei_code=line(41:seiclen(line))
      elseif (line(1:5).eq.'MERGE'.and.line(41:41).eq.'Y') then
        merge_flag=.true.
      elseif (line(1:7).eq.'CHANNEL') then
        n_def_arcfetch_channel=n_def_arcfetch_channel+1
        def_arcfetch_channel(n_def_arcfetch_channel)=
     &      line(41:seiclen(line))
      endif
      goto 10

20    continue
      call sei close( close$, def_unit, code ) ! Close (Default stop on error).
      write(*,*) ' SEISAN FILES ARE MOVED TO DIRECTORY: ' //
     &    outpath(1:seiclen(outpath))

      return
      end
