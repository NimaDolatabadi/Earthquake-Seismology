c
c  Output-Lines in GSE-Format have always a length of 80 characters, also the
c  last line of the DAT2 section. Urs Kradolfer, 6.6.99
c
c   changes
c
c 08 02 2013 jh: dimension fix
c
c  Version 98.002 - 27.11.98
c==============================================================================
c     Karl Koch, 08.05.98
c
c     This file contains two subroutines: GSEIN and GSEOUT
c
c          *) GSEIN ....  reads GSE-waveform data
c
c          *) GSEOUT ...  write GSE-waveform data
c
c==============================================================================
c
c-----
      subroutine gsein(iin,iout,cbuf,iy,ichecksum,ierr)
c------------------------------------------------------------------------------
c     Input/Output parameters:
c        iin ......... logical unit with input data
c        iout ........ logical unit for output data
c        cbuf ........ character array to store compressed data (see c_bufsize)
c        iy .......... integer array to hold raw waveform data (see c_sigsize)
c        ichecksum ... checksum value for data stored in iy according to format
c        ierr ........ error code
c
c        in common block
c        hdr_ifmtnum . GSE format identifier of input data
c        hdr_nsamp ... number of samples in array iy
c
c        disabled
c        iformat_in .. GSE format identifier (ie. =2 for GSE2.0) (output)
c        nmax ........ number of samples in array iy (output)
c        lin ......... character array to hold header lines
c        ilin ........ number of header lines (output)
c------------------------------------------------------------------------------
c
      implicit none

c     -- include header variables (hdr_...) and constants (c_...)
      include 'codeco_common.f'

c     -- parameters
      integer*4 iin, iout
      character*1 cbuf(*)
      integer*4 iy(*)
      integer*4 ichecksum, ierr

c     -- local variables
      integer*4 icomp, i, j, ierror, nchecksum
      integer*4 numout, nchar, ii
      integer*4 trimlen
      integer*4 icheck_sum_gse1,icheck_sum_gse2
      real      fsec                 ! seconds and milliseconds
      character line*132, fmt*6
      character firstline*132        ! store first line for error message
c     -- for month conversions:
      integer   mnths(12)
      data mnths/31,28,31,30,31,30,31,31,30,31,30,31/

c     -- executable code

		hdr_coosys = ' '
      if  (hdr_debug .gt. 2)  write (*,*)  '-- entering gsein'
      ierr = 0

c     -- loop all lines in GSE file until WID is found
1     read(iin,'(a)',end=999) line
      if  (line(1:4) .ne. 'WID1' .and. line(1:4) .ne. 'WID2')  then
c        -- copy all lines before WID-line
         if  (hdr_ofmtnum .gt. 0)
     &      write(iout,'(a)') line(1:trimlen(line))
         goto 1
      else
         if  (line(1:4).eq.'WID1')  hdr_ifmtnum = 1
         if  (line(1:4).eq.'WID2')  hdr_ifmtnum = 2
      endif
      firstline = line
      if  (hdr_debug .gt. 3)  then
         write (*,*)  '-- found hdr line: '
         write (*,'(a)')  firstline
      endif
c
      if  (hdr_ifmtnum .eq. 2)  goto 20
c
c     -- input data are in GSE1.0 format
c
      if  (hdr_debug .gt. 0)  write(*,*)
     &   '-- reading GSE1.0 format data'

c     -- parse first header line of GSE1.0
100   format(i4,i3,3(1x,i2),1x,i3,1x,i8,1x,a6,1x,a8,1x,
     &   a2,1x,f11.7,1x,a6,1x,a4,1x,i1)
      read( line(7:), 100 ) hdr_year, hdr_jday, hdr_hour, hdr_min,
     &   hdr_sec, hdr_msec, hdr_nsamp, hdr_station, hdr_stadescr,
     &   hdr_chan, hdr_smprate, hdr_instr, hdr_ifmtname, hdr_idiff

c     -- read and parse second header line
      read( iin, '(a)' ) line
101   format(f9.6,i1,f7.4,1x,4(f9.4,1x),2(f7.2,1x),f6.1,1x,f6.1)
      read( line, 101 ) hdr_calfac, hdr_calunit, hdr_calper,
     &   hdr_stalat, hdr_stalon, hdr_staelev, hdr_stadepth,
     &   hdr_beamaz, hdr_beamslo, hdr_hang
      if  (hdr_nsamp .gt. c_sigsize)  goto 998

c     -- compute day and month from julian day
cks      if(ifix(float(hdr_year)/4.0)*4 .eq. hdr_year) mnths(2)=29
cks      hdr_month = 0
cks      do i=1,12
cks         hdr_month = hdr_month + mnths(i)
cks         if  (hdr_month .ge. hdr_jday)  goto 60
cks      enddo
cks60    hdr_month = hdr_month - mnths(i)
cks      hdr_day = hdr_jday - hdr_month
cks      hdr_month = i
      call julien( hdr_day, hdr_month, hdr_year, hdr_jday, 0 )

c     -- adjust channel name (insert a 'H' in the middle)
      hdr_chan(3:3) = hdr_chan(2:2)
      hdr_chan(2:2) = 'H'
      call casefold( hdr_chan )

c     -- read away all lines until DAT1
      if  (hdr_debug .gt. 3)  write (*,*) '-- find DAT1'
2     read( iin, '(a)' )  line
      if  (line(1:4) .ne. 'DAT1')  goto 2

c     -- Now read in data according to the format:
      if  (hdr_debug .gt. 2)  write (*,*) '-- read in samples'
      if  (hdr_ifmtname .eq. 'INTV')  then
         if  (hdr_debug .gt. 3)  write (*,*) '-- read INTV data'
         read(iin,*) (iy(i),i=1,hdr_nsamp)
         read(iin,'(4x,6x,i15)') ichecksum
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
      endif
      if(hdr_ifmtname(1:3).eq.'INT'.and.hdr_ifmtname.ne.'INTV')then
         if  (hdr_debug .gt. 3)  write (*,*) '-- read INTx data'
         read(hdr_ifmtname(4:4),*) ii
         read(iin,'(a)') line
         i=(trimlen(line)+ii-1)/ii
         write(fmt,'(''('',i2.2,''i'',i1.1,'')'')') i,ii
         backspace(3)
         read(iin,fmt) (iy(i),i=1,hdr_nsamp)
         read(iin,'(4x,6x,i15)') ichecksum
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
      endif
c
      if(hdr_ifmtname(1:3).eq.'CMP')then
         if  (hdr_debug .gt. 3)  write (*,*) '-- read CMPx data'
         read(hdr_ifmtname(4:4),*) icomp 
         j=0
c        -- loop all data lines
10       continue
            read(iin,'(a)') line
            if(line(1:4).eq.'CHK1') goto 11
            nchar=trimlen(line)
            do i=1,nchar
               j=j+1
               cbuf(j)=line(i:i)
            enddo
         goto 10
11       continue
         read(line(5:),*) ichecksum
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
         nchar=j
         numout=c_sigsize   ! on input, give max. size of array iout (=iy)
         if  (hdr_debug .gt. 3)  write (*,*) '-- decompress', icomp
         if(icomp.eq.6) call DCOMP6(nchar,cbuf,numout,iy,ierror)
         if(icomp.eq.7) call DCOMP7(nchar,cbuf,numout,iy,ierror)
         if(icomp.eq.8) call DCOMP8(nchar,cbuf,numout,iy,ierror)
         if(ierror.eq.-1) write(6,*)'Integer*4 array not large enough !'
         if(numout.ne.hdr_nsamp)then
            write(6,*)'Error in retrieving correct number of samples !'
            write(6,*)'Expected ',hdr_nsamp,', received ',numout
         endif
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
      endif

c     -- remove first or second differences if necessary
      if  (hdr_debug .gt. 3)  write (*,*) '-- remove differences diff:',
     &   hdr_idiff, 'no. samples: ', hdr_nsamp
      do i=1,hdr_idiff
         call REMDIF1(iy,hdr_nsamp)
      enddo
      if  (hdr_debug .gt. 3)  write (*,*) '-- done'
c
      if (hdr_ifmtnum.eq.1) goto 50
      write(6,*) 'There is good news and bad news at this point:'
      write(6,*) 'First the good news: no more CPU time will be used!!!'
      write(6,*) 'and now the bad news: You have screwed up !!!'
      stop
c
c     -- The input data are GSE2.0
c
20    if  (hdr_debug .gt. 0)  write(*,*) '-- reading GSE2.0 format data'

c     -- parse WID2 line
200   format(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3,1x,a5,1x,a3,1x,
     &  a4,1x,a3,1x,i8,1x,f11.6,1x,e10.2,1x,f7.3,1x,a6,1x,f5.1,1x,f4.1)
      read( line(6:), 200 )  hdr_year, hdr_month, hdr_day, hdr_hour,
     &   hdr_min, fsec, hdr_station, hdr_chan, hdr_stadescr,
     &   hdr_ifmtname, hdr_nsamp, hdr_smprate, hdr_calfac, hdr_calper,
     &   hdr_instr, hdr_hang, hdr_vang
      hdr_sec = ifix( fsec )
      hdr_msec = nint( (fsec-float(hdr_sec)) * 1000.0 )
      if  (hdr_debug .gt. 3)  write (*,*) '-- got start time: ',
     &   hdr_year, hdr_month, hdr_day, hdr_hour, hdr_min, hdr_sec,
     &   hdr_msec
      if (hdr_nsamp .gt. c_sigsize) goto 998

c     -- compute julian day
cks      if  (ifix(float(hdr_year)/4.0)*4 .eq. hdr_year)  mnths(2)=29
cks      hdr_jday = 0
cks      do i=1,hdr_month-1
cks          hdr_jday = hdr_jday + mnths(i)
cks      enddo
cks      hdr_jday = hdr_jday + hdr_day
      call julien( hdr_day, hdr_month, hdr_year, hdr_jday, 1 )

c     -- read away all lines until DAT2, read possibly existing STA2-line
      if  (hdr_debug .gt. 3)  write (*,*) '-- find DAT2'
22    read( iin, '(a)' )  line
         if  (line(1:4) .eq. 'STA2')  then
            read( line,
     &         '(5x,a9,1x,f9.5,1x,f10.5,1x,a12,1x,f5.3,1x,f5.3)' )
     &         hdr_network, hdr_stalat, hdr_stalon, hdr_coosys,
     &         hdr_staelev, hdr_stadepth
            hdr_staelev = hdr_staelev * 1000.0
            hdr_stadepth = hdr_stadepth * 1000.0
            if  (hdr_debug .gt. 3)  write (*,*) '-- found STA2 ',
     &         hdr_network, hdr_stalat, hdr_stalon, hdr_coosys,
     &         hdr_staelev, hdr_stadepth
         endif
      if  (line(1:4) .ne. 'DAT2')  goto 22
      if  (hdr_debug .gt. 3)  write (*,*) '-- done'

c     -- Now read in data according to the format:
      if  (hdr_debug .gt. 2)   write (*,*) '-- reading samples'
      if  (hdr_ifmtname .eq. 'INT')  then
         if  (hdr_debug .gt. 3)  write (*,*) '-- reading INT data'
         read(iin,*) (iy(i),i=1,hdr_nsamp)
30       read(iin,'(a)') line
         if(line(1:4).eq.'CHK2') goto 31
         goto 30
31       read(line(5:),*) ichecksum
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
      endif
c
      if  (hdr_ifmtname(1:2) .eq. 'CM')  then
         read(hdr_ifmtname(3:3),*) icomp 
         if  (hdr_debug .gt. 3)  write (*,*) '-- reading CM', icomp
         j=0
c        -- reading all compressed lines
32       continue
            read(iin,'(a)') line
            if(line(1:4).eq.'CHK2') goto 34
            nchar=trimlen(line)
            do i=1,nchar
               j=j+1
               cbuf(j)=line(i:i)
            enddo
         goto 32
  34     continue
         read(line(5:),*) ichecksum
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
         nchar=j
         numout=c_sigsize   ! on input, give max. size of array iout (=iy)
         if  (hdr_debug .gt. 3)  write (*,*) '-- decompress data'
         if(icomp.eq.6) call DCOMP6(nchar,cbuf,numout,iy,ierror)
         if(icomp.eq.7) call DCOMP7(nchar,cbuf,numout,iy,ierror)
         if(icomp.eq.8) call DCOMP8(nchar,cbuf,numout,iy,ierror)
         if(ierror.eq.-1) write(6,*)'Integer*4 array not large enough !'
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
         if(numout.ne.hdr_nsamp)then
            write(6,*)'Error in retrieving correct number of samples !'
            write(6,*)'Expected ',hdr_nsamp,', received ',numout
         endif

c        -- remove first and second differences
         if  (hdr_debug .gt. 3)  write (*,*) '-- remove 2 differences'
         call REMDIF1(iy,hdr_nsamp)
         call REMDIF1(iy,hdr_nsamp)
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
      endif
c
50    continue
      if  (hdr_debug .gt. 3)  write (*,*) '-- check checksums'
      if (hdr_ifmtnum .eq. 1) then
c        -- calculate checksum for GSE1.0 format
         nchecksum=icheck_sum_gse1(iy,hdr_nsamp)
      elseif (hdr_ifmtnum .eq. 2) then
c        -- calculate checksum for GSE2.0 format
         nchecksum=icheck_sum_gse2(iy,hdr_nsamp)
      else
         print *, 'Unknown GSE-Format ',hdr_ifmtnum,'.x'
         stop
      endif
      if(nchecksum.ne.ichecksum)then
         write(6,*)'Error in checksum in the following waveform'
         write(6,'(a)') firstline(1:trimlen(firstline))
         write(6,*)'Read checksum       =',ichecksum
         write(6,*)'Calculated checksum =',nchecksum
         write(6,*)
         ichecksum=nchecksum  ! ... use the correct checksum  !!!
      endif
      if  (hdr_debug .gt. 2)  write (*,*) '-- leaving gsein'
      return
c
 998  print *,' >>>> WARNING: Program dimensions too small for ',
     &    ' the following waveform :'
          print *, line
      goto 1
 999  ierr = 999
      return
      end


c------------------------------------------------------------------------------


      subroutine gseout(iout,cbuf,iy,nchecksum)
c------------------------------------------------------------------------------
c     Input/Output parameters:
c        iout ........ logical unit for output data
c        cbuf ........ character array to store compressed data (see c_bufsize)
c        iy .......... integer array to hold raw waveform data (see c_sigsize)
c        nchecksum ... checksum value for data iy according to hdr_ifmtname
c
c        in common block
c        hdr_ifmtnum . GSE format identifier of input data
c        hdr_nsamp ... number of samples in array iy
c
c        disabled
c        iformat_in
c        nmax
c        format_out .. GSE format identifier for output data (e.g. "CM6")
c        iformat_out . GSE format identifier (ie. =2 for GSE2.0)
c        lin ......... character array to hold header lines
c        ilin ........ number of header lines
c        idiffout .... difference identifier
c------------------------------------------------------------------------------
c
      implicit none

      include 'codeco_common.f'

c     parameters
      integer*4 iout
      character*1 cbuf(*)
      integer*4 iy(*)
      integer*4 nchecksum

c     local variables
c
      integer*4 icomp, i,j, ierror
      integer*4 nchar, ii, ichecksum
      integer*4 trimlen
      integer*4 icheck_sum_gse1,icheck_sum_gse2
      integer*4 n
      real      fsec                ! seconds and milliseconds
      character line*132, fmt*132

c     -- executable code

      if  (hdr_debug .gt. 1)  write (*,*) '-- entering gseout'

      if (hdr_ifmtnum.eq.hdr_ofmtnum) then
          ichecksum=nchecksum
      else
         if (hdr_ofmtnum.eq.2)  then
            if  (hdr_debug .gt. 0)  write (*,*)
     &         '-- writing GSE2.0 output'
c           -- adjust calibration factor to displacement
            if  (hdr_calunit .gt. 0)  then
c              -- divide cal.factor 'hdr_calunit' times by 2*pi/hdr_calper
               hdr_calfac = hdr_calfac / 
     &            (atan(1.0)*8.0/hdr_calper)**hdr_calunit
               hdr_calunit = 0
            endif
c           -- calculate checksum for GSE2.0 format
            ichecksum=icheck_sum_gse2(iy,hdr_nsamp)
         else if  (hdr_ofmtnum .eq. 1)  then
            if  (hdr_debug .gt. 0)  write (*,*)
     &         '-- writing GSE1.0 output'
            ichecksum=icheck_sum_gse1(iy,hdr_nsamp)
            ichecksum=icheck_sum_gse1(iy,hdr_nsamp)   !!! why this ?
         else
            write (*,*) '** gseout: this should not happen'
            write (*,*) '** output format number is ', hdr_ofmtnum
            stop
         endif
      endif

c     -- compute differences if requested
      if  (hdr_debug .gt. 3)  write (*,*) '-- apply differences',
     &   hdr_odiff
      do i=1,hdr_odiff
         call DIF1(iy,hdr_nsamp)
      enddo
      if  (hdr_debug .gt. 3)  write (*,*) '-- done'

c     -- here write out header lines
      if  (hdr_debug .gt. 3)  write (*,*)  '-- write header line(s)'
      if  (hdr_ofmtnum .eq. 1)  then
c        -- first line is straightforward
100      format(a6,i4,i3.3,3(1x,i2.2),1x,i3.3,1x,i8,1x,a6,1x,a8,1x,
     &      a1,a1,1x,f11.7,1x,a6,1x,a4,1x,i1)
         write(iout,100) 'WID1  ', hdr_year, hdr_jday, hdr_hour,
     &      hdr_min, hdr_sec, hdr_msec, hdr_nsamp, hdr_station,
     &      hdr_stadescr, hdr_chan(1:1), hdr_chan(3:3),
     &      hdr_smprate, hdr_instr, hdr_ofmtname, hdr_odiff
c        -- second line's format depends on hdr_calfac
         fmt = '(f9.6,i1,f7.4,1x,4(f9.4,1x),2(f7.2,1x),f6.1,1x,f6.1)'
         n = 6
         if (hdr_calfac .ge. 100.0) n = 9-(log10(hdr_calfac)+3)
         write(fmt(5:5),'(i1)') n
         write(iout,fmt) hdr_calfac, hdr_calunit, hdr_calper,
     &      hdr_stalat, hdr_stalon, hdr_staelev, hdr_stadepth,
     &      hdr_beamaz, hdr_beamslo, hdr_hang
         write( iout, '(a)' )  'DAT1'
      else
         fsec = float(hdr_sec) + float(hdr_msec)/1000.0
200      format(a5,i4,2(1h/,i2.2),1x,2(i2.2,1h:),f6.3,1x,a5,1x,a3,
     &      1x,a4,1x,a3,1x,i8,1x,f11.6,1x,e10.2,1x,f7.3,1x,a6,1x,
     &      f5.1,1x,f4.1)
         write( iout, 200 )  'WID2 ', hdr_year, hdr_month, hdr_day,
     &      hdr_hour, hdr_min, fsec, hdr_station, hdr_chan,
     &      hdr_stadescr, hdr_ofmtname, hdr_nsamp, hdr_smprate,
     &      hdr_calfac, hdr_calper, hdr_instr, hdr_hang, hdr_vang
         fmt = '(a5,a9,1x,f9.5,1x,f10.5,1x,a12,1x,f5.3,1x,f5.3)'
c        -- need to change from f9.5 to f9.4 if hdr_stalat is empty (= -999.0)
         if  (hdr_stalat .le. -100.0)  fmt(14:14) = '4'
         write( iout, fmt )  'STA2 ', hdr_network, hdr_stalat,
     &      hdr_stalon, hdr_coosys, hdr_staelev/1000.0,
     &      hdr_stadepth/1000.0
         write( iout, '(a)' )  'DAT2'
      endif
      if  (hdr_debug .gt. 3)  write (*,*) '-- done'

      if(hdr_ofmtname(1:2).eq.'CM')then
         if (hdr_ofmtnum.eq.1) read(hdr_ofmtname(4:4),*) icomp
         if (hdr_ofmtnum.eq.2) read(hdr_ofmtname(3:3),*) icomp
         if  (hdr_debug .gt. 3)  write (*,*) '-- compress data', icomp
         nchar=c_bufsize ! on input, give max. size of array cbuf
         if(icomp.eq.6) call CMPRS6(hdr_nsamp,iy,nchar,cbuf,ierror)
         if(icomp.eq.7) call CMPRS7(hdr_nsamp,iy,nchar,cbuf,ierror)
         if(icomp.eq.8) call CMPRS8(hdr_nsamp,iy,nchar,cbuf,ierror)
         if  (ierror .eq. -1)  write(*,*)
     &      '** character array not large enough !'
         if  (hdr_debug .gt. 3)  write (*,*) '-- done, found nchar',
     &      nchar
C        TEST FOR MULTIPLE OF 80 CHARACTERS
         if(nchar-((nchar/80)*80).ne.0)then
            write(6,*)  '** not a multiple of 80 characters !'
         endif
         do i=1,nchar/80
            line = ' '
            do j=1,80
               line(j:j)=cbuf(80*(i-1)+j)
            enddo
            if  (hdr_debug .gt. 4) write(*,'(a)') line(1:trimlen(line))
cccc            write(iout,'(a)') line                      ! kradi, 28.11.90
c99 6.6.99/uk  if  (line .ne. ' ') write(iout,'(a)') line(1:trimlen(line))
            if  (line .ne. ' ') write(iout,'(a)') line(1:80)
         enddo
      endif
      if  (hdr_ofmtname(1:3) .eq. 'INT')  then
         if  (hdr_ofmtname(4:4) .eq. 'V' .or. 
     &      hdr_ofmtname(4:4) .eq. ' ')  then
            if  (hdr_debug .gt. 3)  write (*,*)
     &         '-- write uncompressed samples (INT,INTV)', hdr_nsamp
cks            write(iout,*) (iy(i),i=1,hdr_nsamp)
            do  i=1,hdr_nsamp
               write (iout,*) iy(i)
            enddo
         else
             if  (hdr_debug .gt. 3)  write (*,*) '-- write fixed INTx'
             read(hdr_ofmtname(4:4),*) ii
cc             i=(trimlen(line)+ii-1)/ii
             i=(80+ii-1)/ii
             write(fmt,'(''('',i2.2,''i'',i1.1,'')'')') i,ii
             write(iout,fmt) (iy(i),i=1,hdr_nsamp)
         endif
         if  (hdr_debug .gt. 3)  write (*,*) '-- done'
      endif

      if  (hdr_debug .gt. 3)  write (*,*) '-- write checksum'
      if (hdr_ofmtnum.eq.1) then
          line='CHK1'
          write(line(10:24),'(i15)') ichecksum
      endif
      if (hdr_ofmtnum.eq.2) then
          line='CHK2'
          write(line(6:13),'(i8)') ichecksum
      endif
      write(iout,'(a)') line(1:trimlen(line))

      if  (hdr_debug .gt. 1)  write (*,*) '-- leaving gseout'
      return
      end


c -----------------------------------------------------------------------------


      subroutine init_hdrvars

c     Initializes header variables in common block

      include 'codeco_common.f'

c     -- leave hdr_debug untouched
      hdr_year = 0
      hdr_month = 0
      hdr_day = 0
      hdr_jday = 0
      hdr_hour = 0
      hdr_min = 0
      hdr_sec = 0
      hdr_msec = 0
      hdr_nsamp = 0
      hdr_ifmtnum = 0
      hdr_ofmtnum = 0
      hdr_idiff = 0
      hdr_odiff = 0
      hdr_calunit = 0
      hdr_smprate = 0.0
      hdr_calfac = 1.0
      hdr_calper = 1.0
      hdr_stalat = -999.0
      hdr_stalon = -999.0
      hdr_staelev = -999.0
      hdr_stadepth = -999.0
      hdr_beamaz = -1.0
      hdr_beamslo = -1.0
      hdr_hang = -1.0
      hdr_vang = -1.0
      hdr_station = ' '
      hdr_stadescr = ' '
      hdr_chan = ' '
      hdr_instr = ' '
      hdr_network = ' '
      hdr_ifmtname = ' '
      hdr_ofmtname = ' '

      return
      end


c -----------------------------------------------------------------------------
 
