c
c  Convert PCSUDS files to SEISAN format, only pc
c  The program first converts to ascii using sud2asc, then to seisan format.
c
c  Testdata and SUD2ASC reading program supplied by 
c  REFTEK
c
c  Program limitations:
c  number of channels ans samples limited by seisan dimensions
c  max 3000 lines in suds header file
c
c  The channel names and station codes can come from the sudsei.def file, 
c  if no sudsei.def file the names are made as follows:
c 
c  Station code:
c  If a station name is given in header, that is used for all channels
c  If no station name available, the serial number is used
c  If no serial number, all stations are called SSS
c
c  Component codes:
c  If component given as one character (v,n,e), these are transformed
c  to Z,N,E. If no type, that is set to S for short period.
c  If no component info, the component will be given as S  1, S  2 etc
c  If component is x,y and z, it isassume it is an accelerometer.
c
c
c
c  Note: The program has only been tested on a few different data sets.
c        It seems that the text in the ascii files can be different and
c        since that is used to identify variables, the program migh not
c        work. So far, the serial number has been given in two different 
c        ways. Some time a program reading the suds file directly will be 
c        made, hopefully by you !!!
c
c  j. havskov, feb, 96
c
c  updates:
c  may 13 98 by jh: change name of def file, alternative way to read serial 
c                   number
c  march 99 jh    : --------------   version 7.0 check -------------------
c                   5 char station codes, year 2000, new def file format
c                   seisan dimentions
c  oct 1          : increase dimensions for suds header
c  feb 4 2000,jh  : fix program so it works in soem special cases
c  may 15         : new read resp
c  nov 12 2009 jh : write more than 30 channels
c  jan 16 2014 jh : copy the input file to a a file with a short name sud.inp
c                   before conversion, the converison program to ascii cannot
c                   work with long file names.
c                   some changes in component naming, skip a file if end of file
c
  
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
      integer  data(max_sample)
      integer*2 data2(max_sample)
c
c   block of definitions for most conversion programs
c
c-- main header------------------------------------------
      character*80 mainhead(max_trace)
      character*29 mainhead_text   ! text to be put into main header
c-- channel header
      character*1040 chahead
c-- output file name 
      character*80 outfile	  
c-- number of stations
      integer nchan	  
c-- channel to calculate channel header for
      integer ichan
      integer khead     ! number of header lines
c-- network code
      character*5 net_code	  
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)	  
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)
c--channel header date and times	  
      integer year(max_trace),month(max_trace),day(max_trace),
     *hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	  
c
c--------  end of block
c
      real clip                 ! clipping value
      integer nsudshead         ! number of headers in header file
      character*80 text(5000)   ! headers in suds header file 
      character*5  station      ! station code one trace
      character*4  component    ! ---------------------------
      character*5  serial       ! serial number of current unit
      integer flength           ! length of input file name
      integer seiclen           ! function to get length of string
c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question
      integer check_nsamp   ! value used for checking
c--Counters etc
	  integer i,k,code
c

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
       call read_def_chan('sudsei.def',mainhead_text,net_code)
c
c   get file names to convert
c
      in=0
      question=' File name, # or filenr.lis for all'
      call filename(question,infile)	    
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis'.or.
     *infile(1:10).eq.'FILENR.LIS') then
         open(8,file='filenr.lis',status='old',err=20)
         goto 21
 20      continue
         write(6,*)' No filenr.lis'
         stop
 21      continue
         in=1
      endif
c
c   file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') infile
	  flength=seiclen(infile)   ! length of file name
c
c   convert file to ascii using PCSUDS program suds2asc
c
      call systemc('copy '//infile(1:flength)// ' sud.inp',13+flength)
      call systemc
     *('sud2asc sud.inp suds.out',24)
c
c      call systemc
c     *('sud2asc '//infile(1:flength)//' suds.out',8+flength+9)
c
c   open output file and read main info
c
       open(1,file='suds.out',status='old')
       ichan=0        ! reset channel counter
c
c   read header belonging to one channel into text array
c
 2000  continue       ! from below after reading one channel
       ichan=ichan+1

       i=0
 1     continue
          i=i+1
          read(1,'(a)',end=2299) text(i)
c
c   check if end of trace descripture structure
c
          if(text(i)(34:48).eq.'rate correction') goto 2
          goto 1
 2     continue
       goto 2300
 2299  continue
       if(i.lt.10) then
          write(6,*) 'Empty file'
          if(in.eq.0) stop
         close(1)
         close(2)
         goto 1000
       else
          goto 99
       endif
 2300  continue
       nsudshead=i                ! number of headers
       write(6,*)' Number of suds headers', nsudshead
c
c   get info from header
c
       station=' '
       component=' '
       serial='SSSSS'     ! serial number, default is SSSSS
       cbyte(ichan)='4'   ! initially assume 4 byte
       year(ichan)=0
       nsamp(ichan)=0
c
       nchan=1
       do i=1,nsudshead
         if(text(i)(34:51).eq.'samples per second') then  ! get sample rate
            read(text(i)(1:20),'(f10.2)') rate(ichan)
         endif
c
c  2 or 4 byte values from clipping level
c
         if(text(i)(34:47).eq.'clipping value') then  ! get clip value
            read(text(i)(1:20),'(f14.2)') clip
            if(abs(clip).lt.33000) cbyte(ichan)='2'   ! 2 byte
         endif
c
         if(text(i)(34:52).eq.'initial sample time') then ! file start time
            read(text(i)(7:8),'(i2)') year(ichan)    ! year
            if(year(ichan).lt.50) then
               year(ichan)=year(ichan)+2000
            else
               year(ichan)=year(ichan)+1900
            endif
            read(text(i)(1:2),'(i2)') month(ichan)   ! month
            read(text(i)(4:5),'(i2)') day(ichan)     ! day of month
            read(text(i)(10:21),'(i2,1x,i2,1x,f6.3)') 
     *      hour(ichan),min(ichan),sec(ichan)
         endif 
c
         if(text(i)(34:45).eq.'station name') station=text(i)(1:5)  ! station code
         if(text(i)(34:45).eq.'component   ') then
            if(text(i)(1:1).eq.'v') component='   Z'  ! component code
            if(text(i)(1:1).eq.'e') component='   E'  ! component code
            if(text(i)(1:1).eq.'n') component='   N'  ! component code
c
c   assume accelerometer
c
            if(text(i)(1:1).eq.'z') component='HN Z'  ! component code
            if(text(i)(1:1).eq.'x') component='HN E'  ! component code
            if(text(i)(1:1).eq.'y') component='HN N'  ! component code

            if(text(i)(1:1).eq.'V') component='   Z'  ! component code
            if(text(i)(1:1).eq.'E') component='   E'  ! component code
            if(text(i)(1:1).eq.'N') component='   N'  ! component code
         endif
         if(component(4:4).ne.'N'.and.component(4:4).ne.'E'.
     *   and.component(4:4).
     *   ne.'Z') component='Z'
c
         if(text(i)(34:50).eq.'number of samples') then       ! event duration, secs
            call sei get values(1,text(i)(1:20),code)
            nsamp(ichan)=array$(1) 
         endif
c
         if(text(i)(14:23).eq.'Header S/N') serial=text(i)(24:28)
         if(text(i)(34:52).eq.'inst. serial number') 
     *   serial=text(i)(1:5)
       enddo
       write(6,'(a,a,a,a)')' Station ', station,'  Serial ',serial
       write(6,*) 'nsamp=',nsamp(ichan)
c
c   check if values are reasonable, could be some 'funny' channel,
c   seen with IREG time info
c
         if(nsamp(ichan).eq.0) then
            write(6,*) ' Skip channel, funny content'
            ichan=ichan-1
            goto 2000
         endif

c
c   first put in defaults in case no def file
c
            if(station.ne.'     ') then
               stat(ichan)=station
            else
               stat(ichan)=serial
            endif
            if(component(1:2).eq.' ') then
                comp(ichan)(1:2)='S '
            else
                comp(ichan)(1:2)=component(1:2)
            endif
            if(component(3:4).eq.'  ') then
               write(comp(ichan)(3:4),'(i2)') ichan
            else
               comp(ichan)(3:4)=component(3:4)
            endif
c
         write(6,'(1x,a5,i4,1x,4i3,f7.1)') stat(ichan),
     *   year(ichan),month(ichan),
     *   day(ichan),hour(ichan),min(ichan),sec(ichan)
c
c   put in definition from def file
c
         call set_def_chan(ichan,stat(ichan),comp(ichan))

c
c   read data
c
         read(1,*,end=5555)(data(i),i=1,nsamp(ichan))
         goto 5554
 5555    continue
         write(6,*) 'Error with file, skip'
         close(1)
         close(2)
         goto 1000
 5554    continue

c
c   go back for info on next channel header 
c
      goto 2000
c
c-------------------------------------------------------------------
c   now whole file has been read once and all header info available
c   rewind , read again to write out
c-------------------------------------------------------------------
c
 99   continue
c
      rewind 1

c
c   make seisan headers
c
      nchan=ichan-1
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,nsamp,
     *                 rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',outfile
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main header
c
      write(6,'(1x,a)')mainhead(1)(1:75)

      khead=(nchan-1)/3+3
      if(khead.lt.12) khead=12 
      do i=1,khead
         write(2)mainhead(i)
      enddo
C
C   enter channel  loop
c
      do ichan=1,nchan
c
c   make channel header	
c
          call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
          write(6,'(1x,a)') chahead(1:78)
c
c   read channel data,first read down to channel data
c
 1111     continue     ! get here if skip 'funny' channel from below
          i=0
 11       continue
          i=i+1
          read(1,'(a)') text(i)
c
c   check if end of trace descripture structure
c
          if(text(i)(34:48).eq.'rate correction') goto 22
          goto 11
 22       continue
          nsudshead=i
c
c   check header content to make sure it is not a 'funny' channel
c
          check_nsamp=0
          do i=1,nsudshead
c
           if(text(i)(34:50).eq.'number of samples') then       ! event duration, secs
              call sei get values(1,text(i)(1:20),code)
              check_nsamp=array$(1) 
           endif
          enddo
c
       if(check_nsamp.eq.0) then
          goto 1111
       endif

           read(1,*)(data(i),i=1,nsamp(ichan))
c
c   read response curve into header
c

           call read_resp_head(chahead)                                            
c
c   check if response curve
c
c           if(chahead(160:160).eq.'9') write(6,*)
c     *     ' No response file for this channel --------'
c
           write(2)chahead
           if(cbyte(ichan).eq.'2') then
             do i=1,nsamp(ichan)
                data2(i)=data(i)
             enddo
             write(2)(data2(k),k=1,nsamp(ichan))
           else
             write(2) (data(k),k=1,nsamp(ichan))
           endif
c
c   end of one channel
c
      enddo
c
c   end of one file
c
      close(1)
      close(2)
c
      write(6,*)
c
c  back for next file if many
c
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  
