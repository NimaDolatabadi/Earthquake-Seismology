c
c  Convert kinemetrics dataseis files to seisan format. 
c
c  This is program KINSEI,  Aug 96
c
c  r. Arvidsson, march, 1992, original program for PDAS now changed for
c                             kinetrics input data
c                92-03-22     Currently working for PC however only with
c                             16 bits - 2 byte word. 12 bits not fixed yet.
c                92-04-27     bugs , r.a.
c                92 11 15     cut cuttim from end ofdata, jh
c                97 11  7     use a def file, some clean up
c                april 7 99 jh------------  version 7.0 --------------
c                             new general standard, clean up
c                feb 2000, jh major clenaup, junk code removed, bit
c                             manipulation greatly simplified, unix version 
c          may 11  00    lo : use read_resp_head
c          nov 20  01    jh : write out more than 30 channels
c  2010-05-12 pv : logical bhr and bmin change to integer, gave error in f90
c
c ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
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
      character*80 text
c-- number of stations
      integer nchan  
c-- channel to calculate channel header for
      integer ichan
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
c-- input data vector
      integer*2 data(max_sample)
c-- input file name
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in
c-- question
      character*80 question
c-- interval to cut off end
      real cuttim
c-- number of codes
      integer istat
c-- sample rate hz*10
      integer*2 ismpl
c--kin    headers
      character*1 kinhead(2048)
c-- date and time
      integer iday, yr, mth, ndoy
      real        xsec
c--Counters
      integer isamp,ib,i,k,kk,flag, j, jj, ii, filecount,ik
c--Logical check
      logical lhead
c -- logcal used to show if filenr.lis present
      logical flis
c-- block size
      integer blksize
      logical pc,sun,linux  

c -----------------------------------------------------------------
c
c  Kinemetrics header, first block 2048 bytes
c
c -----------------------------------------------------------------
c --
c--  no chan,  samples/sm,
c --
      integer*2  nnchan, nochan 
      equivalence (kinhead(3),nnchan)
      equivalence (kinhead(7),ismpl)

c --
c -- user comments and channel names
c --
      character*32 userc
      character*4  chname(64)
      equivalence (kinhead(225),userc),(kinhead(257),chname)


c--------------------------------------------------------------------
c
c Kinemetrics datablocks 2048 bytes/record
c
c--------------------------------------------------------------------

      integer*2 bcode, byear, bday, bmsc,
     *          muldata(1016)
c     integer*1 bhr, bmin, bqual
c     logical*1 bhr, bmin, bqual
      logical*1 bqual
c pv : logical bhr and bmin gave error in line ~252 
c lo : on old solaris compilers logical*1 for all 3 must be used
      integer*1 bhr, bmin
      character*1 cdata(2048)
      equivalence (cdata(1),bcode)
      equivalence (cdata(3),byear),(cdata(5),bday)
      equivalence (cdata(7),bhr),(cdata(8),bmin)
      equivalence (cdata(9),bmsc),(cdata(11),bqual)
      equivalence (cdata(17),muldata)


c
c print version
c
      include 'version.inc'
      out_version_date='Nove 20, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)


      mainhead_text=' '
      cuttim=5.0    ! cut last 5 seconds of data

c
c   get def file for station codes
c
       text='kinsei.def'
       call read_def_chan(text,mainhead_text,net_code)
c
      blksize=2048
c
      istat=1
c ---------------------------------------------------------------
c
c   get file name
c
      in=0
      question=' File name, # or filenr.lis for all'
11    call filename(question,infile)
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis') then
         open(8,file='filenr.lis',status='old',err=20)
         goto 21
 20      continue
         write(*,'(a)')' No filenr.lis'
         stop
 21      continue
         in=1
         flis = .true.
      endif
c
c -------------------------------------------------------
c
c   file loop if many files
c --------------------------------------------------------
  999 continue
      filecount= filecount + 1
      if(flis) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') infile
      open(1,file=infile,access='direct',recl=blksize,status='old')
c
c  read loop
c
      isamp=0
      ib=1
      flag=0
c-- read header
          k = 1
           do 800 ik=1, (2048/blksize)
              read(1,rec=k,err=10) kinhead
              k=k+1
800        continue
c
c   swap if sun
c
           if(sun) call swap2_one(nnchan)
           if(sun) call swap2_one(ismpl)
           nchan=nnchan
c--- channel
        nochan=nchan
c--- rate
       rate(1)=float(ismpl)/10.0
c
c   read data
c
c---------------------------------------------------------------
       do 1000 jj = 1, nchan     ! channel loop
c --------------------------------------------------------------
c
           isamp=0
           k=2
55         continue
           if(isamp.gt.max_sample) then
              write(6,*)' Too many samples in input file'
              stop
           endif
           do 900 ik=1, (2048/blksize)
              read(1,rec=k,err=10) cdata
              k = k + 1
900        continue
c--
c-- Check if main header should be created
c--
           if (k.eq.3 .and. jj.eq.1) then
              lhead=.true.
           else
             lhead=.false.
           endif
c--
c --- find times, first swap if on sun
c--
c
          if (lhead) then
             if(sun) then
                call swap2_one(byear)
                call swap2_one(bday)
                call swap2_one(bmsc)
             endif
c-- day of year
             yr=byear
             ndoy=bday
             call dte(ndoy,iday,mth,yr)
c
c   convert from signed integer to unsigned integer
c
             if(bmsc.ge.0) then
                xsec=float(bmsc)/1000.0   ! same as signed
             else
                xsec=(2**16+bmsc)/1000.0
             endif
             sec(1)=xsec
c ----------------------------------------------------------------
             year(1)=byear
             month(1)=mth
             hour(1)=bhr
             min(1)=bmin
             sec(1)=xsec
             day(1)=iday
          endif
c
c---------------------------------------------------------------
c  loop to read data
c
       if(sun) call swap2(1016,muldata)
c
       kk=1016/nochan
       kk=kk*nochan               ! location of last real sample
       do 56 ii=jj,kk,nochan
          isamp = isamp + 1
          data(isamp)=muldata(ii)
c
c   convert to unsigned integer
c
          if(data(isamp).lt.0) data(isamp)=2**16+data(isamp)
c
c   convert to signed integer
c
          data(isamp)=data(isamp)-2**15

56     continue

       go to 55
c -------------------------------------------------------------

10     continue
       isamp=isamp-cuttim*rate(1)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   make main header if first channel
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       if (jj .eq. 1 ) then

c
c -- fix network comments if not given in def file
c
       if(mainhead_text.eq.' ') then 
          mainhead_text= userc(1:29)
       endif

c
       cbyte(1)=' '
       nsamp(1)=isamp
       do i=2,nchan
         year(i)=year(1)
         month(i)=month(1)
         day(i)=day(1)
         hour(i)=hour(1)
         min(i)=min(1)
         sec(i)=sec(1)
         rate(i)=rate(1)
         cbyte(i)=' '
         nsamp(i)=nsamp(1)
       enddo
c
       do i=1,nchan
           comp(i)='S  Z'
           stat(i)(1:4)=chname(i)
           do j=1,4
             if(ichar(stat(i)(j:j)).eq.0) stat(i)(j:j)=' '
           enddo
           stat(i)(5:5)=' '
           if(stat(i).eq.' ') then 
              write(stat(i),'(i2)') i
              if(stat(i)(1:1).eq.' ') stat(i)(1:1)='0'
              stat(i)(3:4)=' '
           endif
c
c   put in definition from def file
c
           call set_def_chan(i,stat(i),comp(i))
       enddo
       write(6,*) year(1),month(1),day(1),hour(1),min(1),sec(1)
       write(6,*)' Sample rate',rate(1)
       write(6,*)' Number of channels', nchan


         ichan=1   ! just make main header
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,nsamp,
     *                 rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
         write(6,'(a,a)')' Output file name is: ',outfile
         open(2,file=outfile,status='unknown',form='unformatted')  
       endif
c ----------------------------------------------------
c
c   write data
c
      if (jj .eq. 1) then
         do i=1,12
            write(2) mainhead(i)
         enddo
         if(nchan.gt.30) then
            k=(nchan-31)/3+1
            do i=13,k+12
               write(2) mainhead(i)
            enddo
         endif
      endif

c
c   make channel header
c

         call sheads(year,month,day,hour,min,sec,nchan,jj,
     *                 net_code,mainhead_text,stat,comp,nsamp,
     *                 rate,cbyte,
     *                 outfile,mainhead,chahead)


      write(6,'(1x,a)')chahead(1:70)
c --
c -- write channel number
c --
c      write(6,'(a,i5)') ' channel ', jj

c
c   read response curve into header
c

           call read_resp_head(chahead)             

      write(2) chahead
      write(2)(data(i),i=1,isamp)

1000  continue
c ----------------------------------------------------------------
      write(6,*)
c --
c -- close output and input file
c--
      close(1)
      close(2)
c
c  back for next file if many
c
      if(in.eq.1) goto 999
      if(in.eq.0) goto 11
99    continue
      stop
      end

