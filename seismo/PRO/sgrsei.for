c
c  Converts SeisGram binary files to SEISAN format
c  Maximum number of samples as defined in seisan, at least 500 000,
c  channels codes can be defined using the sgrsei.def definition file. If
c  no definition file, the station name is SGR and the component is S  Z,
c  S  N and S   E.
c  The program has seen very limited testing, and only on 3 componenet data
c
c  The input real values have been multiplied by 100 000 before converting
c  to integers
c
c  The channel order is assumed to be Z, N, E, no checks are made in
c  input file, if wrong it can be changed with def file
c
c  The program inly read 2 bytes at a time, so it is slowwwww
c
c  j. havskov, oct 2000
c
c  updates:
c

      implicit none
      include 'seidim.inc' ! dimensions
c-- output data vector	  
      real       y(max_sample*3)
      character*2 cy(max_sample*6)
c
c-----------------------------------------------------------------------
c   start block of definitions for most conversion programs
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
c-- channel samples and sample 
      integer nsamp(max_trace)
      real rate(max_trace)	  
c
c--end block of definitions most conversion programs
c---------------------------------------------------------------------

      integer doy         ! day of year
      character*80 text   ! general text

c------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
      integer i,j,k
c-- computer type
      logical pc,sun,linux
      character*258 block     ! header
      real x4                 ! for data manipulation
      integer*2 x2
      character*4 char4
      character*2 char2
      equivalence (x4,char4)
      equivalence (x2,char2)
      equivalence (cy,y)
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)
c
c   get def file for station codes, give file name
c
       text='sgrsei.def'
       call read_def_chan(text,mainhead_text,net_code)

c
c   get file name
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
c
c  open file 
c
       open(1,file=infile,status='old',access='direct',recl=258)
c
c   read header block to get start time etc
c
c
       read(1,rec=1,err=99) block
       close(1)

c
c   number of channels
c
       read(block(2:2),'(i1)') nchan
c
c   get file start time etc
c
       stat(1)=block(43:47)       ! station code
       char4=block(97:100)        ! sample rate
       rate(1)=1.0/x4
       char2=block(119:120)       ! year
       year(1)=x2
       char2=block(121:122)       ! day of year
       doy=x2
       char2=block(123:124)       ! hour
       hour(1)=x2
       char2=block(125:126)       ! min
       min(1)=x2
       char4=block(127:130)       ! sec
       sec(1)=x4
c
c   calculate day and month from doy
c
       call dte(doy,day(1),month(1),year(1))

       write(6,*) 'ymdhms',year(1),month(1),day(1),hour(1),min(1),sec(1)
       write(6,*) 'rate,nchan',rate(1),nchan
c
c   read all data
c
       open(1,file=infile,status='old',access='direct',recl=2)

       i=130
       k=1
  333  continue
       read(1,rec=i,err=334) cy(k)
       k=k+1
       i=i+1
       goto 333
  334  continue
       nsamp(1)=(k-1)/(2*nchan)

c
c------------------------------------------------------
c  all data read
c------------------------------------------------------
c
 50   continue
      close(1)
      write(6,*) 'Number of samples', nsamp(1)
      cbyte(1)='4'            ! always assume 4 byte data
c
c   default station and channel names
c
      comp(1)='S  Z'
      comp(2)='S  N'
      comp(3)='S  E'

c
c   make seisan headers
c
      do i=2,nchan
        stat(i)=stat(1)
        year(i)=year(1)
        month(i)=month(1)
        day(i)=day(1)
        hour(i)=hour(1)
        min(i)=min(1)
        sec(i)=sec(1)
        rate(i)=rate(1)
        nsamp(i)=nsamp(1)
        cbyte(i)=cbyte(1)
      enddo

c
c change station or component names according to def file
c
      do i=1,nchan
        call set_def_chan(i,stat(i),comp(i))
      enddo
       write(6,*) 'ymdhms',year(1),month(1),day(1),hour(1),min(1),sec(1)
       write(6,*) 'rate',rate(1)

      ichan=1
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
      write(6,'(a,a)')' Output file name is: ',outfile
c
c   open output file
c
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c

      do i=1,12
         write(2)mainhead(i)
         write(6,'(a80)') mainhead(i)
      enddo
c
C-----------------------------------------
C   enter channel  loop
c-----------------------------------------
c
      do ichan=1,nchan
c
c   make channel header	
c
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                    net_code,mainhead_text,stat,comp,
     *                    nsamp,rate,cbyte,
     *                    outfile,mainhead,chahead)
c
c   write rate a bit more accurate
c
         if(rate(1).lt.100.0) write(chahead(37:43),'(f7.4)') rate(1)
         if(rate(1).lt.10.0) write(chahead(37:43),'(f7.5)') rate(1)
         write(6,'(1x,a)') chahead(1:78)
c                                                                               
c   get response            
c                                                                               
         call read_resp_head(chahead)
c
c   write data
c
         write(2)chahead
         write(2)(int(y(i)*100000),i=ichan,3*nsamp(ichan),3)
c
c   end of channels loop
c
      enddo
      write(6,*)
c
c  back for next file if many
c
      goto 101
 99   continue
      write(6,*)' Error with file'
 101  continue
      close(2)
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  
c
       subroutine get_36_string(val,string6)
c
c   decode guralp base 36 number
c
c      val: input integer
c      string6: 6 char output string
c
       implicit none
       character*6 string6
       integer val
       character*1 st(36)
       logical sun,pc,linux    ! computer type
       integer i,j,k
       data st/'0','1','2','3','4','5','6','7','8','9',
     *         'A','B','C','D','E','F','G','H','I','J',
     *         'K','L','M','N','O','P','Q','R','S','T',
     *         'U','V','W','X','Y','Z'/

        string6=' '      ! initialize with blanks
c
        call computer_type(sun,pc,linux)

c
c   swap if on pc
c
       if(pc) call swap4_one(val)
c
       do i=1,6
          j=val/36
          k=val-j*36
          if(k.ge.0) string6(7-i:7-i)=st(k+1)
          val=j
       enddo

       return
       end

      
      subroutine read_guralp_head(block,year,month,day,hour,min,sec,
     *                            rate,nrecord,comp_code)

c
c  read part of one 16 byte guralp gcf header
c
      implicit none
      character*1 block(1024)   ! guralp data block
      integer year,month,day,doy,hour,min
      real sec
      real rate           ! sample rate
      integer nrecord     ! number of records in data block
      integer comp_code   ! compression code, see guralp format description
      integer i,k
      integer seconds_day ! seconds since midnight
      integer day_89      ! days since 17 nov 1989
      double precision  time89  ! for time conversion
c
c  date and time
c
      day_89=ichar(block(9))*256+ichar(block(10))
      k=day_89          ! save
      day_89=day_89/2   ! lowest bit is part of 17 bit second number
      seconds_day    =ichar(block(11))*256+ichar(block(12))
c
c  check if bit 17 is used
c
      i=k-day_89*2
      if(i.eq.1) seconds_day=seconds_day+65536 ! 17 bit is used
      call timsec(1989,11,17,0,0,0.0,time89)   ! time from 1900 in secs
      time89=time89+day_89*24*3600.0+seconds_day     ! time till now
      call sectim(time89,year,doy,month,day,hour,min,sec) ! data now
c
c  sample rate
c
      rate=ichar(block(14))
c
c  compression code
c
      comp_code=ichar(block(15))
c
c   number of records
c
      nrecord=ichar(block(16))

      return
      end
