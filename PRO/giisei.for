c
c
c  converts geophysical institute of Israel imported  DAQ files to 
c  SEISAN format. The initial station codes are as defined in file, can
c  be converted with normal .def file. If 4 character of station name
c  indicate componenet (N or E), that is blanked out and transferred to 
c  4. character of componenet name BEFORE using the def file conversions.
c
c
c  This program works with a max of 30 channels, if you need more, test files
c  are needed to fix it
c
c  sep 2001 tested with 32 channels
c
c  j. havskov, sep 98
c
c
c
c
c
c  updates:
c
c  may 1999 jh   : ---------------- version 7 upgrade -----------------------
c  sep 2001      : 32 channels
c

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

c-- output data vector	  
      integer*4  data4(max_sample)
      integer*2  data2(max_sample)
      character*3000 block  ! input file header
      integer*2 i_rate
      character*2 c_rate
      integer kbyte,k
      integer first            ! first record of samples-1
c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
      integer i,irec
c-- computer type
      logical pc,sun,linux	  
       integer i_samp    ! number of samples
       character*4 c_samp! ----------------

c
c
      character*1 cc
      character*2 c_short
      integer*2   i_short
c time
      character*16 time
      integer*2 i_time(8)
      integer i_magic               ! magig number
      character*4 magic 
c
      equivalence(i_rate,c_rate)
      equivalence (c_samp,i_samp)
      equivalence (c_short,i_short)
      equivalence (time,i_time)
      equivalence (magic,i_magic)
      

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      call computer_type(sun,pc,linux)

c
c   get def file for station codes
c
       call read_def_chan('giisei.def',mainhead_text,net_code)

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
       open(1,file=infile,status='old',access='direct',recl=3000)
c
c   read header block to get start time etc
c
c
 1     continue
       read(1,rec=1,err=99) block
       close(1)

c
c   magic number
c
       magic=block(1:4)
       if(sun) call swap4_one(i_magic)
c
c   number of samples
c
       c_samp=block(13:16)
       if(sun) call swap4_one(i_samp)
c
c   sample rate
c
       c_rate=block(11:12)
       if(sun) call swap2_one(i_rate)
c
c   number of channels
c
       c_short=block(9:10)
       if(sun) call swap2_one(i_short)
c
c   time
c
       time=block(2285:2300)
       if(sun) call swap2(8,i_time)
c
       write(6,*)' Magic number', i_magic
c
c   number of channels
c
       nchan=i_short
       write(6,*) ' Number of channels, samples and sample rate',
     * nchan,i_samp,i_rate
c
c   get times
c
       year(1) =i_time(6)
       month(1)=i_time(7)
       day(1)  =i_time(8)
       hour(1) =i_time(2)
       min(1)  =i_time(3)
       sec(1)  =i_time(4)+i_time(5)/1000.0
       
       write(6,*) year(1),month(1),day(1),hour(1),min(1),sec(1)
c
       write(6,*) 'Number of samples',i_samp
c
c   2 or 4 bytes
c
       cbyte(1)=' '   ! default 2 bytes, i_magic=131161
       if(i_magic.eq.190266) cbyte(1)='4'
c
c  get stations, assume max 64 channels in input file
c  but only read 32
c
       read(block(1402:1402+32*5-1),'(32(a4,1x))') (stat(i)(1:4),i=1,32)
       do i=1,32          ! only 4 char station names
         stat(i)(5:5)=' '
       enddo
c
c   assume same start time etc. for all channels
c
       do i=1,nchan
          year(i)=year(1)
          month(i)=month(1)
          day(i)=day(1)
          hour(i)=hour(1)
          min(i)=min(1)
          sec(i)=sec(1)
          rate(i)=i_rate
          nsamp(i)=i_samp
          cbyte(i)=cbyte(1)
          comp(i)='S  Z'
       enddo
c
c   if the component is given in the 4 char of station name, use that
c
        do i=1,nchan
          if(stat(i)(4:4).eq.'N'.or.stat(i)(4:4).eq.'E')then
             comp(i)(4:4)=stat(i)(4:4)
             stat(i)(4:4)=' '
          endif
        enddo
c
c  enter loop to define channels if a def file
c
       do ichan=1,nchan
c
c   put in definition from def file
c
           call set_def_chan(ichan,stat(ichan),comp(ichan))
       enddo
c   remove null chars
       do k=1,5
          if(ichar(stat(i)(k:k)).eq.0) stat(i)(k:k)=' '
       enddo
c
c   make seisan header
c
      ichan=1
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
c   write main head
c
      do i=1,12
         write(2)mainhead(i)
         write(6,'(a80)') mainhead(i)
      enddo
c
c  if mor ethan 30 channels
c
      if(nchan.gt.30) then
         k=(nchan-31)/3+1
         do i=13,k+12
            write(2)mainhead(i)
            write(6,'(a80)') mainhead(i)
         enddo
      endif
c
c   calculate address of first sample 
c
      kbyte=2
      if(cbyte(1).eq.'4') kbyte=4   ! 2 or 4 byte integers
      first=2300+nchan*2*kbyte
      first=first/kbyte
c
c   open again with record length kbyte to read samples later
c
       
      open(1,file=infile,status='old',access='direct',recl=kbyte)
c
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
         write(6,'(1x,a)') chahead(1:78)
c
c   get response
c                                                                               
         call read_resp_head(chahead)                                            
c
c   read samples
c
          do i=1,i_samp
            irec=first+i+i_samp*(ichan-1)
c            write(88,*) i,irec
            if(kbyte.eq.2) read(1,rec=irec) data2(i)
            if(kbyte.eq.4) read(1,rec=irec) data4(i)
          enddo
c
c   write data
c
         write(2)chahead
         if(kbyte.eq.2) then
c
c   BUGGGGGG
c
c   the call swap2 does not work here on solris for some files, ok on pc
c   a temporary fix is to do swap directly as done below, problem seem to
c   be that nothing happens in the swap routine in this special case, must be 
c   a memory problem somewhere
c   the swap4 has not been tested 
c
         if(sun)then 
             do i=1,i_samp
               i_short=data2(i)
               cc=c_short(1:1)
               c_short(1:1)=c_short(2:2)
               c_short(2:2)=cc
               data2(i)=i_short
             enddo
c              
c            call swap2(i_samp,data2)
          endif
            write(2) (data2(i),i=1,i_samp)
         endif
         if(kbyte.eq.4) then
            if(sun) call swap4(i_samp,data4)
            write(2) (data4(i),i=1,i_samp)
         endif
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
								  


