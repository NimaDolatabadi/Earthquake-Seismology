c
c  Converts Guralp GCF files to SEISAN format, only works with one channel
c  data. Maximum number of samples as defined in seisan, at least 500 000,
c  channels codes can be defined using the gursei.def definition file. If
c  no definition file, the station name is GURAL and the component is taken
c  the gcf header.
c  
c
c   note sun os must have a small change, see below
c
c  j. havskov, sep 2000
c
c  updates:
c
c  nov 14 2001 jh : also check for linux, logical *1 cannot be used on sus linux
c                   put in first 4 letters from stream as station
c  may    2009 wc : add station name to filename
c  nov 27 2011 jh : remove subroutines and put in file /LIB/guralp.for
c  dec  6 2012 lo : deal with AFAD filenames, possible to have 3-5 char st codes
c                   change nsamp at end to nsamp-1

      implicit none
      include 'seidim.inc' ! dimensions
      include 'chandef.inc'! channel definition
      integer seiclen
c-- output data vector	  
      integer*4  data(max_sample)
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
c-- times since epoch for checking that blocks are contiguous (RL 03/2006)
      double precision start_time
      double precision prev_end_time
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	  
c
c--end block of definitions most conversion programs
c---------------------------------------------------------------------

      integer yr,mo,da,hr,mn    ! date and time any block
      real se,ra                ! second and sample rate, any block
      character*80 text         ! general text

c------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
      integer i,j
c-- computer type
      logical pc,sun,linux
      integer nsmp            ! sample counter
      integer nrecord         ! records in following data section
      integer comp_code       ! guralp compression code
      character*6 system_id,stream_id  ! guralp data identifiers
      integer x4(256)         ! 4 byte integers in a block
      character*1 block(1024) ! corresponding string
      integer*2 x2(512)       ! 2 byte integers in a block
      integer*1 x1(1024)      ! one byte intger, for linux or pc
cfor sun or pc     logical*1 x1(1024)      ! one byte intger
c      logical*1 x1(1024)      ! one byte intger
      integer*4 xlast         ! last value in block, used for control
      integer max24           ! largest 24 bit value
      integer nsamp_block     ! number of samples in one block
      integer sample          ! one sample
      character*4 csample     ! same as string
      equivalence (x4,block)
      equivalence (x2,block)
      equivalence (x1,block)
      equivalence (sample,csample)
      logical afad,defread    ! keep track of def file reading
c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      max24=2**23
      defread=.false.

      call computer_type(sun,pc,linux)
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
c check if afad file
c
c0121114_010559_ARTV_3M81N2.gcf
      afad=.false.
      if ((seiclen(infile).eq.31.and.
     &    infile(9:9).eq.'_'.and.
     &    infile(16:16).eq.'_'.and.
     &    infile(28:31).eq.'.gcf').or.
     &    (seiclen(infile).eq.30.and.
     &    infile(9:9).eq.'_'.and.
     &    infile(16:16).eq.'_'.and.
     &    infile(27:30).eq.'.gcf').or.
     &    (seiclen(infile).eq.32.and.
     &    infile(9:9).eq.'_'.and.
     &    infile(16:16).eq.'_'.and.
     &    infile(29:32).eq.'.gcf')) then
c
c remove current channel definition
c
          write(*,*) ' AFAD file, use station definition from filename'
          ndef_chan=1
          net_code='AFAD'
          i=index(infile(17:),'_')
c          write(*,*) infile(17:),' _ ',i
          def_in_stat(1)=infile(17+i:17+i+3)
          def_out_stat(1)=infile(17:17+i-2)
          def_in_comp(1)=' '
          def_out_comp(1)=' '
          defread=.false.
          afad=.true.
cdagi_4318z.gcf
      elseif ((seiclen(infile).eq.14.and.
     &      infile(5:5).eq.'_'.and.
     &      infile(11:14).eq.'.gcf') 
     &      .or.(seiclen(infile).eq.13.and.
     &      infile(4:4).eq.'_'.and.
     &      infile(10:13).eq.'.gcf') 
     &      .or.(seiclen(infile).eq.15.and.
     &      infile(6:6).eq.'_'.and.
     &      infile(12:15).eq.'.gcf')) then
c          write(*,*) ' AFAD file, use station definition from filename'
          ndef_chan=1
          net_code='AFAD'
          i=index(infile,'_')
          def_in_stat(1)=infile(i+1:i+4)
          def_out_stat(1)=infile(1:i-1)
          def_in_comp(1)=' '
          def_out_comp(1)=' '
          defread=.false.
          afad=.true.
      endif
      if (afad) then
        call casefold(def_out_stat)
      endif
c
c   get def file for station codes, give file name
c
      if (.not.defread.and..not.afad) then
        text='gursei.def'
        call read_def_chan(text,mainhead_text,net_code)
        defread=.true.
      endif
          
c
c  open file 
c
       open(1,file=infile,status='old',access='direct',recl=1024)
c
c   read header block to get start time etc
c
c
       read(1,rec=1,err=99) block

c
c   number of channels, not given in input file, assume 1
c
       nchan=1
c
c   get file start time etc
c
       call read_guralp_head(block,year(1),month(1),day(1),hour(1),
     *    min(1),sec(1),rate(1),nrecord,comp_code)

       write(6,*) 'ymdhms',year(1),month(1),day(1),hour(1),min(1),sec(1)
       write(6,*) 'rate,nr,for',rate(1),nrecord,comp_code
c
c   get system id and stream id
c
       call get_36_string(x4(1),system_id)
       call get_36_string(x4(2),stream_id)
       write(6,*) 'System and stream id ',system_id,'  ',stream_id
c
c---------------------------------------------------------------
c   read file and decode numbers, one block at a time
c---------------------------------------------------------------
c
      i=1
      nsmp=1
c
c   back here after reading and decoding one block
c
 30   continue
c
      read(1,rec=i,err=50) x4
c
c  read header
c
      call read_guralp_head(block,yr,mo,da,hr,
     *    mn,se,ra,nrecord,comp_code)

c
c   this could be a status block indicated by sample rate zero, if so skip it
c
      if(ra.eq.0.0) then
          i=i+1
          goto 30
      endif

c   Check for missing data between previous block and this one.
c   If there is a gap then fill space with 0s                          (RL 03/2006)
c      start_time = (((hr*60)+mn)*60)+se
      call timsec(yr,mo,da,hr,mn,se,start_time)
      if (nsmp .ne. 1 .and. start_time .ne. prev_end_time) then
          nsamp_block = (start_time - prev_end_time) * ra
          do j=1,nsamp_block
              data(nsmp)=0
              nsmp=nsmp+1
          enddo	  
      endif
	 
c
c   samples in block
c
      nsamp_block=comp_code*nrecord
c
c   first sample in block, possibly swap
c
      if(pc.or.linux) call swap4_one(x4(5))
c
c   save last sample in block, possibly swap
c
      xlast=x4(nrecord+6)
      if(pc.or.linux) call swap4_one(xlast)
c
c   put into output data vector
c
      data(nsmp)=x4(5)
      nsmp=nsmp+1
c
c   get remaining compressed values and  find differences
c   depending on compression
c
c
c   one byte data
c
      if(comp_code.eq.4) then
         do j=2,nsamp_block
            data(nsmp)=x1(j+20)+data(nsmp-1)
            nsmp=nsmp+1
         enddo
      endif
c
c   two byte data
c
      if(comp_code.eq.2) then
         if(pc.or.linux) call swap2(512,x2)
         do j=2,nsamp_block
           data(nsmp)=x2(j+10)+data(nsmp-1)
           nsmp=nsmp+1
         enddo
      endif
      
c
c   4 byte data
c
      if(comp_code.eq.1) then
         if(pc.or.linux) call swap4(256,x4)
         do j=2,nsamp_block
           if(x4(j+5)+data(nsmp-1).gt.max24) then  ! check 24 bit overflow
              sample=x4(j+5)+data(nsmp-1)         ! save sample
              if(sun) then
                 csample(1:1)=char(255)            ! add sign
              else
                 csample(4:4)=char(255)
              endif
              data(nsmp)=sample
           else
              data(nsmp)=data(nsmp-1)+x4(j+5)
           endif
           nsmp=nsmp+1
         enddo
      endif
c
c   check that last value is correct
c
      if(data(nsmp-1).ne.xlast) then
        write(6,*)' Something wrong with decompression'
        write(6,*)' Block #', i
        write(6,*) ' Last decompressed ',data(nsmp-1)
        write(6,*) ' Last correct value ',xlast
        stop
      endif

c
c   Remember time of end of block so can check for missing data
c
      prev_end_time = start_time + nsamp_block/ra

c
c   go back to read next block
c
      i=i+1   

      goto 30
c
c------------------------------------------------------
c  all data decoded
c------------------------------------------------------
c
 50   continue
      close(1)
      nsamp(1)=nsmp-1    ! LO should be -1, dec 2012, commenting line below
c      nsamp(1) = nsmp         ! RL
      write(6,*) 'Number of samples', nsmp
      cbyte(1)='4'            ! always assume 4 byte data
      stat(1)=stream_id(1:4)//' '
c
c   default station and channel names
c
c      comp(1)='B  Z'
      comp(1)='BH Z'
      comp(1)(4:4)=stream_id(5:5)
c
c change station or component names according to def file
c
c      write(*,*)' debug ',ndef_chan,def_in_stat(1),def_out_stat(1)
      call set_def_chan(1,stat(1),comp(1))
c
c   make seisan headers
c
      ichan=1
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   fix name so station and component are added
c
      outfile(30:30)='_'
      outfile(31:35)=stat(1)
      outfile(36:36)='_'
      outfile(37:40)=comp(1)
      do i=31,40
         if(outfile(i:i).eq.' ') outfile(i:i)='_'
      enddo        
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
C   enter channel  loop, for the moment one
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
c         write(chahead(37:43),'(f7.3)') rate(1)
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
         write(2) (data(i),i=1,nsmp)
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
