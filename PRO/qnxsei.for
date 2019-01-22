c$debug
c                                                                               
c    transform seislog qnx files to seisan data files               
c                                                                               
c    j. havskov                       
c
c    updates:
c    nov 19 97 jh : spike filter
c    nov 30       : set byte order to pc since not given in qnx file
c    jan 22 98    : new sys_resp, no save_head from now
c    sep 15 98    : indicate clock status in output file
c    dec 18 98    :-------------------- version 7.0 check ------------------
c                  filename, computer type
c    mar 23 99 jh : do not check file fo rmissing blocsks, sample rate etc if
c                   less than 30 blocks
c    jun 7        : spike filter removed, not good, cretes problems with large
c                   events
c    aug 16       : patch to fix problem with two different block header 
c                   formats, one for version before 8.0 and one for 8.0
c    sep 29    jh : check 160 instead of 1040 for nonexistance of cal file
c    feb 2, 2000 lo: changed dimension of y
c    may 11  00 lo: use read_resp_head
c    nov 9   00 lo: ignore when invalid buffer header
c    nov 27  01 lo: increased dimension of y array to 2000000
c    apr 8   02 jh: do not check for missing blocks, did not work if
c                   digitizer had a bad drift
c    jul 10  06 lo: increased dimension of y array to 4000000
c    dec 2007   jh  remove swap routines, are in swap.for
c    may 07  08 lo: increased dimension of y array to 6000000
c
c    Parameter in defines some type of input
c
c    in=0 : One binary file
c    in=1 : Several ------, use filenr.lis as input file name
c                                                                               
c
      implicit none
      include 'seidim.inc'
      external    seiclen                  !JAB(BGS)Mar95. Length
      integer     seiclen                  !JAB(BGS)Mar95. Function.
      character*2 byte_length              ! byte length
      character*3 clock_status             ! is SET if ok
      logical y2k                          ! use with verison 8.0 patch
c                                                                               
c-- seisan  field station file header                
      character*1024	header		
c-- seisan  data file channel header      
      character*1040    chahead         
c-- -------------------main --------      
      character*2000     mainhead        
c-- output file name                                  
      character*80	outfile		          !
c-- save part of header                         
      character*64      block_header              ! 1 sec block header
      integer yy,mo,dd,hh,mm,year                 ! time of  ------
      real    ss
      real*8  total_tim,total_tim_old
      real    time_dif                            ! difference between two headers
      real*8  first_time,last_time                ! time of first and last buffer
      real    total_time_block                    ! file length from block times
      integer nmiss,kmiss                         ! missing blocks
c-- data vector, one buffer or one channel                              
      integer*2		data2(max_sample)	
      integer*4     data4(max_sample)
      integer       error_time                    ! 1: error in time
c      integer*4     y(max_sample)
c      integer*4     y(2000000)
      integer maxy
      parameter (maxy=6000000)
      integer*4     y(maxy)
      integer unit
c-- number of equivalent samples in one buffer and in buffer header, inc header
      integer nsmp_buf,nhead
      integer nsmp_buf_chan   ! number of samples pr block pr chan ex header
c-- input file name
      character*80 infile
c-- top directory
      character*60 top_dir
c-- event number in
      integer number
c-- question
      character*80 question
c-- number of channels and samples/chan, total number of samples           
      integer nchan,nsamp,ntotal
c-- total time window                          
      real              total_time	
c-- block record length                   
      integer           rlength         
c-- number of blocks                              
      integer           nblock
c-- sample rate                                              
      real		rate
      real      dc      ! dc level		
c-- help variables                                     
      integer		i,k,ib,in,j,kk,l

      character*1       dchar
c-- -------------                                    
      integer           ich		
      integer byte_order    ! byte order, machine where written 1: sun, 2:pc,linux
      logical pc,sun,linux


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c set initial file or action, if gt 2, no questions asked 
c
      in=0
      outfile = ' '                       !JAB(BGS)Mar95. Empty filename.
c
c   set computer type
c
      call computer_type(sun,pc,linux)
c
      call dir_char(dchar)
c
c   get top directory
c
      call topdir(top_dir)
      open(10,file='qnxsei.out',status='unknown')
c
c  get file name
c
         question=' Filename, ?, number or filenr.lis for all'
         call filename(question,infile)
c
c  check for no files
c
         if(infile(1:3).eq.'EOF') goto 99
c
c   check which type of input file
c
         if(infile(1:10).eq.'filenr.lis'
     *   .or.infile.eq.'FILENR.LIS') then
            open(8,file='filenr.lis',status='old')
            in=1
         endif
c
c-----------------------------------------------------------------------
c  back here if many binary files converted using filenr.lis
c-----------------------------------------------------------------------
c
 1    continue
      if(in.eq.1) then
         read(8,'(2x,i3,2x,a)') number,infile
         write(6,*)
         write(6,'(1x,i3,3x,a)') number,infile
         if(infile(1:4).eq.'    ') goto 99
      endif
      first_time=0.0
      last_time=0.0
c                                                                               
c  open qnx binary file 
c     
      open(1,file=infile,status='old',access='direct',recl=1024)            
c                                                                               
c   read header 
c                                                                               
      read(1,rec=1) header
c                                                                               
c  make file name 
c             
      outfile=' '      
c
c-- year
c
         outfile(3:4)=header(17:18)
         read(outfile(3:4),'(i2)') i
         if(i.lt.50) then
            outfile(1:2)='20'
         else
            outfile(1:2)='19'
         endif
         outfile(3:4)=header(17:18)
         outfile(5:5)='-'
c-- month
         outfile(6:7)=header(24:25)
         outfile(8:8)='-'
c-- day     
         outfile(9:10)=header(27:28)
         outfile(11:11)='-'
c-- hr      
         outfile(12:13)=header(33:34)
c-- min     
         outfile(14:15)=header(36:37)
         outfile(16:16)='-'
c-- sec
         outfile(17:18)=header(39:40)
         outfile(19:20)='S.'
c-- network or station code
         outfile(21:25)=header(1:5)
         do i=21,25 
            if(outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         outfile(26:27)='_0'
c-- number of channels
         outfile(28:29)=header(63:64)
c
c-- check for blanks                                        
         do i=1,seiclen(outfile)                    !JAB(BGS)Mar95.
            if(outfile(i:i).eq.' ') outfile(i:i)='0'                            
         enddo                                                                  
c
c-- check for blanks                                        
c
            do i=1,29				
               if(outfile(i:i).eq.' ') outfile(i:i)='0'                         
            enddo                                                               
            write(6,207) outfile(1:29)                                          
 207        format(/,' Output file name is: ',a)                              
c
c   read header info
c                                                       
         read(header(81:88),'(i8)',err=384) nsamp
         read(header(63:64),'(i2)',err=384) nchan
         read(header(105:112),'(i8)',err=384)rlength
         read(header(97:98),'(i2)',err=384) unit
         read(header(65:80),'(f16.7)',err=384) rate
         read(header(95:96),'(a2)',err=384)byte_length
         read(header(94:94),'(i1)',err=384) byte_order
c
c  assume written on PC since missing in file
c
         byte_order=2
         rate=1/rate
         goto 385
 384     continue
           write(6,*) 'error in header'
 385     continue
c
c   check rlength size
c
         if(rlength.gt.max_sample*2) then                  !JAB(BGS)Sep95.
            write(6,*)' Too long buffers, rlen= ',rlength
            stop
         endif
c
      write(6,*) 'digitizer',unit,'  byte order',byte_order,
     *'  byte length', byte_length
c
c   calculate number of samples pr buffer inc header and header
c   in samples assuming 4 byte words
c
            nsmp_buf=rlength/4
            nhead=16
            nsmp_buf_chan=(nsmp_buf-nhead)/nchan
c
c   check if not too many samples
c
         if(nsamp.gt.max_sample) then            !JE(BGS)AUG95.
      	    write(6,*)' Too many samples/chan, nsamp= ',nsamp
            stop
         endif			 
         nblock=nsamp*nchan/(nsmp_buf-nhead)                                    
         write(6,*)' nsamp=',nsamp,'  nchan=',nchan,' reclen=',rlength          
         write(6,*)' number of blocks=',nblock                                  
c
c-- total time window assuming no change in rate and no missing buffers
c   is modified later, should also be equal to number of blocks
c
c        total_time=nsamp/rate
         total_time=nblock
c
c   close and reopen input file with record length of data buffers
c		                                                                
            close(1) 
            open(1,file=infile,status='old',
     *      access='direct',recl=rlength)   
c
c   save time status
c
                   read(1,rec=2+1024/rlength) (data4(i),i=1,nsmp_buf)
                   write(block_header,'(16a4)')(data4(i),i=1,16)
c
c   find if before y2k format or not
c
                   read(block_header(20:21),'(i2)') i
                   y2k=.false.
                   if(i.eq.20.or.i.eq.19) y2k=.true.
                   if(y2k) then
                      clock_status=block_header(29:31)
                   else
                      clock_status=block_header(27:29)
                   endif
         write(6,'(a,a)') ' Clock status ',clock_status
c
c  read first 10 and last 10 buffers to check time if at least 30 buffers
c------------------------------------------------------------------------
c
         if(nblock.gt.30) then
         do ib=2,nblock+1           
             if((ib.gt.1.and.ib.le.11).or.
     *          ib.gt.(nblock-9)) then
                   read(1,rec=ib+1024/rlength) (data4(i),i=1,nsmp_buf)
                   write(block_header,'(16a4)')(data4(i),i=1,16)
c                   write(6,'(1x,a)') block_header(1:45)
                   if(y2k) then
                      read(block_header(1:23),
     *           '(i2,1x,i2,1x,f6.3,1x,i2,1x,i2,3x,i2)',err=9121)
     *                hh,mm,ss,dd,mo,yy
                   else
                      read(block_header(1:21),
     *                '(i2,1x,i2,1x,f6.3,1x,i2,1x,i2,1x,i2)')
     *                hh,mm,ss,dd,mo,yy
                   endif
                   if(yy.lt.50) then 
                      yy=yy+2000
                   else
                      yy=yy+1900
                   endif
                   call timsec(yy,mo,dd,hh,mm,ss,total_tim)
c
c   save average time in 10 first and last blocks
c
                   if(ib.gt.1.and.ib.le.11) then
                      first_time=first_time + total_tim 
                   endif
                   if(ib.gt.(nblock-9)) then
                      last_time=last_time + total_tim
                   endif
                   time_dif=total_tim-total_tim_old
             endif
          enddo
c
c   calculate total time from block times and compare, see how many
c   blocks are missing
c
         total_time_block=last_time/10.0-first_time/10.0+10.0
         write(6,'(a,f8.3)') ' Total time from block times     ',
     *   total_time_block
         write(6,'(a,f8.3)') ' Total time from header          ',
     *   total_time
         nmiss=(total_time_block-total_time+0.2)   ! 0.2 allow for error
c
c   normally blocks are missed so nmiss positive, if negative means
c   that clock has jumped back in time
c
         if(nmiss.lt.0) then
            write(6,*)' Blocks missed: ',nmiss
c           write(6,*)' Serious timing problem, continue (y/n) ?'
c           read(5,'(a)') answer
c           if(answer.eq.'y'.or.answer.eq.'Y') then
c              nmiss=0
c           else
c             stop
c           endif
         endif  
c
         if(nmiss.gt.0) then
            write(6,*) ' Blocks missed in file: ',nmiss
            write(10,*)' Blocks missed in file: ',nmiss
         endif
c
c   ignore crazy errors
c
         error_time=0
         if(nmiss.lt.0.or.nmiss.gt.500) then
            nmiss=0
            error_time=1
         endif

9121     continue   ! if error
c
c   calculate new sample rate and total time if no crazy errors
c
         if(error_time.ne.1) then
c  8-4-02   rate=(nsamp+nmiss*nsmp_buf_chan)/
            rate=(nsamp)/
     *      (last_time/10.0-first_time/10.0+10.0)
            write(6,'(a,f8.3)') ' Sample rate based on block times',rate
c  8-4-02   total_time=((nblock+nmiss)*nsmp_buf_chan)/rate
c  8-4-02   nsamp=nsamp+nsmp_buf_chan*nmiss
            total_time=((nblock)*nsmp_buf_chan)/rate
            nsamp=nsamp
         else
            rate=nsmp_buf_chan
         endif
         endif         !   end if more than 30 blocks
c
c   construct  main header
c
         do i=1,2000                                                            
           mainhead(i:i)=' '                                                    
         enddo                                                                  
         mainhead(2:18)='                  '                              
c-- no of channels                        
         mainhead(32:33)=header(63:64)
c-- year, date and time
         mainhead(35:46)=header(17:28)
c
c   fix century
c
         read(mainhead(35:36),'(i2)') year
         if(year.lt.50) then
            mainhead(34:34)='1'
         endif
         mainhead(48:59)=header(33:44)
         write(mainhead(61:69),'(f9.3)') total_time
         j=162
         k=161
         do i=1,nchan                                                           
c--  station and component              
            mainhead(j:j+7)=header(k:k+7)	
            mainhead(j+8:j+8)=' '                                               
c-- rel. start time of channel           
            mainhead(j+9:j+15)='    0.0'	
            write(mainhead(j+17:j+24),'(f8.2)') total_time                      
            if(i.eq.3.or.i.eq.6.or.i.eq.9.or.i.eq.12.or.i.eq.15.                
     *      or.i.eq.18.or.i.eq.21.or.i.eq.24.or.i.eq.27.or.i.eq.30.
     *      or.i.eq.33.or.i.eq.36.or.i.eq.39.or.i.eq.42.or.i.eq.45.
     *      or.i.eq.48.or.i.eq.51.or.i.eq.54.or.i.eq.57.or.i.eq.60.
     *      or.i.eq.63) then        
              j=j+28                                                            
            else                                                                
              j=j+26                                                            
            endif                                                               
            k=k+8
         enddo                                                                  
c
         open(2,file=outfile,
     *   status='unknown',form='unformatted')        
         write(6,'(1x,a,a)')'Output file: ',outfile
c                                                                               
c   write  main header                                       
c                                                                               
         k=1
         write(6,'(a80)') mainhead(1:80)                                                                    
         do i=1,12                                                              
            write(2) mainhead(k:k+79)                                           
            write(6,'(a80)') mainhead(k:k+79)	
            k=k+80                                                              
         enddo                                                                  
c
c   write remaining main header lines if more than 30
c
         if(nchan.gt.30) then
            kk=(nchan-31)/3+1
            do i=13,kk+12
               write(2)mainhead(k:k+79)
               k=k+80
            enddo
         endif
c                                                                               
c   start channel read loop
c
         j=0                                                                    
         do ib=2,nblock+1                                 
c                                                                               
c   read one buffer
c   make sure first data buffer is read
c   at correct place, which is always beyound the 1024 bytes,
c   see manual for format ( fixed below by 1024/rlength),
c   note: all data is transferred to 4 byte array
c                                                                               
           read(1,rec=ib+1024/rlength) (data4(i),i=1,nsmp_buf)
             write(block_header,'(16a4)')(data4(i),i=1,16)
             read(block_header(1:21),
     *       '(i2,1x,i2,1x,f6.3,1x,i2,1x,i2,1x,i2)',err=1311)
     *       hh,mm,ss,dd,mo,yy
             if(yy.lt.50) then
                yy=yy+2000
             else
                yy=yy+1900
             endif
             call timsec(yy,mo,dd,hh,mm,ss,total_tim)
             time_dif=total_tim-total_tim_old
             if(abs(time_dif-1).gt.0.7.and.ib.gt.2) then
                write(10,'(1x,a)') block_header(1:45)
                write(10,*)' Too large time difference between buffers'
                write(10,'(a,f6.2)')' Difference is ',time_dif
                kmiss=time_dif-1+0.3
c
c   ignore crazy errors
c
                if(abs(kmiss).gt.100) kmiss=0
c
                if(kmiss.lt.0) then
c                  write(6,*)' Bad time error, clock goes back in time'
c                  write(6,*)' Continue (y/n)'
c                  read(5,'(a)') answer
c                  if(answer.eq.'y'.or.answer.eq.'Y') then
c                     kmiss=0
c                  else
c                     stop
c                  endif
                 endif
             endif
             total_tim_old=total_tim
             goto 1312 
c
c jump here if error
c
 1311       continue
            write(*,'(a)') ' error in buffer header (1:21)'
c            write(*,'(a)') block_header(1:21)
 1312       continue
c                                                                               
c  remove block buffers and save data in one array  
c                                          
             do k=nhead+1,nsmp_buf
               j=j+1
               if (j.le.maxy) y(j)=data4(k)
             enddo
             ntotal=j
c
c   if missing data, put in here, only if detected for whole
c   file, so a temporary time jump will not put in extra data
c   put in data from previous block to avoid dc jumps
c
c 09-04-02   if(kmiss.gt.0.and.nmiss.gt.0) then
c 09-04-02      write(10,*)' Extra blocks put in:',kmiss
c 09-04-02      do l=1,kmiss
c 09-04-02         do k=nhead+1,nsmp_buf
c 09-04-02            j=j+1
c 09-04-02            y(j)=data4(k)      
c 09-04-02         enddo
c 09-04-02      enddo
c 09-04-02      ntotal=j
c 09-04-02      kmiss=0
c 09-04-02    endif
c
c   end of channel loop
c                                                               
         enddo
         write(6,*)
     * ' Input file read, now follows headers in output file'
c                                                                               
c  end read for one event
c                                                                               
c                                                                               
c  write each channel with channel headers
c                                                                               
         do ich=1,nchan                                                         
            do i=1,1040                                                         
               chahead(i:i)=' '                                                 
            enddo                                                               
            j=(ich-1)*8+161
c-- station code                          
            chahead(1:4)=header(j:j+3)		
c-- componnet
            chahead(6:9)=header(j+4:j+7)	
c-- year, date and time                  
            chahead(10:35)=mainhead(34:59)		
            if(rate.ge.100.0) write(chahead(37:43),'(f7.2)') rate 
            if(rate.lt.100.0) write(chahead(37:43),'(f7.4)') rate 
            if(rate.lt.10.0)  write(chahead(37:43),'(f7.5)') rate 
            write(chahead(45:50),'(i6)') nsamp                                  
c                                                                               
c   get response, save date etc since removed when calling read_resp            
c                                                                               
             call read_resp_head(chahead)                                            
c
c   indicate 2 or 4 byte integer in header
c
            if(byte_length.eq.' 4') chahead(77:77)='4'
c
c   indicate timing accuracy
c
            if(clock_status.ne.'SET') chahead(29:29)='E'
c                                                                               
c   write header                                                                
c                                                                               
            write(2)chahead                                                     
            write(6,'(1x,a70)') chahead(1:70)
c
c   check if response curve
c
            if(chahead(160:160).eq.'9') write(6,*)
     *        ' No response file for this channel --------'
c                                                                            
c  demultiplex data  
c
               j=0
               do k=ich,ntotal,nchan
                  j=j+1
                  if(byte_length.ne.' 4') then
                     data2(j)=y(k)
                  else
                     data4(j)=y(k)
                  endif
               enddo
               if(byte_length.ne.' 4') then
                  if(byte_order.eq.2.and.sun) 
     *            call swap2(nsamp,data2)
                  write(2)(data2(k),k=1,nsamp)
               else
                  if(byte_order.eq.2.and.sun) 
     *            call swap4(nsamp,data4)
c
c   check for spikes, assume only in 4 byte data
c
                  dc=0
                  do i=1,nsamp
                         dc=dc+data4(i)
                  enddo
                  dc=dc/nsamp
c
c   put in dc at missing points
c
c                 do i=1,nsamp
c                     if(data4(i).eq.0)then
c                        data4(i)=dc
c                     endif
c                 enddo
                  i=1
c                 do while (i .le. nsamp - 100) ! assume only in 100 samples
c                   if(iabs(data4(i+1)-data4(i)).gt.10000) then
c                     write(6,*)' spike'
c                     write(17,*) i,data4(i),data4(i+1)
c                     do l=1,100    
c                        data4(l+i)=dc
c                     enddo
c                     i=i+101
c                   else
c                     i=i+1
c                   endif
c                 enddo

                  write(2)(data4(k),k=1,nsamp)
               endif
         enddo
         close(2,err=765)                       
         goto 766
 765     continue
         write(6,*) ' Error close 2'
 766     continue
c
c  close to be able to open next file if more than 1
c                                                                  
         close(1)
c                                                                               
c   back for next event if a list of events are transformed
c   or an ascii input file is used
c              
      if(in.eq.1) goto 1          
c                                                                               
c   end of data                                                                 
c                                                                               
 99   continue                                                                  
      stop                                                                      
      end
