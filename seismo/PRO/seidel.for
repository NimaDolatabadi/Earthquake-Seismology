c
c  program to split seisan waveform files
c
c  j. havskov, march 1993
c  Updates:
c  jul 8 93 by jh  : version 3.0
c  jan 2 94        : bug with outfile if same on pc
c  jun 94          : 999 channels, bug when first and secon outfiles same on pc
c  dec 7           : include seidim
C  dec 8        : Installed file & error handling (libsei)
c  dec 13         ************** version 5.0 *************************
c  jul 27 95 jh    : bug with 30 channles
c  aug 21          : still ------------
c  nov 1  95 jh    : new seisinc routine
c  march 8 99      : ----------------   verison 7.0 check ---------------
c                    stat, year 2000
c  jan5 99 jh      : file name wrong for 2000
c  apr  08 lo      : use filenr.lis and use station in filename if all
c                    channels form same station
c  Maynay 05 14 jh    : fix signalx block
c  2015-06-02 pv  : add signal_int to signalx common block due to
c                   compiler warning
c
      implicit none
c
c  dimentions include defs.
c
      include 'seidim.inc'
c-- main header
      character*80 mainhead(max_trace/3+3)
c-- channel header
      character*1040 chahead	  
c-- input file names
      character*80 infile
c-- output file name 
      character*80 outfile,first_outfile
c-- input question
      character*80 question	  	  
c-- station counter
      integer nstat	  
c-- indicator if first or second file to make
      integer file_sel
      character*5 net_code
c-- number of stations and samples
      integer nsamp,nchan,nchan_out	  
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)	  
c-- main header date and time
      integer myear,mmonth,mday,mdoy,mhour,mmin
      real msec	  
c-- channel start time and interval
      real cstart(max_trace)
      double precision cinter(max_trace)
c-- event start time
      double precision ev_time
c-- number of channels to remove
      integer n_remove	  	  
c-- channel number to remove
      integer chan_remove(max_trace)
c-- channels marked for removal
      integer chan_sel(max_trace)
c-- total window length
      real total_window
c-- channel absolute start time relative earliest channel, from  1900, in sec 
      double precision cabstim(max_trace)     	  	  
c-- channel end time absolute
      double precision cendtim(max_trace)
c-- maximum and minumum of above and their difference
      double precision first_time,last_time
c-- data samples
      integer*2 data2(2*max_sample)
      integer*4 data4(max_sample)	  	  
c-- counters
      integer i,j,k,l,ichan
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code,                  ! Error encoder.
     &          sei real num               ! Get real number
C
       integer  write01,                   ! Output unit 1.
     &          read01,                    ! Input unit1.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       real sei real num                   ! Real number
c-- for component rotation, not used in this routine
      real caz,saz
c     real baz(max_trace)
c     real        del_rot       ! delay rotated trace
c     character*1 rot_comp(max_trace)  ! T or R
c     logical rotate
      integer nchan_last                   ! save number of channels
      integer in                           ! 1 for filenr.lis
      logical singleflag                   ! true if all channels from single station
      character*5 singlestat
c
      real x(max_sample)
      integer signal_int(max_sample)
      equivalence (data2,data4)
c      equivalence (data4,x)
c     common /signalx/x
      common /signalx/x,signal_int
C
C    ============= end of list ==========
      include 'version.inc'


c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c      question=' Filename or number'
c
c  get file name
c
      in=0
      nchan_last=0
      question=' Filename, ?, number or filenr.lis for all'
      call filename(question,infile)
c
c   check which type of input file
c
      if(infile(1:10).eq.'filenr.lis'
     *   .or.infile.eq.'FILENR.LIS') then
            open(8,file='filenr.lis',status='old')
            in=1
      endif
      write(6,*)'No of channels to remove'
      read(5,*)n_remove 
      write(6,*)'Channels to remove'
      read(5,*)(chan_remove(i),i=1,n_remove)

10    continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         write(6,*)
         write(6,'(1x,a)') infile
         if(infile(1:4).eq.'    ') goto 99
      endif

      if(infile(1:3).eq.'EOF') goto 99       
          chr_f_access$='direct'
          f_recl$=2048
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   infile,           ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
c
c   now read headers for input event
c	  
      call seisinc                                                             
     *(read01,0,nchan,0,MAINHEAD,chahead,0.0,0.0)                     
c
c check that number of channels is the same
c
      if (nchan_last.ne.0.and.nchan.ne.nchan_last) then
        write(*,*) ' number of channels changed -> exit '
        goto 99
      endif
      nchan_last=nchan
c
c-- date and time of first channel
c
      read(mainhead(1)(34:59),'(i3,5x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3)')
     *myear,mmonth,mday,mhour,mmin,msec 	   	  	 
      myear=myear+1900
      call timsec
     *(myear,mmonth,mday,mhour,mmin,msec,ev_time)
c
c-- get channel codes, start times and durations
c
      nstat=1
      do i=3,(nchan-1)/3+3
         read(mainhead(i),'(3(1x,2a4,a1,f7.2,1x,f8.2))')
     *   stat(nstat)(1:4),comp(nstat),stat(nstat)(5:5),
     *   cstart(nstat),cinter(nstat),
     *   stat(nstat+1)(1:4),comp(nstat+1),stat(nstat+1)(5:5),
     *   cstart(nstat+1),cinter(nstat+1),	 
     *   stat(nstat+2)(1:4),comp(nstat+2),stat(nstat+2)(5:5),
     *   cstart(nstat+2),cinter(nstat+2)	 
         do k=1,3
            l=nstat+k-1		 		 
            cabstim(l)=ev_time+cstart(l)
            cendtim(l)=cabstim(l)+cinter(l)			
         enddo		 
         nstat=nstat+3		   		 
      enddo
      rewind read01	  
c
c--------------------------------------------------------
c   file header read, sort out channels to extract/delete
c--------------------------------------------------------
c
c   mark channels to remove
c	
      do ichan=1,nchan
        chan_sel(ichan)=0  	  	 
        do k=1,n_remove
           if(chan_remove(k).eq.ichan) then
              chan_sel(ichan)=1
           endif
        enddo
      enddo
	  
c---------------------------------------------------------
c   back here after making file without selected channels
c   selected with variable file_sel 0: traces not selected
c                                   1: traces selected
c---------------------------------------------------------
c
      file_sel=0
      first_outfile(1:30)='                              '
c
 1000 continue
	  
c
c   find earliest trace and total time window 
c
      first_time=1.0e20
      last_time=0.0	  
      singleflag=.true.
      singlestat=' '
      do i=1,nchan
         if(chan_sel(i).eq.file_sel) then 
           if(cabstim(i).lt.first_time) first_time=cabstim(i)
           if(cendtim(i).gt.last_time)  last_time=cendtim(i)
           if (singlestat.ne.' '.and.singlestat.ne.stat(i))
     &       singleflag=.false.
           singlestat=stat(i)
         endif
      enddo
      if (.not.singleflag) singlestat=' '
c
c   find new start times relative to new main header
c
      do i=1,nchan
         if(chan_sel(i).eq.file_sel) cstart(i)=cabstim(i)-first_time
      enddo	
c
      total_window=last_time-first_time
c
c   make new main header, clear first
c
      if(nchan.gt.30) then
         k=(nchan-31)/3+13
      else
         k=12
      endif
      do i=1,k
         do j=1,80
            mainhead(i)(j:j)=' '
         enddo
      enddo		 					 	  
      k=0
      do i=1,nchan
        if(chan_sel(i).eq.file_sel) k=k+1
      enddo
      write(mainhead(1)(31:33),'(i3)') k
      nchan_out=k
c-- start time	  
      call sectim(first_time,myear,mdoy,mmonth,mday,mhour,mmin,msec)
      write(mainhead(1)(34:69),'(i3,1x,i3,1x,i2,1x,i2,1x,i2,1x,
     *i2,1x,f6.3,1x,f9.3)') myear-1900,mdoy,mmonth,mday,mhour,mmin,msec,
     *total_window
c
c-- channels, check for channels not used
c
      l=2
      k=3	  
      do j=1,nchan
        if(chan_sel(j).eq.file_sel) then
           write(mainhead(k)(l:l+24),'(2a4,a1,f7.2,1x,f8.2)')	  	 
     *     stat(j)(1:4),comp(j),stat(j)(5:5),cstart(j),cinter(j)   
           l=l+26
           if(l.gt.60) then
              l=2
              k=k+1
           endif
        endif
      enddo		
c
c   determine net code
c
      if (singleflag) then
        net_code=singlestat
      else
        net_code=mainhead(1)(2:6)
      endif
      if(net_code(1:1).eq.' ') net_code='SPLIT'
      do i=1,5
         if(net_code(i:i).eq.' ') net_code(i:i)='_'
      enddo
c                                                                               
c  make file name
c
c    century
c
         if(myear.ge.2000) then
           outfile(1:2)='20'
         else
           outfile(1:2)='19'
         endif
c             
c-- year                          
         outfile(3:5)=mainhead(1)(35:36)//'-'               
c-- month                                    
         outfile(6:7)=mainhead(1)(42:43)		
         outfile(8:8)='-'
c-- day                                      
         outfile(9:10)=mainhead(1)(45:46)		
         outfile(11:11)='-'
c-- hr                                       
         outfile(12:13)=mainhead(1)(48:49)		
c-- min                                    
         outfile(14:15)=mainhead(1)(51:52)		
         outfile(16:16)='-'
c-- sec                                    
         outfile(17:18)=mainhead(1)(54:55)		
         outfile(19:20)='S.'
c-- network or station code                  
         outfile(21:25)=net_code		
         outfile(26:26)='_'
c-- number of channels
         outfile(27:29)=mainhead(1)(31:33)
c-- check for blanks                                        
         do i=1,29				
            if(outfile(i:i).eq.' ') outfile(i:i)='0'                            
         enddo                                                                  
         write(6,200) outfile(1:29)                                             
 200     format(/,' Output file name is: ',a29)                                
c
c   check that outfile name is not the same as the input
c   file or first output file name 
c   in which case the event should not be written out
c   on sun that should not happend often since file name is to nearest
c   second and number of channels is given
c
         if(infile(1:29).eq.outfile(1:29).or.outfile(1:29).eq.
     *      first_outfile(1:29)) then
            write(6,*)'**************************************'
            write(6,*)'Output file already exists, stop '
            write(6,*)'**************************************'
            stop 
          endif
c
c   save filename to be able to compare when second file is written out
c
         if(file_sel.eq.0) first_outfile=outfile
c
c   open file and write main header
c
          chr_f_form$ = 'unformatted'
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
c-- calculate number of main header lines
         if(nchan_out.gt.30) then
            k=(nchan_out-31)/3+13
         else
            k=12
         endif
      do i=1,k
         write(write01)mainhead(i)
         write(6,'(a80)') mainhead(i)		 
      enddo
c
c   channel loop
c 
          call seisinc                                                             
     *    (read01,0,nchan,0,MAINHEAD,chahead,0.0,0.0)                     
c
         do j=1,nchan	
c
c-- write out again if not selected out
c-- put in response in future
c
            if(chan_sel(j).eq.file_sel) then
               call seisinc                                                             
     *         (read01,j,nchan,2,MAINHEAD,chahead,0.0,0.0)                     
               read(chahead(45:50),'(i6)') nsamp
               write(write01) chahead
               write(6,*) ' Channel copied --------------------'
               write(6,'(1x,a)') chahead(1:60)			
               if(chahead(77:77).eq.'4') then
                  do i=1,nsamp
                     data4(i)=x(i)
                  enddo
                  write(write01)(data4(i),i=1,nsamp)
               else
                  do i=1,nsamp
                     data2(i)=x(i)
                  enddo
                  write(write01)(data2(i),i=1,nsamp)
               endif
            else
c	           write(6,*) ' Channel selected out, not copied *****' 
            endif
         enddo
         call sei close(close$,write01,code)
c
c   check if going back and making second file
c
         if(file_sel.eq.0) then
            file_sel=1
            goto 1000
         endif
c
      goto 10
99    continue

      call sei close(close$+all$,read01, code)       ! Close all open files.
      stop
      end	  	  	 

