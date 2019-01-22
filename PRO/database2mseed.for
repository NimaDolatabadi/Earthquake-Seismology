c
c complete, but test
c
c program to convert waveform files from seisan to miniseed,
c input is seisan database, could be continuous or event database
c
c Lars Ottemoller 29/04/2009
c
c  04 01 2013 jh: nrecord to auto_tr

c steps
c   - get database name, and times
c   - read through s-files
c   - get waveform files
c   - check that they are seisan 
c   - setup component conversion 
c   - convert waveform file
c   - update s-file

      implicit none
      include 'seidim.inc'
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'rea.inc'      
       include 'waveform.inc'      

       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
       integer seiclen
c-- start and end time of select      
       character*14    start_time,end_time 
c-- data base name if not agency (blank)               
       character*40	base_name	
c-- event file name                                   
       character*80	evfile		
c-- data
       character*80     data(max_phase)
c-- select key                                       
       character*10     key		
c-- see subroutine find...       
       integer		status,new_month,fstart,event_no 
c-- counters for events, records etc.      
       integer nr,nd,nl,records,nevent	
c-- true if file exists (CLK)             
       logical          exist           
c-- Arguments passed
       character*80     arg(10)
c---number of arguments and function
      integer nars
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1
       integer          read01
c write unit  #1
       integer          write01,write02
c counters
       integer i,j,n
c file names
       character*160 wavefile
c original filename
       character*80 filename(100),fileout(1000)
c path
       character*160 path
c command
       character*1024 cmd
c original channel number
       integer nchan
c save number of samples
       integer nsamp(100)
       logical flag(100),upd_sfile

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call get_seisan_def

       start_time = ' '
       nars = 0
       call get_arguments(nars,arg)    ! get arguments 
        if(nars .ne. 0) then
         end_time = ' '
         base_name = ' '
         do i = 1,nars
            if(arg(i)(1:11) .eq. '-start_time')then
                read(arg(i+1),'(a14)') start_time
            endif
            if(arg(i)(1:9) .eq. '-end_time')then
              read(arg(i+1),'(a14)') end_time
            endif
            if(arg(i)(1:10).eq. '-base_name')then
              read(arg(i+1),'(a40)') base_name
            endif
            if(arg(i)(1:2).eq.'-h')then
              write(*,*)'    Command line input:'
              write(*,*)'-start_time yymmdd...'
              write(*,*)'-end_time yyyymmdd...(blank=end of month)'
              write(*,*)'-base_name XXX (blank=default)'
              stop
            endif
         enddo
       endif
       if(start_time .ne. ' ') go to 50 
c                                             
c   input base name and time interval                                      
c                                                                               
       write(6,*)' Base name, ,, for local directory, ',
     *'name of index file'
       write(6,*)' or return for default base'                               
 
       read(5,'(a40)') base_name                                                
       write(6,'(1x,a,$)')' Start time                       : '
       read(5,'(a14)') start_time                                               
       write(6,'(1x,a,$)')' End time, return for end of month: '
       read(5,'(a14)') end_time                                                 
 1     continue                                                                 
	                                                                    
c                                                                               
c  reset counters                                                               
c
                                                                               
 50    nl=0                                                                     
       nr=0                                                                     
       nd=0                                                                     
       nevent=0                                                                 
       records=0                                                                
c                                                                               
c   open output file                                                            
c                                                                               
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    'database2mseed.out',  ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.

c                                                                               
c  start read and write loop                                                    
c                                                                               
 5     continue                                                                 
c-- always use next event                                
       key='          '		
       CALL findevin                                                       
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile,fstart,new_month,status)                                 
       if(status.eq.0) then                                                     
c--   (CLK)     
         inquire(file=evfile,exist=exist)                         
c--   (CLK)     
         if (exist) then                                          
           call sei open(old$+warn$,            ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    evfile,                ! File name
     &                    read01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
           call rea_event_in(read01,.true.,data,code)
           nevent=nevent+1                                                      
c                                                                               
c  write event, only first line if compact desired                              
c                                                                               
           write(*,'(a80)',iostat=code) data(1)
           write(write01,'(a)') evfile(1:seiclen(evfile))
           write(write01,'(a80)',iostat=code) data(1)
           call sei close (close$,read01,code)
c
c   get trace data file names
c
           call auto_tr(data,rea_nhead,rea_nrecord,n,filename)
c
c find waveforms and check if seisan
c
           upd_sfile=.false.
           do i=1,n
             flag(i)=.true.
             call get_full_wav_name(filename(i),wavefile)
             wav_filename(i)=wavefile
             n=seiclen(wavefile)-seiclen(filename(i)) 
             path=wavefile(1:n)
             fileout(i)=filename(i)
             if (wavefile.ne.' ') then
               write(*,*) ' path ',path
               write(*,*) ' file ',wavefile
               write(*,*) ' read header ',wav_filename(i)
               call wav_init
               call read_wav_header(i)
c check that SEISAN format and S in filename
               if (wav_file_format(i).eq.'SEISAN'.and.
     &             (filename(i)(19:19).eq.'S'.or.
     &              filename(i)(19:19).eq.'R')) then
                 write(write01,'(a)') 'SEISAN format: ' //
     &              wav_filename(i)(1:seiclen(wav_filename(i)))
                 fileout(i)(19:19)='M'
                 cmd = '  '
                 cmd='seisan2mseed -o '//path(1:seiclen(path))//
     &                fileout(i)(1:seiclen(fileout(i)))
c build component codes
                 nchan=wav_nchan
                 do j=1,wav_nchan
                   nsamp(j)=wav_nsamp(j) 
                   write(cmd(seiclen(cmd)+1:),'(a)') ' -T "'//
     &                 wav_comp(j)(1:2)//wav_comp(j)(4:4)//
     &                 ' ='//wav_comp(j)(1:2)//wav_comp(j)(4:4)//'"'
                 enddo 
                 write(cmd(seiclen(cmd)+2:),'(a)')
     &               ' '//wav_filename(i)(1:seiclen(wav_filename(i)))
                 write(write01,'(a)') cmd(1:seiclen(cmd))
c convert
                 call systemc(cmd,seiclen(cmd))
c check miniseed file
                 wav_filename(i)=path(1:seiclen(path))//
     &                fileout(i)(1:seiclen(fileout(i)))
                 call wav_init
                 call read_wav_header(i)
                 if (wav_nchan.ne.nchan) then
                   flag(i)=.false.
                   write(write01,*) ' nchan ',nchan,wav_nchan
                 endif
                 do j=1,wav_nchan
                   if (nsamp(j).ne.wav_nsamp(j)) then
                     flag(i)=.false. 
                     write(write01,*) ' chan ',j,' samples ',nsamp(j),
     &                  wav_nsamp(j) 
                   endif
                 enddo
                 if (flag(i)) then
                   upd_sfile=.true.
                   write(write01,'(a)') ' conversion complete '//
     &                  fileout(i)(1:seiclen(fileout(i)))
c remove seisan file
                   cmd = ' '
                   write(cmd,'(a)') 'rm '//
     &                 wavefile(1:seiclen(wavefile))
                   call systemc(cmd,seiclen(cmd))
                   write(write01,'(a)') cmd(1:seiclen(cmd))
                 else 
                   write(write01,'(a)') ' error in conversion '//
     &                  filename(i)(1:seiclen(filename(i)))
                 endif

               elseif (wav_file_format(i).eq.'SEISAN') then
                 write(write01,'(a)') 'not SEISAN filename: ' //
     &              wav_filename(i)(1:seiclen(wav_filename(i)))
               else
                 write(write01,'(a)') 'not SEISAN format: ' //
     &              wav_filename(i)(1:seiclen(wav_filename(i)))
               endif 
             endif
           enddo
c
c update s-file
c
           if (upd_sfile) then
               do j=1,rea_nwav
                 write(*,'(a)') rea_wav(j)
                 if (flag(j)) then
                   write(rea_wav(j)(2:),'(a)') 
     &               fileout(j)(1:seiclen(fileout(j)))
                   write(rea_wav(j)(80:80),'(a1)') '6'
                 endif
                 write(*,'(a)') rea_wav(j)
               enddo
c
c overwrite s-file
c
               call sei open(old$+warn$,           ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    evfile,                ! File name
     &                    write02,               ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
               call rea_event_out(write02,.true.,data,code)
               call sei close (close$,write02,code)
           endif

c-- (CLK)
         else                                                      
c-- (CLK)      
           write(6,'(1x,a60,a)') evfile(1:60),' doesn''t exist.'         
c-- (CLK)      
         endif                                                     
c-- back for next event                                         
         write(write01,*)
         goto 5			
       else                                                                     
c-- 3 is end of time period                       
          if(status.eq.2) write(6,*)' End of index file'
          if(status.gt.3)       
     *    write(6,*)' ****** Something wrong, status= ', status                 
       endif                                                                    
c                                                                               
c  print out statistics                                                         
c                                                                               
      write(6,*)                                                                
      write(6,200)                                                       
 200  format(' Output file is database2mseed.out  ')                                         
      write(6,*)                                                                
      write(6,'(a,i7)') ' Total number of events          ',nevent
      call sei close (close$,write01,code)
      stop                                                                      
      end                                                                       
                                                                                
                                                                                
