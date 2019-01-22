c   program to collect event files to one file from
c   standard data base or individual base                              
c   wicount eventn  events and records                                   
c                                                                      
c   jens havskov, dec 88                                             
c                                                                  
c   uppdates                                                      
c   26.May 89 by Leif Kvamme. Testing file existence. (CLK)                
c   may 29  by j.h.   : status messages about errors                            
c   sep 19, 89 by j.h : increase filename to 60 chars                     
c   Aug 23, 90 by K.Atakan : add compact file                            
c   nov 5   90 by j.h.     : fix output file name,                      
c   nov 12  90 by Erik Hjortenberg : adjust to UNIX
c   oct 17  91 jh : 12 char time string
c   nov 26        : new nordic format, new indata
c   May 13  92 cl : Command line input facilitated
c   jul 8   93 jh : VERSION 3.0 ***************
c   aug 23        : filenames to 80
c   aug 25        : ajust to pc
c   jul 26 94     : small comment change
c   nov    94     :  *******************   verson 5.0 ********************
c   nov 24, 1994  : opening files changed
c   jan 11     95  : new find ev in
c   jan 31        : change quesiton
c   jun 95        : put in global dimensions !!!!!
c  
c   jan 98  jh    : Year 2000 fix ***************************************
c   sep  17 98    : ---------------- verison 7.0 check ------------------
c                   local data base to ,,, longer file name print out
c   nov 02        : recompiled to include new findevin and indata
c   sep 16 05 jh  : no longer clean out norsar amplitudes
c 2010-05-04 pv   : changed some tab-formatted source line that gave warnings in f90
c 2010-10-06 jh   : default time limitys for base ,,
c 2015 08 22 jh   : ---------------------- now to 2020
C
      implicit none
      include 'seidim.inc'
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c-- event data
       character*80	data(max_data)	
c-- exp indicator                                         
       character*1	exp		
c-- event type                                      
       character*1      type		
c-- output indicator for comp. or norm.	  
       character*1      outf            
c-- number of recors for event                           
       integer		nrecord		
c-- number of header lines                                 
       integer		nhead		
c-- number of stations                                     
       integer		nstat		
c-- number of phases
       integer nphase
c-- start and end time of select      
       character*14    start_time,end_time 
c-- data base name if not agency (blank)               
       character*40	base_name	
c-- event file name                                   
       character*80	evfile		
c-- select key                                       
       character*10     key		
c-- see subroutine find...       
       integer		status,new_month,fstart,event_no 
c-- counters for events, records etc.      
       integer nr,nd,nl,records,nevent	
c-- counter                                                   
       integer 		i		
c-- true if file exists (CLK)             
       logical          exist           
c-- Arguments passed
       character*80     arg(10)
c-- id line number
       integer id
c---number of arguments and function
      integer nars
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1
       integer          read01
c write unit  #1
       integer          write01



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver


c
c
C
       start_time = ' '
       nars = 0
       call get_arguments(nars,arg)    ! get arguments 
        if(nars .ne. 0) then
         end_time = ' '
         base_name = ' '
         outf = ' '
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
            if(arg(i)(1:10) .eq. '-compact')then
              outf='y'
            endif
            if(arg(i)(1:2).eq.'-h')then
              write(*,*)'    Command line input:'
              write(*,*)'-start_time yymmdd...'
              write(*,*)'-end_time yyyymmdd...(blank=end of month)'
              write(*,*)'-base_name XXX (blank=default)'
              write(*,*)'-compact y/n'
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
c
       read(5,'(a40)') base_name  
       if(base_name(1:2).eq.',,') then                                              
           write(6,'(1x,a,$)')' Start time, enter is 1980        : '                      
           read(5,'(a14)') start_time
           if(start_time.eq.' ') start_time='1980        '
           write(6,'(1x,a,$)')' End time,  enter for 2020        : '                           
           read(5,'(a14)') end_time
           if(end_time.eq.' ') end_time='2020        '
       else     
           write(6,'(1x,a,$)')' Start time                       : '                      
           read(5,'(a14)') start_time                                             
           write(6,'(1x,a,$)')' End time, return for end of month: '                           
           read(5,'(a14)') end_time
       endif                                                 
 1     continue                                                                 
       write(6,*)' Compact output file (Y/N=default) '                          
	   read(5,'(a)') outf 	                    
	                                                                    
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
     &                    'collect.out',         ! File name
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
         call indata(read01,nstat,nphase,nhead,nrecord,type,exp,data,id)
c
c                                                                               
c   count events etc                                                            
c                                                                               
           if(type.eq.'L') nl=nl+1                                              
           if(type.eq.'R') nr=nr+1                                              
           if(type.eq.'D') nd=nd+1                                              
           records=records+nrecord                                              
           nevent=nevent+1                                                      
c                                                                               
c  write event, only first line if compact desired                              
c                                                                               
          if(outf.eq.'y'.or.outf.eq.'Y') nrecord=1
          write(write01,'(a80)',iostat=code)
     *          (data(i),i=1,nrecord)
          call sei code(fort$,code,write01,b_eof)
c                                                                               
c  print event name                                                             
c                                                                               
           write(6,'(1x,a79)') evfile(1:79)
           call sei close (close$,read01,code)
c-- (CLK)
         else                                                      
c-- (CLK)      
           write(6,'(1x,a60,a)') evfile(1:60),' doesn''t exist.'         
c-- (CLK)      
         endif                                                     
c-- back for next event                                         
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
 200  format(' Output file is collect.out  ')                                         
      write(6,*)                                                                
      write(6,201) nevent,nl,nr,nd,records                                      
 201  format(' Total number of events          ',i7,
     *     /,' Total number local events       ',i7                             
     *     /,' Total number of regional events ',i7                             
     *     /,' Total number of distant events  ',i7                             
     *     /,' Total number of records         ',i7)
      call sei close (close$,write01,code)
      stop                                                                      
      end                                                                       
                                                                                
                                                                                
