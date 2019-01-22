c   Program to change magnitude type in NORDIC format type 1 lines
c
c   For e.g. PDE events the magnityde type is changed from
c   SPDE and BPDE to sPDE and bPDE
c
C   The change is need so that the magnityde type follows the 
c   IASPEI standard.
c
c   The IASPEI standard amplitudes for 
c   M_S(BB), M_B,   M_S(20) and  M_b,  in Seisan becomes :
c   SPDE,    BPDE,  sPDE    and  bPDE, respectively.
c
c   Where
c   M_S(BB) is the new broad band surface wave magnitude
c   M_B     is the new broad band P-wave magnitude
c   M_S(20) is the old WWSSNLP surface wave magnitude
c   M_b     is the old WWSSNSP P-wave magnitude
c
c   read more at
c   http://www.isc.ac.uk/doc/analysis/2006p03/magletter.html
c
c   2009-12-24 pv : first version - good luck
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

c-- input indicator
       character*1      inpt            
c-- true if mag type is changed
       logical          changemag


c
c print version
c
      include 'version.inc'
      out_version_date='DEC 24, 2009'
      if (version_new) out_version_date=version_date
      call print_ver
c
       start_time = ' '
c
c  ---------------------------------------------------------------------   
c are you sure
      write(6,*)'  '                               
      write(6,*)' This program will change all header lines (type 1),'                               
      write(6,*)' so that the magnitude types follow the IASPEI', 
     &' standard.'
      write(6,*)' E.g. PDE magnitudes like SPDE and BPDE will be '                               
      write(6,*)' changed to sPDE and bPDE, respectively.'                               
      write(6,*)'  '                               
      write(6,*)' Read more at : '
      write(6,*)'  ',
     &'http://www.isc.ac.uk/doc/analysis/2006p03/magletter.html'
      write(6,*)'  '                               
      write(6,*)' Are you sure that you wish to continue (Y/N=default)'                          
          read(5,'(a)') outf                       
      if(outf.NE."Y") STOP "must be big Y"
      write(6,*)'  '                               
                           
c                                             
c  ---------------------------------------------------------------------   
c main input :
c                                                                                
      write(6,*)' Input is :'
      write(6,*)'          Database   (1)'
      write(6,*)'          index file (2)'
      write(6,*)'          S-file     (3) ',
     &'(also collect.out or select.out)'
      write(6,*)' Enter choise :'
      read(5,'(a)') inpt
      if(inpt.ne."1".and.inpt.ne."2".and.inpt.ne."3") then
          STOP "Wrong choice"
      endif

c  ---------------------------------------------------------------------   
c single file input :
c
      if(inpt.EQ."3") then
       write(6,*)' Enter file name :'
       read(5,'(a40)') evfile

c   open input file                                                            
            call sei open(old$+warn$,            ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    evfile,                ! File name
     &                    read01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
c                                                                               
c   open output file                                                            
c                                                                               
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    'change_mag.out',      ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.

 100  read(read01,'(a80)', END=101) data(1)      ! Read record.
      changemag=.FALSE.
      if(data(1)(80:80).eq."1") then
        if(data(1)(60:60).eq."B") changemag=.TRUE.
        if(data(1)(68:68).eq."B") changemag=.TRUE.
        if(data(1)(76:76).eq."B") changemag=.TRUE.
        if(data(1)(60:60).eq."S") changemag=.TRUE.
        if(data(1)(68:68).eq."S") changemag=.TRUE.
        if(data(1)(76:76).eq."S") changemag=.TRUE.
c
        if(data(1)(60:60).eq."B")data(1)(60:60)="b"
        if(data(1)(68:68).eq."B")data(1)(68:68)="b"
        if(data(1)(76:76).eq."B")data(1)(76:76)="b"
        if(data(1)(60:60).eq."S")data(1)(60:60)="s"
        if(data(1)(68:68).eq."S")data(1)(68:68)="s"
        if(data(1)(76:76).eq."S")data(1)(76:76)="s"
        if(changemag) write(6,'(a80)',iostat=code)data(1)
      endif
      write(write01,'(a80)',iostat=code)data(1)
      goto 100
 101  continue

      call sei close (close$,write01,code)

      write(6,*)                                                                
      write(6,200)                                                       
 200  format(' Output file is change-mag.out  ')                                         
c  ---------------------------------------------------------------------   
c data base input :
c
      else
       write(6,*)'  '                               
       write(6,*)' Remember to make a backup of the database/file,'                          
       write(6,*)' and that you must have read and write permission.'                          
       write(6,*)'  '                               
       write(6,*)' This program will change your database.'                         
       write(6,*)' Are you absolutely sure (Y/N=default) '                          
           read(5,'(a)') outf                       
       if(outf.NE."Y") STOP "must be big Y"
       write(6,*)'  '                               
c                                             
c   input base name and time interval                                      
c                                                                                
       write(6,*)' Enter Data Base name or index file :'
c
       read(5,'(a40)') base_name                                                
       write(6,'(1x,a,$)')' Start time                       : '                      
       read(5,'(a14)') start_time                                               
       write(6,'(1x,a,$)')' End time, return for end of month: '                           
       read(5,'(a14)') end_time                                                 
 1     continue                                                                 
	                                                                    
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
         inquire(file=evfile,exist=exist)                         
         if (exist) then                                          
c
            call sei open(old$+warn$,            ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    evfile,                ! File name
     &                    write01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
       call indata(write01,nstat,nphase,nhead,nrecord,type,exp,data,id)
c        call sei close (close$,read01,code)

c          write(6,'(i5,1x,a79)') event_no,evfile(1:79)
c      write(6,*)nrecord

         do i=1,nrecord
           changemag=.FALSE.
           if(data(i)(80:80).eq."1") then
             if(data(i)(60:60).eq."B") changemag=.TRUE.
             if(data(i)(68:68).eq."B") changemag=.TRUE.
             if(data(i)(76:76).eq."B") changemag=.TRUE.
             if(data(i)(60:60).eq."S") changemag=.TRUE.
             if(data(i)(68:68).eq."S") changemag=.TRUE.
             if(data(i)(76:76).eq."S") changemag=.TRUE.
c            write(6,'(a80)',iostat=code),data(i)
             if(data(i)(60:60).eq."B")data(i)(60:60)="b"
             if(data(i)(68:68).eq."B")data(i)(68:68)="b"
             if(data(i)(76:76).eq."B")data(i)(76:76)="b"
             if(data(i)(60:60).eq."S")data(i)(60:60)="s"
             if(data(i)(68:68).eq."S")data(i)(68:68)="s"
             if(data(i)(76:76).eq."S")data(i)(76:76)="s"
             if(changemag) write(6,'(a80)',iostat=code)data(i)
           endif
         enddo
c         write(6,'(a80)',iostat=code)
c    *          (data(i),i=1,nrecord)
c
                                                                               
c                                                                               
c   open output file                                                            
c                                                                               
c           call sei open(unknown$+warn$,        ! Open a unknown status file.
c           call sei open(old$+warn$,        ! Open a existing file.
c    &                    ' ',                   ! Prompt file name (n/a).
c    &                    evfile,                ! File name
c    &                    write01,               ! Write unit #1
c    &                    b_old,                 ! Already exists? (n/a).
c    &                    code)                  ! Returned condition.

c                                                                               
c  write event
c                                                                               
         rewind (write01,iostat=code)

c         write(write01,'(a80)',iostat=code)
c    *          (data(i),i=1,nrecord)
c         call sei code(fort$,code,write01,b_eof)

            do i=1,nrecord
               write(write01,'(a)',iostat=code) data(i)
               call sei code(fort$,code,write01,b_eof)
            enddo

         call sei close (close$,write01,code)
c                                                                               
c  print event name                                                             
c                                                                               
           write(6,'(1x,a79)') evfile(1:79)
         else                                                      
           write(6,'(1x,a60,a)') evfile(1:60),' doesn''t exist.'         
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
      write(6,*)''
      if(inpt.EQ."1") write(6,*)' Data Base have been corrected',
     *' in the given time window.'
      if(inpt.EQ."2") 
     *   write(6,*)' S-files in index file have been corrected',
     *' in the given time window.'
        write(6,*)''
        write(6,*)' Checked files are listed above',
     *            ' with the changed type 1 lines     '
        write(6,*)''
        write(6,*)' Run UPD to update the CAT part of the database.'
        write(6,*)''
       endif                                                                    
c                                                                               
      stop                                                                      
      end                                                                       
                                                                                
                                                                                
