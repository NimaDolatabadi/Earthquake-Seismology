c$debug
c   program to check S-file data base
c 
c   jens havskov, oct 1999
C   uppdates:
c   oct  29 99 jh : able to do several data bases
c                   automatically find all bases, 
c                   posibility of only finding number of events

        implicit none	
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! File operations.
       include 'seidim.inc'                ! Dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c event data
       character*80	data(max_data)	
c exp indicator
       character*1	exp		
c event type
       character*1      type
c number of recors for event
       integer		nrecord		
c  number of header lines
       integer		nhead		 
c number of stations
       integer		nstat, nphas
c start and end time of select
       character*14     start_time,end_time 
       character*40	base_name(100)	
       character*5      bas(100)      ! real base names
c event file name
       character*80	evfile	
c select key
       character*10     key		
       integer		status,new_month,fstart,event_no 
       integer bad_id,no_id,bad_s   ! error counters 		
       integer 		id		
c logical for existing file or not
       logical          b_old
c returned code      
       integer          code
c units        
       integer          readu,printu, no_id_u,bad_id_u,bad_s_u
       integer nerr                  ! error indicator
       character*1      check        ! kind of check
       character*1      ucase        ! function
       integer ibase, nbase,nb,i
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   open output files
c
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'check_base.out',      ! File name
     &              printu,                ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_no_id.out',     ! File name
     &              no_id_u,                ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_bad_id.out',     ! File name
     &              bad_id_u,              ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_bad_s.out',     ! File name
     &              bad_s_u,               ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
c
c   get possible data bases
c
       call get_base_names(nb,bas)
       write(6,*)' Number of data bases',nb
       write(6,'(13(1x,a5))')(bas(i),i=1,nb)
c
c   input base name and time interval
c
       nbase=1
       write(6,*)
       write(6,*)
     *' Base name(s), * is all, else, one pr line, return for no more'
  50   continue
       read(5,'(a40)') base_name(nbase)
       if(base_name(nbase)(1:1).eq.'*') then
         nbase=nb
         do i=1,nb
           base_name(i)=bas(i)
         enddo
         goto 60
       endif
       if(base_name(nbase).eq.' ') then
          nbase=nbase-1
       else
          nbase=nbase+1
          goto 50
       endif
 60    continue
       write(6,*)' Start time'
       read(5,'(a14)') start_time
       write(6,*)' End time, return for end of month'
       read(5,'(a14)') end_time
       write(6,*)
     *' Check data base (c) or',
     *' only update statistics for program base(u)'
       read(5,'(a)') check
       check=ucase(check)
c
 3     continue
       key='          '		 
       
c
c  reset counters for errors
c
       no_id=0
       bad_id=0
       bad_s=0
c
c  read and check loop
c
       do ibase=1,nbase
 5     continue
       CALL findevin	
     * (base_name(ibase),start_time,end_time,key,0,
     * event_no,evfile,fstart,new_month,status)
c
c  check for end
c
       if(status.ne.0) go to 99	 
       if(check.eq.'U') goto 5
c
       call sei open(old$,                          ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       readu,                 ! Read unit #1
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
   
c
c   read file
c
       call indata
     * (readu,nstat,nphas,nhead,nrecord,type,exp,data,id)
c      write(6,'(a)') data(1)(1:60)
c
c   check if id line
c
       if(id.eq.0) then
          write(printu,*)' No id line'
          write(printu,'(a)') data(1)(1:60)
          write(no_id_u,'(7x,a)') evfile
          no_id=no_id+1
c         stop
       endif
c
c  check if id line and file name are the same
c
       call check_id(evfile,type,data(id),nerr)
       if(nerr.ne.0) then
          write(6,*)' File name and id different'
          write(printu,*)' File name and id different'
          write(printu,'(a)') data(1)(1:60)
          write(bad_id_u,'(7x,a)') evfile
          bad_id=bad_id+1
c         stop
       endif
c
c  check s-file
c
       call check_s(data,nrecord,evfile,nerr,2,printu)
       if(nerr.gt.0) then
          write(6,*)' Error in S-file'
          write(printu,'(a)') data(1)(1:60)
          write(bad_s_u,'(7x,a)') evfile
          bad_s=bad_s+1
c         stop
       endif
c
c   close file
c
       call sei close (close$,readu,code)
c
c   back to next event
c  
       goto 5			


 99    continue
       enddo              ! end of base check loop
c
c  print out statistics
c
       write(6,*)
       write(6,*)' Number of events without id-lines:  ', no_id
       write(6,*)' Number of events with wrong id   :  ', bad_id
       write(6,*)' Number of events with error in file:', bad_s
       

      write(6,*)
      write(6,*)
      write(6,*)' Output file name is check_base.out'
      write(6,*)
     *' Index file with events with no id lines:       index_no_id.out' 
      write(6,*)
     *' Index file with events with wrong id line:     index_bad_id.out' 
      write(6,*)
     *' Index file with events with error in S-file :  index_bad_s.out' 
      write(6,*)

      stop
      end
     

      subroutine get_base_names(nbase,base)
c
c     read all names from REA directory and sort out names 5 characters long
c    
c     
      implicit none
      character*60 top_dir     ! seisan path
      character*1 dchar        ! dir separation char
      logical sun,linux,pc     ! computer type
      character*5 base(*)       ! bases
      integer nbase            ! number of bases
      integer n,i,k
      character*80 all_files(500) ! all names in REA
      character*80 text
      integer seiclen          ! function


      call topdir(top_dir)
      call dir_char(dchar)
      call computer_type(sun,pc,linux)
c
      text=top_dir(1:seiclen(top_dir))//dchar//'REA' 
c
c   make a list of files
c
       call getfiles(text,seiclen(text),all_files,500,n)
c
c   find all with 5 chars and no .
c
       nbase=0
       do i=1,n
         if(seiclen(all_files(i)).ne.5) goto 100 
         do k=1,5
            if(all_files(i)(k:k).eq.'.') goto 100
         enddo
         nbase=nbase+1
         base(nbase)=all_files(i)(1:5)
 100     continue
       enddo
c
c     
      return
      end
