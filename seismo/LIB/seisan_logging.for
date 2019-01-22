c
c  Subroutines for Logging in SEISAN
c
c  First version 2015.05.26 Peter Voss
c
c
c
C########################################################################### 

      subroutine get_log_file_name(sfile,logfile)
c get the name of the log file from the sfile name
c
c 2015-10-20 pv : fix bug while using local database
c 2015-05-26 pv : first version

c-- s-file name               
      character*80 sfile
c-- log file name               
      character*120 logfile

      integer i,seiclen

c     write(6,*)'subroutine get_log_file_name'

      i=seiclen(sfile)

      if(i.GT.19) then
        logfile(1:i-27)=sfile(1:i-27)
        logfile(i-26:i-24)="LOG"
        logfile(i-23:i+4)=sfile(i-27:i)
        logfile(i+5:i+8)=".LOG"
      else
        logfile(1:19)=sfile(1:19)
        logfile(20:23)=".LOG"
      endif
      
c     write(6,*) "logfile name: ",logfile

      return
      end     

C########################################################################### 

      subroutine create_log_file(sfile,operator)
c This subroutine creates a log file to a s-file, but not 
c if the log file already exist
c
c     call create_log_file(sfile)
c
c 2015-10-20 pv : fix bug while using local database
c 2015-05-26 pv : first version

      include 'libsei.inc'                ! Open file definitions
      logical b_old,b_eof,b_flag
      integer read01,code
      integer write01
c-- log file name               
      character*120 logfile
c-- s-file name               
      character*80 sfile
c-- s-file lines
      character*80 text
c-- log file folder
      character*120 folder
      character*140 txt
      character*19  logtime
      character*4   operator
      logical sun,pc,linux 
      logical folder_exist, file_exist
      integer seiclen,fstart,i

      call computer_type(sun,pc,linux)

c     write(6,*)'subroutine create_log_file'
c     call get_log_file_name(sfile,logfile)
      call get_log_file_name(sfile,logfile)

      inquire(file=logfile, exist=file_exist)
      if ( file_exist ) then
c       write(6,'(1x,a)') ' log file already exist'
        return
      endif

      call get_log_time(logtime)

c skip create-folder if working in local database
      if(seiclen(logfile).GT.23) then
c check if log folder exist
      folder=logfile(1:seiclen(logfile)-23)
c     write(6,*)'folder: ',folder
c check if the log folder exist, if not create
      inquire(file=folder, exist=folder_exist)
      if ( .NOT.folder_exist ) then
        write(6,'(1x,a)') 'Creating new log file folder:'
        write(6,'(1x,a)') folder
c create folder
        txt='mkdir -p '//folder
        if(pc) txt(1:9)='mkdir    '
        call systemc(txt,seiclen(txt))
      endif
      endif

c     write(6,*)' logtime: ',logtime

c create log file

c open s-file
      call sei open(old$+warn$,            ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             sfile,              ! File name
     &             read01,                ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition
      if (code .EQ. e_ok$) then
c open log file
        call sei open( unknown$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               logfile,     ! Filename.
     &               write01,              ! Open on unit.
     &               b_flag,               ! Flag existance & usage?.
     &               code )                ! Condition.
        if (code .EQ. e_ok$) then
          write(write01,'(a,a)') 'SEISAN Log file. This file contain',
     +' a log of the event listed below.'
          write(write01,'(a,a)')'This log file was created on: ',logtime
          write(write01,'(a,a)')'The operator was: ',operator
          write(write01,'(a)') 'The s-file content at creation:'
          call sei close( close$, write01, code )   ! Close
        else
          write(6,'(1x,a,a)')' Cannot open file: ',logfile
          return
        endif
        call sei close( close$, read01, code )    ! Close
      else
        write(6,'(1x,a,a)')' Cannot open file: ',sfile
        return
      endif

c add the content of the s-file to the log file:
      call sfile_to_log_file(sfile)

      return
      end     

C########################################################################### 

      subroutine show_log_file(logfile)
c This subtrutine show the content of the log file
c 2015-05-26 pv : first version

      include 'libsei.inc'                ! Open file definitions
      logical b_old,b_eof
      integer read01,code
      character*120 chr_file
c
c-- log file name               
      character*120 logfile
c-- s-file lines
      character*80 text
c-- log file folder
      character*120 folder
      character*140 txt
      logical folder_exist
      integer seiclen,fstart,i

      write(6,'(1x,a)') ''
      write(6,'(a)') ' Log file content: '
      write(6,'(1x,a)') ''

      call sei open(old$+warn$,     ! Open a existing file.
     &             ' ',             ! Prompt file name (n/a).
     &             logfile,         ! File name
     &             read01,          ! Read unit #1
     &             b_old,           ! Already exists? (n/a).
     &             code)            ! Returned condition
      if (code .EQ. e_ok$) then
        i=1
        b_eof=.FALSE.
        do while (.NOT.b_eof)
          read(read01,'(a)',iostat=code) text
          call sei code(fort$,code,read01,b_eof)
          if(.NOT.b_eof) write(6,'(a)') text
          i=i+1
          if(i.gt.40) then
            write(6,*)' Return to continue, q to return to EEV'
            read(5,'(a)') txt
            if(txt(1:1).eq.'q'.or.txt(1:1).eq.'Q') b_eof=.TRUE.
            i=1
          endif
        enddo
        call sei close( close$, read01, code )    ! Close
      else
        write(6,'(1x,a,a)')' Cannot open file: ',logfile
      endif

      return
      end     

C########################################################################### 

      subroutine sfile_to_log_file(sfile)
c This subroutine adds the sfile to the end of the log file
c 2015-05-26 pv : first version
c 2016-09-27 pv : remove double last line of sfile

      include 'libsei.inc'                ! Open file definitions
      logical b_old,b_eof,b_flag
      integer read01,code
      integer write01
c     character*120 chr_file
c-- log file name               
      character*120 logfile
c-- s-file name               
      character*80 sfile
c-- s-file lines
      character*80 text
c-- log file folder
      character*120 folder
      character*80 txt
      character*19  logtime

c     write(6,*)'subroutine sfile_to_log_file'

      call get_log_file_name(sfile,logfile)
      txt(1:40)='----------------------------------------'
      txt(41:80)='----------------------------------------'

c open s-file
      call sei open(old$+warn$,            ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             sfile,              ! File name
     &             read01,                ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition
      if (code .EQ. e_ok$) then
c open log file
        chr_f_access$ = 'APPEND'
        call sei open( old$+warn$,           ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               logfile,     ! Filename.
     &               write01,              ! Open on unit.
     &               b_flag,               ! Flag existance & usage?.
     &               code )                ! Condition.
        if (code .EQ. e_ok$) then
          b_eof=.FALSE.
          do while (.NOT.b_eof)
            read(read01,'(a)',iostat=code) text
            call sei code(fort$,code,read01,b_eof)
            if(.NOT.b_eof) write(write01,'(a)') text
          enddo
          write(write01,'(a80)') txt
          call sei close( close$, write01, code )   ! Close
        else
          write(6,*)''
          write(6,'(1x,a,a)')' Cannot open file: ',logfile
        endif
        call sei close( close$, read01, code )    ! Close
      else
        write(6,*)''
        write(6,'(1x,a,a)')' Cannot open file: ',sfile
      endif

      return
      end     

C########################################################################### 

      subroutine move_log_file(newfile,sfile,operator,action)
c This subroutine add actions to the log file, if the 
c s-file is renamed or moved to the DELET database:
c  action=1 used for events that change type
c  action=2 used for events that are deleted and moved to DELET
c
c 2015-05-26 pv : first version
c         call move_log_file(newfile,sfile,operator)

      include 'libsei.inc'                ! Open file definitions
      logical b_old,b_eof,b_flag
      integer code
      integer read01
      integer write01
      logical file_exist
c
c-- operator code
      character*4 operator
c-- log file name               
      character*120 logfile_old
      character*120 logfile_new
c-- s-file name               
      character*80 sfile
      character*80 newfile
c-- s-file lines
      character*80 text
c-- eev action
      integer action,l1,l2
c-- log file folder
      character*120 folder
      character*140 txt
      character*19  logtime
c
      logical sun,pc,linux 
      logical folder_exist

      integer seiclen

      call computer_type(sun,pc,linux)
c     write(6,*)' subroutine move_log_file'

      call get_log_time(logtime)
      call get_log_file_name(sfile,logfile_old)
      call get_log_file_name(newfile,logfile_new)

c check if log folder exist in DELET database
      if(action.EQ.2)then
        l1=seiclen(logfile_new)-19
        l2=seiclen(logfile_new)-23
        folder=logfile_new(1:l2)
        folder(l1:l2)='DELET'
c       write(6,*)'PV delete folder: ',folder
c check if the log folder exist, if not create
        inquire(file=folder, exist=folder_exist)
        if ( .NOT.folder_exist ) then
          write(6,'(1x,a)') 'Creating new log file folder:'
          write(6,'(1x,a)') folder
c create folder in DELET
          txt='mkdir -p '//folder
          if(pc) txt(1:9)='mkdir    '
          call systemc(txt,seiclen(txt))
        endif
      endif

c check if log file for sfile exist - if not: create
      inquire(file=logfile_old, exist=file_exist)
      if (.NOT.file_exist) then
        write(6,'(1x,a)') ''
        write(6,'(1x,a)') 'No log file, creating a new log file'
        write(6,'(1x,a)') ''
        return
      endif

c move to newfile log name
c chekc if new log file already exist
      inquire(file=logfile_new, exist=file_exist)
      if (file_exist) then
        write(6,'(1x,a)') 'Log already exist - cannot overwrite !!'
        write(6,'(1x,a)') ''
        return
      endif

c open old log file
      call sei open(old$+warn$,         ! Open a existing file.
     &      ' ',                        ! Prompt file name (n/a).
     &      logfile_old,                ! File name
     &      read01,                     ! Read unit #1
     &      b_old,                      ! Already exists? (n/a).
     &      code)                       ! Returned condition
      if (code .EQ. e_ok$) then
c open new log file
        call sei open( unknown$+warn$,  ! Open & warn of errors.
     &      ' ',                        ! No prompt.
     &      logfile_new,                ! Filename.
     &      write01,                    ! Open on unit.
     &      b_flag,                     ! Flag existance & usage?.
     &      code )                      ! Condition.
        if (code .EQ. e_ok$) then
          b_eof=.FALSE.
          do while (.NOT.b_eof)
            read(read01,'(a80)',iostat=code) text
            call sei code(fort$,code,read01,b_eof)
            if(.NOT.b_eof)write(write01,'(a80)') text
          enddo
        else
          write(6,'(1x,a,a)')' Cannot open file: ',logfile_new
          return
        endif
      else
        write(6,'(1x,a,a)')' Cannot open file: ',logfile_old
        return
      endif

c add comment that event type was changed
      write(write01,100) logtime,operator
      if(action.EQ.1) write(write01,'(a,a)') 'The action was:',
     &' Change of event type.'
      if(action.EQ.2) write(write01,'(a,a)') 'The action was:',
     &' Event was deleted.'
      write(write01,'(a)')   'S-file content after action: '

c close new log file
      call sei close( close$, write01, code )   ! Close

c     write(6,*)'PV rename_log_file sfile to log'
c add sfile content to log file
      call sfile_to_log_file(newfile)
      
c     write(6,*)'PV rename_log_file delete old log'
c   delete old log file
      call sei close (delete$,read01,code)

c     write(write01,100) logtime,operator
  100 FORMAT('Time of action: ',a19,' Operator:'a4,1x)

      return
      end     

C########################################################################### 

      subroutine get_log_time(logtime)
c get the time string from the OS
c 2015-05-26 pv : first version

      character*19 logtime
      character*12 timestring
      character*14 timestring1

c     write(6,*)'subroutine get_log_time'
      call systime(timestring,timestring1)

      logtime(1:2)='20'
      logtime(3:4)=timestring(1:2)  ! year
      logtime(5:5)='-'
      logtime(6:7)=timestring(3:4)  ! mdr
      logtime(8:8)='-'
      logtime(9:10)=timestring(5:6)  ! day
      logtime(11:11)=' '
      logtime(12:13)=timestring(7:8)  ! hour min
      logtime(14:14)=':'
      logtime(15:16)=timestring(9:10)  ! hour min
      logtime(17:17)=':'
      logtime(18:19)=timestring(11:12)      ! sec

c     logtime(20:30)=' Time of OS'

      return
      end     

C########################################################################### 

      subroutine message_to_log_file(sfile,operator,action)
c This subroutine added a message in the logfile, the message is
c given by the action integer where:
c
c  action=1 event was updated
c  action=2 REG or PUT
c  action=3 DUP old event
c  action=4 DUP new event
c  action=5 E
c  action=6 COMMENT
c  action=7 EMAIL ALERT
c  action=999 dummy
c
c 2015-05-26 pv : first version

      include 'libsei.inc'                ! Open file definitions
      logical b_old,b_eof,b_flag
      integer code
      integer write01
      logical file_exist
c
c-- operator code
      character*4 operator
c-- log file name               
      character*120 logfile
c-- s-file name               
      character*80 sfile
c-- s-file lines
c     character*80 text
c-- eev action
      integer action
c-- log file folder
c     character*120 folder
c     character*140 txt
      character*19  logtime
c
c     write(6,*)' subroutine message_to_log_file'

      call get_log_time(logtime)
      call get_log_file_name(sfile,logfile)

c check if log file for sfile exist - if not: create
      inquire(file=logfile, exist=file_exist)
      if (.NOT.file_exist) then
        write(6,'(1x,a)') ''
        write(6,'(1x,a)') 'No log file'
        write(6,'(1x,a)') ''
        return
      endif

c open log file
      chr_f_access$ = 'APPEND'
      call sei open( old$+warn$,  ! Open & warn of errors.
     &    ' ',                        ! No prompt.
     &    logfile,                    ! Filename.
     &    write01,                    ! Open on unit.
     &    b_flag,                     ! Flag existance & usage?.
     &    code )                      ! Condition.

      if (code .EQ. e_ok$) then
c add message to log file
        write(write01,100) logtime,operator
  100   FORMAT('Time of action: ',a19,' Operator:'a4,1x)
c update
        if(action.EQ.1) then
          write(write01,'(a,a)') 'The action was:',
     &    ' Event was updated.'
          write(write01,'(a)')   'S-file content after action: '
          call sei close( close$, write01, code )   ! Close
          call sfile_to_log_file(sfile)
        endif
c reg
        if(action.EQ.2) then
          write(write01,'(a,a)') 'The action was:',
     &    ' REG or PUT command executed.'
          write(write01,'(a)')   'S-file content after action: '
          call sei close( close$, write01, code )   ! Close
          call sfile_to_log_file(sfile)
        endif
c dup old event
        if(action.EQ.3) then
          write(write01,'(a,a)') 'The action was:',
     &    ' DUP used to make a copy of this event'
          write(write01,'(a)') 
     &'-------------------------------------------------------------'
          call sei close( close$, write01, code )   ! Close
        endif
c dup new event
        if(action.EQ.4) then
          write(write01,'(a,a)') 'The action was:',
     &    ' DUP command executed from another event.'
          write(write01,'(a)') 
     &'-------------------------------------------------------------'
          call sei close( close$, write01, code )   ! Close
        endif
c edit
        if(action.EQ.5) then
          write(write01,'(a,a)') 'The action was:',
     &    ' E - edit sfile.'
          write(write01,'(a)')   'S-file content after action: '
          call sei close( close$, write01, code )   ! Close
          call sfile_to_log_file(sfile)
        endif
c com  
        if(action.EQ.6) then
          write(write01,'(a,a)') 'The action was:',
     &    ' A comment was added'
          write(write01,'(a)')   'S-file content after action: '
          call sei close( close$, write01, code )   ! Close
          call sfile_to_log_file(sfile)
        endif
c
c email
        if(action.EQ.7) then
          write(write01,'(a,a)') 'The action was:',
     &    ' Alert email(s) was send'
          write(write01,'(a)')   'S-file content during action: '
          call sei close( close$, write01, code )   ! Close
          call sfile_to_log_file(sfile)
        endif
c
c dummy
        if(action.EQ.999) then
          write(write01,'(a,a)') 'The action was:',
     &    'hello from picsub '
          write(write01,'(a)') 
     &'-------------------------------------------------------------'
          call sei close( close$, write01, code )   ! Close
        endif
c
c       if(action.EQ.2) then
c         write(write01,'(a,a)') 'The action was:',
c    &' xxxxxxxxxxxxxxxxxx'
c         call sei close( close$, write01, code )   ! Close
c       endif
      else
        write(6,'(1x,a,a)')' Cannot open file: ',logfile
        return
      endif

      return
      end     

C########################################################################### 

      subroutine pick_to_log_file(sfile,operator,pick)
C This subroutine adds the character used to pick phases etc.
c
c 2015-05-26 pv : first version
c

      include 'libsei.inc'                ! Open file definitions
      logical b_old,b_eof,b_flag
      integer code
      integer write01
      logical file_exist
c
c-- operator code
      character*4 operator
c-- log file name               
      character*120 logfile
c-- s-file name               
      character*80 sfile
c-- s-file lines
      character*1 pick

      character*19  logtime
c
c     write(6,*)' subroutine pick_to_log_file'

      call get_log_time(logtime)
      call get_log_file_name(sfile,logfile)

c check if log file for sfile exist - if not: create
      inquire(file=logfile, exist=file_exist)
      if (.NOT.file_exist) then
        write(6,'(1x,a)') ''
        write(6,'(1x,a)') 'No log file'
        write(6,'(1x,a)') ''
        return
      endif

c open log file
      chr_f_access$ = 'APPEND'
      call sei open( old$+warn$,  ! Open & warn of errors.
     &    ' ',                        ! No prompt.
     &    logfile,                    ! Filename.
     &    write01,                    ! Open on unit.
     &    b_flag,                     ! Flag existance & usage?.
     &    code )                      ! Condition.

      if (code .EQ. e_ok$) then
c add message to log file
        write(write01,100) logtime,operator
  100   FORMAT('Time of action: ',a19,' Operator:'a4,1x)
          write(write01,'(a,a,a)') 'The action was:',
     &    ' MULPLT Phase pick: ',pick
          write(write01,'(a)')
     &'-------------------------------------------------------------'
          call sei close( close$, write01, code )   ! Close
      else
        write(6,'(1x,a,a)')' Cannot open file: ',logfile
        return
      endif

c close log file

      return
      end     

C########################################################################### 

      subroutine string_to_log_file(logstring,sfile,operator)
C This subroutine adds the string given by logstring to the 
c log file.
c
c 2015-05-26 pv : first version

      include 'libsei.inc'                ! Open file definitions
      logical b_old,b_eof,b_flag
      integer code
      integer write01
      logical file_exist
c
c-- operator code
      character*4 operator
c-- log file name               
      character*120 logfile
c-- s-file name               
      character*80 sfile
c-- s-file lines
      character*40 logstring

      character*19  logtime
c
c     write(6,*)' subroutine string_to_log_file'
c     write(6,*)' string: ',logstring

      call get_log_time(logtime)
      call get_log_file_name(sfile,logfile)

c check if log file for sfile exist - if not: create
      inquire(file=logfile, exist=file_exist)
      if (.NOT.file_exist) then
        write(6,'(1x,a)') ''
        write(6,'(1x,a)') 'No log file'
        write(6,'(1x,a)') ''
        return
      endif

c open log file
      chr_f_access$ = 'APPEND'
      call sei open( old$+warn$,  ! Open & warn of errors.
     &    ' ',                        ! No prompt.
     &    logfile,                    ! Filename.
     &    write01,                    ! Open on unit.
     &    b_flag,                     ! Flag existance & usage?.
     &    code )                      ! Condition.

      if (code .EQ. e_ok$) then
c add message to log file
        write(write01,100) logtime,operator
  100   FORMAT('Time of action: ',a19,' Operator:'a4,1x)
          write(write01,'(a,a)') 'The action was: ',logstring
          write(write01,'(a)') 
     &'-------------------------------------------------------------'
          call sei close( close$, write01, code )   ! Close
      else
        write(6,'(1x,a,a)')' Cannot open file: ',logfile
        return
      endif

c close log file

      return
      end     

C########################################################################### 
