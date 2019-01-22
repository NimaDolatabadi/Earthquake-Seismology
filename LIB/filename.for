C**************************************************************************
C                                                                               
      SUBROUTINE filename(question,file_name)                                   
c                                                                               
c    question is printed out first, then                                        
c    routine reads a file name, checks it for one of the following              
c    possibilities                                                              
c                                                                               
c    1: file name longer than 4 chars, routine returns                          
c    2: file name is a question mark: read file listing of files and print      
c       then return for new file name.                                          
c    3: file name is between 1 and 4 chars and not a question mark:            
c       file name is assumed to be a number which indicates nr in list          
c       of files. Finds file name in list and returns it.
c       If not all 4 characters are digits, assume a file name and returns                       
c    5: file name is blank, return EOF                                          
c    6: question is #Next_file#, which will be next file relative to
c       previous number used, question is then not printed out
c                                                                               
c    updates
c    jul 24 92 by jh  : next file option, add eof check 
c    oct 28           : add write out of filenr if next option is used
C!JAB(BGS)Nov94       : install error & file handling...
c    dec 94           : **************version 5.0 *******************
C!JAB(BGS)Mar95       : Diagnostic to inform of filename request rather than
C!JAB(BGS)Mar95         item from filenr.lis
C!JAB(BGS)Mar95       : Add "q" to quit
C!JAB(BGS)Aug95       : If filenr.lis does not exist, message before killing.
C!JAB(BGS)Aug95         In future, this might just give message & return the
C!JAB(BGS)Aug95         condition code to be processed by the calling program
c   jan 17 96 jh      : 4 digit number, less than 4 letter file if none of
c                       the letters are digits
c   mar 31    jh      : reset access variable before opening filenr.lis
c---------------------------------------------------------------------
c   sep 98 jh         : ----------- version 7.0 check -------------
c                       no change
c   sep 25 jh         : stop every 20 lines when using '?'
c   oct 18 2002 jh    : add back option
C
C    Seisan library details...
C    -------------------------
C
      implicit none                                                             
      INCLUDE   'libsei.inc'                   ! Definitions and data defns.
      external  sei open,                      ! Open file handler.
     &          sei close,                     ! Close file handler.
     &          sei code,                      ! Error handler.
     &          sei clen                       ! String length
      integer   sei clen                       ! Function.
c
      integer   code,                          ! Local error condition.
     &          read1                          ! Read unit 1.
      logical   b_flag                         ! Flag existance.?
C
C    -------- end ------------
C
c-- question to be printed out                    
      character*(*) question		   
c-- input output file name             
      character*(*) file_name               
c-- file name on disk file             
      character*80 file_list               
c-- length of file_name                
      integer length                       
c-- number of file in list             
      integer file_nr,old_file_nr                      
      character*80 text
      integer nout                  ! counter for output lines
      character*1 number(10)        ! ascii values of numbers
      logical file                  ! true if a file name
      logical digit                 ! true if a digit
c-- counters                           
      integer i,k                            
      data number/'1','2','3','4','5','6','7','8','9','0'/
      common/filenumberlis/file_nr    !new
C
C    Initialise...
C    =============
C
1     code  = e_ok$                           ! Local condition.
      nout=1
      file_name = ' '
c                                                                               
c    input file name if not next file                                         
c    --------------------------------
c                                                                               
      if(question(1:11).ne.'#Next_file#'. and.
     *   question(1:11).ne.'#Back_file#') then
         write(6,'(a80)')question                                               
         call flush (6)
         read(5,'(a80)') file_name 
      else
         if(question(1:11).eq.'#Next_file#') file_nr=old_file_nr+1
         if(question(1:11).eq.'#Back_file#') then
             file_nr=old_file_nr-1
             if(file_nr.eq.0) file_nr=1
         endif                                                
         goto 1000
      endif
c                                                                               
c    check if blank indicating EOF                                             
c    -----------------------------
c                                                                               
      if(file_name(1:1).eq.' '   .or.       !JAB(BGS)Mar95.
     &   file_name(1:1).eq.'q'   .or.       !JAB(BGS)Mar95.
     &   file_name(1:1).eq.'Q' ) then       !JAB(BGS)Mar95.
         file_name='EOF'                                                        
         goto 9999                        ! Return to caller.
      endif                                                                     
c
c   if firs charcter is  ?, go down to list file list
c
      if(file_name(1:1).eq.'?') goto 1000
     
c
c-- check if a normal file name, pad name with nulls...         
c   --------------------------------------------------
c
      length = sei clen( file_name )      ! Length of file name.
c
c  if length is less than 5, could still be a file name if not
c  all characters are numbers
c
      if(length.lt.5) then
         file=.false.
         do i=1,length
           digit=.false.
           do k=1,10
             if(file_name(i:i).eq.number(k)) digit=.true.
           enddo
           if(.not.digit) file=.true.
         enddo
      endif
      if(length.gt.4.or.file) then
         do i=length+1,80
            file_name(i:i)=char(0)
         enddo
         goto 9999                         ! Return to caller.
      endif                                                                    
c
 1000 continue
c
c    open file containing list of files numbered                                
c    -------------------------------------------
c                                                                               
      chr_f_access$ = ' '
      call sei open( old$+warn$,         ! Open & warn of errors.
     &               ' ',                  ! No prompt.
     &               'filenr.lis',         ! Filename.
     &               read1,                ! Open on unit.
     &               b_filenr$,            ! Flag existance & usage?.
     &               code )                ! Condition.
      chr_err_msg$ = 
     &'**** FATAL: problem opening "filenr.lis"'
      call sei code( stop$, code, 0, b_flag ) ! Force an abort on error.
C
c-- list files                         
c   ----------
c
      if(file_name(1:1).eq.'?') then            !
3     read(read1,'(a80)',iostat=code) file_list ! read item.                 
      call sei code(fort$,code,read1,b_flag)    ! Process outcome.
c
c    End of file...
c    --------------
c
         if( b_flag ) then                      ! End of file.
         call sei close(close$,read1,code)      ! Close file.
         go to 1                                ! & start again.
c
c    Write item to standard output...
c    --------------------------------
c
         else                                   ! Otherwise.
         write(6,'(1x,a79)') file_list(1:79)    ! Write to statndard output.
         nout=nout+1
         if(nout.gt.20) then
            write(6,*)' Return to continue, q to quit listing'
            call flush (6)
            read(5,'(a1)') text(1:1)
            if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') then
               call sei close(close$,read1,code)      ! Close file.
               go to 1                                ! & start again.
            endif
            nout=1
         endif
         go to 3                                ! Read next record.
         end if                                 !
      endif                                                                     
c                                                                               
c   file name is a number, find nr and name in list on disk, if not
c   ---------------------------------------------------------------
c   next file to use                
c   ----------------
c                                                                               
      if(question(1:11).ne.'#Next_file#'.and.
     *   question(1:11).ne.'#Back_file#') then
         if(length.eq.1) read(file_name,'(i1)') file_nr                         
         if(length.eq.2) read(file_name,'(i2)') file_nr                         
         if(length.eq.3) read(file_name,'(i3)') file_nr                         
         if(length.eq.4) read(file_name,'(i4)') file_nr                         
      endif
c
c   write out filen number if next or back file option
c
      if(question(1:11).eq.'#Next_file#'.or.
     *   question(1:11).eq.'#Back_file#') then
          write(6,*)' Filenumber',file_nr
      endif
c                                                                               
c-- read until file nr is hit in file  
c   ---------------------------------
c
      do i=1,file_nr                       
      read(read1,'(7X,a70)',iostat=code) file_name ! Read filename.
      call sei code(fort$,code,read1,b_flag)       ! Process outcome.
      if( b_flag ) goto 50                         ! End of file.
      enddo                                                                     
C
      if(file_name(1:1).eq.' ') goto 50
      i=index(file_name,' ')
c
c   make sure end of file name do not contain blanks
c
      do k=i,80
         file_name(k:k)=char(0)
      enddo
      goto 51
c
 50   continue
         write(6,*)' File number too large'
         file_name(1:3)='EOF'
         call sei close(close$,read1,code)     ! Close file.
         goto 9999                             ! Return to caller.
c
51    call sei close(close$,read1,code)        ! Close file.
c
c   save file number
c
      old_file_nr=file_nr
c
c   Return to caller...
c   ===================
c
9999  return
      end                                                                       
