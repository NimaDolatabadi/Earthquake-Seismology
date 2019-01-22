                                                                                
********************************************************************
      subroutine auto_filename(filename,iport,sfile)
c
c     routine to get the tracedata filename from s-file
c     Filename is assumed to be in a type 6 line in the s-file.
c     The s-file to extract the filename from can either be found in
c     enviromental memory, or it can be read directly.
C     Input:
c       filename: normally blank, if '01' use first waveform file, no question
c       iport
c	if  iport = 0 the s-file is found in 'transfer.idx'
c	if  iport .ne. 0 the s-file is read on iport. It is then assumed
c                         that the s-file is opened outside this routine.
c       if no index.temp file is present, sfile is returned as 'NOINDEX'
c
c     Output:
c       filename
c       sfile name
c
c     written by C. Lindholm, Dec. 1988.
c     Updates:
c            Nov. 25; C. L. ; Change from next line indicator to this lien indicator in S file
c            Jul  20  jh    ; bug
c            nov 9          : filenames to 15
c            feb 21 93      : filenames to 80, only close nport if iport=0
c            aug 22         : fix some reads to make all 80 chars with filenames
C!JAB(BGS)Dec94             : Install error and file handling
c             jan 95        *************   version 5.0 ************
C!JAB(BGS)Mar95             : Prevent core crashing if invalid preference     
c jh aug 96                 : put in to select first wav file if many
c jan 97 jh                 : dimention of filenames from 15 to 50
c sep 98 jh                 : -------------- version 7.0 check -------------
c                             no change
c
c-- name of tracedatafile
      character*80 filename
c-- name of S file
      character*80 sfile
c-- name of tracedatafile
      character*80 filenames(50)
c-- record cards
      character*80 card(200)
c-- port to read from
      integer iport
c
      integer   kk, nport, j, number       ! Missing variables.
C
C    Seisan library inserts and routines...
C    ======================================
C
       include  'libsei.inc'               ! Library definitions & data defns.
       external  sei code,                 ! Error encoder.
     &           sei integer,              ! Get integer.
     &           sei close,                ! Close file opened by...
     &           sei open                  ! Open file handler.
       integer   sei integer               ! & function.
       integer   code                      ! local condition
       character chr_text *(10)            ! Local text string.
       logical   b_flag                    ! Flag end of file or existance.
C
C    ============= end of list ==========
C
c
c    Initialise...
C    =============
C
      kk=0
      nport = iport                         ! Working file unit.
c
      if(iport.eq.0) then 
         call get_env_event(sfile)
         if(sfile(1:3).eq.'   '.or.ichar(sfile(1:1)).eq.0) then
            sfile='NOINDEX'                    ! Flag for output.
            goto 9999                          ! & return to caller.
         endif
         write(*,*)sfile
         call sei open( old$,
     &                  ' ',
     &                  sfile,
     &                  nport,
     &                  b_flag,
     &                  code )
       endif
c
c   read the file
c
40     read(nport,'(a80)',iostat=code)card(200)   ! Read a card!
       call sei code(fort$,code,nport,b_flag)     ! Process the outcome.
       if( b_flag               .or.              ! End of file!.
     &     card(200) .eq. ' ' ) then              ! Or pulled a blank.
       continue                                   !
c
       else if( card(200)(80:80).eq.'6' ) then    !
       kk=kk+1                                    !
       card(kk) = card(200)                       !
       goto 40                                    ! Next card.
c
       else                                       ! Otherwise.
       goto 40                                    ! Next card.
       endif                                      !
c
c-- No file found
c
      if(kk.eq.0)then                             ! Flag for return.
      filename='EOF'                              !
c      call sei close(close$,nport,code)           !
      goto 9999                                   ! Return to caller.
      endif

c-- More files found
      if(kk.gt.1)then
         if(filename.eq.'01') then  ! if only one, skip
            filename(1:59)=card(1)(2:60)
            goto 900
         endif
         write(6,'(a)')' More than one datafile:'
        do j=1,kk
         filenames(j)=card(j)
         write(6,'(1x,i2,2x,a)')j,filenames(j)(2:60)
        enddo
c
1111     write(*,*)                                !JAB(BGS)Mar95.
         write(6,'(a)')' Which do you prefer?'    
         read(5,'(a)') chr_text                    !JAB(BGS)Mar95.
         number = sei integer( chr_text, code )    !JAB(BGS)Mar95.
c
c     Invalid...
c
            if( code .ne. e_ok$ ) then             !JAB(BGS)Mar95.
            write(*,*)'****       ... try again...'!JAB(BGS)Mar95.
            goto 1111                              !JAB(BGS)Mar95.
c
            else if( number .lt. 1    .or.         !JAB(BGS)Mar95.
     &               number .gt. kk ) then         !JAB(BGS)Mar95.
            write(*,*)                             !JAB(BGS)Mar95.
            write(*,*)                             !JAB(BGS)Mar95.
     &'**** WARN: your choice is out of range ...try again ****'
            goto 1111                              !JAB(BGS)Mar95.
            end if                                 !JAB(BGS)Mar95.
         filename(1:59)=card(number)(2:60)
c-- One file found
      else
        filename(1:78)=card(kk)(2:79)
      endif
 900  continue
c
c   Close file if opened inside the this subroutine...
c
      if(iport.eq.0) call sei close(close$,nport,code)
c
c    Return to Caller...
c    ===================
c
9999  return
      end
