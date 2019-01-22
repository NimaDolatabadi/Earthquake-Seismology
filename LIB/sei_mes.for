
      subroutine put_seisan_message(text)
c
c   updates
c
c   sep 98 by jh  : ---------   verison 7.0 check ----------------
c                   no changes
c  mar 30   jh    : add routines put_message and get_message
c  apr 29 2010 lo : fixed character length in put_seisan_message and get_s...
c  feb 01 2011 jh : cut out write to screeen
c  apr 12 2013 jh : array bounds
c
c   put message text in a file seisan.mes
c
      implicit none
      include 'libsei.inc'
      character*(*) text
      integer code   ! code from open
      logical b_old  ! from open
      integer unit   ! unit to write to
      integer seiclen
c
c   open message file
c
c      write(*,*) ' sei_mes with variable text length '
      call sei open(unknown$+warn$,        ! Open a unknown status file.
     &              ' ',                   ! Prompt file name (n/a).
     &              'seisan.mes',          ! File name
     &              unit,                  ! Write unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.

c
c   write to file
c
      write(unit,'(a)') text(1:seiclen(text)) ! fixed problem on solaris lo

c
c  close
c
      call sei close (close$,unit,code)
c
      return
      end

      subroutine get_seisan_message(text)
c
c   get message text from file seisan.mes
c
      implicit none
      include 'libsei.inc'
      character*(*) text
      integer code   ! code from open
      logical b_old  ! from open
      integer unit   ! unit to write to
c
c   open message file
c
      call sei open(old$+ignore$,          ! Open a unknown status file.
     &              ' ',                   ! Prompt file name (n/a).
     &              'seisan.mes',          ! File name
     &              unit,                  ! Write unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.

       if(b_old) then
c
c   read from file
c
         read(unit,'(a)') text
c
c  close
c
         call sei close (delete$,unit,code)
      else
         text=' '
      endif
c
      return
      end


      subroutine put_message(file,text)
c
c   put message text in a file.mes
c   by lo
c
      implicit none
      integer  sei clen      ! character length
      include 'libsei.inc'
      character* (*) text
      character* (*)  file
      character*80 filen
      integer code   ! code from open
      logical b_old  ! from open
      integer unit   ! unit to write to

      filen= file(1:seiclen(file)) // '.mes'

c
c   open message file
c
      call sei open(unknown$+warn$,        ! Open a unknown status file.
     &              ' ',                   ! Prompt file name (n/a).
     &              filen,          ! File name
     &              unit,                  ! Write unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.

c
c   write to file
c
      write(unit,'(a)') text
c
c  close
c
      call sei close (close$,unit,code)
c
      return
      end

      subroutine get_message(file,text)
c
c   get message text from file.mes
c   by lo
c
      implicit none
      integer  sei clen      ! character length
      include 'libsei.inc'
      character*(*) text
      character*(*) file
      character*80 filen
      integer code   ! code from open
      logical b_old  ! from open
      integer unit   ! unit to write to

      filen= file(1:seiclen(file)) // '.mes'
c
c   open message file
c
      call sei open(old$+ignore$,          ! Open a unknown status file.
     &              ' ',                   ! Prompt file name (n/a).
     &              filen,                 ! File name
     &              unit,                  ! Write unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.

       if(b_old) then
c
c   read from file
c
         read(unit,'(a)') text
c
c  close
c
         call sei close (delete$,unit,code)
      else
         text=' '
      endif
c
      return
      end
