      program seedresp2gse
c
c convert seed paz response to gse format
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'resp.inc'

      character*5 sta
      character*4 comp
      character*14 time
      character*80 filename
      double precision event_time
      integer read01,code
      logical b_flag
      integer year,month,day,hour,min
      real sec
      double precision seed_start

c
c get input
c
      write(*,*) ' name of seed response file '
      read(5,'(a)') filename
      write(*,*) ' station name '
      read(5,'(a)') sta
      write(*,*) ' component name '
      read(5,'(a)') comp
      write(*,*) ' date and time (yyyymmddhhmm) '
      read(5,'(a)') time

      call sei open( old$,                          ! Open file (stop on error).
     &                     ' ',                ! No prompt.
     &                     filename,           ! This filename.
     &                     read01,             ! On unit.
     &                     b_flag,             ! Exists!!
     &                     code )              !
      if( code .ne. e_ok$ ) stop

      read(time,'(i4,4i2)') year,month,day,hour,min
      sec=0.
      call timsec(year,month,day,hour,min,sec,event_time)

c
c read seed response into common block
c
      call read_seed_resp(read01,event_time,seed_start)

c
c write gse response
c
      call write_gse_resp(sta,comp,time)
      call sei close ( close$, read01, code )

      write(*,*)
      write(*,*) ' gse response file: gseresp.out '

      stop
      end


