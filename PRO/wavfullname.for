      program wavfullname
c
c program to write out full path, input is given on command line
c    e.g. wavfullpath 2002-11-26-0059-27S.LNQ___013
c will print full path + filename to stamdard output
c
c May 2003, Lars Ottemoller
c
      implicit none

      character*80 arg(1)
      character*240 fullname
      integer narg,seiclen

      call get_seisan_def
      call get_arguments(narg,arg)
      call get_full_wav_name(arg(1),fullname)
      if (seiclen(fullname).le.0) then
        write(*,'(a)') 'not found'
      else
        write(*,'(a)') fullname(1:seiclen(fullname))
      endif
      stop
      end
