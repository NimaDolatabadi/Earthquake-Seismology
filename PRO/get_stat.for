c
c   get station coordinates
c
      include 'libsei.inc'
      real lat,lon,h
      character*5 stat
      integer nargs          ! number of arguments
      character*80 arg(5)    ! arguments
      call get_arguments(nargs,arg)

      stat=arg(1)
      call stat_loc(stat,' ',lat,lon,h)
      k=h
      write(6,'(1x,a5,2f10.4,i10)') stat,lat,lon,k
      stop
      end
