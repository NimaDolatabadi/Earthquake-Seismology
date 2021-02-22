      program wsac
      implicit none
      
!     Maximum Size of Array, in 2-D
      integer MAX
      parameter (MAX=36)

!     Size of arrays to store the data
      real dummy, zdata
      dimension dummy(MAX), zdata(MAX)
      
!     Define variables to be passed into wsac0() 
      character*10 kname
      integer i, j, k
      integer nerr
      integer nx, ny
      real minimum, maximum

!     Define the file to be written and the min and max of the 2-D Array
      kname   = 'xyzdata'
      minimum = 1.0
      maximum = 6.0
      nx      = 6
      ny      = 6
      
      ! Create the 2D Data
      k = 1
      do i = 1,nx
         do j = 1,ny
            zdata(k) = sqrt(j * 1.0 * j + i * 1.0 * i)
            k = k + 1
         enddo
      enddo

      ! Create a new Header and fill it
      !   We are defining the data type, iftype to be 'ixyz', a 2-D Array
      call newhdr
      call setnhv('npts',     MAX,     nerr)
      call setlhv('leven',    .true.,  nerr)
      call setihv('iftype',   'ixyz',  nerr)
      call setnhv('nxsize',   nx,      nerr)
      call setnhv('nysize',   ny,      nerr)
      call setfhv('xminimum', minimum, nerr)
      call setfhv('xmaximum', maximum, nerr)
      call setfhv('yminimum', minimum, nerr)
      call setfhv('ymaximum', maximum, nerr)

!     Write the SAC file kname
!       - kname holds the name of the file to be written
!       - dummy Input Amplitude Data
!       - zdata Input Time Data      
!       - nerr Error return Flag
      call wsac0(kname,dummy,zdata,nerr)

!     Check the Error status
!       - 0 on Success
!       - Non-Zero on Error
      if(nerr .NE. 0) then
         write(*,*)'Error writing SAC File: ', kname,nerr
         call exit(-1)
      endif

      call exit(0)

      end
