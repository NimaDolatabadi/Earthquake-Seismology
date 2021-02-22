      program wsac
      implicit none

!     Define the Maximum size of data array
      integer MAX
      parameter (MAX=200)

!     Define the data array
      real yfunc
      dimension yfunc(MAX)

!     Define variables to be passed to wsac1()
      character*10 kname
      integer j
      integer nerr
      real beg
      real del
      real x

!     Define the file to be written, the beginning time 
!     time sampling, and the initial value
      kname = 'expdata'
      beg   = 0.00
      del   = 0.02
      x     = beg

!     Create the Amplitude data, an Exponential
      do j=1,MAX
         yfunc(j)=exp(-x)
         x=x+del
      enddo

!     Write the SAC file kname
!       - kname holds the name of the file to be written
!       - yfunc Input Amplitude data
!       - MAX number of points to be written
!       - beg Beginning Time of the data
!       - del Time Sampling of the series
!       - nerr Error return Flag
      call wsac1(kname,yfunc,MAX,beg,del,nerr)
      
!     Check the Error status
!       - 0 on Success
!       - Non-Zero on Error
      if(nerr .NE. 0) then
         write(*,*)'Error writing SAC File: ', kname
         call exit(-1)
      endif

      call exit(0)
      end
