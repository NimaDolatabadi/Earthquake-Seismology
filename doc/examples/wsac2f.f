      program wsac2f
      implicit none

!     Define the Maximum size of the data arrays      p
      integer MAX
      parameter (MAX=300)
      
!     Define both data arrays, time and amplitude
      real xdata, ydata
      dimension xdata(MAX), ydata(MAX)

!     Define the varaibles used in the call to wsac2()
      character*10 kname
      integer j
      integer nerr

!     Set the name the file to be written and initial x value
      kname='expdata    '
      xdata(1) = 0.1

!     Create the Amplitude and Time, an Exponential
!     Best viewed with axis as loglin
      ydata(1) = exp(-xdata(1))
      do j=2,MAX
         xdata(j) = xdata(j-1) + xdata(j-1) * 1.0/(4.0 * 3.1415);
         ydata(j) = exp(-xdata(j))
      enddo

!     Write the SAC file kname
!       - kname holds the name of the file to be written
!       - yfunc Input Amplitude Data
!       - MAX number of points to be written
!       - xdata Input Time Data      
!       - nerr Error return Flag
      call wsac2(kname,ydata,MAX,xdata,nerr)

!     Check the Error status
!       - 0 on Success
!       - Non-Zero on Error
      if(nerr .NE. 0) then
         write(*,*)'Error writing SAC File: ', kname,nerr
         call exit(-1)
      endif

      call exit(0)

      end
      
