      program wsac3f
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
      real cona, conb

!     Set the name the file to be written and initial x value
      kname='expdata    '
      xdata(1) = 0.1
      cona     = 12.3
      conb     = -45.6

!     Create the Amplitude and Time, an Exponential
!     Best viewed with axis as loglin
      ydata(1) = exp(-xdata(1))
      do j=2,MAX
         xdata(j) = xdata(j-1) + xdata(j-1) * 1.0/(4.0 * 3.1415);
         ydata(j) = exp(-xdata(j))
      enddo

!     Create a New Header to store more information
!     Newly created header value are set to a default state      
      call newhdr()
      
!     Store values in the newly created header
!     You must define the following header variables
!        - delta  Time Sampling
!                 Only if the file is evenly spaced
!        - b      Beginning Time
!        - e      Ending Time
!        - npts   Number of Points in the File
!        - iftype File Type
!             - itime Time Series File
!             - irlim Spectral File Real/Imaginary 
!             - iamph Spectral File Amplitue/Phase
!             - ixy   X-Y File
!             - iunkn Unknown
!
!     All other variables are up to the user
      call setnhv('npts',    max,        nerr)
      call setlhv('leven',   .false.,    nerr)
      call setfhv('b',       xdata(1),   nerr)
      call setfhv('e',       xdata(max), nerr)
      call setihv('iftype',  'ixy',      nerr)
      call setfhv('user0',   cona,       nerr)
      call setfhv('user1',   conb,       nerr)
      call setkhv('kuser0', 'gendat',    nerr)
      
!     Write the SAC file kname
!       - kname holds the name of the file to be written
!       - xdata Input Time Data      
!       - yfunc Input Amplitude Data
!       - nerr Error return Flag
      call wsac0(kname,xdata,ydata,nerr)

!     Check the Error status
!       - 0 on Success
!       - Non-Zero on Error
      if(nerr .NE. 0) then
         write(*,*)'Error writing SAC File: ', kname,nerr
         call exit(-1)
      endif

      call exit(0)

      end
      
