      program envelopef
      implicit none

      include "sacf.h"

!     Define the Maximum size of the data Array
      integer MAX
      parameter (MAX=1000)

!     Define the Data Array of size MAX
      real yarray, xarray, yenv
      dimension yarray(MAX), yenv(MAX)

!     Declare Variables used in the rsac1() subroutine
      real beg, delta
      integer nlen
      character*20 KNAME
      integer nerr

      kname = 'envelopef_in.sac'

      call rsac1(kname, yarray, nlen, beg, delta, MAX, nerr)

      if(nerr .NE. 0) then
      	  write(*,*)'Error reading in file: ',kname
	  call exit(-1)
      endif

!     Call envelope ( Envelope Routine )
!        - nlen   - Number of points in yarray
!        - yarray - Original Data
!        - yenv   - Envelope of the Original Data
!
      call envelope(nlen, yarray, yenv)

!     Do more processing ....
      
      xarray = 0
      kname='envelopef_out.sac'
!     Write the SAC file kname
!       - kname holds the name of the file to be written
!       - xdata Input Time Data      
!       - yfunc Input Amplitude Data
!       - nerr Error return Flag
      call wsac0(kname,xarray,yenv,nerr)
      if(nerr .NE. 0) then
      	  write(*,*)'Error writing out file: ',kname
	  call exit(-1)
      endif

      call exit(0)

      end program envelopef
