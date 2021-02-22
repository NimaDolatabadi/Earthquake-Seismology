      program filter
      implicit none

      include "sacf.h"

!     Define the Maximum size of the data Array
      integer MAX
      parameter (MAX=1000)

!     Define the Data Array of size MAX
      real yarray, xarray
      dimension yarray(MAX)

!     Declare Variables used in the rsac1() subroutine
      real beg, delta
      integer nlen
      character*20 KNAME
      integer nerr

!     Define variables used in the filtering routine
      real *8 low, high
      real *8 transition_bandwidth, attenuation
      real *8 delta_d
      integer order, passes

      kname = 'filterf_in.sac'

      call rsac1(kname, yarray, nlen, beg, delta, MAX, nerr)
      delta_d = delta

      if(nerr .NE. 0) then
      	  write(*,*)'Error reading in file: ',kname
	  call exit(-1)
      endif

      low    = 0.10
      high   = 1.00
      passes = 2
      order  = 4
      transition_bandwidth = 0.0
      attenuation = 0.0
!     Call xapiir ( Apply a IIR Filter )
!        - yarray - Original Data
!        - nlen   - Number of points in yarray
!        - proto  - Prototype of Filter
!                 - SAC_FILTER_BUTTERWORK        - Butterworth
!                 - SAC_FILTER_BESSEL            - Bessel
!                 - SAC_FILTER_CHEBYSHEV_TYPE_I  - Chebyshev Type I
!                 - SAC_FILTER_CHEBYSHEV_TYPE_II - Chebyshev Type II
!        - transition_bandwidth (Only for Chebyshev Filter)
!                 - Bandwidth as a fraction of the lowpass prototype
!                   cutoff frequency
!        - attenuation (Only for Chebyshev Filter)
!                 - Attenuation factor, equals amplitude reached at
!                   stopband egde
!        - order  - Number of poles or order of the analog prototype
!                   4 - 5 should be ample
!                   Cannot exceed 10
!        - type   - Type of Filter
!                 - SAC_FILTER_BANDPASS
!                 - SAC_FILTER_BANDREJECT
!                 - SAC_FILTER_LOWPASS
!                 - SAC_FILTER_HIGHPASS
!        - low    - Low Frequency Cutoff [ Hertz ]
!                   Ignored on SAC_FILTER_LOWPASS
!        - high   - High Frequency Cutoff [ Hertz ]
!                   Ignored on SAC_FILTER_HIGHPASS
!        - delta  - Sampling Interval [ seconds ]
!        - passes - Number of passes 
!                 - 1 Forward filter only
!                 - 2 Forward and reverse (i.e. zero-phase) filtering
!
      call xapiir(yarray, nlen, 
     +            SAC_BUTTERWORTH, 
     +            transition_bandwidth, attenuation,
     +            order, 
     +            SAC_BANDPASS,
     +            low, high, delta_d, passes)

!     Do more processing ....
      
      xarray = 0
      kname='filterf_out.sac'
!     Write the SAC file kname
!       - kname holds the name of the file to be written
!       - xdata Input Time Data      
!       - yfunc Input Amplitude Data
!       - nerr Error return Flag
      call wsac0(kname,xarray,yarray,nerr)
      if(nerr .NE. 0) then
      	  write(*,*)'Error writing out file: ',kname
	  call exit(-1)
      endif

      call exit(0)

      end program filter
