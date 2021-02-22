      program envelopef
      implicit none

      include "sacf.h"

      integer i
!     Define the Maximum size of the data Array
      integer MAX
      parameter (MAX=4000)

!     Define the Data Array of size MAX
      real yarray1, yarray2, xarray, out, ytmp
      dimension yarray1(MAX), yarray2(MAX), out(MAX*4), ytmp(MAX*4)

!     Declare Variables used in the rsac1() subroutine
      real beg1, beg2, delta, endv
      integer nlen, nlen1, nlen2
      character*30 KNAME
      integer nerr
      real fval
      
!     Cross Correlation Variables
      integer nwin, wlen, nfft
      character *256 error
      real max_value, max_time

!     Read in the first data file
      kname = 'correlatef_in1.sac'
      call rsac1(kname, yarray1, nlen1, beg1, delta, MAX, nerr)

      if(nerr .NE. 0) then
      	  write(*,*)'Error reading in file: ',kname
	  call exit(-1)
      endif

!     Read in the second data file
      kname = 'correlatef_in2.sac'
      call rsac1(kname, yarray2, nlen2, beg2, delta, MAX, nerr)

      if(nerr .NE. 0) then
      	  write(*,*)'Error reading in file: ',kname
	  call exit(-1)
      endif

      nlen = nlen1
!
!     If the signals are not the same length, then find the longest
!     signal, make both signals that length by filling the remainder
!     with zeros (pad at the end) and then run them through crscor
!     This should be fixed in upcoming releases and the introduction
!     of a "correlate" function so you do not need to handle the 
!     signal length, padding, and window lengths, ...
!

      nwin = 1
      wlen = nlen
      nfft = 0
!     Call crscor ( Cross Correlation )
!        - yarray1 - First  Input array to correlate
!        - yarray2 - Second Input array to correlate
!        - nlen    - Number of points in yarray and yarray2
!        - nwin    - Windows to use in the correlation
!        - wlen    - Length of the windows
!        - type    - Type of Window (SAC_RECTANGLE)
!        - out     - output sequence 
!        - nfft    - Length of the output sequence
!        - error   - Error Message
!

      call crscor(yarray1, yarray2, nlen, 
     &            nwin, wlen, SAC_RECTANGLE,
     &            ytmp, nfft, error)

      do i = 1,MAX*4
         out(i) = 0.0
      enddo
!     
!     out[1 : nlen1 - 1 ] <-- ytmp[ nfft - nlen1 + 2 : nfft  ]
!     out[nlen1 : nlen1 + nlen2 - 1 ] <-- ytmp[ 1 : nlen2 ]
!
      do i = 1,nlen1-1
         out(i) = ytmp(nfft - nlen1 + i + 1)
      enddo
      do i = 1,nlen2
         out(nlen1 + i - 1) = ytmp(i)
      enddo


      nfft = nlen1 + nlen2 - 1
      xarray = 0
      beg1 = -delta * (nlen1 - 1.0) + (beg2 - beg1)
      endv = beg1 + delta * (nfft - 1)

      call setnhv('npts',   nfft,    nerr)
      call setfhv('delta',  delta,   nerr)
      call setlhv('leven',  .true.,  nerr)
      call setfhv('b',      beg1,    nerr)
      call setfhv('e',      endv,    nerr)
      call setihv('iftype', "itime", nerr)
      call setnhv('nzyear', SAC_NUMBER_UNDEFINED, nerr)
      call setnhv('nzhour', SAC_NUMBER_UNDEFINED, nerr)
!     Find the maximum value and time of the correlation function 
      max_value = out(1)
      max_time  = 0
      do i = 2,nfft
         if(out(i) > max_value) then
            max_value = out(i)
!           Negative shifts are at the end of the correlation sequence
            if(i > nfft/2) then
               max_time = beg1 + (i - nfft) * delta
            else
               max_time = beg1 + i * delta
            endif
         endif
      enddo

!      call setfhv( 'user0', max_time,  nerr)
!      call setfhv( 'user1', max_value, nerr)


!     Write the SAC file
      kname='correlatef_out1.sac'
      call wsac0(kname, xarray, out, nerr)
      if(nerr .NE. 0) then
      	  write(*,*)'Error writing out file: ',kname
	  call exit(-1)
      endif

      call exit(0)

      end program envelopef
