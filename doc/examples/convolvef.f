      program envelopef
      implicit none

      include "sacf.h"

      integer i,j
!     Define the Maximum size of the data Array
      integer MAX
      parameter (MAX=4000)

!     Define the Data Array of size MAX
      real yarray1, yarray2, ytmp, xarray, out
      dimension yarray1(MAX), yarray2(MAX), ytmp(MAX), out(MAX*4)

!     Declare Variables used in the rsac1() subroutine
      real beg, delta, endv
      integer nlen1, nlen2, nlen
      character*30 KNAME
      integer nerr
      
!     Cross Correlation Variables
      integer nwin, wlen, nfft
      character *256 error
      character *24  kevnm

!     Read in the first data file
      kname = 'convolvef_in1.sac'
      call rsac1(kname, ytmp, nlen1, beg, delta, MAX, nerr)

      if(nerr .NE. 0) then
      	  write(*,*)'Error reading in file: ',kname
	  call exit(-1)
      endif

!     Read in the second data file
      kname = 'convolvef_in2.sac'
      call rsac1(kname, yarray2, nlen2, beg, delta, MAX, nerr)

      if(nerr .NE. 0) then
      	  write(*,*)'Error reading in file: ',kname
	  call exit(-1)
      endif

!     Reverse the First Signal */      
      j = 1
      do i = nlen1,1,-1
         yarray1(j) = ytmp(i)
         j = j + 1
      enddo

      nlen = nlen1
      if(nlen2 > nlen) then
         nlen = nlen2
      endif


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
     &            out, nfft, error)


!     Zero out the tmp signal      
      do i = 1, MAX
         ytmp(i) = 0.0
      enddo

!     Reconstruct the signal from the "cross correlation" back to front
!  
!     ytmp[1        : nlen1 - 1         ] <- out[nfft-nlen1+2 : nfft  ] 
!     ytmp[nlen1    : nlen1 + nlen2 - 1 ] <- out[1            : nlen2 ]
!
!     nfft is the last point of the output sequence
!
      do i = 1,nlen1-1
         ytmp(i) = out(nfft - nlen1 + i + 1)
      enddo
      do i = 1, nlen2
         ytmp(nlen1 + i - 1) = out(i)
      enddo


      nfft = nlen1 + nlen2 - 1
      xarray = 0
      beg = 0
      endv = beg + delta * (nfft - 1)
      j = 1

      call newhdr()
      call setnhv('npts',   nfft,    nerr)
      call setfhv('delta',  delta,   nerr)
      call setlhv('leven',  .true.,  nerr)
      call setfhv('b',      beg,     nerr)
      call setfhv('e',      endv,    nerr)
      call setihv('iftype', 'itime', nerr)
      call setkhv('kstnm',  'sta',   nerr)
      call setkhv('kcmpnm', 'Q',     nerr)
      call setnhv('nwfid',  j, nerr)
      kevnm = 'FUNCGEN: TRIANGLE'
      call setkhv ('kevnm', kevnm, nerr)
!     Write the SAC file
      kname='convolvef_out1.sac'
      call wsac0(kname, xarray, ytmp, nerr)
      if(nerr .NE. 0) then
      	  write(*,*)'Error writing out file: ',kname,nerr
	  call exit(-1)
      endif

      call exit(0)

      end program envelopef
