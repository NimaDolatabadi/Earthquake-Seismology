      program rsac_2
      implicit none

!     Define the Maximum size of the data Array
      integer MAX
      parameter (MAX=3000)

!     Define the Time and Amplitude arrays of zize MAX
      real xarray, yarray
      dimension xarray(MAX), yarray(MAX)

!     Declare Variables used in the rsac2() subroutine
      character*10 kname
      integer nlen
      integer nerr

!     Define the file to be read      
      kname='file2'

!     Call rsac2 to read filename kname
!        - Amplitude Data is loaded into yarray
!        - Length of data is stored in nlen
!        - Time Data is loaded into xarray
!        - MAX is the maximum number of points to be read in 
!        - nerr is the Error return flag
      call rsac2(kname,yarray,nlen,xarray,MAX,nerr)

!     Check the error status, nerr
!        - 0 on Success
!        - Non-Zero on Failure
      if(nerr .ne. 0) then
         write(*,*)'error reading in sac file: ',kname
         call exit(-1)
      endif

!     Do some processing ....

      call exit(0)
      end
      
