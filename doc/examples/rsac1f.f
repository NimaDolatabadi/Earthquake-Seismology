      program rsac
      implicit none

!     Define the Maximum size of the data Array
      integer MAX
      parameter (MAX=1000)

!     Define the Data Array of size MAX
      real yarray
      dimension yarray(MAX)

!     Declare Variables used in the rsac1() subroutine
      real beg, del
      integer nlen
      character*10 KNAME
      integer nerr

!     Define the file to be read      g
      kname = 'FILE1'

!     Call rsac1 to read filename kname
!        - Data is loaded into yarray
!        - Length of data is stored in nlen
!        - Begining time and time sampling are in beg and del
!        - MAX is the maximum number of points to be read in 
!        - nerr is the Error return flag
      call rsac1(kname, yarray, nlen, beg, del, MAX, nerr)

!     Check the error status, nerr
!        - 0 on Success
!        - Non-Zero on Failure
      if(nerr .NE. 0) then
      	  write(*,*)'Error reading in file: ',kname
	  call exit(-1)
      endif

!     Do some processing ....

      call exit(0)
      end
