      program rsac
      implicit none

!     Define the Maximum size of the data Array
      integer max
      parameter (MAX=1000)

!     Define the Data Array of size MAX
      real yarray
      dimension yarray(MAX)
      
!     Declare Variables used in the rsac1() and getfhv() subroutines
      character*10 kname
      integer nlen
      real beg, del
      integer nerr
      integer n1, n2
      real delta, b, t1, t2

!     Define the file to be read      
      kname='file1'

!     Read in the SAC File
      call rsac1(kname,yarray,nlen,beg,del,MAX,nerr)

!     Check the Error status      
      if(nerr .ne. 0) then
         write(*,*)'Error reading SAC file: ',kname
         call exit(-1)
      endif

!     Get floating point header value: Delta
!        'delta' - name of the header variable requested
!        delta   - value of the header variable delta, returned
!        nerr    - Error return flag
      call getfhv('delta',delta,nerr)
      if(nerr .ne. 0) then
         write(*,*)'Error reading variable: delta'
         call exit(-1)
      endif

!     Get floating point header value: B
      call getfhv('b',b,nerr)
      if(nerr .ne. 0) then
         write(*,*)'Error reading variable: b'
         call exit(-1)
      endif

!     Get floating point header value: t1
      call getfhv('t1',t1,nerr)
      if(nerr .ne. 0) then
         write(*,*)'Error reading variable: t1'
         call exit(-1)
      endif

!     Get floating point header value: t2
      call getfhv('t2',t2,nerr)
      if(nerr .ne. 0) then
         write(*,*)'Error reading variable: t2'
         call exit(-1)
      endif

!     Compute the time sample at which t1 and t2 occur
      n1 = int((t1 - b) / delta)
      n2 = int((t2 - b) / delta)

!     ......

      call exit(0)
      end
