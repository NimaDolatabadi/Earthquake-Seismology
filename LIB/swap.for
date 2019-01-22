c     swap routines     j. havskov, 1998
c

      subroutine swap4(nsamp,data)
c
c   routine to swap 4 byte data
c
      implicit none
      integer*4 data(*)         ! data to be swapped
      integer nsamp             ! number of values
      character*1 swap(4),s(4)
      integer*4   x,i,k
      equivalence (x,swap)
c
      do i=1,nsamp
        x=data(i)
        do k=1,4
           s(k)=swap(k)
        enddo
        swap(1)=s(4)
        swap(2)=s(3)
        swap(3)=s(2)
        swap(4)=s(1)
        data(i)=x
      enddo
      return
      end



      subroutine swap4_one(data)
c
c   routine to swap 4 byte data, one value
c
      implicit none
      integer*4 data            ! data to be swapped
      character*1 swap(4),s(4)
      integer*4   x,k
      equivalence (x,swap)
c
        x=data
        do k=1,4
           s(k)=swap(k)
        enddo
        swap(1)=s(4)
        swap(2)=s(3)
        swap(3)=s(2)
        swap(4)=s(1)
        data=x
      return
      end
      
      
      subroutine swap2(nsamp,data)
c
c   routine to swap 2 byte data
c
      implicit none
      integer*2 data(*)         ! data to be swapped
      integer nsamp             ! number of values
      character*1 swap(2),s(2)
      integer*2   x,i,k
      equivalence (x,swap)
c
      do i=1,nsamp
        x=data(i)
        do k=1,2
           s(k)=swap(k)
        enddo
        swap(1)=s(2)
        swap(2)=s(1)
        data(i)=x
      enddo
      return
      end

      subroutine swap2_one(value)
c
c   swap two bytes
c
      implicit none
      integer*2 value,i
      character*2 a2
      character*1 a1
c
      equivalence (a2,i)
c
      i=value
      a1=a2(1:1)
      a2(1:1)=a2(2:2)
      a2(2:2)=a1
      value=i
      return
      end
        

      subroutine swap4r(nsamp,data)
c
c   routine to swap 4 byte data
c
      implicit none
      real*4 data(*),x          ! data to be swapped
      integer nsamp             ! number of values
      character*1 swap(4),s(4)
      integer*4   i,k
      equivalence (x,swap)
c
      do i=1,nsamp
        x=data(i)
        do k=1,4
           s(k)=swap(k)
        enddo
        swap(1)=s(4)
        swap(2)=s(3)
        swap(3)=s(2)
        swap(4)=s(1)
        data(i)=x
      enddo
      return
      end

