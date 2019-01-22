
c
c basic routines and functions used in geneteic algorithm routines
c

      character*(*) function dec_to_bin(x,ll)
c
c convert real number to binary
c
      implicit none
      real x,y
      character*24 text
      integer i,ll,c
c      integer seiclen
      y=x
      c=0
      text='                        '
      do i=ll,1,-1
        c=c+1
        if (y-(2.**(i-1)).gt.0.) then
          y=y-(2.**(i-1))
          write(text(c:c),'(a1)') '1'
        else
          write(text(c:c),'(a1)') '0'
        endif
      enddo
      dec_to_bin = text(1:c)
      end


      real function bin_to_dec(j,ll)
c
c convert binary to decimal number
c
      implicit none
      integer ll,i,j(*)
      real x
      x=0.
      do i=1,ll
        x=x+float(j(i))*2.**(float(i-1))
      enddo
      bin_to_dec=x
      end


      subroutine text_to_array(tt,j,ll)
c
c read numbers in text string into integer array
c
      implicit none
      character*(*) tt
      integer j(*),ll,i,a
      a=0
      do i=ll,1,-1
        a=a+1
        read(tt(i:i),'(i1)') j(a)
      enddo
      return
      end

