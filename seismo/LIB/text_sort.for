      subroutine text_sort(text_in,text_out,order_out,n)
c
c
c      input: text_in: text string to be sorted in 80 char array, only first
c                      70 chars can be used
c             n:       number of strings
c      output text_out: sorted strings
c      order out: order of strings out
c
c      jh april 2003
c
c     changes:
c
      implicit none
c      character*80 text_in(*),text_out(*)
c changed lot 21/4/2006
      character*(*) text_in(*),text_out(*)
      integer order_out(*)
      integer n,i



c   transfer input data
c
          do i=1,n
             text_out(i)(1:80)=text_in(i)
          enddo
c
c   add a number for getting order
c
          do i=1,n
             write(text_out(i)(71:80),'(i8)') i
          enddo
c
c   sort array
c
          call sortfi(text_out,n)
c 
c   find order of sorting and transfer text to output array
c
          do i=1,n
             read(text_out(i)(71:80),'(i8)') order_out(i)
             text_out(i)(71:80)=' '
          enddo 
	  
          return
	  end
