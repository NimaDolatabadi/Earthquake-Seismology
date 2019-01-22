      program hyp
c 
c program to call hypmain, which is included in LIB/hypmain.for
c hypmain is original hyp.for turned into ia subroutine
c
c 27 Aug 2015, jh and lo
c 19 feb 2016  jh        : change name to hypmain
c

      implicit none
      include 'seidim.inc'
      include 'rea.inc'
c
c   message fixed array size since not sure c++ can expand from fortran
c
      character*80 sfilename,message(50)
      integer i,nmessage
      logical from_se        ! true if called from se
      logical locate         ! true if event was located
      sfilename=' '
      from_se=.false.        ! works as normal hyp
c     from_se=.true.         ! for testing in se mode
      if(from_se) sfilename='eev.out'     ! for testing in se mode

      call hypmain(sfilename,from_se,message,nmessage,locate)
      
      if(.not.locate) then
         nmessage=nmessage+1
         message(nmessage)='Event not located'
      endif
c
c   for se testing
c      
      if(from_se) then
         do i=1,nmessage
            write(6,*)'message ',message(i)
         enddo
      endif
      
      stop
      end

