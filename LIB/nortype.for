      subroutine nortype(unit,compact)
c
c   check if file is compact or not.
c
c   jh july 1993
c
c   updates
c
c   sep 16 98, jh : ---------   version 7.0 check-------------------
c                   no changes
c
c   unit : file unit already opened
c   compact: true if compact
c
      implicit none
      character*80 text
      logical compact
      integer i,unit
c
         compact=.true.
c
c  assume that there are not more than 20 epicenter lines for one event
c
         do i=1,20                                                              
            read(unit,'(a80)',end=10) text                                     
            if(text(22:22).ne.'L'.and.text(22:22).ne.'R'.                 
     *      and.text(22:22).ne.'D'.and.text(22:22).ne.' ')                
     *      compact=.false.                                     
c
c   it could be that there are only epicenter lines and blanl lines
c
            if(text(1:30).eq.'                              ') 
     *      compact=.false.
         enddo                      
c
 10      continue                                            
         rewind unit
         return
         end
