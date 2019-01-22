      subroutine inc_id(idline,filename,nf)
c
c     updates:
c
c jun 11 98 jh  : extracted from split and eev
c                 bug in id fixed
c sep 16 98 jh  : ---------- version 7.0 check --------------------
c                 remove pc part
c oct 16          bug
c
      implicit none
c
c   adds one second to file name and id line
c
      character*80 filename,idline
      integer nf      ! number of characters in filename
      integer isec
c
c 
c   read second and add 1, if 60, start over again
c
      read(filename(nf-10:nf-9),'(i2)') isec
      isec=isec+1
      if(isec.gt.60) isec=1
c
c   write back
c
      write(filename(nf-10:nf-9),'(i2)') isec
      if(filename(nf-10:nf-10).eq.' ') 
     *filename(nf-10:nf-10)='0'
c
c   put into id
c
      idline(73:74)=filename(nf-10:nf-9) ! added 6-98
c
c   note in id that this was a dublicate
c
      idline(75:75)='d'
c
      return
      end      

