      subroutine polos(fy,fx,ypol,iant,insidepol)
c
c    updates
c
c    sep 98 by jh : ---------  version 7.0 check ------------------
c                   no changes
c
c     to find out if a given point (fy,fx) is inside any of the
c            polygons defined by ypol.  the number of the
c                   polygon is returned in 'insidepol'
c input :
c  fy,fx           : point to find polygone for
c  ypol(iant,2): array defining the polygone
c  iant       : number of points defining polygone
c output :
c  inside             : true - polygon contains event 
c
      include 'seidim.inc'        ! dimensions
      integer iant
      dimension ypol(max_polyn$,2),x(2),xx(2)
      logical insidepol
      real fy, fx
C
C    ============= Changes =================
C!JAB(BGS)Nov94       : YPOL is multi-dimensional and will not allow
C!JAB(BGS)Nov94         successful compilation with some compiler options.
C!JAB(BGS)Nov94       : Trap zero denominators.   
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei code                    ! Error encoder.
C
       logical  b_flag                      ! Dummy boolean.
C
      integer   npol, npol1, iflag, i, m, n ! Previously undefined (JAB).
      real      x, eps, xx, diff, yy,       ! Ditto.
     &          dx1, dx2, dy1, dy2, xcross  ! Ditto.
C
C    ============= end of changes ==========
C
C    Initialise...
C    -------------
C
      x(1) = fx
      x(2) = fy
      eps= 0.0001
      insidepol = .false.
c
      npol = iant
c
      xx(1)=x(1)
      xx(2)=x(2)
c
    6 iflag = 0
      do 5 i=1,npol
          diff= abs(xx(2)-ypol(i,2))
          if(diff.gt.0.00001) go to 5
            xx(2)= xx(2) + eps
            iflag = 1
    5 continue
      if (iflag.eq.1) go to 6
c
c now, dy1 or dy2 will never be zero  (hopefully..)
c
      npol1= npol+1
      ypol(npol1,1) = ypol(1,1)
      ypol(npol1,2) = ypol(1,2)
c
      dx1 = ypol(1,1) - xx(1)
      dy1 = ypol(1,2) - xx(2)
c
      n= 0
      do 50 i=2,npol1

           dx2 = ypol(i,1) - xx(1)
           dy2 = ypol(i,2) - xx(2)
c
           yy= dy1*dy2
           if (yy.ge.0) go to 10
             if (dx1.lt.0. .and. dx2.lt.0.) go to 10
               if (dx1.gt.0. .and. dx2.gt.0.) go to 20
C
C    Trap potential zero denominator...
C    ----------------------------------
C
                 if( abs(dy1-dy2) .gt. 0.0 ) then
                 xcross= (dy1*dx2 - dy2*dx1)/(dy1-dy2)
C
                 else
                 chr_err_msg$ =              ! Overwrite with this message.
     &'Invalid point in/or polygon, zero denominator (polos)'
                 call sei code( stop$,       ! Stop on error error.
     &                          e_dnom$,     ! Forced by denominator.
     &                          0,           ! Not by fortran unit.
     &                          b_flag )     ! Flag (n/a).
                 end if
C
C    ------------ end -----------------
C
                 if (xcross.lt.0.) go to 10
c
   20          n=n+1
c
   10      dx1=dx2
           dy1=dy2
   50 continue

c
      m=(n/2)*2
      if(m.eq.n) go to 30
c
c n odd, x is inside polygon.
c
      insidepol = .true.
      goto 9999                            ! Return to caller.
c
c n even, x is outside polygon
c

   30 insidepol = .false.
c
C    Return to caller...
C    ===================
C
9999  return
      end
