c
      subroutine xy_ellipse(erx,ery,erz,cvxy,cvxz,cvyz,emaj,emin,ang)
c
c  By Mario Villagran, intended to get information for the
c  norgse program to provide information to IDC from NDC-SOREQ
c
c   changes
c
c  may 12 2000 by jh : remove routine eigen, hyposub2
c  feb 11 2013    jh : dimension fix
c
c  Israel
c  inputs are:
c                erx,ery,erz,cvxy,cvxz,cvyz (outputs of hyp program)
c  outputs are:
c                emaj,emin,ang (dimensions of semi-major axis and azimuth)
c        
c Calculate many things, here this routine only extracts
c the size of the ellipsoide error and the orientation
c from the north (degrees).
c calculate maximum horizontal dimensions of error ellipse
c using ellipsoide dimensions & orientations
c  in ae & ve (output of subroutine ellipse)
c
c changes:
c  june 13, 99 lo: replaced atan2d by atan2, *360/2pi
c  2013-06-12 pv : changed three atan2 lines and
c             removed three lines that changed angel, see comment below.
c
        implicit none
        real ae(9),ve(9)
        real a,b,c,emaj,emin,ang,pi
        real erx,ery,erz,cvxy,cvxz,cvyz,var(3,3)
c
c   covarriance matrix
c
        var(1,1)=erx*erx
        var(2,2)=ery*ery
        var(3,3)=erz*erz
        var(1,2)=cvxy
        var(1,3)=cvxz
        var(2,3)=cvyz
        var(2,1)=var(1,2)
        var(3,1)=var(1,3)
        var(3,2)=var(2,3)
      pi=3.1415926
c
c
        call ellipse_x(var,ae,ve)
c
c find the projection of components of the major ellipse axes (the 3
c eigenvalues of the covariance matrix) on the xy plane
        a=sqrt(abs(ae(1)))*cos(atan2(ve(3),sqrt(ve(1)*ve(1)+ve(2)
     &  *ve(2))))
        b=sqrt(abs(ae(3)))*cos(atan2(ve(6),sqrt(ve(4)*ve(4)+ve(5)
     &  *ve(5))))
        c=sqrt(abs(ae(6)))*cos(atan2(ve(9),sqrt(ve(7)*ve(7)+ve(8)
     &  *ve(8))))
 
c major & minor axes are the largest two of these
        if(a.ge.b.and.a.ge.c)then
         emaj=a
 
c angle between major axis, a, and x-axis
c         ang=atan2d(ve(2),ve(1))
c        ang=atan2(ve(2),ve(1))*360/(2*pi)     ! lo
          ang=atan2(ve(2),ve(1)) ! pv
 
         if(b.ge.c)then
          emin=b
         else
          emin=c
         endif
        elseif(b.ge.a.and.b.ge.c)then
         emaj=b
c         ang=atan2d(ve(5),ve(4))
c        ang=atan2(ve(5),ve(4))*360/(2*pi)     ! lo
          ang=atan2(ve(5),ve(4))  ! pv
         if(c.ge.a)then
          emin=c
         else
          emin=a
         endif
        elseif(c.ge.a.and.c.ge.b)then
         emaj=c
c         ang=atan2d(ve(8),ve(7))
c        ang=atan2(ve(8),ve(7))*360/(2*pi)     ! lo
          ang=atan2(ve(8),ve(7))   ! pv
         if(b.ge.a)then
          emin=b
         else
          emin=a
         endif
        endif
c 2013-06-12 pv: I have no idear why the next three lines are here. They
c give wrong az and are not in old code by Barry, see epimap old_xy_ellipse subroutine:
c       if(ang.ge.0.and.ang.le.90.0)ang=90.0-ang
c       if(ang.gt.90.0)ang=450.0-ang
c       if(ang.lt.0.0)ang=-ang+90.0
c          write(88,*)emaj,emin,ang,emaj*emin*3.1416
        return
        end
c
      subroutine ellipse_x(var,ae,ve)
 
c calculates major axis lengths and their orientations
c from covariance matrix using subroutine eigen
c the input covariance matrix is
c by Barry Lienert, 94
c  varxx=var(1,1), varyy=var(2,2), varzz=var(3,3),... etc
c on output, major axis lengths in descending order
c of size are in ae(1), ae(3) & ae(6)
c x,y,z direction cosines of their (x,y,z) orientations are
c   ve(1),ve(2),ve(3)
c   ve(4),ve(5),ve(6)
c   ve(7),ve(8),ve(9)
 
      implicit none
      real ae(*),var(3,3),ve(*)
      integer ic,j,k
 
c put var in ae in input format used by eigen
        ic=1
        do  j=1,3
         do  k=1,j
          ae(ic)=var(j,k)
          ic=ic+1
         end do
        end do
 
        call eigen(ae,ve,3,0)
c       write(*,*)ae(1),ae(2),ae(3),ae(4),ae(5),ae(6)
        return
        end
c***************************************************************************
