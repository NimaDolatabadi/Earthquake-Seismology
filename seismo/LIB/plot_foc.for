
       SUBROUTINE plot_foc(str,dp,rak,ix,iy,r,color_plane)
c
c   plot fault plane solution
c
c  22 02 2011 jh: reset color to color_def after plotting
c  05 01 2017 jh: add color_plane, add routine fps_gap
c  15 01 2017 Jh: add fps_gap_all
C  06 02 2017 jh: add make_fps_pol,make_fps_pol
c  08 12 2017 jh: change a call due to new focmec
c  20  5 2018 jh: one more call change, bits of new focmec routine put in
c
c      str,dp,rak: strike,dip and rake
c      ix,iy,r: center and radius
c      color_foc_plane: color of focal plane and P and T, if zero use
c                       predfined colors from color.def
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      implicit none
      include 'seiplot.inc'
c
c    Libsei details...
c    =================
c
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code                    ! Error condition handler.
      integer  code                        ! Condition.
      logical  b_flag                      ! Flag!!
      integer color_plane
      real azz(2000),ainn(2000)            ! azimuth and ain for polarity data
      character*1 pol(2000)                ! polarities
      integer npol                         ! number of polarities
      real IX, IY, R, x(1000),XBTP(2)
C ---
      CHARACTER  CBTP(3)*1                  ! symbols for t and p

      REAL ANBTP(6),ANGS(3),ANGS2(3),PTTP(4),DIP(100),STRIKE(100),
     1     RAKE(100),str,dp,rak
      REAL*8 xo, yo, xp, yp, fi, delta, ox, alfa, az, ax, btp(2),
     1       zo, alfa2, fi2
C ---
      REAL*4 MOMTEN(6)
      LOGICAL AN, PT, RIGHT, MT
      real xf(5000),yf(5000)              ! fault planes coordinates     
      integer if                          ! number of points in ----------
      real pi,z,x1,y1,rr,d
      integer ii,j,i,jj,k,icolor,l,kk


      DATA CBTP /'B','T','P'/

c
c    Initialise...
c    =============

c
C ---
      PI = 4.*atan(1.)
C ---
      RIGHT=.TRUE.
c
      call xset_color(color_frame)


      CALL FCIRCL(R, IX,IY)

C --- P and T axes


           dip(1)=dp
           strike(1)=str
           rake(1)=rak

           ANGS(1)=DIP(1)
           ANGS(2)=STRIKE(1)
           ANGS(3)=RAKE(1)

C ------- Calculate auxiliary planes

           mt=.false.
cold           CALL FMREPS(ANBTP,ANGS,PTTP,ANGS2,AN,PT,RIGHT,MT,MOMTEN,
           CALL FMREPS(ANBTP,ANGS,PTTP,ANGS2,PT,RIGHT,MT,MOMTEN,
     &        0,0)
           DIP(2)=ANGS2(1)
           STRIKE(2)=ANGS2(2)
           RAKE(2)=ANGS2(3)
c
c   
          if(r.gt.50) then    ! assume no beach ball and plot p and t
c
C ------- Plot B,T 
c
              do 30 jj = 2, 3    ! from 2 since B no longer plotted
                 if ( jj .eq. 2 ) then
C --------- T axis
                    btp(1)=pttp(3)
                    btp(2)=pttp(4)
                 elseif ( jj .eq. 3 ) then
C --------- P axis
                    btp(1)=pttp(1)
                    btp(2)=pttp(2)
                 endif
C -------- plunge -----------------------------------------------------                 
                 btp(2)= btp(2)*pi/180.0
C -------  trend
                 az    = btp(1)*pi/180.0

C ------- calculate sterographic projection of a line
                 ax= r*dtan(pi/4.0- btp(2)/2.0)
                 xbtp(1) = ax*dsin(az) + ix
                 xbtp(2) = ax*dcos(az) + iy
c
c  plot p and t
c
                 if(color_plane.eq.0) then
                    if(cbtp(jj).eq.'P') call xset_color(color_foc_p)
                    if(cbtp(jj).eq.'T') call xset_color(color_foc_t) 
                 else  
                    if(cbtp(jj).eq.'P') call xset_color(color_plane)
                    if(cbtp(jj).eq.'T') call xset_color(color_plane)
                 endif            

                 call xchars(cbtp(jj),1,xbtp(1)-2.0,xbtp(2)-4.0)
30         continue
       endif
c
c   since small, assume beach ball and then do not plot p and t
c
       if(r.le.50.0) then   ! beach ball
c
c   make points on focal sphere
c
           call make_fps_pol(angs(2),angs(1),angs(3),npol,azz,ainn,pol)
c
c   plot the points to fill out
c
           call plot_pol(azz,ainn,pol,npol,ix,iy,r,1) 
       endif



C ---------------------------------------------------------------------


         if(color_plane.eq.0) then
            call xset_color(color_foc_plane)
         else
            call xset_color(color_plane)
         endif
c
C ------- Plot mechanisms, that is fault lines
c
       if=1  ! points in fp's counter
       do 1000 j = 1 , 2   ! icount is twice the number of mechanisms

         delta  = dip(j)*pi/180.
         fi = strike(j)*pi/180.
         ii=1

         do 100 i = 1, 181
           alfa=(i-1)*pi/181. -pi/2.

C ------- calculate sterographic projection of plane

C ------- coordinate transformation to the x,y,z (E,N,Z) coordinates.
C ------- The xp, yp are the coordinates in the plane of the fault.
C ------- xp direction of dip and yp direction of strike.
            xp=dcos(alfa)
            yp=dsin(alfa)
            zo=xp*dsin(delta)
            xo=( yp*(dsin(fi)) + xp*(dcos(delta)*dcos(fi)) )
            yo=( yp*(dcos(fi)) - xp*(dcos(delta)*dsin(fi)) )
            fi2 = datan(zo/dsqrt(xo*xo + yo*yo))
            if ( xo .gt. 0 .and. yo .gt. 0 ) then
               alfa2 = datan(xo/yo)
            elseif ( xo .gt. 0 .and. yo .lt. 0 ) then
               alfa2 = datan(dabs(yo)/dabs(xo)) + pi/2.
            elseif ( xo .lt. 0 .and. yo .lt. 0 ) then
               alfa2 = datan(dabs(xo)/dabs(yo)) + pi
            elseif ( xo .lt. 0 .and. yo .gt. 0 ) then
               alfa2 = datan(dabs(yo)/dabs(xo)) + 3.*pi/2. 
            endif
            ox=dtan(pi/4.-fi2/2.)
            xo= real(ix) + real(r)*ox*dsin(alfa2)
            yo= real(iy) + real(r)*ox*dcos(alfa2)
            x(ii)   = xo
            xf(if)=xo
            x(ii+1) = yo
            yf(if)=yo
            if=if+1
c ------------- Plot focal mechanism
            if (i .eq. 1) then
               call xmovabs(x(ii),x(ii+1))
            else
               call xdrwabs(x(ii),x(ii+1))
            endif
            ii=ii+2
100      continue

c  make a stroke
c
          call xout(10.0,10.0)

1000  continue
         if=if-1
  
      call xset_color(color_def)
      RETURN
      END


c-----------------------------------------------------------------
      SUBROUTINE FCIRCL(R,X,Y)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  INPUT R - RADIUS
C        X - CENTER OF CIRCLE
C        Y - CENTER OF CIRCLE
C PURPOSE PLOTS A FILLED CIRCLE 
C
C R.N. Arvidsson 1990-05-04
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      implicit integer*2 (v,j-n)
      real  r, x, y, pi
      real ix(722), istat, idevh, ibc, istey, istex, ye(8)
      COMMON /BLCK7/ ISTAT, IDEVH, IBC, ISTEY, ISTEX, YE
      pi = 4.d0*asin(1.d0/(sqrt(2.d0)))
      ii=1
      do 100 i = 1, 361
c        ix(ii)  = x + r*dcos(i*pi/180.)
         ix(ii)  = x + r*cos(i*pi/180.)
c        ix(ii+1)= y + r*dsin(i*pi/180.)
         ix(ii+1)= y + r*sin(i*pi/180.)
c ---------- Plot circe
         if (ii .eq. 1) then
            call xmovabs(ix(ii),ix(ii+1))
         else
            call xdrwabs(ix(ii),ix(ii+1))
         endif
         ii=ii+2
100   continue
      return
      end




      subroutine draw_circle(x,y,r)
c
c  draws a circle with center x,y and radius r
c
      implicit none
      real x,y,x1,y1,r,z
      integer k,i

      call xmovabs(x+r,y)
	  k=100	 
	  z=6.28/k
      do i=1,k
         x1=r*cos(i*z)+x
         y1=r*sin(i*z)+y
         call xdrwabs(x1,y1)
      enddo
      return
      end

      subroutine draw_triangle(x,y,r)
c
c draw triangle inside circle of radius r
c lot 16.01.2002
c
      implicit none
      real x,y,r
      call xmovabs(x,y)
      call xdrwabs(x,y+r)
      call xdrwabs(x-r*sin(3.14/3.),y-r*cos(3.14/3.))
      call xdrwabs(x+r*sin(3.14/3.),y-r*cos(3.14/3.))
      call xdrwabs(x,y+r)

      return
      end





cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine fps_gap(npol,gap)
c
c   calculate gap in azimuth used by fps taking into account that direct rays
c   must have added 180 deg. weighted out phases are included
c   only works with polaritiees an donly requires an S-file
c
c  jh dec 2016
c   
c   input data comes from rea structure
c
c   gap is gap, npol is number of polarities
c

      implicit none
      include 'seidim.inc'
      include 'rea.inc'   
      include 'seisan.inc'       
      
      real az(2000)           ! azimuths available for fps
      integer isort(2000)     ! pointers for sorting
      integer npol            ! number of polarities  from P on Z
      real gap
      integer i

c
c   find number of polarities and az used in fps
c
      npol=0
      do i=1,rea_nphase
         if(rea_phase(i)(1:1).eq.'P'.and.rea_co(i)(2:2).eq.'Z'.
     *   and.(rea_polarity(i).eq.'D'.or.rea_polarity(i).eq.'C').and.
     *   rea_ain(i).gt.0.0) then
           npol=npol+1
           if(npol.gt.2000) then
              write(6,*)' more than 2000 polarities, stop'
              stop
           endif
           az(npol)=rea_az(i)
c
c   add 180 deg to az from direct waves
c
           if(rea_ain(i).gt.90.0) then
               az(npol)=az(npol)+180.0
               if(az(npol).gt.360.0) az(npol)=az(npol)-360.0
           endif
         endif
      enddo
c
c  sort the az
c
      call r4sort(npol,az,isort)
c     do i=1,npol
c        write(6,*) isort(i)
c        write(6,*) az(isort(i))
c      enddo
c
c   find max gap, add one value to represent
c   gap between smallest and largest value
c
      isort(npol+1)=npol+1
      az(npol+1)=az(isort(1))+360.0  ! add first value to complete                          
      
      gap=0.0
      do i=1,npol
         if(az(isort(i+1))-az(isort(i)).gt.gap) 
     *   gap=az(isort(i+1))-az(isort(i))
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine fps_gap_all(nobs,gap,gap_ain)
c
c   calculate gap in azimuth used by fps taking into account that direct rays
c   must have added 180 deg. also calculates gap in ain with all converted to
c   range 0-90 
c
c   amplitude ratios are also counted
c   the routine uses focmec.dat as input so foc_prepare must be called first
c
c  jh dec 2016
c   
c   input data comes from rea structure
c
c   gap is gap, nobs is number of polarities and amplitude ratios
c

      implicit none

      real az(5000)           ! azimuths available for fps
      real ain(5000)          ! ain ---------------------
      integer isort(5000)     ! pointers for sorting
      integer nobs            ! number of polarities and amplitude ratios
      character*80 text
      real gap,gap_ain
      integer i

      nobs=0
c
c   read focmec.dat
c
      open(88,file='focmec.dat',status='old',err=10)
      goto 11
 10   continue
      return

 11   continue
c
c   read first header line
c
      read(88,'(a)',end=10) text

 12   continue
      nobs=nobs+1
      read(88,'(6x,f6.2,f8.2)',end=13) az(nobs),ain(nobs)
c
c   add 180 deg to az from direct waves
c
      if(ain(nobs).gt.90.0) then
         az(nobs)=az(nobs)+180.0
         if(az(nobs).gt.360.0) az(nobs)=az(nobs)-360.0
c
c   also  change ain
c
         ain(nobs)=ain(nobs)-90.0
      endif
      goto 12

 13   continue
      close(88)
      nobs=nobs-1
c
c  sort the az
c
      call r4sort(nobs,az,isort)
c      do i=1,nobs
c        write(6,*) isort(i),az(isort(i)),ain(isort(i))
c      enddo
c
c   find max az gap, add one value to represent
c   gap between smallest and largest value
c
      isort(nobs+1)=nobs+1
      az(nobs+1)=az(isort(1))+360.0  ! add first value to complete                          
      
      gap=0.0

      do i=1,nobs
         if(az(isort(i+1))-az(isort(i)).gt.gap) 
     *   gap=az(isort(i+1))-az(isort(i))
      enddo

c
c  sort the ain
c
c
c   add two values to represent zero and 90
c
      ain(nobs+1)=0.0
      ain(nobs+2)=90.0
      
      call r4sort(nobs+2,ain,isort)

c      do i=1,nobs+2
c        write(6,*) isort(i),az(isort(i)),ain(isort(i))
c      enddo
c
c   find max ain gap
c                            
      
      gap_ain=0.0
      do i=1,nobs+1
         if(ain(isort(i+1))-ain(isort(i)).gt.gap_ain) 
     *   gap_ain=ain(isort(i+1))-ain(isort(i))
      enddo

      return
      end
   
         subroutine  plot_pol(az,ain,pol,npol,x0,y0,r,color_symbol)
c
c   plot polarities  for a fault plane
c
c     az,ain, pol: azimuth, angle of incidence, polarity
c     x0,y0,r: canter of circle with radiuns r
c     color_symbol: color of polarity 
c
c      x0,y0,r: center and radius
c      color_symbol: symbol color
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      implicit none
      include 'seiplot.inc'
c
c    Libsei details...
c    =================
c
      include 'libsei.inc'                 ! Library definitions & data defns.
      integer color_symbol

      real X0, Y0,ax, R

      
      LOGICAL AN, PT, RIGHT, MT
      real az(*),ain(*)
      real x,y
      character*1 pol(*)                  ! polarities
      integer npol                        ! number of polarities
      real pi
      integer i

c
c    Initialise...
c    =============

c
c
C ---
      PI = 4.*atan(1.)
C ---
      
c
      call xset_color(color_symbol)



       do i=1,npol

C ------- calculate sterographic projection of a point

           if ( ain(i) .gt. 90.0 ) then
                ain(i) = 180.0 - ain(i)
                az(i)    = 180.0 + az(i)
           endif
           ain(i) = pi/2.0 - ain(i)*pi/180.0
           az(i)    = az(i)*pi/180.0

            ax= r*tan(pi/4.0-ain(i)/2.0)
            x = ax*sin(az(i)) + x0
            y = ax*cos(az(i)) + y0 
c
c   plot
            if ( pol(i) .eq. 'C' ) then
                call draw_circle(x,y,6.0)
            endif
c
c   for fill, make smaller indicated by small c
c
            if ( pol(i) .eq. 'c' ) then
                 call draw_triangle(x,y,1.5)
c                call draw_circle(x,y,3.0)
c                call draw_circle(x,y,2.0)
             endif

            if ( pol(i) .eq. 'D' ) then
                call draw_triangle(x,y,6.)  
            endif
      enddo

      call xset_color(color_def)
      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_fps_pol(strike,dip,rake,npol,az,ain,pol)

c
c    calculate polarities for a given solution to be used for filling out the 
c    beach balls. 
c

c     input: strike,dip rake : fps solution
c     output: npol polarities at az azimuth, ain angle of incidence and polarity
c             pol. On C is indicated and a a smam c is used to indicate
c             thsi is for fill out only
c
      real xyz(9),xyzden(6)    ! help variables, see focmec subroutines
      real lograt(3)           ! log amplitude ratio for sv/p,sh/p, xx/xx
      real bot(3)              ! p-amplitude with sign
      character*3 flag

      real strike,dip,rake,rd
      character*1 pol(*)
      real az(*),ain(*)
      RD = 45.0/ATAN(1.0)

c
c  setup table of angles
c
       m=0
       do k=2,88,7
          n=10
          n=(10-(k/9)+4)
          do i=1,360,n
             m=m+1
             az(m)=i
             ain(m)=k
          enddo    
       enddo

       npol=m

       do i=1,npol            
c
c   calcualate polarities using part of subroutine from focmec
c    


cold       	  TREND = AZIN/RD
cold	  PLUNGE = (90.0 - PTOANG)/RD 
cold	  COST = COS(TREND)
cold      SINT = SIN(TREND)
cold	  COSP = COS(PLUNGE)
cold	  SINP = SIN(PLUNGE)
cold	  XYZ(1) = COST*COSP
cold	  XYZ(2) = SINT*COSP
cold
cold	  XYZ(3) = SINP
cold	  SPLUNG = (90.0 - STOANG)/RD
cold	  SINP = SIN(SPLUNG)
cold	  COSP = COS(SPLUNG)
C
C	Next two vectors reversed in sign from normal convention because
C       of my (snoke) convention for SV and SH (down and left, facing the station)
C
cold	  XYZ(4) = -COST*SINP
cold	  XYZ(5) = -SINT*SINP
cold	  XYZ(6) = +COSP
cold	  XYZ(7) = SINT
cold          XYZ(8) = -COST
cold	  XYZ(9) = 0.0   


        toang1=ain(i)
        toang2=ain(i)
        azin=az(i)

        TREND = AZIN/RD
        toa = TOANG1/RD 
        COST = COS(TREND)
        SINT = SIN(TREND)
        sinP = sin(toa)
        cosP = cos(toa)
        XYZ(1) = COST*sinp
        XYZ(2) = SINT*sinP
c   Positive z is down
        XYZ(3) = cosP
C  Next two vectors reversed in sign from A&R convention because
C   of my convention for SV and SH (down and left, facing the station)
        XYZ(4) = -COST*cosP
        XYZ(5) = -SINT*cosP
        XYZ(6) = sinp 
        XYZ(7) = SINT
        XYZ(8) = -COST
        XYZ(9) = 0.0
c        IF (SENSE.EQ.'V' .OR. SENSE.EQ.'S' .OR. SENSE.EQ.'H') THEN
            toa = toang2/rd
            sinP = sin(toa)
            cosP = cos(toa)
            XYZden(1) = COST*sinp
            XYZden(2) = SINT*sinP
            XYZden(3) = cosP
            XYZden(4) = -COST*cosP
            XYZden(5) = -SINT*cosP
            XYZden(6) = sinp
c        endif


       
          azin=az(i)
          ptoang=ain(i)
          stoang=ain(i)

       	  TREND = AZIN/RD
	  PLUNGE = (90.0 - PTOANG)/RD 
	  COST = COS(TREND)
	  SINT = SIN(TREND)
	  COSP = COS(PLUNGE)
	  SINP = SIN(PLUNGE)
	  XYZ(1) = COST*COSP
	  XYZ(2) = SINT*COSP

	  XYZ(3) = SINP
	  SPLUNG = (90.0 - STOANG)/RD
	  SINP = SIN(SPLUNG)
	  COSP = COS(SPLUNG)


   

          vpvs3=5.26
          dp1=dip/rd
          st1=strike/rd
          ra1=rake/rd     

          call LRATIO(2,DP1,ST1,RA1,XYZ,xyzden,VPVS3,
     *    LOGRAT(2),TOP,bot(2),CUTP,CUTS,FLAG)
c
c  polarity
c
             pol(i)=' '
             if(bot(2).lt.0.0) then
                pol(i)=' ' ! do not plot
             else
                pol(i)='c' ! samll c for fill
             endif
         enddo
     
      return
      end
      
