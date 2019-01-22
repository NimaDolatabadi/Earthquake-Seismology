c************************************************************************        
       subroutine delip(m,n,np,res,test,smajax,sminax,thetadeg,
     &distind,iflagd,c_lat,std_1)
c
c   subroutine to calculated 
c
c
c            Yochai Ben Horin 1998 
c The covariance matrix is normalized with statistic for n+1 degrees of 
c freedom, the first step is to change the normalization to 2 degrees of 
c freedom and to move from calculation in degrees to km by multipyling
c with the radius of the earth 6378km  then performing rotation on the 2x2
c matrix in order to get diagonal matrix.
c
c
c   m -> number of columns in the SVD matrix
c   n -> number of rows in the SVD matrix
c   np-> number of eigenvectors used in subroutine corr (hyposub2.for)
c   res-> squared weighted sum
c   c_lat->Latitude
c   output
c   smajax, sminax and thetadeg
c  reference: "Confidence Regions and Error Determinations for Seismic Event 
c            Event Location" E.A. Flinn, Reviews of Geophysics Vol.3 1965. 
c             
C
c
c************************************************************************
c      
c     implicit none
      include 'hypparm.inc'
      include 'libsei.inc'
c     include 'comm1.inc'
c     dimension test(1)
      real pi,twopi,halfpi,rad,deg,syy,sxx,sxy,theta
      real smajax,smajaxt,sminax,sminaxt,thetadeg
      real c,si,cc,sisi,rearth,rearth2
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (rearth=6378.)
c************************************************************************
c replacing the F-distribution for np+1 degrees of freedom with F-dist 
c************************************************************************
c local event degs 
      degfb=test(86)    !changed test(87) to test(86) 6/98 BRL
      errb=test(85)
c distant case
      if(distind.eq.'D'.or.iflagd.eq.1)then  !changed 6/98, brl
       degfb=test(92)
       errb=test(91)
      endif
      ndeg=int(degfb+0.499999) !this is K IDC use 99999
c this part reproduce the statistical parameters which were used in the corr function in order
c to normelize the covariance matrix 
      cres=(degfb*errb**2+res)/(degfb+float(n)-float(np+1))
      rs=float(np+1)*cres*fdist(np+1,ndeg+n-np-1,test(87))
c************************************************************************       
c  Statistical factors for two dimention ellipse
      cres2=(degfb*errb**2+res)/(degfb+float(n)-float(2))
      rs2=float(2)*cres2*fdist(2,ndeg+n-2,test(87))
c************************************************************************       
c     write(6,*)var(2,2),var(1,1),var(1,2)
      rearth2=rearth*rearth
      rearthx=rearth*cos(c_lat*rad) ! from epicenter to rotation axis
      syy=var(2,2)*rearth2/rs !deg->km; removing of statistical factors
      sxx=var(1,1)*rearthx*rearthx/rs
      sxy=var(1,2)*rearth*rearthx/rs
      szz=var(3,3)*rearth*rearth/rs
      szy=var(3,2)*rearth*rearth/rs
      szx=var(3,1)*rearth*rearthx/rs
      std_1(1,1)=sqrt(sxx*rs2) !std_1(1,#) is the errors WITH statistics IN (2d)
      std_1(1,2)=sqrt(syy*rs2)
      std_1(1,3)=sqrt(szz*rs2)
      std_1(1,4)=szy*rs2
      std_1(1,5)=szx*rs2
      std_1(1,6)=sxy*rs2
      std_1(2,1)=sqrt(sxx)  !std_1(2,#) is the errors WITHOUT statistics IN (2d)
      std_1(2,2)=sqrt(syy)
      std_1(2,3)=sqrt(szz)
      std_1(2,4)=szy
      std_1(2,5)=szx
      std_1(2,6)=sxy
c set error to 999.9
      do i=1,2
        do j=1,3
           if(std_1(i,j).gt.999.9)std_1(i,j)=999.9
           if(std_1(i,j).eq.0.0)std_1(i,j)=999.9
        enddo
      enddo
      if(m.eq.2)std_1(1,3)=0.0
      if(m.eq.2)std_1(2,3)=0.0
c
	theta=0.5*atan2(2.0*sxy,syy-sxx) !calculating rotation angle
	c=cos(theta)
	si=sin(theta)
	cc=c*c
	sisi=si*si
	smajaxt=sxx*sisi+2.0*sxy*si*c+syy*cc !calculating eigenvalues
	sminaxt=sxx*cc-2.0*sxy*si*c+syy*sisi
      if(smajaxt.lt.0.0.or.sminaxt.lt.0.0)goto22
	smajax=sqrt(smajaxt*rs2) ! axis  including statistical factor for 2-d
	sminax=sqrt(sminaxt*rs2)
	if(theta.lt.0.0) theta=theta+twopi
	if(theta.gt.twopi) theta=theta-twopi
	if(theta.gt.pi) theta=theta-pi
	thetadeg=deg*theta
	goto 23
  22    smajax=-1.
	sminax=-1.
	thetadeg=-1.
  23    return
        end
