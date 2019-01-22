c
c
c                                 old focmec
c
c                all subroutines and include blocks here
c                          nothing in LIB or INC
c                         
C+
C	PROGRAM FOCMEC
C
C	Calculates acceptable focal mechanisms based on polarities
C	  and (S/P) amplitude ratios.
C	Inspired by the Georgia Tech program with statistics
C	  from Kisslinger's.  Conventions are those in
C	  Herrmann as well as Aki & Richards
C	Updates are documented in a separate file.
C
C	Arthur Snoke   Virginia Tech  1984, 2001
C-
c
c
        write(6,*)'OLD FOCMEC MAIN'
	CALL FOCINP
	CALL SRCHFM
	STOP
	END


C+
c
c      changes
c      
c   nov 11 08 jh : put in new oksol from snoke, focinp also changed
c   march 11 2009 jh :  comment out max_sol_old
c   2014-03-10 pv : cleanup due to warnings compiling under Solaris
c   2014-04-14 pv : changed DIMENSION to * in SUBROUTINE GMPRD(A,B,R,N,M,L)
c   2014-04-29 pv : cleanup due to warnings compiling under Solaris
c   2016-07-19 jh : in theoretical ratio, add bottom amplitude to
c                   lratio used to calculate synthetic input values.
c   2017 11 26 jh : put in include block to save thsi program as an old version
c
C	SUBROUTINE AN2DSR(A,N,ANGS,PI)
C
C	Calculates dip, strike and rake (ANGS) - A&R convention,
C		from A and N.
C	12 January 2000:  Fixed a divide by zero when angs(1) .eq. 0
C	1 October 2001: When porting to the PC, there were roundoff
C		errors when acos was near its limits.
C-
	SUBROUTINE AN2DSR(A,N,ANGS,PI)
	REAL N(3),A(3),ANGS(3)
	if (N(3) .eq. -1.0) then
	  angs(2) = atan2(a(2),a(1))
	  angs(1) = 0.0
	else
	  ANGS(2) = ATAN2(-N(1),N(2))
	  if (N(3) .eq. 0.0) then
	    angs(1) = 0.5*PI
	  else IF (ABS(SIN(ANGS(2))) .ge. 0.1) then
	    ANGS(1) = ATAN2(-N(1)/SIN(ANGS(2)),-N(3))
	  else
	    ANGS(1) = ATAN2(N(2)/COS(ANGS(2)),-N(3))
	  end if
	end if
	A1 = A(1)*COS(ANGS(2)) + A(2)*SIN(ANGS(2))
	if (abs(a1) .lt. 0.0001) a1 = 0.0
	if (a(3) .ne. 0.0) then
	  if (angs(1) .ne. 0.0) then
	    ANGS(3) = ATAN2(-A(3)/SIN(ANGS(1)),A1)
	  else
	    ANGS(3) = atan2(-1000000.0*A(3),A1)
	  end if
	else
	  a2 = a(1)*sin(angs(2)) - a(2)*cos(angs(2))
	  if (abs(a2) .lt. 0.0001) a2 = 0.0
	  if (abs(sin(2*angs(2))) .ge. 0.0001) then
	    angs(3) = atan2(a2/sin(2*angs(2)),a1)
	  else if (abs(sin(angs(2))) .ge. 0.0001) then
	    acosarg = amin1(1.0,amax1(-1.0,a(2)/sin(angs(2))))
	    angs(3) = acos(acosarg)
	  else
	    acosarg = amin1(1.0,amax1(-1.0,a1))
	    angs(3) = acos(a1)
	  end if
	end if
	IF (ANGS(1) .lt. 0.0) then
	  ANGS(1) = ANGS(1) + PI
	  ANGS(3) = PI - ANGS(3)
	  IF (ANGS(3) .GT. PI) ANGS(3) = ANGS(3) - 2*PI
	end if
	IF(ANGS(1) .gt. 0.5*PI) then
	  ANGS(1)=PI-ANGS(1)
	  ANGS(2)=ANGS(2)+PI
	  ANGS(3)=-ANGS(3)
	  IF (ANGS(2) .GE. 2*PI) ANGS(2) = ANGS(2) - 2*PI
	end if
	IF (ANGS(2) .LT. 0.0) ANGS(2) = ANGS(2) + 2.0*PI
	RETURN
	END
C+
	SUBROUTINE AN2MOM(A,N,MOMTEN)
C
C	Starting with the A and N axis, calculates the elements
C	  of the moment tensor with unit scalar moment.
C	  Coordinate system:  X = North, Y = East, Z = Down
C	  Convention used is that of Dziewonski & Woodhouse
C	  (JGR 88, 3247-3271, 1983) and Aki & Richards (p 118)
C	24 September 1985: If an element is < 0.000001 (ABS), set to zero
C-
	REAL*4 A(3), N(3), MOMTEN(6)
C	      Moment tensor components:  M(I,j) = A(I)*N(J)+A(J)*N(I)
	MOMTEN(1) = 2.0*A(3)*N(3)	!  MRR = M(3,3)
	MOMTEN(2) = 2.0*A(1)*N(1)	!  MTT = M(1,1)
	MOMTEN(3) = 2.0*A(2)*N(2)	!  MPP = M(2,2)
	MOMTEN(4) = A(1)*N(3)+A(3)*N(1)	!  MRT = M(1,3)
	MOMTEN(5) = -A(2)*N(3)-A(3)*N(2)!  MRP = -M(2,3)
	MOMTEN(6) = -A(2)*N(1)-A(1)*N(2)!  MTP = -M(2,1)
	DO 100 J=1,6
	  IF (ABS(MOMTEN(J)) .LT. 0.000001) MOMTEN(J) = 0.0
100	CONTINUE
	RETURN
	END
C+
	SUBROUTINE ANTPIN (ANBTP,ANGS,ANGS2,PTTP,MOMTEN,PI)
C
C	Calculates other representations of fault planes with
C		trend and plunge of A and N as input.  All
C		angles are in radians.
C	22 July 1985:  Added moment tensor output.
C-
	REAL N(3), MOMTEN(6)
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6),P(3),T(3),A(3),B(3)
	DATA SR2/0.707107/
	CALL TRPL2V(ANBTP(1),A)
	CALL TRPL2V(ANBTP(3),N)
	DO 100 J=1,3
	  T(J) = SR2*(A(J) + N(J))
	  P(J) = SR2*(A(J) - N(J))
100	CONTINUE
	B(1) = P(2)*T(3) - P(3)*T(2)
	B(2) = P(3)*T(1) - P(1)*T(3)
	B(3) = P(1)*T(2) - P(2)*T(1)
	CALL V2TRPL(P,PTTP(1),PI)
	CALL V2TRPL(T,PTTP(3),PI)
	CALL V2TRPL(B,ANBTP(5),PI)
	CALL AN2DSR(A,N,ANGS,PI)
	CALL AN2DSR(N,A,ANGS2,PI)
	CALL AN2MOM(A,N,MOMTEN)
	RETURN
	END
C+
	subroutine cstring(string,nstring)
C
C	Input a character string with a read(*,'(A)') string
C	If first two characters are /* it will read the next entry
C	Tab is a delimiter.
C	Returns string and nstring, number of characters to tab.
C	string stars with first non-blank character.
C       25 May 2001.  Took out parameter statement for tab.
C-
	logical more
	CHARACTER*1 TAB
	CHARACTER*(*) string
C
	tab = char(9)
	more = .true.
	do while (more)
	  read(*,'(A)') string
	  nstring = lenc(string)
	  more = (nstring.ge.2 .and. string(1:2).eq.'/*')
	end do
	IF (nstring .GT. 0) THEN
	  NTAB = INDEX(string(1:nstring),TAB)
	  IF (NTAB .GT. 0) nstring = NTAB - 1
	end if
	return
	end
C+
	character*(*) function cvalue(msg,default,nout)
C
C	MSG gets printed on screen prompting for a character string.
C	DEFAULT is the default string.  NOUT is the number of characters
C	  returned in cvalue.
C	A tab in the input acts as a terminator.
C       27 July 1993: Did input read through cstring so can have 
C         comment lines
C	25 May 2001.  Took out parameter statement for tab.
C-
	character*1 tab
	character*(*) msg,default
	character*80 input
C
        tab = char(9)
	call printx(msg)
	call cstring(input,nout)
c  Kill leading blanks
        do while (nout.gt.0 .and. input(1:1).eq.' ')
          nout = nout - 1
          input(1:nout) = input(2:nout+1)
          input(nout+1:nout+1) = ' '
        end do
	if (nout .eq. 0) then
	  nout = lenc(default)
	  cvalue = default(1:nout)
	else
	  cvalue = input(1:nout)
	end if
	return
	end
C+
	SUBROUTINE DSRIN (ANGS,ANBTP,ANGS2,PTTP,MOMTEN,PI)
C
C	Calculates other representations of fault planes with
C		dip, strike and rake (A&R convinention) input.  All
C		angles are in radians.
C	22 July 1985:  Added moment tensor output (D&W convention)
C	               normalized to unit scalar moment
C	1 October 2001: When ported to the PC, on one compiler there
C		was a roundoff problem for limiting cases.  Included
C		a fix.
C-
	REAL N(3), MOMTEN(6)
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6),P(3),T(3),A(3),B(3)
	DATA SR2/0.707107/
	RAKE=ANGS(3)
	STR = ANGS(2)
	DIP = ANGS(1)
	A(1) = COS(RAKE)*COS(STR) + SIN(RAKE)*COS(DIP)*SIN(STR)
	A(2) = COS(RAKE)*SIN(STR) - SIN(RAKE)*COS(DIP)*COS(STR)
	A(3) = -SIN(RAKE)*SIN(DIP)
	N(1) = -SIN(STR)*SIN(DIP)
	N(2) = COS(STR)*SIN(DIP)
	N(3) = -COS(DIP)
	do j=1,3
          if (abs(A(j)) .le. 0.0001) A(j) = 0.0
          IF ((ABS(A(j))-1.0) .gt. 0.0) A(j)=A(j)/abs(A(j))
          if (abs(N(j)) .le. 0.0001) N(j) = 0.0
          IF ((ABS(N(j))-1.0) .gt. 0.0) N(j)=N(j)/abs(N(j))
        end do
	CALL V2TRPL(A,ANBTP(1),PI)
	CALL V2TRPL(N,ANBTP(3),PI)
	DO 100 J=1,3
	T(J) = SR2*(A(J) + N(J))
100	P(J) = SR2*(A(J) - N(J))
	B(1) = P(2)*T(3) - P(3)*T(2)
	B(2) = P(3)*T(1) - P(1)*T(3)
	B(3) = P(1)*T(2) - P(2)*T(1)
	CALL V2TRPL(P,PTTP(1),PI)
	CALL V2TRPL(T,PTTP(3),PI)
	CALL V2TRPL(B,ANBTP(5),PI)
	CALL AN2DSR(N,A,ANGS2,PI)
	CALL AN2MOM(A,N,MOMTEN)
	RETURN
	END
C+
C	SUBROUTINE FLTSOL(A,N,BMATRX,PLUNGE,TREND,ANGLE,JA)
C
C	Called by SRCHFM, a subroutine of FOCMEC
C	Calculates the A and N vectors (Herrmann's X and Y)
C		for a given trend and plunge for the B axis
C		and  an ANGLE relative to a reference.
C
C	Arthur Snoke  Virginia Tech  May 1984
C-
	SUBROUTINE FLTSOL(A,N,BMATRX,PLUNGE,TREND,ANGLE,JA)

c	INCLUDE 'FOCMEC.INC'

C	FOCMEC.INC
C
C	INCLUDE file for FOCMEC
C	27 August 1991 for the sun now includes SV
C	  polarities and SV/SH ratios
C	2 August 1993: changed ratio evaluations near nodal planes,
C	     so left out RLOW* and RHI* and replaced CUT with CUTP
C	     and CUTS.
C	2 June 2000: Increased MAX to 2000 and made it so only one version
C		appears with the distribution (others are virtual links).
C	NOTE! For some compilers, the ! starting an on-line comment may cause
C		an error.  Simply delete the ends of line starting at the !
C		if that is the case.
C-
c  oct 2010     jh: increase max to 5000
c
	PARAMETER (MAX = 5000)
	PARAMETER (MAX2 = 2*MAX)
C
C	Input Data
C
	REAL*4 XYZ(9,MAX2)	!  R, Theta, and Phi station coordinates 
	REAL*4 PSTATN(MAX)	!  Stations with polarity data
	REAL*4 RSTATN(MAX)	!  Stations with amplitude-ratio data
C				     X = north, Y east, Z down
	INTEGER POLRTY(MAX)	!  1 for C, F, or <;  -1 for D, B, or >
	INTEGER NPOL		!  Number of Polarities read in
	INTEGER KEYPOL(MAX)	!  Keys polarity data relative to input
	REAL*4 LOGRAT(MAX)	!  Log10 of (SV/P), (SH/P), or (SV/SH)
C				   including free surface correction, etc.
	INTEGER NRAT		!  Number of ratios read in
	INTEGER KEYRAT(MAX)	!  Keys ratios data relative to input
	CHARACTER*1 SVSH(2,MAX)	! First element V for SV or H for SH,
C				  Second element polarity: F or B for SV
C				  L or R for SH (back to station) for
C	                          numerator in SV/P, SH/P, or SV/SH ratio
C
C	  Processing things
C
	INTEGER MAXSOL	!  Exit after this many accept. aols.
	REAL*4 ERR		!  Total allowed number of errors
	REAL*4 ERRP		!  Allowed number of P polarity errors
	REAL*4 ERRSV		!  Allowed number of SV polarity errors
	REAL*4 ERRSH		!  Allowed number of SH polarity errors
	REAL*4 THRESH		!  Polarity weighting threshold
C				  (1.0 if no weighting)
	REAL*4 BADPP(MAX)	!  Stations with a P polarity error
	REAL*4 BADSVP(MAX)	!  Stations with a SV polarity error
	REAL*4 BADSHP(MAX)	!  Stations with a SH polarity error
	REAL*4 WBADP(MAX)	!  Weishts of P polarity errors
	REAL*4 WBADSV(MAX)	!  Weights of SV polarity errors
	REAL*4 WBADSH(MAX)	!  Weights of SH polarity errors
	REAL*4 BADP		!  Total weight of P polarity errors
	REAL*4 BADSV		!  Total weight of SV polarity errors
	REAL*4 BADSH		!  Total weight of SH polarity errors
	INTEGER NERRR		!  Allowed number of ratio errors
	REAL*4 ERRRAT		!  Maximum allowed Log10 ratio error
	REAL*4 CALRAT(MAX)	!  Theoretical Log10 of amplitude ratio
	INTEGER WTRAT(MAX)	!  1 if that ratio used, 0 otherwise
	REAL*4 VPVS3		!  Cube of P/S velocity ratio 
	REAL*4 BTMIN,BTDEL,BTMAX	!  Search range in B axis trend
	REAL*4 BPMIN,BPDEL,BPMAX	!  Search range in B axis plunge
	REAL*4 AAMIN,AADEL,AAMAX	!  Search range in A axis angle
C					   measured from B trend
	REAL*4 CUTP		!  cutoff limit for  P radiation factor
C				   in amplitude ratios
	REAL*4 CUTS		!  cutoff limit for  S radiation factor
C				   in amplitude ratios
C
	COMMON /FOCMEC/ PSTATN,RSTATN,XYZ,RD,POLRTY,NPOL,LOGRAT,
     1	  NRAT,MAXSOL,ERRP,ERRSV,NERRR,ERRRAT,CALRAT,WTRAT,VPVS3,
     2	  BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,AAMAX,
     3	  BADPP,KEYRAT,KEYPOL,ERRSH,BADSHP,ERR,THRESH,WBADP,WBADSH,
     4    WBADSV,BADSVP,CUTP,CUTS,BADP,BADSV,BADSH
	COMMON /FMCHAR/ SVSH

	REAL*4 A(3), N(3), BMATRX(3,3),ZROT(3,3),YROT(3,3),ANZ(3,3)
	IF (JA .EQ. 1) THEN
		DO 100 J=1,3
		DO 100 K=1,3
		ZROT(J,K) = 0.0
100		YROT(J,K) = 0.0
C
C	Construct a rotation matrix to map X=North, Y=East,
C		Z=Down into A, N, B
C
C	First rotate about Z (= Down) through an angle TREND.
C		This rotates X into the trend of B
	ZROT(1,1) = COS(TREND)
	ZROT(2,2) = ZROT(1,1)
	ZROT(1,2) = SIN(TREND)
	ZROT(2,1) = -ZROT(1,2)
	ZROT(3,3) = 1.0
C
C	Now rotate about Y through an angle 90-PLUNGE.
C		This rotates the Z axis into B.
C
	YROT(1,1) = SIN(PLUNGE)
	YROT(3,3) = YROT(1,1)
	YROT(3,1) = COS(PLUNGE)
	YROT(1,3) = -YROT(3,1)
	YROT(2,2) = 1.0
C
C	BMATRX is the product of YROT and ZROT
C
	CALL GMPRD(YROT,ZROT,BMATRX,3,3,3)
C
C	BMATRX does not change as ANGLE changes
C
	ENDIF
C
C	Rotate about Z (= B) through an angle ANGLE.
C		This rotates X into A and N into Y.
C	Inverse of matrix rotating by ANGLE about B
C
C
	ZROT(1,1) = COS(ANGLE)
	ZROT(2,2) = ZROT(1,1)
	ZROT(1,2) = SIN(ANGLE)
	ZROT(2,1) = -ZROT(1,2)
	ZROT(3,3) = 1.0
C
C	ANZ is the product of ZROT and BMATRX
C
	CALL GMPRD(ZROT,BMATRX,ANZ,3,3,3)
	DO 200 J=1,3
	A(J) = ANZ(1,J)		!  A is 1st row of ANZ
200	N(J) = ANZ(2,J)		!  N is 2nd row of ANZ
	RETURN
	END
C+
	SUBROUTINE FMREPS(ANBTP,ANGS,PTTP,ANGS2,AN,PT,DSR,mt,MOMTEN,
     .	  LUNIT1,LUNIT2)
C
C	Input: A and N (trned and plunge), or P and T or dip, strike
C	  and rake (A&R convention) or moment tensor (A&R)
C	Output: the other representations plus the auxiliary plane.
C	PTTP:  4 parameters, trend and plunge for P and T
C	  P trend and plunge, T trend and plunge
C	ANBTP  6 parameters, t and p for A, N and B respectively.
C	ANGS   3 parameters,  dip, strike and rake for first plane
C	ANGS2  3 parameters, dip strike and rake for auxiliary plane
C	AN, PT, DSR are LOGICAL variables which are true if
C	  A and N, P and T or dip-strike-rake are input
C	MOMTEN  6 parameters:  the moment tensor (unit scalar
C	  magnitude) D & W convention
C	Angles come in and go out in degrees.
C	If LUNIT1 and/or LUNIT2 are positive, the representations
C	  are written on those logical unit numbers.
C	22 July 1985:  Added moment tensor output
C	30 October 1992:  Fixed up problem if lunit2=5.
C	15 March 2002:  Added moment-tensor input, changed MFF to MPP, etc.
C-
	LOGICAL AN,PT,DSR,MT
	REAL*4 MOMTEN(6)
	INTEGER LUNIT1, LUNIT2
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6)
	RDEG = 45.0/ATAN(1.0)
	PI = 4.0*ATAN(1.0)
	IF (MT) then
	  call mt_in(PTTP,PI)
	  CALL PTTPIN(PTTP,ANGS,ANGS2,ANBTP,MOMTEN,PI)
	else IF (PT) THEN
	  DO 100 J=1,4
100	  PTTP(J) = PTTP(J)/RDEG
	  CALL PTTPIN(PTTP,ANGS,ANGS2,ANBTP,MOMTEN,PI)
	ELSE IF (AN) THEN
	  DO 200 J=1,4
200	  ANBTP(J) = ANBTP(J)/RDEG
	  CALL ANTPIN(ANBTP,ANGS,ANGS2,PTTP,MOMTEN,PI)
	ELSE if (DSR) then
	  DO 300 J=1,3
300	  ANGS(J) = ANGS(J)/RDEG
	  CALL DSRIN(ANGS,ANBTP,ANGS2,PTTP,MOMTEN,PI) 
	END IF
	DO 400 I=1,3
	  ANGS(I) = ANGS(I)*RDEG
	  ANGS2(I) = ANGS2(I)*RDEG
	  PTTP(I) = PTTP(I)*RDEG
  	  ANBTP(I) = ANBTP(I)*RDEG
400	CONTINUE
	ANBTP(4) = ANBTP(4)*RDEG
	ANBTP(5) = ANBTP(5)*RDEG
	ANBTP(6) = ANBTP(6)*RDEG
	PTTP(4) = PTTP(4)*RDEG
	IF (LUNIT1 .GT. 0) THEN
	  WRITE (LUNIT1,1) (ANGS(I),I=1,3)
	  WRITE(LUNIT1,2)(ANGS2(I),I=1,3),'   Auxiliary Plane'
	  WRITE (LUNIT1,3) (ANBTP(J),J=1,4)
	  WRITE(LUNIT1,4) (ANBTP(J),J=5,6)
	  WRITE(LUNIT1,5) PTTP
	  WRITE(LUNIT1,6) MOMTEN
	END IF
	IF (LUNIT2 .GT. 0) THEN
	  if (lunit2 .eq. 5) then
	    WRITE (*,1) (ANGS(I),I=1,3)
	    WRITE(*,2)(ANGS2(I),I=1,3),'   Auxiliary Plane'
	    WRITE (*,3) (ANBTP(J),J=1,4)
	    WRITE(*,4) (ANBTP(J),J=5,6)
	    WRITE(*,5) PTTP
	    WRITE(*,6) MOMTEN
	  else
	    WRITE (LUNIT2,1) (ANGS(I),I=1,3)
	    WRITE(LUNIT2,2)(ANGS2(I),I=1,3),'   Auxiliary Plane'  
	    WRITE (LUNIT2,3) (ANBTP(J),J=1,4)
	    WRITE(LUNIT2,4) (ANBTP(J),J=5,6)
	    WRITE(LUNIT2,5) PTTP
	    WRITE(LUNIT2,6) MOMTEN
	  end if
	END IF
	RETURN
C
1	FORMAT(5X,'Dip,Strike,Rake ',3F9.2)
2	FORMAT(5X,'Dip,Strike,Rake ',3F9.2,A)
3	FORMAT(5X,'Lower Hem. Trend, Plunge of A,N ',4F9.2)
4	FORMAT(5X,'Lower Hem. Trend & Plunge of B ',2F9.2)
5	FORMAT(5X,'Lower Hem. Trend, Plunge of P,T ',4F9.2)
6	FORMAT(5X,'MRR =',F5.2,'  MTT =',F5.2,'  MPP =',F5.2,
     +  '  MRT =',F5.2,'  MRP =',F5.2,'  MTP =',F5.2)
	END

C+
	SUBROUTINE FOCINP
C
C	Input routine for FOCMEC
C
C	Arthur Snoke  Virginia Tech  May 1984
C	Last perturbed 12 October 1990
C	20 August 1991:  sun version.  call assign replaced by open
C	31 August:  expanded to include SV polarities and SV/SH ratios
C	15 May 1992:  Changed name of listing file from lp.lst to 
C		focmec.lst
C	19 June 1993: Separate P and S errors should now work
C	20 Jun 1993:  Belts and braces:  If someone uses a ratio with
C	  no input S take-off angle, now sets it equal to P angle
C	2 August 1993:  Changed rules for ratios near nodal surfaces
C	6 January 1997:  format change
C	7 April 2000: sense='R' no longer allowed for ratios.  First TOANG
C	  is not necessarily for P (if an SH or SV polarity is to be used)
C	1 October 2001: When porting to a PC, found some compilers could
C		not handle reading in an integer for a floating-pint
C		variable (number of polarity errors), so "fixed" it.
C	26 March 2002: If emergent polarity data are included, now it
C		will ask if you want to include it.  Previously it 
C		ignored such data.
C	5 July 2002: Some compilers do not like the way I had used
C	  CVALUE -- CVALUE(1:NFILE) with NFILE defined on the right-hand side.
c	19 November 2008: If INFO was left out for a ratio, did not read line.
C		Now it does.
C-
c	INCLUDE 'FOCMEC.INC'


C	FOCMEC.INC
C
C	INCLUDE file for FOCMEC
C	27 August 1991 for the sun now includes SV
C	  polarities and SV/SH ratios
C	2 August 1993: changed ratio evaluations near nodal planes,
C	     so left out RLOW* and RHI* and replaced CUT with CUTP
C	     and CUTS.
C	2 June 2000: Increased MAX to 2000 and made it so only one version
C		appears with the distribution (others are virtual links).
C	NOTE! For some compilers, the ! starting an on-line comment may cause
C		an error.  Simply delete the ends of line starting at the !
C		if that is the case.
C-
c  oct 2010     jh: increase max to 5000
c
	PARAMETER (MAX = 5000)
	PARAMETER (MAX2 = 2*MAX)
C
C	Input Data
C
	REAL*4 XYZ(9,MAX2)	!  R, Theta, and Phi station coordinates 
	REAL*4 PSTATN(MAX)	!  Stations with polarity data
	REAL*4 RSTATN(MAX)	!  Stations with amplitude-ratio data
C				     X = north, Y east, Z down
	INTEGER POLRTY(MAX)	!  1 for C, F, or <;  -1 for D, B, or >
	INTEGER NPOL		!  Number of Polarities read in
	INTEGER KEYPOL(MAX)	!  Keys polarity data relative to input
	REAL*4 LOGRAT(MAX)	!  Log10 of (SV/P), (SH/P), or (SV/SH)
C				   including free surface correction, etc.
	INTEGER NRAT		!  Number of ratios read in
	INTEGER KEYRAT(MAX)	!  Keys ratios data relative to input
	CHARACTER*1 SVSH(2,MAX)	! First element V for SV or H for SH,
C				  Second element polarity: F or B for SV
C				  L or R for SH (back to station) for
C	                          numerator in SV/P, SH/P, or SV/SH ratio
C
C	  Processing things
C
	INTEGER MAXSOL	!  Exit after this many accept. aols.
	REAL*4 ERR		!  Total allowed number of errors
	REAL*4 ERRP		!  Allowed number of P polarity errors
	REAL*4 ERRSV		!  Allowed number of SV polarity errors
	REAL*4 ERRSH		!  Allowed number of SH polarity errors
	REAL*4 THRESH		!  Polarity weighting threshold
C				  (1.0 if no weighting)
	REAL*4 BADPP(MAX)	!  Stations with a P polarity error
	REAL*4 BADSVP(MAX)	!  Stations with a SV polarity error
	REAL*4 BADSHP(MAX)	!  Stations with a SH polarity error
	REAL*4 WBADP(MAX)	!  Weishts of P polarity errors
	REAL*4 WBADSV(MAX)	!  Weights of SV polarity errors
	REAL*4 WBADSH(MAX)	!  Weights of SH polarity errors
	REAL*4 BADP		!  Total weight of P polarity errors
	REAL*4 BADSV		!  Total weight of SV polarity errors
	REAL*4 BADSH		!  Total weight of SH polarity errors
	INTEGER NERRR		!  Allowed number of ratio errors
	REAL*4 ERRRAT		!  Maximum allowed Log10 ratio error
	REAL*4 CALRAT(MAX)	!  Theoretical Log10 of amplitude ratio
	INTEGER WTRAT(MAX)	!  1 if that ratio used, 0 otherwise
	REAL*4 VPVS3		!  Cube of P/S velocity ratio 
	REAL*4 BTMIN,BTDEL,BTMAX	!  Search range in B axis trend
	REAL*4 BPMIN,BPDEL,BPMAX	!  Search range in B axis plunge
	REAL*4 AAMIN,AADEL,AAMAX	!  Search range in A axis angle
C					   measured from B trend
	REAL*4 CUTP		!  cutoff limit for  P radiation factor
C				   in amplitude ratios
	REAL*4 CUTS		!  cutoff limit for  S radiation factor
C				   in amplitude ratios
C
	COMMON /FOCMEC/ PSTATN,RSTATN,XYZ,RD,POLRTY,NPOL,LOGRAT,
     1	  NRAT,MAXSOL,ERRP,ERRSV,NERRR,ERRRAT,CALRAT,WTRAT,VPVS3,
     2	  BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,AAMAX,
     3	  BADPP,KEYRAT,KEYPOL,ERRSH,BADSHP,ERR,THRESH,WBADP,WBADSH,
     4    WBADSV,BADSVP,CUTP,CUTS,BADP,BADSV,BADSH
	COMMON /FMCHAR/ SVSH


	CHARACTER*1 SENSE,SNUMPOL
	CHARACTER*80 COMMNT,FILENA,CVALUE,DUMMY
	LOGICAL TRUTH, relweight, pguess
	CHARACTER*40 INFO
	RD = 45.0/ATAN(1.0)
cxx
        write(6,*)'call form old'		
	open(2,file='focmec.lst',status='unknown')
	FILENA = 
     1    CVALUE('Output file name (for plotting) [focmec.out]',
     2    'focmec.out',NFILE)
	open(3,file=filena(1:nfile),status='unknown')
	CALL TIMDAT(2,'FOCMEC')
	CALL TIMDAT(3,'FOCMEC')
	COMMNT =
     1    CVALUE('Comment - up to 80 characters',DUMMY,NCOM)
	WRITE(2,'(1X,A)') COMMNT(1:NCOM)
	WRITE(3,'(1X,A)') COMMNT(1:NCOM)
100	FILENA = CVALUE('Input filespec',DUMMY,NFILE)
	OPEN(UNIT=1,FILE=FILENA(1:NFILE),STATUS='OLD',ERR=100)
	READ(1,'(A)') COMMNT
	ncom = lenc(commnt)
	WRITE(*,'(1X,A)') COMMNT(1:NCOM)
	IF (.NOT.TRUTH('Correct file?...[Y]')) GO TO 100
C
	WRITE(2,3) FILENA(1:NFILE)
	WRITE(3,3) FILENA(1:NFILE)
	WRITE(2,'(1X,A)') COMMNT(1:NCOM)
	WRITE(3,'(1X,A)') COMMNT(1:NCOM)
	WRITE(2,4)
	NRAT = 0
	NPOL = 0
	NPPOL = 0
	NSVPOL = 0
	NSHPOL = 0
	J = 0
	NGUESS = 0
200	read(1,'(a)',end=300) commnt
	ncom = lenc(commnt)
	if (ncom .lt. 22) then
	  read(commnt(1:21),'(A4,2F8.2,A1)') STA,AZIN,TOANG1,SENSE
	else
	  READ(commnt,5) STA,AZIN,TOANG1,SENSE,
     .	    RATLOG,SNUMPOL,TOANG2
	  if (ncom .gt. 39) then
	    read(commnt(40:ncom),'(a)') info
	  else
	    info = ' '
	  endif
	end if
	  IF (SENSE.EQ.'V' .OR. SENSE.EQ.'S' .OR. SENSE.EQ.'H') THEN
	    J = J + 1
	    IF (J .GT. MAX2) GO TO 300
	    stoang = toang1
	    ptoang = toang2
	    NRAT = NRAT + 1
	    LOGRAT(NRAT) = RATLOG
	    SVSH(2,NRAT) = SNUMPOL
	    RSTATN(NRAT) = STA
	    NADD = 0
	    SVSH(1,NRAT) = SENSE
	    IF (SENSE .EQ. 'H') NADD = 1000
	    IF (SENSE .EQ. 'S') NADD = 2000
	    WRITE(2,6) RSTATN(NRAT),AZIN,TOANG1,SENSE,LOGRAT(NRAT),
     .	      SVSH(1,NRAT),SVSH(2,NRAT),TOANG2,INFO(1:lenc(info))
	    KEYRAT(NRAT) = J + NADD
	  ELSE
	    WRITE(2,6) STA,AZIN,TOANG1,SENSE
	    IF (SENSE .EQ. 'U') SENSE = 'C'
            if (sense .eq. 'R') sense = '>'
            if (sense .eq. 'L') sense = '<'
	    if (sense.eq.'+' .or. sense.eq.'-' .or. sense.eq.'l'
     1	        .or. sense.eq.'r') then
	      if (nguess .eq. 0) then
	        nguess = 1
		pguess = truth('Include emergent polarity picks?..[Y]')
	      end if
	      if (pguess) then
	        IF (SENSE .EQ. '+') SENSE = 'C'
		IF (SENSE .EQ. '-') SENSE = 'D'
		IF (SENSE .EQ. 'l') SENSE = '<'
		IF (SENSE .EQ. 'r') SENSE = '>'
	      end if
	    end if
	    IF(.NOT.(SENSE.EQ.'C' .OR. SENSE.EQ.'D'
     .        .OR. SENSE .EQ. 'F' .OR. SENSE .EQ. 'B'
     .	      .OR. SENSE .EQ. '<' .OR. SENSE .EQ. '>')) GO TO 200
	      J = J + 1
	      IF (J .GT. MAX2) GO TO 300
	      stoang = toang1
	      ptoang = toang1
	      IF (SENSE .EQ. '<' .OR. SENSE .EQ. '>') THEN
	        NSHPOL = NSHPOL + 1
		if (sense .eq.'R') sense = '>'
	        if (sense .eq. 'L') sense = '<'
	        NADD = 2000
	      ELSE IF (SENSE .EQ. 'F' .OR. SENSE .EQ. 'B') THEN
	        NSVPOL = NSVPOL + 1
	        NADD = 1000
	      ELSE
	        NPPOL = NPPOL + 1
	        NADD = 0
	      END IF
	      NPOL = NPOL + 1
	      KEYPOL(NPOL) = J + NADD
	      PSTATN(NPOL) = STA
	      IF (SENSE .EQ. 'C' .OR. SENSE .EQ. '<'
     .          .OR. SENSE .EQ. 'F') THEN
	             POLRTY(NPOL) = 1
	      ELSE
	             POLRTY(NPOL) = -1
	      END IF
	  ENDIF
	  TREND = AZIN/RD
	  PLUNGE = (90.0 - PTOANG)/RD 
	  COST = COS(TREND)
	  SINT = SIN(TREND)
	  COSP = COS(PLUNGE)
	  SINP = SIN(PLUNGE)
	  XYZ(1,J) = COST*COSP
	  XYZ(2,J) = SINT*COSP
	  XYZ(3,J) = SINP
	  SPLUNG = (90.0 - STOANG)/RD
	  SINP = SIN(SPLUNG)
	  COSP = COS(SPLUNG)
C
C	Next two vectors reversed in sign from normal convention because
C	  of my convention for SV and SH (down and left, facing the station)
C
	  XYZ(4,J) = -COST*SINP
	  XYZ(5,J) = -SINT*SINP
	  XYZ(6,J) = +COSP
	  XYZ(7,J) = SINT
	  XYZ(8,J) = -COST
	  XYZ(9,J) = 0.0
	  GO TO 200
300	CLOSE(UNIT=1)
	WRITE(*,7) NPPOL,NSVPOL,NSHPOL,NRAT
	IF (NPOL .LE. 0) THEN
	  WRITE(2,8)
	  WRITE(3,8)
	  GO TO 400
	ELSE
	  write(*,*) ' Can have relative weighting for polarity errors'
	  write(*,*) '  In this case, weight = theor. rad. factor'
	  write(*,*) '  above a chosen threshold'
	  IF (TRUTH('Want relative weighting?..[N]')) THEN
	    THRESH = RVALUE('Lower threshold for weighting [0.1]',0.1)
	    relweight = .true.
	  ELSE
	    THRESH = 1.0
	    relweight = .false.
	  END IF
	END IF
	IF (NPPOL .GT. 0 .AND. (NSHPOL .GT. 0
     .      .OR. NSVPOL .GT. 0)) THEN
	  WRITE(*,*) 'Options:    Total polarity errors'
	  WRITE(*,*) '         or Separate P and S error limits'
	  IF (TRUTH('Total polarity error option?..[Y]')) THEN
	    if (relweight) then
	      ERR = VALUE('Total number of errors (floating point)')
	      WRITE(3,17) NPPOL,NSVPOL,NSHPOL,ERR,THRESH
	      WRITE(2,17) NPPOL,NSVPOL,NSHPOL,ERR,THRESH
	    else
	      NERR = VALUE('Total number of errors (integer)')
	      err = nerr
	      WRITE(3,18) NPPOL,NSVPOL,NSHPOL,NERR
	      WRITE(2,18) NPPOL,NSVPOL,NSHPOL,NERR 
	    end if
	    ERRP = ERR
            ERRSV = ERR
	    ERRSH = ERR
	    GO TO 400
	  END IF
	END IF
	IF (NPPOL .GT. 0) THEN
	  if (relweight) then
	    ERRP = RVALUE('Allowed P polarity errors..[0.0]',0.0)
	  else
	    NERRP = IVALUE('Allowed P polarity errors..[0]',0)
	    errp = nerrp
	  end if
	  IF (ERRP .GT. float(NPPOL)) ERRP = NPPOL
	else
	  errp = 0.0
	END IF
	IF (NSVPOL .GT. 0) THEN
	  if (relweight) then
	    ERRSV = RVALUE('Allowed SV polarity errors..[0.0]',0.0)
	  else
	    NERRSV = IVALUE('Allowed SV polarity errors..[0]',0)
	    errSV = nerrsv
	  end if
	  IF (ERRSV .GT. float(NSVPOL)) ERRSV = NSVPOL
	else
	  errsv = 0.0
	END IF
	IF (NSHPOL .GT. 0) THEN
	  if (relweight) then
	    ERRSH = RVALUE('Allowed SH polarity errors..[0.0]',0.0)
	  else
	    NERRSH = IVALUE('Allowed SH polarity errors..[0]',0)
	    errsh = nerrsh
	  end if
	  IF (ERRSH .GT. float(NSHPOL)) ERRSH = NSHPOL
	else
	  errsh = 0.0
	END IF
	err = errp + errsv + errsh
	If (nguess .eq. 1) then
		if (pguess) then
		  write(2,*) 'Including emergent polarity picks'
		  write(3,*) 'Including emergent polarity picks'
		else
		  write(2,*) 'Not including emergent polarity picks'
		  write(3,*) 'Not including emergent polarity picks'
		end if
	end if
	if (thresh .lt. 1.0) then
	  write(2,19) nppol,errp,nsvpol,errsv,nshpol,errsh,thresh
	  write(3,19) nppol,errp,nsvpol,errsv,nshpol,errsh,thresh
	else
	  nerrp = errp
	  nerrsv = errsv
	  nerrsh = errsh
	  write(2,9) NPPOL,NERRP,NSVPOL,NERRSV,NSHPOL,NERRSH
	  write(3,9) NPPOL,NERRP,NSVPOL,NERRSV,NSHPOL,NERRSH
	end if
400	IF (NRAT .LE. 0) THEN
	  WRITE(2,12)
	  WRITE(3,12)
	ELSE
	  VPVS = RVALUE('VP/VS ratio at source ...[1.732]',1.732)
	  ERRRAT = RVALUE('Maximum allowed log10 of ratio..[0.6]',0.6)
	  NERRR = IVALUE('Number of allowed amp. ratio errors..[0]',0)
	  write(*,*) 'Next two entries are for near nodal amplitudes'
	  write(*,*) 'CUTP is the lower bound for P radiation factor'
	  write(*,*) 'CUTS is the lower bound for S radiation factor'
	  write(*,*) 'Ratio is indeterminate if both calculated'
	  write(*,*) '    values less than the chosen CUT values'
	  CUTP = RVALUE('CUTP: lower-limit P cutoff...[0.05]',0.05)
	  CUTS = RVALUE('CUTS: lower-limit S cutoff...[0.15]',0.15)
	  IF (NERRR .GT. NRAT) NERRR = NRAT
	  WRITE(2,13) NRAT,NERRR,ERRRAT,VPVS
	  WRITE(2,*) CUTP,' = P radiation cutoff',CUTS,
     .	              ' = S radiation cutoff for ratios'
	  WRITE(3,13) NRAT,NERRR,ERRRAT,VPVS
	  WRITE(3,*) CUTP,' = P radiation cutoff',CUTS,
     .	              ' = S radiation cutoff for ratios'
	  VPVS3 = VPVS**3
	ENDIF
	MAXSOL = IVALUE('Exit after this many acceptable sols...[100]',
     .	  100)
	BTMIN = RVALUE('Minimum search value B trend..[0]',0.0)
	BTDEL = ABS(RVALUE('Increment for B trend..[5 degrees]',5.0))
	BTMAX = RVALUE('Maximum B trend..[355 degrees]',355.0)
	  BTMAX = AMAX1(BTMIN,AMIN1(BTMAX,359.0))
	BPMIN = RVALUE('Minimum search value B plunge..[0]',0.0)
	BPDEL = ABS(RVALUE('Increment for B plunge..[5 degrees]',5.0))
	BPMAX = RVALUE('Maximum B plunge..[90 degrees]',90.0)
	  BPMAX = AMAX1(BPMIN,AMIN1(BPMAX,90.0))
	WRITE(*,*) '"A" angle = zero in vertical plane of B trend'
	AAMIN = RVALUE('Minimum search value A angle..[0]',0.0)
	AADEL = ABS(RVALUE('Increment for A angle..[5 degrees]',5.0))
	AAMAX = RVALUE('Maximum A angle..[85 degrees]',85.0)
cxx	AAMAX = RVALUE('Maximum A angle..[90 degrees]',90.0)   ! jh jul 2016
	  AAMAX = AMAX1(AAMIN,AMIN1(AAMAX,90.0-AADEL))
	WRITE(2,14) BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,
     1	  AAMAX
	WRITE(3,14) BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,
     1	  AAMAX
	WRITE(3,15)
	write(*,'('' '')')
	WRITE(*,15)
	write(2,16)
	RETURN
C
C
3	FORMAT(1X,'Input from a file ',A)
4	FORMAT(/' Statn',T9,'Azimuth',T18,'TOAng',T26,
     1    'Key',T31,'Log10 (S/P)',T44,
     2    'NumPol',T52,'DenTOAng',T62,'Comment')
5	FORMAT(A4,2F8.2,A1,F8.4,1X,A1,1X,F6.2)
6	FORMAT(2X,A4,T10,F5.1,T19,F5.1,T27,A1,T32,F8.4,T41,'S',A1,T47,
     .    A1,T53,F6.2,T60,A)
7	FORMAT(' Input:',I4,' P ',I4,' SV and ',I4,' SH polarities and,',
     .    I4,' ratios')
8	FORMAT(' There are no polarity data')
9	FORMAT(' Polarities/Errors:  P ',I3.3,'/',I2.2,'  SV ',
     .    I3.3,'/',I2.2,'  SH ',I3.3,'/',I2.2)
12	  FORMAT(' There are no amplitude ratio data')
13	  FORMAT(' Input',I3,' ratios',I3,
     1      ' allowed errors, maximum error of',
     2      F5.2,'    VP/VS =',F6.3)
14	FORMAT(' The minimum, increment and maximum B axis trend are ',
     1    3F8.2/' The limits for the B axis',
     2    ' plunge are ',3F8.2/' The limits',
     3    ' for the angle of the A axis are ',3F8.2)
15	FORMAT(T5,'Dip',T11,'Strike',T20,'Rake',T28,'Pol:',
     .    T33,'P',T39,'SV',T45,'SH',
     .    T50,'Rat Err',T59,'RMS RErr',T70,'RErr (All)')
16       FORMAT(' ',79('+')/)
17	FORMAT(I4,' P Pol.',I3,' SV Pol.',I3,' SH Pol.',F5.1,' allowed',
     .    ' (weighted) errors  Threshold =',F5.2)
18	FORMAT(I4,' P Pol.',I3,' SV Pol.',I3,' SH Pol.',I4,' allowed',
     .    '  errors')
19	FORMAT(' Polarities/Errors:  P ',I3.3,'/',F4.1,'  SV ',
     .    I3.3,'/',F4.1,'  SH ',I3.3,'/',F4.1,'  Threshh. =',F5.2)
	END





C+
	SUBROUTINE xFOCINP
c     old version
C
C	Input routine for FOCMEC
C
C	Arthur Snoke  Virginia Tech  May 1984
C	Last perturbed 12 October 1990
C	20 August 1991:  sun version.  call assign replaced by open
C	31 August:  expanded to include SV polarities and SV/SH ratios
C	15 May 1992:  Changed name of listing file from lp.lst to 
C		focmec.lst
C	19 June 1993: Separate P and S errors should now work
C	20 Jun 1993:  Belts and braces:  If someone uses a ratio with
C	  no input S take-off angle, now sets it equal to P angle
C	2 August 1993:  Changed rules for ratios near nodal surfaces
C	6 January 1997:  format change
C	7 April 2000: sense='R' no longer allowed for ratios.  First TOANG
C	  is not necessarily for P (if an SH or SV polarity is to be used)
C	1 October 2001: When porting to a PC, found some compilers could
C		not handle reading in an integer for a floating-pint
C		variable (number of polarity errors), so "fixed" it.
C	26 March 2002: If emergent polarity data are included, now it
C		will ask if you want to include it.  Previously it 
C		ignored such data.
C	5 July 2002: Some compilers do not like the way I had used
C	  CVALUE -- CVALUE(1:NFILE) with NFILE defined on the right-hand side.
C-
c	INCLUDE 'FOCMEC.INC'


C	FOCMEC.INC
C
C	INCLUDE file for FOCMEC
C	27 August 1991 for the sun now includes SV
C	  polarities and SV/SH ratios
C	2 August 1993: changed ratio evaluations near nodal planes,
C	     so left out RLOW* and RHI* and replaced CUT with CUTP
C	     and CUTS.
C	2 June 2000: Increased MAX to 2000 and made it so only one version
C		appears with the distribution (others are virtual links).
C	NOTE! For some compilers, the ! starting an on-line comment may cause
C		an error.  Simply delete the ends of line starting at the !
C		if that is the case.
C-
c  oct 2010     jh: increase max to 5000
c
	PARAMETER (MAX = 5000)
	PARAMETER (MAX2 = 2*MAX)
C
C	Input Data
C
	REAL*4 XYZ(9,MAX2)	!  R, Theta, and Phi station coordinates 
	REAL*4 PSTATN(MAX)	!  Stations with polarity data
	REAL*4 RSTATN(MAX)	!  Stations with amplitude-ratio data
C				     X = north, Y east, Z down
	INTEGER POLRTY(MAX)	!  1 for C, F, or <;  -1 for D, B, or >
	INTEGER NPOL		!  Number of Polarities read in
	INTEGER KEYPOL(MAX)	!  Keys polarity data relative to input
	REAL*4 LOGRAT(MAX)	!  Log10 of (SV/P), (SH/P), or (SV/SH)
C				   including free surface correction, etc.
	INTEGER NRAT		!  Number of ratios read in
	INTEGER KEYRAT(MAX)	!  Keys ratios data relative to input
	CHARACTER*1 SVSH(2,MAX)	! First element V for SV or H for SH,
C				  Second element polarity: F or B for SV
C				  L or R for SH (back to station) for
C	                          numerator in SV/P, SH/P, or SV/SH ratio
C
C	  Processing things
C
	INTEGER MAXSOL	!  Exit after this many accept. aols.
	REAL*4 ERR		!  Total allowed number of errors
	REAL*4 ERRP		!  Allowed number of P polarity errors
	REAL*4 ERRSV		!  Allowed number of SV polarity errors
	REAL*4 ERRSH		!  Allowed number of SH polarity errors
	REAL*4 THRESH		!  Polarity weighting threshold
C				  (1.0 if no weighting)
	REAL*4 BADPP(MAX)	!  Stations with a P polarity error
	REAL*4 BADSVP(MAX)	!  Stations with a SV polarity error
	REAL*4 BADSHP(MAX)	!  Stations with a SH polarity error
	REAL*4 WBADP(MAX)	!  Weishts of P polarity errors
	REAL*4 WBADSV(MAX)	!  Weights of SV polarity errors
	REAL*4 WBADSH(MAX)	!  Weights of SH polarity errors
	REAL*4 BADP		!  Total weight of P polarity errors
	REAL*4 BADSV		!  Total weight of SV polarity errors
	REAL*4 BADSH		!  Total weight of SH polarity errors
	INTEGER NERRR		!  Allowed number of ratio errors
	REAL*4 ERRRAT		!  Maximum allowed Log10 ratio error
	REAL*4 CALRAT(MAX)	!  Theoretical Log10 of amplitude ratio
	INTEGER WTRAT(MAX)	!  1 if that ratio used, 0 otherwise
	REAL*4 VPVS3		!  Cube of P/S velocity ratio 
	REAL*4 BTMIN,BTDEL,BTMAX	!  Search range in B axis trend
	REAL*4 BPMIN,BPDEL,BPMAX	!  Search range in B axis plunge
	REAL*4 AAMIN,AADEL,AAMAX	!  Search range in A axis angle
C					   measured from B trend
	REAL*4 CUTP		!  cutoff limit for  P radiation factor
C				   in amplitude ratios
	REAL*4 CUTS		!  cutoff limit for  S radiation factor
C				   in amplitude ratios
C
	COMMON /FOCMEC/ PSTATN,RSTATN,XYZ,RD,POLRTY,NPOL,LOGRAT,
     1	  NRAT,MAXSOL,ERRP,ERRSV,NERRR,ERRRAT,CALRAT,WTRAT,VPVS3,
     2	  BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,AAMAX,
     3	  BADPP,KEYRAT,KEYPOL,ERRSH,BADSHP,ERR,THRESH,WBADP,WBADSH,
     4    WBADSV,BADSVP,CUTP,CUTS,BADP,BADSV,BADSH
	COMMON /FMCHAR/ SVSH


	CHARACTER*1 SENSE,SNUMPOL
	CHARACTER*80 COMMNT,FILENA,CVALUE,DUMMY
	LOGICAL TRUTH, relweight, pguess
	CHARACTER*40 INFO
	RD = 45.0/ATAN(1.0)		
	open(2,file='focmec.lst',status='unknown')
	FILENA = 
     1    CVALUE('Output file name (for plotting) [focmec.out]',
     2    'focmec.out',NFILE)
	open(3,file=filena(1:nfile),status='unknown')
	CALL TIMDAT(2,'FOCMEC')
	CALL TIMDAT(3,'FOCMEC')
	COMMNT =
     1    CVALUE('Comment - up to 80 characters',DUMMY,NCOM)
	WRITE(2,'(1X,A)') COMMNT(1:NCOM)
	WRITE(3,'(1X,A)') COMMNT(1:NCOM)
100	FILENA = CVALUE('Input filespec',DUMMY,NFILE)
	OPEN(UNIT=1,FILE=FILENA(1:NFILE),STATUS='OLD',ERR=100)
	READ(1,'(A)') COMMNT
	ncom = lenc(commnt)
	WRITE(*,'(1X,A)') COMMNT(1:NCOM)
	IF (.NOT.TRUTH('Correct file?...[Y]')) GO TO 100
C
	WRITE(2,3) FILENA(1:NFILE)
	WRITE(3,3) FILENA(1:NFILE)
	WRITE(2,'(1X,A)') COMMNT(1:NCOM)
	WRITE(3,'(1X,A)') COMMNT(1:NCOM)
	WRITE(2,4)
	NRAT = 0
	NPOL = 0
	NPPOL = 0
	NSVPOL = 0
	NSHPOL = 0
	J = 0
	NGUESS = 0
200	read(1,'(a)',end=300) commnt
	ncom = lenc(commnt)
	if (ncom .lt. 40) then
	  read(commnt(1:21),'(A4,2F8.2,A1)') STA,AZIN,TOANG1,SENSE
	else
	  READ(commnt,5,END=300) STA,AZIN,TOANG1,SENSE,
     .	    RATLOG,SNUMPOL,TOANG2,INFO
	end if
	  IF (SENSE.EQ.'V' .OR. SENSE.EQ.'S' .OR. SENSE.EQ.'H') THEN
	    J = J + 1
	    IF (J .GT. MAX2) GO TO 300
	    stoang = toang1
	    ptoang = toang2
	    NRAT = NRAT + 1
	    LOGRAT(NRAT) = RATLOG
	    SVSH(2,NRAT) = SNUMPOL
	    RSTATN(NRAT) = STA
	    NADD = 0
	    SVSH(1,NRAT) = SENSE
	    IF (SENSE .EQ. 'H') NADD = 1000
	    IF (SENSE .EQ. 'S') NADD = 2000
	    WRITE(2,6) RSTATN(NRAT),AZIN,TOANG1,SENSE,LOGRAT(NRAT),
     .	      SVSH(1,NRAT),SVSH(2,NRAT),TOANG2,INFO(1:lenc(info))
	    KEYRAT(NRAT) = J + NADD
	  ELSE
	    WRITE(2,6) STA,AZIN,TOANG1,SENSE
	    IF (SENSE .EQ. 'U') SENSE = 'C'
            if (sense .eq. 'R') sense = '>'
            if (sense .eq. 'L') sense = '<'
	    if (sense.eq.'+' .or. sense.eq.'-' .or. sense.eq.'l'
     1	        .or. sense.eq.'r') then
	      if (nguess .eq. 0) then
	        nguess = 1
		pguess = truth('Include emergent polarity picks?..[Y]')
	      end if
	      if (pguess) then
	        IF (SENSE .EQ. '+') SENSE = 'C'
		IF (SENSE .EQ. '-') SENSE = 'D'
		IF (SENSE .EQ. 'l') SENSE = '<'
		IF (SENSE .EQ. 'r') SENSE = '>'
	      end if
	    end if
	    IF(.NOT.(SENSE.EQ.'C' .OR. SENSE.EQ.'D'
     .        .OR. SENSE .EQ. 'F' .OR. SENSE .EQ. 'B'
     .	      .OR. SENSE .EQ. '<' .OR. SENSE .EQ. '>')) GO TO 200
	      J = J + 1
	      IF (J .GT. MAX2) GO TO 300
	      stoang = toang1
	      ptoang = toang1
	      IF (SENSE .EQ. '<' .OR. SENSE .EQ. '>') THEN
	        NSHPOL = NSHPOL + 1
		if (sense .eq.'R') sense = '>'
	        if (sense .eq. 'L') sense = '<'
	        NADD = 2000
	      ELSE IF (SENSE .EQ. 'F' .OR. SENSE .EQ. 'B') THEN
	        NSVPOL = NSVPOL + 1
	        NADD = 1000
	      ELSE
	        NPPOL = NPPOL + 1
	        NADD = 0
	      END IF
	      NPOL = NPOL + 1
	      KEYPOL(NPOL) = J + NADD
	      PSTATN(NPOL) = STA
	      IF (SENSE .EQ. 'C' .OR. SENSE .EQ. '<'
     .          .OR. SENSE .EQ. 'F') THEN
	             POLRTY(NPOL) = 1
	      ELSE
	             POLRTY(NPOL) = -1
	      END IF
	  ENDIF
	  TREND = AZIN/RD
	  PLUNGE = (90.0 - PTOANG)/RD 
	  COST = COS(TREND)
	  SINT = SIN(TREND)
	  COSP = COS(PLUNGE)
	  SINP = SIN(PLUNGE)
	  XYZ(1,J) = COST*COSP
	  XYZ(2,J) = SINT*COSP
	  XYZ(3,J) = SINP
	  SPLUNG = (90.0 - STOANG)/RD
	  SINP = SIN(SPLUNG)
	  COSP = COS(SPLUNG)
C
C	Next two vectors reversed in sign from normal convention because
C	  of my convention for SV and SH (down and left, facing the station)
C
	  XYZ(4,J) = -COST*SINP
	  XYZ(5,J) = -SINT*SINP
	  XYZ(6,J) = +COSP
	  XYZ(7,J) = SINT
	  XYZ(8,J) = -COST
	  XYZ(9,J) = 0.0
	  GO TO 200
300	CLOSE(UNIT=1)
	WRITE(*,7) NPPOL,NSVPOL,NSHPOL,NRAT
	IF (NPOL .LE. 0) THEN
	  WRITE(2,8)
	  WRITE(3,8)
	  GO TO 400
	ELSE
	  write(*,*) ' Can have relative weighting for polarity errors'
	  write(*,*) '  In this case, weight = theor. rad. factor'
	  write(*,*) '  above a chosen threshold'
	  IF (TRUTH('Want relative weighting?..[N]')) THEN
	    THRESH = RVALUE('Lower threshold for weighting [0.1]',0.1)
	    relweight = .true.
	  ELSE
	    THRESH = 1.0
	    relweight = .false.
	  END IF
	END IF
	IF (NPPOL .GT. 0 .AND. (NSHPOL .GT. 0
     .      .OR. NSVPOL .GT. 0)) THEN
	  WRITE(*,*) 'Options:    Total polarity errors'
	  WRITE(*,*) '         or Separate P and S error limits'
	  IF (TRUTH('Total polarity error option?..[Y]')) THEN
	    if (relweight) then
	      ERR = VALUE('Total number of errors (floating point)')
	      WRITE(3,17) NPPOL,NSVPOL,NSHPOL,ERR,THRESH
	      WRITE(2,17) NPPOL,NSVPOL,NSHPOL,ERR,THRESH
	    else
	      NERR = VALUE('Total number of errors (integer)')
	      err = nerr
	      WRITE(3,18) NPPOL,NSVPOL,NSHPOL,NERR
	      WRITE(2,18) NPPOL,NSVPOL,NSHPOL,NERR 
	    end if
	    ERRP = ERR
            ERRSV = ERR
	    ERRSH = ERR
	    GO TO 400
	  END IF
	END IF
	IF (NPPOL .GT. 0) THEN
	  if (relweight) then
	    ERRP = RVALUE('Allowed P polarity errors..[0.0]',0.0)
	  else
	    NERRP = IVALUE('Allowed P polarity errors..[0]',0)
	    errp = nerrp
	  end if
	  IF (ERRP .GT. float(NPPOL)) ERRP = NPPOL
	else
	  errp = 0.0
	END IF
	IF (NSVPOL .GT. 0) THEN
	  if (relweight) then
	    ERRSV = RVALUE('Allowed SV polarity errors..[0.0]',0.0)
	  else
	    NERRSV = IVALUE('Allowed SV polarity errors..[0]',0)
	    errSV = nerrsv
	  end if
	  IF (ERRSV .GT. float(NSVPOL)) ERRSV = NSVPOL
	else
	  errsv = 0.0
	END IF
	IF (NSHPOL .GT. 0) THEN
	  if (relweight) then
	    ERRSH = RVALUE('Allowed SH polarity errors..[0.0]',0.0)
	  else
	    NERRSH = IVALUE('Allowed SH polarity errors..[0]',0)
	    errsh = nerrsh
	  end if
	  IF (ERRSH .GT. float(NSHPOL)) ERRSH = NSHPOL
	else
	  errsh = 0.0
	END IF
	err = errp + errsv + errsh
	If (nguess .eq. 1) then
		if (pguess) then
		  write(2,*) 'Including emergent polarity picks'
		  write(3,*) 'Including emergent polarity picks'
		else
		  write(2,*) 'Not including emergent polarity picks'
		  write(3,*) 'Not including emergent polarity picks'
		end if
	end if
	if (thresh .lt. 1.0) then
	  write(2,19) nppol,errp,nsvpol,errsv,nshpol,errsh,thresh
	  write(3,19) nppol,errp,nsvpol,errsv,nshpol,errsh,thresh
	else
	  nerrp = errp
	  nerrsv = errsv
	  nerrsh = errsh
	  write(2,9) NPPOL,NERRP,NSVPOL,NERRSV,NSHPOL,NERRSH
	  write(3,9) NPPOL,NERRP,NSVPOL,NERRSV,NSHPOL,NERRSH
	end if
400	IF (NRAT .LE. 0) THEN
	  WRITE(2,12)
	  WRITE(3,12)
	ELSE
	  VPVS = RVALUE('VP/VS ratio at source ...[1.732]',1.732)
	  ERRRAT = RVALUE('Maximum allowed log10 of ratio..[0.6]',0.6)
	  NERRR = IVALUE('Number of allowed amp. ratio errors..[0]',0)
	  write(*,*) 'Next two entries are for near nodal amplitudes'
	  write(*,*) 'CUTP is the lower bound for P radiation factor'
	  write(*,*) 'CUTS is the lower bound for S radiation factor'
	  write(*,*) 'Ratio is indeterminate if both calculated'
	  write(*,*) '    values less than the chosen CUT values'
	  CUTP = RVALUE('CUTP: lower-limit P cutoff...[0.05]',0.05)
	  CUTS = RVALUE('CUTS: lower-limit S cutoff...[0.15]',0.15)
	  IF (NERRR .GT. NRAT) NERRR = NRAT
	  WRITE(2,13) NRAT,NERRR,ERRRAT,VPVS
	  WRITE(2,*) CUTP,' = P radiation cutoff',CUTS,
     .	              ' = S radiation cutoff for ratios'
	  WRITE(3,13) NRAT,NERRR,ERRRAT,VPVS
	  WRITE(3,*) CUTP,' = P radiation cutoff',CUTS,
     .	              ' = S radiation cutoff for ratios'
	  VPVS3 = VPVS**3
	ENDIF
	MAXSOL = IVALUE('Exit after this many acceptable sols...[100]',
     .	  100)
	BTMIN = RVALUE('Minimum search value B trend..[0]',0.0)
	BTDEL = ABS(RVALUE('Increment for B trend..[5 degrees]',5.0))
	BTMAX = RVALUE('Maximum B trend..[355 degrees]',355.0)
	  BTMAX = AMAX1(BTMIN,AMIN1(BTMAX,359.0))
	BPMIN = RVALUE('Minimum search value B plunge..[0]',0.0)
	BPDEL = ABS(RVALUE('Increment for B plunge..[5 degrees]',5.0))
	BPMAX = RVALUE('Maximum B plunge..[90 degrees]',90.0)
	  BPMAX = AMAX1(BPMIN,AMIN1(BPMAX,90.0))
	WRITE(*,*) '"A" angle = zero in vertical plane of B trend'
	AAMIN = RVALUE('Minimum search value A angle..[0]',0.0)
	AADEL = ABS(RVALUE('Increment for A angle..[5 degrees]',5.0))
	AAMAX = RVALUE('Maximum A angle..[85 degrees]',85.0)
	  AAMAX = AMAX1(AAMIN,AMIN1(AAMAX,90.0-AADEL))
	WRITE(2,14) BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,
     1	  AAMAX
	WRITE(3,14) BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,
     1	  AAMAX
	WRITE(3,15)
	write(*,'('' '')')
	WRITE(*,15)
	write(2,16)
	RETURN
C
C
3	FORMAT(1X,'Input from a file ',A)
4	FORMAT(/' Statn',T9,'Azimuth',T18,'TOAng',T26,
     1    'Key',T31,'Log10 (S/P)',T44,
     2    'NumPol',T52,'DenTOAng',T62,'Comment')
5	FORMAT(A4,2F8.2,A1,F8.4,1X,A1,1X,F6.2,1X,A)
6	FORMAT(2X,A4,T10,F5.1,T19,F5.1,T27,A1,T32,F8.4,T41,'S',A1,T47,
     .    A1,T53,F6.2,T60,A)
7	FORMAT(' Input:',I4,' P ',I4,' SV and ',I4,' SH polarities and,',
     .    I4,' ratios')
8	FORMAT(' There are no polarity data')
9	FORMAT(' Polarities/Errors:  P ',I3.3,'/',I2.2,'  SV ',
     .    I3.3,'/',I2.2,'  SH ',I3.3,'/',I2.2)
12	  FORMAT(' There are no amplitude ratio data')
13	  FORMAT(' Input',I3,' ratios',I3,
     1    ' allowed errors, maximum error of',
     2    F5.2,'    VP/VS =',F6.3)
14	FORMAT(' The minimum, increment and maximum B axis trend are ',
     1    3F8.2/' The limits for the B axis',
     2    ' plunge are ',3F8.2/' The limits',
     3     ' for the angle of the A axis are ',3F8.2)
15	FORMAT(T5,'Dip',T11,'Strike',T20,'Rake',T28,'Pol:',
     .    T33,'P',T39,'SV',T45,'SH',
     .    T50,'Rat Err',T59,'RMS RErr',T70,'RErr (All)')
16       FORMAT(' ',79('+')/)
17	FORMAT(I4,' P Pol.',I3,' SV Pol.',I3,' SH Pol.',F5.1,' allowed',
     .    ' (weighted) errors  Threshold =',F5.2)
18	FORMAT(I4,' P Pol.',I3,' SV Pol.',I3,' SH Pol.',I4,' allowed',
     .    '  errors')
19	FORMAT(' Polarities/Errors:  P ',I3.3,'/',F4.1,'  SV ',
     .    I3.3,'/',F4.1,'  SH ',I3.3,'/',F4.1,'  Threshh. =',F5.2)
	END
C+
      SUBROUTINE GDMOT(RD,IPS,VSVP,AIN,AR,PHR,AV)
C
C  GDMOT GIVES GROUND DISPLACEMENT FOR INCIDENT P (IPS=1) OR S (IPS=2)
C  BULLEN (1963) P. 129 FOR INCIDENT P (BUT AIN=90-E)
C  AIN IS EMERGENCE ANGLE IN DEGREES RELATIVE TO VERTICAL
C  VSVP=VS/VP AT THE SURFACE
C  AV AND AR ARE THE VERTICAL AND RADIAL GROUND DISPLACEMENTS
C  FOR SV INC. BEYOND CRITICAL, PHV=PHR+90.
C     (AR MAY BE NEGATIVE IN THIS CASE - NOT REALLY AN AMPLITUDE)
C-
      COTAN(B) = 1.0/TAN(B)
      COTCOT(B)=1.-COTAN(B)**2
      PHR=0.0
      IF (AIN .EQ. 0.0) THEN
        AR=2.*(IPS-1)
	AV = 2*(2 - IPS)
      ELSE
        IF (IPS .EQ. 1) THEN
          A = AIN/RD
          B=ASIN(SIN(A)*VSVP)
          DEN=2.*COS(A)/(SIN(B)**2*(4.*COTAN(A)*COTAN(B)
     *      +COTCOT(B)**2))
          AR=2.*COTAN(B)*DEN
          AV=-COTCOT(B)*DEN
        ELSE IF (IPS .EQ. 2) then
          B=AIN/RD
          SA=SIN(B)/VSVP
          IF(SA .LE. 1.0) THEN
            A = ASIN(SA)
            DEN=2.*COTAN(B)/(SIN(B)*(4.*COTAN(A)*COTAN(B)
     *        +COTCOT(B)**2))
            AV = -2*COTAN(A)*DEN
            AR=-COTCOT(B)*DEN
          ELSE
            CTA=SQRT(1.-1./(SA*SA))
            DEN=2.*COTAN(B)/(SIN(B)*SQRT((4.*CTA*COTAN(B))**2
     *        +COTCOT(B)**4))
            AV=-2.*CTA*DEN
            AR=0.
            PHR = 90.0
            IF(COTCOT(B).EQ.0.0) RETURN
            AR=-COTCOT(B)*DEN
            PHR=ATAN2(4.*COTAN(B)*CTA,-COTCOT(B)**2)*RD
          END IF
        END IF
      END IF
      RETURN
      END
c+
	character*(*) function GETSTRING(prompt)
c
c  outputs 'prompt' using PRINTX
c  and accepts input character string
c				Alan Linde ... Aug 1986
C       27 July 1993: Did input read through cstring so can have 
C         comment lines
C	12 February 95:  Kill leading blanks
c-
	character*(*) prompt
	character*80 temp
	  getstring = ' '
c output 'prompt'
	call printx(prompt)
	kk=lenc(prompt)
	if (prompt(kk:kk).eq.']') then
	  ll=0
	  do i=kk-1,1,-1
	    if (prompt(i:i).eq.'['.and.ll.eq.0) ll=i+1
	  end do
	  if (ll.ne.0) getstring=prompt(ll:kk-1)
	end if
c  get the response
	call cstring(temp,nout)
c  Kill leading blanks
        do while (nout.gt.1 .and. temp(1:1).eq.' ')
          nout = nout - 1
          temp(1:nout) = temp(2:nout+1)
          temp(nout+1:nout+1) = ' '
        end do
	if (nout .gt. 0) getstring=temp(1:nout)
	return
	end
C+
C     SUBROUTINE GMPRD(A,B,R,N,M,L)
C
C     MULTIPLIES A N BY M MATRIX A TIMES A M BY L MATRIX B GIVING A N
C         BY L MATRIX R
C-
      SUBROUTINE GMPRD(A,B,R,N,M,L)
c     DIMENSION A(1),B(1),R(1)
      DIMENSION A(*),B(*),R(*)
      IR=0
      IK=-M
      DO 10 K=1,L
      IK=IK+M
      DO 10 J=1,N
      IR=IR+1
      JI=J-N
      IB=IK
      R(IR)=0
      DO 10 I=1,M
      JI=JI+N
      IB=IB+1
   10 R(IR)=R(IR)+A(JI)*B(IB)
      RETURN
      END
C+
	Subroutine grndmtn(RDDEG,IPS,VRAT,aemrg,AR,AV,PhR,PhV)
C
C  Version of gdmot using equations from Hudson (or Aki & Richards)
C  aemrg IS EMERGENCE ANGLE IN DEGREES RELATIVE TO UPWARD VERTICAL
C  VRAT=VS/VP AT THE SURFACE
C  AV AND AR ARE THE VERTICAL AND RADIAL GROUND DISPLACEMENT AMPLITUDES
C  PhR and PhV are the phases (in degrees).
C  A and B are the amplitudes for the P and S parts of the reflected
C	waves.  Beyond the critical angle, the phases Ap and Bp are nonzero
C	as A and B become complex.  Below the critical angle A and B
C	can be negative.  In this routine, A and B are not returned.
C	Opposite sign convention for incident SV from Hudson.  My convention
C	is positive incident SV is towards the surface (up and back),
C	his is away from the interface (down and forward).  We have the
C	same convention for reflected SV (up and forward).  Note that in
C	my convention, SV has the same direction relative to the ray
C	propagation for incident and reflected.  Not so for theirs.
C     
C	jas/vt July 2002
C-
	real mb,mbsts
	mb(x) = 1 - 2.0*x*x
	Ap = 0.0
	Bp = 0.0
	PhR = 0.0
	PhV = 0.0
      IF (aemrg .EQ. 0.0) THEN
        AR=2.*(IPS-1)
	PhR = (IPS-1)*180.0
	AV = 2*(2 - IPS)
	A = ips - 2
	B = ips - 1
      else if (aemrg .eq. 90.0) then
	ar = 0.0
	av = 0.0
	A = ips - 2
	B = ips - 1
	Bp = (ips-1)*180.0/RDDEG
      else if (aemrg.eq.45.0 .and. ips.eq.2) then
        A = 0.0
	B = 1.0
	PhR = -90.0
	Ap = 90.0
	av = sqrt(2.0)
	ar = 0.0
      ELSE
        IF (IPS .EQ. 1) THEN
          tp = aemrg/RDDEG
          ts=ASIN(SIN(tp)*VRAT)
	  sts = sin(ts)
	  mbsts = mb(sts)
	  ctp = cos(tp)
	  stp = sin(tp)
	  cts = cos(ts)
	  Anum1 = 4.0*sts*sts*ctp*cts*vrat
	  den = Anum1 + mbsts**2
	  A = (Anum1 - mbsts**2)/den
	  B = 4*sts*ctp*mbsts/den
	  AR = stp*(1.0 + A) + cts*B
	  if (AR .lt. 0.00001) AR = 0.0
	  AV = ctp*(1.0 - A) +sts*B
	  if (AV .lt. 0.00002) AV = 0.0
	else if (ips .eq. 2) then
          ts=aemrg/RDDEG
	  sts = sin(ts)
	  mbsts = mb(sts)
	  d2 = mbsts**2
	  cts = cos(ts)
	  Anum1 = 4.0*sts*cts*vrat
	  Anum = -Anum1*mbsts
          stp = sts/VRAT
          IF(stp .LE. 1.0) THEN
            tp = ASIN(stp)
	    ctp = cos(tp)
	    d1 = sts*ctp*Anum1
	    den = d1 + d2
	    A = Anum/den
	    B = (d1-d2)/den
	    AR = (2.*cts*d2 - stp*Anum)/den
	    PhR = 180.0
	    AV = (2.0*d1*sts - ctp*Anum)/den
	    if (abs(AV) .lt. 0.0001) AV = 0.0
	  else
	    ctp = sqrt(stp*stp-1.0)   ! this is pure imaginary
	    d1 = sts*ctp*Anum1
	    den = sqrt(d1*d1 + d2*d2)
	    B = 1.0
	    A = Anum/den
	    if (abs(A) .lt. 0.0001) A = 0.0
	    if (A .lt. 0.0) then
	      A = -A
	      Ap = rddeg*atan2(-d1,d2)
	    else
	      Ap = rddeg*atan2(d1,-d2)
	    end if
	    Bp = rddeg*atan2(2.0*d1*d2,-d2*d2+d1*d1)
	    AR = (-2.0*d2*cts + Anum*stp)/den 
	    if (abs(AR) .lt. 0.0001) AR = 0.0
	    if (AR .lt. 0.0) then
	      AR = -AR
	      PhR = rddeg*atan2(d1,-d2)
	    else
	      PhR = rddeg*atan2(-d1,d2)
	    end if
	    AV = (2.0*d1*sts - Anum*ctp)/den
	    if (AV .lt. 0.0) then
	      AV = -AV
	      PhV = rddeg*atan2(d1,-d2) + 90.
	    else
	      PhV = rddeg*atan2(-d1,d2) + 90.
	    end if
          END IF
        END IF
      END IF
      RETURN
      END
C+
	INTEGER FUNCTION IVALUE(MSG,IDEF)
C
C PURPOSE:
C		THIS FUNCTION ACCEPTS A MESSAGE (ASKING FOR A VALUE)
C		AND RETURNS THE VALUE ENTERED AT THE TERMINAL
C ROUTINES CALLED:
C		PRINTX
C
C USE:
C		IANS=IVALUE('ENTER AN INTEGER',IDEF)
C	If enter a carriage return, IVALUE is set to IDEF.
C
C AUTHOR:
C			ALAN LINDE ... AUGUST 1980 (for VALUE)
C
C EXTENSIONS:
C	30 JULY 1989:  CAN HANDLE ENTRY FOLLOWED BY A BLANK OR TAB
C       27 July 1993: Did input read through cstring so can have 
C         comment lines
C	19 July 2002: PCs had a problem with single-digit integer, so ...
C-
	CHARACTER*1 E/'E'/,BLANK/' '/
	CHARACTER*30 STUFF
	CHARACTER*(*) MSG
C
100	CALL PRINTX(MSG)
	call cstring(stuff,nin)
	IF (NIN .GT. 0) THEN
	  NBLANK = INDEX(STUFF(1:NIN),BLANK)
	  IF (NBLANK .GT. 0) NIN = NBLANK - 1
	END IF
	IF (NIN .EQ. 0) THEN
	  IVALUE = IDEF
	ELSE
	  if (nin .eq. 1) then
	    stuff(2:2) = stuff(1:1)
	    stuff(1:1) = '0'
	    nin = 2
	  end if
	  READ(STUFF(1:NIN),*,ERR=100) IVALUE
	END IF
	RETURN
	END
C+
	SUBROUTINE IYESNO(MSG,IANS)
C
C
C     PURPOSE:
C	     THIS LITTLE SUBROUTINE ASKS A QUESTION AND RETURNS A
C	     RESPONSE TO THAT QUESTION. THE ANSWER TO THE QUESTION
C	     MUST BE EITHER 'Y' FOR YES, 'N' FOR NO, OR NOTHING
C	     (i.e. simply hitting carrage return) FOR THE DEFAULT
C	     REPONSE TO THE QUESTION.
C
C     ON INPUT:
C	    MSG = BYTE STRING CONTAINING THE QUESTION
C
C     ON OUTPUT:
C	    IANS = THE LOGICAL REPONSE TO THE QUESTION (1 or 0)
C     EXTRA FEATURES:
C	    DEFAULT SITUATION IS:
C	    IF LAST 3 CHARACTERS IN 'MSG' ARE
C	  	     [Y]  OR  [N]
C	    THEN 'IANS' = 1   OR   0
C
C	    IF LAST 3 CHARACTERS ARE NOT ONE OF ABOVE PAIRS
C	    THEN 'IANS' = 0
C	    (i.e. default for no supplied default is N)
C	30 JULY 1989:  IF ENTERED CHARACTER IS A BLANK OR A TAB, 
C	    TREATS AS A NULL ENTRY.
C       27 July 1993: Did input read through cstring so can have 
C         comment lines
C-
	CHARACTER*1 DELIM/'$'/,CHARIN,BLANK/' '/
	CHARACTER*3 TEST,UCY,LCY
	character*80 string_in
	CHARACTER*(*) MSG
	DATA UCY/'[Y]'/,LCY/'[y]'/
	KK = LEN(MSG)
	IF (MSG(KK:KK) .EQ. DELIM) KK = KK - 1
	TEST = MSG(KK-2:KK)
	CALL PRINTX(MSG)
	call cstring(string_in,nchar)
	IF ((NCHAR.GT.0) .AND. (string_in(1:1).EQ.BLANK))
     1    NCHAR = 0
	IF (NCHAR .EQ. 0) THEN
	  IF ((TEST .EQ. UCY) .OR. (TEST .EQ. LCY)) THEN
	    IANS = 1
	  ELSE 
	    IANS = 0
	  END IF
	ELSE
	  charin = string_in(1:1)
	  IF (CHARIN .EQ. UCY(2:2) .OR. CHARIN .EQ. LCY(2:2)) THEN
	    IANS = 1
	  ELSE
	    IANS = 0
	  END IF
	END IF
	RETURN
	END
	function lenc(string)
C+
C	function lenc(string)
C
C	Returns length of character variable STRING excluding right-hand
C	  most blanks or nulls
C-
	character*(*) string
	length = len(string)	! total length
	if (length .eq. 0) then
	  lenc = 0
	  return
	end if
	if(ichar(string(length:length)).eq.0)string(length:length) = ' '
	do j=length,1,-1
	  lenc = j
	  if (string(j:j).ne.' ' .and. ichar(string(j:j)).ne.0) return
	end do
	lenc = 0
	return
	end
C+
	SUBROUTINE LRATIO(JR,DIP,STRIKE,RAKE,XYZ,VPVS3,LOGRAT,TOP,bot,
     1             CUTP,CUTS,FLAG)
c
c  calculates theoretical value of log10 amplitude ratio for a given
c  fault dip, strike, and slip
C	SV/P for JR = 1, SH/P for JR = 2, SV/SH for JR = 3
C	Uses A&R convention for RAKE - negative of Kisslinger's
C
C	TOP contains the sign of the S polarity: + for SV if towards
C	  the station (down on vertical), + for SH if to
C	  left facing station (opposite from others, sorry).  For SV/SH
C	  it will be for SV.
C	Written by Arthur Snoke Virginia Tech June 1984
C	10 June 1986:  Fixed case for 0/0 by taking derivatives of
C	  both numerator and denominator with respect to strike
C	  (derivatives with respect to dip did not work).
C	7 July 1990:  Decided that case for 0/0 was irrelevant.  If one
C	  is near a nodal surface for either the numerator or denominator,
C	  that should suffice in practise.  For FOCMEC therefore simply
C	  want it flagged if near a nodal surface.
C	8 July 1990:  Intorduced CUT as a calling argument to limit the
C	  range of ratios considered to stay away from nodal surfaces.
C	  If both the numerator and denominator are less than CUT,
C	  LOGRAT returned as -3, if only num., as -2, den. +2.
C	29 August 1991:  adapted from LSPRAT to include SV/SH
C	3 August 1993:  Changed procedure for dealing with near-nodal
C	  cases:  Now separate CTP and CUTS for P and S terms.  No more
C	  FACTOR.  Returns a FLAG for near-nodal -- 'NUM' if numerator,
C	  'DEN if denominator, 'N&D' if both, '   ' if non-nodal.
C	6 January 1997: Corrected an error in solutions near a
C	  nodal surface for either the numerator or the denominator
C	  but not both.  Previously the ratio returned was not updated
C	  for such cases, so was what had been found in the previous call.
C	27 February 1997: If numerator is near nodal surface, now returns
C	  correct sign in TOP
C-
	character*3 flag
	REAL*4 LOGRAT,XYZ(9),A(3),N(3)
	A(1) = COS(RAKE)*COS(STRIKE) + SIN(RAKE)*COS(DIP)*SIN(STRIKE)
	A(2) = COS(RAKE)*SIN(STRIKE) - SIN(RAKE)*COS(DIP)*COS(STRIKE)
	A(3) = -SIN(RAKE)*SIN(DIP)
	N(1) = -SIN(STRIKE)*SIN(DIP)
	N(2) = COS(STRIKE)*SIN(DIP)
	N(3) = -COS(DIP)
	RA = 0.0
	RN = 0.0
	TA = 0.0
	TN = 0.0
	PA = 0.0
	PN = 0.0
	DO J=1,3
	  RA = RA + XYZ(J)*A(J)
	  RN = RN + XYZ(J)*N(J)
	  TA = TA + XYZ(J+3)*A(J)
	  TN = TN + XYZ(J+3)*N(J)
	  PA = PA + XYZ(J+6)*A(J)
	  PN = PN + XYZ(J+6)*N(J)
	END DO
	if (JR .lt. 3) then
	  BOT = 2*RA*RN
	else
	  BOT = RA*PN + RN*PA
	end if
	if (JR .eq. 2) then
	  TOP = RA*PN + RN*PA
	else
	  TOP = RA*TN + RN*TA
	end if
	flag = '   '
	IF (JR.ne.3.and.ABS(BOT).LE.CUTP .or. JR.eq.3.and.ABS(BOT)
     1      .le.CUTS) THEN
	  IF (ABS(TOP) .LE. CUTS) THEN
	    flag = 'N&D'
	    lograt = 0.0
	    RETURN
	  ELSE
	    flag = 'DEN'
	    if (jr .ne. 3) then
	      bot = CUTP
	    else
	      bot = CUTS
	    end if
	  END IF
	ELSE IF (ABS(TOP) .LE. CUTS) THEN
	  flag = 'NUM'
	  top = CUTS*sign(1.0,top)
	end if
	IF (JR .NE. 3) THEN
	  LOGRAT = ALOG10(VPVS3*ABS(TOP/BOT))
	ELSE
	  LOGRAT = ALOG10(ABS(TOP/BOT))
	end if
	RETURN
	END
C+
	SUBROUTINE MAXMIN(DATA,NPTS,RMIN,RMAX,ITYPE,NU1,NU2)
C 
C     Returns (real) minimum and maximum values for an array DATA
C	Number of points to be included is NPTS
C	DATA can be INTEGER*2, INTEGER*4, REAL or DOUBLE PRECISION
C		If ITYPE = 'SP' or 'R4', single precision
C		           'DP' or 'R8', double precision
C		           'I2', integer*2
C		           'I4', integer*4
C	If NU1 > 0, prints RMAX and RMIN on unit NU1.  Same for NU2.
C		Arthur Snoke:  February 1983
C	6 October 1988:  If NU1 or NU2 are 5, writes to *  (VAX)
C	7 July 1991:  sun version:  calls minmax
C-
	INTEGER*2 DATA(*)
	character*2 itype
	NSTART = 1
	call minmax(data,nstart,npts,rmin,rmax,itype,nu1,nu2)
	RETURN
	END 
C+
	SUBROUTINE MINMAX(DATA,NSTART,NPTS,RMIN,RMAX,ITYPE,NU1,NU2)
C 
C     Returns (real) minimum and maximum values for an array DATA
C	Number of points to be included is NPTS
C	DATA can be INTEGER*2, INTEGER*4, REAL or DOUBLE PRECISION
C		If ITYPE = 'SP' or 'R4', single precision
C		           'DP' or 'R8', double precision
C		           'I2', integer*2
C		           'I4', integer*4
C	If NU1 > 0, prints RMAX and RMIN on unit NU1.  Same for NU2.
C		Arthur Snoke:  February 1983
C	6 October 1988:  If NU1 or NU2 are 5, writes to *  (VAX)
C	7 July 1991:  unix/sun version
C-
	CHARACTER*2 ICHECK,ITYPE,SP,R4,DP,R8,I2,I4 
	INTEGER*2 DATA(*),IDAT2(4)
	INTEGER*4 IDAT4,MIN,MAX
	real*8 rdat8
	EQUIVALENCE (RDAT4,IDAT4,IDAT2(1),rdat8)
	DATA SP,R4,DP,R8,I2,I4/'SP','R4','DP','R8','I2','I4'/
	RMAX = -1.0E30 
	RMIN = 1.0E30
	ICHECK = ITYPE
	IF (ICHECK .EQ. SP) ICHECK = R4
	IF (ICHECK .EQ. DP) ICHECK = R8
	IF (ICHECK .EQ. I2) NST = NSTART
        IF (ICHECK .EQ. I4 .OR. ICHECK .EQ. R4) NST = 2*NSTART - 1
        IF (ICHECK .EQ. R8) NST = 4*NSTART - 3
        IF (ICHECK .EQ. I2) NTOT = NST - 1 + NPTS
        IF (ICHECK .EQ. R4 .OR. ICHECK .EQ. I4) NTOT = NST - 1 + 2*NPTS
        IF (ICHECK .EQ. R8) NTOT = NST - 1 + 4*NPTS
        IF (ICHECK .EQ. I2) INTRVL = 1
        IF (ICHECK .EQ. R4 .OR. ICHECK .EQ. I4) INTRVL = 2
        IF (ICHECK .EQ. R8) INTRVL = 4
        DO 100 J=NST,NTOT,INTRVL
	IDAT2(1) = DATA(J)
	IDAT2(2) = DATA(J+1)
	IF (ICHECK .EQ. I2) RDAT = IDAT2(1)
	IF (ICHECK .EQ. I4) RDAT = IDAT4
	IF (ICHECK .EQ. R4) RDAT = RDAT4
	if (ICHECK .EQ. R8) then
	  IDAT2(3) = DATA(J+2)
	  IDAT2(4) = DATA(J+3)
	  RDAT = RDAT8
	END IF
	IF (RDAT .LT. RMIN) RMIN = RDAT
100	IF (RDAT .GT. RMAX) RMAX = RDAT
	IF (NU1 .LE. 0) RETURN
	IF (ICHECK .EQ. I4 .OR. ICHECK .EQ. I2) GO TO 200
	HALF = 0.5*ABS(RMIN-RMAX)
	IF (NU1 .NE. 5) THEN
	  WRITE (NU1,1) RMIN,RMAX,HALF
	ELSE
	  WRITE (*,1) RMIN,RMAX,HALF
	END IF
1	FORMAT(' Minimum is ',1PG10.3,'   Maximum is ',G10.3,
     .          '  Half Range is ',G10.3)
	IF (NU2 .GT. 0) THEN
	  IF (NU2 .NE. 5) THEN
	    WRITE (NU2,1) RMIN,RMAX,HALF
	  ELSE
	    WRITE (*,1) RMIN,RMAX,HALF
	  END IF
	END IF
	RETURN
200	MIN = RMIN
	MAX = RMAX
	IF (NU1 .NE. 5) THEN
	  WRITE (NU1,2) MIN,MAX
	ELSE
	  WRITE (*,2) MIN,MAX
	END IF
2	FORMAT(' Minumum is ',I10,'   Maximum is ',I10)
	IF (NU2 .GT. 0) THEN
	  IF (NU2 .NE. 5) THEN
	    WRITE (NU2,2) MIN,MAX
	  ELSE
	    WRITE (*,2) MIN,MAX
	  END IF
	END IF
	RETURN
	END 
C+
	subroutine mt_in(pttp,PI)
C
C	eigenvalues/vectors using EISPACK routines from www.netlib.no
C	Much of code adapted from Jost/Herrmann mteig.f and mtdec.f
C	Uses original EISPACK routines for TRED2 and IMTQL2, not NR
C	Also includes subroutine eig, which calls TRED2 and IMTQL2.
C	15 March 2002
C-
	dimension A(3,3), U(3,3), W(3), PTTP(4), XYZ(3)
	real*4 MRR, MTT, MPP, MRT, MRP, MTP, isotrop
      write(*,*) 'Input MRR MTT MPP MRT MRP MTP (free format)'
      read(*,*) MRR, MTT, MPP, MRT, MRP, MTP
      write(2,*) '  Input is moment tensor (Dziewonski convention)'
      write(2,*) 'MRR MTT MPP MRT MRP MTP'
      write(2,'(1p6g11.4)') MRR, MTT, MPP, MRT, MRP, MTP
C
C	Convention is X north, Y east, Z down
C
      A(3,3) = MRR
      A(1,1) = MTT
      A(2,2) = MPP
      A(1,3) = MRT
      A(3,1) = A(1,3)
      A(2,3) = -MRP
      A(3,2) = A(2,3)
      A(1,2) = -MTP
      A(2,1) = A(1,2)
      call eig(a,u,w)
C
C	Ordering returned is from smallest to highest (P, B, T for DC)
C
C      write(*,*) ' '
C     write(*,*) 'EIGENVALUES                EIGENVECTORS'
C     do j=1,3
C       write(*,'(1pg11.4,2x,0p5f11.4)') W(j),(U(i,j),i=1,3)
C     end do
      isotrop = 0.0
      do j=1,3
        isotrop = isotrop + w(j)
      end do
      devmom = 0.0
      do j=1,3
        w(j) = w(j) - isotrop/3.0
	devmom = devmom + w(j)*w(j)
      end do
      devmom = sqrt(0.5*devmom)
      if (devmom .lt. 0.001*isotrop) devmom = 0.0
      write(*,*) ' '
      write(*,'(a,1pg11.4,a,g11.4)') 
     1  '  Trace of moment tensor = ',isotrop,
     2  '  Deviatoric tensor moment = ', devmom
      write(2,*) ' '
      write(2,'(a,1pg11.4,a,g11.4)') 
     1  '  Trace of moment tensor = ',isotrop,
     2  '  Deviatoric tensor moment = ', devmom
      if (devmom .eq. 0.0) then
        write(2,*) 'Exiting because purely isotropic source'
	write(*,*) 'Exiting because purely isotropic source'
	stop
      end if
      write(2,'(a,1p3g11.4)') '  Deviatoric moment tensor eigenvalues:',
     1   (w(j),j=1,3)
      write(*,'(a,1p3g11.4)') '  Deviatoric moment tensor eigenvalues:',
     1   (w(j),j=1,3)
c---- Dziewonski, Chou, Woodhouse,  JGR 1981 2825-2852
c---- eps=0 pure double couple
c---- eps=0.5 pure CLVD
      eps = abs(w(2)/amax1(-w(1),w(3)))
      eps1 = eps*200.0
      eps2 = 100.0 - eps1
      write(*,'(a)') '  EPSILON    % OF CLVD     % OF DC'
      write(*,'(f9.4,2f12.4)') eps, eps1, eps2
      write(2,'(a)') '  EPSILON    % OF CLVD     % OF DC'
      write(2,'(f9.4,2f12.4)') eps, eps1, eps2
      write(2,*) ' '
      if (eps .ge. 0.25) then
        write(2,*) ' Exiting because less than 50% double couple'
	write(*,*) ' Exiting because less than 50% double couple'
	stop
      end if
C
C	Get trend and plunge for P
C
      do j=1,3
        xyz(j) = u(j,1)
      end do
      call V2TRPL(XYZ,PTTP(1),PI)
C
C	Get trend and plunge for T
C
      do j=1,3
        xyz(j) = u(j,3)
      end do
      call V2TRPL(XYZ,PTTP(3),PI)
C     do j=1,4
C        pttp(j) = rdeg*pttp(j)
C      end do
C      write(*,*) '  '
C      write(*,*) '  Trend and Plunge of P and T'
C     write(*,'(4f11.4)') pttp
      return
      end
      subroutine eig (a,u,w)
      dimension A(3,3), U(3,3), W(3), work(3)
      np = 3
      do i=1,np
        do j=1,3
	  u(i,j) = a(i,j)
	end do
      end do
      n = 3
      call tred2(np,n,a,w,work,u)
      call imtql2(np,n,w,work,u,ierr)
C
C	This system has P, B, T as a right-hand coordinate system.
C	I prefer P, T, B
C
      do j=1,3
        u(j,1) = -u(j,1)
      end do
      return
      end
c---------------
      subroutine tred2(nm,n,a,d,e,z)
c
      integer i,j,k,l,n,nm
      real a(nm,n),d(n),e(n),z(nm,n)
      real f,g,h,hh,scale
c
c     this subroutine is a translation of the algol procedure tred2,
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine reduces a real symmetric matrix to a
c     symmetric tridiagonal matrix using and accumulating
c     orthogonal similarity transformations.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        a contains the real symmetric input matrix.  only the
c          lower triangle of the matrix need be supplied.
c
c     on output
c
c        d contains the diagonal elements of the tridiagonal matrix.
c
c        e contains the subdiagonal elements of the tridiagonal
c          matrix in its last n-1 positions.  e(1) is set to zero.
c
c        z contains the orthogonal transformation matrix
c          produced in the reduction.
c
c        a and z may coincide.  if distinct, a is unaltered.
c
c     Questions and comments should be directed to Alan K. Cline,
c     Pleasant Valley Software, 8603 Altus Cove, Austin, TX 78759.
c     Electronic mail to cline@cs.utexas.edu.
c
c     this version dated january 1989. (for the IBM 3090vf)
c
c     ------------------------------------------------------------------
c
c?      call xuflow(0)
      do 100 i = 1, n
         do 100 j = i, n
  100       z(j,i) = a(j,i)
c
      do 110 i = 1, n
  110    d(i) = a(n,i)
c
      do 300 i = n, 2, -1
         l = i - 1
         h = 0.0e0
         scale = 0.0e0
         if (l .lt. 2) go to 130
c     .......... scale row (algol tol then not needed) ..........
         do 120 k = 1, l
  120    scale = scale + abs(d(k))
c
         if (scale .ne. 0.0e0) go to 140
  130    e(i) = d(l)
c
c"    ( ignore recrdeps
         do 135 j = 1, l
            d(j) = z(l,j)
            z(i,j) = 0.0e0
            z(j,i) = 0.0e0
  135    continue
c
         go to 290
c
  140    do 150 k = 1, l
            d(k) = d(k) / scale
            h = h + d(k) * d(k)
  150    continue
c
         f = d(l)
         g = -sign(sqrt(h),f)
         e(i) = scale * g
         h = h - f * g
         d(l) = f - g
c     .......... form a*u ..........
         do 170 j = 1, l
  170       e(j) = 0.0e0
c
         do 240 j = 1, l
            f = d(j)
            z(j,i) = f
            g = e(j) + z(j,j) * f
c
            do 200 k = j+1, l
               g = g + z(k,j) * d(k)
               e(k) = e(k) + z(k,j) * f
  200       continue
c
            e(j) = g
  240    continue
c     .......... form p ..........
         f = 0.0e0
c
         do 245 j = 1, l
            e(j) = e(j) / h
            f = f + e(j) * d(j)
  245    continue
c
         hh = -f / (h + h)
c     .......... form q ..........
         do 250 j = 1, l
  250       e(j) = e(j) + hh * d(j)
c     .......... form reduced a ..........
         do 280 j = 1, l
            f = -d(j)
            g = -e(j)
c
            do 260 k = j, l
  260          z(k,j) = z(k,j) + f * e(k) + g * d(k)
c
            d(j) = z(l,j)
            z(i,j) = 0.0e0
  280    continue
c
  290    d(i) = h
  300 continue
c     .......... accumulation of transformation matrices ..........
      do 500 i = 2, n
         l = i - 1
         z(n,l) = z(l,l)
         z(l,l) = 1.0e0
         h = d(i)
         if (h .eq. 0.0e0) go to 380
c
         do 330 k = 1, l
  330       d(k) = z(k,i) / h
c"    ( ignore recrdeps
c"    ( prefer vector
         do 360 j = 1, l
            g = 0.0e0
c
            do 340 k = 1, l
  340          g = g + z(k,i) * z(k,j)
c
            g = -g
c
            do 350 k = 1, l
  350          z(k,j) = z(k,j) + g * d(k)
  360    continue
c
  380    do 400 k = 1, l
  400       z(k,i) = 0.0e0
c
  500 continue
c
c"    ( prefer vector
      do 520 i = 1, n
         d(i) = z(n,i)
         z(n,i) = 0.0e0
  520 continue
c
      z(n,n) = 1.0e0
      e(1) = 0.0e0
      return
      end
      subroutine imtql2(nm,n,d,e,z,ierr)
c
      integer i,j,k,l,m,n,nm,ierr
      real d(n),e(n),z(nm,n)
      real b,c,f,g,p,r,s,tst1,tst2
c
c     this subroutine is a translation of the algol procedure imtql2,
c     num. math. 12, 377-383(1968) by martin and wilkinson,
c     as modified in num. math. 15, 450(1970) by dubrulle.
c     handbook for auto. comp., vol.ii-linear algebra, 241-248(1971).
c
c     this subroutine finds the eigenvalues and eigenvectors
c     of a symmetric tridiagonal matrix by the implicit ql method.
c     the eigenvectors of a full symmetric matrix can also
c     be found if  tred2  has been used to reduce this
c     full matrix to tridiagonal form.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        d contains the diagonal elements of the input matrix.
c
c        e contains the subdiagonal elements of the input matrix
c          in its last n-1 positions.  e(1) is arbitrary.
c
c        z contains the transformation matrix produced in the
c          reduction by  tred2, if performed.  if the eigenvectors
c          of the tridiagonal matrix are desired, z must contain
c          the identity matrix.
c
c      on output
c
c        d contains the eigenvalues in ascending order.  if an
c          error exit is made, the eigenvalues are correct but
c          unordered for indices 1,2,...,ierr-1.
c
c        e has been destroyed.
c
c        z contains orthonormal eigenvectors of the symmetric
c          tridiagonal (or full) matrix.  if an error exit is made,
c          z contains the eigenvectors associated with the stored
c          eigenvalues.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the j-th eigenvalue has not been
c                     determined after 30 iterations.
c
c     Questions and comments should be directed to Alan K. Cline,
c     Pleasant Valley Software, 8603 Altus Cove, Austin, TX 78759.
c     Electronic mail to cline@cs.utexas.edu.
c
c     this version dated january 1989. (for the IBM 3090vf)
c
c     ------------------------------------------------------------------
c
c?      call xuflow(0)
      ierr = 0
      if (n .eq. 1) go to 1001
c
      do 100 i = 2, n
  100 e(i-1) = e(i)
c
      e(n) = 0.0e0
c
      do 240 l = 1, n
         j = 0
c     .......... look for small sub-diagonal element ..........
  105    do 110 m = l, n-1
            tst1 = abs(d(m)) + abs(d(m+1))
            tst2 = tst1 + abs(e(m))
            if (tst2 .eq. tst1) go to 120
  110    continue
c
  120    p = d(l)
         if (m .eq. l) go to 240
         if (j .eq. 30) go to 1000
         j = j + 1
c     .......... form shift ..........
         g = (d(l+1) - p) / (2.0e0 * e(l))
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c *      r = pythag(g,1.0d0)
cccccccccccccccccccccccccccccccccccccccccccccccccccc
         if (abs(g).le.1.0e0) then
            r = sqrt(1.0e0 + g*g)
         else
            r = g * sqrt(1.0e0 + (1.0e0/g)**2)
         endif
cccccccccccccccccccccccccccccccccccccccccccccccccccc
         g = d(m) - p + e(l) / (g + sign(r,g))
         s = 1.0e0
         c = 1.0e0
         p = 0.0e0
c     .......... for i=m-1 step -1 until l do -- ..........
         do 200 i = m-1, l, -1
            f = s * e(i)
            b = c * e(i)
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c *         r = pythag(f,g)
cccccccccccccccccccccccccccccccccccccccccccccccccccc
            if (abs(f).ge.abs(g)) then
               r = abs(f) * sqrt(1.0e0 + (g/f)**2)
            else if (g .ne. 0.0e0) then
               r = abs(g) * sqrt((f/g)**2 + 1.0e0)
            else
               r = abs(f)
            endif
cccccccccccccccccccccccccccccccccccccccccccccccccccc
            e(i+1) = r
            if (r .eq. 0.0e0) then
c     .......... recover from underflow ..........
               d(i+1) = d(i+1) - p
               e(m) = 0.0e0
               go to 105
            endif
            s = f / r
            c = g / r
            g = d(i+1) - p
            r = (d(i) - g) * s + 2.0e0 * c * b
            p = s * r
            d(i+1) = g + p
            g = c * r - b
c     .......... form vector ..........
            do 180 k = 1, n
               f = z(k,i+1)
               z(k,i+1) = s * z(k,i) + c * f
               z(k,i) = c * z(k,i) - s * f
  180       continue
c
  200    continue
c
         d(l) = d(l) - p
         e(l) = g
         e(m) = 0.0e0
         go to 105
  240 continue
c     .......... order eigenvalues and eigenvectors ..........
      do 300 i = 1, n-1
         k = i
         p = d(i)
c
         do 260 j = i+1, n
            if (d(j) .ge. p) go to 260
            k = j
            p = d(j)
  260    continue
c
         d(k) = d(i)
         d(i) = p
c
         do 280 j = 1, n
            p = z(j,i)
            z(j,i) = z(j,k)
            z(j,k) = p
  280    continue
c
  300 continue
c
      go to 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 ierr = l
 1001 return
      end
C+
	LOGICAL FUNCTION OKPOL(A,N,NBADP,NBADSV,NBADSH)
C
C	Called by SRCHFM, which is called by FOCMEC
C	Compares observed polarities with trial mechanism
C		designated by A and N
C	NBADP and NBADS return the number of bad P and S polarities
C	  If any of the (weighted) numbers of errors is bigger than the
C	  appropriate maximum allowed, the function is .FALSE. on return
C	23 July 1985:  Added SH polarity check 
C	10 August 1985:  Added SH + P polarity option
C	27 September 1985:  included variable weighting option for
C	  polarities
C	last perturbed 12 October 1990
C	29 August 1991:  sun version.  Now can have SV polarities
C-
c	INCLUDE 'FOCMEC.INC'


C	FOCMEC.INC
C
C	INCLUDE file for FOCMEC
C	27 August 1991 for the sun now includes SV
C	  polarities and SV/SH ratios
C	2 August 1993: changed ratio evaluations near nodal planes,
C	     so left out RLOW* and RHI* and replaced CUT with CUTP
C	     and CUTS.
C	2 June 2000: Increased MAX to 2000 and made it so only one version
C		appears with the distribution (others are virtual links).
C	NOTE! For some compilers, the ! starting an on-line comment may cause
C		an error.  Simply delete the ends of line starting at the !
C		if that is the case.
C-
c  oct 2010     jh: increase max to 5000
c
	PARAMETER (MAX = 5000)
	PARAMETER (MAX2 = 2*MAX)
C
C	Input Data
C
	REAL*4 XYZ(9,MAX2)	!  R, Theta, and Phi station coordinates 
	REAL*4 PSTATN(MAX)	!  Stations with polarity data
	REAL*4 RSTATN(MAX)	!  Stations with amplitude-ratio data
C				     X = north, Y east, Z down
	INTEGER POLRTY(MAX)	!  1 for C, F, or <;  -1 for D, B, or >
	INTEGER NPOL		!  Number of Polarities read in
	INTEGER KEYPOL(MAX)	!  Keys polarity data relative to input
	REAL*4 LOGRAT(MAX)	!  Log10 of (SV/P), (SH/P), or (SV/SH)
C				   including free surface correction, etc.
	INTEGER NRAT		!  Number of ratios read in
	INTEGER KEYRAT(MAX)	!  Keys ratios data relative to input
	CHARACTER*1 SVSH(2,MAX)	! First element V for SV or H for SH,
C				  Second element polarity: F or B for SV
C				  L or R for SH (back to station) for
C	                          numerator in SV/P, SH/P, or SV/SH ratio
C
C	  Processing things
C
	INTEGER MAXSOL	!  Exit after this many accept. aols.
	REAL*4 ERR		!  Total allowed number of errors
	REAL*4 ERRP		!  Allowed number of P polarity errors
	REAL*4 ERRSV		!  Allowed number of SV polarity errors
	REAL*4 ERRSH		!  Allowed number of SH polarity errors
	REAL*4 THRESH		!  Polarity weighting threshold
C				  (1.0 if no weighting)
	REAL*4 BADPP(MAX)	!  Stations with a P polarity error
	REAL*4 BADSVP(MAX)	!  Stations with a SV polarity error
	REAL*4 BADSHP(MAX)	!  Stations with a SH polarity error
	REAL*4 WBADP(MAX)	!  Weishts of P polarity errors
	REAL*4 WBADSV(MAX)	!  Weights of SV polarity errors
	REAL*4 WBADSH(MAX)	!  Weights of SH polarity errors
	REAL*4 BADP		!  Total weight of P polarity errors
	REAL*4 BADSV		!  Total weight of SV polarity errors
	REAL*4 BADSH		!  Total weight of SH polarity errors
	INTEGER NERRR		!  Allowed number of ratio errors
	REAL*4 ERRRAT		!  Maximum allowed Log10 ratio error
	REAL*4 CALRAT(MAX)	!  Theoretical Log10 of amplitude ratio
	INTEGER WTRAT(MAX)	!  1 if that ratio used, 0 otherwise
	REAL*4 VPVS3		!  Cube of P/S velocity ratio 
	REAL*4 BTMIN,BTDEL,BTMAX	!  Search range in B axis trend
	REAL*4 BPMIN,BPDEL,BPMAX	!  Search range in B axis plunge
	REAL*4 AAMIN,AADEL,AAMAX	!  Search range in A axis angle
C					   measured from B trend
	REAL*4 CUTP		!  cutoff limit for  P radiation factor
C				   in amplitude ratios
	REAL*4 CUTS		!  cutoff limit for  S radiation factor
C				   in amplitude ratios
C
	COMMON /FOCMEC/ PSTATN,RSTATN,XYZ,RD,POLRTY,NPOL,LOGRAT,
     1	  NRAT,MAXSOL,ERRP,ERRSV,NERRR,ERRRAT,CALRAT,WTRAT,VPVS3,
     2	  BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,AAMAX,
     3	  BADPP,KEYRAT,KEYPOL,ERRSH,BADSHP,ERR,THRESH,WBADP,WBADSH,
     4    WBADSV,BADSVP,CUTP,CUTS,BADP,BADSV,BADSH
	COMMON /FMCHAR/ SVSH


	REAL*4 A(3),N(3)
	INTEGER NBADP, NBADSH, NBADSV
	OKPOL = .FALSE.
	NBADP = 0
	NBADSV = 0
	NBADSH = 0
	BADP = 0.0
	BADSV = 0.0
	BADSH = 0
	BAD = 0.0
	DO K=1,NPOL
          KKK = KEYPOL(K)
          IF (KKK-1000 .LT. 0) THEN
            KK = KKK
            JR = 1
          ELSE IF (KKK-2000 .LT. 0) THEN
            KK = KKK - 1000
            JR = 2
	    TA = 0.0
	    TN = 0.0
	    do j=1,3
              TA = TA + XYZ(J+3,KK)*A(J)
              TN = TN + XYZ(J+3,KK)*N(J)
	    end do
          ELSE
            KK = KKK - 2000
            JR = 3
	    PA = 0.0
	    PN = 0.0
	    do j=1,3
              PA = PA + XYZ(J+6,KK)*A(J)
              PN = PN + XYZ(J+6,KK)*N(J)
	    end do
          END IF
	  RA = 0.0
	  RN = 0.0
          DO J=1,3
            RA = RA + XYZ(J,KK)*A(J)
            RN = RN + XYZ(J,KK)*N(J)
          END DO
	  IF (JR .eq. 1) THEN    !  This is a P polarity
	    TEST = POLRTY(K)*2*RA*RN
	    IF (TEST .LE. 0.0) THEN
	      BADNOW = AMAX1(-TEST,THRESH)
	      BADP = BADP + BADNOW
	      BAD = BAD + BADNOW
	      IF (BAD .GT. ERR) RETURN
	      IF (BADP .GT. ERRP) RETURN
	      NBADP = NBADP + 1
	      NBAD = NBAD + 1
	      BADPP(NBADP) = PSTATN(K)
	      WBADP(NBADP) = BADNOW
	    ENDIF
	  ELSE IF (JR .eq. 2) then   ! This one is an SV polarity
	    TEST = POLRTY(K)*(RA*TN + RN*TA)
	    IF (TEST .LE. 0.0) THEN
	      BADNOW = AMAX1(-TEST,THRESH)
	      BADSV = BADSV + BADNOW
	      BAD = BAD + BADNOW
	      IF (BAD .GT. ERR) RETURN
	      IF (BADSV .GT. ERRSV) RETURN
	      NBADSV = NBADSV + 1
	      BADSVP(NBADSV) = PSTATN(K)
	      WBADSV(NBADSV) = BADNOW
	    END IF
	  ELSE IF (JR .eq. 3) then   ! This one is an SH polarity
	    TEST = POLRTY(K)*(RA*PN + RN*PA)
	    IF (TEST .LE. 0.0) THEN
	      BADNOW = AMAX1(-TEST,THRESH)
	      BADSH = BADSH + BADNOW
	      BAD = BAD + BADNOW
	      IF (BAD .GT. ERR) RETURN
	      IF (BADSH .GT. ERRSH) RETURN
	      NBADSH = NBADSH + 1
	      BADSHP(NBADSH) = PSTATN(K)
	      WBADSH(NBADSH) = BADNOW
	    END IF
	  END IF
	END DO
	OKPOL = .TRUE.
	RETURN
	END
C+
	LOGICAL FUNCTION OKRAT(DSR,NBADR,ESUM,ESUMAL,DIFF,SPOL,FLAG)
C
C	Called by OKSOL, which is called by FOCMEC
C	Compares observed log10 amplitude ratios with trial mechanism
C	  designated by A and N
C	8 July 1990:  Put in limits on ratios to allow for nodal surfaces.
C	  The limit for the numerator and denominator as used in LSPRAT is
C	  CUT.  Allowing for the other part of the ratio being non-unity is
C	  taken care of through FACTOR.  My choices are CUT=0.05, FACTOR=2.0.
C	  These have been added to the .INC file and are prompted for in FOCINP
C	29 August 1991:  sun version.  Now allows SV/SH ratios
C	3 August 1993:  Changed rules for nodal surfaces.  No more FACTOR,
C	  Now have CUTP and CUTS.  FLAG labels nodel surface cases
C-
c	INCLUDE 'FOCMEC.INC'


C	FOCMEC.INC
C
C	INCLUDE file for FOCMEC
C	27 August 1991 for the sun now includes SV
C	  polarities and SV/SH ratios
C	2 August 1993: changed ratio evaluations near nodal planes,
C	     so left out RLOW* and RHI* and replaced CUT with CUTP
C	     and CUTS.
C	2 June 2000: Increased MAX to 2000 and made it so only one version
C		appears with the distribution (others are virtual links).
C	NOTE! For some compilers, the ! starting an on-line comment may cause
C		an error.  Simply delete the ends of line starting at the !
C		if that is the case.
C-
c  oct 2010     jh: increase max to 5000
c
	PARAMETER (MAX = 5000)
	PARAMETER (MAX2 = 2*MAX)
C
C	Input Data
C
	REAL*4 XYZ(9,MAX2)	!  R, Theta, and Phi station coordinates 
	REAL*4 PSTATN(MAX)	!  Stations with polarity data
	REAL*4 RSTATN(MAX)	!  Stations with amplitude-ratio data
C				     X = north, Y east, Z down
	INTEGER POLRTY(MAX)	!  1 for C, F, or <;  -1 for D, B, or >
	INTEGER NPOL		!  Number of Polarities read in
	INTEGER KEYPOL(MAX)	!  Keys polarity data relative to input
	REAL*4 LOGRAT(MAX)	!  Log10 of (SV/P), (SH/P), or (SV/SH)
C				   including free surface correction, etc.
	INTEGER NRAT		!  Number of ratios read in
	INTEGER KEYRAT(MAX)	!  Keys ratios data relative to input
	CHARACTER*1 SVSH(2,MAX)	! First element V for SV or H for SH,
C				  Second element polarity: F or B for SV
C				  L or R for SH (back to station) for
C	                          numerator in SV/P, SH/P, or SV/SH ratio
C
C	  Processing things
C
	INTEGER MAXSOL	!  Exit after this many accept. aols.
	REAL*4 ERR		!  Total allowed number of errors
	REAL*4 ERRP		!  Allowed number of P polarity errors
	REAL*4 ERRSV		!  Allowed number of SV polarity errors
	REAL*4 ERRSH		!  Allowed number of SH polarity errors
	REAL*4 THRESH		!  Polarity weighting threshold
C				  (1.0 if no weighting)
	REAL*4 BADPP(MAX)	!  Stations with a P polarity error
	REAL*4 BADSVP(MAX)	!  Stations with a SV polarity error
	REAL*4 BADSHP(MAX)	!  Stations with a SH polarity error
	REAL*4 WBADP(MAX)	!  Weishts of P polarity errors
	REAL*4 WBADSV(MAX)	!  Weights of SV polarity errors
	REAL*4 WBADSH(MAX)	!  Weights of SH polarity errors
	REAL*4 BADP		!  Total weight of P polarity errors
	REAL*4 BADSV		!  Total weight of SV polarity errors
	REAL*4 BADSH		!  Total weight of SH polarity errors
	INTEGER NERRR		!  Allowed number of ratio errors
	REAL*4 ERRRAT		!  Maximum allowed Log10 ratio error
	REAL*4 CALRAT(MAX)	!  Theoretical Log10 of amplitude ratio
	INTEGER WTRAT(MAX)	!  1 if that ratio used, 0 otherwise
	REAL*4 VPVS3		!  Cube of P/S velocity ratio 
	REAL*4 BTMIN,BTDEL,BTMAX	!  Search range in B axis trend
	REAL*4 BPMIN,BPDEL,BPMAX	!  Search range in B axis plunge
	REAL*4 AAMIN,AADEL,AAMAX	!  Search range in A axis angle
C					   measured from B trend
	REAL*4 CUTP		!  cutoff limit for  P radiation factor
C				   in amplitude ratios
	REAL*4 CUTS		!  cutoff limit for  S radiation factor
C				   in amplitude ratios
C
	COMMON /FOCMEC/ PSTATN,RSTATN,XYZ,RD,POLRTY,NPOL,LOGRAT,
     1	  NRAT,MAXSOL,ERRP,ERRSV,NERRR,ERRRAT,CALRAT,WTRAT,VPVS3,
     2	  BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,AAMAX,
     3	  BADPP,KEYRAT,KEYPOL,ERRSH,BADSHP,ERR,THRESH,WBADP,WBADSH,
     4    WBADSV,BADSVP,CUTP,CUTS,BADP,BADSV,BADSH
	COMMON /FMCHAR/ SVSH



	character*3 FLAG(MAX)
	INTEGER NBADR
	REAL*4 SPOL(MAX),DSR(3),DIFF(MAX)
	OKRAT = .FALSE.
	NBADR = 0
	IF (NRAT .LE. 0) RETURN
	ESUM = 0.0
	ESUMAL = 0.0
	DO K=1,NRAT
	  KKK = KEYRAT(K)
	  IF (KKK-1000 .LT. 0) THEN
	    KK = KKK
	    JR = 1
	  ELSE IF (KKK-2000 .LT. 0) THEN
	    KK = KKK - 1000
	    JR = 2
	  ELSE
	    KK = KKK - 2000
	    JR = 3
	  END IF
	  CALL LRATIO(JR,DSR(1),DSR(2),DSR(3),XYZ(1,KK),VPVS3,RAT,
     .        SP,bot,CUTP,CUTS,FLAG(K))
	  SPOL(K) = SP
	  CALRAT(K) = RAT
	  IF (FLAG(K) .eq. 'N&D') THEN
c	Both the numberator and the denominator are < cutoff values.
	    DIFF(K) = 0.0
	  ELSE
	    DIFF(K) = LOGRAT(K) - CALRAT(K)
	  END IF
	  ESUMAL = ESUMAL + DIFF(K)**2
	  IF (ABS(DIFF(K)) .GT. ERRRAT) THEN
	    WTRAT(K) = 0
	    NBADR = NBADR + 1
	    IF (NBADR .GT. NERRR) RETURN
	  ELSE
	    WTRAT(K) = 1
	    ESUM = ESUM + DIFF(K)**2
	  ENDIF
	END DO
	OKRAT = .TRUE.
	RETURN
	END
C+

C+
	LOGICAL FUNCTION OKSOL(A,N,ANGLE)
C
C	Called by SRCHFM, which is called by FOCMEC
C	Checks for valid focal mechanisms based on ratios and polarities
C		designated by A and N
C	ANGLE is passed in and written out for a valid solution:
C		It is an internally caluclated quantity - the
C		angle the A axis makes with the plane defined by
C		the B axis and vertical
C
C	Arthur Snoke  Virginia Tech  July 1984
C	24 July 1985:  Added SH polarity check
C	7 July 1990:  Commented out (C90) call to perturb solution
C	  if there are ratio dats based on Kisslinger's least-squares
C	  approach.  Decided it is too misleading, as polarities are
C	  not taken into account.  (Room for improvement!)
C	Latest perturbation (VAX):  12 October 1990
C	31 August 1991:  sun   Allows for SV polarities and SV/SH ratios
C	15 May 1992:  Corrected an error in printing if near a double
C		nodal plane in ratio printout
C	5 August 1993:  Changed rules for ratios when numerator and/or
C	  denominator near nodal surfaces.  Also, if both numerator and
C	  denominator near a nodal surface for all solutions, writes out
C	  rmsall as 99.99 instead of 0.0.  FLAG has been added to output
C	  for ratio data: NUM and DEN for numerator or denominator near
C	  nodal surface respectively; N&D for both.  Summary printout for
C	  total ratio error now same as in short output file -- RMS errors.
C	  Now radiation factors (normalized to unity) are printed out even
C	  if weighted option is not chosen.
C	22 March 2002: Small changes to include fact that fmreps can allow
C	  moment tensor input -- not used in this program.
C	18 November 2008: Now with N&D for all ratios, unacceptable solution
C	  Also unacceptable if there are not more good ratios than bad ratios
C-
c	INCLUDE 'FOCMEC.INC'


C	FOCMEC.INC
C
C	INCLUDE file for FOCMEC
C	27 August 1991 for the sun now includes SV
C	  polarities and SV/SH ratios
C	2 August 1993: changed ratio evaluations near nodal planes,
C	     so left out RLOW* and RHI* and replaced CUT with CUTP
C	     and CUTS.
C	2 June 2000: Increased MAX to 2000 and made it so only one version
C		appears with the distribution (others are virtual links).
C	NOTE! For some compilers, the ! starting an on-line comment may cause
C		an error.  Simply delete the ends of line starting at the !
C		if that is the case.
C-
c  oct 2010     jh: increase max to 5000
c
	PARAMETER (MAX = 5000)
	PARAMETER (MAX2 = 2*MAX)
C
C	Input Data
C
	REAL*4 XYZ(9,MAX2)	!  R, Theta, and Phi station coordinates 
	REAL*4 PSTATN(MAX)	!  Stations with polarity data
	REAL*4 RSTATN(MAX)	!  Stations with amplitude-ratio data
C				     X = north, Y east, Z down
	INTEGER POLRTY(MAX)	!  1 for C, F, or <;  -1 for D, B, or >
	INTEGER NPOL		!  Number of Polarities read in
	INTEGER KEYPOL(MAX)	!  Keys polarity data relative to input
	REAL*4 LOGRAT(MAX)	!  Log10 of (SV/P), (SH/P), or (SV/SH)
C				   including free surface correction, etc.
	INTEGER NRAT		!  Number of ratios read in
	INTEGER KEYRAT(MAX)	!  Keys ratios data relative to input
	CHARACTER*1 SVSH(2,MAX)	! First element V for SV or H for SH,
C				  Second element polarity: F or B for SV
C				  L or R for SH (back to station) for
C	                          numerator in SV/P, SH/P, or SV/SH ratio
C
C	  Processing things
C
	INTEGER MAXSOL	!  Exit after this many accept. aols.
	REAL*4 ERR		!  Total allowed number of errors
	REAL*4 ERRP		!  Allowed number of P polarity errors
	REAL*4 ERRSV		!  Allowed number of SV polarity errors
	REAL*4 ERRSH		!  Allowed number of SH polarity errors
	REAL*4 THRESH		!  Polarity weighting threshold
C				  (1.0 if no weighting)
	REAL*4 BADPP(MAX)	!  Stations with a P polarity error
	REAL*4 BADSVP(MAX)	!  Stations with a SV polarity error
	REAL*4 BADSHP(MAX)	!  Stations with a SH polarity error
	REAL*4 WBADP(MAX)	!  Weishts of P polarity errors
	REAL*4 WBADSV(MAX)	!  Weights of SV polarity errors
	REAL*4 WBADSH(MAX)	!  Weights of SH polarity errors
	REAL*4 BADP		!  Total weight of P polarity errors
	REAL*4 BADSV		!  Total weight of SV polarity errors
	REAL*4 BADSH		!  Total weight of SH polarity errors
	INTEGER NERRR		!  Allowed number of ratio errors
	REAL*4 ERRRAT		!  Maximum allowed Log10 ratio error
	REAL*4 CALRAT(MAX)	!  Theoretical Log10 of amplitude ratio
	INTEGER WTRAT(MAX)	!  1 if that ratio used, 0 otherwise
	REAL*4 VPVS3		!  Cube of P/S velocity ratio 
	REAL*4 BTMIN,BTDEL,BTMAX	!  Search range in B axis trend
	REAL*4 BPMIN,BPDEL,BPMAX	!  Search range in B axis plunge
	REAL*4 AAMIN,AADEL,AAMAX	!  Search range in A axis angle
C					   measured from B trend
	REAL*4 CUTP		!  cutoff limit for  P radiation factor
C				   in amplitude ratios
	REAL*4 CUTS		!  cutoff limit for  S radiation factor
C				   in amplitude ratios
C
	COMMON /FOCMEC/ PSTATN,RSTATN,XYZ,RD,POLRTY,NPOL,LOGRAT,
     1	  NRAT,MAXSOL,ERRP,ERRSV,NERRR,ERRRAT,CALRAT,WTRAT,VPVS3,
     2	  BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,AAMAX,
     3	  BADPP,KEYRAT,KEYPOL,ERRSH,BADSHP,ERR,THRESH,WBADP,WBADSH,
     4    WBADSV,BADSVP,CUTP,CUTS,BADP,BADSV,BADSH
	COMMON /FMCHAR/ SVSH



	CHARACTER*1 POL,MARK(2)
	character*3 flag(MAX)
	LOGICAL OKRAT,OKPOL,PT,AN,ANGS,mt
	REAL*4 DSR(3), DSRD(3),PTTP(4),ANBTP(6),DSR2(3),DIFF(MAX)
	REAL*4 SPOL(MAX),A(3),N(3),MOMTEN(6)
	DATA PT,AN,mt,ANGS/3*.FALSE.,.TRUE./
	DATA MARK/'*',' '/
	save
	OKSOL = .FALSE.
	IF (NPOL .GT. 0) THEN
	  IF (.NOT.OKPOL(A,N,NBADP,NBADSV,NBADSH)) RETURN
C
	ELSE
	  NBADP = 0
	  NBADSV = 0
	  NBADSH = 0
	  BADP = 0.0
	  BADSV = 0.0
	  BADSH = 0.0
	END IF
	do j=1,3
	  if (abs(A(j)) .lt. 0.0001) A(j) = 0.0
	  if (abs(abs(a(j))-1.0) .lt. 0.0001) a(j) = a(j)/abs(a(j))
	  if (abs(N(j)) .lt. 0.0001) N(j) = 0.0
	  if (abs(abs(N(j))-1.0) .lt. 0.0001) N(j) = N(j)/abs(N(j))
	end do
	pi = 4.0*atan(1.0)
	CALL AN2DSR(A,N,DSR,PI)
	IF (NRAT .GT. 0) THEN
	  IF (.NOT.OKRAT(DSR,NBADR,ESUM,ESUMAL,DIFF,SPOL,FLAG)) 
     1        RETURN
C
C	DON'T WANT TO COUNT SOLUTIONS NEAR DOUBLE NODAL SURFACE
C
	  NRATOK = NRAT
	  DO K=1,NRAT
	    IF (FLAG(K) .eq. 'N&D') NRATOK = NRATOK - 1
	  END DO
C
	  if (nratok .gt. 0) then
	    RMSALL = SQRT(ESUMAL/NRATOK)
	  else
	    return
	  endif
	  if (nratok .le. nbadr) return
	ENDIF
	OKSOL = .TRUE.
	DO K=1,3
	  DSRD(K) = DSR(K)*RD
	END DO
	CALL FMREPS(ANBTP,DSRD,PTTP,DSR2,AN,PT,ANGS,MT,MOMTEN,2,0)
	ANGDEG = ANGLE*57.29578
	WRITE(2,1) ANGDEG
	IF (NRAT .GT. 0) THEN
	  RMS = SQRT(ESUM/(NRATOK-NBADR))
	  WRITE(*,3) DSRD,BADP,BADSV,BADSH,NBADR,RMS,RMSALL
	  WRITE(3,3) DSRD,BADP,BADSV,BADSH,NBADR,RMS,RMSALL
	ELSE
	  WRITE(*,3) DSRD,BADP,BADSV,BADSH
	  WRITE(3,3) DSRD,BADP,BADSV,BADSH
	END IF
	IF (NBADP .GT. 0) THEN
	  IF (NBADP .LE. 11) THEN
	    WRITE(2,5) (BADPP(KK),KK=1,NBADP)
	    WRITE(2,11) (WBADP(KK),KK=1,NBADP)
	  ELSE
	    WRITE(2,5) (BADPP(KK),KK=1,11)
	    WRITE(2,11) (WBADP(KK),KK=1,11)
	    WRITE(2,'(20(T23,11(A4,1X)/))') (BADPP(KK),KK=12,NBADP)
	    WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADP(KK),KK=12,NBADP)
	  END IF
	  WRITE(2,12) BADP
	END IF
	IF (NBADSV .GT. 0) THEN
	  IF (NBADSV .LE. 11) THEN
	    WRITE(2,10) (BADSVP(KK),KK=1,NBADSV)
	    WRITE(2,13) (WBADSV(KK),KK=1,NBADSV)
	  ELSE
	    WRITE(2,10) (BADSVP(KK),KK=1,11)
	    WRITE(2,13) (WBADSV(KK),KK=1,11)
	    WRITE(2,'(20(T23,11(A4,1X)/))') (BADSVP(KK),KK=12,NBADSV)
	    WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADSV(KK),KK=12,NBADSV)
	  END IF
	  WRITE(2,14) BADSV
	END IF
	IF (NBADSH .GT. 0) THEN
	  IF (NBADSH .LE. 11) THEN
	    WRITE(2,15) (BADSHP(KK),KK=1,NBADSH)
	    WRITE(2,16) (WBADSH(KK),KK=1,NBADSH)
	  ELSE
	    WRITE(2,15) (BADSHP(KK),KK=1,11)
	    WRITE(2,16) (WBADSH(KK),KK=1,11)
	    WRITE(2,'(20(T23,11(A4,1X)/))') (BADSHP(KK),KK=12,NBADSH)
	    WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADSH(KK),KK=12,NBADSH)
	  END IF
	  WRITE(2,17) BADSH
	END IF
	IF (NRAT .LE. 0) THEN
	  WRITE(2,6)
	  RETURN
	END IF
	WRITE(2,7)
	DO K=1,NRAT
	  IF (SVSH(1,K) .EQ. 'V' .AND. SPOL(K) .GT. 0.0) POL = 'F'
	  IF (SVSH(1,K) .EQ. 'V' .AND. SPOL(K) .LT. 0.0) POL = 'B'
	  IF (SVSH(1,K) .EQ. 'S' .AND. SPOL(K) .GT. 0.0) POL = 'F'
	  IF (SVSH(1,K) .EQ. 'S' .AND. SPOL(K) .LT. 0.0) POL = 'B'
	  IF (SVSH(1,K) .EQ. 'H' .AND. SPOL(K) .GT. 0.0) POL = 'L'
	  IF (SVSH(1,K) .EQ. 'H' .AND. SPOL(K) .LT. 0.0) POL = 'R'
	  WRITE(2,8) LOGRAT(K),CALRAT(K),DIFF(K),MARK(WTRAT(K)+1),
     .	    RSTATN(K),SVSH(1,K),SVSH(2,K),POL,FLAG(K)
	END DO
	WRITE(2,9) RMSALL, RMS
	WRITE(2,6)
	RETURN
C
1	FORMAT(' Angle of "A" with vertical B trend plane is ',F5.1)
3	FORMAT(3F8.2,T30,3F6.1,I9,F10.2,F10.2)
4	FORMAT(3F8.2,T30,3F6.1,I9,10X,F10.2)
5	FORMAT(/' P Polarity error at',T23,11(A4,1X))
6	FORMAT(' ',79('+')/)
7	FORMAT(/T11,'Log10(Ratio)',T53,'Ratio',
     .    T63,'S Polarity'/T6,'Observed',
     1    T16,'Calculated',T30,
     2    'Difference',T42,'Station',T53,' Type',T63,'Obs.',
     3    T69,'Calc.',T75,'Flag')
8	FORMAT(4X,F8.4,T17,F8.4,T30,F8.4,T43,A1,A4,T55,'S',A1,T64,A1,
     .    T71,A1,T76,A3)
9	FORMAT(/' The RMS log amplitude ratio error for all solutions is',
     1    F7.3/' The error for acceptable solutions is',F6.3/)
10	FORMAT(/' SV Polarity error at',T24,11(A4,1X))
11	FORMAT(' P Polarity weights:',T23,11(F4.2,1X))
12	FORMAT('  Total P polarity weight is',F7.3)
13	FORMAT(' SV Polarity weights:',T24,11(F4.2,1X))
14	FORMAT('  Total SV polarity weight is',F7.3)
15	FORMAT(/' SH Polarity error at',T24,11(A4,1X))
16	FORMAT(' SH Polarity weights:',T24,11(F4.2,1X))
17	FORMAT('  Total SH polarity weight is',F7.3)
	END



c         LOGICAL FUNCTION OKSOL_OLD(A,N,ANGLE)   ! jh 19-11-08
C
C	Called by SRCHFM, which is called by FOCMEC
C	Checks for valid focal mechanisms based on ratios and polarities
C		designated by A and N
C	ANGLE is passed in and written out for a valid solution:
C		It is an internally caluclated quantity - the
C		angle the A axis makes with the plane defined by
C		the B axis and vertical
C
C	Arthur Snoke  Virginia Tech  July 1984
C	24 July 1985:  Added SH polarity check
C	7 July 1990:  Commented out (C90) call to perturb solution
C	  if there are ratio dats based on Kisslinger's least-squares
C	  approach.  Decided it is too misleading, as polarities are
C	  not taken into account.  (Room for improvement!)
C	Latest perturbation (VAX):  12 October 1990
C	31 August 1991:  sun   Allows for SV polarities and SV/SH ratios
C	15 May 1992:  Corrected an error in printing if near a double
C		nodal plane in ratio printout
C	5 August 1993:  Changed rules for ratios when numerator and/or
C	  denominator near nodal surfaces.  Also, if both numerator and
C	  denominator near a nodal surface for all solutions, writes out
C	  rmsall as 99.99 instead of 0.0.  FLAG has been added to output
C	  for ratio data: NUM and DEN for numerator or denominator near
C	  nodal surface respectively; N&D for both.  Summary printout for
C	  total ratio error now same as in short output file -- RMS errors.
C	  Now radiation factors (normalized to unity) are printed out even
C	  if weighted option is not chosen.
C	22 March 2002: Small changes to include fact that fmreps can allow
C	  moment tensor input -- not used in this program.
C-
c       INCLUDE 'FOCMEC.INC'
cCHARACTER*1 POL,MARK(2)
ccharacter*3 flag(MAX)
cLOGICAL OKRAT,OKPOL,PT,AN,ANGS,mt
cREAL*4 DSR(3), DSRD(3),PTTP(4),ANBTP(6),DSR2(3),DIFF(MAX)
cREAL*4 SPOL(MAX),A(3),N(3),MOMTEN(6)
cDATA PT,AN,mt,ANGS/3*.FALSE.,.TRUE./
cDATA MARK/'*',' '/
cOKSOL = .FALSE.
cIF (NPOL .GT. 0) THEN
c  IF (.NOT.OKPOL(A,N,NBADP,NBADSV,NBADSH)) RETURN
C
cELSE
c  NBADP = 0
c  NBADSV = 0
c  NBADSH = 0
c  BADP = 0.0
c  BADSV = 0.0
c  BADSH = 0.0
cEND IF
cdo j=1,3
c  if (abs(A(j)) .lt. 0.0001) A(j) = 0.0
c  if (abs(abs(a(j))-1.0) .lt. 0.0001) a(j) = a(j)/abs(a(j))
c  if (abs(N(j)) .lt. 0.0001) N(j) = 0.0
c  if (abs(abs(N(j))-1.0) .lt. 0.0001) N(j) = N(j)/abs(N(j))
cend do
cpi = 4.0*atan(1.0)
cCALL AN2DSR(A,N,DSR,PI)
cIF (NRAT .GT. 0) THEN
c  IF (.NOT.OKRAT(DSR,NBADR,ESUM,ESUMAL,DIFF,SPOL,FLAG)) 
c    1        RETURN
c
cENDIF
cOKSOL = .TRUE.
cDO K=1,3
c  DSRD(K) = DSR(K)*RD
cEND DO
cCALL FMREPS(ANBTP,DSRD,PTTP,DSR2,AN,PT,ANGS,MT,MOMTEN,2,0)
cANGDEG = ANGLE*57.29578
cWRITE(2,1) ANGDEG
cIF (NRAT .GT. 0) THEN
c
c	DON'T WANT TO COUNT SOLUTIONS NEAR DOUBLE NODAL SURFACE
c
c  NRATOK = NRAT
c  DO K=1,NRAT
c    IF (FLAG(K) .eq. 'N&D') NRATOK = NRATOK - 1
c  END DO
c
c  if (nratok .gt. 0) then
c    RMSALL = SQRT(ESUMAL/NRATOK)
c  else
c    rmsall = 99.99
c  end if
c  IF (NRATOK .GT. NBADR) THEN
c    RMS = SQRT(ESUM/(NRATOK-NBADR))
c    WRITE(*,3) DSRD,BADP,BADSV,BADSH,NBADR,RMS,RMSALL
c    WRITE(3,3) DSRD,BADP,BADSV,BADSH,NBADR,RMS,RMSALL
c  ELSE
c    RMS = 99.99
c    WRITE(*,4) DSRD,BADP,BADSV,BADSH,NBADR,RMSALL
c    WRITE(3,4) DSRD,BADP,BADSV,BADSH,NBADR,RMSALL
c  END IF
cELSE
c  WRITE(*,3) DSRD,BADP,BADSV,BADSH
c  WRITE(3,3) DSRD,BADP,BADSV,BADSH
cEND IF
cIF (NBADP .GT. 0) THEN
c  IF (NBADP .LE. 11) THEN
c    WRITE(2,5) (BADPP(KK),KK=1,NBADP)
c    WRITE(2,11) (WBADP(KK),KK=1,NBADP)
c  ELSE
c    WRITE(2,5) (BADPP(KK),KK=1,11)
c    WRITE(2,11) (WBADP(KK),KK=1,11)
c    WRITE(2,'(20(T23,11(A4,1X)/))') (BADPP(KK),KK=12,NBADP)
c    WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADP(KK),KK=12,NBADP)
c  END IF
c  WRITE(2,12) BADP
cEND IF
cIF (NBADSV .GT. 0) THEN
c  IF (NBADSV .LE. 11) THEN
c    WRITE(2,10) (BADSVP(KK),KK=1,NBADSV)
c    WRITE(2,13) (WBADSV(KK),KK=1,NBADSV)
c  ELSE
c    WRITE(2,10) (BADSVP(KK),KK=1,11)
c    WRITE(2,13) (WBADSV(KK),KK=1,11)
c    WRITE(2,'(20(T23,11(A4,1X)/))') (BADSVP(KK),KK=12,NBADSV)
c    WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADSV(KK),KK=12,NBADSV)
c  END IF
c  WRITE(2,14) BADSV
cEND IF
cIF (NBADSH .GT. 0) THEN
c  IF (NBADSH .LE. 11) THEN
c    WRITE(2,15) (BADSHP(KK),KK=1,NBADSH)
c    WRITE(2,16) (WBADSH(KK),KK=1,NBADSH)
c  ELSE
c    WRITE(2,15) (BADSHP(KK),KK=1,11)
c    WRITE(2,16) (WBADSH(KK),KK=1,11)
c    WRITE(2,'(20(T23,11(A4,1X)/))') (BADSHP(KK),KK=12,NBADSH)
c    WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADSH(KK),KK=12,NBADSH)
c  END IF
c  WRITE(2,17) BADSH
cEND IF
cIF (NRAT .LE. 0) THEN
c  WRITE(2,6)
c  RETURN
cEND IF
cWRITE(2,7)
cDO K=1,NRAT
c  IF (SVSH(1,K) .EQ. 'V' .AND. SPOL(K) .GT. 0.0) POL = 'F'
c  IF (SVSH(1,K) .EQ. 'V' .AND. SPOL(K) .LT. 0.0) POL = 'B'
c  IF (SVSH(1,K) .EQ. 'S' .AND. SPOL(K) .GT. 0.0) POL = 'F'
c  IF (SVSH(1,K) .EQ. 'S' .AND. SPOL(K) .LT. 0.0) POL = 'B'
c  IF (SVSH(1,K) .EQ. 'H' .AND. SPOL(K) .GT. 0.0) POL = 'L'
c  IF (SVSH(1,K) .EQ. 'H' .AND. SPOL(K) .LT. 0.0) POL = 'R'
c  WRITE(2,8) LOGRAT(K),CALRAT(K),DIFF(K),MARK(WTRAT(K)+1),
c    .	    RSTATN(K),SVSH(1,K),SVSH(2,K),POL,FLAG(K)
cEND DO
cWRITE(2,9) RMSALL, RMS
cWRITE(2,6)
cRETURN
c
c	FORMAT(' Angle of "A" with vertical B trend plane is ',F5.1)
c	FORMAT(3F8.2,T30,3F6.1,I9,F10.2,F10.2)
c	FORMAT(3F8.2,T30,3F6.1,I9,10X,F10.2)
c	FORMAT(/' P Polarity error at',T23,11(A4,1X))
c	FORMAT(' ',79('+')/)
c	FORMAT(/T11,'Log10(Ratio)',T53,'Ratio',
c    .    T63,'S Polarity'/T6,'Observed',
c    1	  T16,'Calculated',T30,
c    2	  'Difference',T42,'Station',T53,' Type',T63,'Obs.',
c    3	  T69,'Calc.',T75,'Flag')
c	FORMAT(4X,F8.4,T17,F8.4,T30,F8.4,T43,A1,A4,T55,'S',A1,T64,A1,
c    .	  T71,A1,T76,A3)
c	FORMAT(/' The RMS log amplitude ratio error for all solutions is',
c    1	  F7.3/' The error for acceptable solutions is',F6.3/)
c0	FORMAT(/' SV Polarity error at',T24,11(A4,1X))
c1	FORMAT(' P Polarity weights:',T23,11(F4.2,1X))
c2	FORMAT('  Total P polarity weight is',F7.3)
c3	FORMAT(' SV Polarity weights:',T24,11(F4.2,1X))
c4	FORMAT('  Total SV polarity weight is',F7.3)
c5	FORMAT(/' SH Polarity error at',T24,11(A4,1X))
c6	FORMAT(' SH Polarity weights:',T24,11(F4.2,1X))
c7	FORMAT('  Total SH polarity weight is',F7.3)
cEND






	SUBROUTINE PRINTX(LINE)
C+
c	SUBROUTINE PRINTX(LINE)
C  OUTPUTS A MESSAGE TO THE TERMINAL
C  PRINTX STARTS WITH A LINE FEED BUT DOES NOT END WITH A CARRIAGE RETURN
C  THE PRINT HEAD REMAINS AT THE END OF THE MESSAGE
C
C  IF THE MESSAGE LENGTH IS LESS THAN 40,
C	DOTS ARE INSERTED UP TO COL. 39
C	AND A COLON IS PUT IN COL. 40.
C
C  USE FOR CONVERSATIONAL INTERACTION
C			Alan Linde ... April 1980.
C	10 Sugust 1985:  Corrected a minor error for  strings > 40 bytes
C	20 June 1986:  Made it compatible with Fortran 77
C	24 September 2001: On some platforms there are problems when one
C		writes into column 1.  So the write sstatement now has a 
C		1x to start out.
C-
	character*(*) line
	CHARACTER*60 BUF
	CHARACTER*2 COLON
	CHARACTER*1 DOT,DELIM
	DATA DELIM/'$'/,DOT/'.'/,COLON/': '/
	KK = lenc(LINE) !  length minus right-hand blanks
	  IF (LINE(KK:KK) .EQ. DELIM) KK = KK - 1
	  IF (KK .GT. 58) KK = 59
	BUF(1:KK) = LINE(1:KK)
	IF (KK .LT. 49) THEN
	  DO J=KK+1,49
	    BUF(J:J) = DOT
	  END DO
	  KK = 49
	END IF
	BUF(KK:KK+1) = COLON
	KK = KK + 2
	WRITE(*,'(1x,A,$)') BUF(1:KK)
	RETURN
	END
C+
	SUBROUTINE PTTPIN (PTTP,ANGS,ANGS2,ANBTP,MOMTEN,PI)
C
C	Calculates other representations of fault planes with
C		trend and plunge of P and T as input.  All
C		angles are in radians.
C	22 July 1985:  Added moment tensor output
C-
	REAL N(3),MOMTEN(6)
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6),P(3),T(3),A(3),B(3)
	DATA SR2/0.707107/
	CALL TRPL2V(PTTP(1),P)
	CALL TRPL2V(PTTP(3),T)
	DO 100 J=1,3
	  A(J) = SR2*(P(J) + T(J))
	  N(J) = SR2*(T(J) - P(J))
100	CONTINUE
	B(1) = P(2)*T(3) - P(3)*T(2)
	B(2) = P(3)*T(1) - P(1)*T(3)
	B(3) = P(1)*T(2) - P(2)*T(1)
	CALL V2TRPL(A,ANBTP(1),PI)
	CALL V2TRPL(N,ANBTP(3),PI)
	CALL V2TRPL(B,ANBTP(5),PI)
	CALL AN2DSR(A,N,ANGS,PI)
	CALL AN2DSR(N,A,ANGS2,PI)
	CALL AN2MOM(A,N,MOMTEN)
	RETURN
	END
C+
	REAL FUNCTION RVALUE(MSG,RDEF)
C
C PURPOSE:
C		THIS FUNCTION ACCEPTS A MESSAGE (ASKING FOR A VALUE)
C		AND RETURNS THE VALUE ENTERED AT THE TERMINAL
C ROUTINES CALLED:
C		PRINTX
C
C USE:
C		ANS=RVALUE('ENTER AN REAL NUMBER',RDEF)
C	If enter a cariage return, rvalue is set to RDEF
C
C AUTHOR:
C			ALAN LINDE ... AUGUST 1980 (for VALUE)
C
C EXTENSIONS:
C	30 JULY 1989:  CAN HANDLE ENTRY FOLLOWED BY A BLANK OR TAB
C       27 July 1993: Did input read through cstring so can have 
C         comment lines
C	19 July 2002: PCs had a problem with single-digit integers, so ...
C-
	CHARACTER*1 E/'E'/,BLANK/' '/
	CHARACTER*30 STUFF
	CHARACTER*(*) MSG
C
100	CALL PRINTX(MSG)
	call cstring(stuff,nin)
	IF (NIN .GT. 0) THEN
	  NBLANK = INDEX(STUFF(1:NIN),BLANK)
	  IF (NBLANK .GT. 0) NIN = NBLANK - 1
	END IF
	IF (NIN .EQ. 0) THEN
	  RVALUE = RDEF
	ELSE
	  if (nin .eq. 1) then
	    stuff(2:2) = stuff(1:1)
	    stuff(1:1) = '0'
	    nin = 2
	  end if
	  READ(STUFF(1:NIN),*,ERR=100) RVALUE
	END IF
	RETURN
	END
C+
	SUBROUTINE SRCHFM
C
C	Subroutine called by FOCMEC
C
C	Using parameters established in FOCINP it searches the
C	focal sphere for acceptable solutions based on polarity
C	and/or amplitude ratios.  The logic of the search
C	is as follows:  One chooses an orientation for the null
C	or B axis (the trend and plunge).  To cover the focal
C	sphere, the trend would vary from 0 to 360 degrees and
C	the plunge from 0 to 90.  All possible focal mechanisms
C	will then be included if the A axis (Herrmann's X axis)
C	varies from 0 to 180 degrees.  However, solutions in the
C	second quadrant are the same as ones in the first except
C	for the sign of the slip direction.  So only the range
C	from 0 to 90 degrees need be calculated with two 
C	possibilities for slip direction considered for each
C	orientation.  (For the ratios the two solutions would
C	be identical.)  The procedure to calculate the actual
C	solution is described in FLTSOL and the comparison for
C	polarities in OKPOL.
C
C	Arthur Snoke  Virginia Tech  July 1984
C
C	2 September 1986:  made sampling truly equal area on focal sphere
C	8 January 2000:  Added do ... 200 instead of a second do ... 300
C-
c	INCLUDE 'FOCMEC.INC'


C	FOCMEC.INC
C
C	INCLUDE file for FOCMEC
C	27 August 1991 for the sun now includes SV
C	  polarities and SV/SH ratios
C	2 August 1993: changed ratio evaluations near nodal planes,
C	     so left out RLOW* and RHI* and replaced CUT with CUTP
C	     and CUTS.
C	2 June 2000: Increased MAX to 2000 and made it so only one version
C		appears with the distribution (others are virtual links).
C	NOTE! For some compilers, the ! starting an on-line comment may cause
C		an error.  Simply delete the ends of line starting at the !
C		if that is the case.
C-
c  oct 2010     jh: increase max to 5000
c
	PARAMETER (MAX = 5000)
	PARAMETER (MAX2 = 2*MAX)
C
C	Input Data
C
	REAL*4 XYZ(9,MAX2)	!  R, Theta, and Phi station coordinates 
	REAL*4 PSTATN(MAX)	!  Stations with polarity data
	REAL*4 RSTATN(MAX)	!  Stations with amplitude-ratio data
C				     X = north, Y east, Z down
	INTEGER POLRTY(MAX)	!  1 for C, F, or <;  -1 for D, B, or >
	INTEGER NPOL		!  Number of Polarities read in
	INTEGER KEYPOL(MAX)	!  Keys polarity data relative to input
	REAL*4 LOGRAT(MAX)	!  Log10 of (SV/P), (SH/P), or (SV/SH)
C				   including free surface correction, etc.
	INTEGER NRAT		!  Number of ratios read in
	INTEGER KEYRAT(MAX)	!  Keys ratios data relative to input
	CHARACTER*1 SVSH(2,MAX)	! First element V for SV or H for SH,
C				  Second element polarity: F or B for SV
C				  L or R for SH (back to station) for
C	                          numerator in SV/P, SH/P, or SV/SH ratio
C
C	  Processing things
C
	INTEGER MAXSOL	!  Exit after this many accept. aols.
	REAL*4 ERR		!  Total allowed number of errors
	REAL*4 ERRP		!  Allowed number of P polarity errors
	REAL*4 ERRSV		!  Allowed number of SV polarity errors
	REAL*4 ERRSH		!  Allowed number of SH polarity errors
	REAL*4 THRESH		!  Polarity weighting threshold
C				  (1.0 if no weighting)
	REAL*4 BADPP(MAX)	!  Stations with a P polarity error
	REAL*4 BADSVP(MAX)	!  Stations with a SV polarity error
	REAL*4 BADSHP(MAX)	!  Stations with a SH polarity error
	REAL*4 WBADP(MAX)	!  Weishts of P polarity errors
	REAL*4 WBADSV(MAX)	!  Weights of SV polarity errors
	REAL*4 WBADSH(MAX)	!  Weights of SH polarity errors
	REAL*4 BADP		!  Total weight of P polarity errors
	REAL*4 BADSV		!  Total weight of SV polarity errors
	REAL*4 BADSH		!  Total weight of SH polarity errors
	INTEGER NERRR		!  Allowed number of ratio errors
	REAL*4 ERRRAT		!  Maximum allowed Log10 ratio error
	REAL*4 CALRAT(MAX)	!  Theoretical Log10 of amplitude ratio
	INTEGER WTRAT(MAX)	!  1 if that ratio used, 0 otherwise
	REAL*4 VPVS3		!  Cube of P/S velocity ratio 
	REAL*4 BTMIN,BTDEL,BTMAX	!  Search range in B axis trend
	REAL*4 BPMIN,BPDEL,BPMAX	!  Search range in B axis plunge
	REAL*4 AAMIN,AADEL,AAMAX	!  Search range in A axis angle
C					   measured from B trend
	REAL*4 CUTP		!  cutoff limit for  P radiation factor
C				   in amplitude ratios
	REAL*4 CUTS		!  cutoff limit for  S radiation factor
C				   in amplitude ratios
C
	COMMON /FOCMEC/ PSTATN,RSTATN,XYZ,RD,POLRTY,NPOL,LOGRAT,
     1	  NRAT,MAXSOL,ERRP,ERRSV,NERRR,ERRRAT,CALRAT,WTRAT,VPVS3,
     2	  BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,AAMAX,
     3	  BADPP,KEYRAT,KEYPOL,ERRSH,BADSHP,ERR,THRESH,WBADP,WBADSH,
     4    WBADSV,BADSVP,CUTP,CUTS,BADP,BADSV,BADSH
	COMMON /FMCHAR/ SVSH



	REAL*4 A(3), N(3), BMATRX(3,3)
	LOGICAL OKSOL
	NPLUNG = 1 + NINT((BPMAX-BPMIN)/BPDEL)
	if (nplung .eq. 1) bpmax = bpmin
	NAANG = 1 + NINT((AAMAX-AAMIN)/AADEL)
	if (naang .eq. 1) aamax = aamin
	NSOL = 0
cxx
        write(6,*)'np,nt,na', nplung,ntrend,naang
	DO 500 JP = 1,NPLUNG
	  IF (JP .LT. NPLUNG) THEN
	    PLUNGE = (BPMIN + (JP-1)*BPDEL)/RD
	  ELSE
	    PLUNGE = BPMAX/RD
	  ENDIF
	  IF (JP .EQ. NPLUNG .AND. BPMAX .GE. 90.0) THEN
	    BTDELN = 0.0
	    NTREND = 1
	  ELSE
	    BTDELN = BTDEL/COS(PLUNGE)
	    NTREND = NINT((BTMAX+BTDEL-BTMIN)/BTDELN)
	    BTDELN = (BTMAX+BTDEL-BTMIN)/NTREND
	  ENDIF
	  DO 400 JT = 1,NTREND
	    TREND = AMIN1(BTMIN + (JT-1)*BTDELN,BTMAX)/RD
	    IF (PLUNGE .EQ. 0.0 .AND. TREND .GE. 180.0/RD) GO TO 400
	    DO 300 JA = 1,NAANG
	      IF (JA .LT. NAANG) THEN
	        ANGLE = (AAMIN + (JA-1)*AADEL)/RD
	      ELSE
	        ANGLE = AAMAX/RD
	      ENDIF
	      CALL FLTSOL(A,N,BMATRX,PLUNGE,TREND,ANGLE,JA)
	      DO 200 JSLIP = 1,2
	        IF (JSLIP .EQ. 2) THEN
	          DO 100 JS=1,3
	            AN = A(JS)
	            A(JS) = N(JS)
	            N(JS) = -AN
100	          CONTINUE
	        ENDIF
	        IF (.NOT.OKSOL(A,N,ANGLE)) GO TO 200
	        NSOL = NSOL + 1
	        IF (NSOL .EQ. MAXSOL) THEN
	          WRITE(2,3) MAXSOL
	          WRITE(*,3) MAXSOL
3	  FORMAT(' Reached chosen maximum of',I5,' solutions')
	          GO TO 600
	        END IF
200	      continue
300	    continue
400	  continue
500	CONTINUE
600	IF (NSOL .GT. 0) THEN
	  WRITE(*,1) NSOL
	  WRITE(2,1) NSOL
1	FORMAT(/' There are',I4,' acceptable solutions')
	ELSE
	  WRITE(2,2)
	  WRITE(*,2)
2	FORMAT(/' There are no acceptable solutions')
	ENDIF
	RETURN
	END
C+
      SUBROUTINE TIMDAT(NOUT,PROGNM)
C
C      INPUT IS PROGRAM NAME
C         A LINE TO NOUT WITH DATE AND TIME AND PROGNM
C	unix version:  25 June 1991  jas/vtso
C-
      character*24 fdate
      CHARACTER*(*) PROGNM
      if (nout .le. 0) return
      write(nout,*) ' ',fdate(),' for program ',prognm(1:lenc(prognm))
      RETURN
      END
C+
C	SUBROUTINE TRPL2V(TRPL,XYZ)
C
C	Transforms to XYZ components of a unit vector from
C		the trend and plunge for the vector.
C	Trend is the azimuth (clockwise from north looking down)
C	Plunge is the downward dip measured from the horizontal.
C	All angles in radians
C	X is north, Y is east, Z is down
C-
	SUBROUTINE TRPL2V(TRPL,XYZ)
	DIMENSION XYZ(3),TRPL(2)
	XYZ(1) = COS(TRPL(1))*COS(TRPL(2))
	XYZ(2) = SIN(TRPL(1))*COS(TRPL(2))
	XYZ(3) = SIN(TRPL(2))
	do j=1,3
	  if (abs(xyz(j)) .lt. 0.0001) xyz(j) = 0.0
	  if (abs(abs(xyz(j))-1.0).lt.0.0001) xyz(j)=xyz(j)/abs(xyz(j))
	end do
	RETURN
	END
C+
	LOGICAL FUNCTION TRUTH(MSG)
C
C
C PURPOSE:
C		ROUTINE ACCEPTS A MESSAGE (QUESTION) REQUIRING A Y/N RESPONSE
C		AND THE RETURN "TRUTH" IS SET:
C			TRUTH = .TRUE.	  IF RESPONSE IS Y(y)
C			TRUTH = .FALSE.                 N(n)
C
C ROUTINES CALLED:
C			IYESNO
C				WHICH CALLS
C						NSTRNG
C						PRINTX
C
C
C USE:
C	I=TRUTH('ANSWER Y OR N')
C	IF (I) ......
C
C OR
C	IF (TRUTH('REPLY Y OR N')) ....
C
C
C AUTHOR:			ALAN LINDE ... AUGUST 1980
C
C  ENTRY
C			ILOGIC (Alan's original name)
C-
	CHARACTER*(*) MSG
	LOGICAL ILOGIC
	ENTRY ILOGIC(MSG)
	TRUTH=.FALSE.
	CALL IYESNO(MSG,IANS)
	IF (IANS.EQ.1) TRUTH=.TRUE.
	ILOGIC = TRUTH
	RETURN
	END
C+
	SUBROUTINE V2TRPL(XYZ,TRPL,PI)
C
C	Transforms from XYZ components of a unit vector to
C	  the trend and plunge for the vector.
C	Trend is the azimuth (clockwise from north looking down)
C	Plunge is the downward dip measured from the horizontal.
C	All angles in radians
C	X is north, Y is east, Z is down
C	If the component of Z is negative (up), the plunge,TRPL(2),
C	  is replaced by its negative and the trend, TRPL(1),
C	  Is changed by PI.
C	The trend is returned between 0 and 2*PI, the plunge
C	  between 0 and PI/2.
C	12 January 2000: If xyz(3) = -1.0, make the trend PI.  Made
C	  consistency in the roundoff -- all are now 0.0001
C-
	DIMENSION XYZ(3),TRPL(2)
	do j=1,3
	  if (abs(xyz(j)) .le. 0.0001) xyz(j) = 0.0
	  IF (ABS(ABS(XYZ(j))-1.0).LT.0.0001) xyz(j)=xyz(j)/abs(xyz(j))
	end do
	IF (ABS(XYZ(3)) .eq. 1.0) THEN 
C
C	plunge is 90 degrees
C
	  if (xyz(3) .lt. 0.0) then
	    trpl(1) = PI
	  else
	    TRPL(1) = 0.0
	  end if
	  TRPL(2) = 0.5*PI
	  RETURN
	END IF
	IF (ABS(XYZ(1)) .LT. 0.0001) THEN
	  IF (XYZ(2) .GT. 0.0) THEN
	    TRPL(1) = PI/2.
	  ELSE IF (XYZ(2) .LT. 0.0) THEN
	    TRPL(1) = 3.0*PI/2.0
	  ELSE
	    TRPL(1) = 0.0
	  END IF
	ELSE
	  TRPL(1) = ATAN2(XYZ(2),XYZ(1))
	END IF
	C = COS(TRPL(1))
	S = SIN(TRPL(1))
	IF (ABS(C) .GE. 0.1) TRPL(2) = ATAN2(XYZ(3),XYZ(1)/C)
	IF (ABS(C) .LT. 0.1) TRPL(2) = ATAN2(XYZ(3),XYZ(2)/S)
	IF (TRPL(2) .LT. 0.0) THEN
	  TRPL(2) = -TRPL(2)
	  TRPL(1) = TRPL(1) - PI
	  END IF
	IF (TRPL(1) .LT. 0.0) TRPL(1) = TRPL(1) + 2.0*PI
	RETURN
	END
C+
	FUNCTION VALUE(MSG)
C
C PURPOSE:
C		THIS FUNCTION ACCEPTS A MESSAGE (ASKING FOR A VALUE)
C		AND RETURNS THE VALUE ENTERED AT THE TERMINAL
C ROUTINES CALLED:
C		PRINTX, CSTRING
C
C USE:
C		ANS=VALUE('ENTER A REAL VALUE')
C	AND
C		IANS=VALUE('ENTER AN INTEGER')
C
C AUTHOR:
C			ALAN LINDE ... AUGUST 1980
C
C EXTENSIONS:
C
C	  One can now enter exponential format in VALUE or RVALUE
C	30 JULY 1989:  CAN HANDLE ENTRY FOLLOWED BY A BLANK OR TAB
C	27 July 1993: Did input read through cstring so can have
C	  comment lines
C
C	2001.05.24	Khalil Hayek, GSC
C	Changed READ(,F20,) to READ(,*,) to be able to handle
C	integers.
C	Not needed on my compilers, but ok.  Commented out E20 option  (jas)
C	19 July 2002: PCs had a problem with single-digit integer, so ...
C-
	CHARACTER*1 E/'E'/,BLANK/' '/
	CHARACTER*30 STUFF
	CHARACTER*(*) MSG
C
100	CALL PRINTX(MSG)
	call cstring(stuff,nin)
	IF (NIN .GT. 0) THEN
	  NBLANK = INDEX(STUFF(1:NIN),BLANK)
	  IF (NBLANK .GT. 0) NIN = NBLANK - 1
	END IF
	IF (NIN .EQ. 0) THEN
	  VALUE = 0.0
	ELSE
	  if (nin .eq. 1) then
	    stuff(2:2) = stuff(1:1)
	    stuff(1:1) = '0'
	    nin = 2
	  end if
	  READ(STUFF(1:NIN),*,ERR=100) VALUE
	END IF
	RETURN
	END
