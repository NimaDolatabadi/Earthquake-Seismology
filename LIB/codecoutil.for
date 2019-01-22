c***************************************************************************
      subroutine CASEFOLD(cn) ! Urs Kradolfer, 20. Feb. 1990
c
c Input : character-string of any length
c Output: same character-string, but all letters are CAPITAL now
c         (other characters are not changed)
c
c   --> this subroutine needs the function TRIMLEN !
c
c Call  : call CASEFOLD(charstring)
c
      implicit none
      character cn*(*)
      integer i, trimlen,ilen, ival
      ilen=trimlen(cn)
      do i=1,ilen
         ival=ICHAR(cn(i:i))
         if(ival.ge.97.and.ival.le.122)then
            cn(i:i)=CHAR(ival-32)
         endif
      enddo
      RETURN
      end ! of subroutine casefold
c
c***************************************************************************
      integer function TRIMLEN(t)   ! Urs Kradolfer, June 1986
c
c     Call:    nc=TRIMLEN(char)
c
c          --> nc says, how many characters the input-string has
c              (ignoring trailing blanks!).
c
      implicit none
      character t*(*)
      do 1 trimlen=LEN(t),1,-1
    1    if(t(trimlen:trimlen).ne.' ')RETURN
      trimlen=1
      end ! of integer function trimlen
c
c***************************************************************************

      integer function TRIMBEG(t)   ! Urs Kradolfer, 17.1.90
c
c     Call:    nc=TRIMBEG(char)
c
c          --> nc returns the position of the first characters of
c              the input-string (ignoring leading blanks!).
c
      implicit none
      character t*(*)
      do 1 trimbeg=1,LEN(t)
    1    if(t(trimbeg:trimbeg).ne.' ')RETURN
      trimbeg=1

      end ! of integer function trimbeg


C=======================================================================

      SUBROUTINE JULIEN(IJ,IM,IY,IJD,ISI)

C=======================================================================
C     CONVERTS IJ,IM,IY -> IY,IJD      FOR ISI >= 1
C     CONVERTS IY,IJD   -> IJ,IM,IY    FOR ISI <= 0
C     EX:  IJ,IM,IY = 21 02 80    IY,IJD = 1980 52

      INTEGER IMO(12)
      DATA IMO/31,28,31,30,31,30,31,31,30,31,30,31/

      IF(MOD(IY,4).EQ.0) IMO(2)=29
      IF(IY.LT.1900) IY=IY+1900
      if  (iy .lt. 1970)  iy = iy + 100   ! K.S. 20-Aug-98

      IF(ISI.GE.1) THEN
        IJD=0
        DO 10 I=1,IM-1
   10   IJD=IJD+IMO(I)
        IJD=IJD+IJ
      ELSE
        IDL=IJD
        DO 30 I=1,12
        IF(IDL.LE.IMO(I)) GOTO 40
   30   IDL=IDL-IMO(I)
   40   IJ=IDL
        IM=I
      ENDIF

      RETURN
      END

c***************************************************************************
c
      subroutine DIF1(iy,nmax)
c
c     Do Data-compression (first differences)
c
c     Urs Kradolfer, January 1990
c
      implicit none
      integer nmax, iy(nmax), k, xim1, xtemp
c
      xim1=iy(1)
      do k=2,nmax
         xtemp=iy(k)
         iy(k)=xtemp-xim1
         xim1=xtemp
      enddo
c
      RETURN
      end ! of subroutine dif1
c
c***************************************************************************
c
      subroutine REMDIF1(iy,nmax)
c
c     Remove Data-compression (first differences)
c
c     Urs Kradolfer, January 1990
c
      implicit none
      integer nmax, iy(nmax), k
c
      do k=2,nmax
         iy(k)=iy(k)+iy(k-1)
      enddo
c
      RETURN
      end ! of subroutine remdif1
c
c***************************************************************************
      integer*4 function icheck_sum_gse1(ix,n)
c     Karl Koch, May 1995
      integer*4 ix(n),n
      integer*4 i,nchk

      nchk=0
      do i=1,n
         nchk=nchk+ix(i)
      enddo
      icheck_sum_gse1=nchk
      return
      end

c
c***************************************************************************
      integer*4 function icheck_sum_gse2(ix,n)
c     Karl Koch, May 1995
      integer*4 ix(n),n
      integer*4 i,nchk,ixx
      integer*4 MODULO_VALUE
      parameter (MODULO_VALUE = 100 000 000)
      integer*4 modulo
c
      modulo=MODULO_VALUE
      nchk=0
      do i=1,n
        ixx=ix(i)
        if (abs(ixx).ge.modulo) ixx=ixx-(ixx/modulo)*modulo
           nchk=nchk+ixx
        if (abs(nchk).ge.modulo) nchk=nchk-(nchk/modulo)*modulo
      enddo
      icheck_sum_gse2=abs(nchk)
      return
      end



c ==============================================================================



      subroutine get_flags( ifmt, flags )

c     Parses format string 'ifmt' and extracts flags appended after '-'.
c     'ifmt' is truncated at '-'.
c     K. Stammler, 20-Aug-98

c     -- parameters
      character*(*)   ifmt     ! user input (modified)
      character*(*)   flags    ! flag string found (output)

c     -- local variables
      integer         strlth   ! length of input string
      integer         i, j     ! counter
      logical         f_found  ! flags found

c     -- functions
      integer         trimlen

c     -- executable code

      flags = ' '
      strlth = trimlen( ifmt )
      f_found = .false.
      j = 1
      do  i=1,strlth
         if  (f_found)  then
            flags(j:j) = ifmt(i:i)
            ifmt(i:i) = ' '
         endif
         if  (ifmt(i:i) .eq. '-')  then
            f_found = .true.
            ifmt(i:i) = ' '
         endif
      enddo

      end ! of subroutine get_flags



c ==============================================================================


