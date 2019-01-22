
c     Program to read macro seismic information into a nordic file
c
c     C. Lindholm, April 1993
c     jul 9 1993 by jh: version 3.0*******
c     jul 14          : to pc
c     aug cl some changes ??
c     aug 29 93 by jh: use subroutine get_agency instead of getenv
c     jan 94       ********** version 5.0 ************************
c     feb 21 99    -----------verison 7.0 check------------------
c     feb 23 2012 jh  wrong number of letters for MM
c     nov 20 2012 jh  add EM for EMS
c
c
      implicit none
      include 'seidim.inc'
      character*80 data(max_data),macrocard
      character*80 input,sfile
      character*3 agency
      character*2 intscale
      character*1 diastrophism,tsunami,cult,unusual
      character*1 ucase,qualifier,quality
      character*1 type,exp
      real maclat,maclon,macmag,radfelt,intarea1,intarea2
      integer int1,int2,maxint
      integer i,nbend,macronr
      integer nhead,nrecord,nphas,nstat
      integer newcard



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c------ Initialize
c
      quality = ' '
      qualifier = ' '
c     macrocard = ' '
      do i=1,80
        macrocard(i:i)=' '
      enddo
      diastrophism = ' '
      tsunami = ' '
      cult = ' '
      unusual = ' '
      maxint = 0
      intscale = 'MM'
      maclat = 0.0
      maclon = 0.0
      radfelt = 0.0
      int1 = 0
      intarea1 = 0.
      int2 = 0
      intarea2 = 0.
      call get_agency(agency)
c      agency = 'BER'


c      write(6,'(a,$)')' This will prevent carriage return'

      write(*,*)
     +'You will now enter macroseismic information into ',
     +'the S-file in the database'
      write(*,*)' '
      write(*,*)
     +'     A carriage return ==> Proceed to next question'
      write(*,*)
     +'     A /  ==> Input is finished, and written to S-file'
      write(*,*)' '

c      write(*,*)
c     +'Remember that any text that you want ',
c     +'to appear in the bulletin can be written directly ',
c     +'in the S-file!'
c      write(*,*)' ==========Syntax:'
c      write(*,*)
c     +'The first column blank and the next 4 ',
c     +'columns = Bul:, and a linetype 3 indicator'
c
c---- Quality rank of the report
c
      input = ' '
      write(6,'(a,$)')' Quality of this report (A, B, C or D)?:'
      read(*,'(a)')input
      i = NBEND(input)
      if(input(i:i) .eq. '/') go to 99
      quality = ucase(input(i:i)) 
c
c---- Diastrophism code;Only F(aulting) or blank
c
      input = ' '
      write(6,'(a,$)')' Surface Faulting? (y/n=default):'
      read(*,'(a)')input
      i = NBEND(input)
      if(ucase(input(i:i)) .eq. 'Y')diastrophism = 'F'
      if(input(i:i) .eq. '/') go to 99
c
c--- Tsunami or Seiches
c
      input = ' '
      write(6,'(a,$)')' Tsunami(T) or Seiches(S)? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      if(ucase(input(i:i)) .eq. 'T')tsunami = 'T'
      if(ucase(input(i:i)) .eq. 'S')tsunami = 'S'
c
c---- Cultural effects
c
      input = ' '
      write(6,'(a,$)')' C(asualties) or D(amage)? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      if(ucase(input(i:i)) .eq. 'C')cult = 'C'
      if(ucase(input(i:i)) .eq. 'D')cult = 'D'
c
c---- Unusual events
c
      write(*,*)' Possible unusual effects are:'
      write(*,*)' L == Liquefaction'
      write(*,*)' G == Geysir/Fumerol'
      write(*,*)' S == Landslides/Avalanches'
      write(*,*)' B == Sand blows'
      write(*,*)' V == Visual phenomena'
      write(*,*)' M == More than one of the above'

      input = ' '
      write(6,'(a,$)')' Enter code for unusual effect? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      if(ucase(input(i:i)) .eq. 'L')unusual = 'L'
      if(ucase(input(i:i)) .eq. 'G')unusual = 'G'
      if(ucase(input(i:i)) .eq. 'S')unusual = 'S'
      if(ucase(input(i:i)) .eq. 'B')unusual = 'B'
      if(ucase(input(i:i)) .eq. 'V')unusual = 'V'
      if(ucase(input(i:i)) .eq. 'M')unusual = 'M'
c
c---- Maximum intensity
c
      input = ' '
      write(6,'(a,$)')' Maximum intensity (integer)? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      open(77,status='scratch')
      write(77,'(a)') input
      rewind 77      
c      if(i .ge. 1)read(77,*)maxint
      if(input(1:3).ne.'   ') read(77,*) maxint
      rewind 77

      input = ' '
      write(6,'(a,$)')' Intensity qualifier (+/-)? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      read(input(1:i),'(a)')qualifier
c
c---- Intensity scale
c
 30   input = ' '
      write(*,*)' The following intensity scales are accepted:'
      write(*,*)' MM == Modified Mercalli (default)'
      write(*,*)' RF == Rossi Forel'
      write(*,*)' CS == Mercalli Cancani Seberg'
      write(*,*)' SK == Medevev Sponheur Karnik'
      write(*,*)' EM == European Macroseismic Scale'

      write(6,'(a,$)')' Intensity scale? (default=MM):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      if(i .gt. 2)then 
        write(*,*)' Only two characters are accepted'
        go to 30
      endif
c      write(6,*) 'i',i
      intscale(1:1) = ucase(input(1:1))
      intscale(2:2) = ucase(input(2:2))
      if(i .eq. 1)intscale = 'MM'
c
c---- Macroseismic epicenter and magnitude
c
      input = ' '
      write(6,'(a,$)')' Macroseismic latitude? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      read(input(1:i),'(f10.0)')maclat

      input = ' '
      write(6,'(a,$)')' Macroseismic longitude? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      read(input(1:i),'(f10.0)')maclon

      input = ' '
      write(6,'(a,$)')' Macroseismic magnitude? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      read(input(1:i),'(f10.0)')macmag
c
c---- Radius of felt area
c     
      input = ' '
      write(6,'(a,$)')' Radius of felt area (km)? (default=none):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      read(input(1:i),'(f10.0)')radfelt
c
c---- Intensity and area pairs
c
      input = ' '
      write(*,'(2a,$)')
     +' Area 1 where the earthquake was felt at ',
     +'a minimum intensity:'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      read(input(1:i),'(f10.0)')intarea1
      input = ' '
      write(*,'(a,$)')' Intensity bordering that area (integer):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      write(77,'(a)')input
      rewind 77
      if(input(1:3).ne.'   ') read(77,*) int1
      rewind 77
c      read(input(1:i),'(i)')int1

      input = ' '
      write(*,'(2a,$)')
     +' Area 2 where the earthquake was felt at ',
     +'a minimum intensity:'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      read(input(1:i),'(f10.0)')intarea2
      input = ' '
      write(*,'(a,$)')' Intensity bordering that area (integer):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      write(77,'(a)') input
      rewind 77
      if(input(1:3).ne.'   ') read(77,*) int2
c      read(input(1:i),'(i)')int2
c
c---- Agency for macroseismic report
c
      input = ' '
      write(*,'(4a,$)')' Agency for macroseismic report ',
     +'(default = ',agency,'):'
      read(*,'(a)')input
      i = NBEND(input) 
      if(input(i:i) .eq. '/') go to 99
      if(i .gt. 1)read(input(1:i),'(a)')agency

 99   continue
c
c-------- Now write the input on the S-file
c
      call get_env_event(sfile)
      open(18,file=sfile,status='old')
      
      call indata(18,nstat,nphas,NHEAD,NRECORD,TYPE,EXP,DATA,i)
c
c---- First find preexisting Type 2 line
c
      macronr = 0
      do i = 1,nhead
       if(data(i)(80:80) .eq. '2')then
         write(*,*)' Macroseismic information already exists....'
         write(*,'(a)')data(i)
         write(*,*)' 1 ==> Overwrite the existing information'
         write(*,*)' 2 ==> Create a new additional card'
         write(*,'(a,$)')' Enter your choice:'
         read(*,*)newcard
         if(newcard .eq. 1)then
           macrocard = data(i)
           macronr = i
         endif
       endif
      enddo

      write(macrocard(22:22),'(a)')diastrophism
      if(tsunami .eq. 'T')write(macrocard(23:23),'(a)')tsunami
      if(tsunami .eq. 'S')write(macrocard(24:24),'(a)')tsunami
      write(macrocard(25:25),'(a)')cult
      write(macrocard(26:26),'(a)')unusual
      write(macrocard(28:30),'(i2,a1)')maxint,qualifier
      write(macrocard(31:32),'(a)')intscale
      if(maclat .ne. 0.)then
         write(macrocard(34:39),'(f6.2)')maclat
         write(macrocard(41:47),'(f7.2)')maclon
      endif
      if(macmag .ne. 0.)write(macrocard(49:51),'(f3.1)')macmag
      if(radfelt .gt. 1.)then
        radfelt = log10(radfelt)
        write(macrocard(53:56),'(f4.2)')radfelt
      endif
      if(int1 .gt. 0)then
        intarea1 = log10(intarea1)
        write(macrocard(57:63),'(f5.2,i2)')intarea1,int1
      endif

      if(int2 .gt. 0)then
        intarea2 = log10(intarea2)
        write(macrocard(64:70),'(f5.2,i2)')intarea2,int2
      endif

      write(macrocard(72:75),'(a1,a3)')quality,agency
      write(macrocard(80:80),'(a)')'2'

      rewind (18)
      write(18,'(a)')data(1)
c---- New macroseisic card
      if(newcard .eq. 2 .or. macronr .eq. 0)then
        do i = 2,nhead-1
          write(18,'(a)')data(i)
        enddo
        write(18,'(a)')macrocard
        write(18,'(a)')data(nhead)
        do i = nhead+1,nrecord
          write(18,'(a)')data(i)
        enddo
      endif
c---- Overwrite old macroseismic card
      if(newcard .eq. 1)then        
        write(18,'(a)')macrocard
        do i = 2,nrecord
          if(i .ne. macronr)write(18,'(a)')data(i)
        enddo
      endif
c
c---  Finished puh!!!
c      
c      write(*,*)macrocard

c      write(*,*)' Diastrophism:',diastrophism
c      write(*,*)' Tsunami:',tsunami
c      write(*,*)' Cultural:',cult
c      write(*,*)' Unusual:',unusual
c      write(*,*)' Maxint:',maxint
c      write(*,*)' Intscale:',intscale
c      write(*,*) maclat,maclon,macmag
c      write(*,*)radfelt
c      write(*,*)int1,intarea1,int2,intarea2
c      write(*,*)agency


      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C NBEND    UT-F-U   Created: 8/11/89 Copied : 08/28/89 Made at NORSAR
      INTEGER FUNCTION NBEND(STRING)
      CHARACTER*(*) STRING
C.======================================================================
C.    PURPOSE
C     find_Non_Blank_END_of_string                                  UT<<
C.----------------------------------------------------------------------
C.    KEYWORDS
C.----------------------------------------------------------------------
C.    INPUT
C..   STRING - Character string
C
C.    OUTPUT
C..   NBEND  - Position of last non blank character in string
C.----------------------------------------------------------------------
C.    PROGRAMMER    Stein Holger Pettersen
C.    CREATION_DATE 19 Jun 1989
C.    MADE_AT  NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
 
      LS = LEN(STRING)
      DO 10 I = LS,1,-1
         IF (STRING(I:I).NE.' ') GO TO 20
   10 CONTINUE
      I = 1
 
   20 NBEND = I
      RETURN
      END
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      character*1 function ucase(a)
c     convert lower case single char to upper case - machine independent
      
      character*1 chr(26),ucchr(26),a
      data chr/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     &'n','o','p','q','r','s','t','u','v','w','x','y','z'/
      data ucchr/'A','B','C','D','E','F','G','H','I','J','K','L','M',
     &'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      do 10 i=1,26
        if(a.eq.chr(i))then
          ucase=ucchr(i)
          return
        endif
10    continue
      ucase=a
      return
      end
