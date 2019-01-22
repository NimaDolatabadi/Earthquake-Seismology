c
c
c
c  changes
c
c  feb 03 2003  by jh : put in computer indpendent intand instead of unix
c                       only version
c
c
c
c
C******************************************************************************
c#    The following data compression and decompression routines have
c#    been developped by Dr. Shane Ingate and Dr. Ken Muirhead, at the
c#    Australian Seismological Centre, Bureau of Mineral Resources,
c#    Canberra, Australia.
c#    They provided me with these routines in March 1989 during the
c#    session of the Group of Scientific Experts (GSE) in Geneva.
c#    These compression/decompression algorithms are used by all members
c#    of the GSE during the large-scale data exchange experiment GSETT-2,
c#    carried out from 1989 to 1991.
c#    It is recommended to use second differences and the six-bit compression.
c#    The second differences can be computed by two subsequent calls of
c#    subroutine DIF1 (see above).
c#    These routines are already running on many different machines.
c#    Because the AND function is not standard FORTRAN 77, this operation
c#    has been moved to a separate subroutine INTAND. All users of these
c#    routines will have to modify INTAND so that it performs correctly on
c#    their computers.
c#                                 Urs Kradolfer, Swiss Seismological Service
c#
C******************************************************************************
C                                                                       *
C     SUBROUTINE CMPRS6(LX,IX,LC,CBUFF,IERROR)                          *
C                                                                       *
C     ROUTINE TO COMPRESS INTEGER DATA INTO PRINTABLE ASCII CHARACTERS. *
C     INPUTS ARE INTEGER*4 ARRAY IX OF LENGTH LX.                       *
C     LC SHOULD CONTAIN THE DIMENSION OF THE ARRAY CBUFF.               *
C     OUTPUTS ARE CHARACTER*1 ARRAY CBUFF CONTAINING LC CHARACTERS.     * 
C     IF THE ARRAY CBUFF IS NOT LARGE ENOUGH TO CONTAIN ALL OF THE DATA *
C     IERROR IS SET TO -1, OTHERWISE IT IS SET TO ZERO.                 *
C     OUTPUT IS PADDED TO A MULTIPLE OF 80 CHARACTERS USING SPACES.     *
C                                                                       *
C     METHOD OF COMPRESSION IS TO USE THE 6 LEAST SIGNIFICANT SIX BITS  *
C     OF AN EIGHT BIT BYTE SO THAT ALL DATA CAN BE TRANSMITTED AS ASCII *
C     CHARACTERS.                                                       *
C     OF THESE SIX BITS, THE MOST SIGNIFICANT IS USED AS A CONTINUATION *
C     BIT. IF IT IS SET TO ONE THE FOLLOWING BYTE ALSO FORMS PART OF    *
C     THE PRESENT SAMPLE. IF ZERO, THIS IS THE LAST BYTE IN THE SAMPLE. *
C     THE SECOND LEAST SIGNIFICANT BIT IS USED AS A SIGN BIT IN THE     *
C     FIRST BYTE AND FOR DATA IN SUBSEQUENT BYTES (ORIGINAL DATA IS     *
C     FIRST CONVERTED TO SIGN AND MAGNITUDE). ALL OTHER BITS OF FIRST   *
C     AND SUBSEQUENT BYTES FORM THE MAGNITUDE OF THE NUMBER.            *
C                                                                       *
C     TO ENABLE TRANSMISSION OVER AS MANY LINKS AS POSSIBLE DATA IS     *
C     FURTHER TRANSFORMED TO THE CHARACTERS +,-,0 - 9,A - Z, a - z      *
C     USING A LOOKUP TABLE.                                             *
C                                                                       *
C     SUBROUTINES CALLED INTAND (THIS SUBROUTINE IS MACHINE SPECIFIC)   *
C                                                                       *
C     DATA MAY BE DECOMPRESSED USING SUBROUTINE DCOMP6.                 *
C                                                                       *
C************************************************************************ 
C
      SUBROUTINE CMPRS6(LX,IX,LC,CBUFF,IERROR)
      IMPLICIT INTEGER*4 (I-N)
cuk      CHARACTER *1 CBUFF(4),TEMP(4),TEST(4),ACHAR(64),BLANK
cuu91      CHARACTER *1 CBUFF(1),TEMP(4),TEST(4),ACHAR(64),BLANK
      CHARACTER *1 CBUFF(lc),TEMP(4),TEST(4),ACHAR(64),BLANK
cuu91      INTEGER *4 IX(1)
      INTEGER *4 IX(lx)
      EQUIVALENCE (J,TEMP(1)),(TEST,ITEST)
      DATA TEST/'1','2','3','4'/
      DATA ACHAR / '+','-','0','1','2','3','4','5','6','7','8','9','A',
     1 'B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q',
     2 'R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g',
     3 'h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w',
     4 'x','y','z'/
      DATA BLANK /' '/
      DATA N4/16/, N5/32/, N5M1/31/, N6/64/, N9/512/, N10/1024/, 
     1 N10M1/1023/, N14/16384/, N15/32768/, N15M1/32767/, N19/524288/, 
     2 N20/1048576/, N20M1/1048575/, N24/16777216/, N25/33554432/, 
     3 N25M1/33554431/, N28/134217728/, N28M1/134217727/
      IERROR = 0
      IMAX = LC
      MFLAG = N5
      IOFF = 0
      DO 60 I = 1,LX
        JN = IX(I)
C       SET NFLAG TO 1 TO POINT TO FIRST ELEMENT IN LOOKUP TABLE
        NFLAG = 1
C
C       SEE IF NUMBER IS -VE IF SO CONVERT TO SIGN AND MAGNITUDE.
C
        IF(JN .LT. 0) THEN
           NFLAG = NFLAG + N4
           JN = -JN
        END IF
        IF(JN .LT. N4) GO TO 50
C        IF HERE, DATA REQUIRES MORE THAN 1 BYTE
         IF(JN .LT. N9) GO TO 40
C         IF HERE, DATA REQUIRES MORE THAN 2 BYTES
          IF(JN .LT. N14) GO TO 30
C          IF HERE, DATA REQUIRES MORE THAN 3 BYTES.
           IF(JN. LT. N19) GO TO 20
C           IF HERE, DATA REQUIRES MORE THAN 4 BYTES.
            IF(JN. LT. N24) GO TO 10
C            IF HERE, DATA REQUIRES MORE THAN 5 BYTES.
             IF(JN .GT. N28M1) JN=N28M1
C            FILL A BYTE IF HERE NUMBER WILL REQUIRE SIX BYTES
             J = JN/N25 + NFLAG + MFLAG
             IOFF = IOFF + 1
             CBUFF(IOFF) = ACHAR(J)
             CALL INTAND(JN,N25M1,JN)
             NFLAG = 1
   10       CONTINUE
C           FIVE CHARACTERS TO GO
            J = JN/N20 + NFLAG + MFLAG
            IOFF = IOFF + 1
            IF (IOFF .GT. IMAX) GO TO 80
            CBUFF(IOFF) = ACHAR(J)
            CALL INTAND(JN,N20M1,JN)
            NFLAG = 1
   20      CONTINUE
C          FOUR CHARACTERS TO GO
           J = JN/N15 + NFLAG + MFLAG
           IOFF = IOFF + 1
           IF (IOFF .GT. IMAX) GO TO 80
           CBUFF(IOFF) = ACHAR(J)
           CALL INTAND(JN,N15M1,JN)
           NFLAG = 1
   30     CONTINUE
C         THREE CHARACTERS TO GO
          J = JN/N10 + NFLAG + MFLAG
          IOFF = IOFF + 1
          IF (IOFF .GT. IMAX) GO TO 80
          CBUFF(IOFF) = ACHAR(J)
          CALL INTAND(JN,N10M1,JN)
          NFLAG = 1
   40    CONTINUE
C        TWO CHARACTERS TO GO
         J = JN/N5 + NFLAG + MFLAG
         IOFF = IOFF + 1
         IF (IOFF .GT. IMAX) GO TO 80
         CBUFF(IOFF) = ACHAR(J)
         CALL INTAND(JN,N5M1,JN)
         NFLAG = 1
   50   CONTINUE
C       ONE CHARACTER TO GO
        J = JN + NFLAG
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 80
        CBUFF(IOFF) = ACHAR(J)
   60 CONTINUE
C     NOW MAKE OUTPUT BUFFER UP TO A MULTIPLE OF 80 CHARACTERS
C     WITH BLANKS
      K = 80 - (IOFF - ((IOFF/80) * 80))
      IF (K .LT. 2) K = K + 80
C     PUT IN AT LEAST TWO ADJACENT SPACE CHARACTERS TO ASSIST IN
C     DECODING
      DO 70 I = 1, K
         IOFF = IOFF + 1
         IF (IOFF .GT. IMAX) GO TO 80
         CBUFF(IOFF) = BLANK
   70 CONTINUE
   80 CONTINUE
      LC = IOFF
      IF (IOFF .GT. IMAX) THEN
         LC = IMAX
         IERROR = -1
      ENDIF
      RETURN
      END
C******************************************************************
C                                                                 *
C     SUBROUTINE CMPRS7(LX,IX,LC,CBUFF,IERROR)                    *
C                                                                 *
C     ROUTINE TO WORD-COMPRESS DATA INTO 7 BIT BYTES.             *
C     INPUTS ARE INTEGER*4 ARRAY IX CONTAINING LX VALUES TO BE    *
C     COMPRESSED INTO 7 BIT BYTES. THE MOST SIGNIFICANT BIT OF    *
C     EACH BYTE IS NOT USED.                                      *
C     ON ENTRY LC SHOULD CONTAIN DIMENSIONED LENGTH OF CBUFF.     *
C     OUTPUTS ARE CHARACTER*1 ARRAY CBUFF WITH LC MODIFIED TO     * 
C     REFLECT THE NUMBER OF BYTES WHICH HAVE BEEN PUT INTO CBUFF. *
C     LC IS MADE A MULTIPLE OF 80 USING UNIQUE FILL CHARACTERS.   *
C     IF CBUFF IS TOO SMALL TO CONTAIN ALL COMPRESSED DATA        *
C     IERROR WILL CONTAIN -1 OTHERWISE IT WILL BE SET TO ZERO.    *
C     METHODOLOGY IS THAT THE MOST SIGNIFICANT BIT OF EACH BYTE   *
C     IS SET TO ONE IF NEXT BYTE CONTAINS INFORMATION PERTAINING  *
C     TO THE SAME SAMPLE. IN THE FIRST BYTE PERTAINING TO EACH    *
C     SAMPLE, THE SECOND MOST SIGNIFICANT BIT IS USED AS A SIGN   *
C     BIT. THE LEAST SIGNIFICANT 5 BITS OF THE FIRST BYTE AND     *
C     THE LEAST SIGNIFICANT SIX BITS OF ANY SUBSEQUENT BYTE       *
C     PERTAINING TO THE SAME SAMPLE ARE COMPRESSED TOGETHER ON    *
C     DECODING TO GIVE THE MAGNITUDE OF THE SAMPLE.               * 
C     USING THIS COMPRESSION SCHEME THE CODES                     *
C     1000000  1000000  AND 1000000  0000001 WOULD NOT NORMALLY BE*
C     TRANSMITTED (AKIN TO NOT TRANSMITTING LEADING ZEROS).       *
C     THESE TWO PATTERNS ARE USED AS CONTROL PATTERNS.            *
C     THE PATTERN 1000000 1000000 SIGNIFIES THE END OF THE DATA   *
C     AND IS USED TO FILL UP TO A MULTIPLE OF EIGHTY CHARACTERS.  *
C     THE PATTERN 1000000  0000001 INDICATES THAT 32 SHOULD BE    *
C     SUBTRACTED FROM THE PREVIOUS SAMPLE. USING THIS METHODOLOGY,*
C     ANY CONTROL CHARACTER WITH INTEGER VALUE LESS THAN 32       *
C     WHICH A COMMUNICATION LINK WONT ACCEPT CAN BE MODIFIED BY   *
C     ADDING 32 TO IT. THE 32 WILL BE SUBTRACTED ON DECOMPRESSION.*
C     DATA MAY BE DECOMPRESSED USING SUBROUTINE DCOMP7.           *
C                                                                 *
C     SUBROUTINES CALLED   ---   INTAND   ---   CHEKBT   ---      * 
C                                                                 *
C******************************************************************
C
      SUBROUTINE CMPRS7(LX,IX,LC,CBUFF,IERROR)
      IMPLICIT INTEGER*4 (I-N)
cuk      CHARACTER*1 CBUFF(4),TEMP(4),TEST(4),NCNTRL
cuu91      CHARACTER*1 CBUFF(1),TEMP(4),TEST(4),NCNTRL
      CHARACTER*1 CBUFF(lc),TEMP(4),TEST(4),NCNTRL
cuu91      INTEGER *4 IX(*)
      INTEGER *4 IX(lx)
      EQUIVALENCE (J,TEMP),(TEST,ITEST)
      DATA TEST/'1','2','3','4'/
      DATA N5/32/,
     *     N6/64/,        N6M1/63/,
     *     N11/2048/,
     *     N12/4096/,     N12M1/4095/,
     *     N17/131072/,
     *     N18/262144/,   N18M1/262143/,
     *     N23/8388608/,
     *     N24/16777216/, N24M1/16777215/,
     *     N27/134217728/,N27M1/134217727/
C
C     WORK OUT WHICH WAY BYTES ARE STORED IN A COMPUTER WORD
      IBYTE = 4
      IF(ITEST.EQ.(((52*256+51)*256+50)*256+49)) IBYTE = 1
C
      NCNTRL = CHAR(64)
C     NCNTRL IS A 7 BIT BYTE WITH BIT PATTERN 1000000
      MFLAG = N6
      IMAX = LC
      IERROR = 0
      IOFF = 0
      DO 100 II = 1,LX
        IONLY = 0
        JN = IX(II)
C
C       GET SIGN AND MAGNITUDE OF SAMPLE
C
        NFLAG = 0
        IF(JN .LT. 0) THEN
           NFLAG = N5
           JN = -JN
        END IF
        IF(JN .LT. N5) GO TO 40
         IONLY = 1
C        IF HERE, DATA REQUIRES MORE THAN 1 BYTE
         IF(JN .LT. N11) GO TO 30
C         IF HERE, DATA REQUIRES MORE THAN 2 BYTES
          IF(JN. LT. N17) GO TO 20
C          IF HERE, DATA REQUIRES MORE THAN 3 BYTES.
           IF(JN. LT. N23) GO TO 15
C           IF HERE, DATA REQUIRES MORE THAN 4 BYTES.
            IF(JN. GT. N27M1) JN=N27M1
C           FILL A BYTE
            J = JN/N24 + NFLAG + MFLAG
            IOFF = IOFF + 1
            IF (IOFF .GT. IMAX) GO TO 120
            CBUFF(IOFF) = TEMP(IBYTE)
            CALL INTAND(JN,N24M1,JN)
            NFLAG = 0
   15      CONTINUE
C          FOUR MORE BYTES TO GO
           J = JN/N18 + NFLAG + MFLAG
           IOFF = IOFF + 1
           IF (IOFF .GT. IMAX) GO TO 120
           CBUFF(IOFF) = TEMP(IBYTE)
           CALL INTAND(JN,N18M1,JN)
           NFLAG = 0
   20     CONTINUE
C         THREE MORE BYTES TO GO
          J = JN/N12 + NFLAG + MFLAG
          IOFF = IOFF + 1
          IF (IOFF .GT. IMAX) GO TO 120
          CBUFF(IOFF) = TEMP(IBYTE)
          CALL INTAND(JN,N12M1,JN)
          NFLAG = 0
   30    CONTINUE
C        TWO MORE BYTES TO GO
         J = JN/N6 + NFLAG + MFLAG
         IOFF = IOFF + 1
         IF (IOFF .GT. IMAX) GO TO 120
         CBUFF(IOFF) = TEMP(IBYTE)
         CALL INTAND(JN,N6M1,JN)
         NFLAG = 0
   40   CONTINUE
C       ONE MORE BYTE TO GO
        J = JN + NFLAG
C       CHECK AND MODIFY BYTE IF IT CANNOT BE TRANSMITTED
        CALL CHEKBT(TEMP(IBYTE),NSTAT,IBYTE)
C       IF CHARACTER HAS BEEN MODIFIED AND ONLY ONE CHARACTER IN WORD
C       NEED A CHARACTER WITH A CONTINUE BIT - CHECK AND PUT IN IF
C       REQUIRED
        IF (NSTAT .NE. 0) THEN
          IF(IONLY .EQ. 0) THEN
             IOFF = IOFF + 1
             IF (IOFF .GT. IMAX) GO TO 120
             CBUFF(IOFF) = CHAR(64)
          ENDIF
        ENDIF
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 120
        CBUFF(IOFF) = TEMP(IBYTE)
        IF (NSTAT .EQ. 0) GO TO 100
C       CHARACTER WAS MODIFIED PUT OUT THE CODE 1000000 0000001 TO
C       INDICATE THAT IT WAS
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 120
        CBUFF(IOFF) = NCNTRL
        J = 1
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 120
        CBUFF(IOFF) = TEMP(IBYTE)
  100 CONTINUE
C     FINISHED COMPRESSING NOW MAKE UP TO A MULTIPLE OF 80 CHARACTERS
C     FIND OUT HOW MANY CHARACTERS TO PUT IN
      K = 80 - (IOFF - ((IOFF/80) * 80))
      IF (K .LT. 2) K = K + 80
C     FILL TO A MULTIPLE OF 80 CHARACTERS WITH REPEATS OF 
C     THE BIT PATTERN 1000000
      DO 110 I = 1,K
         IOFF = IOFF + 1
         IF(IOFF .GT. IMAX) GO TO 120
         CBUFF(IOFF) = NCNTRL
  110 CONTINUE
  120 CONTINUE
      LC = IOFF
      IF (IOFF .GT. IMAX) THEN
         LC = IMAX
         IERROR = -1
      ENDIF
      RETURN
      END
C******************************************************************
C                                                                 *
C     SUBROUTINE CMPRS8(LX,IX,LC,CBUFF,IERROR)                    *
C                                                                 *
C     ROUTINE TO WORD-COMPRESS DATA INTO 8 BIT BYTES.             *
C     INPUTS ARE INTEGER*4 ARRAY IX CONTAINING LX VALUES TO BE    *
C     COMPRESSED INTO 8 BIT BYTES.                                *
C     ON ENTRY LC SHOULD CONTAIN DIMENSIONED LENGTH OF CBUFF.     *
C     OUTPUTS ARE CHARACTER*1 ARRAY CBUFF WITH LC MODIFIED TO     * 
C     REFLECT THE NUMBER OF BYTES WHICH HAVE BEEN PUT INTO CBUFF. *
C     LC IS MADE A MULTIPLE OF 80 USING UNIQUE FILL CHARACTERS.   *
C     IF CBUFF IS TOO SMALL TO CONTAIN ALL COMPRESSED DATA        *
C     IERROR WILL CONTAIN -1 OTHERWISE IT WILL BE SET TO ZERO.    *
C     METHODOLOGY IS THAT THE MOST SIGNIFICANT BIT OF EACH BYTE   *
C     IS SET TO ONE IF NEXT BYTE CONTAINS INFORMATION PERTAINING  *
C     TO THE SAME SAMPLE. IN THE FIRST BYTE PERTAINING TO EACH    *
C     SAMPLE, THE SECOND MOST SIGNIFICANT BIT IS USED AS A SIGN   *
C     BIT. THE LEAST SIGNIFICANT 6 BITS OF THE FIRST BYTE AND     *
C     THE LEAST SIGNIFICANT SEVEN BITS OF ANY SUBSEQUENT BYTE     *
C     PERTAINING TO THE SAME SAMPLE ARE COMPRESSED TOGETHER ON    *
C     DECODING TO GIVE THE MAGNITUDE OF THE SAMPLE.               * 
C     USING THIS COMPRESSION SCHEME THE CODES                     *
C     10000000 10000000 AND 10000000 00000001 WOULD NOT NORMALLY  *
C     BE TRANSMITTED (AKIN TO NOT TRANSMITTING LEADING ZEROS)     *
C     THESE TWO PATTERNS ARE USED AS CONTROL PATTERNS.            *
C     THE PATTERN 10000000 10000000 SIGNIFIES THE END OF THE DATA *
C     AND IS USED TO FILL UP TO A MULTIPLE OF EIGHTY CHARACTERS.  *
C     THE PATTERN 10000000 0000001 INDICATES THAT 32 SHOULD BE    *
C     SUBTRACTED FROM THE PREVIOUS SAMPLE. USING THIS METHODOLOGY *
C     ANY CONTROL CHARACTER WHICH A COMMUNICATION LINK WONT ACCEPT*
C     CAN BE MODIFIED BY ADDING 32 TO IT. THE 32 WILL BE SUB-     *
C     TRACTED ON DECOMPRESSION                                    *
C     DATA MAY BE DECOMPRESSED USING SUBROUTINE DCOMP8            *
C                                                                 *
C     SUBROUTINES CALLED   ---   INTAND   ---   CHEKBT   ---      * 
C                                                                 *
C******************************************************************
C
      SUBROUTINE CMPRS8(LX,IX,LC,CBUFF,IERROR)
      IMPLICIT INTEGER *4 (I-N)
cuk      CHARACTER *1 CBUFF(4),TEMP(4),TEST(4),NCNTRL
cuu91      CHARACTER *1 CBUFF(1),TEMP(4),TEST(4),NCNTRL
      CHARACTER *1 CBUFF(lc),TEMP(4),TEST(4),NCNTRL
cuu91      INTEGER *4 IX(*)
      INTEGER *4 IX(lx)
      EQUIVALENCE (J,TEMP),(TEST,ITEST)
      DATA TEST/'1','2','3','4'/
      DATA N5/32/,
     *     N6/64/,
     *     N7/128/,       N7M1/127/,
     *     N12/4096/,
     *     N13/8192/,
     *     N14/16384/,    N14M1/16383/,
     *     N19/524288/,
     *     N20/1048576/,
     *     N21/2097152/,  N21M1/2097151/,
     *     N26/67108864/, N28M1/268435455/
      IBYTE = 4
C     WORK OUT WHICH WAY BYTES ARE STORED IN COMPUTER
      IF(ITEST .EQ. (((52*256+51)*256+50)*256+49)) IBYTE = 1
      J = N7 
      NCNTRL = TEMP(IBYTE)
C     NCNTRL IS AN EIGHT BIT BYTE WITH BIT PATTERN 10000000
C
      MFLAG = N7
      IOFF = 0
      IMAX = LC
      IERROR = 0 
      DO 100 II = 1,LX
        IFIX = 0
        JN = IX(II)
C
C       SEE IF NUMBER IS -VE.
C
        NFLAG = 0
        IF(JN .LT. 0) THEN
           NFLAG = N6
           JN = -JN
        END IF
        IF(JN .LT. N6) GO TO 40
C        IF HERE, DATA REQUIRES MORE THAN 1 BYTE
         IF(JN. LT. N13) GO TO 30
C         IF HERE, DATA REQUIRES MORE THAN 2 BYTES
          IF(JN. LT. N20) GO TO 20
C          IF HERE, DATA REQUIRES MORE THAN THREE BYTES
           IF(JN. GT. N28M1) JN = N28M1
C          FILL A BYTE
           J = JN/N21 + NFLAG + MFLAG
           IOFF = IOFF + 1
           IF (IOFF .GT. IMAX) GO TO 120
           CBUFF(IOFF) = TEMP(IBYTE)
           CALL INTAND(JN,N21M1,JN)
           NFLAG = 0
   20     CONTINUE
C         IF HERE, DATA REQUIRES THREE MORE BYTES
          J = JN/N14 + NFLAG + MFLAG
          IOFF = IOFF + 1
          IF (IOFF .GT. IMAX) GO TO 120
          CBUFF(IOFF) = TEMP(IBYTE)
          CALL INTAND(JN,N14M1,JN)
          NFLAG = 0
   30    CONTINUE
C        IF HERE, DATA REQUIRES TWO MORE BYTES
         J = JN/N7 + NFLAG + MFLAG
         IOFF = IOFF + 1
         IF (IOFF .GT. IMAX) GO TO 120
         CBUFF(IOFF) = TEMP(IBYTE)
         CALL INTAND(JN,N7M1,JN)
         NFLAG = 0
   40   CONTINUE
C       FILL IN LAST REMAINING BYTE
        J = JN + NFLAG
C       CHECK AND MODIFY BYTE IF IT CANNOT BE TRANSMITTED
        CALL CHEKBT(TEMP(IBYTE),NSTAT,IBYTE)
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 120
        CBUFF(IOFF) = TEMP(IBYTE)
        IF(NSTAT .EQ. 0) GO TO 100
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 120
        CBUFF(IOFF) = NCNTRL
        J = 1
        IOFF = IOFF + 1
        IF (IOFF .GT. IMAX) GO TO 120
        CBUFF(IOFF) = TEMP(IBYTE)
  100 CONTINUE
C     FINISHED COMPRESSING NOW MAKE UP TO A MULTIPLE OF 80 CHARACTERS
C     FIND HOW MANY CHARACTERS TO PUT IN
      K = 80 - (IOFF - ((IOFF/80) * 80))
      IF (K .LE. 2) K = K + 80
C     NEED AT LEAST TWO NCNTRL CHARACTERS TOGETHER FOR UNIQUENESS
      DO 110 I = 1,K
         IOFF = IOFF + 1
         IF (IOFF .GT. IMAX) GO TO 120
         CBUFF(IOFF) = NCNTRL
  110 CONTINUE
  120 CONTINUE
      LC = IOFF 
      IF ( IOFF .GT. IMAX) THEN
        LC = IMAX
        IERROR = -1
      ENDIF
      RETURN
      END
C***********************************************************************
C                                                                      *
C     SUBROUTINE DCOMP6(LB,IBUF,LOUT,IOUT,IERROR)                      *
C                                                                      *
C     SUBROUTINE DECOMPRESS INTEGER DATA THAT HAS BEEN COMPRESSED      *
C     INTO ASCII CHARACTERS AND RETURNS VALUES IN INTEGER *4 FORMAT    *
C     SEE SUBROUTINE CMPRS6 FOR COMPRESSION FORMAT                     *
C     INPUT - IBUF AN ARRAY OF CONTAINING  LB CHARACTERS.              *
C     LOUT SHOULD CONTAIN THE DIMENSION OF THE INTEGER *4 ARRAY IOUT.  * 
C     ON RETURN ARRAY LOUT WILL BE SET TO CONTAIN THE NUMBER OF INTEGER*
C     VALUES WHICH HAVE BEEN PUT IN THE ARRAY IOUT.                    *
C     IF THE ARRAY IOUT IS NOT LARGE ENOUGH TO CONTAIN ALL OF THE      *
C     DECOMPRESSED VALUES IERROR WILL BE SET TO -1 OTHERWISE IT WILL   *
C     SET TO ZERO                                                      *
C***********************************************************************
C
      SUBROUTINE DCOMP6(LB,IBUF,LOUT,IOUT,IERROR)
      IMPLICIT INTEGER *4 (I-N)
cuk      INTEGER *4 IOUT(*),ICHAR(128)
cuu91      INTEGER *4 IOUT(1),ICHAR(128)
      INTEGER *4 IOUT(lout),ICHAR(128)
cuu91      CHARACTER *1 IBUF(1), ACHAR(4), TEST(4)
      CHARACTER *1 IBUF(lb), ACHAR(4), TEST(4)
      CHARACTER *1 ASPACE,LFEED,CRETN
      EQUIVALENCE (INN,ACHAR),(ITEST,TEST)
      DATA TEST/'1','2','3','4'/
      data ichar/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     1 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     2 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,2,
     3 3,4,5,6,7,8,9,10,11,0,0,0,0,0,0,0,
     4 12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,
     5 28,29,30,31,32,33,34,35,36,37,0,0,0,0,0,0,
     6 38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,
     7 54,55,56,57,58,59,60,61,62,63,0,0,0,0,0,0/
      IERROR = 0
      CRETN = CHAR(13)
      LFEED = CHAR(10)
      ASPACE = CHAR(32)
      IMAX = LOUT
      ISIGN = 16
      IOFLOW = 32
      MASK1 = 15
      MASK2 = 31
C     WORK OUT WHICH WAY BYTES ARE STORED IN COMPUTER
      IBYTE = 4
      IF (ITEST.EQ.(((52*256+51)*256+50)*256+49)) IBYTE = 1
      ICOUNT = 0
      I = 0
      J = 0
C     START OF DECODING
   1  CONTINUE
        I = I + 1
        ACHAR(IBYTE) = IBUF(I)
C       IF A CARRIAGE OR LINE FEED IGNORE, IF A SPACE THEN END OF DATA
        IF (ACHAR(IBYTE) .EQ. CRETN) GO TO 1 
        IF (ACHAR(IBYTE) .EQ. LFEED) GO TO 1 
        IF (ACHAR(IBYTE) .EQ. ASPACE) GO TO 5
        ICOUNT = ICOUNT + 1
C       STRIP OFF ANY HIGHER ORDER BITS
        CALL INTAND(INN,127,K)
C       GET NUMBER REPRESENTION OF INPUT CHARACTER
        INN = ICHAR(K)
C       GET SIGN BIT
        CALL INTAND(INN,ISIGN,JSIGN)
C       GET CONTINUATION BIT (IF ANY)
        CALL INTAND(INN,IOFLOW,JOFLOW)
C       REMOVE BITS WE DONT WANT
        CALL INTAND(INN,MASK1,ITEMP)
   2    CONTINUE
          IF(JOFLOW.EQ.0) GO TO 4
C         THERE IS ANOTHER BYTE IN THIS SAMPLE
          ITEMP = ITEMP * 32
   3        CONTINUE
            I = I + 1
            ACHAR(IBYTE) = IBUF(I)
          IF (ACHAR(IBYTE) .EQ. CRETN) GO TO 3 
          IF (ACHAR(IBYTE) .EQ. LFEED) GO TO 3 
          ICOUNT = ICOUNT + 1
C         STRIP OFF ANY HIGHER ORDER BITS
          CALL INTAND(INN,127,K)
          INN = ICHAR(K)
C         GET CONTINUATION BIT (IF ANY)
          CALL INTAND(INN,IOFLOW,JOFLOW)
          CALL INTAND(INN,MASK2,K)
          ITEMP = ITEMP + K
        GO TO 2
   4    CONTINUE
        IF (JSIGN.NE.0) ITEMP = -ITEMP
        J = J +1
        IF (J .GT. IMAX) GO TO 5
        IOUT(J) = ITEMP
      IF(ICOUNT.LT.LB) GO TO 1
   5  CONTINUE
      LOUT = J
      IF (J .GT. IMAX) THEN
         LOUT = IMAX
         IERROR = -1
      ENDIF
      RETURN
      END
C************************************************************************
C                                                                       *
C     SUBROUTINE DCOMP7(LB,IBUF,LOUT,IOUT,IERROR)                       *
C                                                                       *
C     SUBROUTINE DECOMPRESS INTEGER DATA THAT HAS BEEN COMPRESSED       *
C     INTO 7 BIT CHARACTERS AND RETURNS VALUES IN INTEGER *4 FORMAT.    *
C     SEE SUBROUTINE CMPRS7 FOR COMPRESSION FORMAT.                     *
C     INPUT - IBUF AN ARRAY OF CHARACTERS OF LENGTH LB.                 *
C     LOUT SHOULD CONTAIN THE LENGTH OF THE INTEGER *4 OUTPUT ARRAY IOUT*
C     ON RETURN ARRAY IOUT WILL CONTAIN LOUT INTEGER VALUES.            *
C     IF THE ARRAY IOUT WAS LARGE ENOUGH TO CONTAIN ALL CONVERTED       *
C     VALUES, IERROR WILL CONTAIN ZERO. IF NOT IT WILL BE SET TO -1     *
C                                                                       *
C************************************************************************
C
      SUBROUTINE DCOMP7(LB,IBUF,LOUT,IOUT,IERROR)
      IMPLICIT INTEGER *4 (I-N)
cuk      INTEGER *4 IOUT(*)
cuu91      INTEGER *4 IOUT(1)
      INTEGER *4 IOUT(lout)
cuu91      CHARACTER *1 IBUF(1),ACHAR(4),TEST(4)
      CHARACTER *1 IBUF(lb),ACHAR(4),TEST(4)
      EQUIVALENCE (INN,ACHAR),(ITEST,TEST)
      DATA TEST/'1','2','3','4'/
      DATA ISIGN/32/, IOFLOW/64/, MASK1/31/, MASK2/63/
      DATA ICORR/32/, MASK3/127/, NCNTRL/64/
C     WORK OUT WHICH WAY BYTES ARE STORED IN COMPUTER
      IBYTE = 4
      IF (ITEST.EQ.(((52*256+51)*256+50)*256+49)) IBYTE = 1
      I = 0
      J = 0
      IMAX = LOUT
C     START OF DECODING
   1  CONTINUE
        IEND = 0
        I = I + 1
        ACHAR(IBYTE) = IBUF(I)
C       REMOVE MOST SIGNIFICANT BIT - NOT USED
        CALL INTAND(INN,MASK3,INN)
C       TEST IF CHARACTER IS EQUAL TO NCNTRL
        IF (INN .EQ. NCNTRL) IEND =  1
C       GET SIGN BIT
        CALL INTAND(INN,ISIGN,JSIGN)
C       GET CONTINUATION BIT
        CALL INTAND(INN,IOFLOW,JOFLOW)
C       REMOVE SIGN AND CONTINUATION BITS
        CALL INTAND(INN,MASK1,ITEMP)
   2    CONTINUE
        IF(JOFLOW .EQ. 0) GO TO 3
C       THERE IS ANOTHER BYTE IN THIS SAMPLE
        I = I + 1
        ACHAR(IBYTE) = IBUF(I)
C       REMOVE MOST SIGNIFICANT BIT - NOT USED
        CALL INTAND(INN,MASK3,INN)
C       TEST IF CHARACTER IS EQUAL TO NCNTRL 
        IF (IEND .EQ. 1) THEN
           IF (INN .EQ. NCNTRL) GO TO 4
C          BIT PATTERN 1000000 FOLLOWED BY 1000000 MEANS END OF DATA
C          SEE IF PREVIOUS SAMPLE HAS TO BE MODIFIED
            IF (INN .LT. 16) THEN
               ITEMP = ICORR
C              SUBTRACT CORRECTION FROM PREVIOUS SAMPLE
               IF (IOUT(J) .LT. 0) ITEMP = -ITEMP
               IOUT(J) = IOUT(J) - ITEMP
               GO TO 1
           ENDIF
      ENDIF
      IEND = 0
C     GET CONTINUATION BIT
      CALL INTAND(INN,IOFLOW,JOFLOW)
      CALL INTAND(INN,MASK2,K)
C     SHIFT WHAT WE HAVE 6 BITS LEFT AND ADD IN NEW BITS
      ITEMP = ITEMP * 64 + K
      GO TO 2
   3  CONTINUE
      IF(JSIGN .NE. 0) ITEMP = -ITEMP
      J = J + 1
      IF (J .GT. IMAX) GO TO 4
      IOUT(J) = ITEMP
      IF(I. LT. LB) GO TO 1
   4  CONTINUE
      LOUT = J
      IF (J .GT. IMAX) THEN
         LOUT = IMAX
         IERROR = -1
      ENDIF
      RETURN
      END
C************************************************************************
C     SUBROUTINE DCOMP8(LB,IBUF,LOUT,IOUT,IERROR)                       *
C     SUBROUTINE DECOMPRESS INTEGER DATA THAT HAS BEEN COMPRESSED       * 
C     INTO 8 BIT CHARACTERS AND RETURNS VALUES IN INTEGER *4 FORMAT.    *
C     SEE SUBROUTINE CMPRS8 FOR COMPRESSION FORMAT.                     *
C     INPUT - IBUF AN ARRAY OF CHARACTERS OF LENGTH LB.                 *
C     LOUT SHOULD CONTAIN THE LENGTH OF THE INTEGER *4 OUTPUT ARRAY IOUT*
C     ON RETURN ARRAY IOUT WILL CONTAIN LOUT INTEGER VALUES.            *
C     IF THE ARRAY IOUT WAS LARGE ENOUGH TO CONTAIN ALL CONVERTED       *
C     VALUES IERROR WILL CONTAIN ZERO. IF NOT IT WILL BE SET TO -1      *
C************************************************************************
      SUBROUTINE DCOMP8(LB,IBUF,LOUT,IOUT,IERROR)
      IMPLICIT INTEGER *4 (I-N)
cuk      INTEGER *4 IOUT(*)
cuu91      INTEGER *4 IOUT(1)
      INTEGER *4 IOUT(lout)
cuu91      CHARACTER *1 IBUF(1),ACHAR(4),TEST(4)
      CHARACTER *1 IBUF(lb),ACHAR(4),TEST(4)
      EQUIVALENCE (INN,ACHAR),(ITEST,TEST)
      DATA TEST/'1','2','3','4'/
      DATA N5/32/,N12/4096/,N19/524288/,N26/67108864/
      DATA ISIGN/64/,IOFLOW/128/,MASK1/63/,MASK2/127/
      DATA NCNTRL/128/,ICORR/32/
C     WORK OUT WHICH WAY BYTES ARE STORED IN COMPUTER
      IBYTE = 4
      IF (ITEST.EQ.(((52*256+51)*256+50)*256+49)) IBYTE = 1
      I = 0
      J = 0
      IERROR = 0
      IMAX = LOUT
C     START OF DECODING
   1  CONTINUE
        IEND = 0
        ISUB = 0
        I = I + 1
        ACHAR(IBYTE) = IBUF(I)
C       TEST IF CHARACTER IS EQUAL TO NCNTRL (100000000)
        IF(INN.EQ.NCNTRL) IEND = 1
C       GET SIGN BIT
        CALL INTAND(INN,ISIGN,JSIGN)
C       GET CONTINUATION BIT
        CALL INTAND(INN,IOFLOW,JOFLOW)
C       REMOVE SIGN AND CONTINUATION BITS
        CALL INTAND(INN,MASK1,ITEMP)
   2    CONTINUE
          IF(JOFLOW.EQ.0) GO TO 3
C         THERE IS ANOTHER BYTE IN THIS SAMPLE
          I = I + 1
          ACHAR(IBYTE) = IBUF(I)
C         TEST IF CHARACTER IS EQUAL TO NCNTRL 
          IF (IEND.EQ.1) THEN
             IF (INN .EQ. NCNTRL) GO TO 100
C            IF TWO CONTROL CHARACTERS IN A ROW THEN END OF DATA
C            SEE IF PREVIOUS SAMPLE HAS TO BE MODIFIED
             IF (INN.LT.16) THEN
                ITEMP = ICORR
C               NOW SUBTRACT CORRECTION FROM PREVIOUS SAMPLE
                IF (IOUT(J).LT.0) ITEMP = - ITEMP 
                IOUT(J) = IOUT(J) - ITEMP
              GO TO 1
             ENDIF 
          ENDIF 
          IEND = 0
          ISUB = 0
C         GET CONTINUATION BIT
          CALL INTAND(INN,IOFLOW,JOFLOW)
C         MASK OFF CONTINUATION BIT
          CALL INTAND(INN,MASK2,K)
C         SHIFT WHAT WE HAVE SO FAR 7 BITS LEFT AND ADD IN NEXT BITS
          ITEMP = ITEMP * 128 + K
        GO TO 2
   3    CONTINUE
        IF(JSIGN.NE.0) ITEMP = -ITEMP
        J = J + 1
        IF (J .GT. IMAX) GO TO 100
        IOUT(J) = ITEMP
      IF(I. LT. LB) GO TO 1
  100 CONTINUE
      LOUT = J
      IF (J .GT. IMAX) THEN
        LOUT = IMAX
        IERROR = -1
      ENDIF
      RETURN
      END
C**********************************************************************
C                                                                     *
C      SUBROUTINE INTAND(I1,I2,I3)                                    *
C                                                                     *
C      SUBROUTINE BITWISE "ANDS" THE INTEGERS IN I1 AND I2            *
C      AND RETURNS THE RESULT IN INTEGER I3                           *
C      FOR EXAMPLE                                                    *
C      IF THE 32 BITS IN I1 ARE 11001100 1110001110 11110000 11111111 *
C      AND IN I2 ARE            10101010 1100110011 00110011 00110011 *
C      I3 WILL BECOME           10001000 1100000010 00110000 00110011 *
C                                                                     *
C      NOTE "AND" IS NOT STANDARD FORTRAN 77 FUNCTION SO THIS IS A    *
C      MACHINE DEPENDANT SUBROUTINE                                   *
C**********************************************************************
C
cks       SUBROUTINE INTAND(I1,I2,I3) 
cks       INTEGER *4 I1,I2,I3
cksC##SUN## REMOVE COMMENT FROM NEXT LINE FOR SUN COMPUTERS
cks       I3 = AND(I1,I2)
cksC##DEC## REMOVE COMMENT FROM NEXT LINE FOR PDP AND VAX COMPUTERS and HP's
ckscuk       I3 = JIAND(I1,I2)
cksC##IBM## REMOVE COMMENT FROM NEXT LINE FOR IBM PC'S (I THINK)
cksC      I3 = IAND(I1,I2)
cks       RETURN
cks       END


c =============================================================================

c      -- this routine is machine and compiler dependend.  The appropriate
c      -- command should be selected by the 'conf.csh' script.
c      -- K.S. 21-Aug-98

c       SUBROUTINE INTAND(I1,I2,I3) 
c       INTEGER *4 I1,I2,I3
c       I3 = AND(I1,I2)   ! Sun
c       I3 = AND(I1,I2)   ! Linux (here same as sun)
c       I3 = JIAND(I1,I2)    ! HP, VAX
c      I3 = IAND(I1,I2)     ! PC ?
c       RETURN
c       END
 
c
c   put in by jh feb 3 2003
c
C**********************************************************************
C                                                                     *
C      SUBROUTINE INTAND(I1,I2,I3)                                    *
C                                                                     *
C      SUBROUTINE BITWISE "ANDS" THE INTEGERS IN I1 AND I2            *
C      AND RETURNS THE RESULT IN INTEGER I3                           *
C      FOR EXAMPLE                                                    *
C      IF THE 32 BITS IN I1 ARE 11001100 1110001110 11110000 11111111 *
C      AND IN I2 ARE            10101010 1100110011 00110011 00110011 *
C      I3 WILL BECOME           10001000 1100000010 00110000 00110011 *
C                                                                     *
C      NOTE "AND" IS NOT STANDARD FORTRAN 77 FUNCTION SO THIS IS A    *
C      MACHINE DEPENDANT SUBROUTINE                                   *
C**********************************************************************
C
       SUBROUTINE INTAND(I1,I2,I3) 
       INTEGER *4 I1,I2,I3
       logical sun,pc,linux
       call computer_type(sun,pc,linux)
COMPUTERTYPE
C##SUN## REMOVE COMMENT FROM NEXT LINE FOR SUN COMPUTERS
          if(sun.or.linux) I3 = AND(I1,I2)
C##DEC## REMOVE COMMENT FROM NEXT LINE FOR PDP AND VAX COMPUTERS,+sun
c          if(sun.or.linux)I3 = JIAND(I1,I2)
C##IBM## REMOVE COMMENT FROM NEXT LINE FOR IBM PC'S (I THINK), now also sun !
        if(pc)I3 = IAND(I1,I2)
       RETURN
       END
c

      
       
C************************************************************************
C                                                                       *
C     SUBROUTINE CHEKBT(ACHAR,NSTAT,IBYTE)                              *
C                                                                       *
C     SUBROUTINE CHECKS WHETHER THE CHARACTER ACHAR IS ONE WHICH CANNOT *
C     BE TRANSMITTED OVER A COMMUNICATIONS LINK. IF THE CHARACTER       *
C     CAN BE TRANSMITTED NSTAT IS SET TO 0 OTHERWISE IT IS SET TO 1.    *
C     IF NSTART IS SET TO ONE A ONE BIT IS ADDED INTO THE SIXTH MOST    *
C     SIGNIFICANT BIT OF ACHAR. THIS IS REMOVED IN THE DECODING PROGRAM *
C     THE SUBROUTINE HAS BEEN INITIALLY SET UP TO STOP CONTROL Z,       *
C     CONTROL C, CONTROL P AND CONTROL D CHARACTERS FROM BEING          *   
C     TRANSMITTED. THESE MAY HAVE TO BE CHANGED, DELETED OR ADDED TO    *
C     DEPENDING ON THE PARTICULAR CIRCUIT AND COMPUTERS.                *
C     FOR A FULL BINARY LINK, NO CONTROL CHARACTERS NEED BE STOPPED     *
C     AND THIS SUBROUTINE WILL BECOME REDUNDANT.                        * 
C     TO ADD ANOTHER CONTROL CHARACTER DEFINE IT, SPECIFY IT AND TEST   *
C     FOR IT.                                                           *
C     IBYTE = 1 FOR COMPUTERS WHICH STORE A CHARACTER IN THE MOST       *
C     SIGNIFICANT BITS OF AN INTEGER  *4 WORD. IBYTE = 4 FOR            *
C     COMPUTERS WHICH STORE A CHARACTER IN THE LEAST SIGNIFICANT        *
C     BYTE OF A COMPUTER WORD.                                          *
C     NOTE THIS SUBROUTINE WILL NOT WORK FOR CHARACTERS WHOSE 8 BIT     *
C     INTEGER REPRESENTATION IS GREATER THAN 31                         *
C     MODIFIED 28TH NOVEMBER 1988 BECAUSE OF ERRORS ON VAX COMPUTERS    *
C************************************************************************
C
      SUBROUTINE CHEKBT(ACHAR,NSTAT,IBYTE)
      IMPLICIT INTEGER *4 (I-N)
      CHARACTER *1 ACHAR,CNTRLZ,CNTRLC,CNTRLP,CNTRLD
      CHARACTER *1 OVRLAY,CHAR32
      INTEGER *4 ITEMP,ITEMP1
      EQUIVALENCE (ITEMP,OVRLAY),(ITEMP1,CHAR32)
      ITEMP = 0
      ITEMP1 = 0
      CHAR32 = CHAR(32)
C     DEFINE CONTROL CHARACTERS WHICH SHOULD NOT BE TRANSMITTED
C     THESE WILL VARY FOR DIFFERENT CIRCUITS AND COMPUTERS AND MAY HAVE
C     TO BE FOUND BY TRIAL AND ERROR.
      CNTRLZ = CHAR(26)
      CNTRLC = CHAR(3)
      CNTRLP = CHAR(16)
      CNTRLD = CHAR(4)
      NSTAT = 0
      IF(ACHAR.EQ.CNTRLZ) NSTAT = 1
      IF(ACHAR.EQ.CNTRLC) NSTAT = 1
      IF(ACHAR.EQ.CNTRLP) NSTAT = 1
      IF(ACHAR.EQ.CNTRLD) NSTAT = 1
      IF(NSTAT.EQ.1) THEN
         OVRLAY = ACHAR
         ITEMP = ITEMP + ITEMP1
         ACHAR = OVRLAY
      ENDIF
      RETURN
      END
