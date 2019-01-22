********************************************************************
c
c-this subroutine checks the type line for all lines and
c-calls the specific subroutine to check the line.
c-after the line checked, it prints the error if some was found
c
c     Input:
c       data: records
c       nrec: number of records
c       evfile: file name
c       unit_number: unit number
c
c     Output:
c       nerr: number of errors found
c       gate: output destination
c         0 - screen
c         1 - file
c         2 - both
c
c     written by Carlos Eduardo Vieira, Nov. 1994.
c     Updates:
c
c     dec 14, 94 by j.h. : error using i15 format on pc
c     dec 15             : check if type 1 line first in file
c     dec 29             : accept tline type E, but do not check
c     jun 7              : accept R and T components
c     jun9               : accepct locating indicator *
c     jan 8 96           : accept component V
c     feb 28, 97         : accept id line UP, phase secs in field 23-27
c                          accept magnitude without type
c                          seconds =60 on main header, seconds up to 500 for
c                          phase time
c     nov 12             : accept DUB, NEW, REE
c     aug 98 by jh       : ----------   version 7.0 check ------------------
c                          station name to 5 char
c     oct 01 99 jh       : accept DUP
c     oct 4              : accept ARG
c     jan 7 00           : accept year ge 2000 in id line
c     may 2000 jh        : add  IRIS channel band codes
c     oct 24             : add H-line
c     mar 2 2001 jh      : accept distnace in f-format
c     nov 23 2009 jh     : accepth P as component type, used at inpres
c     mar 1 2010  jh     : accept new magnitudes s and b
c 2010-05-05 pv          : compo accept all seed codes, new are;A,B,C,1,2,3,U,V,W,S
c 2011 02 10 jh          : for f-line, only check strike, dip and rake, to be fixed
c 2011 10 27 jh          : initial type M check
c 2013 10 30 jh          : accept line type P, accepth intensity type EM, was corrected
c                          before ??
c 2014 02 13 jh          : accepth DPH
c 2015 05 13 jh          : accept HIN
c 2015 05 31 jh          : accept  negative depth down to -9.9
c 2015 10 30 jh          : accept ARX
c 2018 11 30 jh          : add LOC
c
      subroutine check_s(data,nrec,evfile,nerr,gate,unit_number)
      implicit none
      character*80 data(*)           ! records
      integer nrec                   ! number of records
      character*80 evfile            ! file name
      integer nerr                   ! number of errors found
      integer gate                   ! output destination
      character*80 ermsg(80)           ! array for error message
      character*1 tline               ! type line
      character*80 cline              ! current line
      integer uni                     ! output unit
      integer i                       ! counter
      integer ind                     ! index
      integer nerror                  ! aux for number of errors
      logical line1                   ! flag for line type 1
      logical lineID                  ! flag for line type ID
      logical errorfound              ! flag for error found 
      integer unit_number             ! unit number

      uni = 6
      nerr = 0
      nerror = 0
      lineID = .false.
      line1 = .false.
      errorfound = .false.
c
c   check that the first line is of type 1, if not add type 1
c
         if(data(1)(80:80).ne.'1') then
               data(1)(80:80)='1'
               if(gate.eq.0.or.gate.eq.2) 
     *         write(6,*)' No line type of first line,',
     *         ' assume type 1'
               if(gate.gt.0) write(unit_number,*)
     *         ' No line type of first line,',
     *                       ' assume type 1'
         endif
c
c-reads until end of file
      do i=1,nrec 
c-sets variable 'cline' with the current line
         cline = data(i) (1:80)
c-sets variable 'tline' with the type line
         tline = cline (80:80)
c-clears the error message variable
         do ind=1,80
            ermsg(ind) = ' '
         end do
         ind = 1
c-checking the type line
         if (tline .eq. '1') then
            line1 = .true. 
            call check_line_type_1(cline,ermsg,nerr)
         else if (tline .eq. '2') then
            call check_line_type_2(cline,ermsg,nerr)
         else if (tline .eq. '4' .or. tline .eq. ' ') then
            call check_line_type_4(cline,ermsg,nerr)
         else if (tline .eq. '6') then
            call check_line_type_6(cline,ermsg,nerr)
         else if (tline .eq. 'F') then
            call check_line_type_M(cline,ermsg,nerr)
         else if (tline .eq. 'M') then
            call check_line_type_M(cline,ermsg,nerr)
         else if (tline .eq. 'I') then
            lineID = .true.
            call check_line_type_I(cline,ermsg,nerr)
         else if (tline .ne. '1' .and. tline .ne. '2' .and.
     1            tline .ne. '3' .and. tline .ne. '4' .and.
     2            tline .ne. '5' .and. tline .ne. '6' .and.
     3            tline .ne. '7' .and. tline .ne. 'F' .and.
     4            tline .ne. 'I' .and. tline .ne. 'E' .and.
     5            tline .ne. 'H' .and. tline .ne. 'P') then
                  if (gate .eq. 0) then
                     uni = 6
                     write( uni,*) ' Unexpected type line'
                     write( uni,*) cline
                  else if (gate .eq. 1) then
                     uni = unit_number
                     if (.not. errorfound) then
                         write( uni,*) '  '
                         write( uni,*) '  '
                         write( uni,*) '  '
                         write(uni,*)'Error found in file: ',evfile
                         errorfound = .true.
                     end if
                     write( uni,*) 'Unexpected type line'
                     write( uni,*) cline
                  else if (gate .eq. 2) then
                     uni = 6
                     write( uni,*) ' Unexpected type line'
                     write( uni,*) cline
                     uni = unit_number
                     if (.not. errorfound) then
                         write( uni,*) '  '
                         write( uni,*) '  '
                         write( uni,*) '  '
                         write(uni,*)'Error found in file: ',evfile
                         errorfound = .true.
                     end if
                     write( uni,*) 'Unexpected type line'
                     write( uni,*) cline
                  end if
         end if
c-checking if some error was found - if found, print it
         if (nerror .ne. nerr) then
            if (gate .eq. 0) then
                uni = 6
                write( uni,*)
                write( uni,'(a)') cline
                write( uni,'(a)') ermsg(ind) (1:80)
            else if (gate .eq. 1) then
                uni = unit_number
                if (.not. errorfound) then
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) 'Error found in file: ',evfile
                    errorfound = .true.
                end if
                write( uni,'(a)') cline
                write( uni,'(a)') ermsg(ind) (1:80)
            else if (gate .eq. 2) then
                uni = 6
                write( uni,'(a)') cline
                write( uni,'(a)') ermsg(ind) (1:80)
                uni = unit_number
                if (.not. errorfound) then
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write(uni,*) 'Error found in file: ',evfile
                    errorfound = .true.
                end if
                write( uni,'(a)') cline
                write( uni,'(a)') ermsg(ind) (1:80)
            end if
            nerror = nerr
         end if
      end do
         if (.not. line1) then
            if (gate .eq. 0) then
                uni = 6
                write( uni,*) ' Line type 1 is missing'
            else if (gate .eq. 1) then
                uni = unit_number
                if (.not. errorfound) then
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) 'Error found in file: ',evfile
                    errorfound = .true.
                end if
                write( uni,*) 'Line type 1 is missing'
            else if (gate .eq. 2) then
                uni = 6
                write( uni,*) ' Line type 1 is missing'
                uni = unit_number
                if (.not. errorfound) then
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) 'Error found in file: ',evfile
                    errorfound = .true.
                end if
                write( uni,*) 'Line type 1 is missing'
            end if
         end if
         if (.not. lineID) then
            if (gate .eq. 0) then
                uni = 6
                write( uni,*) ' Line type I is missing'
            else if (gate .eq. 1) then
                uni = unit_number
                if (.not. errorfound) then
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) 'Error found in file: ',evfile
                    errorfound = .true.
                end if
                write( uni,*) 'Line type I is missing'
            else if (gate .eq. 2) then
                uni = 6
                write( uni,*) ' Line type I is missing'
                uni = unit_number
                if (.not. errorfound) then
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) '  '
                    write( uni,*) 'Error found in file: ',evfile
                    errorfound = .true.
                end if
                write( uni,*) 'Line type I is missing'
            end if
         end if
      end 

c-this subroutine checks all the importants variables
c-for the line type 1
      subroutine check_line_type_1(cline,ermsg,nerr)
      implicit none
      character*80 cline           ! dummy current line
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of error
      real left                     ! value left after division 
      integer status                ! iostatus 
      integer year                  ! year
      integer month                 ! month
      integer day                   ! day
      integer hour                  ! hour
      integer minute                ! minutes
      real secnds                   ! seconds
      character*1 dind              ! distance indicator
      real latit                    ! latitude
      real long                     ! longitude
      real depth                    ! depth
      character*1 depind            ! depth indicator
      character*1 loind             ! locating indicator
      integer nstat                 ! number of stations used
      real rms                      ! RMS of time residuals
      real magn1                    ! magnitude no. 1
      character*1 tmagn1            ! type of magnitude no. 1
      real magn2                    ! magnitude no. 2
      character*1 tmagn2            ! type of magnitude no. 2
      real magn3                    ! magnitude no. 3
      character*1 tmagn3            ! type of magnitude no. 3
      
c-reading and checking status variable year
      read(cline (2:5),'(i4)',err=4,iostat=status) year
 4    continue    
      call check_status(nerr,status,ermsg,2,5)

c-reading and checking status variable month
      read(cline (7:8),'(i2)',err=5,iostat=status) month 
 5    continue 
      call check_status(nerr,status,ermsg,7,8)

c-reading and checking status variable day
      read(cline (9:10),'(i2)',err=6,iostat=status) day 
 6    continue 
      call check_status(nerr,status,ermsg,9,10)

c-reading and checking status variable hour
      read(cline (12:13),'(i2)',err=7,iostat=status) hour 
 7    continue
      call check_status(nerr,status,ermsg,12,13)

c-reading and checking status variable minute
      read(cline (14:15),'(i2)',err=8,iostat=status) minute 
 8    continue
      call check_status(nerr,status,ermsg,14,15)

c-reading and checking status variable secnds
      read(cline (17:20),'(f4.0)',err=9,iostat=status) secnds 
 9    continue
      call check_status(nerr,status,ermsg,17,20)

c-reading and checking status variable dind 
      read(cline (22:22),'(a1)',err=11,iostat=status) dind 
 11   continue
      call check_status(nerr,status,ermsg,22,22)

c-reading and checking status variable latit 
      read(cline (24:30),'(f7.0)',err=13,iostat=status) latit 
 13   continue
      call check_status(nerr,status,ermsg,24,30)

c-reading and checking status variable long 
      read(cline (31:38),'(f8.0)',err=14,iostat=status) long 
 14   continue
      call check_status(nerr,status,ermsg,31,38)

c-reading and checking status variable depth 
      read(cline (39:43),'(f5.0)',err=15,iostat=status) depth 
 15   continue
      call check_status(nerr,status,ermsg,39,43)

c-reading and checking status variable depind 
      read(cline (44:44),'(a1)',err=16,iostat=status) depind 
 16   continue
      call check_status(nerr,status,ermsg,44,44)

c-reading and checking status variable loind 
      read(cline (45:45),'(a1)',err=17,iostat=status) loind
 17   continue
      call check_status(nerr,status,ermsg,45,45)

c-reading and checking status variable nstat 
      read(cline (49:51),'(i3)',err=19,iostat=status) nstat 
 19   continue
      call check_status(nerr,status,ermsg,49,51)

c-reading and checking status variable rms 
      read(cline (52:55),'(f4.0)',err=20,iostat=status) rms 
 20   continue
      call check_status(nerr,status,ermsg,52,55)

c-reading and checking status variable magn1 
      read(cline (57:59),'(f3.0)',err=21,iostat=status) magn1 
 21   continue
      call check_status(nerr,status,ermsg,57,59)

c-reading and checking status variable tmagn1 
      read(cline (60:60),'(a1)',err=22,iostat=status) tmagn1 
 22   continue
      call check_status(nerr,status,ermsg,60,60)

c-reading and checking status variable magn2 
      read(cline (65:67),'(f3.0)',err=24,iostat=status) magn2 
 24   continue
      call check_status(nerr,status,ermsg,65,67)

c-reading and checking status variable tmagn2 
      read(cline (68:68),'(a1)',err=25,iostat=status) tmagn2 
 25   continue
      call check_status(nerr,status,ermsg,68,68)

c-reading and checking status variable magn3
      read(cline (73:75),'(f3.0)',err=27,iostat=status) magn3 
 27   continue
      call check_status(nerr,status,ermsg,73,75)

c-reading and checking status variable tmagn3 
      read(cline (76:76),'(a1)',err=28,iostat=status) tmagn3 
 28   continue
      call check_status(nerr,status,ermsg,76,76)

c-checking year
      if (year .le. 0 .or. year .gt. 2100) then
         call mark_error(nerr,ermsg,2,5)
      end if

c-checking month
      if (month .lt. 1 .or. month .gt. 12) then
         call mark_error(nerr,ermsg,7,8)
      end if

c-checking day finding out if month has 28, 29, 30 or 31 days
      if (month .eq. 01 .or. month .eq. 03 .or. month .eq. 05 .or.     
     1    month .eq. 07 .or. month .eq. 08 .or. month .eq. 10 .or.     
     2    month .eq. 12 .or. month .lt. 01 .or. month .gt. 12) then
         if (day .lt. 1 .or. day .gt. 31) then
            call mark_error(nerr,ermsg,9,10)
         end if
      else if (month .eq. 04 .or. month .eq. 06 .or.
     1         month .eq. 09 .or. month .eq. 11) then
         if (day .lt. 1 .or. day .gt. 30) then
            call mark_error(nerr,ermsg,9,10)
         end if
      else if (month .eq. 02) then
         left = year-int(year/4)*4
         if (left .eq. 0) then
            if (day .lt. 1 .or. day .gt. 29) then
               call mark_error(nerr,ermsg,9,10)
            end if
         else
            if (day .lt. 1 .or. day .gt. 28) then
               call mark_error(nerr,ermsg,9,10)
            end if
         end if
      end if

c-checking hour
      if (hour .lt. 00 .or. hour .gt. 23) then
         call mark_error(nerr,ermsg,12,13)
      end if

c-checking minutes
      if (minute .lt. 00 .or. minute .gt. 59) then
         call mark_error(nerr,ermsg,14,15)
      end if

c-checking seconds
      if (secnds .lt. 00 .or. secnds .gt. 60.0) then
         call mark_error(nerr,ermsg,17,20)
      end if

c-checking distance indicator
      if (dind .ne. 'L' .and. dind .ne. 'R' .and. dind .ne. 'D') then
         call mark_error(nerr,ermsg,22,22)
      end if

c-checking latitude
      if (latit .lt. -90 .or. latit .gt. 90) then
         call mark_error(nerr,ermsg,24,30)
      end if

c-checking longitude
      if (long .lt. -180 .or. long .gt. 180) then
         call mark_error(nerr,ermsg,31,38)
      end if

c-checking depth
      if (depth .lt. -9.90 .or. depth .gt. 900) then
         call mark_error(nerr,ermsg,39,43)
      end if

c_checking depth indicator
      if (depind .ne. 'F' .and. depind .ne. 'S' .and.
     1    depind .ne. ' ') then
         call mark_error(nerr,ermsg,44,44)
      end if

c_checking locating indicator
      if (loind .ne. 'F' .and. loind .ne. 'S' .and.
     1    loind .ne. ' '.and.loind.ne.'*') then
         call mark_error(nerr,ermsg,45,45)
      end if

c-checking number of stations used
      if (nstat .lt. 0) then
         call mark_error(nerr,ermsg,49,51)
      end if

c-checking rms
      if (rms .lt. 0) then
         call mark_error(nerr,ermsg,52,55)
      end if

c-checking magnitude number 1
      if (magn1 .lt. -2 .or. magn1 .gt. 10) then
         call mark_error(nerr,ermsg,57,59)
      end if

c-checking type of magnitude number 1
      if (magn1 .ne. 0) then
         if (tmagn1 .ne. 'L' .and. tmagn1 .ne. 'B' .and.
     1       tmagn1 .ne. 'S' .and. tmagn1 .ne. 'C' .and.
     2       tmagn1 .ne. 'W' .and. tmagn1 .ne. 's' .and.
     3       tmagn1 .ne. 'b' .and. tmagn1.ne.' ') then
            call mark_error(nerr,ermsg,60,60)
         end if
      end if

c-checking magnitude number 2
      if (magn2 .lt. -2 .or. magn2 .gt. 10) then
         call mark_error(nerr,ermsg,65,67)
      end if

c-checking type of magnitude number 2 
      if (magn2 .ne. 0) then
         if (tmagn2 .ne. 'L' .and. tmagn2 .ne. 'B' .and.
     1       tmagn2 .ne. 'S' .and. tmagn2 .ne. 'C' .and.
     2       tmagn2 .ne. 'W' .and. tmagn2 .ne. 's' .and.
     3       tmagn2 .ne. 'b' .and. tmagn2.ne.' ') then
            call mark_error(nerr,ermsg,68,68)
         end if
      end if

c-checking magnitude number 3
      if (magn3 .lt. -2 .or. magn3 .gt. 10) then
         call mark_error(nerr,ermsg,73,75)
      end if

c-checking type of magnitude number 3 
      if (magn3 .ne. 0) then
         if (tmagn3 .ne. 'L' .and. tmagn3 .ne. 'B' .and.
     1       tmagn3 .ne. 'S' .and. tmagn3 .ne. 'C' .and.
     2       tmagn3 .ne. 'W' .and. tmagn3 .ne. 's' .and.
     3       tmagn3 .ne. 'b' .and. tmagn3 .ne.' ') then
            call mark_error(nerr,ermsg,76,76)
         end if
      end if
      end


c-this subroutine checks all the importants variables
c-for the line type 2
      subroutine check_line_type_2(cline,ermsg,nerr)
      character*80 cline           ! dummy current line
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of error
      integer status                ! iostatus
      character*1 dicode            ! diastrophism code
      character*1 tscode            ! tsunami code
      character*1 secode            ! seiche code
      character*1 cultef            ! cultural effects
      character*1 unev              ! unusual events
      integer maxint                ! max intensity
      character*1 maxiq             ! max intensity qualifier
      character*2 intscl            ! intensity scale
      real maclat                   ! macroseismic latitude
      real maclon                   ! macroseismic longitude
      real macmag                   ! macroseismic magnitude
      character*1 tmagn             ! type of magnitude 
      real logrfa                   ! logarithm of radius of felt area
      real logar1                   ! logarithm of area 1 where eq was felt
      integer intba1                ! intensity boardering the area 1
      real logar2                   ! logarithm of area 2 where eq was felt
      integer intba2                ! intensity boardering the area 2 
      character*1 qrrep             ! quality rank of the report

c-reading and checking status variable dicode 
      read(cline (22:22),'(a1)',err=30,iostat=status) dicode 
 30   continue
      call check_status(nerr,status,ermsg,22,22)

c-reading and checking status variable tscode
      read(cline (23:23),'(a1)',err=31,iostat=status) tscode
 31   continue
      call check_status(nerr,status,ermsg,23,23)

c-reading and checking status variable secode
      read(cline (24:24),'(a1)',err=32,iostat=status) secode
 32   continue
      call check_status(nerr,status,ermsg,24,24)

c-reading and checking status variable cultef 
      read(cline (25:25),'(a1)',err=33,iostat=status) cultef 
 33   continue
      call check_status(nerr,status,ermsg,25,25)

c-reading and checking status variable unev
      read(cline (26:26),'(a1)',err=34,iostat=status) unev
 34   continue
      call check_status(nerr,status,ermsg,26,26)

c-reading and checking status variable maxint 
      read(cline (28:29),'(i2)',err=35,iostat=status) maxint 
 35   continue
      call check_status(nerr,status,ermsg,28,29)

c-reading and checking status variable maxiq
      read(cline (30:30),'(a1)',err=36,iostat=status) maxiq
 36   continue
      call check_status(nerr,status,ermsg,30,30)

c-reading and checking status variable intscl
      read(cline (31:32),'(a2)',err=37,iostat=status) intscl
 37   continue
      call check_status(nerr,status,ermsg,31,32)

c-reading and checking status variable maclat 
      read(cline (34:39),'(f6.2)',err=38,iostat=status) maclat
 38   continue
      call check_status(nerr,status,ermsg,34,39)

c-reading and checking status variable maclon 
      read(cline (41:47),'(f7.2)',err=39,iostat=status) maclon
 39   continue
      call check_status(nerr,status,ermsg,41,47)

c-reading and checking status variable macmag
      read(cline (49:51),'(f3.1)',err=40,iostat=status) macmag
 40   continue
      call check_status(nerr,status,ermsg,49,51)

c-reading and checking status variable tmagn
      read(cline (52:52),'(a1)',err=41,iostat=status) tmagn
 41   continue
      call check_status(nerr,status,ermsg,52,52)

c-reading and checking status variable logrfa
      read(cline (53:56),'(f4.2)',err=42,iostat=status) logrfa
 42   continue
      call check_status(nerr,status,ermsg,53,56)

c-reading and checking status variable logar1
      read(cline (57:61),'(f5.2)',err=43,iostat=status) logar1
 43   continue
      call check_status(nerr,status,ermsg,57,61)

c-reading and checking status variable intba1
      read(cline (62:63),'(i2)',err=44,iostat=status) intba1
 44   continue
      call check_status(nerr,status,ermsg,62,63)

c-reading and checking status variable logar2
      read(cline (64:68),'(f5.2)',err=45,iostat=status) logar2
 45   continue
      call check_status(nerr,status,ermsg,64,68)

c-reading and checking status variable intba2
      read(cline (69:70),'(i2)',err=46,iostat=status) intba2
 46   continue
      call check_status(nerr,status,ermsg,69,70)

c-reading and checking status variable qrrep
      read(cline (72:72),'(a1)',err=47,iostat=status) qrrep 
 47   continue
      call check_status(nerr,status,ermsg,72,72)


c-checking dicode
      if (dicode .ne. 'F' .and. dicode .ne. 'U' .and.
     1    dicode .ne. 'D' .and. dicode .ne. ' ') then
         call mark_error(nerr,ermsg,22,22)
      end if

c-checking tscode
      if (tscode .ne. 'T' .and. tscode .ne. 'Q' .and.
     1    tscode .ne. ' ') then
         call mark_error(nerr,ermsg,23,23)
      end if

c-checking secode
      if (secode .ne. 'S' .and. secode .ne. 'Q' .and.
     1    secode .ne. ' ') then
         call mark_error(nerr,ermsg,24,24)
      end if

c-checking cultef
      if (cultef .ne. 'C' .and. cultef .ne. 'D' .and.
     1    cultef .ne. 'F' .and. cultef .ne. 'H' .and.
     2    cultef .ne. ' ') then
         call mark_error(nerr,ermsg,25,25)
      end if

c-checking unev
      if (unev .ne. 'L' .and. unev .ne. 'G' .and.
     2    unev .ne. 'S' .and. unev .ne. 'B' .and.
     3    unev .ne. 'C' .and. unev .ne. 'V' .and.
     4    unev .ne. 'O' .and. unev .ne. 'M' .and.
     5    unev .ne. ' ') then
         call mark_error(nerr,ermsg,26,26)
      end if

c-checking maxint
      if (maxint .lt. 0 .and. maxint .gt. 12) then
         call mark_error(nerr,ermsg,28,29)
      end if

c-checking maxiq
      if (maxiq .ne. '+' .and. maxiq .ne. '-' .and.
     1    maxiq .ne. ' ') then
         call mark_error(nerr,ermsg,30,30)
      end if

c-checking intscl
      if (intscl .ne. 'MM' .and. intscl .ne. 'RF' .and.
     1    intscl .ne. 'CS' .and. intscl .ne. 'SK' .and.
     2    intscl .ne. 'EM' .and. intscl .ne. '  ') then
         call mark_error(nerr,ermsg,31,32)
      end if

c-checking maclat
      if (maclat .lt. -90 .and. maclat .gt. 90) then
         call mark_error(nerr,ermsg,34,39)
      end if

c-checking maclon
      if (maclon .lt. -180 .and. maclon .gt. 180) then
         call mark_error(nerr,ermsg,41,47)
      end if

c-checking macmag
      if (macmag .lt. -2 .and. macmag .gt. 10) then
         call mark_error(nerr,ermsg,49,51)
      end if

c-checking tmagn
      if (tmagn .ne. 'I' .and. tmagn .ne. 'A' .and.
     1    tmagn .ne. 'R' .and. tmagn .ne. '*' .and.
     2    tmagn .ne. ' ') then
         call mark_error(nerr,ermsg,52,52)
      end if

c-checking qrrep
      if (qrrep .ne. 'A' .and. qrrep .ne. 'B' .and.
     1    qrrep .ne. 'C' .and. qrrep .ne. 'D' .and.
     2    qrrep .ne. ' ') then
         call mark_error(nerr,ermsg,72,72)
      end if

      end


c-this subroutine checks all the importants variables
c-for the line type 4
      subroutine check_line_type_4(cline,ermsg,nerr)
      character*80 cline           ! dummy current line
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of error
      integer status                ! iostatus
      character*1 aux5              ! auxiliary for check empty record
      logical nempty                ! controls empty record
      character*5 stname            ! station name
      character*1 itype             ! instrument type
      character*1 compo             ! component
      integer hour                  ! hour
      integer minute                ! minutes
      real secnds                   ! seconds
      integer durat                 ! duration
      real amplit                   ! amplitude
      real period                   ! period
      real diappr                   ! direction of approach
      real phvel                    ! phase velocity
      real snrat                    ! signal to noise ratio
      integer azres                 ! azimuth residual
      real ttres                    ! travel time residual
      integer weight                ! weight
      real epdist                   ! epicentral distance
      integer azsour                ! azimuth at source 

c-reading and checking status variable stname
      read(cline (2:6),'(a5)',err=49,iostat=status) stname
 49   continue
      call check_status(nerr,status,ermsg,2,5)
      if (stname .eq. '    ') then
         nempty = .false.
         do ind=1,80
            read(cline (ind:ind),'(a1)') aux5
            if (aux5 .ne. ' ') then
                nempty = .true.
            end if
         end do
         if (.not. nempty) then 
            go to 71
         end if
      end if

c-reading and checking status variable itype 
      read(cline (7:7),'(a1)',err=50,iostat=status) itype   
 50   continue
      call check_status(nerr,status,ermsg,7,7)

c-reading and checking status variable compo
      read(cline (8:8),'(a1)',err=51,iostat=status) compo
 51   continue
      call check_status(nerr,status,ermsg,8,8)

c-reading and checking status variable hour
      read(cline (19:20),'(i2)',err=57,iostat=status) hour
 57   continue
      call check_status(nerr,status,ermsg,19,20)

c-reading and checking status variable minute
      read(cline (21:22),'(i2)',err=58,iostat=status) minute
 58   continue
      call check_status(nerr,status,ermsg,21,22)

c-reading and checking status variable secnds
      read(cline (23:28),'(f6.0)',err=59,iostat=status) secnds 
 59   continue
      call check_status(nerr,status,ermsg,23,28)

c-reading and checking status variable durat
      read(cline (30:33),'(i4)',err=60,iostat=status) durat
 60   continue
      call check_status(nerr,status,ermsg,30,33)

c-reading and checking status variable amplit
      read(cline (34:40),'(g8.1)',err=61,iostat=status) amplit
 61   continue
      call check_status(nerr,status,ermsg,34,40)

c-reading and checking status variable period 
      read(cline (42:45),'(f4.0)',err=62,iostat=status)  period
 62   continue
      call check_status(nerr,status,ermsg,42,45)

c-reading and checking status variable diappr
      read(cline (47:51),'(f5.0)',err=63,iostat=status) diappr
 63   continue
      call check_status(nerr,status,ermsg,47,51)

c-reading and checking status variable phvel
      read(cline (53:56),'(f4.0)',err=64,iostat=status) phvel
 64   continue
      call check_status(nerr,status,ermsg,53,56)

c-reading and checking status variable snrat
      read(cline (57:60),'(f4.0)',err=65,iostat=status) snrat
 65   continue
      call check_status(nerr,status,ermsg,57,60)

c-reading and checking status variable azres
      read(cline (61:63),'(i3)',err=66,iostat=status) azres
 66   continue
      call check_status(nerr,status,ermsg,61,63)

c-reading and checking status variable ttres
      read(cline (64:68),'(f5.1)',err=67,iostat=status) ttres
 67   continue
      call check_status(nerr,status,ermsg,64,68)

c-reading and checking status variable weight
      read(cline (69:70),'(i2)',err=68,iostat=status) weight
 68   continue
      call check_status(nerr,status,ermsg,69,70)

c-reading and checking status variable epdist
      read(cline (71:75),'(f5.0)',err=69,iostat=status) epdist 
 69   continue
      call check_status(nerr,status,ermsg,71,75)

c-reading and checking status variable azsour 
      read(cline (77:79),'(i3)',err=70,iostat=status) azsour
 70   continue
      call check_status(nerr,status,ermsg,77,79)


c-checking station name
      if (stname .eq. '    ') then
         call mark_error(nerr,ermsg,2,5)
      end if
c-checking itype
      if (itype .ne. 'S' .and. itype .ne. 'I' .and.
     1    itype .ne. 'L' .and. itype .ne. 'A' .and.
     2    itype .ne. 'B' .and. itype .ne. 'U' .and.
     3    itype .ne. 'H' .and. itype .ne. 'M' .and.
     4    itype .ne. 'R' .and. itype .ne. 'E' .and.
     5    itype .ne. ' ' .and. itype .ne. 'V' .and.
     6    itype .ne. 'P' .and. itype .ne. 'N') then
          call mark_error(nerr,ermsg,7,7)
      end if

c-checking compo
      if (compo .ne. 'Z' .and. compo .ne. 'N' .and.
     1    compo .ne. 'E' .and. compo .ne. ' ' .and.
     2    compo .ne. 'A' .and. compo .ne. 'B' .and.
     3    compo .ne. 'C' .and. compo .ne. '1' .and.
     4    compo .ne. '2' .and. compo .ne. '3' .and.
     5    compo .ne. 'U' .and. compo .ne. 'V' .and.
     6    compo .ne. 'W' .and. compo .ne. 'S' .and.
     7    compo .ne. 'P' .and. compo .ne. 'R' .and.
     8    compo .ne. 'T') then
         call mark_error(nerr,ermsg,8,8)
      end if

c-checking hour
      if (hour .lt. 00 .or. hour .gt. 48) then
         call mark_error(nerr,ermsg,19,20)
      end if

c-checking minutes
      if (minute .lt. 00 .or. minute .gt. 59) then
         call mark_error(nerr,ermsg,21,22)
      end if

c-checking seconds
      if (secnds .lt. 00 .or. secnds .gt.500.0 ) then
         call mark_error(nerr,ermsg,24,28)
      end if

c-checking duration
      if (durat .lt. 00) then
         call mark_error(nerr,ermsg,30,33)
      end if

c-checking period
      if (period .lt. 00) then
         call mark_error(nerr,ermsg,42,45)
      end if

c-checking phvel
      if (phvel .lt. 00) then
         call mark_error(nerr,ermsg,53,56)
      end if

 71   continue
      end


c-this subroutine checks all the importants variables
c-for the line type I
      subroutine check_line_type_I(cline,ermsg,nerr)
      character*80 cline           ! dummy current line
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of error
      integer status                ! iostatus
      character*3 lact              ! last action done
      integer year                  ! year
      character*1 aux1              ! auxiliary for date
      integer month                 ! month
      character*1 aux2              ! auxiliary for date
      integer day                   ! day
      character*1 aux3              ! auxiliary for date
      integer hour                  ! hour
      character*1 aux4              ! auxiliary for time
      integer minute                ! minutes
      character*1 id                ! identification (read 1 ch per time)
      integer idint                 ! integer variable for id
      integer ind                   ! index 


c-reading and checking status variable lact
      read(cline (9:11),'(a3)',err=72,iostat=status) lact
 72   continue
      call check_status(nerr,status,ermsg,9,11)

c-reading and checking status variable year
      read(cline (13:14),'(i2)',err=73,iostat=status) year
 73   continue
      call check_status(nerr,status,ermsg,13,14)

c-reading and checking status variable aux1
      read(cline (15:15),'(a1)',err=74,iostat=status) aux1 
 74   continue
      call check_status(nerr,status,ermsg,15,15)

c-reading and checking status variable month
      read(cline (16:17),'(i2)',err=75,iostat=status) month
 75   continue
      call check_status(nerr,status,ermsg,16,17)

c-reading and checking status variable aux2
      read(cline (18:18),'(a1)',err=76,iostat=status) aux2 
 76   continue
      call check_status(nerr,status,ermsg,18,18)

c-reading and checking status variable day
      read(cline (19:20),'(i2)',err=77,iostat=status) day
 77   continue
      call check_status(nerr,status,ermsg,19,20)

c-reading and checking status variable aux3
      read(cline (21:21),'(a1)',err=78,iostat=status) aux3 
 78   continue
      call check_status(nerr,status,ermsg,21,21)

c-reading and checking status variable hour
      read(cline (22:23),'(i2)',err=79,iostat=status) hour
 79   continue
      call check_status(nerr,status,ermsg,22,23)

c-reading and checking status variable aux4
      read(cline (24:24),'(a1)',err=80,iostat=status) aux4 
 80   continue
      call check_status(nerr,status,ermsg,24,24)

c-reading and checking status variable minute
      read(cline (25:26),'(i2)',err=81,iostat=status) minute
 81   continue
      call check_status(nerr,status,ermsg,25,26)

c-reading and checking status variable idint
      read(cline (61:69),'(i9)',err=84,iostat=status) idint 
      read(cline (70:74),'(i5)',err=84,iostat=status) idint 
 84   continue
      call check_status(nerr,status,ermsg,61,74)

c-checking lact
      if (lact .ne. 'SPL' .and. lact .ne. 'REG' .and.
     1    lact .ne. 'UPD' .and. lact .ne .'UP ' .and.
     1    lact .ne. 'NEW' .and. lact. ne. 'DUB' .and.
     !    lact .ne. 'REE' .and. lact. ne. 'HYP' .and.
     1    lact .ne. 'DUP' .and. lact. ne. 'ARG' .and.
     1    lact .ne. 'DPH' .and. lact. ne. 'HIN' .and.
     1    lact. ne. 'ARX' .and. lact. ne. 'LOC') then
         call mark_error(nerr,ermsg,9,11)
      end if

c-checking year
      if (year .lt. 0 .or. year .gt. 2100) then
         call mark_error(nerr,ermsg,13,14)
      end if

c-checking aux1
      if (aux1 .ne. '-') then
         call mark_error(nerr,ermsg,15,15)
      end if

c-checking month
      if (month .lt. 1 .or. month .gt. 12) then
         call mark_error(nerr,ermsg,16,17)
      end if

c-checking aux2
      if (aux2 .ne. '-') then
         call mark_error(nerr,ermsg,18,18)
      end if

c-checking day finding out if month has 28, 29, 30 or 31 days
      if (month .eq. 01 .or. month .eq. 03 .or. month .eq. 05 .or.
     1    month .eq. 07 .or. month .eq. 08 .or. month .eq. 10 .or.
     2    month .eq. 12 .or. month .lt. 01 .or. month .gt. 12) then
         if (day .lt. 1 .or. day .gt. 31) then
            call mark_error(nerr,ermsg,19,20)
         end if
      else if (month .eq. 04 .or. month .eq. 06 .or.
     1         month .eq. 09 .or. month .eq. 11) then
         if (day .lt. 1 .or. day .gt. 30) then
            call mark_error(nerr,ermsg,19,20)
         end if
      else if (month .eq. 02) then
         left = year-int(year/4)*4
         if (left .eq. 0) then
            if (day .lt. 1 .or. day .gt. 29) then
               call mark_error(nerr,ermsg,19,20)
            end if
         else
            if (day .lt. 1 .or. day .gt. 28) then
               call mark_error(nerr,ermsg,19,20)
            end if
         end if
      end if

c-checking aux3
      if (aux3 .ne. ' ') then
         call mark_error(nerr,ermsg,21,21)
      end if

c-checking hour
      if (hour .lt. 00 .or. hour .gt. 23) then
         call mark_error(nerr,ermsg,22,23)
      end if

c-checking aux4
      if (aux4 .ne. ':') then
         call mark_error(nerr,ermsg,24,24)
      end if

c-checking minutes
      if (minute .lt. 00 .or. minute .gt. 59) then
         call mark_error(nerr,ermsg,25,26)
      end if

c-checking ID
      do ind=61,74  
         read(cline (ind:ind),'(a1)',err=81) id 
         if (id .eq. ' ') then
            call mark_error(nerr,ermsg,61,74)
         end if
      end do

      end



c-this subroutine checks all the importants variables
c-for the line type 6
      subroutine check_line_type_6(cline,ermsg,nerr)
      character*80 cline           ! dummy current line
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of error
      integer status                ! iostatus
      character*1 free              ! free position of line 6
      character*1 first             ! first position of valid data

c-reading and checking status variable free 
      read(cline (1:1),'(a1)',err=82,iostat=status) free
 82   continue
      call check_status(nerr,status,ermsg,1,1)

c-reading and checking status variable first
      read(cline (2:2),'(a1)',err=83,iostat=status) first
 83   continue
      call check_status(nerr,status,ermsg,2,2)
      
c-checking freee
      if (free .ne. ' ') then
         call mark_error(nerr,ermsg,1,1)
      end if

c-checking first
      if (first .eq. ' ') then
         call mark_error(nerr,ermsg,2,2)
      end if
 
      end



c-this subroutine checks all the importants variables


c-for the line type F
      subroutine check_line_type_F(cline,ermsg,nerr)
      character*80 cline           ! dummy current line
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of error
      integer status                ! iostatus
      real var1, var2, var3         ! real variables
      integer var4                  ! integer variable 

c-reading and checking status variable var1
      read(cline (1:10),'(f10.1)',err=85,iostat=status) var1
 85   continue
      call check_status(nerr,status,ermsg,1,10)

c-reading and checking status variable var2
      read(cline (11:20),'(f10.1)',err=86,iostat=status) var2
 86   continue
      call check_status(nerr,status,ermsg,11,20)

c-reading and checking status variable var3
      read(cline (21:30),'(f10.1)',err=87,iostat=status) var3
 87   continue
      call check_status(nerr,status,ermsg,21,30)

c-reading and checking status variable var4
c      read(cline (31:35),'(i5)',err=88,iostat=status) var4
c 88   continue
c      call check_status(nerr,status,ermsg,31,40)

      end


c-for the line type M
c
c   to be fixed'
c
      subroutine check_line_type_M(cline,ermsg,nerr)
      character*80 cline           ! dummy current line
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of error
      integer status                ! iostatus
      real var1, var2, var3         ! real variables
      integer var4                  ! integer variable

c-reading and checking status variable var1
c     read(cline (1:10),'(f10.1)',err=85,iostat=status) var1
c85   continue
c     call check_status(nerr,status,ermsg,1,10)

c-reading and checking status variable var2
c     read(cline (11:20),'(f10.1)',err=86,iostat=status) var2
c86   continue
c     call check_status(nerr,status,ermsg,11,20)

c-reading and checking status variable var3
c     read(cline (21:30),'(f10.1)',err=87,iostat=status) var3
c87   continue
c     call check_status(nerr,status,ermsg,21,30)

c-reading and checking status variable var4
c      read(cline (31:35),'(i5)',err=88,iostat=status) var4
c 88   continue
c      call check_status(nerr,status,ermsg,31,40)

      end



c-this subroutine marks with stars the errors found and sets the
c-variable error to become able to print the error message
      subroutine mark_error(nerr,ermsg,begin,end) 
      character*1 ermsg(80)        ! dummy array for error message
      integer nerr               ! dummy for number of errors
      integer begin                 ! first position of error
      integer end                   ! last position of error
      
      nerr = nerr + 1
      do ind=begin,end
         ermsg(ind) = '*'
      end do

      end



c-this subroutine checks the I/O status and 
c-marks with stars the errors found and sets the
c-variable error to become able to print the error message
      subroutine check_status(nerr,status,ermsg,begin,end)
      integer status                ! dummy for iostatus
      character*1 ermsg(80)         ! dummy array for error message
      integer nerr                  ! dummy for number of errors
      integer begin                 ! first position of error
      integer end                   ! last position of error

      if (status .ne. 0) then
         nerr = nerr + 1
         do ind=begin,end
            ermsg(ind) = '*'
         end do
      end if
      end
