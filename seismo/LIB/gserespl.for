c$debug 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c subroutine resp_sei2gse
c
c convert SEISAN response (character*1040) to GSE1 or GSE2
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c last changes
c
c    14 mar 97 LO corrected PAZ SEISAN reading routine
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ----------------------- SEISAN VERSION 7.0 ---------------------------
c mar, 08 99 lo: year 2000 and 5 chars station code
c
c


       subroutine resp_sei2gse(respsei,respout,overwrite)

      implicit none
c output file
      character*80 outfile
c seisan response information
      character*1040 respsei
c seisan station name
      character*5 sstation
c seisan component
      character*4 scomp
c seisan date and time
      integer syear, smonth, sday, shour, smin
c seisan sec
      real ssec
c seisan sample rate
      real ssrate
c response format (FAP, PAZ)
      character*1 srespformat
c seisan gain
      real sgain
c seisan normalization constant
      real snorm
c seisan response information FAP
      real sfrequ(30), samp(30), sphase(30)
c seisan numbers of zeros and poles
      integer snumpoles,snumzeros
c seisan poles and zeros
      real spr(50),spi(50),szr(50),szi(50)
c array of text
      character*8 text(90)
c counter
      integer c,x,a
c response output format
      character*1 respout
c number of fap
      integer numberfap
c overwrite protection
      character*1 overwrite
c array of PAZ lines
      character*80 pazline(10)
c paz value
      real paz(74)

c gse2 component
      character*3 g2comp
c gse2 calibration id line (CAL2)
      character*90 g2idline
c gse2 system sensitivity (nm/counts)
      real g2calib
c gse2 header line
      character*80 g2header
c gse2 data triplets
      character*32 g2triplet

c gse1 line
      character*80 g1line
c gse1 component
      character*2 g1comp
c gse1 scaling factor
      real g1scf
c file name
      character*80 file_name
c network code
      character*5 net_code
c main header in waveform file (not used here)
      character*29 mainhead_text




C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external computer_type,             ! Get platform type.
     &          systemc                   ! System call.
       external sei open,                  ! Open file routine.
     &          sei close                  ! Close file routine.
       integer  write01                    ! Input unit 1.
       integer  code                       ! Local condition.
       logical  exist,b_flag               ! exist flag
   
C
C    ============= end of list ==========


      net_code = ' '
      mainhead_text = ' '
c
c   get def file for station codes, give file name
c
      file_name='gsesei.def'
      call read_def_chan(file_name,mainhead_text,net_code)

c
c check if response exists
c
      if (respsei(321:330).eq.'          '.and.
     &    respsei(161:170).eq.'          ') then
         write(*,*) 'no response information in channel header !!! ' 
         return
      endif
   1  Format(a,$)


c read header information
       sstation = respsei(1:5)
       scomp = respsei(6:9)
       read(respsei(11:12),'(i2)') syear
     
       read(respsei(18:19),'(i2)') smonth
       read(respsei(21:22),'(i2)') sday
       read(respsei(24:25),'(i2)') shour
       read(respsei(27:28),'(i2)') smin
       read(respsei(30:35),'(f6.3)') ssec 
       read(respsei(37:43),'(f7.2)') ssrate
       read(respsei(78:78),'(a1)') srespformat 
     

c read resp information, FAP or PAZ 

       if (srespformat.eq.' ') THEN

c read gain at 1Hz
         read(respsei(201:208),'(g8.3)') sgain

         c=1 
         do x=321,1033,8
           write(text(c)(1:8),'(a8)') respsei(x:x+7)
           c=c+1  
         enddo

c get FAP

         do c=1,10
           read(text(c)(1:8),'(g8.3)') sfrequ(c) 
         enddo
         do c=31,40 
           read(text(c)(1:8),'(g8.3)') sfrequ(c-20)
         enddo
         do c=61,70 
           read(text(c)(1:8),'(g8.3)') sfrequ(c-40)
         enddo

         do c=11,20
           read(text(c)(1:8),'(g8.3)') samp(c-10)
         enddo
         do c=41,50
           read(text(c)(1:8),'(g8.3)') samp(c-30)
         enddo
         do c=71,80
           read(text(c)(1:8),'(g8.3)') samp(c-50)
         enddo

         do c=21,30
           read(text(c)(1:8),'(g8.3)') sphase(c-20)
         enddo
         do c=51,60
           read(text(c)(1:8),'(g8.3)') sphase(c-40)
         enddo
         do c=81,90
           read(text(c)(1:8),'(g8.3)') sphase(c-60)
         enddo

c find number of FAP triplets
         numberfap=0
         do c=1,30
           if (sfrequ(c).gt.0) then
              numberfap=numberfap+1
           endif
         enddo
       
       elseif (srespformat.eq.'P') then

         read(respsei(161:182),'(1x,2i5,g11.4)' )
     &      snumpoles,snumzeros,snorm 

c read the first two,5 poles in pairs of re and im
         read(respsei(183:237),'(5g11.4)')
     &     paz(1),paz(2),paz(3),paz(4),paz(5)


c read the remaining poles and zeros 
         x=1
         do c=241,961,80
           read(respsei(c:c+80),'(a80)') pazline(x)
           x=x+1
         enddo  
 
         a=5

         do x=1,10
           do c=1,67,11
             a=a+1  
             read(pazline(x)(c:c+11),'(g11.4)') paz(a)
           enddo
         enddo

c read real part of poles
         do x=1,snumpoles
           spr(x)=paz(x*2-1)
         enddo

c read imaginary part of poles
         do x=1,snumpoles 
           spi(x)=paz(x*2)
         enddo

c read real part of zeros
         do x=1,snumzeros
           szr(x)=paz(snumpoles*2+x*2-1)
         enddo
         
c read imaginary part of zeros
         do x=1,snumzeros
           szi(x)=paz(snumpoles*2+x*2)
         enddo

       endif  

c end of input part



c make GSE filename same on PC and SUN
       if (respout.eq.'3') then
c         call channel2gse2(scomp,g2comp)  ! convert comp to GSE2
         call set_def_chan(1,sstation,scomp)
         g2comp = scomp(1:3)
         outfile = sstation // g2comp // '2.CAL'
       elseif (respout.eq.'2') then
c        call channel2gse1(scomp,g1comp)  ! convert comp to GSE1
         call set_def_chan(1,sstation,scomp)
         g1comp = scomp(1:2)
         outfile = sstation // g1comp // '_1.CAL'
       endif

c remove blanks
       do c=1,12
          if (outfile(c:c).eq.' ') outfile(c:c)='_'
       enddo

c open file
 100  CONTINUE

      call sei open( check$,               ! check if file exists 
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   exist,            ! Flag.
     &                   code )            ! Returned condition.

      if (exist) then
        if (overwrite.ne.'a'.and.overwrite.ne.'A') then
          write(*,*) outfile,' exists !!!'
          write(*,*) 'overwrite (y/n/a=always) ?  '
          read(*,'(a1)') overwrite
        endif
        if (overwrite.eq.'n'.or.overwrite.eq.'N') then
          write(*,*) ' new file name :  ' 
          read(*,'(a80)') outfile
          goto 100
        else
          call sei open( old$,           ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,            ! Flag.
     &                   code )            ! Returned condition.
          
        endif
      else
        call sei open( new$,           ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,            ! Flag.
     &                   code )            ! Returned condition.

        
      endif

      write(*,*) ' response output file :  ',outfile

c output part GSE2

       if (respout.eq.'3') THEN

c fill character strings with blanks

         do c=1,90
            g2idline(c:c)=' '
         enddo

         do c=1,80
           g2header(c:c)=' '
         enddo



c write ID line
         g2idline(1:4)='CAL2'      
         g2idline(6:10)=sstation(1:5)
         g2idline(12:14)=g2comp
         if (srespformat.eq.' ') then 
           if (sgain.ne.0) then
             g2calib=1/(sgain*1E-9)
           else
             g2calib=1
           endif
           write(g2idline(28:37),'(e10.2)') g2calib
         elseif (srespformat.eq.'P') then
           g2calib=1/(snorm*1E-9)
           write(g2idline(28:37),'(e10.2)') g2calib
         endif
         
       
         g2idline(39:45)='   1.0' 
         if (ssrate.ne.0) write(g2idline(47:56),'(f10.5)') ssrate

c if response from waveform header, dont use starttime
         if (ssec.ne.0) then
           write(g2idline(58:67),'(i4,a1,i2,a1,i2)') 
     &       1950,'/',01,'/',01   ! time fixed
           write(g2idline(69:73),'(i2,a1,i2)') 00,':',00 ! shour, smin
         else
           write(g2idline(58:67),'(i4,a1,i2,a1,i2)')
     &          syear+1900,'/',smonth,'/',sday
           write(g2idline(69:73),'(i2,a1,i2)') shour,':',smin
         endif

  
         write(write01,'(a90)') g2idline(1:90)
         write(*,*) g2idline

         
c write header line and response data

         if (srespformat.eq.' ') THEN
           g2header(1:4)='FAP2'
           g2header(9:9)='C'
           write(g2header(25:27),'(i3)') numberfap

           write(write01,'(a)') g2header
           write(*,'(a53)') g2header(1:53)

c write data triplets
           do c=1,numberfap
             do x=1,32
               g2triplet(x:x)=' '
             enddo
             write(g2triplet(2:11),'(f10.5)') sfrequ(c)
             write(g2triplet(13:27),'(e15.8)') samp(c)
             write(g2triplet(29:32),'(i4)') int(sphase(c))
           
             write(write01,'(a32)') g2triplet
c            write(*,*) g2triplet

           enddo

         elseif (srespformat.eq.'P') THEN
c write header
           g2header(1:4)='PAZ2'
           g2header(9:9)='C'
           write(g2header(41:43),'(i3)') snumpoles
           write(g2header(45:47),'(i3)') snumzeros

           write(write01,'(a)') g2header
           write(*,*) g2header

c write data triplets
c first the poles
           do c=1,snumpoles
             do x=1,32
               g2triplet(x:x)=' '
             enddo
             write(g2triplet(2:16),'(e15.8)') spr(c) 
             write(g2triplet(18:32),'(e15.8)') spi(c)
          
             write(write01,'(a)') g2triplet
c            write(*,*) g2triplet

           enddo

c now the zeros
           do c=1,snumzeros
             do x=1,32
               g2triplet(x:x)=' '
             enddo
             write(g2triplet(2:16),'(e15.8)') szr(c) 
             write(g2triplet(18:32),'(e15.8)') szi(c)
          
             write(write01,'(a)') g2triplet
c            write(*,*) g2triplet

           enddo



         endif 


c write GSE1 output

       elseif (respout.eq.'2') then


c fill character strings with blanks

         do c=1,80
            g1line(c:c)=' '
         enddo


c write ID line
         g1line(1:4)='CAL1'
         g1line(6:10)=sstation(1:5)
         g1line(22:23)=g1comp
         if (srespformat.eq.' ') then
           g1line(32:34)='FAP'
         elseif (srespformat.eq.'P') then
           g1line(32:34)='PAZ'
         endif
         if (ssec.ne.0) then   ! if response from waveform file
           write(g1line(36:41),'(3i2)')
     &       50,01,01    ! time fixed
           write(g1line(43:46),'(2i2)') 0,0   !shour,smin
         else
           write(g1line(36:41),'(3i2)')
     &       syear,smonth,sday
           write(g1line(43:46),'(2i2)') shour,smin
         endif
 
         write(write01,'(a80)') g1line(1:80)
         write(*,*) g1line

c write header line and response data


         if (srespformat.eq.' ') THEN   ! response as FAP
 
           do c=1,80
             g1line(c:c)=' '
           enddo

           write(g1line(1:8),'(i8)') numberfap

           write(write01,'(a)') g1line
c          write(*,'(a)') g1line


c write data triplets
           do c=1,numberfap
             do x=1,80
               g1line(x:x)=' '
             enddo

             write(g1line,'(e10.5,1x,e10.5,1x,f10.4)') sfrequ(c),
     &         samp(c)*1E9/sgain, sphase(c)


             write(write01,'(a)') g1line
c            write(*,*) g1line

           enddo

         elseif (srespformat.eq.'P') THEN   ! PAZ

c write poles
 
           do x=1,80
             g1line(x:x)=' '
           enddo

           write(g1line(1:8),'(i8)') snumpoles

           write(write01,'(a)') g1line
c          write(*,*) g1line

c write real and imaginary part
           do c=1,snumpoles
             do x=1,80
               g1line(x:x)=' '
             enddo
             write(g1line(1:8),'(e8.2)') spr(c)
             write(g1line(9:16),'(e8.2)') spi(c)
             write(write01,'(a)') g1line
c            write(*,*) g1line

           enddo


c write number of zeros
           do x=1,80
             g1line(x:x)=' '
           enddo

           write(g1line(1:8),'(i8)') snumzeros

           write(write01,'(a)') g1line
c          write(*,*) g1line


c write real part
           do c=1,snumzeros
             do x=1,80
               g1line(x:x)=' '
             enddo
             write(g1line(1:8),'(e8.2)') szr(c)
             write(g1line(9:16),'(e8.2)') szi(c)

             write(write01,'(a)') g1line
c            write(*,*) g1line

           enddo


C write scale factor to normalize to grounddisplacement
             do x=1,80
               g1line(x:x)=' '
             enddo
 
             g1scf=1/(snorm*1E-9)
             write(g1line(1:20),'(e16.10)') g1scf
             write(write01,'(a)') g1line


         endif
  

       endif


       call sei close( close$, write01,code ) ! Close file 

       return 
 
       end



ccccccc end of main program          ccccccccccccccccc



cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c convert component from SEISAN to FDSN format used
c in GSE2
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

       subroutine old_channel2gse2(inch,outch)

       implicit none
 
       character*4 inch
       character*3 outch

       outch='   '

c convert channel codes
       if (inch.eq.'S  Z') outch='SHZ'
       if (inch.eq.'S  N') outch='SHN'
       if (inch.eq.'S  E') outch='SHE'
       if (inch.eq.'L  Z') outch='LHZ'
       if (inch.eq.'L  N') outch='LHN'
       if (inch.eq.'L  E') outch='LHE'
       if (inch.eq.'B  Z') outch='BHZ'
       if (inch.eq.'B  N') outch='BHN'
       if (inch.eq.'B  E') outch='BHE'
       if (inch.eq.'BV Z') outch='BHZ'
       if (inch.eq.'BV N') outch='BHN'
       if (inch.eq.'BV E') outch='BHE'
       if (inch.eq.'V  Z') outch='VHZ'
       if (inch.eq.'V  N') outch='VHN'
       if (inch.eq.'V  E') outch='VHE'
       if (inch.eq.'A  Z') outch='SAZ'
       if (inch.eq.'A  N') outch='SAN'
       if (inch.eq.'A  E') outch='SAE'


       return
       end      ! end of subroutine channel2gse



cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c convert component from SEISAN to GSE1 format 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine old_channel2gse1(inch,outch)

       implicit none

       character*4 inch
       character*2 outch

       outch='  '

c convert channel codes
       if (inch.eq.'S  Z') outch='SZ'
       if (inch.eq.'S  N') outch='SN'
       if (inch.eq.'S  E') outch='SE'
       if (inch.eq.'L  Z') outch='LZ'
       if (inch.eq.'L  N') outch='LN'
       if (inch.eq.'L  E') outch='LE'
       if (inch.eq.'B  Z') outch='BZ'
       if (inch.eq.'B  N') outch='BN'
       if (inch.eq.'B  E') outch='BE'
       if (inch.eq.'BV Z') outch='BZ'
       if (inch.eq.'BV N') outch='BN'
       if (inch.eq.'BV E') outch='BE'
       if (inch.eq.'V  Z') outch='VZ'
       if (inch.eq.'V  N') outch='VN'
       if (inch.eq.'V  E') outch='VE'
       if (inch.eq.'A  Z') outch='AZ'
       if (inch.eq.'A  N') outch='AN'
       if (inch.eq.'A  E') outch='AE'


       return
       end      ! end of subroutine channel2gse


cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c convert component from GSE1 to SEISAN format
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine old_channel_gse12sei(inch,outch)

       implicit none

       character*2 inch
       character*4 outch

       outch='    '

c convert channel codes
       if (inch.eq.'SZ') outch='S  Z'
       if (inch.eq.'SN') outch='S  N'
       if (inch.eq.'SE') outch='S  E'
       if (inch.eq.'LZ') outch='L  Z'
       if (inch.eq.'LN') outch='L  N'
       if (inch.eq.'LE') outch='L  E'
       if (inch.eq.'BZ') outch='B  Z'
       if (inch.eq.'BN') outch='B  N'
       if (inch.eq.'BE') outch='B  E'
       if (inch.eq.'VZ') outch='V  Z'
       if (inch.eq.'VN') outch='V  N'
       if (inch.eq.'VE') outch='V  E'
       if (inch.eq.'AZ') outch='A  Z'
       if (inch.eq.'AN') outch='A  N'
       if (inch.eq.'AE') outch='A  E'


       return
       end      ! end of subroutine 


cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c convert component from FDSN to SEISAN format 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

       subroutine old_channel_gse22sei(inch,outch)

       implicit none

       character*3 inch
       character*4 outch

       outch(2:4)=inch

c convert channel codes
       if (inch.eq.'SHZ') outch='S  Z'
       if (inch.eq.'SHN') outch='S  N'
       if (inch.eq.'SHE') outch='S  E'
       if (inch.eq.'LHZ') outch='L  Z'
       if (inch.eq.'LHN') outch='L  N'
       if (inch.eq.'LHE') outch='L  E'
       if (inch.eq.'BHZ') outch='B  Z'
       if (inch.eq.'BHN') outch='B  N'
       if (inch.eq.'BHE') outch='B  E'
       if (inch.eq.'VHZ') outch='V  Z'
       if (inch.eq.'VHN') outch='V  N'
       if (inch.eq.'VHE') outch='V  E'
       if (inch.eq.'SAZ') outch='A  Z'
       if (inch.eq.'SAN') outch='A  N'
       if (inch.eq.'SAE') outch='A  E'

       return
       end      ! end of subroutine 

