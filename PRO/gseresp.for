cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  program: gseresp
c           response conversion: SEISAN <-> GSE2 (FAP or PAZ)
c           
c  author:  Lars Ottemoeller, Feb 97
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c  last changes:
c  feb 25 by jh: comment out external
c  mar 14    lo  corrected SEISAN PAZ output
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ------------------------- SEISAN VERSION 7.0 -------------------------
c  mar 08, 99 lo: changes for year 2000 and 5 chars station code
c  jun 10, 99 lo: use seigse.def file for component conversion
c                 for GSE2 PAZ make use of sfactor variable
c  jun 11     jh  g8 to g8.3 etc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



      program gseresp

      implicit none
c input file
      character*80 infile
c seisan response information
      character*80 respline(13)
      character*1040 respsei
c response output format
      character*1 respout
c counters 
      integer x,c
c overwrite protection
      character*1 overwrite
c input format
      character*1 formatin
c 
      character*80 question

      external resp_sei2gse,inter_p,pazresp
                

c if filenr.lis in=1 else in=0
       integer in

c      external resp_sei2gse   ! commented out by jh, pc problem

C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external computer_type,             ! Get platform type.
     &          systemc                   ! System call.
       external sei open,                  ! Open file routine.
     &          sei close                  ! Close file routine.
       integer  read01,                    ! Input unit 1.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
C
C    ============= end of list ==========


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

   1  Format(a,$)

      overwrite='n'

  10  continue


c start filenr.lis
      in=0
      write(*,*) ' Conversion of response files GSE <-> SEISAN'
      write(*,*)
      question=' Filename or number, filenr.lis for all'
      call filename(question,infile)

      if (infile(1:3).eq.'EOF') STOP
      if(infile(1:10).eq.'filenr.lis') then
         open(1,file='filenr.lis',status='old',err=9999)
         in=1
      endif

11    continue

      write(*,1) ' input format (SEISAN=1, GSE=2) ? '
      read(*,'(a1)') formatin
      write(*,1) ' output format (SEISAN=1, GSE1=2, GSE2=3) ? '
      read(*,'(a1)') respout

      if (respout.eq.formatin) then
        write(*,*) 'input format = output format'
        goto 11
      elseif (formatin.ne.'1'.and.formatin.ne.'2') then
        write(*,*) 'wrong choice !!!'
        goto 11
      elseif (respout.ne.'1'.and.respout.ne.'2'.and.respout.ne.'3')
     &    then
        write(*,*) 'wrong choice !!!'
        goto 11
      elseif (formatin.eq.'2'.and.respout.ne.'1') then
        write(*,*) 'wrong choice !!!'
        goto 11

      endif


  15  continue

      if(in.eq.1) then
         read(1,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(*,'(1x,a)') infile

c get inputfile name

  
      call sei open( old$+warn$,           ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   infile,             ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      if (code.NE.0) then
        stop    
c     else
c       call sei close( close$, read01, code ) ! Close file again
      endif


      if (formatin.eq.'1') then
c
c read SEISAN response file
c
         do c=1,13
           read(read01,'(a80)') respline(c)
         enddo

c write respline into one variable
         x=1
         do c=1,961,80
           respsei(c:c+80)= respline(x)
c          write(*,*) respline(x)
           x=x+1
         enddo
         write(*,*) respsei(1:80) 

c close input file

         call resp_sei2gse(respsei,respout,overwrite)

       elseif (formatin.eq.'2') then
       
         call resp_gse2sei(read01,overwrite)

       endif

      call sei close( close$, read01, code ) ! 

      if (in.eq.1) goto 15
      goto 10

9999  continue
      close(1)
      end



      subroutine resp_gse2sei(read01,overwrite)

      implicit none

      integer read01                             ! read unit
      character*90 gseline                       ! gse line
      integer ingse                              ! gse format
      character*5 station,scomp                  ! station name, component
      character*3 g2comp                         ! component in GSE2
      real g2calib,g1calib                       ! calibration in GSE2 / GSE1
      real srate                                 ! samplerate
      integer year,month,day,hour,min,doy        ! time
      real sec 
      character*2 g1comp                         ! component GSE1
      character*3 g1format                       ! PAZ or FAP
      character*4 g2format                       ! FAP2 or PAZ2
c      real scalib                               ! gain or normalization
      double precision scalib,sfactor            ! gain or normalization
      real poler(100),polei(100),
     &     zeror(100),zeroi(100)                 ! poles and zeros
      complex cpole(100),czero(100)
      integer npoles,nzeros                      ! numbers of PAZ
      integer x,c,n                              ! counter
      real frequ(200),amp(200),phase(200)        ! FAP
      integer nfap                               ! number of FAP triplets
      integer iphase                             ! phase as integer
      real frequencies(30)                       ! frequ values if too many PAZ
      complex clxrsp,response                    ! complex amplitude
      character*1 srespout                       ! P or ' '
      real pi
      real sgain                                 ! gain at 1Hz FAP
      real samp(30),sfrequ(30),sphase(30)        ! SEISAN FAP 
      real fr                                    ! frequency
      character*80 outfile                       ! output in SEISAN
      character*80 text(13)                      ! response in SEISAN
      real number(70)                            ! mix of poles and zeros
      character*1 overwrite                      ! overwrite protection
      character*29 mainhead_text
      character*80 file_name
      character*5 net_code


C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external computer_type              ! Get platform type.
       external sei open,                  ! Open file routine.
     &          sei close                  ! Close file routine.
       integer  write01                    ! Input unit 1.
       integer  code                       ! Local condition.
       logical  exist,b_flag               ! exist flag
C
C    ============= end of list ==========



      complex i    

      DATA PI/3.141592654/
      i=(0.0,1.0)
      nfap=0
      nzeros=0
      npoles=0
      sec=0.

      file_name='gsesei.def'
      net_code=' '

      call read_def_chan(file_name,mainhead_text,net_code)


c reading part: GSE1 or GSE2 and FAP or PAZ


100   continue

       read(read01,'(a)',end=9999) gseline
       if (gseline(1:4).ne.'CAL2'.and.gseline(1:4).ne.'CAL1')
     &       goto 100  
       write(*,*) gseline
       if (gseline(1:4).eq.'CAL2') ingse=2
       if (gseline(1:4).eq.'CAL1') ingse=1

c file in GSE2 
       if (ingse.eq.2) then
         read(gseline(6:10),'(a5)') station
         read(gseline(12:14),'(a3)') g2comp
         scomp = g2comp
         call set_def_chan(1,station,scomp)

c         call channel_gse22sei(g2comp,scomp)
         read(gseline(28:37),'(e10.2)') g2calib
c         scalib=1/g2calib*1E9                     ! scalib in counts/m
         scalib=1.0/g2calib          ! bug fix Mario  , lo
         scalib=scalib*1E9
         read(gseline(47:56),'(f10.5)') srate
         read(gseline(58:67),'(i4,1x,i2,1x,i2)')
     &        year,month,day
c         year=year-1900
         read(gseline(69:73),'(i2,1x,i2)') hour,min

c file in GSE1
       elseif (ingse.eq.1) then
         read(gseline(6:10),'(a5)') station
         read(gseline(22:23),'(a2)') g1comp
         scomp = g1comp
         call set_def_chan(1,station,scomp)

c         call channel_gse12sei(g1comp,scomp)
         read(gseline(32:34),'(a3)') g1format
         read(gseline(36:41),'(3i2)')
     &       year,month,day
         if (year.lt.50) then
            year=year+2000
         else
            year=year+1900
         endif
         read(gseline(43:46),'(2i2)') hour,min

       endif


c response in GSE1
       if (ingse.eq.1) then

         if (g1format.eq.'PAZ') then

           srespout='P'    ! PAZ

           read(read01,'(a)',end=9999) gseline
c read poles
           read(gseline(1:8),'(i8)') npoles  ! get number of poles
           do x=1,npoles
             read(read01,'(a)') gseline
             read(gseline(1:8),'(g8.3)') poler(x)
             read(gseline(9:16),'(g8.3)') polei(x)
             cpole(x)=poler(x) + I * polei(x)
c            write(*,*) poler(x),polei(x)
           enddo

           read(read01,'(a)',end=9999) gseline
c read zeros 
           read(gseline(1:8),'(i8)') nzeros  ! get number of poles
           do x=1,nzeros
             read(read01,'(a)') gseline
             read(gseline(1:8),'(g8.3)') zeror(x)
             read(gseline(9:16),'(g8.3)') zeroi(x)
             czero(x)=zeror(x) + I * zeroi(x)
c            write(*,*) zeror(x),zeroi(x)
           enddo 

c read normalization constant 
           read(read01,'(a)',end=9999) gseline
           read(gseline(1:16),'(g16.3)')  g1calib
           scalib=1/g1calib*1E9

         elseif (g1format.eq.'FAP') then 

           srespout=' '
c read FAP  
           read(read01,'(a)',end=9999) gseline
           read(gseline(1:8),'(i8)') nfap  ! get number of fap 
           do x=1,nfap
             read(read01,'(a)') gseline
             read(gseline,'(e10.5,1x,e10.5,1x,e10.4)') frequ(x),
     &         amp(x),phase(x)
             amp(x)=amp(x)*1E9
c            write(*,*) frequ(x),amp(x),phase(x)
           enddo

         endif


       elseif (ingse.eq.2) then

c read PAZ or FAP and numbers
         read(read01,'(a)',end=9999) gseline
         read(gseline(1:4),'(a4)') g2format
         if (g2format.eq.'PAZ2') then
           read(gseline(11:25),'(e15.8)') sfactor
           scalib = scalib / sfactor
           read(gseline(41:43),'(i3)') npoles
           read(gseline(45:47),'(i3)') nzeros
         elseif (g2format.eq.'FAP2') then
           read(gseline(25:27),'(i3)') nfap
         endif

c PAZ
         if (g2format.eq.'PAZ2') then
           srespout='P'   ! PAZ
           do c=1,npoles     ! read the poles
             read(read01,'(a)',end=9999) gseline
             read(gseline(2:16),'(g15.8)') poler(c) 
             read(gseline(18:32),'(g15.8)') polei(c)
             cpole(c) = poler(c) + i  * polei(c)
c            write(*,*) c,cpole(c)
           enddo
 
          do c=1,nzeros    ! read the zeros
             read(read01,'(a)',end=9999) gseline
             read(gseline(2:16),'(g15.8)') zeror(c) 
             read(gseline(18:32),'(g15.8)') zeroi(c)
             czero(c) = zeror(c) + i  * zeroi(c)
c            write(*,*) c,czero(c)
           enddo

c FAP
         elseif (g2format.eq.'FAP2') then
           srespout=' '
           do c=1,nfap
             read(read01,'(a)',end=9999) gseline
             read(gseline(2:11),'(f10.5)') frequ(c)
             read(gseline(13:27),'(e15.8)') amp(c)
             amp(c)=amp(c)/scalib
             read(gseline(29:32),'(i4)') iphase
             phase(c)=iphase
c           write(*,*) frequ(c),amp(c),phase(c)
            enddo
         endif 
       endif
      

c output part in SEISAN
c       maximum of FAP pairs is 30, if more than interpolate
c       maximum of PAZ is 37, if more than convert to FAP
c        

c set desired frequency value
      data(frequencies(x),x=1,30) / 
     &.01,.014,.019,.026,.035,.048,.065,.089,.12,.17,.23, 
     &.31,.42,.58,.79,1.1,1.5,2.,2.8,3.8,5.2,7.,9.6,13.,18.,
     &25.,34.,46.,63.,86./

      if (srespout.eq.' ') then
        if (nfap.ne.30) then
          fr=1.0
          call inter_p(nfap,fr,frequ,amp,phase,
     &       fr,response)
          sgain=cabs(response)
          do x=1,30
            call inter_p(nfap,fr,frequ,amp,phase,
     &        frequencies(x),response)
c           write(*,*) frequencies(x),response
            sfrequ(x)=frequencies(x)
            samp(x)=cabs(response)/sgain
            sphase(x)=ATAN2(aimag(response),real(response
     &        ))*(180./PI)
            write(*,*) sfrequ(x),samp(x),sphase(x)
          enddo  
        else          ! get gain at 1 Hz and write response
                    ! into sfrequ, samp, sphase
          fr=1.0
            call inter_p(nfap,fr,frequ,amp,phase,
     &        fr,response)
          sgain=cabs(response)
          do x=1,30
            samp(x)=amp(x)/sgain
            sfrequ(x)=frequ(x)
            sphase(x)=phase(x)
          enddo
        endif

      elseif (srespout.eq.'P') then

        if (npoles+nzeros.gt.37) then  ! 37
          srespout=' '  ! FAP
c get amplitude at 1 Hz
          fr=1.0
          call pazresp(fr,scalib,nzeros,czero,
     &       npoles,cpole,clxrsp)
          sgain=CABS(clxrsp)

c get FAP values using the routine pazresp
          do x=1,30
            call pazresp(frequencies(x),scalib,nzeros,czero,
     &        npoles,cpole,clxrsp)
            sfrequ(x)=frequencies(x)
            samp(x)=CABS(clxrsp) / sgain
            sphase(x)=ATAN2(aimag(clxrsp),real(clxrsp))*(180./PI)
c           write(*,*) clxrsp
            write(*,*) sfrequ(x),samp(x),sphase(x)
          enddo
        endif
      endif



c
c SEISAN OUTPUT
c

        do x=1,80
           outfile(x:x)= ' '
        enddo


c make filename and open file
c123456789012345678901234567890
cLOF__A__Z.1989-11-22-1200
c        call date_doy(doy,day,month,year)     ! get day of year
        write(outfile(1:5),'(a5)') station(1:5)
        write(outfile(6:9),'(a4)') scomp
        write(outfile(10:10),'(a1)') '.'
        write(outfile(11:14),'(i4)') year
        outfile(15:15) = '-'
        write(outfile(16:17),'(i2)') month
        outfile(18:18) = '-'
        write(outfile(19:20),'(i2)') month
        outfile(21:21) = '-'
        write(outfile(22:23),'(i2)') hour
        write(outfile(24:25),'(i2)') min

        do x=1,9
          if (outfile(x:x).eq.' ') outfile(x:x)='_'
        enddo

        do x=11,25
          if (outfile(x:x).eq.' ') outfile(x:x)='0'
        enddo
            

        write(*,*) outfile(1:25)

c clear text
      do x=1,13
        do c=1,80
          text(x)(c:c)=' '
        enddo
      enddo        

c write first line
      write(text(1)(1:5),'(a5)') station
      write(text(1)(6:9),'(a4)') scomp
      write(text(1)(10:12),'(i3)') year-1900
      write(text(1)(14:16),'(i3)') doy
      write(text(1)(18:19),'(i2)') month
      write(text(1)(21:22),'(i2)') day
      write(text(1)(24:25),'(i2)') hour
      write(text(1)(27:28),'(i2)') min 
      write(text(1)(30:35),'(f6.3)') 0.
      write(text(1)(78:78),'(a1)') srespout

     

c write FAP

      if (srespout.eq.' ') then

c no comments in second line
c third line, gain at 1Hz only
        write(text(3)(41:48),'(g8.3)') sgain
 
c write frequencies
        c=0
        do x=1,73,8
          c=c+1
          write(text(5)(x:x+7),'(g8.3)') sfrequ(c)
        enddo
        c=10
        do x=1,73,8
          c=c+1
          write(text(8)(x:x+7),'(g8.3)') sfrequ(c)
        enddo
        c=20
        do x=1,73,8
          c=c+1
          write(text(11)(x:x+7),'(g8.3)') sfrequ(c)
        enddo

c write amplitudes
        c=0
        do x=1,73,8
          c=c+1
          write(text(6)(x:x+7),'(g8.3)') samp(c)
        enddo
        c=10
        do x=1,73,8
          c=c+1
          write(text(9)(x:x+7),'(g8.3)') samp(c)
        enddo
        c=20
        do x=1,73,8
          c=c+1
          write(text(12)(x:x+7),'(g8.3)') samp(c)
        enddo

c write phase
        c=0
        do x=1,73,8
          c=c+1
          write(text(7)(x:x+7),'(f8.3)') sphase(c)
        enddo
        c=10
        do x=1,73,8
          c=c+1
          write(text(10)(x:x+7),'(f8.3)') sphase(c)
        enddo
        c=20
        do x=1,73,8
          c=c+1
          write(text(13)(x:x+7),'(f8.3)') sphase(c)
        enddo




c write PAZ

      elseif (srespout.eq.'P') then

c write 3rd line
        write(text(3)(1:22),'(1x,2i5,g11.4)') npoles,nzeros,
     &      scalib 
        write(text(3)(23:77),'(5G11.4)') 
     &    poler(1),polei(1),poler(2),polei(2),poler(3)

c write remaining poles and zeros
        do x=1,npoles-2
          number(x*2-1)=polei(x+2)
          number(x*2)=poler(x+3)
        enddo

        do x=1,nzeros
          number(x*2-1+npoles*2-2)=zeror(x) 
          number(x*2+npoles*2-2)=zeroi(x)
        enddo

        n=0
        do c=4,13
          do x=1,67,11
            n=n+1
            if (n.le.(npoles-2)*2-1+nzeros*2) then
              write(text(c)(x:x+10),'(g11.4)') number(n)
            endif
          enddo
        enddo

      endif


c open file

 150  CONTINUE

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
          write(*,*) 'new file name :  '
          read(*,'(a80)') outfile
          goto 150
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

      write(*,'(a,a)') ' output file :  ',outfile


c write to file
 
      write(*,*) text(1)
      do c=1,13
        write(write01,'(a80)') text(c)
      enddo

 200  continue



9999   continue

       call sei close( close$, write01,code ) ! Close file

       return
       end
