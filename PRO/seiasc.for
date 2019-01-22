c   program to make a seisan ascii file
c
c   J. Havskov, January 1991
c
c   latest update:
c   mar xx 91 by j.h. : program goes both ways
c   oct 21 91         : output file name change, comment for vax
c   dec 13            : use 4 byte integers also
c   apr 8  92         : convert many files with filenr.lis
c   may 12            : remove ; from filename
c   nov 13 92         : fix filename length to 100 chars to routine filename
c   nov 15 93         : question to 80 chars
c   jun 7  94         : channels to 999
c   sep 22 94     mv  : bug with format for more than 30 chan
c   dec 7             : include seidim
C   dec 94            : Installed file & error handling (libsei)
c   dec 13       jh   : ************ version 5.0 *********************
CJAB(BGS)Feb95        : Crashing out...too many files open, close files
CJAB(BGS)Feb95          when finished with them, rather than all at the end
c   apr 3 95 jh       : fix start of file nale BA to also remove Ba, bA and ba
CJAB(BGS)Apr95        : Make doubly converted files uppercase.
CJAB(BGS)May95        : Overwrite PC files when converting to binary and the
CJAB(BGS)May95          the Ascii prefix 'A' is missing.eg. 95041804.C44
c oct 95 jh           : use seisinc to read
c--------------------------------------------------------------------
c sep 98 jh           : -------------- version 7.0 check ------------
c                       remove pc file name differences
c nov 5 98 jh         : remove system_c and computer_type from external dec.
c aug 11 04 jh        : do not clse files initially, seems to give
c                       problems on some linux versions
c apr 16 10 jh        : program failed to take gain constant into account
c may 5  14 jh        : remove variables from signalx
c    2015-06-02 pv   : add signal_int to signalx common block due to
c                      compiler warning
c
      implicit none
      include 'seidim.inc'
      character*80 question
c
c! SINGLE TRACE HEADER
      CHARACTER*1040    TRACEHEAD
c! MAIN HEADERS
      CHARACTER*80      MAINHEAD(max_trace)
      character*80     filein,fileout
      INTEGER           NCHAN
      INTEGER*2         DATA(max_sample*2)
      integer*4         data4(max_sample)
      INTEGER           NSAMP
      real              gain
      integer           mode,in,number
      INTEGER           I,ICHAN,k
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Library definitions & data defns.
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei clen,                  ! String length function.
     &          sei upc,                   ! Uppercase conversion.
     &          sei code                   ! Error encoder.
       integer  sei clen                   ! & function.
C
       integer  write01,                   ! Output unit 1.
     &          read01,                    ! Input unit1.
     &          read02,                    ! Ditto 2.
     &          code                       ! Local condition.
       logical  b_flag                     ! Flag end of file?.
       character*80 chr_file               ! File name
C
C    ============= end of list ==========
c-- 2 or 4 byte word
      character*1 word
c--------- for seisinc ------------------------------
      REAL X(max_sample)
      integer signal_int(max_sample)
c     real baz(max_trace)
c     character*1 rot_comp(max_trace)  ! T or R
c     real        del_rot       ! delay rotated trace
c     logical rotate
      equivalence       (data,data4)
c     common /signalx/x
      common /signalx/x,signal_int



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c    Initialise...
c    =============
c
c   which conversion
c
       write(6,*)' Seisan binary to ascii  (1)'
       write(6,*)' Seisan ascii  to binary (2)'
       read(5,*) mode
c
c   file
c
 10    continue
       question=' Filename or number, filenr.lis for all'
       call filename(question,filein)
       if(filein(1:3).eq.'EOF') goto 99       
c
c   check for one or many files
c
      in=0
         if(filein(1:10).eq.'filenr.lis') then
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'filenr.lis',     ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
         in=1
      endif
c
c  back here if many files to convert using filenr.lis
c
 11   continue
      if(in.eq.1) then
         read(read01,'(2x,i3,2x,a)') number,filein
         write(6,*)
         if(filein(1:4).eq.'    ') goto 99
      endif
      if(in.eq.0) write(6,'(1x,a)') filein
      if(mode.eq.1.and.in.eq.0) fileout='seiasc.out'
      if(mode.eq.2.and.in.eq.0) fileout='seibin.out'
      if(in.eq.1) then
         fileout=filein
         if(mode.eq.1) fileout='A'//filein
         if(mode.eq.2) then
           fileout='B'//filein
cc
           if(fileout(1:2).eq.'BA'.or.fileout(1:2).eq.'Ba'.
     *     or.fileout(1:2).eq.'ba'.or.fileout(1:2).eq.'ba') then
              i=len(fileout)
              fileout(1:i-2)=fileout(3:i)
              fileout(i-1:i)='  '
              call sei upc( fileout )             !JAB(BGS)Apr95. Uppercase.
           endif
c
c   check that there are no ;'s
c
           i=index(fileout,';')
           if(i.gt.1) then
              fileout(i:i+3)='    '
           endif
         endif
      endif
          
c  
c  open files
c
       i=index(fileout,' ')-1
c
cjh 8-04          if( read02 .gt. 0 ) then               ! Close down if open.
cjh 8-04          call sei close( close$, read02, code ) ! Stop on error.
cjh 8-04          end if                                 !
cjh 8-04          if( write01 .gt. 0 ) then              ! Close down if open.
cjh 8-04          call sei close( close$, write01,code ) ! Ditto.
cjh 8-04          end if                                 !
c
       if(mode.eq.1) then
       chr_f_form$ = 'unformatted'         ! Type of file.
       chr_f_access$='direct'
       f_recl$=2048

c          chr_f_form$ = 'unformatted'
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   filein,           ! Filename.
     &                   read02,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
c           open(read02,file=filein,access='direct',recl=2060)
       else
          chr_f_form$ = 'unformatted'
          chr_file = fileout(1:i)
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   chr_file,         ! Filename.
     &                   read02,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
       endif
c
       if(mode.eq.1) then
          chr_file = fileout(1:i)
          call sei open( unknown$,         ! Open file (default=stop on error)
     &                   ' ',              ! Prompt (n/a).
     &                   chr_file,         ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
       else
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   filein,           ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
       endif
C
C   READ MAIN HEADER 
C
      if(mode.eq.1) then
         call seisinc
     *   (read02,1,NCHAN,0,MAINHEAD,TRACEHEAD,0.0,0.0)

         do 1 i=1,12
c            READ(read02) MAINHEAD(i)
            write(write01,'(a80)')mainhead(i)
 1       continue
c         read(mainhead(1),'(30x,i3)') nchan
c
c   read remaining main header lines if more than 30
c
         if(nchan.gt.30) then
            k=(nchan-31)/3+1
            do i=13,k+12
c               read(read02)mainhead(2)
               write(write01,'(a80)')mainhead(i)
            enddo       
         endif                       
      else
         do 2 i=1,12
            read(write01,'(a80)') mainhead(i)
            write(read02) mainhead(i)
 2       continue
         read(mainhead(1),'(30x,i3)') nchan
c
c   read remaining main header lines if more than 30
c
         if(nchan.gt.30) then
            k=(nchan-31)/3+1
            do i=13,k+12
               read(write01,'(a80)')mainhead(2)
               write(read02) mainhead(2)
            enddo       
         endif                       
      endif
C
C   READ DATA  UNTIL END OF FILE
C
      DO 3 ICHAN=1,NCHAN
         write(6,*)' Channel',ichan
         if(mode.eq.1) then
           call seisinc
     *     (read02,ichan,NCHAN,1,MAINHEAD,TRACEHEAD,0.0,0.0)
c            READ(read02) TRACEHEAD
c
c   check if a gain constant is used in which case the
c   values must be divided with gain constant so numbers
c   written corresponds to gain constant.
c
            if(tracehead(76:76).eq.'G') then
               read(tracehead(148:159),'(g12.7)') gain
               READ(TRACEHEAD,'(43X,I7)') NSAMP
c              write(6,*) 'g',gain
               do i=1,nsamp
                  x(i)=x(i)/gain
               enddo
            endif

            DO 4 I=1,13	 		
               WRITE(write01,'(A80)') TRACEHEAD((i-1)*80+1:i*80)
c              write(6,*)TRACEHEAD((i-1)*80+1:i*80)
 4          continue
         else
            DO 5 I=1,13	 		
               READ(write01,'(A80)') TRACEHEAD((i-1)*80+1:i*80)
 5          continue
            write(read02) tracehead
         endif            
C
C   READ NUMBER OF SAMPLES
C
         READ(TRACEHEAD,'(43X,I7)') NSAMP
c         write(6,*)' Number of samples',nsamp
c
c   get if 2 or 4 byte integers
c
         word=tracehead(77:77)
         if(mode.eq.1) then
            if(word.eq.'4') then
c               read(read02)(data4(i),i=1,nsamp)
c               write(write01,'(7i11)')(data4(i),i=1,nsamp)
               write(write01,'(7i11)')(int(x(i)),i=1,nsamp)
            else	    					 
c               READ(read02)(DATA(I),I=1,NSAMP)
c               WRITE(write01,'(13I6)')(DATA(I),I=1,NSAMP)
                WRITE(write01,'(13I6)')(int(x(I)),I=1,NSAMP)
            endif			   
         else
            if(word.eq.'4') then
               read(write01,'(7i11)')(data4(i),i=1,nsamp)
               write(read02)(data4(i),i=1,nsamp)
            else			   			   					 
               READ(write01,'(13I6)')(DATA(I),I=1,NSAMP)
               WRITE(read02)(DATA(I),I=1,NSAMP)
            endif			     
         endif            
 3    continue
C
      write(6,'(a,1x,a)')' Output file name is:',fileout
c
c    Close down files...
c    ===================
c
       call sei close( close$, read02, code ) ! Stop on error.
       call sei close( close$, write01,code ) ! Ditto.
c
c    Next file...
c    ============
c
      if(in.eq.0) goto 10
      if(in.eq.1) goto 11
c
 99   continue
         call sei close(close$+all$,read01, code)       ! Close all open files.
      STOP
      END      


