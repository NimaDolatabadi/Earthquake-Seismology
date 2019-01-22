cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  seipitsa
c
c  conversion from SEISAN binary to PITSA ASCII (input and output)
c  and conversion of response information to GSE1 format
c
c  comment june 10,99: This program from Seisan 7 is not anymore invoked
c                      from eev, since GSE is used instead, however this
c                      program might still be useful to generate one 
c                      column ASCII files
c
c  by Lars Ottemoeller Feb 1997
c changes
c   sep 21, 99 lo: modified ascii output and added ascii input,
c                  program now works in both directions
c
c
c  input arguments:
c
c      1: input filename   2: pitsa filesystem name   
c      3: output format (1=pitsa input, 2=pitsa output)  4: response (y/n)
c
c  last changes:
c
c ---------------- SEISAN 7 ----------------
c june 10, 99 lo: modify to use 5 char station code
c                 removed response output
c may 29  02  jh: ascii to seisan did not work on pc due to interal free formatted read
c                 does not work on pc, replaced by reading directly from file, assumes
c                 all data comes at end
c jan 5  12   jh: Failed for more then 1 000 000 samples di to format error in reading
c                 number of samples
c 2015-06-02 pv : add signal_int to signalx common block due to
c                 compiler warning
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      implicit none
      include  'seidim.inc'                 ! Dimensions.
      include  'libsei.inc'                 ! Library defns.
      integer  write01,                     ! Read/write unit.
     &         code                         ! Condition.
      logical   b_flag                      ! File i/o flag

      real    data (max_sample)             ! the data
      integer signal_int(max_sample)
      integer idata (max_sample)            ! integer data
      integer            iunit              ! input unit
      integer nchan,nsamp,tsamp,i
      character*80      filehead(max_trace)
      character*1040    tracehead
      character*80 infile                   ! input filename
      character*80 outfile                  ! 
c counter
      integer c,cout
      character*3 number
      character*9 filesystem,system       ! PITSA files
c traceheader info
      real srate                             ! sample rate
      integer year,month,day,hour,min        !time
      real sec
      real lat,long                          ! station location
      integer height                         ! station height
      character*5 station                    ! station name
      character*4 comp                       ! component SEISAN
      integer ind                            ! index
      character*1 resp                       ! response wanted (y/n)
      character*1 overwrite                  ! overwrite responsefile
      integer in                             ! 1 for filenr.lis
      integer filec                          ! counting files of filenr.lis
      integer indf                           ! length of filesystem
c question
      character*80 question,line
c mode 
      character*1 mode
      character*29 mainhead_text 
c-- network code
      character*5 net_code
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)

c 
      logical date_set,rate_set,tsamp_set,station_set,comp_set
c time interval
      double precision cinter(max_trace),cstart(max_trace)


c     common /signal/data,rotate,baz,rot_comp,del_rot !for seisinc
c     common /signalx/data
      common /signalx/data,signal_int

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      overwrite='n'

c get arguments
c      call get_arguments(nars,args)

  1   format(a,$) 


 
      filesystem(9:9)=' '

c      if (nars.eq.0) then
        question=' (1) Seisan binary -> Pitsa ASCII or ' // 
     &      ' (2) Pitsa ASCII -> Seisan binary'
        write(*,'(a)') question
        read(*,'(a)') mode
        if (mode.ne.'1'.and.mode.ne.'2') then
          if (mode.eq.' ') stop
          write(*,*) ' wrong choice!'
          stop
        endif
        filec=0 
 20     continue
        question=' Filename or number, filenr.lis for all'
        call filename(question,infile)

        if (infile(1:3).eq.'EOF') stop
c        if (infile(1:10).eq.'filenr.lis') then
c          write(*,*)
c     &    'PITSA filesystem name (not more than 5 characters) :  '
c        else 
c          write(*,*) 
c     &    'PITSA filesystem name (not more than 8 characters) :  '
c        endif
c        read(*,'(a8)') filesystem(1:8)
c        if (filesystem(1:1).eq.' ') filesystem(1:5)='pitsa'
        filesystem(1:6)='pitsa '
c        indf=index(filesystem,' ')-1
        indf=5
        resp = 'n'
 
      system=filesystem

      in=0
c filenr.lis
      if (infile(1:10).eq.'filenr.lis') then
        in=1
        open(60,file=infile,status='old',err=500)
      endif

 10   continue

      if (in.eq.1) then 
         read(60,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      filec=filec+1
      filesystem=system
      write(filesystem(indf+1:indf+4),'(i3)') filec

      do c=1,indf+3
         if (filesystem(c:c).eq.' ') filesystem(c:c)='0'
      enddo

      write(6,'(1x,a13,a)') ' input file: ',infile
      write(*,*) 

      if (mode.eq.'2') goto 1000

c
c seisan -> ascii
c 

c                  
c    Open input file...  
c    ------------------
c              

      chr_f_access$ = 'direct'            ! Type of file.
      f_recl$=2048                        ! Record length
      call sei open( old$,                      !JAB(BGS)Mar95. Open file.
     &               ' ',                       !JAB(BGS)Mar95. No prompt.
     &               infile,                    !JAB(BGS)Mar95. This filename.
     &               iunit,                     !JAB(BGS)Mar95. On unit.
     &               b_flag,                    !JAB(BGS)Mar95. Exists!!
     &               code )                     !JAB(BGS)Mar95. Condition (n/a).



c
c------- Read file headers ----------
       call seisinc
     * (iunit,0,nchan,0,filehead,tracehead,0.0,0.0)

c      do c=1,max_trace
c        write(*,*) filehead(c)
c      enddo

       do cout=1,nchan
         call seisinc
     *       (iunit,cout,nchan,2,filehead,tracehead,0.0,0.0)
c open file
         write(number,'(i3)') cout
         do c=1,3 
           if (number(c:c).eq.' ') number(c:c)='0'
         enddo
         ind=index(filesystem,' ')-1
         if (ind.lt.0) ind=8
         outfile = filesystem(1:ind) // '.' //number
         write01=50
         open(write01,file=outfile,status='unknown')

         write(*,*) tracehead(1:80)
         write(*,*) 'output file :  ',outfile
         if (resp.eq.'Y'.or.resp.eq.'y') then
           write(*,*) 'writing response in GSE1 format ... '
           call resp_sei2gse(tracehead,'2',overwrite)
         endif

 

c
c read and write header info
c

        read(tracehead(44:50),'(i7)') nsamp
        read(tracehead(37:43),'(f7.2)') srate
        read(tracehead(10:35),'(i3,5x,i2,1x,i2,1x,i2,1x,i2,1x,
     &     f6.3)') year,month,day,hour,min,sec
         year = year + 1900
        read(tracehead(52:75),'(f8.4,1x,f9.4,1x,i5)')
     &           lat,long,height
        station(1:5)=tracehead(1:5)
        comp=tracehead(6:9)

c
c see pitsa manual for format descr. page 5-6
c
          write(write01,'
     &       (a12,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3)')
     &         '#START_TIME ',year,month,day,hour,min,sec
          write(write01,'(a11,f7.2)') '#SAMP_FREQ ',srate
          write(write01,'(a6,i6)') '#NDAT ',nsamp
          write(write01,'(a14,a5)') '#STATION_CODE ',station
          write(write01,'(a17,a4)') '#STATION_CHANNEL ',comp

c write data
         do c=1,nsamp
           idata(c)=data(c)
           write(write01,*) idata(c)
         enddo
         
         close(write01)
      enddo
      call sei close( close$, iunit, code)

      if (in.eq.1) goto 10
      goto 20

500   continue
      goto 9999

1000  continue

c
c ascii -> Seisan
c

c
c read headers     
c

       date_set = .false.
       tsamp_set = .false.
       station_set = .false.
       comp_set = .false.
       rate_set = .false.
       nsamp=1
       open(1,file=infile,status='old')
 222   continue
       read(1,'(a)',end=333,err=333) line
       if (line(1:1).eq.'#') then

         write(*,*) line

         if (line(2:11).eq.'START_TIME') then
           call SEI GET VALUES(6, line, CODE )
            year=ARRAY$(1)
            month=ARRAY$(2)
            day=ARRAY$(3)
            hour=ARRAY$(4)
            min=ARRAY$(5)
            sec=ARRAY$(6)
            date_set = .TRUE.
        elseif (line(2:5).eq.'NDAT') then
           call SEI GET VALUES(1, line, CODE )
           tsamp=ARRAY$(1)
           tsamp_set = .true.
 
        elseif (line(2:13).eq.'STATION_CODE') then
            station=line(15:19)
            station_set = .true.
        

        elseif (line(2:16).eq.'STATION_CHANNEL') then
            comp=line(18:21)
            comp_set = .true.
         elseif (line(2:11).eq.'SAMP_FREQ') then
             call SEI GET VALUES(1, line, CODE )
             srate = ARRAY$(1)
             rate_set = .true.
         endif
         goto 222
       endif
       backspace 1
c
c   read data
c
 223   continue
       read (1,*,end=333) data(nsamp)
       idata(nsamp)=data(nsamp)
       nsamp=nsamp+1
       
       goto 223
 333   continue
       close(1)
       nsamp=nsamp-1

       write(*,*) nsamp,tsamp
       if (nsamp.ne.tsamp) then
         write(*,*) ' number of samples in header does not agree'//
     &        ' with number of samples in file !!!'
       endif

       if (.not.date_set) then
         write(6,*) 'year,month,day,hour,min,sec'
         read(5,*) year,month,day,hour,min,sec
       endif
       if (.not.rate_set) then
         write(6,*)'sample rate'
         read(5,*) srate
       endif
       if (.not.station_set) then
         write(6,*)'station and component, a5'
         read(5,'(a5)') station
       endif
       if (.not.comp_set) then
         write(6,*)'component, a4'
         read(5,'(a4)') comp
       endif
c
c calc start time and window length
c

       call timsec(year,month,day,hour,min,sec,cstart(1))

       cinter(1)=(nsamp-1)/srate
c
c  make file name and main header
c             
         nchan=1

         net_code = station(1:3)
         cbyte(1) = '4'

      call sheads(year,month,day,hour,min,sec,nchan,1,
     *                 net_code,mainhead_text,station,comp,
     *                 nsamp,srate,cbyte,
     *                 outfile,filehead,tracehead)

c         call mfhead(year,month,day,hour,min,sec,cinter,nchan,
c     *   station(1:3),station,comp,cstart,cinter,outfile,filehead)

         outfile = outfile(1:29) // '_' // comp
         do i=30,33
           if (outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo

         write(6,'(a,a)') ' Outfile name: ',outfile(1:60)
c
c  open file
c
         open(2,file=outfile,status='unknown',form='unformatted')
c                                                                               
         do i=1,12
            write(2) filehead(i)
         enddo
            write(2)tracehead                                                     
            write(6,'(1x,a70)') tracehead(1:70)                                   
c
c   check if response curve
c
c            if(tracehead(160:160).eq.'9') write(6,*)
c     *        ' No response file for this channel --------'
c  
c   write data
c
            write(2)(idata(i),i=1,nsamp)
            close(2)
c
c   back for next file
c

            if(in.eq.1) goto 10                                                           
            goto 20 

9999  continue
      close(60)

      stop
      end


