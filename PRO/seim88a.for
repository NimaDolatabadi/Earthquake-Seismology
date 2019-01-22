c$debug
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  smars88
c
c  conversion from SEISAN binary to SMARS88
c  and convert response information to GSE1 format
c
c  by Lars Ottemoeller Mar 1997
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c  input arguments:
c
c      1: input filename   2: mars88 filesystem name   
c      3: response (y/n)
c  last changes:
c
c   ------------- Version 7 --------------
c 01.10.99 lo: take out response option
c 
c 2015-06-02 pv  : add signal_int to signalx common block due to
c                  compiler warning
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      implicit none
      include  'seidim.inc'                 ! Dimensions.
      include  'libsei.inc'                 ! Library defns.
      external sei open,                    ! Open file handler.
     &         sei close,                   ! Close file handler.
     &         sei code,                    ! Error handler.
     &         resp_sei2gse,                ! response conversion to GSE
     &         get_arguments                ! get args
      integer  write01,                     ! Read/write unit.
     &         code                         ! Condition.
      logical   b_flag                      ! File i/o flag

      real    data (max_sample)             ! the data
      integer signal_int(max_sample)
c     integer idata (max_sample)            ! integer data
      integer            iunit              ! input unit
      integer nchan,nsamp
      character*80      filehead(max_trace)
      character*1040    tracehead
      logical pc,sun,linux                  ! computer type
      character*80 infile,question          ! input filename
      character*12 outfile                  ! 
c counter
      integer c,cout
      character*3 number
      character*9 filesystem,system       ! MARS88 files
c traceheader info
      real srate                             ! sample rate
      integer israte                         ! sample interval in milliseconds
      integer year,month,day,hour,min        !time
      real sec
      real lat,long                          ! station location
      integer height                         ! station height                   
      character*4 station                    ! station name
      character*4 comp                       ! component SEISAN
      integer ind                            ! index
      character*1 resp                       ! response wanted (y/n)
      character*1 overwrite                  ! overwrite responsefile
      integer nars                           ! number of args
      character*80 args(5)                   ! arguments
      character*240 aoutline                 ! header of ASCII out
      character*26 aouttime                  ! time in ASCII out
      integer in                             ! 1 for filenr.lis
      integer filec                          ! counting files of filenr.lis
      integer indf                           ! length of filesystem

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
      call get_arguments(nars,args)

  1   format(a,$) 

      filesystem = ' '
      filec=0 
      call computer_type(sun,pc,linux)
      if (nars.ne.0) then
        infile=args(1) 
        filesystem(1:8)=args(2)(1:8)
        resp=args(3)(1:1)
        goto 23
      endif

      write(*,*) 'Conversion SEISAN -> MARS88'
      write(*,*)

      filesystem(9:9)=' '

21    continue
      question=' Filename or number, filenr.lis for all'
      call filename(question,infile)

      if (infile(1:3).eq.'EOF') STOP
      if(infile(1:10).eq.'filenr.lis') then
        open(1,file='filenr.lis',status='old',err=22)
        in=1
      endif
      goto 23

22    continue

      write(*,*) ' filenr.lis not found'
      stop

23    continue

      filesystem(1:4)='mars'
      indf=4
      if (in.eq.1) then 
         read(1,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
         filec=filec+1
         write(filesystem(indf+1:indf+4),'(i4)') filec
         do c=1,indf+3
           if (filesystem(c:c).eq.' ') filesystem(c:c)='0'
         enddo
      endif

c        write(*,*) 'extract response in GSE1 format (y/n) ? '
c        read(*,'(a1)') resp
         resp = 'n'

29    continue
 
      system=filesystem




      write(6,'(1x,a)') infile
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

        read(tracehead(45:50),'(i6)') nsamp
        read(tracehead(37:43),'(f7.2)') srate
        read(tracehead(10:35),'(i3,5x,i2,1x,i2,1x,i2,1x,i2,1x,
     &     f6.3)') year,month,day,hour,min,sec
        read(tracehead(52:75),'(f8.4,1x,f9.4,1x,i5)')
     &           lat,long,height
        station(1:4)=tracehead(1:4)
        comp=tracehead(6:9)

        israte=int(1/srate*1000)
        write(aouttime(1:26),'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)')
     &    1900+year,'-',month,'-',day,hour,':',min,':',int(sec)
        write(aoutline,'(a,i3,a,a19,a,a)') 
     &    '# dev 0 samp ',israte
     &    ,' chno 0 scale 1 start ',aouttime(1:19),' expo 0 ',
     &    'lag -?- sync ''none'' counts: stop'
        ind=index(aoutline,' stop')
        write(write01,'(a)') aoutline(1:ind)

c#STATION_TYPE
c#STATION_COMMENT
c#STATION_LOCATION_NUMBER
c#INSTRUMENT_NUMBER
c#EVENT_COOR_1
c#EVENT_COOR_2
c#EVENT_COOR_3
c#EVENT_COOR_TYPE
c#EVENT_TIME
c#EVENT_COMMENT
c#EVENT_AZ
c#EVENT_BACK_AZ
c#EVENT_HYPO_DIST
c#EVENT_EPI_DIST
c#EVENT_TYPE
c#EVENT_LOCAL_MAG
c#EVENT_BODY_WAVE_MAG
c#EVENT_SURFACE_WAVE_MAG
c#EVENT_DURATION_MAG
c#EVENT_MOMENT_MAG


c write data
         do c=1,nsamp
c           idata(c)=data(c)

           write(write01,*) data(c)
         enddo
         
         close(write01)
      enddo
      call sei close( close$, iunit, code)

      if (in.eq.1) goto 23
      goto 21

9999  continue

      close(1)
      end


