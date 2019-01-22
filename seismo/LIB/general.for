c
c    different basic seisan subroutines, most routines moved from comp_unix 
c    when gfortran was implemented on pc
c
c    jh december 2010
c
c changes:
c
c jan 31 2011 jh : add get_agency_hyp: get agency code for current station file
c feb 9  2011  jh: rplace \ by char(92)
c feb 14 2012 lo : use access append in add_fps
c apr 27 2012 jh : add openfile_unit_six,subroutine closefile_unit_six()
c feb 8  2013 jh : when check for compression, return if blank file
c mar 17 2013 jh : add routine lin_interp
c 2014-04-29 pv  : use position append in add fps, access append is not standard fortran
c 2015 112 16 jh:  add mdian and hpsort
c 2016 12 07  jh:  add number_of_events
c 2018 05 20  jh:  check if top_directory has a dir separator at end, if so remove
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine iasp91_filename(name)
c set name of iasp91 files
      implicit none
      logical pc,sun,linux
      character*(*) name
      call computer_type(sun,pc,linux)
c
      if(linux) name='IASP91_linux'
      if(pc) name='IASP91_windows'
      if(sun) name='IASP91_sun'
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine topdir(top_directory)
c
c  path-name for main directory of seisan seismic analysis system
c
      character*(*)      top_directory
      character*1 dchar 
      integer i, seiclen 

c
c   get top dir from enviromental variable
c
      call getenv('SEISAN_TOP',top_directory)
c
c   check that it is not terminated by dir char, if so remove
c
      i=seiclen(top_directory)
      call dir_char(dchar)
      if(top_directory(i:i).eq.dchar) top_directory(i:i)=' '
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine get_editor(eev_edit)
c
c   get editor name for eev
c
      implicit none
      logical pc,sun,linux
      character*(*) eev_edit
      integer sei clen

      call computer_type(sun,pc,linux)
c
c  get env variable
c
      call getenv('SEISAN_EDITOR',eev_edit)
c
c   set to default if not set
c
      if(sei clen(eev_edit).eq.0) then
         if(linux.or.sun) eev_edit='vi'
         if(pc) eev_edit='notepad'
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c get env variable SEISAN_EXTENSION
c
      subroutine get_env_seisan_extension(text)
      implicit none
      character text*(*)
      call getenv('SEISAN_EXTENSION',text)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c get operator id
c
      subroutine get_env_op(text)
      implicit none
      character*(*) text
      call getenv('SEISAN_OPERATOR',text)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    get event name if any
c
      subroutine get_env_event(event)
      implicit none
      character*(*) event
      call getenv('TRANSFER_EVENT',event)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   put event in memory
c
      subroutine put_env_event(event)
      implicit none
      character*(*) event
      character*100 text
      write(text,'(a,a)')'TRANSFER_EVENT=',event
      call putenvsun(text)   !c call
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   put base name in memory
c
      subroutine put_env_base(base)
      implicit none
      character*(*) base
      character*100 text
      write(text,'(a,a)')'TRANSFER_BASE=',base
      call putenvsun(text)   !c call
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine put_env_seistop(path)
      implicit none
      character*(*) path
      character*100 text
      write(text,'(a,a)')'SEISAN_TOP=',path
      call putenvsun(text)   !c call
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine put_env_op(op)
      implicit none
      character*(*) op
      character*100 text
      write(text,'(a,a)')'SEISAN_OPERATOR=',op 
      call putenvsun(text)   !c call
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    get base  name if any
c
      subroutine get_env_base(base)
      implicit none
      character*(*) base 
      call getenv('TRANSFER_BASE',base)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    get alternative cal directory if there 
c
      subroutine get_env_cal(local_cal)
      implicit none
      character*(*) local_cal
      local_cal=' '
      call getenv('LOCAL_CAL',local_cal)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c get SACAUX
c
      subroutine get_env_sacaux(text)
      implicit none
      character*(*) text
      text=' '
      call getenv('SACAUX',text)
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine get_def_base(def_base)
c
c   get enviromental variable def_base 
c
      character*(*) def_base 
      integer i
      call getenv('DEF_BASE',def_base)
      if (def_base(1:1).ne.' ') then
        do i=2,5
          if (def_base(i:i).eq.' ') def_base(i:i) = '_'
        enddo
      endif
      if(def_base.eq.'   '.or.ichar(def_base).eq.0) def_base='AGA__'
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    get a general string from any program
c
      subroutine get_env_string(text)
      implicit none
      character*(*) text
      text=' '
      call getenv('ANY_STRING',text)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   put a general string  to enviromental variable
c
      subroutine put_env_string(text)
      implicit none
      character*(*) text
      character*100 text1
c      integer seiclen
      text1=' '
      write(text1,'(a,a)')'ANY_STRING=',text
      call putenvsun(text1)   !c call
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_agency(agency)
c
c   get agency 
c
      character*(*) agency 
      call getenv('AGENCY',agency)
      return
      end
c
c   get Environment Architecture...
c
      integer function get_arch( chr_arch )
      character    chr_arch *(20)          ! Operating system
      call getenv('SEISARCH',chr_arch)
      chr_arch = chr_arch(:index(chr_arch,' ')-1) //
     &           char(0)                   ! Add a null.
      get_arch = 0                         ! Return a success.
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  send plot to laser
c
      subroutine send_plot(t,ilength)
      character*(*) t
      character*240 text
      integer ilength
      text=t
      call systemc('lpr '//text(1:ilength),ilength+4)
c     call systemc('lp -c '//text(1:ilength),ilength+6)
c     call system('lp -c '//text(1:ilength))
c
c due to problem on Solaris, wait for 5 secs after each plot
c
      call sleep(5)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine dir_char(dchar)
c   directory separator character
      implicit none
      logical pc,sun,linux
      character*1 dchar
      call computer_type(sun,pc,linux)
      dchar='/'
c     if(pc)dchar='\'
      if(pc)dchar=char(92)
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   input of arguments using sun calls
c
      subroutine get_arguments(nars,argument)
      implicit none
      integer nars,i,iargc
c      character*80 argument(*)
      character*(*) argument(*)
c
c   get number of arguments
c
      nars=iargc()
c
c   get actual arguments
c
      if(nars.gt.0) then
         do i=1, nars
           argument(i)=' '    ! jh oct 08
           call getarg(i,argument(i))
         enddo
      endif
c
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  clear underflow on sun to gewt rid of message at end of execution
c
      subroutine clear_underflow
c      j=ieee_flags('clear','exception','all')
c     write(6,*)'clear'
      return
      end
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      
      real function sei rand( function )
CSTART**************************************************************************
C                                                                              *
C   Supplier          : BGS/GSRG Applications Programming Unit                 *
C   System            : SEISAN                                                 *
C   Procedure Name    : SEI RAND                                               *
C   Purpose           : To set up a random sequence (seed 1) or return a       *
C                       a random number depending on function                  *
C   Arguments  -input : function (I) function to perform. Values are:          *       
C                                    OPEN$ - seed the generator                *
C                                    READ$ - read a random value               *
C   Note              : For use on SUN                                         *
C                       Function value returns random number (-1.0 -> +1.0)    *
C   Author            : J. A. Bolton                                           *
C   Date              : 4 July 1995                                            *
C   Version           : V01                                                    *
C                                                                              *
CEND****************************************************************************
c
      external     drand                   ! Random # generator (0.0->1.0)
     &            ,sei code                ! Error handler.
      real*8       drand                   ! & function.
c
c    System definitions...
c    =====================
c
      include 'libsei.inc'                 ! Library definitions.               
c
c    Arguments...
c    ============
c
      integer      function                ! Function toperform.
c
c    Local variable...
c    =================
c
      real*8       value                   ! & value.
      logical      b_flag                  ! Dummy operations flag.
c
c    Initialise...
c    =============
c
      if( function .eq. open$ ) then       ! Set up.
c      value = drand(1)                     ! Start the randomiser.
      value = 0.0d0                        ! Re-set returned value.
c            
c    Read a random number...
c    =======================
c
      else if( function .eq. read$ ) then  ! Get a random number.
      value = -1.0d0 + 2.0d0 ! *drand(0)      ! & value.
c
c    Invalid function...
c    ===================
c
      else                                ! Invalid.
      call sei code( stop$, e_init$, 0, b_flag ) ! Abort, bad initialisation.
      end if                              !
c
c     Return to Caller...
c     ===================
c
9999  sei rand = value                    ! Install the number.
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine get_env_psscale(xscale,yscale)
c
c   get scaling parameters 
c
      implicit none
c      character*80 text
      character*5 scale 
      real xscale,yscale
      integer sei clen
c
c  get SEISAN_PSSCALE_X variable
c
      call getenv('SEISAN_PSSCALE_X',scale)
c
c   set to default if not set
c
      if (sei clen(scale).eq.0) then
         xscale=0.55
      else
        read(scale,'(f5.2)') xscale
      endif

c
c  get SEISAN_PSSCALE_Y variable
c
      call getenv('SEISAN_PSSCALE_Y',scale)
c
c   set to default if not set
c
      if (sei clen(scale).eq.0) then
        yscale=1.0
      else
        read(scale,'(f5.2)') yscale
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine uncompress_file(file)
c
c check if file is compressed and uncompress
c

      implicit none

      character*(*) file      ! file name
      character*220 text     ! system call
      character*60 top_dir   ! Seisan top directory
      character*1 dchar      ! Directory deliminator
      character*20 compression_format
      integer seiclen
      integer i,j            ! counters

      call topdir(top_dir)
      call dir_char(dchar)

      compression_format = ' '

c
c check which compression format used  
c
c      write(6,*)'file from general ',file

c
c   if file blank, return
c
      if (file.eq.' ') return
c
      if (file(seiclen(file)-1:seiclen(file)).eq.'gz') then
        compression_format = 'gzip'
      elseif (file(seiclen(file)-2:seiclen(file)).eq.'zip') then
        compression_format = 'zip'
      elseif (file(seiclen(file)-1:seiclen(file)).eq.'.Z') then
        compression_format = 'compress'
      elseif (file(seiclen(file)-2:seiclen(file)).eq.'bz2') then
        compression_format = 'bzip2'
      endif

      if (compression_format.eq.' ') then
        return
      endif

c
c copy file to TMP directory
c
      text = ' '
      text = 'cp -f ' // file(1:seiclen(file)) // ' ' //
     *         top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar

      write(*,*) text(1:seiclen(text))
      call systemc(text,seiclen(text))

c
c set j to last '/'
c
      j=0
      do i=seiclen(file),1,-1
        if (file(i:i).eq.'/'.and.j.eq.0) j=i
      enddo
      j=j+1

c 
c uncompress if gzip
c   
      text = ' '
      if (compression_format.eq.'gzip') then
        text = 'gunzip -f ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-3)

c 
c uncompress if zip 
c   
      elseif (compression_format.eq.'zip') then
        text = 'unzip ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-4)
c 
c uncompress if compress
c   
      elseif (compression_format.eq.'compress') then
        text = 'uncompress ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-2)
c 
c uncompress if bzip2
c   
      elseif (compression_format.eq.'bzip2') then
        text = 'bzip2 -d ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
        write(*,*) text(1:seiclen(text))
        call systemc(text,seiclen(text))
        file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file)-4)

      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine sun_fix_file(file)
c
c add 2048 empty bytes to seisan files to solve problem with reading last record on Sun
c problem started on Sun's f95 compiler and is due the EOF in the last record
c
c 2011-06-17 pv: first version
c

      implicit none

      character*(*) file      ! file name
      character*220 text     ! system call
      character*60 top_dir   ! Seisan top directory
      character*1 dchar      ! Directory deliminator
      character*20 compression_format
      integer seiclen
      integer i,j            ! counters
c     LOGICAL :: file_exists

      write(6,*)' Fixing SEISAN waveform file in TMP'

      call topdir(top_dir)
      call dir_char(dchar)

      text = ' '
      text = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP'
c     write(*,*) text(1:seiclen(text))

c
c check if file is already in TMP
      if(text(1:seiclen(text)).NE.file(1:seiclen(text)))then
c
c copy file to TMP directory
c
      text = ' '
      text = 'cp -f ' // file(1:seiclen(file)) // ' ' //
     *         top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar

      write(*,*) text(1:seiclen(text))
      call systemc(text,seiclen(text))
      endif

c
c set j to last '/'
c
      j=0
      do i=seiclen(file),1,-1
        if (file(i:i).eq.'/'.and.j.eq.0) j=i
      enddo
      j=j+1

c
c add 2048 spaces to the file in TMP folder
c  
      text = ' '
       text = 'sei2048 >> ' // top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))
c      write(*,*) text(1:seiclen(text))
       call systemc(text,seiclen(text))
       file = top_dir(1:seiclen(top_dir)) // dchar //
     *         'TMP' // dchar //
     *         file(j:seiclen(file))

      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine add_fps(strike,dip,rake,prog,q)
c
c   adds an 'event' to file fps.out with fps so it can be plotted with foc
c
      implicit none
      real strike,dip,rake
      character*7 prog		! program used for fps
      character*1 q             ! quality
      character*80 text
      
c     open(77,file='fps.out',access='append',status='unknown')
      open(77,file='fps.out',POSITION='APPEND',status='unknown')
c
c   read to end, changed lo
c
c 1    continue
c      read(77,'(a)',end=2) text
c      goto 1
c 2    continue
c
c   write solution as an event
c
      text=' '
      text(1:22)=' 2040 0101 0101 00.0 L'
      text(80:80)='1'
      write(77,'(a)') text
      text=' '
      write(text(1:30),'(3f10.1)') strike,dip,rake
      text(71:77)=prog
      text(78:78)=q
      text(79:80)='OF'
      write(77,'(a)') text
      write(77,'(a)') ' '

      close (77)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine sort_event(data,nhead,nrecord)
c
c  sort an s-file acording to distance
c
        implicit none
        include 'seidim.inc'
        character*80 data(*),data1(max_data)
        character*5 old_dist,old_stat
        real dlt(max_data)           ! distances
        integer nhead,nrecord
        integer isort(max_data),lsort(max_data),ksort(max_data)
        integer hour,min
        real sec
        integer i,k,l,m,j


        do i=nhead+1,nrecord-1
            read(data(i)(71:75),'(f5.0)')dlt(i-nhead)
        end do

        call r4sort(nrecord-1-nhead,dlt,isort)
c
c   now sort so the distance sorted phase lines, for each group of
c   as in the original file
c
        old_dist=data(isort(1)+nhead)(71:75)
        old_stat=data(isort(1)+nhead)(2:6)
        k=0
        l=0
        do i=nhead+1,nrecord-1
           l=l+1
           read(data(isort(l)+nhead),'(18x,2i2,f6.2)') hour,min,sec
c new, lo
           if (hour.eq.0.and.min.eq.0.and.sec.eq.0.) then
c             write(*,*) ' sort by distance, time ignored '
             goto 75   ! no times
           endif
c
           if(data(isort(l)+nhead)(71:75).eq.old_dist.and.
     *     data(isort(l)+nhead)(2:6).eq.old_stat) then
              k=k+1
              lsort(k)=isort(l)
              dlt(k)=sec+min*60+hour*3600
              if(i.ne.nrecord-1) goto 73   ! at the end, always sort what is left
              l=l+1                        ! since this is last value
           endif
c
c   if here, new distance or station or last group
c
           if(k.gt.1) then
               call r4sort(k,dlt,ksort)
               m=1
               do j=l-k,l-1
                  isort(j)=lsort(ksort(m))
                  m=m+1
                                    enddo
           endif
           if(i.eq.nrecord-1) goto 73
           old_dist=data(isort(l)+nhead)(71:75)
           old_stat=data(isort(l)+nhead)(2:6)
           k=1    ! this is the first of the next group
           dlt(k)=sec+min*60+hour*3600
           lsort(k)=isort(l)
 73        continue
         enddo
c
c   save
c
 75     continue

        do i=nhead+1,nrecord-1
           data1(i)=data(isort(i-nhead)+nhead)
        end do

        do i=nhead+1,nrecord-1
           data(i)=data1(i)
        enddo

c
        return
        end


c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c          
      integer function JIAND(i1,i2)
      implicit none
      integer i1,i2
      jiand=iand(i1,i2)
      return
      end 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_agency_hyp(file_ind,agency)                            
c                                                                               
c     Routine to return agency in station file                                                      
c                                                                               
c    modified by jh  from stat_loc written by C. Lindholm May 1990                                            
c
c    updates:
c jan 2 2011 jh: fix to read form only last line

      implicit none 
      include 'libsei.inc'
      character*3 agency	
c-- station file indicator x in STATIONx.HYP
      character*1 file_ind
c-- string with data read from station file             
      character*80 text,text1	
c-- station file name
      character*80 stat_file
c-- unit and error code
      integer unit,code

      call get_seisan_def

      stat_file=' '

      stat_file(1:12)='STATION0.HYP'
      if(file_ind.ne.' ') stat_file(8:8)=file_ind ! check if alternative file
c
c  Search  file in current, then in DAT 
c
           call sei get file( open$+ignore$,   ! Check for  file.
     &                        unit,            ! Unit (n/a).
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                   stat_file )           ! For this filename.

                                                                                
      if (unit.eq.0) then
        write(*,*) ' station file not found '
        return
      endif
c                                                                               
c---- read until agency line found, just after control line                                            
c
      agency=' '
                                                                              
1     continue
      read(unit,'(a)',end=999) text
      text1=text  ! save 2. last line
      goto 1                                                               
999   continue
      agency=text1(1:3)
                                                                  
      call sei close(close$,unit,code)
      return
      end

c-----------------------------------------------------------------
c
c open and close a file on unuit 6, used with seisan_explorer to 
c redirect the console oputput to a file
c
      subroutine openfile_unit_six(filename)
      character*80 filename
      open(6,file=filename,status='unknown')
      return
      end

      subroutine closefile_unit_six()
      flush(6)
      close(6)
      return
      end

c------------------------------------------------------------
      subroutine lin_interp(x,x1,x2,y1,y2,y)
c
c   jens havskov march 2013
c
c   linear interpolation. input x is x which is in range x1 to x2
c   corresponding y range is y1 and y2
c   output interpolated y-value is y
c   id is assumed that x2 > x1 
c
      implicit none
      real x,x1,x2,y,y1,y2
      character *80 text
c
c  check
c
      if(x2.lt.x1) then
        write(6,*) ' Wrong x value in lin_interp', x1,x2
        write(6,*) ' Enter to continue'
        read(5,'(a)') text
        return
      endif  
c
c  no interpolation, same x-values
c
      if(x2.eq.x1) then
         y=y1
         return
      endif


      y=((x-x1)/(x2-x1))*(y2-y1)+y1

      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc*******************************************************
!* Given an array X of N numbers, returns their median *
!* value XMED. The array X is modified and returned    *
!* sorted in ascending order.                          *
!*******************************************************

      SUBROUTINE MDIAN(X,N,XMED)
        real X(N)
c
c   next 3 lines jh mod dec 2015
c
        if(n.eq.1) then
          xmed=x(1)
          return    ! lo 5 Jan 2016
        endif

        call hpsort(N,X)
        N2=N/2
        if (2*N2.eq.N) then
          XMED = 0.5*(X(N2)+X(N2+1))
        else
          XMED = X(N2+1)
        endif
        return
        END


!*****************************************************
!*  Sorts an array RA of length N in ascending order *
!*                by the Heapsort method             *
!* ------------------------------------------------- *
!* INPUTS:                                           *
!*          N     size of table RA                       *
!*      RA        table to be sorted                     *
!* OUTPUT:                                           *
!*          RA    table sorted in ascending order        *
!*                                                   *
!* NOTE: The Heapsort method is a N Log2 N routine,  *
!*       and can be used for very large arrays.      *
!*****************************************************         
      SUBROUTINE HPSORT(N,RA)
       real RA(N)
       L=N/2+1
       IR=N
  !The index L will be decremented from its initial value during the
  !"hiring" (heap creation) phase. Once it reaches 1, the index IR 
  !will be decremented from its initial value down to 1 during the
  !"retirement-and-promotion" (heap selection) phase.
10      continue
        if(L > 1)then
          L=L-1
          RRA=RA(L)
        else
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          if(IR.eq.1)then
            RA(1)=RRA
           return
          end if
        end if
        I=L
        J=L+L
20      if(J.le.IR)then
        if(J < IR)then
          if(RA(J) < RA(J+1))  J=J+1
        end if
        if(RRA < RA(J))then
          RA(I)=RA(J)
          I=J; J=J+J
        else
          J=IR+1
        end if
         goto 20
        end if
        RA(I)=RRA
        goto 10
      END


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine number_of_events(unit,file,n)
c
c   counts number of events (n) in sfile (file) using unit n
c
      implicit none
      include 'seidim.inc'                 ! array dimentions
      character*80 data(max_data)          ! one event
      integer nhead,nrecord,nphas,id,nstat ! for reading
      character*1 exp,type
      integer n,unit
      character*80 file


      open(unit,file=file,status='old',err=1)
      goto 2
 1    continue
      write(6,'(a,a)') 'File not there', file
      stop

 2    continue
      n=0
 3    continue
      call indata(unit,nstat,nphas,nhead,nrecord,type,exp,data,id)
      if(nrecord.eq.0) goto 4
      n=n+1
      goto 3
 4    continue
      close(unit)
      return
      end

