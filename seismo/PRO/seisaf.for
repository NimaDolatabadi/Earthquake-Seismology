c
c  program to convert SEISAN files to Sesame Ascii files SAF
c  jh, april 2003
c
c  the 3 first channels in seisan file is read, no check if from same station
c  or order z,n,e
c  it is assumed that all 3 have the same start time, number of samples
c      and sample rate, these values are taken from the first trace
c
c   format description at end of program
c

      implicit none
      include 'seidim.inc'                    ! for giving dimensions
      include 'libsei.inc'                    ! for all seisan subroutines
      include 'waveform.inc'                  ! waveform data
      include 'seisan.inc'                    ! seisan general parameters
      character*80 question                   ! question
      character*80 text                       ! general text string
      integer k,i,in
      external seiclen                        ! lenght of string
      integer seiclen


c
c   get input file name
c
      in=0
      question=' File name, #, ? or filenr.lis for all'
      call filename(question,wav_filename(1))	    
      if(wav_filename(1).eq.'EOF') stop
      if(wav_filename(1)(1:10).eq.'filenr.lis'.or.
     *wav_filename(1)(1:10).eq.'FILENR.LIS') then
         open(8,file='filenr.lis',status='old',err=20)
         goto 21
 20      continue
         write(6,*)' No filenr.lis'
         stop
 21      continue
         in=1
      endif
c
c   file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)') wav_filename(1)
         if(wav_filename(1)(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') wav_filename(1)
c
c  open file 
c  
      wav_nfiles=1                   ! there is only one file in this case
c
c   read all headers of file 
c
      call read_wav_header(1)
c
c   output possible errors
c
       if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     * wav_error_message
c
c   write out the format, can be seisan or GSE currently
c
       write(6,'(1x,a,a)') 'Format ',wav_file_format(1)

c
      write(6,*)' Total number of channels available:',wav_nchan
c
c   terminate if no channels
c
      if(wav_nchan.eq.0) stop
c
c   write some input info for each channnel on screen
c
      write(6,'(a,a)')'CHA STAT  COMP  YEAR MO DA HR MI    SEC NSAMP ',
     *                '    RATE    DELAY DURATION'
      do i=1,wav_nchan
         write(6,'(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i6,3f9.3)')
     *   i, wav_stat(i),wav_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i)
      enddo

c
c   open output file, same as input with SAF at end
c
      open(2,file=wav_filename(1)(1:seiclen(wav_filename))
     *//'_SAF',status='unknown')
      write(6,'(a,a)') ' Output file name: ',
     *wav_filename(1)(1:seiclen(wav_filename))//'_SAF'
     
c
c   write headers
c

c
c   main header
c
      write(2,'(a,a)') 
     *'SESAME ASCII data format (saf) v. 1   ',
     *'(this line must not  be modified)'
c
c  info
c
      write(2,'(a)') 
     *'# Data converted from SEISAN with program SAFSEI'
      write(2,'(a,a)') '# Original file name: ',
     *wav_filename(1)(1:seiclen(wav_filename))
c
c
c  station code, only use code for first channel
c
      write(2,'(a,a)')'STA_CODE = ',wav_stat(1)
c
c   sampLE rate, assume same all
c
      write(2,'(a,f8.3)') 'SAMP_FREQ = ', wav_rate(1)

C
C   number of samples
c
      write(2,'(a,i8)')'NDAT = ', wav_nsamp(1)
c
c   start date and time
c
      write(2,'(a,i5,4i3,f8.3)') 'START_TIME = ', wav_year(1),
     *wav_month(1),wav_day(1),wav_hour(1),wav_min(1),wav_sec(1)
c
c   units
c
      write(2,'(a)')'UNITS = count'
c
c   componenets, GSE style
c
      
      write(2,'(a,a,1x,a)') 
     *'CH0_ID = ',wav_comp(1)(1:1),WAV_comp(1)(4:4) 
      write(2,'(a,a,1x,a)') 
     *'CH1_ID = ',wav_comp(2)(1:1),WAV_comp(2)(4:4) 
      write(2,'(a,a,1x,a)') 
     *'CH2_ID = ',wav_comp(3)(1:1),WAV_comp(3)(4:4) 

c
c   orientation
c
c      write(2,'(a,a)') 'NORTH_ROT = ', '  '
c
c   start of digitial data
c
      write(2,'(a)') 
     *'####-------------------------------------------'
 
c
c
c
c--------------------------------------------------------------------------
c  Data from first 3 channels, it is assumed that they are z,n,e
c  but any 3 channels will be read
c--------------------------------------------------------------------------
c

c
c   read one channel at the time
c
      call wav_read_channel(3)
      do i=1,wav_nsamp(3)
         signal3(i)=signal1(i)
      enddo 
      call wav_read_channel(2)
      do i=1,wav_nsamp(2)
         signal2(i)=signal1(i)
      enddo 
      call wav_read_channel(1)

c
c   write samples, assume number of samples are the same for all 3 as the first
c   channel, write as reals
c
      do i=1,wav_nsamp(1)
          write(2,'(3f13.3)')
     *    signal1(i),signal2(i),signal3(i)
      enddo
c
c  go for next file
c
      close(2)
      if(in.eq.1) goto 1000


      stop
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
c   this routine not used, just added in case program should read the other way
c   in the future

        subroutine rsaf(fname,ndatma,iopt,datath,npts,sfre,ier)
c
c Read Sesame Ascii Format data
c
c a.tento 09/02 v. 2.0
c
c ARGUMENTS:
c -- input --
c fname   :  input file name 
c ndatma  :  row dimension of matrix "datath" as specified in the
c            dimension statement of the calling program
c iopt    :  option = 0 read header and data time histories
c                   = 1 read only header
c -- output -- 
c datath(ndatma,3) : time histories for 3 channels, must be dimensioned
c                     in the calling program
c npts    :  number of points of time histories
c sfre    :  sampling frequency in Hz
c ier     :  error code = 0 : no error
c          = 2 : or 3  problem in getting an unused fortran file unit
c          = 4 : problem in opening the file (e.g. permission denied or
c                                      the file does not already exist)
c          = 7 : the mandatory parameters have NOT been completely set
c          = 8 : number of time history points greater than "ndatma"
c          = 9 : error in reading time histories data
c          = 50 : the opened file is NOT a current version SAF file
c          > 1000 : problem header reading line : ier - 1000
c
c
        implicit none
c
c....... ARGUMENTS
        character  fname*(*)
        integer    ndatma, iopt, npts, ier
        real       datath, sfre
        dimension  datath(ndatma,3)
c
c....... IDENTIFIERS SAF v. 1
        integer   iver, ibeg
        parameter ( iver = 36, ibeg = 24)
        character version*(iver),  begdat*(ibeg), comment*1, sep*1
        parameter ( version = 'SESAME ASCII data format (saf) v. 1 ' )
        parameter ( begdat = '####--------------------' ) 
        parameter ( comment = '#' )
        parameter ( sep = '=' )
c
c....... /SAFHDR8/ 
        integer    length
        parameter ( length = 100 )
        integer    year,  month,  day,  hour,  minute, isec, msec, ndat
        real       second, samp_freq, north_rot 
        character*(length) sta_code, ch0_id, ch1_id, ch2_id, units
        common /safhdr8/  year,  month,  day,  hour,  minute, isec, 
     #         msec,  ndat, second, samp_freq, north_rot, sta_code, 
     #         ch0_id, ch1_id, ch2_id, units
c
c....... UNDEFINED 
        integer      iundef
        real         rundef
        character*8  cundef
        parameter ( iundef = -12345)
        parameter ( rundef = -12345.0e+00 )
        parameter ( cundef = '-12345')
c
c
c....... LOCAL VARIABLES
        integer      mheader, mmand
        parameter    ( mheader = 9, mmand = 8 )
        character*(length) header(mheader), row, argout(2),
     #                     headcod(mheader)
        data headcod / 'STA_CODE','SAMP_FREQ','NDAT','START_TIME',
     #                 'UNITS', 'CH0_ID', 'CH1_ID', 'CH2_ID',
     #                 'NORTH_ROT' /
c
        integer large
        parameter (large=2147483600)
c
        logical      free
        integer      i, nunit, j, ind, k
        integer      numax, numin
        parameter    ( numax=50, numin=31 )
c
c ....... first executable statement
c
        ier = 0
c
c ....... header initialization
c
        do 1 i = 1, mheader
        header(i) = cundef
1       continue
        year      =  iundef
        month     =  iundef
        day       =  iundef
        hour      =  iundef
        minute    =  iundef
        isec      =  iundef
        msec      =  iundef
        ndat      =  iundef
        second    =  rundef
        samp_freq =  rundef
        north_rot =  rundef
        sta_code  =  cundef 
        ch0_id    =  cundef 
        ch1_id    =  cundef 
        ch2_id    =  cundef 
        units     =  cundef
        sfre = samp_freq
        npts = ndat
c
c ....... get a currently unused fortran file unit
c
        do 4 i = numin, numax
        nunit = i
        inquire(unit=nunit,err=302,opened=free)
        if (.not.free) goto 5
4       continue
c
        goto 303
c
5       continue
c
c ....... open file and read first line
c
        open(nunit,file=fname,form='formatted',access='sequential',
     #  status='old', err=304) 
c
        k = 1
        read(nunit,100,err=305)row
        if ( row(1:iver)  .ne.  version ) goto 350
c
        do 6 i = 1, large
        k = i + 1
        read(nunit,100,err=305)row
        if ( row(1:ibeg)  .eq.  begdat ) goto 7
        if ( row(1:1)  .eq.  comment ) goto 6
        if ( row       .eq.  ' '     ) goto 6
        call spltct8(row,sep,argout,length)  
        call toups8(argout(1))
        do 8 j = 1, mheader
        ind = j
        if ( headcod(j)  .eq.  argout(1) ) goto 9
8       continue
        goto 306
9       continue
c
        header(ind)=argout(2)
c
6       continue
c
7       continue
c
        do 2 i = 1, mmand
        if ( header(i) .eq. cundef ) goto 307
2       continue
c
        sta_code = header(1)
        read(header(2),*)samp_freq
        sfre = samp_freq
        read(header(3),*)ndat
        npts = ndat
        read(header(4),*) year,  month,  day,  hour,  minute, second
        isec = int(second)
        msec = int((second - float(isec))*1000.0 + 0.5)
        units = header(5)
        ch0_id = header(6)
        ch1_id = header(7)
        ch2_id = header(8)
        read(header(9),*)north_rot
c
        if ( iopt  .eq.  1 ) goto 99
c
        if ( npts  .gt.  ndatma ) goto 308 
        do 10 i = 1, npts
        read(nunit,*,err=309)(datath(i,j),j=1,3)
10      continue
c
c
99      continue
        close(nunit)
        return
c
c ........ inquire error
302     ier = 2
        return
c
c ........ no units free
303     ier = 3
        return
c
c ........ cannot open
304     ier = 4
        return
c
c----------------------------------------------------------------------
c ........ cannot read at line : ier - 1000
305     ier = 1000 + k
        close(nunit)
        return
c
c ........ uncorrect key word found at line : ier - 1000
306     ier = 1000 + k
        close(nunit)
        return
c
c ........ problem in reading parameter values at line : ier - 1000
311     ier = 1000 + k
        close(nunit)
        return
c----------------------------------------------------------------------
c
c ........ not all the mandatory parameters are  set
307     ier =  7
        close(nunit)
        return
c
c ........ npts > ndatma
308     ier = 8
        close(nunit)
        return
c
c ........ error in reading time histories data
309     ier = 9
        close(nunit)
        return
c
c ........ the opened file is NOT a current version SAF file
350     ier = 50
        close(nunit)
        return
c
c
100     format(a100)
c
c
c
        end
        subroutine toups8(string)
c
c convert "string" to upper case 
c
c a.tento 05/02 v.1.0
c
c ARGUMENTS
c string  : 
c           -- input --
c           character variable to be converted in upper case
c           -- output --
c           upper case version of input string
c
c
c....... ARGUMENTS
        character  string*(*)
c
c....... LOCAL VARIABLES
        integer i, j, k, m
        character*1 upper(26), lower(26) 
        data upper / 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
     #               'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
     #               'U', 'V', 'W', 'X', 'Y', 'Z' /
        data lower / 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
     #               'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 
     #               'u', 'v', 'w', 'x', 'y', 'z' /
c
c ....... first executable statement
c
        m = len(string)
c
        do 1 i = 1, m
        do 2 j = 1, 26
        k = j
        if ( string(i:i)  .eq.  lower(j) ) goto 3
2       continue
        goto 1
3       string(i:i) = upper(k)
1       continue
c
        return
c
        end
        
        subroutine spltct8(row,sep,argout,length)
c
c Split a character string ("row*(length)") in two parts 
c (i.e. keyword and its argument) and put them in  "argout*(length)".
c Separator is specified by  "sep" (character*1).
c Initial blank are removed.
c
c a.tento 5/02 v.1.0
c
c ARGUMENTS:
c -- input --
c row    :  character*length string to be split              [c]
c sep    :  character*1 separator                     [c]
c -- output --
c argout :  character*length vector(2) of parts found in "row" [c]
c length :  length of character string 
c
c
c....... ARGUMENTS
        integer length
c_g77__       character row*(length), sep*1, argout(2)*(length)
        character row*(*), sep*1, argout(2)*(*)
c
c....... LOCAL VARIABLES
        character cblank*1
        parameter (cblank = ' ' )
        integer k, j, i
c
c
        do 10 i = 1, length
        argout(1)(i:i) = cblank
        argout(2)(i:i) = cblank
10      continue
c
        k = 0
        j = 0 
        do 1 i = 1, length
        k = i
        if( row(i:i) .eq. cblank ) goto 1
        if( row(i:i) .eq. sep ) goto 2
        j = j + 1
        argout(1)(j:j) = row(i:i)
1       continue
        return
c
2       continue
        do 3 i = k+1, length
        j = i
        if( row(i:i) .eq. cblank ) goto 3
        goto 4
3       continue
c
4       continue
        argout(2) = row(j:length)
c
        return
c
        end



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c>====================================================================
c>The SESAME ASCII file is organized in 3-columns corresponding to 3
c>channels.
c>
c>Therefore  the structure of such SESAME data file is:
c>
c>- First line :  "SESAME ASCII data format (saf) v. 1  "
c>- Header information
c>    - Empty lines are allowed
c>    - Comments can be added provided that the first character in the row
c>is "#"
c>    - Keywords are not case sensitive
c>    - Separator between keywords and their values is "="
c>    - The order of the keywords is irrelevant
c>    - Blanks can be added everywhere
c>- Data begin after this separator  line : "####--------------------"
c>- Data are written in  three columns free format
c>- Column 1 must correspond to the Vertical channel, columns 2 and 3 to
c>the horizontal ones
c>
c>Mandatory keywords are :
c>- STA_CODE : station/site code (In the GSE2.0 format this parameter is
c>at most 5 characters long)
c>- START_TIME : start date and time - year month day hour minute second
c>(e.g. 1999 4 23 0 3 44.78 )
c>- SAMP_FREQ : sampling frequency in Hertz
c>- NDAT : number of samples
c>- CH0_ID : component 1 definition - It should be the vertical channel.
c>It is a label. (In the GSE2.0 format this parameter is  3 characters
c>long)
c>- CH1_ID : component 2 definition - horizontal
c>- CH2_ID : component 3 definition - horizontal
c>- UNITS : label (e.g. m/s)
c>the following keyword is optional:
c>-  NORTH_ROT : is the orientation of the first horizontal component
c>(channel 1 - column 2) from North clockwise (degrees)
c>
c>For the  data specifically prepared for WP04 (Nikos Theodulidis) it was
c>requested to add some specific comments (line in the header with "#" in
c>the first row), see Rome WP04 minutes for details. Of course these
c>comments do not affect at all the readibility of the file by J-SESAME.
c>
