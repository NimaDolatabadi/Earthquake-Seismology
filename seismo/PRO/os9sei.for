c                                                                               
c    transform os9 binary or ascii files to seisan  data files               
c                                                                               
c    j. havskov                       
c
c    Note: Search for word COMPUTERTYPE to move progarm between, sun 
c          and pc
c
c    Parameter in defines some type of input
c
c    in=0 : One binary file, work only on sun or pc due to byte swapping on vax
c           only binary files on version 1 and 2
c    in=1 : Several ------, use filenr.lis as input file name
c    in=2 : Ascii file of some type
c    in=3 : Only os9.log file, no questions
c    in=4 : Only os9.log file, ------------,output in RAW directory
c
c    in order ot use options 3 and 4, hardwire in (below) and recompile.
c
c    In addtion logicals old,new and asc define file types:
c    old true : 1024 byte headers before june 92
c    new true : 1024 byte headers after june 92
c    asc true : oldest ascii format, used fixed format blocks and a 512
c               byte header. presently (aug 92) only used by bgs
c                                                                               
c    latest update                                                              
c    jan 3 90 by j.h. : transform directly to seisan  data files                
c    jan 7 90         : bugs                                                    
c    oct 1 91         : binary version                                          
c    oct 17 91        : bug with rlength
c    oct 24 91        : little change in filename
c    nov 4  91        : convert ascii files also, same prog. on vax,sun and pc
c    dec 10 91        : change storage vector to one dimentional
c                       on sun fortran version 1.3.1 it is not possible to
c                       write out more than 16384 i*2 variables from a 2-d
c                       array, a Bug!!!!!!  Another bug is that it must be
c                       compiled with option -Bstatic.
c                       Checking od number sizes added since there were
c                       some problems with tcp/ip transmissiion, which sometimes
c                       gave errors.
c    jun 11 92  jh    : new header size in input file, 4 byte integers
c    aug 11 92        : new input file header
c    aug 13 92        : fix to use oldest ASC format
c    sep 14 92        : user interaction if error
c    feb 26 93        : question and infile to 80 chars
c    jun 02 93        : patch to check for errors, fix better in future, 384
c    jul 2  93        : change parameter in to be able to run without questions
c                       and put output files in RAW directory
c    jul 22           : small bug with old ASC format os9.log files
c    dec 93           : upper lower case
c    jan 30 93        : pc byte swappeing, where did it go !!!
c    sep 28 94        : 64 channels
c    oct 13 94        : new seislog ad unit numbers
c    jan 95 95        :**************** version 5.0 ************** seidim
CJAB(BGS)Mar95        : If the generated file suffix in agency BGS (and
CJAB(BGS)Mar95          associated agencies) is contained in the input file
CJAB(BGS)Mar95          suffix, then use the input file suffix.
c mar 29 by jh        : byte length 2 or 4, general
c aug 4               : delete output file if not complete, allow for
c                       missing header data with cmp6 os9.log type file
c aug 15              : bug by not clearing outfile variable
CJE(BGS)Aug95         : for large samples > 50000, program exiting due to too
CJE(BGS)Aug95           many samples/channel. Value hard-wired into test. 
CJE(BGS)Aug95           Value replaced by max_sample as given in seidim.inc.
CJAB(BGS)Sep95        : Same problem with test for rlength, set to max_sample*2
CJAB(BGS)Sep95        : For os9.log files, the sample size was being re-set
CJAB(BGS)Sep95          incorrectly,  causing routine to fail (but nicely!).
CJAB(BGS)Sep95          Further, blocks with different # records than expected
CJAB(BGS)Sep95          forced an "ignore" prompt...now only a warning is given.
CJAB(BGS)Sep95          The mismatching block samples are zeroed, and the #
CJAB(BGS)Sep95          samples re-set to the mimimum of expected and found.
c  oct 12 95 by jh    : fix a JAB bug to get correct number of samples in buffer
c                       set up general byte swap, bug not clearing file name
c                       when in raw mode
c oct 31              : delete when error
c jan 29 96           : problems with byte order
c jan 98              : fix check for response
c may 98              : fix so seislog 7.0 runs with os9.log
c dec 20 98 jh        :  --------- version 7.0 check -----------------
c                       only partly done, old file name, byte order not fixed
c                       on linux
c jan 7  99           : long file name, no linux swap
c jun 15              : new year 2000 qnx format
c jan 3 2000          : some y2k fixing, patch up to be able ro run old qnx
c may 11 2000         : new waveform structure
c September 25, 2000  : add input of file name from prompt
c February 15 2001    : added option for additional command line args
c                             option for using BGS seisan extensions
c                             fixed bug for 16 bit ILI digitizer 95
c                             determine year explicitly from headers
c june 23 2001 jh     : checked os9 files, seemed ok for 1999 and 2000
c
c dec  6              : remove max_sample_dem form include
c jan 29, 2002 lot    : patch for GUV
c feb 02 03     jh    : remove deocmp6 and intand, now in compdecomp
c dec 07 jh           : remove swap routines, are in library
c
      implicit none
      include 'seidim.inc'
      external    seiclen                  !JAB(BGS)Mar95. Length
      integer     seiclen                  !JAB(BGS)Mar95. Function.
      character*2 byte_length              ! byte length
c                                                                               
c-- seisan  field station file header                
      character*1024	header		
c-- seisan  data file channel header      
      character*1040    chahead         
c-- -------------------main --------      
      character*2000     mainhead        
c-- output file name                                  
      integer           os9sei_c                  ! Working length of outfile.
      character*80	outfile		          !
      character*80	outdir                    ! output directory
      character         chr_suffix *(10),         ! Input file suffix.
     &                  chr_agency *(3)           ! & agency.
c-- save part of header                         
c-- data vector, one buffer or one channel                              
      integer*2		data2(max_sample)	
      character*2   swap (max_sample)
      integer*4     data4(max_sample)
c--output data,  whole file, not to be used on pc
      integer*4         y(1600000)
c-- one line of text
      character*80 text
c-- data array used with compression
      integer*4         datax(max_sample)
c-- compressed ascii data	
      character*1       abuf(max_sample)
c-- ascii data
      character*2 cbuf(2560)
c-- character block size compressed
      integer           sblk	
c-- ad unit, 99 is 4 byte nanometrics
      integer unit
c-- number of equivalent samples in one buffer and in buffer header
      integer nsmp_buf,nhead
c-- in: dim of array, out: # of values returned
      integer           out		
c-- error return
      integer ierr
c-- input file name
      character*80 infile
c-- top directory
      character*60 top_dir
c-- event number in
      integer number
c-- question
      character*80 question
c-- number of channels and samples/chan, total number of samples           
      integer nchan,nsamp,ntotal
c-- total time window                          
      real              total_time	
c-- logical for file types etc
      logical old,new,asc,raw,qnx
c logical for using BGS seisan extensions (bjb 2001/02/15)
c      logical bgs_seisan
      character*10 extension
c-- block record length                   
      integer           rlength         
c-- number of blocks                              
      integer           nblock
c-- displacement first sample in buffer, only old files
      integer displm		
c-- sample rate                                              
      integer           irate
      real		rate
c-- help variables                                     
      integer		i,k,ib,in,j,kk
      character*1       dchar,sw,c
c-- -------------                                    
      integer           ich		
      integer byte_order    ! byte order, machine where written 1: sun, 2:pc
c-- for swapping
      integer inputtype
c determine year explicitly (bjb 2001/02/15)
      integer year
c number of command line arguments (bjb 2001/02/15)
      integer narg
c char string for each argument (bjb 2001/02/15)
      character*80 argument(10)

      equivalence(data2,swap)
      logical pc,sun,linux


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c set initial file or action, if gt 2, no questions asked 
c
      in=0
      outfile = ' '                       !JAB(BGS)Mar95. Empty filename.
c
c call function to determine if using BGS seisan extensions
c
c      call get_bgs_seisan(bgs_seisan)
      call get_env_seisan_extension(extension)
c
c   set computer type
c
      call computer_type(sun,pc,linux)
c
      call dir_char(dchar)
c
c   get top directory
c
      call topdir(top_dir)
c
c   check if output in RAW directory
c
      raw=.false.
      if(in.eq.4) raw=.true.
c
c  get file name, could be from line argument
c

      call get_arguments(narg,argument)
      if(narg.eq.1) then
         infile=argument(1)
         goto 1   ! jump other input
      else if (narg.eq.3) then
         infile=argument(1)
         outdir=argument(2)
         read(argument(3),'(i5)') inputtype
c
c set as ascii input if 3rd command line arg is 1
c
         if(inputtype.eq.1) in=2
         goto 1   ! jump other input
      endif

      if(in.lt.3) then
         write(6,*)
     *   ' Input file binary: Give number, name or filenr.lis for all'
         write(6,*)
     *   ' Input file ascii : Filename must be os9.log'
         question=' Filename, ?, or number'
         call filename(question,infile)
c
c  check for no files
c
         if(infile(1:3).eq.'EOF') goto 99
c
c   check which type of input file
c
         if(infile(1:10).eq.'filenr.lis'
     *   .or.infile.eq.'FILENR.LIS') then
            open(8,file='filenr.lis',status='old')
            in=1
         endif
         if(infile(1:7).eq.'os9.log'
     *   .or.infile(1:7).eq.'OS9.LOG') in=2
     
c         if(in.eq.3.or.in.eq.4) then
c            in=2
c            infile(1:7)='os9.log'
c         endif

      endif
c
c-----------------------------------------------------------------------
c  back here if many binary files converted using filenr.lis, or from prompt input
c-----------------------------------------------------------------------
c
 1    continue
      if(in.eq.1) then
         read(8,'(2x,i3,2x,a)') number,infile
         write(6,*)
         write(6,'(1x,i3,3x,a)') number,infile
         if(infile(1:4).eq.'    ') goto 99
      endif
c                                                                               
c  open os9 binary file if that is the option
c     
      if(in.lt.2)
     *open(1,file=infile,status='old',access='direct',recl=1024)            
c
c   open os9 ascii log file if that is the option
c
      if(in.eq.2) open(1,file=infile,status='old')
c                                                                               
c   read header if binary file
c                                                                               
      if(in.lt.2) read(1,rec=1) header
c
c   read header if ascii file, first read forward to 
c   start of event, also enter here after read of one event
c   from asci file
c             
 2    continue
      if(in.eq.2) then
 11      continue
         read(1,'(a80)',end=99) text
         if(text(1:30).eq.'START OF EVENT ***************') then
c
c   check if old asc format indicated by logical asc, that is
c   indicated by ASC at end of START of .... string.
c
            asc=.false.
            if(text(33:35).eq.'ASC') asc=.true.
            if(asc) then
               read(1,'(80a1)',err=15) (header(i:i),i=1,512)
            else
               read(1,'(80a1)',err=15) (header(i:i),i=1,1024)
            endif
            goto 16
 15         continue
 384        continue        ! error from internal read below....
            write(6,202)
 202        format
     *      (/,' Error with os9.log file, will try next event',/)
ctest            write(6,*)' nsamp',nsamp,'nchan',nchan,'unit',unit,'rate',
ctest     *      rate,'displm',displm
            write(6,*)' Return to continue'
            read(5,'(a1)') i
         endif
         goto 11
      endif
 16   continue
c
c   find if old or new header format
c
      new=.true.
      old=.false.
      if(header(10:14).ne.'       ') then
        old=.true.
        new=.false.
      endif
      write(6,*)
      if(asc) write(6,*)' old ASC format'
      if(old) write(6,*)' old header format'
      if(new) write(6,*)' new header format'
c                                                                               
c  make file name 
c             
      outfile=' '      ! jh add oct 95

c
c fix for GUV, lot 29.01.2002
c
         if (header(1:3).eq.'GUV') then
            read(header(17:18),'(i2)') year
            year=year+8-100
            write(header(17:18),'(i2)') year
         endif
c
c-- year
         if(new) outfile(3:4)=header(17:18)
         if(old) outfile(1:2)=header(6:7)
         if(outfile(3:4).eq.'10') outfile(3:4)='00'   ! patch to use old qnx
         read(outfile(3:4),'(i2)') i
         if(i.lt.50) then
            outfile(1:2)='20'
         else
            outfile(1:2)='19'
         endif
c-- month
         outfile(5:5) = '-'
         if(new) outfile(6:7)=header(24:25)
         if(old) outfile(3:4)=header(13:14)
         outfile(8:8)='-'
c-- day     
         if(new) outfile(9:10)=header(27:28)
         if(old) outfile(6:7)=header(16:17)
         outfile(11:11)='-'
c-- hr      
         if(new) outfile(12:13)=header(33:34)
         if(old)outfile(9:10)=header(19:20)
c-- min     
         if(new) outfile(14:15)=header(36:37)
         if(old) outfile(11:12)=header(22:23)
         outfile(16:16)='-'
c-- sec
         if(new) outfile(17:18)=header(39:40)
         if(old) outfile(14:15)=header(25:26)
         outfile(19:20)='S.'
c-- network or station code
         outfile(21:25)=header(1:5)
         do i=21,25
            if(outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         outfile(26:27)='_0'
c-- number of channels
         if(new) outfile(28:29)=header(63:64)
         if(old) outfile(22:23)=header(32:33)

c         if (bgs_seisan) then
         if (extension(1:3).eq.'BGS') then
c-- retrieve code from filename in header for manual extracts etc.
            if(header(897:897).eq.'2')then
               qnx=.true.
            else
               qnx=.false.
            endif
            if(new) then
               if(qnx) then
                  outfile(30:31)=header(923:924)
               else
                  outfile(30:31)=header(920:921)
               endif
            endif
         endif
c
c   BGS...if station code/# channels is given in the suffix of the input
c         file, then use the input file suffix...
c
         call get_agency( chr_agency )                         ! Get agency.
         if( chr_agency .eq. 'BGS' .and.                       ! Valid.
     &       in         .lt. 2   ) then                        !
         chr_suffix = infile(index(infile,'.')+1:)             ! Get suffix.
            if( index(chr_suffix,'.')            .eq. 0   .and.! Valid suffix.
     &          index(chr_suffix,outfile(18:23)) .gt. 0 ) then ! Is there!
            outfile(18:) = chr_suffix                          ! & replace.
            end if                                             !
         endif
c----------------------------------------------------------------
c
c-- check for blanks                                        
         do i=1,seiclen(outfile)                    !JAB(BGS)Mar95.
            if(outfile(i:i).eq.' ') outfile(i:i)='0'                            
         enddo                                                                  
         write(*,*)' Output file name is: ',
     &              outfile(1:seiclen(outfile))     !JAB(BGS)Mar95
c
c bjb 4/11/99
c prefix output directory given in subroutine arguments
c to outputfilename
c
         if(narg.gt.1) then
            outfile=outdir(1:seiclen(outdir))//dchar//outfile
            write(*,*)' Output file name is: ',
     &           outfile(1:seiclen(outfile))
         endif
c
c   read header info
c                                                       
         if(new) read(header(17:18),'(i2)') year
         if(old) read(header(6:7),'(i2)') year
         
         if(year.lt.50) then
            year=year+2000
         else
            year=year+1900
         endif
         write(*,*) 'Year is ', year

         if(new) read(header(81:88),'(i8)',err=384) nsamp
         if(old) read(header(40:45),'(i6)',err=384) nsamp
         if(new) read(header(63:64),'(i2)',err=384) nchan
         if(old) read(header(32:33),'(i2)',err=384) nchan
         if(new) read(header(105:112),'(i8)',err=384)rlength
c        if(old) read(header(548:549),'(i2)',err=384) unit
         if(old) unit=0
         if(new) read(header(97:98),'(i2)',err=384) unit
         if(asc) then
            rlength=5120
         else
            if(old) read(header(561:565),'(i5)',err=384)rlength
         endif
         if(new) read(header(65:80),'(f16.7)',err=384) rate
         if(new) read(header(95:96),'(a2)',err=384)byte_length
         if(new) read(header(94:94),'(i1)',err=384) byte_order
         if(old) read(header(35:38),'(i4)',err=384) irate
         if(old.and.asc) read(header(47:49),'(i3)',err=384) displm
         if(old) then
           rate=float(irate)
         else
           rate=1/rate
         endif
c
c   check rlength size
c
         if(rlength.gt.max_sample*2) then                  !JAB(BGS)Sep95.
            write(6,*)' Too long buffers, rlen= ',rlength
            stop
	  endif		
c
c   for the time being assume that earth data is written on pc systems
c
      if(byte_order.eq.0) byte_order=1       ! if no byte order, assume VME
      if(unit.eq.63) byte_order=2            ! earth data
      write(6,'(1x,a,i3)') 'Digitizer ',unit
      write(6,'(1x,a,a)') 'Number of bytes in data word ', byte_length
c
c   calculate number of samples pr buffer inc header and header
c   in samples taking into account if 2 or 4 byte words
c
         nsmp_buf=rlength/2
         nhead=32

c         if((unit.eq.30.or.unit.eq.36).and.in.eq.2) then
c             byte_length=' 2'
c             goto 3747      ! force 2 byte for ascii transmission
c         endif

c problems with 16 bit ili data both binary and ascii 
c fixed bjb 7/9/99 by removing unit 95 (16 bit ILI) 
c from if statement
c         if(unit.eq.99.or.unit.eq.53.or.unit.eq.56.or.unit.eq.63
c     &   .or.unit.eq.95.or.unit.eq.31               !JE(BGS)Aug95.
c     *   .or.byte_length.eq.' 4')  then
         if(unit.eq.99.or.unit.eq.53.or.unit.eq.56.or.unit.eq.63
     &   .or.unit.eq.31.or.byte_length.eq.' 4')  then
            byte_length=' 4'
            nsmp_buf=nsmp_buf/2              !JAB(BGS)Sep95 reset incorrectly.
            nhead=16
         endif
 3747    continue
         if(asc) nhead=displm
c         byte_length=' 4'           ! XXX
c
c   check if not too many samples
c
         if(nsamp.gt.max_sample) then            !JE(BGS)AUG95.
	    write(6,*)' Too many samples/chan, nsamp= ',nsamp
            stop
         endif			 
         nblock=nsamp*nchan/(nsmp_buf-nhead)                                    
         write(6,*)' nsamp=',nsamp,'  nchan=',nchan,' reclen=',rlength          
         write(6,*)' number of blocks=',nblock                                  
c
c   close and reopen input file with record length of data buffers
c   if a binary file
c		                                                                
         if(in.lt.2) then
            close(1) 
            open(1,file=infile,status='old',
     *      access='direct',recl=rlength)   
         endif
c
c  if on pc and ascii file, open a scratch file to help demultiplexing
c
         if(pc.and.in.eq.2) open(7,form='unformatted',status='scratch')
c
c   construct  main header
c
         do i=1,2000                                                            
           mainhead(i:i)=' '                                                    
         enddo                                                                  
         mainhead(2:18)='                  '                              
c-- no of channels                        
         if(new) mainhead(32:33)=header(63:64)
         if(old) mainhead(32:33)=header(32:33)
c-- year, date and time
         if(year.lt.2000)then
            mainhead(34:34)='0'
         else
            mainhead(34:34)='1'
         endif
         if(new) mainhead(35:46)=header(17:28)
         if(new) mainhead(48:59)=header(33:44)
         if(old) mainhead(35:59)=header(6:30)
c
c   patching up year
c
c         if(mainhead(35:36).eq.'10') mainhead(35:36)='00' ! old qnx, no y2k fix
c         if(mainhead(35:35).eq.'0') mainhead(34:34)='1'
c
c  who commented above out ???
         if(mainhead(35:35).eq.' ') mainhead(35:35)='0'  ! lot 29.01.2002

          mainhead(36:36)=outfile(4:4)
c-- total time window
         total_time=nsamp/rate
         write(mainhead(61:69),'(f9.3)') total_time
         j=162
         if(new) k=161
         if(old) k=51
         do i=1,nchan                                                           
c--  station and component              
            mainhead(j:j+7)=header(k:k+7)	
            mainhead(j+8:j+8)=' '                                               
c-- rel. start time of channel           
            mainhead(j+9:j+15)='    0.0'	
            write(mainhead(j+17:j+24),'(f8.2)') total_time                      
            if(i.eq.3.or.i.eq.6.or.i.eq.9.or.i.eq.12.or.i.eq.15.                
     *      or.i.eq.18.or.i.eq.21.or.i.eq.24.or.i.eq.27.or.i.eq.30.
     *      or.i.eq.33.or.i.eq.36.or.i.eq.39.or.i.eq.42.or.i.eq.45.
     *      or.i.eq.48.or.i.eq.51.or.i.eq.54.or.i.eq.57.or.i.eq.60.
     *      or.i.eq.63) then        
              j=j+28                                                            
            else                                                                
              j=j+26                                                            
            endif                                                               
            if(new) k=k+8
            if(old) k=k+11
         enddo                                                                  
c
c   open output binary file, modify name if it goes in RAW directory
c
         if(raw) then
           i=index(top_dir,' ')-1
           text(1:i)=top_dir(1:i)
           text(i+1:i+1)=dchar
           text(i+2:i+4)='RAW'
           text(i+5:i+5)=dchar
           text(i+6:i+7)=mainhead(45:46)
           text(i+8:i+8)=dchar
           if(text(i+6:i+6).eq.' ')text(i+6:i+6)='0'
c
           os9sei_c = seiclen( outfile )                       !JAB(BGS)Mar95.
           text(i+9:i+8+os9sei_c)=outfile(1:os9sei_c)  !JAB(BGS)Mar95.
           outfile(1:i+8+os9sei_c)=text(1:i+8+os9sei_c)!JAB(BGS)Mar95.
         endif
         open(2,file=outfile,status='unknown',form='unformatted')        
         write(6,'(1x,a,a)')'Output file: ',outfile
c
c                                                                               
c   write  main header                                       
c                                                                               
         k=1                                                                    
         do i=1,12                                                              
            write(2) mainhead(k:k+79)                                           
ctest            write(6,'(a80)') mainhead(k:k+79)	
            k=k+80                                                              
         enddo                                                                  
c
c   write remaining main header lines if more than 30
c
         if(nchan.gt.30) then
            kk=(nchan-31)/3+1
            do i=13,kk+12
               write(2)mainhead(k:k+79)
               k=k+80
            enddo
         endif
c                                                                               
c   start channel read loop
c
         j=0                                                                    
         do ib=2,nblock+1                                 
c                                                                               
c   read one buffer, binary or ascii
c   if on pc and ascii, event must be written out in a single
c   file to do demultiplexing later
c   if binary file, make sure first data buffer is read
c   at correct place, which is always beyound the 1024 bytes,
c   see manual for format ( fixed below by 1024/rlength),
c   note: all data is transferred to 4 byte array
c                                                                               
            if(in.lt.2) then
c              if(unit.ne.99) then
               if(byte_length.ne.' 4') then
                  read(1,rec=ib+1024/rlength) (data2(i),i=1,nsmp_buf)
c
c if pc do byte swapping, is now done below
c
c                  if(pc) then
c                     do i=1,nsmp_buf
c                        sw=swap(i)(2:2)
c                        swap(i)(2:2)=swap(i)(1:1)
c                        swap(i)(1:1)=sw
c                     enddo
c                  endif
                               
                  do i=1,nsmp_buf
                     data4(i)=data2(i)
                  enddo
               else
                  read(1,rec=ib+1024/rlength) (data4(i),i=1,nsmp_buf)
               endif
            endif
c
c   ascii file, check for correct position
c
            if(in.eq.2) then
               read(1,'(i4,4x,i6)',err=27) k,sblk
               write(6,*)' block:',ib-1,'     c bytes:', sblk
               if(k.ne.ib-1) then
c
c  could be that one line is missing in header, try previous line
c  before giving up
c
                  write(6,*)' Error, will try previous line'
                  backspace 1
                  backspace 1
                  read(1,'(i4,4x,i6)',err=27) k,sblk
                  if(k.ne.ib-1) then
                     write(6,202)
                     write(6,*)' Return to continue'
                     read(5,'(a1)') i
                     close(2,status='delete')  ! delete since not complete
                     goto 2
                  endif
               endif
               goto 28
 27            continue
               write(6,202)
            write(6,*)' Return to continue'
            read(5,'(a1)') i
            close(2,status='delete')  ! delete since not complete
               goto 2
 28            continue
c
c   read one  buffer, chose buffer format according to compression,
c   for fomatted input, only os9 files of version later then
c   June 92 can be used
c

               if(sblk.eq.0.and.(.not.asc)) then
                  read(1,'(10i8)',err=27,end=38)
     *            (data4(i),i=1,nsmp_buf)
               endif
               if(asc) then
                  read(1,'(40a2)',err=27,end=38)
     *            (cbuf(i),i=33,2560)
c
c   header not transfered, put in something which can be transformed
c
                  do i=1,32
                     cbuf(i)='aa'
                  enddo
c
c   transform from asci to integer
c
                  call asck(2560,data4,cbuf,-1,ierr)
                  out=nsmp_buf
               endif
c
c   compressed data
c
               if(sblk.ne.0.and.(.not.asc)) then
                  read(1,'(80a1)',err=27,end=38)
     *            (abuf(i),i=1,sblk)
               endif
c
               goto 39
 38            continue
               write(6,*) ' End of file'
               goto 99
 39            continue
c
c   transform from compressed asci to integer if compressed
c
               if(asc) goto 42              
               if(sblk.ne.0) then
                  out = nsmp_buf*2                  !JAB(BGS)Sep95 (outragious!)
cJAB(BGS)Sep95                  out=10000
                  call dcomp6(sblk,abuf,out,data4,ierr)
c
c   check if correct number of samples out
c
                  if(out.ne.nsmp_buf) then
                     write(6,*)' Wrong number of samples for buffer'
                     write(6,*)' Should have been:',nsmp_buf
                     write(6,*)' Is:              ',out
c
                     if( out .gt. nsmp_buf ) then    !JAB(BGS)Sep95
                     do i = nsmp_buf+1,out           !JAB(BGS)Sep95
                     data4(i) = 0.0                  !JAB(BGS)Sep95
                     end do                          !JAB(BGS)Sep95.
                     else                            !JAB(BGS)Sep95
                     do i = out+1, nsmp_buf          !JAB(BGS)Sep95.
                     data4(i) = 0.0                  !JAB(BGS)Sep95
                     end do                          !JAB(BGS)Sep95
                     end if                          !JAB(BGS)Sep95
                     write(6,*)' Ignore(i) or return to continue',
     *                           ' with next event'
                     read(5,'(a1)') c
                     if(c.eq.'i'.or.c.eq.'I') then
                        continue
cJAB(BGS)Sep95                        do i=out,nsmp_buf
cJAB(BGS)Sep95                           data4(i)=0.0
cJAB(BGS)Sep95                        enddo
                     else
                        goto 2
                     endif
                  endif
c
c   decompress by removing differences 2 times
c
                  call rmfdif(out,data4,datax,nchan)
                  call rmfdif(out,datax,data4,nchan)
               endif
 42            continue
               if(pc) then
c                 if(unit.eq.99) then
                  if(byte_length.eq.' 4') then
                     write(7) (data4(i),i=1,nsmp_buf)
                  else
                     do i=1,nsmp_buf
                        data2(i)=data4(i)
                     enddo
                     write(7) (data2(i),i=1,nsmp_buf)
                  endif
               endif
c
c   end of ascii read one channel
c
            endif
c                                                                               
c  remove block buffers and save data in one array if sun 
c                                          
            if(sun.or.linux) then                                     
                do k=nhead+1,nsmp_buf
                  j=j+1
                  y(j)=data4(k)
                enddo
            endif
            ntotal=j
c
c   end of channel loop
c                                                               
         enddo
         write(6,*)
     * ' Input file read, now follows headers in output file'
c                                                                               
c  end read for one event
c                                                                               
c                                                                               
c  write each channel with channel headers
c                                                                               
         do ich=1,nchan                                                         
            do i=1,1040                                                         
               chahead(i:i)=' '                                                 
            enddo                                                               
            if(new) j=(ich-1)*8+161
            if(old) j=(ich-1)*11+51
c-- station code                          
            chahead(1:4)=header(j:j+3)		
c-- componnet
            chahead(6:9)=header(j+4:j+7)	
c-- year, date and time                  
            chahead(10:35)=mainhead(34:59)		
            write(chahead(37:43),'(f7.2)') rate                          
            write(chahead(45:50),'(i6)') nsamp                                  
c                                                                               
c   get response            
c                                                                               
            call read_resp_head(chahead) 
c
c   indicate 2 or 4 byte integer in header
c
            if(unit.eq.99) chahead(77:77)='4'
            if(byte_length.eq.' 4') chahead(77:77)='4'
c                                                                               
c   write header                                                                
c                                                                               
            write(2)chahead                                                     
            write(6,'(1x,a70)') chahead(1:70)
c
c   check if response curve
c
c           if(chahead(160:160).eq.'9') write(6,*)
c            if(wav_resp_status(1:1).eq.'9') write(6,*)
c     *        ' No response file for this channel --------'

c                                                                            
c  demultiplex data if on sun 
c
c  remember, no swapping if input is an ascii file************************
c
c            if(sun.or.linux) then
            if(sun) then
               j=0
               do k=ich,ntotal,nchan
                  j=j+1
c                 if(unit.ne.99) then
                  if(byte_length.ne.' 4') then
                     data2(j)=y(k)
                  else
                     data4(j)=y(k)
                  endif
               enddo
c              if(unit.ne.99) then
               if(byte_length.ne.' 4') then
                  if(byte_order.eq.2.and.in.lt.2) 
     *            call swap2(nsamp,data2)
                  write(2)(data2(k),k=1,nsamp)
               else
                  if(byte_order.eq.2.and.in.lt.2) 
     *            call swap4(nsamp,data4)
                  write(2)(data4(k),k=1,nsamp)
               endif
            endif
c
c   if on pc, whole file must be read for each channel
c   to demultiplex
c
c            if(pc) then
            if(pc.or.linux) then
               if(in.lt.2) rewind(1)
               if(in.eq.2) rewind(7)
               j=0                                                              
               do ib=2+(1024/rlength),nblock+1+1024/rlength                    
c                 if(unit.eq.99) then
                  if(byte_length.eq.' 4') then
                     if(in.lt.2)read(1,rec=ib) (data4(i),i=1,nsmp_buf)         
                     if(in.eq.2)read(7) (data4(i),i=1,nsmp_buf)                
                     if(byte_order.eq.1.and.in.lt.2) 
     *               call swap4(nsmp_buf,data4)

                  else
                     if(in.lt.2) then
                         read(1,rec=ib) (data2(i),i=1,nsmp_buf)          
c
c  do byte swapping
c
c                        do i=1,nsmp_buf
c                           sw=swap(i)(2:2)
c                           swap(i)(2:2)=swap(i)(1:1)
c                           swap(i)(1:1)=sw
c                        enddo
                     endif
                     if(in.eq.2)read(7) (data2(i),i=1,nsmp_buf)                 
                     if(byte_order.eq.1.and.in.lt.2) 
     *               call swap2(nsmp_buf,data2)
                  endif
c
c   demultiplex
c
                  do k=nhead+1,nsmp_buf,nchan                                   
                    j=j+1  
c                   if(unit.eq.99) then
                    if(byte_length.eq.' 4') then
                       datax(j)=data4(k+ich-1)
                    else
                       datax(j)=data2(k+ich-1)
                    endif
                  enddo                                                         
               enddo    
ctest          write(6,*)' nsamp,j', nsamp,j                 
               nsamp=j                                              
c               if(unit.eq.99) then
                if(byte_length.eq.' 4') then
                  write(2)(datax(k),k=1,nsamp)                                        
               else
                  do i=1,nsamp
                     data2(i)=datax(i)
                  enddo
                  write(2)(data2(k),k=1,nsamp)
               endif
            endif
         enddo
         close(2,err=765)                       
         if(pc.and.in.eq.2) close(7)                                  
         goto 766
 765     continue
         write(6,*) ' Error close 2'
 766     continue
c
c  if file is binary, close to be able to open next file if more than 1
c                                                                  
         if(in.lt.2) close(1)
c                                                                               
c   back for next event if a list of events are transformed
c   or an ascii input file is used
c              
      if(in.eq.1) goto 1          
      if(in.eq.2) goto 2
c                                                                               
c   end of data                                                                 
c                                                                               
 99   continue                                                                  
      stop                                                                      
      end                                                                       
C*********************************************************************
C                                                                    *
C     SUBROUTINE RMFDIF(NPTS,IXIN,IXOUT,NCHAN                        *
C  THIS ROUTINE WAS ORIGINALLY LIKE:                                 *
C     SUBROUTINE RMFDIF(NPTS,IXIN,IXOUT)                             *
C                                                                    *
C     THE ROUTINE NOW REMOVES 1. DIFFERENCES IN MULTIPLEXED DATA     *
C         THEREFORE NUMBR OF CHANNELS NCHAN TO BE SPECIFIED          *
C                                                                    *
C     ROUTINE TO REMOVE 1ST DIFFERENCES OF INTEGER ARRAY IXIN        *
C     WHICH HAS LENGTH NPTS AND PUT THE OUTPUT INTO THE ARRAY IXOUT. *
C     THE ARRAY IXOUT MAY BE THE SAME AS THE ARRAY IXIN IN WHICH CASE*
C     THE ARRAY IXIN WILL BE OVERWRITTEN.                            *
C     SUBROUTINE ASSUMES THAT WHEN FIRST DIFFERENCES WERE TAKEN      *
C     STARTING VALUE WAS ZERO. (SEE SUBROUTINE FSTDIF)               *
C                                                                    *
C     SECOND DIFFERENCES CAN BE REMOVED BY CALLING THIS SUBROUTINE   *
C     TWICE                                                          *
C                                                                    *
C*********************************************************************
C
      SUBROUTINE RMFDIF(NPTS,IXIN,IXOUT,NCHAN)                              
      INTEGER *4 IXIN(1),IXOUT(1),I,J,NPTS,NCHAN
C
      DO 11 I = 1,NCHAN
         IXOUT(I) = IXIN(I)
   11 CONTINUE
      I = 0
   12 CONTINUE
      I = I + NCHAN
      DO 13 J = 1,NCHAN
         IXOUT(I+J)=IXOUT(I+J-NCHAN) - IXIN(I+J)
   13 CONTINUE
      IF(I .LT. NPTS) GOTO 12
   10 CONTINUE
      RETURN
      END
C***********************************************************************
c

      subroutine asck(n,ix,xchar,con,ierr)
c 
c   convert integer +/-2048  to 2 byte chars or back if con=-1       
c
c  name changed from asc to asck aug 13, 1992, to avoid name conflict,
c  integer arry now 4 byte to ease programming
c
      implicit none
c...number of samples and array with samples
      integer *4 ix(1)
      integer n
c...character array on return
      character*2 xchar(1)
c...character table used
      character*65 chrnum
c...convertion switch, 1: to asci, -1: to integer
      integer con
c...error parameter, ierr=0 : ok, ierr=1 : not ok
      integer ierr
c...byte 1 and 2
      integer i1,i2
c...intrinsic function
      integer index
c...help varaible
      integer i

c
c   assign numbers to chars
c
      chrnum(1:30)= '0:23456789qwertyuiopasdfghjklz'
      chrnum(31:60)='xcvbnmQWERTYUIOPASDFGHJKLZXCVB'
      chrnum(61:65)='NM-?='
      
      ierr=0
c
c  convert from numbers to 2 byte asci
c
      if(con.eq.1) then
         do 1 i=1,n
            ix(i)=ix(i)+2048
            i1=ix(i)/64
            i2=ix(i)-i1*64+1
            i1=i1+1
            xchar(i)(1:1)=chrnum(i1:i1)
            xchar(i)(2:2)=chrnum(i2:i2)
 1       continue
      endif
c
c   convert to numbers
c
      if(con.eq.-1) then
         do 2 i=1,n
            i1=INDEX(chrnum,xchar(i)(1:1))-1
            i2=INDEX(chrnum,xchar(i)(2:2))-1
            if(i1.eq.-1.or.i2.eq.-1) then
               ierr=1
c               write(17,'(i7,1x,a2)')i,xchar(i)
            endif
            ix(i)=i1*64+i2-2048
 2       continue
       endif
       return
       end

