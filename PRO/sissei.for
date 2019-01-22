c  Program name: sissei
c
c  convert Sismalp files to seisan format, PC, Sun and Linux
c
c  it is assumed that the sis ndx files have the same file names except
c  for the extension and that the file name is 12 characters long
c
c  this is a fist version, only tested with a few files
c
c  complete format description given at end of program
c
c  j. havskov, feb   2000
c
c
c  Help was provided by Philipe Lesage, Universite de Savoie
c
c  updates:
c    may 11  00    lo : use read_resp_head
c    feb 03 03     jh : remove intand, now in compdecomp

      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
      integer*4  data(max_sample)     ! output samples
      integer*4  data4                ! input sample
      character*80 text               ! general text
c
c   block of definitions for most conversion programs
c
c-- main header------------------------------------------
      character*80 mainhead(max_trace)
      character*29 mainhead_text   ! text to be put into main header
c-- channel header
      character*1040 chahead
c-- output file name 
      character*80 outfile	  
c-- number of stations
      integer nchan	  
c-- channel to calculate channel header for
      integer ichan
c-- network code
      character*5 net_code	  
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)	  
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)
c--channel header date and times	  
      integer year(max_trace),month(max_trace),day(max_trace),
     *hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	
ccccccccccccccccccccccccccccccccccccc  end

      integer nbits(max_trace)     ! number of bits in input sample
      integer ngainbits(max_trace) ! number of gain bits in input sample  
      integer nblock(max_trace)    ! number fo block each channel
c-- one file block
      integer*2 block_data(512)
c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
	  integer i,k,l
c-- computer type
      logical pc,sun,linux	  
       integer gain       ! gain
       integer data_mask  ! mask to extract data value exponent
       integer gain_mask  ! mask to extract one sample
       integer data1      ! one sample without gain
      integer irec                ! record number
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)
c
c   get def file for station codes, give file name
c
       call read_def_chan('sissei.def',mainhead_text,net_code)
c
c   get file name
c
      in=0
      question=' File name, # or filenr.lis for all'
      call filename(question,infile)	    
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis'.or.
     *infile(1:10).eq.'FILENR.LIS') then
         open(8,file='filenr.lis',status='old',err=20)
         goto 21
 20      continue
         write(6,*)' No filenr.lis'
         stop
 21      continue
         in=1
      endif
c
c   file loop if many files, here use text header files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') infile
c
c  open file 
c
       open(1,file=infile,status='old')
	   
c
c   read all header lines
c
       nchan=0
 1     continue
c
c   read first line
c
       read(1,'(a)',end=50,err=99) text
       nchan=nchan+1
       stat(nchan)=text(5:8)//' '
       read(text(9:14),'(i6)') nsamp(nchan)
       read(text(21:26),'(i6)') nblock(nchan)
       read(text(27:34),'(f8.2)') rate(nchan)
       read(text(37:47),'(i2,1x,i2,1x,i4)') 
     *                  day(nchan),month(nchan),year(nchan)
       read(text(48:59),'(i2,1x,i2,1x,f6.3)') 
     *                  hour(nchan),min(nchan),sec(nchan)

c
c    read 2. line
c
       read(1,'(a)',end=50) text
       comp(nchan)=' '
       comp(nchan)(1:1)=text(9:9)
       comp(nchan)(4:4)=text(11:11)
       read(text(13:14),'(i2)') nbits(nchan)
       read(text(16:16),'(i1)') ngainbits(nchan)
c
       goto 1
c
c  end of reading header file
c
  50   continue
       close(1)
       write(6,*)' Number of channels',nchan
c
c   open data file
c
       infile(10:12)='SIS'
       open(1,file=infile,status='old',access='direct',recl=1024)      

c--------------------------------------------------------------
c  loop to read whole file
c--------------------------------------------------------------
c
c
c  enter loop to define channels if a def file, set cbyte to always 4
c
       do ichan=1,nchan
         call set_def_chan(ichan,stat(ichan),comp(ichan))
         cbyte(ichan)='4'
       enddo
c
c   make seisan headers
c
      ichan=1
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',outfile
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c
      mainhead(1)(2:29)=mainhead_text    ! put in header text
c
      do i=1,12
         write(2)mainhead(i)
         write(6,'(a80)') mainhead(i)
      enddo

      irec=0
C
C   enter channel  loop
c
      do ichan=1,nchan
c
c   make channel header	
c
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                    net_code,mainhead_text,
     *                    stat,comp,nsamp,rate,cbyte,
     *                    outfile,mainhead,chahead)
         write(6,'(1x,a)') chahead(1:78)
c                                                                               
c   get response            
c                                                                               
         call read_resp_head(chahead)                                            
c
c   read data samples for one channel, first set parameters for extracting
c   gain
c
         gain_mask=2**16-2**(16-ngainbits(ichan))   ! mask to get gain
         data_mask=2**nbits(ichan)-1                ! mask to extract data value        
         l=0
         do i=1,nblock(ichan)
            irec=irec+1
            read(1,rec=irec,err=99) block_data
c
c   swap if sun
c
            if(sun) call swap2(512,block_data)
c
            do k=1,512
               l=l+1
c
c   convert each sample
c
               data4=block_data(k)
               if(ngainbits(ichan).eq.0) then           ! signed 16 bit integer
                  data(l)=data4
               else                                     ! gain ranged
                  call intand(data4,gain_mask,gain)     ! extract gain
                  gain=gain/2**nbits(ichan)             ! shift bits right
                  call intand(data4,data_mask,data1)    ! extract one sample
                  data1=data1-2**(nbits(ichan)-1)       ! convert to signed int
                  data1=data1*2**gain                   ! correct for gain
                  data(l)=data1
               endif
            enddo
         enddo
c
c   write data
c
         write(2)chahead
         write(2) (data(i),i=1,nsamp(ichan))
c
c   end of channels loop
c
      enddo
      write(6,*)
c
c  back for next file if many
c
      goto 101
 99   continue
      write(6,*)' Error with file'
 101  continue
      close(2)
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  
c

c                         APPENDIX 1: *.NDX AND *.SIS FILES



c           The seismic  traces processed by the PickEv 97 software comply
c          with a  specific format,  the SISMALP  format. Its advantage is
c          its simplicity  and compactness,  particularly useful  on a PC.
c          Writing filters  to transform these data into other formats and
c          conversely is very simple.

c           The seismic  samples are stored in the form of consecutive 16-
c          bit integers  grouped in blocks of 512 integers (1024 bytes) in
c          the binary name.SIS file, where name is either the station name
c          or the  event name.  Several traces can be stored one after the
c          other. If  the sample  number of  a trace  is not a multiple of
c          512, samples of any value have to be added to complete the last
c          block of this trace.

c           The name.NDX  text file has 2 lines per trace according to the
c          following format:

c          First line:


c               BALI  4096    52     8    100.  31.05.1997 12:34:56.789
c           ^^^^    ^^    ^^^^  ^^^^^ ^^^^    ^^          ^
c                (1)   (2)   (3)   (4)     (5)           (6)


c           (1) Col. 5-8: Station code
c           (2) Col. 9-14:    Number of samples in signal
c           (3) Col. 15-20:   Number of  first signal block in the direct
c                         access name.SIS  file (the  very first  block of
c                         the file has the number 1)
c           (4) Col. 21-26:   Number of blocks making up the signal
c           (5) Col. 27-34:   Sampling  frequency   (real  value  with  a
c                         decimal point)  expressed in  Hz (in the SISMALP
c                         format versions  previous to version 11.20, this
c                         sampling frequency was an integer)
c           (6) Col. 37-59:   Date and time of first sample, separated by
c                         a space  ( ), a  tilde (~),  a circumflex accent
c                         (^), an underscore (_), a plus (+), a minus (-),
c                         or  an  ampersand  (&)  (see  Sections  1.5  and
c                         2.3.7.13)

c          Second line:


c           SISMALP 3 Z 10 0 * 15.02   -512   -43    38     2   1024 1
c           3
c                  ^ ^ ^  ^ ^ ^     ^^^    ^^^   ^^^^  ^^^^^ ^^^    ^ ^^^
c             (1)   2 3 4 5 6  (7)     (8)   (9)   (10)  (11)   (12)13
c           (14)

c      116   Appendix 1


c       (1) Col. 1-7: Name of acquisition system.
c       (2)  Col. 9:  Component number.
c       (3) Col. 11:  Component identification  (for example  V, Z, N,
c                     E, R, L, T, 1..9, etc.).
c       (4) Col. 13-14:   Number of significant bits (variable NrBits
c                     in PICK.INI, see Section 2.1.6).
c       (5) Col. 16:  Number of  gain  bits  (variable  NrGainBits  in
c                     PICK.INI, see Section 2.1.6).
c                     If  this   number  is   0  samples  are  signed,
c                     otherwise they  are not signed. The gain is 2n,
c
c Comment by J. Havskov: Gain factor must 2**n, also gain is applied
c                        after converting to a signed integer

c                     where n  is the non-signed integer stored in the
c                     NrGainBits gain  bits; the  sample value  is the
c                     integer value  stored in  the  least-significant
c                     NrBits, multiplied by the gain value.
c       (6) Col. 18:  Indicator of reversed polarity under PICKEV (see
c                     Section 2.3.7.9).  A space  ( ) means  a  normal
c                     polarity, a star (*) means a reversed polarity.
c       (7) Col. 20-24:   SISMALP version number.
c       (8) Col. 26-31:   Maximum   possible    amplitude   of    the
c                     acquisition system  (AdcMax). A  negative  value
c                     indicates two's  complement  coding  (e.g.  -512
c                     corresponds to  interval [-512,+511], while +512
c                     corresponds to interval [-512,+512]).
c       (9) Col. 32-37:   Maximum    trace     amplitude    downwards
c                     (TraceMin).
c       (10) Col. 38-43:  Maximum trace amplitude upwards (TraceMax).
c       (11) Col. 44-49:  Mean trace offset (TraceMoy).
c       (12) Col. 51-56:  Number of  samples of  the pre-event window
c                     (PreEvent).
c       (13) Col. 57-58 : Channel of  the acquisition  system (unused
c                     in PickEv 97).
c       (14) Col. 59-62 : Gain of channel (unused in PickEv 97).

c       Columns 20-62  did not  exist in SISMALP format before version
c      14.36. Informations  in columns  26-62 are  thus ignored if the
c      version number  given in  columns 20-24  is blank  or is  below
c      14.36. The  channel and  the gain,  in columns  57-62, did  not
c      exist before version 15.02.

c       AdcMax, TraceMin,  TraceMax, and  TraceMoy  values  are  coded
c      using NrBits  and NrGainBits  values, i.e.  coded with the same
c      algorithm as the samples.

c       AdcMax (columns 26-31) and PreEvent (columns 51-56) fields can
c      be left  blank and are then ignored. The three fields TraceMin,
c      TraceMax, and  TraceMoy (columns  32-49) can be left blank as a
c      whole; they will then be ignored.
c
c   Format provided by
c
c*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c Philippe Lesage                           *
c Laboratoire de Geophysique Interne et     |
c Tectonophysique - Savoie (UMR 5559)       *
c Universite de Savoie                      |
c 73376 Le Bourget-du-Lac Cedex             *
c Telephone : (33) 4 79 75 84 86            |
c Telefax   : (33) 4 79 75 87 42            *
c E-mail    : lesage@univ-savoie.fr         |
c*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


