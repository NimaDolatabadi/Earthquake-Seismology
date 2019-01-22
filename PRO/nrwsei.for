c  Program name: nrwsei
c
c  convert files from geological survey of northrhine-westphalia 
c  to seisan format, PC, Sun and Linux
c
c  format description at end
c
c  this is a fist version, only tested with a few files
c
c  format and test data from klaus lehmann klaus.lehmann@nrw.de
c
c  j. havskov, sep   2006
c
c  updates : 
c  mar 20, 2007 jh : add station KRF
c  april 4 2007 jh : limit filename to 12 chars, fix problem of filenr.lis and code k 
c                    on windows
c  april 10        : fix bug with zero amplitudes
c  may 14  2011 jh : fix gfortran bug, adjust to changed format and 
c                    file name length, add station ACN
c
c
c
c  updates:

      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
c     integer*4  data(max_sample)     ! output samples
      character*80 text,text1         ! general text
      integer code                    ! error code
      integer*4 data(8,500000)        ! samples
      integer*4 x(500000)             ! for swapping
      character*1 stat_letter(20)     ! letter for station
      character*5 sta(20)             ! station code
      integer rec_length              ! record length for one read
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
      integer org_stat            ! original number of stations
c
c print version
c
      include 'version.inc'
      out_version_date='Aug  2006    '
      if (version_new) out_version_date=version_date
      call print_ver
c
      org_stat=14
c
      stat_letter(1)='B'
      stat_letter(2)='E'
      stat_letter(3)='G'
      stat_letter(4)='J'
      stat_letter(5)='K'
      stat_letter(6)='L'
      stat_letter(7)='O'
      stat_letter(8)='P'
      stat_letter(9)='R'
      stat_letter(10)='S'
      stat_letter(11)='T'
      stat_letter(12)='W'
      stat_letter(13)='k'
      stat_letter(14)='A'
    
      sta(1)='BHE  '
      sta(2)='ENT  '
      sta(3)='GSH  '
      sta(4)='JCK  '
      sta(5)='KRF  '
      sta(6)='LOH  '
      sta(7)='OLF  '
      sta(8)='PLH  '
      sta(9)='RWB  '
      sta(10)='SOR  '
      sta(11)='TDN  '
      sta(12)='WBS  '
      sta(13)='KRF  '
      sta(14)='ACN'

      call computer_type(sun,pc,linux)
c
c   get def file for station codes, give file name
c
       call read_def_chan('nrwsei.def',mainhead_text,net_code)
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
c
c   read all header lines
c
       stat(1)='TEST '
       nchan=0
 1     continue
c
c   read  a line
c
       read(1,'(a)',end=50,err=99) text
c
c   date
c
       if(text(1:3).eq.'104') then
          k=1
          do i=5,40
             if(text(i:i).eq.'.') then
                text1(k:k)=' '
             else
                text1(k:k)=text(i:i)
             endif
             k=k+1
          enddo
          call seigetvalues(3,text1,code)
          day(1)=array$(1)
          month(1)=array$(2)
          year(1)=array$(3)
       endif
c
c   station code
c
c
       if(text(1:3).eq.'211') then
         do i=1,org_stat
           stat(1)=' '
           stat(1)(1:1)=text(5:5)
           if(text(5:5).eq.stat_letter(i)) 
     *     stat(1)=sta(i)
         enddo
       endif
c
c  time
c
       if(text(1:3).eq.'105') then
          k=1
          do i=5,30
             if(text(i:i).eq.':') then
                text1(k:k)=' '
             else
                text1(k:k)=text(i:i)
             endif
             k=k+1
          enddo
          call seigetvalues(3,text1,code)
          hour(1)=array$(1)
          min(1)=array$(2)
          sec(1)=array$(3)
          goto 2
       endif
       goto 1
c
c   now main header information is read, go on with channels
c
 2     continue   
       read(1,'(a)',end=50,err=99) text
       if(text(1:19).eq.'#BEGINCHANNELHEADER'.or.
     * text(1:20).eq.'#BEGINNCHANNELHEADER') then
          nchan=nchan+1
       endif
       
c
c   channel id
c
       if(text(1:3).eq.'200') then
          if(text(5:5).eq.'T') then
             nchan=nchan-1
             goto 2    ! do not use time channel , no dat
          endif         
          comp(nchan)(1:4)=text(5:8)
          if(text(5:6).eq.'UD') comp(nchan)='S  Z'
          if(text(5:6).eq.'NS') comp(nchan)='S  N'
          if(text(5:6).eq.'EW') comp(nchan)='S  E'
          if(text(5:7).eq.'DCF') comp(nchan)=' DCF'
        endif

c   station code
c
c
       if(text(1:3).eq.'211') then
         stat(nchan)=' '
         stat(nchan)(1:1)=text(5:5)
         do i=1,org_stat
           if(text(5:5).eq.stat_letter(i))
     *     stat(nchan)=sta(i)
         enddo
       endif

c
c   number of samples
c
        if(text(1:3).eq.'220') then
c           read(text(5:10),'(i6)') nsamp(nchan)
           read(text,*) i,nsamp(nchan)
        endif
c
c   sample rate      
c
        if(text(1:3).eq.'241') then
           read(text(5:12),'(g7.3)') rate(nchan)
c          write(6,*) rate(nchan)
           rate(nchan)=1.0/rate(nchan)
        endif
        goto 2

c
c  end of reading header file
c
 50     continue
        
        close(1)
c
c   copy info to other channels
c
        rate(1)=200.0
        do i=2,nchan
          year(i)=year(1)
          month(i)=month(1)
          day(i)=day(1)
          hour(i)=hour(1)
          min(i)=min(1)
          sec(i)=sec(1)
          rate(i)=rate(1)
        enddo

       write(6,*) ' Number of channels',nchan
       write(6,*) ' Date and time',year(1),month(1),
     *              day(1),hour(1),sec(1)
       write(6,*) ' Rate and nsamp', rate(1),nsamp(1)
c
c   open data file
c
c       infile(10:12)='I32'
       i=index(infile,'.')
       infile(i+1:i+3)='I32'
       
       rec_length=nchan*4
       write(6,*)'Now opening binary file:',infile
c
       open(1,file=infile(1:i+3),status='old',access='direct',
     * recl=rec_length)      

c--------------------------------------------------------------
c  loop to read whole file
c--------------------------------------------------------------
c
c
       do i=1,nsamp(1) 
          read(1,rec=i)(data(k,i),k=1,nchan) 
c         write(6,*) i
       enddo 
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
c   transfer to single array and swap if sun
c
         do k=1,nsamp(ichan)
            x(k)=data(ichan,k)
         enddo
         if(sun) call swap4(nsamp,x)
c
c
c   write data
c
         write(2)chahead
         write(2) (x(i),i=1,nsamp(ichan))
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
								  

c Geological Survey of Northrhine-Westphalia (GD NRW)
c Section Geophysics, Earthquake Hazard Analysis
c Krefeld, Germany

c CONVERSION OF RAW DATA OUTPUT INTO SEISAN FORMAT

c Description of input format for conversion program by J. Havskov
c --------------------------------------------------------------------------------------------
c The output format, which is produced by our data acquisition system, is adapted to the 
c input of the data analysis software which we use at this time, 
c i. e. LabVIEW 7.1 by National Instruments Corporation (Austin, Texas, USA),
c former versions as DIA/DAGO by GfS mbH (Aachen, Germany). 

c For the event (in trigger mode) or time interval data (in continuous registration mode),
c two files are saved, respectively, the header file (1) and the waveform data file (2). The 
c file formats are organised as described below.

c The original data of the outlined example of an event header and related waveform data are 
c contained in the files
c S0912002.DAT	(header)
c S0912002.I32	(waveform data).
c The filename structure (3) is explained below.
c 
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c (1) Header file
c ASCII
c The function of each line is defined by a specific identification number in the beginning of 
c that line. Only lines which are necessary for using with Nordic or SeisAn format are listed 
c in the example below. In our application, we use data file with 
c    1 time channel 			(1 implicite channel), 
c    3 seismometer channels, and
c    1 time code channel 		(4 explicite channels).
c    (4 other channels, optionally	4 explicite channels, unused)	
c The example is depicted line by line, begins after the next two lines          
c                 *** description in these columns ***	------------------------------------------------------------------------------------------
c DIAEXTENDED

c #BEGINGLOBALHEADER				
c 101,				name of data set (max. A72 - 72 chars.)
c 102, 				comments (max. 20 lines a A72)
c 103,				analyst's ID or comments (max. A72)
c 104,13.09.2006			date (dd.mm.yyyy)
c 105,9:14:27			time (hh:mm:ss)	
c 112,High -> Low			byte structure definition for INT32 or REAL64 format, 
c                               respectively, ("Low -> High" using the REAL64 format)
c #ENDGLOBALHEADER

c #BEGINCHANNELHEADER
c 200,T				channel name (max. A16)
c 201,Zeit			comments to channel (e. g. "Zeit" -> Time, max. A72)
c 202,[s]				unit of data (e. g. "[s]" -> (s), max. A12)
c 210,IMPLIZIT			type of channel (IMPLIZIT -> implicite / 
c                               EXPLIZIT-> explicite)
c 220,14801	 		length of data set (number of lines)
c 240,85059010			start value (here: t0)
c 241,5E-03			increment value (s; here: time increment T = 0,005 s), 
c                               i.e. a sampling rate of 200 Hz
c #ENDCHANNELHEADER
c 
c #BEGINCHANNELHEADER
c 200,UD				channel name (max. A16)
c 201,Seismometer vertikal	comments to channel (here: "Seismometer vertikal" ->

c 202,[µV]			unit of data (e. g. "[µV]" -> (µV), max. A12)
c 210,EXPLIZIT			type of channel (IMPLIZIT -> implicite / 
c                               EXPLIZIT-> explicite)
c 211,S0912002.I32		name of waveform data file, s. below (*.I32 using the INT32 / 
c                               *. R64 using the REAL 64 format), file names with 12 chars.
c 213,BLOCK			description of data matrix as data block
c 214,INT32			data type (INT32 / REAL64), s. below
c 220,14801	 		length of data set (number of lines)
c 240,1				offset value
c 241,4				calibration factor (not used in conversion)
c #ENDCHANNELHEADER

c #BEGINCHANNELHEADER
c 200,NS				channel name (max. A16)
c 201,Seismometer nord-sued	comments to channel (here: "Seismometer nord-sued" -> 
c                               Seismometer north-south, max. A72)
c 202,[µV]			unit of data (e. g. "[µV]" -> (µV), max. A12)
c 210,EXPLIZIT			type of channel (IMPLIZIT -> implicite / 
c                               EXPLIZIT-> explicite)
c 211,S0912002.I32		name of waveform data file, s. below 
c 213,BLOCK			description of data matrix as data block
c 214,INT32			data type (INT32 / REAL64), s. below
c 220,14801			length of data set (number of lines)
c 240,0				offset value
c 241,2				calibration  factor (not used in conversion)
c #ENDCHANNELHEADER

c #BEGINCHANNELHEADER
c 200,EW				channel name (max. A16)
c 201,Seismometer ost-west	comments to channel (here: "Seismometer ost-west" -> 
c                               Seismometer east-west, max. A72)
c 202,[µV]			unit of data (e. g. "[µV]" -> (µV), max. A12)
c 210,EXPLIZIT			type of channel (IMPLIZIT -> implicite / 
c                               EXPLIZIT-> explicite)
c 211,S0912002.I32		name of waveform data file, s. below 
c 213,BLOCK			description of data matrix as data block
c 214,INT32			data type (INT32 / REAL64), s. below
c 220,14801			length of data set (number of lines)
c 240,0				offset value
c 241,2				calibration factor (not used in conversion)
c #ENDCHANNELHEADER
	
c #BEGINCHANNELHEADER
c 200,DCF				channel name (max. A16)
c 201,DCF77			comments to channel (here: "DCF77" -> regional radio time 
c                               code, max. A72)
c 202,[µV]			unit of data (e. g. "[µV]" -> (µV), max. A12)
c 210,EXPLIZIT			type of channel (IMPLIZIT -> implicite /
c                               EXPLIZIT-> explicite)
c 211,S0912002.I32		name of waveform data file, s. below (
c 213,BLOCK			description of data matrix as data block
c 214,INT32			data type (INT32 / REAL64), s. below
c 220,14801			length of data set (number of lines)
c 240,0				offset value
c 241,2				calibration factor (not used in conversion)
c #ENDCHANNELHEADER
c 
c ---------------------------------------------------------------------------------------------
c end before former line.
c Empty lines are allowed, comment lines begin with ",".
c 
c (*)	IMPLIZIT means that values are described via start and increment values
c       EXPLIZIT means that values are saved one by one
c (**)	Explicite channels are stored BLOCKwise in waveform data file

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c (2) Waveform data file 
c INT32 	(4-byte Integer format - PC, file extension I32) or 
c REAL64 	(8-byte Real format - UNIX, file extension R64), respectively, 
c both options should be supported.

c Data blocks in binary format (s. above)
c line 1: 	value1(channel UD) 	value1(channel NS) 	value1(channel EW) 	value1(channel DCF)
c line 2: 	value2(channel UD) 	value2(channel NS) 	value2(channel EW) 	value2(channel DCF)
c line 3: 	value3(channel UD) 	value3(channel NS) 	value3(channel EW) 	value3(channel DCF)
c ...		...			...			...			...
c line 14801: ... 			...			...			value14801(channel DCF)

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c (3) Filename structure
c 
c 1)  The station code is contained in the filenames of both, header and waveform data files. 
c The used filename structure can be read from the following rules:
c 
c  s mm dd nnn .ext
c  |  |  |  |    |
c  |  |  |  |    file extension (dat / i32 / r64)
c  |  |  |  event number in the same day (e. g. no. of triggered event) 
c  |  |  day
c  |  month
c  STATION CODE (in abbreviation according to station list)
c 
c List of station abbreviation (12 stations)
c 
c b	BHE
c e	ENT
c g	GSH
c j	JCK
c k	KRF
c l	LOH
c o	OLF
c p	PLH
c r	RWB
c s	SOR
c t	TDN
c w	WBS
c 
c In the mentioned example, i. e.: S0912002.I32 in channel header (line identification no. 211) 
c is the 2nd event file of 12 September at station SOR.
c Remark: Station code and extension will remain unchanged. Chars for date and event number
c may be used otherwise in future.
c 
c 2) All channels have the same start time (does not represent full seconds).
c 
c 3) Time channel must be converted, too, for test and proof purposes. 
c 
c --------------------------------------------------------------------------------------------
c 20/11/2006 	Klaus Lehmann
