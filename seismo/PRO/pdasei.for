c$DEBUG
c
c  Convert pdas files to seisan format
c  
c  At the end of the program is a pdas format description
c
c  j. havskov, december 1991
c
c
c  Carl Mulchay from Geotech Instruments has been very helpful in 
c  supplying format descriptions and test data.
c
c  updates:
c  jan 31 91 by jh: change to read 4 bytes integer a la federico
c  feb 2        cl: read 177 bytes dir access on sun
c  feb 3        cl: due to F77 problems a program pdabin is called and made to create
c  aug 23 93 jh   : filenames to 80
c                   the binary file from an ascii file: pdaasc.out
c  feb 1 94       : header length erroor on sun was 178, now 177
c  aug 95         : header length on pc now seem sto be 184 ????
c                   also fix so program works on pc without pdabin
c  sep 15, 95     : read only the sun way, so header lenght can be variable
c                   however a few samples are then lost at the end, change
c                   pc file names, divide numbers by 1000.
c  spring 96      : use the correct pdas format, the earlier divide by
c                   1000 was caused by having 2 byte or gainranged data
c                   when 4 byte data was assumed !!!!!!!!!!!!!!
c april 99 by jh   : ----------   verision 7.0 upggrade------------------
c august 31       : bugs that was never fixed
c jan 21          : add component to file name
c may 11  00   lo : use read_resp_head
c feb 03  03   jh : remove intand, now in compdecomp
c
c  The program can work with 2 byte, 4 byte and gain ranged data. 
c  It is assumed that channel 1,2 and 3 are SP Z, N and E respectively.
c  If not, change the program. There is no predefined codes for
c  channels 4-6, can be put in. Station codes can be given in an
c  input file pdasei.def, if no def file, the seiral number are used.
c
c  First the header is read with record
c  length 1 to find the length (mostly it seems 177), then the file is closed
c  and reopened with record length = header length. This means that the
c  part of the last record is lost corressponding to max e.g. 177/4 samples.
c  this can be avoided quite easily by reading reading one byte at a time, but
c  it is slowwwwww.
c
c
       implicit none
      include 'libsei.inc'
      include 'seidim.inc'
c-- one byte of data	  
      character*1 onebyte
c-- type of data bytes
      character*4 byte
c-- input data vector	  
      integer*4 data(max_sample),data_temp(max_sample)
      integer*2 data2(max_sample*2)
c-- input data text string, equivalent to data and data2
      character*1 text(max_sample*4)
      character*80 txt   ! general text string
c-- dummy text string used in read statement; Max. length of record=1000
      character*1000 dummy
c-- one sample	  
	  integer*4 x
c-- one sample in 4 bytes	  
	  character*4 swap
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  

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

c-- Pdas header	  
	  character*500 pdas_head
c-- date and time
      character*2 cyear,cmonth,cday,chour,cmin
c-- start sec
      character*6 csec
      character*1 acomp	  	  	  
c-- sampling interval
      real interval	  	  	  	  	  
c--Counters etc
	  integer isamp,i,k,ib,flag,ijk,nbytes,j,nrec
      integer gain   ! gain exponent when gain ranging
c-- computer type
      logical pc,sun,linux	  
	  equivalence (x,swap)
	  equivalence (data,text)
      equivalence (data2,text)


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)
c
      chour= '  '
      cmonth='  '
c
      write(6,*)' This program converts 2 byte, 4 byte and'
     *,' gain ranged data'
      write(6,*)
c
c   get pdas def file for station codes
c
      txt='pdasei.def'
      call read_def_chan(txt,mainhead_text,net_code)
c
c   get file name
c
      in=0
      question=' File name, # or filenr.lis for all'
      call filename(question,infile)	    
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis') then
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
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') infile
c
c  open file and read header
c
       open(1,file=infile,access='direct',recl=1,status='old')
       k=1		 
       ib=1
       flag=0	  
  1    continue
       read(1,rec=k) onebyte
       pdas_head(k:k)=onebyte
      if(onebyte.eq.':') flag=1
      if(k.gt.3.and.pdas_head(k-3:k).eq.'DATA'.and.flag.eq.1) then
         k=k+1
	     read(1,rec=k) onebyte
         k=k+1
	     read(1,rec=k) onebyte
         goto 2
      endif
	  k=k+1		 
	  goto 1 
 2    continue
c
c--  Close and open again, but now with record length equal the full header
c--  Also read header again, but this is later overwritten
      close(1)
      open(1,file=infile,access='direct',recl=k,status='old')
	  read(1,rec=1)dummy(1:k)
      write(6,*)' header length is ',k
c
c   write header
c	   	   	 
      write(6,'(1x,a)')	pdas_head(1:k-1) 
c
c   find date etc from header. Since values are found by finding
c   relevant chars, it should work also if header changes
c	   
       cyear=' '
       cmonth=' '
       cday=' '	   
       chour=' '
       cmin=' '
       csec=' '
       comp(1)='S   Z'
c-- station number used as name
       stat(1)(1:3)=pdas_head(11:13)
       stat(1)(4:5)='  '

       do i=4,k-1
c
c   format of samples
c
          if(pdas_head(i:i+8).eq.'FILE_TYPE') then
             if(pdas_head(i+10:i+12).eq.'INT') byte='two '  ! two byte data
             if(pdas_head(i+10:i+12).eq.'LON') byte='four'  ! four byte data
          endif
          if(pdas_head(i:i+6).eq.'COMMENT') then
              if(pdas_head(i+8:i+11).eq.'GAIN') byte='gain' ! gain ranged
          endif
c
c-- date and time
c
          if(pdas_head(i:i).eq.'-'.and.cmonth.eq.'  ') then
             cmonth=pdas_head(i-2:i-1)
             if(pdas_head(i+3:i+3).eq.'-') then
                cday=pdas_head(i+1:i+2)
                cyear=pdas_head(i+4:i+5)				
             else
                cday(2:2)=pdas_head(i+1:i+1)
                cday(1:1)=' '		
                cyear=pdas_head(i+3:i+4)
             endif
            read(cyear,'(i2)') year(1)
            if(year(1).lt.50) then
               year(1)=year(1)+2000
            else
               year(1)=year(1)+1900
            endif
            read(cmonth,'(i2)') month(1)
            read(cday,'(i2)') day(1)
         endif
          if(pdas_head(i:i).eq.':'.and.chour.eq.'  ') then
             chour=pdas_head(i-2:i-1)
             read(chour,'(i2)') hour(1)
             cmin=pdas_head(i+1:i+2)
             read(cmin,'(i2)') min(1)
             csec(2:6)=pdas_head(i+4:i+8)
             csec(1:1)=' '
             read(csec,'(f6.3)') sec(1)			 
          endif
c--- channel
          if(pdas_head(i-3:i).eq.'nnel') then
             acomp=pdas_head(i+1:i+1)		  
             if(pdas_head(i+1:i+1).eq.'1') comp(1)='S  Z'
             if(pdas_head(i+1:i+1).eq.'2') comp(1)='S  N'
             if(pdas_head(i+1:i+1).eq.'3') comp(1)='S  E'
          endif			   
c--- rate
          if(i.gt.10.and.pdas_head(i-3:i).eq.'RVAL') then
             read(pdas_head(i+1:i+7),'(f6.2)') interval
             if(interval.ne.0.0) rate(1)=1.0/interval
          endif
       enddo
c
c   put in definition from def file
c
         call set_def_chan(1,stat(1),comp(1))

c
c   read data, one header lenght of bytes
c   On sun swap is done by first storing byte 4 in 1 etc.
c	   		  	
       isamp=1
       ijk = 1
	   nrec = 2
 30    read(1,rec=nrec,err=40)dummy(1:k)
       nrec = nrec + 1
       do j = 1,k
         text(ijk) = dummy(j:j)
	     ijk = ijk + 1
	   enddo
	   go to 30
c		  
 40    continue
       write(*,*)' Reading finished with',nrec-2,' records of data'
	   write(*,*)' Number of bytes:',ijk-1
       write(*,*)' Type of data storage ', byte
       nbytes = ijk-1
       if(byte.eq.'four') isamp = nbytes/4
       if(byte.eq.'two ') isamp = nbytes/2
       if(byte.eq.'gain') isamp = nbytes/2
c
c   sometimes last block seems to be corrupted by the above reading routine
c   so throw away
c
       isamp=isamp-80
       cbyte(1)='4'
       if(byte.eq.'two ') cbyte(1)=' '
c
c  do swapping
c		  
       if(sun) then
          if(byte.eq.'four') then
		  do ijk = 1,nbytes,4
		     write(swap,'(4a1)')text(ijk),text(ijk+1),
     +       text(ijk+2),text(ijk+3)
             text(ijk)=swap(4:4)
             text(ijk+1)=swap(3:3)
             text(ijk+2)=swap(2:2)
             text(ijk+3)=swap(1:1)
		  enddo
          endif
          if(byte.eq.'two '.or.byte.eq.'gain') then
		  do ijk = 1,nbytes,2
		     write(swap,'(2a1)')text(ijk),text(ijk+1)
             text(ijk)=swap(2:2)
             text(ijk+1)=swap(1:1)
		  enddo
          endif
       endif
c
c   write seisan file, first make headers
c

 10   continue
c      write(6,*)year(1),month(1),day(1),hour(1),min(1),sec(1)
c      write(6,*)nsamp(1),rate(1),cbyte(1),net_code
      write(6,*) ' Number of samples',isamp 
c      read(5,*) i
c
c   make seisan headers
c
      nchan=1
      ichan=1
      nsamp(1)=isamp
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,nsamp,
     *                 rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   add componenets to make filenames different for different channels
c   and a number to put channels in right order when sorting
c
         if(comp(1)(4:4).eq.'Z') outfile(30:30)='1'
         if(comp(1)(4:4).eq.'N') outfile(30:30)='2'
         if(comp(1)(4:4).eq.'E') outfile(30:30)='3'
         outfile(31:34)=comp(1)
         do i=31,34
            if(outfile(i:i).eq.' '.or.outfile(i:i).eq.char(0)) 
     *      outfile(i:i)='_'
         enddo
         write(6,'(1x,a)') outfile(1:34)                                             
         write(6,*)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',outfile
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write data
c
      write(6,'(1x,a)')mainhead(1)(1:78)
      do i=1,12
        write(2)mainhead(i)
      enddo
c
c   decode gain ranging
c
      if(byte.eq.'gain') then
         do i=1,isamp
            data_temp(i)=data2(i)
            call intand(data_temp(i),3,gain)   ! find gain exponent
            data_temp(i)=data_temp(i)/4       ! strip off two low bytes
            data_temp(i)=data_temp(i)*8**(5-gain)  ! correct for exponent
c            write(16,*) gain,data2(i),data_temp(i)
         enddo
         do i=1,isamp
            data(i)=data_temp(i)
         enddo
      endif
c
c   read response curve into header
c

           call read_resp_head(chahead)                          

      write(2) chahead
      write(6,'(1x,a)') chahead(1:78)
      if(byte.eq.'four'.or.byte.eq.'gain') 
     *  write(2)(data(i), i=1,isamp)
      if(byte.eq.'two ') 
     *  write(2)(data2(i),i=1,isamp)
      write(6,*)
      close(1)
      close(2)

c
c  back for next file if many
c
      if(in.eq.1) goto 1000	  	  	  	  		 	     	  
      stop
      end	   	  	  	         	  	  
								  
								  
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                  PDAS-100 MANUAL EXCERPT                                      
c                                                                               
c                     DATA FILE FORMAT                                          
c                                                                               
c                                                                               
c 4.1.7 Data File                                                               
c                                                                               
c The data file has two parts, a record header and the collected                
c data for a channel.  These files are written to sub-directories               
c  on the PDAS-100 static RAM disk during acquisition.  Each channel            
c  being recorded has its own sub-directory, \CH0, \CH1....                     
c There are four types of files which can be recorded for a channel,            
c  each with its own letter designating type:                                   
c                                                                               
c      \CHj\Pjnnnsss.doy       Primary Window Record                            
c      \CHj\Sjnnnsss.doy       Secondary Window Record                          
c      \CHj\Cjnnnsss.doy       Calibration Window Record                        
c      \CHj\Ejnnnsss.doy       Event Record                                     
c                                                                               
c             j - channel number                                                
c           nnn - last three digits of PDAS-100 serial number                   
c           sss - sequence number                                               
c           doy - day of year of start of file                                  
c                                                                               
c                                                                               
c                                   NOTE                                        
c                                                                               
c The sequence number for Primary windows and for Calibra-tion                  
c windows use the same counting sequence.  That is to say if the                
c first recording of the day was a primary window, it would have                
c sequence number 001.  If the second and third recordings for                  
c that day were calibrations they would be sequence numbers 002                 
c and 003.  If the next recording was a primary window it would                 
c have sequence number 004.                                                     
c                                                                               
c                                                                               
c 4.1.7.1  Data File Header                                                     
c                                                                               
c The first part of the data file is a text listing defining the contents of the
c file.  In particular, this text is in DADiSP import format.  An example data  
c file header follows.                                                          
c                                                                               
c      DATASET  P1001001134                                                     
c      FILE_TYPE  LONG                                                          
c      VERSION next                                                             
c      SIGNAL  channel 1                                                        
c      DATE  5-13-88                                                            
c      TIME   17:59:00.00  (000)                                                
c      INTERVAL  0.010                                                          
c      VERT_UNITS    Counts                                                     
c      HORZ_UNITS   sec                                                         
c      COMMENT   None                                                           
c      DATA                                                                     
c                                                                               
c Definitions for each of the lines in the data file header follow:             
c                                                                               
c                                                                               
c DATASET                                                                       
c                                                                               
c The data set is the DOS file name assigned to the data file during            
c acquisi-tion.  Note that the file extension has been moved to the             
c left of the decimal point.                                                    
c                                                                               
c                                                                               
c FILE TYPE                                                                     
c                                                                               
c The file type defines the method by which the data was recorded and           
c  is stored in the DOS file.                                                   
c                                                                               
c      INTEGER      16-bit (2 byte) FG data samples                             
c         LONG      32-bit (4 byte) data samples                                
c         LONG      14/2 (2 byte) GR data samples                               
c               (see COMMENT also)                                              
c                                                                               
c                                                                               
c VERSION                                                                       
c                                                                               
c The 'next' parameter in this field causes DADiSP to find the highest          
c existing version of the Dataset Name and sets the new version to the          
c next higher value.                                                            
c                                                                               
c                                                                               
c SIGNAL                                                                        
c                                                                               
c This field contains the name given to the channel when the ASP screen         
c was defined during the experiment configuration file creation.                
c                                                                               
c                                                                               
c DATE                                                                          
c                                                                               
c This field contains the date that the data file was recorded.                 
c                                                                               
c                                                                               
c TIME                                                                          
c                                                                               
c This field contains the time for the first sample in the                      
c data                                                                          
c file.  It is recorded in 24 hour format with millisecond                      
c resolution.  The PDAS starts each recording block at the                      
c 10 millisecond mark.  For event files, 1 to 9 milliseconds                    
c are added to the pre-event length to place the start of the                   
c data set on a 10 millisecond mark.                                            
c                                                                               
c                                                                               
c INTERVAL                                                                      
c                                                                               
c This field will reflect the chosen sample rate for the data                   
c channel.  It is provided as sample interval with the units                    
c of seconds.                                                                   
c                                                                               
c                                                                               
c VERT UNITS                                                                    
c                                                                               
c The vertical units for the data will always be counts.                        
c                                                                               
c                                                                               
c HORZ UNITS                                                                    
c                                                                               
c The horizontal units for the data will always be sec. (The                    
c abbreviation for seconds).                                                    
c                                                                               
c                                                                               
c COMMENT                                                                       
c                                                                               
c The information in this field will vary depending on how the data             
c was recorded within the PDAS.  The following information describes            
c what will be in the COMMENT field based on what recording mode was            
c selected.                                                                     
c                                                                               
c                                                                               
c      COMMENT                    DATA FILE TYPE                                
c                                                                               
c      NONE          16 bit (2 byte) FG data samples (INTEGER)                  
c                    32 bit (4 byte) FG data samples (LONG)                     
c       GAINRANGED   14/2 (2 byte) GR. data samples 14/2                        
c (LONG)                                                                        
c     DEGAINRANGED    32 BIT (4 byte) FG data samples (LONG)                    
c                     which were acquired an 14/2 GR samples                    
c and                                                                           
c                     degain-ranged using the DRANGE program.                   
c                                                                               
c                                                                               
c                                 NOTE                                          
c                                                                               
c The data file header is variable in length.  The actual channel data          
c is preceded bythe following string:                                           
c                                                                               
c                          DATA<CR><LF>                                         
c                                                                               
c A suggested method for searching for the channel data is to                   
c perform a string search for the word DATA, then skip two                      
c bytes.  The balance of the channel data file is data in one                   
c of the formats described In the following section.                            
c                                                                               
c                                                                               
c 4.1.7.2  Data File Data                                                       
c                                                                               
c The second part of the data file is the digitized data                        
c collected for the channel.  This is stored in binary                          
c according to three formats.                                                   
c                                                                               
c       Two byte integer                                                        
c       Four byte Integer                                                       
c       Two byte gain range                                                     
c                                                                               
c In all-cases the bytes are organized in 'INTEL" format.                       
c INTEL format for 16 bit data words (as well as 32 bit LONGS)                  
c assumes that the most significant byte is at the highest                      
c memory location.  Bits within the byte follow the convention                  
c of most significant bit being the lift most bit and least                     
c significant bit being the right most.                                         
c                                                                               
c Use of the two byte recording formats increases the length of                 
c data which may be recorded on the PDAS-100, while use of the                  
c four byte integer format provides the most resolution.                        
c                                                                               
c                                                                               
c 16-BIT DATA FORMAT                                                            
c                                                                               
c The two byte integer is stored in two's complement format.                    
c To convert this count to voltage, use the following formula:                  
c                                                                               
c       Let m be the digitized value in counts.                                 
c       Let p be the preamp gain.                                               
c       Let v be volts.                                                         
c                                                                               
c    Then:  v = m / (32768 * p)      high gain inputs                           
c           v = 20 * m / (32768 * p)       low gain inputs                      
c                                                                               
c (Preamp gain (p) = 1, 10 or 100 as specified in ASP set-up.)                  
c                                                                               
c                                                                               
c 32-BIT DATA FORMAT                                                            
c                                                                               
c The four byte integer is also stored in two's complement format.              
c To convert this count to voltage, use the following formula:                  
c                                                                               
c      Let a be the digitized value in counts.                                  
c      Let v be volts.                                                          
c                                                                               
c    Then: v = m / 2147483648        high gain inputs                           
c          v = 20 * m / 2147483648 low gain inputs                              
c                                                                               
c                                                                               
c 14/2 GAIN RANGED FORMAT                                                       
c                                                                               
c The two byte gain ranged format uses a 14/2 gain range.  The gain             
c code is stored in the two least significant bits of the 16 bits,              
c and the digitized value is in the upper 14 bits of the 16 bits.               
c                                                                               
c      Let g be the gain code from the lower two bits.                          
c      Let p be the preamp gain.                                                
c      Let m be the value (two's complement) in upper 14.                       
c      Let v be volts.                                                          
c                                                                               
c    Then:  v = m * (8**(5-g)) / (268435456 * p)        high gain inputs        
c           v = 20 * (m * (8**(5-g))) / (268435456 * P) low gain inputs         
c                                                                               
c The digitized samples are stored packed in the rest of the data file          
c until the end of file is reached.                                             
c 
