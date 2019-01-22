c
c    transform Willy Lee binary files to seisan  data files
c
c    M. Villagran and J. Havskov, Dec 1991
c
c
c
c  this progrma has some problems on sun with what is written in bianry file
c
c    latest update
c    april 93 fix up, check on pc
c    august 93 by jh: filename to 80 chars
c    sep 99 jh change to 7, only change call to computer type
c    may 11  00    lo : use read_resp_head
c
       implicit none
c-- seisan  data file channel header
      character*1040    chahead
c-- -------------------main --------
      character*960     mainhead
c-- output file name
      character*80	outfile		
c-- save part of header
      character*50      save_head	
c-- data vector, input
      integer*2		data(50000)	
c-- input file name
      character*80 infile
c-- question
      character*80 question
c-- number of channels
      integer nchan
c-- total time window
      real              total_time	
c-- help variables
      integer		i,k,in,j
      logical pc,sun,linux
c
c special variables from old wl routine
c
      integer mblo,iscan,ichan,no,nblock,mi,me
      real srate,xrate
      integer imue,iblock
      CHARACTER*4   STAT(16)
      CHARACTER*20  fecha
      INTEGER*2     ZBU2(256)
      INTEGER*2     ILENGHT,SCAN,MAGIC
      character*512 buf
      character*2   sw2
      character*1 b1,b2	   
      character*4   sw4	  	  
      integer*2 i2
      integer*4 i4	  	  
      real re	  
      equivalence   (sw2,i2),(sw4,i4),(sw4,re),(buf,zbu2)	  


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

        call computer_type(sun,pc,linux)
c
C -------------------------------------------------------------------
C       Section to read w. lee files.
C
C        INPUT          The input file is a binary file with
C                       blocks of 512 bytes. This file is the
C                       Version 1.0 output file from Willie Lee
C                       seismic detection system.
C
C        OUTPUT         The output file is a seisan  
c                       binary file
C -------------------------------------------------------------------
c
c  get file name
c
       question='  Filename, ?, or number'
       call filename(question,infile)

c
c  check for no files 
c
      if(infile(1:3).eq.'EOF') stop
c
c   check which type of input file
c
      in=0
      if(infile(1:10).eq.'filenr.lis')then
         open(8,file='filenr.lis',status='old')
         in=1
      endif
c
c  back here if many binary files to convert using filenr.lis
c
 10   continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         write(6,'(1x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
c
c  open  binary willy lee file
c
c          i=index(infile,' ')-1
          OPEN(2,FILE=infile,ACCESS='DIRECT',
     +        RECL=512,STATUS='OLD')
C
C  question about the time of the earthquake in transform process.
C
        write(6,*)' Please give the date of the ',
     +  'earthquake'
        write(6,*)
     * 'YR MO DY HR MN SS.SS (91 12 24 23 59 11.21 is an example)'
        read(5,'(a20)')fecha
        write(6,*)' Sample rate, if 0, 100.81 is used'
        read(5,*) xrate
        srate=xrate
        if(xrate.eq.0.0) srate=100.81
c
c ----    Read first and second blocks
c
c
c   this section not really used since values are hardwired below
c
           sw2=buf(1:2)
           magic=i2
           sw2=buf(3:4)
           ilenght=i2
           sw2=buf(5:6)
           scan=i2
           sw4=buf(7:10)
c           srate=re
c           write(6,*)  magic,ilenght,scan,srate
           magic=255
c           ilenght=1024
c
c  this was different before
c
           ilenght=512
           scan=16
c
c   get stations
c		   
           k=139		   
           do i=1,16
             stat(i)=buf(k:k+3)		   		   
             k=k+24			 
           enddo			 
c
c   read next buffer of header
c		   
           read(2,rec=2) buf
C
C ---      STOP IF THE VERSION IS WRONG should be 1.0
C 255 is a parameter defined by the W. Lee data acquisition
C
           IF (MAGIC.NE.255) THEN
               PRINT*,'   ****   WRONG FORMAT VERSION  **** '
               PRINT*,'   ****      SKIP OR FINISH     **** '
            END IF
           IF(MAGIC.NE.255)stop
		   
c
c  make file name to pc or sun
c
        if(sun)then
c-- year
         outfile(1:2)=fecha(1:2)
c-- month
         outfile(3:4)=fecha(4:5)		
         outfile(5:5)='-'
c-- day
         outfile(6:7)=fecha(7:8)		
         outfile(8:8)='-'
c-- hr
         outfile(9:10)=fecha(10:11)		
c-- min
         outfile(11:12)=fecha(13:14)		
         outfile(13:13)='-'
c-- sec
         outfile(14:15)=fecha(16:17)		
         outfile(16:17)='S.'
c-- network or station code
         outfile(18:20)='UCR'		
         outfile(21:21)='_'
c-- number of channels
         outfile(22:23)='16'
c-- check for blanks
         do 188 i=1,23
            if(outfile(i:i).eq.' ') outfile(i:i)='0'
188      continue
         write(6,200) outfile(1:23)
 200     format(/,' Output file name is: ',a23)
      endif
c
c  make filename if on pc, is shorter than vax and sun
c
         if(pc) then
c-- year
            outfile(1:2)=fecha(1:2)
c-- month
            outfile(3:4)=fecha(4:5)		
c-- day
            outfile(5:6)=fecha(7:8)		
c-- hr
            outfile(7:8)=fecha(10:11)
	    outfile(9:9)='.'		
c-- min
            outfile(11:12)=fecha(13:14)		
c-- network or station code
            outfile(10:10)='L'		
c-- check for blanks
            do 189 i=1,12
               if(outfile(i:i).eq.' ')outfile(i:i)='0'
189         continue
            write(6,207) outfile(1:12)
 207        format(/,' Output file name is: ',a12)
         endif
c
        i=index(outfile,' ')-1
        open(9,file=outfile(1:i)
     * ,form='unformatted',status='unknown')
c
c   construct  main header
c
         do 190 i=1,960
           mainhead(i:i)=' '
190      continue
         mainhead(2:18)='                 '
         mainhead(1:23)=outfile(1:23)
c-- no of channels
         mainhead(32:33)='16'		
c-- year, date and time
         mainhead(35:36)=fecha(1:2)
         mainhead(42:43)=fecha(4:5)
         mainhead(45:46)=fecha(7:8)
         mainhead(48:49)=fecha(10:11)
         mainhead(51:52)=fecha(13:14)
         mainhead(55:59)=fecha(16:20)
         j=162
         k=51
         nchan=16
C
C --- CALCULATE PARAMETERS TO READ THE WAVE FORM DATA
C
C     MBLO  = BLOCKS OF 256 WORDS PER BUFFER
C     ISCAN = BLOCKS TO SKIP TO GET TO THE NEXT BUFFER OF SAME CHANNEL
C

        MBLO = ILENGHT/256
        ISCAN = SCAN*MBLO

C
C ---  get time inside binary Lee file
C
C               this step is missing until now,
C               the program only works if you
C               give the date
			 
C-----------------------------------------------------------------
C ----     Read seismic data and write out in loop
C-----------------------------------------------------------------
c
         do ichan=1,16
             imue = 0
             iblock = 3 + (ICHAN-1)*MBLO
             do 130 no = 1,1000
                nblock = iblock + (mblo-1)
                do 120 mi = iblock,nblock
                    read(2,rec=mi,err=140) buf
c
c   do byte swap if on sun
c	
                    if(sun) then
                       do i=1,511,2
                         b1=buf(i:i)
                         b2=buf(i+1:i+1)
                         buf(i:i)=b2
                         buf(i+1:i+1)=b1				   
                       enddo
                    endif					    						 
                    do 110 me = 1,256
                        imue = imue + 1
                        data(imue) = zbu2(me)-2048
 110                continue
 120             continue
                 iblock = iblock+iscan
 130          continue
 140          continue
c
c   write main header if this is first channel, must be done here since
c   number of samples is not given in wl main header
c
         if(ichan.eq.1) then
            total_time=imue/srate	
            write(mainhead(61:69),'(f9.3)') total_time
            do 191 i=1,nchan
c--  station and component
               mainhead(j:j+3)=stat(i)
               mainhead(j+4:j+7)='S  Z'	
               mainhead(j+8:j+8)=' '
c-- rel. start time of channel
               mainhead(j+9:j+15)='    0.0'	
               write(mainhead(j+17:j+24),'(f8.2)') total_time
               if(i.eq.3.or.i.eq.6.or.i.eq.9.or.i.eq.12.or.i.eq.15.
     *         or.i.eq.18.or.i.eq.21.or.i.eq.24.or.i.eq.27.or.i.eq.30)
     *         then
                 j=j+28
               else
                 j=j+26
               endif
               k=k+11
191         continue
c
c   write  main header
c
            k=1
            do 192 i=1,12
               write(9)mainhead(k:k+79)
               k=k+80
192         continue
          endif

 
c
c   make channel header and write
c
            do 196 i=1,1040
               chahead(i:i)=' '
196         continue
            j=(ichan-1)*11+51
c-- station code
            chahead(1:4)=stat(ichan)		
c-- component
            chahead(6:9)='S  Z'	
c-- year, date and time
            chahead(11:35)=mainhead(35:59)		
            write(chahead(37:43),'(f7.2)')srate
c-- number fo samples			
            write(chahead(45:50),'(i6)') IMUE
c
c   get response, save date etc since removed when calling read_resp
c
            call read_resp_head(chahead)
c
c   write header
c
             write(6,'(1x,a60)') chahead(1:60)
             write(9) chahead
c
c   write data
c
             write(9)(DATA(I),I=1,imue)
          enddo
    	close(9)
        close(2)

        if(in.eq.1)goto 10
c----------------------------------
c end channel loop
c-----------------c		  
c
c
        STOP
        end
