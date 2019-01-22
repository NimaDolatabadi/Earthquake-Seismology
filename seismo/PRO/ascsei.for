c                                                                               
c    transform an ascii file to seisan data file. the input file only contains
c    one column of samples 
c                                                                               
c    j. havskov jan 96                    
c    may 11  00    lo : use read_resp_head
c    feb 02  03    jh : small clean up
c    sep 13           : add input of psn ascii
c    dec 17 2003   jh : fix channel header year, no century
c    aug 30, 2005  jh : fix filename and network code
c    oct 18  2005     : put in scale factor
c    sep  3, 2009  pv : bug, first sample was lost if data was not PSN data, fixed with rewind
c    jan 6   2012  jh : write out number of samples, error message
c
c    latest update                                                              
c
      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
c                                                                               
c-- seisnor data file channel header      
      character*1040    chahead         
c-- -------------------main --------      
      character*80     mainhead(max_trace)
      character*80     text        
c-- date and time
      integer year,month,day,hour,min
      real sec
c-- output file name                                  
      character*80	outfile		
      character*80  question
      character*80  infile
c      character*50      save_head	
c-- stations and components
      character*4 comp(max_trace)
      character*5 stat(max_trace)
c-- data vector, one buffer or one channel                              
      integer*4     data4(max_sample)
      real          x(max_sample)
      real          factor		! scale factor



c-- data interval each channel, start time
      real cinter(max_trace),cstart(max_trace)
c-- number of channels and samples/chan, total number of samples           
      integer nchan,nsamp
c-- sample rate                                              
      real		rate		
c-- help variables                                     
      integer		i,in
 
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver


c
c   get file name
c
      in=0
      question=' File name, # or filenr.lis for all'
      call filename(question,infile)	    
      if(infile.eq.'EOF') stop
      if(infile(1:10).eq.'filenr.lis'.or.infile(1:10).eq.
     *'FILENR.LIS') then
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
c  open file 
c
       open(1,file=infile,status='old')

c
c   check if a psn ascii file
c
       read(1,'(a)' )text
       if(text(1:5).eq.'! PSN') then
          read(1,'(12x,i4,4(1x,i2),1x,f6.3)') 
     *    year,month,day,hour,min,sec
          read(1,'(a)') text
          read(1,'(4x,i4)') i
          rate=i
          do i=1,7
            read(1,'(a)') text
          enddo
          read(1,'(16x,a5)') stat(1)
          if(stat(1).eq.' ') stat(1)='STAT '
          do i=1,4
             read(1,'(a)') text
          enddo
          write(6,'(a)') text
          comp(1)='S   '
          read(1,'(20x,a1)') comp(1)(4:4)
          do i=1,12
            read(1,'(a)') text
          enddo
       else
c
c   get parameters
c
          rewind(1)
          write(6,*) 'year,month,day,hour,min,sec'
          read(5,*) year,month,day,hour,min,sec
          write(6,*)'sample rate'
          read(5,*) rate
          write(6,*)'station, max 5 chars' 
          read(5,'(a5)') stat(1)
          write(6,*)'component, max 4 chars' 
          read(5,'(a4)') comp(1)
       endif
       factor=1.0
       write(6,*)' Enter scale factor, return for no scaling'
       read(5,'(a)') text
       if(text.ne.' ') then
         call sei get values (1,text,i) 
         factor=array$(1)
       endif
       
c
c   read data
c
       nsamp=1
 222   continue
       read(1,*,end=333,err=433) x(nsamp)
       goto 434
 433   continue
          write(6,*) 'Errror reading sample #: ',nsamp
          stop
 434   continue

c      write(6,*) x(nsamp),factor
       data4(nsamp)=x(nsamp)*factor
       nsamp=nsamp+1
       goto 222
 333   continue
       nsamp=nsamp-1
       write(6,*)'Number of samples in file: ',nsamp
       cinter(1)=(nsamp-1)/rate

c                                                                               
c  make file name and main header
c             
         nchan=1
         call mfhead(year,month,day,hour,min,sec,cinter(1),nchan,
     *   stat(1),stat,comp,cstart,cinter,outfile,mainhead)
c
c   different components could be at the same time, add
c   component id to file name
c
         outfile(30:30)='_'
         outfile(32:32)=comp(1)(4:4)
         outfile(31:31)=comp(1)(1:1)
         do i=30,32
            if(outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         write(6,'(a,a)') ' Outfile name: ',outfile(1:32)
c
c  open file
c
         open(2,file=outfile,status='unknown',form='unformatted')
c                                                                               
c   write  main header                                       
c                                                                               
         write(6,'(a)') mainhead(1)(1:75)
         do i=1,12
            write(2) mainhead(i)                                         
         enddo
c                                                                               
c  write channel with channel header
c                                                                               
            do i=1,1040                                                         
               chahead(i:i)=' '                                                 
            enddo                                                               
c-- station code                          
            chahead(1:5)=stat(1)
c-- componnet
            chahead(6:9)=comp(1)	
c-- year, date and time                  
            chahead(10:35)=mainhead(1)(34:59)		
            write(chahead(37:43),'(f7.2)') rate                          
            write(chahead(45:50),'(i6)') nsamp                                  
c                                                                               
c   get response
c
             call read_resp_head(chahead)       
c
c   indicate 2 or 4 byte integer in header
c
             chahead(77:77)='4'
c                                                                               
c   write header                                                                
c                                                                               
            write(2)chahead                                                     
            write(6,'(1x,a70)') chahead(1:70)                                   
c  
c   write data
c
            write(2)(data4(i),i=1,nsamp)
            close(2)
c
c   back for next file
c
            if(in.eq.1) goto 1000                                                                    
 99   continue                                                                  
      stop                                                                      
      end                                                                       


