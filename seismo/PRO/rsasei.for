c                                                                               
c    transform rsa binary files to seisan data files
c    program works with 2 types from the network and the bb files
c
c    network: old format: parameter file must be *.dts, waveform file *.sad
c             new format: parameter file must be *.ds1, waveform file *.sa1
c                                                *.ds3                *.sa3
c    bb data: file nam emaust be *.hdr
c
c    j. havskov, 1992, 2000
c
c    updates:
c    sep 20 jh  : add type ds3
c    oct 17 jh  : fix for antactic dts format
c    dec 20     . add array char to file name
c    feb        : faster reading on sun, do as on pc
c                 fix bb reading for upper and lower case file name
c    mar          increase dimension
c    dec 18 2010 jh: change form=binary to access = stream, not tested
c                        
c
c    Parameter in defines type of file
c
c    in=0 : One binary file, work only on sun or pc due to byte swapping on vax
c    in=1 : Several ------, use filenr.lis as input file name
c                                                                               
c    latest update                                                              
c                                                                               
      implicit 	none
      include 'seidim.inc'
c
c-----------------------------------------------------------------------
c   start block of definitions for most conversion programs
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
c
c--end block of definitions most conversion programs
c---------------------------------------------------------------------

c-- data vector, input and output                              
      integer*2  y(3000000),data(200000)
      integer*4  data4(200000)
c-- input file name
      character*80 infile
c-- event number in
      integer number
c-- total number of samples in all channels
      integer ntotal	  
c-- question
      character*80 question
c-- total time window                          
      real              total_time	
c-- help variables                                     
      integer           i,k,ib,in,j,dtemp,irec
c-- -------------                                    
      logical pc,sun,linux
c- RSA parameters-----------------------------------------------------	  
      integer*4 day1,month1,year1,hour1,min1,isec
      integer*4 max(32),ofset(32)
      real sec1,fsamp
      integer*2 mue(16)	  
      character*1 c	  
      integer*4 nmues
      character*5 stcod(32)
      character*80 fsad,fdts,fnor   !nombres de ficheros incluida via
      character extdts*4,extsad*4,extnor*4,via*60,filein*8
      logical bb     ! indicate bb station
      character*80 text
      common /dts/nchan,day1,month1,year1,
     *hour1,min1,sec1,max,ofset,fsamp,
     +nmues,stcod,fsad,fdts,fnor
c---------------------------------------------------------------------	 



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   get def file for station codes, give file name
c
       text='rsasei.def'
       net_code=' '
       call read_def_chan(text,mainhead_text,net_code)
	  
c
c  initially assumes one binary file
c
      in=0
c
c   set computer type
c
      call computer_type(sun,pc,linux)
c
c  get file name
c
      question='  Filename, ?, # or filenr.lis for all'
      call filename(question,infile)
c
c  check for no files
c
      if(infile(1:3).eq.'EOF') goto 99
c
c   check which type of input file
c
      if(infile(1:10).eq.'filenr.lis') then
         open(8,file='filenr.lis',status='old')
         in=1
      endif
c
c  back here if many binary files to convert using filenr.lis
c
 1    continue
      if(in.eq.1) then
         read(8,'(2x,i3,2x,a)') number,infile
         write(6,*)
         write(6,'(1x,i3,3x,a)') number,infile
         if(infile(1:4).eq.'    ') goto 99
      endif
c
c   open and read header file
c
      write(6,'(1x,a)') infile	  
      open(1,file=infile,status='old')
c
c   old format
c

      if(infile(10:12).eq.'DTS'.or.infile(10:12).eq.'dts') then
         call leedts
         infile(10:12)='SAD'
      endif
c
c new format
c
      if(infile(10:12).eq.'DS1'.or.infile(10:12).eq.'ds1'.
     *or.infile(10:12).eq.'DS3'.or.infile(10:12).eq.'ds3') then
         call leedt1
         infile(10:11)='SA'
      endif
c
c   bb format
c
      bb=.false.
      if(infile(10:12).eq.'HDR'.or.infile(10:12).eq.'hdr')then
         bb=.true.
         call leedt2
      endif
c                                                                               
c  fill in variables in common block 
c
      if(.not.bb) then
         if(year1.gt.50) year1=year1+1900
         if(year1.le.50) year1=year1+2000
      endif
      do i=1,nchan
         year(i)=year1
         month(i)=month1
         day(i)=day1
         hour(i)=hour1
         min(i)=min1
         sec(i)=sec1
         nsamp(i)=nmues
         rate(i)=fsamp
         cbyte(i)=' '
         if(bb) cbyte(i)='4'
         stat(i)=stcod(i)
         comp(i)='S  Z'
         if(bb.and.i.eq.1) comp(i)='B  Z'
         if(bb.and.i.eq.2) comp(i)='B  N'
         if(bb.and.i.eq.3) comp(i)='B  E'
      enddo
c
c  enter loop to define channels if a def file
c
       do i=1,nchan
          call set_def_chan(i,stat(i),comp(i))
       enddo

c
c   make seisan headers
c
      ichan=1      ! only main head
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,stat,comp,
     *                 nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   specially mark array data
c
      if(infile(8:8).eq.'G'.or.infile(8:8).eq.'N'.or.infile(8:8)
     *.eq.'M'.or.infile(8:8).eq.'F') outfile(30:30)=infile(8:8)
c
c   open output file
c
      write(6,'(a,a)')' Output file name is: ',outfile
      open(2,file=outfile,status='unknown',form='unformatted')	  
c
c   write main head
c
      mainhead(1)(2:29)=mainhead_text    ! put in header text
      do i=1,12
         write(2)mainhead(i)
         write(6,'(a80)') mainhead(i)
      enddo
c                                                       
c   check if not too many samples
c
         if(nmues.gt.200000) then
            write(6,*)' Too many samples/chan, nsamp= ',nmues
            stop
         endif			 
c
c   open input binary file if not bb
c
         if(.not.bb) then
c           if(.not.pc) then
c             open(1,file=infile,status='old',access='direct',recl=2)
c             open(1,file=infile,status='old',access='direct',
c    *        recl=nmues*2)
c           else
              open(1,file=infile,status='old',access='stream')  ! gfortran change
c              open(1,file=infile,status='old',form='binary')
c           endif
         endif
c
c   network file
c
         if(.not.bb) then
c
c   read whole file into one array
c
            irec=0
            k=0    ! new
            do j=1,nmues
c              if(pc) then
                  read(1)(mue(i),i=1,nchan)
c              else
c                 do i=1,nchan
c                    irec=irec+1
c                    read(1,rec=irec)mue(i)
c                 enddo
c              endif
               do i=1,nchan
                  k=k+1                                       
                  y(k)=mue(i)                                         
               enddo
            enddo
            close(1)
            ntotal=k
         endif
c
c   channel write loop
c
       do ichan=1,nchan
c
c   make channel header	
c
         chahead=' '
         call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                    net_code,mainhead_text,stat,comp,
     *                    nsamp,rate,cbyte,
     *                    outfile,mainhead,chahead)
         write(6,'(1x,a)') chahead(1:78)
c
c   get response info
c
        call read_resp_head(chahead)                                            
c
c   write header
c
         write(2)chahead
c                                                                            
c
         if(bb) then
            do i=11,12
              infile(i:i)=char(0)  ! sun did not like blanks in file name
            enddo
            if(ichan.eq.1) infile(10:10)='z'
            if(ichan.eq.2) infile(10:11)='ns'
            if(ichan.eq.3) infile(10:11)='ew'
            open(1,file=infile,status='old',access='direct',
     *      recl=4,err=888)
            goto 889
 888        continue
            if(ichan.eq.1) infile(10:10)='Z'
            if(ichan.eq.2) infile(10:11)='NS'
            if(ichan.eq.3) infile(10:11)='EW'
            open(1,file=infile,status='old',access='direct',recl=4)
 889        continue
            write(6,'(a,a)')' Opened file: ',infile
            do i=1,nsamp(1)
               read(1,rec=i) data4(i)
            enddo
            close(1)
            if(sun) call swap4(nsamp(1),data4)   ! swapping
            write(2)(data4(i),i=1,nsamp(1))
         else
            j=0
            do k=ichan,ntotal,nchan
               j=j+1
               data(j)=y(k)
            enddo
            if(sun) call swap2(nsamp(1),data)
            write(2)(data(k),k=1,nsamp(ichan))
         endif
      enddo
      close(2)                       
c
c  end read for one event
c                                                                               
c                                                                               
c   back for next event if a list of events are transformed
c                                                        
      if(in.eq.1) goto 1          
c                                                                               
c   end of data                                                                 
c                                                                               
 99   continue                                                                  
      stop                                                                      
      end                                                                       
c
cccccccccccccccccccc--------------ccccccccccccccccccccccccccc

 
      SUBROUTINE leedts()
c
c   read header for dts file
c
c   not clear who wrote the original routine
c
      include 'libsei.inc'
      integer*4 nchan,day,month,year,hour,min,max(32),ofset(32)
      integer*4 nmues
      character*5 stcod(32)
      character*80 fsad,fdts,fnor   !nombres de ficheros incluida via
 
      common /dts/nchan,day,month,year,hour,min,sec,max,ofset,fsamp,
     +            nmues,stcod,fsad,fdts,fnor 
      character*5 cod
      integer*4 maxi,code
      character*80 linea
      read (1,'(a80)') linea
      call remove_letters(linea)
      call sei get values(1,linea,code)
      nchan=array$(1)
      
      write (6,*) 'nchan= ',nchan
      read (1,'(a80)') linea
      call remove_letters(linea)
      call sei get values(1,linea,code)
      nmues=array$(1)

      write (6,*) 'nmues= ',nmues
      read (1,'(a80)') linea
      read (1,'(a80)') linea
      read (linea,'(I2,1X,I2,1X,I2)') year,month,day
      write (6,*) 'Date ',day,month,year
      read (1,'(a80)') linea
      call remove_letters(linea)
      write(6,*) linea
      read (1,'(a80)') linea
      call remove_letters(linea)
      write(6,*) linea
      read (linea,'(I2,1X,I2,1X,F5.2)') hour,min,sec
      write (6,*) 'hour ',hour,'h',min,'m',sec,'s'
      read (1,'(a80)') linea
      call remove_letters(linea)
      call sei get values(1,linea,code)
      fsamp=array$(1)

c      read (linea, '(F3.0)') fsamp
      write (6,*) fsamp,' sps'
      read (1,'(a80)') linea
      read (1,'(a80)') linea
         do 66 i=1,nchan
            read (1,'(a80)') linea
            read (linea,'(A4,18X,I4)') cod, maxi
c            stcod(i)=linea(:4)
            stcod(i)=cod
            max(i)=maxi
            write (6,*) stcod(i),'max ',max(i)
66       continue
c      close (1)
      end            


      SUBROUTINE leedt1()
      include 'libsei.inc'
      integer*4 nchan,day,month,year,hour,min,max(32),ofset(32)
      integer*4 nmues
      character*5 stcod(32)
      character*80 text
      character*80 fsad,fdts,fnor   !nombres de ficheros incluida via
 
      common /dts/nchan,day,month,year,hour,min,sec,max,ofset,fsamp,
     +            nmues,stcod,fsad,fdts,fnor 
      character*5 cod
      character*80 linea
      integer*4 maxi,code
c
c  number of channels
c
      read (1,'(a)') text
      call sei get values(1,text,code)
      nchan=array$(1)
c
c   number of samples
c
      read (1,'(15x,a)') text
      call sei get values(1,text,code)
      nmues=array$(1)
c
c   sample rate
c
      read (1,'(15x,a)') text
      call sei get values(1,text,code)
      fsamp=array$(1)

c
c   time and date
c
      read (1,'(a)') text
      do i=1,80
         if (text(i:i).eq.'/'.or.text(i:i).eq.':') text(i:i)=' '
      enddo
      write(6,*) text
      call sei get values(6,text,code)
      year=array$(1)
      month=array$(2)
      day=array$(3)
      hour=array$(4)
      min=array$(5)
      sec=array$(6)

      write (6,*) 'fecha ',day,'-',month,'-',year
      write (6,*) 'hora ',hour,'h',min,'m',sec,'s'
      read(1,'(a)') text
         do 66 i=1,nchan
            read (1,'(a)') stcod(i)
c            write (6,*) stcod(i)
66       continue
      end            
      
      SUBROUTINE leedt2()
c
c  for bb files
c
      include 'libsei.inc'
      integer*4 nchan,day,month,year,hour,min,max(32),ofset(32)
      integer*4 nmues
      character*5 stcod(32)
      character*80 text
      character*80 fsad,fdts,fnor   !nombres de ficheros incluida via
 
      common /dts/nchan,day,month,year,hour,min,sec,max,ofset,fsamp,
     +            nmues,stcod,fsad,fdts,fnor 
      character*5 cod
      character*80 linea
      integer*4 maxi,code
      integer test
c
c  number of channels
c
      nchan=3

      do i=1,8
         read(1,'(a)') text
      enddo
c
c   sample rate
c
      read (1,'(24x,a)') text
      do i=1,40
         if(text(i:i).eq.'m'.or.text(i:i).eq.'p'.or.text(i:i).eq.'s')
     *   text(i:i)=' '
      enddo
      call sei get values(1,text,code)
      fsamp=array$(1)
      
      read(1,'(a)') text
      read(1,'(a)') text
      read(1,'(a)') text
c
c   time and date
c
      do i=1,80
         if (text(i:i).eq.'d'.or.text(i:i).eq.':'.or.
     *    text(i:i).eq.'e'.or.text(i:i).eq.'l') text(i:i)=' '
      enddo
      write(6,*) text
      call sei get values(6,text,code)
      year=array$(6)
      month=array$(5)
      day=array$(4)
      hour=array$(1)
      min=array$(2)
      sec=array$(3)

      write (6,*) 'fecha ',day,'-',month,'-',year
      write (6,*) 'hora ',hour,'h',min,'m',sec,'s'
c
c   number of samples
c
      read(1,'(a)') text
      read (1,'(33x,a)') text
      do i=1,40
         if(text(i:i).eq.'s'.or.text(i:i).eq.'.') text(i:i)=' '
      enddo
      call sei get values(1,text,code)
      nmues=array$(1)*fsamp
c
c   station codes
c
       if(test.ne.999999) then
     
       write(6,*)' Give station code'
       read(5,'(a)') stcod(1)
       stcod(2)=stcod(1)
       stcod(3)=stcod(1)
       endif
       test=999999    ! only do above one time

       return
       end
      
      
