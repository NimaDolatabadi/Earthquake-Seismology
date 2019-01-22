c
c  convert Kinematrics K2 files to SEISAN format, only pc
c
c  Testdata and K2 reading program supplied by 
c  Mauricio Ciudad Real, Kinemetrics
c
c  Program limitations:
c  max 900 lines in header file
c
c  The kinemetrics voltage values are converted to counts from the 
c  digitizer assuming 2.5 v out from 1 24 bit digitizer
c  The channel names and station codes can come from the k2sei.def file, 
c  if no k2sei.def file the names are made as follows:
c 
c  Station code:
c  If a station name is given in header, that is used for all channels
c  If no station name available, the serial number is used
c  If no serial number, all stations are call SSS
c
c  Component code:
c  The first 3 channels are called A  N, A  Z and A  E. The following are
c  called A  4, A  5 etc
c
c  The first 28 chars in def file goes into seisan main header
c
c  j. havskov, feb, 96
c
c  updates:
c  
c  mar 21 96 jh: clear up
c  oct 2 97    : fix so it runs with newer version of k2
c  nov 13      : change k2.def to k2sei.def, put in header info in def file
c  jan 22 98 jh: delete kinemetrics ascii files after use
c --------------------- Version 7 -------------------------
c  sep 22 99 lo: version 7 changes, use max_sample
c  sep 29    jh : check 160 instead of 1040 for nonexistance of cal file
c  may 15       : new read resp routine
c  oct 1 ,00    : fix so it works with more than 9 channels
c  nov 12 03 jh : fix so it works with newer file which has shifted line where
c                 number of channels is read
c  sep 2  05 jh : change to kw2asc32 form kw2asc to use with longer file names
c                 required a change in station name, also some clean up
c
c
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
c-- input data vector	  
      real     xdata(max_sample)
c-- output data vector
      integer  data(max_sample)
c-- main header------------------------------------------
      character*80 mainhead(max_trace)
      character*29 mainhead_text
c-- channel header
      character*1040 chahead
      character*50 save_head   ! save part of header when looking for resp info
c-- output file name, definition file 
      character*80 outfile, deffile
c-- number of stations
      integer nchan	  
c-- channel to calculate channel header for
      integer ichan
c-- network code
      character*5 net_code        
c-- stations and components
      character*5 stat(max_trace)
      character*4 comp(max_trace)
c--channel header date and times	  
      integer year(max_trace),month(max_trace),day(max_trace),
     *hour(max_trace),min(max_trace)
      real sec(max_trace)
c-- channel samples and sample rate
      integer nsamp(max_trace)
      real rate(max_trace)	  
      real duration             ! total duration in secs
      integer nkinhead          ! number of headers in kinemetrics header file
      character*80 text(900)    ! headers in header file 
      character*80 text1        ! help text
      character*1   t            ! help text
      character*5  station      ! station code in header file
      integer ndef_stat         ! number of stations/lines in def file
      character*4 serial(200)   ! k2 serial numbers
      character*4 k2serial      ! serial number of current unit
c      character*4 def_stat(200) ! station code in def file
c      character*4 def_comp(200) ! component code in def file
c      integer   def_chan_number(200)! channel number of current unit
      integer flength           ! length of input file name
      integer seiclen           ! function to get length of string
      real factor               ! facot to sample values convert to integers
      character*3 tchan         ! channels name: 001, 002  etc
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)
c-------------------------------------------------------------------
c-- input file name	  
      character*80 infile
c-- infile indicator; in=0: one file, in=1: many files
      integer in	  
c-- question
      character*80 question	  
c--Counters etc
	  integer i,k
      real x
c-- computer type
c      logical pc,sun,linux
      logical no_net                    ! flag if net_code set
      equivalence(xdata,data)



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c      call computer_type(sun,pc,linux)
      mainhead_text=' '
c
c   get def file for station codes, give file name
c
      deffile='k2sei.def'
      no_net = .FALSE.
      net_code=' '

      call read_def_chan(deffile,mainhead_text,net_code)

      if (net_code.eq.' ') no_net = .true.

c
c   get file name
c
19    continue
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
c   file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      write(6,'(1x,a)') infile
	  flength=seiclen(infile)   ! length of file name
c
c   convert file to ascii using kinemetrics program kw2asc32
c
      call systemc('kw2asc32 '//infile(1:flength)//' -h',9+flength+3)
c
c   open header file to find main info
c
       open(1,file=infile(1:flength-3)//'HDR',status='old')
c
c   delete header file not used
c
       open(2,file=infile(1:flength-3)//'SHD',status='unknown')
       close(2,status='delete')

c
c   read whole header file into text array
c
       i=0
 1     continue
          i=i+1
          read(1,'(a)',end=2) text(i)
          goto 1
 2     continue
       close(1,status='delete')
       nkinhead=i                ! number of kinemetrics headers
       write(6,*)' Number of Kinemetrics headers', nkinhead
c
c   get info from header
c
       station=' '
       k2serial='SSSS'     ! serial number, default is SSSS
c
       rate(1)=0.0
       duration=0.0
c
       do i=1,nkinhead
         if(text(i)(6:17).eq.'channel unit'.or.
     *      text(i)(5:16).eq.'channel unit') then   ! get number of channels
            text1=' '
            k=index(text(i),',')
            text1(1:3)=text(i)(k+1:k+3)
            call sei get values(1,text1,k)
            nchan=array$(1)
c            read(text(i)(19:20),'(i2)') nchan
         endif
c
         if(text(i)(1:18).eq.'Samples per second') then  ! get sample rate
            text1=' '
            text1(1:10)=text(i)(20:29)
            call sei get values(1,text1,k)
            rate(1)=array$(1)
c            read(text(i)(20:24),'(i5)') k
c            rate(1)=k
         endif
c
         if(text(i)(1:16).eq.'Event Start Time') then ! file start time
c
c   extract values to read
c      
            text1=' '       
            text1(1:33)=text(i)(18:50)
            do k=1,33
               t=text1(k:k)
               if(t.eq.'/'.or.t.eq.'('.or.t.eq.')'.or.t.eq.':') 
     *         text1(k:k)=' '
            enddo
            call sei get values(7,text1,k)
c            year=array$(3)-1900
            year=array$(3)
            month=array$(1)
            day(1)=array$(2)
            hour(1)=array$(5)
            min(1)=array$(6)
            sec(1)=array$(7)
c            read(text(i)(27:28),'(i2)') year(1)    ! year
c            read(text(i)(19:20),'(i2)') month(1)   ! month
c            read(text(i)(22:23),'(i2)') day(1)     ! day of month
c            read(text(i)(36:47),'(i2,1x,i2,1x,f6.3)') 
c     *      hour(1),min(1),sec(1)
         endif 
c
c   station
c
         do k=1,77
            if(text(i)(k:k+2).eq.'Stn') then
c         
c   first check if station code is blank
c  
                if(text(i)(k+7:k+10).eq.'Site') then
                   station='     '
                else
                   station=text(i)(k+5:k+9)  ! station code
                endif
             endif
         enddo
c
         if(text(i)(1:8).eq.'Duration') then       ! event duration, secs
            text1=' '
            text1(1:9)=text(i)(10:18)
            call sei get values(1,text1,k)
            duration=array$(1)
c            read(text(i)(10:17),'(f8.3)') duration
         endif
c      
c         
         if(text(i)(14:23).eq.'Header S/N') then  ! old version
            if(text(i)(28:28).eq.',') then
                k2serial(1:3)=text(i)(25:27)
                k2serial(4:4)=' '
            else
                k2serial=text(i)(24:27)
            endif
         else
c
c new version
c
            do k=1,77
               if(text(i)(k:k+2).eq.'S/N') then
                  k2serial(1:4)=text(i)(k+3:k+6)
                endif
            enddo
         endif
         
       enddo
c
c   check sample rate
c
      if(rate(1).eq.0.0) then
         write(6,*)' Sample rate not found, set to 100'
       else
         write(6,*)' Sample rate is ', rate(1)
       endif
c
c   if duration is zero, newer verison of k2 without this variable,
c   open channel one and find number of samples by reading to the end 
c
      if(duration.eq.0.0) then
         open(1,file=infile(1:flength-3)//'001',status='old')
         k=0
 60      continue
           read(1,*,end=61) x
           k=k+1
           goto 60
 61      continue
         close(1)
         duration=k/rate(1)
      endif
c
       nsamp(1)=duration*rate(1)
       write(6,'(a,a,a,a)')' Station ', station,'  Serial ',k2serial
       write(6,*) 'nsamp=',nsamp(1),'  number of channels = ',nchan
       write(6,*) year(1),month(1),day(1),hour(1),min(1),sec(1)
c
c  enter loop to define header info
c
       do ichan=1,nchan
c
c   first put in defaults in case no def file
c
            if(station.ne.'     ') then
               stat(ichan)=station
            else
               stat(ichan)=k2serial
            endif
            comp(ichan)(1:4)='    '
            write(comp(ichan)(3:4),'(i2)') ichan 
c            if(ichan.eq.1) comp(ichan)(3:4)=' N'
c            if(ichan.eq.2) comp(ichan)(3:4)=' Z'
c            if(ichan.eq.3) comp(ichan)(3:4)=' E'
c            if(ichan.gt.3) write(comp(ichan)(3:4),'(i2)')ichan



c
c   assume same start and stop time and sample rate for all channels
c            
         year(ichan)=year(1)
         month(ichan)=month(1)
         day(ichan)=day(1)
         hour(ichan)=hour(1)
         min(ichan)=min(1)
         sec(ichan)=sec(1)
         rate(ichan)=rate(1)
         cbyte(ichan)='4'
         nsamp(ichan)=nsamp(1)
c
         write(6,'(1x,a5,i4,4i3,f7.1)') stat(ichan),
     *   year(ichan),month(ichan),
     *   day(ichan),hour(ichan),min(ichan),sec(ichan)
       enddo
c
c   look for values in def file
c
       do i=1,nchan
         call set_def_chan(i,stat(i),comp(i)) 
       enddo

c
c   make seisan headers
c
      ichan=1
      if (no_net) net_code=stat(1)(1:5)
c      write(6,*) no_net, stat(1),net_code
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,
     *                 stat,comp,nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c   open output file
c
      write(6,'(a22,a40)')' Output file name is: ',outfile(1:40)
      open(2,file=outfile,status='unknown',form='unformatted')	  
      mainhead(1)(2:29)=mainhead_text
c
c   write main header
c
      write(6,'(1x,a)') mainhead(1)(1:75)
      do i=1,12
         write(2)mainhead(i)
      enddo
C
C   enter channel  loop
c
      do ichan=1,nchan
c
c   make channel header	
c
      call sheads(year,month,day,hour,min,sec,nchan,ichan,
     *                 net_code,mainhead_text,
     *                 stat,comp,nsamp,rate,cbyte,
     *                 outfile,mainhead,chahead)
c
c  open one channel ascii file
c 
       write(tchan,'(i3)') ichan
       if(tchan(1:1).eq.' ') tchan(1:1)='0'
       if(tchan(2:2).eq.' ') tchan(2:2)='0'
       open(1,file=infile(1:flength-3)//tchan,status='old')
c
c   read file
c
       read(1,*)(xdata(i),i=1,nsamp(1))
       close(1,status='delete')
c
c   convert to real numbers, assume input to be 2.5 v for 24 bit converter
c
       factor=2**23/2.5
       do i=1,nsamp(1)
          data(i)=xdata(i)*factor
       enddo
c                                                                               
c   get response, save date etc since removed when calling read_resp            
c                                                                               
c        save_head=chahead(1:50)                                             
        call read_resp_head(chahead)                                            
c
c   check if response curve
c
c        if(chahead(160:160).eq.'9') write(6,*)
c     *  ' No response file for this channel --------'
c        chahead(1:50)=save_head                                             
c
        write(2)chahead
        write(2) (data(i),i=1,nsamp(ichan))
c
c   end of channels loop
c
      enddo
      write(6,*)
      close(2)
c
c  back for next file if many
c
      if (in.eq.1) goto 1000                                                               
      goto 19
      stop
      end	   	  	  	         	  	  
								  
