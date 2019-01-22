c$DEBUG
c                                                                               
c    transform iris ascii files to seisan  data files 
c                                                                               
c    j. havskov                    
c   jan 29        ************ version 5.0 *************************
c
c    latest update                                                              
c    jul 27, 95 by jh : better error trap
c    aug 21  96       : use v instead of l for file name
c    jan 2   98       : change SS to S for short period
c    jan 23           : implement respense file check
c    dec 16  98    lo : ask for filename, instead of usinf iris.log
c    dec 20  98    jh : ----------  version 7.0 check ---------------
c    feb 12  99    lo : new header format with 6 chars for channel
c    apr 19  00    lo : fix from before for India stations
c    may 11  00    lo : use read_resp_head
c
      implicit none
      include 'seidim.inc'
c                                                                               
c-- seisan  data file channel header      
      character*1040    chahead         
c-- -------------------main --------      
      character*80     mainhead(max_trace)        
c-- date and time
      integer year,month,day,hour,min,isec
      real sec
c-- filter delay
      double precision delay
c-- output file name                                  
      character*80	outfile		
c-- stations and components
      character*4 comp(max_trace),statt
      character*5 stat(max_trace)
      integer*4     data4(90000)
c-- one line of text
      character*80 text
c-- data interval each channel, start time
      real cinter(max_trace),cstart(max_trace)
c-- number of channels and samples/chan, total number of samples           
      integer nchan,nsamp
c-- sample rate                                              
      real		rate		
c-- help variables                                     
      integer		i,k,nline,in
c - - input file name
      character*80 infile
c true for new header
      logical new_header
c index
      integer dot_ind,start_ind
c-- -------------                                    
c
c   open iris ascii log file 
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver


       write(*,*) 'filename'
       read(*,'(a)') infile

       open(1,file=infile,status='old',err=88)
       goto 89
 88    continue
       write(6,*)' No file ',infile
       stop
 89    continue
c
c   read header of ascii file, first read forward to 
c   start of event, also enter here after read of one event
c   from asci file
c             
 2    continue
         read(1,'(a80)',end=99) text
c
c   look for line with station header, has a "/" and "SPS"
c
c old header
cKBS 1999/02/09 06:40:03 +0.428001   SEC    E   LP 1.00 SPS   UNFILTERED 6000
c new header
cONO.10-BHZ 1999/02/09 14:02:00 -0.003374   SEC 40.00 SPS   UNFILTERED 6000
c
         new_header = .false.
         dot_ind=-1
         start_ind=-1
         do i=1,80
            if (text(i:i).eq.'.'.and.dot_ind.eq.-1) then
               dot_ind=i
            endif
            if (text(i:i).ne.' '.and.start_ind.eq.-1) then
               start_ind=i
            endif
            if(text(i:i).eq.'/')  then
               do k=1,78
                  if(text(k:k+2).eq.'SPS') then
c
c check if old or new header
c
c                   if (text(5:5).eq.'.'.or.text(4:4).eq.'.')
                    if (dot_ind.le.11) then
                        new_header = .true.
                        write(*,*) ' new header '
                    endif

                    goto 50
                  endif
               enddo
            endif
         enddo
         goto 2   ! not a header line
 50      continue
c
c   fish out station, can be left or right justified, or be shifted
c          
         statt=text(start_ind:dot_ind-1)

         if (new_header) then
cONO.10-BHZ 1999/02/09 14:02:00 -0.003374   SEC 40.00 SPS   UNFILTERED 6000
c  BHPL.BHZ 2000/04/18 00:03:07 +0.010645   SEC 20.00 SPS   UNFILTERED 6000
cKBS 1999/02/09 06:40:03 +0.428001   SEC    E   LP 1.00 SPS   UNFILTERED 6000

            read(text,'(12x,i4,5(1x,i2),f7.4)')
     *      year,month,day,hour,min,isec,delay

            read(text(49:53),'(f5.2)') rate     
            if(rate.lt.10.0) then
               read(text(70:74),'(i5)') nsamp
            else
               read(text(71:75),'(i5)') nsamp
            endif
c
c component code could be 3 or 6 characters, otherwise add fix
c
            if (11-dot_ind.eq.6) then
              comp(1)(1:1)=text(9:9)
              comp(1)(2:3)=text(6:7)
              comp(1)(4:4)=text(11:11)
c            elseif (11-dot_ind.eq.3) then
c
c assume component is the last three chars
c
            else
              comp(1)(1:2)=text(9:10)
              comp(1)(3:3)=' '
              comp(1)(4:4)=text(11:11)
            endif


         else
c
c   get header info
c
            read(text,'(5x,i4,5(1x,i2),f7.4)')    ! changed delay from 7.3
     *      year,month,day,hour,min,isec,delay
c            write(6,*) 'sec,delay',isec,delay
            read(text(51:55),'(f6.2)') rate
            if(rate.lt.10.0) then
               read(text(73:77),'(i5)') nsamp
            else
               read(text(74:78),'(i5)') nsamp
            endif
            comp(1)(1:1)=text(49:49)
            comp(1)(2:2)=text(48:48)
            if(comp(1)(1:2).eq.'SS') comp(1)(1:2)='S ' ! use only S for SP
            comp(1)(3:3)=' '
            if(text(44:44).eq.' ') then
                comp(1)(4:4)=text(45:45)
            else
                comp(1)(4:4)=text(44:44)
            endif

          endif    
          write(6,*) 'Number of samples',nsamp
c
c   correct for filter delay
c
            sec=isec
            call timadd(year,month,day,hour,min,sec,delay,
     *                  year,month,day,hour,min,sec)
c            write(6,*) 'sec after delay added',sec

            stat(1)(1:4)=statt
            stat(1)(5:5)=' '
            cstart(1)=0.0
            cinter(1)=(nsamp-1)/rate
c
c  blank data array
c
            do i=1,nsamp
               data4(i)=0
            enddo
c
c   read one blank line
c
            read(1,'(a)')text
c
c   calculate number of lines to read, 8 samples pr line, the 9th
c   value is a checksum
c
            i=1
            nline=nsamp/8
            do in=1,nline
              read(1,'(a)',end=2222,err=2222) text
c             write(6,'(a)') text
              if(text(1:10).eq.'          ')goto 2222 
			  backspace 1
              read(1,*,end=2222,err=2222) (data4(k),k=i,i+8)
c             write(6,*)data4(i)
              i=i+8
            enddo
         goto 2223
 2222    continue
            write(6,*)' Data missing, return to continue'
            read(5,'(a)') text
 2223    continue
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
         outfile(31:34)=comp(1)
         do i=31,34
            if(outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo
         write(6,'(a,a)') ' Outfile name: ',outfile(1:34)
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
            chahead(1:4)=statt		
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
c   back for next channel
c
            goto 2                                                                    
 99   continue                                                                  
      stop                                                                      
      end                                                                       


