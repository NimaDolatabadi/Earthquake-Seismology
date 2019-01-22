cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  dimassei - convert IRIS DIMAS ASCII files to Seisan
c
c  Lars Ottemoeller Feb 2009
c
c changes
c    2015-06-02 pv   : add signal_int to signalx common block due to
c                      compiler warning
c
c
      implicit none
      include  'seidim.inc'                 ! Dimensions.
      include  'libsei.inc'                 ! Library defns.
      real    data (max_sample)             ! the data
      integer signal_int(max_sample)
      integer idata (max_sample)            ! integer data
      integer nchan,nsamp,tsamp,i
      character*80      filehead(max_trace)
      character*1040    tracehead
      character*80 infile                   ! input filename
      character*80 outfile                  ! 
      real srate                             ! sample rate
      integer year,month,day,hour,min,isec,xx!time
      real sec
      character*5 station                    ! station name
      character*4 comp                       ! component SEISAN
      character*2 network                    ! network code
      character*1 overwrite                  ! overwrite responsefile
      integer in                             ! 1 for filenr.lis
      integer ind
      integer filec                          ! counting files of filenr.lis
c question
      character*80 question,line
      character*29 mainhead_text 
c-- network code
      character*5 net_code
c-- channel 2 or 4 byte
      character*1 cbyte(max_trace)
c time interval
      double precision cinter(max_trace),cstart(max_trace)

c     common /signalx/data
      common /signalx/data,signal_int

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      overwrite='n'

  1   format(a,$) 
 
 20   continue
      question=' Filename or number, filenr.lis for all'
      call filename(question,infile)

      if (infile(1:3).eq.'EOF') stop

      in=0
c filenr.lis
      if (infile(1:10).eq.'filenr.lis') then
        in=1
        open(60,file=infile,status='old',err=20)
      endif

 10   continue

      if (in.eq.1) then 
         read(60,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif
      filec=filec+1

      write(6,'(1x,a13,a)') ' input file: ',infile
      write(*,*) 

       nsamp=1
       open(1,file=infile,status='old')
 222   continue
c
c read header
c
       do i=1,16
         read(1,'(a)',end=333,err=333) line
         if (i.eq.2) then
           comp=' '
           station=' '
           network=' '
           ind=index(line,' ')
           read(line(1:ind-1),'(a)') station
           line=line(ind+1:)
           ind=index(line,' ')
           line=line(ind+1:)
           read(line,'(a)') comp
           comp(4:4)=comp(3:3)
           comp(3:3)=' '
         elseif (i.eq.3) then
           read(line,'(i4,5i2,i4)') year,month,
     &       day,hour,min,isec,xx
           sec=float(isec)+float(xx)/10./1000.
         elseif (i.eq.4) then
           read(line,*) tsamp
         elseif (i.eq.5) then
           read(line,*) srate
           srate=1./(srate/1000.)
         endif
       enddo
c
c   read data
c
 223   continue
       read (1,*,end=333) data(nsamp)
       idata(nsamp)=data(nsamp)
       nsamp=nsamp+1
       
       goto 223
 333   continue
       close(1)
       nsamp=nsamp-1

       write(*,*) nsamp,tsamp
       if (nsamp.ne.tsamp) then
         write(*,*) ' number of samples in header does not agree'//
     &        ' with number of samples in file !!!'
       endif

c
c calc start time and window length
c

       call timsec(year,month,day,hour,min,sec,cstart(1))

       cinter(1)=(nsamp-1)/srate
c
c  make file name and main header
c             
         nchan=1

         net_code = station
         cbyte(1) = '4'

      call sheads(year,month,day,hour,min,sec,nchan,1,
     *                 net_code,mainhead_text,station,comp,
     *                 nsamp,srate,cbyte,
     *                 outfile,filehead,tracehead)

         outfile = outfile(1:29) // '_' // comp
         do i=30,33
           if (outfile(i:i).eq.' ') outfile(i:i)='_'
         enddo

         write(6,'(a,a)') ' Outfile name: ',outfile(1:60)
c
c  open file
c
         open(2,file=outfile,status='unknown',form='unformatted')
c                                                                               
         do i=1,12
            write(2) filehead(i)
         enddo
         write(2)tracehead                                                     
         write(6,'(1x,a70)') tracehead(1:70)                                   
         write(2)(idata(i),i=1,nsamp)
         close(2)
c
c   back for next file
c
         if(in.eq.1) goto 10    
         goto 20 

9999  continue
      close(60)

      stop
      end


