
c
c program to convert SIL ASCII data to Seisan, lo June 01, 2001
c
c comments: hardwired to comp 'B..."
c           not tested much

      program silsei

      implicit none
      include  'seidim.inc'                 ! Dimensions.
      include  'libsei.inc'                 ! Library defns.

      integer idata (max_sample)            ! integer data
      integer nchan,nsamp,i
      character*80      filehead(max_trace)
      character*1040    tracehead
      character*80 infile                   ! input filename
      character*80 outfile                 
      real srate                             ! sample rate
      integer year,month,day,hour,min        !time
      real sec
      character*5 station                    ! station name
      character*4 comp                       ! component SEISAN
      integer in                             ! 1 for filenr.lis
      integer seiclen
      character*80 question,line
      character*40 text
      character*29 mainhead_text 
      character*5 net_code
      character*1 cbyte(max_trace)
      double precision cinter(max_trace),cstart(max_trace)



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

 10   continue
c
c input file names
c
      question=' Filename or number, filenr.lis for all'
      call filename(question,infile)
      if (infile(1:3).eq.'EOF') stop

      in=0
      if (infile(1:10).eq.'filenr.lis') then
        in=1
        open(60,file=infile,status='old',err=9999)
      endif

 20   continue

      if (in.eq.1) then 
         read(60,'(7x,a)') infile
         if(infile(1:4).eq.'    ') stop
      endif

      write(6,'(1x,a13,a)') ' input file: ',infile
      write(*,*) 

c
c read header
c
      open(1,file=infile,status='old')
      read(1,'(a)',end=333,err=333) line
c arn  2001
      station=line(1:4)
      call CASEFOLD(station)
      read(line(6:9),'(i4)') year
c              12345678901234567890
c /usr/sil/net/may/07/09:44:00.00E00
      read(1,'(a)',end=333,err=333) line
      text=' '
      text= line(seiclen(line)-20:seiclen(line))
      if (text(1:3).eq.'jan') then 
        month=1
      elseif (text(1:3).eq.'feb') then
        month=2
      elseif (text(1:3).eq.'mar') then
        month=3
      elseif (text(1:3).eq.'apr') then
        month=4
      elseif (text(1:3).eq.'may') then
        month=5
      elseif (text(1:3).eq.'jun') then
        month=6
      elseif (text(1:3).eq.'jul') then
        month=7
      elseif (text(1:3).eq.'aug') then
        month=8
      elseif (text(1:3).eq.'sep') then
        month=9
      elseif (text(1:3).eq.'oct') then
        month=10
      elseif (text(1:3).eq.'nov') then
        month=11
      elseif (text(1:3).eq.'dec') then
        month=12
      endif
      read (text(5:6),'(i2)') day
      read (text(8:9),'(i2)') hour
      read (text(11:12),'(i2)') min
      read (text(14:18),'(f5.2)') sec
      comp='B   '
      comp(4:4)=text(19:19)
c 100
      read(1,'(a)',end=333,err=333) line
      read(line,'(f5.0)') srate
c T3795 D0395
c aut
c o/s=   600 drift=     0 pwm=  3484   4 Auto 9
      read(1,'(a)',end=333,err=333) line
      read(1,'(a)',end=333,err=333) line
      read(1,'(a)',end=333,err=333) line
c +data+
      read(1,'(a)',end=333,err=333) line
      if (line(1:6).ne.'+data+') then
         goto 333 
      endif
c  36000
      read(1,'(a)',end=333,err=333) line
      read(line,'(i10)') nsamp
c -117874
      do i=1,nsamp
         read(1,'(a)',end=333,err=333) line
         read(line,'(i14)') idata(i)
      enddo
c
c input done
c

      goto 334
    
 333  continue
      close(1)
      write(*,*) ' error reading SIL file '
      stop
 334  continue
      close(1)

c
c SEISAN output
c

      call timsec(year,month,day,hour,min,sec,cstart(1))
      cinter(1)=(nsamp-1)/srate
c
c  make file name and main header
c             
      nchan=1

      net_code = station(1:3)
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
c open file and write header   
c
      open(2,file=outfile,status='unknown',form='unformatted')
      do i=1,12
         write(2) filehead(i)
      enddo
      write(2)tracehead                                                     
      write(6,'(1x,a70)') tracehead(1:70)                                   

c  
c   write data
c
      write(2) (idata(i),i=1,nsamp)
      close(2)
c
c   back for next file
c

      if(in.eq.1) goto 20                                                           
      goto 10 

9999  continue
      close(60)

      stop
      end


