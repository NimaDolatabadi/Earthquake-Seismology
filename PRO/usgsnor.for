c
c   converts usgs CD rom hypocenters to  nordic format
c   most of the informaiton is used. if more than 3 magnitudes available,
c   only the 3 first are used. number of stations are included when available.
c   depth is indicated as fixed in all cases where operator has been used (A,
c   N, G). macroseismic info is included with max intensity. the residuals 
c   standard deviation is put into rms column. event types are set to R.
c   magnitude types: UK is made blank
c                    b is replaced by B
c                    s is replaced by S
c                    D is replaced by C
c                    w is replaced by W
c   
c
c   jh dec 1995
c
      implicit none
      character*120 text           ! one input line
      character*80  data           ! one output line
      character*80  dataf          ! --------------, felt event 
      integer nmag                 ! number of magnitudes, one event
      character*80 infile
      integer nevent               ! event counter
      integer i
      real x


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c
      nevent=0
c
      write(6,*)' Input file name'
      read(5,'(a)') infile
      open(1,file=infile,status='old')
      open(2,file='usgsnor.out',status='unknown')
c
c  hypocenter read and write loop
c
  1   continue
      read(1,'(a)',end=99) text
      data=' '
      data(2:5)=text(7:10)    ! year
      data(7:10)=text(12:15) ! month day
      data(12:15)=text(16:19)! hour min
c
c  give all type regional
c
      data(22:22)='R'
      if(text(20:24).ne.'     ') then
         read(text(20:24),'(f5.2)') x
         write(data(17:20),'(f4.1)') x   ! seconds
      endif
      data(24:30)=text(27:33)            ! latitude
      data(31:38)=text(34:41)            ! longitude
      if(text(42:44).ne.'   ') then
         read(text(42:44),'(i3)') i
         write(data(39:43),'(f5.1)') float(i) ! depth
      endif
      read(text(90:92),'(i3)',err=222) i  ! if it can be read, a number
      data(49:51)=text(90:92)   ! number of stations
 222  continue                  ! if jump to here, no number for stations
      if(text(50:53).ne.'    ') then ! standard deviation, put in rms column
         read(text(50:53),'(f4.2)') x
         write(data(52:55),'(f4.1)') x
      endif
      data(46:48)=text(1:3)   ! hypocenter agency
      if(text(47:47).eq.'G'.or.text(47:47).eq.'A'.or.
     *   text(47:47).eq.'N') data(44:44)='F'
      nmag=0
      if(text(54:56).ne.'   ') then           ! mb magnitude
         data(60:60)='B'
         data(61:63)=text(1:3)                ! agency
         data(57:59)=text(54:56)
         nmag=nmag+1
      endif
      if(text(59:61).ne.'   ') then           ! ms magnitude
         nmag=nmag+1
         if(nmag.eq.1) then
            data(60:60)='S'
            data(61:63)=text(1:3)                ! agency
            data(57:59)=text(59:61)
         endif
         if(nmag.eq.2) then
            data(68:68)='S'
            data(69:71)=text(1:3)                ! agency
            data(65:67)=text(59:61)
         endif
      endif
      if(text(65:68).ne.'    ') then           ! other magnitude
         read(text(65:68),'(f4.2)') x
         nmag=nmag+1
         if(nmag.eq.1) then
            data(60:60)=text(70:70)
            data(61:63)=text(71:73)                ! agency
            write(data(57:59),'(f3.1)') x
         endif
         if(nmag.eq.2) then
            data(68:68)=text(70:70)
            data(69:71)=text(71:73)                ! agency
            write(data(65:67),'(f3.1)') x
         endif
         if(nmag.eq.3) then
            data(76:76)=text(70:70)
            data(77:79)=text(71:73)                ! agency
            write(data(73:75),'(f3.1)') x
         endif
      endif
      if(text(76:79).ne.'    ') then           ! other magnitude
         read(text(76:79),'(f4.2)') x
         nmag=nmag+1
         if(nmag.eq.1) then
            data(60:60)=text(81:81)
            data(61:63)=text(82:84)                ! agency
            write(data(57:59),'(f3.1)') x
         endif
         if(nmag.eq.2) then
            data(68:68)=text(81:81)
            data(69:71)=text(82:84)                ! agency
            write(data(65:67),'(f3.1)') x
         endif
         if(nmag.eq.3) then
            data(76:76)=text(81:81)
            data(77:79)=text(82:84)                ! agency
            write(data(73:75),'(f3.1)') x
         endif
      endif
c
c   check magnitude types
c
      if(data(60:60).eq.'K') data(60:60)=' ' ! make unknow mag type blank
      if(data(60:60).eq.'D') data(60:60)='C' ! make dur mag type  C
      if(data(60:60).eq.'b') data(60:60)='B' ! make 
      if(data(60:60).eq.'s') data(60:60)='S' ! make 
      if(data(60:60).eq.'w') data(60:60)='W' ! make 
      if(data(68:68).eq.'K') data(68:68)=' ' ! make unknow mag type blank
      if(data(68:68).eq.'D') data(68:68)='C' ! make dur mag type  C
      if(data(68:68).eq.'b') data(68:68)='B' ! make 
      if(data(68:68).eq.'s') data(68:68)='S' ! make 
      if(data(68:68).eq.'w') data(68:68)='W' ! make 
      if(data(76:76).eq.'K') data(76:76)=' ' ! make unknow mag type blank
      if(data(76:76).eq.'D') data(76:76)='C' ! make dur mag type  C
      if(data(76:76).eq.'b') data(76:76)='B' ! make 
      if(data(76:76).eq.'s') data(76:76)='S' ! make 
      if(data(76:76).eq.'w') data(76:76)='W' ! make 
      data(80:80)='1'

c
c   write out
c
      write(2,'(a)') data
c
c  check if felt info
c
      if(text(94:94).ne.' '.and.text(94:94).ne.'.') then
        dataf=' '
        dataf(80:80)='2'
        dataf(25:25)=text(94:94) 
        dataf(29:29)=text(93:93)
        if(dataf(29:29).eq.'.') dataf(29:29)=' '
        write(2,'(a)') dataf
      endif
      write(2,*)'                                            '
      nevent=nevent+1
      goto 1
c
 99   continue
      write(6,*)' Number of events converted:', nevent
      write(6,*)' Output file name is usgsnor.out'
      stop
      end
