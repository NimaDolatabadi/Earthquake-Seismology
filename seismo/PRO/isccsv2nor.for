c   
c   program converts csv ascii  format to nordic format
c   csv is used by isc for catalog data
c   only the first 3 magnitudes are used
c   if a magnitude type in input is b1, output is x
c
c   j. havskov, january 2014


      implicit none
      character*200 t
      character*80 tt
      character*80 infile
      integer i,j,n


       write(6,*) 'Input file'
       read(5,'(a)') infile
       open(1,file=infile,status='old')
       open(2,file='csvnor.out',status='unknown')
       
       n=0
c
c   find first line with data
c
       do i=1,200
          read(1,'(a)') t
          if(t(3:9).eq.'EVENTID') goto 1
       enddo

1      continue

c
c   read in a loop
c
       read(1,'(a)',end=99) t
       if(t.eq.' ') goto 99   
       n=n+1
       tt=' '
       tt(2:5)=t(21:24)
       tt(7:8)=t(26:27)
       tt(9:10)=t(29:30)
       tt(12:13)=t(32:33)
       tt(14:15)=t(35:36)
       tt(17:20)=t(38:41)
       tt(22:22)='L'
       tt(24:30)=t(44:60)
       tt(31:38)=t(53:60)
c   depth
       tt(39:43)=t(63:67)
c   check if depth fixed
       if(t(69:72).eq.'TRUE') tt(44:44)='F'
       tt(46:48)=t(76:78)
c   first mag
       tt(57:59)=t(94:96)
       tt(60:60)=t(87:87)
       if(t(60:61).eq.'b1') tt(60:60)='x'
       tt(61:63)=t(98:101)
c   sec mag
       tt(65:67)=t(116:118)
       tt(68:68)=t(109:109)
       if(t(109:110).eq.'b1') tt(68:68)='x'
       tt(69:71)=t(120:122)
c   third mag
       tt(73:75)=t(138:140)
       tt(76:76)=t(131:131)
       if(t(131:132).eq.'b1') tt(76:76)='x'
       tt(77:79)=t(142:144)
       
       tt(80:80)='1'

       write(2,'(a)') tt       
       goto 1

 99    continue
       write(6,*)'Number of events',n
       write(6,*)'Output file name is csvnor.out'
       stop
       end              
