c 
c PROGRAM RDSEED2SEISAN, written by Peter Voss
c
c program to convert rdseed.stations file to STATION0.HYP format
c and to list data in ARC_CHAN format for SEISAN.DEF, from RESP files
c
c changes
c  28/06/2013 lo change to station output
c
      character*90 text
      character*5 stat
      character*2 net
      character*3 chan
      character*2 loc

      integer lat
      character*1 NS 
      integer lon
      character*1 EW 
      integer dep
      integer seiclen

      integer i,j,k

      real latm, lonm

      latm=0.0
      lonm=0.0
      nchan=1
      write(6,*)' For STATION0.HYP:'
      open(1,file='rdseed.stations')
      open(2,file='rdseed.STATION0.HYP')
    1 read(1,*,END=999) stat, net, latm, lonm, dep
c     write(6,'(a40)') "1234567890123456789012345678901234567890"
c     write(6,*) stat, net, latm, lonm, dep
c     write(6,'("Lat:",f8.4)')latm
      NS="N"
      if(latm.lt.0.0) NS="S"
      lat=int(latm)
      latm=(latm-lat)*60.0
c     write(6,*)lat
c     write(6,*)latm
      lat=abs(lat)
      latm=abs(latm)
c
c     write(6,*)lonm
      EW="E"
      if(lonm.lt.0.0) EW="W"
      lon=int(lonm)
      lonm=(lonm-lon)*60.0
c     write(6,*)lon
c     write(6,*)lonm
      lon=abs(lon)
      lonm=abs(lonm)
c
c     write(6,'("Dep:",i6)')dep
c
      write(6,'(" ",a5,i2,f5.2,a1,i3,f5.2,a1,i4,1x)')
     +stat,lat,latm,NS,lon,lonm,EW,dep
      if (seiclen(stat).eq.5) then ! lo 28 June 2013
        write(2,'(" ",a5,i2,f5.2,a1,i3,f5.2,a1,i4,1x)')
     +    stat,lat,latm,NS,lon,lonm,EW,dep
      else
        write(2,'("  ",a4,i2,f5.2,a1,i3,f5.2,a1,i4,1x)')
     +    stat,lat,latm,NS,lon,lonm,EW,dep
      endif

      write(6,*) '------------------------' 
      goto 1

 999  continue
      close(1)
      close(2)

      write(6,*)' For SEISAN.DEF:'
      open(1,file='filenr.lis')
      open(2,file='rdseed.SEISAN.DEF')
    2 read(1,'(a)',END=888) text
c     write(6,*)text
      if(text(8:12).NE."RESP.") goto 2
      net="  "
      if(text(14:14).EQ.".")read(text(13:13),'(a1)')net(1:1)
      if(text(14:14).NE.".")read(text(13:14),'(a2)')net
      j=16
      if(text(14:14).EQ.".")j=15
      stat="     "
      k=0
      do i=j,j+4
       if(k.EQ.0.AND.text(i:i).EQ.".")k=i-1
      enddo
      read(text(j:k),*)stat(1:k-j+1)
      
      loc="  "
      if(text(k+2:k+2).NE.".")read(text(k+2:k+3),'(a2)')loc

      j=k+5
      if(text(k+2:k+2).EQ.".")j=k+3

      read(text(j:j+3),*)chan

      write(2,'("ARC_CHAN",32x,a5,a3,a2,a2,1x)')stat,chan,net,loc
      write(6,'("ARC_CHAN",32x,a5,a3,a2,a2,1x)')stat,chan,net,loc
      goto 2
 888  continue
      close(1)
      close(2)

      write(6,*)'  ' 
      write(6,*)' Output is written to the files:'
      write(6,*)' rdseed.STATION0.HYP and rdseed.SEISAN.DEF '

      end

