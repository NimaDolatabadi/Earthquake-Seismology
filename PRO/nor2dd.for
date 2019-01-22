c
c  Program to make a jhd phase input file form a Nordic file
c
c  Brian Baptie, 2004
c

c
c changes:
c  02 nov 2011 wc - changed to allow for negative magnitude
c  03 Nov 2011 lo - added high accuracy reading
c  16 Mar 2016 fh - change seconds and minutes when they are 60
c

      implicit none
      CHARACTER*80 DATA(2500)                                                  
      CHARACTER*80 infile                                                     
      CHARACTER*5 stations(2500)                                                  
      CHARACTER*1 TYPE,EXP                                                      
      character*80 station_file
      integer nstat,nphase,nhead,nrecord
      integer sta_list_count
c---number of arguments 
      integer nars
c-- arguments
      character*80 args(10)
      integer n,id
c--compact or not
      logical compact
      logical error

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      error=.false.

c 
c   check if input from file given in argument
      call get_arguments(nars,args)
      if(nars.eq.0) then
         write(6,*)' You must give input file as argument'
         stop
      endif
c
c     case with output on standard output
c
      if(nars.gt.2) then        !JAB(BGS)Mar95. Definitely too many
         write(6,*)' Too many arguments'
         stop
      endif
c
c   output file
c
      open(2,file='phase.dat',status='unknown')
c
c   get input file name
c
      infile=args(1)
      open(1,file=infile,status='old')
c
c   check that not a compact file
c
      call nortype(1,compact)
      if(compact) then
         write(6,*)' Input file is compact, cannot be used'
         stop
      endif
c     
c   read and write to end of file
c
      n=0
 10   continue
c
c   read one event in nordic format
c
      CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 99
c
c   convert phase readings to hypodd input format and write out 
c
      call nor2dd(2,n+1,nrecord,nhead,data,stations,sta_list_count)
      n=n+1
      goto 10
c
 99   continue
      write(6,*)
      write(6,*)' The input file had ',n,' events'
        
      close(1)
      close(2)

c
c find the appropriate station file and check its existence
c
c      call get_station_file(station_file,error)
c      if (error) then
c         stop
c      endif
c     
c find the stations corresponding to those in the station list and
c write out jhd format stalist.elev file containing station details
c        
c      call write_station_output(station_file,
c
c changed to use stat_loc to get location, lot 31/5/2005
c
      call write_station_output(
     1     stations,sta_list_count,error)
      if (error) then
         stop
      endif
c
c extract velocity model parameters from the station file and
c write out jhd format syn.vel velocity model file
c       
c      call get_velocity_model(station_file,error)
c      if (error) then
c         stop
c      endif


      stop
      end
                       

c
c
      subroutine nor2dd(unit,nevent,nrecord,nhead,data,stations,
     1     sta_list_count)
	  
c
c   gets one event in nordic format in array data and writes it to unit
c   in hypoinverse format
c
      implicit none
      character*80 data(*)                                                  
      character*5 stations(*)
      integer sta_list_count
      integer nrecord,unit,nhead,nevent
      integer year,month,day,hour,min
      integer p_weight
      real head_sec, p_sec, p_diff
      real lon,lat,depth
      real weight
      real eh,ez,rms,mag
      double precision head_time,p_time ! abs times for header and phase
      integer j,k
      logical station_in_list
      character*1 c
      integer ain


c
c   read origin time
c
      read(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1)') 
     *year,month,day,hour,min,head_sec

c   check if second/ minute are 60, then correct minute/hour , FH 26 March 2016
125   if (head_sec .ge. 60.) then 
        head_sec = head_sec - 60.
        min = min + 1
      endif      
126   if (min .ge. 60.) then 
        min = min - 60.
        hour = hour + 1
      endif



      read(data(1)(31:38),'(f8.3)') lon 
      read(data(1)(24:30),'(f7.3)') lat
      read(data(1)(39:43),'(f5.1)') depth 
      read(data(1)(53:55),'(f3.1)') rms 
c      read(data(1)(57:59),'(f3.1)') mag 
      read(data(1)(56:59),'(f4.1)') mag 
      eh=0.0
      ez=0.0
c
c search for high accuracy line
c
      do k=1,nhead
        if (data(k)(80:80).eq.'H') then
          read(data(k)(17:22),'(f6.3)') head_sec
          read(data(k)(24:32),'(f9.5)') lat
          read(data(k)(34:43),'(f10.5)') lon
          read(data(k)(45:52),'(f8.3)') depth
          write(*,*) ' found high accuracy solution ',lat,lon,depth
        endif
      enddo

c write out origin details
      write(unit,101) year,month,day,hour,min,head_sec,lat,lon,
     *depth,mag,eh,ez,rms,nevent

c 101  format('#',1x,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f4.1,1x,f7.3,
 101  format('#',1x,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f4.1,f8.4,    ! changed precision to 4 digits after . for lat and lon
c     1     1x,f8.3,1x,f5.1,1x,f3.1,1x,f3.1,1x,f3.1,1x,f3.1,1x,i4)
c     1     1x,f8.3,1x,f5.1,1x,f4.1,1x,f3.1,1x,f3.1,1x,f3.1,1x,i4)
     1     f9.4,1x,f5.1,1x,f4.1,1x,f3.1,1x,f3.1,1x,f3.1,1x,i4)
c
c   get header abs time
c
      call timsec(year,month,day,hour,min,head_sec,head_time)

c
c   read phases to end of event
c
      do k=nhead+1,nrecord-1

c For each P-phase calculate the phase time relative to the origin time
c         if(data(k)(11:11).eq.'P'.or.data(k)(11:11).eq.'S') then
         if(data(k)(11:12).eq.'P '.or.data(k)(11:12).eq.'S ') then
c first arrivals only, have to be just P or S
c         if(data(k)(11:12).eq.'P '.or.data(k)(11:12).eq.'S ') then
c            read(data(k)(19:28),'(i2,i2,1x,f5.2)') hour,min,p_sec
            read(data(k)(19:22),'(2i2)') hour,min
            read(data(k)(23:28),'(f6.3)') p_sec
            read(data(k)(15:15),'(i1)') p_weight
            if(p_weight.eq.0) then
               weight=1.0
            else if(p_weight.eq.1) then
               weight=0.75
            else if(p_weight.eq.2) then
               weight=0.5
            else if(p_weight.eq.3) then
               weight=0.25
            else if(p_weight.eq.4) then
               weight=0.0
            endif
            call timsec(year,month,day,hour,min,p_sec,p_time)
            p_diff=p_time-head_time ! phase time relative to header time
            p_sec=head_sec+p_diff   ! phase second relative to header second  

c write out the formatted phase output

            station_in_list=.false.
            do j=1,sta_list_count
               if(data(k)(2:5).eq.stations(j)(1:4)) then
                  station_in_list=.true.
               endif
            enddo
            if(.not.station_in_list) then
               sta_list_count=sta_list_count+1
               stations(sta_list_count)(1:5)='     '
               stations(sta_list_count)(1:4)=data(k)(2:5)
            endif
            read(data(k)(58:60),'(i3)') ain
c assume that Pn has AIN less than 90 deg
            c=' '
            if (ain.lt.90) then
              c='n' 
            else
              c='g'
            endif
c            write(unit,100) data(k)(2:5),p_diff,weight,data(k)(11:11),c
            write(unit,100) data(k)(2:5),p_diff,weight,data(k)(11:11)

         endif
      enddo

      return


 100  format(a4,1x,f6.2,1x,f4.2,1x,a1,a1)

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C GET_STATION_FILE 
C
C Finds the STATION0.HYP file either in the local directory or in the 
C SEISAN_TOP/DAT directory
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine get_station_file(station_file,error)

      integer itp
      character*80 cur_file        ! station file in current directory
      character*80 dat_file        ! station file in DAT directory
      character*80 station_file
      character*60 top_directory   ! seisan top dir
      character*1 dchar            ! '/' character
      logical error

      error=.false.
c
c   find station file to use
c
      call dir_char(dchar)
      call topdir(top_directory)
      ITP=index(top_directory,' ')-1
c
c  make file name for both file in current and dat directory
c
      cur_file(1:12)='STATION0.HYP'
      dat_file(1:itp)=top_directory(1:itp)	  
      dat_file(itp+1:itp+1)=dchar
      dat_file(itp+2:itp+4)='DAT'	  
      dat_file(itp+5:itp+5)=dchar
      dat_file(itp+6:itp+17)=cur_file(1:12)	  	  
      open(999,file=cur_file,status='old',err=1)
      write(6,2001) cur_file(1:12)
      station_file=cur_file
      goto 2
 1    continue	  
      OPEN(999,FILE=dat_file,STATUS='OLD',err=3)
      write(6,2001) dat_file(1:itp+17)
      station_file=dat_file
      goto 2
 3    continue
      write(6,*)' No station file found'
      error=.true.

 2    continue
      close(999)

 2001 format (' Using station file ', a)


      return

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C WRITE_STATION_OUTPUT write out JHD stat.evel file
C
C Given a list of station names this program
C finds the corresponding entry in the STATION0.HYP file
C and writes out station data in JHD format
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      subroutine write_station_output(station_file,
c     1     stations,sta_list_count,error)
      subroutine write_station_output(
     1     stations,sta_list_count,error)
      implicit none

      character*80 station_file
      CHARACTER*5 stat
      CHARACTER*5 stations(*)
      integer sta_list_count
      character*80 line         ! one line

      integer ilat, ilon
      real dec_lat, dec_lon, mlat, mlon, elev,el
      character*1 dlat, dlon, file_ind

      integer i,j

      logical error
      
      error=.false.
c
c open input and output files
c
c      open(10,FILE=station_file,STATUS='OLD',err=98)
      open(3,file='station.dat',status='unknown',err=99)

      file_ind=' '
      do i=1,sta_list_count
         call stat_loc(stations(i),file_ind,dec_lat,dec_lon,el)
         if (dec_lat.ne.0..and.dec_lon.ne.0.) then
           write(3,100) stations(i), dec_lat, dec_lon
         else
           write(6,'(a)') ' station not found '//stations(i)
         endif
      enddo
      return
 100  format(1x,a4,1x,f8.4,1x,f9.4)
           
c         rewind(10)
c         do j=1,1000
c            read(10,'(a27)') line
c            if(stations(i)(1:4).eq.line(3:6)) then
c               read(line(7:27),'(i2,f5.2,a1,i3,f5.2,a1,f4.0)') 
c     &              ilat, mlat, dlat, ilon, mlon, dlon, elev
c               elev=elev/1000.
c               dec_lon=real(ilon)+(mlon/60.0)
c               if(dlon.eq.'W') then
c                  dec_lon=-1.0*dec_lon
c               endif
c               dec_lat=real(ilat)+(mlat/60.0)
c               if(dlon.eq.'S') then
c                  dec_lat=-1.0*dec_lat
c               endif
c               write(3,100) stations(i), dec_lat, dec_lon
c               goto 11
c            endif
c         enddo
c         
c 11      continue
c      enddo
c
c      close(3)
c      close(10)
c
c      return
c
c 100  format(1x,a4,1x,f8.4,1x,f9.4)
c
c 98   write(6,*) 'Error opening station file ', station_file
c      error=.true.
c      return
 99   write(6,*) 'Error opening output file station.dat'
      error=.true.
      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Subroutine to read the velocity model from a STATION0.HYP file
C
C Requires the name of the file as an input argument
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine get_velocity_model(station_file,error)


      character*80 station_file    ! station file
      character*80 line            ! one line

      real xnear,xfar,startz,vpvs
      real vel(100), z(100)

      integer nlayers
      integer i

      logical error

      error=.false.
c     
c open input and output files
c
      open(10,FILE=station_file,STATUS='OLD',err=98)
      open(4,file='syn.vel',status='unknown',err=99)

      rewind(10)
c
c Find first blank line
c
 12   read(10,'(a)') line
      if (line(1:10).ne.'          ') then
         goto 12
      endif
c
c Find second blank line
c
 13   read(10,'(a)') line
      if (line(1:10).ne.'          ') then
         goto 13
      endif
c
c read velocity model
c
      i=0
      read(10,'(a)') line
 30   continue
      i=i+1
      read(line,'(2f8.3)') vel(i),z(i)
      read(10,'(a)') line
      if(line(1:10).ne.'          ') goto 30
      nlayers=i

c
c  vp/vs, start depth and distance weight
c
      read(10,'(3f5.0,f5.2)') startz,xnear,xfar,vpvs

c write velocity structure to output file
      do i=1,nlayers
         write(4,*) z(i), vel(i), vpvs
      enddo

      close(10)
      close(4)

      return

 98   write(6,*) 'Error opening station file ', station_file
      error=.true.
      return
 99   write(6,*) 'Error opening output file syn.vel'
      error=.true.
      return

      end
