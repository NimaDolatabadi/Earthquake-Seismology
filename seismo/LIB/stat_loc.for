      subroutine stat_loc(station,file_ind,slat,slon,elev)                            
c                                                                               
c     Routine to return coordinates of a given station in                       
c     degrees (and decimaldegrees) and elevation
c     if lat,lon,depth 0.0, station not found                                
c                                                                               
c    written by C. Lindholm May 1990                                            
c
c    updates:
c    mar 23,  92 by j.h. : unix adoption
c    Aug. 20 -93 c.l. topdirectory til charachter*60
c    Jan 95      j.h : ********** version 5.0 *******************
c    Feb 2           : bug
c    sep 16 98 jh    : ----------- version 7.0 check ------------
c    oct               changed for 5 char station
c    feb  2 99 lo    : bug fixed
c    feb 19 99 lo    : elev changed to f4.0
c    oct  16   jh    : posibility of reading coordinates with one more digit
c    oct 22    lo    : return if no station file
c    feb 03 11 jh    : retrun of error code is le 0, not just 0
c    apr  4 12 jh    : add routine stat_loc_many
c    jul    15 jh    : allso return in stat_loc_may if no station file, 
c                      bug in stat_loc_many, probably did not always work

      implicit none 
      include 'libsei.inc'
c-- latitude and longitude (output)                        
      real slat,slon			
c-- elevation (output)                                         
      real elev				
c-- station code (input)                              
      character*5 station 		
c-- station file indicator x in STATIONx.HYP
      character*1 file_ind
c-- string with data read from stationfile             
      character*80 string		
c-- station file name
      character*80 stat_file
c-- internal var.                       
      integer lat,lon
      real dlat,dlon
c-- unit and error code
      integer unit,code
c-- Direction indicators                               
      character*1 ns,ew				
                                     
      slat=0.
      slon=0.
      elev=0.
      stat_file(1:12)='STATION0.HYP'
      if(file_ind.ne.' ') stat_file(8:8)=file_ind ! check if alternative file
c
c  Search  file in current, then in DAT 
c
           call sei get file( open$+ignore$,   ! Check for  file.
     &                        unit,            ! Unit (n/a).
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                   stat_file )           ! For this filename.

                                                                                
      if (unit.le.0) then
        write(6,'(a,a)') 
     * ' Station file not found:  ' ,stat_file(1:12)
        return
      endif
c                                                                               
c---- read until station is found---                                            
c                                                                               
c         write(6,*)' unit', unit
1     read(unit,'(a)',end=999)string
      if(station(5:5).eq.' ') then  ! 4 char station                                              
         if(string(3:6) .ne. station(1:4)) go to 1
      else                          ! 5 char station
         if(string(2:6) .ne. station(1:5)) go to 1
      endif                                              
      if (string(1:2).eq.'RE') goto 1 
c
c-------- station found ---
c
      read(string,10) lat,dlat,ns,lon,dlon,ew,elev                    
10    format(6x,i2,f5.3,a1,i3,f5.3,a1,f4.0)      
      slat = lat + dlat/60.  
      if(ns .eq. 'S') slat = -slat                                              
      slon = lon + dlon/60.                     
      if(ew .eq. 'W') slon = -slon                                              
      goto 20                                                                                
999   continue
      slat = 0.0
      slon = 0.0
      elev = 0.0
 20   continue                                                                
      call sei close(close$,unit,code)
      return
      end                                                                       


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine stat_loc_many
     *(station,nstat,file_ind,slat,slon,elev,err)                            
c                                                                               
c     Routine to return coordinates of a list of stations in                       
c     degrees (and decimaldegrees) and elevation 
c     nstat is number of input stations      
c     returning zero in coordiantes means station was not found
c     err 0 ok, err 1 no station file                         
c                                                                               
c    modified from stat_loc april 3 2012, jh                                           
c
c    updates:

      implicit none 
      include 'libsei.inc'
c-- latitude and longitude (output)                        
      real slat(*),slon(*)			
c-- elevation (output)                                         
      real elev(*)				
c-- station code (input)                              
      character*5 station(*)
c-- number of requested stations
      integer nstat 		
c-- station file indicator x in STATIONx.HYP
      character*1 file_ind
c-- string with data read from stationfile             
      character*80 string		
c-- station file name
      character*80 stat_file
c-- internal var.                       
      integer lat,lon
      real dlat,dlon
c-- unit and error code, counters
      integer unit,code,k,i
c-- Direction indicators                               
      character*1 ns,ew
      integer err         ! id 0 ok, if 1 station file missing				
    
      do i=1,nstat                                     
         slat(i)=0.
         slon(i)=0.
         elev(i)=0.
      enddo
c
      err=0
        
      stat_file(1:12)='STATION0.HYP'
      if(file_ind.ne.' ') stat_file(8:8)=file_ind ! check if alternative file
c
c  Search  file in current, then in DAT 
c
           call sei get file( open$+ignore$,   ! Check for  file.
     &                        unit,            ! Unit (n/a).
     &                        code,            ! Returned condition.
     &                        'DAT',           ! Alternative search directory.
     &                   stat_file )           ! For this filename.

                                                                                
      if (unit.le.0) then
        write(6,'(a,a)') 
     * ' Station file not found in stat_loc_many:  ' ,stat_file(1:12)
        err=1               
        return
      endif
c                                                                               
c---- read until stations are found---                                            
c                                                                               
c         write(6,*)' unit', unit
 
 1    continue                            ! read next station  
 
      read(unit,'(a)',end=20) string
      if (string(1:5).eq.'RESET') goto 1
c
c
c   check if this one in list
c

      do k=1,nstat 
         if(station(k)(5:5).eq.' ') then  ! 4 char station                                           
            if(string(3:6) .eq. station(k)(1:4)) go to 2   ! station found
         else                          ! 5 char station
            if(string(2:6) .eq. station(k)(1:5)) go to 2   ! station found
         endif
      enddo 
c
c   not found, read next station
c
      goto 1                                            
c
c-------- station found ---
c
 2    continue

      read(string,10) lat,dlat,ns,lon,dlon,ew,elev(k)

               
10    format(6x,i2,f5.3,a1,i3,f5.3,a1,f4.0)      
      slat(k) = lat + dlat/60.  
      if(ns .eq. 'S') slat(k) = -slat(k)                                              
      slon(k) = lon + dlon/60.                     
      if(ew .eq. 'W') slon(k) = -slon(k)

      go to 1                       ! read next station

c
c   end of station file 
c                                              
                                                                                
 20   continue   

c
c  there might be several channels with same station code so fill in those
c  forgotten what this is ?
c
      do i=1,nstat
         if(slat(i).eq.0.0.and.slon(i).eq.0.0) then
            do k=1,nstat
               if(station(k).eq.station(i).
     *         and.slat(k).ne.0.0.and.slon(k).ne.0.0) then
                  slat(i)=slat(k)
                  slon(i)=slon(k)
                  elev(i)=elev(k)
               endif
            enddo
         endif
      enddo

 30   continue
                                                                                  
      call sei close(close$,unit,code)
      return
      end 
