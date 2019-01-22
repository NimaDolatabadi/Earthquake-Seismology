c program to convert main parameters from s-file to csv file, that can be read into 
c spreadsheet or ArcGIS
c
c  lo 2012
c

      program norcsv
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer i                           ! counter
      character*80 text

      call get_seisan_def
c
c   open output file

      open(2,file='norcsv.txt',status='unknown')
      open(3,file='header.txt',status='unknown')
      write(2,'(a)')
     & 'date,time,latitude,longitude,depth,magnitude,magtype'
    
c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event form file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1               ! count events

c
c write output
c
      if (hyp_depth(1).eq.-999.) hyp_depth(1)=0.
      if (hyp_lat(1).ne.-999..and.hyp_lon(1).ne.-999.) then
      if (hyp_mag_all(1).ne.-999.) then
       write(text,'(i4.4,"/",i2.2,"/",i2.2,",",i2.2,":",i2.2,":",
     &     f5.2,",",f9.3,",",f8.3,",",f5.1,",",f4.1,",",a1,a3)') 
     & hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),hyp_min(1),
     & hyp_sec(1),hyp_lat(1),hyp_lon(1),hyp_depth(1),
     & hyp_mag_all(1),hyp_mag_type_all(1),hyp_mag_agency_all(1)
       if (text(18:18).eq.' ') text(18:18)='0'
       write(2,'(a)') text
      else
       write(text,'(i4.4,"/",i2.2,"/",i2.2,",",i2.2,":",i2.2,":",
     &     f5.2,",",f9.3,",",f8.3,",",f5.1,",",",")') 
     & hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),hyp_min(1),
     & hyp_sec(1),hyp_lat(1),hyp_lon(1),hyp_depth(1)
       if (text(18:18).eq.' ') text(18:18)='0'
       write(2,'(a)') text
      endif
      endif
c
c   write the whole first header line with selecetd magnitude
c
      do i=56,79
        write(data(1)(i:i),'(a1)') ' '
      enddo
      if (hyp_mag_all(1).ne.-999.) then
        write(data(1)(56:59),'(f4.1)') hyp_mag_all(1)
        write(data(1)(60:60),'(a1)') hyp_mag_type_all(1)
        write(data(1)(61:63),'(a3)') hyp_mag_agency_all(1)
      endif
      write(3,'(a)') data(1)(1:80)
      write(3,'(a)') ' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI'//
     &     ' AZIMU VELO AIN AR TRES W  DIS CAZ7'
      write(3,'(a)')
c
c   get next event
c
      goto 50
c
c     end of file
c
 1000 continue
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      close(3)              ! close output file
      write(6,*) 'Number of events in input file: ', nevent
      write(6,*) 'Output file name:  norcsv.txt'
      write(6,*) 'Header lines with selected magnitude: header.txt'

      stop
      end
