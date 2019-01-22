C
c    program will plot macroseismic observations using GMT
c    can read web files and  seisan storage files,
c    web files are converted to seisan files
c   
c    input is a web file or a seisan macroseismic file
c
c    output is a gmt file macromacp.gmt with commands to plot the
c    macroseismic observations together with an input file iso.inp.
c    within the program, the gmt script is executed and the map ps
c    file macromap.eps displayed.
c
c    if input is from a web file made with macroquest, the plot is also 
c    made and an output file macromap.out is also generated in seisan 
c    macroformat. the program then needs an input file with postal codes.
c
c    The program can also take input from the prompt line:
c
c      -macroinput        file with macroseismic observations,
c                         abs path or in ISO
c      -placename         additional file with place names,
c                         abs path or in DAT, epimap format
c      -postfile          file with postal code, abs path
c
c      -lat               epicenter latitude
c
c      -lon               epicenter longitude
c    
c
c    If used with eev, the place name file must have name place_names.macro
c    
c
c    j. havskov, january 2005
c
c  changes
c
c  feb 15 2006 jh : add epicenter 
c  apr 5  2006 jh : add correct seisan output file name if web input
c  jun 20 2006 jh : change ps to eps
c  jan 26 2007 bm : change colors
c  mar 6  2007 jh : end criteria not err but '---' for macro fil
c  mar 23 2007 jh : make sure size of map ok if only one point
c  may 28 2008 jh : make map 0.1 deg larger in all directions
c  oct 31 2008 jh : change user/bin to bin
c  jan 4  2010 jh : increase dimensions
c  feb 03 2011 jh : do not plot if on pc
c  feb 24 2011 mb : -P in GMT command
c  mar 19 2013 jh : fix problem with norwegian chars, 
c                   add ip number and street
c  may 20 2013 jh : add reading of lat lon
c  mar 26 2014 lo : check for error when reading lat and lon
c 
c  mar    2016 mk : search by address added to script         
c  feb 22 2017 jh : break up long comment line to 80 char lines
c                   so all is included
c  feb 22 2017 jh : break up long comment line to 80 char lines
c                   so all is included. in some cases there is
c                   a CR in string so following strings will
c                   only have first 80 chars copied
c  jan 18 2017 jh : fix problem with output of zero intensity, if
c                   intensity is blank in output MACRO file, do not plot
c
      implicit none
      include 'libsei.inc'

      character*120 text(100)		! general text
      character*80  txt                 ! general text
      character*1200 long_text          ! for  comments
      character*80 txt_all(90000)       ! whole web file
      character*80 macroinput 		! input file with lat,lon and intensity
cp    character*80 post_file            ! file with postal codes
      real lat(15000),lon(15000)	! latitutde and longitude
      real intensity(15000)		! intensity
      real max_lat,min_lat		! latitude limits
      real max_lon,min_lon		! longitude limits
      real epi_lat,epi_lon              ! epicenter
      integer n_observations		! number of intnesity points
      character*80 title		! plot title
      character*80 place_name_file	! place name file, seisan format
      character*80 p_name		! place name
      integer seiclen                   ! function to get length of string
      logical plot_gmt                  ! if true, plot 
      real x,y                          ! help variable
      character*3 format                ! format of input file
      integer narg                      ! number of arguments
      character*80 arg(40)              ! arguments
      integer npost                     ! number of postal codes
cp    character*10 post_code(10000)     ! postal codes
      character*10 ppost                ! one postal code
cp    character*30 psite(10000)         ! postal sites
      character*30 one_site             ! coordinate source
      character*25 reported_site        ! reported site
      character*25 address1,address2    ! road address
      character*120 addressline         ! addresss-string used for search 
      character*80 line                 ! response, address-search
      character*130 command             ! command to run address-search
      character*15 ip_number            ! ip number
      character*80 file_name            ! seisan type output file name
      real plat(10000),plon(10000)      ! lat-lon of postal sites
      integer year,month,day,hour,min,sec
      integer local_hour                ! local hour
      integer nweb                      ! number of lines in web file
      integer n_bad                      ! number of missign postal codes
      real ratio                        ! ratio between yand x on plot
      real xsize                        ! x size in cm
      logical pc,linux,sun
      real swap                         ! var for swapping coordinates
      integer cflag                     ! source off coordinates
      integer nevent                    ! event-counter
      character zoom, zoom_in           ! var for GMT map zoom
      real min_lat_n, max_lat_n         ! new coordinate range zoom
      real min_lon_n, max_lon_n

      integer i,k,kk,code,l,m,ll

      plot_gmt=.true.
c
c   get computertype
c    
      call computer_type(sun,pc,linux)
c
      if(pc) plot_gmt=.false.
      place_name_file=' '
cp      post_file=' '
      epi_lat=999.0
      epi_lon=999.0
      n_bad=0

c
c   get arguments
c
      call get_arguments(narg,arg)

      if(narg.lt.2) then
c
c   get file names
c
         write(6,*)' Input file name with macroseismic data'
         read(5,'(a)') macroinput
         write(6,*)' Place name file, return for none'
         read(5,'(a)') place_name_file
cp        write(6,*)' File with postal codes, return for none',
cp    &   ' or default if web file input'
cp        read(5,'(a)') post_file
      else
         do i=1,narg
            if(arg(i)(1:11).eq.'-macroinput') then
               macroinput=arg(i+1)
            endif
            if(arg(i)(1:10).eq.'-placename') then
               place_name_file=arg(i+1)
            endif
cp           if(arg(i)(1:10).eq.'-postfile') then
cp             post_file=arg(i+1)
cp          endif
            if(arg(i)(1:4).eq.'-lat') then
               read(arg(i+1),'(f8.3)') epi_lat
            endif
            if(arg(i)(1:4).eq.'-lon') then
               read(arg(i+1),'(f8.3)') epi_lon
            endif
          enddo
      endif
      if(epi_lat.lt.900) write(6,*)' Epicenter',epi_lat,epi_lon
c
c   make file with color tables
c
      open(1,file='macro.cpt',status='unknown')
      write(1,'(a)')
     *'0       255     255     0       3     255       255     0'
      write(1,'(a)')
     *'3       255     200     100     4     255       200     100'
      write(1,'(a)')
     *'4       160     96      224     5     160        96     224'
      write(1,'(a)')
     *'5       0       255     0       6       0       255       0'
      write(1,'(a)')
     *'6       0       0       255     7       0         0     255'
      write(1,'(a)')
     *'7       255       0     0       8       255       0       0'
      write(1,'(a)')
     *'8       255     255     255     9       255     255     255'
      write(1,'(a)')
     *'B         0     224     255'
      write(1,'(a)')
     *'F       0       0       0'
      write(1,'(a)')
     *'N       128     128     128'
      close(1)
c
c   open file with macroseismic info
c

c
c   open file in ISO if fom eev (inputy from prompt)
c
      if(narg.ge.2) then
         call sei get file( open$,            ! Get file.
     &                          k,            ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'ISO',            ! Alternative directory to search.
     &                      macroinput)       ! For this station file.
         if(code.ne.e_ok$) then
            write(6,*)' Macroseismic file missing'
            stop
         endif
      else
         k=10
         open(k,file=macroinput,status='old')
      endif


c
c   find format, new or web
c
      format=' '
c
c  try  if web format
c
      read(k,'(a)') txt
      if(txt(1:20).eq.'--------------------') then
          format='web'
          goto 6
      endif
c
c   try new format
c
      rewind k
      read(k,'(a)') txt
      if(txt(49:51).eq.'GMT') then
         format='new'
         goto 6
      endif

 6    continue
      rewind k

      if(format.eq.' ') then
         write(6,*)' Unknown format'
         stop
      else
         write(6,'(a,a)')' Format: ',format
      endif
c
c   if web format, input date etc
c
      if(format.eq.'web') then
      write(6,*)' enter GMT year,month,day,hour,min,sec'
      read(5,*) year,month,day,hour,min,sec
      write(6,*) 'enter local hour'
      read(5,*) local_hour
      write(6,*)' enter location, max 30 chars'
      read(5,'(a)') txt(1:40)
      write(6,*) txt(1:40)
      write(file_name,'(i4,a,i2,a,i2,a,2i2,a,i2,a)')
     &year,'-',month,'-',day,'-',
     &hour,min,'-',sec,'.MACRO'
      do i=1,21
        if(file_name(i:i).eq.' ') file_name(i:i)='0'
      enddo
      write(6,*) ' File name for SEISAN ', file_name
      open(9,file=file_name,status='unknown')
      write(9,
     &'(a,i4,1x,2i2,1x,2i2,1x,i2,a,i4,1x,2i2,1x,2i2,1x,i2,a)')
     & txt(1:30), year,month,day,hour,min,sec,' GMT ',
     & year,month,day,local_hour,min,sec,' Local time'
       write(9,*)'Comment'
      endif


cp  open and read postal codes, if a web file, also try to use
cp  default file if no name is given
cp
cp     npost=0
cp     kk=0
cp
cp  first check if a default post file is to be used
cp
cp     if(format.eq.'web'.and.post_file.eq.' ') then
cp       call sei get file( open$,            ! Get file.
cp    &                          kk,           ! Opened on this unit.
cp    &                      code,             ! Returned condition.
cp    &                      'DAT',            ! Alternative directory to search.
cp    &                      'postal_code.dat')! For this file.
cp        if(code.ne.e_ok$) then
cp           write(6,*)' File postal_code.dat is missing'
cp           stop
cp        endif
cp     endif
cp
cp  specific name is given
cp
cp
cp     if(post_file.ne.' ') then
cp        kk=1
cp        open(kk,file=post_file,status='old')
cp     endif
cp
cp  now read if a file there
cp
cp     if(kk.gt.0) then
cp        i=1
cp1       continue
cp        read(kk,'(a10,2f10.3,2x,a30)',end=2)
cp    &   post_code(i),plat(i),plon(i),psite(i)
cp        I=I+1
cp        goto 1
cp2       continue
cp        npost=i-1
cp        write(6,*) ' Number of postal codes', npost
cp        close(kk)
cp     endif

c 
c   read observations file
c 
      if(format.eq.'old') then
         read(k,'(a)') text(1)
         do i=1,10000
            n_observations=i-1
            read(k,'(28x,2f7.2,8x,f3.1)',end=10,err=3) 
     &      lat(i),lon(i),intensity(i)
            goto 4
  3         continue
            write(*,*) ' error reading ',k
  4         continue
         enddo
 10      continue
      endif
     
      if(format.eq.'new') then
         m=1
         read(k,'(a)')title(1:48) 
         read(k,'(a)') txt
         do i=1, 10000
           read(k,'(a)',end=5) txt
           if(txt(1:5).eq.'-----') goto 5     ! new section with original data
           read(txt,'(2f10.3,f6.1)') 
     &     lat(m),lon(m),intensity(m)
           if(lat(m).eq.0.0.or.lon(m).eq.0.0.or.txt(21:26).eq.' ') then
              m=m-1
           endif
           m=m+1
         enddo
 5       continue
         n_observations=m-1
         write(6,*) 'Number of valid observations: ',n_observations
      endif
c
c----------------read iput from web file-----------------------------
c
      address1=''
      address2=''
      addressline=''
      reported_site=''
      ip_number=''

      if(format.eq.'web') then
         i=0
         ll=0
cx       open(2,file='macromap.out',status='unknown')
         open(7,file='macromap_bad.out',status='unknown')
cx        read(k,'(a)') txt
cx        txt_all(ll)=txt
cx        ll=ll+1
cx        read(k,'(a)') txt
cx        txt_all(ll)=txt
cx        ll=ll+1
cx        read(k,'(a)') txt
cx        txt_all(ll)=txt
cx        ll=ll+1

c-------------------------------------------------------
c   loop for reading observations starts here
c---------------------------------------------------------
          nevent=0
  7    continue
c
c   remove nasty characters
c
         read(k,'(a)',end=8) txt  
         open(8,file='sed.tmp',status='unknown')
         write(8,'(a)') txt(1:seiclen(txt))
         close(8)
         write(command,'(a)') "sed -i 's/å/a/Ig ; "//
     &     "s/ø/o/Ig ; s/æ/e/Ig ; s/?/+/Ig ; "//
     &     "s/|/+/Ig ; s/;/+/Ig ; " //
     &     "' sed.tmp"
         call systemc(command(1:seiclen(command)),seiclen(command))  
         open(8,file='sed.tmp',status='unknown')
         read(8,'(a)') txt
         close(8)

c         do m=1,seiclen(txt)
c            if(ichar(txt(m:m)).lt.32.or.ichar(txt(m:m)).gt.122) then
c              txt(m:m)=' '
c            endif
c         enddo

c
c   append new line to all text
c
         ll=ll+1
c
c   check that not a long line with comments
c
         if(txt(1:10).eq.'kommentar:') then
            backspace k
            long_text=' '
            read(k,'(a)') long_text

c
c   remove nasty characters
c
         open(8,file='sed.tmp',status='unknown')
         write(8,'(a)') long_text(1:seiclen(long_text))
         close(8)
         write(command,'(a)') "sed -i 's/å/a/Ig ; "//
     &     "s/ø/o/Ig ; s/æ/e/Ig ; s/?/+/Ig ; "//
     &     "s/|/+/Ig ; s/;/+/Ig ; " //
     &     "' sed.tmp"
         call systemc(command(1:seiclen(command)),seiclen(command))
         open(8,file='sed.tmp',status='unknown')
         read(8,'(a)') long_text
         close(8)

            kk=seiclen(long_text)
            do m=1,kk,80
              txt_all(ll)=long_text(m:m+80-1)
              ll=ll+1
            enddo
            ll=ll-1
         else
            txt_all(ll)=txt
         endif
c
c   count new observation
c
         if(txt(1:10).eq.'----------') then ! new observation
           nevent=nevent+1
           i=nevent    ! counter for observations
           write(*,'(a,i5)') '---------------- obs number: ',nevent
           goto 7    ! lo 14/03/2016
         endif

c
c   read intensity    
c       
         if(txt(1:10).eq.'intensitet') then
            if(txt(19:20).eq.'II') intensity(i)=2
            if(txt(19:21).eq.'III') intensity(i)=3
            if(txt(19:20).eq.'IV') intensity(i)=4
            if(txt(19:20).eq.'V ') intensity(i)=5
            if(txt(19:20).eq.'VI') intensity(i)=6
            if(txt(19:21).eq.'VII') intensity(i)=7
            if(txt(19:22).eq.'VIII') intensity(i)=8
            if(txt(19:20).eq.'IX') intensity(i)=9
            if(txt(19:20).eq.'X ') intensity(i)=10
            goto 7
         endif
c
c   read ip number
c
         if(txt(1:3).eq.'ip:') then
           ip_number=' '
           ip_number=txt(19:33)
           goto 7
         endif
c
c   read site
c
         if(txt(1:9).eq.'poststed:') then
            reported_site=' '
            reported_site=txt(19:43)
            addressline=''
            if (seiclen(address1).gt.0) then
              addressline=address1(1:seiclen(address1))
            endif
            if (seiclen(address2).gt.0) then 
              addressline=addressline(1:seiclen(addressline))//' '//
     &                  address2(1:seiclen(address2))
            endif
            if (seiclen(reported_site).gt.0) then
              addressline=addressline(1:seiclen(addressline))//' '//
     &                  reported_site(1:seiclen(reported_site))
            endif
            do l=1,seiclen(addressline)
              if (addressline(l:l).eq.' ') addressline(l:l)='+'
            enddo
            if (seiclen(addressline).gt.0) then
              command='address '//addressline(1:seiclen(addressline))// 
     &                  '> address.tmp'
              write(*,*) command(1:seiclen(command))
              call systemc(command(1:seiclen(command)),seiclen(command))  
c                
c   address-search
c
              line=' '
              open(8,file='address.tmp',status='unknown')
              read(8,*) 
              read(8,'(a)') line
              if (line.eq.'Latitude, Longitude not found.') then
                write(*,*) ' address not found ',
     &           addressline(1:seiclen(addressline))
              else
                read(line,*) lat(i),lon(i)
                cflag=3
                write(*,*) 'lat/lon from address ',lat(i),lon(i)
                one_site='address search'
              endif
              close(8)
            endif
            goto 7
         endif
c
c   read address
c
         if(txt(1:8).eq.'adresse1') then
            address1=' '
            address1=txt(19:43)
            goto 7
         endif
         if(txt(1:8).eq.'adresse2') then
            address2=' '
            address2=txt(19:43)
            goto 7
         endif

c 
c flag to indicate where coordinates come from; cflag = 1 (postal code),
c  = 2 (coordinates); = 3 (coordiniates from address)
c
          cflag=0
c
c   postal code
c
         if(txt(1:6).eq.'postnr') then
           ppost=txt(19:28)
cp
cp  find lat long corresponding to postal code
cp
cp          if(npost.eq.0) then
cp            write(6,*)' No post code file'
cp            stop
cp          else
cp            one_site='code unknown'
cp            lat(i)=999.
cp            lon(i)=999.
cp            do l=1,npost
cp              if(ppost(1:4).eq.post_code(l)(1:4)) then
cp                 lat(i)=plat(l)
cp                 lon(i)=plon(l)
cp                 one_site=psite(l)
cp                 cflag=1
cp              endif
cp            enddo
cp          endif
cp          goto 7
         endif
c
c   check if coordiantes, then use those instead of post code location
c
        if(txt(1:10).eq.'lengdegrad') then
           if(txt(12:27).ne.' ') then
             read(txt(12:27),'(f14.3)',err=27)lon(i)
c             write(*,*) ' debug lo lon ',i,lon(i)
             cflag=2
           endif
 27        continue
           goto 7
        endif
c
c   last info for observation, then write out
c
        if(txt(1:10).eq.'breddegrad') then
          if(txt(12:27).ne.' ') then
            read(txt(12:27),'(f14.3)',err=30) lat(i)
c            write(*,*) ' debug lo lat ',i,lat(i)
          endif
          if(one_site.ne.'address search') then
             one_site='user given lat/lon'  !write out source of coordinates
          endif
c
c    check if user-given lat/lon must be swapped
c
          if ((lat(i).ne.999..and.lon(i).ne.999.).and.
     &        (lat(i).ne.0..and.lon(i).ne.0.)) then
            if(lon(i).gt.-10..and.lon(i).lt.35..and.
     &        lat(i).gt.40..and.lat(i).lt.85.) then
c   do nothing 
            else
c   swap coordinates
              swap=lat(i)
              lat(i)=lon(i)
              lon(i)=swap
              write(*,*) ' swapped lat and lon ',lat(i),lon(i)
            endif
          endif

 30      continue       

c
c   check if valid area
c
          if(lat(i).le.85.and.lat(i).ge.40..and.   ! valid area
     &       lon(i).le.35.and.lon(i).ge.-10.) then
             write(9,'(2f10.3,2x,f3.1,1x,a3,1x,a10,2x,a20,a15,2a25)')
     &       lat(i),lon(i),intensity(i),'EMS',
     &       ppost,one_site(1:20),ip_number,address1,
     &       reported_site
          else   
c   if not correct coordinates write to bad-output file.
             write(7,'(2f10.3,2x,f3.1,1x,a3,1x,a10,2x,a20,a15,2a25)')
     &       lat(i),lon(i),intensity(i),'EMS',
     &       ppost,one_site(1:20),ip_number,address1,
     &       reported_site
c             i=i-1   ! lo
             n_bad=n_bad+1
          endif
        endif
c
c read next line
c
        goto 7       

c
c   ----------- finished reading input web file------------------
c   ----------- creating output files ---------------------------
c

 8       continue
         nweb=ll-1
         n_observations=i 
c        write(*,*) ' debug lo ',i
c
c   write original web observation
c
         do i=1,nweb
           write(9,'(a)') txt_all(i)
         enddo
         close(9)
      endif   ! new format
      if(narg.ge.2) call sei close (close$,k,code)

c
c  open file with place names, read and write
c
      if(place_name_file.ne.' ') then
c
c   open file in DAT
c
         call sei get file( open$,            ! Get file.
     &                          m,            ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'DAT',            ! Alternative directory to search.
     &                      place_name_file)  ! For this station file.
         if(code.ne.e_ok$) then
            write(6,*)' Place name file missing'
         endif

        open(4,file='place_name.gmt',status='unknown')
        k=0
 11     continue
        read(m,'(a)',end=12) txt
        k=k+1
        i=index(txt,'  ')
        p_name=txt(1:i)       ! save name
        txt(1:80-i+1)=txt(i:80) ! strip off name
        call sei get values( 2, txt, code )        ! Parse text.
        y=array$(1)                         
        x=array$(2)
        write(4,'(2f8.3,a,a)') 
     &  x,y,' 12 0 1 CM ',p_name(1:seiclen(p_name))
        goto 11
 12     continue
        write(6,*)' Number of place names', k
        call sei close (close$,m,code)
        close(4)
      endif
c
c   write out for gmt script
c
      open(3,file='iso.inp',status='unknown')
      k=1
      max_lat=-100.0
      min_lat=100.0
      max_lon=-400.0
      min_lon=400.0
      do i=1,n_observations
          if(lat(i).ge.54.and.lat(i).le.85.and. ! limited to Norway
     &      lon(i).ge.-10.and.lon(i).le.35) then
              lat(k)=lat(i)
              lon(k)=lon(i)
              k=k+1
              write(3,*) lon(i),lat(i),intensity(i)
            
            if(lat(i).gt.max_lat) max_lat=lat(i)
            if(lat(i).lt.min_lat) min_lat=lat(i)
            if(lon(i).gt.max_lon) max_lon=lon(i)
            if(lon(i).lt.min_lon) min_lon=lon(i)
            write(56,*) i,max_lat,min_lat
          endif
      enddo
      close(3)

c
c   if epicenter is given, make sure it is on map
c
      if(epi_lat.lt.900.0) then
        if(epi_lat.gt.max_lat) max_lat=epi_lat
        if(epi_lat.lt.min_lat) min_lat=epi_lat
        if(epi_lon.gt.max_lon) max_lon=epi_lon
        if(epi_lon.lt.min_lon) min_lon=epi_lon
      endif
c
c   make sure min and max is not the same
c
      if(max_lat-min_lat.lt.0.1) max_lat=max_lat+0.1
      if(max_lon-min_lon.lt.0.1) max_lon=max_lon+0.1
c
c   make it a bit bigger than observations
c
      max_lat=max_lat+(max_lat-min_lat)*0.2
      max_lat=max_lat+0.1
      
      min_lat=min_lat-(max_lat-min_lat)*0.2
      min_lat=min_lat-0.1
c
c   ratio on map
c
      ratio=(max_lat-min_lat)/((max_lon-min_lon)*cos(max_lat/57.0))
c
c   horizontal size. if both y and x size the same, map will be 12 cm wide
c
      xsize=34.0/ratio
      if(xsize.gt.12.0) xsize=12.0

      max_lon=max_lon+(max_lon-min_lon)*0.2
      max_lon=max_lon+0.1

      min_lon=min_lon-(max_lon-min_lon)*0.2
      min_lon=min_lon-0.1

      zoom='n'
 9    continue
      if(zoom.ne.'n') then
         min_lat=min_lat_n
         max_lat=max_lat_n
         min_lon=min_lon_n
         max_lon=max_lon_n
      endif

cp     write(6,*)' Number of observations with postal code', 
cp    &           n_observations
cp     write(6,*)' Number of observations without postal code', 
cp    &           n_bad
      write(6,*)' Lat-lon range ',
     &            min_lat,max_lat,min_lon,max_lon
  
c
c   open file with gmt output script
c
      open(2,file='macromap.gmt',status='unknown')

      k=1
      text(k)='#!/bin/csh'  	    		! start of file
      k=k+1
      text(k)='gmtset UNIX_TIME FALSE'
      k=k+1
      text(k)='gmtset WANT_EURO_FONT TRUE'
      k=k+1
      text(k)='gmtset HEADER_FONT_SIZE 14'
      k=k+1
      write(text(k),'(a,4(f6.1,a))')'set REGION=',	! region limits
     &          min_lon,'/',max_lon,'/',min_lat,'/',max_lat,' '
      do i=12,36
        if(text(k)(i:i).eq.' ') text(k)(i:i)='0'        ! no blanks allowed
      enddo
      k=k+1
c
c   calculate horizontal scale in cm/deg
c
      x=xsize/(max_lon-min_lon)
      write(text(k),'(a,f5.2)')'set PROJ=m',x		! scale     
      if(text(k)(11:11).eq.' ') text(k)(11:11)='0'
      if(text(k)(12:12).eq.' ') text(k)(12:12)='0'
      k=k+1
      write(text(k),'(a,a,a)')'set TITLE="',title(1:48),'"' ! title
      k=k+1
      text(k)='set OUTFILE=macromap.eps'	! ps output file
      k=k+1
      text(k)='set INFILE=iso.inp'
      k=k+1
      write(text(k),'(a,f6.1)') 'set LEGLON=',max_lon+0.1  ! legion corner
      do i=12,15
        if(text(k)(i:i).eq.' ') text(k)(i:i)='0'           ! no blanks allowed
      enddo

      k=k+1
      write(text(k),'(a,f6.1)')'set LEGLAT=',max_lat       ! legion corner
      do i=12,15
        if(text(k)(i:i).eq.' ') text(k)(i:i)='0'           ! no blanks allowed
      enddo
      k=k+1
c
c   epicenter
c
      if(epi_lat.lt.900) then
         write(text(k),'(a,f9.3)')'set EVLON=',epi_lon
         do i=11,19
           if(text(k)(i:i).eq.' ') text(k)(i:i)='0'        ! no blanks allowed
         enddo
         k=k+1
         write(text(k),'(a,f9.3)')'set EVLAT=',epi_lat
         do i=11,19
           if(text(k)(i:i).eq.' ') text(k)(i:i)='0'        ! no blanks allowed
         enddo
         k=k+1
       endif
c
c  place names
c
      if(place_name_file.ne.' ') then
         text(k)='set PLACEFILE=place_name.gmt'
         k=k+1
      endif
c     text(k)='makecpt -Crainbow -I -T0/8/1  >! macro.cpt'
c
c   fixed commands
c

      text(k)='psbasemap -R$REGION -J$PROJ -P -B1.0f10m:'//
     &         '."$TITLE":WSne -K >! $OUTFILE'
      k=k+1
      text(k)='pscoast -R$REGION -J$PROJ -B1.0f10m:.'//
     &         '"$TITLE":WSne -Dh -G255/224/192 -S128/192/224 -W2 '//
     &          '-O -K >> $OUTFILE'
      k=k+1
      if(place_name_file.ne.' ') then
         text(k)='psxy $PLACEFILE -R$REGION -J$PROJ -O'//
     &   ' -Ss0.3 -G0 -K >> $OUTFILE'
         k=k+1
         text(k)='pstext $PLACEFILE -R$REGION -J$PROJ'//
     &   ' -D0/0.4 -O -K >> $OUTFILE'
         k=k+1
      endif
c
c   epicenter
c
      if(epi_lat.lt.900) then
         text(k)='psxy -R$REGION -J$PROJ -B1.0f10m:."$TITLE":WSne'//
     &   ' -O -K -Sa0.8c -G255/255/0 -W1 << EOF >> $OUTFILE'
         k=k+1
         text(k)='$EVLON $EVLAT'
         k=k+1
         text(k)='EOF'
         k=k+1
      endif
 
      text(k)='psxy $INFILE -R$REGION -J$PROJ -B1.0f10m:.'//
     &         '"$TITLE":WSne'//
     &         ' -Cmacro.cpt -O -Sc0.3c -W -G0/0/0 -K >> $OUTFILE'
      k=k+1
      if(epi_lat.lt.900) then
         text(k)='pslegend -R$REGION -J$PROJ -D$LEGLON/$LEGLAT/'//
     &           '2.8/5.2/TL -O -F -G255/255/255 << EOF >> $OUTFILE'
         k=k+1
      else
         text(k)='pslegend -R$REGION -J$PROJ'//
     &           ' -D$LEGLON/$LEGLAT/2.3/4.7/TL'//
     &           ' -O -F -G255/255/255 << EOF >> $OUTFILE'
         k=k+1
      endif
      text(k)='C 0/0/0'
      k=k+1
      text(k)='H 10 1 Intensity'
      k=k+1
      text(k)='D 0.1 1'
      k=k+1
      text(k)='N 1'
      k=k+1
      text(k)='S 0.2 c 0.3c 255/255/0 1p 0.6c I = 0-II '
      k=k+1
      text(k)='S 0.2 c 0.3c 255/200/100 1p 0.6c I = III'
      k=k+1
      text(k)='S 0.2 c 0.3c 160/96/225 1p 0.6c I = IV'
      k=k+1
      text(k)='S 0.2 c 0.3c 0/255/0 1p 0.6c I = V'
      k=k+1
      text(k)='S 0.2 c 0.3c 0/0/255 1p 0.6c I = VI'
      k=k+1
      text(k)='S 0.2 c 0.3c 255/0/0 1p 0.6c I = VII'
      k=k+1
      text(k)='S 0.2 c 0.3c 255/255/255 1p 0.6c I = VIII'
      k=k+1
      if(epi_lat.lt.900.0)then 
         text(k)='S 0.2 a 0.5c 255/255/0 1p 0.6c epicenter'
         k=k+1
      endif
      text(k)='EOF'
c
c   write gmt script
c

      do i=1,k 
         write(2,'(a)') text(i)(1:seiclen(text(i)))
      enddo
      close(2)
 
c
c   plot gmt script if that option
c
      if(plot_gmt) then
        call systemc('chmod +x macromap.gmt',21)   ! make it executable
        call systemc
     &  ('macromap.gmt',12)    ! run gmt

        call systemc('gv macromap.eps',15)         ! plot

c       call systemc(
c    &   'gs -sDEVICE=jpeg -sOutputFile=macromap.jpeg macromap.eps',
c    &    56)

        zoom_in='n' 
        min_lat_n=min_lat
        max_lat_n=max_lat
        min_lon_n=min_lon
        max_lon_n=max_lon

        write(*,*) 'Press y for different coordinate range, else enter'
        read(5,'(a)') zoom_in
        write(*,*) zoom_in
        if(zoom_in.eq.'y') then
           zoom=zoom_in
           write(*,*) ' Enter new latitude range (e.g. 60 70): '
           read(5,*) min_lat_n,max_lat_n
           write(*,*) ' Enter new longitude range (e.g. 5 10): '
           read(5,*) min_lon_n,max_lon_n
           write(*,*) ' values entered ', 
     &       min_lat_n,max_lat_n,min_lon_n,max_lon_n
           goto 9
        endif        

c       call systemc('gv macromap.eps',15)         ! plot
        call systemc(
     &   'gs -sDEVICE=jpeg -sOutputFile=macromap.jpeg macromap.eps',
     &    56)

      endif

      if(format.eq.'web') then
         write(6,*)
     &   ' Output file with identified sites is macromap.out'
         write(6,*)
     &   ' Output file with un-identified sites is macromap_bad.out'
      endif
      stop
      end
