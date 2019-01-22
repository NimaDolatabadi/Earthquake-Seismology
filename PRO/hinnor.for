c--------------------------------------------------------------------------
c  Program for reading hypoinverse archive format data and converting to 
c  nordic format.
c
c  All events are assumed local
c  No check if header time corresponds to phase times

c  Little tested
c  jh november 2011
c  
c jan 8 2013 jh: fix small format change, add posibility for arc line
c oct   2014 jh: add many more parameters
c dec 5 2016 jh: fix id line, prevented split
c
c--------------------------------------------------------------------------c
c
c  For detail on parameters and variables names, see rea.inc
c
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*150 text
      character*80 infile                 ! input file
      character*3  agency                 ! agency for mag and hypo
      double precision  abstime
      integer doy                         ! day of year
c-- arguments
      character*80 args(10)    
c---number of arguments 
      integer nars       

      logical all                         ! true: read all data, false: headers
      logical eof                         ! true: end of file
      logical head                        ! true if header info
      logical read_head                   ! true if header should be read
      logical dup                         ! true if duplicate header
      character*1 answer
      integer code                        ! error return code
      real min                            ! minute of degree
      real x                              ! help variable
      integer nevent                      ! number of events in file
      integer ev_number                   ! original event number
      logical arc                         ! true if arc line
      integer i,k                         ! counters
c
c   open output file
c
       open(2,file='hinnor.out',status='unknown')
        
       infile=' '
c
c   check if input from file given in argument
c
        call get_arguments(nars,args)
        if(nars.eq.1) then
           infile=args(1)
        endif   
c
c   get input file name, check if exist
c

 9    continue
      if(infile.eq.' ') then
         write(6,*) 'Give input file'
         read(5,'(a)') infile
      endif
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      infile=' '
      goto 9
 11   continue

      all=.true.                  ! read all parameters
 
      nevent=0                    ! initialize counter
      read_head=.true.            ! header should be read first time

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c


c
c   new event
c
  50  continue

c
c   clear variables
c
      rea_nphase=0      ! count number of phase lines
      rea_nhyp=0        ! count number of hypocenters
      rea_nwav=0        ! --------------- waveform files
      rea_ncomment=0    ! --------------- comment lines
      rea_nmacro=0      ! --------------- macroseismic data
      rea_nfault=0      ! --------------- fault plane solutions
      rea_nspec=0       ! --------------- spectral solutions
      rea_nmag=0        ! --------------- magnitudes
      rea_id_line=' '   ! no id line

      k=0               ! phase counter
      call rea_hyp_clear(1)
      call rea_hyp_clear(2)
      do i=1,500
         call rea_phase_clear(i)
      enddo
     
      rea_nwav=0  
      rea_locality=' '   
c
c  read one line
c
 20   continue
      read(1,'(a)',end=300,err=300) text

c
c   check if end of event
c
      if(text(1:20).eq.' ') then
         read_head=.true.  
         goto 30  ! end of one event, write out
      endif

         if(read_head) then
            read_head=.false.                   
c
c  
c
       
c            if(text(19:19).eq.'N'.or.text(19:19).eq.'S') then
c            if(text(20:24).ne.' ') then
c
c   id line
c
               rea_id_line=' '
               rea_id_line(65:74)=text(137:146)
               if(rea_id_line(65:65).eq.' ') rea_id_line(65:65)='0'
c assume year is ok
               rea_id_line(61:64)=text(1:4)
               rea_id_line(2:11)='ACTION:HIN' 
               rea_id_line(58:60)='ID:'
               rea_id_line(80:80)='I'             
               head=.true.
               read(text(1:4),'(i4)') hyp_year(1)
               read(text(5:6),'(i2)') hyp_month(1)
               read(text(7:8),'(i2)') hyp_day(1)
               read(text(9:10),'(i2)') hyp_hour(1)
               read(text(11:12),'(i2)') hyp_min(1)
               read(text(13:16),'(f4.2)') hyp_sec(1)
               i=hyp_sec(1)
               write(rea_id_line(61:74),'(i4,6i2)')
     *         hyp_year(1),hyp_month(1),hyp_day(1),
     *         hyp_hour(1),hyp_min(1),i
               do i=61,74
                 if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
               enddo
               rea_id_line(76:76)='L'
c location
               if(text(17:26).ne.' ') then
               read(text(17:18),'(i2)') i
               read(text(20:23),'(f4.2)') min
               hyp_lat(1)=i+min/60.0
               if(text(19:19).eq.'S') hyp_lat(1)=-hyp_lat(1)
c  lon
               read(text(24:26),'(i3)') i
               read(text(28:31),'(f4.2)') min
               hyp_lon(1)=i+min/60.0
               if(text(27:27).eq.' ') hyp_lon(1)=-hyp_lon(1)
               if(text(27:27).eq.'W') hyp_lon(1)=-hyp_lon(1)
               read(text(32:36),'(f5.2)') hyp_depth(1)
c
c   magnitude
c
               rea_nmag=0
               if(text(37:39).ne.' ') then
                  read(text(37:39),'(f3.2)')hyp_mag(1,1)
                  if(hyp_mag(1,1).ne.0.0) then
c
c   since this is amplitude based magnitude, we assume it is L
c
                     hyp_mag_type(1,1)='L'
                     rea_nmag=1
                  endif
c               endif
c   coda mag
c               if(text(71:73).ne.' ') then
                  rea_nmag=rea_nmag+1
                  read(text(71:73),'(f3.2)') hyp_mag(rea_nmag,1)
                  if(hyp_mag(rea_nmag,1).ne.0.0) then
                     hyp_mag_type(rea_nmag,1)='C'
                  else
                     rea_nmag=rea_nmag-1
                  endif
               endif
               endif

               read(text(49:52),'(f4.2)') hyp_rms(1)
               if(text(43:45).ne.' ') then
                  read(text(43:45),'(f3.0)') hyp_gap(1)
                  read(text(86:89),'(f4.2)')hyp_lat_err(1)
                  hyp_lon_err(1)=hyp_lat_err(1)
                  read(text(90:93),'(f4.2)') hyp_depth_err(1)
                  hyp_error(1)=.true.
               endif


cx             endif

             goto 20     ! read next line which should be a phase           
           endif

  
c
c   assume one more phase
c
      k=k+1
c
c   read phases on phase line
c
c
c   start with assumed p, could be blank, then s
c
      call rea_phase_clear(k)
      call rea_phase_clear(k+1)
        
      rea_stat(k)=text(1:5)
c
c   normal location for componet
c
      rea_co(k)(1:1)=text(10:10)
      rea_co(k)(2:2)=text(12:12)
c
c  if blank use seisan fix to trasnsfer 2 letter comp codes
c  when input file made with norhin
c
      if(rea_co(k)(1:1).eq.' ') then
         rea_co(k)(1:1)=text(87:87)
         rea_co(k)(2:2)=text(109:109)
      endif
      rea_onset(k)=text(14:14)
      rea_polarity(k)=text(16:16)
      if(rea_polarity(k).eq.'U') rea_polarity(k)='C'
      read(text(75:78),'(f4.1)') x
      if(x.gt.0.0) then
         rea_dist(k)=x
         read(text(79:81),'(f3.0)') rea_ain(k)
         read(text(92:94),'(f3.0)') rea_az(k)
      endif
      if(text(15:15).ne.' ') then
         rea_phase(k)=text(15:15) ! is p
         if(text(88:91).ne.' ') read(text(88:91),'(f4.0)') rea_coda(k)
         rea_weight_in(k)=text(17:17)
         if(text(39:41).ne.' ') then
            read(text(39:41),'(f3.2)') x   ! weight used
            i=x*10
            write(rea_weight_out(k),'(i2)') i
         endif
         read(text(30:34),'(f5.2)') rea_sec(k)
         if(text(35:38).ne.' ') 
     *   read(text(35:38),'(f4.2)') rea_res(k)     
      endif
c
c   date and time same for p and s except sec
c
      read(text(18:29), '(i4,4i2)') rea_year(k),rea_month(k),
     *rea_day(k),rea_hour(k),rea_min(k)   
                
c
c  now assume s, has same date and time except secs
c
c
c   there is a second phase on same line
c
      if(text(48:48).ne.' ') then

c
c   there was a p so a new phase, rest same as for p
c
         if(text(15:15).ne.' ')then
            k=k+1  ! new phase
            rea_stat(k)=rea_stat(k-1)
            rea_co(k)=rea_co(k-1)
            rea_year(k)=rea_year(k-1)
            rea_month(k)=rea_month(k-1)
            rea_day(k)=rea_day(k-1)
            rea_hour(k)=rea_hour(k-1)
            rea_min(k)=rea_min(k-1)
            rea_phase(k)=text(48:48)
            read(text(42:46),'(f5.2)') rea_sec(k)
            rea_dist(k)=rea_dist(k-1)
            rea_ain(k)=rea_ain(k-1)
            rea_az(k)=rea_az(k-1)
            

          endif   
c
c   only s-phase
c
         read(text(42:46),'(f5.2)') rea_sec(k)
         rea_onset(k)=text(47:47)
         rea_phase(k)=text(48:48)
         rea_weight_in(k)=text(50:50)
         if(text(64:66).ne.' ') then
            read(text(64:66),'(f3.2)') x   ! weight used
            i=x*10
            write(rea_weight_out(k),'(i2)') i
            read(text(51:54),'(f4.1)') rea_res(k)
         endif
      endif
c
c   go for next phase
c
      goto 20

c----------------------------------------------
c  end of one event, write out
c----------------------------------------------

 30   continue

      rea_nphase=k  

      do k=1,rea_nphase
c
c   since all times in output are referred to same minute
c   a new minute must be calculated not to get overflow in seconds
c
         call timsec(rea_year(k),rea_month(k),rea_day(k),
     *   rea_hour(k),rea_min(k),rea_sec(k),abstime)
         call sectim(abstime,rea_year(k),doy,rea_month(k),rea_day(k),
     *   rea_hour(k),rea_min(k),rea_sec(k))

      enddo

     
      nevent=nevent+1               ! count events

c
c   write out event
c
c      if(.not.head) then
c          hyp_year(1)=rea_year(1)
c          hyp_month(1)=rea_month(1)
c          hyp_day(1)=rea_day(1)
c          hyp_hour(1)=rea_hour(1)
c          hyp_min(1)=rea_min(1)
c          hyp_sec(1)=rea_sec(1)
c       endif

       hyp_dist_id(1)='L'
       rea_nhyp=1
       if(dup) then
          hyp_year(2)=hyp_year(1)
          hyp_month(2)=hyp_month(1)
          hyp_day(2)=hyp_day(1)
          hyp_hour(2)=hyp_hour(1)
          hyp_min(2)=hyp_min(1)
          hyp_sec(2)=hyp_sec(1)
          hyp_dist_id(2)=hyp_dist_id(1)
          hyp_lat(2)=hyp_lat(1)
          hyp_lon(2)=hyp_lon(1)
          hyp_depth(2)=hyp_depth(1)
          hyp_agency(2)=hyp_agency(1)
          hyp_rms(2)=hyp_rms(1)
          hyp_mag(1,2)=hyp_mag(1,1)
          hyp_mag_agency(1,2)=hyp_mag_agency(1,1)
          hyp_mag_type(1,2)=hyp_mag_type(1,1)
          rea_nhyp=2
        endif

       call rea_event_out(2,all,data,code)

c
c   write the whole first header line to screen
c
      write(6,'(a)') data(1)(1:79)
c
c   get next event
c
c      read_head=.true.
c      if(.not.eof) goto 50
       goto 50

 300   continue
c
c     end of file
c
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is hinnor.out'

      stop
      end
