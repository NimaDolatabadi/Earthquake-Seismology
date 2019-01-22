c     iscloc_wrap()
c
c  jan 7 2013 jh : comment out seidim.inc since now in hypparm.inc
c  2015-06-02 pv   : add varibles to hyp common block due to
c                    compiler warning
c
c     Subroutine to go between fortran update or hyp and C iscloc.
c
c     Calls:
c           subroutines from rea.for
c           read_stat() from this file.
c           write_hyp() from this file.
c           iscloc() from iscloc.c
c           functions from libsei.inc
c
c     Only read the station file on first visit (nstat=0), store
c     station data with this routine between calls using save.
c     Exception to this is if model in current s-file is different to 
c     that in the previous one - in which case station file is different.
c
c     This is only place that model/station file gets read so read
c     agency and test array here and pass back to calling routine.
c     read_stat() writes mag_cor to /hyp/ common block.

      subroutine iscloc_wrap(iustat,iulst,data,nhead,nrecord,agency,
     &                       test,locate,use_eev)

      implicit none
      save

      external iscloc                     ! C function

      include 'hypparm.inc'               ! nstats, data, test
c      include 'seidim.inc'                ! needed for rea.inc
      include 'rea.inc'                   ! for reading s-files

c Library file open routine needs these
      include 'libsei.inc'                 ! Library definitons.
      external sei open                    ! File open handler.
      external sei close                   ! & closure.
      integer  code                        ! Condition.
      logical  b_flag                      ! A problem flagged?.

c Arguments
      integer iustat                      ! lu to use for station file
      integer iulst                       ! print.out logical unit
      integer nhead                       ! number of headers in data
      integer nrecord                     ! number of lines in data
      character*3 agency                  ! agency read from station list
      logical locate                      ! return true on success 
      logical use_eev                     ! flag controlling output

c Arrays to store staion info - nstats set in hypparm.inc
      character*5 statid(nstats)
      real sta_lat(nstats),sta_lon(nstats)
      integer sta_elev(nstats)
      integer nstat                   ! number of stations in station list

c Arrays for phase data extra to the ones in rea.inc
      real        pha_lat(max_data/2)
      real        pha_lon(max_data/2)
      real        pha_elev(max_data/2)
      real        pha_weight(max_data/2)
      character*8 pha_isc_phase(max_data/2)
      character   char_stat(5,max_data/2)
      character   char_phase(8,max_data/2)
      character   char_isc_phase(8,max_data/2)

      character*1 prev_model          ! to check if new STATION file needed

      integer weight                  ! for writing to s-file
      integer i,j,k

c Program starts cccccccccccccccccccccccccccccc

c Read in prime hypocenter from s-file stored in data()
      call rea_hyp_clear(1)
      call rea_hyp1_in(data(1),1)

c If this is the first visit to this routine then load in station data.
c Also read file if current s-file has a different model in it.
      if (nstat .eq. 0 .or. hyp_model(1) .ne. prev_model) then
         call read_stat(iustat,iulst,hyp_model(1),statid,sta_lat,
     &                  sta_lon,sta_elev,nstat,agency,test,use_eev)
      end if
      prev_model = hyp_model(1)

c Read in phases from s-file stored as data()
      rea_nphase = 0
      do i=nhead+1,nrecord
         if (data(i) .ne. ' ') then
            rea_nphase=rea_nphase+1
            call rea_phase_clear(rea_nphase)          ! clear variables
            call rea_phase_in(rea_nphase,data(i))     ! read

            ! Put station info into arrays parallel to phase data
            j=0
            do while (j .lt. nstat)                   
               j = j+1
               if (rea_stat(rea_nphase) .eq. statid(j)) then
                  pha_lat(rea_nphase)  = sta_lat(j)
                  pha_lon(rea_nphase)  = sta_lon(j)
                  pha_elev(rea_nphase) = sta_elev(j)
                  j=nstat
               end if
            end do

           ! Put strings into 2D arrays of char for easy passing to C
           do j=1,5
              char_stat(j,rea_nphase) = rea_stat(rea_nphase)(j:j)
           end do
           do j=1,8
              char_phase(j,rea_nphase) = rea_phase(rea_nphase)(j:j)
           end do

         else
            if(i.ne.nrecord) then                     ! last can be blank
               write(6,*)' Blank line in phase data'
               write(6,'(a)')data(1)(1:79)
               stop
            end if
         end if
      end do

c Close print.out so that iscloc can append to it
      call sei close(close$,iulst,code)

c Call seisan version of iscloc - rea_nphase=0 if failure
      call iscloc( hyp_year, hyp_month, hyp_day, hyp_hour,
     &             hyp_min, hyp_sec, hyp_fix_org, 
     &             hyp_lat, hyp_lon, hyp_epi_flag,
     &             hyp_depth, hyp_depth_flag,
     &             hyp_rms, hyp_gap, hyp_sec_err,
     &             hyp_lat_err, hyp_lon_err, hyp_depth_err,
     &             char_stat, char_phase,
     &             char_isc_phase, rea_hour, rea_min, rea_sec,
     &             rea_res, rea_dist, rea_az, pha_weight,
     &             pha_lat, pha_lon, pha_elev, rea_nphase )

      locate = .true.
      if (rea_nphase .eq. 0) locate = .false.

c Re-open print.out
      chr_f_access$ = 'APPEND'
      call sei open( old$,                ! Open print file.
     &               ' ',                 ! No prompt.
     &               'print.out', iulst,  ! File & unit.
     &               b_flag,              ! File exists?.
     &               code )               ! Condition (n/a).


c If solution was found then change data array
      if (locate) then

         ! new top line
         call rea_hyp1_out(data(1),1)

         ! check for existing error line
         k=2                                          
         do while (data(k)(80:80) .ne. 'E' .and. k .le. nhead)
            k=k+1
         end do         

         ! if no error record move data array below header down one
         if (data(k)(80:80) .ne. 'E') then
            do i=2,nrecord
               data(nrecord+3-i)=data(nrecord+2-i)
            end do
            nrecord=nrecord+1
            nhead=nhead+1
            k=2                    
         endif              

         ! new error line
         call rea_hype_out(data(k),1)

         ! insert new residuals etc into phase lines in data()
         k=nhead
         do i=1,rea_nphase
            k=k+1

            ! untangle 2D character array holding new phase names 
            j=1
            do while (char_isc_phase(j,i) .ne. '\0')
               pha_isc_phase(i)(j:j) = char_isc_phase(j,i)
               j=j+1
            end do
            pha_isc_phase(i)(j:) = ' '

            ! create string showing weight - multiply real weight by 10
            weight = 10.*pha_weight(i)+0.49
            if (weight .eq. 10) then
               rea_weight_out(i)(1:1) = '1'
               rea_weight_out(i)(2:2) = '0'
            else if (weight .eq. 0) then
               rea_weight_out(i) = ' '
            else
               rea_weight_out(i)(1:1) = ' '
               rea_weight_out(i)(2:2) = char(ichar('0') + weight)
            endif

            call rea_phase_out(i,data(k))
         end do

         ! write lines to print.out in format read back in by hyp
         call write_hyp(iulst,pha_isc_phase,pha_weight)

      end if   ! location succeeded

      return                           
      end        

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     read_stat()
c
c     Subroutine to read hypo71 style station file.
c     Extracts and returns station codes and locations.
c     Extracts station magnitude corrections and puts them in a common block.
c     Reads RESET TEST variables and returns test array.
c     Reads agency from last line of file and returns it.
c
c     Calls:
c           stations() from hyposub1.for
c           functions from libsei.inc
c
c     If use_eev is false stations() will write a stalist to print.out

      subroutine read_stat(iustat,iulst,model,statid,lat,lon,elev,
     &                     nstat,agency,test,use_eev)

      implicit none

      include 'hypparm.inc'                ! nstats and test

c Arguments
      integer iustat                      ! lu to use for station file
      integer iulst                       ! print.out logical unit
      character*1 model                   ! charcter for choice of STATION file
      character*5 statid(*)               ! station codes
      real lat(*),lon(*)
      integer elev(*)
      integer nstat                       ! actual number of stations
      character*3 agency                  ! agency read from station file
      logical use_eev                     ! toggle list output to print.out

c Library file open routine needs these
      include 'libsei.inc'                 ! Library definitons.
      external sei get file,               ! Find file & open handler.
     &         sei close,                  ! & closure.
     &         sei clen,                   ! String length.
     &         sei code                    ! Error condition handler.
      integer  sei clen                    ! & function.
      integer  code                        ! Condition.
      logical  b_flag                      ! A problem flagged?.

c Variables to use in call to stations(), nstats set in hypparm.inc
      integer la(nstats),lo(nstats)
      real alat(nstats),alon(nstats)
      real fmg(nstats),scrst(nstats)
      real dly(nstats)
      integer init
      integer nline
      integer maxline

      character*1 ins(nstats),iew(nstats)

c Common block for magnitude corrections
c     common/hyp/stacode,mag_cor
      common/hyp/stacode,mag_cor,nline,maxline

      character*5 stacode(nstats)         ! same as statid() but in common
      real mag_cor(5,nstats)              ! station magnitude corrections

      character*80 cur_file
      character*80 testdat
      integer i

c Open the stations file. All this stuff is in libsei
      cur_file = 'STATION0.HYP'                ! Set up filename
      if (model.ne.' ') cur_file(8:8)=model    ! and adjust if needs be.
      call sei get file( open$+ignore$,        ! Find file & open it.
     &                   iustat,               ! On unit.
     &                   code,                 ! Returned condition.
     &                   'DAT',                ! Alternative directory to search
     &                   cur_file )            ! For stations file.
      if( code .ne. e_ok$ ) then
         chr_err_msg$ = cur_file(:seiclen(cur_file)) //
     &                  ' does not exist'
         call sei code( stop$,              ! Halt the program with user-.
     &                  e_misf$,            ! Message; dummy use of code.
     &                  0,                  ! Unit (n/a).
     &                  b_flag )            ! Flag (n/a).
      end if

c Report which file is being read
      if (.not. use_eev) then
         inquire(iustat,name=cur_file)
         write (iulst,*) 'Reading station data from ',
     &                  cur_file(:seiclen(cur_file))
         write (iulst,*)
      end if

c Initialise test() with default values
      call settest(test)

c Read RESET lines at start of station file
      testdat='xxxxx'        
      do while (testdat.ne. '     ')
         read(iustat,'(a80)')testdat
         if (testdat(13:13).eq.')')then
            read(testdat,'(t12,i1,t15,f9.4)') i,test(i)
         else if (testdat(14:14).eq.')')then
            read(testdat,'(t12,i2,t16,f9.4)') i,test(i)
         else if(testdat(15:15).eq.')')then
            read(testdat,'(t12,i3,t17,f9.4)') i,test(i)
         end if
         if (.not. use_eev) then
            write(iulst,'(1x,''Reset test('',i3,'')='',f10.4)')i,test(i)
         end if
      end do
      if (.not. use_eev) write (iulst,*)

c Fill arrays with station info
c     statid:       station identifier (5 letters max.)
C     la:           degree portion of station's geographic latitude
c     alat:         minute  "                      "
c     ins:          'S' for south, 'N' for north
C     lo:           degree portion of station's geographic longitude
c     alon:         minute  "                      "
c     iew:          'E' for east, 'W' for west
c     elev:         station elevation in meters
c     dly:          station delay in seconds - not used at present
c     fmg:          not used here
c     scrst:        not used here
c     nstat:        returns number of stations
c     init:         not used here
c     nstats:       maximum number of stations from hypparm.inc
c     mag_cor:      not used here
c     use_eev       flag to control output to print.out

      call stations(statid,la,alat,ins,lo,alon,iew,elev,dly,fmg,
     & scrst,nstat,iustat,iulst,init,nstats,mag_cor,use_eev)

      do i=1,nstat
        stacode(i) = statid(i)
        lat(i) = la(i) + alat(i)/60
        if (ins(i) .eq. 'S') lat(i) = -1*lat(i)

        lon(i) = lo(i) + alon(i)/60
        if (iew(i) .eq. 'W') lon(i) = -1*lon(i)
      end do

c Skip local model at end of station file
      testdat='xxxxx'        
      do while (testdat.ne. '     ')
         read(iustat,'(a80)')testdat
      end do
      read(iustat,'(a80)')testdat

c Read in agency
      read(iustat,'(a3)') agency 

c Close the file (stop on error).
      call sei close(close$,iustat,code)

      return
      end   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     write_hyp()
c
c     Subroutine to write hypocentre and phase data to print.out
c     Information already there from iscloc but need this exact format
c     to keep hyp happy as it reads print.out and writes it to screen.

      subroutine write_hyp(iulst,pha_isc_phase,pha_weight)
      implicit none

      include 'seidim.inc'                 ! needed for rea.inc
      include 'rea.inc'                    ! contains common block with data

c Arguments
      integer     iulst                    ! print.out logical unit
      character*8 pha_isc_phase(*)         ! ISC phase code
      real        pha_weight(*)            ! weights direct from iscloc

c Tempory variables for hypocentre write
      integer yy                           ! 2 digit year
      integer lat1,lon1                    ! degree part
      real lat2,lon2                       ! minute part
      character*1 ns,ew                    ! N/S E/W part
      character*1 m                        ! not sure exactly what m is
      integer cd                           ! convergence flag - always 3 here
      real damp                            ! not used
      character*1 fo                       ! time fixed flag * if fixed
      character*1 fe                       ! epicentre fixed flag * if fixed
      character*1 fd                       ! depth fixed flag * if fixed

c Temporary variables for phase write
      real*8 hyp_abs_time                  ! hypocentre time relative to epoch
      real t_obs                           ! observed travel time 
      real t_calc                          ! calculated travel time

      integer k

c
c Hypocentre header and line
c

c Want 2 digit year
      yy = hyp_year(1) - 100*(hyp_year(1)/100)

c Want lat and lon in bits
      lat1 = abs(hyp_lat(1))
      lat2 = (abs(hyp_lat(1))-lat1)*60
      ns = 'N'
      if (hyp_lat(1) .lt. 0) ns = 'S'
      lon1 = abs(hyp_lon(1))
      lon2 = (abs(hyp_lon(1))-lon1)*60
      ew = 'E'
      if (hyp_lat(1) .lt. 0) ew = 'W'

c m is 3 or 2 if depth fix but not sure what it should be when lat/lon fix
      m=' '

c fo is time fixed flag but * not F
      fo=' '
      if (hyp_fix_org(1) .eq. 'F') fo='*'

c fe is epicentre fixed flag but * not F
      fe=' '
      if (hyp_epi_flag(1) .eq. 'F') fe='*'

c fd is depth fixed flag but * not F
      fd=' '
      if (hyp_depth_flag(1) .eq. 'F') fd='*'

c Some damping parameter thats not aplicable - set to 0
      damp=0

c Only here if converged set cd to 3
      cd=3

      write(iulst,'(/,''   date hrmn   sec      lat      long'',
     &    '' depth   no m    rms  damp erln erlt erdp ic'')')

      ! all errors
      if (hyp_depth_err(1) .gt. 0) then
         write(iulst,1) yy,hyp_month(1),hyp_day(1),hyp_hour(1),
     &               hyp_min(1),hyp_sec(1),fo,
     &               lat1,lat2,ns,lon1,lon2,ew,fe,hyp_depth(1),
     &               fd,rea_nphase,m,hyp_rms(1),damp,
     &               hyp_lon_err(1),hyp_lat_err(1),
     &               hyp_depth_err(1),cd

      ! only errors in lat/lon if depth fixed
      else if (hyp_lat_err(1) .gt. 0) then
         write(iulst,2) yy,hyp_month(1),hyp_day(1),hyp_hour(1),
     &               hyp_min(1),hyp_sec(1),fo,
     &               lat1,lat2,ns,lon1,lon2,ew,fe,hyp_depth(1),
     &               fd,rea_nphase,m,hyp_rms(1),damp,
     &               hyp_lon_err(1),hyp_lat_err(1),cd

      ! no errors at all if epicentre fixed - leave rms/cd off too
      else
         write(iulst,3) yy,hyp_month(1),hyp_day(1),hyp_hour(1),
     &               hyp_min(1),hyp_sec(1),fo,
     &               lat1,lat2,ns,lon1,lon2,ew,fe,hyp_depth(1),
     &               fd,rea_nphase,m,damp
      end if

1     format(1x,3i2,1x,2i2,f6.2,a1,i2,f5.2,a1,i4,f5.1,a1,a1,f5.1,
     &       a1,i4,a1,1x,f7.2,f6.3,3f5.1,i3)
2     format(1x,3i2,1x,2i2,f6.2,a1,i2,f5.2,a1,i4,f5.1,a1,a1,f5.1,
     &       a1,i4,a1,1x,f7.2,f6.3,2f5.1,5x,i3)
3     format(1x,3i2,1x,2i2,f6.2,a1,i2,f5.2,a1,i4,f5.1,a1,a1,f5.1,
     &       a1,i4,a1,1x,7x,f6.3,18x)

c
c Phase header and lines
c
      write(iulst,'(/,'' stn   dist   azm  ain w phas'',
     &   ''    calcphs hrmn tsec  t-obs  t-cal    res   wt di'')')

      ! observed travel times are relative to hypocentre time
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     &            hyp_min(1),hyp_sec(1),hyp_abs_time)

c Loop over phases writing line for each
      do k=1,rea_nphase

         ! Put first motion in char 7 of phase code
         ! Only do this if phase code less than 6 chars long
         if (rea_phase(k)(6:6) .eq. ' ') then
            rea_phase(k)(7:7) = rea_polarity(k)
         end if

         ! calculate t_obs and t_calc on the fly
         call timsec(hyp_year(1),hyp_month(1),hyp_day(1),rea_hour(k),
     &               rea_min(k),rea_sec(k),rea_abs_time(k))
         t_obs = rea_abs_time(k) - hyp_abs_time
         if (t_obs .lt. -3600.) t_obs = t_obs + 86400  ! day change
         t_calc = t_obs - rea_res(k)

         ! only print t_calc, residual and weight when phase has a residual
         if (rea_res(k) .ne. -999) then
            write(iulst,11) rea_stat(k),int(rea_dist(k)+.49999),
     &                  rea_az(k),rea_phase(k),pha_isc_phase(k),
     &                  rea_hour(k),rea_min(k),rea_sec(k),t_obs,
     &                  t_calc,rea_res(k),pha_weight(k)
         else
            write(iulst,12) rea_stat(k),int(rea_dist(k)+.49999),
     &                  rea_az(k),rea_phase(k),pha_isc_phase(k),
     &                  rea_hour(k),rea_min(k),rea_sec(k),t_obs
         end if

      end do
      write(iulst,*)

11    format(1x,a5,i5,f6.1,8x,a8,a8,2i2,f5.1,2f7.1,f7.2,f5.2)
12    format(1x,a5,i5,f6.1,8x,a8,a8,2i2,f5.1,f7.1)

      return
      end
