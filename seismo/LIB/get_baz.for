c$debug
      subroutine get_baz(nchan,stat,data,nhead,nrecord,baz)

c
c   find or calculate station back azimuth for stations in waveform
c   header. The azimuth will be obtained in the following way:
c
c   1: If hypocenter, calculate
c   2: No hypocenter, check S-file for observed azimuths, correct for residuals
c  
c   input  : nchan: number of channels
c            stat:  staiton codes, can be repeated for 3 comp
c            data,nhead,nrecord:  s-file etc
c   output:  baz:  back azimuth
c
c   J Havskov, mar 95
c
c   updates:
c
c   sep 98  by jh :  ----------------   version 7.0 check --------------------
c                    stations changed to 5 chars
c
c   april 11, adopt to new waveform structure
c
      implicit none
      real lat,lon,slat,slon    ! station and hypocenter coordinates
      real height               ! station elevation
      integer nchan             ! number of channels
      integer i,j,ich           ! counters etc
      character*5 stat(*)       ! stations
	  character*80 data(*)      ! s-file
      character*5 old_stat      ! previous station
      real delta, azim          ! epicentral distance and azimuth
      real baz(*)               ! backazimuth each trace
      real  ba                  ! back azimuth and residual from s-file
      integer ba_res
      integer nhead,nrecord     ! header and records in s-file

c
c   put in a default values
c
      do i=1,nchan
         baz(i)=999.0
      enddo
c
c   check if hypocenter is available
c
      if(data(1)(24:30).ne.'       '.and.data(1)(31:38).ne.'        ')
     *then
c
c   read hypocenter
c
         read(data(1)(24:38),'(f7.3,f8.3)') lat,lon
c
c   calculate baz
c
         old_stat='xxxxx'
         do i=1,nchan
c
c  get station coordinates
c
            call stat_loc(stat(i),data(1)(21:21),slat,slon,height)
c
c   use values from previous station if the same
c           
            if(old_stat.eq.stat(i)) then
               baz(i)=baz(i-1)
               goto 10
            endif
c
c   reset station
c
            old_stat=stat(i)
c
c  calculate baz if station was found
c
            if(slat.eq.0.0.and.slon.eq.0.0.and.height.eq.0.0) goto 10
            call azibazi(lat,lon,slat,slon,delta,azim,baz(i))
 10         continue
         enddo
      endif
c
c   check if any missing values can be found in phase file
c
      do ich=1,nchan
         if(baz(ich).eq.999.0) then
            do i=nhead+1,nrecord
               if(stat(ich).eq.data(i)(2:6)) then  ! check if azimuth there
                  if(data(i)(47:51).ne.'     '.or.
     *               data(i)(61:63).ne.'   ') then
                     read(data(i)(47:51),'(f5.1)') ba    ! observed azimuth
                     read(data(i)(61:63),'(i3)') ba_res  ! residual
                     baz(ich)=ba+ba_res                  ! calculated baz
                     goto 20                             ! do not look more 
                  endif
               endif
 20            continue
            enddo
         endif
      enddo
c
      return
      end
