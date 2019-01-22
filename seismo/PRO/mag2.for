	program mag2
	implicit none			! force delcaration of all variables
c
c This program is to establish an ML scale using
c
c                                           Nev             Nstat
c log Ajl = -n log(Rjl/R0) - k (Rjl - R0) + sum (Mj deljl) + sum (Sk delkl) - M0
c                                           j=1              k=1
c
c where:
c
c Ajl	-	observed amplitude at station l for event j
c Rjl	-	hypocentral distance from event j to station l
c R0	-	reference distance
c Mj	-	local magnitude of event j
c Nev	-	number of events
c Sk	-	correction for station l 
c Nstat	-	number of stations
c deljl	-	Kronecker delta
c delkl - 	  "	"
c M0	-	reference magnitude at R0
c n	- 	geometrical spreading parameter
c k	-	anelastic attenuation parameter
c
c Constraint = station corrections sum to zero

c
c changes
c   07/01/2010 lo obtained copy form slsa, some changes on variable types
c   16/04/2010 lo ignore component for site term determination
c   01/07/2010 lo get distance range ratio from parameter file
c                 does not compile with gfortran, check later, lo
c   05/07/2010 lo determine seperate geometrical spreading part for sg/lg
c   05/07/2010 lo changed reading of mag2.par to start in col 41
c   24/08/2010 lo changed order of a in ml scale, added option to fix scale, and to add noise
c   22/09/2010 lo changed to 3 part geometrical spreading and removed bootstrap
c   25/02/2011 Jh changed to use all phases used for Ml, warning if distace > 2500 km
c   10/04/2013 lo added fixing of a in given range (RANGE A in mag2.par)
c         2013 jh+lo add name of parameter file
c

	include 'seidim.inc'		! dimensions for rea block
	include 'rea.inc'		! parameter common bliock

	character*80 data(5000)		! s-file with data in text array
	character*80 infile		! input file

	logical all			! true: read all data, false: headers
	integer code			! error return code
	integer nevent			! number of events in file
	integer i			! counter
	integer k			! counter
	integer no	     	! counter

	integer flag(10000)		! flag

        integer maxevent,maxobs,maxsite
	parameter (maxevent=1500)
        parameter (maxobs=10*maxevent)
        parameter (maxsite=300)
	
	integer year(maxevent)		! array containing data for all events
	integer month(maxevent)
	integer day(maxevent)
	integer hour(maxevent)
	integer min(maxevent)
	real sec(maxevent)
	real lat(maxevent)
	real lon(maxevent)
	real mag(maxevent)
	real depth(maxevent)

	integer nobs			! total number of amplitude observations

	real amp(maxobs)			! arrays containing individual data points
        real vg(maxobs)
        real tempamp(maxsite),tempdis(maxsite),templdis(maxsite)
        real a,b,corr,rms
	real dist(maxobs)
	character*5 stat(maxobs)
	character*2 co(maxobs)
	real per(maxobs)
	integer event_id(maxobs)
	character*8 phase(maxobs)

	real orient			! 0. = use horiz and vert
					! 1. = use horiz only
					! 2. = use vert only

	integer j			! counter

	character*1 disgard(3)	! array containing comps to ignore if orient=1.

c	real log_dist(maxobs)		! distance logs

	real inv_type			! 1. = SVD
					! 2. = LSQR

	real min_dist			! set in mag2.par
	real max_dist			! set in mag2.par
        real min_distrange              ! minimum distance range
        real mind,maxd  
        real x

	character*2 comp_array(100)	! components to ditch
	character*5 stat_array(100)	! stations to ditch

        integer n_stat_array,n_comp_array
	
	character*5 stat_list(maxsite)	! max no of stations = 300
        integer stat_nobs(maxsite)      ! number of observations for each site
c	character*2 co_list(maxsite)	!

	integer nstat 			! no of unique stations
					! not to be confused with nstat in sub

	character*16 evid_list(maxevent)
	integer nev			! number of events for inversion

	integer flag_stat		! flag used for compiling station list
	integer flag_ev			! flag used for compiling event list

	integer ev_index(maxobs)
	integer stat_index(maxobs)

        integer err                     ! error code

c	real magscale(5)		! a, b and c in logA0
c	real magscale_err(5)		! magscale error
c change lo
	real magscale(7)		! a, b and c in logA0
	real magscale_err(7)		! magscale error
	real siteterm(maxsite)		! site terms
	real siteterm_err(maxsite)		! site term errors
	real sourceterm(maxevent)		! source terms
	real sourceterm_err(maxevent)	! source term errors

	real ref(3)			! 1=dist, 2=amp, 3=mag

	integer boot_ninv		! number of inversions for bootstrapping
	real ran2			! random number function ran2
	integer idum			! -ve integer for initialising ran2
	real seq(maxobs)			! array containing random numbers
	integer int_seq       		! array containing rounded randoms converted to int

	real amp_sim(maxobs)		! arrays containing individual data points
	real dist_sim(maxobs)		! in simulated datasets
	character*5 stat_sim(maxobs)
	character*2 co_sim(maxobs)
	real per_sim(maxobs)
	character*16 evid_sim(maxobs)
	character*16 evid_sim_list(maxevent)
	character*5 stat_sim_list(maxsite)
	character*2 co_sim_list(maxsite)

	character*8 phase_sim(maxobs)

        integer maxinv
        parameter (maxinv=1)
	character*16 evid       	! event id in yyyymmddhhmmssss format
	character*16 evid_arr(maxobs)
	
	real siteterm_arr(maxsite,maxinv)	! arrays containing the result of the inversions
	real sourceterm_arr(maxevent,maxinv)	! max no. of inversions = 1 + 4999 bootstrapped
	real magscale_arr(4,maxinv)

	integer nstat_sim
	integer nev_sim

	integer stat_sim_index(maxobs)
	integer ev_sim_index(maxobs)

	integer count
	real sum
	real average

	real xi, xbar
	real stdev
        logical synth
        real noise
        real magsynth(9)
        real hdist    ! hypocentral distance
        integer seiclen

	integer nstat_ev
	integer minobs		!minimum number of obs per event to use
        real mindratio
        real gdist(2)
        real fixscale(4),fixsite,rangea(3)

	real slat,slon,stel
        double precision msec
        real var,sigma
        real wagain
        character*240 mag_par_file
        wagain=2080.

c Open output file that contains all data points

	open(unit=2,file='mag2_amp_obs.out',status='unknown')
	open(unit=3,file='mag2_events_read.out',status='unknown')
 	open(unit=4,file='mag2_amp_dis.out',status='unknown')
	open(unit=8,file='mag2_stat_list.out',status='unknown')
	open(unit=20,file='mag2_events_used.out',status='unknown')

c slsa - write out files containing stations, events and paths
        open(unit=28,file='mag2_statxy.out',status='unknown')
        open(unit=22,file='mag2_evxy.out',status='unknown')
        open(unit=23,file='mag2_paths.out',status='unknown')

c
c   get input file name, check if exist
c

 9    continue

c slsa10
c	write(6,*) 'Susanne version'

	write(6,*) 'Give input file'
	read(5,'(a)') infile
        if (seiclen(infile).le.0) stop

	open(1,file=infile,status='old',err=10)

	goto 11

 10    continue

	write(6,*)' No such input file'

	goto 9

 11   continue

c
c  get parameter file
c
      write(6,*) 'Give parmeter file name, mag2.par is default = enter'
      read(5,'(a)') mag_par_file
      if(mag_par_file.eq.' ') mag_par_file='mag2.par'


c
	all=.true.                  ! read all parameters
	nevent=0                    ! initialize counter

c Read parameter file

	call get_mag2_par(inv_type,orient,min_dist,max_dist,
     &      comp_array,n_comp_array,stat_array,n_stat_array,
     &      ref,boot_ninv,synth,mindratio,minobs,gdist,fixscale,noise,
     &      fixsite,rangea,mag_par_file)
        if (synth) write(*,*) ' synthetic mode ' 
c g1 scale
        magsynth(1)=1.1
        magsynth(2)=.00189
        magsynth(3)=ref(3)-alog10(ref(2)/2080.*1.E6)
     &              -magsynth(1)*alog10(ref(1))-magsynth(2)*ref(1)
c g2 scale
        magsynth(4)=1.1
        magsynth(5)=.00189
        magsynth(6)=ref(3)-alog10(ref(2)/2080.*1.E6)
     &              -magsynth(4)*alog10(ref(1))-magsynth(5)*ref(1)
c g3 scale
        magsynth(7)=1.1
        magsynth(8)=.00189
        magsynth(9)=ref(3)-alog10(ref(2)/2080.*1.E6)
     &              -magsynth(7)*alog10(ref(1))-magsynth(8)*ref(1)

        idum=-1 
        

c        write(*,*) magsynth
c        stop

	
c	write(6,*) 'Orientations to use'	
c	write(6,*) '0. for horiz and vert'
c	write(6,*) '1. for horiz only'
c	write(6,*) '2. for vert only'
c	write(6,*)
c	write(6,*) 'Give choice'
c	read(*,*) orient

c Define arrays containing options for orientations
c If orient=0 then don't do anything
c If orient=1. then sz,bz,hz,az,ez are unacceptable

	write(*,*) 

	if (orient.eq.0.) then
		write(*,*) 'Horiz and vert data'
	endif
  
        disgard(1)=' '
        disgard(2)=' '
        disgard(3)=' '

	if (orient.eq.1.) then
		write(*,*) 'Horizontal data only'	
		disgard(1)='Z'
c		disgard_1(2)='HZ'
c		disgard_1(3)='AZ'
c		disgard_1(4)='BZ'
c		disgard_1(5)='EZ'
	endif

	if (orient.eq.2) then
		write(*,*) 'Vertical data only'
		disgard(1)='E'
		disgard(2)='N'
c		disgard_2(3)='BE'
c		disgard_2(4)='BN'
c		disgard_2(5)='HE'
c		disgard_2(6)='HN'
c		disgard_2(7)='EE'
c		disgard_2(8)='EN'
c		disgard_2(9)='AE'
c		disgard_2(10)='AN'
	endif

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

	nobs=0
        nstat=0
        nev=0

  50  continue
c
c   read all parameters for one event from file unit 1
c
	call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
	if(code.eq.1) goto 1000
c
c slsa10
c	nstat_ev=0

	nevent=nevent+1               ! count events

	year(nevent)=hyp_year(1)
	month(nevent)=hyp_month(1)
	day(nevent)=hyp_day(1)
	hour(nevent)=hyp_hour(1)
	min(nevent)=hyp_min(1)
	sec(nevent)=hyp_sec(1)
	lat(nevent)=hyp_lat(1)
	lon(nevent)=hyp_lon(1)
	mag(nevent)=hyp_mag(1,1)
	depth(nevent)=hyp_depth(1)

        call timsec(year(nevent),month(nevent),day(nevent),
     &    hour(nevent),min(nevent),sec(nevent),msec)

c
c   write the whole first header line
c
c	write(6,'(a)') data(1)(1:79)

	write(3,101) year(nevent),month(nevent),day(nevent),hour(nevent),
     &  min(nevent),sec(nevent),lat(nevent),lon(nevent),mag(nevent),
     &  depth(nevent),nevent

        no=0 
        mind=9999.
        maxd=0.
        do i=1,rea_nphase     	!loop through phase lines
          flag(i)=0
          if( rea_phase(i)(1:1).eq.' '
     *    .or.rea_phase(i)(1:1).eq.'L'
     *    .or.rea_phase(i)(1:2).eq.'S '
     *    .or.rea_phase(i)(1:2).eq.'Sg'
     *    .or.rea_phase(i)(1:2).eq.'SG'
     *    .or.rea_phase(i)(1:4).eq.'AMPL'
     *    .or.rea_phase(i)(1:3).eq.'AML'
     *    .or.rea_phase(i)(1:4).eq.'AMP '
     *    .or.rea_phase(i)(1:4).eq.'IAML') then 

 
              flag(i)=1

              if (rea_amp(i).le.0.) then
                flag(i)=0
              endif
			
              if (rea_per(i).ge.5.0) then	! don't include data with
                flag(i)=0			! period ge 5.0 sec
                write(*,*) ' ignoring period ge 5 '
              endif

              if (rea_dist(i).le.min_dist) then
                flag(i)=0
              endif
c
c compute group velocity
c
              vg(i)=rea_dist(i)/(rea_abs_time(i)-msec) 
              if (vg(i).le.1.) then
                flag(i)=0
                write(*,*) ' ignoring vg le 1 '
              endif

              if (rea_dist(i).gt.max_dist) then
                flag(i)=0
              endif

              if (rea_dist(i).gt.2500.0) then
                write(6,*) rea_stat(i),' ********************** warning'
                write(6,*) data(1)(1:79)
                write(6,*)' This stat/event has a distance of: ', 
     *          rea_dist(i) 
              endif

              if (rea_dist(i).le.0.) then
                flag(i)=0
              endif

              if (orient.eq.1..or.orient.eq.2.) then
                if (rea_co(i)(2:2).ne.' ') then
                  do j=1,3
                    if (rea_co(i)(2:2).eq.disgard(j)) then
                      flag(i)=0
                      write(*,*) ' disgard comp ',rea_co(i)
                    endif
                  enddo
                endif
              endif

              do j=1,n_stat_array
                if (rea_stat(i).eq.stat_array(j)) then
                  write(*,*) rea_stat(i),stat_array(j)
                  flag(i)=0
                endif
              enddo

              do j=1,n_comp_array
                if (rea_co(i).eq.comp_array(j)) then
                  flag(i)=0
                endif
              enddo		
c
c remove amplitudes that are too large for distance
c
              do j=1,rea_nphase       	!loop through phase lines
                if (rea_phase(i).eq.'AML'.and.rea_phase(j).eq.'AML')then
                  if (rea_amp(i).gt.0..and.rea_amp(j).gt.0) then 
                    if (rea_dist(i).gt.rea_dist(j)) then
                      if (rea_amp(i).gt.rea_amp(j)*10.) then
                        flag(i)=0
c              write(*,*) ' amplitude too large for distance '
      write(6,*) ' amplitude too large for distance ', data(1)(1:10),' '
     &     ,rea_stat(i), rea_co(i),rea_dist(i),' ',
     &     rea_amp(i),' ',rea_stat(j),rea_co(j),' ',rea_dist(j),
     &     ' ',rea_amp(j)

                      endif
                    endif
                  endif
                endif
              enddo

              do j=1,i-1
                if (rea_stat(i).eq.rea_stat(j).and.
     &              rea_co(i)(2:2).eq.rea_co(j)(2:2).and.
     &              flag(j).eq.1) then
                  flag(i)=0
                  write(*,*) ' duplicate component '
                endif
              enddo

              if (flag(i).eq.1) then
c count number of aml used           
                no=no+1
c get min and max distance
                if (rea_dist(i).gt.maxd) maxd=rea_dist(i)
                if (rea_dist(i).lt.mind) mind=rea_dist(i)
                tempamp(no)=alog10(rea_amp(i))
                tempdis(no)=sqrt(rea_dist(i)**2+hyp_depth(1)**2)
                templdis(no)=alog10(tempdis(no))

c slsa10 - count number of stations used
c		nstat_ev=nstat_ev+1

              endif
          endif
        enddo
        
	if (no.lt.minobs) then
	  write(6,*) '  ignoring event, too few amplitudes ', no,
     & ' ', data(1)(1:10)
          goto 50
        endif
c        min_distrange=(max_dist-min_dist)/10.
c	min_distrange=(max_dist-min_dist)/5.
        min_distrange=(max_dist-min_dist)*mindratio
        if (maxd-mind.lt.min_distrange) then
          write(*,*) ' ignoring event, distance range too short ',
     &      (maxd-mind)
          goto 50
        endif

c slsa10
c	write(6,*) 'nstat_ev =',nstat_ev
c
c check that lsq fit shows decay of log(amplitudes)
c
        call lsqlin(no,tempdis,tempamp,a,b,corr,rms)
        if (b.gt.0.) then
          goto 50
          write(*,*) ' amplitudes not decaying with distance ',
     &      a,b,corr
        endif
        call lsqlin(no,templdis,tempamp,a,b,corr,rms)
        if (b.gt.0.) then
          goto 50
          write(*,*) ' amplitudes not decaying with distance ',
     &      a,b,corr
        endif

        call rea_event_out(20,.true.,data,code)

c slsa - 19/7/10 write out lat and lons of events used in inversion

        write(22,*) lat(nevent), lon(nevent), depth(nevent)

        do i=1,rea_nphase       	!loop through phase lines
          if (flag(i).eq.1) then
				
            nobs=nobs+1 ! increment nobs for each point
            dist(nobs)=sqrt(rea_dist(i)**2+depth(nevent)**2) ! hypocentral distance
c           if (dist(nobs).eq.ref(1)) 
c     &                            dist(nobs)=dist(nobs)+.1 
c				log_dist(nobs)=log10(rea_dist(i))
c            log_dist(nobs)=alog10(dist(nobs))
            stat(nobs)=rea_stat(i)
            co(nobs)=rea_co(i)
            if (synth) then
              if (dist(nobs).le.gdist(1)) then
c g1
                amp(nobs)=mag(nevent)
     &            -magsynth(1)*alog10(dist(nobs))
     &            -magsynth(2)*dist(nobs)-magsynth(3)
              elseif (dist(nobs).le.gdist(2)) then
c g2
                amp(nobs)=mag(nevent)
     &            -magsynth(4)*alog10(dist(nobs))
     &            -magsynth(5)*dist(nobs)-magsynth(6)
              else
c g3
                amp(nobs)=mag(nevent)
     &            -magsynth(7)*alog10(dist(nobs))
     &            -magsynth(8)*dist(nobs)-magsynth(9)

              endif
              amp(nobs)=10.**amp(nobs)
c
c add noise
c
              if (noise.gt.0.) then
c              write(77,*) amp(nobs)
                write(77,*) amp(nobs)
                amp(nobs)=amp(nobs)+amp(nobs)*(ran2(idum)-.5)*2.*noise
c              write(77,*) amp(nobs)
                write(77,*) amp(nobs)
              write(77,*) 
              endif

            else
              amp(nobs)=rea_amp(i)
            endif

c event_id is assigned for each observation and corresponds to nevent. It is not
c connected to a date and time

            event_id(nobs)=nevent
            call make_ev_id(year(nevent),month(nevent),
     &  day(nevent),hour(nevent),min(nevent),sec(nevent),evid)
            evid_arr(nobs)=evid
	
c Compile station list (this is a list of all the stat/comps for which corrections
c will be determined in the inversion)
c Also compile list of event ids (these are the events for which source terms will
c be determined in the inversion)
 
c            if (nobs.eq.1) then
c              nstat=1
c              nev=1

c stat_list and co_list are lists of all the stations and components in the dataset

c              stat_list(nstat)=stat(nobs)
c              co_list(nstat)=co(nobs)

c event_list is a list of integers which represent events
c I'm not sure if I need to keep event_list(nev)=event_id(nobs)
c I think I can just use evid(nev) instead.

c Make an event ID from the year,month,day,hour,minute and second. This is the 
c index that will hopefully glue everything together.... I'm writing the ID directly
c into an array called evid, which has 5000 elements.
c 

c              evid_list(nev)=evid_arr(nobs)
c            endif
			
c            if (nobs.gt.1) then
              flag_stat=1

              do j=1,nstat
                if (stat(nobs).eq.stat_list(j)) then
c lo 16/4/2010                  if (co(nobs).eq.co_list(j)) then
                    flag_stat=0
c                  endif
                endif
              enddo

              if (flag_stat.eq.1) then
                nstat=nstat+1
                stat_list(nstat)=stat(nobs)
c lo 16/4/2010   co_list(nstat)=co(nobs)
              endif

              flag_ev=1
              do j=1,nev
                if (evid_arr(nobs).eq.evid_list(j)) then
                  flag_ev=0
c if (event_id(nobs).eq.event_list(j)) then
c flag_ev=0
                endif
              enddo

              if (flag_ev.eq.1) then
                nev=nev+1
                evid_list(nev)=evid_arr(nobs)
              endif
              per(nobs)=rea_per(i)
              phase(nobs)=rea_phase(i)
              vg(nobs)=vg(i)

              if (per(nobs).lt.5.)   ! lo 10 oct 2012
     &        write(2,100) amp(nobs),dist(nobs),stat(nobs),co(nobs),
     &     per(nobs),phase(nobs),event_id(nobs),vg(nobs)
              write(4,*) amp(nobs),dist(nobs)

c slsa - 19/7/10
                        write(23,*) lat(nevent),lon(nevent)
                        call stat_loc(stat(nobs),'0',slat,slon,stel)
                        write(23,*) slat,slon
                        write(23,*) '>'

          endif

        enddo


c
c   get next event
c
        write(4,'(a)') '>'
	goto 50

c
c     end of file
c

 1000 continue

c

c Note the indices in event_list, stat_list and co_list for each observation
c This is the master index to which all subsequent simulated datasets 
c should be linked

	do i=1,nobs
		do j=1,nev
			if (evid_arr(i).eq.evid_list(j)) then
				ev_index(i)=j
			endif
		enddo
	enddo

	do i=1,nobs
		do j=1,nstat
			if (stat(i).eq.stat_list(j)) then
c lo 16/4/2010				if (co(i).eq.co_list(j)) then
					stat_index(i)=j
c count number of observations per station
                                        stat_nobs(j)=stat_nobs(j)+1
c				endif
			endif
		enddo
	enddo

				
        do i=1,nstat
c                write(8,*) stat_list(i),co_list(i)
                write(8,*) stat_list(i),stat_nobs(i)
                call stat_loc(stat_list(i),'0',slat,slon,stel)
                write(28,*) slat,slon
        enddo

	write(6,*)            ! blank line
	close(2)              ! close output file
	close(3)
	close(4)
	close(8)
	close(11)
	close(20)

c slsa
        close(28)
        close(22)
        close(23)

	write(6,*) 'Number of events in input file', nevent
	write(6,*) 'Number of events used         ', nev
	write(6,*) 'Number of observations is     ', nobs
	write(6,*) 'Number of stations is         ',nstat
    
        if (rangea(1).ne.0..and.rangea(2).ne.0.and.
     &      rangea(3).ne.0.) then
          open(15,file='mag2_range_a.out',status='unknown')
          fixscale(1)=rangea(1)
          do while(fixscale(1).le.rangea(2))
            write(6,*) ' SVD inversion a= ',fixscale(1)
            call svd_ml(nobs,amp,dist,stat_index,ev_index,ref,
     &  magscale,magscale_err,siteterm,siteterm_err,sourceterm,
     &  sourceterm_err,gdist,fixscale,fixsite,err)
c
c evaluate result
c
        magscale(6)=ref(3)-alog10(ref(2)/wagain*1E6)
     &       -magscale(1)*alog10(ref(1))-magscale(4)*ref(1)
        call mag2var(nobs,amp,dist,nev,ev_index,
     &     nstat,stat_index,siteterm,magscale,
     &     var,sigma)

c
c output
c
            write(15,*) ' values ',fixscale(1),var,sigma
	    do k=1,7
              write(15,'(a,f4.2,a,i1,a,f8.5)') 
     &        ' a=',fixscale(1),' magscale(',k,')=',magscale(k)
            enddo
            fixscale(1)=fixscale(1)+rangea(3)
          enddo
          close(15)
        else
          if (inv_type.eq.1.) then
           write(6,*) 'SVD inversion'
          call svd_ml(nobs,amp,dist,stat_index,ev_index,ref,
     &  magscale,magscale_err,siteterm,siteterm_err,sourceterm,
     &  sourceterm_err,gdist,fixscale,fixsite,err)
	endif

c	if (inv_type.eq.2) then
c		write(*,*) 'LSQR inversion'
c		call lsqr_inv(dist,amp,ev_index,stat_index,nobs,nev,
c     &  nstat,ref,magscale,siteterm,sourceterm)
c	endif

	        do k=1,7
            write(*,'(a,i1,a,f8.5)') ' magscale(',k,')=',magscale(k)
        	enddo
        endif 
c
c evaluate result
c
        magscale(6)=ref(3)-alog10(ref(2)/wagain*1E6)
     &       -magscale(1)*alog10(ref(1))-magscale(4)*ref(1)
        call mag2var(nobs,amp,dist,nev,ev_index,
     &     nstat,stat_index,siteterm,magscale,
     &     var,sigma)
        write(*,*) ' variance and standard deviation: ',
     &     var,sigma

c Write out results to mag2.out

9999	continue
        call write_mag2(inv_type,nev,nstat,nobs,magscale,
     &  magscale_err,siteterm,siteterm_err,sourceterm,sourceterm_err,
     &  ref,stat_list,evid_list,gdist)
c     &  ref,stat_list,co_list,evid_list)


100   format(f8.1,x,f8.1,x,a5,x,a2,f6.2,x,a8,x,i4,x,f6.2)
101   format(i4,x,i2,x,i2,x,i2,x,i2,x,f5.2,x,f6.2,x,f7.2,x,
     &  f3.1,x,f4.1,x,i5)
102   format(a2)
103   format(a1,a1)
104   format(a9,f8.6,a5,f8.6)
105   format(a13,i3,x,f6.3,a5,f6.4)
106   format(a10,i5,x,a5,x,f4.2,a5,f5.3)

	stop
	end

c-------------------------------------------------------------
c Subroutines

	subroutine get_mag2_par(inv_type,orient,min_dist,max_dist,
     &    comp_array,ncomp,stat_array,nstat,ref,boot_ninv,synth,
     &    mindratio,minobs,gdist,fixscale,noise,fixsite,rangea,
     &    mag_par_file)

	implicit none

	include 'libsei.inc'                ! Seisan definitions
	include 'seidim.inc'                ! ------------------
	include 'seisan.inc'                ! ------------------

	integer read1                       ! input unit
	integer code
	character*80 line
	real var

	real inv_type
	real min_dist
	real max_dist
	real orient
	real ref(*)
        real noise

	integer ncomp
	integer nstat
        integer seiclen

	character*2 comp_array(*)
	character*5 stat_array(*)
	character*2 var_char

        real fixscale(*),fixsite,rangea(*)

	integer boot_ninv
        logical synth

	integer minobs
        real mindratio
        real gdist(*)
        character*(*) mag_par_file

c Up to 10 components can be set to be ignored (2 letter code)
c Up to 20 stations can be set to be ignored (up to 4 letter code)

	call sei get file( open$+ignore$,   ! Open waveform file.
     &                   read1,             ! On unit.
     &                   code,              ! Returned condition.
     &                   'DAT',             ! Alternative search director
     &                   mag_par_file)     ! For this filename.

	if(code.ne.e_ok$) then
		write(*,*) ' definition file does not exist: mag2.par'
		stop
	endif
	
	ncomp=0
        synth=.false.
        mindratio=.5
        gdist(1)=0.
        gdist(2)=0.
        fixscale(1)=0.
        fixscale(2)=0.
        fixscale(3)=0.
        fixscale(4)=0.
        noise=0.

10    continue

	read(read1,'(a80)',end=999) line
c        write(*,*) line

	if (line(1:14).eq.'INVERSION TYPE') then
		read(line(41:50),*) var
		if (var.ne.0.) then
			inv_type=var
		endif

	elseif (line(1:9).eq.'BOOTSTRAP') then
		read(line(41:50),*) var
		if (var.ne.0) then
			boot_ninv=var
		endif

	elseif (line(1:9).eq.'SYNTHETIC') then
		read(line(41:50),*) var
		if (var.ne.0) then
			synth=.true.
		endif

	elseif (line(1:9).eq.'DISTANCES') then
		read(line(41:50),*) var
		if (var.ne.0.) then
			min_dist=var
		endif
		read(line(51:60),*) var
		if (var.ne.0.) then
			if (var.gt.min_dist) then
				max_dist=var	
			endif
		endif

	elseif (line(1:11).eq.'ORIENTATION') then
		read(line(41:50),*) var
		orient=var

	elseif (line(1:5).eq.'NOISE') then
		read(line(41:50),*) noise
	elseif (line(1:18).eq.'REFERENCE DISTANCE') then
		read(line(41:50),*) var
		ref(1)=var
	elseif (line(1:14).eq.'SCALE DISTANCE') then
		read(line(41:50),*) var
		gdist(1)=var
		read(line(51:60),*) var
		gdist(2)=var
	elseif (line(1:11).eq.'FIX SCALE A') then
		read(line(41:50),*) fixscale(1)
		read(line(51:60),*) fixscale(2)
		read(line(61:70),*) fixscale(3)
	elseif (line(1:7).eq.'RANGE A') then
		read(line(41:50),*) rangea(1)
		read(line(51:60),*) rangea(2)
		read(line(61:70),*) rangea(3)
	elseif (line(1:11).eq.'FIX SCALE B') then
		read(line(41:50),*) fixscale(4)
	elseif (line(1:8).eq.'FIX SITE') then
		read(line(41:50),*) fixsite
	elseif (line(1:19).eq.'REFERENCE AMPLITUDE') then
		read(line(41:50),*) var
		ref(2)=var

	elseif (line(1:19).eq.'REFERENCE MAGNITUDE') then
		read(line(41:50),*) var
		ref(3)=var

	elseif (line(1:11).eq.'IGNORE COMP') then
		if (seiclen(line(41:50)).ne.0) then
			ncomp=ncomp+1
			comp_array(ncomp)=line(41:50)
		endif
	
	elseif (line(1:11).eq.'IGNORE STAT') then
		if (seiclen(line(41:50)).ne.0) then
			nstat=nstat+1
			stat_array(nstat)=line(41:50)
		endif
c slsa10
	elseif (line(1:27).eq.'MINIMUM NUMBER OF OBS/EVENT') then
		read(line(41:50),*) var
		minobs=var

	elseif (line(1:23).eq.'MIN DISTANCERANGE RATIO') then
		read(line(41:50),*) var
		mindratio=var

	endif

	goto 10

999   continue
      call sei close(close$,read1,code)
      if (minobs.lt.2) minobs=2 ! lo 16 May 2012

      return
      end


c--------------------------------------------------------------------
	subroutine write_mag2(inv_type,nev,nstat,nobs,magscale,
     &  magscale_err,siteterm,siteterm_err,sourceterm,sourceterm_err,
     &  ref,stat_list,evid_list,gdist)
c     &  ref,stat_list,co_list,evid_list)

	implicit none

c Subroutine to write output file mag2.out

c Declare variables
	integer i
	real inv_type
	integer nev
	integer nstat
	integer nobs
	
	real magscale(*), magscale_err(*)
	real siteterm(*), siteterm_err(*)
	real sourceterm(*), sourceterm_err(*)
        real c(3)

	character*16 evid_list(*)
	character*5 stat_list(*)
c	character*2 co_list(*)
	real ref(*)
        real slat,slon,latm,lonm
        real elev
        integer latd,lond
        character*1 latc,lonc
        integer seiclen
        real gdist(*)
        real siteav
        real wagain
        wagain=2080.
	
	open(13,file='mag2.out',status='unknown')
	open(14,file='mag2_station_hyp.out',status='unknown')


c	write(*,*) inv_type 
	write(13,*) 'ML inversion output'
	write(13,*)

	if (inv_type.eq.1) then
		write(13,*) 'SVD inversion'
	endif

	write(13,*)
	write(13,*) 'Total number of events: ',nev
	write(13,*) 'Total number of stations: ',nstat
	write(13,*) 'Total number of observations: ',nobs
	write(13,*)

	write(13,*) 'Reference distance  = ', ref(1)
	write(13,*) 'Reference amplitude = ', ref(2)
	write(13,*) 'Reference magnitude = ', ref(3)

	write(13,*)

c        write(13,'(a)') ' Ml = log A + a log(dist/refdist) '//
c     &      '+ b (dist-refdist) + c + S '
c	write(13,104) 'a1= ', magscale(1), ' +/- ',magscale_err(1)
c	write(13,104) 'a2= ', magscale(2), ' +/- ',magscale_err(2)
c	write(13,104) 'a3= ', magscale(3), ' +/- ',magscale_err(3)
c	write(13,104) 'b = ', magscale(4), ' +/- ',magscale_err(4)
c	write(13,104) 'c1= ', magscale(5), ' +/- ',magscale_err(5)
c	write(13,104) 'c2= ', magscale(6), ' +/- ',magscale_err(6)
c	write(13,104) 'c3= ', magscale(7), ' +/- ',magscale_err(7)

c        c(1)=magscale(5)-magscale(1)*alog10(ref(1))-magscale(4)*ref(1)
c        c(2)=magscale(6)-magscale(2)*alog10(ref(1))-magscale(4)*ref(1)
c        c(3)=magscale(7)-magscale(3)*alog10(ref(1))-magscale(4)*ref(1)
        c(1)=ref(3)-alog10(ref(2)/wagain*1E6)
     &       -magscale(1)*alog10(ref(1))-magscale(4)*ref(1)
        c(2)=ref(3)-alog10(ref(2)/wagain*1E6)
     &       -magscale(2)*alog10(ref(1))-magscale(4)*ref(1)
        c(3)=ref(3)-alog10(ref(2)/wagain*1E6)
     &       -magscale(7)-magscale(3)*alog10(ref(1))-magscale(4)*ref(1)

	write(13,*)
        write(13,'(a)') ' Ml = log A + a log(dist) '//
     &      '+ b (dist) + c + S '
	write(13,104) 'a1 = ', magscale(1), ' +/- ',magscale_err(1)
	write(13,104) 'a2 = ', magscale(2), ' +/- ',magscale_err(2)
	write(13,104) 'a3 = ', magscale(3), ' +/- ',magscale_err(3)
	write(13,104) 'b = ', magscale(4), ' +/- ',magscale_err(4)
	write(13,104) 'c1 = ',c(1)
	write(13,104) 'c2 = ',c(2) 
	write(13,104) 'c3 = ',c(3) 
	write(13,*)
	write(6,*)
        write(6,'(a)') ' Ml = log A + a log(dist) '//
     &      '+ b (dist) + c + S '
	write(6,104) 'a1 = ', magscale(1), ' +/- ',magscale_err(1)
	write(6,104) 'a2 = ', magscale(2), ' +/- ',magscale_err(2)
	write(6,104) 'a3 = ', magscale(3), ' +/- ',magscale_err(3)
	write(6,104) 'b = ', magscale(4), ' +/- ',magscale_err(4)
	write(6,104) 'c1 = ',c(1)
	write(6,104) 'c2 = ',c(2) 
	write(6,104) 'c3 = ',c(3) 
c
c write mag scale to station file
c
        write(14,'(a,f5.3)') 'RESET TEST(76)=',magscale(1)
        write(14,'(a,f7.5)') 'RESET TEST(77)=',magscale(4)
        write(14,'(a,f5.2)') 'RESET TEST(78)=',c(1)
 
        write(14,'(a,f5.3)') 'RESET TEST(176)=',magscale(2)
        write(14,'(a,f7.5)') 'RESET TEST(177)=',magscale(4)
        write(14,'(a,f5.2)') 'RESET TEST(178)=',c(2)
        write(14,'(a,f6.1)') 'RESET TEST(179)=',gdist(1)

        write(14,'(a,f5.3)') 'RESET TEST(180)=',magscale(3)
        write(14,'(a,f7.5)') 'RESET TEST(181)=',magscale(4)
        write(14,'(a,f5.2)') 'RESET TEST(182)=',c(3)
        write(14,'(a,f6.1)') 'RESET TEST(183)=',gdist(2)
        write(14,'(a)') ' '

        siteav=0.
	do i=1,nstat
c
c write out residuals
c
          call stat_loc(stat_list(i),' ',slat,slon,elev)
	  write(13,105) 'Station # ',i, stat_list(i),
c     &  co_list(i),siteterm(i),'  +/- ', siteterm_err(i),
     &  siteterm(i),'  +/- ', siteterm_err(i),
     &    slat,slon
          siteav=siteav+siteterm(i)/nstat
c          write(*,*) ' siteav ',siteav
c
c write stations with location and residual to seisan station file format
c
          latc='N'
          lonc='E'
          if (slat.lt.0.) latc='S'
          if (slon.lt.0.) lonc='W'
          slat=abs(slat)
          slon=abs(slon)
          latd=int(slat)
          lond=int(slon)
          latm=(slat-float(latd))*60.
          lonm=(slon-float(lond))*60.

          if (seiclen(stat_list(i)).lt.5) then
            write(14,'(2x,a4,i2,f5.2,a1,i3,f5.2,a1,i4,6x,f5.2)')
     &       stat_list(i),latd,latm,latc,lond,lonm,lonc,int(elev),
     &       siteterm(i)
          else
            write(14,'(1x,a5,i2,f5.2,a1,i3,f5.2,a1,i4,6x,f5.2)')
     &       stat_list(i),latd,latm,latc,lond,lonm,lonc,int(elev),
     &       siteterm(i)
          endif
	enddo
 
        write(13,'(a,1x,f5.2)') ' Average site term: ',siteav 

	do i=1,nev
		write(13,106) 'Event # ', i,evid_list(i), ' ML = ', 
c put in proper magnitude, xxx
     &  sourceterm(i),' +/- ', sourceterm_err(i)
	enddo

	close(13)
	close(14)

c Format statements

104   format(a9,f8.5,a5,f8.5)
c105   format(a10,x,i3,x,a5,x,a2,x,f6.3,a5,x,f6.4,x,f8.3,x,f8.3)
105   format(a10,x,i3,x,a5,x,f6.3,a5,x,f6.4,x,f8.3,x,f8.3)
106   format(a10,i5,x,a16,x,a5,x,f5.2,a5,f5.3)

	return 
	end


        subroutine mag2var(nobs,amp,dist,nev,ev_index,
     &   nstat,stat_index,siteterm,magscale,
     &   var,sigma)
        implicit none
        integer nobs,nev,ev_index(*),stat_index(*)
        real amp(*),dist(*),magscale(*),siteterm(*)
        integer i,j,k,evobs,nstat,n
        real evav,ml(100),var,sigma,stc,evvar
      
        var=0.
        n=0
        do i=1,nev
          evobs=0
          evav=0.
          evvar=0.
          do j=1,nobs
            if (ev_index(j).eq.i) then
c find station correction
              stc=siteterm(stat_index(j))
c magnitude
              evobs=evobs+1
              n=n+1
              ml(evobs)=a
     &           log10(amp(j))+magscale(1)*alog10(dist(j))+
     &           magscale(4)*dist(j)+magscale(6)+stc
              evav=evav+ml(evobs)
              write(55,*) i,j,stc,ml(evobs),stat_index(j)
            endif
          enddo
          evav=evav/float(evobs)
          do j=1,evobs
            var=var+(ml(j)-evav)**2
            evvar=evvar+(ml(j)-evav)**2
          enddo
          write(55,*) i,evav,evobs,var,sqrt(evvar/float(evobs))
        enddo
        var=var/n
        sigma=sqrt(var)

        return
        end


c--------------------------------------------------------------------
	subroutine make_ev_id(year,month,day,hour,min,sec,evid)
	implicit none

c Routine to construct a 16 character ID for each event by
c concatenating the year, month, day, hour, minute and second

	character*4 string1
	character*2 string2
	character*2 string3
	character*2 string4
	character*2 string5
	character*4 string6
	character*16 evid

	integer year
	integer month
	integer day
	integer hour
	integer min
	real sec
	integer sec_mul

	write(unit=string1,fmt='(i4.4)') year
	write(unit=string2,fmt='(i2.2)') month
	write(unit=string3,fmt='(i2.2)') day
	write(unit=string4,fmt='(i2.2)') hour
	write(unit=string5,fmt='(i2.2)') min
	sec_mul=sec*100
	write(unit=string6,fmt='(i4.4)') sec_mul


	evid=string1//string2//string3//string4//string5//string6
c
	return
	end		
c------------------------------------------------------------------
	subroutine standev(data,n,stdev,boot_ninv,average)
	implicit none

c Declare variables

	real sum
	integer count
	integer n
	integer i
	integer j
	integer jstop
	real xi
	real xbar
	real stdev(*)
        integer maxinv 
        parameter (maxinv=1000)
	real data(maxinv,maxinv)
	real average(n)
	integer boot_ninv

	jstop=boot_ninv+1

	do i=1,n
		sum=0
		count=0

		do j=1,jstop
			if (data(i,j).ne.0.) then
				count=count+1
				xi=data(i,j)
				xbar=average(i)
	
				sum=sum+((xi-xbar)**2)
			endif
		enddo

		stdev(i)=((1/count)*sum)**0.5
	enddo

	return 
	end	


