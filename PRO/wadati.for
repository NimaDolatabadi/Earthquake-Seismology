c$debug
C                                                                               
C                       
C  Program to make wadati diagram and apparent velocity 
c  from a Nordic file with one or many events                                  
C
c  The apparent velocity is calculated from the arrtival times and
c  calculated distances as given in the s-file
c                                                                               

c The purpose of the program is to calculate Vp/Vs values for individual
c events and calculate the average for a group of events. In additon, the
c program can calculate the apparent velocity for each event based on 
c P or S-times. 

c The information can be used to obtain a first impresison of crustal 
c parameters.

c The apparent velocity is calculated based on reported
c arrival times and calculated distances. For each calculation, events 
c can be selected based on: Minimum number of stations, maximum rms of 
c the fit (S-P vs P, or arrival times), minimum correlation 
c coefficient of the fit. For thr eapparent velocity calculation, data 
c can also be selected in distance and azimuth ranges.


c The output gives:

c T0  : Wadati calculated origin time
c N   : Number of stations used for Vp/Vs
c VPS : Vp/Vs ratio
c NP  : Number of stations for P- velocity
c NS  : Number of stations for S-velocity
c AVSP: Aversge S-P times with sd
c AVDI: Averge distance with sd 

c The average Vp/VS is calculated for the whole data set. Individual Vp/Vs
c values outside the range 1.53 to 1.93 are excluded.
c
c
C  J. Havskov  98
c
c
c  updates
c  may 99 by jh -------------------  verison 7.0 check --------------------
c               5 char station codes
c  mar 5, 01, jh : distnace to real
c  apr 19 03  jh : as pointed out bt fernando carrilho, weiting was not used properly
c                  for s in wadati mode
c                                                                               
c
      implicit none
      include 'seidim.inc'        ! dimensions
      CHARACTER*80 DATA(max_data)                                                    
      CHARACTER*1 TYPE,EXP                                                      
      integer nstat,nphase,nhead,nrecord
      integer min_wad    ! minimum number of stations, wadati
      real min_corr      ! -------- correlation coefficient, wadati
      real max_rms       ! maximum rms, wadati
      integer choise     ! wadati or app vel
      real min_vps, max_vps ! acceptable vp vs range
      real min_dist,max_dist,min_az,max_az ! acceptable distance and azimuth 
      character*80 file
      integer hour,min
      integer nwad   ! number of points in wadati diagram
      real sec
      real vps    ! vp/vs ratio
      integer n_min_vel  ! minimum number of stations for ap vel.
      real av_vps(50000)  ! store vps values
      integer nvps       ! number stored
      real corr,rms  ! correlation coefficient and rms, wadati
      real sd        ! standard deviation
      real max_rms_p_vel ! maximum rms for app vel of p
      real corr_p_vel,rms_p_vel,app_p_vel   ! corr and rms for apparent velcity, p vs dist
      real corr_s_vel,rms_s_vel,app_s_vel   ! corr and rms for apparent velcity, p vs dist
      real av_dist_p,sd_dist_p,av_dist_s,sd_dist_s ! average distances
      real avsp,sdsp
      integer np,ns    ! number of p'sand s's used for apparent velocity
      integer n,id
c--compact or not
      logical compact

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c
      write(6,*) 'Input file name'
      read(5,'(a)') file
c
      open(1,file=file,status='old')
c
      open(2,file='wadati.out',status='unknown')
c
c   get selection parameters
c
        write(6,*) ' Wadati (1), apparent velocity (2) or both (3) ?'
        read(5,*) choise
        if(choise.eq.1.or.choise.eq.3) then
           write(6,*)' Wadati parameters:'
           write(6,*) ' Minimum number of stations'
           read(5,*) min_wad
           write(6,*) ' Maximum rms'
           read(5,*) max_rms
           write(6,*) ' Minimum correlation coefficient'
           read(5,*) min_corr 
        endif
c
        if(choise.eq.2.or.choise.eq.3) then
           write(6,*)' Apparent velocity parameters:'
           write(6,*)' Distance range'
           read(5,*) min_dist,max_dist
           write(6,*)' Azimuth range'
           read(5,*) min_az,max_az
           write(6,*) ' Minimum number of stations'
           read(5,*) n_min_vel
           write(6,*) ' Maximum rms'
           read(5,*) max_rms_p_vel
        endif
c
c   set some defaults
c
        min_vps=1.53
        max_vps=1.93
c
c   check that not a compact file
c
        call nortype(1,compact)
        if(compact) then
           write(6,*)' Input file compact, cannot use'
           stop
        endif
c
c   read and write to end of file
c
        nvps=0
        n=0
  10    continue
        CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
        if(nrecord.eq.0) goto 99
c
c
        if(choise.eq.1.or.choise.eq.3) then
c
c   calculate wadati diagram
c
        call wadati
     *  (data,nhead,nrecord,3,nwad,vps,hour,min,sec,rms,corr)
c
c   write result for wadati
c
        if(nwad.ge.2) then
           n=n+1
           write(2,'(1x,a,a,2i2,f6.1,a,i4,a,f5.2,a,f6.2,a,f6.3)')
     *     data(1)(1:20),' T0: ',hour,min,sec,' N: ',nwad,' VPS: ',
     *     vps,' RMS: ',rms,' CORR: ',corr
           write(6,'(1x,a,a,2i2,f6.1,a,i4,a,f5.2,a,f6.2,a,f6.3)')
     *     data(1)(1:20),' T0: ',hour,min,sec,' N: ',nwad,' VPS: ',
     *     vps,' RMS: ',rms,' CORR: ',corr
        else
           write(6,'(1x,a,a)') data(1)(1:20),' No data for Wadati'
        endif
c
c   store good data for wadati average
c
        if(vps.gt.min_vps.and.vps.le.max_vps.and.corr.ge.min_corr.and.
     *  rms.le.max_rms.and.nwad.ge.min_wad) then
          nvps=nvps+1
          av_vps(nvps)=vps
        endif
        endif
c

      if(choise.gt.1) then
c
c   do apparent velocity
c
      call app_velocity
     *(data,nhead,nrecord,4,min_dist,max_dist,min_az,max_az,avsp,sdsp,
     *np,app_p_vel,rms_p_vel,corr_p_vel,av_dist_p,sd_dist_p,
     *ns,app_s_vel,rms_s_vel,corr_s_vel,av_dist_s,sd_dist_s)

c
           
      if(np.ge.n_min_vel.and.rms_p_vel.le.max_rms_p_vel) then
           write(2,'(1x,a,f6.1,f4.1,2x,a,2f6.1,1x,a,i4,a,
     *     f5.2,a,f6.2,a,f6.3)')
     *     'AVSP:',avsp,sdsp,
     *     'AVDI:',av_dist_p,sd_dist_p,
     *     'NP: ',np,' VP : ',app_p_vel,' RMS: '
     *     ,rms_p_vel,' CORR: ',corr_p_vel

           write(6,'(1x,a,f6.1,f4.1,2x,a,2f6.1,1x,a,i4,a,
     *     f5.2,a,f6.2,a,f6.3)')
     *     'AVSP:',avsp,sdsp,
     *     'AVDI',av_dist_p,sd_dist_p,
     *     'NP: ',np,' VP : ',app_p_vel,' RMS: '
     *     ,rms_p_vel,' CORR: ',corr_p_vel
      endif
c
c   put ns high so not to use apperent S-velocity
c
      if(ns.gt.n_min_vel) then
           write(2,'(18x,a,2f6.1,1x,a,i4,a,
     *     f5.2,a,f6.2,a,f6.3)')
     *     'AVDI:',av_dist_s,sd_dist_s,
     *     'NS: ',ns,' VS : ',app_s_vel,' RMS: '
     *     ,rms_s_vel,' CORR: ',corr_s_vel

           write(6,'(18x,a,2f6.1,1x,a,i4,a,
     *     f5.2,a,f6.2,a,f6.3)')
     *     'AVDI:',av_dist_s,sd_dist_s,
     *     'NS: ',ns,' VS : ',app_s_vel,' RMS: '
     *     ,rms_s_vel,' CORR: ',corr_s_vel

      endif
      endif
        goto 10
c
 99     continue
        write(6,*)

        if(choise.eq.1.or.choise.eq.3) then
           write(6,*)
     *     ' Number of events for which vp/vs were calcualted ',n
           write(6,*)
     *     ' Number of events selected for average            ', nvps
           write(6,*)
           write(2,*)
     *     ' Number of events for which vp/vs were calculated ',n
           write(2,*)
     *     ' Number of events selected for average            ', nvps
           write(2,*)
c
c   calculate average vps
c
           if(nvps.ge.2) then
              write(6,*)
              call sdv(nvps,av_vps,vps,sd)
              write(6,'(1x,a,f6.2,a,f6.2,a,i5)')' Average VP/VS =', vps, 
     *        '  SD=',sd,' N=',nvps
              write(2,*)
              call sdv(nvps,av_vps,vps,sd)
              write(2,'(1x,a,f6.2,a,f6.2,a,i5)')' Average VP/VS =', vps, 
     *        '  SD=',sd,' N=',nvps
           endif
        endif
        stop
        end
                       
      subroutine wadati
     *(data,nhead,nrecord,min_weight,nwad,vps,hour,min,sec,rms,corr)
c
c   routine makes wadati analysis of one event
c   jh, february 1998
c
c   input
c
c   data,nhead,nrecord: data, number of headers, number of records 
c   min_weight        : smallest weight to use (0,1,2,3)
c   
c   output
c
c   nwad              : number of points used
c   vps               : vp to vs ratio
c   hour,min,sec      : origin time
c   rms               : rms of fit
c   corr              : correlation coefficient of fit
c
c
      implicit none
      character*80 data(*)
      integer nhead,nrecord,min_weight,nwad
      real vps,t_origin,rms,corr
      real tp(500)   ! p-times
      real tsp(500)  ! s-p times
      real sec
      integer timep  ! hours and minutes in secs
      integer hour,min
      character*5 stat   ! one station code
      character*2 phase  ! phase
      integer i,k,iw
      
c
c
c
      nwad=0
c
c   find pairs of readings, assume same phase, weight can be in column 9 if
c   phase is long
c
      do i=nhead+1,nrecord
        read(data(i)(15:15),'(i1)',err=50) iw     ! read assigned weight
        goto 51
 50     continue
        read(data(i)(9:9),'(i1)') iw              ! read assigned weight
 51     continue
c
c   find p and s-p for wadati
c
          
        if(data(i)(11:11).eq.'P'.and.iw.le.min_weight) then  ! if valid P
           stat=data(i)(2:6)
           phase=data(i)(11:12)  ! p-phase
           phase(1:1)='S'        ! corresponding s-phase
           do k=nhead+1,nrecord
             read(data(k)(15:15),'(i1)',err=60) iw     ! read assigned weight, i to k, apr 03
             goto 61
  60         continue
             read(data(k)(9:9),'(i1)') iw              ! read assigned weight, i to k, apr 03
  61         continue
             if(data(k)(11:12).eq.phase.and.data(k)(2:6).eq.stat.
     *       and.iw.le.min_weight) then  ! same S
                nwad=nwad+1
                read(data(i)(19:28),'(2i2,f6.1)') hour,min,sec
                tp(nwad)=hour*3600.0+min*60.0+sec
                read(data(k)(19:28),'(2i2,f6.1)') hour,min,sec
                tsp(nwad)=hour*3600.0+min*60.0+sec-tp(nwad)
             endif
           enddo
        endif
      enddo
      timep=hour*3600+min*60  
      do i=1,nwad
        tp(i)=tp(i)-timep    ! make values smaller so better fit in lsq
      enddo

c
c  all readings done, now do wadati analysis
c
      if(nwad.ge.2) then
         call lsqlin(nwad,tp,tsp,t_origin,vps,corr,rms)
         t_origin=-t_origin/vps+timep
         vps=vps+1
         hour=t_origin/3600
         min=(t_origin-hour*3600)/60
         sec=t_origin-hour*3600-min*60
      endif

      return
      end


      subroutine app_velocity
     *(data,nhead,nrecord,min_weight,
     *min_dist,max_dist,min_az,max_az,
     *avsp,sdsp,
     *np,app_p_vel,rms_p_vel,corr_p_vel,av_dist_p,sd_dist_p,
     *ns,app_s_vel,rms_s_vel,corr_s_vel,av_dist_s,sd_dist_s)
     
c
c   routine makes apparent velocity analysis of one event
c   jh, march 1998
c
c   input
c
c   data,nhead,nrecord: data, number of headers, number of records 
c   min_weight        : smallest weight to use (0,1,2,3)
c   min_dist,max_ist,min_az,max_az : distance and azimuth range to use
c   
c   output
c
c   avsp,sdsp         : averasge and sd of s-p times
c   av_dist_p         : average distance for P
c   av_dist_s         : average distance for S
c   np                : number of points used for p
c   app_p_vel         : apparent p-velocity
c   rms_p_vel         : rms of fit
c   corr_p_vel        : correlation coefficient of fit
c
c
      implicit none
      character*80 data(*)
      integer nhead,nrecord,min_weight,nsp
      real min_dist,max_dist,min_az,max_az
      integer iaz    ! azimuth for one station, event to statiojn
      real rms_p_vel, corr_p_vel,app_p_vel ! for app velocity
      real rms_s_vel, corr_s_vel,app_s_vel ! for app velocity
      real tp(500)   ! p-times
      real tpp(500)  ! p-times for s-p
      real ts(500)   ! first s
      real av_dist_p,sd_dist_p,av_dist_s,sd_dist_s
      character*5 stat_p(500),stat_s(500)  ! stations having a p or s-reading
      integer np     ! number of p times
      integer ns     ! number of s times
      real tsp(500)  ! s-p times
      real dist_p(500) ! epicentral distances, p
      real dist_s(500) ! epicentral distances, s
      real dist      ! -------------------
      real avsp,sdsp ! average s-p with standard deviation
      real sec
      real a         ! dummy
      integer timep  ! hours and minutes in secs
      integer hour,min
      character*5 stat   ! one station code
      character*2 phase  ! phase
      integer i,k,iw
      
c
c
c
      nsp=0
      np=0
      ns=0
c
c   find pairs of readings, assume same phase, weight can be in column 9 if
c   phase is long
c
      do i=nhead+1,nrecord
        read(data(i)(15:15),'(i1)',err=50) iw     ! read assigned weight
        goto 51
 50     continue
        read(data(i)(9:9),'(i1)') iw              ! read assigned weight
 51     continue
c
c   find p and distance
c 
c        write(6,*) iw,data(i)(11:11),data(i)(71:75)
        if(data(i)(11:11).eq.'P'.and.iw.lt.4.and.
     *  data(i)(71:75).ne.'     ') then            ! if valid P
c            write(6,*) np
            read(data(i)(19:28),'(2i2,f6.1)') hour,min,sec
            read(data(i)(71:79),'(f5.0,1x,i3)') dist,iaz
c
c   check if within range
c
            if(dist.gt.max_dist.or.dist.lt.min_dist.
     *      or.iaz.gt.max_az.or.iaz.lt.min_az) then
               goto 198   ! end of loop, do not use station for P
            endif
            np=np+1
            tp(np)=hour*3600.0+min*60.0+sec
            dist_p(np)=dist
            stat_p(np)=data(i)(2:6)
        endif
 198    continue
c
c  find s and distance
c

        if(data(i)(11:11).eq.'S'.and.iw.lt.4.and.
     *  data(i)(71:75).ne.'     ') then            ! if valid P
c            write(6,*) ns
            read(data(i)(19:28),'(2i2,f6.1)') hour,min,sec
            read(data(i)(71:79),'(f5.0,1x,i3)') dist,iaz
c
c   check if within range
c
            if(dist.gt.max_dist.or.dist.lt.min_dist.
     *      or.iaz.gt.max_az.or.iaz.lt.min_az) then
               goto 199   ! end of loop, do not use station for S
            endif
            ns=ns+1
            ts(ns)=hour*3600.0+min*60.0+sec
            dist_s(ns)=dist
            stat_s(ns)=data(i)(2:6)
        endif
 199    continue
c
c   find p and s-p for average s-p
c
        if(data(i)(11:11).eq.'P'.and.iw.le.min_weight) then  ! if valid P
           stat=data(i)(2:6)
           phase=data(i)(11:12)  ! p-phase
           phase(1:1)='S'        ! corresponding s-phase
           do k=nhead+1,nrecord
             read(data(i)(15:15),'(i1)',err=60) iw     ! read assigned weight
             goto 61
  60         continue
             read(data(i)(9:9),'(i1)') iw              ! read assigned weight
  61         continue
             if(data(k)(11:12).eq.phase.and.data(k)(2:6).eq.stat.
     *       and.iw.le.min_weight) then  ! same S
                nsp=nsp+1
                read(data(i)(19:28),'(2i2,f6.1)') hour,min,sec
                tpp(nsp)=hour*3600.0+min*60.0+sec
                read(data(k)(19:28),'(2i2,f6.1)') hour,min,sec
                tsp(nsp)=hour*3600.0+min*60.0+sec-tpp(nsp)
             endif
           enddo
        endif
      enddo
      timep=hour*3600+min*60  
c
c  all readings done, now find average s-p
c
      if(nsp.ge.2) then
         call sdv(nsp,tsp,avsp,sdsp)
      endif

c
c   apparent velocity from distances and p-times
c
      if(np.ge.2) then
c
c  first get rid of multiple p's for same station
c
         do i=1,np
           do k=1,np
             if((stat_p(k).eq.stat_p(i)).and.(k.ne.i)) then
                if((tp(k).gt.tp(i)).and.tp(i).gt.0.0) tp(k)=0   ! indicate not use
             endif
           enddo
         enddo
         k=0
         do i=1,np
           if(tp(i).ne.0) then
              k=k+1
              tp(k)=tp(i)
              dist_p(k)=dist_p(i)
           endif
         enddo
         np=k

         if(np.ge.2) then
            do i=2,np
              tp(i)=tp(i)-tp(1)  ! make values smaller
            enddo  
            tp(1)=0.0                        
            call lsqlin
     *      (np,tp,dist_p,a,app_p_vel,corr_p_vel,rms_p_vel)
         endif
      endif
c
c   average distance 
c
      call sdv(np,dist_p,av_dist_p,sd_dist_p)
c
c   apparent velocity from distances and s-times
c
      if(ns.ge.2) then
c
c  first get rid of multiple s's for same station
c
         do i=1,ns
           do k=1,ns
             if((stat_s(k).eq.stat_s(i)).and.(k.ne.i)) then
                if((ts(k).gt.ts(i)).and.ts(i).gt.0.0) ts(k)=0   ! indicate not use
             endif
           enddo
         enddo
         k=0
         do i=1,ns
           if(ts(i).ne.0) then
              k=k+1
              ts(k)=ts(i)
              dist_s(k)=dist_s(i)
           endif
         enddo
         ns=k

         if(ns.ge.2) then
            do i=2,ns
              ts(i)=ts(i)-ts(1)  ! make values smaller
            enddo  
            ts(1)=0.0                        
            call lsqlin
     *      (ns,ts,dist_s,a,app_s_vel,corr_s_vel,rms_s_vel)
         endif
      endif
c
c   average distance
c
      call sdv(ns,dist_s,av_dist_s,sd_dist_s)
c
      return
      end









