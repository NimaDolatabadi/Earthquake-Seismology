c--------------------------------------------------------------------------
c  program to compare hypocenters of two cat-files
c
c  jh august 2016

c  The intention is to be able to see the effect of locating the same
c  data set using a different model and or different magnitude scales.

c  the first file is the reference.
c  content compared is origin time, rms, hypocenter and magnitudes.
c  the average difference with sd is calculated.
c  the difference is calculated by reference (file1) minus value
c  in file 2. 
c  in order to compare the files, they must have the same number
c  of events and it is assumed that the events are the same and
c  come in the same order, however the only thing checked is
c  that the files contain the same number of events.
c  if no data ( hyp array initialized to 999), event is skipped.
c  then the list of skipped events i given at the end in a file
c  events can also be skipped if there is a station limit set, will also be
c  listed
c  the number magnitudes for each event do not have to be the same so
c  not all events events need to
c  have magnitudes. Only magnitudes available are compared and listed. 
c  they always come in order Ml, Mc, Mb, MB, Ms, MS, Mw.
c
c  Magnitudes: There is no check if the agency is the same. if a magnitude
c  appears 2 times, the last will be used. note that magnitude in position 3
c  on header line will not be overwritten unless it has the agency identical
c  to the agency in the STATION0.HYP file, this can create problems
c  when comparing so make sure the agency in that position correponds
c  to the agency used when locating.
c
c  there is no check if epicenter is fixed since normally this is what
c  will be compared
c
c  events with a difference of more than 5 km in any one direction are 
c  marked with a star.
c  
c
c  jh august 2016
c--------------------------------------------------------------------------c
c
c 
c
c  changes
c
c  
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile,infile1         ! input file
      integer nstat1(5000),nstat2(5000)
      real lat1(5000),lon1(5000),depth1(5000)
      real lat2(5000),lon2(5000),depth2(5000)
      real dlat(5000),dlon(5000),ddepth(5000)  ! differences
      real*8 origin1(5000),origin2(5000)       ! abs origin times
      real*8 origin_skip(5000)                 ! origin of skipped events
      character*16 cause_skip(5000)            ! cause of skip
      integer number_skip(5000)                ! event number of skip
      real dorigin(5000)
      real rms1(5000),rms2(5000),drms(5000)    ! rms
      character*1 fixh1(5000),fixh2(5000)      ! fix depth flags
      integer mark_number(5000)                ! event numbers deviating
      logical mark                             ! if true, event deviate
      integer nmark                            ! number of events deviating   
      real av,sd                               ! average and sd
      integer minstat                          ! min numb stat to use in comparison
      real min_lat_lon,min_h,min_org,min_rms,min_mag ! min values of diff. for marking in output file 
c
c   Magnitudes
c
      real ml1(5000),ml2(5000)
      real mb1(5000),mb2(5000)
      real mbb1(5000),mbb2(5000)
      real ms1(5000),ms2(5000)
      real mss1(5000),mss2(5000)
      real mw1(5000),mw2(5000)
      real mc1(5000),mc2(5000)
      real dml(5000),dmc(5000)
      real dmb(5000),dmbb(5000)
      real dms(5000),dmss(5000)
      real dmw(5000)
      integer nml1,nml2,nmb1,nmb2,nmbb1,nmbb2,nms1,nms2,
     *nmss1,nmss2,nmw1,nmw2,nmc1,nmc2,ndml,ndmc,ndmb,
     *ndmbb,ndms,ndmss,ndmw,ndepth

      

      logical all                         ! true: read all data, false: headers
      integer nskip                       ! number of events skipped
      integer code                        ! error return code
      character*80 text,text1,text2,text3 ! general text
      character*80 header
      integer nevent1,nevent2             ! number of events in files
      integer i,k,kx                      ! counters

      nskip=0
      ndepth=0
      nml1=0
      nml2=0
      nmb1=0
      nmb2=0
      nmbb1=0
      nmbb2=0
      nms1=0
      nms2=0
      nmss1=0
      nmss2=0
      nmw1=0
      nmw2=0
      nmc1=0
      nmc2=0
      ndml=0
      ndmc=0
      ndmb=0
      ndmbb=0
      ndms=0
      ndmss=0
      ndmw=0
      do i=1,5000
        ml1(i)=-999.0
        ml2(i)=-999.0
        mb1(i)=-999.0
        mb2(i)=-999.0
        mbb1(i)=-999.0
        mbb2(i)=-999.0        
        ms1(i)=-999.0
        ms2(i)=-999.0        
        mss1(i)=-999.0
        mss2(i)=-999.0
        mw1(i)=-999.0
        mw2(i)=-999.0
        mc1(i)=-999.0
        mc2(i)=-999.0
        dml(i)=-999.0
        dmc(i)=-999.0
        dmb(i)=-999.0
        dmbb(i)=-999.0
        dms(i)=-999.0
        dmss(i)=-999.0
        dmw(i)=-999.0 
        fixh1(i)=' '
        fixh2(i)=' ' 
      enddo
c
c
c   open output file

      open(2,file='compare_hyp.out',status='unknown')
    
c
c   get input file 1 name, check if exist
c

 9    continue
      write(6,*) 'Give input file 1, the reference '
      read(5,'(a)') infile
      infile1=infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
      nevent1=0                   ! initialize counter
      nevent2=0

c
c-----------------------------------------------------------------
c  Loop to read events from reference file starts here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 500
c
      nevent1=nevent1+1              ! count events

      if(nevent1.gt.5000) then
         write(6,*)'Too many events, current limit is 5000'
         stop
      endif
c     
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)
c
c   save data
c
      lat1(nevent1)=hyp_lat(1)
      lon1(nevent1)=hyp_lon(1)
      depth1(nevent1)=hyp_depth(1)
      fixh1(nevent1)=hyp_depth_flag(1)
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),origin1(nevent1))
      nstat1(nevent1)=hyp_nstat(1)
      rms1(nevent1)=hyp_rms(1)
      
c
c   magnitudes, only prime
c
c  ml
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'L') then          
           nml1=nml1+1
           ml1(nevent1)=hyp_mag(i,1)
        endif
      enddo
c
c  mc
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'C') then
           nmc1=nmc1+1
           mc1(nevent1)=hyp_mag(i,1)
        endif
      enddo

c
c  mb
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'b') then
           nmb1=nmb1+1
           mb1(nevent1)=hyp_mag(i,1)
        endif
      enddo

c
c  mB
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'B') then
           nmbb1=nmbb1+1
           mbb1(nevent1)=hyp_mag(i,1)
        endif
      enddo

c
c  ms
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'s') then
           nms1=nms1+1
           ms1(nevent1)=hyp_mag(i,1)
        endif
      enddo

c
c  mS
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'S') then
           nmss1=nmss1+1
           mss1(nevent1)=hyp_mag(i,1)
        endif
      enddo

c
c  mw
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'W') then
           nmw1=nmw1+1
           mw1(nevent1)=hyp_mag(i,1)
        endif
      enddo


c
c   get next event
c
      goto 50
c
c     end of file 1
c
 500  continue

      close(1)

c
c   get input file 2 name, check if exist
c

 19   continue
      write(6,*) 'Give input file 2, the file to compare'
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=110)
      goto 111
 110  continue
      write(6,*)' No such input file'
      goto 19
 111  continue
c
c
c-----------------------------------------------------------------
c  Loop to read events from file to compare to starts here
c-----------------------------------------------------------------
c

  150 continue
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent2=nevent2+1               ! count events

c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)
c
c   save data
c
      lat2(nevent2)=hyp_lat(1)
      lon2(nevent2)=hyp_lon(1)
      depth2(nevent2)=hyp_depth(1)
      fixh2(nevent2)=hyp_depth_flag(1)
      nstat2(nevent2)=hyp_nstat(1)
      rms2(nevent2)=hyp_rms(1)
      
c 
c   calculate difference in origin time
c
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),origin2(nevent2))

c
c   magnitudes, only prime
c
c  ml
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'L') then
           nml2=nml2+1
           ml2(nevent2)=hyp_mag(i,1)
        endif
      enddo
c
c  mc
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'C') then
           nmc2=nmc2+1
           mc2(nevent2)=hyp_mag(i,1)
        endif
      enddo
c
c  mb
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'b') then
           nmb2=nmb2+1
           mb2(nevent2)=hyp_mag(i,1)
        endif
      enddo

c
c  mB
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'B') then
           nmbb2=nmbb2+1
           mbb2(nevent2)=hyp_mag(i,1)
        endif
      enddo

c
c  ms
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'s') then
           nms2=nms2+1
           ms2(nevent2)=hyp_mag(i,1)
        endif
      enddo

c
c  mS
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'S') then
           nmss2=nmss2+1
           mss2(nevent2)=hyp_mag(i,1)
        endif
      enddo

c
c  mw
c
      do i=1,6
        if(hyp_mag(i,1).gt.-20.0.and.hyp_mag_type(i,1).eq.'W') then
           nmw2=nmw2+1
           mw2(nevent2)=hyp_mag(i,1)
        endif
      enddo

      goto 150
c
c   
 1000 continue
      close(1)
c
c   check if same number of events in both files
c
      if(nevent1.ne.nevent2) then
         write(6,*)' Different number of events in the two files, stop'
         write(6,*)' Numbers are ',nevent1, nevent2
         stop
      endif

      text1=' '
      write(text1,'(a)')
     *'    #      Origin time   RMS     Lat      Lon Depth    N '
c
c   text for possible magnitudes
c
      kx=62
      if(nml1.gt.0.and.nml2.gt.0) then
         text1(62:65)='Ml'
         kx=kx+5
      endif

      if(nmc1.gt.0.and.nmc2.gt.0) then
         text1(kx:kx+1)='Mc'
         kx=kx+5
      endif

      if(nmb1.gt.0.and.nmb2.gt.0) then
         text1(kx:kx+1)='Mb'
         kx=kx+5
      endif

      if(nmbb1.gt.0.and.nmbb2.gt.0) then
         text1(kx:kx+1)='MB'
         kx=kx+5
      endif

      if(nms1.gt.0.and.nms2.gt.0) then
         text1(kx:kx+1)='Ms'
         kx=kx+5
      endif

      if(nmss1.gt.0.and.nmss2.gt.0) then
         text1(kx:kx+1)='MS'
         kx=kx+5
      endif

      if(nmw1.gt.0.and.nmw2.gt.0) then
         text1(kx:kx+1)='MW'
         kx=kx+5
      endif



c
c   get differences to mark
c
      write(6,*)
      write(6,*) 
     *'Min difference to mark, lat-lon, depth, origin, rms, mag'
      write(6,*) 'Defaults (enter) are 5km, 5km, 1s, 0.5s, 0.2'
      read(5,'(a)') text
      if(text.eq.' ') then
         min_lat_lon=5.0
         min_h=5.0
         min_org=1.0
         min_rms=0.5
         min_mag=0.2
      else
         read(text,*) min_lat_lon,min_h,min_org,min_rms,min_mag
      endif
      write(6,*) 'Min number of stations to compare, def 1'
      read(5,'(a)') text
      if(text.eq.' ') then
         minstat=1
      else
         read(text,*) minstat
      endif
      write(6,*)

      write(2,'(a)') text1
      header=text1
      header(5:5)=' '
      header(56:56)=' '
      write(6,'(a)') header
c
c   both files are read, now calculate differences and print out
c
      nmark=0
      do i= 1,nevent1
         mark=.false.

c
c   skip events with data missing
c
         if(lat1(i).eq.-999.0.or.lon1(i).eq.-999.0.or.depth1(i).
     *   eq.-999.0.
     *   or.lat2(i).eq.-999.0.or.lon2(i).eq.-999.0.or.depth2(i).
     *   eq.-999.0) then
c            write(6,'(a)')'Event skipped: '
            nskip=nskip+1
            origin_skip(nskip)=origin1(i)
            cause_skip(nskip)='missing data    '
            number_skip(nskip)=i
            goto 1500
         endif
c
c   skip events due to minstat
c
         if(nstat1(i).lt.minstat)then
c            write(6,*) 'Skip event ',i 
            nskip=nskip+1
            origin_skip(nskip)=origin1(i)
            cause_skip(nskip)='too few stations'
            number_skip(nskip)=i
            goto 1500
         endif
c
         dlat(i)=lat1(i)-lat2(i)
         dlon(i)=lon1(i)-lon2(i)
         text1=' '
         text2=' '
c
c   only make depth difference if depth not fixed
c
         if(fixh1(i).ne.'F'.and.fixh2(i).ne.'F') then
            ndepth=ndepth+1
            ddepth(ndepth)=depth1(i)-depth2(i)
            write(text2(47:51),'(f5.1)') ddepth(ndepth)
c
c   if depth changes more than min_h, mark with a star
c
            if(abs(ddepth(ndepth)).gt.min_h) text2(52:52)='*'
         endif

         dorigin(i)=origin1(i)-origin2(i)
         drms(i)=(rms1(i)-rms2(i))
c
c   calculate origin of event 1, was not saved
c
         call sectim(origin1(i),hyp_year(1),k,hyp_month(1)
     *   ,hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1))

         write(text1,'(i5,1x,i4,4i2,f4.1,1x,f4.1,1x)') 
     *   i,hyp_year(1),hyp_month(1)
     *   ,hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1),rms1(i)
         do k=7,22
           if(text1(k:k).eq.' ') text1(k:k)='0'
         enddo

         write(text1(23:80),
     *  '(f6.1,1x,f7.3,1x,f8.3,1x,f5.1,1x,i4)') rms1(i),lat1(i),
     *   lon1(i),depth1(i),nstat1(i)

         write(text2(1:45),
     *  '(6x,f16.1,f6.1,1x,f7.3,1x,f8.3)') 
     *   dorigin(i),drms(i),dlat(i),dlon(i)

c
c  mark orign and rms errors larger than min_org and min_rms
c
         if(abs(dorigin(i)).gt.min_org)
     *   text2(23:23)='*'
         if(abs(drms(i)).gt.min_rms)
     *   text2(29:29)='*'
c
c  mark lattitude and longitude error larger than min_lat_lon with a star
c
         if(abs(dlat(i))*111.2.gt.min_lat_lon) text2(37:37)='*'
         if(abs(dlon(i))*111.2*cos(lat1(i)/57.3).gt.min_lat_lon) 
     *   text2(46:46)='*'        
c
c   magnitudes to write
c
c   ml
c
         kx=60
         if(nml1.gt.0.and.nml2.gt.0) then      ! there are some magnitudes
            if(ml1(i).gt.-20.0.and.ml2(i).gt.-20.0) then
               write(text1(60:63),'(f4.1)') ml1(i)
               ndml=ndml+1
               dml(ndml)=ml1(i)-ml2(i)      ! save difference to make average
               write(text2(60:63),'(f4.1)') dml(ndml)
               if(abs(dml(ndml)).gt.min_mag) text2(64:64)='*'
               endif
            kx=kx+5                   ! move position for next magnitude
         endif

c
c   mc
c
         if(nmc1.gt.0.and.nmc2.gt.0) then
            if(mc1(i).gt.-20.0.and.mc2(i).gt.-20.0) then
               ndmc=ndmc+1
               write(text1(kx:kx+3),'(f4.1)') mc1(i)
               dmc(ndmc)=mc1(i)-mc2(i)      ! save difference to make average
               write(text2(kx:kx+3),'(f4.1)') dmc(ndmc)
               if(abs(dmc(ndmc)).gt.min_mag) text2(kx+4:kx+4)='*'
            endif
            kx=kx+5                   ! move position for next magnitude
         endif

c
c   mb
c
         if(nmb1.gt.0.and.nmb2.gt.0) then
            if(mb1(i).gt.-20.0.and.mb2(i).gt.-20.0) then
               write(text1(kx:kx+3),'(f4.1)') mb1(i)
               ndmb=ndmb+1
               dmb(ndmb)=mb1(i)-mb2(i)      ! save difference to make average
               write(text2(kx:kx+3),'(f4.1)') dmb(ndmb)
               if(abs(dmb(ndmb)).gt.min_mag) text2(kx+4:kx+4)='*'
            endif
            kx=kx+5                   ! move position for next magnitude
         endif

c
c   mB
c
         if(nmbb1.gt.0.and.nmbb2.gt.0) then
            if(mbb1(i).gt.-20.0.and.mbb2(i).gt.-20.0) then
               write(text1(kx:kx+3),'(f4.1)') mbb1(i)
               ndmbb=ndmbb+1
               dmbb(ndmbb)=mbb1(i)-mbb2(i)      ! save difference to make average
               write(text2(kx:kx+3),'(f4.1)') dmbb(ndmbb)
               if(abs(dmbb(ndmbb)).gt.min_mag) text2(kx+4:kx+4)='*'
            endif
            kx=kx+5                   ! move position for next magnitude
         endif

c
c   ms
c
         if(nms1.gt.0.and.nms2.gt.0) then
            if(ms1(i).gt.-20.0.and.ms2(i).gt.-20.0) then
               write(text1(kx:kx+3),'(f4.1)') ms1(i)
               ndms=ndms+1
               dms(ndms)=ms1(i)-ms2(i)      ! save difference to make average
               write(text2(kx:kx+3),'(f4.1)') dms(ndms)
               if(abs(dms(ndms)).gt.min_mag) text2(kx+4:kx+4)='*'
            endif
            kx=kx+5                   ! move position for next magnitude
         endif

c
c   mS
c
         if(nmss1.gt.0.and.nmss2.gt.0) then
            if(mss1(i).gt.-20.0.and.mss2(i).gt.-20.0) then
               write(text1(kx:kx+3),'(f4.1)') mss1(i)
               ndmss=ndmss+1
               dmss(ndmss)=mss1(i)-mss2(i)      ! save difference to make average
               write(text2(kx:kx+3),'(f4.1)') dmss(ndmss)
               if(abs(dmss(ndmss)).gt.min_mag) text2(kx+4:kx+4)='*'
            endif
            kx=kx+5                   ! move position for next magnitude
         endif

c
c   mw
c
         if(nmw1.gt.0.and.nmw2.gt.0) then
            if(mw1(i).gt.-20.0.and.mw2(i).gt.-20.0) then
               write(text1(kx:kx+3),'(f4.1)') mw1(i)
               ndmw=ndmw+1
               dmw(ndmw)=mw1(i)-mw2(i)      ! save difference to make average
               write(text2(kx:kx+3),'(f4.1)') dmw(ndmw)
               if(abs(dmw(ndmw)).gt.min_mag) text2(kx+4:kx+4)='*'
            endif
            kx=kx+5                   ! move position for next magnitude
         endif
c
c   to file
c
         write(2,'(a)') text1
         write(2,'(a)') text2         

c
c   save event numbers that have a mark *
c
         mark_number(i)=0
         do k=1,80
            if(text2(k:k).eq.'*') mark_number(i)=1
         enddo

  
 1500    continue
      enddo
c
c   averages etc
c

c
c  correct for skipped events
c
      nevent1=nevent1-nskip
 
      text1='   Average diff.'
      text2='   Standard dev.'
      text3='   Number of values'

      call sdv(nevent1,dorigin,av,sd)
      write(text1(16:22),'(f7.1)'),av
      write(text2(16:22),'(f7.1)'),sd
      call sdv(nevent1,drms,av,sd)
      write(text1(25:28),'(f4.1)'),av
      write(text2(25:28),'(f4.1)'),sd
      call sdv(nevent1,dlat,av,sd)
      write(text1(30:36),'(f7.3)'),av
      write(text2(30:36),'(f7.3)'),sd
      call sdv(nevent1,dlon,av,sd)
      write(text1(38:45),'(f8.3)'),av
      write(text2(38:45),'(f8.3)'),sd
      write(text3(38:45),'(i8)') nevent1
      call sdv(ndepth,ddepth,av,sd)
      write(text1(47:51),'(f5.1)'),av
      write(text2(47:51),'(f5.1)'),sd
      write(text3(47:51),'(i5)') ndepth
c
c
c
c   average magnitudes and sd
c
      kx=60
      if(ndml.gt.0) then
         call sdv(ndml,dml,av,sd)
         write(text1(60:63),'(f4.1)') av
         write(text2(60:63),'(f4.1)') sd
         write(text3(60:63),'(i4)') ndml
         kx=kx+5
      endif

      if(ndmc.gt.0) then
         call sdv(ndmc,dmc,av,sd)
         write(text1(kx:kx+3),'(f4.1)') av
         write(text2(kx:kx+3),'(f4.1)') sd
         write(text3(kx:kx+3),'(i4)') ndmc
         kx=kx+5
      endif

 

      if(ndmb.gt.1) then
         call sdv(ndmb,dmb,av,sd)
         write(text1(kx:kx+3),'(f4.1)') av
         write(text2(kx:kx+3),'(f4.1)') sd
         write(text3(kx:kx+3),'(i4)') ndmb
         kx=kx+5
      endif

      if(ndmbb.gt.1) then
         call sdv(ndmbb,dmbb,av,sd)
         write(text1(kx:kx+3),'(f4.1)') av
         write(text2(kx:kx+3),'(f4.1)') sd
         write(text3(kx:kx+3),'(i4)') ndmbb
         kx=kx+5
      endif

      if(ndms.gt.1) then
         call sdv(ndms,dms,av,sd)
         write(text1(kx:kx+3),'(f4.1)') av
         write(text2(kx:kx+3),'(f4.1)') sd
         write(text3(kx:kx+3),'(i4)') ndms
         kx=kx+5
      endif

      if(ndmss.gt.1) then
         call sdv(ndmss,dmss,av,sd)
         write(text1(kx:kx+3),'(f4.1)') av
         write(text2(kx:kx+3),'(f4.1)') sd
         write(text3(kx:kx+3),'(i4)') ndmss
         kx=kx+5
      endif
      if(ndmw.gt.1) then
         call sdv(ndmw,dmw,av,sd)
         write(text1(kx:kx+3),'(f4.1)') av
         write(text2(kx:kx+3),'(f4.1)') sd
         write(text3(kx:kx+3),'(i4)') ndmw
         kx=kx+5
      endif

      write(2,*)
      write(2,'(a)') header
      write(2,'(a)') text1
      write(2,'(a)') text2
      write(2,'(a)') text3

      write(6,'(a)') text1
      write(6,'(a)') text2
      write(6,'(a)') text3

      
      close(2)              ! close output file

c
c  write out two event groups, those not deviatin and the deviating
c
      open(1,file=infile1,status='old')
      open(12,file='compare_hyp_dev.out',status='unknown')
      open(13,file='compare_hyp_ok.out',status='unknown')
      write(6,*) nevent1
      
      do i=1,nevent1+nskip
         call rea_event_in(1,all,data,code)
         if(mark_number(i).eq.1) then
            call rea_event_out(12,all,data,code)
            nmark=nmark+1
         else
            call rea_event_out(13,all,data,code) 
         endif
      enddo
      close(1)
      close(12)
      close(13)

      write(6,*)'Number of events deviating',nmark
      if(nskip.gt.0) then
         write(6,*)
     *   'Number of events skipped ',nskip
         open(9,file='compare_hyp.skip',status='unknown')
         do i=1,nskip
c
c   calculate origin of event 1, was not saved
c
            call sectim(origin_skip(i),hyp_year(1),k,hyp_month(1)
     *      ,hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1))
            text1=' '
            write(text1,'(i6,1x,a,1x,i4,4i2,f4.1)')
     *      number_skip(i),cause_skip(i), 
     *      hyp_year(1),hyp_month(1)
     *      ,hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1)
            write(9,'(a)') text1
         enddo
         close(9)
         write(6,*)'File with skipped events is compare_hyp.skip'
      endif
      write(6,*)

      write(6,*) 'Output file name is compare_hyp.out'
      write(6,*) 'Output of events deviating is compare_hyp_dev.out'
      write(6,*) 'Output of events ok is compare_hyp_ok.out'

      stop
      end
