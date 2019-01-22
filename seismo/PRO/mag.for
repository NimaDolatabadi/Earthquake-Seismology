c  This program calculates various magnitude relations, is a bit primitive !
c  J Havskov, 1994-95
c-----------------------------------------------------------------------
c
c   updatees
c   aug 23, 95 by jh: also write out all data if converting
c   nov 6           : quick fix to use magnitude in any line for mc regression
c   jan 10  96      : bug in quick fix, now also use agency to select 
c                     magnitude for coda scale
c   feb 28          : bug when searching many header lines
c   oct 30  96      : also do moment, stress drop etc regression
c   mar 25, 97      : fix output -input of agency etc
c ---------------------------------------------------------------------------
c   sep 98 by jh    : ------------   version 7.0 check -----------------
c                     change bad station to 5 chars
c   mar 8     jh    : put maxlik for magnetude correlation
c   sep 23    lo    : put in maxlik a 2nd time
c   dec 5     lo    : add inversion for Ml scale for complete data set
c   mar 2     lo    : add site term for Ml inversion
c   mar 5     jh    : distances to real
c   mar 19          : increase mag field to be able to use negative mags
c   may27           : more spectral parameter selection
c   jun  12         . small text changes
c   june 20 2005 jh : plt to eps
c   dec 2005     jh : disable inversion for ml using all data
c                     max likelihood for coda realtion
c   oct 30 2010  jh : enable replot if pc
c   dec 28 2010  jh : gfortran on pc: remove winplot, put in implicit none, remove
c                     above change, change variabel tol from real to integer,
c                     computer specific reading from keyboard no longer relevant,
c                     only write 79 chars on screen to avoid blank lines
c   feb 22 2011  jh : size from color.def
c   apr 27 2013  jh : select event interactively from plot
c   feb 17 2014  jh : read seisan.def
c   2014-04-14   pv : cleanup Warning: Obsolete: arithmetic IF statement
c   2015-04-19   jh : write whole sfile in mag_new.out and mag_newa.out
c
c   1: Calculation of an mc scale using some selectable m 
c      for calibration events
c   
c   3d regression
c   m = a * log (coda) + b * dist + c
c
c   2d regression
c   m = a * (log (coda) + dist_coff * dist) + c
c   note b = a * dist_coff  where dist_coff is entered
c
c   input is a nordic file where m, coda and dist are given
c   dist is hypocentral distance
c
c------------------------------------------------------------------------
c   2: Calculation of an amplitude attenuation scale using amplitudes 
c      and distances in Nordic file, does not work too well. For each
c      event a,b,c are calcualted using least squares regressian as follows:
c
c      3d regression
c
c      log(amp) = a * log(dist) + b * dist + c
c
c      2d regression
c
c      log(amp)+dist_coff*dist = a * log(dist) + c

c      At the end the average constants a and b are calculated. Hypocentral
c      distance is used.
c------------------------------------------------------------------------
c   3: Calculation of relations between different magnitude scales
c      using already calculated magnitudes in header lines
c      Here one or both can be spectral parameters
c
c-------------------------------------------------------------------------
c   4: Calculation of a converted magntude from one of several input 
c      magnitudes using one of several relations
c
c   mar 22 99 bmt : include winplot.inc
c
      implicit none
      include 'seiplot.inc'   ! seisan graphics
      include 'seidim.inc'
      include 'rea.inc'
      real x(20000),y(20000),z(20000)              ! observations each event
      real xx(20000),yy(20000),zz(20000),yy1(20000) ! observations each event
      real xxx,yyy                        ! help variables
      real mag1(50000),mag2(50000)        ! magnitudes to compare
      character*15 date(50000)            ! date and time of each event
      character*1 cha(10)                 ! dummy for xy_plot
      character*80 text                   ! general text
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      character*200 outstring             ! for spectral output
      integer nspec                       ! number of events with spectra
      character*1  make_plot              ! indicator for making plot
      character*80 data(5000)

      character*80 txt(100)               ! help
      real xc,yc                          ! help
      integer nrecord,nstat,nhead,id,nphas
      integer icoda
      real dist,amp,av,sd,xm1,xm2
      real max_dist          ! maximum distance
      real min_dist          ! minimum distance
      integer iml            ! number of events for amp atteneuation, 3d
      integer iml1           ! number of events for amp atteneuation, 2d
      integer iamp           ! number of  stations for amp inversion
      character*5 bad_stat(100)  ! bad stations, do not use
      integer nbad,ibad      ! number of bad stations
      real depth             ! hypocentral depth
      integer synt           ! 0: real data, 1: syntetic data
      real noise             ! noise for synthetic amplitude data
      character*1 exp,type
      character*1 mag_type   ! correlation magnitude type for mc correlation
      character*3 mag_type_aga ! tyep of agency to use for mc correlation
      real xm                ! correlation magnitude
      character*4 mag_aga1,mag_aga2 ! magnitude and agency 
      character*4 mag_aga_out       ! ---------------- for output conversion
      character*4 mag_aga_rel(250)   ! ---------------- magnitudes to use in --
      real arel(250),brel(250)        ! magnitude relation coefficients
      integer nrel                    ! number of --------------------- sets
      integer nmag_out                ! number of converted magnitudes out
      real xm_in, xm_out              ! in and output magnitude 
      character*8 inmag               ! magntude and agency used for conversion
      character*80 filename 
      real dist_coff         ! distance coeffcient in 2d regressing, see above
      real dist_ratio        ! distance ratio
      real sigmaa,sigmab,r,sigmay(10000) ! statistical parameters
      real a,b,c             ! see above
      real aa(10000),bb(10000),cc(10000)
      real b_2d_amp(10000)    ! b-values for 2d amplitude relation
      integer istat          ! total number of station observavions
      logical ifin           ! switch for event selection
      integer nmagrel        ! number of events for magnitude comparison
      integer i,k,m          ! counters etc
      integer nline          ! nmber of lines in parameter file
      logical compact        ! true if compact file
      character*1 screenout  ! y for printout
      character*5 spec_stat1,spec_stat2 ! stations to use with spectra
      character*4 spec_comp1,spec_comp2 ! components to use with spectra
      character*1 spec_type1,spec_type2 ! P or S spectrum

c
c for inversion of Ml
c
      integer max_x,max_y             ! array dimensions for inversion
      parameter (max_y=2000)          ! number of observations
      parameter (max_x=1000)          ! size of model vector
      real data_vector(max_y)         ! amplitude vector
      real kernel_matrix(max_y,max_x) ! kernel matrix
      real model_vector(max_x)        ! model vector
      real mag_vector(max_x)          ! original magnitude
      real cvm(max_x,max_x)           ! covariance matrix
      integer maxs                    ! maximum number of stations
      parameter(maxs=200)
      character*5 stat_name(maxs)     ! list of station names
      integer stat_max                ! maximum number of stations
      integer stat_index(1000)        ! station index
      integer evcnt,datacnt           ! counter for events used in inversion
      real v(max_x,max_x),w(max_y)    ! matrices used with SVD
      real thresh,wmax                ! used when editing the w vector
      integer tol                     ! -----------------------------
      integer mp,np                   ! number of cols and rows in matrices
      integer j,n

      include 'version.inc'

c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call get_seisan_def
      synt=0
      istat=0
      nbad=0
      ibad=0
      nrel=0
      iml=0
      nspec=0
      iml1=0
      dist_coff=99.9
      nmagrel=0
      compact=.false.
      nrel=0
      evcnt=0
      datacnt=0
      stat_max=0
     
c
c init arrays
c
      do i=1,max_y
        data_vector(i)=0.
        do j=1,max_x
          if (i.eq.1) model_vector(j)=0.
          kernel_matrix(i,j)=0.
        enddo
      enddo
      do i=1,maxs
        stat_name(i)=' '
        mag_vector(i)=0.
      enddo
      do k=1,1000
        stat_index(k)=0
      enddo


c
      write(6,'(a,a)')
     *' Magnitudes and agencies to compare, return for no',
     *          ' comparison,'
      write(6,*) ' can also be spectral parameters'
      write(6,'(a,a)')
     *' The first magnitude ( spectral parameter) will',
     *' be the independent variable'
      write(6,'(a,a)')
     *' If both agencies are blank, agency information is',
     *          ' not used'
      write(6,'(a)')' Abbreviations are:'
      write(6,'(a)')' MOME                log moment'
      write(6,'(a)')' OMEG                log omega0 level'
      write(6,'(a)')' STRE                log stress drop'
      write(6,'(a)')' CORF                log corner frequnecy'
      write(6,'(a)')' RADU                log source radius'
      write(6,'(a)')' ALPA                log spectral decay'
      write(6,*)
      write(6,'(a)')' Example of input format, all UPPER CASE'
      write(6,'(a)')'  x    y'
      write(6,'(a)')' LPDE BBER           2 magnitudes'
      write(6,'(a)')' LPDE MOME           magnitude and log moment'
      write(6,'(a)')
     *' CBER CORF           magnitude and log corner frequency'

       read(5,'(1x,a4,1x,a4)') mag_aga1,mag_aga2



      spec_comp1=' '
      spec_comp2=' '
      spec_stat1=' '
      spec_stat2=' '
      spec_type1=' '
      spec_type2=' '
c
c   check if station specific paremters whciis possible for
c   spectral parameters
c
      if(mag_aga1(1:4).eq.'STRE'.or.mag_aga1(1:4).eq.'RADU'.or.
     *   mag_aga1(1:4).eq.'ALPA'.or.mag_aga1(1:4).eq.'MOME'.or.
     *   mag_aga1(1:4).eq.'OMEG'.or.mag_aga1(1:4).eq.'W   '.or.
     *   mag_aga1(1:4).eq.'CORF') then
         write(6,*)
     *' For spectral values, also possible to chose channel and P or S'
         write(6,*)'Return to use average spectral values'
         write(6,*)'Write below, P is P, S or blank for no type'
         write(6,'(a)')' STAT COMP P for mag_aga1'

         read(5,'(1x,a5,a4,1x,a1)')
     *   spec_stat1,spec_comp1,spec_type1
      endif
c
c   check if station specific paremters which possible for
c   spectral parameters
c
      if(mag_aga2(1:4).eq.'STRE'.or.mag_aga2(1:4).eq.'RADU'.or.
     *   mag_aga2(1:4).eq.'ALPA'.or.mag_aga2(1:4).eq.'MOME'.or.
     *   mag_aga2(1:4).eq.'OMEG'.or.mag_aga2(1:4).eq.'W   '.or. 
     *   mag_aga2(1:4).eq.'CORF') then
         write(6,*)
     *' For spectral values, also possible to chose channel and P or S'
         write(6,*)'Return to use average spectral values'
         write(6,*)'Write below, P is P, S or blank for no type'
         write(6,'(a)')' STAT COMP P for mag_aga2'

          read(5,'(1x,a5,a4,1x,a1)')
     *   spec_stat2,spec_comp2,spec_type2
      endif

c
c   get parameter file if there 
c
         i=1
         open(1,file='mag.par',status='old',err=56)
 49      continue
         read(1,'(a)',end=56) data(i)
         i=i+1
         goto 49
 56      continue
         nline=i-1
c
c   search for keywords for parameters if file has one or more lines
c
         if(nline.gt.0) then
c
c   bad stations
c
            do i=1,nline
               if(data(i)(1:12).eq.'BAD STATION'
     *         .and.data(i)(51:55).ne.'     ') then
                  nbad=nbad+1
                  bad_stat(nbad)=data(i)(51:55)
               endif                   
c
c   magnitude and agency of converted magnitude
c
               if(data(i)(1:6).eq.'MAGAGA'
     *         .and.data(i)(51:54).ne.'    ') then
                  mag_aga_out=data(i)(51:54)
               endif                   
c
c   conversion relations
c
               if(data(i)(1:6).eq.'MAGREL'
     *         .and.data(i)(51:54).ne.'    ') then
                  nrel=nrel+1
                  mag_aga_rel(nrel)=data(i)(51:54)
                  read(data(i)(61:80),'(2f10.3)') arel(nrel),brel(nrel)
               endif                   
c
c   print output
c
               if(data(i)(1:9).eq.'SCREENOUT'
     *         .and.data(i)(51:54).ne.'    ') then
                  screenout=data(i)(51:51)
               endif                   
c
c   magnitude relation generation by regression on coda or amplitude
c			   
               if(data(i)(1:11).eq.'MAG_TYP_COF'
     *         .and.data(i)(51:54).ne.'    ') then
                  mag_type=data(i)(51:51)
                  mag_type_aga=data(i)(52:54)
                  read(data(i)(61:70),'(f10.3)' ) dist_coff
               endif                   
            enddo
            write(6,*)' Number of bad stations', nbad
            write(6,*)' Number of magnitude relations to use',nrel
         else
            write(6,*)' No parameter file mag.par given'
            write(6,*)' No new magntudes generated, must be set up in'
     *                  ,' mag.par'
            write(6,*)' No coda or amplitude realtions made'
         endif
         if(dist_coff.ne.99.9) then
            write(6,*)' Will do a magnitude regression',
     *    ' on coda and/or amplitudes if possible'
	     else
            write(6,*)' No magntude regression done on coda',
     *      ' and/or amplitude, see mag.par for setup'
	     endif
         close(1)
c
c   open output files
c
      open(2,file='mag_coda.out',status='unknown')
      open(12,file='mag_coda_ava.out',status='unknown')
      open(3,file='mag_mag.out',status='unknown')
      open(4,file='mag_amp.out',status='unknown')
      open(7,file='mag_new.out',status='unknown')
      open(8,file='mag_newa.out',status='unknown')
      open(9,file='mag_spec.out',status='unknown')
      open(10,file='mag_ml_inv.out',status='unknown')
c
c   loop for reading files --------------------------------------------------
c
 500  continue
      write(6,*)' File name, if no more, return'
      read(5,'(a)') filename
      if(filename(1:5).eq.'     ') then
         if(compact.or.dist_coff.eq.99.9) then
             goto 250  ! magnitude comparison only
         else
             goto 2  ! magnitude relation processing
         endif
      endif
      open(1,file=filename,status='old',err=5005)
      goto 5006
 5005 continue
      write(6,*)' No such file'
      goto 500
 5006 continue
c
c  find which type of input file
c
      call nortype(1,compact)
      if(compact) write(6,*)' Input file is compact'
c
c   loop for reading events
c
  1   continue
c
c   read one event or one header line
c
      if(compact) then
         read(1,'(a)',end=501) data(1)
         nhead=1
         nrecord=1
         goto 502
 501     continue
            close(1)
            goto 500  ! next file
 502     continue
      else
         call indata(1,nstat,nphas,nhead,nrecord,type,exp,data,id)
         data(1)(80:80)='1'
c
c        write(6,*) nhead,nrecord,nphas
         write(6,'(a)') data(1)(1:79)
c         write(17,*) data(1)
c         read(5,'(a)') text
         if(nrecord.le.0) then
            close(1)
            goto 500   ! check for end of file
         endif
      endif
c
c   read into rea block
c
      rea_nrecord=nrecord
      rea_nhead=nhead
c      write(6,*) 'rea in'
c      read(5,'(a)') text
      call rea_event_in(0,.true.,data,i)

c      write(6,*) 'rea out'
c      read(5,'(a)') text
      if(screenout.eq.'Y') write(6,'(a)')data(1)
c
c   print out spectral parameters
c
      outstring=' '
      do i=2,nhead
        if(data(i)(2:13).eq.'SPEC AVERAGE') then
           nspec=nspec+1
           outstring(1:79)=data(1)(1:79)
           outstring(80:80)=' '
           outstring(105:105)=' '
           outstring(106:145)=data(i)(16:55)
        endif
        if(data(i)(46:48).eq.data(1)(46:48)) then   ! second magnitude line
           outstring(81:104)=data(i)(56:79)
        endif
      enddo
      if(outstring(1:10).ne.'          ')
     * write(9,'(a)')  outstring
c
c   look for magnitudes and spectral paramaters to compare
c   will always use last occurence of data. if agency is blank,
c   use any occurence of magnitude type
c
c   spectral parameter is used as magnitude and agency
c
      xm1=-99.0
      xm2=-99.0

c-------------------------------------
c   check if comparison of parameters
c-------------------------------------

      if(mag_aga1.ne.' ') then

c
c-----------------------------------------------------------
c   select two parameters 
c-----------------------------------------------------------
c-------------------
c   parameter 1
c-------------------
c
          if(spec_stat1.ne.' ') then
c
c   spectral parameters, component and type dependent
c
             do i=1,rea_nphase
                if(spec_stat1(1:4).eq.rea_stat(i)(1:4).
     *          and.spec_comp1.eq.
     *          rea_comp(i).and.spec_type1.eq.rea_phase(i)(5:5)) then
                   if(mag_aga1(1:4).eq.'MOME'.and.rea_moment(i).gt.0.0)
     *             xm1=rea_moment(i)
                   if(mag_aga1(1:4).eq.'STRE'.and.rea_sdrop(i).gt.
     *             0.0)  xm1=alog10(rea_sdrop(i))
                   if(mag_aga1(1:4).eq.'OMEG'.and.rea_omega0(i).gt.-10.)
     *             xm1=rea_omega0(i)
                   if(mag_aga1(1:4).eq.'CORF'.and.rea_cornerf(i).gt.0.0)
     *             xm1=alog10(rea_cornerf(i))
                   if(mag_aga1(1:4).eq.'RADU'.and.rea_radius(i).gt.0.0)
     *             xm1=alog10(rea_radius(i))
                   if(mag_aga1(1:4).eq.'ALPA'.and.rea_slope(i).gt.-99.)
     *             xm1=rea_slope(i)
                   if(mag_aga1(1:4).eq.'W   '.and.rea_mw(i).gt.-10.0)
     *             xm1=rea_mw(i)
                endif
             enddo
          else
c
c   average spectral values, not component or type dependent
c
             if(mag_aga1(1:4).eq.'MOME'.and.rea_av_moment.gt.0.0)
     *       xm1=rea_av_moment
             if(mag_aga1(1:4).eq.'STRE'.and.rea_av_sdrop.gt.
     *       0.0)  xm1=alog10(rea_av_sdrop)
             if(mag_aga1(1:4).eq.'OMEG'.and.rea_av_omega0.gt.-10.)
     *       xm1=rea_av_omega0
             if(mag_aga1(1:4).eq.'CORF'.and.rea_av_cornerf.gt.0.0)
     *       xm1=alog10(rea_av_cornerf)
             if(mag_aga1(1:4).eq.'RADU'.and.rea_av_radius.gt.0.0)
     *       xm1=alog10(rea_av_radius)
             if(mag_aga1(1:4).eq.'ALPA'.and.rea_av_slope.gt.-99.)
     *       xm1=rea_av_slope
          endif
             
c
c   magnitudes with type and agency
c
          do i=1,rea_nmag
              if(mag_aga1(1:1).eq.hyp_mag_type_all(i).and.
     *           mag_aga1(2:4).eq.hyp_mag_agency_all(i)(1:3)) then
                 xm1=hyp_mag_all(i)
              endif
          enddo

c-----------------------------------
c   look for 2. parmeter if first ok
c-----------------------------------

       if(xm1.gt.-99.0) then
          if(spec_stat2.ne.' ') then
c
c   spectral parameters, component and type dependent
c
             do i=1,rea_nphase
                if(spec_stat2(1:4).eq.rea_stat(i)(1:4).
     *          and.spec_comp2.eq.
     *          rea_comp(i).and.spec_type2.eq.rea_phase(i)(5:5)) then
                   if(mag_aga2(1:4).eq.'MOME'.and.rea_moment(i).gt.0.0)
     *             xm2=rea_moment(i)
                   if(mag_aga2(1:4).eq.'STRE'.and.rea_sdrop(i).gt.
     *             0.0)  xm2=alog10(rea_sdrop(i))
                   if(mag_aga2(1:4).eq.'OMEG'.and.rea_omega0(i).gt.-10.)
     *             xm2=rea_omega0(i)
                   if(mag_aga2(1:4).eq.'CORF'.and.rea_cornerf(i).gt.0.0)
     *             xm2=alog10(rea_cornerf(i))
                   if(mag_aga2(1:4).eq.'RADU'.and.rea_radius(i).gt.0.0)
     *             xm2=alog10(rea_radius(i))
                   if(mag_aga2(1:4).eq.'ALPA'.and.rea_slope(i).gt.-99.)
     *             xm2=rea_slope(i)
                   if(mag_aga2(1:4).eq.'W   '.and.rea_mw(i).gt.-10.0)
     *             xm2=rea_mw(i)
                endif
             enddo
          else
c
c   average spectral values, not component or type dependent
c
             if(mag_aga2(1:4).eq.'MOME'.and.rea_av_moment.gt.0.0)
     *       xm2=rea_av_moment
             if(mag_aga2(1:4).eq.'STRE'.and.rea_av_sdrop.gt.
     *       0.0)  xm2=alog10(rea_av_sdrop)
             if(mag_aga2(1:4).eq.'OMEG'.and.rea_av_omega0.gt.-10.)
     *       xm2=rea_av_omega0
             if(mag_aga2(1:4).eq.'CORF'.and.rea_av_cornerf.gt.0.0)
     *       xm2=alog10(rea_av_cornerf)
             if(mag_aga2(1:4).eq.'RADU'.and.rea_av_radius.gt.0.0)
     *       xm2=alog10(rea_av_radius)
             if(mag_aga2(1:4).eq.'ALPA'.and.rea_av_slope.gt.-99.)
     *       xm2=rea_av_slope
          endif    
c
c   magnitudes with type and agency
c
          do i=1,rea_nmag
              if(mag_aga2(1:1).eq.hyp_mag_type_all(i).and.
     *           mag_aga2(2:4).eq.hyp_mag_agency_all(i)(1:3)) then
                 xm2=hyp_mag_all(i)
              endif
          enddo
      endif

c-------------------------------------------------------------------
c   case where no agencies are chosen and no specific chnnels either
c-------------------------------------------------------------------

      if(mag_aga1(2:4).eq.'   '.and.mag_aga2(2:4).eq.'   '.
     *and.spec_stat1.eq.' ') then
          do i=1,nhead
           if(data(i)(80:80).eq.'1') then
           if(mag_aga1(1:1).eq.data(i)(60:60))
     *        read(data(i)(56:59),'(f4.1)') xm1
           if(mag_aga1(1:1).eq.data(i)(68:68))
     *        read(data(i)(64:67),'(f4.1)') xm1
           if(mag_aga1(1:1).eq.data(i)(76:76))
     *        read(data(i)(72:75),'(f4.1)') xm1 
           endif
           if(xm1.gt.-99.0) then
               do k=1,nhead
                if(data(k)(80:80).eq.'1') then
                  if(mag_aga2(1:1).eq.data(k)(60:60))
     *               read(data(k)(56:59),'(f4.1)') xm2
                  if(mag_aga2(1:1).eq.data(k)(68:68))
     *               read(data(k)(64:67),'(f4.1)') xm2
                  if(mag_aga2(1:1).eq.data(k)(76:76))
     *               read(data(k)(72:75),'(f4.1)') xm2 
                endif
               enddo
           endif
          enddo
         endif
c
c   if data found, put into array
c
        if(xm1.gt.-99.0.and.xm2.gt.-99.0) then
            nmagrel=nmagrel+1
            mag1(nmagrel)=xm1
            mag2(nmagrel)=xm2
            date(nmagrel)=data(1)(1:15)
        endif
      endif
C
      read(data(1)(39:43),'(f5.1)') depth
c
c   find if magnitude type is available for magnitude relation, also
c   check for agency. if no agency required, only the magntude type
c   is checked for, else both magnitude type and agency mmust fit
c
      xm=-100.0
     
      do i=1,nhead
        if(i.eq.1.or.data(i)(80:80).eq.'1') then
         if((mag_type_aga.eq.'   '.or.mag_type_aga.eq.data(i)(61:63)).
     *   and.data(i)(60:60).eq.mag_type) read(data(i)(56:59),'(f4.1)')xm
         if((mag_type_aga.eq.'   '.or.mag_type_aga.eq.data(i)(69:71)).
     *   and.data(i)(68:68).eq.mag_type) read(data(i)(64:67),'(f4.1)')xm
         if((mag_type_aga.eq.'   '.or.mag_type_aga.eq.data(i)(77:79)).
     *   and.data(i)(76:76).eq.mag_type) read(data(i)(72:75),'(f4.1)')xm
        endif
      enddo
c
c---------------------------------------------------------------------------
c   do magnitude conversion, look for magnitude with given type and agency,
c   if no agency then use magnitude with same type. The first magntude 
c   found is used
c---------------------------------------------------------------------------
c
      if(nrel.gt.0) then
        do k=1,nrel
          do i=1,nhead
           if(data(i)(80:80).eq.'1') then
           if((mag_aga_rel(k).eq.data(i)(60:63)).or.
     *        (mag_aga_rel(k)(2:4).eq.'   '.and.mag_aga_rel(k)(1:1).eq.
     *        data(i)(60:60))) then
              read(data(i)(56:59),'(f4.1)') xm_in
              xm_out=xm_in*arel(k)+brel(k)
              inmag=data(i)(56:63)
              goto 70
           endif
           if((mag_aga_rel(k).eq.data(i)(68:71)).or.
     *        (mag_aga_rel(k)(2:4).eq.'   '.and.mag_aga_rel(k)(1:1).eq.
     *        data(i)(68:68))) then
              read(data(i)(64:67),'(f4.1)') xm_in
              xm_out=xm_in*arel(k)+brel(k)
              inmag=data(i)(64:71)
              goto 70
           endif
           if((mag_aga_rel(k).eq.data(i)(76:79)).or.
     *        (mag_aga_rel(k)(2:4).eq.'   '.and.mag_aga_rel(k)(1:1).eq.
     *        data(i)(76:76))) then
              read(data(i)(72:75),'(f4.1)') xm_in 
              xm_out=xm_in*arel(k)+brel(k)
              inmag=data(i)(72:79)
              goto 70
           endif
          endif
          enddo
        enddo
        goto 71    ! no magnitude found
 70     continue
        nmag_out=nmag_out+1
c
c  update header, clean out old magntude values and put in just the new,
c  also put magnitude used for conversion in 2. position
c
        data(1)(56:79)='                       '
        data(1)(60:63)=mag_aga_out     ! output type and agency
        write(data(1)(56:59),'(f4.1)') xm_out
        data(1)(64:71)=inmag
C        write(7,'(a)') data(1)    ! jh apr 15
        write(7,'(a)') (data(i),i=1,nrecord)
c        write(8,'(a)') data(1)
        write(8,'(a)') (data(i),i=1,nrecord)
        goto 72
 71     continue
c        write(8,'(a)') data(1) ! write all data if not written above
        write(8,'(a)') (data(i),i=1,nrecord) ! write all data if not written above
 72     continue
      endif
c
c   read amplitudes and coda lengths
c
      iamp=0
      max_dist=0.0
      min_dist=999999.0
c
c   read rest of event if nordic file if not a compact file.
c   if a compact file or no magnitude relation is to be calculated
c   jump this section and go to next event
c
      if(compact.or.dist_coff.eq.99.9) goto 1  
c      if(xm.eq.-100.0) goto 1         ! no magnitude, next event
      m=0                              ! for synthetic data
      do i=nhead+1,nrecord-1
         ifin=.true.
c
c check that station is used 
c
         do k=1,nbad
           if(bad_stat(k).eq.data(i)(2:6)) then
              ifin=.false.
              ibad=ibad+1
              write(6,*)' Station rejected ',bad_stat(k)
           endif
         enddo
         if(ifin) then        ! station is used
            read(data(i)(30:33),'(i4)') icoda
            read(data(i)(71:75),'(f5.0)') dist
            read(data(i)(34:40),'(f8.1)') amp
            dist=sqrt(dist*dist+depth*depth)
c
c  save values
c
           if(icoda.gt.0.and.dist.gt.0.and.xm.ne.-100.0) then
               istat=istat+1
               x(istat)=alog10(float(icoda))
               z(istat)=dist
               y(istat)=xm
c               write(6,*) xm,icoda,dist
            endif
c
c   only use local or regional events for Ml amplitude attenuation
c
            if(data(1)(22:22).eq.'l') data(1)(22:22)='L'
            if(amp.gt.0.and.(data(1)(22:22).eq.'L'
     *      .or.data(1)(22:22).eq.'R').and.dist.gt.0.0) then
               iamp=iamp+1
               xx(iamp)=alog10(dist)
               yy(iamp)=alog10(amp)
               zz(iamp)=dist
c
c sort station into list and set index
c
               do k=1,stat_max
                 if (stat_name(k).eq.data(i)(2:6).or.
     &               stat_name(k).eq.data(i)(3:6)) then
                   if (stat_index(iamp).eq.0) stat_index(iamp)=k
                 endif
               enddo
               if (stat_index(iamp).eq.0) then
                 stat_max = stat_max + 1
                 stat_index(iamp) = stat_max
                 if (data(i)(2:2).eq.' ') then
                   stat_name(stat_max) = data(i)(3:6)
                 else
                   stat_name(stat_max) = data(i)(2:6)
                 endif
               endif
c
c  next section is for testing with syntetic data
c
               if(synt.eq.1) then
                  yy(iamp)=-1.0*alog10(dist)-dist_coff*dist+3.0  
                  m=m+1
                  if(m.eq.2) m=0
                  if(m.eq.0) noise=0.04    ! 10 % amplitude changee
                  if(m.eq.1) noise=-0.04
                  yy(iamp)=yy(iamp)+noise
               endif
               yy1(iamp)=yy(iamp)+dist_coff*dist
c               write(6,*) iamp,amp
              if(dist.gt.max_dist) max_dist=dist
              if(dist.lt.min_dist) min_dist=dist
            endif
         endif
      enddo
c
c   invert for amplitude attenuation, use linear distance corrected amp
c   do not use if less than 3 stations and ditance ratio less than 3
c
      if(max_dist.eq.min_dist) then
         dist_ratio=999.9
      else
         dist_ratio=max_dist/(max_dist-min_dist)
      endif
c
c fill data vector and kernel matrix
c
c  disable by putting a large value
c
      if(iamp.gt.20000.and.dist_ratio.lt.3.0.and.evcnt.lt.max_y) then
        evcnt=evcnt+1
        mag_vector(evcnt)=xm1
c       write(*,*) ' event: ',evcnt
        if (evcnt.gt.max_x-4) then
          write(*,*) ' Max number of events exceeded '
          write(6,*) ' No more data used'
          goto 7777
c         stop
        endif
        do i=1,iamp
          datacnt=datacnt+1
          if (datacnt.gt.max_y) then
            write(*,*) ' Max number of readings exceeded '
            write(6,*) ' No more data used'
            goto 7777
c           stop
          endif
          data_vector(datacnt)=yy(i)       ! amplitude
          kernel_matrix(datacnt,1)=xx(i)   ! log10(dist)
          kernel_matrix(datacnt,2)=zz(i)   ! dist
          kernel_matrix(datacnt,3)=1.      ! constant
          kernel_matrix(datacnt,3+stat_index(i))=1. ! station term
          kernel_matrix(datacnt,maxs+3+evcnt)=1. ! event size term
        enddo
 7777   continue
      endif

      if(iamp.gt.2.and.dist_ratio.lt.3.0) then
c
c   first make a 2 D inversion to remove bad values
c
         CALL LINFIT (xx,yy1, SIGMAY, iamp, 0, A, SIGMAA, B, SIGMAB ,R)
         write(4,'(a,2i6,a,i5,a,f6.2,a,f8.2)')
     *  ' Distance range',int(min_dist),int(max_dist),
     *  '  namp=',iamp, '  corr=',r,'  r=',dist_ratio
c
c   save 2d value if reasonable
c
            if(b.gt.-3.and.b.lt.1.0) then
c            if(r.lt.-0.5.and.iamp.gt.2) then
               iml1=iml1+1
               b_2d_amp(iml1)=b
               write(4,'(1x,a,3x,f10.4,20x,a)')
     *         data(1)(1:15),b,' selected for 2 d'
            else
               write(4,'(1x,a,3x,f10.4,20x,a)')
     *         data(1)(1:15),b,' not selected for 2 d'
            endif
c
c   now make 3 D inversion
c
         if(b.lt.2.0.and.b.gt.-10.0) then   ! do not use very bad values
c
            iml=iml+1
            call invxy(iamp,yy,xx,zz,aa(iml),bb(iml),cc(iml))
c
c   again remove very bad values
c
            if(aa(iml).gt.0.or.aa(iml).lt.-3.0) then
               write(4,'(1x,a,3x,3f10.4,a)')
     *         data(1)(1:15),aa(iml),bb(iml),cc(iml),
     *         ' not selected for 3d'
               iml=iml-1
            else
               write(4,'(1x,a,3x,3f10.4,a)')
     *         data(1)(1:15),aa(iml),bb(iml),cc(iml),' selected for 3d'
            endif

         endif
      endif
      goto 1    ! next event
c
c--------------------------------------------------------------
c  all events read, now calculate 
c--------------------------------------------------------------
c
  2   continue
c
c   fix axis notation
c
      if(mag_aga1(1:4).eq.'STRE'.or.mag_aga1(1:4).eq.'RADU'.or.
     *   mag_aga1(1:4).eq.'ALPA'.or.mag_aga1(1:4).eq.'MOME'.or.
     *   mag_aga1(1:4).eq.'OMEG'.or.mag_aga1(1:4).eq.'W   '.or.
     *   mag_aga1(1:4).eq.'CORF'.and.spec_stat1.eq.' ')
     *   spec_stat1='AVARA'
      if(mag_aga2(1:4).eq.'STRE'.or.mag_aga2(1:4).eq.'RADU'.or.
     *   mag_aga2(1:4).eq.'ALPA'.or.mag_aga2(1:4).eq.'MOME'.or.
     *   mag_aga2(1:4).eq.'OMEG'.or.mag_aga2(1:4).eq.'W   '.or. 
     *   mag_aga2(1:4).eq.'CORF'.and.spec_stat2.eq.' ')
     *   spec_stat2='AVARA'
c


c
c do inversion for Ml scale
c
c  disbale by putting evcnt  =0
      evcnt=0
      if (evcnt.gt.1) then
        mp=datacnt+2
        np=evcnt+3+stat_max
c
c add constraint for constant added to source term
c
        data_vector(mp-1)=2.
        kernel_matrix(mp-1,3)=1.
        data_vector(mp)=0.
c
c remove empty columns from kernel matrix
c
        do j=1,evcnt
          do i=1,mp
            kernel_matrix(i,j+3+stat_max)=
     &           kernel_matrix(i,j+3+maxs)
          enddo
        enddo
c
c add constraint for all source terms to sum up to 0
c
        do i=4,3+stat_max
          kernel_matrix(mp,i)=1
        enddo

c
c write out lin equations
c
        do i=1,mp
          write(23,*) data_vector(i),' = ',(kernel_matrix(i,j),j=1,np)
        enddo
        call svdcmpx(kernel_matrix,mp,np,max_y,max_x,w,v)
c
c edit w, remove small values
c
c   error: where is tol getting a value from ? jh dec 2010 
         wmax=0.
         do j=1,tol
           if(w(j).gt.wmax)wmax=w(j)
         enddo
         thresh=tol*wmax
cerrro         do j=1,xdim
         do j=1,tol              ! errror: xdim was not defined, jh dec 2010
           if (w(j).lt.thresh)w(j)=0.
         enddo

c
c call svbksb
c
         call svbksbx(kernel_matrix,w,v,mp,np,max_y,max_x,
     *      data_vector,model_vector)

c
c call svdvar to get variance
c
         call svdvar(v,np,max_x,w,cvm,max_x)

c          c=-3.-alog10(454.)-model_vector(1)*alog10(100.)-
c     &           model_vector(2)*100.
c     endif

      write(10,'(a,f12.8,a,f12.8)') ' a = ',model_vector(1),
     &      ' +/- ',sqrt(cvm(1,1))
      write(10,'(a,f12.8,a,f12.8)') ' b = ',model_vector(2),
     &      ' +/- ',sqrt(cvm(2,2))
      write(10,'(a,f12.8,a,f12.8)') ' c = ',model_vector(3),
     &      ' +/- ',sqrt(cvm(3,3))
      do i=1,stat_max
        write(10,'(a,i3,a,f5.2,a,f6.3)')
     &     ' Station # ',i,' '//stat_name(i)//' ',
     &     model_vector(i+3), 
     &     ' +/- ',sqrt(cvm(i+3,i+3))
      enddo
      do i=1,evcnt
        write(10,'(a,i3,a,f5.2,a,f6.3,a,f4.1)') 
     &     ' Event # ',i,' Ml= ',model_vector(i+3+stat_max),
     &     ' +/- ',sqrt(cvm(i+3+stat_max,i+3+stat_max)),
     &     ' M orig = ',mag_vector(i)
      enddo
      endif
      close(10)

      write(6,*)'Number of points for 3d amplitude relation',iml
      if(iml.gt.1) then
c         write(6,*)'Number of events for ml ',iml
         write(6,*)'Average a for ml'
         call SDV(iml,aa,AV,SD)
         write(6,*) av,sd
         call SDV(iml,bb,AV,SD)
         write(6,*)'Average b for ml'
         write(6,*) av,sd
      else
         write(6,*) 'Too few points for 3d amplitude relation'
      endif
      write(6,*)'Number of points for 2d amplitude relation',iml1
      if(iml1.gt.1) then
c         write(6,*)'Number of events for ml ',iml1
         call SDV(iml1,b_2d_amp,AV,SD)
         write(6,*)'Average a for ml'
         write(6,*) av,sd
      else
         write(6,*) 'Too few points for 2d amplitude relation'
      endif
c
      if(mag_type.eq.' ') goto 250    ! no Mc calculation
      write(6,*)
      write(6,*)' Number of data points to use for mc',istat
      write(6,*)' Number of data points rejected', ibad
      write(6,*)
      if(istat.lt.3) then
         write(6,*)' Too few points for coda relation'
         goto 250
      endif
c
      write(6,*)' Inversion for 3 parameters'
c     
      call invxy(istat,y,x,z,a,b,c)
      write(6,*)' Coda term',a
      write(6,*)' Distance term',b
      write(6,*)' Constant', c 
c
c   do 2d inverison
c
      write(6,*)
      write(6,*)' Inversion with fixed distance term',dist_coff
      do i=1,istat
        x(i)=x(i)+dist_coff*z(i)
        write(2,*) x(i),y(i)      ! write out for plotting
      enddo
      CALL LINFIT (X, Y, SIGMAY, istat, 0, A, SIGMAA, B, SIGMAB ,R)
      write(6,*)' Coda term, least squares',b,sigmab
      write(6,*)' Constant, least squares', a,sigmaa 
      write(6,*)' Correlation coefficient, least squares ', r
      call maxlik(x,y,istat,a,b,sigmaa,sigmab)
      write(6,*)
      write(6,*)' Coda term, max likelihood',b,sigmab
      write(6,*)' Constant, max likelihood', a,sigmaa
c
c   use average magnitudes
c
      do i=1,istat
            xx(i)=0
            yy(i)=0
            zz(i)=0
      enddo
      n=0
      do i=1,istat
         if(y(i).gt.0.0) then
            k=y(i)*10
            yy(k)=k/10.0
            xx(k)=xx(k)+x(i)
            zz(k)=zz(k)+1
c           write(17,*) i,k,x(i),xx(k),y(i),yy(k),zz(k)
         endif
      enddo
      do i=1,100
         if(zz(i).gt.0) then
           n=n+1
c          write(17,*) n,xx(i),yy(i),zz(i)
           xx(n)=xx(i)/zz(i)
           yy(n)=yy(i)
         endif
      enddo
      write(6,*)
      write(6,*)' Number of avaraged magnitudes',n
      write(6,*)' Now using averaged magnitudes'

      do i=1,n
        x(i)=xx(i)
        y(i)=yy(i)
        write(12,*) x(i),y(i)      ! write out for plotting
      enddo
      CALL LINFIT (X, Y, SIGMAY, n, 0, A, SIGMAA, B, SIGMAB ,R)
      write(6,*)' Coda term, least squares',b,sigmab
      write(6,*)' Constant, least squares', a,sigmaa
      write(6,*)' Correlation coefficient, least squares ', r
      call maxlik(x,y,n,a,b,sigmaa,sigmab)
      write(6,*)
      write(6,*)' Coda term, max likelihood',b,sigmab
      write(6,*)' Constant, max likelihood', a,sigmaa
c


c
c   enter here if a compact file is used
c
 250  continue

c------------------------------------------
c  magnitude/spectral parameter comparison
c------------------------------------------

      if(nmagrel.gt.1) then
         write(6,*)' Number of points for magnitude and or ',
     *   'spectral parameter comparison'
     *               ,nmagrel

c
c   call to linfit only to get correlation 
c

         CALL LINFIT (mag1, mag2, SIGMAY, nmagrel, 0, 
     *   A,SIGMAA, B, SIGMAB ,R)
         write(6,'(2x,a,a,a,f6.3,a,a,a,f7.3)')
     *   'Least squares:      ',mag_aga2,' = ',b,' * ',mag_aga1,' + ',a

c
c   maximum likelihood
c
         call maxlik(mag1,mag2,nmagrel,a,b,sigmaa,sigmab)

         write(6,'(2x,a,a,a,f6.3,a,a,a,f7.3)')
     *   'Maximum likelihood: ',mag_aga2,' = ',b,' * ',mag_aga1,' + ',a
         write(6,*) 
         write(6,*)' Slope', b,sigmab
         write(6,*)' Constant', a,sigmaa
         write(6,*)' Correlation ',r
         call SDV(nmagrel,mag1,AV,SD)
         write(6,'(1x,a,1x,a,1x,2f8.2)')' Average par1(x): ',mag_aga1,av,sd
         call SDV(nmagrel,mag2,AV,SD)
         write(6,'(1x,a,1x,a,1x,2f8.2)')' Average par2(y): ',mag_aga2,av,sd
         do i=1,nmagrel
           write(3,*) mag1(i),mag2(i)
         enddo
c
         write(6,*) ' Plot relation (y/n)'
         read(5,'(a)') make_plot
         if(make_plot.eq.'y'.or.make_plot.eq.'Y') then
 3949    continue    
c
c  set defaults for output on tek screen and one hardcopy file
c
           open(65,file='mag.eps',status='unknown')
           plotunit=65
           plotoption=1
           wsize=60
           call get_window_size
           if(size_mag.gt.0) wsize=size_mag ! from color.def
c

c
c   open plotter
c
           call open_display 
c
c set some postscipt scalings
c
           write(65,*) ' 1.0 0.55 scale'

           write(title,'(a,f7.3,a,f7.3)')
     *     'Maximum likelihood relation: y = ',b,' * x + ',a

c           write(title,'(a,f7.3,a,f7.3)')
c     *     'Least squares relation: y = ',b,' * x + ',a
           xtext='             '
     *     //mag_aga1//' '//spec_stat1//' '//spec_comp1//' '//spec_type1
           ytext='             '
     *     //mag_aga2//' '//spec_stat2//' '//spec_comp2//' '//spec_type2
c
c   plot points
c
           call xy_plot
     *     (1,nmagrel,mag1,mag2,title,xtext,ytext,
     *     600.0,600.0,100.0,100.0,1,1,20.0,
     *     0,cha,i,x,y)
c
c  plot line
c
           call xy_plot_line(b,a,100.0,100.0)
           txt(1)='Select event by                  '
           txt(2)='clicking near symbol             '
           txt(3)='q to quit                        '
           call xmessage(txt,3,25,720.0,700.0)
       
c
c  call up cursxor so plots remains
c
 
 130      continue

          call xy_plot_input(100.0,100.0,cha(1),xxx,yyy,xc,yc)
c
c   find corresponding event
c
          txt(1)=' '
          txt(1)='Too far from symbol,               '
          txt(2)='try again                          '
          txt(3)=' '
          txt(4)=' '
          do i=1,nmagrel
              if(abs(xxx-mag1(i))*xc.lt.10.0.and.   ! distance in pixels
     *           abs(yyy-mag2(i))*yc.lt.8.0) then
                 txt(1)(1:25)=date(i)//'          '
                 txt(2)=' '
                 txt(3)=' '
                 txt(4)=' '
              endif
          enddo
          call xmessage(txt,4,25,720.0,680.0)

          if(cha(1).ne.'q') goto 130   ! next input
c
495        continue
c           call xscursr(i,x(1),y(1))
c           if (char(i).eq.'#') goto 495  ! lot 01/07/2003

c
c   close postscript
c
           call close_post
c
c   close output plot file
c 
c         call sei close(close$,write02,code)
           close(65)
c
c   close display and back to alpha screen
c
          call clear_to_alpha
          write(6,*)' Plot file is mag.eps'
        endif

         write(6,*)
         write(6,*) 

     * 'File with mag and or spectral comparison values is mag_mag.out'
      else
         write(6,*) 'No values for comparison *************'
      endif
      write(6,*)
      if(dist_coff.ne.99.9.and.(.not.compact)) then
	     write(6,*)
     *           'File with coda scale values is mag_coda.out'
             write(6,*)
     *           'File with avaraged coda values is mag_coda_ava,out'
	     write(6,*)'File with individual Ml a and b is mag_amp.out'
      endif
      write(6,*)'Number of converted magnitudes',nmag_out
      if(nmag_out.gt.0) then
      write(6,*)
     *'File with only converted magnitudes is mag_new.out'
      write(6,*)
     *'File with all magnitudes is mag_newa.out'
      endif
      if(nspec.gt.0) then
         write(6,*)' Number of events with spectra', nspec
         write(6,*)' Spectral parameters in file mag_spec.out'
      endif

      stop
      end

C
C  SUBROUTINE LINFIT
C
C  PURPOSE
C   MAKE A LEAST-SQUARES FIT TO DATA WITH A STRAIGHT LINE
C       Y = A + B*X
C
C  USAGE
C    CALL LINFIT (X, Y, SIGMAY, NPTS, MODE, A, SIGMAA, B, SIGMAB ,R)
C
C  DESCRIPTION OF PARAMETERS
C       X       - ARRAY OF DATA POINTS FOR INDEPENDENT VARIABLE
C       Y       - ARRAY OF DATA POINTS FOR DEPENDENT VARIABLE
C       SIGMAY  - ARRAY OF STANDARD DEVIATIONS FOR Y DATA POINTS
C       NPTS    - NUMBER OF PAIRS OF DATA POINTS
C       MODE    - DETERMINES METHOD OF WEIGHTING LEAST-SQUARES FIT
C                 +1 (INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C                  0 (NO WEIGHTING) WEIGHT(I)=1.
C                 -1 (STATISTICAL)  WEIGHT(I)=1./Y(I)
C       A       - Y INTERCEPT OF FITTED STRAIGHT LINE
C       SIGMAA  - STANDARD DEVIATION OF A
C       B       - SLOPE OF FITTED STRAIGHT LINE
C       SIBMAB  - STANDARD DEVIATION OF B
C       R       - LINEAR CORRELATION COEFFICIENT
C
C  SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C    NONE
C
C  MODIFICATIONS FOR FORTRAN II
C    OMIT DOUBLE PRECISION SPECIFICATIONS
C    CHANGE DSQRT TO SQRTF IN STATEMENTS 67,68, AND 71

      subroutine linfit(x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
      dimension x(*),y(*),sigmay(*)
c
c   accumulate weighed sums
c
 11   sum=0.0
      sumx=0.0
      sumy=0.0
      sumx2=0.0
      sumxy=0.0
      sumy2=0.0
 21   continue
      do 50 i=1,npts
        x1=x(i)
        y1=y(i)
c       if(mode) 31,36,38
        if(mode.EQ.0) GOTO 36
        if(mode.GT.0) GOTO 38
c31     if(y1) 34,36,32
 31     if(y1.LT.0) GOTO 34
        if(y1.EQ.0) GOTO 36
 32     weight=1.0/y1
        go to 41
 34     weight=1.0/(-y1)
        go to 41
 36     weight=1.0
        go to 41
 38     weight=1.0/sigmay(i)**2
 41     sum=sum+weight
        sumx=sumx+weight*x1
        sumy=sumy+weight*y1
        sumx2=sumx2+weight*x1*x1
        sumxy=sumxy+weight*x1*y1
        sumy2=sumy2+weight*y1*y1
 50   continue
c
c   calculate coefficients and standard deviations
c
 51   delta=sum*sumx2-sumx*sumx
      a=(sumx2*sumy-sumx*sumxy)/delta
 53   b=(sumxy*sum-sumx*sumy)/delta
cc
c  patched up to use same varnce at all times jh jun 94
c
c61   if(mode) 62,64,62
c62   varnce=1.0
c     go to 67
 64   c=npts-2
c
c   modified to not crash with only 2 points
c
      if(c.gt.0) then
         varnce=(sumy2+a*a*sum+b*b*sumx2
     *   -2.0*(a*sumy+b*sumxy-a*b*sumx))/c
      else
         varnce=0.0
      endif
c  put abs values to not chash with negative sqrt (jh,jan 96)
 67   sigmaa=sqrt(abs(varnce*sumx2/delta))
 68   sigmab=sqrt(abs(varnce*sum/delta))
 71   r=(sum*sumxy-sumx*sumy)/
     *sqrt(abs(delta*(sum*sumy2-sumy*sumy)))
      return
      end


	

      subroutine invxy(n,y,x,z,a,b,cc)
c
c   invert n equations y=ax + bz + cc
c
      implicit none
      real x(*),y(*),z(*)
      double precision c(3,3),h(3)     ! for normal equations
      double precision t
      real a,b,cc
      integer i,j,k,n
c
c   zero
c
      do i=1,3
        h(i)=0.0
        do k=1,3
          c(i,k)=0.0
        enddo
      enddo
c
c   make normal equations
c
      do i=1,n
        c(1,1)=c(1,1)+x(i)*x(i)
        c(1,2)=c(1,2)+x(i)*z(i)
        c(1,3)=c(1,3)+x(i)
        c(2,2)=c(2,2)+z(i)*z(i)
        c(2,3)=c(2,3)+z(i)
        h(1)=h(1)+x(i)*y(i)
        h(2)=h(2)+z(i)*y(i)
        h(3)=h(3)+y(i)
      enddo
c
c   fill out rest
c
      c(3,3)=n
      c(2,1)=c(1,2)
      c(3,2)=c(2,3)
      c(3,1)=c(1,3)
      c(3,2)=c(2,3)
c
c   do inversion
c
      do k=1,2
         do i=k+1,3
           t=c(k,i)/c(k,k)
              do j=i,3
                 c(i,j)=c(i,j)-t*c(k,j)
              enddo
         enddo
       enddo
       do i=2,3
          do k=1,i-1
             h(i)=h(i)-c(k,i)*h(k)/c(k,k)
          enddo
       enddo
       h(3)=h(3)/c(3,3)
       do i=2,1,-1
          do k=i+1,3
             h(i)=h(i)-c(i,k)*h(k)
          enddo
          h(i)=h(i)/c(i,i)
       enddo
c
       a=h(1)
       b=h(2)
       cc=h(3)
       return
       end

      SUBROUTINE SDV(N,X,AV,SD)
C
C   CALCULATES STANDARD DEVIATION SD AND AVERAGE OF
C   N DATA SAMPLES X. IF N LT 10, WEIGHT IS (N-1)
C
      DIMENSION X(*)
C
      IF(N.EQ.0) RETURN
      SD=0.0
      AV=0.0
      DO 1 I=1,N
      AV=AV+X(I)
 1    CONTINUE
      AV=AV/N
      IF(N.EQ.1) RETURN
      DO 2 I=1,N
      SD=SD+(AV-X(I))*(AV-X(I))
 2    CONTINUE
      IF(N.GE.10) SD=SQRT(SD/N)
      IF(N.LT.10) SD=SQRT(SD/(N-1))
      RETURN
      END
C
C

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine xy_plot_input(x0,y0,c,x,y,xc,yc)
c
c  reads positions from the screen much like the main routine
c  all scaling values from routine xy_plot via common
c  x0,y0 are lower left hand corner of plot
c
c  x0,yo : same input as main routine
c  x,y   : output in scaled units
c  c     : character pressed
c  xc    : xscale
c
       implicit none
       character*1 c
       integer ichar
       reaL xc,yc   ! same as x and yscale
       real x1,x,y1,y   ! help variables
       real xfirst,yfirst,x0,y0,xscale,yscale,ymin,ymax,xmin,xmax   ! see xy_plot
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax
c
c   call up cursor
c

1           continue
            call xscursr(ichar,x1,y1)
            if (char(ichar).eq.'#') goto 1
            c=char(ichar)
            x=(x1-x0)/xscale+xfirst
            y=(y1-y0)/yscale+yfirst
            xc=xscale
            yc=yscale
c
      return
      end

