cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
c
c             HYPOSUB1 version 4.0     BRL 6/98
c
c           uses IASP91 software to locate globally as well as locally
c
cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH 

      subroutine hypocent(ifunct,iustat,iuphs,iulst,iusum,iutab,init,
     &inform,data,modnam,eventno,dlt,isort,test,yter,maxline,nstation,
     &locate,covar,oterr,nhead,nrecord,use_eev,nmessage,message)
c
c******** NOTE****** This routine will only locate one event unless
c                    statements around 9   continue are commented out
c                    this for use with seisan
c
c
c  Updates:
c   nov 24, 91 by j.h. : array data added for input in nordic format
c                        changes in getphasn and hypocent
c   dec 1              : make sure routine returns if # of phases lt 3 or
c                        return to 9 for other reason, comment out
c                        if multi event location
c   jan 3   92 by. jh  : bl's new version  (nov 91) updated with new nordic 
c                        format changes from dec 91
c                        use S for starting depth  (same loc as F for fixed)
c   jan 7              : magnitude correction is given to individul station
c                        instead of to average
c   feb 7 92  by j.h   : do not require that veloc .ne. 0.0 to use azimuth
c   May 92 BRL         : added travel time table interpolation for distant
c                        events
c   June 10/92 BRL     : added test parameters to getphasn window
c
c   June 11/92 BRL     : added Buland's IASP91 travel time software
c                        for distant events 
c   oct 27, 92, JH     : always write on screen if station is missing
c   nov 27, 92, JH     : change format to  summary file
c   Oct 11, 93 BRL     : added PARAMETER statement for dimensions
c                        This is now in the include file 'hypparm.inc'
c                        If the dimensions in this file are changed,
c                        hyposub2, hyposub4, hypoloc and hypoloc1 must ALL BE
c                        RECOMPILED as well as hyposub1
c                        added ifunct=3 to print residual summary
c                              ifunct=4 to zero residual sums
c   Dec 30, 93 BRL       ifunct=1 no longer zero's residual count:
c                        needs a separate call with ifunct=4
c   April 2, 1994 BRL    Added eventno to input window & printout
c   4/94                 Added fixor (fix origin time option)
c                        added yter, residual print to screen option
c   apr  28   94  jh     new test
c   may 5, 94     jh     write out geographic, not geocentric coordinates in
c                        data(1), include nstat for header
c   may 11               more on sorting, add locate in window
c   may 27 94     jh     shift residual output in nordic file format to f5.1
c   may 27 94     brl    set fixdep='F' when test(31)<=2
c   Sep 94        brl    added xmag & magtype to getphasn
c   Oct 94        brl    added error output in data array, nhead and nerecord
c                        to input window            
c   Oct 94        brl    added test(31)=-2 option to fix depth at STATION0.HYP
c                        value
CJAB(BGS)Jan95        : Only try to use IASP91 files if they exist.
c   feb 95  jh           if lat or lon outside range, fix to 90 or 360
c   jan 96  jh        : fix problem with lat between -1 and 0 and long between
c                       -1 and 0, bothe were between 0 and 1
c   mar 7 96 jh       : limit oterr to 999.99
c   nov 15, 96 jh     : origin times was fixed when hypocenter fixed, now ok
c   Jan 18, 98 mv     : due to a bug with the az residuals, a new variable 
c                       baz0(narriv) was created to calculate the backazimuths 
c                       at the same time than the azimuths in GEOCENTRIC !!!
c   jan 30     jh     : 5 magnitude residuals
c   jun 98     brl    : added spherical harmonic station corrections
c   sep 98     jh     . ----------------- seisan version 7.0 check -----------
c                       year 2000
c   oct 98              5 char station, if use hypo 71 data input, only 4 
c                       write gap on error line
c
c  oct 28 1998 bmt : linux changed, save and *
c   jul 26 99 jh      :add variable use_eev
c   oct 19 99 jh      : add test(94) to dela with w=4 reintroduction
c  nov 19          : remove v,vp,vs,d,nl,parm from window, to common block
c   dec 10         : year 2000 printout in hypsum.out
c   dec 10         : range of start depths
c   mar 23, 2000 lo: keep fixed origin time
c   aug 19       jh: add one_rms
c   oct 19, 2000   : read minutes of station lat and lon with f5.3 instead
c                    of f5.2, high accuracy output
c   jan 18, 2001   : get negative height from column 1 if depth > 1000 m
c   feb 19, 2001   : special code for BGS, see comments by bjb below
c   mar 4   2001 jh: add test 106 to enable only magnitude calcualtion
c                    write distance as real, fix reading of test above 99
c   may 5          : fix problem with multipel start depth and s-p phases,
c                    requred rereading of all phases for each new depth
c   jan 29  2002 jh: fix overflow in rms ouput to header line
c                    change ditnc eaccuracy in station line to make it
c                    more readbale (and less accurate)
c   feb 6  2002 jh : write angle of incidence in s-file
c   aug 3  2004 fernado carrilho: fix so oring time do not sometimes
c                    become zereo when locating in a sequence
c   may 24 2005 lot: take xnear and xfar from common block if set
c   may 30 2005 lot: added test(107) as flag to read xnear/xfar from sfile
c   feb 15 2006 lot: one more digit for travel time output in print.out
c   jan 24 2007 jh : max elevation was only possible to -999, now to -9999
c   mar 16 2009 jh : lat and long were always positive in hypsum.out. both
c                    negative deg and min.   
c   jun 10 2009 wc : change to length of text in stations()
c   oct 22 2009 jh : add now test parameter for selection of mb attenenuation 
c                    curve, default is now richter
c   may 06 2010 lo : change to writing of xnear/xfar/ztr to s-file, 
c                    only if set there before
c   sep 09 2010 jh : extra blank line in print.out when changing model, 
c                    lack of line
c                    caused crash in focmec reading velocity model in case of
c                    station file change
c   nov 15 2010 jh : save initial xnear and xfar so def values are reset 
c                    after being used for an individual event, as it was
c                    the values used in one event was used in all subsquent 
c                    events
c   dec 19 2010 jh : gfortran on pc does ot accept reading control line if no
c                    blank characters where numbers should be. solved by end=
c                    since numbers there then is read. now also stop if error
c                    or end of file end reading model and control line
c   nov 17 2011  jh: check for error in station file for station coordinates
c   oct 28 2012  jh: multiple depth does no work if s-p and undefined stations since
c                    reading in data again without checking for missing stations
c                    screw up data order. so disable rereading of data in case 
c                    of s-p. instead make sure originall data is kept as is by
c                    disabling reset of isp in hypoloc
c   nov 20 2012  jh: overflow in residual output
c   may 31 2014  jh: prevent crash if extra blank lines before model in station file
c   mar 19 2015  jh: two new reset test parameters
c  2015-06-02 pv   : add varibles to hyp common block due to
c                    compiler warning
c   dec 09 2015  jh: add test 113-115
c   dec 22 2015  jh: add test 116
c   mar 2  2016  jh: add return if icd=6, for se
c   mar 12 2016  jh: add nmessage and message for se
c   mat 18 2016  lo: test 116 default changed to 0=average
c   may 22 2016  jh: make sure maxline is transferred to hyposub3
c   feb 25 2017  jh: bug with not testing all start depths if one failed
c   dec 20 2017  ??: ?????????????
c   jan 12 2018  jh: add to message if station missing
c
cHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
c--- array data for one event in nordic format
c      character*80 data(1)
      save
      logical locate   ! false if event was not located

c--indicator if event has been read, for seisan
      integer one_pass,eventno
c
c    dimensions now in hypparm.inc
      include 'hypparm.inc'
      include 'seisan.inc'
      include 'rea.inc'
c      include 'param.inc'
      dimension fmg(nstats),fmgc(narriv),fmp(narriv)
      dimension fma(nstats),pres(nstats),sres(nstats),
     +pknt(nstats),scrst(nstats,9),dly(nstats),
     +sknt(nstats),sres2(nstats),pres2(nstats)
     +,kntp(nstats),knts(nstats),sknt2(nstats),pknt2(nstats),
     +sw2res(nstats),pw2res(nstats),ielv(nstats)

      dimension la(nstats),alat(nstats),lo(nstats),alon(nstats)
      dimension xhdiff(4),xhd2(4),covar(3,3)
      logical prnt(3)         
      logical use_eev     ! call is from eev
c      logical bgs_seisan
      character*8 phlst(10)
      integer nmessage
      character*80 message(50)
      character*80 modnam,datsave,testdat,prevstat
      character*1 pr(narriv),ins(nstats),iew(nstats),inform,
     +pr1(narriv),pr3(narriv),ins1,iew1,ucase,reff,fixepi,
     &fixdep,wtflag,phsflag,fd,fo,magtype
      real mag_cor(5,nstats)     !cmag
      character*5 s(nstats),stat
      character*4 thrmin
      character*5 depth
      integer*4 idd                                         
      character*1 yter,yquery,sgnla2,sgnlo2
      double precision orgsec,tph1,tph2,firstarr
      real min_rms   ! minimum rms when locating with several start depths
      real min_i     ! min i ...........................................
      integer n_start_depths   ! number of start depths to use
      logical diff_phase       ! difference phases like S-P present
      real    start_depth      ! first start depth
      real    start_depth_increment ! increment in start depths

c change BRL 12/29/93: fmg(narriv) changed to fmg(nstats)              
c 4/94: added sres2, pres2
c 9/94: added di
      character*5 old_stat
      real old_dist
      integer ksort(narriv) !changed 6/98
      integer nline,idummy
      real xlt(narriv)
      character*10 extension
      character*80 text
      
c 4/94: common/comm1/ now in include file
c      include 'comm1.inc'
c     common/hyp/s,mag_cor !cmag
c  pv: should look like this, but maxline was already used:
c     common/hyp/s,mag_cor,nline,maxline         therefore: 
      common/hyp/s,mag_cor,nline,idummy
c
c   transfer maxline to common block, used in hyposub3, jh may 2016
c
      idummy=maxline
c
c reset nmessage, lo correct???
c 
       nmessage=0

c      dimension tt(niasp),dtdl(niasp),dtdh(niasp),dddp(niasp)

      pi=3.1415927
      degtorad=0.0174533
      rearth=6371.
      do i=1,max_data
        data5(i)='                                        '//
     &           '                                        '
      enddo
c
c call function to determine ig BGS seisan outputs are to be used
c bjb 2001/02/14
c
c      call get_bgs_seisan(bgs_seisan)
      call get_env_seisan_extension(extension)

c    ifunct=1 : initialization (only needed once)   <<<<<<<<<<<<<<<<<<<<<<<<<<<

      if(ifunct.eq.1)then

c    set test parameter defaults

        call settest(test)

c set up iasp91 variables and files
        if(test(62).ne.0.0)then
          phlst(1)='all     '
        else
          phlst(1)='basic   '
        endif

c print flags for iasp91 software - disables printout
        prnt(3)=.false.
        prnt(1)=.false.
        prnt(2)=.false.

c readin the iasp91 filenames, open the files and initialize
        if( modnam .ne. ' ' ) then  !JAB(BGS)Jan95
        call tabin(iutab,modnam)
        i1=index(modnam,' ')-1
        call brnset(1,phlst,prnt)
        end if                      !JAB(BGS)Jan95.
c
c    read test parameter changes and station data
c    changed 4/94 to read character string and check for ")" so reset test(1) is OK
        stat='xxxxx'
        j=-1
        if(iulst.gt.0.and.(.not.use_eev))write(iulst,'(a)')' '       
        do while (stat.ne. '     '.and.j.ne.0)
         read(iustat,'(a80)')testdat
         if(testdat(14:14).eq.')')then
          read(testdat,'(a5,t12,i2,t16,f9.4)')stat,j,testj
         elseif(testdat(13:13).eq.')')then
          read(testdat,'(a5,t12,i1,t15,f9.4)')stat,j,testj
         elseif(testdat(15:15).eq.')')then
          read(testdat,'(a5,t12,i3,t17,f9.4)')stat,j,testj
         else
          read(testdat,'(a5)')stat
          j=-1
         endif
         if(j.gt.0)then
          test(j)=testj
          if(iulst.gt.0.and.(.not.use_eev))
     *    write(iulst,'(1x,''Reset test('',i3,'')='',
     &    f10.4)')j,testj
         endif
        end do
        
        if(iulst.gt.0)write(iulst,'(''    '')')
        
        call stations(s,la,alat,ins,lo,alon,iew,ielv,dly,fmg,scrst,nstat
cmag     &  iustat,iulst,init,nstats)
     &  ,iustat,iulst,init,nstats,mag_cor,use_eev)

        ichk=0
        iazflag=int(test(50))
        
        if(ichk.eq.0)write(iulst,'(''     ---------'',
     &  '' HYPOCENTER Version 4.0  1998 -----------''//)')
        ichk=1

c change 10/93: find max elev. station here rather than for each phase
        maxelv=-9999.
        do i=1,nstat
          if(ielv(i).gt.maxelv)then
            maxelv=ielv(i)
            iemx=i
          endif
        end do
        if(iulst.gt.0)write(iulst,1729)s(iemx),la(iemx),alat(iemx),
     &  ins(iemx),lo(iemx),alon(iemx),iew(iemx),maxelv
1729    format(1x,/,' Maximum elev. station: ',a5,1x,2(I4,f5.2,a1),
     &  '   elevation = ',i5,' m')

        if(test(40).ne.0.0.and.iulst.gt.0)then
          write(iulst,*)' ********* used as depth origin *************'
        endif
        if(iulst.gt.0)write(iulst,*)
c
c  check if not extra blank line
c
        read(iustat,'(a)') text
        if(text.eq.' ') then
           write(6,*) 'Extra blank line before model in station file'
           stop
        endif
        backspace(iustat)

c     read in velocity model
c   if vs(i) is not 0.0, the vs(i) are used to find separate
c   travel times and derivatives - otherwise vp/vs ratio is used

        i=1

c    reff is used to specify the moho layer for PN calculation
5       read(iustat,101,end=1999,err=1999)v(i),d(i),vs(i),reff
101     format(3f7.3,a1)
        if(ucase(reff).eq.'N')nmoho=i

c 4/94: added nconrad variable
        if(ucase(reff).eq.'B')nconrad=i
c        write(6,*) v(i),d(i),reff
        
        if(v(i).eq.0.0)go to 6
        i=i+1
        go to 5

c    nl is the number of layers in the velocity model
6       nl=i-1
c
c  model error
c
        goto 2000
 1999   continue
        write(6,*)' Error with model in station file'
        stop
 2000   continue

c    read in trial depth and vp/vs ratio
        read(iustat,'(3f5.0,f5.2,i5,2f5.1)', end =2929, err=2930)
     *  ztr,xnear,xfar,pos,
     *  n_start_depths,start_depth,start_depth_increment
        goto 2929
c
c   control line error
c

 2930   continue
        write(6,*) 'Error in control line in station file'
        stop
 2929   continue
c
        if(n_start_depths.eq.0) n_start_depths=1
        if(xnear.eq.9999.)xnear=20000.
        if(xfar.eq.9999.)xfar=20000.
        if(iulst.gt.0)write(iulst,'(/,'' Trial depth = '',f5.2,
     &  ''  Vp/Vs = '',f5.2,/)')ztr,pos

c
c   save  jh nov 2010
c
        xnear_old=xnear
        xfar_old=xfar
        ztr_old=ztr

c    if vs(1)=0 then set vs(i)=v(i)/pos
        if(vs(1).eq.0.0)then
          do i=1,nl
            vs(i)=v(i)/pos
          end do
        endif
c        write(6,*) 'iulst',iulst

        if(iulst.gt.0)write(iulst,'('' Velocity Model'',/)')
        if(iulst.gt.0)write(iulst,'('' Depth, km   Vp, km/s   Vs, km/s''
     &  )')

        do  i=1,nl
         reff=' '

c 4/94: added printout of moho and conrad
         if(i.eq.nconrad)reff='B'
         if(i.eq.nmoho)reff='N'          
         if(iulst.gt.0)write(iulst,'(1x,f9.2,2f11.2,3x,a1)')
     &    d(i),v(i),vs(i),reff
        end do

c    convert depths to thicknesses and store parameters in parm(i)
        do i=1,nl-1
          parm(nl+i)=d(i+1)-d(i)

c    change 10/93: add maximum elevation to upper layer thickness
c    if test(40)=0.0 Need this because dtdx2 origin is always at maxelv
          if(i.eq.1.and.test(40).eq.0.0)then
            parm(nl+i)=d(i+1)-d(i)+float(maxelv)*.001
          endif
        end do
        do i=1,nl
          parm(i)=v(i)
        end do

c 6/98: added convergence flag
        icflag=0 
        imflag=0

        if(iulst.gt.0.and.(.not.multi_model))write(iusum,849)
849     format(' Date    Origin    Lat ',1x,'    Long ',1x,
     &  '    Depth    Mag No Gap Dmin  Rms  Erh  Erz  Erx      Cvxy',
     &  '      Cvxz      Cvyz     Oterr')
c        if(iulst.gt.0.and.(.not.multi_model))
c    *   write(iulst,'(/,'' Input phases:'',/)')
        return
c1999    init=-1
c        return
      endif

c   end of initialization  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

c   location <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      if(ifunct.eq.2)then
c
c set xnear and xfar if set in event file
c lot 24/5/2005
c
        if (xxnear.ne.-1..and.xxfar.ne.-1.) then
          xnear=xxnear
          xfar=xxfar
        else                       ! jh nov 2010
          xnear=xnear_old
          xfar=xfar_old
          ztr=ztr_old
        endif

c BRL 6/98: save old header before it gets overwritten
        datsave=data(1)

c +++++++++++++++++++  read phases  ++++++++++++++++++++++++
        if(inform.eq.'N')one_pass=0
c
c   with seisan, only pass here once, set flag
c
9       continue
c
c   check for more passes, comment out if not using seisan
c   indicated with N, locate is false if no location was made
c
        if(one_pass.eq.1.and.inform.eq.'N') then
          locate=.false.
          return
        endif

c added 4/94: write event no
        if(iulst.gt.0.and.(.not.multi_model))
     *  write(iulst,'(/,'' EVENT # '',i6,/)')eventno

c intialize the phase count
        nphase=0

c remove comment in this line for seisan
        one_pass=1

c     read in the phase data (HYPO71 format)
        if(inform.eq.'H')then
          if(init.ne.-1)call getphase(nphase,st,phase,pr1,pr,pr3,ip,tp,
     &    fmp,imin,init,iuphs,iulst,narriv)

          if(nphase.eq.0)return
        endif

c     read in the phase data (Nordic format)
        if(inform.eq.'N')then
          ires=0
          ks=0

c  added test parameters to window BRL 6/92
c  added distance indicator, distind

c 11/92 BRL added orgsec to window
c 10/93 removed init.ne.-1 condition
c 4/94: added fixor                                              
c 10/94: added oldrms
          call getphasn(ks,iazflag,st,phase,pr1,pr,pr3,ip,
     &    tp,fmp,fixdep,depth,fixepi,yres,xres,orgsec,fixor,
     &    iulst,data,test,distind,narriv,imap,xmag,magtype,
     &    oldrms,appar_veloc)


c 10/93: make sure distind is u.c.
          distind=ucase(distind) 
          
          nphase=ks                          
          nphase1=ks
          if(nphase.eq.0)return
          if(ucase(fixdep).eq.'F')ires=1
c          if(ucase(fixepi).eq.'F')ires=9  ! how it was before nov 96         
          if(ucase(fixepi).eq.'F')ires=8   ! add jh nov 96
          if(ucase(fixepi).eq.'F'.and.ucase(fixdep).eq.'F'.and.
     *       ucase(fixor) .eq.'F') ires=9            ! add jh nov 96        
          if(ucase(fixepi).eq.'F'.and.ucase(fixdep).eq.'F')ires=7   ! jh nov 96
          if(ucase(fixepi).eq.'F'.or.ucase(fixepi).eq.'S')tph0=orgsec
          if(ucase(fixor).eq.'F'.and.ires.ne.9)ires=2
          
c 4/94: fix origin time
          if(ucase(fixor).eq.'F'.or.test(31).eq.0.)then
           tph0=orgsec
          endif 

c  transform decimal lat./long. to geocentric radians
          lar=int(yres)
          xlatr=abs(yres-float(lar))*60.
          lor=int(xres)
          xlonr=abs(xres-float(lor))*60.

c    lat/long are -ve for S/W  - change back for fold subr.
          ins1='N'
c         if(lar.lt.0)then      ! was so before jan 19, jh
          if(yres.lt.0.0) then  ! new version after jan 19,86
            lar=iabs(lar)
            ins1='S'
          endif
          iew1='E'
c         if(lor.lt.0)then      ! was so before jan 19, 96 jh
          if(xres.lt.0.0) then  ! new verison after jan 19,96, jh
            lor=iabs(lor)
            iew1='W'
          endif      
          call fold(yresn,xresn,lar,ins1,xlatr,lor,iew1,xlonr)

c 12/29/93: added error trap to this statement
c   zresn is measured relative to the origin (sea level or maxelv if test(40)<>0)          
          read(depth,'(f5.1)',err=883)zresn
          go to 884
883       if(iulst.gt.0)then
           write(iulst,'('' character in starting depth'',
     &     '' field on input phase header: '', a5)')depth
           write(iulst,'('' starting depth set to '',f10.1)')ztr
          endif 
          write(*,'('' character in starting depth'',
     &    '' field on input phase header: '', a5)')depth
          zresn=ztr
          
884       continue
        endif
c +++++++++++++++++++++ end of phase read +++++++++++++++++++++++++++++

        kss=1
        iphs=0       

c    find the station coordinates and corrections
        do 133 i=1,nphase
c
c  initilize ips jh oct 28 2012
c
          ips(i)=0

          do 15 j=1,nstat
15        if(s(j).eq.st(i))go to 16

          if(iulst.gt.0)write(iulst,105)st(i)
          write(*,105)st(i)
105       format(1x,'Station ',a5,' is not on station list: ignored')
          nmessage=nmessage+1
          write(message(nmessage),105) st(i)

c added 4/94 to clear old distance from no station phase          
          write(data(imap(i))(61:79),'(''                   '')')

          go to 133
16        continue  
          iphs=iphs+1        
          do k=1,9
           scorr(iphs,k)=scrst(j,k)
          end do 
          imap(iphs)=imap(i)                                                              
c          write(iulst,*)i,j,st(i),s(j)


c change BRL 12/27/93: if test(67)<>0 set blank phases to 'P    '
          if(phase(iphs)(1:4).eq.'    '.and.test(67).ne.0.0)
     &    phase(iphs)='P   '
        
c    convert to geocentric lat., y0(iphs), and long., x0(iphs)
          call fold(y0(iphs),x0(iphs),la(j),ins(j),alat(j),
     &    lo(j),iew(j),alon(j))                          

c    store phases elevation in iselv
          iselv(iphs)=ielv(j)

c change 10/93: max elev removed from here: in init section

          phase(iphs)=phase(i)
          ip(iphs)=ip(i)
          fmgc(iphs)=fmg(j)
          st(iphs)=st(i)
          pr1(iphs)=pr1(i)
          pr(iphs)=pr(i)
          pr3(iphs)=pr3(i)
          tp(iphs)=tp(i)
          fmp(iphs)=fmp(i)
          if(phase(iphs)(1:1).eq.'P')dl(iphs)=dly(j)
          if(phase(iphs)(1:1).eq.'S')dl(iphs)=dly(j)*pos

c    calculate the station weights, dtwt(i)  (never changed)
          if(ip(iphs).ge.0.and.ip(iphs).le.4.and.phase(iphs)(1:2).ne.
     &    'AZ')then

            dtwt(iphs)=(4.-float(ip(iphs)))/4.   
c            write(iulst,'(a8,f6.1)')phase(iphs),dtwt(iphs)
          elseif(phase(iphs)(1:2).eq.'AZ'.and.ip(iphs).ge.0)then

c change 10/93: calculate azimuth weights here
            dtwt(iphs)=(4.-float(ip(iphs)))/4./(degtorad*test(52))
          elseif(ip(iphs).ne.9)then
            write(iulst,'(a5,1x,a8,i3, '' invalid weight: set to zero''
     &       )')st(iphs),phase(iphs),ip(iphs)       
            dtwt(iphs)=0.0
          endif

c change 10/93: initialize dtw1
          dtw1(iphs)=0.0

133     continue

        nphase=iphs 
        
c 5/94 find first arrival station
        if(fixepi.eq.' '.or.ucase(fixepi).eq.'N')then
         firstarr=1.d100
         ifirst=0
        
         do i=1,nphase        
          if(tp(i).lt.firstarr)then
           firstarr=tp(i)
           ifirst=i
          endif
         enddo  
         xres=x0(ifirst)
         yres=y0(ifirst)
c         write(iulst,*)xres/degtorad,yres/degtorad
        endif 


c                   HYPO71 Feature

c   if phase data set is terminated by a 9 in col 19, output is calculated
c   at the latitude, longitude and depth specified on the next card
c   if col 19 is 2, fix origin time to value on next card (mod. 8/18/86)
c   NOTE: this origin time is referenced to the year, month, day and hour 
c   of the first arrival station.
c   if col 19 is 1, fixed depth solution at zres (no extra card needed)

c    ires values of 2 and 9 are hypo71 features - see hypo71 manual

        if(inform.eq.'H')then
          kmin=imin
          ires=kmin-(kmin/10)*10
          if(ires.eq.9.or.ires.eq.8.or.ires.eq.2)then
            read(iustat,771)iorg1,org2,lar,xlatr,lor,xlonr,zres
            if(iulst.gt.0)write(iulst,771)iorg1,org2,lar,xlatr,lor,
     &      xlonr,zres
            call timsec(iyear,imonth,iday,ihr,iorg1,org2,tph0)
771         format(i5,f5.2,2(i5,f5.2),f5.2)

c    lat/long are -ve for S/W  - change back for fold subr.
            ins1='N'
            if(lar.lt.0)then
              lar=iabs(lar)
              ins1='S'
            endif
            iew1='E'
            if(lor.lt.0)then
              lor=iabs(lor)
              iew1='W'
            endif      

            call fold(yres,xres,lar,ins1,xlatr,lor,iew1,xlonr)
          endif
          if(ires.eq.8.or.ires.eq.9)fixepi='F'
          if(ires.eq.9)fixdep='F'
        endif

        if(ires.eq.9.or.ires.eq.7.and.iulst.gt.0)write(iulst,772)
772     format(' ***** hypocenter fixed by operator ... ires = 9 *****'
     &  /)
        if(ires.eq.2.or.ires.eq.7.and.iulst.gt.0.or.ucase(fixor)
     &  .eq.'F'.or.test(31).eq.0.0)write(iulst,773)
773     format(' ***** origin time fixed by operator .. ires = 2 *****'
     &  /)
        if((ires.eq.1.or.abs(test(31)).le.2.).and.iulst.gt.0)
     &  write(iulst,774)
774     format(' ***** depth fixed by operator .. ires = 1 or test(31)'
     &  ,'<= 2 *****'/) 
        if(iulst.gt.0.and.(ires.eq.8.or.abs(test(31)).le.1.0))then
         write(iulst,775)                                        
        endif 
775     format(' ***** epicenter fixed by operator .. ires = 8',
     &  ' *****'/) 

c    setup starting hypocenter, xh
        ix=1
        iy=2
        iz=3
        io=4
        nn=2*nl-1
        xh(1)=xres
        xh(2)=yres
        xh(3)=ztr

        if(inform.eq.'N')then
        
c 10/94: fix to header value if test(31)=+2, leave at ztr if test(31)=-2
c---fixed depth  Added test(31)=1 to fix at depth on phase header
          if(ucase(fixdep).eq.'F'.or.test(31).eq.2.0.or.
     &     abs(test(31)).le.1.and.depth.ne.'     ')xh(3)=zresn
c---starting depth. 
          if(ucase(fixdep).eq.'S')xh(3)=zresn
c---fixed epicenter
          if(ucase(fixepi).eq.'F'.or.ucase(fixepi).eq.'S'.or.
     &     abs(test(31)).le.1.)then
            xh(1)=xresn
            xh(2)=yresn
            ix=1
            iy=2
            iz=3
            io=4
            nn=2*nl-1

          endif
        endif

c 4/94: fix epicenter if test(31)=1          
          if(abs(test(31)).le.1.0)fixepi='F'

c 5/94: fix depth if abs(test(31))<=2          
          if(abs(test(31)).le.2.0)fixdep='F'
          
c 5/94: fix origin time if test(31)=0          
          if(test(31).eq.0.0)fixor='F'

          
c change 10/93: added fixdep and data to window & changed ielv to iselv  
c 4/94: added parm,fixor & isort to window
c 10/94: added oterr to window
c--------------------------------------------------------------------------
c   enter a loop to try all start depths and select solution with smallest
c   rms unless depth is fixed or a starting depth is provided in S-file
c--------------------------------------------------------------------------
c
      if(fixdep.ne.'F'.and.fixdep.ne.'S') then
         min_rms= 999999.0
c
c   find if any difference phases that requres initialization
c   of input variables since some variables times rare replaced
c   bt e.g. S-P times in hypoloc1, hard to figure out exactly which
c   easier to read all again
c
c   oct 28 jh disable since does not work if missing stations
c
c         if(n_start_depths.gt.1) then
c            diff_phase=.false.
c            do i=1,nphase          
c              if(ip(i).eq.9) diff_phase=.true.
c            enddo
c         endif
c
c
c   loop for different start depths
c
         do i=1,n_start_depths
            if(n_start_depths.gt.1) then
c                if(diff_phase) then   ! rered all from 2. time
c                   call getphasn(ks,iazflag,st,phase,pr1,pr,pr3,ip,
c     &             tp,fmp,fixdep,depth,fixepi,yres,xres,orgsec,fixor,
c     &             iulst,data,test,distind,narriv,imap,xmag,magtype,
c     &             oldrms,appar_veloc)
c                endif
                xh(3)=start_depth+(i-1)*start_depth_increment
             endif
c
             call hypoloc(nphase,st,maxelv,phase,tp,dtwt,
     *       ip,x0,y0,iselv,dl,
     &       ires,xnear,xfar,xh,tph0,nmoho,nconrad,test,
     *       iulst,xh1,
     &       tph,std1,dtwt1,tpc,im,dt,dlt,az0,aninc,
     *       ips,rms,xk,ndata,icd,
     &       fixepi,fixdep,fixor,dtw1,distind,trphs,
     *       vapp,azapp,alatm,alonm,
     &       data,isort,di,oterr,nzcount,baz0,scorr,appar_veloc,
     *       nmessage,message)
c
c   added for se mar 2016 jh, return if no location but only
c   if only one start depth, else other start depths are not tested
c
             if(icd.eq.6.and.n_start_depths.eq.1) then
                locate=.false.
                return
             endif
c
c   find minimum rms
c
             if(rms.lt.min_rms) then
                min_i=i
                min_rms=rms
             endif
c            write(6,*) i,rms, xh1
         enddo
c        write(6,*) min_i
c
c   if only one start depth, continue, else repeat with that
c   start depth unless the last
c
         if(n_start_depths.gt.1.and.min_i.ne.n_start_depths) then
c             if(diff_phase) then
c                 call getphasn(ks,iazflag,st,phase,pr1,pr,pr3,ip,
c     &           tp,fmp,fixdep,depth,fixepi,yres,xres,orgsec,fixor,
c     &           iulst,data,test,distind,narriv,imap,xmag,magtype,
c     &           oldrms,appar_veloc)
c             endif
c
             xh(3)=start_depth+(min_i-1)*start_depth_increment
             call hypoloc(nphase,st,maxelv,phase,tp,dtwt,ip,
     *       x0,y0,iselv,dl,
     &       ires,xnear,xfar,xh,tph0,nmoho,nconrad,
     *       test,iulst,xh1,
     &       tph,std1,dtwt1,tpc,im,dt,dlt,az0,aninc,
     *       ips,rms,xk,ndata,icd,
     &       fixepi,fixdep,fixor,dtw1,distind,trphs,
     *       vapp,azapp,alatm,alonm,
     &       data,isort,di,oterr,nzcount,baz0,scorr,appar_veloc,
     *       nmessage,message)
         endif
c
      else   ! location when depth fixed or one start depth

        call hypoloc(nphase,st,maxelv,phase,tp,dtwt,ip,x0,y0,iselv,dl,
     &  ires,xnear,xfar,xh,tph0,nmoho,nconrad,test,iulst,xh1,
     &  tph,std1,dtwt1,tpc,im,dt,dlt,az0,aninc,ips,rms,xk,ndata,icd,
     &  fixepi,fixdep,fixor,dtw1,distind,trphs,vapp,azapp,alatm,alonm,
     &  data,isort,di,oterr,nzcount,baz0,scorr,appar_veloc,
     *  nmessage,message)
      endif
      one_rms=rms   ! save rms this event, now used in grid search
c

c 10/94: put variance in covar
        do i=1,3
         do j=1,3
          covar(i,j)=var(i,j)
         enddo
        enddo                 
        
        if(nphase.lt.2)then
c          write(iulst,'(/''   Less than three phases for this event:'',
c     &    '' Not processed''/)')
          go to 9
        endif

        if(icd.eq.4.or.icd.eq.6)go to 9

c  minimum rms errors
        erh=sqrt(std1(ix)**2+std1(iy)**2)   
        
c change 9/94: output sqrt(var(i,i)) and 3 off-diagonal terms
c of complete covariance matrix
        erx=std1(ix)                    
        ery=std1(iy)
        erz=std1(iz)
        cvxy=rearth*rearth*var(ix,iy)
        cvxz=rearth*rearth*var(ix,iz)
        cvyz=rearth*rearth*var(iy,iz)

c        write(iulst,'(8f8.2)')xh1(2)/degtorad,xh1(1)/degtorad,
c     &  alatm,alonm

c  write start and end solutions on unit 30
c        write(30,'(8f8.2)')xh1(2)/degtorad,xh1(1)/degtorad,xh1(3),
c     &yres,xres,zresn

c calculate distance and azimuth of epicenter from mean station
c position
        call delaz(xh1(2),xh1(1),dekmm,dedegm,azzm,
     &  alatm*degtorad,alonm*degtorad)
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c this line inserted by M. villagran to compute backazimut
c at once and use it after for the residuals
c        call delaz(alatm*degtorad,alonm*degtorad,dekmm,
c     *  dedegm,azb,xh1(2),xh1(1))
cXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxx
        if(azapp.lt.0.)azapp=azapp+360.
c        write(27,'(8f8.2)')dedegm,azzm,azapp,vapp

c  calculate azimuthal spread of stations
c 10/94: changed gap calculation to sort using azimuth
        call r4sort(nphase,az0,isort)
        j1=0                          
        j2=0
        azmax=0.0
        do i=1,nphase
         j=isort(i)
         if(dtw1(j).gt.0.0.and.j1.eq.0)then
          j1=j             
          az1=az0(j1)
         elseif(dtw1(j).gt.0.0.and.j2.eq.0)then
          j2=j       
          az2=az0(j2)
          azgap=az0(j2)-az0(j1)
          if(azgap.gt.azmax)azmax=azgap
          j1=j2
          j2=0
         endif
        end do
        azmax1=360-az2+az1
        if(azmax1.gt.azmax)azmax=azmax1
        igap=int(azmax+0.49999)
        if(iulst.gt.0.and.(.not.multi_model)) then
        write(iulst,'(/'' Azimuthal Gap in Station '',
     &  ''Coverage'',i4,'' degrees'')')igap
        write(iulst,*)' '    ! gfortran pc
        endif
c  calculate coda magnitudes
        fmag=0.0
        kt=0

        do i=1,nphase
          fma(i)=0.0
          if(fmp(i).gt.0.0)then
            kt=kt+1
            fma(i)=test(7)+test(8)*alog10(fmp(i))+test(9)*dlt(i)+fmgc(i)
            fmag=fmag+fma(i)
          endif
        end do

        if(kt.ne.0)fmag=fmag/float(kt)

c 9/94: if magmitude=0 set it to value on header
        if(fmag.eq.0.0)fmag=xmag

c    convert epicenter to geographic lat/long

        call unfold(xh1(iy),xh1(ix),la1,ins1,ala1,lo1,iew1,alo1)
        lo2=lo1
        la2=la1
        alo2=alo1
        ala2=ala1

c changed 6/98 to separately assign signs - were wrong for -0.5 deg
        sgnlo2=' '
        if(iew1.eq.'W')then
         if(test(93).gt.0.0)then
          lo2=360-lo1
          if(alo1.gt.0.)then
           lo2=lo2-1
           alo2=60.-alo1
          endif 
         else
          sgnlo2='-'
         endif  
        endif 
        sgnla2=' '
        if(ins1.eq.'S')then
         sgnla2='-'
        endif 

c 10/94: changed test(31).eq.2 to abs(test(31)).le.2
        fd=' '
        if(ires.eq.1.or.ires.eq.9.or.abs(test(31)).le.2.)fd='*'
        
        fo=' '
        if(ires.eq.2)fo='*'

c    convert origin time to standard format
      call sectim(tph,iyr,jday,imnth,idy,ihn,imm,xhm)
        
c 3/94: save for data output
        xhmsave=xhm
        
c 4/94 read original month, day & year to which arrival times are referenced        
c2000        read(data(1)(2:10),'(i2,1x,2i2)')iyr1,imnth1,idy1                  
        read(data(1)(2:10),'(i4,1x,2i2)')iyr1,imnth1,idy1                  

c calculate difference in hours to be added to the arrival times        
c            call timsec(iyear,imonth,iday,ihr,iorg1,org2,tph0)
        call timsec(iyr1,imnth1,idy1,0,0,0.,tph1)
        call timsec(iyr,imnth,idy,0,0,0.,tph2)
        timdiff=sngl((tph2-tph1)/3600.d0)
c        write(iulst,*)' time diff ',timdiff
        
c  output hypo71 compatible summary
c2000        idd=iyr*10000+imnth*100+idy
        if(iyr.lt.2000) i=iyr-1900
        if(iyr.ge.2000) i=iyr-2000
        idd=i*10000+imnth*100+idy
 
        if(nzcount.eq.0)iddd=0
        
c    dmin = dist of event from closest station
        dmin=dlt(im)
        if(dmin.gt.999.9)dmin=999.9
        if(rms.gt.99.99)rms=99.99
        if(erh.gt.999.9)erh=999.9
        if(erz.gt.999.9)erz=999.9
        if(erx.gt.999.9)erx=999.9
        if(ery.gt.999.9)ery=999.9
        if(oterr.gt.999.99) oterr=999.99   ! change jh march 6, 96

c change JH 3/93: add zeros to hr,min format
c----------------------------------------------------------------------
        idmin=dmin+0.5
c       if(nzcount.gt.0)then
c  following fix by fernando carrilho to avoid orign time to be zero
c  when locang in sequence
        if(nzcount.gt.0.or.data(1)(11:11).eq.'F')then
         write(thrmin,'(2i2)')ihn,imm          
        else
         thrmin='    '
         xhm=0.0
        endif  
        do i=1,4
          if(thrmin(i:i).eq.' ')thrmin(i:i)='0'
        enddo
c changed 4/92: depth is always measured relative to the origin (sea level for test(40)=0)
c       if(test(40).eq.0.0)xh1(iz)=xh1(iz)-float(maxelv)*.001 

c 3/94: xsave, ysave used for location difference
c (geocentric, cannot be used for output j.j. 5/94)
        xsave=xh1(ix)
        if(test(93).gt.0.0)then
         if(xsave.ge.pi)xsave=xsave-2.*pi
         if(xsave.le.-pi)xsave=xsave+2.*pi
        endif 
        ysave=xh1(iy)
        zsave=xh1(iz)       
        
       if(xh1(iz).gt.999.99)xh1(iz)=999.99

c 9/94: output erx, cvxy, cvxz, cvyz so error ellipses can be calculated
c 10/94: added origin time error
        if(iulst.gt.0.and.(.not.multi_model)) then
           if (sgnla2.eq.'-') la2=-la2
           if (sgnlo2.eq.'-') lo2=-lo2
           if (sgnla2.eq.'-') ala2=-ala2
           if (sgnlo2.eq.'-') alo2=-alo2
           write(iusum,952)idd,thrmin,xhm,la2,ala2,lo2,
     &     alo2,xh1(iz),fd,fmag,ndata,igap,dmin,rms,erh,erz,erx,cvxy,
     &     cvxz,cvyz,oterr
c 
c   take signs back to what they were, not clear why they were
c   alway posistive, jh mar 09
c
           if (sgnla2.eq.'-') la2=-la2
           if (sgnlo2.eq.'-') lo2=-lo2
           if (sgnla2.eq.'-') ala2=-ala2
           if (sgnlo2.eq.'-') alo2=-alo2
         endif


952     format(i6,1x,a4,f6.2,i3,f6.2,i4,f6.2,1x,f6.1,
     +  a1,f6.1,i3,i4,f5.1,f5.2,3f5.1,4e10.3)

c accumulate mean square rms value
        if(nphase.gt.3.and.rms.le.test(43))then
          ievent=ievent+1
          rmssum=rmssum+rms*rms
        endif

c  output residual summary
        linecount=0

c changed format 4/94 BRL, again 9/94
c        write(6,*) 'iulst,multi_mod',iulst,multi_model
        if(iulst.gt.0.and.(.not.multi_model)) then
        write(iulst,*)'    '         ! gfortran pc
        write(iulst,'('' stn   dist   azm  ain w phas'',
     &   ''    calcphs hrmn tsec  t-obs  t-cal    res   wt di'')')
        endif

        kst=1
        res=0.0

c   unweighted rms calculation
c wtmax1 is the maximum weight originally specified as 1
c it is used to renormalize all the final weights
        wtmax1=0.0
        ndd=0

        do kst=1,nphase
         if(dtwt1(kst).gt.wtmax1.and.ip(kst).eq.0.and.phase(kst).ne.
     &   'AZ  ')wtmax1=dtwt1(kst)

c change 4/94: dtw1(kst) now .ge.0 - was previously .gt.0 allowing
c bisquare weighting to remove phases
         if(dtw1(kst).ge.0.0.and.phase(kst)(1:2).ne.'AZ')then
          res=res+dt(kst)**2
          ndd=ndd+1
         endif
        end do

        if(wtmax1.eq.0.0)wtmax1=1.0
        if(ndd.ne.0)res=sqrt(res/float(ndd))

c    process residuals------------------------------------------------------------------

c 4/94: sort stations by distance, dlt(i) in isort(j)
        call r4sort(nphase,dlt,isort)
c
c   now sort so the distance sorted phase lines remain in the same order
c   as in the original file
c
        old_stat=st(isort(1))
        old_dist=dlt(isort(1))
        k=0
        l=0
        nvalid=0

        do i=1,nphase

c          5/94: count valid phases
          if(dtwt(i).gt.0.0.and.dtw1(i).gt.0.0)nvalid=nvalid+1

           l=l+1
           if(st(isort(l)).eq.old_stat ! both station and distance
     *     .and.dlt(isort(l)).eq.old_dist) then
              k=k+1
              xlt(k)=isort(l)
           else
              if(k.gt.1) then
                  call r4sort(k,xlt,ksort)
                  m=1
                  do j=l-k,l-1
                     isort(j)=xlt(ksort(m))
                     m=m+1
                  enddo
              endif
              old_stat=st(isort(l))
              old_dist=dlt(isort(l))
              k=1
              xlt(k)=isort(l)
           endif
         enddo
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<        
                                                                                       
c 3/94: look for first phase record index (isindex) in data array
        isindex=2
        
        do while (data(isindex)(80:80).ne.'4'.and.data(isindex)(80:80)
     &  .ne.' ')
          isindex=isindex+1
        enddo
        
        yquery=ucase(yter)
        nazimuth=0
        prevstat='    '
        
c 4/94: changed loop index to k1 and distance sort index to k
        do k1=1,nphase

c 4/94: added pause for screen print
          if(yquery.eq.'Y'.and.linecount.gt.maxline)then
           write(*,'('' RETURN to continue, Q to quit printout '',$)')
           read(*,'(a1)')yquery
           if(ucase(yquery).ne.'Q')yquery='Y'
           linecount=0
          endif 

c k is the index of the sorted phase data which includes azimuths
          
          if(test(71).eq.0.0)then
           k=k1
          else
           k=isort(k1)  
          endif 
          
c imap is an array of index addresses in the original data array
          is=imap(k)   
         
c count the azimuths        
          if(phase(k)(1:2).eq.'AZ')nazimuth=nazimuth+1                                   
           
c          write(*,*)st(k),phase(k),data(is)(1:28)                     
c          read(*,*)

c 10/93: added '*' to showed chang weights and phases
          phsflag=' '
          wtflag=' '
          if(dtw1(k).ne.dtwt(k).and.ip(k).ne.9)wtflag='*'
          if(trphs(k)(1:4).ne.phase(k).and.phase(k)(1:2).ne.'AZ'.and.
     &    ips(k).ne.1.and.phase(k)(1:4).ne.'    ')phsflag='*'

c accumulate residual sums
c 4/94: added pres2 & sres2 to calculate rms deviation
          if(phase(k)(1:2).ne.'AZ'.and.ips(k).ne.1.and.ip(k).le.4)then
            wtt=(4.-float(ip(k)))/4.

c 10/93: exclude invalid phases
            if(dtw1(k).le.0.0)wtt=0.0
            if(rms.le.test(43))then
              do i=1,nstat
                if(s(i).eq.st(k))then
                  if(pr(k).eq.'S'.and.ip(k).ne.4.and.ips(k).ne.1)then
                    sres(i)=sres(i)+dt(k)*wtt 
                    sres2(i)=sres2(i)+(wtt*dt(k))**2
                    sw2res(i)=sw2res(i)+wtt*wtt*dt(k)
                    sknt(i)=sknt(i)+wtt
                    sknt2(i)=sknt2(i)+wtt*wtt
                    knts(i)=knts(i)+1
                  endif
                  if(pr(k).eq.'P'.and.ip(k).ne.4.and.ip(k).ne.9)then
                    pres(i)=pres(i)+dt(k)*wtt       
                    pres2(i)=pres2(i)+(wtt*dt(k))**2
                    pw2res(i)=pw2res(i)+wtt*wtt*dt(k)
                    pknt(i)=pknt(i)+wtt
                    pknt2(i)=pknt2(i)+wtt*wtt
                    kntp(i)=kntp(i)+1 
                  endif
                endif
              end do
            endif
          endif
          azz=az0(k)
          if(azz.lt.0)azz=azz+360
          if(dabs(tp(k)).gt.7200.)then
            tpp=sngl(tp(k)-tph)
            call sectim(tp(k),iyear,jday,imonth,iday,ihn,imm,xhm)
          else
            tpp=sngl(tp(k))
          endif

c   azimuth residual format
          if(phase(k)(1:2).eq.'AZ')then

c  subtract 180 from difference as input azimuth is relative to station
c  whereas calculated azimuth is relative to source (BRL 5/28/92)
c
cXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
c next 3 lines comented by m. villagran 
c the reason is that backazimuth must be calculated with geocentric
coordinates procedure (delaz) and not just az-180
c azb is calculated before at the same place where delaz is called
c           azb=azz-180.
c           if(azb.lt.0.0)azb=360.+azb
c           ttt2=sngl(tp(k))-azb
            ttt2=sngl(tp(k))-baz0(k)
c      write(6,*)'az0 = ',az0(k),'azb= ',baz0(k),'tp(k)= ',tp(k)
            if(ttt2.gt.180.)ttt2=360.-ttt2
            if(ttt2.lt.-180.)ttt2=360.+ttt2
            if(ttt2.gt.999.9)ttt2=999.9
            ttt=sngl(tp(k))
            if(ttt2.gt.999.9)ittt=999.9
            if(ttt2.lt.-99.9)ttt2=-99.9
            if(iulst.gt.0.and.(.not.multi_model))then

c  residual format change 10/93, 3/94, 9/94
              write(iulst,'(1x,a5,i5,f6.1,8x,a8,17x,2f7.1,
     &        f7.2,f5.2,a1,i2)')st(k),int(dlt(k)+.49999),azz,
c    &        phase(k),tp(k),azb,ttt2,degtorad*dtwt1(k),wtflag
     &        phase(k),tp(k),baz0(k),ttt2,degtorad*dtwt1(k),wtflag
     &        ,int(di(k)+.4999)
            endif
            write(data5(is)(37:),'(f7.2,1x,f7.2,1x,f5.2,1x,i2)')
     &       baz0(k),ttt2,degtorad*dtwt1(k),int(di(k)+.4999)
            
c 3/94:write azimuth residual into original data array
c 4/94: put <0 roundoff case in
            ittt=int(ttt2+.49999)
            if(ittt.gt.999)ittt=999
            if(ittt.lt.-99)ittt=-99
            write(data(is)(61:63),'(i3)')ittt            
             
c non-azimuth residuals
          else
            if(dlt(k).gt.99999.9)dlt(k)=99999.9
            if(tpp.gt.9999.9)tpp=9999.9
            if(tpp.lt.-999.9)tpp=-999.9
            if(tpc(k).gt.9999.9)tpc(k)=9999.9
            if(tpc(k).lt.-999.9)tpc(k)=-999.9
            if(dt(k).gt.999.99)dt(k)=999.99
            if(dt(k).lt.-99.99)dt(k)=-99.99
            if(dl(k).gt.999.99)dl(k)=999.99
            if(dtwt1(k).gt.999.99)dtwt1(k)=999.99
            if(fma(k).gt.999.99)fma(k)=999.99
            if(di(k).gt.99.)di(k)=99.

c residual output format for valid absolute arrivals
c 10/94: exclude difference parent phases if nzcount=0
            if(ips(k).eq.0.and.dtw1(k).ge.0.0.and.(ip(k).ne.9.
     &      or.nzcount.gt.0))then
              if(iulst.gt.0.and.(.not.multi_model)) then
c 2/2006 lot, 2 decimals for travel times
                if (tpp.lt.10000.0) then 
                  write(iulst,'(1x,a5,i5,f6.1,f5.1,1x,i1,1x,
     &          2a8,2i2,f5.1,2f7.2,f7.2,f5.2,a1,i2)')st(k),
     &          int(dlt(k)+.49999),azz,aninc(k),ip(k),phase(k),
     &          trphs(k),ihn,imm,xhm,tpp,tpc(k),dt(k),
     &            dtwt1(k)/wtmax1,wtflag,int(di(k)+.4999)
                else
                  write(iulst,'(1x,a5,i5,f6.1,f5.1,1x,i1,1x,
     &           2a8,2i2,f5.1,2f7.1,f7.2,f5.2,a1,i2)')st(k),
     &          int(dlt(k)+.49999),azz,aninc(k),ip(k),phase(k),
     &          trphs(k),ihn,imm,xhm,tpp,tpc(k),dt(k),
     &            dtwt1(k)/wtmax1,wtflag,int(di(k)+.4999)
                endif
              endif
c
c  2-02 jh  write ainc to data array
c
              write(data(is)(57:60),'(i4)') int(aninc(k)+0.5)
c store additional info
              write(data5(is),'(a8,1x,f8.3,1x,f8.3,1x,f5.2,1x,i2)')
     &         trphs(k),tpp,tpc(k),dtwt1(k)/wtmax1,int(di(k)+.4999)

c modification 6/13/92 (BRL) to not print calculated times and residuals
c when weight less than zero
            elseif(dtw1(k).lt.0.0.or.ip(k).eq.9.and.nzcount.eq.0)then
              if(iulst.gt.0.and.(.not.multi_model))
     *        write(iulst,'(1x,a5,i5,f6.1,6x,i1,1x,
     &         a8,8x,2i2,f5.1,f7.1,20x,a1)')st(k),
     &        int(dlt(k)+.49999),azz,ip(k),phase(k),ihn,imm,
     &        xhm,tpp

c format for difference phases
            elseif(ips(k).eq.1.and.dtw1(k).ge.0)then
              tpp=sngl(tp(k))
              if(tpp.gt.999.99)tpp=999.99
              if(tpp.lt.-999.99)tpp=-999.99
              if(iulst.gt.0.and.(.not.multi_model))
     *        write(iulst,'(1x,a5,i5,f6.1,f5.1,1x,i1,1x,
     &        2a8,9x,2f7.1,f7.2,f5.2,a1,i2)')st(k),int(dlt(k)+.49999),
     &        azz,aninc(k),ip(k),phase(k),trphs(k),tpp,tpc(k),dt(k),
     &        dtwt1(k)/wtmax1,wtflag,int(di(k)+.4999)
              write(data5(is),'(a8,1x,f8.3,1x,f8.3,1x,f5.2,1x,i2)')
     &        trphs(k),tpp,tpc(k),dtwt1(k)/wtmax1,int(di(k)+.4999)
            endif
         

c 3/94:write residual into original data array
            if(dtw1(k).ge.0.0.and.(ip(k).ne.9.or.nzcount.gt.0))then
c 5/94 write residual with f5.1, truncate if too large
             if(dt(k).lt.-99.9)dt(k)=-99.9
             if(dt(k).gt.999.9)dt(k)=999.9 
c             write(data(is)(64:68),'(f5.1)')dt(k)
c            if (dt(k).lt.100..and.dt(k).ge.-10.) then 
             if (dt(k).lt.99.9.and.dt(k).ge.-9.9) then ! jh 20/11/2012
               write(data(is)(64:68),'(f5.2)')dt(k) ! lot 21/09/2005
             else
               write(data(is)(64:68),'(f5.1)')dt(k) ! lot 21/09/2005
             endif

c 3/94: and final normalized weight*10
             write(data(is)(69:70),'(i2)')int(.4999+
     &       10.*dtwt1(k)/wtmax1)     
            endif 

c 3/94: and delta & azimuth
            
      data(is)(71:79)=' '
      if(dlt(k).lt.10.0) write(data(is)(71:75),'(f5.2)')dlt(k) 
      if(dlt(k).lt.100.0.and.dlt(k).ge.10.0)
     *write(data(is)(71:75),'(f5.1)')(dlt(k) -0.01) 
cjh jan 02      if(dlt(k).lt.1000.0.and.dlt(k).ge.100.0)
cjh jan 02     *write(data(is)(71:75),'(f5.1)') dlt(k)
      if(dlt(k).ge.100)write(data(is)(71:75),'(i5)')int(dlt(k)+0.5) 

c mar 20001            write(data(is)(71:75),'(i5)')int(dlt(k)+.49999)            
            iazz=int(azz+.49999)
            if(iazz.lt.0)iazz=iazz+360
            write(data(is)(77:79),'(i3)')iazz            
          endif 
c          write(iulst,*)k1,k,imap(k)
        end do 

c 4/94: added blank printout at end
        if(.not.multi_model) write(iulst,'(''      '')')
        
c 3/94: output difference between this and previous solution
        if(data(1)(24:38).ne.'               ')then

         dor=sngl(tph-orgsec)
         ddx=(xsave-xresn)
         if(ddx.ge.pi)ddx=ddx-2.*pi
         if(ddx.le.-pi)ddx=ddx+2.*pi
         ddx=ddx*rearth*cos(ysave)
         ddy=(ysave-yresn)*rearth
         ddz=zsave-zresn                    
         if(iulst.gt.0.and.(.not.multi_model))then
          write(iulst,'(/'' Difference from previous solution:''/)') 
          write(iulst,'('' dorigin= '',f7.1,'' sec dx='',f8.1,
     &    '' km dy='',f8.1,'' km dz= '',f7.1,'' km drms= '',f5.2)')dor,
     &    ddx,ddy,ddz,rms-oldrms 
         endif
c store in hyp block
         hyp_dx=ddx
         hyp_dy=ddy
         hyp_dz=ddz
         hyp_do=dor

c 5/94: use rms<test(45) to accumulate differences   
         if(rms.le.test(45).and.float(nvalid).ge.test(46))then
           xhdiff(1)=xhdiff(1)+ddx
           xhdiff(2)=xhdiff(2)+ddy
           xhdiff(3)=xhdiff(3)+ddz
           xhdiff(4)=xhdiff(4)+dor

           xhd2(1)=xhd2(1)+ddx*ddx
           xhd2(2)=xhd2(2)+ddy*ddy
           xhd2(3)=xhd2(3)+ddz*ddz
           xhd2(4)=xhd2(4)+dor*dor
           idiff=idiff+1
         endif  
        endif     

c  xxlat & xxlon are used to write the solution into the Nordic
c  output file header      
c 10/94: changed conditions to ins1 and iew1 - xxlat and xxlon were always>0!   
        xxlat=float(la1)
        if(ins1.eq.'N')then
         xxlat=xxlat+ala1/60.
        else
         xxlat=-xxlat-ala1/60.
        endif 
        xxlon=float(lo1)
        if(iew1.eq.'E')then
         xxlon=xxlon+alo1/60.
        else
         if(test(93).gt.0.)then
          xxlon=360.-xxlon-alo1/60
         else
          xxlon=-xxlon-alo1/60.
         endif  
        endif   

c write new solution into data header, data(1)      
c save old header in datsave
c j.h. something wrong with lat,lon check
        if(xxlat.gt.90.0.or.xxlat.lt.-90.0) xxlat=90.0
        if(xxlon.gt.360.0.or.xxlon.lt.-360.0) xxlon=360.0

c brl 6/98: remove this - datsave stored in hyp.for
c        datsave=data(1)
c2000        write(data(1)(1:20),'(1x,i4,1x,2i2,1x,a4,f5.1)')iyr+1900,
        write(data(1)(1:20),'(1x,i4,1x,2i2,1x,a4,f5.1)')iyr,

     &   imnth,idy,thrmin,xhmsave
        write(data(1)(24:43),'(f7.3,f8.3,f5.1)')xxlat,
     &  xxlon,zsave   
c j.h.        write(data(1)(24:43),'(f7.3,f8.3,f5.1)')ysave/degtorad,
c j.h.     &  xsave/degtorad,zsave   

c old data gets overwritten: need to replace it
        write(data(1)(44:48),'(a5)')datsave(44:48)
        ndata1=ndata
        if(ndata1.gt.999)ndata1=999
        write(data(1)(49:51),'(i3)')nstation
        rmss1=rms
        if(rmss1.gt.99.9)rmss1=99.9
        write(data(1)(52:55),'(f4.1)')rmss1
        write(data(1)(56:80),'(a25)')datsave(56:80)
        write(data(1)(11:11),'(a1)') datsave(11:11)
        
c 10/94 look for error record
        k=2                                          
        do while (data(k)(80:80).ne.'E'.and.k.le.nhead)
         k=k+1
        enddo         
        if(data(k)(80:80).ne.'E')then

c no error record: move data array below header down 1
         do i=2,nrecord
          data(nrecord+3-i)=data(nrecord+2-i)
          data5(nrecord+3-i)=data5(nrecord+2-i)
         enddo
         nrecord=nrecord+1
         nhead=nhead+1                                

c write errors in new record 2         
         k=2                               
         do i=1,79
          data(k)(i:i)=' '
         enddo 
         data(k)(80:80)='E'
        endif              
        
c write gap on error line   jh oct 98
        write(data(k)(2:8),'(a,i3)')'GAP=',igap
c write out errors into data(k)        
        write(data(k)(15:20),'(f6.2)')oterr
        write(data(k)(25:30),'(f6.1)')ery
        write(data(k)(33:38),'(f6.1)')erx
        write(data(k)(39:43),'(f5.1)')erz
        write(data(k)(44:79),'(3e12.4)')cvxy,cvxz,cvyz
c
c   check if high accuracy output
c
        l=0
        if(high_accuracy) then
           do i=2,nhead           ! find if a line already there
              if(data(i)(80:80).eq.'H') l=i
           enddo
c
c   if no H line, make a new one
c
           if(l.eq.0) then
             do i=nrecord,k+1,-1
                data(i+1)=data(i)
             enddo
             nrecord=nrecord+1
             nhead=nhead+1                                
             l=k+1    ! new record to write in
           endif
c
c  blank H record
c
           do i=1,79
              data(l)(i:i)=' '
           enddo 
           data(l)(80:80)='H'
c
c   write data
c
           data(l)(1:15)=data(1)(1:15)
           write(data(l)(17:22),'(f6.3)') xhmsave
           write(data(l)(24:59),'(f9.5,1x,f10.5,1x,f8.3,1x,f6.3)')
     *     xxlat,xxlon,zsave,rmss1
        endif

c taken out 31/1/2006, lot
        if(extension(1:3).eq.'BGx') then

c Following code added by bjb 14/2/2001 to insert BGS lines 93 and A3
c containing velocity model and starting parameters respectively to
c the readings format output. Lines are all shifted down by one
c before inserting the new line. If line type already exists then 
c it will be overwritten.

c Check if velocity model line already exists
         k=3
         do while (data(k)(79:80).ne.'93'.and.k.le.nhead)
            k=k+1
         enddo         
         
c     if no velocity model record: move data array below header down 1
         if(data(k)(79:80).ne.'93')then
            do i=3,nrecord
               data(nrecord+4-i)=data(nrecord+3-i)
            enddo
            nrecord=nrecord+1
            nhead=nhead+1                                

c write velocity model in new record 93         
            k=3
            do i=1,78
               data(k)(i:i)=' '
            enddo 
            data(k)(79:80)='93'
            do i=1,nl
               j=1+(i-1)*12
               write(data(k)(j:j+11),'(f5.2,1x,f5.2,a1)')v(i),d(i),'/'
            enddo
         else                   ! overwrite existing velocity model
            do i=1,78
               data(k)(i:i)=' '
            enddo 
            do i=1,nl
               j=1+(i-1)*12
               write(data(k)(j:j+11),'(f5.2,1x,f5.2,a1)')v(i),d(i),'/'
            enddo
         endif              
c 
c Next add location parameter line containing start depth,
c xnear, xfar and vpvs
c
c Check for existing line type A3
c
         k=4
         do while (data(k)(79:80).ne.'A3'.and.k.le.nhead)
            k=k+1
         enddo         

c no location parameter record: move data array below header down 1
         if(data(k)(79:80).ne.'A3')then
            do i=4,nrecord
               data(nrecord+5-i)=data(nrecord+4-i)
            enddo
            nrecord=nrecord+1
            nhead=nhead+1                                

c write start depth, xnear, xfar and vpvs to new line type A3
            k=4
            do i=1,78
               data(k)(i:i)=' '
            enddo 
            data(k)(79:80)='A3'
c            write(data(k)(3:5),'(f3.0)') ztr
c            write(data(k)(7:12),'(f6.1)') xnear
c            write(data(k)(14:19),'(f6.1)') xfar
c            write(data(k)(21:24),'(f4.2)') pos
c changed lot 24/5/2005
            write(data(k)(3:5),'(f3.0)') ztr
            write(data(k)(7:10),'(f4.0)') xnear
            write(data(k)(12:15),'(f4.0)') xfar
            write(data(k)(17:20),'(f4.2)') pos
c            return
         else                   ! overwrite existing A3 line
            do i=1,78
               data(k)(i:i)=' '
            enddo 
c            write(data(k)(3:5),'(f3.0)') ztr
c            write(data(k)(7:12),'(f6.1)') xnear
c            write(data(k)(14:19),'(f6.1)') xfar
c            write(data(k)(21:24),'(f4.2)') pos
            write(data(k)(3:5),'(f3.0)') ztr
            write(data(k)(7:10),'(f4.0)') xnear
            write(data(k)(12:15),'(f4.0)') xfar
            write(data(k)(17:20),'(f4.2)') pos
c            return
         endif              
        endif
c 
c add xnear and xfar 
c
c Check for existing 'XNEAR' line type
c
        if (test(107).eq.1..and.distind.eq.'L') then
         if (xxnear.ne.-1..and.xxfar.ne.-1.) then
c          k=3  why 3??? changed to 2, lo 06052010
          k=2
          do while (data(k)(2:6).ne.'XNEAR'.and.k.le.nhead)
             k=k+1
          enddo         
c no location parameter record: move data array below header down 1
          if(data(k)(80:80).ne.'3'.and.data(k)(2:6).ne.'XNEAR')then
            do i=3,nrecord
               data(nrecord+4-i)=data(nrecord+3-i)
            enddo
            nrecord=nrecord+1
            nhead=nhead+1                                

c write xnear, xfar to new line 
            k=3
            data(k)(1:80)=
     &  ' XNEAR        XFAR        SDEP          '//
     &  '                                       3'
            write(data(k)(8:13),'(f6.1)') xnear
            write(data(k)(20:25),'(f6.1)') xfar
            write(data(k)(32:36),'(f5.1)') ztr
          else                   ! overwrite existing line
            write(data(k)(8:13),'(f6.1)') xnear
            write(data(k)(20:25),'(f6.1)') xfar
            write(data(k)(32:36),'(f5.1)') ztr
          endif              
         endif
        endif

        
        if(iulst.gt.0.and.(.not.multi_model))write(iulst,959)res
959     format(/,1x,'unweighted rms = ',f8.2)
        if(.not.multi_model)
     *  write(iulst,'(/,''------------------------------------------'',
     &  ''-----------------------------------'',/)')

        return
      endif


c change 10/93: made this section a separate function
      if(ifunct.eq.3)then
          if(ievent.gt.0)rmssum=sqrt(rmssum/float(ievent))
      
        if(iulst.gt.0.and.(.not.multi_model))then
          
c changed 4/94 to calculate & print rms deviations

         if(idiff.gt.0)then
          xnn=float(idiff)
          write(iulst,'(/,'' average differences from previous'',
     &     '' solutions: '',i8,'' events''/)')idiff          
          write(iulst,'(t28,'' mean'',t40,''  rms''/)')
          xmm=xhdiff(4)/xnn
          xme=sqrt(abs(xhd2(4)*xnn-xhdiff(4)**2))/xnn
          write(iulst,'('' origin time: '',t22,f10.1,'' sec'',
     &     t36,f10.1)')xmm,xme
          xmm=xhdiff(1)/xnn
          xme=sqrt(abs(xhd2(1)*xnn-xhdiff(1)**2))/xnn
          write(iulst,'('' longitude: '',t22,f10.1,'' km'',
     &     t36,f10.1)')xmm,xme
          xmm=xhdiff(2)/xnn
          xme=sqrt(abs(xhd2(2)*xnn-xhdiff(2)**2))/xnn
          write(iulst,'('' latitude: '',t22,f10.1,'' km'',
     &     t36,f10.1)')xmm,xme
          xmm=xhdiff(3)/xnn
          xme=sqrt(abs(xhd2(3)*xnn-xhdiff(3)**2))/xnn
          write(iulst,'('' depth: '',t22,f10.1,'' km'',
     &     t36,f10.1)')xmm,xme                       
          write(iulst,*)
         endif 
         
          write(iulst,980)
980       format(/,' average station residuals (weighted):'//,
     &    ' station',5x,'p residual rms dev  no p',5x,'s residual rms',
     &    ' dev  no s')
981       format(1x,a5,9x,2f8.2,i6,7x,2f8.2,i6)

          do i=1,nstat 
            ipf=0
            isf=0
            if(pknt(i).ne.0)then
              pmean=pres(i)/pknt(i)
              parg=pres2(i)-2.*pw2res(i)*pmean+pknt2(i)*pmean**2
              pres2(i)=sqrt(abs(parg)/pknt(i))
              pres(i)=pres(i)/pknt(i)
              ipf=1
            endif
            if(sknt(i).ne.0)then
              smean=sres(i)/sknt(i)
              sarg=sres2(i)-2.*sw2res(i)*smean+sknt2(i)*smean**2
              sres2(i)=sqrt(abs(sarg)/sknt(i))
              sres(i)=sres(i)/sknt(i)
              isf=1
            endif
            if(iulst.gt.0)then
              if(isf.eq.1.and.ipf.eq.1)write(iulst,981)s(i),pres(i),
     &        pres2(i),kntp(i),sres(i),sres2(i),knts(i)
              if(isf.eq.0.and.ipf.eq.1)write(iulst,981)s(i),pres(i),
     &        pres2(i),kntp(i)
982           format(1x,a5,38x,2f8.2,i6)
              if(isf.eq.1.and.ipf.eq.0)write(iulst,982)s(i),sres(i),
     &        sres2(i),knts(i)
            endif
          end do
c         if(ievent.gt.0)rmssum=sqrt(rmssum/float(ievent))
          write(iulst,'(/,'' Mean rms value ('',i4,'' events) = '',
     &    f12.3)')ievent,rmssum
        endif
      endif

      if(ifunct.eq.4)then

c    initialize p and s residual counts and sums
        do i=1,nstat
          kntp(i)=0
          knts(i)=0
          sknt(i)=0.
          pknt(i)=0.
          sres(i)=0.0
          pres(i)=0.0                        
          pres2(i)=0.0
          sres2(i)=0.0 
          pw2res(i)=0.0
          sw2res(i)=0.0
          sknt2(i)=0.0
          pknt2(i)=0.0
        end do

c  rmssum is used to get a mean rms value for all the events,
c  ievent is the total events located
        rmssum=0.0
        ievent=0

c added these to calculate differences from previous solution and rms
        idiff=0
        do i=1,4
         xhdiff(i)=0.0
         xhd2(i)=0.0
        enddo

      endif

      return
      end

      subroutine fold(alat,alon,la,ins,ala,lo,iew,alo)
c
c-------- given geographic coordinates compute geocentric lat and lon
c
c input:  la     degree portion of latitude in degrees
c         ins    n for north, s for south
c         ala    minutes portion of latitude
c         lo     degree portion of longitude
c         iew    e for east, w for west
c         alo    minutes portion of longitude
c output: alat   geocentric latitude in radians
c         alon   longitude in radians
      save
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
      parameter (c1 = (1.0 - flat)**2)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
      character*1 ins, iew
c
c changed 6/98 brl: fix roundoff problem
      alat = (float(la) + ala*1.6666667e-2)*rad
c ggtogc - convert from geographic to geocentric latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 201
         alat = alat/c1-sign(c2,alat)
         goto 202
  201    alat = atan(c1*tan(alat))
  202    continue
      if (ins .eq. 'S')alat = -alat
      alon = (lo + alo*1.6666667e-2)*rad
      if (iew .eq. 'W') alon = -alon
      return
      end
 
      subroutine back (delat, delon, newlat, newlon, slat, slon)
c
c-------- back - calculate geocentric coordinates of secondary point from
c            step in latitude (km) and longitude(km)
c
c input:  delat     change in earthquake latitude in km (northward positive)
c         delon     change in earthquake longitude in km (westward positive)
c output: newlat    new earthquake geocentric latitude in radians
c         newlon    new earthquake longitude in radians
c
      save
      real newlat,newlon
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(slat)
      ct0 = sin(slat)
      call cvrtop(delat,delon,delta,az1)
      if (az1 .lt. 0.0) az1 = az1 + twopi
c use approximation of local radius for derivative of surface
c     distance with geocentric latitude.
c  more accurate formulation would be:
c      drdth = -r**3 * cos(alat)*sin(alat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(slat)**2/equrad**2 + sin(slat)**2/polrad**2)**(-.5)
      sdelt = sin(delta/radius)
      cdelt = cos(delta/radius)
      cz0 = cos(az1)
      ct1 = st0*sdelt*cz0+ct0*cdelt
      call cvrtop(st0*cdelt-ct0*sdelt*cz0, sdelt*sin(az1), st1, dlon)
      newlat = atan2(ct1, st1)
      newlon = slon + dlon
      if (abs(newlon) .gt. pi) newlon = newlon - sign(twopi, newlon)
      return
      end
 
      subroutine cvrtop(x, y, r, theta)
c
c-------- bruce julian
c
c-------- cvrtop - convert from rectangular to polar coordinates
c
c (output - may overlay x, y)
c
c-------- standard fortran funct. required:  atan2
c-------- funct. required:  hypot
c
      r = hypot(x, y)
      theta = 0.
      if ((y .ne. 0.) .or. (x .ne. 0.)) theta = atan2(y, x)
      return
      end
 
      real function hypot(a, b)
c
c-------- bruce julian
c
c
c-------- hypot - calculates euclidian distance, accurately and
c            avoids overflow
c
      real a, b
      real abs, l, s, t, sqrt
      l = abs(a)
      s = abs(b)
      if (s .le. l) goto 1
         t = s
         s = l
         l = t
   1  if (l .ne. 0.0) goto 2
         hypot = 0.0
         return
   2  s = s/l
      hypot = l*sqrt(s*s+1.0)
      return
      end
 
      subroutine delaz(slat, slon, dekm, dedeg, az0, eqlat, eqlon)
c
c-------- delaz - calculate the distance in km (approx equal to geocentric
c      distance times local radius), and azimuths in radians
c
c input:  slat     station geocentric latitude in radians
c         slon     station longitude in radians
c         eqlat    earthquake geocentric latitude in radians
c         eqlon    earthquake longitude in radians
c output: dekm     distance from earthquake to station in kilometers
c         dedeg    distance from earthqauke to station in degrees
c         az0      azimuth from earthquake to station measured clockwise
c                     from north in degrees
c
      save
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(eqlat)
      ct0 = sin(eqlat)
c use approximation of local radius for derivative of surface
c     distance with geocentric latitude.
c  more accurate formulation would be:
c      drdth = -r**3 * cos(slat)*sin(slat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(eqlat)**2/equrad**2 +
     *sin(eqlat)**2/polrad**2)**(-.5)
      ct1 = sin(slat)
      st1 = cos(slat)
      sdlon = sin(eqlon-slon)
      cdlon = cos(eqlon-slon)
      cdelt = st0*st1*cdlon+ct0*ct1
      call cvrtop(st0*ct1-st1*ct0*cdlon, st1*sdlon, sdelt, az0)
      dedeg = atan2(sdelt, cdelt)*deg
      dekm = radius*atan2(sdelt, cdelt)
      if (az0 .lt. 0.0) az0 = az0 + twopi
      if (az0 .ge. twopi) az0 = az0 - twopi
      az0 = 360.-az0*deg
c calculation of back azimuth if needed
c      call cvrtop(st1*ct0 - st0*ct1*cdlon, (-sdlon)*st0, sdelt, az1)
c      if (az1 .lt. 0.0) az1 = az1 + twopi
      return
      end
 
      subroutine unfold(alat,alon,la,ins,ala,lo,iew,alo)
c     unfold2
c
c-------- given geocentric lat and lon compute geographic coordinates
c            suitable  for printing
c
c input and output definition just reverse of entry fold
c
      save
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
      parameter (c1 = (1.0 - flat)**2)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
      character*1 ins, iew
c
c
c convert from geocentric lat and lon to geographic
c
c     alat     geocentric lat (radians)
c     alon                lon (radians)
c     la,ins,ala          lat in deg and min
c     lo,iew,alo          lon in deg and min
c
c gctogg - convert from geocentric to geographic latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 403
         blat = c1*(alat + sign(c2,alat))
         goto 404
  403    blat = atan(tan(alat)/c1)
  404 continue
      ins = 'N'
      iew = 'E'
      ala1 = blat*deg
      la = ala1
      ala = abs(ala1 - la)*60.
      la = iabs(la)
      ins='N'
      if (ala1 .lt. 0.0) ins = 'S'
      alo1 = alon*deg
      lo = alo1
      alo = abs(alo1 - lo)*60.
      lo = iabs(lo)
      if (alo1 .lt. 0.0) iew = 'W'
      return
      end

      subroutine settest(test)
      dimension test(200)

c set default test parameters - see hypo71 documentation

c  any test values not set below are not used by hypocenter

c  test(2) = step length damping control. to enable this, make it smaller.
c  the damping factor,xk, is increased if the parameter change is > test(2) km.

      test(2)=500.0

c  test(7)-test(9) = duration magnitude coefficients (fmag) - see hypo71 manual
 
      test(7)=-0.87
      test(8)=2.00
      test(9)=0.0035

c  test(11) = maximum no of iterations

      test(11)=99.0

c  test(13) = increment in km at which auxiliary rms values are calculated

      test(13)=10.0

c  additional test values used by hypocenter

c     test(30) = initial value of damping factor, xk

      test(30)=0.005

c     test(31) = max degs freedom: set to 2 for fixed depth solution at header
c                value, -2 at STATION0.HYP value, 1 to fix epicenter at header
c                value, 0 to fix origin time, epicenter & depth at header values
c                 (alternatively set ires=1 as described below)

      test(31)=3.0

c     test(32) = magnitude of parameter changes (km) below which convergence is
c                assumed - increased by a factor of 10 initially

      test(32)=0.05

c     test(34) = minimum residual (sec) for normalizing residuals in
c                bisquare weighting 

      test(34)=0.1

c     test(35) = c value for bisquare weighting

      test(35)=4.685

c     test(36) = rms value below which bisquare residual weighting is applied
c                to residuals for local events - set to 1 to enable this weighting

      test(36)=0.0

c     test(37) = max no of increases in xk before fixing depth

      test(37)=10.

c change 9/94: changed default to 0.0 (least squares)
c     test(38) = 0.0 for least squares errors, =anything else, damped least
c                squares errors with xke error value (see below)  

      test(38)=0.0

c     test(39) = factor by which damping, xk, is increased for rms increases

      test(39)=4.

c change 10/93: changed old test(40) (no longer used) to option for
c setting the depth origin

c     test(40)=0.0 specifies that the depth origin is sea-level, with
c              layer 1 of the velocity model extended up to the maximum
c              elevation station. The depths of all located events are
c              then limited to sea-level.
c              Setting test(40)=1.0 specifies the maximum elevation
c              station in the station list (NOT the phase list) is
c              used as zero depth for both the event depth and the
c              velocity model (this was the old default)

      test(40)=0.0

c     test(41) = maximum distance from nearest station at which hypocentral
c                solutions will be generated

      test(41)=20000.

c     test(43) = maximum rms for residuals to be used in average 
c                station residual calculation.
c                 - doesn't affect hypocenter calculation

c                default vaue changed to 15.0 4/94
      test(43)=15.0

c     test(44)=3.0 Rayleigh wave (R phase) velocity in km/sec

      test(44)=3.0
 
c     test(45) (added 5/94) location differences only accumulated if rms<test(45)
      test(45)=50.0

c     test(46) (added 5/94) location differences only accumulated
c              if nmber of non-zero weight phases is > test(46)
      test(46)=3.0

c     test(47) (added 11/95) set to 1 to enable checking to prevent
c      depth going below moho or conrad when b or n phases present
      test(47)=0.0  
      
C     test(49) T phase velocity in km/sec (added 6/98)
      test(49)=1.484      

c     test(50) = flag for using azimuth phases. 0 disables

      test(50)=1.0

c     test(51) = L phase velocity in km/sec

      test(51)=3.5

c     test(52) = azimuthal error used in azimuth inversion  (degrees) 

      test(52)=5.0

c     test(53) = critical distance (km)  which the starting location
c                is moved out to if N phases are specified

      test(53)=130.

c     6/94: changed this to use 0 to disable, 1 to enable
c     test(56) = 1.0 enables starting location estimates from apparent
c                velocity, distance, azimuths, etc.
c                If test(56)<=0.0 epicenter is taken 0.15 km from
c                the first arrival station

      test(56)=1.0

c     test(57) = distance (geocentric km) beyond which iasp91 is used to
c                calculate travel times

      test(57)=1500.

c     test(58) = maximum apparent velocity (km/sec) for phase data to
c                be used

      test(58)=100.0

c     test(59) = critical distance (km) for PKP core phases

      test(59)=13000.

c     test(60) = seconds by which the arrival time difference between
c     two adjacent stations can exceed the travel time between them
c     Setting this to 0 disables the initial consistency check for
c     bad times in 'hypoloc'

      test(60)=5.0

c     test(61) = multiple of apparent velocity regression residual rms at
c     which arrival times are weighted to zero during start location
c     determination.  Reducing this value will cause arrivals to be
c     rejected when they do not conform to the plane wave set of arrivals
c     which is characteristic of distant events. Unless you are getting
c     a lot of messages ' xxx removed: Apparent velocity deviation =..',
c     in the output, I recommend against changing this default value
c     However, you can disable this feature by setting test(61)=0.0

      test(61)=3.0

c     test(62) = 0.0 limits the IASP91 phase list to 'basic', whereas
c     1.0 specfies all phases to be calculated each time. This can
c     speed up the program, particularly on a slow PC

      test(62)=1.0

c     test(63) is a value that specifies the types of phases used
c     by the subroutine trtime to be specified when calculating
c     travel times and their derivatives and resulting residuals
c     and rms.  The default value of 0 means that all 8 letters
c     of the phase ID are matched. However, first arrivals are used
c     before and after the critical distance for N and G phases,
c     respectively. See the trtime listing (hypoloc1) for details.

      test(63)=0.0

c     test(64) and test(65) are parameters added (10/93) to allow 
c     temporary increases in rms during the iterations. If 
c     rms < test(64)*rms1, where rms1 is the minimum value, 
c     the parameter step will be allowed for test(65) iterations. 
c     I strongly recommend against changing these parameters 
c     from their present defaults. To disable any increases in 
c     rms at all, set test(64)=0.0. This was how HYPOCENTER 
c     originally worked, but expererience with deep events has 
c     indicated that secondary minima can exist at shallow depth
c     in some situations. A factor of 2 increase in rms for a single
c     step was found to be sufficient to allow movement to the
c     lower rms at a greater depth.

      test(64)=2.0
      test(65)=3.0

c     setting test(66) to a non-zero value enables a printout
c     of the dtdx2 layered model travel time calculation errors
c     (critical distances exceeded, etc)

      test(66)=0.0

c     setting test(67) to a value other than zero forces
c     blank phases to be set to P. The default is to ignore
c     blank phases

      test(67)=0.0

c     test(68) is the P-velocity used in depth-phase depth determination

      test(68)=5.0      
      
c     test(69) is the distance (deg) at which PKiKP or PKP are used as the first
c     arrival instead of Pdiff

      test(69)=110.
      
c     test(70) is the maximum depth (km) that the hypocenter is allowed to move to

      test(70)=700. 
      
c     test(71) determines if the nordic output file is sorted by distance. Set to 0.0
c     to disable this sorting

      test(71)=1.0      
      
c     test(72) determines if an IASP91 phases is matched to the input phase ID 
c     (test(72)=0.0) or if the closest IASP91 phase to the observed travel time
c     is used (test(72)>0.0)

      test(72)=0.0
      
c     test(73) is the number of iterations for which hypoloc will attempt 
c     to match phase ID's when test(72)>0      

      test(73)=3.0
      
c     test(74)=1.0 enables the printout of the input phase data in PRINT.OUT

      test(74)=0.0
c  
c     test(75-78) Ml parameters, california standards, hutton and boore
      test(75)=1.0
      test(76)=1.11
      test(77)=0.00189
      test(78)=-2.09
c
c     test(79) is number of stations required to make a solution
      test(79)=1.0
c
c     test(80) is number of phases required to make a solution
      test(80)=2.0
c
c     test(81)=1.0 enables use of local events, 0.0 : do not use local events
      test(81)=1.0
c
c     test(82)=1.0 enables use of regional events, 0.0 : do not use regional events
      test(82)=1.0
c
c     test(83)=1.0 enables use of distant events, 0.0 : do not use distant events
      test(83)=1.0  
      
c     test(84)=1.0 enables ellipticity corrections for IASP91 travel times
      test(84)=1.0    
      
c     error ellipse parameters for local events test(85)-(86) added 9/94
c     test(85)=0.1 assumed error in arrival times for a weight of 1
c                  for local events
      test(85)=0.1
      
c     test(86)=8.0 degrees of freedom in test(85) for local events
      test(86)=8.0
      
c     test(87)=0.1 confidence level that solution will lie outside
c                   the confidence ellipsoid defined by the covariance
c                   matrix (default is 90%  confidence)
      test(87)=0.1                         
      
c     test(88)=10000. rms value (sec) at which residual weighting for
c                     distant events - set to zero to disable weighting
      test(88)=10.      

c     test(89)=1.0    enable use of depth phases in solution - 0 disables
      test(89)=1.0

c     test(90)=1.0    enable use of core phases (PKP, SKP, etc)
      test(90)=1.0  
      
c     test(91)=1.0    a priori error in distant event times
      test(91)=1.0

c     test(92)=8.0    degrees of freedom in test(91)for distant events
      test(92)=8.0                            
      
c     test(93)=0.0    setting test(93)=1 outputs longitudes between 0 and 360
c                     the default is -180<longitude<+180
      test(93)=0.0      
c
c     test(94)=0.0    value of residual below which phases with weight 4 is used
c                     again
      test(94)=0.0

c     test(95)=1.0    setting this to 1.0 disables core phases between 135 & 150 deg          
      test(95)=0.0
      
c     test(96)=0.0    set to 1 to enable variation of depth to find minimum rms
      test(96)=0.0       

c     test(97)=0.0     set to 1 to enable minute error correction
      test(97)=0.0
      
c     test(98)=0.0     set to 1 to enable spherical harmonic station corrections
      test(98)=0.0    
      
c     test(99) L phase weight: multiplied by phase weight. (added 6/98)
      test(99)=1.0
              
c     test(100) R phase weight: multiplied by phase weight. 
      test(100)=1.0
              
c     test(101) T phase weight: multiplied by phase weight. 
      test(101)=0.0

c     test(103) Min number of depth phases for starting depth (added 6/98)
      test(103)=1.0
      
c     test(104) min distance of epicenter from array for distant events            
      test(104)=30.0                   

c     test(105) Set this to 1.0 to enable the use of the 1D gradient model
      test(105)=0.0

c     test(106) Set this to 1.0 to only calculate magnitudes
      test(106)=0.0

c     test(107) Set this to 1.0 to take xnear/xfar from sfile
      test(107)=0.0
c 
c     test(108) set this to 0.0 to use richter mb att, 1.0 to use veith clavson
      test(108)=0.0

c     test(109) in reject mode, if residual is larger than this, it is weighted
c               out. default is 0.8
      test(109)=0.8

c     test(110) in reject mode, in final run, phases with residual larger
c               this and automatic, are removed. default 3.0 
      test(110)=3.0

c     test(111) in reject mode, if several phases from same stations, only 
c               keep the one with lowest residual if value is 1.0.
c               default 1.0 
      test(111)=1.0     

c     test(112) in reject mode,set this to 1.0 enable testing for Sg when 
c     rejecting phases, default 0.0
      test(112)=0.0
c
c     test(113) is the lower distance limit for calculating mb, default
c               is 20 deg
      test(113)=20.0*111.2
c
c     test(114) is the lower distance limit for calculating Ms, default
c               is 20 deg
      test(114)=20.0*111.2
c
c     test(115) is the max depth to use when calculating Ms or MS,
c               default is 60 km
      test(115)=60.0
c
c     test(116) decide if using average or median for magnitudes
c               default is average 1.0
      test(116)=0.0 
c
c     test(117) set to 1. to use mbn magnitude calculation
      test(117)=0.
            
      return
      end

      subroutine getphase(nphase,st,phase,pr1,pr,pr3,ip,tp,fmp,kmin,
     &init,iuphs,iulst,narriv)
   
c      get a set of phases - hypo71 version (S and P on same record)
c      init=-1 means EOF

      save
      character*8 phase(*)
      character*5 st(*),statid
      character*1 prmk(3),srmk(3),pr1(*),pr(*),pr3(*),ucase
      double precision tp(*),tsec
      dimension ip(*),fmp(*)
      ks=0

10      read(iuphs,104,end=99)statid(1:4),(prmk(i),i=1,3),ipp,iyear,
     &  imonth,
     &  iday,khr,kmin,tpp,ts,(srmk(i),i=1,3),is,fmpp
        statid(5:5)=' '
        if(iulst.gt.0.and.statid.ne.'     ')then
          write(iulst,104)statid(1:4),(prmk(i),i=1,3),ipp,iyear,
     *    imonth,iday,
     &    khr,kmin,tpp,ts,(srmk(i),i=1,3),is,fmpp
104       format(a4,3a1,i1,1x,5i2,f5.2,7x,f5.2,3a1,i1,31x,f5.0)
        endif

c   blank record  - end of phase set, exit

        if(statid.eq.'     ')then
          if(ks.eq.0)go to 10
          nphase=ks
          init=2
          return
        endif

        if(ucase(prmk(2)).eq.'P')then
          ks=ks+1
          phase(ks)='P   '
          st(ks)=statid
          ip(ks)=ipp

c change 11/93: day 0 is day 1 in hypo71 format
          if(iday.eq.0)iday=1

          call timsec(iyear,imonth,iday,khr,kmin,tpp,tsec)
          tp(ks)=tsec
          fmp(ks)=fmpp
          pr1(ks)=prmk(1)
          pr(ks)=ucase(prmk(2))
          pr3(ks)=prmk(3)
        endif

        if(ucase(srmk(2)).eq.'S')then

          ks=ks+1
          if(ks.gt.narriv)then
           write(*,*)'Maximum arrivals exceeded - change hypparm.inc an'
     &     ,'d recompile sources'
           stop
          endif  
          phase(ks)='S   '
          st(ks)=statid
          ip(ks)=is
          call timsec(iyear,imonth,iday,khr,kmin,ts,tsec)
          tp(ks)=tsec
          fmp(ks)=0.0
          pr1(ks)=srmk(1)
          pr(ks)=ucase(srmk(2))
          pr3(ks)=srmk(3)

       endif

      go to 10

99    init=-1
      nphase=ks
      return
      end

      subroutine stations(statid,la,alat,ins,lo,alon,iew,ielv,dly,fmg,
     &scrst,nstat,iunit1,iunit2,init,nstats,mag_cor,use_eev)   !mag_corr,scrst added
      
      save
      character*1 ins,iew,ucase
      character*5 statid
c      character*80 text ! changed to 120 wc
      character*120 text
      logical use_eev
      dimension statid(*),la(*),alat(*),lo(*),alon(*),ielv(*),
     &dly(*),fmg(*),ins(*),iew(*)
      real mag_cor(5,nstats),scrst(nstats,9) ! mag_corr, spher. harm. stat. corr.

c   read station coordinates, elevations, delays, etc.

c   hypo71 version

c   statid:       station identifier (5 letters max.)
C   la:           degree portion of station's geographic latitude
c   alat:         minute  "                      "
c   ins:          'S' for south, 'N' for north
C   lo:           degree portion of station's geographic longitude
c   alon:         minute  "                      "
c   iew:          'E' for east, 'W' for west
c   ielv:         station elevation in meters (see note below)
c   dly:          station delay in seconds

c   as mentioned in the b.s.s.a. paper, we found it useful to include
c   station elevations in the calculation, as even small station elevation
c   differences dramatically stabilize solutions at close to zero depth.
C
c   ielv(i) are the station elevations in meters above a suitable datum
c   (e.g. sea level) - they must be positive up. all hypocentral depths
c   in this program are referenced to the highest stations elevation, which
c   is also the top of the velocity model. this referencing is essential
c   as HYPOCENTER will not allow negative hypocentral depths to occur.
C
      i=1
2     continue
      read(iunit1,'(a)',end=99) text
      if(text(2:2).eq.' ') then   ! 4 char station
         read(text,103,err=8844)statid(i)(1:4),la(i),alat(i),
     *   ins(i),lo(i),
     &   alon(i),iew(i),ielv(i),dly(i),(mag_cor(k,i),k=1,5) !mag_corr added
     &,   (scrst(i,j),j=1,9)                                !scrst added
         statid(i)(5:5)=' '
      else    ! 5 char station
         read(text,1103,err=8844)statid(i),la(i),alat(i)
     *   ,ins(i),lo(i),
     &   alon(i),iew(i),ielv(i),dly(i),(mag_cor(k,i),k=1,5) !mag_corr added
     &,   (scrst(i,j),j=1,9)                                !scrst added
      endif
      goto 8845
 8844 continue
      write(6,*)' Something wrong with station line in station file'
      write(6,'(a)') text
      stop
 8845 continue
103   format(2x,a4,i2,f5.3,a1,i3,f5.3,a1,i4,f6.2,5f5.2,9f6.2)  ! 4 char station
1103  format(1x,a5,i2,f5.3,a1,i3,f5.3,a1,i4,f6.2,5f5.2,9f6.2)  ! 5 char station
c
c   check if elevation is negtive at more than 1000 m
c
      if(text(1:1).eq.'-') ielv(i)=-ielv(i)
c
      fmg(i)=mag_cor(1,i)    !cmag
      if(statid(i).eq.'     ')go to 4
c change 10/93: ins changed to ins(i) in this line-was incorrectly
c printing 'N' & 'S' station ID's even though they had been read in
c correctly

c change 12/27: removed station printout
      if(iunit2.gt.0.and.(.not.use_eev))
     *write(iunit2,1203)statid(i),la(i),alat(i),ins(i),
     &lo(i),alon(i),iew(i),ielv(i),dly(i),fmg(i)
1203  format(1x,a5,i2,f6.3,a1,i3,f6.3,a1,i5,f6.2,5f5.2,9f6.2)  ! 5 char station

      ins(i)=ucase(ins(i))
      iew(i)=ucase(iew(i))
      i=i+1
      if(i.le.nstats)go to 2
      write(*,'('' Limit of '',i6,'' stations exceeded - edit'',
     &'' hypparm.inc and recompile sources'')')nstats 
      stop
4     nstat=i-1
      if(iunit2.gt.0.and.(.not.use_eev))write(iunit2,103)                                  
      init=0
      return
99    init=-1
      nstat=i-1
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      character*1 function ucase(a)

c    convert lower case single char to upper case - machine independent

      character*1 chr(26),ucchr(26),a
      data chr/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     &'n','o','p','q','r','s','t','u','v','w','x','y','z'/
      data ucchr/'A','B','C','D','E','F','G','H','I','J','K','L','M',
     &'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      do i=1,26
        if(a.eq.chr(i))then
          ucase=ucchr(i)
          return
        endif
      end do  
      ucase=a
      return
      end

      subroutine azcros(alat1,alon1,aza,alat2,alon2,azb, 
     *  dista,distb,alat,alon,ierr)                     

c Given the locations of two reference points and two azimuths of
c  great circles passing through those points, the
c  program computes the location of the crossing of two great circles
c   and the distances from two reference points. 
c   -- by Steve Bratt and Donna Williams, June 1986. 
                                                                               
c     modified 6/11/92 by B. Lienert
c   to calculate intersections on far side of globe
                                                                               
c  INPUT: 
c  alat1,alon1,alat2,alon2:  locations of reference points 
c  aza,azb: azimuths of great circles passing through points points
c           1 and 2, respectively                                  
c  OUTPUT:                                                         
c  dista,distb:  distance from points 1 and 2 to point             
c                great circles cross                               
c  alat,alon:  location of crossing point.                         
                                                                               
c  All distances, azimuths, and locations are input and output     
c    in degrees.                                                   
                                                                               
c  ierr : = 0 all O.K., = 1 lines do not cross within reasonable   
c         distance.                                                
                                                                               
c  SUBROUTINES CALLED:                                             
c    distaz                                                        
c    latlon, which calls cart
c                        rotate
c                        geog 
      save 
                                                                               
      degtrad=2.0*asin(1.0)/180.0   !changed 6/98 brl

c
c Find azimuth, back azimuth and radial distance between
c  stations.
c
      call distaz(alat1,alon1,alat2,alon2,delta,azi,baz)
c
c Find sign (clockwise = +) and value of angle measured from line
c   between two stations to aza and azb.
c
      ra = aza - azi
      if(abs(ra).gt.180.) ra = -sign((360.-abs(ra)),ra)
      rb = azb - baz
      if(abs(rb).gt.180.) rb = -sign((360.-abs(rb)),rb)
c
c If the signs of ra and rb are the same, the great circles along
c  those azimuths will not cross within a "reasonable" distance. 
c
      if(sign(1.,ra).eq.sign(1.,rb)) then
       ierr = 1 
       return   
      endif  
c            
      ra = abs(ra)
      rb = abs(rb)

      npode=0
      if((ra+rb).gt.180.) then
        npode=1
       
c added 6/11792 BRL
c this is the far sided case: (npode=1)
c reverse the sense of the two azimuths (i.e., take the triangle
c on the opposite side of the line joining the two stations)

        ra=180.-ra
        rb=180.-rb
      endif 
c                       
      ra = ra*degtrad   
      rb = rb*degtrad   
      rc = delta*degtrad
c                      
      c1=tan(0.5*rc) 
      c2=(ra-rb)*0.5 
      c3=(ra+rb)*0.5 
c                      
c          equations for solving for the distances
c                                                 
      f=(c1)*sin(c2)                            
      g=sin(c3)                                 
      h=cos(c2)*(c1)                            
      e=cos(c3)                                 
c                                                 
      c4=atan(f/g) 
      c5=atan(h/e) 
c                    
c       Compute distances (lengths of the triangle)
c                                                  
      distb=(c4+c5)/degtrad                      
      dista=(c5-c4)/degtrad                      
c                                                  
      if(dista.lt.0.0.or.distb.lt.0.0) then          
        ierr = 1                                     
        return                                       
      endif                                      
c                                                  
      if(dista.lt.distb) then                        
       dist = abs(dista)
       az = aza                                     
       alatin = alat1                               
       alonin = alon1                               
      else
       dist = abs(distb)
       az = azb                                     
       alatin = alat2                               
       alonin = alon2
      endif

c addition 6/11/92 BRL
c  antipodal case: 180 - (angular distance)

      if(npode.eq.1)then
       dist=180.-dist
       dista=180.-dista
       distb=180.-distb
      endif

      call latlon(alatin,alonin,dist,az,alat,alon)
      ierr = 0                                        
      return                                          
      end                                             

      subroutine distaz (alat1,alon1,alat2,alon2, delta,azi,baz)
c                                                               
c Calculate angular distance, azimuth and backazimuth between two
c  points on a sphere.                                           
c                                                                
c Input                                                          
c                                                                
c   ALAT1,ALON1  =  latitude and longitude of point 1.           
c   ALAT2,ALON2  =  latitude and longitude of point 2.           
c                                                                
c Output                                                         
c                                                                
c   DELTA  =  angular distance between points 1 and 2.           
c   AZI    =  azimuth from north of point 2 w.r.t. point 1.      
c   BAZ    =  azimuth from north of point 1 w.r.t. point 2.      
c                                                                
c All arguments are in degrees.                                  
c Latitude, longitude and DELTA are geocentric.                  
c Latitude is zero at equator and positive north.                
c Longitude is positive toward the east.                         
c AZI and BAZ are positive and measured clockwise from local north. 
c
c      implicit double precision (a-h,o-z)
c     real alat1,alat2,alon1,alon2,delta,azi,baz 
      save
      pi=3.14159265
      rtod=180./pi
      dtor=pi/180.
c                                                
      rlat1 = dtor*alat1                         
      rlat2 = dtor*alat2                         
      rdlon = dtor*(alon2-alon1)                 
c                                                
      clat1 = cos(rlat1)                                                       
      clat2 = cos(rlat2)                                                       
      slat1 = sin(rlat1)                                                       
      slat2 = sin(rlat2)                                                       
      cdlon = cos(rdlon)                                                       
      sdlon = sin(rdlon)                                                       
c                                                
      cdel = slat1 * slat2 + clat1 * clat2 * cdlon
      cdel = min(cdel,1.0)                                                     
      cdel = max(cdel,-1.0)                                                    
      yazi = sdlon * clat2                        
      xazi = clat1 * slat2 - slat1 * clat2 * cdlon
      ybaz = -sdlon * clat1                       
      xbaz = clat2 * slat1 - slat2 * clat1 * cdlon
c                                                 
      delta = rtod * acos(cdel)                                                
c     write(16,*)'delta ',delta
c      write(*,*)' xazi yazi ',xazi,yazi
      if(yazi.ne.0.0.or.xazi.ne.0.0)then
        azi = rtod * atan2(yazi,xazi)             
        baz = rtod * atan2(ybaz,xbaz) 
      else
        azi=0.0
        baz=0.0
      endif                                           
      if (azi.lt.0.) azi=azi+360.                 
      if (baz.lt.0.) baz=baz+360.                 
c                                                 
      return                                      
      end                                         
      subroutine cart (alat,alon,z,radius, x)     
c                                                 
c Convert a geographical location to geocentric cartesian coordinates,
c  assuming a spherical earth.                                        
c                                                                     
c The cartesian axis are such that                                    
c     - Axis 1 intersects equator at 90 deg longitude (east)          
c     - Axis 2 intersects north pole                                  
c     - Axis 3 intersects equator at  0 deg longitude                 
c                                                                     
c Input                                                               
c                                                                     
c   ALAT  =  latitude (degrees)                                       
c   ALON  =  longitude (degrees)                                      
c   Z     =  depth                                                    
c   RADIUS  =  radius of the earth                                    
c                                                                     
c Output                                                              
c                                                                     
c   X(1:3)  =  vector of geocentric cartesian coordinates             
c              axis 1 intersects equator at  0 deg longitude          
c              axis 2 intersects equator at 90 deg longitude          
c              axis 3 intersects north pole 
c                                           
      parameter (dtor = 1./57.2957795)      
      dimension x(3)                        
c                                           
      r123 = radius - z                     
      r13 = r123*cos(dtor*alat)             
      x(1) = r13*sin(dtor*alon)            
      x(2) = r123*sin(dtor*alat)           
      x(3) = r13*cos(dtor*alon)            
c                                          
      return                               
      end                                  

      subroutine geog (x,radius, alat,alon,z)
c                                            
c Convert geocentric cartesian coordinates to a geographical location,
c  assuming a spherical earth.                                        
c                                                                     
c The cartesian axis are such that     
c     - Axis 1 intersects equator at 90 deg longitude (east)
c     - Axis 2 intersects north pole                        
c     - Axis 3 intersects equator at  0 deg longitude       
c                                                           
c Input                                                     
c                                                           
c   X(1:3)  =  vector of geocentric cartesian coordinates   
c   RADIUS  =  radius of the earth                          
c                                                           
c Output                                                    
c                                                           
c   ALAT  =  latitude (degrees)                             
c   ALON  =  longitude (degrees)                            
c   Z     =  depth                                          
c                                                           
      parameter (rtod = 57.2957795)                         
      dimension x(3)                                        
c                                                           
      r13sq = x(3)*x(3) + x(1)*x(1)                             
      r13 = sqrt(r13sq)                                     
      r123 = sqrt(r13sq + x(2)*x(2))                          
      alon = rtod * atan2 (x(1),x(3))                       
      alat = rtod * atan2 (x(2),r13)                        
      z = radius - r123                                     
c                                                           
      return                                                
      end                                                   
      subroutine latlon (alat1,alon1,delta,azi, alat2,alon2)
c                                                           
c Find a point on a sphere which is a given distance and azimuth
c  away from another point.                                
c                                                          
c Input                                                    
c                                                          
c   ALAT1,ALON1  =  latitude and longitude of point 1.     
c   DELTA  =  angular distance between points 1 and 2.     
c   AZI    =  azimuth from north of point 2 w.r.t. point 1.
c                                                          
c Output                                                   
c                                                          
c   ALAT2,ALON2  =  latitude and longitude of point 2.     
c                                                          
c Subroutines called                                       
c                                                          
c   cart                                                   
c   rotate                                                 
c   geog                                                   
c                                                          
c All arguments are in degrees.                            
c Latitude, longitude and DELTA are geocentric.            
c Latitude is zero at equator and positive north.          
c Longitude is positive toward the east.                   
c AZI is measured clockwise from local north.              
c                                                          
      dimension x(3)                                       
c                                                          
      call cart (90.0-delta,180.0-azi,0.0,1.0, x)          
      call rotate (90.0-alat1,0.0,x)                       
      call geog (x,1.0, alat2,dlon,z)                      
      alon2 = mod(alon1+dlon+180.0,360.0) - 180.0          
c                                                          
      return                                               
      end                                                  
      subroutine rotate (alat,alon,x)                      
c                                                          
c Rotate a 3-vector represented in cartesian coordinates.  
c                                                          
c The cartesian coordinate system is most easily described 
c  in geographic terms.  The origin is at the earth's center. 
c  The axes are such that                                     
c     - Axis 1 intersects equator at 90 deg longitude (east)  
c     - Axis 2 intersects north pole                          
c     - Axis 3 intersects equator at  0 deg longitude         
c                                                             
c On input, the vector to be rotated has components X(i),i=1,2,3
c  in this system.  This procedure rotates the vector           
c  in the following two steps:                                  
c    1. Rotation by ALON degrees westward, about the 2-axis.    
c    2. Rotation by ALAT degrees southward,about the 1-axis.    
c                                                               
c On output, X contains the coordinates of the rotated vector.  
c                                                               
c Another way to interpret X on output:  as the components      
c  of the original vector in a rotated coordinate system.       
c  Put yourself on the earth's surface at the point having
c  latitude ALAT and longitude ALON.  Call this point P.  
c  This rotated system is also geocentric, in which       
c     - Axis 1 points east                                
c     - Axis 2 points north                               
c     - Axis 3 points up                                  
c  where east, north and up are the directions local to P.
c                                                         
      parameter (rtod = 57.2957795)                       
      dimension x(3)                                      
c                                                         
c Do it                                                   
c                                                         
      alatr = alat/rtod   
      alonr = alon/rtod   
      sinlat = sin(alatr) 
      coslat = cos(alatr) 
      sinlon = sin(alonr) 
      coslon = cos(alonr) 
c                         
      a = x(1) * coslon - x(3) * sinlon
      b = x(2)                         
      c = x(1) * sinlon + x(3) * coslon
      x(1) = a                         
      x(2) = b * coslat - c * sinlat   
      x(3) = b * sinlat + c * coslat   
c                                      
      return                           
      end                              
