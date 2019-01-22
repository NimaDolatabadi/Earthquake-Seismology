      program fpfit

c
c  changes
c
c dec 18 2010 jh: gfortran fix to compile
c jan 25 2011 jh: more gfortran, read statments, see gfortran
c feb 28 2011 pv: renamed function compl to scompl, due to Sun f95 compiler problem 
c feb 18 2014 jh: fix an array bou8nd problem

c
c
c     version 1.4  -  January 13, 1999
c
c     purpose:       Calculate double-couple fault plane solutions from p-wave first motions (see Reasenberg, P.  and
c                    D. Oppenheimer, Fpfit, Fpplot and Fppage: Fortran computer programs for calculating and displaying
c                    earthquake fault-plane solutions, U.S. Geological Survey Open-File Report 85-739).
c
c     required routines: all routines are enclosed
c
c      output:       1. an ascii file of y2k-hypo71 summary cards extended with fault plane solution parameters on logical unit sunit.
c                       this file serves as input to programs "qplot" and "plotpt".
c                    2. an ascii file consisting, for each earthquake, of the y2k-hypo71 extended summary card, followed by p & t axes 
c                       of neighboring solutions (within 90% confidence limits), followed by individual p-phase information,
c                       on logical unit punit. this file serves as input to programs "fpplot" and "fppage".
c                    3. an optional ascii file of the minimized fit function about the best solution on logical unit funit.
c                    4. an ascii file announcing the control file parameters, the presence of multiple mechanisms, errors in the
c                       data, a summary of polarity discrepancies by station and reading quality, and the distribution of strike, 
c                       dip, and rake uncertainties on logical unit eunit.
c
c      authors:      Paul Reasenberg and David Oppenheimer, U.S.G.S. in Menlo Park.  Some of the routines
c                    were adapted from code written by John Lahr, Bruce Julian, and Fred Klein.
c                    Mark Matthews, Stanford University, provided assistance in the error propagation analysis.
c
c      revisions in V1.4:
c		    1.	For Hypoinverse, code supports:
c		  	non-y2k and y2k versions of archive files
c			5-letter stations, 3 letter channel names, and netcode
c		    2.	Code output changed to y2k version of hypo71, which includes century.  
c		  	Assumes that years 67-99 are in 20th century, all other 2-digit years are assumed to be 21st century
c		    3.	Code now includes "dbg" option to that details reason for rejecting phases to "out" file
c
c	-----------------------------------------------------------------------
c	Although this program has been used by the USGS, no warranty, expressed
c	or implied, is made by the USGS or the United States Government as to
c	the accuracy and functioning of the program and related program
c	material nor shall the fact of distribution constitute any such
c	warranty, and no responsibility is assumed by the USGS in connection
c	therewith.
c	-----------------------------------------------------------------------
c	
c	
c	
c      installation notes: Check routines "readfl"  and "input" for VMS/SUN specific code
c
      integer           cunit                           
c                                                       ! logical unit # of input control file
      integer           eunit                           
c                                                       ! logical unit # of output of error messages
      integer           funit                           
c                                                       ! logical unit # of output of fit listing for all strikes, dips
      integer           iunit                           
c                                                       ! logical unit # of hypo71 listing file (input file)
      integer           mxdip                           
c                                                       ! maximum # of dip increments permitted
      integer           mxqual                          
c                                                       ! maximum # of qualities permitted
      integer           mxrake                          
c                                                       ! maximum # of rake increments permitted
      integer           mxslns                          
c                                                       ! maximum # of multiple solutions permitted
      integer           mxstat                          
c                                                       ! maximum # of stations permitted
      integer           mxstrk                          
c                                                       ! maximum # of strike increments permitted
      integer           punit                           
c                                                       ! logical unit # of output of extended summary and ray parameters
                                                        
c                                                       ! for use in plotting focal mech. diagrams with plotfm
      integer           sunit                           
c                                                       ! logical unit # of output of extended summary cards
c
      parameter (cunit = 1, eunit = 8, funit = 9, iunit = 2, mxdip = 19,
     & mxqual = 8, mxrake = 19, mxslns = 20, mxstat = 1200, mxstrk = 19,
     & punit = 3, sunit = 4)
c
      real              aerrc                           
c                                                       ! allowable angular difference between complimentary planes in coarse search
      real              aerrf                           
c                                                       ! allowable angular difference between complimentary planes in fine search
      real              ain(mxstat)                     
c                                                       ! ray incidence angles in degrees
      real              ainmax                          
c                                                       ! maximum permitted angle of incidence
      real              ainmin                          
c                                                       ! minimum permitted angle of incidence
      real              ainr                            
c                                                       ! ain converted to radians
      real              az(mxstat)                      
c                                                       ! ray azimuth angles (corresponding to ain)
      real              azr                             
c                                                       ! az converted to radians
      character*1       bdflag                          
c                                                       ! signals polarity discrepancy with best fit solution
      logical           best                            
c                                                       ! flag: true=best solution for event
      real              bot(mxdip,mxstrk,mxrake)        
c                                                       ! sum of product of observed and predicted weights
      character*2       cflag1                          
c                                                       ! signals minimum at edge of fine search grid
      character*1       cflag2                          
c                                                       ! signals minimum at edge of fine search grid
      real              coef(mxstat,6)                  
c                                                       ! coeficients by which moment tensor multiplied to give p radiation pattern
      logical           scompl                           
c                                                       ! function
      real              da1                             
c                                                       ! dip angle of principle solution
      real              da2                             
c                                                       ! dip angle of auxilliary solution
      logical           dbg
c                                                       ! true(false) = do (not) issue warning messages
      real              dd1                             
c                                                       ! dip direction of principle solution
      real              dd2                             
c                                                       ! dip direction of auxilliary solution
      real              ddelc                           
c                                                       ! fault dip increment in degrees for coarse search
      real              ddelf                           
c                                                       ! fault dip increment in degrees for fine search
      real              del(mxdip)                      
c                                                       ! fault dip angle in degrees
      real              delc(mxdip)                     
c                                                       ! fault dip angle in degrees for coarse search
      real              delmax                          
c                                                       ! maximum dip range for solutions with fit<fitlim
      real              delmin                          
c                                                       ! minimum dip range for solutions with fit<fitlim
      real              del0c                           
c                                                       ! initial fault dip angle in degrees for coarse search
      real              del0f                           
c                                                       ! initial fault dip angle in degrees for fine search
      real              del1f                           
c                                                       ! terminating fault dip angle in degrees for fine search
      real              dip                             
c                                                       ! dip angle of best solution
      real              dist(mxstat)                    
c                                                       ! epicentral distance
      real              distmx                          
c                                                       ! maximum permitted epicentral distance
      real              dlamc                           
c                                                       ! fault rake increment in degrees for coarse search
      real              dlamf                           
c                                                       ! fault rake increment in degrees for fine search
      real              dphic                           
c                                                       ! fault strike increment in degrees for coarse search
      real              dphif                           
c                                                       ! fault strike increment in degrees for fine search
      real              eps
c                                                       ! machine precision
      real              epsp1
c                                                       ! machine precision plus 1
      real              erate(mxqual)                   
c                                                       ! assumed weighted error rates for each data class
      character*82      event                           
c                                                       ! summary card
      character*59      evfit                           
c                                                       ! dummy character string to hold fit values on output
      integer           evntid                           
c                                                       ! event id #
      character*50      filnam
c							! file name                          
      logical           first                           
c                                                       ! flag: true=first time into subroutine search
      real              fit90                           
c                                                       ! 90 % confidence limit of fit in fine search
      real              fit(mxdip,mxstrk,mxrake)        
c                                                       ! solution fit; weighted measure of agreement between obs, pred polarities
      real              fitlim                          
c                                                       ! 90% confidence upper limit of fitmnf, fitmnc
      real              fitmnc                          
c                                                       ! fit of best solution corresponding to fit(j1,n1,m1) of coarse search
      real              fitmnf                          
c                                                       ! fit of best solution corresponding to fit(j1,n1,m1) of fine search
      character*1       flag(mxdip,mxstrk,mxrake)       
c                                                       ! output string: if fit<fitlim then '*', otherwise blank
      character*1       flgc(mxslns)                    
c                                                       ! alpha equivalent of distinct solution #
      real              fmagmn                          
c                                                       ! minimum permitted magnitude
      real              gfit(mxdip*mxstrk*mxrake)       
c                                                       ! fits of "good" solutions found in coarse search
      character*1       hndsrc(mxstat)
c                                                       ! allowable hand-timed source codes
      integer           i                               
c                                                       ! loop index
      integer           iamwt                           
c                                                       ! code specifying type of amplitude weighting to use
      integer           ibst                            
c                                                       ! flag: 0(1)=do(not) calculate multiple solutions 
      integer           icmp                            
c                                                       ! flag: 1(0)=do (not) composite data into one mechanism
      integer           id                              
c                                                       ! loop index over ndst
      integer           idate(mxstat,2)                 
c                                                       ! date range of station reversal; 0=>open-ended
      integer           idip1                           
c                                                       ! dip angle of best fit
      integer           idst(mxslns,5)                  
c                                                       ! 1-3=indices of distinct solutions; 4=grid edge flag; 5=skip flag
      integer           idpdr1                          
c                                                       ! dip direction of best fit
      integer           idrng                           
c                                                       ! half-width dip range variation for solutions with fit<fitlim
      integer           ievp                            
c                                                       ! number of events processed
      integer           ievr                            
c                                                       ! number of events read
      integer           ifin                            
c                                                       ! flag: 1(0)=do (not) limit fine search to coarse search range
      integer           ifit(mxstrk)                    
c                                                       ! integer conversion of fit*100 for printer output
      integer           ig                              
c                                                       ! loop index over number of "good" solutions
      integer           igood(mxdip*mxstrk*mxrake,4)    
c                                                       ! array of indices of "good" solutions found in coarse search
      integer           ind(mxstat)                     
c                                                       ! pointer array to sorted order
      integer           index                           
c                                                       ! bin index into ndrng,nsrng,nrrng,nfit
      integer           indxa                           
c                                                       ! index of nearest p-,t-axis azimuth 
      integer           indxp                           
c                                                       ! index of nearest p-, t-axis plunge 
      integer           infmt                           
c                                                       ! input file format : 1=hypo71, 2=hypoinverse, 3=hypoellipse
                                                        
c                                                       !                     4=hypoinverse with shadow card
      integer           ipaxes(73,19)                   
c                                                       ! distinct coarse soltn # of p-axes 90% conf region (azm, plng in 5 deg inc)
      integer           ipwt                            
c                                                       ! weight assigned to p arrival
      integer           irep                            
c                                                       ! flag: 1(0)=do(not) report each fps to terminal when computed 
      integer           irpcnt                          
c                                                       ! counter for header output to terminal
      integer           ires                            
c                                                       ! flag: 0(1)=(un)restricted search
      integer           irrng                           
c                                                       ! half-width rake range variation for solutions with fit<fitlim
      integer           irslam                          
c                                                       ! flag: (0)1=(no) restricted coarse search range for rake angle 
      integer           isrc
c                                                       ! loop index
      integer           islip1                          
c                                                       ! rake of best fit
      integer           isrng                           
c                                                       ! half-width strike range variation for solutions with fit<fitlim
      integer           itaxes(73,19)                   
c                                                       ! distinct coarse soltn # of t-axes 90% conf region (azm, plng in 5 deg inc)
      integer           ittl                            
c                                                       ! title option
      integer           j                               
c                                                       ! loop index over dip
      integer           j1                              
c                                                       ! dip index of best solution
      integer           jmaxc                           
c                                                       ! largest dip index of coarse solution with fit<=fitlim about (j1,n1,m1)
      integer           jmaxf                           
c                                                       ! largest dip index of fine solution with fit<=fitlim about (j1,n1,m1)
      integer           jminc                           
c                                                       ! smallest dip index of coarse solution with fit<=fitlim about (j1,n1,m1)
      integer           jminf                           
c                                                       ! smallest dip index of fine solution with fit<=fitlim about (j1,n1,m1)
      integer           k                               
c                                                       ! loop index
      character*2       kilnet(mxstat)
c                                                       ! seismic network code for kilsta
      character*5       kilsta(mxstat)                  
c                                                       ! ignored station names
      integer           l                               
c                                                       ! loop index over moment tensor
      logical           lopen2                          
c                                                       ! t if sunit open
      logical           lopen3                          
c                                                       ! t if punit open
      logical           lopen4                          
c                                                       ! t if funit open
      integer           lpaxes(73,19)                   
c                                                       ! local solution p-axes 90% conf region (azm, plng in 5 deg inc)
      integer           ltaxes(73,19)                   
c                                                       ! local solution t-axes 90% conf region (azm, plng in 5 deg inc)
      character*124     line                            
c                                                       ! output string of nearby solutions orientations
      integer           m                               
c                                                       ! loop index over rake
      character*1       macsrc(mxstat)
c                                                       ! (input) allowable machine source codes
      integer           m1                              
c                                                       ! rake index of best solution
      integer           m1c                             
c                                                       ! m1 from coarse solution
      integer           minobs                          
c                                                       ! minimum number of observations required
      integer           mmax                            
c                                                       ! temporary value of mmaxc
      integer           mmaxc                           
c                                                       ! largest rake index of coarse solution with fit<=fitlim about (j1,n1,m1)
      integer           mmaxf                           
c                                                       ! largest rake index of fine solution with fit<=fitlim about (j1,n1,m1)
      integer           mmin                            
c                                                       ! temporary value of mminc
      integer           mminc                           
c                                                       ! smallest rake index of coarse solution with fit<=fitlim about (j1,n1,m1)
      integer           mminf                           
c                                                       ! smallest rake index of fine solution with fit<=fitlim about (j1,n1,m1)
      integer           n                               
c                                                       ! loop index of strike
      integer           naux                            
c                                                       ! solns index of complimentary solution
      integer           ncmpnt
c                                                       ! (input) number of allowable component codes (okcmp)
      integer           n1                              
c                                                       ! strike index of best solution
      integer           ndelc                           
c                                                       ! number of fault dip increments for coarse search
      integer           ndelf                           
c                                                       ! number of fault dip increments for fine search
      integer           ndlfdf                          
c                                                       ! default number of fault dip fine increments for unrestricted search 
      integer           ndrng(mxdip)                    
c                                                       ! number of dip solution ranges binned into ddelf degree increments
      integer           ndst                            
c                                                       ! number of distinct solutions found by hhog
      character*2       netcode(mxstat)
c                                                       ! seismic network code 
      integer           nfit(20)                        
c                                                       ! number of solutions binned into .025 fit increments
      integer           ng                              
c                                                       ! number of "good" solutions found in coarse search
      integer           nhsrc
c                                                       ! number of allowed hand-timed source codes (hndsrc)
      integer           nkil                            
c                                                       ! number of ignored stations
      integer           nlamc                           
c                                                       ! number of fault rake increments for coarse search
      integer           nlamf                           
c                                                       ! number of fault rake increments for fine search
      integer           nlmfdf                          
c                                                       ! default number of fault rake fine increments for unrestricted search 
      integer           nmaxc                           
c                                                       ! largest strike index of coarse solution with fit<=fitlim about (j1,n1,m1)
      integer           nmaxf                           
c                                                       ! largest strike index of fine solution with fit<=fitlim about (j1,n1,m1)
      integer           nminc                           
c                                                       ! smallest strike index of coarse solution with fit<=fitlim about (j1,n1,m1)
      integer           nminf                           
c                                                       ! smallest strike index of fine solution with fit<=fitlim about (j1,n1,m1)
      integer           nmsrc
c                                                       ! number of allowed machine source codes (macsrc)
      integer           nphfdf                          
c                                                       ! default number of fault strike fine increments for unrestricted search 
      integer           nphic                           
c                                                       ! number of fault strike increments for coarse search
      integer           nphif                           
c                                                       ! number of fault strike increments for fine search
      integer           nr                              
c                                                       ! -1=eof, 0=skip event, nr>0 => number of stations
      integer           nrsv                            
c                                                       ! number of readings in a composite solution
      integer           nrev                            
c                                                       ! number of reversed stations
      integer           nrrng(mxrake + 1)               
c                                                       ! number of rake solution ranges binned into dlamf degree increments
      integer           nsol                            
c                                                       ! number of planes stored in array solns
      integer           nsrng(mxstrk)                   
c                                                       ! number of strike solution ranges binned into dphif degree increments
      integer           nstat                           
c                                                       ! total # of stations reporting for entire data set
      character*3       okcmp(mxstat)
c                                                       ! (input) allowable component codes
      real              pain                            
c                                                       ! angle of incidence of p axis (deg)
      real              paz                             
c                                                       ! azimuth of p axis (deg)
      real              phi(mxstrk)                     
c                                                       ! fault strike angle in degrees
      real              phic(mxstrk)                    
c                                                       ! fault strike angle in degrees for coarse search
      real              phimax                          
c                                                       ! maximum strike range for solutions with fit<fitlim
      real              phimin                          
c                                                       ! minimum strike range for solutions with fit<fitlim
      real              phi0c                           
c                                                       ! initial fault strike angle in degrees for coarse search
      real              phi0f                           
c                                                       ! initial fault strike angle in degrees for fine search
      real              phi1f                           
c                                                       ! terminating fault strike angle in degrees for fine search
      real              pi                              
c                                                       ! pi
      real              pobs(mxstat)                    
c                                                       ! observed first motion polarities; .5=compression, -.5=dilatation
      real              prad                            
c                                                       ! radiation amplitude corresponding ain, az.
                                                        
c                                                       ! (dilatation) -1.<prad<+1.(compression)
      real              prcntx                          
c                                                       ! % of stations that are machine picked
      character*4       prmk(mxstat)                    
c                                                       ! first motion description (eg. ipu0)
      integer           qcnt(mxqual,2)                  
c                                                       ! (input) indx 1=# of dscrpnt plrties for qlity, indx 2=# of observations
      real              qcntwt(mxqual,2)                
c                                                       ! index 1=weighted # dscrpnt polrities for quality, index 2=sum of weights
      real              rad                             
c                                                       ! conversion from degrees to radians
      real              resmax                          
c                                                       ! maximum permitted p-residual 
      character*2       revnet(mxstat)
c                                                       ! seismic network code for revsta
      character*5       revsta(mxstat)                  
c                                                       ! reversed station names
      real              sa1                             
c                                                       ! slip angle of principle solution
      real              sa2                             
c                                                       ! slip angle of auxilliary solution
      integer           scnt(mxstat,2)                  
c                                                       ! index 1=# of dscrpnt polarities for stat, index 2=# of observations
      real              scntwt(mxstat,2)                
c                                                       ! index 1=weighted # dscrpnt polarities for stat, index 2=sum of weights
      character*1       sflag                           
c                                                       ! flag indicating secondary solutions
      real              sigmaf                          
c                                                       ! calculated standard deviation of fit based on data errors
      real              slip                            
c                                                       ! slip angle of best solution
      real              solns(mxslns,3)                 
c                                                       ! array of final solutions used to check for redundancy
      character*1       src(mxstat)
c							! data source code
      character*5       stat(mxstat)                    
c                                                       ! names of all stations reporting
      real              stdr                            
c                                                       ! station distibution ratio
      character*5       stn(mxstat)                     
c                                                       ! station names per event
      character*5       string                          
c                                                       ! scratch variable
      real              strike                          
c                                                       ! strike of best solution
      real              sumwt                           
c                                                       ! sum of observed first motion weights
      real              tain                            
c                                                       ! angle of incidence of t axis (deg)
      real              taz                             
c                                                       ! azimuth of t axis (deg)
      character*80      title                           
c                                                       ! data set descriptor
      real              tm(6)                           
c                                                       ! moment tensor in upper triangular symetric storage mode
      real              u(3)                            
c                                                       ! cartesian p-wave direction vector (positive up, south, east)
      real              weight(mxqual)                  
c                                                       ! weights associated with qualities
      real              wtmax                           
c                                                       ! maximum weight
      real              wtobs(mxstat)                   
c                                                       ! observed first motions weights
      real              xlam(mxrake)                    
c                                                       ! fault rake angle in degrees
      real              xlamc(mxrake)                   
c                                                       ! fault rake angle in degrees for coarse search
      real              xlam0c                          
c                                                       ! initial fault rake angle in degrees for coarse search
      real              xlam0f                          
c                                                       ! initial fault rake angle in degrees for fine search
      real              xlam1f                          
c                                                       ! terminating fault rake angle in degrees for fine search
      real              xlmmax                          
c                                                       ! maximum rake range for solutions with fit<fitlim
      real              xlmmin                          
c                                                       ! minimum rake range for solutions with fit<fitlim
c
c  initialize statistics arrays to zero, control parameters to defaults
c
      data nstat, nfit /0, 20*0/
      data qcnt, qcntwt /mxqual*0, mxqual*0, mxqual*0.0, mxqual*0.0/
      data ndrng, nsrng, nrrng /mxdip*0, mxstrk*0, mxrake*0, 0/
      data flgc /'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
     & 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T'/
      data erate /0.01, 0.02, 0.05, 0.1, 1.0, 1.0, 1.0, 1.0/
      data dbg /.false./
      data distmx /99999./
      data ainmin /0./
      data ainmax /180./
      data resmax /100./
      data fmagmn /0./
      data iamwt /0/
      data ibst /0/
      data irep /0/
      data irpcnt /19/
      data irslam /0/
      data icmp /0/
      data ifin /1/
      data nmsrc /1/
      data macsrc(1) /'W'/
      data nhsrc /1/
      data hndsrc(1) /'2'/
      data nkil /0/
      data nrev /0/
      data idate /mxstat*0, mxstat*0/
      data infmt /3/
      data ittl /1/
      data title /'none'/
      data minobs /15/
      data ncmpnt /1/
      data okcmp(1) /'VHZ'/
      data ndlfdf /19/
      data nlmfdf /7/
      data nphfdf /19/
      data lopen2 /.false./
      data lopen3 /.false./
      data lopen4 /.false./

      pi = 4.*atan(1.0)
      rad = pi/180.
c
c calculate machine precision
c
      eps = 1.
5     eps = 0.5*eps
      epsp1 = eps + 1.
      if (epsp1 .gt. 1.) goto 5
      if (eps .eq. 0.) then
        print *, 'machine precision equals zero in main routine'
        stop
      end if
c
c get instructions 
c
10    call input (eunit, funit, iunit, punit, sunit, lopen2,
     & lopen3, lopen4, ddelc, ddelf, del0c, distmx, dlamc, dlamf,
     & dphic, dphif, erate, fmagmn, iamwt, ibst, infmt, ires, irep,
     & kilsta, minobs, mxdip, mxqual, mxrake, mxstat, mxstrk, ndelc,
     & nkil, nlamc, nphic, nrev, phi0c, revsta, idate, irslam,
     & title, weight, xlam0c, ifin, ittl, icmp, resmax, ainmin, ainmax, 
     & kilnet, revnet, dbg, ncmpnt, okcmp, macsrc, nmsrc, hndsrc, nhsrc)
c
      ievp = 0
      ievr = 0
      nr = 0
      nrsv = 0
      if (lopen4) call blurb (funit)
c
c find maximum weight
c
      wtmax = 0.
      do 20 i = 1, mxqual
        if (weight(i) .gt. wtmax) wtmax = weight(i)
20    continue
c
c read next event
c
30    ievr = ievr + 1
      if (infmt .eq. 1) then
        call rdeq1 (ain, ainmin, ainmax, az, dist, distmx, eunit, 
     & event, fmagmn, iunit, kilsta, minobs, mxqual, mxstat,
     & nkil, nr, nrev, pobs, prcntx, prmk, resmax, revsta, sigmaf, stn,
     & sumwt, weight, wtobs, icmp, idate, dbg)
      else if (infmt .eq. 2) then
        call rdeq2 (ain, ainmin, ainmax, az, dist, distmx, eunit, 
     & event, fmagmn, iunit, kilsta, minobs, mxqual, mxstat,
     & nkil, nr, nrev, pobs, prcntx, prmk, resmax, revsta, sigmaf, stn,
     & sumwt, weight, wtobs, icmp, idate, dbg)
      else if (infmt .ge. 3) then
       call rdeq3 (ain, ainmin, ainmax, az, dist, distmx, eunit,
     & event, fmagmn, iunit, kilsta, minobs, mxqual, mxstat,
     & nkil, nr, nrev, pobs, prcntx, prmk, resmax, revsta, sigmaf, stn,
     & sumwt, weight, wtobs, icmp, idate, evntid, netcode, src, 
     & kilnet, revnet, dbg, macsrc, nmsrc, hndsrc, nhsrc, ncmpnt, okcmp)
      end if
c
c continue accumulating data for composite until eof
c
        if (icmp .eq. 1) then
          if (nr .ge. 0) then
            if (nr .ne. nrsv) ievp = ievp + 1
            nrsv = nr
            goto 30
          else 
            nr = nrsv
            ievp = ievp - 1
            ievr = ievr - 1
            icmp = 0
            goto 40
          end if
        end if
c
c end of file
c
        if (nr .eq. -1) then
          close (iunit)
          ievr = ievr - 1
          if (nstat .gt. 0) then
            call fpout (ddelf, dlamf, dphif, erate, eunit, ievp, ievr,
     & ind, ires, mxdip, mxqual, mxrake, mxstat, mxstrk, ndelf, ndrng,
     & nfit, nlamf, nphif, nrev, nrrng, nsrng, nstat, qcnt, qcntwt,
     & revsta, scnt, scntwt, stat)
c
          else
            write (eunit, *) '***** fpfit error: no events satisfy in'//
     & 'put criteria *****'
            print *, '***** fpfit error: no events satisfy input criteri
     &a *****'
            inquire (eunit, name = filnam)
	    do 35 i = 50, 1, -1
	      if (filnam(i:i) .ne. ' ') goto 36
35	    continue
36	    print *, '      error messages can be found in ', filnam(1:i)
          end if
          goto 10
        end if
c
c insufficient readings skip event
c
        if (nr .eq. 0) goto 30
c
c good event: begin event loop
c set up p-wave direction unit vector  (up, south, east) for each ray
c
40      do 50 i = 1, nr
          ainr = ain(i)*rad
          azr = az(i)*rad
          u(1) = -cos(ainr)
          u(2) = -sin(ainr)*cos(azr)
          u(3) = sin(ainr)*sin(azr)
c
c find excitation coefficients for determining far-field p radiation pattern
c
          call pexcf (coef, i, mxstat, u)
50      continue
c 
c zero out the p-, t-axes 90% confidence region
c
        do 70 i = 1, 73
          do 60 j = 1, 19
            ipaxes(i,j) = 0
            itaxes(i,j) = 0
60        continue
70      continue
c
c coarse loop through fault models
c
        fit90 = 1.282 * sigmaf
        ievp = ievp + 1
        first = .true.
        call search (bot, coef, ddelc, del, delc, del0c, fit90, dlamc,
     & dphic, eps, first, fit, fitmnc, flag, gfit, iamwt, igood, j1,
     & m1, mxdip, mxrake, mxstat, mxstrk, n1, ndelc, ng, nlamc, nphic,
     & nr, phi, phic, phi0c, pobs, rad, wtobs, xlam, xlamc, xlam0c)
c
c determine distinct solutions from array of "good" solutions in coarse search
c
        call hhog (delc, eunit, j1, n1, m1, igood, ipaxes, itaxes, gfit,
     & ng, idst, ndst, mxdip, mxslns, mxstrk, mxrake, ndelc, nphic,
     & nlamc, phic, xlamc, bot, irslam)
        if (lopen4) then
c
c print out coarse fit function
c
          write (funit, 80) event
80        format (//, '***** coarse search *****', /, a)
          do 90 id = 1, ndst
            flag(idst(id, 1), idst(id, 2), idst(id, 3)) = flgc(id)
90        continue
          do 150 m = 1, nlamc
            write (funit, 100) ifix(xlamc(m))
100         format (///, 3x, 'slip angle = ', i4)
            write (funit, 110) (ifix(phic(n)), n = 1, nphic)
110         format (' strike:', 24(2x, i3))
            write (funit, *) ' dip'
            do 140 j = 1, ndelc
              do 120 n = 1, nphic
                if (lopen4) then
                  ifit(n) = ifix(1000.*fit(j, n, m))
                  if (ifit(n) .eq. 1000) ifit(n) = 999
                end if
120           continue
              write (funit, 130) ifix(delc(j)),
     & (ifit(n), flag(j, n, m), n = 1, nphic)
130           format (i4, 6x, 24(i3, a1, 1x), //)
140         continue
150      continue
        end if
        aerrf = 2.*max(ddelf, dlamf, dphif)
        aerrc = 2.*max(ddelc, dlamc, dphic)
        if (ibst .eq. 1) ndst = 1
        do 155 id = 1, ndst
          j1 = idst(id, 1)
          n1 = idst(id, 2)
          m1 = idst(id, 3)
          idst(id, 5) = 1
c
c  test whether minima is on edge of coarse seach grid.
c
          if (j1 .eq. 1 .or. j1 .eq. ndelc .or. n1 .eq. 1 .or. n1 .eq.
     & nphic .or. m1 .eq. 1 .or. m1 .eq. nlamc) then
            idst(id, 4) = 1
          else
            idst(id, 4) = 0
          end if
c
c express coarse solution in terms of dip direction, dip angle, and slip angle.
c
          call refrmt(delc(j1), idip1, idpdr1, islip1, phic(n1),
     & xlamc(m1))
          solns(id, 1) = idpdr1
          solns(id, 2) = idip1
          solns(id, 3) = islip1
c
c ascertain whether solution is an auxilliary plane of another solution in 
c coarse search. if so, set flag to skip fine search for solution which 
c resides at edge of coarse search grid, since these solutions are less likely
c to converge.
c
          if (id .gt. 1 .and. scompl(solns, id - 1, float(idpdr1),
     & float(idip1), float(islip1), aerrc, mxslns, naux)) then
            if (idst(naux, 4) .eq. 1 .and. idst(id, 4) .eq. 0) then
              idst(naux, 5) = 0
            else 
              idst(id, 5) = 0
            end if
          end if
155     continue
c
c begin loop for fine search centered on each distinct solution found by hhog.
c skip searches on minima that have been identified as auxilliary planes of 
c other solutions.
c
        best = .true.
        nsol = 0
        do 430 id = 1, ndst
          if (idst(id, 5) .eq. 0) goto 430
          j1 = idst(id, 1)
          n1 = idst(id, 2)
          m1 = idst(id, 3)
          m1c = m1
          first = .false.
c
c  find fit90 range of coarse search
c
          fitlim = fitmnc + fit90
          jmaxc= 0
          jminc= ndelc
          nmaxc= 0
          nminc= nphic
          mmaxc= 0
          mminc= nlamc
          do 160 ig = 1, ng
            if (igood(ig, 4) .eq. id .and. gfit(ig) .le. fitlim) then
              j = igood(ig, 1)
              n = igood(ig, 2)
              m = igood(ig, 3)
              if (j .lt. jminc .and. n .eq. n1 .and. m .eq. m1) jminc =j
              if (j .gt. jmaxc .and. n .eq. n1 .and. m .eq. m1) jmaxc =j
              if (n .lt. nminc .and. j .eq. j1 .and. m .eq. m1) nminc =n
              if (n .gt. nmaxc .and. j .eq. j1 .and. m .eq. m1) nmaxc =n
              if (m .lt. mminc .and. j .eq. j1 .and. n .eq. n1) mminc =m
              if (m .gt. mmaxc .and. j .eq. j1 .and. n .eq. n1) mmaxc =m
            end if
160       continue
          if (mminc .eq. 1 .and. mmaxc .eq. nlamc) then
c
c check rake range for wrap around
c
            mmin = 1
            mmax = nlamc
            do 170 ig = 1, ng
              if (igood(ig, 4) .eq. id .and. gfit(ig) .le. fitlim .and.
     & igood(ig, 1) .eq. j1 .and. igood(ig, 2) .eq. n1) then
                if (igood(ig, 3) .eq. mmin + 1) mmin = mmin + 1
              end if
170         continue
            do 180 ig = ng, 1, -1
              if (igood(ig, 4) .eq. id .and. gfit(ig) .le. fitlim .and.
     & igood(ig, 1) .eq. j1 .and. igood(ig, 2) .eq. n1) then
                if (igood(ig, 3) .eq. mmax - 1) mmax = mmax - 1
              end if
180         continue
            if (mmin .ge. 1 .or. mmax .le. nlamc) then
c
c flag wrap around by making mminc > mmaxc
c
              mmaxc = mmin
              mminc = mmax
            end if
          end if
c
c determine fine search range for unrestricted search.
c restrict to fit90 range of coarse search if less than default fine search range
c
          if (ires .eq. 0 .or. (ires .eq. 1 .and. ifin .eq. 0)) then
            if (jminc .gt. 1) then
              del0f = max(delc(jminc - 1), delc(j1) -
     & float(ndlfdf/2)*ddelf)
            else
              del0f = delc(j1) - float(ndlfdf/2)*ddelf
            end if
            if (jmaxc .lt. ndelc) then
              del1f = min(delc(jmaxc + 1), delc(j1) +
     & float((ndlfdf + 1)/2)*ddelf)
            else
              del1f = delc(j1) + float((ndlfdf + 1)/2)*ddelf
            end if
            ndelf = min(ndlfdf, int((del1f - del0f)/ddelf) + 1)
c
            if (nminc .gt. 1) then
              phi0f = max(phic(nminc - 1), phic(n1) -
     & float(nphfdf/2)*dphif)
            else
              phi0f = phic(n1) - float(nphfdf/2)*dphif
            end if
            if (nmaxc .lt. nphic) then
              phi1f = min(phic(nmaxc + 1), phic(n1) +
     & float((nphfdf + 1)/2)*dphif)
            else
              phi1f = phic(n1) + float((nphfdf + 1)/2)*dphif
            end if
            nphif = min(nphfdf, int((phi1f - phi0f)/dphif) + 1)
c
c rake is different since it has wrap-around problems
c
            if (mminc .le. mmaxc) then
              xlam0f = max(xlamc(m1) - float(nlmfdf/2)*dlamf,
     & xlamc(mminc) - dlamc)
              xlam1f = min(xlamc(m1) + float((nlmfdf + 1)/2)*dlamf,
     & xlamc(mmaxc) + dlamc)
            else
              if (m1 .ge. mminc) then
                xlam0f = max(xlamc(m1) - float(nlmfdf/2)*dlamf,
     & xlamc(mminc) - dlamc)
                xlam1f = min(xlamc(m1) + float((nlmfdf + 1)/2)*dlamf,
     & xlamc(mmaxc) + 360. + dlamc)
              else
                xlam0f = max(xlamc(m1) - float(nlmfdf/2)*dlamf,
     & xlamc(mminc) - 360. - dlamc)
                xlam1f = min(xlamc(m1) + float((nlmfdf + 1)/2)*dlamf,
     & xlamc(mmaxc) + dlamc)
              end if
            end if
            nlamf = min(nlmfdf, int((xlam1f - xlam0f)/dlamf) + 1)
          else
c
c determine fine search for restricted search.  restrict to user-specified 
c restricted range as well as fit90 range of coarse search 
c
            del0f = max(delc(j1) - float(ndelc/2)*ddelc, del0c)
            if (jminc .gt. 1) del0f = max(del0f, delc(jminc - 1))
            del1f = min(delc(j1) + float((ndelc + 1)/2)*ddelc,
     & delc(ndelc))
            if (jmaxc .lt. ndelc) del1f = min(del1f, delc(jmaxc + 1))
            ndelf = min(mxdip, int((del1f - del0f)/ddelf) + 1)
c
            phi0f = max(phic(n1) - float(nphic/2)*dphic, phi0c)
            if (nminc .gt. 1) phi0f = max(phi0f, phic(nminc - 1))
            phi1f = min(phic(n1) + float((nphic + 1)/2)*dphic,
     & phic(nphic))
            if (nmaxc .lt. nphic) phi1f = min(phi1f, phic(nmaxc + 1))
            nphif = min(mxstrk, int((phi1f - phi0f)/dphif) + 1)
c
            xlam0f = max(xlamc(m1) - float(nlamc/2)*dlamc, xlam0c)
            if (mminc .gt. 1) xlam0f = max(xlam0f, xlamc(mminc - 1))
            xlam1f = min(xlamc(m1) + float((nlamc + 1)/2)*dlamc,
     & xlamc(nlamc))
            if (mmaxc .lt. nlamc) xlam1f = min(xlam1f, xlamc(mmaxc + 1))
            nlamf = min(mxrake, int((xlam1f - xlam0f)/dlamf) + 1)
          end if
c
c  do fine search
c
          call search (bot, coef, ddelf, del, delc, del0f, fit90, dlamf,
     & dphif, eps, first, fit, fitmnf, flag, gfit, iamwt, igood, j1,
     & m1, mxdip, mxrake, mxstat, mxstrk, n1, ndelf, ng, nlamf, nphif,
     & nr, phi, phic, phi0f, pobs, rad, wtobs, xlam, xlamc, xlam0f)
c
c express fine solution in terms of dip direction, dip angle, and slip angle.
c
          call refrmt(del(j1), idip1, idpdr1, islip1, phi(n1), xlam(m1))
c
c check again if fine search solution is an auxilliary plane of another solution
c or if it is the same as another solution in the list
c
          if (nsol .eq. 0 .or. (.not. scompl(solns, nsol, float(idpdr1),
     & float(idip1), float(islip1), aerrf, mxslns, naux))) then
            nsol = nsol + 1
            solns(nsol, 1) = idpdr1
            solns(nsol, 2) = idip1
            solns(nsol, 3) = islip1
c            if (nsol .eq. 1) write (eunit, *) 'event ', event(1:19),
c     & ' has multiple solutions'
c 
c copy the p-, t-axes 90% confidence region for this discrete solution into 
c local array.
c
            do 200 i = 1, 73
              do 190 j = 1, 19
                if (ipaxes(i, j) .eq. id) then
                  lpaxes(i, j) = id
                else
                  lpaxes(i, j) = 0
                end if
                if (itaxes(i, j) .eq. id) then
                  ltaxes(i, j) = id
                else
                  ltaxes(i, j) = 0
                end if
190           continue
200         continue
c
c find the range of dip, strike and rake spanning each good solution for which 
c the fit is .le. fitlim.
c
            fitlim = fitmnf + fit90
            jmaxf= 0
            jminf= ndelf
            nmaxf= 0
            nminf= nphif
            mmaxf= 0
            mminf= nlamf
            do 210 m = 1, nlamf
              if (fit(j1, n1, m) .le. fitlim) then
                if (m .lt. mminf) mminf= m
                if (m .gt. mmaxf) mmaxf= m
              end if
210         continue
            do 220 n = 1, nphif
              if (fit(j1, n, m1) .le. fitlim) then
                if (n .lt. nminf) nminf= n
                if (n .gt. nmaxf) nmaxf= n
              end if
220         continue
            do 230 j = 1, ndelf
              if (fit(j, n1, m1) .le. fitlim) then
                if (j .lt. jminf) jminf= j
                if (j .gt. jmaxf) jmaxf= j
              end if
230         continue
c
c find largest dip, strike, rake half-width range for this solution
c
            delmin = min(delc(jminc), del(jminf))
            delmax = max(delc(jmaxc), del(jmaxf))
            phimin = min(phic(nminc), phi(nminf))
            phimax = max(phic(nmaxc), phi(nmaxf))
            if (mminc .le. mmaxc) then
              xlmmin = min(xlamc(mminc), xlam(mminf))
              xlmmax = max(xlamc(mmaxc), xlam(mmaxf))
            else if (m1c .ge. mminc) then
              xlmmin = min(xlamc(mminc), xlam(mminf))
              xlmmax = max(xlamc(mmaxc) + 360., xlam(mmaxf))
            else
              xlmmin = min(xlamc(mminc) -360., xlam(mminf))
              xlmmax = max(xlamc(mmaxc), xlam(mmaxf))
            end if
            idrng = nint((delmax - delmin)/2.)
            isrng = nint((phimax - phimin)/2.)
            irrng = nint((xlmmax - xlmmin)/2.)
c
c accumulate statistics on distribution of dip, strike, rake ranges for best 
c solutions only.
c
            if (best .and. (ires .eq. 0)) then
              index = idrng/5 + 1
              if (index .gt. mxdip) index = mxdip
              ndrng(index) = ndrng(index) + 1
              index = isrng/5 + 1
              if (index .gt. mxstrk) index = mxstrk
              nsrng(index) = nsrng(index) + 1
              index = irrng/10 + 1
              if (index .gt. mxrake + 1) index = mxrake
              nrrng(index) = nrrng(index) + 1
            end if
c
c  check for convergence (i.e., minima at grid edge)
c
            if ((ires .eq. 0) .and. (j1 .eq. 1 .or. j1 .eq. ndelf .or.
     & n1 .eq. 1 .or. n1 .eq. nphif .or. m1 .eq. 1 .or. m1 .eq. nlamf)) 
     & then
              cflag1 = 'no'
              cflag2 = 'C'
            else
              cflag1 = '  '
              cflag2 = ' '
            end if
            if (.not. best) then
              sflag = '*'
            else
              sflag = ' '
            end if
            stdr = bot(j1,n1,m1)/sumwt
c
c write out results
c
            if (infmt .eq. 3) then
              write (evfit, 240) idpdr1, idip1, islip1, fit(j1, n1, m1),
     &        nr, fitlim, stdr, prcntx, isrng, idrng, irrng, cflag2,
     &	      sflag, evntid
	    else
              write (evfit, 240) idpdr1, idip1, islip1, fit(j1, n1, m1),
     &        nr, fitlim, stdr, prcntx, isrng, idrng, irrng, cflag2, 
     &	      sflag
	    end if
240         format (i4, i3, i4, 2x, f4.2, 1x, i3, 1x, 2f5.2, 1x, f4.2,
     &      1x, 3i3, 2a1, i10)
            if (lopen4) write (funit, 250) event, evfit
250         format (/////, '***** fine search *****', /, a82, a59)
            if (lopen2) write (sunit, 260) event, evfit
260         format (a82, a59)
            if (lopen3) write (punit, 260) event, evfit
c            if (cflag2 .eq. 'C') write (eunit, 265) event(1:52), idpdr1,
c     & idip1, islip1
c265         format (1x, 'warning: event ', a52, i6, i5, i6,
c     & ' may not have converged to minima')
            if (irep .eq. 1) then
              if (irpcnt .eq. 19) then
                print *, '  #      ORIGIN TIME         LOCATION        '
     & //' DEPTH    MAG   DDR  DIP  RAKE CNVRG'
                print *, ' ---- ------------------- ------------------ '
     & //' -----    ---  ----- ---  ---- -----'
                irpcnt = 1
              end if
c
c   add solution to fps.out, seisan add by jens
c
              call add_fps(float(idpdr1)-90.0, float(idip1), 
     *        float(islip1),'FPFIT  ','T')

              if (best) then
                write (*, 270) ievp, event(1:52), idpdr1, idip1, islip1,
     & cflag1
270             format (1x, i4, 1x, a52, i6, i5, i6, 3x, a)
              else
                write (*, 280) idpdr1, idip1, islip1, cflag1
280             format (1x, '     MULTIPLE SOLUTION', t59, i6, i5, i6,
     & 3x, a)
              end if
              irpcnt = irpcnt + 1
            end if
c
c  loop over search area to print out fit parameters, 90% confidence region
c
            do 310 m = 1, nlamf
              if (lopen4) then
                write (funit, 100) ifix(xlam(m))
                write (funit, 110) (ifix(phi(n)), n = 1, nphif)
                write (funit, *) ' dip'
              end if
              do 300 j = 1, ndelf
                do 290 n = 1, nphif
                  if (lopen4) then
                    ifit(n) = ifix(1000.*fit(j, n, m))
                    if (ifit(n) .eq. 1000) ifit(n) = 999
                  end if
                  if (flag(j, n, m) .eq. '*') then
c
c find nearest grid cell in p, t axes arrays for 90% confidence region output 
c
                    da1 = del(j)
                    dd1 = phi(n) + 90.
                    sa1 = xlam(m)
                    call auxpln (dd1, da1, sa1, dd2, da2, sa2)
                    call tandp (pain, tain, paz, taz, da1, da2, dd1,
     & dd2, sa1, sa2, pi, rad)
                    indxa = nint(paz/5.) + 1
                    indxp = nint(pain/5.) + 1
                    lpaxes(indxa, indxp) = id
                    indxa = nint(taz/5.) + 1
                    indxp = nint(tain/5.) + 1
                    ltaxes(indxa, indxp) = id
                  end if
290             continue
                if (lopen4) write (funit, 130) ifix(del(j)),
     & (ifit(n), flag(j, n, m), n = 1, nphif)
300           continue
310         continue
c
c  output p-axes confidence region
c
            if (lopen3) then
              k = 0
              do 340 i = 1, 73
                do 330 j = 1, 19
                  if (lpaxes(i, j) .eq. id) then
                    k = k + 1
                    write (string, '(i3, i2)') (i - 1)*5, (j - 1)*5
                    if (k .eq. 1) then
                      line = 'P-AX'//string
                    else
                      line = line(1:(k - 1)*5 + 4)//string
                    end if
                    if (k .eq. 25) then
                      k = 0
                      write (punit, 320) line
320                   format (a)
                    end if
                  end if
330             continue
340           continue
              if (k .ne. 0) write (punit, 320) line
c
c  output t-axes confidence region
c
              k = 0
              do 360 i = 1, 73
                do 350 j = 1, 19
                  if (ltaxes(i, j) .eq. id) then
                    k = k + 1
                    write (string, '(i3, i2)') (i - 1)*5, (j - 1)*5
                    if (k .eq. 1) then
                      line = 'T-AX'//string
                    else
                      line = line(1:(k - 1)*5 + 4)//string
                    end if
                    if (k .eq. 25) then
                      k = 0
                      write (punit, 320) line
                    end if
                  end if
350             continue
360           continue
              if (k .ne. 0) write (punit, 320) line
              write (punit, 320) '    '
            end if
c
c accumulate statistics on fit parameter distribution for best solutions only
c
            if (best) then
              index = ifix(fit(j1, n1, m1)/.025) + 1
              if (index .gt. 20) index = 20
              nfit(index) = nfit(index) + 1
            end if
c
c recompute moment tensor representation for best solution to check for 
c polarity discrepancies.
c
            strike = phi(n1)*rad
            dip = del(j1)*rad
            slip = xlam(m1)*rad
            call shrflt (strike, dip, slip, tm)
            do 420 k = 1, nr
              if (nstat .ge. 1) then
                do 370 i = 1, nstat
                  if (stn(k) .eq. stat(i)) goto 380
370             continue
              end if
              nstat = nstat + 1
              if (nstat .gt. mxstat) then
                write (eunit, *) '***** fpfit error: # of stations ha'//
     & 've polarity discepancies exceeds ', mxstat, ' *****'
                write (eunit, *) ' station ', stn(k), ' not reported '//
     & 'in final summary'
                goto 400
              end if
              i = nstat
              stat(nstat) = stn(k)
              scnt(nstat, 1) = 0
              scnt(nstat, 2) = 0
              scntwt(nstat, 1) = 0.
              scntwt(nstat, 2) = 0.
380           read (prmk(k), '(3x, i1)') ipwt
              if (infmt .eq. 3 .and. nmsrc .gt. 0) then
	        do 381 isrc = 1, nmsrc
	          if (src(k) .eq. macsrc(isrc)) ipwt = ipwt + mxqual/2
381	        continue
	      endif
c
c recompute radiation pattern
c
              prad = 0
              do 390 l = 1, 6
                prad = prad + tm(l)*coef(k, l)
390           continue
	      if (prad .eq. 0.) prad = eps
c
c check polarity and update appropriate station count
c
              if (sign(0.5, prad) .ne. pobs(k)) then
                if (best) then
                  scnt(i, 1) = scnt(i, 1) + 1
                  scntwt(i, 1) = scntwt(i, 1) +
     & wtobs(k)*sqrt(abs(prad))
                  qcnt(ipwt + 1, 1) = qcnt(ipwt + 1, 1) + 1
                  qcntwt(ipwt + 1, 1) = qcntwt(ipwt + 1, 1) +
     & wtobs(k)*sqrt(abs(prad))
                end if
                bdflag = '*'
              else
                bdflag = ' '
              end if
              if (best) then
                scnt(i, 2) = scnt(i, 2) + 1
                scntwt(i, 2) = scntwt(i, 2) +
     & wtobs(k)*sqrt(abs(prad))
                qcnt(ipwt + 1, 2) = qcnt(ipwt + 1, 2) + 1
                qcntwt(ipwt + 1, 2) = qcntwt(ipwt + 1, 2) +
     & wtobs(k)*sqrt(abs(prad))
              end if
c
c write out to polarity file
c
400           if (lopen3) write (punit, 410) stn(k), dist(k), az(k), 
     & ain(k), prmk(k), wtobs(k)/wtmax, bdflag
410           format (a5, 3f6.1, 3x, a4, f5.2, 2x, a1)
420         continue
            if (lopen3) write (punit, 320) '              '
          end if
          best = .false.
c
c end of fine search solution loop
c
430     continue
c
c end of event
c
      goto 30
      end
      subroutine askc (prompt, string)
c
c  askc prompts then reads a character string from the terminal.
c  the original value is unchanged by a cr response.
c
      character         prompt*(*)                      
c							! prompt string
      character         string*(*)                      
c							! character response, or original string on cr.

      character         temp*80                         
c							! scratch
      integer           leng                            
c							! function
      integer           nch                             
c							! number of characters
      integer		ounit
c							! logical unit for output 

      parameter (ounit = 0)
c     parameter (ounit = 6)						! VAX/VMS
      nch = leng(string)
10    write (ounit, 20) prompt
20    format (1x, a)
      if (nch .lt. 20) then
        write (ounit, 30) string(1:nch)
30      format (' [cr = ', a, ']? ', $)
      else
        write (ounit, 40) string(1:nch)
40      format (' [cr = ', a, ']?')
      end if
      read (5, '(a)', err = 10, end = 50) temp
      if (leng(temp) .gt. 0) string = temp
50    return
      end

      real function askr (prompt, dflt)
c
c  askr prompts then reads a real value from the terminal.
c  the default value is returned on a cr response.
c
      real              dflt                            
c							! default supplied on carriage return and displayed in prompt
      character         prompt*(*)                      
c							! prompt string

      integer           i                               
c							! loop index
      integer           j                               
c							! loop index
      integer           leng                            
c							! function
      character         temp*20                         
c							! scratch
      integer		ounit
c							! logical unit for output 

      parameter (ounit = 0)
c      parameter (ounit = 6)				! VAX/VMS version
      write (temp, 10) dflt
10    format (g20.5)
      do 20 i = 1, 20
        if (temp(i:i) .ne. ' ') goto 30
20    continue
30    do 40 j = 20, 1, -1
        if (temp(j:j) .ne. ' ') goto 50
40    continue
50    write (ounit, 60) prompt, temp(i:j)
60    format (1x, a, ' [cr = ', a, ']? ', $)
      read (5, '(a)', err = 50, end = 70) temp
      if (leng(temp) .gt. 0) then
        read (temp, *, err = 50) askr
      else
        askr = dflt
      end if
70    return
      end

      subroutine auxpln (dd1, da1, sa1, dd2, da2, sa2)
c
c    Calculate the auxilliary plane of a double couple fault plane solution, given the principle plane.
c
c    written by Paul Reasenberg, June, 1984, from class notes by Dave Boore, (both at the U.S.G.S., Menlo Park.)
c    angle variables phi, del and lam are as defined in Aki and Richards, (1980), p.114.
c
      real              da1                             
c                                                       ! (input)  dip angle in degrees of priciple plane
      real              dd1                             
c                                                       ! (input)  dip directions in degrees of priciple plane
      real              sa1                             
c                                                       ! (input)  slip angle in degrees of priciple plane
      real              da2                             
c                                                       ! (output)  dip angle in degrees of auxilliary plane
      real              dd2                             
c                                                       ! (output)  dip directions in degrees of auxilliary plane
      real              sa2                             
c                                                       ! (output)  slip angle in degrees of auxilliary plane
c

      double precision  bot                             
c                                                       ! scratch variable
      double precision  del1                            
c                                                       ! dip angle of principal plane in radians
      logical           first                           
c                                                       ! test: true if first time into routine
      double precision  phi1                            
c                                                       ! fault plane strike of principal plane
      double precision  phi2                            
c                                                       ! strike of auxilliary plane in radians
      double precision  rad                             
c                                                       ! conversion factor from degrees to radian
      double precision  sgn                             
c                                                       ! saves principal plane slip angle for assigning proper sign to auxilliary
      double precision  top                             
c                                                       ! scratch variable
      double precision  xlam1                           
c                                                       ! slip angle of principal plane in radians
      double precision  xlam2                           
c                                                       ! slip angle of auxilliary plane
c
      data first /.true./
      save first, rad
c
      if (first) then
        first = .false.
        rad = datan(1.0d0)/45.0d0
      end if
c
      phi1 = dd1 - 90.0d0
      if (phi1 .lt. 0.0d0) phi1 = phi1 + 360.0d0
      phi1 = phi1*rad
      del1 = da1*rad
      sgn = sa1
      xlam1 = sa1*rad
c
      top = dcos(xlam1)*dsin(phi1) - dcos(del1)*dsin(xlam1)*dcos(phi1)
      bot = dcos(xlam1)*dcos(phi1) + dcos(del1)*dsin(xlam1)*dsin(phi1)
      dd2 = datan2(top, bot)/rad
      phi2 = (dd2 - 90.0d0)*rad
      if (sa1 .lt. 0.0d0) dd2 = dd2 - 180.0d0
      if (dd2 .lt. 0.0d0) dd2 = dd2 + 360.0d0
      if (dd2. gt. 360.0d0) dd2 = dd2 - 360.0d0
c
      da2 = dacos(dsin(dabs(xlam1))*dsin(del1))/rad
      xlam2 = -dcos(phi2)*dsin(del1)*dsin(phi1) +
     & dsin(phi2)*dsin(del1)*dcos(phi1)
c
c machine accuracy problem
c
      if (dabs(xlam2) .gt. 1.0d0) then
        xlam2 = dsign(1.0d0, xlam2)
      end if
      xlam2 = dsign(dacos(xlam2), sgn)
      sa2 = xlam2/rad
c
      return
      end
      subroutine blurb (funit)
c
c writes preamble blurb to fit-function file
c
      integer           funit                           
c							! (input) logical unit # of output of fit listing for all strikes, dips
c
      integer           i                               
c							! loop index
c
      write (funit, 10) ('-', i = 1, 79)
10    format (79a1, /, 
     & 'note: the solution on the extended summary cards is expressed ',
     & 'in terms of', /, 
     & 'dip-direction, dip, and rake.  the fit-function tables can be ',
     & 'converted into', /,
     & 'this format by the following rules:', /,
     & '    if (dip > 90) then', /,
     & '       dip = 180 - dip', /,
     & '       strike = strike + 180', /,
     & '       rake = -rake')
      write (funit, 20) 
20    format ('    else if (dip < 0) then', /,
     & '       dip = - dip', /,
     & '       strike = strike + 180', /,
     & '       rake = rake + 180', /,
     & '    end if', /,
     & '    dip-direction = modulus(strike + 90, 360)', /,
     & '    if (dip-direction < 0) dip-direction = dip-direction + 360',
     & /, '    rake = modulus(rake, 360)', /,
     & '    if (rake > 180) rake = rake - 360', /,
     & '    if (rake < -180) rake = rake + 360')
      write (funit, 25) 
25    format ('    if (dip = 0) compute auxilliary plane', /,
     & '    if (dip = 90 and dip-direction >= 180) then', /,
     & '       rake = -rake', /,
     & '       dip direction = dip direction - 180', /,
     & '    end if')
      write (funit, 30) ('-', i = 1, 79)
30    format (/,
     & 'all fit scores are multiplied by 1000 and range from 0 to 999.',
     & '  fit scores', /, 
     & 'annotated with an "*" are <= the fit score of the function min',
     & 'ima (ie., the', /,
     & 'best solution) + 90% confidence increment.  fit scores annotat',
     & 'ed with an "a"', /,
     & 'denote function minima.  for coarse search, discrete relative ',
     & 'fit minima', /,
     & 'corresponding to multiple solutions are annotated with "b","c"',
     & ', etc.  tables', /,
     & 'of fit scores for fine search solutions corresponding to these',
     & ' multiples may', /,
     & 'not be shown if the multiple is recognized as the auxilliary s',
     & 'olution of', /,
     & 'another solution', /, 79a1)
      return
      end
      logical function scompl (solns,nsol,dd,da,sa,aerr,mxslns,naux)
c
c this function compares a "new" fault plane solution (dd, da, sa) with a list of other fault plane solutions
c  and checks for any of the following conditions
c
c   1. the "new" solution is similar to one of the solutions in solns
c   2. the compliment of the "new" solution is similar to one of the solutions in solns
c   3. the "new" solution is similar to the compliment of one of the solutions in solns
c
c if any one of the above conditions is true, function scompl returns with a value .true.
c otherwise, function scompl returns with the value .false.
c
c  solutions are similar if all three pairs of corresponding angles differ by less than aerr.
c
      real              aerr                            
c							! allowable difference between corresponding angles of complimentary planes
      real              da                              
c							! dip angle of new plane
      real              dd                              
c							! dip direction angle of new plane
      integer           mxslns                          
c							! maximum # of multiple solutions permitted
      integer           naux                            
c							! solns index of complimentary solution
      integer           nsol                            
c							! number of planes stored in array solns
      real              sa                              
c							! slip angle of new plane
      real              solns(mxslns,3)                 
c							! array of planes (dd, da, sa) to test new plane against
c
      real              rdiff                           
c							! function
      real              aux1(3)                         
c							! dip direction, angle, and rake of auxilliary plane of new plane
      real              aux2(3)                         
c							! dip direction, angle, and rake of auxilliary plane of prrevious planes
c
      scompl = .false.
c
      call auxpln (dd, da, sa, aux1(1), aux1(2), aux1(3))
c
      do 10 naux = 1, nsol
c
c compare new solution with each solution on list
c
        if (abs(dd - solns(naux, 1)) .le. aerr .and.
     & abs(da - solns(naux, 2)) .le. aerr .and.
     & rdiff(sa, solns(naux, 3)) .le. aerr) then
          scompl = .true.
          return
        end if
c
c     compare compliment of "new solution" with each solution on list
c
        if (abs(solns(naux, 1) - aux1(1)) .le. aerr .and.
     & abs(solns(naux, 2) - aux1(2)) .le. aerr .and.
     & rdiff(solns(naux, 3), aux1(3)) .le. aerr) then
          scompl = .true.
          return
        end if
c
        call auxpln (solns(naux, 1), solns(naux, 2), solns(naux, 3),
     & aux2(1), aux2(2), aux2(3))
c
c     compare "new solution" with compliment of each solution on list
c
        if (abs(dd - aux2(1)) .le. aerr .and.
     & abs(da - aux2(2)) .le. aerr .and.
     & rdiff(sa, aux2(3)) .le. aerr) then
          scompl = .true.
          return
        end if
10    continue
c
      return
      end
      subroutine csort(cx, ix, n)
c
c  indirect sort routine from meissner & organick, p.352
c  stores ascending sort order of cx in array ix, leaving cx unchanged
c
      integer           ix(*)                           
c							! (output) pointer array to sorted order
      character*(*)     cx(*)                           
c							! (input) array to be sort
      integer           n                               
c							! (input) number of elements to be sorted
c
      integer           i                               
c							! loop index
      integer           j                               
c							! loop index
      integer           next                            
c							! index into cx
c
      do 10 i = 1, n
        ix(i) = i
10    continue
c
      do 40 j = 1, n - 1
        next = ix(j + 1)
        do 20 i = j, 1, -1
          if (cx(next) .gt. cx(ix(i))) goto 30
          ix(i + 1) = ix(i)
20      continue
30    ix(i + 1) = next
40    continue
c
      return
      end
c     geocen
c
c      geocen - calculate geocentric postitions, distances, and azimuths (bruce julian, usgs menlo park, ca)
c
c      the geocentric distance delta and azimuth az0 from point (lat0, lon0) to point (lat1, lon1) are calculted from
c            cos(delta) = cos(lat0')*cos(lat1')*cos(lon1 - lon0) + sin(lat0')*sin(lat1')
c            sin(delta) = sqrt(a*a + b*b)
c            tan(az0) = a/b
c
c      where
c            a = cos(lat1')*sin(lon1-lon0)
c            b = cos(latn0')*sin(lat1') - sin(lat0')*cos(lat1')*cos(lon1 - lon0)
c            lat0', lat1' = geocentric latitudes of points
c            lon0, lon1 = longitudes of points
c
c      the geocentric latitude lat' is gotten from the geographic latitude lat by tan(lat') = (1 - alpha)*(1 - alpha)*tan(lat),
c      where alpha is the flattening of the ellipsoid.  see function ggtogc for conversion.
c      the back azimuth is calculated by the same formulas with (lat0', lon0) and (lat1', lon1) interchanged.
c      azimuth is measured clockwise from north thru east.
c
      subroutine refpt (olat, olon)
c
      real              olat                            
c							! origin latitude in radians
      real              olon                            
c							! origin longitude in radians
      real              ct0                             
c							! sine of reference point latitude
      real              olatsv                            
c							! reference secondary point latitude
      real              olonsv                            
c							! reference secondary point longitude
      real              st0                             
c							! cosine of reference point latitude
      common /geocen/ st0, ct0, olonsv, olatsv
c
c refpt - store the geocentric coordinates of the refeernce point
c
c
      st0 = cos(olat)
      ct0 = sin(olat)
      olonsv = olon
      olatsv = olat
      return
      end
      subroutine delaz(lat, lon, delta, az0, az1, x, y)
c
c delaz - calculate the geocentric distance, azimuths
c
      real              az0                             
c							! azimuth from reference point to secondary point in radians
      real              az1                             
c							! azimuth from secondary point to reference point in radians
      real              cdelt                           
c							! sine of delta to secondary point
      real              cdlon                           
c							! cosine of difference of secondary point, reference longitude
      real              colat                           
c							! average colatitude of station
      real              ct0                             
c							! sine of reference point latitude
      real              ct1                             
c							! sine of secondary point latitude
      real              delta                           
c							! geocentric distance in degrees
      real              erad                            
c							! equatorial radius (chovitz, 1981, eos, 62, 65-67)
      real              flat                            
c							! earth flattening constant (chovitz, 1981, eos, 62, 65-67)
      real              lambda                          
c							! dummy variable
      real              lat                             
c							! latitude in radians
      real              lon                             
c							! longitude in radians
      real              olatsv                            
c							! origin latitude in radians
      real              olonsv                            
c							! reference secondary point longitude
      real              pi                              
c							! 3.14159...
      real              radius                          
c							! earth radius at colat
      real              sdelt                           
c							! cosine of delta to secondary point
      real              sdlon                           
c							! sine of difference of secondary point, reference longitude
      real              st0                             
c							! cosine of reference point latitude
      real              st1                             
c							! cosine of secondary point latitude
      real              twopi                           
c							! 2*pi
      real              x                               
c							! east-west distance (km)
      real              y                               
c							! north-south distance (km)
c
      parameter (pi = 3.1415926535897, twopi = 2.*pi)
      parameter (flat = 1./298.257, erad = 6378.137)
      parameter (lambda = flat*(2. - flat)/(1. - flat)**2)
c
      common /geocen/ st0, ct0, olonsv, olatsv
c
      ct1 = sin(lat)
      st1 = cos(lat)
      if ((ct1 - ct0) .eq. 0. .and. (lon - olonsv) .eq. 0.) then
        delta = 0.
        az0 = 0.
        az1 = 0.
      else
        sdlon = sin(lon - olonsv)
        cdlon = cos(lon - olonsv)
        cdelt = st0*st1*cdlon + ct0*ct1
        call cvrtop (st0*ct1 - st1*ct0*cdlon, st1*sdlon, sdelt, az0)
        delta = atan2(sdelt, cdelt)
        call cvrtop (st1*ct0 - st0*ct1*cdlon, -sdlon*st0, sdelt, az1)
        if (az0 .lt. 0.0) az0 = az0 + twopi
        if (az1 .lt. 0.0) az1 = az1 + twopi
      end if
      colat = pi/2. - (lat + olatsv)/2.
      radius = erad/sqrt(1.0 + lambda*cos(colat)**2)
      y = radius*delta*cos(az0)
      x = radius*delta*sin(az0)
      return
      end
       subroutine back (delta, az0, lat, lon)
c
c back - calculate geocentric coordinates of secondary point from delta, az
c
      real              az0                             
c							! azimuth from reference point to secondary point in radians
      real              cdelt                           
c							! sine of delta to secondary point
      real              ct0
c							! sine of reference point latitude
      real              ct1                             
c							! sine of secondary point latitude
      real              cz0                             
c							! cosine of azimuth to secondary point
      real              delta                           
c							! geocentric distance in degrees
      real              dlon                            
c							! azimuth in polar coordinates to secondary point ?
      real              lat                             
c							! latitude in radians
      real              lon                             
c							! longitude in radians
      real              olatsv                            
c							! reference secondary point latitude
      real              olonsv                            
c							! reference secondary point longitude
      real              pi                              
c							! 3.14159...
      real              sdelt                           
c							! cosine of delta to secondary point
      real              st0                             
c							! cosine of reference point latitude
      real              st1                             
c							! cosine of secondary point latitude
      real              twopi                           
c							! 2*pi
c
      parameter (pi = 3.1415926535897, twopi = 2.*pi)
c
      common /geocen/ st0, ct0, olonsv, olatsv
c
      sdelt = sin(delta)
      cdelt = cos(delta)
      cz0 = cos(az0)
      ct1 = st0*sdelt*cz0 + ct0*cdelt
      call cvrtop (st0*cdelt - ct0*sdelt*cz0, sdelt*sin(az0), st1, dlon)
      lat = atan2(ct1, st1)
      lon = olonsv + dlon
      if (abs(lon) .gt. pi) lon = lon - sign(twopi, lon)
c
      return
      end
      subroutine cvrtop(x, y, r, theta)
c
c cvrtop - convert from rectangular to polar coordinates (bruce julian, usgs menlo park, ca)
c
      real              x,y                             
c							! x,y rectangular coordinates
      real              r, theta                        
c							! radius, azimuth in polar coordinates
c
      r = sqrt(x*x + y*y)
      theta = atan2(y, x)
      return
      end
	real function ggtogc(lat)

c	convert from geographic to geocentric latitude (bruce r. julian, usgs menlo park, ca     13 sept 1983)

	real			lat			
c							! latitude
	real			c1			
c							! (1 - flattening)**2	

	parameter (c1 = 0.993305242609)

	ggtogc = atan2(c1*sin(lat), cos(lat))
	return
	end
      subroutine hhog (delc, eunit, jstrt, nstrt, mstrt, igood, ipaxes,
     & itaxes, gfit, ng, idst, ndst, mxdip, mxslns, mxstrk, mxrake,
     & ndelc, nphic, nlamc, phic, xlamc, bot, irslam)
c
c  performs a "hedgehog" search through coarse solutions with fits less than fitlim, identifies solutions belonging to
c  discrete localized minima, and returns strike, dip, and rake indices of solution with best fit within each minima
c  for solutions belonging to a localized minima, compute corresponding p & t axes and set nearest grid point of "paxes", 
c  "taxes" with solution number for output of confidence region to .pol file
c
      integer           mxhog                           
c							! maximum number of solutions per localized minima
c
      parameter (mxhog = 6498)                          
c							! mxdip*mxstrk*mxrake (19*19*18)
c
      integer           mxdip                           
c							! (input) maximum number of dip values in search
      integer           mxrake                          
c							! (input) maximum number of rake values in search
      integer           mxslns                          
c							! (input) maximum number or multiple solutions permitted
      integer           mxstrk                          
c							! (input) maximum number or strike values in search
      real              bot(mxdip,mxstrk,mxrake)        
c							! (input) sum of product of observed and predicted weights
      real              delc(mxdip)                     
c							! (output) fault dip angle for coarse search
      integer           eunit                           
c							! (input) logical unit # of output of error messages
      real              gfit(mxdip*mxstrk*mxrake)       
c							! (input) contains fits of solutions in igood
      integer           idst(mxslns,5)                  
c							! (output) 1-3=indices of best fitting solutions in each localized minima
      integer           igood(mxdip*mxstrk*mxrake,4)    
c							! (input) indices of solutions with "good" fits determined by coarse search.
      integer           ipaxes(73,19)                   
c							! (output) distinct soltn # of p-axes 90% conf region 
      integer           irslam                          
c							! (input) flag: (0)1=(no) restricted coarse search range for rake angle 
      integer           itaxes(73,19)                   
c							! (output) distinct soltn # of p-axes 90% conf region 
      integer           jstrt                           
c							! (input) dip index of best solution from coarse search
      integer           mstrt                           
c							! (input) rake index of best solution from coarse search
      integer           ndelc                           
c							! (input) number of increments of dip in coarse search
      integer           ndst                            
c							! (output number of solutions in idst
      integer           ng                              
c							! (input) number of solutions in igood
      integer           nlamc                           
c							! (input) number of increments of rake in coarse search
      integer           nphic                           
c							! (input) number of increments of strike in coarse search
      integer           nstrt                           
c							! (input) strike index of best solution from coarse search
      real              phic(mxstrk)                   
c							! (input) fault strike angle in degrees for coarse search
      real              xlamc(mxrake)                   
c							! (input) fault rake angle in degrees for coarse search
c
      real              best                            
c							! largest bot for solutions with fit=fitmin (ie. ties)
      real              da1                             
c							! dip angle in degrees of priciple plane
      real              da2                             
c							! dip angle in degrees of auxilliary plane
      real              dd1                             
c							! dip directions in degrees of priciple plane
      real              dd2                             
c							! dip directions in degrees of auxilliary plane
      logical           first                           
c							! (input) flag: true=first time into subroutine 
      real              fmin                            
c							! smallest fit value within set of solutions comprising a localized minima
      integer           ic                              
c							! number of solutions used as center point for expansion
      integer           icach(mxhog)                    
c							! pointer array indices of igood
      integer           ict                             
c							! total number of solutions in a hedgehog
      integer           ig                              
c							! index over igood
      integer           ik                              
c							! loop index over icach
      integer           indxpa                          
c							! index of nearest p-axis azimuth grid element 
      integer           indxpp                          
c							! index of nearest p-axis plungle grid element 
      integer           indxta                          
c							! index of nearest t-axis azimuth grid element 
      integer           indxtp                          
c							! index of nearest t-axis plungle grid element 
      integer           j0                              
c							! dip index of center point for expansion
      integer           jj                              
c							! dip index of nearby solution to centerpoint
      integer           m0                              
c							! rake index of center point for expansion
      integer           mm                              
c							! rake index of nearby solution to centerpoint
      integer           mmx                             
c							! rake index of nearby solution to centerpoint
      integer           n0                              
c							! strike index of center point for expansion
      integer           nhh                             
c							! hedgehog index
      integer           nn                              
c							! strike index of nearby solution to centerpoint
      real              pain                            
c							! angle of incidence of p axis (deg)
      real              paz                             
c							! azimuth of p axis (deg)
      real              pi                              
c							! pi
      real              rad                             
c							! conversion from degrees to radians
      real              sa1                             
c							! slip angle in degrees of priciple plane
      real              sa2                             
c							! slip angle in degrees of auxilliary plane
      real              tain                            
c							! angle of incidence of t axis (deg)
      real              taz                             
c							! azimuth of t axis (deg)
c
      data first /.true./
      save first, pi, rad
c
      if (first) then
        first = .false.
        pi = 4.*atan(1.0)
        rad = pi/180.
      end if
c
      nhh = 1
      ic = 0
      ict = 0
      j0 = jstrt
      n0 = nstrt
      m0 = mstrt
c
c expand about (j0, n0, m0) for nearest neighbors
c
20    do 90 jj = j0 - 1, j0 + 1
        if (jj .eq. 0 .or. jj .gt. ndelc) goto 90
        do 80 nn = n0 - 1, n0 + 1
          if (nn .eq. 0 .or. nn .gt. nphic) goto 80
          do 70 mmx = m0 - 1, m0 + 1
            if ((mmx .eq.0 .or. mmx .gt. nlamc) .and. irslam .eq.1) then
              goto 70
            else if (mmx .eq. 0) then
              mm = nlamc 
            else if (mmx .gt. nlamc) then
              mm = 1
            else 
              mm = mmx
            end if
c
c look up each solution in igood. if found, annotate it with the current value of nhh
c
            do 60 ig = 1, ng
              if (igood(ig, 1) .eq. jj .and. igood(ig, 2) .eq. nn
     & .and. igood(ig, 3) .eq. mm .and. igood(ig, 4) .eq. 0) then
                igood(ig, 4) = nhh
c
c check to see if this solution is already in a cache
c
                if (ict .gt. 0) then
                  do 50 ik = 1, ict
                    if (icach(ik) .eq. ig) goto 70
50                continue
                end if
c
c store this solution in cache
c
                ict = ict + 1
                if (ict .gt. mxhog) then
                  write (eunit, *) '***** hhog error: number of solut'//
     & 'ions within hedgehog exceeds ', mxhog, ' *****'
                  stop
                end if
                icach(ict) = ig
              end if
60          continue
70        continue
80      continue
90    continue
c
c select next solution within current hedgehog as starting point for expansion
c
      ic = ic + 1
      if (ic .le. ict) then
        j0 = igood(icach(ic), 1)
        n0 = igood(icach(ic), 2)
        m0 = igood(icach(ic), 3)
        goto 20
      else
c
c finished processing cache for current hedgehog
c
        ict = 0
        ic = 0
        nhh = nhh + 1
        if (nhh .gt. mxslns) then
          print *, '***** hhog error: number of multiple solutions ex'//
     & 'ceeds ', mxslns, ' *****'
          stop
        end if
c
c get next solution from igood that does not already belong to a minima
c
        do 100 ig = 1, ng
          if (igood(ig, 4) .eq. 0) then
            j0 = igood(ig, 1)
            n0 = igood(ig, 2)
            m0 = igood(ig, 3)
            goto 20
          end if
100     continue
      end if
c
c identify solution corresponding to fit minimum within each hedgehog
c
      do 130 ndst = 1, nhh - 1
        fmin = 1.
        best = 0.
        do 120 ig = 1, ng
          if (igood(ig, 4) .eq. ndst) then
            if (gfit(ig) .lt. fmin) fmin = gfit(ig)
c
c find nearest grid cell in p, t axes arrays for 90% confidence region output 
c
            da1 = delc(igood(ig,1))
            dd1 = phic(igood(ig, 2)) + 90.
            sa1 = xlamc(igood(ig, 3))
            call auxpln (dd1, da1, sa1, dd2, da2, sa2)
            call tandp (pain, tain, paz, taz, da1, da2, dd1, dd2, sa1,
     & sa2, pi, rad)
            indxpa = nint(paz/5.) + 1
            indxpp = nint(pain/5.) + 1
            ipaxes(indxpa, indxpp) = ndst
            indxta = nint(taz/5.) + 1
            indxtp = nint(tain/5.) + 1
            itaxes(indxta, indxtp) = ndst
          end if
120     continue
c
c best solution has largest bot
c
        do 125 ig = 1, ng
          if (igood(ig, 4) .eq. ndst) then
            if (gfit(ig) .eq. fmin .and. 
     & bot(igood(ig, 1), igood(ig, 2), igood(ig, 3)) .gt. best) then
              idst(ndst, 1) = igood(ig, 1)
              idst(ndst, 2) = igood(ig, 2)
              idst(ndst, 3) = igood(ig, 3)
              best = bot(igood(ig, 1), igood(ig, 2), igood(ig, 3)) 
            end if
          end if
125     continue
130   continue
      ndst = nhh - 1
c
      return
      end
      subroutine range (prompt, value, vmin, vmax)

      character         prompt*(*)                      
c							! (input) prompt string
      real              value                           
c							! (input/output) value
      real              vmax                            
c							! (input) maximum value
      real              vmin                            
c							! (input) minimum value

      real              askr                            
c							! function

10    value = askr (prompt, value)
      if (value .lt. vmin) then
        write (*, 100) vmin
100     format (/' value must be greater than ', g11.5, '; try again')
        goto 10
      else if (vmax .gt. 0. .and. value .gt. vmax) then
        write (*, 110) vmax
110     format (/' value must be less than ', g11.5, '; try again')
        goto 10
      end if
      return
      end
      integer function jask (prompt, idflt)
c
c  jask prompts then reads an integer value from the terminal.
c  the default value is returned on a cr response.
c
      integer           idflt                           
c							! default supplied on carriage return and displayed in prompt
      character         prompt*(*)                      
c							! prompt string

      character         temp*20                         
c							! scratch
      integer           i                               
c							! loop index
      integer           leng                            
c							! function
      integer           ounit
c                                                       ! logical unit for output 

c      parameter (ounit = 0)
      parameter (ounit = 6)						! VAX/VMS
      write (temp, 10) idflt
10    format (i20)
      do 20 i = 1, 20
        if (temp(i:i) .ne. ' ') goto 30
20    continue
30    write (ounit, 40) prompt, temp(i:20)
40    format (1x, a, ' [cr = ', a, ']? ', $)
      read (5, '(a)', err = 30, end = 50) temp
      if (leng(temp) .gt. 0) then
        read (temp, *, err = 30) jask
      else
        jask = idflt
      end if
50    return
      end

      integer function leng (string)
c
c the non-blank length of string whose physical length is maxlen
c (returns the position of the last non-blank character)
c
      character         string*(*)                      
c							! string
c
      integer           i                               
c							! character position
      integer           maxlen                          
c							! length of string

      maxlen = len(string)
      do 10 i = maxlen,1,-1
        if (string(i:i) .ne. ' ') goto 20
10    continue
      i = 0
20    leng = i
      return
      end
      subroutine params (mxqual, mxstat, ddelc, ddelf, del0c, del1c,
     & distmx, dlamc, dlamf, dpdr0c, dpdr1c, dphic, dphif, erate,
     & filnm1, filnm2, filnm3, filnm4, filnm5, filnm6, fmagmn, iamwt,
     & ifin, infmt, ittl, irep, kilsta, lopen2, lopen3, lopen4, minobs,
     & nkil, nrev, ounit, revsta, title, xlam0c, xlam1c, icmp, ainmin,
     & ainmax, resmax, ibst, idate, ncmpnt, okcmp, macsrc, nmsrc, 
     & hndsrc, nhsrc, kilnet, revnet)

c lists current parameter settings on unit ounit

      integer           mxqual                          
c							! maximum # of qualities permitted
      integer           mxstat                          
c							! maximum # of stations permitted
      real              ainmax                          
c							! maximum permitted angle of incidence
      real              ainmin                          
c							! minimum permitted angle of incidence
      real              ddelc                           
c							! fault dip increment in degrees for coarse search
      real              ddelf                           
c							! fault dip increment in degrees for fine search
      real              del0c                           
c							! minimum value of coarse search dip range in degrees
      real              del1c                           
c							! maximum value of coarse search dip range in degrees
      real              distmx                          
c							! maximum permitted epicentral distance
      real              dlamc                           
c							! fault rake increment in degrees for coarse search
      real              dlamf                           
c							! fault rake increment in degrees for fine search
      real              dpdr0c                          
c							! minimum value of coarse search dip direction range in degrees
      real              dpdr1c                          
c							! maximum value of coarse search dip direction range in degrees
      real              dphic                           
c							! fault strike increment in degrees for coarse search
      real              dphif                           
c							! fault strike increment in degrees for fine search
      real              erate(mxqual)                   
c							! assumed weighted error rates for data, read from control card
      character*40      filnm1                          
c							! name of report output file
      character*40      filnm2                          
c							! name of extended summary output file
      character*40      filnm3                          
c							! name of solution output file
      character*40      filnm4                          
c							! name of fit function output file
      character*40      filnm5                          
c							! name of hypo input file
      character*40      filnm6                          
c							! name of control file
      real              fmagmn                          
c							! minimum permitted magnitude
      character*(*)     hndsrc(mxstat)
c                                                       ! allowable hand-timed source codes
      integer           iamwt                           
c							! flag controling amplitude weighting (0=omit, 1=use)
      integer           ibst                            
c							! flag: 0(1)=do(not) calculate multiple solutions 
      integer           icmp                            
c							! flag: 1(0)=do (not) composite data into one mechanism
      integer           idate(mxstat,2)                 
c							! date range of station reversal; 0=>open-ended
      integer           ifin                            
c							! flag: 1(0)=do (not) limit fine search to coarse search range
      integer           infmt                           
c							! input file format : 1=hypo71, 2=hypoinverse, 3=hypoellipse
                                                        
c							!                              4=hypoinverse with shadow card
      integer           irep                            
c							! flag: 1(0)=do(not) report each fps to terminal when computed 
      integer           ittl                            
c							! title option: 0=no title
      character*(*)     kilnet(mxstat)
c                                                       ! seismic network code for kilsta
      character*(*)     kilsta(mxstat)                  
c							! ignored station names
      logical           lopen2                          
c							! t if sunit open
      logical           lopen3                          
c							! t if punit open
      logical           lopen4                          
c							! t if funit open
      character*(*)     macsrc(mxstat)
c                                                       ! allowable machine source codes
      integer           minobs                          
c							! minimum number of observations required
      integer           ncmpnt
c                                                       ! number of allowable component codes (okcmp)
      integer           nhsrc
c                                                       ! number of allowed hand-timed source codes (hndsrc)
      integer           nkil                            
c							! number of ignored stations
      integer           nmsrc
c                                                       ! number of allowed machine source codes (macsrc)
      integer           nrev                            
c							! number of reversed stations
      character*(*)     okcmp(mxstat)
c                                                       ! allowable component codes
      integer           ounit                           
c							! output unit #
      real              resmax                          
c							! maximum permitted p-residual
      character*(*)     revnet(mxstat)
c                                                       ! seismic network code for revsta
      character*(*)     revsta(mxstat)                  
c							! reversed station names
      character*(*)     title                           
c							! output title
      real              xlam0c                          
c							! minimum value of coarse search rake range in degrees
      real              xlam1c                          
c							! maximum value of coarse search rake range in degrees

      integer           i                              
c							! loop index
      integer           ifor1                          
c							! format statement label
      integer           ifor2                          
c							! format statement label
      integer           ifor3                          
c							! format statement label
      integer           leng                           
c							! function

      if (ounit .eq. 6) then
        assign 100 to ifor1
        assign 200 to ifor2
        assign 300 to ifor3
      else
        assign 500 to ifor1
        assign 600 to ifor2
        assign 700 to ifor3
      end if
c
c if not writing out to save file, print a header
c
      if (ounit .eq. 8) then 
        write (ounit, '(a)') ' parameter settings for fpfit'
        write (ounit, '(a)') ' -----------------------------'
      end if
      write (ounit, ifor2) 'ttl', ittl, title(1:leng(title))
      write (ounit, ifor1) 'hyp', filnm5(1:leng(filnm5))
      write (ounit, ifor1) 'out', filnm1(1:leng(filnm1))
      if (lopen2) write (ounit, ifor1) 'sum', filnm2(1:leng(filnm2))
      if (lopen3) write (ounit, ifor1) 'pol', filnm3(1:leng(filnm3))
      if (lopen4) write (ounit, ifor1) 'fit', filnm4(1:leng(filnm4))
      if (ounit .ne. 18) write (ounit, ifor1) 'jmp', 
     &filnm6(1:leng(filnm6))
      write (ounit, ifor2) 'for', infmt
      write (ounit, ifor3) 'mag', fmagmn
      write (ounit, ifor2) 'obs', minobs
      write (ounit, ifor3) 'dis', distmx
      write (ounit, ifor3) 'res', resmax
      write (ounit, ifor3) 'ain', ainmin, ainmax
      write (ounit, ifor2) 'amp', iamwt
      write (ounit, ifor2) 'bst', ibst
      write (ounit, ifor2) 'fin', ifin
      write (ounit, ifor2) 'rep', irep
      write (ounit, ifor2) 'cmp', icmp
      write (ounit, ifor3) 'hdr', (erate(i), i = 1, mxqual/2)
      write (ounit, ifor3) 'mcr', (erate(i), i = mxqual/2 + 1, mxqual)
      write (ounit, ifor3) 'dir', dpdr0c, dpdr1c, dphic, dphif
      write (ounit, ifor3) 'dip', del0c, del1c, ddelc, ddelf
      write (ounit, ifor3) 'rak', xlam0c, xlam1c, dlamc, dlamf
      if (ncmpnt .gt. 0) then
        do 3 i = 1, ncmpnt
          write (ounit, ifor2) 'chn', i, okcmp(i)
3       continue
      endif
      if (nhsrc .gt. 0) then
        do 4 i = 1, nhsrc
          write (ounit, ifor2) 'hds', i, hndsrc(i)
4       continue
      endif
      if (nmsrc .gt. 0) then
        do 5 i = 1, nmsrc
          write (ounit, ifor2) 'mcs', i, macsrc(i)
5       continue
      endif
      if (ounit .eq. 6 .and. (nkil .gt. 0 .or. nrev .gt. 0)) then
        write (6, '(/, $, a)') ' hit carriage return to continue'
cjh        read (5, '(i)', err = 6) i    ! gfortran 
           read (5, '(a4)', err = 6) i
      end if
6     if (nkil .gt. 0) then
        do 10 i = 1, nkil
          write (ounit, ifor2) 'kil', i, kilsta(i), kilnet(i)
10      continue
      end if
      if (nrev .gt. 0) then
        do 20 i = 1, nrev
          write (ounit, ifor2) 'rev', i, revsta(i), revnet(i), 
     & idate(i, 1), idate(i, 2)
20      continue
      end if
100   format (1x, a3, 1x, '''', a, '''')
200   format (1x, a3, 1x, i3, :, 1x, '''', a, '''', :, 1x, '''', a, 
     1 '''', :, 2(1x,i8))
300   format (1x, a3, 1x, 4g11.4)
500   format (a3, 1x, '''', a, '''')
600   format (a3, 1x, i3, :, 1x, '''', a, '''', :, 1x, '''', a, 
     1 '''', :, 2(1x, i8))
700   format (a3, 1x, 4g11.4)
      return
      end
      subroutine pexcf (coef, i, mxstat, u)
c
c
c calculates coefficients for determining the far-field radiation pattern of p waves from the moment-rate tensor components of a
c point source in an infinite, homogeneous, elastic medium.  the radiation pattern is normalized; to obtain particle amplitudes,
c multiply by
c
c    1.0/(4.0*pi*rho*(v**3)*r),
c
c     where:
c          rho is the density in the source region,
c          v is the p-wave speed in the source region, and
c          r is the geometric spreading factor
c            (for a homogeneous medium, this is equal to the distance
c            to the observation point.)
c
c reference:
c         aki, keiiti, and paul g. richards, quantitative seismology,
c         freeman, san francisco, 1980, equation 49.1, page 118.
c
c written by bruce julian
c
      integer           mxstat                          
c							! (input) maximum # of stations permitted
      real              coef(mxstat, 6)                 
c							! (output) excitation coefficients
      integer           i                               
c							! (input) index of station
      real              u(3)                            
c							! (input) unit vector in ray direction
c
      coef(i, 1) = u(1)*u(1)
      coef(i, 2) = 2.*u(1)*u(2)
      coef(i, 3) = u(2)*u(2)
      coef(i, 4) = 2.*u(3)*u(1)
      coef(i, 5) = 2.*u(2)*u(3)
      coef(i, 6) = u(3)*u(3)
c
      return
      end
      subroutine input (eunit, funit, iunit, punit, sunit, lopen2,
     & lopen3, lopen4, ddelc, ddelf, del0c, distmx, dlamc, dlamf,
     & dphic, dphif, erate, fmagmn, iamwt, ibst, infmt, ires, irep, 
     & kilsta, minobs, mxdip, mxqual, mxrake, mxstat, mxstrk, ndelc,
     & nkil, nlamc, nphic, nrev, phi0c, revsta, idate, irslam,
     & title, weight, xlam0c, ifin, ittl, icmp, resmax, ainmin, ainmax,
     & kilnet, revnet, dbg, ncmpnt, okcmp, macsrc, nmsrc, hndsrc, nhsrc) 

c user interface to program.  reads control parameters interactively or from file.  checks parameters and writes them to
c report file.  online help included

      integer           mxqual                          
c							! (input) maximum # of qualities permitted
      integer           mxstat                          
c							! (input) maximum # of stations permitted
      real              ainmax                          
c							! (output) maximum permitted angle of incidence
      real              ainmin                          
c							! (output) minimum permitted angle of incidence
      logical           dbg
c                                                       ! (output) true(false) = do (not) issue warning messages
      real              ddelc                           
c							! (output) fault dip increment in degrees for coarse search
      real              ddelf                           
c							! (output) fault dip increment in degrees for fine search
      real              del0c                           
c							! (output) initial fault dip angle in degrees for coarse search
      real              distmx                          
c							! (output) maximum permitted epicentral distance
      real              dlamc                           
c							! (output) fault rake increment in degrees for coarse search
      real              dlamf                           
c							! (output) fault rake increment in degrees for fine search
      real              dphic                           
c							! (output) fault strike increment in degrees for coarse search
      real              dphif                           
c							! (output) fault strike increment in degrees for fine search
      real              erate(mxqual)                   
c							! (output) assumed weighted error rates for data, read from control card
      integer           eunit                           
c							! (input) logical unit # of output of error messages
      real              fmagmn                          
c							! (output) minimum permitted magnitude
      integer           funit                           
c							! (input) logical unit # of output of fit listing for all strikes, dips
      character*(*)     hndsrc(mxstat)
c                                                       ! allowable hand-timed source codes
      integer           iamwt                           
c							! (output) flag controling amplitude weighting (0=omit, 1=use)
      integer           ibst                            
c							! (output) flag: 0(1)=do(not) calculate multiple solutions 
      integer           icmp                            
c							! (output) flag: 1(0)=do (not) composite data into one mechanism
      integer           idate(mxstat,2)                 
c							! (output) date range of station reversal; 0=>open-ended
      integer           infmt                           
c							! 1=hypo71 print listing
c							! 2=hypoellipse
c							! 3=hypoinverse 
      integer           irep                            
c							! (output) flag: 1(0)=do(not) report each fps to terminal when computed 
      integer           ires                            
c							! (output) flag: 0(1)=(un)restricted search
      integer           ifin                            
c							! (output) flag: 1(0)=do (not) limit fine search to coarse search range
      integer           ittl                            
c							! (input) title option
      integer           iunit                           
c							! (input) logical unit # of hypo listing file 
      character*(*)     kilnet(mxstat)
c                                                       ! (output) seismic network code for kilsta
      character*(*)     kilsta(mxstat)                  
c							! (output) ignored station names
      logical           lopen2                          
c							! (output) t if sunit open
      logical           lopen3                          
c							! (output) t if punit open
      logical           lopen4                          
c							! (output) t if funit open
      integer           minobs                          
c							! (output) minimum number of observations required
      character*(*)     macsrc(mxstat)
c                                                       ! (input) allowable machine source codes
      integer           mxdip                           
c							! (input) maximum # of dip increments permitted
      integer           mxrake                          
c							! (input) maximum # of rake increments permitted
      integer           mxstrk                          
c							! (input) maximum # of strike increments permitted
      integer           ncmpnt
c                                                       ! (input) number of allowable component codes (okcmp)
      integer           ndelc                           
c							! (output) number of fault dip increments for coarse search
      integer           nhsrc
c                                                       ! number of allowed hand-timed source codes (hndsrc)
      integer           nkil                            
c							! (output) number of ignored stations
      integer           nlamc                           
c							! (output) number of fault rake increments for coarse search
      integer           nmsrc
c                                                       ! number of allowed machine source codes (macsrc)
      integer           nphic                           
c							! (output) number of fault strike increments for coarse search
      integer           nrev                            
c							! (output) number of reversed stations
      character*3       okcmp(mxstat)
c                                                       ! (output) allowable component codes
      real              phi0c                           
c							! (output) initial fault strike angle in degrees for coarse search
      integer           punit                           
c							! (input) logical unit # of output of extended summary and ray parameters
      real              resmax                          
c							! (output) maximum permitted p-residual 
      character*(*)     revnet(mxstat)
c                                                       ! (output) seismic network code for revsta
      character*(*)     revsta(mxstat)                  
c							! (output) reversed station names
      integer           sunit                           
c							! (input) logical unit # of output of extended summary cards
      character*(*)     title                           
c							! (output title
      real              weight(mxqual)                  
c							! (output) weights associated with qualities
      real              xlam0c                          
c							! (output) initial fault rake angle in degrees for coarse search

      logical           askl                            
c							! function
      real              askr                            
c							! function
      character*3       cm                              
c							! command
      character*3       cmpnt
c                                                       ! component code
      integer           cunit                           
c							! logical unit # of control file
      character*24      datstr                          
c							! date
      real              ddlcdf                          
c							! default fault dip increment in degrees for coarse search
      real              ddlfdf                          
c							! default fault dip increment in degrees for fine search
      real              del1c                           
c							! maximum value of coarse search dip range in degrees
      real              dl0cdf                          
c							! default initial fault dip angle in degrees for coarse search
      real              dlmcdf                          
c							! default fault rake increment in degrees for fine search
      real              dlmfdf                          
c							! default fault rake increment in degrees for fine search
      real              dpdr0c                          
c							! minimum value of coarse search dip direction range in degrees
      real              dpdr1c                          
c							! maximum value of coarse search dip direction range in degrees
      real              dphcdf                          
c							! default fault strike increment in degrees for coarse search
      real              dphfdf                          
c							! default fault strike increment in degrees for fine search
      real              er                              
c							! summation of erate array
      integer           erflag                          
c							! error flag; non-zero indicates unable to open
      character*40      filnm1                          
c							! name of report output file; see eunit
      character*40      filnm2                          
c							! name of extended summary output file; see sunit
      character*40      filnm3                          
c							! name of solution output file; see punit
      character*40      filnm4                          
c							! name of fit function output file; see funit
      character*40      filnm5                          
c							! name of hypo input file; see iunit
      character*40      filnm6                          
c							! name of control file; see cunit
      character*40      filnmt                          
c							! full pathname of filnm5
      logical           first                           
c							! flag: t if first time into routine
      integer           i                               
c							! loop index
      integer           ichn                               
c							! array index
      integer           idy                             
c							! day
      integer           ik                              
c							! array index 
      integer           imo                             
c							! month
      integer           inp                             
c							! control input unit number
      character*80      inst                            
c							! parameters on instruction line
      integer           ios                             
c							! iostat specifier
      integer           irsdel                          
c							! flag: (0)1=(no) restricted coarse search range for dip angle 
      integer           irslam                          
c							! flag: (0)1=(no) restricted coarse search range for rake angle 
      integer           irsphi                          
c							! flag: (0)1=(no) restricted coarse search range for strike angle 
      integer           iyr                             
c							! year
      integer           jask                            
c							! function
      integer           leng                            
c							! function
      logical           linst                           
c							! t if no instruction on command line
      real              lm0cdf                          
c							! default initial fault rake angle in degrees for coarse search
      logical           lopen1                          
c							! t if eunit open
      logical           lopen5                          
c							! t if iunit open
      integer           ndlcdf                          
c							! default number of fault dip increments for coarse search
      character*2	ntcode
c							! network code (infmt 3 only)
      integer           nlmcdf                          
c							! default number of fault rake increments for coarse search
      integer           nphcdf                          
c							! default number of fault strike increments for coarse search
      real              ph0cdf                          
c							! default initial fault strike angle in degrees for coarse search
      character*1       src                           
c							! data source variable
      character*5       statn                           
c							! station name
      character*8       timstr                          
c							! time
      real              wt                              
c							! summation of weight array
      real              xlam1c                          
c							! maximum value of coarse search rake range in degrees
      real              yescnt                          
c							! # of times prompt given without response
c
c set up default grid spacing
c
      parameter (ph0cdf =  0., dl0cdf = 10., lm0cdf = -180.)
      parameter (dphcdf = 20., ddlcdf = 20., dlmcdf =   20.)
      parameter (nphcdf =   9, ndlcdf =   5, nlmcdf =    18)
      parameter (dphfdf =  5., ddlfdf =  5., dlmfdf =   10.)

      data filnm1 /'none'/
      data filnm2 /'none'/
      data filnm3 /'none'/
      data filnm4 /'none'/
      data filnm5 /'none'/
      data filnm6 /'fpfit.inp'/
      data cunit /18/
      data inp/5/
      data irsphi/0/
      data irsdel/0/
      data ntcode/'  '/
      data first/.true./

      save first, cunit, filnm1, filnm2, filnm3, filnm4, filnm5,
     & filnm6, inp, dpdr0c, dpdr1c, del1c, xlam1c, ntcode

      if (first) then
c-------------------------------------------------------------------------------
c set default grid parameters 
c-------------------------------------------------------------------------------
        dpdr0c = ph0cdf + 90.
        dpdr1c = dpdr0c + (nphcdf - 1)*dphcdf
        dphic = dphcdf
        dphif = dphfdf
        del0c = dl0cdf
        del1c = del0c + (ndlcdf - 1)*ddlcdf
        ddelc = ddlcdf
        ddelf = ddlfdf
        xlam0c = lm0cdf
        xlam1c = xlam0c + (nlmcdf - 1)*dlmcdf
        dlamc = dlmcdf
        dlamf = dlmfdf
c-------------------------------------------------------------------------------
c open & begin reading optional startup command file first time into routine
c-------------------------------------------------------------------------------
        write (*, 10)
10      format (' Fpfit uses 3-letter LOWER-CASE commands, which can be 
     &followed by', /, ' parameters in free-format, or which display cur
     &rent values & generate prompts.', /, ' Type "hel" for information
     & on available commands.')
        first = .false.
	open (cunit, file=filnm6, form='formatted',status='old',err=100)
c	open (cunit, file=filnm6, form='formatted',status='old',err=100,	! VAX/VMS version
c     1 readonly)								! VAX/VMS version
        inp = cunit
        goto 100
      end if
c-------------------------------------------------------------------------------
c read commands from terminal after a prompt, or from a file
c-------------------------------------------------------------------------------
100   if (inp .eq. 5) write (6, 110)
110   format (' yes? ', $)
c-------------------------------------------------------------------------------
c read a command line
c-------------------------------------------------------------------------------
      if (inp .eq. 5) then
        read (inp, 120, iostat = ios) cm, inst
120     format (a3, a)
      else
        read (inp, 120, iostat = ios) cm, inst
      end if
c-------------------------------------------------------------------------------
c comment
c-------------------------------------------------------------------------------
      if (cm(1:1) .eq. '#') goto 100
c-------------------------------------------------------------------------------
c reformat "@" command to "jmp" command
c-------------------------------------------------------------------------------
      if (cm(1:1) .eq. '@') then
        inst = cm(2:3)//inst
        cm = 'jmp'
      end if
      linst=inst(1:10) .eq. '          '
c-------------------------------------------------------------------------------
c return to interactive mode at end of command line
c-------------------------------------------------------------------------------
      if (ios .eq. -1) then
        if (inp .eq. cunit) then
          close (cunit)
          inp = 5
        else
          yescnt = yescnt + 1
          if (yescnt .eq. 100) then
            print *, '***** program stopped; assumed runaway batch jo
     &b *****'
            stop
          end if
        end if
        goto 100
      end if
      yescnt = 0
c-------------------------------------------------------------------------------
c set up report output file 
c-------------------------------------------------------------------------------
      if (cm .eq. 'out') then
        call readfl (inst, eunit, filnm1, 'new', erflag, 
     & 'file for report output')
        if (erflag .ne. 0) then
          if (erflag .eq. 117) then
            print *, ' "out" file already exists - try again'
          else
            print *, ' error opening "out" file - try again'
          end if
        end if
c-------------------------------------------------------------------------------
c set up summary output file
c-------------------------------------------------------------------------------
      else if (cm .eq. 'sum') then
        call readfl (inst, sunit, filnm2, 'new', erflag, 
     & 'file for extended summary output')
        if (erflag .ne. 0) then
          if (erflag .eq. 117) then
            print *, ' "sum" file already exists - try again'
          else
            print *, ' error opening "sum" file - try again'
          end if
        else if (filnm2(1:4) .ne. 'none') then
          lopen2 = .true.
        else
          lopen2 = .false.
        end if
c-------------------------------------------------------------------------------
c set up polarity output file
c-------------------------------------------------------------------------------
      else if (cm .eq. 'pol') then
        call readfl (inst, punit, filnm3, 'new', erflag, 
     & 'file for solution output')
        if (erflag .ne. 0) then
          if (erflag .eq. 117) then
            print *, ' "pol" file already exists - try again'
          else
            print *, ' error opening "pol" file - try again'
          end if
        else if (filnm3(1:4) .ne. 'none') then
          lopen3 = .true.
        else 
          lopen3 = .false.
        end if
c-------------------------------------------------------------------------------
c set up fit function output file
c-------------------------------------------------------------------------------
      else if (cm .eq. 'fit') then
        call readfl (inst, funit, filnm4, 'new', erflag, 
     & 'file for fit-function output')
        if (erflag .ne. 0) then
          if (erflag .eq. 117) then
            print *, ' "fit" file already exists - try again'
          else
            print *, ' error opening "fit" file - try again'
          end if
        else if (filnm4(1:4) .ne. 'none') then
          lopen4 = .true.
        else
          lopen4 = .false.
        end if
c-------------------------------------------------------------------------------
c set up hypocenter input file
c-------------------------------------------------------------------------------
      else if (cm .eq. 'hyp') then
        call readfl (inst, iunit, filnm5, 'old', erflag, 
     & 'file for hypocenter input')
        if(erflag.ne.0) print *, ' error opening "hyp" file - try again'
c-------------------------------------------------------------------------------
c read a save file
c-------------------------------------------------------------------------------
      else if (cm .eq. 'jmp') then
        if (linst) then
          call askc ('command file to execute', filnm6)
        else if (filnm6(1:4) .ne. 'none') then
          filnm6 = inst(1:leng(inst))
        end if
	open (cunit,file=filnm6,status='old',iostat=ios)
c       open (cunit,file=filnm6,status='old',iostat=ios,readonly)		! VAX/VMS version
        if (ios .ne. 0) then
          print *, ' error opening "jmp" file - try again'
        else
          inp = cunit
        end if
c-------------------------------------------------------------------------------
c set distance cut-off
c-------------------------------------------------------------------------------
      else if (cm .eq. 'dis') then
        if (linst) then
          distmx = askr ('epicentral distance cut-off (km)', distmx)
        else
          read (inst, *, iostat = ios) distmx
        end if
        if (ios .ne. 0 .or. distmx .le. 0) then
          distmx = 99999.
          write (6, 180) cm
180       format (' *** error in "', a3, '" parameters - try again ***')
        end if
c-------------------------------------------------------------------------------
c set debug option
c-------------------------------------------------------------------------------
      else if (cm .eq. 'dbg') then
        if (linst) then
          dbg = askl ('output warning messages (T or F)?', dbg)
        else
          read (inst, *, iostat = ios) dbg
        end if
        if (ios .ne. 0) then
          dbg = .false.
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set angle of incidence range
c-------------------------------------------------------------------------------
      else if (cm .eq. 'ain') then
        if (linst) then
          ainmin = askr ('minimum angle-of-incidence (deg)', ainmin)
          ainmax = askr ('maximum angle-of-incidence (deg)', ainmax)
        else
          read (inst, *, iostat = ios) ainmin, ainmax
        end if
        if (ios .ne. 0 .or. ainmin .lt. 0 .or. ainmin .gt. 180. .or.
     & ainmax .lt. 0 .or. ainmax .gt. 180. .or. ainmax .le. ainmin) then
          ainmin = 0.
          ainmax = 180.
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set p-residual cutoff
c-------------------------------------------------------------------------------
      else if (cm .eq. 'res') then
        if (linst) then
          resmax = askr ('p-residual cutoff (sec)', resmax)
        else
          read (inst, *, iostat = ios) resmax
        end if
        if (ios .ne. 0 ) then
          resmax = 100.
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set minimum magnitude 
c-------------------------------------------------------------------------------
      else if (cm .eq. 'mag') then
        if (linst) then
          fmagmn = askr ('minimum event magnitude', fmagmn)
        else
          read (inst, *, iostat = ios) fmagmn
        end if
        if (ios .ne. 0) then
          fmagmn = 0.
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set minimum # of first motion observations
c-------------------------------------------------------------------------------
      else if (cm .eq. 'obs') then
        if (linst) then
          minobs = jask ('minimum # of first-motion observations (>5)',
     & minobs)
        else
          read (inst, *, iostat = ios) minobs
        end if
        if (ios .ne. 0 .or. minobs .lt. 6) then
          minobs = 15
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set hypocenter input format
c-------------------------------------------------------------------------------
      else if (cm .eq. 'for') then
        if (linst) then
          print *, '1=hypo71 print listing'
          print *, '2=hypoellipse'
          print *, '3=hypoinverse archive'
          infmt = jask ('option', infmt)
        else
          read (inst, *, iostat = ios) infmt
        end if
        if (ios .ne. 0 .or. (infmt. lt. 1 .or. infmt .gt. 3)) then
          infmt = 1
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set flag controling amplitude weighting 
c-------------------------------------------------------------------------------
      else if (cm .eq. 'amp') then
        if (linst) then
          print *, '0=omit amplitude weighting'
          print *, '1=weighted data by p-radiation amplitude'
          iamwt = jask ('option', iamwt)
        else
          read (inst, *, iostat = ios) iamwt
        end if
        if (ios .ne. 0 .or. (iamwt .ne. 0 .and. iamwt .ne. 1)) then
          iamwt = 0
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set flag controling calculation of multiple solutions
c-------------------------------------------------------------------------------
      else if (cm .eq. 'bst') then
        if (linst) then
          print *, '0=search for multiple solutions'
          print *, '1=output only best solution'
          ibst = jask ('option', ibst)
        else
          read (inst, *, iostat = ios) ibst
        end if
        if (ios .ne. 0 .or. (ibst .ne. 0 .and. ibst .ne. 1)) then
          ibst = 0
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set flag to montior execution progress at terminal 
c-------------------------------------------------------------------------------
      else if (cm .eq. 'rep') then
        if (linst) then
          print *, '0=go about computation silently'
          print *, '1=report solutions to terminal when computed'
          irep = jask ('option', irep)
        else
          read (inst, *, iostat = ios) irep
        end if
        if (ios .ne. 0 .or. (irep .ne. 0 .and. irep .ne. 1)) then
          irep = 0
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set hand-picked error rates
c-------------------------------------------------------------------------------
      else if (cm .eq. 'hdr') then
        if (linst) then
          print *, 'assign error rates to p-wt code in percent/100.'
          print *, 'e.g., 0=0% first-motion error rate (ie., perfect dat
     &a)'
          print *, '      1=100% first-motion error rate (ie., always wr
     &ong)'
          do 190 i = 1, mxqual/2
            write (inst, '(a14, i1)') 'p=weight code ', i - 1
            erate(i) = askr (inst(1:15), erate(i))
190       continue
        else
          read (inst, *, iostat = ios) (erate(i), i = 1, mxqual/2)
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
        else 
          do 200 i = 1, mxqual/2
            if (erate(i) .lt. 0. .or. erate(i) .gt. 1) write (6, 195) 
     & 'invalid hand-timed error rate of ', erate(i), ' for code ', i -1
195         format ('0', a, e13.6, a, i1)
200       continue
        end if
c-------------------------------------------------------------------------------
c set machine-picked error rates
c-------------------------------------------------------------------------------
      else if (cm .eq. 'mcr') then
        if (linst) then
          print *, 'assign error rates to p-wt code in percent/100.'
          print *, 'e.g., 0=0% first-motion error rate (ie., perfect dat
     &a)'
          print *, '      1=100% first-motion error rate (ie., always wr
     &ong)'
          do 210 i = mxqual/2 + 1, mxqual
            write (inst, '(a14, i1)') 'p=weight code ', i - mxqual/2 - 1
            erate(i) = askr (inst(1:15), erate(i))
210       continue
        else
          read (inst,*,iostat= ios) (erate(i), i = mxqual/2 + 1, mxqual)
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
        else 
          do 220 i = mxqual/2 + 1, mxqual
            if (erate(i) .lt. 0. .or. erate(i) .gt. 1) write (6, 195)
     & 'invalid machine-timed error rate of ', erate(i), ' for code ',
     & i - mxqual/2 - 1
220       continue
        end if
c-------------------------------------------------------------------------------
c set allowable hand sources
c-------------------------------------------------------------------------------
      else if (cm .eq. 'hds') then
        hndsrc(nhsrc + 1) = '-'
        if (linst) then
          src = '-'
          ichn = nhsrc + 1
          ichn = jask ('source # ', ichn)
          if (ichn .gt. 0) then
            if (ichn .gt. nhsrc) ichn = nhsrc + 1
            if (ichn .le. nhsrc) src = hndsrc(ichn)
            call askc ('1-letter "hand" data source code', src)
          end if
        else
          read (inst, *, iostat = ios) ichn, src
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
        else
          if (src .eq. '-' .and. (ichn .le. nhsrc)) then
            hndsrc(ichn) = '-'
          else if (src .ne. '-') then
            hndsrc(ichn) = src
            if (ichn .gt. nhsrc) nhsrc = ichn
          end if
        end if
c-------------------------------------------------------------------------------
c set allowable machine sources
c-------------------------------------------------------------------------------
      else if (cm .eq. 'mcs') then
        macsrc(nmsrc + 1) = '-'
        if (linst) then
          src = '-'
          ichn = nmsrc + 1
          ichn = jask ('source # ', ichn)
          if (ichn .gt. 0) then
            if (ichn .gt. nmsrc) ichn = nmsrc + 1
            if (ichn .le. nmsrc) src = macsrc(ichn)
            call askc ('1-letter "machine" data source code', src)
          end if
        else
          read (inst, *, iostat = ios) ichn, src
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
        else
          if (src .eq. '-' .and. (ichn .le. nmsrc)) then
            macsrc(ichn) = '-'
          else if (src .ne. '-') then
            macsrc(ichn) = src
            if (ichn .gt. nmsrc) nmsrc = ichn
          end if
        end if
c-------------------------------------------------------------------------------
c kill a station
c-------------------------------------------------------------------------------
      else if (cm .eq. 'kil') then
        kilsta(nkil + 1) = '-----'
        if (linst) then
          statn = 'none '
          ik = nkil + 1
          ik = jask ('station number', ik)
          if (ik .gt. 0) then
            if (ik .gt. nkil) ik = nkil + 1
            if (ik .le. nkil) statn = kilsta(ik)
            call askc ('(upto) 5-letter station name', statn)
            call askc 
     & ('2-letter network code (hypoinverse only; otherwise blank)', 
     & ntcode)
          end if
        else
          read (inst, *, iostat = ios) ik, statn, ntcode
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
        else
          if (statn .eq. '-----' .and. (ik .le. nkil)) then
            kilsta(ik) = '-----'
          else if (statn .ne. '-----') then
            kilsta(ik) = statn
            kilnet(ik) = ntcode
            if (ik .gt. nkil) nkil = ik
          end if
        end if
c-------------------------------------------------------------------------------
c reverse a station
c-------------------------------------------------------------------------------
      else if (cm .eq. 'rev') then
        revsta(nrev + 1) = '-----'
        if (linst) then
          statn = '-----'
          ik = nrev + 1
          ik = jask ('station number', ik)
          if (ik .gt. 0) then
            if (ik .gt. nrev) ik = nrev + 1
            if (ik .le. nrev) statn = revsta(ik)
            call askc ('(upto) 5-letter station name', statn)
            if (statn .ne. '-----') then
              call askc 
     & ('2-letter network code (hypoinverse only; otherwise blank)', 
     & ntcode)
              idate(ik, 1) = jask(
     1 'beginning date of reversal (eg., 19880531; 0=open-ended', 
     2 idate(ik, 1))
              idate(ik, 2) = jask(
     1 'ending date of reversal (eg., 19880531;  0=open-ended', 
     2 idate(ik, 2))
            end if
          end if
        else
          read (inst, *, iostat = ios) ik, statn, ntcode, idate(ik, 1),
     & idate(ik, 2)
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
        else
          if (statn .eq. '-----' .and. (ik .le. nrev)) then
            revsta(ik) = '-----'
          else if (statn .ne. '-----') then
            revsta(ik) = statn
            revnet(ik) = ntcode
            if (ik .gt. nrev) nrev = ik
            do 222 i = 1, 2
              if (idate(ik, i) .ne. 0) then
                iyr = idate(ik, i)/10000
                imo = idate(ik, i)/100 - iyr*100
                idy = idate(ik, i) - iyr*10000 - imo*100
                if ((imo .le. 0 .or. imo .gt. 12) .or. 
     & (idy .le. 0 .or. idy .gt. 31)) then
                  if (i .eq. 1) then
                    write (6, 221) 'begin', statn
                  else
                    write (6, 221) 'end', statn
                  endif
221               format (' invalid ', a, ' date on reversed station "',
     & a, '"; try again')
                  idate(ik, i) = 0
                end if
              end if
222         continue
          end if
        end if
c-------------------------------------------------------------------------------
c set allowable component codes
c-------------------------------------------------------------------------------
      else if (cm .eq. 'chn') then
        okcmp(ncmpnt + 1) = '---'
        if (linst) then
          cmpnt = '---'
          ichn = ncmpnt + 1
          ichn = jask ('channel code number', ichn)
          if (ichn .gt. 0) then
            if (ichn .gt. ncmpnt) ichn = ncmpnt + 1
            if (ichn .le. ncmpnt) cmpnt = okcmp(ichn)
            call askc ('3-letter station channel code', cmpnt)
          end if
        else
          read (inst, *, iostat = ios) ichn, cmpnt
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
        else
          if (cmpnt .eq. '---' .and. (ichn .le. ncmpnt)) then
            okcmp(ichn) = '---'
          else if (cmpnt .ne. '---') then
            okcmp(ichn) = cmpnt
            if (ichn .gt. ncmpnt) ncmpnt = ichn
          end if
        end if
c-------------------------------------------------------------------------------
c save the current selection parameters in a file
c-------------------------------------------------------------------------------
      else if (cm .eq. 'sav') then
        if (linst .or. inst(1:4) .eq. 'e   ') then
          call askc ('file for save parameters', filnm6)
        else
          read (inst, '(a)') filnm6
        end if
        if (filnm6(1:4) .eq. 'none') then
          print *, 'no filename entered - try again'
          goto 100
        end if
	open (cunit,file=filnm6,form='formatted',status='unknown',
     1 iostat=erflag)
c       open (cunit, file = filnm6, form='formatted', status='new',		! VAX/VMS version
c    1 iostat = erflag, carriagecontrol = 'list')				! VAX/VMS version
        if (erflag .ne. 0) then
          print *, ' error opening "sav" file - try again'
          goto 100
        end if
        call params (mxqual, mxstat, ddelc, ddelf, del0c, del1c,
     & distmx, dlamc, dlamf, dpdr0c, dpdr1c, dphic, dphif, erate,
     & filnm1, filnm2, filnm3, filnm4, filnm5, filnm6, fmagmn, iamwt,
     & ifin, infmt, ittl, irep, kilsta, lopen2, lopen3, lopen4, minobs,
     & nkil, nrev, cunit, revsta, title, xlam0c, xlam1c, icmp, ainmin,
     & ainmax, resmax, ibst, idate, ncmpnt, okcmp, macsrc, nmsrc, 
     & hndsrc, nhsrc, kilnet, revnet)
        close (cunit)
c-------------------------------------------------------------------------------
c show current selection parameters 
c-------------------------------------------------------------------------------
      else if (cm .eq. 'sho') then
        call params (mxqual, mxstat, ddelc, ddelf, del0c, del1c,
     & distmx, dlamc, dlamf, dpdr0c, dpdr1c, dphic, dphif, erate,
     & filnm1, filnm2, filnm3, filnm4, filnm5, filnm6, fmagmn, iamwt,
     & ifin, infmt, ittl, irep, kilsta, lopen2, lopen3, lopen4, minobs,
     & nkil, nrev, 6, revsta, title, xlam0c, xlam1c, icmp, ainmin,
     & ainmax, resmax, ibst, idate, ncmpnt, okcmp, macsrc, nmsrc, 
     & hndsrc, nhsrc, kilnet, revnet)
c-------------------------------------------------------------------------------
c set title
c-------------------------------------------------------------------------------
      else if (cm .eq. 'ttl') then
        if (linst) then
          print *, 'default title has form "hypo-file:date-of-computaton
     &"'
          print *, '0=no title'
          print *, '1=default title (hypo filename + date)'
          print *, '2=user-supplied title'
          ittl = jask ('option', ittl)
          if (ittl .eq. 0) then
            title = ' '
          else if (ittl .eq. 2) then
            call askc ('enter title (upto 80 char)', title)
          end if
        else
          read (inst, *, iostat = ios) ittl, title
        end if
        if (ios .ne. 0 .or. (ittl .lt. 0 .or. ittl .gt. 2)) then
          ittl = 1
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c restrict strike range
c-------------------------------------------------------------------------------
      else if (cm .eq. 'dir') then
        irsphi = 1
        if (linst) then
          print *, 'specify dip direction as downdip azimuth in degrees,
     & clockwise from north'
          call range ('minimum value of coarse search range', dpdr0c,
     & -180., 540.)
          call range ('maximum value of coarse search range', dpdr1c,
     & -180., 540.)
          call range ('increment in coarse search range', dphic, 1.,-1.)
          call range ('increment in fine search range', dphif, 1., -1.)
        else
          read (inst, *, iostat = ios) dpdr0c, dpdr1c, dphic, dphif
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
          irsphi = 0
        else 
          if (dpdr1c .lt. dpdr0c) dpdr1c = dpdr1c + 360.
          nphic = int((dpdr1c - dpdr0c)/dphic) + 1
          phi0c = dpdr0c - 90.
          if (nphic .gt. mxstrk) then
            write (*, 300) mxstrk
300         format (/,' (total range)/(coarse increment) > array dimensi
     &on (=', i2, ')',/, ' either decrease range or increase coarse inte
     &rval (ie., try again)')
            irsphi = 0
          else if (nphic .le. 0) then
            write (*, 310)
310         format(/' (total range)/(coarse increment) <= 0; try again')
            irsphi = 0
          else if (inp .eq. 5) then
            print *, 'number of coarse strike increments =', nphic
          else if ((phi0c .eq. ph0cdf) .and. (dphic .eq. dphcdf) .and.
     & (dphif .eq. dphfdf) .and. (nphic .eq. nphcdf)) then
            irsphi = 0
          end if
        end if
c-------------------------------------------------------------------------------
c restrict dip range
c-------------------------------------------------------------------------------
      else if (cm .eq. 'dip') then
        irsdel = 1
        if (linst) then
          print *, 'specify dip angle down from horizontal in degrees'
          call range ('minimum value of coarse search range', del0c, 0.,
     & 180.)
          call range ('maximum value of coarse search range', del1c, 0.,
     & 180.)
          call range ('increment in coarse search range', ddelc, 1.,-1.)
          call range ('increment in fine search range', ddelf, 1., -1.)
        else
          read (inst, *, iostat = ios) del0c, del1c, ddelc, ddelf
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
          irsdel = 0
        else 
          ndelc = int((del1c - del0c)/ddelc) + 1
          if (ndelc .gt. mxdip) then
            write (*, 300) mxdip
            irsdel = 0
          else if (ndelc .le. 0) then
            write (*, 310)
            irsdel = 0
          else if (inp .eq. 5) then
            print *, 'number of coarse dip increments =', ndelc
          else if ((del0c .eq. dl0cdf) .and. (ddelc .eq. ddlcdf) .and.
     & (ddelf .eq. ddlfdf) .and. (ndelc .eq. ndlcdf)) then
            irsdel = 0
          end if
        end if
c-------------------------------------------------------------------------------
c restrict rake range
c-------------------------------------------------------------------------------
      else if (cm .eq. 'rak') then
        irslam = 1
        if (linst) then
          print *, 'specify rake angle in degrees as follows:'
          print *, '0=left lateral, 90=reverse, -90=normal, +-180=right 
     &lateral'
          call range ('minimum value of coarse search range', xlam0c,
     & -360., 360.)
          call range ('maximum value of coarse search range', xlam1c,
     & -360., 360.)
          call range ('increment in coarse search range', dlamc, 1.,-1.)
          call range ('increment in fine search range', dlamf, 1., -1.)
        else
          read (inst, *, iostat = ios) xlam0c, xlam1c, dlamc, dlamf
        end if
        if (ios .ne. 0) then
          write (6, 180) cm
          irslam = 0
        else 
          nlamc = int((xlam1c - xlam0c)/dlamc) + 1
          if (nlamc .gt. mxrake) then
            write (*, 300) mxrake
            irslam = 0
          else if (nlamc .le. 0) then
            write (*, 310)
            irslam = 0
          else if (inp .eq. 5) then
            print *, 'number of coarse rake increments =', nlamc
          else if ((xlam0c .eq. lm0cdf) .and. (dlamc .eq. dlmcdf) .and.
     & (dlamf .eq. dlmfdf) .and. (nlamc .eq. nlmcdf)) then
            irslam = 0
          end if
        end if
c-------------------------------------------------------------------------------
c set flag controling whether fine search restricted to restricted coarse search range
c-------------------------------------------------------------------------------
      else if (cm .eq. 'fin') then
        if (linst) then
          print *, '0=fine search range not limited to restricted coarse
     & search range'
          print *, '1=fine search range limited to restricted coarse sea
     &rch range'
          ifin = jask ('option', ifin)
        else
          read (inst, *, iostat = ios) ifin
        end if
        if (ios .ne. 0 .or. (ifin .ne. 0 .and. ifin .ne. 1)) then
          ifin = 1
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c set flag for computing composite mechanisms
c-------------------------------------------------------------------------------
      else if (cm .eq. 'cmp') then
        if (linst) then
          print *, '0=compute separate mechanisms for each earthquake'
          print *, '1=compute a composite mechanism for all earthquakes 
     &in file'
          icmp = jask ('option', icmp)
        else
          read (inst, *, iostat = ios) icmp
        end if
        if (ios .ne. 0 .or. (icmp .ne. 0 .and. icmp .ne. 1)) then
          icmp = 0
          write (6, 180) cm
        end if
c-------------------------------------------------------------------------------
c compute fault-plane solutions
c-------------------------------------------------------------------------------
      else if (cm .eq. 'fps') then
c
c check if required files are properly opened
c
        inquire (eunit, opened = lopen1)
        inquire (iunit, opened = lopen5, name = filnmt)
        if (.not. lopen1) then
          print *, 'error: report output file not open - type "out"'
          goto 100
        end if
        if (.not. lopen5) then
          print *,'error: hypocenter input file not open - type "hyp"'
          goto 100
        end if
        if (.not. (lopen2 .or. lopen3 .or. lopen4)) then
          print *,'error: no fault-plane solution output files opened'
          print *, '       type "sum", "pol", or "fit"'
          goto 100
        end if
c
c convert estimated error rates to weighting factors
c perfect error rates are tempered to a modest .001 to prevent infinite weights
c
        wt = 0.
        er = 0.
        do 225 i = 1, mxqual
          if (erate(i) .lt. 0.5) then
            if (erate(i) .lt. 0.001) then
              weight(i) = 29.6386
            else
              weight(i) = 1./sqrt(erate(i) - erate(i)*erate(i)) - 2.
            end if
          else
            weight(i) = 0.0
          end if
c
c check erates for likely errors 
c
          wt = wt + weight(i)
          er = er + erate(i)

          if (i .ne. 1) then    ! fix by jh to avoiid array out of bounds
c                                 wnen i=1
          if ((i .ne. 1 .and. i .ne. mxqual/2 + 1) .and.
     & (erate(i) .eq. 0.) .and. (erate(i) .lt. erate(i - 1))) then
            if (i .lt. mxqual/2 + 1) then
              write (6, 224) i - 1, 'hand-read', 'hdr'
            else
              write (6, 224) i - mxqual/2 - 1, 'machine-read', 'mcr'
            end if
224         format (/, ' error: the error rate for ', i1,
     & '-weight ', a, ' data is zero', /, '        type "', a, '"')
            goto 100
        end if
        endif
225     continue
        if (wt .eq. 0.) then
          print *, 'error: all error rates exceed 0.5'
          print *, '       type "hdr" or "mcr"'
          goto 100
        end if
        if (er .eq. 0.) then
          print *,'error: all error rates = 0 (an unrealistic estimate)'
          print *, '       type "hdr" or "mcr"'
          goto 100
        end if
        if (ittl .eq. 1) then
c 
c get current time.  This is a machine-dependent subroutine call
c
          call fdate(datstr)
c         call date(datstr)                                                     !VAX/VMS version
c         call time (timstr)                                                    !VAX/VMS version
          title = filnmt//'   '//datstr(1:24)
c         title = filnmt//'   '//datstr(1:9)//' '//timstr			!VAX/VMS version

        end if
        if (lopen3) write (punit, '(1x, a)') title
c
c if search range not specified, set to default
c
        if (irsphi .eq. 0) then
          phi0c = ph0cdf
          dphic = dphcdf
          dphif = dphfdf
          nphic = nphcdf
          dpdr0c = mod(phi0c + 90., 360.)
          dpdr1c = mod(dpdr0c + (nphic - 1)*dphic, 360.)
        else
          ires = 1
        end if
        if (irsdel .eq. 0) then
          del0c = dl0cdf
          ddelc = ddlcdf
          ddelf = ddlfdf
          ndelc = ndlcdf
          del1c = del0c + (ndelc - 1)*ddelc
        else
          ires = 1
        end if
        if (irslam .eq. 0) then
          xlam0c = lm0cdf
          dlamc = dlmcdf
          dlamf = dlmfdf
          nlamc = nlmcdf
          xlam1c = xlam0c + (nlamc - 1)*dlamc
        else
          ires = 1
        end if
        call params (mxqual, mxstat, ddelc, ddelf, del0c, del1c,
     & distmx, dlamc, dlamf, dpdr0c, dpdr1c, dphic, dphif, erate,
     & filnm1, filnm2, filnm3, filnm4, filnm5, filnm6, fmagmn, iamwt,
     & ifin, infmt, ittl, irep, kilsta, lopen2, lopen3, lopen4, minobs,
     & nkil, nrev, eunit, revsta, title, xlam0c, xlam1c, icmp, ainmin,
     & ainmax, resmax, ibst, idate, ncmpnt, okcmp, macsrc, nmsrc, 
     & hndsrc, nhsrc, kilnet, revnet)
        write (eunit, *)
        if (inp .eq. cunit) then
          close (cunit)
          inp = 5
        end if
        return
c-------------------------------------------------------------------------------
c display a page of help text 
c-------------------------------------------------------------------------------
      else if (cm .eq. 'hel' .or. cm .eq. '?  ') then
        write (6, 230)
230        format (/'  --- i/o commands ---'/
     & '  hyp - set file name of hypocenter input'/
     & '  for - set hypocenter input format'/
     & '  out - set file name of report output'/
     & '  sum - set file name of extended summary output'/
     & '  pol - set file name of solution and first-motion output'/
     & '  fit - set file name of fit-function output'/
     & '  ttl - set title'/
     & '  chn - set permitted 3-letter channel codes'/
     & '  dbg - report non-fatal warnings to "out" file (for=3 only)'/
     & '  rep - set option to monitor execution progress at terminal')
        write (6, 240)
240        format ('  --- do something ---'/
     & '  fps - compute to fault plane solutions'/
     & '  sho - display current command settings'/
     & '  sto - stop the program'/
     & '  jmp - execute a "sav" command file'/
     & '  sav - save current command settings in a file'/
     & '  @command_file - same as "jmp" command'/
     & '  #string - any line beginning with an "#" is considered a comme
     &nt line'//
     & '  --- for information on commands that control solution type "mo
     &r" ---'/)
      else if (cm .eq. 'mor') then
        write (6, 250)
250        format (//'  --- solution control ---'/
     & '  amp - set option to weight data by p-radiation amplitude funct
     &ion'/
     & '  bst - set option to search for multiple solutons'/
     & '  cmp - set option to generate a composite solution'/
     & '  mag - set minimum acceptable event magnitude'/
     & '  obs - set minimum # of p first-motions per event (ignored for 
     &composites)'/
     & '  dis - set maximum permitable distance'/
     & '  res - set maximum permitable p-residual')
        write (6,260)
260        format ('  ain - set permitted angle-of-incidence range'/
     & '  kil - set names of stations to ignore'/
     & '  rev - set names of stations with reverse polarities'/
     & '  hdr*- assign first-motion error rates to p-wt codes of hand-pi
     &cked data'/
     & '  hds - set data source codes for hand-picked data'/
     & '  mcr - assign first-motion error rates to p-wt codes of machine
     &-picked data'/
     & '  mcs - set data source codes for machine-picked data'/
     & '  dir - set restricted downdip azimuth search range'/
     & '  dip - set restricted dip angle search range'/
     & '  rak - set restricted rake angle search range'/
     & '  fin - set option to restrict fine search range to coarse searc
     &h range')
c-------------------------------------------------------------------------------
c stop program
c-------------------------------------------------------------------------------
      else if (cm .eq. 'sto') then
        if (lopen2) close (sunit)
        if (lopen3) close (punit)
        if (lopen4) close (funit)
        close (eunit)
        close (iunit)
        stop
c-------------------------------------------------------------------------------
c i give up
c-------------------------------------------------------------------------------
      
      else if (cm .ne. '   ') then
        print *, cm, ' is an unknown command - try again'
      end if
      goto 100
      end
      subroutine rdeq1 (ain, ainmin, ainmax, az, dist, distmx, eunit, 
     & event, fmagmn, iunit, kilsta, minobs, mxqual, mxstat,
     & nkil, nr, nrev, pobs, prcntx, prmk, resmax, revsta, sigmaf, stn,
     & sumwt, weight, wtobs, icmp, idate, dbg)
c
c reads hypo71 output listing. returns summary card and corresponding phase first motions, qualitites, angles of incidence,
c station names, and azimuths.  calculates standard deviation (sigmaf) of fit from estimated standard deviations of the data.
c the estimated data errors are control-file inputs; corresponding data weights are calculated in main and passed to this
c routine in the parameter "weight".
c
      integer           mxqual                          
c							! (input) maximum # of qualities permitted
      integer           mxstat                          
c							! (input) maximum # of stations permitted
      real              ain(mxstat)                     
c							! (output) ray incidence angles
      real              ainmax                          
c							! (input) maximum permitted angle of incidence
      real              ainmin                          
c							! (input) minimum permitted angle of incidence
      real              az(mxstat)                      
c							! (output) ray azimuth angles (corresponding to ain)
      logical           dbg
c                                                       ! (input) true(false) = do (not) issue warning messages
      real              dist(mxstat)                    
c							! (output) epicentral distance
      real              distmx                          
c							! (input) maximum permitted epicentral distance
      integer           eunit                           
c							! (input) logical unit # of output of error messages
      character*(*)     event                           
c							! (output) summary card
      real              fmagmn                          
c							! (input) minimum permitted magnitude
      integer           icmp                            
c							! (input) 1(0)=do (not) composite data into one mechanism; ievp on output
      integer           idate(mxstat,2)                 
c							! (input) date range of station reversal; 0=>open-ended
      integer           iunit                           
c							! (input) logical unit # of hypo71 listing file (input file)
      character*(*)     kilsta(mxstat)                  
c							! (input) ignored station names
      integer           minobs                          
c							! (input) minimum number of observations required
      integer           nkil                            
c							! (input) number of ignored stations
      integer           nr                              
c							! (output) -1=eof, 0=skip event, nr>0 => number of stations
      integer           nrev                            
c							! (input) number of reversed stations
      real              pobs(mxstat)                    
c							! (output) observed first motion polarities; .5=compression, -.5=dilatation
      real              prcntx                          
c							! (output) % of stations that are machine picked
      character*(*)     prmk(mxstat)                    
c							! (output) first motion description (eg. ipu0)
      real              resmax                          
c							! (input) maximum permitted angle of incidence
      character*(*)     revsta(mxstat)                  
c							! (input) reversed station names
      real              sigmaf                          
c							! (output) calculated standard deviation of fit based on data errors
      character*(*)     stn(mxstat)                     
c							! (output) station names
      real              sumwt                           
c							! (output) sum of observed first motion weights
      real              weight(mxqual)                  
c							! (input) weights associated with qualities
      real              wtobs(mxstat)                   
c							! (input) observed first motions weights
c
cgfortran      character*80      evline         
      character*90      evline                 
c							! temporary line for rearranging event format
      logical           first                           
c							! flag: t=first time into routine
      character*1       fm                              
c							! first motion direction (u, d, +, -)
      real              fmag                            
c							! event magnitude
      integer           i                               
c							! dummy loop index
      integer           ipwt                            
c							! qualiity assigned to p arrival
      integer           iyr
c							! 2-digit year
      integer           j                               
c							! dummy loop index
      integer           jdate                           
c							! date of event
      integer           jwt                             
c							! index for data weight
      integer           k                               
c							! counter of good phase readings
cgfortran      character*80      line  
      character*90      line                            
c							! line of hypo71 station output
      character*1       m                               
c							! test for fortran carriage control
      integer           nclas(20)                       
c							! number of observations in each data class
      real              pres                            
c							! traveltime residual
      character*4       stn4
c                                                       ! first 4 letters of station name
      character*4       test                            
c							! 2nd-6th characters of line of hypo71 output
      real              varf                            
c							! calculated variance in fit based on data errors.
      real              varf1                           
c							! summation of number of observations
      real              varf2                           
c							! summation of number of observations per class x corresponding weight
      real              wt                              
c							! weight assigned to p arrival
c
      data first/.true./
      save first, nclas
c
c reset values
c
      if (icmp .eq. 0 .or. (icmp .eq. 1 .and. first)) then
        do 10 i = 1, mxqual
          nclas(i) = 0
10      continue
        prcntx = 0.
        sumwt = 0.
        first = .false.
      end if
c
c find line prior to summary card
c
cgfortran20    read (iunit, 30, end = 1000) test
20    read (iunit, '(a)', end = 1000, err=1000) evline   ! new

      test=evline(3:6)    ! gfortran
30    format (2x, a4)
      if (test .ne. 'DATE') goto 20
c
c read summary card
c
        read (iunit, 40, end = 1000) evline
cgfortran40      format (1x, a80)
40      format (1x, a)       ! new

        read (evline, '(i6, 39x, f5.2)') jdate, fmag
	if (jdate .ge. 0 .and. jdate .lt. 670000) then
	  jdate = 20000000 + jdate
	else
	  jdate = 19000000 + jdate
	endif
c
c check magntitude
c
        if (fmag .lt. fmagmn) then
          if (icmp .eq. 0) nr = 0
          return
        end if
cgfortran        read (iunit, 30, end = 1000) test
cgfprtran        read (iunit, 30, end = 1000) test
        read(iunit,'(a)') evline  ! new
        read(iunit,'(a)') evline  ! new
        test=evline(3:6)          ! new

c
c check whether phase data or focal mechanism follow
c
        if (test .ne. 'STN ') goto 20
        if (icmp .eq. 0) then
          k = 1
        else
          k = nr + 1
        end if
50      stn(k) = '     '
        read (iunit, 60, end = 70) line
60      format (a)
        read (line, '(a1, a4)') m, stn4
	stn(k) = stn4//' '
c
c check for end of phase data
c
70      if (m.eq.'1' .or. stn(k) .eq. '     ' .or. 
     1 stn(k) .eq. 'DATE ') then
c
c end of event
c
          if (k - 1 .ge. minobs .or. (icmp .eq. 1 .and. k .gt. 1)) then
c
c reformat event into y2k hypo71 summary format
c
            if (icmp .eq. 0 .or. (icmp .eq. 1 .and. nr .eq. 0))  then
	      read (evline, '(i2)') iyr
	      if (iyr .ge. 0 .and. iyr .lt. 67) then
	        event = '20'//evline(1:53)//evline(57:60)//
     1 evline(54:56)//'.0'//evline(63:80)
	      else
	        event = '19'//evline(1:53)//evline(57:60)//
     1 evline(54:56)//'.0'//evline(63:80)
	      endif
	    endif
            nr = k - 1
            prcntx = prcntx/float(nr)
            varf1 = 0.
            varf2 = 0.
            do 80 jwt = 1, mxqual
              varf1 = varf1 + nclas(jwt)
              varf2 = varf2 + nclas(jwt)*weight(jwt)
80          continue
            varf  = varf1/(varf2*varf2)
            sigmaf= sqrt(varf)
          else if (icmp .eq. 0) then
            if (dbg) write (eunit, 85) evline(1:14), k - 1, minobs
85          format (/, ' ', 'event: ', a14,
     1 ' skipped: # of valid readings (=', i4, ') <', i4,/)
            nr = 0
          end if
          return
        end if
c
c ignore this station?
c
        if (nkil .gt. 0) then
          do 90 i = 1, nkil
            if (stn(k) .eq. kilsta(i)) then
              if (dbg)
     1 write (eunit, 105) stn(k),
     2 'name is in "kil" list', evline(1:14)
	      goto 50
	    endif
90        continue
        end if
c
c  so far, so good: now check phase card for polarity, distance, quality
c
        read (line, 100) dist(k), az(k), ain(k), prmk(k), pres
100     format (6x, f5.1, 1x, f3.0, 1x, f3.0, 1x, a4, t55, f5.2)
	call upstr(prmk(k), 4)
        read (prmk(k), '(2x, a1, i1)') fm, ipwt
        if (fm .ne. 'U' .and. fm .ne. 'D' .and. fm .ne. '+' .and.
     & fm .ne. '-' .and. fm .ne. 'C') then
          if (dbg)
     & write (eunit, 105) stn(k),'invalid first motion (='//fm//')',
     & evline(1:14)
	  goto 50
	endif
        if (dist(k) .gt. distmx) then
	  if (dbg)
     & write (eunit, 105) stn(k), 'epicentral distance > "dis" value',
     & evline(1:14)
	  goto 50
	endif
        if (abs(pres) .gt. resmax) then
	  if (dbg) write (eunit, 105) stn(k),
     & 'p-residual > "res" value', evline(1:14)
	  goto 50
	endif
        if (ain(k) .lt. ainmin .or. ain(k) .gt. ainmax) then
	  if (dbg)
     & write (eunit, 105) stn(k), 'take-off angle > "ain" value',
     & evline(1:14)
	  goto 50
	endif
        if (dist(k) .eq. 0.) then
          if (dbg) 
     & write (eunit, 105) stn(k), 'distance = 0',  evline(1:18)
105       format (' ', a5, ' skipped: ', a, ' for event: ', a18) 
          goto 50
        end if
        if (ipwt .ge. mxqual/2) then
          wt = 0.
	  if (dbg) write (eunit, 105) stn(k),
     & 'p-wt='//prmk(k)(4:4), evline(1:14)
          goto 50
        else
          jwt = ipwt + 1
          wt = weight(jwt)
          if (wt .eq. 0.) then
            if (dbg) write (eunit, 105) stn(k),
     & 'assigned hand-pick error rate=0 for p-wt='//prmk(k)(4:4),
     & evline(1:14)
	    goto 50
	  endif
	endif
c
c seisan add by jens
c      section taken out to enable composite solutions seisan way and two
c      different phases, same station
c
c
c check for repeated phase card
c
c        if (k .gt. 2 .and. icmp .eq. 0) then
c          do 120 j = 1, k - 1
c            if (stn(k) .eq. stn(j)) then
c              if (dbg) write (eunit, 105) stn(k), 
c     & 'multiple readings from same station', evline(1:18)
c              goto 50
c            end if
c120       continue
c        end if
c
c flip polariites if station is designated as reversed
c
        do 130 i = 1, nrev
          if (stn(k) .eq. revsta(i) .and. 
     1 jdate .ge. idate(i, 1) .and.
     2 (idate(i, 2) .eq. 0 .or. jdate .le. idate(i, 2))) then
            if (fm .eq. 'U') prmk(k)(3:3) = 'D'
            if (fm .eq. 'C') prmk(k)(3:3) = 'D'
            if (fm .eq. 'D') prmk(k)(3:3) = 'U'
            if (fm .eq. '+') prmk(k)(3:3) = '-'
            if (fm .eq. '-') prmk(k)(3:3) = '+'
            fm = prmk(k)(3:3)
	    if (dbg) write (eunit, 125) stn(k), evline(1:14)
125         format (' ', 'polarity flipped for station: ', a11,
     & ' for event: ', a14)
          end if
130     continue
c
        nclas(jwt) = nclas(jwt) + 1
        wtobs(k) = wt
        sumwt = sumwt + wt
        if (fm.eq.'U' .or. fm.eq.'+' .or. fm.eq.'C' .or. fm.eq.'c') then
          pobs(k) = .5
        else
          pobs(k) = -.5
        end if
c
c increment k and check number against array dimensions
c
        k = k + 1
        if (k .gt. mxstat) then
          write (eunit, *) 
     & '***** rdeq1 error: number of stations readings exceeds ', 
     & mxstat, 'for event: ', evline(1:14), ' *****'
          if (nr .gt. minobs) then
            nr = k - 1
            prcntx = prcntx/float(nr)
          else
            nr = 0
          end if
          return
        end if
c
c read another phase
c
        goto 50
c
c end of file
c
1000  nr = -1
      return
      end
      subroutine rdeq2 (ain, ainmin, ainmax, az, dist, distmx, eunit,
     & event, fmagmn, iunit, kilsta, minobs, mxqual, mxstat,
     & nkil, nr, nrev, pobs, prcntx, prmk, resmax, revsta, sigmaf, stn,
     & sumwt, weight, wtobs, icmp, idate, dbg)
c
c reads hypoellipse archive file. returns summary card and corresponding phase first motions, qualitites, angles of incidence,
c station names, and azimuths.  calculates standard deviation (sigmaf) of fit from estimated standard deviations of the data.
c the estimated data errors are control-file inputs; corresponding data weights are calculated in main and passed to this
c routine in the parameter "weight".
c
      integer           mxqual                          
c							! (input) maximum # of qualities permitted
      integer           mxstat                          
c							! (input) maximum # of stations permitted
      real              ain(mxstat)                     
c							! (output) ray incidence angles
      real              ainmax                          
c							! (input) maximum permitted angle of incidence
      real              ainmin                          
c							! (input) minimum permitted angle of incidence
      real              az(mxstat)                      
c							! (output) ray azimuth angles (corresponding to ain)
      logical           dbg
c                                                       ! (input) true(false) = do (not) issue warning messages
      real              dist(mxstat)                    
c							! (output) epicentral distance
      real              distmx                          
c							! (input) maximum permitted epicentral distance
      integer           eunit                           
c							! (input) logical unit # of output of error messages
      character*(*)     event                           
c							! (output) summary card
      real              fmagmn                          
c							! (input) minimum permitted magnitude
      integer           icmp                            
c							! (input) 1(0)=do (not) composite data into one mechanism; ievp on output
      integer           idate(mxstat,2)                 
c							! (input) date range of station reversal; 0=>open-ended
      integer           iunit                           
c							! (input) logical unit # of hypo71 listing file (input file)
      character*(*)     kilsta(mxstat)                  
c							! (input) ignored station names
      integer           minobs                          
c							! (input) minimum number of observations required
      integer           nkil                            
c							! (input) number of ignored stations
      integer           nr                              
c							! (output) -1=eof, 0=skip event, nr>0 => number of stations
      integer           nrev                            
c							! (input) number of reversed stations
      real              pobs(mxstat)                    
c							! (output) observed first motion polarities; .5=compression, -.5=dilatation
      real              prcntx                          
c							! (output) % of stations that are machine picked
      character*(*)     prmk(mxstat)                    
c							! (output) first motion description (eg. ipu0)
      real              resmax                          
c							! (input) maximum permitted angle of incidence
      character*(*)     revsta(mxstat)                  
c							! (input) reversed station names
      real              sigmaf                          
c							! (output) calculated standard deviation of fit based on data errors
      character*(*)     stn(mxstat)                     
c							! (output) station names
      real              sumwt                           
c							! (output) sum of observed first motion weights
      real              weight(mxqual)                  
c							! (input) weights associated with qualities
      real              wtobs(mxstat)                   
c							! (input) observed first motions weights
c
      real              depth                           
c							! hypocenter depth
      real              dmin                            
c							! distance to nearest station
      real              erh                             
c							! epicentral uncertainty
      real              erz                             
c							! depth uncertainty
      character*106      evline                          
c							! temporary line for summary card
      logical           first                           
c							! flag: t=first time into routine
      character*1       fm                              
c							! first motion direction (u, d, +, -)
      real              fmag                            
c							! event magnitude
      integer           i                               
c							! dummy loop index
      integer           ios                               
c							! iostat variable
      integer           ipwt                            
c							! qualiity assigned to p arrival
      integer           iyr                            
c							! year
      integer           j                               
c							! dummy loop index
      integer           jdate                           
c							! date of event
      integer           jwt                             
c							! index for data weight
      integer           k                               
c							! counter of good phase readings
      character*80      line                            
c							! line of hypo71 station output
      integer           nclas(20)                       
c							! number of observations in each data class
      real              pres                            
c							! traveltime residual
      real              rlatd                           
c							! epicenter latitude degrees
      real              rlatm                           
c							! epicenter latitude minutes
      real              rlond                           
c							! epicenter longitude degrees
      real              rlonm                           
c							! epicenter longitude minutes
      real              rmag                            
c							! event magnitude
      real              rms                             
c							! event traveltime rms
      real              sec                             
c							! origin time seconds
      character*106     test                            
c							! archive record
      real              varf                            
c							! calculated variance in fit based on data errors.
      real              varf1                           
c							! summation of number of observations
      real              varf2                           
c							! summation of number of observations per class x corresponding weight
      real              wt                              
c							! weight assigned to p arrival
	logical           y2k
c                                                       ! y2k format found for hypoellipse file
c
      data first/.true./
      save first, nclas, y2k
c
c reset values
c
      if (icmp .eq. 0 .or. (icmp .eq. 1 .and. first)) then
        do 10 i = 1, mxqual
          nclas(i) = 0
10      continue
        prcntx = 0.
        sumwt = 0.
      endif
c
c what format is this?
c
      if (first) then
15      read (iunit, '(a)', end = 1000) test
        if (test(1:2) .eq. 'C*' .or. .not. (test(81:81) .eq. '/' .or. 
     1 test(83:83) .eq. '/')) goto 15
	if (test(81:81) .eq. '/') then
	  y2k = .false.
	elseif (test(83:83) .eq. '/') then
	  y2k = .true.
	else
	  write(eunit,'(/,a)')'rdeq2 error: cannot determine y2k status'
	  stop
	endif
	rewind(iunit)
        first = .false.
      end if
c
c find summary card
c
20    read (iunit, 30, end = 1000) test
30    format (a)
      if (test(1:2) .eq. 'C*' .or. .not. (test(81:81) .eq. '/' .or. 
     1 test(83:83) .eq. '/')) goto 20
c
c read summary card
c
	if (.not. y2k) then
	  read (test, '(i2)') iyr
	
c assume century
	
	  if (iyr .ge. 0 .and. iyr .lt. 67) then
	    evline = '20'//test
	  else
	    evline = '19'//test
	  endif
	else
	  evline = test
	endif
        read (evline, '(i8, 28x, f2.1)') jdate, fmag
c
c check magntitude
c
        if (fmag .lt. fmagmn) then
          if (icmp .eq. 0) nr = 0
          return
        end if
        if (icmp .eq. 0) then
          k = 1
        else
          k = nr + 1
        end if
50      read (iunit, 30, end = 1000) line
        stn(k) = line(1:4)//' '
        if (stn(k)(1:2) .eq. 'C*') goto 50
c
c check for end of phase data
c
70      if (stn(k) .eq. '     ') then
c
c end of event
c
          if (k - 1 .ge. minobs .or. (icmp .eq. 1 .and. k .gt. 1)) then
c
c reformat summary record into y2k hypo71 summary format
c
            if (icmp .eq. 0 .or. (icmp .eq. 1 .and. nr .eq. 0)) then
              read (evline, 75) sec, rlatd, rlatm, rlond, rlonm, 
     & depth, rmag, dmin, rms, erh, erz
75            format (12x, f4.2, f2.0, 1x, f4.2, f3.0, 1x, f4.2, f5.2,
     &  f2.1, 6x, f3.0, f4.2, 5x, f4.2, 14x, f4.2)
              write (event, 76) evline(1:8), evline(9:12), sec, 
     & int(rlatd), evline(19:19), rlatm, int(rlond), evline(25:25), 
     & rlonm, depth, rmag, evline(39:41), evline(42:44), dmin, rms,
     & erh, erz
76            format (a8, 1x, a4, f6.2, i3, a1, f5.2, i4, a1, f5.2,
     & 2f7.2, a3, 1x, a3, f5.1, f5.2, 2f5.1)
            end if
            nr = k - 1
            prcntx = prcntx/float(nr)
            varf1 = 0.
            varf2 = 0.
            do 80 jwt = 1, mxqual
              varf1 = varf1 + nclas(jwt)
              varf2 = varf2 + nclas(jwt)*weight(jwt)
80          continue
            varf  = varf1/(varf2*varf2)
            sigmaf= sqrt(varf)
          else if (icmp .eq. 0) then
            write (eunit, 85) evline(1:14), k - 1, minobs
85          format (/, ' ', 'event: ', a14,
     1 ' skipped: # of valid readings (=', i4, ') <', i4,/)
            nr = 0
          end if
          return
        end if
c
c ignore this station?
c
        if (nkil .gt. 0) then
          do 90 i = 1, nkil
            if (stn(k) .eq. kilsta(i)) then
	      if (dbg)
     1 write (eunit, 105) stn(k),
     2 'name is in "kil" list', evline(1:14)
	      goto 50
	    endif
90        continue
        end if
c
c  so far, so good: now check phase card for polarity, distance, quality
c
        read (line, 100, iostat = ios, err = 102) prmk(k), dist(k), 
     1 az(k), ain(k), fm, pres
100     format (4x, a4, 16x, f4.1, f3.0, 9x, f3.0, 21x, a1, 10x, f5.2)
102	if (ios .ne. 0) then
	  if (dbg)
     1 write (eunit, 104) line
104	  format ('skipping unrecognized phase: ', a)
	  goto 50
	endif
        prmk(k)(3:3) = fm
        call upstr (prmk(k), 4) 
        if (.not. (prmk(k)(2:2).eq.'P' .or. prmk(k)(2:2).eq.'Z')) then
	  if (dbg) write (eunit, 105) stn(k), 
     1 'invalid component (='//prmk(k)(2:2)//')', evline(1:14) 
	  goto 50
	endif
        if (fm .eq. 'C') fm = 'U'
        if (fm .ne. 'U' .and. fm .ne. 'D' .and. fm .ne. '+' .and.
     & fm .ne. '-' .and. fm .ne. 'C') then
	  if (dbg)
     & write (eunit, 105) stn(k),'invalid first motion (='//fm//')',
     & evline(1:14)
	  goto 50
	endif
        if (dist(k) .gt. distmx) then
	  if (dbg)
     & write (eunit, 105) stn(k), 'epicentral distance > "dis" value',
     & evline(1:14)
	  goto 50
	endif
        if (abs(pres) .gt. resmax) then
	  if (dbg) write (eunit, 105) stn(k),
     & 'p-residual > "res" value', evline(1:14)
	  goto 50
	endif
        if (ain(k) .lt. ainmin .or. ain(k) .gt. ainmax) then
	  if (dbg)
     & write (eunit, 105) stn(k), 'take-off angle > "ain" value',
     & evline(1:14)
	  goto 50
	endif
        if (dist(k) .eq. 0.) then
          if (dbg) 
     & write (eunit, 105) stn(k), 'distance = 0',  evline(1:18)
105       format (' ', a5, ' skipped: ', a, ' for event: ', a18)
          goto 50
        end if
        read (prmk(k), '(3x, i1)') ipwt
        if (ipwt .ge. mxqual/2) then
          wt = 0.
	  if (dbg) write (eunit, 105) stn(k),
     & 'p-wt='//prmk(k)(4:4), evline(1:14)
	  goto 50
        else 
          jwt = ipwt + 1
          wt = weight(jwt)
          if (wt .eq. 0.) then
	    if (dbg) write (eunit, 105) stn(k),
     & 'assigned hand-pick error rate=0 for p-wt='//prmk(k)(4:4),
     & evline(1:14)
            goto 50
          end if
        end if
c
c check for repeated phase card
c
        if (k .gt. 2 .and. icmp .eq. 0) then
          do 120 j = 1, k - 1
            if (stn(k) .eq. stn(j)) then
              if (dbg) write (eunit, 105) stn(k), 
     & 'multiple readings from same station', evline(1:18)
              goto 50
            end if
120       continue
        end if
c
c flip polariites if station is designated as reversed
c
        do 130 i = 1, nrev
          if (stn(k) .eq. revsta(i) .and. 
     1 jdate .ge. idate(i, 1) .and.
     & (idate(i, 2) .eq. 0 .or. jdate .le. idate(i, 2))) then
            if (fm .eq. 'U') prmk(k)(3:3) = 'D'
            if (fm .eq. 'C') prmk(k)(3:3) = 'D'
            if (fm .eq. 'D') prmk(k)(3:3) = 'U'
            if (fm .eq. '+') prmk(k)(3:3) = '-'
            if (fm .eq. '-') prmk(k)(3:3) = '+'
            fm = prmk(k)(3:3)
	    if (dbg) write (eunit, 125) stn(k), evline(1:14)
125         format (' ', 'polarity flipped for station: ', a11,
     & ' for event: ', a14)
          end if
130     continue
c
        nclas(jwt) = nclas(jwt) + 1
        wtobs(k) = wt
        sumwt = sumwt + wt
        if (fm .eq. 'U' .or. fm .eq. '+' .or. fm .eq. 'C') then
          pobs(k) = .5
        else
          pobs(k) = -.5
        end if
c
c increment k and check number against array dimensions
c
        k = k + 1
        if (k .gt. mxstat) then
          write (eunit, *) 
     & '***** rdeq2 error: number of stations readings exceeds ', 
     & mxstat, 'for event:', evline(1:10), ' *****'
          if (nr .gt. minobs) then
            nr = k - 1
            prcntx = prcntx/float(nr)
          else
            nr = 0
          end if
          return
        end if
c
c read another phase
c
        goto 50
c
c end of file
c
1000  nr = -1
      return
      end
      subroutine rdeq3 (ain, ainmin, ainmax, az, dist, distmx, eunit, 
     & event, fmagmn, iunit, kilsta, minobs, mxqual, mxstat,
     & nkil, nr, nrev, pobs, prcntx, prmk, resmax, revsta, sigmaf, stn,
     & sumwt, weight, wtobs, icmp, idate, evntid, netcode, src,
     & kilnet, revnet, dbg, macsrc, nmsrc, hndsrc, nhsrc, ncmpnt, okcmp)
c
c reads hypoinverse archive file. returns summary card and corresponding phase first motions, qualitites, angles of incidence,
c station names, and azimuths.  calculates standard deviation (sigmaf) of fit from estimated standard deviations of the data.
c the estimated data errors are control-file inputs; corresponding data weights are calculated in main and passed to this
c routine in the parameter "weight".
c
      integer           mxqual                          
c							! (input) maximum # of qualities permitted
      integer           mxstat                          
c							! (input) maximum # of stations permitted
      real              ain(mxstat)                     
c							! (output) ray incidence angles
      real              ainmax                          
c							! (input) maximum permitted angle of incidence
      real              ainmin                          
c							! (input) minimum permitted angle of incidence
      real              az(mxstat)                      
c							! (output) ray azimuth angles (corresponding to ain)
      logical		dbg
c							! (input) true(false) = do (not) issue warning messages
      real              dist(mxstat)                    
c							! (output) epicentral distance
      real              distmx                          
c							! (input) maximum permitted epicentral distance
      integer           eunit                           
c							! (input) logical unit # of output of error messages
      character*(*)     event                           
c							! (output) summary card
      integer           evntid                           
c                                                       ! event id #
      real              fmagmn                          
c							! (input) minimum permitted magnitude
      character*1	hndsrc(mxstat)
c							! (input) allowable hand-timed source codes
      integer           icmp                            
c							! (input) 1(0)=do (not) composite data into one mechanism; ievp on output
      integer           idate(mxstat,2)                 
c							! (input) date range of station reversal; 0=>open-ended
      integer           iunit                           
c							! (input) logical unit # of hypo71 listing file (input file)
      character*2	kilnet(mxstat)
c							! (input) seismic network code for kilsta
      character*5	kilsta(mxstat)                  
c							! (input) ignored station names
      character*1	macsrc(mxstat)
c							! (input) allowable machine source codes
      integer           minobs                          
c							! (input) minimum number of observations required
      character*2	netcode(mxstat)
c							! (input) seismic network code (if newfor)
      integer           ncmpnt
c							! (input) number of allowable component codes (okcmp)
      integer           nkil                            
c							! (input) number of ignored stations
      integer           nhsrc
c							! (input) number of allowed hand-timed source codes (hndsrc)
      integer           nmsrc
c							! (input) number of allowed machine source codes (macsrc)
      integer           nr                              
c							! (output) -1=eof, 0=skip event, nr>0 => number of stations
      integer           nrev                            
c							! (input) number of reversed stations
      character*3	okcmp(mxstat)
c							! (input) allowable component codes
      real              pobs(mxstat)                    
c							! (output) observed first motion polarities; .5=compression, -.5=dilatation
      real              prcntx                          
c							! (output) % of stations that are machine picked
      character*4       prmk(mxstat)                    
c							! (output) first motion description (eg. ipu0)
      real              resmax                          
c							! (input) maximum permitted p-residual
      character*2	revnet(mxstat)
c							! (input) seismic network code for revsta
      character*5       revsta(mxstat)                  
c							! (input) reversed station names
      real              sigmaf                          
c							! (output) calculated standard deviation of fit based on data errors
      character*(5)       stn(mxstat)                     
c							! (output) station names
      real              sumwt                           
c							! (output) sum of observed first motion weights
      real              weight(mxqual)                  
c							! (input) weights associated with qualities
      real              wtobs(mxstat)                   
c							! (input) observed first motions weights
c
      real		amag1
c							! secondary magnitude 
      real		amag2
c							! secondary magnitude
      character*1	blast
c							! flag to indicate a blast
      character*1	bstflg
c							! magnitude designator for bstmag
      real		bstmag
c							! best magnitude
      character*1	cm(4)
c							! magnitude designator
      character*1	cm1
c							! secondary magnitude label on summary card
      character*1	cm2
c							! secondary magnitude label on summary card
      character*3	cmpnt
c							! station component code
      real              dep                             
c							! hypocenter depth (not used)
      real              dmin                            
c							! distance to nearest station (not used)
      real              erh                             
c							! horizontal error (not used)
      real              erz                             
c							! vertical error (not used)
      character*174     evline                          
c							! line for reading event summary
      character*1       e_w                          
c							! hemisphere for lat
      logical           first                           
c							! flag: t=first time into routine
      character*1       fm                              
c							! first motion direction (u, d, +, -)
      real              fmag                            
c							! event magnitude
      integer           i                               
c							! dummy loop index
      integer           ic                              
c							! number of characters in summary card
      integer           igap                            
c							! gap (not used)
      integer           ih                             
c							! origin hour 
      integer           im                             
c							! origin minute 
      integer           ios                             
c							! iostat error
      integer           ipwt                            
c							! qualiity assigned to p arrival
      integer           j                               
c							! dummy loop index
      integer           jdate                           
c							! date of event
      integer           jdate1                           
c							! year of event
      integer           jdate2                           
c							! month of event
      integer           jdate3                           
c							! day of event
      integer           jwt                             
c							! index for data weight
      integer           k                               
c							! counter of good phase readings
      integer           lat                             
c							! origin latitude (not used)
      character*120     line                            
c							! line of hypoinverse station data
      integer           llen                             
c							! line length
      integer           lon                             
c							! origin longitude (not used)
      integer           mpref(4)
c							! preference order of 4 magnitudes (USGS ML, UCB ML, coda, xmag)
      integer           nclas(20)                       
c							! number of observations in each data class
      logical		newfor
c							! flag: F/T=old/new hypoinverse station format
      character*1       n_s                          
c							! hemisphere for lon
      integer           nsp                             
c							! number of stations (not used)
      real              pres                            
c							! traveltime residual
      real              rms                             
c							! location rms (not used)
      real              sec                             
c							! origin second (not used)
      logical           shadow                           
c							! shadow card format found
      character*1       shdo                            
c							! shadow card
      character*1       src(mxstat)
c							! data source code
      character*4       stn4                          
c							! first 4 letters of station name
      character*1       stn5                          
c							! fifth letter of station name
      character*6	strg1
c							! scratch string
      character*6	strg2
c							! scratch string
      character*11	test_strg
c							! scratch string
      real              tmag(4)
c							! array of 4 potential magnitudes on summary card                            
      real              varf                            
c							! calculated variance in fit based on data errors.
      real              varf1                           
c							! summation of number of observations
      real              varf2                           
c							! summation of number of observations per class x corresponding weight
      real              wt                              
c							! weight assigned to p arrival
      real              xlat                            
c							! epicentral latitude (not used)
      real              xlon                            
c							! epicentral longitude (not used)
      real              xmag                            
c							! amplitude magnitude
      logical           y2k
c							! y2k format found for hypoinverse file                           
c
      data first/.true./
      data cm/'C', 'A', 'B', 'L'/
      data mpref/4, 3, 1, 2/
      save first, nclas, y2k, shadow, newfor
c
c reset values
c
	if (icmp .eq. 0 .or. (icmp .eq. 1 .and. first)) then
	  do 10 i = 1, mxqual
	    nclas(i) = 0
10	  continue
	  prcntx = 0.
	  sumwt = 0.
	endif
c
c what format is this?
c
c summary
c
	if (first) then
	  read (iunit, 30, end = 1000) evline
	  if (evline(9:9) .ne. ' ' .and. 
     1 (evline(17:17) .eq. ' ' .or. evline(17:17) .eq. 's')) then
	    y2k = .false.
	  elseif (evline(12:12) .ne. ' ' .and. evline(17:17).ne.' ')then
	    y2k = .true.
	  else
	    print *, 'rdeq2 error: y2k format is unclear'
	    stop
	  endif

c shadow

	  read (iunit, '(a)', end = 1000) evline
	  if (evline(1:1) .eq. '$') then
	    shadow = .true.
	  else
	    shadow = .false.
	  endif

c phase

	  if (shadow) read (iunit, '(a)', end = 1000) evline
	  llen = leng(evline)
	  if (.not. y2k) then
	    if (llen .lt. 95) then
	      newfor = .false.
	    else
	      newfor = .true.
	    endif
	  endif
	  rewind (iunit)
          first = .false.
        end if
        evline = ' '
c
c read summary card (skip non-summary cards)
c
20      read (iunit, 30, end = 1000) evline
30      format (a)
        if (shadow) read (iunit, 30, end = 1000) shdo
        ic = ichar (evline(1:1))
        if (ic .lt. 48 .or. ic .gt. 57) goto 20
	if (.not. y2k) then
          read (evline, 40) jdate1, jdate2, jdate3, xmag, fmag, cm1, 
     & amag1, cm2, amag2, blast, bstflg, bstmag
40	  format (3i2, t35, f2.1, t68, f2.1, t115, a1, f3.2, 3x, a1, 
     & f3.2, t77, a1, t139, a1, f3.2)
	  if (jdate1 .gt. 0 .and. jdate1 .lt. 67) then
	    jdate1 = 2000 + jdate1
	  else
	    jdate1 = 1900 + jdate1
	  endif
	else
          read (evline, 41) jdate1, jdate2, jdate3, xmag, fmag, cm1, 
     & amag1, cm2, amag2, blast, bstflg, bstmag
41	  format (i4, 2i2, t37, f3.2, t71, f3.2, t123, a1, f3.2, 3x, a1,
     & f3.2, t81, a1, t147, a1, f3.2)
	endif
        jdate = jdate1*10000 + jdate2*100 + jdate3
c c
c c Choose magnitude from preference list. Search down the list of mags in
c c the preferred order until a non-zero magnitude is found.
c 	tmag(3) = 0
c 	tmag(4) = 0
c c
c c Find the Berkeley & local mag if present
c c
c 	if (cm1 .eq. 'B') tmag(3) = amag1
c 	if (cm1 .eq. 'L') tmag(4) = amag1
c 	if (cm2 .eq. 'B') tmag(3) = amag2
c 	if (cm2 .eq. 'L') tmag(4) = amag2
c c
c c Assemble preference list
c c
c 	tmag(1) = fmag
c 	tmag(2) = xmag
c c
c c The preferred mag is the first non-zero one
c c
c 	do 45 i = 1,4
c 	  bstmag = tmag(mpref(i))
c 	  if (bstmag .gt. 0.) then
c 	    bstflg = cm(mpref(i))
c 	    goto 46
c 	  end if
c 45	continue
c c
c c All magnitudes are zero
c c
c	bstflg = ' '
46      if (bstmag .lt. fmagmn) then
          if (icmp .eq. 0) nr = 0
          return
        end if
c
c get the phase data
c
        if (icmp .eq. 0) then
          k = 1
        else
          k = nr + 1
        end if
50      stn(k) = '    '
        read (iunit, 60, end = 70) line
60      format (a)
        if (shadow) read (iunit, 60,end=70) shdo
	if (.not. y2k) then
	  if (.not. newfor) then
	    read (line, '(a4)') stn4
	    stn5 = ' '
	    netcode(k) = '  '
	  else
	    read (line, '(a4, t95, a1, 3x, a2)') stn4, stn5, netcode(k)
	  end if
	  stn(k) = stn4//stn5
	else
	  read (line, '(a5,a2)') stn(k), netcode(k)
	endif
c
c check for end of phase data
c
70      if (stn(k)(1:4) .eq. '    ') then
c
c end of event
c
	  if (blast .ne. ' ') then
c
c Skip blasts (since sources are non-double-couple
c
            if (dbg) write (eunit, 42) evline(1:14), blast
42	      format (/,' ', 'event: ', a14, 
     1 ' skipped: auxilliarly remark (=', a1, ') is not a blank', /)
              nr = 0
          elseif (k-1 .ge. minobs .or. (icmp.eq.1 .and. k.gt.1)) then
c
c reformat summary record into standard hypo71 summary format
c
            if (icmp .eq. 0 .or. (icmp .eq. 1 .and. nr .eq. 0)) then
	      if (.not. y2k) then
                read (evline, 75, iostat = ios) ih,im, sec, lat, n_s, 
     & xlat, lon, e_w, xlon, dep, nsp, igap, dmin, rms, erh, erz
75              format (6x,2i2, f4.2, i2, a1, f4.2, i3, a1, f4.2, f5.2,
     & 2x, i3, i3, f3.0, f4.2, 31x, 2f4.2)
	      else
                read (evline, 751, iostat = ios) ih,im, sec, lat, n_s, 
     & xlat, lon, e_w, xlon, dep, nsp, igap, dmin, rms, erh, erz
751             format (8x, 2i2, f4.2, i2, a1, f4.2, i3, a1, f4.2, f5.2,
     & t40, i3, i3, f3.0, f4.2, t86, 2f4.2)
	      endif
              write (event, 76) jdate1, jdate2, jdate3, ih,im, sec, lat, 
     1 n_s, xlat, lon, e_w, xlon, dep, bstmag, nsp, igap, dmin, rms, 
     2 erh, erz, bstflg
76            format (i4, 2i2.2, 1x,2i2.2, 1x, f5.2, i3, a1, f5.2,i4,a1,
     & f5.2, 2f7.2, i3, i4, f5.1, f5.2, 2f5.1, t82, a1)
            end if
            nr = k - 1
            prcntx = prcntx/float(nr)
            varf1 = 0.
            varf2 = 0.
            do 80 jwt = 1, mxqual
              varf1 = varf1 + nclas(jwt)
              varf2 = varf2 + nclas(jwt)*weight(jwt)
80          continue
            varf  = varf1/(varf2*varf2)
            sigmaf= sqrt(varf)
	    read (line, '(t63, i10)') evntid
          else if (icmp .eq. 0) then
            if (dbg) write (eunit, 85) evline(1:14), k - 1, minobs
85	    format (/,' ', 'event: ', a14, 
     1 ' skipped: # of valid readings (=', i4, ') <', i4,/)
            nr = 0
          end if
          return
        end if
c
c ignore this station?
c
        if (nkil .gt. 0) then
          do 90  i = 1, nkil
            if (stn(k)//netcode(k) .eq. kilsta(i)//kilnet(i)) then
	      if (dbg)
     1 write (eunit, 105) stn(k)//netcode(k)//'   ', 
     2 'name is in "kil" list', evline(1:14)
	      goto 50
	    endif
90        continue
        end if
c
c  so far, so good: now check phase card for polarity, distance, quality
c
	if (.not. y2k) then
          read (line, 100) prmk(k), pres, dist(k), ain(k), az(k), 
     1 src(k), cmpnt
100       format (4x, a4, t25, f4.2, t59, f4.1, f3.0, t76, f3.0, t92,
     1 a1, t96, a3)
	else
          read (line, 103) cmpnt, prmk(k), pres, dist(k), ain(k), az(k), 
     1 src(k)
103       format (t10, a3, 1x, a4, t35, f4.2, t75, f4.1, f3.0, t92, 
     1 f3.0, t109, a1)
	endif
	test_strg = stn(k)//netcode(k)//'-'//cmpnt
c
c check for acceptable 3-letter component code
c
	if ((.not. y2k .and. newfor) .or. y2k) then
	  if (ncmpnt .gt. 0) then
	    do 101 i = 1, ncmpnt
	      if (cmpnt .eq. okcmp(i)) goto 102
101	    continue
	  else
	    write (eunit, '(a)') 'no valid components set; see "chn"'
	    stop
	  endif
	  if (dbg) 
     1 write (eunit, 105) test_strg, 'invalid component', evline(1:14)
	  goto 50
	endif
102	call upstr (prmk(k), 4)
	read (prmk(k), '(2x, a1, i1)') fm, ipwt
        if (fm .ne. 'U' .and. fm .ne. 'D' .and. fm .ne. '+' .and.
     & fm .ne. '-' .and. fm .ne. 'C') then
	  if (dbg) 
     & write (eunit, 105) test_strg,'invalid first motion (='//fm//')', 
     & evline(1:14)
	  goto 50
	endif
        if (dist(k) .gt. distmx) then
	  if (dbg) then
	    write (strg1, '(f6.1)') dist(k)
	    write (strg2, '(f6.1)') distmx
	    write (eunit, 105) test_strg,
     &'epicentral distance (='//strg1//') > "dis" value (='//strg2//')', 
     & evline(1:14)
	  endif
	  goto 50
	endif
        if (ain(k) .lt. ainmin) then
	  if (dbg) then
	    write (strg1, '(f6.1)') ain(k)
	    write (strg2, '(f6.1)') ainmin
	    write (eunit, 105) test_strg, 
     & 'take-off angle (='//strg1//') < "ainmin" value (='//strg2//')', 
     & evline(1:14)
	  endif
	  goto 50
	endif
        if (ain(k) .gt. ainmax) then
	  if (dbg) then
	    write (strg1, '(f6.1)') ain(k)
	    write (strg2, '(f6.1)') ainmax
	    write (eunit, 105) test_strg, 
     & 'take-off angle (='//strg1//') > "ainmax" value (='//strg2//')', 
     & evline(1:14)
	  endif
	  goto 50
	endif
        if (abs(pres) .gt. resmax) then
	  if (dbg) then
	    write (strg1, '(f6.1)') abs(pres)
	    write (strg2, '(f6.1)') resmax
	    write (eunit, 105) test_strg, 
     & 'p-residual (='//strg1//') > "res" value (='//strg2//')', 
     & evline(1:14)
	  endif
	  goto 50
	endif
        if (dist(k) .eq. 0.) then
	  if (dbg) 
     & write (eunit, 105) test_strg, 'distance = 0', evline(1:14)
105       format (' ', a11, ' skipped: ', a, ' for event: ', a14)
          goto 50
        end if
c
c assign p-weight value based on hand or machine source
c
        if (ipwt .ge. mxqual/2) then
          wt = 0.
	  if (dbg) write (eunit, 105) test_strg,
     & 'p-wt='//prmk(k)(4:4), evline(1:14)
	  goto 50
	else
	  if (nmsrc .gt. 0) then
	    do 106 i = 1, nmsrc
	      if (src(k) .eq. macsrc(i)) then
                jwt = ipwt + mxqual/2 + 1
	        wt = weight(jwt)
                if (wt .ne. 0.) then
	          prcntx = prcntx + 1.
	          goto 108
	        elseif (dbg) then
	          write (eunit, 105) test_strg, 
     & 'assigned machine-pick error rate=0 for p-wt='//prmk(k)(4:4), 
     & evline(1:14)
	          goto 50
	        endif
	      endif
106	    continue
	  endif
	  if (nhsrc .gt. 0) then 
	    do 107 i = 1, nhsrc
	      if (src(k) .eq. hndsrc(i)) then
                jwt = ipwt + 1
                wt = weight(jwt)
                if (wt .ne. 0.) then
	          goto 108
	        else
	          if (dbg) write (eunit, 105) test_strg, 
     & 'assigned hand-pick error rate=0 for p-wt='//prmk(k)(4:4), 
     & evline(1:14)
	          goto 50
                end if
              end if
107	    continue
	  endif
	endif
        if (dbg) write (eunit, 105) test_strg, 
     & 'unknown data source (='//src(k)//')', 
     & evline(1:14)
	goto 50
c
c check for repeated phase card
c
108       if (k .gt. 2 .and. icmp .eq. 0) then
          do 120 j = 1, k - 1
            if (stn(k)//netcode(k) .eq. stn(j)//netcode(j)) then
	      if (dbg) 
     & write (eunit,105)test_strg,'multiple readings from same station',
     & evline(1:14)
              goto 50
            end if
120       continue
        end if
c
c flip polariites if station is designated as reversed
c
	if (nrev .gt. 0) then
          do 130 i = 1, nrev
            if (stn(k)//netcode(k) .eq. revsta(i)//revnet(i) .and.
     & jdate .ge. idate(i, 1) .and.
     & (idate(i, 2) .eq. 0 .or. jdate .le. idate(i, 2))) then
              if (fm .eq. 'U') prmk(k)(3:3) = 'D'
              if (fm .eq. 'C') prmk(k)(3:3) = 'D'
              if (fm .eq. 'D') prmk(k)(3:3) = 'U'
              if (fm .eq. '+') prmk(k)(3:3) = '-'
              if (fm .eq. '-') prmk(k)(3:3) = '+'
              fm = prmk(k)(3:3)
	      if (dbg) write (eunit, 125) test_strg, evline(1:14)
125	      format (' ', 'polarity flipped for station: ', a11, 
     & ' for event: ', a14)
            end if
130       continue
        end if
c
        nclas(jwt) = nclas(jwt) + 1
        wtobs(k) = wt
        sumwt = sumwt + wt
        if (fm .eq. 'U' .or. fm .eq. '+' .or. fm .eq. 'C') then
          pobs(k) = .5
        else
          pobs(k) = -.5
        end if
c
c increment k and check number against array dimensions
c
        k = k + 1
        if (k .gt. mxstat) then
          write (eunit, *) 
     & '***** rdeq3 error: number of stations readings exceeds ', 
     & mxstat, 'for event: ', evline(1:14), ' *****'
          if (nr .gt. minobs) then
            nr = k - 1
            prcntx = prcntx/float(nr)
          else
            nr = 0
          end if
          return
        end if
c
c read another phase
c
        goto 50
c
c end of file
c
1000  nr = -1
      return
      end
      real function rdiff (rake1, rake2)
c
c  returns with the smallest absolute difference in slip angle between rake1 and rake2.
c
c  rake convention follows aki & richards, 1980, quantitative seismology, p. 114
c
      real              rake1                           
c							! (input) first rake
      real              rake2                           
c							! (input) second rake
c
      real              a                               
c							! stores first rake
      real              b                               
c							! stores second rake
      real              c                               
c							! stores rake difference
c
      rdiff = 999.
      a = rake1
      if (rake1 .lt. 0.) a = 360. + rake1
      b = rake2
      if (rake2 .lt. 0.) b = 360. + rake2
      c = abs(a - b)
      if (c .gt. 180.) c = 360. - c
      rdiff = c
      return
      end
      subroutine readfl (inst, unit, filnm, status, erflag, prompt)

c reads file name from terminal and opens file

      character*80      inst                            
c							! (input) parameters on instruction line
      integer           unit                            
c							! (input) logical unit for reporting merge action
      character*40      filnm                           
c							! (input/output) name of file
      character*(*)     prompt                          
c							! (input) prompt string
      character*(*)     status                          
c							! (input) open status ('new' or 'old')
      integer           erflag                          
c							! (output) error flag; non-zero indicates unable to open

      integer           ios                             
c							! iostat specifier

      erflag = 0
      if (inst(1:10) .eq. '          ') then
        call askc (prompt, filnm)
      else
        read (inst, *, iostat = ios) filnm
        if (ios .ne. 0) then
          erflag = 1
          filnm = 'none'
        end if
      end if
      if (filnm(1:4) .ne. 'none') then
        if (status .eq. 'new') then
          open (unit, file = filnm, status = 'unknown', iostat = erflag)
c         open (unit, file = filnm, status = 'new', iostat = erflag,		! VAX/VMS version
c    & carriagecontrol = 'list', recl = 139)					! VAX/VMS version
        else
	  open (unit,file=filnm,status='old',blank='zero',iostat=erflag)
c         open (unit, file = filnm, status = 'old', blank = 'zero',		! VAX/VMS version
c    & iostat = erflag, readonly)						! VAX/VMS version
        end if
        if (erflag .ne. 0) filnm = 'none'
      end if
      if (filnm .eq. 'none') close (unit)
      return
      end
      subroutine refrmt (del, idip, idipdr, islip, phis, xlam)
c
c  reformat dip, strike, and rake angles to integer values and convert strike to down-dip direction
c
      real              del                             
c							! (input) fault dip angle in degrees
      integer           idip                            
c							! (output) fault dip angle in degrees
      integer           idipdr                          
c							! (output) dip direction in degrees
      integer           islip                           
c							! (output) rake in degrees
      real              phis                            
c							! (input) fault strike angle in degrees
      real              xlam                            
c							! (input) fault rake angle in degrees
c
      integer           istrk                           
c							! strike of best fit
      real              da                              
c							! dip angle of new plane
      real              dd                              
c							! dip direction angle of new plane
      real              sa                              
c							! slip angle of new plane
c
      idip = ifix(del)
      istrk = ifix(phis)
      islip = ifix(xlam)
      if (idip .gt. 90) then
        idip = 180 - idip
        istrk = istrk + 180
        islip = -islip
      else if (idip .lt. 0) then
        idip = -idip
        istrk = istrk + 180
        islip = islip + 180
      end if
      idipdr = mod(istrk + 90, 360)
      if (idipdr .lt. 0) idipdr = idipdr + 360
      islip = mod(islip, 360)
      if (islip .gt. 180) islip = islip - 360
      if (islip .lt. -180) islip = islip + 360
c
c replace each plane with idip = 0 by its auxilliary plane
c
      if (idip .eq. 0) then
        call auxpln (float(idipdr), float(idip), float(islip),
     & dd, da, sa)
        idipdr = ifix(dd)
        idip  = ifix(da)
        islip = ifix(sa)
      end if
c
c for cases where plane is vertical and dip direction .ge. 180, flip representation to one with dip direction .le. 180
c
      if (idip .eq. 90 .and. idipdr .ge. 180.) then
        islip = -islip
        idipdr = idipdr - 180.
      end if
c
      return
      end
      subroutine search (bot, coef, ddel, del, delc, del0, dfit, dlam,
     & dphi, eps, first, fit, fitmin, flag, gfit, iamwt, igood, j1,
     & m1, mxdip, mxrake, mxstat, mxstrk, n1, ndel, ng, nlam, nphi, nr,
     & phis, phisc, phis0, pobs, rad, wtobs, xlam, xlamc, xlam0)
c
c loop over the entire focal mechanism space, compute fit parameter for each solution, and return best fit indices
c in case of tie fit, choose fit with largest "bot".  if fine search (first = false) then fill in flag array with
c stars for solutions with fit parameter <= best fit + dfit
c
      integer           mxdip                           
c							! (input) maximum # of dip increments permitted
      integer           mxrake                          
c							! (input) maximum # of rake increments permitted
      integer           mxstat                          
c							! (input) maximum # of stations permitted
      integer           mxstrk                          
c							! (input) maximum # of strike increments permitted
      real              bot(mxdip,mxstrk,mxrake)        
c							! (output) sum of product of observed and predicted weights
      real              coef(mxstat,6)                  
c							! (input) coefficients by which tm multiplied to give p radiation pattern
      real              ddel                            
c							! (input) fault dip increment in degrees
      real              del(mxdip)                      
c							! (output) fault dip angle in degrees
      real              delc(mxdip)                     
c							! (output) fault dip angle for coarse search
      real              del0                            
c							! (input) initial fault dip angle in degrees
      real              dfit                            
c							! (input) increment to fit function
      real              dlam                            
c							! (input) fault rake increment in degrees
      real              dphi                            
c							! (input) fault strike increment in degrees
      real              eps
c                                                       ! (input) machine precision
      logical           first                           
c							! (input) flag: true=first time into subroutine search
      real              fit(mxdip,mxstrk,mxrake)        
c							! (output) weighted measure of agreement between obs, pred polarities
      real              fitmin                          
c							! (output) fit of best solution corresponding to fit(j1, n1, m1)
      character*1       flag(mxdip,mxstrk,mxrake)       
c							! (output) if fit < fitlim then '*', otherwise blank
      real              gfit(mxdip*mxstrk*mxrake)       
c							! (output) fits of good solutions from coarse search
      integer           iamwt                           
c							! (input) flag controling amplitude weighting (0=omit, 1=use)
      integer           igood(mxdip*mxstrk*mxrake,4)    
c							! (output) array containing indices of good solutions (coarse)
      integer           j1                              
c							! (output) dip index of best solution
      integer           m1                              
c							! (output) rake index of best solution
      integer           n1                              
c							! (output) strike index of best solution
      integer           ndel                            
c							! (input) number of fault dip increments
      integer           ng                              
c							! (output) number of good solutions in coarse search
      integer           nlam                            
c							! (input) number of fault rake increments
      integer           nphi                            
c							! (input) number of fault strike increments
      integer           nr                              
c							! (input) -1=eof, 0=nr<minobs, nr>0 => number of stations
      real              phis(mxstrk)                    
c							! (output) fault strike angle in degrees
      real              phisc(mxstrk)                   
c							! (output) fault strike angle in degrees for coarse search
      real              phis0                           
c							! (input) initial fault strike angle in degrees
      real              pobs(mxstat)                    
c							! (input) observed first motion polarities; .5=compression, -.5=dilatation
      real              rad                             
c							! (input) conversion from degrees to radians
      real              wtobs(mxstat)                   
c							! (input) observed first motions weights
      real              xlam(mxrake)                    
c							! (output) fault rake angle in degrees
      real              xlamc(mxrake)                   
c							! (output) fault rake angle in degrees for coarse search
      real              xlam0                           
c							! (input) initial fault rake angle in degrees
c
      real              best                            
c							! largest bot for solutions with fit=fitmin (ie. ties)
      real              bot1                            
c							! sum of product of observed weights
      real              dip                             
c							! fault dip angle in radians
      real              fitlim                          
c							! upper bound on "good" solutions in search
      integer           i                               
c							! loop index 
      integer           j                               
c							! loop index over dip
      integer           k                               
c							! loop index 
      integer           m                               
c							! loop index over rake
      integer           n                               
c							! loop index of strike
      real              prad                            
c							! radiation amplitude corresponding ain, phi.
                                                        
c							! (dilatation) -1.<prad<+1.(compression)
      real              pth                             
c							! predicted first motion polarity; same convention as for pobs
      real              slip                            
c							! fault slip angle in radians
      real              strike                          
c							! fault strike angle in radians
      real              tm(6)                           
c							! moment tensor in upper triangular symetric storage mode
      real              top                             
c							! sum of amp wtd difference of predicted, obs. polarities; 0<= top <=1
      real              top1                            
c							! sum of non-amp wtd difference of predicted, obs. polarities; 0<= top <=1
      real              wtth                            
c							! predicted first motions weights
c
      best = 0.
      fitmin = 2.0
      do 50 m = 1, nlam
        xlam(m) = xlam0 + (m - 1)*dlam
        if (first) xlamc(m)=xlam(m)
        do 40 n = 1, nphi
          phis(n) = phis0 + (n - 1) * dphi
          if (first) phisc(n)=phis(n)
          do 30 j = 1, ndel
            del(j) = del0 + (j - 1) * ddel
            if (first) delc(j)=del(j)
            strike = phis(n)*rad
            dip = del(j)*rad
            slip = xlam(m)*rad
c
c calculate moment tensor representation of shear fault (postive up, south, east)
c
            call shrflt (strike, dip, slip, tm)
c
c calculate radiation pattern for model (eqtn 4.91, aki & richards, pg. 118)
c
            top = 0
            bot(j, n, m) = 0
            top1 = 0.
            bot1 = 0.
            do 20 i = 1, nr
              prad = 0
              do 10 k = 1, 6
                prad = prad + tm(k)*coef(i, k)
10            continue
c
c select amplitude weighting and calculate fit function for this model
c
              pth = sign(0.5, prad)
              wtth = sqrt(abs(prad))
              top = top + abs(pobs(i) - pth)*wtobs(i)*wtth
              bot(j, n, m) = bot(j, n, m) + wtobs(i)*wtth
              top1 = top1 + abs(pobs(i) - pth)*wtobs(i)
              bot1 = bot1 + wtobs(i)
20          continue
c
c use amplitude weighting
            if (iamwt .eq. 1) then
              fit(j, n, m) = top/bot(j, n, m)
c
c do not use amplitude weighting
            else if (iamwt .eq. 0) then
              fit(j, n, m) = top1/bot1
            end if
c
            if (fit(j, n, m) .lt. fitmin) fitmin = fit(j, n, m)
30        continue
40      continue
50    continue
c
c for tie solutions, find solution with most stations away from nodes
c
      fitlim = fitmin + dfit
      if (first) ng = 0
      do 90 m = 1, nlam
        do 80 n = 1, nphi
          do 70 j = 1, ndel
            if (((fit(j, n, m) - fitmin) .lt. eps) .and. bot(j, n, m)
     & .gt. best) then
              best = bot(j, n, m)
              j1 = j
              n1 = n
              m1 = m
            end if
c
c star solutions having fit within dfit of fitmin 
c
            if (fit(j, n, m) .le. fitlim) then
              flag(j, n, m) = '*'
c
c save solutions in coarse search with fit .le. fitlim as "good" solutions
c
              if (first) then
                ng = ng + 1
                igood(ng, 1) = j
                igood(ng, 2) = n
                igood(ng, 3) = m
                igood(ng, 4) = 0
                gfit(ng) = fit(j, n, m)
              end if
            else
              flag(j, n, m) = ' '
            end if
70        continue
80      continue
90    continue
      flag(j1, n1, m1) = 'a'
c
      return
      end
      subroutine shrflt(strike, dip, slip, tm)
c
c    this subroutine calculates the moment-tensor representation of a shear fault, given its strike, dip, and slip angles.
c
c  method:
c    the moment tensor is first expressed in a coordinate system with the z axis normal to the fault plane and the x axis in
c     the slip direction:
c
c                    (0.  0.  1.)
c                    (0.  0.  0.)
c                    (1.  0.  0.)
c
c    this coordinate system is then rotated through the euler angles phi = -slip, theta = -dip, and psi = strike - pi,
c    (conventions of goldstein, classical mechanics, sec 4-4) which results in a (south, east, up) orientation of the (x, y, z)
c    axes, respectively.  a permutation then converts this to the order (up, south, east).  the strength of the double-couple is
c    taken as unity; the calculated moment tensor components must be multiplied by the factor:
c
c         mu*a*s
c
c    where:
c         mu is the rigidity modulus of the medium
c         a is the fault area, and
c         s is the mean dislocation across the fault.
c           (note:  if the mean dislocation velocity is used instead,
c           the result will be the moment-rate tensor.)
c
c    written by bruce r. julian on 7 april, 1977.
c
      real              dip                             
c							! (input) fault dip angle in radians
      real              slip                            
c							! (input) fault slip angle in radians
      real              strike                          
c							! (input) fault strike angle in radians
      real              tm(6)                           
c							! (output) seismic moment tensor arranged in the following order:
                                                        
c							! (r, r)         i.e. (up, up)
                                                        
c							! (r, theta)     i.e. (up, south)
                                                        
c							! (theta, theta) i.e. (south, south)
                                                        
c							! (r, phi)       i.e. (up, east)
                                                        
c							! (theta, phi)   i.e. (south, east)
                                                        
c							! (phi, phi)     i.e. (east, east)
c
      real              a11                             
c							!  transformation matrix
      real              a21                             
c							!  transformation matrix
      real              a31                             
c							!  transformation matrix
      real              a13                             
c							!  transformation matrix
      real              a23                             
c							!  transformation matrix
      real              a33                             
c							!  transformation matrix
      real              cd                              
c							!  cos(dip)
      real              cl                              
c							!  cos(slip)
      real              cs                              
c							!  cos(strike)
      real              sd                              
c							!  sin(dip)
      real              sl                              
c							!  sin(slip)
      real              ss                              
c							!  sin(strike)
c
c  calculate components of orthogonal transformation matrix
c  from fault-oriented to (south, east, up) coordinate system
c
      ss = sin(strike)
      cs = cos(strike)
      sd = sin(dip)
      cd = cos(dip)
      sl = sin(slip)
      cl = cos(slip)
      a11 = -cs*cl - cd*sl*ss
      a21 =  ss*cl - cd*sl*cs
      a31 = sd*sl
      a13 = ss*sd
      a23 = cs*sd
      a33 = cd
c
c  transform moment tensor (0,   0,   1,
c                           0,   0,   0,
c                           1,   0,   0)
c
c  and permute axes to (up, south, east) order
c
      tm(1) = 2*a31*a33
      tm(2) = a11*a33 + a31*a13
      tm(3) = 2*a11*a13
      tm(4) = a21*a33 + a31*a23
      tm(5) = a11*a23 + a21*a13
      tm(6) = 2*a21*a23
c
      return
      end
      subroutine tandp (ainp, aint, azp, azt, da1, da2, dd1, dd2, sa1,
     & sa2, pi, rad)
c
c     given two planes compute az and angle of incidence of p & t axes
c
      real              ainp                            
c                                                       ! angle of incidence of p axis
      real              aint                            
c                                                       ! angle of incidence of t axis
      real              azp                             
c                                                       ! azimuth of p axis
      real              azt                             
c                                                       ! azimuth of t axis
      real              da1                             
c                                                       ! dip angle of priniciple plane
      real              da2                             
c                                                       ! dip angle of auxilliary plane
      real              dd1                             
c                                                       ! dip direction of principle plane
      real              dd2                             
c                                                       ! dip direction of auxilliary plane
      real              sa1                             
c                                                       ! rake of principal plane
      real              sa2                             
c                                                       ! rake of auxilliary plane
      real              pi                              
c                                                       ! pi
      real              rad                             
c                                                       ! pi/180
c
      real              ain1                            
c                                                       ! angle of incidence of p/t axis
      real              ain2                            
c                                                       ! angle of incidence of t/p axis
      real              alat1                           
c                                                       ! dip angle in radians of principle plane measured from vertical
      real              alat2                           
c                                                       ! dip angle in radians of auxilliary plane measured from vertical
      real              alon1                           
c                                                       ! dd1 in radians
      real              alon2                           
c                                                       ! dd2 in radians
      real              azimth                          
c                                                       ! azimuth in radians of pole ??
      real              az0                             
c                                                       ! azimuth from pole of auxilliary plane to pole of principle ??
      real              az1                             
c                                                       ! azimuth of p/t axis
      real              az2                             
c                                                       ! azimuth of t/p axis
      real              bazm                            
c                                                       ! not used
      real              delta                           
c                                                       ! not used
      real              plunge                          
c                                                       ! plunge in radians of pole ??
      real              shift                           
c                                                       ! azimuthal shift from pole of plane to p to t axis (= 45 degrees)??
      real              xpos                            
c                                                       ! not used
      real              ypos                            
c                                                       ! not used
c
      parameter (shift = 0.7853981)
c
      alat1 = (90. - da1)*rad
      alon1 = dd1*rad
      alat2 = (90. - da2)*rad
      alon2 = dd2*rad
      call refpt (alat2, alon2)
      call delaz (alat1, alon1, delta, az0, bazm, xpos, ypos)
      call back (shift, az0, plunge, azimth)
      if (abs(azimth) .gt. pi) azimth = azimth - sign(2.0*pi, azimth)
      az1 = azimth/rad
      ain1 = plunge/rad + 90.
      az0 = az0 + pi
      call back (shift, az0, plunge, azimth)
      if (abs(azimth) .gt. pi) azimth = azimth - sign(2.0*pi, azimth)
      az2 = azimth/rad
      ain2 = plunge/rad + 90.
      if (sa1 .ge. 0.) then
        ainp = ain2
	aint = ain1
	azp = az2
	azt = az1
      else
        ainp = ain1
	aint = ain2
	azp = az1
	azt = az2
      end if	
c
c map axes to lower hemisphere
c
	if (ainp .gt. 90.) then
	  ainp = 180. - ainp
	  azp = 180. + azp
	end if
	if (aint .gt. 90.) then
	  aint = 180. - aint
	  azt = 180. + azt
	end if
	if (azp .lt. 0.) azp = azp + 360.
	if (azt .lt. 0.) azt = azt + 360.
      return
      end
	subroutine upstr (str, len)
c
c	upstr converts the character string str to upper case.
c	len is the number of characters to convert, not to exceed the
c	actual length of str.
c
c	author: fred klein (u.s.g.s)
c
	character		str*(*)
	integer			i
	integer			j
	integer			len

	do 2 i = 1, len
	  j = ichar(str(i:i))
	  if (j .gt. 96 .and. j .lt. 123) str(i:i) =  char(j - 32)
2	continue
	return
	end

      logical function askl (prompt, dflt)
c
c  askl prompts then reads a logical value from the terminal.
c  the default value is returned on a cr response.
c
      logical           dflt                            
c							! default supplied on carriage return and displayed in prompt
      character         prompt*(*)                      
c							! prompt string
      integer           leng                            
c							! function
      character*7	temp
c							! scratch string
      integer		ounit
c							! logical unit for output (0 for UNIX, 6 for VMS)

      parameter (ounit = 6)
c      parameter (ounit = 0)
10    format (l1)
50    write (ounit, 60) prompt, dflt
60    format (1x, a, ' [cr = ', l2, ']? ', $)
      read (5, '(a)', err = 50, end = 70) temp
      if (leng(temp) .gt. 0) then
        read (temp, *, err = 50) askl
      else
        askl = dflt
      end if
70    return
      end



      subroutine fpout (ddelf, dlamf, dphif, erate, eunit, ievp, ievr,
     & ind, ires, mxdip, mxqual, mxrake, mxstat, mxstrk, ndelf, ndrng,
     & nfit, nlamf, nphif, nrev, nrrng, nsrng, nstat, qcnt, qcntwt,
     & revsta, scnt, scntwt, stat)
c
c generate summary listing of polarity discrepancies as a function of station and quality, the distribution of
c fit parameters, and the distribution of dip, strike, and rake ranges about best fit solution
c
      integer           mxqual                          
c							! (input) maximum # of qualities permitted
      integer           mxstat                          
c							! (input) maximum # of stations permitted
      real              ddelf                           
c							! (input) fault dip increment in degrees for fine search
      real              dlamf                           
c							! (input) fault rake increment in degrees for fine search
      real              dphif                           
c							! (input) fault strike increment in degrees for fine search
      real              erate(mxqual)                   
c							! (input) estimated weighted error rates
      integer           eunit                           
c							! (input) logical unit # of output of error messages
      integer           ievp                            
c							! (input) # of events processed
      integer           ievr                            
c							! (input) # of events read
      integer           ind(mxstat)                     
c							! (input) pointer array to sorted order
      integer           ires                            
c							! (input) flag: 0(1)=(un)restricted search
      integer           mxdip                           
c							! (input) maximum # of dip increments permitted
      integer           mxrake                          
c							! (input) maximum # of rake increments permitted
      integer           mxstrk                          
c							! (input) maximum # of strike increments permitted
      integer           ndelf                           
c							! (input) # of fault dip increments for fine search
      integer           ndrng(mxdip)                    
c							! (input) # of dip solution ranges binned into ddelf degree increments
      integer           nfit(20)                        
c							! (input) # of solutions binned into .025 fit increments
      integer           nlamf                           
c							! (input) # of fault rake increments for fine search
      integer           nphif                           
c							! (input) # of fault strike increments for fine search
      integer           nrev                            
c							! (input) # of reversed stations
      integer           nrrng(mxrake + 1)               
c							! (input) # of rake solution ranges binned into dlamf degree increments
      integer           nsrng(mxstrk)                   
c							! (input) # of strike solution ranges binned into dphif degree increments
      integer           nstat                           
c							! (input) total # of stations reporting for entire data set
      integer           qcnt(mxqual,2)                  
c							! (input) indx 1=# of dscrpnt plrties for qlity, indx 2=# of observations
      real              qcntwt(mxqual,2)                
c							! (input) indx 1=weighted # dscrpnt plrties for qlity, indx 2=sum of weights
      character*(*)     revsta(mxstat)                  
c							! (input) reversed station names
      integer           scnt(mxstat,2)                  
c							! (input) index 1=# of dscrpnt polarities for stat, index 2=# of obsrvations
      real              scntwt(mxstat,2)                
c							! (input) indx 1=weighted # dscrpnt polrties for stat, indx 2=sum of weights
      character*(*)     stat(mxstat)                    
c							! (input) names of all stations reporting
c
      character*2       estar                           
c							! flag indicates large discrepancy between actual and estimated error rates
      integer           i                               
c							! loop index
      integer           j                               
c							! dummy variable
      integer           k                               
c							! dummy variable
      integer           natot                           
c							! sum of # of polarities in agreement with solution
      integer           ncfit                           
c							! cumulative # of misfit scores
      integer           ndcrng                          
c							! cumulative # of solutions in dip range distribution
      integer           ndtot                           
c							! sum of # of polarities in discrepancy with solution
      integer           nrcrng                          
c							! cumulative # of solutions in rake range distribution
      integer           nscrng                          
c							! cumulative # of solutions in strike range distribution
      integer           ntot                            
c							! total # of observations
      real              rate                            
c							! weighted error rate per quality class
      character*1       star                            
c							! denotes station designated as reversed
      real              wtot                            
c							! summation of weights over all stations
c
      ndtot = 0
      ntot = 0
      wtot = 0.
      do 5 i = 1, nstat
        ndtot = ndtot + scnt(i, 1)
        ntot = ntot + scnt(i, 2)
        wtot = wtot + scntwt(i, 2)
5     continue
      natot = ntot - ndtot
c
c write out summary of polarity discrepancies by station
c
      write (eunit, 10)
10    format (/, 1x, 'SUMMARY OF STATIONS HAVING POLARITIES IN DISCREP',
     & 'ANCY WITH BEST FIT SOLUTION (* DENOTES REVERSED STATION)', /, 
     & 1x, ' STATION     DISCREPANCIES    AGREEMENTS       TOTAL ',
     & ' WEIGHTED ERROR RATE   TOTAL ERROR CONTRIBUTION', /)
c
c sort stations alphabetically
c
      call csort (stat, ind, nstat)
      do 40 i = 1, nstat
        j = ind(i)
        star = ' '
        do 20 k = 1, nrev
          if (stat(j) .eq. revsta(k)) star = '*'
20      continue
        write (eunit, 30) star, stat(j), scnt(j, 1), scnt(j, 2) -
     & scnt(j, 1), scnt(j, 2), scntwt(j, 1)/scntwt(j, 2),
     & scntwt(j, 1)/wtot
30      format (1x, a1, a5, 3(10x, i5), 9x, f6.3, 10x, f6.4)
40    continue
      write (eunit, 50) ndtot, natot, ntot
50    format (/, 1x, 'TOTAL', 3(8x, i7))
c
c write out summary of hand-picked polarity discrepancies by reading quality
c
      write (eunit, 70)
70    format (/, 1x, 'SUMMARY OF HAND-PICKED DATA WITH RESPECT TO BEST',
     & ' FIT SOLUTIONS', /, 1x, 
     & ' QUAL        DISCREPANCIES    AGREEMENTS       TOTAL     WEIG',
     & 'HTED ERROR RATE', /)
      ndtot = 0
      ntot = 0
      do 90 i = 1, mxqual/2
        estar = '  '
        ndtot = ndtot + qcnt(i, 1)
        ntot = ntot + qcnt(i, 2)
        if (qcntwt(i, 2) .eq. 0.) then
          rate = 0.
        else
          rate = qcntwt(i, 1)/qcntwt(i, 2)
          if (rate .ge. 0.0001) then
            if (abs((erate(i)-rate)/rate) .ge. 0.2) estar = '**'
          end if
        end if
        write (eunit, 80) i - 1, qcnt(i, 1), qcnt(i, 2) -
     & qcnt(i, 1), qcnt(i, 2), rate, estar
80      format (1x, 2x, i1, 2x, 3(8x, i7), 9x, f6.4, 1x, a3)
90    continue
      natot = ntot - ndtot
      write (eunit, 50) ndtot, natot, ntot
c
c write out summary of machine-picked polarity discrepancies by reading quality
c
      write (eunit, 110)
110   format (/, 1x, 'SUMMARY OF MACHINE-PICKED DATA WITH RESPECT TO B',
     & 'EST FIT SOLUTIONS', /, 1x, 
     & ' QUAL        DISCREPANCIES    AGREEMENTS       TOTAL     WEIG',
     & 'HTED ERROR RATE', /)
      ndtot = 0
      ntot = 0
      do 120 i = mxqual/2 + 1, mxqual
        estar =  '  '
        ndtot = ndtot + qcnt(i, 1)
        ntot = ntot + qcnt(i, 2)
        if (qcntwt(i, 2) .eq. 0.) then
          rate = 0.
        else
          rate = qcntwt(i, 1)/qcntwt(i, 2)
          if (rate .ge. 0.0001) then
            if (abs((erate(i)-rate)/rate) .ge. 0.2)  estar = '**'
          end if
        end if
        write (eunit, 80) i - mxqual/2 - 1, qcnt(i, 1), qcnt(i, 2) -
     & qcnt(i, 1), qcnt(i, 2), rate, estar
120   continue
      natot = ntot - ndtot
      write (eunit, 50) ndtot, natot, ntot
c
c write out distribution of fit parameters
c
      write (eunit, 130)
130   format (/, 1x, 'DISTRIBUTION OF SOLUTION MISFIT SCORES', /,
     & 1x, ' MISFIT SCORE     NUM    CUM NUM')
      ncfit = 0
      do 150 i = 1, 20
        ncfit = ncfit + nfit(i)
        write (eunit, 140) float(i - 1)*.025, float(i)*.025, nfit(i),
     1                     ncfit
140     format (1x, f5.3, ' - ', f5.3, 3x, i5, 3x, i6)
150   continue
c
c write out distribution of dip, strike, rake ranges for unrestricted searches
c
      if (ires .eq. 0) then
        write (eunit, 160)
160     format (/, 1x, 'DISTRIBUTION OF SOLUTION DIP RANGES', /,
     & 1x, ' RANGE       NUM    CUM NUM')
        ndcrng = 0
        do 180 i = 1, 10
          ndcrng = ndcrng + ndrng(i)
          write (eunit, 170) float(i - 1)*5.0, ndrng(i), ndcrng
170       format (1x, 1x, f5.1, 5x, i5, 3x, i6)
180     continue
        write (eunit, 190)
190     format (/, 1x, 'DISTRIBUTION OF SOLUTION STRIKE RANGES', /,
     & 1x, ' RANGE       NUM    CUM NUM')
        nscrng = 0
        do 200 i = 1, 19
          nscrng = nscrng + nsrng(i)
          write (eunit, 170) float(i - 1)*5.0, nsrng(i), nscrng
200     continue
        write (eunit, 210)
210     format (/, 1x, 'DISTRIBUTION OF SOLUTION RAKE RANGES', /,
     & 1x, ' RANGE       NUM    CUM NUM')
        nrcrng = 0
        do 220 i = 1, 19
          nrcrng = nrcrng + nrrng(i)
          write (eunit, 170) float(i - 1)*10.0, nrrng(i), nrcrng
220     continue
      end if
c
      write (eunit, *) 
      write (eunit, 230) ievr, ' EVENTS READ, ', ievp, ' PROCESSED'
230   format (2(i9, a))
c
      return
      end
