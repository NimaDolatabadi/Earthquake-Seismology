cProgram norgse
c
c   Mario Villagran, October 1995
c
c   converts nordic format files into GSE2.0 format 
c   as specified in GSETT-3 project to submit information 
c   to the IDC and viceversa.
c
c   Input:   nordic or GSE2.0 format file
c
c            if input is nordic, the corresponding hyp.out
c            and print.out are necesary as well as a correlative
c            number to identify the event in GSE2.0 format.
c
c            if input is GSE2.0 no more info is needed.
c      
c   Oputput: GSE2.0 or nordic format file
c
c   comments: When CONVERTING from nordic to GSE2.0
c
c             vector printout(1000*flines) contains space for 
c             700*flines print.out lines of an event
c             300*flines nordic file lines
c
c   comments: When CONVERTING from GSE2.0 to nordic
c
c             vector printout(1000*flines) contains space for 
c             nordic file lines
c
c             To extend the capacity you have to change
c             the parameter flines (factor to increase 
c             the size of the array 1, 2 ,3....).
c
c   Changes:
c   Feb       1997  m.v. : use time from the system to write GSE2.0
c   March 18             : converts full GSE2.0 <----> nordic, sun or pc
c                        : checks errors and display solutions
c   March 26             : implement 4 magnitudes possibility
c   Aug   19             : allows more than 1 blank line after
c                        : header, region and phases
c   Aug   28             : mb is appropriately included (subr mb_att appended)
c
c   1998
c
c   A major improvement in the program, now allows input and outputs
c   compact files for any format to any format (nordic, GSE2.0)
c
c   Jan   29        m.v. : now also number ID for phases
c   Jan   29        m.v. : creates compact.gse file only with headers.
c   Feb    3        m.v. : all events converted from GSE2.0 to nordic are "R"
c                          fixdepth indicator switched to uppercase "F"
c                          Mb indicator switched to uppercase "B"
c                          line type 1 is now flagged "1" at the 80 column
c                          includes if manual or automatic picking phase "am"
c   Nov   17        m.v. : problem with npha in convertion to gse2.0
c                          also declaring 'T' time valid for T phase or ampl.
c   1999
c   March 03        m.v. : changes after some comments from
c                          Victoria Oancea <oancea@cmr.gov> for the 
c                          compact.gse that is submitted to IDC (gamma info)
c                          see : 
c                             ~seismo/WOR/EXPERIMENTS/REPORTED_to_IDC/idc.mail
c   June 99  jh            : -----------------   SEISAN 7.0 ------------------
c                            no check for year 2000 or 5 char stations, small
c                            format bugs fixed
c                            use seisan systime, cut out conversion to 
c                            gmt in message
c   june 23  lo            : added interactive input
c   sep, 18 2000 lo        : tarray(6)+2000, for system time
c   jul 5   2001 jh        : distance to real
c   march 11 2002 lo       : changed names of output files, and added outfile for ARRIVAL
c   2009-04-30 pv          : nordic output ACTION: line fixed time problem
c   2015-05    jh          : small fix for fixin where event is found
c
      implicit none
c
c size of working array
      integer flines
      parameter        (flines=10)
c-- arguments, input file, location agency and mag agencies.
      character*132    printout(1000*flines),text   !SEE comments BEFORE
      character*80     args(10),infile,printhyp,region,top_directory
      character*30     ttext
      character        fecha*21,numero*8,sta*4,ph*4
      character*11     msta(150)
      character*3      lage,age1,age2,age3,def
c--crustal model, type of event, id of event,kind of magnitudes.
      character*1      model,type,id,fixd,cd,uno,am
      character*2      mc,mc1,mc2,mc3,mc4,evex,pl,r1,r2,r3,r4,r5
c--time variables, hypocentral parameters and magnitudes.
      integer          year,mo,dy,hr,mn,npha,nsta,number,gap,az
      integer          nsm1,nsm2,nsm3,nsm4,n,dist,reg,kstop,phnumber
      real             xdist
      real             mag,mag1,mag2,mag3,mag4,hdist,smag,sbmag,snr
      real             sec,lat,lon,dep,rms,tres,velo
      real             ermag1,ermag2,ermag3,x1,x2,x3,mw,smw,azph,raz
      real             timerr,x,smajor,sminor,erd,dmin,dmax,amp,per
c--ML,Mb
      real             a,b,c,d,smsta,q,equrad,polrad,pi,rad,radius
                       parameter (equrad = 6378.2064)
                       parameter (polrad = 6356.5838)
                       parameter (pi = 3.14159265)
                       parameter (rad = pi/180.)

      integer*4        stime,tarray(9),time,nmsta
      character*12     t1   ! for seisan system time
      character*14     t2   ! ---------------------
c---ellipse error
      real             ery,erx,erz,cvxy,cvxz,cvyz
c---number of arguments and counters
      integer nars,ncards,i,j,k,ncard,npcard,namp,work,karr,jarr,nmw
      integer nbamp
c--compact or not
      logical compact
c--computer type
      logical pc, sun,linux
c---
c

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

        call computer_type(sun,pc,linux)
c
c   check if input from file given in argument
c   display help if necesary
c
      call get_arguments(nars,args)

      if (nars.eq.0) then

        write(6,*) ' Choose option: '
        write(6,*) '     GSE2.0 -> nordic (1) or '
        write(6,*) '     nordic -> GSE2.0 (2)'
        read(*,'(a)') args(1)
        if (args(1).eq.'1') then
          write(*,*) ' GSE2 input file '
          read(*,'(a)') args(2)
        elseif (args(1).eq.'2') then
          write(*,*) ' Nordic input file '
          read(*,'(a)') args(2)
          write(*,*) ' Hypocenter output file (default=print.out)'
          read(*,'(a)') args(3)
          if (args(3).eq.' '.or.args(3).eq.'') args(3)='print.out'
          write(*,*) ' Number of first event'
          read(*,'(a)') args(4)
          write(*,*) ' Number of first phase'
          read(*,'(a)') args(5)
        endif

      elseif (nars.eq.1.and.args(0).eq.'-help') then
5       write(6,*)'          '
        write(6,*)' You must define the desired convertion type,'
        write(6,*)' input files and id number (this last only case 2)'
        write(6,*)' 1 for GSE2.0------> nordic or'
        write(6,*)' 2 for nordic------> GSE2.0'
        write(6,*)' examples:'
        write(6,*)'          '
        write(6,*)' norgse 1 xx.inp'
        write(6,*)'          '
        write(6,*)' This converts file xx.inp from GSE2.0 to nordic'
        write(6,*)' output is file norgse.nor'
        write(6,*)'          '
        write(6,*)' norgse 2 hyp.out print.out 35 850'
        write(6,*)'          '
        write(6,*)' This creates a GSE2.0 file from the hyp.out and'
        write(6,*)' print.out files and gives to the first event '
        write(6,*)' in list the number 35, and 850 to the first phase'
        write(6,*)' if number is not defined 001 is the default'
        write(6,*)' for both, output files are: '
        write(6,*)'   gse_bulletin.out, gse_origin.out and '//
     &            'gse_arrival.out '
        write(6,*)'          '
        stop
      endif
c
c check arguments
c get input file names and decides what to do
c open files
c
      pl='+-'
      read(args(1),'(i1)',err=5)work
      if((work.eq.1.and.nars.gt.2).or.(nars.gt.5).or.
     *(work.gt.2).or.(work.lt.1))then
        write(6,*)' Wrong arguments !!!!!!!!!!!'
        write(6,*)'    '
        goto 5
      endif
      infile=args(2)
      open(1,file=infile,status='old')
c      stime=time()
      call systime(t1,t2)   ! seisan general routine
      read(t1,'(5i2)') (tarray(i),i=6,2,-1)
c      call gmtime(stime, tarray)      !GMT time
c      write(ttext(1:30),'(a7,i4,3(a1,i2),i2,a8)')
c     *'MSG_ID ',tarray(6)+2000,'/',tarray(5)+1,'/',tarray(4),'_',
c     *tarray(3),tarray(2),' ISR_NDC'
c      do i=13,22   !fill blanks
c        if(ttext(i:i).eq.' ')ttext(i:i)='0'
c      enddo

      if(work.eq.1)goto 50
c
c   HERE STARTS CONVERTING FROM NORDIC TO GSE
c
      printhyp=args(3)
      read(args(4),'(i5)',err=6)number
6     if(number.eq.0)number=1
      read(args(5),'(i5)',err=7)phnumber
7     if(phnumber.eq.0)phnumber=1
      open(7,file=printhyp,status='old')
c      open(2,file='norgse.gse',status='unknown')
      open(2,file='gse_bulletin.out',status='unknown')
      open(4,file='gse_origin.out',status='unknown')
      open(3,file='gse_arrival.out',status='unknown')
c      open(4,file='compact.gse',status='unknown')
c
c   check if it is a correct s-file
c
      call nortype(1,compact)
      if(compact) then
       write(6,*)' The file must be an F-file'
       stop
      endif
c
c Gets the time at which the file is done if sun GMT time
c note that on pc the time is local unless some extra-arrangement
c   WRITE GSE2.0 EXCHANGE DATA LINES
c
c
c this part should really not be done here LO!
c

c      write(2,'(a12)')'BEGIN GSE2.0'
c      write(2,'(a13)')'MSG_TYPE DATA'
c      write(2,'(a30)')ttext(1:30)
c      write(2,'(a23)')'DATA_TYPE ORIGIN GSE2.0'
c      write(4,'(a12)')'BEGIN GSE2.0'
c      write(4,'(a13)')'MSG_TYPE DATA'
c      write(4,'(a30)')ttext(1:30)
c      write(4,'(a23)')'DATA_TYPE ORIGIN GSE2.0'

c
c   NORDIC ----> GSE2.0    BIG LOOP STARTS HERE
c
      n=0        !  number of processed events
      a=1.00     ! default calculation of ML is
      b=1.11     ! Hutton & Boore, a,b,c,d
      c=0.00189
      d=-1.9680
c magnitude constants for ML (check if other values in STATION0.HYP)
      call topdir(top_directory)
      i=index(top_directory,' ')-1
      text(1:i+17)=top_directory(1:i)//'/DAT/STATION0.HYP'
      open(8,file=text(1:i+17),status='old')
8     read(8,'(a)')text(1:40)
      if(text(1:5).eq.'RESET')then
       if(text(12:13).eq.'75')read(text(16:),*)a
       if(text(12:13).eq.'76')read(text(16:),*)b
       if(text(12:13).eq.'77')read(text(16:),*)c
       if(text(12:13).eq.'78')read(text(16:),*)d
      endif
      close(8)
c
c  initialize variables
c
  10  continue
        mc1(1:2)='  '
        mc2(1:2)='  '
        mc3(1:2)='  '
        mc4(1:2)='  '
        mag1=0.0
        mag2=0.0
        mag3=0.0
        mag4=0.0
        ermag1=0.0
        ermag2=0.0
        ermag3=0.0
        region(1:80)='  '
        nsta=0
        npha=0
        dmax=0.0
        dmin=15000.00
c
c   GET NECESARY INFORMATION FROM print.out AND THE F-file
c
        ncards=1
15      read(7,'(a80)',end=99)printout(1)(1:80)
           if(printout(1)(1:8).eq.' EVENT #')goto 20
        goto 15
20      ncards=ncards+1
         read(7,'(a80)',end=28)printout(ncards)(1:80)
         if(printout(ncards)(1:10).eq.'----------')goto 28
c
c displays possible errors and stop the program
c
         if(ncards.gt.650*flines)then
          write(6,*)printout(1)(1:20),'to many print.out lines!!'
          write(6,*)'to solve this, edit norgse.f, read comments'
          goto 99
22        write(6,*)'print.out and S-file not matching !!!'
24        goto 99
         endif
        goto 20
28      continue
        do i=1,ncards
          if(printout(i)(1:19).eq.' Origin time error:')
     *    read(printout(i)(20:60),*)timerr
          if(printout(i)(1:18).eq.'   date hrmn   sec')
     *        read(printout(i+1)(1:80),'(73x,f5.1)')erd
          if(printout(i)(6:19).eq.'multiple-phase')
     *    read(printout(i)(1:5),'(i5)')npha
          if(printout(i)(1:14).eq.' Azimuthal Gap')
     *    read(printout(i)(36:38),'(i3)')gap
          if(printout(i)(1:17).eq.' stn   dist   azm')then
             do j=i+1,ncards
               if(printout(j)(1:3).eq.'   ')goto 30
                 read(printout(j)(1:11),'(7x,i4)')k
               if(dmax.lt.k)dmax=k
               if(dmin.gt.k)dmin=k
             enddo
          endif
30        continue
        enddo
c
c read info from current event and
c place the file ready to read next event
c
        ncard=700*flines  !reserve 701*flines to 1000*flines for nordic 
        namp=0
        nbamp=0
        nsm4=0
        npcard=0
        k=0
        smag=0.0
        sbmag=0.0
        smw=0.0
        nmw=0
        nmsta=0
        smsta=0
35      read(1,'(a80)',end=22)text(1:80)
        if(text(1:20).eq.'                    '.
     *      and.text(80:80).eq.' ')goto 40
        ncard=ncard+1
        if(k.eq.0)then
         if(text(80:80).eq.'1'.or.text(80:80).eq.' ')then
          if(text(22:22).eq.'L'.or.text(22:22).eq.'R'.or.
     *    text(22:22).eq.'D')then
            read(text(1:80),100)year,mo,dy,hr,
     *      mn,sec,model,type,id,lat,lon,dep,fixd,lage,nsta,
     *      rms,mag1,mc1(2:2),age1,mag2,mc2(2:2),age2,mag3,
     *      mc3(2:2),age3,uno
            if(mc1(2:2).eq.'C')mag1=0.0        !MC = MD
            if(mc1(2:2).eq.'C')mc1(2:2)=' '    !to avoid repetition
            if(mc2(2:2).eq.'C')mag2=0.0
            if(mc2(2:2).eq.'C')mc2(2:2)=' '
            if(mc3(2:2).eq.'C')mag3=0.0
            if(mc3(2:2).eq.'C')mc3(2:2)=' '
            k=1
          endif
         endif
        endif
        if(text(80:80).eq.'E')
     *    read(text(1:80),'(23X,F7.3,1X,F7.3,F5.1,3E12.4)')
     *    ery,erx,erz,cvxy,cvxz,cvyz
        if(text(2:15).eq.'Ellipse_as_IDC')
     *    read(text(1:80),'(16x,2(f6.1,1x),i3)')smajor,sminor,az
c statistics for MW
        if(text(80:80).eq.'3'.and.text(7:11).ne.'AVERA'.and.
     *  text(72:73).eq.'MW')then
          read(text(75:78),'(f4.1)')mw
          if(mc1(2:2).eq.'W')mag=mag1
          if(mc2(2:2).eq.'W')mag=mag2
          if(mc3(2:2).eq.'W')mag=mag3
          nmw=nmw+1
          smw=smw+(mw-mag)**2
        endif
c get stations with MD
        if(text(80:80).eq.'3'.and.text(2:2).eq.'$')then
          nmsta=nmsta+1
          msta(nmsta)=text(3:13)
          read(msta(nmsta)(9:11),'(f3.1)')x1
          smsta=smsta+x1
        endif
c
c NOTE:
c get standard deviation of IPRG magnitude and number of stations
c used to get it, this in case that s-file was obtained/converted
c from iprg format to nordic (using isrnor), IF NOT  ermag3 and nsm4 
c are obtained later when converting the arrivals.(see ARRIVAL LINES)
c
        if(text(1:5).eq.' O.T.')then
         do i=20,60
          if(text(i:i+3).eq.'ML =')
     *     read(text(i+5:i+19),'(f3.1,4x,f3.1,3x,i2)')mag4,ermag3,nsm4
         enddo
        endif
c sign where arrival lines start (appended on March 18,1997) m.v.
        if(text(1:5).eq.' STAT')npcard=ncard+1
c get number of amplitudes and stand dev of MLs
        if(mc1(2:2).eq.'L')mag=mag1
        if(mc2(2:2).eq.'L')mag=mag2
        if(mc3(2:2).eq.'L')mag=mag3
        if(npcard.gt.0.and.text(34:40).ne.'       '.and.
     *  text(34:40).ne.' AMPLIT')then
          read(text(1:80),'(33x,g8.1,f4.1,25x,f5.0)')amp,per,xdist
          dist=xdist
          hdist=sqrt(real(dist)*real(dist)+dep*dep)
          if(amp.gt.0.0.and.per.lt.5.0)then
            if(text(11:11).ne.'P')then
              namp=namp+1
              smag=smag+(a*alog10(amp)+b*alog10(hdist)+c*hdist+d-mag)**2
            else
              nbamp=nbamp+1
              hdist=real(dist)/111.2  ! hdist is distance in deg
              call mb_att(dep,hdist,q)
              sbmag=sbmag+alog10(amp/per)+q
              amp=0.0
            endif
          endif
        endif
c region
        if(text(1:2).eq.' *')then
          do i=3,80
           if(text(i:i).eq.'*')then
              region(1:)=text(4:i-2)
              call sei upc(region)
              goto 39
           endif
          enddo
        endif
c copy the f-file
39      printout(ncard)(1:80)=text(1:80)
        goto 35
c
c arrange magnitudes
c
40      continue
        if(mc1(2:2).ne.' ')mc1(1:1)='M'
        if(mc2(2:2).ne.' ')mc2(1:1)='M'
        if(mc3(2:2).ne.' ')mc3(1:1)='M'
        if(mag4.gt.0.0)mc4(1:2)='MD'
c
c  get ellipse dimensions
c  
c
c NOW commented out to implement the new NDC ellipse calculation IDCwise...
c  M.V. April 18, 1999.
c       call xy_ellipse(ery,erx,erz,cvxy,cvxz,cvyz,smajor,sminor,x)
c       az=x
c       if(az.lt.0)az=az+360
c
c
        npha=npha+nsta
c
c writes date in "fecha" variable and fill blanks with zeros
c
        write(fecha(1:21),'(i4,2(a1,i2),1x,i2,a1,i2,a1,f4.1)')
     *   year,'/',mo,'/',dy,hr,':',mn,':',sec
        do i=1,21
          if(fecha(i:i).eq.' ')fecha(i:i)='0'
        enddo
        fecha(11:11)=' '
c
c   prepares the event id line in GSE2.0 format
c
        numero(1:8)=' '
        write(numero(1:8),'(i8)')number
        do i=1,8
          if(numero(i:i).eq.' ')numero(i:i)='0'
        enddo
c EVENT ID LINE       !!start writing here
        write(2,'(a5,1x,a8)')'EVENT',numero(1:8)
c
c next line goes out after ~seismo/WOR/EXPERIMENTS/REPORTED_to_IDC/idc.mail
c       write(4,'(a5,1x,a8)')'EVENT',numero(1:8)
c HEADER EVENT LINES
        write(2,150)
        write(2,200)
        if(n.eq.0)write(4,150)
        if(n.eq.0)write(4,200)
c BLANK LINE
        write(2,*)' '
        write(4,*)' '
c ORIGIN LINE 1
        if(nmsta.gt.0)then
        mc4='MD'
        nsm4=nmsta
        mag4=smsta/float(nmsta)
        smsta=0.0
           do k=1,nmsta
             read(msta(nmsta)(9:11),'(f3.1)')x1
             smsta=smsta+(x1-mag4)**2
           enddo
        ermag3=sqrt(smsta/nmsta)
        endif
        if(fixd.eq.'F')fixd='f'
        write(text(1:122),250)fecha(1:21),lat,lon,dep,fixd,
     *  npha,nsta,gap,mc4,mag4,nsm4,numero
        if(mc1.ne.' ')write(text(72:77),'(a2,f4.1)')mc1,mag1
        if(mc1(2:2).eq.'L')write(text(79:80),'(i2)')namp
        if(mc1(2:2).eq.'B')write(text(79:80),'(i2)')nbamp
        if(mc2.ne.' ')write(text(83:88),'(a2,f4.1)')mc2,mag2
        if(mc2(2:2).eq.'L')write(text(90:91),'(i2)')namp
        if(mc2(2:2).eq.'B')write(text(90:91),'(i2)')nbamp
        if(mc3(2:2).ne.' '.and.mc2(2:2).eq.' ')then
         write(text(83:88),'(a2,f4.1)')mc3,mag3
         if(mc3(2:2).eq.'L')write(text(90:91),'(i2)')namp
         if(mc3(2:2).eq.'B')write(text(90:91),'(i2)')nbamp
        endif
c shifts MD to 1st or second position depending on its existence
c if MD is 0.0, only cleans the place
        if(text(96:99).eq.' 0.0')then
          text(94:102)='         '
        else
          if(text(72:73).eq.'  ')then
             text(72:80)=text(94:102)
             text(94:102)='         '
          else
             if(text(83:91).eq.'  ')then
               text(83:91)=text(94:102)
               text(94:102)='         '
             endif
          endif
        endif
        write(2,'(a)')text(1:122)
        write(4,'(a)')text(1:122)
c ORIGIN LINE 2
        radius = cos(lat)**2/equrad**2
        radius = (radius +sin(lat)**2/polrad**2)**(-.5)
        dmin = real(dmin)/(radius*rad)
        dmax = real(dmax)/(radius*rad)
        evex='se'
        if(id.eq.' '.and.mag4.gt.2.5)evex='ke'
        if(id.eq.'E')evex='km'
        if(id.eq.'P')evex='sm'
        write(text(1:110),300)rms,pl,timerr,smajor,sminor,az,pl,erd,
     *  dmin,dmax,pl,ermag1,pl,ermag2,pl,ermag3,'m i',evex
        write(text(75:77),'(a3)')'0.0'
        write(text(86:88),'(a3)')'0.0'
        if(mc1(2:2).eq.'L'.and.namp.gt.1)
     *  write(text(75:77),'(f3.1)')sqrt(smag/namp)
        if(mc1(2:2).eq.'B'.and.nbamp.gt.1)
     *  write(text(75:77),'(f3.1)')sqrt(sbmag/nbamp)
        if(mc1(2:2).eq.'W'.and.nmw.gt.1)
     *  write(text(75:77),'(f3.1)')sqrt(smw/nmw)
        if(mc2(2:2).eq.'L'.and.amp.gt.1)
     *  write(text(86:88),'(f3.1)')sqrt(smag/namp)
        if(mc2(2:2).eq.'B'.and.nbamp.gt.1)
     *  write(text(86:88),'(f3.1)')sqrt(sbmag/nbamp)
        if(mc2(2:2).eq.'W'.and.nmw.gt.1)
     *  write(text(86:88),'(f3.1)')sqrt(smw/nmw)
        if(mc3(2:2).eq.'L'.and.amp.gt.1.and.mag3.gt.0.0.and.
     *  mc2(2:2).eq.' ')write(text(86:88),'(f3.1)')sqrt(smag/namp)
        if(mc3(2:2).eq.'B'.and.nbamp.gt.1.and.mag3.gt.0.0.and.
     *  mc2(2:2).eq.' ')write(text(86:88),'(f3.1)')sqrt(sbmag/nbamp)
        if(mc3(2:2).eq.'W'.and.nmw.gt.1.and.mag3.gt.0.0.and.
     *  mc2(2:2).eq.' ')write(text(86:88),'(f3.1)')sqrt(smw/nmw)
        if(mag1.eq.0.0)write(text(73:77),'(a5)')'     '
        if(mag2.eq.0.0)write(text(84:88),'(a5)')'     '
c shifts MD errors to 1st or second position depending on its existence
c if MD is 0.0, only cleans the place
        if(nsm4.eq.0)then
          text(95:99)='     '
        else
          if(text(73:77).eq.'     ')then
             text(73:77)=text(95:99)
             text(95:99)='     '
          else
             if(text(84:88).eq.'     ')then
               text(84:88)=text(95:99)
               text(95:99)='     '
             endif
          endif
        endif
        write(2,'(a)')text(1:110)
        write(4,'(a)')text(1:110)
c BLANK LINE
        write(2,*)'                              '
        write(4,*)'                              '
c REGION LINE (if)
        if(region(1:5).ne.'     ')write(2,'(a)')region
c HEADER ARRIVAL LINE
        if(ncard-npcard.gt.0)then
        write(2,220)
        if (n.eq.0)write(3,220)
c ARRIVAL LINES
         do i=npcard,ncard
           amp=0.0
           mag=0.0
           def='   '
           mc(1:2)='  '
           read(printout(i)(2:80),1201)sta,ph,cd,hr,mn,sec,amp,
     *     per,azph,velo,snr,k,tres,xdist,az
           raz=real(k)
           if(printout(i)(15:15).ne.'4'.and.printout(i)(11:11).ne.' '.
     *     and.printout(i)(11:11).ne.'T')def(1:1)='T'
           am=' '
           if(printout(i)(16:16).eq.'A')am='a'
           if(printout(i)(16:16).eq.' ')am='m'
           if(cd.eq.'C')cd='c'
           if(cd.eq.'D')cd='d'
           hdist=sqrt(real(dist)*real(dist)+dep*dep)
           if(amp.gt.0.0.and.per.lt.5.0)then
            mag=a*alog10(amp)+b*alog10(hdist)+c*hdist+d
            mc='ML'
            if(ph(1:1).eq.'P')then
               hdist=real(dist)/(radius*rad) ! hdist is distance in deg
               call mb_att(dep,hdist,q)
               mag=alog10(amp/per)+q
               mc='Mb'
            endif
           endif
           write(fecha(12:21),'(2(i2,a1),f4.1)')hr,':',mn,':',sec
           do k=12,21
             if(fecha(k:k).eq.' ')fecha(k:k)='0'
           enddo
           mc4='  '
           if(nmsta.gt.0)then
             do k=1,nmsta
               if(sta(1:4).eq.msta(k)(1:4))then
                  read(msta(k)(9:11),'(f3.1)')mag4
                  mc4='MD'
               endif
             enddo
           endif
           write(numero(1:8),'(i8)')phnumber
           do k=1,8
             if(numero(k:k).eq.' ')numero(k:k)='0'
           enddo
c easy convertion from km to deg...not so accurate.
           x3 = real(dist)/(radius*rad)
           write(text(1:132),140)sta,x3,real(az),am,
     *     cd,ph,fecha,tres,azph,raz,velo,def(1:3),snr,amp,per,mc,
     *     mag,mc4,mag4,numero
           if(text(24:24).eq.' ')text(54:58)='     '
           if(text(100:109).eq.' 0.0  0.00')text(100:109)='          '
           if(text(113:116).eq.' 0.0')text(113:116)='    '
           if(text(118:119).eq.'  ')text(120:123)='    '
           if(azph.eq.0.0)text(60:64)='     '
           if(azph.gt.0.0)text(86:86)='A'
           if(velo.eq.0.0)text(73:77)='     '
           if(velo.gt.0.0)text(87:87)='S'
           if(snr.eq.0.0)text(89:93)='     '
           if(raz.eq.0.0)text(66:71)='      '
           write(2,'(a)')text(1:132)
           write(3,'(a)')text(1:132)
           phnumber=phnumber+1
         enddo
        endif
c BLANK LINE
        write(2,*)'                              '
c write in nordic format also file 'norgse.nor'
c       write(8,'(a80)')(printout(i)(1:80),i=701*flines,ncard+1)
        n=n+1
        number=number+1
      goto 10
c   end of BIG LOOP
c
c   HERE STARTS IF CONVERTING FROM GSE2.0 TO NORDIC
50    open(2,file='norgse.nor',status='unknown')
      n=0  !number of events
      ncards=i
      ncard=0
      kstop=0
c
c   GSE2.0 ----> NORDIC    BIG LOOP STARTS HERE
c
c     NEW EVENT
c
c the variables printout(1*flines)-->printout(350*flines)
c are reserved for lines above the nordic line type 7 (also included
c and printout(351*flines)-->printout(999*flines) for arrival lines
c
c
      write(printout(1000*flines)(1:80),'(a,a)')
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU ',
c     *'VELO SNR AR TRES W  DIS CAZ7'
     *'VELO AIN AR TRES W  DIS CAZ7'
60    continue
      karr=350*flines   !the arrival's counter
      j=0               !the header's counter
      jarr=1            !defines if input is short or long input file
      dmax=0.0
      dmin=15000.00
      mag1=0.0
      mag2=0.0
      mag3=0.0
      mc1(1:2)='  '
      mc2(1:2)='  '
      mc3(1:2)='  '
      nsta=0
      if(jarr.eq.0)write(2,'(a80)')printout(1000*flines)(1:80)
      if(jarr.eq.0)write(2,'(a80)')'    '       !case of short input
65    continue
        read(1,'(a)',end=99)text
        if(text(1:4).eq.'STOP')then
          if(n.eq.0)then
            write(6,*)'No events were found in input file !!!'
68          write(6,*)'Error, Check event # ',n
          endif
          goto 99
        endif
c
c Identifies that there is an event, creates location line 
c of nordic format and ID line
c
        if(text(1:5).eq.'EVENT'.or.text(1:6).eq.'Event ')then
           n=n+1
cxx
           write(6,*) text
        endif
        if(text(8:21).eq.'rms   OT_Error')then     !LONG IG starts here
c allows more than 1 blank line after header (german gse reports case)
c Aug 19, 1997....m.v.
69       read(1,'(a)',end=99)text     !reads blank lines
         if(text(5:5).ne.'/')goto 69
         backspace(1)
c read first header line
         read(1,255,end=99,err=68)year,mo,dy,hr,mn,sec,
     *   lat,lon,dep,fixd,npha,nsta,gap,mc1,mag1,
     *   nsm1,mc2,mag2,nsm2,mc3,mag3,nsm3,lage,numero
cxx
         write(6,*) year,mo,dy,hr,mn

         do i=1,3
         if(lage(i:i).eq.' '.or.lage(i:i).eq.'_')lage(i:i)='X'
         enddo
         if(lage(1:3).eq.'XRE')lage(1:3)='REB'
         if(fixd.eq.'f')fixd='F'
         if(mc1(2:2).eq.'b')mc1(2:2)='B'
         if(mc2(2:2).eq.'b')mc2(2:2)='B'
         if(mc3(2:2).eq.'b')mc3(2:2)='B'
         if(mc1(2:2).eq.'s')mc1(2:2)='S'
         if(mc2(2:2).eq.'s')mc2(2:2)='S'
         if(mc3(2:2).eq.'s')mc3(2:2)='S'
c this line only for Israel
c        if(lage(1:3).ne.'IDC'.and.lage(1:3).ne.'REB')lage(1:3)='ISN'
c read second header line
70       read(1,'(a)',end=99)text     !reads blank lines
         if(text(14:15).ne.'+-')goto 70
         backspace(1)
         read(1,300,end=99,err=68)rms,r1,timerr,smajor,sminor,az,r2,
     *   erd,x1,x2,r3,ermag1,r4,ermag2,r5,ermag3,text(1:3),evex
c line type 1
         write(printout(j+1),100)year,mo,dy,hr,mn,sec,' ','R',' ',lat,
     *   lon,dep,fixd,lage,nsta,rms,mag1,mc1(2:2),'   ',mag2,
     *   mc2(2:2),'   ',mag3,mc3(2:2),'   ','1'
         printout(j+1)(80:80)=' '   !to avoid strange things
         if(evex.eq.'km')printout(j+1)(23:23)='E'
         if(evex.eq.'sm')printout(j+1)(23:23)='P'
         do i=7,18
          if(printout(j+1)(i:i).eq.' ')printout(j+1)(i:i)='0'
         enddo
         printout(j+1)(11:11)=' '
         printout(j+1)(16:16)=' '
         if(mag1.gt.0.0)printout(j+1)(61:63)=lage(1:3)
         if(mag1.eq.0.0)printout(j+1)(57:60)='    '
         if(mag2.gt.0.0)printout(j+1)(69:71)=lage(1:3)
         if(mag2.eq.0.0)printout(j+1)(65:68)='    '
         if(mag3.gt.0.0)printout(j+1)(77:79)=lage(1:3)
         if(mag3.eq.0.0)printout(j+1)(73:76)='    '
c special next 3 lines for Israel. Does not affect other agencies !!
         if(mc1(2:2).eq.'L'.and.lage.eq.'ISN'.and.
     *   mag1.gt.0.0)printout(j+1)(60:60)='C'
         if(mc2(2:2).eq.'L'.and.lage.eq.'ISN'.and.
     *   mag2.gt.0.0)printout(j+1)(68:68)='C'
         if(mc3(2:2).eq.'L'.and.lage.eq.'ISN'.and.
     *   mag3.gt.0.0)printout(j+1)(76:76)='C'
c line type I
c        write(6,*)printout(j+1)
c pv : ttext string did not work !!
      call systime(t1,t2)   ! seisan general routine
         write(printout(j+2)(1:80),'(a12,a14,a34,i4,5i2,a6)')
     *   ' ACTION:REG ',t2,
     *   ' OP:ngse STATUS:               ID:',year,mo,dy,hr,mn,
     *   int(sec),'     I'
c        write(printout(j+2)(1:80),'(a12,a11,1x,a2,a34,i4,5i2,a6)')
c    *   ' ACTION:REG ',ttext(10:20),ttext(21:22),
c    *   ' OP:ngse STATUS:               ID:',year,mo,dy,hr,mn,
c    *   int(sec),'     I'
         printout(j+2)(15:15)='-'
         printout(j+2)(18:18)='-'
         printout(j+2)(21:21)=' '
         printout(j+2)(24:24)=':'
         do i=61,74
           if(printout(j+2)(i:i).eq.' ')printout(j+2)(i:i)='0'
         enddo
c This line to keep some extra information from IDC that nordic
c format does not have specific fields...ellipse and qk ID
         if(mc1(2:2).eq.'B'.and.mag1.gt.0.0)x3=ermag1
         if(mc2(2:2).eq.'B'.and.mag2.gt.0.0)x3=ermag2
         if(mc3(2:2).eq.'B'.and.mag3.gt.0.0)x3=ermag3
         write(printout(j+3),'(1x,a13,2(f6.1,1x),i3,a9,f5.1,12x,
     *   a9,a8,5x,a1)')'IDC: ellipse ',smajor,sminor,az,' MB sd+- ',
     *   x3,'Event ID ',numero,'3'
         if(printout(1)(60:60).ne.'B'.and.printout(1)(68:68).ne.'B'.
     *   and.printout(1)(76:76).ne.'B')printout(j+3)(41:45)='     '
         j=j+3
c in case region is specified adds one line type 3
         reg=0
73       read(1,'(a)',end=99)text     !  reads blank lines
         if(text(1:7).eq.'Sta    ')then
           jarr=0
           goto 80
         else 
           if(text(1:7).eq.'       ')goto 73
           j=j+1
           write(printout(j),'(1x,a2,a50,a3,23x,a1)')
     *     '* ',text(1:50),'***','3'
           reg=1
           goto 73
         endif
c ARRIVALS
80       read(1,'(a)',end=99)text
         if(text.eq.'          ')then
81         read(1,'(a)',end=99)text
           if(text(1:4).ne.'    '.and.text(1:6).ne.'EVENT '.
     *     and.text(1:6).ne.'Event '.and.text(1:4).ne.'STOP')goto 85
           if(text(1:4).eq.'    ')goto 81
           backspace(1)
           if(text(1:4).eq.'STOP')kstop=1
c
c          if(mag1.eq.0.and.mag2.eq.0.and.mag3.eq.0.and.dep.le.1.0.
c    *     and.dmin.lt.1.0.and.dmax.lt.1.5)printout(j+1)(23:23)='P'
c          if(dmax.lt.9.0.and.dmax.gt.5.5.and.
c    *     dmin.gt.1.0)printout(j+1)(22:22)='R'
c          if(dmax.gt.9.0.and.dmin.gt.5.0)printout(j+1)(22:22)='D'
c
           printout(1)(80:80)='1'
82         write(2,'(1x,a79)')(printout(i)(2:80),i=1,j)
           write(2,'(a80)')printout(1000*flines)(1:80)
           write(2,'(1x,a79)')(printout(i)(2:80),i=350*flines+1,karr)
           write(2,'(a80)')'    '
           goto 60
         endif
85       karr=karr+1
         read(text,140)sta,x1,x2,am,cd,ph,fecha,tres,azph,raz,velo,
     *   def,snr,amp,per,mc,mag,mc4,mag4,i
         if(x1.lt.dmin)dmin=x1
         if(x1.gt.dmax)dmax=x1
         if(mag4.gt.0.0)then
          j=j+1
          write(printout(j)(1:80),'(1x,a1,a4,1x,a2,1x,f3.1,66x,a1)')
     *    '$',sta,'MD',mag4,'3'
         endif
         if(cd.ne.' ')call sei upc(cd)
c        if(ph.ne.'  ')call sei upc(ph)
c        ph(2:2)=' '    !not sure about n and g phases
         read(fecha(12:21),'(2(i2,1x),f4.1)')hr,mn,sec
         write(ttext(1:6),'(f6.1)')raz  !approximate az residual
         read(ttext(6:6),'(i1)')k
         if(k.ge.5)raz=raz+1.0
         radius = cos(lat)**2/equrad**2
         radius = (radius +sin(lat)**2/polrad**2)**(-.5)
         radius = radius*x1*rad
         if(velo.gt.999.9)velo=999.9
         if(amp.lt.99.)then !  Stupid compiler 
          write(printout(karr)(2:80),120)sta,ph,cd,hr,mn,sec,
     *    amp,per,azph,velo,snr,int(raz),tres,int(radius),int(x2)
         else
          write(printout(karr)(2:80),122)sta,ph,cd,hr,mn,sec,
     *    amp,per,azph,velo,snr,int(raz),tres,int(radius),int(x2)
         endif
         if(amp.eq.0.0)printout(karr)(34:45)='             '
         if(azph.eq.0.0)printout(karr)(46:51)='      '
         if(velo.eq.0.0)printout(karr)(52:56)='     '
         if(snr.gt.99.9)write(printout(karr)(57:60),'(f4.0)')snr
         if(snr.gt.999.9)printout(karr)(57:60)='999.'
         if(snr.eq.0.0)printout(karr)(57:60)='    '
         if(raz.eq.0.0)printout(karr)(61:63)='   '
         if(raz.lt.-99.9)printout(karr)(61:63)='-99'
         if(tres.lt.-99.9.or.tres.gt.999.9)
     *   write(printout(karr)(64:68),'(f5.0)')tres
         if(def(1:1).eq.' ')write(printout(karr)(15:15),'(a1)')'4'
         if(am.eq.'a')write(printout(karr)(16:16),'(a1)')am
c        printout(karr)(7:8)='SZ'
         if(printout(karr)(19:19).eq.' ')printout(karr)(19:19)='0'
         if(printout(karr)(21:21).eq.' ')printout(karr)(21:21)='0'
         if(printout(karr)(24:24).eq.' ')printout(karr)(24:24)='0'
         goto 80
        endif      !LONG IF ends here
      goto 65
c
c   FORMATS   !!!!!
c
 100    format(1x,i4,2(1x,2i2),1x,f4.1,3a1,f7.3,f8.3,f5.1,a1,1x,a3,
     *  i3,f4.1,3(1x,f3.1,a1,a3),a1)
 120    format(a4,5x,a4,2x,a1,1x,2i2,1x,f5.2,5x,g8.2,f4.1,f6.1,f5.1,
     *  f4.1,i3,f5.1,2x,i5,1x,i3,1x)

 1201   format(a4,5x,a4,2x,a1,1x,2i2,1x,f5.2,5x,g8.2,f4.1,f6.1,f5.1,
     *  f4.1,i3,f5.1,2x,f5.0,1x,i3,1x)

 122    format(a4,5x,a4,2x,a1,1x,2i2,1x,f5.2,5x,g8.1,f4.1,f6.1,f5.1,
     *  f4.1,i3,f5.1,2x,i5,1x,i3,1x)
 140    format(a4,2x,f6.2,f6.1,1x,2a1,2x,a4,4x,a21,1x,f5.1,1x,f5.1,
     *  1x,2f6.1,7x,a3,f6.1,1x,f9.1,1x,f5.2,2(1x,a2,f4.1),1x,a8)
 150    format(3x,'Date',7x,'Time',7x,'Latitude',1x,'Longitude',4x,
     *  'Depth',4x,'Ndef',1x,'Nsta',1x,'Gap',4x,'Mag1',2x,'N',4x,
     *  'Mag2',2x,'N',4x,'Mag3',2x,'N',2x,'Author',10x,'ID')
 200    format(7x,'rms',3x,'OT_Error',6x,'Smajor',1x,'Sminor',1x,
     *  'Az',8x,'Err',3x,'mdist',2x,'Mdist',5x,'Err',8x,'Err',8x,
     *  'Err',5x,'Quality')
 220    format('Sta',5x,'Dist  EvAz',5x,'Phase',6x,'Date',7x,
     *  'Time',5x,'TRes  Azim  AzRes  Slow  SRes Def   SNR'
     *  7x,'Amp   Per   Mag1   Mag2   Arr ID')
c write GSE2.0 format, nsta, no fixing flags for origin time and location.
 250    format(a21,4x,f8.4,1x,f9.4,4x,f5.1,1x,a1,2x,i4,1x,i4,
     *  1x,i3,2x,22x,a2,f4.1,1x,i2,2x,'        ',2x,a8)
c read the GSE2.0 format, except agency and number id
 255    format(i4,1x,4(i2,1x),f4.1,4x,f8.4,1x,f9.4,4x,f5.1,1x,a1,2x,
     *  2(i4,1x),i3,2x,3(a2,f4.1,1x,i2,2x),4x,a3,3x,a8)
 300    format(5x,f5.2,3x,a2,f6.2,4x,f6.1,1x,f6.1,2x,i3,4x,a2,
     *  f5.1,2x,f6.2,1x,f6.2,3x,2(a2,f3.1,6x),a2,f3.1,5x,a3,1x,a2)
c
c Finish the program
close files
c
 99   continue
      close(1)
      if(work.eq.2)then
c        write(2,'(a4)')'STOP'
c        backspace(4)
c        write(4,'(a4)')'STOP'
        close(4)
        close(3)
        close(7)
      else
        if(kstop.eq.0.and.n.gt.0)then
          kstop=1
          goto 82
        endif
      endif
      close(2)
      write(6,*)
      write(6,*)' The output file contains ',n,' events processed'
      stop
      end
