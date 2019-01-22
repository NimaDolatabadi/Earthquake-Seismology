c**************************************************************************
c     Program to do automatic P and S arrival time picking.
c     Also locate event if 3-component azimuth is available.
c
c    input is from an s-file, entered either on prompt line or
c    through environmental varibale from eev
c                    
c                                                                               
c     Written by C. Lindholm April -90                                          
c     Modified by B.O. Ruud Mars -92
c
c  to be fixed
c
c  nlp not defined, look for cfix
c  remove norsar time routines, use seisan
c  routine get_message commented out temporarely until found
c
c updates: 
c jul 19 93 y jh : rmove auto_file routine, incllude station_loc
c aug. 20 -93 c.l. alle topdirectory satt til character*60
c oct 14      jh : make it posible to also use waveform files in local directory
c dec            : bugs, variable kp in select_phase wrong
CJAB(BGS)Feb95   : Install file and error handling
c jun 13 95 jh   : seisinc instead of seisin, dimention changes
c nov 1 95       : new seisinc
c nov8           : fix so no error if no hoizontal channel is 3 comp specified
c nov 26         : replce 8 pole filter with 4 pole, one way
c feb 7, 96      : use max_data for dimentions of data array, autopick to 
c                  autopic, bug in blank line
c mar 25 96      : back to old bndpas filter, modified to one pass
c summer 98 jh   : lo put some autoproccesing in sometimes ????
c nov 16 98 jh   : ----------    version 7.0 check ----------------------
c                  5 char station, year 2000, put in stat_loc from LIB
c jan 04 99 lo   : use get_message for start of autopic from seisnet      
c jan 26 99 lo   : fixed bug in findchan
c apr 06 01 lo   : give sfile and waveform file name as agruments
c apr 09 03 jh   : zero ndc before call to remove_dc
c jul 21 03 jh   : had never been fixed to work with seisan.inc !!!
c may    09 wcc  : a few changes
c 2010-05-05 pv  : changed some tab-formatted source line that gave warnings in f90
c 2011-08-28 jh  : fix so several wav files from s-file can be used, disable seisnet 
c                  option and obscure optoin of using seprate s file and waeform
c                  file, was not documented anywhere!
c 2012 01 25 jh  : remove info on file AUTOPIC.INF, not there
c 2012 01 27 jh  : checked for array bounds, acouple of array indexes were 0,
c                  was brutally fixed (looked for jhfix) so shouls be investigted
c                  many array dimensions were increased by a factor of 10.
c     jan 4 2012    jh: add argument to auto_tr

c
C
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      implicit none
      include 'libsei.inc'                 ! Library definitions & data defns.
      include 'seidim.inc'                 ! array dimentions
      include 'waveform.inc'               ! waveform structure
      include 'seisan.inc'
      external sei get file,               ! Find & open file.
     &         sei clen,                   ! String length function.
     &         sei open,                   ! File open handler.
     &         sei close,                  ! & closure.
     &         sei code                    ! Error condition handler.
      integer  sei clen                    ! & function.
      integer  code,                       ! Condition.
     &         text_c,                     ! Text length.
     &         read2,                      ! Ditto 2.
     &         write1                      ! Write unit1.
      parameter (text_c = 80)              ! & value.
c
      logical  b_flag                      ! Flag!!
      character chr_text   *(text_c)       ! Text string.
C
C    ------- End of details -------
C
c-- real coda and 
      real rcoda
c-- data from seisread2                               
      real data(max_sample)				
c-- data used as input                         
      real datamatrix(max_sample,3)			
c-- file names                           
      character*80 filen,fileout,fil,sfile,phasefile	
c-- New header with hypocenter                      
      character*80 hypohead			
c-- a help variable                                    
      character*80 text 			
c-- station processed                                 
      character*5 station			
c-- station and comp used in seisinc      
      character*5 jstation(max_trace)
      character*4 jcomp(max_trace)
c-- component of station                  
      character*4 incomp		
c-- component of station                              
      character*2 comp(3)			
c-- horizontal component coda
      character*4 hcomp
c-- 3 comp spesification (y/n)                      
      character*1 comp3(max_trace)			
c-- sampling rate                                             
      real srate				
c-- lat, lon, and elev. of station                    
      real slat,slon,elev			
c-- Start and window of trace                           
      real start,windo				
c-- amplitude and period                                    
      real amp,per				
c-- used for quality check   
      real threshold,qratio                     
c-- used to get threshold                                 
      real array(10)				
c-- used to get phasetiming                        
      double precision pfsec			
c-- duration of signal                               
      double precision dur			
c-- used to get phasetiming                                    
      real sec					
c-- min codas accepted                       
      real mincoda			
c-- Time parameters of eventfile        
      integer year,doy,month,day,hour,min	
c-- Vector with selected channels                 
      integer channelname(max_trace)			
c-- Number of selected channels                            
      integer nchan				
c-- Number of channels in data                  
      integer number_of_channels		
c-- length of vectors                                      
      integer nsamp				
c-- counters                                         
      integer i,j,ijk,k
c-- 1/3 (3 for 3 comp data)                            
      integer threecomp				
c-- used in remove_dc                                     
      integer ndc/0/				
c-- used in remove_dc                                         
      integer dc				
c-- filterswitch                                           
      integer ifilt				
c-- unit number 'autopic.out' 
      integer iout                              
c-- unit number 'preproce         
      integer inp                               
c-- unit number 'autopic.out' 
      integer inf                               
c-- window length (samples)       
      integer lwind                             
c-- minimum allowed apparent P-velocity
      real apvmin,svelo
      integer nars
      character*80 arg(5)
      integer read_unit      ! for reading s-file
      character*1 type,exp       ! event type
c
c   following 2 lines for readin s-file
c
      character*80 data1(5000)   
      integer nrecord,nphase,nhead,nstat,id
      character*80 file_out  ! file found
      integer n_wav_file     ! number of wav files in s-file 


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
c
c   get seisan defaults
c
      call get_seisan_def

c--
c                                                                               
c------- initialize ---------                                                   
c                                                                               
      start = 0.                                                                
      windo = 0.                                                                
cx      rotate=.false.
      b_f_debug$ = .false.
c-- maxcoda calculated in loop                             
      mincoda = 20.				
      iout = 67                                                                 
      inp = 68                                                                  
      inf = 69      
      nars=0
      sfile=' '
c                                                                                                            
c                                                                               
      write(*,*)
      write(*,*)
     &'*******************************************************'
      write(*,*)
     &'****                                               ****'
      write(*,*)
     &'****                AUTOPIC program                ****'
      write(*,*)
     &'****                                               ****'
      write(*,*)
     &'*******************************************************'
c
      ifilt = 1                                                                 
c                                                                               
c------ Now start to get the data--------This is also the outer loop ----     

c
c check for arguments
c
      call get_arguments(nars,arg)
      

      if (nars.eq.1) then
 
        sfile=arg(1)

      else
c
c-- Sfile might be in memory        
c         
        call get_env_event(sfile)   ! moved from above lo, 04-04-2001
      endif
c
c  check if s-file given
c
      if(sfile.eq.' ') then
         write(6,*) ' No input of s-file, will stop'
        stop
      endif
c
c   read s-file
c
      
      call sei open( old$,                 ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  sfile,                ! This file.
     &                  read_unit,            ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
      call indata
     *(read_unit,nstat,nphase,nhead,nrecord,type,exp,data1,id)
      call sei close(close$,read_unit,code)    ! Close (stop on erro
c
c   find if any waveform files
c

      call auto_tr(data1,nhead,nrecord,n_wav_file,wav_filename)

      if(n_wav_file.eq.0) then   ! if no waveform, stop
          write(6,*)' No waveform file name in S-file'
          write(6,*)' Will stop'
          stop
      endif
c
c   check if files there
c
        k=0
        do i=1,n_wav_file
c          write(6,*) i,wav_filename(i)
          call  get_full_wav_name(wav_filename(i),file_out)
          if(file_out.eq.' ') then
              write(6,'(1x,a,a)') ' No such file: ',
     *        wav_filename(i)(1:seiclen(wav_filename(i)))
          else
              k=k+1
              wav_filename(k)=file_out    
          endif
        enddo    
c
c   action if no files available
c
        if(k.eq.0) then            !
           write(6,*)' No waveform files found, will stop'
           stop
        endif                                
c                                                                               
c   read all header information from all files selected or available           
c
        call wav_init
        call wav_mem_init                              
        wav_nfiles=k
        do i=1,wav_nfiles
           call read_wav_header(i)
           if(wav_nchan.gt.max_trace) then
              write(6,*)' Too many channels'
              write(6,*)' Will stop'
              stop
           endif   
        enddo
        write(6,'(a,a)')' Read headers from files:'
        do i=1,wav_nfiles
           write(6,'(1x,a)') wav_filename(i)(1:80)
        enddo                              !
c                                                                               
c------- get  data quality parameter                                            
c                                                                               
c      write(*,*)' give threshold for quality check:'                         
c      read(*,*) threshold                                                    
c      parameter = 'fixed'                                                   
      call param('fixed',i,array)                                            
c-- Experienced as reasonable                    
      threshold = array(10)			
      SVELO = ARRAY(6)                                                          
      apvmin = sqrt(3.)*svelo


c set number of channels
c
      number_of_channels=wav_nchan
      year=wav_year(wav_first)
      month=wav_month(wav_first)
      day=wav_day(wav_first)
      hour=wav_hour(wav_first)
      min=wav_min(wav_first)
      sec=wav_sec(wav_first)
      call date_doy(doy,day,month,year)
C                                                                               
C   read station and component code                   
C                                                                               
      do j=1,number_of_channels                                                 
        jstation(j)=wav_stat(j)
        jcomp(j)=wav_comp(j)
      enddo                                                                     
c                                                                               
c  get default channels to use                                                  
c                                                                               
      call channel_def_2(number_of_channels,jstation,jcomp,                    
     + channelname,nchan,comp3)                                                 
c                                                                               
c  write some information to the file 'autopic.out'                         
c                                                                               
      chr_text = 'AUTOPIC.INP'
      call sei get file( check$+ignore$,    ! Check existance.
     &                    0,                 ! Unit (n/a).
     &                    code,              ! Returned condition.
     &                    'DAT',             ! Alternative directory to search.
     &                    chr_text )         ! File to find & return pathname.
c
      call sei open( unknown$,              ! Open file (stop on error).
     &                ' ',                   ! No prompt.
     &                'autopic.out',         ! File to open.
     &                write1,                ! unit to write to.
     &                b_flag,                ! File exists (n/a).
     &                code )                 ! Condition (n/a).
c
      write(write1,'(a,a)',iostat=code)
     &       ' Reading waveforms from file  :',
     &       filen(:seiclen(filen))
      call sei code( fort$, code, write1, b_flag ) ! Process outcome.
c
      write(write1,'(a,a)',iostat=code)
     &       ' Parameter settings in file   :',
     &       chr_text(:seiclen(chr_text))
      call sei code( fort$, code, write1, b_flag ) ! Process outcome.
c
c    Find where parameter information (help) file is...
c
c      chr_text = 'AUTOPIC.INF'
c      call sei get file( check$+ignore$,    ! Check existance.
c     &                    0,                 ! Unit (n/a).
c     &                    code,              ! Returned condition.
c     &                    'DAT',             ! Alternative directory to search.
c     &                    chr_text )         ! File to find & return pathname.
c
c      write(write1,'(a,a)',iostat=code)
c     &       ' Parameter explanation in file:',
c     &       chr_text(:seiclen(chr_text))
c      call sei code( fort$, code, write1, b_flag ) ! Process outcome.
c
      write(write1,*,iostat=code)
c      call sei code( fort$, code, write1, b_flag ) ! Process outcome.
c
c***********************************************************                    
c------- Loop over selected channels starts here ----------                     
c***********************************************************                    
c                                                                               
      do ijk = 1,nchan                                                          
c                                                                               
c------- Initialize for every station -----                                     
c                                                                               
        dur = 0.0d0                                                             
        amp = 0.                                                                
        per = 0.                                                                
        hypohead = ' '                                                          
        k=channelname(ijk)
c                                                                               
c------- Now get the first data, z channel ----- 
c                                                                               
        write(*,'(1x,2a)')    
     *     wav_stat(k)(1:5),wav_comp(k)(1:4)
        call wav_read_channel(k)
        if(wav_error_message.ne.' ') then
          write(*,*) ' error: ',wav_error_message
          goto 123
        endif
c                                                                               
c------- From year, month etc. to absolute time in seconds ------               
c                                                                               
        station=wav_stat(k)
        incomp=wav_comp(k)
        year=wav_year(k)
        month=wav_month(k)
        hour=wav_hour(k)
        min=wav_min(k)
        sec=wav_sec(k)
        call date_doy(doy,day,month,year)

        call timsec(year,month,day,hour,min,sec,pfsec)                          
51      format(a5,a4,i3,1x,i3,4(1x,i2),1x,f6.3)                           
c-- Just format change                    
        call component(incomp,comp(1))		
c                                                                               
c------ Get station coordinates-----                                            
c                                                                               
        call stat_loc(station,' ',slat,slon,elev)                                
c                                                                               
c------- Check if 3 component data ----                                         
c                                                                               
        if(comp3(ijk) .eq. 'y')then                                            
          threecomp = 3                                                         
          write(*,*) ' 3comp'
        else
          threecomp = 1
          write(*,*) ' 1comp'
        endif                                                                   
c                                                                               
c------ Write to autopic.out                                           
c                                                                               
        write(write1,*,iostat=code)
        call sei code( fort$, code, write1, b_flag ) ! Process outcome.
c
        write(write1,*,iostat=code)
     & 'Station           :',station
        call sei code( fort$, code, write1, b_flag ) ! Process outcome.
c
        write(write1,*,iostat=code)
     & 'Number of channels to process if there:',threecomp
        call sei code( fort$, code, write1, b_flag ) ! Process outcome.
c                                                                               
c------- check data quality                                                     
c                                                                               
        srate=wav_rate(k)
        nsamp=wav_nsamp(k) 
        do j=1,nsamp
          data(j)=signal1(j)
        enddo
        ndc=0.0
        call remove_dc(data,ndc,dc,nsamp)                                       
        lwind = srate*5.0                                                       
        call qcheck(data,nsamp,lwind,qratio)                             
c
        write(write1,*,iostat=code)'Quality ratio     :',qratio              
        call sei code( fort$, code, write1, b_flag ) ! Process outcome.

        if (qratio.lt.threshold) go to 123
c                                                                               
c------- calculate coda with Berits routine                                     
c                                                                               
        rcoda = 0.0                                                             
c        call coampl(data,nsamp,rcoda,MAXAMPL,pick_point,srate)                 
c        maxcoda = real(nsamp)/real(srate) - 10.0                               
c        if(   (rcoda .gt. mincoda) .and.                                       
c     +        (rcoda .lt. maxcoda)    ) idur = int(rcoda)                      
c                                                                               
c put z channel into datamatrix
c                                                                               
        do j = 1,nsamp                                                          
           datamatrix(j,3) = data(j)                                            
        enddo                                                                   

        if(comp3(ijk).eq.'y')then                                               
c
c read 3 comp data
c
          write(*,*) ' Qual OK: reading 3comp for ',wav_stat(k)(1:5)
          call wav_read_3channel(k)

          if(wav_error_message.ne.' ') then  ! check for error
            threecomp=1                      ! not there, do not use 3 comp
            write(*,*) ' error: ',wav_error_message
          else
 
c
c change channel name format 
c
            hcomp=incomp    
            hcomp(4:4)='N'                                   
c            call component(hcomp,comp(1))	WCC: CHANGED TO CORRESPOND TO SIGNAL#
            call component(hcomp,comp(2))	
            hcomp(4:4)='E'                                   
c            call component(hcomp,comp(2))	WCC: CHANGED TO CORRESPOND TO SIGNAL#	
            call component(hcomp,comp(3))	
c
c remove DC from horizontal components
c
            ndc=0.0
            call remove_dc(signal2,ndc,dc,nsamp)
            ndc=0.0
            call remove_dc(signal3,ndc,dc,nsamp)
c
c write horizontal components to data array
c
            do j = 1,nsamp                                                        
              datamatrix(j,1) = signal2(j) 
              datamatrix(j,2) = signal3(j)
            enddo                                                                 
          endif
        else
      	  write(*,*) ' Qual OK'
        endif
c                                                                               
c------- Now call the picking and location routine. Additional results          
c        from this routine is written on the files:                             
c                  
        call detect(station,slat,slon,pfsec,datamatrix,                          
     +  max_sample,nsamp,threecomp,srate,hypohead,ifilt,write1,dur)             
c                                                                               
c------- Now read and write phase results from detect -----                     
c                                                                               
        phasefile= ' '
        phasefile = 'detmerge.'//station(1:seiclen(station)) 
        call sei open( old$,                   ! Open file (stop on error).
     &                ' ',                   ! No prompt.
     &                phasefile,             ! File to open.
     &                read2,                 ! unit to write to.
     &                b_flag,                ! File exists (n/a).
     &                code )                 ! Condition (n/a).
c	
        call read_write_phas(read2,sfile,station,slat,slon,comp(1),
     +  dur,amp,per,year,month,day,hypohead,apvmin,write1)
c
        call sei close( delete$, read2, code ) ! Delete (stop on error).
c
c***********************************************************                    
c------- End of loop over selected channels stops here ----------               
c***********************************************************                    
123     continue                                                                  
      enddo                                                                     
c
c    Close down all files...
c    -----------------------
c
      call sei close( close$+all$, 0, code ) ! Close down.
c
c    Tidy up...
c    ==========
c
999   write(*,*)
      write(*,*)' Output written to autopic.out'
      end                                                                       

      SUBROUTINE ASSOCIAT(IN,IASS,IOUT,SLA,SLO,NCOMP,HYPOCEN)                          
      CHARACTER*80 HYPOCEN                                                      
C                                                                               
C     THIS PROGRAM READS DETECTIONS FROM THE FILE 'DETMERGE.ALL'                
C     AND WRITE ASSOCIATED ASSOCIATED PHASES AND ESTIMATED EVENT                
C     PARAMETERS TO THE FILE 'ASSOCIAT.OUT'. THE STRONGEST UNASSOCIATED         
C     DETECTIONS ARE ALSO INCLUDED.                                             
C                                                                               
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code                    ! Error condition handler.
      integer  code                        ! Condition.
      logical  b_flag                      ! Working flag?.
c
c    ----- end -----
c

      INTEGER YEAR, GMT(5), DOY(50), HOUR(50), MIN(50), DA(50),                 
     +  NDET(50),NH(50),NV(50),NCOH(50),                                        
     +  IQL(50),IPS(50),STA(50),AZI(50)                                         
      REAL SEC(50), DUR(50), FRQ(50), SNR(50), VEL(50), DV(50)                  
      REAL*8 EPOTIM(50), OTIME                                                  
      CHARACTER*5 PHASE(50)                                                     
      CHARACTER TEXT*72                                                         
      LOGICAL EOF                                                               
                                                                                
      DATA YEAR/1990/                                                           
      DATA TIMAX/165.0/                                                         
      DATA EOF/.FALSE./                                                         
                                                                                
      DO 3 I=1,3                                                                
      READ(IN,'(A)',iostat=code) TEXT
      call sei code(fort$,code,in,b_flag)    ! Process the outcome.
      WRITE(IASS,'(A)',iostat=code) TEXT    
      call sei code(fort$,code,iass,b_flag)  ! Process the outcome.
      WRITE(IOUT,'(A)') TEXT                                                    
      call sei code(fort$,code,iout,b_flag)  ! Process the outcome.
    3 CONTINUE                                                                  
                                                                                
      N = 0                                                                     
                                                                                
C  READ NEXT DETECTION                                                          
                                                                                
   20 N = N + 1                                                                 
      READ(IN,200,iostat=code)
     +  DOY(N),HOUR(N),MIN(N),SEC(N),DUR(N),FRQ(N),SNR(N),                      
     +  STA(N),NDET(N),NH(N),NV(N),NCOH(N),IQL(N),IPS(N),                       
     +  AZI(N),DA(N),VEL(N),DV(N)                                               
  200 FORMAT(1X,3I3,2F6.0,2F5.0,I6,6I3,I4,I3,F5.0,F4.0)                         
c
      call sei code(fort$,code,in,eof)       ! Process outcome.
c
c   ...end of file...
c
          if( eof ) then                     !
          N = N - 1                                                             
          ND = N                                                                
             IF( N .EQ. 0 ) then
             GO TO 900                                                 
             else
             GO TO 25
             end if
          end if
c
c   ...proceed...
c
      GMT(1) = DOY(N)                                                           
      GMT(2) = HOUR(N)                                                          
      GMT(3) = MIN(N)                                                           
      GMT(4) = INT(SEC(N))                                                      
      GMT(5) = 0                                                                
      CALL GMTEPO(YEAR,GMT,IIRSPS,EPOTIM(N))                                    
      EPOTIM(N) = EPOTIM(N) + (SEC(N)-INT(SEC(N)))                              
                                                                                
C  DO AN INITIAL PHASE IDENTIFICATION                                           
                                                                                
      IF (NCOMP.EQ.3) THEN
      CALL PHASEID (FRQ(N),IPS(N),VEL(N),PHASE(N))                            
      WRITE(IOUT,300,iostat=code)
     &      DOY(N),HOUR(N),MIN(N),SEC(N),DUR(N),FRQ(N),               
     +      SNR(N),STA(N),NDET(N),NH(N),NV(N),NCOH(N),
     &      IQL(N),IPS(N),                
     +      AZI(N),DA(N),VEL(N),DV(N),PHASE(N)                                
      call sei code(fort$,code,iout,b_flag)    ! Process the outcome.
c
      ELSE
      PHASE(N)='Sn/Lg'
      WRITE(IOUT,300,iostat=code)
     &      DOY(N),HOUR(N),MIN(N),SEC(N),DUR(N),FRQ(N),               
     +      SNR(N),STA(N),NDET(N),NH(N),NV(N),NCOH(N),
     &      IQL(N),IPS(N),                
     +      AZI(N),DA(N),VEL(N),DV(N)                                         
      call sei code(fort$,code,iout,b_flag)    ! Process the outcome.
      ENDIF
                                                                                
C  CHECK TIME GAP                                                               
                                                                                
      GAP = EPOTIM(N) - EPOTIM(1)                                               
      IF (GAP.LE.TIMAX) GO TO 20                                                
                                                                                
C  TRY PHASE ASSOCIATION                                                        
                                                                                
      ND = N - 1                                                                
   25 CALL PHASSO(EPOTIM,DUR,FRQ,SNR,STA,NDET,NH,NV,NCOH,IQL,IPS,               
     +            AZI,DA,VEL,DV,ND,NOUT,PHASE,PTOLG,AZIM)                       
      IF (NOUT.LT.0) THEN                                                       
        ND = -NOUT                                                              
        GO TO 25                                                                
      ENDIF                                                                     
                                                                                
C  DECIDE IF TO MAKE ANY OUTPUT                                                 
                                                                                
      NQ1 = 0                                                                   
      NQ2 = 0                                                                   
      DO 27 I=1,NOUT                                                            
        IF (IQL(I).EQ.1) NQ1 = NQ1 + 1                                          
        IF (IQL(I).EQ.2) NQ2 = NQ2 + 1                                          
 27   CONTINUE                                                                  
      IF (.NOT.(NQ1.GE.1.OR.NQ2.GE.2)) GO TO 35                                 
                                                                                
C  LOCATE EVENT IF LOCAL (P AND S) OR TELESEISMIC (P)                           
                                                                                
      TRAVT = 0.                                                                
      IF (PTOLG.GT.0) THEN                                                      
        CALL LOCLOC (SLA,SLO,AZIM,PTOLG,ELA,ELO,DIST,TRAVT)                     
        DELTA = DIST/111.2                                                      
c     ELSE IF (PHASE(1).EQ.'Ptele') THEN                                        
c       CALL LOCTEL (SLA,SLO,AZI(1),VEL(1),ELA,ELO,DELTA,TRAVT)                 
c       DIST = DELTA*111.2                                                      
      ENDIF                                                                     
      DEPTH = 0.                                                                
                                                                                
C  WRITE LOCATION                                                               
                                                                                
      IF (TRAVT.GT.0) THEN                                                      
                                                                                
        GMT(1) = DOY(1)                                                         
        GMT(2) = HOUR(1)                                                        
        GMT(3) = MIN(1)                                                         
        GMT(4) = INT(SEC(1))                                                    
        GMT(5) = 0                                                              
        CALL GMTEPO(YEAR,GMT,IIRSPS,OTIME)                                      
        OTIME = OTIME + (SEC(1)-INT(SEC(1)))                                    
        OTIME = OTIME - TRAVT                                                   
        CALL EPOGMT(OTIME,YEAR,GMT,ITIMT,TIMCOR)                                
        SS = GMT(4) + 0.1*GMT(5) + TIMCOR                                       
        WRITE(IASS,250,iostat=code) (GMT(I),I=1,3),SS,                          
     +                  ELA,ELO,DEPTH,'F',AZIM,DIST,DELTA                       
        call sei code(fort$,code,iass,b_flag )     ! Process outcome.
 250    FORMAT(1X,3I3,F6.2,2F8.2,F5.0,A1,2F7.0,F7.2)                            
c
        WRITE(HYPOCEN,260) GMT(2),GMT(3),SS,ELA,ELO,DEPTH,'F'                   
 260    FORMAT(11X,2I2,F5.1,F10.2,F8.2,F5.1,A1)  
      ENDIF                                                                     
                                                                                
C  WRITE OUTPUT                                                                 
                                                                                
      DO 30 I=1,NOUT                                                            
      WRITE(IASS,300,iostat=code)
     &      DOY(I),HOUR(I),MIN(I),SEC(I),DUR(I),FRQ(I),               
     +      SNR(I),STA(I),NDET(I),NH(I),NV(I),NCOH(I),
     &      IQL(I),IPS(I),                
     +      AZI(I),DA(I),VEL(I),DV(I),PHASE(I)                                  
      call sei code(fort$,code,iass,b_flag )     ! Process outcome.
  300 FORMAT(1X,3I3,2F6.2,2F5.1,I6,6I3,I4,I3,F5.1,F4.1,1X,A5)                   
30    continue
c
      WRITE(IASS,*,iostat=code)                                             
      call sei code(fort$,code,iass,b_flag )     ! Process outcome.
c
C  MOVE UN-USED DETECTIONS TO BEGINNING OF SEQUENCE                             
c
   35 CONTINUE                                                                  
      DO 40 I=NOUT+1,N                                                          
        EPOTIM(I-NOUT) = EPOTIM(I)                                              
        DOY(I-NOUT) = DOY(I)                                                    
        HOUR(I-NOUT) = HOUR(I)                                                  
        MIN(I-NOUT) = MIN(I)                                                    
        SEC(I-NOUT) = SEC(I)                                                    
        DUR(I-NOUT) = DUR(I)                                                    
        FRQ(I-NOUT) = FRQ(I)                                                    
        SNR(I-NOUT) = SNR(I)                                                    
        STA(I-NOUT) = STA(I)                                                    
        NDET(I-NOUT) = NDET(I)                                                  
        NH(I-NOUT) = NH(I)                                                      
        NV(I-NOUT) = NV(I)                                                      
        NCOH(I-NOUT) = NCOH(I)                                                  
        IQL(I-NOUT) = IQL(I)                                                    
        IPS(I-NOUT) = IPS(I)                                                    
        AZI(I-NOUT) = AZI(I)                                                    
        DA(I-NOUT) = DA(I)                                                      
        VEL(I-NOUT) = VEL(I)                                                    
        DV(I-NOUT) = DV(I)                                                      
        PHASE(I-NOUT) = PHASE(I)                                                
   40 CONTINUE                                                                  
      N = N - NOUT                    
      if(n.eq.0) return ! jh fix                                          
      if(n.le.0) n=1  !jhfix but result in infinite loop
      GAP = EPOTIM(N) - EPOTIM(1)                                               
                                                                                
C  DECIDE IF TO READ A NEW DETECTION                                            
                                                                                
      IF (EOF) THEN                                                             
        CONTINUE                                                                
        IF (N.EQ.0) RETURN                                                      
        ND = N                                                                  
        GO TO 25                                                                
      ELSE IF (GAP.LE.TIMAX) THEN                                               
        GO TO 20                                                                
      ELSE IF (N.NE.0) THEN                                                     
        ND = N - 1                                                              
        GO TO 25                                                                
      ENDIF                                                                     
                                                                                
  900 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE COHPOW(Z,MPTS,ISMP,LSTEP,NSTEP,LWIND,                          
     +                  COH,AZI,VRAT,POWX,POWY,POWZ)                            
      REAL Z(MPTS,3)
cccc  COH(NSTEP),AZI(NSTEP),VRAT(NSTEP),                         
c     +     POWX(NSTEP),POWY(NSTEP),POWZ(NSTEP)                                  
      real  COH(5000),AZI(5000),VRAT(5000),                  !jhfix
     +     POWX(5000),POWY(5000),POWZ(5000)                                  
C-----------------------------------------------------------------------        
C                                                                               
C  COMPUTE PREDICTED COHERENCE AND STA POWER SEQUENCES.                         
C                                                                               
C  INPUT :                                                                      
C                                                                               
C     Z(MPTS,3) - DATA MATRIX                                                   
C                 Z(*,1) - NORTH CHANNEL                                        
C                 Z(*,2) - EAST CHANNEL                                         
C                 Z(*,3) - VERTICAL CHANNEL (POSITIVE DIRECTION UP)             
C     MPTS      - USED FOR DIMENSIONING OF DATA MATRIX                          
C     ISMP      - SAMPLE NUMBER FOR MID POINT OF FIRST TIME WINDOW              
C     LSTEP     - STEP LENGTH IN SAMPLES                                        
C     NSTEP     - NO OF TIME STEPS                                              
C     LWIND     - NO OF STEPS WITHIN MOVING WINDOW                              
C                                                                               
C  OUTPUT :                                                                     
C                                                                               
C     COH(*)    - SEQUENCE OF PREDICTED COHERENCE VALUES                        
C     AZI(*)    - SEQUENCE OF AZIMUTHS                                          
C     VRAT(*)   - SEQUENCE OF VERTICAL TO RADIAL POWER RATIOS                   
C     POWX(*)   - SEQUENCE OF POWERS ON NORTH CHANNEL                           
C     POWY(*)   - SEQUENCE OF POWERS ON EAST CHANNEL                            
C     POWZ(*)   - SEQUENCE OF POWERS ON VERTICAL CHANNEL                        
C                                                                               
C-----------------------------------------------------------------------        
C  THE FOLLOWING INTERNALLY DECLARED ARRAYS SHOULD BE AT LEAST                  
C  NSTEP+LWIND LONG.                                                            
      REAL XX(60100),YY(60100),ZZ(60100),XY(60100),XZ(60100),YZ(60100)                
      DATA RADDEG/57.29578/                                                     
C	    write(6,*)'mpts,ismp,lstep,nstep,lwin',
C     *mpts,ismp,lstep,nstep,lwin
C                                                                               
C  CALCULATE THE AUTO AND CROSS CORRELATIONS                                    
C                                                                               
      LENWIN = LSTEP*LWIND                                                      
      II = ISMP - LENWIN/2 - LSTEP
      if(ii.le.-lstep) ii=-lstep+1           ! jhfix                                              
      DO 50 I=1,NSTEP+LWIND-1                                                   
        II = II + LSTEP                                                         
c		if(ii.ge.20000.or.i.gt.6000) write(6,*) ii,i
c		write(6,*)ii,i
        CALL CROSS(Z(II,1),Z(II,1),LSTEP,XX(I))                                 
        CALL CROSS(Z(II,2),Z(II,2),LSTEP,YY(I))                                 
        CALL CROSS(Z(II,3),Z(II,3),LSTEP,ZZ(I))                                 
        CALL CROSS(Z(II,1),Z(II,2),LSTEP,XY(I))                                 
        CALL CROSS(Z(II,1),Z(II,3),LSTEP,XZ(I))                                 
        CALL CROSS(Z(II,2),Z(II,3),LSTEP,YZ(I))
		                                 
   50 CONTINUE                                                                  
C                                                                               
C  AVERAGE OVER WINDOW                                                          
C                                                                               
      DO 60 I=1,NSTEP                                                           
        AXX = 0.                                                                
        AYY = 0.                                                                
        AZZ = 0.                                                                
        AXY = 0.                                                                
        AXZ = 0.                                                                
        AYZ = 0.                                                                
        DO 65 J=I,I+LWIND-1                                                     
          AXX = AXX + XX(J)                                                     
          AYY = AYY + YY(J)                                                     
          AZZ = AZZ + ZZ(J)                                                     
          AXY = AXY + XY(J)                                                     
          AXZ = AXZ + XZ(J)                                                     
   65     AYZ = AYZ + YZ(J)                                                     
        POWX(I) = AXX                                                           
        POWY(I) = AYY                                                           
        POWZ(I) = AZZ                                                           
        XY(I) = AXY                                                             
        XZ(I) = AXZ                                                             
        YZ(I) = AYZ                                                             
   60 CONTINUE                                                                  
C                                                                               
C  CALCULATE PREDICTED COHERENCE AND ASSOCIATED AZIMUTH AND Z/R                 
C                                                                               
      II = ISMP - LENWIN/2 - LSTEP                                              
      NSMP = LSTEP*LWIND                                                        
      DO 70 I=1,NSTEP                                                           
        IF (POWX(I).EQ.0) THEN                                                  
C          WRITE(*,*)' I, POWX ',I,POWX(I)                                      
          GO TO 70                                                              
        ELSE IF (POWY(I).EQ.0) THEN                                             
C          WRITE(*,*)' I, POWY ',I,POWY(I)                                      
          GO TO 70                                                              
        ELSE IF (POWZ(I).EQ.0) THEN                                             
C          WRITE(*,*)' I, POWZ ',I,POWZ(I)                                      
          GO TO 70                                                              
        ENDIF                                                                   
        AZ=ATAN2(-YZ(I),-XZ(I))                                                 
        AZI(I)=AZ*RADDEG                                                        
        IF(AZI(I).LT.0.) AZI(I) = AZI(I) + 360.                                 
        DIVISOR=SQRT(XZ(I)*XZ(I)+YZ(I)*YZ(I))                                   
        IF (DIVISOR.GT.0.01) THEN                                               
          ZOVERR=POWZ(I)/DIVISOR                                                
        ELSE                                                                    
          GO TO  70                                                             
        ENDIF                                                                   
        VRAT(I)=ZOVERR                                                          
        A=-ZOVERR*COS(AZ)                                                       
        B=-ZOVERR*SIN(AZ)                                                       
        ERR=0.                                                                  
        II = II + LSTEP                                                         
C		if(ii.le.0) write(6,*) ii    !jhfix
cxx  fix when jj is zero
        if(ii.le.0) ii=1
        DO 75 JJ=II,II+NSMP-1                                                   
          CC=Z(JJ,3)-A*Z(JJ,1)-B*Z(JJ,2)                                        
          ERR=ERR+CC*CC                                                         
   75   CONTINUE                                                                
        COH(I)=1.-ERR/POWZ(I)                                                   
        IF (COH(I).LT.0) COH(I)=0.                                              
   70 CONTINUE                                                                  
C                                                                               
C  NORMALIZE POWERS                                                             
C                                                                               
      DO 80 I=1,NSTEP                                                           
        POWX(I)=POWX(I)/LENWIN                                                  
        POWY(I)=POWY(I)/LENWIN                                                  
        POWZ(I)=POWZ(I)/LENWIN                                                  
   80 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc           
      SUBROUTINE CROSS(X,Y,L,A)                                                 
C-----------------------------------------------------------------------        
C CALCULATES UNNORMALISED CROSS CORRELATION BETWEEN THE TWO                     
C REAL TIME SERIES X AND Y OF LENGTH L.                                         
C THE RESULT IS RETURNED IN A.                                                  
C-----------------------------------------------------------------------        
      DIMENSION X(L),Y(L)                                                       
      A=0.                                                                      
      DO 1 I=1,L                                                                
        A=A+X(I)*Y(I)                                                           
 1    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                               
      SUBROUTINE DET1C(EPOTIM,THRESH,NDMIN,DS,ISHFT,                    DET00540
     +                 ISIGM,LTAZ,STAZ,NSTEP,IOUT)                      DET00550
      REAL*8 EPOTIM                                                     DET00560
      REAL STAZ(*),LTAZ                                                 DET00570
C-----------------------------------------------------------------------DET00580
C                                                                       DET00590
C  DETECTION ALGORITHM. A STA/LTA DETECTOR IS RUN FOR A SINGLE CHANNEL. DET00600
C  A MINIMUM NUMBER OF SUCCESSIVE TRIGS (NDMIN) IS REQUIRED.            DET00610
C                                                                       DET00620
C  INPUT :                                                              DET00630
C                                                                       DET00640
C     EPOTIM    - EPOCH TIME, MID POINT OF FIRST TIME WINDOW.           DET00650
C     THRESH    - DETECTION THRESHOLD                                   DET00660
C     NDMIN     - THE MINIMUM NUMBER OF SUCCESSIVE TRIGS                DET00670
C                 RECOMMENDED VALUES: 3 - 4                             DET00680
C     DS        - STEP LENGTH IN SEC (1/4 OF WINDOW LENGTH).            DET00690
C     ISHFT     - DELAY OF LTA WINDOW RELATIVE TO STA (IN TIME STEPS),  DET00700
C                 RECOMMENDED VALUES: 0.5 - 5.0 SEC (FREQ DEPENDENT)    DET00710
C     ISIGM     - USED TO COMPUTE LTA, RECOMMENDED VALUES: 6 - 7        DET00720
C     LTAZ      - LTA VALUE OF Z-COMP. (VERTICAL)                       DET00730
C     STAZ(*)   - STA/POWER TIME SEQUENCE OF Z-COMP. (VERTICAL)         DET00740
C     NSTEP     - NO OF TIME STEPS (LENGTH OF ARRAYS)                   DET00750
C     IOUT      - UNIT NUMBER OF OUTPUT FILE.                           DET00760
C                                                                       DET00770
C-----------------------------------------------------------------------DET00780
      INTEGER YEAR, DOY, HOUR, GMT(5)                                   DET00790
      REAL*8 DETTIM                                                     DET00800
C     THE FOLLOWING INTERNALLY DEFINED ARRAYS MUST BE LONG ENOUGH TO    DET00810
C     CONTAIN ALL TRIGS FROM ONE DETECTION.                             DET00820
      REAL DETLTA(100),DETSTA(100)                                      DET00830
      CHARACTER*1 DETCOM(100)                                           DET00840
c
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code                    ! Error condition handler.
      integer  code                        ! Condition.
      logical  b_flag                      ! Working flag?.
c
c    ----- end -----
c
                                                                        DET00850
      NDET = 0                                                          DET00860
      FSTA = 1./FLOAT(2**ISIGM)                                         DET00870
      FLTA = 1.- FSTA                                                   DET00880
      TH = THRESH*THRESH                                                DET00890
C                                                                       DET00900
C  LOOP OVER TIME WINDOWS                                               DET00910
C                                                                       DET00920
      II = ISHFT                                                        DET00930
      DO 20 JJ=1,NSTEP                                                  DET00940
      II = II + 1                                                       DET00950
C                                                                       DET00960
C   CALCULATE STA/LTA RATIOS                                            DET00970
C                                                                       DET00980
         RATIO=0.0                                                      DET00990
         IF(LTAZ.GT.0.0) RATIO=STAZ(II)/LTAZ                            DET01000
C                                                                       DET01010
C   CHECK FOR DETECTION                                                 DET01020
C                                                                       DET01030
         IF (RATIO.GT.TH) THEN                                          DET01040
            NDET=NDET+1                                                 DET01050
            IF (NDET.GT.100) WRITE(*,*)'WARNING, NDET =',NDET           DET01060
            DETCOM(NDET)='Z'                                            DET01070
            DETSTA(NDET)=SQRT(STAZ(II))                                 DET01080
            DETLTA(NDET)=SQRT(LTAZ)                                     DET01090
         ELSE                                                           DET01100
            CONTINUE                                                    DET01110
            IF (NDET.GE.NDMIN) THEN                                     DET01120
C                                                                       DET01130
C   OUTPUT A DETECTION SEQUENCE. STA AND LTA HAVE BEEN STORED IN        DET01140
C   ARRAYS DETSTA AND DETLTA WHILE IN DETECTION MODE.                   DET01150
C                                                                       DET01160
               DETTIM=EPOTIM+(JJ-NDET-1)*DS                             DET01170
               CALL EPOGMT(DETTIM,YEAR,GMT,ITIME,TIMCOR)                DET01180
               DOY=GMT(1)                                               DET01190
               HOUR=GMT(2)                                              DET01200
               MIN=GMT(3)                                               DET01210
               SEC=GMT(4)+0.1*GMT(5)+TIMCOR-DS                          DET01220
               DO 30 J=1,NDET                                           DET01230
                  SEC=SEC+DS                                            DET01240
                  IF (SEC.GE.60) THEN                                   DET01250
                     MIN=MIN+1                                          DET01260
                     SEC=SEC-60.                                        DET01270
                  ENDIF                                                 DET01280
                  IF (MIN.GE.60) THEN                                   DET01290
                     HOUR=HOUR+1                                        DET01300
                     MIN=MIN-60                                         DET01310
                  ENDIF                                                 DET01320
                  SNR=DETSTA(J)/DETLTA(J)                               DET01330
                  if (snr.gt.9999.9) snr=9999.9
                  WRITE(IOUT,200,iostat=code)
     &                  DOY,HOUR,MIN,SEC,DETCOM(J),                     DET01340
     +                  INT(DETSTA(J)),INT(DETLTA(J)),SNR,0.0           DET01350
 200              FORMAT(1X,3I3,F6.2,A3,I7,I6,F6.1,F5.2,F6.1,F5.1)      DET01360
                  call sei code(fort$,code,iout,b_flag)
  30           CONTINUE                                                 DET01370
               WRITE(IOUT,*,iostat=code)                                DET01380
               call sei code(fort$,code,iout,b_flag)
            ENDIF                                                       DET01390
            NDET=0                                                      DET01400
         ENDIF                                                          DET01410
C                                                                       DET01420
C   UPDATE LTA                                                          DET01430
C                                                                       DET01440
      LTAZ=FLTA*LTAZ+FSTA*STAZ(JJ)                                      DET01450
C                                                                       DET01460
C   END OF TIME LOOP                                                    DET01470
C                                                                       DET01480
  20  CONTINUE                                                          DET01490
                                                                        DET01500
      RETURN                                                            DET01510
      END                                                               DET01520
      SUBROUTINE DET3C(EPOTIM,THRES1,THRES2,COHMIN,NDMIN,DS,ISHFT,              
     +                 ISIGM,SVEL,LTAX,LTAY,LTAZ,STAX,STAY,STAZ,                
     +                 COHER,AZIM,VRAT,NSTEP,IOUT)                              
      REAL*8 EPOTIM                                                             
      REAL STAX(*),STAY(*),STAZ(*),                                             
     +     COHER(*),AZIM(*),VRAT(*),                                            
     +     LTAX,LTAY,LTAZ                                                       
c
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code                    ! Error condition handler.
      integer  code                        ! Condition.
      logical  b_flag                      ! Working flag?.
c
c    ----- end -----
c
C-----------------------------------------------------------------------        
C                                                                               
C  DETECTION ALGORITHM. A STA/LTA DETECTOR IS RUN IN PARALLELL FOR THE          
C  THREE COMPONENTS. A LOWERED THRESHOLD IS ALLOWED FOR THE VERTICAL            
C  COMPONENT PROVIDED THAT THE PREDICTED COHERENCE IS GREATER THAN              
C  A GIVEN MINIMUM. A MINIMUM NUMBER OF SUCCESSIVE DETECTIONS IS                
C  REQUIRED TO PRODUCE ANY OUTPUT (SEE PARAMETER NDMIN BELOW).                  
C                                                                               
C  INPUT :                                                                      
C                                                                               
C     EPOTIM    - EPOCH TIME, MID POINT OF FIRST TIME WINDOW.                   
C     THRES1    - DETECTION THRESHOLD FOR VERTICAL COMPONENT WHEN               
C                 PREDICTED COHERENCE IS GREATER THAN COHMIN.                   
C                 RECOMMENDED VALUES: 2.5 - 4.0                                 
C     THRES2    - DETECTION THRESHOLD FOR INDIVIDUAL COMPONENTS,                
C                 INDEPENDENT OF PREDICTED COHERENCE.                           
C                 RECOMMENDED VALUES: 6.0 - 10.0                                
C     COHMIN    - MINIMUM COHERENCE FOR DETECTION WITH THRES1.                  
C                 RECOMMENDED VALUES: 0.4 - 0.6                                 
C     NDMIN     - THE MINIMUM NUMBER OF SUCCESSIVE DETECTIONS,                  
C                 RECOMMENDED VALUES: 3 - 4                                     
C     DS        - STEP LENGTH IN SEC (1/4 OF WINDOW LENGTH).                    
C     ISHFT     - DELAY OF LTA WINDOW RELATIVE TO STA (IN TIME STEPS),          
C                 RECOMMENDED VALUES: 0.5 - 5.0 SEC (FREQ DEPENDENT)            
C     ISIGM     - USED TO COMPUTE LTA, RECOMMENDED VALUES: 6 - 7                
C     SVEL      - LOCAL S-WAVE VELOCITY (KM/S). USED TO COMPUTE APPARENT        
C                 P-WAVE VELOCITY FROM APPARENT ANGLE OF INCIDENCE.             
C                 SHOULD BE SET BY CALIBRATION, MAY BE FREQ DEPENDENT.          
C     LTAX      - LTA VALUE OF X-COMP. (NORTH)                                  
C     LTAY      - LTA VALUE OF Y-COMP. (EAST)                                   
C     LTAZ      - LTA VALUE OF Z-COMP. (VERTICAL)                               
C     STAX(NSTEP+ISHFT) - STA/POWER TIME SEQUENCE OF X-COMP. (NORTH)            
C     STAY(NSTEP+ISHFT) - STA/POWER TIME SEQUENCE OF Y-COMP. (EAST)             
C     STAZ(NSTEP+ISHFT) - STA/POWER TIME SEQUENCE OF Z-COMP. (VERTICAL)         
C     COHER(NSTEP)- PREDICTED COHERENCE                                         
C     AZIM(NSTEP) - AZIMUTH ESTIMATES                                           
C     VRAT(NSTEP) - ESTIMATE OF THE RATIO OF VERTICAL TO HORIZONTAL             
C                   AMPLITUDE, USED FOR VELOCITY ESTIMATION.                    
C     NSTEP       - NO OF TIME STEPS (LENGTH OF ARRAYS)                         
C     IOUT        - UNIT NUMBER OF OUTPUT FILE.                                 
C                                                                               
C     NOTE: STAX, STAY ANS STAZ ARE SHIFTED ISHFT ELEMENTS TO THE RIGTH         
C           RELATIVE TO COHER, AZIM AND VRAT.                                   
C                                                                               
C-----------------------------------------------------------------------        
      INTEGER YEAR, DOY, HOUR, GMT(5)                                           
      REAL*8 DETTIM                                                             
C     THE FOLLOWING INTERNALLY DEFINED ARRAYS MUST BE LONG ENOUGH TO            
C     CONTAIN ALL TRIGS FROM ONE DETECTION.                                     
      REAL DETCOH(2000),DETLTA(2000),DETSTA(2000),                                 
     +     DETAZI(2000),DETVEL(2000)                                              
      CHARACTER*1 DETCOM(2000)                                                   
                                                                                
      NDET = 0                                                                  
      FSTA = 1./FLOAT(2**ISIGM)                                                 
      FLTA = 1.- FSTA                                                           
      TH1 = THRES1*THRES1                                                       
      TH2 = THRES2*THRES2                                                       
C                                                                               
C  LOOP OVER TIME WINDOWS                                                       
C                                                                               
      II = ISHFT                                                                
      DO 20 JJ=1,NSTEP                                                          
      II = II + 1                                                               
C                                                                               
C   CALCULATE STA/LTA RATIOS                                                    
C                                                                               
         RATIX=0.0                                                              
         RATIY=0.0                                                              
         RATIZ=0.0                                                              
         IF(LTAX.GT.0.0) RATIX=STAX(II)/LTAX                                    
         IF(LTAY.GT.0.0) RATIY=STAY(II)/LTAY                                    
         IF(LTAZ.GT.0.0) RATIZ=STAZ(II)/LTAZ                                    
         RATIO=MAX(RATIX,RATIY,RATIZ)                                           
C                                                                               
C   CHECK FOR DETECTION                                                         
C                                                                               
         IF ((RATIZ.GT.TH1.AND.COHER(JJ).GT.COHMIN) .OR.                        
     +        RATIO.GT.TH2) THEN                                                
            NDET=NDET+1                                                         
            IF (NDET.GT.100) WRITE(*,*)'WARNING, NDET =',NDET                   
            DETCOH(NDET)=COHER(JJ)                                              
C                                                                               
C   CALCULATE APPARENT VELOCITY FOR P-WAVES (COHER > COHMIN)                    
C                                                                               
            IF (COHER(JJ).GT.COHMIN) THEN                                       
              ZPOW=STAZ(II)-LTAZ                                                
              RPOW=STAZ(II)/VRAT(JJ)                                            
              IF (ZPOW.LE.0) ZPOW=0.001                                         
              AI=ATAN(RPOW/ZPOW)                                                
              DETVEL(NDET)=SVEL/SIN(AI/2.)                                      
              IF (DETVEL(NDET).GT.99.9) DETVEL(NDET)=99.9                       
              DETAZI(NDET)=AZIM(JJ)                                             
            ENDIF                                                               
            IF (RATIO.EQ.RATIX) THEN                                            
              DETCOM(NDET)='N'                                                  
              DETSTA(NDET)=SQRT(STAX(II))                                       
              DETLTA(NDET)=SQRT(LTAX)                                           
            ELSE IF (RATIO.EQ.RATIY) THEN                                       
              DETCOM(NDET)='E'                                                  
              DETSTA(NDET)=SQRT(STAY(II))                                       
              DETLTA(NDET)=SQRT(LTAY)                                           
            ELSE IF (RATIO.EQ.RATIZ) THEN                                       
              DETCOM(NDET)='Z'                                                  
              DETSTA(NDET)=SQRT(STAZ(II))                                       
              DETLTA(NDET)=SQRT(LTAZ)                                           
            ENDIF                                                               
         ELSE                                                                   
            CONTINUE                                                            
            IF (NDET.GE.NDMIN) THEN                                             
C                                                                               
C   OUTPUT A DETECTION SEQUENCE. STA AND LTA HAVE BEEN STORED IN                
C   ARRAYS DETSTA AND DETLTA WHILE IN DETECTION MODE.                           
C                                                                               
               DETTIM=EPOTIM+(JJ-NDET-1)*DS                                     
               CALL EPOGMT(DETTIM,YEAR,GMT,ITIME,TIMCOR)                        
               DOY=GMT(1)                                                       
               HOUR=GMT(2)                                                      
               MIN=GMT(3)                                                       
               SEC=GMT(4)+0.1*GMT(5)+TIMCOR-DS                                  
               DO 30 J=1,NDET                                                   
                  SEC=SEC+DS                                                    
                  IF (SEC.GE.60) THEN                                           
                     MIN=MIN+1                                                  
                     SEC=SEC-60.                                                
                  ENDIF                                                         
                  IF (MIN.GE.60) THEN                                           
                     HOUR=HOUR+1                                                
                     MIN=MIN-60                                                 
                  ENDIF                                                         
                  SNR=DETSTA(J)/DETLTA(J)                                       
                  if (snr.gt.9999.9) snr=9999.9
c
                  IF (DETCOH(J).GT.COHMIN) THEN                                 
                  WRITE(IOUT,200,iostat=code)
     &                  DOY,HOUR,MIN,SEC,DETCOM(J),                  
     +                  INT(DETSTA(J)),INT(DETLTA(J)),SNR,DETCOH(J),            
     +                  DETAZI(J),DETVEL(J)                                     
                  call sei code(fort$,code,iout,b_flag)
 200              FORMAT(1X,3I3,F6.2,A3,I7,I6,F6.1,F5.2,F6.1,F5.1)            
c
                  ELSE                                                          
                  WRITE(IOUT,200,iostat=code)
     &                  DOY,HOUR,MIN,SEC,DETCOM(J),                  
     +                  INT(DETSTA(J)),INT(DETLTA(J)),SNR,DETCOH(J)             
                  call sei code(fort$,code,iout,b_flag)
                  ENDIF                                                         
  30           CONTINUE                                                         
c
               WRITE(IOUT,*,iostat=code)
               call sei code(fort$,code,iout,b_flag)
            ENDIF                                                               
            NDET=0                                                              
         ENDIF                                                                  
C                                                                               
C   UPDATE LTA                                                                  
C                                                                               
      LTAX=FLTA*LTAX+FSTA*STAX(JJ)                                              
      LTAY=FLTA*LTAY+FSTA*STAY(JJ)                                              
      LTAZ=FLTA*LTAZ+FSTA*STAZ(JJ)                                              
C                                                                               
C   END OF TIME LOOP                                                            
C                                                                               
  20  CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DETECT(STATION,SLAT,SLON,EPOTIM,DATA,MSMP,NSMP,                
     +           NCHAN,SRATE,HYPOCEN,IFILTERSWITCH,IOUT,CODAEPO)                
      CHARACTER*(*) STATION, HYPOCEN                                            
      REAL*8 EPOTIM                                                             
      REAL*4 DATA(MSMP,NCHAN), SRATE, SLAT, SLON                                
c      INTEGER*4 MSMP, NSMP, NCHAN                                               
c
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code,                   ! Error condition handler.
     &         sei open,                   ! open file handler.
     &         sei close                   ! Close file handler.
      integer  code,                       ! Condition.
     &         idet, isca, iass, imer,     ! Local file units.
     &         isca_u(map_c$)              ! Maximum allowed mapped units.
      logical  b_flag                      ! Working flag?.
c
c    Note that the output ISCA units has the maximum allowed... if there are
c         as many filters as this, libsei will shut down orderly, since
c         there are other files open at this point. Currently there is a
c         maximum of 63 mapped units for libsei (parameter controlled)
c
c    ----- end -----
c
C-----------------------------------------------------------------------        
C  S/R FOR DOING DETECTION ASSOCIATION AND LOCATION FOR DATA IN A               
C  SINGLE SEGMENT.                                                              
C  NOTE: ASSOCIATION AND LOCATION IS POSSIBLE ONLY FOR 3C DATA.                 
C                                                                               
C  STATION  - STATION NAME                                                      
C  SLAT     - STATION LATITUDE (DEG)                                            
C  SLON     - STATION LONGITUDE (DEG)                                           
C  EPOTIM   _ EPOCH TIME                                                        
C  DATA(MSMP,NCHAN) - DATA MATRIX                                               
C  MSMP     - MAXIMUM NUMBER OF SAMPLES PER CHANNEL (FOR MATRIX DIM.)           
C  NSMP     - ACTUAL NUMBER OF SAMPLES PER CHANNEL                              
C  NCHAN    - NUMBER OF CHANNELS                                                
C             FOR NCHAN=3 3-COMP DATA IS ASSUMED.                               
C             DATA(*,1) - NORTH CHANNEL                                         
C             DATA(*,2) - EAST CHANNEL                                          
C             DATA(*,3) - VERTICAL CHANNEL (POSITIVE DIRECTION UP)              
C  SRATE    - SAMPLING RATE (HZ)                                                
C  HYPOCEN  - CHARACTER STRING WITH HYPOCENTER (IF ANY, ELSE BLANK)             
C  IFILTERSWITCH - MUST BE =1 IF THE DATA SHOULD BE FILTERED                    
C  IOUT     - UNIT NUMBER FOR THE FILE 'AUTOPIC.OUT'                        
C---------------------------------------------------B.O.RUUD-UIO-APR'90-        
      REAL COH(60000),AZI(60000),VRAT(60000),                                      
     +     STAX(60000),STAY(60000),STAZ(60000),LTAX,LTAY,LTAZ                      
      REAL ARRAY (10),DD(8),FDATA(200000,3)                                      
      REAL*8 START, CODAEPO                                                     
      INTEGER*4 GMT(5), YEAR                                                    
      CHARACTER*12 PARAMETER                                                    
      CHARACTER*20 FNAME                                                        
      DATA YEAR/1991/                                                           
                                                                                
C  GET INPUT PARAMETERS                                                         
                                                                                
      PARAMETER = 'fixed       '                                                
      CALL PARAM(PARAMETER,NR,ARRAY)                                            
      LWIND = INT(ARRAY(1))                                                     
      ISHIFT = INT(ARRAY(2))                                                    
      ISIGMA = INT(ARRAY(3))                                                    
      COHMIN = ARRAY(4)                                                         
      NDMIN = INT(ARRAY(5))                                                     
      SVELO = ARRAY(6)                                                          
      NFILT = INT(ARRAY(7))                                                     
      CRAT = ARRAY(8)                                                           
      LWIN = INT(ARRAY(9))                                                      
                                                                                
      CODA = 0.0                                                                
                                                                                
      DO 100 KF=1,NFILT                                                         
        PARAMETER = 'filter_x    '                                              
        WRITE(PARAMETER(8:8),'(I1)') KF                                         
        CALL PARAM(PARAMETER,NR,ARRAY)                                          
        WINDOW = ARRAY(1)                                                       
        F1 = ARRAY(2)                                                           
        F2 = ARRAY(3)                                                           
        THRSH1 = ARRAY(4)                                                       
        THRSH2 = ARRAY(5)                                                       
        DELTA = WINDOW/LWIND                                                    
        FCEN = (F1+F2)/2.                                                       
        SVEL = SVELO - FCEN/5. + 1.                                             
        QL4 = 2*(THRSH1-1)*NDMIN                                                
        QL3 = 2*QL4                                                             
        QL2 = 2*QL3                                                             
        QL1 = 2*QL2                                                             
                                                                                
C  FILTER DATA                                                                  
                                                                                
        IF (IFILTERSWITCH.EQ.1) THEN                                              
          IF (NCHAN.EQ.1) THEN                                                      
            DO 40 I=1,NSMP                                                          
   40       FDATA(I,1) = DATA(I,3)                                                  
          ELSE                                                                      
            DO 50 J=1,NCHAN                                                         
              DO 50 I=1,NSMP                                                          
   50           FDATA(I,J) = DATA(I,J)                                                  
          ENDIF                                                                     
                                                                                
          SMPINT = 1000./SRATE                                                      
          CALL BNDPAS(F1,F2,SMPINT,DD,GAIN)                                         
          DO 60 J=1,NCHAN                                                           
            CALL FILTER(FDATA(1,J),NSMP,DD,GAIN,1)                                    
c            call recfil(fdata(1,j),nsmp,fdata(1,j),'BU      ',0.0,0.0,4,
c    *'BP      ',f1,f2,1.0/srate,1)
   60       CONTINUE                                                                  
        ENDIF                                                                     
                                                                                
C  COMPUTE STA'S                                                                
                                                                                
        SEGLEN = NSMP/SRATE                                                       
        DELTA = INT(DELTA*SRATE+0.5)/SRATE                                        
        WINDOW = DELTA*LWIND                                                      
        SHIFT = DELTA*ISHIFT                                                      
        LSTEP = DELTA*SRATE + 0.5                                                 
        NSTEP = (SEGLEN-WINDOW-5.0)/DELTA + 0.5                                   
        ISMP = (0.5*WINDOW)*SRATE + 0.5                                           
        IF (NCHAN.EQ.3) THEN                                                      
          IF (IFILTERSWITCH.EQ.1) THEN                                            
            CALL COHPOW(FDATA,200000,ISMP,LSTEP,NSTEP,LWIND,                       
     +                COH,AZI,VRAT,STAX,STAY,STAZ)                              
c-- UNFILTERED DATA                                             
          ELSE						
            CALL COHPOW(DATA,200000,ISMP,LSTEP,NSTEP,LWIND,                        
     +                COH,AZI,VRAT,STAX,STAY,STAZ)                              
          ENDIF                                                                   
        ELSE                                                                      
          IF (IFILTERSWITCH.EQ.1) THEN                                            
            CALL POW(FDATA,ISMP,LSTEP,NSTEP,LWIND,STAZ)                           
          ELSE                                                                    
            CALL POW(DATA,ISMP,LSTEP,NSTEP,LWIND,STAZ)                            
          ENDIF                                                                   
        ENDIF                                                                     
                                                                                
C  INITIALIZE LTA                                                               
                                                                                
        TLTA = 10.0                                                               
        LLTA = TLTA/DELTA + 0.5                                                   
        LTAX = 0.                                                                 
        LTAY = 0.                                                                 
        LTAZ = 0.                                                                 
        DO 70 J=1,LLTA                                                            
          LTAX = LTAX + STAX(J)                                                   
          LTAY = LTAY + STAY(J)                                                   
   70     LTAZ = LTAZ + STAZ(J)                                                   
        LTAX = LTAX/LLTA                                                          
        LTAY = LTAY/LLTA                                                          
        LTAZ = LTAZ/LLTA                                                          
                                                                                
C  FIND END OF CODA                                                             
                                                                                
        IF (NCHAN.EQ.3) THEN                                                      
          CALL CODAXYZ(STAX,STAY,STAZ,LTAX,LTAY,LTAZ,CRAT,LWIN,                   
     +               DELTA,NSTEP,CEND)                                          
        ELSE                                                                      
          CALL CODAZ(STAZ,LTAZ,CRAT,LWIN,DELTA,NSTEP,CEND)                        
        ENDIF                                                                     
        IF (CEND.GT.CODA) CODA=CEND                                               
c
C  OPEN DETECTOR OUTPUT FILE...
c  ----------------------------
c
        if( kf .gt. 1 ) then                 ! To maximise available units.
          call sei close( delete$, idet, code )! Delete last detector unit.
        end if                               !
c
        call sei open( scratch$,             ! Open (default stop on error).
     &               ' ',                  ! No prompt.
     &               ' ',                  ! File name to open (n/a).
     &               idet,                 ! On unit.
     &               b_flag,               ! Flag exists.
     &               code )                ! Condition (n/a).
c
        WRITE(FNAME,'(A,I1,2A)')'detect-',KF,'.',STATION
        WRITE(IDET,200,iostat=code)
     &      'D','H','M','SEC','C','STA','LTA','SNR',                   
     +      'COH','AZI','VEL'                                            
        call sei code( fort$, code, idet, b_flag )  ! Process outcome.
c
  200   FORMAT(/1X,3A3,A6,A3,A7,A6,A6,A5,A6,A5/)                                  
                                                                                
C  THE FOLLOWING 3 LINES ARE JUST TO MAKE SURE THAT ALL TRIGS ARE               
C  WRITTEN OUT FOR THE LAST DETECTION.                                          
                                                                                
        NSTEP = NSTEP - LLTA                                                      
        STAX(NSTEP)=-1.                                                           
        STAY(NSTEP)=-1.                                                           
        STAZ(NSTEP)=-1.                                                           
                                                                                
C  NOW DO DETECTION                                                             
                                                                                
        START = EPOTIM + 0.5*WINDOW + TLTA                                        
        JJ = LLTA + 1                                                             
        II = JJ - ISHIFT
        if(jj.le.0) jj=1     !jhfix
	    if(ii.le.0) ii=1     !jhfix
	                                                            
        IF (NCHAN.EQ.3) THEN                                                      
          CALL DET3C(START,THRSH1,THRSH2,COHMIN,NDMIN,                            
     +       DELTA,ISHIFT,ISIGMA,SVEL,                                          
     +       LTAX,LTAY,LTAZ,STAX(II),STAY(II),STAZ(II),                         
     +       COH(JJ),AZI(JJ),VRAT(JJ),NSTEP,IDET)                               
        ELSE                                                                      
          CALL DET1C(START,THRSH2,NDMIN,                                          
     +       DELTA,ISHIFT,ISIGMA,                                               
     +       LTAZ,STAZ(II),NSTEP,IDET)                                          
        ENDIF                                                                     
c
        WRITE(IDET,*,iostat=code)
        call sei code( fort$, code, idet, b_flag )    ! Process the outcome.
        REWIND( IDET, iostat=code )
        call sei code( fort$, code, idet, b_flag )    ! Process the outcome.
c
C  OPEN DETSCAN OUTPUT FILE...
c  ---------------------------
c
        call sei open( scratch$,             ! Open (default stop on error).
     &               ' ',                  ! No prompt.
     &               ' ',                  ! File name to open (n/a).
     &               isca_u(kf),           ! On unit.
     &               b_flag,               ! Flag exists.
     &               code )                ! Condition (n/a).
        isca = isca_u(kf)                    ! Working unit.
c
        WRITE(FNAME,'(A,I1,2A)')'detsca-',KF,'.',STATION
c
C  SCAN THROUGH RAW DETECTION AND WRITE A COMPRESSED DETECTION FILE             
c
        FREQ=(F1+F2)/2.                                                           
        CALL DETSCAN(IDET,ISCA,                                                   
     +     DELTA,WINDOW,FREQ,THRSH1,THRSH2,COHMIN,NDMIN,                        
     +     QL1,QL2,QL3,QL4)                                                     
        REWIND( ISCA, iostat=code )
        call sei code( fort$, code, isca, b_flag ) ! Process the outcome.
  100 CONTINUE                                                                  
c
C  MERGE THE DETSCAN FILES INTO ONE...
c  ===================================
c
      WRITE(FNAME,'(2A)')'detmerge.',STATION
      call sei open( unknown$,             ! Open (default stop on error).
     &               ' ',                  ! No prompt.
     &               fname,                ! File name to open.
     &               imer,                 ! On unit.
     &               b_flag,               ! Flag exists.
     &               code )                ! Condition (n/a).
c
      CALL DETMERGE(ISCA_u,NFILT,IMER)                                       
c
      CODAEPO=EPOTIM+CODA                                                       
      CALL EPOGMT(CODAEPO,YEAR,GMT,ITIME,TIMCOR)                                
      SEC=GMT(4)+0.1*GMT(5)+TIMCOR                                              
      REWIND( IMER, iostat=code )
      call sei code( fort$, code, imer, b_flag )  ! process outcome.
c
C  TRY TO DO ASSOCIATION AND LOCATION FROM DETECTOR OUTPUT...
c  ----------------------------------------------------------
c
      WRITE(FNAME,'(2A)')'detasso.',STATION
      call sei open( unknown$,             ! Open (default stop on error).
     &               ' ',                  ! No prompt.
     &               fname,                ! File name to open.
     &               iass,                 ! On unit.
     &               b_flag,               ! Flag exists.
     &               code )                ! Condition (n/a).
c
      hypocen(:80) = ' '                   ! Empty string (why an argument?!)
      CALL ASSOCIAT(IMER,IASS,IOUT,SLAT,SLON,NCHAN,HYPOCEN)
c
      WRITE(IOUT,'(1X,3I3,F6.2,A)',iostat=code)
     &     (GMT(I),I=1,3),SEC,'   End of coda'             
      call sei code( fort$, code, iout, b_flag )  ! Process the outcome.
      WRITE(IOUT,*,iostat=code)     
      call sei code( fort$, code, iout, b_flag )  ! Process the outcome.
c
c    Close down the files opened in this module...
c    =============================================
c
      call sei close( delete$, idet, code ) ! Delete (stop on error).
      call sei close( close$, iass, code ) ! Et ditto.
      call sei close( close$,  imer, code ) ! Close (stop on error).
c
      do ic = 1, nfilt                           ! Loop array of units.
      call sei close( delete$, isca_u(ic), code )! & delete.
      end do                                     !
c
c    Return to Caller...
c    ===================
c
      RETURN                                                                    
      END                                                                       
c
      SUBROUTINE DETMERGE(ISCA_u,NFILT,IMER)                                 
C                                                                               
C     THIS PROGRAM READS THE DETECTION FILES CREATED BY DETSCAN (ONE            
C     FILE FOR EACH FILTER) AND MERGE THEM INTO ONE FILE.                       
C                                                                               
      INTEGER YEAR,GMT(5),DOY(50),HOUR(50),MIN(50),DA(50),NDET(50),             
     +        NH(50),NV(50),NCOH(50),IQL(50),IPS(50),STA(50),AZI(50)            
      REAL SEC(50), DUR(50), FRQ(50), SNR(50), VEL(50), DV(50)                  
      REAL QUAL(50)                                                             
      REAL*8 EPOTIM(50), EPOMIN, EPOEND                                         
      LOGICAL EOF(50)                                                           
      CHARACTER TEXT*78                                                         
c
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code                    ! Error condition handler.
      integer  code,                       ! Condition.
     &         imer,                       ! File units.
     &         isca_u(*)                   ! Ditto.
      logical  b_flag                      ! Working flag?.
c
c    ----- end -----
c
      DATA YEAR/1990/                                                           
c
c    Initialise...
c    =============
c
      DO 1 I=1,50                                                               
    1   EOF(I) = .FALSE.                                                          
      NEOF = 0                                                                  
                                                                                
C  READ ONE DETECTION FROM EACH FILE                                            
                                                                                
      DO 10 I=1,NFILT                                                           
        IN = ISCA_u(i)                ! Get unit.
c
        DO 31 J=1,3                                                               
          READ(IN,'(A)',iostat=code) TEXT
          call sei code( fort$, code, in, b_flag )   ! Process outcome.
c
          IF (I.EQ.1) WRITE(IMER,'(A)',iostat=code) TEXT
          call sei code( fort$, code, imer, b_flag ) ! Process outcome.
   31   CONTINUE
c
        READ(IN,200,iostat=code)
     +     DOY(I),HOUR(I),MIN(I),SEC(I),DUR(I),FRQ(I),SNR(I),                   
     +     STA(I),NDET(I),NH(I),NV(I),NCOH(I),IQL(I),IPS(I),                    
     +     AZI(I),DA(I),VEL(I),DV(I),QUAL(I)                                    
        call sei code( fort$, code, in,          ! Process outcome.
     &               eof(i) )                  ! End of file?.
c
  200 FORMAT(1X,3I3,2F6.0,2F5.0,I6,6I3,I4,I3,F5.0,F4.0,F6.0)                    
c
c    Proceed on the outcome...
c    =========================
c    End of this filter file...
c    --------------------------
c
        if( eof(i) ) then                        ! End of file.
          neof = neof + 1                          ! Increment those processed.
          if( neof .eq. nfilt ) then            ! Finished.
            goto 900                              ! Return to caller.
          else                                  ! Otherwise next filter.
            goto 10                               !
          end if                                !
c
c    Process this one...
c    -------------------
c
        else
          gmt(1) = doy(i)                                                           
          gmt(2) = hour(i)                                                          
          gmt(3) = min(i)                                                           
          gmt(4) = int(sec(i))                                                      
          gmt(5) = 0                                                                
c
          call gmtepo(year,gmt,iirsps,epotim(i))                                    
          epotim(i) = epotim(i) + (sec(i)-int(sec(i)))                              
        end if
   10   continue                                                                  
c
C  FIND EARLIEST DETECTION...
c  ==========================
c
20    N = 0                                                                     
      EPOMIN = 999999999999.D0                                                  
      DO 30 I=1,NFILT                                                           
        IF (.NOT.EOF(I).AND.EPOTIM(I).LT.EPOMIN) THEN                           
          N = I                                                                 
          EPOMIN = EPOTIM(I)                                                    
          EPOEND = EPOTIM(I) + DUR(I)                                           
        ENDIF                                                                   
   30 CONTINUE                                                                  
c
C  CHECK FOR OVERLAPPING DETECTIONS...
c  ===================================
c
      DO 35 I=1,NFILT                                                           
        IF (I.EQ.N.OR.EOF(I)) GO TO 35                                          
        IF (EPOTIM(I).LT.EPOEND) THEN                                           
          QI = (SNR(I)-1.)*STA(I)*(NDET(I)+NCOH(I))                             
          QN = (SNR(N)-1.)*STA(N)*(NDET(N)+NCOH(N))                             
          IF (QI.LT.QN) N = I                                                   
          GO TO 40                                                              
        ENDIF                                                                   
   35 CONTINUE                                                                  
                                                                                
C  WRITE OUT EARLIEST DETECTION                                                 
                                                                                
      WRITE(IMER,300,iostat=code) 
     &      DOY(N),HOUR(N),MIN(N),SEC(N),DUR(N),FRQ(N),               
     +      SNR(N),STA(N),NDET(N),NH(N),NV(N),NCOH(N),
     &      IQL(N),IPS(N),                
     +      AZI(N),DA(N),VEL(N),DV(N),QUAL(N)
      call sei code( fort$, code, imer, b_flag )   ! Process the outcome.
  300 FORMAT(1X,3I3,2F6.2,2F5.1,I6,6I3,I4,I3,F5.1,F4.1,F6.1)                    
c
C  READ A NEW DETECTION...
c  =======================
c                                                         
   40 IN = ISCA_u(n)                         ! Relevant unit.
      READ(in,200,iostat=code)
     +     DOY(N),HOUR(N),MIN(N),SEC(N),DUR(N),FRQ(N),SNR(N),                   
     +     STA(N),NDET(N),NH(N),NV(N),NCOH(N),IQL(N),IPS(N),                    
     +     AZI(N),DA(N),VEL(N),DV(N),QUAL(N)
      call sei code( fort$, code, in,          ! Process outcome.
     &               eof(n) )                  ! End of file?.
c
c   Process the outcome...
c   ======================
c   End of this filter file...
c   --------------------------
c
      if( eof(n) ) then                        ! End of file.
      neof = neof + 1                          ! Increment those processed.
         if( neof .eq. nfilt ) then            ! Finished.
         goto 900                              ! Return to caller.
         else                                  ! Otherwise next filter.
         goto 20                               !
         end if                                !
c
c   Process this one...
c   -------------------
c
      else                                     ! Otherwise.
      gmt(1) = doy(n)                                                           
      gmt(2) = hour(n)                                                          
      gmt(3) = min(n)                                                           
      gmt(4) = int(sec(n))                                                      
      gmt(5) = 0                                                                
c
      call gmtepo(year,gmt,iirsps,epotim(n))                                    
      epotim(n) = epotim(n) + (sec(n)-int(sec(n)))                              
      goto 20                                                                  
      end if
c
c   Return to Caller...
c   ===================
c
900   return
      end                                                                       
c
      SUBROUTINE DETSCAN(IN,IOUT,DELTA,WINDOW,FREQ,THRSH1,THRSH2,               
     +           COHMIN,NDMIN,QL1,QL2,QL3,QL4)                                  
C                                                                               
C     THIS PROGRAM READS THE RAW DETECTOR OUTPUT FILES (ONE FOR EACH FILTER)    
C     AND WRITES THE COMPRESSED DETECTIONS (ONE LINE PER DETECTION) TO          
C     OTHER FILES.                                                              
C                                                                               
      INTEGER DOY(200), HOUR(200), MIN(200)                                     
      REAL SEC(200), STA(200), LTA(200), SNR(200), COH(200),                    
     +     AZI(200), VEL(200)                                                   
      REAL*8 EPOTM1, EPOTM2                                                     
      INTEGER YEAR, GMT(5)                                                      
      CHARACTER LINE*80, COMP(200)                                              
c
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code                    ! Error condition handler.
      integer  code,                       ! Condition.
     &         in, iout                    ! I/o units.
      logical  b_flag                      ! Working flag?.
c
c    ----- end -----
c
      DATA YEAR/1990/                                                           
c
c    Initial reading...
c    ==================
c
      READ(IN,*,iostat=code)
      call sei code( fort$, code, in, b_flag )  ! Process the outcome.
      READ(IN,*,iostat=code)
      call sei code( fort$, code, in, b_flag )  ! Process the outcome.
      READ(IN,*,iostat=code)
      call sei code( fort$, code, in, b_flag )  ! Process the outcome.
c
      GAPMIN = 2*WINDOW                                                         
c
C  WRITE A HEADING...
c  ------------------
c
      WRITE(IOUT,201,iostat=code) 
     &      'D','H','M','SEC','DUR','FRQ','SNR','STA',                
     +      'NT','NH','NV','NC','Q','PS','AZI','DA','VEL',
     &      'DV','QUAL'                
      call sei code( fort$, code, iout, b_flag ) ! Process the outcome.
  201 FORMAT(/1X,3A3,2A6,2A5,A6,6A3,A4,A3,A5,A4,A6/)                            
c
C  READ DETECTION SEQUENCE...
c  ==========================
c
   10 I = 0                                                                     
      DUR = 0.0                                                                 
c
   20 READ(in,'(a)',iostat=code) LINE
      call sei code( fort$, code, in, b_flag )   ! Process the outcome.
c
c    Process the resulting outcome...
c    ================================
c    End of File...
c    ==============
c
99    if( b_flag ) then                          ! End of input file.
         if (i.gt.0) then                        ! Complete last processing.
         ndet = i - 1
         call reduce(doy,hour,min,sec,comp,sta,lta,snr,
     +               coh,azi,vel,ndet,dur,freq,
     +               thrsh1,thrsh2,cohmin,ndmin,
     +               ql1,ql2,ql3,ql4,iout)
         end if
c
c    Otherwise...
c    ============
c
      else if( LINE(4:4) .ne. ' ') then          !
      I = I + 1                                                               
      DUR = DUR + DELTA                                                       
      READ(LINE,100) DOY(I),HOUR(I),MIN(I),SEC(I),                            
     +               COMP(I),STA(I),LTA(I),SNR(I),COH(I)                                   
  100 FORMAT(1X,3I3,F6.0,2X,A1,F7.0,2F6.0,F5.0,F6.0,F5.0)                     
         IF( COH(I) .GT. COHMIN ) THEN                                          
         READ(LINE,200) AZI(I),VEL(I)                                          
         ENDIF                                                                 
  200    FORMAT(43X,F6.0,F5.0)                                                  
      GO TO 20                                                                
c
C  CHECK TIME GAP BETWEEN DETECTION SEQUENCES
C  IF GAP IS LESS THAN GAPMIN THE TWO SEQUENCES ARE MERGED INTO ONE
c
      else if( i .ne. 0 ) then
      gmt(1) = doy(i)                                                         
      gmt(2) = hour(i)                                                        
      gmt(3) = min(i)                                                         
      gmt(4) = int(sec(i))                                                    
      gmt(5) = 0                                                              
      call gmtepo(year,gmt,iirsps,epotm1)                                     
      epotm1 = epotm1 + (sec(i) - int(sec(i)))                                
c
      read(in,'(a)',iostat=code) line
      call sei code( fort$, code, in, b_flag )  ! Process the outcome.
          if( b_flag ) then                     ! End of file.
          goto 99                               !
          else if( line(4:4) .eq. ' ' ) then    ! Also finished.
          goto 99                               !
          end if                                !
c 
        I = I + 1                                                               
        READ(LINE,100) DOY(I),HOUR(I),MIN(I),SEC(I),                            
     +                 COMP(I),STA(I),LTA(I),SNR(I),COH(I)
c                                   
        IF (COH(I).GT.COHMIN) READ(LINE,200) AZI(I),VEL(I)                      
        GMT(1) = DOY(I)                                                         
        GMT(2) = HOUR(I)                                                        
        GMT(3) = MIN(I)                                                         
        GMT(4) = INT(SEC(I))                                                    
        GMT(5) = 0                                                              
        CALL GMTEPO(YEAR,GMT,IIRSPS,EPOTM2)                                     
        EPOTM2 = EPOTM2 + (SEC(I) - INT(SEC(I)))                                
c
        GAP = EPOTM2 - EPOTM1                                                   
        IF (GAP.LE.GAPMIN) THEN                                                 
          DUR = DUR + GAP                                                       
          GO TO 20                                                              
        ELSE                                                                    
          NDET = I - 1                                                          
          CALL REDUCE(DOY,HOUR,MIN,SEC,COMP,STA,LTA,SNR,                        
     +                COH,AZI,VEL,NDET,DUR,FREQ,                                
     +                THRSH1,THRSH2,COHMIN,NDMIN,                               
     +                QL1,QL2,QL3,QL4,IOUT)                                     
c
          backspace( in, iostat=code ) 
          call sei code( fort$, code, in, b_flag )  ! Process outcome.
          goto 10                                                              
        ENDIF                                                                   
      ENDIF                                                                     
C
C    Return to Caller...
C    ===================
C
9999  return
      end
C
      SUBROUTINE LOCAT1 (SLA,SLO,AZ,DIST,ELA,ELO)                               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                       
      REAL SLA, SLO, AZ, DIST, ELA, ELO                                         
C----------------------------------------------------------------------         
C     CALCULATE EPICENTRE LOCATIONS.                                            
C                                                                               
C     INPUTS:  SLA,SLO   = STATION LAT,LONG (DEG)                               
C              AZ        = ESTIMATED AZIMUTH (DEG)                              
C              DIST      = EPICENTRAL DISTANCE (DEG)                            
C     OUTPUT:                                                                   
C              ELA,ELO   = EPICENTRE LAT,LONG (DEG)                             
C                                                                               
C     METHOD: SIMPLE TRIANGLE ON A SPHERE WITH ANGLES DOUBLE LETTERED           
C             AND DISTANCES SINGLED LETTERED (A=STA.,B=N.POLE,C=EPI.C).         
C----------------------------------------------------------------------         
C                                                                               
C     EPICENTRE LATITUDE                                                        
C                                                                               
      RAD=3.141596D0/180.D0                                                     
                                                                                
      AA = AZ*RAD                                                               
      B = DIST*RAD                                                              
      C = (90.0D0 - SLA)*RAD                                                    
C                                                                               
      ACOSA = DCOS(B)*DCOS(C) + DSIN(B)*DSIN(C)*DCOS(AA)                        
      SLAL = DACOS(ACOSA)/RAD                                                   
      SLAA = 90.0D0 - SLAL                                                      
C                                                                               
C     EPICENTRE LONGITUDE                                                       
C                                                                               
      ASINA = DSIN(SLAL*RAD)                                                    
      ACOSA = DCOS(SLAL*RAD)                                                    
      SPROD = ASINA*DSIN(C)                                                     
      IF (SPROD.LE. 0.001D0.AND.SPROD.GE.0.D0) SPROD = 0.001D0                  
      IF (SPROD.GE.-0.001D0.AND.SPROD.LE.0.D0) SPROD =-0.001D0                  
      BCOSB = (DCOS(B) - ACOSA*DCOS(C))/SPROD                                   
C                                                                               
      BSINB = DSIN(B)*DSIN(AA)/ASINA                                            
      SLOO = SLO + DATAN2(BSINB,BCOSB)/RAD                                      
      IF (SLOO.GT.180.0D0) SLOO = SLOO - 360.0D0                                
C                                                                               
      ELA = SLAA                                                                
      ELO = SLOO                                                                

C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LOCLOC (SLA,SLO,AZI,PTOLG,ELA,ELO,DIST,TRAV)                   
C                                                                               
C   LOCATE LOCAL EVENT GIVEN AZIMUTH (DEG) AND P-TO-LG TIME                     
C   DIFFERENCE (S)                                                              
C                                                                               
      integer nb, nc                            !JAB(BGS)Mar95.
      PARAMETER (NB=3)                                                          
      REAL DISTS(NB),PTIMES(NB),DIFFS(NB)                                       
C                                                                               
      DATA DISTS/0.0,175.0,1000./                                               
      DATA PTIMES/0.0,28.1,129.3/                                               
      DATA DIFFS/0.0,20.9,150.7/                                                
C                                                                               
      DO 10 I=1,NB                                                              
      nc = i                                     !JAB(BGS)Mar95. Store pointer.
        IF( PTOLG .GT. DIFFS(I) ) then           !JAB(BGS)Mar95. Next range.
        continue                                 !JAB(BGS)Mar95.
        else                                     !JAB(BGS)Mar95. Otherwise.
        GO TO 20                                 !JAB(BGS)Mar95. Interpolate.
        end if                                   !JAB(BGS)Mar95.
  10  CONTINUE                                                                  
                                                                                
C   DIFFERENCE GREATER THAN MAX IN TABLE                                        
       nc = nb                                   !JAB(BGS)Mar95.
                                                                                
C   DIFFERENCE OK                                                               
                                                                                
20    i = nc                                     !JAB(BGS)Mar95.
      if( i .eq. 1 ) then                        !JAB(BGS)Mar95. At minimum.
      dist = dists(1)                            !JAB(BGS)Mar95. & set.
      trav = ptimes(1)                           !JAB(BGS)Mar95.
c
      else                                       !JAB(BGS)Mar95.
      FRAC=(abs(PTOLG)-DIFFS(I-1))/(DIFFS(I)-DIFFS(I-1))                        
      DIST=DISTS(I-1)+FRAC*(DISTS(I)-DISTS(I-1))                                
      TRAV=PTIMES(I-1)+FRAC*(PTIMES(I)-PTIMES(I-1))                             
      end if                                     !JAB(BGS)Mar95.
c
      DELTA=DIST/111.2                                                          
      CALL LOCAT1 (SLA,SLO,AZI,DELTA,ELA,ELO)                                   
c
c    Return to Caller...
c    ===================
c
      RETURN                                                                    
      END                                                                       
c
      subroutine param(parameter,nr,array)                                      
c     subroutine to read input parameters used in program AUTOPIC           
c     Written by C. Lindholm, April 11, 1991                                    
c                                                                               
c     Input:                                                                    
c        parameter: Expexted values: 'fixed', 'filter_x' or 'stations'          
c                                                                               
c     Output:                                                                   
c        nr       : Number of parameters returned in array                      
c        array    : array of real parameter values                              
c                                                                               
      implicit none 
      character*80 string
      real array(*)                                                            
      character*(*) parameter                                                    
      integer nr                                                         
C
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei get file,               ! Find & open file.
     &         sei close,                  ! & closure.
     &         sei code                    ! Error condition handler.
      integer  code,                       ! Condition.
     &         read1                       ! Read unit1.
      character chr_text *(80)             ! & local string.
c
      logical  b_flag                      ! Flag!!
C
C    ------- End of details -------
c                                                                               
c    Open the parameter file and read contents...
c    ============================================
c
       chr_text = 'AUTOPIC.INP'             ! Filename.
       call sei get file( open$+ignore$,     ! Find,open file & ignore warnings.
     &                    read1,             ! On unit.
     &                    code,              ! Returned condition.
     &                    'DAT',             ! Alternative directory to search.
     &                    chr_text )         ! File to find & return pathname.
       chr_err_msg$ =                        ! Message.
     &'**** ERROR: unable to open AUTOPIC.INP             ****'
       call sei code(fort$,code,read1,b_flag)! Process outcome.
c
c    Fixed...
c    ========
c
      if( parameter(1:5) .eq. 'fixed' ) then !
100   	read(read1,'(a)',iostat=code)string    !
      	call sei code(fort$,code,read1,b_flag) ! Process the outcome.
c
c    Premature end of file...
c    ------------------------
c
         if( b_flag ) then                         !
         	chr_err_msg$ =                            ! Local message.
     &'**** ERROR: premature end of file "AUTOPIC.INP"..'
         	call sei code(fort$,e_peof$,read1,b_flag) ! Message and halt.
c
c    Get details...
c    --------------
c
         else if( string(1:1) .eq. ' ' )then       ! Get detail.
         	read(string,'(10f7.0)')array(1),array(2),array(3),                     
     +   	array(4),array(5),array(6),array(7),array(8),array(9),                 
     +   	array(10)                                                              
         	nr = 9                                                                 
         	call sei close( close$, read1, code )     ! Close, stop on error.
c
c    Next record...
c    --------------
c
         else                                      ! Otherwise.
         	goto 100                                  ! Next record.
         end if                                    !
c
c    Filter...
c    =========
c
      else if( parameter(1:6) .eq. 'filter' ) then
200   	read(read1,'(a)',iostat=code)string    !
      	call sei code(fort$,code,read1,b_flag) ! Process the outcome.
c
c    Premature end of file...
c    ------------------------
c
         if( b_flag ) then                         !
         	chr_err_msg$ =                            ! Local message.
     &'**** ERROR: premature end of file "AUTOPIC.INP"..'
         	call sei code(fort$,e_peof$,read1,b_flag) ! Message and halt.
c
c    Get details...
c    --------------
c
         else if( string(1:12) .eq. parameter )  then                           
         	read(string,'(10x,6f10.0)')array(1),array(2),                         
     +   	array(3),array(4),array(5)                                          
         	nr = 5                                                                 
         	call sei close( close$, read1, code )     ! Close, stop on error.
c
c    Next record...
c    --------------
c
         else                                      ! Otherwise.
         	goto 200                                  ! Next record.
         end if                                    !
c
c     Invalid...
c     ==========
c
      else                                              ! Otherwise.
      	write(*,*)'**** ERROR: in routine parameter...'
      	chr_err_msg$ = 
     &'**** ERROR: invalid parameter, should be "fixed" or "filter"'
      	call sei code( stop$, e_init$, 0, b_flag )        ! & halt.
      end if                                            !
c
c    Return to Caller...
c    ===================
c
9999  return
      end                                                                       
c
      SUBROUTINE PHASEID (FRQ,IPS,VEL,PHASE)                                    
      CHARACTER*5 PHASE                                                         
                                                                                
C     S/R TO DO INITIAL PHASE IDENTIFICATION                                    
                                                                                
      DATA FLIMU,FLIML/7.0,4.0/                                                 
      DATA VLIMU,VLIML/11.5,5.5/                                                
                                                                                
      PHASE = '?????'                                                           
                                                                                
      IF (FRQ.LT.FLIML) THEN                                                    
         CONTINUE                                                               
         IF (IPS.GE.4) THEN                                                     
            CONTINUE                                                            
            IF (VEL.GT.VLIMU) THEN                                              
              PHASE = 'Ptele'                                                   
            ELSE IF (VEL.GT.VLIML) THEN                                         
              PHASE = 'Pn/Pg'                                                   
            ENDIF                                                               
         ELSE                                                                   
            PHASE = 'Sn/Lg'                                                     
         ENDIF                                                                  
      ELSE IF (FRQ.LT.FLIMU) THEN                                               
         CONTINUE                                                               
         IF (IPS.GE.4) THEN                                                     
            CONTINUE                                                            
            IF (VEL.GT.VLIML.AND.VEL.LT.VLIMU) THEN                             
               PHASE = 'Pn/Pg'                                                  
            ENDIF                                                               
         ELSE                                                                   
            PHASE = 'Sn/Lg'                                                     
         ENDIF                                                                  
      ELSE                                                                      
         CONTINUE                                                               
         IF (IPS.GE.3) THEN                                                     
            CONTINUE                                                            
            IF (VEL.GT.VLIML) THEN                                              
               PHASE = 'Pn/Pg'                                                  
            ENDIF                                                               
         ELSE                                                                   
            PHASE = 'Sn/Lg'                                                     
         ENDIF                                                                  
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END
c
c                                                                       
                                                                                
      SUBROUTINE PHASSO(EPOTIM,DUR,FRQ,SNR,STA,NDET,NH,NV,NCOH,                 
     +           IQL,IPS,AZI,DA,VEL,DV,ND,NOUT,PHASE,PTOLG,AZIM)                
      INTEGER NDET(50),NH(50),NV(50),NCOH(50),IQL(50),IPS(50),                  
     +        STA(50),AZI(50),DA(50)                                            
      REAL DUR(50), FRQ(50), SNR(50), VEL(50), DV(50)                           
      REAL*8 EPOTIM(50)                                                         
      CHARACTER*5 PHASE(50)                                                     
                                                                                
      PTOLG = -1.                                                               
                                                                                
      IF (PHASE(1).EQ.'Pn/Pg') THEN                                             
                                                                                
C  Look for strongest S = Lg                                                    
                                                                                
        NLG = 0                                                                 
        LGSTA = 0                                                               
        DO 10 I=2,ND                                                            
          IF (PHASE(I)(1:1).NE.'S') GO TO 10                                    
          IF (STA(I).GT.LGSTA) THEN                                             
            LGSTA = STA(I)                                                      
            NLG = I                                                             
          ENDIF                                                                 
   10   CONTINUE                                                                
        IF (NLG.EQ.0) THEN                                                      
          NOUT = 1                                                              
          RETURN                                                                
        ENDIF                                                                   
                                                                                
C  Look for strongest P                                                         
                                                                                
        NMP = 0                                                                 
        MPSTA = 0                                                               
        DO 12 I=1,NLG-1                                                         
          IF (PHASE(I).NE.'Pn/Pg') GO TO 12                                     
          IF (STA(I).GT.MPSTA) THEN                                             
            MPSTA = STA(I)                                                      
            NMP = I                                                             
          ENDIF                                                                 
   12   CONTINUE                                                                
                                                                                
C  Look for first P with same azimuth                                           
                                                                                
        NIP = 0                                                                 
        DO 14 I=NMP-1,1,-1                                                      
          AZIDIF = ABS(AZI(I)-AZI(NMP))                                         
          IF (AZIDIF.GT.180) AZIDIF = ABS(AZIDIF - 360)                         
          IF (AZIDIF.LE.30) THEN                                                
            NIP = I                                                             
          ENDIF                                                                 
   14   CONTINUE                                                                
                                                                                
        IF (NIP.EQ.0) NIP = NMP                                                 
                                                                                
        IF (NIP.EQ.2) THEN                                                      
          NOUT = 1                                                              
          RETURN                                                                
        ELSE IF (NIP.GT.2) THEN                                                 
          NOUT = 1 - NIP                                                        
          RETURN                                                                
        ENDIF                                                                   
                                                                                
C  Chech frequencies                                                            
                                                                                
        IF (FRQ(NLG).GT.FRQ(NMP)) THEN                                          
          NOUT = 1                                                              
          RETURN                                                                
        ENDIF                                                                   
                                                                                
C  Comput S-P time difference                                                   
                                                                                
        PTOLG = EPOTIM(NLG) - EPOTIM(NIP)                                       
                                                                                
C  Set event azimuth                                                            
                                                                                
        AZIM = AZI(NMP)                                                         
                                                                                
        PHASE(NLG) = 'Lg   '                                                    
                                                                                
C  Look for first S                                                             
cfix nlp =?                                                                                
        NIS = 0                                                                 
        DO 16 I=NLP-1,NMP+1,-1                                                  
          TOLG = EPOTIM(NLG) - EPOTIM(I)                                        
          IF (TOLG.GT.(0.4*PTOLG)) GO TO 17                                     
          IF (PHASE(I).EQ.'Sn/Lg') THEN                                         
            NIS = I                                                             
            GO TO 17                                                            
          ENDIF                                                                 
   16   CONTINUE                                                                
   17   CONTINUE                                                                
                                                                                
C  Look for last S                                                              
                                                                                
        NOUT = NLG                                                              
        DO 18 I=NLG+1,ND                                                        
          FRLG = EPOTIM(I) - EPOTIM(NLG)                                        
          IF (FRLG.GT.(0.5*PTOLG)) GO TO 19                                     
          IF (PHASE(I).EQ.'Sn/Lg') THEN                                         
            NOUT = I                                                            
          ELSE                                                                  
            RETURN                                                              
          ENDIF                                                                 
   18   CONTINUE                                                                
   19   CONTINUE                                                                
                                                                                
      ELSE IF (PHASE(1).EQ.'Ptele') THEN                                        
                                                                                
        DO 20 I=2,ND                                                            
          IF (PHASE(I).NE.'Ptele') GO TO 25                                     
          GAP = EPOTIM(I) - EPOTIM(1)                                           
          IF (GAP.GT.30) GO TO 25                                               
          AZIDIF = ABS(AZI(I)-AZI(1))                                           
          IF (AZIDIF.GT.40) GO TO 25                                            
   20   CONTINUE                                                                
   25   NOUT = I-1                                                              
                                                                                
      ELSE IF (PHASE(1).EQ.'Sn/Lg') THEN                                        
                                                                                
        DO 30 I=2,ND                                                            
          IF (PHASE(I).NE.'Sn/Lg') GO TO 35                                     
          GAP = EPOTIM(I) - EPOTIM(1)                                           
          IF (GAP.GT.30) GO TO 35                                               
   30   CONTINUE                                                                
   35   NOUT = I-1                                                              
                                                                                
      ELSE                                                                      
                                                                                
        NOUT = 1                                                                
                                                                                
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END


ccccccccccccccccccccc

                                                                       
      SUBROUTINE POW(Z,ISMP,LSTEP,NSTEP,LWIND,POWZ)                     DET00010
      REAL Z(*),POWZ(*)                                                 DET00020
C-----------------------------------------------------------------------DET00030
C                                                                       DET00040
C  COMPUTE STA POWER SEQUENCES.                                         DET00050
C                                                                       DET00060
C  INPUT :                                                              DET00070
C                                                                       DET00080
C     Z(*)      - DATA ARRAY                                            DET00090
C     ISMP      - SAMPLE NUMBER FOR MID POINT OF FIRST TIME WINDOW      DET00100
C     LSTEP     - STEP LENGTH IN SAMPLES                                DET00110
C     NSTEP     - NO OF TIME STEPS                                      DET00120
C     LWIND     - NO OF STEPS WITHIN MOVING WINDOW                      DET00130
C                                                                       DET00140
C  OUTPUT :                                                             DET00150
C                                                                       DET00160
C     POWZ(*)   - SEQUENCE OF STA POWERS                                DET00170
C                                                                       DET00180
C-----------------------------------------------------------------------DET00190
C  THE FOLLOWING INTERNALLY DECLARED ARRAYS SHOULD BE AT LEAST          DET00200
C  NSTEP+LWIND LONG.                                                    DET00210
      REAL ZZ(6010)                                                     DET00220
C                                                                       DET00230
C  CALCULATE POWERS                                                     DET00240
C                                                                       DET00250
      LENWIN = LSTEP*LWIND                                              DET00260
      II = ISMP - LENWIN/2 - LSTEP                                      DET00270
      DO 50 I=1,NSTEP+LWIND-1                                           DET00280
        II = II + LSTEP                                                 DET00290
        CALL CROSS(Z(II),Z(II),LSTEP,ZZ(I))                             DET00300
   50 CONTINUE                                                          DET00310
C                                                                       DET00320
C  AVERAGE OVER WINDOW                                                  DET00330
C                                                                       DET00340
      DO 60 I=1,NSTEP                                                   DET00350
        AZZ = 0.                                                        DET00360
        DO 65 J=I,I+LWIND-1                                             DET00370
          AZZ = AZZ + ZZ(J)                                             DET00380
   65   CONTINUE                                                        DET00390
        POWZ(I) = AZZ                                                   DET00400
   60 CONTINUE                                                          DET00410
C                                                                       DET00420
C  NORMALIZE POWERS                                                     DET00430
C                                                                       DET00440
      DO 80 I=1,NSTEP                                                   DET00450
        POWZ(I)=POWZ(I)/LENWIN                                          DET00460
C        IF (POWZ(I).EQ.0) THEN                                                 
C          WRITE(*,*)' I, POWZ ',I,POWZ(I)                                      
C        ENDIF                                                                  
   80 CONTINUE                                                          DET00470
                                                                        DET00480
      RETURN                                                            DET00490
      END                                                               DET00500
C                                                                       DET00510
       subroutine qcheck(data,nsamp,lwind,ratio)                         
       real data(*)                                                         
c--------------------------------------------------------------                 
c    subroutine checking data quality
c                                                                               
c    method: calculate the ratio of maximum and minimum rms 
c            amplitude in a moving window              
c                                                                               
c    parameters: data   - amplitude samples                                     
c                nsamp  - total number of samples                               
c                lwind  - number of samples in moving window                    
c                ratio  - calculated quality ratio                              
c---------------------------------------------------------------                
       rmsmax = 0.                                                              
       rmsmin = 9999999999.                                                     
       sum1 = 0.                                                                
       lstep = lwind/2                                                          
       nstep = nsamp/lstep                                                      
                                                                                
       do 20 i=1,nstep                                                          
          sum2 = 0.                                                             
          isamp = (i-1)*lstep                                                   
          do 10 j=1,lwind                                                       
             sum2 = sum2 + data(isamp+j)**2                                     
 10       continue                                                              
          rms = sqrt(sum2/lwind)                                                
          if (rms.gt.rmsmax) rmsmax = rms                                       
          if (rms.lt.rmsmin) rmsmin = rms                                       
c         sum1 = sum1 + sum2                                                    
 20    continue                                                                 
c      rmsave = sqrt(sum1/(nstep*lwind))                                        
c      ratio = rmsmax/rmsave                                                   
       ratio = rmsmax/rmsmin                                                    
                                                                                
       return                                                                   
       end                                                                      

      subroutine read_write_phas(un,sfile,station,slat,slon,comp,
     +  dur,amp,per,year,month,day,hypohead,apvmin,iout)                                      
c                                                                               
c     subroutine to read phases from phase arrival files created by             
c     3 comp. automatic picking routine                                         
c                                                                               
c     Phases can be given new names after following criterias:                  
c         1. If time differnce between 2 successive phases is less than 2.0     
c            seconds then second phase is blank.                                
c         2. Only 2 phases are alowed in S file, -the rest are discarded        
c          a) The first phase is by default named a P-phase.                    
c          b) Phase nr. 2 will be named as S-phase                              
c          c) Time difference between P and S must be in the range              
c              8 - 180 seconds, -else the S will be discarded                    
c                                                                               
c                                                                               
c     written by C. Lindholm, May 1990                                          
c                                                                               
      implicit none 
      include 'seidim.inc'
c
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei code,                   ! Error condition handler.
     &         sei open,                   ! Open file handler.
     &         sei close                   ! Close file handler.
      integer  code,                       ! Condition.
     &         read1                       ! Read unit 1.
      logical  b_flag                      ! Working flag?.
c
c    ----- end -----
c
c-- Max no. of phases                                   
      integer maxphase				
      parameter (maxphase=50)                                                   
                                                                                
c     ------input:                                                              
c-- port to read from                                         
      integer un				
c-- amplitude and period                                    
      real amp,per				
c-- duration of signal                                        
      real*8 dur,msec,psec,ssec
c-- phase string to write to file                
      character*80 phasestring			
c-- station                                           
      character*5 station			
c-- comp of instrument                                  
      character*2 comp				
c-- number of phases                                      
      integer nphase				
c-- minimum allowed apparent P-velocity
      real apvmin,apvel
c-- signal duration                                         
      integer idur				
c-- phasetiming                                    
      integer year,month,day			
c-- hypocenter solution (for single 3-comp station)        
      character*80 hypohead
c-- phasetiming                                    
      integer hour(maxphase)			
c-- phasetiming                                     
      integer min(maxphase)			
c-- phasetiming                                        
      real sec(maxphase)			
c-- azimuth 
      character*3 azi(maxphase)
c-- velocity and SNR    
      character*4 vel(maxphase),snr(maxphase)	
c-- back-azimuth of P               
      real bazi
c-- phase identification                       
      character*3 phase(maxphase)		
      character*1 weight                                                        
c-- s file name and cards                                  
      character*80 sfile,cards(max_data)                                             
c-- number of cards in Sfile                               
      integer ncard				
c-- counters                                               
      integer i,j				
c-- index of P and S                                       
      integer kp, ks			
c-- dummy header to read                               
      character*10 dummy			
c-- quality and p/s measures              
      integer q(maxphase),ps(maxphase)		
c-- for indata
      integer nstat,nphas,nhead,nrecord,id
      character*1 evid,exp
c-- duration and period as char.               
      character*4 duration,period		
c-- amplitude as character                          
      character*6 amplitude			
c-- for epicenter location                          
      real SLAT,SLON,PTOLG,ELAT,ELON,DIST,TRAVT,SS
      integer DOY,HR,MN
c-- unit number of output file 'autopic.out'
      integer iout                                                              

c                                                                               
c------ initialize ---                                                          
c                                                                               
      do i = 1,maxphase                                                         
         q(i) = 0                                                               
         ps(i) = 0                                                              
         hour(i) = 0                                                            
         min(i) = 0                                                             
         sec(i) = 0.                                                            
         azi(i) = '   '                                                         
         vel(i) = '   '                                                         
         phase(i) = '   '                                                       
         phasestring = ' '                                                      
      enddo                                                                     
                                                                                
      nphase = 0                                                                
      amplitude = '      '                                                      
      period = '    '                                                           
      duration = '    '                                                         
      rewind( un, iostat=code )                 ! Rewind file.
      call sei code(fort$,code,un,b_flag)       ! Process the outcome.
c                                                                               
c------ now read file , first read 3 headers ----                               
c                                                                               
      do i = 1,3                                                                
      read(un,'(a)',iostat=code) dummy
      call sei code(fort$,code,un,b_flag)       ! Process outcome.
      enddo                                                                     
                                                                                
      j = 0                                                                     
10    j = j + 1                                                                 
      read(un,100,iostat=code)hour(j),min(j),sec(j),snr(j),
     +                        q(j),ps(j),azi(j),vel(j)                          
100   format(5x,i2,1x,i2,1x,f5.2,12x,a4,20x,i1,1x,i2,1x,a3,4x,a4)              
      call sei code(fort$,code,un,b_flag)       ! Process outcome.
c
      if( b_flag ) then                         ! End of file.
      continue                                  !
c
      else                                      ! Otherwise next record.
      goto 10                                   !                              
      end if                                    !
c                                                                               
c------ Now select phases to use                                                
c                                                                               
99    nphase = j - 1                                                            
      call select_phas(nphase,q,ps,azi,hour,min,sec,                            
     +                 maxphase,phase,kp,ks)                                                   
                                                                                
      do i = 1,nphase                                                           
       if (i.ne.kp) then    ! azumuth only for P ???
         vel(i) = '   '                                        
         azi(i) = '   '                                        
       else
c         read(vel(i),*)apvel
c          apvel=sei get real(vel(i),code)
          read(vel(i),'(f4.1)') apvel
         if (apvel.lt.apvmin) vel(i) = '   '
       endif
      enddo                                                                     
c                                                                               
c -------- Coda calculation                                                     
c                                                                               
      if(nphase .ge. 1)then                                                     
        call timsec(year,month,day,hour(1),min(1),sec(1),msec)                  
        idur = int(dur - msec)                                                  
      else                                                                      
        idur = 0                                                                
      endif                                                                     
c
c ------- Locate event if P-azimuth is available
c
c jhfix, kp becomes zero
      if(kp.gt.0) then  !jhfix
      if (azi(kp).ne.' '.and.ks.ne.0) then
c        read(azi(kp),*) bazi      
        read(azi(kp),'(f3.1)') bazi
        call timsec(year,month,day,hour(kp),min(kp),sec(kp),psec)                  
        call timsec(year,month,day,hour(ks),min(ks),sec(ks),ssec)                  
CJAB(BGS)Mar95        call timsec(year,month,day,hour(kp),min(ks),sec(ks),ssec)

        ptolg = ssec - psec
c
        if( ptolg .lt. 0.0 ) then                   !JAB(BGS)Mar95.
        continue                                    !JAB(BGS)Mar95.
        else                                        !JAB(BGS)Mar95.
        CALL LOCLOC (SLAT,SLON,BAZI,PTOLG,ELAT,ELON,DIST,TRAVT)                     
        msec = psec - travt
        CALL SECTIM (MSEC,YEAR,DOY,MONTH,DAY,HR,MN,SS)
        WRITE(HYPOHEAD,260) YEAR,MONTH,DAY,HR,MN,SS,ELAT,ELON,0.,'F'                   
 260    FORMAT(1X,I4,1X,2I2,1X,2I2,F5.1,F10.2,F8.2,F5.1,A1)                            
        write(hypohead(46:48),'(a3)')station(1:3)                               
c                                                                               
c------- Now write extra header with solution if it exists                      
c                                                                               
        write(iout,'(a)',iostat=code)hypohead   
        call sei code(fort$,code,iout,b_flag) ! Process the outcome.
        end if                                !JAB(BGS)Mar95.
      endif
      endif !jhfix                                                                     
c                                                                               
c ------- Read existing cards in S file                                         
c                                                                               
      call sei open( old$,                    ! Open file (stop on error).
     &               ' ',                     ! No prompt.
     &               sfile,                   ! This file.
     &               read1,                   ! On this unit.
     &               b_flag,                  ! File exists? (n/a).
     &               code )                   ! Condition (n/a).
c
        call indata
     *  (read1,nstat,nphas,nhead,nrecord,evid,exp,cards,id)      
        rewind read1
      ncard = nrecord                        
c                                                                               
c------Now write old cards to S file                                            
c                                                                               
333   do i = 1,ncard-1                                                          
      write(read1,'(a)',iostat=code)cards(i)                                    
      call sei code(fort$,code,read1,b_flag)  ! Process outcome.
      enddo                                                                     
c                                                                               
c------Now write new readings to S file                                         
c                                                                               
      do i = 1,nphase                                                           
        if(amp .ne. 0. .and. phase(i)(2:2) .eq. 'P')                            
     +              write(amplitude,'(f6.1)')amp                                
        if(per .ne. 0. .and. phase(i)(2:2) .eq. 'P')                            
     +              write(period,'(f4.1)')per                                   
        if(idur .ne. 0 .and. phase(i)(2:2) .eq. 'P')                            
     +              write(duration,'(i4)')idur                                  
c                                                                               
c     use q and P/S for weighting                                                   
c                                                                               
        if (phase(i)(2:2) .eq. 'P')then
          write(weight,'(i1)') q(i)-1
        else
          write(weight,'(i1)') q(i)
        endif
                                                                                
        if(phase(i) .ne. ' ')then                                
c          write(phasestring,200)station,comp,phase(i),weight,                   
c     +     hour(i),min(i),sec(i),                                               
c     +     duration,amplitude,period,azi(i),vel(i),snr(i)                              
          write(phasestring,200)station,comp,phase(i),weight,                   
     +     hour(i),min(i),sec(i),                                               
     +     duration,amplitude,period,azi(i),vel(i)
          if(azi(i) .ne. ' ')write(phasestring(50:51),'(a2)')'.0'               
          write(phasestring(10:10),'(a1)')'E'               
          write(phasestring(16:16),'(a1)')'A'               
          write(read1,'(a)',iostat=code)phasestring 
          call sei code(fort$,code,read1,b_flag)    ! Process outcome.
          write(iout,'(a)',iostat=code)phasestring                              
          call sei code(fort$,code,iout,b_flag)     ! Process outcome.
        endif                                                                   
        duration = ' '                                                          
        amplitude = ' '                                                         
        period = ' '                                                            
      enddo                                                                     
200   format(1x,a5,a2,1x,a3,2x,a1,3x,2(i2),1x,f5.2,1x,a4,1x,                 
     +           a6,1x,a4,1x,a3,3x,a4)
      write(read1,'(a)',iostat=code)'          '                               
      call sei code(fort$,code,read1,b_flag)        ! Process outcome.
      call sei close(close$,read1,code)             ! Close, stop on error.
c
c    Return to Caller...
c    ===================
c
9999  return
      end                                                                       
c
      subroutine select_phas(inphas,q,ps,azi,hr,min,sec,                        
     +        max,chphas,kp,ks)                                                       
c                                                                               
c     input: inphas 	Number of input phases                                     
c            q		Quality of the phase                                           
c            ps		PS selector of phase                                           
c            azi	Azimuth of phase                                               
c            hr,min,sec	Arrival time of phase                                   
c     output:                                                                   
c            chphas	Array of phases selected. Blanked phases assumed useless    
c            kp         selected P-phase
c            ks         selected S-phase
c                                                                               
      implicit none 
      integer max                                                               
      real*8 times(50)                                                          
      character*3 chphas(max)                                                   
      integer hr(max),min(max)                                                  
      real sec(max),diff                                                        
      character*3 azi(max)                                                      
      integer ps(max),q(max)                                                    
      integer inphas                                                            
      integer i,k,j,kp,ks,lq                                                       
      integer nq(4)                                                             
      logical strue,ptrue,firstp                                                
                                                                                
c                                                                               
c----- Initialize                                                               
c                                                                               
      kp = 0
      ks = 0
      do i = 1,max                                                              
        chphas(i) = '   '                                                       
      enddo                                                                     
                                                                                
      if(inphas .lt. 1)then                                                     
        write(*,*) ' No phases detected   '                                     
        return                                                                  
      endif                                                                     
c                                                                               
c----- Count number of phases in each qualit category                           
c                                                                               
      nq(1) = 0                                                                 
      nq(2) = 0                                                                 
      nq(3) = 0                                                                 
      nq(4) = 0                                                                 
                                                                                
      do 5 i = 1,inphas                                                         
        if(q(i) .eq. 1) nq(1) = nq(1) + 1                                       
        if(q(i) .eq. 2) nq(2) = nq(2) + 1                                       
        if(q(i) .eq. 3) nq(3) = nq(3) + 1                                       
        if(q(i) .eq. 4) nq(4) = nq(4) + 1                                       
 5    continue                                                                  
                                                                                
c                                                                               
c------ P or S arrival                                                          
c                                                                               
      do 10 i = 1,inphas                                                        
        if(q(i) .eq. 1)then                                                     
          if(ps(i) .lt. 3)then                                                  
            write(chphas(i)(2:2),'(a)') 'S'                                     
cjh            write(azi(i),'(a)') '   '                                           
          else                                                                  
           write(chphas(i)(2:2),'(a)') 'P'                                     
          endif                                                                 
        elseif(q(i) .le. 3)then                                                 
          if(ps(i) .lt. 3)then                                                  
            write(chphas(i)(2:2),'(a)') 'S'                                     
cjh            write(azi(i),'(a)') '   '                                           
          else                                                                  
            write(chphas(i)(2:2),'(a)') 'P'                                     
          endif                                                                 
        endif                                                                   
 10   continue                                                                  
                                                                                
c                                                                               
c----- Only keep the best phases                                                
c                                                                               
      do 12 i = 1,inphas                                                        
        if(q(i) .eq. 4)chphas(i) = '   '                                        
        if( ((nq(1)+nq(2)) .ge. 2) .and.                                        
     +               (q(i) .eq. 3) ) chphas(i) = '   '                          
12    continue                                                                  
                                                                                
c                                                                               
c------- Check times; cancel arrivals to close or too far apart                 
c                                                                               
        if(inphas .ge. 2) then                                                  
          do 20 k = 2,inphas                                                    
          call timsec(90,1,1,hr(k-1),min(k-1),sec(k-1),times(k-1))              
          call timsec(90,1,1,hr(k),min(k),sec(k),times(k))                      
          diff = real(times(k) - times(k-1))                                    
          if(diff .lt. 2.0)then                                                 
            if( q(k-1) .eq. 3)then                                              
              if(q(k) .eq. 3) chphas(k) = '   '                                 
              if(q(k) .lt. 3) chphas(k-1) = '   '                               
            endif                                                               
            if(q(k-1) .le. 2) chphas(k) = '   '                                 
          endif                                                                 
                                                                                
          if( diff .gt. 180.0 .and. chphas(k-1).ne.' ')then                      
               chphas(k) = '   '                                                
          endif                                                                 
20        continue                                                              
        endif                                                                   
                                                                                
c                                                                               
c------ Remove secondary phases with quality = 3                                
c                                                                               
      do 16 i=1,inphas                                                          
        if(q(i) .eq. 3 .and. chphas(i) .ne. ' ')then                            
          do 17 k =i+1,inphas                                                   
            if(q(k) .eq. 3)chphas(k)= '   '                                     
17        continue                                                              
        endif                                                                   
16    continue                                                                  
                                                                                
c                                                                               
c------ Now find non blank phases                                               
c                                                                               
      firstp = .false.                                                          
      ptrue = .false.                                                           
      strue = .false.                                                           
      do 40 i = 1,inphas                                                        
c                                                                               
c------- Make the first nonblank phase a 'P' phase                              
c                                                                               
            if(.not. firstp)then                                                
              if(chphas(i) .ne. ' ')then                                        
                 write(chphas(i)(2:2),'(a)') 'P'                                
                 firstp = .true.                                                
              endif                                                             
            endif                                                               
c                                                                               
c------- Secondary phases are called G phases                                   
c                                                                               
c            if(ptrue.and.chphas(i)(2:2) .eq. 'P')then                          
c                   write(chphas(i)(3:3),'(a)') 'G'                             
c            endif                                                              
c            if(strue.and.chphas(i)(2:2) .eq. 'P')then                          
c                   write(chphas(i)(3:3),'(a)') 'G'                             
c            endif                                                              
c            if(chphas(i)(2:2) .eq. 'P')ptrue = .true.                          
c            if(chphas(i)(2:2) .eq. 'S')strue = .true.                          
40    continue                                                                  
c                                                                               
c---- Now count the number of nonblank phases left                              
c                                                                               
      k = 0                                                                     
      do i =1,inphas                                                            
        if(chphas(i) .ne. ' ') k = k + 1                                        
        if(chphas(i)(2:2).eq.'P') kp=i  !cjh
      enddo                                                                     
c                                                                               
c---- Now remove intermediate phases such that only 2 phases are left           
c                                                                               
      if(k .ge. 2)then                                                          
        do i = 1,inphas                                                         
           if(chphas(i) .ne. ' ')then                                           
cjh           kp = i
             lq = 4                                                             
             do j = i+1,inphas                                                  
                if( (chphas(j) .ne. ' ') .and.                                  
     +              (q(j) .le. lq)      ) then                                  
                    lq = q(j)                                                   
                    ks = j                                                      
                endif                                                           
             enddo                                                              
             goto 123                                                           
           endif                                                                
        enddo                                                                   
123     continue                                                                
        do j = i+1,inphas                                                       
          if(chphas(j) .ne. ' ' .and. j .ne. ks)then                            
              chphas(j) = '   '                                                 
          endif                                                                 
        enddo                                                                   
        if(chphas(ks)(2:2) .ne. 'S') chphas(ks)(2:2)='S'
      endif                                                                     
                                                                                
      return                                                                    
      end                                                                       
                                                                                
      SUBROUTINE REDUCE(DOY,HOUR,MIN,SEC,COMP,STA,LTA,SNR,                      
     +  COH,AZI,VEL,NDET,DUR,FREQ,THRSH1,THRSH2,COHMIN,NDMIN,                   
     +  QL1,QL2,QL3,QL4,IOUT)                                                   
      INTEGER DOY(200), HOUR(200), MIN(200)                                     
      REAL SEC(200), STA(200), LTA(200), SNR(200),                              
     +     COH(200), AZI(200), VEL(200)                                         
      CHARACTER*1 COMP(200)                                                     
      REAL WEI(200)                                                             
                                                                                
      RAD = 3.1415926/180.                                                      
                                                                                
      SNRMAX = 0.                                                               
      STAMAX = 0.                                                               
      ND1 = 0                                                                   
      ND2 = 0                                                                   
      NH = 0                                                                    
      NV = 0                                                                    
      NCOH = 0                                                                  
      MSNR = 0                                                                  
      AUE = 0.                                                                  
      AUN = 0.                                                                  
      SUMWEI = 0.                                                               
      DO 20 I=1,NDET                                                            
        IF (SNR(I).GT.THRSH1.AND.COH(I).GT.COHMIN) ND1 = ND1 + 1                
        IF (SNR(I).GT.THRSH2) ND2 = ND2 + 1                                     
        IF (COMP(I).EQ.'N'.OR.COMP(I).EQ.'E') THEN                              
          NH = NH + 1                                                           
        ELSE IF (COMP(I).EQ.'Z') THEN                                           
          NV = NV + 1                                                           
        ENDIF                                                                   
        IF (COH(I).GT.COHMIN) THEN                                              
          NCOH = NCOH + 1                                                       
          WEI(I) = SQRT(STA(I))*COH(I)                                          
        ELSE                                                                    
          WEI(I) = 0.0
        ENDIF
        IF (SNR(I).GT.SNRMAX) THEN
          SNRMAX = SNR(I)
          MSNR = I
          NCOHP = NCOH
        ENDIF
        IF (STA(I).GT.STAMAX) STAMAX = STA(I)
        IF (I.EQ.20) NC20 = NCOH
   20 CONTINUE
 
      CALL WAZIVE(WEI,AZI,VEL,NDET,AZIM,VELO,SDA,SDV)                           
                                                                                
      IF (SNRMAX.GT.99.9) SNRMAX = 99.9                                         
      IF (STAMAX.GT.99999) STAMAX = 99999                                       
      IF (SDA.GT.99) SDA = 99                                                   
      IF (SDV.GT.9.9) SDV = 9.9     
      IF (VELO.GT.99) VELO = 99
      IF (ND1+ND2.GE.NDMIN) THEN                                                
                                                                                
      IPS = 10*(FLOAT(NV)/FLOAT(NV+NH)+2*FLOAT(NCOH)/FLOAT(NV+NH))/3.           
      IF(NCOH .EQ. 0)IPS = 0                                                    
                                                                                
      QUAL = 0.0                                                                
      DO 55 I=1,NDET                                                            
   55 QUAL = QUAL + (SNR(I)-1.0)*(1.0+COH(I))                                   
                                                                                
      IF (QUAL.GE.QL1) THEN                                                     
        IQUAL = 1                                                               
      ELSE IF (QUAL.GE.QL2) THEN                                                
        IQUAL = 2                                                               
      ELSE IF (QUAL.GE.QL3) THEN                                                
        IQUAL = 3                                                               
      ELSE IF (QUAL.GE.QL4) THEN                                                
        IQUAL = 4                                                               
      ELSE                                                                      
        IQUAL = 5                                                               
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
      IAZI = INT(AZIM+0.5)                                                      
      ISDA = INT(SDA+0.5)     
      if(isda .gt. 99)isda = 99
      if(qual .gt. 9999.9) qual = 9999.9
      WRITE(IOUT,200)DOY(1),HOUR(1),MIN(1),SEC(1),DUR,FREQ,SNRMAX,              
     +               INT(STAMAX),NDET,NH,NV,NCOH,IQUAL,IPS,                     
     +               IAZI,ISDA,VELO,SDV,QUAL  
c DEBUG WCC next
c      WRITE(*,200)DOY(1),HOUR(1),MIN(1),SEC(1),DUR,FREQ,SNRMAX,              
c     +               INT(STAMAX),NDET,NH,NV,NCOH,IQUAL,IPS,                     
c     +               IAZI,ISDA,VELO,SDV,QUAL  
      ENDIF                                                                     
  200 FORMAT(1X,3I3,2F6.2,2F5.1,I6,6I3,I4,I3,F5.1,F4.1,F6.1)                    
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GMTEPO( YEAR, GMT , ITIME , EPOTIM )                           
                                                                                
      INTEGER*4  YEAR   , GMT(5), ITIME                                         
      REAL*8     EPOTIM                                                         
                                                                                
*====================================================================*          
*                                                                    *          
*     Conversion between human time and CSS epoch time.              *          
*     Also give NORSAR IIRSPS binary time.                           *          
*     IIRSPS is integer deciseconds representing time within a year. *          
*     Day one 00:00:00 is ITIME =864000.                             *          
*     Epoch time is double prececision floating point seconds from   *          
*     day one 1970. (I.e. epoch time 0.000 is day one 00:00:00 1970. *          
*                                                                    *          
*====================================================================*          
C                                                                               
C >>>>> INPUT :                                                                 
C                                                                               
C     YEAR    : Year ( ...,1970,...,1987,...).                                  
C     GMT     : Day-of-year,hour,minutes,seconds,deciseconds.                   
C               GMT(1) = day-of-year (1,2,3,...,365,(366)).                     
C               GMT(2) = hours       (00,...,23).                               
C               GMT(3) = minutes     (00,...,59).                               
C               GMT(4) = seconds     (00,...,59).                               
C               GMT(5) = deciseconds (0,...,9).                                 
C               If all GMT's are zero, then ITIME and EPOTIM is zero.           
C                                                                               
C >>>>> OUTPUT:                                                                 
C                                                                               
C     EPOTIM  : Epoch time in seconds. Positive or negative values.             
C               (           0.000 = 1970,001,00,00,00,0 (day 1,1970).           
C     ITIME   : IIRSPS binary time.                                             
C               (864000,..., 316223999,(317087999)).  (ca. < 2**29)             
C                                                                               
*====================================================================*          
                                                                                
      INTEGER*4 DAYS                                                            
      REAL*8    SEC  , EPODAT                                                   
                                                                                
      EPOTIM  = 0.0D0                                                           
      ITIME   = 0                                                               
C                           Error on input if GMT's < 0                         
      IF( GMT(1).LT.0 .OR.                                                      
     +    GMT(2).LT.0 .OR.                                                      
     +    GMT(3).LT.0 .OR.                                                      
     +    GMT(4).LT.0 .OR.                                                      
     +    GMT(5).LT.0      ) RETURN                                             
                                                                                
C                           All GMT's = 0 are accepted as ITIME = 0             
      IF( GMT(1)+GMT(2)+GMT(3)+GMT(4)+GMT(5).EQ.0 ) RETURN                      
                                                                                
C                           Error on input if GMT(1) < 1                        
      IF( GMT(1).LT.1 ) RETURN                                                  
                                                                                
      ITIME = GMT(1)*864000+GMT(2)*36000+GMT(3)*600+GMT(4)*10+GMT(5)            
                                                                                
      DAYS   = 0                                                                
      IF( YEAR .GT.1970 ) THEN                                                  
          DO 100 IYEAR = 1970 , YEAR - 1                                        
             DAYS = DAYS+365                                                    
             IF( (IYEAR - (IYEAR/4)*4)    .EQ.0 .AND.                           
     +           (IYEAR - (IYEAR/100)*100).NE.0 .OR.                            
     +           (IYEAR - (IYEAR/400)*400).EQ.0        ) DAYS = DAYS+1          
100       CONTINUE                                                              
      END IF                                                                    
      IF( YEAR .LT.1970 ) THEN                                                  
          DO 200 IYEAR = YEAR , 1969                                            
             DAYS = DAYS-365                                                    
             IF( (IYEAR - (IYEAR/4)*4)    .EQ.0 .AND.                           
     +           (IYEAR - (IYEAR/100)*100).NE.0 .OR.                            
     +           (IYEAR - (IYEAR/400)*400).EQ.0        ) DAYS = DAYS-1          
200       CONTINUE                                                              
      END IF                                                                    
      DAYS   = DAYS + GMT(1) - 1                                                
      EPODAT = DAYS                                                             
      EPODAT = EPODAT*86400.0D0                                                 
      SEC    = GMT(2)*3600.0D0+GMT(3)*60.0D0+GMT(4)+GMT(5)/10.0D0               
      EPOTIM = EPODAT + SEC                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
                                                                                
      SUBROUTINE EPOGMT( EPOTIM, YEAR, GMT , ITIME , TIMCOR )                   
                                                                                
      REAL*8     EPOTIM                                                         
      INTEGER*4  YEAR   , GMT(5), ITIME                                         
      REAL*4     TIMCOR                                                         
                                                                                
*====================================================================*          
*                                                                    *          
*     Conversion between CSS epoch time and NORSAR IIRSPS binary     *          
*     time. Epoch time is double floating point seconds from 1970.   *          
*     Epoch time is double prececision floating point seconds from   *          
*     day one 1970. (I.e. epoch time 0.000 is day one 00:00:00 1970. *          
*     IIRSPS is integer deciseconds representing time within a year. *          
*     Day one 00:00:00 is ITIME =864000.                             *          
*                                                                    *          
*====================================================================*          
C                                                                               
C >>>>> INPUT :                                                                 
C                                                                               
C     EPOTIM  : Epoch time in seconds. Positive or negative values.             
C               (           0.000 = 1970,001,00,00,00,0 (day 1,1970).           
C                                                                               
C >>>>> OUTPUT:                                                                 
C                                                                               
C     YEAR    : Year ( ...,1970,...,1987,...).                                  
C     GMT     : Corresponding day-of-year,hour,minutes,seconds,decisec.         
C               GMT(1) = day-of-year (1,2,3,...,365,(366)).                     
C               GMT(2) = hours       (00,...,23).                               
C               GMT(3) = minutes     (00,...,59).                               
C               GMT(4) = seconds     (00,...,59).                               
C               GMT(5) = deciseconds (0,...,9).                                 
C     ITIME   : Corresponding IIRSPS binary time.                               
C               (864000,..., 316223999,(317087999)).  (ca. < 2**29)             
C     TIMCOR  : GMT will only be to the precesion of deciseconds.               
C               TIMCOR give the diffrence between input time and                
C               resulting time in seconds.                                      
C                                                                               
*====================================================================*          
                                                                                
      REAL*8   SEC                                                              
      INTEGER  DAYS , DIY                                                       
                                                                                
      DAYS     = EPOTIM/86400.0D0                                               
      IF( EPOTIM.LT.0 ) DAYS   = DAYS   - 1                                     
      GMT(2)   = EPOTIM/3600.0D0 - DAYS*24.0D0                                  
                                                                                
      GMT(3)   = EPOTIM/60.0D0   - DAYS*1440.0D0  - GMT(2)*60.0D0               
                                                                                
      SEC      = EPOTIM          - DAYS*86400.0D0 - GMT(2)*3600.0D0             
     +                           - GMT(3)*60.0D0                                
      GMT(4)   = SEC                                                            
      GMT(5)   = NINT(SEC*10) - GMT(4)*10                                       
                                                                                
      TIMCOR   = SEC - GMT(4) - GMT(5)/10.0                                     
                                                                                
      IF( GMT(5).GE.10 ) THEN                                                   
          GMT(4) = GMT(4) + 1                                                   
          GMT(5) = 0                                                            
          IF( GMT(4).GE.60 ) THEN                                               
              GMT(4) = 0                                                        
              GMT(3) = GMT(3) + 1                                               
              IF( GMT(3).GE.60 ) THEN                                           
                  GMT(3) = 0                                                    
                  GMT(2) = GMT(2) + 1                                           
                  IF( GMT(2).GE.24 ) THEN                                       
                      GMT(2) = 0                                                
                      GMT(1) = GMT(1) + 1                                       
                  END IF                                                        
              END IF                                                            
          END IF                                                                
      ELSE IF( GMT(5).LT. 0 ) THEN                                              
          GMT(4) = GMT(4) - 1                                                   
          GMT(5) = 0                                                            
          IF( GMT(4).LT. 0 ) THEN                                               
              GMT(4) = 0                                                        
              GMT(3) = GMT(3) - 1                                               
              IF( GMT(3).LT. 0 ) THEN                                           
                  GMT(3) = 0                                                    
                  GMT(2) = GMT(2) - 1                                           
                  IF( GMT(2).LT. 0 ) THEN                                       
                      GMT(2) = 0                                                
                      GMT(1) = GMT(1) - 1                                       
                  END IF                                                        
              END IF                                                            
          END IF                                                                
      END IF                                                                    
                                                                                
                                                                                
      IF( DAYS.GE.0 ) THEN                                                      
          IYEAR = 1970                                                          
100       DIY   = 365                                                           
          IF( (IYEAR - (IYEAR/4)*4)    .EQ.0 .AND.                              
     +        (IYEAR - (IYEAR/100)*100).NE.0 .OR.                               
     +        (IYEAR - (IYEAR/400)*400).EQ.0        ) DIY = 366                 
          IF(  DAYS.LT.DIY ) GO TO 200                                          
          IYEAR = IYEAR + 1                                                     
          DAYS  = DAYS  - DIY                                                   
          GOTO 100                                                              
200       CONTINUE                                                              
      ELSE                                                                      
          IYEAR = 1969                                                          
300       DIY   = 365                                                           
          IF( (IYEAR - (IYEAR/4)*4)    .EQ.0 .AND.                              
     +        (IYEAR - (IYEAR/100)*100).NE.0 .OR.                               
     +        (IYEAR - (IYEAR/400)*400).EQ.0        ) DIY = 366                 
          DAYS  = DAYS  + DIY                                                   
          IF(  DAYS.GE.0   ) GO TO 400                                          
          IYEAR = IYEAR - 1                                                     
          GOTO 300                                                              
400       CONTINUE                                                              
      END IF                                                                    
                                                                                
                                                                                
      YEAR   = IYEAR                                                            
      GMT(1) = DAYS+1                                                           
      ITIME  = GMT(1)*864000+GMT(2)*36000+GMT(3)*600+GMT(4)*10+GMT(5)           
                                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE WAZIVE(WEI,AZI,VEL,NUM,OAZ,OVE,SDA,SDV)                        
      REAL WEI(200), AZI(200), VEL(200)                                         
C                                                                               
C  S/R TO CALCULATE OPTIMUM AZIMUTH AND VELOCITY WITH ASSOCIATED                
C  STANDARD ERRORS.                                                             
C                                                                               
      RAD = 3.1415926/180.                                                      
                                                                                
      AUE = 0.                                                                  
      AUN = 0.                                                                  
      SUMWEI = 0.                                                               
      NC = 0                                                                    
      DO 10 I=1,NUM                                                             
        IF (WEI(I).GT.0) THEN                                                   
          NC = NC + 1                                                           
          SLOW = 1./VEL(I)                                                      
          AZIR = AZI(I)*RAD                                                     
          UE = SLOW*SIN(AZIR)                                                   
          UN = SLOW*COS(AZIR)                                                   
          AUE = AUE + UE*WEI(I)                                                 
          AUN = AUN + UN*WEI(I)                                                 
          SUMWEI = SUMWEI + WEI(I)                                              
        ENDIF                                                                   
   10 CONTINUE                                                                  
                                                                                
      IF (SUMWEI.GT.0) THEN                                                     
        AUE = AUE/SUMWEI                                                        
        AUN = AUN/SUMWEI                                                        
        SLO = SQRT(AUE**2+AUN**2)                                               
        OVE = 1./SLO                                                            
        OAZ = ATAN2(AUE,AUN)/RAD                                                
        IF (OAZ.LT.0) OAZ = OAZ + 360.                                          
      ELSE                                                                      
        OAZ = -1.5                                                              
        OVE = 0.0                                                               
        SDA = 0.                                                                
        SDV = 0.                                                                
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
      IF (NC.EQ.1) THEN                                                         
        SDA = 0.                                                                
        SDV = 0.                                                                
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
      XX = 0.                                                                   
      YY = 0.                                                                   
      XY = 0.                                                                   
      SUMWE2 = 0.                                                               
      DO 20 I=1,NUM                                                             
        IF (WEI(I).GT.0) THEN                                                   
          SLOW = 1./VEL(I)                                                      
          AZIR = AZI(I)*RAD                                                     
          UE = SLOW*SIN(AZIR)                                                   
          UN = SLOW*COS(AZIR)                                                   
          XX = XX + ((UE-AUE)*WEI(I))**2                                        
          YY = YY + ((UN-AUN)*WEI(I))**2                                        
          XY = XY + ((UE-AUE)*WEI(I))*((UN-AUN)*WEI(I))                         
          SUMWE2 = SUMWE2 + WEI(I)**2                                           
        ENDIF                                                                   
   20 CONTINUE                                                                  
      XX = XX/SUMWE2                                                            
      YY = YY/SUMWE2                                                            
      XY = XY/SUMWE2                                                            
                                                                                
      DET = (XX-YY)**2 + 4*XY**2                                                
      IF (DET.GT.0) THEN                                                        
        SQR2 = SQRT(2.)                                                         
        RDT = SQRT(DET)                                                         
        R1 = (XX+YY+RDT)/2                                                      
        H1 = SQRT(R1)
        ttt = SQR2*RDT*SQRT(1+(XX-YY)/RDT)
        if(ttt .le. 1.0e-10)ttt = 1.0e-10
        A1 = H1/ttt
        X1 = A1*(XX - YY + RDT)                                                 
        Y1 = A1*2*XY                                                            
        R2 = (XX+YY-RDT)/2                                                      
        IF (R2.LT.0) R2 = 0.                                                    
        H2 = SQRT(R2)                                                           
        ttt = SQR2*RDT*SQRT(1-(XX-YY)/RDT)
        if(ttt .le. 1.0e-10)ttt = 1.0e-10
        A2 = H2/ttt
        X2 = A2*(XX - YY - RDT)                                                 
        Y2 = A2*2*XY                                                            
      ELSE                                                                      
        X1 = SQRT(XX)                                                           
        Y1 = SQRT(YY)                                                           
        H1 = X1                                                                 
        X2 = X1                                                                 
        Y2 = Y1                                                                 
        H2 = H1                                                                 
      ENDIF                                                                     
                                                                                
      IF (H1.NE.0) THEN                                                         
        COSU = ABS((AUE*X1 + AUN*Y1)/(SLO*H1))   
c WCC added to keep SINU from NAN
        IF (COSU.gt.1.0) COSU=1.0
        SINU = SQRT(1-COSU**2)                                                  
        SDV1 = COSU*H1/SLO**2                                                   
        SDA1 = SINU*H1/SLO                                                      
      ELSE                                                                      
        SDV1 = 0.0                                                              
        SDA1 = 0.0                                                              
      ENDIF                                                                     
                                                                                
      IF (H2.NE.0) THEN                                                         
        COSU = ABS((AUE*X2 + AUN*Y2)/(SLO*H2))                                  
c WCC added to keep SINU from NAN
        IF (COSU.gt.1.0) COSU=1.0
        SINU = SQRT(1-COSU**2)                                                  
        SDV2 = COSU*H2/SLO**2                                                   
        SDA2 = SINU*H2/SLO                                                      
      ELSE                                                                      
        SDV2 = 0.0                                                              
        SDA2 = 0.0                                                              
      ENDIF                                                                     
                                                                                
      SDV = SQRT(SDV1**2+SDV2**2)                                               
      SDA = SQRT(SDA1**2+SDA2**2)/RAD      
                                                                                
      RETURN                                                                    
      END                                                                       
      subroutine channel_def_2(number_of_channels,station,comp,                 
     +                             channelname,nchan,comp3)                     
c                                                                               
C     Defaults selection of stations                                            
c                                                                               
c     written by C. Lindholm and J. Havskov                                     
c     modified for use in 3 component analysis                                  
c                                                                               
c     Input:   number_of_channels                                               
c              station                                                          
c              comp                                                             
c              comp3                                                            
c     Output:  channelname                                                      
c              nchan                                                            
c
C
C    SEISAN library/JAB inclusions...
c    --------------------------------
C
      implicit none
      include 'libsei.inc'                 ! Library definitions & data defns.
      include 'seidim.inc'                 ! dimentions
      external sei get file,               ! Find & open file.
     &         sei close,                  ! & closure.
     &         sei code                    ! Error condition handler.
      integer  code,                       ! Condition.
     &         text_c,                     ! Text length.
     &         read1                       ! Read unit1.
      parameter (text_c = 80)              ! & value.
c
      logical  b_flag                      ! Flag!!
      character chr_text   *(text_c)       ! Text string.
C
C    ------- End of details -------
c
c-- input information                    
      character*5 station(max_trace)
      character*4 comp(max_trace)			
c-- defaults from file                       
      character*5 stat(max_trace)
      character*5 com(max_trace)			
c-- path to seismo                             
      integer number_of_channels                                                
c-- channels selected                            
      integer channelname(max_trace)				
c-- number of ch. selected                                
      integer nchan					
c-- Counters                                              
      integer i,j,k					
c-- Array of flags for 3 comp (y/n)                  
      character*1 comp3(max_trace)		
c-- Same as comp3, but only used internal          
      character*1 incomp3(max_trace)		
      character*80 string                                                       
c                                                                               
c----- Initialize -----                                                         
c                                                                               
      do i = 1,max_trace                                                               
        comp3(i) = 'n'                                                          
        incomp3(i) = 'n'                                                        
      enddo                                                                     
      i = 0                                                                     
c                                                                               
c   open and read default file with stations and components to use              
c   --------------------------------------------------------------
c                                                                               
       chr_text = 'AUTOPIC.INP'             ! Filename.
       call sei get file( open$,             ! Find and open file.
     &                    read1,             ! On unit.
     &                    code,              ! Returned condition.
     &                    'DAT',             ! Alternative directory to search.
     &                    chr_text )         ! File to find & return pathname.
c
10    read(read1,'(a)',iostat=code)string                                       
      call sei code(fort$,code,read1,b_flag) ! Process outcome.
c
c    End of file...
c
      if( b_flag ) then                      ! End of file.
      	call sei close( close$, read1, code )  ! Close, stop on error.
      	nchan = i                              !
c
c    Read record...
c
      else if(string(1:1) .eq. '*') then     ! Valid record.
      	i = i + 1                              ! Increment record #.
      	read(string(2:10),'(a5,a4)')stat(i),com(i)                             
      	if(string(12:17) .eq. '3 comp')incomp3(i) = 'y'                           
      	go to 10                                                                  
c
c    Skip...
c
      else                                   ! Otherwise.
      	goto 10                                ! Next record.
      end if                                 !
c                                                                               
c   find which default channeLs are present                                     
c                                                                               
      k=0                                                                       
      do i=1,number_of_channels                                                 
         do j=1,nchan                                                           
            if(station(i).eq.stat(j).and.comp(i).eq.com(j)) then                
               k=k+1                                                            
               channelname(k)=i                                                 
               if(incomp3(j) .eq. 'y') comp3(k) = 'y'                           
            endif                                                               
         enddo                                                                  
      enddo                                                                     
      nchan = k                                                                 
      return                                                                    
      end                                                                       
                                                                                
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
                                                                                
      SUBROUTINE CODAZ(STAZ,LTAZ,CRAT,LWIN,DELTA,NSTEP,CEND)                    
      REAL STAZ(NSTEP), LTAZ                                                    
C-----------------------------------------------------------------------        
C                                                                               
C s/r to find the end of coda. The sta's are first smoothed with                
C a rectangular window covering LWIN sta's. Then the end of coda                
C is found when the smoothed sta's fall below CRAT*LTAZ. The search             
C is started from the end.                                                      
C                                                                               
C   input:                                                                      
C                                                                               
C     STAZ   - array of sta's                                                   
C     LTAZ   - lta (average of sta's in the beginning of records)               
C     CRAT   - ratio used to define end of coda                                 
C     LWIN   - number of sta's in the smoothing window                          
C     DELTA  - step length (sec) between sta's                                  
C     NSTEP  - length of sta array                                              
C                                                                               
C   output:                                                                     
C                                                                               
C     CEND   - end of coda (in sec from beginning of sta array)                 
C                                                                               
C-----------------------------------------------------------------------        
      REAL SSTA(6000)                                                           
                                                                                
C  smooth sta's                                                                 
                                                                                
      MWIN=LWIN/2+1                                                             
      SSTA(MWIN)=0.0                                                            
      DO 10 I=1,MWIN                                                            
 10   SSTA(MWIN)=SSTA(MWIN)+STAZ(I)                                             
      DO 20 I=MWIN+1,NSTEP-MWIN+1                                               
 20   SSTA(I)=SSTA(I-1)+STAZ(I+MWIN-1)-STAZ(I-MWIN)                             
                                                                                
C  find end of coda                                                             
                                                                                
      ALIM=CRAT*LTAZ*LWIN                                                       
      DO 30 I=NSTEP-MWIN+1,MWIN,-1                                              
        IF (SSTA(I).GT.ALIM) GO TO 40                                           
 30   CONTINUE                                                                  
      CEND=0.0                                                                  
      GO TO 90                                                                  
 40   CEND=I*DELTA                                                              
                                                                                
 90   continue                                                                  
      END                                                                       
                                                                                
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
                                                                                
      SUBROUTINE CODAXYZ(STAX,STAY,STAZ,LTAX,LTAY,LTAZ,CRAT,LWIN,               
     +                   DELTA,NSTEP,CEND)                                      
C-----------------------------------------------------------------------        
C                                                                               
C s/r to find the end of coda. The sta's are first smoothed with                
C a rectangular window covering LWIN sta's. Then the end of coda                
C is found when the smoothed sta's fall below CRAT*LTAZ. The search             
C is started from the end.                                                      
C                                                                               
C     NOTE: this is a 3-component version.                                      
C                                                                               
C   input:                                                                      
C                                                                               
C     STAX   - array of sta's (x-comp)                                          
C     STAY   - array of sta's (y-comp)                                          
C     STAZ   - array of sta's (z-comp)                                          
C     LTAX   - lta on x-comp                                                    
C     LTAY   - lta on y-comp                                                    
C     LTAZ   - lta on z-comp                                                    
C     CRAT   - ratio used to define end of coda                                 
C     LWIN   - number of sta's in the smoothing window                          
C     DELTA  - step length (sec) between sta's                                  
C     NSTEP  - length of sta array                                              
C                                                                               
C   output:                                                                     
C                                                                               
C     CEND   - end of coda (in sec from beginning of sta array)                 
C                                                                               
C-----------------------------------------------------------------------        
      REAL SSTA(6000)                                                           
      REAL STAX(*),STAY(*),STAZ(*)                                              
      REAL LTAX,LTAY,LTAZ                                                       
                                                                                
C  smooth sta's                                                                 
                                                                                
      MWIN=LWIN/2+1                                                             
      SSTA(MWIN)=0.0                                                            
      DO 10 I=1,MWIN                                                            
 10   SSTA(MWIN)=SSTA(MWIN)+STAX(I)+STAY(I)+STAZ(I)                             
      DO 20 I=MWIN+1,NSTEP-MWIN+1                                               
 20   SSTA(I)=SSTA(I-1)+STAX(I+MWIN-1)-STAX(I-MWIN)                             
     +                 +STAY(I+MWIN-1)-STAY(I-MWIN)                             
     +                 +STAZ(I+MWIN-1)-STAZ(I-MWIN)                             
                                                                                
C  find end of coda                                                             
                                                                                
      ALIM=CRAT*(LTAX+LTAY+LTAZ)*LWIN                                           
      DO 30 I=NSTEP-MWIN+1,MWIN,-1                                              
        IF (SSTA(I).GT.ALIM) GO TO 40                                           
 30   CONTINUE                                                                  
      CEND=0.0                                                                  
      GO TO 90                                                                  
 40   CEND=I*DELTA                                                              
                                                                                
 90   RETURN                                                                    
      END                                                                       
                                                                                
