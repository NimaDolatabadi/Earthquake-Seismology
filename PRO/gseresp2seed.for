      program gseresp2seed
c
c program to convert gse response files to dataless seed volumes using
c program gse2seed (written by Reinoud Sleeman)
c
c Lars Ottemoller, 24/01/2008
c
c changes
c
c   11/01/2018 lo set stop date and insert location code as auxid
c
      implicit none
      include 'libsei.inc'
      character*80 infile,outfile,seedfile
      character*80 question
      character*120 gseline,line(1000)
      integer read01,read02,code,in,seiclen,ind,write01,i,max
      integer j,k
      logical b_flag
      character*5 stat
      character*4 comp
      character*16 start_date,stop_date
      character*240 system_call
      real srate
      real lat,lon,elev,hang,vang
      character*80 sitename
      integer staunit,chaunit,resunit  ! output files for station,
c        channel and reponse, used for several gse files to merge into one file
c        for gse2seed
      
 10   continue
      question=' Filename or number, filenr.lis for all'
      call filename(question,infile)
      if(infile(1:3).eq.'EOF') goto 99
c
c open merged output files
c
      call sei open( unknown$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'gse.sta',           ! Filename.
     &                   staunit,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
      call sei open( unknown$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'gse.cha',           ! Filename.
     &                   chaunit,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
      call sei open( unknown$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'gse.res',           ! Filename.
     &                   resunit,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
c
c   check for one or many files
c
      in=0
      if(infile(1:10).eq.'filenr.lis') then
          call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   'filenr.lis',     ! Filename.
     &                   read01,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
         in=1
      endif

 20   continue
      if(in.eq.1) then
         read(read01,'(7x,a)') infile
         write(6,*)
         if(seiclen(infile).le.0) goto 90
      endif
c
c set outfile
c
      ind=seiclen(infile)
      if (infile(ind-3:ind).eq.'_GSE') then
        outfile=infile(1:ind-3)//'tmp'
        seedfile=infile(1:ind-3)//'SEED'
      else
        outfile=infile(1:ind)//'tmp'
        seedfile=infile(1:ind)//'SEED'
      endif
 
c
c read file to get station and component code, and start and end time
c
      call sei open( old$+stop$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   infile,           ! Filename.
     &                   read02,           ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.

      max=0
      start_date='          '
      stop_date='2029/12/31 23:59'
      stat='     '
      comp='    '
      sitename = ' '
 30   continue
      read(read02,'(a)',end=40) gseline
      max=max+1
      line(max)=gseline

      if (gseline(1:4).eq.'CAL2') then
        write(*,*) gseline(1:seiclen(gseline))
        read(gseline(6:10),'(a5)') stat
        read(gseline(12:14),'(a3)') comp
        write(line(max)(18:19),'(a2)') '00'   ! insert location code
        read(gseline(47:56),*) srate
        read(gseline(58:73),'(a16)') start_date
        if (seiclen(gseline).gt.74.and.gseline(75:75).ne.' ') then
          read(gseline(75:87),'(a16)') stop_date
        endif
        call stat_loc(stat,' ',lat,lon,elev)

        if (comp(3:3).eq.'Z'.or.comp(3:3).eq.'z') then
          hang=-1.
          vang=0.
        elseif (comp(3:3).eq.'N'.or.comp(3:3).eq.'n') then
          hang=0.
          vang=90.
        elseif (comp(3:3).eq.'E'.or.comp(3:3).eq.'e') then
          hang=90.
          vang=90.
        endif
      endif
c
c check for sitename
c(GSE2SEED_SITENAME Lerwick, Shetland, UK)
c     
      if (gseline(3:19).eq.'GSE2SEED_SITENAME') then
        sitename = gseline(21:seiclen(gseline)-1)
      endif

      goto 30
 40   continue
      call sei close( close$, read02, code )
c
c write out gse2seed input file
c
      call sei open( unknown$,       ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   outfile,          ! Filename.
     &                   write01,          ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition.
 
      do j=1,2
        k=write01
        if (j.eq.2) k=staunit
      write(k,'(a)') 'DATA_TYPE STATION GSE2.0'
      write(k,'(a)')
     &'Sta   Type  Latitude  Longitude    Elev   On Date   Off Date'
      write(k,'(a5,6x,f9.5,1x,f10.5,1x,f7.3,1x,a10,1x,a10,1x,a)')
     &     stat,lat,lon,elev/1000.,start_date(1:10),stop_date(1:10),
     &     sitename(1:seiclen(sitename))
      write(k,*) 
      enddo

      do j=1,2
        k=write01
        if (j.eq.2) k=chaunit
      write(k,'(a)') 'DATA_TYPE CHANNEL GSE2.0'
      write(k,'(a)') 'Sta  Chan Aux   Latitude  Longitude'//
     &'    Elev  Depth   Hang  Vang Sample_Rate Inst     '//
     &'On Date   Off Date'
      write(k,'(a5,1x,a3,3x,a2,1x,f9.5,1x,
     &     f10.5,1x,f7.3,9x,f5.1,1x,f5.1,1x,f11.6,9x,a10,1x,a10)')
     &     stat,comp,'00',lat,lon,elev/1000.,
     &     hang,vang,srate,start_date(1:10),stop_date(1:10)
      write(k,*)
      enddo

      do j=1,2
        k=write01
        if (j.eq.2) k=resunit
      write(k,'(a)') 'DATA_TYPE RESPONSE GSE2.0'
      do i=1,max
        write(k,'(a)') line(i)(1:seiclen(line(i)))
      enddo
      write(k,*)
      enddo
      write(write01,'(a)') 'DATA_TYPE WAVEFORM GSE2.0'
      call sei close( close$, write01, code )
      system_call = 'gse2seed -i '//outfile(1:seiclen(outfile))
     &    //' -o '//seedfile(1:seiclen(seedfile))
        write(*,*) system_call(1:seiclen(system_call))
        call systemc(system_call,seiclen(system_call))

      if (in.eq.1) goto 20

 90   continue
      if (in.eq.1) call sei close( close$, read01, code )
      goto 10

 99   continue
      call sei close( close$, staunit, code )
      call sei close( close$, chaunit, code )
      write(resunit,'(a)') 'DATA_TYPE WAVEFORM GSE2.0'
      call sei close( close$, resunit, code )
      system_call='cat gse.sta gse.cha gse.res > tmp.gse'
        call systemc(system_call,seiclen(system_call))
      system_call = 'gse2seed -i tmp.gse -o dataless.seed'
        write(*,*) system_call(1:seiclen(system_call))
        call systemc(system_call,seiclen(system_call))

      stop
      end


