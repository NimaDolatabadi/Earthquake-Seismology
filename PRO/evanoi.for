      program evanoi

c
c evaluate connoi output and compute pdf 
c
c Lars Ottemoller, 11/06/2009
c
c changes
c  09/04/2010 lo - check for inf
c  14/04/2010 lo - fixed mistakes made when doing gfortran changes
c
      implicit none

      include 'noise.inc'

      character*80 infile,outfile,outps,mode  ! file names
      integer max                  ! max of streams
      parameter (max=199)
      character*9 stream(max),st   ! stream names
      character*5 stations(max)    ! stations names
      character*9 streamout(max)   ! output stream names
      character*80 line            ! text
      integer ind                  ! stream index
      integer nstream              ! number of streams
      integer i,j,k,l              ! counter
      real x,y
      real logf(nf),dff,yy
      real db(ndb),ddb
      integer idata(max,nf,ndb)
      integer seiclen
      real p,maxdb(max,nf,2),maxp
      parameter (maxp=30.)
      character*160 outtext
      character*5 refstat
      integer refind
      integer year,month,day,hour,min,doy,isec,hh1,hh2
      real sec
      double precision msec_start(max),msec_stop(max),msec,msec1,msec2
      character*13 startt,stopt
      character*2 starthh, stophh
      character*14 time
      real duration
      real lat,lon,ele
      integer avmethod ! 1=LO 2=McNamara
      real ts,tc,tl
      character*5 temp

      nstream=0
      avmethod=1
      infile = ' '
      refstat = ' '

      write(*,*) ' Input filename (default connoi.out) '
      read(5,'(a)') infile
      write(*,*) ' start time (yyyymmddhhmmss) '
      read(5,'(a)') time
      if (seiclen(time).le.0) then
        msec1=0.
      else
        read(time,'(i4,5i2)') year,month,day,hour,min,isec
        sec=float(isec)
        call timsec(year,month,day,hour,min,sec,msec1)
      endif
      write(*,*) ' stop time (yyyymmddhhmmss) '
      read(5,'(a)') time
      if (seiclen(time).le.0) then
        msec2=0.
      else
        read(time,'(i4,5i2)') year,month,day,hour,min,isec
        sec=float(isec)
        call timsec(year,month,day,hour,min,sec,msec2)
      endif
      write(*,*) ' give time of day interval in hours (hh hh) '
      line=' '
      read(5,'(a)') line 
      if (seiclen(line).gt.0) then
       read(line(1:seiclen(line)),*) hh1,hh2
      else
        hh1=0
        hh2=0
      endif
      write(*,*) hh1,hh2
      write(*,*) ' Reference station '
      read(5,'(a)') refstat
c      write(*,*) ' Averaging method: 1=default 2=McNamara '
c      read(5,*) avmethod
c      if (avmethod.gt.2.or.avmethod.lt.1) stop
      avmethod=1
      if (seiclen(infile).le.0) infile='connoi.out'
       
      open(1,file=infile,status='unknown')
      open(2,file='evanoi.out',status='unknown')
      open(3,file='mode.out',status='unknown')
      open(4,file='diff.out',status='unknown')
      if (seiclen(refstat).le.0) write(4,*)  

c
c set output frequencies for average event spectra
c
      if (avmethod.eq.1) then
        dff=(alog10(f_high)-alog10(f_low))/float(nf-1)
      else

      endif
c using the same even log spacing for both methods
      do i=1,nf
        logf(i)=alog10(f_low)+dff*float(i-1)
      enddo
c
c set output range for db 
c
      ddb=(db_high-db_low)/float(ndb-1)
      do i=1,ndb
        db(i)=db_low+ddb*float(i-1)
      enddo
      do i=1,max
        do j=1,nf
          do k=1,ndb
            idata(i,j,k)=0
          enddo
        enddo
      enddo
      do i=1,max
        msec_start(i)=0.
        msec_stop(i)=0.
      enddo

10    continue
      read(1,'(a)',end=99) line
c      write(*,*) line
      if (seiclen(line).le.0) goto 10
      if (index(line,'inf').gt.0) goto 10
      if (index(line,'Inf').gt.0) goto 10
      if (index(line,'NaN').gt.0) goto 10
      read(line(1:9),'(a9)') st
      read(line(11:18),'(i4,2i2)') year,month,day
      read(line(20:23),'(2i2)') hour,min
      sec=0.
      call timsec(year,month,day,hour,min,sec,msec)
CWF   HHZ 20090601 0000 0.00   3600.00
c      write(*,*) ' debug ',line
      read(line(30:38),'(f9.2)') duration
      read(line(40:47),*) x
      read(line(49:57),*) y
      ind=0
      do i=1,nstream
        if (st.eq.stream(i)) ind=i
      enddo
      if (ind.eq.0) then
        nstream=nstream+1
        stream(nstream)=st
        stations(nstream)=st(1:5)
        streamout(nstream)=' '
        streamout(nstream)=stream(nstream)
        do i=7,9 
          if (streamout(nstream)(i:i).eq.' ') 
     &       streamout(nstream)(i:i)='_'
        enddo
        ind=nstream
      endif
      if (msec.lt.msec_start(ind).or.
     &    msec_start(ind).eq.0.) msec_start(ind)=msec
      if (msec+duration.gt.msec_stop(ind).or.
     &    msec_stop(ind).eq.0.) msec_stop(ind)=msec+duration
c      do i=1,nf
c        do j=1,ndb
c          if (alog10(x).gt.logf(i)-dff/2..and.
c     &        alog10(x).le.logf(i)+dff/2..and.
c     &        y.gt.db(j)-ddb/2..and.y.le.db(j)+ddb/2.) then
c            idata(ind,i,j)=idata(ind,i,j)+1
c          endif
c        enddo
c      enddo

c check if within time interval
      if (msec1.gt.0..and.msec.gt.msec1.and.
     &     msec2.gt.0..and.msec.le.msec2.or.
     &     msec1.eq.0..and.msec2.eq.0..or.
     &     msec1.eq.0..and.
     &     msec2.gt.0..and.msec.le.msec2.or.
     &     msec2.eq.0..and.
     &     msec1.gt.0..and.msec.gt.msec1) then
c
c check for time of day
c
c         if (hh1.eq.0.and.hh2.eq.0.or.
c     &       (hh2.gt.hh1.and.hour.ge.hh1.and.hour.lt.hh2).or.
c     &       (hh2.lt.hh1.and.(hour.ge.hh2.or.hour.lt.hh1))) then
c changed 31 jan 2014
         if (hh1.eq.0.and.hh2.eq.0.or.
     &       (hh2.gt.hh1.and.hour.ge.hh1.and.hour.lt.hh2).or.
     &       (hh2.lt.hh1.and.hour.ge.hh1).or.
     &       (hh2.lt.hh1.and.hour.lt.hh2)) then

c           write(25,*) hour,hh1,hh2,line

c
c work out f and db index 
c
           if (y.ne.0.) then
             j=int((y-db_low+ddb/2.)/ddb)+1
             if (avmethod.eq.1) then
               i=int((alog10(x)-alog10(f_low)+dff/2.)/dff)+1
c         write(*,*) ' x,y ',x,y
c         write(*,*) ' i,j ',ind,i,j,max,nf,ndb
               if (i.gt.0.and.j.gt.0.and.j.le.ndb)
     &         idata(ind,i,j)=idata(ind,i,j)+1
             elseif (avmethod.eq.2) then
c               k=1
c               do while (k.le.nf)
c                 tc=1./10.**logf(k)
c                 ts=tc/sqrt(2.)
c                 tl=2.*ts
c                 if (1./x.ge.ts.and.1./x.lt.tl.and.
c     &               alog10(1./x).ge.alog10(1./tc)-dff/2..and.
c     &               alog10(1./x).le.alog10(1./tc)+dff/2.) then
c                   if (j.gt.0.and.j.le.ndb)
c     &             idata(ind,k,j)=idata(ind,k,j)+1
c                 endif
c                 k=k+1
c               enddo
             endif
           endif
         endif
      endif

c 
c count data points
c

      goto 10
99    continue
      do i=1,nstream
        lat=0.
        lon=0.
        ele=0.
c        write(*,*) ' get location ',i,stations(i)
        call stat_loc(stations(i),' ',lat,lon,ele)
        do j=1,nf
          maxdb(i,j,1)=0.
          maxdb(i,j,2)=0.
        enddo
        do j=1,nf
          l=0
c count per frequency
          do k=1,ndb
            l=l+idata(i,j,k)
          enddo
          do k=1,ndb
            if (idata(i,j,k).ne.0) then
              p=float(idata(i,j,k))/float(l)*100.
              if (p.gt.max) p=30.   ! set max to 30.
c station coordinates          
              write(2,'(a9,1x,f8.5,1x,f8.2,1x,f6.2)') 
     &          streamout(i),
     &          logf(j),db(k),p
c select max db for each frequency
              if (p.gt.maxdb(i,j,1).and.
     &           p.le.100..and.db(k).le.0.and.
     &           db(k).ge.-200.) then
                maxdb(i,j,1)=p
                maxdb(i,j,2)=db(k)
              endif
            endif
          enddo
          if (maxdb(i,j,2).ne.0.)
     &      write(3,'(a9,1x,f8.5,1x,f8.2,1x,f8.3,1x,f8.3)') 
     &      streamout(i),
     &      logf(j),maxdb(i,j,2),lon,lat
        enddo
      enddo 
      do i=1,nstream
c find reference stream
        refind=0
        do j=1,nstream
          if (stream(j)(1:5).eq.refstat.and.
     &        stream(j)(9:9).eq.stream(i)(9:9)) refind=j
c     &        stream(j)(7:9).eq.stream(i)(7:9)) refind=j
        enddo
        do j=1,nf
          if (maxdb(i,j,2).ne.0..and.refind.ne.0)
     &       write(4,'(a9,1x,f8.5,1x,f8.2)') streamout(i),
     &       logf(j),maxdb(i,j,2)-maxdb(refind,j,2)
        enddo
      enddo 
      close(1)
      close(2)
      close(3)
      close(4)
c
c create plot scripts
c
      do i=1,nstream
        if (stream(i)(9:9).eq.'Z') then
          outfile=stream(i)//'.csh'
          write(temp,'(i2.2,a1,i2.2)') hh1,'-',hh2
	  outps=stream(i)//'_'//temp//'.eps'
	  mode=stream(i)//'_'//temp//'.mode'
          do k=1,9
            if (outfile(k:k).eq.' ') outfile(k:k)='_'
            if (outps(k:k).eq.' ') outps(k:k)='_'
	    if (mode(k:k).eq.' ') mode(k:k)='_'
          enddo
          open(1,file=outfile,status='unknown')
c settings
          outtext='gmtset PAPER_MEDIA a4+ PAGE_ORIENTATION portrait '
     &      //'HEADER_OFFSET -.5 ANNOT_FONT_SIZE 12 '
     &      //'HEADER_FONT_SIZE 12 LABEL_FONT_SIZE 12 '
     &      //'MEASURE_UNIT cm'
          write(1,'(a)') outtext(1:seiclen(outtext))
          outtext='rm -f g.cpt'
          write(1,'(a)') outtext(1:seiclen(outtext))
          outtext='makecpt -Crainbow -T0/15/1 > g.cpt'
          write(1,'(a)') outtext(1:seiclen(outtext))

c Z component
          outtext='grep "'//streamout(i)(1:9)//
     &      '" evanoi.out | awk '//"'"//'{print $3,$4,$5}'//"'"//
     &      ' >! data.xyz'
c          outtext='grep "'//streamout(i)(1:9)//
c     &      '" evanoi.out | awk "{print $3,$4,$5}" >! data.xyz'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='xyz2grd data.xyz -Gdata.grd '//
     &      '-R-2.7/1.7/-200/-50 -V -I.025/1.2'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='grdimage data.grd -R-2.3/1.7/-200/-50 '//
     &      '-JX-14/7 -Cg.cpt -Q255 -X2.5 -Y19.5 -K >! '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))
 
          outtext='grep "'//streamout(i)(1:9)//
     &      '" mode.out | awk '//"'"//'{print $3,$4}'//"'"//
     &      ' >! data.xy'
c          outtext='grep "'//streamout(i)(1:9)//
c     &      '" mode.out | awk "{print $3,$4}" >! data.xy'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy data.xy '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy petersen_low.out '//
     &      '-R -M -W6,0 -JX -K -O >> ' //
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy petersen_high.out '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))
 
          call sectim(msec_start(i),year,doy,month,day,
     &      hour,min,sec)
          startt=' '
          write(startt,'(i4.4,2i2.2,1x,i2.2,i2.2)')
     &      year,month,day,hour,min
          call sectim(msec_stop(i),year,doy,month,day,
     &      hour,min,sec)
          stopt=' '
          write(stopt,'(i4.4,2i2.2,1x,i2.2,i2.2)')
     &      year,month,day,hour,min
          starthh=' '
          write(starthh,'(i2.2)')
     &      hh1
          stophh=' '
          write(stophh,'(i2.2)')
     &      hh2

          outtext='psxy -R2e-2/200/-200/-50 -K -O '//
c     &      '-JX14l/7 -Bg1a1f3:.\"'//streamout(i)(1:9)//
     &      '-JX14l/7 -Bg1a1f3:."'//streamout(i)(1:9)//
     &      ' '//startt(1:13)//'-'//
     &      stopt(1:13)//' '//
     &      starthh (1:2)//'-'//stophh (1:2)//
     &      '":/g30f10a30:"PSD (dB)":SWen << END>> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='END'
          write(1,'(a)') outtext(1:seiclen(outtext))

c N component
          outtext='grep "'//streamout(i)(1:8)//'N'//
     &      '" evanoi.out | awk '//"'"//'{print $3,$4,$5}'//"'"//
     &      ' >! data.xyz'
c          outtext='grep "'//streamout(i)(1:8)//'N'//
c     &      '" evanoi.out | awk "{print $3,$4,$5}" >! data.xyz'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='xyz2grd data.xyz -Gdata.grd '//
     &      '-R-2.7/1.7/-200/-50 -V -I.025/1.2'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='grdimage data.grd -R-2.3/1.7/-200/-50 '//
     &      '-JX-14/7 -Cg.cpt -Q255 -Y-8.5 -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='grep "'//streamout(i)(1:8)//'N'//
     &      '" mode.out | awk '//"'"//'{print $3,$4}'//"'"//
     &      ' >! data.xy'
c          outtext='grep "'//streamout(i)(1:8)//'N'//
c     &      '" mode.out | awk "{print $3,$4}" >! data.xy'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy data.xy '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy petersen_low.out '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy petersen_high.out '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy -R2e-2/200/-200/-50 -K -O '//
     &      '-JX14l/7 -Bg1a1f3:."'//streamout(i)(1:8)//'N'//
     &      '":/g30f10a30:"PSD (dB)":SWen << END>> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='END'
          write(1,'(a)') outtext(1:seiclen(outtext))

c E component
          outtext='grep "'//streamout(i)(1:8)//'E'//
     &      '" evanoi.out | awk '//"'"//'{print $3,$4,$5}'//"'"//
     &      ' >! data.xyz'
c          outtext='grep "'//streamout(i)(1:8)//'E'//
c     &      '" evanoi.out | awk "{print $3,$4,$5}" >! data.xyz'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='xyz2grd data.xyz -Gdata.grd '//
     &      '-R-2.7/1.7/-200/-50 -V -I.025/1.2'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='grdimage data.grd -R-2.3/1.7/-200/-50 '//
     &      '-JX-14/7 -Cg.cpt -Q255 -Y-8.5 -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='grep "'//streamout(i)(1:8)//'E'//
     &      '" mode.out | awk '//"'"//'{print $3,$4}'//"'"//
     &      ' >! data.xy'
c          outtext='grep "'//streamout(i)(1:8)//'E'//
c     &      '" mode.out | awk "{print $3,$4}" >! data.xy'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy data.xy '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy petersen_low.out '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy petersen_high.out '//
     &      '-R -M -W6,0 -JX -K -O >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy -R2e-2/200/-200/-50 -K -O '//
     &      '-JX14l/7 -Bg1a1f3:."'//streamout(i)(1:8)//'E'//
     &      '":/g30f10a30:"PSD (dB)":SWen << END>> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='END'
          write(1,'(a)') outtext(1:seiclen(outtext))

	  outtext='psbasemap -R -K -O -JX -B:"Period (sec)":S >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))
	  
c          outtext='grep "'//streamout(i)(1:8)//'Z'//
c     &      '" diff.out | awk \'{print $3,$4}\' >! data.xy'
          outtext='grep "'//streamout(i)(1:8)//'Z'//
     &      '" diff.out | awk '//"'"//'{print $3,$4}'//"'"//
     &      ' >! data.xy'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy data.xy '//
     &      '-R-2.3/1.7/-50/50 -M -W6,0 '//
     &      '-JX-2.5/1.5 -X15.5 -Y14 -K -O -B0:.\"'//
     &      streamout(i)(1:seiclen(streamout(i)(1:5)))//'-'//
     &      refstat(1:seiclen(refstat))//'\":/25SWen >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='grep "'//streamout(i)(1:8)//'N'//
     &      '" diff.out | awk '//"'"//'{print $3,$4}'//"'"//
     6      ' >! data.xy'
c          outtext='grep "'//streamout(i)(1:8)//'N'//
c     &      '" diff.out | awk "{print $3,$4}" >! data.xy'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy data.xy '//
     &      '-R-2.3/1.7/-50/50 -M -W6,0 '//
     &      '-JX-2.5/1.5 -Y-2.5 -K -O -B0/25SWen >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='grep "'//streamout(i)(1:8)//'E'//
     &      '" diff.out | awk '//"'"//'{print $3,$4}'//"'"//
     &      ' >! data.xy'
c          outtext='grep "'//streamout(i)(1:8)//'E'//
c     &      '" diff.out | awk "{print $3,$4}" >! data.xy'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy data.xy '//
     &      '-R-2.3/1.7/-50/50 -M -W6,0 '//
     &      '-JX-2.5/1.5 -Y-2.5 -K -O -B0/25SWen >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psxy -R2e-2/200/-50/50 -K -O '//
     &      '-JX2.5l/1.5 -Ba1/25SWen << END>> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='END'
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='psscale -Cg.cpt -D0/0/6/1 -O '//
     &      '-B10:Probability: -Y-5 >> '//
     &      outps(1:seiclen(outps))
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='ps2pdf '//
     &      outps(1:seiclen(outps)) 
          write(1,'(a)') outtext(1:seiclen(outtext))

          outtext='mv -f mode.out '//
     &      mode(1:seiclen(mode))  
          write(1,'(a)') outtext(1:seiclen(outtext))

c not good to plot in automatic mode, lo
c          outtext='gs '//
c     &      outps(1:seiclen(outps)) 
c          write(1,'(a)') outtext(1:seiclen(outtext))

          close(1)
        endif
      enddo

      stop
      end
    
