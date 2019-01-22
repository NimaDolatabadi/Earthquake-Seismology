      program magstat
      implicit none
c changes
c
c  17 May 2012 lo: write out period to magstat6.out, write magnitude normalized amplitudes
c

      include 'hypparm.inc'
      include 'libsei.inc'
c      include 'seidim.inc'
      include 'rea.inc'
      character*80 line
      integer i,j,k,l
      integer seiclen
      integer code
      real value
      character*80 filename

      integer maxst
      parameter (maxst=20000)
      real mlsm,mlav,mlsd,tmlsd ! ml sum, average, standard deviation
      real oldml,defml
      real ml(maxst),hdist(maxst),amp(maxst),per(maxst)
      real mldef(maxst)
      integer tmln
      character*5 stat(maxst)
      character*5 statlist(maxst)
      real statres(maxst),res
      integer nstatlist

      character*5 stations(maxst)
      integer nstat
      real residuals(maxst,5000)
      integer nresiduals(maxst)
      real av,sd,x,loga0,loga0hu,loga0ba,loga0ki,loga0al
      integer ind
      real bb,corr,rms
      real avb
      integer nev
      double precision ot
      character*22 gmttime(maxst)

c
c set test parameter defaults
c
      call settest(test)
c
c read STATION0.HYP
c
      nstatlist=0
      avb=0.
      test(75)=1.
      test(76)=1.11
      test(77)=.00189
      test(79)=-2.09
      test(179)=9999.
      test(183)=9999.
      open(1,file='STATION0.HYP',status='old')
10    continue
      read(1,'(a)',end=20) line
      if (line(1:10).eq.'RESET TEST') then
        i=index(line,'(')+1
        j=index(line,')')-1
        read(line(i:j),*) l
        read(line(j+3:seiclen(line)),'(f9.4)') value
        if (l.ge.76.and.l.le.78) write(*,*) ' TEST PARAMETER ',l,value
        if (l.ge.176.and.l.le.179) write(*,*) ' TEST PARAMETER ',l,value
        if (l.ge.180.and.l.le.183) write(*,*) ' TEST PARAMETER ',l,value
        test(l)=value
      endif
c
c read stations
c
c      write(*,*) line
      if (line(11:11).eq.'.'.and.line(20:20).eq.'.') then
        nstatlist=nstatlist+1
        if (line(1:2).eq.'  ') then
          read(line(3:6),'(a4)') statlist(nstatlist)
        else
          read(line(2:6),'(a5)') statlist(nstatlist)
        endif
        statres(nstatlist)=0.
        if (seiclen(line).ge.38) 
     &    read(line(34:38),'(f5.2)') statres(nstatlist)
      endif
      goto 10
20    continue
      close(1)

c
c read datafile
c
      write(*,*) ' Filename of Nordic file '
      read(5,'(a)') filename
      open(1,file=filename,status='old')
      open(2,file='magstat1.out',status='unknown')
      open(3,file='magstat2.out',status='unknown')
      open(10,file='magstat3.out',status='unknown')
      open(11,file='magstat4.out',status='unknown')
      open(12,file='magstat5.out',status='unknown') ! dist,-log A0
      open(13,file='magstat6.out',status='unknown') ! dist,ML-log A
      open(14,file='magstat7.out',status='unknown') ! old ML - new ML
      open(15,file='magstat8.out',status='unknown') ! dist-travel time 
      open(16,file='magstat9.out',status='unknown') ! dist-amp corrected by mag and q
      open(17,file='magstat10.out',status='unknown') ! log A + (3-ML)
      open(18,file='magstat11.out',status='unknown') ! oldmag-newmag
     
      open(4,file='magstat.err',status='unknown')
c 
c write out -log A0
c
      do i=1,1500
        x=float(i)
        loga0hu=1.11*alog10(x)+0.00189*x-2.09   ! Hutton and Boore (1987)
        loga0ba=1.00*alog10(x)+0.00301*x-1.99   ! Bakun and Joyner
        loga0ki=1.55*alog10(x)-2.90   ! Kim NE US
        loga0al=0.91*alog10(x)+0.00087*x-1.68   ! Alsaker Norway
        if (x.le.test(179)) then
          write(12,*) x,test(76)*alog10(x)+test(77)*x+test(78),loga0hu,
     &      loga0ba,loga0ki,loga0al
        elseif (x.le.test(183)) then
          write(12,*) x,test(176)*alog10(x)+test(177)*x+test(178),
     &      loga0hu
        else
          write(12,*) x,test(180)*alog10(x)+test(181)*x+test(182),
     &      loga0hu
        endif
      enddo        
      nstat=0
      tmlsd=0.
      tmln=0
      nev=0
30    continue
      call rea_event_in(1,.true.,data,code)
      if (code.ne.0) goto 40
      write(*,*) data(1)
      k=0
      mlsm=0.
      nev=nev+1
c compute origin time in seconds
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     &   hyp_hour(1),hyp_min(1),hyp_sec(1),ot)
c store old ml
      oldml=-999.
      defml=0.
      do i=1,6
        if (hyp_mag_type(1,i).eq.'L'.and.oldml.eq.-999.) then
          oldml=hyp_mag(1,i)
        endif
      enddo

c
c loop over phases to read amplitudes
c
      do i=1,rea_nphase
c
c check for if station in STATION0.HYP and check for correction
c
        res=0.
        ind=0
        do j=1,nstatlist
            if (statlist(j).eq.rea_stat(i)) then
              res=statres(j)
              if (res.gt.3.) stop
              ind=j
            endif
        enddo
c        write(*,*) ' station correction ',ind,res
c
c only use stations that are in the STATION file
c
c        if ((rea_phase(i).eq.'AML'.or.rea_phase(i).eq.'AMPL'.or.
c     &       rea_phase(i).eq.'IAML'.or.
c     &       rea_phase(i).eq.'E'.or.
c     &       rea_phase(i).eq.'S')
c             .and.rea_dist(i).ge.0..and.
c take all phases, lo 17 May 2012
         if (rea_dist(i).ge.0..and.
     &    hyp_depth(1).ge.0..and.
     &    rea_amp(i).gt.0..and.ind.ne.0) then
          k=k+1 ! number of ml phases for this event
          hdist(k)=sqrt(rea_dist(i)**2+hyp_depth(1)**2) 
          stat(k)=rea_stat(i)
          mldef(k)=1.*alog10(rea_amp(i))+1.11*alog10(hdist(k))+
     &        .00189*hdist(k)-2.09
          if (hdist(k).le.test(179)) then
            ml(k)=test(75)*alog10(rea_amp(i))+test(76)*alog10(hdist(k))+
     &        test(77)*hdist(k)+test(78)
     &        +res
          elseif (hdist(k).le.test(183)) then 
            ml(k)=alog10(rea_amp(i))+test(176)*alog10(hdist(k))+
     &        test(177)*hdist(k)+test(178)
     &        +res
          else 
            ml(k)=alog10(rea_amp(i))+test(180)*alog10(hdist(k))+
     &        test(181)*hdist(k)+test(182)
     &        +res
          endif
          defml=defml+mldef(k)
          amp(k)=rea_amp(i)
          per(k)=rea_per(i)
          if (ml(k).gt.9.) then
            write(4,*) ' too high single Ml ',mlav
            write(4,*) data(1)
          endif
          mlsm=mlsm+ml(k)
          write(*,'(a,a,f9.1,a,f7.1,a,f4.1)') 
     &      ' STAT: '//rea_stat(i)//' COMP: '//rea_co(i),
     &      ' AML: ',rea_amp(i),' HDIST: ',hdist(k),' ML: ',ml(k)
c
c write out travel time
c
          write(*,*) rea_abs_time(i)
          write(15,*) hdist(k),rea_abs_time(i)-ot
          write(15,*) hdist(k),rea_abs_time(i)-ot
c
c store phase time in gmt xy format
c
          write(gmttime(k)(1:22),'(i4.4,"/",i2.2,"/",i2.2,"T",
     &   i2.2,":",i2.2,":",f5.2)')
     &      hyp_year(1),hyp_month(1),hyp_day(1),
     &      rea_hour(i),rea_min(i),rea_sec(i)
          do j=1,22
            if (gmttime(k)(j:j).eq.' ') gmttime(k)(j:j)='0'
          enddo


c magnitude normalized amplitude
          write(16,*) hdist(k),alog10(rea_amp(i))-ml(k)
     6      +test(77)*hdist(k)
        endif
      enddo
c
c average
c
      mlsd=0.
      mlav=0.
      tmln=tmln+k
      if (k.gt.0) then
        mlav=mlsm/float(k)
        write(14,'(f5.1,1x,f5.1)') hyp_mag(1,1),mlav
c
c compute standard deviation
c
        do i=1,k
          mlsd=mlsd+(ml(i)-mlav)**2
          tmlsd=tmlsd+(ml(i)-mlav)**2
        enddo
        mlsd=sqrt(mlsd/float(k))
        write(*,'(a,f5.2,a,f5.2)') ' ML = ',mlav,' +/- ',mlsd
          
        if (mlav.gt.9.) then
          write(4,*) ' too high average Ml ',mlav
          write(4,*) data(1)
        endif
c
c compute trend
c
        call lsqlin(k,hdist,ml,tt,bb,corr,rms)
        write(*,'(a,f7.5)') ' slope ml/dist = ',bb
c        avb=avb+abs(bb)
        avb=avb+bb
c 
c output
c
        write(2,'(f5.2,1x,f5.2)') mlav,mlsd

c comparison old-new ml
        defml=defml/float(k)
        write(18,'(3(f3.1,1x),a)') oldml,defml,mlav,rea_id_line(61:74)

        do i=1,k
          write(3,'(f7.1,1x,f5.2,1x,f5.2)') hdist(i),ml(i),ml(i)-mlav
          if (hdist(i).le.50..and.abs(ml(i)-mlav).gt..75) then
            write(66,*) data(1)
            write(66,*) stat(i),hdist(i),ml(i),mlav,ml(i)-mlav
          endif
          write(13,*) hdist(i),mlav-alog10(amp(i)),per(i)
          write(17,*) hdist(i),(3.-mlav)+alog10(amp(i))
c
c store residual for station
c
          write(11,'(a5,1x,f7.2,1x,f5.2,1x,f5.2,1x,f5.2,1x,a,1x,a)')
     &      stat(i),hdist(i),ml(i),mlav,ml(i)-mlav,gmttime(i),
     &      rea_id_line(61:74)
          ind=0
          do j=1,nstat
            if (stations(j).eq.stat(i)) ind=j
          enddo
          if (ind.eq.0) then
            nstat=nstat+1
            ind=nstat
            stations(ind)=stat(i)
            nresiduals(ind)=0
          endif
          if (ind.gt.500) then
            write(*,*) ' too many stations '
            stop
          endif
          nresiduals(ind)=nresiduals(ind)+1
          if (nresiduals(ind).gt.20000) then
            write(*,*) ' too many residuals ',nresiduals(ind)
            stop
          endif
          residuals(ind,nresiduals(ind))=
     &       residuals(ind,nresiduals(ind))+ml(i)-mlav
c          write(67,*) data(1)
c          write(67,*) ind,nresiduals(ind),residuals(ind,nresiduals(ind))
c     &       ,ml(i),mlav,k
        enddo
      endif

      write(*,*) ' going to next event ...'
      goto 30
40    continue
      close(1)
c
c overall sd
c
      tmlsd=sqrt(tmlsd/float(tmln))
      
      write(*,'(a,i7)')  ' number of events           = ',nev
      write(*,'(a,i7)')  ' number of amplitudes       = ',tmln
      write(*,'(a,f7.4)')' overall standard deviation = ',tmlsd
      write(*,'(a,f7.5)')' overall av slope ml/dist   = ',avb/float(nev)

      do i=1,nstat
        if (nresiduals(i).gt.0) then
          av=0.
          do j=1,nresiduals(i)
            av=av+residuals(i,j)
c            write(68,*) i,j,av,residuals(i,j)
          enddo
          av=av/float(nresiduals(i))
          sd=0.
          do j=1,nresiduals(i)
            sd=sd+(residuals(i,j)-av)**2
          enddo
          sd=sqrt(sd/float(nresiduals(i)))
          write(10,'(a5,1x,f5.2,1x,f5.2,1x,i6)') 
     &      stations(i),av,sd,nresiduals(i)
        endif
      enddo
      close(10)
      close(11)
      close(15)
      close(16)
      close(17)

      stop
      end

