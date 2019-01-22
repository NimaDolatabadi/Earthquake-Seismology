c
c  Driver program used in seisan for fpfit
c
c  J havskov, august 2010
c
c  changes:
c
c 20161202  jh: prepare for composite solution, write result in hyp.out, change
c               name to fpfit and origial program to fpfit_org
c 20161212  jh: always write in hyp.out
c
c

      implicit none
      include 'seidim.inc'                 ! array dimentions
      character*80 data(max_data)          ! one event
      integer nhead,nrecord,nphas,id,nstat ! for reading
      character*1 exp,type                 ! -----------
      character*80 text    ! input text
      character*80 t_out   ! output text
      integer nevent       ! number of events used
      real strike,dip,rake,fit,stdr,del_strike,del_dip,del_rake ! fps parameters
      integer i,k,ifoc

      nevent=1
c
c   make parameter file
c
      call make_fpfit_par

c
c   make run file
c
      open(33,file='fpfit.run',status='unknown')
      write(33,'(a)') 'fps'
      write(33,'(a)') 'sto'
      write(33,'(a)') '   '
      close (33)
c
c   convert seisan print.out to fpfit h71 print file used for input to fpfit
c
      open(33,file='print.out',status='old')
      open(34,file='fpfit.dat',status='unknown')
      i=0
c
c   find header line
c
 1    continue
      read(33,'(a)',end=99) text
      if(text(4:7).eq.'date') goto 2
      goto 1
 2    continue
      read(33,'(a)') text
      write(34,'(a,a)')
     *'  DATE    ORIGIN   LATITUDE',
     *' LONGITUDE  DEPTH    MAG NO           RMS'
c
      t_out=' '
      t_out(1:21)=text(1:21)
      if(text(27:27).eq.'N') t_out(22:22)='n'
      if(text(27:27).eq.'S') t_out(22:22)='s'
      t_out(23:27)=text(22:26)
      t_out(29:31)=text(29:31)
      if(text(37:37).eq.'E') t_out(32:32)='e'
      if(text(37:37).eq.'W') t_out(32:32)='w'
      t_out(33:37)=text(32:36)
      t_out(39:44)=text(38:43)                    ! depth
      t_out(49:51)='1.0'                          ! fix to 1.0
      t_out(52:54)=text(46:48)
      t_out(64:68)=text(53:57)                    ! rms
      write(34,'(a)') t_out
c
c   blank line
c
      write(34,'(a)') ' '
c
c  text
c
      write(34,'(a,a)')'  STN  DIST  AZ',
     *' TOA PRMK HRMN  PSEC TPOBS              PRES  PWT'
c
c  search input phase lines
c
 3    continue
      read(33,'(a)',end=99) text
      if(text(2:4).eq.'stn') goto 4
      goto 3
c
 4    continue
      read(33,'(a)',end=99) text
      if(text.ne.' ') then
         t_out=' '
         t_out(1:15)=text(1:15)
         t_out(17:20)=text(18:20)               ! ain
         if(text(26:26).eq.'P') then            ! only use P-phases
            t_out(22:22)=text(26:26)
         else
            go to 4
         endif
         t_out(23:23)=text(32:32)               ! pol, check if PP etc ?
         t_out(26:35)=text(42:51)
         t_out(54:59)=text(66:71)
         t_out(62:65)=text(73:76)
         t_out(36:41)=text(52:57)
         write(34,'(a)') t_out
         i=i+1
         goto 4
      else
         nevent=nevent+1
         goto 3        ! possibely add next event to composite solution
      endif 
c
c   end of file
c
 99   continue
      write(6,*)' Number of phases',i
      close(33)
      close(34)

      nevent=nevent-1
      write(6,'(a,i4)') 'Number of events used',nevent
c
c   run fpfit
c
      call systemc('fpfit_org<fpfit.run',19)

c
c  get new fault plane solution and print out 
c  put into hyp.out
c

      open(97,file='fpfit.fps',status='old',err=20)
      goto 21
 20   continue
c
c  if no output file, there is no solution
c
      goto 199
 21   continue
c
c  get parameters
c
      text=' '
c
c   read solution
c
      read(97,
     *'(83x,f3.0,1x,f2.0,f4.0,2x,f4.2,5x,f5.1,12x,f2.0,1x,f2.0,1x,f2.0)'
     *,err=199,end=199)
     *strike,dip,rake,fit,stdr,del_strike,del_dip,del_rake
      goto 100
 199  continue
        write(6,*)' No solution'
        stop
 100  continue
      write(6,*)
      write(6,'(a,1x,f6.3)')' Fit',fit
      write(6,'(a,1x,3f6.1)')' Errors in strike, dip and rake',
     *del_strike,del_dip,del_rake
      write(6,*)  
               

      if(nevent.gt.1) write(*,*)
     *'Following line is composite solution written to hyp.out'

c
c   in fpfit, strike is downdip azimuth so correct
c
      strike=strike-90.0
c   
c   write values
c
      text=' '
      write(text,'(3f10.1,3f5.1)')
     *strike,dip,rake,del_strike,del_dip,del_rake
      write(text(46:50),'(f5.1)') fit     ! fit
      write(text(51:55),'(f5.1)') stdr    ! station distribution ratio
      text(71:80)='FPFIT    F'
      write(6,'(a)') text

c
c   put into hyp.out, first event
c
                open(111,file='hyp.out',status='old')
                call indata(111,nstat,nphas,nhead,
     &          nrecord,type,exp,data,id)
                ifoc=0
                do i=2,nhead
c                  write(6,'(a)') data(i)
                   if(data(i)(71:80).eq.'FPFIT   F') then ! overwrite 
                     ifoc=1
                     data(i)=text
                     goto 1212     ! only first instance
                   endif
                enddo
c               write(6,*) ifoc
                if(ifoc.eq.0) then
                   do i=nrecord,nhead,-1
                      data(i+1)=data(i)
                   enddo
                   data(nhead)=text
                   nrecord=nrecord+1
                endif
 1212           continue
                open(112,file='hyp.temp',status='unknown')
                write(112,'(a80)')(data(i),i=1,nrecord)
c
c  read-write rest of hyp.out, there could be more events if composite
c  solution
c
                do k=2,nevent
                   call indata(111,nstat,nphas,nhead,
     &             nrecord,type,exp,data,id)
                   write(112,'(a80)')(data(i),i=1,nrecord)
                enddo
c
c   back to hyp.out
c
                rewind(112)
                rewind(111)

                do k=1,nevent
                   call indata(112,nstat,nphas,nhead,
     &             nrecord,type,exp,data,id)
                   write(111,'(a80)')(data(i),i=1,nrecord)
                enddo
                close(111)
                close(112,status='delete')
   


      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine make_fpfit_par
c
c   make sure there is no fpfit.fps
c
      open(97,file='fpfit.fps',status='unknown')
      close(97,status='delete')
c
c   make fpfit parmeter file, defualt name is fpfit.inp
c
      open(33,file='fpfit.inp',status='unknown')

      write(33,'(a)') "ttl   1 'none'"
      write(33,'(a)') "hyp 'fpfit.dat'"
      write(33,'(a)') "out 'fpfit.out'"
      write(33,'(a)') "sum 'fpfit.fps'"
      write(33,'(a)') "pol 'fpfit.pol'"
      write(33,'(a)') 'for   1'
      write(33,'(a)') 'mag   0.000'    
      write(33,'(a)') 'obs   6'
      write(33,'(a)') 'dis  0.1000E+06'
      write(33,'(a)') 'res   100.0'    
      write(33,'(a)') 'ain   0.000      180.0'    
      write(33,'(a)') 'amp   0'
      write(33,'(a)') 'bst   0'
      write(33,'(a)') 'fin   1'
      write(33,'(a)') 'rep   1'
      write(33,'(a)') 'cmp   0'
      write(33,'(a)') 'hdr  0.1000E-01 0.2000E-01 0.5000E-01 0.1000'    
      write(33,'(a)') 'mcr   1.000      1.000      1.000      1.000'   
      write(33,'(a)') 'dir   90.00      250.0      10.00      1.000'   
      write(33,'(a)') 'dip   0.000      90.00      10.00      1.000'    
      write(33,'(a)') 'rak  -180.0      180.0      20.00      1.000'    
      write(33,'(a)') "chn   1 'VHZ'"
      write(33,'(a)') "hds   1 '2'"
      close(33)
      return
      end

      