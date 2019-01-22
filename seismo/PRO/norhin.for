C                                                                               
C                       
C  Program to make a HYPOINVERSE file from a Nordic file 
c  new version for archive format
c
c  only type L events converted
c  only events with at least 3 stations      
c
c   originally all phase times were referred to same minute and p and s
c   were often on same line. for simplicity ps and s have different lines
c   and times are not referred to same minute

                          

c
c  2014 sep 25 jh: output format is now hypinverse archive format                                                                               
c
c
      implicit none
      CHARACTER*80 DATA(2500)                                                  
      CHARACTER*80 infile                                                     
      CHARACTER*1 TYPE,EXP                                                      
      integer nstat,nphase,nhead,nrecord
c---number of arguments 
      integer nars
c-- arguments
      character*80 args(10)
      integer n,id
c--compact or not
      logical compact


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c 
c   check if input from file given in argument
c
        call get_arguments(nars,args)
        if(nars.eq.0) then
           write(6,*)' You must give input file as argument'
           stop
        endif
c
c   case with output on standard output
c
        if(nars.gt.2) then                   !JAB(BGS)Mar95. Definitely too many
          write(6,*)' Too many arguments'
          stop
        endif
c
c   output file
c
        open(2,file='norhin.out',status='unknown')
c
c   get input file name
c
        infile=args(1)
        open(1,file=infile,status='old')
c
c   check that not a compact file
c
        call nortype(1,compact)
        if(compact) then
           write(6,*)' Input file is compact, cannot be used'
           stop
        endif
c
c   read and write to end of file
c
        n=0
  10    continue
c
c   read one event in nordic format
c
           CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
           if(nrecord.eq.0) goto 99 ! end of file
           if(id.le.1) then
             write(6,*)' No id for event ',data(1)(1:20)
             stop
           endif

           n=n+1
           if(nrecord.eq.0) goto 99
           if(type.ne.'L') goto 10
           if(nstat.lt.3) goto 10
c
c   convert to hypoinverse and write out 
c
           
           call norhin(2,nrecord,nhead,n,id,data)

        goto 10
c
 99     continue
        write(6,*)
c        write(6,*)' The input file had ',n,' events'
c        stop
        end
                       

c
c
      subroutine norhin(unit,nrecord,nhead,n,id,data)
	  
c
c   gets one event in nordic format in array data and writes it to unit
c   in hypoinverse archive format
c
      implicit none
      character*80 data(*)
      character*150 text
      character*1 phs    ! phase
      integer iphs       ! seconds*100 for phase
      integer nrecord,unit,nhead,id
      integer year,month,day,hour,min,doy
      real sec
      character*1 ns,ew    ! ns or ew
      real x             ! help variable
      double precision head_time,phase_time   ! abs times for header and phase
      integer k,i,n,idepth
c
c   write header to nearest minute
c
      text=' '
      write(text,'(a4,a4,a4)') data(1)(2:5),data(1)(7:10),
     *data(1)(12:15)
c
c   sec
c
      read(data(1)(17:20),'(f4.1)') sec
      i=sec*100
      write(text(13:16),'(i4)') i
c
c   location
c
      if(data(1)(25:30).ne.' ') then
c lat
          read(data(1)(24:30),'(f7.3)') x
          ns=' '
          if(x.lt.0.0) ns='S'
          if(x.lt.0.0) x=-x
          i=x
          write(text(17:18),'(i2)') i
          if(text(17:17).eq.' ') text(17:17)='0'
          text(19:19)=ns
          x=(x-i)*6000
          i=x
          write(text(20:23),'(i4)') i
c lon
          read(data(1)(31:38),'(f8.3)') x
          ew=' '
          if(x.gt.0.0) ew='E'
          if(x.lt.0.0) x=-x
          i=x
          write(text(24:26),'(i3)') i
          if(text(24:24).eq.' ') text(24:24)='0'
          if(text(25:25).eq.' ') text(25:25)='0'
          text(27:27)=ew
          x=(x-i)*6000
          i=x
          write(text(28:31),'(i4)') i
c  depth
          read(data(1)(39:43),'(f5.1)') x
          i=x*100
          write(text(32:36),'(i5)') i
          idepth=i
       endif      
c
c  event id
c
      text(137:146)=data(id)(65:74)

      write(unit,'(a)') text
c
c   read reference date
c
      sec=0.0
      read(data(1),'(1x,i4,1x,2i2)') 
     *year,month,day

c
c   read phases to end of event, skip if not P or S
c
      do k=nhead+1,nrecord-1
         phs=data(k)(11:11)
         if(phs.eq.'P'.or.phs.eq.'S') then
            read(data(k)(19:28),'(i2,i2,f6.1)') hour,min,sec
            call timsec(year,month,day,hour,min,sec,phase_time)
            call sectim(phase_time,year,doy,month,day,hour,min,sec)             
            text=' '
            text(1:5)=data(k)(2:6)
            write(text(18:34),'(i4,4i2,i5)') year,month,day,hour,min
            iphs=sec*100  
            
c
c   write p time seconds, different place than s-time
c
            if(phs.eq.'P') then
               write(text(30:34),'(i5)') iphs
               text(14:15)=data(k)(10:11)
               text(17:17)=data(k)(15:15)
               text(88:91)=data(k)(30:33)
            else
               write(text(42:46),'(i5)') iphs
               text(47:48)=data(k)(10:11)
               text(50:50)=data(k)(15:15)
            endif      
            text(16:16)=data(k)(17:17)
c
c  component in availabel fields
c
            text(87:87)=data(k)(7:7)
            text(109:109)=data(k)(8:8)    
            write(unit,'(a)') text
         endif
      enddo
c
c   write blank line or trial depth if fixed depth
c
      text=' '
      if(data(1)(44:44).eq.'F') then
         write(text(30:34),'(i5)') idepth
         text(35:35)='-'
      endif
c
c   check for explosion
c
      if(data(1)(23:23).eq.'E'.or.data(1)(23:23).eq.'P') then
c
c   seems that depth cannot be less than 100 m
c
         text(30:35)='00001-'
      endif
      write(unit,'(a)') text(1:50)
      return
      end
