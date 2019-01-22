c****************************************************************
c Modified version of Ray Buland's global travel time generation
c program which runs on both PC's and SUN's
c must be linked with subroutines in tau.for

c reads the two random access files iasp91.hed & iasp91.tbl
c
c   updates
c   dec 29 94  : adopt to seisan
c   april 99   : -----------  verisopn 7.0 check --------------------
c                remove call to computer type
c   11/03 lot  : add call to iasp91_filename

      program ttim
      save
      parameter (max=100)
      logical log,prnt(3)
      character*8 phcd(max),phlst(10)
      character*80 fnam
      character*80 modnam,comp_modnam
      logical exist
      dimension tt(max),dtdd(max),dtdh(max),dddp(max),mn(max),ts(max)
      dimension xcor(max),tcor(max),usrc(2)
      integer seiclen
c
c
      character*1 dchar
      character*60 top_directory
      data inrec/30/,phlst(1)/'query'/,prnt(3)/.true./

 

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call dir_char(dchar)
      call topdir(top_directory)
      itp= index(top_directory,' ') -1

c
c   check for iaspei files
c
c      modnam='IASP91 '
      call iasp91_filename(comp_modnam)
      inquire(file=comp_modnam
     &   (:seiclen(comp_modnam))//'.TBL',
     &    exist=exist)
      if(exist) then
         modnam=comp_modnam(1:seiclen(comp_modnam))//' '
      else
         modnam=
     *top_directory(1:itp)//dchar//'DAT'//dchar//
     *   comp_modnam(1:seiclen(comp_modnam))//' '
      endif

c      inquire(file='IASP91.TBL',exist=exist)
c      if(exist) then
c         modnam='IASP91 '
c      else
c         modnam=
c     *top_directory(1:itp)//dchar//'DAT'//dchar//'IASP91 '
c      endif


c
      fnam='ttim1.lis'//char(0)
      call assign(10,2,fnam)
c     write(6,*) 'name of model'
c     read (5,fmt='(a)') ctnam
      call tabin(inrec,modnam)
      fnam='Do you want tau interpolation printed?'//char(0)
      call query(fnam,prnt(1))
      fnam='Do you want range summary printed?'//char(0)
      call query(fnam,prnt(2))
      call brnset(1,phlst,prnt)
c
 3    fnam='Source depth (km):'//char(0)
      call query(fnam,log)
      read(*,*)zs
      if(zs.lt.0.) go to 13
      call depset(zs,usrc)
 1    write(*,*)
      fnam='Enter delta:'//char(0)
      call query(fnam,log)
      read(*,*)delta
      if(delta.lt.0.) go to 3
      call trtm(delta,max,n,tt,dtdd,dtdh,dddp,phcd)
      if(n.le.0) go to 2
      do 4 i=1,n
c     if(iupcor(phcd(i)(1:1),dtdd(i),xcor(i),tcor(i)).le.0)
c    1 print *,'iupcor failed on phase ',phcd(i)
      xcor(i)=0.
      tcor(i)=0.
c     if(phcd(i)(1.eq.'Pg'.or.phcd(i).eq.'Pb'.or.phcd(i).eq.'Pn'.or.
c    1 phcd(i).eq.'P') ierr=iupcor('P',dtdd(i),xcor(i),tcor(i))
c     if(phcd(i).eq.'Sg'.or.phcd(i).eq.'Sb'.or.phcd(i).eq.'Sn'.or.
c    1 phcd(i).eq.'S') ierr=iupcor('S',dtdd(i),xcor(i),tcor(i))
      mn(i)=tt(i)/60.
      ts(i)=.01*int(100.*(tt(i)-mn(i)*60.)+.5)
      if(ts(i).lt.60.) go to 4
      mn(i)=mn(i)+1
      ts(i)=ts(i)-60.
 4    continue
c
      do 5 i=1,n
      if(i.gt.1) go to 6
      write(*,100)delta,i,phcd(i),tt(i),mn(i),ts(i),dtdd(i),dtdh(i),
     1 dddp(i)
 100  format(/1x,f6.2,i5,2x,a,f9.2,i4,f7.2,1p3e11.2)
      go to 7
 6    write(*,102)i,phcd(i),tt(i),mn(i),ts(i),dtdd(i),dtdh(i),dddp(i)
 102  format(7x,i5,2x,a,f9.2,i4,f7.2,1p3e11.2)
 7    if(xcor(i).ne.0..or.tcor(i).ne.0.) write(*,103)tcor(i),xcor(i)
 103  format(22x,f9.2,f11.2)
 5    continue
      go to 1
 2    write(*,101)delta
 101  format(/1x,'No arrivals for delta =',f7.2)
      go to 1
 13   call retrns(inrec)
      call retrns(10)
      call vexit(0)
      end
