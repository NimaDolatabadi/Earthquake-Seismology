c
c xclust: program to detect event clusters based on cross-correlation
c         as computed by corr, default input file is cc_pair.out
c
c program does the following:
c         - read data
c         - sort event pairs with descending correlation
c         - find groups, loop over pairs
c            + start with highest correlation
c            + in a loop over all pairs until no more events 
c              added to group, add events that are linked
c              into group 
c            + continue to find next group
c
c Lars Ottemoller, April 2006
c
c changes:
c
c  23/05/06 lot - Changed reading of input data
c  18/01/11 lot - write out locations
c
 
      program xclust
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'version.inc'
      integer maxpair,maxe
      parameter (maxpair=50000)
      parameter (maxe=5000)
      
      character*19 sfile(maxe),text1,text2
      real lon(maxe),lat(maxe),dep(maxe)   ! coordinates
      integer sfileind(maxpair)
      integer nevent
      integer ind1(maxpair),ind2(maxpair)
      integer nchan(maxpair)
      integer group(maxe,maxe,2)
      logical grouped(maxpair),flag,added
      integer ngroup
      real cor(maxpair)
      integer c,i,j,k,l,ind,npair,tu
      character*80 line,infile,outfile,outgroup,outxyz
      real max
      logical tout
      real mincor
      real minlink
      real minchan
      real minpg
      logical sun,pc,linux
      character*80 system_call
      integer seiclen
      character*(80) datax(max_phase)
      integer code
c
c print version
c
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      outfile='xclust.out'
      call computer_type(sun,pc,linux)
c
c read parameter file
c
      inquire(file='xclust.par',exist=flag)
      if (.not.flag) then
        write(*,*) ' no such file: xclust.par '
        stop
      endif
      open(1,file='xclust.par',status='old')
      call read_xclust_def(1,mincor,minlink,minpg,minchan,tout)
      close(1)

c
c trace file
c
      if (tout) then
        tu=4
        open(tu,file='xclust.trace',status='unknown')
      endif
c
c remove index.??? files
c
      if (sun.or.linux) then
        system_call='rm index.???'
        system_call='rm xclustxyz.???'
      elseif (pc) then
        system_call='del index.???'
        system_call='del xclustxyz.???'
      endif
      call systemc(system_call,seiclen(system_call))

c
c read input file
c
      write(*,*) ' Filename of corr output file (e.g. cc_pairs.out) '
      read(5,'(a)') infile
      if (seiclen(infile).le.0) infile='cc_pairs.out'
      inquire(file=infile,exist=flag)
      if (.not.flag) then
        write(*,*) ' no such file: '//infile(1:seiclen(infile))
        stop
      endif
      if(tout)write(tu,'(a)') 
     &   'input file: '//infile(1:seiclen(infile))

      do i=1,maxpair
        grouped(i)=.false.
c        sfileind(ind1(c))=0
        sfileind(i)=0
      enddo

      open(1,file=infile,status='old')
      c=0
      nevent=0
      write(*,*) ' reading data ... '
 10   continue
      read(1,'(a)',end=999) line
      c=c+1
      if (index(line,'inf').gt.0) goto 10
      read(line,'(i4,1x,a19,1x,i4,1x,a19,1x,i3,1x,f5.3)',err=10) 
     &    ind1(c),text1,ind2(c),text2,nchan(c),cor(c)
c       write(*,*) ind1(c),' ',text1,' ',ind2(c),' ',
c     &    text2,' ',nchan(c),' ',cor(c)
c
c store s-file names if correlation is above minimum
c
      if (cor(c).ge.mincor.and.nchan(c).ge.minchan) then
        j=0
        do i=1,nevent
          if (sfile(i).eq.text1) then
            j=i
          endif
        enddo
        if (j.eq.0) then
          nevent=nevent+1
          lon(nevent)=0.
          lat(nevent)=0.
          dep(nevent)=0.
          sfile(nevent)=text1
          j=nevent
          if(tout)write(tu,*) ' new sfile ',nevent,' ',text1
          open(14,file=sfile(nevent),status='old')
          call rea_event_in(14,.true.,datax,code) 
          lon(nevent)=hyp_lon(1)
          lat(nevent)=hyp_lat(1)
          dep(nevent)=hyp_depth(1)
          close(14)
        endif
        if (sfileind(ind1(c)).eq.0) sfileind(ind1(c))=j
        j=0
        do i=1,nevent
          if (sfile(i).eq.text2) then
            j=i
          endif
        enddo
        if (j.eq.0) then
          nevent=nevent+1
          lon(nevent)=0.
          lat(nevent)=0.
          dep(nevent)=0.
          sfile(nevent)=text2
          j=nevent
          if(tout)write(tu,*) ' new sfile ',nevent,text2
c
c read location
c
          open(14,file=sfile(nevent),status='old')
          call rea_event_in(14,.true.,datax,code) 
          lon(nevent)=hyp_lon(1)
          lat(nevent)=hyp_lat(1)
          dep(nevent)=hyp_depth(1)
          close(14)
        endif
        if (sfileind(ind2(c)).eq.0) sfileind(ind2(c))=j
      else
c        write(tu,*) ' correlation low: ',text1,' ',text2,cor(c)
c
c don't use pair if correlation low
c
        c=c-1
      endif
      goto 10

 999  continue

      npair=c
      write(*,*) ' sorting data ... '
c
c sort arrays descending correlation
c
      do i=1,npair-1
c
c find pair with maximum correlation
c
        max=0.
        ind=0
        do j=i+1,npair
          if (cor(j).gt.max) then
            ind=j
            max=cor(j)
          endif
        enddo
c
c swap
c
        if (ind.gt.0) then
c          write(*,*) ' swap ',ind,i,max
          ind1(npair+1)=ind1(i)
          ind2(npair+1)=ind2(i)
          nchan(npair+1)=nchan(i)
          cor(npair+1)=cor(i)

          ind1(i)=ind1(ind)
          ind2(i)=ind2(ind)
          nchan(i)=nchan(ind)
          cor(i)=cor(ind)
          
          ind1(ind)=ind1(npair+1)
          ind2(ind)=ind2(npair+1)
          nchan(ind)=nchan(npair+1)
          cor(ind)=cor(npair+1)
        endif
      enddo
      if(tout)write(tu,*) 
     &   ' pairs: # event1 event2 nchan correlation '
      do i=1,npair
        if(tout)write(tu,*) i,ind1(i),ind2(i),nchan(i),cor(i)
      enddo

c
c build groups
c
      write(*,*) ' building groups ... '
      ngroup=0
      do i=1,npair
        if (.not.grouped(i)) then
c
c remove weak links
c
        if (ngroup.ge.2.and.minlink.gt.0.) then
          if(tout)write(tu,*) 
     &       ' checking for weak links, group ',ngroup
          do l=2,group(ngroup,1,1)+1
c            if(tout)write(tu,*) group(ngroup,l,2),minlink
            if (group(ngroup,l,2).lt.minlink) then
              if(tout)write(tu,*) 
     &        ' weak link ',ngroup,l,group(ngroup,l,2)
              group(ngroup,l,2)=0
              do k=1,npair
                if (group(ngroup,l,1).eq.ind1(k).or.
     &              group(ngroup,l,1).eq.ind2(k)) then
                  grouped(k)=.false. 
                endif
              enddo
            endif
          enddo
        endif
c
c new group
c
          ngroup=ngroup+1
          if(tout)write(tu,*) ' new group ',ngroup
          if(tout)write(tu,*) ' adding pair ',ind1(i),ind2(i)  
c
c first element gives count in group
c
          group(ngroup,1,1)=2
          group(ngroup,2,1)=ind1(i)
          group(ngroup,2,2)=1
          group(ngroup,3,1)=ind2(i)
          group(ngroup,3,2)=1
          grouped(i)=.true.
c
c search remaining pairs and merge into group
c
         added=.true.
         do while(added)
          added=.false.
c          write(*,*) ' still added'
          do j=1,npair
            if (.not.grouped(j)) then
ccc              do k=1,ngroup
                k=ngroup
                flag=.false.
                do l=2,group(k,1,1)+1
                  if (group(k,l,1).eq.ind1(j).or.
     &                group(k,l,1).eq.ind2(j)) then 
                    flag=.true.
                    if(tout)write(tu,*) ' adding pair ',ind1(j),ind2(j)
                  endif
                enddo
c
c add both ind1 and ind2, if pair belongs to group
c
                if (flag) then
                  added=.true.
                  grouped(j)=.true.
                  ind=0
                  do l=2,group(k,1,1)+1
                    if (group(k,l,1).eq.ind1(j)) then
                      group(k,l,2)=group(k,l,2)+1
                      ind=ind1(j) 
                    endif
                  enddo
                  if (ind.eq.0) then
                      group(k,1,1)=group(k,1,1)+1
                      group(k,group(k,1,1)+1,1)=ind1(j) 
                      group(k,group(k,1,1)+1,2)=1 
c             write(*,*) ind,ind1(j),group(k,1,1)
                  endif
                  ind=0
                  do l=2,group(k,1,1)+1
                    if (group(k,l,1).eq.ind2(j)) then
                      group(k,l,2)=group(k,l,2)+1
                      ind=ind2(j)
                    endif
                  enddo
                  if (ind.eq.0) then
                      group(k,1,1)=group(k,1,1)+1
                      group(k,group(k,1,1)+1,1)=ind2(j)
                      group(k,group(k,1,1)+1,2)=1
c             write(*,*) ind,ind2(j),group(k,1,1)
                  endif
                endif
ccc              enddo          
            endif     
          enddo 
         enddo
        endif
      enddo

c
c output
c
      open(2,file=outfile,status='unknown')
      k=0
      do i=1,ngroup
c
c only groups with more than minpg events
c
        if (group(i,1,1).ge.minpg) then
          k=k+1
c
c group index file
c
          outgroup='index.out'
          outxyz='xclustxyz.out'
          write(outgroup(7:9),'(i3.3)') k
          write(outxyz(11:13),'(i3.3)') k
          open(3,file=outgroup,status='unknown')
          open(4,file=outxyz,status='unknown')

          write(2,'(a)') '============= '
          write(2,'(a,i5,a,i6)') ' group: ',k,
     &      ' number of events: ',group(i,1,1)
          write(2,'(a)') '------------- '
          write(2,'(a)') '  event links '
          write(2,'(a)') '------------- '
          do j=2,group(i,1,1)+1
            write(2,'(2x,i5,1x,i5)') group(i,j,1),group(i,j,2)
            if (group(i,j,2).gt.0.) then
              write(3,'(i5,2x,a)') j-1,sfile(sfileind(group(i,j,1))) 
c              write(4,'(a,1x,f8.3,1x,f8.3,1x,f7.3)') 
              write(4,'(a,1x,f8.3,1x,f8.3,1x,f7.3,1x,i5)') 
     &sfile(sfileind(group(i,j,1))),
     &lon(sfileind(group(i,j,1))),lat(sfileind(group(i,j,1))),
     &dep(sfileind(group(i,j,1))),group(i,j,1)
            endif
          enddo
          close(3)
          close(4)
        endif
      enddo

      close(1)
      close(2)
      if (tout) close(tu)
      write(*,*) ' total events: ',nevent
      write(*,*) ' total pairs:  ',npair
      write(*,*) ' total groups: ',k
      write(*,*) ' output file:   xclust.out '
      if(tout)write(*,*) ' trace file:    xclust.trace '
      write(*,*) ' index files:   index.??? '
      write(*,*) ' psxyx files:   xlcustxyz.??? '

      stop
      end



      subroutine read_xclust_def(unit,mincor,minlink,minpergroup,
     &    minchan,tout)
c
c read parameter file xclust.par
c
      implicit none
      character*80 line
      real x
      real mincor,minlink,minpergroup,minchan
      logical tout
      integer i,unit,seiclen
  
c
c init values
c
      mincor=0.
      minlink=0.
      minpergroup=0.
      tout=.false.
c
c read input
c
10    continue

      read(unit,'(a80)',end=20) line

c fill missing characters
      do i=seiclen(line)+1,80
        line(i:i)=' '
      enddo

      if (line(1:19).eq.'MINIMUM CORRELATION') then
        read(line(41:),*) x
        if (x.gt.0..and.x.le.1.) mincor=x
      elseif (line(1:12).eq.'MINIMUM LINK') then
        read(line(41:),*) x
c        if (x.ge.1.) minlink=x
      elseif (line(1:12).eq.'MINIMUM CHAN') then
        read(line(41:),*) x
        if (x.ge.1.) minchan=x
      elseif (line(1:16).eq.'MINIMUM PERGROUP') then
        read(line(41:),*) x
        if (x.ge.1.) minpergroup=x
      elseif (line(1:12).eq.'TRACE OUTPUT') then
        read(line(41:),*) x
        if (x.ge.1.) tout=.true.
      endif
      goto 10
20    continue
      return
      end


