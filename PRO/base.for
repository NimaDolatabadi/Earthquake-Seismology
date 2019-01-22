C$Debug
c
c  this program reads the seisan data base file content and prints
c  out relevant information
c
c
c  j havskov, march 1999
c
c  updates:
c  october 29, 99 jh: Better print out if many bases
c                     not case sensitive, always use upper case
c
      implicit none
      integer nevents                     ! number of events
      integer n_yr_base(150,100)          ! # of events pr year and data base 
      character*5 bases(100)              ! alll data base names
      character*6 out(100)                ! for output writing
      integer nbases                      ! number of bases in use
      character*5 base_wanted             ! base wanted
      logical events_ok                   ! true if data for one year      
      integer data_base(150,12,100)       ! the whole file
      integer nscreen                     ! number of screens to write out
      integer n1,n2                       ! counters
      integer i,k,l,m


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
c   read whole data base content file
c
      call get_all_bases(nbases,bases,data_base)
c
      open(3,file='base.out',status='unknown')
      write(6,*) ' Number of data bases', nbases
c
c
c   go through whole base and calculate number of events per year and base
c
       do i=1,150      ! years
         do l=1,nbases
            n_yr_base(i,l)=0
            do k=1,12     ! months
               nevents=data_base(i,k,l)      
               if(nevents.ne.0) then
c                 if(n_yr_base(i,l).eq.-1) nevents=nevents+1   ! do not add -1
                  n_yr_base(i,l)=n_yr_base(i,l)+nevents     ! sum events pr year
               endif
            enddo
         enddo
       enddo
c
c   print out yearly statistics, only print for years having data
c   in at least one of the data bases
c
      nscreen=(nbases-1)/12+1
      do m=1,nscreen
         n1=1+(m-1)*12
         n2=n1+11
         if(m.eq.nscreen) n2=nbases
         write(6,'(5x,12(1x,a5))') (bases(i),i=n1,n2)
         write(3,'(5x,12(1x,a5))') (bases(i),i=n1,n2)
         do i=1,150
           do l=n1,n2
             if(n_yr_base(i,l).ne.0) then
               do k=n1,n2
                  out(k)=' '
c                 if(n_yr_base(i,k).ne.-1)
                  if(n_yr_base(i,k).ne.0)
     *            write(out(k),'(i6)') n_yr_base(i,k)
               enddo     
               write(3,'(1x,i4,12a6)') i+1899,(out(k),k=n1,n2)
               write(6,'(1x,i4,12a6)') i+1899,(out(k),k=n1,n2)
               goto 50
             endif
           enddo
 50        continue
         enddo
         if(m.ne.nscreen) then
            write(6,*)' Return to continue'
            read(5,'(a)')out(1) 
         endif
      enddo
c
c   details of one data base
c
 60   continue
c
      write(6,*)
      write(6,*) ' More info on one base, give name, else return',
     *             ' to stop'
      read(5,'(a)') base_wanted
      if(base_wanted.ne.' ') then
c
c  find if available and which number
c
         do i=2,5
           if(base_wanted(i:i).eq.' ') base_wanted(i:i)='_'
         enddo
         l=0
         do i=1,nbases
           if(bases(i).eq.base_wanted) l=i
         enddo
         if(l.eq.0) then
            write(6,'(a,a)') ' Base ',base_wanted, ' not available'
            goto 60
         endif
c
c   header line
c
         write(6,'(9x,a,a)') 
     *   'Jan  Feb  Mar  Apr  May  Jun  ',
     *   'Jul  Aug  Sep  Oct  Nov  Dec'
         write(3,'(9x,a,a)') 
     *   'Jan  Feb  Mar  Apr  May  Jun  ',
     *   'Jul  Aug  Sep  Oct  Nov  Dec'
c
c   write statistics for base selected
c
          do i=1,150      ! years
            n_yr_base(i,l)=0
c
c   find if any events this year
c
            events_ok=.false.
            do k=1,12     ! months
               nevents=data_base(i,k,l)      
c              if(nevents.ne.-1) events_ok=.true.
               if(nevents.ne.0) events_ok=.true.
            enddo
c
c   if events, write out
c
            if(events_ok) then
               do k=1,12
                 out(k)=' '
                 if(data_base(i,k,l).ne.0)  
     *           write(out(k)(1:5),'(i5)') data_base(i,k,l)
               enddo
               write(6,'(1x,i4,2x,12a5)')i+1899,(out(k)(1:5),k=1,12)
               write(3,'(1x,i4,2x,12a5)')i+1899,(out(k)(1:5),k=1,12)
            endif
         enddo
c
c   back for another base
c
         goto 60
      endif
c
c     write(6,*)
      write(6,*)' Output file is base.out'
c
      stop
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine get_all_bases(nbases,base,data_base)
c
c     read whole data base log file
c     nbases is the number of bases
c     data_base is the whole log file
c
      implicit none
      include 'libsei.inc'
      character*60 top_directory ! seisan top directory name
      character*800 bases     ! all bases in one string
      character*8   bas(100)  ! -----------
      character*5 base(100)   ! individual bases
      integer nbases          ! number of bases
      integer data_base(150,12,100) ! whole data base, year,month,base
      character*80 base_file  ! data base log file
      integer number(100)     ! number of events
      character*400 cnumber   ! ---------------
      integer pointer         ! pointer to data abse
      integer code            ! error code
      integer log_unit        ! unit to read and write from
      logical b_old           ! logical if file is old
      integer i,k,l
c
      equivalence (bases,bas)
      equivalence (number,cnumber)
c
c
c   get location of log file
c
      call topdir(top_directory)
      i=index(top_directory,' ')-1

      base_file = top_directory(1:i)//'/REA/REA.LOG'
c

c
c   open data base log file
c
      chr_f_access$='direct'
      f_recl$=800
c
      call sei open(old$+warn$,           ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             base_file,             ! File name
     &             log_unit,              ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition
      if (code .ne. e_ok$) then
        write(6,*)' No data base log file'
        stop
      endif
c
c   read text part
c
      read(log_unit,rec=1) bases
c
c   find how many bases
c
      do i=1,100
        base(i)=bas(i)(1:5)
        if(base(i)(1:1).eq.' ') then
          nbases=i-1
          goto 2
        endif
      enddo
 2    continue
c
c
c   close to open with a new record length to read numbers
c
      call sei close(close$,log_unit,code)                       

      chr_f_access$='direct'
      f_recl$=400
      call sei open(old$+warn$,           ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             base_file,             ! File name
     &             log_unit,              ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition
c
c   read all
c
      do i= 1,150
        do k=1,12
          pointer=(i-1)*12+k+2
           read(log_unit,rec=pointer) number       ! read 100 numbers
           do l=1, nbases
             data_base(i,k,l)=number(l)
           enddo
        enddo
      enddo
c
      call sei close(close$,log_unit,code)                       
c
      return
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine get_base_data_year(base,n_yr)
c
c  find data for all years for base base   
c
      implicit none
      include 'libsei.inc'
      integer nevents
      character*5 base
      character*5 bases(100)
      integer n_yr(150)
      integer code            ! error code
      integer log_unit        ! unit to read and write from
      logical b_old           ! logical if file is old
      integer pointer         ! address of element with data base info
      integer ibase           ! number of the data base
      character*80 base_file  ! data base log fil name
      integer nbases          ! number of data bases
      integer i,k
c
c
      base_file='test.log'
c
c  get number of data bases
c
      call base_get_bases(nbases,bases)
      write(6,*)' Number of bases ',nbases
c
c  find the number of the data base to use
c
c
c  get number of data bases
c
      ibase=0
      do i=1,nbases
        if(base.eq.bases(i)) then
           ibase=i
           goto 1
        endif
      enddo
 1    continue
c
c   check if data base was registered in data base log file, if not, stop
c
      if(ibase.eq.0) then
         write(6,*)' Data base missing in log file'
         stop
      endif
c
c   open data base file 
c
       chr_f_access$='direct'
       f_recl$=4
       call sei open(old$+warn$,          ! Open a existing file.
     &             ' ',                   ! Prompt file name (n/a).
     &             base_file,             ! File name
     &             log_unit,              ! Read unit #1
     &             b_old,                 ! Already exists? (n/a).
     &             code)                  ! Returned condition
c
c   go through whole base
c
       do i=1,150      ! years
         n_yr(i)=0
         do k=1,12     ! months
c
c   calculate address to get info
c
            call base_address(i+1900-1,k,ibase,pointer)
c
c  read info
c
            read(log_unit,rec=pointer,err=99) nevents
            if(nevents.ne.0) then
c              if(n_yr(i).eq.-1) nevents=nevents+1   ! do not aff -1
               n_yr(i)=n_yr(i)+nevents               ! sum event pr year
            endif
         enddo
       enddo
       call sei close(close$,log_unit,code)
c
      return
c
 99   continue
      write(6,*)' Error in data base log file'
      return
      end

