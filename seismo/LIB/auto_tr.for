      subroutine auto_tr(data,nhead,nrecord,nfile,filename)
c
c   routine to get trace file names from  a nordic file
c   if file name ends with _LIST, the file contains names of many waveform files,
c   the format is like filenr.lis
c
c   updates
c
c   feb 99 by jh :---------------  verison 7.0 check ------------------
c                 no changes
c   dec 5 2011 jh: add option MANY to exapand files with files
c   apr 30 2012 jh: change MANY to _LIST
c   dec 9 2012 jh : expand dimension of file_list from 200 to 1000
c   dec 30 2012 jh: expand archive request
c   jan 4  2013 jh: more changes to arc, add nrecord
c   jan 9  2013 jh: more changes to arc, option arc_by_default=2
c   jan 16 2013 jh: check time limits for channel validity
c   mar 18 2016 lo: selecting for multiple virtual networks
c   jan 5  2017 lo: init arc_flag
c
c   input:
c   data: nordic S-file
c   nhead: number of headers in file
c   output:
c   nfile: number of waveform files
c   filename: waveform files
c
      implicit none
      include 'seisan.inc'
      character*80 data(*)
      character*(*) filename(*)
      character*80 new_file(10,1000)    ! the expanded file names
      character*80 file_out            ! file with full path
      integer nhead,nfile,nrecord
      character*5 stat(2000)           ! stations read
      integer nlist                    ! number of nlist files
      integer nlist_files(5)           ! number of of files in each LIST file
       integer file_list(1000)   ! indicator if a particular file is of LIST type
      integer YEAR,DOY,MTH,DAY,HR,MIN   ! time and date
      double precision msecs            ! for time conversion
      character*23 start_time_duration  ! for arc reference
      real sec                          ! for arc reference
      logical arc_line                  ! true if any arc lines
      
      integer seiclen
      integer i,k,l,m,kstat,n,o,vn
      logical arc_flag(3000)

      nfile=0
      arc_line=.false.

      do i=1,3000   ! added lo 6 jan 2017
        arc_flag(i)=.false.
      enddo

c
c   if arc is set by default, add an arc line if not already there
c
      if(arc_by_default.gt.0) then
c
c   first check if any arc line
c
         do i=2,nhead
            if(data(i)(80:80).eq.'6'.and.data(i)(2:4).eq.'ARC')goto 220
         enddo
c
c   no line, add it
c              
         do i=nrecord,2,-1
            data(i+1)=data(i)
         enddo

         data(2)=' '
         data(2)(2:6)='ARC *' ! make sure something is written

         if(arc_by_default.eq.1) then
            data(2)(2:6)='ARC *'
         elseif(arc_by_default.eq.2) then
            data(2)(2:6)='ARC P'
         endif
         data(2)(80:80)='6'
         read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *   year,mth,day,hr,min,sec
         call TIMSEC (YEAR,MTH,DAY,HR,MIN,SEC,MSECS)
         msecs=msecs-arc_start_time   ! start arc_start_time before origin time
         call SECTIM (MSECS,YEAR,DOY,MTH,DAY,HR,MIN,SEC)
         write(data(2)(22:38),
     *   '(i4,1x,2i2,1x,2i2,1x,i2)')
     *   YEAR,MTH,DAY,HR,MIN,int(SEC)
         write(data(2)(39:44),'(i6)') int(arc_duration)
         nrecord=nrecord+1
         nhead=nhead+1
 220     continue
      endif
c      write(6,*) 'arc',data(2)
c
c
c   get file names
c
      do i=2,nhead
         if(data(i)(80:80).eq.'6') then
            nfile=nfile+1
            filename(nfile)(1:78)=data(i)(2:79)
            filename(nfile)(79:80)=' '
          endif
      enddo

c      write(6,*)'nfile',nfile,arc_by_default
      if(nfile.eq.0.and.arc_by_default.eq.0) return
c
c   get station names for stations read
c
      k=0
      do i=nhead+1,nrecord
         if(data(i)(2:5).ne.' ') then
            k=k+1
            stat(k)=data(i)(2:5)
         endif
      enddo
      kstat=k
c
c   check if any of the files is a file with files indicated
c   by ending with _LIST
c
      nlist=0     ! number of files with files

      do i=1,nfile
        file_list(i)=0
        k=seiclen(filename(i))

c-------------------------------------------------------------
c   section for LIST file
c-------------------------------------------------------------
        if(filename(i)(k-4:k).eq.'_LIST') then
          file_list(i)=1         ! indicate that this was a LIST file
c
c   check if file exists and where, if not skip
c
          file_out=' '
          call  get_full_wav_name(filename(i),file_out)
          if(file_out.eq.' ') then
              write(6,'(a,a)')' File does not exist ', filename(i)
              write(6,'(a)')  ' File will be skipped'
              goto 200
          endif
c
          nlist=nlist+1
          if(nlist.gt.10) then
             write(6,*) 'Too many LIST files, max 10'
             stop
          endif
          open(96,file=file_out,status='old')
          k=1
 90       continue
          read(96,'(7x,a63)',end=100)new_file(nlist,k)

          if(new_file(nlist,k).eq.' ') goto 100
          k=k+1
          goto 90 
 100      continue
          close(96)
          k=k-1
          nlist_files(nlist)=k
        endif
 200    continue
      enddo

c
c   remove LIST files from list if there, also if it does not exist
c

      k=0
      do i=1,nfile
          if(file_list(i).ne.1) then
            k=k+1
            filename(k)=filename(i)
          endif
      enddo           
      if(nlist.gt.0) then
c
c   now put files in list
c     
      l=k
         do k=1,nlist
            do i=1,nlist_files(k)
               l=l+1
               filename(l)=new_file(k,i)

            enddo
         enddo
         nfile=l
      endif
c
c    end section for LIST file
c
c---------------------------------------------------------------
c  section for ARC references for expansion
c---------------------------------------------------------------
c
c  now check for arc lines
c
   
      do i=1,nfile
        if(filename(i)(1:4).eq.'ARC '.and.(filename(i)(5:9).eq.'P    '.
     *  or.filename(i)(5:5).eq.'*'.or.filename(i)(5:9).eq.' '.or.
     *     filename(i)(5:5).eq.'_'))   ! virtual network
     *  then
c
c   flag if any arc lines
c
           arc_line=.true.
c
c   use all stations in SEISAN.DEF, first find start time and duration
c
           
            start_time_duration=' '

c            write(6,*)'found arc'
c
c   set start time
c
            if(filename(i)(21:37).eq.' ') then   ! take start time from origin time
               read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *         year,mth,day,hr,min,sec
               call TIMSEC (YEAR,MTH,DAY,HR,MIN,SEC,MSECS)
               msecs=msecs-arc_start_time   ! start arc_start_time before origin time
               call SECTIM (MSECS,YEAR,DOY,MTH,DAY,HR,MIN,SEC)
               write(start_time_duration(1:17),
     *         '(i4,1x,2i2,1x,2i2,1x,i2)')
     *         YEAR,MTH,DAY,HR,MIN,int(SEC)
            else                                 ! use given value
               start_time_duration(1:17)=filename(i)(21:37)
            endif
c
c   set duration
c
            if(filename(i)(38:43).eq.' ') then  ! use default
               k=arc_duration
               write(start_time_duration(18:23),'(i6)') k
            else                                ! use given value
               start_time_duration(18:23)=filename(i)(38:43) 
            endif
c
c   now expand for all channels
c
            k=nfile   ! start counting where previous was
c
c   abs time of current event
c
            read(data(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *      year,mth,day,hr,min,sec     
            call TIMSEC (YEAR,MTH,DAY,HR,MIN,SEC,MSECS)
c
            do m=1,arc_nchan
               if (arc_flag(m)) goto 300  ! already selected
c
c   first check if arc channel is available for this time, if not skip
c

c               write(6,*) msecs,arc_start(m), arc_stop(m)
               if(msecs.gt.arc_stop(m).or.msecs.lt.arc_start(m))goto 300
c
c check if channel in virtual network
c
               if (filename(i)(5:5).eq.'_') then
c
c find number of virtual network
c
                 vn=0
                 do n=1,arc_vnet       
                   if (filename(i)(5:9).eq.arc_vnet_name(n)) then
                     vn=n
                   endif
                 enddo 
                 write(55,*)
     &arc_stat(m),arc_comp(m),arc_net(m),arc_loc(m)
                 do n=1,arc_vnet_nchan(vn)
                   write(55,*) arc_vnet_stat(vn,n),
     &arc_vnet_comp(vn,n),arc_vnet_net(vn,n),
     &arc_vnet_loc(vn,n)
                   if (arc_stat(m).eq.arc_vnet_stat(vn,n).and.
     &                 arc_comp(m).eq.arc_vnet_comp(vn,n).and.
     &                 arc_net(m).eq.arc_vnet_net(vn,n).and.
     &                 arc_loc(m).eq.arc_vnet_loc(vn,n)) then
                     goto 240
                   endif
                 enddo
                 goto 300
 240             continue
               endif     

c
c               write(6,*)'time ok'
c
c   if using stations read as selection criteria, check if this station
c   was read
c
c

               if(filename(i)(5:6).eq.'P ') then
                  do n=1,kstat
                     if(stat(n).eq.arc_stat(m)) then
                        goto 250
                     endif
                  enddo
               goto 300   ! station not found
               endif

 250           continue   ! station found

               k=k+1
               filename(k)=' '
               filename(k)(1:4)='ARC  '
               filename(k)(5:9)=arc_stat(m)
c
c   if component is required, only select that component
c
               if(filename(i)(11:13).ne.' ') then
                  if(arc_comp(m).ne.filename(i)(11:13)) then
                     filename(k)=' '
                     k=k-1
                     goto 300
                  endif
               endif
               filename(k)(11:13)=arc_comp(m)
c
c  if a network is given, only chose from that net
c
               if(filename(i)(15:16).ne.' ') then
                   if(arc_net(m).ne.filename(i)(15:16)) then
                     filename(k)=' '
                     k=k-1
                     goto 300
                   endif
               endif
c
c  if a location is given, only chose from with that location
c
               if(filename(i)(18:19).ne.' ') then
                   if(arc_loc(m).ne.filename(i)(18:19)) then
                      filename(k)=' '
                      k=k-1
                      goto 300
                   endif
               endif
               filename(k)(15:16)=arc_net(m)
               filename(k)(18:19)=arc_loc(m)
               filename(k)(21:43)=start_time_duration
               arc_flag(m)=.true.
               nfile=k
c
c               write(6,*) 'file expanded',filename(k)
 300           continue
             enddo
          endif
      enddo
      if(arc_line) then
         nfile=k
c
c   remove old ARC file used for expansion
c
         k=0
         do i=1,nfile
c
c           write(6,*)filename(i)
           if(filename(i)(1:4).eq.'ARC '.and.
     *         (filename(i)(5:9).eq.' '.or.
     *      filename(i)(5:5).eq.'*'.or.filename(i)(5:6).eq.'P '.or.
     *      filename(i)(5:5).eq.'_')) then
              continue
           else
              k=k+1
              filename(k)=filename(i)
c
c              write(6,*) filename(k)
           endif
         enddo
         nfile=k
      endif

 
      return
      end
