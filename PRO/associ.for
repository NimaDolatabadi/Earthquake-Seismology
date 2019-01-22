c   program to  merge single  event files to one file from
c   standard data base or a base and a file. Two events are merged together
c   if they are less than maxdif seconds apart 
c
c   jens havskov, jan 88
C   uppdates:
c   mar 8 by j.h   : also merge distant events
c   jan 19   j.h.  : adaption new data base, file name length
c
c   aug 6    c.l.  : new merge routine implemented
c   feb 17, 92 r.a.: fixed for SUN
c   feb 18,    r.a.: fixed for time difference at new month
c   jul 93    version 3.0
c   aug 23         : filenames to 80
c   nov 24, 1994   : opening files changed
c   dec 94         : ********* version 5.0 ********************
c   jan 11         : new find ev in
c   feb 7 99 jh    : ---------Version 7.0 ---------------------
c                    fix year, start and end times
c   sep 23   jh    : put in confirm overrride, error if dir name had a '-'
c                    put in time routines
c   nov 6,00       : bug: seconds were not used
c   feb 2001       : merge from data base and a file
c   mar 23 2004 jh : also write out events used for merging if from a file
c   mar 22 2011 jh : gfortan bug for default max difference
c   may 14 2012 jh : new merge_f routine
c   may 29 2013 jh : possibel to put header from file first

        implicit none	
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! File operations.
       include 'seidim.inc'                ! Dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
       character*80	data1(max_data),data2(max_data)	
c event data
       character*1	exp		
c exp indicator
       character*1      type1,type2,type
c event type
       character*1      answer		
c answer
       integer		nrecord		
c number of recors for event
       integer		nhead		 
c  number of header lines
       integer		nrecord2	
       character*1      confirm     ! y: confirm delete, n, do not confirm
c  number of recors for event
       integer		nhead2		
c   number of header lines
       integer		nstat, nphas
c number of stations
       character*14     start_time,end_time 
c start and end time of select
       character*40	base_name	
c data base name if not rea (blank)
       character*80	evfile1,evfile2, text	
c event file name
       character*10     key		
c select key
       integer		status,new_month,fstart,event_no 
c see subroutine find...
       integer nass,ndel,nmer 		
c counters for events, records etc.
       integer		maxdif		
c maximum difference in secs b. ev.
       integer		year1,month1,day1,hr1,min1,sec1
       integer		year2,month2,day2,hr2,min2,sec2
       real sec11,sec22
       double precision t1,t2,diff
c time in day-hr-min-sec
       integer		choice		
c choice for merging, see below
       integer		first		
c indicate first time read 1: first
       integer 		i,id		
c print unit
       integer          printu
c logical for existing file or not
       logical          b_old
c returned code                                   
       integer          code
c read unit #1 
       integer          readu1
c read unit #2
       integer          readu2
       integer          npos       ! position of flag
       logical          afile      ! true if file comparison used
       character*80     infile     ! file name
       logical          merged     ! true if event used for merge
       logical          file_first ! true if file header first
c function
       real seirealnum


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      file_first=.false.
c
c   open output file
c
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'associ.out',       ! File name
     &              printu,                ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
c
c   input base name and time interval
c
       write(6,*)' Base name, return for default'
       read(5,'(a40)') base_name
       write(6,*)' Start time'
       read(5,'(a14)') start_time
       write(6,*)' End time, return for end of month'
       read(5,'(a14)') end_time
c
 3     continue
c
c Give time difference between events to be associated
c
13     write(*,*) 
     * ' Give maximum time difference between two events to associcate'
       write(6,*) ' default is 150 s (return):'
       read(5,'(a)') text
       if(text.eq.' ') then
          maxdif=150
       else
          maxdif= sei real num (text,code)
       endif
       write(6,*) ' Time difference selected',maxdif
       write(6,*)		 
c
c  associate from a file or from same base
c
c
       write(6,*)' Associate from a file        :  Enter file name'
       write(6,*)' Associate from same data base:  Enter          '
       read(5,'(a)') infile
       afile=.false.
       if(infile.ne.' ') afile=.true. ! associate from infile'
c
c   open file if associate from file
c
       if(afile) then
          write(6,*) 'Put header from file first (y/n=default)'
          read(5,'(a)') answer
          if(answer.eq.'y') file_first=.true.
 344      continue
          open(3,file=infile,status='old',err=345)
          goto 346
 345      continue
          write(6,*)' Error opening file, enter file name'
          read(5,'(a)') infile
          goto 344
 346      continue
        endif

       if(.not.afile) then
       write(6,*)
     *' check events which can be associated                    (1)'
       write(6,*)
     *' associate and merge events, do NOT delete merged events (2)'      
       write(6,*)
     *' associate and merge events, delete merged events        (3)'
       read(5,*) choice
       if(choice.ne.1.and.choice.ne.2.and.choice.ne.3) then
c
c check choice
c
          write(6,*) 'wrong choice, try again'
          goto 3
       endif
       endif
       if(afile) then
 33    continue
       write(6,*)
     *' check events which can be associated                    (1)'
       write(6,*)
     *' associate and merge events                              (2)'
       read(5,*) choice
       if(choice.ne.1.and.choice.ne.2) then
c
c check choice
c
          write(6,*) 'Wrong choice, try again'
          goto 33
       endif
       endif
c
c always use next event
c
       confirm='n'
       if(choice.gt.1) then
          write(6,*)' Confirm merge (y/n)'
          read(5,'(a)') confirm
       endif

       key='          '
c
c  reset counters
c
       nass=0
       nmer=0
       ndel=0
       first=1
c
c
c----------------------------------------------------------------
c   merge from same base
c----------------------------------------------------------------
c
       if(.not.afile) then


 4     continue
c
c   read first event or first event after a merge
c
       CALL findevin
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile1,fstart,new_month,status)
       if(first.eq.1) then		
c  check if first time around
          new_month=0			
c lower new month flag
          first=0                       
c  lower first flag
       endif

c  return if eof or other erorrs

       if(status.ne.0) go to 99
c
c indicate that no comparisom possible
c
       t1=-1				

       write(6,208) event_no,evfile1(1:50),int(t1)
       goto 6
c
c  read and write loop until pair found
c
 5     continue
c
c   shift to next event, save previous
c
       evfile1=evfile2
       key='          '		
c always use next event
 6     continue			
c start here the first time
       CALL findevin	
c get name of next event
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile2,fstart,new_month,status)

c return if eof or other erorrs

       if(status.ne.0) goto 99
c
c   compare times
c
       call time_evname(evfile1,year1,month1,day1,hr1,min1,sec1,type1)
       call time_evname(evfile2,year2,month2,day2,hr2,min2,sec2,type2)
       sec11=sec1
       sec22=sec2
       call TIMSEC (YEAR1,month1,DAY1,HR1,MIN1,SEC11,t1)
       call TIMSEC (YEAR2,month2,DAY2,HR2,MIN2,SEC22,t2)
       write(6,208) event_no,evfile2(1:55),int(t2-t1)
 208   format(1x,'#',i4,1x,a55,' Diff=',i8,' s')
c
c   check types before comparing times
c
C       if(type1.eq.'D'.or.type2.eq.'D') go to 5	
c do not merge distant events
       if(dabs(t2-t1).lt.maxdif) then
c
c   count events associated 
c
          nass=nass+1
          if(choice.eq.1) then	
c only show which events are associated
             write(printu,217,iostat=code)
     *       evfile2(fstart:fstart+18),evfile1(fstart:fstart+18)
             call sei code(stop$,          ! Force stop on error.
     &                     code,           ! Fortran i/o condition.
     &                     printu,         ! Unit with condition.
     &                     b_old)          ! Flag (n/a).
C
             write(6,217)
     *       evfile2(fstart:fstart+18),evfile1(fstart:fstart+18)
 217         format(1x,'   ***** ',a,' is associated with ',a,' *****')
          else
c
c merge associated events
c
c  question merge
c
            write(6,207) event_no,
     *      evfile2(fstart:fstart+10),evfile1(fstart:fstart+10)
 207        format(1x,'    **** merge #',i3,2x,a,' with ',a,' y/n ****')
            if(confirm.eq.'N'.or.confirm.eq.'n') then
               answer='y'
            else
               read(5,'(a1)') answer
            endif
            if(answer.eq.'y'.or.answer.eq.'Y') then
               call sei open(old$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile1,               ! File name
     &                       readu1,                ! Read unit #1
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
               call sei open(old$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile2,               ! File name
     &                       readu2,                ! Read unit #2
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
c
c   read two input files
c
               call indata
     *         (readu1,nstat,nphas,nhead,nrecord,type,exp,data1,id)
               call indata
     *         (readu2,nstat,nphas,nhead2,nrecord2,type,exp,data2,id)

              
c
c   merge events
c
               call merge_f(data1,data2,nhead,nhead2,nrecord,nrecord2,
     *         status)
               if(status.gt.0) then
                  write(6,*) ' Two events more than 24 h apart or ',
     *            'second event is before first event'
                  stop
               endif


               rewind( readu1, iostat=code )
               call sei code(stop$,             ! Stop program on error.
     &                       code,              ! Fortran i/o condition.
     &                       readu1,            ! Unit with condition.
     &                       b_old)             ! Flag (n/a).
C
               write(readu1,'(a80)',iostat=code)
     &                         (data1(i),i=1,nrecord)   
               call sei code(stop$,             ! Stop program on error.
     &                       code,              ! Fortran i/o condition.
     &                       readu1,            ! Unit with condition.
     &                       b_old)             ! Flag (n/a).
C
c write event
c
c   close and delete file 2 if choice is 3
c
               nmer=nmer+1		
c count merged events
               if(choice.eq.3) then
                  call sei close (delete$+warn$,readu2,code)
                  if (code .eq. e_ok$) write(6,*)
     *                '    **** event',event_no,' deleted ****'
                  ndel=ndel+1		
c count deleted events
               else
                  call sei close (close$,readu2,code)
               endif
               call sei close (close$,readu1,code)
               go to 4	  
c go and read one extra event before continuing
            endif
         endif
       endif
      
       goto 5 ! next event

      endif

c
c-----------------------------------------------------------------
c  merge from file and base
c----------------------------------------------------------------
c-
c
      if(afile) then

        open(4,file='associ_rest.out',status='unknown')
        open(8,file='associ_merg.out',status='unknown')
c
c   initialize by read one event from each source
c                                                
c
c   read event of data base
c
       CALL findevin
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile1,fstart,new_month,status)
       if(first.eq.1) then		
c  check if first time around
          new_month=0			
c lower new month flag
          first=0                       
c  lower first flag
       endif
c
c  return if eof or other erorrs
c
       if(status.ne.0) go to 99
c
c   read first event from file
c
       merged=.false.                ! flag to indicate event not tmerged
       call indata
     * (3,nstat,nphas,nhead2,nrecord2,type,exp,data2,id)
       if(nrecord2.eq.0) goto 99     ! end of file
c
c   compare times
c
       call time_evname(evfile1,year1,month1,day1,hr1,min1,sec1,type1)
       read(data2(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     * year2,month2,day2,hr2,min2,sec22
       sec11=sec1
       call TIMSEC (YEAR1,month1,DAY1,HR1,MIN1,SEC11,t1)
       call TIMSEC (YEAR2,month2,DAY2,HR2,MIN2,SEC22,t2)
       diff=t2-t1
       write(printu,'(a,2x,f12.1)') evfile1(fstart:fstart+19), diff
c
c   merge if difference small enough
c
       if(dabs(diff).le.maxdif) then
         merged=.true.                    ! event merged, do not write out
         npos=2222
         goto 5555   ! merge point
       endif
 2222  continue

c
c  loop for reading in base and file
c

  250  continue
c
c   advance in file until file time has passed data base time
c   check if any events to be merged
c
       do while(t2.le.t1)
c
c   if previous event was not used for merge, write out event in
c   file for not merged events, else write in file for events
c   which were used for merging. note, the actual merged events are written 
c   out in a different file
c
          if(.not.merged) then
              write(4,'(a)') (data2(i),i=1,nrecord2)
              ndel=ndel+1
          else
              write(8,'(a)') (data2(i),i=1,nrecord2)
          endif
c
          call indata
     *    (3,nstat,nphas,nhead2,nrecord2,type,exp,data2,id)
          if(nrecord2.eq.0) goto 99
          merged=.false.                              ! reset merge flag
c          write(6,*)'advance indata'
          read(data2(1),'(1x,i4,1x,2i2,1x,2i2,f5.1)')
     *    year2,month2,day2,hr2,min2,sec22
          call TIMSEC (YEAR2,month2,DAY2,HR2,MIN2,SEC22,t2)
          diff=t2-t1
          write(printu,'(a,2x,f12.1)') evfile1(fstart:fstart+19), diff
          if(dabs(diff).lt.maxdif) then
             merged=.true.       ! event used for merge
c             ndata=ndata+1
             npos=3333
             goto 5555
          endif
 3333     continue
c           if(ndata.gt.0) call data_back(3,ndata)
c           ndata=0
        enddo             
c
c   advance data base one event
c
       CALL findevin
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile1,fstart,new_month,status)
c
c  return if eof or other erorrs
c
       if(status.ne.0) go to 99
       call time_evname(evfile1,year1,month1,day1,hr1,min1,sec1,type1)
       sec11=sec1
       call TIMSEC(YEAR1,month1,DAY1,HR1,MIN1,SEC11,t1)
       diff=t2-t1
       write(printu,'(a,2x,f12.1)') evfile1(fstart:fstart+19), diff
       if(dabs(diff).lt.maxdif) then
          merged=.true.                   ! INDECATE EVENT MERGED
          npos=4444
          goto 5555
       endif
 4444  continue
c
c   back up to check file
c
       goto 250




 5555  continue

c
c   count events associated 
c
          nass=nass+1

          if(choice.eq.1) then
c
c only show which events are associated
c
             write(6,217)
     *       evfile1(fstart:fstart+18),data2(1)(1:20)
             write(printu,217)
     *       evfile1(fstart:fstart+18),data2(1)(1:20)

          else
c
c merge associated events
c

c
c  question merge
c
            write(6,207) event_no,
     *      evfile1(fstart:fstart+10),data2(1)(1:20)
c
            write(printu,207) event_no,
     *      evfile1(fstart:fstart+10),data2(1)(1:20)

            if(confirm.eq.'N'.or.confirm.eq.'n') then
               answer='y'
            else
               read(5,'(a1)') answer
            endif
            if(answer.eq.'y'.or.answer.eq.'Y') then
               call sei open(old$,                  ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile1,               ! File name
     &                       readu1,                ! Read unit #1
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
c
c   read data base file
c
               call indata
     *         (readu1,nstat,nphas,nhead,nrecord,type,exp,data1,id)
           
c
c   merge events
c
               if(file_first) then
                  call merge_f(data2,data1,nhead2,nhead,
     *            nrecord2,nrecord,
     *            status)
               else
                  call merge_f(data1,data2,nhead,nhead2,
     *            nrecord,nrecord2,
     *            status)
               endif
               if(status.gt.0) then
                  write(6,*) ' Two events more than 24 h apart or ',
     *            'second event is before first event'
                  stop
               endif

               rewind( readu1, iostat=code )
               call sei code(stop$,             ! Stop program on error.
     &                       code,              ! Fortran i/o condition.
     &                       readu1,            ! Unit with condition.
     &                       b_old)             ! Flag (n/a).
C
               if(file_first) then
                  write(readu1,'(a80)',iostat=code)
     &            (data2(i),i=1,nrecord2) 
               else
                  write(readu1,'(a80)',iostat=code)
     &            (data1(i),i=1,nrecord)
               endif  
               call sei code(stop$,             ! Stop program on error.
     &                       code,              ! Fortran i/o condition.
     &                       readu1,            ! Unit with condition.
     &                       b_old)             ! Flag (n/a).
               
               call sei close (close$,readu1,code)
             endif
c
c   count merged events
c
             nmer=nmer+1
          endif
c
c  back to where merge was initiated from
c
         if(npos.eq.2222) goto 2222
         if(npos.eq.3333) goto 3333
         if(npos.eq.4444) goto 4444

       endif
c-------------------------------------------------------------------



 99    continue
       if(afile) then
c
c   if merge from file, write out rest of events from file not used
c
          if(.not.merged) then
             write(4,'(a)') (data2(i),i=1,nrecord2)
             ndel=ndel+1
          endif
 90       continue
          call indata
     *    (3,nstat,nphas,nhead2,nrecord2,type,exp,data2,id)
          if(nrecord2.eq.0) goto 199     ! end of file
          write(4,'(a)')(data2(i),i=1,nrecord2)
          ndel=ndel+1
          goto 90
 199      continue
       endif
 
c
c  print out statistics
c

      write(6,*)
      write(6,*)
      if(.not.afile) write(6,201) nass,nmer,ndel
      if(afile) then
        write(6,*) ' Number of associated events from file',nass
        write(6,*) ' Number of merged events              ',nmer
        write(6,*) ' Number of remaining events in file   ',ndel
        write(6,*)
      endif
        

      write(printu,*,iostat=code)
      call sei code( stop$, code, printu, b_old)
C
      write(printu,*,iostat=code)
      call sei code( stop$, code, printu, b_old)
C
      write(printu,201,iostat=code) nass, nmer, ndel
      call sei code( stop$, code, printu, b_old)
C

 201  format(' Total number of associated events',i7,
     *     /,' Total number of merged events    ',i7
     *     /,' Total number of deleted events   ',i7)
c
      write(6,*)' Output of statistics id associ.out'
      if(afile) then
        write(6,*)
     *' Events in file not used for merge: associ_rest.out'
        write(6,*)
     *' Events in file used for merge: associ_merg.out'
      endif
      stop
      end
     

      subroutine time_evname(evname,year,month,day,hr,min,sec,type)
c
c  finds date and event type from seisan data base evname.
c  Possible disk or directory names are of no importance.
c
      implicit none
      integer year,month,day,hr,min,sec
      character*80 evname	
c event file name inc possibly disk name
      character*1  type		
c event type
      integer i
      call get_fstart(evname,i)
      i=i+2
c     i = index(evname,'-')                
c find first '-' in name
      read(evname(i-2:i-1),'(i2)') day
      read(evname(i+1:i+2),'(i2)') hr
      read(evname(i+3:i+4),'(i2)') min
      read(evname(i+6:i+7),'(i2)') sec
      read(evname(i+8:i+8),'(a1)') type
      read(evname(i+11:i+14),'(i4)') year
      read(evname(i+15:i+16),'(i2)') month
         
c
      return
      end
c -----------------------------------------------------


      subroutine data_back(unit,ndata)
c
c   backspace one or several events, not tested
c
      implicit none

      integer ndata,unit,k
      character*80 text
      k=0

  10  continue
      backspace(unit)
      backspace(unit)
      read(unit,'(a)') text
      if(text.eq.' ') k=k+1
      if(k.eq.ndata) return
      goto 10

      return
      end
         


