c
c   program to make data base directory structure, conssist of data base
c   specified and the DEL directory
c
c
c   updates
c   jh jan 95     : ************ version 5.0 ****************************
c      jan 20     : check for DEL
c     jan 31, 96  : make it possible to also make years 00 to 09
c   jan 6, 98 jh  : Year 2000 *******************************************
c   sep 98  jh    : ---------------  version7.0 check -----------------
c                    5 char base names
c   nov 5 98      : system_c to systemc
c   may 18  99    : blank to end of month
c   Jun 15 99  lo : also create WAV structure
c   Dec 06 99  lo : only ask once

      implicit none
c-- top dir
      character*60 top_directory
c-- text
      character*80 text
c-- base name
      character*5 base,save_base
c-- directory to make
      character*80 dir
c-- time interval
      character*6 time1,time2
      integer month1,month2,year1,year2,month,year
c-- dircetory character \ or /
      character*1 dchar
c-- switch for second runs
      logical second_run,both
c-- help variables
      integer id,i
c mode: REA or WAV
      character*4 mode
c is wav directory structure created
      logical wav_done

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call dir_char(dchar)
      second_run=.false.
      both = .false.

      wav_done = .false.

c
c  get input
c
  1   continue
      write(6,*)' Give 1-5 letter base name, UPPER CASE'
      read(5,'(a)') text
      i=index(text,' ')-1
      if(i.lt.2.or.i.gt.5) then
         write(6,*)' Base name MUST be 1-5 characters long'
         goto 1
      endif
      if(text(1:5).eq.'DELET') then
         write(6,*)' DELET is reserved for deleted events'
         goto 1
      endif
c
c   replace blanks with underscore
c
      do i=2,5
        if(text(i:i).eq.' ') text(i:i)='_'
      enddo
      base=text(1:5)
      save_base=base
      write(6,*)' Give start time, year month, e.g. 198302'
      read(5,'(a)') time1
      write(6,*)
     *' Give end time, year month, e.g. 198303, blank for one month'
      read(5,'(a)') time2
      if(time2.eq.' ') time2=time1
      read(time1,'(i4,i2)') year1,month1
      read(time2,'(i4,i2)') year2,month2
      write(6,*) ' Create REA or WAV structure or BOTH'
      read(5,'(a)') mode
      if (mode.eq.'rea') mode='REA'
      if (mode.eq.'wav') mode='WAV'
      if (mode.eq.'both') mode='BOTH'
      if (mode.ne.'WAV'.and.mode.ne.'REA'.and.mode.ne.'BOTH') then
         write(6,*) ' wrong input: ',mode
         stop
      endif
      if (mode.eq.'BOTH') then
        mode='REA'
        both = .true.
      endif
c
c   get top dir
c
      call topdir(top_directory)
      id=index(top_directory,' ')-1
c
c   from here there are two loops, one for the regular directory
c   and one for DELETE, which is the same for all bases
c
 500  continue
      if(second_run.and.mode.eq.'REA') base='DELET'    ! second time make DELET dirs
c
c   make start of dir name
c
      dir(1:id)=top_directory(1:id)
      dir(id+1:id+1)=dchar
      dir(id+2:id+4)=mode   ! lo
      dir(id+5:id+5)=dchar
      dir(id+6:id+10)=base
      dir(id+11:id+11)=dchar
c
c   make fixed directories although they might be there
c
      write(6,'(a,a,2x)')' Making directory ',dir(1:id+10)
      call systemc('mkdir '//dir(1:id+10),id+16)
      if (mode.eq.'REA') then
        dir(id+12:id+14)='LOG'
        write(6,'(a,a,2x)')' Making directory ',dir(1:id+14)
        call systemc('mkdir '//dir(1:id+14),id+20)
        dir(id+12:id+14)='CAT'
        write(6,'(a,a,2x)')' Making directory ',dir(1:id+14)
        call systemc('mkdir '//dir(1:id+14),id+20)
      endif
c
c  loop for making full dir names and creating directories
c
      year=year1
      month=month1
c
c   first make yearly directories
c
  5   continue
         if(year.gt.year2) goto 9
         write(dir(id+12:id+15),'(i4)') year
         if(dir(id+12:id+12).eq.' ') dir(id+12:id+12)='0'
         if(dir(id+13:id+13).eq.' ') dir(id+13:id+13)='0'
         write(6,'(a,a,2x)')' Making directory ',dir(1:id+15)
         call systemc('mkdir '//dir(1:id+15),id+21)
         year=year+1
      goto 5
c
c   make monthly directories
c
 9    continue
      year=year1
      month=month1
c  
 10   continue
      if((year.ge.year2.and.month.gt.month2).or.year.gt.year2) 
     *goto 99
      write(dir(id+12:id+15),'(i4)') year
      if(dir(id+12:id+12).eq.' ') dir(id+12:id+12)='0'
      if(dir(id+13:id+13).eq.' ') dir(id+13:id+13)='0'
      dir(id+16:id+16)=dchar
      write(dir(id+17:id+18),'(i2)') month
      if(dir(id+17:id+17).eq.' ') dir(id+17:id+17)='0'
      write(6,'(a,a,2x)')' Making directory ',dir(1:id+18)
      call systemc('mkdir '//dir(1:id+18),id+24)
      month=month+1
      if(month.eq.13) then
         month=1
         year=year+1
      endif
      goto 10

c
c
 99   continue

      if (mode.eq.'WAV') wav_done = .true.

      if(.not.second_run.and.mode.ne.'WAV') then
         second_run=.true.
         goto 500                ! make second run
      endif

      if (mode.eq.'REA'.and.second_run.and.both) then
          mode = 'WAV'
          base = save_base
          goto 500
      endif

      stop
      end
