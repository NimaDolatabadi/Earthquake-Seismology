      subroutine sfilname
     *(year,month,day,hour,min,sec,base,type,name,length)
c
c
c
c updates
c jh  jul 10 93: agency for REA
c     sep 14   : bug with default agency
c     dec 94   : enable local directory name of base is .
c     jan 11   : new get def base
c     jan 13 98: *********   year 2000 *********************
c     jan 25   : local base can also be ,,,
c     aug      : ----------   version 7.0 check ------------------
c              : only long s-file names, 5 letter base
c     sep 22   : fix bug in file name
c     oct 16   : one more bug
c     sep 30 99: add subroutine check_id
c     mar 07 01: init name before setting it, Linux problem
c     apr 08 01: variable length of name
c
c   makes s file name from year month etc
c
c     base: 5 letter data base
c     type: event type e.g. L
c     name: s file name
c     length: char lenght of name
c
      implicit none
c-- top dir
      character*60 top_directory
c-- date and time
      integer year,month,day,hour,min,sec
c-- base name
      character*5 base
c-- def base 
      character*5 def_base
c-- directory to make
      character*(*) name
      character*80 xname
c-- dircetory character \ or /
      character*1 dchar
c-- event type
      character*1 type
c--
      integer length
c-- help variables
      integer id,i
c
      call dir_char(dchar)
c
c
c  get input
c
c
c   get top dir
c
      call topdir(top_directory)
      id=index(top_directory,' ')-1
c
c   getdef base 
c
      call get_def_base(def_base)
c
c   use agency as default
c
      if(base.eq.'     ') base=def_base
c
c   make start of name
c
      name=' '
      name(1:id)=top_directory(1:id)
      name(id+1:id+1)=dchar
      name(id+2:id+4)='REA'
      name(id+5:id+5)=dchar
      name(id+6:id+10)=base
      name(id+11:id+11)=dchar
      write(name(id+12:id+15),'(i4)') year
      name(id+16:id+16)=dchar
      write(name(id+17:id+18),'(i2)') month
      name(id+19:id+19)=dchar
      write(name(id+20:id+26),'(i2,a,2i2)')day,'-',hour,min
c
      write(name(id+27:id+38),'(a,i2,a,a,i4,i2)')'-',
     *sec,type,'.S',year,month
      do i=1,id+38
         if(name(i:i).eq.' ') name(i:i)='0'
      enddo
      length=id+38
c
c   cut out start of name if base is ,, to indicate local
c   directory
c
      if(base(1:2).eq.',,') then
         xname(1:length-id-19)=name(id+20:length)
         do i=1,80
           name(i:i)=' '
         enddo
         name=xname
         length=length-id -19
      endif
      return
      end
c
c 
c
      subroutine check_id(sfilname,event_id,idline,err)
c
c   compare sfile name with id line,and event type, return err=0 if ok, else,1
c   jh sep 99
c    
      implicit none
      character*80 sfilname,idline
      character*1 event_id
      integer l           ! length of s-file name
      integer err
      integer seiclen
c
      err=0
c
      l=seiclen(sfilname)
      if(sfilname(l-18:l-17).ne.idline(67:68)) err=1   ! day
      if(sfilname(l-15:l-12).ne.idline(69:72)) err=1   ! hour min
      if(sfilname(l-10:l- 9).ne.idline(73:74)) err=1   ! sec
      if(sfilname(l- 5:l- 2).ne.idline(61:64)) err=1   ! year
      if(sfilname(l- 1:l   ).ne.idline(65:66)) err=1   ! month
      if(sfilname(l- 8:l- 8).ne.event_id)      err=1   ! id, L, R or D

c
      return
      end
c
