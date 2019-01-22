c--------------------------------------------------------------------------
c  program to delete ARC lines in an s-file
c--------------------------------------------------------------------------c
c
c  For detail on parameters and variables naames, see rea.inc
c
c  Arguments: 1: file name 2: overwrite 3: operator
c  2 and 3 used with eev
c
c changes
c
c 2016-03-18 pv: first version taken form dels.for
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 text
      character*80 err_text               ! error text
      character*80 infile                 ! input file
c     character*11 delete(2)              ! delete indicator
      integer delp,dels,delsp,delml,delia,delal   ! flags for delete
      integer nars                        ! number of arguments
      character*80 arg(10)                ! arguments
      real amp(3)                         ! amplitudes
c---time of making update
      character*12 p_time
      character*14 proc_time
      character*4 operator                ! operator
      logical dels_amp                    ! true if amplitudes out

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer nd                          ! number of deleted phases
      integer i,ndel,k                    ! counter

      delp=0
      dels=0
      delsp=0
      delml=0
      delia=0
      delal=0
      operator=' '

c     delete(1)='Not delete '
c     delete(2)='Delete     '

c
c   open output file
c
      open(2,file='arc_del.out',status='unknown')
    
c
c   get input file name, check if exist
c
c
c   get arguments
c
      call get_arguments(nars,arg)
c
c   first argument is file name
c
      if(nars.gt.0) then
         infile=arg(1)
         open(1,file=infile,status='old',err=3)
         goto 5
 3       continue
         write(6,*)'No such file'
         stop
 5       continue
      else

 9       continue
         write(6,*) 'Give input file'
         read(5,'(a)') infile
         open(1,file=infile,status='old',err=10)
         goto 11
 10      continue
         write(6,*)' No such input file'
         goto 9
 11     continue
      endif
c
 12   continue

 40   continue

      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter
      nd=0                        ! deleted lines

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event form file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1               ! count events

c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)
c
c   some example of modifying event
c
c----------------------------------------------------
c - remove ARC lines 
c----------------------------------------------------
c
         k=0
         do i=1,rea_nwav
           if(rea_wav(i)(2:5).ne.'ARC ') then
             k=k+1    ! 
             rea_wav(k)=rea_wav(i)
           endif
         enddo
         nd=nd+(rea_nwav-k)
         rea_nwav=k

c

 100  continue         

c
c   write out modified event, the array data has also been modified
c
       call rea_event_out(2,all,data,code)
c
c   possibely overwrite for one event
c
       if(arg(2).eq.'overwrite') then
           if(nars.eq.3) then
               operator=arg(3)(1:4)
           else
               operator=' '
           endif
           rewind 1
           call get_operator(operator)
c
c   get system time
c
           call systime(p_time,proc_time)
c
c   update id line
c
            if(rea_id_line.ne.' ') then
               WRITE(rea_id_line(31:34),'(A)')OPERATOR
               WRITE(rea_id_line(13:26),'(A)')PROC_TIME
               WRITE(rea_id_line(9:11),'(A)')'DPH'
            else
               write(6,*)' No ID line !!!!!!!'
            endif

           call rea_event_out(1,all,data,code)
           close(1)
           goto 1000
       endif
c
c   get next event
c
      goto 50
c
c     end of file
c
 1000 continue
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Number of deleted lines',nd
      write(6,*) 'Output file name is arc_del.out'
      if(dels_amp) write(6,*)
     *'Ouptut file with IAML amplitudes in dels_amp.out' 

      stop
      end
