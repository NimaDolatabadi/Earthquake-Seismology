c--------------------------------------------------------------------------
c  program to delete phases in an s-file
c--------------------------------------------------------------------------
c
c  For detail on parameters and variables naames, see rea.inc
c
c  Arguments: 1: file name 2: overwrite 3: operator
c  2 and 3 used with eev
c
c
c changes
c
c 17  3 2014 jh: terminate with enter instead of 0
c 17 12 2016 jh: also delete fps
c 27 05 2018 jh: bug in above
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
      character*11 delete(2)              ! delete indicator
      integer delp,dels,delsp,delml,delia,delal,delfps   ! flags for delete
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
      delfps=0

      operator=' '

      delete(1)='Not delete '
      delete(2)='Delete     '

c
c   open output file
c
      open(2,file='dels.out',status='unknown')
    
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
c   lines to delete
c
 12   continue
      write(6,*) 'Give line to delete or keep, terminate with enter'

      write(6,'(a,a)')' 1: Lines with P-phase         ',delete(delp+1) 
      write(6,'(a,a)')' 2: Lines with S-phase         ',delete(dels+1)
      write(6,'(a,a)')' 3: Lines with SPEC-phase      ',delete(delsp+1) 
      write(6,'(a,a)')' 4: Lines with IAML-phase      ',delete(delml+1)      
      write(6,'(a,a)')' 5: Lines with IASP-AMP phase  ',delete(delia+1) 
      write(6,'(a,a)')' 6: Lines with any phase       ',delete(delal+1) 
      write(6,'(a,a)')' 7: Lines with fp solutions    ',delete(delfps+1)
  

      read(5,'(a)') text
      if(text.ne.' ') then
         read(text,*) ndel
      else
         goto 40
      endif

      if(ndel.eq.1) then
        if(delp.eq.0)then
          delp=1
        else
          delp=0
        endif
      endif

      if(ndel.eq.2) then
        if(dels.eq.0)then
          dels=1
        else
          dels=0
        endif
      endif

      if(ndel.eq.3) then
        if(delsp.eq.0)then
          delsp=1
        else
          delsp=0
        endif
      endif

      if(ndel.eq.4) then
        if(delml.eq.0)then
          delml=1
        else
          delml=0
        endif
      endif

      if(ndel.eq.5) then
        if(delia.eq.0)then
          delia=1
        else
          delia=0
        endif
      endif
      
      if(ndel.eq.6) then
        if(delal.eq.0)then
          delal=1
        else
          delal=0
        endif
      endif

      if(ndel.eq.7) then
        if(delfps.eq.0)then
          delfps=1
        else
          delfps=0
        endif
      endif

      if(ndel.eq.99) then
         open(7,file='dels_amp.out',status='unknown')
         dels_amp=.true.
         write(6,*)
         write(6,*)'Write out IAML amplitudes, Z, N, E'
         write(6,*)'Only write out if all 3 are present'
         write(6,*)
      endif

      if(ndel.gt.0) goto 12 
 
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
c - remove lines with waveform file names
c----------------------------------------------------
c
c      rea_nwav=0
c
c
c   special option, write out amplitudes IAML grouped by station
c
      if(dels_amp) then
         do i=1,rea_nphase
            amp(1)=0
            amp(2)=0
            amp(3)=0

            if(rea_phase(i)(1:4).eq.'IAML'.and.rea_amp(i).ne.-999.0) 
     *      then
              if(rea_co(i)(2:2).eq.'Z') amp(1)=rea_amp(i) 
              if(rea_co(i)(2:2).eq.'N') amp(2)=rea_amp(i)                    
              if(rea_co(i)(2:2).eq.'E') amp(3)=rea_amp(i)

              do k=i+1,rea_nphase
                if(rea_phase(k)(1:4).eq.'IAML'.
     *          and.rea_stat(k).eq.rea_stat(i).and.rea_amp(k).ne.-999.0) 
     *          then
                   if(rea_co(k)(2:2).eq.'Z') amp(1)=rea_amp(k) 
                   if(rea_co(k)(2:2).eq.'N') amp(2)=rea_amp(k)                    
                   if(rea_co(k)(2:2).eq.'E') amp(3)=rea_amp(k)
                   rea_amp(k)=-999.0   ! signal already used
                endif 
              enddo
c
c   only write out if all 3 amplitudes are there
c
              if(amp(1).ne.0.0.and.amp(2).ne.0.0.and.amp(3).ne.0.0)then
                 write(6,'(a5,1x,3f9.1)') rea_stat(i),amp
                 write(7,'(a5,1x,3f9.1)') rea_stat(i),amp
              endif
            endif
         enddo
      endif                    
c
c   delete all  phases
c----------------------------------------------------
c
      if(delal.eq.1) then
         nd=nd+rea_nphase
         rea_nphase=0
         goto 100
      endif 
         
c
c   delete p-phases
c----------------------------------------------------
c
      if(delp.eq.1) then
         do i=1,rea_nphase
           if(rea_phase(i)(1:1).eq.'P') then
             rea_phase(i)(1:6)='DELETE'
             nd=nd+1
           endif
         enddo
      endif
c
c   delete s-phases
c----------------------------------------------------
c
      if(dels.eq.1) then
         do i=1,rea_nphase
           if(rea_phase(i)(1:1).eq.'S') then
              rea_phase(i)(1:6)='DELETE'
              nd=nd+1
           endif
         enddo
      endif

c
c   delete spec-phases and av spec limes
c----------------------------------------------------
c
      if(delsp.eq.1) then
         do i=1,rea_nphase
           if(rea_phase(i)(1:4).eq.'SPEC') then
              rea_phase(i)(1:6)='DELETE'
              nd=nd+2
           endif
         enddo
         rea_av_moment=0.0
         rea_av_omega0=0.0
         nd=nd+1
         k=0
         do i=1,rea_ncomment
           if(rea_comment(i)(2:8).ne.'SPEC SD') then
             k=k+1    ! SPEC SD not in rea common block
             rea_comment(k)=rea_comment(i)
           endif
         enddo
         nd=nd+(rea_ncomment-k)
         rea_ncomment=k
      endif

c
c   delete IAML-phases
c----------------------------------------------------
c
      if(delml.eq.1) then
         do i=1,rea_nphase
           if(rea_phase(i)(1:4).eq.'IAML') then
              rea_phase(i)(1:6)='DELETE'
              nd=nd+1
           endif
         enddo
      endif

c
c   delete all iasp phases
c----------------------------------------------------
c
      if(delia.eq.1) then
         do i=1,rea_nphase
           if(rea_phase(i)(1:2).eq.'IA'.
     *     or.rea_phase(i)(1:2).eq.'IV') then
              rea_phase(i)(1:6)='DELETE'
              nd=nd+1
           endif
         enddo
      endif

 100  continue    ! here since all phases deleted

c
c   delete fault plane solutions
c----------------------------------------------------
c
      if(delfps.eq.1) then
        nd=nd+rea_nfault
        rea_nfault=0
      endif      

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
      write(6,*) 'Output file name is dels.out'
      if(dels_amp) write(6,*)
     *'Ouptut file with IAML amplitudes in dels_amp.out' 

      stop
      end
