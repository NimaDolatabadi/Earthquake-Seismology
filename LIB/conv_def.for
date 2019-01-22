c  april 27 99 jh : also possible to have def file in dat
c  may23          :bug
c  march 05 01 lo : close def file before return
c  march 23 01 jh : but only if it had been opened, else crash
c  may    7 03 lo : add reading routine for time definition file
c  nov      11 jh : increase dim from 200 to 2000
      subroutine read_def_chan(def_file,mainhead_text,net_code)
c
c   read the channel definition file for conversion programs
c   the output is given in the common block
c
c   j havskov, march 99
c
      implicit none
      include 'libsei.inc'
      external  sei open,                      ! Open file handler.
     &          sei close,                     ! Close file handler.
     &          sei code                       ! Error handler.
      character*5 def_in_stat(2000)  ! input station code in def file
      character*4 def_in_comp(2000)  ! input component code in def file
      character*5 def_out_stat(2000) ! output station code in def file
      character*4 def_out_comp(2000) ! output component code in def file
      integer   def_chan_number(2000)! channel number of current unit
      character*29 mainhead_text    ! waveform header info
      character*5  net_code         ! network code for file name
      integer ndef_chan             ! number of channels defined
      character*80 def_file         ! name of def file
      integer in                    ! file unit
      logical exist                 ! true if def file exists
      integer code                  ! return code from opening def file
      integer i
      common /def_chan/def_in_stat,def_in_comp,def_out_stat,
     *                 def_out_comp,def_chan_number,ndef_chan
c
      mainhead_text=' '
      net_code=' '
c
c   get def file for station codes
c
      ndef_chan=1	   	   	  	  	  
c                                                                               
      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   def_file )       ! For this filename.

c
      if(code.ne.e_ok$) goto 50
      goto 49                  
 50   continue
      i=index(def_file,' ')-1
      write(6,'(a,a,a)')
     *' No ',def_file(1:i),
     *' file, will use internal information for channel codes'
      return
c      goto 51    ! mar 2001  jh
c
c  read def file
c	  
 49   continue
c-- read help line
      read(in,'(a)',end=51,err=60) i  
c-- header info line	  
      read(in,'(a29,1x,a5)',end=51,err=60) mainhead_text,net_code
c-- comment help line
      read(in,'(a)',end=51,err=60) i
c-- codes	  
 53   continue
      read(in,'(i5,1x,a5,2x,a4,1x,a5,2x,a4)',end=51,err=60)
     *def_chan_number(ndef_chan),
     *def_in_stat (ndef_chan),def_in_comp (ndef_chan),
     *def_out_stat(ndef_chan),def_out_comp(ndef_chan)
     
	  ndef_chan=ndef_chan+1
      goto 53
c
c   end of def file
c
 51   continue
      ndef_chan=ndef_chan-1 
      if(ndef_chan.gt.0) then
cc        write(6,*)' Number of channel codes in def file',
cc     *   ndef_chan
cc         write(6,*)
      endif		 		 
c
      goto 99
c
 60   continue
      write(6,*)' Error in def file'
      ndef_chan=0
c
 99   continue
c added close 05 March, 2001
      call sei close(close$, in, code)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_def_time(def_file,ntime)
c
c   read the time definition file for conversion programs
c   the output is given in a common block
c
c   L Ottemoller, January 2002
c
      implicit none
      include 'libsei.inc'
      include 'seidim.inc'
      external  sei open,                      ! Open file handler.
     &          sei close,                     ! Close file handler.
     &          sei code                       ! Error handler.
      character*5 def_time_stat(max_phase)  ! input station code in def file
      character*4 def_time_comp(max_phase)  ! input component code in def file
      double precision def_time_start(max_phase),def_time_end(max_phase)
                                               ! time interval for which to 
                                               ! apply correction
      double precision def_time_correction(max_phase) ! time correction
      integer ndef_time_chan             ! number of channels defined
      character*80 def_file         ! name of def file
      character*80 text             ! text
      integer in                    ! file unit
      logical exist                 ! true if def file exists
      integer code                  ! return code from opening def file
      character*14 starttime,endtime! time as text
      integer year,month,day,hour,min
      real sec
      double precision msec
      integer i,ntime
      common /def_time/def_time_stat,def_time_comp,
     *                 def_time_start,def_time_end,
     *                 def_time_correction,ndef_time_chan
c
c   get time correction for individual channels
c
      ndef_time_chan=1	   	   	  	  	  
      ntime=0

      call sei get file( open$+ignore$,   ! Open waveform file.
     &                   in,              ! On unit.
     &                   code,            ! Returned condition.
     &                   'DAT',           ! Alternative search directory.
     &                   def_file )       ! For this filename.

      if(code.ne.e_ok$) goto 50
      goto 49                  
 50   continue
      i=index(def_file,' ')-1
      write(6,'(a,a,a)')
     *' No ',def_file(1:i),
     *' file for individual channel time correction'
      return
c
c  read def file
c	  
 49   continue
c-- read the 4 header lines
      do i=1,4
        read(in,'(a)',end=51,err=60) text
      enddo

 53   continue

      read(in,'(a5,1x,a4,1x,a14,1x,a14,f20.3)',end=51,err=60)
     &  def_time_stat(ndef_time_chan),def_time_comp(ndef_time_chan), 
     &  starttime,endtime,def_time_correction(ndef_time_chan)

      if (def_time_stat(ndef_time_chan)(1:1).eq.'#') goto 53
   
c
c convert time into seconds
c
      if (starttime.ne.'              ') then
        read(starttime,'(i4,4i2,i2.0)') year,month,day,hour,min,sec
        call timsec(year,month,day,hour,min,sec,msec)
        def_time_start(ndef_time_chan)=msec
      else
        def_time_start(ndef_time_chan)=0.
      endif
      if (endtime.ne.'              ') then
        read(endtime,'(i4,4i2,i2.0)') year,month,day,hour,min,sec
        call timsec(year,month,day,hour,min,sec,msec)
        def_time_end(ndef_time_chan)=msec
      else
        def_time_end(ndef_time_chan)=0.
      endif

      if (ndef_time_chan.le.max_phase.and.
     &    def_time_stat(ndef_time_chan).ne.'     '.and.
     &    def_time_stat(ndef_time_chan).ne.'    '.and.
     &    def_time_correction(ndef_time_chan).ne.0.) then
        ndef_time_chan=ndef_time_chan+1
      endif
      goto 53
c
c   end of def file
c
 51   continue
      ndef_time_chan=ndef_time_chan-1 

      goto 99
 60   continue
      write(6,*)' Error in def file'
      ndef_time_chan=0

 99   continue

      call sei close(close$, in, code)
      ntime=ndef_time_chan
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccc


      subroutine set_def_chan(ichan,stat,comp)
c
c  set channel codes according to def file, if no valid definition is 
c  found, stat and chan are returned unchanged
c
c  principle of conversion in order of precedence:
c
c  both station and component given on input: Converted to what is given
c               for output station and componenet. If both are not present
c               an erro message is given an dno conversion is done
c
c  Only station given in input: The station code is converted to what
c               is given for output station. The component is not changed
c               even if an output component is given. If no output station
c               is given, no conversion is made and an error message is given.
c
c  Only component given in input: The component code is converted to what
c               is given for output componenet. The station is not changed
c               even if an output station is given. If no output componenet
c               is given, no conversion is made and an error message is given.
c
c  No input station or componenet given, only channel number given. The output
c               will get the output station and componenet given for output.
c               If the output station and component is not defined, an error
c               message will be given and no conversion made.
c
c  No definition in def file: No conversion made and a message given.
c  
      implicit none
      character*5 stat              ! station code
      character*4  comp             ! component code
      character*5 def_in_stat(2000)  ! input station code in def file
      character*4 def_in_comp(2000)  ! input component code in def file
      character*5 def_out_stat(2000) ! output station code in def file
      character*4 def_out_comp(2000) ! output component code in def file
      integer   def_chan_number(2000)! channel number of current unit
      integer ndef_chan             ! number of channels defined
      integer ichan                 ! input channel number
      integer i,kdef
      common /def_chan/def_in_stat,def_in_comp,def_out_stat,
     *                 def_out_comp,def_chan_number,ndef_chan
c
c if no defaults, return
c
c
      if(ndef_chan.eq.0) return
c   
c
c   first check if given station and component are defined in input definition,
c   if so use that for the output, overide all other definitions
c

      do i=1,ndef_chan
        if(stat.eq.def_in_stat(i).and.comp.eq.def_in_comp(i)) then
           if(def_out_stat(i).eq.' '.or.def_out_comp(i).eq.' ') then
              write(6,'(a,a,1x,a)') 
     *        ' Missing output definition for input ',
     *        def_in_stat(i),def_in_comp(i)
              write(6,*)' No conversion made of codes for this channel'
           else
              comp=def_out_comp(i)
              stat=def_out_stat(i)
           endif
           return
        endif
      enddo
c
c    next case is if input station is given but component is blank,
c    then the station is changed but the componenet remain unless 
c    component code is set for output
c

      do i=1,ndef_chan
        if(stat.eq.def_in_stat(i).and.def_in_comp(i).eq.' ') then
           if(def_out_stat(i).eq.' ') then
              write(6,'(a,a)') 
     *        ' Missing output definition for station, input is ',
     *        def_in_stat(i)
              write(6,*)' No conversion made of codes for this channel'
           else
              stat=def_out_stat(i)
           endif
           return
        endif
      enddo
c
c    next case is if input component is given but station is blank,
c    then the component is changed but the station remains
c

      do i=1,ndef_chan
        if(comp.eq.def_in_comp(i).and.def_in_stat(i).eq.' ') then
           if(def_out_comp(i).eq.' ') then
              write(6,'(a,a)') 
     *        ' Missing componenet output definition for input ',
     *        def_in_comp(i)
              write(6,*)' No conversion made of codes for this channel'
           else
              comp=def_out_comp(i)
           endif
           return
        endif
      enddo

c
c   if definition input is blank for both station and component,
c   and the output definition exist for that channel, use that
c   irrespective of what was defined from before
c
c   first find if channel number is given
c
      kdef=0
      do i=1,ndef_chan
        if(ichan.eq.def_chan_number(i)) kdef=i
      enddo
c
      if(kdef.gt.0) then
         if( def_in_stat (kdef).eq.' '.and.def_in_comp (kdef).eq.' '.
     *   and.def_out_stat(kdef).ne.' '.and.def_out_comp(kdef).ne.' ') 
     *   then
            stat=def_out_stat(kdef)
            comp=def_out_comp(kdef)
            return
         endif
      endif
c
c   if here, channel was not defined in def file
c
      write(6,'(a,i3,1x,a5,1x,a4)') 
     *' Channel # and name not defined in def file: ',
     *              ichan,stat,comp
c
      return
      end


      subroutine set_time_chan(stat,comp,time,correction)
      implicit none
      include 'seidim.inc'
c
c find individual time correction for given channel
c
      character*(*) stat,comp
      double precision time,correction
      integer i
      character*5 def_time_stat(max_phase)  ! input station code in def file
      character*4 def_time_comp(max_phase)  ! input component code in def file
      double precision def_time_start(max_phase),def_time_end(max_phase)
                                               ! time interval for which to 
                                               ! apply correction
      double precision def_time_correction(max_phase) ! time correction
      integer ndef_time_chan             ! number of channels defined
      common /def_time/def_time_stat,def_time_comp,
     *                 def_time_start,def_time_end,
     *                 def_time_correction,ndef_time_chan

      correction=0.

      do i=1,ndef_time_chan
        if (stat.eq.def_time_stat(i).and.
     &      comp.eq.def_time_comp(i)) then
          if ((time.ge.def_time_start(i).and.
     &         time.lt.def_time_end(i)).or. 
     &        (time.ge.def_time_start(i).and.
     &         def_time_end(i).eq.0.).or. 
     &        (time.lt.def_time_end(i).and.
     &         def_time_start(i).eq.0.).or. 
     &        (def_time_start(i).eq.0..and.
     &         def_time_end(i).eq.0.)) then 

            if (correction.eq.0.) then
              correction=def_time_correction(i)
            else
              write(*,*) ' Time correction defined more than once! '
            endif
          endif
        endif
      enddo

      return 
      end


