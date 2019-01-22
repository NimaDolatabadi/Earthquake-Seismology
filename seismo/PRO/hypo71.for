C--------------------------------------------------------------------
C Define the input and output files for HYPO71 and set the program in
C motion

c
c   changes:
c   
c  feb 3, 2003 jh  : had to comment out program statement on pc
c  2014.03.14 pv : clean up common block:
c               Warning: Padding of 2 bytes required before 'id' in COMMON 'readings'

c      program hypo71
	
      implicit none

      include '../INC/libsei.inc'          ! Library definitions & data defns.
      include '../INC/seidim.inc'          ! array dimensions
      include '../INC/seisan.inc'          ! 

      external sei open,                   ! File open handler.
     &         sei close                   ! & closure.
      integer  code,                       ! Condition.
     &         read1,                      ! Read unit1.
     &         write1                      ! Write unit1.

c---single event file name
      character*80 eventfile
c---phase readings array
      CHARACTER*80 DATA(2500)                                                  
c--- event type, indcator of explosion, indicator of fixing depth  
      character*1 typ,exp
c---agency for magnitude  
      character*3 agency
c---character strings for sorting option
      character*5 old_dist
      character*5 old_stat

c---number of header lines, records, stations in nordic file
      integer nstat,nphase,nhead,nrecord
c---id line indicator
      integer id
c--- 0: data base, 1: single file
      integer base
c---minimum number of stations to locate
      integer minstat
c---minimum number of phases to locate  
      integer minphase
c---number of layers in model
      integer nlayers
c---max number of arrivals in readings file
      integer narriv
      parameter (narriv=2000)
c---arrays for distance sorting option
      integer isort(narriv)
      integer ksort(narriv)
      integer lsort(narriv)

      integer hour,min
c-- program exit status
      integer status
c---counters
      integer i,j,k,l,m

c-- indicator if program is used in connection with EEV in seisan
      logical use_eev
c-- indicator if seisan data base present
      logical seisan
c---location indicator
      logical locate
      logical b_flag
      logical error
      logical output

c---model and location algorithm parameters
      real xnear,xfar,startz,vpvs
      real vel(100), z(100)
c---test parameters for location and magnitude calculation
      real test(200)
c---array containing epicentral distance to each station (used in sorting)
      real dlt(narriv)
      real sec

      common/readings/NSTAT,NPHASE,NHEAD,NRECORD,ID,DATA,TYP,EXP
      common/model/vel,z,nlayers,startz,xnear,xfar,vpvs


C
C    initialise...
C    =============
C
      code   = e_ok$                             ! Local condition.
      read1  = 0                                 ! & file units.
      write1 = 0                                 ! Ditto.
      status=0
      error=.false.
      
c
c   get seisan defaults
c
      call get_seisan_def

c
c   find if program will run from within EEV indicated by the presence of file
c   name in an enviromental variable
c
      call get_env_event(eventfile)

C
C   open data base input single event file and read it
c   set flag for single event nrecord= -1
c
      nrecord = -1
      call sei open( old$,	! Open old file (stop on error).
     &                  ' ',             ! No prompt.
     &                  eventfile, read1,! File & unit.
     &                  b_flag,          ! File exists? (n/a).
     &                  code )           ! Local condition (n/a).
      call indata(read1,nstat,nphase,nhead,nrecord,
     &               typ,exp,data,id)
      call sei close( close$, read1, code ) ! Close file (stop on error).


c
c  if type is E or P, fix depth to 0.0 km
c
      if(data(1)(23:23).eq.'P'.or.data(1)(23:23).eq.'E') then
         data(1)(39:44)='  0.0F'
      endif

      call make_hypo71_input(test,agency,error)
      if (error) then
         write(*,*) 'Error creating input file for HYPO71'
         status=1
         call exit_with_status(status)
      endif

c
c   check if disable location has been set on for this event, in which case
c   event is passed on with no modification to hyp71.out
c
      if(data(1)(45:45).eq.'*') then
         write(6,*)'Event flagged not to be located'
         goto 31                ! go to outputs
      endif

c
c  check if event has enough phases and stations and if it
c  should be located
c     
      minstat=test(79)
      if(minstat.lt.2) minstat=2
      minphase=test(80)
      if(minphase.lt.3) minphase=3

      if(nphase.lt.minphase.or.nstat.lt.minstat) then  
         write(6,291)typ,nstat,nphase
 291     format(1x,'TYPE',1x,a1,2x,'NSTAT=',i2,1x,'NPHASE=',i2,
     1'  NOT LOCATABLE') 
         if(nphase.lt.minphase) write(6,*)' Too few phases'
         if(nstat.lt.minstat) write(6,*) ' Too few stations'
         status=2
         call exit_with_status(status)
      endif
c
c   clear old parameters
c
      call clear_old(data,nhead,nrecord,agency)

      do i=nhead+1,nrecord
         data(i)(61:70)='          '
c
c   keep distance if coda is there, so that magnitude can be calculated
c   in case not enough data for a location
c
         if(data(i)(30:33).eq.'    ') data(i)(71:75)='     '
         data(i)(76:79)='    '

      enddo

c
c call the location routine. This is done using a system call
c since the hypo71 code uses units 5 and 6 for input and output
c The hypo71 code should be in its original form as
c previously used on the vax except it is now a program rather 
c than a subroutine
c

      call systemc("hypo71exe < hypo71.input > hypo71.output",40)

      call update_location(agency,error)
      if (error) then
         write(*,*) 'Error updating location'
         status=3
         call exit_with_status(3)
      endif
      
c
c   clear old magnitudes, except 3.
c
      data(1)(57:72)='                '
      if(data(1)(77:79).eq.agency) data(1)(72:79)=' '
C
c   calculate  magnitudes
C
      output=.false.
      call update_mag(data,nhead,nrecord,agency,test,output)


C
C
c  write normal output file, sort first if event has been located
C

c
c open output file which will contain the updated phase readings
c
      call sei open( unknown$,                ! Open hyp file.
     &               ' ',                     ! No prompt.
     &               'hypo71.out', write1,    ! File & unit.
     &               b_flag,                  ! File exists?.
     &               code )                   ! Condition (n/a).
c
c sort by distance if required
c
      if(test(71).ne.0.0)then
         do i=nhead+1,nrecord-1
            read(data(i)(71:75),'(f5.0)')dlt(i-nhead)
         end do

         call r4sort(nrecord-1-nhead,dlt,isort)
c
c   now sort so the distance sorted phase lines, for each group of
c   as in the original file
c
         old_dist=data(isort(1)+nhead)(71:75)
         old_stat=data(isort(1)+nhead)(2:6)
         k=0
         l=0
         do i=nhead+1,nrecord-1
            l=l+1
            read(data(isort(l)+nhead),'(18x,2i2,1x,f5.2)') hour,min,sec
            if(data(isort(l)+nhead)(71:75).eq.old_dist.and.
     *           data(isort(l)+nhead)(2:6).eq.old_stat) then 
               k=k+1
               lsort(k)=isort(l)
               dlt(k)=sec+min*60+hour*3600
               if(i.ne.nrecord-1) goto 73 ! at the end, always sort what is left
               l=l+1                      ! since this is last value
            endif
c
c   if here, new distance or station or last group
c
            if(k.gt.1) then
               call r4sort(k,dlt,ksort)
               m=1
               do j=l-k,l-1
                  isort(j)=lsort(ksort(m))
                  m=m+1
               enddo 
            endif
            if(i.eq.nrecord-1) goto 73
            old_dist=data(isort(l)+nhead)(71:75)
            old_stat=data(isort(l)+nhead)(2:6)
            k=1                 ! this is the first of the next group
            dlt(k)=sec+min*60+hour*3600
            lsort(k)=isort(l)
 73         continue
         enddo

c
c   write out
c
         do i=1,nhead
            write(write1,'(a80)',iostat=code) data(i)
            call sei code(fort$,code,write1,b_flag) ! process outcome.
         enddo 

         do i=nhead+1,nrecord-1
            write(write1,'(a80)',iostat=code)
     &           data(isort(i-nhead)+nhead)
            call sei code(fort$,code,write1,b_flag) ! process outcome.
         end do          

         write(write1,'(a80)',iostat=code) data(nrecord)
         call sei code(fort$,code,write1,b_flag) ! process outcome.
      else
         write(write1,'(a80)',iostat=code)
     &        (data(i),i=1,nrecord)
         call sei code(fort$,code,write1,b_flag) ! process outcome.
      endif    

c
c close output file
c
      call sei close( close$, write1, code ) ! Close file (stop on error).

c
c create the summary file, hypo71.brief, from hypo71.output file
c this subroutine was copied from the vax with some minor changes.
c
      call make_hypo71_brief(data,nhead,nrecord,error)
      if (error) then
         write(*,*) 'Error creating hypo71.brief'
         status=4
         call exit_with_status(status)
      endif

 31   continue

      if(.not.error) then
         write(*,*) 'HYPO71 completed successfully'
         status=0
         call exit_with_status(status)
      else
         write(*,*) 'Error running HYPO71'
         status=5
         call exit_with_status(status)
      endif

      stop
      end
