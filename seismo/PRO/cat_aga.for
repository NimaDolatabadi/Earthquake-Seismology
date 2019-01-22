c  This program reorders hypocenter lines in a CAT file acording to agency 
c  either given by a file cat_aga.par or interactively.
c  mt hypocenter lines can optionally also be used
c
c  J Havskov, 1996
c-----------------------------------------------------------------------
c
c   updates
c feb 1999  by jh ---------------   version 7.0 chech --------------------
c                 no changes
c nov 25 2013 jh: also optionally check and move mt hypocenter lines
c
      character*80 data(2000)
      integer nrecord,nstat,nhead,id,nphas
      character*1 exp,type
      character*80 filename 
      character*3 agency(200)! agency to use
      integer nagency        ! number of agencies
      integer i              ! counters etc
      logical compact        ! true if compact file
      character*1 answer
      logical get_mt         ! if true get mt_line
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      get_mt=.false.

c
c   get parameter file if there 
c
         i=1
         open(1,file='cat_aga.par',status='old',err=56)
 49      continue
         read(1,'(a3)',end=56) agency(i)
         i=i+1
         goto 49
 56      continue
         nagency=i-1
         if(nagency.gt.0) then
            write(6,*)' Number of agencies', nagency
         else
            write(6,*)' Enter agencies, one pr line, end with return'
            i=1
 59         continue
            read(5,'(a3)') agency(i)
            i=i+1
            if(agency(i-1).ne.'   ') goto 59
            nagency=i-2
         endif
         if(nagency.le.1) then
            write(6,*)' At least two agencies required'
            stop
         endif
         write(6,*)' Move MT hypocenter line (y/enter=n)'
         read(5,'(a)')answer
         if(answer.eq.'y') get_mt=.true.
c
c   open output file
c
      open(2,file='cat_aga.out',status='unknown')
c
c   loop for reading files --------------------------------------------------
c
 500  continue
      write(6,*)' File name, if no more, return'
      read(5,'(a)') filename
      if(filename(1:5).eq.'     ') goto 999
      open(1,file=filename,status='old',err=5005)
      goto 5006
 5005 continue
      write(6,*)' No such file'
      goto 500
 5006 continue
c
c  find which type of input file
c
      call nortype(1,compact)
      if(compact) then 
         write(6,*)' Input file is compact'
         write(6,*)' File must be a CAT file'
         goto 500
      endif
c
c   loop for reading events
c
  1   continue
c
c   read one event or one header line
c
      call indata(1,nstat,nphas,nhead,nrecord,type,exp,data,id)
         if(nrecord.le.0) then
            close(1)
            goto 500   ! check if more files
         endif
c
c   reorganize hypocenter solutions
c
      call agency_order(data,nhead,agency,nagency,get_mt)
c
c   
      write(2,'(a)')(data(i),i=1,nrecord)
      goto 1
c
c   end of all files
c
 999  continue
      write(6,*)' Output file is cat_aga.out'
      stop
      end
c
c
c
      subroutine agency_order(data,nhead,agency,nagency,get_mt)
c
c     j havskov jan 96
c
c   order hypocenter lines acording to priority given in agency list
c   data,nhead         : the usual stuff
c   agency             : the 3 letter codes
c   nagency            : number of agencues
c
      implicit none
      character*80 data(*)
      character*3  agency(*)
      character*80 temp_data(400)    ! temporary to store headers
      logical      selected (400)    ! indicate if line is selected
      logical get_mt                 ! true if mt lines used
      integer nhead,nagency
      integer i,k,j,nout
c
      nout=0                         ! counter of ouput records
      do i=1,400
         selected(i)=.false.
      enddo
c
c  make sure there is a line type 1 in first line
c
      data(1)(80:80)='1'
c
c  go through line by line of data looking for hypocenter lines, also lines
c
      do i=1,nagency
         do k=1,nhead
            if(data(k)(46:48).eq.agency(i).and..not.selected(k).
     *      and.(data(k)(80:80).eq.'1'.or.(data(k)(80:80).eq.'M'.and.
     *      data(k)(2:3).ne.'MT'.and.get_mt))) then

c
c  if m line delete last part, not according to type 1 and make it type 1
c
               if(data(k)(80:80).eq.'M') then
                  data(k)(71:79)=' '
                  data(k)(80:80)='1'
               endif
               nout=nout+1
               temp_data(nout)=data(k)
               selected(k)=.true.
c
c   move other lines along until next hypocenter line or type 7 line
c   or type 4 line
c
               j=1
 50            continue
               if(data(k+j)(80:80).ne.'7'.and.data(k+j)(80:80).ne.'1' 
     *         .and..not.selected(k+j).and.(k+j).le.nhead)then
                 nout=nout+1
                 temp_data(nout)=data(k+j)
                 selected(k+j)=.true.
                 j=j+1
                 goto 50
               endif
            endif
         enddo
      enddo
c
c   put in what remains in original order
c
      do i=1,nhead
         if(.not.selected(i)) then
            nout=nout+1
            temp_data(nout)=data(i)
         endif
      enddo
c
c   put back in original arrray
c
      do i=1,nhead
        data(i)=temp_data(i)
      enddo
c
      return
      end
    
