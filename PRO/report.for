C                                                                               
C                       
C  Program to make output files with comapct information from a nordic file                                  
c  The program will also make a compact output file where the only change
c  to the header lines, is the magnitude order and selection
C                                                                               
C  J. Havskov  1998
c
c  updates
c  
c  ---------------------------------   version 7.0 ------------------------
c  nov 5 98 by jh : linux logic                                                                             
c  sep 19         : was done wrongly
c  sep 27         : error in clear fps
c  mar 15, 02  jh : add report.inp file
c  aug 29  03     : small spelling mistake
c  jan 10  11  jh : numbered output
c  nov 23  11  jh : change magntudes B to b and S to s, pc and linux now
c                   the same
c  jan 28  16  jh : added macroseismic info and locality

c
      implicit none
      include 'seidim.inc'        ! dimensions
      include 'libsei.inc'
      CHARACTER*80 DATA(max_data) ! data arrays
      character*120 text
      CHARACTER*1 TYPE,EXP                                                      
      integer nstat,nphase,nhead,nrecord
c--- fault plane solution
      character*8 mag_out(3) ! magnitude and agency
      real strike,dip,rake
      character*1 answer
      integer istrike,idip,irake
c---number of arguments
      integer nars
c---files as arguments
      character*80 file(5)
      character*80 choise_file      ! optional file with choises
c---unit for output
      character*80 r(100),tr(100)   ! text for input and output
      character*80 tdata(100)       ! the real data
      integer nr(100),ntr(100)      ! number of characters in above
      integer nrout(100)            ! number of item to print out
      integer nin,nout              ! number of fields in and out
      integer nspec,nevent,nerror,nmc,nml,nmb,nms,nmw,nfault! statistics
      integer n,id,i,l,k,code
c--compact or not
      logical compact
      logical number                ! if true, write out numbered lines
      logical sun,pc,linux   ! computer types


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      b_f_message$=.false.
c
      call computer_type(sun,pc,linux)
c
      nin=30
      nout=0
      nevent=0
      nspec=0
      nfault=0
      nerror=0
      nmc=0
      nml=0
      nmb=0
      nms=0
      nmw=0
c
c  define parameters to use, below is seen how selection string looks
c
c Date TimeE L E LatE LonE DepE F Aga Nsta Rms Gap McA MlA MbA MsA MwA Fp Spec Macro Local
c 1    2   3 4 5 6  7 9  9 10 111213  14   15  16  1718192021222324252627 28   29    30               

      r(1)(1:5)='Date '
      nr(1)=5

      tr(1)(1:9)='Year Date'
      ntr(1)=9
      r(2)(1:4)='Time'
      nr(2)=4
      tr(2)(1:9)='HRMM  Sec'
      ntr(2)=9
      r(3)(1:2)='E '
      nr(3)=2
      tr(3)(1:5)='Error'
      ntr(3)=5
      r(4)(1:2)='L '
      nr(4)=2
      tr(4)(1:1)='L'
      ntr(4)=1      
      r(5)(1:2)='E '
      nr(5)=2
      tr(5)(1:1)='E'
      ntr(5)=1
      r(6)(1:3)='Lat'
      nr(6)=3
      tr(6)(1:7)='Latitud'
      ntr(6)=7
      r(7)(1:2)='E '
      nr(7)=2
      tr(7)(1:5)='Error'
      ntr(7)=5
      r(8)(1:3)='Lon'
      nr(8)=3
      tr(8)(1:8)='Longitud'
      ntr(8)=8
      r(9)(1:2)='E '
      nr(9)=2
      tr(9)(1:5)='Error'
      ntr(9)=5
      r(10)(1:4)='Dep '
      nr(10)=4
      tr(10)(1:5)='Depth'
      ntr(10)=5
      r(11)(1:2)='E '
      nr(11)=2
      tr(11)(1:5)='Error'
      ntr(11)=5
      r(12)(1:2)='F '
      nr(12)=2
      tr(12)(1:2)='FF'
      ntr(12)=2
      r(13)(1:4)='Aga '
      nr(13)=4
      tr(13)(1:3)='AGA'
      ntr(13)=3
      r(14)(1:5)='Nsta '
      nr(14)=5
      tr(14)(1:3)='NST'
      ntr(14)=3
      r(15)(1:4)='Rms '
      nr(15)=4
      tr(15)=' RMS'
      ntr(15)=4
      r(16)(1:4)='Gap '
      nr(16)=4
      tr(16)(1:3)='GAP'
      ntr(16)=3
      r(17)(1:2)='Mc'
      nr(17)=2
      tr(17)(1:4)='  Mc'
      ntr(17)=4
      r(18)(1:2)='A '
      nr(18)=2
      tr(18)(1:3)='   '
      ntr(18)=3
      r(19)(1:2)='Ml'
      nr(19)=2
      tr(19)(1:4)='  Ml'
      ntr(19)=4
      r(20)(1:2)='A '
      nr(20)=2
      tr(20)(1:3)='   '
      ntr(20)=3
      r(21)(1:2)='Mb'
      nr(21)=2
      tr(21)(1:4)='  Mb'
      ntr(21)=4
      r(22)(1:2)='A '
      nr(22)=2
      tr(22)(1:3)='   '
      ntr(22)=3
      r(23)(1:2)='Ms'
      nr(23)=2
      tr(23)(1:4)='  Ms'
      ntr(23)=4
      r(24)(1:2)='A '
      nr(24)=2
      tr(24)(1:3)='   '
      ntr(24)=3
      r(25)(1:2)='Mw'
      nr(25)=2
      tr(25)(1:4)='  Mw'
      ntr(25)=4
      r(26)(1:2)='A '
      nr(26)=2
      tr(26)(1:3)='   '
      ntr(26)=3
      r(27)(1:3)='Fp '
      nr(27)=3
      tr(27)(1:15)='STRIK  DIP RAKE'
      ntr(27)=15
      r(28)(1:20)='Spec '
      nr(28)=5
      tr(28)(1:20)='  Mom Strs   f0    r'
      ntr(28)=20
      nr(29)=5
      r(29)(1:5)='Macro'
      tr(29)(1:5)='Macro'
      ntr(29)=5
      nr(30)=6
      r(30)(1:5)=' Local'
      tr(30)(1:40)='Locality                                '
      ntr(30)=40
c 
c   check if input from file given in argument
c
 1      continue
        call get_arguments(nars,file)
        if(nars.eq.0.or.nars.gt.3) then
           write(6,*)' You must give input file as first argument'
           write(6,*)' Optional 2. argument is file name of choises'
           write(6,*)
     *     ' Optional 2. or 3. argument is -n for numbered output'
           stop
        endif
c
c   open input file name, is first argument
c
        open(1,file=file(1),status='old')
c
c   check that not already a compact file
c
        call nortype(1,compact)
        if(compact) then
           write(6,*)' Input file is compact, only some parameters',
     *               ' can be selected, continue(y/n)'
           read(5,'(a)') answer
           if(answer.eq.'y'.or.answer.eq.'Y') then
              continue
           else
              stop
           endif
        endif

c
c   check if prompt input with file name of input file with choises
c   2. argument
c
      choise_file=' '
      if(nars.gt.1.and.file(2)(1:2).ne.'-n') then
         choise_file=file(2)
         write(6,'(a,a)')' Choises from file: ',choise_file
         open(9,file=choise_file,status='old',err=3334)
         read(9,'(a)') text          ! read help line, not used
         read(9,'(a)') text          ! read choises
         goto 3335
 3334    continue
         write(6,'(a,a)')' File does not exist: ',choise_file
         stop
 3335    continue
         close(9)
      endif
c
c   find if argument for numbering output
c
      number=.false.
      if(nars.eq.2.and.file(2)(1:2).eq.'-n') number=.true.
      if(nars.eq.3.and.file(3)(1:2).eq.'-n') number=.true.
c
c   find out if all choises from promt line and get them if true
c   if choise from input file, skip manual input

c
        if(choise_file.eq.' ') then
        write(6,*)
     *  ' Below is shown parameters which can be chosen for output.'
        write(6,*)
     *  ' A return will chose all, placing any character under a field'
        write(6,*)
     *  ' will chose that parameter in the output. Each field starts'
        write(6,*)
     *  ' with a capital letter and ends within the following blank.'
        write(6,*)
     *  ' The order of the output can be changed by placing a number'
        write(6,*)
     *  ' under the field and fields will be written out in the order'
        write(6,*)
     *  ' of the numbers. E after time, lat, lon and dep are errors,'
        write(6,*)
     *  ' L E is distance and event id s, F is both fix flags and A is'
        write(6,*)
     *  ' agency for magnitude.'
        write(6,*)
     *  ' The following example shows that Mc, Depth(Dep) and Time with'
        write(6,*)
     *  ' error are selected and written out in given order.' 

c
c  other compilers might need the two statements
c
c        if(pc)write(6,'(1x,30a)')(r(i)(1:nr(i)),i=1,nin)
c        if(sun.or.linux)write(6,'(30a)')(r(i)(1:nr(i)),i=1,nin)
         write(6,'(30a)')(r(i)(1:nr(i)),i=1,nin)
        write(6,'(1x,a)')
     *  '     30  45              20                       10'

        write(6,*)

c
c   get choises
c
        do i=1,100
          nrout(i)=0
        enddo
c
c        if(pc)write(6,'(1x,28a)')(r(i)(1:nr(i)),i=1,nin)
c        if(sun.or.linux)write(6,'(28a)')(r(i)(1:nr(i)),i=1,nin)
        write(6,'(30a)')(r(i)(1:nr(i)),i=1,nin)
        read(5,'(a)') text
        endif

        l=1
c
        do i=1,nin
          k=l+nr(i)
           if(text(l:k-1).ne.' ') then
              call sei get values(1,text(l:k-1),code)
              if(code.ne.0) goto 222   ! this was not a number
              n=array$(1)
           else
              n=0          ! no choise for this field
           endif
          goto 223         
 222      continue         ! choise was not a number
          n=i              ! no choise of order
 223      continue
          if(n.ne.0) then
             nout=nout+1
             nrout(n)=i
          endif
          l=k
        enddo
c
c   if nothing is chosen, all are chosen
c
        if(nout.eq.0) then
           do i=1,nin
             nrout(i)=i
           enddo
           nout=nin
        endif
        write(6,*)' Number of output fields ',nout
c
c   put choises in correct order
c
          k=0
          do i=1,100
             if(nrout(i).ne.0) then
                k=k+1
                nrout(k)=nrout(i)
             endif
          enddo
c
c   open output files

        open(2,file='report.out',status='unknown')
        open(3,file='report_n.out',status='unknown')
c
c      write header line
c
        if(number) then
           write(2,'(1x,a4,30(a,1x))') '  N ',
     *     (tr(nrout(i))(1:ntr(nrout(i))),i=1,nout)
        else
           write(2,'(1x,30(a,1x))')(tr(nrout(i))
     *     (1:ntr(nrout(i))),i=1,nout)
        endif
c
c   read and write to end of file
c
      n=0
  10  continue
      if(compact) then
         read(1,'(a)',end=99) data(1)
         nrecord=1
         nhead=1
      else
         CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      endif
      if(nrecord.eq.0) goto 99
c
c   take out data from header line
c
      do i=1,100
         tdata(i)=' '
      enddo
c
      tdata(1)(1:9)=data(1)(2:10)
      tdata(2)(1:9)=data(1)(12:20)
      tdata(4)(1:1)= data(1)(22:22)      
      tdata(5)(1:1)= data(1)(23:23)
      tdata(6)(1:7)= data(1)(24:30)
      tdata(8)(1:8)= data(1)(31:38)
      tdata(10)(1:5)= data(1)(39:43)
      tdata(12)(1:2)= data(1)(44:45)
      tdata(13)(1:3)= data(1)(46:48)      
      tdata(14)(1:3)= data(1)(49:51)  ! nst
      tdata(15)(1:4)= data(1)(52:55)  ! rms
      
c
c  get errors, only from first line encountered
c     
      if(.not.compact) then
      do i=2,nhead
        if(data(i)(80:80).eq.'E') then
           nerror=nerror+1
           tdata(16)(1:5)=data(i)(6:8)
           tdata(3)(1:5)=data(i)(16:20)
           tdata(7)(1:5)=data(i)(26:30)
           tdata(9)(1:5)=data(i)(34:38)
           tdata(11)(1:5)=data(i)(39:43)
           goto 2001
        endif
      enddo
 2001 continue
c
c   spectral parameters, first encounter used
c
cSPEC AVERAGE  MO 13.1 ST  3.1 OM  2.4 f0 4.54 R   .27 AL 2.20 WI 10.5 MW  2.7 3
c
      do i=2,nhead
        if(data(i)(2:13).eq.'SPEC AVERAGE') then
           nspec=nspec+1

           tdata(28)(1:20)=data(i)(18:22)//data(i)(26:30)//
     *                     data(i)(42:46)//data(i)(50:54)
                           
           goto 2002
        endif
      enddo
 2002 continue
c
c   fault plane solution, first encounter used
c
      do i=2,nhead
         if(data(i)(80:80).eq.'F') then
            nfault=nfault+1
            read(data(i),'(3f10.0)') strike,dip,rake
            istrike=strike+0.5
            idip=dip+0.5
            irake=rake+0.5
            write(tdata(27)(1:15),'(3i5)') istrike,idip,irake
            goto 2003
          endif
      enddo
 2003 continue

c
c   intensity, use first encounter
c
      do i=2,nhead
         if(data(i)(80:80).eq.'2') then
            tdata(29)(1:5)=data(i)(28:32)
            goto 2004
          endif
      enddo
 2004 continue

c
c   locality, use first encounter
c
      do i=2,nhead
         if(data(i)(80:80).eq.'3'.and.data(i)(2:10).eq.'LOCALITY:') 
     *   then
            tdata(30)(1:40)=data(i)(12:51)
            goto 2005
          endif
      enddo
 2005 continue
      endif         ! for if compact
c
c   get magnitudes, use first occurence
c
      call get_mag_type('C',data,nhead,tdata(17)(1:5),tdata(18)(1:3))
      if(tdata(17)(1:4).ne.'    ') nmc=nmc+1
      call get_mag_type('L',data,nhead,tdata(19)(1:5),tdata(20)(1:3))
      if(tdata(19)(1:4).ne.'    ') nml=nml+1
      call get_mag_type('b',data,nhead,tdata(21)(1:5),tdata(22)(1:3))
      if(tdata(21)(1:4).ne.'    ') nmb=nmb+1
      call get_mag_type('s',data,nhead,tdata(23)(1:5),tdata(24)(1:3))
      if(tdata(23)(1:4).ne.'    ') nms=nms+1
      call get_mag_type('W',data,nhead,tdata(25)(1:5),tdata(26)(1:3))
      if(tdata(25)(1:4).ne.'    ') nmw=nmw+1
c
c   select for nordic output file
c
      do i=1,3
        mag_out(i)=' '
      enddo

      k=0   ! number of magnitudes out
      do i=1,nout
        if(nrout(i).eq.17.or.nrout(i).eq.19.or.nrout(i).eq.21.
     *   or.nrout(i).eq.23.or.nrout(i).eq.25) then
            k=k+1
            if(k.gt.3) goto 1234
            mag_out(k)=tdata(nrout(i))(1:5)//tdata(nrout(i)+1)(1:3)
        endif
      enddo
 1234 continue
c
      data(1)(56:63)=mag_out(1)
      data(1)(64:71)=mag_out(2)
      data(1)(72:79)=mag_out(3)
c
c   write selected data
c
      nevent=nevent+1
   
      if(number) then
         write(2,'(1x,i3,30(1x,a))') nevent,
     *   (tdata(nrout(i))(1:ntr(nrout(i))),i=1,nout)
         write(3,'(a)')(data(i),i=1,nrecord)
      else
         write(2,'(30(1x,a))') (tdata(nrout(i))
     *   (1:ntr(nrout(i))),i=1,nout)
      endif
c

c
c   next event
c
        goto 10
c
 99     continue
        write(6,*)
        write(6,*)' Number of events                           ',nevent
        write(6,*)' Number of events with spectra:             ',nspec
        write(6,*)' Number of events with fault plane solution:',nfault
        write(6,*)' Number of events with error estimates:     ',nerror
        write(6,*)' Number of events with mc             :     ',nmc
        write(6,*)' Number of events with ml             :     ',nml
        write(6,*)' Number of events with mb             :     ',nmb
        write(6,*)' Number of events with ms             :     ',nms
        write(6,*)' Number of events with mw             :     ',nmw
c
        write(6,*)
        write(6,*)' Output report file is report.out'
        write(6,*)' Output nordic file is report_n.out'
        write(6,*)' Output of choises used in report.inp'
        write(6,*)
c
c   write out choises
c
        open(1,file='report.inp',status='unknown')
        write(1,'(30a)')(r(i)(1:nr(i)),i=1,nin)
        write(1,'(a)') text
        close (1)        
        stop

        end
                       
        subroutine get_mag_type(mag_type,data,nhead,mag,mag_aga)
c
c   get first occurrece of a given magnutde with type C,L,B,S,W
c   return magnitude and agency in two text strings
c       
        character*80 data(*)
        character*1 mag_type
        character*5 mag
        character*3 mag_aga
        integer nhead        ! number of header lines
        integer i,k
c
        mag=' '
        mag_aga=' '
c
c   search header lines 
c
        k=0
        do i=1,nhead
           if(data(i)(80:80).eq.'1') then
              if(data(i)(60:60).eq.mag_type) then
                 mag=data(i)(56:60)
                 mag_aga=data(i)(61:63)
                 return
              endif
              if(data(i)(68:68).eq.mag_type) then
                 mag=data(i)(64:68)
                 mag_aga=data(i)(69:71)
                 return
              endif
              if(data(i)(76:76).eq.mag_type) then
                 mag=data(i)(72:76)
                 mag_aga=data(i)(77:79)
                 return
              endif
           endif
        enddo
        return
        end
