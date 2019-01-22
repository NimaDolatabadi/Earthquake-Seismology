C                                                                               
C                       
C  Program to make a compact file from a Nordic file                                  
c  Earlier name of program was compact
C                                                                               
C  J. Havskov  1990 ?
c
c  updates
c  feb 15, 96 jh: copy magnitudes from following lines
c
c  nov  9  98 :-------------   version 7.0 check ------------------------
c              no change                                                                               
c
      implicit none
      include 'seidim.inc'        ! dimensions
      CHARACTER*80 DATA(max_data)                                                    
      CHARACTER*1 TYPE,EXP                                                      
      logical mag                 ! if true, get magnitudes from other lines
      integer nstat,nphase,nhead,nrecord
c---number of arguments
      integer nars
c---files as arguments
      character*80 file(5)
c---unit for output
      integer out
      integer n,id
c--compact or not
      logical compact


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

       mag=.false.

c 
c   check if input from file given in argument
c
 1      continue
        call get_arguments(nars,file)
        if(nars.eq.0.or.nars.gt.3) then
           write(6,*)' You must give arguments, first is input file,'
           write(6,*)' optional second is output file, if an optional'
           write(6,*)' second or 3. is -mag, magnitudes from following'
           write(6,*)' header lines are moved up to empty magnitude'
           write(6,*)' spaces on first line'
           stop
        endif
c
c   case with output on standard output
c
        if(nars.eq.1) then
          out=6
        endif
c
c   output in file or magnitude copy
c
        if(nars.eq.2) then
          if(file(2)(1:4).eq.'-mag') then 
             mag=.true.
             out=6
          else
             out=2
             open(2,file=file(2),status='unknown')
          endif
        endif
c
c   output in file and magnitude copy
c
        if(nars.eq.3.and.
     *    (file(2)(1:4).ne.'-mag'.and.file(3)(1:4).ne.'-mag')) then
           nars=0
           goto 1
        endif
c
        if(nars.eq.3) then
          if(file(2)(1:4).eq.'-mag') then 
             mag=.true.
             out=2
             open(2,file=file(3),status='unknown')
          endif
          if(file(3)(1:4).eq.'-mag') then 
             mag=.true.
             out=2
             open(2,file=file(2),status='unknown')
          endif
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
           write(6,*)' Input file alredy compact'
           stop
        endif
c
c   read and write to end of file
c
        n=0
  10    continue
        CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
        if(nrecord.eq.0) goto 99
        if(mag) call get_mag(data,nhead)
        n=n+1
        write(out,'(a)')data(1)
        goto 10
c
 99     continue
        write(6,*)
        write(6,*)' The input file had ',n,' events'
        stop
        end
                       
        subroutine get_mag(data,nhead)
c
c  if not all positions occupied with magnitudes in first line, move
c  up magnitudes from following header lines. take in order they
c  appear.
c
c       
        character*80 data(*)
        integer nhead        ! number of header lines
        character*7 magtext(3)  ! magnitude with type and agency
        integer i,k

        if(nhead.lt.2) return      ! only one header line, nothing to find
c
c   find number of available magnitude positions 
c
        nfree=0
        if(data(1)(57:59).eq.'   ') then
           nfree=nfree+1
        endif
        if(data(1)(65:67).eq.'   ') then
           nfree=nfree+1
        endif
        if(data(1)(73:75).eq.'   ') then
           nfree=nfree+1
        endif
c
        if(nfree.eq.0) return     ! no room for more
c
c   search following header lines 
c
        k=0
        do i=2,nhead
           if(data(i)(80:80).eq.'1') then
              if(data(i)(57:59).ne.'   ') then
                 k=k+1
                 nfree=nfree-1
                 magtext(k)=data(i)(57:63)
                 if(nfree.eq.0) goto 10
              endif
              if(data(i)(65:67).ne.'   ') then
                 k=k+1
                 nfree=nfree-1
                 magtext(k)=data(i)(65:71)
                 if(nfree.eq.0) goto 10
              endif
              if(data(i)(73:75).ne.'   ') then
                 k=k+1
                 nfree=nfree-1
                 magtext(k)=data(i)(73:79)
                 if(nfree.eq.0) goto 10
              endif
           endif
        enddo
c
c   now put in magnitudes found
c
 10     continue
        if(k.eq.0) return   ! no new magnitudes
        i=1
        if(data(1)(57:59).eq.'   ') then
           data(1)(57:63)=magtext(i)
           i=i+1
           if(i.gt.k) goto 20             ! no more magnitudes
        endif
        if(data(1)(65:67).eq.'   ') then
           data(1)(65:71)=magtext(i)
           i=i+1
           if(i.gt.k) goto 20             ! no more magnitudes
        endif
        if(data(1)(73:75).eq.'   ') then
           data(1)(73:79)=magtext(i)
           i=i+1
           if(i.gt.k) goto 20             ! no more magnitudes
        endif
c
 20     continue
        return
        end
