c
c   program to run hypoinverse
c
c   input and output is an sfile. D and R events are skipped but
c   written out again. Most parameters not used by hypoinverse are
c   kept
c
c   jh october 2014
c

      implicit none
      CHARACTER*80 DATA1(2500),data2(2500),data3(2500)                                                  
      CHARACTER*80 infile                                                                                  
      CHARACTER*1 TYPE,EXP  
c-- arguments
      character*80 args(10)    
c---number of arguments 
      integer nars
c---time of making update
      character*12 p_time
      character*14 proc_time
      character*4 operator
      integer nhyp,norg       ! number of events located by hypoinverse
                              ! number of events in input cat file
      character*80 text                                                       
      integer nstat1,nstat2,nstat3,nphase1,nphase2,nhead1,nhead2,
     *nhead3,nrecord1,nrecord2,nrecord3
      integer i,n,id1,id2,seiclen

c
c    delete old input  output files
c
         open(111,file='norhin.out',status='unknown')
         close(111,status='delete')
         open(111,file='hinnor.out',status='unknown')
         close(111,status='delete')
         open(111,file='hypinv.out',status='unknown')
         close(111,status='delete')
         open(111,file='print.out',status='unknown')
         close(111,status='delete')
         open(111,file='hypinv_seisan.out',status='unknown')
         close(111,status='delete')
c
c


      infile=' '
c
c   check if input from file given in argument
c
      call get_arguments(nars,args)
      if(nars.gt.0) then
         infile=args(1)
         operator=args(2)
      endif 

c
c   get input file name, check if exist
c

 9    continue
      if(infile.eq.' ') then
         write(6,*) 'Give input file'
         read(5,'(a)') infile
 22      continue
         write(6,*) 'Give operator'
         read(5,'(a)') operator
         if(operator.eq.' ') goto 22
      endif
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      infile=' '
      goto 9
 11   continue

c
c  input file ok, now make parmeter files for hypoinverse
c
      call systemc("makehin",7)
c
c     convert data to hypoinverse
c
      call systemc('norhin '//infile(1:seiclen(infile)),
     *seiclen(infile)+7)
c
c   run hypoinverse
c
      call systemc('hypinv',6)
c
c   convert data back to nordic, first check if any location 
c  
 30   continue
      open(10,file='hypinv.out',status='old')
      read(10,'(a)',end=31) text
      i=i+1
      goto 30
 31   continue
      if(i.lt.2) then
          write(6,*)' No hypoinverse solution'
          stop
      endif
c
      call systemc('hinnor hypinv.out',17)
c  
c   open converted file
c
     
      open(2,file='hinnor.out',status='old')
c
c  open output file
c
      open(3,file='hypinv_seisan.out',status='unknown') 
c     
c   read and write to end of input files and create a new nordic 
c   file with all info
c
     

c   read hypoinverse event in nordic format

 1    continue
      CALL INDATA(2,NSTAT2,NPHASE2,NHEAD2,NRECORD2,TYPE,EXP,DATA2,ID2)
      if(nrecord2.eq.0) goto 2
      nhyp=nhyp+1
c
c   read one original event in nordic format 
c
 2    continue
      CALL INDATA(1,NSTAT1,NPHASE1,NHEAD1,NRECORD1,TYPE,EXP,DATA1,ID1)
      if(nrecord1.eq.0) goto 999
      norg=norg+1
c
c   check if the events correspond
c
      if(data1(id1)(61:74).eq.data2(id2)(61:74)) then
c
c   update id line
c
       call systime(p_time,proc_time)
       WRITE(data1(id1)(13:26),'(A)')PROC_TIME
       data1(id1)(9:11)='HIN'
       data1(id1)(31:35)=operator
c
c  merge files if they correspond
c
       call merge_hypoinverse
     *   (data1,data2,data3,nhead1,nhead2,nhead3,
     *    nrecord1,nrecord2,nrecord3)
 
         do i=1,nrecord3
            write(3,'(a)') data3(i)
         enddo
c
c   get next hypoinverse and original
c
         goto 1
c
c  only write original, no corresponding hypoinverse event
c
      else  
         do i=1,nrecord1
            write(3,'(a)') data1(i)
         enddo
c
c   get next original to see if if it corresponds
c
         goto 2
      endif

 999  continue
      write(6,'(a,i5,a,i5)')'Original # ev ',norg,
     *' Hypoinverse # ev',nhyp
      write(6,*) 'Output file is hypinv_seisan.out'
      stop
      end      

      subroutine merge_hypoinverse
     *(data1,data2,data3,nhead1,nhead2,nhead3,nrecord1,
     *nrecord2,nrecord3)

c
c   merge hypoinverse nordic and original nordic
c
c  the data form original which i snot in hypoinverse is 
c  is put in. all original magnitudes are used, original agency
c  fixed flags
c  data1 is original data, data2 is hypoinverse and data3 is merged
c
     
      implicit none
      CHARACTER*80 DATA1(*),data2(*),data3(*) 
      logical err                    ! true if error line                                                                                                                                                                     
      integer nhead1,nhead2,
     *nhead3,nrecord1,nrecord2,nrecord3
      integer i,norg,nhyp

c
c   main header, keep old magnitudes and agency
c
     
      data3(1)=data1(1)
c
c   put in new hypocenter and origin time
c
      data3(1)(1:43)= data2(1)(1:43)
c
c   new rms
c
      data3(1)(52:55)=data2(1)(52:55)
c
c  keep model, type etc
c
      data3(1)(21:23)=data1(1)(21:23)
c
c   old fixed flags
c 
      data3(1)(44:45)=data1(1)(44:45)
c
c   put in remaining headers, replace error line
c
      err=.false.
      do i=2,nhead1
         data3(i)=data1(i)
         if(data1(i)(80:80).eq.'E') then    ! new error line
            data3(i)=data2(2) 
            err=.true.   
         endif
      enddo
      nrecord3=nhead1
c
c   check if error line
c
      if(.not.err) then
         do i=nrecord3,2,-1
           data3(i+1)=data3(i)
         enddo
         data3(2)=data2(2)
      endif
c
c   update id line

c
c   get phases
c

c
c   start with hypoinverse phases
c
      do i=nhead2+1,nrecord2-1
         nrecord3=nrecord3+1
         data3(nrecord3)=data2(i)
      enddo
c
c   get the ones which are not P or S phases
c
      do i=nhead1+1,nrecord1
         if(data1(i)(11:11).ne.'P'.and.data1(i)(11:11).ne.'S') then
            nrecord3=nrecord3+1
            data3(nrecord3)=data1(i)
         endif
      enddo



      return
      end