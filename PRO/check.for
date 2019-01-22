c
c    program to check total content of seisan distribution
c
c    the input file with all info is in seisan.all and is located in the
c    seisan INF directory
c
c    the program can be operated from any directory
c
c changes:
c   August 9, 2001  : add SUP directory
c   May 8, 2003     : small change in questions, used known top dir
c   2010        ??  : also check for REA, CAL and WAV data files
c   feb 11 2011 jh  : all check for compiled c-files
c   sep 08 2014 jh  : also check for subdirs of main dir, see seisan.all
c   mar 9  2014 jh  : two rea test events was check in data base, not 
c                     as it should in REA/REA
c

      implicit none
      character*80 text(1000)   ! whole file
      character*80 subdir
      integer nsubdir
      character*1  dchar        ! dir char
      character*80 top_dir      ! seisan top directory
      integer      nt           ! length of top_dir
      character*80 file         ! input file
      integer      nline        ! number fo lines in input file
      logical      exist        ! true if a file exists
      character*5  dirc         ! verison directory
      integer      nfile_ex_err ! files not found
      integer      nfile_co_err ! compiled files not found
      integer      nfound       ! number of files found
      character*3  dir          ! seisan directory
      character*1  basic_check,comp
      logical      unix,pc      ! system checked
      integer      seiclen
      integer      i,k,l   


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c
      nfile_ex_err=0
      nfile_co_err=0
      nfound=0
      unix=.false.
      pc=.false.
c
      call dir_char(dchar)
c
c    
      call topdir(text(1))
      write(6,*) ' Top directory is defined as: ',text(1)
      write(6,*) ' If not correct,'
      write(6,*) ' Give SEISAN top directory, e.g. /net/seismo/'
      write(6,*) ' If ok, return'
      read(5,'(a)') text(2)
      if(text(2).eq.' ') then
         top_dir=text(1)(1:seiclen(text(1)))//dchar
      else
         top_dir=text(2)
      endif 
      write(6,*)' Basic single platform (b) or '
      write(6,*)
     *' check full original distribution of all platforms (d) ?'
      read(5,'(a)') basic_check
      if(basic_check.eq.'b') then
 88     continue
        write(6,*) 'Unix or PC (u/p)'
        read(5,'(a1)') comp
        if(comp.ne.'u'.and.comp.ne.'p') goto 88
        if(comp.eq.'u') unix=.true.
        if(comp.eq.'p') pc=.true.
      endif
c
c   open and read file
c
      nt=index(top_dir,' ')
      nt=nt-1
      file=top_dir(1:nt)//'INF/seisan.all'
      open(1,file=file,status='old',err=10)
      goto 11
 10   continue
         write(6,'(a,a,a)')' File ',file(1:nt+10),' does not exist'
         stop
 11   continue
c
      nline=1
 12   continue 
      read(1,'(a)',end=13) text(nline)
      nline=nline+1
      goto 12
c
c   end of file
c
 13   continue
      close(1)
      nline=nline-1
      write(6,*)' Number of lines in seisan.all file ', nline
c
c   open output error file
c
      open(2,file='check.out',status='unknown')
      open(3,file='check_ok.out',status='unknown')
c
c   first check existance of all files
c
      dir=' '
      do i=1,nline
         if(text(i)(1:3).eq.'PRO'.or.text(i)(1:3).eq.'LIB'.or.
     *      text(i)(1:3).eq.'INC'.or.text(i)(1:3).eq.'DAT'.or.
     *      text(i)(1:3).eq.'COM'.or.text(i)(1:3).eq.'INF'.or.
     *      text(i)(1:3).eq.'PIC'.or.text(i)(1:3).eq.'SUP'.or.  
     *      text(i)(1:3).eq.'WAV'.or.text(i)(1:3).eq.'REA'.or.
     *      text(i)(1:3).eq.'CAL') then
            dir=text(i)(1:3)
         endif
c
c   extract file name
c
         if(text(i)(3:3).eq.' '.and.text(i)(1:14).ne.' '.
     *      and.dir.ne.'   ') 
     *      then
               k=index(text(i)(4:80),' ')
               dirc=' '
               if(text(i)(1:1).eq.'p') dirc='PCWIN'
               if(text(i)(1:1).eq.'L') dirc='LIN64'
               if(text(i)(1:1).eq.'s') dirc='SOLAR'
               if(text(i)(1:1).eq.'l') dirc='LINUX'
               if(basic_check.eq.'b') then
                  if( dirc.eq.'PCWIN'.and..not.pc) goto 200 ! file not there
                  if((dirc.eq.'SOLAR'.or.dirc.eq.'LIN64'.or.dirc.
     *                   eq.'LINUX'.or.text(i)(1:1).eq.'u').
     *                   and..not.unix) 
     *                   goto 200 ! file should not be there
                  dirc=' '  ! basic check of only main dirs
               endif
                  
               if(dirc.eq.' ') then
                  if(dir.eq.'DAT'.and.basic_check.ne.'b') then   ! if DAT, one more DAT level
                     file=top_dir(1:nt)//dir//'/'//dir//'/'//
     *               text(i)(4:k+2)
                     l=nt+3+1+k-1+4
                  else
                     if(dir.eq.'REA') then
                        if(basic_check.eq.'b') then
                          file=top_dir(1:nt)//dir//'/'//
     *                    '/TEST_/1996/06/'//text(i)(4:k+2)
                          l=nt+3+1+k-1+15
                        else
                           file=top_dir(1:nt)//dir//'/'//
     *                     '/REA/'//text(i)(4:k+2)
                           l=nt+3+1+k-1+10
                        endif
                     else
c
c   test if a subdir
c
                        call find_subdir(text(i),subdir,nsubdir)
                        if(nsubdir.ne.0) then
                           file=top_dir(1:nt)//dir//'/'//
     *                     subdir(1:nsubdir)//'/'//text(i)(4:k+2)
                           l=nt+3+1+k-1+1+nsubdir
                           nsubdir=0
                        else

                           file=top_dir(1:nt)//dir//'/'//text(i)(4:k+2)
                           l=nt+3+1+k-1
                        endif
                     endif
                  endif
               else
                  if(dir.eq.'DAT') then
                     file=top_dir(1:nt)//dir//'/'
     *               //dir//'/'//dirc//'/'//text(i)(4:k+2)
                     l=nt+3+1+5+1+k-1+4
                  else
                     file=top_dir(1:nt)
     *               //dir//'/'//dirc//'/'//text(i)(4:k+2)
                     l=nt+3+1+5+1+k-1
                  endif
               endif

c            write(6,'(a)') file
            inquire(file=file,exist=exist)
             

            if(.not.exist) then
               write(6,'(a,a)') ' File not found: ',file
               write(2,'(a,a)') '          File not found: ',file
               nfile_ex_err=nfile_ex_err+1
            else
               write(3,'(a,a)') ' File found: ',file
               nfound=nfound+1
            endif
            if(dir.eq.'PRO') then
               if(file(l-2:l).eq.'for'.or.file(l-2:l-2).eq.'c') then
               if(unix) then
                  file(l-3:l)=' '
                  inquire(file=file,exist=exist)
               endif
               if(pc) then
                  file(l-3:l)='.exe'
                  inquire(file=file,exist=exist)
               endif
               if(.not.exist) then
                  write(*,'(a,a)') ' Compiled file not found: ',file
                  write(2,'(a,a)') ' Compiled file not found: ',file
                  nfile_co_err=nfile_co_err+1
               endif
               endif
            endif
         endif
 200     continue
      enddo
c
c   end of all
c

 99   continue


      write(6,*) ' Number of files found     ', nfound
      write(6,*) ' Number of files not found          ', 
     *nfile_ex_err
      write(6,*) ' Number of compiled files not found ', 
     *nfile_co_err
      write(6,*) ' File names not found given in check.out'
      write(6,*) ' File names found given in  check_ok.out'
c
      stop
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine find_subdir(text,subdir,nsubdir)
c
c  parse line for s: and extract out the subdir if any
c
      implicit none
      character*80 text
      character*80 subdir
      integer nsubdir,k,i,seiclen

      k=0
      i=seiclen(text)

      k=index(text,'s:')

      if(k.gt.0) then
         subdir(1:i-(k+1))=text(k+2:i)
        nsubdir=i-k-1
      endif
      return
      enD
             
