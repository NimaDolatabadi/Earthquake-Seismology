c
c  running guralp conversion program many times, jh, dec, 2012
c
      implicit none
      integer i,k,l,ix,m,day1,day2
      character*80 text
      character*200 longtext
      integer seiclen
      character*1 dchar
      character*80 filename
      logical pc,linux,sun

      call computer_type(sun,pc,linux)
      
      call dir_char(dchar)
      
      write(6,*)
      write(6,*)'Program converts gcf file to miniseed using gcf2msd'
      write(6,*)
     *'The data is stored in 15 min files',
     *' in a year, month, day, hour, min structure like:'
      write(6,*)'2012/12/02/01/nn, nn=00,15,30,45'
      write(6,*)'Up to one month of data can be converted in one run'
      write(6,*)
      
      write(6,*) 'Give complete path to month including month'
      write(6,*) 'Example: /home/guralp/2012/01/'
      
      read(5,'(a)') text
      write(6,*)'Give start day and end day'
      read(5,*) day1,day2

      ix=seiclen(text)
c
c   go in loop for days
c
      do m=day1,day2
c
c  go in a loop for one day
c
         do i=0,23
c
c  loop for each 15 min
c
            do k=0,59,15
c
c   make text string for file name
c
               write(text(ix+1:ix+2),'(i2)') m
               text(ix+3:ix+3)=dchar
               write(text(ix+4:ix+5),'(i2)') i
               text(ix+6:ix+6)=dchar
               write(text(ix+7:ix+8),'(i2)') k
               text(ix+9:ix+9)=dchar
               text(ix+10:ix+14)='*.gcf'
               do l=1,ix+14
                  if(text(l:l).eq.' ') text(l:l)='0'
               enddo
c
c  call conversion
c
               if(pc) then
                  longtext='gcf2msd '//text(1:ix+14)//
     *            ' /net:TU /sys /o:'//text(1:ix)//'msd'
                  write(6,*) longtext(1:seiclen(longtext))
                  call systemc(longtext,seiclen(longtext))
               else
                  longtext='dirf '//text(1:ix+14)
                  write(6,*) longtext(1:seiclen(longtext))
                  call systemc(longtext,seiclen(longtext))
                  open(77,file='filenr.lis',status='old')
 25               continue
                  read(77,'(7x,a)') filename
                  if(filename.eq.' ') goto 30
                  longtext='gcf2msd '//filename//
     *            ' /net:TU /sys /o:'//text(1:ix)//'msd'
                  write(6,*) longtext(1:seiclen(longtext))
                  call systemc(longtext,seiclen(longtext))
                  goto 25
 30               continue
                  close (77)
                endif
            enddo
         enddo
      enddo

      write(6,*)
      write(6,'(a,a)') 'Output in file: ',text(1:ix)//'msd'
      
      stop
      end 
