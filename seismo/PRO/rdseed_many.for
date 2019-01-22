c
c
c   read one or many segments from a seed file, mainly
c   intended to chop up a seed file in many smaller files
c   of equal size. smallest interval is one minute
c   there is no selection of channels or format, assumed 
c   that seed file contain what you want
c
c   the program calls rdseed
c
c   jh jan 2004
c

      implicit none
      integer year,month,day,hour,doy,min ! times
      integer del_min               ! length of extract
      integer number_intervals       ! number of intervals
      integer i,k
      character*80  seed_file        ! seed volume name
      character*80 text              ! general text


      write(6,*) 'Seed file name'
      read(5,'(a)') seed_file
      write(6,*) 'start time YYYY,MM,DD,HH,MM'
      read(5,*) year,month,day,hour,min
      write(6,*) 'Interval in minutes'
      read(5,*) del_min
      write(6,*) 'Number of intervals to extract'
      read(5,*) number_intervals
      call DATE_DOY (DOY,DAY,MONTH,YEAR)
c
c   start extract loop
c
      do i=1,number_intervals
c
c   write input file for rdseed
c
          open(1,file='rdseed.inp',status='unknown')
          write(1,'(a)') seed_file

          do k=1,2
              write(1,*)
          enddo
          write(1,'(a)')'d'       ! extract data command
          write(1,'(a)')
          do k=1,7
              write(1,*)
          enddo
c
c  time interval
c
          write(text,'(i4,a,i3,a,i2,a,i2)') year,',',doy,',',
     *    hour,':',min
          do k=1,14
            if(text(k:k).eq.' ') text(k:k)='0'
          enddo
          write(1,'(a)') text(1:14)
c         write(7,'(a)') text(1:14)
c
c  end of interval
c
c
c   add for next interval, is also end of interval
c
          min=min+del_min
          if(min.ge.60)then
 1           continue
             min=min-60     
             hour=hour+1
             if(min.ge.60) goto 1
             if(hour.ge.24) then
                 hour=hour-24
                 doy=doy+1
             endif
           endif
          write(text,'(i4,a,i3,a,i2,a,i2)') year,',',doy,',',
     *    hour,':',min
          do k=1,14
            if(text(k:k).eq.' ') text(k:k)='0'
          enddo
          write(1,'(a)') text(1:14)
c         write(7,'(a)') text(1:14)
 
          do k=1,2
             write(1,*)
          enddo
          write(1,*)'quit'
c
          close(1)
c
c   extract data
c
          call systemc('rdseed<rdseed.inp',17)
          
      enddo
c
      stop
      end
          
c
      
