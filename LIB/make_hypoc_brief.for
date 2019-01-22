
      subroutine make_hypoc_brief(data,nstat,nphase,nhead,nrecord)
c
c make summary output format according to BGS preference
c bjb 2001/02/14
c
c   changes:
c
c  mar 2 2001 jh : accept distance in f-format
c
      include '../INC/libsei.inc'          ! Library definitions & data defns.
      include '../INC/seidim.inc'          ! array dimensions

      external sei open,                   ! File open handler.
     &         sei close                   ! & closure.
      integer  code,                       ! Condition.
     &         read1,                      ! Read unit1.
     &         write1                      ! Write unit1.

c---phase readings array
      CHARACTER*80 DATA(2500)                                                  

c---number of header lines, records, stations in nordic file
      integer nstat,nphase,nhead,nrecord

      real sec
      real*8 declat,declon
      real latmin,lonmin
      real*8 grid_east,grid_north
      real depth
      real rms
      real mag1,mag2,mag3
      real oterr
      real erx,ery,erz
      real res
      
      integer year,month,day
      integer hour,min
      integer lat,lon
c      integer nstat
      integer gap
      integer dist,azi
      real xdist
      integer weight

      character*1 mtyp1,mtyp2,mtyp3
      character*1 w_or_e, n_or_s
      character*5 stat
      character*4 phas

      logical error

      error=.false.

C
C   open data base input single event file and read it
c   set flag for single event nrecord= -1
c
c      nrecord = -1
c      call sei open( old$,	! Open old file (stop on error).
c     &                  ' ',             ! No prompt.
c     &                  'hyp.out', read1,! File & unit.
c     &                  b_flag,          ! File exists? (n/a).
c     &                  code )           ! Local condition (n/a).
c      call indata(read1,nstat,nphase,nhead,nrecord,
c     &               typ,exp,data,id)
c      call sei close( close$, read1, code ) ! Close file (stop on error).

c
c open output file
c
      call sei open( unknown$,                ! Open brief file.
     &               ' ',                     ! No prompt.
     &               'hyp.brief', write1,     ! File & unit.
     &               b_flag,                  ! File exists?.
     &               code )                   ! Condition (n/a).

      do i=1,nhead

         if(data(i)(80:80).eq.'1') then

            read(data(i)(2:5), '(i4)') year
            read(data(i)(7:8), '(i2)') month
            read(data(i)(9:10), '(i2)') day

            write(write1,
     *           '(''Date        : '',i2.2,''/'',i2.2,''/'',i4.4)')
     *           day,month,year
            
            read(data(i)(12:13), '(i4)') hour
            read(data(i)(14:15), '(i2)') min
            read(data(i)(17:20), '(f4.1)') sec

            write(write1,
     *           '(''Origin time : '',i2.2,'':'',i2.2,'':'',f4.1)')
     *           hour,min,sec
      
C convert lat and lon to degrees and minutes
       
            read(data(i)(24:30), '(f7.3)') declat
            read(data(i)(31:38), '(f8.3)') declon
            read(data(i)(39:43), '(f5.1)') depth

            lat=int(declat)
            lon=int(declon)
            
            if (lon.lt.0.0) then
               w_or_e = 'W'
               lon = -lon
            else
               w_or_e = 'E'
            end if
            
            if (lat.lt.0.0) then
               n_or_s = 'S'
               lat = -lat
            else
               n_or_s = 'N'
            end if

            latmin=(abs(declat)-real(lat))*60.0
            lonmin=(abs(declon)-real(lon))*60.0


            write(write1,'(''Epicentre   : '',i2.2,''-'',f5.2,'' deg '',
     *           a1,1x,i3,''-'',f5.2,
     *           '' deg '',a1,f9.4,f9.4 )')
     *           lat,latmin,n_or_s,lon,lonmin,w_or_e,declat, declon

            call latlon2ukgrid(error,declat, declon, grid_east, 
     *           grid_north)

            write(write1,
     *       '(''Grid ref    : '',f9.3,'' East /'',f9.3,'' North'')')
     *           grid_east, grid_north
      
            if (data(i)(45:45) .eq. '*') then
               write(write1,
     *              '(''Depth       : '',f5.1,'' kms  (fixed)'')')
     *              depth
            else
               write(write1,'(''Depth       : '',f5.1,'' kms'')') depth
            endif
      

            read(data(i)(49:51), '(i3)') nstat
            read(data(i)(52:55), '(f4.1)') rms



            if (data(i)(56:59) .ne. '    ') then
               read(data(i)(56:59), '(f4.1)') mag1
               read(data(i)(60:60), '(a1)') mtyp1
               write(write1,'(''Magnitude   : '',f4.1,1x,''M'',a1)')
     *              mag1,mtyp1
            endif


            if (data(i)(64:67) .ne. '    ') then
               read(data(i)(64:67), '(f4.1)') mag2
               read(data(i)(68:68), '(a1)') mtyp2
               write(write1,'(''Magnitude   : '',f4.1,1x,''M'',a1)')
     *              mag2,mtyp2
            endif
            if (data(i)(72:75) .ne. '    ') then
               read(data(i)(72:75), '(f4.1)') mag3
               read(data(i)(76:76), '(a1)') mtyp3
               write(write1,'(''Magnitude   : '',f4.1,1x,''M'',a1)')
     *              mag3,mtyp3
            endif

         else if(data(i)(80:80).eq.'E') then

               read(data(i)(6:8), '(i3)') gap
               read(data(i)(15:20), '(f6.2)') oterr
               read(data(i)(25:30), '(f6.1)') ery
               read(data(i)(33:38), '(f6.1)') erx
               read(data(i)(39:43), '(f5.1)') erz
               write(write1,*)
               write(write1,*)
     1    '             NO GAP  RMS   ERX   ERY   ERZ'
               write(write1,'(''Statistics  : ''
     1              ,i2,1x,i3,1x,f4.1,f6.1,f6.1,f6.1)')
     2              nstat,gap,rms,erx,ery,erz
      
         endif

      enddo

      write(write1,*)
      write(write1,*) 'STAT  PHASE DIST AZI   RES WT'

      do i=nhead+1,nrecord-1
         read(data(i)(2:6), '(a5)') stat
         read(data(i)(11:14), '(a4)') phas
         read(data(i)(64:68), '(f5.1)') res
         read(data(i)(69:70), '(i2)') weight
         read(data(i)(71:75), '(f5.0)') xdist
         dist=xdist+0.5
         read(data(i)(77:79), '(i3)') azi

         write(write1,'(1x,a5,1x,a4,1x,i5,1x,i3,1x,f5.1,1x,i2)')
     1        stat,phas,dist,azi,res,weight
      enddo
         

c      call sei close( close$, read1, code ) ! Close file (stop on error).
      call sei close( close$, write1, code ) ! Close file (stop on error).


C need to check if the file is empty..
      if(n_or_s.eq.' ') then    ! hyp.brief is empty
         error = .true.
      end if       

      return

      end

