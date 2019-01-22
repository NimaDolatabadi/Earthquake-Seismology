c--------------------------------------------------------------------------
c  read harward moment tensor  solutions (global cmt solutions), output in nordic format
c--------------------------------------------------------------------------
c
c
c  For detail on parameters and variables names, see rea.inc
c
c   update
c
c  2010 0427 jh : B to b and S to s for magnitudes
c  2011 0308 jh : HRW to HRV, put in HRV as agency for fps, assume quality A
c                 do not output zero magnitude, output more info
c  2011 1021 pv : add mt type lines
c  2012 0426 pv : changed mt_val to absolute values, and to Nm
c  2012 1118 jh : add nkd format and full format
c  2017 0105 Jh : bug reading dip and slip in standard format, it must have changed 
c  
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file
      integer strike,dip,slip
      character*3 format_file             ! format
      character*80 text

      real cent_time_diff                 ! difference hypocenter and ccentroid time
      double precision  time              ! abs time                  
      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer i,j,k                       ! counter

      write(6,*)
      write(6,*) 'This program suports 3 formats:'
      write(6,*) 'Standard format: Default format on screen'
      write(6,*) 'Full format: Full format on screen'
      write(6,*) 'ndk format: File format for downloaded file'
      write(6,*)
c
c   open output file

       open(2,file='harnor.out',status='unknown')
    
c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input file'
      read(5,'(a)') infile
      open(1,file=infile,status='old',err=10)
      goto 11
 10    continue
      write(6,*)' No such input file'
      goto 9
 11   continue
c
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter
      rea_locality=''

c
c   find which format
c
      format_file='-'
      read(1,'(a)') text
      if(text(10:10).eq.'/') then
         format_file='ndk'
         rewind 1
         goto 15
      endif

      rewind 1
      do i=1,15
        read(1,'(a)') text
        if(text(1:21).eq.'Output in full format') then
          format_file='ful'
          rewind 1
          goto 15
        endif
        if(text(3:6).eq.'Date') then
          format_file=' '
          rewind 1
          goto 15
        endif
      enddo

 15   continue

      if(format_file.eq.' ') write(6,*)   'Standard format'
      if(format_file.eq.'ndk') write(6,*) 'ndk format'
      if(format_file.eq.'ful') write(6,*) 'Full format'
      if(format_file.eq.'-') then
         write(6,*)'Not a valid format'
         stop
      endif

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event from file unit 1
c
      read(1,'(a)',end=1000) text

      if(format_file.eq.' ') then   ! standard format 
c
c----------------------------------------------------------
c  standard format
c----------------------------------------------------------
c        
          if(text(3:6).ne.'Date') goto 50
c
c   start of event found
c
          call rea_hyp_clear(1)
          rea_id_line=' '
          rea_nhyp=1
          rea_nfault=1
          rea_nstat=0
          rea_nhead=2
          rea_nrecord=3
          rea_nphase=0
          hyp_fix_org(1)=' '
          hyp_dist_id(1)='D'
          read(text(9:18),'(i4,1x,i2,1x,i2)') 
     *    hyp_year(1),hyp_month(1),hyp_day(1)
c
c  read centroid time
c    
          read(text(37:46),'(i2,1x,i2,1x,f4.1)') 
     *    hyp_hour(1),hyp_min(1),hyp_sec(1)

          read(1,'(a)') text
          read(text,'(6x,f7.2,6x,f7.2)') 
     *    hyp_lat(1),hyp_lon(1)

          read(1,'(8x,f6.1)') hyp_depth(1)
          read(1,'(a)') rea_comment(1)
          rea_comment(1)(71:80)='GlobCMT  3'
          read(1,'(a)') rea_comment(2)
          rea_comment(2)(71:80)='GlobCMT  3'
c         rea_ncomment=2
          rea_ncomment=0     ! do not write out the comments

          read(1,'(7x,f3.1,9x,f3.1,9x,f3.1,19x,g8.2,1x))') 
     *    hyp_MAG(1,1),hyp_mag(2,1),hyp_mag(3,1),mt_moment(1)
          mt_moment(1)=mt_moment(1)/10.0**7      ! from Dyne-cm to Nm
c
          read(1,'(a)') text

          k=index(text,'strike=')
          read(text(k+7:k+10),*) strike

          k=index(text,'dip=')
          read(text(k+4:k+7),*) dip

          k=index(text,'slip=')
          read(text(k+5:k+8),*) slip


          
c          read(1,'(23x,i3,8x,i2,8x,i4)') strike,dip,slip

          mt_year(1)=hyp_year(1)
          mt_month(1)=hyp_month(1)
          mt_day(1)=hyp_day(1)
          mt_hour(1)=hyp_hour(1)
          mt_min(1)=hyp_min(1)
          mt_sec(1)=hyp_sec(1)
          mt_lat(1)=hyp_lat(1)
          mt_lon(1)=hyp_lon(1)
          mt_depth(1)=hyp_depth(1)
          hyp_agency(1)='HRV'
c
c         Expo:
c
          read(rea_comment(2),'(22x,i2,1x)')mt_exp(1)
          text=' '
          j=0
          k=1
             do i=25,70
               j=j+1
               text(j:j)=rea_comment(2)(i:i)
c              if(rea_comment(2)(i:i).eq." ".AND.text(1:1).NE." ")then
               if(rea_comment(2)(i:i).eq." ")then
                  if(text(1:1).eq." ")then
                    text=' '
                    j=0
                  else
                    read(text,'(f6.3,1x)')mt_val(k,1)
                    text=' '
                    j=0
                    k=k+1
                  endif
                endif
             enddo

      elseif(format_file.eq.'ndk') then
c
c--------------------------------------------------------
c format is ndk
c--------------------------------------------------------
c
          call rea_hyp_clear(1)
          rea_id_line=' '
          rea_nhyp=1
          rea_nfault=1
          rea_nstat=0
          rea_nhead=2
          rea_nrecord=3
          rea_nphase=0
          hyp_fix_org(1)=' '
          hyp_dist_id(1)='D'
c
c   read hypocenter values
c
          read(text(6:15),'(i4,1x,i2,1x,i2)') 
     *    hyp_year(1),hyp_month(1),hyp_day(1)
          read(text(17:26),'(i2,1x,i2,1x,f4.1)') 
     *    hyp_hour(1),hyp_min(1),hyp_sec(1)
c          write(6,*) hyp_year(1)
          read(text(49:55),'(f3.1,1x,f3.1)') hyp_mag(2,1),hyp_mag(3,1)
          hyp_agency(1)=text(1:3)   ! 4. the character missing
          read(text(28:47),'(f6.3,1x,f7.3,1x,f5.2)') 
     *    hyp_lat(1),hyp_lon(1),hyp_depth(1)

          read(1,'(a)') text
          read(1,'(a)') text
c
c   read centroid values
c
          read(text(24:53),'(f6.2,6x,f7.2,6x,f5.1)') 
     *    mt_lat(1),mt_lon(1),mt_depth(1)
          read(text(10:18),'(f9.1)') cent_time_diff
c
c   calculate centroid origin time
c
          call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *    hyp_min(1),hyp_sec(1),time)
          time=time+cent_time_diff
          call sectim(time,mt_year(1),i,mt_month(1),mt_day(1),
     *    mt_hour(1),mt_min(1),mt_sec(1))
c
          rea_comment(1)=' '
          rea_comment(1)(71:80)='GlobCMT  3'
          read(1,'(a)') text
          read(text(1:2),'(i2)')mt_exp(1)
          read(text(4:80),'(6(f6.3,7x))')(mt_val(i,1),i=1,6)
          read(1,'(a)') text
          read(text(58:68),'(i3,i3,i5)') strike,dip,slip
          read(text(50:56),'(f7.3)')mt_moment(1)
          mt_moment(1)=mt_moment(1)/10.0**7      ! from Dyne-cm to Nm
          mt_moment(1)=mt_moment(1)*10.0**mt_exp(1) ! add expo
          hyp_mag(1,1)=(2.0/3.0)*alog10(mt_moment(1))-6.07
c
c-----------------------------------------------------
c   format is full
c-----------------------------------------------------
c
      elseif(format_file.eq.'ful') then

          if(text(1:10).ne.'Event name') goto 50
         
          call rea_hyp_clear(1)
          rea_id_line=' '
          rea_nhyp=1
          rea_nfault=1
          rea_nstat=0
          rea_nhead=2
          rea_nrecord=3
          rea_nphase=0
          hyp_fix_org(1)=' '
          hyp_dist_id(1)='D'

          do i=1,2
             read(1,'(a)') text
          enddo
          text(1:13)=' '
          do i=1,70
            if(text(i:i).eq.'/') text(i:i)=' '
          enddo
          read(text,*) hyp_year(1),hyp_month(1),hyp_day(1)
          do i=1,8
             read(1,'(a)') text
          enddo
          hyp_agency(1)=text(1:3)
          read(text,'(9x,i2,3x,i2,2x,f5.2,4x,f6.2,2x,f7.2,2x,f5.1,
     *    2x,f3.1,2x,f3.1)') 
     *    hyp_hour(1),hyp_min(1),hyp_sec(1),
     *    hyp_lat(1),hyp_lon(1),hyp_depth(1), hyp_mag(2,1),hyp_mag(3,1)
          read(1,'(a)') text
          read(text,'(9x,i2,3x,i2,2x,f5.2,4x,f6.2,2x,f7.2,2x,f5.1)') 
     *    mt_hour(1),mt_min(1),mt_sec(1),
     *    mt_lat(1),mt_lon(1),mt_depth(1)
          mt_year(1)=hyp_year(1)
          mt_month(1)=hyp_month(1)
          mt_day(1)=hyp_day(1)
          do i=1,5
             read(1,'(a)') text
          enddo
          read(text(30:31),'(i2)') mt_exp(1)
          read(1,'(a)') text
          read(1,'(a)') text

          read(text,'(5x,6(f8.3))')(mt_val(i,1),i=1,6)
          do i=1,6
c             mt_val(i,1)=mt_val(i,1)*10**(mt_exp(1)-7)
          enddo
          do i=1,3
             read(1,'(a)') text
          enddo
          read(text(6:8),'(f3.1)') hyp_mag(1,1)
          read(text(27:35),'(g9.2)') mt_moment(1)
          mt_moment(1)=mt_moment(1)/10**7
          read(1,'(a)') text
c
c   blank text
c
          text(1:20)=' '
          do i =21,70
            if(text(i:i).eq.'i') text(i:i)=' '
            if(text(i:i).eq.'p') text(i:i)=' '
            if(text(i:i).eq.'s') text(i:i)=' '
            if(text(i:i).eq.'l') text(i:i)=' '            
            if(text(i:i).eq.'d') text(i:i)=' '
            if(text(i:i).eq.'=') text(i:i)=' '
          enddo        
          read(text,*) strike,dip,slip
       endif
        


c
c   check for zero magnitude
c
          do i=1,3
            if(hyp_mag(i,1).eq.0.0) hyp_mag(i,1)=-999.0
          enddo
c          write(6,*) hyp_mag(1,1)
c          read(5,*) i
          hyp_mag_type(1,1)='W'
          hyp_mag_type(2,1)='b'
          hyp_mag_type(3,1)='s'
          hyp_mag_agency(1,1)='HRV  '
          hyp_mag_agency(2,1)='HRV  '
          hyp_mag_agency(3,1)='HRV  '
c
c   write fps
c
          rea_fault(1)=' '
          write(rea_fault(1),'(3f10.0)') 
     *    float(strike),float(dip),float(slip)
          rea_fault(1)(67:69)='HRV'
          rea_fault(1)(71:76)='HARVAR'
          rea_fault(1)(78:78)='A'
          rea_fault(1)(80:80)='F'
          nevent=nevent+1               ! count events
c    
c   write on screen a bit info on event
c
          write(6,'(a,i4,1x,2i2,1x,2i2,1x,f4.1,2x,3i5)') 
     *    'Time, strike, dip, rake  ',
     *    hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *    hyp_min(1),hyp_sec(1),strike,dip,slip
 
c mt part:

          mt_nmt=1
          mt_coor(1)='S'
          mt_mag(1)=hyp_mag(1,1)
          mt_mag_type(1)=hyp_mag_type(1,1)
          mt_agency(1)='HRV  '
          mt_method(1)='GlobCMT'
c
c
c  exponent and to Nm
c
         do i=1,6
           mt_val(i,1)=mt_val(i,1)*10.0**mt_exp(1) ! add expo
           mt_val(i,1)=mt_val(i,1)/10.0**7         ! Dyne-cm to Nm
         enddo

c
c   write out 
c
         call rea_event_out(2,all,data,code)
c
c   get next event
c
      goto 50
c
c     end of file
c
 1000 continue
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is harnor.out'
c     write(6,*) 'MT',mt_exp(1)
c     write(6,*) 'MT'
c     write(6,*) 'MT',rea_comment(2)(1:22)
c     write(6,*) 'MT'
c     write(6,*) 'MT',text
c     write(6,*) 'MT',mt_val(6,1)

      stop
      end
